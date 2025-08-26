using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Layout;
using LPR381.Core;
using Microsoft.FSharp.Core;
using System.Linq;
using System.Text;
using System;
using System.Collections.Generic;
using System.Globalization;

namespace LPR381.UI;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();
    }

    // --- New lightweight parser for linear expressions like "3x1 - 2x2 + x3" (no LPObjective/LPConstraint) ---
    // Returns sequence of (coeff, name)
    private static List<(double coeff, string name)> ParseLinearTerms(string expr)
    {
        var s = (expr ?? string.Empty).Replace(" ", "");
        if (s.Length == 0) return new List<(double, string)>();
        // Normalize leading sign
        if (s[0] != '+' && s[0] != '-') s = "+" + s;

        var terms = new List<(double, string)>();
        int i = 0;
        while (i < s.Length)
        {
            char sign = s[i]; // '+' or '-'
            if (sign != '+' && sign != '-') throw new InvalidOperationException($"Bad sign at pos {i} in '{expr}'");
            i++;

            // read coefficient (optional, default 1)
            int start = i;
            bool hasDigits = false;
            while (i < s.Length && (char.IsDigit(s[i]) || s[i] == '.')) { i++; hasDigits = true; }
            double coeff = hasDigits ? double.Parse(s.Substring(start, i - start), System.Globalization.CultureInfo.InvariantCulture) : 1.0;
            if (sign == '-') coeff = -coeff;

            // expect a variable name; allow letters, digits, underscore (e.g., x1, y, var3)
            if (i >= s.Length || (!char.IsLetter(s[i]) && s[i] != '_'))
                throw new InvalidOperationException($"Expected a variable name after coefficient in '{expr}' near position {i}.");

            start = i;
            i++; // consume first char of name
            while (i < s.Length && (char.IsLetterOrDigit(s[i]) || s[i] == '_')) i++;
            string name = s.Substring(start, i - start);

            terms.Add((coeff, name));
        }
        return terms;
    }

    //Add Constraints Button Functionality
    private void AddConstraintButton_Click(object sender, RoutedEventArgs e)
    {
        // Create a new row
        var row = new StackPanel
        {
            Orientation = Orientation.Horizontal,
            Margin = new Thickness(0, 5, 0, 0)
        };

        // Coefficients textbox
        var coeffBox = new TextBox
        {
            Width = 250,
            Watermark = "Coefficients e.g. 2x1 + 3x2"
        };

        // Operator dropdown
        var combo = new ComboBox
        {
            Width = 70,
            Margin = new Thickness(5, 0, 0, 0),
            Items =
                {
                    new ComboBoxItem { Content = "<=" },
                    new ComboBoxItem { Content = ">=" },
                    new ComboBoxItem { Content = "=" }
                }
        };

        // RHS textbox
        var rhsBox = new TextBox
        {
            Width = 80,
            Watermark = "RHS",
            Margin = new Thickness(5, 0, 0, 0),
        };
        // Delete button
        var deleteBtn = new Button
        {
            Content = "Delete",    // small cross
            Width = 110,
            Height = 30,
            Margin = new Thickness(5, 0, 0, 0)
        };

        // delete functionality
        deleteBtn.Click += (s, args) =>
        {
            ConstraintsPanel.Children.Remove(row);
        };

        // Add them to row
        row.Children.Add(coeffBox);
        row.Children.Add(combo);
        row.Children.Add(rhsBox);
        row.Children.Add(deleteBtn);

        // Add row to panel
        ConstraintsPanel.Children.Add(row);
    }

    private static ConstraintSign ParseSign(string s) =>
            s switch
            {
                "<=" => ConstraintSign.LessOrEqual,
                ">=" => ConstraintSign.GreaterOrEqual,
                "=" => ConstraintSign.Equal,
                _ => throw new InvalidOperationException($"Unknown sign '{s}'")
            };

    private void SubmitButton_Click(object? sender, RoutedEventArgs e)
    {
        OutputBox.Text = "";

        try
        {
            // 1) Objective type
            var objTypeIsMax = (ObjectiveTypeCombo.SelectedItem as ComboBoxItem)?.Content?.ToString() == "Max";
            var objectiveType = objTypeIsMax ? ObjectiveType.Max : ObjectiveType.Min;

            // 2) Parse objective (e.g., "3x1 + 2x2")
            var objText = ObjectiveTextBox.Text?.Trim() ?? "";
            if (string.IsNullOrWhiteSpace(objText))
                throw new InvalidOperationException("Please enter an objective, e.g. 3x1 + 2x2.");
            var objectiveTerms = ParseLinearTerms(objText);

            // 3) Parse constraints
            var parsedConstraints = new List<(List<(double coeff, string name)> lhs, ConstraintSign sign, double rhs, string asText)>();
            foreach (var row in ConstraintsPanel.Children.OfType<StackPanel>())
            {
                var lhsText = (row.Children[0] as TextBox)?.Text?.Trim() ?? "";
                var signStr = ((row.Children[1] as ComboBox)?.SelectedItem as ComboBoxItem)?.Content?.ToString() ?? "<=";
                var rhsStr = (row.Children[2] as TextBox)?.Text?.Trim() ?? "";

                if (string.IsNullOrWhiteSpace(lhsText)) continue; // skip empty rows

                if (!double.TryParse(rhsStr, NumberStyles.Float, CultureInfo.InvariantCulture, out var rhs))
                    throw new InvalidOperationException($"RHS must be a number: '{rhsStr}'");

                var sign = ParseSign(signStr);
                var lhsTerms = ParseLinearTerms(lhsText);
                if (lhsTerms.Count == 0)
                    throw new InvalidOperationException($"Constraint '{lhsText} {signStr} {rhsStr}' has empty left-hand side.");

                parsedConstraints.Add((lhsTerms, sign, rhs, $"{lhsText} {signStr} {rhsStr}"));
            }

            if (parsedConstraints.Count == 0)
                throw new InvalidOperationException("Add at least one constraint.");

            // 4) Build arrays needed by LPFormulation
            // Collect variable names from objective and all constraints, preserve appearance order
            var nameOrder = new List<string>();
            void addNames(IEnumerable<(double coeff, string name)> terms)
            {
                foreach (var t in terms)
                {
                    var name = t.name;
                    if (!nameOrder.Contains(name))
                        nameOrder.Add(name);
                }
            }
            addNames(objectiveTerms);
            foreach (var c in parsedConstraints)
                addNames(c.lhs);

            var varNames = nameOrder.ToArray();

            // objective coeffs aligned to varNames
            var objCoeffs = new double[varNames.Length];
            foreach (var (coeff, name) in objectiveTerms)
            {
                var idx = Array.IndexOf(varNames, name);
                if (idx >= 0) objCoeffs[idx] += coeff;
            }

            // constraint matrix
            var m = parsedConstraints.Count;
            var n = varNames.Length;
            var A = new double[m, n];
            var b = new double[m];
            var signs = new ConstraintSign[m];

            for (int i = 0; i < m; i++)
            {
                var c = parsedConstraints[i];
                b[i] = c.rhs;
                signs[i] = c.sign;

                foreach (var (coeff, name) in c.lhs)
                {
                    var j = Array.IndexOf(varNames, name);
                    if (j >= 0) A[i, j] += coeff;
                }
            }

            // default: x >= 0, continuous
            var varSignRestrictions = Enumerable.Repeat(SignRestriction.Positive, n).ToArray();
            var varIntRestrictions = Enumerable.Repeat(IntRestriction.Unrestricted, n).ToArray();

            var formulation = new LPFormulation(
                objectiveType,
                varNames,
                objCoeffs,
                A,
                signs,
                b,
                varSignRestrictions,
                varIntRestrictions
            );

            // 5) Run primal simplex and capture iterations
            ITree<SimplexNode> node = new PrimalSimplex(formulation);
            int iterations = 0;
            var dumps = new List<string>();

            while (true)
            {
                iterations++;

                // --- capture current tableau before moving to child/result ---
                var tableauObj = node.Item.GetType().GetProperty("Tableau")?.GetValue(node.Item);
                if (tableauObj != null)
                {
                    var pretty = FormatTableau(tableauObj);
                    dumps.Add($"Iteration {iterations - 1}\n{pretty}");
                }
                // -------------------------------------------------------------

                var provider = (ISimplexResultProvider)node.Item;
                var opt = provider.SimplexResult; // FSharpOption<SimplexResult>

                if (opt != null && Microsoft.FSharp.Core.FSharpOption<SimplexResult>.get_IsSome(opt))
                {
                    // terminal node: print result AND show all iterations
                    var res = opt.Value;
                    var sb = new System.Text.StringBuilder();

                    if (res.IsOptimal)
                    {
                        var optimal = (SimplexResult.Optimal)res;
                        var canonicalX = optimal.Item1;
                        var z = optimal.Item2;

                        var originalX = formulation.fromLPCanonical(canonicalX);

                        sb.AppendLine("Status: OPTIMAL");
                        foreach (var kv in originalX.OrderBy(kv => kv.Key))
                            sb.AppendLine($"{kv.Key} = {kv.Value:0.###}");
                        sb.AppendLine($"Objective = {z:0.###}");
                        sb.AppendLine($"Iterations = {iterations - 1}");
                    }
                    else if (res.IsUnbounded)
                    {
                        var ub = (SimplexResult.Unbounded)res;
                        sb.AppendLine("Status: UNBOUNDED");
                        sb.AppendLine(ub.Item);
                    }
                    else if (res.IsInfeasible)
                    {
                        var inf = (SimplexResult.Infeasible)res;
                        sb.AppendLine("Status: INFEASIBLE");
                        sb.AppendLine($"Infeasible at row index: {inf.Item}");
                    }

                    OutputBox.Text = sb.ToString();
                    IterationsBox.Text = string.Join("\n", dumps);   // <� show all captured tableaus
                    break;
                }

                // Not terminal yet ? move to child (next tableau)
                var kids = node.Children;
                if (kids == null || kids.Length == 0)
                    throw new InvalidOperationException("Simplex produced no further children but no result was reported.");

                node = kids[0];
            }

        }
        catch (Exception ex)
        {
            OutputBox.Text = "Error: " + ex.Message;
        }
    }

    // Try to get a property by name with a strong type using reflection (friendly to F# records)
    private static T? GetProp<T>(object obj, string name)
    {
        var pi = obj.GetType().GetProperty(name);
        if (pi == null) return default;
        var val = pi.GetValue(obj);
        return val is T t ? t : default;
    }

    // Pretty-print a simplex tableau with headers aligned
    private static string FormatTableau(object tableauObj)
    {
        // Expected F# record members (adjust names here if your record uses different ones)
        // ColumnNames : string[]
        // RowNames    : string[]
        // Values      : double[,]
        var cols = GetProp<string[]>(tableauObj, "ColumnNames")
                   ?? GetProp<string[]>(tableauObj, "Columns")
                   ?? Array.Empty<string>();
        var rows = GetProp<string[]>(tableauObj, "RowNames")
                   ?? GetProp<string[]>(tableauObj, "Rows")
                   ?? Array.Empty<string>();
        var vals = GetProp<double[,]>(tableauObj, "Values")
                   ?? GetProp<double[,]>(tableauObj, "Matrix")
                   ?? GetProp<double[,]>(tableauObj, "Data");

        if (vals == null)
            return "(No tableau values available)";

        int m = vals.GetLength(0);
        int n = vals.GetLength(1);

        // Column widths
        int labelW = Math.Max(6, rows.DefaultIfEmpty("").Max(s => s?.Length ?? 0));
        var colW = new int[n];
        for (int j = 0; j < n; j++)
        {
            int w = (cols != null && j < cols.Length && cols[j] != null) ? cols[j].Length : 0;
            for (int i = 0; i < m; i++)
            {
                var s = vals[i, j].ToString("0.###", System.Globalization.CultureInfo.InvariantCulture);
                if (s.Length > w) w = s.Length;
            }
            colW[j] = Math.Max(6, w);
        }

        var sb = new System.Text.StringBuilder();

        // Header line
        sb.Append(' '.ToString().PadLeft(labelW)).Append(" | ");
        for (int j = 0; j < n; j++)
        {
            var name = (cols != null && j < cols.Length && cols[j] != null) ? cols[j] : $"c{j}";
            sb.Append(name.PadLeft(colW[j])).Append(' ');
        }
        sb.AppendLine();

        // Separator
        sb.AppendLine(new string('-', labelW + 3 + colW.Sum() + n));

        // Rows
        for (int i = 0; i < m; i++)
        {
            var rname = (rows != null && i < rows.Length && rows[i] != null) ? rows[i] : $"r{i}";
            sb.Append(rname.PadLeft(labelW)).Append(" | ");
            for (int j = 0; j < n; j++)
            {
                var s = vals[i, j].ToString("0.###", System.Globalization.CultureInfo.InvariantCulture);
                sb.Append(s.PadLeft(colW[j])).Append(' ');
            }
            sb.AppendLine();
        }

        return sb.ToString();
    }

}
