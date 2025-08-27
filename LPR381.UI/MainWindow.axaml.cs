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
        IterationsBox.Text = "";

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

                if (string.IsNullOrWhiteSpace(lhsText)) continue;

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

            // 4) Build arrays for LPFormulation (same as before)
            var nameOrder = new List<string>();
            void addNames(IEnumerable<(double coeff, string name)> terms)
            {
                foreach (var t in terms)
                    if (!nameOrder.Contains(t.name)) nameOrder.Add(t.name);
            }
            addNames(objectiveTerms);
            foreach (var c in parsedConstraints) addNames(c.lhs);

            var varNames = nameOrder.ToArray();

            var objCoeffs = new double[varNames.Length];
            foreach (var (coeff, name) in objectiveTerms)
            {
                var idx = Array.IndexOf(varNames, name);
                if (idx >= 0) objCoeffs[idx] += coeff;
            }

            int m = parsedConstraints.Count;
            int n = varNames.Length;
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

            bool useRevised = IsRevisedSelected();

            if (!useRevised)
            {
                // ====================== PRIMAL (unchanged) ======================
                ITree<SimplexNode> node = new PrimalSimplex(formulation);
                int iterations = 0;
                var dumps = new List<string>();

                while (true)
                {
                    iterations++;

                    var tableauObj = node.Item.GetType().GetProperty("Tableau")?.GetValue(node.Item);
                    if (tableauObj != null)
                    {
                        var pretty = FormatTableau(tableauObj);
                        dumps.Add($"Iteration {iterations - 1}\n{pretty}");
                    }

                    var provider = (ISimplexResultProvider)node.Item;
                    var opt = provider.SimplexResult;
                    if (opt != null && FSharpOption<SimplexResult>.get_IsSome(opt))
                    {
                        var res = opt.Value;
                        var sb = new StringBuilder();

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
                        IterationsBox.Text = string.Join("\n", dumps);
                        break;
                    }

                    var kids = node.Children;
                    if (kids == null || kids.Length == 0)
                        throw new InvalidOperationException("Simplex produced no further children but no result was reported.");

                    node = kids[0];
                }
            }
            else
            {
                // ====================== REVISED (final) ===========================
                ITree<RevisedSimplexNode> node = new RevisedPrimalSimplex(formulation);

                // Auto dual-switch if infeasible start
                var xB0 = ComputeXB(node.Item);
                if (xB0.Any(v => v < -1e-9))
                {
                    node = new RevisedDualSimplex(formulation);
                }

                int iter = 0;
                var dumps = new List<string>();

                while (true)
                {
                    // Dump product-form + price-out each iteration
                    dumps.Add(DumpRevisedIterationCompact(iter, node.Item));

                    // Terminal result?
                    var resNow = GetSimplexResultOrNull(node.Item);
                    if (resNow != null)
                    {
                        var result = resNow;   // <-- no `.Value`
                        var sb = new StringBuilder();

                        if (result.IsOptimal)
                        {
                            var opt = (SimplexResult.Optimal)result;
                            var canonicalX = opt.Item1;
                            var z = opt.Item2;
                            var originalX = formulation.fromLPCanonical(canonicalX);

                            sb.AppendLine("Status: OPTIMAL");
                            foreach (var kv in originalX.OrderBy(kv => kv.Key))
                                sb.AppendLine($"{kv.Key} = {kv.Value:0.###}");
                            sb.AppendLine($"Objective = {z:0.###}");
                            sb.AppendLine($"Iterations = {iter}");
                        }
                        else if (result.IsUnbounded)
                        {
                            var ub = (SimplexResult.Unbounded)result;
                            sb.AppendLine("Status: UNBOUNDED");
                            sb.AppendLine(ub.Item);
                        }
                        else if (result.IsInfeasible)
                        {
                            var inf = (SimplexResult.Infeasible)result;
                            sb.AppendLine("Status: INFEASIBLE");
                            sb.AppendLine($"Infeasible at row index: {inf.Item}");
                        }

                        OutputBox.Text = sb.ToString();
                        IterationsBox.Text = string.Join("\n", dumps);
                        break;
                    }


                    // Advance
                    var kids = node.Children;
                    if (kids == null || kids.Length == 0)
                    {
                        // No child but also no explicit result — stop gracefully and show what we have
                        OutputBox.Text = "Reached a terminal node but the solver did not expose a final result.\n" +
                                         "Check the last iteration dump for optimality (reduced costs and x_B).";
                        IterationsBox.Text = string.Join("\n", dumps);
                        break;
                    }

                    node = kids[0];
                    iter++;
                }

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

    // Detect dropdown choice (case-insensitive)
    private bool IsRevisedSelected()
    {
        var algoCombo = this.FindControl<ComboBox>("AlgorithmCombo");
        var label = (algoCombo?.SelectedItem as ComboBoxItem)?.Content?.ToString() ?? "";
        return label.IndexOf("revised", StringComparison.OrdinalIgnoreCase) >= 0;
    }

    // Robustly detect a terminal result from the revised node (supports either
    // a SimplexResult property or a DU state like ResultState)
    private static bool TryGetTerminalResult(object revisedItem, out SimplexResult result)
    {
        // 1) SimplexResult option?
        var opt1 = GetProp<Microsoft.FSharp.Core.FSharpOption<SimplexResult>>(revisedItem, "SimplexResult");
        if (opt1 != null && Microsoft.FSharp.Core.FSharpOption<SimplexResult>.get_IsSome(opt1))
        {
            result = opt1.Value;
            return true;
        }

        // 2) Result option?
        var opt2 = GetProp<Microsoft.FSharp.Core.FSharpOption<SimplexResult>>(revisedItem, "Result");
        if (opt2 != null && Microsoft.FSharp.Core.FSharpOption<SimplexResult>.get_IsSome(opt2))
        {
            result = opt2.Value;
            return true;
        }

        // 3) DU State = ResultState(...)
        var state = GetProp<object>(revisedItem, "State");
        if (state != null)
        {
            // try common generated flags: IsResultState / IsResult / IsFinal
            var isProps = new[] { "IsResultState", "IsResult", "IsFinal" };
            foreach (var pn in isProps)
            {
                var pi = state.GetType().GetProperty(pn);
                if (pi != null)
                {
                    var flag = pi.GetValue(state) as bool?;
                    if (flag == true)
                    {
                        var itemPi = state.GetType().GetProperty("Item") ?? state.GetType().GetProperty("Result");
                        if (itemPi != null)
                        {
                            result = (SimplexResult)itemPi.GetValue(state);
                            return true;
                        }
                    }
                }
            }
        }

        result = default!;
        return false;
    }

    // ===== Pretty printer Revised =====
    // ---------- Compact, aligned Revised Simplex iteration dump ----------
    private static string DumpRevisedIterationCompact(int iter, RevisedSimplexNode item)
    {
        // Helpers
        string N(double v, int w = 10) => v.ToString("0.###", CultureInfo.InvariantCulture).PadLeft(w);
        string H(int w) => new string('-', w);
        string J(params string[] cols) => string.Join("  ", cols); // join with 2 spaces
        string Mat(double[,] M, int w = 10)
        {
            int r = M.GetLength(0), c = M.GetLength(1);
            var sb = new StringBuilder();
            for (int i = 0; i < r; i++)
            {
                var row = new StringBuilder();
                for (int j = 0; j < c; j++) row.Append(N(M[i, j], w));
                sb.AppendLine(row.ToString());
            }
            return sb.ToString();
        }
        string MatCols(double[,] M, string[] headers, int w = 10) // headers length must match columns (or be empty)
        {
            int r = M.GetLength(0), c = M.GetLength(1);
            var sb = new StringBuilder();
            if (headers != null && headers.Length == c)
                sb.AppendLine(string.Join("", headers.Select(h => h.PadLeft(w))));
            for (int i = 0; i < r; i++)
            {
                var row = new StringBuilder();
                for (int j = 0; j < c; j++) row.Append(N(M[i, j], w));
                sb.AppendLine(row.ToString());
            }
            return sb.ToString();
        }
        double[] MatVec(double[,] M, double[] v)
        {
            int m = M.GetLength(0), n = M.GetLength(1);
            var y = new double[m];
            for (int i = 0; i < m; i++) { double s = 0; for (int j = 0; j < n; j++) s += M[i, j] * v[j]; y[i] = s; }
            return y;
        }
        double[,] TakeCols(double[,] A, int[] idx)
        {
            int m = A.GetLength(0), n = idx.Length; var R = new double[m, n];
            for (int j = 0; j < n; j++) { int cj = idx[j]; for (int i = 0; i < m; i++) R[i, j] = A[i, cj]; }
            return R;
        }
        double[,] ColToMat(double[] col)
        {
            int m = col.Length; var M = new double[m, 1];
            for (int i = 0; i < m; i++) M[i, 0] = col[i];
            return M;
        }

        // Canonical data
        var C = item.Canon;
        var names = C.VariableNames;
        int m = C.RHS.Count;
        int n = C.Objective.Count;
        var A = C.ConstraintMatrix.ToArray();  // m×n
        var c = C.Objective.ToArray();         // n
        var b = C.RHS.ToArray();               // m
        var Bidx = item.Basis;                 // length m
        var Binv = item.BInverse;              // m×m

        // Basic / nonbasic
        var isBasic = new bool[n];
        foreach (var j in Bidx) isBasic[j] = true;
        var NBidx = Enumerable.Range(0, n).Where(j => !isBasic[j]).ToArray();

        // x_B, y^T, reduced costs
        var xB = MatVec(Binv, b);
        var cB = Bidx.Select(j => c[j]).ToArray();
        var yT = new double[m];
        for (int col = 0; col < m; col++) { double s = 0; for (int i = 0; i < m; i++) s += cB[i] * Binv[i, col]; yT[col] = s; }
        var rc = new double[n];
        for (int j = 0; j < n; j++) { double yTAj = 0; for (int i = 0; i < m; i++) yTAj += yT[i] * A[i, j]; rc[j] = c[j] - yTAj; }

        // Choose entering (max +rc). If none positive -> optimal (will be handled by solver result)
        int entering = -1; double best = 0.0;
        foreach (var j in NBidx) if (rc[j] > best + 1e-12) { best = rc[j]; entering = j; }

        // Direction and ratios
        var d = new double[m];
        if (entering >= 0)
        {
            var a_j = new double[m]; for (int i = 0; i < m; i++) a_j[i] = A[i, entering];
            d = MatVec(Binv, a_j);
        }
        var theta = new double[m];
        for (int i = 0; i < m; i++) theta[i] = (d[i] > 1e-12) ? (xB[i] / d[i]) : double.NaN;
        int leavePos = -1; double minTh = double.PositiveInfinity;
        for (int i = 0; i < m; i++) if (!double.IsNaN(theta[i]) && theta[i] < minTh - 1e-12) { minTh = theta[i]; leavePos = i; }
        int leavingVar = (leavePos >= 0 ? Bidx[leavePos] : -1);

        // Ā = B^-1 A_N
        var AN = TakeCols(A, NBidx);
        var Abar = new double[m, NBidx.Length];
        for (int j = 0; j < NBidx.Length; j++)
        {
            var col = new double[m]; for (int i = 0; i < m; i++) col[i] = AN[i, j];
            var v = MatVec(Binv, col);
            for (int i = 0; i < m; i++) Abar[i, j] = v[i];
        }

        // Try η
        double[,] eta = null;
        try { eta = item.ProductForm.EtaMatrix; } catch { }

        // Column labels for Abar|b|θ
        var nbNames = NBidx.Select(j => names[j]).ToArray();
        var headers = nbNames.Concat(new[] { "b", "θ" }).ToArray();

        // Build the joined matrix [Abar | b | theta]
        var big = new double[m, NBidx.Length + 2];
        for (int i = 0; i < m; i++)
        {
            for (int j = 0; j < NBidx.Length; j++) big[i, j] = Abar[i, j];
            big[i, NBidx.Length] = b[i];
            big[i, NBidx.Length + 1] = double.IsNaN(theta[i]) ? 0.0 : theta[i];
        }

        // Output
        var outp = new StringBuilder();
        outp.AppendLine($"ITERATION T-{iter}");
        outp.AppendLine(H(80));

        // Pivot line
        if (entering >= 0 && leavingVar >= 0)
            outp.AppendLine($"Pivot: enter {names[entering]} -> leave {names[leavingVar]} (row {leavePos})");
        else
            outp.AppendLine("Pivot: (none/terminal)");

        outp.AppendLine();

        // Left summary tables
        outp.AppendLine("Basics (names):    " + string.Join(", ", Bidx.Select(j => names[j])));
        outp.AppendLine("c_B:               " + string.Join(" ", cB.Select(v => N(v))));
        outp.AppendLine("x_B = B^-1 b:      " + string.Join(" ", xB.Select(v => N(v))));
        outp.AppendLine("y^T = c_B^T B^-1:  " + string.Join(" ", yT.Select(v => N(v))));
        outp.AppendLine();

        // Price-out summary (nonbasics)
        outp.AppendLine("Nonbasics (name, c_j, c̄_j):");
        for (int k = 0; k < NBidx.Length; k++)
        {
            int j = NBidx[k];
            outp.AppendLine($"  {names[j].PadRight(8)}  c={N(c[j])}   c̄={N(rc[j])}");
        }
        outp.AppendLine();

        // Product form (center)
        outp.AppendLine("B^-1:");
        outp.Append(Mat(Binv));
        outp.AppendLine();

        if (entering >= 0)
        {
            outp.AppendLine($"A_j (entering col {names[entering]}):");
            outp.Append(Mat(ColToMat(Enumerable.Range(0, m).Select(i => A[i, entering]).ToArray())));
            outp.AppendLine();

            outp.AppendLine("A_j* = B^-1 A_j (direction d):");
            outp.Append(Mat(ColToMat(d)));
            outp.AppendLine();
        }

        if (eta != null)
        {
            outp.AppendLine("η (eta) for pivot:");
            outp.Append(Mat(eta));
            outp.AppendLine();
        }

        // Price-out right (Abar | b | theta)
        outp.AppendLine("Ā = B^-1 A_N   |   b   |   θ");
        outp.AppendLine(string.Join("", headers.Select(h => h.PadLeft(10))));
        outp.Append(Mat(big));

        outp.AppendLine(H(80));
        return outp.ToString();
    }






    private static SimplexResult? GetSimplexResultOrNull(object nodeItem)
    {
        // Try interface first (same as primal)
        if (nodeItem is ISimplexResultProvider p)
        {
            var opt = p.SimplexResult;
            if (opt != null && Microsoft.FSharp.Core.FSharpOption<SimplexResult>.get_IsSome(opt))
                return opt.Value;
        }

        // Try common property names
        var pi = nodeItem.GetType().GetProperty("SimplexResult")
              ?? nodeItem.GetType().GetProperty("Result");
        if (pi != null)
        {
            var val = pi.GetValue(nodeItem);
            if (val is Microsoft.FSharp.Core.FSharpOption<SimplexResult> opt2 &&
                Microsoft.FSharp.Core.FSharpOption<SimplexResult>.get_IsSome(opt2))
                return opt2.Value;
        }

        // Try DU State = ResultState(...)
        var state = nodeItem.GetType().GetProperty("State")?.GetValue(nodeItem);
        if (state != null)
        {
            foreach (var flag in new[] { "IsResultState", "IsResult", "IsFinal" })
            {
                var f = state.GetType().GetProperty(flag)?.GetValue(state) as bool?;
                if (f == true)
                {
                    var itemPi = state.GetType().GetProperty("Item") ?? state.GetType().GetProperty("Result");
                    if (itemPi != null) return (SimplexResult)itemPi.GetValue(state);
                }
            }
        }

        return null;
    }

    private static double[] ComputeXB(RevisedSimplexNode item)
    {
        var m = item.Canon.RHS.Count;
        var b = item.Canon.RHS.ToArray();
        var Binv = item.BInverse; // double[m,m]
        var xB = new double[m];
        for (int i = 0; i < m; i++)
        {
            double s = 0;
            for (int k = 0; k < m; k++) s += Binv[i, k] * b[k];
            xB[i] = s;
        }
        return xB;
    }





}
