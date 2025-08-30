using Avalonia;
using Avalonia.Controls;
using LPR381.Core;
using Microsoft.FSharp.Core;
using System.Linq;
using System.Text;
using System;
using System.Collections.Generic;
using System.Globalization;
using Avalonia.Interactivity;
using Avalonia.Layout;
using LPR381.UI.Solvers;
using Avalonia.Controls.Templates;
using LPR381.UI.Models;
using LPR381.UI.Util;
using LPR381.UI.Core;
using System.Threading.Tasks;
using System.IO;
using Avalonia.Media;

namespace LPR381.UI;

public partial class MainWindow : Window
{
    private TextBox? _iterationsBox;
    private TextBox? _objBox;
    private StackPanel? _panel;
    private ISolverRunner? _lastRunner;
    private readonly List<RadioButton> _algorithmRadios = new();

    public MainWindow()
    {
        InitializeComponent();
        CacheControls();
        SetupAlgorithmRadioButtons();
    }

    private void CacheControls()
    {
        _iterationsBox = this.FindControl<TextBox>("IterationsBox");
        _objBox = this.FindControl<TextBox>("ObjectiveTextBox");
        _panel = this.FindControl<StackPanel>("ConstraintsPanel");
    }

    private void SetupAlgorithmRadioButtons()
    {
        var radioPanel = this.FindControl<StackPanel>("AlgorithmRadioPanel");
        if (radioPanel == null) return;

        foreach (var entry in SolverRegistry.Available)
        {
            var radio = new RadioButton
            {
                Content = entry.Display,
                Tag = entry.Key,
                GroupName = "Algorithm",
                Margin = new Thickness(0, 2)
            };
            _algorithmRadios.Add(radio);
            radioPanel.Children.Add(radio);
        }
        
        if (_algorithmRadios.Count > 0)
            _algorithmRadios[0].IsChecked = true;
    }

    private async void SubmitButton_Click(object? sender, RoutedEventArgs e) => await Solve();

    private async Task Solve()
    {
        var summaryBox = this.FindControl<TextBox>("SolutionSummaryBox");
        var sensitivityBox = this.FindControl<TextBox>("SensitivityBox");
        if (_iterationsBox != null) _iterationsBox.Text = "";
        if (summaryBox != null) summaryBox.Text = "";
        if (sensitivityBox != null) sensitivityBox.Text = "";

        try
        {
            var input = GetUserInput();
            var runner = GetSelectedRunner();
            
            // Quick validation
            if (runner.Key == "knapsack" && (input.Constraints?.Length != 1))
            {
                if (summaryBox != null) summaryBox.Text = "Knapsack requires exactly one constraint";
                return;
            }

            // Run solver
            var summary = await runner.RunAsync(input);
            _lastRunner = runner;

            // Update iterations
            if (_iterationsBox != null)
                _iterationsBox.Text = string.Join("\n\n", runner.Iterations.Select(IterationFormat.Pretty));

            // Update summary
            if (summaryBox != null)
            {
                summaryBox.Text = $"{summary.Message}\nObjective = {summary.Objective}\n" +
                    string.Join("\n", summary.VariableValues.Select(kv => $"{kv.Key} = {kv.Value}"));
            }

            // TODO: Add sensitivity analysis
            if (sensitivityBox != null)
            {
                sensitivityBox.Text = "Sensitivity analysis will be available here.";
            }

            // Switch to results tab
            var resultsTab = this.FindControl<TabItem>("ResultsTab");
            if (resultsTab?.Parent is TabControl mainTabControl)
            {
                mainTabControl.SelectedItem = resultsTab;
            }
        }
        catch (Exception ex)
        {
            if (summaryBox != null) summaryBox.Text = "Error: " + ex.Message;
        }
    }

    // ===== Helpers =====

    private UserProblem GetUserInput()
    {
        if (_objBox == null || _panel == null)
            throw new InvalidOperationException("UI not ready.");

        var (signRestrictions, intRestrictions) = GetVariableRestrictions();
        
        return new UserProblem
        {
            ObjectiveLine = _objBox.Text ?? "",
            Constraints = ReadConstraintLines(_panel),
            IntMode = "continuous", // Default since we use individual restrictions now
            VariableSignRestrictions = signRestrictions,
            VariableIntRestrictions = intRestrictions
        };
    }

    private ISolverRunner GetSelectedRunner()
    {
        var selectedRadio = _algorithmRadios.FirstOrDefault(r => r.IsChecked == true);
        if (selectedRadio?.Tag is not string key)
            throw new InvalidOperationException("Please select a solver.");

        var entry = SolverRegistry.Available.FirstOrDefault(e => e.Key == key);
        return entry?.Factory() ?? throw new InvalidOperationException("Invalid solver selection.");
    }

    private static string[] ReadConstraintLines(StackPanel panel)
    {
        // Supports either:
        //  - rows created by AddConstraintButton_Click (lhs | sign | rhs), OR
        //  - plain TextBox lines inside the panel with full "lhs <= rhs"
        var lines = panel.Children
            .Select(child =>
            {
                if (child is StackPanel row && row.Children.Count >= 3)
                {
                    var lhs = (row.Children[0] as TextBox)?.Text?.Trim() ?? "";
                    var sign = ((row.Children[1] as ComboBox)?.SelectedItem as ComboBoxItem)?.Content?.ToString()?.Trim() ?? "";
                    var rhs = (row.Children[2] as TextBox)?.Text?.Trim() ?? "";
                    if (!string.IsNullOrWhiteSpace(lhs) && !string.IsNullOrWhiteSpace(sign) && !string.IsNullOrWhiteSpace(rhs))
                        return $"{lhs} {sign} {rhs}";
                    return "";
                }
                if (child is TextBox tb)
                    return tb.Text?.Trim() ?? "";
                return "";
            })
            .Where(s => !string.IsNullOrWhiteSpace(s))
            .ToArray();

        return lines;
    }
    

    
    private void ClearButton_Click(object? sender, RoutedEventArgs e)
    {
        ClearAllInputs();
    }

    private void ClearAllInputs()
    {
        // Clear objective
        if (_objBox != null) _objBox.Text = "";
        
        // Reset objective type to Max
        var objTypeCombo = this.FindControl<ComboBox>("ObjectiveTypeCombo");
        if (objTypeCombo != null) objTypeCombo.SelectedIndex = 0;
        
        // Clear all constraints except the first one
        if (_panel != null)
        {
            _panel.Children.Clear();
            AddConstraintRow(); // Add one empty constraint row
        }
        
        // Clear variable restrictions
        var restrictionsPanel = this.FindControl<StackPanel>("VariableRestrictionsPanel");
        if (restrictionsPanel != null) restrictionsPanel.Children.Clear();
        
        // Clear canonical preview
        var previewBox = this.FindControl<TextBox>("CanonicalPreviewBox");
        if (previewBox != null) previewBox.Text = "";
        
        // Clear file status
        var fileStatusText = this.FindControl<TextBlock>("FileStatusText");
        if (fileStatusText != null) fileStatusText.Text = "";
        
        // Reset algorithm selection to first option
        if (_algorithmRadios.Count > 0)
            _algorithmRadios[0].IsChecked = true;
    }

    private (Dictionary<string, SignRestriction>, Dictionary<string, IntRestriction>) GetVariableRestrictions()
    {
        var signRestrictions = new Dictionary<string, SignRestriction>();
        var intRestrictions = new Dictionary<string, IntRestriction>();
        var restrictionsPanel = this.FindControl<StackPanel>("VariableRestrictionsPanel");
        
        if (restrictionsPanel != null)
        {
            foreach (var child in restrictionsPanel.Children)
            {
                if (child is StackPanel row && row.Children.Count >= 2)
                {
                    if (row.Children[1] is ComboBox combo && combo.Tag is string variableName)
                    {
                        var selectedTag = (combo.SelectedItem as ComboBoxItem)?.Tag?.ToString() ?? "+";
                        
                        (signRestrictions[variableName], intRestrictions[variableName]) = selectedTag switch
                        {
                            "+" => (SignRestriction.Positive, IntRestriction.Unrestricted),
                            "-" => (SignRestriction.Negative, IntRestriction.Unrestricted),
                            "urs" => (SignRestriction.Unrestricted, IntRestriction.Unrestricted),
                            "int" => (SignRestriction.Positive, IntRestriction.Integer),
                            "bin" => (SignRestriction.Positive, IntRestriction.Binary),
                            _ => (SignRestriction.Positive, IntRestriction.Unrestricted)
                        };
                    }
                }
            }
        }
        
        return (signRestrictions, intRestrictions);
    }



    private void AddConstraintButton_Click(object sender, RoutedEventArgs e)
    {
        AddConstraintRow();
    }

    private void UpdateVariablesButton_Click(object? sender, RoutedEventArgs e)
    {
        UpdateVariableRestrictions();
    }

    private void UpdateVariableRestrictions()
    {
        var restrictionsPanel = this.FindControl<StackPanel>("VariableRestrictionsPanel");
        if (restrictionsPanel == null || _objBox == null) return;

        restrictionsPanel.Children.Clear();

        try
        {
            var variables = ExtractVariablesFromObjective(_objBox.Text ?? "");
            
            foreach (var variable in variables)
            {
                var row = new StackPanel
                {
                    Orientation = Orientation.Horizontal,
                    Margin = new Thickness(0, 2)
                };

                var label = new TextBlock
                {
                    Text = $"{variable}:",
                    Width = 60,
                    VerticalAlignment = VerticalAlignment.Center
                };

                var combo = new ComboBox
                {
                    Width = 120,
                    Margin = new Thickness(5, 0),
                    Tag = variable,
                    Items =
                    {
                        new ComboBoxItem { Content = "+", Tag = "+" },
                        new ComboBoxItem { Content = "-", Tag = "-" },
                        new ComboBoxItem { Content = "urs", Tag = "urs" },
                        new ComboBoxItem { Content = "int", Tag = "int" },
                        new ComboBoxItem { Content = "bin", Tag = "bin" }
                    },
                    SelectedIndex = 0 // Default to +
                };

                row.Children.Add(label);
                row.Children.Add(combo);
                restrictionsPanel.Children.Add(row);
            }
        }
        catch (Exception ex)
        {
            var errorText = new TextBlock
            {
                Text = $"Error parsing variables: {ex.Message}",
                Foreground = new SolidColorBrush(Colors.Red)
            };
            restrictionsPanel.Children.Add(errorText);
        }
    }

    private static List<string> ExtractVariablesFromObjective(string objective)
    {
        var variables = new HashSet<string>();
        if (string.IsNullOrWhiteSpace(objective)) return new List<string>();

        // Remove "max" or "min" prefix if present
        var cleanObjective = objective.Trim();
        if (cleanObjective.StartsWith("max", StringComparison.OrdinalIgnoreCase) ||
            cleanObjective.StartsWith("min", StringComparison.OrdinalIgnoreCase))
        {
            cleanObjective = cleanObjective.Substring(3).Trim();
        }

        // Simple regex-like parsing to extract variable names
        var terms = cleanObjective.Split(new[] { '+', '-' }, StringSplitOptions.RemoveEmptyEntries);
        
        foreach (var term in terms)
        {
            var cleanTerm = term.Trim();
            if (string.IsNullOrEmpty(cleanTerm)) continue;

            // Extract variable name (letters and digits after any coefficient)
            var variablePart = "";
            bool foundVariable = false;
            
            for (int i = 0; i < cleanTerm.Length; i++)
            {
                char c = cleanTerm[i];
                if (char.IsLetter(c) || (foundVariable && char.IsDigit(c)))
                {
                    foundVariable = true;
                    variablePart += c;
                }
                else if (foundVariable)
                {
                    break;
                }
            }

            if (!string.IsNullOrEmpty(variablePart))
            {
                variables.Add(variablePart);
            }
        }

        return variables.OrderBy(v => v).ToList();
    }
    private async void UploadFileButton_Click(object sender, RoutedEventArgs e)
    {
        var dialog = new OpenFileDialog();
        dialog.AllowMultiple = false;
        dialog.Filters.Add(new FileDialogFilter() { Name = "Text Files", Extensions = { "txt" } });

        var result = await dialog.ShowAsync(this);

        if (result != null && result.Length > 0)
        {
            try
            {
                string filePath = result[0];
                string fileContent = File.ReadAllText(filePath);
                
                // Parse file using F# InputFile module
                LPFormulation formulation = default!;
                string error = "";
                if (!InputFile.TryInterpretText(fileContent, ref formulation, ref error))
                {
                    var statusText = this.FindControl<TextBlock>("FileStatusText");
                    if (statusText != null)
                    {
                        statusText.Text = $"Error: {error}";
                        statusText.Foreground = new SolidColorBrush(Colors.Red);
                    }
                    return;
                }
                
                // Populate UI fields from formulation
                PopulateFromFormulation(formulation);
                
                // Show success status
                var fileStatusText = this.FindControl<TextBlock>("FileStatusText");
                if (fileStatusText != null)
                {
                    fileStatusText.Text = "✓ File loaded successfully";
                    fileStatusText.Foreground = new SolidColorBrush(Colors.Green);
                }
            }
            catch (Exception ex)
            {
                var statusText = this.FindControl<TextBlock>("FileStatusText");
                if (statusText != null)
                {
                    statusText.Text = $"Error: {ex.Message}";
                    statusText.Foreground = new SolidColorBrush(Colors.Red);
                }
            }
        }
    }
    
    private void PopulateFromFormulation(LPFormulation formulation)
    {
        // Set objective type
        var objTypeCombo = this.FindControl<ComboBox>("ObjectiveTypeCombo");
        if (objTypeCombo != null)
        {
            objTypeCombo.SelectedIndex = formulation.ObjectiveType == ObjectiveType.Max ? 0 : 1;
        }
        
        // Set objective function
        if (_objBox != null)
        {
            var objTerms = formulation.VarNames.Zip(formulation.Objective, (name, coeff) => $"{coeff:+0.###;-0.###}{name}");
            _objBox.Text = string.Join(" ", objTerms);
        }
        
        // Variable type is now handled individually per variable
        
        // Clear existing constraints and add new ones
        if (_panel != null)
        {
            _panel.Children.Clear();
            
            for (int i = 0; i < formulation.ConstraintSigns.Length; i++)
            {
                var terms = new List<string>();
                for (int j = 0; j < formulation.VarNames.Length; j++)
                {
                    var coeff = formulation.ConstraintCoefficients[i, j];
                    if (Math.Abs(coeff) > 1e-9)
                        terms.Add($"{coeff:+0.###;-0.###}{formulation.VarNames[j]}");
                }
                var sign = formulation.ConstraintSigns[i] switch
                {
                    ConstraintSign.LessOrEqual => "<=",
                    ConstraintSign.GreaterOrEqual => ">=",
                    _ => "="
                };
                
                AddConstraintRow(string.Join(" ", terms), sign, formulation.RHS[i].ToString());
            }
        }
        
        // Update variable restrictions based on formulation
        UpdateVariableRestrictions();
        
        // Set the sign restrictions from the formulation
        var restrictionsPanel = this.FindControl<StackPanel>("VariableRestrictionsPanel");
        if (restrictionsPanel != null)
        {
            for (int i = 0; i < Math.Min(formulation.VarNames.Length, formulation.VarSignRestrictions.Length); i++)
            {
                var varName = formulation.VarNames[i];
                var restriction = formulation.VarSignRestrictions[i];
                
                // Find the combo box for this variable
                foreach (var child in restrictionsPanel.Children)
                {
                    if (child is StackPanel row && row.Children.Count >= 2)
                    {
                        if (row.Children[1] is ComboBox combo && combo.Tag?.ToString() == varName)
                        {
                            var intRestriction = i < formulation.VarIntRestrictions.Length ? formulation.VarIntRestrictions[i] : IntRestriction.Unrestricted;
                            
                            combo.SelectedIndex = (restriction, intRestriction) switch
                            {
                                (SignRestriction.Negative, IntRestriction.Unrestricted) => 1, // -
                                (SignRestriction.Unrestricted, IntRestriction.Unrestricted) => 2, // urs
                                (SignRestriction.Positive, IntRestriction.Integer) => 3, // int
                                (SignRestriction.Positive, IntRestriction.Binary) => 4, // bin
                                _ => 0 // + (positive)
                            };
                            break;
                        }
                    }
                }
            }
        }
    }
    
    private void AddConstraintRow(string lhs = "", string sign = "<=", string rhs = "")
    {
        var row = new StackPanel
        {
            Orientation = Orientation.Horizontal,
            Margin = new Thickness(0, 5, 0, 0)
        };

        var coeffBox = new TextBox
        {
            Width = 250,
            Watermark = "Coefficients e.g. 2x1 + 3x2",
            Text = lhs
        };

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
        combo.SelectedIndex = sign switch { ">=" => 1, "=" => 2, _ => 0 };

        var rhsBox = new TextBox
        {
            Width = 80,
            Watermark = "RHS",
            Margin = new Thickness(5, 0, 0, 0),
            Text = rhs
        };
        
        var deleteBtn = new Button
        {
            Content = "✕",
            Width = 30,
            Height = 30,
            Margin = new Thickness(5, 0, 0, 0),
            FontSize = 14,
            FontWeight = FontWeight.Bold
        };

        deleteBtn.Click += (s, args) => _panel?.Children.Remove(row);

        row.Children.Add(coeffBox);
        row.Children.Add(combo);
        row.Children.Add(rhsBox);
        row.Children.Add(deleteBtn);

        _panel?.Children.Add(row);
    }

    private void ViewIterationsButton_Click(object? sender, RoutedEventArgs e)
    {
        var iterationsSubTab = this.FindControl<TabItem>("IterationsSubTab");
        if (iterationsSubTab?.Parent is TabControl resultsTabControl)
        {
            resultsTabControl.SelectedItem = iterationsSubTab;
        }
    }

    private async void PreviewCanonicalButton_Click(object? sender, RoutedEventArgs e)
    {
        var previewBox = this.FindControl<TextBox>("CanonicalPreviewBox");
        if (previewBox == null) return;

        try
        {
            var input = GetUserInput();
            var tempRunner = new PrimalSimplexRunner(); // Use any runner to build formulation
            var formulation = ((SolverRunner)tempRunner).BuildFormulation(input);
            var canonical = formulation.ToLPCanonical();
            
            var preview = $"Objective: {canonical.ObjectiveType}\n";
            preview += $"Variables: {string.Join(", ", canonical.VariableNames)}\n";
            preview += $"Objective coeffs: [{string.Join(", ", canonical.Objective.ToArray().Select(x => x.ToString("F2")))}]\n";
            preview += $"Constraints: {canonical.ConstraintMatrix.RowCount} x {canonical.ConstraintMatrix.ColumnCount}\n";
            preview += $"RHS: [{string.Join(", ", canonical.RHS.ToArray().Select(x => x.ToString("F2")))}]";
            
            previewBox.Text = preview;
        }
        catch (Exception ex)
        {
            previewBox.Text = $"Error: {ex.Message}";
        }
    }
    
    private static UserProblem ConvertToUserProblem(LPFormulation formulation)
    {
        // Convert objective
        var objType = formulation.ObjectiveType == ObjectiveType.Max ? "max" : "min";
        var objTerms = formulation.VarNames.Zip(formulation.Objective, (name, coeff) => $"{coeff:+0.###;-0.###}{name}");
        var objLine = $"{objType} {string.Join(" ", objTerms)}";
        
        // Convert constraints
        var constraints = new string[formulation.ConstraintSigns.Length];
        for (int i = 0; i < constraints.Length; i++)
        {
            var terms = new List<string>();
            for (int j = 0; j < formulation.VarNames.Length; j++)
            {
                var coeff = formulation.ConstraintCoefficients[i, j];
                if (Math.Abs(coeff) > 1e-9)
                    terms.Add($"{coeff:+0.###;-0.###}{formulation.VarNames[j]}");
            }
            var sign = formulation.ConstraintSigns[i] switch
            {
                ConstraintSign.LessOrEqual => "<=",
                ConstraintSign.GreaterOrEqual => ">=",
                _ => "="
            };
            constraints[i] = $"{string.Join(" ", terms)} {sign} {formulation.RHS[i]}";
        }
        
        // Determine variable type
        var intMode = "continuous";
        if (formulation.VarIntRestrictions.Any(r => r == IntRestriction.Binary))
            intMode = "binary";
        else if (formulation.VarIntRestrictions.Any(r => r == IntRestriction.Integer))
            intMode = "integer";
        
        return new UserProblem
        {
            ObjectiveLine = objLine,
            Constraints = constraints,
            IntMode = intMode
        };
    }
    
    private async void ExportButton_Click(object sender, RoutedEventArgs e)
    {
        if (_lastRunner?.Iterations.Count == 0)
        {
            var summaryBox = this.FindControl<TextBox>("SolutionSummaryBox");
            if (summaryBox != null) summaryBox.Text = "No results to export. Please solve a problem first.";
            return;
        }

        var dialog = new SaveFileDialog();
        dialog.Filters.Add(new FileDialogFilter() { Name = "Text Files", Extensions = { "txt" } });
        dialog.DefaultExtension = "txt";

        var result = await dialog.ShowAsync(this);
        if (result != null)
        {
            try
            {
                _lastRunner.ExportToFile(result);
                var summaryBox = this.FindControl<TextBox>("SolutionSummaryBox");
                if (summaryBox != null) summaryBox.Text += $"\n\nResults exported to: {result}";
            }
            catch (Exception ex)
            {
                var summaryBox = this.FindControl<TextBox>("SolutionSummaryBox");
                if (summaryBox != null) summaryBox.Text += $"\n\nExport failed: {ex.Message}";
            }
        }
    }
}
//// --- New lightweight parser for linear expressions like "3x1 - 2x2 + x3" (no LPObjective/LPConstraint) ---
//// Returns sequence of (coeff, name)
//private static List<(double coeff, string name)> ParseLinearTerms(string expr)
//{
//    var s = (expr ?? string.Empty).Replace(" ", "");
//    if (s.Length == 0) return new List<(double, string)>();
//    // Normalize leading sign
//    if (s[0] != '+' && s[0] != '-') s = "+" + s;

//    var terms = new List<(double, string)>();
//    int i = 0;
//    while (i < s.Length)
//    {
//        char sign = s[i]; // '+' or '-'
//        if (sign != '+' && sign != '-') throw new InvalidOperationException($"Bad sign at pos {i} in '{expr}'");
//        i++;

//        // read coefficient (optional, default 1)
//        int start = i;
//        bool hasDigits = false;
//        while (i < s.Length && (char.IsDigit(s[i]) || s[i] == '.')) { i++; hasDigits = true; }
//        double coeff = hasDigits ? double.Parse(s.Substring(start, i - start), System.Globalization.CultureInfo.InvariantCulture) : 1.0;
//        if (sign == '-') coeff = -coeff;

//        // expect a variable name; allow letters, digits, underscore (e.g., x1, y, var3)
//        if (i >= s.Length || (!char.IsLetter(s[i]) && s[i] != '_'))
//            throw new InvalidOperationException($"Expected a variable name after coefficient in '{expr}' near position {i}.");

//        start = i;
//        i++; // consume first char of name
//        while (i < s.Length && (char.IsLetterOrDigit(s[i]) || s[i] == '_')) i++;
//        string name = s.Substring(start, i - start);

//        terms.Add((coeff, name));
//    }
//    return terms;
//}

//Add Constraints Button Functionality


//private static ConstraintSign ParseSign(string s) =>
//        s switch
//        {
//            "<=" => ConstraintSign.LessOrEqual,
//            ">=" => ConstraintSign.GreaterOrEqual,
//            "=" => ConstraintSign.Equal,
//            _ => throw new InvalidOperationException($"Unknown sign '{s}'")
//        };

//private void SubmitButton_Click(object? sender, RoutedEventArgs e)
//{
//    OutputBox.Text = "";
//    IterationsBox.Text = "";

//    try
//    {
//        // === 1) Objective type ===
//        var objTypeIsMax = (ObjectiveTypeCombo.SelectedItem as ComboBoxItem)?.Content?.ToString() == "Max";
//        var objectiveType = objTypeIsMax ? ObjectiveType.Max : ObjectiveType.Min;

//        // === 2) Parse objective ===
//        var objText = ObjectiveTextBox.Text?.Trim() ?? "";
//        if (string.IsNullOrWhiteSpace(objText))
//            throw new InvalidOperationException("Please enter an objective, e.g. 3x1 + 2x2.");
//        var objectiveTerms = ParseLinearTerms(objText);

//        // === 3) Parse constraints ===
//        var parsedConstraints = new List<(List<(double coeff, string name)> lhs, ConstraintSign sign, double rhs, string asText)>();
//        foreach (var row in ConstraintsPanel.Children.OfType<StackPanel>())
//        {
//            var lhsText = (row.Children[0] as TextBox)?.Text?.Trim() ?? "";
//            var signStr = ((row.Children[1] as ComboBox)?.SelectedItem as ComboBoxItem)?.Content?.ToString() ?? "<=";
//            var rhsStr = (row.Children[2] as TextBox)?.Text?.Trim() ?? "";

//            if (string.IsNullOrWhiteSpace(lhsText)) continue;

//            if (!double.TryParse(rhsStr, NumberStyles.Float, CultureInfo.InvariantCulture, out var rhs))
//                throw new InvalidOperationException($"RHS must be a number: '{rhsStr}'");

//            var sign = ParseSign(signStr);
//            var lhsTerms = ParseLinearTerms(lhsText);
//            if (lhsTerms.Count == 0)
//                throw new InvalidOperationException($"Constraint '{lhsText} {signStr} {rhsStr}' has empty left-hand side.");

//            parsedConstraints.Add((lhsTerms, sign, rhs, $"{lhsText} {signStr} {rhsStr}"));
//        }
//        if (parsedConstraints.Count == 0)
//            throw new InvalidOperationException("Add at least one constraint.");

//        // === 4) Build LPFormulation arrays ===
//        var nameOrder = new List<string>();
//        void addNames(IEnumerable<(double coeff, string name)> terms)
//        {
//            foreach (var t in terms)
//                if (!nameOrder.Contains(t.name)) nameOrder.Add(t.name);
//        }
//        addNames(objectiveTerms);
//        foreach (var c in parsedConstraints) addNames(c.lhs);

//        var varNames = nameOrder.ToArray();

//        var objCoeffs = new double[varNames.Length];
//        foreach (var (coeff, name) in objectiveTerms)
//        {
//            var idx = Array.IndexOf(varNames, name);
//            if (idx >= 0) objCoeffs[idx] += coeff;
//        }

//        int m = parsedConstraints.Count;
//        int n = varNames.Length;
//        var A = new double[m, n];
//        var b = new double[m];
//        var signs = new ConstraintSign[m];

//        for (int i = 0; i < m; i++)
//        {
//            var c = parsedConstraints[i];
//            b[i] = c.rhs;
//            signs[i] = c.sign;
//            foreach (var (coeff, name) in c.lhs)
//            {
//                var j = Array.IndexOf(varNames, name);
//                if (j >= 0) A[i, j] += coeff;
//            }
//        }

//        var varSignRestrictions = Enumerable.Repeat(SignRestriction.Positive, n).ToArray();
//        var varIntRestrictions = Enumerable.Repeat(IntRestriction.Unrestricted, n).ToArray();

//        var formulation = new LPFormulation(
//            objectiveType,
//            varNames,
//            objCoeffs,
//            A,
//            signs,
//            b,
//            varSignRestrictions,
//            varIntRestrictions
//        );

//        // === 5) Pick the solver from dropdown via registry ===
//        var algoCombo = this.FindControl<ComboBox>("AlgorithmCombo");
//        var selectedItem = (algoCombo?.SelectedItem as ComboBoxItem);
//        var selectedKey = selectedItem?.Tag?.ToString() ?? "primal-tableau";

//        var runner = SolverRegistry.CreateRunner(
//            selectedKey,
//            FormatTableau,                                   // used by PrimalSimplexRunner
//            (iter, nodeItem) => DumpRevisedIterationCompact(iter, nodeItem), // used by RevisedSimplexRunner
//            GetSimplexResultOrNull,
//            ComputeXB
//        );

//        // === 6) Solve and display ===
//        var result = runner.Solve(formulation);
//        OutputBox.Text = result.OutputText;
//        IterationsBox.Text = result.IterationsText;
//    }
//    catch (Exception ex)
//    {
//        OutputBox.Text = "Error: " + ex.Message;
//    }
//}



//// Try to get a property by name with a strong type using reflection (friendly to F# records)
//private static T? GetProp<T>(object obj, string name)
//{
//    var pi = obj.GetType().GetProperty(name);
//    if (pi == null) return default;
//    var val = pi.GetValue(obj);
//    return val is T t ? t : default;
//}

//// Pretty-print a simplex tableau with headers aligned
//private static string FormatTableau(object tableauObj)
//{
//    // Expected F# record members (adjust names here if your record uses different ones)
//    // ColumnNames : string[]
//    // RowNames    : string[]
//    // Values      : double[,]
//    var cols = GetProp<string[]>(tableauObj, "ColumnNames")
//               ?? GetProp<string[]>(tableauObj, "Columns")
//               ?? Array.Empty<string>();
//    var rows = GetProp<string[]>(tableauObj, "RowNames")
//               ?? GetProp<string[]>(tableauObj, "Rows")
//               ?? Array.Empty<string>();
//    var vals = GetProp<double[,]>(tableauObj, "Values")
//               ?? GetProp<double[,]>(tableauObj, "Matrix")
//               ?? GetProp<double[,]>(tableauObj, "Data");

//    if (vals == null)
//        return "(No tableau values available)";

//    int m = vals.GetLength(0);
//    int n = vals.GetLength(1);

//    // Column widths
//    int labelW = Math.Max(6, rows.DefaultIfEmpty("").Max(s => s?.Length ?? 0));
//    var colW = new int[n];
//    for (int j = 0; j < n; j++)
//    {
//        int w = (cols != null && j < cols.Length && cols[j] != null) ? cols[j].Length : 0;
//        for (int i = 0; i < m; i++)
//        {
//            var s = vals[i, j].ToString("0.###", System.Globalization.CultureInfo.InvariantCulture);
//            if (s.Length > w) w = s.Length;
//        }
//        colW[j] = Math.Max(6, w);
//    }

//    var sb = new System.Text.StringBuilder();

//    // Header line
//    sb.Append(' '.ToString().PadLeft(labelW)).Append(" | ");
//    for (int j = 0; j < n; j++)
//    {
//        var name = (cols != null && j < cols.Length && cols[j] != null) ? cols[j] : $"c{j}";
//        sb.Append(name.PadLeft(colW[j])).Append(' ');
//    }
//    sb.AppendLine();

//    // Separator
//    sb.AppendLine(new string('-', labelW + 3 + colW.Sum() + n));

//    // Rows
//    for (int i = 0; i < m; i++)
//    {
//        var rname = (rows != null && i < rows.Length && rows[i] != null) ? rows[i] : $"r{i}";
//        sb.Append(rname.PadLeft(labelW)).Append(" | ");
//        for (int j = 0; j < n; j++)
//        {
//            var s = vals[i, j].ToString("0.###", System.Globalization.CultureInfo.InvariantCulture);
//            sb.Append(s.PadLeft(colW[j])).Append(' ');
//        }
//        sb.AppendLine();
//    }

//    return sb.ToString();
//}



//// ===== Pretty printer Revised =====
//// ---------- Compact, aligned Revised Simplex iteration dump ----------
//private static string DumpRevisedIterationCompact(int iter, RevisedSimplexNode item)
//{
//    // Helpers
//    string N(double v, int w = 10) => v.ToString("0.###", CultureInfo.InvariantCulture).PadLeft(w);
//    string H(int w) => new string('-', w);
//    string J(params string[] cols) => string.Join("  ", cols); // join with 2 spaces
//    string Mat(double[,] M, int w = 10)
//    {
//        int r = M.GetLength(0), c = M.GetLength(1);
//        var sb = new StringBuilder();
//        for (int i = 0; i < r; i++)
//        {
//            var row = new StringBuilder();
//            for (int j = 0; j < c; j++) row.Append(N(M[i, j], w));
//            sb.AppendLine(row.ToString());
//        }
//        return sb.ToString();
//    }
//    string MatCols(double[,] M, string[] headers, int w = 10) // headers length must match columns (or be empty)
//    {
//        int r = M.GetLength(0), c = M.GetLength(1);
//        var sb = new StringBuilder();
//        if (headers != null && headers.Length == c)
//            sb.AppendLine(string.Join("", headers.Select(h => h.PadLeft(w))));
//        for (int i = 0; i < r; i++)
//        {
//            var row = new StringBuilder();
//            for (int j = 0; j < c; j++) row.Append(N(M[i, j], w));
//            sb.AppendLine(row.ToString());
//        }
//        return sb.ToString();
//    }
//    double[] MatVec(double[,] M, double[] v)
//    {
//        int m = M.GetLength(0), n = M.GetLength(1);
//        var y = new double[m];
//        for (int i = 0; i < m; i++) { double s = 0; for (int j = 0; j < n; j++) s += M[i, j] * v[j]; y[i] = s; }
//        return y;
//    }
//    double[,] TakeCols(double[,] A, int[] idx)
//    {
//        int m = A.GetLength(0), n = idx.Length; var R = new double[m, n];
//        for (int j = 0; j < n; j++) { int cj = idx[j]; for (int i = 0; i < m; i++) R[i, j] = A[i, cj]; }
//        return R;
//    }
//    double[,] ColToMat(double[] col)
//    {
//        int m = col.Length; var M = new double[m, 1];
//        for (int i = 0; i < m; i++) M[i, 0] = col[i];
//        return M;
//    }

//    // Canonical data
//    var C = item.Canon;
//    var names = C.VariableNames;
//    int m = C.RHS.Count;
//    int n = C.Objective.Count;
//    var A = C.ConstraintMatrix.ToArray();  // m×n
//    var c = C.Objective.ToArray();         // n
//    var b = C.RHS.ToArray();               // m
//    var Bidx = item.Basis;                 // length m
//    var Binv = item.BInverse;              // m×m

//    // Basic / nonbasic
//    var isBasic = new bool[n];
//    foreach (var j in Bidx) isBasic[j] = true;
//    var NBidx = Enumerable.Range(0, n).Where(j => !isBasic[j]).ToArray();

//    // x_B, y^T, reduced costs
//    var xB = MatVec(Binv, b);
//    var cB = Bidx.Select(j => c[j]).ToArray();
//    var yT = new double[m];
//    for (int col = 0; col < m; col++) { double s = 0; for (int i = 0; i < m; i++) s += cB[i] * Binv[i, col]; yT[col] = s; }
//    var rc = new double[n];
//    for (int j = 0; j < n; j++) { double yTAj = 0; for (int i = 0; i < m; i++) yTAj += yT[i] * A[i, j]; rc[j] = c[j] - yTAj; }

//    // Choose entering (max +rc). If none positive -> optimal (will be handled by solver result)
//    int entering = -1; double best = 0.0;
//    foreach (var j in NBidx) if (rc[j] > best + 1e-12) { best = rc[j]; entering = j; }

//    // Direction and ratios
//    var d = new double[m];
//    if (entering >= 0)
//    {
//        var a_j = new double[m]; for (int i = 0; i < m; i++) a_j[i] = A[i, entering];
//        d = MatVec(Binv, a_j);
//    }
//    var theta = new double[m];
//    for (int i = 0; i < m; i++) theta[i] = (d[i] > 1e-12) ? (xB[i] / d[i]) : double.NaN;
//    int leavePos = -1; double minTh = double.PositiveInfinity;
//    for (int i = 0; i < m; i++) if (!double.IsNaN(theta[i]) && theta[i] < minTh - 1e-12) { minTh = theta[i]; leavePos = i; }
//    int leavingVar = (leavePos >= 0 ? Bidx[leavePos] : -1);

//    // Ā = B^-1 A_N
//    var AN = TakeCols(A, NBidx);
//    var Abar = new double[m, NBidx.Length];
//    for (int j = 0; j < NBidx.Length; j++)
//    {
//        var col = new double[m]; for (int i = 0; i < m; i++) col[i] = AN[i, j];
//        var v = MatVec(Binv, col);
//        for (int i = 0; i < m; i++) Abar[i, j] = v[i];
//    }

//    // Try η
//    double[,] eta = null;
//    try { eta = item.ProductForm.EtaMatrix; } catch { }

//    // Column labels for Abar|b|θ
//    var nbNames = NBidx.Select(j => names[j]).ToArray();
//    var headers = nbNames.Concat(new[] { "b", "θ" }).ToArray();

//    // Build the joined matrix [Abar | b | theta]
//    var big = new double[m, NBidx.Length + 2];
//    for (int i = 0; i < m; i++)
//    {
//        for (int j = 0; j < NBidx.Length; j++) big[i, j] = Abar[i, j];
//        big[i, NBidx.Length] = b[i];
//        big[i, NBidx.Length + 1] = double.IsNaN(theta[i]) ? 0.0 : theta[i];
//    }

//    // Output
//    var outp = new StringBuilder();
//    outp.AppendLine($"ITERATION T-{iter}");
//    outp.AppendLine(H(80));

//    // Pivot line
//    if (entering >= 0 && leavingVar >= 0)
//        outp.AppendLine($"Pivot: enter {names[entering]} -> leave {names[leavingVar]} (row {leavePos})");
//    else
//        outp.AppendLine("Pivot: (none/terminal)");

//    outp.AppendLine();

//    // Left summary tables
//    outp.AppendLine("Basics (names):    " + string.Join(", ", Bidx.Select(j => names[j])));
//    outp.AppendLine("c_B:               " + string.Join(" ", cB.Select(v => N(v))));
//    outp.AppendLine("x_B = B^-1 b:      " + string.Join(" ", xB.Select(v => N(v))));
//    outp.AppendLine("y^T = c_B^T B^-1:  " + string.Join(" ", yT.Select(v => N(v))));
//    outp.AppendLine();

//    // Price-out summary (nonbasics)
//    outp.AppendLine("Nonbasics (name, c_j, c̄_j):");
//    for (int k = 0; k < NBidx.Length; k++)
//    {
//        int j = NBidx[k];
//        outp.AppendLine($"  {names[j].PadRight(8)}  c={N(c[j])}   c̄={N(rc[j])}");
//    }
//    outp.AppendLine();

//    // Product form (center)
//    outp.AppendLine("B^-1:");
//    outp.Append(Mat(Binv));
//    outp.AppendLine();

//    if (entering >= 0)
//    {
//        outp.AppendLine($"A_j (entering col {names[entering]}):");
//        outp.Append(Mat(ColToMat(Enumerable.Range(0, m).Select(i => A[i, entering]).ToArray())));
//        outp.AppendLine();

//        outp.AppendLine("A_j* = B^-1 A_j (direction d):");
//        outp.Append(Mat(ColToMat(d)));
//        outp.AppendLine();
//    }

//    if (eta != null)
//    {
//        outp.AppendLine("η (eta) for pivot:");
//        outp.Append(Mat(eta));
//        outp.AppendLine();
//    }

//    // Price-out right (Abar | b | theta)
//    outp.AppendLine("Ā = B^-1 A_N   |   b   |   θ");
//    outp.AppendLine(string.Join("", headers.Select(h => h.PadLeft(10))));
//    outp.Append(Mat(big));

//    outp.AppendLine(H(80));
//    return outp.ToString();
//}

//private static SimplexResult? GetSimplexResultOrNull(object nodeItem)
//{
//    // Try interface first (same as primal)
//    if (nodeItem is ISimplexResultProvider p)
//    {
//        var opt = p.SimplexResult;
//        if (opt != null && Microsoft.FSharp.Core.FSharpOption<SimplexResult>.get_IsSome(opt))
//            return opt.Value;
//    }

//    // Try common property names
//    var pi = nodeItem.GetType().GetProperty("SimplexResult")
//          ?? nodeItem.GetType().GetProperty("Result");
//    if (pi != null)
//    {
//        var val = pi.GetValue(nodeItem);
//        if (val is Microsoft.FSharp.Core.FSharpOption<SimplexResult> opt2 &&
//            Microsoft.FSharp.Core.FSharpOption<SimplexResult>.get_IsSome(opt2))
//            return opt2.Value;
//    }

//    // Try DU State = ResultState(...)
//    var state = nodeItem.GetType().GetProperty("State")?.GetValue(nodeItem);
//    if (state != null)
//    {
//        foreach (var flag in new[] { "IsResultState", "IsResult", "IsFinal" })
//        {
//            var f = state.GetType().GetProperty(flag)?.GetValue(state) as bool?;
//            if (f == true)
//            {
//                var itemPi = state.GetType().GetProperty("Item") ?? state.GetType().GetProperty("Result");
//                if (itemPi != null) return (SimplexResult)itemPi.GetValue(state);
//            }
//        }
//    }

//    return null;
//}

//private static double[] ComputeXB(RevisedSimplexNode item)
//{
//    var m = item.Canon.RHS.Count;
//    var b = item.Canon.RHS.ToArray();
//    var Binv = item.BInverse; // double[m,m]
//    var xB = new double[m];
//    for (int i = 0; i < m; i++)
//    {
//        double s = 0;
//        for (int k = 0; k < m; k++) s += Binv[i, k] * b[k];
//        xB[i] = s;
//    }
//    return xB;
//}
