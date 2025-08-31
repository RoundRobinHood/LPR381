using Avalonia;
using Avalonia.Controls;
using Avalonia.Media;
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
    private TextBox? _outputBox;
    private TextBox? _iterationsBox;
    private TextBox? _objBox;
    private StackPanel? _panel;
    private ComboBox? _algoCombo;
    private ComboBox? _varTypeCombo;
    private TextBlock? _solverInfoText;
    private TextBox? _solutionSummaryBox;
    private TextBox? _sensitivityAnalysisBox;
    
    // Sensitivity Analysis controls
    private ComboBox? _saNonBasicVarSelect;
    private TextBox? _saNonBasicNewValue;
    private TextBlock? _saNonBasicStatus;
    private ComboBox? _saBasicVarSelect;
    private TextBox? _saBasicNewValue;
    private TextBlock? _saBasicStatus;
    private ComboBox? _saColumnVarSelect;
    private TextBox? _saColumnNewValue;
    private TextBlock? _saColumnStatus;
    private ComboBox? _saRhsConstraintSelect;
    private TextBox? _saRhsNewValue;
    private TextBlock? _saRhsStatus;
    private DataGrid? _saShadowPricesGrid;
    private TextBox? _saNewActivityName;
    private TextBox? _saNewActivityObjCoeff;
    private TextBox? _saNewActivityCoeffs;
    private TextBlock? _saNewActivityStatus;
    private TextBox? _saNewConstraintName;
    private TextBox? _saNewConstraintCoeffs;
    private TextBox? _saNewConstraintRhs;
    private TextBlock? _saNewConstraintStatus;
    private TextBox? _saDualityDisplay;
    private TextBlock? _saDualityStatus;
    private ISolverRunner? _lastRunner;
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
        _outputBox = this.FindControl<TextBox>("OutputBox");
        _iterationsBox = this.FindControl<TextBox>("IterationsBox");
        _objBox = this.FindControl<TextBox>("ObjectiveTextBox");
        _panel = this.FindControl<StackPanel>("ConstraintsPanel");
        _algoCombo = this.FindControl<ComboBox>("AlgorithmCombo");
        _varTypeCombo = this.FindControl<ComboBox>("VariableTypeCombo");
        _solverInfoText = this.FindControl<TextBlock>("SolverInfoText");
        _solutionSummaryBox = this.FindControl<TextBox>("SolutionSummaryBox");
        _sensitivityAnalysisBox = this.FindControl<TextBox>("SensitivityAnalysisBox");
        
        // Cache sensitivity analysis controls
        _saNonBasicVarSelect = this.FindControl<ComboBox>("SA_NonBasic_VarSelect");
        _saNonBasicNewValue = this.FindControl<TextBox>("SA_NonBasic_NewValue");
        _saNonBasicStatus = this.FindControl<TextBlock>("SA_NonBasic_Status");
        _saBasicVarSelect = this.FindControl<ComboBox>("SA_Basic_VarSelect");
        _saBasicNewValue = this.FindControl<TextBox>("SA_Basic_NewValue");
        _saBasicStatus = this.FindControl<TextBlock>("SA_Basic_Status");
        _saColumnVarSelect = this.FindControl<ComboBox>("SA_Column_VarSelect");
        _saColumnNewValue = this.FindControl<TextBox>("SA_Column_NewValue");
        _saColumnStatus = this.FindControl<TextBlock>("SA_Column_Status");
        _saRhsConstraintSelect = this.FindControl<ComboBox>("SA_RHS_ConstraintSelect");
        _saRhsNewValue = this.FindControl<TextBox>("SA_RHS_NewValue");
        _saRhsStatus = this.FindControl<TextBlock>("SA_RHS_Status");
        _saShadowPricesGrid = this.FindControl<DataGrid>("SA_ShadowPrices_Grid");
        _saNewActivityName = this.FindControl<TextBox>("SA_NewActivity_Name");
        _saNewActivityObjCoeff = this.FindControl<TextBox>("SA_NewActivity_ObjCoeff");
        _saNewActivityCoeffs = this.FindControl<TextBox>("SA_NewActivity_Coeffs");
        _saNewActivityStatus = this.FindControl<TextBlock>("SA_NewActivity_Status");
        _saNewConstraintName = this.FindControl<TextBox>("SA_NewConstraint_Name");
        _saNewConstraintCoeffs = this.FindControl<TextBox>("SA_NewConstraint_Coeffs");
        _saNewConstraintRhs = this.FindControl<TextBox>("SA_NewConstraint_RHS");
        _saNewConstraintStatus = this.FindControl<TextBlock>("SA_NewConstraint_Status");
        _saDualityDisplay = this.FindControl<TextBox>("SA_Duality_Display");
        _saDualityStatus = this.FindControl<TextBlock>("SA_Duality_Status");
        
        // Populate algorithm combos from registry
        if (_algoCombo != null)
        {
            _algoCombo.Items.Clear();
            foreach (var entry in SolverRegistry.Available)
            {
                _algoCombo.Items.Add(new ComboBoxItem { Content = entry.Display, Tag = entry.Key });
            }
            _algoCombo.SelectedIndex = 0;
        }
        
        var fileAlgoCombo = this.FindControl<ComboBox>("FileAlgorithmCombo");
        if (fileAlgoCombo != null)
        {
            fileAlgoCombo.Items.Clear();
            foreach (var entry in SolverRegistry.Available)
            {
                fileAlgoCombo.Items.Add(new ComboBoxItem { Content = entry.Display, Tag = entry.Key });
            }
            fileAlgoCombo.SelectedIndex = 0;
        }
    }

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
        if (_outputBox != null) _outputBox.Text = "";
        if (_iterationsBox != null) _iterationsBox.Text = "";

        try
        {
            if (_objBox == null || _panel == null || _algoCombo == null)
            {
                if (_outputBox != null) _outputBox.Text = "UI not ready.";
                return;
            }

            var input = new UserProblem
            {
                ObjectiveLine = _objBox.Text ?? "",
                Constraints = ReadConstraintLines(_panel),
                IntMode = GetVariableType()
            };

            var runner = CreateRunnerFromCombo(_algoCombo);
            
            // Quick validation without building full model
            if (runner.Key == "knapsack" && (input.Constraints?.Length != 1))
            {
                if (_outputBox != null) _outputBox.Text = "Knapsack requires exactly one constraint";
                return;
            }

            // Run
            var summary = await runner.RunAsync(input);
            _lastRunner = runner;

            if (_iterationsBox != null)
                _iterationsBox.Text = string.Join("\n\n", runner.Iterations.Select(IterationFormat.Pretty));

            if (_outputBox != null)
            {
                _outputBox.Text = $"{summary.Message}\nObjective = {summary.Objective}\n" +
                    string.Join("\n", summary.VariableValues.Select(kv => $"{kv.Key} = {kv.Value}"));
            }
            
            // Update solution summary tab
            if (_solutionSummaryBox != null)
            {
                _solutionSummaryBox.Text = $"{summary.Message}\n\nObjective Value: {summary.Objective}\n\nVariable Values:\n" +
                    string.Join("\n", summary.VariableValues.Select(kv => $"{kv.Key} = {kv.Value}"));
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

    private static ISolverRunner CreateRunnerFromCombo(ComboBox combo)
    {
        var selectedIndex = combo.SelectedIndex;
        if (selectedIndex < 0 || selectedIndex >= SolverRegistry.Available.Count)
            throw new InvalidOperationException("Please select a solver.");
        
        return SolverRegistry.Available[selectedIndex].Factory();
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
    

    
    private string GetVariableType()
    {
        if (_varTypeCombo?.SelectedItem is ComboBoxItem item)
            return item.Tag?.ToString() ?? "continuous";
        return "continuous";
    private void ClearButton_Click(object? sender, RoutedEventArgs e)
    {
        ClearAllInputs();
    }

    private void ClearAllInputs()
    {
        // Clear objective
        if (_objBox != null) _objBox.Text = "";
        
        // Reset objective type to no selection
        var objTypeCombo = this.FindControl<ComboBox>("ObjectiveTypeCombo");
        if (objTypeCombo != null) objTypeCombo.SelectedIndex = -1;
        
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
    
    private void AddConstraintRow(string lhs = "", string sign = "", string rhs = "")
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
        combo.SelectedIndex = sign switch { ">=" => 1, "=" => 2, "<=" => 0, _ => -1 };

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
            var tempRunner = new PrimalSimplexRunner();
            var formulation = ((SolverRunner)tempRunner).BuildFormulation(input);
            
            // Use the same canonical form generation as iterations
            tempRunner.GetType().GetMethod("AddCanonicalForm", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance)
                ?.Invoke(tempRunner, new object[] { formulation });
            
            if (tempRunner.Iterations.Count > 0)
            {
                var canonicalTableau = tempRunner.Iterations[0];
                previewBox.Text = IterationFormat.Pretty(canonicalTableau);
            }
            else
            {
                previewBox.Text = "No canonical form generated";
            }
        }
        catch (Exception ex)
        {
            if (_outputBox != null) _outputBox.Text = "Error: " + ex.Message;
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
                if (_outputBox != null) _outputBox.Text += $"\n\nResults exported to: {result}";
            }
            catch (Exception ex)
            {
                if (_outputBox != null) _outputBox.Text += $"\n\nExport failed: {ex.Message}";
            }
        }
    }
    
    // Sensitivity Analysis event handlers
    
    // Non-Basic Variables
    private void SA_NonBasic_DisplayRange_Click(object? sender, RoutedEventArgs e)
    {
        if (_saNonBasicStatus != null) _saNonBasicStatus.Text = "Non-basic variable range display not implemented yet.";
    }
    
    private void SA_NonBasic_ApplyChange_Click(object? sender, RoutedEventArgs e)
    {
        if (_saNonBasicStatus != null) _saNonBasicStatus.Text = "Non-basic variable change not implemented yet.";
    }
    
    // Basic Variables
    private void SA_Basic_DisplayRange_Click(object? sender, RoutedEventArgs e)
    {
        if (_saBasicStatus != null) _saBasicStatus.Text = "Basic variable range display not implemented yet.";
    }
    
    private void SA_Basic_ApplyChange_Click(object? sender, RoutedEventArgs e)
    {
        if (_saBasicStatus != null) _saBasicStatus.Text = "Basic variable change not implemented yet.";
    }
    
    // Non-Basic Variable Column
    private void SA_Column_DisplayRange_Click(object? sender, RoutedEventArgs e)
    {
        if (_saColumnStatus != null) _saColumnStatus.Text = "Variable column range display not implemented yet.";
    }
    
    private void SA_Column_ApplyChange_Click(object? sender, RoutedEventArgs e)
    {
        if (_saColumnStatus != null) _saColumnStatus.Text = "Variable column change not implemented yet.";
    }
    
    // RHS Analysis
    private void SA_RHS_DisplayRange_Click(object? sender, RoutedEventArgs e)
    {
        if (_saRhsStatus != null) _saRhsStatus.Text = "RHS range display not implemented yet.";
    }
    
    private void SA_RHS_ApplyChange_Click(object? sender, RoutedEventArgs e)
    {
        if (_saRhsStatus != null) _saRhsStatus.Text = "RHS change not implemented yet.";
    }
    
    // Shadow Prices
    private void SA_ShadowPrices_Display_Click(object? sender, RoutedEventArgs e)
    {
        // Display shadow prices in the grid
    }
    
    // Add New Activity
    private void SA_NewActivity_Add_Click(object? sender, RoutedEventArgs e)
    {
        if (_saNewActivityStatus != null) _saNewActivityStatus.Text = "Add new activity not implemented yet.";
    }
    
    // Add New Constraint
    private void SA_NewConstraint_Add_Click(object? sender, RoutedEventArgs e)
    {
        if (_saNewConstraintStatus != null) _saNewConstraintStatus.Text = "Add new constraint not implemented yet.";
    }
    
    // Duality
    private void SA_Duality_Apply_Click(object? sender, RoutedEventArgs e)
    {
        if (_saDualityStatus != null) _saDualityStatus.Text = "Apply duality not implemented yet.";
    }
    
    private void SA_Duality_Solve_Click(object? sender, RoutedEventArgs e)
    {
        if (_saDualityStatus != null) _saDualityStatus.Text = "Solve dual problem not implemented yet.";
    }
    
    private void SA_Duality_Verify_Click(object? sender, RoutedEventArgs e)
    {
        if (_saDualityStatus != null) _saDualityStatus.Text = "Verify duality not implemented yet.";
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
