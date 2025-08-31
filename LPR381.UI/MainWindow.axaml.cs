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

namespace LPR381.UI;

public partial class MainWindow : Window
{
    private TextBox? _iterationsBox;
    private TextBox? _objBox;
    private StackPanel? _panel;
    private TextBox? _solutionSummaryBox;
    
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
    private TextBlock? _saShadowPricesStatus;
    private TextBox? _saShadowPricesOutput;
    private TextBox? _saNewActivityName;
    private TextBox? _saNewActivityObjCoeff;
    private TextBox? _saNewActivityCoeffs;
    private TextBox? _saAddElementsOutput;
    private TextBox? _saNewConstraintName;
    private TextBox? _saNewConstraintCoeffs;
    private TextBox? _saNewConstraintRhs;
    private ComboBox? _saNewConstraintSign;
    private TextBox? _saDualityDisplay;
    private TextBlock? _saDualityStatus;
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
        _solutionSummaryBox = this.FindControl<TextBox>("SolutionSummaryBox");
        
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
        _saShadowPricesStatus = this.FindControl<TextBlock>("SA_ShadowPrices_Status");
        _saShadowPricesOutput = this.FindControl<TextBox>("SA_ShadowPrices_Output");
        _saNewActivityName = this.FindControl<TextBox>("SA_NewActivity_Name");
        _saNewActivityObjCoeff = this.FindControl<TextBox>("SA_NewActivity_ObjCoeff");
        _saNewActivityCoeffs = this.FindControl<TextBox>("SA_NewActivity_Coeffs");
        _saAddElementsOutput = this.FindControl<TextBox>("SA_AddElements_Output");
        _saNewConstraintName = this.FindControl<TextBox>("SA_NewConstraint_Name");
        _saNewConstraintCoeffs = this.FindControl<TextBox>("SA_NewConstraint_Coeffs");
        _saNewConstraintRhs = this.FindControl<TextBox>("SA_NewConstraint_RHS");
        _saNewConstraintSign = this.FindControl<ComboBox>("SA_NewConstraint_Sign");
        _saDualityDisplay = this.FindControl<TextBox>("SA_Duality_Display");
        _saDualityStatus = this.FindControl<TextBlock>("SA_Duality_Status");
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
        if (_iterationsBox != null) _iterationsBox.Text = "";
        if (_solutionSummaryBox != null) _solutionSummaryBox.Text = "";

        try
        {
            if (_objBox == null || _panel == null)
            {
                if (_solutionSummaryBox != null) _solutionSummaryBox.Text = "UI not ready.";
                return;
            }

            var input = GetUserInput();
            var runner = GetSelectedRunner();
            
            // Quick validation without building full model
            if (runner.Key == "knapsack" && (input.Constraints?.Length != 1))
            {
                if (_solutionSummaryBox != null) _solutionSummaryBox.Text = "Knapsack requires exactly one constraint";
                return;
            }

            // Run
            var summary = await runner.RunAsync(input);
            _lastRunner = runner;

            if (_iterationsBox != null)
                _iterationsBox.Text = string.Join("\n\n", runner.Iterations.Select(IterationFormat.Pretty));
            
            // Update solution summary tab
            if (_solutionSummaryBox != null)
            {
                _solutionSummaryBox.Text = $"{summary.Message}\n\nObjective Value: {summary.Objective}\n\nVariable Values:\n" +
                    string.Join("\n", summary.VariableValues.Select(kv => $"{kv.Key} = {kv.Value}"));
            }
            
            // Populate sensitivity analysis dropdowns after successful solve
            PopulateSensitivityAnalysis();
            
            // Clear any previous sensitivity analysis status messages
            if (_saNonBasicStatus != null) _saNonBasicStatus.Text = "Ready for analysis";
            if (_saBasicStatus != null) _saBasicStatus.Text = "Ready for analysis";
            if (_saColumnStatus != null) _saColumnStatus.Text = "Ready for analysis";
            if (_saRhsStatus != null) _saRhsStatus.Text = "Ready for analysis";
            
            // Switch to results tab
            var resultsTab = this.FindControl<TabItem>("ResultsTab");
            if (resultsTab?.Parent is TabControl mainTabControl)
            {
                mainTabControl.SelectedItem = resultsTab;
            }
        }
        catch (Exception ex)
        {
            if (_solutionSummaryBox != null) _solutionSummaryBox.Text = "Error: " + ex.Message;
            ClearSensitivityDropdowns();
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
            _panel?.Children.Remove(row);
        };

        // Add them to row
        row.Children.Add(coeffBox);
        row.Children.Add(combo);
        row.Children.Add(rhsBox);
        row.Children.Add(deleteBtn);

        // Add row to panel
        _panel?.Children.Add(row);
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
                        // Provide helpful error message for decimal format issues
                        if (error.Contains("Use period (.) for decimals"))
                        {
                            statusText.Text = $"❌ {error}";
                        }
                        else
                        {
                            statusText.Text = $"❌ File parsing error: {error}";
                        }
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
                    // Check for common file issues
                    if (ex.Message.Contains("Use period (.) for decimals"))
                    {
                        statusText.Text = $"❌ {ex.Message}";
                    }
                    else if (ex is FileNotFoundException)
                    {
                        statusText.Text = "❌ File not found";
                    }
                    else if (ex is UnauthorizedAccessException)
                    {
                        statusText.Text = "❌ Cannot access file (check permissions)";
                    }
                    else
                    {
                        statusText.Text = $"❌ Error loading file: {ex.Message}";
                    }
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
            var objTerms = formulation.VarNames.Zip(formulation.Objective, (name, coeff) => $"{coeff.ToString("+0.###;-0.###", CultureInfo.InvariantCulture)}{name}");
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
                        terms.Add($"{coeff.ToString("+0.###;-0.###", CultureInfo.InvariantCulture)}{formulation.VarNames[j]}");
                }
                var sign = formulation.ConstraintSigns[i] switch
                {
                    ConstraintSign.LessOrEqual => "<=",
                    ConstraintSign.GreaterOrEqual => ">=",
                    _ => "="
                };
                
                AddConstraintRow(string.Join(" ", terms), sign, formulation.RHS[i].ToString(CultureInfo.InvariantCulture));
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
            previewBox.Text = $"Error: {ex.Message}";
        }
    }
    
    private static UserProblem ConvertFormulationToUserProblem(LPFormulation formulation)
    {
        // Convert objective
        var objType = formulation.ObjectiveType == ObjectiveType.Max ? "max" : "min";
        var objTerms = formulation.VarNames.Zip(formulation.Objective, (name, coeff) => $"{coeff.ToString("+0.###;-0.###", CultureInfo.InvariantCulture)}{name}");
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
                    terms.Add($"{coeff.ToString("+0.###;-0.###", CultureInfo.InvariantCulture)}{formulation.VarNames[j]}");
            }
            var sign = formulation.ConstraintSigns[i] switch
            {
                ConstraintSign.LessOrEqual => "<=",
                ConstraintSign.GreaterOrEqual => ">=",
                _ => "="
            };
            constraints[i] = $"{string.Join(" ", terms)} {sign} {formulation.RHS[i].ToString(CultureInfo.InvariantCulture)}";
        }
        
        // Convert variable restrictions
        var signRestrictions = new Dictionary<string, SignRestriction>();
        var intRestrictions = new Dictionary<string, IntRestriction>();
        
        for (int i = 0; i < formulation.VarNames.Length; i++)
        {
            signRestrictions[formulation.VarNames[i]] = formulation.VarSignRestrictions[i];
            intRestrictions[formulation.VarNames[i]] = formulation.VarIntRestrictions[i];
        }
        
        return new UserProblem
        {
            ObjectiveLine = objLine,
            Constraints = constraints,
            IntMode = "continuous",
            VariableSignRestrictions = signRestrictions,
            VariableIntRestrictions = intRestrictions
        };
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
                if (_solutionSummaryBox != null) _solutionSummaryBox.Text += $"\n\nResults exported to: {result}";
            }
            catch (Exception ex)
            {
                if (_solutionSummaryBox != null) _solutionSummaryBox.Text += $"\n\nExport failed: {ex.Message}";
            }
        }
    }
    
    // Sensitivity Analysis functionality
    private void PopulateSensitivityAnalysis()
    {
        try
        {
            var context = GetSensitivityContext();
            if (context == null) 
            {
                ClearSensitivityDropdowns();
                return;
            }

            var formulation = context.Formulation;
            var basis = context.Basis;
            
            // Populate variable dropdowns
            var basicVars = new List<string>();
            var nonBasicVars = new List<string>();
            
            for (int i = 0; i < formulation.VarNames.Length; i++)
            {
                var varName = formulation.VarNames[i];
                if (context.IsInBasis(i))
                    basicVars.Add(varName);
                else
                    nonBasicVars.Add(varName);
            }
            
            // Basic variables dropdown
            if (_saBasicVarSelect != null)
            {
                _saBasicVarSelect.Items.Clear();
                if (basicVars.Count > 0)
                {
                    foreach (var var in basicVars)
                        _saBasicVarSelect.Items.Add(var);
                    _saBasicVarSelect.SelectedIndex = 0;
                }
                else
                {
                    _saBasicVarSelect.Items.Add("No basic variables available");
                    _saBasicVarSelect.SelectedIndex = 0;
                }
            }
            
            // Non-basic variables dropdown
            if (_saNonBasicVarSelect != null)
            {
                _saNonBasicVarSelect.Items.Clear();
                if (nonBasicVars.Count > 0)
                {
                    foreach (var var in nonBasicVars)
                        _saNonBasicVarSelect.Items.Add(var);
                    _saNonBasicVarSelect.SelectedIndex = 0;
                }
                else
                {
                    _saNonBasicVarSelect.Items.Add("No non-basic variables available");
                    _saNonBasicVarSelect.SelectedIndex = 0;
                }
            }
            
            // Non-basic variables dropdown for column analysis
            if (_saColumnVarSelect != null)
            {
                _saColumnVarSelect.Items.Clear();
                if (nonBasicVars.Count > 0)
                {
                    foreach (var var in nonBasicVars)
                        _saColumnVarSelect.Items.Add(var);
                    _saColumnVarSelect.SelectedIndex = 0;
                }
                else
                {
                    _saColumnVarSelect.Items.Add("No non-basic variables available");
                    _saColumnVarSelect.SelectedIndex = 0;
                }
            }
            
            // Constraints dropdown
            if (_saRhsConstraintSelect != null)
            {
                _saRhsConstraintSelect.Items.Clear();
                if (formulation.ConstraintSigns.Length > 0)
                {
                    for (int i = 0; i < formulation.ConstraintSigns.Length; i++)
                        _saRhsConstraintSelect.Items.Add($"Constraint {i + 1}");
                    _saRhsConstraintSelect.SelectedIndex = 0;
                }
                else
                {
                    _saRhsConstraintSelect.Items.Add("No constraints available");
                    _saRhsConstraintSelect.SelectedIndex = 0;
                }
            }
        }
        catch
        {
            ClearSensitivityDropdowns();
        }
    }
    
    private void ClearSensitivityDropdowns()
    {
        _saBasicVarSelect?.Items.Clear();
        _saNonBasicVarSelect?.Items.Clear();
        _saColumnVarSelect?.Items.Clear();
        _saRhsConstraintSelect?.Items.Clear();
    }
    
    private RelaxedSimplexSensitivityContext? GetSensitivityContext()
    {
            
        try
        {
            if(_lastRunner == null) return null;
            var formulation = ((SolverRunner)_lastRunner).BuildFormulation(GetUserInput());
            switch(_lastRunner?.Key) {
                case "primal-simplex":
                    return (new PrimalSimplex(formulation) as ITree<SimplexNode>)?.SensitivityContext;
                case "revised-primal-simplex":
                    return (new RevisedPrimalSimplex(formulation) as ITree<RevisedSimplexNode>)?.SensitivityContext;
                case "revised-dual-simplex":
                    return (new RevisedDualSimplex(formulation) as ITree<RevisedSimplexNode>)?.SensitivityContext;
                case "branch-and-bound":
                    return (new RevisedBranchAndBound(formulation) as ITree<RevisedBNBNode>)?.SensitivityContext;
                case "cutting-plane":
                    return (new RevisedCuttingPlanes(formulation) as ITree<RevisedCuttingPlanesNode>)?.SensitivityContext;
                default:
                    return null;
            }
        }
        catch
        {
            return null;
        }
    }
    
    // Sensitivity Analysis event handlers
    
    // Non-Basic Variables
    private void SA_NonBasic_DisplayRange_Click(object? sender, RoutedEventArgs e)
    {
        var context = GetSensitivityContext();
        var selectedVar = _saNonBasicVarSelect?.SelectedItem?.ToString();
        
        if (context == null || string.IsNullOrEmpty(selectedVar))
        {
            if (_saNonBasicStatus != null) _saNonBasicStatus.Text = "Please select a variable and ensure a solution exists.";
            return;
        }
        
        var varIndex = Array.IndexOf(context.Formulation.VarNames, selectedVar);
        if (varIndex >= 0)
        {
            var range = context.ObjectiveCoeffRange(varIndex);
            if (_saNonBasicStatus != null)
                _saNonBasicStatus.Text = $"Range: [{range.LowerBound:F3}, {range.UpperBound:F3}]";
        }
    }
    
    private void SA_NonBasic_ApplyChange_Click(object? sender, RoutedEventArgs e)
    {
        try
        {
            var context = GetSensitivityContext();
            var selectedVar = _saNonBasicVarSelect?.SelectedItem?.ToString();
            var newValueText = _saNonBasicNewValue?.Text?.Trim();
            
            if (context == null || string.IsNullOrEmpty(selectedVar))
            {
                if (_saNonBasicStatus != null) _saNonBasicStatus.Text = "Please select a variable and ensure a solution exists.";
                return;
            }
            
            if (string.IsNullOrEmpty(newValueText))
            {
                if (_saNonBasicStatus != null) _saNonBasicStatus.Text = "Please enter a new objective coefficient value.";
                return;
            }
            
            if (!double.TryParse(newValueText, out var newValue))
            {
                if (_saNonBasicStatus != null) _saNonBasicStatus.Text = "Please enter a valid number for the new coefficient.";
                return;
            }
            
            var varIndex = Array.IndexOf(context.Formulation.VarNames, selectedVar);
            if (varIndex < 0)
            {
                if (_saNonBasicStatus != null) _saNonBasicStatus.Text = "Selected variable not found in formulation.";
                return;
            }
            
            // Get the current range for validation
            var range = context.ObjectiveCoeffRange(varIndex);
            
            // Check if the new value is within the allowable range
            if (newValue < range.LowerBound || newValue > range.UpperBound)
            {
                if (_saNonBasicStatus != null) 
                    _saNonBasicStatus.Text = $"Warning: New value {newValue:F3} is outside the allowable range [{range.LowerBound:F3}, {range.UpperBound:F3}]. This may affect optimality.";
            }
            else
            {
                if (_saNonBasicStatus != null) 
                    _saNonBasicStatus.Text = $"New coefficient {newValue:F3} applied successfully. Value is within allowable range [{range.LowerBound:F3}, {range.UpperBound:F3}].";
            }
            
            // Display the change analysis
            var sb = new StringBuilder();
            sb.AppendLine($"NON-BASIC VARIABLE CHANGE ANALYSIS:");
            sb.AppendLine($"=====================================");
            sb.AppendLine($"Variable: {selectedVar}");
            sb.AppendLine($"Current Coefficient: {context.Formulation.Objective[varIndex]:F6}");
            sb.AppendLine($"New Coefficient: {newValue:F6}");
            sb.AppendLine($"Change: {newValue - context.Formulation.Objective[varIndex]:F6}");
            sb.AppendLine($"Allowable Range: [{range.LowerBound:F6}, {range.UpperBound:F6}]");
            sb.AppendLine();
            
            if (newValue < range.LowerBound || newValue > range.UpperBound)
            {
                sb.AppendLine("WARNING: New value is outside the allowable range!");
                sb.AppendLine("This change may:");
                sb.AppendLine("- Make the current solution infeasible");
                sb.AppendLine("- Require re-solving the problem");
                sb.AppendLine("- Change the optimal solution");
            }
            else
            {
                sb.AppendLine("New value is within the allowable range.");
                sb.AppendLine("The current solution remains optimal with this change.");
            }
            
            // Update the display in the main text area if available
            if (_saNonBasicStatus != null)
            {
                var currentText = _saNonBasicStatus.Text;
                _saNonBasicStatus.Text = currentText + "\n\n" + sb.ToString();
            }
        }
        catch (Exception ex)
        {
            if (_saNonBasicStatus != null) _saNonBasicStatus.Text = $"Error applying variable change: {ex.Message}";
        }
    }
    
    // Basic Variables
    private void SA_Basic_DisplayRange_Click(object? sender, RoutedEventArgs e)
    {
        var context = GetSensitivityContext();
        var selectedVar = _saBasicVarSelect?.SelectedItem?.ToString();
        
        if (context == null || string.IsNullOrEmpty(selectedVar))
        {
            if (_saBasicStatus != null) _saBasicStatus.Text = "Please select a variable and ensure a solution exists.";
            return;
        }
        
        var varIndex = Array.IndexOf(context.Formulation.VarNames, selectedVar);
        if (varIndex >= 0)
        {
            var range = context.ObjectiveCoeffRange(varIndex);
            if (_saBasicStatus != null)
                _saBasicStatus.Text = $"Range: [{range.LowerBound:F3}, {range.UpperBound:F3}]";
        }
    }
    
    private void SA_Basic_ApplyChange_Click(object? sender, RoutedEventArgs e)
    {
        try
        {
            var context = GetSensitivityContext();
            var selectedVar = _saBasicVarSelect?.SelectedItem?.ToString();
            var newValueText = _saBasicNewValue?.Text?.Trim();
            
            if (context == null || string.IsNullOrEmpty(selectedVar))
            {
                if (_saBasicStatus != null) _saBasicStatus.Text = "Please select a variable and ensure a solution exists.";
                return;
            }
            
            if (string.IsNullOrEmpty(newValueText))
            {
                if (_saBasicStatus != null) _saBasicStatus.Text = "Please enter a new objective coefficient value.";
                return;
            }
            
            if (!double.TryParse(newValueText, out var newValue))
            {
                if (_saBasicStatus != null) _saBasicStatus.Text = "Please enter a valid number for the new coefficient.";
                return;
            }
            
            var varIndex = Array.IndexOf(context.Formulation.VarNames, selectedVar);
            if (varIndex < 0)
            {
                if (_saBasicStatus != null) _saBasicStatus.Text = "Selected variable not found in formulation.";
                return;
            }
            
            // Get the current range for validation
            var range = context.ObjectiveCoeffRange(varIndex);
            
            // Check if the new value is within the allowable range
            if (newValue < range.LowerBound || newValue > range.UpperBound)
            {
                if (_saBasicStatus != null) 
                    _saBasicStatus.Text = $"Warning: New value {newValue:F3} is outside the allowable range [{range.LowerBound:F3}, {range.UpperBound:F3}]. This may affect optimality.";
            }
            else
            {
                if (_saBasicStatus != null) 
                    _saBasicStatus.Text = $"New coefficient {newValue:F3} applied successfully. Value is within allowable range [{range.LowerBound:F3}, {range.UpperBound:F3}].";
            }
            
            // Display the change analysis
            var sb = new StringBuilder();
            sb.AppendLine($"BASIC VARIABLE CHANGE ANALYSIS:");
            sb.AppendLine($"===============================");
            sb.AppendLine($"Variable: {selectedVar}");
            sb.AppendLine($"Current Coefficient: {context.Formulation.Objective[varIndex]:F6}");
            sb.AppendLine($"New Coefficient: {newValue:F6}");
            sb.AppendLine($"Change: {newValue - context.Formulation.Objective[varIndex]:F6}");
            sb.AppendLine($"Allowable Range: [{range.LowerBound:F6}, {range.UpperBound:F6}]");
            sb.AppendLine();
            
            if (newValue < range.LowerBound || newValue > range.UpperBound)
            {
                sb.AppendLine("  WARNING: New value is outside the allowable range!");
                sb.AppendLine("This change may:");
                sb.AppendLine("- Make the current solution infeasible");
                sb.AppendLine("- Require re-solving the problem");
                sb.AppendLine("- Change the optimal solution");
                sb.AppendLine("- Affect the basis structure");
            }
            else
            {
                sb.AppendLine(" New value is within the allowable range.");
                sb.AppendLine("The current solution remains optimal with this change.");
                sb.AppendLine("Note: Basic variables have more restrictive ranges than non-basic variables.");
            }
            
            // Update the display in the main text area if available
            if (_saBasicStatus != null)
            {
                var currentText = _saBasicStatus.Text;
                _saBasicStatus.Text = currentText + "\n\n" + sb.ToString();
            }
        }
        catch (Exception ex)
        {
            if (_saBasicStatus != null) _saBasicStatus.Text = $"Error applying variable change: {ex.Message}";
        }
    }
    
    // Non-Basic Variable Column
    private void SA_Column_DisplayRange_Click(object? sender, RoutedEventArgs e)
    {
        var context = GetSensitivityContext();
        var selectedVar = _saColumnVarSelect?.SelectedItem?.ToString();
        
        if (context == null || string.IsNullOrEmpty(selectedVar))
        {
            if (_saColumnStatus != null) _saColumnStatus.Text = "Please select a variable and ensure a solution exists.";
            return;
        }
        
        var varIndex = Array.IndexOf(context.Formulation.VarNames, selectedVar);
        if (varIndex >= 0)
        {
            var range = context.ObjectiveCoeffRange(varIndex);
            if (_saColumnStatus != null)
                _saColumnStatus.Text = $"Range: [{range.LowerBound:F3}, {range.UpperBound:F3}]";
        }
    }
    
    private void SA_Column_ApplyChange_Click(object? sender, RoutedEventArgs e)
    {
        try
        {
            var context = GetSensitivityContext();
            var selectedVar = _saColumnVarSelect?.SelectedItem?.ToString();
            var newValueText = _saColumnNewValue?.Text?.Trim();
            
            if (context == null || string.IsNullOrEmpty(selectedVar))
            {
                if (_saColumnStatus != null) _saColumnStatus.Text = "Please select a variable and ensure a solution exists.";
                return;
            }
            
            if (string.IsNullOrEmpty(newValueText))
            {
                if (_saColumnStatus != null) _saColumnStatus.Text = "Please enter a new objective coefficient value.";
                return;
            }
            
            if (!double.TryParse(newValueText, out var newValue))
            {
                if (_saColumnStatus != null) _saColumnStatus.Text = "Please enter a valid number for the new coefficient.";
                return;
            }
            
            var varIndex = Array.IndexOf(context.Formulation.VarNames, selectedVar);
            if (varIndex < 0)
            {
                if (_saColumnStatus != null) _saColumnStatus.Text = "Selected variable not found in formulation.";
                return;
            }
            
            // Get the current range for validation
            var range = context.ObjectiveCoeffRange(varIndex);
            
            // Check if the new value is within the allowable range
            if (newValue < range.LowerBound || newValue > range.UpperBound)
            {
                if (_saColumnStatus != null) 
                    _saColumnStatus.Text = $"Warning: New value {newValue:F3} is outside the allowable range [{range.LowerBound:F3}, {range.UpperBound:F3}]. This may affect optimality.";
            }
            else
            {
                if (_saColumnStatus != null) 
                    _saColumnStatus.Text = $"New coefficient {newValue:F3} applied successfully. Value is within allowable range [{range.LowerBound:F3}, {range.UpperBound:F3}].";
            }
            
            // Display the change analysis
            var sb = new StringBuilder();
            sb.AppendLine($"COLUMN VARIABLE CHANGE ANALYSIS:");
            sb.AppendLine($"=================================");
            sb.AppendLine($"Variable: {selectedVar}");
            sb.AppendLine($"Current Coefficient: {context.Formulation.Objective[varIndex]:F6}");
            sb.AppendLine($"New Coefficient: {newValue:F6}");
            sb.AppendLine($"Change: {newValue - context.Formulation.Objective[varIndex]:F6}");
            sb.AppendLine($"Allowable Range: [{range.LowerBound:F6}, {range.UpperBound:F6}]");
            sb.AppendLine();
            
            if (newValue < range.LowerBound || newValue > range.UpperBound)
            {
                sb.AppendLine(" WARNING: New value is outside the allowable range!");
                sb.AppendLine("This change may:");
                sb.AppendLine("- Make the current solution infeasible");
                sb.AppendLine("- Require re-solving the problem");
                sb.AppendLine("- Change the optimal solution");
                sb.AppendLine("- Affect the reduced costs");
            }
            else
            {
                sb.AppendLine(" New value is within the allowable range.");
                sb.AppendLine("The current solution remains optimal with this change.");
                sb.AppendLine("Note: Column analysis examines the impact of coefficient changes on the entire column.");
            }
            
            // Update the display in the main text area if available
            if (_saColumnStatus != null)
            {
                var currentText = _saColumnStatus.Text;
                _saColumnStatus.Text = currentText + "\n\n" + sb.ToString();
            }
        }
        catch (Exception ex)
        {
            if (_saColumnStatus != null) _saColumnStatus.Text = $"Error applying variable change: {ex.Message}";
        }
    }
    
    // RHS Analysis
    private void SA_RHS_DisplayRange_Click(object? sender, RoutedEventArgs e)
    {
        var context = GetSensitivityContext();
        var selectedIndex = _saRhsConstraintSelect?.SelectedIndex;
        
        if (context == null || !selectedIndex.HasValue || selectedIndex < 0)
        {
            if (_saRhsStatus != null) _saRhsStatus.Text = "Please select a constraint and ensure a solution exists.";
            return;
        }
        
        var range = context.RHSRange(selectedIndex.Value);
        if (_saRhsStatus != null)
            _saRhsStatus.Text = $"Range: [{range.LowerBound:F3}, {range.UpperBound:F3}]";
    }
    
    private void SA_RHS_ApplyChange_Click(object? sender, RoutedEventArgs e)
    {
        try
        {
            var context = GetSensitivityContext();
            var selectedIndex = _saRhsConstraintSelect?.SelectedIndex;
            var newValueText = _saRhsNewValue?.Text?.Trim();
            
            if (context == null || !selectedIndex.HasValue || selectedIndex < 0)
            {
                if (_saRhsStatus != null) _saRhsStatus.Text = "Please select a constraint and ensure a solution exists.";
                return;
            }
            
            if (string.IsNullOrEmpty(newValueText))
            {
                if (_saRhsStatus != null) _saRhsStatus.Text = "Please enter a new right-hand-side value.";
                return;
            }
            
            if (!double.TryParse(newValueText, out var newValue))
            {
                if (_saRhsStatus != null) _saRhsStatus.Text = "Please enter a valid number for the new RHS value.";
                return;
            }
            
            var constraintIndex = selectedIndex.Value;
            if (constraintIndex >= context.Formulation.RHS.Length)
            {
                if (_saRhsStatus != null) _saRhsStatus.Text = "Selected constraint index is out of range.";
                return;
            }
            
            // Get the current range for validation
            var range = context.RHSRange(constraintIndex);
            var currentRHS = context.Formulation.RHS[constraintIndex];
            
            // Check if the new value is within the allowable range
            if (newValue < range.LowerBound || newValue > range.UpperBound)
            {
                if (_saRhsStatus != null) 
                    _saRhsStatus.Text = $"Warning: New RHS value {newValue:F3} is outside the allowable range [{range.LowerBound:F3}, {range.UpperBound:F3}]. This may affect feasibility.";
            }
            else
            {
                if (_saRhsStatus != null) 
                    _saRhsStatus.Text = $"New RHS value {newValue:F3} applied successfully. Value is within allowable range [{range.LowerBound:F3}, {range.UpperBound:F3}].";
            }
            
            // Get shadow price for this constraint
            var shadowPrice = context.ShadowPrices[constraintIndex];
            
            // Display the change analysis
            var sb = new StringBuilder();
            sb.AppendLine($"RHS CONSTRAINT CHANGE ANALYSIS:");
            sb.AppendLine($"=================================");
            sb.AppendLine($"Constraint Index: {constraintIndex + 1}");
            sb.AppendLine($"Current RHS: {currentRHS:F6}");
            sb.AppendLine($"New RHS: {newValue:F6}");
            sb.AppendLine($"Change: {newValue - currentRHS:F6}");
            sb.AppendLine($"Allowable Range: [{range.LowerBound:F6}, {range.UpperBound:F6}]");
            sb.AppendLine($"Shadow Price: {shadowPrice:F6}");
            sb.AppendLine();
            
            if (newValue < range.LowerBound || newValue > range.UpperBound)
            {
                sb.AppendLine(" WARNING: New RHS value is outside the allowable range!");
                sb.AppendLine("This change may:");
                sb.AppendLine("- Make the current solution infeasible");
                sb.AppendLine("- Require re-solving the problem");
                sb.AppendLine("- Change the optimal solution");
                sb.AppendLine("- Affect the basis structure");
            }
            else
            {
                sb.AppendLine(" New RHS value is within the allowable range.");
                sb.AppendLine("The current solution remains feasible with this change.");
                
                // Calculate the impact on objective value
                var objectiveChange = shadowPrice * (newValue - currentRHS);
                sb.AppendLine($"Estimated Objective Change: {objectiveChange:F6}");
                sb.AppendLine($"Shadow Price Interpretation: {GetShadowPriceInterpretation(shadowPrice)}");
            }
            
            // Update the display in the main text area if available
            if (_saRhsStatus != null)
            {
                var currentText = _saRhsStatus.Text;
                _saRhsStatus.Text = currentText + "\n\n" + sb.ToString();
            }
        }
        catch (Exception ex)
        {
            if (_saRhsStatus != null) _saRhsStatus.Text = $"Error applying RHS change: {ex.Message}";
        }
    }
    
    // Shadow Prices
    private void SA_ShadowPrices_Display_Click(object? sender, RoutedEventArgs e)
    {
        try
        {
            var context = GetSensitivityContext();
            if (context == null)
            {
                if (_saShadowPricesStatus != null) _saShadowPricesStatus.Text = "No solution available for shadow price analysis";
                if (_saShadowPricesOutput != null) _saShadowPricesOutput.Text = "No solution available";
                return;
            }
            
            var shadowPrices = context.ShadowPrices;
            var items = new List<ShadowPriceItem>();
            var output = new StringBuilder();
            output.AppendLine("SHADOW PRICES:");
            
            for (int i = 0; i < shadowPrices.Length; i++)
            {
                var interpretation = shadowPrices[i] > 0 ? "Increasing RHS improves objective" :
                                   shadowPrices[i] < 0 ? "Increasing RHS worsens objective" :
                                   "RHS change has no effect";
                
                items.Add(new ShadowPriceItem
                {
                    Name = $"Constraint {i + 1}",
                    Value = shadowPrices[i],
                    Interpretation = interpretation
                });
                
                output.AppendLine($"Constraint {i + 1}: {shadowPrices[i]:F6} - {interpretation}");
            }
            
            if (_saShadowPricesGrid != null)
                _saShadowPricesGrid.ItemsSource = items;
            if (_saShadowPricesOutput != null)
                _saShadowPricesOutput.Text = output.ToString();
            if (_saShadowPricesStatus != null) _saShadowPricesStatus.Text = $"Displayed {items.Count} shadow prices";
        }
        catch (Exception ex)
        {
            if (_saShadowPricesStatus != null) _saShadowPricesStatus.Text = $"Error: {ex.Message}";
            if (_saShadowPricesOutput != null) _saShadowPricesOutput.Text = $"Error: {ex.Message}";
        }
    }
    
    // Add New Activity
    private async void SA_NewActivity_Add_Click(object? sender, RoutedEventArgs e)
    {
        try
        {
            var context = GetSensitivityContext();
            if (context == null)
            {
                if (_saAddElementsOutput != null) _saAddElementsOutput.Text = "No solution available for adding activity.";
                return;
            }
            
            var activityName = _saNewActivityName?.Text?.Trim();
            var objCoeffText = _saNewActivityObjCoeff?.Text?.Trim();
            var coeffsText = _saNewActivityCoeffs?.Text?.Trim();
            
            if (string.IsNullOrEmpty(activityName) || string.IsNullOrEmpty(objCoeffText) || string.IsNullOrEmpty(coeffsText))
            {
                if (_saAddElementsOutput != null) _saAddElementsOutput.Text = "Please fill in all fields.";
                return;
            }
            
            if (!double.TryParse(objCoeffText, CultureInfo.InvariantCulture, out var objCoeff))
            {
                if (_saAddElementsOutput != null) _saAddElementsOutput.Text = "Invalid objective coefficient. Use period (.) for decimals.";
                return;
            }
            
            var coeffs = coeffsText.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                .Select(s => double.TryParse(s, CultureInfo.InvariantCulture, out var val) ? val : double.NaN)
                .ToArray();
                
            if (coeffs.Any(double.IsNaN))
            {
                if (_saAddElementsOutput != null) _saAddElementsOutput.Text = "Invalid coefficients. Use period (.) for decimals and space-separate values.";
                return;
            }
            
            if (coeffs.Length != context.Formulation.ConstraintSigns.Length)
            {
                if (_saAddElementsOutput != null) _saAddElementsOutput.Text = $"Expected {context.Formulation.ConstraintSigns.Length} coefficients (one for each constraint), got {coeffs.Length}.";
                return;
            }
            
            // Create new formulation with added activity
            var newFormulation = context.Formulation.WithActivity(activityName, objCoeff, coeffs, SignRestriction.Positive);
            
            // Create RevisedPrimalSimplex for new formulation with added activity
            var newTree = new RevisedPrimalSimplex(newFormulation);
            var result = Explorer.SolveSimplex(newTree);
            
            var sb = new StringBuilder();
            sb.AppendLine($"NEW ACTIVITY '{activityName}' ADDED:");
            sb.AppendLine($"Objective coefficient: {objCoeff}");
            sb.AppendLine($"Constraint coefficients: [{string.Join(", ", coeffs)}]");
            sb.AppendLine();
            
            // Show original solution for comparison
            if (_lastRunner != null)
            {
                var originalInput = GetUserInput();
                var originalSummary = await _lastRunner.RunAsync(originalInput);
                sb.AppendLine("ORIGINAL SOLUTION:");
                sb.AppendLine($"Objective Value: {originalSummary.Objective:F6}");
                foreach (var kv in originalSummary.VariableValues)
                {
                    sb.AppendLine($"{kv.Key} = {kv.Value:F6}");
                }
                sb.AppendLine();
            }
            
            sb.AppendLine("NEW SOLUTION (with added activity):");
            
            var (resultCase, resultFields) = FSharpInterop.ReadUnion(result);
            
            if (resultCase == "Optimal")
            {
                var objValue = (double)resultFields[2];
                var varValues = (Dictionary<string, double>)resultFields[1];
                
                sb.AppendLine($"Objective Value: {objValue:F6}");
                sb.AppendLine("Variable Values:");
                foreach (var kv in varValues)
                {
                    sb.AppendLine($"{kv.Key} = {kv.Value:F6}");
                }
                
                // Add optimal tableau display
                sb.AppendLine();
                sb.AppendLine("OPTIMAL TABLEAU:");
                try
                {
                    // Use PrimalSimplexRunner to get complete tableau
                    var tempRunner = new PrimalSimplexRunner();
                    var tempInput = ConvertFormulationToUserProblem(newFormulation);
                    await tempRunner.RunAsync(tempInput);
                    
                    if (tempRunner.Iterations.Count > 0)
                    {
                        var finalIteration = tempRunner.Iterations.Last();
                        sb.AppendLine(IterationFormat.Pretty(finalIteration));
                    }
                    else
                    {
                        sb.AppendLine("No tableau available");
                    }
                }
                catch (Exception ex)
                {
                    sb.AppendLine($"Could not generate tableau: {ex.Message}");
                }

            }
            else
            {
                sb.AppendLine($"Result: {resultCase}");
            }
            
            if (_saAddElementsOutput != null) _saAddElementsOutput.Text = sb.ToString();
        }
        catch (Exception ex)
        {
            if (_saAddElementsOutput != null) _saAddElementsOutput.Text = $"Error: {ex.Message}";
        }
    }
    
    // Add New Constraint
    private async void SA_NewConstraint_Add_Click(object? sender, RoutedEventArgs e)
    {
        try
        {
            var context = GetSensitivityContext();
            if (context == null)
            {
                if (_saAddElementsOutput != null) _saAddElementsOutput.Text = "No solution available for adding constraint.";
                return;
            }
            
            var constraintName = _saNewConstraintName?.Text?.Trim();
            var coeffsText = _saNewConstraintCoeffs?.Text?.Trim();
            var rhsText = _saNewConstraintRhs?.Text?.Trim();
            var signText = (_saNewConstraintSign?.SelectedItem as ComboBoxItem)?.Content?.ToString();
            
            if (string.IsNullOrEmpty(constraintName) || string.IsNullOrEmpty(coeffsText) || string.IsNullOrEmpty(rhsText) || string.IsNullOrEmpty(signText))
            {
                if (_saAddElementsOutput != null) _saAddElementsOutput.Text = "Please fill in all fields.";
                return;
            }
            
            if (!double.TryParse(rhsText, CultureInfo.InvariantCulture, out var rhs))
            {
                if (_saAddElementsOutput != null) _saAddElementsOutput.Text = "Invalid RHS value. Use period (.) for decimals.";
                return;
            }
            
            var coeffs = coeffsText.Split(' ', StringSplitOptions.RemoveEmptyEntries)
                .Select(s => double.TryParse(s, CultureInfo.InvariantCulture, out var val) ? val : double.NaN)
                .ToArray();
                
            if (coeffs.Any(double.IsNaN))
            {
                if (_saAddElementsOutput != null) _saAddElementsOutput.Text = "Invalid coefficients. Use period (.) for decimals and space-separate values.";
                return;
            }
            
            if (coeffs.Length != context.Formulation.VarNames.Length)
            {
                if (_saAddElementsOutput != null) _saAddElementsOutput.Text = $"Expected {context.Formulation.VarNames.Length} coefficients (one for each variable: {string.Join(", ", context.Formulation.VarNames)}), got {coeffs.Length}.\nExample: For constraint 'x2 <= 5', enter coefficients '0 1 0' (0 for x1, 1 for x2, 0 for x3).";
                return;
            }
            
            // Parse constraint sign
            var constraintSign = signText switch
            {
                "<=" => ConstraintSign.LessOrEqual,
                ">=" => ConstraintSign.GreaterOrEqual,
                "=" => ConstraintSign.Equal,
                _ => ConstraintSign.LessOrEqual
            };
            
            // Create constraint terms
            var constraintTerms = context.Formulation.VarNames.Zip(coeffs, (name, coeff) => Tuple.Create(coeff, name)).ToArray();
            var constraint = new LPConstraint(constraintTerms, constraintSign, rhs);
            
            // Create new formulation with added constraint
            var newFormulation = context.Formulation.WithConstraint(constraint);
            
            // Use RevisedDualSimplex to solve the new problem with added constraint
            var newTree = new RevisedDualSimplex(newFormulation);
            var result = Explorer.SolveSimplex(newTree);
            
            // Parse the F# result
            var (resultCase, resultFields) = FSharpInterop.ReadUnion(result);
            
            bool isOptimal = resultCase == "Optimal";
            double objectiveValue = 0.0;
            Dictionary<string, double> variableValues = new Dictionary<string, double>();
            string resultMessage = resultCase;
            
            if (isOptimal)
            {
                objectiveValue = (double)resultFields[2];
                variableValues = (Dictionary<string, double>)resultFields[1];
            }
            
            var sb = new StringBuilder();
            sb.AppendLine($"NEW CONSTRAINT '{constraintName}' ADDED:");
            
            // Display constraint in readable format
            var constraintDisplay = new List<string>();
            for (int i = 0; i < context.Formulation.VarNames.Length; i++)
            {
                if (Math.Abs(coeffs[i]) > 1e-9)
                {
                    var sign = coeffs[i] >= 0 && constraintDisplay.Count > 0 ? " + " : coeffs[i] < 0 ? " - " : "";
                    var absCoeff = Math.Abs(coeffs[i]);
                    var coeffStr = absCoeff == 1.0 ? "" : absCoeff.ToString("G3");
                    constraintDisplay.Add($"{sign}{coeffStr}{context.Formulation.VarNames[i]}");
                }
            }
            if (constraintDisplay.Count == 0) constraintDisplay.Add("0");
            sb.AppendLine($"Constraint: {string.Join(" ", constraintDisplay)} {signText} {rhs:G3}");
            sb.AppendLine();
            
            // Show original solution for comparison
            if (_lastRunner != null)
            {
                var originalInput = GetUserInput();
                var originalSummary = await _lastRunner.RunAsync(originalInput);
                sb.AppendLine("ORIGINAL SOLUTION:");
                sb.AppendLine($"Objective Value: {originalSummary.Objective:F6}");
                foreach (var kv in originalSummary.VariableValues)
                {
                    sb.AppendLine($"{kv.Key} = {kv.Value:F6}");
                }
                sb.AppendLine();
            }
            
            sb.AppendLine("NEW SOLUTION (with added constraint):");
            
            if (isOptimal)
            {
                sb.AppendLine($"Objective Value: {objectiveValue:F6}");
                sb.AppendLine("Variable Values:");
                foreach (var kv in variableValues)
                {
                    sb.AppendLine($"{kv.Key} = {kv.Value:F6}");
                }
                
                // Check if constraint is binding
                var constraintValue = 0.0;
                for (int i = 0; i < context.Formulation.VarNames.Length; i++)
                {
                    if (variableValues.TryGetValue(context.Formulation.VarNames[i], out var varValue))
                    {
                        constraintValue += coeffs[i] * varValue;
                    }
                }
                sb.AppendLine();
                sb.AppendLine($"Constraint evaluation: {constraintValue:F6} {signText} {rhs:F6}");
                
                bool isBinding = false;
                switch (signText)
                {
                    case "<=":
                        isBinding = Math.Abs(constraintValue - rhs) < 1e-6;
                        break;
                    case ">=":
                        isBinding = Math.Abs(constraintValue - rhs) < 1e-6;
                        break;
                    case "=":
                        isBinding = Math.Abs(constraintValue - rhs) < 1e-6;
                        break;
                }
                
                sb.AppendLine($"Constraint is {(isBinding ? "BINDING (active)" : "NOT BINDING (slack exists)")}");
                if (!isBinding && signText == "<=")
                {
                    sb.AppendLine($"Slack: {rhs - constraintValue:F6}");
                }
                
                // Add optimal tableau display
                sb.AppendLine();
                sb.AppendLine("OPTIMAL TABLEAU:");
                try
                {
                    // Use PrimalSimplexRunner to get complete tableau
                    var tempRunner = new PrimalSimplexRunner();
                    var tempInput = ConvertFormulationToUserProblem(newFormulation);
                    await tempRunner.RunAsync(tempInput);
                    
                    if (tempRunner.Iterations.Count > 0)
                    {
                        var finalIteration = tempRunner.Iterations.Last();
                        sb.AppendLine(IterationFormat.Pretty(finalIteration));
                    }
                    else
                    {
                        sb.AppendLine("No tableau available");
                    }
                }
                catch (Exception ex)
                {
                    sb.AppendLine($"Could not generate tableau: {ex.Message}");
                }

            }
            else
            {
                sb.AppendLine($"Result: {resultMessage}");
            }
            
            if (_saAddElementsOutput != null) _saAddElementsOutput.Text = sb.ToString();
        }
        catch (Exception ex)
        {
            if (_saAddElementsOutput != null) _saAddElementsOutput.Text = $"Error: {ex.Message}";
        }
    }
    
    // Duality
    private SimplexResult? _primalResult;
    private SimplexResult? _dualResult;
    
    private void SA_Duality_Apply_Click(object? sender, RoutedEventArgs e)
    {
        try
        {
            var context = GetSensitivityContext();
            if (context == null)
            {
                if (_saDualityStatus != null) _saDualityStatus.Text = "No solution available for duality analysis.";
                return;
            }
            
            var dualFormulation = context.GetDualFormulation().ToLPFormulation();
            var sb = new StringBuilder();
            sb.AppendLine("DUAL FORMULATION:");
            sb.AppendLine($"Objective: {dualFormulation.ObjectiveType}");
            sb.AppendLine();
            
            // Format objective function
            sb.Append(dualFormulation.ObjectiveType == ObjectiveType.Max ? "Maximize: " : "Minimize: ");
            for (int i = 0; i < dualFormulation.VarNames.Length; i++)
            {
                var coeff = dualFormulation.Objective[i];
                if (i == 0)
                    sb.Append($"{coeff:F3}{dualFormulation.VarNames[i]}");
                else
                    sb.Append(coeff >= 0 ? $" + {coeff:F3}{dualFormulation.VarNames[i]}" : $" - {Math.Abs(coeff):F3}{dualFormulation.VarNames[i]}");
            }
            sb.AppendLine();
            sb.AppendLine();
            sb.AppendLine("Subject to:");
            
            // Format constraints
            for (int i = 0; i < dualFormulation.ConstraintSigns.Length; i++)
            {
                for (int j = 0; j < dualFormulation.VarNames.Length; j++)
                {
                    var coeff = dualFormulation.ConstraintCoefficients[i, j];
                    if (j == 0)
                        sb.Append($"{coeff:F3}{dualFormulation.VarNames[j]}");
                    else
                        sb.Append(coeff >= 0 ? $" + {coeff:F3}{dualFormulation.VarNames[j]}" : $" - {Math.Abs(coeff):F3}{dualFormulation.VarNames[j]}");
                }
                var sign = dualFormulation.ConstraintSigns[i] switch
                {
                    ConstraintSign.LessOrEqual => "<=",
                    ConstraintSign.GreaterOrEqual => ">=",
                    _ => "="
                };
                sb.AppendLine($" {sign} {dualFormulation.RHS[i]:F3}");
            }
            
            if (_saDualityDisplay != null) _saDualityDisplay.Text = sb.ToString();
            if (_saDualityStatus != null) _saDualityStatus.Text = "Dual formulation generated successfully.";
        }
        catch (Exception ex)
        {
            if (_saDualityStatus != null) _saDualityStatus.Text = $"Error generating dual: {ex.Message}";
        }
    }
    
    private async void SA_Duality_Solve_Click(object? sender, RoutedEventArgs e)
    {
        try
        {
            var context = GetSensitivityContext();
            if (context == null)
            {
                if (_saDualityStatus != null) _saDualityStatus.Text = "No solution available for dual solving.";
                return;
            }
            
            // Store primal result from current solution  
            if (_lastRunner != null)
            {
                var primalSummary = await _lastRunner.RunAsync(GetUserInput());
                _primalResult = primalSummary.IsOptimal ? 
                    SimplexResult.NewOptimal(new Dictionary<string, double>(), primalSummary.VariableValues, primalSummary.Objective) :
                    null;
            }
            
            // Use shadow prices as dual solution (optimal dual variables)
            var shadowPrices = context.ShadowPrices;
            var dualVariables = new Dictionary<string, double>();
            var dualObjective = 0.0;
            
            for (int i = 0; i < shadowPrices.Length; i++)
            {
                var dualVarName = $"y{i + 1}";
                dualVariables[dualVarName] = shadowPrices[i];
                dualObjective += context.Formulation.RHS[i] * shadowPrices[i];
            }
            
            _dualResult = SimplexResult.NewOptimal(new Dictionary<string, double>(), dualVariables, dualObjective);
            
            var sb = new StringBuilder();
            sb.AppendLine("DUAL SOLUTION:");
            sb.AppendLine($"Objective Value: {dualObjective:F6}");
            sb.AppendLine();
            sb.AppendLine("Variable Values (Shadow Prices):");
            foreach (var kv in dualVariables)
            {
                sb.AppendLine($"{kv.Key} = {kv.Value:F6}");
            }
            
            if (_saDualityDisplay != null) _saDualityDisplay.Text += "\n\n" + sb.ToString();
            if (_saDualityStatus != null) _saDualityStatus.Text = "Dual solution obtained from shadow prices.";
        }
        catch (Exception ex)
        {
            if (_saDualityStatus != null) _saDualityStatus.Text = $"Error solving dual: {ex.Message}";
        }
    }
    
    private void SA_Duality_Verify_Click(object? sender, RoutedEventArgs e)
    {
        try
        {
            if (_primalResult == null || _dualResult == null)
            {
                if (_saDualityStatus != null) _saDualityStatus.Text = "Please solve both primal and dual problems first.";
                return;
            }
            
            var context = GetSensitivityContext();
            if (context == null)
            {
                if (_saDualityStatus != null) _saDualityStatus.Text = "No solution available for duality verification.";
                return;
            }
            
            var dualityResult = context.VerifyDuality(_primalResult, _dualResult);
            
            var sb = new StringBuilder();
            sb.AppendLine("\n\nDUALITY VERIFICATION:");
            
            var (dualityCase, dualityFields) = FSharpInterop.ReadUnion(dualityResult);
            
            switch (dualityCase)
            {
                case "StrongDuality":
                    var primalObj = (double)dualityFields[0];
                    var dualObj = (double)dualityFields[1];
                    sb.AppendLine($"✓ STRONG DUALITY CONFIRMED");
                    sb.AppendLine($"Primal Objective: {primalObj:F6}");
                    sb.AppendLine($"Dual Objective: {dualObj:F6}");
                    sb.AppendLine($"Difference: {Math.Abs(primalObj - dualObj):E6}");
                    break;
                    
                case "WeakDuality":
                    var primalObjWeak = (double)dualityFields[0];
                    var dualObjWeak = (double)dualityFields[1];
                    sb.AppendLine($"⚠ WEAK DUALITY DETECTED");
                    sb.AppendLine($"Primal Objective: {primalObjWeak:F6}");
                    sb.AppendLine($"Dual Objective: {dualObjWeak:F6}");
                    sb.AppendLine($"Gap: {Math.Abs(primalObjWeak - dualObjWeak):F6}");
                    break;
                    
                case "NoDuality":
                    var reason = (string)dualityFields[0];
                    sb.AppendLine($"❌ NO DUALITY");
                    sb.AppendLine($"Reason: {reason}");
                    break;
            }
            
            if (_saDualityDisplay != null) _saDualityDisplay.Text += sb.ToString();
            if (_saDualityStatus != null) _saDualityStatus.Text = "Duality verification completed.";
        }
        catch (Exception ex)
        {
            if (_saDualityStatus != null) _saDualityStatus.Text = $"Error verifying duality: {ex.Message}";
        }
    }
    
    private string FormatObjective(LPFormulation formulation)
    {
        var objType = formulation.ObjectiveType == ObjectiveType.Max ? "max" : "min";
        var terms = new List<string>();
        for (int i = 0; i < formulation.VarNames.Length; i++)
        {
            var coeff = formulation.Objective[i];
            terms.Add($"{coeff:+0.###;-0.###}{formulation.VarNames[i]}");
        }
        return $"{objType} {string.Join(" ", terms)}";
    }
    
    private string[] FormatConstraints(LPFormulation formulation)
    {
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
        return constraints;
    }
    
    private string GetShadowPriceInterpretation(double shadowPrice)
    {
        return shadowPrice switch
        {
            > 0 => "Positive shadow price - increasing RHS improves objective value",
            < 0 => "Negative shadow price - increasing RHS worsens objective value",
            _ => "Zero shadow price - RHS changes have no effect on objective"
        };
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
