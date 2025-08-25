using Avalonia;
using Avalonia.Controls;
using Avalonia.Interactivity;
using Avalonia.Layout;
using LPR381.Core;

namespace LPR381.UI;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();
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

}
