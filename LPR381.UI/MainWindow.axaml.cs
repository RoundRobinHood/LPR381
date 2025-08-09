using Avalonia.Controls;
using LPR381.Core;

namespace LPR381.UI;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();

        int result = Logic.add(3, 4);

        this.Title = $"3 + 4 = {result}";
    }
}
