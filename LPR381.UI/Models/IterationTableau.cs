using System;

namespace LPR381.UI.Models
{
    public sealed class IterationTableau
    {
        public string Title { get; set; } = "";
        public string[] Columns { get; set; } = Array.Empty<string>();
        public string[] Rows { get; set; } = Array.Empty<string>();
        public double[,] Values { get; set; } = new double[0, 0];
    }
}
