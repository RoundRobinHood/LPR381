using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LPR381.UI.Models
{
    public sealed class IterationTableau
    {
        public string Title { get; set; } = "";
        public IReadOnlyList<string> Columns { get; set; } = new List<string>();
        public IReadOnlyList<string> Rows { get; set; } = new List<string>();
        public double[,] Values { get; set; } = new double[0, 0];
    }
}
