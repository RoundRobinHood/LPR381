using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LPR381.UI.Util
{
    public static class JaggedToRect
    {
        // Helper if you ever need to convert double[][] -> double[,]
        public static double[,] To2D(this double[][] jagged)
        {
            if (jagged.Length == 0) return new double[0, 0];
            int m = jagged.Length;
            int n = jagged[0].Length;
            var r = new double[m, n];
            for (int i = 0; i < m; i++)
            {
                if (jagged[i].Length != n) throw new ArgumentException("Jagged rows have inconsistent length.");
                for (int j = 0; j < n; j++) r[i, j] = jagged[i][j];
            }
            return r;
        }
    }
}
