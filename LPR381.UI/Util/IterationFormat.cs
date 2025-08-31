using LPR381.UI.Models;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LPR381.UI.Util
{
    public static class IterationFormat
    {
        public static string Pretty(IterationTableau it)
        {
            var cols = it.Columns?.ToArray() ?? Array.Empty<string>();
            var rows = it.Rows?.ToArray() ?? Array.Empty<string>();
            var v = it.Values ?? new double[0, 0];

            int m = v.GetLength(0);
            int n = v.GetLength(1);

            // compute column widths
            int[] colWidths = new int[Math.Max(n + 1, 1)];
            colWidths[0] = Math.Max(4, rows.DefaultIfEmpty("").Max(s => (s ?? "").Length));
            for (int j = 0; j < n; j++)
            {
                int w = cols.Length > j ? (cols[j] ?? "").Length : 0;
                for (int i = 0; i < m; i++)
                {
                    string s = double.IsNaN(v[i, j]) ? "" : v[i, j].ToString("0.####");
                    if (s.Length > w) w = s.Length;
                }
                colWidths[j + 1] = Math.Max(6, w);
            }

            string pad(string s, int width) => (s ?? "").PadLeft(width);
            var line = new string('-', colWidths.Sum() + (colWidths.Length + 1) * 3);

            var head = "   " + pad("", colWidths[0]) + "   " +
                       string.Join("   ", Enumerable.Range(0, n).Select(j => pad(cols.Length > j ? cols[j] : "", colWidths[j + 1])));

            var body = string.Join(Environment.NewLine,
                Enumerable.Range(0, m).Select(i =>
                {
                    var cells = string.Join("   ", Enumerable.Range(0, n).Select(j =>
                    {
                        var s = double.IsNaN(v[i, j]) ? "" : v[i, j].ToString("0.###");
                        return pad(s, colWidths[j + 1]);
                    }));
                    return "   " + pad(rows.Length > i ? rows[i] : "", colWidths[0]) + "   " + cells;
                }));

            return $"{it.Title}\n{line}\n{head}\n{line}\n{body}\n{line}";
        }
    }
}
