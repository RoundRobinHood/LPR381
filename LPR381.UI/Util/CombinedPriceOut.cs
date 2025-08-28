using LPR381.UI.Models;
using Microsoft.FSharp.Reflection;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace LPR381.UI.Util
{
    public static class CombinedPriceOut
    {
        /// <summary>
        /// Build ONE combined table per pivot iteration:
        ///  - Row 0: reduced costs r̄ across variable columns
        ///  - Rows 1..m: product-form (entering column for Primal; leaving row for Dual) + b̂ and θ/|θ|
        /// </summary>
        public static void TryAdd(object revisedNode, string titleBase, string stateCase, object[] stateFields, List<IterationTableau> sink)
        {
            if (stateCase != "Pivot") return; // Only valid for Pivot

            // ---- fetch PriceOutInfo DU safely
            var poiProp = revisedNode.GetType().GetProperty("PriceOutInfo", BindingFlags.Public | BindingFlags.Instance);
            if (poiProp == null) return;

            object poi;
            try { poi = poiProp.GetValue(revisedNode)!; }
            catch { return; } // not a pivot node at runtime

            var uf = FSharpValue.GetUnionFields(poi, poi.GetType(), null);
            var caseName = uf.Item1.Name;
            var fields = uf.Item2;

            // Canon vars & basis names for nicer headers
            var (varNamesAll, basisNames) = TryGetNames(revisedNode);

            switch (caseName)
            {
                case "Primal":
                    // fields: [0]=nonBasicVars: (reducedCost:double * name:string)[],
                    //         [1]=x_B: double[], [2]=ratios: double[], [3]=enteringColumn: double[]
                    var nonBasic = ReadTupleArrayOfDoubleString(fields[0]);
                    var xB = ToDoubleArray(fields[1]);
                    var ratios = ToDoubleArray(fields[2]);
                    var enterCol = ToDoubleArray(fields[3]);

                    // Build columns: all nonbasic variable names + "b̂" + "θ"
                    var nbNames = nonBasic.Select(t => t.name).ToArray();
                    var cols = nbNames.Concat(new[] { "b̂", "θ" }).ToArray();

                    // Build matrix size: (1 + m) x (nb + 2)
                    int m = xB.Length;
                    int n = nbNames.Length + 2;
                    var mat = new double[1 + m, n];
                    FillNaN(ref mat);

                    // r̄ on row 0
                    for (int j = 0; j < nbNames.Length; j++) mat[0, j] = nonBasic[j].value;

                    // product-form: only the ENTERING variable column gets aⱼ̂
                    int? enterIndex = TryGetEnteringIndex(stateFields, revisedNode, nbNames, varNamesAll);
                    if (enterIndex.HasValue && enterCol.Length == m && enterIndex.Value < nbNames.Length)
                        for (int i = 0; i < m; i++) mat[1 + i, enterIndex.Value] = enterCol[i];

                    // b̂ (col nb) and θ (col nb+1)
                    for (int i = 0; i < m; i++) { mat[1 + i, nbNames.Length] = xB[i]; }
                    for (int i = 0; i < m; i++) { mat[1 + i, nbNames.Length + 1] = ratios[i]; }

                    // Row names: r̄, then basis names if we have them
                    var rows = new string[1 + m];
                    rows[0] = "r̄";
                    for (int i = 0; i < m; i++) rows[1 + i] = basisNames?.ElementAtOrDefault(i) ?? $"r{i + 1}";

                    sink.Add(new IterationTableau
                    {
                        Title = $"{titleBase} – Price-out (combined)",
                        Columns = cols,
                        Rows = rows,
                        Values = mat
                    });
                    break;

                case "Dual":
                    // fields: [0]=x_B: double[], [1]=leavingRow: double[], [2]=reducedCost: double[], [3]=absRatios: double[]
                    var xBDual = ToDoubleArray(fields[0]);
                    var lrow = ToDoubleArray(fields[1]);
                    var rCosts = ToDoubleArray(fields[2]);
                    var absRatio = ToDoubleArray(fields[3]);

                    // Columns: all variable names (if available/length match) + "b̂" + "|θ|"
                    var varCols = (varNamesAll != null && varNamesAll.Length == rCosts.Length)
                                  ? varNamesAll
                                  : Enumerable.Range(1, rCosts.Length).Select(i => $"x{i}").ToArray();
                    var colsD = varCols.Concat(new[] { "b̂", "|θ|" }).ToArray();

                    int mD = xBDual.Length;
                    int nD = varCols.Length + 2;
                    var matD = new double[1 + mD, nD];
                    FillNaN(ref matD);

                    // r̄ on row 0 (across all vars)
                    for (int j = 0; j < varCols.Length && j < rCosts.Length; j++) matD[0, j] = rCosts[j];

                    // product-form row: leavingRow spans across ALL var columns
                    for (int j = 0; j < varCols.Length && j < lrow.Length; j++) matD[1, j] = lrow[j];

                    // b̂ and |θ|
                    for (int i = 0; i < mD; i++) { matD[1 + i, varCols.Length] = xBDual[i]; }
                    for (int i = 0; i < mD; i++) { matD[1 + i, varCols.Length + 1] = absRatio.ElementAtOrDefault(i); }

                    var rowsD = new string[1 + mD];
                    rowsD[0] = "r̄";
                    for (int i = 0; i < mD; i++) rowsD[1 + i] = basisNames?.ElementAtOrDefault(i) ?? $"r{i + 1}";

                    sink.Add(new IterationTableau
                    {
                        Title = $"{titleBase} – Price-out (combined)",
                        Columns = colsD,
                        Rows = rowsD,
                        Values = matD
                    });
                    break;

                default:
                    // Unknown DU case — ignore
                    break;
            }
        }

        // ---------- helpers ----------

        private static void FillNaN(ref double[,] M)
        {
            for (int i = 0; i < M.GetLength(0); i++)
                for (int j = 0; j < M.GetLength(1); j++)
                    M[i, j] = double.NaN;
        }

        private static (string[]? allVars, string[]? basisNames) TryGetNames(object node)
        {
            string[]? allVars = null, basisNames = null;

            var canon = node.GetType().GetProperty("Canon", BindingFlags.Public | BindingFlags.Instance)?.GetValue(node);
            if (canon != null)
            {
                var vnProp = canon.GetType().GetProperty("VariableNames", BindingFlags.Public | BindingFlags.Instance);
                if (vnProp != null && vnProp.GetValue(canon) is IEnumerable<string> vs)
                    allVars = vs.ToArray();
            }

            var basisIdx = node.GetType().GetProperty("Basis", BindingFlags.Public | BindingFlags.Instance)?.GetValue(node) as int[];
            if (basisIdx != null && allVars != null)
                basisNames = basisIdx.Select(i => i >= 0 && i < allVars.Length ? allVars[i] : $"r{i}").ToArray();

            return (allVars, basisNames);
        }

        private static int? TryGetEnteringIndex(object[] stateFields, object node, string[] nbNames, string[]? allVars)
        {
            // Pivot(row:int, column:int, eta:...) — column is the entering variable index in Canon
            if (stateFields.Length >= 2 && stateFields[1] is int colIndex)
            {
                if (allVars != null && colIndex >= 0 && colIndex < allVars.Length)
                {
                    var name = allVars[colIndex];
                    int j = Array.IndexOf(nbNames, name);
                    if (j >= 0) return j;
                }
            }
            return null;
        }

        private static List<(double value, string name)> ReadTupleArrayOfDoubleString(object tuplesArray)
        {
            var list = new List<(double, string)>();
            if (tuplesArray is IEnumerable en)
            {
                foreach (var t in en)
                {
                    var tt = t.GetType();
                    var v = Convert.ToDouble(tt.GetProperty("Item1")!.GetValue(t)!);
                    var s = (string)tt.GetProperty("Item2")!.GetValue(t)!;
                    list.Add((v, s));
                }
            }
            return list;
        }

        private static double[] ToDoubleArray(object o)
        {
            if (o is double[] arr) return arr;
            if (o is IEnumerable<double> seq) return seq.ToArray();
            if (o is IEnumerable en)
            {
                var list = new List<double>();
                foreach (var x in en) list.Add(Convert.ToDouble(x));
                return list.ToArray();
            }
            return Array.Empty<double>();
        }
    }
}
