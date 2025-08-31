using LPR381.Core;
using LPR381.UI.Core;
using LPR381.UI.Models;
using System.Collections.Generic;
using System.Linq;
using System;

namespace LPR381.UI.Solvers
{
    public sealed class RevisedSimplexRunner(bool isPrimal) : SolverRunner
    {
        public override string Key => isPrimal ? "revised-primal-simplex" : "revised-dual-simplex";
        public override string Display => "Revised Simplex";

        public SolveSummary Solve(string title_prefix, ITree<RevisedSimplexNode> root)
        {
            var canon = root.Item.Canon;
            var summary = new SolveSummary();
            var stack = new Stack<(ITree<RevisedSimplexNode> node, int idx)>();
            stack.Push((root, 0));

            while (stack.Count > 0)
            {
                var (cur, idx) = stack.Pop();
                var node = cur.Item;

                var (stateCase, stateFields) = FSharpInterop.ReadUnion(node.State);
                string title = $"{title_prefix}Iteration {idx}";
                if (stateCase == "Pivot") title += " (Pivot)";
                else if (stateCase == "ResultState") title += " (Final)";


                // Price Out Information
                if (stateCase == "Pivot")
                {
                    var leavingBasis = (int)stateFields[0];
                    var enteringVariable = (int)stateFields[1];
                    var enteringVariableName = canon.VariableNames[enteringVariable];
                    var leavingVariableName = canon.VariableNames[node.Basis[leavingBasis]];
                    var eta = (EtaMatrix)stateFields[2];

                    // eta * B⁻¹ (Product Form)
                    var binv = node.BInverse;
                    int m = binv.GetLength(0);
                    var rc = Enumerable.Range(1, m).Select(i => $"r{i}").ToArray();
                    var cc = Enumerable.Range(1, m).Select(i => $"c{i}").ToArray();
                    _iterations.Add(new IterationTableau
                    {
                        Title = $"{title} - E (Product Form)",
                        Columns = cc,
                        Rows = rc,
                        Values = eta.matrix.ToArray()
                    });
                    _iterations.Add(new IterationTableau 
                    { 
                        Title = $"{title} – B⁻¹ (Product Form)", 
                        Columns = cc, 
                        Rows = rc, 
                        Values = binv 
                    });

                    try
                    {
                        var priceOut = node.PriceOutInfo;
                        var (priceCase, priceFields) = FSharpInterop.ReadUnion(priceOut);
                        
                        if (priceCase == "Primal")
                        {
                            var nbvCosts = (Tuple<double, string>[])priceFields[0];
                            var rhs = (double[])priceFields[1];
                            var ratios = (double[])priceFields[2];
                            var enteringColumn = (double[])priceFields[3];

                            var columns = new List<string>();
                            for(int i = 0;i < nbvCosts.Length; i++) {
                                columns.Add(nbvCosts[i].Item2);
                            }
                            columns.Add("rhs");
                            columns.Add("ratios");

                            var rows = new List<string>();
                            rows.Add("z");
                            for(int i = 0;i < rhs.Length; i++) {
                                rows.Add($"c{i+1}");
                            }

                            var values = new double[rows.Count, columns.Count];

                            for(int i = 0; i < nbvCosts.Length; i++) {
                                values[0, i] = -nbvCosts[i].Item1;
                            }
                            values[0, nbvCosts.Length] = node.ObjectiveValue;

                            for(int i = 1; i < rows.Count; i++) {
                                for(int j = 0; j < nbvCosts.Length; j++) {
                                    if(columns[j] == enteringVariableName) {
                                        values[i, j] = enteringColumn[i-1];
                                    } else {
                                        values[i, j] = double.NaN;
                                    }
                                }
                            }

                            for(int i = 1; i < rows.Count; i++) {
                                values[i, columns.Count - 2] = rhs[i-1];
                                values[i, columns.Count - 1] = ratios[i-1];
                            }
                            
                            _iterations.Add(new IterationTableau
                            {
                                Title = $"{title} – Price Out Info (decision: pivot from {leavingVariableName} to {enteringVariableName})",
                                Columns = columns.ToArray(),
                                Rows = rows.ToArray(),
                                Values = values
                            });
                        } else {
                            var rhs = (double[])priceFields[0];
                            var leavingRow = (double[])priceFields[1];
                            var costs = (double[])priceFields[2];
                            var absRatios = (double[])priceFields[3];

                            var columns = new List<string>();
                            var rows = new[] { "z", $"c{leavingBasis}", "absRatio" };

                            var columnValues = new List<double[]>();
                            for(int i = 0; i < leavingRow.Length; i++) {
                                if(leavingRow[i] >= 0) continue;
                                
                                columnValues.Add(new double[]{
                                    -costs[i],
                                    leavingRow[i],
                                    absRatios[i]
                                });

                                columns.Add(canon.VariableNames[i]);
                            }
                            columnValues.Add(new double[] {
                                node.ObjectiveValue,
                                rhs[leavingBasis],
                                0.0
                            });
                            columns.Add("rhs");

                            var values = new double[rows.Length, columns.Count];
                            for(int i = 0;i < columnValues.Count; i++) {
                                for(int j = 0; j < rows.Length; j++) {
                                    values[j,i] = columnValues[i][j];
                                }
                            }

                            _iterations.Add(new IterationTableau
                            {
                                Title = $"{title} - Dual Price Out Info (decision: pivot from {leavingVariableName} to {enteringVariableName})",
                                Columns = columns.ToArray(),
                                Rows = rows.ToArray(),
                                Values = values
                            });
                        }
                    }
                    catch (Exception ex) {
                        Console.WriteLine("Error occurred while adding price out iterations: {0}", ex);
                    }
                }

                var provider = (ISimplexResultProvider)node;
                if (FSharpInterop.TrySome(provider.SimplexResult, out var res))
                {
                    var (caseName, fields) = FSharpInterop.ReadUnion(res);
                    switch (caseName)
                    {
                        case "Optimal":
                            summary.IsOptimal = true;
                            summary.Objective = (double)fields[2];
                            summary.VariableValues = FSharpInterop.ToDict(fields[1]);
                            summary.Message = "Optimal solution found.";
                            break;
                        case "Unbounded":
                            summary.Message = $"Unbounded (variable {fields[0]}).";
                            break;
                        case "Infeasible":
                            summary.Message = $"Infeasible (constraint {fields[0]}).";
                            break;
                    }
                }

                var children = cur.Children;
                for (int i = children.Length - 1; i >= 0; --i)
                    stack.Push((children[i], idx + 1));
            }

            return summary;
        }

        protected override SolveSummary Solve(LPFormulation model)
        {
            _iterations.Clear();
            AddCanonicalForm(model);
            
            var canon = model.ToLPCanonical();
            ITree<RevisedSimplexNode> root;
            if(isPrimal) {
                root = new RevisedPrimalSimplex(model);
            } else {
                root = new RevisedDualSimplex(model);
            }

            return Solve("", root);
        }
    }
}
