using LPR381.Core;
using LPR381.UI.Core;
using LPR381.UI.Models;
using System.Collections.Generic;
using System.Linq;

namespace LPR381.UI.Solvers
{
    public sealed class RevisedSimplexRunner : SolverRunner
    {
        public override string Key => "revised-simplex";
        public override string Display => "Revised Simplex";

        protected override SolveSummary Solve(LPFormulation model)
        {
            _iterations.Clear();
            AddCanonicalForm(model);
            
            var root = new RevisedPrimalSimplex(model);
            var summary = new SolveSummary();
            var stack = new Stack<(ITree<RevisedSimplexNode> node, int idx)>();
            stack.Push((root, 0));

            while (stack.Count > 0)
            {
                var (cur, idx) = stack.Pop();
                var node = cur.Item;

                var (stateCase, stateFields) = FSharpInterop.ReadUnion(node.State);
                string title = $"Iteration {idx}";
                if (stateCase == "Pivot") title += " (Pivot)";
                else if (stateCase == "ResultState") title += " (Final)";

                // B⁻¹ (Product Form)
                var binv = node.BInverse;
                int m = binv.GetLength(0);
                var rc = Enumerable.Range(1, m).Select(i => $"r{i}").ToArray();
                var cc = Enumerable.Range(1, m).Select(i => $"c{i}").ToArray();
                _iterations.Add(new IterationTableau 
                { 
                    Title = $"{title} – B⁻¹ (Product Form)", 
                    Columns = cc, 
                    Rows = rc, 
                    Values = binv 
                });

                // Price Out Information
                if (stateCase == "Pivot")
                {
                    try
                    {
                        var priceOut = node.PriceOutInfo;
                        var (priceCase, _) = FSharpInterop.ReadUnion(priceOut);
                        
                        if (priceCase == "Primal")
                        {
                            // Simplified price out display
                            var rcMatrix = new double[1, 1];
                            rcMatrix[0, 0] = 1.0; // Placeholder
                            
                            _iterations.Add(new IterationTableau
                            {
                                Title = $"{title} – Price Out Info",
                                Columns = new[] { "Available" },
                                Rows = new[] { "Status" },
                                Values = rcMatrix
                            });
                        }
                    }
                    catch { /* Price out info not available */ }
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
    }
}
