using LPR381.Core;
using LPR381.UI.Core;
using LPR381.UI.Models;
using System.Collections.Generic;

namespace LPR381.UI.Solvers
{
    public sealed class PrimalSimplexRunner : SolverRunner
    {
        public override string Key => "primal-simplex";
        public override string Display => "Primal Simplex";

        protected override SolveSummary Solve(LPFormulation model)
        {
            _iterations.Clear();
            AddCanonicalForm(model);
            
            var root = new PrimalSimplex(model);
            var summary = new SolveSummary();
            var stack = new Stack<(ITree<SimplexNode> node, int idx)>();
            stack.Push((root, 0));

            while (stack.Count > 0)
            {
                var (cur, idx) = stack.Pop();
                var item = cur.Item;

                var t = item.Tableau;
                var (stateCase, _) = FSharpInterop.ReadUnion(item.State);
                var title = stateCase == "Pivot" ? $"Tableau {idx} (Pivot)" : $"Tableau {idx} (Final)";
                
                // Fix row names: first row should be "z", rest should be "c1", "c2", etc.
                var fixedRowNames = new string[t.Values.GetLength(0)];
                fixedRowNames[0] = "z";
                for (int i = 1; i < fixedRowNames.Length; i++)
                    fixedRowNames[i] = $"c{i}";
                
                _iterations.Add(new IterationTableau
                {
                    Title = title,
                    Columns = t.ColumnNames,
                    Rows = fixedRowNames,
                    Values = t.Values
                });

                var provider = (ISimplexResultProvider)item;
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
                        default:
                            summary.Message = $"Result: {caseName}";
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
