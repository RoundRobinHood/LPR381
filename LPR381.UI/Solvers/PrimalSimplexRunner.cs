using LPR381.Core;
using LPR381.UI.Models;
using LPR381.UI.Util;
using Microsoft.FSharp.Core;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LPR381.UI.Solvers
{
    public sealed class PrimalSimplexRunner : BaseSolver, ISolverRunner
    {
        private readonly List<IterationTableau> _iters = new();
        public string Key => "primal-simplex";
        public string Display => "Primal Simplex";
        public IReadOnlyList<IterationTableau> Iterations => _iters;

        public Task<SolveSummary> RunAsync(UserProblem input)
        {
            return Task.Run(() =>
            {
                _iters.Clear();

                LPFormulation model = BuildFormulation(input);

                // Your public F# ctor
                ITree<SimplexNode> root = new PrimalSimplex(model);

                var summary = new SolveSummary();

                var stack = new Stack<(ITree<SimplexNode> node, int idx)>();
                stack.Push((root, 0));

                while (stack.Count > 0)
                {
                    var (cur, idx) = stack.Pop();
                    var item = cur.Item;   // SimplexNode

                    // collect tableau snapshot
                    var t = item.Tableau;  // has ColumnNames, RowNames, Values (double[,])
                    _iters.Add(new IterationTableau
                    {
                        Title = $"Iter {idx}",
                        Columns = t.ColumnNames,
                        Rows = t.RowNames,
                        Values = t.Values
                    });

                    // ----- FIXED: C# pattern-match over F# option + DU -----
                    var provider = (ISimplexResultProvider)item;
                    var maybe = provider.SimplexResult;  // FSharpOption<SimplexResult>
                    if (FSharpDu.TrySome(maybe, out var res))
                    {
                        var (caseName, fields) = FSharpDu.ReadUnion(res);

                        switch (caseName)
                        {
                            case "Optimal":
                                // fields: [canonVars; formVars; z]
                                var formVars = FSharpDu.ToDict(fields[1]);
                                var z = (double)fields[2];

                                summary.IsOptimal = true;
                                summary.Objective = z;
                                summary.VariableValues = formVars;
                                summary.Message = "Optimal solution found.";
                                break;

                            case "Unbounded":
                                // fields: [varName]
                                var varName = (string)fields[0];
                                summary.IsOptimal = false;
                                summary.Message = $"Unbounded (variable {varName}).";
                                break;

                            case "Infeasible":
                                // fields: [stopRow] (int or string depending on your DU)
                                summary.IsOptimal = false;
                                summary.Message = $"Infeasible (stopping constraint row {fields[0]}).";
                                break;

                            default:
                                summary.IsOptimal = false;
                                summary.Message = $"Result: {caseName}";
                                break;
                        }
                    }
                    // ---------------------------------------------

                    // traverse children
                    var children = cur.Children; // ITree<SimplexNode>[]
                    for (int i = children.Length - 1; i >= 0; --i)
                        stack.Push((children[i], idx + 1));
                }

                if (string.IsNullOrEmpty(summary.Message))
                    summary.Message = "No result produced.";

                return summary;
            });
        }
    }
}
