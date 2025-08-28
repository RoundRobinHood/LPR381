using LPR381.Core;
using LPR381.UI.Models;
using LPR381.UI.Util;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LPR381.UI.Solvers
{
    public sealed class RevisedSimplexRunner : BaseSolver, ISolverRunner
    {
        private readonly List<IterationTableau> _iters = new();
        public string Key => "revised-simplex";
        public string Display => "Revised Simplex";
        public IReadOnlyList<IterationTableau> Iterations => _iters;

        public Task<SolveSummary> RunAsync(UserProblem input)
        {
            return Task.Run(() =>
            {
                _iters.Clear();

                var model = BuildFormulation(input);
                ITree<RevisedSimplexNode> root = new RevisedPrimalSimplex(model);

                var summary = new SolveSummary();

                var stack = new Stack<(ITree<RevisedSimplexNode> node, int idx)>();
                stack.Push((root, 0));

                while (stack.Count > 0)
                {
                    var (cur, idx) = stack.Pop();
                    var node = cur.Item;

                    // ---- Title by state
                    string title = $"Iter {idx}";
                    var (stateCase, stateFields) = FSharpDu.ReadUnion(node.State);
                    if (stateCase == "Pivot")
                    {
                        string extra = "";
                        if (stateFields.Length >= 1 && stateFields[0] is int enter) extra = $"enter={enter}";
                        if (stateFields.Length >= 2 && stateFields[1] is int leave) extra += (extra == "" ? "" : " / ") + $"leave={leave}";
                        title += extra == "" ? " (pivot)" : $" (pivot {extra})";
                    }
                    else if (stateCase == "Result" || stateCase == "ResultState")
                    {
                        title += " (result)";
                    }

                    // ---- Always show current B^{-1}
                    var binv = node.BInverse;
                    int m = binv.GetLength(0);
                    var rc = Enumerable.Range(1, m).Select(i => $"r{i}").ToArray();
                    var cc = Enumerable.Range(1, m).Select(i => $"c{i}").ToArray();
                    _iters.Add(new IterationTableau { Title = $"{title} – B⁻¹", Columns = cc, Rows = rc, Values = binv });

                    // ---- If state is Pivot, show Eta from ProductForm (safe access)
                    if (stateCase == "Pivot")
                    {
                        try
                        {
                            var eta = node.ProductForm.EtaMatrix;
                            int rm = eta.GetLength(0), cn = eta.GetLength(1);
                            var rnames = Enumerable.Range(1, rm).Select(i => $"r{i}").ToArray();
                            var cnames = Enumerable.Range(1, cn).Select(i => $"c{i}").ToArray();
                            _iters.Add(new IterationTableau { Title = $"{title} – Eta", Columns = cnames, Rows = rnames, Values = eta });
                        }
                        catch { /* ignore if not available */ }
                    }

                    // ---- Price-out tables (reduced costs, b-hat, ratios) if available
                    var isPivot = stateCase == "Pivot";
                    CombinedPriceOut.TryAdd(node, title, stateCase, stateFields, _iters);

                    // ---- Result (F# option + DU)
                    var provider = (ISimplexResultProvider)node;
                    var maybe = provider.SimplexResult;
                    if (FSharpDu.TrySome(maybe, out var res))
                    {
                        var (caseName, fields) = FSharpDu.ReadUnion(res);
                        switch (caseName)
                        {
                            case "Optimal":
                                var formVars = FSharpDu.ToDict(fields[1]);
                                var z = (double)fields[2];
                                summary.IsOptimal = true;
                                summary.Objective = z;
                                summary.VariableValues = formVars;
                                summary.Message = "Optimal solution found.";
                                break;

                            case "Unbounded":
                                summary.IsOptimal = false;
                                summary.Message = $"Unbounded (variable {fields[0]}).";
                                break;

                            case "Infeasible":
                                summary.IsOptimal = false;
                                summary.Message = $"Infeasible (stopping constraint row {fields[0]}).";
                                break;
                        }
                    }

                    // ---- Traverse
                    var children = cur.Children;
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
