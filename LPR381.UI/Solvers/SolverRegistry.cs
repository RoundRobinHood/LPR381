using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LPR381.UI.Solvers
{
    public static class SolverRegistry
    {
        public sealed record Entry(string Key, string Display, Func<ISolverRunner> Factory);

        public static IReadOnlyList<Entry> Available { get; } = new List<Entry>
        {
            new("primal-simplex",  "Primal Simplex",  () => new PrimalSimplexRunner()),
            new("revised-primal-simplex", "Revised Primal Simplex", () => new RevisedSimplexRunner(true)),
            new("revised-dual-simplex", "Revised Dual Simplex", () => new RevisedSimplexRunner(false)),
            new("branch-and-bound", "Branch & Bound",  () => new BranchAndBoundRunner()),
            new("cutting-plane",   "Cutting Plane",   () => new CuttingPlaneRunner()),
            new("knapsack",        "Knapsack",        () => new KnapsackRunner())
        };
    }
}
