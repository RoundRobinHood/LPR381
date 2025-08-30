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
            new("revised-simplex", "Revised Simplex", () => new RevisedSimplexRunner()),
            new("cutting-plane",   "Cutting Plane",   () => new CuttingPlaneRunner()),
            new("knapsack",        "Knapsack",        () => new KnapsackRunner()),
            // Add future solvers here; they will appear in the UI automatically.
        };
    }
}
