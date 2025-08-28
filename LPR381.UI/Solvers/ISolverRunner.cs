using LPR381.Core;
using LPR381.UI.Models;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace LPR381.UI.Solvers
{
    public sealed class SolveSummary
    {
        public bool IsOptimal { get; set; }
        public double Objective { get; set; }
        public IDictionary<string, double> VariableValues { get; set; } = new Dictionary<string, double>();
        public string Message { get; set; } = "";
    }

    public interface ISolverRunner
    {
        string Key { get; }
        string Display { get; }

        /// Populated after RunAsync: used to fill the Iterations tab.
        IReadOnlyList<IterationTableau> Iterations { get; }

        /// Takes raw UI lines, builds an LPFormulation using existing F# parsers,
        /// runs the solver, fills Iterations, and returns a summary.
        Task<SolveSummary> RunAsync(UserProblem input);
    }
}
