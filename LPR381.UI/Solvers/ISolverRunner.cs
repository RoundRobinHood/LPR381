using LPR381.UI.Models;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace LPR381.UI.Solvers
{
    public sealed class SolveSummary
    {
        public bool IsOptimal { get; set; }
        public double Objective { get; set; }
        public Dictionary<string, double> VariableValues { get; set; } = new();
        public string Message { get; set; } = "";
    }

    public interface ISolverRunner
    {
        string Key { get; }
        string Display { get; }
        IReadOnlyList<IterationTableau> Iterations { get; }
        Task<SolveSummary> RunAsync(UserProblem input);
    }
}
