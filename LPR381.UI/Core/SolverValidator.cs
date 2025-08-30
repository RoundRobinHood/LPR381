using LPR381.Core;
using System.Linq;

namespace LPR381.UI.Core
{
    public static class SolverValidator
    {
        public static string ValidateSolver(string solverKey, LPFormulation model)
        {
            return solverKey switch
            {
                "knapsack" => ValidateKnapsack(model),
                "branch-and-bound" => ValidateIntegerSolver(model, "Branch & Bound"),
                "cutting-plane" => ValidateIntegerSolver(model, "Cutting Plane"),
                _ => "" // Primal and Revised Simplex work with any LP
            };
        }

        private static string ValidateKnapsack(LPFormulation model)
        {
            if (model.ConstraintCoefficients.GetLength(0) != 1)
                return "Knapsack requires exactly one constraint";
            
            if (model.ConstraintSigns[0] != ConstraintSign.LessOrEqual)
                return "Knapsack requires ≤ constraint";
            
            if (model.Objective.Any(c => c <= 0))
                return "Knapsack requires positive objective coefficients";
            
            if (model.ObjectiveType != ObjectiveType.Max)
                return "Knapsack requires maximization objective";
            
            return "";
        }

        private static string ValidateIntegerSolver(LPFormulation model, string solverName)
        {
            if (model.VarIntRestrictions.All(r => r == IntRestriction.Unrestricted))
                return $"{solverName} requires at least one integer variable";
            
            return "";
        }
    }
}