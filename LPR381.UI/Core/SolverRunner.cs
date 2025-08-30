using LPR381.Core;
using LPR381.UI.Models;
using LPR381.UI.Solvers;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using System.IO;
using LPR381.UI.Util;

namespace LPR381.UI.Core
{
    public abstract class SolverRunner : ISolverRunner
    {
        protected readonly List<IterationTableau> _iterations = new();
        
        public abstract string Key { get; }
        public abstract string Display { get; }
        public IReadOnlyList<IterationTableau> Iterations => _iterations;
        
        public Task<SolveSummary> RunAsync(UserProblem input) => 
            Task.FromResult(Solve(BuildFormulation(input)));
        
        protected abstract SolveSummary Solve(LPFormulation model);
        
        protected virtual LPFormulation BuildFormulation(UserProblem input)
        {
            var builder = new LPFormulationBuilder();
            var model = builder.Build(input);
            
            // Apply variable type restrictions
            var intRestrictions = new IntRestriction[model.VarNames.Length];
            var restriction = input.IntMode switch
            {
                "integer" => IntRestriction.Integer,
                "binary" => IntRestriction.Binary,
                _ => IntRestriction.Unrestricted
            };
            System.Array.Fill(intRestrictions, restriction);
            
            return new LPFormulation(
                model.ObjectiveType, model.VarNames, model.Objective,
                model.ConstraintCoefficients, model.ConstraintSigns, model.RHS,
                model.VarSignRestrictions, intRestrictions);
        }

        protected void AddCanonicalForm(LPFormulation model)
        {
            var canon = model.ToLPCanonical();
            var m = canon.ConstraintMatrix.RowCount;
            var n = canon.ConstraintMatrix.ColumnCount;
            
            // Objective row
            var objMatrix = new double[1, n + 1];
            for (int j = 0; j < n; j++) objMatrix[0, j] = canon.Objective[j];
            objMatrix[0, n] = 0; // RHS for objective
            
            _iterations.Add(new IterationTableau
            {
                Title = "Canonical Form - Objective",
                Columns = canon.VariableNames.Concat(new[] { "RHS" }).ToArray(),
                Rows = new[] { "z" },
                Values = objMatrix
            });
            
            // Constraint matrix
            var constraintMatrix = new double[m, n + 1];
            for (int i = 0; i < m; i++)
            {
                for (int j = 0; j < n; j++)
                    constraintMatrix[i, j] = canon.ConstraintMatrix[i, j];
                constraintMatrix[i, n] = canon.RHS[i];
            }
            
            _iterations.Add(new IterationTableau
            {
                Title = "Canonical Form - Constraints",
                Columns = canon.VariableNames.Concat(new[] { "RHS" }).ToArray(),
                Rows = Enumerable.Range(1, m).Select(i => $"c{i}").ToArray(),
                Values = constraintMatrix
            });
        }
        
        public void ExportToFile(string filePath)
        {
            var content = string.Join("\n\n", _iterations.Select(IterationFormat.Pretty));
            File.WriteAllText(filePath, content);
        }
    }
}