using LPR381.Core;
using LPR381.UI.Models;
using LPR381.UI.Solvers;
using System;
using System.Collections.Generic;
using System.Globalization;
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
        
        public async Task<SolveSummary> RunAsync(UserProblem input)
        {
            return await Task.Run(() => Solve(BuildFormulation(input)));
        }
        
        protected abstract SolveSummary Solve(LPFormulation model);
        
        public virtual LPFormulation BuildFormulation(UserProblem input)
        {
            if (input?.ObjectiveLine == null) 
                throw new ArgumentException("Objective is required");

            var (objType, objExpr) = ParseObjective(input.ObjectiveLine);
            
            LPObjective obj = default!;
            string objErr = "";
            if (!LPObjective.TryParse(objType, objExpr, ref obj, ref objErr))
                throw new FormatException($"Invalid objective: {objErr}");

            var constraints = ParseConstraints(input.Constraints ?? System.Array.Empty<string>());
            var baseModel = new LPFormulation(obj, constraints);
            
            // Apply variable sign restrictions
            var signRestrictions = new SignRestriction[baseModel.VarNames.Length];
            for (int i = 0; i < baseModel.VarNames.Length; i++)
            {
                var varName = baseModel.VarNames[i];
                signRestrictions[i] = input.VariableSignRestrictions?.ContainsKey(varName) == true
                    ? input.VariableSignRestrictions[varName]
                    : SignRestriction.Positive; // Default to positive
            }
            
            // Apply variable type restrictions
            var intRestrictions = new IntRestriction[baseModel.VarNames.Length];
            for (int i = 0; i < baseModel.VarNames.Length; i++)
            {
                var varName = baseModel.VarNames[i];
                if (input.VariableIntRestrictions?.ContainsKey(varName) == true)
                {
                    intRestrictions[i] = input.VariableIntRestrictions[varName];
                }
                else
                {
                    // Fallback to global IntMode if individual restriction not specified
                    intRestrictions[i] = input.IntMode switch
                    {
                        "integer" => IntRestriction.Integer,
                        "binary" => IntRestriction.Binary,
                        _ => IntRestriction.Unrestricted
                    };
                }
            }
            
            return new LPFormulation(
                baseModel.ObjectiveType, baseModel.VarNames, baseModel.Objective,
                baseModel.ConstraintCoefficients, baseModel.ConstraintSigns, baseModel.RHS,
                signRestrictions, intRestrictions);
        }

        private static (ObjectiveType type, string expr) ParseObjective(string line)
        {
            var s = line.TrimStart();
            var type = s.StartsWith("min", StringComparison.OrdinalIgnoreCase) ? ObjectiveType.Min : ObjectiveType.Max;
            
            if (s.StartsWith("max", StringComparison.OrdinalIgnoreCase) || s.StartsWith("min", StringComparison.OrdinalIgnoreCase))
                s = s.Substring(3).TrimStart();
            
            var eq = s.IndexOf('=');
            if (eq >= 0) s = s.Substring(eq + 1).Trim();
            
            return (type, s);
        }

        private static LPConstraint[] ParseConstraints(string[] lines)
        {
            var validLines = lines.Where(s => !string.IsNullOrWhiteSpace(s)).ToArray();
            if (validLines.Length == 0) 
                throw new ArgumentException("At least one constraint is required");

            var constraints = new LPConstraint[validLines.Length];
            for (int i = 0; i < validLines.Length; i++)
            {
                var (left, sign, rhs) = SplitConstraint(validLines[i]);
                string err = "";
                if (!LPConstraint.TryParse(sign, rhs, left, ref constraints[i], ref err))
                    throw new FormatException($"Invalid constraint '{validLines[i]}': {err}");
            }
            return constraints;
        }

        private static (string left, string sign, double rhs) SplitConstraint(string line)
        {
            var parts = line.Split(new[] { "<=", ">=", "=" }, StringSplitOptions.None);
            if (parts.Length != 2) 
                throw new FormatException("Constraint must be 'left <= right', 'left >= right', or 'left = right'");

            var left = parts[0].Trim();
            var right = parts[1].Trim();
            var sign = line.Contains("<=") ? "<=" : line.Contains(">=") ? ">=" : "=";

            if (!double.TryParse(right, NumberStyles.Float, CultureInfo.InvariantCulture, out var rhs))
                throw new FormatException("Right-hand side must be a number");

            return (left, sign, rhs);
        }

        protected void AddCanonicalForm(LPFormulation model)
        {
            var canon = model.ToLPCanonical();
            var m = canon.ConstraintMatrix.RowCount;
            var n = canon.ConstraintMatrix.ColumnCount;
            
            // Combined matrix: objective row + constraint rows
            var combinedMatrix = new double[m + 1, n + 1];
            
            // Objective row (first row)
            for (int j = 0; j < n; j++) combinedMatrix[0, j] = canon.Objective[j];
            combinedMatrix[0, n] = 0;
            
            // Constraint rows
            for (int i = 0; i < m; i++)
            {
                for (int j = 0; j < n; j++)
                    combinedMatrix[i + 1, j] = canon.ConstraintMatrix[i, j];
                combinedMatrix[i + 1, n] = canon.RHS[i];
            }
            
            // Column names
            var columnNames = new string[n + 1];
            Array.Copy(canon.VariableNames, columnNames, n);
            columnNames[n] = "RHS";
            
            // Row names: z + constraints
            var rowNames = new string[m + 1];
            rowNames[0] = "z";
            for (int i = 0; i < m; i++) rowNames[i + 1] = $"c{i + 1}";
            
            _iterations.Add(new IterationTableau
            {
                Title = "Canonical Form",
                Columns = columnNames,
                Rows = rowNames,
                Values = combinedMatrix
            });
        }
        
        public void ExportToFile(string filePath)
        {
            var content = string.Join("\n\n", _iterations.Select(IterationFormat.Pretty));
            File.WriteAllText(filePath, content);
        }
    }
}