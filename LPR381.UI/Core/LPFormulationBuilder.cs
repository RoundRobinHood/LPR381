using LPR381.Core;
using LPR381.UI.Models;
using System;
using System.Globalization;
using System.Linq;
using System.Text.RegularExpressions;

namespace LPR381.UI.Core
{
    public class LPFormulationBuilder
    {
        public LPFormulation Build(UserProblem input)
        {
            if (input?.ObjectiveLine == null) 
                throw new ArgumentException("Objective is required");

            var (objType, objExpr) = ParseObjective(input.ObjectiveLine);
            
            LPObjective obj = default!;
            string objErr = "";
            if (!LPObjective.TryParse(objType, objExpr, ref obj, ref objErr))
                throw new FormatException($"Invalid objective: {objErr}");

            var constraints = ParseConstraints(input.Constraints ?? Array.Empty<string>());
            return new LPFormulation(obj, constraints);
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
            var match = Regex.Match(line, @"^(.*?)(<=|>=|=)\s*([+-]?\d+(?:\.\d+)?)\s*$", RegexOptions.None, TimeSpan.FromSeconds(1));
            if (!match.Success) 
                throw new FormatException("Constraint must be 'left <= right', 'left >= right', or 'left = right'");

            if (!double.TryParse(match.Groups[3].Value, NumberStyles.Float, CultureInfo.InvariantCulture, out var rhs))
                throw new FormatException("Right-hand side must be a number");

            return (match.Groups[1].Value.Trim(), match.Groups[2].Value.Trim(), rhs);
        }
    }
}