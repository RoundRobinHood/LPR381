using LPR381.Core;
using LPR381.UI.Models;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace LPR381.UI.Solvers
{
    /// Shared bits: build LPFormulation from raw lines using your F# parsing APIs.
    public abstract class BaseSolver
    {
        protected LPFormulation BuildFormulation(UserProblem input)
        {
            if (input == null) throw new ArgumentNullException(nameof(input));

            // ---------- OBJECTIVE ----------
            var raw = (input.ObjectiveLine ?? "").Trim();
            if (raw.Length == 0)
                throw new Exception("Objective Function is empty. Please Enter an Objective Function");

            var (objType, objExpr) = ExtractObjectiveTypeAndExpr(raw);

            LPObjective obj = default!;
            string objErr = "";
            // F# signature: bool LPObjective.TryParse(ObjectiveType, string, ref LPObjective, ref string)
            if (!LPObjective.TryParse(objType, objExpr, ref obj, ref objErr))
                throw new Exception($"Invalid objective: {objErr}");

            // ---------- CONSTRAINTS ----------
            var lines = (input.Constraints ?? Array.Empty<string>())
                        .Where(s => !string.IsNullOrWhiteSpace(s))
                        .Select(s => s.Trim())
                        .ToArray();
            if (lines.Length == 0) throw new Exception("Please enter at least one constraint.");

            var cons = new LPConstraint[lines.Length];
            for (int i = 0; i < lines.Length; i++)
            {
                var (left, sign, rhs) = SplitConstraint(lines[i]);
                string cErr = "";
                LPConstraint c = default!;
                // F# signature (string overload): bool TryParse(string sign, double rhs, string left, ref LPConstraint, ref string)
                if (!LPConstraint.TryParse(sign, rhs, left, ref c, ref cErr))
                    throw new Exception($"Invalid constraint: \"{lines[i]}\" ({cErr})");
                cons[i] = c;
            }

            // Use the convenience F# ctor that builds the matrix and var set for you
            return new LPFormulation(obj, cons);
        }

        private static (ObjectiveType objType, string expr) ExtractObjectiveTypeAndExpr(string raw)
        {
            var s = raw.TrimStart();
            ObjectiveType t;
            if (s.StartsWith("max", StringComparison.OrdinalIgnoreCase))
            {
                t = ObjectiveType.Max; s = s.Substring(3).TrimStart();
            }
            else if (s.StartsWith("min", StringComparison.OrdinalIgnoreCase))
            {
                t = ObjectiveType.Min; s = s.Substring(3).TrimStart();
            }
            else
            {
                // Default to Max if not specified
                t = ObjectiveType.Max;
            }

            // Accept "z = ..." or ":="; keep only the RHS expression for TryParse
            var eq = s.IndexOf('=');
            if (eq >= 0) s = s.Substring(eq + 1).Trim();

            return (t, s);
        }

        private static (string left, string sign, double rhs) SplitConstraint(string line)
        {
            // Match "left <= right" / "left >= right" / "left = right"
            var m = Regex.Match(line, @"^(.*?)(<=|>=|=)(.*)$");
            if (!m.Success) throw new Exception("Constraint must contain <=, >= or =");

            var left = m.Groups[1].Value.Trim();
            var sign = m.Groups[2].Value.Trim();
            var right = m.Groups[3].Value.Trim();

            if (!double.TryParse(right, NumberStyles.Float, CultureInfo.InvariantCulture, out var rhs))
                throw new Exception("Right-hand side must be a number.");

            return (left, sign, rhs);
        }
    }
}
