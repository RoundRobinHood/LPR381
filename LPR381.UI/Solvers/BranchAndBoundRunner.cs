using LPR381.Core;
using LPR381.UI.Models;
using LPR381.UI.Util;
using Microsoft.FSharp.Reflection;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace LPR381.UI.Solvers
{
    public sealed class BranchAndBoundRunner : ISolverRunner
    {
        private readonly List<IterationTableau> _iters = new();
        public string Key => "branch-and-bound";
        public string Display => "Branch & Bound";
        public IReadOnlyList<IterationTableau> Iterations => _iters;

        public Task<SolveSummary> RunAsync(UserProblem input)
        {
            return Task.Run(() =>
            {
                _iters.Clear();

                // Build a formulation that honors integrality (defaults to all-integer)
                var (model, varNames, isIntVar) = BuildIntegerFormulation(input);

                // Read objective type so we can keep the BEST integer solution
                var objType = model.ObjectiveType;

                // Strongly-typed BnB (from your BranchAndBound.fs)
                ITree<RevisedBNBNode> root = new RevisedBranchAndBound(model);

                var summary = new SolveSummary();
                double bestZ = (objType == ObjectiveType.Max) ? double.NegativeInfinity : double.PositiveInfinity;

                bool Better(double zNew, double zOld) =>
                    objType == ObjectiveType.Max ? zNew > zOld + 1e-9 : zNew < zOld - 1e-9;

                var stack = new Stack<(ITree<RevisedBNBNode> node, int depth)>();
                stack.Push((root, 0));

                while (stack.Count > 0)
                {
                    var (cur, depth) = stack.Pop();
                    var item = cur.Item;               // RevisedBNBNode

                    // Decode state DU for titles (Branch(variableIndex, variableName) | ResultState(SimplexResult))
                    var uf = FSharpValue.GetUnionFields(item.State, item.State.GetType(), null);
                    var stateName = uf.Item1.Name;
                    var stateFields = uf.Item2;

                    // Pull the subproblem solution (LP relaxation at this node)
                    var (hasSol, solVars, z) = TryGetSolution(item);

                    // If this is a branch state, display the chosen variable’s value and floor/ceil
                    if (stateName == "Branch" && stateFields.Length >= 2 && hasSol)
                    {
                        var varIndex = (int)stateFields[0];
                        var varName = stateFields[1]?.ToString() ?? $"x{varIndex + 1}";

                        // Try the named variable’s value; otherwise any fractional integer variable
                        double value = solVars.TryGetValue(varName, out var v)
                            ? v
                            : solVars.Where(kv => IsIntName(kv.Key, varNames, isIntVar))
                                     .Select(kv => kv.Value)
                                     .FirstOrDefault(double.NaN);

                        var t = new double[1, 3];
                        t[0, 0] = value;
                        t[0, 1] = double.IsNaN(value) ? double.NaN : Math.Floor(value);
                        t[0, 2] = double.IsNaN(value) ? double.NaN : Math.Ceiling(value);

                        _iters.Add(new IterationTableau
                        {
                            Title = $"Node {depth} – branch on {varName}",
                            Columns = new[] { "value", "floor", "ceil" },
                            Rows = new[] { varName },
                            Values = t
                        });
                    }

                    // If this node produces an optimal subproblem solution, show the vector
                    if (hasSol && !double.IsNaN(z))
                    {
                        var rows = solVars.Keys.ToArray();
                        var vals = new double[rows.Length, 1];
                        for (int i = 0; i < rows.Length; i++) vals[i, 0] = solVars[rows[i]];

                        _iters.Add(new IterationTableau
                        {
                            Title = $"Node {depth} – {stateName}",
                            Columns = new[] { "value" },
                            Rows = rows,
                            Values = vals
                        });

                        // Keep the BEST integral solution seen so far (not just the first)
                        if (IsIntegral(solVars, varNames, isIntVar) && Better(z, bestZ))
                        {
                            bestZ = z;
                            summary.IsOptimal = true;
                            summary.Objective = z;
                            summary.VariableValues = new Dictionary<string, double>(solVars);
                            summary.Message = "Optimal integer solution found.";
                        }
                    }

                    // Traverse children (reverse for natural order)
                    var children = cur.Children;
                    for (int i = children.Length - 1; i >= 0; --i)
                        stack.Push((children[i], depth + 1));
                }

                if (string.IsNullOrEmpty(summary.Message))
                    summary.Message = summary.IsOptimal ? "Optimal integer solution found." : "Branch & Bound finished.";

                return summary;
            });
        }

        // ---------------- formulation & parsing aligned to Formulation.fs ----------------

        private static (LPFormulation model, string[] varNames, bool[] isIntVar) BuildIntegerFormulation(UserProblem input)
        {
            if (input == null) throw new ArgumentNullException(nameof(input));

            // Objective
            var raw = (input.ObjectiveLine ?? "").Trim();
            if (raw.Length == 0) throw new Exception("Objective is empty. Example: 'max z = 3x1 + 2x2'");

            var (objType, objExpr) = ExtractObjectiveTypeAndExpr(raw);

            LPObjective obj = default!;
            string objErr = "";
            if (!LPObjective.TryParse(objType, objExpr, ref obj, ref objErr))
                throw new Exception($"Invalid objective: {objErr}");

            // Constraints (LPConstraint.TryParse(string sign, double rhs, string left, ref res, ref err))
            var lines = (input.Constraints ?? Array.Empty<string>())
                        .Where(s => !string.IsNullOrWhiteSpace(s))
                        .Select(s => s.Trim())
                        .ToArray();
            if (lines.Length == 0) throw new Exception("Please enter at least one constraint.");

            var cons = new LPConstraint[lines.Length];
            for (int i = 0; i < lines.Length; i++)
            {
                var (left, sign, rhsScalar) = SplitConstraint(lines[i]);
                string cErr = "";
                LPConstraint c = default!;
                if (!LPConstraint.TryParse(sign, rhsScalar, left, ref c, ref cErr))
                    throw new Exception($"Invalid constraint: \"{lines[i]}\" ({cErr})");
                cons[i] = c;
            }

            // Default sign restrictions = Positive (non-negative)
            var signR = Enumerable.Repeat(SignRestriction.Positive, obj.LinearSum.Length).ToArray();

            // Integer restrictions from UI mode
            var (intR, isInt) = BuildIntRestrictions(obj, cons, input);

            // Use your convenience ctor
            var model = new LPFormulation(obj, cons, signR, intR);

            // Read back final names from the model
            return (model, model.VarNames, isInt);
        }

        private static (IntRestriction[] intR, bool[] isInt) BuildIntRestrictions(LPObjective obj, LPConstraint[] cons, UserProblem input)
        {
            // Build the name list in the same style as the F# ctor
            var namesFromObj = obj.LinearSum.Select(t => t.Item2).ToArray();
            var namesFromCons = cons.SelectMany(c => c.LeftSide.Select(t => t.Item2)).ToArray();
            var varNames = namesFromObj.Concat(namesFromCons).Distinct(StringComparer.Ordinal).ToArray();
            int n = varNames.Length;

            // Mode from UI (fix: use the real value, not a typo)
            //var mode = (input.IntMode ?? "integer").Trim().ToLowerInvariant();
            var mode = "integer";

            var intR = new IntRestriction[n];
            var isInt = new bool[n];

            switch (mode)
            {
                case "continuous":
                    for (int i = 0; i < n; i++) { intR[i] = IntRestriction.Unrestricted; isInt[i] = false; }
                    break;

                case "binary":
                    for (int i = 0; i < n; i++) { intR[i] = IntRestriction.Binary; isInt[i] = true; }
                    break;

                case "custom":
                    {
                        for (int i = 0; i < n; i++) { intR[i] = IntRestriction.Unrestricted; isInt[i] = false; }

                        var dvl = (input.DecisionVariablesLine ?? "").Trim();
                        if (!string.IsNullOrEmpty(dvl))
                        {
                            var lower = dvl.ToLowerInvariant();

                            var binSet = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                            var binMatch = Regex.Match(lower, @"\bbin\s*:\s*([^;]+)");
                            if (binMatch.Success)
                            {
                                foreach (var tok in binMatch.Groups[1].Value.Split(new[] { ',', ' ' }, StringSplitOptions.RemoveEmptyEntries))
                                    binSet.Add(tok.Trim());
                            }

                            var intMatch = Regex.Match(lower, @"\bint\s*:\s*([^;]+)");
                            if (intMatch.Success)
                            {
                                var intSet = new HashSet<string>(StringComparer.OrdinalIgnoreCase);
                                foreach (var tok in intMatch.Groups[1].Value.Split(new[] { ',', ' ' }, StringSplitOptions.RemoveEmptyEntries))
                                    intSet.Add(tok.Trim());

                                for (int i = 0; i < n; i++)
                                {
                                    if (binSet.Contains(varNames[i])) { intR[i] = IntRestriction.Binary; isInt[i] = true; }
                                    else if (intSet.Contains(varNames[i])) { intR[i] = IntRestriction.Integer; isInt[i] = true; }
                                }
                            }
                        }
                        break;
                    }

                case "integer":
                default:
                    for (int i = 0; i < n; i++) { intR[i] = IntRestriction.Integer; isInt[i] = true; }
                    break;
            }

            return (intR, isInt);
        }

        private static (ObjectiveType objType, string expr) ExtractObjectiveTypeAndExpr(string raw)
        {
            var s = raw.TrimStart();
            ObjectiveType t;
            if (s.StartsWith("max", StringComparison.OrdinalIgnoreCase)) { t = ObjectiveType.Max; s = s.Substring(3).TrimStart(); }
            else if (s.StartsWith("min", StringComparison.OrdinalIgnoreCase)) { t = ObjectiveType.Min; s = s.Substring(3).TrimStart(); }
            else { t = ObjectiveType.Max; }
            var eq = s.IndexOf('=');
            if (eq >= 0) s = s.Substring(eq + 1).Trim();
            return (t, s);
        }

        private static (string left, string sign, double rhs) SplitConstraint(string line)
        {
            var m = Regex.Match(line, @"^(.*?)(<=|>=|=)\s*([+-]?\d+(?:\.\d+)?)\s*$");
            if (!m.Success) throw new Exception("Constraint must be 'left <= right', 'left >= right', or 'left = right'.");
            var left = m.Groups[1].Value.Trim();
            var sign = m.Groups[2].Value.Trim();
            if (!double.TryParse(m.Groups[3].Value, NumberStyles.Float, CultureInfo.InvariantCulture, out var rhs))
                throw new Exception("Right-hand side must be a number.");
            return (left, sign, rhs);
        }

        // ----- node/result helpers -----

        private static (bool has, IDictionary<string, double> vars, double z) TryGetSolution(RevisedBNBNode node)
        {
            try
            {
                var optMaybe = (node as ISimplexResultProvider).SimplexResult; // Option<SimplexResult>
                if (FSharpDu.TrySome(optMaybe, out var res))
                {
                    var (caseName, fields) = FSharpDu.ReadUnion(res);
                    if (caseName == "Optimal")
                    {
                        var vars = FSharpDu.ToDict(fields[1]);  // formulationVars
                        var z = (double)fields[2];
                        return (true, vars, z);
                    }
                }
            }
            catch { /* ignore */ }
            return (false, new Dictionary<string, double>(), double.NaN);
        }

        private static bool IsIntName(string name, string[] varNames, bool[] isIntVar)
        {
            for (int i = 0; i < varNames.Length && i < isIntVar.Length; i++)
                if (isIntVar[i] && string.Equals(varNames[i], name, StringComparison.OrdinalIgnoreCase))
                    return true;
            return false;
        }

        private static bool IsIntegral(IDictionary<string, double> sol, string[] varNames, bool[] isIntVar)
        {
            for (int i = 0; i < varNames.Length && i < isIntVar.Length; i++)
            {
                if (!isIntVar[i]) continue;
                if (!sol.TryGetValue(varNames[i], out var v)) continue;
                if (Math.Abs(v - Math.Round(v)) > 1e-9) return false;
            }
            return true;
        }
    }
}


