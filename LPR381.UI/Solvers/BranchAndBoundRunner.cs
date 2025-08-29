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

                // Build formulation (honors IntMode & optional "int:/bin:" list)
                var (model, varNames, isIntVar, objType) = BuildIntegerFormulation(input);

                // Best-so-far tracking (incumbent)
                double bestZ = (objType == ObjectiveType.Max) ? double.NegativeInfinity : double.PositiveInfinity;
                var bestVars = new Dictionary<string, double>();
                bool Better(double zNew, double zOld)
                    => objType == ObjectiveType.Max ? zNew > zOld + 1e-9 : zNew < zOld - 1e-9;

                // Counters (optional, appended to summary)
                int visited = 0, branched = 0, leaves = 0, fathomed = 0;

                // Root of B&B from your F#
                ITree<RevisedBNBNode> root = new RevisedBranchAndBound(model);

                var summary = new SolveSummary();

                // Recursive DFS so we can emit an explicit "Backtrack" entry on unwind
                void Explore(ITree<RevisedBNBNode> cur, int depth, string path)
                {
                    visited++;

                    var node = cur.Item;                    // RevisedBNBNode
                    var children = cur.Children;

                    // Decode state DU for a friendly title
                    var uf = FSharpValue.GetUnionFields(node.State, node.State.GetType(), null);
                    var stateName = uf.Item1.Name;
                    var stateFields = uf.Item2;

                    // Subproblem LP-relaxation solution at this node (if any)
                    var (hasSol, solVars, z, resultCase) = TryGetSolution(node);

                    // Node status (bound/integral) table — always add one per node
                    {
                        var t = new double[1, 3];
                        t[0, 0] = hasSol ? z : double.NaN;                       // Bound from LP relaxation
                        t[0, 1] = (bestVars.Count == 0) ? double.NaN :           // Best-so-far
                                  bestZ;
                        t[0, 2] = IsIntegral(solVars, varNames, isIntVar) ? 1 : 0; // Integral flag (1/0)

                        var title = $"Node {path} – {stateName}";
                        _iters.Add(new IterationTableau
                        {
                            Title = $"{title} – status",
                            Columns = new[] { "bound(z*)", "best", "integral?" },
                            Rows = new[] { "status" },
                            Values = t
                        });
                    }

                    // Branch decision (value / floor / ceil)
                    if (stateName == "Branch" && stateFields.Length >= 2 && hasSol)
                    {
                        var varIndex = (int)stateFields[0];
                        var varName = stateFields[1]?.ToString() ?? $"x{varIndex + 1}";
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
                            Title = $"Node {path} – branch on {varName}",
                            Columns = new[] { "value", "floor", "ceil" },
                            Rows = new[] { varName },
                            Values = t
                        });

                        branched++;
                    }

                    // Sub-problem snapshot (B^{-1}) if available
                    TryAddSubproblemSnapshot(node, path);

                    // If optimal LP solution exists, show it as a vector
                    if (hasSol)
                    {
                        var rows = solVars.Keys.ToArray();
                        var vals = new double[rows.Length, 1];
                        for (int i = 0; i < rows.Length; i++) vals[i, 0] = solVars[rows[i]];

                        _iters.Add(new IterationTableau
                        {
                            Title = $"Node {path} – {resultCase}",
                            Columns = new[] { "value" },
                            Rows = rows,
                            Values = vals
                        });

                        // Update incumbent if integral and better
                        if (IsIntegral(solVars, varNames, isIntVar) && (bestVars.Count == 0 || Better(z, bestZ)))
                        {
                            bestZ = z;
                            bestVars = new Dictionary<string, double>(solVars);
                            summary.IsOptimal = true;
                            summary.Objective = z;
                            summary.VariableValues = new Dictionary<string, double>(solVars);
                            summary.Message = "Optimal integer solution found.";
                        }
                    }

                    // Fathoming (leaf) annotation
                    if (children.Length == 0)
                    {
                        leaves++;
                        string reason = "exhausted";
                        if (!hasSol || resultCase == "Infeasible") reason = "infeasible";
                        else if (IsIntegral(solVars, varNames, isIntVar)) reason = "integral";
                        else if (bestVars.Count > 0 && !Better(z, bestZ)) reason = "bound";

                        fathomed++;
                        _iters.Add(new IterationTableau
                        {
                            Title = $"Node {path} – FATHOMED ({reason})",
                            Columns = new[] { "reason" },
                            Rows = new[] { "—" },
                            Values = new double[1, 1] { { double.NaN } }   // prints as blank
                        });
                    }

                    // Explore children (this *creates and displays all sub-problems*)
                    for (int i = 0; i < children.Length; i++)
                        Explore(children[i], depth + 1, path + "." + (i + 1));

                    // Explicit backtrack marker when we unwind this node
                    _iters.Add(new IterationTableau
                    {
                        Title = $"Backtrack from Node {path}",
                        Columns = new[] { " " },
                        Rows = new[] { " " },
                        Values = new double[1, 1] { { double.NaN } }
                    });
                }

                // Run DFS
                Explore(root, 0, "1");

                // Final best-candidate banner (for emphasis at the end)
                if (bestVars.Count > 0)
                {
                    var rows = bestVars.Keys.ToArray();
                    var vals = new double[rows.Length, 1];
                    for (int i = 0; i < rows.Length; i++) vals[i, 0] = bestVars[rows[i]];

                    _iters.Add(new IterationTableau
                    {
                        Title = $"Best candidate (incumbent) – z = {bestZ}",
                        Columns = new[] { "value" },
                        Rows = rows,
                        Values = vals
                    });
                }

                // Append counters to the summary message
                if (string.IsNullOrEmpty(summary.Message))
                    summary.Message = "Branch & Bound finished.";
                summary.Message += $"\nVisited: {visited}, Branched: {branched}, Leaves: {leaves}, Fathomed: {fathomed}.";

                return summary;
            });
        }

        // ---------- Sub-problem snapshot (B^{-1}) ----------
        private void TryAddSubproblemSnapshot(RevisedBNBNode bnbNode, string path)
        {
            try
            {
                var sub = bnbNode.SubSolution; // RevisedSimplexNode
                var bInv = sub.BInverse;       // double[,]
                int m = bInv.GetLength(0);
                var rows = Enumerable.Range(1, m).Select(i => $"r{i}").ToArray();
                var cols = Enumerable.Range(1, m).Select(i => $"c{i}").ToArray();

                _iters.Add(new IterationTableau
                {
                    Title = $"Node {path} – Sub-problem B⁻¹",
                    Columns = cols,
                    Rows = rows,
                    Values = bInv
                });
            }
            catch
            {
                // if the sub-solver doesn't expose BInverse here, just skip
            }
        }

        // ---------- Formulation building (aligned to your Formulation.fs) ----------
        private static (LPFormulation model, string[] varNames, bool[] isIntVar, ObjectiveType objType)
            BuildIntegerFormulation(UserProblem input)
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
            // We'll size this to obj.LinearSum length; the F# ctor will reconcile sizes internally.
            var signR = Enumerable.Repeat(SignRestriction.Positive, obj.LinearSum.Length).ToArray();

            // Integer restrictions from UI mode / custom list
            var (intR, isInt) = BuildIntRestrictions(obj, cons, input);

            // Convenience ctor from your Formulation.fs
            var model = new LPFormulation(obj, cons, signR, intR);

            // Variable names after ctor
            return (model, model.VarNames, isInt, objType);
        }

        private static (IntRestriction[] intR, bool[] isInt) BuildIntRestrictions(LPObjective obj, LPConstraint[] cons, UserProblem input)
        {
            // Build names like the F# side: from objective + constraints
            var namesFromObj = obj.LinearSum.Select(t => t.Item2).ToArray();
            var namesFromCons = cons.SelectMany(c => c.LeftSide.Select(t => t.Item2)).ToArray();
            var varNames = namesFromObj.Concat(namesFromCons).Distinct(StringComparer.Ordinal).ToArray();
            int n = varNames.Length;

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

        // ---------- parsing helpers ----------
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

        // ---------- node/result helpers ----------
        private static (bool has, IDictionary<string, double> vars, double z, string caseName)
            TryGetSolution(RevisedBNBNode node)
        {
            try
            {
                var optMaybe = (node as ISimplexResultProvider).SimplexResult; // Option<SimplexResult>
                if (FSharpDu.TrySome(optMaybe, out var res))
                {
                    var (caseName, fields) = FSharpDu.ReadUnion(res);
                    switch (caseName)
                    {
                        case "Optimal":
                            return (true, FSharpDu.ToDict(fields[1]), (double)fields[2], "Optimal");
                        case "Unbounded":
                            return (false, new Dictionary<string, double>(), double.NaN, "Unbounded");
                        case "Infeasible":
                            return (false, new Dictionary<string, double>(), double.NaN, "Infeasible");
                    }
                }
            }
            catch { /* ignore */ }
            return (false, new Dictionary<string, double>(), double.NaN, "");
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


