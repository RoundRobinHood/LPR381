using LPR381.Core;
using LPR381.UI.Models;
using Microsoft.FSharp.Reflection;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace LPR381.UI.Solvers
{
    public sealed class CuttingPlaneRunner : BaseSolver, ISolverRunner
    {
        private readonly List<IterationTableau> _iters = new();
        public string Key => "cutting-plane";
        public string Display => "Cutting Plane";
        public IReadOnlyList<IterationTableau> Iterations => _iters;

        public Task<SolveSummary> RunAsync(UserProblem input)
        {
            return Task.Run(() =>
            {
                _iters.Clear();

                // 1) Build model with integer restrictions for cutting plane
                var model = BuildIntegerFormulation(input);

                // 2) Create the Cutting-Plane solver from LPR381.Core (via reflection; no .fs edits).
                object solver;
                try
                {
                    solver = CreateCuttingSolver(model);
                }
                catch (Exception ex)
                {
                    throw new InvalidOperationException($"Failed to create cutting plane solver: {ex.Message}", ex);
                }

                // 3) Treat solver as an ITree<*> and walk it (DFS).
                var accessors = FindTreeAccessors(solver.GetType());
                if (accessors == null)
                {
                    var props = solver.GetType().GetProperties().Select(p => p.Name).ToArray();
                    var interfaces = solver.GetType().GetInterfaces().Select(i => i.Name).ToArray();
                    throw new InvalidOperationException($"Cutting-Plane solver does not expose ITree<>-like Item/Children. Type: {solver.GetType().Name}, Properties: [{string.Join(", ", props)}], Interfaces: [{string.Join(", ", interfaces)}]");
                }
                var (itemProp, childrenProp) = accessors.Value;

                // Track best integer (incumbent)
                var objType = model.ObjectiveType;
                double bestZ = (objType == ObjectiveType.Max) ? double.NegativeInfinity : double.PositiveInfinity;
                var bestVars = new Dictionary<string, double>();
                bool Better(double a, double b) => objType == ObjectiveType.Max ? a > b + 1e-9 : a < b - 1e-9;

                var stack = new Stack<(object nodeWrapper, string path)>();
                stack.Push((solver, "1"));

                while (stack.Count > 0)
                {
                    var (cur, path) = stack.Pop();
                    var node = itemProp.GetValue(cur)!; // underlying node (record/DU)
                    var stateName = TryReadUnionName(node, "State") ?? "Node";

                    // LP relaxation result at this node (if exposed)
                    var (hasSol, z, vars, caseName) = TryGetSimplex(node);

                    // Status
                    var status = new double[1, 3];
                    status[0, 0] = hasSol ? z : double.NaN;                  // bound
                    status[0, 1] = bestVars.Count == 0 ? double.NaN : bestZ; // best
                    status[0, 2] = IsIntegral(vars) ? 1 : 0;                 // integral?
                    _iters.Add(new IterationTableau
                    {
                        Title = $"CP {path} – {stateName} – status",
                        Columns = new[] { "bound(z*)", "best", "integral?" },
                        Rows = new[] { "status" },
                        Values = status
                    });

                    // Try to show the cut added at this node (when present)
                    TryAddCutRow(node, path);

                    // Optional: show B^{-1} if node carries a revised-simplex sub-solution
                    TryAddBInverseSnapshot(node, path);

                    // Current relaxation solution vector
                    if (hasSol && vars.Count > 0)
                    {
                        var rows = vars.Keys.ToArray();
                        var vals = new double[rows.Length, 1];
                        for (int i = 0; i < rows.Length; i++) vals[i, 0] = vars[rows[i]];
                        _iters.Add(new IterationTableau
                        {
                            Title = $"CP {path} – {caseName}",
                            Columns = new[] { "value" },
                            Rows = rows,
                            Values = vals
                        });

                        // Update incumbent if integral and better
                        if (IsIntegral(vars) && (bestVars.Count == 0 || Better(z, bestZ)))
                        {
                            bestZ = z;
                            bestVars = new Dictionary<string, double>(vars);
                        }
                    }

                    // Push children in reverse to get left→right order
                    var kids = (Array)childrenProp.GetValue(cur)!;
                    for (int i = kids.Length - 1; i >= 0; i--)
                        stack.Push((kids.GetValue(i)!, $"{path}.{i + 1}"));
                }

                // Summary
                var summary = new SolveSummary();
                if (bestVars.Count > 0)
                {
                    summary.IsOptimal = true;
                    summary.Objective = bestZ;
                    summary.VariableValues = new Dictionary<string, double>(bestVars);
                    summary.Message = "Cutting Plane finished (integer incumbent found).";

                    var rows = bestVars.Keys.ToArray();
                    var vals = new double[rows.Length, 1];
                    for (int i = 0; i < rows.Length; i++) vals[i, 0] = bestVars[rows[i]];
                    _iters.Add(new IterationTableau
                    {
                        Title = $"Best integer (incumbent) – z = {bestZ}",
                        Columns = new[] { "value" },
                        Rows = rows,
                        Values = vals
                    });
                }
                else
                {
                    summary.IsOptimal = false;
                    summary.Message = "Cutting Plane finished (no integer incumbent).";
                }

                return summary;
            });
        }

        // ---------- helpers ----------

        private LPFormulation BuildIntegerFormulation(UserProblem input)
        {
            var baseModel = BuildFormulation(input); // from BaseSolver
            
            // Create integer restrictions (all variables as integers)
            var intRestrictions = new IntRestriction[baseModel.VarNames.Length];
            for (int i = 0; i < intRestrictions.Length; i++)
                intRestrictions[i] = IntRestriction.Integer;
            
            // Rebuild with integer restrictions
            return new LPFormulation(
                baseModel.ObjectiveType,
                baseModel.VarNames,
                baseModel.Objective,
                baseModel.ConstraintCoefficients,
                baseModel.ConstraintSigns,
                baseModel.RHS,
                baseModel.VarSignRestrictions,
                intRestrictions
            );
        }

        private static (System.Reflection.PropertyInfo item, System.Reflection.PropertyInfo children)? FindTreeAccessors(Type t)
        {
            // locate Item / Children on the concrete type that implements ITree<…>
            var item = t.GetProperty("Item") ?? t.GetProperty("Current") ?? t.GetProperty("Node");
            var kids = t.GetProperty("Children") ?? t.GetProperty("Nodes") ?? t.GetProperty("Next");
            if (item != null && kids != null) return (item, kids);

            // Check interface implementations (F# explicit interface implementations)
            var itreeInterface = t.GetInterfaces().FirstOrDefault(i => i.IsGenericType && i.GetGenericTypeDefinition().Name == "ITree`1");
            if (itreeInterface != null)
            {
                var interfaceMap = t.GetInterfaceMap(itreeInterface);
                PropertyInfo? itemProp = null, childrenProp = null;
                
                foreach (var method in interfaceMap.InterfaceMethods)
                {
                    if (method.Name.StartsWith("get_Item"))
                        itemProp = itreeInterface.GetProperty("Item");
                    else if (method.Name.StartsWith("get_Children"))
                        childrenProp = itreeInterface.GetProperty("Children");
                }
                
                if (itemProp != null && childrenProp != null)
                    return (itemProp, childrenProp);
            }

            // sometimes the ITree implementation is on the base type
            if (t.BaseType != null)
            {
                var bi = t.BaseType.GetProperty("Item") ?? t.BaseType.GetProperty("Current") ?? t.BaseType.GetProperty("Node");
                var bk = t.BaseType.GetProperty("Children") ?? t.BaseType.GetProperty("Nodes") ?? t.BaseType.GetProperty("Next");
                if (bi != null && bk != null) return (bi, bk);
            }
            return null;
        }

        private static object CreateCuttingSolver(LPFormulation model)
        {
            var asm = typeof(LPFormulation).Assembly;

            // First, try to find RevisedCuttingPlanes directly
            var cuttingPlanesType = asm.GetType("LPR381.Core.RevisedCuttingPlanes");
            if (cuttingPlanesType != null)
            {
                var ctor = cuttingPlanesType.GetConstructor(new[] { typeof(LPFormulation) });
                if (ctor != null)
                    return ctor.Invoke(new object[] { model });
            }

            bool LooksLikeSolver(Type t)
            {
                if (!t.IsClass || !t.IsPublic) return false;
                if (t.Namespace != "LPR381.Core") return false;

                var n = t.Name.ToLowerInvariant();
                // must be a cutting/gomory thing
                if (!(n.Contains("cut") || n.Contains("gomory"))) return false;
                // exclude common non-solver artifacts
                if (n.EndsWith("state") || n.EndsWith("node") || n.EndsWith("result") || n.EndsWith("info"))
                    return false;

                // must implement ITree<...> or expose Item/Children
                var hasITree = t.GetInterfaces().Any(i => i.IsGenericType && i.Name.StartsWith("ITree`", StringComparison.Ordinal));
                var hasProps = (t.GetProperty("Children") != null)
                            && (t.GetProperty("Item") != null || t.GetProperty("Current") != null || t.GetProperty("Node") != null);

                return hasITree || hasProps;
            }

            // candidates ordered to prefer those with LPFormulation ctor
            var candidates = asm.GetTypes()
                .Where(LooksLikeSolver)
                .OrderByDescending(t => t.GetConstructor(new[] { typeof(LPFormulation) }) != null)
                .ToList();

            // 1) Prefer a direct (LPFormulation) constructor
            foreach (var t in candidates)
            {
                var ctor = t.GetConstructor(new[] { typeof(LPFormulation) });
                if (ctor != null)
                    return ctor.Invoke(new object[] { model });
            }

            // 2) Otherwise try common static factories that return an ITree-like object
            string[] factoryNames = { "Create", "Start", "Init", "FromModel", "Build", "Run" };
            foreach (var t in candidates)
            {
                foreach (var fname in factoryNames)
                {
                    var m = t.GetMethod(fname, BindingFlags.Public | BindingFlags.Static, binder: null,
                                        types: new[] { typeof(LPFormulation) }, modifiers: null);
                    if (m == null) continue;

                    var obj = m.Invoke(null, new object[] { model });
                    if (obj != null)
                    {
                        // basic sanity: returned object should look like an ITree wrapper
                        var hasChildren = obj.GetType().GetProperty("Children") != null;
                        var hasItem = obj.GetType().GetProperty("Item") != null
                                          || obj.GetType().GetProperty("Current") != null
                                          || obj.GetType().GetProperty("Node") != null;
                        if (hasChildren && hasItem)
                            return obj;
                    }
                }
            }

            throw new InvalidOperationException(
                "Could not find a Cutting-Plane solver class that implements ITree and accepts LPFormulation (or a factory that returns it). " +
                "If your solver has a different public name or signature, tell me the class/factory name and I’ll pin the selector to it.");
        }


        private static (bool has, double z, Dictionary<string, double> vars, string caseName) TryGetSimplex(object node)
        {
            try
            {
                // If the node implements ISimplexResultProvider, read Option<SimplexResult> (null => None)
                if (node is ISimplexResultProvider p)
                {
                    var opt = p.SimplexResult;
                    if (opt != null)
                    {
                        var (name, fields) = ReadDu(opt.Value);
                        if (name == "Optimal")
                            return (true, (double)fields[2], ToDict(fields[1]), "Optimal");
                        return (false, double.NaN, new Dictionary<string, double>(), name);
                    }
                }

                // Else try a SimplexResult property on the node
                var sr = node.GetType().GetProperty("SimplexResult")?.GetValue(node);
                if (sr != null)
                {
                    var (name, fields) = ReadDu(sr);
                    if (name == "Optimal")
                        return (true, (double)fields[2], ToDict(fields[1]), "Optimal");
                    return (false, double.NaN, new Dictionary<string, double>(), name);
                }
            }
            catch { /* ignore */ }

            return (false, double.NaN, new Dictionary<string, double>(), "");
        }

        private static string? TryReadUnionName(object node, string propName)
        {
            try
            {
                var state = node.GetType().GetProperty(propName)?.GetValue(node);
                if (state == null) return null;
                var uf = FSharpValue.GetUnionFields(state, state.GetType(), null);
                return uf.Item1.Name;
            }
            catch { return null; }
        }

        private void TryAddCutRow(object node, string path)
        {
            // Heuristic: look for a property that holds the last/added cut
            // Common names: Cut, AddedCut, GomoryCut, LastCut
            var cutObj =
                node.GetType().GetProperty("Cut")?.GetValue(node) ??
                node.GetType().GetProperty("AddedCut")?.GetValue(node) ??
                node.GetType().GetProperty("GomoryCut")?.GetValue(node) ??
                node.GetType().GetProperty("LastCut")?.GetValue(node);

            if (cutObj == null) return;

            double[]? coeffs = TryGet<double[]>(cutObj, "Coefficients")
                            ?? TryGet<double[]>(cutObj, "Coeff")
                            ?? TryGet<double[]>(cutObj, "Left")
                            ?? TryGet<double[]>(cutObj, "Row");

            object rhsObj =
                cutObj.GetType().GetProperty("Rhs")?.GetValue(cutObj)
                ?? cutObj.GetType().GetProperty("RightHandSide")?.GetValue(cutObj)
                ?? cutObj.GetType().GetProperty("B")?.GetValue(cutObj);

            double rhs = rhsObj != null
                ? Convert.ToDouble(rhsObj, CultureInfo.InvariantCulture)
                : double.NaN;

            string[]? names = TryGet<string[]>(cutObj, "Names")
                           ?? TryGet<string[]>(cutObj, "Variables")
                           ?? TryGet<string[]>(cutObj, "VarNames");

            if (coeffs == null) return;

            var cols = (names != null && names.Length == coeffs.Length)
                       ? names
                       : Enumerable.Range(1, coeffs.Length).Select(i => $"x{i}").ToArray();

            var M = new double[1, coeffs.Length + 1];
            for (int j = 0; j < coeffs.Length; j++) M[0, j] = coeffs[j];
            M[0, coeffs.Length] = rhs;

            _iters.Add(new IterationTableau
            {
                Title = $"CP {path} – Added cut",
                Columns = cols.Concat(new[] { "rhs" }).ToArray(),
                Rows = new[] { "cut" },
                Values = M
            });
        }

        private void TryAddBInverseSnapshot(object node, string path)
        {
            try
            {
                var sub = node.GetType().GetProperty("SubSolution")?.GetValue(node);
                if (sub == null) return;
                var bInv = (double[,])sub.GetType().GetProperty("BInverse")!.GetValue(sub)!;

                int m = bInv.GetLength(0);
                var rows = Enumerable.Range(1, m).Select(i => $"r{i}").ToArray();
                var cols = Enumerable.Range(1, m).Select(i => $"c{i}").ToArray();

                _iters.Add(new IterationTableau
                {
                    Title = $"CP {path} – Sub-problem B⁻¹",
                    Columns = cols,
                    Rows = rows,
                    Values = bInv
                });
            }
            catch { /* optional */ }
        }

        // tiny reflection helpers
        private static (string name, object[] fields) ReadDu(object du)
        {
            var uf = FSharpValue.GetUnionFields(du, du.GetType(), null);
            return (uf.Item1.Name, uf.Item2);
        }

        private static Dictionary<string, double> ToDict(object mapLike)
        {
            if (mapLike is IEnumerable<KeyValuePair<string, double>> kvs)
                return new Dictionary<string, double>(kvs);

            var dict = new Dictionary<string, double>();
            if (mapLike is System.Collections.IEnumerable en)
            {
                foreach (var it in en)
                {
                    if (it is null) continue;
                    var t = it.GetType();
                    var key = t.GetProperty("Key")?.GetValue(it)?.ToString() ?? "";
                    var val = t.GetProperty("Value")?.GetValue(it);
                    if (!string.IsNullOrEmpty(key) && val != null)
                        dict[key] = Convert.ToDouble(val, CultureInfo.InvariantCulture);
                }
            }
            return dict;
        }

        private static T? TryGet<T>(object obj, string name)
        {
            var p = obj.GetType().GetProperty(name);
            if (p == null) return default;
            var v = p.GetValue(obj);
            if (v is T t) return t;
            try { return (T)Convert.ChangeType(v, typeof(T), CultureInfo.InvariantCulture); } catch { return default; }
        }

        private static bool IsIntegral(Dictionary<string, double> sol)
        {
            foreach (var v in sol.Values)
                if (Math.Abs(v - Math.Round(v)) > 1e-9) return false;
            return true;
        }
    }
}
