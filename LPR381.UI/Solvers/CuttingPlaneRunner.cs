using LPR381.Core;
using LPR381.UI.Core;
using LPR381.UI.Models;
using System.Collections.Generic;
using System.Linq;
using MathNet.Numerics.LinearAlgebra;

namespace LPR381.UI.Solvers
{
    public sealed class CuttingPlaneRunner : SolverRunner
    {
        public override string Key => "cutting-plane";
        public override string Display => "Cutting Plane";

        public override LPFormulation BuildFormulation(UserProblem input)
        {
            var baseModel = base.BuildFormulation(input);
            var intRestrictions = new IntRestriction[baseModel.VarNames.Length];
            System.Array.Fill(intRestrictions, IntRestriction.Integer);
            
            return new LPFormulation(
                baseModel.ObjectiveType, baseModel.VarNames, baseModel.Objective,
                baseModel.ConstraintCoefficients, baseModel.ConstraintSigns, baseModel.RHS,
                baseModel.VarSignRestrictions, intRestrictions);
        }

        protected override SolveSummary Solve(LPFormulation model)
        {
            _iterations.Clear();
            AddCanonicalForm(model);
            
            var solver = new RevisedCuttingPlanes(model);
            var result = SolveCuttingPlaneWithIterations(solver);
            
            return result;
        }
        
        private SolveSummary SolveCuttingPlaneWithIterations(RevisedCuttingPlanes solver)
        {
            var stack = new Stack<(ITree<RevisedCuttingPlanesNode> node, int iteration)>();
            stack.Push((solver, 0));
            var summary = new SolveSummary();
            
            while (stack.Count > 0)
            {
                var (cur, iter) = stack.Pop();
                var item = cur.Item;
                var subSolution = item.SubSolution;
                
                // Show Product Form (B⁻¹)
                var binv = subSolution.BInverse;
                int m = binv.GetLength(0);
                var rc = Enumerable.Range(1, m).Select(i => $"r{i}").ToArray();
                var cc = Enumerable.Range(1, m).Select(i => $"c{i}").ToArray();
                
                _iterations.Add(new IterationTableau
                {
                    Title = $"CP Iteration {iter} - B⁻¹ (Product Form)",
                    Columns = cc,
                    Rows = rc,
                    Values = binv
                });

                var gens = new RevisedSimplexRunner(false);
                gens.Solve($"CP Iteration {iter} - ", item.RevisedTree);
                _iterations.AddRange(gens.Iterations);
                
                // Show current solution
                var canon = subSolution.Canon;
                var bInvMatrix = Matrix<double>.Build.DenseOfArray(subSolution.BInverse);
                var xB = bInvMatrix * canon.RHS;
                var solMatrix = new double[m, 1];
                for (int i = 0; i < m; i++)
                {
                    solMatrix[i, 0] = xB[i];
                }
                
                _iterations.Add(new IterationTableau
                {
                    Title = $"CP Iteration {iter} - Current Solution",
                    Columns = new[] { "x_B" },
                    Rows = subSolution.Basis.Select(b => canon.VariableNames[b]).ToArray(),
                    Values = solMatrix
                });
                
                var (stateCase, stateFields) = FSharpInterop.ReadUnion(item.State);
                
                if (stateCase == "ResultState")
                {
                    var resultState = (SimplexResult)stateFields[0];
                    var (resultCase, resultFields) = FSharpInterop.ReadUnion(resultState);
                    
                    if (resultCase == "Optimal")
                    {
                        summary.IsOptimal = true;
                        summary.Objective = (double)resultFields[2];
                        summary.VariableValues = FSharpInterop.ToDict(resultFields[1]);
                        summary.Message = "Cutting Plane: Optimal integer solution found.";
                    }
                    else
                    {
                        summary.Message = $"Cutting Plane: {resultCase}";
                    }
                }
                else if (stateCase == "Cut")
                {
                    // Show the cut being added
                    var cutVar = (int)stateFields[0];
                    var cutVarName = (string)stateFields[1];
                    
                    var cutMatrix = new double[1, 2];
                    cutMatrix[0, 0] = cutVar;
                    cutMatrix[0, 1] = subSolution.ObjectiveValue;
                    
                    _iterations.Add(new IterationTableau
                    {
                        Title = $"CP Iteration {iter} - Gomory Cut Added",
                        Columns = new[] { "Fractional Var", "LP Value" },
                        Rows = new[] { cutVarName },
                        Values = cutMatrix
                    });
                    
                    // Continue with children
                    var children = cur.Children;
                    for (int i = children.Length - 1; i >= 0; --i)
                        stack.Push((children[i], iter + 1));
                }
            }
            
            return summary;
        }
    }
}
