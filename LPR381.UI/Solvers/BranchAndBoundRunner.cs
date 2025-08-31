using LPR381.Core;
using LPR381.UI.Core;
using LPR381.UI.Models;
using System.Collections.Generic;
using System.Linq;

namespace LPR381.UI.Solvers
{
    public sealed class BranchAndBoundRunner : SolverRunner
    {
        public override string Key => "branch-and-bound";
        public override string Display => "Branch & Bound";

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
            
            var bnb = new RevisedBranchAndBound(model);
            var result = SolveBNBWithIterations(bnb, model);
            
            return result;
        }
        
        private SolveSummary SolveBNBWithIterations(RevisedBranchAndBound bnb, LPFormulation model)
        {
            var queue = new Queue<(ITree<RevisedBNBNode> node, string path, int depth)>();
            queue.Enqueue((bnb, "1", 0));
            
            var bestSolution = new SolveSummary();
            var nodeCount = 0;
            
            while (queue.Count > 0)
            {
                var (node, path, depth) = queue.Dequeue();
                nodeCount++;
                
                var item = node.Item;
                var subSolution = item.SubSolution;
                
                // Show sub-problem tableau
                var binv = subSolution.BInverse;
                int m = binv.GetLength(0);
                var rc = Enumerable.Range(1, m).Select(i => $"r{i}").ToArray();
                var cc = Enumerable.Range(1, m).Select(i => $"c{i}").ToArray();
                
                _iterations.Add(new IterationTableau
                {
                    Title = $"Node {path} (Depth {depth}) - B⁻¹",
                    Columns = cc,
                    Rows = rc,
                    Values = binv
                });
                
                // Show node status
                var (stateCase, stateFields) = FSharpInterop.ReadUnion(item.State);
                var statusMatrix = new double[1, 3];
                statusMatrix[0, 0] = subSolution.ObjectiveValue;
                statusMatrix[0, 1] = stateCase == "Branch" ? 0 : 1; // 0=branch, 1=fathom
                statusMatrix[0, 2] = depth;
                
                _iterations.Add(new IterationTableau
                {
                    Title = $"Node {path} - Status ({stateCase})",
                    Columns = new[] { "LP Bound", "Fathomed", "Depth" },
                    Rows = new[] { "Value" },
                    Values = statusMatrix
                });

                var gens = new RevisedSimplexRunner(false);
                gens.Solve($"Node {path} - ", node.Item.RevisedTree);
                
                _iterations.AddRange(gens.Iterations);
                
                if (stateCase == "ResultState")
                {
                    var resultState = (SimplexResult)stateFields[0];
                    var (resultCase, resultFields) = FSharpInterop.ReadUnion(resultState);
                    
                    if (resultCase == "Optimal")
                    {
                        var objValue = (double)resultFields[2];
                        var vars = FSharpInterop.ToDict(resultFields[1]);
                        
                        if (!bestSolution.IsOptimal || 
                            (model.ObjectiveType == ObjectiveType.Max && objValue > bestSolution.Objective) ||
                            (model.ObjectiveType == ObjectiveType.Min && objValue < bestSolution.Objective))
                        {
                            bestSolution.IsOptimal = true;
                            bestSolution.Objective = objValue;
                            bestSolution.VariableValues = vars;
                            bestSolution.Message = $"Best candidate found at node {path}";
                        }
                    }
                }
                else if (stateCase == "Branch")
                {
                    // Add children to queue
                    var children = node.Children;
                    for (int i = 0; i < children.Length; i++)
                    {
                        queue.Enqueue((children[i], $"{path}.{i+1}", depth + 1));
                    }
                }
            }
            
            // Show final best candidate
            if (bestSolution.IsOptimal)
            {
                var finalMatrix = new double[bestSolution.VariableValues.Count, 1];
                var varNames = bestSolution.VariableValues.Keys.ToArray();
                for (int i = 0; i < varNames.Length; i++)
                {
                    finalMatrix[i, 0] = bestSolution.VariableValues[varNames[i]];
                }
                
                _iterations.Add(new IterationTableau
                {
                    Title = $"Best Candidate - Objective = {bestSolution.Objective}",
                    Columns = new[] { "Value" },
                    Rows = varNames,
                    Values = finalMatrix
                });
            }
            
            return bestSolution;
        }
    }
}
