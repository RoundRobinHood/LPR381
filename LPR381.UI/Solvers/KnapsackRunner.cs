using LPR381.Core;
using LPR381.UI.Core;
using LPR381.UI.Models;
using System.Collections.Generic;
using System.Linq;

namespace LPR381.UI.Solvers
{
    public sealed class KnapsackRunner : SolverRunner
    {
        public override string Key => "knapsack";
        public override string Display => "Knapsack";

        public override LPFormulation BuildFormulation(UserProblem input)
        {
            var baseModel = base.BuildFormulation(input);
            var intRestrictions = new IntRestriction[baseModel.VarNames.Length];
            System.Array.Fill(intRestrictions, IntRestriction.Binary);
            
            return new LPFormulation(
                baseModel.ObjectiveType, baseModel.VarNames, baseModel.Objective,
                baseModel.ConstraintCoefficients, baseModel.ConstraintSigns, baseModel.RHS,
                baseModel.VarSignRestrictions, intRestrictions);
        }

        protected override SolveSummary Solve(LPFormulation model)
        {
            _iterations.Clear();
            AddCanonicalForm(model);
            
            var knapsack = new KnapsackTree(model);
            var result = SolveKnapsackWithBacktracking(knapsack, model);
            
            return result;
        }
        
        private SolveSummary SolveKnapsackWithBacktracking(KnapsackTree knapsack, LPFormulation model)
        {
            var stack = new Stack<(ITree<KnapsackNode> node, string path, int depth)>();
            stack.Push((knapsack, "Root", 0));
            
            var bestSolution = new SolveSummary();
            var allNodes = new List<(string path, KnapsackNode node, bool fathomed, string reason)>();
            
            while (stack.Count > 0)
            {
                var (node, path, depth) = stack.Pop();
                var item = node.Item;
                var currentVals = item.GetCurrentValues();
                var givenVals = item.GetGivenValues();
                
                // Show sub-problem table
                var knapsackCanon = item.Knapsack;
                var subProbMatrix = new double[knapsackCanon.VariableNames.Length, 4];
                double totalValue = 0, totalWeight = 0;
                
                for (int i = 0; i < knapsackCanon.VariableNames.Length; i++)
                {
                    var varName = knapsackCanon.VariableNames[i];
                    var value = currentVals.ContainsKey(varName) ? currentVals[varName] : 0.0;
                    
                    subProbMatrix[i, 0] = knapsackCanon.Objective[i];
                    subProbMatrix[i, 1] = knapsackCanon.Weights[i];
                    subProbMatrix[i, 2] = value;
                    subProbMatrix[i, 3] = knapsackCanon.Objective[i] * value;
                    
                    totalValue += knapsackCanon.Objective[i] * value;
                    totalWeight += knapsackCanon.Weights[i] * value;
                }
                
                _iterations.Add(new IterationTableau
                {
                    Title = $"Sub-problem {path} (Depth {depth}) - Weight: {totalWeight:F2}/{knapsackCanon.MaxWeight:F2}",
                    Columns = new[] { "Value", "Weight", "x_i", "Value*x_i" },
                    Rows = knapsackCanon.VariableNames,
                    Values = subProbMatrix
                });
                
                var (stateCase, stateFields) = FSharpInterop.ReadUnion(item.State);
                bool fathomed = false;
                string fathomReason = "";
                
                if (stateCase == "ResultState")
                {
                    var resultState = (SimplexResult)stateFields[0];
                    var (resultCase, resultFields) = FSharpInterop.ReadUnion(resultState);
                    
                    if (resultCase == "Optimal")
                    {
                        var objValue = (double)resultFields[2];
                        var vars = FSharpInterop.ToDict(resultFields[1]);
                        
                        fathomed = true;
                        fathomReason = "Integer solution found";
                        
                        if (!bestSolution.IsOptimal || 
                            (model.ObjectiveType == ObjectiveType.Max && objValue > bestSolution.Objective) ||
                            (model.ObjectiveType == ObjectiveType.Min && objValue < bestSolution.Objective))
                        {
                            bestSolution.IsOptimal = true;
                            bestSolution.Objective = objValue;
                            bestSolution.VariableValues = vars;
                            bestSolution.Message = $"Best candidate found at node {path}";
                            fathomReason = "New best candidate";
                        }
                        else
                        {
                            fathomReason = "Dominated by best candidate";
                        }
                    }
                    else if (resultCase == "Infeasible")
                    {
                        fathomed = true;
                        fathomReason = "Infeasible sub-problem";
                        
                        // Show infeasible tree identification
                        var infeasMatrix = new double[1, 2];
                        infeasMatrix[0, 0] = totalWeight;
                        infeasMatrix[0, 1] = knapsackCanon.MaxWeight;
                        
                        _iterations.Add(new IterationTableau
                        {
                            Title = $"INFEASIBLE TREE at node {path} - Weight exceeds capacity",
                            Columns = new[] { "Current Weight", "Max Capacity" },
                            Rows = new[] { "Constraint" },
                            Values = infeasMatrix
                        });
                    }
                }
                else if (stateCase == "Branch")
                {
                    var fractionalItem = (int)stateFields[0];
                    
                    if (!fathomed)
                    {
                        // Add children to stack (reverse order for proper traversal)
                        var children = node.Children;
                        for (int i = children.Length - 1; i >= 0; i--)
                        {
                            var childPath = path == "Root" ? $"{i+1}" : $"{path}.{i+1}";
                            stack.Push((children[i], childPath, depth + 1));
                        }
                        
                        // Show branching decision
                        var branchMatrix = new double[1, 3];
                        branchMatrix[0, 0] = fractionalItem;
                        branchMatrix[0, 1] = totalValue;
                        branchMatrix[0, 2] = children.Length;
                        
                        _iterations.Add(new IterationTableau
                        {
                            Title = $"Node {path} - Branching on item {fractionalItem}",
                            Columns = new[] { "Item Index", "LP Bound", "Children" },
                            Rows = new[] { "Branch" },
                            Values = branchMatrix
                        });
                    }
                }
                
                allNodes.Add((path, item, fathomed, fathomReason));
                
                // Show fathoming status
                var statusMatrix = new double[1, 4];
                statusMatrix[0, 0] = totalValue;
                statusMatrix[0, 1] = totalWeight;
                statusMatrix[0, 2] = fathomed ? 1 : 0;
                statusMatrix[0, 3] = depth;
                
                _iterations.Add(new IterationTableau
                {
                    Title = $"Node {path} - Status: {(fathomed ? $"FATHOMED ({fathomReason})" : "BRANCH")}",
                    Columns = new[] { "LP Value", "Weight", "Fathomed", "Depth" },
                    Rows = new[] { "Status" },
                    Values = statusMatrix
                });
            }
            
            // Show final summary of all nodes
            var summaryMatrix = new double[allNodes.Count, 3];
            var summaryRows = new string[allNodes.Count];
            
            for (int i = 0; i < allNodes.Count; i++)
            {
                var (path, node, fathomed, reason) = allNodes[i];
                var currentVals = node.GetCurrentValues();
                double nodeValue = 0;
                
                for (int j = 0; j < node.Knapsack.VariableNames.Length; j++)
                {
                    var varName = node.Knapsack.VariableNames[j];
                    var value = currentVals.ContainsKey(varName) ? currentVals[varName] : 0.0;
                    nodeValue += node.Knapsack.Objective[j] * value;
                }
                
                summaryMatrix[i, 0] = nodeValue;
                summaryMatrix[i, 1] = fathomed ? 1 : 0;
                summaryMatrix[i, 2] = bestSolution.IsOptimal && 
                    System.Math.Abs(nodeValue - bestSolution.Objective) < 1e-6 ? 1 : 0;
                summaryRows[i] = path;
            }
            
            _iterations.Add(new IterationTableau
            {
                Title = "All Sub-problems Summary",
                Columns = new[] { "LP Value", "Fathomed", "Best" },
                Rows = summaryRows,
                Values = summaryMatrix
            });
            
            // Show best candidate
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
                    Title = $"BEST CANDIDATE - Objective = {bestSolution.Objective}",
                    Columns = new[] { "Value" },
                    Rows = varNames,
                    Values = finalMatrix
                });
            }
            
            return bestSolution;
        }
    }
}
