namespace LPR381.Core

open System
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra

type KnapsackState =
    | Branch of fractionalItem:int
    | ResultState of result:SimplexResult

type KnapsackNode =
    internal {
        knapsack: KnapsackCanonical
        fixedItems: Dictionary<int, double>
        currentValues: Dictionary<int, double>
        state: KnapsackState
    }
    
    member this.Knapsack = this.knapsack
    member this.FixedItems = Dictionary<int, double>(this.fixedItems)
    member this.CurrentValues = Dictionary<int, double>(this.currentValues)
    member this.State = this.state
    
    member this.GetCurrentValues() =
        let result = Dictionary<string, double>()
        for i in 0 .. this.knapsack.VariableNames.Length - 1 do
            let varName = this.knapsack.VariableNames.[i]
            match this.currentValues.TryGetValue i with
            | true, value -> result.[varName] <- value
            | false, _ -> result.[varName] <- 0.0
        result
    
    member this.GetGivenValues() =
        let result = Dictionary<string, double>()
        for kvp in this.fixedItems do
            let varName = this.knapsack.VariableNames.[kvp.Key]
            result.[varName] <- kvp.Value
        result

    interface ISimplexResultProvider with
        member this.SimplexResult = 
            match this.state with
            | ResultState s -> Some s
            | _ -> Option.None

type KnapsackTree(item: KnapsackNode, formulation: LPFormulation) =
    
    static let solveFractionalKnapsack (knapsack: KnapsackCanonical) (fixedItems: Dictionary<int, double>) =
        let currentValues = Dictionary<int, double>()
        let mutable remainingWeight = knapsack.MaxWeight
        let mutable totalValue = 0.0
        let mutable fractionalItem = -1
        
        // First, account for fixed items
        for kvp in fixedItems do
            currentValues.[kvp.Key] <- kvp.Value
            remainingWeight <- remainingWeight - kvp.Value * knapsack.Weights.[kvp.Key]
            totalValue <- totalValue + kvp.Value * knapsack.Objective.[kvp.Key]
        
        // If fixed items already exceed capacity, this is infeasible
        if remainingWeight < 0.0 then
            // Return infeasible state - all items get their fixed values
            for i in 0 .. knapsack.VariableNames.Length - 1 do
                if not (fixedItems.ContainsKey i) then
                    currentValues.[i] <- 0.0
            (currentValues, -infinity, -1)
        else
            // Initialize all non-fixed items to 0
            for i in 0 .. knapsack.VariableNames.Length - 1 do
                if not (fixedItems.ContainsKey i) then
                    currentValues.[i] <- 0.0
            
            // Then, greedily pack items by rank order
            for itemIdx in knapsack.RankOrder do
                if not (fixedItems.ContainsKey itemIdx) then
                    let itemWeight = knapsack.Weights.[itemIdx]
                    let itemValue = knapsack.Objective.[itemIdx]
                    
                    if remainingWeight > 0.0 then
                        if itemWeight <= remainingWeight then
                            // Take the whole item
                            currentValues.[itemIdx] <- 1.0
                            remainingWeight <- remainingWeight - itemWeight
                            totalValue <- totalValue + itemValue
                        elif itemWeight > 0.0 then
                            // Take fractional amount
                            let fraction = remainingWeight / itemWeight
                            currentValues.[itemIdx] <- fraction
                            totalValue <- totalValue + fraction * itemValue
                            fractionalItem <- itemIdx
                            remainingWeight <- 0.0
            
            (currentValues, totalValue, fractionalItem)
    
    static let isIntegerSolution (currentValues: Dictionary<int, double>) =
        currentValues.Values |> Seq.forall (fun v -> abs(v - round(v)) < 1e-9)
    
    static let node (knapsack: KnapsackCanonical) (fixedItems: Dictionary<int, double>) (formulation: LPFormulation) =
        let (currentValues, totalValue, fractionalItem) = solveFractionalKnapsack knapsack fixedItems
        
        if isIntegerSolution currentValues then
            // This is a candidate solution
            let var_dict = Dictionary<string, double>()
            for i in 0 .. knapsack.VariableNames.Length - 1 do
                let varName = knapsack.VariableNames.[i]
                match currentValues.TryGetValue i with
                | true, value -> var_dict.[varName] <- value
                | false, _ -> var_dict.[varName] <- 0.0
            
            {
                knapsack = knapsack
                fixedItems = fixedItems
                currentValues = currentValues
                state = ResultState (Optimal(var_dict, formulation.fromLPCanonical var_dict, totalValue))
            }
        elif fractionalItem >= 0 then
            // Need to branch on the fractional item
            {
                knapsack = knapsack
                fixedItems = fixedItems
                currentValues = currentValues
                state = Branch fractionalItem
            }
        else
            // Should not happen in valid knapsack problems
            let var_dict = Dictionary<string, double>()
            {
                knapsack = knapsack
                fixedItems = fixedItems
                currentValues = currentValues
                state = ResultState (Infeasible 0)
            }
    
    let children = 
        lazy (
            match item.state with
            | ResultState _ -> [||]
            | Branch fractionalItemIdx ->
                // Create two children: one with the item set to 0, one with it set to 1
                let child0Fixed = Dictionary<int, double>(item.fixedItems)
                child0Fixed.[fractionalItemIdx] <- 0.0
                let child0 = KnapsackTree(node item.knapsack child0Fixed formulation, formulation)
                
                let child1Fixed = Dictionary<int, double>(item.fixedItems)
                child1Fixed.[fractionalItemIdx] <- 1.0
                let child1 = KnapsackTree(node item.knapsack child1Fixed formulation, formulation)
                
                [| child0 :> ITree<KnapsackNode>; child1 :> ITree<KnapsackNode> |]
        )
    
    new(formulation: LPFormulation) =
        let knapsackCanonical = formulation.ToKnapsackCanonical()
        let initialFixed = Dictionary<int, double>()
        KnapsackTree(node knapsackCanonical initialFixed formulation, formulation)
    
    interface ITree<KnapsackNode> with
        member _.Item = item
        member _.Children = children.Value
        member _.Formulation = formulation
        member _.SensitivityContext = null
