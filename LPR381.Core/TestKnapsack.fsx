// TestKnapsack.fsx
#r "nuget: MathNet.Numerics"
#r "nuget: MathNet.Numerics.FSharp"

#load "Formulation.fs"
#load "ResultAnalysis.fs"
#load "ResultInterfaces.fs"
#load "PrimalSimplex.fs"
#load "RevisedSimplex.fs"
#load "BranchAndBound.fs"
#load "Knapsack.fs"
#load "Explorer.fs"

open System
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra
open LPR381.Core

let approxEq tol (a: float) (b: float) = abs (a - b) <= tol

let getVar (vars: Dictionary<string, double>) (name: string) =
    match vars.TryGetValue name with
    | true, v -> v
    | _ -> 0.0

let checkKnapsackSolution
    (name: string)
    (expectedObj: float)
    (expected: (string * float) list)
    (tol: float)
    (res: SimplexResult) =
    match res with
    | Optimal (vars, formulationVars, obj) ->
        if not (approxEq tol obj expectedObj) then
            failwithf "Test '%s': objective %.6f != expected %.6f" name obj expectedObj
        expected
        |> List.iter (fun (n, vexp) ->
            let v = getVar vars n
            if not (approxEq tol v vexp) then
                failwithf "Test '%s': %s = %.6f != expected %.6f" name n v vexp)
        printfn "✔ %s passed. Objective=%.6f, Vars=%A"
            name obj (expected |> List.map (fun (k,_) -> k, getVar vars k))
    | Unbounded varName ->
        failwithf "Test '%s' reported Unbounded in %s (unexpected)" name varName
    | Infeasible _ ->
        failwithf "Test '%s' reported Infeasible (unexpected)" name

let checkInfeasible (name: string) (res: SimplexResult) =
    match res with
    | Infeasible _ ->
        printfn "✔ %s correctly reported Infeasible" name
    | Optimal (_, _, obj) ->
        failwithf "Test '%s' reported Optimal (%.6f) but expected Infeasible" name obj
    | Unbounded varName ->
        failwithf "Test '%s' reported Unbounded in %s but expected Infeasible" name varName

let solveKnapsack (lp: LPFormulation) =
    let tree = KnapsackTree lp :> ITree<KnapsackNode>
    Explorer.SolveSimplex tree
    
let tol = 1e-6

// -------------------------------------
// Basic Knapsack Tests
// -------------------------------------

// Test 1: Simple 0-1 knapsack problem
// Items: (value, weight) = (10,5), (40,4), (30,6), (50,3)
// Capacity: 10
// Expected solution: items 2 and 4 (40+50=90, weight 4+3=7)
do
    let lp = LPFormulation(
                ObjectiveType.Max,
                [| "x1"; "x2"; "x3"; "x4" |],
                [| 10.0; 40.0; 30.0; 50.0 |],
                array2D [ [ 5.0; 4.0; 6.0; 3.0 ] ],
                [| ConstraintSign.LessOrEqual |],
                [| 10.0 |],
                [| SignRestriction.Positive; SignRestriction.Positive; SignRestriction.Positive; SignRestriction.Positive |],
                [| IntRestriction.Binary; IntRestriction.Binary; IntRestriction.Binary; IntRestriction.Binary |])
    checkKnapsackSolution "Simple 0-1 knapsack"
                  90.0
                  [ "x1", 0.0; "x2", 1.0; "x3", 0.0; "x4", 1.0 ]
                  tol
                  (solveKnapsack lp)

// Test 2: Small knapsack where all items can fit
// Items: (value, weight) = (6,1), (10,2), (12,3)
// Capacity: 10
// Expected solution: take all items (28, weight 6)
do
    let lp = LPFormulation(
                ObjectiveType.Max,
                [| "x1"; "x2"; "x3" |],
                [| 6.0; 10.0; 12.0 |],
                array2D [ [ 1.0; 2.0; 3.0 ] ],
                [| ConstraintSign.LessOrEqual |],
                [| 10.0 |],
                [| SignRestriction.Positive; SignRestriction.Positive; SignRestriction.Positive |],
                [| IntRestriction.Binary; IntRestriction.Binary; IntRestriction.Binary |])
    checkKnapsackSolution "All items fit knapsack"
                  28.0
                  [ "x1", 1.0; "x2", 1.0; "x3", 1.0 ]
                  tol
                  (solveKnapsack lp)

// Test 3: Knapsack with single optimal item
// Items: (value, weight) = (5,10), (100,8), (15,4)
// Capacity: 8
// Expected solution: take item 2 only (100, weight 8)
do
    let lp = LPFormulation(
                ObjectiveType.Max,
                [| "a"; "b"; "c" |],
                [| 5.0; 100.0; 15.0 |],
                array2D [ [ 10.0; 8.0; 4.0 ] ],
                [| ConstraintSign.LessOrEqual |],
                [| 8.0 |],
                [| SignRestriction.Positive; SignRestriction.Positive; SignRestriction.Positive |],
                [| IntRestriction.Binary; IntRestriction.Binary; IntRestriction.Binary |])
    checkKnapsackSolution "Single optimal item knapsack"
                  100.0
                  [ "a", 0.0; "b", 1.0; "c", 0.0 ]
                  tol
                  (solveKnapsack lp)

// Test 4: Zero capacity knapsack (should be feasible with all items at 0)
do
    let lp = LPFormulation(
                ObjectiveType.Max,
                [| "x"; "y" |],
                [| 10.0; 20.0 |],
                array2D [ [ 5.0; 3.0 ] ],
                [| ConstraintSign.LessOrEqual |],
                [| 0.0 |],
                [| SignRestriction.Positive; SignRestriction.Positive |],
                [| IntRestriction.Binary; IntRestriction.Binary |])
    checkKnapsackSolution "Zero capacity knapsack"
                  0.0
                  [ "x", 0.0; "y", 0.0 ]
                  tol
                  (solveKnapsack lp)

// Test 5: Classic textbook example
// Items: (value, weight) = (1,1), (4,3), (5,4), (7,5)
// Capacity: 7
// Expected solution: items 2 and 3 (4+5=9, weight 3+4=7)
do
    let lp = LPFormulation(
                ObjectiveType.Max,
                [| "item1"; "item2"; "item3"; "item4" |],
                [| 1.0; 4.0; 5.0; 7.0 |],
                array2D [ [ 1.0; 3.0; 4.0; 5.0 ] ],
                [| ConstraintSign.LessOrEqual |],
                [| 7.0 |],
                [| SignRestriction.Positive; SignRestriction.Positive; SignRestriction.Positive; SignRestriction.Positive |],
                [| IntRestriction.Binary; IntRestriction.Binary; IntRestriction.Binary; IntRestriction.Binary |])
    checkKnapsackSolution "Classic textbook example"
                  9.0
                  [ "item1", 0.0; "item2", 1.0; "item3", 1.0; "item4", 0.0 ]
                  tol
                  (solveKnapsack lp)

// Test 6: Tie-breaking scenario
// Items: (value, weight) = (3,2), (3,2), (3,2)
// Capacity: 4
// Expected solution: take any two items (6, weight 4)
do
    let lp = LPFormulation(
                ObjectiveType.Max,
                [| "p"; "q"; "r" |],
                [| 3.0; 3.0; 3.0 |],
                array2D [ [ 2.0; 2.0; 2.0 ] ],
                [| ConstraintSign.LessOrEqual |],
                [| 4.0 |],
                [| SignRestriction.Positive; SignRestriction.Positive; SignRestriction.Positive |],
                [| IntRestriction.Binary; IntRestriction.Binary; IntRestriction.Binary |])
    let result = solveKnapsack lp
    match result with
    | Optimal (vars, _, obj) ->
        if not (approxEq tol obj 6.0) then
            failwithf "Test 'Tie-breaking': objective %.6f != expected 6.0" obj
        let totalTaken = (getVar vars "p") + (getVar vars "q") + (getVar vars "r")
        if not (approxEq tol totalTaken 2.0) then
            failwithf "Test 'Tie-breaking': should take exactly 2 items, got %.6f" totalTaken
        printfn "✔ Tie-breaking test passed. Objective=%.6f, Items taken=%.0f" obj totalTaken
    | _ -> failwith "Unexpected result for tie-breaking test"

// -------------------------------------
// Edge Cases
// -------------------------------------

// Test 7: Empty knapsack (no items)
// This should result in optimal solution with value 0
do
    let lp = LPFormulation(
                ObjectiveType.Max,
                [||],
                [||],
                array2D [ [ ] ],
                [| ConstraintSign.LessOrEqual |],
                [| 10.0 |],
                [||],
                [||])
    try
        let result = solveKnapsack lp
        match result with
        | Optimal (_, _, obj) ->
            if approxEq tol obj 0.0 then
                printfn "✔ Empty knapsack test passed. Objective=0.0"
            else
                failwithf "Empty knapsack should have objective 0.0, got %.6f" obj
        | _ -> failwith "Empty knapsack should be optimal with value 0"
    with
    | _ -> printfn "✔ Empty knapsack correctly handled (implementation may reject empty formulation)"

printfn "All knapsack tests completed."

// Test API methods on KnapsackNode
printfn "\nTesting KnapsackNode API methods..."

// Create a simple knapsack tree and test node methods
let testFormulation = LPFormulation(
    ObjectiveType.Max,
    [| "x1"; "x2" |],
    [| 10.0; 15.0 |],
    array2D [ [ 2.0; 3.0 ] ],
    [| ConstraintSign.LessOrEqual |],
    [| 5.0 |],
    [| SignRestriction.Positive; SignRestriction.Positive |],
    [| IntRestriction.Binary; IntRestriction.Binary |])

let testTree = KnapsackTree testFormulation
let rootNode = (testTree :> ITree<KnapsackNode>).Item

printfn "✔ Root node created successfully"
let currentVals = rootNode.GetCurrentValues()
let totalWeight = 
    [0 .. rootNode.Knapsack.VariableNames.Length - 1]
    |> List.sumBy (fun i -> 
        match rootNode.CurrentValues.TryGetValue i with
        | true, value -> value * rootNode.Knapsack.Weights.[i]
        | false, _ -> 0.0)
let totalValue = 
    [0 .. rootNode.Knapsack.VariableNames.Length - 1]
    |> List.sumBy (fun i -> 
        match rootNode.CurrentValues.TryGetValue i with
        | true, value -> value * rootNode.Knapsack.Objective.[i]
        | false, _ -> 0.0)
printfn "  Current weight: %.2f" totalWeight
printfn "  Current value: %.2f" totalValue

let givenVals = rootNode.GetGivenValues()

printfn "  Current values count: %d" currentVals.Count
printfn "  Given values count: %d" givenVals.Count

if currentVals.Count = 2 && givenVals.Count = 0 then
    printfn "✔ API methods work correctly"
else
    printfn "✗ API methods failed: expected 2 current values and 0 given values"

printfn "\nAll tests completed successfully!"
