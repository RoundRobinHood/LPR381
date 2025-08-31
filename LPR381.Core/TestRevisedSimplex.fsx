// TestRevisedSimplex.fsx
#r "nuget: MathNet.Numerics"
#r "nuget: MathNet.Numerics.FSharp"

#load "Formulation.fs"
#load "ResultAnalysis.fs"
#load "ResultInterfaces.fs"
#load "PrimalSimplex.fs"
#load "RevisedSimplex.fs"
#load "BranchAndBound.fs"
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

let checkSolution
    (name: string)
    (expectedObj: float)
    (expected: (string * float) list)
    (tol: float)
    (res: SimplexResult) =
    match res with
    | Optimal (vars, _, obj) ->
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

let solve (lp: LPFormulation) =
    let tree = RevisedDualSimplex lp
    Explorer.SolveSimplex tree
    
let tol = 1e-6

// -------------------------------------
// <= constraint tests
// -------------------------------------

// Test 1: Max 3x+2y, s.t. x+y≤4, x≤2, y≤3, x,y≥0
do
    let lp = LPFormulation(
                ObjectiveType.Max,
                [| "x"; "y" |],
                [| 3.0; 2.0 |],
                array2D [ [ 1.0; 1.0 ]
                          [ 1.0; 0.0 ]
                          [ 0.0; 1.0 ] ],
                [| ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual |],
                [| 4.0; 2.0; 3.0 |],
                [| SignRestriction.Positive; SignRestriction.Positive |],
                [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |])
    checkSolution "Max 3x+2y under basic bounds"
                  10.0
                  [ "x", 2.0; "y", 2.0 ]
                  tol
                  (solve lp)

// Test 2: Max x+y, s.t. 2x+y≤6, x+2y≤8
do
    let lp = LPFormulation(
                ObjectiveType.Max,
                [| "x"; "y" |],
                [| 1.0; 1.0 |],
                array2D [ [ 2.0; 1.0 ]
                          [ 1.0; 2.0 ] ],
                [| ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual |],
                [| 6.0; 8.0 |],
                [| SignRestriction.Positive; SignRestriction.Positive |],
                [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |])
    checkSolution "Max x+y with two constraints"
                  (14.0/3.0)
                  [ "x", (4.0/3.0); "y", (10.0/3.0) ]
                  tol
                  (solve lp)

// Test 3: Min x+2y, s.t. x+y≤4, x≤3, y≤3
do
    let lp = LPFormulation(
                ObjectiveType.Min,
                [| "x"; "y" |],
                [| 1.0; 2.0 |],
                array2D [ [ 1.0; 1.0 ]
                          [ 1.0; 0.0 ]
                          [ 0.0; 1.0 ] ],
                [| ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual |],
                [| 4.0; 3.0; 3.0 |],
                [| SignRestriction.Positive; SignRestriction.Positive |],
                [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |])
    checkSolution "Min x+2y under simple bounds"
                  0.0
                  [ "x", 0.0; "y", 0.0 ]
                  tol
                  (solve lp)

// Test 4: Max 3x+5y, s.t. x≤4, y≤6, 3x+2y≤18
do
    let lp = LPFormulation(
                ObjectiveType.Max,
                [| "x"; "y" |],
                [| 3.0; 5.0 |],
                array2D [ [ 1.0; 0.0 ]
                          [ 0.0; 1.0 ]
                          [ 3.0; 2.0 ] ],
                [| ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual |],
                [| 4.0; 6.0; 18.0 |],
                [| SignRestriction.Positive; SignRestriction.Positive |],
                [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |])
    checkSolution "Max 3x+5y classic"
                  36.0
                  [ "x", 2.0; "y", 6.0 ]
                  tol
                  (solve lp)


// -------------------------------------
// >= constraint tests (to stress dual simplex)
// -------------------------------------

// Test 5: Min x+y, s.t. x+y ≥ 5, x,y ≥ 0
// Obvious solution: (5,0) or (0,5), z=5
do
    let lp = LPFormulation(
                ObjectiveType.Min,
                [| "x"; "y" |],
                [| 1.0; 1.0 |],
                array2D [ [ 1.0; 1.0 ] ],
                [| ConstraintSign.GreaterOrEqual |],
                [| 5.0 |],
                [| SignRestriction.Positive; SignRestriction.Positive |],
                [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |])
    let r = solve lp
    // Accept either extreme as optimal
    match r with
    | Optimal (vars, _, obj) ->
        if not (approxEq tol obj 5.0) then failwith "Test 'Min x+y, x+y≥5' wrong objective"
        let x = getVar vars "x"
        let y = getVar vars "y"
        if not (approxEq tol (x+y) 5.0) then failwith "Test 'Min x+y, x+y≥5' wrong vars"
        printfn "✔ Min x+y with ≥ constraint passed."
    | _ -> failwith "Unexpected result for ≥ test"

// Test 6: Max x, s.t. x ≥ 3, y ≥ 2, and x+ y ≤ 10
// Optimum at (x,y) = (8,2), z=8
do
    let lp = LPFormulation(
                ObjectiveType.Max,
                [| "x"; "y" |],
                [| 1.0; 0.0 |],
                array2D [ [ 1.0; 0.0 ]
                          [ 0.0; 1.0 ]
                          [ 1.0; 1.0 ] ],
                [| ConstraintSign.GreaterOrEqual; ConstraintSign.GreaterOrEqual; ConstraintSign.LessOrEqual |],
                [| 3.0; 2.0; 10.0 |],
                [| SignRestriction.Positive; SignRestriction.Positive |],
                [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |])
    checkSolution "Max x with mixed ≥ and ≤"
                  8.0
                  [ "x", 8.0; "y", 2.0 ]
                  tol
                  (solve lp)

printfn "All tests completed."

