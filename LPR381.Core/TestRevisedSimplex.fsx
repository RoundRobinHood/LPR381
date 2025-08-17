// TestRevisedSimplex.fsx
#r "nuget: MathNet.Numerics"
#r "nuget: MathNet.Numerics.FSharp"

#load "Formulation.fs"
#load "RevisedSimplex.fs"

open System
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra
open LPR381.Core

let (|~|) (a: float) (b: float) = abs (a - b) // small helper

let approxEq tol (a: float) (b: float) = abs (a - b) <= tol

let getVar (vars: Dictionary<string, double>) (name: string) =
    match vars.TryGetValue name with
    | true, v -> v
    | _ -> 0.0  // slack and extras exist; we only care about original vars

let checkSolution
    (name: string)
    (expectedObj: float)
    (expected: (string * float) list)
    (tol: float)
    (res: SimplexResult) =
    match res with
    | Optimal (vars, obj) ->
        // Check objective
        if not (approxEq tol obj expectedObj) then
            failwithf "Test '%s' failed: objective %.6f != expected %.6f" name obj expectedObj

        // Check variables (only for the named originals)
        expected
        |> List.iter (fun (n, vexp) ->
            let v = getVar vars n
            if not (approxEq tol v vexp) then
                failwithf "Test '%s' failed: %s = %.6f != expected %.6f" name n v vexp)

        printfn "✔ %s passed. Objective=%.6f, Vars=%A"
            name obj (expected |> List.map (fun (k,_) -> k, getVar vars k))

    | Unbounded varName ->
        failwithf "Test '%s' reported Unbounded in direction of %s (unexpected for this test)" name varName
    | Infeasible _ ->
        failwithf "Test '%s' reported Infeasible (unexpected for this test)" name

// Shorthands
let mkVec (xs: float[]) = Vector<double>.Build.DenseOfArray xs
let mkMat (xs: double[,]) = Matrix<double>.Build.DenseOfArray xs

let mkLP
    (objectiveType: ObjectiveType)
    (varNames: string[])
    (objective: float[])
    (A: double[,])            // each row is one constraint
    (rhs: float[])
    =
    // All constraints are <= for these tests
    let m = rhs.Length
    let signs = Array.init m (fun _ -> ConstraintSign.LessOrEqual)
    // All variables are x >= 0
    let varSigns = Array.init varNames.Length (fun _ -> SignRestriction.Positive)
    // No integrality in these tests
    let varInts  = Array.init varNames.Length (fun _ -> IntRestriction.Unrestricted)

    LPFormulation(
        objectiveType,
        varNames,
        objective,
        A,
        signs,
        rhs,
        varSigns,
        varInts
    )

let solve (lp: LPFormulation) =
    let canon = lp.ToLPCanonical()
    RevisedSimplex.SolvePrimal canon

// -------------------------------------
// Test suite (≤ constraints only)
// -------------------------------------

let tol = 1e-6

// Test 1: Max 3x + 2y
// s.t. x + y ≤ 4, x ≤ 2, y ≤ 3, x,y ≥ 0
// Expected optimum at (x,y) = (2,2), z = 10
do
    let lp =
        mkLP ObjectiveType.Max
             [| "x"; "y" |]
             [| 3.0; 2.0 |]
             (array2D [ [ 1.0; 1.0 ]
                        [ 1.0; 0.0 ]
                        [ 0.0; 1.0 ] ])
             [| 4.0; 2.0; 3.0 |]

    let r = solve lp
    checkSolution "Max 3x+2y under basic bounds"
                  10.0
                  [ "x", 2.0; "y", 2.0 ]
                  tol
                  r

// Test 2: Max x + y
// s.t. 2x + y ≤ 6, x + 2y ≤ 8, x,y ≥ 0
// Intersection gives (x,y) = (4/3, 10/3), z = 14/3
do
    let lp =
        mkLP ObjectiveType.Max
             [| "x"; "y" |]
             [| 1.0; 1.0 |]
             (array2D [ [ 2.0; 1.0 ]
                        [ 1.0; 2.0 ] ])
             [| 6.0; 8.0 |]

    let r = solve lp
    checkSolution "Max x+y with two constraints"
                  (14.0/3.0)
                  [ "x", (4.0/3.0); "y", (10.0/3.0) ]
                  tol
                  r

// Test 3: Min x + 2y
// s.t. x + y ≤ 4, x ≤ 3, y ≤ 3, x,y ≥ 0
// Minimum at (0,0), z = 0
do
    let lp =
        mkLP ObjectiveType.Min
             [| "x"; "y" |]
             [| 1.0; 2.0 |]
             (array2D [ [ 1.0; 1.0 ]
                        [ 1.0; 0.0 ]
                        [ 0.0; 1.0 ] ])
             [| 4.0; 3.0; 3.0 |]

    let r = solve lp
    checkSolution "Min x+2y under simple bounds"
                  0.0
                  [ "x", 0.0; "y", 0.0 ]
                  tol
                  r

// Test 4: Max 3x + 5y
// s.t. x ≤ 4, y ≤ 6, 3x + 2y ≤ 18, x,y ≥ 0
// Known optimum at (2,6), z = 36
do
    let lp =
        mkLP ObjectiveType.Max
             [| "x"; "y" |]
             [| 3.0; 5.0 |]
             (array2D [ [ 1.0; 0.0 ]   // x ≤ 4
                        [ 0.0; 1.0 ]   // y ≤ 6
                        [ 3.0; 2.0 ] ])// 3x + 2y ≤ 18
             [| 4.0; 6.0; 18.0 |]

    let r = solve lp
    checkSolution "Max 3x+5y classic"
                  36.0
                  [ "x", 2.0; "y", 6.0 ]
                  tol
                  r

printfn "All tests completed."

