// TestRevisedBNB.fsx
#r "nuget: MathNet.Numerics"
#r "nuget: MathNet.Numerics.FSharp"

// adjust paths if your files live in a different folder
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

let printSimplexResult (resOpt: SimplexResult) =
    match resOpt with
    | Optimal (canonVars, formVars, obj) ->
        printfn "Optimal objective = %g" obj
        printfn "  canonical vars: %A" (canonVars |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toArray)
        printfn "  formulation vars: %A" (formVars |> Seq.map (fun kv -> kv.Key, kv.Value) |> Seq.toArray)
    | Unbounded varName ->
        printfn "Unbounded (variable: %s)" varName
    | Infeasible constraintIdx ->
        printfn "Infeasible (stopping constraint index: %d)" constraintIdx

// small helper to run and catch exceptions
let runBNB (name: string) (lp: LPFormulation) =
    try
        printfn "=== Running test: %s ===" name
        // construct RevisedBranchAndBound using the provided constructor
        let bnb = RevisedBranchAndBound lp

        let result = Explorer.SolveSimplex bnb
        printSimplexResult result
    with ex ->
        printfn "Test '%s' raised exception: %s" name ex.Message

// -------------------------------
// Test cases
// -------------------------------

// 1) Simple feasible LP (<= constraints). Maximize 3x + 2y
let lp1 =
    LPFormulation(
        ObjectiveType.Max,
        [| "x"; "y" |],
        [| 3.0; 2.0 |],
        array2D [ [ 1.0; 1.0 ]
                  [ 1.0; 0.0 ]
                  [ 0.0; 1.0 ] ],
        [| ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual |],
        [| 4.0; 2.0; 3.0 |],
        [| SignRestriction.Positive; SignRestriction.Positive |],
        [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |]
    )

// 2) Infeasible LP: x + y <= 2 and x + y >= 5
let lp2 =
    LPFormulation(
        ObjectiveType.Max,
        [| "x"; "y" |],
        [| 1.0; 1.0 |],
        array2D [ [ 1.0; 1.0 ]
                  [ 1.0; 1.0 ] ],
        [| ConstraintSign.LessOrEqual; ConstraintSign.GreaterOrEqual |],
        [| 2.0; 5.0 |],
        [| SignRestriction.Positive; SignRestriction.Positive |],
        [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |]
    )

// 3) Unbounded LP: Maximize x - subject to x - y >= 0, y >= 0 (x can go to +inf)
let lp3 =
    LPFormulation(
        ObjectiveType.Max,
        [| "x"; "y" |],
        [| 1.0; 0.0 |], // maximize x
        array2D [ [ 1.0; -1.0 ]
                  [ 0.0; 1.0 ] ],
        [| ConstraintSign.GreaterOrEqual; ConstraintSign.GreaterOrEqual |],
        [| 0.0; 0.0 |],
        [| SignRestriction.Positive; SignRestriction.Positive |],
        [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |]
    )

// 4) Small MIP: maximize x + y, x,y binary, with x + y <= 1 (i.e., one of them at most)
let lp4 =
    // Build via LPObjective / LPConstraint constructor to exercise that constructor path
    let obj = LPObjective(ObjectiveType.Max, [| (1.0, "x"); (1.0, "y") |])
    let c1 = LPConstraint([| (1.0, "x"); (1.0, "y") |], ConstraintSign.LessOrEqual, 1.0)
    // when creating LPFormulation from LPObjective + LPConstraint[], we get variable ordering via bordaCount
    let signRestrictions = [| SignRestriction.Positive; SignRestriction.Positive |]
    let intRestrictions = [| IntRestriction.Binary; IntRestriction.Binary |]
    LPFormulation(obj, [| c1 |], signRestrictions, intRestrictions)

// 5) Mixed constraints: test >= with minimization. Minimize x + y s.t. x + y >= 5 (optimal 5)
let lp5 =
    LPFormulation(
        ObjectiveType.Min,
        [| "x"; "y" |],
        [| 1.0; 1.0 |],
        array2D [ [ 1.0; 1.0 ] ],
        [| ConstraintSign.GreaterOrEqual |],
        [| 5.0 |],
        [| SignRestriction.Positive; SignRestriction.Positive |],
        [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |]
    )

// 6) Another feasible continuous example from your earlier tests: Max 3x+5y s.t. x≤4, y≤6, 3x+2y≤18
let lp6 =
    LPFormulation(
        ObjectiveType.Max,
        [| "x"; "y" |],
        [| 3.0; 5.0 |],
        array2D [ [ 1.0; 0.0 ]
                  [ 0.0; 1.0 ]
                  [ 3.0; 2.0 ] ],
        [| ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual |],
        [| 4.0; 6.0; 18.0 |],
        [| SignRestriction.Positive; SignRestriction.Positive |],
        [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |]
    )

// List of tests (name, LP)
let tests : (string * LPFormulation)[] =
    [|
        "Feasible Max 3x+2y (basic bounds)", lp1
        "Infeasible x+y <=2 and x+y >=5", lp2
        "Unbounded maximize x", lp3
        "Binary MIP: x,y binary, x+y<=1", lp4
        "Minimize x+y subject to x+y >= 5", lp5
        "Feasible Max 3x+5y classic", lp6
    |]

// Run them all
for (name, lp) in tests do
    runBNB name lp

printfn "All BNB tests finished."

