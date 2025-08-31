#r "nuget: MathNet.Numerics"
#load "Formulation.fs"
#load "RevisedSimplex.fs"
#load "BranchAndBound.fs"
#load "Explorer.fs"
#load "ResultAnalysis.fs"

open LPR381.Core

let printDualityResult (result: DualityResult) =
    match result with
    | StrongDuality (primal, dual) ->
        printfn "✔ Strong Duality: Primal=%.6f, Dual=%.6f" primal dual
    | WeakDuality (primal, dual) ->
        printfn "⚠ Weak Duality: Primal=%.6f, Dual=%.6f" primal dual
    | NoDuality reason ->
        printfn "❌ No Duality: %s" reason

let testDuality (name: string) (lp: LPFormulation) =
    try
        printfn "\n=== %s ===" name
        
        // Solve primal
        let primalTree = RevisedDualSimplex lp
        let primalResult = Explorer.SolveSimplex primalTree
        printfn "Primal result: %A" primalResult
        
        // Get dual formulation and solve
        let dualFormulation = DualFormulation(lp).ToLPFormulation()
        let dualTree = RevisedDualSimplex dualFormulation
        let dualResult = Explorer.SolveSimplex dualTree
        printfn "Dual result: %A" dualResult
        
        // Verify duality
        let dualityResult = 
          match primalResult, dualResult with
          | Optimal(_, _, primalObj), Optimal(_, _, dualObj) ->
            let tolerance = 1e-6
            if abs(primalObj - dualObj) < tolerance then
              StrongDuality(primalObj, dualObj)
            else
              WeakDuality(primalObj, dualObj)
          | Optimal(_, _, primalObj), Unbounded _ -> WeakDuality(primalObj, infinity)
          | Unbounded _, Optimal(_, _, dualObj) -> WeakDuality(infinity, dualObj)
          | Infeasible _, Unbounded _ -> NoDuality "Primal infeasible, dual unbounded"
          | Unbounded _, Infeasible _ -> NoDuality "Primal unbounded, dual infeasible"
          | _ -> NoDuality "Both problems infeasible or other error"
        printDualityResult dualityResult
        
    with ex ->
        printfn "Error in test '%s': %s" name ex.Message

// Test 1: Standard maximization problem with strong duality
let lp1 = LPFormulation(
    ObjectiveType.Max,
    [| "x1"; "x2" |],
    [| 3.0; 2.0 |],
    array2D [ [ 1.0; 1.0 ]
              [ 2.0; 1.0 ] ],
    [| ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual |],
    [| 4.0; 6.0 |],
    [| SignRestriction.Positive; SignRestriction.Positive |],
    [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |])

// Test 2: Minimization problem
let lp2 = LPFormulation(
    ObjectiveType.Min,
    [| "x"; "y" |],
    [| 2.0; 3.0 |],
    array2D [ [ 1.0; 2.0 ]
              [ 3.0; 1.0 ] ],
    [| ConstraintSign.GreaterOrEqual; ConstraintSign.GreaterOrEqual |],
    [| 5.0; 4.0 |],
    [| SignRestriction.Positive; SignRestriction.Positive |],
    [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |])

testDuality "Max 3x1+2x2 standard form" lp1
testDuality "Min 2x+3y with >= constraints" lp2

printfn "\nDuality analysis tests completed."
