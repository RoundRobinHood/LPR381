#r "nuget: MathNet.Numerics"
#load "Formulation.fs"
#load "RevisedSimplex.fs"
#load "BranchAndBound.fs"
#load "Explorer.fs"
#load "ResultAnalysis.fs"

open LPR381.Core

// Example: Max 3x + 2y subject to x + y <= 4, 2x + y <= 6, x,y >= 0
let formulation = LPFormulation(
    ObjectiveType.Max,
    [| "x"; "y" |],
    [| 3.0; 2.0 |],
    array2D [ [ 1.0; 1.0 ]
              [ 2.0; 1.0 ] ],
    [| ConstraintSign.LessOrEqual; ConstraintSign.LessOrEqual |],
    [| 4.0; 6.0 |],
    [| SignRestriction.Positive; SignRestriction.Positive |],
    [| IntRestriction.Unrestricted; IntRestriction.Unrestricted |])

// Perform complete sensitivity analysis
let (primalResult, dualResult, dualityResult) = Explorer.PerformSensitivityAnalysis formulation

printfn "=== SENSITIVITY ANALYSIS RESULTS ==="
printfn "\nPrimal Problem Result:"
printfn "%A" primalResult

printfn "\nDual Problem Result:"
printfn "%A" dualResult

printfn "\nDuality Verification:"
match dualityResult with
| StrongDuality (p, d) -> printfn "Strong Duality: Primal=%.6f, Dual=%.6f" p d
| WeakDuality (p, d) -> printfn "Weak Duality: Primal=%.6f, Dual=%.6f" p d
| NoDuality reason -> printfn "No Duality: %s" reason
