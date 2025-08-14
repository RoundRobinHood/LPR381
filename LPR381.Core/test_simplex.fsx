#r "nuget: MathNet.Numerics, 5.0.0"
#load "Formulation.fs"
#load "PrimalSimplex.fs"

open LPR381.Core
open LPR381.Core.LpCanonicalBuilder

// Oakfield example (no manual s/e columns needed)
let can =
    canonicalFrom ObjectiveType.Max
        [| 8.0; 5.0 |]
        [| le 6.0  [| 1.0; 1.0 |]
           le 45.0 [| 9.0; 5.0 |] |]

//let can =
//    canonicalFrom ObjectiveType.Max
//        [| 2.0; 3.0; 3.0; 5.0; 2.0; 4.0 |]
//        [| le 40  [| 11.0; 8.0; 6.0; 14.0; 10.0; 10.0 |]
//           le 1  [| 1.0; 0; 0; 0; 0; 0 |]
//           le 1  [| 0; 1.0; 0; 0; 0; 0 |]
//           le 1  [| 0; 0; 1.0; 0; 0; 0 |]
//           le 1  [| 0; 0; 0; 1.0; 0; 0 |]
//           le 1  [| 0; 0; 0; 0; 1.0; 0 |]
//           le 1  [| 0; 0; 0; 0; 0; 1.0 |]|]

let tbl0 = PrimalSimplex.initializeFromCanonical can
let res  = PrimalSimplex.solveWithLogging tbl0 PrimalSimplex.Pretty.printFullTable

printfn "Status: %A" res.Status
printfn "z* = %.3f" res.ObjectiveValue
res.VariableValues |> Array.iter (fun (n,v) -> printfn "%s = %.3f" n v)
