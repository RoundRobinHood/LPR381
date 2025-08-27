#r "nuget: MathNet.Numerics"
#load "Formulation.fs"

open MathNet.Numerics.LinearAlgebra
open LPR381.Core

// ----------------------------
// Edge-case showcase
// ----------------------------
let lpEdge =
  LPFormulation(
    ObjectiveType.Max,
    [| "xPos"; "xNeg"; "xUrs"; "xBin"; "xInt" |],
    [| 5.0; -2.0; 3.0; 1.0; 4.0 |],       // Arbitrary coefficients
    array2D [ [  1.0; -1.0;  2.0;  1.0;  0.0 ]   // <= constraint
              [ -2.0;  3.0; -1.0;  0.0;  1.0 ]   // >= constraint
              [  0.0;  1.0;  1.0;  1.0; -1.0 ] ],// = constraint
    [| ConstraintSign.LessOrEqual
       ConstraintSign.GreaterOrEqual
       ConstraintSign.Equal |],
    [| 10.0; 5.0; 0.0 |],                  // RHS values
    [| SignRestriction.Positive            // xPos ≥ 0
       SignRestriction.Negative            // xNeg ≤ 0
       SignRestriction.Unrestricted        // xUrs free
       SignRestriction.Positive            // xBin ≥ 0
       SignRestriction.Positive |],        // xInt ≥ 0
    [| IntRestriction.Unrestricted         // xPos is continuous
       IntRestriction.Unrestricted         // xNeg continuous
       IntRestriction.Unrestricted         // xUrs continuous
       IntRestriction.Binary               // xBin ∈ {0,1}
       IntRestriction.Integer |]           // xInt integer
  )

// ----------------------------
// Convert to canonical
// ----------------------------
let canonEdge = lpEdge.ToLPCanonical()

// ----------------------------
// Print results
// ----------------------------
let printCanonical (name: string) (c: LPCanonical) =
    printfn "---- %s ----" name
    printfn "Objective: %A" c.Objective
    printfn "Constraint matrix:\n%A" c.ConstraintMatrix
    printfn "RHS: %A" c.RHS
    printfn "Vars: %A" c.VariableNames
    printfn "Int restrictions: %A\n" c.VarIntRestrictions

printCanonical "EdgeCase LP" canonEdge

