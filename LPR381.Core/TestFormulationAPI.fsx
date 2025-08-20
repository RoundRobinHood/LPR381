// Load MathNet.Numerics
#r "nuget: MathNet.Numerics"

open System
open MathNet.Numerics.LinearAlgebra

// Load your formulation file
#load "Formulation.fs"

open LPR381.Core

// --- Example LP setup ---
let objectiveStr = "3x - 2y + z"
let constraintsStr = [|
    ("<=", 5.0, "1x + 2y - z")
    (">=", 2.0, "2x - y + 3z")
    ("=", 4.0, "x + y + z")
|]

// Parse objective
let mutable objResult = Unchecked.defaultof<LPObjective>
let mutable objError = ""
if not (LPObjective.TryParse(ObjectiveType.Max, objectiveStr, &objResult, &objError)) then
    failwithf "Objective parsing failed: %s" objError

// Parse constraints
let constraints =
    constraintsStr
    |> Array.map (fun (sign,right,expr) ->
        let mutable c = Unchecked.defaultof<LPConstraint>
        let mutable err = ""
        if not (LPConstraint.TryParse(sign, right, expr, &c, &err)) then
            failwithf "Constraint parsing failed: %s" err
        c
    )

// Sign restrictions and integer restrictions for variables
let signRestrictions = [| SignRestriction.Positive; SignRestriction.Negative; SignRestriction.Unrestricted |]
let intRestrictions = [| IntRestriction.Unrestricted; IntRestriction.Integer; IntRestriction.Binary |]

// --- Build LP using new constructor ---
let lpFromParsing = LPFormulation(objResult, constraints, signRestrictions, intRestrictions)

// --- Build LP manually ---
let manualVarNames = [| "x"; "y"; "z" |]
let manualObjective = [| 3.0; -2.0; 1.0 |]
let manualConstraintCoeffs = array2D [|
    [|1.0; 2.0; -1.0|]
    [| 2.0; -1.0; 3.0 |] // note: depends on canonicalization
    [|1.0; 1.0; 1.0|]
|]
let manualConstraintSigns = [| ConstraintSign.LessOrEqual; ConstraintSign.GreaterOrEqual; ConstraintSign.Equal |]
let manualRHS = [| 5.0; 2.0; 4.0 |]

let lpManual = LPFormulation(
    ObjectiveType.Max,
    manualVarNames,
    manualObjective,
    manualConstraintCoeffs,
    manualConstraintSigns,
    manualRHS,
    signRestrictions,
    intRestrictions
)

// --- Simple checker function ---
let checkArrays name arr1 arr2 =
    if arr1 = arr2 then
        printfn "%s match ✔" name
    else
        printfn "%s mismatch ❌" name
        printfn "  Expected: %A" arr2
        printfn "  Got:      %A" arr1

printfn "--- Checking LP construction ---"
checkArrays "Variable names" lpFromParsing.VarNames lpManual.VarNames
checkArrays "Objective values" lpFromParsing.Objective lpManual.Objective
checkArrays "Constraint signs" lpFromParsing.ConstraintSigns lpManual.ConstraintSigns
checkArrays "RHS" lpFromParsing.RHS lpManual.RHS

printfn "Constraint coefficients (manual vs parsing):"
printfn "Manual:\n%A" lpManual.ConstraintCoefficients
printfn "Parsing:\n%A" lpFromParsing.ConstraintCoefficients

printfn "Finished LP construction test."
