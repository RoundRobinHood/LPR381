#r "nuget: MathNet.Numerics"
#load "Formulation.fs"
open System
open LPR381.Core

// Helper to test a single input
let testLinearSum (input: string) (expected: (double * string)[]) =
    try
        let result = Parsing.linearSum input
        let success =
            result.Length = expected.Length &&
            Array.forall2 (fun (c1,s1) (c2,s2) -> c1 = c2 && s1 = s2) result expected
        if success then
            printfn "✔ Passed: '%s' -> %A" input result
        else
            printfn "❌ Failed: '%s'" input
            printfn "   Got:      %A" result
            printfn "   Expected: %A" expected
    with ex ->
        printfn "❌ Exception for input '%s': %s" input ex.Message

// --- Test cases ---
let tests = [
    "3x + 2y - z", [| (3.0,"x"); (2.0,"y"); (-1.0,"z") |]
    "-x + 4y", [| (-1.0,"x"); (4.0,"y") |]
    "+ -2x + y", [| (-2.0,"x"); (1.0,"y") |]
    "5.5a -3.2b + c1", [| (5.5,"a"); (-3.2,"b"); (1.0,"c1") |]
    "x+y+z", [| (1.0,"x"); (1.0,"y"); (1.0,"z") |]
    "-0.5x + -1.25y", [| (-0.5,"x"); (-1.25,"y") |]
    "   7x   +2y -  z", [| (7.0,"x"); (2.0,"y"); (-1.0,"z") |]
    "x", [| (1.0,"x") |]
    "-x", [| (-1.0,"x") |]
]

for (input, expected) in tests do
    testLinearSum input expected

printfn "Finished linearSum tests."

