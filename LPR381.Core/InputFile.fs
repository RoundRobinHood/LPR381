namespace LPR381.Core

open System
open System.IO

module InputFile =
  let TryInterpretLines (input: string[]) (result: byref<LPFormulation>) (error: byref<string>)=
    try
      if input.Length < 2 then failwith "File must have at least objective line and sign restriction line"
      // Read objective line
      let split = input.[0].Split ' '
      let objectiveType = 
        match split.[0] with
        | "max" -> ObjectiveType.Max
        | "min" -> ObjectiveType.Min
        | s -> failwithf "Unexpected objective type '%s'" s

      let parseCoeff (s: string)=
        if s.Length < 2 then failwithf "Invalid coeff string: '%s'" s
        let sign: Double = 
          match s.[0] with
          | '+' -> 1.0
          | '-' -> -1.0
          | c -> failwithf "Unexpected sign '%c'" c

        sign * Double.Parse s.[1..]

      let objective = split.[1..] |> Array.map parseCoeff

      let rec getConstraints (lineNum: int) (constraintCoeff: double array list) (constraintSigns: ConstraintSign list) (rhs: double list) =
        if lineNum >= input.Length then failwith "Missing sign restriction line"
        if String.IsNullOrWhiteSpace input.[lineNum] then getConstraints (lineNum+1) constraintCoeff constraintSigns rhs
        else
          let line = input.[lineNum]
          if line.StartsWith "+" || line.StartsWith "-" then
            let split = line.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            if split.[0].Length = 1 then lineNum, constraintCoeff, constraintSigns, rhs else
            
            // Parse constraint by finding the sign
            let constraintSign, _rhs, coeffCount =
              // Check if last two tokens are sign + number (e.g., ">= 4")
              if split.Length >= 2 && (split.[split.Length - 2] = "<=" || split.[split.Length - 2] = ">=" || split.[split.Length - 2] = "=") then
                let sign = match split.[split.Length - 2] with
                           | "<=" -> ConstraintSign.LessOrEqual
                           | ">=" -> ConstraintSign.GreaterOrEqual  
                           | "=" -> ConstraintSign.Equal
                           | _ -> failwith "Invalid sign"
                sign, Double.Parse split.[split.Length - 1], split.Length - 2
              else
                // Check if last token contains sign + number (e.g., ">=4")
                let last = split.[split.Length - 1]
                if last.StartsWith "<=" then ConstraintSign.LessOrEqual, Double.Parse last.[2..], split.Length - 1
                elif last.StartsWith ">=" then ConstraintSign.GreaterOrEqual, Double.Parse last.[2..], split.Length - 1
                elif last.StartsWith "=" then ConstraintSign.Equal, Double.Parse last.[1..], split.Length - 1
                else failwithf "Invalid constraint format: '%s'" last
            
            let coeff = split.[.. coeffCount - 1] |> Array.map parseCoeff

            getConstraints (lineNum + 1) (coeff :: constraintCoeff) (constraintSign :: constraintSigns) (_rhs :: rhs)
          else 
            lineNum, constraintCoeff, constraintSigns, rhs

      let signRestrictionRow, constraintCoeff, constraintSigns, rhs = getConstraints 1 [] [] []

      let signRestrictions, intRestrictions = input.[signRestrictionRow].Split ' ' |> Array.map (fun x ->
          match x with
          | "+" -> SignRestriction.Positive, IntRestriction.Unrestricted
          | "-" -> SignRestriction.Negative, IntRestriction.Unrestricted
          | "urs" -> SignRestriction.Unrestricted, IntRestriction.Unrestricted
          | "int" -> SignRestriction.Positive, IntRestriction.Integer
          | "bin" -> SignRestriction.Positive, IntRestriction.Binary
          | s -> failwithf "Invalid sign restriction: '%s'" s) |> Array.unzip

      result <- LPFormulation(objectiveType, [| 1 .. objective.Length |] |> Array.map (fun x -> sprintf "x%d" x), objective, array2D constraintCoeff, List.toArray constraintSigns, List.toArray rhs, signRestrictions, intRestrictions)

      true
    with ex ->
      error <- ex.Message
      false
  
  let TryInterpretText (text: string) (result: byref<LPFormulation>) (error: byref<string>) =
    let lines =
      text.Split(
        [|"\r\n"; "\n"|], 
        StringSplitOptions.None
      )
    TryInterpretLines lines &result &error

  let TryInterpretFile (path: string) (result: byref<LPFormulation>) (error: byref<string>) =
    let lines = File.ReadAllLines path
    TryInterpretLines lines &result &error
