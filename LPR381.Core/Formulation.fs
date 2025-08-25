namespace LPR381.Core

open System
open System.Globalization
open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra

type ObjectiveType =
  | Min = 0
  | Max = 1

type ConstraintSign =
  | LessOrEqual = 0
  | Equal = 1
  | GreaterOrEqual = 2

type SignRestriction =
  | Positive = 0
  | Unrestricted = 1
  | Negative = 2

type IntRestriction =
  | Integer = 0
  | Binary = 1
  | Unrestricted = 2

module Parsing=
  let linearSum (input: string): (double * string) array =
    let skipWhitespace (i: int) =
      if i >= input.Length then i else i + (input.[i..] |> Seq.takeWhile Char.IsWhiteSpace |> Seq.length)

    let rec parseTerms (i: int) = 
      let i = skipWhitespace i
      if i >= input.Length then [] else

      if input.[i] <> '+' && input.[i] <> '-' && Char.IsLetterOrDigit input.[i] |> not then
        failwithf "Invalid character: '%c'" input.[i]

      let sign = if input.[i] = '-' then -1.0 else 1.0
      let operator = if input.[i] = '-' || input.[i] = '+' then i else -1
      let i = if input.[i] = '+' || input.[i] = '-' then i + 1 else i
      if i >= input.Length then 
        if input.[i-1] = '+' || input.[i-1] = '-' then failwith "Missing variable name" 
        else [] 
      else

      let i = skipWhitespace i
      if i >= input.Length then if operator <> -1 then failwithf "Missing term after '%c'" input.[operator] else [] 
      else

      let coefficientStr = input.[i..] |> Seq.takeWhile (fun c -> Char.IsDigit c || c = '.' || c = '-') |> Seq.toArray |> String

      let coefficient = if coefficientStr.Length = 0 then sign else sign * Double.Parse(coefficientStr, CultureInfo.InvariantCulture)

      let i = skipWhitespace (i + coefficientStr.Length)
      if i >= input.Length then failwith "Missing variable name"

      let variableName = input.[i..] |> Seq.takeWhile (fun c -> Char.IsLetterOrDigit c || c = '_') |> Seq.toArray |> String
      if variableName.Length = 0 then failwith "Missing variable name"

      (coefficient, variableName) :: parseTerms (i + variableName.Length)

    parseTerms 0 |> List.toArray

  let tryLinearSum (input: string, result: byref<(double * string) array>, error: byref<string>)=
    try
      result <- linearSum input
      error <- ""
      true
    with ex ->
      result <- [||]
      error <- ex.Message
      false

  let bordaCount (rankings: string array array) =
    // Collect the union of all items across all rankings
    let allItems =
      rankings
      |> Seq.collect id
      |> Seq.distinct
      |> Seq.toList

    let scores = Dictionary<string, int>()
    allItems |> List.iter (fun item -> scores.[item] <- 0)

    for ranking in rankings do
      let n = ranking.Length
      ranking
      |> Array.iteri (fun i item ->
        let points = n - i - 1
        scores.[item] <- scores.[item] + points
      )

    scores
    |> Seq.sortByDescending (fun kv -> kv.Value)
    |> Seq.map (fun kv -> kv.Key)
    |> Seq.toArray

type LPObjective(
  objectiveType: ObjectiveType,
  linearSum: (double * string) array
)=
  member val ObjectiveType = objectiveType
  member val LinearSum = linearSum

  static member TryParse(objectiveType: ObjectiveType, input: string, result: byref<LPObjective>, error: byref<string>)=
    try
      result <- LPObjective(objectiveType, Parsing.linearSum input)
      error <- ""
      true
    with ex ->
      result <- LPObjective(objectiveType, [||])
      error <- ex.Message
      false

type LPConstraint(
  leftSide: (double * string) array,
  constraintSign: ConstraintSign,
  rightSide: double
)=
  member val LeftSide = leftSide
  member val ConstraintSign = constraintSign
  member val RightSide = rightSide

  static member TryParse(constraintSign: ConstraintSign, rightSide: double, leftSide: string, result: byref<LPConstraint>, error: byref<string>)=
    try
      result <- LPConstraint(Parsing.linearSum leftSide, constraintSign, rightSide)
      error <- ""
      true
    with ex ->
      result <- LPConstraint([||], constraintSign, rightSide)
      error <- ex.Message
      false

  static member TryParse(constraintSign: string, rightSide: double, leftSide: string, result: byref<LPConstraint>, error: byref<string>)=
    match constraintSign with
    | ">=" -> LPConstraint.TryParse(ConstraintSign.GreaterOrEqual, rightSide, leftSide, &result, &error)
    | "<=" -> LPConstraint.TryParse(ConstraintSign.LessOrEqual, rightSide, leftSide, &result, &error)
    | "=" -> LPConstraint.TryParse(ConstraintSign.Equal, rightSide, leftSide, &result, &error)
    | s ->
      result <- LPConstraint([||], ConstraintSign.LessOrEqual, rightSide)
      error <- sprintf "Invalid constraint sign: '%s'" s
      false


type LPCanonical(
  objectiveType: ObjectiveType,
  objective: Vector<double>,
  constraintCoefficients: Matrix<double>,
  rhs: Vector<double>,
  variableNames: string[],
  varIntRestrictions: IntRestriction[]
) =
  member val ObjectiveType = objectiveType
  member val Objective = objective
  member val ConstraintMatrix = constraintCoefficients
  member val RHS = rhs
  member val VariableNames = variableNames
  member val VarIntRestrictions = varIntRestrictions

type LPFormulation(
  objectiveType: ObjectiveType,
  varNames: string[],
  objective: double[],
  constraintCoefficients: double[,],
  constraintSigns: ConstraintSign[],
  rhs: double[],
  varSignRestrictions: SignRestriction[],
  varIntRestrictions: IntRestriction[]
) =
  member val ObjectiveType = objectiveType
  member val VarNames = varNames
  member val Objective = objective
  member val ConstraintCoefficients = constraintCoefficients
  member val ConstraintSigns = constraintSigns
  member val RHS = rhs
  member val VarSignRestrictions = varSignRestrictions
  member val VarIntRestrictions = varIntRestrictions

  member this.ToLPCanonical() =
    // Step 1: Calculate final matrix size
    // constraints: '<=/>=' count + 2 * '=' count
    // variables: 'positive/negative' count + 2 * 'urs' count + '<=/>=' count + 2 * '=' count
    let constrIneqCount = this.ConstraintSigns |> Array.filter (fun x -> x = ConstraintSign.GreaterOrEqual || x = ConstraintSign.LessOrEqual) |> Array.length
    let constrEqualCount = this.ConstraintSigns.Length - constrIneqCount
    let ursCount = this.VarSignRestrictions |> Array.filter (fun x -> x = SignRestriction.Unrestricted) |> Array.length
    let posNegCount = this.VarSignRestrictions.Length - ursCount

    let constraintCount = constrIneqCount + 2 * constrEqualCount
    let variableCount = posNegCount + 2 * ursCount + constraintCount

    let objective = Vector<double>.Build.Dense(variableCount, 0.0)
    let constraintMat = Matrix<double>.Build.Dense(constraintCount, variableCount, 0.0)
    let rhs = Vector<double>.Build.Dense(constraintCount, 0.0)
    let variableNames = Array.create variableCount ""
    let mutable varIntRestrictions = Array.create (variableCount - this.VarIntRestrictions.Length) IntRestriction.Unrestricted |> Array.append this.VarIntRestrictions

    // Step 1: Set up variable names & objective row
    let mutable writeColumn = 0
    for i in [| 0 .. this.VarSignRestrictions.Length - 1 |] do
      match this.VarSignRestrictions.[i] with
      | SignRestriction.Positive ->
        variableNames.[writeColumn] <- this.VarNames.[i]
        objective.[writeColumn] <- this.Objective.[i]
      | SignRestriction.Negative ->
        variableNames.[writeColumn] <- sprintf "%s-" this.VarNames.[i]
        objective.[writeColumn] <- -this.Objective.[i]
      | SignRestriction.Unrestricted ->
        variableNames.[writeColumn] <- sprintf "%s+" this.VarNames.[i]
        objective.[writeColumn] <- this.Objective.[i]
        writeColumn <- writeColumn + 1
        variableNames.[writeColumn] <- sprintf "%s-" this.VarNames.[i]
        objective.[writeColumn] <- -this.Objective.[i]
      | _ -> ()
      writeColumn <- writeColumn + 1

    for i in [| 0 .. this.ConstraintSigns.Length - 1 |] do
      match this.ConstraintSigns.[i] with
      | ConstraintSign.LessOrEqual ->
        variableNames.[writeColumn] <- sprintf "s%d" (i + 1)
      | ConstraintSign.Equal ->
        variableNames.[writeColumn] <- sprintf "s%d" (i + 1)
        writeColumn <- writeColumn + 1
        variableNames.[writeColumn] <- sprintf "e%d" (i + 1)
      | ConstraintSign.GreaterOrEqual ->
        variableNames.[writeColumn] <- sprintf "e%d" (i + 1)
      | _ -> ()
      writeColumn <- writeColumn + 1         

    // Leftover variables are bin constraint ones (s-vars)
    while writeColumn < variableCount do
      let constraint_count = writeColumn - (variableCount - constraintCount)
      variableNames.[writeColumn] <- sprintf "s%d" constraint_count
      writeColumn <- writeColumn + 1

    // Step 2: Constraints     
    let getVarCoefficients readIdx =
      let row = Vector<double>.Build.Dense(variableCount, 0.0)
      let mutable column = 0
      for i in [| 0 .. this.ConstraintCoefficients.GetLength 1 - 1 |] do
        match this.VarSignRestrictions.[i] with
        | SignRestriction.Positive ->
          row.[column] <- this.ConstraintCoefficients.[readIdx,i]
        | SignRestriction.Negative ->
          row.[column] <- -this.ConstraintCoefficients.[readIdx, i]
        | SignRestriction.Unrestricted ->
          row.[column] <- this.ConstraintCoefficients.[readIdx, i]
          column <- column + 1
          row.[column] <- -this.ConstraintCoefficients.[readIdx, i]
        | _ -> ()
        column <- column + 1
      row

    writeColumn <- variableCount - constraintCount
    let mutable writeRow = 0
    for i in [| 0 .. this.ConstraintSigns.Length - 1 |] do
      match this.ConstraintSigns.[i] with
      | ConstraintSign.LessOrEqual ->
        let row = getVarCoefficients i
        row.[writeColumn] <- 1
        constraintMat.SetRow(writeRow, row)
        rhs.[writeRow] <- this.RHS.[i]
      | ConstraintSign.GreaterOrEqual ->
        let row = -getVarCoefficients i
        row.[writeColumn] <- 1
        constraintMat.SetRow(writeRow, row)
        rhs.[writeRow] <- -this.RHS.[i]
      | ConstraintSign.Equal ->
        let row1 = getVarCoefficients i
        let row2 = -row1.Clone()

        row1.[writeColumn] <- 1
        constraintMat.SetRow(writeRow, row1)
        rhs.[writeRow] <- this.RHS.[i]

        writeRow <- writeRow + 1
        writeColumn <- writeColumn + 1

        row2.[writeColumn] <- 1
        constraintMat.SetRow(writeRow, row2)
        rhs.[writeRow] <- -this.RHS.[i]
      | _ -> ()
      writeColumn <- writeColumn + 1
      writeRow <- writeRow + 1

      LPCanonical(this.ObjectiveType, objective, constraintMat, rhs, variableNames, varIntRestrictions)

    member this.fromLPCanonical(var_dict: Dictionary<string, double>)=
      let ret = Dictionary<string, double>()

      for i in [ 0 .. this.VarNames.Length - 1 ] do
        let varName = this.VarNames.[i]
        match this.VarSignRestrictions.[i] with
        | SignRestriction.Positive ->
          ret.[varName] <- var_dict.[varName]
        | SignRestriction.Negative ->
          ret.[varName] <- -var_dict.[sprintf "%s-" varName]
        | SignRestriction.Unrestricted ->
          ret.[varName] <- var_dict.[sprintf "%s+" varName] - var_dict.[sprintf "%s-" varName]
        | _ -> ()

      ret
      

type ITree<'T> =
  abstract member Item: 'T
  abstract member Children: ITree<'T>[]

type SimplexResult =
  | Optimal of Dictionary<string, double> * double
  | Unbounded of string
  | Infeasible of int

type ISimplexResultProvider =
  abstract member SimplexResult: Option<SimplexResult>
    // Step 3: Add constraints for binary variables
    for i in [| 0 .. this.VarIntRestrictions.Length - 1 |] do
      match this.VarIntRestrictions.[i] with
      | IntRestriction.Unrestricted | IntRestriction.Integer -> ()
      | IntRestriction.Binary ->
        constraintMat.[writeRow, i] <- 1
        constraintMat.[writeRow, writeColumn] <- 1
        rhs.[writeRow] <- 1
        writeRow <- writeRow + 1
        writeColumn <- writeColumn + 1
        varIntRestrictions.[i] <- IntRestriction.Integer
      | _ -> ()

    LPCanonical(this.ObjectiveType, objective, constraintMat, rhs, variableNames, varIntRestrictions)

  new(objective: LPObjective, constraints: LPConstraint[], signRestrictions: SignRestriction[], intRestrictions: IntRestriction[])=
    let variableNames = 
      [| objective.LinearSum
      |> Array.map snd |]
      |> Array.append (constraints |> Array.map (fun x -> x.LeftSide |> Array.map snd))
      |> Parsing.bordaCount

    let lookup (values: (double * string)[]) (item: string)=
      match values |> Array.tryFind (fun x -> x |> snd = item) with
      | Some tuple -> tuple |> fst
      | None -> 0.0

    let constraintCoefficients =
      constraints
      |> Array.map (fun x -> variableNames |> Array.map (lookup x.LeftSide))
      |> array2D

    LPFormulation(
      objective.ObjectiveType,
      variableNames,
      objective.LinearSum |> Array.map fst,
      constraintCoefficients,
      constraints |> Array.map (fun x -> x.ConstraintSign),
      constraints |> Array.map (fun x -> x.RightSide),
      signRestrictions,
      intRestrictions
    )

  new(objective: LPObjective, constraints: LPConstraint[])=
    let varCount =
      objective.LinearSum
      |> Array.append (constraints |> Array.collect (fun x -> x.LeftSide))
      |> Array.distinctBy (fun x -> x |> snd)  |> Array.length

    let signRestrictions = Array.init varCount (fun _ -> SignRestriction.Positive)
    let intRestrictions = Array.init varCount (fun _ -> IntRestriction.Unrestricted)

    LPFormulation(
      objective,
      constraints,
      signRestrictions,
      intRestrictions
    )
