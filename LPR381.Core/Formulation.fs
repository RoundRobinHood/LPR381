namespace LPR381.Core

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

module Parsing =
  let linearSum (input: string): (double * string) array =
    let skipWhitespace (i: int) =
      if i >= input.Length then i
      else i + (input.[i..] |> Seq.takeWhile Char.IsWhiteSpace |> Seq.length)

    let rec parseTerms (i: int) =
      let i = skipWhitespace i
      if i >= input.Length then [] else

      if input.[i] <> '+' && input.[i] <> '-' && Char.IsLetterOrDigit input.[i] |> not then
        failwithf "Invalid character: '%c'" input.[i]

      let sign = if input.[i] = '-' then -1.0 else 1.0
      let hadOperator = input.[i] = '-' || input.[i] = '+'
      let i = if hadOperator then i + 1 else i

      if i >= input.Length then
        if hadOperator then failwith "Missing variable name" else []
      else
        let i = skipWhitespace i
        if i >= input.Length then
          if hadOperator then failwithf "Missing term after '%c'" (if sign < 0.0 then '-' else '+') else []
        else
          let coefficientStr =
            input.[i..] |> Seq.takeWhile (fun c -> Char.IsDigit c || c = '.' || c = '-') |> Seq.toArray |> String
          let coefficient =
            if coefficientStr.Length = 0 then sign
            else sign * Double.Parse(coefficientStr, CultureInfo.InvariantCulture)
          let i = skipWhitespace (i + coefficientStr.Length)
          if i >= input.Length then failwith "Missing variable name"
          let variableName =
            input.[i..] |> Seq.takeWhile (fun c -> Char.IsLetterOrDigit c || c = '_') |> Seq.toArray |> String
          if variableName.Length = 0 then failwith "Missing variable name"
          (coefficient, variableName) :: parseTerms (i + variableName.Length)

    parseTerms 0 |> List.toArray

  let tryLinearSum (input: string, result: byref<(double * string) array>, error: byref<string>) =
    try
      result <- linearSum input
      error <- ""
      true
    with ex ->
      result <- [||]
      error <- ex.Message
      false

  /// Simple Borda count to produce a stable, merged ordering of variable names
  let bordaCount (rankings: string array array) =
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
) =
  member val ObjectiveType = objectiveType
  member val LinearSum = linearSum

  static member TryParse(objectiveType: ObjectiveType, input: string, result: byref<LPObjective>, error: byref<string>) =
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
) =
  member val LeftSide = leftSide
  member val ConstraintSign = constraintSign
  member val RightSide = rightSide

  static member TryParse(constraintSign: ConstraintSign, rightSide: double, leftSide: string, result: byref<LPConstraint>, error: byref<string>) =
    try
      result <- LPConstraint(Parsing.linearSum leftSide, constraintSign, rightSide)
      error <- ""
      true
    with ex ->
      result <- LPConstraint([||], constraintSign, rightSide)
      error <- ex.Message
      false

  static member TryParse(constraintSign: string, rightSide: double, leftSide: string, result: byref<LPConstraint>, error: byref<string>) =
    match constraintSign with
    | ">=" -> LPConstraint.TryParse(ConstraintSign.GreaterOrEqual, rightSide, leftSide, &result, &error)
    | "<=" -> LPConstraint.TryParse(ConstraintSign.LessOrEqual, rightSide, leftSide, &result, &error)
    | "="  -> LPConstraint.TryParse(ConstraintSign.Equal, rightSide, leftSide, &result, &error)
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

  /// Build a canonical form with nonnegative variables, slack/surplus/equality rows,
  /// and (optionally) x <= 1 rows for binary vars.
  member this.ToLPCanonical() =
    // ---- Step 1: Sizes ----
    let nVarsOriginal = this.VarSignRestrictions.Length
    let ineqCount =
      this.ConstraintSigns
      |> Array.filter (fun x -> x = ConstraintSign.GreaterOrEqual || x = ConstraintSign.LessOrEqual)
      |> Array.length
    let eqCount = this.ConstraintSigns.Length - ineqCount

    let baseRowCount = ineqCount + 2 * eqCount
    let binaryCount =
      this.VarIntRestrictions |> Array.filter ((=) IntRestriction.Binary) |> Array.length
    let totalRowCount = baseRowCount + binaryCount

    // Columns: expanded original vars (URS -> 2 columns), plus one slack/surplus per row
    let ursCount = this.VarSignRestrictions |> Array.filter ((=) SignRestriction.Unrestricted) |> Array.length
    let posNegCount = nVarsOriginal - ursCount
    let expandedVarCols = posNegCount + 2 * ursCount
    let totalColCount = expandedVarCols + totalRowCount

    let objective = Vector<double>.Build.Dense(totalColCount, 0.0)
    let constraintMat = Matrix<double>.Build.Dense(totalRowCount, totalColCount, 0.0)
    let rhs = Vector<double>.Build.Dense(totalRowCount, 0.0)
    let variableNames = Array.create totalColCount ""
    let varIntRestrictions = Array.create totalColCount IntRestriction.Unrestricted

    // ---- Step 2: Name expanded variables & objective coefficients & integrality map ----
    let mutable writeCol = 0
    for i = 0 to nVarsOriginal - 1 do
      match this.VarSignRestrictions.[i] with
      | SignRestriction.Positive ->
          variableNames.[writeCol] <- this.VarNames.[i]
          objective.[writeCol] <- this.Objective.[i]
          varIntRestrictions.[writeCol] <- this.VarIntRestrictions.[i]
          writeCol <- writeCol + 1
      | SignRestriction.Negative ->
          variableNames.[writeCol] <- sprintf "%s-" this.VarNames.[i]
          objective.[writeCol] <- -this.Objective.[i]
          // If original is integer/binary, carry it to the single column we created
          varIntRestrictions.[writeCol] <- this.VarIntRestrictions.[i]
          writeCol <- writeCol + 1
      | SignRestriction.Unrestricted ->
          variableNames.[writeCol] <- sprintf "%s+" this.VarNames.[i]
          objective.[writeCol] <- this.Objective.[i]
          varIntRestrictions.[writeCol] <- this.VarIntRestrictions.[i]
          writeCol <- writeCol + 1

          variableNames.[writeCol] <- sprintf "%s-" this.VarNames.[i]
          objective.[writeCol] <- -this.Objective.[i]
          varIntRestrictions.[writeCol] <- this.VarIntRestrictions.[i]
          writeCol <- writeCol + 1
      | _ -> ()

    // Slack/surplus/equality/binary slack columns start here
    let mutable slackCol = expandedVarCols

    // Preassign names for constraint-related columns (<=: s#, >=: e#, =: s#, e#)
    // We'll use row indices to make names stable and readable.
    // Fill during row construction to avoid mismatches.

    // Helper to get expanded row of coefficients for constraint i
    let getVarCoefficients (rowIdx: int) =
      let row = Vector<double>.Build.Dense(totalColCount, 0.0)
      let mutable col = 0
      for j = 0 to this.ConstraintCoefficients.GetLength(1) - 1 do
        match this.VarSignRestrictions.[j] with
        | SignRestriction.Positive ->
            row.[col] <- this.ConstraintCoefficients.[rowIdx, j]
            col <- col + 1
        | SignRestriction.Negative ->
            row.[col] <- -this.ConstraintCoefficients.[rowIdx, j]
            col <- col + 1
        | SignRestriction.Unrestricted ->
            row.[col] <- this.ConstraintCoefficients.[rowIdx, j]
            col <- col + 1
            row.[col] <- -this.ConstraintCoefficients.[rowIdx, j]
            col <- col + 1
        | _ -> ()
      row

    // ---- Step 3: Build constraint rows ----
    let mutable writeRow = 0
    for i = 0 to this.ConstraintSigns.Length - 1 do
      match this.ConstraintSigns.[i] with
      | ConstraintSign.LessOrEqual ->
          let row = getVarCoefficients i
          row.[slackCol] <- 1.0
          constraintMat.SetRow(writeRow, row)
          rhs.[writeRow] <- this.RHS.[i]
          variableNames.[slackCol] <- sprintf "s%d" (i + 1)
          writeRow <- writeRow + 1
          slackCol <- slackCol + 1

      | ConstraintSign.GreaterOrEqual ->
          // Multiply by -1 to convert to <=, then add slack
          let row = - (getVarCoefficients i)
          row.[slackCol] <- 1.0
          constraintMat.SetRow(writeRow, row)
          rhs.[writeRow] <- -this.RHS.[i]
          // Keep "e#" naming to reflect original >= (excess) semantics
          variableNames.[slackCol] <- sprintf "e%d" (i + 1)
          writeRow <- writeRow + 1
          slackCol <- slackCol + 1

      | ConstraintSign.Equal ->
          // First: <= with s_i
          let row1 = getVarCoefficients i
          row1.[slackCol] <- 1.0
          constraintMat.SetRow(writeRow, row1)
          rhs.[writeRow] <- this.RHS.[i]
          variableNames.[slackCol] <- sprintf "s%d" (i + 1)
          writeRow <- writeRow + 1
          slackCol <- slackCol + 1

          // Second: >= part -> multiply by -1, add "e_i"
          let row2 = - (getVarCoefficients i)
          row2.[slackCol] <- 1.0
          constraintMat.SetRow(writeRow, row2)
          rhs.[writeRow] <- -this.RHS.[i]
          variableNames.[slackCol] <- sprintf "e%d" (i + 1)
          writeRow <- writeRow + 1
          slackCol <- slackCol + 1

      | _ -> ()

    // ---- Step 4: For binary vars, add x <= 1 as x + s_b = 1 (since x >= 0 already by sign) ----
    // This keeps rows canonical with a single +1 slack.
    if binaryCount > 0 then
      for i = 0 to nVarsOriginal - 1 do
        if this.VarIntRestrictions.[i] = IntRestriction.Binary then
          // Find the canonical column index of this variable (assumes Positive or URS)
          // Prefer exact "x", then "x+" for URS. (Binary with Negative sign doesn't make sense.)
          let name = this.VarNames.[i]
          let varColOpt =
            variableNames
            |> Array.tryFindIndex ((=) name)
            |> Option.orElseWith (fun () -> variableNames |> Array.tryFindIndex ((=) (name + "+")))
          match varColOpt with
          | Some varCol ->
              let row = Vector<double>.Build.Dense(totalColCount, 0.0)
              row.[varCol] <- 1.0
              row.[slackCol] <- 1.0
              constraintMat.SetRow(writeRow, row)
              rhs.[writeRow] <- 1.0
              variableNames.[slackCol] <- sprintf "s_b%d" (i + 1)
              // Keep integrality marker for the variable column as Binary
              writeRow <- writeRow + 1
              slackCol <- slackCol + 1
          | None ->
              // If not found, we silently skip (covers odd cases); you can raise if you prefer:
              // failwithf "Binary var '%s' not found among canonical columns." name
              ()
      // writeRow should now equal totalRowCount

    LPCanonical(this.ObjectiveType, objective, constraintMat, rhs, variableNames, varIntRestrictions)

  /// Map a solution in canonical variables back to original variables
  member this.fromLPCanonical (var_dict: Dictionary<string, double>) =
    let ret = Dictionary<string, double>()
    for i = 0 to this.VarNames.Length - 1 do
      let varName = this.VarNames.[i]
      match this.VarSignRestrictions.[i] with
      | SignRestriction.Positive ->
          ret.[varName] <- if var_dict.ContainsKey(varName) then var_dict.[varName] else 0.0
      | SignRestriction.Negative ->
          let key = sprintf "%s-" varName
          ret.[varName] <- if var_dict.ContainsKey(key) then -var_dict.[key] else 0.0
      | SignRestriction.Unrestricted ->
          let kp = sprintf "%s+" varName
          let km = sprintf "%s-" varName
          let vp = if var_dict.ContainsKey(kp) then var_dict.[kp] else 0.0
          let vm = if var_dict.ContainsKey(km) then var_dict.[km] else 0.0
          ret.[varName] <- vp - vm
      | _ -> ()
    ret

  // ---- Convenience ctors ----
  new (objective: LPObjective, constraints: LPConstraint[], signRestrictions: SignRestriction[], intRestrictions: IntRestriction[]) =
    let variableNames =
      [| objective.LinearSum |> Array.map snd |]
      |> Array.append (constraints |> Array.map (fun x -> x.LeftSide |> Array.map snd))
      |> Parsing.bordaCount

    let lookup (values: (double * string)[]) (item: string) =
      match values |> Array.tryFind (fun x -> snd x = item) with
      | Some tuple -> fst tuple
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

  new (objective: LPObjective, constraints: LPConstraint[]) =
    let varCount =
      objective.LinearSum
      |> Array.append (constraints |> Array.collect (fun x -> x.LeftSide))
      |> Array.distinctBy snd
      |> Array.length

    let signRestrictions = Array.init varCount (fun _ -> SignRestriction.Positive)
    let intRestrictions  = Array.init varCount (fun _ -> IntRestriction.Unrestricted)

    LPFormulation(objective, constraints, signRestrictions, intRestrictions)

type ITree<'T> =
  abstract member Item: 'T
  abstract member Children: ITree<'T>[]

type SimplexResult =
  | Optimal of Dictionary<string, double> * double
  | Unbounded of string
  | Infeasible of int

type ISimplexResultProvider =
  abstract member SimplexResult: Option<SimplexResult>
