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
      
      // Check for comma in coefficient which indicates wrong decimal separator
      let nextChar = if i + coefficientStr.Length < input.Length then Some input.[i + coefficientStr.Length] else None
      if nextChar = Some ',' then
        failwithf "Invalid decimal format. Use period (.) for decimals, not comma (,). Found comma after '%s'" coefficientStr

      let coefficient = 
        if coefficientStr.Length = 0 then 
          sign 
        else 
          try
            sign * Double.Parse(coefficientStr, CultureInfo.InvariantCulture)
          with
          | :? System.FormatException -> 
            if coefficientStr.Contains(",") then
              failwithf "Invalid coefficient '%s'. Use period (.) for decimals, not comma (,)" coefficientStr
            else
              failwithf "Invalid coefficient format '%s'" coefficientStr

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

  new(variableNames: string array, values: double array, constraintSign: ConstraintSign, rhs: double)=
    LPConstraint(Array.zip values variableNames, constraintSign, rhs)

  new(variableNames: string array, values: Vector<double>, constraintSign: ConstraintSign, rhs: double)=
    LPConstraint(variableNames, values.ToArray(), constraintSign, rhs)

type KnapsackCanonical(
  variableNames: string array,
  objective: Vector<double>,
  weights: Vector<double>,
  maxWeight: double
) =
  let rankOrder = lazy (
      Array.zip (objective.ToArray()) (weights.ToArray())
      |> Array.mapi (fun i (z, c) -> i, z / c)
      |> Array.sortByDescending snd
      |> Array.map fst
    )

  member val Objective = objective
  member val VariableNames = variableNames
  member val Weights = weights
  member val MaxWeight = maxWeight
  member _.RankOrder = rankOrder.Value

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

  member this.WithConstraint(constraintObject: LPConstraint)=
    let lookup (values: (double * string)[]) (item: string)=
      match values |> Array.tryFind (fun x -> x |> snd = item) with
      | Some tuple -> tuple |> fst
      | None -> 0.0

    let newRow = variableNames |> Array.map (lookup constraintObject.LeftSide) |> Vector<double>.Build.Dense
    let newColumn = Vector<double>.Build.Dense (constraintCoefficients.RowCount + 1, 0.0)
    newColumn.[constraintCoefficients.RowCount] <- 1.0

    match constraintObject.ConstraintSign with
      | ConstraintSign.LessOrEqual -> 
        let newMatrix = constraintCoefficients.InsertRow(constraintCoefficients.RowCount, newRow).InsertColumn(constraintCoefficients.ColumnCount, newColumn)
        let newRHS = Vector<double>.Build.Dense(rhs.Count + 1, constraintObject.RightSide)
        rhs.CopySubVectorTo(newRHS, 0, 0, rhs.Count)
        let newVarNames = Array.append variableNames [| sprintf "s%d" newColumn.Count |]
        let newIntRestrictions =  Array.append varIntRestrictions [| IntRestriction.Unrestricted |]
        let newObjective = Vector<double>.Build.Dense(newRow.Count + 1, 0.0)
        objective.CopySubVectorTo(newObjective, 0, 0, objective.Count)
        LPCanonical(objectiveType, newObjective, newMatrix, newRHS, newVarNames, newIntRestrictions)
      | ConstraintSign.GreaterOrEqual ->
        let newMatrix = constraintCoefficients.InsertRow(constraintCoefficients.RowCount, -newRow).InsertColumn(constraintCoefficients.ColumnCount, newColumn)
        let newRHS = Vector<double>.Build.Dense(rhs.Count + 1, -constraintObject.RightSide)
        rhs.CopySubVectorTo(newRHS, 0, 0, rhs.Count)
        let newVarNames = Array.append variableNames [| sprintf "e%d" newColumn.Count |]
        let newIntRestrictions = Array.append varIntRestrictions [| IntRestriction.Unrestricted |]
        let newObjective = Vector<double>.Build.Dense(newRow.Count + 1, 0.0)
        objective.CopySubVectorTo(newObjective, 0, 0, objective.Count)
        LPCanonical(objectiveType, newObjective, newMatrix, newRHS, newVarNames, newIntRestrictions)
      | ConstraintSign.Equal ->
        this.WithConstraint(LPConstraint(constraintObject.LeftSide, ConstraintSign.LessOrEqual, constraintObject.RightSide))
            .WithConstraint(LPConstraint(constraintObject.LeftSide, ConstraintSign.GreaterOrEqual, constraintObject.RightSide))
      | s -> failwithf "Invalid constraint sign: %A" s

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

  do
    if objective.Length <> varNames.Length then invalidArg "varNames" "VarNames dimensions mismatch with objective length"
    if objective.Length <> constraintCoefficients.GetLength 1 then invalidArg "constraintCoefficients" "Constraint coefficients dimensions mismatch with objective length"
    if objective.Length <> varSignRestrictions.Length then invalidArg "varSignRestrictions" "Sign restrictions must be same length as objective"
    if objective.Length <> varIntRestrictions.Length then invalidArg "varIntRestrictions" "Int restrictions must be same length as objective"
    
    if rhs.Length <> constraintCoefficients.GetLength 0 then invalidArg "constraintCoefficients" "Constraint coefficients must have the same row count as rhs"
    if rhs.Length <> constraintSigns.Length then invalidArg "constraintSigns" "Constraint signs must be the same length as rhs"

  let objectiveObject = lazy (
      LPObjective(objectiveType, Array.zip objective varNames)
    )

  let constraintObjects = lazy (
      let grabRow (row: int) =
        [| 0 .. varNames.Length - 1 |]
        |> Array.map (Array2D.get constraintCoefficients row)

      constraintSigns
      |> Array.mapi (fun i s -> LPConstraint(varNames, grabRow i, s, rhs.[i]))
    )

  member val ObjectiveType = objectiveType
  member val VarNames = varNames
  member val Objective = objective
  member val ConstraintCoefficients = constraintCoefficients
  member val ConstraintSigns = constraintSigns
  member val RHS = rhs
  member val VarSignRestrictions = varSignRestrictions
  member val VarIntRestrictions = varIntRestrictions

  member _.WithConstraint(constr: LPConstraint)=
    let rec sort (names: string list) (values: (double * string) array) (ret: (double * string) array) =
      match names with
      | name :: rest ->
        sort rest (values |> Array.filter (snd >> (<>) name)) (Array.append ret (values |> Array.filter (snd >> (=) name)))
      | [] ->
        Array.append ret values

    let sorted = LPConstraint(sort (Array.toList varNames) constr.LeftSide [||], constr.ConstraintSign, constr.RightSide)
    let signRestrictions = Array.append varSignRestrictions (Array.create (max(sorted.LeftSide.Length - varSignRestrictions.Length) 0) SignRestriction.Positive)
    let intRestrictions = Array.append varIntRestrictions (Array.create (max(sorted.LeftSide.Length - varIntRestrictions.Length) 0) IntRestriction.Unrestricted)

    LPFormulation(objectiveObject.Value, Array.append constraintObjects.Value [| sorted |], signRestrictions, intRestrictions)

  member _.WithActivity(variableName: string) (objectiveCoeff: double) (coeffColumn: double array) (signRestriction: SignRestriction)  =
    if varNames |> Array.exists ((=) variableName) then invalidArg "name" "Variable name already taken"
    if coeffColumn.Length <> rhs.Length then invalidArg "coeffColumn" "Coefficient column not the same length as the other columns"

    let newCoeffMatrix = Array2D.zeroCreate rhs.Length (varNames.Length+1)
    for i in [ 0 .. rhs.Length - 1 ] do
      for j in [ 0 .. varNames.Length ] do
        if j = varNames.Length then
          newCoeffMatrix.[i,j] <- coeffColumn.[i]
        else
          newCoeffMatrix.[i,j] <- constraintCoefficients.[i,j]

    LPFormulation(
      objectiveType, 
      Array.append varNames [| variableName |],
      Array.append objective [| objectiveCoeff |],
      newCoeffMatrix,
      constraintSigns,
      rhs,
      Array.append varSignRestrictions [| signRestriction |],
      Array.append varIntRestrictions [| IntRestriction.Unrestricted |]
    )

  member _.WithRHSUpdate(row: int) (newValue: double)=
    if row < 0 || row >= rhs.Length then invalidArg "row" "row out of bounds"
    let newRHS = rhs |> Array.copy
    newRHS.[row] <- newValue

    LPFormulation(objectiveType, varNames, objective, constraintCoefficients, constraintSigns, newRHS, varSignRestrictions, varIntRestrictions)

  member _.WithObjectiveUpdate(column: int) (newValue: double)=
    if column < 0 || column >= varNames.Length then invalidArg "column" "column out of bounds"
    let newObjective = objective |> Array.copy
    newObjective.[column] <- newValue

    LPFormulation(objectiveType, varNames, newObjective, constraintCoefficients, constraintSigns, rhs, varSignRestrictions, varIntRestrictions)

  member _.WithConstraintCoeffUpdate (row: int) (column: int) (newValue: double) =
    if row < 0 || row >= rhs.Length then invalidArg "row" "row out of bounds"
    if column < 0 || column >= varNames.Length then invalidArg "column" "column out of bounds"

    let newCoeffMatrix = constraintCoefficients |> Array2D.copy
    newCoeffMatrix.[row, column] <- newValue

    LPFormulation(objectiveType, varNames, objective, newCoeffMatrix, constraintSigns, rhs, varSignRestrictions, varIntRestrictions)

  member _.ToKnapsackCanonical() =
    if varIntRestrictions |> Array.exists ((<>) IntRestriction.Binary) then failwith "Invalid knapsack: all variables should be binary"
    if constraintCoefficients.GetLength 0 <> 1 then failwith "Invalid knapsack: multiple constraints"
    if constraintSigns.[0] <> ConstraintSign.LessOrEqual then failwith "Invalid knapsack: constraint must be <="
    if objective |> Array.exists ((>) 0.0) then failwith "Invalid knapsack: objective coefficients must be positive"
    if objectiveType <> ObjectiveType.Max then failwith "Invalid knapsack: objective must be max"

    let weights = [| 0 .. objective.Length - 1 |] |> Array.map (Array2D.get constraintCoefficients 0)

    KnapsackCanonical(varNames, Vector<double>.Build.Dense objective, Vector<double>.Build.Dense weights, rhs.[0])


  member this.ToLPCanonical() =
    // Step 1: Calculate final matrix size
    // constraints: '<=/>=' count + 2 * '=' count
    // variables: 'positive/negative' count + 2 * 'urs' count + '<=/>=' count + 2 * '=' count
    let constrIneqCount = this.ConstraintSigns |> Array.filter (fun x -> x = ConstraintSign.GreaterOrEqual || x = ConstraintSign.LessOrEqual) |> Array.length
    let constrEqualCount = this.ConstraintSigns.Length - constrIneqCount
    let ursCount = this.VarSignRestrictions |> Array.filter (fun x -> x = SignRestriction.Unrestricted) |> Array.length
    let posNegCount = this.VarSignRestrictions.Length - ursCount
    let binCount = this.VarIntRestrictions |> Array.filter ((=) IntRestriction.Binary) |> Array.length

    let constraintCount = constrIneqCount + 2 * constrEqualCount + binCount
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

    let constrStart = writeColumn
    for i in [| 0 .. this.ConstraintSigns.Length - 1 |] do
      match this.ConstraintSigns.[i] with
      | ConstraintSign.LessOrEqual ->
        variableNames.[writeColumn] <- sprintf "s%d" (writeColumn - constrStart + 1)
      | ConstraintSign.Equal ->
        variableNames.[writeColumn] <- sprintf "s%d" (writeColumn - constrStart + 1)
        writeColumn <- writeColumn + 1
        variableNames.[writeColumn] <- sprintf "e%d" (writeColumn - constrStart + 1)
      | ConstraintSign.GreaterOrEqual ->
        variableNames.[writeColumn] <- sprintf "e%d" (writeColumn - constrStart + 1)
      | _ -> ()
      writeColumn <- writeColumn + 1         

    // Leftover variables are bin constraint ones (s-vars)
    while writeColumn < variableCount do
      let constraint_count = writeColumn - constrStart + 1
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

    writeColumn <- constrStart
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

    // Step 3: Add constraints for binary variables
    for i in [| 0 .. this.VarIntRestrictions.Length - 1 |] do
      match this.VarIntRestrictions.[i] with
      | IntRestriction.Unrestricted | IntRestriction.Integer -> ()
      | IntRestriction.Binary ->
        let variableColumn = variableNames |> Array.findIndex ((=) this.VarNames.[i])
        constraintMat.[writeRow, variableColumn] <- 1
        constraintMat.[writeRow, writeColumn] <- 1
        rhs.[writeRow] <- 1
        writeRow <- writeRow + 1
        writeColumn <- writeColumn + 1
        varIntRestrictions.[i] <- IntRestriction.Integer
      | _ -> ()

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

type SimplexResult =
  | Optimal of canonicalVars:Dictionary<string, double> * formulationVars:Dictionary<string, double> * objectiveValue:double
  | Unbounded of variableName:string
  | Infeasible of stoppingConstraint:int

type DualityResult =
  | StrongDuality of primalObjective:double * dualObjective:double
  | WeakDuality of primalObjective:double * dualObjective:double
  | NoDuality of reason:string

type DualFormulation(primal: LPFormulation) =
  let dualObjectiveType = if primal.ObjectiveType = ObjectiveType.Max then ObjectiveType.Min else ObjectiveType.Max
  let dualVarNames = Array.init primal.RHS.Length (fun i -> sprintf "y%d" (i+1))
  let dualObjective = primal.RHS
  let dualConstraintMatrix = Array2D.init primal.VarNames.Length primal.RHS.Length (fun i j -> primal.ConstraintCoefficients.[j,i])
  let dualRHS = primal.Objective
  
  // Dual constraint signs based on primal variable sign restrictions
  let dualConstraintSigns = 
    primal.VarSignRestrictions |> Array.map (fun signRestr ->
      match primal.ObjectiveType, signRestr with
      | ObjectiveType.Max, SignRestriction.Positive -> ConstraintSign.GreaterOrEqual
      | ObjectiveType.Max, SignRestriction.Negative -> ConstraintSign.LessOrEqual
      | ObjectiveType.Max, SignRestriction.Unrestricted -> ConstraintSign.Equal
      | ObjectiveType.Min, SignRestriction.Positive -> ConstraintSign.LessOrEqual
      | ObjectiveType.Min, SignRestriction.Negative -> ConstraintSign.GreaterOrEqual
      | ObjectiveType.Min, SignRestriction.Unrestricted -> ConstraintSign.Equal
      | _ -> ConstraintSign.Equal)
  
  // Dual variable sign restrictions based on primal constraint signs
  let dualSignRestrictions = 
    primal.ConstraintSigns |> Array.map (fun constrSign ->
      match primal.ObjectiveType, constrSign with
      | ObjectiveType.Max, ConstraintSign.LessOrEqual -> SignRestriction.Positive
      | ObjectiveType.Max, ConstraintSign.GreaterOrEqual -> SignRestriction.Negative
      | ObjectiveType.Max, ConstraintSign.Equal -> SignRestriction.Unrestricted
      | ObjectiveType.Min, ConstraintSign.LessOrEqual -> SignRestriction.Negative
      | ObjectiveType.Min, ConstraintSign.GreaterOrEqual -> SignRestriction.Positive
      | ObjectiveType.Min, ConstraintSign.Equal -> SignRestriction.Unrestricted
      | _ -> SignRestriction.Unrestricted)
  
  let dualIntRestrictions = Array.create dualVarNames.Length IntRestriction.Unrestricted

  member _.ToLPFormulation() =
    LPFormulation(
      dualObjectiveType,
      dualVarNames,
      dualObjective,
      dualConstraintMatrix,
      dualConstraintSigns,
      dualRHS,
      dualSignRestrictions,
      dualIntRestrictions
    )
