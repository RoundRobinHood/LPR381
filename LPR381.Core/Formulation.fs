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
