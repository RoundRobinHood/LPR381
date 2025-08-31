namespace LPR381.Core

open MathNet.Numerics.LinearAlgebra

type BindSource =
  | Constraint of int
  | Variable of int*string
  | Singularity
  | None

type VariableRange =
  {
    UpperBound: double
    LowerBound: double
    UpperBoundInclusive: bool
    LowerBoundInclusive: bool
    UpperBoundSource: BindSource
    LowerBoundSource: BindSource
  }
  
  static member Unbounded =
    {
      UpperBound = infinity
      LowerBound = -infinity
      UpperBoundInclusive = false
      LowerBoundInclusive = false
      UpperBoundSource = None
      LowerBoundSource = None
    }

type CanonicalSensitivityContext (
  canon: LPCanonical,
  basis: int array,
  bInverse: Matrix<double>
)=
  let B = basis |> Array.map canon.ConstraintMatrix.Column |> Matrix<double>.Build.DenseOfColumnVectors
  let BDeterm = lazy (B.Determinant())
  let c_B = lazy (basis |> Array.map (fun x -> canon.Objective.[x]) |> Vector<double>.Build.Dense)
  let shadow_prices = lazy (c_B.Value * bInverse)
  let reduced_costs = lazy (canon.Objective - shadow_prices.Value * canon.ConstraintMatrix)
  let b = lazy (bInverse * canon.RHS)
  let constraintMatrix = lazy (bInverse * canon.ConstraintMatrix)
  let determB = lazy (b.Value * BDeterm.Value)
  let determC = lazy (reduced_costs.Value * BDeterm.Value)

  member val Canon = canon
  member val Basis = basis
  member val bInverse = bInverse

  member _.CB = c_B.Value
  member _.ShadowPrices = shadow_prices.Value
  member _.ReducedCosts = reduced_costs.Value
  member _.OptimalRHS = b.Value
  member _.ConstraintMatrix = constraintMatrix.Value

  member _.RHSRange i =
    let mutable lowerBoundSource, upperBoundSource = None, None
    let mutable lowerBound, upperBound = -infinity, infinity
    for j, rhs in b.Value.EnumerateIndexed() do
      let coeff = bInverse.[j, i]
      if coeff <> 0 then
        let ratio = -rhs / coeff
        if ratio > 0 && ratio < upperBound || ratio = 0 && coeff < 0 then
          upperBound <- ratio
          upperBoundSource <- Constraint j
        elif ratio < 0 && ratio > lowerBound || ratio = 0 && coeff > 0 then
          lowerBound <- ratio
          lowerBoundSource <- Constraint j
    {
      UpperBound = canon.RHS.[i] + upperBound
      LowerBound = canon.RHS.[i] + lowerBound
      UpperBoundInclusive = upperBound <> infinity
      LowerBoundInclusive = lowerBound <> -infinity
      UpperBoundSource = upperBoundSource
      LowerBoundSource = lowerBoundSource
    }

  member _.ObjectiveCoeffRange i =
    match basis |> Array.tryFindIndex ((=) i) with
    | Some basis_index ->
      let mutable lowerBoundSource, upperBoundSource = None, None
      let mutable lowerBound, upperBound = -infinity, infinity
      for j, coeff in (constraintMatrix.Value.Row basis_index).EnumerateIndexed() do
        if basis |> Array.contains j |> not && coeff <> 0 then
          let coeff = if canon.ObjectiveType = ObjectiveType.Max then coeff else -coeff
          let ratio = reduced_costs.Value.[j] / coeff
          if ratio < 0 && ratio > lowerBound || ratio = 0 && coeff < 0 then
            lowerBound <- ratio
            lowerBoundSource <- Variable (j, canon.VariableNames.[j])
          elif ratio > 0 && ratio < upperBound || ratio = 0 && coeff > 0 then
            upperBound <- ratio
            upperBoundSource <- Variable (j, canon.VariableNames.[j])

      {
        UpperBound = canon.Objective.[i] + upperBound
        LowerBound = canon.Objective.[i] + lowerBound
        UpperBoundInclusive = upperBound <> infinity
        LowerBoundInclusive = lowerBound <> -infinity
        UpperBoundSource = upperBoundSource
        LowerBoundSource = lowerBoundSource
      }
    | Option.None ->
      if canon.ObjectiveType = ObjectiveType.Max then
        {
          UpperBound = canon.Objective.[i] - reduced_costs.Value.[i]
          LowerBound = -infinity
          UpperBoundInclusive = true
          LowerBoundInclusive = false
          UpperBoundSource = Variable (i, canon.VariableNames.[i])
          LowerBoundSource = None
        }
      else
        {
          UpperBound = infinity
          LowerBound = canon.Objective.[i] - reduced_costs.Value.[i]
          UpperBoundInclusive = false
          LowerBoundInclusive = true
          UpperBoundSource = None
          LowerBoundSource = Variable (i, canon.VariableNames.[i])
        }

  member _.ConstraintCellRange i j =
    let value = canon.ConstraintMatrix.[i, j]
    match basis |> Array.tryFindIndex ((=) j) with
    | Option.None ->
      let coeff = - (bInverse.Column i).DotProduct c_B.Value
      let coeff = if canon.ObjectiveType = ObjectiveType.Max then coeff else -coeff
      let ratio = - reduced_costs.Value.[j] / coeff
      if ratio < 0 || ratio = 0 && coeff < 0 then
        {
          UpperBound = infinity
          LowerBound = value + ratio
          UpperBoundInclusive = false
          LowerBoundInclusive = true
          UpperBoundSource = None
          LowerBoundSource = Variable (j, canon.VariableNames.[j])
        }
      elif ratio > 0 || ratio = 0 && coeff > 0 then
        {
          UpperBound = value + ratio
          LowerBound = -infinity
          UpperBoundInclusive = true
          LowerBoundInclusive = false
          UpperBoundSource = Variable (j, canon.VariableNames.[j])
          LowerBoundSource = None
        }
      else
        {
          UpperBound = infinity
          LowerBound = -infinity
          UpperBoundInclusive = false
          LowerBoundInclusive = false
          UpperBoundSource = None
          LowerBoundSource = None
        }
    | Some basis_index ->
      let determinant_difference = BDeterm.Value * bInverse.[basis_index, i]
      let DT_diff = BDeterm.Value * (bInverse.[basis_index, i] * bInverse - (bInverse.Column i).OuterProduct (bInverse.Row basis_index))

      let reduced_cost_delta = c_B.Value * DT_diff * canon.ConstraintMatrix - determinant_difference * canon.Objective
      let rhs_delta = DT_diff * canon.RHS

      let mutable lowerBoundSource, upperBoundSource = None, None
      let mutable lowerBound, upperBound = -infinity, infinity

      if determinant_difference <> 0 then
        let singularity_ratio = - BDeterm.Value / determinant_difference
        if singularity_ratio < 0 && singularity_ratio > lowerBound then
          lowerBound <- singularity_ratio
          lowerBoundSource <- Singularity
        elif singularity_ratio > 0 && singularity_ratio < upperBound then
          upperBound <- singularity_ratio
          upperBoundSource <- Singularity

      for var, coeff in reduced_cost_delta.EnumerateIndexed() do
        if basis |> Array.contains var |> not && coeff <> 0 then
          let coeff = if canon.ObjectiveType = ObjectiveType.Max then coeff else -coeff
          let ratio = determC.Value.[var] / coeff
          if ratio < 0 && ratio > lowerBound || ratio = 0 && coeff < 0 then
            lowerBound <- ratio
            lowerBoundSource <- Variable (var, canon.VariableNames.[var])
          elif ratio > 0 && ratio < upperBound || ratio = 0 && coeff > 0 then
            upperBound <- ratio
            upperBoundSource <- Variable (var, canon.VariableNames.[var])

      for row, coeff in rhs_delta.EnumerateIndexed() do
        if coeff <> 0 then
          let ratio = - determB.Value.[row] / coeff
          if ratio < 0 && ratio > lowerBound || ratio = 0 && coeff < 0 then
            lowerBound <- ratio
            lowerBoundSource <- Constraint row
          elif ratio > 0 && ratio < upperBound || ratio = 0 && coeff > 0 then
            upperBound <- ratio
            upperBoundSource <- Constraint row

      {
        UpperBound = value + upperBound
        LowerBound = value + lowerBound
        UpperBoundInclusive = upperBound <> infinity
        LowerBoundInclusive = lowerBound <> -infinity
        UpperBoundSource = upperBoundSource
        LowerBoundSource = lowerBoundSource
      }

  new(canon: LPCanonical, basis: int array)=
    let B =
      basis
      |> Array.map canon.ConstraintMatrix.Column
      |> Matrix<double>.Build.DenseOfColumnVectors

    CanonicalSensitivityContext(canon, basis, B.Inverse())

[<AllowNullLiteral>]
type RelaxedSimplexSensitivityContext(
  formulation: LPFormulation,
  canon: LPCanonical,
  basis: int array,
  bInverse: Matrix<double>
)=
  let constraintCardinality = function
    | ConstraintSign.LessOrEqual | ConstraintSign.GreaterOrEqual -> 1
    | ConstraintSign.Equal -> 2
    | _ -> failwith "Invalid constraint sign"

  let variableCardinality = function
    | SignRestriction.Positive | SignRestriction.Negative -> 1
    | SignRestriction.Unrestricted -> 2
    | _ -> failwith "Invalid sign restriction"

  let getCanonConstraintCount constraintsigns =
    constraintsigns |> Array.map constraintCardinality |> Array.sum

  let getCanonVariableCount signrestrictions =
    signrestrictions |> Array.map variableCardinality |> Array.sum

  let formulationConstraint canonConstraint =
    let rec ans (i: int) (sum: int) =
      let amount = constraintCardinality formulation.ConstraintSigns.[i]
      if sum + amount > canonConstraint || i = formulation.ConstraintSigns.Length - 1 then
        i
      else
        ans (i + 1) (sum + amount)
    ans 0 0

  let formulationVariable canonVariable =
    if canonVariable > getCanonVariableCount formulation.VarSignRestrictions then
      canonVariable, canon.VariableNames.[canonVariable]
    else
    let rec ans (i: int) (sum: int) =
      let amount = variableCardinality formulation.VarSignRestrictions.[i]
      if sum + amount > canonVariable || i = formulation.Objective.Length - 1 then
        i
      else
        ans (i + 1) (sum + amount)
    let formVar = ans 0 0
    formVar, formulation.VarNames.[formVar]

  let formulationSource = function
    | Constraint i -> Constraint (formulationConstraint i)
    | Variable (i, _) -> 
      let formulationVariable = formulationVariable i
      Variable formulationVariable
    | x -> x

  let flipRange range =
    {
      UpperBound = -range.LowerBound
      LowerBound = -range.UpperBound
      UpperBoundInclusive = range.LowerBoundInclusive
      LowerBoundInclusive = range.UpperBoundInclusive
      UpperBoundSource = range.LowerBoundSource
      LowerBoundSource = range.LowerBoundSource
    }

  let intersectRange range1 range2 =
    {
      UpperBound = if range1.UpperBound < range2.UpperBound then range1.UpperBound else range2.UpperBound
      LowerBound = if range1.LowerBound > range2.LowerBound then range1.LowerBound else range2.LowerBound
      UpperBoundInclusive = if range1.UpperBound < range2.UpperBound then range1.UpperBoundInclusive else range2.UpperBoundInclusive
      LowerBoundInclusive = if range1.LowerBound > range2.LowerBound then range1.LowerBoundInclusive else range2.LowerBoundInclusive
      UpperBoundSource = if range1.UpperBound < range2.UpperBound then range1.UpperBoundSource else range2.UpperBoundSource
      LowerBoundSource = if range1.LowerBound > range2.LowerBound then range1.LowerBoundSource else range2.LowerBoundSource
    }

  let formulationRange range =
    { range with UpperBoundSource = formulationSource range.UpperBoundSource; LowerBoundSource = formulationSource range.LowerBoundSource }

  let canonicalContext = CanonicalSensitivityContext(canon, basis, bInverse)

  let shadow_prices = lazy (
      formulation.ConstraintSigns
      |> Array.mapi (fun i x -> getCanonConstraintCount formulation.ConstraintSigns.[0..i-1], x)
      |> Array.map (fun (i, x) -> 
        match x with
        | ConstraintSign.LessOrEqual | ConstraintSign.GreaterOrEqual -> canonicalContext.ShadowPrices.[i]
        | ConstraintSign.Equal -> canonicalContext.ShadowPrices.[i] - canonicalContext.ShadowPrices.[i+1]
        | _ -> failwith "Invalid constraint sign"
      )
    )

  do
    if formulation.VarIntRestrictions |> Array.exists ((<>) IntRestriction.Unrestricted) then
      invalidArg "formulation" "Can't do relaxed sensitivity analysis on a problem with int-restricted variables"

  member val Formulation = formulation
  member val Canon = canon
  member val Basis = basis
  member val BInverse = bInverse.ToArray()
  member val internal bInverse = bInverse
  member _.CanonicalContext = canonicalContext

  member _.ShadowPrices = shadow_prices.Value

  member _.GetDualFormulation() = DualFormulation formulation

  member _.VerifyDuality(primalResult: SimplexResult, dualResult: SimplexResult) =
    match primalResult, dualResult with
    | Optimal(_, _, primalObj), Optimal(_, _, dualObj) ->
      let tolerance = 1e-6
      if abs(primalObj - dualObj) < tolerance then
        StrongDuality(primalObj, dualObj)
      else
        WeakDuality(primalObj, dualObj)
    | Optimal(_, _, primalObj), Unbounded _ -> WeakDuality(primalObj, infinity)
    | Unbounded _, Optimal(_, _, dualObj) -> WeakDuality(infinity, dualObj)
    | Infeasible _, Unbounded _ -> NoDuality "Primal infeasible, dual unbounded"
    | Unbounded _, Infeasible _ -> NoDuality "Primal unbounded, dual infeasible"
    | _ -> NoDuality "Both problems infeasible or other error"
  
  member _.IsInBasis variable =
    let var_names = basis |> Array.map (Array.get canon.VariableNames)
    let var_name = formulation.VarNames.[variable]
    match formulation.VarSignRestrictions.[variable] with
    | SignRestriction.Positive -> Array.exists ((=) var_name) var_names
    | SignRestriction.Negative -> Array.exists ((=) (sprintf "%s-" var_name)) var_names
    | SignRestriction.Unrestricted -> Array.exists ((=) (sprintf "%s+" var_name)) var_names || Array.exists ((=) (sprintf "%s-" var_name)) var_names
    | _ -> failwith "Invalid sign restriction"

  member _.RHSRange i =
    let canonicalIndex = getCanonConstraintCount formulation.ConstraintSigns.[0..i-1]
    match formulation.ConstraintSigns.[i] with
    | ConstraintSign.LessOrEqual ->
      canonicalContext.RHSRange canonicalIndex |> formulationRange
    | ConstraintSign.GreaterOrEqual ->
      canonicalContext.RHSRange canonicalIndex |> flipRange |> formulationRange
    | ConstraintSign.Equal ->
      let coeffs = bInverse.Column canonicalIndex - bInverse.Column (canonicalIndex + 1)
      
      let mutable lowerBoundSource, upperBoundSource = None, None
      let mutable lowerBound, upperBound = -infinity, infinity

      for j, coeff in coeffs.EnumerateIndexed() do
        if coeff <> 0 then
          let ratio = -canonicalContext.OptimalRHS.[j] / coeff
          if ratio > 0 && ratio < upperBound || ratio = 0 && coeff < 0 then
            upperBound <- ratio
            upperBoundSource <- Constraint (formulationConstraint j)
          elif ratio < 0 && ratio > lowerBound || ratio = 0 && coeff > 0 then
            lowerBound <- ratio
            lowerBoundSource <- Constraint (formulationConstraint j)

      {
        UpperBound = formulation.RHS.[i] + upperBound
        LowerBound = formulation.RHS.[i] + lowerBound
        UpperBoundInclusive = upperBound <> infinity
        LowerBoundInclusive = lowerBound <> -infinity
        UpperBoundSource = upperBoundSource
        LowerBoundSource = lowerBoundSource
      }
    | _ -> failwith "Unexpected error: incorrect constraint sign type"

  member _.ObjectiveCoeffRange i =
    let canonicalIndex = getCanonVariableCount formulation.VarSignRestrictions.[0..i-1]
    match formulation.VarSignRestrictions.[i] with
    | SignRestriction.Positive ->
      canonicalContext.ObjectiveCoeffRange canonicalIndex |> formulationRange
    | SignRestriction.Negative ->
      canonicalContext.ObjectiveCoeffRange canonicalIndex |> flipRange |> formulationRange
    | SignRestriction.Unrestricted ->
      let posIsBasic = Array.exists ((=) canonicalIndex) basis
      let negIsBasic = Array.exists ((=) (canonicalIndex+1)) basis

      if not posIsBasic && not negIsBasic then
        let posRange = canonicalContext.ObjectiveCoeffRange canonicalIndex |> formulationRange
        let negRange = canonicalContext.ObjectiveCoeffRange (canonicalIndex + 1) |> flipRange |> formulationRange

        intersectRange posRange negRange
      else
        let posBasisIndex = if posIsBasic then basis |> Array.findIndex ((=) canonicalIndex) else -1
        let negBasisIndex = if negIsBasic then basis |> Array.findIndex ((=) (canonicalIndex+1)) else -1
        let mutable lowerBoundSource, upperBoundSource = None, None
        let mutable lowerBound, upperBound = -infinity, infinity
        for j in [ 0 .. canon.Objective.Count - 1 ] do
          let coeffA = if posIsBasic then canonicalContext.ConstraintMatrix.[posBasisIndex, j] else 0
          let coeffB = if negIsBasic then canonicalContext.ConstraintMatrix.[negBasisIndex, j] else 0
          let coeff = if canon.ObjectiveType = ObjectiveType.Max then coeffA - coeffB else coeffB - coeffA
          if basis |> Array.contains j |> not && coeff <> 0 && j <> canonicalIndex && j <> canonicalIndex + 1 then
            let ratio = canonicalContext.ReducedCosts.[j] / coeff
            let formulationVar = formulationVariable j
            if ratio < 0 && ratio > lowerBound || ratio = 0 && coeff < 0 then
              lowerBound <- ratio
              lowerBoundSource <- Variable formulationVar
            elif ratio > 0 && ratio < upperBound || ratio = 0 && coeff > 0 then
              upperBound <- ratio
              upperBoundSource <- Variable formulationVar

        {
          UpperBound = formulation.Objective.[i] + upperBound
          LowerBound = formulation.Objective.[i] + lowerBound
          UpperBoundInclusive = upperBound <> infinity
          LowerBoundInclusive = lowerBound <> -infinity
          UpperBoundSource = upperBoundSource
          LowerBoundSource = lowerBoundSource
        }
    | _ -> failwith "Invalid sign restriction"

  member _.ConstraintCellRange i j =
    let value = formulation.ConstraintCoefficients.[i, j]
    let canonicalColumn = getCanonVariableCount formulation.VarSignRestrictions.[0..j-1]
    let canonicalRow = getCanonConstraintCount formulation.ConstraintSigns.[0..i-1]
    match formulation.VarSignRestrictions.[j] with
    | SignRestriction.Positive ->
      match formulation.ConstraintSigns.[i] with
      | ConstraintSign.LessOrEqual ->
        canonicalContext.ConstraintCellRange canonicalRow canonicalColumn |> formulationRange
      | ConstraintSign.GreaterOrEqual ->
        canonicalContext.ConstraintCellRange canonicalRow canonicalColumn |> flipRange |> formulationRange
      | ConstraintSign.Equal ->
        let coeff = - (bInverse.Column canonicalRow - bInverse.Column (canonicalRow+1)).DotProduct canonicalContext.CB
        let coeff = if canon.ObjectiveType = ObjectiveType.Max then coeff else -coeff
        let ratio = - canonicalContext.ReducedCosts.[canonicalColumn] / coeff
        if ratio < 0 || ratio = 0 && coeff < 0 then
          {
            UpperBound = infinity
            LowerBound = value + ratio
            UpperBoundInclusive = false
            LowerBoundInclusive = true
            UpperBoundSource = None
            LowerBoundSource = Variable (j, formulation.VarNames.[j])
          }
        elif ratio > 0 || ratio = 0 && coeff > 0 then
          {
            UpperBound = value + ratio
            LowerBound = -infinity
            UpperBoundInclusive = true
            LowerBoundInclusive = false
            UpperBoundSource = Variable (j, formulation.VarNames.[j])
            LowerBoundSource = None
          }
        else
          {
            UpperBound = infinity
            LowerBound = -infinity
            UpperBoundInclusive = false
            LowerBoundInclusive = false
            UpperBoundSource = None
            LowerBoundSource = None
          }
      | _ -> failwith "Invalid constraint sign"
    | SignRestriction.Negative ->
      match formulation.ConstraintSigns.[i] with
      | ConstraintSign.LessOrEqual ->
        canonicalContext.ConstraintCellRange canonicalRow canonicalColumn |> flipRange |> formulationRange
      | ConstraintSign.GreaterOrEqual ->
        canonicalContext.ConstraintCellRange canonicalRow canonicalColumn |> formulationRange
      | ConstraintSign.Equal ->
        let coeff = (bInverse.Column canonicalRow - bInverse.Column (canonicalRow+1)).DotProduct canonicalContext.CB
        let coeff = if canon.ObjectiveType = ObjectiveType.Max then coeff else -coeff
        let ratio = - canonicalContext.ReducedCosts.[canonicalColumn] / coeff
        let value = formulation.ConstraintCoefficients.[i,j]
        if ratio < 0 || ratio = 0 && coeff < 0 then
          {
            UpperBound = infinity
            LowerBound = value + ratio
            UpperBoundInclusive = false
            LowerBoundInclusive = true
            UpperBoundSource = None
            LowerBoundSource = Variable (j, formulation.VarNames.[j])
          }
        elif ratio > 0 || ratio = 0 && coeff > 0 then
          {
            UpperBound = value + ratio
            LowerBound = -infinity
            UpperBoundInclusive = true
            LowerBoundInclusive = false
            UpperBoundSource = Variable (j, formulation.VarNames.[j])
            LowerBoundSource = None
          }
        else
          {
            UpperBound = infinity
            LowerBound = -infinity
            UpperBoundInclusive = false
            LowerBoundInclusive = false
            UpperBoundSource = None
            LowerBoundSource = None
          }
      | _ -> failwith "Invalid constraint sign"
    | SignRestriction.Unrestricted ->
      let posIsBasic = Array.exists ((=) canonicalColumn) basis
      let negIsBasic = Array.exists ((=) (canonicalColumn+1)) basis
      let posBasisIndex = if posIsBasic then basis |> Array.findIndex ((=) canonicalColumn) else -1
      let negBasisIndex = if negIsBasic then basis |> Array.findIndex ((=) (canonicalColumn+1)) else -1
      let basic_index = if posIsBasic then posBasisIndex else negBasisIndex
      let non_basic_index = if posIsBasic then canonicalColumn + 1 else canonicalColumn

      if not posIsBasic && not negIsBasic then
        match formulation.ConstraintSigns.[i] with
        | ConstraintSign.LessOrEqual ->
          intersectRange (canonicalContext.ConstraintCellRange canonicalRow canonicalColumn) (canonicalContext.ConstraintCellRange canonicalRow (canonicalColumn+1) |> flipRange) |> formulationRange 
        | ConstraintSign.GreaterOrEqual ->
          intersectRange (canonicalContext.ConstraintCellRange canonicalRow canonicalColumn |> flipRange) (canonicalContext.ConstraintCellRange canonicalRow (canonicalColumn+1)) |> formulationRange
        | ConstraintSign.Equal ->
          let coeff = (bInverse.Column canonicalRow - bInverse.Column (canonicalRow + 1)).DotProduct canonicalContext.CB
          let coeff = if canon.ObjectiveType = ObjectiveType.Max then coeff else -coeff
          if coeff <> 0 then
            let posRatio = - canonicalContext.ReducedCosts.[canonicalColumn] / coeff
            let negRatio = canonicalContext.ReducedCosts.[canonicalColumn+1] / coeff
            let posRange =
              if posRatio > 0 || posRatio = 0 && coeff > 0 then
                {
                  UpperBound = value + posRatio
                  LowerBound = -infinity
                  UpperBoundSource = Variable (j, formulation.VarNames.[j])
                  LowerBoundSource = None
                  UpperBoundInclusive = true
                  LowerBoundInclusive = false
                }
              elif posRatio < 0 || posRatio = 0 && coeff < 0 then
                {
                  UpperBound = infinity
                  LowerBound = -infinity
                  UpperBoundSource = None
                  LowerBoundSource = Variable (j, formulation.VarNames.[j])
                  UpperBoundInclusive = false
                  LowerBoundInclusive = true
                }
              else
                VariableRange.Unbounded
            let negRange =
              if negRatio > 0 || negRatio = 0 && coeff < 0 then
                {
                  UpperBound = value + negRatio
                  LowerBound = -infinity
                  UpperBoundSource = Variable (j, formulation.VarNames.[j])
                  LowerBoundSource = None
                  UpperBoundInclusive = true
                  LowerBoundInclusive = false
                }
              elif negRatio < 0 || negRatio = 0 && coeff > 0 then
                {
                  UpperBound = infinity
                  LowerBound = value + negRatio
                  UpperBoundSource = None
                  LowerBoundSource = Variable (j, formulation.VarNames.[j])
                  UpperBoundInclusive = false
                  LowerBoundInclusive = true
                }
              else
                VariableRange.Unbounded
            intersectRange posRange negRange
          else
            VariableRange.Unbounded
        | _ -> failwith "Invalid constraint sign"
      else
        let determinant_difference, DT_diff =
          match formulation.ConstraintSigns.[i] with
          | ConstraintSign.LessOrEqual ->
            bInverse.[basic_index, canonicalRow] / bInverse.Determinant(),
            (bInverse.[basic_index, canonicalRow] * bInverse - (bInverse.Column canonicalRow).OuterProduct (bInverse.Row basic_index)) / bInverse.Determinant()
          | ConstraintSign.GreaterOrEqual ->
            -bInverse.[basic_index, canonicalRow] / bInverse.Determinant(),
            -(bInverse.[basic_index, canonicalRow] * bInverse - (bInverse.Column canonicalRow).OuterProduct (bInverse.Row basic_index)) / bInverse.Determinant()
          | ConstraintSign.Equal ->
            let u = Vector<double>.Build.Dense(bInverse.RowCount, 0.0)
            u.[canonicalRow] <- 1.0
            u.[canonicalRow+1] <- -1.0
            let v = Vector<double>.Build.Dense(bInverse.ColumnCount, 0.0)
            v.[basic_index] <- 1.0

            v * bInverse * u / bInverse.Determinant(),
            (v * bInverse * u * bInverse - bInverse * u.OuterProduct v * bInverse) / bInverse.Determinant()
          | _ -> failwith "Invalid constraint sign"

        let determinant_difference, DT_diff = if negIsBasic then -determinant_difference, -DT_diff else determinant_difference, DT_diff

        let reduced_cost_delta = canonicalContext.CB * DT_diff * canon.ConstraintMatrix - determinant_difference * canon.Objective
        let rhs_delta = DT_diff * canon.RHS

        let mutable lowerBoundSource, upperBoundSource = None, None
        let mutable lowerBound, upperBound = -infinity, infinity

        if determinant_difference <> 0 then
          let singularity_ratio = - determinant_difference / bInverse.Determinant()
          if singularity_ratio < 0 then
            lowerBound <- singularity_ratio
            lowerBoundSource <- Singularity
          elif singularity_ratio > 0 then
            upperBound <- singularity_ratio
            upperBoundSource <- Singularity

        let var_coeff = - (bInverse.Column canonicalRow - bInverse.Column (canonicalRow+1)).DotProduct canonicalContext.CB
        let var_coeff = if not negIsBasic then -var_coeff else var_coeff

        let determC = canonicalContext.ReducedCosts / bInverse.Determinant()
        let determB = canonicalContext.OptimalRHS / bInverse.Determinant()

        for var, coeff in reduced_cost_delta.EnumerateIndexed() do
          if basis |> Array.contains var |> not && coeff <> 0 && var <> canonicalColumn && var <> canonicalColumn + 1 then
            let coeff = if var = non_basic_index then coeff + var_coeff else coeff
            let coeff = if canon.ObjectiveType = ObjectiveType.Max then coeff else -coeff
            let ratio = determC.[var] / coeff
            if ratio < 0 && ratio > lowerBound || ratio = 0 && coeff < 0 then
              lowerBound <- ratio
              lowerBoundSource <- Variable (formulationVariable var)
            elif ratio > 0 && ratio < upperBound || ratio = 0 && coeff > 0 then
              upperBound <- ratio
              upperBoundSource <- Variable (formulationVariable var)

        for row, coeff in rhs_delta.EnumerateIndexed() do
          if coeff <> 0 then
            let ratio = - determB.[row] / coeff
            if ratio < 0 && ratio > lowerBound || ratio = 0 && coeff < 0 then
              lowerBound <- ratio
              lowerBoundSource <- Constraint (formulationConstraint row)
            elif ratio > 0 && ratio < upperBound || ratio = 0 && coeff > 0 then
              upperBound <- ratio
              upperBoundSource <- Constraint (formulationConstraint row)

        {
          UpperBound = value + upperBound
          LowerBound = value + lowerBound
          UpperBoundInclusive = upperBound <> infinity && not upperBoundSource.IsSingularity
          LowerBoundInclusive = lowerBound <> -infinity && not lowerBoundSource.IsSingularity
          UpperBoundSource = upperBoundSource
          LowerBoundSource = lowerBoundSource
        }
    | _ -> failwith "Invalid constraint sign"

  new(formulation: LPFormulation, canon: LPCanonical, basis: int array)=
    let B =
      basis
      |> Array.map canon.ConstraintMatrix.Column
      |> Matrix<double>.Build.DenseOfColumnVectors
    
    RelaxedSimplexSensitivityContext(formulation, canon, basis, B.Inverse())

  new(formulation: LPFormulation, basis: int array)=
    RelaxedSimplexSensitivityContext(formulation, formulation.ToLPCanonical(), basis)
