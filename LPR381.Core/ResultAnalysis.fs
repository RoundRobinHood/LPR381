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
          LowerBound = infinity
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

type RelaxedSimplexSensitivityContext(
  formulation: LPFormulation,
  canon: LPCanonical,
  basis: int array,
  bInverse: Matrix<double>
)=
  member val Formulation = formulation
  member val Canon = canon
  member val Basis = basis
  member val BInverse = bInverse.ToArray()
  member val internal bInverse = bInverse

  new(formulation: LPFormulation, canon: LPCanonical, basis: int array)=
    let B =
      basis
      |> Array.map canon.ConstraintMatrix.Column
      |> Matrix<double>.Build.DenseOfColumnVectors
    
    RelaxedSimplexSensitivityContext(formulation, canon, basis, B.Inverse())

  new(formulation: LPFormulation, basis: int array)=
    RelaxedSimplexSensitivityContext(formulation, formulation.ToLPCanonical(), basis)
