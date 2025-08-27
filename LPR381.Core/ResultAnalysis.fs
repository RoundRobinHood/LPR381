namespace LPR381.Core

open MathNet.Numerics.LinearAlgebra
open System.Collections.Generic

type BindSource =
  | Constraint of int
  | Variable of string
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

type OptimalSimplexBasis(canon: LPCanonical, basis: int array, bInverse: Matrix<double>, variable_values: Dictionary<string, double>, objective_value: double)=
  let optimalRHS = lazy (bInverse * canon.RHS)
  let shadowPrices = lazy (
      let c_B = basis |> Array.map (fun x -> canon.Objective.[x]) |> Vector<double>.Build.Dense
      
      c_B * bInverse
    )

  member val Canon = canon
  member val Basis = basis
  member val BInverse = bInverse
  member val Variable_values = variable_values
  member val ObjectiveValue = objective_value

  member _.GetShadowPrice i = shadowPrices.Value.[i]
  member _.ShadowPrices = shadowPrices.Value.ToArray()

  member _.RHSRange i =
    let mutable lowerBoundSource, upperBoundSource = None, None
    let mutable lowerBound, upperBound = -infinity, infinity
    for j, rhs in optimalRHS.Value.EnumerateIndexed() do
      let coeff = bInverse.[i, j]
      if coeff <> 0 then
        let ratio = -rhs / coeff
        if ratio > 0 && ratio < upperBound then
          upperBound <- ratio
          upperBoundSource <- Constraint j
        elif ratio < 0 && ratio > lowerBound then
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

type SimplexResult =
  | Optimal of Dictionary<string, double> * double
  | Unbounded of string
  | Infeasible of int

type ISimplexResultProvider =
  abstract member SimplexResult: Option<SimplexResult>
