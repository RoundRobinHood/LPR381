namespace LPR381.Core

open MathNet.Numerics.LinearAlgebra

type RevisedCuttingPlanesState =
  | Cut of variableIndex:int * variableName:string
  | ResultState of result:SimplexResult

type RevisedCuttingPlanesNode(revisedTree: RevisedDualSimplex)=
  let sub_solution = lazy (
      let rec solve (tree: RevisedDualSimplex) =
        let itree = tree :> ITree<RevisedSimplexNode>
        match itree.Item.state with
        | RevisedTableauState.ResultState _ -> itree.Item
        | _ -> solve (itree.Children.[0] :?> RevisedDualSimplex)

      solve revisedTree
    )
  let state = lazy (
      let canon = (revisedTree :> ITree<RevisedSimplexNode>).Item.canon
      match sub_solution.Value.state with
      | RevisedTableauState.ResultState x ->
        match x with
        | Optimal (canonicalVars, formulationVars, objectiveValue) ->
          match
            canon.VariableNames |> Array.zip [| 0 .. canon.VariableNames.Length - 1 |]
            |> Array.tryFind
              (fun x ->
                canon.VarIntRestrictions.[fst x] = IntRestriction.Integer &&
                abs (round canonicalVars.[snd x] - canonicalVars.[snd x]) > 1e-9)
                with
          | Some x ->
            Cut x
          | Option.None -> Optimal (canonicalVars, formulationVars, objectiveValue) |> ResultState
        | x -> ResultState x

      | _ -> failwith "Invalid solution state"
  )

  member val RevisedTree = revisedTree
  member _.State = state.Value
  member _.SubSolution = sub_solution.Value

  interface ISimplexResultProvider with
    member _.SimplexResult =
      match state.Value with
      | ResultState x -> Some x
      | Cut _ -> Option.None

type RevisedCuttingPlanes(item: RevisedCuttingPlanesNode, formulation: LPFormulation)=
  let children = lazy (
      match item.State with
        | ResultState _ -> [||]
        | Cut (cut_variable, _) ->
          let node = item.SubSolution
          let canon = node.canon
          let cut_basis_index = node.basis |> Array.findIndex ((=) cut_variable)
          let get_gomory_coeff x = floor x - x
          let solution_rhs = (node.bInverse * canon.RHS).[cut_basis_index] |> get_gomory_coeff

          let coeff = (node.bInverse.Row cut_basis_index * canon.ConstraintMatrix).Map get_gomory_coeff
          let new_canon = canon.WithConstraint(LPConstraint(canon.VariableNames, coeff, ConstraintSign.LessOrEqual, solution_rhs))

          let new_row = Vector<double>.Build.Dense(canon.RHS.Count, 0.0)
          let new_column = Vector<double>.Build.Dense(canon.RHS.Count + 1, 0.0)
          new_column.[canon.RHS.Count] <- 1.0

          let new_bInverse = node.bInverse.InsertRow(canon.RHS.Count, new_row).InsertColumn(canon.RHS.Count, new_column)

          let new_basis = Array.append node.basis [| canon.VariableNames.Length |]

          [| RevisedCuttingPlanes(RevisedDualSimplex(formulation, new_canon, new_basis, new_bInverse) |> RevisedCuttingPlanesNode, formulation) :> ITree<RevisedCuttingPlanesNode> |]
    )

  new(formulation: LPFormulation)=
    RevisedCuttingPlanes(RevisedCuttingPlanesNode(RevisedDualSimplex formulation), formulation)

  interface ITree<RevisedCuttingPlanesNode> with
    member _.Item = item
    member _.Children = children.Value
    member _.Formulation = formulation
    member _.SensitivityContext =
      let canon = item.SubSolution.canon
      if canon.VarIntRestrictions |> Array.exists ((<>) IntRestriction.Unrestricted) then null else
      RelaxedSimplexSensitivityContext(formulation, canon, item.SubSolution.basis, item.SubSolution.bInverse)
      
