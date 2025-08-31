namespace LPR381.Core

open MathNet.Numerics.LinearAlgebra

type RevisedBNBState =
  | Branch of variableIndex:int * variableName:string
  | ResultState of result:SimplexResult

type RevisedBNBNode(revisedTree: RevisedDualSimplex)=
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
            Branch x
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
      | Branch _ -> Option.None

type RevisedBranchAndBound(item: RevisedBNBNode, formulation: LPFormulation)=
  let children = lazy (
      match item.State with
        | ResultState _ -> [||]
        | Branch (branch_variable, variable_name) ->
          let node = item.SubSolution
          let canon = node.canon
          let solution_rhs = node.bInverse * canon.RHS
          let branch_basis_index = node.basis |> Array.findIndex ((=) branch_variable)

          let less_canon = canon.WithConstraint(LPConstraint([| (1.0, variable_name) |], ConstraintSign.LessOrEqual, floor(solution_rhs.[branch_basis_index])))
          let great_canon = canon.WithConstraint(LPConstraint([| (1.0, variable_name) |], ConstraintSign.GreaterOrEqual, ceil(solution_rhs.[branch_basis_index])))

          let new_column = Vector<double>.Build.Dense(node.basis.Length + 1, 0.0)
          new_column.[node.basis.Length] <- 1.0

          let less_bInverse = node.bInverse.InsertRow(node.basis.Length, -node.bInverse.Row branch_basis_index).InsertColumn(node.basis.Length, new_column)
          let more_bInverse = node.bInverse.InsertRow(node.basis.Length, node.bInverse.Row branch_basis_index).InsertColumn(node.basis.Length, new_column)

          let new_basis = Array.append node.basis [| canon.VariableNames.Length |]

          [|
            RevisedBranchAndBound(RevisedBNBNode(RevisedDualSimplex(formulation, less_canon, new_basis, less_bInverse)), formulation) :> ITree<RevisedBNBNode>
            RevisedBranchAndBound(RevisedBNBNode(RevisedDualSimplex(formulation, great_canon, new_basis, more_bInverse)), formulation)
          |]
    )

  new(formulation: LPFormulation)=
    RevisedBranchAndBound(RevisedBNBNode(RevisedDualSimplex formulation), formulation)

  interface ITree<RevisedBNBNode> with
    member _.Item = item
    member _.Children = children.Value
    member _.Formulation = formulation
    member _.SensitivityContext =
      let canon = item.SubSolution.canon
      if canon.VarIntRestrictions |> Array.exists ((<>) IntRestriction.Unrestricted) then null else
      RelaxedSimplexSensitivityContext(formulation, canon, item.SubSolution.basis, item.SubSolution.bInverse)

