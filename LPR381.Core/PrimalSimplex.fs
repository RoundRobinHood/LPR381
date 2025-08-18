namespace LPR381.Core

open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra

type Tableau =
  {
    ColumnNames: string[]
    RowNames: string[]
    Values: Matrix<double>
  }

type TableauState =
  | Pivot of int * int
  | ResultState of SimplexResult

type SimplexNode =
  {
    tableau: Tableau
    state: TableauState
  }

type PrimalSimplex(item: SimplexNode, objectiveType: ObjectiveType) =
  static let node(tableau: Tableau, objectiveType: ObjectiveType) =
    // Choose entering variable
    let mutable enteringVariable = -1
    let mutable bestReducedCost = 0.0
    for i in [ 0 .. tableau.Values.ColumnCount - 1 ] do
      let reducedCost = tableau.Values.[0, i]
      match objectiveType with
      | ObjectiveType.Max ->
        if reducedCost < 0 && reducedCost < bestReducedCost then
          bestReducedCost <- reducedCost
          enteringVariable <- i
      | ObjectiveType.Min ->
        if reducedCost > 0 && reducedCost > bestReducedCost then
          bestReducedCost <- reducedCost
          enteringVariable <- i
      | _ -> ()

    if enteringVariable = -1 then
      let var_dict = Dictionary<string, double>()
      // NOTE: if multiple variables share a basis row, the first is taken as basic and the rest as 0
      let rows_grabbed = HashSet<int>()
      for i in [ 0 .. tableau.Values.ColumnCount - 2 ] do
        let rec isUnitColumn oneRow j =
          if j >= tableau.Values.RowCount then
            oneRow
          else
            let value = tableau.Values.[j, i]
            if abs value < 1e-9 then
              isUnitColumn oneRow (j + 1)
            elif abs(value - 1.0) < 1e-9 then
              if oneRow = -1 then
                isUnitColumn j (j + 1)
              else
                -1
            else
              -1

        let oneRow = isUnitColumn -1 1
        if oneRow = -1 || rows_grabbed.Contains oneRow then
          var_dict.[tableau.ColumnNames.[i]] <- 0
        else
          rows_grabbed.Add oneRow |> ignore
          var_dict.[tableau.ColumnNames.[i]] <- tableau.Values.[oneRow, tableau.Values.ColumnCount - 1]
      {
        tableau= tableau
        state= ResultState (Optimal (var_dict, tableau.Values.[0, tableau.Values.ColumnCount - 1]))
      }
    else
      // Choose leaving variable
      let mutable minRatio = infinity
      let mutable leavingBasisIndex = -1

      for i in [ 1 .. tableau.Values.RowCount - 1 ] do
        if tableau.Values.[i, enteringVariable] > 0.0 then
          let ratio = tableau.Values.[i, tableau.Values.ColumnCount - 1] / tableau.Values.[i, enteringVariable]
          if ratio < minRatio then
            minRatio <- ratio
            leavingBasisIndex <- i

      if leavingBasisIndex = -1 then
        {
          tableau= tableau
          state= ResultState (Unbounded tableau.ColumnNames.[enteringVariable])
        }
      else
        {
          tableau= tableau
          state= Pivot (leavingBasisIndex, enteringVariable)
        }

  let children =
    lazy (
      match item.state with
      | ResultState _ -> [||]
      | Pivot (r, c) ->
        let newValues = item.tableau.Values.Clone()
        newValues.SetRow(r, newValues.Row r / newValues.[r, c])
        for j in [ 0 .. newValues.RowCount - 1 ] do
          if j <> r then
            newValues.SetRow(j, newValues.Row j - newValues.Row r * newValues.[j, c])

        [| PrimalSimplex (node ({
            item.tableau with Values = newValues
          }, objectiveType), objectiveType) :> ITree<SimplexNode> |]
    )

  new(canon: LPCanonical)=
    let values = Matrix<double>.Build.Dense(canon.ConstraintMatrix.RowCount + 1, canon.ConstraintMatrix.ColumnCount + 1)
    let objective = values.Row 0
    objective.SetSubVector(0, canon.Objective.Count, -canon.Objective)
    values.SetRow(0, objective)
    let rhs = values.Column (values.ColumnCount - 1)
    rhs.SetSubVector(1, canon.RHS.Count, canon.RHS)
    values.SetColumn(values.ColumnCount - 1, rhs)

    values.SetSubMatrix(1, 0, canon.ConstraintMatrix)

    PrimalSimplex(node ({
      ColumnNames= Array.append canon.VariableNames [| "RHS" |]
      RowNames= Array.init canon.ConstraintMatrix.RowCount (fun i -> sprintf "c%d" i)
      Values = values
    }, canon.ObjectiveType), canon.ObjectiveType)

  interface ITree<SimplexNode> with
    member _.Item = item
    member _.Children = children.Value

module Simplex =
  let SolvePrimal(canon: LPCanonical)=
    let root = PrimalSimplex canon :> ITree<SimplexNode>
    let rec solve (node: ITree<SimplexNode>) =
      match node.Item.state with
      | ResultState s -> s
      | Pivot _ -> solve node.Children.[0]

    solve root
