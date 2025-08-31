namespace LPR381.Core

open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra

type Tableau = 
  internal {
    columnNames: string[]
    rowNames: string[]
    values: Matrix<double>
  }
  member this.ColumnNames= this.columnNames
  member this.RowNames= this.rowNames
  member this.Values= this.values.ToArray()

type TableauState =
  | Pivot of row:int * col:int
  | ResultState of result:SimplexResult

type SimplexNode =
  {
    Tableau: Tableau
    State: TableauState
  }
  interface ISimplexResultProvider with
    member this.SimplexResult =
      match this.State with
      | ResultState s -> Some s
      | _ -> Option.None

type PrimalSimplex(item: SimplexNode, objectiveType: ObjectiveType, formulation: LPFormulation) =
  static let node(tableau: Tableau, objectiveType: ObjectiveType, formulation: LPFormulation) =
    // Choose entering variable
    let mutable enteringVariable = -1
    let mutable bestReducedCost = 0.0
    for i in [ 0 .. tableau.values.ColumnCount - 1 ] do
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
      for i in [ 0 .. tableau.values.ColumnCount - 2 ] do
        let rec isUnitColumn oneRow j =
          if j >= tableau.values.RowCount then
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
          var_dict.[tableau.ColumnNames.[i]] <- tableau.Values.[oneRow, tableau.values.ColumnCount - 1]
      let objValue = if objectiveType = ObjectiveType.Min then -tableau.Values.[0, tableau.values.ColumnCount - 1] else tableau.Values.[0, tableau.values.ColumnCount - 1]
      {
        Tableau= tableau
        State= ResultState (Optimal (var_dict, formulation.fromLPCanonical var_dict, objValue))
      }
    else
      // Choose leaving variable
      let mutable minRatio = infinity
      let mutable leavingBasisIndex = -1

      for i in [ 1 .. tableau.values.RowCount - 1 ] do
        if tableau.Values.[i, enteringVariable] > 0.0 then
          let ratio = tableau.Values.[i, tableau.values.ColumnCount - 1] / tableau.Values.[i, enteringVariable]
          if ratio < minRatio then
            minRatio <- ratio
            leavingBasisIndex <- i

      if leavingBasisIndex = -1 then
        {
          Tableau= tableau
          State= ResultState (Unbounded tableau.ColumnNames.[enteringVariable])
        }
      else
        {
          Tableau= tableau
          State= Pivot (leavingBasisIndex, enteringVariable)
        }

  let children =
    lazy (
      match item.State with
      | ResultState _ -> [||]
      | Pivot (r, c) ->
        let newValues = item.Tableau.values.Clone()
        newValues.SetRow(r, newValues.Row r / newValues.[r, c])
        for j in [ 0 .. newValues.RowCount - 1 ] do
          if j <> r then
            newValues.SetRow(j, newValues.Row j - newValues.Row r * newValues.[j, c])

        [| PrimalSimplex (node ({ item.Tableau with values = newValues }, objectiveType, formulation), objectiveType, formulation) :> ITree<SimplexNode> |]
    )

  new(formulation: LPFormulation)=
    let canon = formulation.ToLPCanonical()
    let values = Matrix<double>.Build.Dense(canon.ConstraintMatrix.RowCount + 1, canon.ConstraintMatrix.ColumnCount + 1)
    let objective = values.Row 0
    objective.SetSubVector(0, canon.Objective.Count, -canon.Objective)
    values.SetRow(0, objective)
    let rhs = values.Column (values.ColumnCount - 1)
    rhs.SetSubVector(1, canon.RHS.Count, canon.RHS)
    values.SetColumn(values.ColumnCount - 1, rhs)

    values.SetSubMatrix(1, 0, canon.ConstraintMatrix)

    PrimalSimplex(node ({
      columnNames= Array.append canon.VariableNames [| "RHS" |]
      rowNames= Array.init canon.ConstraintMatrix.RowCount (fun i -> sprintf "c%d" i)
      values = values
    }, canon.ObjectiveType, formulation), canon.ObjectiveType, formulation)

  interface ITree<SimplexNode> with
    member _.Item = item
    member _.Children = children.Value
    member _.Formulation = formulation
    member this.SensitivityContext =
      let rec solve (itree: ITree<SimplexNode>) =
        match itree.Item.State with
        | Pivot _ -> solve itree.Children.[0]
        | ResultState _ -> itree.Item

      let canon = formulation.ToLPCanonical()
      let solution = solve (this :> ITree<SimplexNode>)
      match solution.State with
      | Pivot _ -> failwith "Unexpected: pivot after solve"
      | ResultState (Optimal _) ->
        let basis = Array.init canon.RHS.Count (fun _ -> 0)
        let rows_grabbed = HashSet<int>()
        for i in [ 0 .. solution.Tableau.values.ColumnCount - 2 ] do
          let rec isUnitColumn oneRow j =
            if j >= solution.Tableau.values.RowCount then
              oneRow
            else
              let value = solution.Tableau.Values.[j, i]
              if abs value < 1e-9 then
                isUnitColumn oneRow (j+1)
              elif abs(value - 1.0) < 1e-9 then
                if oneRow = -1 then
                  isUnitColumn j (j + 1)
                else
                  -1
              else
                -1

          let oneRow = isUnitColumn -1 1
          if oneRow <> -1 && rows_grabbed.Contains oneRow |> not then
            basis.[oneRow-1] <- i
        RelaxedSimplexSensitivityContext(formulation, basis)
      | ResultState _ -> null
