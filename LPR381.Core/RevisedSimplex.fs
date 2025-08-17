namespace LPR381.Core

open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra

type SimplexResult =
  | Optimal of Dictionary<string, double> * double
  | Unbounded of string
  | Infeasible of int

type EtaMatrix(row: int, d: Vector<double>) =
  let lazyMatrix = lazy (
    let E = Matrix<double>.Build.Dense(d.Count, d.Count, 0.0)
    printf "Evaluating matrix\n"
    for i in 0 .. d.Count - 1 do
      for j in 0 .. d.Count - 1 do
        if i = row then
          E.[i,j] <- d.[j]
        elif i = j then
          E.[i,j] <- 1.0
    E
  )

  member _.matrix = lazyMatrix.Value

  member _.apply(B: Matrix<double>) =
    let m, n = B.RowCount, B.ColumnCount
    let result = B.Clone()
    for j in 0 .. n-1 do
      let mutable sum = 0.0
      for k in 0 .. m-1 do
        sum <- sum + d.[k] * B.[k,j]
      result.[row,j] <- sum
    result

type ITree<'T> =
  abstract member Item: 'T
  abstract member Children: ITree<'T>[]

type TableauState =
  | Pivot of int * int
  | ResultState of SimplexResult

type RevisedSimplexNode=
  {
    canon: LPCanonical
    basis: array<int>
    state: TableauState
  }

type RevisedPrimalSimplex(item: RevisedSimplexNode)=
  static let node(basis: array<int>, canon: LPCanonical)=
    let readyCanon =
      if canon.ObjectiveType = ObjectiveType.Min then
        LPCanonical(canon.ObjectiveType, -canon.Objective, canon.ConstraintMatrix, canon.RHS, canon.VariableNames, canon.VarIntRestrictions)
      else
        canon

    let b = Matrix<double>.Build.Dense(readyCanon.RHS.Count, readyCanon.RHS.Count, 0.0)
    for i in [ 0 .. basis.Length - 1 ] do
      b.SetColumn(i, readyCanon.ConstraintMatrix.Column(basis.[i]))
    let bInverse = b.Inverse()

    // Choose entering variable
    let c_B = basis |> Array.map (fun x -> readyCanon.Objective.[x]) |> Vector.Build.Dense
    let mutable maxReducedCost = -infinity
    let mutable enteringVariable = -1
    for i in [ 0 .. readyCanon.Objective.Count - 1 ] do
      if basis |> Array.contains i |> not then
        let reducedCost = readyCanon.Objective.[i] - c_B.DotProduct(bInverse * readyCanon.ConstraintMatrix.Column i)
        if reducedCost > 0 && reducedCost > maxReducedCost then
          maxReducedCost <- reducedCost
          enteringVariable <- i

    let x_B = bInverse * readyCanon.RHS
    if enteringVariable = -1 then
      let var_dict = Dictionary<string, double>()
      let mutable basic_count = 0
      for i in [ 0 .. readyCanon.Objective.Count - 1 ] do
        if basis |> Array.contains i then
          var_dict.[readyCanon.VariableNames.[i]] <- x_B.[basic_count]
          basic_count <- basic_count + 1
        else
          var_dict.[readyCanon.VariableNames.[i]] <- 0.0
      let objective_value = (basis |> Array.map (fun x -> canon.Objective.[x]) |> Vector.Build.Dense).DotProduct x_B
      {
        canon= canon
        basis= basis
        state= ResultState (Optimal(var_dict, objective_value))
      }
    else
      // Choose leaving variable
      let d = bInverse * readyCanon.ConstraintMatrix.Column enteringVariable
      let mutable minRatio = infinity
      let mutable leavingBasisIndex = -1
      for i in [ 0 .. basis.Length - 1 ] do
        if d.[i] > 0.0 then
          let ratio = x_B.[i] / d.[i]
          if ratio < minRatio then
            minRatio <- ratio
            leavingBasisIndex <- i
      
      if leavingBasisIndex = -1 then
        {
          canon= canon
          basis= basis
          state= ResultState (Unbounded readyCanon.VariableNames.[enteringVariable])
        }
      else
        // Pivot
        {
          canon= canon
          basis= basis
          state= Pivot (leavingBasisIndex, enteringVariable)
        }

  let children =
    lazy (
      match item.state with
      | ResultState _  -> [||]
      | Pivot (r, c) ->
        let basis = Array.copy item.basis
        basis.[r] <- c
        [| RevisedPrimalSimplex(node (basis, item.canon)) :> ITree<RevisedSimplexNode> |]
    )


  new(canon: LPCanonical)=
    RevisedPrimalSimplex(node( [| canon.Objective.Count - canon.RHS.Count .. canon.Objective.Count - 1|], canon))

  interface ITree<RevisedSimplexNode> with
    member _.Item = item
    member _.Children = children.Value

module RevisedSimplex =
  let SolvePrimal(canon: LPCanonical)=
    let root = RevisedPrimalSimplex canon :> ITree<RevisedSimplexNode>
    let rec solve (node: ITree<RevisedSimplexNode>) =
      match node.Item.state with
      | ResultState s -> s
      | Pivot _ -> solve node.Children.[0]

    solve root
