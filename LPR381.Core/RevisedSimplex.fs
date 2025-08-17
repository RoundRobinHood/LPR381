namespace LPR381.Core

open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra

type SimplexResult =
  | Optimal of Dictionary<string, double> * double
  | Unbounded of string
  | Infeasible of int

type EtaMatrix(col: int, d: Vector<double>) =
  let lazyMatrix = lazy (
    let n = d.Count
    let E = Matrix<double>.Build.DenseIdentity n
    for i in 0 .. n - 1 do
      if i <> col then
        E.[i, col] <- -d.[i] / d.[col]
      else
        E.[i, col] <- 1.0 / d.[col]
    E
  )

  member _.matrix = lazyMatrix.Value

  member this.apply(bInverse: Matrix<double>) =
    this.matrix * bInverse

type TableauState =
  | Pivot of int * int * EtaMatrix
  | ResultState of SimplexResult

type RevisedSimplexNode=
  {
    canon: LPCanonical
    basis: array<int>
    bInverse: Matrix<double>
    state: TableauState
  }

type RevisedPrimalSimplex(item: RevisedSimplexNode)=
  static let node(basis: array<int>, canon: LPCanonical, bInverse: Matrix<double>)=
    let readyCanon =
      if canon.ObjectiveType = ObjectiveType.Min then
        LPCanonical(canon.ObjectiveType, -canon.Objective, canon.ConstraintMatrix, canon.RHS, canon.VariableNames, canon.VarIntRestrictions)
      else
        canon

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
        bInverse= bInverse
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
          bInverse= bInverse
          state= ResultState (Unbounded readyCanon.VariableNames.[enteringVariable])
        }
      else
        // Pivot
        {
          canon= canon
          basis= basis
          bInverse= bInverse
          state= Pivot (leavingBasisIndex, enteringVariable, EtaMatrix(leavingBasisIndex, d))
        }

  let children =
    lazy (
      match item.state with
      | ResultState _  -> [||]
      | Pivot (r, c, eta) ->
        let basis = Array.copy item.basis
        basis.[r] <- c
        [| RevisedPrimalSimplex(node (basis, item.canon, eta.apply item.bInverse)) :> ITree<RevisedSimplexNode> |]
    )


  new(canon: LPCanonical)=
    let basis = [| canon.Objective.Count - canon.RHS.Count .. canon.Objective.Count - 1 |]
    RevisedPrimalSimplex(node(basis, canon, Matrix<double>.Build.DenseIdentity basis.Length))

  interface ITree<RevisedSimplexNode> with
    member _.Item = item
    member _.Children = children.Value

type RevisedDualSimplex(item: RevisedSimplexNode)=
  static let node(basis: array<int>, canon: LPCanonical, bInverse: Matrix<double>)=
    let readyCanon =
      if canon.ObjectiveType = ObjectiveType.Min then
        LPCanonical(canon.ObjectiveType, -canon.Objective, canon.ConstraintMatrix, canon.RHS, canon.VariableNames, canon.VarIntRestrictions)
      else
        canon

    // Check for negative RHS
    let rhs = bInverse * readyCanon.RHS
    if rhs.Exists ((>) 0.0) then
      // Choose leaving variable
      let mutable mostNegative = 0.0
      let mutable leavingBasisIndex = -1
      for i in [ 0 .. rhs.Count - 1 ] do
        if rhs.[i] < mostNegative then
          mostNegative <- rhs.[i]
          leavingBasisIndex <- i

      // Choose entering variable
      let c_B = basis |> Array.map (fun x -> readyCanon.Objective.[x]) |> Vector.Build.Dense
      let constraintRow = bInverse.Row leavingBasisIndex * readyCanon.ConstraintMatrix
      let mutable minRatio = infinity
      let mutable enteringVariable = -1
      for i in [ 0 .. readyCanon.Objective.Count - 1 ] do
        if constraintRow.[i] < 0 then
          let reducedCost = readyCanon.Objective.[i] - c_B.DotProduct(bInverse * readyCanon.ConstraintMatrix.Column i)
          let ratio = abs(reducedCost / constraintRow.[i])
          if ratio < minRatio then
            minRatio <- ratio
            enteringVariable <- i

      if enteringVariable = -1 then
        {
          canon= canon
          basis= basis
          bInverse= bInverse
          state= ResultState (Infeasible leavingBasisIndex)
        }
      else
        let d = bInverse * readyCanon.ConstraintMatrix.Column enteringVariable
        {
          canon= canon
          basis= basis
          bInverse= bInverse
          state= Pivot (leavingBasisIndex, enteringVariable, EtaMatrix(leavingBasisIndex, d))
        }
    else
      // Choose entering variable
      let c_B = basis |> Array.map (fun x -> readyCanon.Objective.[x]) |> Vector<double>.Build.Dense
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
          bInverse= bInverse
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
            bInverse= bInverse
            state= ResultState (Unbounded readyCanon.VariableNames.[enteringVariable])
          }
        else
          // Pivot
          {
            canon= canon
            basis= basis
            bInverse= bInverse
            state= Pivot (leavingBasisIndex, enteringVariable, EtaMatrix(leavingBasisIndex, d))
          }
  
  let children =
    lazy (
      match item.state with
      | ResultState _ -> [||]
      | Pivot (r, c, eta) ->
        let basis = Array.copy item.basis
        basis.[r] <- c
        [| RevisedDualSimplex(node (basis, item.canon, eta.apply item.bInverse)) :> ITree<RevisedSimplexNode> |]
      )

  new(canon: LPCanonical)=
    let basis = [| canon.Objective.Count - canon.RHS.Count .. canon.Objective.Count - 1 |]
    RevisedDualSimplex(node(basis, canon, Matrix<double>.Build.DenseIdentity basis.Length))

  interface ITree<RevisedSimplexNode> with
    member _.Item = item
    member _.Children = children.Value

module RevisedSimplex =
  let SolvePrimal(canon: LPCanonical)=
    let root = RevisedPrimalSimplex canon :> ITree<RevisedSimplexNode>
    let rec solve (node: ITree<RevisedSimplexNode>) =
      match node.Item.state with
      | ResultState s -> s
      | Pivot _ -> 
        solve node.Children.[0]

    solve root

  let SolveDual(canon: LPCanonical)=
    let root = RevisedDualSimplex canon :> ITree<RevisedSimplexNode>
    let rec solve (node: ITree<RevisedSimplexNode>) =
      match node.Item.state with
      | ResultState s -> s
      | Pivot _ -> solve node.Children.[0]

    solve root
