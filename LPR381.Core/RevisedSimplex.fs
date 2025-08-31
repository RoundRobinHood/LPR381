namespace LPR381.Core

open System.Collections.Generic
open MathNet.Numerics.LinearAlgebra

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

  member _.enteringColumn = d.ToArray()

  member _.matrix = lazyMatrix.Value

  member this.apply(bInverse: Matrix<double>) =
    this.matrix * bInverse

type RevisedTableauState =
  | Pivot of leavingBasis:int * enteringVariable:int * eta:EtaMatrix
  | ResultState of result:SimplexResult

type ProductForm =
  {
    EtaMatrix: double[,]
    BInverse: double[,]
  }

type PriceOutInfo =
  | Primal of nonBasicVariableCosts:(double * string)[] * b:double[] * ratios:double[] * enteringColumn:double[]
  | Dual of b:double[] * leavingRow:double[] * costs:double[] * absRatios:double[]

type RevisedSimplexNode=
  internal {
    canon: LPCanonical
    basis: array<int>
    bInverse: Matrix<double>
    state: RevisedTableauState
  }
    member this.Canon= this.canon
    member this.Basis= this.basis
    member this.BInverse= this.bInverse.ToArray()
    member this.State= this.state

    member this.ProductForm =
      match this.state with
      | Pivot (_, _, eta) ->
        {
          EtaMatrix= eta.matrix.ToArray()
          BInverse= this.bInverse.ToArray()
        }
      | _ -> failwith "Product form not available: not a pivoting table"

    member this.PriceOutInfo =
      match this.state with
      | Pivot (row, column, _) ->
        let x_B = this.bInverse * this.canon.RHS
        let objective = if this.canon.ObjectiveType = ObjectiveType.Max then this.canon.Objective else -this.canon.Objective
        let c_B = this.basis |> Array.map (fun x -> objective.[x]) |> Vector.Build.Dense
        let reducedCost = Vector<double>.Build.Dense(this.canon.Objective.Count, 0.0)
        for i in [ 0 .. reducedCost.Count - 1 ] do
          reducedCost.[i] <- objective.[i] - c_B.DotProduct(this.bInverse * this.canon.ConstraintMatrix.Column i)
        if x_B.Exists((>) 0.0) then
          let leavingRow = this.bInverse.Row row * this.canon.ConstraintMatrix
          let absRatios = leavingRow.Clone()
          for i in [ 0 .. absRatios.Count - 1 ] do
            if leavingRow.[i] < 0.0 then
              absRatios.[i] <- reducedCost.[i] / leavingRow.[i]
            else
              absRatios.[i] <- nan

          Dual(x_B.ToArray(), leavingRow.ToArray(), reducedCost.ToArray(), absRatios.ToArray())
        else
          let nonBasicVars = [| 0 .. this.canon.Objective.Count - 1 |] |> Array.filter (fun x -> this.Basis |> Array.contains x |> not) |> Array.map (fun x -> reducedCost.[x], this.canon.VariableNames.[x])
          let enteringColumn = this.bInverse * this.canon.ConstraintMatrix.Column column
          let ratios = enteringColumn.Clone()
          for i in [ 0 .. ratios.Count - 1 ] do
            if enteringColumn.[i] <> 0.0 then
              ratios.[i] <- x_B.[i] / enteringColumn.[i]
            else
              ratios.[i] <- nan

          Primal(nonBasicVars, x_B.ToArray(), ratios.ToArray(), enteringColumn.ToArray())
      | _ -> failwith "Price out info not available: not a pivoting table"

    member this.ObjectiveValue =
      let c_B = this.basis |> Array.map (fun x -> this.Canon.Objective.[x]) |> Vector.Build.Dense
      c_B.DotProduct(this.bInverse * this.canon.RHS)

    member this.SimplexNode =
      let c_B = this.basis |> Array.map (fun x -> this.Canon.Objective.[x]) |> Vector<double>.Build.Dense
      let constraintMat = this.bInverse * this.canon.ConstraintMatrix
      let rhs = this.bInverse * this.canon.RHS
      let objective_value = this.ObjectiveValue
      let reduced_costs = c_B * constraintMat - this.canon.Objective
      let columnNames = Array.append this.canon.VariableNames [| "RHS" |]
      let rowNames = Array.init this.canon.ConstraintMatrix.RowCount (fun i -> sprintf "c%d" i)
      let values = Matrix<double>.Build.Dense(this.canon.ConstraintMatrix.RowCount + 1, this.canon.ConstraintMatrix.ColumnCount + 1)  

      let objective = values.Row 0
      
      objective.SetSubVector(0, reduced_costs.Count, reduced_costs)
      objective.[reduced_costs.Count] <- objective_value
      values.SetRow(0, objective)

      let _rhs = values.Column rhs.Count
      _rhs.SetSubVector(1, rhs.Count, rhs)
      values.SetColumn(rhs.Count, _rhs)

      values.SetSubMatrix(1, 0, constraintMat)

      let state = 
        match this.state with
        | Pivot (r, c, _) -> TableauState.Pivot (r+1, c)
        | ResultState x -> TableauState.ResultState x

      {
        Tableau = {
          columnNames = Array.append this.canon.VariableNames [| "RHS" |]
          rowNames = Array.init rhs.Count (sprintf "c%d")
          values = values
        }
        State = state
      }

  interface ISimplexResultProvider with
    member this.SimplexResult = 
      match this.state with
        | ResultState s -> Some s
        | _ -> Option.None

type RevisedPrimalSimplex(item: RevisedSimplexNode, formulation: LPFormulation)=
  static let node(basis: array<int>, canon: LPCanonical, bInverse: Matrix<double>, formulation: LPFormulation)=
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
      for i in [ 0 .. readyCanon.Objective.Count - 1 ] do
        match basis |> Array.tryFindIndex ((=) i) with
        | Some basis_index ->
          var_dict.[readyCanon.VariableNames.[i]] <- x_B.[basis_index]
        | Option.None ->
          var_dict.[readyCanon.VariableNames.[i]] <- 0.0
      let objective_value = (basis |> Array.map (fun x -> canon.Objective.[x]) |> Vector.Build.Dense).DotProduct x_B
      {
        canon= canon
        basis= basis
        bInverse= bInverse
        state= ResultState (Optimal(var_dict, formulation.fromLPCanonical var_dict, objective_value))
      }

    else
      // Choose leaving variable
      let d = bInverse * readyCanon.ConstraintMatrix.Column enteringVariable
      let mutable minRatio = infinity
      let mutable leavingBasisIndex = -1
      let ratios = Array.init basis.Length (fun _ -> 0.0)
      for i in [ 0 .. basis.Length - 1 ] do
        if d.[i] > 0.0 then
          let ratio = x_B.[i] / d.[i]
          if ratio < minRatio then
            minRatio <- ratio
            leavingBasisIndex <- i
          ratios.[i] <- ratio
        elif d.[i] < 0.0 then
          ratios.[i] <- x_B.[i] / d.[i]
        else
          ratios.[i] <- nan
      
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
        [| RevisedPrimalSimplex(node (basis, item.canon, eta.apply item.bInverse, formulation), formulation) :> ITree<RevisedSimplexNode> |]
    )


  new(formulation: LPFormulation)=
    let canon = formulation.ToLPCanonical()
    let basis = [| canon.Objective.Count - canon.RHS.Count .. canon.Objective.Count - 1 |]
    RevisedPrimalSimplex(node(basis, canon, Matrix<double>.Build.DenseIdentity basis.Length, formulation), formulation)

  interface ITree<RevisedSimplexNode> with
    member _.Item = item
    member _.Children = children.Value
    member _.Formulation = formulation
    member this.SensitivityContext =
      let rec solve (itree: ITree<RevisedSimplexNode>) =
        match itree.Item.state with
        | Pivot _ -> solve itree.Children.[0]
        | ResultState _ -> itree.Item

      let solution = solve (this :> ITree<RevisedSimplexNode>)
      RelaxedSimplexSensitivityContext(formulation, solution.canon, solution.basis, solution.bInverse)

type RevisedDualSimplex(item: RevisedSimplexNode, formulation: LPFormulation)=
  static let node(basis: array<int>, canon: LPCanonical, bInverse: Matrix<double>, formulation: LPFormulation)=
    let readyCanon =
      if canon.ObjectiveType = ObjectiveType.Min then
        LPCanonical(canon.ObjectiveType, -canon.Objective, canon.ConstraintMatrix, canon.RHS, canon.VariableNames, canon.VarIntRestrictions)
      else
        canon

    // Check for negative RHS
    let rhs = bInverse * readyCanon.RHS
    if rhs.Exists ((>) -1e-9) then
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
      let ratios = Array.init readyCanon.Objective.Count (fun _ -> 0.0)
      for i in [ 0 .. readyCanon.Objective.Count - 1 ] do
        if constraintRow.[i] < -1e-9 && basis |> Array.exists ((=) i) |> not then
          let reducedCost = readyCanon.Objective.[i] - c_B.DotProduct(bInverse * readyCanon.ConstraintMatrix.Column i)
          let ratio = abs(reducedCost / constraintRow.[i])
          if ratio < minRatio then
            minRatio <- ratio
            enteringVariable <- i
          ratios.[i] <- ratio
        else
          ratios.[i] <- nan

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
        for i in [ 0 .. readyCanon.Objective.Count - 1 ] do
          match basis |> Array.tryFindIndex ((=) i) with
          | Some basis_index ->
            var_dict.[readyCanon.VariableNames.[i]] <- x_B.[basis_index]
          | Option.None ->
            var_dict.[readyCanon.VariableNames.[i]] <- 0.0

        let objective_value = (basis |> Array.map (fun x -> canon.Objective.[x]) |> Vector.Build.Dense).DotProduct x_B
        {
          canon= canon
          basis= basis
          bInverse= bInverse
          state= ResultState (Optimal(var_dict, formulation.fromLPCanonical var_dict, objective_value))
        }

      else
        // Choose leaving variable
        let d = bInverse * readyCanon.ConstraintMatrix.Column enteringVariable
        let mutable minRatio = infinity
        let mutable leavingBasisIndex = -1
        let ratios = Array.init basis.Length (fun _ -> 0.0)
        for i in [ 0 .. basis.Length - 1 ] do
          if d.[i] > 0.0 then
            let ratio = x_B.[i] / d.[i]
            if ratio < minRatio then
              minRatio <- ratio
              leavingBasisIndex <- i
            ratios.[i] <- ratio
          elif d.[i] < 0.0 then
            ratios.[i] <- x_B.[i] / d.[i]
          else
            ratios.[i] <- nan

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
        [| RevisedDualSimplex(node (basis, item.canon, eta.apply item.bInverse, formulation), formulation) :> ITree<RevisedSimplexNode> |]
      )

  new(formulation: LPFormulation)=
    let canon = formulation.ToLPCanonical()
    let basis = [| canon.Objective.Count - canon.RHS.Count .. canon.Objective.Count - 1 |]
    RevisedDualSimplex(node(basis, canon, Matrix<double>.Build.DenseIdentity basis.Length, formulation), formulation)

  internal new(formulation: LPFormulation, canon: LPCanonical, basis: int array, bInverse: Matrix<double>)=
    RevisedDualSimplex(node(basis, canon, bInverse, formulation), formulation)

  new(formulation: LPFormulation, basis: int array)=
    let canon = formulation.ToLPCanonical()
    if basis.Length <> canon.RHS.Count then invalidArg "basis" "Basis is the wrong size"
    let b = basis |> Array.map canon.ConstraintMatrix.Column |> Matrix<double>.Build.DenseOfColumnVectors
    let bInverse = b.Inverse()

    RevisedDualSimplex(node(basis, canon, bInverse, formulation), formulation)

  new(context: RelaxedSimplexSensitivityContext)=
    if context = null then invalidArg "context" "Context is null"

    RevisedDualSimplex(context.Formulation, context.Basis)

  member _.WithFormulationRHSUpdate (row: int) (newValue: double) =
    let new_formulation = formulation.WithRHSUpdate row newValue

    RevisedDualSimplex(new_formulation, item.basis)

  member _.WithFormulationObjectiveUpdate (column: int) (newValue: double) =
    let new_formulation = formulation.WithObjectiveUpdate column newValue

    RevisedDualSimplex(new_formulation, item.basis)

  member _.WithFormulationConstraintCoeffUpdate (row: int) (column: int) (newValue: double) =
    let new_formulation = formulation.WithConstraintCoeffUpdate row column newValue

    RevisedDualSimplex(new_formulation, item.basis)

  member _.WithActivity(variableName: string) (objectiveCoeff: double) (coeffColumn: double array) (signRestriction: SignRestriction) =
    let new_formulation = formulation.WithActivity variableName objectiveCoeff coeffColumn signRestriction

    RevisedDualSimplex(new_formulation, item.basis)

  member _.WithConstraint(constr: LPConstraint) =
    let new_formulation = formulation.WithConstraint constr
    
    let basis =
      match constr.ConstraintSign with
      | ConstraintSign.LessOrEqual | ConstraintSign.GreaterOrEqual ->
        Array.append item.basis [| item.canon.Objective.Count |]
      | ConstraintSign.Equal ->
        Array.append item.basis [| item.canon.Objective.Count ; item.canon.Objective.Count + 1 |]
      | _ -> failwith "Invalid constraint sign"

    RevisedDualSimplex(new_formulation, basis)

  interface ITree<RevisedSimplexNode> with
    member _.Item = item
    member _.Children = children.Value
    member _.Formulation = formulation
    member this.SensitivityContext =
      let rec solve (itree: ITree<RevisedSimplexNode>) =
        match itree.Item.state with
        | Pivot _ -> solve itree.Children.[0]
        | ResultState _ -> itree.Item

      let solution = solve (this :> ITree<RevisedSimplexNode>)
      RelaxedSimplexSensitivityContext(formulation, solution.canon, solution.basis, solution.bInverse)
