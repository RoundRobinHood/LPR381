namespace LPR381.Core

open System.Collections.Generic

module Explorer =
  let rec SolveSimplex<'T when 'T :> ISimplexResultProvider>(tree: ITree<'T>)=
    let queue = Queue<ITree<'T>>()
    
    queue.Enqueue tree

    let mutable bestOptimal : Option<SimplexResult> = Option.None
    let mutable infeasibleFound = false
    let mutable infeasibleConstraint = -1

    let rec loop () =
      if queue.Count = 0 then
        // Decide final outcome
        match bestOptimal with
        | Some opt -> opt
        | Option.None when infeasibleFound -> Infeasible infeasibleConstraint
        | Option.None -> Infeasible -1
      else
        let node = queue.Dequeue()

        match node.Item.SimplexResult with
        | Some (Unbounded v) ->
          // If any branch is unbounded, whole problem is unbounded
          Unbounded v
        | Some (Optimal (canon, form, obj)) ->
          match bestOptimal with
          | Option.None -> bestOptimal <- Some (Optimal (canon, form, obj))
          | Some (Optimal (_, _, bestObj)) ->
              if (if tree.Formulation.ObjectiveType = ObjectiveType.Max then obj > bestObj else obj < bestObj) then
                  bestOptimal <- Some (Optimal (canon, form, obj))
          | _ -> ()
          node.Children |> Array.iter queue.Enqueue
          loop ()
        | Some (Infeasible constraintNum) ->
          infeasibleFound <- true
          infeasibleConstraint <- constraintNum
          node.Children |> Array.iter queue.Enqueue
          loop ()
        | Option.None ->
          node.Children |> Array.iter queue.Enqueue
          loop ()

    loop ()

  let SolveBNB(bnb: RevisedBranchAndBound)=
    let itree = bnb :> ITree<RevisedBNBNode>

    let objective_type = itree.Item.SubSolution.Canon.ObjectiveType

    let is_better_solution (current: SimplexResult option) (test: SimplexResult) =
      match current with
      | Option.None -> true
      | Some state ->
        match state with
        | Optimal (_, _, objective) ->
          match test with
          | Optimal (_, _, testObjective) ->
            match objective_type with
            | ObjectiveType.Max -> testObjective > objective
            | ObjectiveType.Min -> testObjective < objective
            | _ -> failwithf "Invalid objective type: %A" objective_type
          | _ -> false
        | _ -> true

    let shouldnt_bound (current: SimplexResult option) (test: RevisedBNBNode) =
      match current with
      | Some (Optimal (_, _, objective)) ->
        let newObjective = test.SubSolution.ObjectiveValue
        match objective_type with
        | ObjectiveType.Max -> newObjective >= objective
        | ObjectiveType.Min -> newObjective <= objective
        | _ -> failwithf "Invalid objective type: %A" objective_type
      | _ -> true

    let rec solve (best_solution: SimplexResult option) (nodes: ITree<RevisedBNBNode> list)=

      let rec iterate (best_solution: SimplexResult option) (node_list: ITree<RevisedBNBNode> list)=
        match node_list with
        | [] -> best_solution
        | node :: rest ->
          let item = node.Item
          match item.State with
          | RevisedBNBState.ResultState state ->
            if is_better_solution best_solution state then
              iterate (Some state) rest
            else
              iterate best_solution rest
          | RevisedBNBState.Branch _ ->
            if shouldnt_bound best_solution item then
              iterate (solve best_solution (node.Children |> Array.toList)) rest
            else
              iterate best_solution rest

      iterate best_solution nodes

    match solve Option.None [ itree ] with
    | Some x -> x
    | Option.None -> failwith "No simplex result (unexpected)"

  let PerformSensitivityAnalysis(formulation: LPFormulation) =
    let tree = RevisedDualSimplex formulation
    let result = SolveSimplex tree
    
    match result with
    | Optimal _ ->
      let dualFormulation = DualFormulation formulation
      let dualResult = SolveSimplex (RevisedDualSimplex (dualFormulation.ToLPFormulation()))
      
      let dualityResult = 
        match result, dualResult with
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
      
      result, dualResult, dualityResult
    | _ -> failwithf "Cannot perform sensitivity analysis on non-optimal result: %A" result
