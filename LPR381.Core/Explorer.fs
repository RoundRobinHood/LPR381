namespace LPR381.Core

module Explorer=
  let rec SolveSimplex<'T when 'T :> ISimplexResultProvider>(tree: ITree<'T>)=
    match tree.Item.SimplexResult with
    | Some result -> result
    | None -> SolveSimplex(tree.Children.[0])
