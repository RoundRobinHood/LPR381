namespace LPR381.Core

type ITree<'T> =
  abstract member Item: 'T
  abstract member Children: ITree<'T>[]
  abstract member Formulation: LPFormulation
  abstract member SensitivityContext: RelaxedSimplexSensitivityContext

type ISimplexResultProvider =
  abstract member SimplexResult: Option<SimplexResult>
