namespace LPR381.Core

open System
open MathNet.Numerics.LinearAlgebra

// ==========================
// PRIMAL SIMPLEX + LOGGING
// ==========================
module PrimalSimplex =
    // ---- numeric tol ----
    let eps = 1e-9

    // ---- data types ----
    type Tableau =
        { A : Matrix<double>           // m x n
          b : Vector<double>           // m
          c : Vector<double>           // n (max-form)
          basis : int[]                // length m
          varNames : string[]          // length n
          objectiveType : ObjectiveType }

    type SolveStatus =
        | Optimal
        | Unbounded of pivotCol:int
        | InfeasibleStart of string

    type Iteration =
        { IterNo        : int
          EnteringCol   : int option
          LeavingRow    : int option
          EnteringName  : string option
          LeavingName   : string option
          Basis         : int[]
          A             : Matrix<double>
          b             : Vector<double>
          Rc            : Vector<double>        // reduced costs: c - A^T y
          ZRow          : Vector<double>        // z - c = -Rc (tableau top row)
          Theta         : double option[]       // ratio column for current entering col
          Z             : double                // objective in original sense
          VarNames      : string[] }

    type SolveResult =
        { Status : SolveStatus
          FinalTableau : Tableau
          ObjectiveValue : double
          VariableValues : (string * double)[] }

    // ---- helpers ----
    let private isUnitColumn (A: Matrix<double>) (row:int) (col:int) =
        let m = A.RowCount
        if abs (A.[row,col] - 1.0) > eps then false
        else
            let mutable ok = true
            for i in 0 .. m - 1 do
                if i <> row && abs A.[i,col] > eps then ok <- false
            ok

    let private buildB (A: Matrix<double>) (basis:int[]) =
        let m = A.RowCount
        let B = Matrix<double>.Build.Dense(m, m, 0.0)
        for j in 0 .. m - 1 do
            B.SetColumn(j, A.Column(basis.[j]))
        B

    let private reducedCosts (tbl: Tableau) =
        // y from B^T y = c_B, then rc = c - A^T y
        let cB = Vector<double>.Build.DenseOfArray(tbl.basis |> Array.map (fun j -> tbl.c.[j]))
        let y  = (buildB tbl.A tbl.basis).Transpose().Solve(cB)
        tbl.c - tbl.A.TransposeThisAndMultiply(y)

    let private ratioTest (col: Vector<double>) (b: Vector<double>) =
        let mutable idx = -1
        let mutable minVal = Double.PositiveInfinity
        for i in 0 .. col.Count - 1 do
            if col.[i] > eps then
                let r = b.[i] / col.[i]
                if r < minVal - 1e-12 then
                    minVal <- r
                    idx <- i
        if idx = -1 then None else Some idx

    let private pivot (tbl: Tableau) (pr:int) (pc:int) =
        let A = tbl.A.Clone()
        let b = tbl.b.Clone()
        let m = A.RowCount

        let piv = A.[pr, pc]
        A.SetRow(pr, A.Row(pr) / piv)
        b.[pr] <- b.[pr] / piv

        for i in 0 .. m - 1 do
            if i <> pr then
                let factor = A.[i, pc]
                if abs factor > eps then
                    A.SetRow(i, A.Row(i) - factor * A.Row(pr))
                    b.[i] <- b.[i] - factor * b.[pr]

        { tbl with A = A
                   b = b
                   basis = tbl.basis |> Array.mapi (fun r j -> if r = pr then pc else j) }

    // ---- public API ----

    /// Build initial tableau from canonical LP (expects identity-like columns available).
    let initializeFromCanonical (lp: LPCanonical) =
        // flip Min to Max internally
        let c = if lp.ObjectiveType = ObjectiveType.Min then -lp.Objective else lp.Objective
        let A = lp.ConstraintMatrix.Clone()
        let b = lp.RHS.Clone()
        let m, n = A.RowCount, A.ColumnCount

        // pick identity columns for a starting basis
        let basis = Array.create m -1
        let used  = Array.zeroCreate n
        for i in 0 .. m - 1 do
            let mutable chosen = -1
            for j in 0 .. n - 1 do
                if used.[j] = 0 && isUnitColumn A i j && chosen = -1 then
                    chosen <- j
                    used.[j] <- 1
            if chosen = -1 then
                for j in 0 .. n - 1 do
                    if used.[j] = 0 && abs(A.[i,j]) > eps && chosen = -1 then
                        chosen <- j
                        used.[j] <- 1
            basis.[i] <- chosen

        if basis |> Array.exists ((=) -1) then
            raise (Exception "No clear basic columns found (Phase I needed).")

        { A = A; b = b; c = c; basis = basis
          varNames = lp.VariableNames; objectiveType = lp.ObjectiveType }

    let private snapshot (tbl: Tableau) (iter:int) (entering:int option) (leaving:int option) =
        let rc   = reducedCosts tbl
        let zrow = -rc

        // θ (ratio) for chosen entering column
        let theta =
            match entering with
            | Some pc ->
                [| for i in 0 .. tbl.A.RowCount - 1 ->
                    let a = tbl.A.[i, pc]
                    if a > eps then Some (tbl.b.[i] / a) else None |]
            | None -> Array.create tbl.A.RowCount None

        // compute current x and z (original sense)
        let n = tbl.A.ColumnCount
        let x = Array.create n 0.0
        for i in 0 .. tbl.basis.Length - 1 do
            x.[tbl.basis.[i]] <- tbl.b.[i]
        let zMax = (Vector<double>.Build.DenseOfArray x).DotProduct tbl.c
        let z = match tbl.objectiveType with | ObjectiveType.Min -> -zMax | _ -> zMax

        let enteringName = entering |> Option.map (fun j -> tbl.varNames.[j])
        let leavingName  = leaving  |> Option.map (fun r -> tbl.varNames.[tbl.basis.[r]])

        { IterNo       = iter
          EnteringCol  = entering
          LeavingRow   = leaving
          EnteringName = enteringName
          LeavingName  = leavingName
          Basis        = Array.copy tbl.basis
          A            = tbl.A.Clone()
          b            = tbl.b.Clone()
          Rc           = rc
          ZRow         = zrow
          Theta        = theta
          Z            = z
          VarNames     = tbl.varNames }

    module Pretty =
        // tune these:
        let colW = 10       // width of each column cell
        let blockCols = 8   // how many variable columns per block before wrapping

        // number + text formatting
        let fmt (x: double) = sprintf "%*.3f" colW x            // right-aligned numbers
        let right (s: string) =
            let s' = if s.Length > colW then s.Substring(0,colW) else s
            s'.PadLeft(colW)
        let center (s: string) =
            let s' = if s.Length > colW then s.Substring(0,colW) else s
            let pad = colW - s'.Length
            let left = pad / 2
            String(' ', left) + s' + String(' ', pad - left)

        let private printBlock (it: Iteration) (j0:int) (j1:int) =
            // header (centered), mark entering column with *
            let headers =
                [ for j in j0 .. j1-1 ->
                    let name =
                        match it.EnteringCol with
                        | Some pc when pc = j -> it.VarNames.[j] + "*"
                        | _ -> it.VarNames.[j]
                    center name ]
                |> String.concat ""

            // header row: tag | vars | rhs | theta
            printfn " %s | %s %s %s"
                (center "") headers (center "rhs") (center "theta")

            // z-row (z-c) aligned with data
            let zRowVals = [ for j in j0 .. j1-1 -> fmt it.ZRow.[j] ] |> String.concat ""
            printfn " %s | %s %s %s" (right "z") zRowVals (fmt 0.0) (center "-")

            // each constraint row
            for i in 0 .. it.A.RowCount - 1 do
                let rowVals = [ for j in j0 .. j1-1 -> fmt it.A.[i,j] ] |> String.concat ""
                let thetaStr =
                    match it.Theta.[i] with
                    | Some t -> fmt t
                    | None   -> center "-"
                let tag =
                    match it.LeavingRow with
                    | Some r when r = i -> sprintf "%d*" (i+1)
                    | _ -> sprintf "%d" (i+1)
                printfn " %s | %s %s %s" (right tag) rowVals (fmt it.b.[i]) thetaStr

            printfn ""  // blank line after block

        /// Full tableau with paging in blocks (prevents wrapping in narrow consoles).
        let printFullTable (it: Iteration) =
            let n = it.A.ColumnCount
            printfn "-------------- T-%d  (z = %.3f) --------------" it.IterNo it.Z
            let mutable j = 0
            while j < n do
                let j1 = min n (j + blockCols)
                printBlock it j j1
                j <- j1



    /// Solve and call `log` with a snapshot every iteration (incl. initial & final).
    let solveWithLogging (tbl0: Tableau) (log: Iteration -> unit) =
        let mutable tbl  = tbl0
        let mutable iter = 0
        let mutable status : SolveStatus = Optimal

        // initial tableau
        log (snapshot tbl iter None None)

        let mutable running = true
        while running do
            let rc = reducedCosts tbl
            // choose entering col (most positive rc)
            let mutable pc = -1
            let mutable best = 0.0
            for j in 0 .. rc.Count - 1 do
                if rc.[j] > best + eps then best <- rc.[j]; pc <- j

            if pc = -1 then
                // optimal
                log (snapshot tbl (iter + 1) None None)
                status <- Optimal
                running <- false
            else
                // leaving row by ratio test
                let col = tbl.A.Column(pc)
                match ratioTest col tbl.b with
                | None ->
                    log (snapshot tbl (iter + 1) (Some pc) None)
                    status <- Unbounded pc
                    running <- false
                | Some pr ->
                    // snapshot before the pivot with chosen (pc,pr)
                    log (snapshot tbl (iter + 1) (Some pc) (Some pr))
                    tbl <- pivot tbl pr pc
                    iter <- iter + 1

        // final result
        let n = tbl.A.ColumnCount
        let x = Array.create n 0.0
        for i in 0 .. tbl.basis.Length - 1 do x.[tbl.basis.[i]] <- tbl.b.[i]
        let zMax = (Vector<double>.Build.DenseOfArray x).DotProduct tbl.c
        let z = match tbl.objectiveType with | ObjectiveType.Min -> -zMax | _ -> zMax

        { Status = status
          FinalTableau = tbl
          ObjectiveValue = z
          VariableValues = Array.zip tbl.varNames x }

    /// Solve without printing
    let solve (tbl0: Tableau) =
        solveWithLogging tbl0 (fun _ -> ())

// ===================================
// “JUST GIVE ME C + CONSTRAINTS” API
// ===================================
module LpCanonicalBuilder =

    type NatConstraint = { Coeffs: double[]; Sign: ConstraintSign; RHS: double }
    // helpers
    let le rhs coeffs = { Coeffs = coeffs; Sign = ConstraintSign.LessOrEqual;    RHS = rhs }
    let ge rhs coeffs = { Coeffs = coeffs; Sign = ConstraintSign.GreaterOrEqual; RHS = rhs }
    let eq rhs coeffs = { Coeffs = coeffs; Sign = ConstraintSign.Equal;          RHS = rhs }

    /// Build LPCanonical and auto-add s/e columns.
    /// NOTE: some ≥ or = cases may require Phase I to get a feasible start (normal).
    let canonicalFromWithNames
        (sense       : ObjectiveType)
        (c           : double[])
        (constraints : NatConstraint[])
        (varNames    : string[]) =

        let n = c.Length
        let baseNames =
            if varNames.Length >= n then varNames.[0..n-1]
            else Array.init n (fun i -> if i < varNames.Length then varNames.[i] else $"x{i+1}")

        // count rows & added cols
        let mutable rows, addedCols = 0, 0
        for con in constraints do
            match con.Sign with
            | ConstraintSign.LessOrEqual    -> rows <- rows + 1; addedCols <- addedCols + 1
            | ConstraintSign.GreaterOrEqual -> rows <- rows + 1; addedCols <- addedCols + 1
            | ConstraintSign.Equal          -> rows <- rows + 2; addedCols <- addedCols + 2

        let m, nTot = rows, n + addedCols
        let A = Matrix<double>.Build.Dense(m, nTot, 0.0)
        let b = Vector<double>.Build.Dense(m, 0.0)
        let cVec = Vector<double>.Build.Dense(nTot, 0.0)
        for j in 0 .. n-1 do cVec.[j] <- c.[j]

        let names = System.Collections.Generic.List<string>(baseNames)
        let mutable r, add = 0, 0

        let putRow (coeffs:double[]) rhs addName =
            for j in 0 .. n-1 do A.[r,j] <- coeffs.[j]
            A.[r, n + add] <- 1.0
            b.[r] <- rhs
            names.Add(addName)
            add <- add + 1
            r <- r + 1

        for i = 0 to constraints.Length - 1 do
            let con = constraints.[i]
            match con.Sign with
            | ConstraintSign.LessOrEqual ->
                putRow con.Coeffs con.RHS $"s{i+1}"
            | ConstraintSign.GreaterOrEqual ->
                // flip to ≤ and add an 'e' column
                putRow (con.Coeffs |> Array.map (fun x -> -x)) (-con.RHS) $"e{i+1}"
            | ConstraintSign.Equal ->
                putRow con.Coeffs con.RHS $"s{i+1}"
                putRow (con.Coeffs |> Array.map (fun x -> -x)) (-con.RHS) $"e{i+1}"

        let allNames = names.ToArray()
        let ints     = Array.init nTot (fun _ -> IntRestriction.Unrestricted)
        LPCanonical(sense, cVec, A, b, allNames, ints)

    /// Convenience: auto names x1..xn
    let canonicalFrom (sense:ObjectiveType) (c:double[]) (constraints:NatConstraint[]) =
        let defaultNames = Array.init c.Length (fun i -> $"x{i+1}")
        canonicalFromWithNames sense c constraints defaultNames
