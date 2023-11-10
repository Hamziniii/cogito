module table

type CellRow = int
type CellCol = int
type CellPos = { Row: CellRow; Col: CellCol }

type CellVal =
    | CellInt of int
    | CellStr of string
    | CellExpr of string

type Table =
    { Cells: Map<CellPos, CellVal>
      Rows: int
      Cols: int }

type HorizDir =
    | LToR
    | RToL

type VertDir =
    | TToB
    | BToT

type IterOrder =
    | ByRow of HorizDir * VertDir
    | ByCol of VertDir * HorizDir
    | Unspecified

type Range =
    { Top: CellRow
      Bottom: CellRow
      Left: CellCol
      Right: CellCol
      Order: IterOrder }

let newTable: Table =
    { Cells = Map.empty
      Rows = 0
      Cols = 0 }

let getCell (t: Table) (p: CellPos) : CellVal option = Map.tryFind p t.Cells

let printTable (t: Table) : unit =
    printfn "------------------------------ TABLE ------------------------------"

    for i in 1 .. t.Rows do
        for j in 1 .. t.Cols do
            match getCell t { Row = i; Col = j } with
            | Some(CellInt v) -> printf "%d" v
            | Some(CellStr v) -> printf "%s" v
            | Some(CellExpr v) -> printf "%s" v
            | None -> printf "None"

            printf "\t"

        printfn ""

    printfn "---------------------------- END TABLE ----------------------------"

let updateCell (t: Table) (p: CellPos) (v: CellVal) : Table =
    { Cells = Map.add p v t.Cells
      Rows = if p.Row > t.Rows then p.Row else t.Rows
      Cols = if p.Col > t.Cols then p.Col else t.Cols }

let removeCell (t: Table) (p: CellPos) : Table =
    { Cells = Map.remove p t.Cells
      Rows = t.Rows
      Cols = t.Cols }

let copyCell (t: Table) (op: CellPos) (np: CellPos) : Table =
    match getCell t op with
    | Some v -> updateCell t np v
    | None -> removeCell t np

let moveCell (t: Table) (op: CellPos) (np: CellPos) : Table =
    match getCell t op with
    | Some v -> updateCell (removeCell t op) np v
    | None -> removeCell t np

let rec updateCells
    (nextCell: CellPos -> CellPos)
    (stopCond: CellPos -> bool)
    (t: Table)
    (p: CellPos)
    (f: CellPos -> CellVal option)
    : Table =
    if stopCond p then
        t
    else
        let nt =
            match f p with
            | Some v -> updateCell t p v
            | None -> removeCell t p

        updateCells nextCell stopCond nt (nextCell p) f

let rec removeCells (nextCell: CellPos -> CellPos) (stopCond: CellPos -> bool) (t: Table) (p: CellPos) : Table =
    if stopCond p then
        t
    else
        removeCells nextCell stopCond (removeCell t p) (nextCell p)

let getStopCond (r: Range) : (CellPos -> bool) =
    match r.Order with
    | Unspecified
    | ByRow(_, TToB) -> fun p -> p.Row > r.Bottom
    | ByRow(_, BToT) -> fun p -> p.Row < r.Top
    | ByCol(_, LToR) -> fun p -> p.Col > r.Right
    | ByCol(_, RToL) -> fun p -> p.Col < r.Left

let getNextCell (r: Range) : (CellPos -> CellPos) =
    match r.Order with
    | Unspecified
    | ByRow(LToR, TToB) ->
        fun p ->
            if p.Col < r.Right then
                { Row = p.Row; Col = p.Col + 1 }
            else
                { Row = p.Row + 1; Col = r.Left }
    | ByRow(LToR, BToT) ->
        fun p ->
            if p.Col < r.Right then
                { Row = p.Row; Col = p.Col + 1 }
            else
                { Row = p.Row - 1; Col = r.Left }
    | ByRow(RToL, TToB) ->
        fun p ->
            if p.Col > r.Left then
                { Row = p.Row; Col = p.Col - 1 }
            else
                { Row = p.Row + 1; Col = r.Right }
    | ByRow(RToL, BToT) ->
        fun p ->
            if p.Col > r.Left then
                { Row = p.Row; Col = p.Col - 1 }
            else
                { Row = p.Row - 1; Col = r.Right }
    | ByCol(TToB, LToR) ->
        fun p ->
            if p.Row < r.Bottom then
                { Row = p.Row + 1; Col = p.Col }
            else
                { Row = r.Top; Col = p.Col + 1 }
    | ByCol(TToB, RToL) ->
        fun p ->
            if p.Row < r.Bottom then
                { Row = p.Row + 1; Col = p.Col }
            else
                { Row = r.Top; Col = p.Col - 1 }
    | ByCol(BToT, LToR) ->
        fun p ->
            if p.Row > r.Top then
                { Row = p.Row - 1; Col = p.Col }
            else
                { Row = r.Bottom; Col = p.Col + 1 }
    | ByCol(BToT, RToL) ->
        fun p ->
            if p.Row > r.Top then
                { Row = p.Row - 1; Col = p.Col }
            else
                { Row = r.Bottom; Col = p.Col - 1 }

let getStartPos (r: Range) : CellPos =
    match r.Order with
    | Unspecified
    | ByRow(LToR, TToB) -> { Row = r.Top; Col = r.Left }
    | ByRow(LToR, BToT) -> { Row = r.Bottom; Col = r.Left }
    | ByRow(RToL, TToB) -> { Row = r.Top; Col = r.Right }
    | ByRow(RToL, BToT) -> { Row = r.Bottom; Col = r.Right }
    | ByCol(TToB, LToR) -> { Row = r.Top; Col = r.Left }
    | ByCol(TToB, RToL) -> { Row = r.Top; Col = r.Right }
    | ByCol(BToT, LToR) -> { Row = r.Bottom; Col = r.Left }
    | ByCol(BToT, RToL) -> { Row = r.Bottom; Col = r.Right }

let updateSubtable (t: Table) (r: Range) (f: CellPos -> CellVal option) : Table =
    updateCells (getNextCell r) (getStopCond r) t (getStartPos r) f

let removeSubtable (t: Table) (r: Range) : Table =
    removeCells (getNextCell r) (getStopCond r) t (getStartPos r)

let moveSubtable (t: Table) (r: Range) (op: CellPos) (np: CellPos) : Table =
    let rowOff = np.Row - op.Row
    let colOff = np.Col - op.Col

    let f =
        fun p ->
            getCell
                t
                { Row = p.Row - rowOff
                  Col = p.Col - colOff }

    let nr =
        { Top = r.Top + rowOff
          Bottom = r.Bottom + rowOff
          Right = r.Right + colOff
          Left = r.Left + colOff
          Order = r.Order }

    updateCells (getNextCell nr) (getStopCond nr) (removeSubtable t r) (getStartPos nr) f
