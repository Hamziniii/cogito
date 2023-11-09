module table

type CellRow = int
type CellCol = int
type CellPosition = { Row: CellRow; Col: CellCol }

type CellValue =
    | CellInt of int
    | CellString of string
    | CellExpression of string

type Table =
    { Cells: Map<CellPosition, CellValue>
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

type Subtable =
    { Top: CellRow
      Bottom: CellRow
      Left: CellCol
      Right: CellCol
      Order: IterOrder }

let newTable: Table =
    { Cells = Map.empty
      Rows = 0
      Cols = 0 }

let getCell (t: Table) (p: CellPosition) : CellValue option = Map.tryFind p t.Cells

let printTable (t: Table) : unit =
    printfn "------------------------------ TABLE ------------------------------"

    for i in 1 .. t.Rows do
        for j in 1 .. t.Cols do
            match getCell t { Row = i; Col = j } with
            | Some(CellInt v) -> printf "%d" v
            | Some(CellString v) -> printf "%s" v
            | Some(CellExpression v) -> printf "%s" v
            | None -> printf "None"

            printf "\t"

        printfn ""

    printfn "---------------------------- END TABLE ----------------------------"

let updateCell (t: Table) (p: CellPosition) (v: CellValue) : Table =
    { Cells = Map.add p v t.Cells
      Rows = if p.Row > t.Rows then p.Row else t.Rows
      Cols = if p.Col > t.Cols then p.Col else t.Cols }

let removeCell (t: Table) (p: CellPosition) : Table =
    { Cells = Map.remove p t.Cells
      Rows = t.Rows
      Cols = t.Cols }

let copyCell (t: Table) (op: CellPosition) (np: CellPosition) : Table =
    match getCell t op with
    | Some v -> updateCell t np v
    | None -> removeCell t np

let moveCell (t: Table) (op: CellPosition) (np: CellPosition) : Table =
    match getCell t op with
    | Some v -> updateCell (removeCell t op) np v
    | None -> removeCell t np

let rec updateCells
    (nextCell: CellPosition -> CellPosition)
    (stopCond: CellPosition -> CellPosition -> bool)
    (t: Table)
    (sp: CellPosition)
    (ep: CellPosition)
    (f: CellPosition -> CellValue option)
    : Table =
    if stopCond sp ep then
        t
    else
        let nt =
            match f sp with
            | Some v -> updateCell t sp v
            | None -> removeCell t sp

        updateCells nextCell stopCond nt (nextCell sp) ep f

let rec removeCells
    (nextCell: CellPosition -> CellPosition)
    (stopCond: CellPosition -> CellPosition -> bool)
    (t: Table)
    (sp: CellPosition)
    (ep: CellPosition)
    : Table =
    if stopCond sp ep then
        t
    else
        removeCells nextCell stopCond (removeCell t sp) (nextCell sp) ep

let getStopCond (st: Subtable) : (CellPosition -> CellPosition -> bool) =
    match st.Order with
    | Unspecified
    | ByRow(_, TToB) -> fun sp ep -> sp.Row > ep.Row
    | ByRow(_, BToT) -> fun sp ep -> sp.Row < ep.Row
    | ByCol(_, LToR) -> fun sp ep -> sp.Col > ep.Col
    | ByCol(_, RToL) -> fun sp ep -> sp.Col < ep.Col

let getNextCell (st: Subtable) : (CellPosition -> CellPosition) =
    match st.Order with
    | Unspecified
    | ByRow(LToR, TToB) ->
        fun p ->
            if p.Col < st.Right then
                { Row = p.Row; Col = p.Col + 1 }
            else
                { Row = p.Row + 1; Col = st.Left }
    | ByRow(LToR, BToT) ->
        fun p ->
            if p.Col < st.Right then
                { Row = p.Row; Col = p.Col + 1 }
            else
                { Row = p.Row - 1; Col = st.Left }
    | ByRow(RToL, TToB) ->
        fun p ->
            if p.Col > st.Left then
                { Row = p.Row; Col = p.Col - 1 }
            else
                { Row = p.Row + 1; Col = st.Right }
    | ByRow(RToL, BToT) ->
        fun p ->
            if p.Col > st.Left then
                { Row = p.Row; Col = p.Col - 1 }
            else
                { Row = p.Row - 1; Col = st.Right }
    | ByCol(TToB, LToR) ->
        fun p ->
            if p.Row < st.Bottom then
                { Row = p.Row + 1; Col = p.Col }
            else
                { Row = st.Top; Col = p.Col + 1 }
    | ByCol(TToB, RToL) ->
        fun p ->
            if p.Row < st.Bottom then
                { Row = p.Row + 1; Col = p.Col }
            else
                { Row = st.Top; Col = p.Col - 1 }
    | ByCol(BToT, LToR) ->
        fun p ->
            if p.Row > st.Top then
                { Row = p.Row - 1; Col = p.Col }
            else
                { Row = st.Bottom; Col = p.Col + 1 }
    | ByCol(BToT, RToL) ->
        fun p ->
            if p.Row > st.Top then
                { Row = p.Row - 1; Col = p.Col }
            else
                { Row = st.Bottom; Col = p.Col - 1 }

let getStartPos (st: Subtable) : CellPosition =
    match st.Order with
    | Unspecified
    | ByRow(LToR, TToB) -> { Row = st.Top; Col = st.Left }
    | ByRow(LToR, BToT) -> { Row = st.Bottom; Col = st.Left }
    | ByRow(RToL, TToB) -> { Row = st.Top; Col = st.Right }
    | ByRow(RToL, BToT) -> { Row = st.Bottom; Col = st.Right }
    | ByCol(TToB, LToR) -> { Row = st.Top; Col = st.Left }
    | ByCol(TToB, RToL) -> { Row = st.Top; Col = st.Right }
    | ByCol(BToT, LToR) -> { Row = st.Bottom; Col = st.Left }
    | ByCol(BToT, RToL) -> { Row = st.Bottom; Col = st.Right }

let getEndPos (st: Subtable) : CellPosition =
    match st.Order with
    | Unspecified
    | ByRow(LToR, TToB) -> { Row = st.Bottom; Col = st.Right }
    | ByRow(LToR, BToT) -> { Row = st.Top; Col = st.Right }
    | ByRow(RToL, TToB) -> { Row = st.Bottom; Col = st.Left }
    | ByRow(RToL, BToT) -> { Row = st.Top; Col = st.Left }
    | ByCol(TToB, LToR) -> { Row = st.Bottom; Col = st.Right }
    | ByCol(TToB, RToL) -> { Row = st.Bottom; Col = st.Left }
    | ByCol(BToT, LToR) -> { Row = st.Top; Col = st.Right }
    | ByCol(BToT, RToL) -> { Row = st.Top; Col = st.Left }

let updateSubtable (t: Table) (st: Subtable) (f: CellPosition -> CellValue option) : Table =
    updateCells (getNextCell st) (getStopCond st) t (getStartPos st) (getEndPos st) f

let removeSubtable (t: Table) (st: Subtable) : Table =
    removeCells (getNextCell st) (getStopCond st) t (getStartPos st) (getEndPos st)

let moveSubtable (t: Table) (st: Subtable) (op: CellPosition) (np: CellPosition) : Table =
    let rowOff = np.Row - op.Row
    let colOff = np.Col - op.Col

    let f =
        fun p ->
            getCell
                t
                { Row = p.Row - rowOff
                  Col = p.Col - colOff }

    let nst =
        { Top = st.Top + rowOff
          Bottom = st.Bottom + rowOff
          Right = st.Right + colOff
          Left = st.Left + colOff
          Order = st.Order }

    updateCells (getNextCell nst) (getStopCond nst) (removeSubtable t st) (getStartPos nst) (getEndPos nst) f
