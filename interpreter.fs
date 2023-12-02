module interpreter

(* Heavily inspired by Homework 4. *)

(* Syntax *)

type Ident = string

type Exp =
    | Var of Ident
    | Int of int
    | Add of Exp * Exp
    | Sub of Exp * Exp
    | Bool of bool
    | And of Exp * Exp
    | Or of Exp * Exp
    | Not of Exp
    | Gt of Exp * Exp
    | Lt of Exp * Exp
    | Eq of Exp * Exp
    | Str of string
    | Excel of table.ExcelExpr
    | Cell of Exp * Exp
    | View of Exp
    | Deref of Exp
    | Range of Exp * Exp * table.IterOrder
    | Array of table.CellVal option array2d

type Cmd =
    | Assign of Ident * Exp
    | Seq of Cmd list
    | Skip
    | If of Exp * Cmd * Cmd
    | While of Exp * Cmd
    | Fill of Exp * Exp
    | Delete of Exp
    | Load of string
    | Store of string
    | ForRange of Ident * Exp * Cmd

type Value =
    | IntVal of int
    | BoolVal of bool
    | StrVal of string
    | ExcelVal of table.ExcelExpr
    | CellVal of table.CellPos
    | RangeVal of table.Range
    | ArrayVal of table.CellVal option array2d

type Env = Map<Ident, Value>

type Clay =
    { Iterating: bool; Next: table.CellPos }

type Config = Cmd * Env * Clay * table.Table

(* Helpers *)

let cellPosToExp (p: table.CellPos) : Exp = Cell(Int p.Row, Int p.Col)

let viewCell (t: table.Table) (v: table.CellVal option) : table.CellVal option =
    match v with
    | Some(table.CellExpr x) -> Some(table.CellInt(int (excel.evalExcelExpr t x)))
    | _ -> v

let viewTable (t: table.Table) =
    { table.Cells =
        Map.map
            (fun p v ->
                match v with
                | table.CellExpr x -> table.CellInt(int (excel.evalExcelExpr t x))
                | _ -> v)
            t.Cells
      table.Rows = t.Rows
      table.Cols = t.Cols }

(* Semantics *)

let rec evalExp (e: Exp) (r: Env) (t: table.Table) : Value option =
    match e with
    | Var x -> Map.tryFind x r
    | Int i -> Some(IntVal i)
    | Add(e1, e2) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(IntVal i1), Some(IntVal i2) -> Some(IntVal(i1 + i2))
        | _, _ -> None
    | Sub(e1, e2) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(IntVal i1), Some(IntVal i2) -> Some(IntVal(i1 - i2))
        | _, _ -> None
    | Bool b -> Some(BoolVal b)
    | And(e1, e2) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(BoolVal b1), Some(BoolVal b2) -> Some(BoolVal(b1 && b2))
        | _, _ -> None
    | Or(e1, e2) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(BoolVal b1), Some(BoolVal b2) -> Some(BoolVal(b1 || b2))
        | _, _ -> None
    | Not e ->
        match evalExp e r t with
        | Some(BoolVal b) -> Some(BoolVal(not b))
        | _ -> None
    | Gt(e1, e2) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(IntVal i1), Some(IntVal i2) -> Some(BoolVal(i1 > i2))
        | _, _ -> None
    | Lt(e1, e2) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(IntVal i1), Some(IntVal i2) -> Some(BoolVal(i1 < i2))
        | _, _ -> None
    | Eq(e1, e2) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(IntVal i1), Some(IntVal i2) -> Some(BoolVal(i1 = i2))
        | Some(BoolVal b1), Some(BoolVal b2) -> Some(BoolVal(b1 = b2))
        | Some(StrVal s1), Some(StrVal s2) -> Some(BoolVal(s1 = s2))
        | Some(CellVal c1), Some(CellVal c2) -> Some(BoolVal(c1 = c2))
        | Some(RangeVal g1), Some(RangeVal g2) -> Some(BoolVal(g1 = g2))
        | _, _ -> None
    | Str s -> Some(StrVal s)
    | Excel x -> Some(ExcelVal x)
    | Cell(e1, e2) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(IntVal i1), Some(IntVal i2) -> Some(CellVal { Row = i1; Col = i2 })
        | _ -> None
    | View e ->
        match evalExp e r t with
        | Some(CellVal p) ->
            match table.getCell t p with
            | Some(table.CellInt i) -> Some(IntVal i)
            | Some(table.CellStr s) -> Some(StrVal s)
            | Some(table.CellExpr x) -> Some(IntVal(int (excel.evalExcelExpr t x)))
            | _ -> None
        | Some(RangeVal g) -> Some(ArrayVal(Array2D.map (viewCell t) (table.rangeToArray t g)))
        | _ -> None
    | Deref e ->
        match evalExp e r t with
        | Some(CellVal p) ->
            match table.getCell t p with
            | Some(table.CellInt i) -> Some(IntVal i)
            | Some(table.CellStr s) -> Some(StrVal s)
            | Some(table.CellExpr x) -> Some(ExcelVal x)
            | _ -> None
        | Some(RangeVal g) -> Some(ArrayVal(table.rangeToArray t g))
        | _ -> None
    | Range(e1, e2, o) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(CellVal p1), Some(CellVal p2) -> Some(RangeVal(table.cellsToRange p1 p2 o))
        | _ -> None
    | Array a -> Some(ArrayVal a)

let rec stepCmd ((c, r, y, t): Config) : Config option =
    match c with
    | Assign(x, e) ->
        match evalExp e r t with
        | Some v -> Some(Skip, Map.add x v r, y, t)
        | _ -> None
    | Seq(Skip :: c2) -> Some(Seq(c2), r, y, t)
    | Seq(c1 :: c2) ->
        match stepCmd (c1, r, y, t) with
        | Some(c1', r', y', t') -> Some(Seq(c1' :: c2), r', y', t')
        | _ -> None
    | Seq([]) -> Some(Skip, r, y, t)
    | Skip -> None
    | If(e, c1, c2) ->
        match evalExp e r t with
        | Some(BoolVal true) -> Some(c1, r, y, t)
        | Some(BoolVal false) -> Some(c2, r, y, t)
        | _ -> None
    | While(e, b) -> Some(If(e, Seq([ b; While(e, b) ]), Skip), r, y, t)
    | Fill(e, v) ->
        match evalExp e r t, evalExp v r t with
        | Some(CellVal p), Some(IntVal i) -> Some(Skip, r, y, table.updateCell t p (table.CellInt i))
        | Some(CellVal p), Some(StrVal s) -> Some(Skip, r, y, table.updateCell t p (table.CellStr s))
        | Some(CellVal p), Some(ExcelVal x) -> Some(Skip, r, y, table.updateCell t p (table.CellExpr x))
        | Some(RangeVal g), Some(IntVal i) -> Some(Skip, r, y, table.updateRange t g (fun p -> Some(table.CellInt i)))
        | Some(RangeVal g), Some(StrVal s) -> Some(Skip, r, y, table.updateRange t g (fun p -> Some(table.CellStr s)))
        | Some(RangeVal g), Some(ArrayVal a) ->
            if
                Array2D.length1 a = (g.Bottom - g.Top + 1)
                && Array2D.length2 a = (g.Right - g.Left + 1)
            then
                Some(Skip, r, y, table.updateRange t g (fun p -> a[p.Row - g.Top, p.Col - g.Left]))
            else
                None
        | _ -> None
    | Delete e ->
        match evalExp e r t with
        | Some(CellVal p) -> Some(Skip, r, y, table.removeCell t p)
        | Some(RangeVal g) -> Some(Skip, r, y, table.removeRange t g)
        | _ -> None
    | Load f ->
        match file.loadTable f with
        | Some nt -> Some(Skip, r, y, nt)
        | None -> None
    | Store f ->
        match file.storeTable f t with
        | true -> Some(Skip, r, y, t)
        | false -> None
    | ForRange(x, e, b) ->
        match evalExp e r t with
        | Some(RangeVal g) ->
            if not y.Iterating then
                Some(
                    Seq([ Assign(x, cellPosToExp (table.getStartPos g)); b; ForRange(x, e, b) ]),
                    r,
                    { Iterating = true
                      Next = table.getNextCell g (table.getStartPos g) },
                    t
                )
            else if y.Iterating && not ((table.getStopCond g) y.Next) then
                Some(
                    Seq([ Assign(x, cellPosToExp y.Next); b; ForRange(x, e, b) ]),
                    r,
                    { Iterating = true
                      Next = table.getNextCell g y.Next },
                    t
                )
            else
                Some(
                    Skip,
                    r,
                    { Iterating = false
                      Next = { Row = 0; Col = 0 } },
                    t
                )
        | _ -> None

let rec runConfig (cfg: Config) : Config =
    match stepCmd cfg with
    | Some cfg' -> runConfig cfg'
    | None -> cfg

let rec runProg (c: Cmd) : Config =
    runConfig (
        c,
        Map.empty,
        { Iterating = false
          Next = { Row = 0; Col = 0 } },
        table.newTable
    )
