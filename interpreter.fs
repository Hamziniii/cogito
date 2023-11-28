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
    | Eq of Exp * Exp
    | Str of string
    | Cell of Exp * Exp
    | Deref of Exp
    | Range of Exp * Exp * table.IterOrder
    | Array of table.CellVal option array array

type Cmd =
    | Assign of Ident * Exp
    | Seq of Cmd * Cmd
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
    | CellVal of table.CellPos
    | RangeVal of table.Range
    | ArrayVal of table.CellVal option array array

type Env = Map<Ident, Value>

type Config = Cmd * Env * table.Table

(* Helpers *)

let cellPosToExp (p: table.CellPos) : Exp = Cell(Int p.Row, Int p.Col)

let cellVarToPos (x: Ident) (r: Env) : table.CellPos =
    match Map.find x r with
    | CellVal p -> p

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
    | Eq(e1, e2) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(IntVal i1), Some(IntVal i2) -> Some(BoolVal(i1 = i2))
        | Some(BoolVal b1), Some(BoolVal b2) -> Some(BoolVal(b1 = b2))
        | Some(StrVal s1), Some(StrVal s2) -> Some(BoolVal(s1 = s2))
        | Some(CellVal c1), Some(CellVal c2) -> Some(BoolVal(c1 = c2))
        | Some(RangeVal g1), Some(RangeVal g2) -> Some(BoolVal(g1 = g2))
        | _, _ -> None
    | Str s -> Some(StrVal s)
    | Cell(e1, e2) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(IntVal i1), Some(IntVal i2) -> Some(CellVal { Row = i1; Col = i2 })
        | _ -> None
    | Deref e ->
        match evalExp e r t with
        | Some(CellVal p) ->
            match table.getCell t p with
            | Some(table.CellInt i) -> Some(IntVal i)
            | Some(table.CellStr s) -> Some(StrVal s)
            | _ -> None
        | Some(RangeVal g) -> Some(ArrayVal(table.rangeToArray t g))
        | _ -> None
    | Range(e1, e2, o) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(CellVal p1), Some(CellVal p2) -> Some(RangeVal(table.cellsToRange p1 p2 o))
        | _ -> None
    | Array a -> Some(ArrayVal a)

let rec stepCmd ((c, r, t): Config) : Config option =
    match c with
    | Assign(x, e) ->
        match evalExp e r t with
        | Some v -> Some(Skip, Map.add x v r, t)
        | _ -> None
    | Seq(Skip, c2) -> Some(c2, r, t)
    | Seq(c1, c2) ->
        match stepCmd (c1, r, t) with
        | Some(c1', r', t') -> Some(Seq(c1', c2), r', t')
        | _ -> None
    | Skip -> None
    | If(e, c1, c2) ->
        match evalExp e r t with
        | Some(BoolVal true) -> Some(c1, r, t)
        | Some(BoolVal false) -> Some(c2, r, t)
        | _ -> None
    | While(e, b) -> Some(If(e, Seq(b, While(e, b)), Skip), r, t)
    | Fill(e, v) ->
        match evalExp e r t, evalExp v r t with
        | Some(CellVal p), Some(IntVal i) -> Some(Skip, r, table.updateCell t p (table.CellInt i))
        | Some(CellVal p), Some(StrVal s) -> Some(Skip, r, table.updateCell t p (table.CellStr s))
        | Some(RangeVal g), Some(IntVal i) -> Some(Skip, r, table.updateRange t g (fun p -> Some(table.CellInt i)))
        | Some(RangeVal g), Some(StrVal s) -> Some(Skip, r, table.updateRange t g (fun p -> Some(table.CellStr s)))
        | Some(RangeVal g), Some(ArrayVal a) ->
            if a.Length = (g.Bottom - g.Top + 1) && a[0].Length = (g.Right - g.Left + 1) then
                Some(Skip, r, table.updateRange t g (fun p -> a[p.Row - g.Top][p.Col - g.Left]))
            else
                None
        | _ -> None
    | Delete e ->
        match evalExp e r t with
        | Some(CellVal p) -> Some(Skip, r, table.removeCell t p)
        | Some(RangeVal g) -> Some(Skip, r, table.removeRange t g)
        | _ -> None
    | Load f ->
        match file.loadTable f with
        | Some nt -> Some(Skip, r, nt)
        | None -> None
    | Store f ->
        match file.storeTable f t with
        | true -> Some(Skip, r, t)
        | false -> None
    | ForRange(x, e, b) ->
        match evalExp e r t with
        | Some(RangeVal g) ->
            Some(
                Seq(
                    Assign(x, cellPosToExp (table.getStartPos g)),
                    While(
                        Not(Eq(Var x, cellPosToExp (table.getNextCell g (table.getEndPos g)))),
                        Seq(b, Assign(x, cellPosToExp (table.getNextCell g (cellVarToPos x r))))
                    )
                ),
                r,
                t
            )
        | _ -> None

let rec runConfig (cfg: Config) : Config =
    match stepCmd cfg with
    | Some cfg' -> runConfig cfg'
    | None -> cfg

let rec runProg (c: Cmd) : Config =
    runConfig (c, Map.empty, table.newTable)
