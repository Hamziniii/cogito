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

type Value =
    | IntVal of int
    | BoolVal of bool
    | StrVal of string
    | CellVal of table.CellPos
    | RangeVal of table.Range
    | ArrayVal of table.CellVal option array array

type Env = Map<Ident, Value>

type Config = Cmd * Env * table.Table

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
    | Eq(e1, e2) ->
        match evalExp e1 r t, evalExp e2 r t with
        | Some(IntVal i1), Some(IntVal i2) -> Some(BoolVal(i1 = i2))
        | Some(BoolVal b1), Some(BoolVal b2) -> Some(BoolVal(b1 = b2))
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
            Some(Skip, r, table.updateRange t g (fun p -> a[p.Row - g.Top][p.Col - g.Left]))
        | _ -> None
    | Delete e ->
        match evalExp e r t with
        | Some(CellVal p) -> Some(Skip, r, table.removeCell t p)
        | Some(RangeVal g) -> Some(Skip, r, table.removeRange t g)
        | _ -> None
    | Load f -> Some(Skip, r, file.loadTable f)
    | Store f ->
        file.storeTable f t
        Some(Skip, r, t)

let rec runConfig (cfg: Config) : Config =
    match stepCmd cfg with
    | Some cfg' -> runConfig cfg'
    | None -> cfg

let rec runProg (c: Cmd) : Config =
    runConfig (c, Map.empty, table.newTable)
