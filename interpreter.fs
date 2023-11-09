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
// | Cell of table.CellValue
// | Range of table.Subtable

type Cmd =
    | Assign of Ident * Exp
    | Seq of Cmd * Cmd
    | Skip
    | If of Exp * Cmd * Cmd
    | While of Exp * Cmd

type Value =
    | IntVal of int
    | BoolVal of bool
// | CellVal of table.CellValue
// | RangeVal of table.Subtable

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

let rec runConfig (cfg: Config) : Config =
    match stepCmd cfg with
    | Some cfg' -> runConfig cfg'
    | None -> cfg

let rec runProg (c: Cmd) : Config =
    runConfig (c, Map.empty, table.newTable)
