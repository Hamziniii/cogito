module excel

open FSharp.Stats

let rec cellValToFloat (t: table.Table) (v: table.CellVal option) : float =
    match v with
    | Some(table.CellInt i) -> float i
    | Some(table.CellStr s) -> 0
    | Some(table.CellExpr e) -> evalExcelExpr t e
    | _ -> 0

and evalExcelExpr (t: table.Table) (e: table.ExcelExpr) : float =
    match e with
    | table.SUM l -> aggregateRanges t l |> List.sum
    | table.PRODUCT l -> aggregateRanges t l |> List.fold (*) 1
    | table.AVERAGE l -> aggregateRanges t l |> List.average
    | table.MEDIAN l -> aggregateRanges t l |> List.median
    | table.MODE l -> aggregateRanges t l |> List.countBy id |> List.maxBy snd |> fst
    | table.MAX l -> aggregateRanges t l |> List.max
    | table.MIN l -> aggregateRanges t l |> List.min
    | table.VAR l -> aggregateRanges t l |> Seq.var
    | table.STDEV l -> aggregateRanges t l |> Seq.stDev
    | table.VARP l -> aggregateRanges t l |> Seq.varPopulation
    | table.STDEVP l -> aggregateRanges t l |> Seq.stDevPopulation

and aggregateRanges (t: table.Table) (l: table.Range list) : float list =
    List.map (cellValToFloat t) (table.rangesToList t l)
