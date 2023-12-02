// IMPORTANT:
// MUST RUN AFTER EXAMPLE 1

module main

open table
open interpreter

let range =
    { table.Top = 1
      table.Bottom = 1
      table.Left = 1
      table.Right = 10
      table.Order = ByRow(LToR, TToB) }

let prog =
    Seq(
        [ Fill(Cell(Int 1, Int 1), Int 5)
          Assign("x", Int 1)
          ForRange("i", Range(Cell(Int 1, Int 2), Cell(Int 1, Int 10), ByRow(LToR, TToB)), Seq([
            Fill(Var "i", Add(Deref(Cell(Int 1, Var "x")), Int 1))
            Assign("x", Add(Var "x", Int 1))
          ]))

          Fill(Range(Cell(Int 2, Int 1), Cell(Int 10, Int 2), ByRow(LToR, TToB)), Array (array2D [|
              [| Some(CellStr "Mean"); Some(CellExpr(AVERAGE [ range ])) |]
              [| Some(CellStr "MEDIAN"); Some(CellExpr(MEDIAN [ range ])) |]
              [| Some(CellStr "MODE"); Some(CellExpr(MODE [ range ])) |]
              [| Some(CellStr "STDEVP"); Some(CellExpr(STDEVP [ range ])) |]
              [| Some(CellStr "STDEV"); Some(CellExpr(STDEV [ range ])) |]
              [| Some(CellStr "VARP"); Some(CellExpr(VARP [ range ])) |]
              [| Some(CellStr "MIN"); Some(CellExpr(MIN [ range ])) |]
              [| Some(CellStr "MAX"); Some(CellExpr(MAX [ range ])) |]
              [| Some(CellStr "SUM"); Some(CellExpr(SUM [ range ])) |]
          |]))

          Fill(Range(Cell(Int 2, Int 4), Cell(Int 10, Int 4), ByCol(TToB, LToR)), Str "DEREF: " )
          Assign("x", Int 2)
          ForRange("i", Range(Cell(Int 2, Int 5), Cell(Int 10, Int 5), ByCol(TToB, LToR)), Seq([
            Fill(Var "i", Deref(Cell(Var "x", Int 2)))
            Assign("x", Add(Var "x", Int 1))
          ]))

          Fill(Range(Cell(Int 2, Int 6), Cell(Int 10, Int 6), ByCol(TToB, LToR)), Str "VIEW: " )
          Fill(Range(Cell(Int 2, Int 7), Cell(Int 10, Int 7), ByCol(TToB, LToR)), View(Range(Cell(Int 2, Int 2), Cell(Int 10, Int 2), ByCol(TToB, LToR))))
        ]
    )

let (c, r, y, t) = runProg prog

// printfn "Cmd: %A" c
// printfn "Env: %A" r
// printfn "Clay: %A" y
// printfn "Table: %A" t

// printfn "\nRaw Table:\n"
// table.printTable t

printfn "\nEvaluated:\n"
table.printTable (interpreter.viewTable t)
