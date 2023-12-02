module main

open table
open interpreter

let students: CellVal option array2d =
    array2D
        [| [| Some(CellInt 99) |]
           [| Some(CellInt 81) |]
           [| Some(CellInt 89) |]
           [| Some(CellInt 12) |]
           [| Some(CellInt 66) |]
           [| Some(CellInt 45) |]
           [| Some(CellInt 94) |]
           [| Some(CellInt 5) |]
           [| Some(CellInt 13) |]
           [| Some(CellInt 100) |] |]

let range =
    { table.Top = 1
      table.Bottom = 10
      table.Left = 1
      table.Right = 1
      table.Order = ByCol(TToB, LToR) }

let prog =
    Seq(
        [ Assign("students", Array students)
          Assign("range", Range(Cell(Int 1, Int 1), Cell(Int 10, Int 1), ByCol(TToB, LToR)))
          Fill(Var "range", Var "students")
          Fill(Range(Cell(Int 1, Int 3), Cell(Int 4, Int 4), ByRow(LToR, TToB)), Array stats) 
        ]
    )

let (c, r, y, t) = runProg prog

printfn "Cmd: %A" c
printfn "Env: %A" r
printfn "Clay: %A" y
printfn "Table: %A" t

printfn "\nRaw Table:\n"
table.printTable t

printfn "\nEvaluated:\n"
table.printTable (interpreter.viewTable t)
