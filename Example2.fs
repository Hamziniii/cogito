module main

open table
open interpreter

let prog =
    Seq(
        [ Assign("x", Int 1)
          ForRange("i", Range(Cell(Int 1, Int 1), Cell(Int 10, Int 10), ByRow(LToR, TToB)), Seq([ 
            Fill(Var "i", Var "x")
            Assign("x", Add(Var "x", Int 1))
          ])) 
          Fill(Range(Cell(Int 11, Int 1), Cell(Int 11, Int 10), ByRow(LToR, TToB)), Str "-")
          Assign("x", Int 1)
          ForRange("i", Range(Cell(Int 12, Int 1), Cell(Int 21, Int 10), ByCol(TToB, LToR)), Seq([ 
            Fill(Var "i", Var "x")
            Assign("x", Add(Var "x", Int 1))
          ]))
        ]
    )
//Fill(Var "i", Int 1)
let (c, r, y, t) = runProg prog

// printfn "\nRaw Table:\n"
// table.printTable t

printfn "\nEvaluated:\n"
table.printTable (interpreter.viewTable t)