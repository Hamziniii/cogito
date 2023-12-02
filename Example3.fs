// IMPORTANT:
// MUST RUN AFTER EXAMPLE 1

module main

open table
open interpreter

let prog =
    Seq(
        [ Load("students.json")  
          Fill(Cell(Int 6, Int 3), Str " = A")
          Fill(Cell(Int 7, Int 3), Str ">= B")
          Fill(Cell(Int 8, Int 3), Str ">= C")
          Fill(Cell(Int 9, Int 3), Str ">= D")
          Fill(Cell(Int 10, Int 3), Str ">= F")

          Fill(Range(Cell(Int 6, Int 4), Cell(Int 10, Int 4), ByCol(TToB, LToR)), Int 0)

          Assign("x", Int 0)
          While(Lt(Var "x", Int 4), Seq([
            ForRange("i", Range(Cell(Int 1, Int 1), Cell(Int 10, Int 1), ByCol(TToB, LToR)), 
              If(Gt(Deref (Var "i"), Sub(Int 89, Mult(Var "x", Int 10))), 
                Fill(Cell(Add(Int 6, Var "x"), Int 4), 
                  Add(Int 1, Deref(Cell(Add(Int 6, Var "x"), Int 4)))),
                Skip))
            Assign("x", Add(Var "x", Int 1))
          ]))
          ForRange("i", Range(Cell(Int 1, Int 1), Cell(Int 10, Int 1), ByCol(TToB, LToR)), Fill(Cell(Add(Int 6, Var "x"), Int 4), Add(Int 1, Deref(Cell(Add(Int 6, Var "x"), Int 4)))))
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
