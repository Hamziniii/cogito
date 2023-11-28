module main

open table

let mutable t = table.newTable

t <- table.updateCell t { Row = 3; Col = 3 } (CellInt 9)
t <- table.updateCell t { Row = 3; Col = 4 } (CellInt 12)
t <- table.updateCell t { Row = 4; Col = 3 } (CellInt 12)
t <- table.updateCell t { Row = 4; Col = 4 } (CellInt 16)

table.printTable t

t <-
    table.moveRange
        t
        { Top = 3
          Bottom = 4
          Left = 3
          Right = 4
          Order = Unspecified }
        { Row = 3; Col = 3 }
        { Row = 1; Col = 1 }

table.printTable t
