module file

open MBrace.FsPickler.Json
open System.IO

let jsonSerializer = FsPickler.CreateJsonSerializer(indent = true)

let loadTable (f: string) : table.Table option =
    try
        Some(jsonSerializer.UnPickleOfString(File.ReadAllText f))
    with _ ->
        None

let storeTable (f: string) (t: table.Table) : bool =
    try
        File.WriteAllText(f, jsonSerializer.PickleToString t)
        true
    with _ ->
        false
