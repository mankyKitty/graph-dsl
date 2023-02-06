module Program

open FSharp.Json

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful

type ElemSelect =
    | GraphVertex
    | GraphEdge

type Property =
    | Tag of string
    | Value of int

type CompOp =
    | EqualTo of ElemSelect * Property
    | ValueLessThan of int
    | ValueGreaterThan of int

type MoveOp =
    | ToVertex of string    // Explicit tag name of the target vertex
    | AlongEdge of string   // Explicit tag name of the edge to move along.
    | FirstEdge of CompOp   // Move along the first edge matching the given selection options
    | FirstVertex of CompOp // Try to move to the first vertex connected to the current vertex matching the given comparison


// Example for dumping JSON of structures
// let main argv =
//     let mv = ToVertex "camels"
//     let s = Json.serialize mv
//     printfn "%s" s
//     0

let f rq =
    let moveOp =
      rq.rawForm
      |> UTF8.toString
      |> Json.deserialize<MoveOp>

    printfn "We have a MoveOp : %s" <| (match moveOp with
        | ToVertex _a -> "To Vertex"
        | AlongEdge _a -> "Along Edge"
        | FirstEdge _a -> "Along First Matching Edge"
        | FirstVertex _a -> "Along First Matching Vertex")

    OK (Json.serialize moveOp)

let app =
    choose
      [ GET >=> choose
          [ path "/hello" >=> OK "Hello GET" ]
        POST >=> choose
          [ path "/move" >=> request f ] ] // curl -X POST -vv --data "{\"ToVertex\":\"camels\"}" http://localhost:8080/move

[<EntryPoint>]
let main argv =
    startWebServer defaultConfig app
    0
