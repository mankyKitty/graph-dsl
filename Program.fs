module Program

open FSharp.Json

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

// let encodeCompOp (i : bool, cmp : CompOp) =
//     let (t, v) = match cmp with
//     | EqualTo (e, p) ->
//         let encE = jstring (match e with
//                             | GraphVertex -> "vertex"
//                             | GraphEdge -> "edge")
//         let encP = (match p with
//                     | Tag s -> jobject ["tag", jstring s]
//                     | Value v -> jobject ["value", jint v])
//         ("equalTo", jobject ["property", encE, "args", encP])
//     | ValueLessThan v -> ("valLessThan", jint v)
//     | ValueGreaterThan v -> ("valGreaterThan", jint v)
//     encode i <| jobject [ "comparison", jstring t
//                           "args", v
//                           ]

// let encodeMoveOp (i : bool, m : MoveOp) =
//     let (act, v) = match m with
//     | ToVertex t -> ("toVertex", jstring t)
//     | AlongEdge t -> ("alongEdge", jstring t)
//     | FirstEdge c -> ("firstEdgeMatching", encodeCompOp i c)
//     | FirstVertex c -> ("firstVertexMatching", encodeCompOp i c)
//     encode i <| jobject [
//       "action", jstring act
//       "actionArgs", v
//     ]

[<EntryPoint>]
let main argv =
    let mv = ToVertex "camels"
    let s = Json.serialize mv
    printfn "%s" s

