module Program

open FSharp.Json

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors

open QuikGraph
open GraphDSL.Zipper

type Vert =
    { Tag: string
      Value: int }

type AppGraph = BidirectionalGraph<Vert, TaggedEdge<Vert,string>>

type Property =
    | Tag of string
    | Value of int

type CompOp =
    | EqualTo of Property
    | ValueLessThan of int
    | ValueGreaterThan of int

// Our boundary types are different to the internal zipper types so there isn't conceptual leakage between the two.
[<JsonUnion(Mode = UnionMode.CaseKeyAsFieldValue, CaseKeyField="moveOp", CaseValueField="moveInputs")>]
type MoveOp =
    | ToVertex of Vert      // Explicit tag name of the target vertex
    | AlongEdge of string   // Explicit tag name of the edge to move along.
    | FirstEdge of string   // Move along the first edge matching the given selection options
    | FirstVertex of CompOp // Try to move to the first vertex connected to the current vertex matching the given comparison
    | Back

let newEdge (a: Vert, b: Vert, t: string) : TaggedEdge<Vert, string> =
    new TaggedEdge<Vert, string>(a,b,t)

let newVert (tag: string, v: int): Vert =
    { Tag = tag; Value = v }

let mkCompOp (c: CompOp): (Vert -> bool) =
    match c with
        | ValueLessThan i -> fun v -> v.Value < i
        | ValueGreaterThan i -> fun v -> v.Value > i
        | EqualTo (Tag s) -> fun v -> v.Tag = s
        | EqualTo (Value vl) -> fun v -> v.Value = vl

let f g z rq =
    let moveOp =
      rq.rawForm
      |> UTF8.toString
      |> Json.deserialize<MoveOp>

    printfn "[[[[]]]] %s" (moveOp.ToString())
    let moveRes = match moveOp with
                    | ToVertex vval -> moveToVertex (g, !z, vval)
                    | AlongEdge eege -> moveAlongEdge (g, !z, newEdge(newVert("dummy",-1),newVert("dummy",-2), eege))
                    | FirstEdge s -> moveAlongFirstMatchingEdge(g, !z, (fun ve -> ve.Tag = s))
                    | FirstVertex co -> moveAlongFirstMatchingVertex(g, !z, mkCompOp co)
                    | Back -> Some(moveBack !z)

    match moveRes with
        | None -> NOT_FOUND "damn"
        | Some r ->
            z := r
            OK (Json.serialize (!z).Cursor)

let whereami z _rq = OK (Json.serialize (!z).Cursor)

let app g z =
    choose
      [ POST >=> choose
          [ path "/move" >=> request (f g z) ]
        GET >=> choose
          [ path "/whereami" >=> request (whereami z) ] ]

let addEdgeOrDie (g: AppGraph, e: TaggedEdge<Vert,string>) =
    if g.AddEdge(e) then () else failwith "Failed to add edge"

let addVertexOrDie2 (g:AppGraph) (v:Vert) =
    if g.AddVertex(v) then () else failwith "Failed to add edge"

let addVertexOrDie (g: AppGraph, v: Vert) =
    if g.AddVertex(v) then () else failwith "Failed to add edge"

let generateFreshGraph: AppGraph =
    let mutable g = new BidirectionalGraph<Vert, TaggedEdge<Vert,string>> ()
    let vzero = newVert("zero", 0)
    let vone = newVert("one", 1)
    let vtwo = newVert("two", 2)
    let vthree = newVert("three", 3)
    let vfortytwo = newVert("forty-two", 42)

    addVertexOrDie (g, vzero)
    addVertexOrDie (g, vone)
    addVertexOrDie (g, vtwo)
    addVertexOrDie (g, vthree)
    addVertexOrDie (g, vfortytwo)

    addEdgeOrDie (g, (newEdge (vzero, vone, "add_one")))
    addEdgeOrDie (g, (newEdge (vzero, vtwo, "add_two")))
    addEdgeOrDie (g, (newEdge (vtwo, vthree, "two_three")))
    addEdgeOrDie (g, (newEdge (vone, vthree, "one_three")))
    addEdgeOrDie (g, (newEdge (vthree, vfortytwo, "end")))

    g

// $ curl -X POST -vvv --data '{"moveOp":"ToVertex","moveInputs":{"Tag":"one","Value":1}}' http://localhost:8080/move
// $ curl -X POST -vvv --data '{"moveOp":"Back","moveInputs":[]}' http://localhost:8080/move
[<EntryPoint>]
let main argv =
    let mutable g = generateFreshGraph
    printfn "Graph Status verticies: %i, edges: %i" (g.VertexCount) (g.EdgeCount)
    let root = newVert("zero", 0)
    printfn "Root JSON: %s" (Json.serialize root)
    let mutable freshZip = ref (createZipper root)
    startWebServer defaultConfig (app g freshZip)
    0
