module Program

open FSharp.Json

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
// Added by n7581769.
open Suave.Writers

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

// Types added by n7581769.
// A record type for storing an edge to be converted to JSON.
type MiniJsonEdge = {
    Start : Vert
    End : Vert
    Tag : string
}

// A record type with a list of the above edge types to show what edges are
// connected to a vertex.
type ConnectedEdges = {
    Edges : MiniJsonEdge list
}

// Version of the above that includes the source vertex, used to create a graph
// in a visualisation.
type MiniGraph = {
    Vertex : Vert
    Edges : MiniJsonEdge list
}


// Data used in a MetadataSearch move request.
type Query =
    { Property: string
      Value: string }

// Used in the below type to specify whether to use an AND or OR operation on
// the given queries.
type LogicalOperation =
    | AND
    | OR

// Data used in a MetadataSearchMulti move request.
type MultiQuery = {
    Operation: LogicalOperation
    Queries: Query list
}

// A simple set of metadata to use for the example.
type ExampleInfo = {
    Id : string;
    Name : string;
    Synonyms : string list;
}

// MiniGraph type that allows for metadata being sent with it.
(*type MiniGraphMetadata = {
    Vertex : Vert
    Edges : MiniJsonEdge list
    Metadata : ExampleInfo list
}*)

// MiniGraph type that allows for zipper history being sent with it.
type MiniGraphMetadata = {
    Vertex : Vert
    Edges : MiniJsonEdge list
    History : Move<Vert, TaggedEdge<Vert,string>> list
}

// Added by n7581769.
// Creates a example set of metadata for the example graph.
let GenerateExampleMetadata () =
    [{
        Id = "0";
        Name = "zero";
        Synonyms = ["Starting vertex"; "Zero"]
    };
    {
        Id = "1";
        Name = "one";
        Synonyms = ["First child vertex"; "One"]
    };
    {
        Id = "2";
        Name = "two";
        Synonyms = ["Second child vertex"; "Two"]
    };
    {
        Id = "3";
        Name = "three";
        Synonyms = ["Third child vertex"; "Three"]
    };
    {
        Id = "42";
        Name = "forty-two";
        Synonyms = ["The Answer to the Ultimate Question of Life, the Universe, and Everything"; "Forty-two"]
    }]

// Added by n7581769.
// Checks if the supplied query makes a match in the supplied metadata row.
let SearchMetadata (row : ExampleInfo) query =

    // Check which property is being queried and then check if that property's
    // value contains the query value. For properties that are a list, check
    // each item in that list.
    match query.Property with
        | "Id" -> if row.Id.Contains(query.Value) then true else false
        | "Name" -> if row.Name.Contains(query.Value) then true else false
        | "Synonyms" -> if (not (List.tryFind (fun (item: string) -> item.Contains(query.Value)) row.Synonyms = None)) then true else false
        | _ -> false

// Added by n7581769.
// Attempts to move to a vertex meeting the specified query conditions.
// Only accepts a single query that specifies a property to search on and a
// value that that property contains.
let findFirstWithMetadata((graph : BidirectionalGraph<Vert, TaggedEdge<Vert,string>>), zipper, (query : Query)) =

        // Get the metadata for the graph.
        let metadata = GenerateExampleMetadata()

        // Create the function to test for true or false on a vertex.
        let filterQuery (vertex : Vert) =
            // First, try to find a matching row in the metadata.
            match List.tryFind (fun (row : ExampleInfo) ->
            row.Name.ToLower().Equals(vertex.Tag.ToLower())) metadata with
            // If a metadata match was found, check the metadata for a query
            // match.
            | Some (row) -> SearchMetadata row query
            // Otherwise return false.
            | None -> false

        // Call the movement function for a matching vertex.
        moveAlongFirstMatchingVertex(graph, !zipper, filterQuery)

// Added by n7581769.
// Attempts to move to a vertex meeting the specified query conditions.
// Accepts a list of queries (consisting of the property and the value they
// should contain) and the logical operator to use with those queries.
let findFirstWithMetadataMulti((graph : BidirectionalGraph<Vert, TaggedEdge<Vert,string>>), zipper, (queryList : MultiQuery)) =

    // If there is less than two queries, or the logical operation given isn't
    // AND or OR, end the function with None.
    if (queryList.Queries.Length < 2 || (not (queryList.Operation.Equals(AND)) && not (queryList.Operation.Equals(OR)))) then
        None
    else

        // Get the metadata for the graph.
        let metadata = GenerateExampleMetadata()

        // Create the function to test for true or false on a vertex.
        let filterQuery (vertex : Vert) =
            // First, try to find a matching row in the metadata.
            match List.tryFind (fun (row : ExampleInfo) ->
            row.Name.ToLower().Equals(vertex.Tag.ToLower())) metadata with
            // If a metadata match was found, check the metadata for a query
            // match.
            | Some (row) ->
                // Create a list of true/false based on each query and if there
                // is a match.
                let matches = List.map (SearchMetadata row) queryList.Queries
                // If the operation is AND, return true if all queries matched.
                if (queryList.Operation.Equals(AND) && not (List.contains false matches)) then true
                // If the operation is OR, return true if at least one query
                // matched.
                elif (queryList.Operation.Equals(OR) && List.contains true matches) then true
                // Otherwise return false.
                else false
            // Otherwise return false.
            | None -> false

        // Call the movement function for a matching vertex.
        moveAlongFirstMatchingVertex(graph, !zipper, filterQuery)

// Our boundary types are different to the internal zipper types so there isn't conceptual leakage between the two.
[<JsonUnion(Mode = UnionMode.CaseKeyAsFieldValue, CaseKeyField="moveOp", CaseValueField="moveInputs")>]
type MoveOp =
    | ToVertex of Vert      // Explicit tag name of the target vertex
    | AlongEdge of string   // Explicit tag name of the edge to move along.
    | FirstEdge of string   // Move along the first edge matching the given selection options
    | FirstVertex of CompOp // Try to move to the first vertex connected to the current vertex matching the given comparison
    | Back
    // MoveOps added by n7581769.
    | MetadataSearch of Query
    | MetadataSearchMulti of MultiQuery

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

    // Message changed by n7581769.
    printfn "Received move operation: %s" (moveOp.ToString())
    let moveRes = match moveOp with
                    | ToVertex vval -> moveToVertex (g, !z, vval)
                    | AlongEdge eege -> moveAlongEdge (g, !z, newEdge(newVert("dummy",-1),newVert("dummy",-2), eege))
                    | FirstEdge s -> moveAlongFirstMatchingEdge(g, !z, (fun ve -> ve.Tag = s))
                    | FirstVertex co -> moveAlongFirstMatchingVertex(g, !z, mkCompOp co)
                    | Back -> Some(moveBack !z)
                    // MoveOps added by n7581769.
                    | MetadataSearch query -> findFirstWithMetadata(g, z, query)
                    | MetadataSearchMulti queryList -> findFirstWithMetadataMulti(g, z, queryList)

    match moveRes with
        // Message changed by n7581769.
        | None -> NOT_FOUND "No valid node to move to."
        | Some r ->
            z := r
            OK (Json.serialize (!z).Cursor)

let whereami z _rq = OK (Json.serialize (!z).Cursor)

// Routes added by n7581769.
// Returns all edges connected to the current vertex.
let connectedRoute (graph : BidirectionalGraph<Vert, TaggedEdge<Vert,string>>) zipper _request =
    //printfn "Received wheretogo operation."
    let edges = graph.OutEdges((!zipper).Cursor)
    let edgesMap = Seq.map (fun (item : TaggedEdge<Vert,string>) -> { Start = item.Source; End = item.Target; Tag = item.Tag }) edges
    OK (Json.serialize { Edges = Seq.toList edgesMap })

// Returns the current vertex and all edges connected to it.
let getGraphRoute (graph : BidirectionalGraph<Vert, TaggedEdge<Vert,string>>) zipper _request =
    printfn "Received getGraph operation."
    let currentVertex = (!zipper).Cursor
    let edges = Seq.filter (fun (item : TaggedEdge<Vert,string>) -> item.Source.Equals(currentVertex) || item.Target.Equals(currentVertex)) graph.Edges
    let edgesMap = Seq.map (fun (item : TaggedEdge<Vert,string>) -> { Start = item.Source; End = item.Target; Tag = item.Tag }) edges
    (*let metadataSubset = Seq.filter (fun (item: ExampleInfo) -> 
        match Seq.tryFind (fun (edge : TaggedEdge<Vert,string>) -> edge.Source.Tag.Equals(item.Name) || edge.Target.Tag.Equals(item.Name)) edges with
            | Some (row) -> true
            | None -> false) (GenerateExampleMetadata())*)
    OK (Json.serialize { Vertex = (!zipper).Cursor; Edges = Seq.toList edgesMap(*; Metadata = Seq.toList metadataSubset*); History = (!zipper).History })

// Added by n7581769.
//https://stackoverflow.com/questions/44359375/allow-multiple-headers-with-cors-in-suave
let setCORSHeaders =
    addHeader  "Access-Control-Allow-Origin" "*"
    >=> addHeader "Access-Control-Allow-Headers" "content-type"

// Added by n7581769.
// Returns the graph's metadata.
let getMetadataRoute _request =
    printfn "Received getMetadata operation."
    OK (Json.serialize (GenerateExampleMetadata()))

// CORS handlers added by n7581769.
let app g z =
    choose
      [ POST >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/move" >=> request (f g z) ] )
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/whereami" >=> request (whereami z) ] ) //] )
        // Routes added by n7581769.
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/wheretogo" >=> request (connectedRoute g z) ] )
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/getGraph" >=> request (getGraphRoute g z) ] )
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/getMetadata" >=> request (getMetadataRoute)] ) ]

let addEdgeOrDie (g: AppGraph, e: TaggedEdge<Vert,string>) =
    if g.AddEdge(e) then () else failwith "Failed to add edge"

let addVertexOrDie2 (g:AppGraph) (v:Vert) =
    if g.AddVertex(v) then () else failwith "Failed to add vertex"

let addVertexOrDie (g: AppGraph, v: Vert) =
    if g.AddVertex(v) then () else failwith "Failed to add vertex"

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

// Example curl commands (Linux):
// ToVertex:
// $ curl -X POST -vvv --data '{"moveOp":"ToVertex","moveInputs":{"Tag":"one","Value":1}}' http://localhost:8080/move
// Back:
// $ curl -X POST -vvv --data '{"moveOp":"Back","moveInputs":[]}' http://localhost:8080/move
// MetadataSearch:
// $ curl -X POST -vvv --data '{"moveOp":"MetadataSearch","moveInputs":{"Property":"Synonyms","Value":"child"}}' http://localhost:8080/move
// MetadataSearchMulti (AND):
// $ curl -X POST -vvv --data '{"moveOp":"MetadataSearchMulti","moveInputs":{"Operation":"AND","Queries":[{"Property":"Synonyms","Value":"child"},{"Property":"Name","Value":"two"}]}}' http://localhost:8080/move
// MetadataSearchMulti (OR):
// $ curl -X POST -vvv --data '{"moveOp":"MetadataSearchMulti","moveInputs":{"Operation":"OR","Queries":[{"Property":"Synonyms","Value":"child"},{"Property":"Name","Value":"two"}]}}' http://localhost:8080/move

// Example curl commands (Windows):
// ToVertex:
// curl -X POST -vvv --data {\"moveOp\":\"ToVertex\",\"moveInputs\":{\"Tag\":\"one\",\"Value\":1}} http://localhost:8080/move
// Back:
// curl -X POST -vvv --data {\"moveOp\":\"Back\",\"moveInputs\":[]} http://localhost:8080/move
// MetadataSearch:
// curl -X POST -vvv --data {\"moveOp\":\"MetadataSearch\",\"moveInputs\":{\"Property\":\"Synonyms\",\"Value\":\"child\"}} http://localhost:8080/move
// MetadataSearchMulti (AND):
// curl -X POST -vvv --data {\"moveOp\":\"MetadataSearchMulti\",\"moveInputs\":{\"Operation\":\"AND\",\"Queries\":[{\"Property\":\"Synonyms\",\"Value\":\"child\"},{\"Property\":\"Name\",\"Value\":\"two\"}]}} http://localhost:8080/move
// MetadataSearchMulti (OR):
// curl -X POST -vvv --data {\"moveOp\":\"MetadataSearchMulti\",\"moveInputs\":{\"Operation\":\"OR\",\"Queries\":[{\"Property\":\"Synonyms\",\"Value\":\"child\"},{\"Property\":\"Name\",\"Value\":\"two\"}]}} http://localhost:8080/move
[<EntryPoint>]
let main argv =
    let mutable g = generateFreshGraph
    printfn "Graph Status verticies: %i, edges: %i" (g.VertexCount) (g.EdgeCount)
    let root = newVert("zero", 0)
    printfn "Root JSON: %s" (Json.serialize root)
    let mutable freshZip = ref (createZipper root)
    startWebServer defaultConfig (app g freshZip)
    0
