module GraphDSL.Server

// Package for parsing and serialising JSON objects.
open FSharp.Json

// Suave package and its modules for running a server.
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open Suave.Writers

// GraphDSL types and functions used in this file.
open GraphDSL.Zipper
open GraphDSL.Scoring
open GraphDSL.Types

// ----------------------------------------------------------------------------
// ------ Types ------
// ----------------------------------------------------------------------------

// Type to specify which property of a vertex to test with a comparison
// operation.
type Property =
    | Tag of string
    | Value of int

// Type to define possible comparison operations.
// EqualTo can test against both vertex tags and values; ValueLessThan and
// ValueGreaterThan can only test against vertex values.
type CompOp =
    | EqualTo of Property
    | ValueLessThan of int
    | ValueGreaterThan of int

// Data used in a MetadataSearch move request.
type Query =
    { Property: string
      SearchTerm: string }

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

// Our boundary types are different to the internal zipper types so there isn't conceptual leakage between the two.
[<JsonUnion(Mode = UnionMode.CaseKeyAsFieldValue, CaseKeyField="moveOp", CaseValueField="moveInputs")>]
type MoveOp =
    // Standard operations:
    | ToVertex of Vert      // Move to the vertex that matches the given tag and value.
    | AlongEdge of string   // Move to the vertex that is the destination of the given edge.
    | FirstEdge of string   // Move along the first edge matching the given selection options.
    | FirstVertex of CompOp // Move to the first vertex connected to the current vertex matching the given comparison.
    // History operations:
    | Back                  // Move to the vertex that was before the last move in the zipper's history. Does not erase the history.
    | Forward                           // Move to the vertex that was after the next move in the zipper's history. Does not erase the history.
    | GoToHistory of int                // Move to the vertex after the specified step in the zipper's history (or the starting vertex if given 0). Does not erase the history.
    // Special operations:
    | ForceToVertex of Vert             // Move to the vertex that matches the given tag and value. Can move across backwards edges.
    | MetadataSearch of Query           // Move to the first vertex whose metadata matches the given search query.
    | MetadataSearchMulti of MultiQuery // Move to the first vertex whose metadata matches the given multiple search queries (either all of them via AND, or at least one via OR).
    | NextMostConnected of bool         // Move to the vertex with the most outgoing connections connected to the current cursor (that has not already been visited, if true is sent).
    | NextHighestQueryScore of Query    // Move to the vertex whose edge connecting it to the current vertex has the highest (weighted) score, based on a given query.
    | NextHighestScore of bool          // Move to the vertex on the other side of the edge with the highest score connected to the current cursor (that has not already been visited, if true is sent).

// ----------------------------------------------------------------------------
// ------ Graph movement functions ------
// ----------------------------------------------------------------------------

// Comparison operations for matching to vertices. Can work against the value
// or the tag. Values can be tested to be equal to, greater than or less than
// the given value, and tags can be tested to be equal to the given value.
let mkCompOp (comparison: CompOp): (Vert -> bool) =
    match comparison with
        | ValueLessThan i -> fun v -> v.Value < i
        | ValueGreaterThan i -> fun v -> v.Value > i
        | EqualTo (Tag s) -> fun v -> v.Tag = s
        | EqualTo (Value vl) -> fun v -> v.Value = vl

// Function for checking if a vertex is in the history; used in the
// function below.
let checkInHistory (zipper : Zipper<Vert, AppEdge> ref) (vertex : Vert) =
    match Seq.tryFind(fun historyVert -> vertex.Equals(historyVert)) (last ((zipper.Value).HistoryIndex + 1) (zipper.Value).VertHistory) with
    | Some (result) -> true
    | None -> false

// Attempts to move to a previously unvisited vertex based upon the highest
// edge score from the current vertex.
let findNextHighestScore unvisited (graph : AppGraph) (zipper : Zipper<Vert, AppEdge> ref) =

    // Sort the list of edges connected to the current vertex in descending
    // order of their edge score.
    let sortedEdges = Seq.sortByDescending (fun (edge : AppEdge) -> edge.Value) (graph.OutEdges((zipper.Value).Cursor))
#if LOGGING && VERBOSE
    Seq.iter (fun (edge : AppEdge) -> printfn "Score of the edge to %s has the value %f" edge.Target.Tag edge.Value) sortedEdges
    Seq.iter (fun (vertex : Vert) -> printfn "Vertex %s has been visited" vertex.Tag) (last ((zipper.Value).HistoryIndex + 1) (zipper.Value).VertHistory)
#endif

    // Function to check for the first edge in the sorted list. This will be
    // the edge that is connected to the next most connected vertex.
    let targetEdge =
        if (unvisited) then
            // Find a vertex that's not already in the history.
            // Also check that the score does not equal zero.
            Seq.tryFind (fun (sortedEdge : AppEdge) -> not (checkInHistory zipper sortedEdge.Target) && sortedEdge.Value > 0.0) sortedEdges
        else
            Seq.tryFind (fun (sortedEdge : AppEdge) -> sortedEdge.Value > 0.0) sortedEdges

    // Create the function to test for true or false on an edge.
    let filterQuery (currentEdge : AppEdge) =
        match targetEdge with
        | Some edge -> currentEdge.Equals(edge)
        | None -> false

    // Call the movement function for a matching edge.
    let result = moveAlongFirstMatchingEdge(graph, zipper.Value, filterQuery)

#if LOGGING && VERBOSE
    match result with
        | Some r -> printfn "Successfully moved to %s." r.Cursor.Tag
        | None -> printfn "No matching edge to move along."
#endif

    result

// Attempts to move to the connected vertex that has the most outgoing
// connections.
let findNextMostConnected unvisited (graph : AppGraph) (zipper : Zipper<Vert, AppEdge> ref) =

    let currentCursor = (zipper.Value).Cursor

    // Calculate the edge values for this operation.
    let tempGraph = Basic.calculateEdgeValues_ConnectionsSingle currentCursor graph

    // Call the function for moving to the vertex connected to the edge with
    // the highest score (which hasn't been visited yet).
    findNextHighestScore unvisited tempGraph zipper

// ------ Functions for moving based on graph metadata. ------

// Attempts to move to a connected vertex that is "most relevant" to a
// particular query. Essentially, a weighted score is calculated for each
// outgoing edge for four steps away from the initial vertex, with higher
// weightings for closer vertices to the starting point (that fulfils the
// query condition), and the zipper will travel down the edge that has the
// greatest score.
let findNextHighestQueryScore(graph : AppGraph) (zipper : Zipper<Vert, AppEdge> ref) condition =

    let currentCursor = (zipper.Value).Cursor

    // Calculate the edge values for this operation.
#if USE_DFS
    let tempGraph = WeightedDFS.calculateEdgeValues_ConditionSingle condition ScoringValues.NUM_SCORE_STEPS currentCursor graph
#else
    let tempGraph = WeightedBFS.calculateEdgeValues_ConditionSingle condition ScoringValues.NUM_SCORE_STEPS currentCursor graph
#endif

    // Call the function for moving to the vertex connected to the edge with
    // the highest score.
    findNextHighestScore false tempGraph zipper

// Checks if the supplied query makes a match in the supplied metadata row.
let SearchMetadata (row : Map<string, string>) query =

    // Check which property is being queried and then check if that property's
    // value contains the query value. For properties that are a list, check
    // each item in that list.
    match Map.tryFind(query.Property) row with
        | Some value -> not (Seq.tryFind (fun (item: string) -> item.ToLower().Contains(query.SearchTerm.ToLower())) (value.Split(",")) = None)
        | None -> false

// Checks the loaded metadata for the specified vertex.
// The key to match on is passed as a parameter.
let findMatchingMetadataRow (vertex: Vert) matchKey metadata =
  List.tryFind (fun (row : Map<string, string>) -> row[matchKey].ToLower().Equals(vertex.Tag.ToLower())) metadata

// Determines whether a given vertex matches a query based on metadata. Used
// for the below function.
let findFirstWithMetadata_filter (metadata : Map<string, string> list) (query : Query) (vertex : Vert) =
    // First, try to find a matching row in the metadata.
    // From Sean:
    match findMatchingMetadataRow vertex "Name" metadata with
    // If a metadata match was found, check the metadata for a query
    // match.
    | Some (row) -> SearchMetadata row query
    // Otherwise return false.
    | None -> false

// Attempts to move to a vertex meeting the specified query conditions.
// Only accepts a single query that specifies a property to search on and a
// value that that property contains.
let findFirstWithMetadata (metadata : Map<string, string> list) (graph : AppGraph) (zipper : Zipper<Vert, AppEdge> ref) (query : Query) =
    // Set up the function for filtering with the query.
    let filterQuery = findFirstWithMetadata_filter metadata query
    // Call the movement function for a matching vertex.
    moveAlongFirstMatchingVertex(graph, zipper.Value, filterQuery)

// Attempts to move to a vertex meeting the specified query conditions.
// Accepts a list of queries (consisting of the property and the value they
// should contain) and the logical operator to use with those queries.
let findFirstWithMetadataMulti (metadata : Map<string, string> list) (graph : AppGraph) (zipper : Zipper<Vert, AppEdge> ref) (queryList : MultiQuery) =
    // Create the function to test for true or false on a vertex.
    let filterQuery (vertex : Vert) =
        // First, try to find a matching row in the metadata.
        match findMatchingMetadataRow vertex "Name" metadata with
        // If a metadata match was found, check the metadata for a query match.
        | Some (row) ->
            // If the operation is AND, return true if all queries matched.
            // If the operation is OR, return true if at least one query
            let comp = if queryList.Operation.Equals(AND) then (&&) else (||)
            (queryList.Operation.Equals(AND), queryList.Queries)
              ||> List.fold (fun accum v -> comp accum (SearchMetadata row v))
        // Otherwise return false.
        | None -> false
    // Call the movement function for a matching vertex.
    moveAlongFirstMatchingVertex(graph, zipper.Value, filterQuery)

// ----------------------------------------------------------------------------
// ------ Example graph functions ------
// ----------------------------------------------------------------------------

// Generates the graph for this example.
let generateFreshGraph (): AppGraph =

    // Create the vertices for the graph.
    let vzero = newVert "zero" 0
    let vone = newVert "one" 1
    let vtwo = newVert "two" 2
    let vthree = newVert "three" 3
    let vfortytwo = newVert "forty-two" 42
    let visolated = newVert "nine-thousand-and-one" 9001

    // Attempt to add the vertices to the graph.
    let graph = new AppGraph ()
                |> tryAddVertex vzero
                |> tryAddVertex vone
                |> tryAddVertex vtwo
                |> tryAddVertex vthree
                |> tryAddVertex vfortytwo
                |> tryAddVertex visolated

    // Attempt to add edges to the graph.
                |> tryAddEdge (newEdge vzero vone "add_one" ScoringValues.DEFAULT_EDGE_VALUE)
                |> tryAddEdge (newEdge vzero vtwo "add_two" ScoringValues.DEFAULT_EDGE_VALUE)
                |> tryAddEdge (newEdge vtwo vthree "two_three" ScoringValues.DEFAULT_EDGE_VALUE)
                |> tryAddEdge (newEdge vone vthree "one_three" ScoringValues.DEFAULT_EDGE_VALUE)
                |> tryAddEdge (newEdge vthree vfortytwo "end" ScoringValues.DEFAULT_EDGE_VALUE)

    // Go through all the edges in the graph, and set their value to the number
    // of connected edges of the target vertex.
    //calculateEdgeValues_Connections(g)
#if USE_DFS
                |> WeightedDFS.calculateEdgeValues_Connections ScoringValues.NUM_SCORE_STEPS true
#else
                |> WeightedBFS.calculateEdgeValues_Connections ScoringValues.NUM_SCORE_STEPS
#endif

    // Return the completed graph.
    graph

// Creates a example set of metadata for the example graph.
// Synonyms are used in the example visuaisation for filters and can also be
// used for move operations based on vertices matching certain criteria.
let GenerateExampleMetadata () =
    let metadata = [
        {
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
        };
        {
            Id = "9001";
            Name = "nine-thousand-and-one";
            Synonyms = ["Isolated vertex"; "Over nine thousand"]
        }
    ]

    List.map (fun row ->
        Map.empty
            .Add("Id", row.Id)
            .Add("Name", row.Name)
            .Add("Synonyms", String.concat "," row.Synonyms)
    ) metadata

// ----------------------------------------------------------------------------
// ------ Server functions ------
// ----------------------------------------------------------------------------

// Adds CORS headers to allow cross origin requests.
// This is required for NetworkVis.js to be able to connect to the server to
// get network graph information and send move requests.
// Taken from https://www.fssnip.net/mL/title/CORS-response-with-Suave via
// https://stackoverflow.com/questions/44359375/allow-multiple-headers-with-cors-in-suave
let setCORSHeaders =
    addHeader "Access-Control-Allow-Origin" "*"
    >=> addHeader "Access-Control-Allow-Headers" "content-type"

// Handles a move request. Attempts to complete the request, failing with a 404
// response if the operation did not return a valid vertex.
let moveRoute metadata graph (zipper : Zipper<Vert, AppEdge> ref) request =
    let moveOp =
      request.rawForm
      |> UTF8.toString
      |> Json.deserialize<MoveOp>

    printfn "Received move operation: %s" (string moveOp)

    // Determine the move operation to use.
    let moveRes = match moveOp with
                    // Standard operations:
                    | ToVertex vval -> moveToVertex (graph, zipper.Value, vval)
                    | AlongEdge edge -> moveAlongEdge (graph, zipper.Value, (newEdge (newVert "dummy" -1) (newVert "dummy" -2) edge -1))
                    | FirstEdge s -> moveAlongFirstMatchingEdge(graph, zipper.Value, (fun ve -> ve.Tag = s))
                    | FirstVertex co -> moveAlongFirstMatchingVertex(graph, zipper.Value, mkCompOp co)
                    // History operations:
                    | Back -> Some(moveBack zipper.Value)
                    | Forward -> Some(moveForward zipper.Value)
                    | GoToHistory i -> Some(moveToHistoryIndex i zipper.Value)
                    // Special operations
                    | ForceToVertex vval -> forceMoveToVertex (graph, zipper.Value, vval)
                    | MetadataSearch query -> findFirstWithMetadata metadata graph zipper query
                    | MetadataSearchMulti queryList -> findFirstWithMetadataMulti metadata graph zipper queryList
                    | NextMostConnected unvisited -> findNextMostConnected unvisited graph zipper
                    | NextHighestQueryScore query -> findNextHighestQueryScore graph zipper ((findFirstWithMetadata_filter metadata) query)
                    | NextHighestScore unvisited -> findNextHighestScore unvisited graph zipper

    // Attempt to complete the move operation.
    match moveRes with
        | None ->
            printfn "Remained at vertex %s (%i) as move action did not return a valid vertex." zipper.Value.Cursor.Tag zipper.Value.Cursor.Value
            NOT_FOUND "No valid vertex to move to."
        | Some newZipper ->
            if (newZipper.Cursor.Equals(zipper.Value.Cursor)) then
                printfn "Remained at vertex %s (%i) as move action returned the same vertex." newZipper.Cursor.Tag newZipper.Cursor.Value
            else
                printfn "Moved to vertex %s (%i)." newZipper.Cursor.Tag newZipper.Cursor.Value
            zipper.Value <- newZipper
            OK (Json.serialize (zipper.Value).Cursor)

// Handles a getLocation request by returning the current zipper cursor.
let locationRoute (zipper : Zipper<Vert, TaggedValueEdge<Vert, string, float>> ref) _request =
    printfn "Received getLocation operation."
    OK (Json.serialize (zipper.Value).Cursor)

// Returns all edges connected to the current vertex.
let connectedRoute (graph : AppGraph) (zipper : Zipper<Vert, AppEdge> ref) _request =
    printfn "Received getDestinations operation."
    let edges = graph.OutEdges((zipper.Value).Cursor)
    let edgesMap = Seq.map (fun (item : AppEdge) -> item.ToRecord()) edges
    OK (Json.serialize {| Edges = Seq.toList edgesMap |})

// Returns the current vertex and all edges connected to it.
let getCursorSurroundsRoute (graph : AppGraph) (zipper : Zipper<Vert, AppEdge> ref) _request =
    printfn "Received getCursorSurrounds operation."
    let currentVertex = (zipper.Value).Cursor
    let edges = Seq.filter (fun (item : AppEdge) -> item.Source.Equals(currentVertex) || item.Target.Equals(currentVertex)) graph.Edges
    let edgesMap = Seq.map (fun (item : AppEdge) -> item.ToRecord()) edges
    OK (Json.serialize {| Vertex = (zipper.Value).Cursor; Edges = Seq.toList edgesMap; History = (zipper.Value).VertHistory; HistoryIndex = (zipper.Value).HistoryIndex |})

// Returns the graph's metadata.
let getMetadataRoute metadata _request =
    printfn "Received getMetadata operation."
    OK (Json.serialize metadata)

// Returns all the vertices in the current graph.
let getVerticesRoute (graph : AppGraph) _request =
    printfn "Received getVertices operation."
    OK (Json.serialize (Seq.toList graph.Vertices))

// Returns all the edges in the current graph.
let getEdgesRoute (graph : AppGraph) _request =
    printfn "Received getEdges operation."
    let edgesMap = Seq.map (fun (item : AppEdge) -> item.ToRecord()) graph.Edges
    OK (Json.serialize (Seq.toList edgesMap))

// Returns all the verticies and edges in the current graph.
let getGraphRoute (graph : AppGraph) _request =
    printfn "Received getGraph operation."
    let edgesMap = Seq.map (fun (item : AppEdge) -> item.ToRecord()) graph.Edges
    OK (Json.serialize {| Vertices = (Seq.toList graph.Vertices); Edges = (Seq.toList edgesMap)|})

// Handles the routes for the example.
let app graph zipper =

    // Generate the metadata for this example, as it's used in moveRoute and
    // getMetadataRoute.
    let metadata = GenerateExampleMetadata()
    choose

        // POST requests
        [ POST >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
            // Request to move the current zipper cursor.
            [ path "/move" >=> request (moveRoute metadata graph zipper) ] )

        // GET requests
          GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
            [
                // Request to return the current zipper cursor.
                path "/getLocation" >=> request (locationRoute zipper)

                // Request to show all valid destinations from the current
                // zipper cursor.
                path "/getDestinations" >=> request (connectedRoute graph zipper)

                // Request to return the current zipper cursor and all vertices
                // connected to it. Includes vertices that have an incoming
                // edge to the current cursor.
                path "/getCursorSurrounds" >=> request (getCursorSurroundsRoute graph zipper)

                // Request to get the metadata for the graph.
                path "/getMetadata" >=> request (getMetadataRoute metadata)

                // Request to get a list of all vertices in the current graph.
                path "/getVertices" >=> request (getVerticesRoute graph)

                // Request to get a list of all edges in the current graph.
                path "/getEdges" >=> request (getEdgesRoute graph)

                // Request to get all vertices and all edges in the current graph.
                path "/getGraph" >=> request (getGraphRoute graph)
            ] ) ]

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
// NextMostConnected:
// $ curl -X POST -vvv --data '{"moveOp":"NextMostConnected","moveInputs":true}' http://localhost:8080/move
// Forward:
// $ curl -X POST -vvv --data '{"moveOp":"Forward","moveInputs":[]}' http://localhost:8080/move
// GoToHistory:
// $ curl -X POST -vvv --data '{"moveOp":"GoToHistory","moveInputs":0}' http://localhost:8080/move
// NextHighestQueryScore:
// $ curl -X POST -vvv --data '{"moveOp":"NextMostConnected","moveInputs":{"Property":"Synonyms","Value":"child"}}' http://localhost:8080/move
// NextHighestScore:
// $ curl -X POST -vvv --data '{"moveOp":"NextHighestScore","moveInputs":true}' http://localhost:8080/move

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
// NextMostConnected:
// curl -X POST -vvv --data {\"moveOp\":\"NextMostConnected\",\"moveInputs\":true} http://localhost:8080/move
// Forward:
// curl -X POST -vvv --data {\"moveOp\":\"Forward\",\"moveInputs\":[]} http://localhost:8080/move
// GoToHistory:
// curl -X POST -vvv --data {\"moveOp\":\"GoToHistory\",\"moveInputs\":0} http://localhost:8080/move
// NextHighestQueryScore:
// curl -X POST -vvv --data {\"moveOp\":\"NextMostConnected\",\"moveInputs\":{\"Property\":\"Synonyms\",\"Value\":\"child\"}}} http://localhost:8080/move
// NextHighestScore:
// curl -X POST -vvv --data {\"moveOp\":\"NextHighestScore\",\"moveInputs\":true} http://localhost:8080/move
[<EntryPoint>]
// Main function for the example serer. Creates a graph and zipper and uses
// them to start the web server with the specified configuration.
let main argv =
    let graph = generateFreshGraph()
#if LOGGING
    printfn "Created a graph with %i vertices and %i edges." (graph.VertexCount) (graph.EdgeCount)
#endif
    let root = newVert "zero" 0
    printfn "Starting at vertex: %s" (Json.serialize root)
    let mutable freshZip = ref (createZipper root)
    startWebServer defaultConfig (app graph freshZip)
    0
