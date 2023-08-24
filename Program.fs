module Program

open FSharp.Json

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
// Added by Samuel Smith n7581769.
open Suave.Writers

open QuikGraph
open GraphDSL.Zipper

// Specifies the vertex used for the bidirectional graph. Each vertex has a
// string tag to name it, and a integer value that is used as an identifier.
type Vert =
    { Tag: string
      Value: int }

// Specifies the configuration for the QuikGraph used.
type AppGraph = BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>

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

// Types added by Samuel Smith n7581769.
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

// MiniGraph type that allows for zipper history being sent with it.
type MiniGraphMetadata = {
    Vertex : Vert
    Edges : MiniJsonEdge list
    History : Vert list
    HistoryIndex : int
}

// Default value for edges.
let DEFAULT_EDGE_VALUE = 1.0

// Added by Samuel Smith n7581769.
// Creates a example set of metadata for the example graph.
// Synonyms are used in the example visuaisation for filters and can also be
// used for move operations based on vertices matching certain criteria.
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

// Added by Samuel Smith n7581769.
// Checks if the supplied query makes a match in the supplied metadata row.
let SearchMetadata (row : ExampleInfo) query =

    // Check which property is being queried and then check if that property's
    // value contains the query value. For properties that are a list, check
    // each item in that list.
    match query.Property with
        | "Id" -> row.Id.Contains(query.Value)
        | "Name" -> row.Name.Contains(query.Value)
        | "Synonyms" -> not (List.tryFind (fun (item: string) -> item.Contains(query.Value)) row.Synonyms = None)
        | _ -> false

// Checks the loaded metadata for the specified vertex (using its name as the
// identifier).
let findMatchingMetadataRow (vertex: Vert) mdata =
  List.tryFind (fun (row : ExampleInfo) -> row.Name.ToLower().Equals(vertex.Tag.ToLower())) mdata

// Added by Samuel Smith n7581769.
// Determines whether a given vertex matches a query based on metadata. Used
// for the below function.
let findFirstWithMetadata_filter (metadata : ExampleInfo list) (query : Query) (vertex : Vert) =
    // First, try to find a matching row in the metadata.
    // From Sean:
    match findMatchingMetadataRow vertex metadata with
    // If a metadata match was found, check the metadata for a query
    // match.
    | Some (row) -> SearchMetadata row query
    // Otherwise return false.
    | None -> false

// Added by Samuel Smith n7581769.
// Attempts to move to a vertex meeting the specified query conditions.
// Only accepts a single query that specifies a property to search on and a
// value that that property contains.
let findFirstWithMetadata((graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>), zipper, (query : Query)) =
    // Get the metadata for the graph.
    let metadata = GenerateExampleMetadata()
    // Set up the function for filtering with the query.
    let filterQuery = findFirstWithMetadata_filter metadata query
    // Call the movement function for a matching vertex.
    moveAlongFirstMatchingVertex(graph, !zipper, filterQuery)

// Added by Samuel Smith n7581769.
// Attempts to move to a vertex meeting the specified query conditions.
// Accepts a list of queries (consisting of the property and the value they
// should contain) and the logical operator to use with those queries.
let findFirstWithMetadataMulti((graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>), zipper, (queryList : MultiQuery)) =
    // Get the metadata for the graph.
    let metadata = GenerateExampleMetadata()
    // Create the function to test for true or false on a vertex.
    let filterQuery (vertex : Vert) =
        // First, try to find a matching row in the metadata.
        match findMatchingMetadataRow vertex metadata with
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
    moveAlongFirstMatchingVertex(graph, !zipper, filterQuery)

// Our boundary types are different to the internal zipper types so there isn't conceptual leakage between the two.
[<JsonUnion(Mode = UnionMode.CaseKeyAsFieldValue, CaseKeyField="moveOp", CaseValueField="moveInputs")>]
type MoveOp =
    | ToVertex of Vert      // Move to the vertex that matches the given tag and value.
    | AlongEdge of string   // Move to the vertex that is the destination of the given edge.
    | FirstEdge of string   // Move along the first edge matching the given selection options.
    | FirstVertex of CompOp // Move to the first vertex connected to the current vertex matching the given comparison.
    | Back                  // Move to the vertex that was before the last move in the zipper's history. Does not erase the history.
    // MoveOps added by Samuel Smith n7581769.
    | MetadataSearch of Query           // Move to the first vertex whose metadata matches the given search query.
    | MetadataSearchMulti of MultiQuery // Move to the first vertex whose metadata matches the given multiple search queries (either all of them via AND, or at least one via OR).
    | ForceToVertex of Vert             // Move to the vertex that matches the given tag and value. Can move across backwards edges.
    | NextMostConnected                 // Move to the vertex with the most outgoing connections connected to the current cursor (that has not already been visited).
    | Forward                           // Move to the vertex that was after the next move in the zipper's history. Does not erase the history.
    | GoToHistory of int                // Move to the vertex after the specified step in the zipper's history (or the starting vertex if given 0). Does not erase the history.
    | NextHighestQueryScore of Query    // Move to the vertex whose edge connecting it to the current vertex has the highest (weighted) score, based on a given query.

// Creates a new edge from the first specified vertex to the second with the
// specified tag.
let newEdge (a: Vert, b: Vert, t: string) : TaggedValueEdge<Vert, string, float> =
    new TaggedValueEdge<Vert, string, float>(a,b,t,DEFAULT_EDGE_VALUE)

// Creates a new vertex with the specified tag and value.
let newVert (tag: string, v: int): Vert =
    { Tag = tag; Value = v }

// Comparison operations for matching to vertices. Can work against the value
// or the tag. Values can be tested to be equal to, greater than or less than
// the given value, and tags can be tested to be equal to the given value.
let mkCompOp (c: CompOp): (Vert -> bool) =
    match c with
        | ValueLessThan i -> fun v -> v.Value < i
        | ValueGreaterThan i -> fun v -> v.Value > i
        | EqualTo (Tag s) -> fun v -> v.Tag = s
        | EqualTo (Value vl) -> fun v -> v.Value = vl

// Resets all edge values to the default of 1.0.
let resetEdgeValues (g : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) =
#if DEBUG
    printfn "Resetting edge values..."
#endif
    Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Value <- DEFAULT_EDGE_VALUE) g.Edges
#if DEBUG
    printfn "Changed edge values."
#endif

// Calculates edge values for a given graph by how many connections the target
// vertex has.
let calculateEdgeValues_Connections (g : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) =
#if DEBUG
    printfn "Changing edge values to number of target's connections..."
#endif
    Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Value <- Seq.length (g.OutEdges(edge.Target))) g.Edges
#if DEBUG
    printfn "Changed edge values."
#endif

// Calculates a weighted edge value based on a condition and how many vertices
// meet that condition a certain number of steps away.
// (Note: some innacuracies may result thanks to the method used to avoid
// counting vertices more than once.)
let calculateEdgeValues_WeightedCondition (g : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps condition =
#if DEBUG
    printfn "Changing edge values to weighted score based on condition and %i steps..." numSteps
#endif
    // At present, I'm not sure how to keep a record of what vertices have been
    // visited without using an array that can change outside the function.
    let mutable visitedVerts = []

    // Recursive function for looking through vertices up to a certain number
    // of steps away, and adding up the score for all verticies that meet a
    // given condition, weighting the score on how far the vertex is from the
    // starting point.
    // Ideally this should be in a seperate function since it's the same as the
    // one in the below function, but it's relying on the mutable right now.
    let rec scoring = fun (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps condition stepsToGo verts previousScore ->

        // If there are no more steps to go, stop travelling down edges.
        match stepsToGo with
        | 0 -> (previousScore)
        | _ -> (
            Seq.fold (fun (previousScore : float) (vert : Vert) ->
            // If this vertex has already been visited, don't go any further
            // down this path.
            if (Seq.contains(vert) visitedVerts) then
                previousScore
            // If this vertex meets the condition, add the weighted score to
            // the accumulated score and add it as a visited vertex.
            elif (condition vert) then
                let scoreToAdd = ((1.0/(float numSteps)) * (float stepsToGo))
                let currentScore = previousScore + scoreToAdd
                visitedVerts <- List.append visitedVerts [vert]
                scoring graph numSteps condition (stepsToGo - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) (graph.OutEdges(vert))) currentScore
            // Otherwise add it as a visited vertex but don't add to the score.
            else
                visitedVerts <- List.append visitedVerts [vert]
                scoring graph numSteps condition (stepsToGo - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) (graph.OutEdges(vert))) previousScore
                ) previousScore verts
            )

    // For every vertex in the network...
    Seq.iter (fun (vert : Vert) ->

        // Iterate through the adjacent edges first. This is done here
        // since the scores should be applied to these edges (they aren't
        // actually done so yet in this test).
        Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) ->
            // Only proceed with calculating the score if this edge isn't
            // a loop.
            if (edge.Source.Equals(edge.Target)) then
                ()
            else
                // Initialise the list of visited verticies for this starting
                // point.
                visitedVerts <- [vert]

                // Check if the target vertex for the current edge meets
                // the condition. If it does, start the score at 1.0,
                // otherwise start it at zero.
                let startingScore = match condition edge.Target with
                                    | true -> 1.0
                                    | _ -> 0.0

                // Start the recursion for subsequent edges after this one.
                let score = scoring g numSteps condition (numSteps - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) (g.OutEdges(edge.Target))) startingScore
                edge.Value <- score
                ()
            ) (g.OutEdges(vert))
        ()
    ) g.Vertices
#if DEBUG
    printfn "Changed edge values."
#endif
    ()

// Calculates a weighted edge value based on a condition and how many vertices
// meet that condition a certain number of steps away.
// (Note: some innacuracies may result thanks to the method used to avoid
// counting vertices more than once.)
// This version only calculates for a specified origin vertex.
let calculateEdgeValues_WeightedConditionSingle (g : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps condition (origin : Vert) =
#if DEBUG
    printfn "Changing edge values to weighted score based on condition and %i steps..." numSteps
#endif
    // At present, I'm not sure how to keep a record of what vertices have been
    // visited without using an array that can change outside the function.
    let mutable visitedVerts = []

    // Recursive function for looking through vertices up to a certain number
    // of steps away, and adding up the score for all verticies that meet a
    // given condition, weighting the score on how far the vertex is from the
    // starting point.
    // Ideally this should be in a seperate function since it's the same as the
    // one in the above function, but it's relying on the mutable right now.
    let rec scoring = fun (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps condition stepsToGo verts previousScore ->

        // If there are no more steps to go, stop travelling down edges.
        match stepsToGo with
        | 0 -> (previousScore)
        | _ -> (
            Seq.fold (fun (previousScore : float) (vert : Vert) ->
            // If this vertex has already been visited, don't go any further
            // down this path.
            if (Seq.contains(vert) visitedVerts) then
                previousScore
            // If this vertex meets the condition, add the weighted score to
            // the accumulated score and add it as a visited vertex.
            elif (condition vert) then
                let scoreToAdd = ((1.0/(float numSteps)) * (float stepsToGo))
                let currentScore = previousScore + scoreToAdd
                visitedVerts <- List.append visitedVerts [vert]
                scoring graph numSteps condition (stepsToGo - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) (graph.OutEdges(vert))) currentScore
            // Otherwise add it as a visited vertex but don't add to the score.
            else
                visitedVerts <- List.append visitedVerts [vert]
                scoring graph numSteps condition (stepsToGo - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) (graph.OutEdges(vert))) previousScore
                ) previousScore verts
            )

    // For the specified vertex...
    let vert = origin

    // Iterate through the adjacent edges first. This is done here
    // since the scores should be applied to these edges (they aren't
    // actually done so yet in this test).
    Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) ->
        // Only proceed with calculating the score if this edge isn't
        // a loop.
        if (edge.Source.Equals(edge.Target)) then
            ()
        else
            // Initialise the list of visited verticies for this starting
            // point.
            visitedVerts <- [vert]

            // Check if the target vertex for the current edge meets
            // the condition. If it does, start the score at 1.0,
            // otherwise start it at zero.
            let startingScore = match condition edge.Target with
                                | true -> 1.0
                                | _ -> 0.0

            // Start the recursion for subsequent edges after this one.
            let score = scoring g numSteps condition (numSteps - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) (g.OutEdges(edge.Target))) startingScore
            edge.Value <- score
            ()
        ) (g.OutEdges(vert))
#if DEBUG
    printfn "Changed edge values."
#endif
    ()

// Added by Samuel Smith n7581769.
// Attempts to move to the connected vertex that has the most outgoing
// connections.
let findNextMostConnected((g : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>), z) =

    // Calculate the edge values for this operation.
    calculateEdgeValues_Connections(g)

    // Sort the list of edges connected to the current vertex by how many
    // connections their targets have.
    let sortedEdges = Seq.sortByDescending (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Value) (g.OutEdges((!z).Cursor))
    #if DEBUG
    Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) -> printfn "Vertex %s has %f targets" edge.Target.Tag edge.Value) sortedEdges
    Seq.iter (fun (vertex : Vert) -> printfn "Vertex %s has been visited" vertex.Tag) (last ((!z).HistoryIndex + 1) (!z).VertHistory)
    #endif

    // Function for checking if a vertex is in the history; used in the
    // function below.
    let checkInHistory (vertex : Vert) =
        match Seq.tryFind(fun historyVert -> vertex.Equals(historyVert)) (last ((!z).HistoryIndex + 1) (!z).VertHistory) with
        | Some (result) -> true
        | None -> false

    // Function to check for the first edge in the sorted list whose target
    // isn't in the history. This will be the edge that is connected to the
    // next most connected vertex.
    let targetEdge =
        // Find a vertex that's not already in the history.
        match Seq.tryFind (fun (sortedEdge : TaggedValueEdge<Vert,string,float>) -> not (checkInHistory sortedEdge.Target)) sortedEdges with
        | Some (result) -> result
        | None -> new TaggedValueEdge<Vert, string, float>(newVert("null_1", -1),newVert("null_2", -2),"null",-DEFAULT_EDGE_VALUE) // This is beccause each side needs to return a value.

    // Create the function to test for true or false on an edge.
    let filterQuery (edge : TaggedValueEdge<Vert,string,float>) =
        edge.Equals(targetEdge)

    // Call the movement function for a matching edge.
    let result = moveAlongFirstMatchingEdge(g, !z, filterQuery)

    // Reset the edge values.
    resetEdgeValues(g)

    result

// Added by Samuel Smith n7581769.
// Attempts to move to a connected vertex that is "most relevant" to a
// particular query. Essentially, a weighted score is calculated for each
// outgoing edge for four steps away from the initial vertex, with higher
// weightings for closer vertices to the starting point (that fulfil the
// query condition), and the zipper will travel down the edge that has the
// greatest score.
let findNextHighestQueryScore((g : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>), z, condition) =

    // Calculate the edge values for this operation.
    calculateEdgeValues_WeightedConditionSingle g 4 condition (!z).Cursor

    // Sort the list of edges connected to the current vertex by their score.
    let sortedEdges = Seq.sortByDescending (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Value) (g.OutEdges((!z).Cursor))
    #if DEBUG
    Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) -> printfn "Edge to vertex %s has a score of %f" edge.Target.Tag edge.Value) sortedEdges
    Seq.iter (fun (vertex : Vert) -> printfn "Vertex %s has been visited" vertex.Tag) (last ((!z).HistoryIndex + 1) (!z).VertHistory)
    #endif

    // Function for checking if a vertex is in the history; used in the
    // function below.
    let checkInHistory (vertex : Vert) =
        match Seq.tryFind(fun historyVert -> vertex.Equals(historyVert)) (last ((!z).HistoryIndex + 1) (!z).VertHistory) with
        | Some (result) -> true
        | None -> false

    // Function to check for the first edge in the sorted list whose target
    // isn't in the history. This will be the edge that is connected to the
    // next most connected vertex.
    let targetEdge =
        // Find a vertex that's not already in the history.
        // Also check that the score does not equal zero.
        match Seq.tryFind (fun (sortedEdge : TaggedValueEdge<Vert,string,float>) -> not (checkInHistory sortedEdge.Target) && sortedEdge.Value > 0.0) sortedEdges with
        | Some (result) -> result
        | None -> new TaggedValueEdge<Vert, string, float>(newVert("null_1", -1),newVert("null_2", -2),"null",-DEFAULT_EDGE_VALUE) // This is beccause each side needs to return a value.

    // Create the function to test for true or false on an edge.
    let filterQuery (edge : TaggedValueEdge<Vert,string,float>) =
        edge.Equals(targetEdge)

    // Call the movement function for a matching edge.
    let result = moveAlongFirstMatchingEdge(g, !z, filterQuery)

    // Reset the edge values.
    resetEdgeValues(g)

    result

// Handles a move request. Attempts to complete the request, failing with a 404
// response if the operation did not return a valid vertex.
let f g z rq =
    let moveOp =
      rq.rawForm
      |> UTF8.toString
      |> Json.deserialize<MoveOp>

    // Message changed by Samuel Smith n7581769.
    printfn "Received move operation: %s" (moveOp.ToString())

    // Determine the move operation to use.
    let moveRes = match moveOp with
                    | ToVertex vval -> moveToVertex (g, !z, vval)
                    | AlongEdge eege -> moveAlongEdge (g, !z, newEdge(newVert("dummy",-1),newVert("dummy",-2), eege))
                    | FirstEdge s -> moveAlongFirstMatchingEdge(g, !z, (fun ve -> ve.Tag = s))
                    | FirstVertex co -> moveAlongFirstMatchingVertex(g, !z, mkCompOp co)
                    | Back -> Some(moveBack !z)
                    // MoveOps added by Samuel Smith n7581769.
                    | MetadataSearch query -> findFirstWithMetadata(g, z, query)
                    | MetadataSearchMulti queryList -> findFirstWithMetadataMulti(g, z, queryList)
                    | ForceToVertex vval -> forceMoveToVertex (g, !z, vval)
                    | NextMostConnected -> findNextMostConnected (g, z)
                    | Forward -> Some(moveForward !z)
                    | GoToHistory i -> Some(moveToHistoryIndex !z i)
                    | NextHighestQueryScore query -> findNextHighestQueryScore (g, z, findFirstWithMetadata_filter (GenerateExampleMetadata()) query)

    // Attempt to complete the move operation.
    match moveRes with
        // Message changed by Samuel Smith n7581769.
        | None -> NOT_FOUND "No valid vertex to move to."
        | Some r ->
            z := r
            OK (Json.serialize (!z).Cursor)

// Handles a whereami request by returning the current zipper cursor.
let whereami z _rq = OK (Json.serialize (!z).Cursor)

// Routes added by Samuel Smith n7581769.
// Returns all edges connected to the current vertex.
let connectedRoute (g : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) z _request =
    //printfn "Received wheretogo operation."
    let edges = g.OutEdges((!z).Cursor)
    let edgesMap = Seq.map (fun (item : TaggedValueEdge<Vert,string,float>) -> { Start = item.Source; End = item.Target; Tag = item.Tag }) edges
    OK (Json.serialize { Edges = Seq.toList edgesMap })

// Returns the current vertex and all edges connected to it.
let getGraphRoute (g : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) z _request =
    printfn "Received getGraph operation."
    let currentVertex = (!z).Cursor
    let edges = Seq.filter (fun (item : TaggedValueEdge<Vert,string,float>) -> item.Source.Equals(currentVertex) || item.Target.Equals(currentVertex)) g.Edges
    let edgesMap = Seq.map (fun (item : TaggedValueEdge<Vert,string,float>) -> { Start = item.Source; End = item.Target; Tag = item.Tag }) edges
    OK (Json.serialize { Vertex = (!z).Cursor; Edges = Seq.toList edgesMap; History = (!z).VertHistory; HistoryIndex = (!z).HistoryIndex })

// Added by Samuel Smith n7581769.
// Adds CORS headers to allow cross origin requests.
//https://stackoverflow.com/questions/44359375/allow-multiple-headers-with-cors-in-suave
let setCORSHeaders =
    addHeader "Access-Control-Allow-Origin" "*"
    >=> addHeader "Access-Control-Allow-Headers" "content-type"

// Added by Samuel Smith n7581769.
// Returns the graph's metadata.
let getMetadataRoute _request =
    printfn "Received getMetadata operation."
    OK (Json.serialize (GenerateExampleMetadata()))

// Returns all the vertices in the current graph.
let getVerticesRoute (g : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) _request =
    printfn "Received getVertices operation."
    OK (Json.serialize (Seq.toList g.Vertices))

// Returns all the edges in the current graph.
let getEdgesRoute (g : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) _request =
    printfn "Received getEdges operation."
    let edgesMap = Seq.map (fun (item : TaggedValueEdge<Vert,string,float>) -> { Start = item.Source; End = item.Target; Tag = item.Tag }) g.Edges
    OK (Json.serialize (Seq.toList edgesMap))

// CORS handlers added by Samuel Smith n7581769.
// Handles the routes for the example.
let app g z =
    choose

      // Request to move the current zipper cursor.
      [ POST >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/move" >=> request (f g z) ] )

        // Request to return the current zipper cursor.
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/whereami" >=> request (whereami z) ] )

        // Routes added by Samuel Smith n7581769.
        // Request to show all valid destinations from the current zipper
        // cursor.
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/wheretogo" >=> request (connectedRoute g z) ] )

        // Request to return the current zipper cursor and all vertices
        // connected to it. Includes vertices that have an ingoing edge to
        // the current cursor.
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/getGraph" >=> request (getGraphRoute g z) ] )

        // Request to get the metadata for the graph.
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/getMetadata" >=> request (getMetadataRoute)] )

        // Request to get a list of all vertices in the current graph.
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/getVertices" >=> request (getVerticesRoute g) ] )

        // Request to get a list of all edges in the current graph.
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/getEdges" >=> request (getEdgesRoute g) ] ) ]

// Attempts to add an edge to the graph.
// Changes by Samuel Smith n7581769:
// - No longer fails if the edge already exists; and
// - Changed name to tryAddEdge.
let tryAddEdge (g: AppGraph, e: TaggedValueEdge<Vert,string,float>) =
    let success = g.AddEdge(e)
    // If the edge add operation returned false (i.e. the edge wasn't added)
    // then check if the edge is in the graph; this means that the edge was
    // already in the graph.
    if (not success) then
        if (g.ContainsEdge(e)) then
            ()
        // Otherwise, the operation failed for another reason and there's a
        // problem with creating the graph.
        else
            failwith "Failed to add edge"
    else
        ()

// Attempts to add a avertex to the graph.
// Changes by Samuel Smith n7581769:
// - No longer fails if the vertex already exists; and
// - Changed name to tryAddVertex.
let tryAddVertex (g: AppGraph, v: Vert) =
    let success = g.AddVertex(v)
    // If the vertex add operation returned false (i.e. the vertex wasn't
    // added) then check if the vertex is in the graph; this means that the
    // vertex was already in the graph.
    if (not success) then
        if (g.ContainsVertex(v)) then
            ()
        // Otherwise, the operation failed for another reason and there's a
        // problem with creating the graph.
        else
            failwith "Failed to add vertex"
    else
        ()

// Generates the graph for this example.
let generateFreshGraph: AppGraph =
    let mutable g = new BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>> ()

    // Create the vertices for the graph.
    let vzero = newVert("zero", 0)
    let vone = newVert("one", 1)
    let vtwo = newVert("two", 2)
    let vthree = newVert("three", 3)
    let vfortytwo = newVert("forty-two", 42)

    // Attempt to add the vertices to the graph.
    tryAddVertex (g, vzero)
    tryAddVertex (g, vone)
    tryAddVertex (g, vtwo)
    tryAddVertex (g, vthree)
    tryAddVertex (g, vfortytwo)

    // Attempt to add edges to the graph.
    tryAddEdge (g, (newEdge (vzero, vone, "add_one")))
    tryAddEdge (g, (newEdge (vzero, vtwo, "add_two")))
    tryAddEdge (g, (newEdge (vtwo, vthree, "two_three")))
    tryAddEdge (g, (newEdge (vone, vthree, "one_three")))
    tryAddEdge (g, (newEdge (vthree, vfortytwo, "end")))

    // Go through all the edges in the graph, and set their value to the number
    // of connected edges of the target vertex.
    //calculateEdgeValues_Connections(g)

    // Return the completed graph.
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
// NextMostConnected:
// $ curl -X POST -vvv --data '{"moveOp":"NextMostConnected","moveInputs":[]}' http://localhost:8080/move
// Forward:
// $ curl -X POST -vvv --data '{"moveOp":"Forward","moveInputs":[]}' http://localhost:8080/move
// GoToHistory:
// $ curl -X POST -vvv --data '{"moveOp":"GoToHistory","moveInputs":0}' http://localhost:8080/move
// NextHighestQueryScore:
// $ curl -X POST -vvv --data '{"moveOp":"NextMostConnected","moveInputs":{"Property":"Synonyms","Value":"child"}}' http://localhost:8080/move

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
// curl -X POST -vvv --data {\"moveOp\":\"NextMostConnected\",\"moveInputs\":[]} http://localhost:8080/move
// Forward:
// curl -X POST -vvv --data {\"moveOp\":\"Forward\",\"moveInputs\":[]} http://localhost:8080/move
// GoToHistory:
// curl -X POST -vvv --data {\"moveOp\":\"GoToHistory\",\"moveInputs\":0} http://localhost:8080/move
// NextHighestQueryScore:
// curl -X POST -vvv --data {\"moveOp\":\"NextMostConnected\",\"moveInputs\":{\"Property\":\"Synonyms\",\"Value\":\"child\"}}} http://localhost:8080/move
[<EntryPoint>]
// Main function for the example serer. Creates a graph and zipper and uses
// them to start the web server with the specified configuration.
let main argv =
    let mutable g = generateFreshGraph
    printfn "Graph Status vertices: %i, edges: %i" (g.VertexCount) (g.EdgeCount)
    let root = newVert("zero", 0)
    printfn "Root JSON: %s" (Json.serialize root)
    let mutable freshZip = ref (createZipper root)
    startWebServer defaultConfig (app g freshZip)
    0
