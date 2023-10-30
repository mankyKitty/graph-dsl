module Program

open FSharp.Json

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open Suave.Writers

open QuikGraph
open GraphDSL.Zipper
open QuikGraph.Algorithms.Search
open QuikGraph.Algorithms.ShortestPath

// ----------------------------------------------------------------------------
// ------ Types ------
// ----------------------------------------------------------------------------

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

// A record type for storing an edge to be converted to JSON.
type MiniJsonEdge = {
    Start : Vert
    End : Vert
    Tag : string
    Value : float
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

// Our boundary types are different to the internal zipper types so there isn't conceptual leakage between the two.
[<JsonUnion(Mode = UnionMode.CaseKeyAsFieldValue, CaseKeyField="moveOp", CaseValueField="moveInputs")>]
type MoveOp =
    | ToVertex of Vert      // Move to the vertex that matches the given tag and value.
    | AlongEdge of string   // Move to the vertex that is the destination of the given edge.
    | FirstEdge of string   // Move along the first edge matching the given selection options.
    | FirstVertex of CompOp // Move to the first vertex connected to the current vertex matching the given comparison.
    | Back                  // Move to the vertex that was before the last move in the zipper's history. Does not erase the history.
    | MetadataSearch of Query           // Move to the first vertex whose metadata matches the given search query.
    | MetadataSearchMulti of MultiQuery // Move to the first vertex whose metadata matches the given multiple search queries (either all of them via AND, or at least one via OR).
    | ForceToVertex of Vert             // Move to the vertex that matches the given tag and value. Can move across backwards edges.
    | NextMostConnected                 // Move to the vertex with the most outgoing connections connected to the current cursor (that has not already been visited).
    | Forward                           // Move to the vertex that was after the next move in the zipper's history. Does not erase the history.
    | GoToHistory of int                // Move to the vertex after the specified step in the zipper's history (or the starting vertex if given 0). Does not erase the history.
    | NextHighestQueryScore of Query    // Move to the vertex whose edge connecting it to the current vertex has the highest (weighted) score, based on a given query.

// ----------------------------------------------------------------------------
// ------ Global values ------
// ----------------------------------------------------------------------------
// I'm aware that functional programming doesn't like global variables like
// this, but I'll have them here for now to avoid some magic numbers.

// Default value for edges.
let DEFAULT_EDGE_VALUE = 1.0

// Value for number of steps in weighted score.
let NUM_SCORE_STEPS = 4

#if DEBUG
let STEP_WEIGHT = 0.25
#endif

// ----------------------------------------------------------------------------
// ------ Graph creation functions ------
// ----------------------------------------------------------------------------

// Creates a new edge from the first specified vertex to the second with the
// specified tag.
let newEdge (sourceVert: Vert, targetVert: Vert, tag: string) : TaggedValueEdge<Vert, string, float> =
    new TaggedValueEdge<Vert, string, float>(sourceVert,targetVert,tag,DEFAULT_EDGE_VALUE)

// Creates a new vertex with the specified tag and value.
let newVert (tag: string, value: int): Vert =
    { Tag = tag; Value = value }

// Attempts to add an edge to the graph.
let tryAddEdge (graph: AppGraph, edge: TaggedValueEdge<Vert,string,float>) =
    let success = graph.AddEdge(edge)
    // If the edge add operation returned false (i.e. the edge wasn't added)
    // then check if the edge is in the graph; this means that the edge was
    // already in the graph.
    if (not success) then
        if (graph.ContainsEdge(edge)) then
            ()
        // Otherwise, the operation failed for another reason and there's a
        // problem with creating the graph.
        else
            failwith "Failed to add edge"
    else
        ()

// Attempts to add a avertex to the graph.
let tryAddVertex (graph: AppGraph, vert: Vert) =
    let success = graph.AddVertex(vert)
    // If the vertex add operation returned false (i.e. the vertex wasn't
    // added) then check if the vertex is in the graph; this means that the
    // vertex was already in the graph.
    if (not success) then
        if (graph.ContainsVertex(vert)) then
            ()
        // Otherwise, the operation failed for another reason and there's a
        // problem with creating the graph.
        else
            failwith "Failed to add vertex"
    else
        ()

// ----------------------------------------------------------------------------
// ------ Scoring functions ------
// ----------------------------------------------------------------------------

// Resets all edge values to the default of 1.0.
let resetEdgeValues (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) =
#if DEBUG
    printfn "Resetting edge values..."
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
#endif
    Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Value <- DEFAULT_EDGE_VALUE) graph.Edges
#if DEBUG
    stopWatch.Stop()
    let ms = stopWatch.Elapsed.TotalMilliseconds
    printfn "Changed edge values in %f ms." ms
#endif

// Calculates edge values for a given graph by how many connections the target
// vertex has.
let calculateEdgeValues_Connections (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) =
#if DEBUG
    printfn "Changing edge values to number of target's connections..."
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
#endif
    Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Value <- Seq.length (graph.OutEdges(edge.Target))) graph.Edges
#if DEBUG
    stopWatch.Stop()
    let ms = stopWatch.Elapsed.TotalMilliseconds
    printfn "Changed edge values in %f ms." ms
#endif

// ------ Depth first traversal scoring based on graph connectedness ------

// Recursive function for looking through vertices up to a certain number
// of steps away, and adding up the score for all verticies based on their
// connectedness, weighting the score on how far the vertex is from the
// starting point.
let rec scoring_ConnectionsWeighted = fun (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps stepsToGo verts previousScore uniqueOnly (visitedVerts : Vert list ref) ->

        // If there are no more steps to go, stop travelling down edges.
        match stepsToGo with
        | 0 -> (previousScore)
        | _ -> (
            Seq.fold (fun (previousScore : float) (vert : Vert) ->
            // If this vertex has already been visited, don't go any further
            // down this path.
            if (Seq.contains(vert) (!visitedVerts) && uniqueOnly) then
                previousScore
            // Otherwise add to the score based on the vertex's connectedness.
            else
                // Only count edges that don't loop back to the same vertex.
                let outgoingEdges = Seq.filter (fun (edge : TaggedValueEdge<Vert,string,float>) -> not (edge.Source.Equals(edge.Target))) (graph.OutEdges(vert))
#if DEBUG
                let scoreToAdd = if (numSteps > 4) then
                                    (1.0/(float numSteps))  * (float stepsToGo) * float (Seq.length outgoingEdges)
                                 else
                                    STEP_WEIGHT  * (float stepsToGo) * float (Seq.length outgoingEdges)
#else
                let scoreToAdd = (1.0/(float numSteps))  * (float stepsToGo) * float (Seq.length outgoingEdges)
#endif
(*#if DEBUG
                printfn "%s is %i away and so its connections can be counted - adding %f to score (currently %f)" vert.Tag (numSteps + 1 - stepsToGo) scoreToAdd previousScore
#endif*)
                let currentScore = previousScore + scoreToAdd
                visitedVerts := List.append (!visitedVerts) [vert]
                scoring_ConnectionsWeighted graph numSteps (stepsToGo - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) outgoingEdges) currentScore uniqueOnly visitedVerts
            ) previousScore verts
        )

// Iterator function for connection weighted scoring.
let edgeIterator_ConnectionsWeighted (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) vert numSteps uniqueOnly (edge : TaggedValueEdge<Vert,string,float>) =
    // Only proceed with calculating the score if this edge isn't
    // a loop.
    if (edge.Source.Equals(edge.Target)) then
        ()
    else
(*#if DEBUG
        printfn "------ Calculating score for edge %s to %s ------" edge.Source.Tag edge.Target.Tag
#endif*)
        // Initialise the list of visited verticies for this starting point.
        // At present, I'm not sure how to keep a record of what vertices have
        // been visited without using an array that can change outside the
        // function.
        let mutable visitedVerts = [vert; edge.Target]

        // Get the connectedness of the current vertex.
        // Only count edges that don't loop back to the same vertex.
        let outgoingEdges = Seq.filter (fun (edge : TaggedValueEdge<Vert,string,float>) -> not (edge.Source.Equals(edge.Target))) (graph.OutEdges(edge.Target))
        let startingScore = float (Seq.length outgoingEdges)
(*#if DEBUG
        printfn "Added %f to score from edges from %s" startingScore edge.Target.Tag
#endif*)

        // Start the recursion for subsequent edges after this one.
        let score = scoring_ConnectionsWeighted graph numSteps (numSteps - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) outgoingEdges) startingScore uniqueOnly (ref visitedVerts)
        edge.Value <- score

(*#if DEBUG
        printfn "Edge score set to %f" score
#endif*)
        ()

// Calculates a weighted edge value based on connectedness, considering
// vertices that are a certain number of steps away.
// (Note: some innacuracies may result thanks to the method used to avoid
// counting vertices more than once.)
let calculateEdgeValues_ConnectionsWeighted (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps uniqueOnly =
#if DEBUG
    printfn "Changing edge values to weighted score based on number of connections and %i steps..." numSteps
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
#endif

    // Recursive function for looking through vertices up to a certain number
    // of steps away, and adding up the score for all verticies based on their
    // connectedness, weighting the score on how far the vertex is from the
    // starting point.

    // For every vertex in the network...
    Seq.iter (fun (vert : Vert) ->
(*#if DEBUG
        printfn "------------ Calculating scores from vertex %s -----------" vert.Tag
#endif*)

        // Iterate through the adjacent edges first. This is done here
        // since the scores should be applied to these edges (they aren't
        // actually done so yet in this test).
        Seq.iter (edgeIterator_ConnectionsWeighted graph vert numSteps uniqueOnly) (graph.OutEdges(vert))
        ()
    ) graph.Vertices
#if DEBUG
    stopWatch.Stop()
    let ms = stopWatch.Elapsed.TotalMilliseconds
    printfn "Changed edge values in %f ms." ms
#endif
    ()

// Calculates a weighted edge value based on connectedness, considering
// vertices that are a certain number of steps away.
// (Note: some innacuracies may result thanks to the method used to avoid
// counting vertices more than once.)
// This version only calculates for a specified origin vertex.
let calculateEdgeValues_ConnectionsWeightedSingle (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps (origin : Vert) uniqueOnly =
#if DEBUG
    printfn "Changing edge values to weighted score based on number of connections and %i steps..." numSteps
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
#endif

    // For the specified vertex...
    let vert = origin

    // Iterate through the adjacent edges first. This is done here
    // since the scores should be applied to these edges (they aren't
    // actually done so yet in this test).
    Seq.iter (edgeIterator_ConnectionsWeighted graph vert numSteps uniqueOnly) (graph.OutEdges(vert))
#if DEBUG
    stopWatch.Stop()
    let ms = stopWatch.Elapsed.TotalMilliseconds
    printfn "Changed edge values in %f ms." ms
#endif
    ()

// ------ Breadth first traversal scoring based on graph connectedness ------

// Iterator function for connection weighted BFS scoring.
let edgeIterator_ConnectionsWeighted_BFS graph vert numSteps (edge : TaggedValueEdge<Vert,string,float>) =
    // Only proceed with calculating the score if this edge isn't a loop.
    if (edge.Source.Equals(edge.Target)) then
        ()
    else
(*#if DEBUG
        printfn "------ Calculating score for edge %s to %s ------" edge.Source.Tag edge.Target.Tag
#endif*)

        // Initialise the shortest path algorithm to allow seeing how far a vertex
        // is from the origin.
        // Edges and vertices are given a weight of 1 since we only want to know
        // the number of edges a vertex is away.
        let sp = new AStarShortestPathAlgorithm<Vert, TaggedValueEdge<Vert,string,float>>(graph, (fun edge -> 1.0), (fun vert -> 1.0))

        // A mutable value for the current score. Starts with the connectedness
        // of the current vertex.
        // Only count edges that don't loop back to the same vertex.
        let outgoingEdges = Seq.filter (fun (edge : TaggedValueEdge<Vert,string,float>) -> not (edge.Source.Equals(edge.Target))) (graph.OutEdges(edge.Target))
        let mutable score = float (Seq.length outgoingEdges)
(*#if DEBUG
        printfn "Added %f to score from edges from %s" score edge.Target.Tag
#endif*)
        // Do not traverse if the number of steps is one.
        if (numSteps > 1) then

            // Get the distances of all other vertices from the edge's target.
            sp.Compute(edge.Target)

            // When a vertex is first visited in the search, determine the
            // score based on the distance.
            // Note: I'd prefer this to be a seperate function rather than
            // defined here, but I couldn't get it to work with a mutable score
            // value even if I passed a reference.
            let calculateScore (currentVert : Vert) =

                // Check how far the current vertex is from the starting one.
                // - If 0, this is a loop, so ignore this vertex.
                // - If greater than the total number of steps minus one,
                //   ignore this vertex. (Minus one is to account for the first
                //   step being completed already.)
                // Otherwise add a weighted score.
                match sp.GetDistance(currentVert) with
                | 0.0 -> ()
                | (value) when value > float (numSteps - 1) -> ()
                | (value) ->
                    // Only count edges that don't loop back to the same
                    // vertex.
                    let outgoingEdges = Seq.filter (fun (edge : TaggedValueEdge<Vert,string,float>) -> not (edge.Source.Equals(edge.Target))) (graph.OutEdges(currentVert))
#if DEBUG
                    let scoreToAdd = if (numSteps > 4) then
                                        (1.0/(float numSteps)) * ((float numSteps) - value) * float (Seq.length outgoingEdges)
                                        else
                                        STEP_WEIGHT * ((float numSteps) - value) * float (Seq.length outgoingEdges)
#else
                    let scoreToAdd = (1.0/(float numSteps)) * ((float numSteps) - value) * float (Seq.length outgoingEdges)
#endif
(*#if DEBUG
                    printfn "%s is %f (therefore %f total) away and so its connections can be counted - adding %f to score (currently %f)" currentVert.Tag value (value + 1.0) scoreToAdd score
#endif*)
                    score <- score + scoreToAdd

            // Initialise the breadth-first search.
            // A filter is added to avoid traversing past the original node or
            // the current target.
            let bfs = new BreadthFirstSearchAlgorithm<Vert, TaggedValueEdge<Vert,string,float>>(
                null,
                graph,
                new QuikGraph.Collections.Queue<Vert>(),
                new System.Collections.Generic.Dictionary<Vert, GraphColor>(),
                (fun currentEdges -> Seq.filter (fun currentEdge -> not ((currentEdge.Target.Equals(vert)) || (currentEdge.Target.Equals(edge.Target)))) currentEdges)
            )

            // Set the breadth-first search to use the function above with the
            // score value for this edge.
            bfs.add_DiscoverVertex(calculateScore)

            // Compute the breadth-first search from this edge's target.
            bfs.Compute(edge.Target)

        // Set the edge's value to the computed score.
        edge.Value <- score

(*#if DEBUG
        printfn "Edge score set to %f" score
#endif*)
        ()

// Calculates a weighted edge value based on connectedness, considering
// vertices that are a certain number of steps away.
// Uses QuikGraph's breadth-first search function.
let calculateEdgeValues_ConnectionsWeighted_BFS (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps =
#if DEBUG
    printfn "Changing edge values to weighted score based on number of connections and %i steps..." numSteps
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
#endif

    // For every vertex in the network...
    Seq.iter (fun (vert : Vert) ->
(*#if DEBUG
        printfn "------------ Calculating scores from vertex %s -----------" vert.Tag
#endif*)

        // Iterate through the adjacent edges first, so the scores can be
        // applied to each edge.
        Seq.iter (edgeIterator_ConnectionsWeighted_BFS graph vert numSteps) (graph.OutEdges(vert))
    ) graph.Vertices
#if DEBUG
    stopWatch.Stop()
    let ms = stopWatch.Elapsed.TotalMilliseconds
    printfn "Changed edge values in %f ms." ms
#endif
    ()

// Calculates a weighted edge value based on connectedness, considering
// vertices that are a certain number of steps away.
// Uses QuikGraph's breadth-first search function.
// This version only calculates for a specified origin vertex.
let calculateEdgeValues_ConnectionsWeightedSingle_BFS (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps (origin : Vert) =
#if DEBUG
    printfn "Changing edge values to weighted score based on number of connections and %i steps..." numSteps
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
#endif

    // Initialise the shortest path algorithm to allow seeing how far a vertex
    // is from the origin.
    // Edges and vertices are given a weight of 1 since we only want to know
    // the number of edges a vertex is away.
    let sp = new AStarShortestPathAlgorithm<Vert, TaggedValueEdge<Vert,string,float>>(graph, (fun edge -> 1.0), (fun vert -> 1.0))

    // For the specified vertex...
    let vert = origin
(*#if DEBUG
    printfn "------------ Calculating scores from vertex %s -----------" vert.Tag
#endif*)

    // Iterate through the adjacent edges first, so the scores can be applied
    // to each edge.
    Seq.iter (edgeIterator_ConnectionsWeighted_BFS graph vert numSteps) (graph.OutEdges(vert))
#if DEBUG
    stopWatch.Stop()
    let ms = stopWatch.Elapsed.TotalMilliseconds
    printfn "Changed edge values in %f ms." ms
#endif
    ()

// ------ Depth first traversal scoring based on a given condition ------

// Recursive function for looking through vertices up to a certain number
// of steps away, and adding up the score for all verticies that meet a
// given condition, weighting the score on how far the vertex is from the
// starting point.
let rec scoring_ConditionWeighted = fun (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps condition stepsToGo verts previousScore (visitedVerts : Vert list ref) ->

    // If there are no more steps to go, stop travelling down edges.
    match stepsToGo with
    | 0 -> (previousScore)
    | _ -> (
        Seq.fold (fun (previousScore : float) (vert : Vert) ->
        // If this vertex has already been visited, don't go any further
        // down this path.
        if (Seq.contains(vert) (!visitedVerts)) then
            previousScore
        // If this vertex meets the condition, add the weighted score to
        // the accumulated score and add it as a visited vertex.
        elif (condition vert) then
            let scoreToAdd = ((1.0/(float numSteps)) * (float stepsToGo))
            let currentScore = previousScore + scoreToAdd
            visitedVerts := List.append (!visitedVerts) [vert]
            scoring_ConditionWeighted graph numSteps condition (stepsToGo - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) (graph.OutEdges(vert))) currentScore visitedVerts
        // Otherwise add it as a visited vertex but don't add to the score.
        else
            visitedVerts := List.append (!visitedVerts) [vert]
            scoring_ConditionWeighted graph numSteps condition (stepsToGo - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) (graph.OutEdges(vert))) previousScore visitedVerts
            ) previousScore verts
        )

// Iterator function for condition weighted scoring.
let edgeIterator_ConditionWeighted graph vert numSteps condition (edge : TaggedValueEdge<Vert,string,float>) =
    // Only proceed with calculating the score if this edge isn't a loop.
    if (edge.Source.Equals(edge.Target)) then
        ()
    else
        // Initialise the list of visited verticies for this starting point.
        // At present, I'm not sure how to keep a record of what vertices have
        // been visited without using an array that can change outside the
        // function.
        let mutable visitedVerts = [vert; edge.Target]

        // Check if the target vertex for the current edge meets the
        // the condition. If it does, start the score at 1.0,
        // otherwise start it at zero.
        let startingScore = match condition edge.Target with
                            | true -> 1.0
                            | _ -> 0.0

        // Start the recursion for subsequent edges after this one.
        let score = scoring_ConditionWeighted graph numSteps condition (numSteps - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) (graph.OutEdges(edge.Target))) startingScore (ref visitedVerts)
        edge.Value <- score
        ()

// Calculates a weighted edge value based on a condition and how many vertices
// meet that condition a certain number of steps away.
// (Note: some innacuracies may result thanks to the method used to avoid
// counting vertices more than once.)
let calculateEdgeValues_ConditionWeighted (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps condition =
#if DEBUG
    printfn "Changing edge values to weighted score based on condition and %i steps..." numSteps
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
#endif
    // At present, I'm not sure how to keep a record of what vertices have been
    // visited without using an array that can change outside the function.
    let mutable visitedVerts = []

    // For every vertex in the network...
    Seq.iter (fun (vert : Vert) ->

        // Iterate through the adjacent edges first. This is done here
        // since the scores should be applied to these edges (they aren't
        // actually done so yet in this test).
        Seq.iter (edgeIterator_ConditionWeighted graph vert numSteps condition) (graph.OutEdges(vert))
        ()
    ) graph.Vertices
#if DEBUG
    stopWatch.Stop()
    let ms = stopWatch.Elapsed.TotalMilliseconds
    printfn "Changed edge values in %f ms." ms
#endif
    ()

// Calculates a weighted edge value based on a condition and how many vertices
// meet that condition a certain number of steps away.
// (Note: some innacuracies may result thanks to the method used to avoid
// counting vertices more than once.)
// This version only calculates for a specified origin vertex.
let calculateEdgeValues_ConditionWeightedSingle (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps condition (origin : Vert) =
#if DEBUG
    printfn "Changing edge values to weighted score based on condition and %i steps..." numSteps
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
#endif

    // For the specified vertex...
    let vert = origin

    // Iterate through the adjacent edges first. This is done here
    // since the scores should be applied to these edges (they aren't
    // actually done so yet in this test).
    Seq.iter (edgeIterator_ConditionWeighted graph vert numSteps condition) (graph.OutEdges(vert))
#if DEBUG
    stopWatch.Stop()
    let ms = stopWatch.Elapsed.TotalMilliseconds
    printfn "Changed edge values in %f ms." ms
#endif
    ()

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

// Attempts to move to the connected vertex that has the most outgoing
// connections.
let findNextMostConnected((graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>), zipper) =

    // Sort the list of edges connected to the current vertex by how many
    // connections their targets have.
    let sortedEdges = Seq.sortByDescending (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Value) (graph.OutEdges((!zipper).Cursor))
    #if DEBUG
    //Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) -> printfn "Vertex %s has %f targets" edge.Target.Tag edge.Value) sortedEdges
    Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) -> printfn "Vertex %s weighted score based on connectedness is %f" edge.Target.Tag edge.Value) sortedEdges
    Seq.iter (fun (vertex : Vert) -> printfn "Vertex %s has been visited" vertex.Tag) (last ((!zipper).HistoryIndex + 1) (!zipper).VertHistory)
    #endif

    // Function for checking if a vertex is in the history; used in the
    // function below.
    let checkInHistory (vertex : Vert) =
        match Seq.tryFind(fun historyVert -> vertex.Equals(historyVert)) (last ((!zipper).HistoryIndex + 1) (!zipper).VertHistory) with
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
    let result = moveAlongFirstMatchingEdge(graph, !zipper, filterQuery)

    result

// Attempts to move to a connected vertex that is "most relevant" to a
// particular query. Essentially, a weighted score is calculated for each
// outgoing edge for four steps away from the initial vertex, with higher
// weightings for closer vertices to the starting point (that fulfil the
// query condition), and the zipper will travel down the edge that has the
// greatest score.
let findNextHighestQueryScore((graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>), zipper, condition) =

    let currentCursor = (!zipper).Cursor

    // Calculate the edge values for this operation.
#if DEBUG
    calculateEdgeValues_ConditionWeightedSingle graph NUM_SCORE_STEPS condition currentCursor
#else
    calculateEdgeValues_ConditionWeightedSingle graph NUM_SCORE_STEPS condition currentCursor
#endif

    // Sort the list of edges connected to the current vertex by their score.
    let sortedEdges = Seq.sortByDescending (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Value) (graph.OutEdges(currentCursor))
    #if DEBUG
    Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) -> printfn "Edge to vertex %s has a score of %f" edge.Target.Tag edge.Value) sortedEdges
    Seq.iter (fun (vertex : Vert) -> printfn "Vertex %s has been visited" vertex.Tag) (last ((!zipper).HistoryIndex + 1) (!zipper).VertHistory)
    #endif

    // Function for checking if a vertex is in the history; used in the
    // function below.
    let checkInHistory (vertex : Vert) =
        match Seq.tryFind(fun historyVert -> vertex.Equals(historyVert)) (last ((!zipper).HistoryIndex + 1) (!zipper).VertHistory) with
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
    let result = moveAlongFirstMatchingEdge(graph, !zipper, filterQuery)

    // Reset the edge values (according to the orignal cursor location).
#if DEBUG
    //calculateEdgeValues_ConnectionsWeightedSingle_BFS graph NUM_SCORE_STEPS currentCursor
    calculateEdgeValues_ConnectionsWeightedSingle_BFS graph NUM_SCORE_STEPS currentCursor
#else
    calculateEdgeValues_ConnectionsWeightedSingle graph NUM_SCORE_STEPS currentCursor true
#endif

    result

// ----------------------------------------------------------------------------
// ------ Example graph functions ------
// ----------------------------------------------------------------------------

// Generates the graph for this example.
let generateFreshGraph (): AppGraph =
    let mutable graph = new BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>> ()

    // Create the vertices for the graph.
    let vzero = newVert("zero", 0)
    let vone = newVert("one", 1)
    let vtwo = newVert("two", 2)
    let vthree = newVert("three", 3)
    let vfortytwo = newVert("forty-two", 42)

    // Attempt to add the vertices to the graph.
    tryAddVertex (graph, vzero)
    tryAddVertex (graph, vone)
    tryAddVertex (graph, vtwo)
    tryAddVertex (graph, vthree)
    tryAddVertex (graph, vfortytwo)

    // Attempt to add edges to the graph.
    tryAddEdge (graph, (newEdge (vzero, vone, "add_one")))
    tryAddEdge (graph, (newEdge (vzero, vtwo, "add_two")))
    tryAddEdge (graph, (newEdge (vtwo, vthree, "two_three")))
    tryAddEdge (graph, (newEdge (vone, vthree, "one_three")))
    tryAddEdge (graph, (newEdge (vthree, vfortytwo, "end")))

    // Go through all the edges in the graph, and set their value to the number
    // of connected edges of the target vertex.
    //calculateEdgeValues_Connections(g)
#if DEBUG
    //calculateEdgeValues_ConnectionsWeighted_BFS graph NUM_SCORE_STEPS
    calculateEdgeValues_ConnectionsWeighted_BFS graph NUM_SCORE_STEPS
#else
    calculateEdgeValues_ConnectionsWeighted graph NUM_SCORE_STEPS true
#endif

    // Return the completed graph.
    graph

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

// ------ Functions for moving based on graph metadata. ------

// Checks the loaded metadata for the specified vertex (using its name as the
// identifier).
let findMatchingMetadataRow (vertex: Vert) mdata =
  List.tryFind (fun (row : ExampleInfo) -> row.Name.ToLower().Equals(vertex.Tag.ToLower())) mdata

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

// Attempts to move to a vertex meeting the specified query conditions.
// Only accepts a single query that specifies a property to search on and a
// value that that property contains.
let findFirstWithMetadata (metadata : ExampleInfo list) (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) zipper (query : Query) =
    // Set up the function for filtering with the query.
    let filterQuery = findFirstWithMetadata_filter metadata query
    // Call the movement function for a matching vertex.
    moveAlongFirstMatchingVertex(graph, !zipper, filterQuery)

// Attempts to move to a vertex meeting the specified query conditions.
// Accepts a list of queries (consisting of the property and the value they
// should contain) and the logical operator to use with those queries.
let findFirstWithMetadataMulti (metadata : ExampleInfo list) (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) zipper (queryList : MultiQuery) =
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

// ----------------------------------------------------------------------------
// ------ Server functions ------
// ----------------------------------------------------------------------------

// Handles a move request. Attempts to complete the request, failing with a 404
// response if the operation did not return a valid vertex.
let moveRoute metadata graph zipper request =
    let moveOp =
      request.rawForm
      |> UTF8.toString
      |> Json.deserialize<MoveOp>

    printfn "Received move operation: %s" (moveOp.ToString())

    // Determine the move operation to use.
    let moveRes = match moveOp with
                    | ToVertex vval -> moveToVertex (graph, !zipper, vval)
                    | AlongEdge eege -> moveAlongEdge (graph, !zipper, newEdge(newVert("dummy",-1),newVert("dummy",-2), eege))
                    | FirstEdge s -> moveAlongFirstMatchingEdge(graph, !zipper, (fun ve -> ve.Tag = s))
                    | FirstVertex co -> moveAlongFirstMatchingVertex(graph, !zipper, mkCompOp co)
                    | Back -> Some(moveBack !zipper)
                    | MetadataSearch query -> findFirstWithMetadata metadata graph zipper query
                    | MetadataSearchMulti queryList -> findFirstWithMetadataMulti metadata graph zipper queryList
                    | ForceToVertex vval -> forceMoveToVertex (graph, !zipper, vval)
                    | NextMostConnected -> findNextMostConnected (graph, zipper)
                    | Forward -> Some(moveForward !zipper)
                    | GoToHistory i -> Some(moveToHistoryIndex !zipper i)
                    | NextHighestQueryScore query -> findNextHighestQueryScore (graph, zipper, (findFirstWithMetadata_filter metadata) query)

    // Attempt to complete the move operation.
    match moveRes with
        | None -> NOT_FOUND "No valid vertex to move to."
        | Some newZipper ->
            zipper := newZipper
            OK (Json.serialize (!zipper).Cursor)

// Handles a whereami request by returning the current zipper cursor.
let whereami zipper _request = OK (Json.serialize (!zipper).Cursor)

// Returns all edges connected to the current vertex.
let connectedRoute (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) zipper _request =
    //printfn "Received wheretogo operation."
    let edges = graph.OutEdges((!zipper).Cursor)
    let edgesMap = Seq.map (fun (item : TaggedValueEdge<Vert,string,float>) -> { Start = item.Source; End = item.Target; Tag = item.Tag; Value = item.Value }) edges
    OK (Json.serialize { Edges = Seq.toList edgesMap })

// Returns the current vertex and all edges connected to it.
let getGraphRoute (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) zipper _request =
    printfn "Received getGraph operation."
    let currentVertex = (!zipper).Cursor
    let edges = Seq.filter (fun (item : TaggedValueEdge<Vert,string,float>) -> item.Source.Equals(currentVertex) || item.Target.Equals(currentVertex)) graph.Edges
    let edgesMap = Seq.map (fun (item : TaggedValueEdge<Vert,string,float>) -> { Start = item.Source; End = item.Target; Tag = item.Tag; Value = item.Value }) edges
    OK (Json.serialize { Vertex = (!zipper).Cursor; Edges = Seq.toList edgesMap; History = (!zipper).VertHistory; HistoryIndex = (!zipper).HistoryIndex })

// Adds CORS headers to allow cross origin requests.
//https://stackoverflow.com/questions/44359375/allow-multiple-headers-with-cors-in-suave
let setCORSHeaders =
    addHeader "Access-Control-Allow-Origin" "*"
    >=> addHeader "Access-Control-Allow-Headers" "content-type"

// Returns the graph's metadata.
let getMetadataRoute _request =
    printfn "Received getMetadata operation."
    OK (Json.serialize (GenerateExampleMetadata()))

// Returns all the vertices in the current graph.
let getVerticesRoute (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) _request =
    printfn "Received getVertices operation."
    OK (Json.serialize (Seq.toList graph.Vertices))

// Returns all the edges in the current graph.
let getEdgesRoute (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) _request =
    printfn "Received getEdges operation."
    let edgesMap = Seq.map (fun (item : TaggedValueEdge<Vert,string,float>) -> { Start = item.Source; End = item.Target; Tag = item.Tag; Value = item.Value }) graph.Edges
    OK (Json.serialize (Seq.toList edgesMap))

// Handles the routes for the example.
let app graph zipper =
    choose

      // Request to move the current zipper cursor.
      [ POST >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/move" >=> request (moveRoute (GenerateExampleMetadata()) graph zipper) ] )

        // Request to return the current zipper cursor.
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/whereami" >=> request (whereami zipper) ] )

        // Request to show all valid destinations from the current zipper
        // cursor.
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/wheretogo" >=> request (connectedRoute graph zipper) ] )

        // Request to return the current zipper cursor and all vertices
        // connected to it. Includes vertices that have an ingoing edge to
        // the current cursor.
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/getGraph" >=> request (getGraphRoute graph zipper) ] )

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
          [ path "/getVertices" >=> request (getVerticesRoute graph) ] )

        // Request to get a list of all edges in the current graph.
        GET >=> fun context ->
                context |> (
                    setCORSHeaders
                    >=> choose
          [ path "/getEdges" >=> request (getEdgesRoute graph) ] ) ]

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
    let mutable graph = generateFreshGraph()
    printfn "Graph Status vertices: %i, edges: %i" (graph.VertexCount) (graph.EdgeCount)
    let root = newVert("zero", 0)
    printfn "Root JSON: %s" (Json.serialize root)
    let mutable freshZip = ref (createZipper root)
    startWebServer defaultConfig (app graph freshZip)
    0
