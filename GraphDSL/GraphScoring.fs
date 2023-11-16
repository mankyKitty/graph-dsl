module GraphDSL.Scoring

#if DEBUG
// If debugging, use the stopwatches provided by this module for timing.
open System.Diagnostics
#endif

// GraphDSL types and functions used in this file.
open GraphDSL.Types

// QuikGraph package and its modules used for creating a directed graph.
open QuikGraph
open QuikGraph.Algorithms.Search
open QuikGraph.Algorithms.ShortestPath

// ----------------------------------------------------------------------------
// ------ Global values ------
// ----------------------------------------------------------------------------

// Values that are reused throughout this file.
module ScoringValues =

    // Default value for edges.
    let DEFAULT_EDGE_VALUE = 1.0

    // Number of steps to use when generating weighted scores.
    let NUM_SCORE_STEPS = 4

#if DEBUG
    // "Step" value to use when determining weighted scores. Each step further
    // the first will have the score contributed by each connection decreased
    // by this value multiplied by the number of steps away.
    // For example:
    // - Number of steps: 2, step base scores: 1.0, 0.75.
    // - Number of steps: 3, step base scores: 1.0, 0.75, 0.5.
    // - Number of steps: 4, step base scores: 1.0, 0.75, 0.5, 0.25.
    // If the number of steps (above) is greater than 4, then the regular
    // expression that scales the weighted scores by the total number of edges
    // is used.
    let STEP_WEIGHT = 0.25
#endif

// ----------------------------------------------------------------------------
// ------ Scoring functions ------
// ----------------------------------------------------------------------------

// Functions that return a value for edges based on a simple condition.
module Basic = 

    // Resets all edge values to the default of 1.0.
    let resetEdgeValues (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) =
    #if DEBUG
        printfn "Resetting edge values..."
        let stopWatch = Stopwatch.StartNew()
    #endif
        Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Value <- ScoringValues.DEFAULT_EDGE_VALUE) graph.Edges
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
        let stopWatch = Stopwatch.StartNew()
    #endif
        Seq.iter (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Value <- Seq.length (graph.OutEdges(edge.Target))) graph.Edges
    #if DEBUG
        stopWatch.Stop()
        let ms = stopWatch.Elapsed.TotalMilliseconds
        printfn "Changed edge values in %f ms." ms
    #endif

// Functions that use depth first search to create a score based on vertices
// that are deeper in the graph than the edge's target.
module DFS = 

    // ------ Depth first traversal scoring based on graph connectedness ------

    // Recursive function for looking through vertices up to a certain number
    // of steps away, and adding up the score for all verticies based on their
    // connectedness, weighting the score on how far the vertex is from the
    // starting point.
    let rec private scoring_ConnectionsWeighted = fun (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps stepsToGo verts previousScore uniqueOnly (visitedVerts : Vert list ref) ->

            // If there are no more steps to go, stop travelling down edges.
            match stepsToGo with
            | 0 -> (previousScore)
            | _ -> (
                Seq.fold (fun (previousScore : float) (vert : Vert) ->
                // If this vertex has already been visited, don't go any further
                // down this path.
                if (Seq.contains(vert) (visitedVerts.Value) && uniqueOnly) then
                    previousScore
                // Otherwise add to the score based on the vertex's connectedness.
                else
                    // Only count edges that don't loop back to the same vertex.
                    let outgoingEdges = Seq.filter (fun (edge : TaggedValueEdge<Vert,string,float>) -> not (edge.Source.Equals(edge.Target))) (graph.OutEdges(vert))
                    // Each step after the first should have the score each
                    // connection contributes decreased.
    #if DEBUG
                    // If in debug mode, the weighting should decrease by the same
                    // amount per step to allow for better comparisons as we
                    // explore scoring possibilities.
                    // This only works up to a certain number of steps, however, so
                    // make sure this wouldn't cause odd results first. If it would,
                    // use the regular calculation (described below).
                    let scoreToAdd = if (ScoringValues.STEP_WEIGHT * float numSteps > 1) then
                                        (1.0/(float numSteps))  * (float stepsToGo) * float (Seq.length outgoingEdges)
                                        else
                                        ScoringValues.STEP_WEIGHT * (float stepsToGo) * float (Seq.length outgoingEdges)
    #else
                    // Each step after the first should have the score each
                    // connection contributes decreased.
                    // This will be scaled by the total number of steps. So for
                    // example, if there are three total steps, each edge will
                    // contribute 1.0 for the first step, 0.66... in the second
                    // step and 0.33... in the third step.
                    let scoreToAdd = (1.0/(float numSteps))  * (float stepsToGo) * float (Seq.length outgoingEdges)
    #endif
    #if DEBUG && VERBOSE
                    printfn "%s is %i away and so its connections can be counted - adding %f to score (currently %f)" vert.Tag (numSteps + 1 - stepsToGo) scoreToAdd previousScore
    #endif
                    let currentScore = previousScore + scoreToAdd
                    visitedVerts.Value <- List.append (visitedVerts.Value) [vert]
                    scoring_ConnectionsWeighted graph numSteps (stepsToGo - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) outgoingEdges) currentScore uniqueOnly visitedVerts
                ) previousScore verts
            )

    // Iterator function for connection weighted scoring.
    let private edgeIterator_ConnectionsWeighted (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) vert numSteps uniqueOnly (edge : TaggedValueEdge<Vert,string,float>) =
        // Only proceed with calculating the score if this edge isn't
        // a loop.
        if (edge.Source.Equals(edge.Target)) then
            ()
        else
    #if DEBUG && VERBOSE
            printfn "------ Calculating score for edge %s to %s ------" edge.Source.Tag edge.Target.Tag
    #endif
            // Initialise the list of visited verticies for this starting point.
            // At present, I'm not sure how to keep a record of what vertices have
            // been visited without using an array that can change outside the
            // function.
            let mutable visitedVerts = [vert; edge.Target]

            // Get the connectedness of the current vertex.
            // Only count edges that don't loop back to the same vertex.
            let outgoingEdges = Seq.filter (fun (edge : TaggedValueEdge<Vert,string,float>) -> not (edge.Source.Equals(edge.Target))) (graph.OutEdges(edge.Target))
            let startingScore = float (Seq.length outgoingEdges)
    #if DEBUG && VERBOSE
            printfn "Added %f to score from edges from %s" startingScore edge.Target.Tag
    #endif

            // Start the recursion for subsequent edges after this one.
            let score = scoring_ConnectionsWeighted graph numSteps (numSteps - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) outgoingEdges) startingScore uniqueOnly (ref visitedVerts)
            edge.Value <- score

    #if DEBUG && VERBOSE
            printfn "Edge score set to %f" score
    #endif
            ()

    // Calculates a weighted edge value based on connectedness, considering
    // vertices that are a certain number of steps away.
    // (Note: some innacuracies may result thanks to the method used to avoid
    // counting vertices more than once.)
    let calculateEdgeValues_ConnectionsWeighted (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps uniqueOnly =
    #if DEBUG
        printfn "Changing edge values to weighted score based on number of connections and %i steps..." numSteps
        let stopWatch = Stopwatch.StartNew()
    #endif

        // Recursive function for looking through vertices up to a certain number
        // of steps away, and adding up the score for all verticies based on their
        // connectedness, weighting the score on how far the vertex is from the
        // starting point.

        // For every vertex in the network...
        Seq.iter (fun (vert : Vert) ->
    #if DEBUG && VERBOSE
            printfn "------------ Calculating scores from vertex %s -----------" vert.Tag
    #endif

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
        let stopWatch = Stopwatch.StartNew()
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

    // ------ Depth first traversal scoring based on a given condition ------

    // Recursive function for looking through vertices up to a certain number
    // of steps away, and adding up the score for all verticies that meet a
    // given condition, weighting the score on how far the vertex is from the
    // starting point.
    let rec private scoring_ConditionWeighted = fun (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps condition stepsToGo verts previousScore (visitedVerts : Vert list ref) ->

        // If there are no more steps to go, stop travelling down edges.
        match stepsToGo with
        | 0 -> (previousScore)
        | _ -> (
            Seq.fold (fun (previousScore : float) (vert : Vert) ->
            // If this vertex has already been visited, don't go any further
            // down this path.
            if (Seq.contains(vert) (visitedVerts.Value)) then
                previousScore
            // If this vertex meets the condition, add the weighted score to
            // the accumulated score and add it as a visited vertex.
            // down this path.
            elif (condition vert) then
    #if DEBUG
                // If in debug mode, the weighting should decrease by the same
                // amount per step to allow for better comparisons as we
                // explore scoring possibilities.
                // This only works up to a certain number of steps, however, so
                // make sure this wouldn't cause odd results first. If it would,
                // use the regular calculation (described below).
                let scoreToAdd = if (ScoringValues.STEP_WEIGHT * float numSteps > 1) then
                                    ((1.0/(float numSteps)) * (float stepsToGo))
                                    else
                                    ScoringValues.STEP_WEIGHT * (float stepsToGo)
    #else
                // Each step after the first should have the score each
                // vertex fitting the conditions contributes decreased.
                // This will be scaled by the total number of steps. So for
                // example, if there are three total steps, each vertex will
                // contribute 1.0 for the first step, 0.66... in the second
                // step and 0.33... in the third step.
                let scoreToAdd = ((1.0/(float numSteps)) * (float stepsToGo))
    #endif
    #if DEBUG && VERBOSE
                printfn "%s is %i away and meets the condition, therefore it can be counted - adding %f to score (currently %f)" vert.Tag (numSteps + 1 - stepsToGo) scoreToAdd previousScore
    #endif
                let currentScore = previousScore + scoreToAdd
                visitedVerts.Value <- List.append (visitedVerts.Value) [vert]
                scoring_ConditionWeighted graph numSteps condition (stepsToGo - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) (graph.OutEdges(vert))) currentScore visitedVerts
            // Otherwise add it as a visited vertex but don't add to the score.
            else
                visitedVerts.Value <- List.append (visitedVerts.Value) [vert]
                scoring_ConditionWeighted graph numSteps condition (stepsToGo - 1) (Seq.map (fun (edge : TaggedValueEdge<Vert,string,float>) -> edge.Target) (graph.OutEdges(vert))) previousScore visitedVerts
                ) previousScore verts
            )

    // Iterator function for condition weighted scoring.
    let private edgeIterator_ConditionWeighted graph vert numSteps condition (edge : TaggedValueEdge<Vert,string,float>) =
        // Only proceed with calculating the score if this edge isn't a loop.
        if (edge.Source.Equals(edge.Target)) then
            ()
        else
    #if DEBUG && VERBOSE
            printfn "------ Calculating score for edge %s to %s ------" edge.Source.Tag edge.Target.Tag
    #endif
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
    #if DEBUG && VERBOSE
            printfn "Added %f to score from whether or not %s meets the condition" startingScore edge.Target.Tag
    #endif

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
        let stopWatch = Stopwatch.StartNew()
    #endif

        // For every vertex in the network...
        Seq.iter (fun (vert : Vert) ->
    #if DEBUG && VERBOSE
            printfn "------------ Calculating scores from vertex %s -----------" vert.Tag
    #endif

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
        let stopWatch = Stopwatch.StartNew()
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

// Functions that use breadth first search to create a score based on vertices
// that are deeper in the graph than the edge's target.
module BFS = 

    // ------ Breadth first traversal scoring based on graph connectedness ------

    // Iterator function for connection weighted BFS scoring.
    let private edgeIterator_ConnectionsWeighted graph vert numSteps (edge : TaggedValueEdge<Vert,string,float>) =
        // Only proceed with calculating the score if this edge isn't a loop.
        if (edge.Source.Equals(edge.Target)) then
            ()
        else
    #if DEBUG && VERBOSE
            printfn "------ Calculating score for edge %s to %s ------" edge.Source.Tag edge.Target.Tag
    #endif

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
    #if DEBUG && VERBOSE
            printfn "Added %f to score from edges from %s" score edge.Target.Tag
    #endif
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
                        // If in debug mode, the weighting should decrease by the
                        // same amount per step to allow for better comparisons as
                        // we explore scoring possibilities.
                        // This only works up to a certain number of steps,
                        // however, so make sure this wouldn't cause odd results
                        // first. If it would use the regular calculation (described below).
                        let scoreToAdd = if (ScoringValues.STEP_WEIGHT * float numSteps > 1) then
                                            (1.0/(float numSteps)) * ((float numSteps) - value) * float (Seq.length outgoingEdges)
                                            else
                                            ScoringValues.STEP_WEIGHT * ((float numSteps) - value) * float (Seq.length outgoingEdges)
    #else
                        // Each step after the first should have the score each
                        // connection contributes decreased.
                        // This will be scaled by the total number of steps. So for
                        // example, if there are three total steps, each edge will
                        // contribute 1.0 for the first step, 0.66... in the second
                        // step and 0.33... in the third step.
                        let scoreToAdd = (1.0/(float numSteps)) * ((float numSteps) - value) * float (Seq.length outgoingEdges)
    #endif
    #if DEBUG && VERBOSE
                        printfn "%s is %f (therefore %f total) away and so its connections can be counted - adding %f to score (currently %f)" currentVert.Tag value (value + 1.0) scoreToAdd score
    #endif
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

    #if DEBUG && VERBOSE
            printfn "Edge score set to %f" score
    #endif
            ()

    // Calculates a weighted edge value based on connectedness, considering
    // vertices that are a certain number of steps away.
    // Uses QuikGraph's breadth-first search function.
    let calculateEdgeValues_ConnectionsWeighted (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps =
    #if DEBUG
        printfn "Changing edge values to weighted score based on number of connections and %i steps..." numSteps
        let stopWatch = Stopwatch.StartNew()
    #endif

        // For every vertex in the network...
        Seq.iter (fun (vert : Vert) ->
    #if DEBUG && VERBOSE
            printfn "------------ Calculating scores from vertex %s -----------" vert.Tag
    #endif

            // Iterate through the adjacent edges first, so the scores can be
            // applied to each edge.
            Seq.iter (edgeIterator_ConnectionsWeighted graph vert numSteps) (graph.OutEdges(vert))
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
    let calculateEdgeValues_ConnectionsWeightedSingle (graph : BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>) numSteps (origin : Vert) =
    #if DEBUG
        printfn "Changing edge values to weighted score based on number of connections and %i steps..." numSteps
        let stopWatch = Stopwatch.StartNew()
    #endif

        // For the specified vertex...
        let vert = origin
    #if DEBUG && VERBOSE
        printfn "------------ Calculating scores from vertex %s -----------" vert.Tag
    #endif

        // Iterate through the adjacent edges first, so the scores can be applied
        // to each edge.
        Seq.iter (edgeIterator_ConnectionsWeighted graph vert numSteps) (graph.OutEdges(vert))
    #if DEBUG
        stopWatch.Stop()
        let ms = stopWatch.Elapsed.TotalMilliseconds
        printfn "Changed edge values in %f ms." ms
    #endif
        ()

    // ------ Breadth first traversal scoring based on a given condition ------
