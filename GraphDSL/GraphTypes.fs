module GraphDSL.Types

open System

// QuikGraph package and its modules used for creating a directed graph.
open QuikGraph

// ----------------------------------------------------------------------------
// ------ Types ------
// ----------------------------------------------------------------------------

// A new edge type for QuikGraph that includes a value.
type TaggedValueEdge<'Vertex, 'Tag, 'Value>(source : 'Vertex, target : 'Vertex, tag : 'Tag, value : 'Value) =

    // Use the original TaggedEdge from QuikGraph as a base.
    inherit TaggedEdge<'Vertex, 'Tag>(source, target, tag)

    // Event for informing if the value has changed (since TaggedEdge has it
    // for the tag).
    let valueChanged = new Event<_>()

    // The value given to the edge.
    let mutable value = value

    // Print the value along with the original string of TaggedEdge.
    override _.ToString() = String.Format("{0} (Value: {1})", base.ToString(), value)

    // Public getter and setter for Value; triggers the valueChanged event when
    // a new value is added.
    member this.Value
        with get () = value
        and set (newValue) =
            // I could not figure out how to make this return without sending
            // an event if the value to set is the same. Attempting to use an
            // if or match statement returned the error "A type paramater is
            // missing a constraint". I only added the event for consistency
            // with the other QuikGraph edge types, so it's not too important
            // and I've ignored that case for the moment.
            value <- newValue;
            valueChanged.Trigger()

    // Public event that can be used to receive notifications that the value on
    // the edge has changed.
    [<CLIEvent>]
    member this.ValueChanged = valueChanged.Publish

    // Returns an anonymous record version of the edge, for export into JSON
    // (as FSharp.Json can't serialize an edge on its own.)
    member this.ToRecord () =
        {|
            Source = this.Source;
            Target = this.Target;
            Tag = this.Tag;
            Value = this.Value;
        |}

// Specifies the vertex used for the bidirectional graph. Each vertex has a
// string tag to name it, and a integer value that is used as an identifier.
type Vert =
    {
        Tag: string
        Value: int
    }

// Specifies the edge used for the QuikGraph.
type AppEdge = TaggedValueEdge<Vert,string,float>

// Specifies the configuration for the QuikGraph used.
type AppGraph = BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>

// ----------------------------------------------------------------------------
// ------ Graph creation functions ------
// ----------------------------------------------------------------------------

// Creates a new edge from the first specified vertex to the second with the
// specified tag.
let newEdge sourceVert targetVert tag value : TaggedValueEdge<Vert, string, float> =
    new TaggedValueEdge<Vert, string, float>(sourceVert,targetVert,tag,value)

// Creates a new vertex with the specified tag and value.
let newVert tag value : Vert =
    { Tag = tag; Value = value }

// Attempts to add an edge to the graph.
let tryAddEdge (edge: AppEdge) (graph: AppGraph) =
    if (graph.ContainsEdge(edge)) then
#if LOGGING && VERBOSE
        printfn "Edge %s (%i) to %s (%i) is already in the graph" edge.Source.Tag edge.Source.Value edge.Target.Tag edge.Target.Value
#endif
        graph
    else
        let newGraph = graph.Clone()
        let success = newGraph.AddEdge(edge)
        // If the edge add operation returned false, the edge wasn't added.
        if (not success) then
            printfn "Failed to add edge %s (%i) to %s (%i)" edge.Source.Tag edge.Source.Value edge.Target.Tag edge.Target.Value

            // Return the original graph in this case.
            graph
        else
#if LOGGING && VERBOSE
            printfn "Successfully added edge %s (%i) to %s (%i)" edge.Source.Tag edge.Source.Value edge.Target.Tag edge.Target.Value
#endif
            newGraph

// Attempts to add a vertex to the graph.
let tryAddVertex (vert: Vert) (graph: AppGraph) =
    // Check if the vertex already exists first, and if it does, return
    // immediately with the existing graph.
    if (graph.ContainsVertex(vert)) then
#if LOGGING && VERBOSE
        printfn "Vertex %s (%i) is already in the graph" vert.Tag vert.Value
#endif
        graph
    else
        let newGraph = graph.Clone()
        let success = newGraph.AddVertex(vert)
        // If the vertex add operation returned false, the vertex wasn't added.
        if (not success) then
            printfn "Failed to add vertex %s (%i)" vert.Tag vert.Value

            // Return the original graph in this case.
            graph
        else
#if LOGGING && VERBOSE
            printfn "Successfully added vertex %s (%i)" vert.Tag vert.Value
#endif
            newGraph

// Attempts to add multiple edges to the graph.
// Duplicate edges will be ignored, but an error with adding any edge will stop
// the whole operation and return the original graph.
let tryAddEdges (edges: AppEdge list) (graph : AppGraph) =
    // First, make sure at least one edge is not already in the graph.
    match List.tryFind (fun edge -> not (graph.ContainsEdge(edge))) edges with
        | None ->
#if LOGGING && VERBOSE
                printfn "All %i edge(s) are already in the graph" (List.length edges)
#endif
                graph
        // If there are at least some edges that can be added, attempt to do
        // so.
        | Some _ ->
                let newGraph = graph.Clone()

                // If AddEdge returns false and the edge doesn't exist, there
                // was an error, so revert back to the original graph.
                match List.tryFind (fun edge -> not (newGraph.AddEdge(edge)) && not (newGraph.ContainsEdge(edge))) edges with
                | None ->
#if LOGGING && VERBOSE
                    printfn "Successfully added %i edges" (List.length edges)
#endif
                    newGraph
                | Some edge ->
                    printfn "Failed to add edge %s (%i) to %s (%i). Reverting any changes made" edge.Source.Tag edge.Source.Value edge.Target.Tag edge.Target.Value
                    graph

// Attempts to add multiple vertices to the graph.
// Duplicate vertices will be ignored, but an error with adding any vertex will
// stop the whole operation and return the original graph.
let tryAddVertices (verts: Vert list) (graph : AppGraph) =
    // First, make sure at least one edge is not already in the graph.
    match List.tryFind (fun vert -> not (graph.ContainsVertex(vert))) verts with
        | None ->
#if LOGGING && VERBOSE
                printfn "All %i vertices are already in the graph" (List.length verts)
#endif
                graph
        // If there are at least some vertices that can be added, attempt to
        // do so.
        | Some _ ->
                let newGraph = graph.Clone()

                // If AddVertex returns false and the vertex doesn't exist,
                // there was an error, so revert back to the original graph.
                match List.tryFind (fun vert -> not (newGraph.AddVertex(vert)) && not (newGraph.ContainsVertex(vert))) verts with
                | None ->
#if LOGGING && VERBOSE
                    printfn "Successfully added %i vertices" (List.length verts)
#endif
                    newGraph
                | Some vert ->
                    printfn "Failed to add vertex %s (%i). Reverting any changes made" vert.Tag vert.Value
                    graph

// Creates a clone of a graph.
// QuikGraph's clone doesn't create new instances of the vertices and edges
// (shown by changing edge values - they will change on both the original and
// the clone) so this creates a "deep" copy.
let deepCloneAppGraph (graph : AppGraph) =
#if LOGGING
    printfn "Making deep clone of graph..."
#if VERBOSE
    printfn "Original graph has %i vertices and %i edges" (Seq.length graph.Vertices) (Seq.length graph.Edges)
#endif
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
#endif

    // Since we're making a brand new graph that no other function should see
    // until it's done, we don't need to worry about side effects during the
    // process.
    let newGraph = new AppGraph()
    let newVerts = Seq.map (fun vert -> newVert vert.Tag vert.Value) graph.Vertices
    let newEdges = Seq.map (fun (edge : AppEdge) -> newEdge (newVert edge.Source.Tag edge.Source.Value) (newVert edge.Target.Tag edge.Target.Value) edge.Tag edge.Value) graph.Edges
    Seq.iter (fun vert -> newGraph.AddVertex(vert) |> ignore) newVerts
    Seq.iter (fun edge -> newGraph.AddEdge(edge) |> ignore) newEdges
    if (Seq.length newGraph.Vertices <> Seq.length graph.Vertices || Seq.length newGraph.Edges <> Seq.length graph.Edges) then
        failwith "Failed to deep clone graph - the number of edges or vertices is different."
#if LOGGING
    stopWatch.Stop()
    let ms = stopWatch.Elapsed.TotalMilliseconds
#if VERBOSE
    printfn "Cloned graph has %i vertices and %i edges" (Seq.length newGraph.Vertices) (Seq.length newGraph.Edges)
#endif
    printfn "Cloned graph in %f ms." ms
#endif
    newGraph