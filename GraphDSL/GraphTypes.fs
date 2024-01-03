﻿module GraphDSL.Types

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
    let newGraph = graph.Clone()
    let success = newGraph.AddEdge(edge)
    // If the edge add operation returned false (i.e. the edge wasn't added)
    // then check if the edge is in the graph; this means that the edge was
    // already in the graph.
    if (not success) then
        if (newGraph.ContainsEdge(edge)) then
#if LOGGING && VERBOSE
            printfn "Edge %s (%i) to %s (%i) already exists" edge.Source.Tag edge.Source.Value edge.Target.Tag edge.Target.Value
#endif
            newGraph
        // Otherwise, the operation failed for another reason and there's a
        // problem with creating the graph.
        else
            printfn "Failed to add edge %s (%i) to %s (%i)" edge.Source.Tag edge.Source.Value edge.Target.Tag edge.Target.Value
            newGraph
    else
#if LOGGING && VERBOSE
        printfn "Successfully added edge %s (%i) to %s (%i)" edge.Source.Tag edge.Source.Value edge.Target.Tag edge.Target.Value
        //printfn "Original graph has %i edges" (Seq.length graph.Edges)
        //printfn "New graph has %i edges" (Seq.length newGraph.Edges)
#endif
        newGraph

// Attempts to add a avertex to the graph.
let tryAddVertex (vert: Vert) (graph: AppGraph) =
    let newGraph = graph.Clone()
    let success = newGraph.AddVertex(vert)
    // If the vertex add operation returned false (i.e. the vertex wasn't
    // added) then check if the vertex is in the graph; this means that the
    // vertex was already in the graph.
    if (not success) then
        if (newGraph.ContainsVertex(vert)) then
#if LOGGING && VERBOSE
            printfn "Vertex %s (%i) already exists" vert.Tag vert.Value
#endif
            newGraph
        // Otherwise, the operation failed for another reason and there's a
        // problem with creating the graph.
        else
            printfn "Failed to add vertex %s (%i)" vert.Tag vert.Value
            newGraph
    else
#if LOGGING && VERBOSE
        printfn "Successfully added vertex %s (%i)" vert.Tag vert.Value
        //printfn "Original graph has %i vertices" (Seq.length graph.Vertices)
        //printfn "New graph has %i vertices" (Seq.length newGraph.Vertices)
#endif
        newGraph

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
    // Go through all the edges first and create copies of them anad their
    // vertices.
    let newGraph = Seq.fold (fun graph (edge : AppEdge) ->
        let copySource = newVert edge.Source.Tag edge.Source.Value
        let copyTarget = newVert edge.Target.Tag edge.Target.Value
        let copyEdge = newEdge copySource copyTarget edge.Tag edge.Value
        tryAddVertex copySource graph
            |> tryAddVertex copyTarget
            |> tryAddEdge copyEdge) (new AppGraph()) graph.Edges
    // Then add any vertices that remain (i.e. any vertices that aren't
    // connected to anything else).
                |> Seq.fold (fun graph (vert : Vert) ->
                    let copyVert = newVert vert.Tag vert.Value
                    tryAddVertex copyVert graph) <| (Seq.filter (fun vert -> Seq.length (graph.OutEdges(vert)) < 1) graph.Vertices)
#if LOGGING
    stopWatch.Stop()
    let ms = stopWatch.Elapsed.TotalMilliseconds
#if VERBOSE
    printfn "Cloned graph has %i vertices and %i edges" (Seq.length newGraph.Vertices) (Seq.length newGraph.Edges)
#endif
    printfn "Cloned graph in %f ms." ms
#endif
    newGraph