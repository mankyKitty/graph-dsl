// My unfamiliarity with the tower of abstractions used in F# makes the following a bit muddy.
module GraphDSL.Zipper

open QuikGraph

// The moves that you can make, each one denotes a single step. More complicated moves are completed
// by composing these individual moves together.
type Move<'V, 'E> =
    | ToVertex of 'V * 'V
    | AlongEdge of 'V * 'E
    | FirstEdgeMatching of 'V * ('E -> bool) * 'E
    | FirstVertexMatching of 'V * ('V -> bool) * 'V

// The zipper structure itself keeps a reference to the current position The history should probably
// be nothing but references as well to avoid copying entire sections of the graph but I thought it
// might obscure the overall intent.
type Zipper<'V, 'E> =
    { Cursor: 'V
      History: list<Move<'V, 'E>> }

// A helper function to perform a given movement on the graph and record that in the history.

// Create the zipper from the given starting point
let createZipper (a: 'V) : Zipper<'V, 'E> = { Cursor = a; History = [] }

// From a Vertex, try to move to a specific known vertex that is expected to be connected to this one.
let moveToVertex (g: BidirectionalGraph<'V, 'E>, z: Zipper<'V, 'E>, v: 'V) : Option<Zipper<'V, 'E>> =
    match g.OutEdges(z.Cursor) |> Seq.tryFind (fun e -> e.Target = v) with
    | None -> None
    | Some (next) ->
        Some(
            { Cursor = next.Target
              History = ((ToVertex(z.Cursor, v)) :: z.History) }
        )

// Move along a known edge that is expected to be connected to the current vertex.
let moveAlongEdge (g: BidirectionalGraph<'V, 'E>, z: Zipper<'V, 'E>, along: 'E) : Option<Zipper<'V, 'E>> =
    match
        g.OutEdges(z.Cursor)
        |> Seq.tryFind (fun e -> e.Source = along.Source && e.Target = along.Target)
    with
    | None -> None
    | Some (next) ->
        Some(
            { Cursor = next.Target
              History = (AlongEdge(z.Cursor, along) :: z.History) }
        )

// Inspect the edges leading away from this Vertex and move along the first one that matches the given function.
let moveAlongFirstMatchingEdge
    (
        g: BidirectionalGraph<'V, 'E>,
        z: Zipper<'V, 'E>,
        f: ('E -> bool)
    ) : Option<Zipper<'V, 'E>> =
    match g.OutEdges(z.Cursor) |> Seq.tryFind (f) with
    | None -> None
    | Some (next) ->
        Some(
            { Cursor = next.Target
              History = (FirstEdgeMatching(z.Cursor, f, next) :: z.History) }
        )

// Inspect all vertexs one step away from the current not and move to the first one that matches the given function.
let moveAlongFirstMatchingVertex
    (
        g: BidirectionalGraph<'V, 'E>,
        z: Zipper<'V, 'E>,
        f: ('V -> bool)
    ) : Option<Zipper<'V, 'E>> =
    match g.OutEdges(z.Cursor) |> Seq.tryFind (fun e -> f (e.Target)) with
    | None -> None
    | Some (next) ->
        Some(
            { Cursor = next.Target
              History = (FirstVertexMatching(z.Cursor, f, next.Target) :: z.History) }
        )

// Move one step "back". This reverts the most recent single step.
let moveBack (z: Zipper<'V, 'E>) : Zipper<'V, 'E> =
    match z.History with
    | [] -> z
    | h :: t ->
        { Cursor =
            match h with
            | ToVertex (v, _) -> v
            | AlongEdge (v, _) -> v
            | FirstEdgeMatching (v, _, _) -> v
            | FirstVertexMatching (v, _, _) -> v
          History = t }
