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
    | ForceToVertex of 'V * 'V

// The zipper structure itself keeps a reference to the current position The history should probably
// be nothing but references as well to avoid copying entire sections of the graph but I thought it
// might obscure the overall intent.
type Zipper<'V, 'E> =
    { Cursor: 'V
      History: list<Move<'V, 'E>> }

// A helper function to perform a given movement on the graph and record that in the history.
let tryMovement
    (
        g: BidirectionalGraph<'V, 'E>,
        z: Zipper<'V, 'E>,
        fn: ('E -> bool),
        cons: ('V -> 'E -> Move<'V, 'E>)
    ) : Option<Zipper<'V, 'E>> =
    g.OutEdges(z.Cursor)
    |> Seq.tryFind fn
    |> Option.map (fun next ->
        { Cursor = next.Target
          History = (cons z.Cursor next) :: z.History })

// Create the zipper from the given starting point
let createZipper (a: 'V) : Zipper<'V, 'E> = { Cursor = a; History = [] }

// From a Vertex, try to move to a specific known vertex that is expected to be connected to this one.
let moveToVertex (g: BidirectionalGraph<'V, 'E>, z: Zipper<'V, 'E>, v: 'V) : Option<Zipper<'V, 'E>> =
    tryMovement (g, z, (fun e -> e.Target = v), (fun c n -> ToVertex(c, v)))

// Move along a known edge that is expected to be connected to the current vertex.
let moveAlongEdge (g: BidirectionalGraph<'V, 'E>, z: Zipper<'V, 'E>, along: 'E) : Option<Zipper<'V, 'E>> =
    tryMovement (
        g,
        z,
        (fun e -> e.Source = along.Source && e.Target = along.Target),
        (fun c n -> AlongEdge(z.Cursor, along))
    )

// Inspect the edges leading away from this Vertex and move along the first one that matches the given function.
let moveAlongFirstMatchingEdge
    (
        g: BidirectionalGraph<'V, 'E>,
        z: Zipper<'V, 'E>,
        f: ('E -> bool)
    ) : Option<Zipper<'V, 'E>> =
    tryMovement (g, z, f, (fun c next -> FirstEdgeMatching(c, f, next)))

// Inspect all vertexs one step away from the current not and move to the first one that matches the given function.
let moveAlongFirstMatchingVertex
    (
        g: BidirectionalGraph<'V, 'E>,
        z: Zipper<'V, 'E>,
        f: ('V -> bool)
    ) : Option<Zipper<'V, 'E>> =
    tryMovement (g, z, (fun e -> f (e.Target)), (fun c n -> FirstVertexMatching(c, f, n.Target)))

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
            | ForceToVertex (v, _) -> v
          History = t }

// Added by Samuel Smith n7581769.
// A helper function to perform a given movement on the graph and record that in the history.
// This will allow a movement to a non-connected vertex as long as the vertex exists.
let tryForceMovement
    (
        g: BidirectionalGraph<'V, 'E>,
        z: Zipper<'V, 'E>,
        v: 'V,
        cons: ('V -> 'E -> Move<'V, 'E>)
    ) : Option<Zipper<'V, 'E>> =
    g.Vertices
    |> Seq.tryFind (fun vert -> vert = v)
    |> Option.map (fun next ->
        { Cursor = next
          // I've used the first edge in the graph as a dummy edge to use in the
          // cons function since it expects an edge despite not using it and I'm
          // not sure how to change it to not need one. :P
          History = (cons z.Cursor (Seq.first g.Edges).Value) :: z.History })

// Added by Samuel Smith n7581769.
// From a Vertex, try to move to a specific known vertex.
// This will allow a movement to a non-connected vertex as long as the vertex exists.
let forceMoveToVertex (g: BidirectionalGraph<'V, 'E>, z: Zipper<'V, 'E>, v: 'V) : Option<Zipper<'V, 'E>> =
    tryForceMovement (g, z, v, (fun c n -> ForceToVertex(c, v)))
