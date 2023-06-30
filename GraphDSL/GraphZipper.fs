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
      History: list<Move<'V, 'E>>
// Added by Samuel Smith n7581769.
// A list of vertices that have been visited (to enable checking what's been
// visited easily).
      VertHistory: list<'V>
// The current position in the history we're at. Used to enable going back
// without clearing the history.
      HistoryIndex: int }

// Added by Samuel Smith n7581769.
// Helper function to get last N elements of a list.
// https://stackoverflow.com/questions/18670134/how-can-i-take-last-n-items-from-a-list-in-f
let last n xs = List.toSeq xs |> Seq.skip (xs.Length - n) |> Seq.toList

// A helper function to perform a given movement on the graph and record that in the history.
// Modified by Samuel Smith n7581769: If not at the end of the history, any
// history past the current point is removed.
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
          History = (cons z.Cursor next) :: last z.HistoryIndex z.History
          VertHistory = next.Target :: last (z.HistoryIndex + 1) z.VertHistory
          HistoryIndex = z.HistoryIndex + 1 })

// Create the zipper from the given starting point
let createZipper (a: 'V) : Zipper<'V, 'E> = { Cursor = a; History = []; VertHistory = [a]; HistoryIndex = 0 }

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

// Added by Samuel Smith n7581769.
// Move to an arbitrary point in the history.
// If given a integer higher than the number of history entries, defaults to
// the last history item, and defaults to the starting vertex if given a
// negative integer.
let moveToHistoryIndex (z: Zipper<'V, 'E>) i : Zipper<'V, 'E> =
    // Thanks to the vertex list, we can just jump to the vertex directly - no
    // need to interpret the move history list.
    let newZ =
        // If given the same index as the zipper is already on, just return the
        // existing zipper.
        if (i.Equals(z.HistoryIndex)) then
            z
        // If given an index greater than the length of the history, cap it at
        // the latest item in the history.
        else if (i > z.VertHistory.Length - 1) then
                { Cursor = List.item (0) z.VertHistory;
                History = z.History;
                VertHistory = z.VertHistory;
                HistoryIndex = (z.VertHistory.Length - 1)}
        // Otherwise set the cursor and history index accordingly. The history
        // lists start with the latest item, so we need to subtract the
        // supplied integer from the length to get the correct index for the
        // list.
        // If given a negative index, cap it at the first item in the history.
        else
                { Cursor = List.item ((z.VertHistory.Length - 1) - (max 0 i)) z.VertHistory;
                History = z.History;
                VertHistory = z.VertHistory;
                HistoryIndex = (max 0 i)}
    newZ

// Move one step "back" in the history.
// Modified by Samuel Smith n7581769: The history is not erased when moving
// back. Also uses the new moveToHistoryIndex function instead of a complex
// match in the movement history.
let moveBack (z: Zipper<'V, 'E>) : Zipper<'V, 'E> =
    (*match last (max 0 z.HistoryIndex) z.History with
    | [] -> z
    | h :: t ->
        { Cursor =
            match h with
            | ToVertex (v, _) -> v
            | AlongEdge (v, _) -> v
            | FirstEdgeMatching (v, _, _) -> v
            | FirstVertexMatching (v, _, _) -> v
            | ForceToVertex (v, _) -> v
          History = z.History
          VertHistory = z.VertHistory
          HistoryIndex = z.HistoryIndex - 1}*)
    moveToHistoryIndex z (z.HistoryIndex - 1)


// Added by Samuel Smith n7581769.
// A helper function to perform a given movement on the graph and record that
// in the history.
// This will allow a movement to a non-connected vertex as long as the vertex
// exists.
// If not at the end of the history, any history past the current point is
// removed.
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
          History = (cons z.Cursor (Seq.first g.Edges).Value) :: last z.HistoryIndex z.History
          VertHistory = next :: last (z.HistoryIndex + 1) z.VertHistory
          HistoryIndex = z.HistoryIndex + 1 })

// Added by Samuel Smith n7581769.
// From a Vertex, try to move to a specific known vertex.
// This will allow a movement to a non-connected vertex as long as the vertex exists.
let forceMoveToVertex (g: BidirectionalGraph<'V, 'E>, z: Zipper<'V, 'E>, v: 'V) : Option<Zipper<'V, 'E>> =
    tryForceMovement (g, z, v, (fun c n -> ForceToVertex(c, v)))

// Added by Samuel Smith n7581769.
// Move one step forward in the history.
// Essentially uses the above function to move one higher than the current
// history index.
let moveForward (z: Zipper<'V, 'E>) : Zipper<'V, 'E> =
    moveToHistoryIndex z (z.HistoryIndex + 1)