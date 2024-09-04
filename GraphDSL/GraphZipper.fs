// -----------------------------------------------------------------------------
// Contains all types and functions relating to the Zipper, used to navigate
// graphs.
// -----------------------------------------------------------------------------

// My unfamiliarity with the tower of abstractions used in F# makes the following a bit muddy.
module GraphDSL.Zipper

open QuikGraph

// The moves that you can make, each one denotes a single step. More
// complicated moves are completed by composing these individual moves
// together.
type Move<'V, 'E> =
    | ToVertex of 'V * 'V
    | AlongEdge of 'V * 'E
    | FirstEdgeMatching of 'V * ('E -> bool) * 'E
    | FirstVertexMatching of 'V * ('V -> bool) * 'V
    | ForceToVertex of 'V * 'V

// The zipper structure itself keeps a reference to the current position.
// The history should probably be nothing but references as well to avoid copying
// entire sections of the graph but I thought it might obscure the overall intent.
type Zipper<'V, 'E> =
    {
        // The vertex that the zipper is currently at.
        Cursor: 'V

        // A list of the movement actions that have been performed using the
        // zipper. The first element is the most recent move.
        History: list<Move<'V, 'E>>

        // A list of vertices that have been visited (to enable checking what's
        // been visited easily). Like above, the most recent vertex is the first
        // element. The vertex the zipper started at is always included.
        VertHistory: list<'V>

        // The current position in the history the zipper is at. By default,
        // this is equal to the number of entries in the history, or 0 if there
        // are none. Instead of changing the actual history lists, this is used
        // to indicate a change in position if the back or forward commands are
        // used.
        HistoryIndex: int
    }

// Helper function to get last N elements of a list.
// It is based off the code from this Stack Overflow answer:
// https://stackoverflow.com/questions/18670134/how-can-i-take-last-n-items-from-a-list-in-f
let last n xs = List.toSeq xs |> Seq.skip (xs.Length - n) |> Seq.toList

// A helper function to perform a given movement on the graph and record that
// in the history. If not at the end of the history, any history past the
// current point is removed.
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
        {
            // Set the cursor to the target vertex.
            Cursor = next.Target

            // Add a new entry to the start of the movement history for this
            // move. Takes only the existing entries before the current history
            // index if the zipper is not at the end.
            History = (cons z.Cursor next) :: last z.HistoryIndex z.History

            // Does the same as above for the vertex history. One is added to
            // the history index when using the last function since the vertex
            // history always has an additional element (the first vertex).
            VertHistory = next.Target :: last (z.HistoryIndex + 1) z.VertHistory

            // Increment the history index by one to correspond to the new
            // entry.
            HistoryIndex = z.HistoryIndex + 1
        })

// Create a new zipper staring at the specified vertex.
let createZipper (a: 'V) : Zipper<'V, 'E> = { Cursor = a; History = []; VertHistory = [a]; HistoryIndex = 0 }

// Attempts to move from the current cursor to the given vertex in the given
// graph. There should be a valid edge connecting the source vertex to the
// target vertex.
let moveToVertex
    (
        g: BidirectionalGraph<'V, 'E>,
        z: Zipper<'V, 'E>, v: 'V
    ) :Option<Zipper<'V, 'E>> =
    tryMovement (g, z, (fun e -> e.Target = v), (fun c n -> ToVertex(c, v)))

// Attempts to move from the current cursor across the given edge in the given
// graph. The edge must be connected to the source vertex.
let moveAlongEdge
    (
        g: BidirectionalGraph<'V, 'E>,
        z: Zipper<'V, 'E>,
        along: 'E
    ) : Option<Zipper<'V, 'E>> =
    tryMovement (
        g,
        z,
        (fun e -> e.Source = along.Source && e.Target = along.Target),
        (fun c n -> AlongEdge(z.Cursor, along))
    )

// Attempts to move along the first edge conneted to the current cursor in the
// given graph that meets the specifed condition.
let moveAlongFirstMatchingEdge
    (
        g: BidirectionalGraph<'V, 'E>,
        z: Zipper<'V, 'E>,
        f: ('E -> bool)
    ) : Option<Zipper<'V, 'E>> =
    tryMovement (g, z, f, (fun c next -> FirstEdgeMatching(c, f, next)))

// Attempts to move to the first vertex connected to the current cursor in the
// given graph that meets the specified condition.
let moveAlongFirstMatchingVertex
    (
        g: BidirectionalGraph<'V, 'E>,
        z: Zipper<'V, 'E>,
        f: ('V -> bool)
    ) : Option<Zipper<'V, 'E>> =
    tryMovement (g, z, (fun e -> f (e.Target)), (fun c n -> FirstVertexMatching(c, f, n.Target)))

// Move to an arbitrary point in the history.
// If given a integer higher than the number of history entries, defaults to
// the last history item, and defaults to the starting vertex if given a
// negative integer.
let moveToHistoryIndex i (z: Zipper<'V, 'E>) : Zipper<'V, 'E> =

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

// Move one step "back" in the history. Originally, this was a complicated
// function that did the reverse of the previous move, but now it simply
// decreases the current history index by one (calling the associated function
// to move to an arbitrary point in the history), preseving the current history
// in the process.
let moveBack (z: Zipper<'V, 'E>) : Zipper<'V, 'E> =
    moveToHistoryIndex (z.HistoryIndex - 1) z

// A helper function to perform a given movement on the graph and record that
// in the history. If not at the end of the history, any history past the
// current point is removed.
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

          // I've used the first edge in the graph as a dummy edge to use in
          // the cons function since it expects an edge despite not using it
          // and I'm not sure how to change it to not need one.
          History = (cons z.Cursor (Seq.first g.Edges).Value) :: last z.HistoryIndex z.History
          VertHistory = next :: last (z.HistoryIndex + 1) z.VertHistory
          HistoryIndex = z.HistoryIndex + 1 })

// From a Vertex, try to move to a specific known vertex.
// This will allow a movement to a non-connected vertex as long as the vertex
// exists.
let forceMoveToVertex
    (
        g: BidirectionalGraph<'V, 'E>,
        z: Zipper<'V, 'E>,
        v: 'V
    ) : Option<Zipper<'V, 'E>> =
    tryForceMovement (g, z, v, (fun c n -> ForceToVertex(c, v)))

// Move one step forward in the history. The history is not altered in the
// process.
let moveForward (z: Zipper<'V, 'E>) : Zipper<'V, 'E> =
    moveToHistoryIndex (z.HistoryIndex + 1) z