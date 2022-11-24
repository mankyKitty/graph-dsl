// My unfamiliarity with the tower of abstractions used in F# makes the following a bit muddy.
module Graph.Zipper

// This zipper assumes a graph that is structured similar to the following:
// type Node 'a =
//     { value: 'a
//       edges: (Node 'a) list }

// The moves that you can make, each one denotes a single step. More complicated moves are completed
// by composing these individual moves together.
type Move 'node 'edge =
    | ToNode 'node
    | AlongEdge 'edge
    | FirstEdgeMatching ('edge -> bool) 'edge
    | FirstNodeMatching ('node -> bool) 'node

// The zipper structure itself keeps a reference to the current position The history should probably
// be nothing but references as well to avoid copying entire sections of the graph but I thought it
// might obscure the overall intent.
type Zipper 'node 'edge =
    { Cursor: 'a ref
      History: (Move 'node 'edge) list }

// A helper function to perform a given movement on the graph and record that in the history.
let tryMove : (fn: ('edge -> bool), mvCons: ('edge -> Move<'node,'edge>), z: Zipper<'node,'edge>): Optional<Zipper<'node,'edge>> =
    match z.Cursor.edges.findWith(fn) with
        | None -> None
        | Some(nextEdge) -> Some(Zipper { Cursor = ref(nextEdge.node); History = (mvCons(nextEdge)::(z.History) })

// Create the zipper from the given starting point
let createZipper : ( a:'node ref ) : Zipper<'node,'edge> =
    Zipper { Cursor = a; History = [] }

// From a Node, try to move to a specific known node that is expected to be connected to this one.
let moveToNode : (n: 'node, z: Zipper<'node,'edge>): Optional<Zipper<'node, 'edge>> =
    tryMove (fun e -> e.node == n) (fun e -> ToNode e.node) z

// Move along a known edge that is expected to be connected to the current node.
let moveAlongEdge : (e: 'edge, z: Zipper<'node, 'edge>): Optional<Zipper<'node, 'edge>> =
    tryMove (fun edge -> e == edge) (fun edge -> AlongEdge edge)

// Inspect the edges leading away from this Node and move along the first one that matches the given function.
let moveAlongFirstMatchingEdge: (fn: ('edge -> bool), z: Zipper<'node,'edge>): Optional<Zipper<'node,'edge>> =
    tryMove fn (fun edge -> FirstEdgeMatching fn edge)

// Inspect all nodes one step away from the current not and move to the first one that matches the given function.
let moveAlongFirstMatchingNode: (fn: ('node -> bool), z: Zipper<'node,'edge>): Optional<Zipper<'node,'edge>> =
    tryMove (fun edge -> fn(edge.node)) (fun edge -> FirstNodeMatching fn edge.node)

// Move one step "back". This reverts the most recent single step.
let moveBack : (z: Zipper<'node,'edge>): Zipper<'node,'edge> =
    let fetchNode (m: Move 'node 'edge) = match m with
        | ToNode n -> n
        | AlongEdge e -> e.node
        | FirstEdgeMatching _f e -> e.node
        | FirstNodeMatching _f n -> n

    let (newCursor, newHistory) = match z.History with
        | h::t -> (fetchNode h, t)
        | [] -> (z.Cursor, [])

    Zipper { Cursor = newCursor; History = newHistory }
