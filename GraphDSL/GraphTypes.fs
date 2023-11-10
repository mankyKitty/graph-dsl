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

// Specifies the vertex used for the bidirectional graph. Each vertex has a
// string tag to name it, and a integer value that is used as an identifier.
type Vert =
    { 
        Tag: string
        Value: int 
    }

// Specifies the configuration for the QuikGraph used.
type AppGraph = BidirectionalGraph<Vert, TaggedValueEdge<Vert,string,float>>