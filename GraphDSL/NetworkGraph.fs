module GraphDSL.NetworkGraph

open System

open FSharp.FGL
open FSharp.FGL.Directed

open GraphDSL.TranscriptionFactor

let tf_tf_graph (xs: TranscriptionFactor list) : Graph<TFName, TFName, string> =
    let vertices =
        List.fold (fun acc tf -> List.append acc [ (tf.Name, tf.Name); (tf.RegulatedBy, tf.RegulatedBy) ])
                   []
                   xs
        |> List.distinct

    let edges = List.map (fun tf -> (tf.Name, tf.RegulatedBy, "regulated by")) xs

    Console.WriteLine("Vertices: {0}", vertices)
    Console.WriteLine("Edges: {0}", edges)
    Graph.create vertices edges
