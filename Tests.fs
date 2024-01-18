module Tests

open System
open Xunit
open Xunit.Abstractions

open FParsec

open QuikGraph

open GraphDSL.TranscriptionFactor
open GraphDSL.Zipper
open GraphDSL.Types


type GraphDSLTests(output:ITestOutputHelper) =

    [<Fact>]
    member __.``Single Valid Parse - TranscriptionFactor`` () =
        let input = "AcrR	marR	-	[GEA, APPH, GEA, IEP, IMP, APIORCISFBSCS, BPP]	STRONG"

        let expected =
            { Name = TFName "AcrR"
              Target = TFName "marR"
              RegulatoryEffect = Repressor
              SupportingEvidence = [ "GEA"; "APPH"; "GEA"; "IEP"; "IMP"; "APIORCISFBSCS"; "BPP" ]
              EvidenceType = Strong }

        let actual = run pTranscriptionFactor input
        match actual with
        | Failure (err, _a, _b) -> Assert.True(false, sprintf "Test 1 failure - %s" err)
        | Success (tf, _x, _y) ->
            Assert.True(tf.Equals(expected), "Test 1 failure - Parsing did not produce the expected transcription factor value.")
            output.WriteLine("Test 1 success - Parsed transcription factor string to {0}.", tf)

    member __.manymanyInputs =
        "CRP	xylR	+	[GEA, AIFS, APPH, APPP, GEA, HIFS, IEP, IHBCE, IMP, IPI, AIBSCS, APIORCISFBSCS]	STRONG	
    CRP	zraR	+	[GEA, AIFS, APPH, APPP, GEA, HIFS, IEP, IHBCE, IMP, IPI, AIBSCS]	STRONG	
    CRP-Sxy	sxy	+	[GEA, IMP, APIORCISFBSCS]	WEAK	
    CadC	cadC	+	[GEA, IMP, BPP]	STRONG	
    CdaR	cdaR	+	[GEA, GEA, IFC, BPP]	STRONG	
    CecR	cecR	-	[GEA, AUPEINH, HIFS, IEP, IPI, APIORCISFBSCS, BPP, GS]	STRONG	
    ChbR	chbR	+	[GEA, IDA, BPP]	STRONG	
    ChbR	chbR	-	[GEA, IDA, BPP]	STRONG	
    CpxR	baeR	+	[GEA, IC, AIFS, HIFS, IEP, IPI, BPP, IC]	STRONG	
    CpxR	cpxR	+	[GEA, AIFS, HIFS, IEP, IPI, APIORCISFBSCS, BPP]	STRONG	
    CpxR	csgD	-	[GEA, AIFS, HIFS, IEP, IPI, APIORCISFBSCS, BPP]	STRONG	
    CpxR	marA	+	[GEA, AIFS, HIFS, IEP, IPI, APIORCISFBSCS, BPP]	STRONG	
    CpxR	marR	+	[GEA, AIFS, HIFS, IEP, IPI, APIORCISFBSCS, BPP]	STRONG	
    Cra	betI	+	[GEA, APPH, HIFS, BPP]	STRONG	
    Cra	crp	+	[GEA, APPH, HIFS, BPP, SM]	STRONG	
    Cra	csgD	+	[GEA, APPH, HIFS, AIBSCS, BPP, SM]	STRONG	
    Cra	glcC	-	[GEA, APPH, HIFS, AIBSCS]	STRONG	
    kCra	marA	-	[GEA, APPH, HIFS, AIBSCS]	STRONG  
    "

    [<Fact>]
    member __.``Several Valid TranscriptionFactor`` () =
        match run (many1 (pTranscriptionFactor .>> (spaces >>. optional skipNewline))) __.manymanyInputs with
        | Failure (err, _a, _b) -> Assert.True(false, sprintf "Test 2 failure - %s" err)
        | Success (xs, _a, _b) ->
            Assert.True(List.length xs = 18, "Test 2 failure - Did not parse the expected number of rows.")
            output.WriteLine("Test 2 success - Parsed transcription factor strings to {0} rows.", (List.length xs))

    [<Fact>]
    member __.``Parse network_tf_tf.txt file`` () =
        let enc = new System.Text.UTF8Encoding()

        match runParserOnFile p_network_tf_tf_file () "input_files/network_tf_tf.txt" enc with
        | Failure (err, _a, _b) -> Assert.True(false, sprintf "Test 3 failure - %s" err)
        | Success (xs, _a_, b) ->
            Assert.True(List.length xs = 493, "Test 3 failure - Did not parse the expected number of rows.")
            output.WriteLine("Test 3 success - Parsed RegulonDB transcription factor file to {0} rows.", (List.length xs))

    member __.testGraph (g: BidirectionalGraph<int, TaggedEdge<int, string>>) : BidirectionalGraph<int, TaggedEdge<int, string>> =
        g.AddVertex(1) |> ignore
        g.AddVertex(2) |> ignore
        g.AddVertex(3) |> ignore
        g.AddEdge(new TaggedEdge<int, string>(1, 2, "one two")) |> ignore
        g.AddEdge(new TaggedEdge<int, string>(3, 2, "three two")) |> ignore
        g.AddEdge(new TaggedEdge<int, string>(2, 1, "two one")) |> ignore
        g.AddEdge(new TaggedEdge<int, string>(1, 3, "one three")) |> ignore
        g

    [<Fact>]
    member __.``Zipper - Graph Move to Vertex`` () =
        let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
        let g0 = __.testGraph (g)
        let z = createZipper (1)

        match moveToVertex (g0, z, 2) with
        | None -> Assert.True(false, "Test 4 failure - Zipper failed to complete a moveToVertex action.")
        | Some (newZ) ->
            Assert.True(newZ.Cursor = 2, "Test 4 failure - Zipper moveToVertex action moved to an unexpected vertex.")
            output.WriteLine("Test 4 success - Zipper moved from starting vertex 1 to vertex 2 via a moveToVertex action.")

    [<Fact>]
    member __.``Zipper - Graph Move Along Edge`` () =
        let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
        let g0 = __.testGraph (g)
        let e = new TaggedEdge<int, string>(1, 3, "one three")
        let z = createZipper (1)

        match moveAlongEdge (g0, z, e) with
        | None -> Assert.True(false, "Test 5 failure - Zipper failed to complete a moveAlongEdge action.")
        | Some (newZ) ->
            Assert.True(newZ.Cursor = 3, "Test 5 failure - Zipper moveAlongEdge action moved to an unexpected vertex.")
            output.WriteLine("Test 5 success - Zipper moved from starting vertex 1 to vertex 3 via a moveAlongEdge action.")

    [<Fact>]
    member __.``Zipper - Graph Along First Matching Edge`` () =
        let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
        let g0 = __.testGraph (g)
        let z = createZipper (1)

        match moveAlongFirstMatchingEdge (g0, z, (fun e -> e.Tag = "one two")) with
        | None -> Assert.True(false, "Test 6 failure - Zipper failed to complete a moveAlongFirstMatchingEdge action.")
        | Some (newZ) ->
            Assert.True(newZ.Cursor = 2, "Test 6 failure - Zipper moveAlongFirstMatchingEdge action moved to an unexpected vertex.")
            output.WriteLine("Test 6 success - Zipper moved from starting vertex 1 to vertex 2 via a moveAlongFirstMatchingEdge action.")

    [<Fact>]
    member __.``Zipper - Graph Along First Matching Vertex`` () =
        let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
        let g0 = __.testGraph (g)
        let z = createZipper (1)

        match moveAlongFirstMatchingVertex (g0, z, (fun v -> v > 2)) with
        | None -> Assert.True(false, "Test 7 failure - Zipper failed to complete a moveAlongFirstMatchingVertex action.")
        | Some (newZ) ->
            Assert.True(newZ.Cursor = 3, "Test 7 failure - Zipper moveAlongFirstMatchingVertex action moved to an unexpected vertex.")
            output.WriteLine("Test 7 success - Zipper moved from starting vertex 1 to vertex 2 via a moveAlongFirstMatchingVertex action.")

    [<Fact>]
    member __.``Zipper - Graph Move Backwards`` () =
        let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
        let g0 = __.testGraph (g)

        let z =
            createZipper (1)
            |> (fun z0 -> moveToVertex (g0, z0, 3))
            |> Option.bind (fun z0 -> moveToVertex (g0, z0, 2))
            |> Option.bind (fun z0 -> moveToVertex (g0, z0, 1))
            |> Option.map (fun z0 -> moveBack z0)

        match z with
        | None -> Assert.True(false, "Test 8 failure - Zipper failed to complete a moveBack action.")
        | Some (zz) ->
            Assert.True(zz.Cursor = 2, "Test 8 failure - Zipper moveBack action moved to an unexpected vertex.")
            output.WriteLine("Test 8 success - Zipper moved backwards in history from vertex 1 to vertex 2 via a moveBack action.")

    [<Fact>]
    member __.``Zipper - Graph Move Forwards`` () =
        let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
        let g0 = __.testGraph (g)

        let z =
            createZipper (1)
            |> (fun z0 -> moveToVertex (g0, z0, 3))
            |> Option.bind (fun z0 -> moveToVertex (g0, z0, 2))
            |> Option.bind (fun z0 -> moveToVertex (g0, z0, 1))
            |> Option.map (fun z0 -> moveBack z0)
            |> Option.map (fun z0 -> moveForward z0)

        match z with
        | None -> Assert.True(false, "Test 9 failure - Zipper failed to complete a moveForward action.")
        | Some (zz) ->
            Assert.True(zz.Cursor = 1, "Test 9 failure - Zipper moveForward action moved to an unexpected vertex.")
            output.WriteLine("Test 9 success - Zipper moved forwards in history from vertex 2 to vertex 1 via a moveForward action.")

    [<Fact>]
    member __.``Zipper - Graph Move To History`` () =
        let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
        let g0 = __.testGraph (g)

        let z =
            createZipper (1)
            |> (fun z0 -> moveToVertex (g0, z0, 3))
            |> Option.bind (fun z0 -> moveToVertex (g0, z0, 2))
            |> Option.bind (fun z0 -> moveToVertex (g0, z0, 1))
            |> Option.map (fun z0 -> moveToHistoryIndex 1 z0)

        match z with
        | None -> Assert.True(false, "Test 10 failure - Zipper failed to complete a moveToHistoryIndex action.")
        | Some (zz) ->
            Assert.True(zz.Cursor = 3, "Test 10 failure - Zipper moveToHistoryIndex action moved to an unexpected vertex.")
            output.WriteLine("Test 10 success - Zipper moved in history from vertex 1 to vertex 3 via a moveToHistoryIndex action.")

    [<Fact>]
    member __.``Zipper - Graph Force Move to Vertex`` () =
        let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
        let g0 = __.testGraph (g)

        let z =
            createZipper (1)
            |> (fun z0 -> moveToVertex (g0, z0, 3))
            |> Option.bind (fun z0 -> forceMoveToVertex (g0, z0, 1))

        match z with
        | None -> Assert.True(false, "Test 11 failure - Zipper failed to complete a forceMoveToVertex action.")
        | Some (zz) ->
            Assert.True(zz.Cursor = 1, "Test 11 failure - Zipper forceMoveToVertex action moved to an unexpected vertex.")
            output.WriteLine("Test 11 success - Zipper moved from vertex 3 to vertex 1 via a forceMoveToVertex action.")

    [<Fact>]
    member __.``Graph Types - Create Graph With Individual Vertices\Edges`` () =
        let vzero = newVert "zero" 0
        let vone = newVert "one" 1
        let vtwo = newVert "two" 2
        let vthree = newVert "three" 3
        let vfortytwo = newVert "forty-two" 42

        let graph = new AppGraph ()
                    |> tryAddVertex vzero
                    |> tryAddVertex vone
                    |> tryAddVertex vtwo
                    |> tryAddVertex vthree
                    |> tryAddVertex vfortytwo
                    |> tryAddEdge (newEdge vzero vone "add_one" 1)
                    |> tryAddEdge (newEdge vzero vtwo "add_two" 1)
                    |> tryAddEdge (newEdge vtwo vthree "two_three" 1)
                    |> tryAddEdge (newEdge vone vthree "one_three" 1)
                    |> tryAddEdge (newEdge vthree vfortytwo "end" 1)

        Assert.True(Seq.length graph.Vertices = 5 && Seq.length graph.Edges = 5, "Test 12 failure - Graph does not have five vertices and five edges.")
        output.WriteLine("Test 12 success - Graph has five vertices and five edges.")

    [<Fact>]
    member __.``Graph Types - Create Graph With Lists`` () =
        let vzero = newVert "zero" 0
        let vone = newVert "one" 1
        let vtwo = newVert "two" 2
        let vthree = newVert "three" 3
        let vfortytwo = newVert "forty-two" 42

        let graph = new AppGraph ()
                    |> tryAddVertices [vzero; vone; vtwo; vthree; vfortytwo]
                    |> tryAddEdges [
                        (newEdge vzero vone "add_one" 1);
                        (newEdge vzero vtwo "add_two" 1);
                        (newEdge vtwo vthree "two_three" 1);
                        (newEdge vone vthree "one_three" 1);
                        (newEdge vthree vfortytwo "end" 1)
                    ]

        Assert.True(Seq.length graph.Vertices = 5 && Seq.length graph.Edges = 5, "Test 13 failure - Graph does not have five vertices and five edges.")
        output.WriteLine("Test 13 success - Graph has five vertices and five edges.")