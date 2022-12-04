module Tests

open System
open Xunit

open FParsec

open QuikGraph

open GraphDSL.TranscriptionFactor
open GraphDSL.NetworkGraph
open GraphDSL.Zipper

[<Fact>]
let ``Single Valid Parse - TranscriptionFactor`` () =
    let input = "AcrR	marR	-	[GEA, APPH, GEA, IEP, IMP, APIORCISFBSCS, BPP]	STRONG"

    let expected =
        { Name = TFName "AcrR"
          RegulatedBy = TFName "marR"
          RegulatoryEffect = Repressor
          SupportingEvidence = [ "GEA"; "APPH"; "GEA"; "IEP"; "IMP"; "APIORCISFBSCS"; "BPP" ]
          EvidenceType = Strong }

    match run pTranscriptionFactor input with
    | Failure (err, _a, _b) -> Assert.True(false, err)
    | Success (tf, _x, _y) -> Assert.Equal(tf, expected)

let manymanyInputs =
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
let ``Several Valid TranscriptionFactor`` () =
    match run (many1 (pTranscriptionFactor .>> (spaces >>. optional skipNewline))) manymanyInputs with
    | Failure (err, _a, _b) -> Assert.True(false, err)
    | Success (xs, _a, _b) -> Assert.Equal(List.length xs, 18)

[<Fact>]
let ``Parse network_tf_tf.txt file`` () =
    let enc = new System.Text.UTF8Encoding()

    match runParserOnFile p_network_tf_tf_file () "input_files/network_tf_tf.txt" enc with
    | Failure (err, _a, _b) -> Assert.True(false, err)
    | Success (xs, _a_, b) -> Assert.True(List.length xs = 493)

let testGraph (g: BidirectionalGraph<int, TaggedEdge<int, string>>) : BidirectionalGraph<int, TaggedEdge<int, string>> =
    g.AddVertex(1) |> ignore
    g.AddVertex(2) |> ignore
    g.AddVertex(3) |> ignore
    g.AddEdge(new TaggedEdge<int, string>(1, 2, "one two")) |> ignore
    g.AddEdge(new TaggedEdge<int, string>(3, 2, "three two")) |> ignore
    g.AddEdge(new TaggedEdge<int, string>(2, 1, "two one")) |> ignore
    g.AddEdge(new TaggedEdge<int, string>(1, 3, "one three")) |> ignore
    g

[<Fact>]
let ``Zipper - Graph Move to Vertex`` () =
    let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
    let g0 = testGraph (g)
    let z = createZipper (1)

    match moveToVertex (g0, z, 2) with
    | None -> Assert.True(false, "Couldn't move to known good vertex")
    | Some (newZ) -> Assert.True(newZ.Cursor = 2, "Did not move to correct vertex")

[<Fact>]
let ``Zipper - Graph Move Along Edge`` () =
    let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
    let g0 = testGraph (g)
    let e = new TaggedEdge<int, string>(1, 3, "one three")
    let z = createZipper (1)

    match moveAlongEdge (g0, z, e) with
    | None -> Assert.True(false, "Couldn't move along known good edge")
    | Some (newZ) -> Assert.True(newZ.Cursor = 3, "Did not along correct vertex")

[<Fact>]
let ``Zipper - Graph Along First Matching Edge`` () =
    let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
    let g0 = testGraph (g)
    let z = createZipper (1)

    match moveAlongFirstMatchingEdge (g0, z, (fun e -> e.Tag = "one two")) with
    | None -> Assert.True(false, "Couldn't move along known good edge")
    | Some (newZ) -> Assert.True(newZ.Cursor = 2, "Did not along correct vertex")

[<Fact>]
let ``Zipper - Graph Along First Matching Vertex`` () =
    let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
    let g0 = testGraph (g)
    let z = createZipper (1)

    match moveAlongFirstMatchingVertex (g0, z, (fun v -> v > 2)) with
    | None -> Assert.True(false, "Couldn't move to first node matching")
    | Some (newZ) -> Assert.True(newZ.Cursor = 3, "Did not move to expected vertex")

[<Fact>]
let ``Zipper - Graph Move Backwards`` () =
    let g = new BidirectionalGraph<int, TaggedEdge<int, string>>()
    let g0 = testGraph (g)

    let z =
        createZipper (1)
        |> (fun z0 -> moveToVertex (g0, z0, 3))
        |> Option.bind (fun z0 -> moveToVertex (g0, z0, 2))
        |> Option.bind (fun z0 -> moveToVertex (g0, z0, 1))

    match z with
    | None -> Assert.True(false, "Couldn't move correctly")
    | Some (zz) -> Assert.True(zz.Cursor = 1, "Maybe?")

    let z1 = Option.map moveBack z

    match z1 with
    | None -> Assert.True(false, "Couldn't move correctly")
    | Some (zz) -> Assert.True(zz.Cursor = 2, "Maybe?")
