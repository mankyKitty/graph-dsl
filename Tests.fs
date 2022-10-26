module Tests

open System
open Xunit

open GraphDSL.TranscriptionFactor
open FParsec

[<Fact>]
let ``Single Valid Parse - TranscriptionFactor`` () =
    let input = "AcrR	marR	-	[GEA, APPH, GEA, IEP, IMP, APIORCISFBSCS, BPP]	STRONG"

    let expected =
        { Name = "AcrR"
          RegulatedBy = "marR"
          RegulatoryEffect = Repressor
          SupportingEvidence = [ "GEA"; "APPH"; "GEA"; "IEP"; "IMP"; "APIORCISFBSCS"; "BPP" ]
          EvidenceType = Strong }

    match run pTranscriptionFactor input with
    | Failure (err, _a, _b) -> Assert.True(false, err)
    | Success (tf, _x, _y) -> Assert.Equal(tf, expected)

let manymanyInputs = "CRP	xylR	+	[GEA, AIFS, APPH, APPP, GEA, HIFS, IEP, IHBCE, IMP, IPI, AIBSCS, APIORCISFBSCS]	STRONG	
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
        | Success (xs, _a, _b) ->
            printfn "%O" xs
            Assert.Equal(List.length xs, 18)

[<Fact>]
let ``Parse network_tf_tf.txt file`` () =
    let enc = new System.Text.UTF8Encoding()
    match runParserOnFile p_network_tf_tf_file () "input_files/network_tf_tf.txt" enc with
        | Failure (err, _a, _b) -> Assert.True(false, err)
        | Success (xs, _a_, b) -> Assert.True(List.length xs = 493)
