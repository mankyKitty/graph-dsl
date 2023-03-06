module GraphDSL.TranscriptionFactor

open FParsec

// Apparently polymorphism is hard, I can hear Hindley-Milner laughing.
type UserState = unit
type P<'T> = Parser<'T, UserState>

type RegulatoryEffect =
    | Activator
    | Repressor
    | Dual
    | Unknown

let pRegulatoryEffect: P<RegulatoryEffect> =
    choice
        [ stringReturn "+" Activator
          stringReturn "-" Repressor
          stringReturn "+-" Dual
          stringReturn "?" Unknown ]

type EvidenceType =
    | Weak
    | Strong
    | Confirmed

let pEvidenceType: P<EvidenceType> =
    choice
        [ stringReturn "WEAK" Weak
          stringReturn "STRONG" Strong
          stringReturn "CONFIRMED" Confirmed ]

type TFName = TFName of string

let pTFName: P<TFName> =
    many1Satisfy (fun c -> isAsciiLetter c || "-".Contains(c) || isDigit c) |>> TFName

// An individual Transaction Factor line is composed of the following components.

// (1) Transcription Factor (TF) Name
// (2) TF  regulated by TF
// (3) Regulatory effect of the TF on the regulated gene (+ activator, - repressor, +- dual, ? unknown)
// (4) Evidence that supports the existence of the regulatory interaction
// (5) Evidence Type [Weak, Strong, Confirmed]
//      - For details see: http://regulondb.ccg.unam.mx/evidenceclassification
type TranscriptionFactor =
    { Name: TFName
      Target: TFName
      RegulatoryEffect: RegulatoryEffect
      SupportingEvidence: string list
      EvidenceType: EvidenceType }

let pTranscriptionFactor: P<TranscriptionFactor> =
    let sep0 = skipMany tab <|> spaces

    let pevidence =
        between (pchar '[') (pchar ']') (sepBy1 (manySatisfy isAsciiUpper) (pstring ", "))

    pipe5
        (pTFName .>> sep0)
        (pTFName .>> sep0)
        (pRegulatoryEffect .>> sep0)
        (pevidence .>> sep0)
        pEvidenceType
        (fun name target eff evi evitype ->
            { Name = name
              Target = target
              RegulatoryEffect = eff
              SupportingEvidence = evi
              EvidenceType = evitype })

// This should parse the entirety of a network_tf_tf.txt file.
let p_network_tf_tf_file: P<TranscriptionFactor list> =
    (skipMany (pchar '#' >>. skipRestOfLine true))
    >>. many1 (pTranscriptionFactor .>> (spaces >>. optional skipNewline))
