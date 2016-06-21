

type Spieler = A | B

type Punkt =
    | Null
    | Fünfzehn
    | Dreißig

type NächsterPunkt =
    | Punkt of Punkt
    | Vierzig
    
let nächsterPunkt punkt =
    match punkt with
    | Null     -> Punkt Fünfzehn
    | Fünfzehn -> Punkt Dreißig
    | Dreißig  -> Vierzig

type Spielstand =
    | Punkte of (Punkt * Punkt)
    | Einer40 of (Spieler * Punkt)
    | Einstand
    | Vorteil of Spieler
    | Gewonnen of Spieler

// Hilfsfunktionen
// ------------------------------------------
let getPunkte spieler =
    match spieler with
    | A -> fun (a,b) -> (a,b)
    | B -> fun (a,b) -> (b,a)

let setzeSpielerPunkt punkte gegnerPunkte spieler = 
    match spieler with
    | A -> (punkte, gegnerPunkte)
    | B -> (gegnerPunkte, punkte)
// -------------------------------------------

let (|Punkte|_|) spieler spielstand =
    match spielstand with
    | Punkte punkte -> Some (getPunkte spieler punkte)
    | _             -> None

let punktGewinn spielstand gewinner =
    match spielstand with
    | Punkte gewinner (punkte, gegnerPunkte) ->
        match nächsterPunkt punkte with
        | Punkt neuePunkte ->
            Punkte  (gewinner |> setzeSpielerPunkt neuePunkte gegnerPunkte)
        | Vierzig ->
            Einer40 (gewinner, gegnerPunkte)
    | Einer40 (spieler40, _) when spieler40 = gewinner ->
            Gewonnen gewinner
    | Einer40 (spieler40, gegnerPunkte) ->
            match nächsterPunkt gegnerPunkte with
            | Vierzig ->
                Einstand
            | Punkt neuePunkte ->
                Einer40 (spieler40, neuePunkte)
    | Einstand ->
        Vorteil gewinner
    | Vorteil vorteilSpieler when vorteilSpieler = gewinner ->
        Gewonnen gewinner
    | Vorteil _ ->
            Einstand
    | sonst -> sonst
    
let anfangsStand = Punkte (Null, Null)

let spiele (punktGewinneDurch : Spieler list) : Spielstand list =
    punktGewinneDurch
    |> List.scan punktGewinn anfangsStand

spiele [A;A;B;A;B;B;A;B;B;B]
