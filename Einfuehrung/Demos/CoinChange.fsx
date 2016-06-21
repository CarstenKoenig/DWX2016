let Münzen = [200; 100; 50; 20; 10; 5; 2; 1]

let wechsle betrag =
    if betrag < 0 then failwith "Sie müssen mir ja Geld geben!"
    let rec wechsle' restMünzen restBetrag =
        if restBetrag = 0 then [] else
        match restMünzen with
        | (m::ms) ->
            if m <= restBetrag then
                m :: wechsle' restMünzen (restBetrag - m)
            else
                wechsle' ms restBetrag
        | [] -> failwith "sollte nicht passieren"
    wechsle' Münzen betrag
