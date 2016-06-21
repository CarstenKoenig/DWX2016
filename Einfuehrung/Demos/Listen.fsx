


let zahlen = [1;2;3;4]




let mehrZahlen = [1..10]





let zusammen = zahlen @ mehrZahlen





List.sum zahlen




List.map (fun z -> z*2) zahlen




// Üblicherweise
mehrZahlen
|> List.filter (fun z -> z%2=0)
|> List.map (fun z -> z*2)
|> List.sum




let leer = []
1::[2;3]
1::2::3::[]




let (kopf::rest) = zahlen




let istLeer (xs : int list) =
    match xs with
    | [] -> "leer"
    | (kopf::rest) -> "nicht-leer - fängt mit " + string kopf + " an"

istLeer zahlen
istLeer leer
