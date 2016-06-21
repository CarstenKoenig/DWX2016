let Münzen = [200; 100; 50; 20; 10; 5; 2; 1]

let wechsle betrag =
    let flatten xs =
        List.collect (fun (n,x) -> List.replicate n x) xs
    let fold (restBetrag, ergs) münze =
        let n = restBetrag / münze
        let r = restBetrag % münze
        (restBetrag - n * münze, (n,münze)::ergs)
    List.fold fold (betrag, []) Münzen
    |> snd
    |> List.rev
    |> flatten
