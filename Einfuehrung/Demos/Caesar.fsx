module Caesar =
    type Nachricht<'label> = 
        private | Nachricht of string
        override this.ToString() = 
            match this with 
            | Nachricht s -> s

    type Klartext   = private | Klartext
    type Geheimtext = private | Geheimtext

    type Schlüssel = int

    let nachricht : string -> Nachricht<Klartext> =
        let gültigeZeichen = System.Collections.Generic.HashSet ['A'..'Z']
        fun s ->
            if System.String.IsNullOrEmpty s 
                then "" 
                else s.ToUpper()
            |> Seq.filter gültigeZeichen.Contains
            |> Array.ofSeq
            |> System.String
            |> Nachricht

    let private nmap f (Nachricht s) : Nachricht<'label> =
        Seq.map f s
        |> Array.ofSeq
        |> System.String
        |> Nachricht

    let private translate (key : Schlüssel) (c : char) =
        let mod' n d =
            let m = n % d
            if m < 0 then m+d else m
        char <| (mod' (int c - int 'A' + key) 26) + int 'A'
    
    let verschlüsseln (key : Schlüssel ) 
                      (n : Nachricht<Klartext>) 
                      : Nachricht<Geheimtext> =
        nmap (translate key) n

    let entschlüsseln (key : Schlüssel) 
                      (n : Nachricht<Geheimtext>) 
                      : Nachricht<Klartext> =
        nmap (translate -key) n
