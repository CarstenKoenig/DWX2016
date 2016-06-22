#r "FsCheck.dll"

module Caesar =

    type Schlüssel = private Schlüssel of int
    
    let schlüssel k =
        abs k
        |> fun k -> k % 25
        |> fun k -> if k = 0 then 25 else k
        |> Schlüssel

    let private reverse (Schlüssel k) =
        Schlüssel (-k)
    
    type Nachricht<'label> =
        private | Nachricht of string
        override this.ToString () =
            let (Nachricht msg) = this in msg

    let private ofChars cs : string =
        Array.ofSeq cs
        |> System.String

    let private filterString f =
        Seq.filter f >> ofChars

    let private mapNachricht f (Nachricht msg) : Nachricht<'b> =
        msg
        |> Seq.map f
        |> ofChars
        |> Nachricht

    let private normalize s =
       if System.String.IsNullOrEmpty s
       then ""
       else s.Trim().ToUpper()

    type Klartext =
        private | Klartext
        
    let nachricht : string -> Nachricht<Klartext> =
        let gültigeZeichen = System.Collections.Generic.HashSet ['A'..'Z']
        fun s ->
            normalize s
            |> filterString gültigeZeichen.Contains
            |> Nachricht

    let private mod' n d =
        let m = n % d
        if m < 0 then m+d else m

    let private translate (Schlüssel key) (c : char) =
        let n = int c - int 'A'
        mod' (n + key) 26 + int 'A'
        |> char

    type Verschlüsselt =
        private | Verschlüsselt
        
    let verschlüsseln (key : Schlüssel )
                      : Nachricht<Klartext> -> Nachricht<Verschlüsselt> =
        mapNachricht (translate key)

    let entschlüsseln (key : Schlüssel )
                      : Nachricht<Verschlüsselt> -> Nachricht<Klartext> =
        mapNachricht (translate (reverse key))

/// die FsCheck Tests
[<AutoOpen>]
module ``Caesar-Tests`` =
  open FsCheck
  open System
  open Caesar

  type Marker = class end

  type MyGenerators =
      static member KlartextNachricht() =
          { new Arbitrary<Nachricht<Klartext>>() with
            override __.Generator =
                Gen.growingElements ['A'..'Z']
                |> Gen.listOf
                |> Gen.map (List.toArray >> String)
                |> Gen.filter (not << String.IsNullOrEmpty)
                |> Gen.map nachricht
            override __.Shrinker s =
                let s = string s
                if s.Length < 2 then Seq.empty else
                let mid = s.Length / 2
                seq [ s.Substring(0,mid); s.Substring(mid) ]
                |> Seq.map nachricht
          }
      static member Schlüssel() =
          { new Arbitrary<Schlüssel>() with
            override __.Generator =
                Gen.growingElements [1..25]
                |> Gen.map schlüssel
          }

  Arb.register<MyGenerators>()

  let ``verschlüsselte Nachricht ist nicht gleich der Orginalnachricht``
    (key : Schlüssel, msg : Nachricht<Klartext>) =
    let enc = Caesar.verschlüsseln key msg
    string enc <> string msg

  let ``Länge bleibt gleich``
    (key : Schlüssel, msg : Nachricht<Klartext>) =
    let enc = Caesar.verschlüsseln key msg
    (string msg).Length = (string msg).Length

  let ``verschlüsseln >> entschlüsseln = id``
    (key : Schlüssel, msg : Nachricht<Klartext>) =
    let ver = Caesar.verschlüsseln key msg
    let ent = Caesar.entschlüsseln key ver
    msg = ent
    |@ sprintf "nach dem Entschlüsseln war %O plötzlich %O" msg ent

  let checkAll() =
    Check.QuickAll (typeof<Marker>.DeclaringType)
