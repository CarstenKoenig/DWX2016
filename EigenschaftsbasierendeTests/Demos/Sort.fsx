#r "FsCheck.dll"

let sortieren xs = List.sort xs

let rec istSortiert =
    function
    | [] | [_] -> true
    | a::b::rest ->
        a <= b && istSortiert (b::rest)

/// die FsCheck Tests
[<AutoOpen>]
module ``Sortieren Tests`` =
    open FsCheck

    let ``nach Sortieren ist jedes Element kleiner-gleich seinem Nachfolger`` 
        (zahlen : int list) =
        sortieren zahlen |> istSortiert

    let ``Sortieren ändert Länge der Liste nicht`` 
        (zahlen : int list) =
        let sortiert = sortieren zahlen
        List.length sortiert = List.length zahlen

    let ``nochmal sortieren ändert nichts`` 
        (zahlen : int list) =
        let einmal = sortieren zahlen
        let zweimal = sortieren einmal
        einmal = zweimal

    let ``Sort kommutiert mit Filter`` 
        (zahlen : int list)
        (f: int -> bool) =
        let weg1 = List.sort zahlen |> List.filter f
        let weg2 = List.filter f zahlen |> List.sort
        weg1 = weg2

    let checkAll() =
        Check.Verbose ``nach Sortieren ist jedes Element kleiner-gleich seinem Nachfolger``
//        Check.Quick ``Sortieren ändert Länge der Liste nicht``
//        Check.Quick ``nochmal sortieren ändert nichts`` 
//        Check.Quick ``Sort kommutiert mit Filter``
