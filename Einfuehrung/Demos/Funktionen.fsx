// Kommentare wie in C#
(* oder
über mehrere Linien
so *)



let sag was zu =
    was + " " + zu




sag "Hallo" "Carsten"




let plus a b = a + b




plus 4 (3*4)




let schreiWas (was : string) zu =
    was.ToUpper() + " " + zu




let sagHalloZu = sag "Hallo"




sagHalloZu "Carsten"



"Carsten" |> sagHalloZu

