open System

//variabila ce enumera niste nume
let names = ["Paul"; "Ion"; "Maria"]

//metoda ce prefixeaza string-ul de baza cu un alt string, si adauga ", " intre string-uri
let prefix prefixStr baseStr =
    prefixStr + ", " + baseStr

//metoda ce tine minte metoda prefix cu variabila prefixStr="Hello"
//Cand vom da acestei metode un string ca si argument, va considera ca este baseStr din prefix
let prefixWithHello = prefix "Hello"

//metoda ce adauga "!" la finalul unui string
let shout s =
    s + "!"

//metoda care va considera argumentul ca fiind argumentul la prefixWithHello
//iar rezultatul este argument al metodei shout
let bigHello = prefixWithHello >> shout

//metoda main (the EntryPoint for this aplication, metoda care se apeleaza la rularea aplicatiei)
[<EntryPoint>]
let main argv =
    //fiecare nume din names este argument pentru bigHello. Sortam rezultatele si le afisam.
    names 
    |> Seq.map bigHello
    |> Seq.sort
    |> Seq.iter (printfn "%s")

    Console.ReadKey()
    0