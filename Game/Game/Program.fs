open System

//
//------------------------ Models -----------------------
//

//type { } este similar cu struct
type Details = {
    Name: string
    Description: string
}

type Item = {
    Details: Details
}

//type | este similar cu enum
type RoomId = 
    | RoomId of string

type Exit =
    | PassableExit of string * destination: RoomId
    | LockedExit of string * key: Item * next: Exit
    | NoExit of string option
    
type Exits = {
    North: Exit
    South: Exit
    East: Exit
    West: Exit
}

//fiecare Room va avea Id pt identificare, detalii(un nume si o descriere), o lista de items(posibil goala)
//si 4 iesiri care pot avea sau nu acces. pentru o iesire inchisa, folosind un item putem sa o deschidem
type Room = {
    Id: RoomId
    Details: Details
    Items: Item list
    Exits: Exits
}

//jucatorul are detalii, o camera in care se afla(identificata dupa Id) si o lista de Items
type Player = {
    Details: Details
    Location: RoomId
    Inventory: Item list
}

//lumea generata este formata din mai multe camere si un player
type World = {
    Rooms: Map<RoomId, Room>
    Player: Player
}

//
//---------------------- Initial World --------------------
//

let key: Item = { 
    Details = { 
        Name = "A shiny key"
        Description = "This key looks like it could open a nearby door."
    } 
}

let allRooms = [
    {
        Id = RoomId "center"
        Details = { 
            Name = "A central room"
            Description = "You are standing in a central room with exits in all directions.  A single brazier lights the room."
        }
        Items = []
        Exits = { 
            North = PassableExit ("You see a darkened passageway to the north.", RoomId "north1")
            South = PassableExit ("You see door to the south.  A waft of cold air hits your face.", RoomId "south1")
            East = LockedExit ("You see a locked door to the east.", key, PassableExit ("You see an open door to the east.", RoomId "east1"))
            West = PassableExit ("You see an interesting room to the west.", RoomId "west1") 
        }
    }

    { 
        Id = RoomId "north1"
        Details = { 
            Name = "A dark room"
            Description = "You are standing in a very dark room.  You hear the faint sound of rats scurrying along the floor."
        }
        Items = []
        Exits = { 
            North = NoExit None
            South = PassableExit ("You see an dimly lit room to the south.", RoomId "center")
            East = NoExit None
            West = NoExit None 
        }
    }

    { 
        Id = RoomId "south1"
        Details = { 
            Name = "A cold room"
            Description = "You are standing in a room that feels very cold.  Your breath instantly turns into a white puff."
        }
        Items = []
        Exits = { 
            North = PassableExit ("You see an exit to the north.  That room looks much warmer.", RoomId "center")
            South = NoExit None
            East = NoExit None
            West = NoExit None 
        }
    }

    { 
        Id = RoomId "west1"
        Details = { 
            Name = "A cozy room"
            Description = "This room seems very cozy, as if someone had made a home here.  Various personal belongings are strewn about."
        }
        Items = [ key ]
        Exits = { 
            North = NoExit None
            South = NoExit None
            East = PassableExit ("You see a doorway back to the lit room.", RoomId "center")
            West = NoExit None 
        }
    }

    { 
        Id = RoomId "east1"
        Details = { 
            Name = "An open meadow"
            Description = "You are in an open meadow.  The sun is bright and it takes some time for your eyes to adjust."
        }
        Items = []
        Exits = { 
            North = NoExit None
            South = NoExit None
            East = NoExit None
            West = PassableExit ("You see stone doorway to the west.  Why would you want to go back there?", RoomId "center") 
        }
    }
]

let player = { 
    Details = { Name = "Luke"; Description = "Just your average adventurer."}
    Inventory = []
    Location = RoomId "center" 
}

let gameWorld = { 
    Rooms =
        allRooms
        |> Seq.map (fun room -> (room.Id, room))
        |> Map.ofSeq
    Player = player
}

//
//----------------------------- Logic -------------------------------
//

//un nou tip pentru cazuri de succes si de esec. acestea inca tin minte tipul precedent de date, fiind generice
type Result<'TSuccess, 'TFailure> =
    | Success of 'TSuccess
    | Failure of 'TFailure

//bind leaga tipul Result cu functia pe care trebuie sa o execute
let bind processFunc lastResult =
    match lastResult with
    | Success s -> processFunc s
    | Failure f -> Failure f

//suprascrierea unui operator pentru a folosi metoda bind mai usor
let (>>=) x f =
    bind f x

//switch transforma rezultatul unei functii intr-un tip Result de felul Success
let switch processFunc input =
    Success (processFunc input)

//se cauta camera dupa Id in lumea data. Daca nu se gaseste, se returneaza un string
let getRoom world roomId =
    match world.Rooms.TryFind roomId with
    | Some room -> Success room
    | None -> Failure "Room does not exist!"

//afisarea unui obiect de tip detalii intr-un mod convenabil
let describeDetails details =
    sprintf "\n%s\n%s\n\n\n" details.Name details.Description

//extragem detaliile dintr-o camera
let extractDetailsFromRoom (room: Room) =
    room.Details

//cautam camera in care jucatorul se afla deocamdata in lumea generata, luam detaliile acesteia si le afisam
let describeCurrentRoom world =
    world.Player.Location
    |> getRoom world
    |> (bind (switch extractDetailsFromRoom) >> bind (switch describeDetails))

//prescurtari pentru cele patru cai pe care le putem alege, pentru a putea citi codul mai usor
let north exits = exits.North
let south exits = exits.South
let east exits = exits.East
let west exits = exits.West

//metoda care returneaza camera in care se afla jucatorul din lumea primita ca si parametru
let getCurrentRoom world =
    world.Player.Location
    |> getRoom world

//metoda care schimba camera in care se afla jucatorul cu camera primita ca si parametru
let setCurrentRoom world room = { 
    world with Player = { world.Player with Location = room.Id} 
}

//se verifica toate iesirile, si se returneaza un rezultat de succes (roomId) sau esec (un string corespunzator)
let getExit direction exits =
    match (direction exits) with
    | PassableExit (_, roomId) -> Success roomId
    | LockedExit (_, _, _) -> Failure "There is a locked door in that direction."
    | NoExit (_) -> Failure "There is no room in that direction."

//metoda care incearca sa mute jucatorul intr-o alta camera: se ia camera curenta, se iau iesirile acesteia, 
//dupa care incercam sa mergem in camera urmatoare daca rezultatul din setCurrentRoom este cu succes
let move direction world =
    world
    |> getCurrentRoom
    >>= switch (fun room -> room.Exits) 
    >>= getExit direction
    >>= getRoom world
    >>= switch (setCurrentRoom world)

//dorim sa afisam rezultatele chiar daca a aparut o camera inchisa sau nicio camera pe parcursul miscarii jucatorului
let displayResult result =
    match result with
    | Success s -> printf "%s" s
    | Failure f -> printf "\n%s\n\n\n" f

[<EntryPoint>]
let main argv =
    
    //lumea generata are o camera centrala si 4 camere in fiecare directie, in est fiind inchisa.
    gameWorld
    |> move east
    >>= describeCurrentRoom
    |> displayResult

    //in orice alta directie am merge, vom primi afisata descrierea acelei camere
    gameWorld
    |> move south
    >>= describeCurrentRoom
    |> displayResult

    //dar acele 4 camere nu mai au acces la alte camere, deci nu putem merge de doua ori in aceeasi directie
    gameWorld
    |> move south
    >>= move south
    >>= describeCurrentRoom
    |> displayResult

    //cat timp nu dam de nicio eroare, putem sa tot navigam aceste camere
    gameWorld
    |> move south
    >>= move north
    >>= move north
    >>= move south
    >>= move west
    >>= describeCurrentRoom
    |> displayResult

    Console.ReadKey()
    0