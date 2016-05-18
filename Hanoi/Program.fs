// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Ring = int

type Post = int list

type Game = Post list

let init i =
    [ [ 1 .. i ]; []; [] ]

let display (game : Game) = 
    let displayPost (post : Post) =
        printfn "|%s" (post |> List.fold (fun acc elem -> elem.ToString() + acc) "")
    game |> List.iter displayPost

let removeTop postNumber (game : Game) =
    let removeTop (post : Post) = 
        match post with
        | [] -> (None, post)
        | h::t -> Some(h), t
    let top, newPost = removeTop game.[postNumber]
    let newGame = game |> List.mapi (fn i elem -> 

[<EntryPoint>]
let main argv = 
    let game = init 5
    display game

    0 // return an integer exit code
