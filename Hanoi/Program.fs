// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

type Post = int list

type Game = Post list

let display (game : Game) = 
    let displayPost (post : Post) =
        printfn "|%s" (post |> List.fold (fun acc elem -> elem.ToString() + acc) "")
    game |> List.iter displayPost
    printfn ""
    game

let init i =
    [ [ 1 .. i ]; []; [] ] |> display

let top postNumber (game : Game) =
    if game.Length <= postNumber 
    then None 
    else match game.[postNumber] with
         | [] -> None
         | h :: t -> Some h

let pop postNumber (game : Game) =
    let pop (post : Post) = 
        match post with
        | [] -> post
        | h::t -> t
    game |> List.mapi (fun i elem -> if i = postNumber then pop elem else elem)

let push (ringNumber : int Option) postNumber (game : Game) =
    let push ringNumber (post : Post) =
        match ringNumber with
        | Some n -> n :: post
        | None -> post 
    game |> List.mapi (fun i elem -> if i = postNumber then push ringNumber elem else elem)

let moveTop fromPost toPost (game : Game) =
    let ring = game |> top fromPost
    game |> pop fromPost |> push ring toPost |> display

let rec moveStack fromPost toPost viaPost height (game : Game) =
    match height with
    | 0 -> game
    | 1 -> game |> moveTop fromPost toPost
    | _ -> game |> moveStack fromPost viaPost toPost (height - 1) |> moveTop fromPost toPost |> moveStack viaPost toPost fromPost (height - 1)

[<EntryPoint>]
let main argv = 
    ignore (init 9 |> moveStack 0 1 2 9)

    0 // return an integer exit code
