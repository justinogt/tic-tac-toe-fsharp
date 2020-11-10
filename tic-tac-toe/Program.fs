open System

type AxisX = Left | MiddleX | Right
type AxisY = Top | MiddleY | Bottom

type Position = Position of AxisX * AxisY

type Piece = X | O

type Slot =
    | SlotEmpty of Position
    | SlotFull of Position * Piece

type Line = Line of Slot * Slot * Slot

type Board = Board of Line * Line * Line

type Turn = Turn of Piece

module Position =
    let Create x y = Position (x, y)

module Piece =
    let Draw piece =
        match piece with
        | X -> "x"
        | O -> "o"

module Slot =
    let CreateEmpty position = SlotEmpty position

    let Draw slot =
        match slot with
        | SlotEmpty _ -> "0"
        | SlotFull (_, piece) -> Piece.Draw piece

module Line =
    let Create axisY = Line (Slot.CreateEmpty (Position.Create Left axisY), Slot.CreateEmpty (Position.Create MiddleX axisY), Slot.CreateEmpty (Position.Create Right axisY))

    let Draw line =
        let (Line (s1, s2, s3)) = line
        sprintf "%s|%s|%s" (Slot.Draw s1) (Slot.Draw s2) (Slot.Draw s3)

module Board =
    let Create = Board (Line.Create Top, Line.Create MiddleY, Line.Create Bottom)

    let Draw board =
        let (Board (l1, l2, l3)) = board
        sprintf "%s\n%s\n%s" (Line.Draw l1) (Line.Draw l2) (Line.Draw l3)

type GameState = Running | EndMatch of Piece | Exit

type GameWorld = {
    Board: Board
    Turn: Turn
    Cursor: Position
    State: GameState
}

type GameMsg =
    | Up
    | Right
    | Down
    | Left
    | Commit
    | Quit
    | EmptyMsg

let updateGame world msg =
    match msg with
    | Up -> world
    | _ -> world

let drawGame world =
    Console.Clear()
    let (Turn piece) = world.Turn
    printfn "Turn of %s" (Piece.Draw piece)
    printf "%s" (Board.Draw world.Board)

    let input = Console.ReadKey()
    match input.Key with
    | ConsoleKey.UpArrow -> Up
    | ConsoleKey.RightArrow -> Right
    | ConsoleKey.DownArrow -> Down
    | ConsoleKey.LeftArrow -> Left
    | ConsoleKey.Enter -> Commit
    | ConsoleKey.Escape -> Quit
    | _ -> EmptyMsg

let rec game world update draw =
    match world.State with
    | Running -> game (world |> draw |> (update world)) update draw
    | EndMatch w -> EmptyMsg
    | Exit -> EmptyMsg

[<EntryPoint>]
let main argv =
    game { Board = Board.Create; Turn = Turn X; Cursor = Position.Create MiddleX MiddleY; State = Running } updateGame drawGame |> ignore
    0
