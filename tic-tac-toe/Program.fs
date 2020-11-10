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
    
    let DrawCursorOver piece =
        match piece with
        | X -> "X"
        | O -> "O"

module Slot =
    let CreateEmpty position = SlotEmpty position

    let Draw slot cursor =
        match slot, cursor with
        | (SlotEmpty slotPos, cursorPos) -> if slotPos = cursorPos then "#" else "-"
        | (SlotFull (slotPos, piece), cursorPos) -> if slotPos = cursorPos then Piece.DrawCursorOver piece else Piece.Draw piece

module Line =
    let Create axisY = Line (Slot.CreateEmpty (Position.Create Left axisY), Slot.CreateEmpty (Position.Create MiddleX axisY), Slot.CreateEmpty (Position.Create Right axisY))

    let Draw line cursor =
        let (Line (s1, s2, s3)) = line
        sprintf "%s|%s|%s" (Slot.Draw s1 cursor) (Slot.Draw s2 cursor) (Slot.Draw s3 cursor)

module Board =
    let Create = Board (Line.Create Top, Line.Create MiddleY, Line.Create Bottom)

    let Draw board cursor =
        let (Board (l1, l2, l3)) = board
        sprintf "%s\n%s\n%s" (Line.Draw l1 cursor) (Line.Draw l2 cursor) (Line.Draw l3 cursor)

type GameState = Running | EndMatch of Piece | Exit

type GameWorld = {
    Board: Board
    Turn: Turn
    Cursor: Position
    State: GameState
}

type GameMsg =
    | MoveUp
    | MoveRight
    | MoveDown
    | MoveLeft
    | Commit
    | Quit
    | EmptyMsg

let moveUp yPos =
    match yPos with
    | Bottom -> MiddleY
    | MiddleY -> Top
    | _ -> yPos

let moveRight xPos =
    match xPos with
    | Left -> MiddleX
    | MiddleX -> Right
    | _ -> xPos

let moveDown yPos =
    match yPos with
    | Top -> MiddleY
    | MiddleY -> Bottom
    | _ -> yPos

let moveLeft xPos =
    match xPos with
    | Right -> MiddleX
    | MiddleX -> Left
    | _ -> xPos

let updateGame world msg =
    match msg, world.Cursor with
    | (MoveUp, Position (x, y)) -> { world with Cursor = Position (x, (moveUp y)) }
    | (MoveRight, Position (x, y)) -> { world with Cursor = Position ((moveRight x), y) }
    | (MoveDown, Position (x, y)) -> { world with Cursor = Position (x, (moveDown y)) }
    | (MoveLeft, Position (x, y)) -> { world with Cursor = Position ((moveLeft x), y) }
    | _ -> world

let drawGame world =
    Console.Clear()
    let (Turn piece) = world.Turn
    printfn "Turn of %s" (Piece.DrawCursorOver piece)
    printf "%s" (Board.Draw world.Board world.Cursor)

    let input = Console.ReadKey()
    match input.Key with
    | ConsoleKey.UpArrow -> MoveUp
    | ConsoleKey.RightArrow -> MoveRight
    | ConsoleKey.DownArrow -> MoveDown
    | ConsoleKey.LeftArrow -> MoveLeft
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
