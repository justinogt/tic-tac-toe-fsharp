open System

type AxisX = Left | MiddleX | Right
type AxisY = Top | MiddleY | Bottom

type Piece = X | O

type Position = Position of AxisX * AxisY

type Slot =
    | SlotEmpty of Position
    | SlotFull of Position * Piece

type Board = Board of Slot list

type Turn = Turn of Piece

module Turn =
    let change (Turn piece) =
        match piece with
        | X -> Turn O
        | O -> Turn X

module Position =
    let create x y = Position (x, y)

module Piece =
    let draw piece =
        match piece with
        | X -> "x"
        | O -> "o"
    
    let drawCursorOver piece =
        match piece with
        | X -> "X"
        | O -> "O"

module Slot =
    let createEmpty x y = SlotEmpty (Position (x, y))

    let draw slot cursorPosition =
        match slot with
        | SlotEmpty slotPos -> if slotPos = cursorPosition then "#" else "-"
        | SlotFull (slotPos, piece) -> if slotPos = cursorPosition then Piece.drawCursorOver piece else Piece.draw piece

module Board =
    let create = Board [
        Slot.createEmpty Left Top; Slot.createEmpty MiddleX Top; Slot.createEmpty Right Top;
        Slot.createEmpty Left MiddleY; Slot.createEmpty MiddleX MiddleY; Slot.createEmpty Right MiddleY;
        Slot.createEmpty Left Bottom; Slot.createEmpty MiddleX Bottom; Slot.createEmpty Right Bottom;
    ]

    let draw (Board slots) cursor =
        let drawLine line cursor =
            match line with
            | s1 :: s2 :: s3 :: _ -> sprintf "%s|%s|%s\n" (Slot.draw s1 cursor) (Slot.draw s2 cursor) (Slot.draw s3 cursor) 
            | _ -> ""

        slots
        |> List.chunkBySize 3
        |> List.fold (fun accL line -> accL + drawLine line cursor) ""

    let isValidMove (Board slots) position =
        slots
        |> List.exists (fun slot ->
            match slot with
            | SlotEmpty slotPos -> slotPos = position
            | _ -> false)

    let putPieceAtSlot (Board slots) position (Turn piece: Turn) =
        slots
        |> List.map (fun slot ->
            match slot with
            | SlotEmpty slotPosition -> if slotPosition = position then SlotFull (slotPosition, piece) else slot
            | _ -> slot)
        |> Board

    let hasPieceWonXorY (Board slots) piece (Position (xAxis, yAxis)) =
        slots
        |> List.filter (fun slot ->
            match slot with
            | SlotFull (Position (slotX, slotY), slotPiece) when slotPiece = piece -> slotX = xAxis || slotY = yAxis
            | _ -> false)
        |> List.length |> fun amount -> amount >= 3

    let hasPieceWon board piece =
        [Position.create Left Top; Position.create MiddleX MiddleY; Position.create Right Bottom]
        |> List.map (fun axis -> hasPieceWonXorY board piece axis)
        |> List.fold (||) false


type GameState = Running | EndMatch of Piece | Exit

type GameWorld = {
    Board: Board
    Turn: Turn
    Cursor: Position
    State: GameState
}

let newGame = { Board = Board.create; Turn = Turn X; Cursor = Position.create MiddleX MiddleY; State = Running }

type GameAction =
    | ActionUp
    | ActionRight
    | ActionDown
    | ActionLeft
    | ActionCommit
    | ActionQuit
    | ActionRestart
    | ActionEmpty

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
    | (ActionUp, Position (x, y)) -> { world with Cursor = Position (x, (moveUp y)) }
    | (ActionRight, Position (x, y)) -> { world with Cursor = Position ((moveRight x), y) }
    | (ActionDown, Position (x, y)) -> { world with Cursor = Position (x, (moveDown y)) }
    | (ActionLeft, Position (x, y)) -> { world with Cursor = Position ((moveLeft x), y) }

    | (ActionCommit, cursorPos) ->
        if Board.isValidMove world.Board cursorPos then
            let newBoard = Board.putPieceAtSlot world.Board cursorPos world.Turn
            { world with Board = newBoard; Turn = Turn.change world.Turn }
        else
            world

    | (ActionQuit, _) -> { world with State = Exit }
    | (ActionRestart, _) -> newGame
    | (ActionEmpty, _) -> world

let drawGame world =
    Console.Clear()
    let (Turn piece) = world.Turn
    printfn "Turn of %s" (Piece.drawCursorOver piece)
    printf "%s" (Board.draw world.Board world.Cursor)

    printf "Has any player won: %b" (Board.hasPieceWon world.Board O)

    let input = Console.ReadKey()
    match input.Key with
    | ConsoleKey.UpArrow -> ActionUp
    | ConsoleKey.RightArrow -> ActionRight
    | ConsoleKey.DownArrow -> ActionDown
    | ConsoleKey.LeftArrow -> ActionLeft
    | ConsoleKey.Enter -> ActionCommit
    | ConsoleKey.Escape -> ActionQuit
    | ConsoleKey.R -> ActionRestart
    | _ -> ActionEmpty

let rec game world =
    match world.State with
    | Running -> game (world |> drawGame |> (updateGame world))
    | EndMatch w -> ActionEmpty
    | Exit -> ActionEmpty

[<EntryPoint>]
let main argv =
    game newGame |> ignore
    0
