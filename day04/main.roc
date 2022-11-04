app "day04"
    packages { pf: "../../roc/examples/cli/cli-platform/main.roc" }
    imports [
        pf.Program.{ Program, ExitCode },
        pf.Stdout,
        pf.Task.{ Task, await },
        pf.File,
        pf.Path.{ Path },
    ]
    provides [main] to pf

main : Program
main = Program.noArgs mainTask

mainTask : Task ExitCode [] [Write [Stdout], Read [File]]
mainTask =
    task =
        fileContents <- await (File.readUtf8 (Path.fromStr "input.txt"))
        fileContentChunks =
            fileContents
            |> Str.trim
            |> Str.split "\n\n"
        numsToDrawStr <-
            List.first fileContentChunks
            |> Result.mapErr \_ -> InvalidInput
            |> Task.fromResult
            |> await
        bingoBoardStrs = List.dropFirst fileContentChunks
        numsToDraw <-
            Str.split numsToDrawStr ","
            |> List.mapTry Str.toNat
            |> Result.mapErr \_ -> InvalidInput
            |> Task.fromResult
            |> await
        boards <-
            bingoBoardStrs
            |> List.mapTry parseBingoBoard
            |> Task.fromResult
            |> await
        firstBoard <-
            List.first boards
            |> Result.mapErr \_ -> InvalidInput
            |> Task.fromResult
            |> await
        output =
            (List.map firstBoard \row ->
                List.map row Num.toStr
                |> Str.joinWith ", ")
            |> Str.joinWith "\n"
        Stdout.line output

    Task.attempt task \result ->
        when result is
            Ok {} -> Task.succeed (Program.exitCode 0)
            Err e ->
                msg =
                    when e is
                        FileReadErr path _ | FileReadUtf8Err path _ ->
                            pathStr = Path.display path
                            "Failed to read file \"\(pathStr)\""
                        InvalidInput | InvalidBingoBoardStr ->
                            "Invalid Input"
                Stdout.line "Error: \(msg)"
                |> Program.exit 1

parseBingoBoard : Str -> Result BingoBoard [InvalidBingoBoardStr]
parseBingoBoard = \string ->
    lines <-
        Str.split string "\n"
        |> \lines ->
            when List.len lines is
                5 -> Ok lines
                _ -> Err InvalidBingoBoardStr
        |> Result.try
    List.mapTry lines \l ->
        l
        |> Str.split " "
        |> List.dropIf Str.isEmpty
        |> List.mapTry Str.toNat
        |> Result.mapErr \_ -> InvalidBingoBoardStr

doesBoardWin : BingoBoard, Set Nat -> Bool
doesBoardWin = \board, drawnNums ->
    numWasDrawn = \num -> Set.contains drawnNums num

    doesRowWin : BingoBoard, Nat -> Bool
    doesRowWin = \b, rowIndex ->
        expect rowIndex < 5
        row = List.get b rowIndex |> Result.withDefault (crash {})
        List.all row numWasDrawn

    doesColumnWin : BingoBoard, Nat -> Bool
    doesColumnWin = \b, colIndex ->
        expect colIndex < 5
        List.all b \row ->
            num = List.get row colIndex |> Result.withDefault (crash {})
            numWasDrawn num

    List.range 0 4
    |> List.any \index ->
        doesRowWin board index || doesColumnWin board index

BingoBoard : List (List Nat)

crash : {} -> *

expect
    parseBingoBoard
    """
    97 62 17  5 79
     1 99 98 80 84
    44 16  2 40 94
    68 95 49 32  8
    38 35 23 89  3
    """
    == Ok [
          [97, 62, 17,  5, 79],
          [1, 99, 98, 80, 84],
          [44, 16, 2, 40, 94],
          [68, 95, 49, 32, 8],
          [38, 35, 23, 89, 3],
      ]

testBoard =
    parseBingoBoard
    """
    1  2  3  4  5
    6  7  8  9  10
    11 12 13 14 15
    16 17 18 19 20
    21 22 23 24 25
    """

expect
    testBoard == Ok [
       [1, 2, 3, 4, 5],
       [6, 7, 8, 9, 10],
       [11, 12, 13, 14, 15],
       [16, 17, 18, 19, 20],
       [21, 22, 23, 24, 25],
       ]

expect
    (doesBoardWin testBoard (Set.fromList [1, 2, 3, 4, 5])) == True

expect
    (doesBoardWin testBoard (Set.fromList [1, 6, 11, 16, 21])) == True

expect
    (doesBoardWin testBoard (Set.fromList [1, 2, 3, 4, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 25])) == False

expect
    (doesBoardWin testBoard (Set.fromList [1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 25])) == True
