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
        #numsToDrawStr <- await (Task.fromResult (List.first fileContentChunks |> Result.onErr \_ -> Err InvalidInput))
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
        Stdout.line "hi"

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

BingoBoard : List (List Nat)
