app "day02"
    packages { pf: "../cli-platform/main.roc" }
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

mainTask : Task ExitCode [] [Read [File], Write [Stdout]]
mainTask =
    task =
        fileContents <- await (File.readUtf8 (Path.fromStr "input.txt"))
        fileLines =
            fileContents
            |> Str.trim
            |> Str.split "\n"

        moveCommands <- await (Task.fromResult (List.mapTry fileLines parseMoveCommand))

        answer1 = part1CalculatePosition moveCommands
        answer2 = part2CalculatePosition moveCommands

        _ <- await (Stdout.line (Num.toStr (answer1.depth * answer1.distance)))
        Stdout.line (Num.toStr (answer2.depth * answer2.distance))

    Task.attempt task \result ->
        when result is
            Ok {} ->
                Task.succeed (Program.exitCode 0)

            Err err ->
                msg =
                    when err is
                        FileReadErr path _ | FileReadUtf8Err path _ ->
                            filename = Path.display path

                            "Failed to read file \"\(filename)\""

                        InvalidMoveCommand -> "Encountered an invalid move command"

                Stdout.line "Error: \(msg)"
                |> Program.exit 1

part1CalculatePosition : List MoveCommand -> Position
part1CalculatePosition = \commands ->
    List.walk commands initialPosition \state, command ->
        { direction, amount } = command

        when direction is
            Up -> { state & depth: state.depth - amount }
            Down -> { state & depth: state.depth + amount }
            Forward -> { state & distance: state.distance + amount }

part2CalculatePosition : List MoveCommand -> Position
part2CalculatePosition = \commands ->
    List.walk commands { initialPosition & aim: 0 } \state, command ->
        { direction, amount } = command

        when direction is
            Up -> { state & aim: state.aim - amount }
            Down -> { state & aim: state.aim + amount }
            Forward -> { state & distance: state.distance + amount, depth: state.depth + (state.aim * amount) }

parseMoveCommand : Str -> Result MoveCommand [InvalidMoveCommand]*
parseMoveCommand = \commandStr ->
    result =
        { before: directionStr, after: distanceStr } <- Result.try (Str.splitFirst commandStr " ")
        direction <- Result.try (parseDirection directionStr)
        amount <- Result.map (Str.toNat distanceStr)
        { direction, amount }

    result
    |> Result.mapErr \_ -> InvalidMoveCommand

parseDirection : Str -> Result Direction [InvalidDirection]*
parseDirection = \str ->
    if Str.startsWith str "up" then
        Ok Up
    else if Str.startsWith str "down" then
        Ok Down
    else if Str.startsWith str "forward" then
        Ok Forward
    else
        Err InvalidDirection

MoveCommand : {
    direction : Direction,
    amount : Nat,
}

Direction : [Up, Down, Forward]

Position : {
    aim : Nat,
    depth : Nat,
    distance : Nat,
}

initialPosition : Position
initialPosition = { aim: 0, depth: 0, distance: 0 }
