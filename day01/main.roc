app "day01"
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
        fileContent <- await (File.readUtf8 (Path.fromStr "input.txt"))
        fileLines =
            fileContent
            |> Str.trim
            |> Str.split "\n"

        depths <- await (Task.fromResult (List.mapTry fileLines Str.toNat))
        numDepthIncreases = countAdjacentIncreases depths
        numTripleWindowDepthIncreases = countTripleWindowIncreases depths

        _ <- await (Stdout.line (Num.toStr numDepthIncreases))
        Stdout.line (Num.toStr numTripleWindowDepthIncreases)

    Task.attempt task \result ->
        when result is
            Ok {} ->
                Program.exitCode 0 |> Task.succeed

            Err err ->
                msg =
                    when err is
                        FileReadErr path _ | FileReadUtf8Err path _ ->
                            filename = Path.display path

                            "Failed to read file \"\(filename)\""

                        InvalidNumStr -> "Encountered invalid characters for numeral"

                Stdout.line "Error: \(msg)"
                |> Program.exit 1

countAdjacentIncreases : List Nat -> Nat
countAdjacentIncreases = \nums ->
    adjacentPairs = List.map2 nums (List.dropFirst nums) Pair

    List.countIf adjacentPairs \Pair prev next -> next > prev

countTripleWindowIncreases : List Nat -> Nat
countTripleWindowIncreases = \depths ->
    tripleWindowSums =
        List.map3 depths (List.dropFirst depths) (List.drop depths 2) \first, second, third ->
            first + second + third

    countAdjacentIncreases tripleWindowSums
