app "day03"
    packages { pf: "../cli-platform/main.roc" }
    imports [
        pf.Program.{ Program, ExitCode },
        pf.Stdout,
        pf.Task.{ Task, await },
        pf.File,
        pf.Path.{ Path },
    ]
    provides [main] to pf

validBinaryNumberLength = 12

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
        parsedInputs <- await (Task.fromResult (List.mapTry fileLines validateAndParseInputString))
        { gamma, epsilon } = calculatePowerConsumption parsedInputs
        { oxygenGeneratorRating, co2ScrubberRating } = calculateLifeSupport parsedInputs
        _ <- await (Stdout.line (Num.toStr (gamma * epsilon)))
        Stdout.line (Num.toStr (oxygenGeneratorRating * co2ScrubberRating))

    Task.attempt task \result ->
        when result is
            Ok {} -> Task.succeed (Program.exitCode 0)
            Err err ->
                msg = 
                    when err is
                        InvalidBinaryNumStr ->
                            "Found invalid character when parsing input file (must be only 1's and 0's)"
                        WrongInputLength ->
                            "Found input of wrong length in input file"
                        FileReadErr path _ ->
                            filename = Path.display path
                            "Failed to read file \"\(filename)\""
                        FileReadUtf8Err path _ ->
                            filename = Path.display path
                            "Found invalid characters when attempting to read file \"\(filename)\""
                Stdout.line "Error: \(msg)" |> Program.exit 1

validateAndParseInputString : Str -> Result ValidatedBinaryNumber [WrongInputLength, InvalidBinaryNumStr]*
validateAndParseInputString = \string ->
    if Str.countUtf8Bytes string != validBinaryNumberLength then
        Err WrongInputLength
    else
        Str.toUtf8 string |> List.mapTry \char ->
            when char is
                '0' -> Ok Zero
                '1' -> Ok One
                _ -> Err InvalidBinaryNumStr

calculatePowerConsumption : List ValidatedBinaryNumber -> PowerConsumption
calculatePowerConsumption = \inputs ->
    numInputs = List.len inputs

    oneCounts = countOnesInEachPosition inputs
    gammaBinaryNumber =
        List.map oneCounts \count ->
            onesRatio = (Num.toFrac count) / (Num.toFrac numInputs)
            if onesRatio >= 0.5 then
                One
            else
                Zero
    epsilonBinaryNumber =
        List.map gammaBinaryNumber flipBinaryDigit

    { gamma: binaryNumberToNat gammaBinaryNumber, epsilon: binaryNumberToNat epsilonBinaryNumber }

calculateLifeSupport : List ValidatedBinaryNumber -> LifeSupportRating
calculateLifeSupport = \inputs ->
    oxygenRatingBinary = calculateOxygenRating inputs 0 |> List.first |> Result.withDefault []
    co2RatingBinary = calculateCo2Rating inputs 0 |> List.first |> Result.withDefault []
    {
        oxygenGeneratorRating: binaryNumberToNat oxygenRatingBinary,
        co2ScrubberRating: binaryNumberToNat co2RatingBinary,
    }

calculateOxygenRating : List ValidatedBinaryNumber, Nat -> List ValidatedBinaryNumber
calculateOxygenRating = \inputs, digitIndex ->
    if List.len inputs <= 1 then
        inputs
    else if digitIndex >= validBinaryNumberLength then
        inputs
    else
        mostCommonDigit = findMostCommonDigitInPosition inputs digitIndex
        filteredInputs = List.keepIf inputs \number ->
            (binaryNumberGetDigit number digitIndex) == mostCommonDigit
        calculateOxygenRating filteredInputs (digitIndex + 1)

calculateCo2Rating : List ValidatedBinaryNumber, Nat -> List ValidatedBinaryNumber
calculateCo2Rating = \inputs, digitIndex ->
    if List.len inputs <= 1 then
        inputs
    else if digitIndex >= validBinaryNumberLength then
        inputs
    else
        mostCommonDigit = findMostCommonDigitInPosition inputs digitIndex
        leastCommonDigit = flipBinaryDigit mostCommonDigit
        filteredInputs = List.keepIf inputs \number ->
            (binaryNumberGetDigit number digitIndex) == leastCommonDigit
        calculateCo2Rating filteredInputs (digitIndex + 1)

countOnesInEachPosition : List ValidatedBinaryNumber -> List Nat
countOnesInEachPosition = \numbers ->
    sumTwoListsPairwise = \a, b ->
        List.map2 a b Num.add

    List.walk numbers (List.repeat 0 12) \total, num ->
        sumTwoListsPairwise total (List.map num binaryDigitToNat)

findMostCommonDigitInPosition : List ValidatedBinaryNumber, Nat -> BinaryDigit
findMostCommonDigitInPosition = \numbers, digitIndex ->
    getDigitInPosition = \num -> binaryNumberGetDigit num digitIndex

    onesCount =
        List.map numbers getDigitInPosition
        |> List.countIf \digit -> digit == One
    onesRatio =
        (Num.toFrac onesCount) / (Num.toFrac (List.len numbers))

    if onesRatio >= 0.5 then
        One
    else
        Zero

flipBinaryDigit : BinaryDigit -> BinaryDigit
flipBinaryDigit = \digit ->
    when digit is
        One -> Zero
        Zero -> One

binaryDigitToNat : BinaryDigit -> Nat
binaryDigitToNat = \digit ->
    when digit is
        Zero -> 0
        One -> 1

binaryNumberToNat : ValidatedBinaryNumber -> Nat
binaryNumberToNat = \number ->
    List.walk number 0 \total, digit ->
        when digit is
            Zero -> (Num.shiftLeftBy total 1) + 0
            One -> (Num.shiftLeftBy total 1) + 1

binaryNumberGetDigit : ValidatedBinaryNumber, Nat -> BinaryDigit
binaryNumberGetDigit = \number, index ->
    when List.get number index is
        Ok digit -> digit
        Err OutOfBounds -> crash {}

PowerConsumption : {
    gamma: Nat,
    epsilon: Nat,
}

LifeSupportRating : {
    co2ScrubberRating: Nat,
    oxygenGeneratorRating: Nat,
}

ValidatedBinaryNumber : List BinaryDigit

BinaryDigit : [Zero, One]

crash : {} -> *

expect validateAndParseInputString "001101011011" == Ok [Zero,Zero,One,One,Zero,One,Zero,One,One,Zero,One,One]
expect validateAndParseInputString "0001101011011" == Err WrongInputLength
expect validateAndParseInputString "031101011011" == Err InvalidBinaryNumStr
expect validateAndParseInputString "01101011011" == Err WrongInputLength

expect binaryNumberToNat [Zero, One, One, Zero] == 6
expect binaryNumberToNat [One, Zero, Zero, Zero, One, One] == 35

expect countOnesInEachPosition [
    [One, Zero, One, One, Zero, Zero],
    [Zero, One, One, One, Zero, Zero],
    [One, Zero, One, One, Zero, Zero],
  ] == [2, 1, 3, 3, 0, 0]

expect ([
    [One, Zero, One, One, Zero, Zero],
    [Zero, One, One, One, Zero, Zero],
    [One, Zero, One, One, Zero, Zero],
  ] |> findMostCommonDigitInPosition 2) == One

expect ([
    [One, Zero, One, One, Zero, Zero],
    [Zero, One, One, One, Zero, Zero],
    [One, Zero, One, One, Zero, Zero],
  ] |> findMostCommonDigitInPosition 1) == Zero

expect (calculatePowerConsumption [
    [Zero, Zero, One, One, Zero, Zero, One, One, Zero, Zero, One, One],
    [Zero, Zero, One, One, Zero, Zero, One, One, Zero, Zero, One, One],
    [Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero, Zero],
    ]) == {gamma: 0b001100110011, epsilon: 0b110011001100}

expect ((calculateLifeSupport [
    [Zero, Zero, One, One, Zero, Zero, One, One, Zero, Zero, One, One],
    [Zero, Zero, One, One, Zero, Zero, One, One, Zero, Zero, One, Zero],
    [One, One, Zero, Zero, One, One, Zero, Zero, One, One, Zero, Zero],
  ]) |> .oxygenGeneratorRating) == 0b001100110011

expect ((calculateLifeSupport [
    [Zero, Zero, One, One, Zero, Zero, One, One, Zero, Zero, One, One],
    [Zero, Zero, One, One, Zero, Zero, One, One, Zero, Zero, One, Zero],
    [One, One, Zero, Zero, One, One, Zero, Zero, One, One, Zero, Zero],
  ]) |> .co2ScrubberRating) == 0b110011001100
