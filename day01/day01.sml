val lines = String.tokens (Char.contains "\n\r")
val unlines = String.concatWith "\n"

fun readLines filename =
    let val ins = TextIO.openIn filename
    in  lines (TextIO.inputAll ins)
        before TextIO.closeIn ins
    end

infix |>
fun x |> f = f x

fun sum xs = foldl (op+) 0 xs
fun maximum xs = foldl Int.max (hd xs) xs
fun woopsie() = raise Fail "Should not happen"


fun input() = readLines "input.txt"

val test = [ "1abc2",
             "pqr3stu8vwx",
             "a1b2c3d4e5f",
             "treb7uchet"]

fun find p s = CharVector.find p s |> valOf
fun stringRev s = String.explode s |> rev |> CharVector.fromList
fun findLast p s = stringRev s |> find p

fun firstAndLast cs =
    String.implode [find Char.isDigit cs, findLast Char.isDigit cs]

fun parse_line_part1 line =
    line
        |> firstAndLast
        |> Int.fromString
        |> valOf

fun report part fmt ans =
    ( print part; print ": "
    ; fmt ans |> print
    ; print "\n")

fun part1 input = input |> map parse_line_part1 |> sum
val answer1 = input() |> part1 |> report "Part 1" Int.toString

val test2 = ["two1nine",
             "eightwothree",
             "abcone2threexyz",
             "xtwone3four",
             "4nineeightseven2",
             "zoneight234",
             "7pqrstsixteen"]

fun scanNumWrong cs =
    case cs of
        #"o" :: #"n" :: #"e" ::  cs => SOME(#"1", cs)
      | #"t" :: #"w" :: #"o" ::  cs => SOME(#"2", cs)
      | #"t" :: #"h" :: #"r" :: #"e" :: #"e" ::  cs => SOME(#"3", cs)
      | #"f" :: #"o" :: #"u" :: #"r" ::  cs => SOME(#"4", cs)
      | #"f" :: #"i" :: #"v" :: #"e" ::  cs => SOME(#"5", cs)
      | #"s" :: #"i" :: #"x" ::  cs => SOME(#"6", cs)
      | #"s" :: #"e" :: #"v" :: #"e" :: #"n" ::  cs => SOME(#"7", cs)
      | #"e" :: #"i" :: #"g" :: #"h" :: #"t" ::  cs => SOME(#"8", cs)
      | #"n" :: #"i" :: #"n" :: #"e" ::  cs => SOME(#"9", cs)
      | c :: cs => if Char.isDigit c then SOME(c, cs) else NONE
      | _ => NONE

fun scanWrong [] = []
  | scanWrong cs =
    case scanNumWrong cs of
        SOME(c, cs) => c :: scanWrong cs
      | _ => scanWrong (tl cs)

fun scanNum cs =
    case cs of
        #"o" :: #"n" :: #"e" :: _ => SOME #"1"
      | #"t" :: #"w" :: #"o" :: _ => SOME #"2"
      | #"t" :: #"h" :: #"r" :: #"e" :: #"e" :: _ => SOME #"3"
      | #"f" :: #"o" :: #"u" :: #"r" :: _ => SOME #"4"
      | #"f" :: #"i" :: #"v" :: #"e" :: _ => SOME #"5"
      | #"s" :: #"i" :: #"x" :: _ => SOME #"6"
      | #"s" :: #"e" :: #"v" :: #"e" :: #"n" :: _ => SOME #"7"
      | #"e" :: #"i" :: #"g" :: #"h" :: #"t" :: _ => SOME #"8"
      | #"n" :: #"i" :: #"n" :: #"e" :: _ => SOME #"9"
      | c :: _ => if Char.isDigit c then SOME c else NONE
      | _ => NONE

fun scan [] = []
  | scan (cs as _ :: rest) =
    case scanNum cs of
        SOME c => c :: scan rest
      | _ => scan rest

fun firstAndLast2 cs =
    String.implode [hd cs, hd(rev cs)]

fun parse_line_part2 line =
    line
        |> String.explode
        |> scan
        |> firstAndLast2
        |> Int.fromString
        |> valOf

fun part2 input = input |> map parse_line_part2 |> sum
val answer2 = input() |> part2 |> report "Part 2" Int.toString
