let readLines filePath = System.IO.File.ReadLines filePath

let words (s : string) = s.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)

let parse (str : string) =
    let nums = ((str.Split ":")[1]).Split "|"
    let ws, hs = nums[0], nums[1]
    let ints s = words s |> Seq.map int
    (ints ws, ints hs)

let input () =
    readLines "input.txt"
    |> Seq.map parse

let matches (wins, has) =
    let common = Set.intersect (Set.ofSeq wins) (Set.ofSeq has)
    Set.count common

let pow2 n = 1 <<< n

let points card =
    let m = matches card
    if m = 0 then 0 else pow2 (m-1)

let part1 input =
    Seq.map points input
    |> Seq.sum

let rec addCopies i m ns =
    match i, m, ns with
        | 0, _, rest -> rest
        | i, m, (n :: rest) -> (m+n) :: addCopies (i-1) m rest
        | _ -> failwith "Impossible"

let game acc card =
    match acc with
        | (n :: copies, sum) ->
            let m = matches card
            (addCopies m n copies, sum+n)
        | _ -> failwith "Impossible"

let part2 input =
    let copies = List.init (Seq.length input) (fun _ -> 1)
    let (_, result) = Seq.fold game (copies, 0) input
    result

let main =
    let inp = input()
    printfn "Part 1: %A" (part1 inp)
    printfn "Part 2: %A" (part2 inp)
