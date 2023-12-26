namespace advent

module Common = 
    open System.IO
    let input day = $"./Inputs/day%i{day}" |> File.ReadAllLines

    let answerMsg p result = 
        sprintf "Part %i - Your puzzle answer was: %i" p result

module Day1 =
    let numbers =
        [
            "zero", "0"
            "one", "1"
            "two", "2"
            "three", "3"
            "four", "4"
            "five", "5"
            "six", "6"
            "seven", "7"
            "eight", "8"
            "nine", "9"
        ] |> Set.ofList

    let tryToInt (s: string): option<int> =
        match System.Int32.TryParse s with
        | true, v -> Some v
        | false, _ -> None
    
    let isDigit = System.Text.RegularExpressions.Regex("\d")
    
    let fromSpelled (s: string) =
        match tryToInt s with
        | Some _ -> s
        | None -> 
        numbers
        |> Set.filter (fun (spelled, _) -> s = spelled) 
        |> Seq.head 
        |> snd

    let decrypt (s: string): seq<(string * int)> = 
        numbers 
        |> Seq.collect (fun (k, v) -> [|
            (k, s.IndexOf(k))
            (k, s.LastIndexOf(k))
            (v, s.IndexOf(v))
            (v, s.LastIndexOf(v))
        |])
        |> Seq.sortBy (fun (_, idx) -> idx)
        |> Seq.filter (fun (_, idx) -> idx >= 0)
        
    let toDigits (s: string) = 
        seq {
            for m in isDigit.Matches s do
                yield m.Groups[0].Value
        } |> Seq.reduce (fun acc e -> acc + e)
    
    let toCalibration (s: string) = 
        match s.Length with
        | 1 -> sprintf "%c%c" s[0] s[0]
        | 2 -> s
        | _ -> sprintf "%c%c" s[0] s[s.Length - 1]
    
    let partOne (document: seq<string>) =
        document
        |> Seq.map toDigits
        |> Seq.map toCalibration
        |> Seq.choose tryToInt
        |> Seq.sum

    let partTwo (document: seq<string>) =
        document
        |> Seq.map decrypt
        |> Seq.map (fun (s) -> (
            s |> Seq.head |> fst |> fromSpelled,  
            s |> Seq.last |> fst |> fromSpelled
        ))
        |> Seq.map (fun (f, s) -> f + s) 
        |> Seq.choose tryToInt
        |> Seq.sum
    
    let invoke =
        printfn "Day 1"
        
        let input = Common.input 1

        input
        |> partOne
        |> Common.answerMsg 1
        |> printfn "%s"

        input
        |> partTwo
        |> Common.answerMsg 2
        |> printfn "%s"

