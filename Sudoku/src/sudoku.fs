module Sudoku

open FsConstraintSolver

// +-----+-----+-----+-----+-----+-----+-----+-----+-----+
// |0    |1    |2    |3    |4    |5    |6    |7    |8    |
// +-----+-----+-----+-----+-----+-----+-----+-----+-----+
// |9    |10   |11   |12   |13   |14   |15   |16   |17   |
// +-----+-----+-----+-----+-----+-----+-----+-----+-----+
// |18   |19   |20   |21   |22   |23   |24   |25   |26   |
// +-----+-----+-----+-----+-----+-----+-----+-----+-----+
// |27   |28   |29   |30   |31   |32   |33   |34   |35   |
// +-----+-----+-----+-----+-----+-----+-----+-----+-----+
// |36   |37   |38   |39   |40   |41   |42   |43   |44   |
// +-----+-----+-----+-----+-----+-----+-----+-----+-----+
// |45   |46   |47   |48   |49   |50   |51   |52   |53   |
// +-----+-----+-----+-----+-----+-----+-----+-----+-----+
// |54   |55   |56   |57   |58   |59   |60   |61   |62   |
// +-----+-----+-----+-----+-----+-----+-----+-----+-----+
// |63   |64   |65   |66   |67   |68   |69   |70   |71   |
// +-----+-----+-----+-----+-----+-----+-----+-----+-----+
// |72   |73   |74   |75   |76   |77   |78   |79   |80   |
// +-----+-----+-----+-----+-----+-----+-----+-----+-----+

type CellValue =
    |Empty |One |Two |Three |Four |Five |Six |Seven |Eight |Nine
    with
        member x.toInt() =
                match x with
                | Empty -> None
                | One   -> Some 1 | Two   -> Some 2 | Three -> Some 3
                | Four  -> Some 4 | Five  -> Some 5 | Six   -> Some 6
                | Seven -> Some 7 | Eight -> Some 8 | Nine  -> Some 9
        member x.toStr() =
                x.toInt() |> Option.map string |> Option.defaultValue " "
type CellPos = int * int
let cellValue (x:int) =
    match x with
    | x when (x < 1) && (x > 9) -> Empty
    | 1 -> One   | 2 -> Two   | 3 -> Three
    | 4 -> Four  | 5 -> Five  | 6 -> Six
    | 7 -> Seven | 8 -> Eight | 9 -> Nine
    | _ -> failwith "Shouldn't happen #saod87h"

type SudokuTable = Map<CellPos,CellValue>


let empty =
    List.allPairs [1..9] [1..9]
    |> List.map (fun pos -> pos,Empty)
    |> Map.ofList

let buildSudokuConstraints() =

    // Rows in which nodes cannot be equal
    let rows =
        [0 .. 9 .. 72]
        |> List.map (fun n -> List.map ((+) n) [0..8])

    // Columns in which nodes cannot be equal
    let columns =
        [0..8]
        |> List.map (fun n -> List.map ((+) n) [0 .. 9 .. 72])

    // Ninths in which nodes cannot be equal
    let ninths =
        [0 .. 3 .. 6]
        |> List.collect (fun n -> List.map ((+) n) [0;27;54])
        |> List.map (fun n -> List.map ((+) n) [0;1;2;9;10;11;18;19;20])

    // Default domains
    let domains =
        Set.ofList [1..9]
        |> List.replicate 81

    // Generate constraints
    let constrs =
        rows @ columns @ ninths
        |> List.collect (fun l -> List.allPairs l l)
        |> List.filter (fun (a,b) -> a <> b)
        |> List.map (fun (a,b) -> (a, Neq b))

    // Build and get result
    constraintGraphBuild constrs domains
    |> function
       | Ok    x   -> x
       | Error str -> failwith "huh?"

// Cell coordinate starts at 1
let setCell (cGraph:ConstraintGraph<_>) ((x,y):CellPos) (value:CellValue) =
    let cellIndex = x - 1 + (y - 1)*9
    match value.toInt() with
    | Some x ->
        setDomain cGraph cellIndex (Set.ofList [x])
    | None   ->
        setDomain cGraph cellIndex (Set.ofList [1..9])

let getCell (table:SudokuTable) i (dmn,_) =
    let value =
        match Set.toList dmn with
        | [x] -> cellValue x
        | _ -> failwith "Shouldn't happen #98ads"
    Map.add (i%9 + 1 , i/9 + 1) value table

let tableToCGraph table =
    Map.fold setCell (buildSudokuConstraints()) table

let cGraphToTable cGraph =
    Map.fold getCell empty cGraph


let solve (table:SudokuTable) =
    tableToCGraph table
    |> backtrackingSearch makeArcConsistent
    |> Option.map cGraphToTable
    |> Option.defaultValue table
