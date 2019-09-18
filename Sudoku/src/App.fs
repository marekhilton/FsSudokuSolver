module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open System

// MODEL

type Model = int
type State =
    {table:Sudoku.SudokuTable
     active: Sudoku.CellPos option}

// EVENTS
type Event =
    | SetCell of Sudoku.CellPos * string
    | Solve
    | Reset
    | StartEdit of Sudoku.CellPos
    | EndEdit

let init() : State =
    {table = Sudoku.empty
     active = None}

// UPDATE

let update (msg:Event) (state:State) =
    match msg with
    | SetCell(pos,str) ->
        match Int32.TryParse str with
        | true , x when (x > 0) && (x < 10) ->
            {state
             with table = Map.add pos (Sudoku.cellValue x) state.table}
        | false , _ -> {state
                        with table = Map.add pos Sudoku.Empty state.table}
        | _ -> state
    | Solve -> {state with table = Sudoku.solve state.table}
    | Reset -> init()
    | StartEdit pos -> {state with active = Some pos}
    | EndEdit -> {state with active = None}

// VIEW (rendered with React)

let border = "3px solid black"
let cellDims = ("40px","40px")
let borderStyle pos =
    let vBorder =
        if (fst pos) % 3 = 0
        then [BorderRight border]
        elif (fst pos % 3 = 1)
        then [BorderLeft border]
        else []
    let hBorder =
        if (snd pos) % 3 = 0
        then [BorderBottom border]
        elif (snd pos % 3 = 1)
        then [BorderTop border]
        else []
    vBorder @ hBorder
let cellStyle =
    [Background "#b0b0b0";
     Width (fst cellDims);
     TextAlign TextAlignOptions.Center]
let renderEditor dispatch pos value =
    td [Class "selected"
        Style [TextAlign TextAlignOptions.Center]]
       [input [
          Style [Width "20px"
                 TextAlign TextAlignOptions.Center]
          AutoFocus true
          OnChange (fun e ->
                        dispatch (SetCell(pos,e.Value))
                        dispatch EndEdit)
          Value value ]
    ]

let renderCell dispatch pos state =
    if state.active = Some pos then
        let cValue =
            Map.tryFind pos state.table
            |> Option.map (fun v -> v.toStr())
        renderEditor dispatch pos cValue
    else
        match Map.tryFind pos state.table with
        | Some cValue ->
            Option.map string (cValue.toInt())
            |> Option.defaultValue ""
        | None ->
            "ERROR"
        |> fun s -> td [Style (cellStyle @ (borderStyle pos))
                        OnClick (fun _ -> dispatch(StartEdit pos))]
                       [str s]

let tableToUI state dispatch renderCellFunc =
    let renderRow row =
        tr [Style [VerticalAlign "center";
                   Height (snd cellDims)]]
           [for pos in row -> renderCellFunc dispatch pos state]
    Map.toSeq state.table
    |> Seq.groupBy (fun ((x,y),v) -> y)
    |> Seq.sortBy fst
    |> Seq.map ((fun (_,lst) -> Seq.sortBy fst lst |> Seq.map fst) >> renderRow)

let sudokuTable state dispatch =
    table [] [
        thead [] []
        tbody []
              (tableToUI state dispatch renderCell)
    ]
let view (state:State) dispatch =
    div [] [
        sudokuTable state dispatch
        div []
            [button [OnClick (fun _ -> dispatch Solve)] [ str "Solve"];
             button [OnClick (fun _ -> dispatch Reset)] [ str "Reset"]]
    ]

    
// App
Program.mkSimple init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
