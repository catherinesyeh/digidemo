module App

open Browser.Dom
open Browser.Svg
open Fable.Core

open ProgramParser
open ProgramInterpreter

// Get references to the required HTML elements
let myButton = document.querySelector(".submit") :?> Browser.Types.HTMLButtonElement
let myCode = document.querySelector(".code") :?> Browser.Types.HTMLTextAreaElement

let body = document.querySelector(".body") :?> Browser.Types.HTMLBodyElement
let left = document.querySelector(".left") :?> Browser.Types.HTMLDivElement
let right = document.querySelector(".right") :?> Browser.Types.HTMLDivElement

let select = document.querySelector(".ex") :?> Browser.Types.HTMLSelectElement

// Fix layout of site
window.onload <- fun _ ->
    body.insertBefore(left, body.firstChild) |> ignore

    let input = myCode.value // get code
    myButton.click() |> ignore
    myCode.value <- input

// Register button click
myButton.onclick <- fun _ ->
    let input = myCode.value // get code

    match parse input with // try to parse expression
    | Some ast -> 
        let svg = eval ast
        right.innerHTML <- svg
        myCode.value <- "Enjoy your pattern. Feel free to write another program!"
    | None -> 
        myCode.value <- "Invalid expression."

// Load an example
select.onchange <- fun _ ->
    let choice = select.value
    match choice with
    | "2rows" ->
        myCode.value <-
            "(name 2ROWS)" +
            "\n\n(strings 6 CornflowerBlue CornflowerBlue Coral Coral BurlyWood BurlyWood)" +
            "\n\n(repeat 6 AAAAA)"
    | "arrow" ->
         myCode.value <-
            "(name ARROW)" +
            "\n\n(strings 8 LightPink LightSeaGreen LightSeaGreen LightPink LightPink LightSeaGreen LightSeaGreen LightPink)" +
            "\n\n(repeat 2 A_A_B_B _A_A_B_)"
    | "heart" ->
        myCode.value <-
            "(name HEART)" +
            "\n\n(strings 8 PeachPuff PaleVioletRed PeachPuff PaleVioletRed PaleVioletRed PeachPuff PaleVioletRed PeachPuff)" +
            "\n\nC_A_B_D" + 
            "\n_A_A_B_" +
            "\nB_A_B_A" +
            "\n_A_A_B_" +
            "\nA_A_B_B" + 
            "\n_A_A_B_" +
            "\nC_A_B_D" + 
            "\n_D_A_C_"
    | "gradientsquares" ->
        myCode.value <-
            "(name GRADIENTSQUARES)" +
            "\n\n(strings 16" + 
            "\ngray" +
            "\nsilver" +
            "\nindianred" +
            "\nlightblue" +
            "\nrosybrown" +
            "\nskyblue" +
            "\nsilver" +
            "\ngray" +
            "\ngray" +
            "\nsilver" +
            "\nskyblue" +
            "\nrosybrown" +
            "\nlightblue" +
            "\nindianred" +
            "\nsilver" +
            "\ngray)" +
            "\n\n(repeat 2" +
            "\n(repeat 4" +
            "\nA_A_A_A_B_B_B_B" +
            "\n_A_A_A_A_B_B_B_)" +
            "\nA_A_A_A_B_B_B_B" +
            "\n_C_C_C_A_D_D_D_" +
            "\nB_B_B_B_A_A_A_A" +
            "\n_B_B_B_A_A_A_A_" +
            "\nB_B_B_A_A_A_A_A" +
            "\n_D_D_C_C_C_C_C_" +
            "\nA_A_A_B_B_B_B_B" +
            "\n_A_A_A_B_B_B_B_)"
    | _ -> // "icecream"
        myCode.value <-
            "(name ICECREAM)" +
            "\n\n(strings 15" +
            "\ndarkslategray" +
            "\nperu" +
            "\ndarkslategray" +
            "\nperu" +
            "\ndarkslategray" +
            "\nperu" +
            "\ndarkslategray" +
            "\nsienna" +
            "\ndarkslategray" +
            "\nlightpink" +
            "\ndarkslategray" +
            "\nlightpink" +
            "\ndarkslategray" +
            "\ncrimson" +
            "\ndarkslategray)" +
            "\n\n(repeat 2" +
            "\nC_C_C_C_C_C_C_" +
            "\n_D_D_C_C_D_D_D" +
            "\nC_C_D_D_D_D_C_" +
            "\n_D_C_C_C_C_C_D" +
            "\nC_D_D_D_D_D_D_" +
            "\n_D_C_C_C_C_C_D" +
            "\nC_C_D_D_D_D_C_" +
            "\n_D_D_C_C_D_D_D" +
            "\nC_C_C_C_C_C_C_" +
            "\n_D_D_D_D_D_D_D)"