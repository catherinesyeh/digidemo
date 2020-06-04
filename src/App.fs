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