module App

open Browser.Dom
open Browser.Svg
open Fable.Core

open ProgramParser
open ProgramInterpreter

// Get references to the required HTML elements
let myButton = document.querySelector(".submit") :?> Browser.Types.HTMLButtonElement
let myCode = document.querySelector(".code") :?> Browser.Types.HTMLTextAreaElement

let left = document.querySelector(".left") :?> Browser.Types.HTMLDivElement
let right = document.querySelector(".right") :?> Browser.Types.HTMLDivElement

let parent = left.parentNode
parent.insertBefore(left, parent.firstChild)

// Register button click
myButton.onclick <- fun _ ->
    let input = myCode.value // get code

    match parse input with // try to parse expression
    | Some ast -> 
        let svg = eval ast
        right.innerHTML <- svg
        myCode.value <- "Enjoy your pattern. Feel free to write another program too!"
    | None -> 
        myCode.value <- "Invalid expression."