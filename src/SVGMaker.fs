(* This program produces SVG visualizations of friendship bracelet patterns. *)
module SVG
open System

// tab helpers
let tab1 = "\n\t"
let tab2 = "\n\t\t"
let tab3 = "\n\t\t\t"
let tab4 = "\n\t\t\t\t"
let tab5 = "\n\t\t\t\t\t"

// deal with hex codes
let hexHelper (color : string) = 
    let label = 
        match color.[0] with
        | '#' -> color.[1..] // hex code found
        | _ -> color
    label

// set up <body> part of html doc
let startBody name s w h strstyle =
    let numStrings = List.length s

    // compile necessary svg defs
    let svgdefs = 
        "<svg class=\"pattern\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 " + w + " " + h + "\">" +
        tab1 + "<defs>" +
        tab2 + "<marker id=\"arrowhead\" markerWidth=\"5\" markerHeight=\"5\" refX=\"0\" refY=\"2\" orient=\"auto\">" +
        tab3 + "<polygon points=\"0 0, 4 2, 0 4\" fill=\"white\" />" +
        tab2 + "</marker>" +
        tab2 + "<symbol id=\"rarrow\">" +
        tab3 + "<line x1=\"30\" y1=\"40\" x2=\"60\" y2=\"40\" stroke=\"white\" stroke-width=\"3\" marker-end=\"url(#arrowhead)\" />" +
        tab2 + "</symbol>" +
        tab2 + "<symbol id=\"knot\">" +
        tab3 + "<circle cx=\"50\" cy=\"50\" r=\"40\" />" +
        tab2 + "</symbol>" +
        tab2 + "<symbol id=\"twoarrows\">" +
        tab3 + "<g>" +
        tab4 + "<use xlink:href=\"#rarrow\" />" +
        tab4 + "<use xlink:href=\"#rarrow\" transform=\"translate(0 20)\" />" +
        tab3 + "</g>" +
        tab2 + "</symbol>" +
        tab2 + "<symbol id=\"fliparrows\">" +
        tab3 + "<g>" +
        tab4 + "<use xlink:href=\"#rarrow\" transform=\"scale(0.5,0.5)\" />" +
        tab4 + "<use xlink:href=\"#rarrow\" transform=\"translate(50 10) scale(-0.5,0.5)\" />" +
        tab3 + "</g>" +
        tab2 + "</symbol>" +
        tab2 + "<symbol id=\"rr\">" +
        tab3 + "<g>" +
        tab4 + "<use xlink:href=\"#knot\" fill=\"var(--color)\" transform=\"scale(0.5,0.5)\" />" +
        tab4 + "<use xlink:href=\"#twoarrows\" transform=\"scale(0.5, 0.5)\" />" +
        tab3 + "</g>" +
        tab2 + "</symbol>" +
        tab2 + "<symbol id=\"ll\">" +
        tab3 + "<use xlink:href=\"#rr\" transform=\"translate(50 0) scale(-1,1)\" />" +
        tab2 + "</symbol>" +
        tab2 + "<symbol id=\"rl\">" +
        tab3 + "<g>" +
        tab4 + "<use xlink:href=\"#knot\" fill=\"var(--color)\" transform=\"scale(0.5,0.5)\" />" +
        tab4 + "<use xlink:href=\"#fliparrows\" />" +
        tab3 + "</g>" +
        tab2 + "</symbol>" +
        tab2 + "<symbol id=\"lr\">" +
        tab3 + "<use xlink:href=\"#rl\" transform=\"translate(50 0) scale(-1,1)\" />" +
        tab2 + "</symbol>" +
        tab2 + "<symbol id=\"skip\" display=\"none\">" +
        tab3 + "<use xlink:href=\"#knot\" fill=\"none\" />" +
        tab2 + "</symbol>" +
        tab1 + "</defs>" +
        tab1 + "<style type=\"text/css\">" +
        tab2 + ".title {" +
        tab3 + "font: bold 32px 'Roboto';" +
        tab3 + "letter-spacing: 0.3em;" +
        tab3 + "text-transform: uppercase;" +
        tab2 + "}" +
        tab2 + ".row {" +
        tab3 + "font: bold 20px 'Roboto';" +
        tab2 + "}" +
        tab2 + ".num {" +
        tab3 + "font: 16px 'Roboto';" +
        tab2 + "}" +
        strstyle +
        tab1 + "</style>" +
        tab1 + "<text x=\"50%\" y=\"25\" class=\"title\" text-anchor=\"middle\">" + name + "</text>" +
        tab1 + "<g id=\"rowhead\" transform=\"translate(0 70)\">" +
        tab2 + "<text x=\"0\" y=\"20\" class=\"row\">ROW</text>"

    // build the header row (shows original order of strings in their respective colors)
    let rowHead i = 
        let color = List.item(i) s
        let label = hexHelper color
        let xoffset = 118 + i * 100
        let stringnum = i + 1
        tab2 + "<svg class=\"string" + label + "\">" +
        tab3 + "<text x=\"" + (xoffset |> string) + "\" y=\"20\" class=\"row\" fill=\"var(--color)\">" + (stringnum |> string) + "</text>" +
        tab2 + "</svg>"

    // add header row
    let rec listMaker acc i = 
        if i = numStrings then
            acc
        else
            listMaker (List.append acc [i]) (i + 1)
     
    let headList = listMaker [] 0

    let allText = 
        (List.fold(fun acc elem -> acc + (rowHead elem)) svgdefs headList) + 
        tab1 + "</g>"
        
    allText

// add strings to SVG
let addStrings s rows =
    let numStrings = List.length s
    let width = (100 * (numStrings + 1))
    let height = (100 * (rows + 1) + 50)

    // create 1 string style element
    let addString color =
        let label = hexHelper color
        let styleElem = 
            tab2 + ".string" + label + " {" +
            tab3 + "--color: " + color + ";" +
            tab2 + "}"
        styleElem

    let uniqueColors = List.distinct s // list that stores only keeps 1 copy of each color used in the pattern
    let allStyle = (List.fold (fun acc elem -> acc + (addString elem)) "" uniqueColors)

    (width |> string, height |> string, allStyle)

// draw paths of strings
let drawPaths pos strings rows =
    let startText = tab1 + "<g id=\"paths\">"
    let finaly = (210 + 100 * (rows - 1)) |> string

    let nextPoint (p : string) = // extend path by processing next knot
        match p.Length with
        | 1 -> // reached last knot
            ""
        | _ -> // keep going
            let coords = p.Split([|","|], StringSplitOptions.RemoveEmptyEntries) // get coordinates of knot
            let row = coords.[0] |> int
            let knot = coords.[1] |> int
            let x = (175 + 100 * knot) |> string
            let y = (155 + 100 * (row - 1)) |> string
            x + " " + y + " " // done with this knot!

    let findXEndPos (knots : string list) = // find final x pos
        let lastknot = List.item(knots.Length - 2) knots // find last knot position
        let dir = List.item(knots.Length - 1) knots // find direction string is pointing after last knot
        let x = ((lastknot.Split([|","|], StringSplitOptions.RemoveEmptyEntries)).[1]) |> int
        let xpos = 
            match dir with 
            | "l" -> // ends on left side of knot
                (125 + 100 * x) |> string
            | _ -> // ends on right side of knot
                (225 + 100 * x) |> string
        xpos

    let onePath (s : string) i = // process path for one string
        let color = List.item(i) strings
        let label = hexHelper color
        let startx = (125 + 100 * i) |> string
        let path = 
            tab2 + "<svg class=\"string" + label + "\">" +
            tab3 + "<path d=\"M " + startx + " 100 "
        
        let knots = s.Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |> Seq.toList // make list of knots to connect
        let allPoints = List.fold (fun acc elem -> acc + (nextPoint elem)) path knots

        let finalx = findXEndPos knots 
        let finishedPath = 
            allPoints + 
            finalx + " " + finaly + "\"" +
            tab3 + " stroke=\"var(--color)\" stroke-width=\"2\" fill=\"none\" />" +
            tab2 + "</svg>"
        finishedPath

    let rec pathHelper todo soFar i =
        if (List.isEmpty todo) then // finished processing all the paths
            soFar
        else // still some work to do
            let nextPart = onePath (List.head todo) i
            pathHelper (List.tail todo) (soFar + nextPart) (i + 1)

    let pathText = pathHelper pos startText 0 // start processing the paths

    let allText = 
        pathText +
        tab1 + "</g>"

    allText

// add each row to the svg
let addRows strings (res: string) =
    let startRow = tab1 + "<g id=\"rows\" transform=\"translate(0 130)\">"
    let numStrings = List.length strings
    
    let addKnot s i = // add a knot
        let xOffset = (100 * (i - 1)) |> string
        match s with
        | ">>" -> 
            let text = 
                tab5 + "<use xlink:href=\"#rr\" x=\"" + xOffset + "\" />" +
                tab4 + "</svg>"
            (i+1, text)
        | "<<" -> 
            let text =
                tab5 + "<use xlink:href=\"#ll\" x=\"" + xOffset + "\" />" +
                tab4 + "</svg>"
            (i+1, text)
        | ">" -> 
            let text = 
                tab5 + "<use xlink:href=\"#rl\" x=\"" + xOffset + "\" />" +
                tab4 + "</svg>"
            (i+1, text)
        | "<" -> 
            let text =
                tab5 + "<use xlink:href=\"#lr\" x=\"" + xOffset + "\" />" +
                tab4 + "</svg>"
            (i+1, text)
        | "_" -> 
            let text =
                tab4 + "<svg>" +
                tab5 + "<use xlink:href=\"#skip\" x=\"" + xOffset + "\" />" +
                tab4 + "</svg>"
            (i+1, text)
        | _ -> // color
            let text = tab4 + "<svg class=\"string" + (hexHelper s) + "\">"
            (i, text)

    let oneRow (row: string) = // add each row
        let split = row.Split([|" "|], StringSplitOptions.RemoveEmptyEntries) |> Seq.toList // get each individual item
        let rnum = List.head split // get row num
        let v = rnum |> int
        let yOffset = 100 * (v - 1)
        // start row
        let text = 
            tab2 + "<g id=\"row" + rnum + "\" class=\"num\" transform=\"translate(0 " + (yOffset |> string) + ")\">" +
            tab3 + "<text x=\"0\" y=\"30\">" + rnum + "</text>" +
            tab3 + "<g transform=\"translate(150 0)\">"
        let knots = List.tail split // rest of items should be knots

        let rec knotHelper itemInd knotInd =
            let s = List.item(itemInd) knots
            let res = addKnot s knotInd
            match res with
            | (n, str) ->
                if n = numStrings then // done with row
                    str
                else // keep going
                    str + (knotHelper (itemInd + 1) n)

        let addOn = knotHelper 0 1

        let endText =
            tab3 + "</g>" +
            tab2 + "</g>"
        
        text + addOn + endText
    
    // extract knots from string
    let arr = res.Split([|"Row\n"|], StringSplitOptions.RemoveEmptyEntries)
    let rows = arr.[1] // should be in second part of split string
    let rlist = rows.Split([|"\n"|], StringSplitOptions.RemoveEmptyEntries) |> Seq.toList // extract rows with "/n"

    let endRow = 
        (List.fold (fun acc elem -> acc + (oneRow elem)) startRow rlist) +
        tab1 + "</g>" +
        "\n</svg>"

    endRow

// generate SVG string
let makeSVG name strings result rows pos =
    let onlyColors = // remove number at end of color names in list of strings
        List.map (fun (x:string) -> 
            (x.Split([|" "|], StringSplitOptions.RemoveEmptyEntries)).[0]) strings 

    let (w, h, sofar) = addStrings onlyColors rows
    let body = (startBody name onlyColors w h sofar)
    let paths = body + (drawPaths pos onlyColors rows)
    let fullstring = paths + (addRows onlyColors result)
    fullstring