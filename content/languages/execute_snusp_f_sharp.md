+++
title = "Execute SNUSP/F Sharp"
description = ""
date = 2010-02-06T14:34:07Z
aliases = []
[extra]
id = 5253
[taxonomies]
categories = []
tags = []
+++


This is the [F#](https://rosettacode.org/wiki/F_Sharp) implementation of the "modular" version.  Perhaps I'll get around to bloated later.  Allows infinite size data space to the left and right of the original data pointer.  I originally mistook the meaning of '<code>,</code>' and had the user input an arbitrary number which I would place on the tape, but after looking at the sample multiplication program, realized that I was supposed to input the ascii value of the single key entered.  Still, it seems like a good command to allow for reading an arbitrary value so I arbitrarily allocated '<code>~</code>' as the command for that purpose.

Bloated version below this modular version.  Somehow this page has been set up so I can't seem to change the original comments to note this fact.  Probably I'm just too dumb to know now.
<br clear=all>
## Modular SNUSP
```fsharp
open System
open System.Collections.Generic

type IP (p:(int*int), d:(int*int), dim1, dim2) =
    let mutable _p = p
    let mutable _d = d
    let mutable _fValid = true
    member this.dim1 = dim1
    member this.dim2 = dim2
    member this.x = fst this.pos
    member this.y = snd this.pos
    member this.dx = fst this.dir
    member this.dy = snd this.dir
    member this.pos with get() = _p
                    and set newp = _p <- newp
    member this.dir with get() = _d
                    and set newd = _d <- newd
    member this.Clone() = new IP((this.x, this.y), (this.dx, this.dy), dim1, dim2)
    member this.Invalidate() = _fValid <- false
    member this.SetTo(ip : IP) = this.pos <- ip.pos; this.dir <- ip.dir
    member this.Advance() =
        this.pos <- ((fst this.pos) + (fst this.dir), (snd this.pos) + (snd this.dir))
    member this.Valid() = 
        _fValid && this.x >= 0 && this.x < dim2 && this.y >= 0 && this.y < dim2
    member this.Reflect(c) =
        match c with
        | '/' -> this.dir <- (-this.dy, -this.dx)
        | '\\' -> this.dir <- (this.dy, this.dx)
        | _ -> ignore()
 
let RCSNUSP (pgmStr : string) =
    let StringToPgm (str : string) =
        let stringsPre =
            str.Trim([|'\n'; '\r'|]).Split([|'\n'|])
            |> Seq.map (fun s -> s.Trim([|'\n'; '\r'|]))
        let maxLen =
            stringsPre
            |> Seq.map (fun s -> s.Length)
            |> Seq.max
        let strings =
            stringsPre
            |> Seq.map (fun s -> s.PadRight(maxLen))
        strings
        |> Seq.map (fun s -> s.Trim([|'\n'; '\r'|]).ToCharArray())
        |> array2D

    let pgm = StringToPgm pgmStr
    let ptr = ref 0                                                     // Pointer into input
    let stk = new Stack<IP>()                                           // Instruction stack
    let input = ref (Array.create 100 0)

    let LocateStart pgm =
        let fFound = ref false
                   
        let s1 =
            seq {
                for i = 0 to ((Array2D.length1 pgm)-1) do
                    for j = 0 to ((Array2D.length2 pgm)-1) do
                        yield (pgm.[i,j], (j, i)) }
            |> Seq.skipWhile (fun (c, _) -> fFound := !fFound || (c = '$'); not !fFound)
            |> Seq.truncate 1
            |> Seq.toArray

        if not !fFound then
            (0,0)
        else
            s1
            |> Seq.head
            |> snd
    let ip = new IP(LocateStart pgm, (1, 0), Array2D.length1 pgm, Array2D.length2 pgm)                                // Instruction Pointer

    let InputNumber() =
        let mutable fValid = false
        let mutable num = 0
        printfn "Enter a valid number"
        fValid <- Int32.TryParse(Console.ReadLine(), &num);
        while not fValid do
            printfn "Invalid input.  Please enter a valid number"
            fValid <- Int32.TryParse(Console.ReadLine(), &num);
        num

    let InputAscii() =
        printfn "Enter an ascii character"
        let chOut = (Console.ReadKey().KeyChar)
        printfn ""
        int chOut

    let MovePtr fRight =
        if fRight then
            ptr := !ptr + 1
            if !ptr >= (!input).Length then
                Array.Resize(input, (!input).Length + 20)
        else
            ptr := !ptr - 1
            if !ptr < 0 then
                let newInput = Array.create ((!input).Length + 20) 0
                Array.ConstrainedCopy(!input, 0, newInput, 20, (!input).Length)
                ptr := 19
                
    let interpretCmd() =
        //if @"><+-.,;/\?!@#".Contains(pgm.[ip.y, ip.x].ToString()) then
        //    printfn "TapePos: %d; TapeVal: %d; Pos (%d, %d) : %c" !ptr (!input).[!ptr] ip.x ip.y pgm.[ip.y, ip.x]
        //    Console.ReadKey() |> ignore
        match pgm.[ip.y, ip.x] with
        | '>' -> MovePtr true; ip.Advance()
        | '<' -> MovePtr false; ip.Advance()
        | '+' -> (!input).[!ptr] <- (!input).[!ptr] + 1; ip.Advance()
        | '-' -> (!input).[!ptr] <- (!input).[!ptr] - 1; ip.Advance()
        | '.' -> printf "%c" (char (!input).[!ptr]); ip.Advance()
        | ',' -> (!input).[!ptr] <- InputAscii(); ip.Advance()
        | '~' -> (!input).[!ptr] <- InputNumber(); ip.Advance()
        | '/' | '\\' -> ip.Reflect(pgm.[ip.y, ip.x]); ip.Advance()
        | '?' -> ip.Advance(); if (!input).[!ptr] = 0 then ip.Advance()
        | '!' -> ip.Advance(); ip.Advance()
        | '@' -> stk.Push(ip.Clone()); ip.Advance()
        | '#' -> if stk.Count = 0 then
                    ip.Invalidate() 
                 else
                    ip.SetTo(stk.Pop())
                    ip.Advance()
                    ip.Advance()
        | _ -> ip.Advance()

    while ip.Valid() do
        interpretCmd() |> ignore
    (!input).[!ptr]
```

## Bloated SNUSP
Okay, I did the bloated version and made a few other changes.  Using <code>*</code> rather than <code>~</code> for inputting a number because it's easy for <code>~</code> to get lost in the visual sea of characters that makes up the typical program.  Also made separate classes for the engine and the IP.  I allow for infinite data space in both directions and threads not waiting for input run while other threads are blocked on input. I tried this on all the programs I could find.  There don't appear to be any which really utilize the 2D data space, but I tested it with some simple test programs and it seems okay.

```fsharp
// Bloated RCSNUSP
open System
open System.Collections.Generic

type IP (p:(int*int), d:(int*int), dim1, dim2) =
    let mutable _p = p                                          // Current instruction position
    let mutable _d = d                                          // Current direction we're headed
    let mutable _fValid = true                                  // Override telling us we're invalid
    let mutable _dataptr = (0, 0)                               // Data Pointer
    let _callStack = new Stack<IP>()                            // Call stack
    let _dim1 = dim1                                            // Dimensions of our instruction space
    let _dim2 = dim2
    // Some properties for the above
    member t.pos     with get() = _p
                        and set newp = _p <- newp
    member t.dir     with get() = _d
                        and set newd = _d <- newd
    member t.dataptr   with get() = _dataptr
                          and set newp = _dataptr <- newp
    member t.x = fst t.pos                                      // X value of our instruction pointer
    member t.y = snd t.pos                                      // Y value of our instruction pointer
    member t.dx = fst t.dir                                     // X value of our direction
    member t.dy = snd t.dir                                     // Y value of our direction
    member private t.Clone() = new IP((t.x, t.y), (t.dx, t.dy), _dim1, _dim2)
                                                                // Used for call/return where we don't need the data pointer
    member t.Split() =
        let newThread = t.Clone()                               // New thread starts with same position/direction
        newThread.dataptr <- t.dataptr                      // but also inherits the same data position
        newThread

    member t.Invalidate() = _fValid <- false
    member t.SetTo(ip : IP) = t.pos <- ip.pos; t.dir <- ip.dir
    member t.Advance() = t.pos <- ((fst t.pos) + (fst t.dir), (snd t.pos) + (snd t.dir))
                                                                // All important Advance moves the IP in the current direction
    member t.InRange() =
        t.x >= 0 && t.x < _dim2 && t.y >= 0 && t.y < _dim1      // See if the IP is still in instruction space

    member t.Valid() = _fValid && t.InRange()                   // See if we're valid
    member t.Reflect(c) =
        match c with                                            // See which character we're dealing with
        | '/' -> t.dir <- (-t.dy, -t.dx)                        // and do the
        | '\\' -> t.dir <- (t.dy, t.dx)                         // right thing
        | _ -> ignore()
    member t.Return() =
        if _callStack.Count = 0 then                            // If there's nothing on the call stack to return to
            t.Invalidate()                                      // kill ourselves
        else
            t.SetTo(_callStack.Pop())                           // Otherwise, retrieve the old position and direction
            t.Advance()                                         // and advance twice to continue
            t.Advance()
    member t.Call() =
        _callStack.Push(t.Clone())                              // Push our return point onto the stack

type SNUSP (pgmStr : string) =
    let _pgm =
        let stringsPre =
            pgmStr.Trim([|'\n'; '\r'|]).Split([|'\n'|])         // Trim off any initial CR/LFs and split at internal CRs
            |> Seq.map (fun s -> s.Trim([|'\n'; '\r'|]))        // We may or may not still have left over 'n' and 'r's hanging around
        let maxLen =
            stringsPre                                          // Array of stripped program lines
            |> Seq.map (fun s -> s.Length)                      // Find their length
            |> Seq.max                                          // and take the max
        let strings =
            stringsPre                                          // Array of stripped program lines
            |> Seq.map (fun s -> s.PadRight(maxLen))            // Pad them all the the max length
        strings
        |> Seq.map (fun s -> s.ToCharArray())                   // Turn all the lines to char arrays
        |> array2D                                              // Turn the whole thing into a 2D array

    let LocateStart pgm =
        let fFound = ref false
        let s1 =
            // It seems like this should just be an imperative nested for, but since F# doesn't have a break statement, once you
            // start into a for you have to go all the way through it.  The sequence below will stop as soon as it finds the '$'.
            seq {
                for i = 0 to ((Array2D.length1 pgm)-1) do
                    for j = 0 to ((Array2D.length2 pgm)-1) do
                        yield (pgm.[i,j], (j, i)) }             // Sequence of (value, position) pairs
            |> Seq.skipWhile (fun (c, _) -> fFound := !fFound || (c = '$'); not !fFound)
                                                                // Keep track of whether we found a '$" and only keep the good stuff
            |> Seq.truncate 1                                   // Stop as soon as we've found the '$'
            |> Seq.toArray                                      // Required to keep lazy evaluations from not actually finding anything at all

        if not !fFound then                                     // No '$' means start at (0,0)
            (0,0)
        else
            s1
            |> Seq.head                                         // else, the only value in the sequence is our '$'
            |> snd                                              // and the second value in it is it's position

    let _ips =
        let ret = new List<IP>()                                // list of currently active IPs
        ret.Add(new IP(LocateStart _pgm, (1, 0), Array2D.length1 _pgm, Array2D.length2 _pgm))
                                                                // Add our initial IP into it
        ret

    let _data2d = ref (Array2D.create 10 10 0)                  // 2D data space
    let mutable _fWaitingForKey = false                         // So we can handle ',' operator nicely
    let _lstSplitIps = new List<IP>()                           // List of IPs to be deleted after the current tick
    let _rnd = new Random()
    let mutable _builtString = ""

    member private t.ips = ref _ips
    member private t.ipLastRemoved = ref (new IP((0,0), (0,0), 0, 0))
                                                                // Last IP removed (for return value)
    
    member t.ReturnValue() = t.GetData (!t.ipLastRemoved)       // Return value

    member private t.InputNumber() =
        let mutable out = None                                  // No valid values yet
        if not _fWaitingForKey then                             // If necessary
            _fWaitingForKey <- true
            printf "Enter a valid number: "                     // Ask the user for a number
        if Console.KeyAvailable then                            // If the user has pressed a key
            let chNext = Console.ReadKey().KeyChar              // get the key
            if chNext = '\r' then                               // If they pressed enter
                printfn ""
                let mutable num = 0                             // set up a number to receive the final value
                let fValid = Int32.TryParse(_builtString, &num) // see if they entered a valid value
                if fValid then                                  // If they did, then
                    out <- Some(num)                            // Set our output to it
                    _fWaitingForKey <- false                    // And stop checking for keys
                else                                            // else
                    printf "Invalid input.  Please enter a valid number: "
                                                                // prompt for a valid value
                    _builtString <- ""                           // and reset the built string
            else
                _builtString <- _builtString + chNext.ToString() // if no enter, just append the value to the built up string
        out                                                     // return our output

    member private t.InputAscii() =
        if not _fWaitingForKey then                             // If we haven't put up the prompt yet
            _fWaitingForKey <- true                             // keep track of the fact that we have
            printf "Enter an ascii character: "                 // and put the prompt up
        if Console.KeyAvailable then                            // If there are keys available
            let chOut = Console.ReadKey().KeyChar               // Get the key
            _fWaitingForKey <- false                            // Allow the prompt to be put up
            printfn ""                                          // print a CR,
            Some(int chOut)                                     // return with the booty
        else
            None                                                // return nothing

    static member private OutputAscii(c) = printf "%c" c        // Print a single character

    member private t.ExpandData top left bottom right =
        let newInput = Array2D.create ((Array2D.length1 !_data2d) + top + bottom) (Array2D.length2 !_data2d + left + right) 0
                                                                // Create the new array with enough space
        for i in [0..((Array2D.length1 !_data2d) - 1)] do       // For each row
            for j in [0..((Array2D.length2 !_data2d) - 1)] do   // and column
                newInput.[i + top, j + left] <- (!_data2d).[i,j] // Copy old data to the new array
        _data2d := newInput                                     // Switch to the new array
        if top <> 0 || left <> 0 then                           // If necessary
            !t.ips |> Seq.iter (fun ip -> (ip.dataptr <- ((fst ip.dataptr) + left, ((snd ip.dataptr) + top))))
                                                                // Change data pointers in all the IPs

    member private t.MovePtr2d (ip:IP) (dx, dy) =
        ip.dataptr <- ((fst ip.dataptr) + dx, (snd ip.dataptr) + dy)
                                                                // Move the pointer
        match (dx, dy) with                                     // Check the result to see if we need to expand
        | (0,_) ->                                              // Check the vertical direction
            if (snd ip.dataptr) >= (Array2D.length1 !_data2d) then
                                                                // If we've overflowed the rows
                t.ExpandData 0 0 10 0                           // Expand on the bottom
            elif (snd ip.dataptr) < 0 then                    // If we've underflowed the rows
                t.ExpandData 10 0 0 0                           // Expand on the top
        | (_,0) ->                                              // Check the horizontal direction
            if (fst ip.dataptr) >= (Array2D.length2 !_data2d) then
                                                                // If we've overflowed the columns
                t.ExpandData 0 0 0 10                           // expand on the right
            elif (fst ip.dataptr) < 0 then                    // If we've underflowed columns
                t.ExpandData 0 10 0 0                           // expand on the left
        | _ -> raise <| new System.ArgumentException("Bad direction in MovePtr2d")
                                                                // This should never happen

    member private t.Split(ip:IP) =
        let newIp = ip.Split()                                  // Get an IP for the new thread
        newIp.Advance();                                        // advance it twice
        newIp.Advance();
        _lstSplitIps.Add(newIp)                                 // and add it to the list of splits

    member t.lstIp = !t.ips                                     // List of active IPs

    member private t.GetData (ip:IP) =
        (!_data2d).[snd ip.dataptr, fst ip.dataptr]             // Data being pointed at currently

    member private t.SetData (ip:IP) n =
        (!_data2d).[snd ip.dataptr, fst ip.dataptr] <- n        // Set the value at the current position

    member t.InterpretCmd(ip:IP) =                              // Interpret a single command for a single IP
        // Rudimentary debugging aid - interferes with any input and doesn't distinguish between threads
        //if @"><+-.,;:*/\?!@#".Contains(pgm.[ip.y, ip.x].ToString()) then
        //    printfn "TapePos: %A; TapeVal: %d; Pos (%d, %d) : %c" ip.dataptr (t.GetData ip) ip.x ip.y _pgm.[ip.y, ip.x]
        //    Console.ReadKey() |> ignore
        if ip.Valid() then
            match _pgm.[ip.y, ip.x] with
            | '>' -> t.MovePtr2d ip (1, 0); ip.Advance()
            | '<' -> t.MovePtr2d ip (-1, 0); ip.Advance()
            | ';' -> t.MovePtr2d ip (0, 1); ip.Advance()
            | ':' -> t.MovePtr2d ip (0, -1); ip.Advance()
            | '+' -> t.SetData ip ((t.GetData ip) + 1); ip.Advance()
            | '-' -> t.SetData ip ((t.GetData ip) - 1); ip.Advance()
            | '%' -> t.SetData ip (_rnd.Next(0, 256)); ip.Advance()
            | '.' -> SNUSP.OutputAscii(char (t.GetData ip)); ip.Advance()
            | ',' -> 
                let chOpt = t.InputAscii()                      // Check if there are any keys waiting
                if chOpt <> None then                           // If there are...
                    t.SetData ip (Option.get chOpt)             // set the current position to the ascii value of the key
                    ip.Advance()                                // and go on to the next instruction
            | '*' -> 
                let valOpt = t.InputNumber()                    // Do the same for numbers
                if valOpt <> None then
                    t.SetData ip (Option.get valOpt)
                    ip.Advance()
            | '/' | '\\' -> ip.Reflect(_pgm.[ip.y, ip.x]); ip.Advance()
            | '?' -> ip.Advance(); if (t.GetData ip) = 0 then ip.Advance()
            | '!' -> ip.Advance(); ip.Advance()
            | '&' -> t.Split(ip); ip.Advance()
            | '@' -> ip.Call(); ip.Advance()
            | '#' -> ip.Return()
            | _ -> ip.Advance()
            if not (ip.Valid()) then
                t.ipLastRemoved := ip

    member t.InterpretCmd() =                                   // Interpret a single command for each active IP
        _lstSplitIps.Clear()                                    // Clear the split array
        let ipRemovals =
            !t.ips                                              // Start with all ips
            |> Seq.filter (fun ip -> t.InterpretCmd(ip);        // Interpret the command
                                        not (ip.Valid()))       // Collect the IP if it went invalid
            |> Seq.toArray                                      // Force laziness to act
        ipRemovals                                              // For each invalid IP found
        |> Seq.iter (fun ip -> (!t.ips).Remove(ip) |> ignore)   // remove it from the list of active IPs
        _lstSplitIps                                            // For each new thread created
        |> Seq.iter (fun ip -> (!t.ips).Add(ip) |> ignore)      // Add it to the list of active IPs

    member t.Execute() =                                        // Execute til exit
        while (!t.ips).Count <> 0 do                            // While there are active threads
            t.InterpretCmd()                                    // Interpret commands

    static member Run(pgm) = 
        let snusp = new SNUSP(pgm)                              // Create an engine
        snusp.Execute()                                         // and run the program with it
        snusp.ReturnValue()                                     // Return the return value
```


## Example Inputs
```fsharp
let p1 = @"
 read two characters    ,>,==\  *    /
### =============
 ATOI   ----------\ 
 convert to integers /=/@</@=/  *   // /===== ITOA  ++++++++++\ /----------/ 
            multiply @ \=!\
### ===
/ //           /++++++++++/ \----------\ 
        convert back !/@!\
### ======
/            \++++++++++\ /----------/ 
and print the result \/  \.#    *                  /++++++++++/ \--------#
/
### ==============
/          *                  \++++++++#
|
|    /-<+>\                    #/?=<<<<\!>>>>\                   />>+<+<-\ 
|   #\?===/! BMOV1 =====\       \->>>>+/    //  /
### ==
 BSPL2 !\======?/#
|    /->+<\         /===|
### =====
 FMOV4 =/  //                /<<+>+>-\ 
|   #\?===/! FMOV1 =|===|
### ===========\  /====/  /===
 FSPL2 !\======?/#
|                /==|===|
### ========
|==|
### =
/
|           * * *|* | * | * * * * * * *|* | * * *                /+<-\ 
|           * />@/<@/>>@/>>
### \ /=
>>\@<\@<\  *   /==== ADD2  !\>=?/<#
\===== MUL2 =?/>@\==<#<<<==\  \!\<<<<@\>>>>-?/\ *  //            /-\ 
            *    \\        \/@
### ==
|======</ * //  /== ZERO  !\?/#
            * * * \\* * * * | * * * * | * * * * *//  //
                   \\       |         \
### ====
/  //
                    \======!\
### =================
/
"
// Should print out multiplication of two input ascii values
RCSNUSP p1 |> ignore
printfn ""
printfn "Next three should all yield 48"
printfn "value = %d" (RCSNUSP @"
    /\/\/\
$===++++++\
   /++++++/
   \++++++\
   /++++++/
   \/\/\/#
")
printfn "value = %d" (RCSNUSP @"
     #/\/\
$===!\++++\
     /++++/
   /=\++++\
   \!\/\/\/
")
printfn "value = %d" (RCSNUSP @"6=@@@+@+++++#")
```

