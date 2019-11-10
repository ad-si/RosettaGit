+++
title = "Execute Brainfuck/F Sharp"
description = ""
date = 2010-02-06T14:20:06Z
aliases = []
[extra]
id = 5249
[taxonomies]
categories = []
tags = []
+++

{{implementation|Brainfuck}}{{collection|RCBF}}
An implementation of [[Brainfuck]] in [[F Sharp|F#]].

<br clear=all/>
<div style='width: full; overflow: scroll'>
```fsharp
open System

let RCBF (pgmStr : string) =
    let pgm = Seq.toArray pgmStr                                        // Turn program into an array
    let ptr = ref 0                                                     // Pointer into input
    let ip = ref 0                                                      // Instruction Pointer
    let input = ref (Array.create 100 0)

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

    let AdvanceIp() = ip := !ip + 1                                     // Advance IP
    let RetreatIp() = ip := !ip - 1                                     // Decrement IP

    let SkipBrackets fForward =
        if (!input).[!ptr] <> 0 then                                       // If we have a 0 input
            AdvanceIp()                                                 // Then just advance to next instruction
        else                                                            // otherwise
            let fnMove = if fForward then AdvanceIp else RetreatIp      // get the appropriate function to move forward or backward
            let mutable cBrackets = 1                                   // Count of brackets we've seen
            while cBrackets > 0 do                                      // While we have unmatched brackets
                fnMove()                                                // move to the next character
                if !ip >= 0 && !ip < pgm.Length then                    // and if we're still within range
                    match pgm.[!ip] with                                // look at the character
                    | '[' -> cBrackets <- cBrackets + 1                 // If it's a '[' then count up brackets
                    | ']' -> cBrackets <- cBrackets - 1                 // If it's a ']' then count down brackets
                    | _ -> ignore(0)                                    // Ignore anything else
            AdvanceIp()                                                 // When we're on the matching bracket, skip to the next instruction

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
        match pgm.[!ip] with
        | '>' -> MovePtr true; AdvanceIp()
        | '<' -> MovePtr false; AdvanceIp()
        | '+' -> (!input).[!ptr] <- (!input).[!ptr] + 1; AdvanceIp()
        | '-' -> (!input).[!ptr] <- (!input).[!ptr] - 1; AdvanceIp()
        | '.' -> printf "%c" (char (!input).[!ptr]); AdvanceIp()
        | ',' -> (!input).[!ptr] <- InputNumber(); AdvanceIp()
        | '~' -> (!input).[!ptr] <- InputAscii(); AdvanceIp()
        | '[' -> SkipBrackets true
        | ']' -> SkipBrackets false
        | _ -> AdvanceIp()

    while !ip >= 0 && !ip < pgm.Length do
        interpretCmd()
```
</div>

Hello world in RCBF (stolen from the Wiki site):
<div style='width: full; overflow: scroll'>
```fsharp
let pgmHelloWorld = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>."
let tape = Array.create 5 0
Rcbf pgmHelloWorld tape
```
</div>
