+++
title = "4-rings or 4-squares puzzle"
description = ""
date = "2019-08-24T00:22:36Z"
aliases = []

[extra]
id = 21262
task = """
Replace `a`, `b`, `c`, `d`, `e`, `f` and `g` with the decimal digits
LOW ─► HIGH such that the sum of the letters
inside of each of the four large squares add up to the same sum.

```txt
╔═══════╗   ╔═══════════╗
║ a     ║   ║     e     ║
║   ┌───╫───╫───┐   ┌───╫───┐
║   │ b ║   ║ d │   │ f ║   │
╚═══╪═══╝   ╚═══╪═══╪═══╝   │
    │     c     │   │     g │
    └───────────┘   └───────┘
```

- Show all solutions for each letter being unique with LOW=1 HIGH=7
- Show all solutions for each letter being unique with LOW=3 HIGH=9
- Show only the  number of solutions when each letter can be non-unique
    LOW=0 HIGH=9
"""

[taxonomies]
categories = ["games", "puzzles"]
tags = ["game", "puzzle"]
languages = [
  "algol_68",
  "applescript",
  "arm_assembly",
  "asm",
  "awk",
  "befunge",
  "c",
  "clojure",
  "cpp",
  "d",
  "fsharp",
  "factor",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "lisp",
  "lua",
  "modula-2",
  "nim",
  "pascal",
  "perl_6",
  "perl",
  "phix",
  "picat",
  "prolog",
  "python",
  "rexx",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "simula",
  "sql_pl",
  "stata",
  "tcl",
  "visual_basic_.net",
  "visual_basic",
  "yabasic",
  "zkl",
]
+++

Related task:
- [Solve the no connection puzzle](/)

---


## ALGOL 68

As with the REXX solution, we use explicit loops to generate the permutations.

```algol68
BEGIN
    # solve the 4 rings or 4 squares puzzle                                             #
    # we need to find solutions to the equations: a + b = b + c + d = d + e + f = f + g #
    # where a, b, c, d, e, f, g in lo : hi ( not necessarily unique )                   #
    # depending on show, the solutions will be printed or not                           #
    PROC four rings = ( INT lo, hi, BOOL unique, show )VOID:
    BEGIN
        INT  solutions := 0;
        BOOL allow duplicates = NOT unique;
        # calculate field width for printinhg solutions #
        INT  width := -1;
        INT  max := ABS IF ABS lo > ABS hi THEN lo ELSE hi FI;
        WHILE max > 0 DO
            width -:= 1;
            max OVERAB 10
        OD;
        # find solutions #
        FOR a FROM lo TO hi DO
            FOR b FROM lo TO hi DO
                IF allow duplicates OR a /= b THEN
                    INT t = a + b;
                    FOR c FROM lo TO hi DO
                        IF allow duplicates OR ( a /= c AND b /= c ) THEN
                            FOR d FROM lo TO hi DO
                                IF allow duplicates OR ( a /= d AND b /= d AND c /= d )
                                THEN
                                    IF b + c + d = t THEN
                                        FOR e FROM lo TO hi DO
                                            IF allow duplicates
                                            OR ( a /= e AND b /= e AND c /= e AND d /= e )
                                            THEN
                                                FOR f FROM lo TO hi DO
                                                    IF allow duplicates
                                                    OR ( a /= f AND b /= f AND c /= f AND d /= f AND e /= f )
                                                    THEN
                                                        IF d + e + f = t THEN
                                                            FOR g FROM lo TO hi DO
                                                                IF allow duplicates
                                                                OR ( a /= g AND b /= g AND c /= g AND d /= g AND e /= g AND f /= g )
                                                                THEN
                                                                    IF f + g = t THEN
                                                                        solutions +:= 1;
                                                                        IF show THEN
                                                                            print( ( whole( a, width ), whole( b, width )
                                                                                   , whole( c, width ), whole( d, width )
                                                                                   , whole( e, width ), whole( f, width )
                                                                                   , whole( g, width ), newline
                                                                                   )
                                                                                 )
                                                                        FI
                                                                    FI
                                                                FI
                                                            OD # g #
                                                        FI
                                                    FI
                                                OD # f #
                                            FI
                                        OD # e #
                                    FI
                                FI
                            OD # d #
                        FI
                    OD # c #
                FI
            OD # b #
        OD # a # ;
        print( ( whole( solutions, 0 )
               , IF unique THEN " unique" ELSE " non-unique" FI
               , " solutions in "
               , whole( lo, 0 )
               , " to "
               , whole( hi, 0 )
               , newline
               , newline
               )
             )
    END # four rings # ;

    # find the solutions as required for the task #
    four rings( 1, 7, TRUE,  TRUE  );
    four rings( 3, 9, TRUE,  TRUE  );
    four rings( 0, 9, FALSE, FALSE )
END
```

Output:

```txt
 3 7 2 1 5 4 6
 4 5 3 1 6 2 7
 4 7 1 3 2 6 5
 5 6 2 3 1 7 4
 6 4 1 5 2 3 7
 6 4 5 1 2 7 3
 7 2 6 1 3 5 4
 7 3 2 5 1 4 6
8 unique solutions in 1 to 7

 7 8 3 4 5 6 9
 8 7 3 5 4 6 9
 9 6 4 5 3 7 8
 9 6 5 4 3 8 7
4 unique solutions in 3 to 9

2860 non-unique solutions in 0 to 9
```



## AppleScript

Translated from JavaScript
Translated from Haskell (Structured search example)

```applescript
use framework "Foundation" -- for basic NSArray sort

on run
    unlines({"rings(true, enumFromTo(1, 7))\n", ¬
        map(show, (rings(true, enumFromTo(1, 7)))), ¬
        "\nrings(true, enumFromTo(3, 9))\n", ¬
        map(show, (rings(true, enumFromTo(3, 9)))), ¬
        "\nlength(rings(false, enumFromTo(0, 9)))\n", ¬
        show(|length|(rings(false, enumFromTo(0, 9))))})
end run

-- RINGS -----------------------------------------------------------------------

-- rings :: noRepeatedDigits -> DigitList -> Lists of solutions
-- rings :: Bool -> [Int] -> [[Int]]
on rings(u, digits)
    set ds to reverse_(sort(digits))
    set h to head(ds)

    -- QUEEN -------------------------------------------------------------------
    script queen
        on |λ|(q)
            script
                on |λ|(x)
                    x + q ≤ h
                end |λ|
            end script
            set ts to filter(result, ds)
            if u then
                set bs to delete_(q, ts)
            else
                set bs to ds
            end if

            -- LEFT BISHOP and its ROOK-----------------------------------------
            script leftBishop
                on |λ|(lb)
                    set lRook to lb + q
                    if lRook > h then
                        {}
                    else
                        if u then
                            set rbs to difference(ts, {q, lb, lRook})
                        else
                            set rbs to ds
                        end if

                        -- RIGHT BISHOP and its ROOK ---------------------------
                        script rightBishop
                            on |λ|(rb)
                                set rRook to rb + q
                                if (rRook > h) or (u and (rRook = lb)) then
                                    {}
                                else
                                    set rookDelta to lRook - rRook
                                    if u then
                                        set ks to difference(ds, ¬
                                            {q, lb, rb, rRook, lRook})
                                    else
                                        set ks to ds
                                    end if

                                    -- KNIGHTS LEFT AND RIGHT ------------------
                                    script knights
                                        on |λ|(k)
                                            set k2 to k + rookDelta

                                            if elem(k2, ks) and ((not u) or ¬
                                                notElem(k2, ¬
                                                    {lRook, k, lb, q, rb, rRook})) then
                                                {{lRook, k, lb, q, rb, k2, rRook}}
                                            else
                                                {}
                                            end if
                                        end |λ|
                                    end script

                                    concatMap(knights, ks)
                                end if
                            end |λ|
                        end script

                        concatMap(rightBishop, rbs)
                    end if
                end |λ|
            end script

            concatMap(leftBishop, bs)
        end |λ|
    end script

    concatMap(queen, ds)
end rings

-- GENERIC FUNCTIONS -----------------------------------------------------------

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lst to {}
    set lng to length of xs
    tell mReturn(f)
        repeat with i from 1 to lng
            set lst to (lst & |λ|(contents of item i of xs, i, xs))
        end repeat
    end tell
    return lst
end concatMap

-- delete :: Eq a => a -> [a] -> [a]
on delete_(x, xs)
    set mbIndex to elemIndex(x, xs)
    set lng to length of xs

    if mbIndex is not missing value then
        if lng > 1 then
            if mbIndex = 1 then
                items 2 thru -1 of xs
            else if mbIndex = lng then
                items 1 thru -2 of xs
            else
                tell xs to items 1 thru (mbIndex - 1) & ¬
                    items (mbIndex + 1) thru -1
            end if
        else
            {}
        end if
    else
        xs
    end if
end delete_

-- difference :: [a] -> [a] -> [a]
on difference(xs, ys)
    script mf
        on except(a, y)
            if a contains y then
                my delete_(y, a)
            else
                a
            end if
        end except
    end script

    foldl(except of mf, xs, ys)
end difference

-- elem :: Eq a => a -> [a] -> Bool
on elem(x, xs)
    xs contains x
end elem

-- elemIndex :: a -> [a] -> Maybe Int
on elemIndex(x, xs)
    set lng to length of xs
    repeat with i from 1 to lng
        if x = (item i of xs) then return i
    end repeat
    return missing value
end elemIndex

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if n < m then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- head :: [a] -> a
on head(xs)
    if length of xs > 0 then
        item 1 of xs
    else
        missing value
    end if
end head

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- length :: [a] -> Int
on |length|(xs)
    length of xs
end |length|

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- notElem :: Eq a => a -> [a] -> Bool
on notElem(x, xs)
    xs does not contain x
end notElem

-- reverse_ :: [a] -> [a]
on |reverse|:xs
    if class of xs is text then
        (reverse of characters of xs) as text
    else
        reverse of xs
    end if
end |reverse|:

-- show :: a -> String
on show(e)
    set c to class of e
    if c = list then
        script serialized
            on |λ|(v)
                show(v)
            end |λ|
        end script

        "[" & intercalate(", ", map(serialized, e)) & "]"
    else if c = record then
        script showField
            on |λ|(kv)
                set {k, ev} to kv
                "\"" & k & "\":" & show(ev)
            end |λ|
        end script

        "{" & intercalate(", ", ¬
            map(showField, zip(allKeys(e), allValues(e)))) & "}"
    else if c = date then
        "\"" & iso8601Z(e) & "\""
    else if c = text then
        "\"" & e & "\""
    else if (c = integer or c = real) then
        e as text
    else if c = class then
        "null"
    else
        try
            e as text
        on error
            ("«" & c as text) & "»"
        end try
    end if
end show

-- sort :: [a] -> [a]
on sort(xs)
    ((current application's NSArray's arrayWithArray:xs)'s ¬
        sortedArrayUsingSelector:"compare:") as list
end sort

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines
```

Output:

```txt
rings(true, enumFromTo(1, 7))

[7, 3, 2, 5, 1, 4, 6]
[6, 4, 1, 5, 2, 3, 7]
[5, 6, 2, 3, 1, 7, 4]
[4, 7, 1, 3, 2, 6, 5]
[7, 2, 6, 1, 3, 5, 4]
[6, 4, 5, 1, 2, 7, 3]
[4, 5, 3, 1, 6, 2, 7]
[3, 7, 2, 1, 5, 4, 6]

rings(true, enumFromTo(3, 9))

[9, 6, 4, 5, 3, 7, 8]
[8, 7, 3, 5, 4, 6, 9]
[9, 6, 5, 4, 3, 8, 7]
[7, 8, 3, 4, 5, 6, 9]

length(rings(false, enumFromTo(0, 9)))

2860
```



## ARM Assembly

Works with Raspberry Pi.

```ARM-Assembly
/* ARM assembly Raspberry PI  */
/*  program square4.s   */

/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

.equ NBBOX,  7

/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessDeb:           .ascii "a="
sMessValeur_a:     .fill 11, 1, ' '            @ size => 11
                    .ascii "b="
sMessValeur_b:     .fill 11, 1, ' '            @ size => 11
                    .ascii "c="
sMessValeur_c:     .fill 11, 1, ' '            @ size => 11
                    .ascii "d="
sMessValeur_d:     .fill 11, 1, ' '            @ size => 11
                    .ascii "\n"
                    .ascii "e="
sMessValeur_e:     .fill 11, 1, ' '            @ size => 11
                    .ascii "f="
sMessValeur_f:     .fill 11, 1, ' '            @ size => 11
                    .ascii "g="
sMessValeur_g:     .fill 11, 1, ' '            @ size => 11

szCarriageReturn:   .asciz "\n************************\n"

sMessNbSolution:   .ascii "Number of solutions :"
sMessCounter:     .fill 11, 1, ' '            @ size => 11
                   .asciz "\n\n\n"

/*********************************/
/* UnInitialized data            */
/*********************************/
.bss
.align 4
iValues_a:                .skip 4 * NBBOX
iValues_b:                .skip 4 * NBBOX - 1
iValues_c:                .skip 4 * NBBOX - 2
iValues_d:                .skip 4 * NBBOX - 3
iValues_e:                .skip 4 * NBBOX - 4
iValues_f:                .skip 4 * NBBOX - 5
iValues_g:                .skip 4 * NBBOX - 6
iCounterSol:              .skip 4
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main
main:                                             @ entry of program
    mov r0,#1
    mov r1,#7
    mov r2,#3                                     @ 0 = rien 1 = display 2 = count 3 = les deux
    bl searchPb
    mov r0,#3
    mov r1,#9
    mov r2,#3                                     @ 0 = rien 1 = display 2 = count 3 = les deux
    bl searchPb
    mov r0,#0
    mov r1,#9
    mov r2,#2                                     @ 0 = rien 1 = display 2 = count 3 = les deux
    bl prepSearchNU

100:                                              @ standard end of the program
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc #0                                        @ perform the system call

iAdrszCarriageReturn:            .int szCarriageReturn

/******************************************************************/
/*     search problèm  value not unique                           */
/******************************************************************/
/* r0 contains start digit */
/* r1 contains end digit */
/* r2 contains action (0 display 1 count) */
prepSearchNU:
    push {r3-r12,lr}                              @ save  registers
    mov r5,#0                                     @ counter
    mov r12,r0                                    @ a
1:
    mov r11,r0                                    @ b
2:
    mov r10,r0                                    @ c
3:
    mov r9,r0                                     @ d
4:
    add r4,r12,r11                                @ a + b reference
    add r3,r11,r10
    add r3,r9                                     @ b + c + d
    cmp r4,r3
    bne 10f
    mov r8,r0                                     @ e
5:
    mov r7,r0                                     @ f
6:
    add r3,r9,r8
    add r3,r7                                     @ d + e + f
    cmp r3,r4
    bne 9f
    mov r6,r0                                     @ g
7:
    add r3,r7,r6                                  @ f + g
    cmp r3,r4
    bne 8f                                        @ not OK
                                                  @ OK
    add r5,#1                                     @ increment counter

8:
    add r6,#1                                     @ increment g
    cmp r6,r1
    ble 7b
9:
    add r7,#1                                     @ increment f
    cmp r7,r1
    ble 6b
    add r8,#1                                     @ increment e
    cmp r8,r1
    ble 5b
10:
    add r9,#1                                     @ increment d
    cmp r9,r1
    ble 4b
    add r10,#1                                    @ increment c
    cmp r10,r1
    ble 3b
    add r11,#1                                    @ increment b
    cmp r11,r1
    ble 2b
    add r12,#1                                    @ increment a
    cmp r12,r1
    ble 1b

    @ end
    tst r2,#0b10                                    @ print count ?
    beq 100f
    mov r0,r5                                       @ counter
    ldr r1,iAdrsMessCounter
    bl conversion10
    ldr r0,iAdrsMessNbSolution
    bl affichageMess

100:
    pop {r3-r12,lr}                                 @ restaur registers
    bx lr                                           @return
iAdrsMessCounter:                .int sMessCounter
iAdrsMessNbSolution:             .int sMessNbSolution

/******************************************************************/
/*     search problem  unique solution                            */
/******************************************************************/
/* r0 contains start digit */
/* r1 contains end digit */
/* r2 contains action (0 display 1 count) */
searchPb:
    push {r0-r12,lr}                                  @ save  registers
    @ init
    ldr r3,iAdriValues_a                              @ area value a
    mov r4,#0
1:                                                    @ loop init value a
    str r0,[r3,r4,lsl #2]
    add r4,#1
    add r0,#1
    cmp r0,r1
    ble 1b

    mov r5,#0                                         @ solution counter
    mov r12,#-1
2:
    add r12,#1                                        @ increment indice a
    cmp r12,#NBBOX-1
    bgt 90f
    ldr r0,iAdriValues_a                              @ area value a
    ldr r1,iAdriValues_b                              @ area value b
    mov r2,r12                                        @ indice  a
    mov r3,#NBBOX                                     @ number of origin values
    bl prepValues
    mov r11,#-1
3:
    add r11,#1                                        @ increment indice b
    cmp r11,#NBBOX - 2
    bgt 2b
    ldr r0,iAdriValues_b                              @ area value b
    ldr r1,iAdriValues_c                              @ area value c
    mov r2,r11                                        @ indice b
    mov r3,#NBBOX -1                                  @ number of origin values
    bl prepValues
    mov r10,#-1
4:
    add r10,#1
    cmp r10,#NBBOX - 3
    bgt 3b
    ldr r0,iAdriValues_c
    ldr r1,iAdriValues_d
    mov r2,r10
    mov r3,#NBBOX - 2
    bl prepValues
    mov r9,#-1
5:
    add r9,#1
    cmp r9,#NBBOX - 4
    bgt 4b
    @ control 2 firsts squares
    ldr r0,iAdriValues_a
    ldr r0,[r0,r12,lsl #2]
    ldr r1,iAdriValues_b
    ldr r1,[r1,r11,lsl #2]
    add r4,r0,r1                               @ a + b   value first square
    ldr r0,iAdriValues_c
    ldr r0,[r0,r10,lsl #2]
    add r7,r1,r0                               @ b + c
    ldr r1,iAdriValues_d
    ldr r1,[r1,r9,lsl #2]
    add r7,r1                                  @ b + c + d
    cmp r7,r4                                  @ equal first square ?
    bne 5b
    ldr r0,iAdriValues_d
    ldr r1,iAdriValues_e
    mov r2,r9
    mov r3,#NBBOX - 3
    bl prepValues
    mov r8,#-1
6:
    add r8,#1
    cmp r8,#NBBOX - 5
    bgt 5b
    ldr r0,iAdriValues_e
    ldr r1,iAdriValues_f
    mov r2,r8
    mov r3,#NBBOX - 4
    bl prepValues
    mov r7,#-1
7:
    add r7,#1
    cmp r7,#NBBOX - 6
    bgt 6b
    ldr r0,iAdriValues_d
    ldr r0,[r0,r9,lsl #2]
    ldr r1,iAdriValues_e
    ldr r1,[r1,r8,lsl #2]
    add r3,r0,r1                                @ d + e
    ldr r1,iAdriValues_f
    ldr r1,[r1,r7,lsl #2]
    add r3,r1                                   @ de + e + f
    cmp r3,r4                                   @ equal first square ?
    bne 7b
    ldr r0,iAdriValues_f
    ldr r1,iAdriValues_g
    mov r2,r7
    mov r3,#NBBOX - 5
    bl prepValues
    mov r6,#-1
8:
    add r6,#1
    cmp r6,#NBBOX - 7
    bgt 7b
    ldr r0,iAdriValues_f
    ldr r0,[r0,r7,lsl #2]
    ldr r1,iAdriValues_g
    ldr r1,[r1,r6,lsl #2]
    add r3,r0,r1                               @ f +g
    cmp r4,r3                                  @ equal first square ?
    bne 8b
    add r5,#1                                  @ increment counter
    ldr r0,[sp,#8]                             @ load action for two parameter in stack
    tst r0,#0b1
    beq 9f                                     @ display solution ?
    ldr r0,iAdriValues_a
    ldr r0,[r0,r12,lsl #2]
    ldr r1,iAdrsMessValeur_a
    bl conversion10
    ldr r0,iAdriValues_b
    ldr r0,[r0,r11,lsl #2]
    ldr r1,iAdrsMessValeur_b
    bl conversion10
    ldr r0,iAdriValues_c
    ldr r0,[r0,r10,lsl #2]
    ldr r1,iAdrsMessValeur_c
    bl conversion10
    ldr r0,iAdriValues_d
    ldr r0,[r0,r9,lsl #2]
    ldr r1,iAdrsMessValeur_d
    bl conversion10
    ldr r0,iAdriValues_e
    ldr r0,[r0,r8,lsl #2]
    ldr r1,iAdrsMessValeur_e
    bl conversion10
    ldr r0,iAdriValues_f
    ldr r0,[r0,r7,lsl #2]
    ldr r1,iAdrsMessValeur_f
    bl conversion10
    ldr r0,iAdriValues_g
    ldr r0,[r0,r6,lsl #2]
    ldr r1,iAdrsMessValeur_g
    bl conversion10
    ldr r0,iAdrsMessDeb
    bl affichageMess
9:
    b 8b    @ suite

90:
    ldr r0,[sp,#8]                                @ load action for two parameter in stack
    tst r0,#0b10
    beq 100f                                      @ display counter ?
    mov r0,r5
    ldr r1,iAdrsMessCounter
    bl conversion10
    ldr r0,iAdrsMessNbSolution
    bl affichageMess
100:
    pop {r0-r12,lr}                               @ restaur registers
    bx lr                                         @return
iAdriValues_a:                   .int iValues_a
iAdriValues_b:                   .int iValues_b
iAdriValues_c:                   .int iValues_c
iAdriValues_d:                   .int iValues_d
iAdriValues_e:                   .int iValues_e
iAdriValues_f:                   .int iValues_f
iAdriValues_g:                   .int iValues_g

iAdrsMessValeur_a:               .int sMessValeur_a
iAdrsMessValeur_b:               .int sMessValeur_b
iAdrsMessValeur_c:               .int sMessValeur_c
iAdrsMessValeur_d:               .int sMessValeur_d
iAdrsMessValeur_e:               .int sMessValeur_e
iAdrsMessValeur_f:               .int sMessValeur_f
iAdrsMessValeur_g:               .int sMessValeur_g
iAdrsMessDeb:                    .int sMessDeb
iAdriCounterSol:                 .int iCounterSol
/******************************************************************/
/*     copy value area  and substract value of indice             */
/******************************************************************/
/* r0 contains the address of values origin */
/* r1 contains the address of values destination */
/* r2 contains value indice to substract     */
/* r3 contains origin values number          */
prepValues:
    push {r1-r6,lr}                                @ save  registres
    mov r4,#0                                      @ indice origin value
    mov r5,#0                                      @ indice destination value
1:
    cmp r4,r2                                      @ substract indice ?
    beq 2f                                         @ yes -> jump
    ldr r6,[r0,r4,lsl #2]                          @ no -> copy value
    str r6,[r1,r5,lsl #2]
    add r5,#1                                      @ increment destination indice
2:
   add r4,#1                                       @ increment origin indice
   cmp r4,r3                                       @ end ?
   blt 1b
100:
    pop {r1-r6,lr}                                 @ restaur registres
    bx lr                                          @return
/******************************************************************/
/*     display text with size calculation                         */
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length
1:                                                 @ loop length calculation
    ldrb r1,[r0,r2]                                @ read octet start position + index
    cmp r1,#0                                      @ if 0 its over
    addne r2,r2,#1                                 @ else add 1 in the length
    bne 1b                                         @ and loop
                                                   @ so here r2 contains the length of the message
    mov r1,r0                                      @ address message in r1
    mov r0,#STDOUT                                 @ code to write to the standard output Linux
    mov r7, #WRITE                                 @ code call system "write"
    svc #0                                         @ call systeme
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres */
    bx lr                                          @ return
/******************************************************************/
/*     Converting a register to a decimal unsigned                */
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                                 @ save registers
    mov r3,r1
    mov r2,#LGZONECAL
1:                                                  @ start loop
    bl divisionpar10U                               @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0
    subne r2,#1                                     @ else previous position
    bne 1b                                          @ and loop
                                                    @ and move digit from left of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]
    add r2,#1
    add r4,#1
    cmp r2,#LGZONECAL
    ble 2b
                                                      @ and move spaces in end on area
    mov r0,r4                                         @ result length
    mov r1,#' '                                       @ space
3:
    strb r1,[r3,r4]                                   @ store space in area
    add r4,#1                                         @ next position
    cmp r4,#LGZONECAL
    ble 3b                                            @ loop if r4 <= area size

100:
    pop {r1-r4,lr}                                    @ restaur registres
    bx lr                                             @return

/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient    */
/* r1 remainder   */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0)
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function
iMagicNumber:  	.int 0xCCCCCCCD
```

Output:


```txt
a=3          b=7          c=2          d=1
e=5          f=4          g=6
************************
a=4          b=5          c=3          d=1
e=6          f=2          g=7
************************
a=4          b=7          c=1          d=3
e=2          f=6          g=5
************************
a=5          b=6          c=2          d=3
e=1          f=7          g=4
************************
a=6          b=4          c=1          d=5
e=2          f=3          g=7
************************
a=6          b=4          c=5          d=1
e=2          f=7          g=3
************************
a=7          b=2          c=6          d=1
e=3          f=5          g=4
************************
a=7          b=3          c=2          d=5
e=1          f=4          g=6
************************
Number of solutions :8

a=7          b=8          c=3          d=4
e=5          f=6          g=9
************************
a=8          b=7          c=3          d=5
e=4          f=6          g=9
************************
a=9          b=6          c=4          d=5
e=3          f=7          g=8
************************
a=9          b=6          c=5          d=4
e=3          f=8          g=7
************************
Number of solutions :4

Number of solutions :2860
```


## AWK

```AWK
# syntax: GAWK -f 4-RINGS_OR_4-SQUARES_PUZZLE.AWK
# converted from C
BEGIN {
    cmd = "SORT /+16"
    four_squares(1,7,1,1)
    four_squares(3,9,1,1)
    four_squares(0,9,0,0)
    four_squares(0,6,1,0)
    four_squares(2,8,1,0)
    exit(0)
}
function four_squares(plo,phi,punique,pshow) {
    lo = plo
    hi = phi
    unique = punique
    show = pshow
    solutions = 0
    print("")
    if (show) {
      print("A B C D E F G  sum  A+B B+C+D D+E+F F+G")
      print("-------------  ---  -------------------")
    }
    acd()
    close(cmd)
    tmp = (unique) ? "unique" : "non-unique"
    printf("%d-%d: %d %s solutions\n",lo,hi,solutions,tmp)
}
function acd() {
    for (c=lo; c<=hi; c++) {
      for (d=lo; d<=hi; d++) {
        if (!unique || c != d) {
          a = c + d
          if (a >= lo && a <= hi && (!unique || (c != 0 && d != 0))) {
            ge()
          }
        }
      }
    }
}
function bf() {
    for (f=lo; f<=hi; f++) {
      if (!unique || (f != a && f != c && f != d && f != g && f != e)) {
        b = e + f - c
        if (b >= lo && b <= hi && (!unique || (b != a && b != c && b != d && b != g && b != e && b != f))) {
          solutions++
          if (show) {
            printf("%d %d %d %d %d %d %d %4d  ",a,b,c,d,e,f,g,a+b) | cmd
            printf("%d+%d ",a,b) | cmd
            printf("%d+%d+%d ",b,c,d) | cmd
            printf("%d+%d+%d ",d,e,f) | cmd
            printf("%d+%d\n",f,g) | cmd
          }
        }
      }
    }
}
function ge() {
    for (e=lo; e<=hi; e++) {
      if (!unique || (e != a && e != c && e != d)) {
        g = d + e
        if (g >= lo && g <= hi && (!unique || (g != a && g != c && g != d && g != e))) {
          bf()
        }
      }
    }
}
```

Output:

```txt
A B C D E F G  sum  A+B B+C+D D+E+F F+G
-------------  ---  -------------------
4 5 3 1 6 2 7    9  4+5 5+3+1 1+6+2 2+7
7 2 6 1 3 5 4    9  7+2 2+6+1 1+3+5 5+4
3 7 2 1 5 4 6   10  3+7 7+2+1 1+5+4 4+6
6 4 1 5 2 3 7   10  6+4 4+1+5 5+2+3 3+7
6 4 5 1 2 7 3   10  6+4 4+5+1 1+2+7 7+3
7 3 2 5 1 4 6   10  7+3 3+2+5 5+1+4 4+6
4 7 1 3 2 6 5   11  4+7 7+1+3 3+2+6 6+5
5 6 2 3 1 7 4   11  5+6 6+2+3 3+1+7 7+4
1-7: 8 unique solutions

A B C D E F G  sum  A+B B+C+D D+E+F F+G
-------------  ---  -------------------
7 8 3 4 5 6 9   15  7+8 8+3+4 4+5+6 6+9
8 7 3 5 4 6 9   15  8+7 7+3+5 5+4+6 6+9
9 6 4 5 3 7 8   15  9+6 6+4+5 5+3+7 7+8
9 6 5 4 3 8 7   15  9+6 6+5+4 4+3+8 8+7
3-9: 4 unique solutions

0-9: 2860 non-unique solutions

0-6: 4 unique solutions

2-8: 8 unique solutions
```


## Befunge

This is loosely based on the [[4-rings_or_4-squares_puzzle#C|C]] algorithm,
although many of the conditions have been combined to minimize branching.
There is no option to choose whether the results are displayed or not -
unique solutions are always displayed,
and non-unique solutions just return the solution count.

```befunge
550" :woL">:#,_&>00p" :hgiH">:#,_&>1+10p" :)n/y( euqinU">:#,_>~>:4v
v!g03!:\*`\g01\!`\g00:p05:+g03:p04:_$30g1+:10g\`v1g<,+$p02%2_|#`*8<
>>+\30g-!+20g*!*00g\#v_$40g1+:10g\`^<<1g00p03<<<_$55+:,\."snoitul"v
v!`\g00::p07:+g04p06:<^<`\g01:+1g06$<_v#!\g00*!*g02++!-g05<  v"so"<
>\10g\`*\:::30g-!\40g-!+\50g-!+\60g-!  +60g::30g-!\40g-!+\^  >:#,_@
>0g50g.......55+,0vg02+1_80g1+:10g\`!^>>:80p60g+30g-:90p::00g\`!>>v
^9g03g04g06g08g07<_>>0>>^<<*!*g02++!-g07\+!-g06\+!-g05\+!-g04\!-<<\
>>10g\`*\:::::30g-!\40g-!+\50g-!+\60g-!+\70g-!+\80g-!+80g::::30g^^>
```

Output:

```txt
Low: 1
High: 7
Unique (y/n): y

4 7 1 3 2 6 5
6 4 1 5 2 3 7
3 7 2 1 5 4 6
5 6 2 3 1 7 4
7 3 2 5 1 4 6
4 5 3 1 6 2 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4

8 solutions
```

```txt
Low: 3
High: 9
Unique (y/n): y

7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7

4 solutions
```

```txt
Low: 0
High: 9
Unique (y/n): n


2860 solutions
```


## C

```C
#include <stdio.h>

#define TRUE 1
#define FALSE 0

int a,b,c,d,e,f,g;
int lo,hi,unique,show;
int solutions;

void
bf() {
    for (f = lo;f <= hi; f++)
        if ((!unique) ||
           ((f != a) && (f != c) && (f != d) && (f != g) && (f != e)))
            {
            b = e + f - c;
            if ((b >= lo) && (b <= hi) &&
                   ((!unique) || ((b != a) && (b != c) &&
                   (b != d) && (b != g) && (b != e) && (b != f))))
                {
                solutions++;
                if (show)
                    printf("%d %d %d %d %d %d %d\n",a,b,c,d,e,f,g);
                }
            }
}


void
ge() {
    for (e = lo;e <= hi; e++)
        if ((!unique) || ((e != a) && (e != c) && (e != d)))
            {
            g = d + e;
            if ((g >= lo) && (g <= hi) &&
                   ((!unique) || ((g != a) && (g != c) &&
                   (g != d) && (g != e))))
                bf();
            }
}

void
acd() {
    for (c = lo;c <= hi; c++)
        for (d = lo;d <= hi; d++)
            if ((!unique) || (c != d))
                {
                a = c + d;
                if ((a >= lo) && (a <= hi) &&
                   ((!unique) || ((c != 0) && (d != 0))))
                    ge();
                }
}


void
foursquares(int plo,int phi, int punique,int pshow) {
    lo = plo;
    hi = phi;
    unique = punique;
    show = pshow;
    solutions = 0;

    printf("\n");

    acd();

    if (unique)
        printf("\n%d unique solutions in %d to %d\n",solutions,lo,hi);
    else
        printf("\n%d non-unique solutions in %d to %d\n",solutions,lo,hi);
}

main() {
    foursquares(1,7,TRUE,TRUE);
    foursquares(3,9,TRUE,TRUE);
    foursquares(0,9,FALSE,FALSE);
}
```

Output

```txt
4 7 1 3 2 6 5
6 4 1 5 2 3 7
3 7 2 1 5 4 6
5 6 2 3 1 7 4
7 3 2 5 1 4 6
4 5 3 1 6 2 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4

8 unique solutions in 1 to 7

7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7

4 unique solutions in 3 to 9


2860 non-unique solutions in 0 to 9
```


## C++

```cpp
//C++14/17
#include <algorithm>//std::for_each
#include <iostream> //std::cout
#include <numeric>  //std::iota
#include <vector>   //std::vector, save solutions
#include <list>     //std::list, for fast erase

using std::begin, std::end, std::for_each;

//Generates all the valid solutions for the problem in the specified range [from, to)
std::list<std::vector<int>> combinations(int from, int to)
{
    if (from > to)
        return {};                          //Return nothing if limits are invalid

    auto pool = std::vector<int>(to - from);//Here we'll save our values
    std::iota(begin(pool), end(pool), from);//Populates pool

    auto solutions = std::list<std::vector<int>>{};   //List for the solutions

    //Brute-force calculation of valid values...
    for (auto a : pool)
        for (auto b : pool)
            for (auto c : pool)
                for (auto d : pool)
                    for (auto e : pool)
                        for (auto f : pool)
                            for (auto g : pool)
                                if ( a      == c + d
                                  && b + c  == e + f
                                  && d + e  ==     g )
                                    solutions.push_back({a, b, c, d, e, f, g});
    return solutions;
}

//Filter the list generated from "combinations" and return only lists with no repetitions
std::list<std::vector<int>> filter_unique(int from, int to)
{
    //Helper lambda to check repetitions:
    //If the count is > 1 for an element, there must be a repetition inside the range
    auto has_non_unique_values = [](const auto & range, auto target)
    {
        return std::count( begin(range), end(range), target) > 1;
    };

    //Generates all the solutions...
    auto results = combinations(from, to);

    //For each solution, find duplicates inside
    for (auto subrange = cbegin(results); subrange != cend(results); ++subrange)
    {
        bool repetition = false;

        //If some element is repeated, repetition becomes true
        for (auto x : *subrange)
            repetition |= has_non_unique_values(*subrange, x);

        if (repetition)    //If repetition is true, remove the current subrange from the list
        {
            results.erase(subrange);        //Deletes subrange from solutions
            --subrange;                     //Rewind to the last subrange analysed
        }
    }

    return results; //Finally return remaining results
}

template <class Container> //Template for the sake of simplicity
inline void print_range(const Container & c)
{
    for (const auto & subrange : c)
    {
        std::cout << "[";
        for (auto elem : subrange)
            std::cout << elem << ' ';
        std::cout << "\b]\n";
    }
}


int main()
{
    std::cout << "Unique-numbers combinations in range 1-7:\n";
    auto solution1 = filter_unique(1, 8);
    print_range(solution1);
    std::cout << "\nUnique-numbers combinations in range 3-9:\n";
    auto solution2 = filter_unique(3,10);
    print_range(solution2);
    std::cout << "\nNumber of combinations in range 0-9: "
              << combinations(0, 10).size() << "." << std::endl;

    return 0;
}
```

Output

```txt
Unique-numbers combinations in range 1-7:
[3 7 2 1 5 4 6]
[4 5 3 1 6 2 7]
[4 7 1 3 2 6 5]
[5 6 2 3 1 7 4]
[6 4 1 5 2 3 7]
[6 4 5 1 2 7 3]
[7 2 6 1 3 5 4]
[7 3 2 5 1 4 6]

Unique-numbers combinations in range 3-9:
[7 8 3 4 5 6 9]
[8 7 3 5 4 6 9]
[9 6 4 5 3 7 8]
[9 6 5 4 3 8 7]

Number of combinations in range 0-9: 2860.
```


## C#

Translated from Java

```c#
using System;
using System.Linq;

namespace Four_Squares_Puzzle {
    class Program {
        static void Main(string[] args) {
            fourSquare(1, 7, true, true);
            fourSquare(3, 9, true, true);
            fourSquare(0, 9, false, false);
        }

        private static void fourSquare(int low, int high, bool unique, bool print) {
            int count = 0;

            if (print) {
                Console.WriteLine("a b c d e f g");
            }
            for (int a = low; a <= high; ++a) {
                for (int b = low; b <= high; ++b) {
                    if (notValid(unique, b, a)) continue;

                    int fp = a + b;
                    for (int c = low; c <= high; ++c) {
                        if (notValid(unique, c, b, a)) continue;
                        for (int d = low; d <= high; ++d) {
                            if (notValid(unique, d, c, b, a)) continue;
                            if (fp != b + c + d) continue;

                            for (int e = low; e <= high; ++e) {
                                if (notValid(unique, e, d, c, b, a)) continue;
                                for (int f = low; f <= high; ++f) {
                                    if (notValid(unique, f, e, d, c, b, a)) continue;
                                    if (fp != d + e + f) continue;

                                    for (int g = low; g <= high; ++g) {
                                        if (notValid(unique, g, f, e, d, c, b, a)) continue;
                                        if (fp != f + g) continue;

                                        ++count;
                                        if (print) {
                                            Console.WriteLine("{0} {1} {2} {3} {4} {5} {6}", a, b, c, d, e, f, g);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (unique) {
                Console.WriteLine("There are {0} unique solutions in [{1}, {2}]", count, low, high);
            }
            else {
                Console.WriteLine("There are {0} non-unique solutions in [{1}, {2}]", count, low, high);
            }
        }

        private static bool notValid(bool unique, int needle, params int[] haystack) {
            return unique && haystack.Any(p => p == needle);
        }
    }
}
```

Output:

```txt
a b c d e f g
3 7 2 1 5 4 6
4 5 3 1 6 2 7
4 7 1 3 2 6 5
5 6 2 3 1 7 4
6 4 1 5 2 3 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4
7 3 2 5 1 4 6
There are 8 unique solutions in [1, 7]
a b c d e f g
7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7
There are 4 unique solutions in [3, 9]
There are 2860 non-unique solutions in [0, 9]
```


## Clojure

```clojure
(use '[clojure.math.combinatorics]

(defn rings [r & {:keys [unique] :or {unique true}}]
    (if unique
      (apply concat (map permutations (combinations r 7)))
      (selections r 7)))

(defn four-rings [low high & {:keys [unique] :or {unique true}}]
  (for [[a b c d e f g] (rings (range low (inc high)) :unique unique)
    :when (= (+ a b) (+ b c d) (+ d e f) (+ f g))] [a b c d e f g]))
```

Output:

```txt
=> (pprint (four-rings 1 7))
([3 7 2 1 5 4 6]
 [4 5 3 1 6 2 7]
 [4 7 1 3 2 6 5]
 [5 6 2 3 1 7 4]
 [6 4 1 5 2 3 7]
 [6 4 5 1 2 7 3]
 [7 2 6 1 3 5 4]
 [7 3 2 5 1 4 6])
nil

=> (pprint (four-rings 3 9))
([7 8 3 4 5 6 9] [8 7 3 5 4 6 9] [9 6 4 5 3 7 8] [9 6 5 4 3 8 7])
nil

=> (count (four-rings 0 9 :unique false))
2860
```



## Common Lisp

```lisp
(defpackage four-rings
  (:use common-lisp)
  (:export display-solutions))
(in-package four-rings)

(defun correct-answer-p (a b c d e f g)
  (let ((v (+ a b)))
    (and (equal v (+ b c d))
         (equal v (+ d e f))
         (equal v (+ f g)))))

(defun combinations-if (func len unique min max)
  (let ((results nil))
    (labels ((inner (cur)
               (if (eql (length cur) len)
                 (when (apply func (reverse cur))
                   (push cur results))
                 (dotimes (i (- max min))
                   (when (or (not unique)
                             (not (member (+ i min) cur)))
                     (inner (append (list (+ i min)) cur)))))))
      (inner nil))
    results))

(defun four-rings-solutions (low high unique)
  (combinations-if #'correct-answer-p 7 unique low (1+ high)))

(defun display-solutions ()
  (let ((letters '((a b c d e f g))))
    (format t "Low 1, High 7, unique letters: ~%~{~{~3A~}~%~}~%"
            (append letters (four-rings-solutions 1 7 t)))
    (format t "Low 3, High 9, unique letters: ~%~{~{~3A~}~%~}~%"
            (append letters (four-rings-solutions 3 9 t)))
    (format t "Number of solutions for Low 0, High 9 non-unique:~%~A~%"
            (length (four-rings-solutions 0 9 nil)))))
```

Output:

```txt
CL-USER> (four-rings:display-solutions)
Low 1, High 7, unique letters:
A  B  C  D  E  F  G
6  4  1  5  2  3  7
4  5  3  1  6  2  7
3  7  2  1  5  4  6
7  3  2  5  1  4  6
4  7  1  3  2  6  5
5  6  2  3  1  7  4
7  2  6  1  3  5  4
6  4  5  1  2  7  3

Low 3, High 9, unique letters:
A  B  C  D  E  F  G
7  8  3  4  5  6  9
8  7  3  5  4  6  9
9  6  4  5  3  7  8
9  6  5  4  3  8  7

Number of solutions for Low 0, High 9 non-unique:
2860
NIL
```


## Crystal

Translated from Ruby

```ruby
def check(list)
  a, b, c, d, e, f, g = list
  first = a + b
  {b + c + d, d + e + f, f + g}.all? &.==(first)
end

def four_squares(low, high, unique = true, show = unique)
  solutions = [] of Array(Int32)
  if unique
    uniq = "unique"
    (low..high).to_a.each_permutation(7, true) { |ary| solutions << ary.clone if check(ary) }
  else
    uniq = "non-unique"
    (low..high).to_a.each_repeated_permutation(7, true) { |ary| solutions << ary.clone if check(ary) }
  end
  if show
    puts " " + ("a".."g").join("  ")
    solutions.each { |ary| p ary }
  end
  puts "#{solutions.size} #{uniq} solutions in #{low} to #{high}"
  puts
end

{ {1, 7}, {3, 9} }.each do |(low, high)|
  four_squares(low, high)
end
four_squares(0, 9, false)
```


## D

```D
import std.stdio;

void main() {
    fourSquare(1,7,true,true);
    fourSquare(3,9,true,true);
    fourSquare(0,9,false,false);
}

void fourSquare(int low, int high, bool unique, bool print) {
    int count;

    if (print) {
        writeln("a b c d e f g");
    }
    for (int a=low; a<=high; ++a) {
        for (int b=low; b<=high; ++b) {
            if (!valid(unique, a, b)) continue;

            int fp = a+b;
            for (int c=low; c<=high; ++c) {
                if (!valid(unique, c, a, b)) continue;
                for (int d=low; d<=high; ++d) {
                    if (!valid(unique, d, a, b, c)) continue;
                    if (fp != b+c+d) continue;

                    for (int e=low; e<=high; ++e) {
                        if (!valid(unique, e, a, b, c, d)) continue;
                        for (int f=low; f<=high; ++f) {
                            if (!valid(unique, f, a, b, c, d, e)) continue;
                            if (fp != d+e+f) continue;

                            for (int g=low; g<=high; ++g) {
                                if (!valid(unique, g, a, b, c, d, e, f)) continue;
                                if (fp != f+g) continue;

                                ++count;
                                if (print) {
                                    writeln(a,' ',b,' ',c,' ',d,' ',e,' ',f,' ',g);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    if (unique) {
        writeln("There are ", count, " unique solutions in [",low,",",high,"]");
    } else {
        writeln("There are ", count, " non-unique solutions in [",low,",",high,"]");
    }
}

bool valid(bool unique, int needle, int[] haystack ...) {
    if (unique) {
        foreach (value; haystack) {
            if (needle == value) {
                return false;
            }
        }
    }
    return true;
}
```

Output:

```txt
a b c d e f g
3 7 2 1 5 4 6
4 5 3 1 6 2 7
4 7 1 3 2 6 5
5 6 2 3 1 7 4
6 4 1 5 2 3 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4
7 3 2 5 1 4 6
There are 8 unique solutions in [1,7]
a b c d e f g
7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7
There are 4 unique solutions in [3,9]
There are 2860 non-unique solutions in [0,9]
```


## F#

```fsharp
(* A simple function to generate the sequence
   Nigel Galloway: January 31st., 2017 *)
type G = {d:int;x:int;b:int;f:int}
let N n g =
  {(max (n-g) n) .. (min (g-n) g)} |> Seq.collect(fun d->{(max (d+n+n) (n+n))..(min (g+g) (d+g+g))}           |> Seq.collect(fun x ->
  seq{for a in n .. g do for b in n .. g do if (a+b) = x then for c in n .. g do if (b+c+d) = x then yield b} |> Seq.collect(fun b ->
  seq{for f in n .. g do for G in n .. g do if (f+G) = x then for e in n .. g do if (f+e+d) = x then yield f} |> Seq.map(fun f -> {d=d;x=x;b=b;f=f}))))

printfn "%d" (Seq.length (N 0 9))
```

Output:

```txt
2860
```

```fsharp
(* A simple function to generate the sequence with unique values
   Nigel Galloway: January 31st., 2017 *)
type G = {d:int;x:int;b:int;f:int}
let N n g =
  {(max (n-g) n) .. (min (g-n) g)} |> Seq.filter(fun d -> d <> 0) |> Seq.collect(fun d->{(max (d+n+n) (n+n)) .. (min (g+g) (d+g+g))} |> Seq.collect(fun x ->
  seq{for a in n .. g do if a <> d then for b in n .. g do if (a+b) = x && b <> a && b <> d then for c in n .. g do if (b+c+d) = x && c <> d && c <> a && c <> b then yield b} |> Seq.collect(fun b ->
  seq{for f in n .. g do if f <> d && f <> b && f <> (x-b) && f <> (x-d-b) then for G in n .. g do if (f+G) = x && G <> d && G <> b && G <> f && G <> (x-b) && G <> (x-d-b) then for e in n .. g do if (f+e+d) = x && e <> d && e <> b && e <> f && e <> G && e <> (x-b) && e <> (x-d-b) then yield f} |> Seq.map(fun f -> {d=d;x=x;b=b;f=f}))))

for n in N 1 7 do
    printfn "%d,%d,%d,%d,%d,%d,%d" (n.x-n.b) n.b (n.x-n.d-n.b) n.d (n.x-n.d-n.f) n.f (n.x-n.f)
```

Output:

```txt
4,5,3,1,6,2,7
7,2,6,1,3,5,4
3,7,2,1,5,4,6
6,4,5,1,2,7,3
4,7,1,3,2,6,5
5,6,2,3,1,7,4
6,4,1,5,2,3,7
7,3,2,5,1,4,6
```

and:

```fsharp
for n in N 3 9 do
    printfn "%d,%d,%d,%d,%d,%d,%d" (n.x-n.b) n.b (n.x-n.d-n.b) n.d (n.x-n.d-n.f) n.f (n.x-n.f)
```

Output:

```txt
7,8,3,4,5,6,9
9,6,5,4,3,8,7
8,7,3,5,4,6,9
9,6,4,5,3,7,8
```


## Factor

This solution uses the `backtrack` vocabulary —
Factor's implementation of John McCarthy's [http://www.rosettacode.org/wiki/Amb ambiguous operator](/).
In short, we define 7 integers that can take up any value
within the range that we give it, such as [3,9], and assign them names a-g.
We then test whether the four sums from the puzzle are equal,
and if applicable, whether a-g are unique.
We send this boolean value to `must-be-true` and if it's false,
then the other possibilities will be explored through the power of continuations.

`bag-of` is a combinator (higher-order function)
that yields _every_ solution in a collection.
If we had written `4-rings` without using `bag-of`,
it would have returned only the first solution it found.

```factor
USING: arrays backtrack formatting grouping kernel locals math
math.ranges prettyprint sequences sequences.generalizations
sets ;
IN: rosetta-code.4-rings

:: 4-rings ( lo hi unique? -- seq ) [
        7 [ lo hi [a,b] amb-lazy ] replicate
        7 firstn :> ( a b c d e f g )
        { a b c d e f g } :> p
        a b +
        b c d + +
        d e f + +
        f g +
        4array all-equal?
        unique? [ p all-unique? and ] when
        must-be-true p
    ] bag-of ;

: report ( lo hi unique? -- )
    3dup 4-rings over [ dup . ] when length swap "" "non-" ?
    "In [%d, %d] there are %d %sunique solutions.\n" printf ;

1 7 t report
3 9 t report
0 9 f report
```

Output:

```txt
V{
    { 3 7 2 1 5 4 6 }
    { 4 5 3 1 6 2 7 }
    { 4 7 1 3 2 6 5 }
    { 5 6 2 3 1 7 4 }
    { 6 4 1 5 2 3 7 }
    { 6 4 5 1 2 7 3 }
    { 7 2 6 1 3 5 4 }
    { 7 3 2 5 1 4 6 }
}
In [1, 7] there are 8 unique solutions.
V{
    { 7 8 3 4 5 6 9 }
    { 8 7 3 5 4 6 9 }
    { 9 6 4 5 3 7 8 }
    { 9 6 5 4 3 8 7 }
}
In [3, 9] there are 4 unique solutions.
In [0, 9] there are 2860 non-unique solutions.
```


## Fortran

This uses the facility standardised in F90 whereby DO-loops can have text labels attached (not in the usual label area) so that the END DO statement can have the corresponding label, and any CYCLE statements can use it also.
Similarly, the subroutine's END statement bears the name of the subroutine.
This is just syntactic decoration.
Rather more useful is extended syntax for dealing with arrays and especially the function ANY for making multiple tests without having to enumerate them in the code.
To gain this convenience, the EQUIVALENCE statement makes variables A, B, C, D, E, F, and G occupy the same storage as <code>INTEGER V(7)</code>, an array.

One could abandon the use of the named variables in favour of manipulating the array equivalent, and indeed develop code which performs the nested loops via messing with the array, but for simplicity, the individual variables are used.
However, tempting though it is to write a systematic sequence of seven nested DO-loops, the variables are not in fact all independent: some are fixed once others are chosen.
Just cycling through all the notional possibilities when one only is in fact possible is a bit too much brute-force-and-ignorance, though other problems with other constraints, may encourage such exhaustive stepping.
As a result, the code is more tightly bound to the specific features of the problem.

Also standardised in F90 is the $ format code, which specifies that the output line is not to end with the WRITE statement.
The problem here is that Fortran does not offer an IF ...FI bracketing construction inside an expression, that would allow something like
```Fortran
WRITE(...) FIRST,LAST,IF (UNIQUE) THEN "Distinct values only" ELSE "Repeated values allowed" FI // "."
```
so that the correct alternative will be selected.
Further, an array (that would hold those two texts) can't be indexed by a LOGICAL variable, and playing with EQUIVALENCE won't help, because the numerical values revealed thereby for .TRUE.
and .FALSE.
may not be 1 and 0.
And anyway, parameters are not allowed to be accessed via EQUIVALENCE to another variable.

So, a two-part output, and to reduce the blather, two IF-statements.

```Fortran
      SUBROUTINE FOURSHOW(FIRST,LAST,UNIQUE)	!The "Four Rings" or "Four Squares" puzzle.
Choose values such that A+B = B+C+D = D+E+F = F+G, all being integers in FIRST:LAST...
       INTEGER FIRST,LAST	!The range of allowed values.
       LOGICAL UNIQUE		!Solutions need not have unique values.
       INTEGER A,B,C,D,E,F,G	!Ah, Diophantus of Alexandria.
       INTEGER V(7),S,N		!Assistants.
       EQUIVALENCE (V(1),A),(V(2),B),(V(3),C),		!Yes,
     1             (V(4),D),(V(5),E),(V(6),F),(V(7),G)	!We're all individuals.
        WRITE (6,1) FIRST,LAST	!Announce: first part.
    1   FORMAT (/,"The Four Rings puzzle, over ",I0," to ",I0,".",$)	!$: An addendum follows.
        IF (UNIQUE) WRITE (6,*) "Distinct values only."	!Save on the THEN ... ELSE ... END IF blather.
        IF (.NOT.UNIQUE) WRITE (6,*) "Repeated values allowed."	!Perhaps the compiler will be smarter.

        N = 0	!No solutions have been found.
      BB:DO B = FIRST,LAST	!Start chugging through the possibilities.
        CC:DO C = FIRST,LAST		!Brute force and ignorance.
             IF (UNIQUE .AND. B.EQ.C) CYCLE CC	!The first constraint shows up.
          DD:DO D = FIRST,LAST		!Start by forming B, C, and D.
               IF (UNIQUE .AND. ANY(V(2:3).EQ.D)) CYCLE DD	!Ignoring A just for now.
               S = B + C + D		!This is the common sum.
               A = S - B		!The value of A is not free from BCD.
               IF (A < FIRST .OR. A > LAST) CYCLE DD	!And it may not be within bounds.
               IF (UNIQUE .AND. ANY(V(2:4).EQ.A)) CYCLE DD	!Or, if required so, unique.
            EE:DO E = FIRST,LAST	!Righto, A,B,C,D are valid. Try an E.
                 IF (UNIQUE .AND. ANY(V(1:4).EQ.E)) CYCLE EE	!Precluded already?
                 F = S - (E + D)		!No. So therefore, F is determined.
                 IF (F < FIRST .OR. F > LAST) CYCLE EE	!Acceptable?
                 IF (UNIQUE .AND. ANY(V(1:5).EQ.F)) CYCLE EE	!And, if required, unique?
                 G = S - F			!Yes! So finally, G is determined.
                 IF (G < FIRST .OR. G > LAST) CYCLE EE	!Acceptable?
                 IF (UNIQUE .AND. ANY(V(1:6).EQ.G)) CYCLE EE	!And, if required, unique?
                 N = N + 1			!Yes! Count a solution set!
                 IF (UNIQUE) WRITE (6,"(7I3)") V	!Show its values.
               END DO EE			!Consder another E.
             END DO DD			!Consider another D.
           END DO CC		!Consider another C.
         END DO BB	!Consider another B.
        WRITE (6,2) N	!Announce the count.
    2   FORMAT (I9," found.")	!Numerous, if no need for distinct values.
      END SUBROUTINE FOURSHOW	!That was fun!

      PROGRAM POKE

      CALL FOURSHOW(1,7,.TRUE.)
      CALL FOURSHOW(3,9,.TRUE.)
      CALL FOURSHOW(0,9,.FALSE.)

      END
```

Output:
(Not in a neat order because the first variable is not determined first.)

```txt
The Four Rings puzzle, over 1 to 7. Distinct values only.
  7  2  6  1  3  5  4
  7  3  2  5  1  4  6
  6  4  1  5  2  3  7
  6  4  5  1  2  7  3
  4  5  3  1  6  2  7
  5  6  2  3  1  7  4
  4  7  1  3  2  6  5
  3  7  2  1  5  4  6
        8 found.

The Four Rings puzzle, over 3 to 9. Distinct values only.
  9  6  4  5  3  7  8
  9  6  5  4  3  8  7
  8  7  3  5  4  6  9
  7  8  3  4  5  6  9
        4 found.

The Four Rings puzzle, over 0 to 9. Repeated values allowed.
     2860 found.
```


One might hope that the ANY function will quit as soon as possible
and that it will not be invoked if UNIQUE is false,
but the modernisers have rejected reliance on
[[Talk:Short-circuit_evaluation#Compiler_optimisations.3F|short-circuit evaluation]]
and the "help" is quite general on the workings of the ANY function,
as also is modern. Here is a sample of the code produced
by the Compaq 6.6a Visual Fortran F90/95 compiler,
in its normal "debugging" condition and array bound checking of course active...

```txt
31:                    IF (UNIQUE .AND. ANY(V(1:6).EQ.G)) CYCLE EE    !And, if required, unique?
00401496   mov         edi,dword ptr [UNIQUE]
00401499   mov         edi,dword ptr [edi]
0040149B   mov         ebx,dword ptr [G (00470380)]
004014A1   mov         eax,0
004014A6   mov         ecx,1
004014AB   mov         dword ptr [ebp-60h],1
004014B2   cmp         dword ptr [ebp-60h],6
004014B6   jg          FOURSHOW+4C4h (004014fc)
004014B8   cmp         ecx,1
004014BB   jl          FOURSHOW+48Ah (004014c2)
004014BD   cmp         ecx,7
004014C0   jle         FOURSHOW+493h (004014cb)
004014C2   xor         esi,esi
004014C4   mov         dword ptr [ebp-6Ch],esi
004014C7   dec         esi
004014C8   bound       esi,qword ptr [ebp-6Ch]
004014CB   imul        esi,ecx,4
004014CE   mov         esi,dword ptr S+4 (00470364)[esi]
004014D4   xor         edx,edx
004014D6   cmp         esi,ebx
004014D8   sete        dl
004014DB   mov         dword ptr [ebp-6Ch],edx
004014DE   mov         edx,eax
004014E0   or          edx,dword ptr [ebp-6Ch]
004014E3   and         edx,1
004014E6   mov         eax,edx
004014E8   neg         eax
004014EA   mov         esi,ecx
004014EC   add         esi,1
004014EF   mov         ecx,esi
004014F1   mov         edx,dword ptr [ebp-60h]
004014F4   add         edx,1
004014F7   mov         dword ptr [ebp-60h],edx
004014FA   jmp         FOURSHOW+47Ah (004014b2)
004014FC   and         edi,eax
004014FE   mov         edx,edi
00401500   and         edx,1
00401503   cmp         edx,0
00401506   jne         FOURSHOW+531h (00401569)
32:                    N = N + 1          !Yes! Count a solution set!
00401508   mov         esi,dword ptr [N (0047035c)]
0040150E   add         esi,1
00401511   mov         dword ptr [N (0047035c)],esi
33:                    IF (UNIQUE) WRITE (6,"(7I3)") V    !Show its values.
```

I'd rather say nothing at all.


## FreeBASIC

```freebasic
' version 18-03-2017
' compile with: fbc -s console

' TRUE/FALSE are built-in constants since FreeBASIC 1.04
' But we have to define them for older versions.
#Ifndef TRUE
  #Define FALSE 0
  #Define TRUE Not FALSE
#EndIf

Sub four_rings(low As Long, high As Long, unique As Long, show As Long)

  Dim As Long a, b, c, d, e, f, g
  Dim As ULong t, total
  Dim As ULong l = Len(Str(high))
  If l < Len(Str(low)) Then l = Len(Str(low))


  If show = TRUE Then
    For a = 97 To 103
      Print Space(l); Chr(a);
    Next
    Print
    Print String((l +1) * 7, "=");
    Print
  End If

  For a = low To high
    For b = low To high
      If unique = TRUE Then
        If b = a Then Continue For
      End If
      t = a + b
      For c = low To high
        If unique = TRUE Then
          If c = a OrElse c = b Then Continue For
        End If
        For d = low To high
          If unique = TRUE Then
            If d = a OrElse d = b OrElse d = c Then Continue For
          End If
          If b + c + d = t Then
            For e = low To high
              If unique = TRUE Then
                If e = a OrElse e = b OrElse e = c OrElse e = d Then Continue For
              End If
              For f = low To high
                If unique = TRUE Then
                  If f = a OrElse f = b OrElse f = c OrElse f = d OrElse f = e Then Continue For
                End If
                If d + e + f = t Then
                  For g = low To high
                    If unique = TRUE Then
                      If g = a OrElse g = b OrElse g = c OrElse g = d OrElse g = e OrElse g = f Then Continue For
                    End If
                    If f + g = t Then
                      total += 1
                      If show = TRUE Then
                        Print Using String(l +1, "#"); a; b; c; d; e; f; g
                      End If
                    End If
                  Next
                End If
              Next
            Next
          End If
        Next
      Next
    Next
  Next

  If unique = TRUE Then
    Print
    Print total; " Unique solutions for "; Str(low); " to "; Str(high)
  Else
    Print total; " Non unique solutions for "; Str(low); " to "; Str(high)
  End If
  Print String(40, "-") : Print
End Sub

' ------=< MAIN >=------

four_rings(1, 7,  TRUE,  TRUE)
four_rings(3, 9,  TRUE,  TRUE)
four_rings(0, 9, FALSE, FALSE)

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

Output:

```txt
 a b c d e f g

### ========

 3 7 2 1 5 4 6
 4 5 3 1 6 2 7
 4 7 1 3 2 6 5
 5 6 2 3 1 7 4
 6 4 1 5 2 3 7
 6 4 5 1 2 7 3
 7 2 6 1 3 5 4
 7 3 2 5 1 4 6

8 Unique solutions for 1 to 7
----------------------------------------

 a b c d e f g

### ========

 7 8 3 4 5 6 9
 8 7 3 5 4 6 9
 9 6 4 5 3 7 8
 9 6 5 4 3 8 7

4 Unique solutions for 3 to 9
----------------------------------------

2860 Non unique solutions for 0 to 9
----------------------------------------
```


## Go

```go
package main

import "fmt"

func main(){
	n, c := getCombs(1,7,true)
	fmt.Printf("%d unique solutions in 1 to 7\n",n)
	fmt.Println(c)
	n, c = getCombs(3,9,true)
	fmt.Printf("%d unique solutions in 3 to 9\n",n)
	fmt.Println(c)
	n, _ = getCombs(0,9,false)
	fmt.Printf("%d non-unique solutions in 0 to 9\n",n)
}

func getCombs(low,high int,unique bool) (num int,validCombs [][]int){
	for a := low; a <= high; a++ {
		for b := low; b <= high; b++ {
			for c := low; c <= high; c++ {
				for d := low; d <= high; d++ {
					for e := low; e <= high; e++ {
						for f := low; f <= high; f++ {
							for g := low; g <= high; g++ {
								if validComb(a,b,c,d,e,f,g) {
									if !unique || isUnique(a,b,c,d,e,f,g) {
										num++
										validCombs = append(validCombs,[]int{a,b,c,d,e,f,g})
									}
								}
							}
						}
					}
				}
			}
		}
	}
	return
}
func isUnique(a,b,c,d,e,f,g int) (res bool) {
	data := make(map[int]int)
	data[a]++
	data[b]++
	data[c]++
	data[d]++
	data[e]++
	data[f]++
	data[g]++
	return len(data) == 7
}
func validComb(a,b,c,d,e,f,g int) bool{
	square1 := a + b
	square2 := b + c + d
	square3 := d + e + f
	square4 := f + g
	return square1 == square2 && square2 == square3 && square3 == square4
}
```

Output:

```txt
8 unique solutions in 1 to 7
[
    [3 7 2 1 5 4 6]
    [4 5 3 1 6 2 7]
    [4 7 1 3 2 6 5]
    [5 6 2 3 1 7 4]
    [6 4 1 5 2 3 7]
    [6 4 5 1 2 7 3]
    [7 2 6 1 3 5 4]
    [7 3 2 5 1 4 6]
]
4 unique solutions in 3 to 9
[[7 8 3 4 5 6 9] [8 7 3 5 4 6 9] [9 6 4 5 3 7 8] [9 6 5 4 3 8 7]]
2860 non-unique solutions in 0 to 9
```


## Groovy

Translated from Java

```groovy
class FourRings {
    static void main(String[] args) {
        fourSquare(1, 7, true, true)
        fourSquare(3, 9, true, true)
        fourSquare(0, 9, false, false)
    }

    private static void fourSquare(int low, int high, boolean unique, boolean print) {
        int count = 0

        if (print) {
            println("a b c d e f g")
        }
        for (int a = low; a <= high; ++a) {
            for (int b = low; b <= high; ++b) {
                if (notValid(unique, a, b)) continue

                int fp = a + b
                for (int c = low; c <= high; ++c) {
                    if (notValid(unique, c, a, b)) continue
                    for (int d = low; d <= high; ++d) {
                        if (notValid(unique, d, a, b, c)) continue
                        if (fp != b + c + d) continue

                        for (int e = low; e <= high; ++e) {
                            if (notValid(unique, e, a, b, c, d)) continue
                            for (int f = low; f <= high; ++f) {
                                if (notValid(unique, f, a, b, c, d, e)) continue
                                if (fp != d + e + f) continue

                                for (int g = low; g <= high; ++g) {
                                    if (notValid(unique, g, a, b, c, d, e, f)) continue
                                    if (fp != f + g) continue

                                    ++count
                                    if (print) {
                                        printf("%d %d %d %d %d %d %d%n", a, b, c, d, e, f, g)
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if (unique) {
            printf("There are %d unique solutions in [%d, %d]%n", count, low, high)
        } else {
            printf("There are %d non-unique solutions in [%d, %d]%n", count, low, high)
        }
    }

    private static boolean notValid(boolean unique, int needle, int ... haystack) {
        return unique && Arrays.stream(haystack).anyMatch({ p -> p == needle })
    }
}
```

Output:

```txt
a b c d e f g
3 7 2 1 5 4 6
4 5 3 1 6 2 7
4 7 1 3 2 6 5
5 6 2 3 1 7 4
6 4 1 5 2 3 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4
7 3 2 5 1 4 6
There are 8 unique solutions in [1, 7]
a b c d e f g
7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7
There are 4 unique solutions in [3, 9]
There are 2860 non-unique solutions in [0, 9]
```


## Haskell

### By exhaustive search

```haskell
import Data.List
import Control.Monad

perms :: (Eq a) => [a] -> [[a]]
perms [] = [[]]
perms xs = [ x:xr | x <- xs, xr <- perms (xs\\[x]) ]

combs :: (Eq a) => Int -> [a] -> [[a]]
combs 0 _ = [[]]
combs n xs = [ x:xr | x <- xs, xr <- combs (n-1) xs ]

ringCheck :: [Int] -> Bool
ringCheck [x0, x1, x2, x3, x4, x5, x6] =
          v == x1+x2+x3
       && v == x3+x4+x5
       && v == x5+x6
    where v = x0 + x1

fourRings :: Int -> Int -> Bool -> Bool -> IO ()
fourRings low high allowRepeats verbose = do
    let candidates = if allowRepeats
                     then combs 7 [low..high]
                     else perms [low..high]

        solutions = filter ringCheck candidates

    when verbose $ mapM_ print solutions

    putStrLn $    show (length solutions)
               ++ (if allowRepeats then " non" else "")
               ++ " unique solutions for "
               ++ show low
               ++ " to "
               ++ show high

    putStrLn ""

main = do
   fourRings 1 7 False True
   fourRings 3 9 False True
   fourRings 0 9 True False
```


Output:

```txt
[3,7,2,1,5,4,6]
[4,5,3,1,6,2,7]
[4,7,1,3,2,6,5]
[5,6,2,3,1,7,4]
[6,4,1,5,2,3,7]
[6,4,5,1,2,7,3]
[7,2,6,1,3,5,4]
[7,3,2,5,1,4,6]
8 unique solutions for 1 to 7

[7,8,3,4,5,6,9]
[8,7,3,5,4,6,9]
[9,6,4,5,3,7,8]
[9,6,5,4,3,8,7]
4 unique solutions for 3 to 9

2860 non unique solutions for 0 to 9
```



### By structured search

For a faster solution (under a third of a second,
vs over 25 seconds on this system for the brute force approach above),
we can nest a series of smaller and more focused searches
from the central digit outwards.

Two things to notice:
1. If we call the central digit the Queen,
    then in any solution the Queen plus its left neighbour
    (left Bishop) must sum to the value of the left Rook (leftmost digit).
    Symmetrically, the right Rook must be the sum of the Queen and right Bishop.
2. The difference between the left Rook and the right Rook must be
    (minus) the difference between the left Knight (between bishop and rook)
    and the right Knight.

Nesting four bind operators (>>=),
we can then build the set of solutions in the order:
queens, left bishops and rooks, right bishops and rooks, knights.
Probably less readable, but already fast, and could be further optimized.

```haskell
import Data.List (delete, sortBy, (\\))
import Control.Monad (when)
import Data.Bool (bool)

type Rings = [(Int, Int, Int, Int, Int, Int, Int)]

rings :: Bool -> [Int] -> Rings
rings u digits =
  let ds = sortBy (flip compare) digits
  in ds >>= queen u (head ds) ds

queen :: Bool -> Int -> [Int] -> Int -> Rings
queen u h ds q =
  let ts = filter ((<= h) . (q +)) ds
  in bool ds (delete q ts) u >>= leftBishop u q h ts ds

leftBishop :: Bool -> Int -> Int -> [Int] -> [Int] -> Int -> Rings
leftBishop u q h ts ds lb =
  let lRook = lb + q
  in bool
       []
       (bool ds (ts \\ [q, lb, lRook]) u >>= rightBishop u q h lb ds lRook)
       (lRook <= h)

rightBishop :: Bool -> Int -> Int -> Int -> [Int] -> Int -> Int -> Rings
rightBishop u q h lb ds lRook rb =
  let rRook = q + rb
  in bool
       []
       (let ks = bool ds (ds \\ [q, lb, rb, rRook, lRook]) u
        in ks >>= knights u (lRook - rRook) lRook lb q rb rRook ks)
       ((rRook <= h) && (not u || (rRook /= lb)))

knights :: Bool -> Int -> Int -> Int -> Int -> Int -> Int -> [Int] -> Int -> Rings
knights u rookDelta lRook lb q rb rRook ks k =
  let k2 = k + rookDelta
  in [ (lRook, k, lb, q, rb, k2, rRook)
     | (k2 `elem` ks) && (not u || notElem k2 [lRook, k, lb, q, rb, rRook]) ]


-- TEST ---------------------------------------------------
main :: IO ()
main = do
  let f (k, xs) = putStrLn k >> nl >> mapM_ print xs >> nl
      nl = putStrLn []
  mapM_
    f
    [ ("rings True [1 .. 7]", rings True [1 .. 7])
    , ("rings True [3 .. 9]", rings True [3 .. 9])
    ]
  f ("length (rings False [0 .. 9])", [length (rings False [0 .. 9])])
```

Output:

```txt
rings True [1 .. 7]

(7,3,2,5,1,4,6)
(6,4,1,5,2,3,7)
(5,6,2,3,1,7,4)
(4,7,1,3,2,6,5)
(7,2,6,1,3,5,4)
(6,4,5,1,2,7,3)
(4,5,3,1,6,2,7)
(3,7,2,1,5,4,6)

rings True [3 .. 9]

(9,6,4,5,3,7,8)
(8,7,3,5,4,6,9)
(9,6,5,4,3,8,7)
(7,8,3,4,5,6,9)

length (rings False [0 .. 9])

2860
```


## J

Implementation for the unique version of the puzzle:

```J
fspuz=:dyad define
  range=: x+i.1+y-x
  lo=. 6+3*x
  hi=. _3+2*y
  r=.i.0 0
  if. lo <: hi do.
    for_T.lo ([+[:i.1+-~) hi do.
      range2=: (#~ (T-{.range)>:]) range
      range3=: (#~ (T-+/2{.range)>:]) range
      ab=: (#~ ~:/"1) (,.T-])range2
      abc=: ;ab <@([ ,"1 0 -.~)"1/range3
      abcd=: (#~ T = +/@}."1) ;abc <@([ ,"1 0 -.~)"1/range3
      abcde=: ;abcd <@([ ,"1 0 -.~)"1/range3
      abcdef=: (#~ T = +/@(3}.])"1) ;abcde <@([ ,"1 0 -.~)"1/range3
      abcdefg=: (#~ T = +/@(5}.])"1) ;abcdef <@([ ,"1 0 -.~)"1/range2
      r=.r,(#~ x<:<./"1)(#~ y>:>./"1)abcdefg
    end.
  end.
)
```

Implementation for the non-unique version of the puzzle:

```J
fspuz2=:dyad define
  range=: x+i.1+y-x
  lo=. 3*x
  hi=. 2*y
  r=.i.0 0
  if. lo <: hi do.
    for_T.lo ([+[:i.1+-~) hi do.
      ab=: (,.T-])range
      abc=: ,/ab,"1 0/ range
      abcd=: (#~ T = +/@}."1) ,/abc,"1 0/ range
      abcde=: ,/abcd,"1 0/ range
      abcdef=: (#~ T = +/@(3}.])"1) ,/abcde ,"1 0/ range
      abcdefg=: (#~ T = +/@(5}.])"1) ,/abcdef,"1 0/ range
      r=.r,(#~ x<:<./"1)(#~ y>:>./"1)abcdefg
    end.
  end.
)
```

Task examples:

```J
   1 fspuz 7
4 5 3 1 6 2 7
7 2 6 1 3 5 4
3 7 2 1 5 4 6
6 4 1 5 2 3 7
6 4 5 1 2 7 3
7 3 2 5 1 4 6
4 7 1 3 2 6 5
5 6 2 3 1 7 4
   3 fspuz 9
7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7
   #0 fspuz2 9
2860
```


## Java

Uses java 8 features.

```Java
import java.util.Arrays;

public class FourSquares {
    public static void main(String[] args) {
        fourSquare(1, 7, true, true);
        fourSquare(3, 9, true, true);
        fourSquare(0, 9, false, false);
    }

    private static void fourSquare(int low, int high, boolean unique, boolean print) {
        int count = 0;

        if (print) {
            System.out.println("a b c d e f g");
        }
        for (int a = low; a <= high; ++a) {
            for (int b = low; b <= high; ++b) {
                if (notValid(unique, a, b)) continue;

                int fp = a + b;
                for (int c = low; c <= high; ++c) {
                    if (notValid(unique, c, a, b)) continue;
                    for (int d = low; d <= high; ++d) {
                        if (notValid(unique, d, a, b, c)) continue;
                        if (fp != b + c + d) continue;

                        for (int e = low; e <= high; ++e) {
                            if (notValid(unique, e, a, b, c, d)) continue;
                            for (int f = low; f <= high; ++f) {
                                if (notValid(unique, f, a, b, c, d, e)) continue;
                                if (fp != d + e + f) continue;

                                for (int g = low; g <= high; ++g) {
                                    if (notValid(unique, g, a, b, c, d, e, f)) continue;
                                    if (fp != f + g) continue;

                                    ++count;
                                    if (print) {
                                        System.out.printf(
                                            "%d %d %d %d %d %d %d%n",
                                            a, b, c, d, e, f, g
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        if (unique) {
            System.out.printf(
                "There are %d unique solutions in [%d, %d]%n",
                count, low, high
            );
        } else {
            System.out.printf(
                "There are %d non-unique solutions in [%d, %d]%n",
                count, low, high
            );
        }
    }

    private static boolean notValid(boolean unique, int needle, int... haystack) {
        return unique && Arrays.stream(haystack).anyMatch(p -> p == needle);
    }
}
```

Output:

```txt
a b c d e f g
3 7 2 1 5 4 6
4 5 3 1 6 2 7
4 7 1 3 2 6 5
5 6 2 3 1 7 4
6 4 1 5 2 3 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4
7 3 2 5 1 4 6
There are 8 unique solutions in [1, 7]
a b c d e f g
7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7
There are 4 unique solutions in [3, 9]
There are 2860 non-unique solutions in [0, 9]
```


## JavaScript

### ES6

Translated from Haskell}} (Structured search versio

```javascript
(() => {

    // 4-rings or 4-squares puzzle ------------------------

    // rings :: noRepeatedDigits -> DigitList -> solutions
    // rings :: Bool -> [Int] -> [[Int]]
    const rings = (uniq, digits) => {
        return 0 < digits.length ? (() => {
            const
                ns = sortBy(flip(compare), digits),
                h = head(ns);

            // CENTRAL DIGIT :: d
            return bindList(
                ns,
                d => {
                    const ts = filter(x => (x + d) <= h, ns);

                    // LEFT OF CENTRE :: c and a
                    return bindList(
                        uniq ? delete_(d, ts) : ns,
                        c => {
                            const a = c + d;

                            // RIGHT OF CENTRE :: e and g
                            return a > h ? (
                                []
                            ) : bindList(uniq ? (
                                difference(ts, [d, c, a])
                            ) : ns, e => {
                                const g = d + e;
                                return ((g > h) || (uniq && (g === c))) ? (
                                    []
                                ) : (() => {
                                    const
                                        agDelta = a - g,
                                        bfs = uniq ? difference(
                                            ns, [d, c, e, g, a]
                                        ) : ns;

                                    // MID LEFT, MID RIGHT :: b and f
                                    return bindList(bfs, b => {
                                        const f = b + agDelta;
                                        return elem(f, bfs) && (
                                            !uniq || notElem(f, [
                                                a, b, c, d, e, g
                                            ])
                                        ) ? ([
                                            [a, b, c, d, e, f, g]
                                        ]) : [];
                                    });
                                })();
                            });
                        });
                });
        })() : []
    };


    // TEST -----------------------------------------------
    const main = () => {
        return unlines([
            'rings(true, enumFromTo(1,7))\n',
            unlines(map(show, rings(true, enumFromTo(1, 7)))),

            '\nrings(true, enumFromTo(3, 9))\n',
            unlines(map(show, rings(true, enumFromTo(3, 9)))),

            '\nlength(rings(false, enumFromTo(0, 9)))\n',
            length(rings(false, enumFromTo(0, 9)))
            .toString(),
            ''
        ]);
    };

    // GENERIC FUNCTIONS ----------------------------------

    // bindList (>>=) :: [a] -> (a -> [b]) -> [b]
    const bindList = (xs, mf) => [].concat.apply([], xs.map(mf));

    // compare :: a -> a -> Ordering
    const compare = (a, b) => a < b ? -1 : (a > b ? 1 : 0);

    // delete_ :: Eq a => a -> [a] -> [a]
    const delete_ = (x, xs) =>
        xs.length > 0 ? (
            (x === xs[0]) ? (
                xs.slice(1)
            ) : [xs[0]].concat(delete_(x, xs.slice(1)))
        ) : [];

    // difference :: Eq a => [a] -> [a] -> [a]
    const difference = (xs, ys) => {
        const s = new Set(ys);
        return xs.filter(x => !s.has(x));
    };

    // elem :: Eq a => a -> [a] -> Bool
    const elem = (x, xs) => xs.indexOf(x) !== -1;

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f => (a, b) => f.apply(null, [b, a]);

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // length :: [a] -> Int
    const length = xs => xs.length;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // notElem :: Eq a => a -> [a] -> Bool
    const notElem = (x, xs) => xs.indexOf(x) === -1;

    // show :: a -> String
    const show = x => JSON.stringify(x); //, null, 2);

    // sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    const sortBy = (f, xs) => xs.sort(f);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');


    // MAIN ---
    return main();
})();
```

Output:

```txt
rings(true, enumFromTo(1,7))

[7,3,2,5,1,4,6]
[6,4,1,5,2,3,7]
[5,6,2,3,1,7,4]
[4,7,1,3,2,6,5]
[7,2,6,1,3,5,4]
[6,4,5,1,2,7,3]
[4,5,3,1,6,2,7]
[3,7,2,1,5,4,6]

rings(true, enumFromTo(3, 9))

[9,6,4,5,3,7,8]
[8,7,3,5,4,6,9]
[9,6,5,4,3,8,7]
[7,8,3,4,5,6,9]

length(rings(false, enumFromTo(0, 9)))

2860
```


## Julia

Translated from Python

```julia
using Combinatorics

function foursquares(low, high, onlyunique=true, showsolutions=true)
    integers = collect(low:high)
    count = 0
    sumsallequal(c) = c[1] + c[2] == c[2] + c[3] + c[4] == c[4] + c[5] + c[6] == c[6] + c[7]
    combos = onlyunique ? combinations(integers) :
                          with_replacement_combinations(integers, 7)
    for combo in combos, plist in unique(collect(permutations(combo, 7)))
        if sumsallequal(plist)
            count += 1
            if showsolutions
                println("$plist is a solution for the list $integers")
            end
        end
    end
    println("""Total $(onlyunique?"unique ":"")solutions for HIGH $high, LOW $low: $count""")
end

foursquares(1, 7, true, true)
foursquares(3, 9, true, true)
foursquares(0, 9, false, false)
```

Output:

```txt
[3, 7, 2, 1, 5, 4, 6] is a solution for the list [1, 2, 3, 4, 5, 6, 7]
[4, 5, 3, 1, 6, 2, 7] is a solution for the list [1, 2, 3, 4, 5, 6, 7]
[4, 7, 1, 3, 2, 6, 5] is a solution for the list [1, 2, 3, 4, 5, 6, 7]
[5, 6, 2, 3, 1, 7, 4] is a solution for the list [1, 2, 3, 4, 5, 6, 7]
[6, 4, 1, 5, 2, 3, 7] is a solution for the list [1, 2, 3, 4, 5, 6, 7]
[6, 4, 5, 1, 2, 7, 3] is a solution for the list [1, 2, 3, 4, 5, 6, 7]
[7, 2, 6, 1, 3, 5, 4] is a solution for the list [1, 2, 3, 4, 5, 6, 7]
[7, 3, 2, 5, 1, 4, 6] is a solution for the list [1, 2, 3, 4, 5, 6, 7]
Total unique solutions for HIGH 7, LOW 1: 8
[7, 8, 3, 4, 5, 6, 9] is a solution for the list [3, 4, 5, 6, 7, 8, 9]
[8, 7, 3, 5, 4, 6, 9] is a solution for the list [3, 4, 5, 6, 7, 8, 9]
[9, 6, 4, 5, 3, 7, 8] is a solution for the list [3, 4, 5, 6, 7, 8, 9]
[9, 6, 5, 4, 3, 8, 7] is a solution for the list [3, 4, 5, 6, 7, 8, 9]
Total unique solutions for HIGH 9, LOW 3: 4
Total solutions for HIGH 9, LOW 0: 2860
```


## Kotlin

Translated from C

```scala
// version 1.1.2

class FourSquares(
    private val lo: Int,
    private val hi: Int,
    private val unique: Boolean,
    private val show: Boolean
) {
    private var a = 0
    private var b = 0
    private var c = 0
    private var d = 0
    private var e = 0
    private var f = 0
    private var g = 0
    private var s = 0

    init {
        println()
        if (show) {
            println("a b c d e f g")
            println("-------------")
        }
        acd()
        println("\n$s ${if (unique) "unique" else "non-unique"} solutions in $lo to $hi")
    }

    private fun acd() {
        c = lo
        while (c <= hi) {
            d = lo
            while (d <= hi) {
                if (!unique || c != d) {
                    a = c + d
                    if ((a in lo..hi) && (!unique || (c != 0 && d!= 0))) ge()
                }
                d++
            }
            c++
        }
    }

    private fun bf() {
        f = lo
        while (f <= hi) {
            if (!unique || (f != a && f != c && f != d && f != e && f!= g)) {
                b = e + f - c
                if ((b in lo..hi) && (!unique || (b != a && b != c && b != d && b != e && b != f && b!= g))) {
                    s++
                    if (show) println("$a $b $c $d $e $f $g")
                }
            }
            f++
        }
    }

    private fun ge() {
        e = lo
        while (e <= hi) {
            if (!unique || (e != a && e != c && e != d)) {
                g = d + e
                if ((g in lo..hi) && (!unique || (g != a && g != c && g != d && g != e))) bf()
            }
            e++
        }
    }
}

fun main(args: Array<String>) {
    FourSquares(1, 7, true, true)
    FourSquares(3, 9, true, true)
    FourSquares(0, 9, false, false)
}
```

Output:

```txt
a b c d e f g
-------------
4 7 1 3 2 6 5
6 4 1 5 2 3 7
3 7 2 1 5 4 6
5 6 2 3 1 7 4
7 3 2 5 1 4 6
4 5 3 1 6 2 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4

8 unique solutions in 1 to 7

a b c d e f g
-------------
7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7

4 unique solutions in 3 to 9


2860 non-unique solutions in 0 to 9
```



## Lua

Translated from D

```lua
function valid(unique,needle,haystack)
    if unique then
        for _,value in pairs(haystack) do
            if needle == value then
                return false
            end
        end
    end
    return true
end

function fourSquare(low,high,unique,prnt)
    count = 0
    if prnt then
        print("a", "b", "c", "d", "e", "f", "g")
    end
    for a=low,high do
        for b=low,high do
            if valid(unique, a, {b}) then
                fp = a + b
                for c=low,high do
                    if valid(unique, c, {a, b}) then
                        for d=low,high do
                            if valid(unique, d, {a, b, c}) and fp == b + c + d then
                                for e=low,high do
                                    if valid(unique, e, {a, b, c, d}) then
                                        for f=low,high do
                                            if valid(unique, f, {a, b, c, d, e}) and fp == d + e + f then
                                                for g=low,high do
                                                    if valid(unique, g, {a, b, c, d, e, f}) and fp == f + g then
                                                        count = count + 1
                                                        if prnt then
                                                            print(a, b, c, d, e, f, g)
                                                        end
                                                    end
                                                end
                                            end
                                        end
                                    end
                                end
                            end
                        end
                    end
                end
            end
        end
    end
    if unique then
        print(string.format("There are %d unique solutions in [%d, %d]", count, low, high))
    else
        print(string.format("There are %d non-unique solutions in [%d, %d]", count, low, high))
    end
end

fourSquare(1,7,true,true)
fourSquare(3,9,true,true)
fourSquare(0,9,false,false)
```

Output:

```txt
a       b       c       d       e       f       g
3       7       2       1       5       4       6
4       5       3       1       6       2       7
4       7       1       3       2       6       5
5       6       2       3       1       7       4
6       4       1       5       2       3       7
6       4       5       1       2       7       3
7       2       6       1       3       5       4
7       3       2       5       1       4       6
There are 8 unique solutions in [1, 7]
a       b       c       d       e       f       g
7       8       3       4       5       6       9
8       7       3       5       4       6       9
9       6       4       5       3       7       8
9       6       5       4       3       8       7
There are 4 unique solutions in [3, 9]
There are 2860 non-unique solutions in [0, 9]
```


## Modula-2

```modula2
MODULE FourSquare;
FROM Conversions IMPORT IntToStr;
FROM Terminal IMPORT *;

PROCEDURE WriteInt(num : INTEGER);
VAR str : ARRAY[0..16] OF CHAR;
BEGIN
    IntToStr(num,str);
    WriteString(str);
END WriteInt;

PROCEDURE four_square(low, high : INTEGER; unique, print : BOOLEAN);
VAR count : INTEGER;
VAR a, b, c, d, e, f, g : INTEGER;
VAR fp : INTEGER;
BEGIN
    count:=0;

    IF print THEN
        WriteString('a b c d e f g');
        WriteLn;
    END;
    FOR a:=low TO high DO
        FOR b:=low TO high DO
            IF unique AND (b=a) THEN CONTINUE; END;

            fp:=a+b;
            FOR c:=low TO high DO
                IF unique AND ((c=a) OR (c=b)) THEN CONTINUE; END;
                FOR d:=low TO high DO
                    IF unique AND ((d=a) OR (d=b) OR (d=c)) THEN CONTINUE; END;
                    IF fp # b+c+d THEN CONTINUE; END;

                    FOR e:=low TO high DO
                        IF unique AND ((e=a) OR (e=b) OR (e=c) OR (e=d)) THEN CONTINUE; END;
                        FOR f:=low TO high DO
                            IF unique AND ((f=a) OR (f=b) OR (f=c) OR (f=d) OR (f=e)) THEN CONTINUE; END;
                            IF fp # d+e+f THEN CONTINUE; END;

                            FOR g:=low TO high DO
                                IF unique AND ((g=a) OR (g=b) OR (g=c) OR (g=d) OR (g=e) OR (g=f)) THEN CONTINUE; END;
                                IF fp # f+g THEN CONTINUE; END;

                                INC(count);
                                IF print THEN
                                    WriteInt(a);
                                    WriteString(' ');
                                    WriteInt(b);
                                    WriteString(' ');
                                    WriteInt(c);
                                    WriteString(' ');
                                    WriteInt(d);
                                    WriteString(' ');
                                    WriteInt(e);
                                    WriteString(' ');
                                    WriteInt(f);
                                    WriteString(' ');
                                    WriteInt(g);
                                    WriteLn;
                                END;
                            END;
                        END;
                    END;
                END;
            END;
        END;
    END;
    IF unique THEN
        WriteString('There are ');
        WriteInt(count);
        WriteString(' unique solutions in [');
        WriteInt(low);
        WriteString(', ');
        WriteInt(high);
        WriteString(']');
        WriteLn;
    ELSE
        WriteString('There are ');
        WriteInt(count);
        WriteString(' non-unique solutions in [');
        WriteInt(low);
        WriteString(', ');
        WriteInt(high);
        WriteString(']');
        WriteLn;
    END;
END four_square;

BEGIN
    four_square(1,7,TRUE,TRUE);
    four_square(3,9,TRUE,TRUE);
    four_square(0,9,FALSE,FALSE);
    ReadChar; (* Wait so results can be viewed. *)
END FourSquare.
```


## Nim

Adapted from Rust version.

```nim
func isUnique(a, b, c, d, e, f, g: uint8): bool =
  a != b and a != c and a != d and a != e and a != f and a != g and
    b != c and b != d and b != e and b != f and b != g and
    c != d and c != e and c != f and c != g and
    d != e and d != f and d != f and
    e != f and e != g and
    f != g

func isSolution(a, b, c, d, e, f, g: uint8): bool =
  let sum = a + b
  sum == b + c + d and sum == d + e + f and sum == f + g

func fourSquares(l, h: uint8, unique: bool): seq[array[7, uint8]] =
  for a in l..h:
    for b in l..h:
      for c in l..h:
        for d in l..h:
          for e in l..h:
            for f in l..h:
              for g in l..h:
                if (not unique or isUnique(a, b, c, d, e, f, g)) and
                   isSolution(a, b, c, d, e, f, g):
                  result &= [a, b, c, d, e, f, g]

proc printFourSquares(l, h: uint8, unique = true) =
  let solutions = fourSquares(l, h, unique)

  if unique:
    for s in solutions:
      echo s

  echo solutions.len, (if unique: " " else: " non-"), "unique solutions in ",
     l, " to ", h, " range\n"

when isMainModule:
  printFourSquares(1, 7)
  printFourSquares(3, 9)
  printFourSquares(0, 9, unique = false)
```

Output:

```txt
[3, 7, 2, 1, 5, 4, 6]
[4, 5, 3, 1, 6, 2, 7]
[4, 7, 1, 3, 2, 6, 5]
[5, 6, 2, 3, 1, 7, 4]
[6, 4, 1, 5, 2, 3, 7]
[6, 4, 5, 1, 2, 7, 3]
[7, 2, 6, 1, 3, 5, 4]
[7, 3, 2, 5, 1, 4, 6]
8 unique solutions in 1 to 7 range

[7, 8, 3, 4, 5, 6, 9]
[8, 7, 3, 5, 4, 6, 9]
[9, 6, 4, 5, 3, 7, 8]
[9, 6, 5, 4, 3, 8, 7]
4 unique solutions in 3 to 9 range

2860 non-unique solutions in 0 to 9 range
```


## Pascal

{{works with|Free Pascal}}
There are so few solutions of 7 consecutive numbers,
so I used a modified version, to get all the expected solutions at once.

```pascal
program square4;
{$MODE DELPHI}
{$R+,O+}
const
  LoDgt = 0;
  HiDgt = 9;
type
  tchkset = set of LoDgt..HiDgt;
  tSol = record
           solMin : integer;
           solDat : array[1..7] of integer;
         end;

var
  sum,a,b,c,d,e,f,g,cnt,uniqueCount : NativeInt;
  sol : array of tSol;

procedure SolOut;
var
  i,j,mn: NativeInt;
Begin
  mn := 0;
  repeat
    writeln(mn:3,' ...',mn+6:3);
    For i := Low(sol) to High(sol) do
      with sol[i] do
        IF solMin = mn then
        Begin
          For j := 1 to 7 do
            write(solDat[j]:3);
          writeln;
        end;
    writeln;
    inc(mn);
  until mn > HiDgt-6;
end;

function CheckUnique:Boolean;
var
  i,sum,mn: NativeInt;
  chkset : tchkset;

Begin
  chkset:= [];
  include(chkset,a);include(chkset,b);include(chkset,c);
  include(chkset,d);include(chkset,e);include(chkset,f);
  include(chkset,g);
  sum := 0;
  For i := LoDgt to HiDgt do
    IF i in chkset then
      inc(sum);

  result := sum = 7;
  IF result then
  begin
    inc(uniqueCount);
    //find the lowest entry
    mn:= LoDgt;
    For i := LoDgt to HiDgt do
      IF i in chkset then
      Begin
        mn := i;
        BREAK;
      end;
    // are they consecutive
    For i := mn+1 to mn+6  do
      IF NOT(i in chkset) then
        EXIT;

    setlength(sol,Length(sol)+1);
    with sol[high(sol)] do
      Begin
        solMin:= mn;
        solDat[1]:= a;solDat[2]:= b;solDat[3]:= c;
        solDat[4]:= d;solDat[5]:= e;solDat[6]:= f;
        solDat[7]:= g;
      end;
  end;
end;

Begin
  cnt := 0;
  uniqueCount := 0;
  For a:= LoDgt to HiDgt do
  Begin
    For b := LoDgt to HiDgt do
    Begin
      sum := a+b;
      //a+b = b+c+d => a = c+d => d := a-c
      For c := a-LoDgt downto LoDgt do
      begin
        d := a-c;
        e := sum-d;
        IF e>HiDgt then
          e:= HiDgt;
        For e := e downto LoDgt do
          begin
          f := sum-e-d;
          IF f in [loDGt..Hidgt]then
          Begin
            g := sum-f;
            IF g in [loDGt..Hidgt]then
            Begin
              inc(cnt);
              CheckUnique;
            end;
          end;
        end;
      end;
    end;
  end;
  SolOut;
  writeln('       solution count for ',loDgt,' to ',HiDgt,' = ',cnt);
  writeln('unique solution count for ',loDgt,' to ',HiDgt,' = ',uniqueCount);
end.
```

Output:

```txt
  0 ...  6
  4  2  3  1  5  0  6
  5  1  3  2  4  0  6
  6  0  5  1  3  2  4
  6  0  4  2  3  1  5

  1 ...  7
  3  7  2  1  5  4  6
  4  5  3  1  6  2  7
  4  7  1  3  2  6  5
  5  6  2  3  1  7  4
  6  4  5  1  2  7  3
  6  4  1  5  2  3  7
  7  2  6  1  3  5  4
  7  3  2  5  1  4  6

  2 ...  8
  5  7  3  2  6  4  8
  5  8  3  2  4  7  6
  5  8  2  3  4  6  7
  6  7  4  2  3  8  5
  7  4  5  2  6  3  8
  7  6  4  3  2  8  5
  8  3  6  2  5  4  7
  8  4  6  2  3  7  5

  3 ...  9
  7  8  3  4  5  6  9
  8  7  3  5  4  6  9
  9  6  5  4  3  8  7
  9  6  4  5  3  7  8

       solution count for 0 to 9 = 2860
unique solution count for 0 to 9 = 192
```


## Perl

Relying on the modules `ntheory` and `Set::CrossProduct` to generate the tuples needed.
Both are supply results via iterators, particularly important in the latter case,
to avoid gobbling too much memory.

{{libheader|ntheory}}

```perl
use ntheory qw/forperm/;
use Set::CrossProduct;

sub four_sq_permute {
    my($list) = @_;
    my @solutions;
    forperm {
       @c = @$list[@_];
       push @solutions, [@c] if check(@c);
    } @$list;
    print +@solutions . " unique solutions found using: " . join(', ', @$list) . "\n";
    return @solutions;
}

sub four_sq_cartesian {
    my(@list) = @_;
    my @solutions;
    my $iterator = Set::CrossProduct->new( [(@list) x 7] );
    while( my $c = $iterator->get ) {
       push @solutions, [@$c] if check(@$c);
    }
    print +@solutions . " non-unique solutions found using: " . join(', ', @{@list[0]}) . "\n";
    return @solutions;
}

sub check {
    my(@c) = @_;
    $a = $c[0] + $c[1];
    $b = $c[1] + $c[2] + $c[3];
    $c = $c[3] + $c[4] + $c[5];
    $d = $c[5] + $c[6];
    $a == $b and $a == $c and $a == $d;
}

sub display {
    my(@solutions) = @_;
    my $fmt = "%2s " x 7 . "\n";
    printf $fmt, ('a'..'g');
    printf $fmt, @$_ for @solutions;
    print "\n";
}

display four_sq_permute( [1..7] );
display four_sq_permute( [3..9] );
display four_sq_permute( [8, 9, 11, 12, 17, 18, 20, 21] );
four_sq_cartesian( [0..9] );
```

Output:

```txt
8 unique solutions found using: 1, 2, 3, 4, 5, 6, 7
 a  b  c  d  e  f  g
 3  7  2  1  5  4  6
 4  5  3  1  6  2  7
 4  7  1  3  2  6  5
 5  6  2  3  1  7  4
 6  4  1  5  2  3  7
 6  4  5  1  2  7  3
 7  2  6  1  3  5  4
 7  3  2  5  1  4  6

4 unique solutions found using: 3, 4, 5, 6, 7, 8, 9
 a  b  c  d  e  f  g
 7  8  3  4  5  6  9
 8  7  3  5  4  6  9
 9  6  4  5  3  7  8
 9  6  5  4  3  8  7

8 unique solutions found using: 8, 9, 11, 12, 17, 18, 20, 21
 a  b  c  d  e  f  g
17 21  8  9 11 18 20
17 21  9  8 12 18 20
20 18  8 12  9 17 21
20 18 11  9  8 21 17
20 18 11  9 12 17 21
20 18 12  8  9 21 17
21 17  9 12  8 18 20
21 17 12  9 11 18 20

2860 non-unique solutions found using: 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
```


## Perl 6

Works with Rakudo 2016.12.

```perl6
sub four-squares ( @list, :$unique=1, :$show=1 ) {

    my @solutions;

    for $unique.&combos -> @c {
        @solutions.push: @c if [==]
          @c[0] + @c[1],
          @c[1] + @c[2] + @c[3],
          @c[3] + @c[4] + @c[5],
          @c[5] + @c[6];
    }

    say +@solutions, ($unique ?? ' ' !! ' non-'), "unique solutions found using {join(', ', @list)}.\n";

    my $f = "%{@list.max.chars}s";

    say join "\n", (('a'..'g').fmt: $f), @solutions».fmt($f), "\n" if $show;

    multi combos ( $ where so * ) { @list.combinations(7).map: |*.permutations }

    multi combos ( $ where not * ) { [X] @list xx 7 }
}

# TASK
four-squares( [1..7] );
four-squares( [3..9] );
four-squares( [8, 9, 11, 12, 17, 18, 20, 21] );
four-squares( [0..9], :unique(0), :show(0) );
```

Output:

```txt
8 unique solutions found using 1, 2, 3, 4, 5, 6, 7.

a b c d e f g
3 7 2 1 5 4 6
4 5 3 1 6 2 7
4 7 1 3 2 6 5
5 6 2 3 1 7 4
6 4 1 5 2 3 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4
7 3 2 5 1 4 6


4 unique solutions found using 3, 4, 5, 6, 7, 8, 9.

a b c d e f g
7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7


8 unique solutions found using 8, 9, 11, 12, 17, 18, 20, 21.

 a  b  c  d  e  f  g
17 21  8  9 11 18 20
20 18 11  9  8 21 17
17 21  9  8 12 18 20
20 18  8 12  9 17 21
20 18 12  8  9 21 17
21 17  9 12  8 18 20
20 18 11  9 12 17 21
21 17 12  9 11 18 20


2860 non-unique solutions found using 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.
```


## Picat

```Picat
import cp.

main =>
  puzzle_all(1, 7, true, Sol1),
  foreach(Sol in Sol1) println(Sol) end,
  nl,

  puzzle_all(3, 9, true, Sol2),
  foreach(Sol in Sol2) println(Sol) end,
  nl,

  puzzle_all(0, 9, false, Sol3),
  println(len=Sol3.len),
  nl.

puzzle_all(Min, Max, Distinct, LL) =>
    L = [A,B,C,D,E,F,G],
    L :: Min..Max,
    if Distinct then
      all_different(L)
    else
      true
    end,
    T #= A+B,
    T #= B+C+D,
    T #= D+E+F,
    T #= F+G,
    % Another approach:
    % Sums = $[A+B,B+C+D,D+E+F,F+G],
    % foreach(I in 2..Sums.len) Sums[I] #= Sums[I-1] end,
    LL = solve_all(L).

```


Test:

```txt

Picat> main
[3,7,2,1,5,4,6]
[4,5,3,1,6,2,7]
[4,7,1,3,2,6,5]
[5,6,2,3,1,7,4]
[6,4,1,5,2,3,7]
[6,4,5,1,2,7,3]
[7,2,6,1,3,5,4]
[7,3,2,5,1,4,6]

[7,8,3,4,5,6,9]
[8,7,3,5,4,6,9]
[9,6,4,5,3,7,8]
[9,6,5,4,3,8,7]

len = 2860
```


## Phix

```Phix
integer solutions

procedure check(sequence set, bool show)
    integer {a,b,c,d,e,f,g} = set, ab = a+b
    if ab=b+d+c and ab=d+e+f and ab=f+g then
        solutions += 1
        if show then
            ?set
        end if
    end if
end procedure

procedure foursquares(integer lo, integer hi, bool uniq, bool show)
sequence set = repeat(lo,7)
    solutions = 0
    if uniq then
        for i=1 to 7 do
            set[i] = lo+i-1
        end for
        for i=1 to factorial(7) do
            check(permute(i,set),show)
        end for
    else
        integer done = 0
        while not done do
            check(set,show)
            for i=1 to 7 do
                set[i] += 1
                if set[i]<=hi then exit end if
                if i=7 then
                    done = 1
                    exit
                end if
                set[i] = lo
            end for
        end while
    end if
    printf(1,"%d solutions\n",solutions)
end procedure
foursquares(1,7,uniq:=True,show:=True)
foursquares(3,9,True,True)
foursquares(0,9,False,False)
```

Output:

```txt
{6,4,5,1,2,7,3}
{3,7,2,1,5,4,6}
{6,4,1,5,2,3,7}
{4,7,1,3,2,6,5}
{7,3,2,5,1,4,6}
{5,6,2,3,1,7,4}
{4,5,3,1,6,2,7}
{7,2,6,1,3,5,4}
8 solutions
{7,8,3,4,5,6,9}
{8,7,3,5,4,6,9}
{9,6,4,5,3,7,8}
{9,6,5,4,3,8,7}
4 solutions
2860 solutions
```


## PL/SQL

{{works with|Oracle}}

```plsql
create table allints (v number);
create table results
(
a number,
b number,
c number,
d number,
e number,
f number,
g number
);

create or replace procedure foursquares(lo number,hi number,uniq boolean,show boolean)
as
    a number;
    b number;
    c number;
    d number;
    e number;
    f number;
    g number;
    out_line varchar2(2000);

    cursor results_cur is
    select
       a,
       b,
       c,
       d,
       e,
       f,
       g
    from
        results
    order by
        a,b,c,d,e,f,g;

    results_rec results_cur%rowtype;

    solutions number;
    uorn varchar2(2000);
begin
    solutions := 0;
    delete from allints;
    delete from results;
    for i in lo..hi loop
        insert into allints values (i);
    end loop;
    commit;

    if uniq = TRUE then
        insert into results
            select
                a.v a,
                b.v b,
                c.v c,
                d.v d,
                e.v e,
                f.v f,
                g.v g
            from
                allints a, allints b, allints c,allints d,
                allints e, allints f, allints g
            where
                a.v not in (b.v,c.v,d.v,e.v,f.v,g.v) and
                b.v not in (c.v,d.v,e.v,f.v,g.v) and
                c.v not in (d.v,e.v,f.v,g.v) and
                d.v not in (e.v,f.v,g.v) and
                e.v not in (f.v,g.v) and
                f.v not in (g.v) and
                a.v = c.v + d.v and
                g.v = d.v + e.v and
                b.v = e.v + f.v - c.v
            order by
                a,b,c,d,e,f,g;
        uorn := ' unique solutions in ';
    else
        insert into results
            select
                a.v a,
                b.v b,
                c.v c,
                d.v d,
                e.v e,
                f.v f,
                g.v g
            from
                allints a, allints b, allints c,allints d,
                allints e, allints f, allints g
            where
                a.v = c.v + d.v and
                g.v = d.v + e.v and
                b.v = e.v + f.v - c.v
            order by
                a,b,c,d,e,f,g;
        uorn := ' non-unique solutions in ';
    end if;
    commit;

    open results_cur;
    loop
        fetch results_cur into results_rec;
        exit when results_cur%notfound;
        a := results_rec.a;
        b := results_rec.b;
        c := results_rec.c;
        d := results_rec.d;
        e := results_rec.e;
        f := results_rec.f;
        g := results_rec.g;

        solutions := solutions + 1;
        if show = TRUE then
            out_line := to_char(a) || ' ';
            out_line := out_line || ' ' || to_char(b) || ' ';
            out_line := out_line || ' ' || to_char(c) || ' ';
            out_line := out_line || ' ' || to_char(d) || ' ';
            out_line := out_line || ' ' || to_char(e) || ' ';
            out_line := out_line || ' ' || to_char(f) ||' ';
            out_line := out_line || ' ' || to_char(g);
        end if;

        dbms_output.put_line(out_line);
    end loop;
    close results_cur;
    out_line := to_char(solutions) || uorn;
    out_line := out_line || to_char(lo) || ' to ' || to_char(hi);
    dbms_output.put_line(out_line);

end;
/
```

Output

```txt
SQL> execute foursquares(1,7,TRUE,TRUE);
3  7  2  1  5  4  6
4  5  3  1  6  2  7
4  7  1  3  2  6  5
5  6  2  3  1  7  4
6  4  1  5  2  3  7
6  4  5  1  2  7  3
7  2  6  1  3  5  4
7  3  2  5  1  4  6
8 unique solutions in 1 to 7

PL/SQL procedure successfully completed.

SQL> execute foursquares(3,9,TRUE,TRUE);
7  8  3  4  5  6  9
8  7  3  5  4  6  9
9  6  4  5  3  7  8
9  6  5  4  3  8  7
4 unique solutions in 3 to 9

PL/SQL procedure successfully completed.

SQL> execute foursquares(0,9,FALSE,FALSE);
2860 non-unique solutions in 0 to 9

PL/SQL procedure successfully completed.
```


## Prolog

Works with SWI-Prolog 7.5.8

```Prolog
:- use_module(library(clpfd)).

% main predicate
my_sum(Min, Max, Top, LL):-
    L = [A,B,C,D,E,F,G],
    L ins Min..Max,
    (   Top == 0
    ->  all_distinct(L)
    ;    true),
    R #= A+B,
    R #= B+C+D,
    R #= D+E+F,
    R #= F+G,
    setof(L, labeling([ff], L), LL).


my_sum_1(Min, Max) :-
    my_sum(Min, Max, 0, LL),
    maplist(writeln, LL).

my_sum_2(Min, Max, Len) :-
    my_sum(Min, Max, 1, LL),
    length(LL, Len).
```

Output

```txt
 ?- my_sum_1(1,7).
[3,7,2,1,5,4,6]
[4,5,3,1,6,2,7]
[4,7,1,3,2,6,5]
[5,6,2,3,1,7,4]
[6,4,1,5,2,3,7]
[6,4,5,1,2,7,3]
[7,2,6,1,3,5,4]
[7,3,2,5,1,4,6]
true.

 ?- my_sum_1(3,9).
[7,8,3,4,5,6,9]
[8,7,3,5,4,6,9]
[9,6,4,5,3,7,8]
[9,6,5,4,3,8,7]
true.

 ?- my_sum_2(0,9,N).
N = 2860.
```


## Python

### Procedural

#### Itertools

```Python
import itertools

def all_equal(a,b,c,d,e,f,g):
    return a+b == b+c+d == d+e+f == f+g

def foursquares(lo,hi,unique,show):
    solutions = 0
    if unique:
        uorn = "unique"
        citer = itertools.combinations(range(lo,hi+1),7)
    else:
        uorn = "non-unique"
        citer =  itertools.combinations_with_replacement(range(lo,hi+1),7)

    for c in citer:
            for p in set(itertools.permutations(c)):
                if all_equal(*p):
                    solutions += 1
                    if show:
                        print str(p)[1:-1]

    print str(solutions)+" "+uorn+" solutions in "+str(lo)+" to "+str(hi)
    print
```

Output

```txt
foursquares(1,7,True,True)
4, 5, 3, 1, 6, 2, 7
3, 7, 2, 1, 5, 4, 6
5, 6, 2, 3, 1, 7, 4
4, 7, 1, 3, 2, 6, 5
6, 4, 5, 1, 2, 7, 3
7, 3, 2, 5, 1, 4, 6
7, 2, 6, 1, 3, 5, 4
6, 4, 1, 5, 2, 3, 7
8 unique solutions in 1 to 7


foursquares(3,9,True,True)
7, 8, 3, 4, 5, 6, 9
9, 6, 4, 5, 3, 7, 8
8, 7, 3, 5, 4, 6, 9
9, 6, 5, 4, 3, 8, 7
4 unique solutions in 3 to 9


foursquares(0,9,False,False)
2860 non-unique solutions in 0 to 9
```


#### Generators

Faster solution without itertools

```Python

def foursquares(lo,hi,unique,show):

    def acd_iter():
        """
        Iterates through all the possible valid values of
        a, c, and d.

        a = c + d
        """
        for c in range(lo,hi+1):
            for d in range(lo,hi+1):
                if (not unique) or (c <> d):
                    a = c + d
                    if a >= lo and a <= hi:
                        if (not unique) or (c <> 0 and d <> 0):
                            yield (a,c,d)

    def ge_iter():
        """
        Iterates through all the possible valid values of
        g and e.

        g = d + e
        """
        for e in range(lo,hi+1):
            if (not unique) or (e not in (a,c,d)):
                g = d + e
                if g >= lo and g <= hi:
                    if (not unique) or (g not in (a,c,d,e)):
                        yield (g,e)

    def bf_iter():
        """
        Iterates through all the possible valid values of
        b and f.

        b = e + f - c
        """
        for f in range(lo,hi+1):
            if (not unique) or (f not in (a,c,d,g,e)):
                b = e + f - c
                if b >= lo and b <= hi:
                    if (not unique) or (b not in (a,c,d,g,e,f)):
                        yield (b,f)

    solutions = 0
    acd_itr = acd_iter()
    for acd in acd_itr:
        a,c,d = acd
        ge_itr = ge_iter()
        for ge in ge_itr:
            g,e = ge
            bf_itr = bf_iter()
            for bf in bf_itr:
                b,f = bf
                solutions += 1
                if show:
                    print str((a,b,c,d,e,f,g))[1:-1]
    if unique:
        uorn = "unique"
    else:
        uorn = "non-unique"

    print str(solutions)+" "+uorn+" solutions in "+str(lo)+" to "+str(hi)
    print
```

Output:

```txt
foursquares(1,7,True,True)
4, 7, 1, 3, 2, 6, 5
6, 4, 1, 5, 2, 3, 7
3, 7, 2, 1, 5, 4, 6
5, 6, 2, 3, 1, 7, 4
7, 3, 2, 5, 1, 4, 6
4, 5, 3, 1, 6, 2, 7
6, 4, 5, 1, 2, 7, 3
7, 2, 6, 1, 3, 5, 4
8 unique solutions in 1 to 7


foursquares(3,9,True,True)
7, 8, 3, 4, 5, 6, 9
8, 7, 3, 5, 4, 6, 9
9, 6, 4, 5, 3, 7, 8
9, 6, 5, 4, 3, 8, 7
4 unique solutions in 3 to 9


foursquares(0,9,False,False)
2860 non-unique solutions in 0 to 9
```


### Functional

Translated from Haskell.
Translated from JavaScript.
Works with Python 3.7.

```python
'''4-rings or 4-squares puzzle'''

from itertools import chain


# rings :: noRepeatedDigits -> DigitList -> Lists of solutions
# rings :: Bool -> [Int] -> [[Int]]
def rings(uniq):
    '''Sets of unique or non-unique integer values
       (drawn from the `digits` argument)
       for each of the seven names [a..g] such that:
       (a + b) == (b + c + d) == (d + e + f) == (f + g)
    '''
    def go(digits):
        ns = sorted(digits, reverse=True)
        h = ns[0]

        # CENTRAL DIGIT :: d
        def central(d):
            xs = list(filter(lambda x: h >= (d + x), ns))

            # LEFT NEIGHBOUR AND LEFTMOST :: c and a
            def left(c):
                a = c + d
                if a > h:
                    return []
                else:
                    # RIGHT NEIGHBOUR AND RIGHTMOST :: e and g
                    def right(e):
                        g = d + e
                        if ((g > h) or (uniq and (g == c))):
                            return []
                        else:
                            agDelta = a - g
                            bfs = difference(ns)(
                                [d, c, e, g, a]
                            ) if uniq else ns

                            # MID LEFT AND RIGHT :: b and f
                            def midLeftRight(b):
                                f = b + agDelta
                                return [[a, b, c, d, e, f, g]] if (
                                    (f in bfs) and (
                                        (not uniq) or (
                                            f not in [a, b, c, d, e, g]
                                        )
                                    )
                                ) else []

    # CANDIDATE DIGITS BOUND TO POSITIONS [a .. g] --------

                            return concatMap(midLeftRight)(bfs)

                    return concatMap(right)(
                        difference(xs)([d, c, a]) if uniq else ns
                    )

            return concatMap(left)(
                delete(d)(xs) if uniq else ns
            )

        return concatMap(central)(ns)

    return lambda digits: go(digits) if digits else []


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Testing unique digits [1..7], [3..9] and unrestricted digits'''

    print(main.__doc__ + ':\n')
    print(unlines(map(
        lambda tpl: '\nrings' + repr(tpl) + ':\n\n' + unlines(
            map(repr, uncurry(rings)(*tpl))
        ), [
            (True, enumFromTo(1)(7)),
            (True, enumFromTo(3)(9))
        ]
    )))
    tpl = (False, enumFromTo(0)(9))
    print(
        '\n\nlen(rings' + repr(tpl) + '):\n\n' +
        str(len(uncurry(rings)(*tpl)))
    )


# GENERIC -------------------------------------------------

# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).
    '''
    return lambda xs: list(
        chain.from_iterable(map(f, xs))
    )


# delete :: Eq a => a -> [a] -> [a]
def delete(x):
    '''xs with the first of any instances of x removed.'''
    def go(xs):
        xs.remove(x)
        return xs
    return lambda xs: go(list(xs)) if (
        x in xs
    ) else list(xs)


#  difference :: Eq a => [a] -> [a] -> [a]
def difference(xs):
    '''All elements of ys except any also found in xs'''
    def go(ys):
        s = set(ys)
        return [x for x in xs if x not in s]
    return lambda ys: go(ys)


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# uncurry :: (a -> b -> c) -> ((a, b) -> c)
def uncurry(f):
    '''A function over a pair of arguments,
       derived from a vanilla or curried function.
    '''
    return lambda x, y: f(x)(y)


# unlines :: [String] -> String
def unlines(xs):
    '''A single string formed by the intercalation
       of a list of strings with the newline character.
    '''
    return '\n'.join(xs)


# MAIN ---
if __name__ == '__main__':
    main()
```

Output:

```txt
Testing unique digits [1..7], [3..9] and unrestricted digits:

rings(True, [1, 2, 3, 4, 5, 6, 7]):

[7, 3, 2, 5, 1, 4, 6]
[6, 4, 1, 5, 2, 3, 7]
[5, 6, 2, 3, 1, 7, 4]
[4, 7, 1, 3, 2, 6, 5]
[7, 2, 6, 1, 3, 5, 4]
[6, 4, 5, 1, 2, 7, 3]
[4, 5, 3, 1, 6, 2, 7]
[3, 7, 2, 1, 5, 4, 6]

rings(True, [3, 4, 5, 6, 7, 8, 9]):

[9, 6, 4, 5, 3, 7, 8]
[8, 7, 3, 5, 4, 6, 9]
[9, 6, 5, 4, 3, 8, 7]
[7, 8, 3, 4, 5, 6, 9]


len(rings(False, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9])):

2860
```


## REXX

### fast version

This REXX version is faster than the more idiomatic version,
but is longer (statement-wise) and a bit easier to read (visualize).

```rexx
/*REXX pgm solves the 4-rings puzzle,  where letters represent unique (or not) digits). */
arg LO HI unique show .                          /*the  ARG  statement capitalizes args.*/
if LO=='' | LO==","  then LO=1                   /*Not specified?  Then use the default.*/
if HI=='' | HI==","  then HI=7                   /* "      "         "   "   "     "    */
if unique=='' | unique==',' | unique=='UNIQUE'  then unique=1  /*unique letter solutions*/
                                                else unique=0  /*non-unique        "    */
if   show=='' |   show==',' |   show=='SHOW'    then show=1    /*noshow letter solutions*/
                                                else show=0    /*  show    "       "    */
w=max(3, length(LO), length(HI) )                /*maximum width of any number found.   */
bar=copies('═', w)                               /*define a horizontal bar (for title). */
times=HI - LO + 1                                /*calculate number of times to loop.   */
#=0                                              /*number of solutions found (so far).  */
       do a=LO     for times
          do b=LO  for times
          if unique  then  if b==a  then  iterate
             do c=LO  for times
             if unique  then  do;  if c==a  then  iterate
                                   if c==b  then  iterate
                              end
                do d=LO  for times
                if unique  then  do;  if d==a  then  iterate
                                      if d==b  then  iterate
                                      if d==c  then  iterate
                                 end
                   do e=LO  for times
                   if unique  then  do;  if e==a  then  iterate
                                         if e==b  then  iterate
                                         if e==c  then  iterate
                                         if e==d  then  iterate
                                    end
                      do f=LO  for times
                      if unique  then  do;  if f==a  then  iterate
                                            if f==b  then  iterate
                                            if f==c  then  iterate
                                            if f==d  then  iterate
                                            if f==e  then  iterate
                                       end
                         do g=LO  for times
                         if unique  then  do;  if g==a  then  iterate
                                               if g==b  then  iterate
                                               if g==c  then  iterate
                                               if g==d  then  iterate
                                               if g==e  then  iterate
                                               if g==f  then  iterate
                                          end
                         sum=a+b
                         if   f+g\==sum  then  iterate
                         if b+c+d\==sum  then  iterate
                         if d+e+f\==sum  then  iterate
                         #=# + 1                          /*bump the count of solutions.*/
                         if #==1  then call align  'a',  'b',  'c',  'd',  'e',  'f',  'g'
                         if #==1  then call align  bar,  bar,  bar,  bar,  bar,  bar,  bar
                                       call align   a,    b,    c,    d,    e,    f,    g
                         end   /*g*/
                      end      /*f*/
                   end         /*e*/
                end            /*d*/
             end               /*c*/
          end                  /*b*/
       end                     /*a*/
say
                 _= ' non-unique'
if  unique  then _= ' unique '
say #  _  'solutions found.'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
align: parse arg a1,a2,a3,a4,a5,a6,a7
       if show  then say left('',9)  center(a1,w) center(a2,w) center(a3,w) center(a4,w),
                                     center(a5,w) center(a6,w) center(a7,w)
       return
```

{{out|output|text=  when using the default inputs:   <tt>   1   7 </tt>}}

```txt

           a   b   c   d   e   f   g
          ═══ ═══ ═══ ═══ ═══ ═══ ═══
           3   7   2   1   5   4   6
           4   5   3   1   6   2   7
           4   7   1   3   2   6   5
           5   6   2   3   1   7   4
           6   4   1   5   2   3   7
           6   4   5   1   2   7   3
           7   2   6   1   3   5   4
           7   3   2   5   1   4   6

8  unique  solutions found.
```

{{out|output|text=  when using the input of:   <tt>   3   9 </tt>}}

```txt
           a   b   c   d   e   f   g
          ═══ ═══ ═══ ═══ ═══ ═══ ═══
           7   8   3   4   5   6   9
           8   7   3   5   4   6   9
           9   6   4   5   3   7   8
           9   6   5   4   3   8   7

4  unique  solutions found.
```

{{out|output|text=  when using the input of:   <tt>   0   9   non-unique   noshow </tt>}}

```txt
2860  non-unique solutions found.
```


### idiomatic version

This REXX version is slower than the faster version
(because of the multiple <big> '''if''' </big> clauses.

Note that the REXX language doesn't have short-circuits
(when executing multiple clauses in <big> '''if''' </big>
(and other) statements.

```rexx
/*REXX pgm solves the 4-rings puzzle,  where letters represent unique (or not) digits). */
arg LO HI unique show .                          /*the  ARG  statement capitalizes args.*/
if LO=='' | LO==","  then LO=1                   /*Not specified?  Then use the default.*/
if HI=='' | HI==","  then HI=7                   /* "      "         "   "   "     "    */
if unique=='' | unique==',' | unique=='UNIQUE'  then u=1       /*unique letter solutions*/
                                                else u=0       /*non-unique        "    */
if   show=='' |   show==',' |   show=='SHOW'    then show=1    /*noshow letter solutions*/
                                                else show=0    /*  show    "       "    */
w=max(3, length(LO), length(HI) )                /*maximum width of any number found.   */
bar=copies('═', w)                               /*define a horizontal bar (for title). */
times=HI - LO + 1                                /*calculate number of times to loop.   */
#=0                                              /*number of solutions found (so far).  */
     do       a=LO  for times
      do      b=LO  for times;  if u  then  if b==a                           then iterate
       do     c=LO  for times;  if u  then  if c==a|c==b                      then iterate
        do    d=LO  for times;  if u  then  if d==a|d==b|d==c                 then iterate
         do   e=LO  for times;  if u  then  if e==a|e==b|e==c|e==d            then iterate
          do  f=LO  for times;  if u  then  if f==a|f==b|f==c|f==d|f==e       then iterate
           do g=LO  for times;  if u  then  if g==a|g==b|g==c|g==d|g==e|g==f  then iterate
           sum=a+b
           if f+g==sum & b+c+d==sum & d+e+f==sum  then #=#+1      /*bump # of solutions.*/
                                                  else iterate    /*sum not equal, no─go*/
           if #==1  then call align  'a',  'b',  'c',  'd',  'e',  'f',  'g'
           if #==1  then call align  bar,  bar,  bar,  bar,  bar,  bar,  bar
                         call align   a,    b,    c,    d,    e,    f,    g
           end   /*g*/                                        /*for 1st time, show title*/
          end    /*f*/
         end     /*e*/
        end      /*d*/
       end       /*c*/
      end        /*b*/
     end         /*a*/
say
           _= ' non-unique'
if u  then _= ' unique '
say #  _  'solutions found.'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
align: parse arg a1,a2,a3,a4,a5,a6,a7
       if show  then say  left('',9)  center(a1,w) center(a2,w) center(a3,w) center(a4,w),
                                      center(a5,w) center(a6,w) center(a7,w)
       return
```

Output:

_identical to the faster REXX version_


## Ruby

```ruby
def four_squares(low, high, unique=true, show=unique)
  f = -> (a,b,c,d,e,f,g) {[a+b, b+c+d, d+e+f, f+g].uniq.size == 1}
  if unique
    uniq = "unique"
    solutions = [*low..high].permutation(7).select{|ary| f.call(*ary)}
  else
    uniq = "non-unique"
    solutions = [*low..high].repeated_permutation(7).select{|ary| f.call(*ary)}
  end
  if show
    puts " " + [*"a".."g"].join("  ")
    solutions.each{|ary| p ary}
  end
  puts "#{solutions.size} #{uniq} solutions in #{low} to #{high}"
  puts
end

[[1,7], [3,9]].each do |low, high|
  four_squares(low, high)
end
four_squares(0, 9, false)
```

Output:

```txt
 a  b  c  d  e  f  g
[3, 7, 2, 1, 5, 4, 6]
[4, 5, 3, 1, 6, 2, 7]
[4, 7, 1, 3, 2, 6, 5]
[5, 6, 2, 3, 1, 7, 4]
[6, 4, 1, 5, 2, 3, 7]
[6, 4, 5, 1, 2, 7, 3]
[7, 2, 6, 1, 3, 5, 4]
[7, 3, 2, 5, 1, 4, 6]
8 unique solutions in 1 to 7

 a  b  c  d  e  f  g
[7, 8, 3, 4, 5, 6, 9]
[8, 7, 3, 5, 4, 6, 9]
[9, 6, 4, 5, 3, 7, 8]
[9, 6, 5, 4, 3, 8, 7]
4 unique solutions in 3 to 9

2860 non-unique solutions in 0 to 9
```


## Rust

```rust
#![feature(inclusive_range_syntax)]

fn is_unique(a: u8, b: u8, c: u8, d: u8, e: u8, f: u8, g: u8) -> bool {
    a != b && a != c && a != d && a != e && a != f && a != g &&
    b != c && b != d && b != e && b != f && b != g &&
    c != d && c != e && c != f && c != g &&
    d != e && d != f && d != g &&
    e != f && e != g &&
    f != g
}

fn is_solution(a: u8, b: u8, c: u8, d: u8, e: u8, f: u8, g: u8) -> bool {
    a + b == b + c + d &&
        b + c + d == d + e + f &&
        d + e + f == f + g
}

fn four_squares(low: u8, high: u8, unique: bool) -> Vec<Vec<u8>> {
    let mut results: Vec<Vec<u8>> = Vec::new();

    for a in low..=high {
        for b in low..=high {
            for c in low..=high {
                for d in low..=high {
                    for e in low..=high {
                        for f in low..=high {
                            for g in low..=high {
                                if (!unique || is_unique(a, b, c, d, e, f, g)) &&
                                    is_solution(a, b, c, d, e, f, g) {
                                    results.push(vec![a, b, c, d, e, f, g]);
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    results
}

fn print_results(solutions: &Vec<Vec<u8>>) {
    for solution in solutions {
        println!("{:?}", solution)
    }
}

fn print_results_summary(solutions: usize, low: u8, high: u8, unique: bool) {
    let uniqueness = if unique {
        "unique"
    } else {
        "non-unique"
    };
    println!("{} {} solutions in {} to {} range", solutions, uniqueness, low, high)
}

fn uniques(low: u8, high: u8) {
    let solutions = four_squares(low, high, true);
    print_results(&solutions);
    print_results_summary(solutions.len(), low, high, true);
}

fn nonuniques(low: u8, high: u8) {
    let solutions = four_squares(low, high, false);
    print_results_summary(solutions.len(), low, high, false);
}

fn main() {
    uniques(1, 7);
    println!();
    uniques(3, 9);
    println!();
    nonuniques(0, 9);
}
```

Output:

```txt
[3, 7, 2, 1, 5, 4, 6]
[4, 5, 3, 1, 6, 2, 7]
[4, 7, 1, 3, 2, 6, 5]
[5, 6, 2, 3, 1, 7, 4]
[6, 4, 1, 5, 2, 3, 7]
[6, 4, 5, 1, 2, 7, 3]
[7, 2, 6, 1, 3, 5, 4]
[7, 3, 2, 5, 1, 4, 6]
8 unique solutions in 1 to 7 range

[7, 8, 3, 4, 5, 6, 9]
[8, 7, 3, 5, 4, 6, 9]
[9, 6, 4, 5, 3, 7, 8]
[9, 6, 5, 4, 3, 8, 7]
4 unique solutions in 3 to 9 range

2860 non-unique solutions in 0 to 9 range
```


## Scala

Translated from Java

```scala
object FourRings {
  def isValid(unique: Boolean, needle: Integer, haystack: Integer*): Boolean
  = !unique || !haystack.contains(needle)

  def fourSquare(low: Int, high: Int, unique: Boolean, print: Boolean): Unit = {
    var count = 0
    if (print) {
      println("a b c d e f g")
    }
    (low to high).foreach(a => (low to high).foreach(b => if (isValid(unique, a, b)) {
      val fp = a + b
      (low to high).foreach(c => if (isValid(unique, c, a, b)) {
        (low to high).foreach(d => if (isValid(unique, d, a, b, c) && fp == b + c + d) {
          (low to high).foreach(e => if (isValid(unique, e, a, b, c, d)) {
            (low to high).foreach(f => if (isValid(unique, f, a, b, c, d, e) && fp == d + e + f) {
              (low to high).foreach(g => if (isValid(unique, g, a, b, c, d, e, f) && fp == f + g) {
                count = count + 1
                if (print) {
                  printf("%d %d %d %d %d %d %d%n", a, b, c, d, e, f, g)
                }
              })
            })
          })
        })
      })
    }))
    if (unique) {
      printf("There are %d unique solutions in [%d, %d]%n", count, low, high)
    } else {
      printf("There are %d non-unique solutions in [%d, %d]%n", count, low, high)
    }
  }

  def main(args: Array[String]): Unit = {
    fourSquare(1, 7, unique = true, print = true)
    fourSquare(3, 9, unique = true, print = true)
    fourSquare(0, 9, unique = false, print = false)
  }
}
```

Output:

```txt
a b c d e f g
3 7 2 1 5 4 6
4 5 3 1 6 2 7
4 7 1 3 2 6 5
5 6 2 3 1 7 4
6 4 1 5 2 3 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4
7 3 2 5 1 4 6
There are 8 unique solutions in [1, 7]
a b c d e f g
7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7
There are 4 unique solutions in [3, 9]
There are 2860 non-unique solutions in [0, 9]
```


## Scheme

```scheme
(import (scheme base)
        (scheme write)
        (srfi 1))

;; return all combinations of size elements from given set
(define (combinations size set unique?)
  (if (zero? size)
    (list '())
    (let loop ((base-combns (combinations (- size 1) set unique?))
               (results '())
               (items set))
      (cond ((null? base-combns) ; end, as no base-combinations to process
             results)
            ((null? items)       ; check next base-combination
             (loop (cdr base-combns)
                   results
                   set))
            ((and unique?        ; ignore if wanting list unique
                  (member (car items) (car base-combns) =))
             (loop base-combns
                   results
                   (cdr items)))
            (else                ; keep the new combination
              (loop base-combns
                    (cons (cons (car items) (car base-combns))
                          results)
                    (cdr items)))))))

;; checks if all 4 sums are the same
(define (solution? a b c d e f g)
  (= (+ a b)
     (+ b c d)
     (+ d e f)
     (+ f g)))

;; Tasks
(display "Solutions: LOW=1 HIGH=7\n")
(display (filter (lambda (combination) (apply solution? combination))
                 (combinations 7 (iota 7 1) #t))) (newline)

(display "Solutions: LOW=3 HIGH=9\n")
(display (filter (lambda (combination) (apply solution? combination))
                 (combinations 7 (iota 7 3) #t))) (newline)

(display "Solution count: LOW=0 HIGH=9 non-unique\n")
(display (count (lambda (combination) (apply solution? combination))
                (combinations 7 (iota 10 0) #f))) (newline)
```

Output:

```txt
Solutions: LOW=1 HIGH=7
(
    (4 5 3 1 6 2 7)
    (6 4 1 5 2 3 7)
    (3 7 2 1 5 4 6)
    (7 3 2 5 1 4 6)
    (4 7 1 3 2 6 5)
    (7 2 6 1 3 5 4)
    (5 6 2 3 1 7 4)
    (6 4 5 1 2 7 3)
)
Solutions: LOW=3 HIGH=9
((7 8 3 4 5 6 9) (8 7 3 5 4 6 9) (9 6 4 5 3 7 8) (9 6 5 4 3 8 7))
Solution count: LOW=0 HIGH=9 non-unique
2860
```


## Sidef

Translated from Perl 6

```ruby
func four_squares (list, unique=true, show=true) {

    var solutions = []

    func check(c) {
        solutions << c if ([
            c[0] + c[1],
            c[1] + c[2] + c[3],
            c[3] + c[4] + c[5],
            c[5] + c[6],
        ].uniq.len == 1)
    }

    if (unique) {
        list.combinations(7, {|*a|
            a.permutations { |*c|
                check(c)
            }
        })
    } else {
        7.of { list }.cartesian {|*c|
            check(c)
        }
    }

    say (solutions.len,
        (unique ? ' ' : ' non-'),
        "unique solutions found using #{list.join(', ')}.\n")

    if (show) {
        var f = "%#{list.max.len+1}s"
        say ("\n".join(
                ('a'..'g').map{f % _}.join,
                solutions.map{ .map{f % _}.join }...
        ), "\n")
    }
}

# TASK
four_squares(@(1..7))
four_squares(@(3..9))
four_squares([8, 9, 11, 12, 17, 18, 20, 21])
four_squares(@(0..9), unique: false, show: false)
```

Output:

```txt
8 unique solutions found using 1, 2, 3, 4, 5, 6, 7.

 a b c d e f g
 3 7 2 1 5 4 6
 4 5 3 1 6 2 7
 4 7 1 3 2 6 5
 5 6 2 3 1 7 4
 6 4 1 5 2 3 7
 6 4 5 1 2 7 3
 7 2 6 1 3 5 4
 7 3 2 5 1 4 6

4 unique solutions found using 3, 4, 5, 6, 7, 8, 9.

 a b c d e f g
 7 8 3 4 5 6 9
 8 7 3 5 4 6 9
 9 6 4 5 3 7 8
 9 6 5 4 3 8 7

8 unique solutions found using 8, 9, 11, 12, 17, 18, 20, 21.

  a  b  c  d  e  f  g
 17 21  8  9 11 18 20
 20 18 11  9  8 21 17
 17 21  9  8 12 18 20
 20 18  8 12  9 17 21
 20 18 12  8  9 21 17
 21 17  9 12  8 18 20
 20 18 11  9 12 17 21
 21 17 12  9 11 18 20

2860 non-unique solutions found using 0, 1, 2, 3, 4, 5, 6, 7, 8, 9.
```


## Simula

```simula
BEGIN

    INTEGER PROCEDURE GETCOMBS(LOW, HIGH, UNIQUE, COMBS);
        INTEGER LOW, HIGH;
        INTEGER ARRAY COMBS;
        BOOLEAN UNIQUE;
    BEGIN
        INTEGER A, B, C, D, E, F, G;
        INTEGER NUM;

        BOOLEAN PROCEDURE ISUNIQUE(A, B, C, D, E, F, G);
            INTEGER A, B, C, D, E, F, G;
        BEGIN
            INTEGER ARRAY DATA(LOW:HIGH);
            INTEGER I;

            FOR I := LOW STEP 1 UNTIL HIGH DO
                DATA(I) := -1;

            FOR I := A, B, C, D, E, F, G DO
              IF DATA(I) = -1
                  THEN DATA(I) := 1
                  ELSE GOTO L;

            ISUNIQUE := TRUE;
        L:
        END;

        PROCEDURE ADDCOMB;
        BEGIN
            NUM := NUM + 1;
            COMBS(NUM, LOW + 0) := A;
            COMBS(NUM, LOW + 1) := B;
            COMBS(NUM, LOW + 2) := C;
            COMBS(NUM, LOW + 3) := D;
            COMBS(NUM, LOW + 4) := E;
            COMBS(NUM, LOW + 5) := F;
            COMBS(NUM, LOW + 6) := G;
        END;

        FOR A := LOW STEP 1 UNTIL HIGH DO
        FOR B := LOW STEP 1 UNTIL HIGH DO
        FOR C := LOW STEP 1 UNTIL HIGH DO
        FOR D := LOW STEP 1 UNTIL HIGH DO
        FOR E := LOW STEP 1 UNTIL HIGH DO
        FOR F := LOW STEP 1 UNTIL HIGH DO
        FOR G := LOW STEP 1 UNTIL HIGH DO
        BEGIN
            IF VALIDCOMB(A, B, C, D, E, F, G) THEN
            BEGIN
                IF UNIQUE THEN
                    BEGIN IF ISUNIQUE(A, B, C, D, E, F, G) THEN ADDCOMB END
                ELSE ADDCOMB;
            END;
        END;
        GETCOMBS := NUM;
    END;


    BOOLEAN PROCEDURE VALIDCOMB(A, B, C, D, E, F, G);
        INTEGER A, B, C, D, E, F, G;
    BEGIN
        INTEGER SQUARE1, SQUARE2, SQUARE3, SQUARE4;

        SQUARE1 := A + B;
        SQUARE2 := B + C + D;
        SQUARE3 := D + E + F;
        SQUARE4 := F + G;
        VALIDCOMB := SQUARE1 = SQUARE2 AND SQUARE2 = SQUARE3 AND SQUARE3 = SQUARE4
    END;

    COMMENT ----- MAIN PROGRAM ----- ;

    INTEGER ARRAY LO(1:3);
    INTEGER ARRAY HI(1:3);
    BOOLEAN ARRAY UQ(1:3);
    INTEGER I;

    LO(1) := 1; HI(1) := 7; UQ(1) := TRUE;
    LO(2) := 3; HI(2) := 9; UQ(2) := TRUE;
    LO(3) := 0; HI(3) := 9; UQ(3) := FALSE;

    FOR I := 1 STEP 1 UNTIL 3 DO
    BEGIN
        INTEGER LOW, HIGH;
        BOOLEAN UNIQ;

        LOW := LO(I); HIGH := HI(I); UNIQ := UQ(I);
        BEGIN
            INTEGER ARRAY VALIDCOMBS(1:8000, LOW:HIGH);
            INTEGER N;

            N := GETCOMBS(LOW, HIGH, UNIQ, VALIDCOMBS);
            OUTINT(N, 0);
            IF UNIQ THEN OUTTEXT(" UNIQUE");
            OUTTEXT(" SOLUTIONS IN ");
            OUTINT(LOW, 0); OUTTEXT(" TO ");
            OUTINT(HIGH, 0);
            OUTIMAGE;
            IF I < 3 THEN
            BEGIN INTEGER I, J;
                FOR I := 1 STEP 1 UNTIL N DO
                BEGIN
                    OUTTEXT("[");
                    FOR J := LOW STEP 1 UNTIL HIGH DO
                        OUTINT(VALIDCOMBS(I, J), 2);
                    OUTTEXT(" ]");
                    OUTIMAGE;
                END;
            END;
        END;
    END;

END.
```

Output:

```txt
8 UNIQUE SOLUTIONS IN 1 TO 7
[ 3 7 2 1 5 4 6 ]
[ 4 5 3 1 6 2 7 ]
[ 4 7 1 3 2 6 5 ]
[ 5 6 2 3 1 7 4 ]
[ 6 4 1 5 2 3 7 ]
[ 6 4 5 1 2 7 3 ]
[ 7 2 6 1 3 5 4 ]
[ 7 3 2 5 1 4 6 ]
4 UNIQUE SOLUTIONS IN 3 TO 9
[ 7 8 3 4 5 6 9 ]
[ 8 7 3 5 4 6 9 ]
[ 9 6 4 5 3 7 8 ]
[ 9 6 5 4 3 8 7 ]
2860 SOLUTIONS IN 0 TO 9
```


## SQL PL

Works with Db2 LUW version 9.7 or higher.

With SQL PL:

```sql_pl

--#SET TERMINATOR @

SET SERVEROUTPUT ON @

CREATE TABLE ALL_INTS (
  V INTEGER
)@

CREATE TABLE RESULTS (
  A INTEGER,
  B INTEGER,
  C INTEGER,
  D INTEGER,
  E INTEGER,
  F INTEGER,
  G INTEGER
)@

CREATE OR REPLACE PROCEDURE FOUR_SQUARES(
  IN LO INTEGER,
  IN HI INTEGER,
  IN UNIQ SMALLINT,
  --IN UNIQ BOOLEAN,
  IN SHOW SMALLINT)
  --IN SHOW BOOLEAN)
 BEGIN
  DECLARE A INTEGER;
  DECLARE B INTEGER;
  DECLARE C INTEGER;
  DECLARE D INTEGER;
  DECLARE E INTEGER;
  DECLARE F INTEGER;
  DECLARE G INTEGER;
  DECLARE OUT_LINE VARCHAR(2000);
  DECLARE I SMALLINT;

  DECLARE SOLUTIONS INTEGER;
  DECLARE UORN VARCHAR(2000);

  SET SOLUTIONS = 0;
  DELETE FROM ALL_INTS;
  DELETE FROM RESULTS;
  SET I = LO;
  WHILE (I <= HI) DO
   INSERT INTO ALL_INTS VALUES (I);
   SET I = I + 1;
  END WHILE;
  COMMIT;

  -- Computes unique solutions.
  IF (UNIQ = 0) THEN
  --IF (UNIQ = TRUE) THEN
   INSERT INTO RESULTS
     SELECT
      A.V A, B.V B, C.V C, D.V D, E.V E, F.V F, G.V G
     FROM
      ALL_INTS A, ALL_INTS B, ALL_INTS C, ALL_INTS D, ALL_INTS E, ALL_INTS F,
      ALL_INTS G
     WHERE
          A.V NOT IN (B.V, C.V, D.V, E.V, F.V, G.V)
      AND B.V NOT IN (C.V, D.V, E.V, F.V, G.V)
      AND C.V NOT IN (D.V, E.V, F.V, G.V)
      AND D.V NOT IN (E.V, F.V, G.V)
      AND E.V NOT IN (F.V, G.V)
      AND F.V NOT IN (G.V)
      AND A.V = C.V + D.V
      AND G.V = D.V + E.V
      AND B.V = E.V + F.V - C.V
     ORDER BY
      A, B, C, D, E, F, G;
   SET UORN = ' unique solutions in ';
  ELSE
   -- Compute non-unique solutions.
   INSERT INTO RESULTS
     SELECT
      A.V A, B.V B, C.V C, D.V D, E.V E, F.V F, G.V G
     FROM
      ALL_INTS A, ALL_INTS B, ALL_INTS C, ALL_INTS D, ALL_INTS E, ALL_INTS F,
      ALL_INTS G
     WHERE
          A.V = C.V + D.V
      AND G.V = D.V + E.V
      AND B.V = E.V + F.V - C.V
     ORDER BY
      A, B, C, D, E, F, G;
   SET UORN = ' non-unique solutions in ';
  END IF;
  COMMIT;

  -- Counts the possible solutions.
  FOR v AS c CURSOR FOR
    SELECT
     A, B, C, D, E, F, G
    FROM RESULTS
    ORDER BY
     A, B, C, D, E, F, G
    DO
   SET SOLUTIONS = SOLUTIONS + 1;
   -- Shows the results.
   IF (SHOW = 0) THEN
   --IF (SHOW = TRUE) THEN
    SET OUT_LINE = A || ' ' || B || ' ' || C || ' ' || D || ' ' || E || ' '
      || F ||' ' || G;
    CALL DBMS_OUTPUT.PUT_LINE(OUT_LINE);
   END IF;
  END FOR;

  SET OUT_LINE = SOLUTIONS || UORN || LO || ' to ' || HI;
  CALL DBMS_OUTPUT.PUT_LINE(OUT_LINE);
 END
@

CALL FOUR_SQUARES(1, 7, 0, 0)@
CALL FOUR_SQUARES(3, 9, 0, 0)@
CALL FOUR_SQUARES(0, 9, 1, 1)@
```

Output:

```txt
db2 -td@
db2 => CREATE TABLE ALL_INTS ( V INTEGER )
DB20000I  The SQL command completed successfully.

db2 => CREATE TABLE RESULTS ( A INTEGER, B INTEGER, C INTEGER, D INTEGER, E INTEGER, F INTEGER, G INTEGER )
DB20000I  The SQL command completed successfully.

db2 => CREATE OR REPLACE PROCEDURE FOUR_SQUARES(
...
db2 (cont.) => END @
DB20000I  The SQL command completed successfully.

db2 => CALL FOUR_SQUARES(1, 7, 0, 0)

  Return Status = 0

3 7 2 1 5 4 6
4 5 3 1 6 2 7
4 7 1 3 2 6 5
5 6 2 3 1 7 4
6 4 1 5 2 3 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4
7 3 2 5 1 4 6
8 unique solutions in 1 TO 7

db2 => CALL FOUR_SQUARES(3, 9, 0, 0)

  Return Status = 0

7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7
4 unique solutions in 3 TO 9

CALL FOUR_SQUARES(0, 9, 1, 1)

  Return Status = 0

2860 non-unique solutions in 0 TO 9
```


## Stata

Use the program '''perm''' in the [[Permutations]] task
for the first two questions, as it's fast enough.
Use '''joinby''' for the third.

```stata
perm 7
rename * (a b c d e f g)
list if a==c+d & b+c==e+f & d+e==g, noobs sep(50)

  +---------------------------+
  | a   b   c   d   e   f   g |
  |---------------------------|
  | 3   7   2   1   5   4   6 |
  | 4   5   3   1   6   2   7 |
  | 4   7   1   3   2   6   5 |
  | 5   6   2   3   1   7   4 |
  | 6   4   1   5   2   3   7 |
  | 6   4   5   1   2   7   3 |
  | 7   2   6   1   3   5   4 |
  | 7   3   2   5   1   4   6 |
  +---------------------------+

foreach var of varlist _all {
	replace `var'=`var'+2
}
list if a==c+d & b+c==e+f & d+e==g, noobs sep(50)

  +---------------------------+
  | a   b   c   d   e   f   g |
  |---------------------------|
  | 7   8   3   4   5   6   9 |
  | 8   7   3   5   4   6   9 |
  | 9   6   4   5   3   7   8 |
  | 9   6   5   4   3   8   7 |
  +---------------------------+

clear
set obs 10
gen b=_n-1
gen q=1
save temp, replace
rename b c
joinby q using temp
rename b d
joinby q using temp
rename b e
gen a=c+d
gen g=d+e
drop if a>9 | g>9
joinby q using temp
gen f=b+c-e
drop if f<0 | f>9
drop q
order a b c d e f g
erase temp.dta
count
  2,860
```


## Tcl

This task is a good opportunity to practice metaprogramming in Tcl.
The procedure <tt>compile_4rings</tt> builds a lambda expression
which takes values for <tt>{a b c d e f g}</tt> as parameters and returns
<tt>true</tt> if those values satisfy the specified expressions (<tt>$exprs</tt>).
This approach lets the bytecode compiler optimise our code.

For the final challenge, we vary the code generation a bit in <tt>compile_4rings_hard</tt>:
instead of a lambda taking parameters,
this generates a nested loop that searches exhaustively through the possible values for each variable.

The puzzle can be varied freely by changing the values of <tt>$vars</tt>
and <tt>$exprs</tt> specified at the top of the script.

```Tcl
set vars {a b c d e f g}
set exprs {
    {$a+$b}
    {$b+$c+$d}
    {$d+$e+$f}
    {$f+$g}
}

proc permute {xs} {
    if {[llength $xs] < 2} {
        return $xs
    }
    set i -1
    foreach x $xs {
        incr i
        set rest [lreplace $xs $i $i]
        foreach rest [permute $rest] {
            lappend res [list $x {*}$rest]
        }
    }
    return $res
}

proc range {a b} {
    set a [uplevel 1 [list expr $a]]
    set b [uplevel 1 [list expr $b]]
    set res {}
    while {$a <= $b} {
        lappend res $a
        incr a
    }
    return $res
}

proc compile_4rings {vars exprs} {
    set script "set _ \[[list expr [lindex $exprs 0]]\]\n"
    foreach expr [lrange $exprs 1 end] {
        append script "if {\$_ != $expr} {return false}\n"
    }
    append script "return true\n"
    list $vars $script
}

proc solve_4rings {vars exprs range} {
    set lambda [compile_4rings $vars $exprs]
    foreach values [permute $range] {
        if {[apply $lambda {*}$values]} {
            puts " $values"
        }
    }
}

proc compile_4rings_hard {vars exprs values} {
    append script "set _ \[[list expr [lindex $exprs 0]]\]\n"
    foreach expr [lrange $exprs 1 end] {
        append script "if {\$_ != $expr} {continue}\n"
    }
    append script "incr res\n"
    foreach var $vars {
        set script [list foreach $var $values $script]
    }
    set script "set res 0\n$script\nreturn \$res"
    list {} $script
}

proc solve_4rings_hard {vars exprs range} {
    apply [compile_4rings_hard $vars $exprs $range]
}

puts "# Combinations of 1..7:"
solve_4rings $vars $exprs [range 1 7]
puts "# Combinations of 3..9:"
solve_4rings $vars $exprs [range 3 9]
puts "# Number of solutions, free over 0..9:"
puts [solve_4rings_hard $vars $exprs [range 0 9]]
```

Output:

```txt
# Combinations of 1..7:
 3 7 2 1 5 4 6
 4 5 3 1 6 2 7
 4 7 1 3 2 6 5
 5 6 2 3 1 7 4
 6 4 1 5 2 3 7
 6 4 5 1 2 7 3
 7 2 6 1 3 5 4
 7 3 2 5 1 4 6
# Combinations of 3..9:
 7 8 3 4 5 6 9
 8 7 3 5 4 6 9
 9 6 4 5 3 7 8
 9 6 5 4 3 8 7
# Number of solutions, free over 0..9:
2860
```


## X86 Assembly

Works with NASM.
Works with Linux.

64 bit

```asm
; Based on C version http://rosettacode.org/wiki/4-rings_or_4-squares_puzzle#C

%define TRUE 1
%define FALSE 0

global main,foursquares,acd,ge,bf,print_output
extern printf

segment .data

a dq 0
b dq 0
c dq 0
d dq 0
e dq 0
f dq 0
g dq 0

lo dq 0
hi dq 0
unique dq 0
show dq 0
solutions dq 0

output_fmt db `%ld %ld %ld %ld %ld %ld %ld\n`,0

segment .text

main:
    push rbp
    mov rbp,rsp

    mov rdi,1
    mov rsi,7
    mov rdx,TRUE
    mov rcx,TRUE
    call foursquares

    mov rdi,3
    mov rsi,9
    mov rdx,TRUE
    mov rcx,TRUE
    call foursquares

    mov rdi,0
    mov rsi,9
    mov rdx,FALSE
    mov rcx,FALSE
    call foursquares

    xor rax,rax
    leave
    ret

segment .data

newlinefmt db `\n`,0
uniquefmt db `\n%ld unique solutions in %ld to %ld\n`,0
nonuniquefmt db `\n%ld non-unique solutions in %ld to %ld\n`,0

segment .text

foursquares:
    push rbp
    mov rbp,rsp

    mov qword [lo],rdi
    mov qword [hi],rsi
    mov qword [unique],rdx
    mov qword [show],rcx
    mov qword [solutions],0

    lea rdi,[newlinefmt]
    xor rax,rax
    call printf

    call acd

    mov rax,qword [unique]
    mov rbx,TRUE
    cmp rax,rbx
    je .isunique

    lea rdi,[nonuniquefmt]
    mov rsi,qword [solutions]
    mov rdx,qword [lo]
    mov rcx,qword [hi]
    xor rax,rax
    call printf
    jmp .done

.isunique:
    lea rdi,[uniquefmt]
    mov rsi,qword [solutions]
    mov rdx,qword [lo]
    mov rcx,qword [hi]
    xor rax,rax
    call printf

.done:
    xor rax,rax
    leave
    ret

segment .text

acd:
    push rbp
    mov rbp,rsp

    mov rax,qword [lo] ; c = lo
    mov qword [c],rax

.nextouterfor:
    mov rax,qword [c]  ; c <= hi
    mov rcx,qword [hi]
    cmp rax,rcx
    jg .doneouterfor

    mov rax,qword [lo] ; d = lo
    mov qword [d],rax

.nextinnerfor:
    mov rax,qword [d]  ; d <= hi
    mov rcx,qword [hi]
    cmp rax,rcx
    jg .doneinnerfor

    mov rax,qword [unique]
    mov rcx,FALSE
    cmp rax,rcx
    je .inif

    mov rax,qword [c]
    mov rcx,qword [d]
    cmp rax,rcx
    jne .inif
    jmp .iffails

.inif:
    mov rax,qword [c]
    mov rcx,qword [d]
    add rax,rcx
    mov qword [a],rax

; ((a >= lo) &&
;  (a <= hi) &&
;  ((!unique) ||
;   ((c != 0) &&
;    (d != 0)
;   )
;  )
; )
    mov rax,qword [a]  ;(a >= lo)
    mov rcx,qword [lo]
    cmp rax,rcx
    jl .iffails

    mov rax,qword [a]  ;(a <= hi)
    mov rcx,qword [hi]
    cmp rax,rcx
    jg .iffails

    mov rax,qword [unique] ;(!unique)
    mov rcx,FALSE
    cmp rax,rcx
    je .ifsucceeds

    mov rax,qword [c]  ;(c != 0)
    mov rcx,0
    cmp rax,rcx
    je .iffails

    mov rax,qword [d]  ;(d != 0)
    mov rcx,0
    cmp rax,rcx
    je .iffails

.ifsucceeds:

    call ge

.iffails:
    mov rax,qword [d] ; d++
    inc rax
    mov qword [d],rax
    jmp .nextinnerfor

.doneinnerfor:
    mov rax,qword [c] ; c++
    inc rax
    mov qword [c],rax
    jmp .nextouterfor

.doneouterfor:
    xor rax,rax
    leave
    ret

ge:
    push rbp
    mov rbp,rsp

    mov rax,qword [lo] ; e = lo
    mov qword [e],rax

.nextfor:
    mov rax,qword [e]  ; e <= hi
    mov rcx,qword [hi]
    cmp rax,rcx
    jg .donefor

    mov rax,qword [unique]
    mov rcx,FALSE
    cmp rax,rcx
    je .inif

; ((e != a) && (e != c) && (e != d))

    mov rax,qword [e]

    mov rcx,qword [a] ; (e != a)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [c] ; (e != c)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [d] ; (e != d)
    cmp rax,rcx
    je .skipif

.inif:
    mov rax,qword [d] ; g = d + e
    mov rcx,qword [e]
    add rax,rcx
    mov qword [g],rax

; ((g >= lo) &&
;  (g <= hi) &&
;  ((!unique) ||
;   ((g != a) &&
;    (g != c) &&
;    (g != d) &&
;    (g != e)
;   )
;  )
; )

    mov rax,qword [g]  ;(g >= lo)
    mov rcx,qword [lo]
    cmp rax,rcx
    jl .skipif

    mov rax,qword [g]  ;(g <= hi)
    mov rcx,qword [hi]
    cmp rax,rcx
    jg .skipif

    mov rax,qword [unique] ;(!unique)
    mov rcx,FALSE
    cmp rax,rcx
    je .innerifsucceeds

    mov rax,qword [g]  ;(g != a)
    mov rcx,qword [a]
    cmp rax,rcx
    je .skipif

    mov rcx,qword [c]  ;(g != c)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [d]  ;(g != d)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [e]  ;(g != e)
    cmp rax,rcx
    je .skipif

.innerifsucceeds:
    call bf

.skipif:
    mov rax,qword [e] ; e++
    inc rax
    mov qword [e],rax
    jmp .nextfor

.donefor:
    xor rax,rax
    leave
    ret

segment .text

bf:
    push rbp
    mov rbp,rsp

    mov rax,qword [lo] ; f = lo
    mov qword [f],rax

.nextfor:
    mov rax,qword [f]  ; f <= hi
    mov rcx,qword [hi]
    cmp rax,rcx
    jg .donefor

    mov rax,qword [unique]
    mov rcx,FALSE
    cmp rax,rcx
    je .inif

; ((f != a) && (f != c) && (f != d) && (f != g) && (f != e))

    mov rax,qword [f]

    mov rcx,qword [a] ; (f != a)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [c] ; (f != c)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [d] ; (f != d)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [g] ; (f != g)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [e] ; (f != e)
    cmp rax,rcx
    je .skipif

.inif:
    mov rax,qword [e] ; b = e + f - c;
    mov rcx,qword [f]
    add rax,rcx
    mov rcx,qword [c]
    sub rax,rcx
    mov qword [b],rax

; ((b >= lo) &&
;  (b <= hi) &&
;  ((!unique) ||
;   ((b != a) &&
;    (b != c) &&
;    (b != d) &&
;    (b != g) &&
;    (b != e) &&
;    (b != f)
;   )
;  )
; )

    mov rax,qword [b]  ;(b >= lo)
    mov rcx,qword [lo]
    cmp rax,rcx
    jl .skipif

    mov rax,qword [b]  ;(b <= hi)
    mov rcx,qword [hi]
    cmp rax,rcx
    jg .skipif

    mov rax,qword [unique] ;(!unique)
    mov rcx,FALSE
    cmp rax,rcx
    je .innerifsucceeds

    mov rax,qword [b]  ;(b != a)
    mov rcx,qword [a]
    cmp rax,rcx
    je .skipif

    mov rcx,qword [c]  ;(b != c)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [d]  ;(b != d)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [g]  ;(b != g)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [e]  ;(b != e)
    cmp rax,rcx
    je .skipif

    mov rcx,qword [f]  ;(b != f)
    cmp rax,rcx
    je .skipif

.innerifsucceeds:
    mov rax,qword [solutions] ; solutions++
    inc rax
    mov qword [solutions],rax

    mov rax,qword [show]
    cmp rax,TRUE
    jne .skipif

    call print_output

.skipif:
    mov rax,qword [f] ; f++
    inc rax
    mov qword [f],rax
    jmp .nextfor

.donefor:
    xor rax,rax
    leave
    ret

print_output:
    push rbp
    mov rbp,rsp


; printf("%d %d %d %d %d %d %d\n",a,b,c,d,e,f,g);

    lea rdi,[output_fmt]
    mov rsi,qword [a]
    mov rdx,qword [b]
    mov rcx,qword [c]
    mov r8,qword [d]
    mov r9,qword [e]
    mov rax,qword [g]
    push rax
    mov rax,qword [f]
    push rax
    xor rax,rax
    call printf

    xor rax,rax
    leave
    ret

```

Output

```txt


4 7 1 3 2 6 5
6 4 1 5 2 3 7
3 7 2 1 5 4 6
5 6 2 3 1 7 4
7 3 2 5 1 4 6
4 5 3 1 6 2 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4

8 unique solutions in 1 to 7

7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7

4 unique solutions in 3 to 9


2860 non-unique solutions in 0 to 9
```


## VBA

Translated from C

```vb
Dim a As Integer, b As Integer, c As Integer, d As Integer
Dim e As Integer, f As Integer, g As Integer
Dim lo As Integer, hi As Integer, unique As Boolean, show As Boolean
Dim solutions As Integer
Private Sub bf()
    For f = lo To hi
        If ((Not unique) Or _
            ((f <> a And f <> c And f <> d And f <> g And f <> e))) Then
            b = e + f - c
            If ((b >= lo) And (b <= hi) And _
                ((Not unique) Or ((b <> a) And (b <> c) And _
                (b <> d) And (b <> g) And (b <> e) And (b <> f)))) Then
                solutions = solutions + 1
                If show Then Debug.Print a; b; c; d; e; f; g
            End If
        End If
    Next
End Sub
Private Sub ge()
    For e = lo To hi
        If ((Not unique) Or ((e <> a) And (e <> c) And (e <> d))) Then
            g = d + e
            If ((g >= lo) And (g <= hi) And _
                ((Not unique) Or ((g <> a) And (g <> c) And _
                (g <> d) And (g <> e)))) Then
                bf
            End If
        End If
    Next
End Sub
Private Sub acd()
    For c = lo To hi
        For d = lo To hi
            If ((Not unique) Or (c <> d)) Then
                a = c + d
                If ((a >= lo) And (a <= hi) And _
                    ((Not unique) Or ((c <> 0) And (d <> 0)))) Then
                    ge
                End If
            End If
        Next d
    Next c
End Sub
Private Sub foursquares(plo As Integer, phi As Integer, punique As Boolean, pshow As Boolean)
    lo = plo
    hi = phi
    unique = punique
    show = pshow
    solutions = 0
    acd
    Debug.Print
    If unique Then
        Debug.Print solutions; " unique solutions in"; lo; "to"; hi
    Else
        Debug.Print solutions; " non-unique solutions in"; lo; "to"; hi
    End If
End Sub
Public Sub program()
    Call foursquares(1, 7, True, True)
    Debug.Print
    Call foursquares(3, 9, True, True)
    Call foursquares(0, 9, False, False)
End Sub

```

Output:

```txt
4  7  1  3  2  6  5
6  4  1  5  2  3  7
3  7  2  1  5  4  6
5  6  2  3  1  7  4
7  3  2  5  1  4  6
4  5  3  1  6  2  7
6  4  5  1  2  7  3
7  2  6  1  3  5  4

8  unique solutions in 1 to 7

7  8  3  4  5  6  9
8  7  3  5  4  6  9
9  6  4  5  3  7  8
9  6  5  4  3  8  7

4  unique solutions in 3 to 9

2860  non-unique solutions in 0 to 9
```


## Visual Basic .NET

Similar to the other brute-force algorithims, but with a couple of enhancements.
A "used" list is maintained to simplify checking of the nested variables overlap.
Also the ''d'', ''f'' and ''g'' '''For Each''' loops are constrained
by the other variables instead of blindly going through all combinations.

```vbnet
Module Module1

    Dim CA As Char() = "0123456789ABC".ToCharArray()

    Sub FourSquare(lo As Integer, hi As Integer, uni As Boolean, sy As Char())
        If sy IsNot Nothing Then Console.WriteLine("a b c d e f g" & vbLf & "-------------")
        Dim r = Enumerable.Range(lo, hi - lo + 1).ToList(), u As New List(Of Integer),
            t As Integer, cn As Integer = 0
        For Each a In r
            u.Add(a)
            For Each b In r
                If uni AndAlso u.Contains(b) Then Continue For
                u.Add(b)
                t = a + b
                For Each c In r : If uni AndAlso u.Contains(c) Then Continue For
                    u.Add(c)
                    For d = a - c To a - c
                        If d < lo OrElse d > hi OrElse uni AndAlso u.Contains(d) OrElse
                            t <> b + c + d Then Continue For
                        u.Add(d)
                        For Each e In r
                            If uni AndAlso u.Contains(e) Then Continue For
                            u.Add(e)
                            For f = b + c - e To b + c - e
                                If f < lo OrElse f > hi OrElse uni AndAlso u.Contains(f) OrElse
                                    t <> d + e + f Then Continue For
                                u.Add(f)
                                For g = t - f To t - f : If g < lo OrElse g > hi OrElse
                                    uni AndAlso u.Contains(g) Then Continue For
                                    cn += 1 : If sy IsNot Nothing Then _
                                        Console.WriteLine("{0} {1} {2} {3} {4} {5} {6}",
                                            sy(a), sy(b), sy(c), sy(d), sy(e), sy(f), sy(g))
                                Next : u.Remove(f) : Next : u.Remove(e) : Next : u.Remove(d)
                    Next : u.Remove(c) : Next : u.Remove(b) : Next : u.Remove(a)
        Next : Console.WriteLine("{0} {1}unique solutions for [{2},{3}]{4}",
                                 cn, If(uni, "", "non-"), lo, hi, vbLf)
    End Sub

    Sub main()
        fourSquare(1, 7, True, CA)
        fourSquare(3, 9, True, CA)
        fourSquare(0, 9, False, Nothing)
        fourSquare(5, 12, True, CA)
    End Sub

End Module
```

Output:
(Added the zkl example for [5,12])

```txt
a b c d e f g
-------------
3 7 2 1 5 4 6
4 5 3 1 6 2 7
4 7 1 3 2 6 5
5 6 2 3 1 7 4
6 4 1 5 2 3 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4
7 3 2 5 1 4 6
8 unique solutions for [1,7]

a b c d e f g
-------------
7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7
4 unique solutions for [3,9]

2860 non-unique solutions for [0,9]

a b c d e f g
-------------
B 9 6 5 7 8 C
B A 6 5 7 9 C
C 8 7 5 6 9 B
C 9 7 5 6 A B
4 unique solutions for [5,12]
```


## Yabasic

Translated from D

```Yabasic
fourSquare(1,7,true,true)
fourSquare(3,9,true,true)
fourSquare(0,9,false,false)


sub fourSquare(low, high, unique, prin)
    local count, a, b, c, d, e, f, g, fp

    if (prin) print "a b c d e f g"

    for a = low to high
        for b = low to high
            if (not valid(unique, a, b)) continue

            fp = a+b
            for c = low to high
                if (not valid(unique, c, a, b)) continue
                for d = low to high
                    if (not valid(unique, d, a, b, c)) continue
                    if (fp <> b+c+d) continue

                    for e = low to high
                        if (not valid(unique, e, a, b, c, d)) continue
                        for f = low to high
                            if (not valid(unique, f, a, b, c, d, e)) continue
                            if (fp <> d+e+f) continue

                            for g = low to high
                                if (not valid(unique, g, a, b, c, d, e, f)) continue
                                if (fp <> f+g) continue

                                count = count + 1
                                if (prin) print a," ",b," ",c," ",d," ",e," ",f," ",g
                            next
                        next
                    next
                next
            next
        next
    next
    if (unique) then
        print "There are ", count, " unique solutions in [",low,",",high,"]"
    else
        print "There are ", count, " non-unique solutions in [",low,",",high,"]"
    end if
end sub

sub valid(unique, needle, n1, n2, n3, n4, n5, n6)
    local i

    if (unique) then
        for i = 1 to numparams - 2
            switch i
                case 1: if needle = n1 return false : break
                case 2: if needle = n2 return false : break
                case 3: if needle = n3 return false : break
                case 4: if needle = n4 return false : break
                case 5: if needle = n5 return false : break
                case 6: if needle = n6 return false : break
            end switch
        next
    end if
    return true
end sub
```

Output:

```txt
a b c d e f g
3 7 2 1 5 4 6
4 5 3 1 6 2 7
4 7 1 3 2 6 5
5 6 2 3 1 7 4
6 4 1 5 2 3 7
6 4 5 1 2 7 3
7 2 6 1 3 5 4
7 3 2 5 1 4 6
There are 8 unique solutions in [1,7]
a b c d e f g
7 8 3 4 5 6 9
8 7 3 5 4 6 9
9 6 4 5 3 7 8
9 6 5 4 3 8 7
There are 4 unique solutions in [3,9]
There are 2860 non-unique solutions in [0,9]
```


## zkl

```zkl
    // unique: No repeated numbers in solution
fcn fourSquaresPuzzle(lo=1,hi=7,unique=True){  //-->list of solutions
   _assert_(0<=lo and hi<36);
   notUnic:=fcn(a,b,c,etc){ abc:=vm.arglist; // use base 36, any repeated character?
      abc.apply("toString",36).concat().unique().len()!=abc.len()
   };
   s:=List();		// solutions
   foreach a,b,c in ([lo..hi],[lo..hi],[lo..hi]){ // chunk to reduce unique
      if(unique and notUnic(a,b,c)) continue;     // solution space. Slow VM
      foreach d,e in ([lo..hi],[lo..hi]){	  // -->for d { for e {} }
         if(unique and notUnic(a,b,c,d,e)) continue;
	 foreach f,g in ([lo..hi],[lo..hi]){
	    if(unique and notUnic(a,b,c,d,e,f,g)) continue;
	    sqr1,sqr2,sqr3,sqr4 := a+b,b+c+d,d+e+f,f+g;
	    if((sqr1==sqr2==sqr3) and sqr1==sqr4) s.append(T(a,b,c,d,e,f,g));
	 }
      }
   }
   s
}
```

```zkl
fcn show(solutions,msg){
   if(not solutions){ println("No solutions for",msg); return(); }

   println(solutions.len(),msg," solutions found:");
   w:=(1).max(solutions.pump(List,(0).max,"numDigits")); // max width of any number found
   fmt:=" " + "%%%ds ".fmt(w)*7;  // eg " %1s %1s %1s %1s %1s %1s %1s"
   println(fmt.fmt(["a".."g"].walk().xplode()));
   println("-"*((w+1)*7 + 1));	  // calculate the width of horizontal bar
   foreach s in (solutions){ println(fmt.fmt(s.xplode())) }
}
fourSquaresPuzzle() : show(_," unique (1-7)");      println();
fourSquaresPuzzle(3,9) : show(_," unique (3-9)");   println();
fourSquaresPuzzle(5,12) : show(_," unique (5-12)"); println();
println(fourSquaresPuzzle(0,9,False).len(),	// 10^7 possibilities
   " non-unique (0-9) solutions found.");
```

Output:

```txt
8 unique (1-7) solutions found:
 a b c d e f g
---------------
 3 7 2 1 5 4 6
 4 5 3 1 6 2 7
 4 7 1 3 2 6 5
 5 6 2 3 1 7 4
 6 4 1 5 2 3 7
 6 4 5 1 2 7 3
 7 2 6 1 3 5 4
 7 3 2 5 1 4 6

4 unique (3-9) solutions found:
 a b c d e f g
---------------
 7 8 3 4 5 6 9
 8 7 3 5 4 6 9
 9 6 4 5 3 7 8
 9 6 5 4 3 8 7

4 unique (5-12) solutions found:
  a  b  c  d  e  f  g
----------------------
 11  9  6  5  7  8 12
 11 10  6  5  7  9 12
 12  8  7  5  6  9 11
 12  9  7  5  6 10 11

2860 non-unique (0-9) solutions found.
```
