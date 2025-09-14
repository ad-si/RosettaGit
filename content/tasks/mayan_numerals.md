+++
title = "Mayan numerals"
description = ""
date = 2019-10-05T21:32:57Z
aliases = []
[extra]
id = 22133
[taxonomies]
categories = ["Mayan numerals", "task"]
tags = []
languages = [
  "applescript",
  "factor",
  "go",
  "haskell",
  "javascript",
  "julia",
  "perl",
  "perl_6",
  "phix",
  "python",
  "rexx",
  "rust",
  "zkl",
]
+++

## Task
Present numbers using the Mayan numbering system   (displaying the Mayan numerals in a cartouche).


;Mayan numbers:
Normally, Mayan <u>numbers</u> are written vertically   (top─to─bottom)   with the most significant
numeral at the top    (in the sense that decimal numbers are written left─to─right with the most significant
digit at the left).   This task will be using a left─to─right (horizontal) format,   mostly for familiarity and
readability,   and to conserve screen space (when showing the output) on this task page.


;Mayan numerals:
Mayan <u>numerals</u>   (a base─20 "digit" or glyph)   are written in two orientations,   this
task will be using the "vertical" format   (as displayed below).   Using the vertical format makes
it much easier to draw/construct the Mayan numerals (glyphs) with simple dots ('''.''')
and hyphen ('''-''');     (however, round bullets ('''∙''') and long dashes ('''─''')
make a better presentation on Rosetta Code).


Furthermore, each Mayan numeral   (for this task)   is to be displayed as a
cartouche   (enclosed in a box)   to make it easier to parse (read);   the box may be
drawn with any suitable (ASCII or Unicode) characters that are presentable/visible in all web browsers.


;Mayan numerals added to Unicode
Mayan numerals (glyphs) were added to the Unicode Standard in June of 2018   (this corresponds with
version 11.0).   But since most web browsers don't support them at this time,   this Rosetta Code
task will be constructing the glyphs with "simple" characters and/or ASCII art.


;The "zero" glyph:
The Mayan numbering system has the concept of   '''zero''',   and should be shown by a glyph that represents
an upside─down (sea) shell,   or an egg.   The Greek letter '''theta'''   (<big>'''Θ'''</big>)   can be
used   (which more─or─less, looks like an
egg).   A   ''commercial at''   symbol   (<big>'''@'''</big>)   could make a poor substitute.


;Mayan glyphs (constructed):
The Mayan numbering system is
a   [[https://en.wikipedia.org/wiki/Vigesimal vigesimal (base 20)]]   positional numeral system.



;The Mayan numerals   (and some random numbers)   shown in the   ''vertical''   format would be shown as:

       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║    ║                            ║    ║    ║
       ║    ║                      ║ ∙  ║                            ║    ║    ║
  1──► ║    ║                11──► ║────║                      21──► ║    ║    ║
       ║ ∙  ║                      ║────║                            ║ ∙  ║ ∙  ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║    ║                            ║    ║    ║
       ║    ║                      ║ ∙∙ ║                            ║    ║    ║
  2──► ║    ║                12──► ║────║                      22──► ║    ║    ║
       ║ ∙∙ ║                      ║────║                            ║ ∙  ║ ∙∙ ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║    ║                            ║    ║    ║
       ║    ║                      ║∙∙∙ ║                            ║    ║    ║
  3──► ║    ║                13──► ║────║                      40──► ║    ║    ║
       ║∙∙∙ ║                      ║────║                            ║ ∙∙ ║ Θ  ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║    ║                            ║    ║    ║
       ║    ║                      ║∙∙∙∙║                            ║    ║    ║
  4──► ║    ║                14──► ║────║                      80──► ║    ║    ║
       ║∙∙∙∙║                      ║────║                            ║∙∙∙∙║ Θ  ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║    ║                            ║    ║    ║
       ║    ║                      ║────║                            ║    ║    ║
  5──► ║    ║                15──► ║────║                      90──► ║    ║────║
       ║────║                      ║────║                            ║∙∙∙∙║────║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║ ∙  ║                            ║    ║    ║
       ║    ║                      ║────║                            ║    ║    ║
  6──► ║ ∙  ║                16──► ║────║                     100──► ║    ║    ║
       ║────║                      ║────║                            ║────║ Θ  ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║ ∙∙ ║                            ║    ║    ║
       ║    ║                      ║────║                            ║    ║    ║
  7──► ║ ∙∙ ║                17──► ║────║                     200──► ║────║    ║
       ║────║                      ║────║                            ║────║ Θ  ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╗
       ║    ║                      ║∙∙∙ ║                            ║    ║    ║
       ║    ║                      ║────║                     300──► ║────║    ║
  8──► ║∙∙∙ ║                18──► ║────║                            ║────║    ║
       ║────║                      ║────║                            ║────║ Θ  ║
       ╚════╝                      ╚════╝                            ╚════╩════╝
       ╔════╗                      ╔════╗                            ╔════╦════╦════╗
       ║    ║                      ║∙∙∙∙║                            ║    ║    ║    ║
       ║    ║                      ║────║                     400──► ║    ║    ║    ║
  9──► ║∙∙∙∙║                19──► ║────║                            ║    ║    ║    ║
       ║────║                      ║────║                            ║ ∙  ║ Θ  ║ Θ  ║
       ╚════╝                      ╚════╝                            ╚════╩════╩════╝
       ╔════╗                      ╔════╦════╗                       ╔════╦════╦════╦════╗
       ║    ║                      ║    ║    ║                       ║    ║    ║    ║    ║
       ║    ║                      ║    ║    ║                       ║    ║    ║    ║    ║
 10──► ║────║                20──► ║    ║    ║             16,000──► ║    ║    ║    ║    ║
       ║────║                      ║ ∙  ║ Θ  ║                       ║ ∙∙ ║ Θ  ║ Θ  ║ Θ  ║
       ╚════╝                      ╚════╩════╝                       ╚════╩════╩════╩════╝


Note that the Mayan numeral   '''13'''   in   ''horizontal''   format would be shown as:

                                   ╔════╗
                                   ║  ││║
                                   ║ ∙││║
                             13──► ║ ∙││║        ◄─── this glyph form won't be used in this Rosetta Code task.
                                   ║ ∙││║
                                   ╚════╝



Other forms of cartouches (boxes) can be used for this task.


;Task requirements:
:*   convert the following decimal numbers to Mayan numbers:
:::*         4,005
:::*         8,017
:::*                 326,205
:::*                 886,205
:*   show a   <u>unique</u>   ''interesting/pretty/unusual/intriguing/odd/amusing/weird''   Mayan number
:*   show all output here


;Related tasks:
:*   [[Roman numerals/Encode]]   ─── convert numeric values into Roman numerals
:*   [[Roman numerals/Decode]]   ─── convert Roman numerals into Arabic numbers


;See also:
:*   The Wikipedia entry:   [[https://en.wikipedia.org/wiki/Maya_numerals Mayan numerals]]






## AppleScript

Applescript supports only small integers (up to (2^29)-1 = 536870911).

```applescript
use AppleScript version "2.4"
use framework "Foundation"
use scripting additions

-- MAYAN NUMBERS ------------------------------------------

-- mayanNumber:: Int -> [[String]]
on mayanNumber(n)
    showIntAtBase(20, my mayanDigit, n, {})
end mayanNumber

-- mayanDigit :: Int -> String
on mayanDigit(n)
    if 0 < n then
        set r to n mod 5
        bool({}, {concat(replicate(r, "●"))}, 0 < r) & ¬
            replicate(n div 5, "━━")
    else
        {"Θ"}
    end if
end mayanDigit

-- mayanFrame :: Int -> String
on mayanFrame(n)
    "Mayan " & (n as string) & ":\n" & wikiTable({|class|:¬
        "wikitable", colwidth:¬
        "3em", cell:¬
        "vertical-align:bottom;", |style|:¬
        "text-align:center;background-color:#F0EDDE;" & ¬
        "color:#605B4B;border:2px solid silver"})'s ¬
        |λ|({map(intercalateS("
"), mayanNumber(n))}) & "\n"
end mayanFrame

-- TEST ---------------------------------------------------

on run
    set str to unlines(map(mayanFrame, ¬
        {4005, 8017, 326205, 886205, 2978480}))
    
    set the clipboard to (str)
    return str
end run

-- GENERIC ------------------------------------------------

-- Just :: a -> Maybe a
on Just(x)
    {type:"Maybe", Nothing:false, Just:x}
end Just

-- Nothing :: Maybe a
on Nothing()
    {type:"Maybe", Nothing:true}
end Nothing

-- Tuple (,) :: a -> b -> (a, b)
on Tuple(a, b)
    {type:"Tuple", |1|:a, |2|:b, length:2}
end Tuple

-- bool :: a -> a -> Bool -> a
on bool(f, t, p)
    if p then
        t
    else
        f
    end if
end bool

-- concat :: [[a]] -> [a]
-- concat :: [String] -> String
on concat(xs)
    set lng to length of xs
    if 0 < lng and string is class of (item 1 of xs) then
        set acc to ""
    else
        set acc to {}
    end if
    repeat with i from 1 to lng
        set acc to acc & item i of xs
    end repeat
    acc
end concat

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

-- intercalateS :: String -> [String] -> String
on intercalateS(sep)
    script
        on |λ|(xs)
            set {dlm, my text item delimiters} to {my text item delimiters, sep}
            set s to xs as text
            set my text item delimiters to dlm
            return s
        end |λ|
    end script
end intercalateS

-- lookupDict :: a -> Dict -> Maybe b
on lookupDict(k, dct)
    set ca to current application
    set v to (ca's NSDictionary's dictionaryWithDictionary:dct)'s objectForKey:k
    if missing value ≠ v then
        Just(item 1 of ((ca's NSArray's arrayWithObject:v) as list))
    else
        Nothing()
    end if
end lookupDict

-- Lift 2nd class handler function into 1st class script wrapper 
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

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


-- | The 'maybe' function takes a default value, a function, and a 'Maybe'
-- value.  If the 'Maybe' value is 'Nothing', the function returns the
-- default value.  Otherwise, it applies the function to the value inside
-- the 'Just' and returns the result.
-- maybe :: b -> (a -> b) -> Maybe a -> b
on maybe(v, f, mb)
    if Nothing of mb then
        v
    else
        tell mReturn(f) to |λ|(Just of mb)
    end if
end maybe

-- quotRem :: Int -> Int -> (Int, Int)
on quotRem(m, n)
    Tuple(m div n, m mod n)
end quotRem

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary 
-- assembly of a target length
-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}
    
    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- showIntAtBase :: Int -> (Int -> [String]) -> Int -> String -> String
on showIntAtBase(base, toDigit, n, rs)
    script showIt
        property f : mReturn(toDigit)
        on |λ|(nd_, r)
            set {n, d} to ({|1|, |2|} of nd_)
            set r_ to {f's |λ|(d)} & r
            if n > 0 then
                |λ|(quotRem(n, base), r_)
            else
                r_
            end if
        end |λ|
    end script
    showIt's |λ|(quotRem(n, base), rs)
end showIntAtBase

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines

-- unwords :: [String] -> String
on unwords(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, space}
    set s to xs as text
    set my text item delimiters to dlm
    return s
end unwords

-- wikiTable :: Dict -> [[String]] -> String
on wikiTable(opts)
    script ksv
        on |λ|(k)
            script
                on |λ|(s)
                    script
                        on |λ|(v)
                            k & v & s
                        end |λ|
                    end script
                end |λ|
            end script
        end |λ|
    end script
    script
        on |λ|(rows)
            script boxStyle
                on |λ|(a, k)
                    maybe(a, ksv's |λ|(a & k & "=\"")'s |λ|("\" "), ¬
                        lookupDict(k, opts))
                end |λ|
            end script
            script rowText
                on |λ|(row, iRow)
                    script cellText
                        on |λ|(cell)
                            if 1 = iRow then
                                set w to maybe("", ksv's |λ|("width:")'s |λ|(";"), ¬
                                    lookupDict("colwidth", opts))
                            else
                                set w to ""
                            end if
                            set s to maybe(w, ksv's |λ|(w)'s |λ|(""), ¬
                                lookupDict("cell", opts))
                            if 0 < length of s then
                                "style=\"" & s & "\"|" & cell
                            else
                                cell
                            end if
                        end |λ|
                    end script
                    intercalateS("\n|")'s |λ|(map(cellText, row))
                end |λ|
            end script
            "{| " & unwords(foldl(boxStyle, "", {"class", "style"})) & "\n|" & ¬
                intercalateS("\n|-\n")'s |λ|(map(rowText, rows)) & "\n|}"
        end |λ|
    end script
end wikiTable
```

{{Out}}
Mayan 4005:
{| class="wikitable" style="text-align:center;background-color:#F0EDDE;color:#605B4B;border:2px solid silver" 
|style="width:3em;vertical-align:bottom;"|━━
━━
|style="width:3em;vertical-align:bottom;"|Θ
|style="width:3em;vertical-align:bottom;"|━━
|}

Mayan 8017:
{| class="wikitable" style="text-align:center;background-color:#F0EDDE;color:#605B4B;border:2px solid silver" 
|style="width:3em;vertical-align:bottom;"|●
|style="width:3em;vertical-align:bottom;"|Θ
|style="width:3em;vertical-align:bottom;"|Θ
|style="width:3em;vertical-align:bottom;"|●●
━━
━━
━━
|}

Mayan 326205:
{| class="wikitable" style="text-align:center;background-color:#F0EDDE;color:#605B4B;border:2px solid silver" 
|style="width:3em;vertical-align:bottom;"|●●
|style="width:3em;vertical-align:bottom;"|Θ
|style="width:3em;vertical-align:bottom;"|━━
━━
━━
|style="width:3em;vertical-align:bottom;"|━━
━━
|style="width:3em;vertical-align:bottom;"|━━
|}

Mayan 886205:
{| class="wikitable" style="text-align:center;background-color:#F0EDDE;color:#605B4B;border:2px solid silver" 
|style="width:3em;vertical-align:bottom;"|━━
|style="width:3em;vertical-align:bottom;"|━━
━━
|style="width:3em;vertical-align:bottom;"|━━
━━
━━
|style="width:3em;vertical-align:bottom;"|━━
━━
|style="width:3em;vertical-align:bottom;"|━━
|}

Mayan 2978480:
{| class="wikitable" style="text-align:center;background-color:#F0EDDE;color:#605B4B;border:2px solid silver" 
|style="width:3em;vertical-align:bottom;"|●●●
━━
━━
━━
|style="width:3em;vertical-align:bottom;"|●●
━━
━━
|style="width:3em;vertical-align:bottom;"|●
━━
|style="width:3em;vertical-align:bottom;"|●●●●
|style="width:3em;vertical-align:bottom;"|Θ
|}


## Factor


```factor
USING: arrays formatting io kernel make math math.extras
sequences ;
IN: rosetta-code.mayan-numerals

: mayan-digit ( n -- m pair ) 20 /mod 5 /mod swap 2array ;

: integer>mayan ( n -- seq )
    [ [ mayan-digit , ] until-zero ] { } make reverse ;

: ones ( n -- str ) [ CHAR: ● ] "" replicate-as ;
: fives ( n -- str ) [ "——" ] replicate "
" join ;

: numeral ( pair -- str )
    dup sum zero? [ drop "Θ" ]
    [ first2 [ ones ] [ fives ] bi* 2array harvest "
" join ]
    if ;

: .table-row ( pair -- )
    "|style=\"width:3em;vertical-align:bottom;\"|" write numeral
    print ;

: .table-head ( -- )
    "class=\"wikitable\" style=\"text-align:center;\"" print ;

: .table-body ( n -- ) integer>mayan [ .table-row ] each ;

: .mayan ( n -- )
    [ "Mayan %d:\n" printf ]
    [ "{|" write .table-head .table-body "|}" print ] bi ;

: mayan-numerals ( -- )
    { 4005 8017 326205 886205 } [ .mayan ] each ;

MAIN: mayan-numerals
```

{{out}}
Mayan 4005:
{|class="wikitable" style="text-align:center;"
|style="width:3em;vertical-align:bottom;"|——
——
|style="width:3em;vertical-align:bottom;"|Θ
|style="width:3em;vertical-align:bottom;"|——
|}
Mayan 8017:
{|class="wikitable" style="text-align:center;"
|style="width:3em;vertical-align:bottom;"|●
|style="width:3em;vertical-align:bottom;"|Θ
|style="width:3em;vertical-align:bottom;"|Θ
|style="width:3em;vertical-align:bottom;"|●●
——
——
——
|}
Mayan 326205:
{|class="wikitable" style="text-align:center;"
|style="width:3em;vertical-align:bottom;"|●●
|style="width:3em;vertical-align:bottom;"|Θ
|style="width:3em;vertical-align:bottom;"|——
——
——
|style="width:3em;vertical-align:bottom;"|——
——
|style="width:3em;vertical-align:bottom;"|——
|}
Mayan 886205:
{|class="wikitable" style="text-align:center;"
|style="width:3em;vertical-align:bottom;"|——
|style="width:3em;vertical-align:bottom;"|——
——
|style="width:3em;vertical-align:bottom;"|——
——
——
|style="width:3em;vertical-align:bottom;"|——
——
|style="width:3em;vertical-align:bottom;"|——
|}
Mayan 194481:
{|class="wikitable" style="text-align:center;"
|style="width:3em;vertical-align:bottom;"|●
|style="width:3em;vertical-align:bottom;"|●●●●
|style="width:3em;vertical-align:bottom;"|●
——
|style="width:3em;vertical-align:bottom;"|●●●●
|style="width:3em;vertical-align:bottom;"|●
|}


## Go


```go
package main

import (
    "fmt"
    "strconv"
)

const (
    ul = "╔"
    uc = "╦"
    ur = "╗"
    ll = "╚"
    lc = "╩"
    lr = "╝"
    hb = "═"
    vb = "║"
)

var mayan = [5]string{
    "    ",
    " ∙  ",
    " ∙∙ ",
    "∙∙∙ ",
    "∙∙∙∙",
}

const (
    m0 = " Θ  "
    m5 = "────"
)

func dec2vig(n uint64) []uint64 {
    vig := strconv.FormatUint(n, 20)
    res := make([]uint64, len(vig))
    for i, d := range vig {
        res[i], _ = strconv.ParseUint(string(d), 20, 64)
    }
    return res
}

func vig2quin(n uint64) [4]string {
    if n >= 20 {
        panic("Cant't convert a number >= 20")
    }
    res := [4]string{mayan[0], mayan[0], mayan[0], mayan[0]}
    if n == 0 {
        res[3] = m0
        return res
    }
    fives := n / 5
    rem := n % 5
    res[3-fives] = mayan[rem]
    for i := 3; i > 3-int(fives); i-- {
        res[i] = m5
    }
    return res
}

func draw(mayans [][4]string) {
    lm := len(mayans)
    fmt.Print(ul)
    for i := 0; i < lm; i++ {
        for j := 0; j < 4; j++ {
            fmt.Print(hb)
        }
        if i < lm-1 {
            fmt.Print(uc)
        } else {
            fmt.Println(ur)
        }
    }
    for i := 1; i < 5; i++ {
        fmt.Print(vb)
        for j := 0; j < lm; j++ {
            fmt.Print(mayans[j][i-1])
            fmt.Print(vb)
        }
        fmt.Println()
    }
    fmt.Print(ll)
    for i := 0; i < lm; i++ {
        for j := 0; j < 4; j++ {
            fmt.Print(hb)
        }
        if i < lm-1 {
            fmt.Print(lc)
        } else {
            fmt.Println(lr)
        }
    }
}

func main() {
    numbers := []uint64{4005, 8017, 326205, 886205, 1081439556}
    for _, n := range numbers {
        fmt.Printf("Converting %d to Mayan:\n", n)
        vigs := dec2vig(n)
        lv := len(vigs)
        mayans := make([][4]string, lv)
        for i, vig := range vigs {
            mayans[i] = vig2quin(vig)
        }
        draw(mayans)
        fmt.Println()
    }
}
```


{{out}}

```txt

Converting 4005 to Mayan:
╔════╦════╦════╗
║    ║    ║    ║
║    ║    ║    ║
║────║    ║    ║
║────║ Θ  ║────║
╚════╩════╩════╝

Converting 8017 to Mayan:
╔════╦════╦════╦════╗
║    ║    ║    ║ ∙∙ ║
║    ║    ║    ║────║
║    ║    ║    ║────║
║ ∙  ║ Θ  ║ Θ  ║────║
╚════╩════╩════╩════╝

Converting 326205 to Mayan:
╔════╦════╦════╦════╦════╗
║    ║    ║    ║    ║    ║
║    ║    ║────║    ║    ║
║    ║    ║────║────║    ║
║ ∙∙ ║ Θ  ║────║────║────║
╚════╩════╩════╩════╩════╝

Converting 886205 to Mayan:
╔════╦════╦════╦════╦════╗
║    ║    ║    ║    ║    ║
║    ║    ║────║    ║    ║
║    ║────║────║────║    ║
║────║────║────║────║────║
╚════╩════╩════╩════╩════╝

Converting 1081439556 to Mayan:
╔════╦════╦════╦════╦════╦════╦════╗
║ ∙  ║ ∙∙ ║∙∙∙ ║∙∙∙∙║∙∙∙ ║ ∙∙ ║ ∙  ║
║────║────║────║────║────║────║────║
║────║────║────║────║────║────║────║
║────║────║────║────║────║────║────║
╚════╩════╩════╩════╩════╩════╩════╝

```



## Haskell


```haskell
import Data.List (intercalate, transpose)
import qualified Data.Map.Strict as M
import Data.Maybe (maybe)
import Data.Bool (bool)

-- MAIN ---------------------------------------------------
main :: IO ()
main =
  (putStrLn . unlines) $
  mayanFramed <$> [4005, 8017, 326205, 886205, 1081439556, 1000000, 1000000000]

-- MAYAN NUMBERS ------------------------------------------
mayanGlyph :: Int -> [[String]]
mayanGlyph =
  filter (any (not . null)) .
  transpose . leftPadded . flip (showIntAtBaseS 20 mayanDigit) []

mayanDigit :: Int -> [String]
mayanDigit n
  | 0 /= n =
    replicate (rem n 5) mayaOne : concat (replicate (quot n 5) [mayaFive])
  | otherwise = [[mayaZero]]

mayanFramed :: Int -> String
mayanFramed =
  ("Mayan " ++) .
  ((++) <$> show <*>
   ((":\n\n" ++) .
    wikiTable
      (M.fromList
         [ ( "style"
           , concat
               [ "text-align:center;"
               , "background-color:#F0EDDE;"
               , "color:#605B4B;"
               , "border:2px solid silver;"
               ])
         , ("colwidth", "3em;")
         ]) .
    mayanGlyph))

mayaZero, mayaOne :: Char
mayaZero = '\920'

mayaOne = '\9679'

mayaFive :: String
mayaFive = "\9473\9473"

-- NUMERIC BASES ------------------------------------------
-- Based on the Prelude showIntAtBase but uses an (Int -> [String])
-- (rather than Int -> Char) function as its second argument.
--
-- Shows a /non-negative/ 'Integral' number using the base specified by the
-- first argument, and the **String** representation specified by the second.
showIntAtBaseS
  :: Integral a
  => a -> (Int -> [String]) -> a -> [[String]] -> [[String]]
showIntAtBaseS base toStr n0 r0 =
  let go (n, d) r =
        seq s $
        case n of
          0 -> r_
          _ -> go (quotRem n base) r_
        where
          s = toStr (fromIntegral d)
          r_ = s : r
  in go (quotRem n0 base) r0

-- DISPLAY ------------------------------------------------
wikiTable :: M.Map String String -> [[String]] -> String
wikiTable opts rows
  | null rows = []
  | otherwise =
    "{| " ++
    foldr
      (\k a -> maybe a (((a ++ k ++ "=\"") ++) . (++ "\" ")) (M.lookup k opts))
      []
      ["class", "style"] ++
    '\n' :
    intercalate
      "|-\n"
      (zipWith
         (\row i ->
             unlines
               (fmap
                  ((bool
                      []
                      (maybe
                         "|"
                         (("|style=\"width:" ++) . (++ "\""))
                         (M.lookup "colwidth" opts))
                      (0 == i) ++) .
                   ('|' :))
                  row))
         rows
         [0 ..]) ++
    "|}\n\n"

leftPadded :: [[String]] -> [[String]]
leftPadded xs =
  let w = maximum (length <$> xs)
  in ((++) =<< flip replicate [] . (-) w . length) <$> xs
```

{{Out}}
Mayan 4005:
{| style="text-align:center;background-color:#F0EDDE;color:#605B4B;border:2px solid silver;" 
|style="width:3em;"|━━
|style="width:3em;"|
|style="width:3em;"|
|-
|━━
|Θ
|━━
|}


Mayan 8017:

{| style="text-align:center;background-color:#F0EDDE;color:#605B4B;border:2px solid silver;" 
|style="width:3em;"|
|style="width:3em;"|
|style="width:3em;"|
|style="width:3em;"|●●
|-
|
|
|
|━━
|-
|
|
|
|━━
|-
|●
|Θ
|Θ
|━━
|}


Mayan 326205:

{| style="text-align:center;background-color:#F0EDDE;color:#605B4B;border:2px solid silver;" 
|style="width:3em;"|
|style="width:3em;"|
|style="width:3em;"|━━
|style="width:3em;"|
|style="width:3em;"|
|-
|
|
|━━
|━━
|
|-
|●●
|Θ
|━━
|━━
|━━
|}


Mayan 886205:

{| style="text-align:center;background-color:#F0EDDE;color:#605B4B;border:2px solid silver;" 
|style="width:3em;"|
|style="width:3em;"|
|style="width:3em;"|━━
|style="width:3em;"|
|style="width:3em;"|
|-
|
|━━
|━━
|━━
|
|-
|━━
|━━
|━━
|━━
|━━
|}


Mayan 1081439556:

{| style="text-align:center;background-color:#F0EDDE;color:#605B4B;border:2px solid silver;" 
|style="width:3em;"|●
|style="width:3em;"|●●
|style="width:3em;"|●●●
|style="width:3em;"|●●●●
|style="width:3em;"|●●●
|style="width:3em;"|●●
|style="width:3em;"|●
|-
|━━
|━━
|━━
|━━
|━━
|━━
|━━
|-
|━━
|━━
|━━
|━━
|━━
|━━
|━━
|-
|━━
|━━
|━━
|━━
|━━
|━━
|━━
|}


Mayan 1000000:

{| style="text-align:center;background-color:#F0EDDE;color:#605B4B;border:2px solid silver;" 
|style="width:3em;"|●
|style="width:3em;"|
|style="width:3em;"|
|style="width:3em;"|
|style="width:3em;"|
|-
|━━
|━━
|Θ
|Θ
|Θ
|}


Mayan 1000000000:

{| style="text-align:center;background-color:#F0EDDE;color:#605B4B;border:2px solid silver;" 
|style="width:3em;"|━━
|style="width:3em;"|●●
|style="width:3em;"|
|style="width:3em;"|
|style="width:3em;"|
|style="width:3em;"|
|style="width:3em;"|
|-
|━━
|━━
|━━
|
|
|
|
|-
|━━
|━━
|━━
|Θ
|Θ
|Θ
|Θ
|}


## JavaScript


```JavaScript
(() => {
    'use strict';

    const main = () =>
        unlines(
            map(mayanFramed,
                [4005, 8017, 326205, 886205, 1081439556, 1000000, 1000000000]
            )
        );

    // MAYAN NUMBERS --------------------------------------

    // mayanFramed :: Int -> String
    const mayanFramed = n =>
        '\nMayan ' + n.toString() + ':\n\n' +
        wikiTable({
            style: 'text-align:center; background-color:#F0EDDE; ' +
            'color:#605B4B; border:2px solid silver',
            colwidth: '3em'
        })(
            mayanGlyph(n)
        );

    // mayanGlyph :: Int -> [[String]]
    const mayanGlyph = n =>
        filter(any(compose(not, isNull)),
            transpose(leftPadded(
                showIntAtBase(20, mayanDigit, n, [])
            ))
        );

    // mayanDigit :: Int -> [String]
    const mayanDigit = n =>
        0 !== n ? cons(
            replicateString(rem(n, 5), '●'),
            replicate(quot(n, 5), '━━')
        ) : ['Θ'];

    // FORMATTING -----------------------------------------

    // wikiTable :: Dict -> [[a]] -> String
    const wikiTable = opts => rows => {
        const colWidth = () =>
            'colwidth' in opts ? (
                '|style="width:' + opts.colwidth + ';"'
            ) : '';
        return 0 < rows.length ? (
            '{| ' + ['class', 'style'].reduce(
                (a, k) => k in opts ? (
                    a + k + '="' + opts[k] + '" '
                ) : a, ''
            ) + '\n' + rows.map(
                (row, i) => row.map(
                    x => (0 === i ? (
                        colWidth() + '| '
                    ) : '|') + (x.toString() || ' ')
                ).join('\n')
            ).join('\n|-\n') + '\n|}\n\n'
        ) : '';
    };

    // leftPadded :: [[String]] -> [[String]]
    const leftPadded = xs => {
        const w = maximum(map(length, xs));
        return map(
            x => replicate(w - x.length, '').concat(x),
            xs
        );
    };

    // GENERIC FUNCTIONS ----------------------------------

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // any :: (a -> Bool) -> [a] -> Bool
    const any = p => xs => xs.some(p);

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
        };

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (f, g) => x => f(g(x));

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.reduce((a, x) => a.concat(f(x)), []);

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) =>
        Array.isArray(xs) ? (
            [x].concat(xs)
        ) : 'GeneratorFunction' !== xs.constructor.constructor.name ? (
            x + xs
        ) : ( // Existing generator wrapped with one additional element
            function*() {
                yield x;
                let nxt = xs.next()
                while (!nxt.done) {
                    yield nxt.value;
                    nxt = xs.next();
                }
            }
        )();

    // filter :: (a -> Bool) -> [a] -> [a]
    const filter = (f, xs) => xs.filter(f);

    // foldl1 :: (a -> a -> a) -> [a] -> a
    const foldl1 = (f, xs) =>
        1 < xs.length ? xs.slice(1)
        .reduce(f, xs[0]) : xs[0];

    // isNull :: [a] -> Bool
    // isNull :: String -> Bool
    const isNull = xs =>
        Array.isArray(xs) || ('string' === typeof xs) ? (
            1 > xs.length
        ) : undefined;

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // maximum :: Ord a => [a] -> a
    const maximum = xs =>
        0 < xs.length ? (
            foldl1((a, x) => x > a ? x : a, xs)
        ) : undefined;

    //  Ordering: (LT|EQ|GT):
    //  GT: 1 (or other positive n)
    //    EQ: 0
    //  LT: -1 (or other negative n)

    // maximumBy :: (a -> a -> Ordering) -> [a] -> a
    const maximumBy = (f, xs) =>
        0 < xs.length ? (
            xs.slice(1)
            .reduce((a, x) => 0 < f(x, a) ? x : a, xs[0])
        ) : undefined;

    // not :: Bool -> Bool
    const not = b => !b;

    // quot :: Int -> Int -> Int
    const quot = (n, m) => Math.floor(n / m);

    // quotRem :: Int -> Int -> (Int, Int)
    const quotRem = (m, n) =>
        Tuple(Math.floor(m / n), m % n);

    // rem :: Int -> Int -> Int
    const rem = (n, m) => n % m;

    // replicate :: Int -> a -> [a]
    const replicate = (n, x) =>
        Array.from({
            length: n
        }, () => x);

    // replicateString :: Int -> String -> String
    const replicateString = (n, s) => s.repeat(n);

    // showIntAtBase :: Int -> (Int -> [String])
    //               -> Int -> [[String]] -> [[String]]
    const showIntAtBase = (base, toStr, n, rs) => {
        const go = ([n, d], r) => {
            const r_ = cons(toStr(d), r);
            return 0 !== n ? (
                go(Array.from(quotRem(n, base)), r_)
            ) : r_;
        };
        return go(Array.from(quotRem(n, base)), rs);
    };

    // transpose :: [[a]] -> [[a]]
    const transpose = tbl => {
        const
            gaps = replicate(
                length(maximumBy(comparing(length), tbl)), []
            ),
            rows = map(xs => xs.concat(gaps.slice(xs.length)), tbl);
        return map(
            (_, col) => concatMap(row => [row[col]], rows),
            rows[0]
        );
    };

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

{{Out}}
Mayan 4005:

{| style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver" 
|style="width:3em;"| ━━
|style="width:3em;"|  
|style="width:3em;"|  
|-
|━━
|Θ
|━━
|}



Mayan 8017:

{| style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver" 
|style="width:3em;"|  
|style="width:3em;"|  
|style="width:3em;"|  
|style="width:3em;"| ●●
|-
| 
| 
| 
|━━
|-
| 
| 
| 
|━━
|-
|●
|Θ
|Θ
|━━
|}



Mayan 326205:

{| style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver" 
|style="width:3em;"|  
|style="width:3em;"|  
|style="width:3em;"| ━━
|style="width:3em;"|  
|style="width:3em;"|  
|-
| 
| 
|━━
|━━
| 
|-
|●●
|Θ
|━━
|━━
|━━
|}



Mayan 886205:

{| style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver" 
|style="width:3em;"|  
|style="width:3em;"|  
|style="width:3em;"| ━━
|style="width:3em;"|  
|style="width:3em;"|  
|-
| 
|━━
|━━
|━━
| 
|-
|━━
|━━
|━━
|━━
|━━
|}



Mayan 1081439556:

{| style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver" 
|style="width:3em;"| ●
|style="width:3em;"| ●●
|style="width:3em;"| ●●●
|style="width:3em;"| ●●●●
|style="width:3em;"| ●●●
|style="width:3em;"| ●●
|style="width:3em;"| ●
|-
|━━
|━━
|━━
|━━
|━━
|━━
|━━
|-
|━━
|━━
|━━
|━━
|━━
|━━
|━━
|-
|━━
|━━
|━━
|━━
|━━
|━━
|━━
|}



Mayan 1000000:

{| style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver" 
|style="width:3em;"| ●
|style="width:3em;"|  
|style="width:3em;"|  
|style="width:3em;"|  
|style="width:3em;"|  
|-
|━━
|━━
|Θ
|Θ
|Θ
|}



Mayan 1000000000:

{| style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver" 
|style="width:3em;"| ━━
|style="width:3em;"| ●●
|style="width:3em;"|  
|style="width:3em;"|  
|style="width:3em;"|  
|style="width:3em;"|  
|style="width:3em;"|  
|-
|━━
|━━
|━━
| 
| 
| 
| 
|-
|━━
|━━
|━━
|Θ
|Θ
|Θ
|Θ
|}



## Julia

<h4> First version, using a style sheet, output to html, with the capability of proper style within the browser.</h4>

```julia
using Gumbo
 
mayan_glyphs(x, y) = (x == 0 && y == 0) ? "\n<td>Θ</td>\n" : "<td>\n" * "●" ^ x * "<br />\n───" ^ y * "</td>\n"
 
inttomayan(n) = (s = string(n, base=20); map(ch -> reverse(divrem(parse(Int, ch, base=20), 5)), split(s, "")))
 
function testmayan()
    startstring = """\n
<style>
table.roundedcorners { 
  border: 1px solid DarkOrange;
  border-radius: 13px; 
  border-spacing: 1;
  }
table.roundedcorners td, 
table.roundedcorners th { 
  border: 2px solid DarkOrange;
  border-radius: 13px; 
  border-bottom: 3px solid DarkOrange;
  vertical-align: bottom;
  text-align: center;
  padding: 10px; 
  }
</style>
\n"""
 
    txt = startstring
 
    for n in [4005, 8017, 326205, 886205, 70913241, 2147483647]
    txt *= "<h3>The Mayan representation for the integer $n is: </h3><table class=\"roundedcorners\"><tr>" *
            join(map(x -> mayan_glyphs(x[1], x[2]), inttomayan(n))) * "</tr></table>\n\n"
    end
 
    println(parsehtml(txt))
end

testmayan()

```

<h4> Version 2, using local wiki style table formatting: translation of style code from the Perl 6 version. </h4>

```julia
mayan_glyphs(x, y) = (x == 0 && y == 0) ? "\n| style=$tdconfig | Θ" : "\n| style=$tdconfig | " * "●" ^ x * "
───" ^ y

inttomayan(n) = (s = string(n, base=20); map(ch -> reverse(divrem(parse(Int, ch, base=20), 5)), split(s, "")))

tableconfig = raw""" " border: 1px solid DarkOrange;   border-radius: 13px;   border-spacing: 1; " """
tdconfig = raw""" "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;" """

function testmayan()
    txt = ""
    for n in [4005, 8017, 326205, 886205, 70913241, 2147483647]
        txt *= "\n'''The Mayan representation for the integer $n is:'''\n" *
               "{| style=$tableconfig \n|- " *
               join(map(x -> mayan_glyphs(x[1], x[2]), inttomayan(n))) * "\n|}\n\n"
    end
    println(txt)
end

testmayan()

```

{{out}}

'''The Mayan representation for the integer 4005 is:'''
{| style= " border: 1px solid DarkOrange;   border-radius: 13px;   border-spacing: 1; "
|-
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | 
───
───
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | Θ
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | 
───
|}


'''The Mayan representation for the integer 8017 is:'''
{| style= " border: 1px solid DarkOrange;   border-radius: 13px;   border-spacing: 1; "
|-
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | Θ
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | Θ
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●●
───
───
───
|}


'''The Mayan representation for the integer 326205 is:'''
{| style= " border: 1px solid DarkOrange;   border-radius: 13px;   border-spacing: 1; "
|-
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●●
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | Θ
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | 
───
───
───
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | 
───
───
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | 
───
|}


'''The Mayan representation for the integer 886205 is:'''
{| style= " border: 1px solid DarkOrange;   border-radius: 13px;   border-spacing: 1; "
|-
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | 
───
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | 
───
───
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | 
───
───
───
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | 
───
───
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | 
───
|}


'''The Mayan representation for the integer 70913241 is:'''
{| style= " border: 1px solid DarkOrange;   border-radius: 13px;   border-spacing: 1; "
|-
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●●
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●●●
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●●●●
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●●●
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●●
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●
|}


'''The Mayan representation for the integer 2147483647 is:'''
{| style= " border: 1px solid DarkOrange;   border-radius: 13px;   border-spacing: 1; "
|-
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●●●
───
───
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●
───
───
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | 
───
───
───
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●●●●
───
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●●
| style= "border: 2px solid DarkOrange; border-radius: 13px; border-bottom: 2px solid DarkOrange; vertical-align: bottom; text-align: center; padding: 10px;"  | ●●
───
|}


## Perl

{{trans|Perl 6}}
{{libheader|ntheory}}

```perl
use ntheory qw/fromdigits todigitstring/;

my $t_style = '"border-collapse: separate; text-align: center; border-spacing: 3px 0px;"';
my $c_style = '"border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;'.
  'border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;'.
  'vertical-align: bottom;width: 3.25em;"';

sub cartouches {
    my($num, @digits) = @_;
    my $render;
    for my $d (@digits) {
        $render .= "| style=$c_style | $_\n" for glyphs(@$d);
    }
    chomp $render;
    join "\n", "\{| style=$t_style", "|+ $num", '|-', $render, '|}'
}

sub glyphs {
    return 'Θ' unless $_[0] || $_[1];
    join '
', '●' x $_[0], ('───') x $_[1];
}

sub mmod {
    my($n,$b) = @_;
    my @nb;
    return 0 unless $n;
    push @nb, fromdigits($_, $b) for split '', todigitstring($n, $b);
    return @nb;
}

for $n (qw<4005 8017 326205 886205 26960840421>) {
    push @output, cartouches($n, map { [reverse mmod($_,5)] } mmod($n,20) );
}

print join "\n
\n", @output;
```

{{out}}
{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 4005
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | Θ
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
|}


{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 8017
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | Θ
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | Θ
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●
───
───
───
|}


{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 326205
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | Θ
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
|}


{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 886205
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
|}


{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 26960840421
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
|}


## Perl 6

Just a matter of converting to base twenty, then divmod 5 each digit and map to the appropriate glyph. Most of this is display code to deal with the cartouche requirement.

I actually spent a little time looking into what exactly a Mayan cartouche was supposed to look like. The classic Mayan cartouche was kind of a rounded rectangle with three little semi-circles underneath. Kind of looks like a picture frame. In general, they were only used when expressing significant "holy" dates in the Mayan calender.

These don't particularly resemble the Mayan cartouche, more like mahjong tiles actually. It would have been ''so'' much easier and more compact though if I could have put all the styling into a CSS file instead of inlining it, but oh well, it was entertaining to do.


```perl6
my $t-style = '"border-collapse: separate; text-align: center; border-spacing: 3px 0px;"';
my $c-style = '"border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;'~
  'border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;'~
  'vertical-align: bottom;width: 3.25em;"';

sub mayan ($int) { $int.polymod(20 xx *).reverse.map: *.polymod(5) }

sub display ($num, @digits) {
    join "\n", "\{| style=$t-style", "|+ $num",
    '|-', (|@digits.map: {"| style=$c-style | $_"}), '|}'
}

my @output = <4005 8017 326205 886205 1081439556 503491211079>.map:
  { display $_, .&mayan.map: { (join '
', '●' x .[0], '───' xx .[1]) || 'Θ' } }

say @output.join: "\n
\n";
```


{{out}}
{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 4005
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | Θ
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
|}


{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 8017
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | Θ
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | Θ
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●
───
───
───
|}


{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 326205
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | Θ
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
|}


{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 886205
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
|}


{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 1081439556
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
───
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●
───
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●●
───
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●●●
───
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●●
───
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●
───
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
───
───
───
|}


{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 503491211079
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●●●
───
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●●
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | Θ
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●●
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●●●
───
───
───
|}


## Phix

{{trans|Perl 6}}
Three-way output controlled by the first two constants.

```Phix
-- demo\rosetta\Mayan_numerals.exw
constant as_html = true,        -- false == nasty ascii
         inline_css = true      -- also uses wiki tables ({| etc) if false

string html = ""

constant t_style = "border-collapse: separate; text-align: center; border-spacing: 3px 0px;",
         c_style = "border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;"&
                   "border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;"&
                   "vertical-align: bottom;width: 3.25em;",
         dot = "&#x25cf;",
         bar = "&#x2500;&#x2500;&#x2500;",
         zero = "&#x0398;",
         digits = {" 0  "," .  "," .. ","... ","...."} 

function to_seq(atom a)
    sequence s = {}
    while true do
        s = prepend(s,remainder(a,20))
        a = floor(a/20)
        if a=0 then exit end if
    end while
    return s
end function

procedure show_mayan(atom a)
    sequence s = to_seq(a)
    if not as_html then
        string tb = join(repeat('+',length(s)+1),"------"),
               ln = join(repeat('|',length(s)+1),"      ")
        sequence res = {tb,ln,ln,ln,ln,tb}
        for i=1 to length(s) do
            integer si = s[i], l = 5, m = i*7-4
            while true do
                res[l][m..m+3] = digits[min(si+1,5)]
                si -= 5
                if si<=0 then exit end if
                l -= 1
            end while
        end for
        printf(1,"%d\n%s\n\n",{a,join(res,"\n")})
    else
        for i=1 to length(s) do
            sequence res = repeat("",4)
            integer si = s[i], l = 4
            while true do
                res[l] = iff(si>=5?bar:iff(si?join(repeat(dot,si),""):zero))
                si -= 5
                if si<=0 then exit end if
                l -= 1
            end while
            s[i] = join(res,"
")
        end for
        if inline_css then
            html &= sprintf("  <table>\n   <caption>%d</caption>\n   <tr>\n",a)
            for i=1 to length(s) do
                html &= sprintf("    <td>%s</td>\n",{s[i]})
            end for
            html &= "   </tr>\n  </table>\n"
        else
            html &= sprintf("{| style=\"%s\"\n|+ %d\n|-\n",{t_style,a})
            for i=1 to length(s) do
                html &= sprintf("| style=\"%s\" | %s\n",{c_style,s[i]})
            end for
            html &= "|}\n"
        end if
    end if
end procedure

constant html_header = """
<!DOCTYPE html>
<html lang="en">
 <head>
  <meta charset="utf-8" />
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  <title>Mayan numerals</title>
  <style>
    table {%s}
    td { %s }
  </style>
 </head>
 <body>
  <h2>Mayan numerals</h2>
""",
wiki_header = """
The following is intended to be pasted into the rosettacode wiki, or similar

""",
html_footer = """
 </body>
</html>
"""

constant tests = {4005, 8017, 326205, 886205, 26960840421, 126524984376952}

for i=1 to length(tests) do show_mayan(tests[i]) end for

if as_html then
    string filename = "Mayan_numerals.html"
    integer fn = open(filename,"w")
    if inline_css then
        printf(fn,html_header,{t_style,c_style})
    else
        printf(fn,wiki_header)
    end if
    puts(fn,html)
    if inline_css then
        puts(fn,html_footer)
    end if
    close(fn)
    if inline_css then
        system(filename)
    else
        printf(1,"See %s\n",{filename})
        {} = wait_key()
    end if
else
    ?"done"
    {} = wait_key()
end if
```

{{out}}
With inline_css set to false. Note these are always height-4.
{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 4005
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 

&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x0398;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x2500;&#x2500;&#x2500;
|}
{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 8017
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x25cf;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x0398;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x0398;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | &#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
|}
{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 326205
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x25cf;&#x25cf;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x0398;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 

&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x2500;&#x2500;&#x2500;
|}
{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 886205
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 

&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 

&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x2500;&#x2500;&#x2500;
|}
{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 26960840421
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x25cf;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x25cf;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x25cf;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x25cf;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x25cf;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x25cf;
|}
{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 126524984376952
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 

&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x25cf;&#x25cf;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 

&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | &#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 

&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 


&#x25cf;&#x25cf;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 

&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
|}


{{out}}
With inline_css set to true. As noted on the perl6 entry, this is much neater, but does not display nicely on this page.

<small>
(These captions are centred; to exactly match the above just add a simple caption {text-align: left;} in the style section.)
</small>

```txt

<!DOCTYPE html>
<html lang="en">
 <head>
  <meta charset="utf-8" />
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  <title>Mayan numerals</title>
  <style>
    table {border-collapse: separate; text-align: center; border-spacing: 3px 0px;}
    td { border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;
         -moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em; }
  </style>
 </head>
 <body>
  <h2>Mayan numerals</h2>
  <table>
   <caption>4005</caption>
   <tr>
    <td>

&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;</td>
    <td>


&#x0398;</td>
    <td>


&#x2500;&#x2500;&#x2500;</td>
   </tr>
  </table>
  <table>
   <caption>8017</caption>
   <tr>
    <td>


&#x25cf;</td>
    <td>


&#x0398;</td>
    <td>


&#x0398;</td>
    <td>&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;</td>
   </tr>
  </table>
  <table>
   <caption>326205</caption>
   <tr>
    <td>


&#x25cf;&#x25cf;</td>
    <td>


&#x0398;</td>
    <td>
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;</td>
    <td>

&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;</td>
    <td>


&#x2500;&#x2500;&#x2500;</td>
   </tr>
  </table>
  <table>
   <caption>886205</caption>
   <tr>
    <td>


&#x2500;&#x2500;&#x2500;</td>
    <td>

&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;</td>
    <td>
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;</td>
    <td>

&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;</td>
    <td>


&#x2500;&#x2500;&#x2500;</td>
   </tr>
  </table>
  <table>
   <caption>26960840421</caption>
   <tr>
    <td>


&#x25cf;</td>
    <td>


&#x25cf;</td>
    <td>


&#x25cf;</td>
    <td>


&#x2500;&#x2500;&#x2500;</td>
    <td>


&#x2500;&#x2500;&#x2500;</td>
    <td>


&#x2500;&#x2500;&#x2500;</td>
    <td>


&#x25cf;</td>
    <td>


&#x25cf;</td>
    <td>


&#x25cf;</td>
   </tr>
  </table>
  <table>
   <caption>126524984376952</caption>
   <tr>
    <td>
&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;</td>
    <td>

&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;</td>
    <td>


&#x25cf;&#x25cf;</td>
    <td>

&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;</td>
    <td>
&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;</td>
    <td>&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;</td>
    <td>
&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;</td>
    <td>

&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;</td>
    <td>


&#x25cf;&#x25cf;</td>
    <td>

&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;</td>
    <td>
&#x25cf;&#x25cf;
&#x2500;&#x2500;&#x2500;
&#x2500;&#x2500;&#x2500;</td>
   </tr>
  </table>
 </body>
</html>

```


{{out}}
With output_html set to false.

```txt

4005
+------+------+------+
|      |      |      |
|      |      |      |
| .... |      |      |
| .... |  0   | .... |
+------+------+------+

8017
+------+------+------+------+
|      |      |      |  ..  |
|      |      |      | .... |
|      |      |      | .... |
|  .   |  0   |  0   | .... |
+------+------+------+------+

326205
+------+------+------+------+------+
|      |      |      |      |      |
|      |      | .... |      |      |
|      |      | .... | .... |      |
|  ..  |  0   | .... | .... | .... |
+------+------+------+------+------+

886205
+------+------+------+------+------+
|      |      |      |      |      |
|      |      | .... |      |      |
|      | .... | .... | .... |      |
| .... | .... | .... | .... | .... |
+------+------+------+------+------+

26960840421
+------+------+------+------+------+------+------+------+------+
|      |      |      |      |      |      |      |      |      |
|      |      |      |      |      |      |      |      |      |
|      |      |      |      |      |      |      |      |      |
|  .   |  .   |  .   | .... | .... | .... |  .   |  .   |  .   |
+------+------+------+------+------+------+------+------+------+

126524984376952
+------+------+------+------+------+------+------+------+------+------+------+
|      |      |      |      |      |  ..  |      |      |      |      |      |
|  ..  |      |      |      |  ..  | .... |  ..  |      |      |      |  ..  |
| .... |  ..  |      |  ..  | .... | .... | .... |  ..  |      |  ..  | .... |
| .... | .... |  ..  | .... | .... | .... | .... | .... |  ..  | .... | .... |
+------+------+------+------+------+------+------+------+------+------+------+

```



## Python

{{Works with|Python|3.7}}

```Python
'''Mayan numerals'''

from functools import (reduce)


# MAYAN NUMERALS ------------------------------------------

# mayanNumerals :: Int -> [[String]]
def mayanNumerals(n):
    '''Rows of Mayan digit cells,
       representing the integer n.
    '''
    return showIntAtBase(20)(
        mayanDigit
    )(n)([])


# mayanDigit :: Int -> [String]
def mayanDigit(n):
    '''List of strings representing a Mayan digit.'''
    if 0 < n:
        r = n % 5
        return [
            (['●' * r] if 0 < r else []) +
            (['━━'] * (n // 5))
        ]
    else:
        return ['Θ']


# mayanFramed :: Int -> String
def mayanFramed(n):
    '''Mayan integer in the form of a
       Wiki table source string.
    '''
    return 'Mayan ' + str(n) + ':\n\n' + (
        wikiTable({
            'class': 'wikitable',
            'style': cssFromDict({
                'text-align': 'center',
                'background-color': '#F0EDDE',
                'color': '#605B4B',
                'border': '2px solid silver'
            }),
            'colwidth': '3em',
            'cell': 'vertical-align: bottom;'
        })([[
            '
'.join(col) for col in mayanNumerals(n)
        ]])
    )


# TEST ----------------------------------------------------

# main :: IO ()
def main():
    '''Mayan numeral representations of various integers'''
    print(
        main.__doc__ + ':\n\n' +
        '\n'.join(mayanFramed(n) for n in [
            4005, 8017, 326205, 886205, 1081439556,
            1000000, 1000000000
        ])
    )


# BOXES ---------------------------------------------------

# wikiTable :: Dict -> [[a]] -> String
def wikiTable(opts):
    '''Source text for wiki-table display of rows of cells,
       using CSS key-value pairs in the opts dictionary.
    '''
    def colWidth():
        return 'width:' + opts['colwidth'] + '; ' if (
            'colwidth' in opts
        ) else ''

    def cellStyle():
        return opts['cell'] if 'cell' in opts else ''

    return lambda rows: '{| ' + reduce(
        lambda a, k: (
            a + k + '="' + opts[k] + '" ' if k in opts else a
        ),
        ['class', 'style'],
        ''
    ) + '\n' + '\n|-\n'.join(
        '\n'.join(
            ('|' if (0 != i and ('cell' not in opts)) else (
                '|style="' + colWidth() + cellStyle() + '"|'
            )) + (
                str(x) or ' '
            ) for x in row
        ) for i, row in enumerate(rows)
    ) + '\n|}\n\n'


# GENERIC -------------------------------------------------

# cssFromDict :: Dict -> String
def cssFromDict(dct):
    '''CSS string from a dictinary of key-value pairs'''
    return reduce(
        lambda a, k: a + k + ':' + dct[k] + '; ', dct.keys(), ''
    )


# showIntAtBase :: Int -> (Int -> String) -> Int -> String -> String
def showIntAtBase(base):
    '''String representation of an integer in a given base,
       using a supplied function for the string representation
       of digits.
    '''
    def wrap(toChr, n, rs):
        def go(nd, r):
            n, d = nd
            r_ = toChr(d) + r
            return go(divmod(n, base), r_) if 0 != n else r_
        return 'unsupported base' if 1 >= base else (
            'negative number' if 0 > n else (
                go(divmod(n, base), rs))
        )
    return lambda toChr: lambda n: lambda rs: (
        wrap(toChr, n, rs)
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}
Mayan numeral representations of various integers:

Mayan 4005:

{| class="wikitable" style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver; " 
|style="width:3em; vertical-align: bottom;"|━━
━━
|style="width:3em; vertical-align: bottom;"|Θ
|style="width:3em; vertical-align: bottom;"|━━
|}


Mayan 8017:

{| class="wikitable" style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver; " 
|style="width:3em; vertical-align: bottom;"|●
|style="width:3em; vertical-align: bottom;"|Θ
|style="width:3em; vertical-align: bottom;"|Θ
|style="width:3em; vertical-align: bottom;"|●●
━━
━━
━━
|}


Mayan 326205:

{| class="wikitable" style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver; " 
|style="width:3em; vertical-align: bottom;"|●●
|style="width:3em; vertical-align: bottom;"|Θ
|style="width:3em; vertical-align: bottom;"|━━
━━
━━
|style="width:3em; vertical-align: bottom;"|━━
━━
|style="width:3em; vertical-align: bottom;"|━━
|}


Mayan 886205:

{| class="wikitable" style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver; " 
|style="width:3em; vertical-align: bottom;"|━━
|style="width:3em; vertical-align: bottom;"|━━
━━
|style="width:3em; vertical-align: bottom;"|━━
━━
━━
|style="width:3em; vertical-align: bottom;"|━━
━━
|style="width:3em; vertical-align: bottom;"|━━
|}


Mayan 1081439556:

{| class="wikitable" style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver; " 
|style="width:3em; vertical-align: bottom;"|●
━━
━━
━━
|style="width:3em; vertical-align: bottom;"|●●
━━
━━
━━
|style="width:3em; vertical-align: bottom;"|●●●
━━
━━
━━
|style="width:3em; vertical-align: bottom;"|●●●●
━━
━━
━━
|style="width:3em; vertical-align: bottom;"|●●●
━━
━━
━━
|style="width:3em; vertical-align: bottom;"|●●
━━
━━
━━
|style="width:3em; vertical-align: bottom;"|●
━━
━━
━━
|}


Mayan 1000000:

{| class="wikitable" style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver; " 
|style="width:3em; vertical-align: bottom;"|●
━━
|style="width:3em; vertical-align: bottom;"|━━
|style="width:3em; vertical-align: bottom;"|Θ
|style="width:3em; vertical-align: bottom;"|Θ
|style="width:3em; vertical-align: bottom;"|Θ
|}


Mayan 1000000000:

{| class="wikitable" style="text-align:center; background-color:#F0EDDE; color:#605B4B; border:2px solid silver; " 
|style="width:3em; vertical-align: bottom;"|━━
━━
━━
|style="width:3em; vertical-align: bottom;"|●●
━━
━━
|style="width:3em; vertical-align: bottom;"|━━
━━
|style="width:3em; vertical-align: bottom;"|Θ
|style="width:3em; vertical-align: bottom;"|Θ
|style="width:3em; vertical-align: bottom;"|Θ
|style="width:3em; vertical-align: bottom;"|Θ
|}


## REXX


```rexx
/*REXX program converts decimal numbers to the Mayan numbering system (with cartouches).*/
parse arg $                                      /*obtain optional arguments from the CL*/
if $=''  then $= 4005  8017  326205  886205,     /*Not specified?  Then use the default.*/
                 172037122592320200101           /*Morse code for MAYAN; egg is a blank.*/

  do j=1  for words($)                           /*convert each of the numbers specified*/
  #= word($, j)                                  /*extract a number from (possible) list*/
  say
  say  center('converting the decimal number '     #     " to a Mayan number:", 90,  '─')
  say
  call $MAYAN   #   '(overlap)'                  /*invoke the  $MAYAN (REXX) subroutine.*/
  say
  end   /*j*/                                    /*stick a fork in it,  we're all done. */
```

The   '''$MAYAN.REX'''   (REXX program) subroutine can be seen here   ───►   [[Mayan_numerals\$MAYAN.REX]].

{{out|output|text=  when using the default inputs:}}

```txt

─────────────────converting the decimal number  4005  to a Mayan number:──────────────────

╔════╦════╦════╗
║    ║    ║    ║
║    ║    ║    ║
║────║    ║    ║
║────║ Θ  ║────║
╚════╩════╩════╝


─────────────────converting the decimal number  8017  to a Mayan number:──────────────────

╔════╦════╦════╦════╗
║    ║    ║    ║ ∙∙ ║
║    ║    ║    ║────║
║    ║    ║    ║────║
║ ∙  ║ Θ  ║ Θ  ║────║
╚════╩════╩════╩════╝


────────────────converting the decimal number  326205  to a Mayan number:─────────────────

╔════╦════╦════╦════╦════╗
║    ║    ║    ║    ║    ║
║    ║    ║────║    ║    ║
║    ║    ║────║────║    ║
║ ∙∙ ║ Θ  ║────║────║────║
╚════╩════╩════╩════╩════╝


────────────────converting the decimal number  886205  to a Mayan number:─────────────────

╔════╦════╦════╦════╦════╗
║    ║    ║    ║    ║    ║
║    ║    ║────║    ║    ║
║    ║────║────║────║    ║
║────║────║────║────║────║
╚════╩════╩════╩════╩════╝


─────────converting the decimal number  172037122592320200101  to a Mayan number:─────────

╔════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╦════╗
║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║
║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║
║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║    ║
║────║────║ Θ  ║ ∙  ║────║ Θ  ║────║ ∙  ║────║────║ Θ  ║ ∙  ║────║ Θ  ║────║ ∙  ║
╚════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╩════╝

```



## Rust


```rust
const ONE: &str = "●";
const FIVE: &str = "——";
const ZERO: &str = "Θ";

fn main() {
    println!("{}", mayan(4005));
    println!("{}", mayan(8017));
    println!("{}", mayan(326_205));
    println!("{}", mayan(886_205));
    println!("{}", mayan(69));
    println!("{}", mayan(420));
    println!("{}", mayan(1_063_715_456));
}

fn mayan(dec: i64) -> String {
    let mut digits = vec![];
    let mut num = dec;
    while num > 0 {
        digits.push(num % 20);
        num /= 20;
    }
    digits = digits.into_iter().rev().collect();
    let mut boxes = vec!["".to_string(); 6];
    let n = digits.len();
    for (i, digit) in digits.iter().enumerate() {
        if i == 0 {
            boxes[0] = "┏━━━━".to_string();
            if i == n - 1 {
                boxes[0] += "┓";
            }
        } else if i == n - 1 {
            boxes[0] += "┳━━━━┓";
        } else {
            boxes[0] += "┳━━━━";
        }
        for j in 1..5 {
            boxes[j] += "┃";
            let elem = 0.max(digit - (4 - j as i64) * 5);
            if elem >= 5 {
                boxes[j] += &format!("{: ^4}", FIVE);
            } else if elem > 0 {
                boxes[j] += &format!("{: ^4}", ONE.repeat(elem as usize % 15));
            } else if j == 4 {
                boxes[j] += &format!("{: ^4}", ZERO);
            } else {
                boxes[j] += &"    ";
            }
            if i == n - 1 {
                boxes[j] += "┃";
            }
        }
        if i == 0 {
            boxes[5] = "┗━━━━".to_string();
            if i == n - 1 {
                boxes[5] += "┛";
            }
        } else if i == n - 1 {
            boxes[5] += "┻━━━━┛";
        } else {
            boxes[5] += "┻━━━━";
        }
    }

    let mut mayan = format!("Mayan {}:\n", dec);
    for b in boxes {
        mayan += &(b + "\n");
    }
    mayan
}
```

{{out}}

```txt

Mayan 4005:
┏━━━━┳━━━━┳━━━━┓
┃    ┃    ┃    ┃
┃    ┃    ┃    ┃
┃ —— ┃    ┃    ┃
┃ —— ┃ Θ  ┃ —— ┃
┗━━━━┻━━━━┻━━━━┛

Mayan 8017:
┏━━━━┳━━━━┳━━━━┳━━━━┓
┃    ┃    ┃    ┃ ●● ┃
┃    ┃    ┃    ┃ —— ┃
┃    ┃    ┃    ┃ —— ┃
┃ ●  ┃ Θ  ┃ Θ  ┃ —— ┃
┗━━━━┻━━━━┻━━━━┻━━━━┛

Mayan 326205:
┏━━━━┳━━━━┳━━━━┳━━━━┳━━━━┓
┃    ┃    ┃    ┃    ┃    ┃
┃    ┃    ┃ —— ┃    ┃    ┃
┃    ┃    ┃ —— ┃ —— ┃    ┃
┃ ●● ┃ Θ  ┃ —— ┃ —— ┃ —— ┃
┗━━━━┻━━━━┻━━━━┻━━━━┻━━━━┛

Mayan 886205:
┏━━━━┳━━━━┳━━━━┳━━━━┳━━━━┓
┃    ┃    ┃    ┃    ┃    ┃
┃    ┃    ┃ —— ┃    ┃    ┃
┃    ┃ —— ┃ —— ┃ —— ┃    ┃
┃ —— ┃ —— ┃ —— ┃ —— ┃ —— ┃
┗━━━━┻━━━━┻━━━━┻━━━━┻━━━━┛

Mayan 69:
┏━━━━┳━━━━┓
┃    ┃    ┃
┃    ┃    ┃
┃    ┃●●●●┃
┃●●● ┃ —— ┃
┗━━━━┻━━━━┛

Mayan 420:
┏━━━━┳━━━━┳━━━━┓
┃    ┃    ┃    ┃
┃    ┃    ┃    ┃
┃    ┃    ┃    ┃
┃ ●  ┃ ●  ┃ Θ  ┃
┗━━━━┻━━━━┻━━━━┛

Mayan 1063715456:
┏━━━━┳━━━━┳━━━━┳━━━━┳━━━━┳━━━━┳━━━━┓
┃ ●  ┃    ┃    ┃    ┃    ┃    ┃ ●  ┃
┃ —— ┃ ●● ┃    ┃    ┃    ┃ ●● ┃ —— ┃
┃ —— ┃ —— ┃●●● ┃    ┃●●● ┃ —— ┃ —— ┃
┃ —— ┃ —— ┃ —— ┃●●●●┃ —— ┃ —— ┃ —— ┃
┗━━━━┻━━━━┻━━━━┻━━━━┻━━━━┻━━━━┻━━━━┛

```



## zkl


```zkl
var zero=" \u0398  ",one="\u2219", five=String("\u2500"*4,"\n"), fill="    \n";
var ds=T("    "," \u2219  "," \u2219\u2219 ","\u2219\u2219\u2219 ","\u2219\u2219\u2219\u2219");
fcn mayan(m){  //--> lists of lists of strings (each line of tile)
   m.toString(20).pump(List,fcn(m){		// 4,005 --> "a05"
      bars,dots := m.toInt(20).divr(5);		// "a" --> 10 --> (2,0)
      if(0==bars==dots) return(String(fill*3, zero).split("\n"));  // 0
      String(fill*(3-bars), ds[dots],"\n", five*bars)[0,-1].split("\n") // tile
   })
}
```


```zkl
var vbar="\u2551", hbar="\u2550"*4, ut=(hbar+"\u2566"), bt=(hbar+"\u2569");
fcn displayMayan(m){  // eg 11 is ( (    ),( *  ),(----),(----) )
   println("\u2554", ut*(m.len()-1), hbar,"\u2557");  // top
   foreach n in (4){ println(vbar, m.apply("get",n).concat(vbar), vbar) }
   println("\u255a", bt*(m.len()-1), hbar,"\u255d");  // bottom
}
```


```zkl
foreach m in (T(4_005, 8_017, 326_205, 886_205, 503_491_211_079, 88_637_341)){
   println("\n%,d:".fmt(m)); mayan(m):displayMayan(_);
}
```

{{out}}
<pre style="height:40ex">
4,005:
╔════╦════╦════╗
║    ║    ║    ║
║    ║    ║    ║
║────║    ║    ║
║────║ Θ  ║────║
╚════╩════╩════╝

8,017:
╔════╦════╦════╦════╗
║    ║    ║    ║ ∙∙ ║
║    ║    ║    ║────║
║    ║    ║    ║────║
║ ∙  ║ Θ  ║ Θ  ║────║
╚════╩════╩════╩════╝

326,205:
╔════╦════╦════╦════╦════╗
║    ║    ║    ║    ║    ║
║    ║    ║────║    ║    ║
║    ║    ║────║────║    ║
║ ∙∙ ║ Θ  ║────║────║────║
╚════╩════╩════╩════╩════╝

886,205:
╔════╦════╦════╦════╦════╗
║    ║    ║    ║    ║    ║
║    ║    ║────║    ║    ║
║    ║────║────║────║    ║
║────║────║────║────║────║
╚════╩════╩════╩════╩════╝

503,491,211,079:
╔════╦════╦════╦════╦════╦════╦════╦════╦════╗
║∙∙∙∙║    ║    ║    ║    ║    ║    ║    ║∙∙∙∙║
║────║∙∙∙ ║    ║    ║    ║    ║    ║∙∙∙ ║────║
║────║────║ ∙∙ ║    ║    ║    ║ ∙∙ ║────║────║
║────║────║────║ ∙  ║ Θ  ║ ∙  ║────║────║────║
╚════╩════╩════╩════╩════╩════╩════╩════╩════╝

88,637,341:
╔════╦════╦════╦════╦════╦════╦════╗
║    ║    ║    ║∙∙∙∙║    ║    ║    ║
║    ║    ║∙∙∙ ║────║∙∙∙ ║    ║    ║
║    ║ ∙∙ ║────║────║────║ ∙∙ ║    ║
║ ∙  ║────║────║────║────║────║ ∙  ║
╚════╩════╩════╩════╩════╩════╩════╝

```

The Perl6 numerals look so good, let's copy them.
{{trans|Perl6}}

```zkl
var [const]
  t_style=0''"border-collapse: separate; text-align: center; border-spacing: 3px 0px;"',
  c_style=0''"border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;'
    0''border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;'
    0''vertical-align: bottom;width: 3.25em;"';
var [const] dot="\u25cf", bar="\u2500\u2500\u2500!", zero=" \u0398 ";

fcn mayan(m){ m.toString(20).pump(List,fcn(m){ m.toInt(20).divr(5) }) }
fcn display(num,digits){
   "{| style=%s\n|+ %s \n|-\n%s|}".fmt(t_style,num,
      digits.split().pump(String, "| style=%s | %s\n".fmt.fp(c_style)) );
}

T(88637341,26960840421).apply(
   fcn(m){ 
      mayan(m).pump(String,
	 fcn([(bars,dots)]){
	    (if(bars or dots) (dot*dots + "!" + bar*bars)[0,-1] else zero)
	    + " "
	 }).replace("!","
") : display(m,_)
   })
.concat("\n
\n").println();
```

{{out}}
{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 88637341 
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●●
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●●●
───
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●●
───
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●●
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
|}


{| style="border-collapse: separate; text-align: center; border-spacing: 3px 0px;"
|+ 26960840421 
|-
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | 
───
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
| style="border: solid black 2px;background-color: #fffff0;border-bottom: double 6px;border-radius: 1em;-moz-border-radius: 1em;-webkit-border-radius: 1em;vertical-align: bottom;width: 3.25em;" | ●
|}
