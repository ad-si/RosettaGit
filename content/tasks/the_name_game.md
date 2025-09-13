+++
title = "The Name Game"
description = ""
date = 2019-07-04T15:14:29Z
aliases = []
[extra]
id = 21761
[taxonomies]
categories = ["task"]
tags = []
+++

Write a program that accepts a name as input and outputs the lyrics to the Shirley Ellis song "The Name Game"

The regular verse

Unless your name begins with a vowel (A, E, I, O, U), 'B', 'F' or 'M' you don't have to care about special rules.
The verse for the name 'Gary' would be like this:

    Gary, Gary, bo-bary
    Banana-fana fo-fary
    Fee-fi-mo-mary
    Gary!

At the end of every line, the name gets repeated without the first letter: Gary becomes ary
If we take (X) as the full name (Gary) and (Y) as the name without the first letter (ary) the verse would look like this:

    (X), (X), bo-b(Y)
    Banana-fana fo-f(Y)
    Fee-fi-mo-m(Y)
    (X)!

Vowel as first letter of the name

If you have a vowel as the first letter of your name (e.g. Earl) you do not truncate the name.
The verse looks like this:

    Earl, Earl, bo-bearl
    Banana-fana fo-fearl
    Fee-fi-mo-mearl
    Earl!

'B', 'F' or 'M' as first letter of the name

In case of a 'B', an 'F' or an 'M' (e.g. Billy, Felix, Mary) there is a special rule.
The line which would 'rebuild' the name (e.g. bo-billy) is sang without the first letter of the name.
The verse for the name Billy looks like this:

    Billy, Billy, bo-illy
    Banana-fana fo-filly
    Fee-fi-mo-milly
    Billy!

For the name 'Felix', this would be right:

    Felix, Felix, bo-belix
    Banana-fana fo-elix
    Fee-fi-mo-melix
    Felix!



## Ada

```Ada
with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

procedure The_Name_Game
is
   package ACH renames Ada.Characters.Handling;
   package ASU renames Ada.Strings.Unbounded;
   function "+"(input : in String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;
   function "+"(input : in ASU.Unbounded_String) return String renames ASU.To_String;

   function Normalize_Case(input : in String) return String is
   begin
      return ACH.To_Upper(input(input'First))
        & ACH.To_Lower(input(input'First + 1 .. input'Last));
   end Normalize_Case;

   function Transform(input : in String; letter : in Character) return String is
   begin
      case input(input'First) is
         when 'A' | 'E' | 'I' | 'O' | 'U' =>
            return letter & ACH.To_Lower(input);
         when others =>
            if ACH.To_Lower(input(input'First)) = letter then
               return input(input'First + 1 .. input'Last);
            else
               return letter & input(input'First + 1 .. input'Last);
            end if;
      end case;
   end Transform;

   procedure Lyrics(name : in String)
   is
      normalized : constant String := Normalize_Case(name);
   begin
      Ada.Text_IO.Put_Line(normalized & ", " & normalized & ", bo-" & Transform(normalized, 'b'));
      Ada.Text_IO.Put_Line("Banana-fana, fo-" & Transform(normalized, 'f'));
      Ada.Text_IO.Put_Line("fi-fee-mo-" & Transform(normalized, 'm'));
      Ada.Text_IO.Put_Line(normalized & '!');
      Ada.Text_IO.New_Line;
   end Lyrics;

   names : constant array(1 .. 5) of ASU.Unbounded_String :=
     (+"Gary",
      +"EARL",
      +"billy",
      +"FeLiX",
      +"Mary");
begin
   for name of names loop
      Lyrics(+name);
   end loop;
end The_Name_Game;
```

```txt
Gary, Gary, bo-bary
Banana-fana, fo-fary
fi-fee-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana, fo-fearl
fi-fee-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana, fo-filly
fi-fee-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana, fo-elix
fi-fee-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana, fo-fary
fi-fee-mo-ary
Mary!


```



## ALGOL 68

```algol68

main:(
  PROC print lyrics = (STRING name) VOID:
    BEGIN
      PROC change name = (STRING name, CHAR initial) STRING:
        BEGIN
          CHAR lower first = to lower(name[1]);
          IF char in string(lower first, NIL, "aeiou") THEN
            lower first + name[2:]
          ELIF lower first = initial THEN
            name[2:]
          ELSE
            initial + name[2:]
          FI
        END;

    print((name, ", ", name, ", bo-", change name(name, "b"), new line,
           "Banana-fana fo-", change name(name, "f"), new line,
           "Fee-fi-mo-", change name(name, "m"), new line,
           name, "!", new line))
  END;

  []STRING names = ("Gary", "Earl", "Billy", "Felix", "Mary");

  FOR i FROM LWB names TO UPB names DO
    print lyrics(names[i]);
    print(new line)
  OD
)

```



## AWK


```AWK

# syntax: GAWK -f THE_NAME_GAME.AWK
BEGIN {
    n = split("gary,earl,billy,felix,mary,shirley",arr,",")
    for (i=1; i<=n; i++) {
      print_verse(arr[i])
    }
    exit(0)
}
function print_verse(name,  c,x,y) {
    x = toupper(substr(name,1,1)) tolower(substr(name,2))
    y = (x ~ /^[AEIOU]/) ? tolower(x) : substr(x,2)
    c = substr(x,1,1)
    printf("%s, %s, bo-%s%s\n",x,x,(c~/B/)?"":"b",y)
    printf("Banana-fana fo-%s%s\n",(c~/F/)?"":"f",y)
    printf("Fee-fi-mo-%s%s\n",(c~/M/)?"":"m",y)
    printf("%s!\n\n",x)
}

```

```txt

Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Shirley, Shirley, bo-bhirley
Banana-fana fo-fhirley
Fee-fi-mo-mhirley
Shirley!


```


## C

```c
#include <stdio.h>
#include <string.h>

void print_verse(const char *name) {
    char *x, *y;
    int b = 1, f = 1, m = 1, i = 1;

    /* ensure name is in title-case */
    x = strdup(name);
    x[0] = toupper(x[0]);
    for (; x[i]; ++i) x[i] = tolower(x[i]);

    if (strchr("AEIOU", x[0])) {
        y = strdup(x);
        y[0] = tolower(y[0]);
    }
    else {
        y = x + 1;
    }

    switch(x[0]) {
        case 'B': b = 0; break;
        case 'F': f = 0; break;
        case 'M': m = 0; break;
        default : break;
    }

    printf("%s, %s, bo-%s%s\n", x, x, (b) ? "b" : "", y);
    printf("Banana-fana fo-%s%s\n", (f) ? "f" : "", y);
    printf("Fee-fi-mo-%s%s\n", (m) ? "m" : "", y);
    printf("%s!\n\n", x);
}

int main() {
    int i;
    const char *names[6] = {"gARY", "Earl", "Billy", "Felix", "Mary", "sHIRley"};
    for (i = 0; i < 6; ++i) print_verse(names[i]);
    return 0;
}
```


```txt

Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Shirley, Shirley, bo-bhirley
Banana-fana fo-fhirley
Fee-fi-mo-mhirley
Shirley!

```



## C++

```cpp
#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

static void printVerse(const std::string& name) {
    std::string x = name;
    std::transform(x.begin(), x.end(), x.begin(), ::tolower);
    x[0] = toupper(x[0]);

    std::string y;
    switch (x[0]) {
    case 'A':
    case 'E':
    case 'I':
    case 'O':
    case 'U':
        y = x;
        std::transform(y.begin(), y.end(), y.begin(), ::tolower);
        break;
    default:
        y = x.substr(1);
        break;
    }

    std::string b("b" + y);
    std::string f("f" + y);
    std::string m("m" + y);

    switch (x[0]) {
    case 'B':
        b = y;
        break;
    case 'F':
        f = y;
        break;
    case 'M':
        m = y;
        break;
    default:
        break;
    }

    printf("%s, %s, bo-%s\n", x.c_str(), x.c_str(), b.c_str());
    printf("Banana-fana fo-%s\n", f.c_str());
    printf("Fee-fi-mo-%s\n", m.c_str());
    printf("%s!\n\n", x.c_str());
}

int main() {
    using namespace std;

    vector<string> nameList{ "Gary", "Earl", "Billy", "Felix", "Mary", "Steve" };
    for (auto& name : nameList) {
        printVerse(name);
    }

    return 0;
}
```

```txt
Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Steve, Steve, bo-bteve
Banana-fana fo-fteve
Fee-fi-mo-mteve
Steve!
```


## C#
```c#
using System;
using System.Collections.Generic;
using System.Text;

namespace TheNameGame {
    class Program {
        static void PrintVerse(string name) {
            StringBuilder sb = new StringBuilder(name.ToLower());
            sb[0] = Char.ToUpper(sb[0]);
            string x = sb.ToString();
            string y = "AEIOU".IndexOf(x[0]) > -1 ? x.ToLower() : x.Substring(1);
            string b = "b" + y;
            string f = "f" + y;
            string m = "m" + y;
            switch (x[0]) {
                case 'B':
                    b = y;
                    break;
                case 'F':
                    f = y;
                    break;
                case 'M':
                    m = y;
                    break;
            }
            Console.WriteLine("{0}, {0}, bo-{1}", x, b);
            Console.WriteLine("Banana-fana fo-{0}", f);
            Console.WriteLine("Fee-fi-mo-{0}", m);
            Console.WriteLine("{0}!", x);
            Console.WriteLine();
        }

        static void Main(string[] args) {
            List<string> nameList = new List<string>() { "Gary", "Earl", "Billy", "Felix", "Mary", "Steve" };
            nameList.ForEach(PrintVerse);
        }
    }
}
```

```txt
Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Steve, Steve, bo-bteve
Banana-fana fo-fteve
Fee-fi-mo-mteve
Steve!
```



## D


```d
import std.algorithm;
import std.array;
import std.conv;
import std.stdio;
import std.uni;

void printVerse(string name) {
    auto sb = name.map!toLower.array;
    sb[0] = sb[0].toUpper;

    string x = sb.to!string;
    string y;
    switch(sb[0]) {
        case 'A':
        case 'E':
        case 'I':
        case 'O':
        case 'U':
            y = x.map!toLower.to!string;
            break;
        default:
            y = x[1..$];
            break;
    }
    string b = "b" ~ y;
    string f = "f" ~ y;
    string m = "m" ~ y;
    switch (x[0]) {
        case 'B':
            b = y;
            break;
        case 'F':
            f = y;
            break;
        case 'M':
            m = y;
            break;
        default:
            // no adjustment needed
            break;
    }

    writeln(x, ", ", x, ", bo-", b);
    writeln("Banana-fana fo-", f);
    writeln("Fee-fi-mo-", m);
    writeln(x, "!\n");
}

void main() {
    foreach (name; ["Gary","Earl","Billy","Felix","Mary","steve"]) {
        printVerse(name);
    }
}
```


=={{header|F_Sharp|F#}}==

### The function


```fsharp

// The Name Game. Nigel Galloway: March 28th., 2018
let   fN g =
  let fG α β γ = printfn "%s, %s, bo-%s\nBanana-fana fo-%s\nFee-fi-mo-%s\n%s!" g g α β γ g
  match g.ToLower().[0] with
  |'a'|'e'|'i'|'o'|'u' as n  -> fG ("b"+(string n)+g.[1..]) ("f"+(string n)+g.[1..]) ("m"+(string n)+g.[1..])
  |'b'                       -> fG (g.[1..]) ("f"+g.[1..]) ("m"+g.[1..])
  |'f'                       -> fG ("b"+g.[1..]) (g.[1..]) ("m"+g.[1..])
  |'m'                       -> fG ("b"+g.[1..]) ("f"+g.[1..]) (g.[1..])
  |_                         -> fG ("b"+g.[1..]) ("f"+g.[1..]) ("m"+g.[1..])

```


### The Task


```fsharp

fN "Nigel"

```

```txt

Nigel, Nigel, bo-bigel
Banana-fana fo-figel
Fee-fi-mo-migel
Nigel!

```


```fsharp

fN "Earl"

```

```txt

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

```


```fsharp

fN "Billy"

```

```txt

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

```


```fsharp

fN "Fred"

```

```txt

Fred, Fred, bo-bred
Banana-fana fo-red
Fee-fi-mo-mred
Fred!

```


```fsharp

fN "Mum"

```

```txt

Mum, Mum, bo-bum
Banana-fana fo-fum
Fee-fi-mo-um
Mum!

```



## Factor

```factor
USING: ascii combinators interpolate io kernel locals
pair-rocket qw sequences ;
IN: rosetta-code.name-game

: vowel? ( char -- ? ) "AEIOU" member? ;

:: name-game ( Name -- )

    Name first  :> L
    Name >lower :> name! L vowel? [ name rest name! ] unless
    "b"         :> B!
    "f"         :> F!
    "m"         :> M!

    L { CHAR: B => [ "" B! ]
        CHAR: F => [ "" F! ]
        CHAR: M => [ "" M! ] [ drop ] } case

[I ${Name}, ${Name}, bo-${B}${name}
Banana-fana fo-${F}${name}
Fee-fi-mo-${M}${name}
${Name}!I] nl nl ;

qw{ Gary Earl Billy Felix Milton Steve } [ name-game ] each
```

```txt

Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Milton, Milton, bo-bilton
Banana-fana fo-filton
Fee-fi-mo-ilton
Milton!

Steve, Steve, bo-bteve
Banana-fana fo-fteve
Fee-fi-mo-mteve
Steve!

```



## Go

```go
package main

import (
    "fmt"
    "strings"
)

func printVerse(name string) {
    x := strings.Title(strings.ToLower(name))
    y := x[1:]
    if strings.Contains("AEIOU", x[:1]) {
        y = strings.ToLower(x)
    }
    b := "b" + y
    f := "f" + y
    m := "m" + y
    switch x[0] {
    case 'B':
        b = y
    case 'F':
        f = y
    case 'M':
        m = y
    }
    fmt.Printf("%s, %s, bo-%s\n", x, x, b)
    fmt.Printf("Banana-fana fo-%s\n", f)
    fmt.Printf("Fee-fi-mo-%s\n", m)
    fmt.Printf("%s!\n\n", x)
}

func main() {
    names := [6]string{"gARY", "Earl", "Billy", "Felix", "Mary", "SHIRley"}
    for _, name := range names {
        printVerse(name)
    }
}
```


```txt

Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Shirley, Shirley, bo-bhirley
Banana-fana fo-fhirley
Fee-fi-mo-mhirley
Shirley!

```



## Haskell


```Haskell

-- The Name Game, Ethan Riley, 22nd May 2018
import Data.Char

isVowel :: Char -> Bool
isVowel c
    | char == 'A' = True
    | char == 'E' = True
    | char == 'I' = True
    | char == 'O' = True
    | char == 'U' = True
    | otherwise = False
    where char = toUpper c

isSpecial :: Char -> Bool
isSpecial c
    | char == 'B' = True
    | char == 'F' = True
    | char == 'M' = True
    | otherwise = False
    where char = toUpper c

shorten :: String -> String
shorten name
    | isVowel $ head name = map toLower name
    | otherwise = map toLower $ tail name

line :: String -> Char -> String -> String
line prefix letter name
    | letter == char = prefix ++ shorten name ++ "\n"
    | otherwise = prefix ++ letter:[] ++ shorten name ++ "\n"
    where char = toLower $ head name

theNameGame :: String -> String
theNameGame name =
    line (name ++ ", " ++ name ++ ", bo-") 'b' name ++
    line "Banana-fana fo-" 'f' name ++
    line "Fee-fi-mo-" 'm' name ++
    name ++ "!\n"

main =
    mapM_ (putStrLn . theNameGame) ["Gary", "Earl", "Billy", "Felix", "Mike", "Steve"]

```

```txt

Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mike, Mike, bo-bike
Banana-fana fo-fike
Fee-fi-mo-ike
Mike!

Steve, Steve, bo-bteve
Banana-fana fo-fteve
Fee-fi-mo-mteve
Steve!

```



## Java

```Java
import java.util.stream.Stream;

public class NameGame {
    private static void printVerse(String name) {
        StringBuilder sb = new StringBuilder(name.toLowerCase());
        sb.setCharAt(0, Character.toUpperCase(sb.charAt(0)));
        String x = sb.toString();
        String y = "AEIOU".indexOf(x.charAt(0)) > -1 ? x.toLowerCase() : x.substring(1);
        String b = "b" + y;
        String f = "f" + y;
        String m = "m" + y;
        switch (x.charAt(0)) {
            case 'B':
                b = y;
                break;
            case 'F':
                f = y;
                break;
            case 'M':
                m = y;
                break;
            default:
                // no adjustment needed
                break;
        }
        System.out.printf("%s, %s, bo-%s\n", x, x, b);
        System.out.printf("Banana-fana fo-%s\n", f);
        System.out.printf("Fee-fi-mo-%s\n", m);
        System.out.printf("%s!\n\n", x);
    }

    public static void main(String[] args) {
        Stream.of("Gary", "Earl", "Billy", "Felix", "Mary", "Steve").forEach(NameGame::printVerse);
    }
}
```

```txt
Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Steve, Steve, bo-bteve
Banana-fana fo-fteve
Fee-fi-mo-mteve
Steve!
```



## Julia


```julia
import Compat: uppercasefirst

function printverse(name::AbstractString)
    X = uppercasefirst(lowercase(name))
    Y = X[1] ∈ ('A', 'E', 'I', 'O', 'U') ? X : SubString(X, 2)
    b = X[1] == 'B' ? "" : "b"
    f = X[1] == 'F' ? "" : "f"
    m = X[1] == 'M' ? "" : "m"
    println("""\
    $(X), $(X), bo-$b$(Y)
    Banana-fana fo-$f$(Y)
    Fee-fi-mo-$m$(Y)
    $(X)!
    """)
    return nothing
end

foreach(TheNameGame.printverse, ("gARY", "Earl", "Billy", "Felix", "Mary", "sHIRley"))
```


```txt
Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bEarl
Banana-fana fo-fEarl
Fee-fi-mo-mEarl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Shirley, Shirley, bo-bhirley
Banana-fana fo-fhirley
Fee-fi-mo-mhirley
Shirley!
```



## Kotlin


```scala
// version 1.2.31

fun printVerse(name: String) {
    val x = name.toLowerCase().capitalize()
    val y = if (x[0] in "AEIOU") x.toLowerCase() else x.substring(1)
    var b = "b$y"
    var f = "f$y"
    var m = "m$y"
    when (x[0]) {
        'B'  -> b = "$y"
        'F'  -> f = "$y"
        'M'  -> m = "$y"
        else -> {} // no adjustment needed
    }
    println("$x, $x, bo-$b")
    println("Banana-fana fo-$f")
    println("Fee-fi-mo-$m")
    println("$x!\n")
}

fun main(args: Array<String>) {
    listOf("Gary", "Earl", "Billy", "Felix", "Mary", "Steve").forEach { printVerse(it) }
}
```


```txt

Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Steve, Steve, bo-bteve
Banana-fana fo-fteve
Fee-fi-mo-mteve
Steve!

```


## Lua

```lua
function printVerse(name)
    local sb = string.lower(name)
    sb = sb:gsub("^%l", string.upper)
    local x = sb
    local x0 = x:sub(1,1)

    local y
    if x0 == 'A' or x0 == 'E' or x0 == 'I' or x0 == 'O' or x0 == 'U' then
        y = string.lower(x)
    else
        y = x:sub(2)
    end

    local b = "b" .. y
    local f = "f" .. y
    local m = "m" .. y

    if x0 == 'B' then
        b = y
    elseif x0 == 'F' then
        f = y
    elseif x0 == 'M' then
        m = y
    end

    print(x .. ", " .. x .. ", bo-" .. b)
    print("Banana-fana fo-" .. f)
    print("Fee-fi-mo-" .. m)
    print(x .. "!")
    print()

    return nil
end

local nameList = { "Gary", "Earl", "Billy", "Felix", "Mary", "Steve" }
for _,name in pairs(nameList) do
    printVerse(name)
end
```

```txt
Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Steve, Steve, bo-bteve
Banana-fana fo-fteve
Fee-fi-mo-mteve
Steve!
```



## M2000 Interpreter

Function format$ with one parameter (a string) process escape codes
so we can alter line song$ ...
      song$=format$("\r\n{0}, {0}, bo-{2}{1}\r\nBanana-fana fo-{3}{1}\r\nFee-fi-mo-{4}{1}\r\n{0}!\r\n")


```M2000 Interpreter

Module The.Name.Game {
      Flush
      Data "Gary", "Earl","Billy","Felix"
      Document doc$="The Name Game"+{
      }
      Report doc$
      song$={
         {0}, {0}, bo-{2}{1}
         Banana-fana fo-{3}{1}
         Fee-fi-mo-{4}{1}
         {0}!
         }
      While not empty {
            Read x$
            x$=ucase$(left$(x$,1))+lcase$(mid$(x$,2))
            b$=if$(x$ ~ "B*"->"", "b")
            f$=if$(x$ ~ "F*"->"", "f")
            m$=if$(x$ ~ "M*"->"", "m")
            y$=if$(x$ ~ "[AEIOU]*"->lcase$(x$),Mid$(x$, 2))
            toprint$=format$(song$, x$, y$, b$, f$, m$)
            doc$=toprint$
            report toprint$
      }
      Clipboard doc$
}
The.Name.Game

```


<pre style="height:30ex;overflow:scroll">
The Name Game

Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!
</pre >


## min

```min
("AEIOU" "" split swap in?) :vowel?

(
  :Name
  Name "" split first :L
  Name lowercase :name (L vowel?) (name "" split rest "" join @name) unless
  "b" :B
  "f" :F
  "m" :M
  (
    ((L "B" ==) ("" @B))
    ((L "F" ==) ("" @F))
    ((L "M" ==) ("" @M))
  ) case
  "$1, $1, bo-$3$2\nBanana-fana fo-$4$2\nFee-fi-mo-$5$2\n$1!\n"
  (Name name B F M) => % puts!
) :name-game

("Gary" "Earl" "Billy" "Felix" "Milton" "Steve") 'name-game foreach
```

```txt

Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Milton, Milton, bo-bilton
Banana-fana fo-filton
Fee-fi-mo-ilton
Milton!

Steve, Steve, bo-bteve
Banana-fana fo-fteve
Fee-fi-mo-mteve
Steve!

```


=={{header|Modula-2}}==

```modula2
MODULE NameGame;
FROM Strings IMPORT Concat;
FROM ExStrings IMPORT Lowercase;
FROM Terminal IMPORT WriteString, WriteLn, ReadChar;

PROCEDURE PrintVerse(name : ARRAY OF CHAR);
TYPE String = ARRAY[0..64] OF CHAR;
VAR y,b,f,m : String;
BEGIN
    Lowercase(name);

    CASE name[0] OF
        'a','e','i','o','u' : y := name;
    ELSE
        y := name[1..LENGTH(name)];
    END;

    Concat("b", y, b);
    Concat("f", y, f);
    Concat("m", y, m);

    CASE name[0] OF
        'b' : b := y; |
        'f' : f := y; |
        'm' : m := y;
    ELSE
    END;

    name[0] := CAP(name[0]);

    (* Line 1 *)
    WriteString(name);
    WriteString(", ");
    WriteString(name);
    WriteString(", bo-");
    WriteString(b);
    WriteLn;

    (* Line 2 *)
    WriteString("Banana-fana fo-");
    WriteString(f);
    WriteLn;

    (* Line 3 *)
    WriteString("Fee-fi-mo-");
    WriteString(m);
    WriteLn;

    (* Line 4 *)
    WriteString(name);
    WriteString("!");
    WriteLn;

    WriteLn;
END PrintVerse;

BEGIN
    PrintVerse("Gary");
    PrintVerse("Earl");
    PrintVerse("Billy");
    PrintVerse("Felix");
    PrintVerse("Mary");
    PrintVerse("Steve");

    ReadChar;
END NameGame.
```



## Perl

```perl
sub printVerse {
    $x  = ucfirst lc shift;
    $x0 = substr $x, 0, 1;
    $y  = $x0 =~ /[AEIOU]/ ? lc $x : substr $x, 1;
    $b  = $x0 eq 'B' ? $y : 'b' . $y;
    $f  = $x0 eq 'F' ? $y : 'f' . $y;
    $m  = $x0 eq 'M' ? $y : 'm' . $y;
    print "$x, $x, bo-$b\n" .
          "Banana-fana fo-$f\n" .
          "Fee-fi-mo-$m\n" .
          "$x!\n\n";
}

printVerse($_) for <Gary Earl Billy Felix Mary Steve>;
```

```txt
Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Steve, Steve, bo-bteve
Banana-fana fo-fteve
Fee-fi-mo-mteve
Steve!
```



## Perl 6

Meh. The rules leave out some corner cases (see Steve) but what the heck, technically correct is the best kind of correct.


```perl6
sub mangle ($name, $initial) {
    my $fl = $name.lc.substr(0,1);
    $fl ~~ /<[aeiou]>/
    ?? $initial~$name.lc
    !! $fl eq $initial
    ?? $name.substr(1)
    !! $initial~$name.substr(1)
}

sub name-game (Str $name) {
    qq:to/NAME-GAME/;
    $name, $name, bo-{ mangle $name, 'b' }
    Banana-fana fo-{ mangle $name, 'f' }
    Fee-fi-mo-{ mangle $name, 'm' }
    $name!
    NAME-GAME
}

say .&name-game for <Gary Earl Billy Felix Mike Steve>
```

```txt
Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mike, Mike, bo-bike
Banana-fana fo-fike
Fee-fi-mo-ike
Mike!

Steve, Steve, bo-bteve
Banana-fana fo-fteve
Fee-fi-mo-mteve
Steve!

```



## Phix

```Phix
constant fmt = """
%s, %s, bo-%s
Banana-fana fo-%s
Fee-fi-mo-%s
%s!

"""
procedure printVerse(string name)
    string x = lower(name)
    integer x1 = upper(x[1]),
            vowel = find(x1,"AEIUO")!=0
    string y = x[2-vowel..$],
           b = 'b'&y, f = 'f'&y, m = 'm'&y
    x[1] = x1
    switch x1 do
        case 'B': b = y
        case 'F': f = y
        case 'M': m = y
    end switch
    printf(1,fmt,{x, x, b, f, m, x})
end procedure

constant tests = {"gARY", "Earl", "Billy", "Felix", "Mary", "SHIRley"}
for i=1 to length(tests) do printVerse(tests[i]) end for
```

```txt

Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Shirley, Shirley, bo-bhirley
Banana-fana fo-fhirley
Fee-fi-mo-mhirley
Shirley!

```



## Python


```Python
def print_verse(n):
    l = ['b', 'f', 'm']
    s = n[1:]
    if str.lower(n[0]) in l:
        l[l.index(str.lower(n[0]))] = ''
    elif n[0] in ['A', 'E', 'I', 'O', 'U']:
        s = str.lower(n)
    print('{0}, {0}, bo-{2}{1}\nBanana-fana fo-{3}{1}\nFee-fi-mo-{4}{1}\n{0}!\n'.format(n, s, *l))

# Assume that the names are in title-case and they're more than one character long
for n in ['Gary', 'Earl', 'Billy', 'Felix', 'Mary']:
    print_verse(n)
```



## REXX

Extra code was added to the REXX program to capitalize the name  (and lowercase all characters in the name except the 1<sup>st</sup> character).

Also, dual names are supported  (like Mary Ann).

```rexx
/*REXX program  displays the  lyrics  of the  song  "The Name Game"  by Shirley Ellis.  */
parse arg $                                      /*obtain optional argument(s) from C.L.*/
if $=''  then $="gAry, eARL, billy, FeLix, MarY" /*Not specified?  Then use the default.*/
                                                 /* [↑]  names separated by commas.     */
      do j=1  until $='';    $=space($)          /*elide superfluous blanks from list.  */
      parse var  $  name','  $                   /*get name (could be 2 words) from list*/
      call song     name                         /*invoke subroutine to display lyrics. */
      end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
song: arg c 2 1 z;  @b='b';    @f="f";    @m='m' /*obtain name;  assign three variables.*/
      @abc= 'abcdefghijklmnopqrstuvwxyz'; @abcU=@abc; upper @abcU    /*build 2 alphabets*/
      z=c || translate( substr(z, 2),@abc,@abcU) /*capitalize name, lowercase the rest. */
      parse var   z   f  2  ''  1  z             /*get name, 1st letter, rest of name.  */
      y=substr(z, 2);     zl=translate(z, @abc, @abcU)               /*lowercase 2 vars.*/
                  select
                  when pos(f, 'AEIOU')\==0  then do;  say z','  z", bo-b"zl
                                                      say 'Banana-fana fo-f'zl
                                                      say 'Fee-fi-mo-m'zl
                                                 end
                  when pos(f, 'BFM'  )\==0  then do;  if f=='B'  then @b=
                                                      if f=='F'  then @f=
                                                      if f=='M'  then @m=
                                                      say z','  z", bo-"@b || y
                                                      say 'Banana-fana fo-'@f || y
                                                      say 'Fee-fi-mo-'@m || y
                                                 end
                  otherwise                           say z','  z", bo-b"y
                                                      say 'Banana-fana fo-f'y
                                                      say 'Fee-fi-mo-m'y
                  end   /*select*/
      say z'!';                           say
      return
```

```txt

Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

```



## Scala


```Scala
object NameGame extends App {
  private def printVerse(name: String): Unit = {
    val x = name.toLowerCase.capitalize

    val y = if ("AEIOU" contains x.head) x.toLowerCase else x.tail

    val (b, f, m) = x.head match {
      case 'B' => (y, "f" + y, "m" + y)
      case 'F' => ("b" + y, y, "m" + y)
      case 'M' => ("b" + y, "f" + y, y)
      case _   => ("b" + y, "f" + y, "m" + y)
    }

    printf("%s, %s, bo-%s\n", x, x, b)
    printf("Banana-fana fo-%s\n", f)
    println(s"Fee-fi-mo-$m")
    println(s"$x!\n")
  }

  Stream("gAry", "earl", "Billy", "Felix", "Mary", "Steve").foreach(printVerse)
}
```


See it running in your browser by [https://scastie.scala-lang.org/UilFJ0zFSJu4Qk2xaw2r4A Scastie (JVM)].

## VBA


```vb
Option Explicit

Sub Main()
Dim a, r, i As Integer
Const SCHEM As String = "(X), (X), bo-b(Y)^Banana-fana fo-f(Y)^Fee-fi-mo-m(Y)^(X)!^"
   'init
   a = Array("GaRY", "Earl", "Billy", "Felix", "Mary", "Mike", "Frank")
   'compute
   r = TheGameName(a, SCHEM)
   'return
   For i = LBound(r) To UBound(r)
      Debug.Print r(i)
   Next i
End Sub

Private Function TheGameName(MyArr, S As String) As String()
Dim i As Integer, s1 As String, s2 As String, tp As String, t() As String
   ReDim t(UBound(MyArr))
   For i = LBound(MyArr) To UBound(MyArr)
      tp = Replace(S, "^", vbCrLf)
      s2 = LCase(Mid(MyArr(i), 2)): s1 = UCase(Left(MyArr(i), 1)) & s2
      Select Case UCase(Left(MyArr(i), 1))
         Case "A", "E", "I", "O", "U": tp = Replace(tp, "(Y)", LCase(MyArr(i)))
         Case "B", "F", "M"
            tp = Replace(tp, "(Y)", s2)
            tp = Replace(tp, LCase(MyArr(i)), s2)
         Case Else: tp = Replace(tp, "(Y)", s2)
      End Select
      t(i) = Replace(tp, "(X)", s1)
   Next
   TheGameName = t
End Function
```

```txt
Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Mike, Mike, bo-bike
Banana-fana fo-fike
Fee-fi-mo-ike
Mike!

Frank, Frank, bo-brank
Banana-fana fo-rank
Fee-fi-mo-mrank
Frank!
```



## Visual Basic .NET

```vbnet
Option Strict On

Imports System.Text

Module Module1

    Sub PrintVerse(name As String)
        Dim sb As New StringBuilder(name.ToLower())
        sb(0) = Char.ToUpper(sb(0))

        Dim x = sb.ToString()
        Dim y = If("AEIOU".IndexOf(x(0)) > -1, x.ToLower(), x.Substring(1))
        Dim b = "b" + y
        Dim f = "f" + y
        Dim m = "m" + y
        Select Case x(0)
            Case "B"c
                b = y
                Exit Select
            Case "F"c
                f = y
                Exit Select
            Case "M"c
                m = y
                Exit Select
        End Select

        Console.WriteLine("{0}, {0}, bo-{1}", x, b)
        Console.WriteLine("Banana-fana fo-{0}", f)
        Console.WriteLine("Fee-fi-mo-{0}", m)
        Console.WriteLine("{0}!", x)
        Console.WriteLine()
    End Sub

    Sub Main()
        Dim nameList As New List(Of String) From {"Gary", "Earl", "Billy", "Felix", "Mary", "Steve"}
        nameList.ForEach(AddressOf PrintVerse)
    End Sub

End Module
```

```txt
Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Steve, Steve, bo-bteve
Banana-fana fo-fteve
Fee-fi-mo-mteve
Steve!
```



## zkl

```zkl
fcn printVerse(name){
   z,x := name[0].toLower(), z.toUpper() + name[1,*].toLower();
   y:=( if("aeiou".holds(z)) name.toLower() else x[1,*] );
   b,f,m := T("b","f","m").apply('wrap(c){ z==c and y or c+y });
   println("%s, %s, bo-%s".fmt(x,x,b));
   println("Banana-fana fo-",f);
   println("Fee-fi-mo-",m);
   println(x,"!\n");
}
```


```zkl
List("Gary", "Earl", "Billy", "Felix", "Mary", "Steve").apply2(printVerse);
```

<pre style="height:35ex">
Gary, Gary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-mary
Gary!

Earl, Earl, bo-bearl
Banana-fana fo-fearl
Fee-fi-mo-mearl
Earl!

Billy, Billy, bo-illy
Banana-fana fo-filly
Fee-fi-mo-milly
Billy!

Felix, Felix, bo-belix
Banana-fana fo-elix
Fee-fi-mo-melix
Felix!

Mary, Mary, bo-bary
Banana-fana fo-fary
Fee-fi-mo-ary
Mary!

Steve, Steve, bo-bteve
Banana-fana fo-fteve
Fee-fi-mo-mteve
Steve!

```

