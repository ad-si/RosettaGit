+++
title = "Separate the house number from the street name"
description = ""
date = 2019-06-08T21:30:33Z
aliases = []
[extra]
id = 17696
[taxonomies]
categories = []
tags = []
+++

{{Draft task}} [[Category:Web]]
{{Template:Clarify task}}
In Germany and the Netherlands postal addresses have the form: street name, followed by the house number, in accordance with the national standards DIN 5008 respectively NEN 5825. The problem is that some street names have numbers (e.g. special years) and some [http://en.wikipedia.org/wiki/House_numbering#Europe house numbers] have characters as an extension. It's a real life problem and difficult because in the Netherlands some street names are a tribute to our liberators. The street names have the numbers 40 and 45 indicating the years of war between 1940 and 1945.
;Task:

Write code that correctly separates the house number from the street name and presents them both. 
<em>No static data must be shown, only processed data.</em>

The suggested approach is to either use the regular expression in the Scala entry or to devise an equivalent algorithm.

The test-set:

```txt
Plataanstraat 5
Straat 12
Straat 12 II
Dr. J. Straat   12
Dr. J. Straat 12 a
Dr. J. Straat 12-14
Laan 1940 – 1945 37
Plein 1940 2
1213-laan 11
16 april 1944 Pad 1
1e Kruisweg 36
Laan 1940-’45 66
Laan ’40-’45
Langeloërduinen 3 46
Marienwaerdt 2e Dreef 2
Provincialeweg N205 1
Rivium 2e Straat 59.
Nieuwe gracht 20rd
Nieuwe gracht 20rd 2
Nieuwe gracht 20zw /2
Nieuwe gracht 20zw/3
Nieuwe gracht 20 zw/4
Bahnhofstr. 4
Wertstr. 10
Lindenhof 1
Nordesch 20
Weilstr. 6
Harthauer Weg 2
Mainaustr. 49
August-Horch-Str. 3
Marktplatz 31
Schmidener Weg 3
Karl-Weysser-Str. 6
```



## EchoLisp


```scheme

(lib 'struct)
(lib 'sql)

(define adresses (make-table (struct adresse (name number))))

;; parse street<space>number| II...
(define extractor (make-regexp 
	"^(.*?)\\s(\\d+$|\\d+.*$|II*$)"))
	
;; making our best with the special names
(define specials '("1940" "1945" "'45" "'40"))

(define (rep-special str specials)
		(for/fold (ostr str) ((special specials))
		(string-replace ostr special (string-append "@" special))))
		
(define (un-rep-special record)
	(set-car! record (string-replace (car record) "/@/g" "" )))		


    
(define (task)
(for-each (lambda (x)  
	(table-insert adresses
	(un-rep-special
    (or*  (regexp-exec extractor (rep-special x specials))
          (list x '❓❓❓))))) adressen))

(define adressen '(
"Plataanstraat 5"
"Straat 12"
"Straat 12 II"
"Straat 1940 II"
"Dr. J. Straat   40"
"Dr. J. Straat 12 a"
"Dr. J. Straat 12-14"
"Laan 1940 – 1945 37"
"Plein 1940 2"
"1213-laan 11"
"16 april 1944 Pad 1"
"1e Kruisweg 36"
"Laan 1940-'45 66"
"Laan '40-'45"
"Langeloërduinen 3 46"
"Marienwaerdt 2e Dreef 2"
"Provincialeweg N205 1"
"Rivium 2e Straat 59."
"Nieuwe gracht 20rd"
"Nieuwe gracht 20rd 2"
"Nieuwe gracht 20zw /2"
"Nieuwe gracht 20zw/3"
"Nieuwe gracht 20 zw/4"
"Bahnhofstr. 4"
"Wertstr. 10"
"Lindenhof 1"
"Nordesch 20"
"Weilstr. 6"
"Harthauer Weg 2"
"Mainaustr. 49"
"August-Horch-Str. 3"
"Marktplatz 31"
"Schmidener Weg 3"
"Karl-Weysser-Str. 6"))

```

{{out}}

```txt

(task)
(table-print   adresses)
[0]   Plataanstraat         5             
[1]   Straat                12            
[2]   Straat                12 II         
[3]   Straat 1940           II            
[4]   Dr. J. Straat         40            
[5]   Dr. J. Straat         12 a          
[6]   Dr. J. Straat         12-14         
[7]   Laan 1940 – 1945      37            
[8]   Plein 1940            2             
[9]   1213-laan             11            
[10]   16 april              1944 Pad 1    
[11]   1e Kruisweg           36            
[12]   Laan 1940-'45         66            
[13]   Laan '40-'45          ❓❓❓           
[14]   Langeloërduinen       3 46          
[15]   Marienwaerdt          2e Dreef 2    
[16]   Provincialeweg N205   1             
[17]   Rivium                2e Straat 59. 
[18]   Nieuwe gracht         20rd          
[19]   Nieuwe gracht         20rd 2        
[20]   Nieuwe gracht         20zw /2       
[21]   Nieuwe gracht         20zw/3        
[22]   Nieuwe gracht         20 zw/4       
[23]   Bahnhofstr.           4             
[24]   Wertstr.              10            
[25]   Lindenhof             1             
[26]   Nordesch              20            
[27]   Weilstr.              6             
[28]   Harthauer Weg         2             
[29]   Mainaustr.            49            
[30]   August-Horch-Str.     3             
[31]   Marktplatz            31            
[32]   Schmidener Weg        3             
[33]   Karl-Weysser-Str.     6             

```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "strings"
)

func isDigit(b byte) bool {
    return '0' <= b && b <= '9'
}

func separateHouseNumber(address string) (street string, house string) {
    length := len(address)
    fields := strings.Fields(address)
    size := len(fields)
    last := fields[size-1]
    penult := fields[size-2]
    if isDigit(last[0]) {
        isdig := isDigit(penult[0])
        if size > 2 && isdig && !strings.HasPrefix(penult, "194") {
            house = fmt.Sprintf("%s %s", penult, last)
        } else {
            house = last
        }
    } else if size > 2 {
        house = fmt.Sprintf("%s %s", penult, last)
    }
    street = strings.TrimRight(address[:length-len(house)], " ")
    return
}

func main() {
    addresses := [...]string{
        "Plataanstraat 5",
        "Straat 12",
        "Straat 12 II",
        "Dr. J. Straat   12",
        "Dr. J. Straat 12 a",
        "Dr. J. Straat 12-14",
        "Laan 1940 - 1945 37",
        "Plein 1940 2",
        "1213-laan 11",
        "16 april 1944 Pad 1",
        "1e Kruisweg 36",
        "Laan 1940-'45 66",
        "Laan '40-'45",
        "Langeloërduinen 3 46",
        "Marienwaerdt 2e Dreef 2",
        "Provincialeweg N205 1",
        "Rivium 2e Straat 59.",
        "Nieuwe gracht 20rd",
        "Nieuwe gracht 20rd 2",
        "Nieuwe gracht 20zw /2",
        "Nieuwe gracht 20zw/3",
        "Nieuwe gracht 20 zw/4",
        "Bahnhofstr. 4",
        "Wertstr. 10",
        "Lindenhof 1",
        "Nordesch 20",
        "Weilstr. 6",
        "Harthauer Weg 2",
        "Mainaustr. 49",
        "August-Horch-Str. 3",
        "Marktplatz 31",
        "Schmidener Weg 3",
        "Karl-Weysser-Str. 6",
    }
    fmt.Println("Street                   House Number")
    fmt.Println("---------------------    ------------")
    for _, address := range addresses {
        street, house := separateHouseNumber(address)
        if house == "" {
            house = "(none)"
        }
        fmt.Printf("%-22s   %s\n", street, house)
    }
}
```


{{out}}

```txt

Street                   House Number
---------------------    ------------
Plataanstraat            5
Straat                   12
Straat                   12 II
Dr. J. Straat            12
Dr. J. Straat            12 a
Dr. J. Straat            12-14
Laan 1940 - 1945         37
Plein 1940               2
1213-laan                11
16 april 1944 Pad        1
1e Kruisweg              36
Laan 1940-'45            66
Laan '40-'45             (none)
Langeloërduinen          3 46
Marienwaerdt 2e Dreef    2
Provincialeweg N205      1
Rivium 2e Straat         59.
Nieuwe gracht            20rd
Nieuwe gracht            20rd 2
Nieuwe gracht            20zw /2
Nieuwe gracht            20zw/3
Nieuwe gracht            20 zw/4
Bahnhofstr.              4
Wertstr.                 10
Lindenhof                1
Nordesch                 20
Weilstr.                 6
Harthauer Weg            2
Mainaustr.               49
August-Horch-Str.        3
Marktplatz               31
Schmidener Weg           3
Karl-Weysser-Str.        6

```



## J


'''Solution''':

```j
special=: '4',.'012345'
digit=: '0123456789'

nope=: 3 :'>./2+I. special +/@:(E."1) y'
here=: 3 :'I.y e.digit'
din5008=: ({.;}.)~ here {.@#~ nope < here
```


Sample data (works with either of the above implementations):


```j
   sampledata=: ];._2 noun define
Straat 12
Straat 12 II
Dr. J. Straat   12
Dr. J. Straat 12 a
Dr. J. Straat 12-14
Laan 1940 – 1945 37
Plein 1940 2
1213-laan 11
16 april 1944 Pad 1
1e Kruisweg 36
Laan 1940-’45 66
Laan ’40-’45
Langeloërduinen 3 46
Marienwaerdt 2e Dreef 2
Provincialeweg N205 1
Rivium 2e Straat 59.
Nieuwe gracht 20rd
Nieuwe gracht 20rd 2
Nieuwe gracht 20zw /2
Nieuwe gracht 20zw/3
Nieuwe gracht 20 zw/4
Bahnhofstr. 4
Wertstr. 10
Lindenhof 1
Nordesch 20
Weilstr. 6
Harthauer Weg 2
Mainaustr. 49
August-Horch-Str. 3
Marktplatz 31
Schmidener Weg 3
Karl-Weysser-Str. 6
)
```


'''Example''':
```j
   din5008"1 sampledata
┌───────────────────┬───────────────────────┐
│Straat             │12                     │
├───────────────────┼───────────────────────┤
│Straat             │12 II                  │
├───────────────────┼───────────────────────┤
│Dr. J. Straat      │12                     │
├───────────────────┼───────────────────────┤
│Dr. J. Straat      │12 a                   │
├───────────────────┼───────────────────────┤
│Dr. J. Straat      │12-14                  │
├───────────────────┼───────────────────────┤
│Laan 1940 – 1945 │37                     │
├───────────────────┼───────────────────────┤
│Plein 1940         │2                      │
├───────────────────┼───────────────────────┤
│                   │1213-laan 11           │
├───────────────────┼───────────────────────┤
│16 april 1944 Pad  │1                      │
├───────────────────┼───────────────────────┤
│                   │1e Kruisweg 36         │
├───────────────────┼───────────────────────┤
│Laan 1940-’45    │66                     │
├───────────────────┼───────────────────────┤
│                   │Laan ’40-’45       │
├───────────────────┼───────────────────────┤
│Langeloërduinen   │3 46                   │
├───────────────────┼───────────────────────┤
│Marienwaerdt       │2e Dreef 2             │
├───────────────────┼───────────────────────┤
│Provincialeweg N   │205 1                  │
├───────────────────┼───────────────────────┤
│Rivium             │2e Straat 59.          │
├───────────────────┼───────────────────────┤
│Nieuwe gracht      │20rd                   │
├───────────────────┼───────────────────────┤
│Nieuwe gracht      │20rd 2                 │
├───────────────────┼───────────────────────┤
│Nieuwe gracht      │20zw /2                │
├───────────────────┼───────────────────────┤
│Nieuwe gracht      │20zw/3                 │
├───────────────────┼───────────────────────┤
│Nieuwe gracht      │20 zw/4                │
├───────────────────┼───────────────────────┤
│Bahnhofstr.        │4                      │
├───────────────────┼───────────────────────┤
│Wertstr.           │10                     │
├───────────────────┼───────────────────────┤
│Lindenhof          │1                      │
├───────────────────┼───────────────────────┤
│Nordesch           │20                     │
├───────────────────┼───────────────────────┤
│Weilstr.           │6                      │
├───────────────────┼───────────────────────┤
│Harthauer Weg      │2                      │
├───────────────────┼───────────────────────┤
│Mainaustr.         │49                     │
├───────────────────┼───────────────────────┤
│August-Horch-Str.  │3                      │
├───────────────────┼───────────────────────┤
│Marktplatz         │31                     │
├───────────────────┼───────────────────────┤
│Schmidener Weg     │3                      │
├───────────────────┼───────────────────────┤
│Karl-Weysser-Str.  │6                      │
└───────────────────┴───────────────────────┘
```




## Julia

Uses the regex from the Perl version.

```julia
const regex = r"""^ (.*?) \s+
                  (
                      \d* (\-|\/)? \d*
                    | \d{1,3} [a-zI./ ]* \d{0,3}
                  )
                  $"""x
 
const adressen = """
    Plataanstraat 5
    Straat 12
    Straat 12 II
    Dr. J. Straat   12
    Dr. J. Straat 12 a
    Dr. J. Straat 12-14
    Laan 1940 – 1945 37
    Plein 1940 2
    1213-laan 11
    16 april 1944 Pad 1
    1e Kruisweg 36
    Laan 1940-’45 66
    Laan ’40-’45
    Langeloërduinen 3 46
    Marienwaerdt 2e Dreef 2
    Provincialeweg N205 1
    Rivium 2e Straat 59.
    Nieuwe gracht 20rd
    Nieuwe gracht 20rd 2
    Nieuwe gracht 20zw /2
    Nieuwe gracht 20zw/3
    Nieuwe gracht 20 zw/4
    Bahnhofstr. 4
    Wertstr. 10
    Lindenhof 1
    Nordesch 20
    Weilstr. 6
    Harthauer Weg 2
    Mainaustr. 49
    August-Horch-Str. 3
    Marktplatz 31
    Schmidener Weg 3
    Karl-Weysser-Str. 6"""

for line in strip.(split(adressen, "\n"))
    if (matched = match(regex, line)) != nothing
        street, number = matched.captures
        println(rpad(line, 30), "split as street => $street, number => $number")
    else
        println(rpad(line, 30), "(Error)")
    end
end

```
{{out}}

```txt

Plataanstraat 5               split as street => Plataanstraat, number => 5
Straat 12                     split as street => Straat, number => 12
Straat 12 II                  split as street => Straat, number => 12 II
Dr. J. Straat   12            split as street => Dr. J. Straat, number => 12
Dr. J. Straat 12 a            split as street => Dr. J. Straat, number => 12 a
Dr. J. Straat 12-14           split as street => Dr. J. Straat, number => 12-14
Laan 1940 – 1945 37           split as street => Laan 1940 – 1945, number => 37
Plein 1940 2                  split as street => Plein 1940, number => 2
1213-laan 11                  split as street => 1213-laan, number => 11
16 april 1944 Pad 1           split as street => 16 april 1944 Pad, number => 1
1e Kruisweg 36                split as street => 1e Kruisweg, number => 36
Laan 1940-’45 66              split as street => Laan 1940-’45, number => 66
Laan ’40-’45                  (Error)
Langeloërduinen 3 46          split as street => Langeloërduinen, number => 3 46
Marienwaerdt 2e Dreef 2       split as street => Marienwaerdt 2e Dreef, number => 2
Provincialeweg N205 1         split as street => Provincialeweg N205, number => 1
Rivium 2e Straat 59.          split as street => Rivium 2e Straat, number => 59.
Nieuwe gracht 20rd            split as street => Nieuwe gracht, number => 20rd
Nieuwe gracht 20rd 2          split as street => Nieuwe gracht, number => 20rd 2
Nieuwe gracht 20zw /2         split as street => Nieuwe gracht, number => 20zw /2
Nieuwe gracht 20zw/3          split as street => Nieuwe gracht, number => 20zw/3
Nieuwe gracht 20 zw/4         split as street => Nieuwe gracht, number => 20 zw/4
Bahnhofstr. 4                 split as street => Bahnhofstr., number => 4
Wertstr. 10                   split as street => Wertstr., number => 10
Lindenhof 1                   split as street => Lindenhof, number => 1
Nordesch 20                   split as street => Nordesch, number => 20
Weilstr. 6                    split as street => Weilstr., number => 6
Harthauer Weg 2               split as street => Harthauer Weg, number => 2
Mainaustr. 49                 split as street => Mainaustr., number => 49
August-Horch-Str. 3           split as street => August-Horch-Str., number => 3
Marktplatz 31                 split as street => Marktplatz, number => 31
Schmidener Weg 3              split as street => Schmidener Weg, number => 3
Karl-Weysser-Str. 6           split as street => Karl-Weysser-Str., number => 6

```



## Kotlin


```scala
// version 1.0.6

val r = Regex("""\s+""")

fun separateHouseNumber(address: String): Pair<String, String> {
    val street: String
    val house:  String
    val len    = address.length
    val splits = address.split(r)
    val size   = splits.size
    val last   = splits[size - 1]
    val penult = splits[size - 2]
    if (last[0] in '0'..'9') {
        if (size > 2 && penult[0] in '0'..'9' && !penult.startsWith("194")) house = penult + " " + last
        else house = last
    }
    else if (size > 2) house = penult + " " + last
    else house = ""
    street = address.take(len - house.length).trimEnd()
    return Pair(street, house)
}

fun main(args: Array<String>) {
    val addresses = arrayOf(
        "Plataanstraat 5",
        "Straat 12",
        "Straat 12 II",
        "Dr. J. Straat   12",
        "Dr. J. Straat 12 a",
        "Dr. J. Straat 12-14",
        "Laan 1940 - 1945 37",
        "Plein 1940 2",
        "1213-laan 11",
        "16 april 1944 Pad 1",
        "1e Kruisweg 36",
        "Laan 1940-'45 66",
        "Laan '40-'45",
        "Langeloërduinen 3 46",
        "Marienwaerdt 2e Dreef 2",
        "Provincialeweg N205 1",
        "Rivium 2e Straat 59.",
        "Nieuwe gracht 20rd",
        "Nieuwe gracht 20rd 2",
        "Nieuwe gracht 20zw /2",
        "Nieuwe gracht 20zw/3",
        "Nieuwe gracht 20 zw/4",
        "Bahnhofstr. 4",
        "Wertstr. 10",
        "Lindenhof 1",
        "Nordesch 20",
        "Weilstr. 6",
        "Harthauer Weg 2",
        "Mainaustr. 49",
        "August-Horch-Str. 3",
        "Marktplatz 31",
        "Schmidener Weg 3",
        "Karl-Weysser-Str. 6"
    )
    println("Street                   House Number")
    println("---------------------    ------------")
    for (address in addresses) {
        val (street, house) = separateHouseNumber(address)
        println("${street.padEnd(22)}   ${if (house != "") house else "(none)"}")
    }
}
```


{{out}}

```txt

Street                   House Number
---------------------    ------------
Plataanstraat            5
Straat                   12
Straat                   12 II
Dr. J. Straat            12
Dr. J. Straat            12 a
Dr. J. Straat            12-14
Laan 1940 - 1945         37
Plein 1940               2
1213-laan                11
16 april 1944 Pad        1
1e Kruisweg              36
Laan 1940-'45            66
Laan '40-'45             (none)
Langeloërduinen          3 46
Marienwaerdt 2e Dreef    2
Provincialeweg N205      1
Rivium 2e Straat         59.
Nieuwe gracht            20rd
Nieuwe gracht            20rd 2
Nieuwe gracht            20zw /2
Nieuwe gracht            20zw/3
Nieuwe gracht            20 zw/4
Bahnhofstr.              4
Wertstr.                 10
Lindenhof                1
Nordesch                 20
Weilstr.                 6
Harthauer Weg            2
Mainaustr.               49
August-Horch-Str.        3
Marktplatz               31
Schmidener Weg           3
Karl-Weysser-Str.        6

```



## Perl


```perl
@addresses = (
'Plataanstraat 5',      'Straat 12',             'Straat 12 II',            'Dr. J. Straat   12',
'Dr. J. Straat 12 a',   'Dr. J. Straat 12-14',   'Laan 1940 – 1945 37',     'Plein 1940 2',
'1213-laan 11',         '16 april 1944 Pad 1',   '1e Kruisweg 36',          'Laan 1940-’45 66',
'Laan ’40-’45',         'Langeloërduinen 3 46',  'Marienwaerdt 2e Dreef 2', 'Provincialeweg N205 1',
'Rivium 2e Straat 59.', 'Nieuwe gracht 20rd',    'Nieuwe gracht 20rd 2',    'Nieuwe gracht 20zw /2',
'Nieuwe gracht 20zw/3', 'Nieuwe gracht 20 zw/4', 'Bahnhofstr. 4',           'Wertstr. 10',
'Lindenhof 1',          'Nordesch 20',           'Weilstr. 6',              'Harthauer Weg 2',
'Mainaustr. 49',        'August-Horch-Str. 3',   'Marktplatz 31',           'Schmidener Weg 3',
'Karl-Weysser-Str. 6');

for (@addresses) {
    my($street,$number) =
    m[^ (.*?) \s+
        (
           \d* (\-|\/)? \d*
         | \d{1,3} [a-zI./ ]* \d{0,3}
        )
      $]x;
    $number ? printf "%-26s\t%s\n", ($street, $number) : ($_, "\t(no match)");
}
```

{{out}}
<pre  style="height:35ex">Plataanstraat             5
Straat                    12
Straat                    12 II
Dr. J. Straat             12
Dr. J. Straat             12 a
Dr. J. Straat             12-14
Laan 1940 – 1945          37
Plein 1940                2
1213-laan                 11
16 april 1944 Pad         1
1e Kruisweg               36
Laan 1940-’45             66
Laan ’40-’45              (no match)
Langeloërduinen           3 46
Marienwaerdt 2e Dreef     2
Provincialeweg N205       1
Rivium 2e Straat          59.
Nieuwe gracht             20rd
Nieuwe gracht             20rd 2
Nieuwe gracht             20zw /2
Nieuwe gracht             20zw/3
Nieuwe gracht             20 zw/4
Bahnhofstr.               4
Wertstr.                  10
Lindenhof                 1
Nordesch                  20
Weilstr.                  6
Harthauer Weg             2
Mainaustr.                49
August-Horch-Str.         3
Marktplatz                31
Schmidener Weg            3
Karl-Weysser-Str.         6
```



## Perl 6

An unquestioning translation of the Scala example's regex to show how we lay out such regexes for readability in Perl 6, except that we take the liberty of leaving the space out of the house number.  
(Hard constants like 1940 and 1945 are a code smell, 
and the task should probably not require such constants unless there is a standard to point to that mandates them.) 
So expect this solution to change if the task is actually defined reasonably, such as by specifying that four-digit house numbers are excluded in Europe.  
(In contrast, four- and five-digit house numbers are not uncommon 
in places such as the U.S. where each block gets a hundred house numbers 
to play with, and there are cities with hundreds of blocks along a street.)

```perl6
say m[
    ( .*? )

    [
        \s+
        (
        | \d+ [ \- | \/ ] \d+
        | <!before 1940 | 1945> \d+ <[ a..z I . / \x20 ]>* \d*
        )
    ]?

    $
] for lines;
```

{{out}}

```txt
｢Plataanstraat 5｣
 0 => ｢Plataanstraat｣
 1 => ｢5｣

｢Straat 12｣
 0 => ｢Straat｣
 1 => ｢12｣

｢Straat 12 II｣
 0 => ｢Straat｣
 1 => ｢12 II｣

｢Dr. J. Straat   12｣
 0 => ｢Dr. J. Straat｣
 1 => ｢12｣

｢Dr. J. Straat 12 a｣
 0 => ｢Dr. J. Straat｣
 1 => ｢12 a｣

｢Dr. J. Straat 12-14｣
 0 => ｢Dr. J. Straat｣
 1 => ｢12-14｣

｢Laan 1940 – 1945 37｣
 0 => ｢Laan 1940 – 1945｣
 1 => ｢37｣

｢Plein 1940 2｣
 0 => ｢Plein 1940｣
 1 => ｢2｣

｢1213-laan 11｣
 0 => ｢1213-laan｣
 1 => ｢11｣

｢16 april 1944 Pad 1｣
 0 => ｢16 april 1944 Pad｣
 1 => ｢1｣

｢1e Kruisweg 36｣
 0 => ｢1e Kruisweg｣
 1 => ｢36｣

｢Laan 1940-’45 66｣
 0 => ｢Laan 1940-’45｣
 1 => ｢66｣

｢Laan ’40-’45｣
 0 => ｢Laan ’40-’45｣

｢Langeloërduinen 3 46｣
 0 => ｢Langeloërduinen｣
 1 => ｢3 46｣

｢Marienwaerdt 2e Dreef 2｣
 0 => ｢Marienwaerdt 2e Dreef｣
 1 => ｢2｣

｢Provincialeweg N205 1｣
 0 => ｢Provincialeweg N205｣
 1 => ｢1｣

｢Rivium 2e Straat 59.｣
 0 => ｢Rivium 2e Straat｣
 1 => ｢59.｣

｢Nieuwe gracht 20rd｣
 0 => ｢Nieuwe gracht｣
 1 => ｢20rd｣

｢Nieuwe gracht 20rd 2｣
 0 => ｢Nieuwe gracht｣
 1 => ｢20rd 2｣

｢Nieuwe gracht 20zw /2｣
 0 => ｢Nieuwe gracht｣
 1 => ｢20zw /2｣

｢Nieuwe gracht 20zw/3｣
 0 => ｢Nieuwe gracht｣
 1 => ｢20zw/3｣

｢Nieuwe gracht 20 zw/4｣
 0 => ｢Nieuwe gracht｣
 1 => ｢20 zw/4｣

｢Bahnhofstr. 4｣
 0 => ｢Bahnhofstr.｣
 1 => ｢4｣

｢Wertstr. 10｣
 0 => ｢Wertstr.｣
 1 => ｢10｣

｢Lindenhof 1｣
 0 => ｢Lindenhof｣
 1 => ｢1｣

｢Nordesch 20｣
 0 => ｢Nordesch｣
 1 => ｢20｣

｢Weilstr. 6｣
 0 => ｢Weilstr.｣
 1 => ｢6｣

｢Harthauer Weg 2｣
 0 => ｢Harthauer Weg｣
 1 => ｢2｣

｢Mainaustr. 49｣
 0 => ｢Mainaustr.｣
 1 => ｢49｣

｢August-Horch-Str. 3｣
 0 => ｢August-Horch-Str.｣
 1 => ｢3｣

｢Marktplatz 31｣
 0 => ｢Marktplatz｣
 1 => ｢31｣

｢Schmidener Weg 3｣
 0 => ｢Schmidener Weg｣
 1 => ｢3｣

｢Karl-Weysser-Str. 6｣
 0 => ｢Karl-Weysser-Str.｣
 1 => ｢6｣


```



## Phix

{{trans|Go}}

```Phix
function isDigit(integer ch)
    return ch>='0' and ch<='9'
end function
 
function separateHouseNumber(sequence address)
    address = split(address,no_empty:=true)
    string street, house
    integer h = 0
    if length(address)>1 then
        string last = address[$]
        if isDigit(last[1]) then
            h = 1
            string penult = address[$-1]
            if length(address)>2
            and isDigit(penult[1])
            and match("194",penult)!=1 then
                h = 2
            end if
        elsif length(address)>2 then
            h = 2
        end if
    end if
    if h then
        street = join(address[1..$-h])
        house = join(address[$-h+1..$])
    else
        street = join(address)
        house = "(none)"
    end if
    return {street,house}
end function
 
constant addresses = {"Plataanstraat 5",
                      "Straat 12",
                      "Straat 12 II",
                      "Dr. J. Straat   12",
                      "Dr. J. Straat 12 a",
                      "Dr. J. Straat 12-14",
                      "Laan 1940 - 1945 37",
                      "Plein 1940 2",
                      "1213-laan 11",
                      "16 april 1944 Pad 1",
                      "1e Kruisweg 36",
                      "Laan 1940-'45 66",
                      "Laan '40-'45",
                      "Langeloërduinen 3 46",
                      "Marienwaerdt 2e Dreef 2",
                      "Provincialeweg N205 1",
                      "Rivium 2e Straat 59.",
                      "Nieuwe gracht 20rd",
                      "Nieuwe gracht 20rd 2",
                      "Nieuwe gracht 20zw /2",
                      "Nieuwe gracht 20zw/3",
                      "Nieuwe gracht 20 zw/4",
                      "Bahnhofstr. 4",
                      "Wertstr. 10",
                      "Lindenhof 1",
                      "Nordesch 20",
                      "Weilstr. 6",
                      "Harthauer Weg 2",
                      "Mainaustr. 49",
                      "August-Horch-Str. 3",
                      "Marktplatz 31",
                      "Schmidener Weg 3",
                      "Karl-Weysser-Str. 6"}

procedure main()
    printf(1,"Street                   House Number\n")
    printf(1,"---------------------    ------------\n")
    for i=1 to length(addresses) do
        printf(1,"%-22s   %s\n", separateHouseNumber(addresses[i]))
    end for
end procedure
main()
```

{{out}}

```txt

Street                   House Number
---------------------    ------------
Plataanstraat            5
Straat                   12
Straat                   12 II
Dr. J. Straat            12
Dr. J. Straat            12 a
Dr. J. Straat            12-14
Laan 1940 - 1945         37
Plein 1940               2
1213-laan                11
16 april 1944 Pad        1
1e Kruisweg              36
Laan 1940-'45            66
Laan '40-'45             (none)
LangeloÙrduinen          3 46
Marienwaerdt 2e Dreef    2
Provincialeweg N205      1
Rivium 2e Straat         59.
Nieuwe gracht            20rd
Nieuwe gracht            20rd 2
Nieuwe gracht            20zw /2
Nieuwe gracht            20zw/3
Nieuwe gracht            20 zw/4
Bahnhofstr.              4
Wertstr.                 10
Lindenhof                1
Nordesch                 20
Weilstr.                 6
Harthauer Weg            2
Mainaustr.               49
August-Horch-Str.        3
Marktplatz               31
Schmidener Weg           3
Karl-Weysser-Str.        6

```



## Python

<!-- ?? missing code ?? -->

```python

Plataanstraat 5           split as (Plataanstraat, 5)
Straat 12                 split as (Straat, 12)
Straat 12 II              split as (Straat, 12 II)
Dr. J. Straat   12        split as (Dr. J. Straat  , 12)
Dr. J. Straat 12 a        split as (Dr. J. Straat, 12 a)
Dr. J. Straat 12-14       split as (Dr. J. Straat, 12-14)
Laan 1940 – 1945 37       split as (Laan 1940 – 1945, 37)
Plein 1940 2              split as (Plein 1940, 2)
1213-laan 11              split as (1213-laan, 11)
16 april 1944 Pad 1       split as (16 april 1944 Pad, 1)
1e Kruisweg 36            split as (1e Kruisweg, 36)
Laan 1940-’45 66          split as (Laan 1940-’45, 66)
Laan ’40-’45              split as (Laan ’40-’45,)
Langeloërduinen 3 46      split as (Langeloërduinen, 3 46)
Marienwaerdt 2e Dreef 2   split as (Marienwaerdt 2e Dreef, 2)
Provincialeweg N205 1     split as (Provincialeweg N205, 1)
Rivium 2e Straat 59.      split as (Rivium 2e Straat, 59.)
Nieuwe gracht 20rd        split as (Nieuwe gracht, 20rd)
Nieuwe gracht 20rd 2      split as (Nieuwe gracht, 20rd 2)
Nieuwe gracht 20zw /2     split as (Nieuwe gracht, 20zw /2)
Nieuwe gracht 20zw/3      split as (Nieuwe gracht, 20zw/3)
Nieuwe gracht 20 zw/4     split as (Nieuwe gracht, 20 zw/4)
Bahnhofstr. 4             split as (Bahnhofstr., 4)
Wertstr. 10               split as (Wertstr., 10)
Lindenhof 1               split as (Lindenhof, 1)
Nordesch 20               split as (Nordesch, 20)
Weilstr. 6                split as (Weilstr., 6)
Harthauer Weg 2           split as (Harthauer Weg, 2)
Mainaustr. 49             split as (Mainaustr., 49)
August-Horch-Str. 3       split as (August-Horch-Str., 3)
Marktplatz 31             split as (Marktplatz, 31)
Schmidener Weg 3          split as (Schmidener Weg, 3)
Karl-Weysser-Str. 6       split as (Karl-Weysser-Str., 6)''')
```



## Racket


Same as other regexp-splittings on this page.  (I don't see much point in this, but the related [[Starting_a_web_browser]] seems like a good idea.)


```racket

#lang racket

(define extractor-rx
  (pregexp (string-append "^(.*?)\\s+((?:"
                          "(?:\\d+[-/]\\d+)"
                          "|(?:(?!1940|1945)\\d+[a-zI. /]*\\d*)"
                          ")$)")))

(define adressen
  "Plataanstraat 5
   Straat 12
   Straat 12 II
   Straat 1940 II
   Dr. J. Straat   40
   Dr. J. Straat 12 a
   Dr. J. Straat 12-14
   Laan 1940 – 1945 37
   Plein 1940 2
   1213-laan 11
   16 april 1944 Pad 1
   1e Kruisweg 36
   Laan 1940-’45 66
   Laan ’40-’45
   Langeloërduinen 3 46
   Marienwaerdt 2e Dreef 2
   Provincialeweg N205 1
   Rivium 2e Straat 59.
   Nieuwe gracht 20rd
   Nieuwe gracht 20rd 2
   Nieuwe gracht 20zw /2
   Nieuwe gracht 20zw/3
   Nieuwe gracht 20 zw/4
   Bahnhofstr. 4
   Wertstr. 10
   Lindenhof 1
   Nordesch 20
   Weilstr. 6
   Harthauer Weg 2
   Mainaustr. 49
   August-Horch-Str. 3
   Marktplatz 31
   Schmidener Weg 3
   Karl-Weysser-Str. 6")

(define (splits-adressen str)
  (regexp-match extractor-rx str))

(for ([str (in-list (string-split adressen #rx" *\r?\n *"))])
  (printf "~s -> ~s\n" str
          (cond [(splits-adressen str) => cdr]
                [else '???])))

```


{{out}}

```txt

"Plataanstraat 5" -> ("Plataanstraat" "5")
"Straat 12" -> ("Straat" "12")
"Straat 12 II" -> ("Straat" "12 II")
"Straat 1940 II" -> ???
"Dr. J. Straat   40" -> ("Dr. J. Straat" "40")
"Dr. J. Straat 12 a" -> ("Dr. J. Straat" "12 a")
"Dr. J. Straat 12-14" -> ("Dr. J. Straat" "12-14")
"Laan 1940 – 1945 37" -> ("Laan 1940 – 1945" "37")
"Plein 1940 2" -> ("Plein 1940" "2")
"1213-laan 11" -> ("1213-laan" "11")
"16 april 1944 Pad 1" -> ("16 april 1944 Pad" "1")
"1e Kruisweg 36" -> ("1e Kruisweg" "36")
"Laan 1940-’45 66" -> ("Laan 1940-’45" "66")
"Laan ’40-’45" -> ???
"Langeloërduinen 3 46" -> ("Langeloërduinen" "3 46")
"Marienwaerdt 2e Dreef 2" -> ("Marienwaerdt 2e Dreef" "2")
"Provincialeweg N205 1" -> ("Provincialeweg N205" "1")
"Rivium 2e Straat 59." -> ("Rivium 2e Straat" "59.")
"Nieuwe gracht 20rd" -> ("Nieuwe gracht" "20rd")
"Nieuwe gracht 20rd 2" -> ("Nieuwe gracht" "20rd 2")
"Nieuwe gracht 20zw /2" -> ("Nieuwe gracht" "20zw /2")
"Nieuwe gracht 20zw/3" -> ("Nieuwe gracht" "20zw/3")
"Nieuwe gracht 20 zw/4" -> ("Nieuwe gracht" "20 zw/4")
"Bahnhofstr. 4" -> ("Bahnhofstr." "4")
"Wertstr. 10" -> ("Wertstr." "10")
"Lindenhof 1" -> ("Lindenhof" "1")
"Nordesch 20" -> ("Nordesch" "20")
"Weilstr. 6" -> ("Weilstr." "6")
"Harthauer Weg 2" -> ("Harthauer Weg" "2")
"Mainaustr. 49" -> ("Mainaustr." "49")
"August-Horch-Str. 3" -> ("August-Horch-Str." "3")
"Marktplatz 31" -> ("Marktplatz" "31")
"Schmidener Weg 3" -> ("Schmidener Weg" "3")
"Karl-Weysser-Str. 6" -> ("Karl-Weysser-Str." "6")

```



## REXX


```rexx
/*REXX program splits  an European mail address  into  an address  and  a house number. */
                                       != '│'    /*a pipe-ish symbol for $ concatenation*/
         $= "Plataanstraat 5"          ! ,
            "Straat 12"                ! ,
            "Straat 12 II"             ! ,
            "Dr. J. Straat   12"       ! ,
            "Dr. J. Straat 12 a"       ! ,
            "Dr. J. Straat 12-14"      ! ,
            "Laan 1940 - 1945 37"      ! ,
            "Plein 1940 2"             ! ,
            "1213-laan 11"             ! ,
            "16 april 1944 Pad 1"      ! ,
            "1e Kruisweg 36"           ! ,
            "Laan 1940-'45 66"         ! ,
            "Laan '40-'45"             ! ,
            "Langeloërduinen 3 46"     ! ,
            "Provincialeweg N205 1"    ! ,
            "Rivium 2e Straat 59."     ! ,
            "Nieuwe gracht 20rd"       ! ,
            "Nieuwe gracht 20rd 2"     ! ,
            "Nieuwe gracht 20zw /2"    ! ,
            "Nieuwe gracht 20zw/3"     ! ,
            "Nieuwe gracht 20 zw/4"    ! ,
            "Bahnhofstr. 4"            ! ,
            "Wertstr. 10"              ! ,
            "Lindenhof 1"              ! ,
            "Nordesch 20"              ! ,
            "Weilstr. 6"               ! ,
            "Harthauer Weg 2"          ! ,
            "Mainaustr. 49"            ! ,
            "August-Horch-Str. 3"      ! ,
            "Marktplatz 31"            ! ,
            "Schmidener Weg 3"         ! ,
            "Karl-Weysser-Str. 6"
$=space($)
w=0
       do j=1  until $=='';  parse var $ addr '│' $
       @.j=space(addr);      w=max(w, length(@.j) )
       end   /*j*/                               /* [↑]  parse  $  string, make @ array.*/
w=w+2                                            /*expand the width for the display.    */
say center('address', w)         center('house number', 12)
say center('',        w, "═")    center(''            , 12, "═")
#=j-1                                            /*define the number of addresses in  $.*/

       do k=1  for  #;           sp=split(@.k)   /*split each  @.  address: addr, house#*/
       HN=subword(@.k, sp+1);    if HN==''  then HN='   (none) '  /*handle a null house#*/
       say left( subword(@.k, 1, sp), w)         HN
       end   /*k*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
split: procedure; parse arg txt;   n=words(txt);   s=n-1;   p=word(txt,s);   e=word(txt,n)
       if p>1939 & p<1946 | s<2    then p=.    ;   if verify("'",e,"M")\==0  then return n
       pl=verify(0123456789, left(p,1), 'M')\==0
       if (verify('/', e, "M")\==0  &  pl)  |  datatype(p, 'W')  | ,
          (datatype(e, 'N')  &  pl & \verify("'", p, "M"))  then s=s-1
       if s==0  then s=n                         /*if no split, then relocate split to ∞*/
       return s                                  /* [↑]  indicate where to split the txt*/
```

{{out|output|text=  when using the default (internal) input:}}

```txt

        address         house number
═══════════════════════ ════════════
Plataanstraat           5
Straat                  12
Straat                  12 II
Dr. J. Straat           12
Dr. J. Straat           12 a
Dr. J. Straat           12-14
Laan 1940 - 1945        37
Plein 1940              2
1213-laan               11
16 april 1944 Pad       1
1e Kruisweg             36
Laan 1940-'45           66
Laan '40-'45               (none)
Langeloërduinen         3 46
Provincialeweg N205     1
Rivium 2e Straat        59.
Nieuwe gracht           20rd
Nieuwe gracht           20rd 2
Nieuwe gracht           20zw /2
Nieuwe gracht           20zw/3
Nieuwe gracht           20 zw/4
Bahnhofstr.             4
Wertstr.                10
Lindenhof               1
Nordesch                20
Weilstr.                6
Harthauer Weg           2
Mainaustr.              49
August-Horch-Str.       3
Marktplatz              31
Schmidener Weg          3

```



## Scala


```Scala
object SplitHouseNumber extends App {
  val extractor = new scala.util.matching.Regex( """(\s\d+[-/]\d+)|(\s(?!1940|1945)\d+[a-zI. /]*\d*)$|\d+\['][40|45]$""")

  def adressen: Iterator[String] =
    """Plataanstraat 5
      |Straat 12
      |Straat 12 II
      |Straat 1940 II
      |Dr. J. Straat   40
      |Dr. J. Straat 12 a
      |Dr. J. Straat 12-14
      |Laan 1940 – 1945 37
      |Plein 1940 2
      |1213-laan 11
      |16 april 1944 Pad 1
      |1e Kruisweg 36
      |Laan 1940-’45 66
      |Laan ’40-’45
      |Langeloërduinen 3 46
      |Marienwaerdt 2e Dreef 2
      |Provincialeweg N205 1
      |Rivium 2e Straat 59.
      |Nieuwe gracht 20rd
      |Nieuwe gracht 20rd 2
      |Nieuwe gracht 20zw /2
      |Nieuwe gracht 20zw/3
      |Nieuwe gracht 20 zw/4
      |Bahnhofstr. 4
      |Wertstr. 10
      |Lindenhof 1
      |Nordesch 20
      |Weilstr. 6
      |Harthauer Weg 2
      |Mainaustr. 49
      |August-Horch-Str. 3
      |Marktplatz 31
      |Schmidener Weg 3
      |Karl-Weysser-Str. 6""".stripMargin.lines

  def splitsAdressen(input: String): (String, String) = 
    (extractor.split(input).mkString, extractor.findFirstIn(input).getOrElse(""))

  adressen.foreach(s => println(f"$s%-25s split as ${splitsAdressen(s)}"))
}
```

{{out}}

```txt
Plataanstraat 5           split as (Plataanstraat, 5)
Straat 12                 split as (Straat, 12)
Straat 12 II              split as (Straat, 12 II)
Dr. J. Straat   12        split as (Dr. J. Straat  , 12)
Dr. J. Straat 12 a        split as (Dr. J. Straat, 12 a)
Dr. J. Straat 12-14       split as (Dr. J. Straat, 12-14)
Laan 1940 – 1945 37       split as (Laan 1940 – 1945, 37)
Plein 1940 2              split as (Plein 1940, 2)
1213-laan 11              split as (1213-laan, 11)
16 april 1944 Pad 1       split as (16 april 1944 Pad, 1)
1e Kruisweg 36            split as (1e Kruisweg, 36)
Laan 1940-’45 66          split as (Laan 1940-’45, 66)
Laan ’40-’45              split as (Laan ’40-’45,)
Langeloërduinen 3 46      split as (Langeloërduinen, 3 46)
Marienwaerdt 2e Dreef 2   split as (Marienwaerdt 2e Dreef, 2)
Provincialeweg N205 1     split as (Provincialeweg N205, 1)
Rivium 2e Straat 59.      split as (Rivium 2e Straat, 59.)
Nieuwe gracht 20rd        split as (Nieuwe gracht, 20rd)
Nieuwe gracht 20rd 2      split as (Nieuwe gracht, 20rd 2)
Nieuwe gracht 20zw /2     split as (Nieuwe gracht, 20zw /2)
Nieuwe gracht 20zw/3      split as (Nieuwe gracht, 20zw/3)
Nieuwe gracht 20 zw/4     split as (Nieuwe gracht, 20 zw/4)
Bahnhofstr. 4             split as (Bahnhofstr., 4)
Wertstr. 10               split as (Wertstr., 10)
Lindenhof 1               split as (Lindenhof, 1)
Nordesch 20               split as (Nordesch, 20)
Weilstr. 6                split as (Weilstr., 6)
Harthauer Weg 2           split as (Harthauer Weg, 2)
Mainaustr. 49             split as (Mainaustr., 49)
August-Horch-Str. 3       split as (August-Horch-Str., 3)
Marktplatz 31             split as (Marktplatz, 31)
Schmidener Weg 3          split as (Schmidener Weg, 3)
Karl-Weysser-Str. 6       split as (Karl-Weysser-Str., 6)
```



## Sidef

{{trans|Perl 6}}

```ruby
var re = %r[
    ( .*? )
    (?:
        \s+
        (
        | \d+ (?: \- | \/ ) \d+
        | (?! 1940 | 1945) \d+ [ a-z I . / \x20 ]* \d*
        )
    )?
$]x

ARGF.each { |line|
    line.chomp!
    if (var m = line.match(re)) {
        printf("%-25s split as (#{m[0]}, #{m[1]})\n", line)
    }
    else {
        warn "Can't parse: «#{line}»"
    }
}
```

{{out}}

```txt

Plataanstraat 5           split as (Plataanstraat, 5)
Straat 12                 split as (Straat, 12)
Straat 12 II              split as (Straat, 12 II)
Dr. J. Straat   12        split as (Dr. J. Straat, 12)
Dr. J. Straat 12 a        split as (Dr. J. Straat, 12 a)
Dr. J. Straat 12-14       split as (Dr. J. Straat, 12-14)
Laan 1940 – 1945 37       split as (Laan 1940 – 1945, 37)
Plein 1940 2              split as (Plein 1940, 2)
1213-laan 11              split as (1213-laan, 11)
16 april 1944 Pad 1       split as (16 april 1944 Pad, 1)
1e Kruisweg 36            split as (1e Kruisweg, 36)
Laan 1940-’45 66          split as (Laan 1940-’45, 66)
Laan ’40-’45              split as (Laan ’40-’45, )
Langeloërduinen 3 46      split as (Langeloërduinen, 3 46)
Marienwaerdt 2e Dreef 2   split as (Marienwaerdt 2e Dreef, 2)
Provincialeweg N205 1     split as (Provincialeweg N205, 1)
Rivium 2e Straat 59.      split as (Rivium 2e Straat, 59.)
Nieuwe gracht 20rd        split as (Nieuwe gracht, 20rd)
Nieuwe gracht 20rd 2      split as (Nieuwe gracht, 20rd 2)
Nieuwe gracht 20zw /2     split as (Nieuwe gracht, 20zw /2)
Nieuwe gracht 20zw/3      split as (Nieuwe gracht, 20zw/3)
Nieuwe gracht 20 zw/4     split as (Nieuwe gracht, 20 zw/4)
Bahnhofstr. 4             split as (Bahnhofstr., 4)
Wertstr. 10               split as (Wertstr., 10)
Lindenhof 1               split as (Lindenhof, 1)
Nordesch 20               split as (Nordesch, 20)
Weilstr. 6                split as (Weilstr., 6)
Harthauer Weg 2           split as (Harthauer Weg, 2)
Mainaustr. 49             split as (Mainaustr., 49)
August-Horch-Str. 3       split as (August-Horch-Str., 3)
Marktplatz 31             split as (Marktplatz, 31)
Schmidener Weg 3          split as (Schmidener Weg, 3)
Karl-Weysser-Str. 6       split as (Karl-Weysser-Str., 6)

```



## Tcl

{{trans|Scala}}

```tcl
proc split_DE_NL_address {streetAddress} {
    set RE {(?x)
	^ (.*?) (
	    (?:\s \d+ [-/] \d+)
	|
	    (?:\s (?!1940|1945)\d+ [a-zI. /]* \d*)
	)? $
    }
    regexp $RE $streetAddress -> str num
    return [list [string trim $str] [string trim $num]]
}

set data {
    Plataanstraat 5
    Straat 12
    Straat 12 II
    Dr. J. Straat   12
    Dr. J. Straat 12 a
    Dr. J. Straat 12-14
    Laan 1940 – 1945 37
    Plein 1940 2
    1213-laan 11
    16 april 1944 Pad 1
    1e Kruisweg 36
    Laan 1940-’45 66
    Laan ’40-’45
    Langeloërduinen 3 46
    Marienwaerdt 2e Dreef 2
    Provincialeweg N205 1
    Rivium 2e Straat 59.
    Nieuwe gracht 20rd
    Nieuwe gracht 20rd 2
    Nieuwe gracht 20zw /2
    Nieuwe gracht 20zw/3
    Nieuwe gracht 20 zw/4
    Bahnhofstr. 4
    Wertstr. 10
    Lindenhof 1
    Nordesch 20
    Weilstr. 6
    Harthauer Weg 2
    Mainaustr. 49
    August-Horch-Str. 3
    Marktplatz 31
    Schmidener Weg 3
    Karl-Weysser-Str. 6
}

foreach streetAddress [split $data "\n"] {
    set streetAddress [string trim $streetAddress]
    if {$streetAddress eq ""} continue
    lassign [split_DE_NL_address $streetAddress] str num
    puts "split <$streetAddress> as <$str> <$num>"
}
```

{{out}}

```txt

split <Plataanstraat 5> as <Plataanstraat> <5>
split <Straat 12> as <Straat> <12>
split <Straat 12 II> as <Straat> <12 II>
split <Dr. J. Straat   12> as <Dr. J. Straat> <12>
split <Dr. J. Straat 12 a> as <Dr. J. Straat> <12 a>
split <Dr. J. Straat 12-14> as <Dr. J. Straat> <12-14>
split <Laan 1940 – 1945 37> as <Laan 1940 – 1945> <37>
split <Plein 1940 2> as <Plein 1940> <2>
split <1213-laan 11> as <1213-laan> <11>
split <16 april 1944 Pad 1> as <16 april 1944 Pad> <1>
split <1e Kruisweg 36> as <1e Kruisweg> <36>
split <Laan 1940-’45 66> as <Laan 1940-’45> <66>
split <Laan ’40-’45> as <Laan ’40-’45> <>
split <Langeloërduinen 3 46> as <Langeloërduinen> <3 46>
split <Marienwaerdt 2e Dreef 2> as <Marienwaerdt 2e Dreef> <2>
split <Provincialeweg N205 1> as <Provincialeweg N205> <1>
split <Rivium 2e Straat 59.> as <Rivium 2e Straat> <59.>
split <Nieuwe gracht 20rd> as <Nieuwe gracht> <20rd>
split <Nieuwe gracht 20rd 2> as <Nieuwe gracht> <20rd 2>
split <Nieuwe gracht 20zw /2> as <Nieuwe gracht> <20zw /2>
split <Nieuwe gracht 20zw/3> as <Nieuwe gracht> <20zw/3>
split <Nieuwe gracht 20 zw/4> as <Nieuwe gracht> <20 zw/4>
split <Bahnhofstr. 4> as <Bahnhofstr.> <4>
split <Wertstr. 10> as <Wertstr.> <10>
split <Lindenhof 1> as <Lindenhof> <1>
split <Nordesch 20> as <Nordesch> <20>
split <Weilstr. 6> as <Weilstr.> <6>
split <Harthauer Weg 2> as <Harthauer Weg> <2>
split <Mainaustr. 49> as <Mainaustr.> <49>
split <August-Horch-Str. 3> as <August-Horch-Str.> <3>
split <Marktplatz 31> as <Marktplatz> <31>
split <Schmidener Weg 3> as <Schmidener Weg> <3>
split <Karl-Weysser-Str. 6> as <Karl-Weysser-Str.> <6>

```


## TUSCRIPT


```tuscript

$$ MODE DATA

$$ addressen=*
Plataanstraat 5
Straat 12
Straat 12 II
Dr. J. Straat   12
Dr. J. Straat 12 a
Dr. J. Straat 12-14
Laan 1940 - 1945 37
Plein 1940 2
1213-laan 11
16 april 1944 Pad 1
1e Kruisweg 36
Laan 1940-'45 66
Laan '40-'45
Langeloërduinen 3 46
Marienwaerdt 2e Dreef 2
Provincialeweg N205 1
Rivium 2e Straat 59.
Nieuwe gracht 20rd
Nieuwe gracht 20rd 2
Nieuwe gracht 20zw /2
Nieuwe gracht 20zw/3
Nieuwe gracht 20 zw/4
Bahnhofstr. 4
Wertstr. 10
Lindenhof 1
Nordesch 20
Weilstr. 6
Harthauer Weg 2
Mainaustr. 49
August-Horch-Str. 3
Marktplatz 31
Schmidener Weg 3
Karl-Weysser-Str. 6
$$ MODE TUSCRIPT,{}

BUILD S_TABLE regex=*
DATA : {\0}*{]}:
DATA : {\0}*{1-3}[IVXLC]{]}:
DATA :: 194{\0}::
DATA :: '{2}{\0}::
DATA :: {\0}*{\A}::

x= SPLIT(addressen,|regex,street,number)

output=JOIN(street," <--> ",number)

TRACE *output

```

Output:
<pre style='height:30ex;overflow:scroll'>
TRACE *    50    -*SKRIPTE  202
output       = *
           1 = Plataanstraat <-->  5
           2 = Straat <-->  12
           3 = Straat <-->  12 II
           4 = Dr. J. Straat   <-->  12
           5 = Dr. J. Straat <-->  12 a
           6 = Dr. J. Straat <-->  12-14
           7 = Laan 1940 - 1945 <-->  37
           8 = Plein 1940 <-->  2
           9 = 1213-laan <-->  11
          10 = 16 april 1944 Pad <-->  1
          11 = 1e Kruisweg <-->  36
          12 = Laan 1940-'45 <-->  66
          13 = Laan '40-'45 <--> 
          14 = Langeloërduinen <-->  3 46
          15 = Marienwaerdt 2e Dreef <-->  2
          16 = Provincialeweg N205 <-->  1
          17 = Rivium 2e Straat <-->  59.
          18 = Nieuwe gracht <-->  20rd
          19 = Nieuwe gracht <-->  20rd 2
          20 = Nieuwe gracht <-->  20zw /2
          21 = Nieuwe gracht <-->  20zw/3
          22 = Nieuwe gracht <-->  20 zw/4
          23 = Bahnhofstr. <-->  4
          24 = Wertstr. <-->  10
          25 = Lindenhof <-->  1
          26 = Nordesch <-->  20
          27 = Weilstr. <-->  6
          28 = Harthauer Weg <-->  2
          29 = Mainaustr. <-->  49
          30 = August-Horch-Str. <-->  3
          31 = Marktplatz <-->  31
          32 = Schmidener Weg <-->  3
          33 = Karl-Weysser-Str. <-->  6

```

