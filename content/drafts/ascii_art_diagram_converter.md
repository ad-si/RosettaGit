+++
title = "ASCII art diagram converter"
description = ""
date = 2019-06-16T21:10:22Z
aliases = []
[extra]
id = 16505
[taxonomies]
categories = []
tags = []
+++

{{draft task}}
Given the RFC 1035 message diagram from Section 4.1.1 (Header section format) as a string:
http://www.ietf.org/rfc/rfc1035.txt

 +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 |                      ID                       |
 +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
 +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 |                    QDCOUNT                    |
 +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 |                    ANCOUNT                    |
 +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 |                    NSCOUNT                    |
 +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 |                    ARCOUNT                    |
 +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

Where (every column of the table is 1 bit):

 ID is 16 bits
 QR = Query (0) or Response (1)
 Opcode = Four bits defining kind of query:
   0:    a standard query (QUERY)
   1:    an inverse query (IQUERY)
   2:    a server status request (STATUS)
   3-15: reserved for future use
 AA = Authoritative Answer bit
 TC = Truncation bit
 RD = Recursion Desired bit
 RA = Recursion Available bit
 Z = Reserved
 RCODE = Response code
 QC = Question Count
 ANC = Answer Count
 AUC = Authority Count
 ADC = Additional Count

Write a function, member function, class or template that accepts a similar multi-line string as input to define a data structure or something else able to decode or store a header with that specified bit structure.

If your language has macros, introspection, code generation, or powerful enough templates, then accept such string at compile-time to define the header data structure statically.

Such "Header" function or template should accept a table with 8, 16, 32 or 64 columns, and any number of rows. For simplicity the only allowed symbols to define the table are + - | (plus, minus, pipe), and whitespace. Lines of the input string composed just of whitespace should be ignored. Leading and trailing whitespace in the input string should be ignored, as well as  before and after each table row. The box for each bit of the diagram takes four chars "+--+". The code should perform a little of validation of the input string, but for brevity a full validation is not required.

Bonus: perform a thoroughly validation of the input string.


## D

This solution generates anonymous struct code at compile-time, that can be mixed-in inside a struct or class.

```d
string makeStructFromDiagram(in string rawDiagram) pure @safe {
    import std.conv: text;
    import std.format: format;
    import std.string: strip, splitLines, indexOf;
    import std.array: empty, popFront;

    static void commitCurrent(ref uint anonCount,
                              ref uint totalBits,
                              ref size_t currentBits,
                              ref string code,
                              ref string currentName) pure @safe {
        if (currentBits) {
            code ~= "\t";

            currentName = currentName.strip;
            if (currentName.empty) {
                anonCount++;
                currentName = "anonymous_field_" ~ anonCount.text;
            }

            string type;
            if (currentBits == 1)
                type = "bool";
            else if (currentBits <= ubyte.sizeof * 8)
                type = "ubyte";
            else if (currentBits <= ushort.sizeof * 8)
                type = "ushort";
            else if (currentBits <= uint.sizeof * 8)
                type = "uint";
            else if (currentBits <= ulong.sizeof * 8)
                type = "ulong";
            //else if (currentBits <= ucent.sizeof * 8)
            //    type = "ucent";
            else assert(0, "Too many bits for the item " ~ currentName);

            immutable byteOffset = totalBits / 8;
            immutable bitOffset = totalBits % 8;


            // Getter:
            code ~= "@property " ~ type ~ " " ~ currentName ~
                    "() const pure nothrow @safe {\n";
            code ~= "\t\t";
            if (currentBits == 1) {
                code ~= format("return (_payload[%d] & (1 << (7-%d))) ? true : false;",
                               byteOffset, bitOffset);
            } else if (currentBits < 8) {
                auto mask = (1 << currentBits) - 1;
                mask <<= 7 - bitOffset - currentBits + 1;
                code ~= format("return (_payload[%d] & 0b%08b) >> %d;",
                               byteOffset, mask, 7 - bitOffset - currentBits + 1);
            } else {
                assert(currentBits % 8 == 0);
                assert(bitOffset == 0);
                code ~= type ~ " v = 0;\n\t\t";

                code ~= "version(LittleEndian) {\n\t\t";
                foreach (immutable i; 0 .. currentBits / 8)
                    code ~=  "\tv |= (cast(" ~ type ~ ") _payload[" ~
                             text(byteOffset + i) ~ "]) << (" ~
                             text((currentBits / 8) - i - 1) ~
                             " * 8);\n\t\t";
                code ~= "} else static assert(0);\n\t\t";
                code ~= "return v;";
            }
            code ~= "\n";
            code ~= "\t}\n\t";


            // Setter:
            code ~= "@property void " ~ currentName ~ "(in " ~ type ~
                    " value) pure nothrow @safe {\n";
            code ~= "\t\t";
            if (currentBits < 8) {
                auto mask = (1 << currentBits) - 1;
                mask <<= 7 - bitOffset - currentBits + 1;
                code ~= format("_payload[%d] &= ~0b%08b;\n\t\t",
                               byteOffset, mask);
                code ~= "assert(value < " ~ text(1 << currentBits) ~
                        ");\n\t\t";
                code~=format("_payload[%d] |= cast(ubyte) value << %d;",
                               byteOffset, 7 - bitOffset - currentBits + 1);
            } else {
                assert(currentBits % 8 == 0);
                assert(bitOffset == 0);

                code ~= "version(LittleEndian) {\n\t\t";
                foreach (immutable i; 0 .. currentBits / 8)
                    code ~= "\t_payload[" ~ text(byteOffset + i) ~
                            "] = (value >> (" ~
                            text((currentBits / 8) - i - 1) ~
                            " * 8) & 0xff);\n\t\t";
                code ~= "} else static assert(0);";
            }

            code ~= "\n";
            code ~= "\t}\n";
            totalBits += currentBits;
        }

        currentBits = 0;
        currentName = null;
    }

    enum C : char { pipe='|', cross='+' }
    enum cWidth = 3; // Width of a bit cell in the table.
    immutable diagram = rawDiagram.strip;

    size_t bitCountPerRow = 0, currentBits;
    uint anonCount = 0, totalBits;
    string currentName;
    string code = "struct {\n"; // Anonymous.

    foreach (line; diagram.splitLines) {
        assert(!line.empty);
        line = line.strip;
        if (line[0] == C.cross) {
            commitCurrent(anonCount, totalBits, currentBits, code, currentName);
            if (bitCountPerRow == 0)
                bitCountPerRow = (line.length - 1) / cWidth;
            else
                assert(bitCountPerRow == (line.length - 1) / cWidth);
        } else {
            // A field of some sort.
            while (line.length > 2) {
                assert(line[0] != '/',
                       "Variable length data not supported");
                assert(line[0] == C.pipe, "Malformed table");
                line.popFront;
                const idx = line[0 .. $ - 1].indexOf(C.pipe);
                if (idx != -1) {
                    const field = line[0 .. idx];
                    line = line[idx .. $];

                    commitCurrent(anonCount, totalBits, currentBits, code, currentName);
                    currentName = field;
                    currentBits = (field.length + 1) / cWidth;
                    commitCurrent(anonCount, totalBits, currentBits, code, currentName);
                } else {
                    // The full row or a continuation of the last.
                    currentName ~= line[0 .. $ - 1];
                    // At this point, line does not include the first
                    // C.pipe, but the length will include the last.
                    currentBits += line.length / cWidth;

                    line = line[$ .. $];
                }
            }
        }
    }

    // Using bytes to avoid endianness issues.
    // hopefully the compiler will optimize it, otherwise
    // maybe we could specialize the properties more.
    code ~= "\n\tprivate ubyte[" ~ text((totalBits + 7) / 8) ~ "] _payload;\n";

    return code ~ "}";
}


void main() { // Testing.
    import std.stdio;

    enum diagram = "
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                      ID                       |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    QDCOUNT                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    ANCOUNT                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    NSCOUNT                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    ARCOUNT                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+";

    // To debug the code generation:
    //pragma(msg, diagram.makeStructFromDiagram);

    // Usage.
    static struct Header {
        mixin(diagram.makeStructFromDiagram);
    }

    Header h;
    h.ID = 10;
    h.RA = true;
    h.ARCOUNT = 255;
    h.Opcode = 7;

    // See the byte representation to test the setter's details.
    h._payload.writeln;

    // Test the getters:
    assert(h.ID == 10);
    assert(h.RA == true);
    assert(h.ARCOUNT == 255);
    assert(h.Opcode == 7);
}
```

{{out}}

```txt
[0, 10, 56, 128, 0, 0, 0, 0, 0, 0, 0, 255]
```


Static support for BigEndian is easy to add.

It also supports larger values like this, that is 32 bits long:


```txt
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                   ThirtyTwo                   |
|                                               |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
```



## Go


```go
package main

import (
    "fmt"
    "log"
    "math/big"
    "strings"
)

type result struct {
    name  string
    size  int
    start int
    end   int
}

func (r result) String() string {
    return fmt.Sprintf("%-7s   %2d    %3d   %3d", r.name, r.size, r.start, r.end)
}

func validate(diagram string) []string {
    var lines []string
    for _, line := range strings.Split(diagram, "\n") {
        line = strings.Trim(line, " \t")
        if line != "" {
            lines = append(lines, line)
        }
    }
    if len(lines) == 0 {
        log.Fatal("diagram has no non-empty lines!")
    }
    width := len(lines[0])
    cols := (width - 1) / 3
    if cols != 8 && cols != 16 && cols != 32 && cols != 64 {
        log.Fatal("number of columns should be 8, 16, 32 or 64")
    }
    if len(lines)%2 == 0 {
        log.Fatal("number of non-empty lines should be odd")
    }
    if lines[0] != strings.Repeat("+--", cols)+"+" {
        log.Fatal("incorrect header line")
    }
    for i, line := range lines {
        if i == 0 {
            continue
        } else if i%2 == 0 {
            if line != lines[0] {
                log.Fatal("incorrect separator line")
            }
        } else if len(line) != width {
            log.Fatal("inconsistent line widths")
        } else if line[0] != '|' || line[width-1] != '|' {
            log.Fatal("non-separator lines must begin and end with '|'")
        }
    }
    return lines
}

func decode(lines []string) []result {
    fmt.Println("Name     Bits  Start  End")
    fmt.Println("
### ====  ====  =====  
")
    start := 0
    width := len(lines[0])
    var results []result
    for i, line := range lines {
        if i%2 == 0 {
            continue
        }
        line := line[1 : width-1]
        for _, name := range strings.Split(line, "|") {
            size := (len(name) + 1) / 3
            name = strings.TrimSpace(name)
            res := result{name, size, start, start + size - 1}
            results = append(results, res)
            fmt.Println(res)
            start += size
        }
    }
    return results
}

func unpack(results []result, hex string) {
    fmt.Println("\nTest string in hex:")
    fmt.Println(hex)
    fmt.Println("\nTest string in binary:")
    bin := hex2bin(hex)
    fmt.Println(bin)
    fmt.Println("\nUnpacked:\n")
    fmt.Println("Name     Size  Bit pattern")
    fmt.Println("
### ====  ====  =============
")
    for _, res := range results {
        fmt.Printf("%-7s   %2d   %s\n", res.name, res.size, bin[res.start:res.end+1])
    }
}

func hex2bin(hex string) string {
    z := new(big.Int)
    z.SetString(hex, 16)
    return fmt.Sprintf("%0*b", 4*len(hex), z)
}

func main() {
    const diagram = `
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
         |                      ID                       |
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
        |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
        |                    QDCOUNT                    |
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

        |                    ANCOUNT                    |
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
        |                    NSCOUNT                    |
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
        |                    ARCOUNT                    |
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    `
    lines := validate(diagram)
    fmt.Println("Diagram after trimming whitespace and removal of blank lines:\n")
    for _, line := range lines {
        fmt.Println(line)
    }
    fmt.Println("\nDecoded:\n")
    results := decode(lines)
    hex := "78477bbf5496e12e1bf169a4" // test string
    unpack(results, hex)
}
```


{{out}}

```txt

Diagram after trimming whitespace and removal of blank lines:

+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                      ID                       |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    QDCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    ANCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    NSCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    ARCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

Decoded:

Name     Bits  Start  End

### ====  ====  =====  

ID        16      0    15
QR         1     16    16
Opcode     4     17    20
AA         1     21    21
TC         1     22    22
RD         1     23    23
RA         1     24    24
Z          3     25    27
RCODE      4     28    31
QDCOUNT   16     32    47
ANCOUNT   16     48    63
NSCOUNT   16     64    79
ARCOUNT   16     80    95

Test string in hex:
78477bbf5496e12e1bf169a4

Test string in binary:
011110000100011101111011101111110101010010010110111000010010111000011011111100010110100110100100

Unpacked:

Name     Size  Bit pattern

### ====  ====  =============

ID        16   0111100001000111
QR         1   0
Opcode     4   1111
AA         1   0
TC         1   1
RD         1   1
RA         1   1
Z          3   011
RCODE      4   1111
QDCOUNT   16   0101010010010110
ANCOUNT   16   1110000100101110
NSCOUNT   16   0001101111110001
ARCOUNT   16   0110100110100100

```



## J



```J
require'strings'

soul=: -. {.
normalize=: [:soul' ',dltb;._2

mask=: 0: _1} '+' = {.
partition=: '|' = mask #"1 soul
labels=: ;@(([: <@}: <@dltb;._1)"1~ '|'&=)@soul
names=: ;:^:(0 = L.)

unpacker=:1 :0
  p=. , partition normalize m
  p #.;.1 (8#2) ,@:#: ]
)

packer=:1 :0
  w=. -#;.1 ,partition normalize m
  _8 (#.\ ;) w ({. #:)&.> ] 
)

getter=:1 :0
  nm=. labels normalize m
  (nm i. names@[) { ]
)

setter=:1 :0
  q=. ''''
  n=. q,~q,;:inv labels normalize m
  1 :('(',n,' i.&names m)}')
)

starter=:1 :0
  0"0 labels normalize m
)
```


Sample definition (note the deliberate introduction of extraneous whitespace in locations the task requires us to ignore it.


```j
sample=: 0 :0
  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                      ID                       |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    QDCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    ANCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+  
|                    NSCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    ARCOUNT                    |  
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

)

unpack=: sample unpacker
pack=: sample packer
get=: sample getter
set=: sample setter
start=: sample starter
```


Example data for sample definition:


```J

   4095 13 5 6144 4096 'ID Opcode RCODE ARCOUNT QDCOUNT' set start
4095 0 13 0 0 0 0 0 5 4096 0 0 6144
   pack 4095 13 5 6144 4096 'ID Opcode RCODE ARCOUNT QDCOUNT' set start
15 255 104 5 16 0 0 0 0 0 24 0
   unpack 0 10 56 128 0 0 0 0 0 0 0 255
10 0 7 0 0 0 1 0 0 0 0 0 255
   'Opcode' get unpack 0 10 56 128 0 0 0 0 0 0 0 255
7
```


In other words:

:unpack converts an octet sequence to the corresponding numeric sequence
:pack converts a numeric sequence to the corresponding octet sequence
:get extracts named elements from the numeric sequence
:set updates named elements in the numeric sequence
:start represents the default "all zeros" sequence which may be used to derive other sequences

Note that this implementation assumes that the ascii diagram represents the native word width on a single line, and assumes well formed data.



## Julia

The validator() function can be customized. The one used only checks length.

```julia
diagram = """
         +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
         |                      ID                       |
         +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
         |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
         +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
         |                    QDCOUNT                    |
         +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
         |                    ANCOUNT                    |
         +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
         |                    NSCOUNT                    |
         +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
         |                    ARCOUNT                    |
         +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+"""

testhexdata = "78477bbf5496e12e1bf169a4"

struct BitField
    name::String
    bits::Int
    fieldstart::Int
    fieldend::Int
end

function diagramtostruct(txt)
    bitfields = Vector{BitField}()
    lines = map(strip, split(txt, "\n"))
    for row in 1:2:length(lines)-1
        nbits = sum(x -> x == '+', lines[row]) - 1
        fieldpos = findall(x -> x == '|', lines[row + 1])
        bitaccum = div(row, 2) * nbits
        for (i, field) in enumerate(fieldpos[1:end-1])
            endfield = fieldpos[i + 1]
            bitsize = div(endfield - field, 3)
            bitlabel = strip(lines[row + 1][field+1:endfield-1])
            bitstart = div(field - 1, 3) + bitaccum
            bitend = bitstart + bitsize - 1
            push!(bitfields, BitField(bitlabel, bitsize, bitstart, bitend))
        end
    end
    bitfields
end

binbyte(c) = string(parse(UInt8, c, base=16), base=2, pad=8)
hextobinary(s) = reduce(*, map(binbyte, map(x -> s[x:x+1], 1:2:length(s)-1)))
validator(binstring, fields) = length(binstring) == sum(x -> x.bits, fields)

function bitreader(bitfields, hexdata)
    println("\nEvaluation of hex data $hexdata as bitfields:")
    println("Name     Size          Bits\n-------  ----  ----------------")
    b = hextobinary(hexdata)
    @assert(validator(b, bitfields))
    for bf in bitfields
        pat = b[bf.fieldstart+1:bf.fieldend+1]
        println(rpad(bf.name, 9), rpad(bf.bits, 6), lpad(pat, 16))
    end
end

const decoded = diagramtostruct(diagram)

println("Diagram as bit fields:\nName    Bits  Start  End\n------  ----  -----  ---")
for bf in decoded
    println(rpad(bf.name, 8), rpad(bf.bits, 6), rpad(bf.fieldstart, 6), lpad(bf.fieldend, 4))
end

bitreader(decoded, testhexdata)

```
{{out}}

```txt

Diagram as bit fields:
Name    Bits  Start  End
------  ----  -----  ---
ID      16    0       15
QR      1     16      16
Opcode  4     17      20
AA      1     21      21
TC      1     22      22
RD      1     23      23
RA      1     24      24
Z       3     25      27
RCODE   4     28      31
QDCOUNT 16    32      47
ANCOUNT 16    48      63
NSCOUNT 16    64      79
ARCOUNT 16    80      95

Evaluation of hex data 78477bbf5496e12e1bf169a4 as bitfields:
Name     Size          Bits
-------  ----  ----------------
ID       16    0111100001000111
QR       1                    0
Opcode   4                 1111
AA       1                    0
TC       1                    1
RD       1                    1
RA       1                    1
Z        3                  011
RCODE    4                 1111
QDCOUNT  16    0101010010010110
ANCOUNT  16    1110000100101110
NSCOUNT  16    0001101111110001
ARCOUNT  16    0110100110100100

```



## Perl 6

{{works with|Rakudo|2018.05}}


```perl6
grammar RFC1025 {
    rule  TOP {  <.line-separator> [<line> <.line-separator>]+ }
    rule  line-separator { <.ws> '+--'+ '+' }
    token line  { <.ws> '|' +%% <field>  }
    token field  { \s* <label> \s* }
    token label { \w+[\s+\w+]* }
}

sub bits ($item) { ($item.chars + 1) div 3 }

sub deconstruct ($bits, %struct) {
    map { $bits.substr(.<from>, .<bits>) }, @(%struct<fields>);
}

sub interpret ($header) {
    my $datagram = RFC1025.parse($header);
    my %struct;
    for $datagram.<line> -> $line {
        FIRST %struct<line-width> = $line.&bits;
        state $from = 0;
        %struct<fields>.push: %(:bits(.&bits), :ID(.<label>.Str), :from($from.clone), :to(($from+=.&bits)-1))
          for $line<field>;
    }
    %struct
}

use experimental :pack;

my $diagram = q:to/END/;

    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                      ID                       |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    QDCOUNT                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    ANCOUNT                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    NSCOUNT                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    ARCOUNT                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+

END

my %structure = interpret($diagram);

say 'Line width: ', %structure<line-width>, ' bits';
printf("Name: %7s, bit count: %2d, bit %2d to bit %2d\n", .<ID>, .<bits>, .<from>, .<to>) for @(%structure<fields>);
say "\nGenerate a random 12 byte \"header\"";
say my $buf = Buf.new((^0xFF .roll) xx 12);
say "\nShow it converted to a bit string";
say my $bitstr = $buf.unpack('C*')».fmt("%08b").join;
say "\nAnd unpack it";
printf("%7s, %02d bits: %s\n", %structure<fields>[$_]<ID>,  %structure<fields>[$_]<bits>,
  deconstruct($bitstr, %structure)[$_]) for ^@(%structure<fields>);
```

{{out}}

```txt
Line width: 16 bits
Name:      ID, bit count: 16, bit  0 to bit 15
Name:      QR, bit count:  1, bit 16 to bit 16
Name:  Opcode, bit count:  4, bit 17 to bit 20
Name:      AA, bit count:  1, bit 21 to bit 21
Name:      TC, bit count:  1, bit 22 to bit 22
Name:      RD, bit count:  1, bit 23 to bit 23
Name:      RA, bit count:  1, bit 24 to bit 24
Name:       Z, bit count:  3, bit 25 to bit 27
Name:   RCODE, bit count:  4, bit 28 to bit 31
Name: QDCOUNT, bit count: 16, bit 32 to bit 47
Name: ANCOUNT, bit count: 16, bit 48 to bit 63
Name: NSCOUNT, bit count: 16, bit 64 to bit 79
Name: ARCOUNT, bit count: 16, bit 80 to bit 95

Generate a random 12 byte "header"
Buf:0x<78 47 7b bf 54 96 e1 2e 1b f1 69 a4>

Show it converted to a bit string
011110000100011101111011101111110101010010010110111000010010111000011011111100010110100110100100

And unpack it
     ID, 16 bits: 0111100001000111
     QR, 01 bits: 0
 Opcode, 04 bits: 1111
     AA, 01 bits: 0
     TC, 01 bits: 1
     RD, 01 bits: 1
     RA, 01 bits: 1
      Z, 03 bits: 011
  RCODE, 04 bits: 1111
QDCOUNT, 16 bits: 0101010010010110
ANCOUNT, 16 bits: 1110000100101110
NSCOUNT, 16 bits: 0001101111110001
ARCOUNT, 16 bits: 0110100110100100
```



## Phix

Should work on any width, but didn't actually test, or verify width is 8/16/32/64.

```Phix
function interpret(sequence lines)
    if remainder(length(lines),2)!=1 then
        crash("missing header/footer?")
    end if
    string l1 = lines[1]
    integer w = length(l1)
    integer bits = (w-1)/3  -- sug: check this is 8/16/32/64
    if l1!=join(repeat("+",bits+1),"--") then
        crash("malformed header?")
    end if
    sequence res = {}
    integer offset = 0
    for i=1 to length(lines) do
        string li = lines[i]
        if remainder(i,2) then
            if li!=l1 then
                crash("missing separator (line %d)?",{i})
            end if
        else
            if li[1]!='|' or li[w]!='|' then
                crash("missing separator on line %d",{i})
            end if
            integer k = 1
            while true do
                integer l = find('|',li,k+1)
                string desc = trim(li[k+1..l-1])
                {k,l} = {l,(l-k)/3}
                res = append(res,{desc,l,offset})
                offset += l
                if k=w then exit end if
            end while                   
        end if
    end for
    res = append(res,{"total",0,offset})
    return res
end function

procedure unpack(string data, sequence res)
    if length(data)*8!=res[$][3] then
        crash("wrong length")
    end if
    string bin = ""
    for i=1 to length(data) do
        bin &= sprintf("%08b",data[i])
    end for
    printf(1,"\n\nTest bit string:\n%s\n\nUnpacked:\n",{bin})
    for i=1 to length(res)-1 do
        {string name, integer bits, integer offset} = res[i]
        printf(1,"%7s, %02d bits: %s\n",{name,bits,bin[offset+1..offset+bits]})
    end for
end procedure

function trimskip(string diagram)
--
-- split's ",no_empty:=true)" is not quite enough here.
-- Note that if copy/paste slips in any tab characters, 
-- it will most likely trigger a length mismatch error.
--
    sequence lines = split(diagram,'\n')
    integer prevlli = 0
    for i=length(lines) to 1 by -1 do
        string li = trim(lines[i])
        integer lli = length(li)
        if lli then
            if prevlli=0 then
                prevlli = lli
            elsif lli!=prevlli then
                crash("mismatching lengths")
            end if
            lines[i] = li
        else
            lines[i..i] = {}
        end if
    end for
    return lines
end function

constant diagram = """

    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                      ID                       |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    QDCOUNT                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    ANCOUNT                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    NSCOUNT                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    |                    ARCOUNT                    |
    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
 
"""

sequence lines = trimskip(diagram)
sequence res = interpret(lines)
printf(1,"--Name--  Size  Offset\n")
for i=1 to length(res) do
    printf(1," %-7s   %2d  %5d\n",res[i])
end for

unpack(x"78477bbf5496e12e1bf169a4",res)
```

{{out}}

```txt

--Name--  Size  Offset
 ID        16      0
 QR         1     16
 Opcode     4     17
 AA         1     21
 TC         1     22
 RD         1     23
 RA         1     24
 Z          3     25
 RCODE      4     28
 QDCOUNT   16     32
 ANCOUNT   16     48
 NSCOUNT   16     64
 ARCOUNT   16     80
 total      0     96


Test bit string:
011110000100011101111011101111110101010010010110111000010010111000011011111100010110100110100100

Unpacked:
     ID, 16 bits: 0111100001000111
     QR, 01 bits: 0
 Opcode, 04 bits: 1111
     AA, 01 bits: 0
     TC, 01 bits: 1
     RD, 01 bits: 1
     RA, 01 bits: 1
      Z, 03 bits: 011
  RCODE, 04 bits: 1111
QDCOUNT, 16 bits: 0101010010010110
ANCOUNT, 16 bits: 1110000100101110
NSCOUNT, 16 bits: 0001101111110001
ARCOUNT, 16 bits: 0110100110100100

```



## Racket


Three files:
* <b><code>ascii-art-parser.rkt</code>:</b> provides the function <code>ascii-art->struct</code>, which converts ASCII art from a string (or input port) to a list of word-number, bit range and id
* <b><code>ascii-art-reader.rkt</code>:</b> uses this to provide a sytntax <code>define-ascii-art-structure</code> which defines a structure using the art work
* <b><code>test-ascci-art-reader.rkt</code>:</b> gives it all a rigourousish going over

Note that if you want to extend the word width too 32-bits (or more) add multiples of eight bit blocks horizontally (i.e. <code>--+--+--+--+--+--+--+--+</code>). IMO Having the diagrams 16-bits wide reflects the choice of a 16-bit word as the natural word size of the interface. If it were 32 or 64, the blocks would have to be wider.

<b><code>ascii-art-parser.rkt</code></b>
Note that this is in the <code>racket/base</code> language so it doesn't overburden the modules that import it, especially since they're at the suntax phase.

```racket
#lang racket/base
(require (only-in racket/list drop-right)
         (only-in racket/string string-trim))

(provide ascii-art->struct)

;; reads ascii art from a string or input-port
;; returns:
;;   list of (word-number highest-bit lowest-bit name-symbol)
;;   bits per word
(define (ascii-art->struct art)
  (define art-inport
    (cond
      [(string? art) (open-input-string art)]
      [(input-port? art) art]
      [else (raise-argument-error 'ascii-art->struct
                                  "(or/c string? input-port?)"
                                  art)]))
  (define lines
    (for/list ((l (in-port (lambda (p)
                             (define pk (peek-char p))
                             (case pk ((#\+ #\|) (read-line p))
                               (else eof)))
                           art-inport)))
      l))
  (when (null? lines)
    (error 'ascii-art->struct "no lines"))
  (define bit-re #px"[|+]([^|+]*)")
  (define cell-re #px"[|]([^|]*)")
  
  (define bit-boundaries (regexp-match-positions* bit-re (car lines)))
  
  (define bits/word (sub1 (length bit-boundaries)))
  
  (unless (zero? (modulo bits/word 8))
    (error 'ascii-art->struct "diagram is not a multiple of 8 bits wide"))
  
  (define-values (pos->bit-start# pos->bit-end#)
    (for/fold ((s# (hash)) (e# (hash)))
              ((box (in-range bits/word))
               (boundary (in-list bit-boundaries)))
      (define bit (- bits/word box 1))
      (values (hash-set s# (car boundary) bit)
              (hash-set e# (cdr boundary) bit))))
  
  (define fields
    (apply append
           (for/list ((line-number (in-naturals))
                      (line (in-list lines))
                      #:when (odd? line-number))
             (define word (quotient line-number 2))
             (define cell-positions (regexp-match-positions* cell-re line))
             (define cell-contents (regexp-match* cell-re line))
             (for/list ((cp (in-list (drop-right cell-positions 1)))
                        (cnt (in-list cell-contents)))
               (define cell-start-bit (hash-ref pos->bit-start# (car cp)))
               (define cell-end-bit (hash-ref pos->bit-end# (cdr cp)))
               (list word cell-start-bit cell-end-bit (string->symbol (string-trim (substring cnt 1))))))))
  (values fields bits/word))
```


<b><code>ascii-art-reader.rkt</code></b>

```racket
#lang racket
(require (for-syntax "ascii-art-parser.rkt"))
(require (for-syntax racket/syntax))

(provide (all-defined-out))

(define-syntax (define-ascii-art-structure stx)
  (syntax-case stx ()
    [(_ id art)
     (let*-values (((all-fields bits/word) (ascii-art->struct (syntax-e #'art))))
       (with-syntax
           ((bytes->id (format-id stx "bytes->~a" #'id))
            (id->bytes (format-id stx "~a->bytes" #'id))
            (word-size (add1 (car (for/last ((f all-fields)) f))))
            (fld-ids (map cadddr all-fields))
            
            (fld-setters
             (cons
              #'id
              (for/list ((fld (in-list all-fields)))
                (let* ((bytes/word (quotient bits/word 8))
                       (start-byte (let ((word-no (car fld))) (* word-no bytes/word))))
                  `(bitwise-bit-field (integer-bytes->integer bs
                                                              #f
                                                              (system-big-endian?)
                                                              ,start-byte
                                                              ,(+ start-byte bytes/word))
                                      ,(caddr fld)
                                      ,(add1 (cadr fld)))))))
            
            (set-fields-bits
             (list*
              'begin
              (for/list ((fld (in-list all-fields)))
                (define val (cadddr fld))
                (define start-bit (cadr fld))
                (define end-bit (caddr fld))
                (define start-byte (let ((word-no (car fld))) (* word-no (quotient bits/word 8))))
                (define fld-bit-width (- start-bit end-bit -1))
                (define aligned?/width (and (= end-bit 0)
                                            (= (modulo start-bit 8) 7)
                                            (quotient fld-bit-width 8)))
                (case aligned?/width
                  [(2 4)
                   `(integer->integer-bytes ,val
                                            ,aligned?/width
                                            #f
                                            (system-big-endian?)
                                            rv
                                            ,start-byte)]
                  [else
                   (define the-byte (+ start-byte (quotient end-bit 8)))
                   `(bytes-set! rv
                                ,the-byte
                                (bitwise-ior (arithmetic-shift (bitwise-bit-field ,val 0 ,fld-bit-width)
                                                               ,(modulo end-bit 8))
                                             (bytes-ref rv ,the-byte)))])))))
         #`(begin
             (struct id fld-ids #:mutable)
             
             (define (bytes->id bs)
               fld-setters)
             
             (define (id->bytes art-in)
               (match-define (id #,@#'fld-ids) art-in)
               (define rv (make-bytes (* word-size #,(quotient bits/word 8))))
               set-fields-bits
               rv))))]))
```


<b><code>test-ascii-art-reader.rkt</code></b>

```racket
#lang racket
(require "ascii-art-reader.rkt")
(require "ascii-art-parser.rkt")
(require tests/eli-tester)

(define rfc-1035-header-art
  #<<EOS
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                      ID                       |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    QDCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    ANCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    NSCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    ARCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
EOS
  )

(define-values (rslt rslt-b/w) (ascii-art->struct rfc-1035-header-art))

(test
 rslt-b/w => 16
 rslt =>
 '((0 15  0 ID)
   (1 15 15 QR)
   (1 14 11 Opcode)
   (1 10 10 AA)
   (1  9  9 TC)
   (1  8  8 RD)
   (1  7  7 RA)
   (1  6  4 Z)
   (1  3  0 RCODE)
   (2 15  0 QDCOUNT)
   (3 15  0 ANCOUNT)
   (4 15  0 NSCOUNT)
   (5 15  0 ARCOUNT)))

(define-ascii-art-structure rfc-1035-header #<<EOS
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                      ID                       |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    QDCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    ANCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    NSCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
|                    ARCOUNT                    |
+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
EOS
                     )

(define h-bytes
  (bytes-append
   (integer->integer-bytes #x1234 2 #f)
   (integer->integer-bytes #x5678 2 #f)
   (integer->integer-bytes #x9abc 2 #f)
   (integer->integer-bytes #xdef0 2 #f)
   (integer->integer-bytes #xfedc 2 #f)
   (integer->integer-bytes #xba98 2 #f)))

(define h-bytes~
  (bytes-append
   (integer->integer-bytes #x1234 2 #f (not (system-big-endian?)))
   (integer->integer-bytes #x5678 2 #f (not (system-big-endian?)))
   (integer->integer-bytes #x9abc 2 #f (not (system-big-endian?)))
   (integer->integer-bytes #xdef0 2 #f (not (system-big-endian?)))
   (integer->integer-bytes #xfedc 2 #f (not (system-big-endian?)))
   (integer->integer-bytes #xba98 2 #f (not (system-big-endian?)))))

(define h (bytes->rfc-1035-header h-bytes))
(define bytes-h (rfc-1035-header->bytes h))

(define h~ (bytes->rfc-1035-header h-bytes~))
(define bytes-h~ (rfc-1035-header->bytes h~))

(test
 (rfc-1035-header-ID h) => #x1234
 (rfc-1035-header-ARCOUNT h) => #xBA98
 (rfc-1035-header-RCODE h) => 8
 (rfc-1035-header-ID h~) => #x3412
 (rfc-1035-header-ARCOUNT h~) => #x98BA
 (rfc-1035-header-RCODE h~) => 6
 h-bytes => bytes-h
 h-bytes~ => bytes-h~)

(set-rfc-1035-header-RA! h 0)

(set-rfc-1035-header-Z! h 7)
(test
 (rfc-1035-header-Z (bytes->rfc-1035-header (rfc-1035-header->bytes h))) => 7
 (rfc-1035-header-RA (bytes->rfc-1035-header (rfc-1035-header->bytes h))) => 0)
(set-rfc-1035-header-Z! h 15) ;; naughty -- might splat RA
(test
 (rfc-1035-header-Z (bytes->rfc-1035-header (rfc-1035-header->bytes h))) => 7
 (rfc-1035-header-RA (bytes->rfc-1035-header (rfc-1035-header->bytes h))) => 0)
```


{{out}}
Nothing much to see... all tests pass


## REXX

Some code was added to validate the input file. 

```rexx
/*REXX program interprets an  ASCII art diagram  for  names  and  their bit length(s).  */
numeric digits 100                               /*be able to handle large numbers.     */
er = '***error*** illegal input txt'             /*a literal used for error messages.   */
parse arg iFID test .                            /*obtain optional input─FID & test─data*/
if iFID=='' | iFID==","  then iFID= 'ASCIIART.TXT'               /*use the default iFID.*/
if test=='' | test==","  then test= 'cafe8050800000808080000a'   /* "   "     "    data.*/
w= 0;         wb= 0;      !.= 0;     $=          /*W   (max width name),  bits,  names. */
p.= 0;        p.0= 1                             /*P.α   is structure bit position.     */
                                                 /* [↓]  read the input text file (iFID)*/
  do j=1  while lines(iFID)\==0;     q= linein(iFID);             say  '■■■■■text►'q
  q= strip(q);            if q==''  then iterate /*strip leading and trailing blanks.   */
  _L= left(q, 1);         _R= right(q, 1)        /*get extreme left and right characters*/
                                                 /* [↓]  is this record an "in-between"?*/
  if _L=='+'  then do;    if verify(q, '+-')\==0  then say er    "(invalid grid):"     q
                          iterate                /*skip this record, it's a single "+". */
                   end
  if _L\=='|' | _R\=="|"  then do;      say er  '(boundary): '   q;     iterate
                               end
     do  until q=='|'; parse var q '|' n "|" -1 q;   x= n      /*parse record for names.*/
     n= strip(n);      w= max(w, length(n) );   if n==''  then leave   /*is  N   null?  */
     if words(n)\==1  then do;  say er'(invalid name): '    n;          iterate j
                           end                   /* [↑]  add more name validations.     */
     $$= $;     nn= n;       upper $$ n          /*N   could be a mixed─case name.      */
     if wordpos(nn, $$)\==0  then do;   say er  '(dup name):'    n;     iterate j
                                  end
     $= $ n                                      /*add the   N   (name)  to the $ list. */
     #= words($);     !.#= (length(x) + 1) % 3   /*assign the number of bits for  N.    */
     wb= max(wb, !.#)                            /*max # of bits; # names prev. to this.*/
     prev= # - 1;     p.#= p.prev + !.prev       /*number of names previous to this name*/
     end   /*until*/
  end      /*j*/

if j==1  then do;   say er   '(file not found): '     iFID;              exit 12
              end
say
     do k=1  for words($)
     say right( word($, k), w)right(!.k, 4)        "bits,  bit position:"right(p.k, 5)
     end   /*k*/
say                                              /* [↓]  Any (hex) data to test?        */
L= length(test);      if L==0  then exit         /*stick a fork in it,  we're all done. */
bits= x2b(test)                                  /*convert test data to a bit string.   */
wm= length( x2d( b2x( copies(1, wb) ) ) )  +  1  /*used for displaying max width numbers*/
say 'test (hex)='    test                 "    length="   L          'hexadecimal digits.'
say
    do r=1  by 8+8  to L*4;   _1= substr(bits, r, 8, 0);       _2= substr(bits, r+8, 8, 0)
    say 'test (bit)='   _1 _2          "   hex="          b2x(_1)     b2x(_2)
    end   /*r*/
say
    do m=1  for words($)                         /* [↓]  display hex string in lowercase*/
    _= translate( b2x( substr( bits, p.m, !.m) ),     'abcdef',    "ABCDEF" )
    say right( word($, m), w+2)     '  decimal='right( x2d(_), wm)         "      hex="  _
    end   /*m*/                                  /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

■■■■■text►
■■■■■text►
■■■■■text►     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
■■■■■text►     |                      ID                       |
■■■■■text►     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
■■■■■text►     |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
■■■■■text►     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
■■■■■text►     |                    QDCOUNT                    |
■■■■■text►     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
■■■■■text►     |                    ANCOUNT                    |
■■■■■text►     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
■■■■■text►     |                    NSCOUNT                    |
■■■■■text►     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
■■■■■text►     |                    ARCOUNT                    |
■■■■■text►     +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
■■■■■text►
■■■■■text►

     ID  16 bits,  bit position:    1
     QR   1 bits,  bit position:   17
 OPCODE   4 bits,  bit position:   18
     AA   1 bits,  bit position:   22
     TC   1 bits,  bit position:   23
     RD   1 bits,  bit position:   24
     RA   1 bits,  bit position:   25
      Z   3 bits,  bit position:   26
  RCODE   4 bits,  bit position:   29
QDCOUNT  16 bits,  bit position:   33
ANCOUNT  16 bits,  bit position:   49
NSCOUNT  16 bits,  bit position:   65
ARCOUNT  16 bits,  bit position:   81

test (hex)= cafe8050800000808080000a     length= 24 hexadecimal digits.

test (bit)= 11001010 11111110    hex= CA FE
test (bit)= 10000000 01010000    hex= 80 50
test (bit)= 10000000 00000000    hex= 80 00
test (bit)= 00000000 10000000    hex= 00 80
test (bit)= 10000000 10000000    hex= 80 80
test (bit)= 00000000 00001010    hex= 00 0A

       ID   decimal= 51966       hex= cafe
       QR   decimal=     1       hex= 1
   OPCODE   decimal=     0       hex= 0
       AA   decimal=     0       hex= 0
       TC   decimal=     0       hex= 0
       RD   decimal=     0       hex= 0
       RA   decimal=     0       hex= 0
        Z   decimal=     5       hex= 5
    RCODE   decimal=     0       hex= 0
  QDCOUNT   decimal= 32768       hex= 8000
  ANCOUNT   decimal=   128       hex= 0080
  NSCOUNT   decimal= 32896       hex= 8080
  ARCOUNT   decimal=    10       hex= 000a

```



## Tcl

{{improve|Tcl|This example is *incorrect*.  It relies on an assumption that sequential bitfields in the same byte can be parsed by the [binary] command, which is not the case.  The test "appears" correct because encode and decode suffer the same bug and hence round-trip works.

A wrapper which doesn't disturb the below code too much is in progress.}}

This is a nice task to illustrate a couple of important concepts in Tcl:

  * using [http://wiki.tcl.tk/5042 dict]ionaries, taking advantage of their ordering properties
  * the [http://wiki.tcl.tk/1181 binary] command
  * using (semi-)structured text as part of your source code

In this implementation, '''parse''' produces a dictionary from names to bit-lengths.  '''encode''' and '''decode''' use these to produce appropriate binary format strings, and then do what they say on the tin.  As implemented, this is limited to unsigned numeric values in fields.  Supporting unsigned values, strings and enums would require parsing a more complex annotation than only the ASCII art packet structure, but ought not take much more code.

```Tcl

namespace eval asciipacket {
    proc assert {expr} {    ;# for "static" assertions that throw nice errors
        if {![uplevel 1 [list expr $expr]]} {
            raise {ASSERT ERROR} "{$expr} {[subst -noc $expr]}"
        }
    }

    proc b2h {data} {       ;# format a binary string in hex digits
        binary scan $data H* hex; set hex
    }

    proc parse {s} {
        set result {}                       ;# we will return a dictionary
        set s [string trim $s]              ;# remove whitespace
        set s [split $s \n]                 ;# split into lines
        set s [lmap x $s {string trim $x}]  ;# trim whitespace from each line
        set s [lassign $s border0]          ;# pop off top border row
                                            ;# calculate chars per row, chars per bit
        set rowlen [llength [string map {+ \ } $border0]]
        set bitlen [expr {([string length $border0] - 1) / $rowlen}]
        assert {$bitlen * $rowlen + 1 == [string length $border0]}

        foreach {row border} $s {
            assert {$border eq $border0}
            set row [string trim $row |]
            foreach field [split $row |] {
                set len [string length |$field]
                assert {$len % $bitlen == 0}
                set name [string trim $field]
                set nbits [expr {$len / $bitlen}]
                assert {![dict exists $result $name]}
                dict set result $name $nbits
            }
        }
        return $result
    }

    proc encode {schema values} {
        set bincodes {1 B 8 c 16 S 32 W}    ;# see binary(n)
        set binfmt ""                       ;# format string
        set binargs ""                      ;# positional args
        dict for {name bitlen} $schema {
            set val [dict get $values $name]
            if {[dict exists $bincodes $bitlen]} {
                set fmt "[dict get $bincodes $bitlen]"
            } else {
                set val [format %0${bitlen}b $val]
                set fmt "B${bitlen}"
            }
            append binfmt $fmt
            lappend binargs $val
        }
        binary format $binfmt {*}$binargs
    }


    proc decode {schema data} {
        set result   {}                     ;# we will return a dict
        set bincodes {1 B 8 c 16 S 32 W}    ;# see binary(n)
        set binfmt   ""                     ;# format string
        set binargs  ""                     ;# positional args
        dict for {name bitlen} $schema {
            if {[dict exists $bincodes $bitlen]} {
                set fmt "[dict get $bincodes $bitlen]u" ;# note unsigned
            } else {
                set fmt "B${bitlen}"
            }
            append binfmt $fmt
            lappend binargs $name
        }
        binary scan $data $binfmt {*}$binargs
        foreach _ $binargs {
            dict set result $_ [set $_]
        }
        return $result
    }
}

```

And here is how to use it with the original test data:

```Tcl

proc test {} {
    set header {
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
        |                      ID                       |
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
        |QR|   Opcode  |AA|TC|RD|RA|   Z    |   RCODE   |
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
        |                    QDCOUNT                    |
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
        |                    ANCOUNT                    |
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
        |                    NSCOUNT                    |
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
        |                    ARCOUNT                    |
        +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
    }

    set schema [asciipacket::parse $header]
    set values {
        ID 0xcafe
        QR 1
        Opcode 5
        AA 1
        TC 0
        RD 0
        RA 1
        Z  4
        RCODE 8
        QDCOUNT 0x00a5
        ANCOUNT 0x0a50
        NSCOUNT 0xa500
        ARCOUNT 0x500a
    }
    set pkt [asciipacket::encode $schema $values]
    puts "encoded packet (hex): [asciipacket::b2h $pkt]"
    array set decoded [asciipacket::decode $schema $pkt]
    parray decoded
}
test

```

{{Out}}

```txt

encoded packet (hex): cafe805080000080808000a50a50a500500a
decoded(AA)      = 1
decoded(ANCOUNT) = 2640
decoded(ARCOUNT) = 20490
decoded(ID)      = 51966
decoded(NSCOUNT) = 42240
decoded(Opcode)  = 0101
decoded(QDCOUNT) = 165
decoded(QR)      = 1
decoded(RA)      = 1
decoded(RCODE)   = 1000
decoded(RD)      = 0
decoded(TC)      = 0
decoded(Z)       = 100

```

