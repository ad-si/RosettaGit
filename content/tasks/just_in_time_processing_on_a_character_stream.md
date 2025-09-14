+++
title = "Just in time processing on a character stream"
description = ""
date = 2019-08-30T11:11:46Z
aliases = []
[extra]
id = 16903
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "c",
  "cpp",
  "csharp",
  "d",
  "go",
  "java",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "rexx",
  "tcl",
  "zkl",
]
+++

Given a stream of characters, presumably (simulated) from a keyboard, that contain the separators "formfeed", "linefeed", "tab" and "space" characters. Print out the i<sup>th</sup> character of the i<sup>th</sup> tab-field of the i<sup>th</sup> line of the i<sup>th</sup> page to reveal a secret password.

Stop processing immediately upon encountering a "!" found uniquely in this <i>i,i,i,i</i> position (least the system self destruct).  The "!" may be found/permitted else where however, in which case it should be ignored.

Ideally this can be generalise as follows:
* The separators (formfeed, linefeed, tab, space) provided from a user supplied array and can include additional/alternative separators, e.g. (formfeed, linefeed, ".", "," ," ",...).
* These selection criterial is generalised i<sup>th</sup>,i<sup>th</sup>,i<sup>th</sup>,i<sup>th</sup> to a boolean function of <i>f(page,line,field,word,...) <b>or</b> f(i<sup>th</sup>,j<sup>th</sup>,k<sup>th</sup>,l<sup>th</sup>,m<sup>th</sup>,etc...)</i>

Provide a reasonably interesting message to be decoded, e.g. Silence-Dogood.  Your choice.

This task was inspired by the movie "[[wp:National_Treasure_%28film%29|National Treasure]]" with refers to a "[[wp:Book cipher|book cipher]]".


## C

```c
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct UserInput {
    char formFeed;
    char lineFeed;
    char tab;
    char space;
};

struct UserInputNode {
    struct UserInput ui;
    struct UserInputNode *next;
};

bool decode(FILE *fp, const struct UserInput ui) {
    char f = 0, l = 0, t = 0, s = 0;
    char buffer[1];

    while (fread(buffer, 1, 1, fp)) {
        char c = buffer[0];

        if (f == ui.formFeed && l == ui.lineFeed && t == ui.tab && s == ui.space) {
            if (c == '!')
                return false;
            putchar(c);
            return true;
        } else if (c == '\f') {
            f++;
            l = t = s = 0;
        } else if (c == '\n') {
            l++;
            t = s = 0;
        } else if (c == '\t') {
            t++;
            s = 0;
        } else {
            s++;
        }
    }

    return false;
}

void decodeFile(char *fileName, struct UserInputNode *uin) {
    FILE *fp;

    fp = fopen(fileName, "r");
    if (NULL == fp) {
        fprintf(stderr, "Could not find %s\n", fileName);
        return;
    }

    if (NULL == uin) {
        fprintf(stderr, "No user input detected!\n");
        return;
    }

    while (NULL != uin) {
        if (!decode(fp, uin->ui)) {
            break;
        }
        fseek(fp, 0, SEEK_SET);
        uin = uin->next;
    }
    printf("\n\n");
}

struct UserInputNode *getUserInput() {
    struct UserInputNode *uip, *temp;

    // 0 18 0 0
    temp = malloc(sizeof(struct UserInputNode));
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 18;
    temp->ui.tab = 0;
    temp->ui.space = 0;
    uip = temp;

    // 0 68 0 1
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 68;
    temp->ui.tab = 0;
    temp->ui.space = 1;

    // 0 100 0 32
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 100;
    temp->ui.tab = 0;
    temp->ui.space = 32;

    // 0 114 0 45
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 114;
    temp->ui.tab = 0;
    temp->ui.space = 45;

    // 0 38 0 26
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 38;
    temp->ui.tab = 0;
    temp->ui.space = 26;

    // 0 16 0 21
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 16;
    temp->ui.tab = 0;
    temp->ui.space = 21;

    // 0 17 0 59
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 17;
    temp->ui.tab = 0;
    temp->ui.space = 59;

    // 0 11 0 29
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 11;
    temp->ui.tab = 0;
    temp->ui.space = 29;

    // 0 102 0 0
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 102;
    temp->ui.tab = 0;
    temp->ui.space = 0;

    // 0 10 0 50
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 10;
    temp->ui.tab = 0;
    temp->ui.space = 50;

    // 0 39 0 42
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 39;
    temp->ui.tab = 0;
    temp->ui.space = 42;

    // 0 33 0 50
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 33;
    temp->ui.tab = 0;
    temp->ui.space = 50;

    // 0 46 0 54
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 46;
    temp->ui.tab = 0;
    temp->ui.space = 54;

    // 0 76 0 47
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 76;
    temp->ui.tab = 0;
    temp->ui.space = 47;

    // 0 84 2 28
    temp->next = malloc(sizeof(struct UserInputNode));
    temp = temp->next;
    temp->ui.formFeed = 0;
    temp->ui.lineFeed = 84;
    temp->ui.tab = 2;
    temp->ui.space = 28;

    temp->next = NULL;
    return uip;
}

void freeUserInput(struct UserInputNode *uip) {
    if (NULL == uip) {
        return;
    }

    freeUserInput(uip->next);
    uip->next = NULL;

    free(uip);
}

int main() {
    struct UserInputNode *uip;

    uip = getUserInput();
    decodeFile("theRaven.txt", uip);
    freeUserInput(uip);

    return 0;
}
```

```txt
Silence-Dogood.
```



## C++

Text used to encode:[http://paulo-jorente.de/text/theRaven.txt The Raven - by E.A.Poe]

```cpp

#include <vector>
#include <iostream>
#include <fstream>
#include <sstream>

typedef struct {
    int s[4];
}userI;

class jit{
public:
    void decode( std::string& file, std::vector<userI>& ui ) {
        std::ifstream f( file.c_str(), std::ios_base::in );
        fileBuffer = std::string( ( std::istreambuf_iterator<char>( f ) ), std::istreambuf_iterator<char>() );
        f.close();
        for( std::vector<userI>::iterator t = ui.begin(); t != ui.end(); t++ ) {
            if( !decode( ( *t ).s ) ) break;
        }
        std::cout << "\n\n";
    }
private:
    bool decode( int* ui ) {
        int l = 0, t = 0, p = 0, c = 0, a = 0;
        for( std::string::iterator i = fileBuffer.begin(); i != fileBuffer.end(); i++ ) {
            if( p == ui[0] && l == ui[1] && t == ui[2] && c == ui[3] ) {
                if( *i == '!' )  return false;
                std::cout << *i; return true;
            }
            if( *i == '\n' )      { l++; t = c = 0; }
            else if( *i == '\t' ) { t++; c = 0; }
            else if( *i == '\f' ) { p++; l = t = c = 0; }
            else                  { c++;}
        }
        return false;
    }
    std::string fileBuffer;
};
void getUserInput( std::vector<userI>& u ) {
    std::string h = "0 18 0 0 0 68 0 1 0 100 0 32 0 114 0 45 0 38 0 26 0 16 0 21 0 17 0 59 0 11 "
                    "0 29 0 102 0 0 0 10 0 50 0 39 0 42 0 33 0 50 0 46 0 54 0 76 0 47 0 84 2 28";
    //std::getline( std::cin, h );
    std::stringstream ss( h );
    userI a;
    int x = 0;
    while( std::getline( ss, h, ' ' ) ) {
        a.s[x] = atoi( h.c_str() );
        if( ++x == 4 ) {
            u.push_back( a );
            x = 0;
        }
    }
}
int main( int argc, char* argv[] ) {
    std::vector<userI> ui;
    getUserInput( ui );

    jit j;
    j.decode( std::string( "theRaven.txt" ), ui );
    return 0;
}

```

```txt

Silence-Dogood.

```


## C#
```c#
using System;
using System.Collections.Generic;
using System.Linq;

namespace JustInTimeProcessing {
    struct UserInput {
        public UserInput(string ff, string lf, string tb, string sp) {
            FormFeed = (char)int.Parse(ff);
            LineFeed = (char)int.Parse(lf);
            Tab = (char)int.Parse(tb);
            Space = (char)int.Parse(sp);
        }

        public char FormFeed { get; }
        public char LineFeed { get; }
        public char Tab { get; }
        public char Space { get; }
    }

    class Program {
        static List<UserInput> GetUserInput() {
            string h = "0 18 0 0 0 68 0 1 0 100 0 32 0 114 0 45 0 38 0 26 0 16 0 21 0 17 0 59 0 11 "
                + "0 29 0 102 0 0 0 10 0 50 0 39 0 42 0 33 0 50 0 46 0 54 0 76 0 47 0 84 2 28";
            return h.Split(' ')
                .Select((x, idx) => new { x, idx })
                .GroupBy(x => x.idx / 4)
                //.Select(g => g.Select(a => a.x))
                .Select(g => {
                    var ge = g.Select(a => a.x).ToArray();
                    return new UserInput(ge[0], ge[1], ge[2], ge[3]);
                })
                .ToList();
        }

        static void Decode(string filename, List<UserInput> uiList) {
            string text = System.IO.File.ReadAllText(filename);

            bool Decode2(UserInput ui) {
                char f = (char)0;
                char l = (char)0;
                char t = (char)0;
                char s = (char)0;

                foreach (char c in text) {
                    if (f == ui.FormFeed && l == ui.LineFeed && t == ui.Tab && s == ui.Space) {
                        if (c == '!') return false;
                        Console.Write(c);
                        return true;
                    }
                    if (c == '\u000c') {
                        f++; l = (char)0; t = (char)0; s = (char)0;
                    } else if (c == '\n') {
                        l++; t = (char)0; s = (char)0;
                    } else if (c == '\t') {
                        t++; s = (char)0;
                    } else {
                        s++;
                    }
                }

                return false;
            }

            foreach (UserInput ui in uiList) {
                if (!Decode2(ui)) {
                    break;
                }
            }
            Console.WriteLine();
        }

        static void Main(string[] args) {
            var uiList = GetUserInput();
            Decode("theraven.txt", uiList);

            Console.ReadLine();
        }
    }
}
```

```txt
Silence-Dogood.
```



## D

```D
import std.algorithm;
import std.array;
import std.conv;
import std.file;
import std.range;
import std.stdio;

struct UserInput {
    char formFeed;
    char lineFeed;
    char tab;
    char space;

    this(string ff, string lf, string tb, string sp) {
        formFeed = cast(char) ff.to!int;
        lineFeed = cast(char) lf.to!int;
        tab = cast(char) tb.to!int;
        space = cast(char) sp.to!int;
    }
}

auto getUserInput() {
    auto h = "0 18 0 0 0 68 0 1 0 100 0 32 0 114 0 45 0 38 0 26 0 16 0 21 0 17 0 59 0 11 "
           ~ "0 29 0 102 0 0 0 10 0 50 0 39 0 42 0 33 0 50 0 46 0 54 0 76 0 47 0 84 2 28";
    auto ctor = (string[] a) => UserInput(a[0], a[1], a[2], a[3]);
    return h.split(' ').chunks(4).map!ctor.array;
}

void decode(string fileName, UserInput[] uiList) {
    auto text = readText(fileName);

    bool decode2(UserInput ui) {
        char f = 0;
        char l = 0;
        char t = 0;
        char s = 0;
        foreach (c; text) {
            if (f == ui.formFeed && l == ui.lineFeed && t == ui.tab && s == ui.space) {
                if (c == '!') return false;
                write(c);
                return true;
            }
            if (c == '\u000c') {
                f++; l=0; t=0; s=0;
            } else if (c == '\n') {
                l++; t=0; s=0;
            } else if (c == '\t') {
                t++; s=0;
            } else {
                s++;
            }
        }
        return false;
    }

    foreach (ui; uiList) {
        if (!decode2(ui)) break;
    }
    writeln;
}

void main() {
    auto uiList = getUserInput();
    decode("theRaven.txt", uiList);
}
```

```txt
Silence-Dogood.
```



## Go

```go
package main

import (
    "fmt"
    "io/ioutil"
    "log"
    "strconv"
    "strings"
)

type userInput struct{ formFeed, lineFeed, tab, space int }

func getUserInput() []userInput {
    h := "0 18 0 0 0 68 0 1 0 100 0 32 0 114 0 45 0 38 0 26 0 16 0 21 0 17 0 59 0 11 " +
        "0 29 0 102 0 0 0 10 0 50 0 39 0 42 0 33 0 50 0 46 0 54 0 76 0 47 0 84 2 28"
    flds := strings.Fields(h)
    var uis []userInput
    var uif [4]int
    for i := 0; i < len(flds); i += 4 {
        for j := 0; j < 4; j++ {
            uif[j], _ = strconv.Atoi(flds[i+j])
        }
        uis = append(uis, userInput{uif[0], uif[1], uif[2], uif[3]})
    }
    return uis
}

func decode(fileName string, uis []userInput) error {
    text, err := ioutil.ReadFile(fileName)
    if err != nil {
        return err
    }

    decode2 := func(ui userInput) bool {
        var f, l, t, s int
        for _, c := range text {
            if f == ui.formFeed && l == ui.lineFeed && t == ui.tab && s == ui.space {
                if c == '!' {
                    return false
                }
                fmt.Printf("%c", c)
                return true
            }
            switch c {
            case '\f':
                f++
                l = 0
                t = 0
                s = 0
            case '\n':
                l++
                t = 0
                s = 0
            case '\t':
                t++
                s = 0
            default:
                s++
            }
        }
        return true
    }

    for _, ui := range uis {
        if !decode2(ui) {
            break
        }
    }
    fmt.Println()
    return nil
}

func main() {
    uis := getUserInput()
    err := decode("theRaven.txt", uis)
    if err != nil {
        log.Fatal(err)
    }
}
```


```txt

Silence-Dogood.

```



## Java

```java
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.function.Predicate;

public class Processing {
    private static class UserInput {
        private char formFeed;
        private char lineFeed;
        private char tab;
        private char space;

        private UserInput(char formFeed, char lineFeed, char tab, char space) {
            this.formFeed = formFeed;
            this.lineFeed = lineFeed;
            this.tab = tab;
            this.space = space;
        }

        char getFormFeed() {
            return formFeed;
        }

        char getLineFeed() {
            return lineFeed;
        }

        char getTab() {
            return tab;
        }

        char getSpace() {
            return space;
        }
    }

    private static List<UserInput> getUserInput() {
        String h = "0 18 0 0 0 68 0 1 0 100 0 32 0 114 0 45 0 38 0 26 0 16 0 21 0 17 0 59 0 11 " +
            "0 29 0 102 0 0 0 10 0 50 0 39 0 42 0 33 0 50 0 46 0 54 0 76 0 47 0 84 2 28";
        String[] s = h.split(" ");

        List<UserInput> uiList = new ArrayList<>();
        for (int idx = 0; idx + 3 < s.length; idx += 4) {
            char c0 = (char) Integer.parseInt(s[idx + 0]);
            char c1 = (char) Integer.parseInt(s[idx + 1]);
            char c2 = (char) Integer.parseInt(s[idx + 2]);
            char c3 = (char) Integer.parseInt(s[idx + 3]);

            UserInput userInput = new UserInput(c0, c1, c2, c3);
            uiList.add(userInput);
        }
        return uiList;
    }

    private static void decode(String fileName, List<UserInput> uiList) throws IOException {
        Path path = Paths.get(fileName);
        byte[] bytes = Files.readAllBytes(path);
        String text = new String(bytes, StandardCharsets.UTF_8);

        Predicate<UserInput> decode2 = (UserInput ui) -> {
            char f = 0;
            char l = 0;
            char t = 0;
            char s = 0;
            char ff = ui.getFormFeed();
            char lf = ui.getLineFeed();
            char tb = ui.getTab();
            char sp = ui.getSpace();

            for (char c : text.toCharArray()) {
                if (f == ff && l == lf && t == tb && s == sp) {
                    if (c == '!') {
                        return false;
                    }
                    System.out.print(c);
                    return true;
                }
                switch (c) {
                    case '\u000c':
                        f++;
                        l = 0;
                        t = 0;
                        s = 0;
                        break;
                    case '\n':
                        l++;
                        t = 0;
                        s = 0;
                        break;
                    case '\t':
                        t++;
                        s = 0;
                        break;
                    default:
                        s++;
                        break;
                }
            }

            return false;
        };

        for (UserInput ui : uiList) {
            if (!decode2.test(ui)) {
                break;
            }
        }
        System.out.println();
    }

    public static void main(String[] args) throws IOException {
        List<UserInput> uiList = getUserInput();
        decode("theRaven.txt", uiList);
    }
}
```

```txt
Silence-Dogood.
```



## Julia

Customization is via adding to or deleting from the chars dictionary.

```julia
@enum streamstate GET_FF GET_LF GET_TAB GET_CHAR ABORT
chars = Dict(GET_FF => ['\f'], GET_LF => ['\n'], GET_TAB => ['\t'])

function stream_decode_jit(iostream)
    msg, state, ffcount, lfcount, tabcount, charcount = "", GET_FF, 0, 0, 0, 0
    while true
        if state == ABORT || eof(iostream)
            return msg
        end
        ch = read(iostream, Char)
        if state == GET_FF && ch in chars[GET_FF]
            ffcount += 1
            state = GET_LF
            lfcount = 0
        elseif state == GET_LF && ch in chars[GET_LF]
            if (lfcount += 1) == ffcount
                state = GET_TAB
                tabcount = 0
            end
        elseif state == GET_TAB && ch in chars[GET_TAB]
            if (tabcount += 1) == ffcount
                state = GET_CHAR
                charcount = 0
            end
        elseif state == GET_CHAR
            if (charcount += 1) == ffcount
                print(ch)
                msg *= ch
                if ch == '!'
                    state = ABORT
                else
                    state = GET_FF
                end
            end
        end
    end
end

stream_decode_jit(open("filename.txt", "r"))

```



## Kotlin

```scala
// version 1.2.10

import java.io.File

data class UserInput(val formFeed: Int, val lineFeed: Int, val tab: Int, val space: Int)

fun getUserInput(): List<UserInput> {
    val h = "0 18 0 0 0 68 0 1 0 100 0 32 0 114 0 45 0 38 0 26 0 16 0 21 0 17 0 59 0 11 " +
            "0 29 0 102 0 0 0 10 0 50 0 39 0 42 0 33 0 50 0 46 0 54 0 76 0 47 0 84 2 28"
    return h.split(' ').chunked(4).map {
        val (ff, lf, tb, sp) = it
        UserInput(ff.toInt(), lf.toInt(), tb.toInt(), sp.toInt())
    }
}

fun decode(fileName: String, uiList: List<UserInput>) {
    val text = File(fileName).readText()

    fun decode2(ui: UserInput): Boolean {
        var f = 0
        var l = 0
        var t = 0
        var s = 0
        val (ff, lf, tb, sp) = ui
        for (c in text) {
            if (f == ff && l == lf && t == tb && s == sp) {
                if (c == '!') return false
                print(c)
                return true
            }
            when (c) {
                '\u000c' -> { f++; l = 0; t = 0; s = 0 }
                '\n'     -> { l++; t = 0; s = 0 }
                '\t'     -> { t++; s = 0 }
                else     -> { s++ }
            }
        }
        return false
    }

    for (ui in uiList) {
        if (!decode2(ui)) break
    }
    println()
}

fun main(args: Array<String>) {
    val uiList = getUserInput()
    decode("theRaven.txt", uiList)
}
```


```txt

Silence-Dogood.

```



## Perl

```perl
use strict;
use warnings;
use feature 'say';
use feature 'state';

use ntheory qw/fromdigits todigitstring/;

my $key = 'perl5';
srand fromdigits($key,36) % 2**63;

my @stream;

sub stream {
    my($i) = shift;
    state @chars;
    push @chars, chr($_) for 14..127;
    $stream[$i] = $chars[rand 1+127-14] unless $stream[$i];
}

sub jit_encode {
    my($str) = shift;
    my $i = 0;
    my $last = 0;
    my $enc = '';
    for my $c (split '', $str) {
        my $h;
        my $l = '';
        ++$i until $c eq stream($i);
        my $o = $i - $last;
        $l    = $o % 26;
        $h    = $o - $l if $o > 26;
        $l   += 10;
        $enc .= ($h ? uc todigitstring($h,36) : '') . lc todigitstring($l,36);
        $last = $i;
    }
    $enc
}

sub jit_decode {
    my($str) = shift;
    my @matches = $str =~ /((.*?) ([a-z]))/gx;
    my $dec = '';
    my $i = 0;
    for my $j (0 .. @matches/3 - 1) {
        my $o = ( fromdigits($matches[3*$j+1],36) - 10 // 0) +
                ( fromdigits($matches[3*$j+2],36)      // 0);
        $i   += $o;
        $dec .= $stream[$i];
    }
    $dec
}

my $enc = jit_encode('The slithey toves did gyre and gimble in the wabe');
say my $result = "Encoded\n$enc\n\nDecoded\n" . jit_decode($enc);
```

```txt
Encoded
bQh52yf9Ex2Wi4Cv1GcyoAUcBKbke3Mo5Ss2WybQlvQs1GviQc1GnQg5Sv26j1Gm26m3Mp26iQz78lQx2Wel1Gcs52y5Sds3Me26l52au1Gwe1Gl

Decoded
The slithey toves did gyre and gimble in the wabe
```



## Perl 6

This is a something of a toy encoder / decoder and probably shouldn't be used for anything serious.

Default encode/decode key is 'Perl 6' Feed it a pass phrase at the command line to use that instead.

Will handle any visible character in the ASCII range as well as space and new-line.


```perl6
#`[
Set srand to set the encode / decode "key".
Need to use the same "key" and same implementation
of Perl 6 to encode / decode. Gain "security" by
exchanging "keys" by a second channel. Default
"key" is "Perl 6"
]

unit sub MAIN ($key = 'Perl 6');

srand $key.comb(/<.alnum>/).join.parse-base(36) % 2**63;

my @stream = (flat "\n", ' ' .. '~').roll(*);

sub jit-encode (Str $str) {
    my $i = 0;
    my $last = 0;
    my $enc = '';
    for $str.comb -> $c {
        my $h;
        my $l = '';
        ++$i until $i > 1 && $c eq @stream[$i];
        my $o = $i - $last;
        $l    = $o % 26;
        $h    = $o - $l if $o >= 26;
        $l   += 10;
        $enc ~= ($h ?? $h.base(36).uc !! '') ~ ($l.base(36).lc);
        $last = $i;
    }
    my $block = 60;
    $enc.comb($block).join: "\n"
}

sub jit-decode (Str $str is copy) {
    $str.=subst("\n", '', :g);
    $str ~~ m:g/((.*?) (<:Ll>))/;
    my $dec = '';
    my $i = 0;
    for $/.List -> $l {
        my $o = ($l[0][1].Str.parse-base(36) - 10 // 0) +
                ($l[0][0].Str.parse-base(36) // 0);
        $i += $o;
        $dec ~= @stream[$i];
    }
    $dec
}

my $secret = q:to/END/;
In my opinion, this task is pretty silly.

'Twas brillig, and the slithy toves
    Did gyre and gimble in the wabe.

!@#$%^&*()_+}{[].,><\|/?'";:1234567890
END

say "== Secret: ==\n$secret";

say "\n== Encoded: ==";
say my $enc = jit-encode($secret);

say "\n== Decoded: ==";
say jit-decode($enc);
```



```txt
== Secret: ==
In my opinion, this task is pretty silly.

'Twas brillig, and the slithy toves
    Did gyre and gimble in the wabe.

!@#$%^&*()_+}{[].,><\|/?'";:1234567890


== Encoded: ==
26j52d6Ie1Ge4Cd26po1GdsQa3Ms52piQd4Cn3Md2Wcf1GtciQz1GwQb5Si2
WnQlQmQjQv1GmQra2Wt4Cpc1Gysatu26va1Gq52x4Cp1Gv4CeQv1Gb1Gp4Co
6IbaQyAUmd26a7Yw3Mh2Wu26v1GfQsQwbQpn26z1Gi1Gl1GmQb1Gfs26v4Ce
Qy2Wm78xaaa4Cj26x6If3Msqu2Wx2Wku1Gh52ydQh3Mb78rll1GvcQap5Sgy
Qm26s1Gh26clj1Gm1GzA4y26bat1Gdk1Gs1Gpm1GlQs7Ys52dQw1Giv5Se5S
u3Mb1Gucn4Cq26h26pQq2Wh5Sh7Yi8OrpQl26p26i3MqtQiQhQi4Ckb3Mi

== Decoded: ==
In my opinion, this task is pretty silly.

'Twas brillig, and the slithy toves
    Did gyre and gimble in the wabe.

!@#$%^&*()_+}{[].,><\|/?'";:1234567890
```



## Phix

```Phix
-- demo/rosetta/BookCipher.exw
function decode(integer fn, sequence ui)
    integer {ff,lf,tab,sp} = ui,
             f = 0, l = 0, t = 0, s = 0
    while true do
        integer c = getc(fn)
        if c=-1 then return false end if
        if f==ff and l==lf and t==tab and s==sp then
            if c=='!' then return false end if
            puts(1,c)
            return true
        elsif c==#0C then
            f += 1
            {l, t, s} @= 0
        elsif c=='\n' then
            l += 1
            {t, s} @= 0
        elsif c=='\t' then
            t += 1
            s = 0
        else
            s += 1
        end if
    end while
    return false
end function

include builtins\libcurl.e

procedure decodeFile(string filename, url, sequence code)
    if not file_exists(filename) then
        printf(1,"Downloading %s...\n",{filename})
        CURLcode res = curl_easy_get_file(url,"",filename) -- (no proxy)
        if res!=CURLE_OK then
            string error = sprintf("%d",res)
            if res=CURLE_COULDNT_RESOLVE_HOST then
                error &= " [CURLE_COULDNT_RESOLVE_HOST]"
            end if
            crash("Error %s downloading file\n", {error})
        end if
    end if
    integer fn = open(filename, "r")
    if fn=-1 then crash("could not open %s",{filename}) end if
    for i=1 to length(code) do
        if not decode(fn,code[i]) then exit end if
        if seek(fn, 0)!=SEEK_OK then crash("seek error") end if
    end for
    printf(1,"\n\n");
end procedure

constant code = {{0,18,0,0},
                 {0,68,0,1},
                 {0,100,0,32},
                 {0,114,0,45},
                 {0,38,0,26},
                 {0,16,0,21},
                 {0,17,0,59},
                 {0,11,0,29},
                 {0,102,0,0},
                 {0,10,0,50},
                 {0,39,0,42},
                 {0,33,0,50},
                 {0,46,0,54},
                 {0,76,0,47},
                 {0,84,2,28}}
decodeFile("theRaven.txt", "http://paulo-jorente.de/text/theRaven.txt", code)
{} = wait_key()
```

```txt

Silence-Dogood.

```



## Python

```python
import sys

class UserInput:
    def __init__(self,chunk):
        self.formFeed = int(chunk[0])
        self.lineFeed = int(chunk[1])
        self.tab = int(chunk[2])
        self.space = int(chunk[3])

    def __str__(self):
        return "(ff=%d; lf=%d; tb=%d; sp%d)" % (self.formFeed,self.lineFeed,self.tab,self.space)

def chunks(l,n):
    for i in xrange(0, len(l), n):
        yield l[i:i+n]

def getUserInput():
    h = "0 18 0 0 0 68 0 1 0 100 0 32 0 114 0 45 0 38 0 26 0 16 0 21 0 17 0 59 0 11 "\
        "0 29 0 102 0 0 0 10 0 50 0 39 0 42 0 33 0 50 0 46 0 54 0 76 0 47 0 84 2 28"
    ha = h.split()
    return [UserInput(chunk) for chunk in chunks(ha, 4)]

def decode(filename,uiList):
    f = open(filename, "r")
    text = f.read()

    def decode2(ui):
        f = 0
        l = 0
        t = 0
        s = 0
        for c in text:
            if f == ui.formFeed and l == ui.lineFeed and t == ui.tab and s == ui.space:
                if c == '!':
                    return False
                sys.stdout.write(c)
                return True
            if c == '\u000c':
                f=f+1
                l=0
                t=0
                s=0
            elif c == '\n':
                l=l+1
                t=0
                s=0
            elif c == '\t':
                t=t+1
                s=0
            else:
                s=s+1
        return False

    for ui in uiList:
        if not decode2(ui):
            break
    print

##### Main #####

uiList = getUserInput()
decode("theRaven.txt", uiList)
```

```txt
Silence-Dogood.
```



## Racket


```racket
#lang racket

(define user-input
  (~a "0 18 0 0 0 68 0 1 0 100 0 32 0 114 0 45 0 38 0 26 0 16 0 21 0 17 0 59 0 11 "
      "0 29 0 102 0 0 0 10 0 50 0 39 0 42 0 33 0 50 0 46 0 54 0 76 0 47 0 84 2 28"))

(define content (file->string "theRaven.txt"))

(define (decode slice)
  (match-define (list ff lf tb sp) slice)
  (let loop ([f 0] [l 0] [t 0] [s 0] [xs (string->list content)])
    (define next (curryr loop (rest xs)))
    (match (first xs)
      [c #:when (and (= f ff) (= l lf) (= t tb) (= s sp)) c]
      [#\u000c   (next (add1 f) 0 0 0)]
      [#\newline (next f (add1 l) 0 0)]
      [#\tab     (next f l (add1 t) 0)]
      [_         (next f l t (add1 s))])))

(for ([slice (in-slice 4 (map string->number (string-split user-input)))])
  (define c (decode slice))
  #:break (char=? #\! c)
  (display c))
```


http://paulo-jorente.de/text/theRaven.txt

```txt

Silence-Dogood.

```



## REXX

The input file used by this REXX program only contains one page;   it has no   ''FF''   (''formfeed'')   characters in it),

and the injection of   ''FF''   characters into the file would be like putting pencil marks into a holy book.   <big><big><big> ☺ </big></big></big>

```rexx
/*REXX program  extracts characters  by using a  book cipher  (that is a  text file).   */
parse arg iFID .                                 /*obtain optional name of file  (book).*/
if iFID=='' | iFID==","  then iFID= 'JIT.TXT'    /*Not specified?  Then use the default.*/
$= 'abcdefghijklmnopqrstuvwxyz';  _=$;  upper _;  $= "0123456789"$ || _;  $$=$ || xrange()
prompt= '────────── enter four positive integers         or        Quit'
pag=1;    lin=1;    FF= 'c'x                     /*assume start of  page 1,  line 1.    */
@.=                                              /*read the entire book from the file.  */
    do  while  lines(iFID)\==0                   /*process lines from input stream(file)*/
    _= translate( linein(iFID), , '9'x)          /*obtain a single line from input file.*/
    if pos(FF, _)\==0  then do; pag=pag+1; lin=1 /*bump page counter; reset line counter*/
                            end                  /* [↑]  handle finding of FF (formfeed)*/
    @.pag.lin= _                                 /*obtain a single line from input file.*/
          lin= lin + 1                           /*bump the line counter.               */
    end   /*while*/                              /* [↑]  read the entire input stream.  */
?=                                               /*define the phrase to be null (so far)*/
       do ask=0;       say prompt;       pull y  /*get just─in─time positional numbers. */
       if abbrev('QUIT', y, 1)  then exit 0      /*the user wants out of here, so exit. */
       y=space( translate(y, $, $$) )            /*allow any separator the user wants.  */
       parse var  y    a.1   a.2   a.3   a.4     /*parse the pertinent parameters.      */
       if words(y)>4   then do;    say 'too many parameters entered.'
                                   iterate  ask
                            end                  /*go and try again to obtain the parms.*/
         do k=1  for 4;  is#= datatype(a.k, 'W') /*validate  parms  {positive integers}.*/
         if is#  then a.k= a.k / 1               /*normalize the number (for indexing). */
         if is#  &  a.k>0  then iterate          /*found a good parameter?   Then skip. */
         say 'parameter '      k      " is missing or not a positive integer: "      a.k
         iterate  ask                            /*go and ask for another set of parms. */
         end   /*k*/                             /* [↑]  done with the validations.     */
       parse value a.1 a.2 a.3 a.4  with p L w c /*parse parameters for specific names. */
       x=substr( word( @.p.L, w), c,  1)         /*extract a character from the book.   */
       if x=='!'  then leave                     /*if the  stop  char was found,  done. */
       say right(x  '◄─── a letter', 46)         /*might as well echo char to terminal. */
       ?= ? || x                                 /*append the character to the phrase.  */
       end   /*j*/                               /* [↑]  display letters found in book. */
say '═════►'   ?                                 /*stick a fork in it,  we're all done. */
```

```txt

  1,   133,   4,   5
  1,    34,   9,   3
  1,  1377,   2,   2
  1,     4,   8,   4
  1,   265,   3,   5
  1,   413,  10,   2
  1,    10,  12,   1
  1,   144,  10,  10
  1,   571,   4,  12

```

```txt

──────────enter four parameters  (all positive integers)   or   QUIT
  1,   133,   4,   5                            ◄■■■■■■■■■ user input (first reponse).
                               s ◄─── a letter

    ∙
    ∙                                           (some prompts and reponses elided.)
    ∙

──────────enter four parameters  (all positive integers)   or   QUIT
  1,   571,   4,  12                            ◄■■■■■■■■■ user input (ninth reponse).
═════► so─true.

```

The input file used (the IBM jargon file) in the above REXX program can be found here ───►   [[Just_in_time_processing_on_a_character_stream/REXX/JIT.TXT]].




## Tcl

```tcl
package require Tcl 8.6

oo::class create JustInTimeStreamExtract {
    variable map counter counters last
    constructor {{pageSeparator "\f"} {lineSeparator "\n"} {fieldSeparator "\t"}} {
	dict set map $pageSeparator NextPage
	dict set map $lineSeparator NextLine
	dict set map $fieldSeparator NextField
	set counter 1
	array set counters {page 0 line 0 field 0 char 0}
	set last ""
    }

    method emit {char} {
	puts -nonewline $char
	set last $char
    }
    method finished {} {
	if {$last ne "\n"} {
	    puts ""
	}
    }

    method stream {{channel stdin} {length 1}} {
	try {
	    while 1 {
		set str [read $channel $length]
		if {[eof $channel]} break
		foreach c [split $str ""] {
		    if {[dict exists $map $c]} {
			my [dict get $map $c]
		    } else {
			my NextChar $c
		    }
		}
	    }
	} trap STOP {} {
	    # Do nothing
	}
	my finished
    }

    method NextPage {} {
	incr counters(page)
	array set counters {line 0 field 0 char 0}
    }
    method NextLine {} {
	incr counters(line)
	array set counters {field 0 char 0}
    }
    method NextField {} {
	incr counters(field)
	set counters(char) 0
    }
    method NextChar {char} {
	incr counters(char)
	if {[my PrintThisOne?]} {
	    if {$char eq "!"} {
		throw STOP "stop character found"
	    }
	    incr counter
	    my emit $char
	    array set counters {page 0 line 0 field 0 char 0}
	}
    }

    method PrintThisOne? {} {
	tcl::mathop::== $counter $counters(page) $counters(line) $counters(field) $counters(char)
    }
}
```

Demonstration of use:

```tcl
[JustInTimeStreamExtract new] stream [open "sample.txt"]
```

<!-- no output; I'll wait for someone else to invent something to decode… -->


## zkl

```zkl
class FlyBy{
   fcn decode(file,tuplets){
      codePad:=File(file).read().mode(String); // blob of text
      tuplets.pump(Console.print, _decode.fp1(codePad));
      println();
    }
    fcn [private] _decode(idx,codePad){ // idx is (ff,lf,tab,chr) offset info codePad
       z:=-1;
       foreach n,c in (idx.zip(T("\f","\n","\t"))){
          do(n){ if(Void==(z=codePad.find(c,z+1))) return(Void.Stop); }
       }
       if(z==-1) z=0;  // (0,0,0,n)
       try{ return(codePad[z + idx[-1] + 1]) }catch{ return(Void.Stop) }
    }
}

fcn getUserInput{
   // I don't know a user would enter this but we have
   // a string of 4 item tuplets : (formfeeds, linefeeds, tabs, characters), ...
   // each tuplet is an offset into a code pad (text)
   h:="0 18 0 0 0 68 0 1 0 100 0 32 0 114 0 45 0 38 0 26 0 16 0 21 0 17 0 59 0 11 "
      "0 29 0 102 0 0 0 10 0 50 0 39 0 42 0 33 0 50 0 46 0 54 0 76 0 47 0 84 2 28";
   h.split(" ").pump(List,T(Void.Read,3),
      fcn(ff,lf,t,s){ vm.arglist.apply("toInt") });
}
```


```zkl
input:=getUserInput();
   // our code pad is: http://paulo-jorente.de/text/theRaven.txt
FlyBy.decode("theRaven.txt",input);
```

```txt

Silence-Dogood.

```

