+++
title = "Checksumcolor"
description = ""
date = 2019-08-23T07:11:17Z
aliases = []
[extra]
id = 22287
[taxonomies]
categories = ["task"]
tags = []
+++

'''Context:''' In December 2013 [https://lists.gnu.org/archive/html/coreutils/2013-12/msg00152.html a patch was proposed to the coreutils list] to add a --color option to the commands md5sum and shaXsum to display the checksums in color to make it easier to visually identify similarities in a list of printed checksums. The patch was not accepted for inclusion and instead it was suggested to create a command line utility which can be used to pipe the output of the md5sum and shaXsum commands similar to the utility colordiff.

'''Task:''' The task is to create this command line utility that we can use to pipe the output of the md5sum and shaXsum commands and that colors the checksum part of the output. Take each group of 3 or 6 hexadecimal characters and interpret it as if it was a color code and print it with the closest console color. Print with colors if the output is the terminal or print the input unchanged if the output of the utility is a pipe.

'''Example:'''

 $ md5sum coreutils-* | checksumcolor
 <span style="color:#AA0">ab2</span><span style="color:#0AA">0d8</span><span style="color:#00A">40e13a</span><span style="color:#0AA">dfe</span><span style="color:#AA0">bf2</span><span style="color:#A0A">b69</span><span style="color:#00A">36a</span><span style="color:#0AA">2da</span><span style="color:#A00">b07</span>1b  coreutils-8.29.tar.gz
 <span style="color:#A00">b25</span><span style="color:#AA0">9b2</span><span style="color:#A00">936</span><span style="color:#AA0">bb4</span><span style="color:#A00">600</span><span style="color:#0AA">9be</span><span style="color:#0A0">3f5</span><span style="color:#AA0">cc0</span><span style="color:#0A0">6a12c3</span>2d  coreutils-8.30.tar.gz
 <span style="color:#00A">03c</span><span style="color:#A00">f26420</span><span style="color:#AA0">de5</span><span style="color:#00A">66c306</span><span style="color:#A00">d34</span><span style="color:#0AA">0df</span><span style="color:#00A">52f</span><span style="color:#0AA">6cc</span>d7  coreutils-8.31.tar.gz



## Go

```go
package main

import (
    "bufio"
    "fmt"
    "golang.org/x/crypto/ssh/terminal"
    "log"
    "os"
    "regexp"
    "strconv"
)

type Color struct{ r, g, b int }

type ColorEx struct {
    color Color
    code  string
}

var colors = []ColorEx{
    {Color{15, 0, 0}, "31"},
    {Color{0, 15, 0}, "32"},
    {Color{15, 15, 0}, "33"},
    {Color{0, 0, 15}, "34"},
    {Color{15, 0, 15}, "35"},
    {Color{0, 15, 15}, "36"},
}

func squareDist(c1, c2 Color) int {
    xd := c2.r - c1.r
    yd := c2.g - c1.g
    zd := c2.b - c1.b
    return xd*xd + yd*yd + zd*zd
}

func printColor(s string) {
    n := len(s)
    k := 0
    for i := 0; i < n/3; i++ {
        j := i * 3
        c1 := s[j]
        c2 := s[j+1]
        c3 := s[j+2]
        k = j + 3
        r, err := strconv.ParseInt(fmt.Sprintf("0x%c", c1), 0, 64)
        check(err)
        g, err := strconv.ParseInt(fmt.Sprintf("0x%c", c2), 0, 64)
        check(err)
        b, err := strconv.ParseInt(fmt.Sprintf("0x%c", c3), 0, 64)
        check(err)
        rgb := Color{int(r), int(g), int(b)}
        m := 676
        colorCode := ""
        for _, cex := range colors {
            sqd := squareDist(cex.color, rgb)
            if sqd < m {
                colorCode = cex.code
                m = sqd
            }
        }
        fmt.Printf("\033[%s;1m%c%c%c\033[00m", colorCode, c1, c2, c3)
    }
    for j := k; j < n; j++ {
        c := s[j]
        fmt.Printf("\033[0;1m%c\033[00m", c)
    }
}

var (
    r       = regexp.MustCompile("^([A-Fa-f0-9]+)([ \t]+.+)$")
    scanner = bufio.NewScanner(os.Stdin)
    err     error
)

func colorChecksum() {
    for scanner.Scan() {
        line := scanner.Text()
        if r.MatchString(line) {
            submatches := r.FindStringSubmatch(line)
            s1 := submatches[1]
            s2 := submatches[2]
            printColor(s1)
            fmt.Println(s2)
        } else {
            fmt.Println(line)
        }
    }
    check(scanner.Err())
}

func cat() {
    for scanner.Scan() {
        line := scanner.Text()
        fmt.Println(line)
    }
    check(scanner.Err())
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    if terminal.IsTerminal(int(os.Stdout.Fd())) {
        colorChecksum()
    } else {
        cat()
    }
}
```


```txt

Same as OCaml entry.

```


## Julia

Usage: md5sum *.* | julia thisfile.jl

```julia
while(occursin(r"^[\d\w]{32}", (begin s = readline() end)))
    (crc, restofline) = split(s, " ", limit=2)
    for i in 1:3:length(crc)-3
        print("\e[38;2", join([";$(16 * parse(Int, string(c), base=16))" 
            for c in crc[i:i+2]], ""), "m", crc[i:i+2])
    end
    println("\e[0m", crc[end-1:end], " ", restofline)
end

```
Same as the OCaml entry.


## OCaml



```ocaml
#load "unix.cma"
#load "str.cma"

let colors = [|
  ((15,  0,  0), "31");
  (( 0, 15,  0), "32");
  ((15, 15,  0), "33");
  (( 0,  0, 15), "34");
  ((15,  0, 15), "35");
  (( 0, 15, 15), "36");
|]

let square_dist (r1, g1, b1) (r2, g2, b2) =
  let xd = r2 - r1 in
  let yd = g2 - g1 in
  let zd = b2 - b1 in
  (xd * xd + yd * yd + zd * zd)

let print_color s =
  let n = String.length s in
  let k = ref 0 in
  for i = 0 to pred (n / 3) do
    let j = i * 3 in
    let c1 = s.[j]
    and c2 = s.[j+1]
    and c3 = s.[j+2] in
    k := j+3;
    let rgb =
      int_of_string (Printf.sprintf "0x%c" c1),
      int_of_string (Printf.sprintf "0x%c" c2),
      int_of_string (Printf.sprintf "0x%c" c3)
    in
    let m = ref 676 in
    let color_code = ref "" in
    Array.iter (fun (color, code) ->
      let sqd = square_dist color rgb in
      if sqd < !m then begin
        color_code := code;
        m := sqd;
      end
    ) colors;
    Printf.printf "\027[%s;1m%c%c%c\027[00m" !color_code c1 c2 c3;
  done;
  for j = !k to pred n do
    let c = s.[j] in
    Printf.printf "\027[0;1m%c\027[00m" c;
  done

let r = Str.regexp "^\\([A-Fa-f0-9]+\\)\\([ \t]+.+\\)$"

let color_checksum () =
  try while true do
    let line = input_line stdin in
    if Str.string_match r line 0
    then begin
      let s1 = Str.matched_group 1 line in
      let s2 = Str.matched_group 2 line in
      print_color s1;
      print_endline s2;
    end
    else print_endline line
  done with End_of_file -> ()

let cat () =
  try while true do
    let line = input_line stdin in
    print_endline line
  done with End_of_file -> ()

let () =
  if Unix.isatty Unix.stdout
  then color_checksum ()
  else cat ()
```


 $ md5sum coreutils-* | ocaml checksumcolor.ml
 <span style="color:#AA0">ab2</span><span style="color:#0AA">0d8</span><span style="color:#00A">40e13a</span><span style="color:#0AA">dfe</span><span style="color:#AA0">bf2</span><span style="color:#A0A">b69</span><span style="color:#00A">36a</span><span style="color:#0AA">2da</span><span style="color:#A00">b07</span>1b  coreutils-8.29.tar.gz
 <span style="color:#A00">b25</span><span style="color:#AA0">9b2</span><span style="color:#A00">936</span><span style="color:#AA0">bb4</span><span style="color:#A00">600</span><span style="color:#0AA">9be</span><span style="color:#0A0">3f5</span><span style="color:#AA0">cc0</span><span style="color:#0A0">6a12c3</span>2d  coreutils-8.30.tar.gz
 <span style="color:#00A">03c</span><span style="color:#A00">f26420</span><span style="color:#AA0">de5</span><span style="color:#00A">66c306</span><span style="color:#A00">d34</span><span style="color:#0AA">0df</span><span style="color:#00A">52f</span><span style="color:#0AA">6cc</span>d7  coreutils-8.31.tar.gz


## Perl

```perl
use strict;
use warnings;
use Term::ANSIColor qw<colored :constants256>;

while (<>) {
    my($cs,$fn) = /(^\S+)\s+(.*)/;
    print colored($_, 'ansi' . hex $_) for $cs =~ /(..)/g;
    print " $fn\n";
}
```

<font face='fixedsys, lucida console, terminal, vga, monospace' style='line-height: 1; letter-spacing: 0; font-size: 12pt'>
<span style="color:#d7af00;">b2</span><span style="color:#d7af5f;">b3</span><span style="color:#d7ff87;">c0</span><span style="color:#808080;">f4</span><span style="color:#af00ff;">81</span><span style="color:#0000ff;">15</span><span style="color:#afd7d7;">98</span><span style="color:#875f00;">5e</span><span style="filter: contrast(70%) brightness(190%);color:blue;">0c</span><span style="color:#af8787;">8a</span><span style="color:#5f8700;">40</span><span style="color:#8787d7;">68</span><span style="color:#af5f5f;">83</span><span style="color:#ffaf5f;">d7</span><span style="color:#c0c0c0;">fa</span><span style="color:#ff005f;">c5</span> ref/test/not-in-kansas.txt

<span style="color:#5fffff;">57</span><span style="color:#0a0a0a;">e9</span><span style="color:#8787ff;">69</span><span style="color:#ffd75f;">dd</span><span style="color:#8787af;">67</span><span style="color:#afd7af;">97</span><span style="color:#ff5f5f;">cb</span><span style="color:#8787ff;">69</span><span style="color:#af8787;">8a</span><span style="color:#d7af87;">b4</span><span style="color:#5f87d7;">44</span><span style="color:#8787d7;">68</span><span style="color:#ffafff;">db</span><span style="color:#6a6a6a;">f2</span><span style="color:#d7afff;">b7</span><span style="color:#d700d7;">a4</span> ref/test/reverse_words.txt

<span style="color:#afaf87;">90</span><span style="color:#00ffaf;">31</span><span style="color:#ff5fff;">cf</span><span style="filter: contrast(70%) brightness(190%);color:green;">0a</span><span style="color:#ff00af;">c7</span><span style="color:#f5f5f5;">ff</span><span style="color:#d75faf;">a9</span><span style="color:#87af87;">6c</span><span style="color:#ff87ff;">d5</span><span style="color:#d787d7;">b0</span><span style="color:#5fafd7;">4a</span><span style="color:#d75f87;">a8</span><span style="color:purple;">05</span><span style="color:#5f87af;">43</span><span style="color:#d70087;">a2</span><span style="color:#8787d7;">68</span> ref/test/sample.txt

</font>


## Perl 6

To determine the colors, rather than breaking the md5sum into groups of 3 characters, (which leaves two lonely characters at the end), I elected to replicate the first 5 characters onto the end, then for each character, used it and the 5 characters following as a true-color index. I also added an option to output as HTML code for ease of pasting in here. 


```perl6
unit sub MAIN ($mode = 'ANSI');

if $*OUT.t or $mode eq 'HTML' { # if OUT is a terminal or if in HTML $module

    say '<div style="background-color:black; font-size:125%; font-family: Monaco, monospace;">'
      if $mode eq 'HTML';

    while my $line = get() {
        my $cs  = $line.words[0];
        my $css = $cs ~ $cs.substr(0,5);
        given $mode {
            when 'ANSI' {
                print "\e[48;5;232m";
                .print for $css.comb.rotor(6 => -5)».map({ ($^a, $^b).join })\
                .map( { sprintf "\e[38;2;%d;%d;%dm", |$_».parse-base(16) } ) Z~ $cs.comb;
                say "\e[0m  {$line.words[1..*]}";
            }
            when 'HTML' {
                print "$_\</span>" for $css.comb.rotor(6 => -5)\
                .map( { "<span style=\"color:#{.join};\">" } ) Z~ $cs.comb;
                say " <span style=\"color:#ffffff\"> {$line.words[1..*]}</span>";
                say '
';
            }
            default { say $line; }
        }
    }

    say '</div>' if $mode eq 'HTML';
} else { # just pass the unaltered line through
    .say while $_ = get();
}

```


Can't really show the ANSI output directly so show the HTML output. Essentially identical.

```txt
md5sum *.p6 | perl6 checksum-color.p6 HTML > checksum-color.html
```

yields:

<div style="background-color:black; font-size:125%; font-family: Monaco, monospace;">
<span style="color:#f09a3f;">f</span><span style="color:#09a3fc;">0</span><span style="color:#9a3fc8;">9</span><span style="color:#a3fc85;">a</span><span style="color:#3fc855;">3</span><span style="color:#fc8551;">f</span><span style="color:#c8551d;">c</span><span style="color:#8551d8;">8</span><span style="color:#551d8a;">5</span><span style="color:#51d8a7;">5</span><span style="color:#1d8a70;">1</span><span style="color:#d8a703;">d</span><span style="color:#8a703d;">8</span><span style="color:#a703d6;">a</span><span style="color:#703d64;">7</span><span style="color:#03d64d;">0</span><span style="color:#3d64d8;">3</span><span style="color:#d64d8e;">d</span><span style="color:#64d8e9;">6</span><span style="color:#4d8e91;">4</span><span style="color:#d8e918;">d</span><span style="color:#8e918e;">8</span><span style="color:#e918ec;">e</span><span style="color:#918ece;">9</span><span style="color:#18ece2;">1</span><span style="color:#8ece23;">8</span><span style="color:#ece236;">e</span><span style="color:#ce236f;">c</span><span style="color:#e236f0;">e</span><span style="color:#236f09;">2</span><span style="color:#36f09a;">3</span><span style="color:#6f09a3;">6</span> <span style="color:#ffffff"> checksum-color (another copy).p6</span>


<span style="color:#f09a3f;">f</span><span style="color:#09a3fc;">0</span><span style="color:#9a3fc8;">9</span><span style="color:#a3fc85;">a</span><span style="color:#3fc855;">3</span><span style="color:#fc8551;">f</span><span style="color:#c8551d;">c</span><span style="color:#8551d8;">8</span><span style="color:#551d8a;">5</span><span style="color:#51d8a7;">5</span><span style="color:#1d8a70;">1</span><span style="color:#d8a703;">d</span><span style="color:#8a703d;">8</span><span style="color:#a703d6;">a</span><span style="color:#703d64;">7</span><span style="color:#03d64d;">0</span><span style="color:#3d64d8;">3</span><span style="color:#d64d8e;">d</span><span style="color:#64d8e9;">6</span><span style="color:#4d8e91;">4</span><span style="color:#d8e918;">d</span><span style="color:#8e918e;">8</span><span style="color:#e918ec;">e</span><span style="color:#918ece;">9</span><span style="color:#18ece2;">1</span><span style="color:#8ece23;">8</span><span style="color:#ece236;">e</span><span style="color:#ce236f;">c</span><span style="color:#e236f0;">e</span><span style="color:#236f09;">2</span><span style="color:#36f09a;">3</span><span style="color:#6f09a3;">6</span> <span style="color:#ffffff"> checksum-color (copy).p6</span>


<span style="color:#f09a3f;">f</span><span style="color:#09a3fc;">0</span><span style="color:#9a3fc8;">9</span><span style="color:#a3fc85;">a</span><span style="color:#3fc855;">3</span><span style="color:#fc8551;">f</span><span style="color:#c8551d;">c</span><span style="color:#8551d8;">8</span><span style="color:#551d8a;">5</span><span style="color:#51d8a7;">5</span><span style="color:#1d8a70;">1</span><span style="color:#d8a703;">d</span><span style="color:#8a703d;">8</span><span style="color:#a703d6;">a</span><span style="color:#703d64;">7</span><span style="color:#03d64d;">0</span><span style="color:#3d64d8;">3</span><span style="color:#d64d8e;">d</span><span style="color:#64d8e9;">6</span><span style="color:#4d8e91;">4</span><span style="color:#d8e918;">d</span><span style="color:#8e918e;">8</span><span style="color:#e918ec;">e</span><span style="color:#918ece;">9</span><span style="color:#18ece2;">1</span><span style="color:#8ece23;">8</span><span style="color:#ece236;">e</span><span style="color:#ce236f;">c</span><span style="color:#e236f0;">e</span><span style="color:#236f09;">2</span><span style="color:#36f09a;">3</span><span style="color:#6f09a3;">6</span> <span style="color:#ffffff"> checksum-color.p6</span>


<span style="color:#bbd8a9;">b</span><span style="color:#bd8a92;">b</span><span style="color:#d8a92c;">d</span><span style="color:#8a92c3;">8</span><span style="color:#a92c32;">a</span><span style="color:#92c326;">9</span><span style="color:#2c326c;">2</span><span style="color:#c326c8;">c</span><span style="color:#326c8a;">3</span><span style="color:#26c8a3;">2</span><span style="color:#6c8a35;">6</span><span style="color:#c8a35e;">c</span><span style="color:#8a35e8;">8</span><span style="color:#a35e80;">a</span><span style="color:#35e80d;">3</span><span style="color:#5e80d2;">5</span><span style="color:#e80d2d;">e</span><span style="color:#80d2d7;">8</span><span style="color:#0d2d71;">0</span><span style="color:#d2d71a;">d</span><span style="color:#2d71ab;">2</span><span style="color:#d71ab8;">d</span><span style="color:#71ab89;">7</span><span style="color:#1ab890;">1</span><span style="color:#ab8902;">a</span><span style="color:#b8902c;">b</span><span style="color:#8902cd;">8</span><span style="color:#902cdb;">9</span><span style="color:#02cdbb;">0</span><span style="color:#2cdbbd;">2</span><span style="color:#cdbbd8;">c</span><span style="color:#dbbd8a;">d</span> <span style="color:#ffffff"> something-completely-different.p6</span>


</div>


## Phix

Since text_color() accepts 0..15 we may as well just do it digit-by-digit, 
but avoid (eg) black-on-black by using the inverse background colour as well.

Terminal handling omitted.

```Phix
procedure colourhex(string s)
    for i=1 to length(s) do
        integer ch = s[i],
                k = find(upper(ch),"123456789ABCDEF")
        text_color(15-k)
        bk_color(k)
        puts(1,ch)
    end for
    text_color(BRIGHT_WHITE) -- 15
    bk_color(BLACK)
end procedure
colourhex("#0123456789ABCDEF\n")
```

Varies between windows and linux, but something a bit like this (which is a mock-up):
<div width:600px; font-size:100%; font-family: Monaco, monospace;">
<span style="background-color:black; color:white;">#0</span><span style="background-color:blue; color:yellow;">1</span><span style="background-color:red; color:magenta;">2</span><span style="background-color:lilac; color:green;">3</span><span style="background-color:cyan; color:brown;">4</span><span style="background-color:purple; color:gray;">5</span><span style="background-color:orange; color:pink;">6</span><span style="background-color:indigo; color:mint;">7</span><span style="background-color:mint; color:indigo;">8</span><span style="background-color:pink; color:orange;">9</span><span style="background-color:gray; color:purple;">A</span><span style="background-color:brown; color:cyan;">B</span><span style="background-color:green; color:lilac;">C</span><span style="background-color:magenta; color:red;">D</span><span style="background-color:yellow; color:blue;">E</span><span style="background-color:white; color:black;">F</span>
</div>


## Sidef


```ruby
var ansi = frequire("Term::ANSIColor")

func colorhash(hash) {
    hash.split(2).map{|s| ansi.colored(s, "ansi" + s.hex) }.join
}

ARGF.each {|line|
    if (STDOUT.is_on_tty && (line =~ /^([[:xdigit:]]+)(.*)/)) {|m|
        say (colorhash(m[0]), m[1])
    }
    else {
        say line
    }
}
```

<div style="background-color:black; color:white; width:600px; font-size:100%; font-family: Monaco, monospace;">
<font color="#FF5252">% </font>md5sum *.sf | sf checksumcolor.sf


<font color="#A8A8A8">f8</font><font color="#D78700">ac</font><font color="#42A5F5">04</font><font color="#D7FFAF">c1</font><font color="#AF5FAF">85</font><font color="#AF0000">7c</font><font color="#000000">10</font><font color="#AFAFAF">91</font><font color="#5F87FF">45</font><font color="#FFD7FF">e1</font><font color="#D7FF5F">bf</font><font color="#F5F5F5">0f</font><font color="#FFFF00">e2</font><font color="#5FFFAF">55</font><font color="#5FD7D7">50</font><font color="#FF8787">d2</font>  checksumcolor (copy).sf


<font color="#A8A8A8">f8</font><font color="#D78700">ac</font><font color="#42A5F5">04</font><font color="#D7FFAF">c1</font><font color="#AF5FAF">85</font><font color="#AF0000">7c</font><font color="#000000">10</font><font color="#AFAFAF">91</font><font color="#5F87FF">45</font><font color="#FFD7FF">e1</font><font color="#D7FF5F">bf</font><font color="#F5F5F5">0f</font><font color="#FFFF00">e2</font><font color="#5FFFAF">55</font><font color="#5FD7D7">50</font><font color="#FF8787">d2</font>  checksumcolor.sf


<font color="#D787AF">af</font><font color="#708284">08</font><font color="#8787FF">69</font><font color="#5F875F">41</font><font color="#D7AFD7">b6</font><font color="#FFD700">dc</font><font color="#8787AF">67</font><font color="#252525">00</font><font color="#008700">1c</font><font color="#FFAF87">d8</font><font color="#00FFAF">31</font><font color="#D7D7AF">bb</font><font color="#0087AF">1f</font><font color="#00AF00">22</font><font color="#D7FFFF">c3</font><font color="#3A3A3A">ed</font>  farey.sf


<font color="#D7AFAF">b5</font><font color="#AF5FAF">85</font><font color="#D7FFD7">c2</font><font color="#5FD7FF">51</font><font color="#5FAF00">46</font><font color="#121212">e9</font><font color="#5FD75F">4d</font><font color="#767676">f3</font><font color="#87D700">70</font><font color="#303030">ec</font><font color="#5FAF87">48</font><font color="#5FAF00">46</font><font color="#8787FF">69</font><font color="#00005F">11</font><font color="#FFAFAF">d9</font><font color="#D78787">ae</font>  pell.sf
</div>


## zkl

```zkl
var [const] colorRGBs=T(T(15,  0,  0), T(0 ,15,  0), T(15, 15,  0),
			T( 0,  0, 15), T(15, 0, 15), T( 0, 15, 15) ),
            colorTxt =T("31","32","33","34","35","36");  // esc[<ab>m
fcn squareDist(rgb1,rgb2){ rgb2.zipWith('-,rgb1).apply("pow",2).sum(0) }
fcn colorize(chksum){   // "check sum" --> ansi color escape sequence
   k:=chksum.len()/3*3; // every three digits gets its own color
   chksum[0,k].pump(String,T(Void.Read,2), fcn(r,g,b){
      // find color closest to these three digits of check sum
      // minMaxNs returns indexes of min and max (in list of ints)
      vm.arglist.apply("toInt",16) : // f("a","b","c")-->(10,11,12)
        colorRGBs.apply(squareDist.fp(_)) : (0).minMaxNs(_)[0] : colorTxt[_] :
        "\e[%s;1m%c%c%c".fmt(_, r,g,b)
   })
   .append("\e[0m",chksum[k,*]); // reset color, rest of check sum
}
```

Fake "md5sum coreutils-* | zkl checksumcolor" for testing

```zkl
re,lines := RegExp("([A-Fa-f0-9]+)([ \t]+.+)"),
#<<<
"ab20d840e13adfebf2b6936a2dab071b  coreutils-8.29.tar.gz
b259b2936bb46009be3f5cc06a12c32d  coreutils-8.30.tar.gz
03cf26420de566c306d340df52f6ccd7  coreutils-8.31.tar.gz"
#<<<
.split("\n");

foreach line in (lines){ 
   if(re.search(line)){
      chksum,txt := re.matched[1,*];
      println(colorize(chksum),txt);
   }
}
```

```txt
Same as the OCaml entry
```

This is what we would do to implement "md5sum chksum.zkl  | zkl chksum"
(instead of the above test code)

```zkl
re:=RegExp("([A-Fa-f0-9]+)([ \t]+.+)");
foreach line in (File.stdin){ 
   if(re.search(line)){
      chksum,txt := re.matched[1,*];
      println(colorize(chksum),txt);
   } else print(line);
}
```

