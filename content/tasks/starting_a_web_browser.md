+++
title = "Starting a web browser"
description = ""
date = 2019-04-26T13:34:39Z
aliases = []
[extra]
id = 17802
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "echolisp",
  "go",
  "perl",
  "perl_6",
  "phix",
  "racket",
  "scala",
  "tcl",
]
+++

{{Draft task}} [[Category:Web]]
Build and show a web page displaying the data from the task [[Separate_the_house_number_from_the_street_name|separate the house number from the street name]] in a formatted and colored table.

## Task

Write the code which automatically opens a web page in a browser showing the addresses. 
<em>No static data must be shown, only processed data.</em>



## EchoLisp

Since we are already in a browser window, the browser is started. '''print-hatml''' sends an html string to the standard output area. The table is styled with the "htable" css class, which is included in EchoLisp.

```scheme

;; table from "Separate_the_house_number_from_the_street_name"
;;
(print-html (table->html adresses))

```

<table class = "htable" style = "border: 1px solid blue;">
<tr>
<td> Plataanstraat</td> <td> 5</td>
</tr>
<tr>
<td> Straat</td> <td> 12</td>
</tr>
<tr>
<td> Straat</td> <td> 12 II</td>
</tr>
<tr>
<td> Straat 1940</td> <td> II</td>
</tr>
<tr>
<td> Dr. J. Straat </td> <td> 40</td>
</tr>
<tr>
<td> Dr. J. Straat</td> <td> 12 a</td>
</tr>
<tr>
<td> Dr. J. Straat</td> <td> 12-14</td>
</tr>
<tr>
<td> Laan 1940 – 1945</td> <td> 37</td>
</tr>
<tr>
<td> Plein 1940</td> <td> 2</td>
</tr>
<tr>
<td> 1213-laan</td> <td> 11</td>
</tr>
<tr>
<td> 16 april</td> <td> 1944 Pad 1</td>
</tr>
<tr>
<td> 1e Kruisweg</td> <td> 36</td>
</tr>
<tr>
<td> Laan 1940-'45</td> <td> 66</td>
</tr>
<tr>
<td> Laan '40-'45</td> <td> ❓❓❓</td>
</tr>
<tr>
<td> Langeloërduinen</td> <td> 3 46</td>
</tr>
<tr>
<td> Marienwaerdt</td> <td> 2e Dreef 2</td>
</tr>
<tr>
<td> Provincialeweg N205</td> <td> 1</td>
</tr>
<tr>
<td> Rivium</td> <td> 2e Straat 59.</td>
</tr>
<tr>
<td> Nieuwe gracht</td> <td> 20rd</td>
</tr>
<tr>
<td> Nieuwe gracht</td> <td> 20rd 2</td>
</tr>
<tr>
<td> Nieuwe gracht</td> <td> 20zw /2</td>
</tr>
<tr>
<td> Nieuwe gracht</td> <td> 20zw/3</td>
</tr>
<tr>
<td> Nieuwe gracht</td> <td> 20 zw/4</td>
</tr>
<tr>
<td> Bahnhofstr.</td> <td> 4</td>
</tr>
<tr>
<td> Wertstr.</td> <td> 10</td>
</tr>
<tr>
<td> Lindenhof</td> <td> 1</td>
</tr>
<tr>
<td> Nordesch</td> <td> 20</td>
</tr>
<tr>
<td> Weilstr.</td> <td> 6</td>
</tr>
<tr>
<td> Harthauer Weg</td> <td> 2</td>
</tr>
<tr>
<td> Mainaustr.</td> <td> 49</td>
</tr>
<tr>
<td> August-Horch-Str.</td> <td> 3</td>
</tr>
<tr>
<td> Marktplatz</td> <td> 31</td>
</tr>
<tr>
<td> Schmidener Weg</td> <td> 3</td>
</tr>
<tr>
<td> Karl-Weysser-Str.</td> <td> 6</td>
</tr>
</table>


## Go

This uses the same format and color scheme for the table as the Perl 6 entry.

```go
package main

import (
    "fmt"
    "html/template"
    "log"
    "os"
    "os/exec"
    "strings"
    "time"
)

type row struct {
    Address, Street, House, Color string
}

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

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

var tmpl = `
<head>
  <title>Rosetta Code - Start a Web Browser</title>
  <meta charset="UTF-8">
</head>
<body bgcolor="#d8dcd6">
  <table border="2">
    <p align="center">
      <font face="Arial, sans-serif" size="5">Split the house number from the street name</font>    
      <tr bgcolor="#02ccfe"><th>Address</th><th>Street</th><th>House Number</th></tr>
      {{range $row := .}}
      <tr bgcolor={{$row.Color}}>         
        <td>{{$row.Address}}</td>
        <td>{{$row.Street}}</td>
        <td>{{$row.House}}</td>
      </tr>
      {{end}}
    </p>
  </table>
</body>
`
func main() {
    addresses := []string{
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
    browser := "firefox" // or whatever
    colors := [2]string{"#d7fffe", "#9dbcd4"}
    fileName := "addresses_table.html"
    ct := template.Must(template.New("").Parse(tmpl))
    file, err := os.Create(fileName)
    check(err)
    rows := make([]row, len(addresses))
    for i, address := range addresses {
        street, house := separateHouseNumber(address)
        if house == "" {
            house = "(none)"
        }
        color := colors[i%2]
        rows[i] = row{address, street, house, color}
    }   
    err = ct.Execute(file, rows)
    check(err)
    cmd := exec.Command(browser, fileName)
    err = cmd.Run()
    check(err)
    file.Close()
    time.Sleep(5 * time.Second) // wait for 5 seconds before deleting file
    err = os.Remove(fileName)
    check(err)
}
```


```txt

Similar to the Perl 6 entry.

```



## Perl

Borrowing code from the [[Separate_the_house_number_from_the_street_name|Separate the house number from the street name]] task.
```perl
use File::Temp qw(tempfile);

my @addresses = (
'Plataanstraat 5',      'Straat 12',             'Straat 12 II',            'Dr. J. Straat   12',
'Dr. J. Straat 12 a',   'Dr. J. Straat 12-14',   'Laan 1940 – 1945 37',     'Plein 1940 2',
'1213-laan 11',         '16 april 1944 Pad 1',   '1e Kruisweg 36',          'Laan 1940-’45 66',
'Laan ’40-’45',         'Langeloërduinen 3 46',  'Marienwaerdt 2e Dreef 2', 'Provincialeweg N205 1',
'Rivium 2e Straat 59.', 'Nieuwe gracht 20rd',    'Nieuwe gracht 20rd 2',    'Nieuwe gracht 20zw /2',
'Nieuwe gracht 20zw/3', 'Nieuwe gracht 20 zw/4', 'Bahnhofstr. 4',           'Wertstr. 10',
'Lindenhof 1',          'Nordesch 20',           'Weilstr. 6',              'Harthauer Weg 2',
'Mainaustr. 49',        'August-Horch-Str. 3',   'Marktplatz 31',           'Schmidener Weg 3',
'Karl-Weysser-Str. 6');


my @row_color = ('#d7fffe', '#9dbcd4');

# build the table
sub genTable {
    my $table = '<table border="2"> <tr bgcolor="#02ccfe">' .
    qq|<th>Address</th><th>Street</th><th>House Number</th>\n|;
    my $i = 0;
    for my $addr (@addresses) {
        $table .= qq|<tr bgcolor="@{[@row_color[$i++ % 2]]}"><td>$addr</td>|;

        my($street,$number) =  $addr =~
        m[^ (.*?) \s+
            (
               \d* (\-|\/)? \d*
             | \d{1,3} [a-zI./ ]* \d{0,3}
            ) $
         ]x;
        if (!$number) { $street = $addr; $number = '(no match)' }
        $table .= qq|<td>$street</td><td>$number</td></tr>\n|;
    }
    return $table . '</table>';
}

my $streets_and_numbers = genTable();

# generate the page content
sub content {
return <<END;
<html>
<head>
<title>Rosetta Code - Start a Web Browser</title>
<meta charset="UTF-8">
</head>
<body bgcolor="#d8dcd6">
<p align="center">
<font face="Arial, sans-serif" size="5">Split the house number from the street name</font>
</p>
<p align="center">
$streets_and_numbers
</p>
</body>
</html>
END
}

# Use a temporary file name and file handle
my ($fn, $fh) = tempfile :suffix('.html');

# dump the content to the file
open my $fh, '>', $fn;
print $fh content();
close $fh;

# use appropriate command for X11 (other systems will need different invocation)
my $command = "xdg-open $fn";

# start the browser
system "$command";

# wait for a bit to give browser time to load before destroying temp file
sleep 5;

```

<table border="2"> <tr bgcolor="#02ccfe"><th>Address</th><th>Street</th><th>House Number</th>
<tr bgcolor="#d7fffe"><td>Plataanstraat 5</td><td>Plataanstraat</td><td>5</td></tr>
<tr bgcolor="#9dbcd4"><td>Straat 12</td><td>Straat</td><td>12</td></tr>
<tr bgcolor="#d7fffe"><td>Straat 12 II</td><td>Straat</td><td>12 II</td></tr>
<tr bgcolor="#9dbcd4"><td>Dr. J. Straat   12</td><td>Dr. J. Straat</td><td>12</td></tr>
<tr bgcolor="#d7fffe"><td>Dr. J. Straat 12 a</td><td>Dr. J. Straat</td><td>12 a</td></tr>
<tr bgcolor="#9dbcd4"><td>Dr. J. Straat 12-14</td><td>Dr. J. Straat</td><td>12-14</td></tr>
<tr bgcolor="#d7fffe"><td>Laan 1940 – 1945 37</td><td>Laan 1940 – 1945</td><td>37</td></tr>
<tr bgcolor="#9dbcd4"><td>Plein 1940 2</td><td>Plein 1940</td><td>2</td></tr>
<tr bgcolor="#d7fffe"><td>1213-laan 11</td><td>1213-laan</td><td>11</td></tr>
<tr bgcolor="#9dbcd4"><td>16 april 1944 Pad 1</td><td>16 april 1944 Pad</td><td>1</td></tr>
<tr bgcolor="#d7fffe"><td>1e Kruisweg 36</td><td>1e Kruisweg</td><td>36</td></tr>
<tr bgcolor="#9dbcd4"><td>Laan 1940-’45 66</td><td>Laan 1940-’45</td><td>66</td></tr>
<tr bgcolor="#d7fffe"><td>Laan ’40-’45</td><td>Laan ’40-’45</td><td>(no match)</td></tr>
<tr bgcolor="#9dbcd4"><td>Langeloërduinen 3 46</td><td>Langeloërduinen</td><td>3 46</td></tr>
<tr bgcolor="#d7fffe"><td>Marienwaerdt 2e Dreef 2</td><td>Marienwaerdt 2e Dreef</td><td>2</td></tr>
<tr bgcolor="#9dbcd4"><td>Provincialeweg N205 1</td><td>Provincialeweg N205</td><td>1</td></tr>
<tr bgcolor="#d7fffe"><td>Rivium 2e Straat 59.</td><td>Rivium 2e Straat</td><td>59.</td></tr>
<tr bgcolor="#9dbcd4"><td>Nieuwe gracht 20rd</td><td>Nieuwe gracht</td><td>20rd</td></tr>
<tr bgcolor="#d7fffe"><td>Nieuwe gracht 20rd 2</td><td>Nieuwe gracht</td><td>20rd 2</td></tr>
<tr bgcolor="#9dbcd4"><td>Nieuwe gracht 20zw /2</td><td>Nieuwe gracht</td><td>20zw /2</td></tr>
<tr bgcolor="#d7fffe"><td>Nieuwe gracht 20zw/3</td><td>Nieuwe gracht</td><td>20zw/3</td></tr>
<tr bgcolor="#9dbcd4"><td>Nieuwe gracht 20 zw/4</td><td>Nieuwe gracht</td><td>20 zw/4</td></tr>
<tr bgcolor="#d7fffe"><td>Bahnhofstr. 4</td><td>Bahnhofstr.</td><td>4</td></tr>
<tr bgcolor="#9dbcd4"><td>Wertstr. 10</td><td>Wertstr.</td><td>10</td></tr>
<tr bgcolor="#d7fffe"><td>Lindenhof 1</td><td>Lindenhof</td><td>1</td></tr>
<tr bgcolor="#9dbcd4"><td>Nordesch 20</td><td>Nordesch</td><td>20</td></tr>
<tr bgcolor="#d7fffe"><td>Weilstr. 6</td><td>Weilstr.</td><td>6</td></tr>
<tr bgcolor="#9dbcd4"><td>Harthauer Weg 2</td><td>Harthauer Weg</td><td>2</td></tr><tr bgcolor="#d7fffe"><td>Mainaustr. 49</td><td>Mainaustr.</td><td>49</td></tr>
<tr bgcolor="#9dbcd4"><td>August-Horch-Str. 3</td><td>August-Horch-Str.</td><td>3</td></tr>
<tr bgcolor="#d7fffe"><td>Marktplatz 31</td><td>Marktplatz</td><td>31</td></tr>
<tr bgcolor="#9dbcd4"><td>Schmidener Weg 3</td><td>Schmidener Weg</td><td>3</td></tr>
<tr bgcolor="#d7fffe"><td>Karl-Weysser-Str. 6</td><td>Karl-Weysser-Str.</td><td>6</td></tr>
</table>


## Perl 6

Uses the code from the [[Separate_the_house_number_from_the_street_name|Separate the house number from the street name]] task almost verbatim. Included here to make a complete, runnable example.


```perl6
use File::Temp;

my $addresses = qq :to /END/;
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
    END

my @row-color = '#d7fffe', '#9dbcd4';

# build the table
sub genTable () {
    my $table = '<table border="2"> <tr bgcolor="#02ccfe">' ~
    qq|<th>Address</th><th>Street</th><th>House Number</th>\n|;
    my $i = 0;
    for $addresses.lines -> $addr {
        $table ~= qq|<tr bgcolor="{@row-color[$i++ % 2]}"><td>{$addr}</td>|;
        $addr ~~ m[
            ( .*? )
            [
                \s+
                (
                | \d+ [ \- | \/ ] \d+
                | <!before 1940 | 1945> \d+ <[ a..z I . / \x20 ]>* \d*
                )
            ]?
            $
        ];
        quietly $table ~= qq|<td>{$0.Str}</td><td>{$1.Str||''}</td></tr>\n|;
    }
    $table ~ '</table>';
}

# generate the page content
sub content {
    qq :to /END/;
    <html>
    <head>
    <title>Rosetta Code - Start a Web Browser</title>
    <meta charset="UTF-8">
    </head>
    <body bgcolor="#d8dcd6">
    <p align="center">
    <font face="Arial, sans-serif" size="5">Split the house number from the street name</font>
    </p>
    <p align="center">
    { genTable }
    </p>
    </body>
    </html>
    END
}

# Use a temporary file name and file handle
my ($fn, $fh) = tempfile :suffix('.html');

# dump the content to the file
$fh.spurt: content;

# use appropriate command for Windows or X11
# other OSs/WMs may need different invocation
my $command = $*DISTRO.is-win ?? "start $fn" !! "xdg-open $fn";

# start the browser
shell $command;

# wait for a bit to give browser time to load before destroying temp file
sleep 5;

```

{{out}} Will start the default browser (or open a new tab/window in a running one) and display this table.
<table border="2"> <tr bgcolor="#02ccfe"><th>Address</th><th>Street</th><th>House Number</th>
<tr bgcolor="#d7fffe"><td>Plataanstraat 5</td><td>Plataanstraat</td><td>5</td></tr>
<tr bgcolor="#9dbcd4"><td>Straat 12</td><td>Straat</td><td>12</td></tr>
<tr bgcolor="#d7fffe"><td>Straat 12 II</td><td>Straat</td><td>12 II</td></tr>
<tr bgcolor="#9dbcd4"><td>Dr. J. Straat   12</td><td>Dr. J. Straat</td><td>12</td></tr>
<tr bgcolor="#d7fffe"><td>Dr. J. Straat 12 a</td><td>Dr. J. Straat</td><td>12 a</td></tr>
<tr bgcolor="#9dbcd4"><td>Dr. J. Straat 12-14</td><td>Dr. J. Straat</td><td>12-14</td></tr>
<tr bgcolor="#d7fffe"><td>Laan 1940 – 1945 37</td><td>Laan 1940 – 1945</td><td>37</td></tr>
<tr bgcolor="#9dbcd4"><td>Plein 1940 2</td><td>Plein 1940</td><td>2</td></tr>
<tr bgcolor="#d7fffe"><td>1213-laan 11</td><td>1213-laan</td><td>11</td></tr>
<tr bgcolor="#9dbcd4"><td>16 april 1944 Pad 1</td><td>16 april 1944 Pad</td><td>1</td></tr>
<tr bgcolor="#d7fffe"><td>1e Kruisweg 36</td><td>1e Kruisweg</td><td>36</td></tr>
<tr bgcolor="#9dbcd4"><td>Laan 1940-’45 66</td><td>Laan 1940-’45</td><td>66</td></tr>
<tr bgcolor="#d7fffe"><td>Laan ’40-’45</td><td>Laan ’40-’45</td><td></td></tr>
<tr bgcolor="#9dbcd4"><td>Langeloërduinen 3 46</td><td>Langeloërduinen</td><td>3 46</td></tr>
<tr bgcolor="#d7fffe"><td>Marienwaerdt 2e Dreef 2</td><td>Marienwaerdt 2e Dreef</td><td>2</td></tr>
<tr bgcolor="#9dbcd4"><td>Provincialeweg N205 1</td><td>Provincialeweg N205</td><td>1</td></tr>
<tr bgcolor="#d7fffe"><td>Rivium 2e Straat 59.</td><td>Rivium 2e Straat</td><td>59.</td></tr>
<tr bgcolor="#9dbcd4"><td>Nieuwe gracht 20rd</td><td>Nieuwe gracht</td><td>20rd</td></tr>
<tr bgcolor="#d7fffe"><td>Nieuwe gracht 20rd 2</td><td>Nieuwe gracht</td><td>20rd 2</td></tr>
<tr bgcolor="#9dbcd4"><td>Nieuwe gracht 20zw /2</td><td>Nieuwe gracht</td><td>20zw /2</td></tr>
<tr bgcolor="#d7fffe"><td>Nieuwe gracht 20zw/3</td><td>Nieuwe gracht</td><td>20zw/3</td></tr>
<tr bgcolor="#9dbcd4"><td>Nieuwe gracht 20 zw/4</td><td>Nieuwe gracht</td><td>20 zw/4</td></tr>
<tr bgcolor="#d7fffe"><td>Bahnhofstr. 4</td><td>Bahnhofstr.</td><td>4</td></tr>
<tr bgcolor="#9dbcd4"><td>Wertstr. 10</td><td>Wertstr.</td><td>10</td></tr>
<tr bgcolor="#d7fffe"><td>Lindenhof 1</td><td>Lindenhof</td><td>1</td></tr>
<tr bgcolor="#9dbcd4"><td>Nordesch 20</td><td>Nordesch</td><td>20</td></tr>
<tr bgcolor="#d7fffe"><td>Weilstr. 6</td><td>Weilstr.</td><td>6</td></tr>
<tr bgcolor="#9dbcd4"><td>Harthauer Weg 2</td><td>Harthauer Weg</td><td>2</td></tr>
<tr bgcolor="#d7fffe"><td>Mainaustr. 49</td><td>Mainaustr.</td><td>49</td></tr>
<tr bgcolor="#9dbcd4"><td>August-Horch-Str. 3</td><td>August-Horch-Str.</td><td>3</td></tr>
<tr bgcolor="#d7fffe"><td>Marktplatz 31</td><td>Marktplatz</td><td>31</td></tr>
<tr bgcolor="#9dbcd4"><td>Schmidener Weg 3</td><td>Schmidener Weg</td><td>3</td></tr>
<tr bgcolor="#d7fffe"><td>Karl-Weysser-Str. 6</td><td>Karl-Weysser-Str.</td><td>6</td></tr>
</table>


## Phix


```Phix
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

function isDigit(integer ch)
    return ch>='0' and ch<='9'
end function
 
function separateHouseNumber(integer i)
    string address = addresses[i]
    sequence parts = split(address,no_empty:=true)
    string street, house
    integer h = 0
    if length(parts)>1 then
        string last = parts[$]
        if isDigit(last[1]) then
            h = 1
            string penult = parts[$-1]
            if length(parts)>2
            and isDigit(penult[1])
            and match("194",penult)!=1 then
                h = 2
            end if
        elsif length(parts)>2 then
            h = 2
        end if
    end if
    if h then
        street = join(parts[1..$-h])
        house = join(parts[$-h+1..$])
    else
        street = join(parts)
        house = "(none)"
    end if
    string colour = iff(mod(i,2)=0?"#d7fffe":"#9dbcd4")
    return {colour,address,street,house}
end function
 
constant html_hdr = """
<html>
 <head>
 <title>Rosetta Code - Start a Web Browser</title>
 <meta charset="UTF-8">
 </head>
 <body bgcolor="#d8dcd6">
  <p align="center">
  <font face="Arial, sans-serif" size="5">Split the house number from the street name</font>
  </p>
  <p align="center">
   <table border="2"> <tr bgcolor="#02ccfe">
    <th>Address</th><th>Street</th><th>House Number</th>
""",
         html_line = """
    <tr bgcolor=%s><td>%s</td><td>%s</td><td>%s</td></tr>
""",
         html_ftr = """
   </table>
  </p>
 </body>
</html>
"""

procedure main()
    integer fn = open("test.html","w")
    printf(fn,html_hdr)
    for i=1 to length(addresses) do
        printf(fn,html_line,separateHouseNumber(i))
    end for
    printf(fn,html_ftr)
    close(fn)
    system("test.html")
end procedure
main()
```

output as perl


## Racket



```racket

#lang at-exp racket

... same code as "Separate_the_house_number_from_the_street_name" ...

(require net/sendurl scribble/html)

(define (render-table)
  (for/list ([str (in-list (string-split adressen #rx" *\r?\n *"))]
             [i   (in-naturals)])
    (tr bgcolor: (if (even? i) "#fcf" "#cff")
        (td str)
        (map td (cond [(splits-adressen str) => cdr] [else '(??? ???)])))))

@(compose1 send-url/contents xml->string){
  @html{@head{@title{Splitting Results}}
        @body{@h1{Splitting Results}
              @table{@render-table}}}
}

```



## Scala

<!-- ==={{libheader|Scala}}=== -->
Uses nothing but the Standard Library:

```scala
import java.awt.Desktop
import java.io.{IOException, PrintWriter}
import java.net.{URI, ServerSocket}
import scala.xml.Elem

class WebServer(port: Int, soleDocument: Elem) extends Thread {
  this.setName(s"Server at $port")

  override def run() {
    val listener = try {
      new ServerSocket(port)
    } catch {
      case e: java.net.BindException => throw new IllegalStateException(s"Port $port already taken!")
    }
    println(s"Listening on port ${listener.getLocalPort}")

    while (!Thread.interrupted()) {
      try {
        //print(".")
        val socket = listener.accept
        new PrintWriter(socket.getOutputStream, true).println(soleDocument)
        socket.close()
      } catch {
        case ioe: IOException => println(ioe)
      }
    }
  }
} // class WebServer

object HtmlServer extends App {
  val PORT = 64507
  // Main
  val thread = new WebServer(PORT, HtmlBuilder)
  val uri = URI.create(s"http://localhost:$PORT/")
  thread.start()

  def HtmlBuilder: Elem = {
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

    def getSplittedAddresses(addresses: Iterator[String]) = {
      val extractor = new scala.util.matching.Regex( """(\s\d+[-/]\d+)|(\s(?!1940|1945)\d+[a-zI. /]*\d*)$|\d+\['][40|45]$""")


      def splitsAdres(input: String): (String, String) =
        (extractor.split(input).mkString, extractor.findFirstIn(input).getOrElse(""))

      addresses.map(org => {
        val temp = splitsAdres(org)
        List(org, temp._1, temp._2)
      })
    }

    def generateTable: Elem = {

      def coloring(rownum: Any): String = {
        rownum match {
          case Nil => "#9bbb59"
          case n: Int => if (n % 2 == 0) "#ebf1de" else "#d8e4bc"
        }
      }

      <table border="10">
        {(List(List("Given Address", "Street", "House Number")) ++ getSplittedAddresses(adressen)).
        zipWithIndex.map { case (row, rownum) => (if (rownum == 0) Nil else rownum) +: row}.map(row =>
        <tr bgcolor={coloring(row.head)}>
          {row.map(cell =>
          if (row.head == Nil)
            <th>
              {cell}
            </th>
          else
            <td>
              {cell}
            </td>)}
        </tr>)}
      </table>
    } // def generateTable

    <html>
      <head>
        <title>Rosetta.org Task solution</title>
      </head>
      <body lang="en-US" bgcolor="#e6e6ff" dir="LTR">
        <p align="CENTER">
          <font face="Arial, sans-serif" size="5">Split the house number from the street name</font>
        </p>
        <p align="CENTER">
          {generateTable}
        </p>
      </body>
    </html>
  } // def content
  if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE))
    Desktop.getDesktop.browse(uri)
  else println(s"Automatic start of Web browser not possible.\nWeb browser must be started manually, use $uri.")

  if (!thread.isAlive) sys.exit(-1)
  println("Web server started.")
  do print("Do you want to shutdown this server? <Y(es)/N>: ") while (!scala.io.StdIn.readBoolean)
  sys.exit()
}
```

<font face="Arial, sans-serif" size="5">Split the house number from the street name</font>
<p align="center"><table border="10"><tr bgcolor="#9bbb59"><th></th><th>Given Address</th><th>Street</th><th>House Number</th></tr><tr bgcolor="#d8e4bc"><td>1</td><td>Plataanstraat 5</td><td>Plataanstraat</td><td>5</td></tr><tr bgcolor="#ebf1de"><td>2</td><td>Straat 12</td><td>Straat</td><td>12</td></tr><tr bgcolor="#d8e4bc"><td>3</td><td>Straat 12 II</td><td>Straat</td><td>12 II</td></tr><tr bgcolor="#ebf1de"><td>4</td><td>Straat 1940 II</td><td>Straat 1940 II</td><td></td></tr><tr bgcolor="#d8e4bc"><td>5</td><td>Dr. J. Straat40</td><td>Dr. J. Straat  </td><td>40</td></tr><tr bgcolor="#ebf1de"><td>6</td><td>Dr. J. Straat 12 a</td><td>Dr. J. Straat</td><td>12 a</td></tr><tr bgcolor="#d8e4bc"><td>7</td><td>Dr. J. Straat 12-14</td><td>Dr. J. Straat</td><td>12-14</td></tr><tr bgcolor="#ebf1de"><td>8</td><td>Laan 1940 – 1945 37</td><td>Laan 1940 – 1945</td><td>37</td></tr><tr bgcolor="#d8e4bc"><td>9</td><td>Plein 1940 2</td><td>Plein 1940</td><td>2</td></tr><tr bgcolor="#ebf1de"><td>10</td><td>1213-laan 11</td><td>1213-laan</td><td>11</td></tr><tr bgcolor="#d8e4bc"><td>11</td><td>16 april 1944 Pad 1</td><td>16 april 1944 Pad</td><td>1</td></tr><tr bgcolor="#ebf1de"><td>12</td><td>1e Kruisweg 36</td><td>1e Kruisweg</td><td>36</td></tr><tr bgcolor="#d8e4bc"><td>13</td><td>Laan 1940-’45 66</td><td>Laan 1940-’45</td><td>66</td></tr><tr bgcolor="#ebf1de"><td>14</td><td>Laan ’40-’45</td><td>Laan ’40-’45</td><td></td></tr><tr bgcolor="#d8e4bc"><td>15</td><td>Langeloërduinen 3 46</td><td>Langeloërduinen</td><td>3 46</td></tr><tr bgcolor="#ebf1de"><td>16</td><td>Marienwaerdt 2e Dreef 2</td><td>Marienwaerdt 2e Dreef</td><td>2</td></tr><tr bgcolor="#d8e4bc"><td>17</td><td>Provincialeweg N205 1</td><td>Provincialeweg N205</td><td>1</td></tr><tr bgcolor="#ebf1de"><td>18</td><td>Rivium 2e Straat 59.</td><td>Rivium 2e Straat</td><td>59.</td></tr><tr bgcolor="#d8e4bc"><td>19</td><td>Nieuwe gracht 20rd</td><td>Nieuwe gracht</td><td>20rd</td></tr><tr bgcolor="#ebf1de"><td>20</td><td>Nieuwe gracht 20rd 2</td><td>Nieuwe gracht</td><td>20rd 2</td></tr><tr bgcolor="#d8e4bc"><td>21</td><td>Nieuwe gracht 20zw /2</td><td>Nieuwe gracht</td><td>20zw /2</td></tr><tr bgcolor="#ebf1de"><td>22</td><td>Nieuwe gracht 20zw/3</td><td>Nieuwe gracht</td><td>20zw/3</td></tr><tr bgcolor="#d8e4bc"><td>23</td><td>Nieuwe gracht 20 zw/4</td><td>Nieuwe gracht</td><td>20 zw/4</td></tr><tr bgcolor="#ebf1de"><td>24</td><td>Bahnhofstr. 4</td><td>Bahnhofstr.</td><td>4</td></tr><tr bgcolor="#d8e4bc"><td>25</td><td>Wertstr. 10</td><td>Wertstr.</td><td>10</td></tr><tr bgcolor="#ebf1de"><td>26</td><td>Lindenhof 1</td><td>Lindenhof</td><td>1</td></tr><tr bgcolor="#d8e4bc"><td>27</td><td>Nordesch 20</td><td>Nordesch</td><td>20</td></tr><tr bgcolor="#ebf1de"><td>28</td><td>Weilstr. 6</td><td>Weilstr.</td><td>6</td></tr><tr bgcolor="#d8e4bc"><td>29</td><td>Harthauer Weg 2</td><td>Harthauer Weg</td><td>2</td></tr><tr bgcolor="#ebf1de"><td>30</td><td>Mainaustr. 49</td><td>Mainaustr.</td><td>49</td></tr><tr bgcolor="#d8e4bc"><td>31</td><td>August-Horch-Str. 3</td><td>August-Horch-Str.</td><td>3</td></tr><tr bgcolor="#ebf1de"><td>32</td><td>Marktplatz 31</td><td>Marktplatz</td><td>31</td></tr><tr bgcolor="#d8e4bc"><td>33</td><td>Schmidener Weg 3</td><td>Schmidener Weg</td><td>3</td></tr><tr bgcolor="#ebf1de"><td>34</td><td>Karl-Weysser-Str. 6</td><td>Karl-Weysser-Str.</td><td>6</td></tr></table></p>


## Tcl

{{works with|Tcl|8.6}}<!--required for [file tempfile]-->

```tcl
package require Tcl 8.6

# This is identical to the address task. Skip forward to the next section...
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

# Construct the HTML to show
set html "<html><head><meta charset=\"UTF-8\">
<title>split_DE_NL_address</title></head><body>
<table border><tr><th>Address</th><th>Street</th><th>Number</th></tr>"
foreach streetAddress [split $data "\n"] {
    set streetAddress [string trim $streetAddress]
    if {$streetAddress eq ""} continue
    lassign [split_DE_NL_address $streetAddress] str num
    append html "<tr><td>$streetAddress</td><td>$str</td><td>$num</td></tr>"
}
append html "</table>
</body></html>"
# Pick a unique filename with .html extension (important!)
set f [file tempfile filename street.html]
fconfigure $f -encoding utf-8
puts $f $html
close $f

##### THE WEB BROWSER LAUNCH CODE MAGIC #####
# Relies on the default registration of browsers for .html files
# This is all very platform specific; Android requires another incantation again
if {$tcl_platform(platform) eq "windows"} {
    exec {*}[auto_execok start] "" [file nativename $filename]
} elseif {$tcl_platform(os) eq "Darwin"} {
    exec open $filename
} else {
    exec xdg_open $filename
}
```

<!-- Strictly, this is just the contents of the <body> inside a <div>… --><div style="margin-left: 50px">
<table border><tr><th>Address</th><th>Street</th><th>Number</th></tr><tr><td>Plataanstraat 5</td><td>Plataanstraat</td><td>5</td></tr><tr><td>Straat 12</td><td>Straat</td><td>12</td></tr><tr><td>Straat 12 II</td><td>Straat</td><td>12 II</td></tr><tr><td>Dr. J. Straat   12</td><td>Dr. J. Straat</td><td>12</td></tr><tr><td>Dr. J. Straat 12 a</td><td>Dr. J. Straat</td><td>12 a</td></tr><tr><td>Dr. J. Straat 12-14</td><td>Dr. J. Straat</td><td>12-14</td></tr><tr><td>Laan 1940 – 1945 37</td><td>Laan 1940 – 1945</td><td>37</td></tr><tr><td>Plein 1940 2</td><td>Plein 1940</td><td>2</td></tr><tr><td>1213-laan 11</td><td>1213-laan</td><td>11</td></tr><tr><td>16 april 1944 Pad 1</td><td>16 april 1944 Pad</td><td>1</td></tr><tr><td>1e Kruisweg 36</td><td>1e Kruisweg</td><td>36</td></tr><tr><td>Laan 1940-’45 66</td><td>Laan 1940-’45</td><td>66</td></tr><tr><td>Laan ’40-’45</td><td>Laan ’40-’45</td><td></td></tr><tr><td>Langeloërduinen 3 46</td><td>Langeloërduinen</td><td>3 46</td></tr><tr><td>Marienwaerdt 2e Dreef 2</td><td>Marienwaerdt 2e Dreef</td><td>2</td></tr><tr><td>Provincialeweg N205 1</td><td>Provincialeweg N205</td><td>1</td></tr><tr><td>Rivium 2e Straat 59.</td><td>Rivium 2e Straat</td><td>59.</td></tr><tr><td>Nieuwe gracht 20rd</td><td>Nieuwe gracht</td><td>20rd</td></tr><tr><td>Nieuwe gracht 20rd 2</td><td>Nieuwe gracht</td><td>20rd 2</td></tr><tr><td>Nieuwe gracht 20zw /2</td><td>Nieuwe gracht</td><td>20zw /2</td></tr><tr><td>Nieuwe gracht 20zw/3</td><td>Nieuwe gracht</td><td>20zw/3</td></tr><tr><td>Nieuwe gracht 20 zw/4</td><td>Nieuwe gracht</td><td>20 zw/4</td></tr><tr><td>Bahnhofstr. 4</td><td>Bahnhofstr.</td><td>4</td></tr><tr><td>Wertstr. 10</td><td>Wertstr.</td><td>10</td></tr><tr><td>Lindenhof 1</td><td>Lindenhof</td><td>1</td></tr><tr><td>Nordesch 20</td><td>Nordesch</td><td>20</td></tr><tr><td>Weilstr. 6</td><td>Weilstr.</td><td>6</td></tr><tr><td>Harthauer Weg 2</td><td>Harthauer Weg</td><td>2</td></tr><tr><td>Mainaustr. 49</td><td>Mainaustr.</td><td>49</td></tr><tr><td>August-Horch-Str. 3</td><td>August-Horch-Str.</td><td>3</td></tr><tr><td>Marktplatz 31</td><td>Marktplatz</td><td>31</td></tr><tr><td>Schmidener Weg 3</td><td>Schmidener Weg</td><td>3</td></tr><tr><td>Karl-Weysser-Str. 6</td><td>Karl-Weysser-Str.</td><td>6</td></tr></table>
</div>
