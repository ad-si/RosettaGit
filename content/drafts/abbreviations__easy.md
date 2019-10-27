+++
title = "Abbreviations, easy"
description = ""
date = 2019-09-18T11:31:06Z
aliases = []
[extra]
id = 21596
[taxonomies]
categories = []
tags = []
+++

{{task}}

This task is an easier (to code) variant of the Rosetta Code task: <big>   [[Abbreviations, simple]]</big>.


For this task, the following   ''command table''   will be used:
    Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy
    COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find
    NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput
    Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO
    MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT
    READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT
    RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up

<!--
The unsorted list (above) was constructed to help ensure that program entries don't get tripped up by performing a left-to-right string comparison.
!-->

<!--
For the curious amongst you (and pack-rats of old publications, this is an old XEDIT command table; the sub-commands were extracted from:
                                                         Virtual Machine/
                                                         System Product

                                                         System Product
                                                         SP Editor
                                                         Command
                                                         Language
                                                         Reference
                                                         Summary

                                                         Release 4

                                                         SX24-5122-3



                                                         Fourth Edition (December 1984)

                                                         Information herein is extracted from
                                                         Virtual Machine/System Product System Product Editor Command and Macro Reference, SC24-5221.


This old version does  not  have some of the newer commands like  QQuit  and  PQUIT,  for instance,
and those commands that deal with  DBCS  (double-byte character set).
!-->

Notes concerning the above   ''command table'':
::*   it can be thought of as one long literal string   (with blanks at end-of-lines)
::*   it may have superfluous blanks
::*   it may be in any case (lower/upper/mixed)
::*   the order of the words in the   ''command table''   must be preserved as shown
::*   the user input(s) may be in any case (upper/lower/mixed)
::*   commands will be restricted to the Latin alphabet   (A ──► Z,   a ──► z)
::*   A valid abbreviation is a word that has:
:::*   at least the minimum length of the number of capital letters of the word in the ''command table''
:::*   compares equal (regardless of case) to the leading characters of the word in the ''command table''
:::*   a length <u>not</u> longer than the word in the ''command table''
::::*   '''ALT''',   '''aLt''',   '''ALTE''',   and   '''ALTER'''   are all abbreviations of   '''ALTer'''
::::*   '''AL''',   '''ALF''',   '''ALTERS''',   '''TER''',   and   '''A'''    <u>aren't</u> valid abbreviations of   '''ALTer'''
::::*   The number of capital letters in   '''ALTer'''   indicates that any abbreviation for   '''ALTer'''   must be at least three letters
::::*   Any word longer than five characters <u>can't</u> be an abbreviation for   '''ALTer'''
::::*   '''o''',    '''ov''',    '''oVe''',    '''over''',    '''overL''',    '''overla'''   are all acceptable abbreviations for   '''Overlay'''
::*   if there isn't any lowercase letters in the word in the ''command table'',   then there isn't an abbreviation permitted

<!--
::*   a Latin alphabet is used for this Rosetta Code task because it behaves well when letters are uppercased
!-->

;Task:
::*   The command table needn't be verified/validated.
::*   Write a function to validate if the user "words"   (given as input)   are valid   (in the ''command table'').
::*   If the word   is   valid,   then return the full uppercase version of that "word".
::*   If the word        isn't     valid,   then return the lowercase string:   <big> '''*error*''' </big>       (7 characters).
::*   A blank input   (or a null input)   should return a null string.
::*   Show all output here.


;An example test case to be used for this task:
For a user string of:
  riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin
the computer program should return the string:
  RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT


;Related tasks:
:*   [[Abbreviations, simple]]
:*   [[Abbreviations, automatic]]
:*   [[Suffixation of decimal numbers]]





## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}
Uses Algol 68G specific is lower and to upper procedures. Does not use a hash table.

```algol68
# "Easy" abbreviations                                                 #
# table of "commands" - upper-case indicates the mminimum abbreviation #
STRING command table = "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy "
                     + "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find "
                     + "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput "
                     + "Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO "
                     + "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT "
                     + "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT "
                     + "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up";

# returns the next word from text, updating pos                        #
PRIO NEXTWORD = 1;
OP   NEXTWORD = ( REF INT pos, STRING text )STRING:
     BEGIN
        # skip spaces #
        WHILE IF pos > UPB text THEN FALSE ELSE text[ pos ] = " " FI DO pos +:= 1 OD;
        # get the word #
        STRING word := "";
        WHILE IF pos > UPB text THEN FALSE ELSE text[ pos ] /= " " FI DO
            word +:= text[ pos ];
            pos +:= 1
        OD;
        word
     END # NEXTWORD # ;
# returns text converted to upper case                                 #
OP   TOUPPER  = ( STRING text )STRING:
     BEGIN
        STRING result := text;
        FOR ch pos FROM LWB result TO UPB result DO
            IF is lower( result[ ch pos ] ) THEN result[ ch pos ] := to upper( result[ ch pos ] ) FI
        OD;
        result
     END # TOUPPER # ;
# returns the minimum abbreviation length of command                   #
OP   MINABLENGTH = ( STRING command )INT:
     BEGIN
        INT ab min := LWB command;
        WHILE IF ab min > UPB command THEN FALSE ELSE is upper( command[ ab min ] ) FI DO ab min +:= 1 OD;
        ab min - LWB command
     END # MINABLENGTH # ;
# searches for word in command table and returns the full command      #
# matching the possible abbreviation or *error* if there is no match   #
PRIO EXPAND  = 1;
OP   EXPAND  = ( STRING command table, word )STRING:
     IF word = ""
     THEN # empty word #
        ""
     ELSE # non-empty word #
        INT    word len    = ( UPB word + 1 ) - LWB word;
        STRING upper word := TOUPPER word;
        STRING result     := "*error*";
        INT    pos        := LWB command table;
        WHILE  STRING command := pos NEXTWORD command table;
               IF command = ""
               THEN # end of command table #
                   FALSE
               ELIF word len < MINABLENGTH command OR word len > ( ( UPB command + 1 ) - LWB command )
               THEN # word is too short or too long - try the next command #
                   TRUE
               ELIF upper word = TOUPPER command[ LWB command : ( LWB command - 1 ) + word len ]
               THEN # found the command #
                   result := TOUPPER command;
                   FALSE
               ELSE # word doexn't match - try the next command #
                   TRUE
               FI
        DO SKIP OD;
        result
     FI # EXPAND # ;
# tests the EXPAND operator                                            #
PROC test expand = ( STRING words, command table )VOID:
     BEGIN
        STRING results := "", separator := "";
        INT    pos   := LWB words;
        WHILE STRING word = pos NEXTWORD words; word /= "" DO
            results +:= separator + ( command table EXPAND word );
            separator := " "
        OD;
        print( ( "Input:  ", words, newline ) );
        print( ( "Output: ", results, newline ) )
     END # test expand # ;

# task test cases                                                      #
test expand( "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin", command table );
test expand( "",                                                                      command table )
```

{{out}}

```txt

Input:  riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin
Output: RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT
Input:
Output:

```



## Haskell



```haskell

import Data.Maybe (fromMaybe)
import Data.List (find, isPrefixOf)
import Data.Char (toUpper, isUpper)

isAbbreviationOf :: String -> String -> Bool
isAbbreviationOf abbreviation command =
  minimumPrefix `isPrefixOf` normalizedAbbreviation
  && normalizedAbbreviation `isPrefixOf` normalizedCommand
  where
    normalizedAbbreviation = map toUpper abbreviation
    normalizedCommand = map toUpper command
    minimumPrefix = takeWhile isUpper command


expandAbbreviation :: String -> String -> Maybe String
expandAbbreviation commandTable abbreviation = do
  command <- find (isAbbreviationOf abbreviation) (words commandTable)
  return $ map toUpper command


commandTable = unwords [
  "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy",
  "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find",
  "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput",
  "Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO",
  "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT",
  "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT",
  "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up"]


main :: IO ()
main = do
  input <- getLine
  let abbreviations = words input
  let commands = map (fromMaybe "*error*" . expandAbbreviation commandTable) abbreviations
  putStrLn $ unwords results

```



## Factor


```factor
USING: arrays ascii assocs combinators.short-circuit io kernel
literals math qw sequences sequences.extras splitting.extras ;
IN: rosetta-code.abbreviations-easy

CONSTANT: commands qw{
Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst
COMPress COpy COUnt COVerlay CURsor DELete CDelete Down
DUPlicate Xedit EXPand EXTract Find NFind NFINDUp NFUp CFind
FINdup FUp FOrward GET Help HEXType Input POWerinput Join SPlit
SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix
MACRO MErge MODify MOve MSG Next Overlay PARSE PREServe PURge
PUT PUTD  Query  QUIT READ  RECover REFRESH RENum REPeat
Replace CReplace  RESet  RESTore  RGTLEFT RIght LEft  SAVE  SET
SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up }

CONSTANT: user-input $[ "riG   rePEAT copies  put mo   rest    "
"types   fup.    6       poweRin" append ]

: starts-with? ( cand com -- ? ) [ >upper ] bi@ start 0 = ;
: capitals ( str -- n ) [ LETTER? ] count ;
: min-len? ( candidate command -- ? ) capitals swap length <= ;
: not-longer? ( candidate command -- ? ) [ length ] bi@ <= ;
: permitted? ( candidate command -- ? ) dup [ letter? ] count 0
    > [ [ >upper ] bi@ = ] dip or ;
    
: valid-abbr? ( candidate command -- ? )
    {
        [ permitted?   ]
        [ starts-with? ]
        [ min-len?     ]
        [ not-longer?  ]
    } 2&& ;

: find-command ( candidate -- command/f )
    commands swap [ swap valid-abbr? ] curry find nip ;
    
: process-candidate ( candidate -- abbr/error )
    find-command [ >upper ] [ "*error*" ] if* ;
    
: process-user-string ( str -- seq ) dup "" = [ drop "" ]
    [ " " split-harvest [ process-candidate ] map ] if ;
    
: .abbr ( input -- )
    [ " " split-harvest ] [ process-user-string ] bi zip
    [ first2 32 pad-longest 2array ] map
    [ keys ] [ values ] bi
    [ " " join ] bi@
    [ "User words: " write print ]
    [ "Full words: " write print ] bi* ;
    
: main ( -- ) user-input "" [ .abbr ] bi@ ;
    
MAIN: main
```

{{out}}

```txt

User words: riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin
Full words: RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT
User words:
Full words:

```



## Go

{{trans|Kotlin}}

```go
package main

import (
    "fmt"
    "strings"
)

var table =
    "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy " +
    "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find " +
    "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput " +
     "Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO " +
    "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT " +
    "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT " +
    "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up "

func validate(commands, words []string, minLens []int) []string {
    results := make([]string, 0)
    if len(words) == 0 {
        return results
    }
    for _, word := range words {
        matchFound := false
        wlen := len(word)
        for i, command := range commands {
            if minLens[i] == 0 || wlen < minLens[i] || wlen > len(command) {
                continue
            }
            c := strings.ToUpper(command)
            w := strings.ToUpper(word)
            if strings.HasPrefix(c, w) {
                results = append(results, c)
                matchFound = true
                break
            }
        }
        if !matchFound {
            results = append(results, "*error*")
        }
    }
    return results
}

func main() {
    table = strings.TrimSpace(table)
    commands := strings.Fields(table)
    clen := len(commands)
    minLens := make([]int, clen)
    for i := 0; i < clen; i++ {
        count := 0
        for _, c := range commands[i] {
            if c >= 'A' && c <= 'Z' {
                count++
            }
        }
        minLens[i] = count
    }
    sentence :=  "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
    words := strings.Fields(sentence)
    results := validate(commands, words, minLens)
    fmt.Print("user words:  ")
    for j := 0; j < len(words); j++ {
        fmt.Printf("%-*s ", len(results[j]), words[j])
    }
    fmt.Print("\nfull words:  ")
    fmt.Println(strings.Join(results, " "))
}
```


{{out}}

```txt

user words:  riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin    
full words:  RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## J

Using the expand definition as well as its dependencies from [[Abbreviations, simple#J]]
we convert this command table into the form with the abbreviation length given as a number.

```J

   COMMAND_TABLE=: noun define
Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy
   COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find
   NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput
   Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO
   MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT
   READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT
   RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up
 )
 
   'CAPA CAPZ'=:a.i.'AZ'
   CT =: (, 3 ": [: +/ (CAPA&<: *. <:&CAPZ)@:(a.&i.))&.>&.:;: CRLF -.~ COMMAND_TABLE
   user_words =: 'riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin'
   CT expand user_words
RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT
   
```



## Julia

{{works with|Julia|0.6}}
{{trans|Kotlin}}


```julia
const table =
    "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy " *
    "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find " *
    "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput " *
    "Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO " *
    "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT " *
    "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT " *
    "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up "

function validate(commands::AbstractVector{<:AbstractString},
        minlens::AbstractVector{<:Integer}, words::AbstractVector{<:AbstractString})
    r = String[]
    for word in words
        matchfound = false
        for (i, command) in enumerate(commands)
            if iszero(minlens[i]) || length(word) ∉ minlens[i]:length(command)
                continue
            end
            if startswith(lowercase(command), lowercase(word))
                push!(r, uppercase(command))
                matchfound = true
                break
            end
        end
        !matchfound && push!(r, "*error*")
    end
    return r
end

commands = split(strip(table))
minlens  = count.(isupper, commands)
sentence = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
words    = split(sentence)
result   = validate(commands, minlens, words)
println("User words: ", join(lpad.(words,  11)))
println("Full words: ", join(lpad.(result, 11)))
```


{{out}}

```txt
User words:         riG     rePEAT     copies        put         mo       rest      types       fup.          6    poweRin
Full words:       RIGHT     REPEAT    *error*        PUT       MOVE    RESTORE    *error*    *error*    *error* POWERINPUT
```



## Kotlin


```scala
// version 1.1.4-3

val r = Regex("[ ]+")

val table = 
    "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy " +
    "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find " +
    "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput " +
    "Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO " + 
    "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT " +
    "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT " +
    "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up "

fun validate(commands: List<String>, minLens: List<Int>, words: List<String>): List<String> {
    if (words.isEmpty()) return emptyList<String>()
    val results = mutableListOf<String>()
    for (word in words) {
        var matchFound = false
        for ((i, command) in commands.withIndex()) {
            if (minLens[i] == 0 || word.length !in minLens[i] .. command.length) continue 
            if (command.startsWith(word, true)) {
                results.add(command.toUpperCase())
                matchFound = true
                break
            }
        }
        if (!matchFound) results.add("*error*")
    }
    return results
}
    
fun main(args: Array<String>) {
    val commands = table.trimEnd().split(r)
    val minLens = MutableList(commands.size) { commands[it].count { c -> c.isUpperCase() } }
    val sentence = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
    val words = sentence.trim().split(r)
    val results = validate(commands, minLens, words)  
    print("user words:  ")
    for (j in 0 until words.size) print("${words[j].padEnd(results[j].length)} ")
    print("\nfull words:  ")
    for (j in 0 until results.size) print("${results[j]} ")
    println()
}
```


{{out}}

```txt

user words:  riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin    
full words:  RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT 

```




## OCaml



```ocaml
let cmds = "\
  Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy
  COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find
  NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput
  Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO
  MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT
  READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT
  RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up"

let user =
  "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"

let char_is_uppercase c =
  match c with
  | 'A'..'Z' -> true
  | _ -> false

let get_abbr s =
  let seq = String.to_seq s in
  let seq = Seq.filter char_is_uppercase seq in
  (String.of_seq seq)

let () =
  let cmds = Str.split (Str.regexp "[ \r\n]+") cmds in
  let cmds =
    List.map (fun s ->
      get_abbr s,
      String.uppercase_ascii s
    ) cmds
  in
  let user = Str.split (Str.regexp "[ \r\n]+") user in
  let r =
    List.map (fun ucmd ->
      let n = String.length ucmd in
      let find_abbr (abbr, cmd) =
        let na = String.length abbr in
        let nc = String.length cmd in
        if n < na || nc < n then false else
          let sub = String.sub cmd 0 n in
          (sub = String.uppercase_ascii ucmd)
      in
      match List.find_opt find_abbr cmds with
      | Some (_, found) -> found
      | None -> "*error*"
    ) user
  in
  print_endline (String.concat " " r)
```


{{out}}


```txt
$ ocaml str.cma abbr.ml
RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## Perl

{{trans|Perl 6}}

```perl
@c = (join ' ', qw<
Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy
COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find
NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput
Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO
MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT
READ  RECover REFRESH RENum Replace REPeat  CReplace  RESet  RESTore  RGTLEFT
RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up
>) =~ /([A-Z]+)([a-z]*)(?:\s+|$)/g;

my %abr = ('' => '', ' ' => '');
for ($i = 0; $i < @c; $i += 2) {
    $sl = length($s =    $c[$i]  );
    $ll = length($l = uc $c[$i+1]);
    $abr{$s} = $w = $s.$l;
    map { $abr{substr($w, 0, $_)} = $w } $sl .. $ll;
    $abr{$w} = $w; # full command should always work
}

$fmt = "%-10s";
$inp = sprintf $fmt, 'Input:';
$out = sprintf $fmt, 'Output:';
for $str ('', qw<riG rePEAT copies put mo rest types fup. 6 poweRin>) {
    $inp .= sprintf $fmt, $str;
    $out .= sprintf $fmt, $abr{uc $str} // '*error*';
}

print "$inp\n$out\n"
```

{{out}}

```txt
Input:              riG       rePEAT    copies    put       mo        rest      types     fup.      6         poweRin
Output:             RIGHT     REPEAT    *error*   PUT       MOVE      RESTORE   *error*   *error*   *error*   POWERINPUT

```



## Perl 6

{{works with|Rakudo|2017.08}}

Demonstrate that inputting an empty string returns an empty string in addition to the required test input.


```perl6
<
Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy
COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find
NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput
Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO
MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT
READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT
RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up
> ~~ m:g/ ((<.:Lu>+) <.:Ll>*) /;

my %abr = '' => '', |$/.map: {
    my $abbrv = .[0].Str.fc;
    |map { $abbrv.substr( 0, $_ ) => $abbrv.uc },
    .[0][0].Str.chars .. $abbrv.chars
};

sub abbr-easy ( $str ) { %abr{$str.trim.fc} // '*error*' }

# Testing
for 'riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin', '' -> $str {
    put ' Input: ', $str;
    put 'Output: ', join ' ', $str.words.map: *.&abbr-easy;
}
```

{{out}}

```txt
 Input: riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin
Output: RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT
 Input: 
Output: 
```



## Phix


```Phix
constant abbrtxt = """
   Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy
   COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find
   NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput
   Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO
   MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT
   READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT
   RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up
""",
input = "riG   rePEAT copies  put mo   rest    types   fup.    6   poweRin"

function set_min_lengths(sequence a)
    sequence res = {}
    for i=1 to length(a) do
        string ai = a[i], uai = upper(ai)
        for j=-length(ai) to 0 do
            if j=0 or ai[j]!=uai[j] then
                res = append(res,{ai,length(ai)+j})
                exit
            end if
        end for
    end for
    return res
end function

constant abbrevs = set_min_lengths(split(substitute(abbrtxt,"\n"," "),no_empty:=true))
constant inputs = split(input,no_empty:=true)

for i=1 to length(inputs) do
    string ii = inputs[i],
           res = "*error*"
    for j=1 to length(abbrevs) do
        {string aj, integer l} = abbrevs[j]
        if length(ii)>=l
        and match(ii,aj,case_insensitive:=true)==1 then
            res = upper(aj)
            exit
        end if
    end for
    puts(1,res&" ")
end for
puts(1,"\n")
```

{{out}}

```txt

RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## Python

{{works with|Python|3.6}}

```python


command_table_text = """Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy
   COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find
   NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput
   Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO
   MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT
   READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT
   RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up"""

user_words = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"


def find_abbreviations_length(command_table_text):
    """ find the minimal abbreviation length for each word by counting capital letters.
        a word that does not have capital letters
        gets it's full lengths as the minimum.
    """
    command_table = dict()
    for word in command_table_text.split():
        abbr_len = sum(1 for c in word if c.isupper())
        if abbr_len == 0:
            abbr_len = len(word)
        command_table[word] = abbr_len
    return command_table


def find_abbreviations(command_table):
    """ for each command insert all possible abbreviations"""
    abbreviations = dict()
    for command, min_abbr_len in command_table.items():
        for l in range(min_abbr_len, len(command)+1):
            abbr = command[:l].lower()
            abbreviations[abbr] = command.upper()
    return abbreviations


def parse_user_string(user_string, abbreviations):
    user_words = [word.lower() for word in user_string.split()]
    commands = [abbreviations.get(user_word, "*error*") for user_word in user_words]
    return " ".join(commands)


command_table = find_abbreviations_length(command_table_text)
abbreviations_table = find_abbreviations(command_table)

full_words = parse_user_string(user_words, abbreviations_table)

print("user words:", user_words)
print("full words:", full_words)

```



## Racket



```racket
#lang racket

(define command-string
  "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy
   COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find
   NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput
   Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO
   MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT
   READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT
   RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up")

(define input-string
  "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin")

(define (make-command command)
  (cons (string-upcase command)
        (list->string (takef (string->list command) char-upper-case?))))

(define commands (map make-command (string-split command-string)))

(string-join
 (for/list ([s (in-list (string-split input-string))])
   (define up (string-upcase s))
   (or (for/or ([command (in-list commands)])
         (match-define (cons full-command abbrev) command)
         (and (string-prefix? up abbrev)
              (string-prefix? full-command up)
              full-command))
       "*error*"))
 " ")
```


{{out}}

```txt

"RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT"

```



## REXX

<lang>/*REXX program validates a user  "word"  against a  "command table"  with abbreviations.*/
parse arg uw                                     /*obtain optional arguments from the CL*/
if uw=''  then uw= 'riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin'
say 'user words: '   uw

@= 'Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy'   ,
   'COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find'   ,
   'NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput'   ,
   'Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO'   ,
   'MErge MOve MODify MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT'   ,
   'READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT'   ,
   'RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up'

say 'full words: '   validate(uw)                /*display the result(s) to the terminal*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
validate: procedure expose @;  arg x;  upper @   /*ARG   capitalizes all the  X  words. */
          $=                                     /*initialize the return string to null.*/
             do j=1  to words(x);   _=word(x, j) /*obtain a word from the     X  list.  */
               do k=1  to words(@); a=word(@, k) /*get a legitimate command name from @.*/
               L=verify(_, 'abcdefghijklmnopqrstuvwxyz', "M")  /*maybe get abbrev's len.*/
               if L==0  then L=length(_)         /*0?  Command name can't be abbreviated*/
               if abbrev(a, _, L)   then do; $=$ a;  iterate j;  end  /*is valid abbrev?*/
               end   /*k*/
             $=$ '*error*'                       /*processed the whole list, not valid. */
             end     /*j*/
          return strip($)                        /*elide the superfluous leading blank. */
```

{{out|output|text=  when using the default input:}}

```txt

user words:  riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin
full words:  RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## Ruby



```ruby
#!/usr/bin/env ruby

cmd_table = File.read(ARGV[0]).split
user_str = File.read(ARGV[1]).split

user_str.each do |abbr|
  candidate = cmd_table.find do |cmd|
    cmd.count('A-Z') <= abbr.length && abbr.casecmp(cmd[0...abbr.length]).zero?
  end

  print candidate.nil? ? '*error*' : candidate.upcase

  print ' '
end

puts

```

{{out}}

```txt

$ ./abbreviations_easy.rb cmd_table user_str
RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## Rust



```rust
use std::collections::HashMap;

fn main() {
    let commands = "
        Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy \
        COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find \
        NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput \
        Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO \
        MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT \
        READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT \
        RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up \
    ";
    let split = commands.split_ascii_whitespace();
    let count_hashtable: HashMap<&str, usize> = split.map(|word| {
        (word, word.chars().take_while(|c| c.is_ascii_uppercase()).count())
    }).collect();

    let line = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin";
    let mut words_vec: Vec<String> = vec![];
    for word in line.split_ascii_whitespace() {
        let split = commands.split_ascii_whitespace();
        let abbr = split.filter(|x| {
            x.to_ascii_lowercase().starts_with(&word.to_ascii_lowercase()) &&
            word.len() >= *count_hashtable.get(x).unwrap()
        }).next();
        words_vec.push(match abbr {
            Some(word) => word.to_ascii_uppercase(),
            None => String::from("*error*"),
        });
    }
    let corrected_line = words_vec.join(" ");
    println!("{}", corrected_line);
}

```

{{out}}

```txt

RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## Scala


```Scala

object Main extends App {
    implicit class StrOps(i: String) {
        def isAbbreviationOf(target: String): Boolean = {
            @scala.annotation.tailrec
            def checkPAsPrefixOfM(p: List[Char], m: List[Char]): Boolean = (p, m) match {
                case (Nil, _) => true //prefix empty
                case (_, Nil) => false //main string empty
                case (ph :: pt, mh :: mt) if ph.toUpper == mh.toUpper => checkPAsPrefixOfM(pt, mt) //case insensitive match of head characters
                case _ => false
            }
            i.length >= target.count(_.isUpper) && checkPAsPrefixOfM(i.toList, target.toList)
        }
    }

    val commands = """
              |Add ALTer BAckup Bottom CAppend Change SCHANGE CInsert CLAst COMPress COpy
              |COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find
              |NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput
              |Join SPlit SPLTJOIN LOAD Locate CLocate LOWercase UPPercase LPrefix MACRO
              |MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD Query QUIT
              |READ RECover REFRESH RENum REPeat Replace CReplace RESet RESTore RGTLEFT
              |RIght LEft SAVE SET SHift SI SORT SOS STAck STATus TOP TRAnsfer Type Up
      """.stripMargin.replace("\n", " ").trim.split(" ")

    val input = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin".split(" ").filter(!_.isEmpty)

    val resultLine = input.map{ i =>
        commands.find(c => i.isAbbreviationOf(c)).map(_.toUpperCase).getOrElse("*error*")
    }.mkString(" ")

    println(resultLine)
}

```

{{out}}

```txt

RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## Tcl


```tcl

proc appendCmd {word} {
  # Procedure to append the correct command from the global list ::cmds
  # for the word given as parameter to the global list ::result.
  # If a matching word has been found and appended to ::result, this procedure
  # behaves like a "continue" statement, causing the loop containing it to
  # jump over the rest of the body.
  set candidates [lsearch -inline -all -nocase -glob $::cmds "${word}*"]
  foreach cand $candidates {
    if {[string length $word] >= $::minLen($cand)} {
      lappend ::result [string toupper $cand]
      return -code continue
    }
  }
}

set cmds {Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy
   COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find
   NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput
   Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO
   MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT
   READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT
   RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up}

# Find the minimum lengths necessary for each command.
foreach c $cmds {
  regexp {^[A-Z]+} $c match
  set minLen($c) [string length $match]
}

set words {riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin}
set result {}

foreach w $words {
  appendCmd $w
  lappend result *error*
}

puts $result

```

{{out}}

```txt

RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## VBA


```vb
Private Function ValidateUserWords(userstring As String) As String
    Dim s As String
    Dim user_words() As String
    Dim command_table As Scripting.Dictionary
    Set command_table = New Scripting.Dictionary
    Dim abbreviations As Scripting.Dictionary
    Set abbreviations = New Scripting.Dictionary
    abbreviations.CompareMode = TextCompare
    Dim commandtable() As String
    Dim commands As String
    s = s & "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy "
    s = s & "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find "
    s = s & "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput "
    s = s & "Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO "
    s = s & "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT "
    s = s & "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT "
    s = s & "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up "
    commandtable = Split(s, " ")
    Dim i As Integer
    For Each word In commandtable
        If Len(word) > 0 Then
            i = 1
            Do While Mid(word, i, 1) >= "A" And Mid(word, i, 1) <= "Z"
                i = i + 1
            Loop
            command_table.Add Key:=word, Item:=i - 1
        End If
    Next word
    For Each word In command_table
        For i = command_table(word) To Len(word)
            On Error Resume Next
            abbreviations.Add Key:=Left(word, i), Item:=UCase(word)
        Next i
    Next word
    user_words() = Split(userstring, " ")
    For Each word In user_words
        If Len(word) > 0 Then
            If abbreviations.exists(word) Then
                commands = commands & abbreviations(word) & " "
            Else
                commands = commands & "*error* "
            End If
        End If
    Next word
    ValidateUserWords = commands
End Function
Public Sub program()
    Dim guserstring As String
    guserstring = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
    Debug.Print "user words:", guserstring
    Debug.Print "full words:", ValidateUserWords(guserstring)
End Sub
```
{{out}}
```txt
user words:   riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin
full words:   RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT 
```



## Yabasic


```Yabasic
data "Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy"
data "COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find"
data "NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput"
data "Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO"
data "MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT"
data "READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT"
data "RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up"
data ""

dim abrev$(1)

do
    read a$
    if a$ = "" break
    s$ = s$ + " " + a$
loop

size = token(s$, abrev$())

do
    input "Input abbreviation: " a$
    l1 = len(a$)
    
    if l1 = 0 break
    test = false
    for i = 1 to size
        l2 = uppers(abrev$(i))
        if lower$(left$(abrev$(i), l1)) = lower$(left$(a$, l1)) and l1 >= l2 then
            print upper$(abrev$(i))
            test = true
        end if
    next
    if not test print "*error*"
loop

sub uppers(s$)
    local l, i, c$, n
    
    l = len(s$)
    for i = 1 to l
        c$ = mid$(s$, i, 1)
        if c$ >= "A" and c$ <= "Z" n = n + 1
    next
    return n
end sub

```



## zkl

Rather more brute force than I'd like but hashing the command table is 
just too much code. And the table is so small...

```zkl
commands:=Data(0,String,	// "Add\0ALTer\0..."
#<<<
"Add ALTer  BAckup Bottom  CAppend Change SCHANGE  CInsert CLAst COMPress COpy
 COUnt COVerlay CURsor DELete CDelete Down DUPlicate Xedit EXPand EXTract Find
 NFind NFINDUp NFUp CFind FINdup FUp FOrward GET Help HEXType Input POWerinput
 Join SPlit SPLTJOIN  LOAD  Locate CLocate  LOWercase UPPercase  LPrefix MACRO
 MErge MODify MOve MSG Next Overlay PARSE PREServe PURge PUT PUTD  Query  QUIT
 READ  RECover REFRESH RENum REPeat  Replace CReplace  RESet  RESTore  RGTLEFT
 RIght LEft  SAVE  SET SHift SI  SORT  SOS  STAck STATus  TOP TRAnsfer Type Up "
.split());
#<<<

testText:=" riG   rePEAT copies  put mo   rest    types "
	  "fup.    6       poweRin";

testText.split().apply('wrap(word){ 
   sz,w := word.len(),word + "*";
   foreach c in (commands){	// rather inelegant but gotta ignore case
      // check for length requirement and, if there, verify
      if(c.matches(w) and sz>=(c-"abcdefghijklmnopqrstuvwxyz").len())
	 return(c.toUpper());
   }
   "*error*"
.concat(" ").println();
```

{{out}}

```txt

RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```

