+++
title = "Abbreviations, simple"
description = ""
date = 2019-10-20T07:16:58Z
aliases = []
[extra]
id = 21595
[taxonomies]
categories = []
tags = []
+++

{{task}}

The use of   abbreviations    (also sometimes called synonyms, nicknames, AKAs, or aliases)    can be an 

easy way to add flexibility when specifying or using commands, sub─commands, options, etc.

<!--
(AKA =  also known as)
!-->

For this task, the following   ''command table''   will be used:
    add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
    compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
    3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
    forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
    locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
    msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
    refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
    2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1

<!--
The unsorted list (above) was constructed to help ensure that program entries don't get tripped up by performing a straight left-to-right string comparison.
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
::*   a command is followed by an optional number, which indicates the minimum abbreviation
::*   A valid abbreviation is a word that has:
:::*   at least the minimum length of the word's minimum number in the ''command table''
:::*   compares equal (regardless of case) to the leading characters of the word in the ''command table''
:::*   a length <u>not</u> longer than the word in the ''command table''
::::*   '''ALT''',   '''aLt''',   '''ALTE''',   and   '''ALTER'''   are all abbreviations of   '''ALTER 3'''
::::*   '''AL''',   '''ALF''',   '''ALTERS''',   '''TER''',   and   '''A'''    <u>aren't</u> valid abbreviations of   '''ALTER 3'''
::::*   The   '''3'''   indicates that any abbreviation for   '''ALTER'''   must be at least three characters
::::*   Any word longer than five characters <u>can't</u> be an abbreviation for   '''ALTER'''
::::*   '''o''',    '''ov''',    '''oVe''',    '''over''',    '''overL''',    '''overla'''   are all acceptable abbreviations for   '''overlay 1'''
::*   if there isn't a number after the command,   then there isn't an abbreviation permitted

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
:*   [[Abbreviations, easy]]
:*   [[Abbreviations, automatic]]
:*   [[Suffixation of decimal numbers]]





## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}

```algol68
# "Simple" abbreviations                                               #

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
# returns text converted to an INT or -1 if text is not a number         #
OP   TOINT    = ( STRING text )INT:
     BEGIN
        INT  result     := 0;
        BOOL is numeric := TRUE;
        FOR ch pos FROM UPB text BY -1 TO LWB text WHILE is numeric DO
            CHAR c = text[ ch pos ];
            is numeric := ( c >= "0" AND c <= "9" );
            IF is numeric THEN ( result *:= 10 ) +:= ABS c - ABS "0" FI        
        OD;
        IF is numeric THEN result ELSE -1 FI
     END # TOINT # ;
# returns the length of word                                             #
OP LENGTH = ( STRING word )INT: 1 + ( UPB word - LWB word );
# counts the number of commands in commands                              #
PROC count commands = ( STRING commands )INT:
     BEGIN
        INT    result         := 0;
        INT    pos            := LWB commands;
        WHILE  STRING command := pos NEXTWORD commands; command /= "" DO
            IF TOINT command < 0 THEN
                # not an abbreviation length                             #
                result +:= 1
            FI
        OD;
        result
     END # count commands # ;

# list of "commands" -  the words are optionally followed by the minimum #
#                       length of abbreviation - if there isn't a number #
#                       the command can only appear in full              #
STRING commands
   = "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 "
   + "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate "
   + "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 "
   + "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load "
   + "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 "
   + "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 "
   + "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left "
   + "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 "
   ;
# build the tables of the commands and their minimum lengths             #
PROC load commands = ( STRING commands )VOID:
     BEGIN
        INT    cmd pos        := 0;
        INT    pos            := LWB command table;
        WHILE  STRING command := pos NEXTWORD commands; command /= "" DO
            INT len := TOINT command;
            IF len >= 0 THEN
                # have an abbreviation length                            #
                IF cmd pos > 0 THEN min abbreviation[ cmd pos ] := len FI
            ELSE
                # new command                                            #
                cmd pos +:= 1;
                command table[    cmd pos ] := TOUPPER command;
                min abbreviation[ cmd pos ] := LENGTH  command
            FI
        OD
     END # load commands # ;
# searches for word in command table and returns the full command      #
# matching the possible abbreviation or *error* if there is no match   #
OP   EXPAND  = ( STRING word )STRING:
     IF word = ""
     THEN # empty word #
        ""
     ELSE # non-empty word #
        INT    word len    = LENGTH word;
        STRING upper word := TOUPPER word;
        STRING result     := "*error*";
        FOR cmd pos FROM LWB command table TO UPB command table
        WHILE  STRING command := command table[ cmd pos ];
               IF word len < min abbreviation[ cmd pos ] OR word len > LENGTH command
               THEN # word is too short or too long - try the next command #
                   TRUE
               ELIF upper word = command[ LWB command : ( LWB command - 1 ) + word len ]
               THEN # found the command #
                   result := command;
                   FALSE
               ELSE # word doexn't match - try the next command #
                   TRUE
               FI
        DO SKIP OD;
        result
     FI # EXPAND # ;

# tests the EXPAND operator                                            #
PROC test expand = ( STRING words )VOID:
     BEGIN
        STRING results := "", separator := "";
        INT    pos   := LWB words;
        WHILE STRING word = pos NEXTWORD words; word /= "" DO
            results +:= separator + EXPAND word;
            separator := " "
        OD;
        print( ( "Input:  ", words, newline ) );
        print( ( "Output: ", results, newline ) )
     END # test expand # ;

# build the command table                                              #
[ 1 : count commands( commands ) ]STRING command table;
[ 1 : UPB command table          ]INT    min abbreviation;
load commands( commands );

# task test cases                                                      #
test expand( "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin" )
```

{{out}}

```txt

Input:  riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin
Output: RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## Crystal


```crystal
COMMAND_TABLE = 
  "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
   compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
   3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
   forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
   locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
   msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
   refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
   2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1"

def parse_command_table(input : String)
    cmds = {} of String => String
    table = input.strip.split

    word = table[0].upcase
    table.each do |item|
        if /[0-9]+/.match(item)
            abbreviation_length = item.to_i
            (0..word.size-abbreviation_length).each do |i|
                cmds[word[(0..abbreviation_length-1+i)]] = word
            end
        else
            word = item.upcase
            cmds[word] = word
        end
    end
    return cmds
end

def parse_user_input(input : String?, commands)
    output = ""
    unless input.nil? 
        user_commands = input.strip.split
        user_commands.each do |command|
            command = command.upcase
            if commands.has_key?(command)
                output += commands[command]
            else
                output += "*error*"
            end
            output += " "
        end
    end
    return output
end

cmds = parse_command_table(COMMAND_TABLE)
puts "Input:"
user_input = gets
puts "Output:"
puts parse_user_input(user_input, cmds)

```


{{out}}

```txt

Input:
riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin
Output:
RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT 

```



## Factor


```factor
USING: arrays assocs combinators formatting fry grouping.extras
kernel literals math math.parser multiline sequences
splitting.extras unicode ;
IN: rosetta-code.abbr-simple

CONSTANT: input $[
"riG   rePEAT copies  put mo   rest    types   fup.    6       "
"poweRin" append
]

<<     ! Make the following two words available at parse time.

: abbr-pair ( first second -- seq/f )
    {
        { [ 2dup drop [ digit? ] all? ] [ 2drop f ] }
        {
            [ 2dup nip [ Letter? ] all? ]
            [ drop >upper 0 2array ]
        }
        [ [ >upper ] [ string>number ] bi* 2array ]
    } cond ;

: parse-commands ( seq -- seq )
    " \n" split-harvest [ abbr-pair ] 2clump-map sift ;

>>

CONSTANT: commands $[
HEREDOC: END
add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange
Cinsert 2  Clast 3 compress 4 copy 2 count 3 Coverlay 3 cursor 3
delete 3 Cdelete 2  down 1  duplicate 3 xEdit 1 expand 3 extract
3 find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 forward
2  get  help 1  hexType 4  input 1 powerInput 3  join  1 split 2
spltJOIN load locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix
2  macro  merge 2  modify  3  move 2 msg  next 1 overlay 1 parse
preserve 4 purge 3 put putD query 1 quit  read recover 3 refresh
renum 3 repeat 3 replace 1 Creplace  2 reset 3 restore 4 rgtLEFT
right 2 left 2  save  set  shift 2  si  sort sos  stack 3 status
4 top  transfer 3  type 1  up 1
END
parse-commands
]

: valid-abbrevs ( assoc seq -- assoc )
    dup '[ [ _ head? ] [ _ length <= ] bi* and ] assoc-filter ;

: find-command ( seq -- seq )
    >upper [ commands ] dip valid-abbrevs
    [ "*error*" ] [ first first ] if-empty ;

: (find-commands) ( seq -- seq )
    " " split-harvest [ find-command ] map " " join ;

: find-commands ( seq -- seq )
    dup empty? not [ (find-commands) ] when ;

: show-commands ( seq -- )
    dup find-commands " Input: \"%s\"\nOutput: \"%s\"\n" printf
    ;

: main ( -- ) input "" [ show-commands ] bi@ ;

MAIN: main
```

{{out}}

```txt

 Input: "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
Output: "RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT"
 Input: ""
Output: ""

```



## Forth


Works with any ANS Forth 

Needs the FMS-SI (single inheritance) library code located here:
http://soton.mpeforth.com/flag/fms/index.html

```forth
include FMS-SI.f
include FMS-SILib.f

${ add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
   compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
   3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
   forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
   locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
   msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
   refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
   2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 } 
   value list  ' upper: list map:

: compare' { adr len $obj -- f }
  len $obj size: > if false exit then
  adr len $obj @: drop len compare 0= ;

: <= ( n1 n2 = f) 1+ swap > ;     

: abbrev 0 0 { adr len obj1 obj2 -- }
  list uneach:
  list each: drop to obj1 
  begin
   list each: 
  while
   to obj2
   obj2 @: >integer
     if  \ word followed by a number
         len <= if adr len obj1 compare'
                   if obj1 p: exit then
                then list each: if to obj1  else ." *error* " exit then
     else \ word not followed by a number
          adr len obj1 @: compare 0=
          if obj1 p: exit then
          obj2 to obj1
     then 
  repeat ; 

${ riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin }
  value input-list  ' upper: input-list map:

: valid-input ( adr len -- f)
  over + swap do i c@ isalpha 0= if ." *error* " unloop false exit then loop true ;

: run
  begin
    input-list each:
  while
    dup @: valid-input
    if @: abbrev space else drop then
  repeat ;


run RIGHT REPEAT *error*  PUT MOVE RESTORE *error*  *error* *error* POWERINPUT ok


```



## Go

{{trans|Kotlin}}

```go
package main

import (
	"io"
	"os"
	"strconv"
	"strings"
	"text/tabwriter"
)

func readTable(table string) ([]string, []int) {
	fields := strings.Fields(table)
	var commands []string
	var minLens []int

	for i, max := 0, len(fields); i < max; {
		cmd := fields[i]
		cmdLen := len(cmd)
		i++

		if i < max {
			num, err := strconv.Atoi(fields[i])
			if err == nil && 1 <= num && num < cmdLen {
				cmdLen = num
				i++
			}
		}

		commands = append(commands, cmd)
		minLens = append(minLens, cmdLen)
	}

	return commands, minLens
}

func validateCommands(commands []string, minLens []int, words []string) []string {
	var results []string
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

func printResults(words []string, results []string) {
	wr := tabwriter.NewWriter(os.Stdout, 0, 1, 1, ' ', 0)
	io.WriteString(wr, "user words:")
	for _, word := range words {
		io.WriteString(wr, "\t"+word)
	}
	io.WriteString(wr, "\n")
	io.WriteString(wr, "full words:\t"+strings.Join(results, "\t")+"\n")
	wr.Flush()
}

func main() {
	const table = "" +
		"add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 " +
		"compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate " +
		"3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 " +
		"forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load " +
		"locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 " +
		"msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 " +
		"refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left " +
		"2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 "

	const sentence = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"

	commands, minLens := readTable(table)
	words := strings.Fields(sentence)

	results := validateCommands(commands, minLens, words)

	printResults(words, results)
}

```


{{out}}

```txt

user words:  riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin    
full words:  RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## Haskell

{{Trans|Python}}

```haskell
import Data.List (find, isPrefixOf)
import Data.Char (isDigit, toUpper)
import Data.Maybe (maybe)

withExpansions :: [(String, Int)] -> String -> String
withExpansions tbl s = unwords $ expanded tbl <$> words s

expanded :: [(String, Int)] -> String -> String
expanded tbl k = maybe "*error" fst (expand k)
  where
    expand [] = Just ([], 0)
    expand s =
      let u = toUpper <$> s
          lng = length s
      in find (\(w, n) -> lng >= n && isPrefixOf u w) tbl

cmdsFromString :: String -> [(String, Int)]
cmdsFromString s =
  let go w@(x:_) (xs, n)
        | isDigit x = (xs, read w :: Int)
        | otherwise = ((toUpper <$> w, n) : xs, 0)
  in fst $ foldr go ([], 0) (words s)

-- TESTS --------------------------------------------------
table :: [(String, Int)]
table =
  cmdsFromString
    "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1 \
    \Schange Cinsert 2  Clast 3 compress 4 copy 2 count 3 Coverlay 3 \
    \cursor 3  delete 3 Cdelete 2  down 1  duplicate 3 xEdit 1 expand 3 \
    \extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 \
    \forward 2  get  help 1 hexType 4 input 1 powerInput 3  join 1 \
    \split 2 spltJOIN load locate 1 Clocate 2 lowerCase 3 upperCase 3 \
    \Lprefix 2  macro  merge 2 modify 3 move 2 msg  next 1 overlay 1 \
    \parse preserve 4 purge 3 put putD query 1 quit read recover 3 \
    \refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 \
    \rgtLEFT right 2 left 2  save  set  shift 2  si  sort  sos stack 3 \
    \status 4 top  transfer 3  type 1  up 1"

main :: IO ()
main = do
  let unAbbrev = withExpansions table
  print $
    unAbbrev
      "riG   rePEAT copies  put mo   rest    types   fup.    6      poweRin"
  print $ unAbbrev ""
```

{{Out}}

```txt
"RIGHT REPEAT *error PUT MOVE RESTORE *error *error *error POWERINPUT"
""
```



## J

Warning: http://rosettacode.org/wiki/Abbreviations,_easy#J uses this code without duplication.  Must provide expand with the same signature.  x is the command table string, y is the user sentence.

```J

range=:<.+[:i.>.-<.
assert 2 3 4 -: 2 range 5
assert (i.0) -: 3 range 3

abbreviate =: 3 :0
 NB. input is the abbreviation table
 NB. output are the valid abbreviations
 y =. toupper CRLF -.~ y
 y =. ;: y
 y =. (([: (,3":#)L:_1 {)`[`]}~ ([: I. [: -. {.@e.&Num_j_&>)) y
 y =. [&.:(;:inv) y
 y =. _2 ({. , <./@:".&.>@:{:)\ y
 y =. (<@] , ((range #) <@{."0 _ ]))&.>~/"1 y
 ambiguities =. ;#~1<[:+/e.
 (([: ~. {.@:[ , -.)L:_1 <@:ambiguities) y
)
assert ('ABC A AB' ,&(<@;:) 'ABCDE ABCD') -: abbreviate 'abc 1 abcde 3'
assert ('ABC A AB' ,&(<@;:) 'ABCDE') -: abbreviate 'abc 1 abcde'


expand =: dyad define
 a =. abbreviate x
 words =. <;._2 ' ' ,~ deb toupper y
 interval_index =. <: +/\ #&> a
 a =. a , <,<'*error*'
 ;:inv {.&> (interval_index I.(; a )i."_ 0 words){ a
)

```



```txt

   command_table =: 0 :0
add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
   compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
   3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
   forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
   locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
   msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
   refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
   2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1
)

   user_words =: 'riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin'

   command_table expand user_words
RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## JavaScript

{{Trans|Haskell}}
{{Trans|Python}}

```javascript
(() => {
    'use strict';

    // withExpansions :: [(String, Int)] -> String -> String
    const withExpansions = tbl => s =>
        unwords(map(expanded(tbl), words(s)));

    // expanded :: [(String, Int)] -> String -> String
    const expanded = tbl => s => {
        const
            lng = s.length,
            u = toUpper(s),
            p = wn => {
                const [w, n] = Array.from(wn);
                return lng >= n && isPrefixOf(u, w);
            }
        return maybe(
            '*error*',
            fst,
            0 < lng ? (
                find(p, tbl)
            ) : Just(Tuple([], 0))
        );
    };

    // cmdsFromString :: String -> [(String, Int)]
    const cmdsFromString = s =>
        fst(foldr(
            (w, a) => {
                const [xs, n] = Array.from(a);
                return isDigit(head(w)) ? (
                    Tuple(xs, parseInt(w, 10))
                ) : Tuple(
                    [Tuple(toUpper(w), n)].concat(xs),
                    0
                );
            },
            Tuple([], 0),
            words(s)
        ));

    // TEST -----------------------------------------------
    const main = () => {

        // table :: [(String, Int)]
        const table = cmdsFromString(
            `add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1
            Schange Cinsert 2  Clast 3 compress 4 copy 2 count 3 Coverlay 3
            cursor 3  delete 3 Cdelete 2  down 1  duplicate 3 xEdit 1 expand 3
            extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
            forward 2  get  help 1 hexType 4 input 1 powerInput 3  join 1
            split 2 spltJOIN load locate 1 Clocate 2 lowerCase 3 upperCase 3
            Lprefix 2  macro  merge 2 modify 3 move 2 msg  next 1 overlay 1
            parse preserve 4 purge 3 put putD query 1 quit read recover 3
            refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4
            rgtLEFT right 2 left 2  save  set  shift 2  si  sort  sos stack 3
            status 4 top  transfer 3  type 1  up 1`
        );

        return fTable(
            'Abbreviation tests:\n',
            s => "'" + s + "'",
            s => "\n\t'" + s + "'",
            withExpansions(table),
            [
                'riG   rePEAT copies  put mo   rest    types   fup.    6      poweRin',
                ''
            ]
        );
    };

    // GENERIC FUNCTIONS ----------------------------------

    // Just :: a -> Maybe a
    const Just = x => ({
        type: 'Maybe',
        Nothing: false,
        Just: x
    });

    // Nothing :: Maybe a
    const Nothing = () => ({
        type: 'Maybe',
        Nothing: true,
    });

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (f, g) => x => f(g(x));

    // find :: (a -> Bool) -> [a] -> Maybe a
    const find = (p, xs) => {
        for (let i = 0, lng = xs.length; i < lng; i++) {
            if (p(xs[i])) return Just(xs[i]);
        }
        return Nothing();
    };

    // flip :: (a -> b -> c) -> b -> a -> c
    const flip = f =>
        1 < f.length ? (
            (a, b) => f(b, a)
        ) : (x => y => f(y)(x));

    // foldl1 :: (a -> a -> a) -> [a] -> a
    const foldl1 = (f, xs) =>
        1 < xs.length ? xs.slice(1)
        .reduce(f, xs[0]) : xs[0];

    // foldr :: (a -> b -> b) -> b -> [a] -> b
    const foldr = (f, a, xs) => xs.reduceRight(flip(f), a);

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // fTable :: String -> (a -> String) ->
    //                     (b -> String) -> (a -> b) -> [a] -> String
    const fTable = (s, xShow, fxShow, f, xs) => {
        // Heading -> x display function ->
        //           fx display function ->
        //    f -> values -> tabular string
        const
            ys = map(xShow, xs),
            w = maximum(map(length, ys)),
            rows = zipWith(
                (a, b) => justifyRight(w, ' ', a) + ' -> ' + b,
                ys,
                map(compose(fxShow, f), xs)
            );
        return s + '\n' + unlines(rows);
    };

    // head :: [a] -> a
    const head = xs => xs.length ? xs[0] : undefined;

    // isDigit :: Char -> Bool
    const isDigit = c => {
        const n = ord(c);
        return 48 <= n && 57 >= n;
    };

    // isPrefixOf takes two lists or strings and returns
    // true iff the first is a prefix of the second.

    // isPrefixOf :: [a] -> [a] -> Bool
    // isPrefixOf :: String -> String -> Bool
    const isPrefixOf = (xs, ys) => {
        const go = (xs, ys) => {
            const intX = xs.length;
            return 0 < intX ? (
                ys.length >= intX ? xs[0] === ys[0] && go(
                    xs.slice(1), ys.slice(1)
                ) : false
            ) : true;
        };
        return 'string' !== typeof xs ? (
            go(xs, ys)
        ) : ys.startsWith(xs);
    };

    // justifyRight :: Int -> Char -> String -> String
    const justifyRight = (n, cFiller, s) =>
        n > s.length ? (
            s.padStart(n, cFiller)
        ) : s;

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

    // maybe :: b -> (a -> b) -> Maybe a -> b
    const maybe = (v, f, m) =>
        m.Nothing ? v : f(m.Just);

    // ord :: Char -> Int
    const ord = c => c.codePointAt(0);

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        'GeneratorFunction' !== xs.constructor.constructor.name ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // toUpper :: String -> String
    const toUpper = s => s.toLocaleUpperCase();

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // words :: String -> [String]
    const words = s => s.split(/\s+/);

    // Use of `take` and `length` here allows zipping with non-finite lists
    // i.e. generators like cycle, repeat, iterate.

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const
            lng = Math.min(length(xs), length(ys)),
            as = take(lng, xs),
            bs = take(lng, ys);
        return Array.from({
            length: lng
        }, (_, i) => f(as[i], bs[i], i));
    };

    // MAIN ---
    return main();
})();
```

{{Out}}

```txt
Abbreviation tests:

'riG   rePEAT copies  put mo   rest    types   fup.    6      poweRin' -> 
    'RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT'
                                                                    '' -> 
    ''
```



## Julia


```julia

const commandtable = """
add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1"""

function makedict(tbl)
    str = split(uppercase(tbl), r"\s+")
    dict = Dict{String, String}()
    for (i, s) in enumerate(str)
        if (n = tryparse(Int, s)) != nothing
            dict[str[i-1][1:n]] = str[i-1]
        else
            dict[s] = s
        end
    end
    dict
end

function okabbrev(dict, abb)
    for i in length(abb):-1:1
        if haskey(dict, abb[1:i])
            com = dict[abb[1:i]]
            if length(abb) <= length(com) && abb == com[1:length(abb)]
                return dict[abb[1:i]]
            end
        end
    end
    return "*error*"
end

formattedprint(arr, n) =  (for s in arr print(rpad(s, n)) end; println())

function teststring(str)
    d = makedict(commandtable)
    commands = split(str, r"\s+")
    formattedprint(commands, 9)
    formattedprint([okabbrev(d, uppercase(s)) for s in commands], 9)
end

teststring("riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin")

```
 {{output}} 
```txt

riG      rePEAT   copies   put      mo       rest     types    fup.     6        poweRin
RIGHT    REPEAT   *error*  PUT      MOVE     RESTORE  *error*  *error*  *error*  POWERINPUT

```



## Kotlin


```kotlin
import java.util.Locale

private const val table = "" +
        "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 " +
        "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate " +
        "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 " +
        "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load " +
        "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 " +
        "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 " +
        "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left " +
        "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 "

private data class Command(val name: String, val minLen: Int)

private fun parse(commandList: String): List<Command> {
    val commands = mutableListOf<Command>()
    val fields = commandList.trim().split(" ")
    var i = 0
    while (i < fields.size) {
        val name = fields[i++]
        var minLen = name.length
        if (i < fields.size) {
            val num = fields[i].toIntOrNull()
            if (num != null && num in 1..minLen) {
                minLen = num
                i++
            }
        }
        commands.add(Command(name, minLen))
    }
    return commands
}

private fun get(commands: List<Command>, word: String): String? {
    for ((name, minLen) in commands) {
        if (word.length in minLen..name.length && name.startsWith(word, true)) {
            return name.toUpperCase(Locale.ROOT)
        }
    }
    return null
}

fun main(args: Array<String>) {
    val commands = parse(table)
    val sentence = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
    val words = sentence.trim().split(" ")

    val results = words.map { word -> get(commands, word) ?: "*error*" }

    val paddedUserWords = words.mapIndexed { i, word -> word.padEnd(results[i].length) }
    println("user words:  ${paddedUserWords.joinToString(" ")}")
    println("full words:  ${results.joinToString(" ")}")
}

```


{{out}}

```txt

user words:  riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin    
full words:  RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT 

```



## MiniScript


```MiniScript
c = "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2 Clast 3"
c = c + " compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate"
c = c + " 3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2"
c = c + " forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load"
c = c + " locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2"
c = c + " msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3"
c = c + " refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left"
c = c + " 2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1"

minLen = {}
lastItem = ""
for item in c.split
    if item == "" then continue
    item = item.upper
    if lastItem and item[0] >= "0" and item[0] <= "9" then
        minLen[lastItem] = val(item)
        lastItem = ""
    else
        minLen[item] = null
        lastItem = item
    end if
end for

check = function(word)
    word = word.upper
    for key in minLen.indexes
        if key[:word.len] != word then continue
        min = minLen[key]
        if min and word.len < min then continue
        return key
    end for
    return "*error*"
end function

input = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"

output = []
for word in input.split
    if word == "" then continue
    output.push check(word)
end for
print output.join
```


{{out}}

```txt
RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT
```



## OCaml


```ocaml
open String

let table_as_string =
  "add 1 alter 3  backup 2  bottom 1  Cappend 2  change 1 \
   Schange  Cinsert 2  Clast 3 compress 4 copy 2 count 3 \
   Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate 3 \
   xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 \
   Cfind 2 findUP 3 fUP 2 forward 2  get  help 1 hexType 4 \
   input 1 powerInput 3  join 1 split 2 spltJOIN load locate 1 \
   Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 \
   modify 3 move 2 msg  next 1 overlay 1 parse preserve 4 purge 3 \
   put putD query 1 quit  read recover 3 refresh renum 3 repeat 3 \
   replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left 2 \
   save  set  shift 2  si  sort  sos  stack 3 status 4 top \
   transfer 3  type 1  up 1"
```


The interesting part below is the '''compare''' function.
We'll use it when storing our entries ( {"add";1}, {"alter";3}, etc )
'''and''' when looking for a word, from a given abbreviation.
For the former, a simple comparison function on strings is enough,
but for the latter, we need to ensure that the word corresponds to the abbreviation
(the abbreviation is a substring of the word and the abbreviation length is sufficient).

```ocaml
module Entry = struct
  type t = { word : string ; min : int }
  let compare e1 e2 =
    let n1 = length e1.word in
    if n1 < e2.min || n1 > length e2.word then
      compare e1.word e2.word
    else
      compare (sub e1.word 0 n1) (sub e2.word 0 n1)
end

module Table = Set.Make(Entry)
```


The few functions below are used to build the table from the string at the beginning.

```ocaml
let clean_strings strs =
  List.filter (fun w -> w <> "" && w <> " ") strs

let rec split = function
  | []      -> Table.empty

  | [w]     ->
     Table.singleton { word=w ; min=length w }

  | w::x::t ->
     try
       let m = int_of_string x in
       Table.add { word=uppercase_ascii w ; min=m } (split t)
     with Failure _ ->
       let m = length w in
       Table.add { word=uppercase_ascii w ; min=m } (split (x::t))

let make_table table_as_string =
  split_on_char ' ' table_as_string
  |> clean_strings
  |> split
```


Finally, here is the function looking for a word :

```ocaml
let abbrev (table:Table.t) (w:string) : string =
  let w = uppercase_ascii w in
  try
    let e = Table.find { word=w ; min=length w } table in e.word
  with Not_found -> "*error*"

let rec check (table:Table.t) (inputs:string list) : unit =
  List.map (abbrev table) inputs
  |> List.iter (Printf.printf "%s ");
  print_newline ()
      
let main =
  let table = make_table table_as_string in
  let inputs = ["riG";"rePEAT";"copies";"put";"mo";"rest";"types";"fup.";"6";"poweRin"] in
  check table inputs;
  exit 0
```


{{out}}

```txt
Input:              riG       rePEAT    copies    put       mo        rest      types     fup.      6         poweRin
Output:             RIGHT     REPEAT    *error*   PUT       MOVE      RESTORE   *error*   *error*   *error*   POWERINPUT
```



## Perl

{{trans|Perl 6}}

```perl
@c = (uc join ' ', qw<
add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1
>) =~ /([a-zA-Z]+(?:\s+\d+)?)(?=\s+[a-zA-Z]|$)/g;

my %abr = ('' => '', ' ' => '');
for (@c) {
    ($w,$sl) = split ' ', $_;
    $ll = length($w);
    $sl = $ll unless $sl;
    $abr{substr($w,0,$sl)} = $w;
    map { $abr{substr($w, 0, $_)} = $w } $sl .. $ll;
}

$fmt = "%-10s";
$inp = sprintf $fmt, 'Input:';
$out = sprintf $fmt, 'Output:';
for $str ('', qw<riG rePEAT copies put mo rest types fup. 6 poweRin>) {
    $inp .= sprintf $fmt, $str;
    $out .= sprintf $fmt, $abr{uc $str} // '*error*';
}

print "$inp\n$out\n";
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
add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1
> ~~ m:g/ (<.alpha>+) \s* (\d*) /;

my %abr = '' => '', |$/.map: {
    my $abbrv = .[0].Str.fc;
    |map { $abbrv.substr( 0, $_ ) => $abbrv.uc },
    +(.[1].Str || .[0].Str.chars) .. .[0].Str.chars;
};

sub abbr-simple ( $str ) { %abr{$str.trim.fc} // '*error*' }

# Testing
for 'riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin', '' -> $str {
    put ' Input: ', $str;
    put 'Output: ', join ' ', $str.words.map: *.&abbr-simple;
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
   add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
   compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
   3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
   forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
   locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
   msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
   refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
   2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1
""",
input = "riG   rePEAT copies  put mo   rest    types   fup.    6   poweRin"

function set_min_lengths(sequence a)
--
-- set default lengths and apply min lengths if present, eg ..."macro","merge","2",...
--  ==> {.."macro",5}} ==> {.."macro",5},{"merge",5}} ==> {.."macro",5},{"merge",2}}
-- ie both macro and merge get a default min length of 5, but the min length of merge 
--    is overwritten when the "2" is processed.
--
    sequence res = {}
    for i=1 to length(a) do
        string ai = a[i]
        if length(ai)=1 and ai>="1" and ai<="9" then
            res[$][2] = ai[1]-'0'
        else
            res = append(res,{ai,length(ai)})
        end if
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


### =Procedural=

{{works with|Python|3.6}}

```python


command_table_text = """add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
   compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
   3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
   forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
   locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
   msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
   refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
   2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1"""

user_words = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"


def find_abbreviations_length(command_table_text):
    """ find the minimal abbreviation length for each word.
        a word that does not have minimum abbreviation length specified
        gets it's full lengths as the minimum.
    """
    command_table = dict()
    input_iter = iter(command_table_text.split())

    word = None
    try:
        while True:
            if word is None:
                word = next(input_iter)
            abbr_len = next(input_iter, len(word))
            try:
                command_table[word] = int(abbr_len)
                word = None
            except ValueError:
                command_table[word] = len(word)
                word = abbr_len
    except StopIteration:
        pass
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

{{out}}

```txt
input:  ""
output: ""
input:  "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
output: "RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT"

```




### =Composition of pure functions=

{{Works with|Python|3.7}}

```python
'''Simple abbreviations'''

from functools import reduce
import re


# withExpansions :: [(String, Int)] -> String -> String
def withExpansions(table):
    '''A string in which all words are either
       expanded (if they match abbreviations in
       a supplied table), or replaced with an
       '*error*' string.
    '''
    return lambda s: unwords(map(
        expanded(table), words(s)
    ))


# expanded :: [(String, Int)] -> String -> String
def expanded(table):
    '''An abbreviation (or error string) for
       a given word, based on a table of full
       strings and minimum abbreviation lengths.
    '''
    def expansion(k):
        u = k.upper()
        lng = len(k)

        def p(wn):
            w, n = wn
            return w.startswith(u) and lng >= n
        return find(p)(table) if k else Just(('', 0))

    return lambda s: maybe('*error*')(fst)(expansion(s))


# cmdsFromString :: String -> [(String, Int)]
def cmdsFromString(s):
    '''A simple rule-base consisting of a
       list of tuples [(
          upper case expansion string,
          minimum character count of abbreviation
       )],
       obtained by a parse of single input string.
    '''
    def go(a, x):
        xs, n = a
        return (xs, int(x)) if x.isdigit() else (
            ([(x.upper(), n)] + xs, 0)
        )
    return fst(reduce(
        go,
        reversed(re.split(r'\s+', s)),
        ([], 0)
    ))


# TEST -------------------------------------------------
def main():
    '''Tests of abbreviation expansions'''

    # table :: [(String, Int)]
    table = cmdsFromString(
        '''add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1
            Schange Cinsert 2  Clast 3 compress 4 copy 2 count 3 Coverlay 3
            cursor 3  delete 3 Cdelete 2  down 1  duplicate 3 xEdit 1 expand 3
            extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
            forward 2  get  help 1 hexType 4 input 1 powerInput 3  join 1
            split 2 spltJOIN load locate 1 Clocate 2 lowerCase 3 upperCase 3
            Lprefix 2  macro  merge 2 modify 3 move 2 msg  next 1 overlay 1
            parse preserve 4 purge 3 put putD query 1 quit read recover 3
            refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4
            rgtLEFT right 2 left 2  save  set  shift 2  si  sort  sos stack 3
            status 4 top  transfer 3  type 1  up 1'''
    )

    # tests :: [String]
    tests = [
        'riG   rePEAT copies  put mo   rest    types   fup.    6      poweRin',
        ''
    ]

    print(
        fTable(__doc__ + ':\n')(lambda s: "'" + s + "'")(
            lambda s: "\n\t'" + s + "'"
        )(withExpansions(table))(tests)
    )


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# Just :: a -> Maybe a
def Just(x):
    '''Constructor for an inhabited Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': False, 'Just': x}


# Nothing :: Maybe a
def Nothing():
    '''Constructor for an empty Maybe (option type) value.'''
    return {'type': 'Maybe', 'Nothing': True}


# find :: (a -> Bool) -> [a] -> Maybe a
def find(p):
    '''Just the first element in the list that matches p,
       or Nothing if no elements match.'''
    def go(xs):
        for x in xs:
            if p(x):
                return Just(x)
        return Nothing()
    return lambda xs: go(xs)


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# fTable :: String -> (a -> String)
#                  -> (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function ->
                 fx display function ->
          f -> value list -> tabular string.'''
    def go(xShow, fxShow, f, xs):
        w = max(map(compose(len)(xShow), xs))
        return s + '\n' + '\n'.join([
            xShow(x).rjust(w, ' ') + (' -> ') + fxShow(f(x))
            for x in xs
        ])
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# maybe :: b -> (a -> b) -> Maybe a -> b
def maybe(v):
    '''Either the default value v, if m is Nothing,
       or the application of f to x,
       where m is Just(x).'''
    return lambda f: lambda m: v if m.get('Nothing') else (
        f(m.get('Just'))
    )


# unwords :: [String] -> String
def unwords(xs):
    '''A space-separated string derived from
       a list of words.'''
    return ' '.join(xs)


# words :: String -> [String]
def words(s):
    '''A list of words delimited by characters
       representing white space.'''
    return re.split(r'\s+', s)


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Simple abbreviations:

'riG   rePEAT copies  put mo   rest    types   fup.    6      poweRin' -> 
    'RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT'
                                                                    '' -> 
    ''
```



## Racket


```racket
#lang racket
(require srfi/13)

(define command-table #<<EOS
add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1
EOS
  )

(define command/abbr-length-pairs
  (let loop ((cmd-len-list (string-split command-table)) (acc (list)))
    (match cmd-len-list
      ((list) (sort acc < #:key cdr))
      ((list-rest a (app string->number (and ad (not #f))) dd) (loop dd (cons (cons a ad) acc)))
      ((list-rest a d) (loop d (cons (cons a (string-length a)) acc))))))

(define (validate-word w)
  (or (let ((w-len (string-length w)))
        (for/first ((candidate command/abbr-length-pairs)
                    #:when (and (>= w-len (cdr candidate)) (string-prefix-ci? w (car candidate))))
          (string-upcase (car candidate))))
      "*error*"))

(define (validate-string s) (string-join (map validate-word (string-split s))))

(module+ main
  (define (debug-i/o s) (printf "input:  ~s~%output: ~s~%" s (validate-string s)))
  (debug-i/o "")
  (debug-i/o "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"))

(module+ test
  (require rackunit)
  (check-equal?
   (validate-string "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin")
   "RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT")
  (check-equal? (validate-string "") ""))
```

{{out}}

```txt
input:  ""
output: ""
input:  "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
output: "RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT"

```



## REXX


```rexx
/*REXX program validates a user  "word"  against a  "command table"  with abbreviations.*/
parse arg uw                                     /*obtain optional arguments from the CL*/
if uw=''  then uw= 'riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin'
say 'user words: '   uw

@= 'add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3',
   'compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate',
   '3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2',
   'forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load',
   'locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2',
   'msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3',
   'refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left',
   '2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1'

say 'full words: '   validate(uw)                /*display the result(s) to the terminal*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
validate: procedure expose @;  arg x;  upper @   /*ARG   capitalizes all the  X  words. */
          $=                                     /*initialize the return string to null.*/
             do j=1  to words(x);   _=word(x, j) /*obtain a word from the     X  list.  */
               do k=1  to words(@); a=word(@, k) /*get a legitmate command name from  @.*/
               L=word(@, k+1)                    /*··· and maybe get it's abbrev length.*/
               if datatype(L, 'W')  then k=k + 1       /*yuppers, it's an abbrev length.*/
                                    else L=length(a)   /*nope,  it can't be abbreviated.*/
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


```Ruby
str = "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 
   compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
   3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
   forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
   locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
   msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
   refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
   2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1"

RE = /(?<word1>[a-zA-Z]+)\s+(?<word2>[a-zA-Z]+)/
str = str.upcase
# add missing wordsizes
2.times{ str.gsub!(RE){ [  $~[:word1], $~[:word1].size, $~[:word2] ].join(" ")} }

table = Hash[*str.split].transform_values(&:to_i)

test =  "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin"
ar = test.split.map do |w|
  (res = table.detect{|k,v| k.start_with?(w.upcase) && w.size >= v}) ? res[0] : "*error*"
end

puts ar.join(" ")

```

{{out}}

```txt
RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## Rust

{{works with|Rust|1.35.0}}
cargo clippy and cargo fmt run against it

```rust
use std::collections::HashMap;

// The plan here is to build a hashmap of all the commands keyed on the minimum number of
// letters than can be provided in the input to match. For each known command it will appear
// in a list of possible commands for a given string lengths. A command can therefore appear a
// number of times. For example, the command 'recover' has a minimum abbreviation length of 3.
// In the hashmap 'recover' will be stored behind keys for 3, 4, 5, 6 & 7 as any abbreviation of
// 'recover' from 3 until 7 letters inclusive can match. This way, once the length of the input
// string is known a subset of possible matches can be retrieved immediately and then checked.
//
fn main() {
    let command_table_string =
        "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
   compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
   3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
   forward 2  get  help 1 hexType 4  input_command 1 powerInput 3  join 1 split 2 spltJOIN load
   locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
   msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
   refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
   2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1";

    // Split up the command table string using the whitespace and set up an iterator
    // to run through it. We need the iterator to be peekable so that we can look ahead at
    // the next item.
    let mut iter = command_table_string.split_whitespace().peekable();

    let mut command_table = HashMap::new();

    // Attempt to take two items at a time from the command table string. These two items will be
    // the command string and the minimum length of the abbreviation. If there is no abbreviation length
    // then there is no number provided. As the second item might not be a number, so we need to peek at
    // it first. If it is a number we can use it as a key for the hashmap. If it is not a number then
    // we use the length of the first item instead because no abbreviations are available for the
    // word i.e. the whole word must be used. A while loop is used because we need to control iteration
    // and look ahead.
    //
    while let Some(command_string) = iter.next() {
        let command_string_length = command_string.len() as i32;

        let min_letter_match = match iter.peek() {
            Some(potential_number) => match potential_number.parse::<i32>() {
                Ok(number) => {
                    iter.next();
                    number
                }
                Err(_) => command_string_length,
            },
            None => break,
        };

        // The word must be stored for every valid abbreviation length.
        //
        for i in min_letter_match..=command_string_length {
            let cmd_list = command_table.entry(i).or_insert_with(Vec::new);
            cmd_list.push(command_string.to_uppercase());
        }
    }

    const ERROR_TEXT: &str = "*error*";

    let test_input_text = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin";

    let mut output_text = String::new();

    let mut iter = test_input_text.split_whitespace().peekable();

    // Run through each item in the input string, find the length of it
    // and then use this to fetch a list of possible matches.
    // A while loop is used because we need to look ahead in order to indentify
    // the last item and avoid adding an unnecessary space.
    //
    while let Some(input_command) = iter.next() {
        let input_command_length = input_command.len() as i32;

        let command_list = match command_table.get(&input_command_length) {
            Some(list) => list,
            None => {
                output_text.push_str(ERROR_TEXT);
                continue;
            }
        };

        let input_command_caps = input_command.to_uppercase();
        let matched_commands: Vec<&String> = command_list
            .iter()
            .filter(|command| command.starts_with(&input_command_caps))
            .collect();

        // Should either be 0 or 1 command found
        assert!(
            matched_commands.len() < 2,
            "Strange.. {:?}",
            matched_commands
        );

        match matched_commands.first() {
            Some(cmd) => output_text.push_str(cmd),
            None => output_text.push_str(ERROR_TEXT),
        }

        if iter.peek().is_some() {
            output_text.push(' ');
        }
    }

    println!("Input was: {}", test_input_text);
    println!("Output is: {}", output_text);

    let correct_output = "RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT";
    assert_eq!(output_text, correct_output)
}
```

{{out}}

```txt

Input was: riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin
Output is: RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```



## Scala


```Scala

object Main extends App {
    implicit class StrOps(i: String) {
        def isAbbreviationOf(target: String, targetMinLength: Int): Boolean = {
            @scala.annotation.tailrec
            def checkPAsPrefixOfM(p: List[Char], m: List[Char]): Boolean = (p, m) match {
                case (Nil, _) => true //prefix empty
                case (_, Nil) => false //main string empty
                case (ph :: pt, mh :: mt) if ph.toUpper == mh.toUpper => checkPAsPrefixOfM(pt, mt) //case insensitive match of head characters
                case _ => false
            }
            i.length >= targetMinLength && checkPAsPrefixOfM(i.toList, target.toList)
        }
    }

    val commands = """
         add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
         compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
         3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
         forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
         locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
         msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
         refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
         2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1
      """.stripMargin.replace("\n", " ").trim.split(" ")

    val commandWithMinLengths = commands.sliding(2, 1)
                                       .filter{ window => 
                                           window.length > 1 && Try(window(0).toInt).toOption.isEmpty
                                       }
                                       .map{ w => 
                                           (
                                               w(0), 
                                               Try(w(1).toInt).toOption.getOrElse(0)
                                           )
                                       }
                                       .toList

    val input = "riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin".split(" ").filter(!_.isEmpty)

    val resultLine = input.map{ i =>
        commandWithMinLengths.find{case (c, l) => i.isAbbreviationOf(c, l)}.map(_._1.toUpperCase).getOrElse("*error*")
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

puts "user words: $words"
puts $result
```

{{out}}
```txt
./abbreviations_simple.tcl
user words:   riG   rePEAT copies  put mo   rest    types   fup.    6       poweRin
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
    s = s & "add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3 "
    s = s & "compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate "
    s = s & "3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2 "
    s = s & "forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load "
    s = s & "locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2 "
    s = s & "msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3 "
    s = s & "refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left "
    s = s & "2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1 "
    commandtable = Split(s, " ")
    Dim i As Integer, word As Variant, number As Integer
    For i = LBound(commandtable) To UBound(commandtable)
        word = commandtable(i)
        If Len(word) > 0 Then
            i = i + 1
            Do While Len(commandtable(i)) = 0: i = i + 1: Loop
            number = Val(commandtable(i))
            If number > 0 Then
                command_table.Add Key:=word, Item:=number
            Else
                command_table.Add Key:=word, Item:=Len(word)
                i = i - 1
            End If
        End If
    Next i
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



## zkl


```zkl
commands:=Data(0,String,	// "add\01\0alter\0..."
#<<<
"add 1  alter 3  backup 2  bottom 1  Cappend 2  change 1  Schange  Cinsert 2  Clast 3
compress 4 copy 2 count 3 Coverlay 3 cursor 3  delete 3 Cdelete 2  down 1  duplicate
3 xEdit 1 expand 3 extract 3  find 1 Nfind 2 Nfindup 6 NfUP 3 Cfind 2 findUP 3 fUP 2
forward 2  get  help 1 hexType 4  input 1 powerInput 3  join 1 split 2 spltJOIN load
locate 1 Clocate 2 lowerCase 3 upperCase 3 Lprefix 2  macro  merge 2 modify 3 move 2
msg  next 1 overlay 1 parse preserve 4 purge 3 put putD query 1 quit  read recover 3
refresh renum 3 repeat 3 replace 1 Creplace 2 reset 3 restore 4 rgtLEFT right 2 left
2  save  set  shift 2  si  sort  sos  stack 3 status 4 top  transfer 3  type 1  up 1"
.toUpper().split());
#<<<

var szs=Dictionary(); // [<index>:<length> ...]
n:=0; while(n<commands.len()){
   cmd,nc := commands.readString(n), n + cmd.len() + 1;
   len:=commands.readString(nc);
   if(len.matches("[0-9]*")){ szs[n]=len.toInt(); n=nc+len.len()+1 }
   else			    { szs[n]=cmd.len();   n=nc; }
}

testText:="riG   rePEAT copies  put mo   rest    types "
          "   fup.    6       poweRin";

testText.split().apply('wrap(w){ 
   w=w.toUpper(); n:=0;
   while(True){  // check for length requirement and, if there, verify
      n=commands.find(w,n);
      if(Void==n) return("*error*");	// end of loop if no match
      c:=commands.readString(n);
      if(w.len()>=szs.find(n,99999)) return(c);
      n+=c.len();
   }
}).concat(" ").println();
```

{{out}}

```txt

RIGHT REPEAT *error* PUT MOVE RESTORE *error* *error* *error* POWERINPUT

```

