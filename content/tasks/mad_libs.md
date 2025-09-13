+++
title = "Mad Libs"
description = ""
date = 2019-10-05T20:28:44Z
aliases = []
[extra]
id = 10812
[taxonomies]
categories = ["task"]
tags = []
+++

[[wp:Mad Libs|Mad Libs]] is a phrasal template word game where one player prompts another for a list of words to substitute for blanks in a story, usually with funny results.


;Task;
Write a program to create a Mad Libs like story.

The program should read an arbitrary multiline story from input.

The story will be terminated with a blank line.

Then, find each replacement to be made within the story, ask the user for a word to replace it with, and make all the replacements.

Stop when there are none left and print the final story.


The input should be an arbitrary story in the form:

```txt

<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

```

Given this example, it should then ask for a <tt>name</tt>, a <tt>he or she</tt> and a <tt>noun</tt> (<tt><nowiki><name></nowiki></tt> gets replaced both times with the same value).


## Related tasks

*   [[Old_lady_swallowed_a_fly]]
*   [[The Twelve Days of Christmas]]




== {{header|Ada}} ==

The fun of Mad Libs is not knowing the story ahead of time, so the program reads the story template from a text file. The name of the text file is given as a command line argument.


```Ada
with Ada.Text_IO, Ada.Command_Line, String_Helper;

procedure Madlib is

   use String_Helper;

   Text: Vector := Get_Vector(Ada.Command_Line.Argument(1));
   M, N: Natural;

begin
   -- search for templates and modify the text accordingly
   for I in Text.First_Index .. Text.Last_Index loop
      loop
         Search_Brackets(Text.Element(I), "<", ">", M, N);
      exit when M=0; -- "M=0" means "not found"
         Ada.Text_IO.Put_Line("Replacement for " & Text.Element(I)(M .. N) & "?");
         declare
            Old: String := Text.Element(I)(M .. N);
            New_Word: String := Ada.Text_IO.Get_Line;
         begin
            for J in I .. Text.Last_Index loop
               Text.Replace_Element(J, Replace(Text.Element(J), Old, New_Word));
            end loop;
         end;
      end loop;
   end loop;

   -- write the text
   for I in Text.First_Index .. Text.Last_Index loop
      Ada.Text_IO.Put_Line(Text.Element(I));
   end loop;
end Madlib;
```


It uses an auxiliary package String_Helper for simple string functions;


```Ada
with Ada.Containers.Indefinite_Vectors;

package String_Helper is

   function Index(Source: String; Pattern: String) return Natural;

   procedure Search_Brackets(Source: String;
                             Left_Bracket: String;
                             Right_Bracket: String;
                             First, Last: out Natural);
      -- returns indices of first pair of brackets in source
      -- indices are zero if no such brackets are found

   function Replace(Source: String; Old_Word: String; New_Word: String)
                   return String;

   package String_Vec is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => String);

   type Vector is new String_Vec.Vector with null record;

   function Get_Vector(Filename: String) return Vector;

end String_Helper;
```


Here is the implementation of String_Helper:


```Ada
with Ada.Strings.Fixed, Ada.Text_IO;

package body String_Helper is

   function Index(Source: String; Pattern: String) return Natural is
   begin
      return Ada.Strings.Fixed.Index(Source => Source, Pattern => Pattern);
   end Index;

   procedure Search_Brackets(Source: String;
                             Left_Bracket: String;
                             Right_Bracket: String;
                             First, Last: out Natural) is
   begin
      First := Index(Source, Left_Bracket);
      if First = 0 then
         Last := 0; -- not found
      else
         Last := Index(Source(First+1 .. Source'Last), Right_Bracket);
         if Last = 0 then
            First := 0; -- not found;
         end if;
      end if;
   end Search_Brackets;

   function Replace(Source: String; Old_Word: String; New_Word: String)
                   return String is
      L: Positive := Old_Word'Length;
      I: Natural := Index(Source, Old_Word);
   begin
      if I = 0 then
         return Source;
      else
         return Source(Source'First .. I-1) & New_Word
           & Replace(Source(I+L .. Source'Last), Old_Word, New_Word);
      end if;
   end Replace;

   function Get_Vector(Filename: String) return Vector is
      F: Ada.Text_IO.File_Type;
      T: Vector;
   begin
      Ada.Text_IO.Open(F, Ada.Text_IO.In_File, Filename);
      while not Ada.Text_IO.End_Of_File(F) loop
         T.Append(Ada.Text_IO.Get_Line(F));
      end loop;
      Ada.Text_IO.Close(F);
      return T;
   end Get_Vector;

end String_Helper;
```


A sample run (with the story template in t.txt):


```txt
./madlib t.txt
Replacement for <name>?
Hilla, the hypohondraic,
Replacement for <he or she>?
She
Replacement for <noun>?
headache
Hilla, the hypohondraic, went for a walk in the park. She
found a headache. Hilla, the hypohondraic, decided to take it home.
```



## Aime


```aime
file f;
data b;
list l;
record r;

f.stdin;

o_text("Enter the blank line terminated story:\n");

while (0 < f.b_line(b)) {
    l.append(b);
}

for (, b in l) {
    integer p, q;
    text s, t;

    while ((p = b.place('<')) ^ -1) {
        q = b.probe(p, '>');
        if (q ^ -1) {
            s = bq_string(b, p + 1, q - 1);
            b.erase(p, q);
            if (!r.key(s)) {
                o_("Replacement for `", s, "':\n");
                f.line(t);
                r.put(s, t);
            }
            b.paste(p, r[s]);
        }
    }
}

l.ucall(o_, 0, "\n");
```



## ALGOL 68

```algol68
# Mad Libs style story generation                                        #

# gets the story template from the file f. The template terminates with  #
# a blank line                                                           #
# The user is then promoted for the replacements for the <word> markers  #
# and the story is printed with the substitutions made                   #
PROC story = ( REF FILE f )VOID:
BEGIN

    # a linked list of strings, used to hold the story template          #
    MODE STRINGLIST = STRUCT( STRING text, REF STRINGLIST next );

    # a linked list of pairs of strings, used to hold the replacements   #
    MODE REPLACEMENTLIST = STRUCT( STRING word
                                 , STRING replacement
                                 , REF REPLACEMENTLIST next
                                 );

    # NIL reference for marking the end of a STRINGLIST                  #
    REF STRINGLIST      nil stringlist      = NIL;

    # NIL reference for marking the end of a REPLACEMENTLIST             #
    REF REPLACEMENTLIST nil replacementlist = NIL;


    # returns "text" with trailing spaces removed                        #
    OP RTRIM = ( STRING text )STRING:
    BEGIN
        INT trim pos := UPB text;
        FOR text pos FROM UPB text BY -1 TO LWB text WHILE text[ text pos ] = " "
        DO
            trim pos := text pos - 1
        OD;

        text[ LWB text : trim pos ]
    END; # RTRIM #

    # looks for word in the dictionary. If it is found, replacement is   #
    # set to its replacement and TRUE is returned. If word not present,  #
    # FALSE is returned - uses recursion                                 #
    PROC find replacement = ( STRING word
                            , REF STRING replacement
                            , REF REPLACEMENTLIST dictionary
                            )BOOL:
        IF   dictionary IS nil replacementlist
        THEN
            FALSE
        ELIF word OF dictionary = word
        THEN
            replacement := replacement OF dictionary;
            TRUE
        ELSE
            find replacement( word, replacement, next OF dictionary )
        FI; # find replacement #


    # read the story template                                            #

    # the result has a dummy element so "next OF next" is always valid   #
    REF STRINGLIST     story := HEAP STRINGLIST := ( "dummy", nil stringlist );
    REF REF STRINGLIST next  := story;

    # read the story template, terminates with a blank line              #

    WHILE
        STRING text;
        get( f, ( text, newline ) );
        text := RTRIM text;
        text /= ""
    DO
        # add the line to the end of the list #
        next := ( next OF next ) := HEAP STRINGLIST := ( text, nil stringlist )
    OD;

    # find the <word> markers in the story and replace them with the     #
    # user's chosen text - we ignore the dummy element at the start      #

    REF REPLACEMENTLIST dictionary := nil replacementlist;
    REF STRINGLIST      line       := story;

    WHILE line := next OF line;
          line ISNT nil stringlist
    DO
        # have a line of text - replace all the <word> markers in it     #
        STRING word, replacement;
        INT start pos, end pos;
        WHILE char in string( "<", start pos, text OF line )
          AND char in string( ">", end pos,   text OF line )
        DO
            # have a marker, get it from the line                        #
            word := ( text OF line )[ start pos : end pos ];
            # get its replacement                                        #
            IF NOT find replacement( word, replacement, dictionary )
            THEN
                # we don't already have a replacement for word           #
                # get one from the user and add it to the dictionary     #
                print( ( "What should replace ", word, "? " ) );
                read( ( replacement, newline ) );
                dictionary := HEAP REPLACEMENTLIST := ( word, replacement, dictionary )
            FI;
            # replace <word> with the replacement                        #
            text OF line := ( text OF line )[ : start pos - 1 ]
                          + replacement
                          + ( text OF line )[ end pos + 1 : ]
        OD
    OD;

    # print the story, ignoring the dummy element at the start           #

    line := story;

    WHILE line := next OF line;
          line ISNT nil stringlist
    DO
        print( ( text OF line, newline ) )
    OD

END; # story #

main:(

    # we read the template from stand in (the keyboard unless it's been  #
    # redirected) we could prompt the user for a template file name,     #
    # open it and read that instead...                                   #
    print( ( "Please Enter the story template terminated by a blank line", newline ) );
    story( stand in )

)
```

```txt

Please Enter the story template terminated by a blank line
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

What should replace <name>? Al
What should replace <he or she>? They
What should replace <noun>? NOTETY STYLE pack
Al went for a walk in the park. They
found a NOTETY STYLE pack. Al decided to take it home.

```





## AppleScript


----

```AppleScript


set theNoun to the text returned of (display dialog "What is your noun?" default answer "")

set thePerson to the text returned of (display dialog "What is the person's name?" default answer "")

set theGender to the text returned of (display dialog "He or she? Please capitalize." default answer "")

display dialog thePerson & " went for a walk in the park. " & theGender & " found a " & theNoun & ".  " & thePerson & " decided to take it home."


```



## AutoHotkey

Like some other examples, this prompts the user for a text file template.<br/>AutoHotkey is interestingly well suited for this task...

```AHK
FileSelectFile, filename, , %A_ScriptDir%, Select a Mad Libs template, *.txt
If ErrorLevel
	ExitApp ; the user canceled the file selection
FileRead, contents, %filename%
pos := match := 0
While pos := RegExMatch(contents, "<[^>]+>", match, pos+strLen(match))
{
	InputBox, repl, Mad Libs, Enter a replacement for %match%:
	If ErrorLevel ; cancelled inputbox
		ExitApp
	StringReplace, contents, contents, %match%, %repl%, All
}
MsgBox % contents
```

```txt
Han Solo went for a walk in the park. She
found a flagpole. Han Solo decided to take it home.
```



## AWK


```AWK

# syntax: GAWK -f MAD_LIBS.AWK
BEGIN {
    print("enter story:")
}
{   story_arr[++nr] = $0
    if ($0 ~ /^ *$/) {
      exit
    }
    while ($0 ~ /[<>]/) {
      L = index($0,"<")
      R = index($0,">")
      changes_arr[substr($0,L,R-L+1)] = ""
      sub(/</,"",$0)
      sub(/>/,"",$0)
    }
}
END {
    PROCINFO["sorted_in"] = "@ind_str_asc"
    print("enter values for:")
    for (i in changes_arr) { # prompt for replacement values
      printf("%s ",i)
      getline rec
      sub(/ +$/,"",rec)
      changes_arr[i] = rec
    }
    printf("\nrevised story:\n")
    for (i=1; i<=nr; i++) { # print the story
      for (j in changes_arr) {
        gsub(j,changes_arr[j],story_arr[i])
      }
      printf("%s\n",story_arr[i])
    }
    exit(0)
}

```

```txt

enter story:
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

enter values for:
<he or she> She
<name> Barbara
<noun> flower

revised story:
Barbara went for a walk in the park. She
found a flower. Barbara decided to take it home.

```




## BASIC256


```BASIC256

cadena = "<name> went for a walk in the park. <he or she> found a <noun>. <name> decided to take it home."
k = instr(cadena, "<")

print "La historia: " : print cadena : print
while k
	reemplaza = mid(cadena, k, instr(cadena, ">") - k + 1)
	print "What should replace "; reemplaza; : input " ", con
	while k
		cadena = left(cadena, k-1) + con + mid(cadena, k + length(reemplaza), length(cadena))
		k = instr(cadena, reemplaza, k)
	end while
	k = instr(cadena, "<", length(reemplaza))
end while
print : print "La historia final: " : print cadena
end

```



== {{header|C}} ==
Dealing with c strings can be quite annoying. This solution implements a dynamic string type with simple functionality which makes the actual madlibs logic a bit clearer. A quicker solution would probably circumvent the copying and moving over characters in the string by utilizing a different data structure that could make use of pointers to input data. This would reduce copying, and require less memory, and allocations for.

```c

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define err(...) fprintf(stderr, ## __VA_ARGS__), exit(1)

/* We create a dynamic string with a few functions which make modifying
 * the string and growing a bit easier */
typedef struct {
    char *data;
    size_t alloc;
    size_t length;
} dstr;

inline int dstr_space(dstr *s, size_t grow_amount)
{
    return s->length + grow_amount < s->alloc;
}

int dstr_grow(dstr *s)
{
    s->alloc *= 2;
    char *attempt = realloc(s->data, s->alloc);

    if (!attempt) return 0;
    else s->data = attempt;

    return 1;
}

dstr* dstr_init(const size_t to_allocate)
{
    dstr *s = malloc(sizeof(dstr));
    if (!s) goto failure;

    s->length = 0;
    s->alloc  = to_allocate;
    s->data   = malloc(s->alloc);

    if (!s->data) goto failure;

    return s;

failure:
    if (s->data) free(s->data);
    if (s) free(s);
    return NULL;
}

void dstr_delete(dstr *s)
{
    if (s->data) free(s->data);
    if (s) free(s);
}

dstr* readinput(FILE *fd)
{
    static const size_t buffer_size = 4096;
    char buffer[buffer_size];

    dstr *s = dstr_init(buffer_size);
    if (!s) goto failure;

    while (fgets(buffer, buffer_size, fd)) {
        while (!dstr_space(s, buffer_size))
            if (!dstr_grow(s)) goto failure;

        strncpy(s->data + s->length, buffer, buffer_size);
        s->length += strlen(buffer);
    }

    return s;

failure:
    dstr_delete(s);
    return NULL;
}

void dstr_replace_all(dstr *story, const char *replace, const char *insert)
{
    const size_t replace_l = strlen(replace);
    const size_t insert_l  = strlen(insert);
    char *start = story->data;

    while ((start = strstr(start, replace))) {
        if (!dstr_space(story, insert_l - replace_l))
            if (!dstr_grow(story)) err("Failed to allocate memory");

        if (insert_l != replace_l) {
            memmove(start + insert_l, start + replace_l, story->length -
                    (start + replace_l - story->data));

            /* Remember to null terminate the data so we can utilize it
             * as we normally would */
            story->length += insert_l - replace_l;
            story->data[story->length] = 0;
        }

        memmove(start, insert, insert_l);
    }
}

void madlibs(dstr *story)
{
    static const size_t buffer_size = 128;
    char insert[buffer_size];
    char replace[buffer_size];

    char *start,
         *end = story->data;

    while (start = strchr(end, '<')) {
        if (!(end = strchr(start, '>'))) err("Malformed brackets in input");

        /* One extra for current char and another for nul byte */
        strncpy(replace, start, end - start + 1);
        replace[end - start + 1] = '\0';

        printf("Enter value for field %s: ", replace);

        fgets(insert, buffer_size, stdin);
        const size_t il = strlen(insert) - 1;
        if (insert[il] == '\n')
            insert[il] = '\0';

        dstr_replace_all(story, replace, insert);
    }
    printf("\n");
}

int main(int argc, char *argv[])
{
    if (argc < 2) return 0;

    FILE *fd = fopen(argv[1], "r");
    if (!fd) err("Could not open file: '%s\n", argv[1]);

    dstr *story = readinput(fd); fclose(fd);
    if (!story) err("Failed to allocate memory");

    madlibs(story);
    printf("%s\n", story->data);
    dstr_delete(story);
    return 0;
}

```

```txt

./a.out madlibs.txt
Enter value for field <name>: John
Enter value for field <he or she>: he
Enter value for field <noun>: Flamingo

John went for a walk in the park. he
found a Flamingo. John decided to take it home.

```


== {{header|C++}} ==

```cpp
#include <iostream>
#include <string>
using namespace std;

int main()
{
  string story, input;

  //Loop
  while(true)
  {
    //Get a line from the user
    getline(cin, input);

    //If it's blank, break this loop
    if(input == "\r")
      break;

    //Add the line to the story
    story += input;
  }

  //While there is a '<' in the story
  int begin;
  while((begin = story.find("<")) != string::npos)
  {
    //Get the category from between '<' and '>'
    int end = story.find(">");
    string cat = story.substr(begin + 1, end - begin - 1);

    //Ask the user for a replacement
    cout << "Give me a " << cat << ": ";
    cin >> input;

    //While there's a matching category
    //in the story
    while((begin = story.find("<" + cat + ">")) != string::npos)
    {
      //Replace it with the user's replacement
      story.replace(begin, cat.length()+2, input);
    }
  }

  //Output the final story
  cout << endl << story;

  return 0;
}
```



## C#


```c#
using System;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;

namespace MadLibs_RosettaCode
{
	class Program
	{
		static void Main(string[] args)
		{
			string madLibs =
@"Write a program to create a Mad Libs like story.
The program should read an arbitrary multiline story from input.
The story will be terminated with a blank line.
Then, find each replacement to be made within the story,
ask the user for a word to replace it with, and make all the replacements.
Stop when there are none left and print the final story.
The input should be an arbitrary story in the form:
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.
Given this example, it should then ask for a name,
a he or she and a noun (<name> gets replaced both times with the same value).";

			StringBuilder sb = new StringBuilder();
			Regex pattern = new Regex(@"\<(.*?)\>");
			string storyLine;
			string replacement;

			Console.WriteLine(madLibs + Environment.NewLine + Environment.NewLine);
			Console.WriteLine("Enter a story: ");

			// Continue to get input while empty line hasn't been entered.
			do
			{
				storyLine = Console.ReadLine();
				sb.Append(storyLine + Environment.NewLine);
			} while (!string.IsNullOrEmpty(storyLine) && !string.IsNullOrWhiteSpace(storyLine));

			// Retrieve only the unique regex matches from the user entered story.
			Match nameMatch = pattern.Matches(sb.ToString()).OfType<Match>().Where(x => x.Value.Equals("<name>")).Select(x => x.Value).Distinct() as Match;
			if(nameMatch != null)
			{
				do
				{
					Console.WriteLine("Enter value for: " + nameMatch.Value);
					replacement = Console.ReadLine();
				} while (string.IsNullOrEmpty(replacement) || string.IsNullOrWhiteSpace(replacement));
				sb.Replace(nameMatch.Value, replacement);
			}

			foreach (Match match in pattern.Matches(sb.ToString()))
			{
				replacement = string.Empty;
				// Guarantee we get a non-whitespace value for the replacement
				do
				{
					Console.WriteLine("Enter value for: " + match.Value);
					replacement = Console.ReadLine();
				} while (string.IsNullOrEmpty(replacement) || string.IsNullOrWhiteSpace(replacement));

				int location = sb.ToString().IndexOf(match.Value);
				sb.Remove(location, match.Value.Length).Insert(location, replacement);
			}

			Console.WriteLine(Environment.NewLine + Environment.NewLine + "--[ Here's your story! ]--");
			Console.WriteLine(sb.ToString());
		}
	}
}

```



## Clojure


```clojure
(ns magic.rosetta
    (:require [clojure.string :as str]))

(defn mad-libs
    "Write a program to create a Mad Libs like story.
     The program should read an arbitrary multiline story from input.
     The story will be terminated with a blank line.
     Then, find each replacement to be made within the story,
     ask the user for a word to replace it with, and make all the replacements.
     Stop when there are none left and print the final story.
     The input should be an arbitrary story in the form:
     <name> went for a walk in the park. <he or she>
     found a <noun>. <name> decided to take it home.
     Given this example, it should then ask for a name,
     a he or she and a noun (<name> gets replaced both times with the same value). "
    []
    (let
        [story (do
            (println "Please enter story:")
            (loop [story []]
                (let [line (read-line)]
                    (if (empty? line)
                        (str/join "\n" story)
                        (recur (conj story line))))))
         tokens (set (re-seq #"<[^<>]+>" story))
         story-completed (reduce
            (fn [s t]
                (str/replace s t (do
                    (println (str "Substitute " t ":"))
                    (read-line))))
            story
            tokens)]
        (println (str
            "Here is your story:\n"
            "------------------------------------\n"
            story-completed))))
; Sample run at REPL:
;
; user=> (magic.rosetta/mad-libs)
; Please enter story:
; One day <who> wake up at <where>.
; <who> decided to <do something>.
; While <who> <do something>, strange man
; appears and gave <who> a <thing>.

; Substitute <where>:
; Sweden
; Substitute <thing>:
; Nobel prize
; Substitute <who>:
; Bob Dylan
; Substitute <do something>:
; walk
; Here is your story:
; ------------------------------------
; One day Bob Dylan wake up at Sweden.
; Bob Dylan decided to walk.
; While Bob Dylan walk, strange man
; appears and gave Bob Dylan a Nobel prize.

```



## D

```d
import std.stdio, std.regex, std.algorithm, std.string, std.array;

void main() {
    writeln("Enter a story template, terminated by an empty line:");
    string story;
    while (true) {
        auto line = stdin.readln().strip();
        if (line.empty) break;
        story ~= line ~ "\n";
    }

    auto re = regex("<.+?>", "g");
    auto fields = story.match(re).map!q{a.hit}().array().sort().uniq();
    foreach (field; fields) {
        writef("Enter a value for '%s': ", field[1 .. $ - 1]);
        story = story.replace(field, stdin.readln().strip());
    }

    writeln("\nThe story becomes:\n", story);
}
```

```txt
Enter a story template, terminated by an empty line:
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

Enter a value for 'he or she': She
Enter a value for 'name': Monica
Enter a value for 'noun': cockerel

The story becomes:
Monica went for a walk in the park. She
found a cockerel. Monica decided to take it home.
```



## Erlang


```erlang
-module(madlib).
-compile(export_all).

main() ->
    main([]).

main([]) ->
    madlib(standard_io);
main([File]) ->
    {ok, F} = file:open(File,read),
    madlib(F).

madlib(Device) ->
    {Dict, Lines} = parse(Device),
    Substitutions = prompt(Dict),
    print(Substitutions, Lines).

prompt(Dict) ->
    Keys = dict:fetch_keys(Dict),
    lists:foldl(fun (K,D) ->
                        S = io:get_line(io_lib:format("Please name a ~s: ",[K])),
                        dict:store(K, lists:reverse(tl(lists:reverse(S))), D)
                end, Dict, Keys).

print(Dict,Lines) ->
    lists:foreach(fun (Line) ->
                          io:format("~s",[substitute(Dict,Line)])
                  end, Lines).

substitute(Dict,Line) ->
    Keys = dict:fetch_keys(Dict),
    lists:foldl(fun (K,L) ->
                        re:replace(L,K,dict:fetch(K,Dict),[global,{return,list}])
                end, Line, Keys).

parse(Device) ->
    parse(Device, dict:new(),[]).

parse(Device, Dict,Lines) ->
    case io:get_line(Device,"") of
        eof ->
            {Dict, lists:reverse(Lines)};
        "\n" ->
            {Dict, lists:reverse(Lines)};
        Line ->
            parse(Device, parse_line(Dict, Line), [Line|Lines])
    end.

parse_line(Dict, Line) ->
    {match,Matches} = re:run(Line,"<.*?>",[global,{capture,[0],list}]),
    lists:foldl(fun ([M],D) ->
                        dict:store(M,"",D)
                end, Dict, Matches).
```


This version can be called via either madlib:main() or madlib:main(File) to read from standard_in or from a file.
It utilizes the re module to both collect and substitute the words to substitute.
The dict module is used as a mapping between variables and the players desired replacement. dict acts as an immutable hash, dict:store/3 returns a new dictionary with a new or updated key.

```erlang>68
 madlib:main("test.mad").
Please name a <noun>: banana
Please name a <name>: Jack
Please name a <he or she>: She
Jack went for a walk in the park. She
found a banana. Jack decided to take it home.ok
69>
```



## Factor


```factor
USING: formatting io kernel make regexp sequences sets splitting ;
IN: rosetta-code.mad-libs

: get-mad-lib ( -- str )
    "Enter a mad lib. A blank line signals end of input." print
    [
        [ "> " write flush readln dup , empty? f t ? ] loop
    ] { } make harvest "\n" join ;

: find-replacements ( str -- seq )
    R/ <[\w\s]+>/ all-matching-subseqs members ;

: query ( str -- str )
    rest but-last "Enter a(n) %s: " printf flush readln ;

: replacements ( str seq -- str )
    dup [ query ] map [ replace ] 2each ;

: mad-libs ( -- )
    get-mad-lib dup find-replacements replacements nl print ;

MAIN: mad-libs
```

```txt

Enter a mad lib. A blank line signals end of input.
> <name> went for a walk in the park. <he or she>
> found a <noun>. <name> decided to take it home.
>
Enter a(n) name: William
Enter a(n) he or she: he
Enter a(n) noun: crystal

William went for a walk in the park. he
found a crystal. William decided to take it home.

```



## Fortran

Early Fortran had no provision for manipulating text, only arrangements to provide fixed headings for tables, etc. Later came the ability to read and write text via the A1 format code into say an array of integers, usually 16-bit. Such arrays could be manipulated and their numerical values were accessible. Curiously, early SNOBOL interpreters were written in Fortran, because of the portability of such arrangements. Next came the ability for READ and WRITE statements to read from or write to such arrays, as well as normal I/O devices. Finally, Fortran 77 allowed a declaration such as <code>CHARACTER*6 A(200)</code> meaning an array of two hundred elements, each of six characters.

There is no direct equivalent to a string-type variable, one that has a current length as well as text content, however one can proceed with a character variable called say NAME and an associated integer called LNAME, and be diligent. When a character variable is passed to a subprogram, its length is passed as a secret additional parameter, and this can be accessed via a function LEN(param). Later Fortran systems allow the declaration of compound data types, and routines for manipulating them can be "overloaded" onto standard operators so that they may be used in normal-looking expressions, but a great deal of jargon is required for a comprehensive solution. Similarly, character variables can be reallocated storage to fit the size of their current usage, with of course the risk of memory leaks on top of the overhead.

Simple character variables have fixed storage allocation, so one is, as often, confronted with the need to choose sizes that are surely big enough. Thus the story could be stored in an array <code>CHARACTER*100 STORY(200)</code> to allow for up to two hundred lines, each of up to a hundred characters in length, and similarly for the collection of target and replacement texts. Obviously, most lines will be barely filled just to protect against the risk of one longish line, but profligate usage of storage is the modern way.

The plan here is to use a pool of storage, one large character storage area (STASH) along with an array ISTASH giving the positions of the initial character of each stashed text string. Such strings can then be referred to by their index number, a simple integer, which can be stored in other lists as ordinary arrays. The key point is that there is no change to the size of any stashed strings, nor are any deleted. This means no shuffling about with the need to update various arrays. Thus, the i'th stashed string is found in STASH starting at character <code>ISTASH(i)</code> and so by extension, its last character is at <code>ISTASH(i + 1) - 1</code> Thereby the storage of the strings wastes no space, though STASH still has to be "big enough".

Because there is no "length" attribute as a part of a character variable, pieces of a character variable can be passed to subprograms without the need for copies. Specifically, if a string consisted of a one-byte length followed by the storage area for the content (as say in Pascal), passing a substring of characters 7-11 would mean creating a copy in other storage with a length byte having 5 followed by the content. The subprogram could ''not'' receive some sort of pointer to the parent string, because the need to place the length byte for the string type would mean overwriting a character of the parent string. Because Fortran has no such length feature, specifying say <code>STASH(7:11)</code> as a parameter involves ''no'' copying of the content. A subprogram to convert text to uppercase would work on the source data in place rather than copy-in, copy-out. In particular, the comparison of a portion of a character variable to another portion of another character variable is achieved ''without copying text''.

The absence of a "current length" notion also means that when an assignment is made to a character variable, if the source is shorter than the recipient, trailing spaces will be supplied, possibly in very large numbers. A statement <code>STASH = "Short"</code> would copy five characters and then follow with spaces to the end. This can be avoided by specifying <code>STASH(1:5) = "Short"</code> with the usual risk of miscounting in the programming. Character variables typically contain trailing spaces, and the rule for comparison ignores them. Thus "Short" and "Short   " are deemed equal. For this reason, the < and > bounding the target texts are included in their string: "<Short>" and "<Short   >" are definitely not equal.

There is only token checking for ill-formed stories. The checking for array overflow and the like is present on principle, but is unlikely to be exercised in simple runs, and so only uncivilised messages are evoked. Although allowing replacement texts to themselves refer to <...> entries (and add their own) is not called for, the notion adds to the entertainment value, so... This is most easily done via recursion. There has never been any difficulty in having a Fortran subprogram invoke itself or otherwise engage in recursion, it is just that early implementations did not support any sort of stack mechanism to handle the returns to the correct places, just one level of return, and so it was endlessly said that recursion is impossible in Fortran. But it is not actually a language problem, as demonstrated by the Burroughs 6700 implementation, whereby it just worked. This has been ignored in later versions of Fortran, which require the use of the magic word RECURSIVE for all those subprograms that may dabble in unnatural practices.

Otherwise, the older styles of Fortran could dabble in recursion only via the provision of an array or two and a counter and some control code to save and restore whatever state is necessary for the task at hand. This is done in WRITESTORY where the line being transcribed to output has its scan interrupted by the replacement text, which is scanned for replacements, after which the interrupted scan is resumed. The earlier version without this extension merely scanned each line character-by-character via a DO-loop, using the bounds of the text being scanned. Some compilers allow the index and bound of a DO-loop to be adjusted during the progress of the loop (as when the scan of one text would be interrupted by a new scan of a replacement) however many do not and some even declare such attempts an error. So instead a DO WHILE statement is used. Older Fortrans would of course lack such constructions, and so many GO TO statements would appear.

Output is a problem. Languages such as Pascal have their default action after a WRITE statement of not starting a new line, so pieces of text could be written one after another. Fortran is the other way around, always ending the line. This could be countered with lineprinter output through the use of "overprint" options, but such piecemeal hammering at a line would be frowned upon. Later Fortrans offer gibberish to achieve continued-style output, but instead, the plan here is to place the pieces in an output area, the unused space in STASH being the obvious candidate. This alas means that texts have to be copied into place rather than being written to output directly from their storage area. But it also makes it easy to improve the layout: lines can be written no longer than some limit with breaks at the spaces between words. Further, a very simple protocol can be used to preserve paragraphs. Each line of the story is deemed a continuation of the previous line's text (and a space is supplied to separate the last word's end from the start of the next line's first word), however, should the incoming line start with a space, it is deemed to mark the start of a new paragraph: existing text is flushed and the new line of input starts a new line of output, thus preserving the indentation marking a paragraph. Unfortunately, web pages routinely employ blank lines between paragraphs, which should only appear between sections.

An example of the resulting opportunities for confusion:

 Reads a story in template form, containing special entries such as <dog's name> amongst the text.
 You will be invited to supply a replacement text for each such entry, as encountered,
 after which the story will be presented with your substitutions made.

 Here goes... Reading file Madlib.txt

 Enter your text for <name>: Joseph the <adjective>
 Enter your text for <he or she>: He was poor but <adjective> and
 Enter your text for <adjective>: lucky
 Enter your text for <noun>: high-value banknote with "<name>" written on it

    Righto!

 Joseph the lucky went for a walk in the park. He was poor but
 lucky and found a high-value banknote with "Joseph the lucky"
 written on it. Joseph the lucky decided to take it home.


```Fortran

      MODULE MADLIB	!Messing with COMMON is less convenient.
       INTEGER MSG,KBD,INF		!I/O unit numbers.
       DATA MSG,KBD,INF/6,5,10/		!Output, input, some disc file.
       INTEGER LSTASH,NSTASH,MSTASH	!Prepare a common text stash.
       PARAMETER (LSTASH = 246810, MSTASH = 6666)	!LSTASH characters for MSTASH texts.
       CHARACTER*(LSTASH) STASH		!The pool.
       INTEGER ISTASH(MSTASH + 1)	!Fingers start positions, and thus end positions by extension.
       DATA NSTASH,ISTASH(1)/0,1/	!Empty pool: no entries, first available character is at 1.
       INTEGER MANYLINES,MANYTESTS	!I also want some lists of texts.
       PARAMETER (MANYLINES = 1234)	!This is to hold the story.
       INTEGER NSTORY,STORY(MANYLINES)	!Fingering texts in the stash.
       PARAMETER (MANYTESTS = 1234)	!Likewise, some target/replacement texts.
       INTEGER NTESTS,TARGET(MANYTESTS),REPLACEMENT(MANYTESTS)	!Thus.
       DATA NSTORY,NTESTS/0,0/		!No story lines, and no tests.
       INTEGER STACKLIMIT		!A recursion limit.
       PARAMETER (STACKLIMIT = 28)	!This should suffice.

       CONTAINS
        SUBROUTINE CROAK(GASP)	!A dying remark.
         CHARACTER*(*) GASP	!The last words.
         WRITE (MSG,*) "Oh dear."	!Shock.
         WRITE (MSG,*) GASP		!Aargh!
         STOP "How sad."		!Farewell, cruel world.
        END SUBROUTINE CROAK	!Farewell...

        SUBROUTINE SHOWSTASH(BLAH,I)	!One might be wondering.
         CHARACTER*(*) BLAH		!An annotation.
         INTEGER I			!The desired stashed text.
          WRITE (MSG,1) BLAH,I,STASH(ISTASH(I):ISTASH(I + 1) - 1)	!Whee!
    1     FORMAT (A,': Text(',I0,')="',A,'"')	!Hopefully, helpful.
        END SUBROUTINE SHOWSTASH	!Ah, debugging.

        INTEGER FUNCTION EATTEXT(IN)	!Add a text to STASH and finger it.
Co-opts the as-yet unused space in STASH as its scratchpad.
         INTEGER IN	!Input from this I/O unit number.
         INTEGER I,N,L	!Fingers.
          I = ISTASH(NSTASH + 1)!First available position in STASH.
          N = LSTASH - I + 1	!Number of characters yet unused.
          IF (N.LT.666) CALL CROAK("Insufficient STASH space remains!")
          READ (IN,1,END = 66) L,STASH(I:I + MIN(L,N) - 1)	!Calculated during the read.
    1     FORMAT (Q,A)		!Obviously, Q = character count incoming, A = accept all of them.
          L = I + MIN(L,N) - 1	!The last character read.
   10     IF (L.LT.I) GO TO 66	!A blank line! Deemed end-of-file.
          IF (ICHAR(STASH(L:L)).LE.ICHAR(" ")) THEN	!A trailing space?
            L = L - 1		!Yes. Pull back.
            GO TO 10		!And try again.
          END IF		!So much for trailing spaces.
          IF (NSTASH.GE.MSTASH) CALL CROAK("Too many texts!")
          NSTASH = NSTASH + 1	!Admit another text.
          ISTASH(NSTASH + 1) = L + 1	!The start point of the following text.
          EATTEXT = NSTASH	!STASH(ISTASH(n):ISTASH(n + 1) - 1) has text n.
         RETURN			!All well.
   66     EATTEXT = 0		!Sez: "No text".
        END FUNCTION EATTEXT	!Rather odd side effects.

        INTEGER FUNCTION ADDSTASH(TEXT)	!Appends an arbitrary text to the pool of stashed texts.
         CHARACTER*(*) TEXT	!The stuff.
         INTEGER I		!A finger.
          IF (NSTASH.GE.MSTASH) CALL CROAK("The text pool is crowded!")	!Alas.
          I = ISTASH(NSTASH + 1)	!First unused character.
          IF (I + LEN(TEXT).GT.LSTASH) CALL CROAK("Overtexted!")	!Alack.
          STASH(I:I + LEN(TEXT) - 1) = TEXT	!Place.
          NSTASH = NSTASH + 1			!Count in another entry.
          ISTASH(NSTASH + 1) = I + LEN(TEXT)	!The new "first available" position.
          ADDSTASH = NSTASH	!Pass a finger back to the caller.
        END FUNCTION ADDSTASH	!Just an integer.

        INTEGER FUNCTION ANOTHER(TEXT)	!Possibly add TEXT to the table of target texts.
Collects TARGET REPLACEMENT pairs (increasing NTESTS) as directed by INSPECT.
         CHARACTER*(*) TEXT	!The text of the target.
         INTEGER I,IT		!Steppers.
          ANOTHER = 0		!Possibly, the text is already in the table.
          DO I = 1,NTESTS	!So, step through the known target texts.
            IT = TARGET(I)		!Finger a target text.
            IF (TEXT.EQ.STASH(ISTASH(IT):ISTASH(IT + 1) - 1)) RETURN	!Already have this one.
          END DO		!Otherwise, try the next.
          IF (NTESTS.GE.MANYTESTS) CALL CROAK("Too many tests!")	!Oh dear.
          NTESTS = NTESTS + 1		!Count in another.
          TARGET(NTESTS) = ADDSTASH(TEXT)!Stash its text and get a finger to it.
          ANOTHER = NTESTS		!My caller will want to know which test.
          WRITE (MSG,1) TEXT		!Now request the replacement text.
    1     FORMAT ("Enter your text for ",A,": ",$)	!Obviously, the $ indicates "no new line".
          REPLACEMENT(NTESTS) = EATTEXT(KBD)	!Zero for "no text".
        END FUNCTION ANOTHER	!Produces entries for TARGET and REPLACEMENT.

        SUBROUTINE INSPECT(X)	!Examine text number X for the special <...> sequence.
Calls for inspection of REPLACEMENT texts as well, should ANOTHER report a new entry.
         INTEGER X	!Fingers the text in STASH via ISTASH(X).
         INTEGER MARK	!Recalls where the < was found.
         INTEGER IT,NEW	!Fingers to entries in STASH.
         INTEGER I	!A stepper.
         INTEGER SP,STACK(STACKLIMIT)	!Prepare for some recursion.
          SP = 1		!Start with the starter.
          STACK(1) = X		!Stack up.
          DO WHILE(SP.GT.0)	!While texts are yet uninspected,
            IT = STACK(SP)		!Finger one.
            SP = SP - 1			!Working down the stack.
            MARK = 0			!Uninitialised variables are bad.
            DO I = ISTASH(IT),ISTASH(IT + 1) - 1!Step through the stashed text.
              IF (STASH(I:I).EQ."<") THEN	!Is it the starter?
                MARK = I			!Yes. Remember where it is.
              ELSE IF (STASH(I:I).EQ.">") THEN	!The ender?
                IF (MARK.LE.0) CALL CROAK("A > with no preceeding <!")	!Bah.
                NEW = ANOTHER(STASH(MARK:I))	!Consider the spanned text.
                IF (NEW.GT.0) THEN		!If that became a new table entry,
                  IF (SP.GE.STACKLIMIT) CALL CROAK("Stack overflow!")	!Its replacement is to be inspected.
                  SP = SP + 1			!But I'm still busy with the current text.
                  STACK(SP) = REPLACEMENT(NEW)	!So, stack it for later.
                END IF			!So much for that <...> apparition.
                MARK = 0		!Be ready to check afresh for the next.
              END IF		!So much for that character.
            END DO		!On to the next.
          END DO	!So much for that stacked entry.
        END SUBROUTINE INSPECT	!WRITESTORY will rescan the story lines.

        SUBROUTINE READSTORY(IN)!Read and stash the lines.
         INTEGER IN		!Input from here.
         INTEGER LINE		!A finger to the story line.
   10    LINE = EATTEXT(IN)	!So, grab a line.
         IF (LINE.GT.0) THEN	!A live line?
           NSTORY = NSTORY + 1	!Yes.Count it in.
           STORY(NSTORY) = LINE	!Save it in the story list.
           CALL INSPECT(LINE)	!Look for trouble as well.
           GO TO 10		!And go for the next line.
         END IF			!Oh for while (Line:=EatText(in)) > 0 do SaveAndInspect(Line);
        END SUBROUTINE READSTORY!Simple enough, anyway.

        SUBROUTINE WRITESTORY(WIDTH)	!Applying the replacements, with replacement replacement too.
Co-opts the as-yet unused space in STASH as its output scratchpad.
Can't rely on changing the index and bounds of a DO-loop on the fly.
         INTEGER WIDTH
         INTEGER LINE,IT,I,J	!Steppers.
         INTEGER L,L0,N		!Fingers.
         INTEGER TAIL,MARK,LAST	!Scan choppers.
         INTEGER SP,STACKI(STACKLIMIT),STACKL(STACKLIMIT)	!Ah, recursion.
          L0 = ISTASH(NSTASH + 1)	!The first available place in the stash.
          L = L0 - 1			!Syncopation for my output finger.
       LL:DO LINE = 1,NSTORY		!Step through the lines of the story.
            SP = 0			!Start with the task in hand.
            IT = STORY(LINE)		!Finger the stashed line.
            LAST = ISTASH(IT + 1) - 1	!Find its last character in STASH.
            I = ISTASH(IT)		!Find its first character in STASH.
            TAIL = I - 1		!Syncopation. No text from this line yet.
            IF (STASH(I:I).LE." ") THEN	!The line starts with a space?
              CALL BURP			!Yes. Flush, so as to start a new paragraph.
            ELSE IF (LINE.GT.1) THEN	!Otherwise, the line is a continuation.
              L = L + 1			!So, squeeze in a space as a separator.
              STASH(L:L) = " "		!Since its text follows on.
            END IF			!Now for the content of the line.
  666    II:DO WHILE(I.LE.LAST)		!Step along its text.
              IF (STASH(I:I).EQ."<") THEN	!Trouble starter?
                MARK = I			!Yes. Remember where.
              ELSE IF (STASH(I:I).EQ.">") THEN	!The corresponding ender?
                CALL APPEND(TAIL + 1,MARK - 1)	!Waiting text up to the mark.
             JJ:DO J = 1,NTESTS		!Step through the target texts.
                  IT = TARGET(J)		!Finger one.
                  IF (STASH(ISTASH(IT):ISTASH(IT + 1) - 1)	!Its stashed text.
     1            .EQ.STASH(MARK:I)) THEN		!Matches the suspect text?
                    IT = REPLACEMENT(J)		!Yes! Finger the replacement text.
                    IF (IT.GT.0) THEN	!Null replacements can be ignored.
                      IF (SP.GE.STACKLIMIT) CALL CROAK("StackOverflow!")	!Always diff. messages.
                      SP = SP + 1		!Interrupt the current scan.
                      STACKI(SP) = I		!Remember where we're up to,
                      STACKL(SP) = LAST		!And the end of the text.
                      I = ISTASH(IT) - 1	!One will be added shortly, at JJ+1.
                      LAST = ISTASH(IT + 1) - 1	!Preempt the scan-in-progress.
                    END IF			!To work along the replacement text.
                    EXIT JJ		!Found the target, so the search is finished.
                  END IF		!Otherwise,
                END DO JJ		!Try the next target text.
                TAIL = I		!Normal text resumes at TAIL + 1.
              END IF			!Enough analysis of that character from the story line.
              I = I + 1			!The next to consider.
            END DO II		!Perhaps we've finished this text.
            IF (SP.GT.0) THEN	!Yes! But, were we interrupted in a previous scan?
              CALL APPEND(TAIL + 1,LAST)!Yes! Roll the tail of the just-finished scan.
              TAIL = STACKI(SP)		!The stacked value of I was fingering a >.
              LAST = STACKL(SP)		!And this was the end of the text.
              SP = SP - 1		!So we've recovered where the scan was.
              I = TAIL + 1		!And this is the next to look at.
              GO TO 666			!Proceed to do so.
            END IF		!But if all is unstacked,
            CALL APPEND(TAIL + 1,LAST)	!Don't forget the tail end.
          END DO LL			!On to the next story line.
          CALL BURP		!Any waiting text must be less than WIDTH.
         CONTAINS		!Some assistants, defined after usage...
          SUBROUTINE APPEND(IST,LST)	!Has access to L.
           INTEGER IST,LST		!To copy STASH(IST:LST) to the scratchpad.
           INTEGER N			!The number of characters to copy.
            N = LST - IST + 1		!So find out.
            IF (N.LE.0) RETURN		!Avoid relying on zero-length action.
            IF (L + N.GT.LSTASH) CALL CROAK("Out of stash!")	!Oh dear.
            STASH(L + 1:L + N) = STASH(IST:LST)	!There they go.
            L = L + N			!Advance my oputput finger.
            IF (L - L0 + 1.GE.WIDTH) CALL BURP	!Enough to be going on with?
          END SUBROUTINE APPEND		!Few invocations, if with tricky parameters.
          SUBROUTINE BURP		!Flushes forth up to WIDTH characters.
           INTEGER N,W,L1		!And slides any remnant back.
            N = L - L0 + 1		!So, how many characters are waiting?
            IF (N.LE.WIDTH) THEN	!Too many for one line of output?
              L1 = L			!Nope. Roll the lot.
             ELSE			!Otherwise, a partial flush.
              W = L0 + WIDTH - 1	!Last character that can be fitted into WIDTH.
              DO L1 = W,L0,-1		!Look for a good split.
                IF (STASH(L1:L1).LE." ") EXIT	!Like, at a space.
              END DO			!Keep winding back.
              IF (L1.LE.L0) L1 = W	!No pleasing split found. Just roll a full width.
            END IF			!Ready to roll.
            WRITE (MSG,"(A)") STASH(L0:L1)	!Thus!
            IF (N.LE.WIDTH) THEN	!If the whole text was written,
              L = L0 - 1		!Then there is no text in the scratchpad.
             ELSE			!If only L0:L1 were written of L0:L,
              W = L0 + L - L1 - 1	!How far will the remaining text extend?
              STASH(L0:W) = STASH(L1 + 1:L)	!Shift it.
              L = W			!Finger the last used character position.
            END IF			!One trim is enough, even if the scracchpad contains multiple widths' worth..
          END SUBROUTINE BURP		!Since I don't want to flush the lot.
        END SUBROUTINE WRITESTORY	!Just a sequence of lines.
      END MODULE MADLIB		!Enough of that.

      PROGRAM MADLIBBER	!See, for example, https://en.wikipedia.org/wiki/Mad_Libs
      USE MADLIB
      WRITE (MSG,1)	!It's polite to explain.
    10FORMAT ("Reads a story in template form, containing special ",
     1 "entries such as <dog's name> amongst the text.",/,
     2 "You will be invited to supply a replacement text for each "
     3 "such entry, as encountered,",/,
     4 "after which the story will be presented with your ",
     5 "substitutions made.",//,
     6 "Here goes... Reading file Madlib.txt",/)
      OPEN(INF,STATUS="OLD",ACTION="READ",FORM="FORMATTED",
     1 FILE = "Madlib.txt")
      CALL READSTORY(INF)
      CLOSE(INF)
      WRITE (MSG,*)
      WRITE (MSG,*) "  Righto!"
      WRITE (MSG,*)
      CALL WRITESTORY(66)
      END

```




## FreeBASIC


```freebasic

Dim As String con, cadena
cadena = "<name> went for a walk in the park. <he or she> found a <noun>. <name> decided to take it home."
Dim As Integer k = Instr(cadena, "<")

Print "La historia: "
Print cadena & Chr(10)
While k
    Dim As String reemplaza = Mid(cadena, k, Instr(cadena, ">") - k + 1)
    Print "What should replace "; reemplaza; : Input con
    While k
        cadena = Left(cadena, k-1) & con & Mid(cadena, k + Len(reemplaza))
        k = Instr(k, cadena, reemplaza)
    Wend
    k = Instr(cadena, "<")
Wend
Print Chr(10) & "La historia final: "
Print cadena & Chr(10)

```




## Go

Variance: The fun of Mad Libs is not knowing the story ahead of time, so instead of asking the player to enter the story template, my program asks the player to enter the file name of a story template (with contents presumably unknown to the player.)

```go
package main

import (
    "bufio"
    "fmt"
    "io/ioutil"
    "log"
    "os"
    "regexp"
    "strings"
)

func main() {
    pat := regexp.MustCompile("<.+?>")
    if len(os.Args) != 2 {
        fmt.Println("usage: madlib <story template file>")
        return
    }
    b, err := ioutil.ReadFile(os.Args[1])
    if err != nil {
        log.Fatal(err)
    }
    tmpl := string(b)
    s := []string{}          // patterns in order of appearance
    m := map[string]string{} // mapping from patterns to replacements
    for _, p := range pat.FindAllString(tmpl, -1) {
        if _, ok := m[p]; !ok {
            m[p] = ""
            s = append(s, p)
        }
    }
    fmt.Println("Enter replacements:")
    br := bufio.NewReader(os.Stdin)
    for _, p := range s {
        for {
            fmt.Printf("%s: ", p[1:len(p)-1])
            r, isPre, err := br.ReadLine()
            if err != nil {
                log.Fatal(err)
            }
            if isPre {
                log.Fatal("you're not playing right. :P")
            }
            s := strings.TrimSpace(string(r))
            if s == "" {
                fmt.Println("  hmm?")
                continue
            }
            m[p] = s
            break
        }
    }
    fmt.Println("\nYour story:\n")
    fmt.Println(pat.ReplaceAllStringFunc(tmpl, func(p string) string {
        return m[p]
    }))
}
```

Sample run:

```txt

Enter replacements:
character name: Wonko the Sane
third person pronoun for character: he
noun: wild weasel

Your story:

Wonko the Sane went for a walk in the park. he
found a wild weasel. Wonko the Sane decided to take it home.

```



## Haskell

This will read a template story via stdin with no arguments, or read from a file if given as an argument.

```Haskell
import System.IO (stdout, hFlush)

import System.Environment (getArgs)

import qualified Data.Map as M (Map, lookup, insert, empty)

getLines :: IO [String]
getLines = reverse <$> getLines_ []
  where
    getLines_ xs = do
      line <- getLine
      case line of
        [] -> return xs
        _ -> getLines_ $ line : xs

prompt :: String -> IO String
prompt p = putStr p >> hFlush stdout >> getLine

getKeyword :: String -> Maybe String
getKeyword ('<':xs) = getKeyword_ xs []
  where
    getKeyword_ [] _ = Nothing
    getKeyword_ (x:'>':_) acc = Just $ '<' : reverse ('>' : x : acc)
    getKeyword_ (x:xs) acc = getKeyword_ xs $ x : acc
getKeyword _ = Nothing

parseText :: String -> M.Map String String -> IO String
parseText [] _ = return []
parseText line@(l:lx) keywords =
  case getKeyword line of
    Nothing -> (l :) <$> parseText lx keywords
    Just keyword -> do
      let rest = drop (length keyword) line
      case M.lookup keyword keywords of
        Nothing -> do
          newword <- prompt $ "Enter a word for " ++ keyword ++ ": "
          rest_ <- parseText rest $ M.insert keyword newword keywords
          return $ newword ++ rest_
        Just knownword -> do
          rest_ <- parseText rest keywords
          return $ knownword ++ rest_

main :: IO ()
main = do
  args <- getArgs
  nlines <-
    case args of
      [] -> unlines <$> getLines
      arg:_ -> readFile arg
  nlines_ <- parseText nlines M.empty
  putStrLn ""
  putStrLn nlines_
```



## J


Implementation:


```J
require 'general/misc/prompt regex'

madlib=:3 :0
  smoutput 'Please enter the story template'
  smoutput 'See http://rosettacode.org/wiki/Mad_Libs for details'
  t=.''
  while.#l=.prompt '' do. t=.t,l,LF end.
  repl=. ~.'<[^<>]*>' rxall t
  for_bef. repl do.
    aft=. prompt (}.}:;bef),': '
    t=.t rplc bef,<aft
  end.
  t
)
```


Example use:


```J
   madlib''
Please enter the story template
See http://rosettacode.org/wiki/Mad_Libs for details
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

name: Jill
he or she: she
noun: rock
Jill went for a walk in the park. she
found a rock. Jill decided to take it home.

```



## Java

This is extremely messy code. There's bound to be a more optimal way of doing this.


```Java
import java.util.Map;
import java.util.HashMap;
import java.util.Scanner;
import java.util.StringTokenizer;

public class MadLibs
{
	public static void main(String[] args)
	{
		Scanner s=new Scanner(System.in);
		String line;
		StringBuffer storybuffer=new StringBuffer();

		//Accept lines until empty line is entered
		while(!(line=s.nextLine()).isEmpty())
			storybuffer.append(" "+line);

		//Remove first space
		storybuffer.delete(0, 1);
		String story=storybuffer.toString();
		//Split
		StringTokenizer str=new StringTokenizer(story);
		String word;
		StringBuffer finalstory=new StringBuffer();

		//Store added elements
		Map<String,String> hash=new HashMap<String,String>();
		while(str.hasMoreTokens())
		{
			word=str.nextToken();
			if(word.contains("<"))
			{
				String add="";
				//Element prompt could be more than one word
				if(!word.contains(">"))
				{
					//Build multi-word prompt
					String phrase="";
					do{
						phrase+=word+" ";
					}while(!(word=str.nextToken()).contains(">"));
					word=phrase+word;
				}
				//Account for element placeholder being immediately followed by . or , or whatever.
				if(word.charAt(word.length()-1)!='>')
					add=word.substring(word.lastIndexOf('>')+1);

				//Store id of element in hash table
				String id=word.substring(0,word.lastIndexOf('>')+1);
				String value;

				if(!hash.containsKey(id))
				{
					//New element
					System.out.println("Enter a "+ id);
					value=s.nextLine()+add;
					hash.put(id, value);
				}
				//Previously entered element
				else
					value=hash.get(id);
				word=value;
			}
			finalstory.append(word+" ");
		}
		System.out.println(finalstory.toString());
		s.close();
	}
}
```


```txt

<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

Enter a <name>
Champak
Enter a <he or she>
He
Enter a <noun>
hippo

Champak went for a walk in the park. He found a hippo. Champak decided to take it home.


```


=={{header|Icon}} and {{header|Unicon}}==
This just runs with the sample.  It would be much more fun with a database of randomly selected story templates.

```Icon
procedure main()
   ml := "<name> went for a walk in the park. There <he or she> _
          found a <noun>. <name> decided to take it home."  # sample
   MadLib(ml)                                               # run it
end

link strings

procedure MadLib(story)
   write("Please provide words for the following:")
   V := []
   story ? while ( tab(upto('<')), put(V,tab(upto('>')+1)) )
   every writes(v := !set(V)," : ") do
      story := replace(story,v,read())
   write("\nYour MadLib follows:\n",story)
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/strings.icn strings.icn provides replace]

```txt
Please provide words for the following:
<noun> : keys
<he or she> : she
<name> : Goldilocks

Your MadLib follows:
Goldilocks went for a walk in the park. There she found a keys. Goldilocks decided to take it home.
```



## Julia

```julia

function madlibs(template)
    println("The story template is:\n", template)
    fields = Set(getfield.(collect(eachmatch(r"<[^>]+>", template)), :match))
    print("\nInput a comma-separated list of words to replace the following items:",
          join(fields, ", "), "\n -> ")
    values = split(readline(STDIN), ",")
    for (m, v) in zip(fields, values)
        template = replace(template, m, v)
    end
    println("\nThe story becomes:\n", template)
end

const template = """
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.
"""

madlibs(template)
```


```txt
The story template is:
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.


Input a comma-separated list of words to replace the following items:<name>, <he or she>, <noun>
 -> Martin,He,dog

The story becomes:
Martin went for a walk in the park. He
found a dog. Martin decided to take it home.

```



## Kotlin


```scala
// version 1.1.2

fun main(args: Array<String>) {
    println("Please enter a multi-line story template terminated by a blank line\n")
    val sb  = StringBuilder()
    while (true) {
        val line = readLine()!!
        if (line.isEmpty()) break
        sb.append("$line\n") // preserve line breaks
    }
    var story = sb.toString()
    // identify blanks
    val r = Regex("<.*?>")
    val blanks = r.findAll(story).map { it.value }.distinct()
    println("Please enter your replacements for the following 'blanks' in the story:")
    for (blank in blanks) {
        print("${blank.drop(1).dropLast(1)} : ")
        val repl = readLine()!!
        story = story.replace(blank, repl)
    }
    println("\n$story")
}
```


```txt

Please enter a multi-line story template terminated by a blank line

<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home

Please enter your replacements for the following 'blanks' in the story:
name : Alice
he or she : he
noun : flying saucer

Alice went for a walk in the park. he
found a flying saucer. Alice decided to take it home

```



## Liberty BASIC


```lb
temp$="<name> went for a walk in the park. <he or she> found a <noun>. <name> decided to take it home."
k = instr(temp$,"<")
while k
    replace$   = mid$(temp$,k,instr(temp$,">")-k + 1)
    print "Replace:";replace$;" with:"; :input with$
    while k
        temp$  = left$(temp$,k-1) + with$ + mid$(temp$,k + len(replace$))
        k      = instr(temp$,replace$,k)
    wend
k = instr(temp$,"<")
wend
print temp$
wait

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Like some other examples, this prompts the user for a filename for a story template rather than reading the story in from user input.

```Mathematica
text = Import[
   InputString["Enter the filename of the story template:"]];
answers =
  AssociationMap[
   InputString[
     "Enter a" <>
      If[StringMatchQ[#,
        "<" ~~ Alternatives @@ Characters["aeiou"] ~~ ___], "n", ""] <>
       " " <> StringTrim[#, "<" | ">"] <> ":"] &,
   Union[StringCases[text, RegularExpression["<[^>]+>"]]]];
Print[StringReplace[text, Normal[answers]]];
```

```txt
George went for a walk in the park. he
found a car. George decided to take it home.
```



## Nim

```nim
import rdstdin, re, algorithm, sequtils, strutils

#let templ = readLineFromStdin "Enter your story: "
const templ = """<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home."""

echo "The story template is:\n", templ
var fields = templ.findAll re"<[^>]+>"
fields.sort(cmp)
fields = deduplicate fields
let values = readLineFromStdin("\nInput a comma-separated list of words to replace the following items\n  " & fields.join(",") & ": ").split(",")

var story = templ
for f,v in zip(fields, values).items:
  story = story.replace(f, v)
echo "\nThe story becomes:\n\n", story
```

Sample run:

```txt
The story template is:
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

Input a comma-separated list of words to replace the following items
  <he or she>,<name>,<noun>: She,Monica L.,cockerel

The story becomes:

Monica L. went for a walk in the park. She
found a cockerel. Monica L. decided to take it home.
```



## Oforth



```Oforth
: madlibs
| story i word |

   "" while(System.Console askln dup notEmpty) [ + ] drop ->story

   while(story indexOf('<') dup ->i notNull) [
      story extract(i, story indexOfFrom('>', i)) ->word
      story replaceAll(word, "Word for" . word . System.Console askln) ->story
      ]

   "Your story :" . story println ;
```



## Pascal

There is no search&replace function made available, nor provision for reading text into a workspace wherein inspection and a mass adjustment could be made. So, some programming. For large stories, the obvious method would be to use a temporary file to save the story, as the specification states that the modified text is to be written out after all the lines of the story have been read. Otherwise, they could be written to an output file as the input file is read. Turbo Pascal offers a "string" variable of up to 255 characters plus a not entirely rigid type matching requirement for parameters (whereby the length of a parameter is a part of its type, so an array or string of ''n'' elements differs in type from one with ''m'' elements: official Pascal is strict about this) so there is some relief. Later systems such as Delphi allow much longer "string" type variables. Thus, no line may exceed 255 characters in length. The table of target texts could of course be sorted (via an index) and a binary search used to find entries, but that would worthwhile only for larger applications.

Because a replacement text could in principle be lengthy (<name> = Monsieur Creosote, the well-known gourmand who ate three suckling pigs in one sitting and called for more) adjusting the stored strings is risky. Instead, the texts are written out piecemeal. There is no attempt to re-flow the resulting output into (say) neat 60-character lines.

A particular feature of Pascal strings is that text[i] gets the i'th character of a string variable ''text'' but copy(text,start,count) is needed for two or more characters. It is not clear whether in <code>if Target[it] = copy(bumf,mark,i - mark + 1) then ...</code> a temporary variable will be made to hold the result of the ''copy'' (as is implied by the name) or whether the comparison will be done "in place". Using <code>if Target[it] = hit then ...</code> ensures that one copy is made but loses the possibility that no copy would be made.

There is no checking that (for example) a > follows a <, nor whether spacing within <...> as in < a noun > is to be objected to. Likewise, will <name> differ from <name >? To ensure that, the < and > symbols are made a part of the Target text. Story text cannot employ a < symbol as itself - it would have to be something like "<please type a <>". However, an entertaining extension is to allow the replacement text to contain references to <...> entries which means that < is a reserved symbol even within a replacement text so that it would have to be <please type a less-than symbol>. Except that won't work, because the replacement text ''is'' inspected.

So, no stories involving < symbols, please.

```Pascal

Program Madlib; Uses DOS, crt; {See, for example, https://en.wikipedia.org/wiki/Mad_Libs}
{Reads the lines of a story but which also contain <xxx> sequences. For each value of xxx,
 found as the lines of the story are read, a request is made for a replacement text.
 The story is then written out with the corresponding replacements made.}
{Concocted by R.N.McLean (whom God preserve), Victoria university, NZ.}
 Procedure Croak(gasp: string); {A dying message.}
  Begin
   Writeln(' Eurghfff...');
   Writeln(Gasp);
   HALT;
  End;
 var inf: text; {Drivelstuff.}
 const StoryLimit = 66;TableLimit = 65;		{Big enough.}
 var Story: array[1..StoryLimit] of string;	{Otherwise, use a temporary disc file.}
 var Target,Replacement: array[1..TableLimit] of string;
 var StoryLines,TableCount: integer;		{Usage.}

 Function Reading(var inf: text;var Aline: string): boolean;
  Begin
   Aline:='';
   Reading:=true;
   if eoln(inf) then Reading:=false	{Agh! Why can't the read statement return true/false?}
    else ReadLn(inf,Aline);
   if Aline = '' then Reading:=false;	{Specified that a blank line ends the story.}
  End;

 Procedure Inspect(text: string); Forward;{I'd rather have multi-pass compilation than deal with this.}

 Procedure Table(it: string);	{Check it as a target, and obtain its replacement.}
  var i: integer;	{A stepper.}
  Begin
   for i:=1 to TableCount do if it = Target[i] then exit;	{Already in the table?}
   if TableCount >= TableLimit then Croak('Too many table entries!');	{No. Room for another?}
   inc(TableCount);				{Yes.}
   Target[TableCount]:=it;			{Include the < and > to preclude partial matches.}
   write('Enter your text for ',it,': ');	{Pretty please?}
   readln(Replacement[TableCount]);		{Thus.}
   Inspect(Replacement[TableCount]);		{Enable full utilisation.}
  End; {of Table.}

 var InDeep: integer;	{Counts inspection recursion.}
 Procedure Inspect(text: string);		{Look for <...> in text.}
  var i: integer;	{A stepper.}
  var mark: integer;	{Fingers the latest < in Aline.}
  Begin
   inc(InDeep);		{Supply an opportunity, and fear the possibilities.}
   if InDeep > 28 then Croak('Excessive recursion! Inspecting ' + text);
   for i:=1 to Length(text) do	{Now scan the line for trouble.}
    if text[i] = '<' then mark:=i	{Trouble starts here? Just mark its place.}
     else if text[i] = '>' then	{Trouble ends here?}
      Table(copy(text,mark,i - mark + 1));	{Deal with it.}
   dec(InDeep);		{I'm done.}
  End; {of Inspect.}

 Procedure Swallow(Aline: string);	{Add a line to the story, and inspect it for <...>.}
  Begin
   if StoryLines >= StoryLimit then Croak('Too many lines in the story!');	{Suspicion forever.}
   inc(StoryLines);		{Otherwise, this is safe.}
   Story[StoryLines]:=Aline;	{So save another line.}
   Inspect(Aline);		{Look for any <...> inclusions.}
  End; {of Swallow.}

 var Rolling: integer;		{Counts rolling rolls.}
 Procedure Roll(bumf: string);	{Write a line, with amendments.}
  var last,mark: integer;	{Fingers for the scan.}
  var hit: string;		{Copied once.}
  var i,it: integer;		{Steppers.}
  label hic;	{Oh dear.}
  Begin
   inc(Rolling);	{Here I go.}
   if Rolling > 28 then Croak('Excessive recursion! Rolling ' + bumf);	{Self-expansion is out.}
   last:=0;			{Where the previous text ended.}
   for i:=1 to Length(bumf) do	{Scan the text.}
    if bumf[i] = '<' then mark:=i	{Remember where a <...> starts.}
     else if bumf[i] = '>' then		{So that when the stopper is found,}
      begin				{It can be recognised.}
       Write(copy(bumf,last + 1,mark - last - 1));	{Text up to the <.}
       hit:=copy(bumf,mark,i - mark + 1);	{Grab this once.}
       for it:=1 to TableCount do	{Search my table.}
        if Target[it] = hit then	{A match?}
         begin				{Yes!}
          Roll(Replacement[it]);	{Write this instead.}
          goto hic;			{There is no "exit loop" style statement.}
         end;				{"Exit" exits the procedure or function.}
   hic:last:=i;		{Advance the trailing finger.}
      end;	{On to the next character.}
   Write(copy(bumf,last + 1,Length(bumf) - last));	{Text after the last >, possibly null.}
   dec(Rolling);			{I'm done.}
   if Rolling <= 0 then WriteLn;	{And if this is the first level, add a end-of-line.}
  End;	{of Roll.}

 var inname: string;	{For a file name.}
 var Aline: string;	{A scratchpad.}
 var i: integer;	{A stepper.}
 BEGIN
  InDeep:=0;		{No inspections yet.}
  Rolling:=0;		{No output.}
  inname:=ParamStr(1);	{Perhaps the file name is specified as a run-time parameter.}
  if inname = '' then inname:='Madlib.txt';	{If not, this will do.}
  Assign(inf,inname); Reset(inf);		{Open the input file.}
  StoryLines:=0; TableCount:=0;			{Prepare the counters.}
  while reading(inf,Aline) do Swallow(Aline);	{Read and inspect the story.}
  close(inf);					{Finished with input.}
  for i:=1 to StoryLines do Roll(Story[i]);	{Write the amended story.}
 END.

```

Example run:
 C:\Nicky\Pascal\FILEME~1>MADLIB.EXE
 Enter your text for <name>: Jack
 Enter your text for <he or she>: He
 Enter your text for <noun>: banknote with <name> written on it
 Jack went for a walk in the park. He
 found a banknote with Jack written on it. Jack decided to take it home.


## Perl

Use the name of the file with a story as the parameter to the programme.

```perl
#!/usr/bin/perl
use warnings;
use strict;

my $template = shift;
open my $IN, '<', $template or die $!;
my $story = do { local $/ ; <$IN> };

my %blanks;
undef $blanks{$_} for $story =~ m/<(.*?)>/g;

for my $blank (sort keys %blanks) {
    print "$blank: ";
    chomp (my $replacement = <>);
    $blanks{$blank} = $replacement;
}

$story =~ s/<(.*?)>/$blanks{$1}/g;
print $story;
```



## Perl 6

Some explanation: <tt>S:g[...] = ...</tt> is a global substitution that returns its result.  <tt>%</tt> is an anonymous state variable in which we cache any results of a prompt using the <tt>//=</tt> operator, which assigns only if the left side is undefined.  <tt>slurp</tt> reads an entire file from STDIN or as named in the argument list.

```perl6
print S:g[ '<' (.*?) '>' ] = %.{$0} //= prompt "$0? " given slurp;
```

Sample run:

```txt
$ madlibs walk
name? Phydeaux
He or She? She
noun? flea
Phydeaux went for a walk in the park. She
found a flea. Phydeaux decided to take it home.
```



## Phix

Set mlfile to the name of a suitable file, if you have one, otherwise it uses the default story.

```Phix
string mlfile = "", -- eg story.txt
mltxt = iff(length(mlfile)?join(read_lines(mlfile),"\n"):"""
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.
""")

sequence strings = {}, replacements = {}
integer startpos, endpos=1
while 1 do
    startpos = find('<',mltxt,endpos)
    if startpos=0 then exit end if
    endpos = find('>',mltxt,startpos)
    if endpos=0 then ?"missing >" abort(0) end if
    string s = mltxt[startpos..endpos]
    if not find(s,strings) then
        strings = append(strings,s)
        replacements = append(replacements,prompt_string(sprintf("Enter replacement for %s:",{s})))
    end if
end while
puts(1,substitute_all(mltxt,strings,replacements))
```

```txt

Enter replacement for <name>:Pete
Enter replacement for <he or she>:He
Enter replacement for <noun>:Rosetta Code Task
Pete went for a walk in the park. He
found a Rosetta Code Task. Pete decided to take it home.

```



## Pike

this solution uses readline to make editing more convenient.

```Pike
#!/usr/bin/pike

Stdio.Readline readln = Stdio.Readline();

void print_help()
{
    write(#"Write a Story.

Names or objects in the story can be made variable by
referencing them as <person> <object>, etc.
End the story with an empty line.

Type show to read the story. You will be asked to fill the variables,
and the the story will be shown.

Type help to see this message again.
Type exit to quit.

");
}

void add_line(string input)
{
    array variables = parse_for_variables(input);
    write("Found variables: %{\"%s\" %}\n", variables);
    story += input+"\n";
}

array parse_for_variables(string input)
{
    array vars = Array.flatten(array_sscanf(input, "%*[^<>]%{<%[^<>]>%*[^<>]%}%*[^<>]"));
    return Array.uniq(vars);
}

mapping fill_variables(string story)
{
    array vars = parse_for_variables(story);
    mapping variables = ([]);
    foreach(vars;; string name)
    {
        string value = readln->read(sprintf("Please name a%s %s: ", (<'a','e','i','o','u'>)[name[1]]?"":"n", name));
        if (value != "")
            variables["<"+name+">"] = value;
    }
    return variables;
}

void show_story(string story)
{
    mapping variables = fill_variables(story);
    write("\n"+replace(story, variables));
}

void do_exit()
{
    exit(0);
}

mapping functions = ([ "help":print_help,
                       "show":show_story,
                       "exit":do_exit,
                     ]);
string story = "";

void main()
{
    Stdio.Readline.History readline_history = Stdio.Readline.History(512);
    readln->enable_history(readline_history);

    string prompt="> ";

    print_help();
    while(1)
    {
        string input=readln->read(prompt);
        if(!input)
            exit(0);
        if(input == "")
            show_story(story);
        else if (functions[input])
            functions[input]();
        else add_line(input);
    }
}
```


 Write a Story.

 Names or objects in the story can be made variable by
 referencing them as <person> <object>, etc.
 End the story with an empty line.

 Type show to read the story. You will be asked to fill the variables,
 and the the story will be shown.

 Type help to see this message again.
 Type exit to quit.

 > <person> is a programmer.
 Found variables: "person"
 > <he or she> created <website> for all of us to enjoy.
 Found variables: "he or she" "website"
 >
 Please name a person: Michael
 Please name a he or she: he
 Please name a website: RosettaCode

 Michael is a programmer.
 he created RosettaCode for all of us to enjoy.
 >
 Please name a person: Guilaumme
 Please name a he or she: he
 Please name a website: PLEAC

 Guilaumme is a programmer.
 he created PLEAC for all of us to enjoy.
 > exit


## PicoLisp

This function extends the syntax a bit to be able to express different words with the same description, the syntax is <name:description>, if the description is omitted the name is used instead, keeping backwards compatibility with the syntax used in the task description:

```PicoLisp
(de madlib (Template)
   (setq Template (split (chop Template) "<" ">"))
   (let (Reps () Text ())
      (while Template
         (push 'Text (pop 'Template))
         (let? Rep (mapcar pack (split (pop 'Template) ":"))
            (if (assoc (car Rep) Reps)
               (push 'Text (cdr @))
               (until (and
                         (prin "Gimme a(n) " (or (cadr Rep) (car Rep)) ": ")
                         (clip (in NIL (line)))
                         (push 'Text @)
                         (push 'Reps (cons (car Rep) @)) )
                  (prinl "Huh? I got nothing.") ) ) ) )
      (prinl (need 30 '-))
      (prinl (flip Text)) ) )

```


This runs the example:

```txt
(madlib
   "<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home." )

Gimme a(n) name: Mr. T
Gimme a(n) he or she: he
Gimme a(n) noun: fool
------------------------------
Mr. T went for a walk in the park. he
found a fool. Mr. T decided to take it home.

```


Example with the extended syntax:

```txt
(madlib
   "<1:name> went for a walk in the park. <2:he or she>
found a <3:noun (singular)>. <1> decided to take it home.

On <4:his or her> way home, <2> found a <5:noun (singular)>,
two <7:noun (plural)> and a <8:noun (singular)>." )

Gimme a(n) name: MC Hammer
Gimme a(n) he or she: he
Gimme a(n) noun (singular): pair of baggy pants
Gimme a(n) his or her: his
Gimme a(n) noun (singular): hammer
Gimme a(n) noun (plural): STOP signs
Gimme a(n) noun (singular): clock
------------------------------
MC Hammer went for a walk in the park. he
found a pair of baggy pants. MC Hammer decided to take it home.

On his way home, he found a hammer,
two STOP signs and a clock.

```



## PL/I

```PL/I
(stringrange, stringsize):                            /* 2 Nov. 2013 */
Mad_Libs: procedure options (main);
   declare (line, left, right) character (100) varying;
   declare true bit(1) value ('1'b), false bit (1) value ('0'b);
   declare name    character (20) varying, seen_name    bit (1) initial (false);
   declare pronoun character (20) varying, seen_pronoun bit (1) initial (false);
   declare noun    character (20) varying, seen_noun    bit (1) initial (false);
   declare replaced_all bit (1);
   declare in file input;

   open file (in) title ('/MADLIBS.DAT,type(text),recsize(100)');

   do forever;
      get file (in) edit (line) (L);
      if line = '' then leave;

      do until (replaced_all);
         replaced_all = true;
         if index(line, '<name>') > 0 then
            if seen_name then
               do until (index(line, '<name>') = 0);
                  call split(line, '<name>', left, right);
                  line = left || name || right;
                  replaced_all = false;
               end;
            else
               do;
                  put skip list ('Please type a name:');
                  get edit (name) (L);
                  seen_name = true; replaced_all = false;
               end;
         if index(line, '<he or she>') > 0 then
            if seen_pronoun then
               do until (index(line, '<he or she>') = 0);
                  call split(line, '<he or she>', left, right);
                  line = left || pronoun || right;
                  replaced_all = false;
               end;
            else
               do;
                   put skip list ('Please type a pronoun (he or she):');
                   get edit (pronoun) (L);
                   seen_pronoun = true; replaced_all = false;
               end;
         if index(line, '<noun>') > 0 then
            if seen_noun then
               do until (index(line, '<noun>') = 0);
                  call split(line, '<noun>', left, right);
                  line = left || noun || right;
                  replaced_all = false;
               end;
            else
               do;
                  put skip list ('Please type a noun:');
                  get edit (noun) (L);
                  seen_noun = true; replaced_all = false;
               end;
         end;
      put skip list (line);
   end;

split: procedure (line, text, Left, Right);
   declare (line, text, left, right) character (*) varying;
   declare i fixed binary;

   i = index(line, text);
   left  = substr(line, 1, i-1);
   right = substr(line, i+length(text), length(line) - (i + length(text)) + 1 );
end split;

end Mad_Libs;
```


```txt

Please type a name:
Please type a pronoun (he or she):
Please type a noun:

John went for a walk in the park. he
found a dog. John decided to take it home.

```



## PowerShell


```PowerShell

function New-MadLibs
{
    [CmdletBinding(DefaultParameterSetName='None')]
    [OutputType([string])]
    Param
    (
        [Parameter(Mandatory=$false)]
        [AllowEmptyString()]
        [string]
        $Name = "",

        [Parameter(Mandatory=$false, ParameterSetName='Male')]
        [switch]
        $Male,

        [Parameter(Mandatory=$false, ParameterSetName='Female')]
        [switch]
        $Female,

        [Parameter(Mandatory=$false)]
        [AllowEmptyString()]
        [string]
        $Item = ""
    )

    if (-not $Name)
    {
        $Name = (Get-Culture).TextInfo.ToTitleCase((Read-Host -Prompt "`nEnter a name").ToLower())
    }
    else
    {
        $Name = (Get-Culture).TextInfo.ToTitleCase(($Name).ToLower())
    }

    if ($Male)
    {
        $pronoun = "He"
    }
    elseif ($Female)
    {
        $pronoun = "She"
    }
    else
    {
        $title   = "Gender"
        $message = "Select $Name's Gender"
        $_male   = New-Object System.Management.Automation.Host.ChoiceDescription "&Male", "Selects male gender."
        $_female = New-Object System.Management.Automation.Host.ChoiceDescription "&Female", "Selects female gender."
        $options = [System.Management.Automation.Host.ChoiceDescription[]]($_male, $_female)
        $result  = $host.UI.PromptForChoice($title, $message, $options, 0)

        switch ($result)
        {
            0 {$pronoun = "He"}
            1 {$pronoun = "She"}
        }
    }

    if (-not $Item)
    {
        $Item = Read-Host -Prompt "`nEnter an item"
    }

    "`n{0} went for a walk in the park. {1} found a {2}. {0} decided to take it home.`n" -f $Name, $pronoun, $Item
}

```

Command line input:

```PowerShell

New-MadLibs -Name hank -Male -Item shank

```

```txt

Hank went for a walk in the park. He found a shank. Hank decided to take it home.

```

Prompt for input:

```PowerShell

New-MadLibs

```

```txt

Enter a name: hank

Gender
Select Hank's Gender
[M] Male  [F] Female  [?] Help (default is "M"): ?
M - Selects male gender.
F - Selects female gender.
[M] Male  [F] Female  [?] Help (default is "M"): m

Enter an item: shank

Hank went for a walk in the park. He found a shank. Hank decided to take it home.

```

Command line input using splatting:

```PowerShell

$paramLists = @(@{Name='mary'; Female=$true; Item="little lamb"},
                @{Name='hank'; Male=$true; Item="shank"},
                @{Name='foo'; Male=$true; Item="bar"})

foreach ($paramList in $paramLists)
{
    New-MadLibs @paramList
}

```

```txt

Mary went for a walk in the park. She found a little lamb. Mary decided to take it home.


Hank went for a walk in the park. He found a shank. Hank decided to take it home.


Foo went for a walk in the park. He found a bar. Foo decided to take it home.

```



## Python


```python
import re

# Optional Python 2.x compatibility
#try: input = raw_input
#except: pass

template = '''<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.'''

def madlibs(template):
    print('The story template is:\n' + template)
    fields = sorted(set( re.findall('<[^>]+>', template) ))
    values = input('\nInput a comma-separated list of words to replace the following items'
                   '\n  %s: ' % ','.join(fields)).split(',')
    story = template
    for f,v in zip(fields, values):
        story = story.replace(f, v)
    print('\nThe story becomes:\n\n' + story)

madlibs(template)
```


```txt
The story template is:
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

Input a comma-separated list of words to replace the following items
  <he or she>,<name>,<noun>: She,Monica L.,cockerel

The story becomes:

Monica L. went for a walk in the park. She
found a cockerel. Monica L. decided to take it home.
```



## Racket

Instead of writing the story in the console, it reads from a file given by the player, this is mainly to keep surprise about the final text

```Racket
(define (get-mad-libs file)
  (with-input-from-file file
    (lambda ()
      (for/fold ((text ""))
        ((line (in-lines)))
        (string-append text line "\n")))))


(define (replace-context mad-libs)
  (define matches
    (regexp-match* #rx"<[a-zA-Z0-9 ]*>" mad-libs))
  (map
   (lambda (context)
     (display (format "~a?" context))
     (cons context (read-line)))
   (remove-duplicates matches)))

(define (play-mad-libs)
  (display "Tell me a file to play Mad Libs: ")
  (define text (get-mad-libs (read-line)))
  (define matches (replace-context text))

  (display
   (for/fold ((mad-story text))
     ((change (in-list matches)))
     (string-replace mad-story (car change) (cdr change)))))

(play-mad-libs)
```


{{out}} with the story from this page

```txt
Tell me a file to play Mad Libs: mad lib.txt
<name>?Don Quixote
<he or she>?he
<noun>?Windmill
Don Quixote went for a walk in the park. he
found a Windmill. Don Quixote decided to take it home
```



## REBOL


```rebol

t: {<name> went for a walk in the park. <he or she> found a <noun>. <name> decided to take it home.}
view layout [a: area wrap t  btn "Done" [x: a/text unview]]
parse x [any [to "<" copy b thru ">" (append w: [] b)] to end]
foreach i unique w [replace/all x i ask join i ": "]  alert x

```



## REXX


```rexx
/*REXX program prompts the user for a template substitutions within a story  (MAD LIBS).*/
parse arg iFID .                                 /*allow user to specify the input file.*/
if iFID=='' | iFID=="," then iFID="MAD_LIBS.TXT" /*Not specified?  Then use the default.*/
@.=                                              /*assign defaults to some variables.   */
$=;          do recs=1  while  lines(iFID)\==0   /*read the input file until it's done. */
             @.recs=linein(iFID);  $=$  @.recs   /*read a record;  and append it to  @  */
             if @.recs=''  then leave            /*Read a blank line?   Then we're done.*/
             end  /*recs*/
recs=recs-1                                      /*adjust for a E─O─F  or  a blank line.*/
pm= 'please enter a word or phrase to replace: ' /*this is part of the  Prompt Message. */
!.=0                                             /*placeholder for phrases in  MAD LIBS.*/
#=0;    do  forever                              /*look for templates within the text.  */
        parse var  $   '<'   ?   ">"   $         /*scan for   <ααα>   stuff in the text.*/
        if ?=''   then leave                     /*No   ααα ?   Then we're all finished.*/
        if !.?    then iterate                   /*Already asked?   Then keep scanning. */
        !.?=1                                    /*mark this   ααα   as being  "found". */
               do  until  ans\=''                /*prompt user for a replacement.       */
               say '───────────'   pm    ?       /*prompt the user with a prompt message*/
               parse pull ans                    /*PULL  obtains the text from console. */
               end   /*forever*/
        #=#+1                                    /*bump the template counter.           */
        old.# = '<'?">";           new.# = ans   /*assign the "old" name and "new" name.*/
        end   /*forever*/
say                                              /*display a blank line for a separator.*/
say;  say copies('═', 79)                        /*display a blank line  and  a fence.  */

        do m=1  for recs                         /*display the text,  line for line.    */
                do n=1  for #                    /*perform substitutions in the text.   */
                @.m=changestr(old.n, @.m, new.n) /*maybe replace text in  @.m  haystack.*/
                end   /*n*/
        say @.m                                  /*display the (new) substituted text.  */
        end   /*m*/

say copies('═', 79)                              /*display a  final (output) fence.     */
say                                              /*stick a fork in it,  we're all done. */
```

Some older REXXes don't have a   '''changestr'''   BIF, so one is included here   ──►   [[CHANGESTR.REX]].



'''output'''   when using the default input fileID:

```txt

─────────── please enter a word or phrase to replace:  name
Mary                              ◄■■■■■■■■■■ user input
─────────── please enter a word or phrase to replace:  he or she
she                               ◄■■■■■■■■■■ user input
─────────── please enter a word or phrase to replace:  noun
little lamb                       ◄■■■■■■■■■■ user input


═══════════════════════════════════════════════════════════════════════════════
Mary went for a walk in the park. she
found a little lamb.  Mary decided to take it home.
═══════════════════════════════════════════════════════════════════════════════

```



## Ring


```ring

temp="<name> went for a walk in the park. <he or she> found a <noun>. <name> decided to take it home."
k = substr(temp,"<")
while k
      replace   = substr(temp,k,substr(temp,">")-k + 1)
      see "replace:" + replace + " with: "
      give with
      while k
            temp  = left(temp,k-1) + with + substr(temp,k + len(replace))
            k = substr(temp,replace)
      end
      k = substr(temp,"<")
end
see temp + nl

```



## Ruby


```ruby
puts "Enter a story, terminated by an empty line:"
story = ""
until (line = gets).chomp.empty?
  story << line
end

story.scan(/(?<=[<]).+?(?=[>])/).uniq.each do |var|
  print "Enter a value for '#{var}': "
  story.gsub!(/<#{var}>/, gets.chomp)
end

puts
puts story
```


Example

```txt
Enter a story, terminated by an empty line:
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

Enter a value for 'name': FOO
Enter a value for 'he or she': BAR
Enter a value for 'noun': BAZ

FOO went for a walk in the park. BAR
found a BAZ. FOO decided to take it home.
```



## Run BASIC


```runbasic
temp$="<name> went for a walk in the park. <he or she> found a <noun>. <name> decided to take it home."
k = instr(temp$,"<")
while k
    replace$   = mid$(temp$,k,instr(temp$,">")-k + 1)
    print "Replace:";replace$;" with:"; :input with$
    while k
        temp$  = left$(temp$,k-1) + with$ + mid$(temp$,k + len(replace$))
        k      = instr(temp$,replace$,k)
    wend
k = instr(temp$,"<")
wend
print temp$
wait
```

```txt

Replace:<name> with:?Fred
Replace:<he or she> with:?he
Replace:<noun> with:?cat
Fred went for a walk in the park. he found a cat. Fred decided to take it home.

```


## Rust

```rust
extern crate regex;

use regex::Regex;
use std::collections::HashMap;
use std::io;

fn main() {
    let mut input_line = String::new();
    let mut template = String::new();

    println!("Please enter a multi-line story template with <parts> to replace, terminated by a blank line.\n");
    loop {
        io::stdin()
            .read_line(&mut input_line)
            .expect("The read line failed.");
        if input_line.trim().is_empty() {
            break;
        }
        template.push_str(&input_line);
        input_line.clear();
    }

    let re = Regex::new(r"<[^>]+>").unwrap();
    let mut parts: HashMap<_, _> = re
        .captures_iter(&template)
        .map(|x| (x.get(0).unwrap().as_str().to_string(), "".to_string()))
        .collect();
    if parts.is_empty() {
        println!("No <parts> to replace.\n");
    } else {
        for (k, v) in parts.iter_mut() {
            println!("Please provide a replacement for {}: ", k);
            io::stdin()
                .read_line(&mut input_line)
                .expect("The read line failed.");
            *v = input_line.trim().to_string();
            println!();
            template = template.replace(k, v);
            input_line.clear();
        }
    }
    println!("Resulting story:\n\n{}", template);
}
```

```txt

Please enter a multi-line story template with <parts> to replace, terminated by a blank line.

<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

Please provide a replacement for <name>:
John Wick

Please provide a replacement for <he or she>:
He

Please provide a replacement for <noun>:
puppy

Resulting story:

John Wick went for a walk in the park. He
found a puppy. John Wick decided to take it home.

```



## Scala


```Scala
object MadLibs extends App{
  val input = "<name> went for a walk in the park. <he or she>\nfound a <noun>. <name> decided to take it home."
  println(input)
  println

  val todo = "(<[^>]+>)".r
  val replacements = todo.findAllIn(input).toSet.map{found: String =>
    found -> readLine(s"Enter a $found ")
  }.toMap

  val output = todo.replaceAllIn(input, found => replacements(found.matched))
  println
  println(output)
}
```

```txt
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

Enter a <name> Foo
Enter a <he or she> He
Enter a <noun> bar

Foo went for a walk in the park. He
found a bar. Foo decided to take it home.
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var string: story is "";
    var string: line is "";
    var integer: pos1 is 0;
    var integer: pos2 is 1;
    var string: field is "";
  begin
    writeln("Enter a story template, terminated by an empty line:");
    repeat
      readln(line);
      if line <> "" then
        story &:= line & "\n";
      end if;
    until line = "";
    pos1 := pos(story, '<');
    while pos1 <> 0 and pos2 <> 0 do
      pos2 := pos(story, '>', pos1);
      if pos2 <> 0 then
        field := story[pos1 .. pos2];
        write("Enter a value for " <& field <& ": ");
        story := replace(story, field, getln(IN));
        pos1 := pos(story, '<', pos1);
      end if;
    end while;
    writeln;
    writeln("The story becomes:");
    write(story);
  end func;
```


```txt

Enter a story template, terminated by an empty line:
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

Enter a value for <name>: Katharine
Enter a value for <he or she>: she
Enter a value for <noun>: goldbar

The story becomes:
Katharine went for a walk in the park. she
found a goldbar. Katharine decided to take it home.

```



## Sidef

```ruby
var story = ARGF.slurp;

var blanks = Hash.new;
while (var m = /<(.*?)>/.gmatch(story)) {
    blanks.append(m[0]);
}

blanks.keys.sort.each { |blank|
    var replacement = Sys.scanln("#{blank}: ");
    blanks{blank} = replacement;
}

print story.gsub(/<(.*?)>/, {|s1| blanks{s1} });
```



## tbas


```qbasic
SUB BETWEEN$(TXT$, LHS$, RHS$, AFTER)
	LET LHS = POS(TXT$, LHS$, AFTER)
	IF LHS = 0 THEN
		'LHS$ NOT FOUND IN TXT$. RETURN EMPTY STRING
		RETURN ""
		EXIT SUB
	END IF
	LET RHS = POS(TXT$, RHS$, LHS + LEN(LHS$))
	IF RHS = 0 THEN
		'NO RHS$ FOUND IMMEDIATELY AFTER LHS$ IN TXT$. RETURN EMPTY STRING.
		RETURN ""
		EXIT SUB
	END IF
	RETURN SEG$(TXT$, LHS + LEN(LHS$), RHS-1)
END SUB

DECLARE SUB REPLACE$(INTHIS$, FINDTHIS$, WITHTHIS$)
	LET T = POS(INTHIS$, FINDTHIS$)
	RETURN INTHIS$
	IF T <> 0 THEN
		MID$(INTHIS$, T, LEN(FINDTHIS$)) = WITHTHIS$
		RETURN INTHIS$
	END IF
END SUB

LET STORY$ = ""
LET STORYLINE$ = ""
WHILE TRUE
	LINE INPUT "Enter line of story (empty line to stop)"; STORYLINE$
	IF STORYLINE$ = "" THEN
		EXIT WHILE
	END IF
	STORY$ = STORY$ + " " + STORYLINE$
END WHILE

WHILE TRUE
	LET TXT$ = ""
	LET KEY$ = BETWEEN$(STORY$, "<", ">", 0)
	IF KEY$ <> "" THEN
		PRINT "PLEASE ENTER A " + KEY$;
		LINE INPUT TXT$
		LET SRCH$ = "<" + KEY$ + ">"
		STORY$ = REPLACE$(STORY$, SRCH$, TXT$)
	ELSE
		EXIT WHILE
	END IF
END WHILE

PRINT STORY$
```



## Tcl


```tcl
package require Tcl 8.5

# Read the template...
puts [string repeat "-" 70]
puts "Enter the story template, ending with a blank line"
while {[gets stdin line] > 0} {
    append content $line "\n"
}

# Read the mapping...
puts [string repeat "-" 70]
set mapping {}
foreach piece [regexp -all -inline {<[^>]+>} $content] {
    if {[dict exists $mapping $piece]} continue
    puts -nonewline "Give me a $piece: "
    flush stdout
    dict set mapping $piece [gets stdin]
}

# Apply the mapping and print...
puts [string repeat "-" 70]
puts -nonewline [string map $mapping $content]
puts [string repeat "-" 70]
```

Sample session:

```txt

----------------------------------------------------------------------
Enter the story template, ending with a blank line
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

----------------------------------------------------------------------
Give me a <name>: Wonko the Sane
Give me a <he or she>: He
Give me a <noun>: wild weasel
----------------------------------------------------------------------
Wonko the Sane went for a walk in the park. He
found a wild weasel. Wonko the Sane decided to take it home.
----------------------------------------------------------------------

```



## VBScript


```vb

Function mad_libs(s)
	Do
		If InStr(1,s,"<") <> 0 Then
			start_position = InStr(1,s,"<") + 1
			end_position = InStr(1,s,">")
			parse_string = Mid(s,start_position,end_position-start_position)
			WScript.StdOut.Write parse_string & "? "
			input_string = WScript.StdIn.ReadLine
			s = Replace(s,"<" & parse_string & ">",input_string)
		Else
			Exit Do
		End If
	Loop
	mad_libs = s
End Function

WScript.StdOut.Write mad_libs("<name> went for a walk in the park. <he or she> found a <noun>. <name> decided to take it home.")
WScript.StdOut.WriteLine

```


```txt

F:\>cscript /nologo mad_libs.vbs
name? babe
he or she? she
noun? cat
babe went for a walk in the park. she found a cat. babe decided to take it home.

```




## Visual Basic .NET


"\<" can be used to escape a "<" outside of a template (the escape is not recognized within a template), and "\>" can be used to escape a ">" inside of a template. Templates can span lines. A possible improvement from a UX standpoint is to check for template validity before beginning to request for substitutions; currently, unclosed templates are only found when they are encountered in the replacement process.


```vbnet
Imports System.Text

Module Program
    Private Function GetStory() As String
        Dim input As New StringBuilder()
        Do
            Dim nextLine = Console.ReadLine()
            If String.IsNullOrEmpty(nextLine) Then Exit Do
            input.AppendLine(nextLine)
        Loop

        Dim story = input.ToString()
        Return story
    End Function

    Sub Main()
        Dim input As String = GetStory()

        Dim result As New StringBuilder()
        Dim replacements As New Dictionary(Of String, String)

        For i = 0 To input.Length - 1
            Dim curChar = input(i)

            ' For all characters but '<', append it and move on.
            If curChar <> "<"c Then
                result.Append(curChar)
            Else
                ' If the character before was a backslash, then replace the backslash in the result with a '<' and move on.
                If i > 0 AndAlso input(i - 1) = "\"c Then
                    result(result.Length - 1) = "<"c
                    Continue For
                End If

                ' Search for the first '>' that isn't preceded by a backslash.
                Dim closeBracketPos = -1
                For ind = i To input.Length - 1
                    If input(ind) = ">"c AndAlso input(ind - 1) <> "\"c Then
                        closeBracketPos = ind
                        Exit For
                    End If
                Next

                ' The search failed to find a '>'.
                If closeBracketPos < 0 Then
                    Console.WriteLine($"ERROR: Template starting at position {i} is not closed.")
                    Environment.Exit(-1)
                End If

                ' The text between the current character and the found '>' character, with escape sequences simplified.
                Dim key As String = input.Substring(i + 1, closeBracketPos - i - 1).Replace("\>", ">", StringComparison.Ordinal)

                Dim subst As String = Nothing
                ' Ask for and store a replacement value if there isn't already one for the key.
                If Not replacements.TryGetValue(key, subst) Then
                    Console.Write($"Enter a {key}: ")
                    subst = Console.ReadLine()
                    replacements.Add(key, subst)
                End If

                result.Append(subst)
                i = closeBracketPos
            End If
        Next

        Console.WriteLine()
        Console.Write(result)
    End Sub
End Module
```


```txt
<name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

Enter a name: Foo
Enter a he or she: it
Enter a noun: bar

Foo went for a walk in the park. it
found a bar. Foo decided to take it home.
```


```txt
<multi-
line
template>
...
\<\<   <\>thing<>
...
\>     <\<thing>
...
<<<thing>>>

Enter a multi-
line
template: foo
Enter a >thing<: bar
Enter a \<thing: baz
Enter a <<thing: qux

foo
...
<<   bar
...
\>     baz
...
qux>>
```



## zkl


```zkl
story,line,re:=Data(),"",RegExp("(<[^>]+>)");
do{ line=ask("Story: "); story.write(line,"\n") }while(line);
while(re.search(story,True)){
   z,ml,N:=re.matched,z[1],z[0][1]; // z=( (0,6),"<name>" )
   s:=ask("Text to replace ",ml," with: ");
   while(Void!=(n:=story.find(ml))){ story[n,N]=s } // replace all <names>s
}
println("-----------------");
story.text.print();
```

```txt

Story: <name> went for a walk in the park. <he or she>
found a <noun>. <name> decided to take it home.

Story: Story: Text to replace <name> with: Susan
Text to replace <he or she> with: she
Text to replace <noun> with: rock
-----------------
Susan went for a walk in the park. she
found a rock. Susan decided to take it home.


```

The "Story: Story:" was because I pasted the story causing i/o to get out of sync.
