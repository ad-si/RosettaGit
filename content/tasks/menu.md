+++
title = "Menu"
description = ""
date = 2019-10-08T11:52:20Z
aliases = []
[extra]
id = 4314
[taxonomies]
categories = ["task", "Text processing"]
tags = []
languages = [
  "algol_68",
  "autohotkey",
  "awk",
  "axe",
  "basic",
  "batch_file",
  "bbc_basic",
  "brat",
  "c",
  "ceylon",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "erre",
  "es",
  "euphoria",
  "factor",
  "fantom",
  "forth",
  "fortran",
  "gambas",
  "go",
  "haskell",
  "hicest",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "logo",
  "lua",
  "matlab",
  "min",
  "mumps",
  "nim",
  "ocaml",
  "openedge_progress",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "prodos",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "tcl",
  "unix_shell",
  "ursa",
  "vbscript",
  "xpl0",
  "zkl",
]
+++

## Task

Given a prompt and a list containing a number of strings of which one is to be selected, create a function that:

* prints a textual menu formatted as an index value followed by its corresponding string for each item in the list;
* prompts the user to enter a number;
* returns the string corresponding to the selected index number.



The function should reject input that is not an integer or is out of range by redisplaying the whole menu before asking again for a number. The function should return an empty string if called with an empty list.

For test purposes use the following four phrases in a list:
    fee fie
    huff and puff
    mirror mirror
    tick tock

;Note:
This task is fashioned after the action of the [http://www.softpanorama.org/Scripting/Shellorama/Control_structures/select_statements.shtml Bash select statement].

=={{header|Ada|}}==

```Ada
with ada.text_io,Ada.Strings.Unbounded; use  ada.text_io, Ada.Strings.Unbounded;

procedure menu is
	type menu_strings is array (positive range <>) of Unbounded_String ;
	function "+" (s : string) return Unbounded_String is (To_Unbounded_String (s));

	function choice (m : menu_strings; prompt : string) return string is
	begin
		if m'length > 0 then
			loop
				put_line (prompt);
				for i in m'range loop
					put_line (i'img &") " & To_String (m(i)));
				end loop;
				begin
					return To_String (m(positive'value (get_line)));
					exception when others => put_line ("Try again !");
				end;
			end loop;
		end if;
		return "";
	end choice;

begin
	put_line ("You chose " &
		choice ((+"fee fie",+"huff and puff",+"mirror mirror",+"tick tock"),"Enter your choice "));
end menu;
```



## ALGOL 68

```algol68
PROC menu select := (FLEX[]STRING items, UNION(STRING, VOID) prompt)STRING:
(
        INT choice;

        IF LWB items <= UPB items THEN
                WHILE
                        FOR i FROM LWB items TO UPB items DO
                                printf(($g(0)") "gl$, i, items[i]))
                        OD;
                        CASE prompt IN
                                (STRING prompt):printf(($g" "$, prompt)),
                                (VOID):printf($"Choice ? "$)
                        ESAC;
                        read((choice, new line));
                # WHILE # 1 > choice OR choice > UPB items
                DO SKIP OD;
                items[choice]
        ELSE
                ""
        FI
);

test:(
        FLEX[0]STRING items := ("fee fie", "huff and puff", "mirror mirror", "tick tock");
        STRING prompt := "Which is from the three pigs : ";

        printf(($"You chose "g"."l$, menu select(items, prompt)))
)
```

Output:

```txt

1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Which is from the three pigs :  2
You chose huff and puff.

```



## AutoHotkey

```autohotkey
GoSub, CreateGUI
return

Submit:
Gui, Submit, NoHide
If Input =
 GuiControl,,Output
Else If Input not between 1 and 4
{
 Gui, Destroy
 Sleep, 500
 GoSub, CreateGUI
}
Else {
 GuiControlGet, string,,Text%Input%
 GuiControl,,Output,% SubStr(string,4)
}
return

CreateGUI:
list = fee fie,huff and puff,mirror mirror,tick tock
Loop, Parse, list, `,
 Gui, Add, Text, vText%A_Index%, %A_Index%: %A_LoopField%
Gui, Add, Text, ym, Which is from the three pigs?
Gui, Add, Edit, vInput gSubmit
Gui, Add, Edit, vOutput
Gui, Show
return

GuiClose:
ExitApp
```



## AWK


```AWK

# syntax: GAWK -f MENU.AWK
BEGIN {
    print("you picked:",menu(""))
    print("you picked:",menu("fee fie:huff and puff:mirror mirror:tick tock"))
    exit(0)
}
function menu(str,  ans,arr,i,n) {
    if (str == "") {
      return
    }
    n = split(str,arr,":")
    while (1) {
      print("")
      for (i=1; i<=n; i++) {
        printf("%d - %s\n",i,arr[i])
      }
      printf("? ")
      getline ans
      if (ans in arr) {
        return(arr[ans])
      }
      print("invalid choice")
    }
}

```



## Axe

In Axe, static data (such as strings) is laid out sequentially in memory. So the H in "HUFF" is the byte after the null terminator for "FIE". However, null terminators are only added to strings when they are stored with the store symbol →. strGet returns a pointer to the start of the nth null-terminated string in the data, which is why the strings must be laid out in memory correctly.

```axe
"FEE FIE"→Str1
"HUFF AND PUFF"→Str2
"MIRROR MIRROR"→Str3
"TICK TOCK"→Str4
For(I,1,4)
 Disp I▶Hex+3,":",strGet(Str1,I-1),i
End
Disp "NUMBER? "
input→A
{A}-'0'→N
If N<1 or N>4
 Disp "BAD NUMBER",i
 Return
End
Disp strGet(Str1,N-1),i
```



## BASIC

```qbasic
 function sel$(choices$(), prompt$)
   if ubound(choices$) - lbound(choices$) = 0 then sel$ = ""
   ret$ = ""
   do
      for i = lbound(choices$) to ubound(choices$)
         print i; ": "; choices$(i)
      next i
      input ;prompt$, index
      if index <= ubound(choices$) and index >= lbound(choices$) then ret$ = choices$(index)
   while ret$ = ""
   sel$ = ret$
end function
```



```dos
@echo off & setlocal enabledelayedexpansion

set "menuChoices="fee fie","huff and puff","mirror mirror","tick tock""

call :menu

pause>nul & exit


:menu
	if defined menuChoices (
		set "counter=0" & for %%a in (%menuChoices%) do (
			set /a "counter+=1"
			set "currentMenuChoice=%%a"
			set option[!counter!]=!currentMenuChoice:"=!
		)
	)
:tryagain
cls&echo.
for /l %%a in (1,1,%counter%) do echo %%a^) !option[%%a]!
echo.
set /p "input=Choice 1-%counter%: "
echo.
for /l %%a in (1,1,%counter%) do (
	if !input! equ %%a echo You chose [ %%a^) !option[%%a]! ] & goto :EOF
)
echo.
echo.Invalid Input. Please try again...
pause
goto :tryagain
```



## Batch File


```dos

@echo off

call:menu "fee fie" "huff and puff" "mirror mirror" "tick tock"
pause>nul
exit /b

:menu
cls
setlocal enabledelayedexpansion
set count=0
set reset=endlocal ^& goto menu
:menuloop
for %%i in (%*) do (
	set /a count+=1
	set string[!count!]=%%~i
	echo string[!count!] = %%~i
)
echo.
set /p choice=^>
if "%choice%"=="" %reset%
set "isNum="
for /f "delims=0123456789" %%i in ("%choice%") do set isNum=%%i
if defined isNum %reset%
if %choice% gtr %count% %reset%
echo.!string[%choice%]!
goto:eof

```




## BBC BASIC


```bbcbasic
      DIM list$(4)
      list$() = "fee fie", "huff and puff", "mirror mirror", "tick tock"
      selected$ = FNmenu(list$(), "Please make a selection: ")
      PRINT selected$
      END

      DEF FNmenu(list$(), prompt$)
      LOCAL index%, select$
      IF SUM(list$()) = "" THEN = ""
      REPEAT
        CLS
        FOR index% = 0 TO DIM(list$() ,1)
          IF list$(index%)<>"" PRINT ; index% ":", list$(index%)
        NEXT
        PRINT prompt$ ;
        INPUT "" select$
        index% = VAL(select$)
        IF select$<>STR$(index%) index% = -1
        IF index%>=0 IF index%<=DIM(list$() ,1) IF list$(index%)="" index% = -1
      UNTIL index%>=0 AND index%<=DIM(list$(), 1)
      = list$(index%)
```

Empty entries in the list are not offered as options, nor accepted as a selection.


## Brat


```brat
menu = { prompt, choices |
  true? choices.empty?
  { "" }
  {
    choices.each_with_index { c, i |
      p "#{i}. #{c}"
    }

    selection = ask prompt

      true? selection.numeric?
      { selection = selection.to_i
        true? selection < 0 || { selection >= choices.length }
          { p "Selection is out of range"; menu prompt, choices }
          { choices[selection] }
      }
    { p "Selection must be a number"; menu prompt, choices }
  }
}

p menu "Selection: " ["fee fie" "huff and puff" "mirror mirror" "tick tock"]
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

const char *menu_select(const char *const *items, const char *prompt);

int
main(void)
{
	const char *items[] = {"fee fie", "huff and puff", "mirror mirror", "tick tock", NULL};
	const char *prompt = "Which is from the three pigs?";

	printf("You chose %s.\n", menu_select(items, prompt));

	return EXIT_SUCCESS;
}

const char *
menu_select(const char *const *items, const char *prompt)
{
	char buf[BUFSIZ];
	int i;
	int choice;
	int choice_max;

	if (items == NULL)
		return NULL;

	do {
		for (i = 0; items[i] != NULL; i++) {
			printf("%d) %s\n", i + 1, items[i]);
		}
		choice_max = i;
		if (prompt != NULL)
			printf("%s ", prompt);
		else
			printf("Choice? ");
		if (fgets(buf, sizeof(buf), stdin) != NULL) {
			choice = atoi(buf);
		}
	} while (1 > choice || choice > choice_max);

	return items[choice - 1];
}
```



## C++


```cpp
#include <iostream>
#include <string>
#include <vector>

void print_menu(const std::vector<std::string>& terms)
{
    for (size_t i = 0; i < terms.size(); i++) {
        std::cout << i + 1 << ") " << terms[i] << '\n';
    }
}

int parse_entry(const std::string& entry, int max_number)
{
    int number = std::stoi(entry);
    if (number < 1 || number > max_number) {
        throw std::invalid_argument("");
    }

    return number;
}

std::string data_entry(const std::string& prompt, const std::vector<std::string>& terms)
{
    if (terms.empty()) {
        return "";
    }

    int choice;
    while (true) {
        print_menu(terms);
        std::cout << prompt;

        std::string entry;
        std::cin >> entry;

        try {
            choice = parse_entry(entry, terms.size());
            return terms[choice - 1];
        } catch (std::invalid_argument&) {
            // std::cout << "Not a valid menu entry!" << std::endl;
        }
    }
}

int main()
{
    std::vector<std::string> terms = {"fee fie", "huff and puff", "mirror mirror", "tick tock"};
    std::cout << "You chose: " << data_entry("> ", terms) << std::endl;
}

```



## C#


```c#

using System;
using System.Collections.Generic;

public class Menu
{
        static void Main(string[] args)
        {
            List<string> menu_items = new List<string>() { "fee fie", "huff and puff", "mirror mirror", "tick tock" };
            //List<string> menu_items = new List<string>();
            Console.WriteLine(PrintMenu(menu_items));
            Console.ReadLine();
        }
        private static string PrintMenu(List<string> items)
        {
            if (items.Count == 0)
                return "";

            string input = "";
            int i = -1;
            do
            {
                for (int j = 0; j < items.Count; j++)
                    Console.WriteLine("{0}) {1}", j, items[j]);

                Console.WriteLine("What number?");
                input = Console.ReadLine();

            } while (!int.TryParse(input, out i) || i >= items.Count || i < 0);
            return items[i];
        }
}

```



## Ceylon


```ceylon
"Run the module `menu`."
shared void run() {
 	value selection = menu("fee fie", "huff And puff", "mirror mirror", "tick tock");
 	print(selection);
}

String menu(String* strings) {
	if(strings.empty) {
		return "";
	}
	value entries = map(zipEntries(1..strings.size, strings));
	while(true) {
		for(index->string in entries) {
			print("``index``) ``string``");
		}
		process.write("> ");
		value input = process.readLine();
		if(exists input, exists int = parseInteger(input), exists string = entries[int]) {
			return string;
		}
	}
}


```



## Clojure


```clojure
(defn menu [prompt choices]
  (if (empty? choices)
    ""
    (let [menutxt (apply str (interleave
                              (iterate inc 1)
                              (map #(str \space % \newline) choices)))]
      (println menutxt)
      (print prompt)
      (flush)
      (let [index (read-string (read-line))]
        ; verify
        (if (or (not (integer? index))
                (> index (count choices))
                (< index 1))
          ; try again
          (recur prompt choices)
          ; ok
          (nth choices (dec index)))))))

(println "You chose: "
         (menu "Which is from the three pigs: "
               ["fee fie" "huff and puff" "mirror mirror" "tick tock"]))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Test-Prompt-Menu.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  Num-Options    USAGE UNSIGNED-INT VALUE 4.
       01  Example-Menu.
           03  Example-Options-Data.
               05  FILLER PIC X(30) VALUE "fee fie".
               05  FILLER PIC X(30) VALUE "huff and puff".
               05  FILLER PIC X(30) VALUE "mirror mirror".
               05  FILLER PIC X(30) VALUE "tick tock".

           03  Example-Options-Values REDEFINES Example-Options-Data.
               05  Example-Options PIC X(30) OCCURS 4 TIMES.

       01  Chosen-Option PIC X(30).

       PROCEDURE DIVISION.
           CALL "Prompt-Menu" USING BY CONTENT Num-Options
               BY CONTENT Example-Menu
               BY REFERENCE Chosen-Option

           DISPLAY "You chose: " Chosen-Option

           GOBACK
           .

       END PROGRAM Test-Prompt-Menu.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. Prompt-Menu.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       01  User-Input        USAGE UNSIGNED-INT.
       01  Input-Flag        PIC X.
           88  Valid-Input   VALUE "Y".

       01  Options-Index     USAGE UNSIGNED-INT.
       01  Index-Display     PIC Z(10).

       LINKAGE SECTION.

       01  Num-Options       USAGE UNSIGNED-INT.
       01  Menu-Options.
           03  Options-Table PIC X(30) OCCURS 0 TO 10000000 TIMES
               DEPENDING ON Num-Options.

       01  Chosen-Option     PIC X(30).

       PROCEDURE DIVISION USING Num-Options Menu-Options Chosen-Option.
       Main.
           IF Num-Options = 0
               MOVE SPACES TO Chosen-Option
               GOBACK
           END-IF

           PERFORM UNTIL Valid-Input
               PERFORM Display-Menu-Options

               DISPLAY "Choose an option: " WITH NO ADVANCING
               ACCEPT User-Input

               PERFORM Validate-Input
           END-PERFORM

           MOVE Options-Table (User-Input) TO Chosen-Option

           GOBACK
           .

       Display-Menu-Options.
           PERFORM VARYING Options-Index FROM 1 BY 1
                   UNTIL Num-Options < Options-Index
               MOVE Options-Index TO Index-Display
               DISPLAY
                   Index-Display ". " Options-Table (Options-Index)
               END-DISPLAY
           END-PERFORM
           .

       Validate-Input.
           IF User-Input = 0 OR > Num-Options
               DISPLAY "Invalid input."
           ELSE
               SET Valid-Input TO TRUE
           END-IF
           .

       END PROGRAM Prompt-Menu.
```



## Common Lisp


```lisp
(defun select (prompt choices)
  (if (null choices)
    ""
    (do (n)
        ((and n (<= 0 n (1- (length choices))))
         (nth n choices))
      (format t "~&~a~%" prompt)
      (loop for n from 0
            for c in choices
            do (format t "  ~d) ~a~%" n c))
      (force-output)
      (setf n (parse-integer (read-line *standard-input* nil)
                             :junk-allowed t)))))
```



## D


```d
import std.stdio, std.conv, std.string, std.array, std.typecons;

string menuSelect(in string[] entries) {
    static Nullable!(int, -1) validChoice(in string input,
                                          in int nEntries)
    pure nothrow {
        try {
            immutable n = input.to!int;
            return typeof(return)((n >= 0 && n <= nEntries) ? n : -1);
        } catch (Exception e) // Very generic
            return typeof(return)(-1); // Not valid.
    }

    if (entries.empty)
        return "";

    while (true) {
        "Choose one:".writeln;
        foreach (immutable i, const entry; entries)
            writefln("  %d) %s", i, entry);
        "> ".write;
        immutable input = readln.chomp;
        immutable choice = validChoice(input, entries.length - 1);
        if (choice.isNull)
            "Wrong choice.".writeln;
        else
            return entries[choice]; // We have a valid choice.
    }
}

void main() {
    immutable items = ["fee fie", "huff and puff",
                       "mirror mirror", "tick tock"];
    writeln("You chose '", items.menuSelect, "'.");
}
```

```txt
Choose one:
  0) fee fie
  1) huff and puff
  2) mirror mirror
  3) tick tock
> 2
You chose 'mirror mirror'.
```



## Elixir


```elixir
defmodule Menu do
  def select(_, []), do: ""
  def select(prompt, items) do
    IO.puts ""
    Enum.with_index(items) |> Enum.each(fn {item,i} -> IO.puts " #{i}. #{item}" end)
    answer = IO.gets("#{prompt}: ") |> String.strip
    case Integer.parse(answer) do
      {num, ""} when num in 0..length(items)-1 -> Enum.at(items, num)
      _ -> select(prompt, items)
    end
  end
end

# test empty list
response = Menu.select("Which is empty", [])
IO.puts "empty list returns: #{inspect response}"

# "real" test
items = ["fee fie", "huff and puff", "mirror mirror", "tick tock"]
response = Menu.select("Which is from the three pigs", items)
IO.puts "you chose: #{inspect response}"
```


```txt

empty list returns: ""

 0. fee fie
 1. huff and puff
 2. mirror mirror
 3. tick tock
Which is from the three pigs: 4

 0. fee fie
 1. huff and puff
 2. mirror mirror
 3. tick tock
Which is from the three pigs: 3
you chose: "tick tock"

```



## ERRE


```ERRE

PROCEDURE Selection(choices$[],prompt$->sel$)
   IF UBOUND(choices$,1)-LBOUND(choices$,1)=0 THEN
      sel$=""
      EXIT PROCEDURE
   END IF
   ret$=""
   REPEAT
      FOR i=LBOUND(choices$,1) TO UBOUND(choices$,1) DO
         PRINT(i;": ";choices$[i])
      END FOR
      PRINT(prompt$;)
      INPUT(index)
      IF index<=UBOUND(choices$,1) AND index>=LBOUND(choices$,1) THEN ret$=choices$[index] END IF
   UNTIL ret$<>""
   sel$=ret$
END PROCEDURE

```



## Euphoria


```euphoria
include get.e

function menu_select(sequence items, object prompt)
    if length(items) = 0 then
        return ""
    else
        for i = 1 to length(items) do
            printf(1,"%d) %s\n",{i,items[i]})
        end for

        if atom(prompt) then
            prompt = "Choice?"
        end if

        return items[prompt_number(prompt,{1,length(items)})]
    end if
end function

constant items = {"fee fie", "huff and puff", "mirror mirror", "tick tock"}
constant prompt = "Which is from the three pigs? "

printf(1,"You chose %s.\n",{menu_select(items,prompt)})
```



## Factor


```factor
USING: formatting io kernel math math.parser sequences ;

: print-menu ( seq -- )
    [ 1 + swap "%d - %s\n" printf ] each-index
    "Your choice? " write flush ;

: (select) ( seq -- result )
    dup print-menu readln string>number dup integer? [
        drop 1 - swap 2dup bounds-check?
        [ nth ] [ nip (select) ] if
    ] [ drop (select) ] if* ;

: select ( seq -- result ) [ "" ] [ (select) ] if-empty ;
```


Example usage:

```txt
( scratchpad ) { "fee fie" "huff and puff" "mirror mirror" "tick tock" } select
1 - fee fie
2 - huff and puff
3 - mirror mirror
4 - tick tock
Your choice? 1

--- Data stack:
"fee fie"
```



## Fantom

```fantom
class Main
{
  static Void displayList (Str[] items)
  {
    items.each |Str item, Int index|
    {
      echo ("$index: $item")
    }
  }

  public static Str getChoice (Str[] items)
  {
    selection := -1
    while (selection == -1)
    {
      displayList (items)
      Env.cur.out.print ("Select: ").flush
      input := Int.fromStr(Env.cur.in.readLine, 10, false)
      if (input != null)
      {
        if (input >= 0 && input < items.size)
        {
          selection = input
        }
      }
      echo ("Try again")
    }
    return items[selection]
  }

  public static Void main ()
  {
    choice := getChoice (["fee fie", "huff and puff", "mirror mirror", "tick tock"])
    echo ("You chose: $choice")
  }
}
```



## Forth


### Idiomatic Forth

Out of the box Forth does not have lists. This version uses strings and a vector table, which arguably is more how  one would do this task in Forth. It returns a nil string if a nil string is given otherwise the input string becomes the title of the menu.

```Forth
\ Rosetta Code Menu Idiomatic Forth

\ vector table compiler
: CASE:  ( -- ) CREATE ;
: |      ( -- <text>)  '  ,  ;  IMMEDIATE
: ;CASE  ( -- ) DOES>  SWAP CELLS + @ EXECUTE ;

: NIL      ( -- addr len) S" " ;
: FEE      ( -- addr len) S" fee fie" ;
: HUFF     ( -- addr len) S" huff and puff" ;
: MIRROR   ( -- addr len) S" mirror mirror" ;
: TICKTOCK ( -- addr len) S" tick tock" ;

CASE: SELECT ( n -- addr len)
     | NIL | FEE | HUFF | MIRROR | TICKTOCK
;CASE

CHAR 1 CONSTANT '1'
CHAR 4 CONSTANT '4'
: BETWEEN ( n low hi -- ?)  1+ WITHIN ;

: MENU ( addr len -- addr len )
       DUP 0=
       IF
          2DROP  NIL  EXIT
       ELSE
          BEGIN
             CR
             CR 2DUP 3 SPACES   TYPE
             CR   ." 1 " 1 SELECT TYPE
             CR   ." 2 " 2 SELECT TYPE
             CR   ." 3 " 3 SELECT TYPE
             CR   ." 4 " 4 SELECT TYPE
             CR ." Choice: " KEY DUP EMIT
             DUP '1' '4' BETWEEN 0=
          WHILE
              DROP
          REPEAT
          -ROT 2DROP    \ drop input string
          CR [CHAR] 0 -  SELECT
       THEN
;
```



### If there must be lists

Here we extend Forth to support simple lists and complete the task using the language extensions.

```forth
\ Rosetta Menu task with Simple lists in Forth

: STRING, ( caddr len -- ) HERE  OVER CHAR+  ALLOT  PLACE ;
: "       ( -- ) [CHAR] " PARSE  STRING, ;

: {       ( -- ) ALIGN 0 C, ;
: }       ( -- ) { ;

: {NEXT} ( str -- next_str)  COUNT + ;
: {NTH}  ( n array_addr -- str)  SWAP 0 DO {NEXT} LOOP ;

: {LEN}  ( array_addr -- )  \ count strings in a list
          0 >R                      \ Counter on Rstack
          {NEXT}                    \ skip 1st empty string
          BEGIN
             {NEXT} DUP C@          \ Fetch length byte
          WHILE                     \ While true
             R> 1+ >R               \ Inc. counter
          REPEAT
          DROP
          R> ;                      \ return counter to data stack

: {TYPE}    ( $ -- ) COUNT TYPE ;
: '"'    ( -- )   [CHAR] " EMIT ;
: {""}   ( $ -- )  '"' SPACE {TYPE} '"' SPACE ;
: }PRINT ( n array -- ) {NTH} {TYPE} ;

\
### == TASK BEGINS ==

CREATE GOODLIST
       { " fee fie"
         " huff and puff"
         " mirror mirror"
         " tick tock" }

CREATE NIL  {   }

CHAR 1 CONSTANT '1'
CHAR 4 CONSTANT '4'
CHAR 0 CONSTANT '0'

: BETWEEN ( n low hi -- ?)  1+ WITHIN ;

: .MENULN ( n -- n) DUP '0' + EMIT SPACE OVER }PRINT ;

: MENU    ( list -- string )
       DUP {LEN} 0=
       IF
           DROP NIL
       ELSE
          BEGIN
             CR
             CR 1 .MENULN
             CR 2 .MENULN
             CR 3 .MENULN
             CR 4 .MENULN
             CR ." Choice: " KEY DUP EMIT
             DUP '1' '4' BETWEEN
         0= WHILE
              DROP
          REPEAT
         [CHAR] 0 -
         CR SWAP {NTH}
       THEN
;
```


Test at the gForth console
<PRE>GOODLIST MENU

1 fee fie
2 huff and puff
3 mirror mirror
4 tick tock
Choice: 0

1 fee fie
2 huff and puff
3 mirror mirror
4 tick tock
Choice: Q

1 fee fie
2 huff and puff
3 mirror mirror
4 tick tock
Choice: 2
 ok
{TYPE} huff and puff ok
  ok
NIL MENU  ok
{TYPE}  ok</PRE>


## Fortran


Please find the build instructions in the comments at the start of the FORTRAN 2008 source. Compiler: gfortran from the GNU compiler collection. Command interpreter: bash.

```FORTRAN

!a=./f && make $a && OMP_NUM_THREADS=2 $a
!gfortran -std=f2008 -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none f.f08 -o f

      module menu
      public :: selector
      contains

      function selector(title,options) result(choice)
      character(len=*),intent(in) :: title
      character(len=*),dimension(:),intent(in) :: options
      character(len=len(options)) :: choice
      integer :: i,ichoose,ios,n

      choice = ""

      n = size(options)
      if (n > 0) then
        do
          print "(a)",title
          print "(i8,"", "",a)",(i,options(i),i=1,n)
          read (*,fmt="(i8)",iostat=ios) ichoose

          if (ios == -1) exit ! EOF error
          if (ios /= 0) cycle ! other error
          if (ichoose < 1) cycle
          if (ichoose > n) cycle ! out-of-bounds

          choice = options(ichoose)
          exit
        end do
      end if
      end function selector
      end module menu

      program menu_demo
      use menu
      character(len=14),dimension(:),allocatable :: zero_items,fairytale
      character(len=len(zero_items)) :: s

      !! empty list demo
      allocate(zero_items(0))
      print "(a)","input items:",zero_items
      s = selector('Choose from the empty list',zero_items)
      print "(a)","returned:",s
      if (s == "") print "(a)","(an empty string)"

      !! Fairy tale demo
      allocate(fairytale(4))
      fairytale = (/'fee fie       ','huff and puff ', &
        'mirror mirror ','tick tock     '/)
      print "(a)","input items:",fairytale
      s = selector('Choose a fairy tale',fairytale)
      print "(a)","returned: ",s
      if (s == "") print "(a)","(an empty string)"

      end program menu_demo


```


=={{header|F Sharp|F#}}==

```fsharp
open System

let rec menuChoice (options : string list) prompt =
    if options = [] then ""
    else
        for i = 0 to options.Length - 1 do
            printfn "%d. %s" (i + 1) options.[i]

        printf "%s" prompt
        let input = Int32.TryParse(Console.ReadLine())

        match input with
        | true, x when 1 <= x && x <= options.Length -> options.[x - 1]
        | _, _ -> menuChoice options prompt

[<EntryPoint>]
let main _ =
    let menuOptions = ["fee fie"; "huff and puff"; "mirror mirror"; "tick tock"]
    let choice = menuChoice menuOptions "Choose one: "
    printfn "You chose: %s" choice

    0
```



## Gambas

```gambas
Public Sub Form_Open()
Dim sMenu As String[] = ["fee fie", "huff and puff", "mirror mirror", "tick tock"]
Dim sAnswer As String

Do
  sAnswer = InputBox("0: fee fie 1: huff and puff 2: mirror mirror 3: tick tock", "Please select an number")
  If InStr("0123", sAnswer) Then Message("You selected item " & sAnswer & " - " & sMenu[Val(sAnswer)], "OK")
Loop

End
```



## Go


```go
package main

import "fmt"

func menu(choices []string, prompt string) string {
    if len(choices) == 0 {
        return ""
    }
    var c int
    for {
        fmt.Println("")
        for i, s := range choices {
            fmt.Printf("%d.  %s\n", i+1, s)
        }
        fmt.Print(prompt)
        _, err := fmt.Scanln(&c)

        if err == nil && c > 0 && c <= len(choices) {
            break
        }
    }
    return choices[c-1]
}

func main() {
    pick := menu(nil, "No prompt")
    fmt.Printf("No choices, result = %q\n", pick)

    choices := []string{
        "fee fie",
        "huff and puff",
        "mirror mirror",
        "tick tock",
    }
    pick = menu(choices, "Enter number: ")
    fmt.Printf("You picked %q\n", pick)
}
```

Output:

```txt

No choices, result = ""

1.  fee fie
2.  huff and puff
3.  mirror mirror
4.  tick tock
Enter number: 2
You picked "huff and puff"

```



## Haskell


```Haskell
module RosettaSelect where

import Data.Maybe (listToMaybe)
import Control.Monad (guard)

select :: [String] -> IO String
select []   = return ""
select menu = do
  putStr $ showMenu menu
  putStr "Choose an item: "
  choice <- getLine
  maybe (select menu) return $ choose menu choice

showMenu :: [String] -> String
showMenu menu = unlines [show n ++ ") " ++ item | (n, item) <- zip [1..] menu]

choose :: [String] -> String -> Maybe String
choose menu choice = do
  n <- maybeRead choice
  guard $ n > 0
  listToMaybe $ drop (n-1) menu

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads
```


Example usage, at the GHCI prompt:

```Haskell
*RosettaSelect> select ["fee fie", "huff and puff", "mirror mirror", "tick tock"]
1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Choose an item: 3
"mirror mirror"
*RosettaSelect>
```



## HicEst

```HicEst
CHARACTER list = "fee fie,huff and puff,mirror mirror,tick tock,", answer*20

   POP(Menu=list, SelTxt=answer)

SUBROUTINE list ! callback procedure must have same name as menu argument
 ! Subroutine with no arguments: all objects are global
 ! The global variable $$ returns the selected list index
   WRITE(Messagebox, Name) answer, $$
END
```


=={{header|Icon}} and {{header|Unicon}}==
```Icon
procedure main()

L := ["fee fie", "huff and puff", "mirror mirror", "tick tock"]

every i := 1 to *L do
   write(i,") ",L[i])
repeat {
   writes("Choose a number from the menu above: ")
   a := read()
   if 1 <= integer(a) <= i then break
   }
write("You selected ",a," ==> ",L[a])
end
```



## J


'''Solution:'''

```j

CHOICES =: ];._2 'fee fie;huff and puff;mirror mirror;tick tock;'
PROMPT =: 'Which is from the three pigs? '

showMenu =: smoutput@:(,"1~ (' ' ,.~ 3 ": i.@:(1 ,~ #)))
read_stdin =: 1!:1@:1:

menu =: '? '&$: :(4 : 0)
 NB. use:  [prompt] menu choice_array
 CHOICES =. y
 if. 0 = # CHOICES do. return. end.
 PROMPT =. x
 whilst. RESULT -.@:e. i. # CHOICES do.
  showMenu CHOICES
  smoutput PROMPT
  RESULT =. _1 ". read_stdin''
 end.
 RESULT {:: CHOICES
)

```


See [[Talk:Select#J_implementation|Talk page]] for explanation.


## Java


```java5
public static String select(List<String> list, String prompt){
    if(list.size() == 0) return "";
    Scanner sc = new Scanner(System.in);
    String ret = null;
    do{
        for(int i=0;i<list.size();i++){
            System.out.println(i + ": "+list.get(i));
        }
        System.out.print(prompt);
        int index = sc.nextInt();
        if(index >= 0 && index < list.size()){
            ret = list.get(index);
        }
    }while(ret == null);
    return ret;
}
```



## JavaScript

```javascript
const readline = require('readline');

async function menuSelect(question, choices) {
  if (choices.length === 0) return '';

  const prompt = choices.reduce((promptPart, choice, i) => {
    return promptPart += `${i + 1}. ${choice}\n`;
  }, '');

  let inputChoice = -1;
  while (inputChoice < 1 || inputChoice > choices.length) {
    inputChoice = await getSelection(`\n${prompt}${question}: `);
  }

  return choices[inputChoice - 1];
}

function getSelection(prompt) {
  return new Promise((resolve) => {
    const lr = readline.createInterface({
      input: process.stdin,
      output: process.stdout
    });

    lr.question(prompt, (response) => {
      lr.close();
      resolve(parseInt(response) || -1);
    });
  });
}

const choices = ['fee fie', 'huff and puff', 'mirror mirror', 'tick tock'];
const question = 'Which is from the three pigs?';
menuSelect(question, choices).then((answer) => {
  console.log(`\nYou chose ${answer}`);
});
```



## jq

This version uses jq 1.5's 'input' builtin to read programmatically from STDIN.

```jq
def choice:
  def read(prompt; max):
    def __read__:
      prompt,
      ( input as $input
        | if ($input|type) == "number" and 0 < $input and $input <= max then $input
          else __read__
          end);
    __read__;

  if length == 0 then ""
  else
  . as $in
  | ("Enter your choice:\n" +
     (reduce range(0; length) as $i (""; . + "\($i + 1): \($in[$i])\n")) ) as $prompt
  | read($prompt; length) as $read
  | if ($read|type) == "string" then $read
    else "Thank you for selecting \($in[$read-1])" end
  end ;
```

'''Example:'''

```jq
["fee fie", "huff and puff", "mirror mirror", "tick tock"] | choice
```


```sh

$ jq -n -r -f Menu.jq
Enter your choice:
1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock

5
Enter your choice:
1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock

1
Thank you for selecting fee fie
```



## Julia

```julia
function _menu(items)
    for (ind, item) in enumerate(items)
        @printf "  %2i) %s\n" ind item
    end
end

_ok(::Any,::Any) = false
function _ok(reply::AbstractString, itemcount)
    n = tryparse(Int, reply)
    return isnull(n) || 0 ≤ get(n) ≤ itemcount
end

"Prompt to select an item from the items"
function _selector(items, prompt::AbstractString)
    isempty(items) && return ""
    reply = -1
    itemcount = length(items)
    while !_ok(reply, itemcount)
        _menu(items)
        print(prompt)
        reply = strip(readline(STDIN))
    end
    return items[parse(Int, reply)]
end

items = ["fee fie", "huff and puff", "mirror mirror", "tick tock"]
item = _selector(items, "Which is from the three pigs: ")
println("You chose: ", item)
```



## Kotlin


```scala
// version 1.1.2

fun menu(list: List<String>): String {
    if (list.isEmpty()) return ""
    val n = list.size
    while (true) {
        println("\n   M E N U\n")
        for (i in 0 until n) println("${i + 1}: ${list[i]}")
        print("\nEnter your choice 1 - $n : ")
        val index = readLine()!!.toIntOrNull()
        if (index == null || index !in 1..n) continue
        return list[index - 1]
    }
}

fun main(args: Array<String>) {
    val list = listOf(
        "fee fie",
        "huff and puff",
        "mirror mirror",
        "tick tock"
    )
    val choice = menu(list)
    println("\nYou chose : $choice")
}
```

Sample session:
```txt

   M E N U

1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock

Enter your choice 1 - 4 : 0

   M E N U

1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock

Enter your choice 1 - 4 : asdf

   M E N U

1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock

Enter your choice 1 - 4 : 2

You chose : huff and puff

```



## Logo

```logo
to select :prompt [:options]
  foreach :options [(print # ?)]
  forever [
    type :prompt type "| |
    make "n readword
    if (and [number? :n] [:n >= 1] [:n <= count :options]) [output item :n :options]
    print sentence [Must enter a number between 1 and] count :options
  ]
end

print equal? [huff and puff] (select
  [Which is from the three pigs?]
  [fee fie] [huff and puff] [mirror mirror] [tick tock])

```



## Lua


```lua
function select (list)
   if not list or #list == 0 then
      return ""
   end
   local last, sel = #list
   repeat
      for i,option in ipairs(list) do
         io.write(i, ". ", option, "\n")
      end
      io.write("Choose an item (1-", tostring(last), "): ")
      sel = tonumber(string.match(io.read("*l"), "^%d+$"))
   until type(sel) == "number" and sel >= 1 and sel <= last
   return list[math.floor(sel)]
end

print("Nothing:", select {})
print()
print("You chose:", select {"fee fie", "huff and puff", "mirror mirror", "tick tock"})
```


```txt

Nothing:

1. fee fie
2. huff and puff
3. mirror mirror
4. tick tock
Choose an item (1-4): 0
1. fee fie
2. huff and puff
3. mirror mirror
4. tick tock
Choose an item (1-4): a
1. fee fie
2. huff and puff
3. mirror mirror
4. tick tock
Choose an item (1-4): 1.7
1. fee fie
2. huff and puff
3. mirror mirror
4. tick tock
Choose an item (1-4): 10
1. fee fie
2. huff and puff
3. mirror mirror
4. tick tock
Choose an item (1-4): 3
You chose:  mirror mirror

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
'''Interpreter:''' Wolfram Desktop and Wolfram Desktop Kernel
Redisplays the list of choices on every invalid input as per the task description. In the notebook interface (of Wolfram Desktop, at least), Print[] would most pragmatically be located outside of the loop because Input[] uses a dialog box.


```Mathematica
textMenu[data_List] := Module[{choice},
  If[Length@data == 0, Return@""];
  While[!(IntegerQ@choice && Length@data >= choice > 0),
   MapIndexed[Print[#2[[1]], ") ", #1]&, data];
   choice = Input["Enter selection..."]
   ];
  data[[choice]]
  ]
```

```txt
Wolfram Desktop Kernel (using Wolfram Language 12.0.0) for Microsoft Windows (64-bit)
Copyright 1988-2019 Wolfram Research, Inc.

In[1]:= (*! ELIDED !*)

In[2]:= textMenu[{}]

Out[2]=

In[3]:= textMenu[{"fee fie", "huff and puff", "mirror mirror", "tick tock"}]
1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Enter selection...0
1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Enter selection...5
1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Enter selection...-1
1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Enter selection...fee fie
1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Enter selection...3

Out[3]= mirror mirror
```



## MATLAB


```MATLAB
function sucess = menu(list)

    if numel(list) == 0
        sucess = '';
        return
    end

    while(true)

        disp('Please select one of these options:');

        for i = (1:numel(list))

            disp([num2str(i) ') ' list{i}]);

        end

        disp([num2str(numel(list)+1) ') exit']);

        try
            key = input(':: ');
            if key == numel(list)+1
                break
            elseif (key > numel(list)) || (key < 0)
                continue
            else
                disp(['-> ' list{key}]);
            end
        catch
            continue
        end


    end

    sucess = true;

end

```



## min

min has an operator <code>choose</code> that nearly conforms to this task. The input list is altered so that the choice can be returned, and the empty list case is handled.

```min
(
  :prompt =list
  (list bool)
  (list (' dup append) map prompt choose)
  ("") if
) :menu

("fee fie" "huff and puff" "mirror mirror" "tick tock")
"Enter an option" menu
"You chose: " print! puts!
```

```txt

Enter an option
1 - fee fie
2 - huff and puff
3 - mirror mirror
4 - tick tock
Enter your choice (1 - 4): 5
Invalid choice.
1 - fee fie
2 - huff and puff
3 - mirror mirror
4 - tick tock
Enter your choice (1 - 4): 1
You chose: fee fie

```


=={{header|Modula-2}}==
```modula2
MODULE  Menu;

FROM    InOut IMPORT WriteString, WriteCard, WriteLn, ReadCard;

CONST   StringLength     = 100;
        MenuSize         = 4;

TYPE    String           = ARRAY[0..StringLength-1] OF CHAR;

VAR     menu             : ARRAY[0..MenuSize] OF String;
        selection, index : CARDINAL;

BEGIN
    menu[1] := "fee fie";
    menu[2] := "huff and puff";
    menu[3] := "mirror mirror";
    menu[4] := "tick tock";

    FOR index := 1 TO HIGH(menu) DO
        WriteString("[");
        WriteCard(    index,1);
        WriteString(        "] ");
        WriteString(            menu[index]);
        WriteLn;
    END;(*of FOR*)

    WriteString("Choose what you want : ");
    ReadCard(selection);

    IF (selection <= HIGH(menu)) AND (selection > 0) THEN
        WriteString("You have chosen: ");
        WriteString(                  menu[selection]);
        WriteLn;
    ELSE
        WriteString("Selection is out of range!");
        WriteLn;
    END (*of IF*)
END Menu.
```



## MUMPS


```MUMPS
MENU(STRINGS,SEP)
 ;http://rosettacode.org/wiki/Menu
 NEW I,A,MAX
 ;I is a loop variable
 ;A is the string read in from the user
 ;MAX is the number of substrings in the STRINGS list
 ;SET STRINGS="fee fie^huff and puff^mirror mirror^tick tock"
 SET MAX=$LENGTH(STRINGS,SEP)
 QUIT:MAX=0 ""
WRITEMENU
 FOR I=1:1:MAX WRITE I,": ",$PIECE(STRINGS,SEP,I),!
 READ:30 !,"Choose a string by its index: ",A,!
 IF (A<1)!(A>MAX)!(A\1'=A) GOTO WRITEMENU
 KILL I,MAX
 QUIT $PIECE(STRINGS,SEP,A)
```

Usage:
```txt

USER>W !,$$MENU^ROSETTA("fee fie^huff and puff^mirror mirror^tick tock","^")

1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock

Choose a string by its index: 3
mirror mirror
USER>W !,$$MENU^ROSETTA("fee fie^huff and puff^mirror mirror^tick tock","^")

1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock

Choose a string by its index: 5
1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock

Choose a string by its index: A
1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock

Choose a string by its index: 0
1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock

Choose a string by its index: 1
fee fie

```



## Nim

```nim
import strutils, rdstdin

proc menu(xs) =
  for i,x in xs: echo "  ",i,") ",x

proc ok(reply, count): bool =
  try:
    let n = parseInt(reply)
    return 0 <= n and n < count
  except: return false

proc selector(xs, prompt): string =
  if xs.len == 0: return ""
  var reply = "-1"
  while not ok(reply, xs.len):
    menu(xs)
    reply = readLineFromStdin(prompt).strip()
  return xs[parseInt(reply)]

const xs = ["fee fie", "huff and puff", "mirror mirror", "tick tock"]
let item = selector(xs, "Which is from the three pigs: ")
echo "You chose: ", item
```

Output:

```txt
  0) fee fie
  1) huff and puff
  2) mirror mirror
  3) tick tock
Which is from the three pigs: foo
  0) fee fie
  1) huff and puff
  2) mirror mirror
  3) tick tock
Which is from the three pigs: 4
  0) fee fie
  1) huff and puff
  2) mirror mirror
  3) tick tock
Which is from the three pigs: 2
You chose: mirror mirror
```



## OCaml


```ocaml

let select ?(prompt="Choice? ") = function
  | [] -> ""
  | choices ->
      let rec menu () =
        List.iteri (Printf.printf "%d: %s\n") choices;
        print_string prompt;
        try List.nth choices (read_int ())
        with _ -> menu ()
      in menu ()

```


Example use in the REPL:

```ocaml

# select ["fee fie"; "huff and puff"; "mirror mirror"; "tick tock"];;
0: fee fie
1: huff and puff
2: mirror mirror
3: tick tock
Choice? 2
- : string = "mirror mirror"

```



## OpenEdge/Progress


```progress
FUNCTION bashMenu RETURNS CHAR(
   i_c AS CHAR
):

   DEF VAR ii        AS INT.
   DEF VAR hfr       AS HANDLE.
   DEF VAR hmenu     AS HANDLE EXTENT.
   DEF VAR ikey      AS INT.
   DEF VAR ireturn   AS INT INITIAL ?.

   EXTENT( hmenu ) = NUM-ENTRIES( i_c ).

   CREATE FRAME hfr ASSIGN
      WIDTH    =  80
      HEIGHT   =  NUM-ENTRIES( i_c )
      PARENT   =  CURRENT-WINDOW
      VISIBLE  =  TRUE
      .

   DO ii = 1 TO NUM-ENTRIES( i_c ):

      CREATE TEXT hmenu ASSIGN
         FRAME          =  hfr
         FORMAT         =  "x(79)"
         SCREEN-VALUE   =  SUBSTITUTE( "&1. &2", ii, ENTRY( ii, i_c ) )
         ROW            =  ii
         VISIBLE        =  TRUE
         .

   END.

   IF i_c = "" THEN
      ireturn = 1.

   DO WHILE ireturn = ?:

      READKEY.
      ikey = INTEGER( CHR( LASTKEY ) ) NO-ERROR.
      IF ikey >= 1 AND ikey <= NUM-ENTRIES( i_c ) THEN
         ireturn = ikey.

   END.

   RETURN ENTRY( ireturn, i_c ).

END FUNCTION.

MESSAGE
   bashMenu( "fee fie,huff and puff,mirror mirror,tick tock" )
VIEW-AS ALERT-BOX.
```



## Oz


```oz
declare
  fun {Select Prompt Items}
     case Items of nil then ""
     else
	for
	   Item in Items
	   Index in 1..{Length Items}
	do
	   {System.showInfo Index#") "#Item}
	end
	{System.printInfo Prompt}
	try
	   {Nth Items {ReadInt}}
	catch _ then
	   {Select Prompt Items}
	end
     end
  end

  fun {ReadInt}
     class TextFile from Open.file Open.text end
     StdIo = {New TextFile init(name:stdin)}
  in
     {String.toInt {StdIo getS($)}}
  end

  Item = {Select "Which is from the three pigs: "
	  ["fee fie" "huff and puff" "mirror mirror" "tick tock"]}

in
  {System.showInfo "You chose: "#Item}
```



## PARI/GP

```parigp
choose(v)=my(n);for(i=1,#v,print(i". "v[i]));while(type(n=input())!="t_INT"|n>#v|n<1,);v[n]
choose(["fee fie","huff and puff","mirror mirror","tick tock"])
```



## Pascal

Tested with Free Pascal 2.6.4 (arm).

```pascal
program Menu;
{$ASSERTIONS ON}
uses
  objects;
var
  MenuItems :PUnSortedStrCollection;
  selected  :string;

Function SelectMenuItem(MenuItems :PUnSortedStrCollection):string;
var
  i, idx :integer;
  code   :word;
  choice :string;
begin
  // Return empty string if the collection is empty.
  if MenuItems^.Count = 0 then
  begin
    SelectMenuItem := '';
    Exit;
  end;

  repeat
    for i:=0 to MenuItems^.Count-1 do
    begin
      writeln(i+1:2, ') ', PString(MenuItems^.At(i))^);
    end;
    write('Make your choice: ');
    readln(choice);
    // Try to convert choice to an integer.
    // Code contains 0 if this was successful.
    val(choice, idx, code)
  until (code=0) and (idx>0) and (idx<=MenuItems^.Count);
  // Return the selected element.
  SelectMenuItem := PString(MenuItems^.At(idx-1))^;
end;

begin
  // Create an unsorted string collection for the menu items.
  MenuItems := new(PUnSortedStrCollection, Init(10, 10));

  // Add some menu items to the collection.
  MenuItems^.Insert(NewStr('fee fie'));
  MenuItems^.Insert(NewStr('huff and puff'));
  MenuItems^.Insert(NewStr('mirror mirror'));
  MenuItems^.Insert(NewStr('tick tock'));

  // Display the menu and get user input.
  selected := SelectMenuItem(MenuItems);
  writeln('You chose: ', selected);

  dispose(MenuItems, Done);

  // Test function with an empty collection.
  MenuItems := new(PUnSortedStrCollection, Init(10, 10));

  selected := SelectMenuItem(MenuItems);
  // Assert that the function returns an empty string.
  assert(selected = '', 'Assertion failed: the function did not return an empty string.');

  dispose(MenuItems, Done);
end.
```


```txt

$ bin/menu
 1) fee fie
 2) huff and puff
 3) mirror mirror
 4) tick tock
Make your choice: abc
 1) fee fie
 2) huff and puff
 3) mirror mirror
 4) tick tock
Make your choice: 99
 1) fee fie
 2) huff and puff
 3) mirror mirror
 4) tick tock
Make your choice: 3
You chose: mirror mirror

```



## Perl


```perl
sub menu
{
        my ($prompt,@array) = @_;
        return '' unless @array;

        print "  $_: $array[$_]\n" for(0..$#array);
        print $prompt;
        $n = <>;
        return $array[$n] if $n =~ /^\d+$/ and defined $array[$n];
        return &menu($prompt,@array);
}

@a = ('fee fie', 'huff and puff', 'mirror mirror', 'tick tock');
$prompt = 'Which is from the three pigs: ';

$a = &menu($prompt,@a);

print "You chose: $a\n";
```



## Perl 6


```perl6
sub menu ( $prompt, @items ) {
    return '' unless @items.elems;
    repeat until my $selection ~~ /^ \d+ $/ && @items[--$selection] {
        my $i = 1;
        say "  {$i++}) $_" for @items;
        $selection = prompt $prompt;
    }
    return @items[$selection];
}

my @choices = 'fee fie', 'huff and puff', 'mirror mirror', 'tick tock';
my $prompt = 'Enter the number corresponding to your selection: ';

my $answer = menu( $prompt, [] );
say "You chose: $answer" if $answer.chars;

$answer = menu( $prompt, @choices );
say "You chose: $answer" if $answer.chars;
```



## PL/I

```PL/I


test: proc options (main);

declare menu(4) character(100) varying static initial (
   'fee fie', 'huff and puff', 'mirror mirror', 'tick tock');
declare (i, k) fixed binary;

do i = lbound(menu,1) to hbound(menu,1);
   put skip edit (trim(i), ': ', menu(i) ) (a);
end;
put skip list ('please choose an item number');
get list (k);
if k >= lbound(menu,1) & k <= hbound(menu,1) then
   put skip edit ('you chose ', menu(k)) (a);
else
   put skip list ('Could not find your phrase');

end test;

```



## Phix


```Phix
function menu_select(sequence items, object prompt)
sequence res = ""
    items = remove_all("",items)
    if length(items)!=0 then
        while 1 do
            for i=1 to length(items) do
                printf(1,"%d) %s\n",{i,items[i]})
            end for
            puts(1,iff(atom(prompt)?"Choice?":prompt))
            res = scanf(trim(gets(0)),"%d")
            puts(1,"\n")
            if length(res)=1 then
                integer nres = res[1][1]
                if nres>0 and nres<=length(items) then
                    res = items[nres]
                    exit
                end if
            end if
        end while
    end if
    return res
end function

constant items = {"fee fie", "huff and puff", "mirror mirror", "tick tock"}
constant prompt = "Which is from the three pigs? "
string res = menu_select(items,prompt)
printf(1,"You chose %s.\n",{res})
```



## PHP


```php
<?php
$stdin = fopen("php://stdin", "r");
$allowed = array(1 => 'fee fie', 'huff and puff', 'mirror mirror', 'tick tock');

for(;;) {
    foreach ($allowed as $id => $name) {
        echo "  $id: $name\n";
    }
    echo "Which is from the four pigs: ";
    $stdin_string = fgets($stdin, 4096);
    if (isset($allowed[(int) $stdin_string])) {
        echo "You chose: {$allowed[(int) $stdin_string]}\n";
        break;
    }
}
```



## PicoLisp


```PicoLisp
(de choose (Prompt Items)
   (use N
      (loop
         (for (I . Item) Items
            (prinl I ": " Item) )
         (prin Prompt " ")
         (flush)
         (NIL (setq N (in NIL (read))))
         (T (>= (length Items) N 1) (prinl (get Items N))) ) ) )
(choose "Which is from the three pigs?"
   '("fee fie" "huff and puff" "mirror mirror" "tick tock") )
```

```txt

1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock
Which is from the three pigs? q
1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock
Which is from the three pigs? 5
1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock
Which is from the three pigs? 2
huff and puff

```



## PowerShell

```PowerShell

function Select-TextItem
{
  <#
    .SYNOPSIS
        Prints a textual menu formatted as an index value followed by its corresponding string for each object in the list.
    .DESCRIPTION
        Prints a textual menu formatted as an index value followed by its corresponding string for each object in the list;
        Prompts the user to enter a number;
        Returns an object corresponding to the selected index number.
    .PARAMETER InputObject
        An array of objects.
    .PARAMETER Prompt
        The menu prompt string.
    .EXAMPLE
        “fee fie”, “huff and puff”, “mirror mirror”, “tick tock” | Select-TextItem
    .EXAMPLE
        “huff and puff”, “fee fie”, “tick tock”, “mirror mirror” | Sort-Object | Select-TextItem -Prompt "Select a string"
    .EXAMPLE
        Select-TextItem -InputObject (Get-Process)
    .EXAMPLE
        (Get-Process | Where-Object {$_.Name -match "notepad"}) | Select-TextItem -Prompt "Select a Process" | Stop-Process -ErrorAction SilentlyContinue
  #>
    [CmdletBinding()]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true)]
        $InputObject,

        [Parameter(Mandatory=$false)]
        [string]
        $Prompt = "Enter Selection"
    )

    Begin
    {
        $menuOptions = @()
    }
    Process
    {
        $menuOptions += $InputObject
    }
    End
    {
        do
        {
            [int]$optionNumber = 1

            foreach ($option in $menuOptions)
            {
                Write-Host ("{0,3}: {1}" -f $optionNumber,$option)

                $optionNumber++
            }

            Write-Host ("{0,3}: {1}" -f 0,"To cancel")

            [int]$choice = Read-Host $Prompt

            $selectedValue = ""

            if ($choice -gt 0 -and $choice -le $menuOptions.Count)
            {
                $selectedValue = $menuOptions[$choice - 1]
            }
        }
        until ($choice -eq 0 -or $choice -le $menuOptions.Count)

        return $selectedValue
    }
}

“fee fie”, “huff and puff”, “mirror mirror”, “tick tock” | Select-TextItem -Prompt "Select a string"

```


```txt

  1: fee fie
  2: huff and puff
  3: mirror mirror
  4: tick tock
  0: To cancel
Select a string: 3
mirror mirror

```



## ProDOS

<lang>
:a
printline
### =======MENU=======

printline 1. Fee Fie
printline 2. Huff Puff
printline 3. Mirror, Mirror
printline 4. Tick, Tock
editvar /newvar /value=a /userinput=1 /title=What page do you want to go to?
if -a- /hasvalue 1 printline You chose a line from the book Jack and the Beanstalk. & exitcurrentprogram 1
if -a- /hasvalue 2 printline You chose a line from the book The Three Little Pigs. & exitcurrentprogram 1
if -a- /hasvalue 3 printline You chose a line from the book Snow White. & exitcurrentprogram 1
if -a- /hasvalue 4 printline You chose a line from the book Beauty and the Beast. & exitcurrentprogram 1
printline You either chose an invalid choice or didn't chose.
editvar /newvar /value=goback /userinput=1 /title=Do you want to chose something else?
if -goback- /hasvalue y goto :a else exitcurrentprogram 1
```



## Prolog

```prolog

rosetta_menu([], "") :- !.              %% Incase of an empty list.
rosetta_menu(Items, SelectedItem) :-
    repeat,                             %% Repeat until everything that follows is true.
        display_menu(Items),            %% IO
        get_choice(Choice),             %% IO
    number(Choice),                     %% True if Choice is a number.
    nth1(Choice, Items, SelectedItem),  %% True if SelectedItem is the 1-based nth member of Items, (fails if Choice is out of range)
    !.

display_menu(Items) :-
    nl,
    foreach( nth1(Index, Items, Item),
             format('~w) ~s~n', [Index, Item]) ).

get_choice(Choice) :-
    prompt1('Select a menu item by number:'),
    read(Choice).

```


Example run:


```prolog

?- rosetta_menu(["fee fie", "huff and puff", "mirror mirror", "tick tock"], String).

1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Select a menu item by number:a.

1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Select a menu item by number:10.

1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Select a menu item by number:3.
String = "mirror mirror".

```



## PureBasic


```PureBasic
If OpenConsole()
  Define i, txt$, choice
  Dim txts.s(4)
  EnableGraphicalConsole(1)  ;- Enable graphical mode in the console
  Repeat
    ClearConsole()
    Restore TheStrings       ; Set reads address
    For i=1 To 4
      Read.s  txt$
      txts(i)=txt$
      ConsoleLocate(3,i): Print(Str(i)+": "+txt$)
    Next
    ConsoleLocate(3,6): Print("Your choice? ")
    choice=Val(Input())
  Until choice>=1 And choice<=4
  ClearConsole()
  ConsoleLocate(3,2): Print("You chose: "+txts(choice))
  ;
  ;-Now, wait for the user before ending to allow a nice presentation
  ConsoleLocate(3,5): Print("Press ENTER to quit"): Input()
EndIf
End

DataSection
  TheStrings:
  Data.s  "fee fie", "huff And puff", "mirror mirror", "tick tock"
EndDataSection
```



## Python



```python
def _menu(items):
    for indexitem in enumerate(items):
        print ("  %2i) %s" % indexitem)

def _ok(reply, itemcount):
    try:
        n = int(reply)
        return 0 <= n < itemcount
    except:
        return False

def selector(items, prompt):
    'Prompt to select an item from the items'
    if not items: return ''
    reply = -1
    itemcount = len(items)
    while not _ok(reply, itemcount):
        _menu(items)
        # Use input instead of raw_input for Python 3.x
        reply = raw_input(prompt).strip()
    return items[int(reply)]

if __name__ == '__main__':
    items = ['fee fie', 'huff and puff', 'mirror mirror', 'tick tock']
    item = selector(items, 'Which is from the three pigs: ')
    print ("You chose: " + item)
```


Sample runs:

```txt
   0) fee fie
   1) huff and puff
   2) mirror mirror
   3) tick tock
Which is from the three pigs:  -1
   0) fee fie
   1) huff and puff
   2) mirror mirror
   3) tick tock
Which is from the three pigs:      0
You chose: fee fie
>>>
### ============================= RESTART =============================

>>>
   0) fee fie
   1) huff and puff
   2) mirror mirror
   3) tick tock
Which is from the three pigs: 4
   0) fee fie
   1) huff and puff
   2) mirror mirror
   3) tick tock
Which is from the three pigs: 3
You chose: tick tock
```



## R


Uses [http://www.stat.ucl.ac.be/ISdidactique/Rhelp/library/base/html/menu.html menu].

```R
showmenu <- function(choices = NULL)
{
   if (is.null(choices)) return("")
   ans <- menu(choices)
   if(ans==0) "" else choices[ans]

}
str <- showmenu(c("fee fie", "huff and puff", "mirror mirror", "tick tock"))
str <- showmenu()

```



## Racket



```Racket

#lang racket

(define (menu choices)
  (cond [(null? choices) ""]
        [else (for ([c choices] [i (in-naturals 1)]) (printf "~a. ~a\n" i c))
              (printf "Enter a number: ")
              (define n (string->number (read-line)))
              (or (and (exact-integer? n)
                       (<= 1 n (length choices))
                       (list-ref choices (sub1 n)))
                  (menu choices))]))

(menu '("fee fie" "huff and puff" "mirror mirror" "tick tock"))

```


Sample Run:

```txt

1. fee fie
2. huff and puff
3. mirror mirror
4. tick tock
Enter a number: three
1. fee fie
2. huff and puff
3. mirror mirror
4. tick tock
Enter a number: help
1. fee fie
2. huff and puff
3. mirror mirror
4. tick tock
Enter a number: 3!!
1. fee fie
2. huff and puff
3. mirror mirror
4. tick tock
Enter a number: 3
"mirror mirror"

```



## REBOL

```REBOL
REBOL [
	Title: "Text Menu"
	URL: http://rosettacode.org/wiki/Select
]

choices: ["fee fie" "huff and puff" "mirror mirror" "tick tock"]
choice: ""

valid?: func [
	choices [block! list! series!]
	choice
][
	if error? try [choice: to-integer choice] [return false]
	all [0 < choice  choice <= length? choices]
]

while [not valid? choices choice][
	repeat i length? choices [print ["  " i ":" choices/:i]]
	choice: ask "Which is from the three pigs? "
]
print ["You chose:" pick choices to-integer choice]
```


Output:


```txt
   1 : fee fie
   2 : huff and puff
   3 : mirror mirror
   4 : tick tock
Which is from the three pigs? klf
   1 : fee fie
   2 : huff and puff
   3 : mirror mirror
   4 : tick tock
Which is from the three pigs? 5
   1 : fee fie
   2 : huff and puff
   3 : mirror mirror
   4 : tick tock
Which is from the three pigs? 2
You chose: huff and puff
```



## REXX


```rexx
/*REXX program displays a list,  then prompts the user for a selection number (integer).*/
        do forever                               /*keep prompting until response is OK. */
        call list_create                         /*create the list from scratch.        */
        call list_show                           /*display (show)  the list to the user.*/
        if #==0   then return ''                 /*if list is empty,  then return  null.*/
        say right(' choose an item by entering a number from 1 ───►' #, 70, '═')
        parse pull x                             /*get the user's choice  (if any).     */

              select
              when x=''              then call sayErr  "a choice wasn't entered"
              when words(x)\==1      then call sayErr  'too many choices entered:'
              when \datatype(x,'N')  then call sayErr  "the choice isn't numeric:"
              when \datatype(x,'W')  then call sayErr  "the choice isn't an integer:"
              when x<1 | x>#         then call sayErr  "the choice isn't within range:"
              otherwise              leave       /*this leaves the    DO FOREVER   loop.*/
              end   /*select*/
        end         /*forever*/
                                                 /*user might've entered   2.  or  003  */
x=x/1                                            /*normalize the number (maybe).        */
say;  say 'you chose item' x": " #.x
return #.x                                       /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
list_create:   #.1= 'fee fie'                    /*this is one method for list-building.*/
               #.2= 'huff and puff'
               #.3= 'mirror mirror'
               #.4= 'tick tock'
               #=4                               /*store the number of choices in   #   */
               return                            /*(above)  is just one convention.     */
/*──────────────────────────────────────────────────────────────────────────────────────*/
list_show:     say                               /*display a blank line.                */
                      do j=1  for #              /*display the list of choices.         */
                      say '[item'   j"]   " #.j  /*display item number with its choice. */
                      end   /*j*/
               say                               /*display another blank line.          */
               return
/*──────────────────────────────────────────────────────────────────────────────────────*/
sayErr:        say;         say  '***error***'  arg(1)  x;          say;            return
```

'''output'''   (which includes what the user entered:

```txt

[item 1]  fee fie
[item 2]  huff and puff
[item 3]  mirror mirror
[item 4]  tick tock

════════════════════ choose an item by entering a number from 1 ───► 4
2           ◄■■■■■■■■■■■■■■■■■■■■■■ what the user entered at the terminal.

you chose item 2:  huff and puff

```



## Ring


```ring

aList = ["fee fie", "huff and puff", "mirror mirror", "tick tock"]
selected = menu(aList, "please make a selection: ")
see "" + selected + nl

func menu aList, prompt
     ndex = 1
     while index>0 and index<=len(aList)
           for index = 1 to len(aList)
               if aList[index]!="" see "" + index + " : " + aList[index] + " " ok
           next
           see nl
           see prompt
           give select
           index = number(select)
           see "" + aList[index] + nl
           if select!=string(index) index = -1 ok
           if index>=0 if index<=len(aList) if aList[index]="" index = -1 ok ok ok
     end
     return aList[index]

```

Output:

```txt

1 : fee fie 2 : huff and puff 3 : mirror mirror 4 : tick tock
please make a selection: 2
huff and puff
1 : fee fie 2 : huff and puff 3 : mirror mirror 4 : tick tock
please make a selection: 1
fee fie
1 : fee fie 2 : huff and puff 3 : mirror mirror 4 : tick tock
please make a selection: 4
tick tock
1 : fee fie 2 : huff and puff 3 : mirror mirror 4 : tick tock
please make a selection: 3
mirror mirror

```



## Ruby


```ruby

def select(prompt, items = [])
  if items.empty?
    ''
  else
    answer = -1
    until (0...items.length).cover?(answer)
      items.each_with_index {|i,j| puts "#{j}. #{i}"}
      print "#{prompt}: "
      begin
        answer = Integer(gets)
      rescue ArgumentError
        redo
      end
    end
    items[answer]
  end
end

# test empty list
response = select('Which is empty')
puts "empty list returns: >#{response}<\n"

# "real" test
items = ['fee fie', 'huff and puff', 'mirror mirror', 'tick tock']
response = select('Which is from the three pigs', items)
puts "you chose: >#{response}<"

```



## Run BASIC


```runbasic
dim choose$(5)
choose$(1) = "1 Fee Fie"
choose$(2) = "2 Huff Puff"
choose$(3) = "3 Mirror, Mirror"
choose$(4) = "4 Tick, Tock"
choose$(5) = "Exit"

[start]
print "Menu Selection"
listbox #lb,choose$(),5
button #sel, "Accept",[select]
wait

[select]
selected$=#lb selection$()
print " "
if selected$<>"" then
  print "You selected ";selected$
 else
  print "No selection made"
end if
 button #con, "Continue",[go2]
wait
[go2]
if selected$<>"Exit" then
  cls
  goto [start]
 else
  cls
  end
end if
```



## Rust


```rust

fn menu_select<'a>(items: &'a [&'a str]) -> &'a str {
    if items.len() == 0 {
        return "";
    }

    let stdin = std::io::stdin();
    let mut buffer = String::new();

    loop {
        for (i, item) in items.iter().enumerate() {
            println!("{}) {}", i + 1, item);
        }
        print!("Pick a number from 1 to {}: ", items.len());

        // Read the user input:
        stdin.read_line(&mut buffer).unwrap();
        println!();

        if let Ok(selected_index) = buffer.trim().parse::<usize>() {
            if 0 < selected_index {
                if let Some(selected_item) = items.get(selected_index - 1) {
                    return selected_item;
                }
            }
        }

        // The buffer will contain the old input, so we need to clear it before we can reuse it.
        buffer.clear();
    }
}

fn main() {
    // Empty list:
    let selection = menu_select(&[]);
    println!("No choice: {:?}", selection);

    // List with items:
    let items = [
        "fee fie",
        "huff and puff",
        "mirror mirror",
        "tick tock",
    ];

    let selection = menu_select(&items);
    println!("You chose: {}", selection);
}

```



## Scala

===Scala idiom (Functional)===
<lang>import scala.util.Try

object Menu extends App {
  val choice = menu(list)

  def menu(menuList: Seq[String]): String = {
    if (menuList.isEmpty) "" else {
      val n = menuList.size

      def getChoice: Try[Int] = {
        println("\n   M E N U\n")
        menuList.zipWithIndex.map { case (text, index) => s"${index + 1}: $text" }.foreach(println(_))
        print(s"\nEnter your choice 1 - $n : ")
        Try {
          io.StdIn.readInt()
        }
      }

      menuList(Iterator.continually(getChoice)
        .dropWhile(p => p.isFailure || !(1 to n).contains(p.get))
        .next.get - 1)
    }
  }

  def list = Seq("fee fie", "huff and puff", "mirror mirror", "tick tock")

  println(s"\nYou chose : $choice")
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: menuSelect (in array string: items, in string: prompt) is func
  result
    var string: selection is "";
  local
    var string: item is "";
    var integer: index is 0;
    var integer: num is 0;
  begin
    if length(items) <> 0 then
      repeat
        for item key index range items do
          writeln(index <& ". " <& item);
        end for;
        write(prompt);
        readln(num);
      until num >= 1 and num <= length(items);
      selection := items[num];
    end if
  end func;

const array string: items is [] ("fee fie", "huff and puff", "mirror mirror", "tick tock");
const string: prompt is "Which is from the three pigs? ";

const proc: main is func
  begin
    writeln("You chose " <& menuSelect(items, prompt));
  end func;
```



## Sidef


```ruby
func menu (prompt, arr) {
    arr.len > 0 || return ''
    loop {
        for i in ^arr {
            say "  #{i}: #{arr[i]}"
        }
        var n = Sys.scanln(prompt) \\ return()
        n ~~ /^[0-9]+\z/ ? Num(n) : next
        arr.exists(n) && return arr[n]
    }
}

var list = ['fee fie', 'huff and puff', 'mirror mirror', 'tick tock']
var prompt = 'Please choose an item number: '

var answer = menu(prompt, list)
say "You choose: #{answer}"
```



## Tcl


```tcl
proc select {prompt choices} {
    set nc [llength $choices]
    if {!$nc} {
	return ""
    }
    set numWidth [string length $nc]
    while true {
	set i 0
	foreach s $choices {
	    puts [format "  %-*d: %s" $numWidth [incr i] $s]
	}
	puts -nonewline "$prompt: "
	flush stdout
	gets stdin num
	if {[string is int -strict $num] && $num >= 1 && $num <= $nc} {
	    incr num -1
	    return [lindex $choices $num]
	}
    }
}
```

Testing it out interactively...

```tcl
% puts >[select test {}]<
><
% puts >[select "Which is from the three pigs" {
    "fee fie" "huff and puff" "mirror mirror" "tick tock"
}]<
  1: fee fie
  2: huff and puff
  3: mirror mirror
  4: tick tock
Which is from the three pigs: 0
  1: fee fie
  2: huff and puff
  3: mirror mirror
  4: tick tock
Which is from the three pigs: skdfjhgz
  1: fee fie
  2: huff and puff
  3: mirror mirror
  4: tick tock
Which is from the three pigs:
  1: fee fie
  2: huff and puff
  3: mirror mirror
  4: tick tock
Which is from the three pigs: 5
  1: fee fie
  2: huff and puff
  3: mirror mirror
  4: tick tock
Which is from the three pigs: 2
>huff and puff<
```


=={{header|TI-83 BASIC}}==
TI-83 BASIC does not support lists of strings, so this works by accepting a string containing an arbitrary number of items separated by colons. If you want to use a different delimiter, change the colons on lines 2, 3, 6 and 7 to your symbol of choice.

The calculator's screen isn't big enough to display more than 7 (9 on the new C Silver Edition and CE calcs) options at a time, so the display scrolls to accommodate options if necessary. You won't be able to see options that have scrolled off the top of the screen, but they're still accessible from the input.

Although TI-BASIC ''can'' handle empty strings, there's no way to give it one through the Input function, so it doesn't have to worry about being "called with an empty list."

 Input "",Str1 //input as ITEM 1:ITEM 2:ITEM 3...
 ":"+Str1+":→Str1
 Σ(sub(Str1,X,1)=":",X,1,length(Str1→X
 0→dim(L₁
 For(Z,2,length(Str1
 inString(Str1,":",Z-1
 1+Ans+.01inString(Str1,":",Ans+1→L₁(1+dim(L₁
 ᴇ2fPart(Ans→Z
 End
 seq(iPart(L₁(X))+.01(ᴇ2fPart(L₁(X))-iPart(L₁(X))),X,1,dim(L₁→L₁
 Repeat A>0 and A<X
 ClrHome
 For(Z,1,dim(L₁
 Disp "  :"+sub(Str1,iPart(L₁(Z)),ᴇ2fPart(L₁(Z
 Output(min(7,Z),1+(Z≤7),Z
 End
 Input A
 End
 Disp sub(Str1,iPart(L₁(A)),ᴇ2fPart(L₁(A

Output with <tt>FEE FIE:HUFF AND PUFF:MIRROR MIRROR:TICK TOCK</tt>

<lang> 1:FEE FIE
 2:HUFF AND PUFF
 3:MIRROR MIRROR
 4:TICK TOCK
? [flashing cursor]
```



The language also has a Menu( command, but it doesn't really follow the requirements for the challenge (only up to 7 options allowed, no user input, option is selected using the arrow keys instead of by entering a number, etc)
 "FEE FIE→Str0
 "HUFF AND PUFF→Str1
 "MIRROR MIRROR→Str2
 "TICK TOCK→Str3
 Menu("CHOOSE",Str0,A,Str1,B,Str2,C,Str3,D)
 Lbl A
 Disp Str0
 Return
 Lbl B
 Disp Str1
 Return
 Lbl C
 Disp Str2
 Return
 Lbl D
 Disp Str3


## UNIX Shell

This example uses the [http://www.softpanorama.org/Scripting/Shellorama/Control_structures/select_statements.shtml Bash select statement], but Bash did not invent this feature. The ''select'' loop comes originally from the [[Korn Shell]], and appears in some other shells. This loop always continues to read menu choices until the script breaks the loop, or the standard input reaches end of file (EOF).

* If the user enters a blank line, the ''select'' loop repeats the list of choices. This is the only way to print the list again. An invalid choice only repeats the prompt, not the list.

Our ''choose'' function wraps a ''select'' loop. This wrapper implements the task requirement to provide an empty string from an empty list of choices. It also breaks the ''select'' loop after the first good choice.

```bash
# choose 'choice 1' 'choice 2' ...
#   Prints menu to standard error. Prompts with PS3.
#   Reads REPLY from standard input. Sets CHOICE.
choose() {
    CHOICE=                     # Default CHOICE is empty string.
    [[ $# -gt 0 ]] || return    # Return if "$@" is empty.
    select CHOICE; do           # Select from "$@".
        if [[ -n $CHOICE ]]; then
            break
        else
            echo Invalid choice.
        fi
    done
}

PS3='Which is from the three pigs: '
choose 'fee fie' 'huff and puff' 'mirror mirror' 'tick tock'
[[ -n $CHOICE ]] && echo You chose: $CHOICE
[[ -z $CHOICE ]] && echo No input.
```



```txt
$ bash menu.sh
1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Which is from the three pigs: 5
Invalid choice.
Which is from the three pigs: 2
You chose: huff and puff
$
```



```txt
$ zsh menu.sh
1) fee fie         2) huff and puff   3) mirror mirror   4) tick tock
Which is from the three pigs: 2
You chose: huff and puff
```


=
## es
=
There is no ''select'' loop, but this ''es'' script provides just enough code to mimic one.

* ''Deviation from task:'' When the list of choices is empty, this function returns an empty list, not an empty string.


```es
# choose 'choice 1' 'choice 2' ...
#   Prints menu to standard error. Prompts with $prompt.
#   Returns choice. If no input, returns empty list.
fn choose args {
	# If args are empty, return empty list.
	~ $#args 0 && return

	# Echo to standard error.
	let (reply =; choice =; fn-menu =; fn-ch =) >[1=2] {
		fn-menu = {
			# Show menu.
			let (i = 1) for (c = $args) {
				echo $i') '$c
				i = `{expr $i + 1}
			}
		}
		fn-ch = {
			# Set choice = $args($reply), but ignore error
			# if $reply is not a valid index.
			choice = <={catch @ e {result} {
				result $args($reply)
			}}
		}

		menu
		forever {
			# Show prompt, read reply.
			echo -n $prompt
			reply = <={%read}

			# If no input, return empty list.
			~ $#reply 0 && return

			# Parse reply and return choice.
			reply = <={%split \ \t\n $reply}
			if {~ $#reply 0} {
				# Empty reply: show menu again.
				menu
			} {~ $#reply 1 && ch; ~ $#choice 1} {
				return $choice
			} {
				echo Invalid choice.
			}
		}
	}
}

let (choice = <={
	local (prompt = 'Which is from the three pigs: ')
		choose 'fee fie' 'huff and puff' 'mirror mirror' 'tick tock'
}) {
	~ $#choice 1 && echo You chose: $choice
	~ $#choice 0 && echo No input.
}
```



```txt
$ es menu.es
1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Which is from the three pigs: 2
You chose: huff and puff
```



## Ursa

```ursa
def _menu (string<> items)
	for (decl int i) (< i (size items)) (inc i)
		out "  " i ") " items<i> endl console
	end for
end _menu

def _ok (string reply, int itemcount)
	try
		decl int n
		set n (int reply)
		return (and (or (> n 0) (= n 0)) (< n itemcount))
	catch
		return false
	end try
end _ok

def selector (string<> items, string prompt)
	# Prompt to select an item from the items
	if (= (size items) 0)
		return ""
	end if
	decl int itemcount reply
	set reply -1
	set itemcount (size items)
	while (not (_ok reply itemcount))
		_menu items
		out prompt console
		set reply (in int console)
	end while
	return items<(int reply)>
end selector

decl string<> items
append "fee fie" "huff and puff" "mirror mirror" "tick tock" items
decl string item
set item (selector items "Which is from the three pigs: ")
out "You chose: " item endl console
```



## VBScript

```vb

Do
	WScript.StdOut.Write "1. fee fie" & vbCrLf
	WScript.StdOut.Write "2. huff puff" & vbCrLf
	WScript.StdOut.Write "3. mirror mirror" & vbCrLf
	WScript.StdOut.Write "4. tick tock" & vbCrLf
	WScript.StdOut.Write "Please Enter Your Choice: " & vbCrLf
	choice = WScript.StdIn.ReadLine
	Select Case choice
		Case "1"
			WScript.StdOut.Write "fee fie" & vbCrLf
			Exit Do
		Case "2"
			WScript.StdOut.Write "huff puff" & vbCrLf
			Exit Do
		Case "3"
			WScript.StdOut.Write "mirror mirror" & vbCrLf
			Exit Do
		Case "4"
			WScript.StdOut.Write "tick tock" & vbCrLf
			Exit Do
		Case Else
			WScript.StdOut.Write choice & " is an invalid choice. Please try again..." &_
				vbCrLf & vbCrLf
	End Select
Loop

```


```txt


F:\>cscript /nologo menu.vbs
1. fee fie
2. huff puff
3. mirror mirror
4. tick tock
Please Enter Your Choice:
9
9 is an invalid choice. Please try again...

1. fee fie
2. huff puff
3. mirror mirror
4. tick tock
Please Enter Your Choice:
f
f is an invalid choice. Please try again...

1. fee fie
2. huff puff
3. mirror mirror
4. tick tock
Please Enter Your Choice:
3
mirror mirror

```



## XPL0

```XPL0
include c:\cxpl\codes;
string 0;

func Menu(List);
int  List;
int  Size, I, C;
[Size:= List(0);
if Size < 1 then return List(0);
for I:= 1 to Size-1 do
        [IntOut(0, I);  Text(0, ": ");
        Text(0, List(I));  CrLf(0);
        ];
CrLf(0);
Text(0, List(Size));            \display prompt
loop    [C:= ChIn(0);           \buffered keyboard requires Enter key
        if C>=^1 & C<=Size-1+^0 then return List(C-^0);
        Text(0, "Please enter 1 thru ");  IntOut(0, Size-1);
        Text(0, ": ");
        ];
];

Text(0, Menu([5, "fee fie", "huff and puff", "mirror mirror", "tick tock",
                "Which phrase is from the Three Little Pigs? "]))
```


Example output:

```txt

1: fee fie
2: huff and puff
3: mirror mirror
4: tick tock

Which phrase is from the Three Little Pigs? 5
Please enter 1 thru 4: 2
huff and puff

```



## zkl


```zkl
fcn teleprompter(options){
   os:=T("exit").extend(vm.arglist); N:=os.len();
   if(N==1) return("");
   while(1){
      Utils.zipWith(fcn(n,o){"%d) %s".fmt(n,o).println()},[0..],os);
      a:=ask("Your choice: ");
      try{ n:=a.toInt(); if(0<=n<N) return(os[n]); } catch{}
      println("Ack, not good");
   }
}

teleprompter("fee fie" , "huff and puff" , "mirror mirror" , "tick tock")
.println();
```

```txt

0) exit
1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Your choice: abc
Ack, not good
0) exit
1) fee fie
2) huff and puff
3) mirror mirror
4) tick tock
Your choice: 3
mirror mirror

```


