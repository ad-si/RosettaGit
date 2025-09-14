+++
title = "Decision tables"
description = ""
date = 2019-06-13T21:39:21Z
aliases = []
[extra]
id = 9184
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "awk",
  "c",
  "cobol",
  "d",
  "go",
  "j",
  "javascript",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "sidef",
  "tcl",
]
+++

[[wp:Decision_table|Decision Tables]] are a precise yet compact way to model complicated logic.


## Task

Demonstrate how your language implements decision tables.

Use this [[wp:Decision_table#Example|example]] of Printer Troubleshooting given in the Wikipedia article.






## Ada

First the specification of a generic decision table package:

```Ada
generic
   type Condition is (<>);
   type Action is (<>);
   with function Ask_Question (Cond: Condition) return Boolean;
   with procedure Give_Answer (Act:  Action);
   with procedure No_Answer;

package Generic_Decision_Table is

   type Answers is array(Condition) of Boolean;
   type Rule_R is record
      If_Then: Answers;
      Act:     Action;
   end record;
   type Rule_A is array(Positive range <>) of Rule_R;

   procedure React(Rules: Rule_A);

end Generic_Decision_Table;

```

Next, the implementation of the generic decision table package:

```Ada
package body Generic_Decision_Table is
   procedure React(Rules: Rule_A) is
      A: Answers;
      Some_Answer: Boolean := False;
   begin
      for C in A'Range loop
         A(C) := Ask_Question(C);
      end loop;
      for R in Rules'Range loop
         if A = Rules(R).If_Then then
            Give_Answer(Rules(R).Act);
            Some_Answer := True;
         end if;
      end loop;
      if not Some_Answer then
         No_Answer;
      end if;
   end React;

end Generic_Decision_Table;

```

That was easy! Now we implement the printer troubleshooting application:

```Ada
with Generic_Decision_Table, Ada.Text_IO;

procedure Printer_Decision_Table is

   type Condition is (Does_Not_Print, Red_Light_Flashing, Unrecognised);
   type Action is (Power_Cable, Printer_Computer_Cable, Software_Installed,
                   New_Ink, Paper_Jam);

   function Question(Cond: Condition) return Boolean is
      use Ada.Text_IO;
      Ch: Character;
   begin
      case Cond is
         when Does_Not_Print =>
            Put("Printer does not print?");
         when Red_Light_Flashing =>
            Put("A red light is flashing?");
         when Unrecognised =>
            Put("Printer is unrecognised?");
      end case;
      Put_Line (" y/Y => 'yes', any other input: 'no'");
      Get(Ch);
      return (Ch='y') or (Ch='Y');
   end Question;

   procedure Answer(Act: Action) is
      use Ada.Text_IO;
   begin
      case Act is
         when Power_Cable =>
            Put_Line("Check the power cable!");
         when Printer_Computer_Cable =>
            Put_Line("Check the printer-computer cable!");
         when Software_Installed =>
            Put_Line("Ensure the printer software is installed!");
         when New_Ink =>
            Put_Line("Check/replace ink!");
         when Paper_Jam =>
            Put_Line("Check for paper jam!");
      end case;
   end Answer;

   procedure No_Answer is
   begin
      Ada.Text_IO.Put_Line("Sorry! I have no idea what to do now!");
   end No_Answer;

   package DT is new Generic_Decision_Table
     (Condition, Action, Question, Answer, No_Answer);

   R: DT.Rule_A := (((True,  False, True),  Power_Cable),
                    ((True,  True,  True),  Printer_Computer_Cable),
                    ((True,  False, True),  Printer_Computer_Cable),
                    ((True,  True,  True),  Software_Installed),
                    ((True,  False, True),  Software_Installed),
                    ((False, True,  True),  Software_Installed),
                    ((False, False, True),  Software_Installed),
                    ((True,  True,  True),  New_Ink),
                    ((True,  True,  False), New_Ink),
                    ((False, True,  True),  New_Ink),
                    ((False, True,  False), New_Ink),
                    ((True,  True,  False), Paper_Jam),
                    ((True,  False, False), Paper_Jam)
                   );

begin
   DT.React(R);
end Printer_Decision_Table;
```

Sample output:

```txt
> ./printer_decision_table
Printer does not print? y/Y => 'yes', any other input: 'no'
y
A red light is flashing? y/Y => 'yes', any other input: 'no'
n
Printer is unrecognised? y/Y => 'yes', any other input: 'no'
n
Check for paper jam!

> ./printer_decision_table
Printer does not print? y/Y => 'yes', any other input: 'no'
y
A red light is flashing? y/Y => 'yes', any other input: 'no'
y
Printer is unrecognised? y/Y => 'yes', any other input: 'no'
n
Check/replace ink!
Check for paper jam!

> ./printer_decision_table
Printer does not print? y/Y => 'yes', any other input: 'no'
n
A red light is flashing? y/Y => 'yes', any other input: 'no'
n
Printer is unrecognised? y/Y => 'yes', any other input: 'no'
n
Sorry! I have no idea what to do now!
```



## AutoHotkey


```AutoHotkey
Conditions =
(
Printer does not print			|Y|Y|Y|Y|N|N|N|N|
A red light is flashing			|Y|Y|N|N|Y|Y|N|N|
Printer is unrecognized			|Y|N|Y|N|Y|N|Y|N|
)

Actions=
(
Check the power cable			| | |x| | | | | |
Check the printer-computer cable	|x| |x| | | | | |
Ensure printer software is installed	|x| |x| |x| |x| |
Check/replace ink			|x|x| | |x|x| | |
Check for paper jam			| |x| |x| | | | |
)

Condition:=[], Action:=[], Correlation:=[]
loop, parse, Conditions, `n
{
    No:= A_Index, 	RegExMatch(A_LoopField, "^(.*?)\t+(.*)", m),	Cond%No% := m1
    for k, v in StrSplit(m2, "|")
	Condition[No, k] := v="Y"?1:0
}
loop, parse, Actions, `n
{
    No:= A_Index	, RegExMatch(A_LoopField, "^(.*?)\t+(.*)", m),	Act%No% := m1
    for k, v in StrSplit(m2, "|")
	Action[No, A_Index] := v="X"?1:0
}

loop, % Condition[1].MaxIndex()
{
    j := A_Index,	    CondLine:=ActLine:=""
    loop, % Condition.MaxIndex()
	CondLine .= Condition[A_Index,j]
    loop, % Action.MaxIndex()
	ActLine.= Action[A_Index,j]?1:0
    Correlation[CondLine]:=ActLine
}

Gui, font,, Courier
Gui, add, text, w456
Gui, add, text, wp h1 0x7 y+0
loop, parse, Conditions, `n
{
    Gui, add, text, y+0 , % A_LoopField
    Gui, add, text, wp h1 0x7 y+0
}
Gui, add, text, wp
Gui, add, text, wp h1 0x7 y+0
loop, parse, Actions, `n
{
    Gui, add, text, y+0 , % A_LoopField
    Gui, add, text, wp h1 0x7 y+0
}
Gui, add, text, wp
loop, % Condition.MaxIndex()
    Gui, add, Checkbox,vC%A_Index% gSubmit wp h15, % Cond%A_Index%
Gui, add, text, wp , take the following actions(s):
AM := Action.MaxIndex()
Gui, add, Edit, vOutput ReadOnly r%AM% wp -TabStop
Gui, show
return

Submit:
Gui, Submit, NoHide
CondLine:=Res:=""
loop, % Condition.MaxIndex()
    CondLine.= C%A_Index%
MyCorr := Correlation[CondLine]
loop, parse, MyCorr
    if A_LoopField
	Res .= Act%A_Index% "`n"
GuiControl,, Output, % Res
return
```


## AWK


```AWK

# syntax: GAWK -f DECISION_TABLES.AWK DECISION_TABLES.TXT
BEGIN {
    FS = ":"
}
{   if ($0 ~ /^#/) { # comment
      next
    }
    if (rule_length == 0) { # 1st rule
      rule_length = length($2)
    }
    if (rule_length != length($2)) {
      error(sprintf("FILENAME=%s, FNR=%d, rule length not consistent with previous lengths, %s",FILENAME,FNR,$0))
    }
    if ($1 == "C") { # condition
      if ($2 !~ /^[NY]+$/) {
        error(sprintf("FILENAME=%s, FNR=%d, rule S/B 'N' or 'Y', %s",FILENAME,FNR,$0))
      }
      c_arr[++c] = $2 SUBSEP $3
    }
    else if ($1 == "A") { # action
      if ($2 !~ /^[X-]+$/) {
        error(sprintf("FILENAME=%s, FNR=%d, rule S/B 'X' or '-', %s",FILENAME,FNR,$0))
      }
      a_arr[++a] = $2 SUBSEP $3
    }
}
END {
    validate_rules()
    if (errors > 0) { exit(1) }
    show_actions(ask_conditions())
    exit(0)
}
function ask_conditions(  ans,i,key) {
    print("condtions:")
    for (i=1; i<=c; i++) {
      while (1) {
        printf("  %s? ",getext(c_arr[i]))
        getline ans <"con"
        if (ans ~ /^[nNyY]/) {
          key = key toupper(substr(ans,1,1))
          break
        }
      }
    }
    return(key)
}
function getext(str) {
    return(substr(str,index(str,SUBSEP)+1))
}
function show_actions(user_reply,  hits,i,key) {
    key = key_arr[user_reply]
    print("actions:")
    for (i=1; i<=a; i++) {
      if (substr(a_arr[i],key,1) == "X") {
        printf("  %s\n",getext(a_arr[i]))
        hits++
      }
    }
    printf("%d found\n",hits)
}
function validate_rules(  i,j,key) {
    for (i=1; i<=rule_length; i++) {
      key = ""
      for (j=1; j<=c; j++) {
        key = key substr(c_arr[j],i,1)
      }
      if (key in key_arr) {
        error(sprintf("duplicate key: %s",key))
      }
      key_arr[key] = i
    }
}
function error(message) {
    printf("error: %s\n",message)
    errors++
    return
}

```

<p>decision table:</p>

```txt

# RULES    CONDITIONS
C:YYYYNNNN:Printer does not print
C:YYNNYYNN:A red light is flashing
C:YNYNYNYN:Printer is unrecognized
# RULES    ACTIONS
A:--X-----:Check the power cable
A:X-X-----:Check the printer-computer cable
A:X-X-X-X-:Ensure printer software is installed
A:XX--XX--:Check/replace ink
A:-X-X----:Check for paper jam

```

<p>Output:</p>

```txt

condtions:
  Printer does not print? Y
  A red light is flashing? Y
  Printer is unrecognized? Y
actions:
  Check the printer-computer cable
  Ensure printer software is installed
  Check/replace ink
3 found

condtions:
  Printer does not print? N
  A red light is flashing? N
  Printer is unrecognized? N
actions:
0 found

```



## C

With flaky keyboard input:
```c
#include <stdio.h>

#define N_COND 3
#define COND_LEN (1 << N_COND)

struct { const char *str, *truth;}
cond[N_COND] = {
	{"Printer does not print",		"1111...."},
	{"A red light is flashing",		"11..11.."},
	{"Printer is unrecognised",		"1.1.1.1."},
},
solu[] = {
	{"Check the power cable",		"..1....."},
	{"Check the printer-computer cable",	"1.1....."},
	{"Ensure printer software is installed","1.1.1.1."},
	{"Check/replace ink",			"11..11.."},
	{"Check for paper jam",			".1.1...."},
};

int main()
{
	int q, ans, c;

	for (q = ans = c = 0; q < N_COND; q++) {
		do {
			if (c != '\n') printf("%s? ", cond[q].str);
			c = getchar();
		} while (c != 'y' && c != 'n');
		ans = (ans << 1) | (c != 'y');
	}

	if (ans == COND_LEN - 1)
		printf("\nSo, you don't have a problem then?\n");
	else {
		printf("\nSolutions:\n");
		for (q = 0; q < sizeof(solu)/sizeof(solu[0]); q++)
			if (solu[q].truth[ans] == '1')
				printf("    %s\n", solu[q].str);
	}
	return 0;
}
```
output<lang>Printer does not print? y
A red light is flashing? n
Printer is unrecognised? y

Solutions:
    Check the power cable
    Check the printer-computer cable
    Ensure printer software is installed
```



## COBOL


```cobol>        >
 SOURCE FORMAT FREE
identification division.
program-id. 'decisiontable'.

environment division.
configuration section.
repository.
    function all intrinsic.

data division.

working-storage section.

01  conditions.
    03  notprinting pic x.
    03  flashing pic x.
    03  notrecognized pic x.

procedure division.
start-decision-table.

display space

display 'The printer does not print (Y or N) ' with no advancing
accept notprinting

display 'A red light is flashing (Y or N) ' with no advancing
accept flashing

display 'The printer is unrecognized (Y or N) ' with no advancing
accept notrecognized

move upper-case(conditions) to conditions

display space

*>decision table Printer troubleshooter

*>  conditions
*>Printer does not print               Y  Y  Y  Y  N  N  N  N
*>A red light is flashing              Y  Y  N  N  Y  Y  N  N
*>Printer is unrecognized              Y  N  Y  N  Y  N  Y  N
*>  actions
*>Check the power cable                      X
*>Check the printer-computer cable     X     X
*>Ensure printer software is installed X     X     X     X
*>Check/replace ink                    X  X        X  X
*>Check for paper jam                     X     X

*>end decision table

evaluate notprinting also flashing also notrecognized

when 'Y' also 'Y' also 'Y'
    display 'Check the printer-computer cable'
    display 'Ensure printer software is installed'
    display 'Check/replace ink'

when 'Y' also 'Y' also 'N'
    display 'Check/replace ink'
    display 'Check for paper jam'

when 'Y' also 'N' also 'Y'
    display 'Check the power cable'
    display 'Check the printer-computer cable'
    display 'Ensure printer software is installed'

when 'Y' also 'N' also 'N'
    display 'Check for paper jam'

when 'N' also 'Y' also 'Y'
    display 'Ensure printer software is installed'
    display 'Check/replace ink'

when 'N' also 'Y' also 'N'
    display 'Check/replace ink'

when 'N' also 'N' also 'Y'
    display 'Ensure printer software is installed'

when 'N' also 'N' also 'N'
    display 'no action found'

when other
    display 'invalid input: ' notprinting space flashing space notrecognized

end-evaluate

display space

stop run
.

end program 'decisiontable'.

```


```txt
$ cobc -xj decisiontable.cob

The printer does not print (Y or N) n
A red light is flashing (Y or N) n
The printer is unrecognized (Y or N) y

Ensure printer software is installed

```



## D


```d
import std.stdio, std.algorithm, std.exception, std.array;

immutable struct DecisionTable {
    alias immutable(bool)[] IBA;
    const string[] conds,  actions;
    immutable IBA[IBA] rules;

    private static immutable(bool[]) growTo(in bool[] b,
                                            in size_t newLen)
    pure nothrow {
        auto result = new bool[newLen];
        result[0 .. b.length] = b[];
        return result.assumeUnique;
    }

    this(immutable string[] c,
         immutable string[] a,
         immutable bool[][][] q) pure nothrow {
        conds = c;
        actions = a;
        IBA[IBA] r;
        foreach (p; q)
            r[growTo(p[0], conds.length)] =
                growTo(p[1], actions.length);
        rules = r.assumeUnique;
    }

    string[] test(in bool[] tested,
                  in string NoMatchMsg="It is fine :)")
    const pure nothrow {
        string[] rightActions;
        auto iTested = growTo(tested, conds.length);
        if (iTested in rules)
            foreach (immutable i, immutable e; rules[iTested])
                if (e)
                    rightActions ~= actions[i];

        if (!rightActions.empty)
            return rightActions;
        return [NoMatchMsg];
    }

    void consult() const {
        bool[] query;

        foreach (cond; conds) {
            write(cond, "? [y=yes/others=no] ");
            string answer = "no";
            try
                answer = stdin.readln;
            catch (StdioException)
                writeln("no");
            query ~= !!answer.startsWith('y', 'Y');
        }

        writefln("%-(%2s\n%)", test(query));
    }
}

void main() {
    enum { F = false, T = true }
    immutable d = immutable(DecisionTable)(
            ["Printer is unrecognised",
             "A red light is flashing",
             "Printer does not print"],

            ["Check the power cable",
             "Check the printer-computer cable",
             "Ensure printer software is installed",
             "Check/replace ink",
             "Check for paper jam"],

             [[[T, F, F], [F, F, T]],
              [[F, T, F], [F, F, F, T]],
              [[T, T, F], [F, F, T, T]],
              [[F, F, T], [F, F, F, F, T]],
              [[T, F, T], [T, T, T]],
              [[F, T, T], [F, F, F, T, T]],
              [[T, T, T], [F, T, T, T, F]]
             ]
        );

    d.consult;
}
```

```txt
Printer is unrecognised? [y=yes/others=no] no
A red light is flashing? [y=yes/others=no] no
Printer does not print? [y=yes/others=no] no
It is fine :)
```



### Alternative Version

```d
import std.stdio, std.string;

struct DataPair(size_t N) {
    string message;
    immutable char[N] truth;
}

immutable DataPair!8[] conditions = [
    {"Printer does not print",               "####...."},
    {"A red light is flashing",              "##..##.."},
    {"Printer is unrecognised",              "#.#.#.#."}];

immutable DataPair!8[] solutions = [
    {"Check the power cable",                "..#....."},
    {"Check the printer-computer cable",     "#.#....."},
    {"Ensure printer software is installed", "#.#.#.#."},
    {"Check/replace ink",                    "##..##.."},
    {"Check for paper jam",                  ".#.#...."}];

void main() {
    size_t code = 0;

    foreach (immutable cond; conditions) {
        write(cond.message, "? [y=yes/others=no] ");
        string answer = "no";
        try
            answer = stdin.readln();
        catch (StdioException)
            writeln("no");
        code = (code << 1) | !answer.startsWith('y', 'Y');
    }

    if (code == (2 ^^ conditions.length) - 1)
        writeln("\nSo, you don't have a problem then?");
    else {
        writeln("\nSolutions:");
        foreach (immutable sol; solutions)
            if (sol.truth[code] == '#')
                writeln("    ", sol.message);
    }
}
```

```txt
Printer does not print? [y=yes/others=no] no
A red light is flashing? [y=yes/others=no] no
Printer is unrecognised? [y=yes/others=no] no

So, you don't have a problem then?
```



## Go

Go has no specific support for decision tables, but they can be implemented easily.  With a little ingenuity, literal data can be arranged in rows and columns in a way that preserves the visual associations of decision tables.  Go has an init function that might be useful for compiling decision tables at program startup.  And Go maps allow efficient lookup of actions given conditions.

```go
package main

import (
    "errors"
    "fmt"
    "os"
)

type dtText struct {
    rules, text string
}

var ptText = []dtText{
    {"YYYYNNNN", "Printer does not print"},
    {"YYNNYYNN", "A red light is flashing"},
    {"YNYNYNYN", "Printer is unrecognised"},
    {"--------", ""},
    {"  X     ", "Check the power cable"},
    {"X X     ", "Check the printer-computer cable"},
    {"X X X X ", "Ensure printer software is installed"},
    {"XX  XX  ", "Check/replace ink"},
    {" X X    ", "Check for paper jam"},
}

type dtMap map[string][]string

func compileDT(t []dtText) (dtMap, error) {
    if len(t) == 0 {
        return nil, errors.New("Empty decision table")
    }
    var conditions, actions []dtText
    ruleColumns := len(t[0].rules)
    for i, row := range t {
        if len(row.rules) != ruleColumns {
            return nil, errors.New("Inconsistent number of rule columns")
        }
        if len(row.text) == 0 {
            if conditions != nil {
                return nil, errors.New("Multple separator lines")
            }
            if i == 0 {
                return nil, errors.New("No conditions specified")
            }
            if i == len(t)-1 {
                return nil, errors.New("No actions specified")
            }
            conditions = t[:i]
            actions = t[i+1:]
        }
    }
    if conditions == nil {
        return nil, errors.New("Missing separator line")
    }
    m := make(map[string][]string, ruleColumns)
    kb := make([]byte, len(conditions))
    for col := 0; col < ruleColumns; col++ {
        for i, c := range conditions {
            kb[i] = c.rules[col]
        }
        key := string(kb)
        for _, a := range actions {
            if a.rules[col] != ' ' {
                m[key] = append(m[key], a.text)
            }
        }
    }
    return m, nil
}

func init() {
    var err error
    if ptMap, err = compileDT(ptText); err != nil {
        fmt.Println(err)
        os.Exit(1)
    }
}

var ptMap dtMap

func main() {
    for _, a := range ptMap["NYY"] {
        fmt.Println(a)
    }
}
```

Output:

```txt

Ensure printer software is installed
Check/replace ink

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
record cond(text,flags)
record  act(text,flags,aflags)

procedure main()
   DT := [
      cond("Printer does not print",                  "YYYYNNNN"),
      cond("A red light is flashing",		      "YYNNYYNN"),
      cond("Printer is unrecognised",		      "YNYNYNYN"),
      ,                                            # separator
      act("Check the power cable",		      "NNYNNNNN"),
      act("Check the printer-computer cable",	      "YNYNNNNN"),
      act("Ensure printer software is installed",     "YNYNYNYN"),
      act("Check/replace ink",			      "YYNNYYNN"),
      act("Check for paper jam",		      "NYNYNNNN") ]

   every (act := [])[1 to *DT] := ''               # empty csets for actions
   ans := ''                                       # empty answer cset
   every d := DT[i := 1 to *DT] do {
      /conds := list(*d.flags,'')
      case type(d) of {
         "cond":{
            sel := cset(&cset[i])                  # selector for this cond
            ans ++:= ( ask(d.text) == "Y", sel)    # ... add to answer
            every j := 1 to *conds do              # assemble condition flags
               if d.flags[j] == "Y" then conds[j] ++:= sel
            }
         "act":{
            d.aflags := list(*conds,'')
            every j := 1 to *conds do {
               if d.flags[j] == "Y" then
                  d.aflags[j] := conds[j]          # matching conditions
               if d.aflags[j] == ans
                  then write(d.text)               # matched, write action
               }
            }
         default: write("----------------------------")   # separator
         }
      }
end

procedure ask(text)
repeat {
   writes(text," ? ")
   a := map(trim(read()),&lcase,&ucase)
   case a of {
      "Y": return a
      "N": fail
      default: write(a," is an invalid response, enter a Y or N, ")
      }
   }
end
```


Sample Output:
```txt
Printer does not print ? Y
A red light is flashing ? N
Printer is unrecognised ? N
----------------------------
Check for paper jam
```



## J

'''Solution''':
```j
require'strings'

'RULE_NAMES    RULES'=: |:':'&cut;._2 noun define
 	Printer does not print:              	Y   Y   Y   Y   N   N   N   N
 	A red light is flashing:             	Y   Y   N   N   Y   Y   N   N
 	Printer is unrecognised:             	Y   N   Y   N   Y   N   Y   N
)

'ACTION_NAMES  ACTIONS'=: |:':'&cut;._2 noun define
 	Check the power cable:               	-   -   X   -   -   -   -   -
 	Check the printer-computer cable:    	X   -   X   -   -   -   -   -
 	Ensure printer software is installed:	X   -   X   -   X   -   X   -
 	Check/replace ink:                   	X   X   -   -   X   X   -   -
 	Check for paper jam:                 	-   X   -   X   -   -   -   -
)

assert (-:~.)|: 'Y' =/&;: RULES
RULE_TABLE=: (,/'X'=/&;: ACTIONS) /:&|: 'Y' =/&;: RULES

troubleshoot =:  verb define
   RULE_TABLE troubleshoot~ RULE_NAMES ,&< ACTION_NAMES
:
   'q a'=.x
   smoutput 'Having trouble?  Let''s track down the problem:'
   options=. a #~ y {~ #. 'Y'={.@toupper@deb@(1!:1)@1:@smoutput@,&'?'@dtb"1 q
   (,~ ('/'cut'Suggested resolutions:/Solution unknown.') {::~ 0=#) options
)
```


'''Example''' (''solution found''):
```j
   troubleshoot ''
Having trouble?  Let's track down the problem:
 	Printer does not print?
Y
 	A red light is flashing?
Y
 	Printer is unrecognised?
Y
Suggested resolutions:
 	Check the printer-computer cable
 	Ensure printer software is installed
 	Check/replace ink
```

'''Example''' (''solution not found''):
```j
   troubleshoot ''
Having trouble?  Let's track down the problem:
	Printer does not print?
N
	A red light is flashing?
N
	Printer is unrecognised?
N
Solution unknown.

```



###  Comments

The only interesting line in this solution is the one that assigns <tt>RULE_TABLE</tt>.  The rest is mostly ancillary support.

For large numbers of rules and few actions, J's native support of sparse arrays might provide a performance advantage, particularly in space.  A minor note about the implementation here: the verb (function) <tt>troubleshoot</tt> is generalized, and reusable on any set of rule table, questions, and suggested actions.  The default is to use those given in the printer troubleshooting table.




## JavaScript


### Interactive

Since this is an interactive web page, results can't be listed. See http://jsfiddle.net/rGP6C/1/ for a live demo.

```javascript><html

<head></head>
<body>
    <form id="form"></form>
    <br/>Possible solutions:
    <ul id="ul"></ul>
</body>
<script src="http://code.jquery.com/jquery-latest.min.js"></script>
<script type="text/javascript">
    var questions = [
        { bools:[1,1,1,1,0,0,0,0], text:"Printer does not print" },
        { bools:[1,1,0,0,1,1,0,0], text:"A red light is flashing" },
        { bools:[1,0,1,0,1,0,1,0], text:"Printer is unrecognized" },
    ];
    var answers = [
        { bools:[0,0,1,0,0,0,0,0], text:"Check the power cable" },
        { bools:[1,0,1,0,0,0,0,0], text:"Check the printer-computer cable" },
        { bools:[1,0,1,0,1,0,1,0], text:"Ensure printer software is installed" },
        { bools:[1,1,0,0,1,1,0,0], text:"Check/replace ink" },
        { bools:[0,1,0,1,0,0,0,0], text:"Check for paper jam" },
    ]

    $(document).ready(function() {
        // Init form with questions. "Value" is a descending power of 2.
        var value = questions[0].bools.length;
        for (var i = 0; i < questions.length; i++) {
            value /= 2;
            var el = '<br /><input type="checkbox" value="' + value + '">' + questions[i].text;
            $("#form").append(el);
        }

        // Respond to a checkbox action.
        $('input:checkbox').change(function() {

            // Figure out which combination of checkboxes the user selected.
            var sum = 0;
            $('input:checkbox:checked').each(function () {
                sum += Number(this.value);
            });

            // Translate sum into an index (column #) into bools.
            var index = questions[0].bools.length - sum - 1;

            // Clear the answers.
            $('#ul').html('');

            // Add appropriate answers.
            for (var i = 0; i < answers.length; i++) {
                if (answers[i].bools[index]) {
                    $('#ul').append('<li>' + answers[i].text + '</li>')
                }
            }
        });
    });
</script>
</html>
```


### DecTab object

This implements a DecisionTable object called <code>DecTab</code>. The JavaScript dialect is Microsoft's JScript.

```javascript

var DecTab = function () {
  this.conditions = [];
  this.rules = [];
  this.actions = [];
}

DecTab.prototype.Conditions = function () {
  var args = [].slice.call(arguments);
  for (var i = 0; i < args.length; i++) {
    this.conditions.push(args[i]);
  }
  return this;
};

DecTab.prototype.RulesActions = function (rules, actions) {
  var count = 0;
  var actionable;
  this.rules.push(rules)
  this.actions.push(actions);
  return this;
};

DecTab.prototype.Decide = function () {
  var decision = [];
  var decided = false;
  for (var i = 0; i < this.conditions.length; i++) {
    decision.push((this.conditions[i]()));
  }
  var decisionstring = "." + decision.join(".") + ".";

  for (i = 0; i < this.rules.length; i++) {
    var rule = [];
    for (var j = 0; j < this.rules[i].length; j++) {
      rule.push((this.rules[i][j]));
    }
    var rulestring = "." + rule.join(".") + ".";
    if (rulestring === decisionstring) {
      decided = true;
      for (var k = 0; k < this.actions[i].length; k++) {
        this.actions[i][k]();
      }
      break;
    }
  }
  if (!decided) {
    WScript.Echo("No decision table rows matched.");
  }
}

```

The task is rendered as follows

```javascript

  function Ask(q) {
    WScript.StdOut.Write(q);
    WScript.StdOut.Write(" [Y/N]?");
    var ans = WScript.StdIn.ReadLine();
    ans = ans.substr(0, 1).toUpperCase() === "Y" ? "Y" : "N";
    return ans;
  }
  function Tell(w) {
    WScript.Echo(w);
  }

  function Check_the_power_cable() {
    Tell("Check the power cable.");
  }
  function Check_the_printer_computer_cable() {
    Tell("Check the printer-computer cable.");
  }
  function Ensure_printer_software_is_installed() {
    Tell("Ensure printer software is installed.");
  }
  function Check_replace_ink() {
    Tell("Check/replace ink.");
  }
  function Check_for_paper_jam() {
    Tell("Check for paper jam.");
  }

  function Printer_Prints() {
    return Ask("Printer prints");
  }
  function A_red_light_is_flashing() {
    return Ask("A red light is flashing");
  }
  function Printer_is_recognized_by_computer() {
    return Ask("Printer is recognized by computer");
  }

  var DT = new DecTab()
  .Conditions(
    Printer_Prints,
    A_red_light_is_flashing,
    Printer_is_recognized_by_computer)
  .RulesActions(["N", "Y", "N"], [Check_the_printer_computer_cable, Ensure_printer_software_is_installed, Check_replace_ink])
  .RulesActions(["N", "Y", "Y"], [Check_replace_ink, Check_for_paper_jam])
  .RulesActions(["N", "N", "N"], [Check_the_power_cable, Check_the_printer_computer_cable, Ensure_printer_software_is_installed])
  .RulesActions(["N", "N", "Y"], [Check_for_paper_jam])
  .RulesActions(["Y", "Y", "N"], [Ensure_printer_software_is_installed, Check_replace_ink])
  .RulesActions(["Y", "Y", "Y"], [Check_replace_ink])
  .RulesActions(["Y", "N", "N"], [Ensure_printer_software_is_installed])
  .RulesActions(["Y", "N", "Y"], [])
  .Decide();

```

And a sample run

```text

C:\>CScript DecisionTable.js
Printer prints [Y/N]?Y
A red light is flashing [Y/N]?Y
Printer is recognized by computer [Y/N]?Y
Check/replace ink.

```




## Julia

```julia
const queries = [("Printer does not print",  0b11110000),
                 ("A red light is flashing", 0b11001100),
                 ("Printer is unrecognised", 0b10101010)]

const answers = Dict(0b00100000 => "Check the power cable",
                     0b10100000 => "Check the printer-computer cable",
                     0b10101010 => "Ensure printer software is installed",
                     0b11001100 => "Check/replace ink",
                     0b01010000 => "Check for paper jam",
                     0b00000001 => "Cannot diagnose any problem.")

function ynprompt(prompt)
    while true
        print(prompt, "?: ")
        if (ans = strip(uppercase(readline()))[1]) in ['Y', 'N']
            return ans
        end
    end
end

function decide(queries, answers)
    cond = 0b11111111
    for (prompt, value) in queries
        cond &= (ynprompt(prompt) == 'Y' ? value : UInt8(~value))
    end
    for (bitpat, diagnosis) in answers
        if cond & bitpat != 0
            println(diagnosis)
        end
    end
end

decide(queries, answers)

```
```txt

Printer does not print?: y
A red light is flashing?: y
Printer is unrecognised?: n
Check/replace ink
Check for paper jam

```



## Kotlin


```scala
// version 1.1.51

val conditions = listOf(
    "Printer prints"                    to "NNNNYYYY",
    "A red light is flashing"           to "YYNNYYNN",
    "Printer is recognized by computer" to "NYNYNYNY"
)

val actions = listOf(
    "Check the power cable"                to "NNYNNNNN",
    "Check the printer-computer cable"     to "YNYNNNNN",
    "Ensure printer software is installed" to "YNYNYNYN",
    "Check/replace ink"                    to "YYNNNYNN",
    "Check for paper jam"                  to "NYNYNNNN"
)

fun main(args: Array<String>) {
    val nc = conditions.size
    val na = actions.size
    val nr = conditions[0].second.length  // number of rules
    val np = 7  // index of 'no problem' rule
    println("Please answer the following questions with a y or n:")
    val answers = BooleanArray(nc)
    for (c in 0 until nc) {
        var input: String
        do {
            print("  ${conditions[c].first} ? ")
            input = readLine()!!.toUpperCase()
        }
        while (input != "Y" && input != "N")
        answers[c] = (input == "Y")
    }
    println("\nRecommended action(s):")
    outer@ for (r in 0 until nr) {
        for (c in 0 until nc) {
            val yn = if (answers[c]) 'Y' else 'N'
            if (conditions[c].second[r] != yn) continue@outer
        }
        if (r == np) {
            println("  None (no problem detected)")
        }
        else {
            for (a in 0 until na) {
                if (actions[a].second[r] == 'Y') println("  ${actions[a].first}")
            }
        }
        return
    }
}
```


Sample input/output:

```txt

Please answer the following questions with a y or n:
  Printer prints ? n
  A red light is flashing ? n
  Printer is recognized by computer ? n

Recommended action(s):
  Check the power cable
  Check the printer-computer cable
  Ensure printer software is installed

```



## Perl


```perl
use strict;
use warnings;

sub decide {
    our(@q,%a);
    local *q = shift;
    local *a = shift;
    my $bit;
    my $cnt = 1;
    for my $prompt (@q) {
        print "$prompt: ";
        $bit += $cnt if <> =~ /y/i;
        $cnt *= 2;
    }
    $bit = 2**$bit;

    while (my ($bitpat,$diagnosis) = each %a) {
        print "$diagnosis\n" if $bit & $bitpat;
    }
}

my @queries = (
    'Printer is unrecognised',
    'A red light is flashing',
    'Printer does not print',
);
my %answers = (
    0b00100000 => 'Check the power cable',
    0b10100000 => 'Check the printer-computer cable',
    0b10101010 => 'Ensure printer software is installed',
    0b11001100 => 'Check/replace ink',
    0b01010000 => 'Check for paper jam',
);

decide(\@queries,\%answers);
```

```txt
Printer is unrecognised: n
A red light is flashing: n
Printer does not print: y
Check for paper jam
```



## Perl 6


```perl6
sub decide (@q, @s) {
    my $bit = 2 ** [+] (1,2,4...*) Z* reverse @q.map: {
	so prompt(.value ~ "? ") ~~ /:i ^y/;
    }
    say "  $_" for @s.grep(*.key +& $bit)».value || "No clue!";
}

loop {
    decide
    (
	  "Y Y Y Y N N N N" => "Printer does not print",
	  "Y Y N N Y Y N N" => "A red light is flashing",
	  "Y N Y N Y N Y N" => "Printer is unrecognised",
    ),
    (
	:2<0_0_1_0_0_0_0_0> => "Check the power cable",
	:2<1_0_1_0_0_0_0_0> => "Check the printer-computer cable",
	:2<1_0_1_0_1_0_1_0> => "Ensure printer software is installed",
	:2<1_1_0_0_1_1_0_0> => "Check/replace ink",
	:2<0_1_0_1_0_0_0_0> => "Check for paper jam",
    );
    say '';
}
```

A bit of explanation: we pass in two pair lists for the questions and solutions; we ignore the keys of the questions, since they can be
generated by regarding them as a binary counter from right to left, with the least significant bit on the bottom.  The <tt>@q.map</tt> runs the prompts and maps them to booleans
using case-insensitive matching.  We reverse that list and zip multiply with powers of two to figure out which bit we're going to grep for. (The zip stops on the shorter list, which is always going to be the list of booleans, since the list of powers of two is infinite.) We sum up those powers of two using a <tt>[+]</tt> reduction metaoperator, which in this case gives us a number from 0 to 7.  Then we take 2 to that power.

The solutions list of pairs is conveniently keyed on binary numbers written in colon radix notation, so we grep the keys containing the correct bit, then map the pair list to its values using a hyperoperator to parallelize it.  Unlike in Perl 5, we can use <tt>||</tt> on lists as well as scalars to provide a default result if nothing matches.
```txt
Printer does not print? n
A red light is flashing? y
Printer is unrecognised? n
  Check/replace ink

Printer does not print? y
A red light is flashing? n
Printer is unrecognised? y
  Check the power cable
  Check the printer-computer cable
  Ensure printer software is installed

Printer does not print? n
A red light is flashing? n
Printer is unrecognised? n
  No clue!

Printer does not print? ^C
```



## Phix

Written such that conditions and actions could easily be read from a plain text file,
and should cope fairly well with different lengths, missing/conflicting options, etc.

```Phix
constant conditions = """
YYYYNNNN:Printer does not print
YYNNYYNN:A red light is flashing
YNYNYNYN:Printer is unrecognized
"""
constant actions = """
--X-----:Check the power cable
X-X-----:Check the printer-computer cable
X-X-X-X-:Ensure printer software is installed
XX--XX--:Check/replace ink
-X-X----:Check for paper jam
"""

procedure validate(sequence s, integer l, string letters, name)
    if remainder(length(s),2) then
        throw(sprintf("uneven %s pairings",{name}))
    end if
    for i=1 to length(s) by 2 do
        string si = s[i]
        if length(si)!=l then
            throw(sprintf("mismatched lengths (line %d)",i))
        end if
        for j=1 to length(si) do
            if not find(si[j],letters) then
                throw(sprintf("invalid letter [%c not in %s]",{si[j],letters}))
            end if
        end for
    end for
end procedure

constant qa = split_any(conditions,"\n:",no_empty:=true),
         sa = split_any(actions,"\n:",no_empty:=true)
integer l = length(qa[1])

function ask_questions()
    sequence valid = repeat(true,l)
    integer a
    for i=1 to length(qa) by 2 do
        while true do
            puts(1,qa[i+1]&":? ")
            a = upper(wait_key())
            if find(a,"YN") then exit end if
            puts(1,"\nplease enter Y or N\n")
        end while
        printf(1,"%c\n",a)
        for j=1 to l do
            if a!=qa[i][j] then valid[j] = false end if
        end for
    end for
    return valid
end function

procedure show_answers(integer k)
    integer count = 0
    for i=1 to length(sa) by 2 do
        if sa[i][k]='X' then
            puts(1,sa[i+1]&"\n")
            count += 1
        end if
    end for
    printf(1,"%d answers\n",count)
end procedure

procedure main()
    try
        validate(qa,l,"YN","condition")
        validate(sa,l,"-X","action")
    catch e
        ?e
        abort(0)
    end try
    sequence valid = ask_questions()
    integer sv = sum(valid)
    if sv=0 then
        puts(1,"no answers")
    elsif sv=1 then
        show_answers(find(true,valid))
    else
        puts(1,"multiple answer sets\n")
        for i=1 to length(valid) do
            if valid[i] then
                printf(1,"for %d:\n",i)
                show_answers(i)
            end if
        end for
    end if
end procedure
main()
```

```txt

Printer does not print:? N
A red light is flashing:? Y
Printer is unrecognized:? Y
Ensure printer software is installed
Check/replace ink
2 answers

```



## PicoLisp

We allow ourselves a luxurious user interface:

```PicoLisp
(de yes? (Cond)
   (out NIL (prin (car Cond) "? "))
   (in NIL
      (use Reply
         (loop
            (setq Reply (read))
            (T (member Reply '(T Y YES Yes y yes true 1))
               T )
            (T (member Reply '(NIL N NO No n no false 0)))
            (prinl "Please answer 'Yes' or 'No'") ) ) ) )
```

The decision table used in the example:

```PicoLisp
(de *Conditions
   ("Printer does not print"                T   T   T   T  NIL NIL NIL NIL)
   ("A red light is flashing"               T   T  NIL NIL  T   T  NIL NIL)
   ("Printer is unrecognised"               T  NIL  T  NIL  T  NIL  T  NIL) )

(de *Actions
   ("Check the power cable"                NIL NIL  T)
   ("Check the printer-computer cable"      T  NIL  T)
   ("Ensure printer software is installed"  T  NIL  T  NIL  T  NIL  T)
   ("Check/replace ink"                     T   T  NIL NIL  T   T)
   ("Check for paper jam"                  NIL  T  NIL  T) )
```

The decision can be made directly on the condition and action data, without the need to create intermediate tables:

```PicoLisp
(de decide ()
   (let Reply (mapcar yes? *Conditions)
      (extract and
         (apply pick (append *Conditions *Actions)
            '(@
               (unless (pick '((Flg) (<> Flg (next))) Reply)
                  (rest) ) ) )
         (mapcar car *Actions) ) ) )
```

Output:

```txt
: (decide)
Printer does not print? y
A red light is flashing? y
Printer is unrecognised? n
-> ("Check/replace ink" "Check for paper jam")

: (decide)
Printer does not print? n
A red light is flashing? y
Printer is unrecognised? y
-> ("Ensure printer software is installed" "Check/replace ink")

: (decide)
Printer does not print? n
A red light is flashing? n
Printer is unrecognised? n
-> NIL
```



## Python


```python

'''
Create a Decision table then use it
'''

def dt_creator():
    print("\n\nCREATING THE DECISION TABLE\n")
    conditions = input("Input conditions, in order, separated by commas: ")
    conditions = [c.strip() for c in conditions.split(',')]
    print( ("\nThat was %s conditions:\n  " % len(conditions))
           + '\n  '.join("%i: %s" % x for x in enumerate(conditions, 1)) )
    print("\nInput an action, a semicolon, then a list of tuples of rules that trigger it. End with a blank line")
    action2rules, action = [], ' '
    while action:
        action = input("%i: " % (len(action2rules) + 1)).strip()
        if action:
            name, _, rules = [x.strip() for x in action.partition(';')]
            rules = eval(rules)
            assert all(len(rule) == len(conditions) for rule in rules), \
                   "The number of conditions in a rule to trigger this action is wrong"
            action2rules.append((name, rules))
    actions = [x[0] for x in action2rules]
    # Map condition to actions
    rule2actions = dict((y,[]) for y in set(sum((x[1] for x in action2rules), [])))
    for action, rules in action2rules:
        for r in rules:
            rule2actions[r].append( action )
    return conditions, rule2actions

def dt_user(dt, default=['Pray!']):
    conditions, rule2actions = dt
    print("\n\nUSING THE DECISION TABLE\n")
    rule = tuple(int('y' == input("%s? (Answer y if statement is true or n): " % c)) for c in conditions)
    print("Try this:\n  " + '\n  '.join(rule2actions.get(rule, default)))

if __name__ == '__main__':
    dt = dt_creator()
    dt_user(dt)
    dt_user(dt)
    dt_user(dt)
```


'''Sample Run'''

```txt


CREATING THE DECISION TABLE

Input conditions, in order, separated by commas: Printer does not print, A red light is flashing, Printer is unrecognised

That was 3 conditions:
  1: Printer does not print
  2: A red light is flashing
  3: Printer is unrecognised

Input an action, a semicolon, then a list of tuples of rules that trigger it. End with a blank line
1: Check the power cable; [(1,0,1)]
2: Check the printer-computer cable; [(1,1,1), (1,0,1)]
3: Ensure printer software is installed; [(1,1,1), (1,0,1), (0,1,1), (0,0,1)]
4: Check/replace ink;  [(1,1,1), (1,1,0), (0,1,1), (0,1,0)]
5: Check for paper jam; [(1,1,0), (1,0,0)]
6:


USING THE DECISION TABLE

Printer does not print? (Answer y if statement is true or n): n
A red light is flashing? (Answer y if statement is true or n): y
Printer is unrecognised? (Answer y if statement is true or n): y
Try this:
  Ensure printer software is installed
  Check/replace ink


USING THE DECISION TABLE

Printer does not print? (Answer y if statement is true or n): y
A red light is flashing? (Answer y if statement is true or n): n
Printer is unrecognised? (Answer y if statement is true or n): y
Try this:
  Check the power cable
  Check the printer-computer cable
  Ensure printer software is installed


USING THE DECISION TABLE

Printer does not print? (Answer y if statement is true or n): n
A red light is flashing? (Answer y if statement is true or n): n
Printer is unrecognised? (Answer y if statement is true or n): n
Try this:
  Pray!
```



## Racket


This implementation shows off the 2d language. The "Actions" could be a square array of empty and X boxes.
I thought it might be fun to merge them.


```racket
#lang unstable/2d racket

(define (ask-y/n q)
  (printf "~a [y/n]?" q)
  (define (get-y/n)
    (match (read-line)
      [(? eof-object?) eof]
      [(regexp #rx"^[yY]") #t]
      [(regexp #rx"^[nN]") #f]
      [_ (printf "reply yes or no, please:") (get-y/n)]))
  (get-y/n))

(define (cells->hash grid)
  (for*/hash ((block (in-list grid)) (address (in-list (car block))))
    (values address (match (cdr block)
                      ['(N) #f]
                      ['(Y) #t]
                      ['(X) 'X]
                      [else else]))))

(define (run-decision-table tbl)
  (match-define (list '2ddecision_table col-widths row-heights all-cells ...) tbl)

  ;; after this, the rules without an X are removed
  (define full-cells (filter (match-lambda [(list _ _ _ ...) #t] [_ #f]) all-cells))

  ;; cell addresses are (list column row)
  (match-define
    (list-no-order
     `((,_ ... (1 1) ,_ ...)                           ,caption ...)
     `(((,rules-columns ,_) ...)                        Rules)
     `(((,(app add1 text-columns) ,condition-rows) ...) Conditions)
     `(((,_ ,action-rows) ...)                          Actions)
     remaining-cells ...)
    full-cells)

  (define remaining-cells# (cells->hash remaining-cells))
  (define (cell# c r (dflt #f))
    (hash-ref remaining-cells# (list c r) dflt))

  (define text-column (first text-columns))

  (let question-loop ((remain-conds condition-rows) (remain-acts action-rows))
    (match remain-conds
      [(list) (displayln "I give up... read the manual or something.")]
      [(list conds-h conds-t ...)
       (match (ask-y/n (string-join (map ~a (cell# text-column conds-h)) " "))
         [(? eof-object?) "bye!"]
         [y/n-response
          (define remain-acts-
            (for/list
                ((action remain-acts)
                 #:when (for/first
                            ((rule-c (in-list rules-columns))
                             #:when (eq? (cell# rule-c conds-h) y/n-response) ; matches rule flag
                             #:when (equal? (cell# rule-c action #f) 'X)) ; has an X
                          #t))
              action))
          (match remain-acts-
            [(list) (printf "No more actions... no more suggestions from the rules!~%")]
            [(list only-action) (printf "Suggested action: ~s~%" (cell# text-column only-action))]
            [_ (question-loop conds-t remain-acts-)])])])))

(define printer-troubleshooting-2dtable
  '#2ddecision_table
  ╔═╦════════════╦════════════════════════════════════════════╦═╦═╦═╦═╦═╦═╦═╦═╗
  ║ ║            ║                                            ║ ║ ║ ║ ║ ║ ║ ║ ║
  ╠═╬════════════╩════════════════════════════════════════════╬═╩═╩═╩═╩═╩═╩═╩═╣
  ║ ║ Printer troubleshooter                                  ║ Rules         ║
  ╠═╬════════════╦════════════════════════════════════════════╬═══════╦═══════╣
  ║ ║ Conditions ║ Printer does not print                     ║Y      ║N      ║
  ╠═╣            ╠════════════════════════════════════════════╣   ╔═══╬═══╗   ║
  ║ ║            ║ A red light is flashing                    ║   ║N  ║Y  ║   ║
  ╠═╣            ╠════════════════════════════════════════════╣ ╔═╬═╗ ║ ╔═╬═╗ ║
  ║ ║            ║ Printer is unrecognized                    ║ ║N║Y║ ║ ║N║Y║ ║
  ╠═╬════════════╬════════════════════════════════════════════╬═╩═╬═╬═╩═╩═╩═╩═╣
  ║ ║ Actions    ║ Check the power cable                      ║   ║X║         ║
  ╠═╣            ╠════════════════════════════════════════════╬═╗ ║ ║         ║
  ║ ║            ║ Check the printer-computer cable           ║X║ ║ ║         ║
  ╠═╣            ╠════════════════════════════════════════════╣ ║ ║ ║ ╔═╗ ╔═╗ ║
  ║ ║            ║ Ensure printer software is installed       ║ ║ ║ ║ ║X║ ║X║ ║
  ╠═╣            ╠════════════════════════════════════════════╣ ╚═╬═╝ ║ ╚═╬═╝ ║
  ║ ║            ║ Check/replace ink                          ║   ║   ║   ║   ║
  ╠═╣            ╠════════════════════════════════════════════╬═╗ ║ ╔═╬═══╝   ║
  ║ ║            ║ Check for paper jam                        ║ ║ ║ ║X║       ║
  ╚═╩════════════╩════════════════════════════════════════════╩═╩═╩═╩═╩═══════╝)

(run-decision-table printer-troubleshooting-2dtable)
```


A trial run...

```txt
Printer does not print [y/n]?n
A red light is flashing [y/n]?y
Printer is unrecognized [y/n]?n
Suggested action: (Check/replace ink)
```



## REXX

This REXX example shows how one version of a decision table could be implemented,

There was additional support added to the code for:
::*   a   ''no solution found''   message
::*   a   ''don't care''   requirement   (regarding the decision table)
::*   a method of specifying requirements and not needing recoding for future queries
::*   used a minimalistic way in expressing the decision table
::*   extra prompting when there was a user response error
::*   handles superfluous blanks and some punctuation.
::*   displaying of a countdown so user knows length of interrogation
::*   visual fidelity aids for postmortem analysis (as in logs)
::*   a method of allowing the user to quit (opt-out of) the interrogation

```rexx
/*REXX program demonstrates a (query)  decision table  and  possible corrective actions.*/
Q.=;      Q.1 = 'Does the printer not print?'
          Q.2 = 'Is there a red light flashing on the printer?'
          Q.3 = 'Is the printer unrecognized by the software?'
          Q.0 =  3
action.=            /* Y=yes       N=no        if character isn't a letter = don't care.*/

      /*    ┌─────◄── answer to 1st question     (it can be in upper\lower\mixed case). */
      /*    │┌────◄──   "     " 2nd    "           "  "   "  "   "     "     "     "    */
      /*    ││┌───◄──   "     " 3rd    "           "  "   "  "   "     "     "     "    */
      /*    │││                                                                         */
      /*    ↓↓↓                                                                         */
action.1 = 'yny'  ;         pos.1 = 'Check the power cable.'
action.2 = 'y.y'  ;         pos.2 = 'check the printer-computer cable.'
action.3 = '..y'  ;         pos.3 = 'Ensure printer software is installed.'
action.4 = '.y.'  ;         pos.4 = 'Check/replace ink.'
action.5 = 'y.n'  ;         pos.5 = 'Check for paper jam.'

      do i=1  for Q.0;   ans.i=asker(i);   end   /*display the question, obtain response*/
say                                              /*display a blank line before questions*/
possible=0                                       /*we'll be counting possible solutions.*/

  do k=1  while action.k\==''                    /*filter the answers via decision table*/

                do j=1;      d=substr(action.k, j, 1);           upper d
                jm=j//Q.0;   if jm==0  then jm=Q.0
                if d==' '              then leave
                if \datatype(d, 'U')   then iterate
                if d\==ans.jm          then iterate k
                if j==Q                then leave
                end   /*j*/
  say pos.k                                      /*this could be a possible solution.   */
  possible=possible+1                            /*count number of possible solutions.  */
  end       /*k*/

if possible==0  then say 'No solutions given for the information supplied.'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
asker:  arg ?;   oops=0;         Qprefix=copies('─', 9)   '(question'   ?   "of"   Q.0') '
howTo = '(You can answer with a  Yes or No   [or  Quit])'

  do forever
  if oops  then do;  say;  say right(howTo,79);   say;   oops=0;   end
  say Qprefix Q.?;         parse pull x                    /*ask question (after prompt)*/
  x=strip(space(x),,'.');  parse upper var x  u  1  u1  2  /*u1=1st character of answer.*/
  if words(x)==0         then iterate                      /*Nothing entered? Try again.*/
  if abbrev('QUIT',u,1)  then exit                         /*user is tired of answering.*/
  if (abbrev('YES',u) | abbrev("NO",u)) & words(x)==1  then return u1
  say 'invalid response: '   x;          oops=1
  end   /*forever*/
```

'''output'''    (a screen scraping using a DOS prompt window for the possible responses)
DECISION.REX  is the REXX program that produced this output.
<pre style="height:50ex">
D:\►rexx decision

───────── (question 1 of 3)  Does the printer not print?
no
───────── (question 2 of 3)  Is there a red light flashing on the printer?
No
───────── (question 3 of 3)  Is the printer unrecognized by the software?
n

No solutions given for the information supplied.

D:\►rexx decision
───────── (question 1 of 3)  Does the printer not print?
n
───────── (question 2 of 3)  Is there a red light flashing on the printer?
n
───────── (question 3 of 3)  Is the printer unrecognized by the software?
yes

Ensure printer software is installed.

D:\►rexx decision
───────── (question 1 of 3)  Does the printer not print?
n
───────── (question 2 of 3)  Is there a red light flashing on the printer?
Ye
───────── (question 3 of 3)  Is the printer unrecognized by the software?
n

Check/replace ink.

D:\►rexx decision
───────── (question 1 of 3)  Does the printer not print?
n
───────── (question 2 of 3)  Is there a red light flashing on the printer?
yes.
───────── (question 3 of 3)  Is the printer unrecognized by the software?
   yes

Ensure printer software is installed.
Check/replace ink.

D:\►rexx decision
───────── (question 1 of 3)  Does the printer not print?
y
───────── (question 2 of 3)  Is there a red light flashing on the printer?
no
───────── (question 3 of 3)  Is the printer unrecognized by the software?
no

Check for paper jam.

D:\►rexx decision
───────── (question 1 of 3)  Does the printer not print?
y
───────── (question 2 of 3)  Is there a red light flashing on the printer?
n
───────── (question 3 of 3)  Is the printer unrecognized by the software?
y

Check the power cable.
check the printer-computer cable.
Ensure printer software is installed.

D:\►rexx decision
───────── (question 1 of 3)  Does the printer not print?
y
───────── (question 2 of 3)  Is there a red light flashing on the printer?
y
───────── (question 3 of 3)  Is the printer unrecognized by the software?
n

Check/replace ink.
Check for paper jam.

D:\►rexx decision
───────── (question 1 of 3)  Does the printer not print?
y
───────── (question 2 of 3)  Is there a red light flashing on the printer?
y
───────── (question 3 of 3)  Is the printer unrecognized by the software?
y

check the printer-computer cable.
Ensure printer software is installed.
Check/replace ink.

D:\►

```



## Ring


```ring

# Project : Decision tables

see "The printer does not print (Y or N) "
give notprinting
see "A red light is upper(flashing) (Y or N) "
give flashing
see "The printer is unrecognized (Y or N) "
give notrecognized

if upper(notprinting) = "Y" and upper(flashing) = "Y" and upper(notrecognized) = "Y"
   see "Check the printer-computer cable" + nl
   see "Ensure printer software is installed" + nl
   see "Check/replace ink"
elseif upper(notprinting) = "Y" and upper(flashing) = "Y" upper(notrecognized) = "N"
        see "Check/replace ink" + nl
        see "Check for paper jam" + nl
elseif upper(notprinting) = "Y" and  upper(flashing) = "N" and upper(notrecognized) = "Y"
        see "Check the power cable"
        see "Check the printer-computer cable" + nl
        see "Ensure printer software is installed" + nl
elseif upper(notprinting) = "Y" and upper(flashing) = "N" and upper(notrecognized) = "N"
        see "Check for paper jam" + nl
elseif upper(notprinting) = "N" and upper(flashing) = "Y" and upper(notrecognized) = "Y"
        see "Ensure printer software is installed" + nl
        see "Check/replace ink" + nl
elseif upper(notprinting) = "N" and upper(flashing) = "Y" and upper(notrecognized) = "N"
        see "Check/replace ink" + nl
elseif upper(notprinting) = "N" and upper(flashing) = "N" and upper(notrecognized) = "Y"
        see "Ensure printer software is installed" + nl
elseif upper(notprinting) = "N" and upper(flashing) = "N" and upper(notrecognized) = "N"
        see "no action found" + nl
else
        see "invalid input: " + upper(notprinting) + " " + upper(flashing) + " " + upper(notrecognized) + nl
ok

```

Output:

```txt

The printer does not print (Y or N) y
A red light is upper(flashing) (Y or N) y
The printer is unrecognized (Y or N) y
Check the printer-computer cable
Ensure printer software is installed
Check/replace ink

```



## Ruby


```ruby
class DecisionTable
  def initialize(conditions, actions)
    @conditions = conditions
    @actions = []
    @rules = []
    actions.each {|action, ruleset| @actions << action; @rules << ruleset}
  end

  def run
    puts "Conditions:"
    index = ask_conditions
    puts "Actions:"
    results = @rules.each_with_index.inject([]) do |sum, (ruleset, idx)|
      sum << @actions[idx] if ruleset[index] == 1
      sum
    end
    results << "PC LOAD LETTER" if results.empty?
    results.each {|res| puts "  #{res}"}
    puts ""
  end

  private
  def ask_conditions
    answers = @conditions.inject("") {|sum, c| sum + get_response(c)}
    answers.to_i(2)
  end

  def get_response(prompt)
    loop do
      print "  #{prompt}? "
      case STDIN.gets.strip.downcase
      when /^y/ then return "0"
      when /^n/ then return "1"
      end
    end
  end
end

dt = DecisionTable.new(
      [
        "Printer does not print",              #  Y Y Y Y N N N N
        "A red light is flashing",             #  Y Y N N Y Y N N
        "Printer is unrecognised",             #  Y N Y N Y N Y N
      ],
      [
        ["Check the power cable",                [0,0,1,0,0,0,0,0]],
        ["Check the printer-computer cable",     [1,0,1,0,0,0,0,0]],
        ["Ensure printer software is installed", [1,0,1,0,1,0,1,0]],
        ["Check/replace ink",                    [1,1,0,0,1,1,0,0]],
        ["Check for paper jam",                  [0,1,0,1,0,0,0,0]],
      ]
     )
loop {dt.run}
```


Example

```txt
Conditions:
  Printer does not print? y
  A red light is flashing? y
  Printer is unrecognised? y
Actions:
  Check the printer-computer cable
  Ensure printer software is installed
  Check/replace ink

Conditions:
  Printer does not print? y
  A red light is flashing? y
  Printer is unrecognised? n
Actions:
  Check/replace ink
  Check for paper jam

Conditions:
  Printer does not print? y
  A red light is flashing? n
  Printer is unrecognised? y
Actions:
  Check the power cable
  Check the printer-computer cable
  Ensure printer software is installed

Conditions:
  Printer does not print? y
  A red light is flashing? n
  Printer is unrecognised? n
Actions:
  Check for paper jam

Conditions:
  Printer does not print? n
  A red light is flashing? y
  Printer is unrecognised? y
Actions:
  Ensure printer software is installed
  Check/replace ink

Conditions:
  Printer does not print? n
  A red light is flashing? y
  Printer is unrecognised? n
Actions:
  Check/replace ink

Conditions:
  Printer does not print? n
  A red light is flashing? n
  Printer is unrecognised? y
Actions:
  Ensure printer software is installed

Conditions:
  Printer does not print? n
  A red light is flashing? n
  Printer is unrecognised? n
Actions:
  PC LOAD LETTER
```



## Sidef

```ruby
func decide (q, s) {

    var bits = q.map { |p|
        read("#{p.value}? ", String) ~~ /^y/i ? 1 : 0
    }

    var n = with (0) { |t|
        bits.each { |b|
            t <<= 1
            t |= b
        }
        1 << t
    }

    s.grep { .key & n }.map{ .value }.each { |ans|
        say "   #{ans}"
    }
}

loop {
    decide(
      [
        Pair("Y Y Y Y N N N N", "Printer does not print"),
        Pair("Y Y N N Y Y N N", "A red light is flashing"),
        Pair("Y N Y N Y N Y N", "Printer is unrecognised"),
      ],
      [
        Pair(0b0_0_1_0_0_0_0_0, "Check the power cable"),
        Pair(0b1_0_1_0_0_0_0_0, "Check the printer-computer cable"),
        Pair(0b1_0_1_0_1_0_1_0, "Ensure printer software is installed"),
        Pair(0b1_1_0_0_1_1_0_0, "Check/replace ink"),
        Pair(0b0_1_0_1_0_0_0_0, "Check for paper jam"),
      ]
    )
    say ''
}
```

```txt

Printer does not print? y
A red light is flashing? n
Printer is unrecognised? n
   Check for paper jam

Printer does not print? n
A red light is flashing? n
Printer is unrecognised? y
   Ensure printer software is installed

Printer does not print? y
A red light is flashing? y
Printer is unrecognised? n
   Check/replace ink
   Check for paper jam

Printer does not print? n
A red light is flashing? y
Printer is unrecognised? y
   Ensure printer software is installed
   Check/replace ink

Printer does not print? ^C

```


## Tcl


```tcl
package require TclOO

#http://rosettacode.org/wiki/Keyboard_Input/Obtain_a_Y_or_N_response#Tcl
proc yesno {{message "Press Y or N to continue"}} {
    fconfigure stdin -blocking 0
    exec stty raw
    read stdin ; # flush
    puts -nonewline "${message}: "
    flush stdout
    while {![eof stdin]} {
        set c [string tolower [read stdin 1]]
        if {$c eq "y" || $c eq "n"} break
    }
    puts [string toupper $c]
    exec stty -raw
    fconfigure stdin -blocking 1
    return [expr {$c eq "y"}]
}

oo::class create DecisionTable {
    variable qlist responses
    constructor {questions responseMap} {
	set qlist $questions
	set responses $responseMap
    }

    method consult {} {
	set idx 0
	foreach q $qlist {
	    set answer [yesno "$q? \[y/n\]"]
	    set idx [expr {$idx*2 + (1-$answer)}]
	}
	foreach {msg map} $responses {
	    # Allow a column to be omitted; magic!
	    if {"0[lindex $map $idx]"} {
		puts $msg
	    }
	}
    }
}
```

Demonstration:

```tcl
DecisionTable create printerDiagnosisTable {
    "Printer does not print"
    "A red light is flashing"
    "Printer is unrecognised"
} {
    "Check the power cable"			{0 0 1}
    "Check the printer-computer cable"		{1 0 1}
    "Ensure printer software is installed"	{1 0 1 0 1 0 1}
    "Check/replace ink"				{1 1 0 0 1 1}
    "Check for paper jam"			{0 1 0 1}
}
printerDiagnosisTable consult
```

Output:

```txt

Printer does not print? [y/n]: N
A red light is flashing? [y/n]: Y
Printer is unrecognised? [y/n]: Y
Ensure printer software is installed
Check/replace ink

```

