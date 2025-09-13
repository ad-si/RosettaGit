+++
title = "Execute a Markov algorithm"
description = ""
date = 2019-05-20T23:26:52Z
aliases = []
[extra]
id = 5222
[taxonomies]
categories = ["task", "Compilers and Interpreters"]
tags = []
+++

## Task

Create an interpreter for a [[wp:Markov algorithm|Markov Algorithm]].

Rules have the syntax:
 <ruleset> ::= ((<comment> | <rule>) <newline>+)*
 <comment> ::= # {<any character>}
 <rule> ::= <pattern> <whitespace> -> <whitespace> [.] <replacement>
 <whitespace> ::= (<tab> | <space>) [<whitespace>]
There is one rule per line.

If there is a   <b>.</b>   (period)   present before the   '''<replacement>''',   then this is a terminating rule in which case the interpreter must halt execution.

A ruleset consists of a sequence of rules, with optional comments.


<big><big> Rulesets </big></big>

Use the following tests on entries:


;Ruleset 1:

```txt

# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule

```

Sample text of:
: <code> I bought a B of As from T S. </code>
Should generate the output:
: <code> I bought a bag of apples from my brother. </code>


;Ruleset 2:
A test of the terminating rule

```txt

# Slightly modified from the rules on Wikipedia
A -> apple
B -> bag
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
```

Sample text of:
: <code>I bought a B of As from T S.</code>
Should generate:
: <code>I bought a bag of apples from T shop.</code>


;Ruleset 3:
This tests for correct substitution order and may trap simple regexp based replacement routines if special regexp characters are not escaped.

```txt

# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule

```

Sample text of:
: <code>I bought a B of As W my Bgage from T S.</code>
Should generate:
: <code>I bought a bag of apples with my money from T shop.</code>


;Ruleset 4:
This tests for correct order of scanning of rules, and may trap replacement routines that scan in the wrong order.   It implements a general unary multiplication engine.   (Note that the input expression must be placed within underscores in this implementation.)

```txt

### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ ->

```

Sample text of:
: <code> _1111*11111_ </code>
should generate the output:
: <code> 11111111111111111111 </code>


;Ruleset 5:
A simple [http://en.wikipedia.org/wiki/Turing_machine Turing machine],
implementing a three-state [http://en.wikipedia.org/wiki/Busy_beaver busy beaver].

The tape consists of '''0'''s and '''1'''s,   the states are '''A''', '''B''', '''C''' and '''H''' (for '''H'''alt), and the head position is indicated by writing the state letter before the character where the head is.
All parts of the initial tape the machine operates on have to be given in the input.

Besides demonstrating that the Markov algorithm is Turing-complete, it also made me catch a bug in the C++ implementation which wasn't caught by the first four rulesets.

```txt

# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11

```

This ruleset should turn
: <code> 000000A000000 </code>
into
: <code> 00011H1111000 </code>





## Ada

markov.ads:

```Ada
with Ada.Strings.Unbounded;

package Markov is
   use Ada.Strings.Unbounded;
   type Ruleset (Length : Natural) is private;
   type String_Array is array (Positive range <>) of Unbounded_String;
   function Parse (S : String_Array) return Ruleset;
   function Apply (R : Ruleset; S : String) return String;
private
   type Entry_Kind is (Comment, Rule);
   type Set_Entry (Kind : Entry_Kind := Rule) is record
      case Kind is
         when Rule =>
            Source         : Unbounded_String;
            Target         : Unbounded_String;
            Is_Terminating : Boolean;
         when Comment =>
            Text           : Unbounded_String;
      end case;
   end record;
   subtype Rule_Entry is Set_Entry (Kind => Rule);
   type Entry_Array is array (Positive range <>) of Set_Entry;
   type Ruleset (Length : Natural) is record
      Entries : Entry_Array (1 .. Length);
   end record;
end Markov;
```


markov.adb:

```Ada
package body Markov is

   function Parse (S : String_Array) return Ruleset is
      Result : Ruleset (Length => S'Length);
   begin
      for I in S'Range loop
         if Length (S (I)) = 0 or else Element (S (I), 1) = '#' then
            Result.Entries (I) := (Kind => Comment, Text => S (I));
         else
            declare
               Separator   : Natural;
               Terminating : Boolean;
               Target      : Unbounded_String;
            begin
               Separator := Index (S (I), " -> ");
               if Separator = 0 then
                  raise Constraint_Error;
               end if;
               Target      :=
                  Unbounded_Slice
                    (Source => S (I),
                     Low    => Separator + 4,
                     High   => Length (S (I)));
               Terminating := Length (Target) > 0
                             and then Element (Target, 1) = '.';
               if Terminating then
                  Delete (Source => Target, From => 1, Through => 1);
               end if;
               Result.Entries (I) :=
                 (Kind           => Rule,
                  Source         => Unbounded_Slice
                                      (Source => S (I),
                                       Low    => 1,
                                       High   => Separator - 1),
                  Target         => Target,
                  Is_Terminating => Terminating);
            end;
         end if;
      end loop;
      return Result;
   end Parse;

   procedure Apply
     (R        : Rule_Entry;
      S        : in out Unbounded_String;
      Modified : in out Boolean)
   is
      Pattern : String  := To_String (R.Source);
      Where   : Natural := Index (S, Pattern);
   begin
      while Where /= 0 loop
         Modified := True;
         Replace_Slice
           (Source => S,
            Low    => Where,
            High   => Where + Pattern'Length - 1,
            By     => To_String (R.Target));
         Where := Index (S, Pattern, Where + Length (R.Target));
      end loop;
   end Apply;

   function Apply (R : Ruleset; S : String) return String is
      Result       : Unbounded_String := To_Unbounded_String (S);
      Current_Rule : Set_Entry;
      Modified     : Boolean          := False;
   begin
      loop
         Modified := False;
         for I in R.Entries'Range loop
            Current_Rule := R.Entries (I);
            if Current_Rule.Kind = Rule then
               Apply (Current_Rule, Result, Modified);
               exit when Current_Rule.Is_Terminating or else Modified;
            end if;
         end loop;
         exit when not Modified;
      end loop;
      return To_String (Result);
   end Apply;

end Markov;
```


test_markov.adb:

```Ada
with Ada.Command_Line;
with Ada.Text_IO.Unbounded_IO;
with Ada.Strings.Unbounded;
with Markov;

procedure Test_Markov is
   use Ada.Strings.Unbounded;
   package IO renames Ada.Text_IO.Unbounded_IO;
   Rule_File  : Ada.Text_IO.File_Type;
   Line_Count : Natural := 0;
begin
   if Ada.Command_Line.Argument_Count /= 2 then
      Ada.Text_IO.Put_Line ("Usage: test_markov ruleset_file source_file");
      return;
   end if;
   Ada.Text_IO.Open
     (File => Rule_File,
      Mode => Ada.Text_IO.In_File,
      Name => Ada.Command_Line.Argument (1));
   while not Ada.Text_IO.End_Of_File (Rule_File) loop
      Ada.Text_IO.Skip_Line (Rule_File);
      Line_Count := Line_Count + 1;
   end loop;
   declare
      Lines : Markov.String_Array (1 .. Line_Count);
   begin
      Ada.Text_IO.Reset (Rule_File);
      for I in Lines'Range loop
         Lines (I) := IO.Get_Line (Rule_File);
      end loop;
      Ada.Text_IO.Close (Rule_File);

      declare
         Ruleset     : Markov.Ruleset := Markov.Parse (Lines);
         Source_File : Ada.Text_IO.File_Type;
      begin
         Ada.Text_IO.Open
           (File => Source_File,
            Mode => Ada.Text_IO.In_File,
            Name => Ada.Command_Line.Argument (2));
         while not Ada.Text_IO.End_Of_File (Source_File) loop
            Ada.Text_IO.Put_Line
              (Markov.Apply (Ruleset, Ada.Text_IO.Get_Line (Source_File)));
         end loop;
         Ada.Text_IO.Close (Source_File);
      end;
   end;
end Test_Markov;
```


Output (rulesX contains the ruleset of above examples and testX the example text):

```txt
$ ./test_markov rules1 test1
I bought a bag of apples from my brother.
$ ./test_markov rules2 test2
I bought a bag of apples from T shop.
$ ./test_markov rules3 test3
I bought a bag of apples with my money from T shop.
$ ./test_markov rules4 test4
11111111111111111111
$ ./test_markov rules5 test5
00011H1111000
```



## AutoHotkey


```autohotkey
;---------------------------------------------------------------------------
; Markov Algorithm.ahk
; by wolf_II
;---------------------------------------------------------------------------
; interpreter for a Markov Algorithm
;---------------------------------------------------------------------------



;---------------------------------------------------------------------------
AutoExecute: ; auto-execute section of the script
;---------------------------------------------------------------------------
    #SingleInstance, Force          ; only one instance allowed
    #NoEnv                          ; don't check empty variables
    StartupDir := A_WorkingDir      ; remember startup directory
    SetWorkingDir, %A_ScriptDir%    ; change directoy
    StringCaseSense, On             ; case sensitive comparisons
    ;-----------------------------------------------------------------------
    AppName := "Markov Algorithm"
    Gosub, GuiCreate
    Gui, Show,, %AppName%

Return



;---------------------------------------------------------------------------
GuiCreate: ; create the GUI
;---------------------------------------------------------------------------
    ; GUI options
    Gui, -MinimizeBox
    Gui, Add, Edit, y0 h0 ; catch the focus

    ; Ruleset
    Gui, Add, GroupBox, w445 h145 Section, Ruleset
    Gui, Add, Edit, xs+15 ys+20 w300 r8 vRuleset
    Gui, Add, Button, x+15 w100, Load Ruleset
    Gui, Add, Button, wp, Save Ruleset
    Gui, Add, Button, w30, 1
    Gui, Add, Button, x+5 wp, 2
    Gui, Add, Button, x+5 wp, 3
    Gui, Add, Button, xs+330 y+6 wp, 4
    Gui, Add, Button, x+5 wp, 5

    ; String
    Gui, Add, GroupBox, xs w445 h75 Section, String
    Gui, Add, Edit, xs+15 ys+20 w300 vString
    Gui, Add, Button, x+15 w100, Apply Ruleset
    Gui, Add, Button, xp wp Hidden, Stop
    Gui, Add, CheckBox, xs+15 yp+30 vSingleStepping, Single Stepping?

    ; Output
    Gui, Add, GroupBox, xs w445 h235 Section, Output
    Gui, Add, Edit, xs+15 ys+20 w415 r15 ReadOnly vOutput HwndhOut

Return



;---------------------------------------------------------------------------
GuiClose:
;---------------------------------------------------------------------------
    ExitApp

Return



;---------------------------------------------------------------------------
ButtonLoadRuleset: ; load ruleset from file
;---------------------------------------------------------------------------
    Gui, +OwnDialogs
    FileSelectFile, RulesetFile,,, Load Ruleset, *.markov
    If Not SubStr(RulesetFile, -6) = ".markov"
        RulesetFile .= ".markov"
    If FileExist(RulesetFile) {
        FileRead, Ruleset, %RulesetFile%
        GuiControl,, Ruleset, %Ruleset%
    } Else
        MsgBox, 16, Error - %AppName%, File not found:`n`n"%RulesetFile%"

Return



;---------------------------------------------------------------------------
ButtonSaveRuleset: ; save ruleset to file
;---------------------------------------------------------------------------
    Gui, +OwnDialogs
    Gui, Submit, NoHide
    FileSelectFile, RulesetFile, S16,, Save Ruleset, *.markov
    If Not SubStr(RulesetFile, -6) = ".markov"
        RulesetFile .= ".markov"
    FileDelete, %RulesetFile%
    FileAppend, %Ruleset%, %RulesetFile%
    Gui, Show

Return


_
;---------------------------------------------------------------------------
Button1: ; http://rosettacode.org/wiki/Execute_a_Markov_algorithm#Ruleset_1
;---------------------------------------------------------------------------
    GuiControl,, Output ; clear output
    GuiControl,, String, I bought a B of As from T S.
    GuiControl,, Ruleset,
    (LTrim
    # This rules file is extracted from Wikipedia:
    # http://en.wikipedia.org/wiki/Markov_Algorithm
    A -> apple
    B -> bag
    S -> shop
    T -> the
    the shop -> my brother
    a never used -> .terminating rule
    )

Return



;---------------------------------------------------------------------------
Button2: ; http://rosettacode.org/wiki/Execute_a_Markov_algorithm#Ruleset_2
;---------------------------------------------------------------------------
    GuiControl,, Output ; clear output
    GuiControl,, String, I bought a B of As from T S.
    GuiControl,, Ruleset,
    (LTrim
    # Slightly modified from the rules on Wikipedia
    A -> apple
    B -> bag
    S -> .shop
    T -> the
    the shop -> my brother
    a never used -> .terminating rule
    )

Return



;---------------------------------------------------------------------------
Button3: ; http://rosettacode.org/wiki/Execute_a_Markov_algorithm#Ruleset_3
;---------------------------------------------------------------------------
    GuiControl,, Output ; clear output
    GuiControl,, String, I bought a B of As W my Bgage from T S.
    GuiControl,, Ruleset,
    (LTrim
    # BNF Syntax testing rules
    A -> apple
    WWWW -> with
    Bgage -> ->.*
    B -> bag
    ->.* -> money
    W -> WW
    S -> .shop
    T -> the
    the shop -> my brother
    a never used -> .terminating rule
    )

Return



;---------------------------------------------------------------------------
Button4: ; http://rosettacode.org/wiki/Execute_a_Markov_algorithm#Ruleset_4
;---------------------------------------------------------------------------
    GuiControl,, Output ; clear output
    GuiControl,, String, _1111*11111_
    GuiControl,, Ruleset,
    (LTrim
    ### Unary Multiplication Engine, for testing Markov Algorithm implementations
    ### By Donal Fellows.
    # Unary addition engine
    _+1 -> _1+
    1+1 -> 11+
    # Pass for converting from the splitting of multiplication into ordinary
    # addition
    1! -> !1
    ,! -> !+
    _! -> _
    # Unary multiplication by duplicating left side, right side times
    1*1 -> x,@y
    1x -> xX
    X, -> 1,1
    X1 -> 1X
    _x -> _X
    ,x -> ,X
    y1 -> 1y
    y_ -> _
    # Next phase of applying
    1@1 -> x,@y
    1@_ -> @_
    ,@_ -> !_
    ++ -> +
    # Termination cleanup for addition
    _1 -> 1
    1+_ -> 1
    _+_ ->
    )

Return



;---------------------------------------------------------------------------
Button5: ; http://rosettacode.org/wiki/Execute_a_Markov_algorithm#Ruleset_5
;---------------------------------------------------------------------------
    GuiControl,, Output ; clear output
    GuiControl,, String, 000000A000000
    GuiControl,, Ruleset,
    (LTrim
    # Turing machine: three-state busy beaver
    #
    # state A, symbol 0 => write 1, move right, new state B
    A0 -> 1B
    # state A, symbol 1 => write 1, move left, new state C
    0A1 -> C01
    1A1 -> C11
    # state B, symbol 0 => write 1, move left, new state A
    0B0 -> A01
    1B0 -> A11
    # state B, symbol 1 => write 1, move right, new state B
    B1 -> 1B
    # state C, symbol 0 => write 1, move left, new state B
    0C0 -> B01
    1C0 -> B11
    # state C, symbol 1 => write 1, move left, halt
    0C1 -> H01
    1C1 -> H11
    )

Return



;---------------------------------------------------------------------------
ButtonApplyRuleset: ; flow control for Algorithm
;---------------------------------------------------------------------------
    ; prepare
    Gui, Submit, NoHide
    GuiControl,, Output ; clear
    Controls(False) ; disable
    Count := 0
    Subst := True
    Stop  := False

    ; keep substituting for as long as necessary
    While, Subst {
        Subst := False ; reset control variable
        IfEqual, Stop, 1, Break
        Gosub, Algorithm
    }

    ; clean up
    Output("Substitution count: " Count)
    Controls(True) ; re-enable

Return



;---------------------------------------------------------------------------
ButtonStop: ; this button is initially hidden
;---------------------------------------------------------------------------
    Stop := True

Return



;---------------------------------------------------------------------------
Algorithm: ; http://rosettacode.org/wiki/Execute_a_Markov_algorithm
;---------------------------------------------------------------------------
    ; Parse the ruleset and apply each rule to the string. Whenever a rule
    ; has changed the string goto first rule. Continue until a encountering
    ; a terminating rule, or until no further changes to the strings are
    ; made.
    ;-----------------------------------------------------------------------
    Loop, Parse, Ruleset, `n, `r ; always start from the beginning
    {
        ; check for comment
        If SubStr(A_LoopField, 1, 1) = "#"
            Continue ; get next line

        ; split a rule into $Search, $Terminator and $Replace
        LookFor := "(?P<Search>.+) -> (?P<Terminator>\.?)(?P<Replace>.+)"
        RegExMatch(A_LoopField, LookFor, $)

        ; single stepping through possible substitutions
        If SingleStepping
            MsgBox,, %AppName%, % ""
            . "Rule = """ A_LoopField """`n`n"
            . "Search`t= """ $Search """`n"
            . "Replace`t= """ $Replace """`n"
            . "Termintor`t= """ ($Terminator ? "True" : "False") """`n"

        ; try to substitute
        StringReplace, String, String, %$Search%, %$Replace%, UseErrorLevel

        ; any success?
        If ErrorLevel {     ; yes, substitution done
            Count++         ; keep count
            Subst := True   ; set control variable
            Output(String)  ; write new string to output
        }

        ; terminate?
        If $Terminator {    ; yes, terminate
            Stop := True    ; set control variable
            Break           ; back to flow control
        }

        ; we are not yet terminated ...
        If Subst            ; but we just did a substitution
            Break           ; back to flow control
    }

Return



;---------------------------------------------------------------------------
Controls(Bool) { ; [en|dis]able controls
;---------------------------------------------------------------------------
    Enable  := Bool ? "+" : "-"
    Disable := Bool ? "-" : "+"
    Loop, 2
        GuiControl, %Disable%ReadOnly, % "Edit" A_Index + 1
    Loop, 7
        GuiControl, %Disable%Disabled, % "Button" A_Index + 1
    GuiControl, %Disable%Disabled, Edit4
    GuiControl, %Disable%Hidden, Button10
    GuiControl, %Enable%Hidden, Button11
    GuiControl, %Disable%Disabled, Button12
}



;---------------------------------------------------------------------------
Output(Text) { ; append text to output
;---------------------------------------------------------------------------
    static EM_REPLACESEL = 0xC2
    global hOut
    Sleep, 100
    Text .= "`r`n"
    SendMessage, EM_REPLACESEL,, &Text,, ahk_id %hOut%
}



;---------- end of file ----------------------------------------------------
```



## BBC BASIC


```bbcbasic
      PRINT FNmarkov("ruleset1.txt", "I bought a B of As from T S.")
      PRINT FNmarkov("ruleset2.txt", "I bought a B of As from T S.")
      PRINT FNmarkov("ruleset3.txt", "I bought a B of As W my Bgage from T S.")
      PRINT FNmarkov("ruleset4.txt", "_1111*11111_")
      PRINT FNmarkov("ruleset5.txt", "000000A000000")
      END

      DEF FNmarkov(rulefile$, text$)
      LOCAL i%, done%, rules%, rule$, old$, new$
      rules% = OPENIN(rulefile$)
      IF rules%=0 ERROR 100, "Cannot open rules file"
      REPEAT
        rule$ = GET$#rules%
        IF ASC(rule$)<>35 THEN
          REPEAT
            i% = INSTR(rule$, CHR$(9))
            IF i% MID$(rule$,i%,1) = " "
          UNTIL i%=0
          i% = INSTR(rule$, " -> ")
          IF i% THEN
            old$ = LEFT$(rule$,i%-1)
            WHILE RIGHT$(old$)=" " old$ = LEFT$(old$) : ENDWHILE
            new$ = MID$(rule$,i%+4)
            WHILE ASC(new$)=32 new$ = MID$(new$,2) : ENDWHILE
            IF ASC(new$)=46 new$ = MID$(new$,2) : done% = TRUE
            i% = INSTR(text$,old$)
            IF i% THEN
              text$ = LEFT$(text$,i%-1) + new$ + MID$(text$,i%+LEN(old$))
              PTR#rules% = 0
            ENDIF
          ENDIF
        ENDIF
      UNTIL EOF#rules% OR done%
      CLOSE #rules%
      = text$
```

'''Output:'''

```txt

I bought a bag of apples from my brother.
I bought a bag of apples from T shop.
I bought a bag of apples with my money from T shop.
11111111111111111111
00011H1111000

```



## Bracmat

Save the following text to a file "markov.bra":

```bracmat

markov=
{
First the patterns that describe the rules syntax.
This is a naive and not very efficient way to parse the rules, but it closely
matches the problem description, which is nice.
}
    ( ruleset
    =     >%@" " ?   { Added: assume that a rule cannot start with whitespace.
                       The %@ say that the thing to match must be exactly one
                       byte. % means 'one or more'. @ means 'zero or one'.
                     }
        : ((!comment|!rule) !newlines) !ruleset
      |         { Recursion terminates here: match empty string. }
    )
  & (comment="#" ?com)
  & ( rule
    =   %?pattern
        !whitespace
        "->"
        !whitespace
        ( "." %?replacement&stop:?stop
        | %?replacement
        )
    )
  & ( whitespace
    = (\t|" ") (!whitespace|)
    )
  & ( newlines
    =   ( (\n|\r)
        & ( :!pattern:!replacement {Do nothing. We matched an empty line.}
          |   (!pattern.!replacement.!stop) !rules:?rules
                {
                Add pattern, replacement and the stop (empty string or "stop")
                to a list of triplets. This list will contain the rules in
                reverse order.
                Then, reset these variables, so they are not added once more
                if an empty line follows.
                }
            & :?stop:?pattern:?replacement
          )
        )
        (!newlines|)
    )
{
Compile the textual rules to a single Bracmat pattern.
}
  & ( compileRules
    =   stop pattern replacement rules,pat rep stp
      .   :?stop:?pattern:?replacement:?rules
                {
                Important! Initialise these variables.
                }
        & @(!arg:!ruleset)
                {
                That's all. The textual rules are parsed and converted to a
                list of triplets. The rules in the list are in reversed order.
                }
        & !rules:(?pat.?rep.?stp) ?rules
                {
                The head of the list is the last rule. Use it to initialise
                the pattern "ruleSetAsPattern".
                The single quote introduces a macro substition. All symbols
                preceded with a $ are substituted.
                }
        &
            ' ( ?A ()$pat ?Z
              & $stp:?stop
              & $rep:?replacement
              )
          : (=?ruleSetAsPattern)
                {
                Add all remaining rules as new subpatterns to
                "ruleSetAsPattern". Separate with the OR symbol.
                }
        &   whl
          ' ( !rules:(?pat.?rep.?stp) ?rules
            &
                ' (   ?A ()$pat ?Z
                    & $stp:?stop
                    & $rep:?replacement
                  | $ruleSetAsPattern
                  )
              : (=?ruleSetAsPattern)
            )
        & '$ruleSetAsPattern
    )
        {
        Function that takes two arguments: a rule set (as text)
        and a subject string.
        The function returns the transformed string.
        }
  & ( applyRules
    =     rulesSetAsText subject ruleSetAsPattern
        , A Z replacement stop
      .   !arg:(?rulesSetAsText.?subject)
        & compileRules$!rulesSetAsText:(=?ruleSetAsPattern)
                {
                Apply rule until no match
                or until variable "stop" has been set to the value "stop".
                }
        &   whl
          ' ( @(!subject:!ruleSetAsPattern)
            & str$(!A !replacement !Z):?subject
            & !stop:~stop
            )
        & !subject
    )
{
Tests:
}
  &   out
    $ ( applyRules
      $ ( "# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule
"
        . "I bought a B of As from T S."
        )
      )
  &   out
    $ ( applyRules
      $ ( "# Slightly modified from the rules on Wikipedia
A -> apple
B -> bag
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
"
        . "I bought a B of As from T S."
        )
      )
  &   out
    $ ( applyRules
      $ ( "# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
"
        . "I bought a B of As W my Bgage from T S."
        )
      )
  &   out
    $ ( applyRules
      $ ( "### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ ->
"
        . "_1111*11111_"
        )
      )
  &   out
    $ ( applyRules
      $ ( "# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11
"
        . 000000A000000
        )
      )
  & ok
| failure;

```


Test:


```txt

{?} get$"markov.bra"
{!} markov
    S   0,01 sec
{?} !markov
I bought a bag of apples from my brother.
I bought a bag of apples from T shop.
I bought a bag of apples with my money from T shop.
11111111111111111111
00011H1111000
{!} ok
    S   0,41 sec

```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <ctype.h>

typedef struct { char * s; size_t alloc_len; } string;

typedef struct {
	char *pat, *repl;
	int terminate;
} rule_t;

typedef struct {
	int n;
	rule_t *rules;
	char *buf;
} ruleset_t;

void ruleset_del(ruleset_t *r)
{
	if (r->rules) free(r->rules);
	if (r->buf) free(r->buf);
	free(r);
}

string * str_new(const char *s)
{
	int l = strlen(s);
	string *str = malloc(sizeof(string));
	str->s = malloc(l + 1);
	strcpy(str->s, s);
	str->alloc_len = l + 1;
	return str;
}

void str_append(string *str, const char *s, int len)
{
	int l = strlen(str->s);
	if (len == -1) len = strlen(s);

	if (str->alloc_len < l + len + 1) {
		str->alloc_len = l + len + 1;
		str->s = realloc(str->s, str->alloc_len);
	}
	memcpy(str->s + l, s, len);
	str->s[l + len] = '\0';
}

/* swap content of dest and src, and truncate src string */
void str_transfer(string *dest, string *src)
{
	size_t tlen = dest->alloc_len;
	dest->alloc_len = src->alloc_len;
	src->alloc_len = tlen;

	char *ts = dest->s;
	dest->s = src->s;
	src->s = ts;
	src->s[0] = '\0';
}

void str_del(string *s)
{
	if (s->s) free(s->s);
	free(s);
}

void str_markov(string *str, ruleset_t *r)
{
	int i, j, sl, pl;
	int changed = 0, done = 0;
	string *tmp = str_new("");

	while (!done) {
		changed = 0;
		for (i = 0; !done && !changed && i < r->n; i++) {
			pl = strlen(r->rules[i].pat);
			sl = strlen(str->s);
			for (j = 0; j < sl; j++) {
				if (strncmp(str->s + j, r->rules[i].pat, pl))
					continue;
				str_append(tmp, str->s, j);
				str_append(tmp, r->rules[i].repl, -1);
				str_append(tmp, str->s + j + pl, -1);

				str_transfer(str, tmp);
				changed = 1;

				if (r->rules[i].terminate)
					done = 1;
				break;
			}
		}
		if (!changed) break;
	}
	str_del(tmp);
	return;
}

ruleset_t* read_rules(const char *name)
{
	struct stat s;
	char *buf;
	size_t i, j, k, tmp;
	rule_t *rules = 0;
	int n = 0; /* number of rules */

	int fd = open(name, O_RDONLY);
	if (fd == -1) return 0;

	fstat(fd, &s);
	buf = malloc(s.st_size + 2);
	read(fd, buf, s.st_size);
	buf[s.st_size] = '\n';
	buf[s.st_size + 1] = '\0';
	close(fd);

	for (i = j = 0; buf[i] != '\0'; i++) {
		if (buf[i] != '\n') continue;

		/* skip comments */
		if (buf[j] == '#' || i == j) {
			j = i + 1;
			continue;
		}

		/* find the '->' */
		for (k = j + 1; k < i - 3; k++)
			if (isspace(buf[k]) && !strncmp(buf + k + 1, "->", 2))
				break;

		if (k >= i - 3) {
			printf("parse error: no -> in %.*s\n", i - j, buf + j);
			break;
		}

		/* left side: backtrack through whitespaces */
		for (tmp = k; tmp > j && isspace(buf[--tmp]); );
		if (tmp < j) {
			printf("left side blank? %.*s\n", i - j, buf + j);
			break;
		}
		buf[++tmp] = '\0';

		/* right side */
		for (k += 3; k < i && isspace(buf[++k]););
		buf[i] = '\0';

		rules = realloc(rules, sizeof(rule_t) * (1 + n));
		rules[n].pat = buf + j;

		if (buf[k] == '.') {
			rules[n].terminate = 1;
			rules[n].repl = buf + k + 1;
		} else {
			rules[n].terminate = 0;
			rules[n].repl = buf + k;
		}
		n++;

		j = i + 1;
	}

	ruleset_t *r = malloc(sizeof(ruleset_t));
	r->buf = buf;
	r->rules = rules;
	r->n = n;
	return r;
}

int test_rules(const char *s, const char *file)
{
	ruleset_t * r = read_rules(file);
	if (!r) return 0;
	printf("Rules from '%s' ok\n", file);

	string *ss = str_new(s);
	printf("text:     %s\n", ss->s);

	str_markov(ss, r);
	printf("markoved: %s\n", ss->s);

	str_del(ss);
	ruleset_del(r);

	return printf("\n");
}

int main()
{
	/* rule 1-5 are files containing rules from page top */
	test_rules("I bought a B of As from T S.", "rule1");
	test_rules("I bought a B of As from T S.", "rule2");
	test_rules("I bought a B of As W my Bgage from T S.", "rule3");
	test_rules("_1111*11111_", "rule4");
	test_rules("000000A000000", "rule5");

	return 0;
}
```
output<lang>Rules from 'rule1' ok
text:     I bought a B of As from T S.
markoved: I bought a bag of apples from my brother.

Rules from 'rule2' ok
text:     I bought a B of As from T S.
markoved: I bought a bag of apples from T shop.

Rules from 'rule3' ok
text:     I bought a B of As W my Bgage from T S.
markoved: I bought a bag of apples with my money from T shop.

Rules from 'rule4' ok
text:     _1111*11111_
markoved: 11111111111111111111

Rules from 'rule5' ok
text:     000000A000000
markoved: 00011H1111000

```



## C++

Note: Non-use of <code>iswhite</code> is intentional, since depending on the locale, other chars besides space and tab might be detected by that function.

```cpp

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <vector>
#include <string>

struct rule
{
  std::string pattern;
  std::string replacement;
  bool terminal;
  rule(std::string pat, std::string rep, bool term):
    pattern(pat),
    replacement(rep),
    terminal(term)
  {
  }
};

std::string const whitespace = " \t";
std::string::size_type const npos = std::string::npos;

bool is_whitespace(char c)
{
  return whitespace.find(c) != npos;
}

std::vector<rule> read_rules(std::ifstream& rulefile)
{
  std::vector<rule> rules;
  std::string line;
  while (std::getline(rulefile, line))
  {
    std::string::size_type pos;

    // remove comments
    pos = line.find('#');
    if (pos != npos)
      line.resize(pos);

    // ignore lines consisting only of whitespace
    if (line.find_first_not_of(whitespace) == npos)
      continue;

    // find "->" surrounded by whitespace
    pos = line.find("->");
    while (pos != npos && (pos == 0 || !is_whitespace(line[pos-1])))
      pos = line.find("->", pos+1);

    if (pos == npos || line.length() < pos+3 || !is_whitespace(line[pos+2]))
    {
      std::cerr << "invalid rule: " << line << "\n";
      std::exit(EXIT_FAILURE);
    }

    std::string pattern = line.substr(0, pos-1);
    std::string replacement = line.substr(pos+3);

    // remove additional separating whitespace
    pattern.erase(pattern.find_last_not_of(whitespace)+1);
    replacement.erase(0, replacement.find_first_not_of(whitespace));

    // test for terminal rule
    bool terminal = !replacement.empty() && replacement[0] == '.';
    if (terminal)
      replacement.erase(0,1);

    rules.push_back(rule(pattern, replacement, terminal));
  }

  return rules;
}

std::string markov(std::vector<rule> rules, std::string input)
{
  std::string& output = input;
  std::vector<rule>::iterator iter = rules.begin();

  // Loop through each rule, transforming our current version
  // with each rule.
  while (iter != rules.end())
  {
    std::string::size_type pos = output.find(iter->pattern);
    if (pos != npos)
    {
      output.replace(pos, iter->pattern.length(), iter->replacement);
      if (iter->terminal)
        break;
      iter = rules.begin();
    }
    else
      ++iter;
  }

  return output;
}

int main(int argc, char* argv[])
{
  if (argc != 3)
  {
    std::cout << "usage:\n " << argv[0] << " rulefile text\n";
    return EXIT_FAILURE;
  }

  std::ifstream rulefile(argv[1]);
  std::vector<rule> rules = read_rules(rulefile);

  std::string input(argv[2]);
  std::string output = markov(rules, input);

  std::cout << output << "\n";
}
```



## Common Lisp

I should mention that this uses the regular expression machinery present in Allegro Lisp but not Common Lisp generally (though there are public domain Lisp libraries).

```lisp
;;; Keeps track of all our rules
(defclass markov ()
  ((rules :initarg :rules :initform nil :accessor rules)))

;;; Definition of a rule
(defclass rule ()
  ((pattern :initarg :pattern :accessor pattern)
   (replacement :initarg :replacement :accessor replacement)
   (terminal :initform nil :initarg :terminal :accessor terminal)))

;;; Parse a rule with this regular expression
(defparameter *rex->* (compile-re "^(.+)(?: |\\t)->(?: |\\t)(\\.?)(.*)$"))

;;; Create a rule and add it to the markov object
(defmethod update-markov ((mkv markov) lhs terminating rhs)
  (setf (rules mkv) (cons
                     (make-instance 'rule :pattern lhs :replacement rhs :terminal terminating)
                     (rules mkv))))

;;; Parse a line and add it to the markov object
(defmethod parse-line ((mkv markov) line)
  (let ((trimmed (string-trim #(#\Space #\Tab) line)))
    (if (not (or
              (eql #\# (aref trimmed 0))
              (equal "" trimmed)))
        (let ((vals (multiple-value-list (match-re *rex->* line))))
          (if (not (car vals))
              (progn
                (format t "syntax error in ~A" line)
                (throw 'fail t)))
          (update-markov mkv (nth 2 vals) (equal "." (nth 3 vals)) (nth 4 vals))))))

;;; Make a markov object from the string of rules
(defun make-markov (rules-text)
  (catch 'fail
         (let ((mkv (make-instance 'markov)))
           (with-input-from-string (s rules-text)
             (loop for line = (read-line s nil)
                 while line do
                   (parse-line mkv line)))
           (setf (rules mkv) (reverse (rules mkv)))
           mkv)))

;;; Given a rule and bounds where it applies, apply it to the input text
(defun adjust (rule-info text)
  (let* ((rule (car rule-info))
         (index-start (cadr rule-info))
         (index-end (caddr rule-info))
         (prefix (subseq text 0 index-start))
         (suffix (subseq text index-end))
         (replace (replacement rule)))
    (concatenate 'string prefix replace suffix)))

;;; Get the next applicable rule or nil if none
(defmethod get-rule ((markov markov) text)
  (dolist (rule (rules markov) nil)
    (let ((index (search (pattern rule) text)))
      (if index
          (return (list rule index (+ index (length (pattern rule)))))))))

;;; Interpret text using a markov object
(defmethod interpret ((markov markov) text)
  (let ((rule-info (get-rule markov text))
        (ret text))
    (loop (if (not rule-info) (return ret))
          (setf ret (adjust rule-info ret))
          (if (terminal (car rule-info)) (return ret))
          (setf rule-info (get-rule markov ret)))))
```

Testing:
<lang>(defparameter
    *rules1*
"# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule")

;;; Lots of other defparameters for rules omitted here...

(defun test ()
  (format t "~A~%" (interpret (make-markov *rules1*) "I bought a B of As from T S."))
  (format t "~A~%" (interpret (make-markov *rules2*) "I bought a B of As from T S."))
  (format t "~A~%" (interpret (make-markov *rules3*) "I bought a B of As W my Bgage from T S."))
  (format t "~A~%" (interpret (make-markov *rules4*) "_1111*11111_"))
  (format t "~A~%" (interpret (make-markov *rules5*) "000000A000000"))
  )
(test)
I bought a bag of apples from my brother.
I bought a bag of apples from T shop.
I bought a bag of apples with my money from T shop.
11111111111111111111
00011H1111000
NIL

```



## D

```d
void main() {
    import std.stdio, std.file, std.regex, std.string, std.range,
           std.functional;

    const rules = "markov_rules.txt".readText.splitLines.split("");
    auto tests = "markov_tests.txt".readText.splitLines;
    auto re = ctRegex!(r"^([^#]*?)\s+->\s+(\.?)(.*)"); // 160 MB RAM.

    alias slZip = curry!(zip, StoppingPolicy.requireSameLength);
    foreach (test, const rule; slZip(tests, rules)) {
        const origTest = test.dup;

        string[][] capt;
        foreach (const line; rule) {
            auto m = line.match(re);
            if (!m.empty) {
                //capt.put(m.captures.dropOne);
                capt ~= m.captures.dropOne.array;
            }
        }

    REDO:
        const copy = test;
        foreach (const c; capt) {
            test = test.replace(c[0], c[2]);
            if (c[1] == ".")
                break;
            if (test != copy)
                goto REDO;
        }
        writefln("%s\n%s\n", origTest, test);
    }
}
```

```txt
I bought a B of As from T S.
I bought a bag of apples from my brother.

I bought a B of As from T S.
I bought a bag of apples from T shop.

I bought a B of As W my Bgage from T S.
I bought a bag of apples with my money from T shop.

_1111*11111_
11111111111111111111

000000A000000
00011H1111000
```


=={{header|Déjà Vu}}==
This implementation expect the initial text on the command line and the ruleset on STDIN.

```dejavu
(remove-comments) text:
	]
	for line in text:
		if and line not starts-with line "#":
			line
	[

(markov-parse) text:
	]
	for line in text:
		local :index find line " -> "
		local :pat slice line 0 index
		local :rep slice line + index 4 len line
		local :term starts-with rep "."
		if term:
			set :rep slice rep 1 len rep
		& pat & term rep
	[

markov-parse:
	(markov-parse) (remove-comments) split !decode!utf-8 !read!stdin "\n"

(markov-tick) rules start:
	for rule in copy rules:
		local :pat &< rule
		local :rep &> dup &> rule
		local :term &<
		local :index find start pat
		if < -1 index:
			)
			slice start + index len pat len start
			rep
			slice start 0 index
			concat(
			return term
	true start

markov rules:
	true
	while:
		not (markov-tick) rules

!. markov markov-parse get-from !args 1
```



## EchoLisp


```scheme

;; rule := (pattern replacement [#t terminal])

(define-syntax-rule  (pattern rule) (first rule))
(define-syntax-rule  (repl sule) (second rule))
(define-syntax-rule  (term? rule) (!empty? (cddr rule)))

;; (alpha   .beta )--> (alpha beta #t)
(define (term-rule rule)
        (if (string=? (string-first (repl rule)) ".")
            (list (pattern rule) (string-rest (repl rule)) #t)
            rule ))

;; returns list of rules
(define (parse-rules lines)
    (map term-rule
    (for/list [(line (string-split lines "\n"))]
        #:continue (string=? (string-first line) "#")
        (map string-trim
        (string-split (string-replace line "/\\t/g" " ") " -> ")))))

;; markov machine
(define (markov i-string rules)
    (while
        (for/fold (run #f) ((rule rules))
        #:when (string-index (pattern rule) i-string)
          (set! i-string (string-replace i-string (pattern rule) (repl rule)))
            ;;(writeln rule i-string) ;; uncomment for trace
            #:break (term? rule)  => #f
            #:break #t  => #t ))
    i-string)

(define (task i-string  RS)
    (markov i-string (parse-rules RS)))

```

```txt

(define RS1 #<<
# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
C -> chinchard
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule
>>#)

;; [ Other rules sets here ...]

(define i-string-1 "I bought a B of As and Cs from T S.")
(define i-string-2 "I bought a B of As from T S.")
(define i-string-3 "I bought a B of As W my Bgage from T S.")
(define i-string-4 "_1111*11111_")
(define i-string-5 "000000A000000")

(task i-string-1 RS1)
    → "I bought a bag of apples and chinchards from my brother."
(task  i-string-2 RS2)
    → "I bought a bag of apples from T shop."
(task  i-string-3 RS3)
    → "I bought a bag of apples with my money from T shop."
(task  i-string-4 RS4)
    → "11111111111111111111"
(task  i-string-5 RS5)
    → "00011H1111000"

```


=={{header|F_Sharp|F#}}==
<p>Using Partial Active Pattern to simplify pattern matching.</p>

```fsharp
open System
open System.IO
open System.Text.RegularExpressions

type Rule = {
    matches : Regex
    replacement : string
    terminate : bool
}

let (|RegexMatch|_|) regexStr input =
    let m = Regex.Match(input, regexStr, RegexOptions.ExplicitCapture)
    if m.Success then Some (m) else None

let (|RuleReplace|_|) rule input =
    let replaced = rule.matches.Replace(input, rule.replacement, 1, 0)
    if input = replaced then None
    else Some (replaced, rule.terminate)

let parseRules line =
    match line with
    | RegexMatch "^#" _ -> None
    | RegexMatch "(?<pattern>.*?)\s+->\s+(?<replacement>.*)$" m ->
        let replacement = (m.Groups.Item "replacement").Value
        let terminate = replacement.Length > 0 && replacement.Substring(0,1) = "."
        let pattern = (m.Groups.Item "pattern").Value
        Some {
            matches = pattern |> Regex.Escape |> Regex;
            replacement = if terminate then replacement.Substring(1) else replacement;
            terminate = terminate
        }
    | _ -> failwith "illegal rule definition"

let rec applyRules input = function
| [] -> (input, true)
| rule::rules ->
    match input with
    | RuleReplace rule (withReplacement, terminate) ->
        (withReplacement, terminate)
    | _ -> applyRules input rules

[<EntryPoint>]
let main argv =
    let rules = File.ReadAllLines argv.[0] |> Array.toList |> List.choose parseRules
    let rec run input =
        let output, terminate = applyRules input rules
        if terminate then output
        else run output

    Console.ReadLine()
    |> run
    |> printfn "%s"
    0
```

```txt
H:\RosettaCode\ExecMarkovAlgo>echo I bought a B of As from T S. | Fsharp\RosettaCode\bin\Debug\RosettaCode.exe m1
I bought a bag of apples from my brother.

H:\RosettaCode\ExecMarkovAlgo>echo I bought a B of As from T S.| Fsharp\RosettaCode\bin\Debug\RosettaCode.exe m2
I bought a bag of apples from T shop.

H:\RosettaCode\ExecMarkovAlgo>echo I bought a B of As W my Bgage from T S.| Fsharp\RosettaCode\bin\Debug\RosettaCode.exe m3
I bought a bag of apples with my money from T shop.

H:\RosettaCode\ExecMarkovAlgo>echo _1111*11111_ | Fsharp\RosettaCode\bin\Debug\RosettaCode.exe m4
11111111111111111111

H:\RosettaCode\ExecMarkovAlgo>echo 000000A000000 | Fsharp\RosettaCode\bin\Debug\RosettaCode.exe m5
00011H1111000
```



## Go


```go
package main

import (
    "fmt"
    "regexp"
    "strings"
)

type testCase struct {
    ruleSet, sample, output string
}

func main() {
    fmt.Println("validating", len(testSet), "test cases")
    var failures bool
    for i, tc := range testSet {
        if r, ok := interpret(tc.ruleSet, tc.sample); !ok {
            fmt.Println("test", i+1, "invalid ruleset")
            failures = true
        } else if r != tc.output {
            fmt.Printf("test %d: got %q, want %q\n", i+1, r, tc.output)
            failures = true
        }
    }
    if !failures {
        fmt.Println("no failures")
    }
}

func interpret(ruleset, input string) (string, bool) {
    if rules, ok := parse(ruleset); ok {
        return run(rules, input), true
    }
    return "", false
}

type rule struct {
    pat  string
    rep  string
    term bool
}

var (
    rxSet   = regexp.MustCompile(ruleSet)
    rxEle   = regexp.MustCompile(ruleEle)
    ruleSet = `(?m:^(?:` + ruleEle + `)*$)`
    ruleEle = `(?:` + comment + `|` + ruleRe + `)\n+`
    comment = `#.*`
    ruleRe  = `(.*)` + ws + `->` + ws + `([.])?(.*)`
    ws      = `[\t ]+`
)

func parse(rs string) ([]rule, bool) {
    if !rxSet.MatchString(rs) {
        return nil, false
    }
    x := rxEle.FindAllStringSubmatchIndex(rs, -1)
    var rules []rule
    for _, x := range x {
        if x[2] > 0 {
            rules = append(rules,
                rule{pat: rs[x[2]:x[3]], term: x[4] > 0, rep: rs[x[6]:x[7]]})
        }
    }
    return rules, true
}

func run(rules []rule, s string) string {
step1:
    for _, r := range rules {
        if f := strings.Index(s, r.pat); f >= 0 {
            s = s[:f] + r.rep + s[f+len(r.pat):]
            if r.term {
                return s
            }
            goto step1
        }
    }
    return s
}

// text all cut and paste from RC task page
var testSet = []testCase{
    {`# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule
`,
        `I bought a B of As from T S.`,
        `I bought a bag of apples from my brother.`,
    },
    {`# Slightly modified from the rules on Wikipedia
A -> apple
B -> bag
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
`,
        `I bought a B of As from T S.`,
        `I bought a bag of apples from T shop.`,
    },
    {`# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
`,
        `I bought a B of As W my Bgage from T S.`,
        `I bought a bag of apples with my money from T shop.`,
    },
    {`### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ ->
`,
        `_1111*11111_`,
        `11111111111111111111`,
    },
    {`# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11
`,
        `000000A000000`,
        `00011H1111000`,
    },
}
```

```txt

validating 5 test cases
no failures

```



## Groovy


```groovy
def markovInterpreterFor = { rules ->
    def ruleMap = [:]
    rules.eachLine { line ->
        (line =~ /\s*(.+)\s->\s([.]?)(.+)\s*/).each { text, key, terminating, value ->
            if (key.startsWith('#')) { return }
            ruleMap[key] = [text: value, terminating: terminating]
        }
    }
    [interpret: { text ->
        def originalText = ''
        while (originalText != text) {
            originalText = text
            for (Map.Entry e : ruleMap.entrySet()) {
                if (text.indexOf(e.key) >= 0) {
                    text = text.replace(e.key, e.value.text)
                    if (e.value.terminating) {
                        return text
                    }
                    break
                }
            }
        }
        text
    }]
}
```

The test code is below (with the markov rulesets 2..5 elided):

```groovy
def verify = { ruleset ->
    [withInput: { text ->
        [hasOutput: { expected ->
            def result = ruleset.interpret(text)
            println "Input: '$text' has output: '$result'"
            assert expected == result
        }]
    }]
}

def ruleset1 = markovInterpreterFor("""
# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule""")
println ruleset1.interpret('I bought a B of As from T S.')
verify ruleset1 withInput 'I bought a bag of apples from T shop.' hasOutput 'I bought a bag of apples from my brother.'

def ruleset2 = markovInterpreterFor("""...""")
verify ruleset2 withInput 'I bought a B of As from T S.' hasOutput 'I bought a bag of apples from T shop.'

def ruleset3 = markovInterpreterFor("""...""")
verify ruleset3 withInput 'I bought a B of As W my Bgage from T S.' hasOutput 'I bought a bag of apples with my money from T shop.'

def ruleset4 = markovInterpreterFor("""...""")
verify ruleset4 withInput '_1111*11111_' hasOutput '11111111111111111111'

def ruleset5 = markovInterpreterFor("""...""")
verify ruleset5 withInput '000000A000000' hasOutput '00011H1111000'
```

```txt

I bought a bag of apples from my brother.
Input: 'I bought a bag of apples from T shop.' has output: 'I bought a bag of apples from my brother.'
Input: 'I bought a B of As from T S.' has output: 'I bought a bag of apples from T shop.'
Input: 'I bought a B of As W my Bgage from T S.' has output: 'I bought a bag of apples with my money from T shop.'
Input: '_1111*11111_' has output: '11111111111111111111'
Input: '000000A000000' has output: '00011H1111000'

```





## Haskell

This program expects a source file as an argument and uses the standard input and output devices for the algorithm's I/O.


```haskell
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes)
import Control.Monad
import Text.ParserCombinators.Parsec
import System.IO
import System.Environment (getArgs)

main = do
   args <- getArgs
   unless (length args == 1) $
       fail "Please provide exactly one source file as an argument."
   let sourcePath = head args
   source <- readFile sourcePath
   input <- getContents
   case parse markovParser sourcePath source of
       Right rules -> putStr $ runMarkov rules input
       Left  err   -> hPutStrLn stderr $ "Parse error at " ++ show err

data Rule = Rule
   {from :: String, terminating :: Bool, to :: String}

markovParser :: Parser [Rule]
markovParser = liftM catMaybes $
    (comment <|> rule) `sepEndBy` many1 newline
  where comment = char '#' >> skipMany nonnl >> return Nothing
        rule = liftM Just $ liftM3 Rule
            (manyTill (nonnl <?> "pattern character") $ try arrow)
            (succeeds $ char '.')
            (many nonnl)
        arrow = ws >> string "->" >> ws <?> "whitespace-delimited arrow"
        nonnl = noneOf "\n"
        ws = many1 $ oneOf " \t"
        succeeds p = option False $ p >> return True

runMarkov :: [Rule] -> String -> String
runMarkov rules s = f rules s
  where f []                              s = s
        f (Rule from terminating to : rs) s = g "" s
          where g _      ""    = f rs s
                g before ahead@(a : as) = if from `isPrefixOf` ahead
                  then let new = reverse before ++ to ++ drop (length from) ahead
                       in if terminating then new else f rules new
                  else g (a : before) as
```


=={{header|Icon}} and {{header|Unicon}}==


```unicon
procedure main(A)
    rules := loadRules(open(A[1],"r"))
    every write(line := !&input, " -> ",apply(rules, line))
end

record rule(pat, term, rep)

procedure loadRules(f)
    rules := []
    every !f ? if not ="#" then put(rules,
                    rule(1(trim(tab(find("->"))),move(2),tab(many(' \t'))),
                         (="."|&null), trim(tab(0))))
    return rules
end

procedure apply(rules, line)
    s := line
    repeat {
       s ?:= tab(find((r := !rules).pat)) || r.rep || (move(*r.pat),tab(0))
       if (s == line) | \r.term then return s else line := s
       }
end
```


Sample runs using above rule sets and test strings:

```txt

->ma mars.1
I bought a B of As from T S.
I bought a B of As from T S. -> I bought a bag of apples from my brother.
->ma mars.2
I bought a B of As from T S.
I bought a B of As from T S. -> I bought a bag of apples from T shop.
->ma mars.3
I bought a B of As W my Bgage from T S.
I bought a B of As W my Bgage from T S. -> I bought a bag of apples with my money from T shop.
->ma mars.4
_1111*11111_
_1111*11111_ -> 11111111111111111111
->ma mars.5
000000A000000
000000A000000 -> 00011H1111000

```



## J

'''Solution''':
```j
require'strings regex'

markovLexer =:  verb define
  rules =.  LF cut TAB&=`(,:&' ')}y
  rules =.  a: -.~ (dltb@:{.~ i:&'#')&.> rules
  rules =.  0 _1 {"1 '\s+->\s+' (rxmatch rxcut ])S:0 rules
  (,. ] (}.&.>~ ,. ]) ('.'={.)&.>)/ |: rules
)


replace     =:  dyad define
  'index patternLength replacement'=.  x
  'head tail' =.  index split y
  head, replacement, patternLength }. tail
)

matches     =:  E. i. 1:

markov      =:  dyad define
  ruleIdx =. 0 [ rules =.  markovLexer x
  while. ruleIdx < #rules do.
    'pattern replacement terminating' =. ruleIdx { rules
    ruleIdx =. 1 + ruleIdx
    if. (#y) > index =. pattern matches y do.
      y =. (index ; (#pattern) ; replacement) replace y
      ruleIdx =. _ * terminating
    end.
  end.
  y
)
```


'''Example''':
```j
   m1 =. noun define
	# This rules file is extracted from Wikipedia:
	# http://en.wikipedia.org/wiki/Markov_Algorithm
	A -> apple
	B -> bag
	S -> shop
	T -> the
	the shop -> my brother
	a never used -> .terminating rule
)

   m1 markov 'I bought a B of As from T S.'
I bought a bag of apples from my brother.

```

'''Discussion''': The J implementation correctly processes all the rulesets.  More details are available on the [[Talk:Markov Algorithm#explicit_vs_tacit|the talk page]].


## Java

```java
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Markov {

    public static void main(String[] args) throws IOException {

        List<String[]> rules = readRules("markov_rules.txt");
        List<String> tests = readTests("markov_tests.txt");

        Pattern pattern = Pattern.compile("^([^#]*?)\\s+->\\s+(\\.?)(.*)");

        for (int i = 0; i < tests.size(); i++) {
            String origTest = tests.get(i);

            List<String[]> captures = new ArrayList<>();
            for (String rule : rules.get(i)) {
                Matcher m = pattern.matcher(rule);
                if (m.find()) {
                    String[] groups = new String[m.groupCount()];
                    for (int j = 0; j < groups.length; j++)
                        groups[j] = m.group(j + 1);
                    captures.add(groups);
                }
            }

            String test = origTest;
            String copy = test;
            for (int j = 0; j < captures.size(); j++) {
                String[] c = captures.get(j);
                test = test.replace(c[0], c[2]);
                if (c[1].equals("."))
                    break;
                if (!test.equals(copy)) {
                    j = -1; // redo loop
                    copy = test;
                }
            }
            System.out.printf("%s\n%s\n\n", origTest, test);
        }
    }

    private static List<String> readTests(String path)
            throws IOException {
        return Files.readAllLines(Paths.get(path), StandardCharsets.UTF_8);
    }

    private static List<String[]> readRules(String path)
            throws IOException {
        String ls = System.lineSeparator();
        String lines = new String(Files.readAllBytes(Paths.get(path)), "UTF-8");
        List<String[]> rules = new ArrayList<>();
        for (String line : lines.split(ls + ls))
            rules.add(line.split(ls));
        return rules;
    }
}
```


Output:


```txt
I bought a B of As from T S.
I bought a bag of apples from my brother.

I bought a B of As from T S.
I bought a bag of apples from T shop.

I bought a B of As W my Bgage from T S.
I bought a bag of apples with my money from T shop.

_1111*11111_
11111111111111111111

000000A000000
00011H1111000
```



## JavaScript


```javascript
/**
 * Take a ruleset and return a function which takes a string to which the rules
 * should be applied.
 * @param {string} ruleSet
 * @returns {function(string): string}
 */
const markov = ruleSet => {

  /**
   * Split a string at an index
   * @param {string} s The string to split
   * @param {number} i The index number where to split.
   * @returns {Array<string>}
   */
  const splitAt = (s, i) => [s.slice(0, i), s.slice(i)];

  /**
   * Strip a leading number of chars from a string.
   * @param {string} s The string to strip the chars from
   * @param {string} strip A string who's length will determine the number of
   *    chars to strip.
   * @returns {string}
   */
  const stripLeading = (s, strip) => s.split('')
      .filter((e, i) => i >= strip.length).join('');

  /**
   * Replace the substring in the string.
   * @param {string} s The string to replace the substring in
   * @param {string} find The sub-string to find
   * @param {string} rep The replacement string
   * @returns {string}
   */
  const replace = (s, find, rep) => {
    let result = s;
    if (s.indexOf(find) >= 0) {
      const a = splitAt(s, s.indexOf(find));
      result = [a[0], rep, stripLeading(a[1], find)].join('');
    }
    return result;
  };

  /**
   * Convert a ruleset string into a map
   * @param {string} ruleset
   * @returns {Map}
   */
  const makeRuleMap = ruleset => ruleset.split('\n')
      .filter(e => !e.startsWith('#'))
      .map(e => e.split(' -> '))
      .reduce((p,c) => p.set(c[0], c[1]), new Map());

  /**
   * Recursively apply the ruleset to the string.
   * @param {Map} rules The rules to apply
   * @param {string} s The string to apply the rules to
   * @returns {string}
   */
  const parse = (rules, s) => {
    const o = s;
    for (const [k, v] of rules.entries()) {
      if (v.startsWith('.')) {
        s = replace(s, k, stripLeading(v, '.'));
        break;
      } else {
        s = replace(s, k, v);
        if (s !== o) { break; }
      }
    }
    return o === s ? s : parse(rules, s);
  };

  const ruleMap = makeRuleMap(ruleSet);

  return str => parse(ruleMap, str)
};


const ruleset1 = `# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule`;

const ruleset2 = `# Slightly modified from the rules on Wikipedia
A -> apple
B -> bag
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule`;

const ruleset3 = `# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule`;

const ruleset4 = `### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ -> `;

const ruleset5 = `# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11`;

console.log(markov(ruleset1)('I bought a B of As from T S.'));
console.log(markov(ruleset2)('I bought a B of As from T S.'));
console.log(markov(ruleset3)('I bought a B of As W my Bgage from T S.'));
console.log(markov(ruleset4)('_1111*11111_'));
console.log(markov(ruleset5)('000000A000000'));
```

Output:

```txt
I bought a bag of apples from my brother.
I bought a bag of apples from T shop.
I bought a bag of apples with my money from T shop.
11111111111111111111
00011H1111000
```



## Julia

'''Module''':

```julia
module MarkovAlgos

struct MarkovRule{F,T}
    patt::F
    repl::T
    term::Bool
end

isterminating(r::MarkovRule) = r.term
Base.show(io::IO, rule::MarkovRule) =
    print(io, rule.patt, " → ", isterminating(rule) ? "." : "", rule.repl)
function Base.convert(::Type{MarkovRule}, s::AbstractString)
    rmatch = match(r"^(.+)\s+->\s*(\.)?(.*)?$", s)
    if rmatch ≡ nothing || isempty(rmatch.captures)
        throw(ParseError("not valid rule: " * s))
    end
    patt, term, repl = rmatch.captures
    return MarkovRule(patt, repl ≢ nothing ? repl : "", term ≢ nothing)
end

function ruleset(file::Union{AbstractString,IO})
    ruleset = Vector{MarkovRule}(0)
    for line in eachline(file)
        ismatch(r"(^#|^\s*$)", line) || push!(ruleset, MarkovRule(line))
    end
    return ruleset
end

apply(text::AbstractString, rule::MarkovRule) = replace(text, rule.patt, rule.repl)
function apply(file::Union{AbstractString,IO}, ruleset::AbstractVector{<:MarkovRule})
    text = readstring(file)
    redo = !isempty(text)
    while redo
        matchrule = false
        for rule in ruleset
            if contains(text, rule.patt)
                matchrule = true
                text = apply(text, rule)
                redo = !isterminating(rule)
                break
            end
        end
        redo = redo && matchrule
    end
    return text
end

end  # module MarkovAlgos
```


'''Main''':

```julia
include("module.jl")

let rulesets = @.("data/markovrules0" * string(1:5) * ".txt"),
    ruletest = @.("data/markovtest0" * string(1:5) * ".txt")
    for i in eachindex(rulesets, ruletest)
        rules = MarkovAlgos.ruleset(rulesets[i])
        println("# Example n.$i")
        println("Original:\n", readstring(ruletest[i]))
        println("Transformed:\n", MarkovAlgos.apply(ruletest[i], rules))
    end
end
```


```txt
# Example n.1
Original:
I bought a B of As from T S.

Transformed:
I bought a bag of apples from my brother.

# Example n.2
Original:
I bought a B of As from T S.

Transformed:
I bought a bag of apples from T shop.

# Example n.3
Original:
I bought a B of As W my Bgage from T S.

Transformed:
I bought a bag of apples with my baggage from T shop.

# Example n.4
Original:
_1111*11111_

Transformed:
11111111111111111111

# Example n.5
Original:
000000A000000

Transformed:
00011H1111000
```



## Kotlin

```scala
// version 1.1.51

import java.io.File
import java.util.regex.Pattern

/* rulesets assumed to be separated by a blank line in file */
fun readRules(path: String): List<List<String>> {
    val ls = System.lineSeparator()
    return File(path).readText().split("$ls$ls").map { it.split(ls) }
}

/* tests assumed to be on consecutive lines */
fun readTests(path: String) = File(path).readLines()

fun main(args: Array<String>) {
    val rules = readRules("markov_rules.txt")
    val tests = readTests("markov_tests.txt")
    val pattern = Pattern.compile("^([^#]*?)\\s+->\\s+(\\.?)(.*)")

    for ((i, origTest) in tests.withIndex()) {
        val captures = mutableListOf<List<String>>()
        for (rule in rules[i]) {
            val m = pattern.matcher(rule)
            if (m.find()) {
                val groups = List<String>(m.groupCount()) { m.group(it + 1) }
                captures.add(groups)
            }
        }
        var test = origTest

        do {
            val copy = test
            var redo = false
            for (c in captures) {
                test = test.replace(c[0], c[2])
                if (c[1] == ".") break
                if (test != copy) { redo = true; break }
            }
        }
        while (redo)

        println("$origTest\n$test\n")
    }
}
```


```txt

I bought a B of As from T S.
I bought a bag of apples from my brother.

I bought a B of As from T S.
I bought a bag of apples from T shop.

I bought a B of As W my Bgage from T S.
I bought a bag of apples with my money from T shop.

_1111*11111_
11111111111111111111

000000A000000
00011H1111000

```



## Lua


```lua
-- utility method to escape punctuation
function normalize(str)
	local result = str:gsub("(%p)", "%%%1")
	-- print(result)
	return result
end

-- utility method to split string into lines
function get_lines(str)
	local t = {}
	for line in str:gmatch("([^\n\r]*)[\n\r]*") do
		table.insert(t, line)
	end
	return t
end

local markov = {}
local MARKOV_RULE_PATTERN = "(.+)%s%-%>%s(%.?)(.*)"

function markov.rule(pattern,replacement,terminating)
	return {
		pattern = pattern,
		replacement = replacement,
		terminating = (terminating == ".")
	}, normalize(pattern)
end

function markov.make_rules(sample)
	local lines = get_lines(sample)
	local rules = {}
	local finders = {}
	for i,line in ipairs(lines) do
		if not line:find("^#") then
		s,e,pat,term,rep = line:find(MARKOV_RULE_PATTERN)
		if s then
			r, p = markov.rule(pat,rep,term)
			rules[p] = r
			table.insert(finders, p)
		end
		end
	end
	return {
		rules = rules,
		finders = finders
	}
end

function markov.execute(state, sample_input)

local rules, finders = state.rules, state.finders
local found = false -- did we find any rule?
local terminate = false

repeat
found = false

for i,v in ipairs(finders) do
	local found_now = false -- did we find this rule?
	if sample_input:find(v) then
		found = true
		found_now = true
	end
	sample_input = sample_input:gsub(v, rules[v].replacement, 1)
	-- handle terminating rules
	if found_now then
		if rules[v].terminating then terminate = true end
		break
	end
end

until not found or terminate

return sample_input
end
------------------------------------------
------------------------------------------

local grammar1 = [[
# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule
]]

local grammar2 = [[
# Slightly modified from the rules on Wikipedia
A -> apple
B -> bag
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
]]

local grammar3 = [[
# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
]]

local grammar4 = [[
### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ ->
]]

local grammar5 = [[
# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11
]]

local text1 = "I bought a B of As from T S."
local text2 = "I bought a B of As W my Bgage from T S."
local text3 = '_1111*11111_'
local text4 = '000000A000000'

------------------------------------------
------------------------------------------

function do_markov(rules, input, output)
	local m = markov.make_rules(rules)
	input = markov.execute(m, input)
	assert(input == output)
	print(input)
end

do_markov(grammar1, text1, 'I bought a bag of apples from my brother.')
do_markov(grammar2, text1, 'I bought a bag of apples from T shop.')
-- stretch goals
do_markov(grammar3, text2, 'I bought a bag of apples with my money from T shop.')
do_markov(grammar4, text3, '11111111111111111111')
do_markov(grammar5, text4, '00011H1111000')
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```mathematica
markov[ruleset_, text_] :=
  Module[{terminating = False, output = text,
    rules = StringCases[
      ruleset, {StartOfLine ~~ pattern : Except["\n"] .. ~~
         " " | "\t" .. ~~ "->" ~~ " " | "\t" .. ~~ dot : "" | "." ~~
         replacement : Except["\n"] .. ~~ EndOfLine :> {pattern,
         replacement, dot == "."}}]},
   While[! terminating, terminating = True;
    Do[If[! StringFreeQ[output, rule[[1]]],
      output = StringReplace[output, rule[[1]] -> rule[[2]]];
      If[! rule[[3]], terminating = False]; Break[]], {rule, rules}]];
    output];
```

Example:

```mathematica
markov["# Turing machine: three-state busy beaver
 #
 # state A, symbol 0 => write 1, move right, new state B
 A0 -> 1B
 # state A, symbol 1 => write 1, move left, new state C
 0A1 -> C01
 1A1 -> C11
 # state B, symbol 0 => write 1, move left, new state A
 0B0 -> A01
 1B0 -> A11
 # state B, symbol 1 => write 1, move right, new state B
 B1 -> 1B
 # state C, symbol 0 => write 1, move left, new state B
 0C0 -> B01
 1C0 -> B11
 # state C, symbol 1 => write 1, move left, halt
 0C1 -> H01
 1C1 -> H11", "000000A000000"]
```

Output:

```txt
"00011H1111000"
```


=={{header|MK-61/52}}==
<lang>	9	П4
	КИП4	[x]	П7	Вx	{x}	П8
	ИП8	ИПE	*	П8	{x}	x=0	08
	П5	ИП9	П1	lg	[x]	10^x	П3
		ИП1	П2
		Сx	П6
			ИП2	ИП7	-	x=0	70
				ИП9	^	lg	[x]	1	+	ИП5	-	10^x	/	[x]
				ИП6	ИП8	x#0	50	lg	[x]	1	+	+	10^x	*
				ИП9	ИП6	10^x	П7	/	{x}	ИП7	*	+
				ИП8	ИП7	*	+	П9
			С/П	БП	00
			x>=0	80
			КИП6
			ИП2	ИПE	/	[x]	П2
		x=0	26
		КИП5
		ИП1	ИП3	/	{x}	ИП3	*	П1
		ИП3	ИПE	/	[x]	П3
	x=0	22
	ИП4	ИП0	-	9	-	x=0	02	С/П
```


Under the rules of left 4 registers, under the word has 8 character cells, the alphabet of the digits from 1 to 8. Rules are placed in "123,456", where "123" is a fragment, and "456" is to be replaced, in the registers of the РA to РD. The number of rules is stored in Р0, the initial word is in Р9. Number triggered rule is the last digit registration Р4 (0 to 3), if no rule did not work, the indicator 0, otherwise the current word to be processed. In РE is stored 10.


## OCaml


I'm not familiar with string processing, or parsing, so there are probably better ways to express this in OCaml. One might be with the mikmatch library which allows pattern-matching with regexps. Here I've only used the OCaml stdlib...


```OCaml
(* Useful for resource cleanup (such as filehandles) *)
let try_finally x f g =
  try let res = f x in g x; res
  with e -> g x; raise e

(* Substitute string 'b' for first occurance of regexp 'a' in 's';
 * Raise Not_found if there was no occurance of 'a'. *)
let subst a b s =
  ignore (Str.search_forward a s 0); (* to generate Not_found *)
  Str.replace_first a b s

let parse_rules cin =
  let open Str in
  let rule = regexp "\\(.+\\)[ \t]+->[ \t]+\\(.*\\)" in
  let leader s c = String.length s > 0 && s.[0] = c in
  let parse_b s = if leader s '.' then (string_after s 1,true) else (s,false) in
  let rec parse_line rules =
    try
      let s = input_line cin in
      if leader s '#' then parse_line rules
      else if string_match rule s 0 then
        let a = regexp_string (matched_group 1 s) in
        let b,terminate = parse_b (matched_group 2 s) in
        parse_line ((a,b,terminate)::rules)
      else failwith ("parse error: "^s)
    with End_of_file -> rules
  in List.rev (parse_line [])

let rec run rules text =
  let rec apply s = function
    | [] -> s
    | (a,b,term)::next ->
        try
          let s' = subst a b s in
          if term then s' else run rules s'
        with Not_found -> apply s next
  in apply text rules

let _ =
  if Array.length Sys.argv <> 2 then
    print_endline "Expecting one argument: a filename where rules can be found."
  else
    let rules = try_finally (open_in Sys.argv.(1)) parse_rules close_in in
    (* Translate lines read from stdin, until EOF *)
    let rec translate () =
      print_endline (run rules (input_line stdin));
      translate ()
    in try translate () with End_of_file -> ()
```


With the above compiled to an executable 'markov', and the five rule-sets in files, strings are accepted on stdin for translation:

```txt

<~/rosetta$> markov rules1
I bought a B of As from T S.
I bought a bag of apples from my brother.

<~/rosetta$> markov rules2
I bought a B of As from T S.
I bought a bag of apples from T shop.

<~/rosetta$> markov rules3
I bought a B of As W my Bgage from T S.
I bought a bag of apples with my money from T shop.

<~/rosetta$> markov rules4
_1111*11111_
11111111111111111111

<~/rosetta$> markov rules5
000000A000000
00011H1111000

```



## Perl

This program expects a source file as an argument and uses the standard input and output devices for the algorithm's I/O.


```perl
@ARGV == 1 or die "Please provide exactly one source file as an argument.\n";
open my $source, '<', $ARGV[0] or die "I couldn't open \"$ARGV[0]\" for reading. ($!.)\n";
my @rules;
while (<$source>)
   {/\A#/ and next;
    my @a = /(.*?)\s+->\s+(\.?)(.*)/ or die "Syntax error: $_";
    push @rules, \@a;}
close $source;

my $input = do {local $/; <STDIN>;};

OUTER:
   {foreach (@rules)
       {my ($from, $terminating, $to) = @$_;
        $input =~ s/\Q$from\E/$to/
            and ($terminating ? last OUTER : redo OUTER);}}

print $input;
```



## Perl 6


Run this without arguments and it will scan the cwd for rules.* files and their corresponding test.*.

Run it with two filenames or one filename and some text to run a rulefile on the file contents or the given text.

Add --verbose to see the replacements step-by-step.


```perl6
grammar Markov {
    token TOP {
        ^ [^^ [<rule> | <comment>] $$ [\n|$]]* $
        { make $<rule>>>.ast }
    }
    token comment {
        <before ^^> '#' \N*
        { make Nil }
    }
    token ws {
        [' '|\t]*
    }
    rule rule {
        <before ^^>$<pattern>=[\N+?] '->'
        $<terminal>=[\.]?$<replacement>=[\N*]
        { make {:pattern($<pattern>.Str),
                :replacement($<replacement>.Str),
                :terminal($<terminal>.Str eq ".")} }
    }
}

sub run(:$ruleset, :$start_value, :$verbose?) {
    my $value = $start_value;
    my @rules = Markov.parse($ruleset).ast.list;
    loop {
        my $beginning = $value;
        for @rules {
            my $prev = $value;
            $value = $value.subst(.<pattern>, .<replacement>);
            say $value if $verbose && $value ne $prev;
            return $value if .<terminal>;
            last if $value ne $prev;
        }
        last if $value eq $beginning;
    }
    return $value;
}

multi sub MAIN(Bool :$verbose?) {
    my @rulefiles = dir.grep(/rules.+/).sort;
    for @rulefiles -> $rulefile {
        my $testfile = $rulefile.subst("rules", "test");
        my $start_value = (try slurp($testfile).trim-trailing)
                          // prompt("please give a start value: ");

        my $ruleset = slurp($rulefile);
        say $start_value.perl();
        say run(:$ruleset, :$start_value, :$verbose).perl;
        say '';
    }
}

multi sub MAIN(Str $rulefile where *.IO.f, Str $input where *.IO.f, Bool :$verbose?) {
    my $ruleset = slurp($rulefile);
    my $start_value = slurp($input).trim-trailing;
    say "starting with $start_value.perl()";
    say run(:$ruleset, :$start_value, :$verbose).perl;
}

multi sub MAIN(Str $rulefile where *.IO.f, *@pieces, Bool :$verbose?) {
    my $ruleset = slurp($rulefile);
    my $start_value = @pieces.join(" ");
    say "starting with $start_value.perl()";
    say run(:$ruleset, :$start_value, :$verbose).perl;
}
```



## Phix


```Phix
procedure markov(string rules, input, expected)
    sequence subs = {}, reps = {}
    sequence lines = split(substitute(rules,"\t"," "),'\n')
    for i=1 to length(lines) do
        string li = lines[i]
        if length(li) and li[1]!='#' then
            integer k = match(" -> ",li)
            if k then
                subs = append(subs,trim(li[1..k-1]))
                reps = append(reps,trim(li[k+4..$]))
            end if
        end if
    end for
    string res = input
    bool term = false
    while 1 do
        bool found = false
        for i=1 to length(subs) do
            string sub = subs[i]
            integer k = match(sub,res)
            if k then
                found = true
                string rep = reps[i]
                if length(rep) and rep[1]='.' then
                    rep = rep[2..$]
                    term = true
                end if
                res[k..k+length(sub)-1] = rep
                exit
            end if
            if term then exit end if
        end for
        if term or not found then exit end if
    end while
    ?{input,res,iff(res=expected?"ok":"**ERROR**")}
end procedure

constant ruleset1 = """
# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule"""
markov(ruleset1,"I bought a B of As from T S.","I bought a bag of apples from my brother.")

constant ruleset2 = """
# Slightly modified from the rules on Wikipedia
A -> apple
B -> bag
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule"""
markov(ruleset2,"I bought a B of As from T S.","I bought a bag of apples from T shop.")

constant ruleset3 = """
# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule"""
markov(ruleset3,"I bought a B of As W my Bgage from T S.","I bought a bag of apples with my money from T shop.")

constant ruleset4 = """
### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ ->
"""
markov(ruleset4,"_1111*11111_","11111111111111111111")

constant ruleset5 = """
# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11
"""
markov(ruleset5,"000000A000000","00011H1111000")
```

```txt

{"I bought a B of As from T S.","I bought a bag of apples from my brother.","ok"}
{"I bought a B of As from T S.","I bought a bag of apples from T shop.","ok"}
{"I bought a B of As W my Bgage from T S.","I bought a bag of apples with my money from T shop.","ok"}
{"_1111*11111_","11111111111111111111","ok"}
{"000000A000000","00011H1111000","ok"}

```



## PicoLisp


```PicoLisp
(de markov (File Text)
   (use (@A @Z R)
      (let Rules
         (make
            (in File
               (while (skip "#")
                  (when (match '(@A " " "-" ">" " " @Z) (replace (line) "@" "#"))
                     (link (cons (clip @A) (clip @Z))) ) ) ) )
         (setq Text (chop Text))
         (pack
            (loop
               (NIL
                  (find
                     '((R) (match (append '(@A) (car R) '(@Z)) Text))
                     Rules )
                  Text )
               (T (= "." (cadr (setq R @)))
                  (append @A (cddr R) @Z) )
               (setq Text (append @A (cdr R) @Z)) ) ) ) ) )
```

Output:

```txt
: (markov "r1" "I bought a B of As from T S.")
-> "I bought a bag of apples from my brother."

: (markov "r2" "I bought a B of As from T S.")
-> "I bought a bag of apples from T shop."

: (markov "r3" "I bought a B of As W my Bgage from T S.")
-> "I bought a bag of apples with my money from T shop."

: (markov "r4" "_1111*11111_")
-> "11111111111111111111"

: (markov "r5" "000000A000000")
-> "00011H1111000"
```



## Prolog

Works with SWI-Prolog and module(library(lambda)).

Module lambda can be found there : http://www.complang.tuwien.ac.at/ulrich/Prolog-inedit/lambda.pl


```prolog
:- module('markov.pl', [markov/3, apply_markov/3]).

:- use_module(library(lambda)).

apply_markov(Rules, Sentence, Replacement) :-
	maplist(\X^Y^(atom_chars(X, Ch), phrase(markov(Y), Ch, [])), Rules, TmpRules),
	% comments produce empty rules
	exclude(=([]), TmpRules, LstRules),

	atom_chars(Sentence, L),
	apply_rules(L, LstRules, R),
	atom_chars(Replacement, R).

apply_rules(In, Rules, Out ) :-
	apply_one_rule(In, Rules, Out1, Keep_On),
	(   Keep_On = false
	->  Out = Out1
	;   apply_rules(Out1, Rules, Out)).


apply_one_rule(In, [Rule | Rules], Out, Keep_On) :-
	extract(Rule, In, Out1, KeepOn),
	(   KeepOn = false
	->  Out = Out1, Keep_On = KeepOn
	;   (KeepOn = stop
	    ->	Out = Out1,
		Keep_On = true
	    ;	apply_one_rule(Out1, Rules, Out, Keep_On))).

apply_one_rule(In, [], In, false) .


extract([Pattern, Replace], In, Out, Keep_On) :-
	(   Replace = [.|Rest]
	->  R = Rest
	;   R = Replace),
	(   (append(Pattern, End, T), append(Deb, T, In))
	->  extract([Pattern, Replace], End, NewEnd, _Keep_On),
	    append_3(Deb, R, NewEnd, Out),
	    Keep_On = stop
	;   Out = In,
	    (   R = Replace
	    ->  Keep_On = true
	    ;   Keep_On = false)).


append_3(A, B, C, D) :-
	       append(A, B, T),
	       append(T, C, D).

% creation of the rules
markov(A) --> line(A).

line(A) --> text(A), newline.


newline --> ['\n'], newline.
newline --> [].

text([]) --> comment([]).
text(A) --> rule(A).

comment([]) --> ['#'], anything.

anything --> [X], {X \= '\n'}, anything.
anything --> ['\n'].
anything --> [].

rule([A,B]) -->
	pattern(A), whitespaces, ['-', '>'], whitespaces, end_rule(B).

pattern([X | R]) --> [X], {X \= '\n'}, pattern(R).
pattern([]) --> [].

whitespaces --> ['\t'], whitespace.
whitespaces --> [' '], whitespace.

whitespace --> whitespaces.
whitespace --> [].

end_rule([.| A]) --> [.], rest_of_rule(A).
end_rule(A) --> rest_of_rule(A).
end_rule([]) --> [].

rest_of_rule(A) --> replacement(A).

replacement([X | R]) --> [X], {X \= '\n'}, replacement(R).
replacement([]) --> [].

```

Code to test :

```Prolog
:- use_module('markov.pl').
:- use_module(library(lambda)).

markov :-
	maplist(\X^(call(X), nl,nl), [markov_1, markov_2, markov_3, markov_4, markov_5]).

markov_1 :-
	A = ['# This rules file is extracted from Wikipedia:',
	     '# http://en.wikipedia.org/wiki/Markov_Algorithm',
	     'A -> apple',
	     'B -> bag',
	     'S -> shop',
	     'T -> the',
	     'the shop -> my brother',
	     'a never used -> .terminating rule'],
	B = 'I bought a B of As from T S.',
	apply_markov(A, B, R),
	writeln(B),
	writeln(R).


markov_2 :-
	A = ['# Slightly modified from the rules on Wikipedia',
	     'A -> apple',
	     'B -> bag',
	     'S -> .shop',
	     'T -> the',
	     'the shop -> my brother',
	     'a never used -> .terminating rule'],

	B = 'I bought a B of As from T S.',

	apply_markov(A, B, R),
	writeln(B),
	writeln(R).


markov_3 :-
	A = ['# BNF Syntax testing rules',
	     'A -> apple',
	     'WWWW -> with',
	     'Bgage -> ->.*',
	     'B -> bag',
	     '->.* -> money',
	     'W -> WW',
	     'S -> .shop',
	     'T -> the',
	     'the shop -> my brother',
	     'a never used -> .terminating rule'],

	B = 'I bought a B of As W my Bgage from T S.',

	apply_markov(A, B, R),
	writeln(B),
	writeln(R).


markov_4 :-
	A = ['### Unary Multiplication Engine, for testing Markov Algorithm implementations',
	     '### By Donal Fellows.',
	     '# Unary addition engine',
	     '_+1 -> _1+',
	     '1+1 -> 11+',
	     '# Pass for converting from the splitting of multiplication into ordinary',
	     '# addition',
	     '1! -> !1',
	     ',! -> !+',
	     '_! -> _',
	     '# Unary multiplication by duplicating left side, right side times',
	     '1*1 -> x,@y',
	     '1x -> xX',
	     'X, -> 1,1',
	     'X1 -> 1X',
	     '_x -> _X',
	     ',x -> ,X',
	     'y1 -> 1y',
	     'y_ -> _',
	     '# Next phase of applying',
	     '1@1 -> x,@y',
	     '1@_ -> @_',
	     ',@_ -> !_',
	     '++ -> +',
	     '# Termination cleanup for addition',
	     '_1 -> 1',
	     '1+_ -> 1',
	     '_+_ -> '],

	B =  '_1111*11111_',

	apply_markov(A, B, R),
	writeln(B),
	writeln(R).

markov_5 :-
	A = ['# Turing machine: three-state busy beaver',
	     '#',
	     '# state A, symbol 0 => write 1, move right, new state B',
	     'A0 -> 1B',
	     '# state A, symbol 1 => write 1, move left, new state C',
	     '0A1 -> C01',
	     '1A1 -> C11',
	     '# state B, symbol 0 => write 1, move left, new state A',
	     '0B0 -> A01',
	     '1B0 -> A11',
	     '# state B, symbol 1 => write 1, move right, new state B',
	     'B1 -> 1B',
	     '# state C, symbol 0 => write 1, move left, new state B',
	     '0C0 -> B01',
	     '1C0 -> B11',
	     '# state C, symbol 1 => write 1, move left, halt',
	     '0C1 -> H01',
	     '1C1 -> H11'],

	B = '000000A000000',
	apply_markov(A, B, R),
	writeln(B),
	writeln(R).

```

Output :

```txt
 ?- markov.
I bought a B of As from T S.
I bought a bag of apples from my brother.


I bought a B of As from T S.
I bought a bag of apples from T shop.


I bought a B of As W my Bgage from T S.
I bought a bag of apples with my money from T shop.


_1111*11111_
11111111111111111111


000000A000000
00011H1111000


true .

```



## PureBasic

The GUI used here allows a ruleset to be loaded from a text file or manually added one rule at a time.  Symbol input can be tested anytime by selecting 'Interpret'.

```PureBasic
Structure mRule
  pattern.s
  replacement.s
  isTerminal.i
EndStructure

Procedure parseRule(text.s, List rules.mRule())
  #tab = 9: #space = 32: #whiteSpace$ = Chr(#space) + Chr(#tab)
  Protected tLen, cPtr, nChar.c, pEnd, pLast, pattern.s

  cPtr = 1
  If FindString(#whiteSpace$, Left(text, cPtr), 1): ProcedureReturn 0: EndIf ;parse error
  If Left(text, cPtr) = "#": ProcedureReturn 2: EndIf ;comment skipped

  tLen = Len(text)
  Repeat
    cPtr + 1
    If cPtr > tLen: ProcedureReturn 0: EndIf ;parse error
    nChar = Asc(Mid(text, cPtr, 1))
    Select nChar
      Case #space, #tab
        Select pEnd
          Case 0 To 2
            pEnd = 1
            pLast = cPtr - 1
          Case 3
            pattern = Left(text, pLast)
        EndSelect
      Case '-'
        If pEnd = 1: pEnd = 2: EndIf
      Case '>'
        If pEnd = 2: pEnd = 3: EndIf
    EndSelect
  Until pattern <> ""

  Repeat
    cPtr + 1
  Until Not FindString(#whiteSpace$, Mid(text, cPtr, 1), 1)
  Protected isTerminal
  If Mid(text, cPtr, 1) = "."
    isTerminal = #True: cPtr + 1
  EndIf

  LastElement(rules()): AddElement(rules())
  rules()\pattern = pattern
  rules()\replacement = Right(text, tLen - cPtr + 1)
  rules()\isTerminal = isTerminal
  ProcedureReturn 1 ;processed rule
EndProcedure

Procedure.s interpretMarkov(text.s, List rules.mRule())
  Repeat
    madeReplacement = #False
    ForEach rules()
      If FindString(text, rules()\pattern, 1)
        text = ReplaceString(text, rules()\pattern, rules()\replacement)
        madeReplacement = #True: isFinished = rules()\isTerminal
        Break
      EndIf
    Next
  Until Not madeReplacement Or isFinished
  ProcedureReturn text
EndProcedure

Procedure addRule(text.s, List rules.mRule())
  Protected result = parseRule(text, rules())
  Select result
    Case 0: AddGadgetItem(7, -1, "Invalid rule: " + #DQUOTE$ + text + #DQUOTE$)
    Case 1: AddGadgetItem(7, -1, "Added: " + #DQUOTE$ + text + #DQUOTE$)
    Case 2: AddGadgetItem(7, -1, "Comment: " + #DQUOTE$ + text + #DQUOTE$)
  EndSelect
EndProcedure

OpenWindow(0, 0, 0, 350, 300, "Markov Algorithm Interpreter", #PB_Window_SystemMenu)
ButtonGadget(0, 45, 10, 75, 20, "Load Ruleset")
ButtonGadget(1, 163, 10, 65, 20, "Add Rule")
ButtonGadget(2, 280, 10, 65, 20, "Interpret")
TextGadget(3, 5, 40, 30, 20, "Input:")
StringGadget(4, 45, 40, 300, 20,"")
TextGadget(5, 5, 100, 35, 20, "Output:")
ButtonGadget(6, 160, 70, 70, 20, "Clear Output")
EditorGadget(7, 45, 100, 300, 195, #PB_Editor_ReadOnly)

NewList rules.mRule()
Define event, isDone, text.s, result, file.s
Repeat
  event = WaitWindowEvent()
  Select event
    Case #PB_Event_Gadget
      Select EventGadget()
        Case 0
          Define file.s, rule.s
          file = OpenFileRequester("Select rule set", "*.txt", "Text (*.txt)|*.txt", 0)
          If file
            ClearList(rules())
            ReadFile(0, file)
            While Not(Eof(0))
              addRule(ReadString(0), rules())
            Wend
            AddGadgetItem(7, -1, "Loaded " +  Str(ListSize(rules())) + " rules."): AddGadgetItem(7, -1, "")
          EndIf
        Case 1
          addRule(GetGadgetText(4), rules())
        Case 2
          text = GetGadgetText(4): AddGadgetItem(7, -1, "Interpret: " + #DQUOTE$ + text + #DQUOTE$)
          AddGadgetItem(7, -1, "Result: " + #DQUOTE$ + interpretMarkov(text, rules()) + #DQUOTE$): AddGadgetItem(7, -1, "")
        Case 6
          ClearGadgetItems(7)
      EndSelect
    Case #PB_Event_CloseWindow
      isDone = #True
  EndSelect
Until isDone

```

Sample output from loading Ruleset 1 and interpreting a symbol:

```txt
Comment: "# This rules file is extracted from Wikipedia:"
Comment: "# http://en.wikipedia.org/wiki/Markov_Algorithm"
Added: "A -> apple"
Added: "B -> bag"
Added: "S -> shop"
Added: "T -> the"
Added: "the shop -> my brother"
Added: "a never used -> .terminating rule"
Loaded 6 rules.

Interpret: "I bought a B of As from T S."
Result: "I bought a bag of apples from my brother."
```



## Python

The example uses a regexp to parse the syntax of the grammar. This regexp is multi-line and verbose, and uses named groups to aid in understanding the regexp and to allow more meaningful group names to be used when extracting the replacement data from the grammars in function <code>extractreplacements</code>.

The example gains flexibility by not being tied to specific files. The functions may be imported into other programs which then can provide textual input from their sources without the need to pass 'file handles' around.

```python
import re

def extractreplacements(grammar):
    return [ (matchobj.group('pat'), matchobj.group('repl'), bool(matchobj.group('term')))
                for matchobj in re.finditer(syntaxre, grammar)
                if matchobj.group('rule')]

def replace(text, replacements):
    while True:
        for pat, repl, term in replacements:
            if pat in text:
                text = text.replace(pat, repl, 1)
                if term:
                    return text
                break
        else:
            return text

syntaxre = r"""(?mx)
^(?:
  (?: (?P<comment> \# .* ) ) |
  (?: (?P<blank>   \s*  ) (?: \n | $ )  ) |
  (?: (?P<rule>    (?P<pat> .+? ) \s+ -> \s+ (?P<term> \.)? (?P<repl> .+) ) )
)$
"""

grammar1 = """\
# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule
"""

grammar2 = '''\
# Slightly modified from the rules on Wikipedia
A -> apple
B -> bag
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
'''

grammar3 = '''\
# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
'''

grammar4 = '''\
### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ ->
'''

grammar5 = '''\
# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11
'''

text1 = "I bought a B of As from T S."

text2 = "I bought a B of As W my Bgage from T S."

text3 = '_1111*11111_'

text4 = '000000A000000'


if __name__ == '__main__':
    assert replace(text1, extractreplacements(grammar1)) \
           == 'I bought a bag of apples from my brother.'
    assert replace(text1, extractreplacements(grammar2)) \
           == 'I bought a bag of apples from T shop.'
    # Stretch goals
    assert replace(text2, extractreplacements(grammar3)) \
           == 'I bought a bag of apples with my money from T shop.'
    assert replace(text3, extractreplacements(grammar4)) \
           == '11111111111111111111'
    assert replace(text4, extractreplacements(grammar5)) \
           == '00011H1111000'
```



## Racket



### The Markov algorithm interpreter


The <tt>Markov-algorithm</tt> for a set of rules returns a function which maps from a string to string and can be used as a first-class object. Rules are represented by abstract data structures.


```racket

#lang racket

(struct ->  (A B))
(struct ->. (A B))

(define ((Markov-algorithm . rules) initial-string)
  (let/cc stop
    ; rewriting rules
    (define (rewrite rule str)
      (match rule
        [(->  a b) (cond [(replace a str b) => apply-rules]
                         [else str])]
        [(->. a b) (cond [(replace a str b) => stop]
                         [else str])]))
    ; the cycle through rewriting rules
    (define (apply-rules s) (foldl rewrite s rules))
    ; the result is a fixed point of rewriting procedure
    (fixed-point apply-rules initial-string)))

;; replaces the first substring A to B in a string s
(define (replace A s B)
  (and (regexp-match? (regexp-quote A) s)
       (regexp-replace (regexp-quote A) s B)))

;; Finds the least fixed-point of a function
(define (fixed-point f x0)
  (let loop ([x x0] [fx (f x0)])
    (if (equal? x fx) fx (loop fx (f fx)))))

```


Example of use:


```racket

> (define MA
    (Markov-algorithm
     (->  "A" "apple")
     (->  "B" "bag")
     (->. "S" "shop")
     (->  "T" "the")
     (->  "the shop" "my brother")
     (->. "a never used" "terminating rule")))

> (MA "I bought a B of As from T S.")
"I bought a bag of apples from T shop."

```



### The source reader


To read from a file just replace <tt>with-input-from-string</tt> ==> <tt>with-input-from-file</tt>.


```racket

;; the reader
(define (read-rules source)
  (with-input-from-string source
    (λ () (for*/list ([line (in-lines)]
                      #:unless (should-be-skipped? line))
            (match line
              [(rx-split A "[[:blank:]]->[[:blank:]][.]" B) (->. A B)]
              [(rx-split A "[[:blank:]]->[[:blank:]]" B)    (->  A B)])))))

;; the new pattern for the match form
(define-match-expander rx-split
  (syntax-rules ()
    [(rx-split A rx B)
     (app (λ (s) (regexp-split (pregexp rx) s)) (list A B))]))

;; skip empty lines and comments
(define (should-be-skipped? line)
  (or (regexp-match? #rx"^#.*" line)
      (regexp-match? #px"^[[:blank:]]*$" line)))

(define (read-Markov-algorithm source)
  (apply Markov-algorithm (read-rules source)))

```


Examples:


```racket

(define R3 (read-Markov-algorithm "
# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule"))

(define R4
  (read-Markov-algorithm "
### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ -> "))

(define R5
  (read-Markov-algorithm "
# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11"))

```



```racket

> (R3 "I bought a B of As W my Bgage from T S.")
"I bought a bag of apples with my money from T shop."

> (R4 "_1111*11111_")
"11111111111111111111"

> (R5 "000000A000000")
"00011H1111000"

```



## REXX

Code was added to the REXX example to optionally list the contents of the ruleset and/or the Markov entries.

Also, blank lines in the ruleset were treated as comments.

```rexx
/*REXX program executes a  Markov  algorithm(s)  against  specified entries.            */
parse arg low high .                             /*allows which  ruleset  to process.   */
if  low=='' |  low==","  then  low=1             /*Not specified?  Then use the default.*/
if high=='' | high==","  then high=6             /* "      "         "   "   "     "    */
tellE= low<0;          tellR= high<0             /*flags: used to display file contents.*/
call readEntry
               do j=abs(low)  to abs(high)       /*process each of these  rulesets.     */
               call readRules j                  /*read    a particular   ruleset.      */
               call execRules j                  /*execute "     "            "         */
               say 'result for ruleset'      j      "───►"      !.j
               end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
execRules: parse arg q .;           if tellE | tellR  then say      /*show a blank line?*/
             do f=1
                do k=1  while @.k\=='';      if left(@.k, 1)=='#' | @.k=''  then iterate
                parse var  @.k   a   ' ->'    b  /*obtain the  A  &  B  parts from rule.*/
                a=strip(a);      b=strip(b)      /*strip leading and/or trailing blanks.*/
                fullstop= left(b, 1)==.          /*is this a  "fullstop"  rule?   1≡yes */
                if fullstop  then b=substr(b, 2) /*purify the  B  part of the rule.     */
                old=!.q                          /*remember the value before the change.*/
                !.q=changestr(a, !.q, b)         /*implement the  ruleset  change.      */
                if fullstop   then if old\==!.q  then return          /*should we stop? */
                if old\==!.q  then iterate f     /*Has Entry changed?   Then start over.*/
                end   /*k*/
              return
              end     /*f*/
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
readEntry: eFID= 'MARKOV.ENT';     if tellE  then say               /*show a blank line?*/
           !.=                                   /*placeholder for all the test entries.*/
                  do e=1  while lines(eFID)\==0  /*read the input file until End-Of-File*/
                  !.e=linein(eFID);  if tellE  then say 'test entry'    e    "───►"    !.e
                  end   /*e*/                    /* [↑]  read and maybe echo the entry. */
           return
/*──────────────────────────────────────────────────────────────────────────────────────*/
readRules: parse arg ? .;  rFID= 'MARKOV_R.'?;  if tellR  then say  /*show a blank line?*/
           @.=                                   /*placeholder for all the Markov rules.*/
                  do r=1  while lines(rFID)\==0  /*read the input file until End-Of-File*/
                  @.r=linein(rFID);  if tellR  then say 'ruleSet' ?"."left(r,4) '───►' @.r
                  end   /*r*/                    /* [↑]  read and maybe echo the rule.  */
           return
```

Some older REXXes don't have a   '''changestr'''   BIF, so one is included here   ──►   [[CHANGESTR.REX]].



```txt

result for ruleset 1 ───► I bought a B of As from T S.
result for ruleset 2 ───► I bought a bag of apples from T shop.
result for ruleset 3 ───► I bought a bag of apples with my money from T shop.
result for ruleset 4 ───► 11111111111111111111
result for ruleset 5 ───► 00011H1111000
result for ruleset 6 ───► |||||

```

```txt

# Rewrite binary numbers to their unary value (| bars).
# I.E.:   101   [base 2]   will be converted to 5 bars.
#──────────────────────────────────────────────────────
|0 -> 0||
1  -> 0|
0  ->

```



## Ruby

```Ruby
def setup(ruleset)
  ruleset.each_line.inject([]) do |rules, line|
    if line =~ /^\s*#/
      rules
    elsif line =~ /^(.+)\s+->\s+(\.?)(.*)$/
      rules << [$1, $3, $2 != ""]
    else
      raise "Syntax error: #{line}"
    end
  end
end

def morcov(ruleset, input_data)
  rules = setup(ruleset)
  while (matched = rules.find { |match, replace, term|
    input_data[match] and input_data.sub!(match, replace)
    }) and !matched[2]
  end
  input_data
end
```


'''Test:'''

```Ruby
ruleset1 = <<EOS
# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule
EOS

puts morcov(ruleset1, "I bought a B of As from T S.")

ruleset2 = <<EOS
# Slightly modified from the rules on Wikipedia
A -> apple
B -> bag
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
EOS

puts morcov(ruleset2, "I bought a B of As from T S.")

ruleset3 = <<EOS
# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
EOS

puts morcov(ruleset3, "I bought a B of As W my Bgage from T S.")

ruleset4 = <<EOS
### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ ->
EOS

puts morcov(ruleset4, "_1111*11111_")

ruleset5 = <<EOS
# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11
EOS

puts morcov(ruleset5, "000000A000000")
```


```txt

I bought a bag of apples from my brother.
I bought a bag of apples from T shop.
I bought a bag of apples with my money from T shop.
11111111111111111111
00011H1111000

```



## Scala

```scala
import scala.io.Source

object MarkovAlgorithm {
  val RulePattern = """(.*?)\s+->\s+(\.?)(.*)""".r
  val CommentPattern = """#.*|\s*""".r

  def rule(line: String) = line match {
    case CommentPattern() => None
    case RulePattern(pattern, terminal, replacement) => Some(pattern, replacement, terminal == ".")
    case _ => error("Syntax error on line "+line)
  }

  def main(args: Array[String]) {
    if (args.size != 2 ) {
      println("Syntax: MarkovAlgorithm inputFile inputPattern")
      exit(1)
    }

    val rules = (Source fromPath args(0) getLines () map rule).toList.flatten

    def algorithm(input: String): String = rules find (input contains _._1) match {
      case Some((pattern, replacement, true)) => input replaceFirst ("\\Q"+pattern+"\\E", replacement)
      case Some((pattern, replacement, false)) => algorithm(input replaceFirst ("\\Q"+pattern+"\\E", replacement))
      case None => input
    }

    println(args(1))
    println(algorithm(args(1)))
  }
}
```


Script-style, and more concise:


```scala
import scala.io.Source

if (argv.size != 2 ) error("Syntax: MarkovAlgorithm inputFile inputPattern")

val rulePattern = """(.*?)\s+->\s+(\.?)(.*)""".r
val isComment = (_: String) matches "#.*|\\s*"
val rules = Source fromPath args(0) getLines () filterNot isComment map (rulePattern unapplySeq _ get) toList;

def algorithm(input: String): String = rules find (input contains _.head) match {
  case Some(Seq(pattern, ".", replacement)) => input replaceFirst ("\\Q"+pattern+"\\E", replacement)
  case Some(Seq(pattern, "", replacement)) => algorithm(input replaceFirst ("\\Q"+pattern+"\\E", replacement))
  case None => input
}

println(argv(1))
println(algorithm(argv(1)))
```


Sample outputs:


```txt

C:\>scala MarkovAlgorithm ruleset1.txt "I bought a B of As from T S."
I bought a B of As from T S.
I bought a bag of apples from my brother.

C:\>scala MarkovAlgorithm ruleset2.txt "I bought a B of As from T S."
I bought a B of As from T S.
I bought a bag of apples from T shop.

C:\>scala MarkovAlgorithm ruleset3.txt "I bought a B of As W my Bgage from T S."
I bought a B of As W my Bgage from T S.
I bought a bag of apples with my money from T shop.

C:\>scala MarkovAlgorithm ruleset4.txt "_1111*11111_"
_1111*11111_
11111111111111111111

```


The script is called much in the same way, but with the ".scala" extension added.


## Scheme


The following implementation uses several string-related procedures provided by SRFI-13 [http://srfi.schemers.org/srfi-13/srfi-13.html].


```scheme

(define split-into-lines
  (lambda (str)
    (let loop ((index 0)
               (result '()))
      (let ((next-index (string-index str #\newline index)))
        (if next-index
            (loop (+ next-index 1)
                  (cons (substring str index next-index) result))
            (reverse (cons (substring str index) result)))))))

(define parse-rules
  (lambda (str)
    (let loop ((rules (split-into-lines str))
               (result '()))
      (if (null? rules)
          (reverse result)
          (let ((rule (car rules)))
            (loop (cdr rules)
                  (if (or (string=? rule "")
                          (eq? (string-ref rule 0) #\#))
                      result
                      (cons
                       (let ((index (string-contains rule "->" 1)))
                         (list (string-trim-right (substring rule 0 index))
                               (string-trim (substring rule (+ index 2)))))
                       result))))))))


(define apply-rules
  (lambda (str rules)
    (let loop ((remaining rules)
               (result str))
      (if (null? remaining)
          result
          (let* ((rule (car remaining))
                 (pattern (car rule))
                 (replacement (cadr rule))
                 (start (string-contains result pattern)))
            (if start
                (if (eq? #\. (string-ref replacement 0))
                    (string-replace result replacement start
                                    (+ start (string-length pattern)) 1)
                    (apply-rules
                     (string-replace result replacement start
                                     (+ start (string-length pattern)))
                     rules))
                (loop (cdr remaining) result)))))))

```



## SequenceL



```sequenceL

import <Utilities/Sequence.sl>;

Rule ::= ( pattern : char(1),
		   replacement : char(1),
		   terminal : bool);

ReplaceResult ::= (newString : char(1), wasReplaced : bool);

main(args(2)) := markov(createRule(split(args[1], '\n')), 1, args[2]);

createRule(line(1)) :=
	let
		containsComments := firstIndexOf(line, '#');
		removedComments := line when containsComments = 0 else
						   line[1 ... containsComments - 1];

		arrowLocation := startOfArrow(removedComments, 1);
		lhs := removedComments[1 ... arrowLocation-1];
		rhs := removedComments[arrowLocation + 4 ... size(removedComments)];
		isTerminal := size(rhs) > 0 and rhs[1] = '.';
	in
		(pattern : lhs,
		 replacement : rhs[2 ... size(rhs)] when isTerminal else rhs,
		 terminal : isTerminal) when size(removedComments) > 0 and arrowLocation /= -1;

startOfArrow(line(1), n) :=
	-1 when n > size(line) - 3 else
	n when (line[n]=' ' or line[n]='\t') and
	       line[n+1] = '-' and line[n+2] = '>' and
	       (line[n+3]=' ' or line[n+3]='\t') else
	startOfArrow(line, n+1);

markov(rules(1), n, input(1)) :=
	let
		replaced := replaceSubString(input, rules[n].pattern, rules[n].replacement, 1);
	in
	input when n > size(rules) else
	replaced.newString when replaced.wasReplaced and rules[n].terminal else
	markov(rules, 1, replaced.newString) when replaced.wasReplaced else
	markov(rules, n+1, input);

replaceSubString(str(1), original(1), new(1), n) :=
	(newString : str, wasReplaced : false)
		when n > size(str) - size(original) + 1 else
	(newString : str[1 ... n - 1] ++ new ++ str[n + size(original) ... size(str)], wasReplaced : true)
		when equalList(str[n ... n + size(original) - 1], original) else
	replaceSubString(str, original, new, n + 1);


```



## SNOBOL4

Note that the run-time data is immediately after the "end" label. This works with CSNOBOL4, on a Unix (or Unix-like) platform.
The Markov rules are actually compiled into the program after parsing, and are then directly executed (self-modifying code).

```SNOBOL4

#!/bin/sh
         exec "snobol4" "-r" "$0" "$@"
*
* http://rosettacode.org/wiki/Execute_a_Markov_algorithm
*
         define('repl(s1,s2,s3)c,t,findc') :(repl_end)
repl     s2 len(1) . c = :f(freturn)
         findc = break(c) . t len(1)
         s2 = pos(0) s2
repl_1   s1 findc = :f(repl_2)
         s1 s2 = :f(repl_3)
         repl = repl t s3 :(repl_1)
repl_3   repl = repl t c :(repl_1)
repl_2   repl = repl s1 :(return)
repl_end
*
         define('quote(s)q,qq') :(quote_end)
quote    q = "'"; qq = '"'
         quote = q repl(s, q, q ' ' qq q qq ' ' q) q :(return)
quote_end
*
         whitespace = span(' ' char(9))
top      r = 0
read     s = input :f(end)
         s pos(0) 'ENDRULE' rpos(0) :s(interp)
         s pos(0) '#' :s(read)
         pattern =; replacement =; term =
         s arb . pattern whitespace '->' whitespace
+           ('.' | '') . term arb . replacement rpos(0) :f(syntax)
         r = r + 1
         f = ident(term, '.') ' :(done)'
         f = ident(term) ' :f(rule' r + 1 ')s(rule1)'
         c = 'rule' r ' s ' quote(pattern) ' = ' quote(replacement) f
         code(c) :s(read)
         output = 'rule: ' s ' generates code ' c ' in error' :(end)
syntax   output = 'rule: ' s ' in error' :(read)
interp   code('rule' r + 1 ' :(done)')
go       s = input :f(end)
         s pos(0) 'END' rpos(0) :s(top)f(rule1)
done     output = s :(go)
end
# This rules file is extracted from Wikipedia:
# http://en.wikipedia.org/wiki/Markov_Algorithm
A -> apple
B -> bag
S -> shop
T -> the
the shop -> my brother
a never used -> .terminating rule
ENDRULE
I bought a B of As from T S.
END
# Slightly modified from the rules on Wikipedia
A -> apple
B -> bag
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
ENDRULE
I bought a B of As from T S.
END
# BNF Syntax testing rules
A -> apple
WWWW -> with
Bgage -> ->.*
B -> bag
->.* -> money
W -> WW
S -> .shop
T -> the
the shop -> my brother
a never used -> .terminating rule
ENDRULE
I bought a B of As W my Bgage from T S.
END
### Unary Multiplication Engine, for testing Markov Algorithm implementations
### By Donal Fellows.
# Unary addition engine
_+1 -> _1+
1+1 -> 11+
# Pass for converting from the splitting of multiplication into ordinary
# addition
1! -> !1
,! -> !+
_! -> _
# Unary multiplication by duplicating left side, right side times
1*1 -> x,@y
1x -> xX
X, -> 1,1
X1 -> 1X
_x -> _X
,x -> ,X
y1 -> 1y
y_ -> _
# Next phase of applying
1@1 -> x,@y
1@_ -> @_
,@_ -> !_
++ -> +
# Termination cleanup for addition
_1 -> 1
1+_ -> 1
_+_ ->
ENDRULE
_1111*11111_
END
# Turing machine: three-state busy beaver
#
# state A, symbol 0 => write 1, move right, new state B
A0 -> 1B
# state A, symbol 1 => write 1, move left, new state C
0A1 -> C01
1A1 -> C11
# state B, symbol 0 => write 1, move left, new state A
0B0 -> A01
1B0 -> A11
# state B, symbol 1 => write 1, move right, new state B
B1 -> 1B
# state C, symbol 0 => write 1, move left, new state B
0C0 -> B01
1C0 -> B11
# state C, symbol 1 => write 1, move left, halt
0C1 -> H01
1C1 -> H11
ENDRULE
000000A000000
END

```




## Swift

```swift
import Foundation

func setup(ruleset: String) -> [(String, String, Bool)] {
    return ruleset.componentsSeparatedByCharactersInSet(NSCharacterSet.newlineCharacterSet())
        .filter { $0.rangeOfString("^s*#", options: .RegularExpressionSearch) == nil }
        .reduce([(String, String, Bool)]()) { rules, line in
            let regex = try! NSRegularExpression(pattern: "^(.+)\\s+->\\s+(\\.?)(.*)$", options: .CaseInsensitive)
            guard let match = regex.firstMatchInString(line, options: .Anchored, range: NSMakeRange(0, line.characters.count)) else { return rules }
            return rules + [(
                (line as NSString).substringWithRange(match.rangeAtIndex(1)),
                (line as NSString).substringWithRange(match.rangeAtIndex(3)),
                (line as NSString).substringWithRange(match.rangeAtIndex(2)) != ""
            )]
        }
}

func markov(ruleset: String, var input: String) -> String {
    let rules = setup(ruleset)
    var terminate = false
    while !terminate {
        guard let i = rules.indexOf ({
            if let range = input.rangeOfString($0.0) {
                input.replaceRange(range, with: $0.1)
                return true
            }
            return false
        }) else { break }
        terminate = rules[i].2
    }
    return input
}


let tests: [(ruleset: String, input: String)] = [
    ("# This rules file is extracted from Wikipedia:\n# http://en.wikipedia.org/wiki/Markov_Algorithm\nA -> apple\nB -> bag\nS -> shop\nT -> the\nthe shop -> my brother\na never used -> .terminating rule", "I bought a B of As from T S."),
    ("# Slightly modified from the rules on Wikipedia\nA -> apple\nB -> bag\nS -> .shop\nT -> the\nthe shop -> my brother\na never used -> .terminating rule", "I bought a B of As from T S."),
    ("# BNF Syntax testing rules\nA -> apple\nWWWW -> with\nBgage -> ->.*\nB -> bag\n->.* -> money\nW -> WW\nS -> .shop\nT -> the\nthe shop -> my brother\na never used -> .terminating rule", "I bought a B of As W my Bgage from T S."),
    ("### Unary Multiplication Engine, for testing Markov Algorithm implementations\n### By Donal Fellows.\n# Unary addition engine\n_+1 -> _1+\n1+1 -> 11+\n# Pass for converting from the splitting of multiplication into ordinary\n# addition\n1! -> !1\n,! -> !+\n_! -> _\n# Unary multiplication by duplicating left side, right side times\n1*1 -> x,@y\n1x -> xX\nX, -> 1,1\nX1 -> 1X\n_x -> _X\n,x -> ,X\ny1 -> 1y\ny_ -> _\n# Next phase of applying\n1@1 -> x,@y\n1@_ -> @_\n,@_ -> !_\n++ -> +\n# Termination cleanup for addition\n_1 -> 1\n1+_ -> 1\n_+_ ->", "_1111*11111_"),
    ("# Turing machine: three-state busy beaver\n#\n# state A, symbol 0 => write 1, move right, new state B\nA0 -> 1B\n# state A, symbol 1 => write 1, move left, new state C\n0A1 -> C01\n1A1 -> C11\n# state B, symbol 0 => write 1, move left, new state A\n0B0 -> A01\n1B0 -> A11\n# state B, symbol 1 => write 1, move right, new state B\nB1 -> 1B\n# state C, symbol 0 => write 1, move left, new state B\n0C0 -> B01\n1C0 -> B11\n# state C, symbol 1 => write 1, move left, halt\n0C1 -> H01\n1C1 -> H11", "000000A000000")
]

for (index, test) in tests.enumerate() {
    print("\(index + 1):", markov(test.ruleset, input: test.input))
}


```

```txt

1: I bought a bag of apples from my brother.
2: I bought a bag of apples from T shop.
3: I bought a bag of apples with my money from T shop.
4: 11111111111111111111
5: 00011H1111000

```



## Tcl

```tcl
package require Tcl 8.5
if {$argc < 3} {error "usage: $argv0 ruleFile inputFile outputFile"}
lassign $argv ruleFile inputFile outputFile

# Read the file of rules
set rules {}
set f [open $ruleFile]
foreach line [split [read $f] \n[close $f]] {
    if {[string match "#*" $line] || $line eq ""} continue
    if {[regexp {^(.+)\s+->\s+(\.?)(.*)$} $line -> from final to]} {
	lappend rules $from $to [string compare "." $final] [string length $from]
    } else {
	error "Syntax error: \"$line\""
    }
}

# Apply the rules
set f [open $inputFile]
set out [open $outputFile w]
foreach line [split [read $f] \n[close $f]] {
    set any 1
    while {$any} {
	set any 0
	foreach {from to more fl} $rules {
	    # If we match the 'from' pattern...
	    if {[set idx [string first $from $line]] >= 0} {
		# Change for the 'to' replacement
		set line [string replace $line $idx [expr {$idx+$fl-1}] $to]

		# Stop if we terminate, otherwise note that we've more work to do
        	set any $more
		break;	# Restart search for rules to apply
	    }
	}
        #DEBUG# puts $line
    }

    # Output the processed line
    puts $out $line
}
close $out
```

In the case where there are no terminating rules and no overlapping issues, the following is an alternative:

```tcl
package require Tcl 8.5
if {$argc < 3} {error "usage: $argv0 ruleFile inputFile outputFile"}
lassign $argv ruleFile inputFile outputFile

# Read the file of rules
set rules {}
set f [open $ruleFile]
foreach line [split [read $f] \n[close $f]] {
    if {[string match "#*" $line] || $line eq ""} continue
    if {[regexp {^(.+)\s+->\s+(.*)$} $line -> from to]} {
        dict set rules $from $to
    } else {
	error "Syntax error: \"$line\""
    }
}

# Apply the rules in a simplistic manner
set in [open $inputFile]
set out [open $outputFile w]
set data [read $in]
close $in
while 1 {
    set newData [string map $rules $data]
    if {$newData eq $data} break
    set data $newData
}
puts $out $data
close $out
```



## VBScript


### =Implementation=


```vb

class markovparser

	dim aRules
	public property let ruleset( sBlock )
		dim i
		aRules = split( sBlock, vbNewLine )
		'~ remove blank lines from end of array
		do while aRules( ubound( aRules ) ) = vbnullstring
			redim preserve aRules( ubound( aRules ) - 1 )
		loop
		'~ parse array
		for i = lbound( aRules ) to ubound( aRules )
			if left( aRules( i ), 1 ) = "#" then
				aRules( i ) = Array( vbnullstring, aRules(i))
			else
				aRules( i ) = Split( aRules( i ), " -> ", 2 )
			end if
		next
	end property

	public function apply( sArg )
		dim ruleapplied
		dim terminator
		dim was
		dim i
		dim repl
		dim changes

		ruleapplied = true
		terminator = false

		do while ruleapplied and (not terminator)
			changes = 0
			was = sArg
			for i = lbound( aRules ) to ubound( aRules )
				repl = aRules(i)(1)
				if left( repl, 1 ) = "." then
					terminator = true
					repl = mid( repl, 2 )
				end if
				sArg = replace( sArg, aRules(i)(0), repl)
				if was <> sArg then
					changes = changes + 1
					if changes = 1 then
						exit for
					end if
				end if
				if terminator then
					exit for
				end if
			next
			if changes = 0 then
				ruleapplied = false
			end if
		loop
		apply = sArg
	end function

	sub dump
		dim i
		for i = lbound( aRules ) to ubound( aRules )
			wscript.echo eef(aRules(i)(0)=vbnullstring,aRules(i)(1),aRules(i)(0)& " -> " & aRules(i)(1))  & eef( left( aRules(i)(1), 1 ) = ".", " #terminator", "" )
		next
	end sub

	private function eef( bCond, sExp1, sExp2 )
		if bCond then
			eef = sExp1
		else
			eef = sExp2
		end if
	end function
end class

```



### ==Invocation==


```vb

dim m1
set m1 = new markovparser
m1.ruleset = "# This rules file is extracted from Wikipedia:" & vbNewLine & _
"# http://en.wikipedia.org/wiki/Markov_Algorithm" & vbNewLine & _
"A -> apple" & vbNewLine & _
"B -> bag" & vbNewLine & _
"S -> shop" & vbNewLine & _
"T -> the" & vbNewLine & _
"the shop -> my brother" & vbNewLine & _
"a never used -> .terminating rule"
wscript.echo m1.apply( "I bought a B of As from T S.")

dim m2
set m2 = new markovparser
m2.ruleset = replace( "# Slightly modified from the rules on Wikipedia\nA -> apple\nB -> bag\nS -> .shop\nT -> the\nthe shop -> my brother\na never used -> .terminating rule", "\n", vbNewLine )
'~ m1.dump
wscript.echo m2.apply( "I bought a B of As from T S.")

dim m3
set m3 = new markovparser
m3.ruleset = replace("# BNF Syntax testing rules\nA -> apple\nWWWW -> with\nBgage -> ->.*\nB -> bag" & vbNewLine & _
"->.* -> money\nW -> WW\nS -> .shop\nT -> the\nthe shop -> my brother\na never used -> .terminating rule", "\n", vbNewLine )
wscript.echo m3.apply("I bought a B of As W my Bgage from T S.")

set m4 = new markovparser
m4.ruleset = "### Unary Multiplication Engine, for testing Markov Algorithm implementations" & vbNewLine & _
"### By Donal Fellows." & vbNewLine & _
"# Unary addition engine" & vbNewLine & _
"_+1 -> _1+" & vbNewLine & _
"1+1 -> 11+" & vbNewLine & _
"# Pass for converting from the splitting of multiplication into ordinary" & vbNewLine & _
"# addition" & vbNewLine & _
"1! -> !1" & vbNewLine & _
",! -> !+" & vbNewLine & _
"_! -> _" & vbNewLine & _
"# Unary multiplication by duplicating left side, right side times" & vbNewLine & _
"1*1 -> x,@y" & vbNewLine & _
"1x -> xX" & vbNewLine & _
"X, -> 1,1" & vbNewLine & _
"X1 -> 1X" & vbNewLine & _
"_x -> _X" & vbNewLine & _
",x -> ,X" & vbNewLine & _
"y1 -> 1y" & vbNewLine & _
"y_ -> _" & vbNewLine & _
"# Next phase of applying" & vbNewLine & _
"1@1 -> x,@y" & vbNewLine & _
"1@_ -> @_" & vbNewLine & _
",@_ -> !_" & vbNewLine & _
"++ -> +" & vbNewLine & _
"# Termination cleanup for addition" & vbNewLine & _
"_1 -> 1" & vbNewLine & _
"1+_ -> 1" & vbNewLine & _
"_+_ -> "
'~ m4.dump
wscript.echo m4.apply( "_1111*11111_")

set fso = createobject("scripting.filesystemobject")
set m5 = new markovparser
m5.ruleset = fso.opentextfile("busybeaver.tur").readall
wscript.echo m5.apply("000000A000000")

```



### ==Output==


```vb

I bought a bag of apples from my brother.
I bought a bag of apples from T shop.
I bought a bag of apples with my money from T shop.
11111111111111111111
00011H1111000

```



## zkl


```zkl
fcn parseRuleSet(lines){
   if(vm.numArgs>1) lines=vm.arglist;  // lines or object
   ks:=L(); vs:=L();
   foreach line in (lines){
      if(line[0]=="#") continue;	// nuke <comment>
      pattern,replacement:=line.replace("\t"," ")
         .split(" -> ",1).apply("strip");
      ks.append(pattern); vs.append(replacement);
   }
   return(ks,vs);
}

fcn markov(text,rules){
   ks,vs:=rules; eks:=ks.enumerate();
   do{ go:=False;
      foreach n,k in (eks){
	 if (Void!=text.find(k)){
	    if (Void==(v:=vs[n])) return(text);
	    if (v[0,1]==".") v=v[1,*] else go=True;
	    text=text.replace(k,v,1);
	    break;  // restart after every rule application, unless terminating
	 }
      }
   }while(go);
   text
}
```


```zkl
ruleSet:=parseRuleSet("# This rules file is extracted from Wikipedia:",
   "# http://en.wikipedia.org/wiki/Markov_Algorithm",
   "A\t->\tapple", "B -> bag", "S -> shop", "T -> the",
   "the shop -> my brother", "a never used -> .terminating rule");
ruleSet.println();
markov("I bought a B of As from T S.",ruleSet).println();
```

```txt

L(L("A","B","S","T","the shop","a never used"),L("apple","bag","shop","the","my brother",".terminating rule"))
I bought a bag of apples from my brother.

```


```zkl
parseRuleSet(  // rule set in a list
 T("# Slightly modified from the rules on Wikipedia",
   "A -> apple", "B -> bag", "S -> .shop", "T -> the",
   "the shop -> my brother", "a never used -> .terminating rule")) :
markov("I bought a B of As from T S.",_).println();

parseRuleSet("# BNF Syntax testing rules", "A -> apple",
   "WWWW -> with", "Bgage -> ->.*", "B -> bag", "->.* -> money",
   "W -> WW", "S -> .shop", "T -> the",
   "the shop -> my brother", "a never used -> .terminating rule") :
markov("I bought a B of As W my Bgage from T S.",_).println();
```

```txt

I bought a bag of apples from T shop.
I bought a bag of apples with my money from T shop.

```

For the next two tasks, read the rule set from a file.

```zkl
parseRuleSet(File("ruleSet4")) : markov("_1111*11111_",_).println();
parseRuleSet(File("ruleSet5")) : markov("000000A000000",_).println();
```

```txt

11111111111111111111
00011H1111000

```



