+++
title = "Twelve statements"
description = ""
date = 2019-10-18T20:41:42Z
aliases = []
[extra]
id = 12318
[taxonomies]
categories = ["task", "Puzzles"]
tags = []
languages = [
  "ada",
  "algol_w",
  "autohotkey",
  "bbc_basic",
  "bracmat",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "eiffel",
  "elena",
  "erre",
  "forth",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "mathematica",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "prolog",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "sidef",
  "swift",
  "tcl",
  "txr",
  "ubasic_4th",
  "vba",
  "yabasic",
  "zkl",
]
+++

This puzzle is borrowed from   [http://math-frolic.blogspot.co.uk/2012/08/mind-wrenching.html math-frolic.blogspot].


Given the following twelve statements, which of them are true?

```txt

 1.  This is a numbered list of twelve statements.
 2.  Exactly 3 of the last 6 statements are true.
 3.  Exactly 2 of the even-numbered statements are true.
 4.  If statement 5 is true, then statements 6 and 7 are both true.
 5.  The 3 preceding statements are all false.
 6.  Exactly 4 of the odd-numbered statements are true.
 7.  Either statement 2 or 3 is true, but not both.
 8.  If statement 7 is true, then 5 and 6 are both true.
 9.  Exactly 3 of the first 6 statements are true.
10.  The next two statements are both true.
11.  Exactly 1 of statements 7, 8 and 9 are true.
12.  Exactly 4 of the preceding statements are true.

```



## Task

When you get tired of trying to figure it out in your head,
write a program to solve it, and print the correct answer or answers.


;Extra credit:
Print out a table of near misses, that is, solutions that are contradicted by only a single statement.





## Ada

Here is the main program, using a generic package Logic.
The expression function introduced by the new standard Ada 2012
are very handy for this task.


```Ada
with Ada.Text_IO, Logic;

procedure Twelve_Statements is

   package L is new Logic(Number_Of_Statements => 12); use L;

   -- formally define the 12 statements as expression function predicates
   function P01(T: Table) return Boolean is (T'Length = 12);              -- list of 12 statements
   function P02(T: Table) return Boolean is (Sum(T(7 .. 12)) = 3);        -- three of last six
   function P03(T: Table) return Boolean is (Sum(Half(T, Even)) = 2);     -- two of the even
   function P04(T: Table) return Boolean is (if T(5) then T(6) and T(7)); -- if 5 is true, then ...
   function P05(T: Table) return Boolean is
      ( (not T(2)) and (not T(3)) and (not T(4)) );                       -- none of preceding three
   function P06(T: Table) return Boolean is (Sum(Half(T, Odd)) = 4);      -- four of the odd
   function P07(T: Table) return Boolean is (T(2) xor T(3));              -- either 2 or 3, not both
   function P08(T: Table) return Boolean is (if T(7) then T(5) and T(6)); -- if 7 is true, then ...
   function P09(T: Table) return Boolean is (Sum(T(1 .. 6)) = 3);         -- three of first six
   function P10(T: Table) return Boolean is (T(11) and T(12));            -- next two
   function P11(T: Table) return Boolean is (Sum(T(7..9)) = 1);           -- one of 7, 8, 9
   function P12(T: Table) return Boolean is (Sum(T(1 .. 11)) = 4);        -- four of the preding

   -- define a global list of statements
   Statement_List: constant Statements :=
     (P01'Access, P02'Access, P03'Access, P04'Access, P05'Access, P06'Access,
      P07'Access, P08'Access, P09'Access, P10'Access, P11'Access, P12'Access);

   -- try out all 2^12 possible choices for the table
   procedure Try(T: Table; Fail: Natural; Idx: Indices'Base := Indices'First) is

      procedure Print_Table(T: Table) is
	 use Ada.Text_IO;
      begin
	 Put("    ");
	 if Fail > 0 then
	    Put("(wrong at");
	    for J in T'Range loop
	       if Statement_List(J)(T) /= T(J) then
		  Put(Integer'Image(J) & (if J < 10 then ")  " else ") "));
	       end if;
	    end loop;
	 end if;
	 if T = (1..12 => False) then
	    Put_Line("All false!");
	 else
	    Put("True are");
	    for J in T'Range loop
	       if T(J) then
		  Put(Integer'Image(J));
	       end if;
	    end loop;
	    New_Line;
	 end if;
      end Print_Table;

      Wrong_Entries: Natural := 0;

   begin
      if Idx <= T'Last then
	 Try(T(T'First .. Idx-1) & False & T(Idx+1 .. T'Last), Fail, Idx+1);
	 Try(T(T'First .. Idx-1) & True  & T(Idx+1 .. T'Last), Fail, Idx+1);
      else -- now Index > T'Last and we have one of the 2^12 choices to test
	 for J in T'Range loop
	    if Statement_List(J)(T) /= T(J) then
	       Wrong_Entries := Wrong_Entries + 1;
	    end if;
	 end loop;
	 if Wrong_Entries = Fail then
	    Print_Table(T);
	 end if;
      end if;
   end Try;

begin
   Ada.Text_IO.Put_Line("Exact hits:");
   Try(T => (1..12 => False), Fail => 0);
   Ada.Text_IO.New_Line;
   Ada.Text_IO.Put_Line("Near Misses:");
   Try(T => (1..12 => False), Fail => 1);
end Twelve_Statements;
```


```txt
Exact hits:
    True are 1 3 4 6 7 11

Near Misses:
    (wrong at 1)  True are 5 8 11
    (wrong at 1)  True are 5 8 10 11 12
    (wrong at 1)  True are 4 8 10 11 12
    (wrong at 8)  True are 1 5
    (wrong at 11) True are 1 5 8
    (wrong at 12) True are 1 5 8 11
    (wrong at 12) True are 1 5 8 10 11 12
    (wrong at 8)  True are 1 5 6 9 11
    (wrong at 8)  True are 1 4
    (wrong at 12) True are 1 4 8 10 11 12
    (wrong at 6)  True are 1 4 6 8 9
    (wrong at 7)  True are 1 3 4 8 9
    (wrong at 9)  True are 1 3 4 6 7 9
    (wrong at 12) True are 1 2 4 7 9 12
    (wrong at 10) True are 1 2 4 7 9 10
    (wrong at 8)  True are 1 2 4 7 8 9
```


Here is the definition the package Logic:


```Ada
generic
   Number_Of_Statements: Positive;
package Logic is

   --types
   subtype Indices is Natural range 1 .. Number_Of_Statements;
   type Table is array(Indices range <>) of Boolean;
   type Predicate is access function(T: Table) return Boolean;
   type Statements is array(Indices) of Predicate;
   type Even_Odd is (Even, Odd);

   -- convenience functions
   function Sum(T: Table) return Natural;
   function Half(T: Table; Which: Even_Odd) return Table;

end Logic;
```


And here is the implementation of the "convenience functions" in Logic:


```Ada
package body Logic is

   function Sum(T: Table) return Natural is
      Result: Natural := 0;
   begin
      for I in T'Range loop
         if T(I) then
            Result := Result + 1;
         end if;
      end loop;
      return Result;
   end Sum;

   function Half(T: Table; Which: Even_Odd) return Table is
      Result: Table(T'Range);
      Last: Natural := Result'First - 1;
   begin
      for I in T'Range loop
         if I mod 2 = (if (Which=Odd) then 1 else 0) then
            Last := Last+1;
            Result(Last) := T(I);
         end if;
      end loop;
      return Result(Result'First .. Last);
   end Half;

end Logic;
```



## ALGOL W


```algolw
begin
    % we have 12 statements to determine the truth/falsehood of (see task)  %

    logical array stmt, expected( 1 :: 12 );

    % logical (boolean) to integer utility procedure                        %
    integer procedure toInteger ( logical value v ) ; if v then 1 else 0;

    % procedure to determine whether the statements are true or not         %
    procedure findExpectedValues ;
    begin
        expected(  1 ) := true;
        expected(  2 ) := 3 = ( toInteger( stmt(  7 ) ) + toInteger( stmt(  8 ) )
                              + toInteger( stmt(  9 ) ) + toInteger( stmt( 10 ) )
                              + toInteger( stmt( 11 ) ) + toInteger( stmt( 12 ) )
                              );
        expected(  3 ) := 2 = ( toInteger( stmt(  2 ) ) + toInteger( stmt(  4 ) )
                              + toInteger( stmt(  6 ) ) + toInteger( stmt(  8 ) )
                              + toInteger( stmt( 10 ) ) + toInteger( stmt( 12 ) )
                              );
        expected(  4 ) := ( not stmt( 5 ) ) or ( stmt( 6 ) and stmt( 7 ) );
        expected(  5 ) := not ( stmt( 2 ) or stmt( 3 ) or stmt( 4 ) );
        expected(  6 ) := 4 = ( toInteger( stmt(  1 ) ) + toInteger( stmt(  3 ) )
                              + toInteger( stmt(  5 ) ) + toInteger( stmt(  7 ) )
                              + toInteger( stmt(  9 ) ) + toInteger( stmt( 11 ) )
                              );
        expected(  7 ) := stmt( 2 ) not = stmt( 3 );
        expected(  8 ) := ( not stmt( 7 ) ) or ( stmt( 5 ) and stmt( 6 ) );
        expected(  9 ) := 3 = ( toInteger( stmt(  1 ) ) + toInteger( stmt(  2 ) )
                              + toInteger( stmt(  3 ) ) + toInteger( stmt(  4 ) )
                              + toInteger( stmt(  5 ) ) + toInteger( stmt(  6 ) )
                              );
        expected( 10 ) := stmt( 11 ) and stmt( 12 );
        expected( 11 ) := 1 = ( toInteger( stmt(  7 ) )
                              + toInteger( stmt(  8 ) )
                              + toInteger( stmt(  9 ) )
                              );
        expected( 12 ) := 4 = ( toInteger( stmt(  1 ) ) + toInteger( stmt(  2 ) )
                              + toInteger( stmt(  3 ) ) + toInteger( stmt(  4 ) )
                              + toInteger( stmt(  5 ) ) + toInteger( stmt(  6 ) )
                              + toInteger( stmt(  7 ) ) + toInteger( stmt(  8 ) )
                              + toInteger( stmt(  9 ) ) + toInteger( stmt( 10 ) )
                              + toInteger( stmt( 11 ) )
                              );
    end expected ;

    % clearly, statement 1 is true, however to enumerate the near           %
    % solutions, we need to consider "solutions" where statement 1 is false %
    % we iterate through the possibilities for the statements,              %
    % looking for a non-contradictory set of values                         %
    % we print the solutions with allowedContradictions contradictions      %
    procedure printSolutions ( integer    value allowedContradictions
                             ; string(60) value heading
                             ) ;
    begin
        logical array wrong( 1 :: 12 );
        write( heading );
        write( "     1  2  3  4  5  6  7  8  9 10 11 12"  );
        write( "
### ==============================
" );
        % there are 12 statements, so we have 2^12 possible combinations    %
        for solution := 1 until 4096 do begin
            integer n, incorrect;
            % convert the number to the set of true/false values            %
            n := solution;
            for dPos := 1 until 12 do begin
                stmt( dPos ) := odd( n );
                n := n div 2;
            end for_dPos ;
            % get the expected values of the statements, based on the       %
            % suggested values                                              %
            findExpectedValues;
            % count the contradictions, if we have the required number,     %
            % print the solution                                            %
            incorrect := 0;
            for dPos := 1 until 12 do begin
                wrong( dPos ) := expected( dPos ) not = stmt( dPos );
                incorrect     := incorrect + toInteger( wrong( dPos ) );
            end for_dPos ;
            if incorrect = allowedContradictions then begin
                % have a solution                                            %
                write( "    " );
                for s := 1 until 12 do writeon( s_w := 0
                                              , " "
                                              , if stmt(  s ) then "T" else "-"
                                              , if wrong( s ) then "*" else " "
                                              );
            end ;
        end for_solution ;
    end printSolutions ;

    % find complete solutions                                                %
    printSolutions( 0, "Solutions" );
    % find near solutions                                                    %
    printSolutions( 1, "Near solutions (incorrect values marked ""*"")" );

end.
```

```txt
Solutions
     1  2  3  4  5  6  7  8  9 10 11 12

### ==============================

     T  -  T  T  -  T  T  -  -  -  T  -
Near solutions (incorrect values marked "*")
     1  2  3  4  5  6  7  8  9 10 11 12

### ==============================

     T  -  -  T  -  -  -  -* -  -  -  -
     T  -  -  -  T  -  -  -* -  -  -  -
     T  -  -  -  T  -  -  T  -  -  -* -
     T  -  T  T  -  T  T  -  T* -  -  -
     T  -  T  T  -  -  -* T  T  -  -  -
     T  -  -  T  -  T* -  T  T  -  -  -
     T  T  -  T  -  -  T  T* T  -  -  -
     T  T  -  T  -  -  T  -  T  T* -  -
     -* -  -  -  T  -  -  T  -  -  T  -
     T  -  -  -  T  -  -  T  -  -  T  -*
     T  -  -  -  T  T  -  -* T  -  T  -
     T  T  -  T  -  -  T  -  T  -  -  T*
     -* -  -  T  -  -  -  T  -  T  T  T
     T  -  -  T  -  -  -  T  -  T  T  T*
     -* -  -  -  T  -  -  T  -  T  T  T
     T  -  -  -  T  -  -  T  -  T  T  T*

```



## AutoHotkey


Just like the Python version, this code uses bruteforce (4096 iterations) to set 12 flags and test all statements on each iteration.
If the proposed flags match the results after validating each statement, we have the solution.
The code shows all cases where we have at least S-1 matches (where S = 12 statements).


```AutoHotkey
; Note: the original puzzle provides 12 statements and starts with
; "Given the following twelve statements...", so the first statement
; should ignore the F1 flag and always be true (see "( N == 1 )").

S := 12 ; number of statements
Output := ""
Loop, % 2**S {
	;;If !Mod(A_Index,100) ;; optional 'if' to show the loop progress
	;;	ToolTip, Index: %A_Index%
	SetFlags(A_Index-1), Current := "", Count := 0
	Loop, %S%
		R := TestStatement(A_Index), Current .= " " R, Count += (R == F%A_Index%)
	If ( Count >= S-1 )
		Output .= Count " ->" Current "`n"
	If ( Count = S )
		Solution := "`nSolution = " Current
}
ToolTip
MsgBox, % Output . Solution
Return

;-------------------------------------------------------------------------------------

SetFlags(D) {
	Local I
	Loop, %S%
		I := S-A_Index+1 , F%I% := (D >> (S-A_Index)) & 1
}

;-------------------------------------------------------------------------------------

TestStatement(N) {
	Local I, C := 0
	If ( N == 1 ) ; This is a numbered list of twelve statements.
		Return ( S == 12 ) ; should always be true
	If ( N == 2 ) { ; Exactly 3 of the last 6 statements are true.
		Loop, 6
			I := S-A_Index+1 , C += F%I%
		Return ( C == 3 )
	}
	If ( N == 3 ) { ; Exactly 2 of the even-numbered statements are true.
		Loop, %S%
			C += ( !Mod(A_Index,2) & F%A_Index% )
		Return ( C == 2 )
	}
	If ( N == 4 ) ; If statement 5 is true, then statements 6 and 7 are both true.
		Return ( F5 ? F6 & F7 : 1 )
	If ( N == 5 ) { ; The 3 preceding statements are all false.
		Loop, 3
			I := N-A_Index , C += F%I%
		Return ( C == 0 )
	}
	If ( N == 6 ) { ; Exactly 4 of the odd-numbered statements are true.
		Loop, %S%
			C += ( !!Mod(A_Index,2) & F%A_Index% )
		Return ( C == 4 )
	}
	If ( N == 7 ) ; Either statement 2 or 3 is true, but not both.
		Return ( F2 ^ F3 )
	If ( N == 8 ) ; If statement 7 is true, then 5 and 6 are both true.
		Return ( F7 ? F5 & F6 : 1 )
	If ( N == 9 ) { ; Exactly 3 of the first 6 statements are true.
		Loop, 6
			C += F%A_Index%
		Return ( C == 3 )
	}
	If ( N == 10 ) ; The next two statements are both true.
		Return ( F11 & F12 )
	If ( N == 11 ) ; Exactly 1 of statements 7, 8 and 9 are true
		Return ( F7+F8+F9 == 1 )
	If ( N == 12 ) { ; Exactly 4 of the preceding statements are true
		Loop, % N-1
			C += F%A_Index%
		Return ( C == 4 )
	}
}
```

```txt
11 -> 1 0 0 1 0 0 0 1 0 0 0 0
11 -> 1 0 0 0 1 0 0 1 0 0 0 0
11 -> 1 0 0 0 1 0 0 1 0 0 1 0
11 -> 1 0 1 1 0 1 1 0 0 0 0 0
11 -> 1 0 1 1 0 0 1 1 1 0 0 0
11 -> 1 0 0 1 0 0 0 1 1 0 0 0
11 -> 1 1 0 1 0 0 1 0 1 0 0 0
11 -> 1 1 0 1 0 0 1 0 1 0 0 0
12 -> 1 0 1 1 0 1 1 0 0 0 1 0
11 -> 1 0 0 0 1 0 0 1 0 0 1 0
11 -> 1 0 0 0 1 0 0 1 0 0 1 1
11 -> 1 0 0 0 1 1 0 1 1 0 1 0
11 -> 1 1 0 1 0 0 1 0 1 0 0 0
11 -> 1 0 0 1 0 0 0 1 0 1 1 1
11 -> 1 0 0 1 0 0 0 1 0 1 1 0
11 -> 1 0 0 0 1 0 0 1 0 1 1 1
11 -> 1 0 0 0 1 0 0 1 0 1 1 0

Solution =  1 0 1 1 0 1 1 0 0 0 1 0
```



## BBC BASIC

```bbcbasic
      nStatements% = 12
      DIM Pass%(nStatements%), T%(nStatements%)

      FOR try% = 0 TO 2^nStatements%-1

        REM Postulate answer:
        FOR stmt% = 1 TO 12
          T%(stmt%) = (try% AND 2^(stmt%-1)) <> 0
        NEXT

        REM Test consistency:
        Pass%(1)  = T%(1) = (nStatements% = 12)
        Pass%(2)  = T%(2) = ((T%(7)+T%(8)+T%(9)+T%(10)+T%(11)+T%(12)) = -3)
        Pass%(3)  = T%(3) = ((T%(2)+T%(4)+T%(6)+T%(8)+T%(10)+T%(12)) = -2)
        Pass%(4)  = T%(4) = ((NOT T%(5) OR (T%(6) AND T%(7))))
        Pass%(5)  = T%(5) = (NOT T%(2) AND NOT T%(3) AND NOT T%(4))
        Pass%(6)  = T%(6) = ((T%(1)+T%(3)+T%(5)+T%(7)+T%(9)+T%(11)) = -4)
        Pass%(7)  = T%(7) = ((T%(2) EOR T%(3)))
        Pass%(8)  = T%(8) = ((NOT T%(7) OR (T%(5) AND T%(6))))
        Pass%(9)  = T%(9) = ((T%(1)+T%(2)+T%(3)+T%(4)+T%(5)+T%(6)) = -3)
        Pass%(10) = T%(10) = (T%(11) AND T%(12))
        Pass%(11) = T%(11) = ((T%(7)+T%(8)+T%(9)) = -1)
        Pass%(12) = T%(12) = ((T%(1)+T%(2)+T%(3)+T%(4)+T%(5)+T%(6) + \
        \                      T%(7)+T%(8)+T%(9)+T%(10)+T%(11)) = -4)

        CASE SUM(Pass%()) OF
          WHEN -11:
            PRINT "Near miss with statements ";
            FOR stmt% = 1 TO 12
              IF T%(stmt%) PRINT ; stmt% " ";
              IF NOT Pass%(stmt%) miss% = stmt%
            NEXT
            PRINT "true (failed " ;miss% ")."
          WHEN -12:
            PRINT "Solution! with statements ";
            FOR stmt% = 1 TO 12
              IF T%(stmt%) PRINT ; stmt% " ";
            NEXT
            PRINT "true."
        ENDCASE

      NEXT try%
      END
```

```txt

Near miss with statements 1 4 true (failed 8).
Near miss with statements 1 5 true (failed 8).
Near miss with statements 1 5 8 true (failed 11).
Near miss with statements 1 3 4 6 7 9 true (failed 9).
Near miss with statements 1 3 4 8 9 true (failed 7).
Near miss with statements 1 4 6 8 9 true (failed 6).
Near miss with statements 1 2 4 7 8 9 true (failed 8).
Near miss with statements 1 2 4 7 9 10 true (failed 10).
Solution! with statements 1 3 4 6 7 11 true.
Near miss with statements 5 8 11 true (failed 1).
Near miss with statements 1 5 8 11 true (failed 12).
Near miss with statements 1 5 6 9 11 true (failed 8).
Near miss with statements 1 2 4 7 9 12 true (failed 12).
Near miss with statements 4 8 10 11 12 true (failed 1).
Near miss with statements 1 4 8 10 11 12 true (failed 12).
Near miss with statements 5 8 10 11 12 true (failed 1).
Near miss with statements 1 5 8 10 11 12 true (failed 12).

```



## Bracmat


```bracmat
(
    ( number
    =   n done ntest oldFT
      .   !arg:(?done.(=?ntest).?oldFT)
        & 0:?n
        & (   !done
            :   ?
                ( !ntest
                . !oldFT&1+!n:?n&~
                )
                ?
          | !n
          )
    )
  & ( STATEMENTS
    =   ( (1."This is a numbered list of twelve statements.")
        . 1
        . (
          =   n nr done toDo
            .   !arg:(?done.?toDo)
              & 0:?n
              &   whl
                ' ( !done:(?nr.?) ?done
                  & 1+!n:!nr:?n
                  )
              &   whl
                ' ( !toDo:((?nr.?).?) ?toDo
                  & 1+!n:!nr:?n
                  )
              & (!n:12&true|false)
          )
        )
        ( (2."Exactly 3 of the last 6 statements are true.")
        . end
        . (
          =   done toDo lastSix
            .   !arg:(?done.?toDo)
              & !done:? [-7 ?lastSix
              & (   number$(!lastSix.(=?).true):3
                  & true
                | false
                )
          )
        )
        ( (3."Exactly 2 of the even-numbered statements are true.")
        . end
        . (
          =   done toDo ii
            .   !arg:(?done.?toDo)
              & (       number
                      $ ( !done
                        . (=?ii&!ii*1/2:~/)
                        . true
                        )
                    : 2
                  & true
                | false
                )
          )
        )
        ( (4."If statement 5 is true, then statements 6 and 7 are both true.")
        . 7
        . (
          =   done toDo
            .   !arg:(?done.?toDo)
              & (     !done
                    : ( ? (5.false) ?
                      |   ? (6.true) ?
                        : ? (7.true) ?
                      )
                  & true
                | false
                )
          )
        )
        ( (5."The 3 preceding statements are all false.")
        . 5
        . (
          =   done toDo
            .   !arg:(?done.?toDo)
              & (     !done
                    :   ?
                        (?.false)
                        (?.false)
                        (?.false)
                        (?.?)
                  & true
                | false
                )
          )
        )
        ( (6."Exactly 4 of the odd-numbered statements are true.")
        . end
        . (
          =   done toDo i
            .   !arg:(?done.?toDo)
              & (       number
                      $ ( !done
                        . (=?i&!i*1/2:/)
                        . true
                        )
                    : 4
                  & true
                | false
                )
          )
        )
        ( (7."Either statement 2 or 3 is true, but not both.")
        . 7
        . (
          =   done toDo
            .   !arg:(?done.?toDo)
              & (       number
                      $ (!done.(=2|3).true)
                    : 1
                  & true
                | false
                )
          )
        )
        ( (8."If statement 7 is true, then 5 and 6 are both true.")
        . 8
        . (
          =   done toDo
            .   !arg:(?done.?toDo)
              & (     !done
                    : ( ? (7.false) ?
                      |   ? (5.true) ?
                        : ? (6.true) ?
                      )
                  & true
                | false
                )
          )
        )
        ( (9."Exactly 3 of the first 6 statements are true.")
        . 9
        . (
          =   done toDo firstSix
            .   !arg:(?done.?toDo)
              & !done:?firstSix [6 ?
              & (   number$(!firstSix.(=?).true):3
                  & true
                | false
                )
          )
        )
        ( (10."The next two statements are both true.")
        . 12
        . (
          =   done toDo
            .   !arg:(?done.?toDo)
              & (   !done:? (?.true) (?.true)
                  & true
                | false
                )
          )
        )
        ( (11."Exactly 1 of statements 7, 8 and 9 are true.")
        . 11
        . (
          =   done toDo
            .   !arg:(?done.?toDo)
              & (       number
                      $ ( !done
                        . (=7|8|9)
                        . true
                        )
                    : 1
                  & true
                | false
                )
          )
        )
        ( (12."Exactly 4 of the preceding statements are true.")
        . 12
        . (
          =   done toDo preceding
            .   !arg:(?done.?toDo)
              & !done:?preceding (?.?)
              & (   number$(!preceding.(=?).true):4
                  & true
                | false
                )
          )
        )
    )
  & ( TestTruth
    =     done toDo postponedTests testToBePostponed
        , n when test FT oldFT A Z text
        , postponedTest testNow
      .   !arg:(?done.?toDo.?postponedTests)
        & (   !toDo:
            & "We have come to the end of the list of tests.
               Perform any tests that had to be postponed until now."
            &   whl
              ' (   !postponedTests
                  : (?.?oldFT.(=?postponedTest)) ?A
                & postponedTest$(!done.):!oldFT
                & !A:?postponedTests
                )
            & !postponedTests:
            & out$("Solution:" !done)
            & ~
          |     !toDo
              : ((?n.?text).?when.(=?test)) ?toDo
            & "'false' and 'true' are just two symbols, not 'boolean values'.
                You can choose other symbols if you like.
                The program first guesses the first symbol and assigns it to the variable FT.
                After backtracking, the second symbol is guessed and assigned to FT.
                This is done for each statement."
            &   false true
              :   ?
                  %@?FT
                  ( ?
                  & 1+!guesses:?guesses
                  & (!n.!FT):?testNow
                  & "Do all tests that had to be postponed until now, unless one of those tests
                    fails. Remove the successful tests from the list of postponed tests."
                  &   whl
                    ' (   !postponedTests
                        :   ?A
                            (!n.?oldFT.(=?postponedTest))
                            ?Z
                      &   postponedTest$(!done !testNow.!toDo)
                        : !oldFT
                      & !A !Z:?postponedTests
                      )
                  & "Check that all tests that had to be postponed until now are removed from
                     the list of postponed tests. Only then go on with looking at testing
                     the current statement. Backtrack if a test failed."
                  & !postponedTests:~(? (!n.?) ?)
                  & (   !when:>!n
                      & "The current statement cannot be tested right now. Postpone it to
                         the earliest coming statement where the current statement can be
                         tested.
                         (The earliest statement, denoted by 'when', is computed manually.)"
                      & (!when.!FT.'$test):?testToBePostponed
                    |   "No need to postpone. Test the current statement now."
                      & :?testToBePostponed
                      & "If the test fails, backtrack. If it succeeds, go on to the next
                         statement."
                      & test$(!done !testNow.!toDo):!FT
                    )
                  & "So far so good. Test the next statements. (recursively)"
                  &   TestTruth
                    $ ( !done !testNow
                      . !toDo
                      . !testToBePostponed !postponedTests
                      )
                  )
          )
    )
  & 0:?guesses
  & TestTruth$(.!STATEMENTS.)
|   out
  $ ( str
    $ ( "That's it. I made "
        !guesses
        " true/false guesses in all. (A brute force method needs 2^12="
        2^12
        " guesses."
      )
    )
);
```

```txt
  Solution:
  (1.true)
  (2.false)
  (3.true)
  (4.true)
  (5.false)
  (6.true)
  (7.true)
  (8.false)
  (9.false)
  (10.false)
  (11.true)
  (12.false)
That's it. I made 220 true/false guesses in all. (A brute force method needs 2^12=4096 guesses.
```


## Clojure


```clojure
(use '[clojure.math.combinatorics]

(defn xor? [& args]
  (odd? (count (filter identity args))))

(defn twelve-statements []
  (for [[a b c d e f g h i j k l] (selections [true false] 12)
    :when (true? a)
    :when (if (= 3 (count (filter true? [g h i j k l]))) (true? b) (false? b))
    :when (if (= 2 (count (filter true? [b d f h j l]))) (true? c) (false? c))
    :when (if (or (false? e) (every? true? [e f g])) (true? d) (false? d))
    :when (if (every? false? [b c d]) (true? e) (false? e))
    :when (if (= 4 (count (filter true? [a c e g i k]))) (true? f) (false? f))
    :when (if (xor? (true? b) (true? c)) (true? g) (false? g))
    :when (if (or (false? g) (every? true? [e f g])) (true? h) (false? h))
    :when (if (= 3 (count (filter true? [a b c d e f]))) (true? i) (false? i))
    :when (if (every? true? [k l]) (true? j) (false? j))
    :when (if (= 1 (count (filter true? [g h i]))) (true? k) (false? k))
    :when (if (= 4 (count (filter true? [a b c d e f g h i j k]))) (true? l) (false? l))]
  [a b c d e f g h i j k l]))
```


```txt

=> (twelve-statements)
([true false true true false true true false false false true false])

```



## Common Lisp


```lisp

(defparameter *state* (make-list 12))

(defparameter *statements* '(t                                                    ; 1
                             (= (count-true '(7 8 9 10 11 12)) 3)                 ; 2
                             (= (count-true '(2 4 6 8 10 12)) 2)                  ; 3
                             (or (not (p 5)) (and (p 6) (p 7)))                   ; 4
                             (and (not (p 2)) (not (p 3)) (not (p 4)))            ; 5
                             (= (count-true '(1 3 5 7 9 11)) 4)                   ; 6
                             (or (and (p 2) (not (p 3))) (and (not (p 2)) (p 3))) ; 7
                             (or (not (p 7)) (and (p 5) (p 6)))                   ; 8
                             (= (count-true '(1 2 3 4 5 6)) 3)                    ; 9
                             (and (p 11) (p 12))                                  ;10
                             (= (count-true '(7 8 9)) 1)                          ;11
                             (= (count-true '(1 2 3 4 5 6 7 8 9 10 11)) 4)))      ;12

(defun start ()
  (loop while (not (equal *state* '(t t t t t t t t t t t t)))
        do (progn (let ((true-stats (check)))
		       (cond ((= true-stats 11) (result nil))
			     ((= true-stats 12) (result t))))
		  (new-state))))

(defun check ()
  (loop for el in *state*
        for stat in *statements*
        counting (eq el (eval stat)) into true-stats
        finally (return true-stats)))

(defun count-true (lst)
  (loop for i in lst
    counting (nth (- i 1) *state*) into total
    finally (return total)))

(defun p (n)
  (nth (- n 1) *state*))

(defun new-state ()
  (let ((contr t))
       (loop for i from 0 to 11
         do (progn (setf (nth i *state*) (not (eq (nth i *state*) contr)))
                   (setq contr (and contr (not (nth i *state*))))))))

(defun result (?)
  (format t "~:[Missed by one~;Solution:~] ~%~{~:[F~;T~] ~}~%" ? *state*))
```


```txt

Missed by one
T F F T F F F F F F F F
Missed by one
T F F F T F F F F F F F
Missed by one
T F F F T F F T F F F F
Missed by one
T F T T F T T F T F F F
Missed by one
T F T T F F F T T F F F
Missed by one
T F F T F T F T T F F F
Missed by one
T T F T F F T T T F F F
Missed by one
T T F T F F T F T T F F
Solution:
T F T T F T T F F F T F
Missed by one
F F F F T F F T F F T F
Missed by one
T F F F T F F T F F T F
Missed by one
T F F F T T F F T F T F
Missed by one
T T F T F F T F T F F T
Missed by one
F F F T F F F T F T T T
Missed by one
T F F T F F F T F T T T
Missed by one
F F F F T F F T F T T T
Missed by one
T F F F T F F T F T T T
NIL
```



## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;

public static class TwelveStatements
{
    public static void Main() {
        Func<Statements, bool>[] checks = {
            st => st[1],
            st => st[2] == (7.To(12).Count(i => st[i]) == 3),
            st => st[3] == (2.To(12, by: 2).Count(i => st[i]) == 2),
            st => st[4] == st[5].Implies(st[6] && st[7]),
            st => st[5] == (!st[2] && !st[3] && !st[4]),
            st => st[6] == (1.To(12, by: 2).Count(i => st[i]) == 4),
            st => st[7] == (st[2] != st[3]),
            st => st[8] == st[7].Implies(st[5] && st[6]),
            st => st[9] == (1.To(6).Count(i => st[i]) == 3),
            st => st[10] == (st[11] && st[12]),
            st => st[11] == (7.To(9).Count(i => st[i]) == 1),
            st => st[12] == (1.To(11).Count(i => st[i]) == 4)
        };

        for (Statements statements = new Statements(0); statements.Value < 4096; statements++) {
            int count = 0;
            int falseIndex = 0;
            for (int i = 0; i < checks.Length; i++) {
                if (checks[i](statements)) count++;
                else falseIndex = i;
            }
            if (count == 0) Console.WriteLine($"{"All wrong:", -13}{statements}");
            else if (count == 11) Console.WriteLine($"{$"Wrong at {falseIndex + 1}:", -13}{statements}");
            else if (count == 12) Console.WriteLine($"{"All correct:", -13}{statements}");
        }
    }

    struct Statements
    {
        public Statements(int value) : this() { Value = value; }

        public int Value { get; }

        public bool this[int index] => (Value & (1 << index - 1)) != 0;

        public static Statements operator ++(Statements statements) => new Statements(statements.Value + 1);

        public override string ToString() {
            Statements copy = this; //Cannot access 'this' in anonymous method...
            return string.Join(" ", from i in 1.To(12) select copy[i] ? "T" : "F");
        }

    }

    //Extension methods
    static bool Implies(this bool x, bool y) => !x || y;

    static IEnumerable<int> To(this int start, int end, int by = 1) {
        while (start <= end) {
            yield return start;
            start += by;
        }
    }

}
```

```txt

Wrong at 8:  T F F T F F F F F F F F
Wrong at 8:  T F F F T F F F F F F F
Wrong at 11: T F F F T F F T F F F F
Wrong at 9:  T F T T F T T F T F F F
Wrong at 7:  T F T T F F F T T F F F
Wrong at 6:  T F F T F T F T T F F F
Wrong at 8:  T T F T F F T T T F F F
Wrong at 10: T T F T F F T F T T F F
All correct: T F T T F T T F F F T F
Wrong at 1:  F F F F T F F T F F T F
Wrong at 12: T F F F T F F T F F T F
Wrong at 8:  T F F F T T F F T F T F
Wrong at 12: T T F T F F T F T F F T
All wrong:   F F T T T T F F T T F T
All wrong:   F T T F T T T F T F T T
Wrong at 1:  F F F T F F F T F T T T
Wrong at 12: T F F T F F F T F T T T
Wrong at 1:  F F F F T F F T F T T T
Wrong at 12: T F F F T F F T F T T T
```



## C++

```c++
#include <iostream>

#include <vector>
#include <string>
#include <cmath>

using namespace std;

// convert int (0 or 1) to string (F or T)
inline
string str(int n)
{
    return  n ? "T": "F";
}

int main(void)
{
    int solution_list_number = 1;
    vector<string> st;
    st = {
        " 1. This is a numbered list of twelve statements.",
        " 2. Exactly 3 of the last 6 statements are true.",
        " 3. Exactly 2 of the even-numbered statements are true.",
        " 4. If statement 5 is true, then statements 6 and 7 are both true.",
        " 5. The 3 preceding statements are all false.",
        " 6. Exactly 4 of the odd-numbered statements are true.",
        " 7. Either statement 2 or 3 is true, but not both.",
        " 8. If statement 7 is true, then 5 and 6 are both true.",
        " 9. Exactly 3 of the first 6 statements are true.",
        " 10. The next two statements are both true.",
        " 11. Exactly 1 of statements 7, 8 and 9 are true.",
        " 12. Exactly 4 of the preceding statements are true."
    };  //  Good solution is: 1 3 4 6 7 11 are true

    int n = 12; // Number of statements.
    int nTemp = (int)pow(2, n); // Number of solutions to check.
    for (int counter = 0; counter < nTemp; counter++)
    {
        vector<int> s;
        for (int k = 0; k < n; k++)
        {
            s.push_back((counter >> k) & 0x1);
        }
        vector<int> test(12);
        int sum = 0;
        // check each of the nTemp solutions for match.
        // 1. This is a numbered list of twelve statements.
        test[0] = s[0];

        // 2. Exactly 3 of the last 6 statements are true.
        sum = s[6]+ s[7]+s[8]+s[9]+s[10]+s[11];
        test[1] = ((sum == 3) == s[1]);

        // 3. Exactly 2 of the even-numbered statements are true.
        sum = s[1]+s[3]+s[5]+s[7]+s[9]+s[11];
        test[2] = ((sum == 2) == s[2]);

        // 4. If statement 5 is true, then statements 6 and 7 are both true.
        test[3] = ((s[4] ? (s[5] && s[6]) : true) == s[3]);

        // 5. The 3 preceding statements are all false.
        test[4] = (((s[1] + s[2] + s[3]) == 0) == s[4]);

        // 6. Exactly 4 of the odd-numbered statements are true.
        sum = s[0] + s[2] + s[4] + s[6] + s[8] + s[10];
        test[5] = ((sum == 4) == s[5]);

        // 7. Either statement 2 or 3 is true, but not both.
        test[6] = (((s[1] + s[2]) == 1) == s[6]);

        // 8. If statement 7 is true, then 5 and 6 are both true.
        test[7] = ((s[6] ? (s[4] && s[5]) : true) == s[7]);

        // 9. Exactly 3 of the first 6 statements are true.
        sum = s[0]+s[1]+s[2]+s[3]+s[4]+s[5];
        test[8] = ((sum == 3) == s[8]);

        // 10. The next two statements are both true.
        test[9] = ((s[10] && s[11]) == s[9]);

        // 11. Exactly 1 of statements 7, 8 and 9 are true.
        sum = s[6]+ s[7] + s[8];
        test[10] = ((sum == 1) == s[10]);

        // 12. Exactly 4 of the preceding statements are true.
        sum = s[0]+s[1]+s[2]+s[3]+s[4]+s[5]+s[6]+s[7]+s[8]+s[9]+s[10];
        test[11] = ((sum == 4) == s[11]);

        // Check test results and print solution if 11 or 12 are true
        int resultsTrue = 0;
        for(unsigned int i = 0; i < test.size(); i++){
            resultsTrue += test[i];
        }
        if(resultsTrue == 11 || resultsTrue == 12){
            cout << solution_list_number++ << ". " ;
            string output = "1:"+str(s[0])+"  2:"+str(s[1])+"  3:"+str(s[2])
                        +"  4:"+str(s[3])+"  5:"+str(s[4])+"  6:"+ str(s[5])
                        +"  7:"+str(s[6])+"  8:"+str(s[7])+"  9:"+str(s[8])
                        +"  10:"+str(s[9])+"  11:"+str(s[10])+"  12:"+ str(s[11]);

            if (resultsTrue == 12) {
                cout << "Full Match, good solution!" << endl;
                cout << "\t" << output << endl;
            }
            else if(resultsTrue == 11){
                int i;
                for(i = 0; i < 12; i++){
                    if(test[i] == 0){
                        break;
                    }
                }
                cout << "Missed by one statement: " << st[i] << endl;
                cout << "\t" << output << endl;
            }
        }
    }
}

```

```txt

1. Missed by one statement:  8. If statement 7 is true, then 5 and 6 are both true.
    1:T  2:F  3:F  4:T  5:F  6:F  7:F  8:F  9:F  10:F  11:F  12:F
2. Missed by one statement:  8. If statement 7 is true, then 5 and 6 are both true.
    1:T  2:F  3:F  4:F  5:T  6:F  7:F  8:F  9:F  10:F  11:F  12:F
3. Missed by one statement:  11. Exactly 1 of statements 7, 8 and 9 are true.
    1:T  2:F  3:F  4:F  5:T  6:F  7:F  8:T  9:F  10:F  11:F  12:F
4. Missed by one statement:  9. Exactly 3 of the first 6 statements are true.
    1:T  2:F  3:T  4:T  5:F  6:T  7:T  8:F  9:T  10:F  11:F  12:F
5. Missed by one statement:  7. Either statement 2 or 3 is true, but not both.
    1:T  2:F  3:T  4:T  5:F  6:F  7:F  8:T  9:T  10:F  11:F  12:F
6. Missed by one statement:  6. Exactly 4 of the odd-numbered statements are true.
    1:T  2:F  3:F  4:T  5:F  6:T  7:F  8:T  9:T  10:F  11:F  12:F
7. Missed by one statement:  8. If statement 7 is true, then 5 and 6 are both true.
    1:T  2:T  3:F  4:T  5:F  6:F  7:T  8:T  9:T  10:F  11:F  12:F
8. Missed by one statement:  10. The next two statements are both true.
    1:T  2:T  3:F  4:T  5:F  6:F  7:T  8:F  9:T  10:T  11:F  12:F
9. Full Match, good solution!
    1:T  2:F  3:T  4:T  5:F  6:T  7:T  8:F  9:F  10:F  11:T  12:F
10. Missed by one statement:  1. This is a numbered list of twelve statements.
    1:F  2:F  3:F  4:F  5:T  6:F  7:F  8:T  9:F  10:F  11:T  12:F
11. Missed by one statement:  12. Exactly 4 of the preceding statements are true.
    1:T  2:F  3:F  4:F  5:T  6:F  7:F  8:T  9:F  10:F  11:T  12:F
12. Missed by one statement:  8. If statement 7 is true, then 5 and 6 are both true.
    1:T  2:F  3:F  4:F  5:T  6:T  7:F  8:F  9:T  10:F  11:T  12:F
13. Missed by one statement:  12. Exactly 4 of the preceding statements are true.
    1:T  2:T  3:F  4:T  5:F  6:F  7:T  8:F  9:T  10:F  11:F  12:T
14. Missed by one statement:  1. This is a numbered list of twelve statements.
    1:F  2:F  3:F  4:T  5:F  6:F  7:F  8:T  9:F  10:T  11:T  12:T
15. Missed by one statement:  12. Exactly 4 of the preceding statements are true.
    1:T  2:F  3:F  4:T  5:F  6:F  7:F  8:T  9:F  10:T  11:T  12:T
16. Missed by one statement:  1. This is a numbered list of twelve statements.
    1:F  2:F  3:F  4:F  5:T  6:F  7:F  8:T  9:F  10:T  11:T  12:T
17. Missed by one statement:  12. Exactly 4 of the preceding statements are true.
    1:T  2:F  3:F  4:F  5:T  6:F  7:F  8:T  9:F  10:T  11:T  12:T

```



## D


```d
import std.stdio, std.algorithm, std.range, std.functional;

immutable texts = [
    "this is a numbered list of twelve statements",
    "exactly 3 of the last 6 statements are true",
    "exactly 2 of the even-numbered statements are true",
    "if statement 5 is true, then statements 6 and 7 are both true",
    "the 3 preceding statements are all false",
    "exactly 4 of the odd-numbered statements are true",
    "either statement 2 or 3 is true, but not both",
    "if statement 7 is true, then 5 and 6 are both true",
    "exactly 3 of the first 6 statements are true",
    "the next two statements are both true",
    "exactly 1 of statements 7, 8 and 9 are true",
    "exactly 4 of the preceding statements are true"];

immutable pure @safe @nogc bool function(in bool[])[12] predicates = [
    s => s.length == 12,
    s => s[$ - 6 .. $].sum == 3,
    s => s.dropOne.stride(2).sum == 2,
    s => s[4] ? (s[5] && s[6]) : true,
    s => s[1 .. 4].sum == 0,
    s => s.stride(2).sum == 4,
    s => s[1 .. 3].sum == 1,
    s => s[6] ? (s[4] && s[5]) : true,
    s => s[0 .. 6].sum == 3,
    s => s[10] && s[11],
    s => s[6 .. 9].sum == 1,
    s => s[0 .. 11].sum == 4];

void main() @safe {
    enum nStats = predicates.length;

    foreach (immutable n; 0 .. 2 ^^ nStats) {
        bool[nStats] st, matches;
        nStats.iota.map!(i => !!(n & (2 ^^ i))).copy(st[]);
        st[].zip(predicates[].map!(f => f(st)))
            .map!(s_t => s_t[0] == s_t[1]).copy(matches[]);
        if (matches[].sum >= nStats - 1) {
            if (matches[].all)
                ">>> Solution:".writeln;
            else
                writefln("Missed by statement: %d",
                         matches[].countUntil(false) + 1);
            writefln("%-(%s %)", st[].map!q{ "FT"[a] });
        }
    }
}
```

```txt
Missed by statement: 8
T F F T F F F F F F F F
Missed by statement: 8
T F F F T F F F F F F F
Missed by statement: 11
T F F F T F F T F F F F
Missed by statement: 9
T F T T F T T F T F F F
Missed by statement: 7
T F T T F F F T T F F F
Missed by statement: 6
T F F T F T F T T F F F
Missed by statement: 8
T T F T F F T T T F F F
Missed by statement: 10
T T F T F F T F T T F F
>>> Solution:
T F T T F T T F F F T F
Missed by statement: 1
F F F F T F F T F F T F
Missed by statement: 12
T F F F T F F T F F T F
Missed by statement: 8
T F F F T T F F T F T F
Missed by statement: 12
T T F T F F T F T F F T
Missed by statement: 1
F F F T F F F T F T T T
Missed by statement: 12
T F F T F F F T F T T T
Missed by statement: 1
F F F F T F F T F T T T
Missed by statement: 12
T F F F T F F T F T T T
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			-- Possible solutions.
		do
			create s.make_filled (False, 1, 12)
			s [1] := True
			recurseAll (2)
			io.put_string (counter.out + " solution found. ")
		end

feature {NONE}

	s: ARRAY [BOOLEAN]

	check2: BOOLEAN
			-- Is statement 2 fulfilled?
		local
			count: INTEGER
		do
			across
				7 |..| 12 as c
			loop
				if s [c.item] then
					count := count + 1
				end
			end
			Result := s [2] = (count = 3)
		end

	check3: BOOLEAN
			-- Is statement 3 fulfilled?
		local
			count, i: INTEGER
		do
			from
				i := 2
			until
				i > 12
			loop
				if s [i] then
					count := count + 1
				end
				i := i + 2
			end
			Result := s [3] = (count = 2)
		end

	check4: BOOLEAN
			-- Is statement 4 fulfilled?
		do
			Result := s [4] = ((not s [5]) or (s [6] and s [7]))
		end

	check5: BOOLEAN
			-- Is statement 5 fulfilled?
		do
			Result := s [5] = ((not s [2]) and (not s [3]) and (not s [4]))
		end

	check6: BOOLEAN
			-- Is statement 6 fulfilled?
		local
			count, i: INTEGER
		do
			from
				i := 1
			until
				i > 11
			loop
				if s [i] then
					count := count + 1
				end
				i := i + 2
			end
			Result := s [6] = (count = 4)
		end

	check7: BOOLEAN
			-- Is statement 7 fulfilled?
		do
			Result := s [7] = ((s [2] or s [3]) and not (s [2] and s [3]))
		end

	check8: BOOLEAN
			-- Is statement 8 fulfilled?
		do
			Result := s [8] = (not s [7] or (s [5] and s [6]))
		end

	check9: BOOLEAN
			-- Is statement 9 fulfilled?
		local
			count: INTEGER
		do
			across
				1 |..| 6 as c
			loop
				if s [c.item] then
					count := count + 1
				end
			end
			Result := s [9] = (count = 3)
		end

	check10: BOOLEAN
			-- Is statement 10 fulfilled?
		do
			Result := s [10] = (s [11] and s [12])
		end

	check11: BOOLEAN
			-- Is statement 11 fulfilled?
		local
			count: INTEGER
		do
			across
				7 |..| 9 as c
			loop
				if s [c.item] then
					count := count + 1
				end
			end
			Result := s [11] = (count = 1)
		end

	check12: BOOLEAN
			-- Is statement 12 fulfilled?
		local
			count: INTEGER
		do
			across
				1 |..| 11 as c
			loop
				if s [c.item] then
					count := count + 1
				end
			end
			Result := (s [12] = (count = 4))
		end

	counter: INTEGER

	checkit
			-- Check if all statements are correctly solved.
		do
			if check2 and check3 and check4 and check5 and check6 and check7 and check8 and check9 and check10 and check11 and check12 then
				across
					1 |..| 12 as c
				loop
					if s [c.item] then
						io.put_string (c.item.out + "%T")
					end
				end
				io.new_line
				counter := counter + 1
			end
		end

	recurseAll (k: INTEGER)
			-- All possible True and False combinations to check for a solution.
		do
			if k = 13 then
				checkit
			else
				s [k] := False
				recurseAll (k + 1)
				s [k] := True
				recurseAll (k + 1)
			end
		end

end

```

```txt

1    3    4    6    7    11
1 solution found.

```


## Elena

ELENA 4.1 :

```elena
import system'routines;
import extensions;
import extensions'text;

extension op
{
    printSolution(bits)
        = self.zipBy(bits,
            (s,b => s.iif("T","F") + (s.xor:b).iif("* ","  "))).summarize(new StringWriter());

    toBit()
        = self.iif(1,0);
}

puzzle = new Func1[]::
(
    (bits => bits.Length == 12),

    (bits => bits.last(6).selectBy:(x => x.toBit()).summarize() == 3 ),

    (bits => bits.zipBy(new Range(1, 12),
                        (x,i => (i.toInt().isEven()).and:x.toBit())).summarize() == 2 ),

    (bits => bits[4].iif(bits[5] && bits[6],true) ),

    (bits => ((bits[1] || bits[2]) || bits[3]).Inverted ),

    (bits => bits.zipBy(new Range(1, 12),
                        (x,i => (i.toInt().isOdd()).and:x.toBit() )).summarize() == 4 ),

    (bits => bits[1].xor(bits[2]) ),

    (bits => bits[6].iif(bits[5] && bits[4],true) ),

    (bits => bits.top(6).selectBy:(x => x.toBit() ).summarize() == 3 ),

    (bits => bits[10] && bits[11] ),

    (bits => (bits[6].toBit() + bits[7].toBit() + bits[8].toBit())==1 ),

    (bits => bits.top(11).selectBy:(x => x.toBit()).summarize() == 4 )
);

public program()
{
    console.writeLine();

    for(int n := 0, n < 2.power(12), n += 1)
    {
        var bits := BitArray32.load(n).top(12).toArray();
        var results := puzzle.selectBy:(r => r(bits)).toArray();

        var counts := bits.zipBy(results, (b,r => b.xor:r.toBit() )).summarize();

        counts =>
            0  { console.printLine("Total hit :",results.printSolution:bits) }
            1  { console.printLine("Near miss :",results.printSolution:bits) }
            12 { console.printLine("Total miss:",results.printSolution:bits) };
    };

    console.readChar()
}
```

```txt

Near miss :T  F  F  T  F  F  F  T* F  F  F  F
Near miss :T  F  F  F  T  F  F  T* F  F  F  F
Near miss :T  F  F  F  T  F  F  T  F  F  T* F
Near miss :T  F  T  T  F  T  T  F  F* F  F  F
Near miss :T  F  T  T  F  F  T* T  T  F  F  F
Near miss :T  F  F  T  F  F* F  T  T  F  F  F
Near miss :T  T  F  T  F  F  T  F* T  F  F  F
Near miss :T  T  F  T  F  F  T  F  T  F* F  F
Total hit :T  F  T  T  F  T  T  F  F  F  T  F
Near miss :T* F  F  F  T  F  F  T  F  F  T  F
Near miss :T  F  F  F  T  F  F  T  F  F  T  T*
Near miss :T  F  F  F  T  T  F  T* T  F  T  F
Near miss :T  T  F  T  F  F  T  F  T  F  F  F*
Total miss:T* T* F* F* F* F* T* T* F* F* T* F*
Total miss:T* F* F* T* F* F* F* T* F* T* F* F*
Near miss :T* F  F  T  F  F  F  T  F  T  T  T
Near miss :T  F  F  T  F  F  F  T  F  T  T  F*
Near miss :T* F  F  F  T  F  F  T  F  T  T  T
Near miss :T  F  F  F  T  F  F  T  F  T  T  F*

```



## ERRE


```ERRE

PROGRAM TWELVE_STMS

!$DYNAMIC
DIM PASS%[0],T%[0]

FUNCTION EOR(X,Y)
    EOR=(X AND NOT(Y)) OR (NOT(X) AND Y)
END FUNCTION

BEGIN
      NSTATEMENTS%=12
      !$DIM PASS%[NSTATEMENTS%],T%[NSTATEMENTS%]

      FOR TRY%=0 TO 2^NSTATEMENTS%-1 DO

        ! Postulate answer:
        FOR STMT%=1 TO 12 DO
          T%[STMT%]=(TRY% AND 2^(STMT%-1))<>0
        END FOR

        ! Test consistency:
        PASS%[1]=T%[1]=(NSTATEMENTS%=12)
        PASS%[2]=T%[2]=((T%[7]+T%[8]+T%[9]+T%[10]+T%[11]+T%[12])=-3)
        PASS%[3]=T%[3]=((T%[2]+T%[4]+T%[6]+T%[8]+T%[10]+T%[12])=-2)
        PASS%[4]=T%[4]=((NOT T%[5] OR (T%[6] AND T%[7])))
        PASS%[5]=T%[5]=(NOT T%[2] AND NOT T%[3] AND NOT T%[4])
        PASS%[6]=T%[6]=((T%[1]+T%[3]+T%[5]+T%[7]+T%[9]+T%[11])=-4)
        PASS%[7]=T%[7]=(EOR(T%[2],T%[3]))
        PASS%[8]=T%[8]=((NOT T%[7] OR (T%[5] AND T%[6])))
        PASS%[9]=T%[9]=((T%[1]+T%[2]+T%[3]+T%[4]+T%[5]+T%[6])=-3)
        PASS%[10]=T%[10]=(T%[11] AND T%[12])
        PASS%[11]=T%[11]=((T%[7]+T%[8]+T%[9])=-1)
        PASS%[12]=T%[12]=((T%[1]+T%[2]+T%[3]+T%[4]+T%[5]+T%[6]+T%[7]+T%[8]+T%[9]+T%[10]+T%[11])=-4)

        SUM=0
        FOR I%=1 TO 12 DO
           SUM=SUM+PASS%[I%]
        END FOR

        CASE SUM OF
          -11->
            PRINT("Near miss with statements ";)
            FOR STMT%=1 TO 12 DO
              IF T%[STMT%] THEN PRINT(STMT%;) END IF
              IF NOT PASS%[STMT%] THEN MISS%=STMT% END IF
            END FOR
            PRINT("true (failed ";MISS%;").")
          END ->
          -12->
            PRINT("Solution! with statements ";)
            FOR STMT%=1 TO 12 DO
              IF T%[STMT%] THEN PRINT(STMT%;) END IF
            END FOR
            PRINT("true.")
          END ->
        END CASE

      END FOR ! TRY%
END PROGRAM
```

```txt

Near miss with statements  1  4 true (failed  8 ).
Near miss with statements  1  5 true (failed  8 ).
Near miss with statements  1  5  8 true (failed  11 ).
Near miss with statements  1  3  4  6  7  9 true (failed  9 ).
Near miss with statements  1  3  4  8  9 true (failed  7 ).
Near miss with statements  1  4  6  8  9 true (failed  6 ).
Near miss with statements  1  2  4  7  8  9 true (failed  8 ).
Near miss with statements  1  2  4  7  9  10 true (failed  10 ).
Solution! with statements  1  3  4  6  7  11 true.
Near miss with statements  5  8  11 true (failed  1 ).
Near miss with statements  1  5  8  11 true (failed  12 ).
Near miss with statements  1  5  6  9  11 true (failed  8 ).
Near miss with statements  1  2  4  7  9  12 true (failed  12 ).
Near miss with statements  4  8  10  11  12 true (failed  1 ).
Near miss with statements  1  4  8  10  11  12 true (failed  12 ).
Near miss with statements  5  8  10  11  12 true (failed  1 ).
Near miss with statements  1  5  8  10  11  12 true (failed  12 ).

```



## Forth

Forth is excellently suited to solve this, because it has excellent support for manipulating bitpatterns.

```forth
: lastbit                              ( n1 -- n2)
  dup if 1 swap begin dup 1 <> while swap 1+ swap 1 rshift repeat drop then
;

: bit 1 swap lshift and 0<> ;          ( n1 n2 -- f)
: bitcount 0 swap begin dup while dup 1- and swap 1+ swap repeat drop ;

12 constant #stat                      \ number of statements
                                       \ encoding of the statements
: s1 >r #stat 12 = r> 0 bit = ;        \ heavy use of binary
: s2 >r r@ 4032 and bitcount 3 = r> 1 bit = ;
: s3 >r r@ 2730 and bitcount 2 = r> 2 bit = ;
: s4 >r r@ 4 bit 0= 96 r@ over and = or r> 3 bit = ;
: s5 >r r@ 14 and 0= r> 4 bit = ;
: s6 >r r@ 1365 and bitcount 4 = r> 5 bit = ;
: s7 >r r@ 1 bit r@ 2 bit xor r> 6 bit = ;
: s8 >r r@ 6 bit 0= 48 r@ over and = or r> 7 bit = ;
: s9 >r r@ 63 and bitcount 3 = r> 8 bit = ;
: s10 >r 3072 r@ over and = r> 9 bit = ;
: s11 >r r@ 448 and bitcount 1 = r> 10 bit = ;
: s12 >r r@ 2047 and bitcount 4 = r> 11 bit = ;
: list #stat 0 do dup i bit if i 1+ . then loop drop ;

: nearmiss?                            \ do we have a near miss?
  over #stat 1- = if                   ( true-pattern #true stat-pattern)
    ." Near miss with statements " dup list ." true (failed "
    >r over invert 1 #stat lshift 1- and lastbit 0 .r ." )" cr r>
  then                                 \ extract the failed statement
;
                                       \ have we found a solution?
: solution?                            ( true-pattern #true stat-pattern)
  over #stat = if ." Solution! with statements " dup list ." true." cr then
;

: 12statements                         \ test the twelve patterns
  1 #stat lshift 0 do                  \ create another bit pattern
    i s12   2* i s11 + 2* i s10 + 2* i s9 + 2* i s8 + 2* i s7 + 2*
    i s6  + 2* i s5  + 2* i s4  + 2* i s3 + 2* i s2 + 2* i s1 +
    abs dup bitcount i solution? nearmiss? drop drop drop
  loop                                 \ count number of bytes and evaluate
;

12statements
```

```txt

Near miss with statements 1 4 true (failed 8)
Near miss with statements 1 5 true (failed 8)
Near miss with statements 1 5 8 true (failed 11)
Near miss with statements 1 3 4 6 7 9 true (failed 9)
Near miss with statements 1 3 4 8 9 true (failed 7)
Near miss with statements 1 4 6 8 9 true (failed 6)
Near miss with statements 1 2 4 7 8 9 true (failed 8)
Near miss with statements 1 2 4 7 9 10 true (failed 10)
Solution! with statements 1 3 4 6 7 11 true.
Near miss with statements 5 8 11 true (failed 1)
Near miss with statements 1 5 8 11 true (failed 12)
Near miss with statements 1 5 6 9 11 true (failed 8)
Near miss with statements 1 2 4 7 9 12 true (failed 12)
Near miss with statements 4 8 10 11 12 true (failed 1)
Near miss with statements 1 4 8 10 11 12 true (failed 12)
Near miss with statements 5 8 10 11 12 true (failed 1)
Near miss with statements 1 5 8 10 11 12 true (failed 12)
 ok

```


## Go


```go
package main

import "fmt"

// its' not too much more work to check all the permutations concurrently
var solution = make(chan int)
var nearMiss = make(chan int)
var done = make(chan bool)

func main() {
    // iterate and use the bits as the permutation
    for i := 0; i < 4096; i++ {
        go checkPerm(i)
    }
    // collect the misses and list them after the complete solution(s)
    var ms []int
    for i := 0; i < 4096; {
        select {
        case <-done:
            i++
        case s := <-solution:
            print12("solution", s)
        case m := <-nearMiss:
            ms = append(ms, m)
        }
    }
    for _, m := range ms {
        print12("near miss", m)
    }
}

func print12(label string, bits int) {
    fmt.Print(label, ":")
    for i := 1; i <= 12; i++ {
        if bits&1 == 1 {
            fmt.Print(" ", i)
        }
        bits >>= 1
    }
    fmt.Println()
}

func checkPerm(tz int) {
    // closure returns true if tz bit corresponding to
    // 1-based statement number is 1.
    ts := func(n uint) bool {
        return tz>>(n-1)&1 == 1
    }
    // variadic closure returns number of statements listed as arguments
    // which have corresponding tz bit == 1.
    ntrue := func(xs ...uint) int {
        nt := 0
        for _, x := range xs {
            if ts(x) {
                nt++
            }
        }
        return nt
    }
    // a flag used on repeated calls to test.
    // set to true when first contradiction is found.
    // if another is found, this function (checkPerm) can "short circuit"
    // and return immediately without checking additional statements.
    var con bool
    // closure called to test each statement
    test := func(statement uint, b bool) {
        switch {
        case ts(statement) == b:
        case con:
            panic("bail")
        default:
            con = true
        }
    }
    // short circuit mechanism
    defer func() {
        if x := recover(); x != nil {
            if msg, ok := x.(string); !ok && msg != "bail" {
                panic(x)
            }
        }
        done <- true
    }()

    // 1. This is a numbered list of twelve statements.
    test(1, true)

    // 2. Exactly 3 of the last 6 statements are true.
    test(2, ntrue(7, 8, 9, 10, 11, 12) == 3)

    // 3. Exactly 2 of the even-numbered statements are true.
    test(3, ntrue(2, 4, 6, 8, 10, 12) == 2)

    // 4. If statement 5 is true, then statements 6 and 7 are both true.
    test(4, !ts(5) || ts(6) && ts(7))

    // 5. The 3 preceding statements are all false.
    test(5, !ts(4) && !ts(3) && !ts(2))

    // 6. Exactly 4 of the odd-numbered statements are true.
    test(6, ntrue(1, 3, 5, 7, 9, 11) == 4)

    // 7. Either statement 2 or 3 is true, but not both.
    test(7, ts(2) != ts(3))

    // 8. If statement 7 is true, then 5 and 6 are both true.
    test(8, !ts(7) || ts(5) && ts(6))

    // 9. Exactly 3 of the first 6 statements are true.
    test(9, ntrue(1, 2, 3, 4, 5, 6) == 3)

    // 10. The next two statements are both true.
    test(10, ts(11) && ts(12))

    // 11. Exactly 1 of statements 7, 8 and 9 are true.
    test(11, ntrue(7, 8, 9) == 1)

    // 12. Exactly 4 of the preceding statements are true.
    test(12, ntrue(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11) == 4)

    // no short circuit? send permutation as either near miss or solution
    if con {
        nearMiss <- tz
    } else {
        solution <- tz
    }
}
```

```txt

solution: 1 3 4 6 7 11
near miss: 1 4
near miss: 1 5
near miss: 1 5 8
near miss: 1 3 4 6 7 9
near miss: 1 3 4 8 9
near miss: 1 4 6 8 9
near miss: 1 2 4 7 8 9
near miss: 1 2 4 7 9 10
near miss: 5 8 11
near miss: 1 5 8 11
near miss: 1 5 6 9 11
near miss: 1 2 4 7 9 12
near miss: 1 4 8 10 11 12
near miss: 4 8 10 11 12
near miss: 5 8 10 11 12
near miss: 1 5 8 10 11 12

```



## Groovy

Solution:

```groovy
enum Rule {
    r01( 1, { r()*.num == (1..12) }),
    r02( 2, { r(7..12).count { it.truth } == 3 }),
    r03( 3, { r(2..12, 2).count { it.truth } == 2 }),
    r04( 4, { r(5).truth ? r(6).truth && r(7).truth : true }),
    r05( 5, { r(2..4).count { it.truth } == 0 }),
    r06( 6, { r(1..11, 2).count { it.truth } == 4 }),
    r07( 7, { r(2).truth != r(3).truth }),
    r08( 8, { r(7).truth ? r(5).truth && r(6).truth : true }),
    r09( 9, { r(1..6).count { it.truth } == 3 }),
    r10(10, { r(11).truth && r(12).truth }),
    r11(11, { r(7..9).count { it.truth } == 1 }),
    r12(12, { r(1..11).count { it.truth } == 4 });

    final int num
    final Closure statement
    boolean truth

    static final List<Rule> rules = [ null, r01, r02, r03, r04, r05, r06, r07, r08, r09, r10, r11, r12]

    private Rule(num, statement) {
        this.num = num
        this.statement = statement
    }

    public static Rule       r(int index) { rules[index] }
    public static List<Rule> r() { rules[1..12] }
    public static List<Rule> r(List<Integer> indices) { rules[indices] }
    public static List<Rule> r(IntRange indices) { rules[indices] }
    public static List<Rule> r(IntRange indices, int step) { r(indices.step(step)) }

    public static void setAllTruth(int bits) {
        (1..12).each { r(it).truth = !(bits & (1 << (12 - it))) }
    }

    public static void evaluate() {
        def nearMisses = [:]
        (0..<(2**12)).each { i ->
            setAllTruth(i)
            def truthCandidates = r().findAll { it.truth }
            def truthMatchCount = r().count { it.statement() == it.truth }
            if (truthMatchCount == 12) {
                println ">Solution< ${truthCandidates*.num}"
            } else if (truthMatchCount == 11) {
                def miss = (1..12).find { r(it).statement() != r(it).truth }
                nearMisses << [(truthCandidates): miss]
            }
        }
        nearMisses.each { truths, miss ->
            printf ("Near Miss: %-21s (failed %2d)\n", "${truths*.num}", miss)
        }
    }
}

Rule.evaluate()
```


```txt
>Solution< [1, 3, 4, 6, 7, 11]
Near Miss: [1, 2, 4, 7, 8, 9]    (failed  8)
Near Miss: [1, 2, 4, 7, 9, 10]   (failed 10)
Near Miss: [1, 2, 4, 7, 9, 12]   (failed 12)
Near Miss: [1, 3, 4, 6, 7, 9]    (failed  9)
Near Miss: [1, 3, 4, 8, 9]       (failed  7)
Near Miss: [1, 4, 6, 8, 9]       (failed  6)
Near Miss: [1, 4, 8, 10, 11, 12] (failed 12)
Near Miss: [1, 4]                (failed  8)
Near Miss: [1, 5, 6, 9, 11]      (failed  8)
Near Miss: [1, 5, 8, 10, 11, 12] (failed 12)
Near Miss: [1, 5, 8, 11]         (failed 12)
Near Miss: [1, 5, 8]             (failed 11)
Near Miss: [1, 5]                (failed  8)
Near Miss: [4, 8, 10, 11, 12]    (failed  1)
Near Miss: [5, 8, 10, 11, 12]    (failed  1)
Near Miss: [5, 8, 11]            (failed  1)
```




## Haskell

Shows answers with 1 for true, followed by list of indices of contradicting elements in each set of 1/0s (index is 0-based).


```haskell
import Data.List (findIndices)

tf = mapM (\_ -> [1,0])

wrongness b = findIndices id . zipWith (/=) b . map (fromEnum . ($ b))

statements = [	(==12) . length,
		3 ⊂ [length statements-6..],
		2 ⊂ [1,3..],
		4 → [4..6],
		0 ⊂ [1..3],
		4 ⊂ [0,2..],
		1 ⊂ [1,2],
		6 → [4..6],
		3 ⊂ [0..5],
		2 ⊂ [10,11],
		1 ⊂ [6,7,8],
		4 ⊂ [0..10]
	] where
	(s ⊂ x) b = s == (sum . map (b!!) . takeWhile (< length b)) x
	(a → x) b = (b!!a == 0) || all ((==1).(b!!)) x

testall s n = [(b, w) | b <- tf s, w <- [wrongness b s], length w == n]

main = let t = testall statements in do
	putStrLn "Answer"
	mapM_ print $ t 0
	putStrLn "Near misses"
	mapM_ print $ t 1
```

```txt

Answer
([1,0,1,1,0,1,1,0,0,0,1,0],[])
Near misses
([1,1,0,1,0,0,1,1,1,0,0,0],[7])
([1,1,0,1,0,0,1,0,1,1,0,0],[9])
([1,1,0,1,0,0,1,0,1,0,0,1],[11])
([1,0,1,1,0,1,1,0,1,0,0,0],[8])
([1,0,1,1,0,0,0,1,1,0,0,0],[6])
([1,0,0,1,0,1,0,1,1,0,0,0],[5])
([1,0,0,1,0,0,0,1,0,1,1,1],[11])
([1,0,0,1,0,0,0,0,0,0,0,0],[7])
([1,0,0,0,1,1,0,0,1,0,1,0],[7])
([1,0,0,0,1,0,0,1,0,1,1,1],[11])
([1,0,0,0,1,0,0,1,0,0,1,0],[11])
([1,0,0,0,1,0,0,1,0,0,0,0],[10])
([1,0,0,0,1,0,0,0,0,0,0,0],[7])
([0,0,0,1,0,0,0,1,0,1,1,1],[0])
([0,0,0,0,1,0,0,1,0,1,1,1],[0])
([0,0,0,0,1,0,0,1,0,0,1,0],[0])

```



## J

In the following 'apply' is the foreign conjunction:

```j
   apply
128!:2

NB. example
   '*:' apply 1 2 3
1 4 9
```

This enables us to apply strings (left argument) being verbs to the right argument, mostly a noun.

```j
S=: <;._2 (0 :0)
 12&=@#                   NB. 1.  This is a numbered list of twelve statements.
 3=+/@:{.~&_6             NB. 2.  Exactly 3 of the last 6 statements are true.
 2= +/@:{~&1 3 5 7 9 11   NB. 3.  Exactly 2 of the even-numbered statements are true.
 4&{=*./@:{~&4 5 6        NB. 4.  If statement 5 is true, then statements 6 and 7 are both true.
 0=+/@:{~&1 2 3           NB. 5.  The 3 preceding statements are all false.
 4=+/@:{~&0 2 4 6 8 10    NB. 6.  Exactly 4 of the odd-numbered statements are true.
 1=+/@:{~&1 2             NB. 7.  Either statement 2 or 3 is true, but not both.
 6&{=*./@:{~&4 5 6        NB. 8.  If statement 7 is true, then 5 and 6 are both true.
 3=+/@:{.~&6              NB. 9.  Exactly 3 of the first 6 statements are true.
 2=+/@:{~&10 11           NB. 10. The next two statements are both true.
 1=+/@:{~&6 7 8           NB. 11. Exactly 1 of statements 7, 8 and 9 are true.
 4=+/@:{.~&11             NB. 12. Exactly 4 of the preceding statements are true.
)

testall=: (];"1 0<@I.@:(]~:(apply&><))"1) #:@i.@(2&^)@#
```


The output follows the Haskell convention: true/false bitstring followed by the index of a contradiction

'''All true'''

```j
   (#~0=#@{::~&_1"1) testall S
┌───────────────────────┬┐
│1 0 1 1 0 1 1 0 0 0 1 0││
└───────────────────────┴┘
```

Or, numerically:

```j
   1+I.;(#~0=#@{::~&_1"1) testall S
1 3 4 6 7 11
```


'''Near misses'''

```j
   (#~1=#@{::~&_1"1) testall S
┌───────────────────────┬──┐
│0 0 0 0 1 0 0 1 0 0 1 0│0 │
├───────────────────────┼──┤
│0 0 0 0 1 0 0 1 0 1 1 1│0 │
├───────────────────────┼──┤
│0 0 0 1 0 0 0 1 0 1 1 1│0 │
├───────────────────────┼──┤
│1 0 0 0 1 0 0 0 0 0 0 0│7 │
├───────────────────────┼──┤
│1 0 0 0 1 0 0 1 0 0 0 0│10│
├───────────────────────┼──┤
│1 0 0 0 1 0 0 1 0 0 1 0│11│
├───────────────────────┼──┤
│1 0 0 0 1 0 0 1 0 1 1 1│11│
├───────────────────────┼──┤
│1 0 0 0 1 1 0 0 1 0 1 0│7 │
├───────────────────────┼──┤
│1 0 0 1 0 0 0 0 0 0 0 0│7 │
├───────────────────────┼──┤
│1 0 0 1 0 0 0 1 0 1 1 1│11│
├───────────────────────┼──┤
│1 0 0 1 0 1 0 1 1 0 0 0│5 │
├───────────────────────┼──┤
│1 0 1 1 0 0 0 1 1 0 0 0│6 │
├───────────────────────┼──┤
│1 0 1 1 0 1 1 0 1 0 0 0│8 │
├───────────────────────┼──┤
│1 1 0 1 0 0 1 0 1 0 0 1│11│
├───────────────────────┼──┤
│1 1 0 1 0 0 1 0 1 1 0 0│9 │
├───────────────────────┼──┤
│1 1 0 1 0 0 1 1 1 0 0 0│7 │
└───────────────────────┴──┘
```


'''Iterative for all true'''

A repeat while true approach:  x f^:(p)^:_ y

```j
   (-N)&{. #: S <:@]^:((]-.@-:(apply&><)"1) (-N)&{.@#:@])^:(_) 2^N=.#S
1 0 1 1 0 1 1 0 0 0 1 0
```


Here is an alternative representation of the statements which might be slightly easier to read. (The behavior is identical):


```j
true=:1 :'(m-1)&{'

S=: <;._2 (0 :0)
 12 = #                   NB. 1.  This is a numbered list of twelve statements.
 3 (= +/) _6&{.           NB. 2.  Exactly 3 of the last 6 statements are true.
 2 (= +/) (12$0 1)&#      NB. 3.  Exactly 2 of the even-numbered statements are true.
 5 true (<: */) 6 7 true  NB. 4.  If statement 5 is true, then statements 6 and 7 are both true.
 0 (= +/) 2 3 4 true      NB. 5.  The 3 preceding statements are all false.
 4 (= +/) (12$1 0)&#      NB. 6.  Exactly 4 of the odd-numbered statements are true.
 1 (= +/) 2 3 true        NB. 7.  Either statement 2 or 3 is true, but not both.
 7 true (<: */) 5 6 true  NB. 8.  If statement 7 is true, then 5 and 6 are both true.
 3 (= +/) 6&{.            NB. 9.  Exactly 3 of the first 6 statements are true.
 */@(11 12 true)          NB. 10. The next two statements are both true.
 1 (= +/) 7 8 9 true      NB. 11. Exactly 1 of statements 7, 8 and 9 are true.
 4 (= +/) }:              NB. 12. Exactly 4 of the preceding statements are true.
)
```


And here is an approach which does not use the verb apply, but instead mostly relies on simple arithmetic.


```j
'sum not mask'=: |:".;._2(0 :0)
   0; 0; 0 0 0 0 0 0 0 0 0 0 0 0   NB. 1.  This is a numbered list of twelve statements.
   3; 0; 0 0 0 0 0 0 1 1 1 1 1 1   NB. 2.  Exactly 3 of the last 6 statements are true.
   2; 0; 0 1 0 1 0 1 0 1 0 1 0 1   NB. 3.  Exactly 2 of the even-numbered statements are true.
   2; 5; 0 0 0 0 0 1 1 0 0 0 0 0   NB. 4.  If statement 5 is true, then statements 6 and 7 are both true.
   0; 0; 0 1 1 1 0 0 0 0 0 0 0 0   NB. 5.  The 3 preceding statements are all false.
   4; 0; 1 0 1 0 1 0 1 0 1 0 1 0   NB. 6.  Exactly 4 of the odd-numbered statements are true.
   1; 0; 0 1 1 0 0 0 0 0 0 0 0 0   NB. 7.  Either statement 2 or 3 is true, but not both.
   2; 7; 0 0 0 0 1 1 0 0 0 0 0 0   NB. 8.  If statement 7 is true, then 5 and 6 are both true.
   3; 0; 1 1 1 1 1 1 0 0 0 0 0 0   NB. 9.  Exactly 3 of the first 6 statements are true.
   2; 0; 0 0 0 0 0 0 0 0 0 0 1 1   NB. 10. The next two statements are both true.
   1; 0; 0 0 0 0 0 0 1 1 1 0 0 0   NB. 11. Exactly 1 of statements 7, 8 and 9 are true.
   4; 0; 1 1 1 1 1 1 1 1 1 1 1 0   NB. 12. Exactly 4 of the preceding statements are true.
)
propositions=: |:#:i.2^#sum

errors=: propositions~:(1 - not { 1,propositions) >. sum = mask +/ .*propositions
```


Now, as before, we can find the consistent set of true and false values:


```J
   #:I.0=+/errors
1 0 1 1 0 1 1 0 0 0 1 0
   1+I.#:I.0=+/errors   NB. true propositions for the consistent case
1 3 4 6 7 11
```


And, we can find the set which is inconsistent for only one proposition:


```J
   offby1=: 1=+/errors
   'Statement ',"1 (":1+I.|: offby1 #"1 errors),"1 ' is inconsistent with exactly ',"1 ((1":@:+I.)"1 #:I.offby1),"1 ' being true'
Statement  1 is inconsistent with exactly 5 8 11         being true
Statement  1 is inconsistent with exactly 5 8 10 11 12   being true
Statement  1 is inconsistent with exactly 4 8 10 11 12   being true
Statement  8 is inconsistent with exactly 1 5            being true
Statement 11 is inconsistent with exactly 1 5 8          being true
Statement 12 is inconsistent with exactly 1 5 8 11       being true
Statement 12 is inconsistent with exactly 1 5 8 10 11 12 being true
Statement  8 is inconsistent with exactly 1 5 6 9 11     being true
Statement  8 is inconsistent with exactly 1 4            being true
Statement 12 is inconsistent with exactly 1 4 8 10 11 12 being true
Statement  6 is inconsistent with exactly 1 4 6 8 9      being true
Statement  7 is inconsistent with exactly 1 3 4 8 9      being true
Statement  9 is inconsistent with exactly 1 3 4 6 7 9    being true
Statement 12 is inconsistent with exactly 1 2 4 7 9 12   being true
Statement 10 is inconsistent with exactly 1 2 4 7 9 10   being true
Statement  8 is inconsistent with exactly 1 2 4 7 8 9    being true
```



## Java


The following Java code uses brute force. It tries to translate the logical statements as naturally as possible. The run time is almost zero.


```Java

public class LogicPuzzle
{
    boolean S[] = new boolean[13];
    int Count = 0;

    public boolean check2 ()
    {
        int count = 0;
        for (int k = 7; k <= 12; k++)
            if (S[k]) count++;
        return S[2] == (count == 3);
    }

    public boolean check3 ()
    {
        int count = 0;
        for (int k = 2; k <= 12; k += 2)
            if (S[k]) count++;
        return S[3] == (count == 2);
    }

    public boolean check4 ()
    {
        return S[4] == ( !S[5] || S[6] && S[7]);
    }

    public boolean check5 ()
    {
        return S[5] == ( !S[2] && !S[3] && !S[4]);
    }

    public boolean check6 ()
    {
        int count = 0;
        for (int k = 1; k <= 11; k += 2)
            if (S[k]) count++;
        return S[6] == (count == 4);
    }

    public boolean check7 ()
    {
        return S[7] == ((S[2] || S[3]) && !(S[2] && S[3]));
    }

    public boolean check8 ()
    {
        return S[8] == ( !S[7] || S[5] && S[6]);
    }

    public boolean check9 ()
    {
        int count = 0;
        for (int k = 1; k <= 6; k++)
            if (S[k]) count++;
        return S[9] == (count == 3);
    }

    public boolean check10 ()
    {
        return S[10] == (S[11] && S[12]);
    }

    public boolean check11 ()
    {
        int count = 0;
        for (int k = 7; k <= 9; k++)
            if (S[k]) count++;
        return S[11] == (count == 1);
    }

    public boolean check12 ()
    {
        int count = 0;
        for (int k = 1; k <= 11; k++)
            if (S[k]) count++;
        return S[12] == (count == 4);
    }

    public void check ()
    {
        if (check2() && check3() && check4() && check5() && check6()
            && check7() && check8() && check9() && check10() && check11()
            && check12())
        {
            for (int k = 1; k <= 12; k++)
                if (S[k]) System.out.print(k + " ");
            System.out.println();
            Count++;
        }
    }

    public void recurseAll (int k)
    {
        if (k == 13)
            check();
        else
        {
            S[k] = false;
            recurseAll(k + 1);
            S[k] = true;
            recurseAll(k + 1);
        }
    }

    public static void main (String args[])
    {
        LogicPuzzle P = new LogicPuzzle();
        P.S[1] = true;
        P.recurseAll(2);
        System.out.println();
        System.out.println(P.Count + " Solutions found.");
    }
}

```

```txt

1 3 4 6 7 11

1 Solutions found.

```



## jq

In this section, we use a brute-force strategy, mainly for the sake
of comparability with many of the other solutions on this page, but
also because it requires only 2^12 tests -- or 2^11 since the truth
of the first statement is manifest.  It is worth noting, however,
that an alternative strategy would be to include some of the
constraints inside the generator.

(The truth or falsity of the first statement is not completely "logical" because it requires some kind of inspection that the statements are numbered. It is reasonable, however, to interpret (1) to mean that there are 12 statements.)

One interesting aspect of the following jq program is the
helper function, indexed(filter): it obviates the need here not only
for a specific "select every nth item" filter, but also for a
generic "with_index" annotator.

```jq
def indexed(filter):
  . as $in
  | reduce range(0;length) as $i ([]; if ($i | filter) then . + [$in[$i]] else . end);

def count(value): map(select(. == value)) | length;

# The truth or falsity of the 12 statements can be captured in an array of size 12:
def generate(k):
  if k == 1 then [true], [false]
  else generate(1) + generate(k-1)
  end;

# Input: a boolean array
def evaluate:
  [ (length == 12),                                          #1
    ((.[6:] | count(true)) == 3),                            #2
    ((indexed(. % 2 == 1) | count(true)) == 2),              #3
    (if .[4] then .[5] and .[6] else true end),              #4
    ((.[1:4] | count(false)) == 3),                          #5
    ((indexed(. % 2 == 0) | count(true)) == 4),              #6
    (([.[1], .[2]] | count(true)) == 1),                     #7
    (if .[6] then .[4] and .[5] else true end),              #8
    ((.[0:6] | count(true)) == 3),                           #9
    (.[10] and .[11]),                                      #10
    ((.[6:9] | count(true)) == 1),                          #11
    ((.[0:11] | count(true)) == 4)                          #12
  ];

# The following query generates the solution to the problem:
# generate(12) | . as $vector | if evaluate == $vector then $vector else empty end

# Running "task" as defined next would generate
# both the general solution as well as the off-by-one solutions:

def task:

  # count agreements
  def agreed(x;y): reduce range(0;x|length) as $i (0; if x[$i] == y[$i] then .+1 else . end);

  reduce generate(12) as $vector
    ([]; ($vector | evaluate) as $e
         | agreed($vector; $e) as $agreed
         | if $agreed == 12 then [[12,$vector]] + .
           elif $agreed == 11 then . +  [[11, $vector]]
           else .
           end);

# Since the solutions have been given elsewhere, we simply count the
# number of exact and off-by-one solutions:

task | length
```

 $ jq -M -n -f Twelve_statements.jq
 17


## Julia

This task involves only 12 statements, so an exhaustive search of the 2^12 possible statement value combinations is quite feasible.  The program shows "total misses" and the distribution of numbers of hits in addition to solutions and near misses.

```Julia

function showflaggedbits{T<:BitArray{1}}(a::T, f::T)
    tf = map(x->x ? "T" : "F", a)
    flg = map(x->x ? "*" : " ", f)
    join(tf .* flg, " ")
end

const props = [s -> length(s) == 12,
               s -> sum(s[7:12]) == 3,
               s -> sum(s[2:2:end]) == 2,
               s -> !s[5] || (s[6] & s[7]),
               s -> !any(s[2:4]),
               s -> sum(s[1:2:end]) == 4,
               s -> s[2] $ s[3],
               s -> !s[7] || (s[5] & s[6]),
               s -> sum(s[1:6]) == 3,
               s -> s[11] & s[12],
               s -> sum(s[7:9]) == 1,
               s -> sum(s[1:end-1]) == 4]

const NDIG = length(props)
NDIG < WORD_SIZE || println("WARNING, too many propositions!")

mhist = zeros(Int, NDIG+1)

println("Checking the ", NDIG, " statements against all possibilities.\n")
print(" "^15)
for i in 1:NDIG
    print(@sprintf "%3d" i)
end
println()

for i in 0:(2^NDIG-1)
    s = bitpack(digits(i, 2, NDIG))
    t = bitpack([p(s) for p in props])
    misses = s$t
    mcnt = sum(misses)
    mhist[NDIG-mcnt+1] += 1
    mcnt < 2 || mcnt == NDIG || continue
    if mcnt == 0
        print("    Exact Match: ")
    elseif mcnt == NDIG
        print("     Total Miss: ")
    else
        print("      Near Miss: ")
    end
    println(showflaggedbits(t, misses))
end

println()
println("Distribution of matches")
println(" Matches  Cases")
for i in (NDIG+1):-1:1
    println(@sprintf "    %2d => %4d" i-1 mhist[i])
end

```


```txt

Checking the 12 statements against all possibilities.

                 1  2  3  4  5  6  7  8  9 10 11 12
      Near Miss: T  F  F  T  F  F  F  T* F  F  F  F
      Near Miss: T  F  F  F  T  F  F  T* F  F  F  F
      Near Miss: T  F  F  F  T  F  F  T  F  F  T* F
      Near Miss: T  F  T  T  F  T  T  F  F* F  F  F
      Near Miss: T  F  T  T  F  F  T* T  T  F  F  F
      Near Miss: T  F  F  T  F  F* F  T  T  F  F  F
      Near Miss: T  T  F  T  F  F  T  F* T  F  F  F
      Near Miss: T  T  F  T  F  F  T  F  T  F* F  F
    Exact Match: T  F  T  T  F  T  T  F  F  F  T  F
      Near Miss: T* F  F  F  T  F  F  T  F  F  T  F
      Near Miss: T  F  F  F  T  F  F  T  F  F  T  T*
      Near Miss: T  F  F  F  T  T  F  T* T  F  T  F
      Near Miss: T  T  F  T  F  F  T  F  T  F  F  F*
     Total Miss: T* T* F* F* F* F* T* T* F* F* T* F*
     Total Miss: T* F* F* T* F* F* F* T* F* T* F* F*
      Near Miss: T* F  F  T  F  F  F  T  F  T  T  T
      Near Miss: T  F  F  T  F  F  F  T  F  T  T  F*
      Near Miss: T* F  F  F  T  F  F  T  F  T  T  T
      Near Miss: T  F  F  F  T  F  F  T  F  T  T  F*

Distribution of matches
 Matches  Cases
    12 =>    1
    11 =>   16
    10 =>   65
     9 =>  236
     8 =>  488
     7 =>  781
     6 =>  909
     5 =>  791
     4 =>  514
     3 =>  205
     2 =>   75
     1 =>   13
     0 =>    2

```



## Kotlin


```scala
// version 1.1.3

typealias Predicate = (String) -> Boolean

val predicates = listOf<Predicate>(
    { it.length == 13 },  // indexing starts at 0 but first bit ignored
    { (7..12).count { i -> it[i] == '1' } == 3 },
    { (2..12 step 2).count { i -> it[i] == '1' } == 2 },
    { it[5] == '0' || (it[6] == '1' && it[7] == '1') },
    { it[2] == '0' && it[3]  == '0' && it[4] == '0' },
    { (1..11 step 2).count { i -> it[i] == '1' } == 4 },
    { (it[2] == '1') xor (it[3] == '1') },
    { it[7] == '0' || (it[5] == '1' && it[6] == '1') },
    { (1..6).count { i -> it[i] == '1' } == 3 },
    { it[11] == '1' && it[12] == '1' },
    { (7..9).count { i -> it[i] == '1' } == 1 },
    { (1..11).count { i -> it[i] == '1' } == 4 }
)

fun show(s: String, indent: Boolean) {
    if (indent) print("    ")
    for (i in s.indices) if (s[i] == '1') print("$i ")
    println()
}

fun main(args: Array<String>) {
    println("Exact hits:")
    for (i in 0..4095) {
        val s = i.toString(2).padStart(13, '0')
        var j = 1
        if (predicates.all { it(s) == (s[j++] == '1') }) show(s, true)
    }

    println("\nNear misses:")
    for (i in 0..4095) {
        val s = i.toString(2).padStart(13, '0')
        var j = 1
        if (predicates.count { it(s) == (s[j++] == '1') } == 11) {
            var k = 1
            val iof = predicates.indexOfFirst { it(s) != (s[k++] == '1') } + 1
            print("    (Fails at statement ${"%2d".format(iof)})  ")
            show(s, false)
        }
    }
}
```


```txt

Exact hits:
    1 3 4 6 7 11

Near misses:
    (Fails at statement  1)  5 8 11
    (Fails at statement  1)  5 8 10 11 12
    (Fails at statement  1)  4 8 10 11 12
    (Fails at statement  8)  1 5
    (Fails at statement 11)  1 5 8
    (Fails at statement 12)  1 5 8 11
    (Fails at statement 12)  1 5 8 10 11 12
    (Fails at statement  8)  1 5 6 9 11
    (Fails at statement  8)  1 4
    (Fails at statement 12)  1 4 8 10 11 12
    (Fails at statement  6)  1 4 6 8 9
    (Fails at statement  7)  1 3 4 8 9
    (Fails at statement  9)  1 3 4 6 7 9
    (Fails at statement 12)  1 2 4 7 9 12
    (Fails at statement 10)  1 2 4 7 9 10
    (Fails at statement  8)  1 2 4 7 8 9

```



## Mathematica


```mathematica
Print["Answer:\n", Column@Cases[#, {s_, 0} :> s], "\nNear misses:\n",
   Column@Cases[#, {s_, 1} :> s]] &[{#,
    Count[Boole /@ {Length@# == 12, Total@#[[7 ;;]] == 3,
        Total@#[[2 ;; 12 ;; 2]] == 2, #[[5]] (#[[6]] + #[[7]] - 2) ==
         0, Total@#[[2 ;; 4]] == 0,
        Total@#[[1 ;; 11 ;; 2]] == 4, #[[2]] + #[[3]] ==
         1, #[[7]] (#[[5]] + #[[6]] - 2) == 0,
        Total@#[[;; 6]] == 3, #[[11]] + #[[12]] == 2,
        Total@#[[7 ;; 9]] == 1, Total@#[[;; 11]] == 4} - #,
     Except[0]]} & /@ Tuples[{1, 0}, 12]]
```


```txt
Answer:
{1,0,1,1,0,1,1,0,0,0,1,0}


Near misses:
{1,1,0,1,0,0,1,1,1,0,0,0}
{1,1,0,1,0,0,1,0,1,1,0,0}
{1,1,0,1,0,0,1,0,1,0,0,1}
{1,0,1,1,0,1,1,0,1,0,0,0}
{1,0,1,1,0,0,0,1,1,0,0,0}
{1,0,0,1,0,1,0,1,1,0,0,0}
{1,0,0,1,0,0,0,1,0,1,1,1}
{1,0,0,1,0,0,0,0,0,0,0,0}
{1,0,0,0,1,1,0,0,1,0,1,0}
{1,0,0,0,1,0,0,1,0,1,1,1}
{1,0,0,0,1,0,0,1,0,0,1,0}
{1,0,0,0,1,0,0,1,0,0,0,0}
{1,0,0,0,1,0,0,0,0,0,0,0}
{0,0,0,1,0,0,0,1,0,1,1,1}
{0,0,0,0,1,0,0,1,0,1,1,1}
{0,0,0,0,1,0,0,1,0,0,1,0}
```



## Pascal

Inspired by the C++ implementation, this version makes extensive use of Pascal's built-in set handling capabilities.


```pascal
PROGRAM TwelveStatements;

{
  This program searches through the 4095 possible sets
  of 12 statements for any which may be self-consistent.
}

CONST
    max12b = 4095; { Largest 12 byte number. }

TYPE
    statnum = 1..12;  { statement numbers }
    statset = set of statnum; { sets of statements }

VAR { global variables for use in main algorithm }
    trialNumber: integer;
    trialSet, testResults: statset;

function Convert(n: integer): statset;
{
  Converts an integer into a set of statements.
  For each "1" in the last 12 bits of
  the integer's binary representation,
  a statement number is put into the set.
}
var
    i: statnum;
    s: statset;
begin
    s := []; { Empty set. }
    for i := 12 downto 1 do begin
        if (n mod 2) = 1 then s := s + [i];
        n := n div 2
    end;
    Convert := s
end;

procedure Express(truths: statset);
{
  Writes the statement number of each "truth",
  with at least one space in front,
  all on one line.
}
var n: statnum;
begin
    for n := 1 to 12 do
     if n in truths then write(n:3);
    writeln
end;

function Count(truths: statset): integer;
{ Counts the statement numbers in the set. }
var
    s: statnum;
    i: integer;
begin
    i := 0;
    for s := 1 to 12 do if s in truths then i := i + 1;
    Count := i
end;

function Test(truths: statset): statset;
{
  Starts with a set of supposedly true statements
  and checks which of the 12 statements can actually
  be confirmed about the set itself.
}
var
    evens, odds, confirmations: statset;
begin
    evens := [2, 4, 6, 8, 10, 12];
    odds := [1, 3, 5, 7, 9, 11];

    { Statement 1 is necessarily true. }
    confirmations := [1];

    { Statement 2 }
    if Count(truths * [7..12]) = 3
     then confirmations := confirmations + [2];

    { Statement 3 }
    if Count(truths * evens) = 2
     then confirmations := confirmations + [3];

    { Statement 4 is true if 6 and 7 are true, or if 5 is false. }
    if ([6, 7] <= truths) or not (5 in truths)
     then confirmations := confirmations + [4];

    { Statement 5 }
    if [2, 3, 4] <= truths
     then confirmations := confirmations + [5];

    { Statement 6 }
    if Count(truths * odds) = 4
     then confirmations := confirmations + [6];

    { Statement 7 }
    if (2 in truths) xor (3 in truths)
     then confirmations := confirmations + [7];

    { Statement 8 is true if 5 and 6 are true, or if 7 is false. }
    if ([5, 6] <= truths) or not (7 in truths)
     then confirmations := confirmations + [8];

    { Statement 9 }
    if Count(truths * [1..6]) = 3
     then confirmations := confirmations + [9];

    { Statement 10 }
    if [11, 12] <= truths
     then confirmations := confirmations + [10];

    { Statement 11 }
    if Count(truths * [7, 8, 9]) = 1
     then confirmations := confirmations + [11];

    { Statement 12 }
    if Count(truths - [12]) = 4
     then confirmations := confirmations + [12];

    Test := confirmations
end;

BEGIN  { Main algorithm. }
    for trialNumber := 1 to max12b do begin
        trialSet := Convert(trialNumber);
        testResults := Test(trialSet);
        if testResults = trialSet then Express(trialSet)
    end;
    writeln('Done. Press ENTER.');
    readln
END.
```


```txt
1 3 4 6 7 11
Done. Press ENTER.
```



## Perl


```perl
use List::Util 'sum';

my @condition = (
                 sub { 0 }, # dummy sub for index 0
                 sub { 13==@_ },
                 sub { 3==sum @_[7..12] },
                 sub { 2==sum @_[2,4,6,8,10,12] },
                 sub { $_[5] ? ($_[6] and $_[7]) : 1 },
                 sub { !$_[2] and !$_[3] and !$_[4] },
                 sub { 4==sum @_[1,3,5,7,9,11] },
                 sub { $_[2]==1-$_[3] },
                 sub { $_[7] ? ($_[5] and $_[6]) : 1 },
                 sub { 3==sum @_[1..6] },
                 sub { 2==sum @_[11..12] },
                 sub { 1==sum @_[7,8,9] },
                 sub { 4==sum @_[1..11] },
                );

sub miss {
  return grep { $condition[$_]->(@_) != $_[$_] } 1..12;
}

for (0..2**12-1) {
  my @truth = split //, sprintf "0%012b", $_;
  my @no = miss @truth;
  print "Solution: true statements are ", join( " ", grep { $truth[$_] } 1..12), "\n" if 0 == @no;
  print "1 miss (",$no[0],"): true statements are ", join( " ", grep { $truth[$_] } 1..12), "\n" if 1 == @no;
}

```

```txt
1 miss (1): true statements are 5 8 11
1 miss (1): true statements are 5 8 10 11 12
1 miss (1): true statements are 4 8 10 11 12
1 miss (8): true statements are 1 5
1 miss (11): true statements are 1 5 8
1 miss (12): true statements are 1 5 8 11
1 miss (12): true statements are 1 5 8 10 11 12
1 miss (8): true statements are 1 5 6 9 11
1 miss (8): true statements are 1 4
1 miss (12): true statements are 1 4 8 10 11 12
1 miss (6): true statements are 1 4 6 8 9
1 miss (7): true statements are 1 3 4 8 9
Solution: true statements are 1 3 4 6 7 11
1 miss (9): true statements are 1 3 4 6 7 9
1 miss (12): true statements are 1 2 4 7 9 12
1 miss (10): true statements are 1 2 4 7 9 10
1 miss (8): true statements are 1 2 4 7 8 9

```



## Perl 6

```perl6
sub infix:<→> ($protasis, $apodosis) { !$protasis or $apodosis }

my @tests =
    { .end == 12 and all(.[1..12]) === any(True, False) },
    { 3 == [+] .[7..12] },
    { 2 == [+] .[2,4...12] },
    { .[5] → all .[6,7] },
    { none .[2,3,4] },
    { 4 == [+] .[1,3...11] },
    { one .[2,3] },
    { .[7] → all .[5,6] },
    { 3 == [+] .[1..6] },
    { all .[11,12] },
    { one .[7,8,9] },
    { 4 == [+] .[1..11] },
;

my @solutions;
my @misses;

for [X] (True, False) xx 12 {
    my @assert = Nil, |$_;
    my @result = Nil, |@tests.map({ ?.(@assert) });
    my @true = @assert.grep(?*, :k);
    my @cons = (@assert Z=== @result).grep(!*, :k);
    given @cons {
        when 0 { push @solutions, "<{@true}> is consistent."; }
        when 1 { push @misses, "<{@true}> implies { "¬" if !@result[~$_] }$_." }
    }
}

.say for @solutions;
say "";
say "Near misses:";
.say for @misses;
```


```txt
<1 3 4 6 7 11> is consistent.

Near misses:
<1 2 4 7 8 9> implies ¬8.
<1 2 4 7 9 10> implies ¬10.
<1 2 4 7 9 12> implies ¬12.
<1 3 4 6 7 9> implies ¬9.
<1 3 4 8 9> implies 7.
<1 4 6 8 9> implies ¬6.
<1 4 8 10 11 12> implies ¬12.
<1 4> implies 8.
<1 5 6 9 11> implies 8.
<1 5 8 10 11 12> implies ¬12.
<1 5 8 11> implies 12.
<1 5 8> implies 11.
<1 5> implies 8.
<4 8 10 11 12> implies 1.
<5 8 10 11 12> implies 1.
<5 8 11> implies 1.
```



## Phix


```Phix
string s    -- (eg "101101100010")
integer t   -- scratch

function s1() return length(s)=12 end function
function s2() t=0 for i=7 to 12 do t+=s[i]='1' end for return t=3 end function
function s3() t=0 for i=2 to 12 by 2 do t+=s[i]='1' end for return t=2 end function
function s4() return s[5]='0' or (s[6]='1' and s[7]='1') end function
function s5() return s[2]='0' and s[3]='0' and s[4]='0' end function
function s6() t=0 for i=1 to 12 by 2 do t+=s[i]='1' end for return t=4 end function
function s7() return s[2]!=s[3] end function
function s8() return s[7]='0' or (s[5]='1' and s[6]='1') end function
function s9() t=0 for i=1 to 6 do t+=s[i]='1' end for return t=3 end function
function s10() return s[11]='1' and s[12]='1' end function
function s11() t=0 for i=7 to 9 do t+=s[i]='1' end for return t=1 end function
function s12() t=0 for i=1 to 11 do t+=s[i]='1' end for return t=4 end function

sequence r = repeat(0,12)
for b=1 to 12 do
    r[b] = routine_id(sprintf("s%d",b))
end for
for i=0 to power(2,12)-1 do
    s = sprintf("%012b",i)
    for b=1 to 12 do
        if call_func(r[b],{})!=(s[b]='1') then exit end if
        if b=12 then ?s end if
    end for
end for
```

```txt

"101101100010"

```



## Prolog

Works with '''SWI-Prolog''' and '''library(clpfd)'''.

```Prolog
puzzle :-
        % 1. This is a numbered list of twelve statements.
	L = [A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12],
	L ins 0..1,
	element(1, L, 1),

       % 2.  Exactly 3 of the last 6 statements are true.
	A2 #<==>  A7 + A8 + A9 + A10 + A11 + A12 #= 3,

       % 3.  Exactly 2 of the even-numbered statements are true.
	A3 #<==> A2 + A4 + A6 + A8 + A10 + A12 #= 2,

       % 4.  If statement 5 is true, then statements 6 and 7 are both true.
	A4 #<==> (A5 #==> (A6 #/\ A7)),

       % 5.  The 3 preceding statements are all false.
	A5 #<==> A2 + A3 + A4 #= 0,

       % 6.  Exactly 4 of the odd-numbered statements are true.
	A6 #==> A1 + A3 + A5 + A7 + A9 + A11 #= 4,

        % 7.  Either statement 2 or 3 is true, but not both.
	A7 #<==> A2 + A3 #= 1,

        % 8.  If statement 7 is true, then 5 and 6 are both true.
	A8 #<==> (A7 #==>  A5 #/\ A6),


        % 9.  Exactly 3 of the first 6 statements are true.
	A9 #<==> A1 + A2 + A3 + A4 + A5 + A6 #= 3,

        % 10.  The next two statements are both true.
	A10 #<==> A11 #/\ A12,

        % 11.  Exactly 1 of statements 7, 8 and 9 are true.
	A11 #<==> A7 + A8 + A9 #= 1,

        % 12.  Exactly 4 of the preceding statements are true.
	A12 #<==> A1 + A2 + A3 + A4 + A5 + A6 + A7 +A8 + A9 + A10 + A11 #= 4,

	label(L),
        numlist(1, 12, NL),
	write('Statements '),
	maplist(my_write, NL, L),
	writeln('are true').


my_write(N, 1) :-
	format('~w ', [N]).

my_write(_N, 0).

```

```txt
 ?- puzzle.
Statements 1 3 4 6 7 11 are true
true .
```



## Python

Note: we choose to adapt the statement numbering to zero-based indexing in the constraintinfo lambda expressions but convert back to one-based on output.

The program uses brute force to generate all possible boolean values of the twelve statements,
then checks if the actual value of the statements matches the proposed or matches apart from exactly one deviation.
Python's boolean type <tt>bool</tt>is a subclass of <tt>int</tt>, so boolean values True, False can be used as integers (1, 0, respectively) in numerical contexts.
This fact is used in the lambda expressions that use function sum.

```python

from itertools import product
#from pprint import pprint as pp

constraintinfo = (
  (lambda st: len(st) == 12                 ,(1, 'This is a numbered list of twelve statements')),
  (lambda st: sum(st[-6:]) == 3             ,(2, 'Exactly 3 of the last 6 statements are true')),
  (lambda st: sum(st[1::2]) == 2            ,(3, 'Exactly 2 of the even-numbered statements are true')),
  (lambda st: (st[5]&st[6]) if st[4] else 1 ,(4, 'If statement 5 is true, then statements 6 and 7 are both true')),
  (lambda st: sum(st[1:4]) == 0             ,(5, 'The 3 preceding statements are all false')),
  (lambda st: sum(st[0::2]) == 4            ,(6, 'Exactly 4 of the odd-numbered statements are true')),
  (lambda st: sum(st[1:3]) == 1             ,(7, 'Either statement 2 or 3 is true, but not both')),
  (lambda st: (st[4]&st[5]) if st[6] else 1 ,(8, 'If statement 7 is true, then 5 and 6 are both true')),
  (lambda st: sum(st[:6]) == 3              ,(9, 'Exactly 3 of the first 6 statements are true')),
  (lambda st: (st[10]&st[11])               ,(10, 'The next two statements are both true')),
  (lambda st: sum(st[6:9]) == 1             ,(11, 'Exactly 1 of statements 7, 8 and 9 are true')),
  (lambda st: sum(st[0:11]) == 4            ,(12, 'Exactly 4 of the preceding statements are true')),
)

def printer(st, matches):
    if False in matches:
        print('Missed by one statement: %i, %s' % docs[matches.index(False)])
    else:
        print('Full match:')
    print('  ' + ', '.join('%i:%s' % (i, 'T' if t else 'F') for i, t in enumerate(st, 1)))

funcs, docs = zip(*constraintinfo)

full, partial = [], []

for st in product( *([(False, True)] * 12) ):
    truths = [bool(func(st)) for func in funcs]
    matches = [s == t for s,t in zip(st, truths)]
    mcount = sum(matches)
    if mcount == 12:
        full.append((st, matches))
    elif mcount == 11:
        partial.append((st, matches))

for stm in full + partial:
    printer(*stm)
```


```txt
Full match:
  1:T, 2:F, 3:T, 4:T, 5:F, 6:T, 7:T, 8:F, 9:F, 10:F, 11:T, 12:F
Missed by one statement: 1, This is a numbered list of twelve statements:
  1:F, 2:F, 3:F, 4:F, 5:T, 6:F, 7:F, 8:T, 9:F, 10:F, 11:T, 12:F
Missed by one statement: 1, This is a numbered list of twelve statements:
  1:F, 2:F, 3:F, 4:F, 5:T, 6:F, 7:F, 8:T, 9:F, 10:T, 11:T, 12:T
Missed by one statement: 1, This is a numbered list of twelve statements:
  1:F, 2:F, 3:F, 4:T, 5:F, 6:F, 7:F, 8:T, 9:F, 10:T, 11:T, 12:T
Missed by one statement: 8, If statement 7 is true, then 5 and 6 are both true:
  1:T, 2:F, 3:F, 4:F, 5:T, 6:F, 7:F, 8:F, 9:F, 10:F, 11:F, 12:F
Missed by one statement: 11, Exactly 1 of statements 7, 8 and 9 are true:
  1:T, 2:F, 3:F, 4:F, 5:T, 6:F, 7:F, 8:T, 9:F, 10:F, 11:F, 12:F
Missed by one statement: 12, Exactly 4 of the preceding statements are true:
  1:T, 2:F, 3:F, 4:F, 5:T, 6:F, 7:F, 8:T, 9:F, 10:F, 11:T, 12:F
Missed by one statement: 12, Exactly 4 of the preceding statements are true:
  1:T, 2:F, 3:F, 4:F, 5:T, 6:F, 7:F, 8:T, 9:F, 10:T, 11:T, 12:T
Missed by one statement: 8, If statement 7 is true, then 5 and 6 are both true:
  1:T, 2:F, 3:F, 4:F, 5:T, 6:T, 7:F, 8:F, 9:T, 10:F, 11:T, 12:F
Missed by one statement: 8, If statement 7 is true, then 5 and 6 are both true:
  1:T, 2:F, 3:F, 4:T, 5:F, 6:F, 7:F, 8:F, 9:F, 10:F, 11:F, 12:F
Missed by one statement: 12, Exactly 4 of the preceding statements are true:
  1:T, 2:F, 3:F, 4:T, 5:F, 6:F, 7:F, 8:T, 9:F, 10:T, 11:T, 12:T
Missed by one statement: 6, Exactly 4 of the odd-numbered statements are true:
  1:T, 2:F, 3:F, 4:T, 5:F, 6:T, 7:F, 8:T, 9:T, 10:F, 11:F, 12:F
Missed by one statement: 7, Either statement 2 or 3 is true, but not both:
  1:T, 2:F, 3:T, 4:T, 5:F, 6:F, 7:F, 8:T, 9:T, 10:F, 11:F, 12:F
Missed by one statement: 9, Exactly 3 of the first 6 statements are true:
  1:T, 2:F, 3:T, 4:T, 5:F, 6:T, 7:T, 8:F, 9:T, 10:F, 11:F, 12:F
Missed by one statement: 12, Exactly 4 of the preceding statements are true:
  1:T, 2:T, 3:F, 4:T, 5:F, 6:F, 7:T, 8:F, 9:T, 10:F, 11:F, 12:T
Missed by one statement: 10, The next two statements are both true:
  1:T, 2:T, 3:F, 4:T, 5:F, 6:F, 7:T, 8:F, 9:T, 10:T, 11:F, 12:F
Missed by one statement: 8, If statement 7 is true, then 5 and 6 are both true:
  1:T, 2:T, 3:F, 4:T, 5:F, 6:F, 7:T, 8:T, 9:T, 10:F, 11:F, 12:F
```



## Racket


This question really begs to be done with <tt>amb</tt>


```Racket

#lang racket

;; A quick `amb' implementation
(define failures null)
(define (fail)
  (if (pair? failures) ((first failures)) (error "no more choices!")))
(define (amb/thunks choices)
  (let/cc k (set! failures (cons k failures)))
  (if (pair? choices)
    (let ([choice (first choices)]) (set! choices (rest choices)) (choice))
    (begin (set! failures (rest failures)) (fail))))
(define-syntax-rule (amb E ...) (amb/thunks (list (lambda () E) ...)))
(define (assert condition) (unless condition (fail)))

;; just to make things more fun
(define (⇔ x y) (assert (eq? x y)))
(require (only-in racket [and ∧] [or ∨] [implies ⇒] [xor ⊻] [not ¬]))
(define (count xs)
  (let loop ([n 0] [xs xs])
    (if (null? xs) n (loop (if (car xs) (add1 n) n) (cdr xs)))))
;; even more fun, make []s infix
(require (only-in racket [#%app r:app]))
(define-syntax (#%app stx)
  (if (not (eq? #\[ (syntax-property stx 'paren-shape)))
    (syntax-case stx () [(_ x ...) #'(r:app x ...)])
    (syntax-case stx ()
      ;; extreme hack on next two cases, so it works for macros too.
      [(_ x op y) (syntax-property #'(op x y) 'paren-shape #f)]
      [(_ x op y op1 z) (free-identifier=? #'op #'op1)
       (syntax-property #'(op x y z) 'paren-shape #f)])))
;; might as well do more
(define-syntax-rule (define-booleans all x ...)
  (begin (define x (amb #t #f)) ...
         (define all (list x ...))))

(define (puzzle)
  (define-booleans all q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12)
  ;; 1.  This is a numbered list of twelve statements.
  [q1 ⇔ [12 = (length all)]]
  ;; 2.  Exactly 3 of the last 6 statements are true.
  [q2 ⇔ [3 = (count (take-right all 6))]]
  ;; 3.  Exactly 2 of the even-numbered statements are true.
  [q3 ⇔ [2 = (count (list q2 q4 q6 q8 q10 q12))]]
  ;; 4.  If statement 5 is true, then statements 6 and 7 are both true.
  [q4 ⇔ [q5 ⇒ [q6 ∧ q7]]]
  ;; 5.  The 3 preceding statements are all false.
  [q5 ⇔ (¬ [q2 ∨ q3 ∨ q4])]
  ;; 6.  Exactly 4 of the odd-numbered statements are true.
  [q6 ⇔ [4 = (count (list q1 q3 q5 q7 q9 q11))]]
  ;; 7.  Either statement 2 or 3 is true, but not both.
  [q7 ⇔ [q2 ⊻ q3]]
  ;; 8.  If statement 7 is true, then 5 and 6 are both true.
  [q8 ⇔ [q7 ⇒ (and q5 q6)]]
  ;; 9.  Exactly 3 of the first 6 statements are true.
  [q9 ⇔ [3 = (count (take all 3))]]
  ;; 10. The next two statements are both true.
  [q10 ⇔ [q11 ∧ q12]]
  ;; 11. Exactly 1 of statements 7, 8 and 9 are true.
  [q11 ⇔ [1 = (count (list q7 q8 q9))]]
  ;; 12. Exactly 4 of the preceding statements are true.
  [q12 ⇔ [4 = (count (drop-right all 1))]]
  ;; done
  (for/list ([i (in-naturals 1)] [q all] #:when q) i))

(puzzle)
;; -> '(1 3 4 6 7 11)


```



## REXX


### generalized logic


```rexx
/*REXX program solves the  "Twelve Statement Puzzle".                                   */
q=12;      @stmt=right('statement',20)           /*number of statements in the puzzle.  */
m=0                                              /*[↓]  statement one is  TRUE  by fiat.*/
      do pass=1  for 2                           /*find the maximum number of  "trues". */
        do e=0   for 2**(q-1);    n = '1'right( x2b( d2x( e ) ),  q-1,  0)
              do b=1  for q                      /*define various bits in the number  Q.*/
              @.b=substr(n, b, 1)                /*define a particular  @  bit  (in  Q).*/
              end   /*b*/
        if @.1  then if yeses(1,  1)                        \==1  then iterate
        if @.2  then if yeses(7, 12)                        \==3  then iterate
        if @.3  then if yeses(2, 12,2)                      \==2  then iterate
        if @.4  then if yeses(5,  5)   then  if yeses(6, 7) \==2  then iterate
        if @.5  then if yeses(2,  4)                        \==0  then iterate
        if @.6  then if yeses(1, 12,2)                      \==4  then iterate
        if @.7  then if yeses(2,  3)                        \==1  then iterate
        if @.8  then if yeses(7,  7)   then  if yeses(5,6)  \==2  then iterate
        if @.9  then if yeses(1,  6)                        \==3  then iterate
        if @.10 then if yeses(11,12)                        \==2  then iterate
        if @.11 then if yeses(7,  9)                        \==1  then iterate
        if @.12 then if yeses(1, 11)                        \==4  then iterate
        g=yeses(1, 12)
        if pass==1  then do;  m=max(m,g);  iterate;  end
                    else if g\==m     then iterate
            do j=1  for q;             z=substr(n, j, 1)
            if z  then  say @stmt right(j, 2)    " is "    word('false true', 1 + z)
            end   /*tell*/
        end       /*e*/
      end         /*pass*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
yeses: parse arg L,H,B;  #=0;    do i=L  to H  by word(B 1, 1);  #=#+@.i;  end;   return #
```

'''output'''

```txt

           statement  1  is  true
           statement  3  is  true
           statement  4  is  true
           statement  6  is  true
           statement  7  is  true
           statement 11  is  true

```



### discrete logic


```rexx
/*REXX program solves the  "Twelve Statement Puzzle".                                   */
q=12;      @stmt=right('statement',20)           /*number of statements in the puzzle.  */
m=0                                              /*[↓]  statement one is  TRUE  by fiat.*/
      do pass=1  for 2                           /*find the maximum number of  "trues". */
        do e=0   for 2**(q-1);    n = '1'right( x2b( d2x( e ) ),  q-1,  0)
              do b=1  for q                      /*define various bits in the number  Q.*/
              @.b=substr(n, b, 1)                /*define a particular  @  bit  (in  Q).*/
              end   /*b*/
        if @.1  then   if \ @.1                             then iterate
        if @.2  then   if @.7+@.8+@.9+@.10+@.11+@.12  \==3  then iterate
        if @.3  then   if @.2+@.4+@.6+@.8+@.10+@.12   \==2  then iterate
        if @.4  then   if @.5  then if \(@.6 & @.7)         then iterate
        if @.5  then   if @.2  |  @.3  |  @.4               then iterate
        if @.6  then   if @.1+@.3+@.5+@.7+@.9+@.11    \==4  then iterate
        if @.7  then   if \ (@.2  &&  @.3 )                 then iterate
        if @.8  then   if @.7  then  if \(@.5 & @.6)        then iterate
        if @.9  then   if @.1+@.2+@.3+@.4+@.5+@.6     \==3  then iterate
        if @.10 then   if \ (@.11 & @.12)                   then iterate
        if @.11 then   if @.7+@.8+@.9                 \==1  then iterate
                          g=@.1 +@.2 +@.3 +@.4 +@.5 +@.6 +@.7 +@.8+ @.9 +@.10 +@.11
        if @.12 then   if g                           \==4  then iterate
        g=g + @.12
        if pass==1  then do;  m=max(m,g);  iterate;  end
                    else if g\==m     then iterate
            do j=1  for q;  z=substr(n, j, 1)
            if z  then  say @stmt right(j, 2)        " is "        word('false true', 1+z)
            end   /*tell*/
        end       /*e*/
      end         /*pass*/                       /*stick a fork in it,  we're all done. */
```

'''output'''   is the same as the 1<sup>st</sup> version.





### optimized


```rexx
/*REXX program solves the  "Twelve Statement Puzzle".                                   */
q=12;      @stmt=right('statement',20)           /*number of statements in the puzzle.  */
m=0                                              /*[↓]  statement one is  TRUE  by fiat.*/
      do pass=1  for 2                           /*find the maximum number of  "trues". */
        do e=0   for 2**(q-1);    n = '1'right( x2b( d2x( e ) ),  q-1,  0)
        parse var  n  @1 2 @2 3 @3 4 @4 5 @5 6 @6 7 @7 8 @8 9 @9 10 @10 11 @11 12 @12
/*▒▒▒▒  if @1  then   if \ @1                           then iterate  ▒▒▒▒*/
        if @2  then   if @7+@8+@9+@10+@11+@12  \==3     then iterate
        if @3  then   if @2+@4+@6+@8+@10+@12   \==2     then iterate
        if @4  then   if @5  then if \(@6 & @7)         then iterate
        if @5  then   if @2  |  @3  |  @4               then iterate
        if @6  then   if @1+@3+@5+@7+@9+@11    \==4     then iterate
        if @7  then   if \ (@2  &&  @3 )                then iterate
        if @8  then   if @7  then  if \(@5 & @6)        then iterate
        if @9  then   if @1+@2+@3+@4+@5+@6     \==3     then iterate
        if @10 then   if \ (@11 & @12)                  then iterate
        if @11 then   if @7+@8+@9              \==1     then iterate
                         g=@1 + @2 + @3 + @4 + @5 + @6 + @7 + @8 + @9 + @10 + @11
        if @12 then   if g                     \==4     then iterate
        g=g + @12
        if pass==1  then do;  m=max(m,g);  iterate;  end
                    else if g\==m     then iterate
            do j=1  for q;  z=substr(n, j, 1)
            if z  then say  @stmt right(j, 2)        " is "        word('false true', 1+z)
            end   /*j*/
        end       /*e*/
      end         /*pass*/                       /*stick a fork in it,  we're all done. */
```

'''output'''   is the same as the 1<sup>st</sup> version.





## Ruby


```ruby
constraints = [
  ->(st) { st.size == 12 },
  ->(st) { st.last(6).count(true) == 3 },
  ->(st) { st.each_slice(2).map(&:last).count(true) == 2 },
  ->(st) { st[4] ? (st[5] & st[6]) : true },
  ->(st) { st[1..3].none? },
  ->(st) { st.each_slice(2).map(&:first).count(true) == 4 },
  ->(st) { st[1] ^ st[2] },
  ->(st) { st[6] ? (st[4] & st[5]) : true  },
  ->(st) { st.first(6).count(true) == 3 },
  ->(st) { st[10] & st[11] },
  ->(st) { st[6..8].one? },
  ->(st) { st[0,11].count(true) == 4 },
]

Result = Struct.new(:truths, :consistency)

results = [true, false].repeated_permutation(12).map do |truths|
  Result.new(truths, constraints.zip(truths).map {|cn,truth| cn[truths] == truth })
end

puts "solution:",
  results.find {|r| r.consistency.all? }.truths.to_s

puts "\nnear misses: "
near_misses = results.select {|r| r.consistency.count(false) == 1 }
near_misses.each do |r|
  puts "missed by statement #{r.consistency.index(false) + 1}", r.truths.to_s
end
```


```txt

solution:
[true, false, true, true, false, true, true, false, false, false, true, false]

near misses:
missed by statement 8
[true, true, false, true, false, false, true, true, true, false, false, false]
missed by statement 10
[true, true, false, true, false, false, true, false, true, true, false, false]
missed by statement 12
[true, true, false, true, false, false, true, false, true, false, false, true]
missed by statement 9
[true, false, true, true, false, true, true, false, true, false, false, false]
missed by statement 7
[true, false, true, true, false, false, false, true, true, false, false, false]
missed by statement 6
[true, false, false, true, false, true, false, true, true, false, false, false]
missed by statement 12
[true, false, false, true, false, false, false, true, false, true, true, true]
missed by statement 8
[true, false, false, true, false, false, false, false, false, false, false, false]
missed by statement 8
[true, false, false, false, true, true, false, false, true, false, true, false]
missed by statement 12
[true, false, false, false, true, false, false, true, false, true, true, true]
missed by statement 12
[true, false, false, false, true, false, false, true, false, false, true, false]
missed by statement 11
[true, false, false, false, true, false, false, true, false, false, false, false]
missed by statement 8
[true, false, false, false, true, false, false, false, false, false, false, false]
missed by statement 1
[false, false, false, true, false, false, false, true, false, true, true, true]
missed by statement 1
[false, false, false, false, true, false, false, true, false, true, true, true]
missed by statement 1
[false, false, false, false, true, false, false, true, false, false, true, false]

```



## Scala

===Imperative Programming (Ugly)===
```Scala
class LogicPuzzle {
  val s = new Array[Boolean](13)
  var count = 0

  def check2: Boolean = {
    var count = 0
    for (k <- 7 to 12) if (s(k)) count += 1

    s(2) == (count == 3)
  }

  def check3: Boolean = {
    var count = 0
    for (k <- 2 to 12 by 2) if (s(k)) count += 1

    s(3) == (count == 2)
  }

  def check4: Boolean = s(4) == (!s(5) || s(6) && s(7))

  def check5: Boolean = s(5) == (!s(2) && !s(3) && !s(4))

  def check6: Boolean = {
    var count = 0
    for (k <- 1 to 11 by 2) if (s(k)) count += 1
    s(6) == (count == 4)
  }

  def check7: Boolean = s(7) == ((s(2) || s(3)) && !(s(2) && s(3)))

  def check8: Boolean = s(8) == (!s(7) || s(5) && s(6))

  def check9: Boolean = {
    var count = 0
    for (k <- 1 to 6) if (s(k)) count += 1

    s(9) == (count == 3)
  }

  def check10: Boolean = s(10) == (s(11) && s(12))

  def check11: Boolean = {
    var count = 0
    for (k <- 7 to 9) if (s(k)) count += 1

    s(11) == (count == 1)
  }

  def check12: Boolean = {
    var count = 0
    for (k <- 1 to 11) if (s(k)) count += 1
    s(12) == (count == 4)
  }

  def check(): Unit = {
    if (check2 && check3 && check4 && check5 && check6 && check7 && check8 && check9 && check10 && check11 && check12) {
      for (k <- 1 to 12) if (s(k)) print(k + " ")
      println()
      count += 1
    }
  }

  def recurseAll(k: Int): Unit = {
    if (k == 13) check()
    else {
      s(k) = false
      recurseAll(k + 1)
      s(k) = true
      recurseAll(k + 1)
    }
  }
}

object LogicPuzzle extends App {
  val p = new LogicPuzzle
  p.s(1) = true
  p.recurseAll(2)
  println()
  println(s"${p.count} Solutions found.")

}
```

{{Out}}See it in running in your browser by [https://scastie.scala-lang.org/XaRsz8gOQxavQ5rwMxi9ZA Scastie (JVM)] or
by [https://scalafiddle.io/sf/x73tnT6/1 ScalaFiddle (JavaScript)].


## Sidef

```ruby
var conditions = [
    { false },
    {|a| a.len == 13 },
    {|a| [a[7..12]].count(true) == 3 },
    {|a| [a[2..12 `by` 2]].count(true) == 2 },
    {|a| a[5] ? (a[6] && a[7]) : true },
    {|a| !a[2] && !a[3] && !a[4] },
    {|a| [a[1..11 `by` 2]].count(true) == 4 },
    {|a| a[2] == true^a[3] },
    {|a| a[7] ? (a[5] && a[6]) : true },
    {|a| [a[1..6]].count(true)  == 3 },
    {|a| [a[11,12]].count(true) == 2 },
    {|a| [a[7..9]].count(true)  == 1 },
    {|a| [a[1..11]].count(true) == 4 },
]

func miss(args) {
    1..12 -> grep {|i| conditions[i](args) != args[i] }
}

for k in (^(1<<12)) {
  var t  = ("0%012b" % k -> chars.map {|bit| bit == '1' })
  var no = miss(t)
  no.len == 0 && say "Solution: true statements are #{1..12->grep{t[_]}.join(' ')}"
  no.len == 1 && say "1 miss (#{no[0]}): true statements are #{1..12->grep{t[_]}.join(' ')}"
}
```

```txt

1 miss (1): true statements are 5 8 11
1 miss (1): true statements are 5 8 10 11 12
1 miss (1): true statements are 4 8 10 11 12
1 miss (8): true statements are 1 5
1 miss (11): true statements are 1 5 8
1 miss (12): true statements are 1 5 8 11
1 miss (12): true statements are 1 5 8 10 11 12
1 miss (8): true statements are 1 5 6 9 11
1 miss (8): true statements are 1 4
1 miss (12): true statements are 1 4 8 10 11 12
1 miss (6): true statements are 1 4 6 8 9
1 miss (7): true statements are 1 3 4 8 9
Solution: true statements are 1 3 4 6 7 11
1 miss (9): true statements are 1 3 4 6 7 9
1 miss (12): true statements are 1 2 4 7 9 12
1 miss (10): true statements are 1 2 4 7 9 10
1 miss (8): true statements are 1 2 4 7 8 9

```



## Swift

==={{trans|Java}}===

```Swift
var statements = Array(count: 13, repeatedValue: false)
statements[1] = true
var count = 0

func check2() -> Bool {
    var count = 0
    for (var k = 7; k <= 12; k++) {
        if (statements[k]) {
            count++
        }
    }
    return statements[2] == (count == 3)
}

func check3() -> Bool {
    var count = 0
    for (var k = 2; k <= 12; k += 2) {
        if (statements[k]) {
            count++
        }
    }
    return statements[3] == (count == 2)
}

func check4() -> Bool {
    return statements[4] == (!statements[5] || statements[6] && statements[7])
}

func check5() -> Bool {
    return statements[5] == (!statements[2] && !statements[3] && !statements[4])
}

func check6() -> Bool {
    var count = 0
    for (var k = 1; k <= 11; k += 2) {
        if (statements[k]) {
            count++
        }
    }
    return statements[6] == (count == 4)
}

func check7() -> Bool {
    return statements[7] == ((statements[2] || statements[3]) && !(statements[2] && statements[3]))
}

func check8() -> Bool {
    return statements[8] == ( !statements[7] || statements[5] && statements[6])
}

func check9() -> Bool {
    var count = 0
    for (var k = 1; k <= 6; k++) {
        if (statements[k]) {
            count++
        }
    }
    return statements[9] == (count == 3)
}

func check10() -> Bool {
    return statements[10] == (statements[11] && statements[12])
}

func check11() -> Bool {
    var count = 0
    for (var k = 7; k <= 9; k++) {
        if (statements[k]) {
            count++
        }
    }

    return statements[11] == (count == 1)
}

func check12() -> Bool {
    var count = 0
    for (var k = 1; k <= 11; k++) {
        if (statements[k]) {
            count++
        }
    }
    return statements[12] == (count == 4)
}

func check() {
    if (check2() && check3() && check4() && check5() && check6()
        && check7() && check8() && check9() && check10() && check11()
        && check12()) {
            for (var k = 1; k <= 12; k++) {
                if (statements[k]) {
                    print("\(k) ")
                }
            }
            println()
            count++
    }
}

func checkAll(k:Int) {
    if (k == 13) {
        check()
    } else {
        statements[k] = false
        checkAll(k + 1)
        statements[k] = true
        checkAll(k + 1)
    }
}

checkAll(2)
println()
println("\(count) solutions found")
```

```txt

1 3 4 6 7 11

1 solutions found
Program ended with exit code: 0

```


===Semi-functional solution===

```Swift
import Foundation

internal enum PaddingOption {
    case Left
    case Right
}

extension Array {
    func pad(element: Element, times: Int, toThe: PaddingOption) -> Array<Element> {
        let padded = [Element](count: times, repeatedValue: element)
        switch(toThe) {
        case .Left:
            return padded + self
        case .Right:
            return self + padded
        }
    }

    func take(n: Int) -> Array<Element> {
        if n <= 0 {
            return []
        }

        return Array(self[0..<Swift.min(n, self.count)])
    }

    func drop(n: Int) -> Array<Element> {
        if n <= 0 {
            return self
        } else if n >= self.count {
            return []
        }

        return Array(self[n..<self.count])
    }

    func stride(n: Int) -> Array<Element> {
        var result:[Element] = []
        for i in Swift.stride(from: 0, to: self.count, by: n) {
            result.append(self[i])
        }
        return result
    }

    func zipWithIndex() -> Array<(Element, Int)> {
        return [(Element, Int)](zip(self, indices(self)))
    }
}

extension Int {
    func binaryRepresentationOfLength(length: Int) -> [Int] {
        var binaryRepresentation:[Int] = []
        var value = self
        while (value != 0) {
            binaryRepresentation.append(value & 1)
            value /= 2
        }
        return binaryRepresentation.pad(0, times: length-binaryRepresentation.count, toThe: .Right).reverse()
    }
}

let problem = [
    "1.  This is a numbered list of twelve statements.",
    "2.  Exactly 3 of the last 6 statements are true.",
    "3.  Exactly 2 of the even-numbered statements are true.",
    "4.  If statement 5 is true, then statements 6 and 7 are both true.",
    "5.  The 3 preceding statements are all false.",
    "6.  Exactly 4 of the odd-numbered statements are true.",
    "7.  Either statement 2 or 3 is true, but not both.",
    "8.  If statement 7 is true, then 5 and 6 are both true.",
    "9.  Exactly 3 of the first 6 statements are true.",
    "10. The next two statements are both true.",
    "11. Exactly 1 of statements 7, 8 and 9 are true.",
    "12. Exactly 4 of the preceding statements are true."]

let statements:[([Bool] -> Bool)] = [
    { s in s.count == 12 },
    { s in s.drop(6).filter({ $0 }).count == 3 },
    { s in s.drop(1).stride(2).filter({ $0 }).count == 2 },
    { s in s[4] ? (s[5] && s[6]) : true },
    { s in s.drop(1).take(3).filter({ $0 }).count == 0 },
    { s in s.stride(2).filter({ $0 }).count == 4 },
    { s in [s[1], s[2]].filter({ $0 }).count == 1 },
    { s in s[6] ? (s[4] && s[5]) : true },
    { s in s.take(6).filter({ $0 }).count == 3 },
    { s in [s[10], s[11]].filter({ $0 }).count == 2 },
    { s in [s[6], s[7], s[8]].filter({ $0 }).count == 1 },
    { s in s.take(11).filter({ $0 }).count == 4 }
]

for variant in 0..<(1<<statements.count) {
    let attempt = variant.binaryRepresentationOfLength(statements.count).map { $0 == 1 }

    if statements.map({ $0(attempt) }) == attempt {
        let trueAre = attempt.zipWithIndex().filter { $0.0 }.map { $0.1 + 1 }
        println("Solution found! True are: \(trueAre)")
    }
}
```

```txt

Solution found! True are: [1, 3, 4, 6, 7, 11]

```



## Tcl

{{works with|Tcl|8.6}} <!-- but not 8.6b3; the lmap command post-dates that release -->

```tcl
package require Tcl 8.6

# Function to evaluate the truth of a statement
proc tcl::mathfunc::S {idx} {
    upvar 1 state s
    apply [lindex $s [expr {$idx - 1}]] $s
}
# Procedure to count the number of statements which are true
proc S+ args {
    upvar 1 state state
    tcl::mathop::+ {*}[lmap i $args {expr {S($i)}}]
}
# Turn a list of expressions into a list of lambda terms
proc lambdas items {lmap x $items {list state [list expr $x]}}

# Find the truth assignment that produces consistency. And those that are
# near misses too.
proc findTruthMatch {statements} {
    set n [llength $statements]
    for {set i 0} {$i < 2**$n} {incr i} {
	set state [split [format %0.*b $n $i] ""]
	set truths [lmap f $statements {apply $f [lambdas $state]}]
	set counteq [tcl::mathop::+ {*}[lmap s $state t $truths {expr {
	    $s == $t
	}}]]
	if {$counteq == $n} {
	    lappend exact $state
	} elseif {$counteq == $n-1} {
	    set j 0
	    foreach s $state t $truths {
		incr j
		if {$s != $t} {
		    lappend differ $state $j
		    break
		}
	    }
	}
    }
    return [list $exact $differ]
}

# Rendering code
proc renderstate state {
    return ([join [lmap s $state {
	incr i
	expr {$s ? "S($i)" : "\u00acS($i)"}
    }] "\u22c0"])
}

# The statements, encoded as expressions
set statements {
    {[llength $state] == 12}
    {[S+ 7 8 9 10 11 12] == 3}
    {[S+ 2 4 6 8 10 12] == 2}
    {S(5) ? S(6) && S(7) : 1}
    {[S+ 2 3 4] == 0}
    {[S+ 1 3 5 7 9 11] == 4}
    {S(2) != S(3)}
    {S(7) ? S(5) && S(6) : 1}
    {[S+ 1 2 3 4 5 6] == 3}
    {S(11) && S(12)}
    {[S+ 7 8 9] == 1}
    {[S+ 1 2 3 4 5 6 7 8 9 10 11] == 4}
}
# Find the truth assignment(s) that give consistency
lassign [findTruthMatch [lambdas $statements]] exact differ
# Print the results
foreach state $exact {
    puts "exact match\t[renderstate $state ]"
}
foreach {state j} $differ {
    puts "almost found\t[renderstate $state] \u21d2 [expr {[lindex $state $j-1]?"\u00ac":{}}]S($j)"
}
```

```txt

exact match     (S(1)?¬S(2)?S(3)?S(4)?¬S(5)?S(6)?S(7)?¬S(8)?¬S(9)?¬S(10)?S(11)?¬S(12))
almost found    (¬S(1)?¬S(2)?¬S(3)?¬S(4)?S(5)?¬S(6)?¬S(7)?S(8)?¬S(9)?¬S(10)?S(11)?¬S(12)) ? S(1)
almost found    (¬S(1)?¬S(2)?¬S(3)?¬S(4)?S(5)?¬S(6)?¬S(7)?S(8)?¬S(9)?S(10)?S(11)?S(12)) ? S(1)
almost found    (¬S(1)?¬S(2)?¬S(3)?S(4)?¬S(5)?¬S(6)?¬S(7)?S(8)?¬S(9)?S(10)?S(11)?S(12)) ? S(1)
almost found    (S(1)?¬S(2)?¬S(3)?¬S(4)?S(5)?¬S(6)?¬S(7)?¬S(8)?¬S(9)?¬S(10)?¬S(11)?¬S(12)) ? S(8)
almost found    (S(1)?¬S(2)?¬S(3)?¬S(4)?S(5)?¬S(6)?¬S(7)?S(8)?¬S(9)?¬S(10)?¬S(11)?¬S(12)) ? S(11)
almost found    (S(1)?¬S(2)?¬S(3)?¬S(4)?S(5)?¬S(6)?¬S(7)?S(8)?¬S(9)?¬S(10)?S(11)?¬S(12)) ? S(12)
almost found    (S(1)?¬S(2)?¬S(3)?¬S(4)?S(5)?¬S(6)?¬S(7)?S(8)?¬S(9)?S(10)?S(11)?S(12)) ? ¬S(12)
almost found    (S(1)?¬S(2)?¬S(3)?¬S(4)?S(5)?S(6)?¬S(7)?¬S(8)?S(9)?¬S(10)?S(11)?¬S(12)) ? S(8)
almost found    (S(1)?¬S(2)?¬S(3)?S(4)?¬S(5)?¬S(6)?¬S(7)?¬S(8)?¬S(9)?¬S(10)?¬S(11)?¬S(12)) ? S(8)
almost found    (S(1)?¬S(2)?¬S(3)?S(4)?¬S(5)?¬S(6)?¬S(7)?S(8)?¬S(9)?S(10)?S(11)?S(12)) ? ¬S(12)
almost found    (S(1)?¬S(2)?¬S(3)?S(4)?¬S(5)?S(6)?¬S(7)?S(8)?S(9)?¬S(10)?¬S(11)?¬S(12)) ? ¬S(6)
almost found    (S(1)?¬S(2)?S(3)?S(4)?¬S(5)?¬S(6)?¬S(7)?S(8)?S(9)?¬S(10)?¬S(11)?¬S(12)) ? S(7)
almost found    (S(1)?¬S(2)?S(3)?S(4)?¬S(5)?S(6)?S(7)?¬S(8)?S(9)?¬S(10)?¬S(11)?¬S(12)) ? ¬S(9)
almost found    (S(1)?S(2)?¬S(3)?S(4)?¬S(5)?¬S(6)?S(7)?¬S(8)?S(9)?¬S(10)?¬S(11)?S(12)) ? ¬S(12)
almost found    (S(1)?S(2)?¬S(3)?S(4)?¬S(5)?¬S(6)?S(7)?¬S(8)?S(9)?S(10)?¬S(11)?¬S(12)) ? ¬S(10)
almost found    (S(1)?S(2)?¬S(3)?S(4)?¬S(5)?¬S(6)?S(7)?S(8)?S(9)?¬S(10)?¬S(11)?¬S(12)) ? ¬S(8)

```



## TXR



```txrlisp
(defmacro defconstraints (name size-name (var) . forms)
  ^(progn (defvar ,size-name ,(length forms))
          (defun ,name (,var)
            (list ,*forms))))

(defconstraints con con-count (s)
  (= (length s) con-count) ;; tautology
  (= (countq t [s -6..t]) 3)
  (= (countq t (mapcar (op if (evenp @1) @2) (range 1) s)) 2)
  (if [s 4] (and [s 5] [s 6]) t)
  (none [s 1..3])
  (= (countq t (mapcar (op if (oddp @1) @2) (range 1) s)) 4)
  (and (or [s 1] [s 2]) (not (and [s 1] [s 2])))
  (if [s 6] (and [s 4] [s 5]) t)
  (= (countq t [s 0..6]) 3)
  (and [s 10] [s 11])
  (= (countq t [s 6..9]) 1)
  (= (countq t [s 0..con-count]) 4))

(defun true-indices (truths)
  (mappend (do if @1 ^(,@2)) truths (range 1)))

(defvar results
  (append-each ((truths (rperm '(nil t) con-count)))
    (let* ((vals (con truths))
           (consist [mapcar eq truths vals])
           (wrong-count (countq nil consist))
           (pos-wrong (+ 1 (or (posq nil consist) -2))))
      (cond
        ((zerop wrong-count)
         ^((:----> ,*(true-indices truths))))
        ((= 1 wrong-count)
         ^((:close ,*(true-indices truths) (:wrong ,pos-wrong))))))))

(each ((r results))
  (put-line `@r`))
```


```txt
close 5 8 11 (wrong 1)
close 1 5 (wrong 8)
close 1 5 8 (wrong 11)
close 1 5 8 11 (wrong 12)
close 1 5 8 10 11 12 (wrong 12)
close 1 5 6 9 11 (wrong 8)
close 1 3 4 8 9 (wrong 7)
----> 1 3 4 6 7 11
close 1 3 4 6 7 9 (wrong 9)
close 1 2 4 7 9 12 (wrong 12)
close 1 2 4 7 9 10 (wrong 10)
close 1 2 4 7 8 9 (wrong 8)
```



## uBasic/4tH

<lang>S = 12
For T = 0 To (2^S)-1

  For I = 1 To 12
      Push T, 2^(I-1) : Gosub 100
      @(I) = Pop() # 0
  Next

  REM Test consistency:

  @(101) = @(1)  = (S = 12)
  @(102) = @(2)  = ((@(7)+@(8)+@(9)+@(10)+@(11)+@(12)) = 3)
  @(103) = @(3)  = ((@(2)+@(4)+@(6)+@(8)+@(10)+@(12)) = 2)
  @(104) = @(4)  = ((@(5)=0) + (@(6) * @(7)) # 0)
  @(105) = @(5)  = ((@(2)=0) * (@(3)=0) * (@(4)=0))
  @(106) = @(6)  = ((@(1)+@(3)+@(5)+@(7)+@(9)+@(11)) = 4)
  @(107) = @(7)  = ((@(2) + @(3)) = 1)
  @(108) = @(8)  = ((@(7)=0) + (@(5) * @(6)) # 0)
  @(109) = @(9)  = ((@(1)+@(2)+@(3)+@(4)+@(5)+@(6)) = 3)
  @(110) = @(10) = (@(11) * @(12))
  @(111) = @(11) = ((@(7)+@(8)+@(9)) = 1)
  @(112) = @(12) = ((@(1)+@(2)+@(3)+@(4)+@(5)+@(6)+@(7)+@(8)+@(9)+@(10)+@(11)) = 4)

  Q = 0
  For I = 101 To 112
      Q = Q + @(I)
  Next

  If (Q = 11) Then
     Print "Near miss with statements ";
     For I = 1 To 12
       If @(I) Then
          Print I; " ";
       Endif
       If (@(I+100) = 0) Then
          M = I
       Endif
     Next
     Print "true (failed " ;M; ")."
  Endif

  If (Q = 12) Then
     Print "Solution! with statements ";
     For I = 1 TO 12
       If @(I) Then
          Print I; " ";
       Endif
     Next
     Print "true."
  Endif

Next
End

100 Rem a hard way to do a binary AND
    q = Pop() : p = Pop() : Push 0

    Do While (p * q) * (Tos() = 0)
       Push Pop() + (p % 2) * (q % 2)
       p = p / 2
       q = q / 2
    Loop

    Return
```

Output:

```txt
Near miss with statements 1 4 true (failed 8).
Near miss with statements 1 5 true (failed 8).
Near miss with statements 1 5 8 true (failed 11).
Near miss with statements 1 3 4 6 7 9 true (failed 9).
Near miss with statements 1 3 4 8 9 true (failed 7).
Near miss with statements 1 4 6 8 9 true (failed 6).
Near miss with statements 1 2 4 7 8 9 true (failed 8).
Near miss with statements 1 2 4 7 9 10 true (failed 10).
Solution! with statements 1 3 4 6 7 11 true.
Near miss with statements 5 8 11 true (failed 1).
Near miss with statements 1 5 8 11 true (failed 12).
Near miss with statements 1 5 6 9 11 true (failed 8).
Near miss with statements 1 2 4 7 9 12 true (failed 12).
Near miss with statements 4 8 10 11 12 true (failed 1).
Near miss with statements 1 4 8 10 11 12 true (failed 12).
Near miss with statements 5 8 10 11 12 true (failed 1).
Near miss with statements 1 5 8 10 11 12 true (failed 12).
```



## VBA

```vb
Public s As String    '-- (eg "101101100010")
Public t As Integer   '-- scratch

Function s1()
    s1 = Len(s) = 12
End Function
Function s2()
    t = 0
    For i = 7 To 12
        t = t - (Mid(s, i, 1) = "1")
    Next i
    s2 = t = 3
End Function
Function s3()
    t = 0
    For i = 2 To 12 Step 2
        t = t - (Mid(s, i, 1) = "1")
    Next i
    s3 = t = 2
End Function
Function s4()
    s4 = Mid(s, 5, 1) = "0" Or ((Mid(s, 6, 1) = "1" And Mid(s, 7, 1) = "1"))
End Function
Function s5()
    s5 = Mid(s, 2, 1) = "0" And Mid(s, 3, 1) = "0" And Mid(s, 4, 1) = "0"
End Function
Function s6()
    t = 0
    For i = 1 To 12 Step 2
        t = t - (Mid(s, i, 1) = "1")
    Next i
    s6 = t = 4
End Function
Function s7()
    s7 = Mid(s, 2, 1) <> Mid(s, 3, 1)
End Function
Function s8()
    s8 = Mid(s, 7, 1) = "0" Or (Mid(s, 5, 1) = "1" And Mid(s, 6, 1) = "1")
End Function
Function s9()
    t = 0
    For i = 1 To 6
        t = t - (Mid(s, i, 1) = "1")
    Next i
    s9 = t = 3
End Function
Function s10()
    s10 = Mid(s, 11, 1) = "1" And Mid(s, 12, 1) = "1"
End Function
Function s11()
    t = 0
    For i = 7 To 9
        t = t - (Mid(s, i, 1) = "1")
    Next i
    s11 = t = 1
End Function
Function s12()
    t = 0
    For i = 1 To 11
        t = t - (Mid(s, i, 1) = "1")
    Next i
    s12 = t = 4
End Function

Public Sub twelve_statements()
    For i = 0 To 2 ^ 12 - 1
        s = Right(CStr(WorksheetFunction.Dec2Bin(64 + i \ 128)), 5) _
            & Right(CStr(WorksheetFunction.Dec2Bin(256 + i Mod 128)), 7)
        For b = 1 To 12
            Select Case b
                Case 1: If s1 <> (Mid(s, b, 1) = "1") Then Exit For
                Case 2: If s2 <> (Mid(s, b, 1) = "1") Then Exit For
                Case 3: If s3 <> (Mid(s, b, 1) = "1") Then Exit For
                Case 4: If s4 <> (Mid(s, b, 1) = "1") Then Exit For
                Case 5: If s5 <> (Mid(s, b, 1) = "1") Then Exit For
                Case 6: If s6 <> (Mid(s, b, 1) = "1") Then Exit For
                Case 7: If s7 <> (Mid(s, b, 1) = "1") Then Exit For
                Case 8: If s8 <> (Mid(s, b, 1) = "1") Then Exit For
                Case 9: If s9 <> (Mid(s, b, 1) = "1") Then Exit For
                Case 10: If s10 <> (Mid(s, b, 1) = "1") Then Exit For
                Case 11: If s11 <> (Mid(s, b, 1) = "1") Then Exit For
                Case 12: If s12 <> (Mid(s, b, 1) = "1") Then Exit For
            End Select
            If b = 12 Then Debug.Print s
        Next
    Next
End Sub
```
```txt
Found solution:101101100010
```



## Yabasic

```Yabasic
sub s1() return len(s$)=12 end sub
sub s2() local t, i : t=0 : for i=7 to 12 : t = t + (mid$(s$, i, 1) <> "0") : next : return t=3 end sub
sub s3() local t, i : t=0 : for i=2 to 12 step 2 : t = t + (mid$(s$, i, 1) <> "0") : next : return t=2 end sub
sub s4() return mid$(s$, 5, 1) = "0" or (mid$(s$, 6, 1) <> "0" and mid$(s$, 7, 1) <> "0") end sub
sub s5() return mid$(s$, 2, 1) = "0" and mid$(s$, 3, 1) = "0" and mid$(s$, 4, 1) = "0" end sub
sub s6() local t, i : t=0 : for i=1 to 12 step 2 : t = t + mid$(s$, i, 1) <> "0" : next : return t=4 end sub
sub s7() return mid$(s$, 2, 1) <> mid$(s$, 3, 1) end sub
sub s8() return mid$(s$, 7, 1) = "0" or (mid$(s$, 5, 1) <> "0" and mid$(s$, 6, 1) <> "0") end sub
sub s9() local t, i : t=0 : for i=1 to 6 : t = t + mid$(s$, i, 1) <> "0" : next : return t=3 end sub
sub s10() return mid$(s$, 11, 1) <> "0" and mid$(s$, 12, 1) <> "0" end sub
sub s11() local t, i : t=0 : for i=7 to 9 : t = t + mid$(s$, i, 1) <> "0" : next : return t=1 end sub
sub s12() local t, i : t=0 : for i=1 to 11 : t = t + mid$(s$, i, 1) <> "0" : next : return t=4 end sub

dim r$(12)

for b=1 to 12
    r$(b) = "s"+str$(b)
next
for i=0 to 2^12-1
    s$ = right$("000000000000" + bin$(i), 12)
    for b=1 to 12
        if execute(r$(b)) <> (mid$(s$, b, 1) <> "0") break
        if b=12 print s$
    next
next
```



## zkl


```zkl
var statements; // list of 13 Bools, statements[0] is garbage to make 1 based
fcn s0 { False }  // dummy for padding
fcn s1 { True }
fcn s2 { statements[-6,*].filter().len()==3 }
fcn s3 { [2..12,2].apply(statements.get).filter().len()==2 }
fcn s4 { if(statements[5]) statements[6]==statements[7]==True else True }
fcn s5 { statements[2,3].filter().len()==0 }
fcn s6 { [1..12,2].apply(statements.get).filter().len()==4 }
fcn s7 { statements[2]!=statements[3] }
fcn s8 { if(statements[7]) statements[5]==statements[6]==True else True }
fcn s9 { statements[1,6].filter().len()==3 }
fcn s10{ statements[11]==statements[12]==True }
fcn s11{ statements[7,3].filter().len()==1 }
fcn s12{ statements[1,11].filter().len()==4 }

filters:=T(s0,s1,s2,s3,s4,s5,s6,s7,s8,s9,s10,s11,s12);
foreach n in ((2).pow(12)){  // 4k
   // 5-->"0000000000101"-->("0","0"..."1")-->(F,F,...T)
   statements="%013.2B".fmt(n).split("").apply('==("1"));
   r:=filters.run(True);  // and return list of results
   if(r==statements) print("<<<<<<<<<<<<<<<<Solution");
   else{
      diff:=r.zipWith('!=,statements);
      if(diff.sum(0)==1) print("Diff @",diff.filter1n());
   }
}
fcn print(msg){
   (12).pump(List,'wrap(n){ statements[n] and n or Void.Skip })
   .concat(",").println(" : ",vm.pasteArgs());
}
```

```txt

5,8,11 : Diff @1
5,8,10,11,12 : Diff @1
4,8,10,11,12 : Diff @1
1,5 : Diff @8
1,5,8 : Diff @11
1,5,8,11 : Diff @12
1,5,8,10,11,12 : Diff @12
1,5,6,9,11 : Diff @8
1,4 : Diff @8
1,4,8,10,11,12 : Diff @12
1,4,6,8,9 : Diff @6
1,3,4,8,9 : Diff @7
1,3,4,6,7,11 : <<<<<<<<<<<<<<<<Solution
1,3,4,6,7,9 : Diff @9
1,2,4,7,9,12 : Diff @12
1,2,4,7,9,10 : Diff @10
1,2,4,7,8,9 : Diff @8

```

