+++
title = "Execute HQ9+"
description = ""
date = 2019-10-22T03:58:55Z
aliases = []
[extra]
id = 4558
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Implement a   ''' [[HQ9+]] '''   interpreter or compiler.





## Ada


see [[Execute HQ9+/Ada]]


## Agena

Tested with Agena 2.9.5 Win32

```agena
# HQ9+ interpreter

# execute an HQ9+ program in the code string - code is not case sensitive
hq9 := proc( code :: string ) is
    local hq9Accumulator := 0; # the HQ9+ accumulator
    local hq9Operations  :=    # table of HQ9+ operations and their implemntations
          [ "q" ~ proc() is print( code ) end
          , "h" ~ proc() is print( "Hello, world!" ) end
          , "9" ~ proc() is
                      local writeBottles := proc( bottleCount :: number, message :: string ) is
                                                print( bottleCount
                                                     & " bottle"
                                                     & if bottleCount <> 1 then "s " else " " fi
                                                     & message
                                                     )
                                            end;

                      for bottles from 99 to 1 by -1 do
                          writeBottles( bottles, "of beer on the wall" );
                          writeBottles( bottles, "of beer" );
                          print( "Take one down, pass it around," );
                          if bottles > 1 then
                              writeBottles( bottles - 1, "of beer on the wall." )
                          fi;
                          print()
                      od;
                      print( "No more bottles of beer on the wall." )
                  end
          , "+" ~ proc() is inc hq9Accumulator, 1 end
          ];
    for op in lower( code ) do
        if hq9Operations[ op ] <> null then
            hq9Operations[ op ]()
        else
            print( '"' & op & '" not implemented' )
        fi
    od
end;

# prompt for HQ9+ code and execute it, repeating until an empty code string is entered
scope
    local code;
    do
        write( "HQ9+> " );
        code := io.read();
        hq9( code )
    until code = ""
epocs;
```



## ALGOL 68

Translation of DWScript. the accumulator is global.

```algol68
# the increment-only accumulator #
INT hq9accumulator := 0;

# interpret a HQ9+ code string #
PROC hq9 = ( STRING code )VOID:
    FOR i TO UPB code
    DO
        CHAR   op = code[ i ];
        IF op = "Q" OR op = "q"
        THEN
            # display the program #
            print( ( code, newline ) )
        ELIF op = "H" OR op = "h"
        THEN
            print( ( "Hello, world!", newline ) )
        ELIF op = "9"
        THEN
            # 99 bottles of beer #
            FOR bottles FROM 99 BY -1 TO 1 DO
                 STRING bottle count = whole( bottles, 0 ) + IF bottles > 1 THEN " bottles" ELSE " bottle" FI;
                 print( ( bottle count, " of beer on the wall", newline ) );
                 print( ( bottle count, " bottles of beer.",    newline ) );
                 print( ( "Take one down, pass it around,",     newline ) );
                 IF bottles > 1
                 THEN
                     print( ( whole( bottles - 1, 0 ), " bottles of beer on the wall.", newline, newline ) )
                 FI
            OD;
            print( ( "No more bottles of beer on the wall.", newline ) )
        ELIF op = "+"
        THEN
            # increment the accumulator #
            hq9accumulator +:= 1
        ELSE
            # unimplemented operation #
            print( ( """", op, """ not implemented", newline ) )
        FI
    OD;


# test the interpreter #
BEGIN
    STRING code;
    print( ( "HQ9+> " ) );
    read( ( code, newline ) );
    hq9( code )
END
```



## ALGOL W

Based on ALGOL 68 (which is a translation of DWScript)...

```algolw
begin

    procedure writeBottles( integer value bottleCount ) ;
        begin
            write( bottleCount, " bottle" );
            if bottleCount not = 1 then writeon( "s " ) else writeon( " " );
        end writeBottles ;

    procedure hq9 ( string(32) value code   % code to execute %
                  ; integer    value length % length of code  %
                  ) ;
        for i := 0 until length - 1 do begin
            string(1) op;

            % the increment-only accumulator %
            integer   hq9accumulator;

            hq9accumulator := 0;
            op             := code(i//1);

            if op = "Q" or op = "q" then write( code )
            else if op = "H" OR op = "h" then write( "Hello, World!" )
            else if op = "9" then begin
                % 99 bottles of beer %
                i_w := 1; s_w := 0;
                for bottles := 99 step -1 until 1 do begin
                    writeBottles( bottles ); writeon( "of beer on the wall" );
                    writeBottles( bottles ); writeon( "of beer" );;
                    write( "Take one down, pass it around," );
                    if bottles > 1 then begin
                        writeBottles( bottles - 1 ); writeon( "of beer on the wall." )
                    end;
                    write()
                end;
                write( "No more bottles of beer on the wall." )
                end
            else if op = "+" then hq9accumulator := hq9accumulator + 1
            else write( """", op, """ not implemented" )
        end hq9 ;


    % test the interpreter %
    begin
        string(32) code;
        integer    codeLength;
        write( "HQ9+> " );
        read( code );
        codeLength := 31;
        while codeLength >= 0 and code(codeLength//1) = " " do codeLength := codeLength - 1;
        hq9( code, codeLength + 1 )
    end
end.
```



## Applesoft BASIC


```ApplesoftBasic
100 INPUT "HQ9+ : "; I$
110 LET J$ = I$ + CHR$(13)
120 LET H$ = "HELLO, WORLD!"
130 LET B$ = "BOTTLES OF BEER"
140 LET W$ = " ON THE WALL"
150 LET W$ = W$ + CHR$(13)
160 FOR I = 1 TO LEN(I$)
170     LET C$ = MID$(J$, I, 1)
180     IF C$ = "H" THEN PRINT H$
190     IF C$ = "Q" THEN PRINT I$
200     LET A = A + (C$ = "+")
210     IF C$ <> "9" THEN 280
220     FOR B = 99 TO 1 STEP -1
230         PRINT B " " B$ W$ B " " B$
240         PRINT "TAKE ONE DOWN, ";
250         PRINT "PASS IT AROUND"
260         PRINT B - 1 " " B$ W$
270     NEXT B
280 NEXT I
```



## x86 Assembly


```X86 Assembly


;ds:si: pointer to asciiz string containing HQ9++ source code
ExecuteHQ9:
	push ax
	push dx
	push si
	push di
	push es
	push bx
	mov bx, si
	.interpret:
		lodsb
		cmp al, 'H'
		je .doHelloWorld
		cmp al, 'Q'
		je .doPrintCode
		cmp al, '9'
		je .doDrinkBeer
		cmp al, '+'
		je .doCounter
		pop bx
		pop es
		pop di
		pop si
		pop dx
		pop ax
		ret

	.doHelloWorld:
		push ds
		mov ax, cs
		mov ds, ax
		push si
		mov si, .dataHelloWorld
		call .printString
		pop si
		pop ds
		jmp .interpret

	.doPrintCode:
		push si
		mov si, bx
		call .printString
		pop si
		jmp .interpret

	.doDrinkBeer:
		push ds
		push si
		push ax
		mov ax, cs
		mov ds, ax
		mov ax, 99
		.beer_loop:
			call .printHexNumber
			mov si, .dataBeerSong1
			call .printString
			call .printHexNumber
			mov si, .dataBeerSong2
			call .printString
			dec ax
			call .printHexNumber
			mov si, .dataBeerSong3
			call .printString
			test ax, ax
			jnz .beer_loop
		pop ax
		pop si
		pop ds
		jmp .interpret

	.doCounter:
		push ax
		inc ax
		pop ax
		jmp .interpret

	.printString:
		push ax
		push si
	 .looping:
		lodsb
		test al, al
		jz .done
		mov ah, 0Eh
		int 10h
		jmp .looping
	 .done:
		pop si
		pop ax
		ret

	.printHexNumber:
			pusha
			push ds
			mov ax, cs
			mov ds, ax
			push word 0
			mov bx, ax
			xor dx, dx
			mov cx, 4r
	 .convert_loop:
			mov ax, bx
			and ax, 0Fh
			cmp ax, 9
			ja  .greater_than_9
			add ax, '0'
			jmp .converted
	 .greater_than_9:
			add ax, 'A'-0Ah
	 .converted:
			push ax
			shr bx, 4
			dec cx
			jnz .convert_loop
	 .popoff:
			pop ax
			cmp ax, 0
			je .done
			mov ah, 0Eh
			int 10h
			jmp .popoff
		.done:
			pop ds
			popa
			ret

	.dataHelloWorld: db "Hello World!", 0
	.dataBeerSong1: db " bottles of beer on the wall ", 0
	.dataBeerSong2: db " bottles of beer", 13, 10, "Take one down, pass it around "
	.dataBeerSong3: db 0, " bottles of beer on the wall", 0


```



## AutoHotkey


```AutoHotkey
; http://www.autohotkey.com/forum/viewtopic.php?p=356268#356268

testCode := "hq9+HqQ+Qq"

MsgBox % RunHQ9Plus(testCode)

;---------------------------------

RunHQ9Plus(input)
{
  Loop, Parse, input
    If ( A_LoopField = "+" )
      acc++
    Else If ( A_LoopField = "H" )
      output .= "Hello, world!`n"
    Else If ( A_LoopField = "Q" )
      output .= input "`n"
    Else If ( A_LoopField = "9" )
      Loop, 99
      {
        ; following 4 lines could be only 1 long line
        output .= (99+1-A_Index) " bottles of beer on the wall`n"
        output .= (99+1-A_Index) " bottles of beer`n"
        output .= "Take one down, pass it around`n"
        output .= (99-A_Index) " bottles of beer on the wall`n`n"
      }
  Return output
}
```




## BASIC256

```BASIC256

# Intérprete de HQ9+

global codigo
codigo = ""

function HQ9plus(codigo)
	acumulador = 0
	HQ9plus1 = ""
	for cont = 1 to length(codigo)
		op =  upper(mid(codigo, cont, 1))
		begin case
			case op = "H"
				HQ9plus1 = HQ9plus1 + "Hello, world!"
			case op = "Q"
				HQ9plus1 = HQ9plus1 + codigo
			case op = "9"
				for botellas = 99 to 1 step -1
					HQ9plus1 = HQ9plus1 + string(botellas) + " bottle"
					if (botellas > 1) then HQ9plus1 = HQ9plus1 + "s"
					HQ9plus1 = HQ9plus1 + " of beer on the wall, " + string(botellas) + " bottle"
					if (botellas > 1) then HQ9plus1 = HQ9plus1 + "s"
					HQ9plus1 = HQ9plus1 + " of beer,"  + chr(13) + chr(10) + "Take one down, pass it around, " + string(botellas - 1) + " bottle"
					if (botellas > 2) then HQ9plus1 = HQ9plus1 + "s"
					HQ9plus1 = HQ9plus1 + " of beer on the wall." + chr(13) + chr(10) + chr(10)
				next botellas
				HQ9plus1 = HQ9plus1 + "No more bottles of beer on the wall, no more bottles of beer." + chr(13) + chr(10) + "Go to the store and buy some more, 99 bottles of beer on the wall."
			case op = "+"
				acumulador = (acumulador + 1)
			case op = "E"
				end
		end case
		if mid(codigo, cont, 1) <> "+" then
			HQ9plus1 = HQ9plus1 + chr(13) + chr(10)
		end if
	next cont
	HQ9plus = left(HQ9plus1, (length(HQ9plus1) - 2))
end function


cls
do
	input codigo
	print HQ9plus(codigo): print
until false
end

```




## BBC BASIC


```bbcbasic
      PROChq9plus("hq9+HqQ+Qq")
      END

      DEF PROChq9plus(code$)
      LOCAL accumulator%, i%, bottles%
      FOR i% = 1 TO LEN(code$)
        CASE MID$(code$, i%, 1) OF
          WHEN "h","H": PRINT "Hello, world!"
          WHEN "q","Q": PRINT code$
          WHEN "9":
            bottles% = 99
            WHILE bottles%
              PRINT ;bottles% " bottles of beer on the wall, ";
              PRINT ;bottles% " bottles of beer,"
              bottles% -= 1
              PRINT "Take one down, pass it around, ";
              PRINT ;bottles% " bottles of beer on the wall."
            ENDWHILE
          WHEN "+": accumulator% += 1
        ENDCASE
      NEXT i%
      ENDPROC
```

'''Output:'''

```txt

Hello, world!
hq9+HqQ+Qq
99 bottles of beer on the wall, 99 bottles of beer,
Take one down, pass it around, 98 bottles of beer on the wall.
98 bottles of beer on the wall, 98 bottles of beer,
Take one down, pass it around, 97 bottles of beer on the wall.
...
3 bottles of beer on the wall, 3 bottles of beer,
Take one down, pass it around, 2 bottles of beer on the wall.
2 bottles of beer on the wall, 2 bottles of beer,
Take one down, pass it around, 1 bottles of beer on the wall.
1 bottles of beer on the wall, 1 bottles of beer,
Take one down, pass it around, 0 bottles of beer on the wall.
Hello, world!
hq9+HqQ+Qq
hq9+HqQ+Qq
hq9+HqQ+Qq
hq9+HqQ+Qq

```



## C


```c
void runCode(const char *code)
{
    int c_len = strlen(code);
    int i, bottles;
    unsigned accumulator=0;
    for(i=0;i<c_len;i++)
    {
        switch(code[i])
        {
            case 'Q':
                printf("%s\n", code);
                break;

            case 'H':
                printf("Hello, world!\n");
                break;

            case '9':
                //Nice bottles song alg. from RC :)
                bottles = 99;
                do {
                    printf("%d bottles of beer on the wall\n", bottles);
                    printf("%d bottles of beer\n", bottles);
                    printf("Take one down, pass it around\n");
                    printf("%d bottles of beer on the wall\n\n", --bottles);
                } while( bottles > 0 );
                break;

            case '+':
                //Am I the only one finding this one weird? :o
                accumulator++;
                break;
        }
    }
};
```



## C++

Basically the same as the C example, although this has been C++'ified with strings and streams.

```cpp
void runCode(string code)
{
    int c_len = code.length();
    unsigned accumulator=0;
    int bottles;
    for(int i=0;i<c_len;i++)
    {
        switch(code[i])
        {
            case 'Q':
                cout << code << endl;
                break;

            case 'H':
                cout << "Hello, world!" << endl;
                break;

            case '9':
                //Nice bottles song alg. from RC :)
                bottles = 99;
                do {
                    cout << bottles << " bottles of beer on the wall" << endl;
                    cout << bottles << " bottles of beer" << endl;
                    cout << "Take one down, pass it around" << endl;
                    cout << --bottles << " bottles of beer on the wall" << endl << endl;
                } while( bottles > 0 );
                break;

            case '+':
                //Am I the only one finding this one weird? :o
                accumulator++;
                break;
        }
    }
};
```



## C#



```c#

using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void RunCode(string code)
    {
        int accumulator = 0;
        var opcodes = new Dictionary<char, Action>
        {
            {'H', () => Console.WriteLine("Hello, World!"))},
            {'Q', () => Console.WriteLine(code) },
            {'9', () => Console.WriteLine(Enumerable.Range(1,100).Reverse().Select(n => string.Format("{0} bottles of beer on the wall\n{0} bottles of beer\nTake one down, pass it around\n{1} bottles of beer on the wall\n", n, n-1)).Aggregate((a,b) => a + "\n" + b))},
            {'+', () => accumulator++ }
        }

        foreach(var c in code)
            opcodes[c]();
    }
}

```



## Ceylon


```ceylon
shared void run() {

	void eval(String code) {

		variable value accumulator = 0;

		for(c in code.trimmed.lowercased) {
			switch(c)
			case('h') {
				print("Hello, world!");
			}
			case('q') {
				print(code);
			}
			case('9') {
				function bottles(Integer i) =>
						switch(i)
						case(0) "No bottles"
						case(1) "One bottle"
						else "``i`` bottles";
				for(i in 99..1) {
					print("``bottles(i)`` of beer on the wall,
					       ``bottles(i)`` of beer,
					       take one down and pass it around,
					       ``bottles(i - 1)`` of beer on the wall!");
				}
			}
			case('+') {
				accumulator++;
			}
			else {
				print("syntax error");
			}
		}
	}

	eval("hq9+");
}
```



## Clojure


```clojure
(ns anthony.random.hq9plus
  (:require [clojure.string :as str]))

(defn bottles []
  (loop [bottle 99]
    (if (== bottle 0)
      ()
      (do
        (println (str bottle " bottles of beer on the wall"))
        (println (str bottle " bottles of beer"))
        (println "Take one down, pass it around")
        (println (str bottle " bottles of beer on the wall"))
        (recur (dec bottle))))))

(defn execute-hq9plus [& commands]
  (let [accumulator (atom 0)]
    (loop [pointer 0]
      (condp = (nth commands pointer)
        \H (println "Hello, world!")
        \Q (println (str/join commands))
        \9 (bottles)
        \+ (reset! accumulator (inc @accumulator)))
      (if-not (= (inc pointer) (count commands)) (recur (inc pointer))))))
```



## COBOL


```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Exec-Hq9.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       78  Code-Length VALUE 256.

       01  i           PIC 999.
       01  accumulator PIC 999.

       01  bottles     PIC 999.

       LINKAGE SECTION.
       01  hq9-code    PIC X(Code-Length).

       PROCEDURE DIVISION USING BY VALUE hq9-code.
           PERFORM VARYING i FROM 1 BY 1 UNTIL Code-Length < i
               EVALUATE hq9-code (i:1)
                   WHEN "Q"
                       DISPLAY FUNCTION TRIM(hq9-code)

                   WHEN "H"
                       DISPLAY "Hello, World!"

                   WHEN "9"
                       MOVE 99 TO bottles
                       PERFORM UNTIL bottles = ZERO
                           DISPLAY
                               bottles " bottles of beer on the wall"
                           DISPLAY bottles " bottles of beer"
                           DISPLAY "Take one down, pass it around"
                           SUBTRACT 1 FROM bottles
                           DISPLAY
                               bottles " bottles of beer on the wall"
                           DISPLAY SPACE
                       END-PERFORM

                   WHEN "+"
                       ADD 1 TO accumulator
               END-EVALUATE
           END-PERFORM

           GOBACK
           .
```



## Common Lisp


See [[Execute HQ9+/Common Lisp]].


## D


```d
import std.stdio, std.string;

void main(in string[] args) {
    if (args.length != 2 ||
        args[1].length != args[1].countchars("hHqQ9+")) {
        writeln("Not valid HQ9+ code.");
        return;
    }

    ulong accumulator;
    foreach (immutable c; args[1]) {
        final switch(c) {
            case 'Q', 'q':
                writeln(args[1]);
                break;
            case 'H', 'h':
                writeln("Hello, world!");
                break;
            case '9':
                int bottles = 99;
                while (bottles > 1) {
                    writeln(bottles, " bottles of beer on the wall,");
                    writeln(bottles, " bottles of beer.");
                    writeln("Take one down, pass it around,");
                    if (--bottles > 1)
                        writeln(bottles,
                                " bottles of beer on the wall.\n");
                }
                writeln("1 bottle of beer on the wall.\n");
                break;
            case '+':
                accumulator++;
                break;
        }
    }
}
```



## DWScript


```dwscript
procedure RunCode(code : String);
var
   i : Integer;
   accum, bottles : Integer;
begin
   for i:=1 to Length(code) do begin
      case code[i] of
         'Q', 'q' : PrintLn(code);
         'H', 'h' : PrintLn('Hello, world!');
         '9' : begin
            bottles:=99;
            while bottles>1 do begin
               Print(bottles); PrintLn(' bottles of beer on the wall,');
               Print(bottles); PrintLn(' bottles of beer.');
               PrintLn('Take one down, pass it around,');
               Dec(bottles);
               if bottles>1 then begin
                  Print(bottles); PrintLn(' bottles of beer on the wall.'#13#10);
               end;
            end;
            PrintLn('1 bottle of beer on the wall.');
         end;
         '+' : Inc(accum);
      else
         PrintLn('Syntax Error');
      end;
   end;
end;
```



## Dyalect



```dyalect
func eval(code) {
    var accumulator = 0
    var opcodes = (
        "H": () => print("Hello, World!"),
        "Q": () => print(code),
        "9": () => {
                var quantity = 99
                while quantity > 1 {
                    print("\(quantity) bottles of beer on the wall, \(quantity) bottles of beer.")
                    print("Take one down and pass it around, \(quantity - 1) bottles of beer.")
                    quantity -= 1
                }
                print("1 bottle of beer on the wall, 1 bottle of beer.")
                print("Take one down and pass it around, no more bottles of beer on the wall.\n")
                print("No more bottles of beer on the wall, no more bottles of beer.")
                print("Go to the store and buy some more, 99 bottles of beer on the wall.")
            },
        "+": () => accumulator += 1
    )

    for c in code {
        opcodes[c.upper()]()
    }
}
```



## E


See [[Execute HQ9+/E]].


## Ela



### Impure approach



```ela
open unsafe.console char unsafe.cell imperative

eval src = eval' src
     where eval' [] = ()
           eval' (x::xs) | be 'H' = h() `seq` eval' xs
                         | be 'Q' = q() `seq` eval' xs
                         | be '9' = n() `seq` eval' xs
                         | be '+' = p() `seq` eval' xs
                         | else = fail ("Unrecognized " ++ x)
                         where r = ref 0
                               be c = char.upper x == c
                               h () = writen "Hello, world!"
                               q () = writen src
                               p () = r.+
                               n () = bottles [99,98..1]
                                  where bottles [] = ()
                                        bottles (x::xs) = rec write
                                          (show x) " bottles of beer of the wall\r\n"
                                          (show x) " bottles of beer\r\n"
                                          "Take one down, pass it around\r\n"
                                          `seq` bottles xs
```



### Pure version


An interpreter itself has no side effects:


```ela
open list char

eval src = eval' src 0
     where eval' [] a = []
           eval' (x::xs) a | be 'H' = h :: eval' xs a
                           | be 'Q' = q :: eval' xs a
                           | be '9' = force n :: eval' xs a
                           | be '+' = eval' xs (a+1)
                           | else   = fail "Invalid instruction."
                           where be c = char.upper x == c
                                 h = "Hello, world!"
                                 q  = src
                                 n = (& bottles [99,98..1])
                                     where bottles [] = ""
                                           bottles (x::xs) =
                                             show x ++ " bottles of beer of the wall\r\n"
                                             ++ show x ++ " bottles of beer\r\n"
                                             ++ "Take one down, pass it around\r\n"
                                             ++ bottles xs
```


It slightly alters an original HQ9+ specification. HQ9+ is an impure language that does console output. However console output is the only interaction that a user can see when executing HQ9+ program. This interpreter doesn't output to console but instead generates a list with all outputs. An accumulator is moved to the interpter arguments and the need for a reference cell is eliminated. Once an interpreter completes a client code can output to console using monads like so:


```ela
open imperative monad io

print_and_eval src = do
  res <- return $ eval src
  return $ each print res
  where print x = do putStrLn x

print_and_eval "HQ9+" ::: IO
```



## Erlang


```Erlang
% hq9+ Erlang implementation (JWL)
% http://www.erlang.org/
-module(hq9p).
-export([main/1]).

%% bottle helper routine
bottle(0) ->
  io:format("No more bottles of beer ");

bottle(1) ->
  io:format("1 bottle of beer ");

bottle(N) when N > 0 ->
  io:format("~w bottles of beer ", [N]).

%% Implementation of instructions
beer(0) ->
  bottle(0), io:format("on the wall~n"),
  bottle(0), io:format("on the wall~nGo to the store and buy some more~n"),
  io:format("99 bottles of beer on the wall.~n");

beer(N) ->
  bottle(N), io:format("on the wall~n"),
  bottle(N), io:format("~nTake one down and pass it around~n"),
  bottle(N-1), io:format("on the wall~n~n"),
  beer(N-1).

hello() ->
  io:format("Hello world!~n", []).

prog(Prog) ->
  io:format("~s~n", [Prog]).

inc(Acc) ->
  Acc+1.

%% Interpreter
execute(Instruction, Prog, Acc) ->
  case Instruction of
    $H -> hello(), Acc;
    $Q -> prog(Prog), Acc;
    $9 -> beer(99), Acc;
    $+ -> inc(Acc);
       _ -> io:format("Invalid instruction: ~c~n", [Instruction]), Acc
  end.

main([], _Prog, Acc) ->
  Acc;

main([Instruction | Rest], Prog, Acc) ->
  NewAcc = execute(Instruction, Prog, Acc),
  main(Rest, Prog, NewAcc).

main(Prog) ->
  Compiled = string:to_upper(Prog),
  main(Compiled, Prog, 0).


```



## Factor


```factor
USING: combinators command-line formatting interpolate io kernel
math math.ranges multiline namespaces sequences ;
IN: rosetta-code.hq9+

STRING: verse
${3} bottle${1} of beer on the wall
${3} bottle${1} of beer
Take one down, pass it around
${2} bottle${0} of beer on the wall
;

: bottles ( -- )
    99 1 [a,b]
    [ dup 1 - 2dup [ 1 = "" "s" ? ] bi@ verse interpolate nl ]
    each ;

SYMBOL: accumulator

CONSTANT: commands
{
    { CHAR: H [ drop "Hello, world!" print ] }
    { CHAR: Q [ print ] }
    { CHAR: 9 [ drop bottles ] }
    { CHAR: + [ drop accumulator inc ] }
    [ nip "Invalid command: %c" sprintf throw ]
}

: interpret-HQ9+ ( str -- )
    dup [ commands case ] with each accumulator off ;

: main ( -- ) command-line get first interpret-HQ9+ ;

MAIN: main
```

Test run on the command line:

```txt

>factor -run=rosetta-code.hq9+ H+Q+9+
Hello, world!
H+Q+9+
99 bottles of beer on the wall
99 bottles of beer
Take one down, pass it around
98 bottles of beer on the wall
. . .
1 bottle of beer on the wall
1 bottle of beer
Take one down, pass it around
0 bottles of beer on the wall

```



## Forth



```forth
variable accumulator
: H cr ." Hello, world!" ;
: Q cr 2dup type ;
: 9 99 verses ;  \ http://rosettacode.org/wiki/99_Bottles_of_Beer#Forth
: + 1 accumulator +! ;

: hq9+ ( "code" -- )
  parse-word 2dup bounds ?do
    i 1 [ get-current literal ] search-wordlist
    if execute else true abort" invalid HQ9+ instruction"
  then loop 2drop ;
```



## Fortran

This is F77 style except for the END SUBROUTINE HQ9, since F90+ allows the END statement to name its subroutine, and more seriously, the SELECT CASE construction that avoids interminable IF ... THEN ... ELSE IF ... sequences or even, a computed GO TO. The obvious data structure is the CHARACTER type, introduced with F77.

The only difficulty lies in the phasing of the various components of the recital (note the lines ending with commas or periods), and especially, producing correct grammar for the singular case. One could simply produce the likes of *"1 bottles of beer", or perhaps "1 bottle(s) of beer" but having been hounded for decades by compilers quibbling over syntax trivia, a certain sensitivity has arisen. For this case, the requirement is to append a "s" or not to "bottle" and the task is quite vexing because Fortran does not allow within expressions syntax such as
```Fortran
"bottle" // IF (B.NE.1) THEN "s" FI // " of beer"
```
 so alternative schemes must be devised. There are many possibilities. The output line could be written piecemeal using the "non-advancing" options introduced in F90 with the "s" being written or not, or, the output line could be developed piecemeal in a CHARACTER variable in a similar way then written in one go. Alternatively, a character variable SUFFIX could be employed, which contains either "s" or " " with its usage being <code>..."bottle"//SUFFIX(1:LSTNB(SUFFIX))//...</code> where function LSTNB fingers the last non-blank character (if function TRIM or LEN_TRIM are unavailable), or, with F2003 there is a facility whereby SUFFIX can be declared with a varying length so as to be either "s" or "". Still another ploy would be to replace the "s" by a "null" character (character code zero) that will be passed over by the device showing the output. Or maybe not...

However, because the tail end of the recital does not conform to the structure of the earlier verses, it seemed easier to combine the singular case with the coda, especially since "No bottles" is to be produced instead of "0 bottles". It would be easy enough to devise a function CARDINAL(N) that would return "Ninety-nine", ... "One", "No" but the required code would swamp the rest of the project.

So, there is a careful factorisation of the text phrases into FORMAT and WRITE statements. Note that "free-format" output (as with <code>WRITE (6,*)</code>) starts in the second column, whereas formatted output starts in the first column. Inspection of the code file HQ9.exe shows that the compiler has recognised that the multiple appearances of the text literals "bottles" (three) and "bottle" (two) are the same and there is only one value of each constant in the code file. However, it has not noticed that the text "bottle" can be extracted from "bottles", which could in turn be found within a larger text literal "No bottles of beer on the wall" which also contains the subsequence " on the wall" - perhaps the code to do this would consume more space than would be saved by having a single multiple-use text constant in the code for those, or perhaps the problem is just too difficult in general to be worth the effort of devising and executing a worthwhile analysis, given that only a few bytes might be saved in a code file of 480Kb. This of course must contain the format interpretation subsystem and so forth, not just the code for the Fortran source. Even so, this program (with minor changes to the syntax) could be written in Fortran IV for an IBM1130, and would run in a computer with a total memory size of 8Kb. On such systems, much thought would go in to minimising space lost to verbose texts and good exposition as well as such reuse opportunities: gaining access to 32Kb or even 64Kb systems would be a great relief. But these days, memory space is not at a premium, and we are told that modern compilers produce excellent code.


```Fortran

      SUBROUTINE HQ9(CODE)	!Implement the rather odd HQ9+ instruction set.
       CHARACTER*(*) CODE	!One operation code per character.
       INTEGER I,B	!Steppers.
       INTEGER A	!An accumulator.
        A = 0		!Initialised.
        DO I = 1,LEN(CODE)	!Step through the code.
          SELECT CASE(CODE(I:I))!Inspect the operation code.
           CASE(" ")		!Might as well do nothing.
           CASE("+")		!Increment the accumulator.
            A = A + 1		!Thus. Though, nothing refers to it...
           CASE("h","H")	!Might as well allow upper or lower case.
            WRITE (6,*) "Hello, world!"	!Hi there!
           CASE("q","Q")	!Show the (rather questionable) code.
            WRITE (6,*) CODE	!Thus.
           CASE("9")		!Recite "99 bottles of beer"...
            DO B = 99,2,-1	!Grammar is to be upheld, so the singular case is special.
              WRITE (6,1) B,"bottles"," on the wall,",B,"bottles","."	!Two lots: number, text, text.
    1         FORMAT (I2,1X,A," of beer",A)	!Exhausted by the first triplet, so a new line for the second.
              WRITE (6,2)			!Now for the reduction.
    2         FORMAT ("Take one down, pass it around,")	!Announce.
              IF (B.GT.2) WRITE (6,1) B - 1,"bottles"," on the wall."	!But, not for the singular state.
            END DO		!Recite the next stanza.
            WRITE (6,1) 1,"bottle"," on the wall,",1,"bottle","."	!The singular case. No longer "bottles".
            WRITE (6,2)						!There's nothing so lonesome, morbid or drear,
            WRITE (6,*) "No bottles of beer on the wall."	!Than to stand at the bar of a pub with no beer.
            WRITE (6,*) "Go to the store, buy some more."	!Take action.
           CASE DEFAULT		!Denounce any unknown operation codes.
            WRITE (6,*) "Unrecognised code:",CODE(I:I)	!This is why a space is treated separately.
          END SELECT		!So much for that operation code.
        END DO			!On to the next.
      END SUBROUTINE HQ9	!That was odd.

      PROGRAM POKE
      CALL HQ9("hq9")
      END
```


To show that the juggling works,

```txt

 Hello, world!
 hq9
99 bottles of beer on the wall,
99 bottles of beer.
Take one down, pass it around,
98 bottles of beer on the wall.
98 bottles of beer on the wall,
98 bottles of beer.
Take one down, pass it around,
97 bottles of beer on the wall.
...
Take one down, pass it around,
 2 bottles of beer on the wall.
 2 bottles of beer on the wall,
 2 bottles of beer.
Take one down, pass it around,
 1 bottle of beer on the wall,
 1 bottle of beer.
Take one down, pass it around,
 No bottles of beer on the wall.
 Go to the store, buy some more.

```




## FreeBASIC


```freebasic

' Intérprete de HQ9+
' FB 1.05.0 Win64
'

Dim Shared codigo As String: codigo = ""

Function HQ9plus(codigo As String) As String
    Dim As Byte botellas, cont
    Dim acumulador As Uinteger = 0
    Dim HQ9plus1 As String
    For cont = 1 To Len(codigo)
        Select Case Ucase(Mid(codigo, cont, 1))
        Case "H"
            HQ9plus1 = HQ9plus1 + "Hello, world!"
        Case "Q"
            HQ9plus1 = HQ9plus1 + codigo
        Case "9"
            For botellas = 99 To 1 Step -1
                HQ9plus1 = HQ9plus1 + Str(botellas) + " bottle"
                If (botellas > 1) Then HQ9plus1 = HQ9plus1 + "s"
                HQ9plus1 = HQ9plus1 + " of beer on the wall, " + Str(botellas) + " bottle"
                If (botellas > 1) Then HQ9plus1 = HQ9plus1 + "s"
                HQ9plus1 = HQ9plus1 + " of beer,"  + Chr(13) + Chr(10) +_
                "Take one down, pass it around, " + Str(botellas - 1) + " bottle"
                If (botellas > 2) Then HQ9plus1 = HQ9plus1 + "s"
                HQ9plus1 = HQ9plus1 + " of beer on the wall." + Chr(13) + Chr(10) + Chr(10)
            Next botellas
            HQ9plus1 = HQ9plus1 + "No more bottles of beer on the wall, no more bottles of beer." +_
            Chr(13) + Chr(10) + "Go to the store and buy some more, 99 bottles of beer on the wall."
        Case "+"
            acumulador = (acumulador + 1)
        Case "E"
            End
        Case Else
            'no es una instrucción válida
        End Select
        If Mid(codigo, cont, 1) <> "+" Then
            HQ9plus1 = HQ9plus1 + Chr(13) + Chr(10)
        End If
    Next cont
    HQ9plus = Left(HQ9plus1, (Len(HQ9plus1) - 2))
End Function


Cls
Do
    Input codigo
    Print HQ9plus(codigo): Print
Loop While Inkey <> Chr(27)
End

```




## Go


See [[RCHQ9+/Go]].


## Golo


```golo
module hq9plus

function main = |args| {
  var accumulator = 0
  let source = readln("please enter your source code: ")
  foreach ch in source: chars() {
    case {
      when ch == 'h' or ch == 'H' {
        println("Hello, world!")
      }
      when ch == 'q' or ch == 'Q' {
        println(source)
      }
      when ch == '9' {
        ninety9Bottles()
      }
      when ch == '+' {
        accumulator = accumulator + 1
      }
      otherwise {
        println("syntax error")
      }
    }
  }
}

function bottles = |amount| -> match {
  when amount == 1 then "One bottle"
  when amount == 0 then "No bottles"
  otherwise amount + " bottles"
}

function ninety9Bottles = {
  foreach n in [99..0]: decrementBy(1) {
    println(bottles(n) + " of beer on the wall,")
    println(bottles(n) + " of beer!")
    println("Take one down, pass it around,")
    println(bottles(n - 1) + " of beer on the wall!")
  }
}

```



## Haskell


See [[Execute HQ9+/Haskell]].


## Haxe


```javascript
// live demo: http://try.haxe.org/#2E7D4
static function hq9plus(code:String):String {
	var out:String = "";
	var acc:Int = 0;
	for (position in 0 ... code.length) switch (code.charAt(position)) {
		case "H", "h": out += "Hello, World!\n";
		case "Q", "q": out += '$code\n';
		case "9":
			var quantity:Int = 99;
			while (quantity > 1) {
				out += '$quantity bottles of beer on the wall, $quantity bottles of beer.\n';
				out += 'Take one down and pass it around, ${--quantity} bottles of beer.\n';
			}
			out += "1 bottle of beer on the wall, 1 bottle of beer.\n" +
				"Take one down and pass it around, no more bottles of beer on the wall.\n\n" +
				"No more bottles of beer on the wall, no more bottles of beer.\n" +
				"Go to the store and buy some more, 99 bottles of beer on the wall.\n";
		case "+": acc++;
	}
	return out;
}
```



## Inform 7



```inform7
HQ9+ is a room.

After reading a command:
	interpret the player's command;
	reject the player's command.

To interpret (code - indexed text):
	let accumulator be 0;
	repeat with N running from 1 to the number of characters in code:
		let C be character number N in code in upper case;
		if C is "H":
			say "Hello, world!";
		otherwise if C is "Q":
			say "[code][line break]";
		otherwise if C is "9":
			repeat with iteration running from 1 to 99:
				let M be 100 - iteration;
				say "[M] bottle[s] of beer on the wall[line break]";
				say "[M] bottle[s] of beer[line break]";
				say "Take one down, pass it around[line break]";
				say "[M - 1] bottle[s] of beer on the wall[paragraph break]";
		otherwise if C is "+":
			increase accumulator by 1.
```


=={{header|Icon}} and {{header|Unicon}}==
Process HQ9+ from command line arguments and input until an error or end-of file.

```Icon
procedure main(A)
repeat writes("Enter HQ9+ code: ") & HQ9(get(A)|read()|break)
end

procedure HQ9(code)
static bnw,bcr
initial {  # number matching words and line feeds for the b-th bottle
   bnw   := table(" bottles"); bnw[1] := " bottle"; bnw[0] := "No more bottles"
   bcr   := table("\n"); bcr[0]:=""
   }
every c := map(!code) do                         # ignore case
   case c of {                                   # interpret
   "h"  : write("Hello, World!")                 # . hello
   "q"  : write(code)                            # . quine
   "9"  : {                                      # . 99 bottles
          every b := 99 to 1 by -1 do writes(
             bcr[b],b,bnw[b]," of beer on the wall\n",
             b,bnw[b]," of beer\nTake one down, pass it around\n",
             1~=b|"",bnw[b-1]," of beer on the wall",bcr[b-1])
          write(", ",map(bnw[b-1])," of beer.\nGo to the store ",
                "and buy some more, 99 bottles of beer on the wall.")
          }
   "+"  : { /acc := 0 ; acc +:=1 }                # . yes it is weird
   default: stop("Syntax error in ",code)         # . error/exit
   }
return
end
```


## J


From [[99_Bottles_of_Beer#J|99 Bottles of Beer]]

```J
bob =: ": , ' bottle' , (1 = ]) }. 's of beer'"_
bobw=: bob , ' on the wall'"_
beer=: bobw , ', ' , bob , '; take one down and pass it around, ' , bobw@<:
```


The rest of the interpreter:

```J
H=: smoutput bind 'Hello, world!'
Q=: smoutput @ [
hq9=: smoutput @: (beer"0) bind (1+i.-99)
hqp=: (A=:1)1 :'0 0$A=:A+m[y'@]

hq9p=: H`H`Q`Q`hq9`hqp@.('HhQq9+' i. ])"_ 0~
```


Example use:


```J
   hq9p 'hqQQq'
Hello, world!
hqQQq
hqQQq
hqQQq
hqQQq
```



## Java


See [[RCHQ9+/Java]].


## JavaScript

The function below executes a HQ9+ program and returns the program output as a string.

```javascript
function hq9plus(code) {
  var out = '';
  var acc = 0;

  for (var i=0; i<code.length; i++) {
    switch (code.charAt(i)) {
      case 'H': out += "hello, world\n"; break;
      case 'Q': out += code + "\n"; break;
      case '9':
        for (var j=99; j>1; j--) {
          out += j + " bottles of beer on the wall, " + j + " bottles of beer.\n";
          out += "Take one down and pass it around, " + (j-1) + " bottles of beer.\n\n";
        }
        out += "1 bottle of beer on the wall, 1 bottle of beer.\n" +
            "Take one down and pass it around, no more bottles of beer on the wall.\n\n" +
            "No more bottles of beer on the wall, no more bottles of beer.\n" +
            "Go to the store and buy some more, 99 bottles of beer on the wall.\n";
        break;
      case '+': acc++; break;
    }
  }
  return out;
}
```



## Julia

```julia
hello()   = println("Hello, world!")
quine()   = println(src)
bottles() = for i = 99:-1:1 print("\n$i bottles of beer on the wall\n$i bottles of beer\nTake one down, pass it around\n$(i-1) bottles of beer on the wall\n") end
acc = 0
incr()    = global acc += 1

const dispatch = Dict(
'h' => hello,
'q' => quine,
'9' => bottles,
'+' => incr)

if length(ARGS) < 1
    println("Usage: julia ./HQ9+.jl file.hq9")
    exit(1)
else
    file = ARGS[1]
end

try
    open(file) do s
        global src = readstring(s)
    end
catch
    warning("can't open $file")
    exit(1)
end

for i in lowercase(src)
    if haskey(dispatch, i) dispatch[i]() end
end
```



## Kotlin


```scala
// version 1.1.3

fun hq9plus(code: String) {
    var acc = 0
    val sb = StringBuilder()
    for (c in code) {
        sb.append(
            when (c) {
                'h', 'H' -> "Hello, world!\n"
                'q', 'Q' -> code + "\n"
                '9'-> {
                    val sb2 = StringBuilder()
                    for (i in 99 downTo 1) {
                        val s = if (i > 1) "s" else ""
                        sb2.append("$i bottle$s of beer on the wall\n")
                        sb2.append("$i bottle$s of beer\n")
                        sb2.append("Take one down, pass it around\n")
                    }
                    sb2.append("No more bottles of beer on the wall!\n")
                    sb2.toString()
                 }
                '+'  -> { acc++; "" }  // yeah, it's weird!
                else -> throw IllegalArgumentException("Code contains illegal operation '$c'")
            }
        )
    }
    println(sb)
}

fun main(args: Array<String>) {
    val code = args[0]  // pass in code as command line argument (using hq9+)
    hq9plus(code)
}
```


```txt

Hello, world!
hq9+
99 bottles of beer on the wall
99 bottles of beer
Take one down, pass it around
....
2 bottles of beer on the wall
2 bottles of beer
Take one down, pass it around
1 bottle of beer on the wall
1 bottle of beer
Take one down, pass it around
No more bottles of beer on the wall!

```



## Liberty BASIC


```lb
'Try this hq9+ program - "hq9+HqQ+Qq"
Prompt "Please input your hq9+ program."; code$
Print hq9plus$(code$)
End

Function hq9plus$(code$)
    For i = 1 to Len(code$)
        Select Case
            Case Upper$(Mid$(code$, i, 1)) = "H"
                hq9plus$ = hq9plus$ + "Hello, world!"
            Case Upper$(Mid$(code$, i, 1)) = "Q"
                hq9plus$ = hq9plus$ + code$
            Case Mid$(code$, i, 1) = "9"
                For bottles = 99 To 1 Step -1
                     hq9plus$ = hq9plus$ + str$(bottles) + " bottle"
                     If (bottles > 1) Then hq9plus$ = hq9plus$ + "s"
                     hq9plus$ = hq9plus$ + " of beer on the wall, " + str$(bottles) + " bottle"
                     If (bottles > 1) Then hq9plus$ = hq9plus$ + "s"
                     hq9plus$ = hq9plus$ + " of beer,"  + chr$(13) + chr$(10) + "Take one down, pass it around, " + str$(bottles - 1) + " bottle"
                     If (bottles > 2) Or (bottles = 1) Then hq9plus$ = hq9plus$ + "s"
                     hq9plus$ = hq9plus$ + " of beer on the wall." + chr$(13) + chr$(10)
                Next bottles
                hq9plus$ = hq9plus$ + "No more bottles of beer on the wall, no more bottles of beer." _
                                    + chr$(13) + chr$(10) + "Go to the store and buy some more, 99 bottles of beer on the wall."
            Case Mid$(code$, i, 1) = "+"
                accumulator = (accumulator + 1)
        End Select
        If Mid$(code$, i, 1) <> "+" Then
            hq9plus$ = hq9plus$ + chr$(13) + chr$(10)
        End If
    Next i
    hq9plus$ = Left$(hq9plus$, (Len(hq9plus$) - 2))
End Function
```



## Lua


```lua

function runCode( code )
    local acc, lc = 0
    for i = 1, #code do
        lc = code:sub( i, i ):upper()
        if lc == "Q" then print( lc )
        elseif lc == "H" then print( "Hello, World!" )
        elseif lc == "+" then acc = acc + 1
        elseif lc == "9" then
            for j = 99, 1, -1 do
                if j > 1 then
                    print( string.format( "%d bottles of beer on the wall\n%d bottles of beer\nTake one down, pass it around\n%d bottles of beer on the wall\n", j, j, j - 1 ) )
                else
                    print( "1 bottle of beer on the wall\n1 bottle of beer\nTake one down and pass it around\nno more bottles of beer on the wall\n\n"..
                           "No more bottles of beer on the wall\nNo more bottles of beer\n"..
                           "Go to the store and buy some more\n99 bottles of beer on the wall.\n" )
                end
            end
        end
    end
end

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

<lang>hq9plus[program_] :=
 Module[{accumulator = 0, bottle},
  bottle[n_] :=
   ToString[n] <> If[n == 1, " bottle", " bottles"] <> " of beer";
  Do[Switch[chr, "H", Print@"hello, world", "Q", Print@program, "9",
    Print@StringJoin[
      Table[bottle[n] <> " on the wall\n" <> bottle[n] <>
        "\ntake one down, pass it around\n" <> bottle[n - 1] <>
        " on the wall" <> If[n == 1, "", "\n\n"], {n, 99, 1, -1}]],
    "+", accumulator++], {chr, Characters@program}]; accumulator]
```



## MiniScript


```MiniScript
code = input("Enter HQ9+ program: ")

sing = function()
    for i in range(99,2)
        print i + " bottles of beer on the wall, " + i + " bottles of beer"
        print "Take one down, pass it around, " + (i-1) + " bottle" + "s"*(i>2) + " of beer on the wall"
    end for
    print "1 bottle of beer on the wall, 1 bottle of beer"
    print "Take one down, pass it around, no bottles of beer on the wall!"
end function

accumulator = 0
for c in code
    c = c.lower
    if c == "h" then print "Hello World"
    if c == "q" then print code
    if c == "9" then sing
    if c == "+" then accumulator = accumulator + 1
end for
```

```txt
Enter HQ9+ program: hq9+
Hello World
hq9+
99 bottles of beer on the wall, 99 bottles of beer
Take one down, pass it around, 98 bottles of beer on the wall
98 bottles of beer on the wall, 98 bottles of beer
Take one down, pass it around, 97 bottles of beer on the wall
...
2 bottles of beer on the wall, 2 bottles of beer
Take one down, pass it around, 1 bottle of beer on the wall
1 bottle of beer on the wall, 1 bottle of beer
Take one down, pass it around, no bottles of beer on the wall!
```



## NetRexx

See [[RCHQ9+/NetRexx]].


## Nim


Modify contents of the program variable as you see fit.


```nim

var program = "9hHqQ+"
var i = 0

proc bottle(n: int): string =
  case n
  of 0:
    result = "No more bottles"
  of 1:
    result = "1 bottle"
  else:
    result = $n & " bottles"

proc ninetyNineBottles =
  for n in countdown(99, 1):
    echo bottle(n), " bottle of beer on the wall"
    echo bottle(n), " bottle of beer"
    echo "Take one down, pass it around"
    echo bottle(n - 1), " of beer on the wall"

for token in items(program):
  case token
  of 'h', 'H':
    echo("Hello, world!")
  of 'q', 'Q':
    echo(program)
  of '9':
    ninetyNineBottles()
  of '+':
    inc(i)
  else:
    echo("Unknown command: ", token)

```


=={{header|NS-HUBASIC}}==
<lang NS-HUBASIC>10 INPUT "INPUT HQ9+ CODE: ",I$
20 B$=" BOTTLES OF BEER"
30 W$=" ON THE WALL"
40 FOR I=1 TO LEN(I$)
50 C$=MID$(I$,I,1)
60 IF C$="H" THEN PRINT "HELLO, WORLD!"
70 IF C$="Q" THEN PRINT I$
80 A=A+(C$="+")
90 IF C$<>"9" GOTO 170
100 FOR B=99 TO 1 STEP -1
110 PRINT B B$ W$
120 PRINT B B$
130 PRINT "TAKE ONE DOWN,"
140 PRINT "PASS IT AROUND"
150 PRINT B-1 B$ W$
160 NEXT
170 NEXT
```



## OCaml

Regrettably, HQ9+ suffers from remarkably poor implementations, even though the spec nailed down every aspect of the language (apart from the exact lyrics of the '9' operation, this obviously to allow for localization.) What's worse, the only implementation linked from the spec, when it was accessible, was an OCaml work that <i>refused to implement the '+' operation</i> among its several other deviations. The following code borrows 'beer' from its page.


```ocaml
let hq9p line =
  let accumulator = ref 0 in
  for i = 0 to (String.length line - 1) do
    match line.[i] with
    | 'h' | 'H' -> print_endline "Hello, world!"
    | 'q' | 'Q' -> print_endline line
    | '9' -> beer 99
    | '+' -> incr accumulator
  done
```



## PARI/GP

Unlike many other implementations, this version will not overflow when the accumulator hits 2<sup>64</sup> (or as low as 2<sup>31</sup> in some versions).

The lyrics are based on the reference implementation.  The endline and case-insensitivity are from an example in the spec.

```parigp
beer(n)={
  if(n == 1,
    print("1 bottle of beer on the wall");
    print("1 bottle of beer");
    print("Take one down and pass it around");
    print("No bottles of beer on the wall")
  ,
    print(n" bottles of beer on the wall");
    print(n" bottles of beer");
    print("Take one down and pass it around");
    print(n-1," bottles of beer on the wall\n");
    beer(n-1)
  )
};
HQ9p(s)={
  my(accum=0,v=Vec(s));
  for(i=1,#s,
    if(v[i] == "H" || v[i] == "h", print("Hello, world!"); next);
    if(v[i] == "Q" || v[i] == "q", print(s); next);
    if(v[i] == "9", beer(99); next);
    if(v[i] == "+", accum++, error("Nasal demons"))
  )
};
```


Sample input/output:

```txt
>HQ9p("qqqq")
qqqq
qqqq
qqqq
qqqq
```



## Perl

This implementation uses the ''switch'' feature.

```perl
#!/usr/bin/perl
use warnings;
use strict;
use feature qw(say switch);

my @programme = <> or die "No input. Specify a program file or pipe it to the standard input.\n";

for (@programme) {
    for my $char (split //) {
        given ($char) {
            when ('H') { hello()             }
            when ('Q') { quinne(@programme)  }
            when ('9') { bottles()           }
            default    { die "Unknown instruction $char.\n" } # Comment this line to ignore other instructions.
        }
    }
}

sub hello {
    print 'Hello World';
}

sub quinne {
    print @programme;
}

sub bottles {
    for my $n (reverse 0 .. 99) {
        my $before = bottle_count($n);
        my $after  = bottle_count($n - 1);
        my $action = bottle_action($n);
        say "\u$before of beer on the wall, $before of beer.";
        say "$action, $after of beer on the wall.";
        say q() if $n;
    }
}

sub bottle_count {
    my $n = shift;
    given ($n) {
        when    (-1) { return '99 bottles'      }
        when    (0)  { return 'no more bottles' }
        when    (1)  { return '1 bottle'        }
        default      { return "$n bottles"      }
    }
}

sub bottle_action {
    my $n = shift;
    return 'Take one down and pass it around' if $n > 0;
    return 'Go to the store and buy some more';
}
```



## Perl 6

The spec is kind of vague about how to do error handling... and whether white space is significant... and how the accumulator should be accessed... and pretty much everything else too.


```perl6
class HQ9Interpreter {
    has @!code;
    has $!accumulator;
    has $!pointer;

    method run ($code) {
        @!code = $code.comb;
        $!accumulator = 0;
        $!pointer = 0;
        while $!pointer < @!code {
            given @!code[$!pointer].lc {
                when 'h' { say 'Hello world!' }
                when 'q' { say @!code }
                when '9' { bob(99) }
                when '+' { $!accumulator++ }
                default  { note "Syntax error: Unknown command \"{@!code[$!pointer]}\"" }
            }
	    $!pointer++;
        }
    }
    sub bob ($beer is copy) {
        sub what  { "{$beer??$beer!!'No more'} bottle{$beer-1??'s'!!''} of beer" };
        sub where { 'on the wall' };
        sub drink { $beer--; "Take one down, pass it around," }
        while $beer {
            .say for "&what() &where(),", "&what()!",
                     "&drink()", "&what() &where()!", ''
        }
    }
}

# Feed it a command string:

my $hq9 = HQ9Interpreter.new;
$hq9.run("hHq+++Qq");
say '';
$hq9.run("Jhq.k+hQ");
```


Output:

```txt

Hello world!
Hello world!
hHq+++Qq
hHq+++Qq
hHq+++Qq

Syntax error: Unknown command "J"
Hello world!
Jhq.k+hQ
Syntax error: Unknown command "."
Syntax error: Unknown command "k"
Hello world!
Jhq.k+hQ

```


Or start a REPL (Read Execute Print Loop) and interact at the command line:

```perl6
my $hq9 = HQ9Interpreter.new;
while 1 {
    my $in = prompt('HQ9+>').chomp;
    last unless $in.chars;
    $hq9.run($in)
}
```



## Phix

copied from [[99_Bottles_of_Beer#Phix|99_Bottles_of_Beer]]

```Phix
constant ninetynine = 99 -- (set this to 9 for testing)

function bottles(integer count)
    if count=0 then     return "no more bottles"
    elsif count=1 then  return "1 bottle" end if
    if count=-1 then count = ninetynine end if
    return sprintf("%d bottles",count)
end function

function bob(integer count)
    return bottles(count)&" of beer"
end function

function up1(string bob)
-- Capitalise sentence start (needed just the once, "no more"=>"No more")
    bob[1] = upper(bob[1])
    return bob
end function

procedure ninetyninebottles()
string this = bob(ninetynine)
string that = "Take one down, pass it around,\n"
    for i=ninetynine to 0 by -1 do
        puts(1,up1(this)&" on the wall,\n")
        puts(1,this&".\n")
        if i=0 then that = "Go to the store, buy some more,\n"
        elsif i=1 then that[6..8] = "it" end if
        this = bob(i-1)
        puts(1,that&this&" on the wall.\n\n")
    end for
--  if getc(0) then end if
end procedure
```

the interpreter

```Phix
procedure hq9(string code)
integer accumulator = 0
    for i=1 to length(code) do
        switch(upper(code[i]))
            case 'H': printf(1,"Hello, world!\n")
            case 'Q': printf(1,"%s\n", code);
            case '9': ninetyninebottles()
            case '+': accumulator += 1
        end switch
    end for
end procedure

hq9("hq9+HqQ+Qq")
```



## PicoLisp


```PicoLisp
(de hq9+ (Code)
   (let Accu 0
      (for C (chop Code)
         (case C
            ("H" (prinl "Hello, world"))
            ("Q" (prinl Code))
            ("9"
               (for (N 99 (gt0 N))
                  (prinl N " bottles of beer on the wall")
                  (prinl N " bottles of beer")
                  (prinl "Take one down, pass it around")
                  (prinl (dec 'N) " bottles of beer on the wall")
                  (prinl) ) )
            ("+" (inc 'Accu)) ) )
      Accu ) )
```



## PowerShell

I'm not sure why, but it bothered me that the '+' function of HQ9+ was completely useless.  So I added the <code>-Global</code>
switch option.  When specified it creates and/or increments a variable named '+' in the global scope.

This program treats all text except for 'H','Q','9' and '+' as whitespace and when 'Q' is called, returns the exact text.  This
is only my preference and could be easily modified.

As far as I can tell, there are no errors in HQ9+; but, supposing there are, a 'Default' could be added to the switch statement.

```PowerShell

function Invoke-HQ9PlusInterpreter ([switch]$Global)
{
    $sb = New-Object -TypeName System.Text.StringBuilder

    for ($i = 99; $i -gt 2; $i--)
    {
        $sb.Append((("{0,2} bottles of beer on the wall, " +
                     "{0,2} bottles of beer! Take one down, pass it around, " +
                     "{1,2} bottles of beer on the wall.`n") -f $i, ($i - 1))) | Out-Null
    }
    $sb.Append((" 2 bottles of beer on the wall, " +
                " 2 bottles of beer! Take one down, pass it around, " +
                " 1 bottle  of beer on the wall.`n")) | Out-Null
    $sb.Append((" 1 bottle  of beer on the wall, " +
                " 1 bottle  of beer! Take one down, pass it around...`n")) | Out-Null
    $sb.Append(("No more bottles of beer on the wall, No more bottles of beer!`n" +
                "Go to the store and get us some more, 99 bottles of beer on the wall!")) | Out-Null

    $99BottlesOfBeer = $sb.ToString()

    $helloWorld = "Hello, world!"

    if ($Global) {New-Variable -Name "+" -Value 0 -Scope Global -ErrorAction SilentlyContinue}

    Write-Host "Press Ctrl-C or Enter nothing to exit." -ForegroundColor Cyan

    while ($code -ne "")
    {
        $code = Read-Host -Prompt "HQ9+"

        ($code.ToCharArray() | Select-String -Pattern "[HQ9+]").Matches.Value | ForEach-Object {
            switch ($_)
            {
                "H" {$helloWorld;           break}
                "Q" {$code;                 break}
                "9" {$99BottlesOfBeer;      break}
                "+" {if ($Global) {${global:+}++}}
            }
        }
    }
}

Set-Alias -Name HQ9+ -Value Invoke-HQ9PlusInterpreter

```

Example sessions:

```txt

PS C:\Scripts> HQ9+ -Global
Press Ctrl-C or Enter nothing to exit.
HQ9+: 9headquarters++
99 bottles of beer on the wall, 99 bottles of beer! Take one down, pass it around, 98 bottles of beer on the wall.
98 bottles of beer on the wall, 98 bottles of beer! Take one down, pass it around, 97 bottles of beer on the wall.
.
.
.
 3 bottles of beer on the wall,  3 bottles of beer! Take one down, pass it around,  2 bottles of beer on the wall.
 2 bottles of beer on the wall,  2 bottles of beer! Take one down, pass it around,  1 bottle  of beer on the wall.
 1 bottle  of beer on the wall,  1 bottle  of beer! Take one down, pass it around...
No more bottles of beer on the wall, No more bottles of beer!
Go to the store and get us some more, 99 bottles of beer on the wall!
Hello, world!
9headquarters++
HQ9+:

PS C:\Scripts> ${+}
2

PS C:\Scripts> HQ9+ -Global
Press Ctrl-C or Enter nothing to exit.
HQ9+: hq++
Hello, world!
hq++
HQ9+:

PS C:\Scripts> ${+}
4


```



## PureBasic


```PureBasic
Procedure hq9plus(code.s)
  Protected accumulator, i, bottles
  For i = 1 To Len(code)
    Select Mid(code, i, 1)
      Case "h", "H"
        PrintN("Hello, world!")
      Case "q", "Q"
        PrintN(code)
      Case "9"
        bottles = 99
        While bottles
          PrintN(Str(bottles) + " bottles of beer on the wall, " + Str(bottles) + " bottles of beer,")
          bottles - 1
          PrintN("Take one down, pass it around, " + Str(bottles) + " bottles of beer on the wall.")
        Wend
      Case "+"
        accumulator + 1
    EndSelect
  Next i
EndProcedure

If OpenConsole()
  Define testCode.s = "hq9+HqQ+Qq"
  hq9plus(testCode)

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```



## Python


See [[RCHQ9+/Python]].


## Racket


Getting HQ9+ code from standard output is not easy without a trailing newline.
So I've extended the language (maybe the program could do with a flag to allow/
disallow newlines). To make up for that slackness, this implementation is
strictly case-sensitive.


```racket
#lang racket
; if we `for` over the port, we won't have the program in memory for 'Q'
(define (parse-HQ9+ the-program)
  (define oTW " on the wall")
  (and ; ensures the accumulator is never seen!
   (for/fold ((A 0))
     ((token (in-string the-program)))
     (case token
       ((#\H) (display "hello, world") A)
       ((#\Q) (display the-program) A)
       ;; official esolang version of 99-BoB at:
       ;;  http://esolangs.org/wiki/99_bottles_of_beer
       ((#\9)
        (displayln
        (let ((BoB (lambda (n)
                     (string-append
                      (case n ((1) "1 bottle") ((0) "No bottles")
                        (else (format "~a bottles" n)))
                      " of beer"))))
          (string-join
           (for/list ((btls (in-range 99 0 -1)))
            (string-append (BoB btls)oTW",\n"(BoB btls)
                           ".\nTake one down, pass it around,\n"
                           (BoB (sub1 btls))oTW"."))
           "\n\n"))) A)
       ((#\+) (add1 A))
       ((#\newline) ; language extension, makes getting standard in easier
        (eprintf "warning: HQ9+: language extension ~s" token)
        A)
       (else (error "syntax error: HQ9+: unrecognised token ~s" token))))
   (void)))

(module+ main (parse-HQ9+ (port->string)))

(module+ test
  (require rackunit)
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ ""))) "")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "H"))) "hello, world")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "Q"))) "Q")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "QQ"))) "QQQQ")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "+"))) "")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "+"))) "")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "++"))) "")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "+++"))) "")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ "+++++++++++++++++"))) "")
  (check-equal? (with-output-to-string (lambda () (parse-HQ9+ (make-string 10000 #\+)))) "")
  ;;; you can jolly well read (and sing along to) the output of '9'
  )
```



## REXX

Note that the actual text of the   ''Hello, world!''   message can differ among definitions.

```rexx
/*REXX program implements the   HQ9+   language. ───────────────────────────────────────*/
arg pgm .                                                    /*obtain optional argument.*/
accumulator=0                                                /*assign default to accum. */

      do instructions=1  for length(pgm);               ?=substr(pgm, instructions, 1)
           select
           when ?=='H' then say "Hello, world!"              /*text varies on definition*/
           when ?=='Q' then do j=1  for sourceline();   say sourceline(j);   end  /*j*/
           when ?== 9  then call 99
           when ?=='+' then accumulator=accumulator+1
           otherwise say 'invalid HQ9+ instruction:' ?
           end   /*select*/
      end        /*instructions*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
99:           do j=99  by -1  to 1
              say j 'bottle's(j)  "of beer the wall,"
              say j 'bottle's(j)  "of beer."
              say 'Take one down, pass it around,'
              n=j-1
              if n==0 then n='no'                                 /*cheating to use  0. */
              say n  'bottle's(j-1)  "of beer the wall."
              say
              end   /*j*/
    say 'No more bottles of beer on the wall,'                    /*finally, last verse.*/
    say 'no more bottles of beer.'
    say 'Go to the store and buy some more,'
    say '99 bottles of beer on the wall.'
    return
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:  if arg(1)==1  then return '';             return "s"          /*a simple pluralizer.*/
```

'''output'''   when using the input of:   <tt> HHH </tt>

```txt

Hello, world!
Hello, world!
Hello, world!

```



## Ring


```ring

# Project : Execute HQ9

bottle("hq9+HqQ+Qq")

func bottle(code)
     accumulator = 0
     for i = 1 to len(code)
         switch code[i]
                on "h"
                    see "Hello, world!" + nl
                on "H"
                    see "hello, world!" + nl
                on "q"
                    see code + nl
                on "Q"
                    see code + nl
                on "9"
                    bottles = 99
                    while bottles > 0
                          see "" + bottles + " bottles of beer on the wall, "
                          see "" + bottles + " bottles of beer," + nl
                          bottles = bottles - 1
                          see "take one down, pass it around, "
                          see "" + bottles + " bottles of beer on the wall." + nl
                    end
                on "+"
                    accumulator = accumulator + 1
         off
     next

```

Output:

```txt

Hello, world!
hq9+HqQ+Qq
99 bottles of beer on the wall, 99 bottles of beer,
Take one down, pass it around, 98 bottles of beer on the wall.
98 bottles of beer on the wall, 98 bottles of beer,
Take one down, pass it around, 97 bottles of beer on the wall.
...
3 bottles of beer on the wall, 3 bottles of beer,
Take one down, pass it around, 2 bottles of beer on the wall.
2 bottles of beer on the wall, 2 bottles of beer,
Take one down, pass it around, 1 bottles of beer on the wall.
1 bottles of beer on the wall, 1 bottles of beer,
Take one down, pass it around, 0 bottles of beer on the wall.
Hello, world!
hq9+HqQ+Qq
hq9+HqQ+Qq
hq9+HqQ+Qq
hq9+HqQ+Qq

```



## Ruby


See [[RCHQ9+/Ruby]].



## Rust


```rust
use std::env;

// HQ9+ requires that '+' increments an accumulator, but it's inaccessible (and thus, unused).
#[allow(unused_variables)]
fn execute(code: &str) {
    let mut accumulator = 0;

    for c in code.chars() {
        match c {
            'Q' => println!("{}", code),
            'H' => println!("Hello, World!"),
            '9' => {
                for n in (1..100).rev() {
                    println!("{} bottles of beer on the wall", n);
                    println!("{} bottles of beer", n);
                    println!("Take one down, pass it around");
                    if (n - 1) > 1 {
                        println!("{} bottles of beer on the wall\n", n - 1);
                    } else {
                        println!("1 bottle of beer on the wall\n");
                    }
                }
            }
            '+' => accumulator += 1,
            _ => panic!("Invalid character '{}' found in source.", c),
        }
    }
}

fn main() {
    execute(&env::args().nth(1).unwrap());
}
```



## Scala


```Scala
def hq9plus(code: String) : String = {
    var out = ""
    var acc = 0

    def bottle(num: Int) : Unit = {
        if (num > 1) {
            out += num + " bottles of beer on the wall, " + num + " bottles of beer.\n"
            out += "Take one down and pass it around, " + (num - 1) + " bottle"

            if (num > 2) out += "s"

            out += " of beer.\n\n"
            bottle(num - 1)
        }
        else {
            out += "1 bottle of beer on the wall, 1 bottle of beer.\n" +
                "Take one down and pass it around, no more bottles of beer on the wall.\n\n" +
                "No more bottles of beer on the wall, no more bottles of beer.\n" +
                "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
        }
    }

    def handle(char: Char) = char match {
        case 'H' => out += "Hello world!\n"
        case 'Q' => out += code + "\n"
        case '+' => acc += 1
        case '9' => bottle(99)
    }

    code.toList foreach handle
    out
}

println(hq9plus("HQ9+"))

```



## Seed7

The program below accepts the HQ9+ program as command line parameter:


```seed7
$ include "seed7_05.s7i";

const proc: runCode (in string: code) is func
  local
    var char: ch is ' ';
    var integer: bottles is 0;
    var integer: accumulator is 0;
  begin
    for ch range code do
      case ch of
        when {'H'}: writeln("Hello, world!");
        when {'Q'}: writeln(code);
        when {'9'}: bottles := 99;
                    repeat
                      writeln(bottles <& " bottles of beer on the wall");
                      writeln(bottles <& " bottles of beer");
                      writeln("Take one down, pass it around");
                      decr(bottles);
                      writeln(bottles <& " bottles of beer on the wall");
                      writeln;
                    until bottles = 0;
        when {'+'}: incr(accumulator);
      end case;
    end for;
  end func;

const proc: main is func
  begin
    if length(argv(PROGRAM)) >= 1 then
      runCode(argv(PROGRAM)[1]);
    end if;
  end func;
```



## Sidef

```ruby
class HQ9Interpreter {
    has pointer;
    has accumulator;

    func bob (beer) {
        func what  { "#{beer ? beer : 'No more'} bottle#{beer-1 ? 's' : ''} of beer" }
        func where { 'on the wall' }
        func drink { beer--; "Take one down, pass it around," }

        while (beer.is_pos) {
            [[what(), where()], [what()],
            [drink()], [what(), where()], []].each{.join(' ').say}
        }
    }

    method run (code) {
        var chars = code.chars;
        accumulator = 0;
        pointer = 0;
        while (pointer < chars.len) {
            given (chars[pointer].lc) {
                when ('h') { say 'Hello world!' }
                when ('q') { say code }
                when ('9') { bob(99) }
                when ('+') { accumulator++ }
                default    { warn %Q(Syntax error: Unknown command "#{chars[pointer]}") }
            }
            pointer++;
        }
    }
}
```


Usage:

```ruby
var hq9 = HQ9Interpreter();
hq9.run("hHq+++Qq");
```


```txt

Hello world!
Hello world!
hHq+++Qq
hHq+++Qq
hHq+++Qq

```


Or start a REPL (Read Execute Print Loop) and interact at the command line:

```ruby
var hq9 = HQ9Interpreter();
loop {
    var in = read('HQ9+>', String) \\ break;
    hq9.run(in)
}
```



## Tcl


See [[RCHQ9+/Tcl]].


## Ursa


see [[Execute HQ9+/Ursa]]


## Ursala


See [[RCHQ9+/Ursala]].


## XSLT



### XSLT 1.0



### =Basic implementation=


Requires <code>bottles.xsl</code> (below).


```xml
<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	<!-- bottles.xsl defines $entire-bottles-song -->
	<xsl:import href="bottles.xsl"/>

	<xsl:output method="text" encoding="utf-8"/>

	<xsl:variable name="hello-world">
		<xsl:text>Hello, world!&#10;</xsl:text>
	</xsl:variable>

	<!-- Main template -->
	<xsl:template match="/">
		<xsl:call-template name="run">
			<xsl:with-param name="code" select="string(.)"/>
		</xsl:call-template>
	</xsl:template>

	<!-- Runs HQ9+ code from string starting at given index (default 1) -->
	<xsl:template name="run">
		<xsl:param name="code"/>
		<xsl:param name="starting-at" select="1"/>

		<!-- Fetches instruction and forces to upper-case -->
		<xsl:variable name="inst" select="translate(substring($code, $starting-at, 1), 'hq', 'HQ')"/>

		<!-- Only if not at end -->
		<xsl:if test="$inst != ''">
			<xsl:choose>
				<xsl:when test="$inst = 'H'">
					<xsl:value-of select="$hello-world"/>
				</xsl:when>
				<xsl:when test="$inst = 'Q'">
					<xsl:value-of select="$code"/>
					<xsl:text>&#10;</xsl:text>
				</xsl:when>
				<xsl:when test="$inst = '9'">
					<xsl:value-of select="$entire-bottles-song"/>
				</xsl:when>
				<xsl:when test="$inst = '+'">
					<!-- XSLT has no meaningful equivalent of write-only variables -->
				</xsl:when>
				<!-- Otherwise, do nothing -->
			</xsl:choose>

			<!-- Proceed with next instruction -->
			<xsl:call-template name="run">
				<xsl:with-param name="code" select="$code"/>
				<xsl:with-param name="starting-at" select="$starting-at + 1"/>
			</xsl:call-template>
		</xsl:if>
	</xsl:template>
</xsl:stylesheet>
```



### ==Details==


Input to this sheet is given by placing the entire source as a single <code><nowiki><code/></nowiki></code> element. For example, to run the example program <code>qqqq</code>, use the sheet to transform the document


```xml><code>qqqq</code></lang


Newlines are added in roughly the same places as in the C version. For example, the program <code>qqqq</code> results in four lines of output rather than one long line.

XSLT has no meaningful way to process a write-only variable like the accumulator, so <code>+</code> is a no-op.

Characters other than <code>HQhq9+</code> are no-ops, but are echoed verbatim by <code>Q</code>/<code>q</code>.


### =Implementation supporting multiple programs and accumulator output=


Requires <code>bottles.xsl</code> (below)


```xml
<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0">
	<!-- bottles.xsl defines $entire-bottles-song -->
	<xsl:import href="bottles.xsl"/>

	<xsl:output method="xml" encoding="utf-8"/>

	<xsl:variable name="hello-world">
		<xsl:text>Hello, world!&#10;</xsl:text>
	</xsl:variable>

	<!-- Main template -->
	<xsl:template match="/">
		<results>
			<xsl:apply-templates select="//code"/>
		</results>
	</xsl:template>

	<!-- <code/> template -->
	<xsl:template match="code">
		<xsl:call-template name="run">
			<xsl:with-param name="code" select="string(.)"/>
		</xsl:call-template>
	</xsl:template>

	<!-- Runs HQ9+ code from string -->
	<xsl:template name="run">
		<xsl:param name="code"/>

		<xsl:call-template name="_run-remaining-code">
			<!-- Initial value is the entire input program plus a newline -->
			<xsl:with-param name="quine" select="concat($code,'&#10;')"/>

			<!-- Initial value is the entire input program with [hq] changed to upper-case -->
			<xsl:with-param name="code" select="translate($code, 'hq', 'HQ')"/>

			<!-- Initial value is empty -->
			<xsl:with-param name="output"/>

			<!-- Initial value is 0 -->
			<xsl:with-param name="accumulator" select="0"/>
		</xsl:call-template>
	</xsl:template>

	<!-- Runs the remainder of some already-started HQ9+ code -->
	<!-- Tail recursion allows this function to effectively update its own state -->
	<xsl:template name="_run-remaining-code">
		<!-- The text to be output on 'Q' -->
		<xsl:param name="quine"/>
		<!-- The remaining instructions for the program, already upper-case -->
		<xsl:param name="code"/>
		<!-- Output that has already been collected -->
		<xsl:param name="output"/>
		<!-- Current accumulator value -->
		<xsl:param name="accumulator"/>

		<!--
			 If there are instructions remaining, runs the next instruction and then recurses.
			 If there are no instructions left, produces the final output and accumulator before exiting.
		-->
		<xsl:choose>
			<xsl:when test="$code = ''">
				<!-- Reached the end of the program; output results -->
				<result>
					<xsl:if test="$accumulator != 0">
						<xsl:attribute name="accumulator"><xsl:value-of select="$accumulator"/></xsl:attribute>
					</xsl:if>
					<xsl:copy-of select="$output"/>
				</result>
			</xsl:when>
			<xsl:otherwise>
				<!-- At least one more instruction; run and recurse -->
				<xsl:variable name="inst" select="substring($code, 1, 1)"/>
				<xsl:variable name="remaining" select="substring($code, 2)"/>

				<!-- Decide what to add to accumulator -->
				<xsl:variable name="accumulator-inc">
					<xsl:choose>
						<xsl:when test="$inst = '+'">1</xsl:when>
						<xsl:otherwise>0</xsl:otherwise>
					</xsl:choose>
				</xsl:variable>

				<!-- Decide what to append to output -->
				<xsl:variable name="output-inc">
					<xsl:choose>
						<xsl:when test="$inst = 'H'"><xsl:value-of select="$hello-world"/></xsl:when>
						<xsl:when test="$inst = 'Q'"><xsl:value-of select="$quine"/></xsl:when>
						<xsl:when test="$inst = '9'"><xsl:value-of select="$entire-bottles-song"/></xsl:when>
					</xsl:choose>
				</xsl:variable>

				<!-- Recurse to continue processing program -->
				<xsl:call-template name="_run-remaining-code">
					<!-- $quine is the $quine originally passed without changes -->
					<xsl:with-param name="quine" select="$quine"/>
					<!-- $code is the $code from this invocation with the first character removed -->
					<xsl:with-param name="code" select="$remaining"/>
					<!-- $output is the $output from this invocation with $output-inc appended -->
					<xsl:with-param name="output">
						<xsl:copy-of select="$output"/>
						<xsl:copy-of select="$output-inc"/>
					</xsl:with-param>
					<!-- $accumulator is the $accumulator from this invocation with $accumulator-inc added -->
					<xsl:with-param name="accumulator" select="$accumulator + $accumulator-inc"/>
				</xsl:call-template>
			</xsl:otherwise>
		</xsl:choose>
	</xsl:template>
</xsl:stylesheet>
```



### ==Details==


This sheet demonstrates the use of a tail-recursive template to simulate a narrowly mutable state, which is used for both the output and the accumulator.

Input to this sheet is given by placing one or more sources as <code><nowiki><code/></nowiki></code> elements. For example, to run the example program <code>qqqq</code>, use the sheet to transform the document


```xml><code>qqqq</code></lang


or the programs <code>qqqq</code> and <code>++++</code> can be run in the same pass by transforming


```xml><programs

  <code>qqqq</code>
  <code>++++</code>
</programs>
```


The output document is a <code><nowiki><results/></nowiki></code> element containing a <code><nowiki><result/></nowiki></code> element for each <code><nowiki><code/></nowiki></code> element processed from the input. If a <code>+</code> appeared in the program, the <code><nowiki><result/></nowiki></code> element will indicate the final value of the accumulator in its <code>accumulator</code> attribute. For example, the output for the latter example, would be


```xml><results><result
qqqq
qqqq
qqqq
qqqq
</result><result accumulator="4"/></results>
```



### =bottles.xsl=


This sheet defines a value for the variable <code>$entire-bottles-song</code> (see [[99 Bottles of Beer]] for the general idea).


```xml
<?xml version="1.0"?>
<xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform" version="1.0" xmlns:lo="urn:uuid:59afd337-03a8-49d9-a7a8-8e2cbc4ef9cc">
	<!-- Note: xmlns:lo is defined as a sort of pseudo-private namespace -->

	<!-- Given a count and a suffix (default " on the wall"), renders one number-containing line of the bottles song -->
	<xsl:template name="lo:line">
		<xsl:param name="count"/>
		<xsl:param name="suffix"> on the wall</xsl:param>
		<xsl:value-of select="$count"/>
		<xsl:text> bottle</xsl:text>
		<xsl:if test="$count != 1">s</xsl:if>
		<xsl:text> of beer</xsl:text>
		<xsl:value-of select="$suffix"/>
		<xsl:text>&#10;</xsl:text>
	</xsl:template>

	<!-- Given a count, renders one verse of the bottles song -->
	<xsl:template name="lo:verse">
		<xsl:param name="count"/>
		<xsl:call-template name="lo:line">
			<xsl:with-param name="count" select="$count"/>
		</xsl:call-template>
		<xsl:call-template name="lo:line">
			<xsl:with-param name="count" select="$count"/>
			<!-- empty suffix for this line -->
			<xsl:with-param name="suffix"/>
		</xsl:call-template>
		<xsl:text>Take one down, pass it around&#10;</xsl:text>
		<xsl:call-template name="lo:line">
			<xsl:with-param name="count" select="$count - 1"/>
		</xsl:call-template>
		<xsl:text>&#10;</xsl:text>
	</xsl:template>

	<!-- Given a starting count, renders the entire bottles song -->
	<xsl:template name="lo:song">
		<xsl:param name="count"/>
		<xsl:if test="$count &gt; 0">
			<xsl:call-template name="lo:verse">
				<xsl:with-param name="count" select="$count"/>
			</xsl:call-template>
			<xsl:call-template name="lo:song">
				<xsl:with-param name="count" select="$count - 1"/>
			</xsl:call-template>
		</xsl:if>
	</xsl:template>

	<!-- The entire bottles song -->
	<xsl:variable name="entire-bottles-song">
		<xsl:call-template name="lo:song">
			<xsl:with-param name="count" select="99"/>
		</xsl:call-template>
	</xsl:variable>

</xsl:stylesheet>
```



## zkl


```zkl
fcn runHQ9(code){
   acc:=0;
   foreach c in (code){
      switch(c){
	 case("H"){ println("hello, world"); }
	 case("Q"){ print(code); }
	 case("+"){ acc+=1; }
	 case("9"){ wall_O_beer(); }
      }
   }
}
fcn wall_O_beer(){
   [99..0,-1].pump(fcn(n){
      println(beers(n), " on the wall, ", beers(n).toLower(), ".\n",
	 n==0 and ("Go to the store and buy some more, 99 bottles of beer") or
	 ("Take one down and pass it around, " + beers(n-1).toLower()),
	 " on the wall.\n")
   });
}
fcn beers(n){
    (n==0 and "No more bottles" or (n==1 and "1 bottle" or "" + n + " bottles"))
    + " of beer"
}
```


```zkl
runHQ9("90HQ+junk");
```

```txt

99 bottles of beer on the wall, 99 bottles of beer.
Take one down and pass it around, 98 bottles of beer on the wall.

98 bottles of beer on the wall, 98 bottles of beer.
Take one down and pass it around, 97 bottles of beer on the wall.
...
No more bottles of beer on the wall, no more bottles of beer.
Go to the store and buy some more, 99 bottles of beer on the wall.

hello, world
90HQ+junk

```


