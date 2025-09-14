+++
title = "Subleq"
description = ""
date = 2019-10-22T03:19:58Z
aliases = []
[extra]
id = 19060
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "algol_w",
  "befunge",
  "c",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "forth",
  "fortran",
  "go",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "logo",
  "lua",
  "miniscript",
  "objeck",
  "oforth",
  "oorexx",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "python",
  "r",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "sidef",
  "sinclair_zx81_basic",
  "swift",
  "tcl",
  "ubasic_4th",
  "zkl",
  "zx_spectrum_basic",
]
+++

[[eso:Subleq|Subleq]] is an example of a [[wp:One_instruction_set_computer|One-Instruction Set Computer (OISC)]].

It is named after its only instruction, which is '''SU'''btract and '''B'''ranch if '''L'''ess than or '''EQ'''ual to zero.

## Task

Your task is to create an interpreter which emulates a SUBLEQ machine.

The machine's memory consists of an array of signed integers.   These integers may be interpreted in three ways:
::::*   simple numeric values
::::*   memory addresses
::::*   characters for input or output

Any reasonable word size that accommodates all three of the above uses is fine.

The program should load the initial contents of the emulated machine's memory, set the instruction pointer to the first address (which is defined to be address 0), and begin emulating the machine, which works as follows:
:#   Let '''A''' be the value in the memory location identified by the instruction pointer;   let '''B''' and '''C''' be the values stored in the next two consecutive addresses in memory.
:#   Advance the instruction pointer three words   (it will then point at the address ''after'' the address contained in '''C''').
:#   If '''A''' is   '''-1'''   (negative unity),   then a character is read from the machine's input and stored in the address given by '''B'''.   '''C''' is unused.
:#   If '''B''' is   '''-1'''   (negative unity),   then the number contained in the address given by '''A''' is interpreted as a character and written to the machine's output.   '''C''' is unused.
:#   Otherwise, both '''A''' and '''B''' are treated as addresses.   The number contained in address '''A''' is subtracted from the number in address '''B'''    (and the result stored back in address '''B''').   If the result is zero or negative, the number in '''C''' becomes the new instruction pointer.
:#   If the instruction pointer becomes negative, execution halts.

Your solution should accept as input a program to execute on the machine, separately from the input fed to the emulated machine once it is running.

This program should be in the form of raw subleq "machine code" - whitespace-separated decimal numbers, with no symbolic names or other assembly-level extensions, to be loaded into memory starting at address   '''0'''   (zero).

For purposes of this task, show the output of your solution when fed the below   "Hello, world!"   program.

As written, the example assumes ASCII or a superset of it, such as any of the Latin-N character sets or Unicode;   you may translate the numbers representing characters into another character set if your implementation runs in a non-ASCII-compatible environment.


```txt
15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0
```


The above "machine code" corresponds to something like this in a hypothetical assembler language:


```txt
start:
    zero, message, -1
    message, -1, -1
    neg1, start+1, -1
    neg1, start+3, -1
    zero, zero, start
zero: 0
neg1: -1
message: "Hello, world!\n\0"
```






## Ada


```Ada
with Ada.Text_IO;

procedure Subleq is

   Storage_Size: constant Positive := 2**8; -- increase or decrease memory
   Steps: Natural := 999; -- "emergency exit" to stop endless loops

   subtype Address is Integer range -1 .. (Storage_Size-1);
   subtype Memory_Location is Address range 0 .. Address'Last;

   type Storage is array(Memory_Location) of Integer;

   package TIO renames Ada.Text_IO;
   package IIO is new TIO.Integer_IO(Integer);

   procedure Read_Program(Mem: out Storage) is
      Idx: Memory_Location := 0;
   begin
      while not TIO.End_Of_Line loop
	 IIO.Get(Mem(Idx));
 	 Idx := Idx + 1;
      end loop;
   exception
      when others => TIO.Put_Line("Reading program: Something went wrong!");
   end Read_Program;

   procedure Execute_Program(Mem: in out Storage) is
      PC: Integer := 0; -- program counter
      function Source return Integer is (Mem(PC));
      function Dest return Integer is (Mem(PC+1));
      function Branch return Integer is (Mem(PC+2));
      function Next return Integer is (PC+3);
   begin
      while PC >= 0 and Steps >= 0 loop
	 Steps := Steps -1;
	 if Source = -1 then -- read input
            declare
               Char: Character;
            begin
               TIO.Get (Char);
               Mem(Dest) := Character'Pos (Char);
            end;
	    PC := Next;
	 elsif Dest = -1 then -- write output
	    TIO.Put(Character'Val(Mem(Source)));
	    PC := Next;
	 else -- subtract and branch if less or equal
	    Mem(Dest) := Mem(Dest) - Mem(Source);
	    if Mem(Dest) <= 0 then
	       PC := Branch;
	    else
	       PC := Next;
	    end if;
	 end if;
      end loop;
      TIO.Put_Line(if PC >= 0 then "Emergency exit: program stopped!" else "");
    exception
      when others => TIO.Put_Line("Failure when executing Program");
   end Execute_Program;

   Memory: Storage := (others => 0); -- no initial "junk" in memory!

begin

   Read_Program(Memory);
   Execute_Program(Memory);

end Subleq;
```



```txt
>./subleq
15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0
Hello, world!

```



## ALGOL 68


```algol68
# Subleq program interpreter                                                 #
# executes the program specified in code, stops when the instruction pointer #
# becomes negative                                                           #
PROC run subleq = ( []INT code )VOID:
     BEGIN
        INT   max memory = 3 * 1024;
        [ 0 : max memory - 1 ]INT memory;
        # load the program into memory                                       #
        # a slice yields a row with LWB 1...                                 #
        memory[ 0 : UPB code - LWB code ] := code[ AT 1 ];
        # start at instruction 0                                             #
        INT   ip := 0;
        # execute the instructions until ip is < 0                           #
        WHILE ip >= 0 DO
            # get three words at ip and advance ip past them                 #
            INT a := memory[ ip     ];
            INT b := memory[ ip + 1 ];
            INT c := memory[ ip + 2 ];
            ip +:= 3;
            # execute according to a, b and c                                #
            IF   a = -1 THEN
                # input a character to b                                     #
                CHAR input;
                get( stand in, ( input ) );
                memory[ b ] := ABS input
            ELIF b = -1 THEN
                # output character from a                                    #
                print( ( REPR memory[ a ] ) )
            ELSE
                # subtract and branch if le 0                                #
                memory[ b ] -:= memory[ a ];
                IF memory[ b ] <= 0 THEN
                    ip := c
                FI
            FI
        OD
     END # run subleq # ;

# test the interpreter with the hello-world program specified in the task    #
run subleq( (  15,  17,  -1,  17,  -1,  -1
            ,  16,   1,  -1,  16,   3,  -1
            ,  15,  15,   0,   0,  -1,  72
            , 101, 108, 108, 111,  44,  32
            , 119, 111, 114, 108, 100,  33
            ,  10,   0
            )
          )

```

```txt

Hello, world!

```



## ALGOL W

```algolw
% Subleq program interpreter                                                 %
begin

    % executes the program specified in scode, stops when the instruction    %
    % pointer becomes negative                                               %
    procedure runSubleq ( integer array scode( * )
                        ; integer value codeLength
                        ) ;
    begin
        integer maxMemory;
        maxMemory := 3 * 1024;
        begin
            integer array memory ( 0 :: maxMemory - 1 );
            integer       ip, a, b, c;
            for i := 0 until maxMemory - 1 do memory( i ) := 0;
            % load the program into memory                                   %
            for i := 0 until codeLength do memory( i ) := scode( i );

            % start at instruction 0                                         %
            ip := 0;
            % execute the instructions until ip is < 0                       %
            while ip >= 0 do begin
                % get three words at ip and advance ip past them             %
                a  := memory( ip     );
                b  := memory( ip + 1 );
                c  := memory( ip + 2 );
                ip := ip + 3;
                % execute according to a, b and c                            %
                if       a = -1 then begin
                    % input a character to b                                 %
                    string(1) input;
                    read( input );
                    memory( b ) := decode( input )
                    end
                else if b = -1 then begin
                    % output character from a                                %
                    writeon( code( memory( a ) ) )
                    end
                else begin
                    % subtract and branch if le 0                            %
                    memory( b ) := memory( b ) - memory( a );
                    if memory( b ) <= 0 then ip := c
                end
            end % while-do %
        end
    end % runSubleq % ;

    % test the interpreter with the hello-world program specified in the task %
    begin
        integer array code ( 0 :: 31 );
        integer       codePos;
        codePos := 0;
        for i :=  15,  17,  -1,  17,  -1,  -1
               ,  16,   1,  -1,  16,   3,  -1
               ,  15,  15,   0,   0,  -1,  72
               , 101, 108, 108, 111,  44,  32
               , 119, 111, 114, 108, 100,  33
               ,  10,   0
        do begin
            code( codePos ) := i;
            codePos := codePos + 1;
        end;
        runSubleq( code, 31 )
    end

end.
```

```txt

Hello, world!

```


=={{Header|BBC BASIC}}==
The BBC BASIC implementation reads the machine code program as a string from standard input and stores it in an array of signed 32-bit integers. The default size of the array is 256, but other values could easily be substituted. No attempt is made to handle errors arising from invalid Subleq programs.

```bbcbasic>REM
subleq
DIM memory%(255)
counter% = 0
INPUT "SUBLEQ> " program$
WHILE INSTR(program$, " ")
    memory%(counter%) = VAL(LEFT$(program$, INSTR(program$, " ") - 1))
    program$ = MID$(program$, INSTR(program$, " ") + 1)
    counter% += 1
ENDWHILE
memory%(counter%) = VAL(program$)
counter% = 0
REPEAT
    a% = memory%(counter%)
    b% = memory%(counter% + 1)
    c% = memory%(counter% + 2)
    counter% += 3
    IF a% = -1 THEN
        INPUT "SUBLEQ> " character$
        memory%(b%) = ASC(character$)
    ELSE
        IF b% = -1 THEN
            PRINT CHR$(memory%(a%));
        ELSE
            memory%(b%) = memory%(b%) - memory%(a%)
            IF memory%(b%) <= 0 THEN counter% = c%
        ENDIF
    ENDIF
UNTIL counter% < 0
```


Output:

```txt
SUBLEQ> 15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0
Hello, world!
```



## Befunge

The Subleq source is read from stdin, terminated by any control character - typically a carriage return or line feed, but a tab will also suffice. Thereafter any input read from stdin is considered input to the program itself.

The word size is limited to the cell size of the Befunge playfield, so it can be as low as 8 bits in many interpreters. The code automatically adjusts for unsigned implementations, though, so negative values will always be supported.

Also note that in some buggy interpreters you may need to pad the Befunge playfield with additional blank lines or spaces in order to initialise a writable memory area (without which the Subleq source may fail to load).


```befunge
01-00p00g:0`*2/00p010p0>$~>:4v4:-1g02p+5/"P"\%"P":p01+1:g01+g00*p02+1_v#!`"/":<
\0_v#-"-":\1_v#!`\*84:_^#- *8< >\#%"P"/#:5#<+g00g-\1+:"P"%\"P"v>5+#\*#<+"0"-~>^
<~0>#<$#-0#\<>$0>:3+\::"P"%\"P"/5+g00g-:1+#^_$:~>00gvv0gp03:+5/"P"\p02:%"P":< ^
>>>>>> , >>>>>> ^$p+5/"P"\%"P":-g00g+5/"P"\%"P":+1\+<>0g-\-:0v>5+g00g-:1+>>#^_$
        -:0\`#@_^<<<<<_1#`-#0:#p2#g5#08#3*#g*#0%#2\#+2#g5#08#<**/5+g00g
```


```txt
15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0
Hello, world!
```



## C

Takes the subleq instruction file as input, prints out usage on incorrect invocation.

```C

#include<stdlib.h>
#include<stdio.h>

void subleq(int* code){
	int ip = 0, a, b, c, nextIP,i;
	char ch;

	while(0<=ip){
		nextIP = ip + 3;
		a = code[ip];
		b = code[ip+1];
		c = code[ip+2];

		if(a==-1){
			scanf("%c",&ch);
			code[b] = (int)ch;
		}
		else if(b==-1){
			printf("%c",(char)code[a]);
		}
		else{
			code[b] -= code[a];
			if(code[b]<=0)
				nextIP = c;
		}
		ip = nextIP;
	}
}

void processFile(char* fileName){
	int *dataSet, i, num;

	FILE* fp = fopen(fileName,"r");

	fscanf(fp,"%d",&num);

	dataSet = (int*)malloc(num*sizeof(int));

	for(i=0;i<num;i++)
		fscanf(fp,"%d",&dataSet[i]);

	fclose(fp);

	subleq(dataSet);
}

int main(int argC,char* argV[])
{
	if(argC!=2)
		printf("Usage : %s <subleq code file>");
	else
		processFile(argV[1]);
	return 0;
}

```

Input file (subleqCode.txt), first row contains the number of code points ( integers in 2nd row):

```txt

32
15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0

```

Invocation and output:

```txt

C:\rosettaCode>subleq.exe subleqCode.txt
Hello, world!

```



## C++


```cpp

#include <fstream>
#include <iostream>
#include <iterator>
#include <vector>

class subleq {
public:
    void load_and_run( std::string file ) {
        std::ifstream f( file.c_str(), std::ios_base::in );
        std::istream_iterator<int> i_v, i_f( f );
        std::copy( i_f, i_v, std::back_inserter( memory ) );
        f.close();
        run();
    }

private:
    void run() {
        int pc = 0, next, a, b, c;
        char z;
        do {
            next = pc + 3;
            a = memory[pc]; b = memory[pc + 1]; c = memory[pc + 2];
            if( a == -1 ) {
                std::cin >> z; memory[b] = static_cast<int>( z );
            } else if( b == -1 ) {
                std::cout << static_cast<char>( memory[a] );
            } else {
                memory[b] -= memory[a];
                if( memory[b] <= 0 ) next = c;
            }
            pc = next;
        } while( pc >= 0 );
    }

    std::vector<int> memory;
};

int main( int argc, char* argv[] ) {
    subleq s;
    if( argc > 1 ) {
        s.load_and_run( argv[1] );
    } else {
        std::cout << "usage: subleq <filename>\n";
    }
    return 0;
}

```


```txt

subleq test.txt
Hello, world!

```


## C#
```c#
using System;

namespace Subleq {
    class Program {
        static void Main(string[] args) {
            int[] mem = {
                15, 17, -1, 17, -1, -1, 16, 1, -1, 16,
                3, -1, 15, 15, 0, 0, -1, 72, 101, 108,
                108, 111, 44, 32, 119, 111, 114, 108, 100, 33,
                10, 0,
            };

            int instructionPointer = 0;

            do {
                int a = mem[instructionPointer];
                int b = mem[instructionPointer + 1];

                if (a == -1) {
                    mem[b] = Console.Read();
                }
                else if (b == -1) {
                    Console.Write((char)mem[a]);
                }
                else {
                    mem[b] -= mem[a];
                    if (mem[b] < 1) {
                        instructionPointer = mem[instructionPointer + 2];
                        continue;
                    }
                }

                instructionPointer += 3;
            } while (instructionPointer >= 0);
        }
    }
}
```

```txt
Hello, world!
```



## COBOL

For compatibility with online COBOL compilers, where file IO is not supported, this implementation reads the Subleq program from the console. Note that COBOL tables (arrays) are indexed from 1 rather than 0, and so are character sets: in an ASCII environment 'A' is coded as 66 (the sixty-sixth character), not 65.

```cobol
identification division.
program-id. subleq-program.
data division.
working-storage section.
01  subleq-source-code.
    05 source-string                      pic x(2000).
01  subleq-virtual-machine.
    05 memory-table.
        10 memory                         pic s9999
            occurs 500 times.
    05 a                                  pic s9999.
    05 b                                  pic s9999.
    05 c                                  pic s9999.
    05 instruction-pointer                pic s9999.
    05 input-output-character             pic x.
01  working-variables.
    05 loop-counter                       pic 9999.
    05 instruction-counter                pic 9999.
    05 string-pointer                     pic 9999.
    05 adjusted-index-a                   pic 9999.
    05 adjusted-index-b                   pic 9999.
    05 output-character-code              pic 9999.
procedure division.
read-source-paragraph.
    accept source-string from console.
    display 'READING SUBLEQ PROGRAM... ' with no advancing.
    move 1 to string-pointer.
    move 0 to instruction-counter.
    perform split-source-paragraph varying loop-counter from 1 by 1
        until loop-counter is greater than 500
        or string-pointer is greater than 2000.
    display instruction-counter with no advancing.
    display ' WORDS READ.'.
execute-paragraph.
    move 1 to instruction-pointer.
    move 0 to instruction-counter.
    display 'BEGINNING RUN... '.
    display ''.
    perform execute-instruction-paragraph
        until instruction-pointer is negative.
    display ''.
    display 'HALTED AFTER ' instruction-counter ' INSTRUCTIONS.'.
    stop run.
execute-instruction-paragraph.
    add 1 to instruction-counter.
    move memory(instruction-pointer) to a.
    add 1 to instruction-pointer.
    move memory(instruction-pointer) to b.
    add 1 to instruction-pointer.
    move memory(instruction-pointer) to c.
    add 1 to instruction-pointer.
    if a is equal to -1 then perform input-paragraph.
    if b is equal to -1 then perform output-paragraph.
    if a is not equal to -1 and b is not equal to -1
        then perform subtraction-paragraph.
split-source-paragraph.
    unstring source-string delimited by all spaces
        into memory(loop-counter)
        with pointer string-pointer.
    add 1 to instruction-counter.
input-paragraph.
    display '> ' with no advancing.
    accept input-output-character from console.
    add 1 to b giving adjusted-index-b.
    move function ord(input-output-character)
        to memory(adjusted-index-b).
    subtract 1 from memory(adjusted-index-b).
output-paragraph.
    add 1 to a giving adjusted-index-a.
    add 1 to memory(adjusted-index-a) giving output-character-code.
    move function char(output-character-code)
        to input-output-character.
    display input-output-character with no advancing.
subtraction-paragraph.
    add 1 to c.
    add 1 to a giving adjusted-index-a.
    add 1 to b giving adjusted-index-b.
    subtract memory(adjusted-index-a) from memory(adjusted-index-b).
    if memory(adjusted-index-b) is equal to zero
        or memory(adjusted-index-b) is negative
        then move c to instruction-pointer.
```

```txt
READING SUBLEQ PROGRAM... 0032 WORDS READ.
BEGINNING RUN...

Hello, world!

HALTED AFTER 0073 INSTRUCTIONS.
```



## Common Lisp


```lisp
(defun run (memory)
  (loop for pc = 0 then next-pc
        until (minusp pc)
        for a = (aref memory pc)
        for b = (aref memory (+ pc 1))
        for c = (aref memory (+ pc 2))
        for next-pc = (cond ((minusp a)
                             (setf (aref memory b) (char-code (read-char)))
                             (+ pc 3))
                            ((minusp b)
                             (write-char (code-char (aref memory a)))
                             (+ pc 3))
                            ((plusp (setf (aref memory b)
                                          (- (aref memory b) (aref memory a))))
                             (+ pc 3))
                            (t c))))

(defun main ()
  (let ((memory (vector 15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72
                        101 108 108 111 44 32 119 111 114 108 100 33 10 0)))
    (run memory)))
```

```txt
Hello, world!
```



## D


```D
import std.stdio;

void main() {
    int[] mem = [
         15,  17,  -1,  17,  -1,  -1,  16,   1,
         -1,  16,   3,  -1,  15,  15,   0,   0,
         -1,  72, 101, 108, 108, 111,  44,  32,
        119, 111, 114, 108, 100,  33,  10,   0
    ];

    int instructionPointer = 0;

    do {
        int a = mem[instructionPointer];
        int b = mem[instructionPointer + 1];

        if (a == -1) {
            int input;
            readf!" %d"(input);
            mem[b] = input;
        } else if (b == -1) {
            write(cast(char) mem[a]);
        } else {
            mem[b] -= mem[a];
            if (mem[b] < 1) {
                instructionPointer = mem[instructionPointer + 2];
                continue;
            }
        }

        instructionPointer += 3;
    } while (instructionPointer >= 0);
}
```


```txt
Hello, world!
```



## Forth

Note that Forth is stack oriented. Hence, the code is toggled in in reverse.
<lang>create M 32 cells allot

: enter refill drop parse-word evaluate ; : M[] cells M + ;
: init M 32 cells bounds ?do i ! 1 cells +loop ;
: b-a+! dup dup cell+ @ M[] swap @ M[] @ negate over +! ;
: c b-a+! @ 1- 0< if 2 cells + @ else swap 3 + then nip ;
: b? dup cell+ @ 0< if @ M[] @ emit 3 + else c then ;
: a? dup @ 0< if cell+ @ M[] enter swap ! 3 + else b? then ;
: subleq cr 0 begin dup 1+ 0> while dup M[] a? repeat drop ;

0 10 33 100 108 114 111 119 32 44 111 108 108 101 72
-1 0 0 15 15 -1 3 16 -1 1 16 -1 -1 17 -1 17 15

init subleq
```

```txt
init subleq
Hello, world!
 ok
```



## Fortran

There is no protocol for getting the programme into the computer, as with a bootstrap sequence. Pre-emptively reading a sequence of numbers into a MEM array would do, and Fortran offers a free-format input option that would do it easily, except, there is no provision for knowing the number of values to read before they are read. A <code>READ (IN,*) MEM(1:N)</code> or similar would read input until values for all N elements had been found, reading additional records as required, and strike end-of-file if there were not enough supplied. One could then rewind the file and try again with a different value of N in a variant of a binary search, but this would be grotesque. This is why a common style is <code>READ(IN,*) N,A(1:N)</code> The alternative would be to read each record of the input file into a text variable, then scan the text and extract numbers as encountered until end-of-file or some suitable indication is reached. This is good, but, how long a record must the text variable allow for? More annoyance! A lot of infrastructure detracting from the prime task, so, a pre-emptive set of values for an array INITIAL, as per the example.

Fortran arrays start with element one. Other languages require a start of zero. Whichever is selected, some parts of a formula may naturally start with zero and others start with one and there is no escape. When translating formulae into furrytran, this can mean a change of interpretation of certain parts of the formulae, or, the introduction of an offset so that wherever a formula calls for A(i), you code A(i + 1) and so forth. It is also possible to play tricks via the likes of <code>EQUIVALENCE (A(1),A1(2))</code> where array A1 has elements one to a hundred, and so array A indexes these same elements as zero to ninety-nine. This of course will only work if array bound checking is not strict, which was usual because most early fortran compilers only provided bound checking as a special feature to be asked for politely. Another ploy would be to devise <code>FUNCTION A(I)</code> in place of an array A, and then one could employ whatever indexing one desired to read a value. Languages such as Pascal preclude this, because although A(i) is a function, an array must have A[i]. Alas, Fortran does not support palindromic function usage, (as with SUBSTR in pl/i) so although one can have <code>N = DAYNUM(Year,Month,Day)</code> the reverse function can't be coded as <code>DAYNUM(Year,Month,Day) = N</code>, a pity.

But Fortran 90 introduced the ability to specify the lower bounds of an array, so MEM(0:LOTS) is available without difficulty, and formulae may be translated with greater ease: handling offsets is a simple clerical task; computers excel at simple clerical tasks, so, let the computer do it. Otherwise, the following code would work with F77, except possibly for the odd usage of $ in a FORMAT statement so that each character of output is not on successive lines.


```Fortran

      PROGRAM SUBLEQ0	!Simulates a One-Instruction computer, with Subtract and Branch if <= 0.
      INTEGER LOTS,LOAD		!Document some bounds.
      PARAMETER (LOTS = 36, LOAD = 31)	!Sufficient for the example.
      INTEGER IAR, MEM(0:LOTS)		!The basic storage of a computer. IAR could be in memory too.
      INTEGER ABC(3),A,B,C		!A hardware register. Could use INTEGER*1 for everything...
      EQUIVALENCE (ABC(1),A),(ABC(2),B),(ABC(3),C)	!It has components.
      INTEGER INITIAL(0:LOAD)		!There is no sign of a bootstrap loader sequence!
      DATA INITIAL/15,17,-1,17,-1,-1,16,1,-1,16,3,-1,15,15,0,0,-1,	!These are operations, it so happens.
     1          72,101,108,108,111,44,32,119,111,114,108,100,33,10,0/	!And these happen to be ASCII character code numbers.
Core memory initialisation.
      MEM = -66			!Accessing uninitialised memory is improper. This might cause hiccoughs..
      MEM(0:LOAD) = INITIAL	!No bootstrap!
      IAR = 0			!The Instruction Address Register starts at the start.
Commence execution of the current instruction.
  100 ABC = MEM(IAR:IAR + 2)	!Load the three-word instruction.
      IAR = IAR + 3		!Advance IAR accordingly.
      IF (A .EQ. -1) THEN	!Decode the instruction as per the design.
        WRITE (6,102)			!Supply a prompt, otherwise, obscurity results.
  102   FORMAT (" A number:",$)		!But, that will make a mess of the layout.
        READ (5,*) MEM(B)		!The specified action is to read as a number.
      ELSE IF (B .EQ. -1) THEN	!This is for output.
        WRITE (6,103) CHAR(MEM(A))	!As specified, interpret a number as a character.
  103   FORMAT (A1,$)			!The $, obviously, states: do not end the line and start the next.
      ELSE			!And this is a two-part action.
        MEM(B) = MEM(B) - MEM(A)	!Perform arithmetic.
        IF (MEM(B).LE.0) IAR = C	!And based on the result, maybe a GO TO.
      END IF			!So much for decoding.
      IF (IAR.GE.0) GO TO 100	!Keep at it.
      END	!That was simple.

```

For simplicity there are no checks on memory bounds or endless looping, nor any trace output. The result is

```txt

Hello, world!

```

And the linefeed (character(10)) had been sent forth, but is not apparent because it just ended the line.


## Go


```go
package main

import (
	"io"
	"log"
	"os"
)

func main() {
	var mem = []int{
		15, 17, -1, 17, -1, -1, 16, 1, -1, 16, 3, -1, 15, 15, 0, 0, -1,
		//'H', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '!', '\n',
		72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33, 10,
		0,
	}
	for ip := 0; ip >= 0; {
		switch {
		case mem[ip] == -1:
			mem[mem[ip+1]] = readbyte()
		case mem[ip+1] == -1:
			writebyte(mem[mem[ip]])
		default:
			b := mem[ip+1]
			v := mem[b] - mem[mem[ip]]
			mem[b] = v
			if v <= 0 {
				ip = mem[ip+2]
				continue
			}
		}
		ip += 3
	}
}

func readbyte() int {
	var b [1]byte
	if _, err := io.ReadFull(os.Stdin, b[:]); err != nil {
		log.Fatalln("read:", err)
	}
	return int(b[0])
}

func writebyte(b int) {
	if _, err := os.Stdout.Write([]byte{byte(b)}); err != nil {
		log.Fatalln("write:", err)
	}
}
```

A much longer version using types, methods, etc
and that supports supplying a program via a file or the command line,
and provides better handling of index out of range errors
is [[/go|also available]].


## Haskell

Inspired by the Racket solution.

```Haskell
{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.State
import Data.Char (chr, ord)
import Data.IntMap

subleq = loop 0
    where
      loop ip =
          when (ip >= 0) $
          do m0 <- gets (! ip)
             m1 <- gets (! (ip + 1))
             if m0 < 0
                then do modify . insert m1 ch . ord =<< liftIO getChar
                        loop (ip + 3)
                else if m1 < 0
                        then do liftIO . putChar . chr =<< gets (! m0)
                                loop (ip + 3)
                        else do v <- (-) <$> gets (! m1) <*> gets (! m0)
                                modify $ insert m1 v
                                if v <= 0
                                   then loop =<< gets (! (ip + 2))
                                   else loop (ip + 3)

main = evalStateT subleq helloWorld
    where
      helloWorld =
          fromList $
          zip [0..]
              [15, 17, -1, 17, -1, -1, 16, 1, -1, 16, 3, -1, 15, 15, 0, 0, -1, 72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 33, 10, 0]

```



## J



```J
readchar=:3 :0
  if.0=#INBUF do. INBUF=:LF,~1!:1]1 end.
  r=.3 u:{.INBUF
  INBUF=:}.INBUF
  r
)

writechar=:3 :0
  OUTBUF=:OUTBUF,u:y
)

subleq=:3 :0
  INBUF=:OUTBUF=:''
  p=.0
  whilst.0<:p do.
    'A B C'=. (p+0 1 2){y
    p=.p+3
    if._1=A do. y=. (readchar'') B} y
    elseif._1=B do. writechar A{y
    elseif. 1   do.
      t=. (B{y)-A{y
      y=. t B}y
      if. 0>:t do.p=.C end.
    end.
  end.
  OUTBUF
)
```


Example:


```J
   subleq 15 17 _1 17 _1 _1 16 1 _1 16 3 _1 15 15 0 0 _1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0
Hello, world!
```



## Java


```java
import java.util.Scanner;

public class Subleq {

    public static void main(String[] args) {
        int[] mem = {15, 17, -1, 17, -1, -1, 16, 1, -1, 16, 3, -1, 15, 15, 0, 0,
            -1, 72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33, 10, 0};

        Scanner input = new Scanner(System.in);
        int instructionPointer = 0;

        do {
            int a = mem[instructionPointer];
            int b = mem[instructionPointer + 1];

            if (a == -1) {
                mem[b] = input.nextInt();

            } else if (b == -1) {
                System.out.printf("%c", (char) mem[a]);

            } else {

                mem[b] -= mem[a];
                if (mem[b] < 1) {
                    instructionPointer = mem[instructionPointer + 2];
                    continue;
                }
            }

            instructionPointer += 3;

        } while (instructionPointer >= 0);
    }
}
```



```txt
Hello, world!
```



## jq

The subleq function defined here emulates the subleq OSIC; it
produces a stream of characters.

The program as presented here can
be used with jq 1.4, but to see the stream of characters it produces
as a stream of strings requires either a more recent version of jq
or some post-processing.  The output shown below assumes the -j
(--join-output) command-line option is available.

```jq
# If your jq has while/2 then the following definition can be omitted:
def while(cond; update):
  def _while: if cond then ., (update | _while) else empty end;
  _while;

# subleq(a) runs the program, a, an array of integers.
# Input: the data
# When the subleq OSIC is about to emit a NUL character, it stops instead.
def subleq(a):
  . as $input
  # state: [i, indexIntoInput, a, output]
  | [0, 0, a]
  | while( .[0] >= 0 and .[3] != 0 ;
           .[0] as $i
           | .[1] as $ix
           | .[2] as $a
           | if $a[$i] == -1 then
                if $input and $ix < ($input|length)
                then [$i+3, $ix + 1, ($a[$a[$i + 1]] = $input[$ix]), null]
                else [-1]
                end
              elif $a[$i + 1] == -1 then [$i+3, $ix, $a, $a[$a[$i]]]
              else
                [$i, $ix, ($a | .[.[$i + 1]] -= .[.[$i]]), null]
                | .[2] as $a
                | if $a[$a[$i+1]] <= 0 then .[0] = $a[$i + 2] else . end
                | .[0] += 3
              end )
  | .[3] | select(.) | [.] | implode;

subleq([15, 17, -1, 17, -1, -1, 16, 1, -1, 16, 3, -1, 15, 15,  0, 0, -1,
        72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33, 10, 0])
```

```sh
$ jq -r -j -n -f subleq.jq
Hello, world!
```



## Julia

'''Module''':

```julia
module Subleq

using Compat

# Helper function because julia has 1-indexed arrays
using MacroTools
macro shiftgetindex(shift, blk)
    return esc(MacroTools.postwalk(blk) do x
        if isa(x, Expr)
            if x.head == :ref
                x.args[2] = :($(x.args[2]) + $shift)
            elseif x.head == :call && x.args[1] == :getindex
                x.args[3] = :($(x.args[3]) + $shift)
            end
        end
        return x
    end)
end

function interpret(words::AbstractVector{Int})
    words = copy(words)
    buf = IOBuffer()
    ip = 0
    @shiftgetindex 1 while true
        a, b, c = words[ip:ip+2]
        ip += 3
        if a < 0
            print("Enter a character: ")
            words[b] = parse(Int, readline(stdin))
        elseif b < 0
            print(buf, Char(words[a]))
        else
            words[b] -= words[a]
            if words[b] ≤ 0
                ip = c
            end
            ip < 0 && break
        end
    end
    return String(take!(buf))
end

interpret(src::AbstractString) = interpret(parse.(Int, split(src)))

end  # module Subleq
```


'''Main''':

```julia
print(Subleq.interpret("15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101
    108 108 111 44 32 119 111 114 108 100 33 10 0"))
```


```txt
Hello, world!
```



## Kotlin


```scala
// version 1.1.2

fun subleq(program: String) {
    val words = program.split(' ').map { it.toInt() }.toTypedArray()
    val sb = StringBuilder()
    var ip = 0
    while (true) {
        val a = words[ip]
        val b = words[ip + 1]
        var c = words[ip + 2]
        ip += 3
        if (a < 0) {
            print("Enter a character : ")
            words[b] = readLine()!![0].toInt()
        }
        else if (b < 0) {
            sb.append(words[a].toChar())
        }
        else {
            words[b] -= words[a]
            if (words[b] <= 0) ip = c
            if (ip < 0) break
        }
    }
    print(sb)
}

fun main(args: Array<String>) {
    val program = "15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0"
    subleq(program)
}
```


```txt

Hello, world!

```



## Logo


```logo
make "memory (array 32 0)

to load_subleq
  local "i make "i 0
  local "line
  make "line readlist
  while [or (not empty? :line) (not list? :line)] [
    foreach :line [
        setitem :i :memory ?
        make "i sum :i 1
    ]
    make "line readlist
  ]
end

to run_subleq
  make "ip 0
  while [greaterequal? :ip 0] [
    local "a make "a item :ip :memory
    make "ip sum :ip 1
    local "b make "b item :ip :memory
    make "ip sum :ip 1
    local "c make "c item :ip :memory
    make "ip sum :ip 1
    cond [
     [[less? :a 0]  setitem :b :memory ascii readchar ]
     [[less? :b 0]  type char item :a :memory ]
     [else
        local "av make "av item :a :memory
        local "bv make "bv item :b :memory
        local "diff make "diff difference :bv :av
        setitem :b :memory :diff
        if [lessequal? :diff 0] [make "ip :c]]]
    ]
end

load_subleq
run_subleq
bye
```


```txt
logo subleq.lg
15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0
^D
Hello, world!
```



## Lua


```Lua
function subleq (prog)
    local mem, p, A, B, C = {}, 0
    for word in prog:gmatch("%S+") do
        mem[p] = tonumber(word)
        p = p + 1
    end
    p = 0
    repeat
        A, B, C = mem[p], mem[p + 1], mem[p + 2]
        if A == -1 then
            mem[B] = io.read()
        elseif B == -1 then
            io.write(string.char(mem[A]))
        else
            mem[B] = mem[B] - mem[A]
            if mem[B] <= 0 then p = C end
        end
        p = p + 3
    until not mem[mem[p]]
end

subleq("15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0")
```



## MiniScript


```MiniScript
memory = []
step = 3
currentAddress = 0
out = ""

process = function(address)
    A = memory[address].val
    B = memory[address + 1].val
    C = memory[address + 2].val
    nextAddress = address + step

    if A == -1 then
        memory[B] = input
    else if B == -1 then
        globals.out = globals.out + char(memory[A].val)
    else
        memory[B] = str(memory[B].val - memory[A].val)
        if memory[B] < 1 then nextAddress = C
    end if
    return nextAddress
end function

print
memory = input("Enter SUBLEQ program").split

print
print "Running Program"
print "-------------------"
processing = currentAddress < memory.len
while processing
    currentAddress = process(currentAddress)
    if currentAddress >= memory.len or currentAddress == -1 then
        processing = false
    end if
end while

print out
print "-------------------"
print "Execution Complete"
```

```txt

Enter SUBLEQ program
15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0

Running Program
-------------------
Hello, world!

-------------------
Execution Complete

```


=={{header|Modula-2}}==

```modula2
MODULE Subleq;
FROM Terminal IMPORT Write,WriteString,WriteLn,ReadChar;

TYPE MEMORY = ARRAY[0..31] OF INTEGER;
VAR
    mem : MEMORY;
    ip,a,b : INTEGER;
    ch : CHAR;
BEGIN
    mem := MEMORY{
         15,  17,  -1,  17,  -1,  -1,  16,   1,
         -1,  16,   3,  -1,  15,  15,   0,   0,
         -1,  72, 101, 108, 108, 111,  44,  32,
        119, 111, 114, 108, 100,  33,  10,   0
    };

    ip := 0;
    REPEAT
        a := mem[ip];
        b := mem[ip+1];

        IF a = -1 THEN
            ch := ReadChar();
            mem[b] := ORD(ch);
        ELSIF b = -1 THEN
            Write(CHR(mem[a]));
        ELSE
            DEC(mem[b],mem[a]);
            IF mem[b] < 1 THEN
                ip := mem[ip+2];
                CONTINUE
            END
        END;

        INC(ip,3)
    UNTIL ip < 0;
    WriteLn;

    ReadChar
END Subleq.
```



## Objeck

```objeck
use System.IO;

class Sublet {
  function : Main(args : String[]) ~ Nil {
    mem := [
      15, 17, -1, 17, -1, -1, 16, 1, -1, 16,
      3, -1, 15, 15, 0, 0, -1, 72, 101, 108,
      108, 111, 44, 32, 119, 111, 114, 108, 100, 33,
      10, 0];

    instructionPointer := 0;

    do {
      a := mem[instructionPointer];
      b := mem[instructionPointer + 1];

      if (a = -1) {
        mem[b] := Console->ReadString()->Get(0);
        instructionPointer += 3;
      }
      else if (b = -1) {
        value := mem[a]->As(Char);
        value->Print();
        instructionPointer += 3;
      }
      else {
        mem[b] -= mem[a];
        if (mem[b] < 1) {
          instructionPointer := mem[instructionPointer + 2];
        }
        else {
          instructionPointer += 3;
        };
      };
    }
    while (instructionPointer >= 0);
  }
}
```



```txt

Hello, world!

```




## Oforth



```oforth
: subleq(program)
| ip a b c newb |
   program asListBuffer ->program
   0 ->ip
   while( ip 0 >= ) [
      ip 1+ dup program at ->a 1+ dup program at ->b 1+ program at ->c
      ip 3 + ->ip
      a -1 = ifTrue: [ b System.In >> nip program put continue ]
      b -1 = ifTrue: [ System.Out a 1+ program at <<c drop continue ]
      b 1+ program at a 1+ program at - ->newb
      program put(b 1+, newb)
      newb 0 <= ifTrue: [ c ->ip ]
      ] ;

[15, 17, -1, 17, -1, -1, 16, 1, -1, 16, 3, -1, 15, 15, 0, 0, -1, 72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108, 100, 33, 10, 0 ]
subleq
```



## ooRexx


### version 1

reformatted and long variable names that suit all Rexxes.

```oorexx
/*REXX program simulates execution of a  One-Instruction Set Computer (OISC). */
Signal on Halt                         /*enable user to halt the simulation.  */
cell.=0                                /*zero-out all of real memory locations*/
ip=0                                   /*initialize ip  (instruction pointer).*/
Parse Arg memory                       /*get optional low memory vals from CL.*/
memory=space(memory)                   /*elide superfluous blanks from string.*/

If memory==''  Then Do
  memory='15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1' /* common start     */
  If 3=='f3'x  Then                    /* EBCDIC                              */
    memory=memory '200 133 147 147 150 107 64 166 150 153 147 132  90  21 0'
  else /* ASCII      H   e   l   l   o   , bla  w   o   r   l   d   ! l/f */
    memory=memory ' 72 101 108 108 111  44 32 119 111 114 108 100  33  10 0'
  End

Do i=0 For words(memory)               /* copy memory to cells                */
  cell.i=word(memory,i+1)
  End

Do Until ip<0                          /* [?]  neg addresses are treated as -1*/
  a=cell(ip)
  b=cell(ip+1)
  c=cell(ip+2)                         /*get values for  A,  B,  and  C.      */
  ip=ip+3                              /*advance the ip (instruction pointer).*/
  Select                               /*choose an instruction state.         */
    When a<0 Then cell.b=charin()            /* read a character from term.   */
    When b<0 Then call charout ,d2c(cell.a)  /* write "    "      to    "     */
    Otherwise Do
      cell.b=cell.b-cell.a             /* put difference ---? loc  B.         */
      If cell.b<=0  Then ip=c          /* if ¬positive, set ip to  C.         */
      End
    End
  End
Exit
cell: Parse arg _
      Return cell._                    /*return the contents of "memory" loc _*/
halt: Say 'REXX program halted by user.'
      Exit 1
```

```txt
Hello, world!
```



### version 2

Using an array object instead of a stem for cells.

Array indexes must be positive!

```oorexx
/*REXX program simulates execution of a  One-Instruction Set Computer (OISC). */
Signal on Halt                         /*enable user to halt the simulation.  */
cell=.array~new                        /*zero-out all of real memory locations*/
ip=0                                   /*initialize ip  (instruction pointer).*/
Parse Arg memory                       /*get optional low memory vals from CL.*/
memory=space(memory)                   /*elide superfluous blanks from string.*/

if memory==''  then Do
  memory='15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1' /* common start     */
  If 3=="f3"x  then                    /* EBCDIC                              */
    memory=memory '200 133 147 147 150 107 64 166 150 153 147 132  90  21 0'
  else /* ASCII      H   e   l   l   o   , bla  w   o   r   l   d   ! l/f */
    memory=memory ' 72 101 108 108 111  44 32 119 111 114 108 100  33  10 0'
  End

Do i=1 To words(memory)               /* copy memory to cells                */
  cell[i]=word(memory,i)
  End

Do Until ip<0                          /* [?]  neg addresses are treated as -1*/
  a=cell[ip+1]
  b=cell[ip+2]
  c=cell[ip+3]                         /*get values for  A,  B,  and  C.      */
  ip=ip+3                              /*advance the ip (instruction pointer).*/
  Select                               /*choose an instruction state.         */
    When a<0   then cell[b+1]=charin()           /* read a character from term*/
    When b<0   then call charout ,d2c(cell[a+1]) /* write "    "      to    " */
    Otherwise Do
      cell[b+1]-=cell[a+1]             /* put difference ---? loc  B[         */
      If cell[b+1]<=0  Then ip=c       /* if ¬positive, set ip to  C[         */
      End
    End
  End
Exit
halt: Say 'REXX program halted by user.';
      Exit 1
```



## Pascal

```pascal
PROGRAM OISC;

CONST
	MAXADDRESS = 1255;

TYPE
	MEMORY = PACKED ARRAY [0 .. MAXADDRESS] OF INTEGER;

VAR
	MEM : MEMORY;
	FILENAME : STRING;

PROCEDURE LOADTEXT (FILENAME : STRING; VAR MEM : MEMORY);
	VAR
		NUMBERS : TEXT;
		ADDRESS : INTEGER;
	BEGIN
		ASSIGN (NUMBERS, FILENAME);
		ADDRESS := 0;
		RESET (NUMBERS);
		WHILE (ADDRESS <= MAXADDRESS) AND NOT EOF (NUMBERS) DO BEGIN
			READ (NUMBERS, MEM [ADDRESS]);
			ADDRESS := ADDRESS + 1
		END;
		CLOSE (NUMBERS);
		FOR ADDRESS := ADDRESS TO MAXADDRESS DO
			MEM [ADDRESS] := 0
	END;

PROCEDURE SUBLEQ (VAR MEM : MEMORY);
	VAR
		ADDRESS, A, B, C : INTEGER;
		IO : CHAR;
	BEGIN
		ADDRESS := 0;
		WHILE ADDRESS >= 0 DO BEGIN
			A := MEM [ADDRESS];
			B := MEM [ADDRESS + 1];
			C := MEM [ADDRESS + 2];
			ADDRESS := ADDRESS + 3;
			IF A = -1 THEN BEGIN
				READ (IO);
				MEM [B] := ORD (IO)
			END
			ELSE IF B = -1 THEN BEGIN
				IO := CHR (MEM [A]);
				WRITE (IO)
			END
			ELSE BEGIN
				MEM [B] := MEM [B] - MEM [A];
				IF MEM [B] <= 0 THEN ADDRESS := C
			END
		END
	END;

BEGIN
	WRITE ('Filename>');
	READLN (FILENAME);
	LOADTEXT (FILENAME, MEM);
	SUBLEQ (MEM);
END.
```


hello-world.txt

```txt
15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0
```


```txt
Filename>hello-world.txt
Hello, world!
```



## Perl


```perl
#!/usr/bin/env perl
use strict;
use warnings;
my $file = shift;
my @memory = ();
open (my $fh, $file);
while (<$fh>) {
  chomp;
  push @memory, split;
}
close($fh);
my $ip = 0;
while ($ip >= 0 && $ip < @memory) {
  my ($a, $b, $c) = @memory[$ip,$ip+1,$ip+2];
 $ip += 3;
 if ($a < 0) {
    $memory[$b] = ord(getc);
 } elsif ($b < 0) {
    print chr($memory[$a]);
 } else {
    if (($memory[$b] -= $memory[$a]) <= 0) {
     $ip = $c;
   }
 }
}
```

```txt
Hello, world!
```



## Perl 6

```perl6
my @hello-world = <15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0>;

my @memory = @hello-world;
my $ip = 0;
while $ip >= 0 && $ip < @memory {
   my ($a, $b, $c) = @memory[$ip, $ip+1, $ip+2];
   $ip += 3;
   if $a < 0 {
       @memory[$b] = getc.ord;
   } elsif $b < 0 {
       print @memory[$a].chr;
   } else {
       if (@memory[$b] -= @memory[$a]) <= 0 {
           $ip = $c;
       }
   }
}
```


```txt
Hello, world!
```



## Phix


```Phix
procedure subleq(sequence code)
    integer ip := 0
    while ip>=0 do
        integer {a,b,c} = code[ip+1..ip+3]
        ip += 3
        if a=-1 then
            code[b+1] = getc(0)
        elsif b=-1 then
            puts(1,code[a+1])
        else
            code[b+1] -= code[a+1]
            if code[b+1]<=0 then
                ip := c
            end if
        end if
    end while
end procedure

subleq({15, 17,  -1,  17,  -1,  -1, 16,  1,  -1,  16,   3,  -1,
        15, 15,   0,   0,  -1,  72, 101, 108, 108, 111, 44, 32,
        119, 111, 114, 108, 100, 33, 10, 0})
```

```txt

Hello, world!

```



## PicoLisp


```PicoLisp
(de mem (N)
   (nth
      (quote
         15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1
         72 101 108 108 111 44 32 119 111 114 108 100 33 10 0 )
      (inc N) ) )

(for (IP (mem 0)  IP)
   (let (A (pop 'IP)  B (pop 'IP)  C (pop 'IP))
      (cond
         ((lt0 A) (set (mem B) (char)))
         ((lt0 B) (prin (char (car (mem A)))))
         ((le0 (dec (mem B) (car (mem A))))
            (setq IP (mem C)) ) ) ) )
```

Output:

```txt
Hello, world!
```



## PowerShell

```PowerShell

function Invoke-Subleq ([int[]]$Program)
{
    [int]$ip, [string]$output = $null

    try
    {
        while ($ip -ge 0)
        {
            if ($Program[$ip] -eq -1)
            {
                $Program[$Program[$ip + 1]] = [int](Read-Host -Prompt SUBLEQ)[0]
            }
            elseif ($Program[$ip + 1] -eq -1)
            {
                $output += "$([char]$Program[$Program[$ip]])"
            }
            else
            {
                $Program[$Program[$ip + 1]] -= $Program[$Program[$ip]]

                if ($Program[$Program[$ip + 1]] -le 0)
                {
                    $ip = $Program[$ip + 2]
                    continue
                }
            }

            $ip += 3
        }

        return $output
    }
    catch [IndexOutOfRangeException],[Exception]
    {
        Write-Host "$($Error[0].Exception.Message)" -ForegroundColor Red
    }
}

```


```PowerShell

Invoke-Subleq -Program 15,17,-1,17,-1,-1,16,1,-1,16,3,-1,15,15,0,0,-1,72,101,108,108,111,44,32,119,111,114,108,100,33,10,0

```

```txt

Hello, world!

```



## Python



```python
import sys

def subleq(a):
    i = 0
    try:
        while i >= 0:
            if a[i] == -1:
                a[a[i + 1]] = ord(sys.stdin.read(1))
            elif a[i + 1] == -1:
                print(chr(a[a[i]]), end="")
            else:
                a[a[i + 1]] -= a[a[i]]
                if a[a[i + 1]] <= 0:
                    i = a[i + 2]
                    continue
            i += 3
    except (ValueError, IndexError, KeyboardInterrupt):
        print("abort")
        print(a)

subleq([15, 17, -1, 17, -1, -1, 16, 1, -1, 16, 3, -1, 15, 15,
        0, 0, -1, 72, 101, 108, 108, 111, 44, 32, 119, 111,
        114, 108, 100, 33, 10, 0])
```



## R



```rsplus

mem <- c(15, 17, -1, 17, -1, -1, 16, 1,
         -1, 16, 3, -1, 15, 15, 0, 0,
         -1, 72, 101, 108, 108, 111, 44,
         32, 119, 111, 114, 108, 100,
         33, 10, 0)

getFromMemory <- function(addr) { mem[[addr + 1]] } # because first element in mem is mem[[1]]
setMemory <- function(addr, value) { mem[[addr + 1]] <<- value }
subMemory <- function(x, y) { setMemory(x, getFromMemory(x) - getFromMemory(y)) }

instructionPointer <- 0
while (instructionPointer >= 0) {
  a <- getFromMemory(instructionPointer)
  b <- getFromMemory(instructionPointer + 1)
  c <- getFromMemory(instructionPointer + 2)
  if (b == -1) {
    cat(rawToChar(as.raw(getFromMemory(a))))
  } else {
    subMemory(b, a)
    if (getFromMemory(b) < 1) {
      instructionPointer <- getFromMemory(instructionPointer + 2)
      next
    }
  }
  instructionPointer <- instructionPointer + 3
}

```

```txt
Hello, world!
```



## Racket

{{trans|Go}} The negative addresses are treated as -1.

```Racket
#lang racket

(define (subleq v)
  (define (mem n)
    (vector-ref v n))
  (define (mem-set! n x)
    (vector-set! v n x))
  (let loop ([ip 0])
    (when (>= ip 0)
      (define m0 (mem ip))
      (define m1 (mem (add1 ip)))
      (cond
        [(< m0 0) (mem-set! m1 (read-byte))
                  (loop (+ ip 3))]
        [(< m1 0) (write-byte (mem m0))
                  (loop (+ ip 3))]
        [else (define v (- (mem m1) (mem m0)))
              (mem-set! m1 v)
              (if (<= v 0)
                 (loop (mem (+ ip 2)))
                 (loop (+ ip 3)))]))))

(define Hello (vector 15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1
                    ; H    e    l    l    o    ,  <sp> w    o    r    l    d    !   \n
                      72   101  108  108  111  44  32  119  111  114  108  100  33  10
                      0))

(subleq Hello)
```

```txt
Hello, world!
```



## REXX

The REXX version supports   '''ASCII'''   and   '''EBCDIC'''   integer (glyphs)   for the message text.

The REXX language has no concept of a   ''word'',   but for storing numbers, the default is nine decimal digits.

```rexx
/*REXX program  simulates  the  execution  of a   One─Instruction Set Computer  (OISC). */
signal on halt                                   /*enable user to  halt  the simulation.*/
parse arg $                                      /*get optional low memory vals from CL.*/
$$= '15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1'  /*common stuff for EBCDIC & ASCII.*/
    /*EBCDIC "then" choice [↓]       H   e   l   l   o  , BLANK w   o   r   l   d  !  LF*/
if $='' then if 6=="f6"x  then $=$$ 200 133 147 147 150 107 64 166 150 153 147 132 90 21 0
                          else $=$$  72 101 108 108 111  44 32 119 111 114 108 100 33 10 0
                        /* [↑]  ASCII   (the "else" choice).                Line Feed≡LF*/
@.= 0                                            /*zero all memory & instruction pointer*/
         do j=0  for words($);  @.j=word($,j+1)  /*assign memory.  OISC is zero─indexed.*/
         end   /*j*/                             /*obtain A, B, C memory values──►────┐ */
    do #=0  by 3 until #<0;     a= @(#-3);    b= @(#-2);     c= @(#-1)   /* ◄─────────┘ */
        select                                   /*choose an instruction state.         */
        when a<0  then @.b= charin()             /*  read a character from the terminal.*/
        when b<0  then call charout , d2c(@.a)   /* write "     "      to   "     "     */
        otherwise      @.b= @.b - @.a            /*put difference  ────►  location  B.  */
                    if @.b<=0  then #= c         /*Not positive?   Then set  #  to  C.  */
        end   /*select*/                         /* [↑]  choose one of two states.      */
    end       /*#*/                              /*leave the DO loop if  #  is negative.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
@:     parse arg @z;    return @.@z              /*return  a  memory location (cell @Z).*/
halt:  say 'The One─Instruction Set Computer simulation pgm was halted by user.';   exit 1
```

```txt

Hello, world!

```



## Ruby


```Ruby
class Computer
  def initialize program
    @memory = program.map{|instruction| instruction.to_i}
    @instruction_pointer = 0
  end

  def step
    return nil if @instruction_pointer < 0

    a, b, c = @memory[@instruction_pointer .. @instruction_pointer + 2]
    @instruction_pointer += 3

    if a == -1
      b = readchar
    elsif b == -1
      writechar @memory[a]
    else
      difference = @memory[b] - @memory[a]
      @memory[b] = difference
      @instruction_pointer = c if difference <= 0
    end

    @instruction_pointer
  end

  def run
    current_pointer = @instruction_pointer
    current_pointer = step while current_pointer >= 0
  end

  private

  def readchar
    gets[0].ord
  end

  def writechar code_point
    print code_point.chr
  end
end

subleq = Computer.new ARGV

subleq.run
```

'''Sample usage:'''

```txt

>ruby subleq.rb 15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0
Hello, world!

```



## Scala

===Imperative, Javaish, destructible opcodes read===

```Scala
import java.util.Scanner

object Subleq extends App {
  val mem = Array(15, 17, -1, 17, -1, -1, 16, 1, -1, 16, 3, -1, 15, 15, 0, 0, -1,
    'H', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd', '!', 10, 0)
  val input = new Scanner(System.in)
  var instructionPointer = 0

  do {
    val (a, b) = (mem(instructionPointer), mem(instructionPointer + 1))
    if (a == -1) mem(b) = input.nextInt
    else if (b == -1) print(f"${mem(a)}%c")
    else {
      mem(b) -= mem(a)
      if (mem(b) < 1) instructionPointer = mem(instructionPointer + 2) - 3
    }
    instructionPointer += 3
  } while (instructionPointer >= 0)
}
```

{{Out}}See it running in your browser by [https://scastie.scala-lang.org/f4MszRqZR5qtxI6YwarJhw Scastie (JVM)].

## Sidef

```ruby
var memory = ARGV.map{.to_i};
var ip = 0;

while (ip.ge(0) && ip.lt(memory.len)) {
    var (a, b, c) = memory[ip, ip+1, ip+2];
    ip += 3;
    if (a < 0) {
        memory[b] = STDIN.getc.ord;
    }
    elsif (b < 0) {
        print memory[a].chr;
    }
    elsif ((memory[b] -= memory[a]) <= 0) {
        ip = c
    }
}
```


```txt
$ sidef subleq.sf 15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0
Hello, world!

```



## Sinclair ZX81 BASIC

The ZX81's character set does not include lower-case letters or the ! character. It also happens to use 0 as the code for a blank, making zero-terminated strings awkward; this program gets around the difficulty by the stupid trick of always storing <math>c</math>+1 instead of <math>c</math> where <math>c</math> is a printable character code.

Requires at least 2k of RAM.

```basic
 10 DIM M(32)
 20 INPUT P$
 30 LET W=1
 40 LET C=1
 50 IF C<LEN P$ THEN GOTO 80
 60 LET M(W)=VAL P$
 70 GOTO 150
 80 IF P$(C)=" " THEN GOTO 110
 90 LET C=C+1
100 GOTO 50
110 LET M(W)=VAL P$( TO C-1)
120 LET P$=P$(C+1 TO )
130 LET W=W+1
140 GOTO 40
150 LET P=0
160 LET A=M(P+1)
170 LET B=M(P+2)
180 LET C=M(P+3)
190 LET P=P+3
200 IF A=-1 THEN GOTO 260
210 IF B=-1 THEN GOTO 290
220 LET M(B+1)=M(B+1)-M(A+1)
230 IF M(B+1)<=0 THEN LET P=C
240 IF P<0 THEN STOP
250 GOTO 160
260 INPUT C$
270 LET M(B+1)=1+CODE C$
280 GOTO 160
290 IF M(A+1)<>118 THEN GOTO 320
300 PRINT
310 GOTO 160
320 PRINT CHR$ (M(A+1)-1);
330 GOTO 160
```

```txt
15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 46 43 50 50 53 27 1 61 53 56 50 42 28 118 0
```

```txt
HELLO, WORLD.
```



## Swift


```swift
func subleq(_ inst: inout [Int]) {
  var i = 0

  while i >= 0 {
    if inst[i] == -1 {
      inst[inst[i + 1]] = Int(readLine(strippingNewline: true)!.unicodeScalars.first!.value)
    } else if inst[i + 1] == -1 {
      print(String(UnicodeScalar(inst[inst[i]])!), terminator: "")
    } else {
      inst[inst[i + 1]] -= inst[inst[i]]

      if inst[inst[i + 1]] <= 0 {
        i = inst[i + 2]
        continue
      }
    }

    i += 3
  }
}

var prog = [
  15, 17, -1, 17, -1, -1, 16, 1, -1, 16, 3, -1, 15, 15,
  0, 0, -1, 72, 101, 108, 108, 111, 44, 32, 119, 111,
  114, 108, 100, 33, 10, 0
]

subleq(&prog)

```


```txt
Hello, world!
```



## Tcl



```Tcl

namespace import ::tcl::mathop::-

proc subleq {pgm} {
    set ip 0
    while {$ip >= 0} {
        lassign [lrange $pgm $ip $ip+2] a b c
        incr ip 3
        if {$a == -1} {
            scan [read stdin 1] %C char
            lset pgm $b $char
        } elseif {$b == -1} {
            set char [format %c [lindex $pgm $a]]
            puts -nonewline $char
        } else {
            lset pgm $b [set res [- [lindex $pgm $b] [lindex $pgm $a]]]
            if {$res <= 0} {
                set ip $c
            }
        }
    }
}

fconfigure stdout -buffering none
subleq {15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0}

```


```txt
Hello, world!
```



## uBasic/4tH

<lang>GoSub _Initialize                      ' Initialize memory

i = 0                                  ' Reset instruction pointer

Do While i > -1                        ' While IP is not negative
  A = @(i)                             ' Fill the registers with
  B = @(i+1)                           ' opcodes and operands
  C = @(i+2)

  i = i + 3                            ' Increment instruction counter
                                       ' A<0 = Input, B<0 = Output
  If B < 0 Then Print CHR(@(A)); : Continue
  If A < 0 Then Input "Enter: ";@(B) : Continue
  @(B) = @(B) - @(A) : If @(B) < 1 Then i = C
Loop                                   ' Change memory contents
                                       ' And optionally the IP
End
                                       ' Corresponds to assembler language:
_Initialize                            ' start:
  @(0) = 15                            '   zero, message, -1
  @(1) = 17
  @(2) = -1
  @(3) = 17                            '   message, -1, -1
  @(4) = -1
  @(5) = -1
  @(6) = 16                            '   neg1, start+1, -1
  @(7) = 1
  @(8) = -1
  @(9) = 16                            '   neg1, start+3, -1
  @(10) = 3
  @(11) = -1
  @(12) = 15                           '   zero, zero, start
  @(13) = 15
  @(14) = 0
  @(15) = 0                            ' zero: 0
  @(16) = -1                           ' neg1: -1
  @(17) = 72                           ' message: "Hello, world!\n\0"
  @(18) = 101
  @(19) = 108
  @(20) = 108
  @(21) = 111
  @(22) = 44
  @(23) = 32
  @(24) = 119
  @(25) = 111
  @(26) = 114
  @(27) = 108
  @(28) = 100
  @(29) = 33                           ' Works only with ASCII
  @(30) = 10                           ' Replace with =ORD(c) when required
  @(31) = 0
Return
```

```txt
Hello, world!

0 OK, 0:2010
```



## zkl

```zkl
fcn subleq(a,a1,a2,etc){ a=vm.arglist.copy();
   i:=0;
   while(i>=0){ A,B,C:=a[i,3];
      if(A==-1) a[B]=ask("::").toInt(); // or File.stdin.read(1)[0] // int
      else if(B==-1) print(a[A].toChar());
      else if( (a[B]-=a[A]) <=0) { i=C; continue; }
      i+=3;
   }
}
```


```zkl
subleq(15, 17, -1, 17,  -1,  -1,  16,   1, -1, 16,   3,  -1,  15,  15,
        0,  0, -1, 72, 101, 108, 108, 111, 44, 32, 119, 111, 114, 108,
      100, 33, 10,  0);
```

```txt
Hello, world!
```



## ZX Spectrum Basic

Reads the Subleq program from the keyboard, as space-separated numbers, and executes it. A couple of implementation details (arrays being indexed from 1 rather than from 0; the control character ASCII 10 needing to be intercepted specially, because it would otherwise be printed as <code>?</code> rather than as a newline character) are hidden from the Subleq programmer. Lines <code>10</code> to <code>140</code> are the machine code loader, lines <code>150</code> to <code>310</code> the VM.

```zxbasic
 10 DIM m(512)
 20 INPUT p$
 30 LET word=1
 40 LET char=1
 50 IF char<LEN p$ THEN GO TO 80
 60 LET m(word)=VAL p$
 70 GO TO 150
 80 IF p$(char)=" " THEN GO TO 110
 90 LET char=char+1
100 GO TO 50
110 LET m(word)=VAL p$( TO char-1)
120 LET p$=p$(char+1 TO )
130 LET word=word+1
140 GO TO 40
150 LET ptr=0
160 LET a=m(ptr+1)
170 LET b=m(ptr+2)
180 LET c=m(ptr+3)
190 LET ptr=ptr+3
200 IF a=-1 THEN GO TO 260
210 IF b=-1 THEN GO TO 290
220 LET m(b+1)=m(b+1)-m(a+1)
230 IF m(b+1)<=0 THEN LET ptr=c
240 IF ptr<0 THEN STOP
250 GO TO 160
260 INPUT c$
270 LET m(b+1)=CODE c$
280 GO TO 160
290 IF m(a+1)=10 THEN PRINT : GO TO 160
300 PRINT CHR$ m(a+1);
310 GO TO 160
```

```txt
Hello, world!
```

