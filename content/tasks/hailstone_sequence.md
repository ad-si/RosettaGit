+++
title = "Hailstone sequence"
description = ""
date = 2019-04-25T14:06:58Z
aliases = []
[extra]
id = 6243
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "abap",
  "acl2",
  "algol",
  "apl",
  "awk",
  "ada",
  "aime",
  "applesoft",
  "autohotkey",
  "autoit",
  "basic",
  "bbc",
  "batch",
  "befunge",
  "bracmat",
  "brat",
  "c",
  "c_sharp",
  "c_plus_plus",
  "clips",
  "cobol",
  "ceylon",
  "clojure",
  "coffeescript",
  "commodore",
  "common",
  "crystal",
  "d",
  "dcl",
  "dart",
  "dc",
  "delphi",
  "erlang",
  "eiffel",
  "elena",
  "elixir",
  "euler",
  "euphoria",
  "excel",
  "ezhil",
  "false",
  "forth",
  "fortran",
  "frege",
  "frink",
  "funl",
  "futhark",
  "gap",
  "go",
  "groovy",
  "haskell",
  "inform",
  "io",
  "ioke",
  "j",
  "java",
  "javascript",
  "julia",
  "k",
  "kotlin",
  "lua",
  "ml",
  "ocaml",
  "pascal",
  "perl",
  "php",
  "prolog",
  "pure",
  "python",
  "r",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "smalltalk",
  "swift",
  "tcl",
  "vbscript",
  "visual_basic",
  "xpl0",
  "zkl",
]
+++

The Hailstone sequence of numbers can be generated from a starting positive integer,   n   by:
*   If   n   is     '''1'''     then the sequence ends.
*   If   n   is   '''even''' then the next   n   of the sequence   <big><code> = n/2 </code></big>
*   If   n   is   '''odd'''    then the next   n   of the sequence   <big><code> = (3 * n) + 1 </code></big>


The (unproven) [[wp:Collatz conjecture|Collatz conjecture]] is that the hailstone sequence for any starting number always terminates.


The hailstone sequence is also known as   ''hailstone numbers''   (because the values are usually subject to multiple descents and ascents like hailstones in a cloud).

This sequence is also known as the   ''Collatz sequence''.


## Task

# Create a routine to generate the hailstone sequence for a number.
# Use the routine to show that the hailstone sequence for the number 27 has 112 elements starting with <code>27, 82, 41, 124</code> and ending with <code>8, 4, 2, 1</code>
# Show the number less than 100,000 which has the longest hailstone sequence together with that sequence's length.
    (But don't show the actual sequence!)


## See also

*   [http://xkcd.com/710 xkcd] (humourous).





## 360 Assembly


```360asm
*        Hailstone sequence        16/08/2015
HAILSTON CSECT
         USING  HAILSTON,R12
         LR     R12,R15
         ST     R14,SAVER14
BEGIN    L      R11,=F'100000'     nmax
         LA     R8,27              n=27
         LR     R1,R8
         MVI    FTAB,X'01'         ftab=true
         BAL    R14,COLLATZ
         LR     R10,R1             p
         XDECO  R8,XDEC            n
         MVC    BUF1+10(6),XDEC+6
         XDECO  R10,XDEC           p
         MVC    BUF1+18(5),XDEC+7
         LA     R5,6
         LA     R3,0               i
         LA     R4,BUF1+25
LOOPED   L      R2,TAB(R3)         tab(i)
         XDECO  R2,XDEC
         MVC    0(7,R4),XDEC+5
         LA     R3,4(R3)           i=i+1
         LA     R4,7(R4)
         C      R5,=F'4'
         BNE    BCT
         LA     R4,7(R4)
BCT      BCT    R5,LOOPED
         XPRNT  BUF1,80            print hailstone(n)=p,tab(*)
         MVC    LONGEST,=F'0'      longest=0
         MVI    FTAB,X'00'         ftab=true
         LA     R8,1               i
LOOPI    CR     R8,R11             do i=1 to nmax
         BH     ELOOPI
         LR     R1,R8              n
         BAL    R14,COLLATZ
         LR     R10,R1             p
         L      R4,LONGEST
         CR     R4,R10             if longest<p
         BNL    NOTSUP
         ST     R8,IVAL            ival=i
         ST     R10,LONGEST        longest=p
NOTSUP   LA     R8,1(R8)           i=i+1
         B      LOOPI
ELOOPI   EQU    *                  end i
         XDECO  R11,XDEC           maxn
         MVC    BUF2+9(6),XDEC+6
         L      R1,IVAL            ival
         XDECO  R1,XDEC
         MVC    BUF2+28(6),XDEC+6
         L      R1,LONGEST         longest
         XDECO  R1,XDEC
         MVC    BUF2+36(5),XDEC+7
         XPRNT  BUF2,80            print maxn,hailstone(ival)=longest
         B      RETURN
*        *      *                  r1=collatz(r1)
COLLATZ  LR     R7,R1              m=n  (R7)
         LA     R6,1               p=1  (R6)
LOOPP    C      R7,=F'1'           do p=1 by 1 while(m>1)
         BNH    ELOOPP
         CLI    FTAB,X'01'         if ftab
         BNE    NONOK
         C      R6,=F'1'           if p>=1
         BL     NONOK
         C      R6,=F'3'           & p<=3
         BH     NONOK
         LR     R1,R6              then
         BCTR   R1,0
         SLA    R1,2
         ST     R7,TAB(R1)         tab(p)=m
NONOK    LR     R4,R7              m
         N      R4,=F'1'           m&1
         LTR    R4,R4              if m//2=0  (if not(m&1))
         BNZ    ODD
EVEN     SRA    R7,1               m=m/2
         B      EIFM
ODD      LA     R3,3
         MR     R2,R7              *m
         LA     R7,1(R3)           m=m*3+1
EIFM     CLI    FTAB,X'01'         if ftab
         BNE    NEXTP
         MVC    TAB+12,TAB+16      tab(4)=tab(5)
         MVC    TAB+16,TAB+20      tab(5)=tab(6)
         ST     R7,TAB+20          tab(6)=m
NEXTP    LA     R6,1(R6)           p=p+1
         B      LOOPP
ELOOPP   LR     R1,R6              end p; return(p)
         BR     R14                end collatz
*
RETURN   L      R14,SAVER14        restore caller address
         XR     R15,R15            set return code
         BR     R14                return to caller
SAVER14  DS     F
IVAL     DS     F
LONGEST  DS     F
N        DS     F
TAB      DS     6F
FTAB     DS     X
BUF1     DC     CL80'hailstone(nnnnnn)=nnnnn : nnnnnn nnnnnn nnnnnn ...*
               ... nnnnnn nnnnnn nnnnnn'
BUF2     DC     CL80'longest <nnnnnn : hailstone(nnnnnn)=nnnnn'
XDEC     DS     CL12
         YREGS
         END    HAILSTON
```

```txt

hailstone(    27)=  112 :     27     82     41 ......      4      2      1
longest <100000 : hailstone( 77031)=  351

```



## ABAP


```ABAP

CLASS lcl_hailstone DEFINITION.
  PUBLIC SECTION.
    TYPES: tty_sequence TYPE STANDARD TABLE OF i
                             WITH NON-UNIQUE EMPTY KEY,
           BEGIN OF ty_seq_len,
             start TYPE i,
             len   TYPE i,
           END OF ty_seq_len,
           tty_seq_len TYPE HASHED TABLE OF ty_seq_len
                            WITH UNIQUE KEY start.

    CLASS-METHODS:
      get_next
        IMPORTING
          n                           TYPE i
        RETURNING
          VALUE(r_next_hailstone_num) TYPE i,

      get_sequence
        IMPORTING
          start             TYPE i
        RETURNING
          VALUE(r_sequence) TYPE tty_sequence,

      get_longest_sequence_upto
        IMPORTING
          limit                     TYPE i
        RETURNING
          VALUE(r_longest_sequence) TYPE ty_seq_len.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_seq,
             start TYPE i,
             seq   TYPE tty_sequence,
           END OF ty_seq.
    CLASS-DATA: sequence_buffer TYPE HASHED TABLE OF ty_seq
                                     WITH UNIQUE KEY start.
ENDCLASS.

CLASS lcl_hailstone IMPLEMENTATION.
  METHOD get_next.
    r_next_hailstone_num = COND #( WHEN n MOD 2 = 0 THEN n / 2
                                   ELSE ( 3 * n ) + 1 ).
  ENDMETHOD.

  METHOD get_sequence.
    INSERT start INTO TABLE r_sequence.
    IF start = 1.
      RETURN.
    ENDIF.

    READ TABLE sequence_buffer ASSIGNING FIELD-SYMBOL(<buff>)
                               WITH TABLE KEY start = start.
    IF sy-subrc = 0.
      INSERT LINES OF <buff>-seq INTO TABLE r_sequence.
    ELSE.
      DATA(seq) = get_sequence( get_next( start ) ).
      INSERT LINES OF seq INTO TABLE r_sequence.
      INSERT VALUE ty_seq( start = start
                           seq   = seq ) INTO TABLE sequence_buffer.
    ENDIF.
  ENDMETHOD.

  METHOD get_longest_sequence_upto.
    DATA: max_seq TYPE ty_seq_len,
          act_seq TYPE ty_seq_len.

    DO limit TIMES.
      act_seq-len = lines( get_sequence( sy-index ) ).

      IF act_seq-len > max_seq-len.
        max_seq-len   = act_seq-len.
        max_seq-start = sy-index.
      ENDIF.
    ENDDO.

    r_longest_sequence = max_seq.
  ENDMETHOD.
ENDCLASS.

START-OF-SELECTION.
  cl_demo_output=>begin_section( |Hailstone sequence of 27 is: | ).
  cl_demo_output=>write( REDUCE string( INIT result = ``
                                        FOR item IN lcl_hailstone=>get_sequence( 27 )
                                        NEXT result = |{ result } { item }| ) ).
  cl_demo_output=>write( |With length: { lines( lcl_hailstone=>get_sequence( 27 ) ) }| ).
  cl_demo_output=>begin_section( |Longest hailstone sequence upto 100k| ).
  cl_demo_output=>write( lcl_hailstone=>get_longest_sequence_upto( 100000 ) ).
  cl_demo_output=>display( ).

```

```txt

Hailstone sequence of 27 is:

27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274 137 412 206 103 310 155 466 233 700 350 175 526 263 790 395 1186 593 1780 890 445 1336 668 334 167 502 251 754 377 1132 566 283 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 1619 4858 2429 7288 3644 1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154 577 1732 866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35 106 53 160 80 40 20 10 5 16 8 4 2 1

With length: 112

Longest hailstone sequence upto 100k

Structure
START LEN
77031 351


```



## ACL2


```Lisp
(defun hailstone (len)
    (loop for x = len
             then (if (evenp x)
                         (/ x 2)
                         (+ 1 (* 3 x)))
        collect x until (= x 1)))

;; Must be tail recursive
(defun max-hailstone-start (limit mx curr)
   (declare (xargs :mode :program))
   (if (zp limit)
       (mv mx curr)
       (let ((new-mx (len (hailstone limit))))
          (if (> new-mx mx)
              (max-hailstone-start (1- limit) new-mx limit)
              (max-hailstone-start (1- limit) mx curr)))))
```


```txt
&gt; (take 4 (hailstone 27))
(27 82 41 124)
&gt; (nthcdr 108 (hailstone 27))
(8 4 2 1)
&gt; (len (hailstone 27))
112
&gt; (max-hailstone-start 100000 0 0)
(351 77031)
```



## Ada

Similar to [[#C|C method]]:

```Ada
with Ada.Text_IO; use Ada.Text_IO;
procedure hailstone is
	type int_arr is array(Positive range <>) of Integer;
	type int_arr_pt is access all int_arr;

	function hailstones(num:Integer; pt:int_arr_pt) return Integer is
		stones : Integer := 1;
		n : Integer := num;
		begin
		if pt /= null then pt(1) := num; end if;
		while (n/=1) loop
			stones := stones + 1;
			if n mod 2 = 0 then n := n/2;
			else n := (3*n)+1;
			end if;
			if pt /= null then pt(stones) := n; end if;
		end loop;
		return stones;
	end hailstones;

	nmax,stonemax,stones : Integer := 0;
	list : int_arr_pt;
begin
	stones := hailstones(27,null);
	list := new int_arr(1..stones);
	stones := hailstones(27,list);
	put(" 27: "&Integer'Image(stones)); new_line;
	for n in 1..4 loop put(Integer'Image(list(n)));	end loop;
	put(" .... ");
	for n in stones-3..stones loop put(Integer'Image(list(n))); end loop;
	new_line;
	for n in 1..100000 loop
		stones := hailstones(n,null);
		if stones>stonemax then
			nmax := n; stonemax := stones;
		end if;
	end loop;
	put_line(Integer'Image(nmax)&" max @ n= "&Integer'Image(stonemax));
end hailstone;
```

```txt

 27:  112
 27 82 41 124 ....  8 4 2 1
 77031 max @ n=  351

```



### Alternative method

A method without pointers or dynamic memory allocation, but slower for simply counting. This is also used for the "executable library" task [[Executable library#Ada]].

hailstones.ads:

```Ada
package Hailstones is
   type Integer_Sequence is array(Positive range <>) of Integer;
   function Create_Sequence (N : Positive) return Integer_Sequence;
end Hailstones;
```

hailstones.adb:

```Ada
package body Hailstones is
   function Create_Sequence (N : Positive) return Integer_Sequence is
   begin
      if N = 1 then
         -- terminate
         return (1 => N);
      elsif N mod 2 = 0 then
         -- even
         return (1 => N) & Create_Sequence (N / 2);
      else
         -- odd
         return (1 => N) & Create_Sequence (3 * N + 1);
      end if;
   end Create_Sequence;
end Hailstones;
```

example main.adb:

```Ada
with Ada.Text_IO;
with Hailstones;

procedure Main is
   package Integer_IO is new Ada.Text_IO.Integer_IO (Integer);

   procedure Print_Sequence (X : Hailstones.Integer_Sequence) is
   begin
      for I in X'Range loop
         Integer_IO.Put (Item => X (I), Width => 0);
         if I < X'Last then
            Ada.Text_IO.Put (", ");
         end if;
      end loop;
      Ada.Text_IO.New_Line;
   end Print_Sequence;

   Hailstone_27 : constant Hailstones.Integer_Sequence :=
     Hailstones.Create_Sequence (N => 27);

begin
   Ada.Text_IO.Put_Line ("Length of 27:" & Integer'Image (Hailstone_27'Length));
   Ada.Text_IO.Put ("First four: ");
   Print_Sequence (Hailstone_27 (Hailstone_27'First .. Hailstone_27'First + 3));
   Ada.Text_IO.Put ("Last four: ");
   Print_Sequence (Hailstone_27 (Hailstone_27'Last - 3 .. Hailstone_27'Last));

   declare
      Longest_Length : Natural := 0;
      Longest_N      : Positive;
      Length         : Natural;
   begin
      for I in 1 .. 99_999 loop
         Length := Hailstones.Create_Sequence (N => I)'Length;
         if Length > Longest_Length then
            Longest_Length := Length;
            Longest_N := I;
         end if;
      end loop;
      Ada.Text_IO.Put_Line ("Longest length is" & Integer'Image (Longest_Length));
      Ada.Text_IO.Put_Line ("with N =" & Integer'Image (Longest_N));
   end;
end Main;
```

```txt
Length of 27: 112
First four: 27, 82, 41, 124
Last four: 8, 4, 2, 1
Longest length is 351
with N = 77031
```



## Aime


```aime
void
print_hailstone(integer h)
{
    list l;

    while (h ^ 1) {
        lb_p_integer(l, h);
        h = h & 1 ? 3 * h + 1 : h / 2;
    }

    o_form("hailstone sequence for ~ is ~1 ~ ~ ~ .. ~ ~ ~ ~, it is ~ long\n",
           l[0], l[1], l[2], l[3], l[-3], l[-2], l[-1], 1, ~l + 1);
}

void
max_hailstone(integer x)
{
    integer e, i, m;
    index r;

    m = 0;
    i = 1;
    while (i < x) {
        integer h, k, l;

        h = i;
        l = 1;
        while (h ^ 1) {
            if (i_j_integer(k, r, h)) {
                l += k;
                break;
            } else {
                l += 1;
                h = h & 1 ? 3 * h + 1 : h / 2;
            }
        }

        r[i] = l - 1;

        if (m < l) {
            m = l;
            e = i;
        }

        i += 1;
    }

    o_form("hailstone sequence length for ~ is ~\n", e, m);
}

integer
main(void)
{
    print_hailstone(27);
    max_hailstone(100000);

    return 0;
}
```

```txt
hailstone sequence for 27 is 27 82 41 124 .. 8 4 2 1, it is 112 long
hailstone sequence length for 77031 is 351
```



## ALGOL 68

{{trans|C}} - note: This specimen retains the original C coding style.
```algol68
MODE LINT = # LONG ... # INT;

PROC hailstone = (INT in n, REF[]LINT array)INT:
(
    INT hs := 1;
    INT index := 0;
    LINT n := in n;

    WHILE n /= 1 DO
        hs +:= 1;
        IF array ISNT REF[]LINT(NIL) THEN array[index +:= 1] := n FI;
        n := IF ODD n THEN 3*n+1 ELSE n OVER 2 FI
    OD;
    IF array ISNT REF[]LINT(NIL) THEN array[index +:= 1] := n FI;
    hs
);

main:
(
    INT j, hmax := 0;
    INT jatmax, n;
    INT border = 4;

    FOR j TO 100000-1 DO
       n := hailstone(j, NIL);
       IF hmax < n THEN
           hmax := n;
           jatmax := j
       FI
    OD;

    [2]INT test := (27, jatmax);
    FOR key TO UPB test DO
        INT val = test[key];
        n := hailstone(val, NIL);
        [n]LINT array;
        n := hailstone(val, array);

        printf(($"[ "n(border)(g(0)", ")" ..."n(border)(", "g(0))"] len="g(0)l$,
            array[:border], array[n-border+1:], n))
        #;free(array) #
    OD;
    printf(($"Max "g(0)" at j="g(0)l$, hmax, jatmax))
# ELLA Algol68RS:
    print(("Max",hmax," at j=",jatmax, new line))
#
)
```

```txt

[ 27, 82, 41, 124,  ..., 8, 4, 2, 1] len=112
[ 77031, 231094, 115547, 346642,  ..., 8, 4, 2, 1] len=351
Max 351 at j=77031

```



## ALGOL W


```algolw
begin
    % show some Hailstone Sequence related information                       %
    % calculates the length of the sequence generated by n,                  %
    % if showFirstAndLast is true, the first and last 4 elements of the      %
    % sequence are stored in first and last                                  %
    % hs holds a cache of the upbHs previously calculated sequence lengths   %
    % if showFirstAndLast is false, the cache will be used                   %
    procedure hailstone ( integer value  n
                        ; integer array  first, last ( * )
                        ; integer result length
                        ; integer array  hs          ( * )
                        ; integer value  upbHs
                        ; logical value  showFirstAndLast
                        ) ;
    if not showFirstAndLast and n <= upbHs and hs( n ) not = 0 then begin
        % no need to store the start and end of the sequence and we already  %
        % know the length of the sequence for n                              %
        length := hs( n )
        end
    else begin
        % must calculate the sequence length                                 %
        integer sv;
        for i := 1 until 4 do first( i ) := last( i ) := 0;
        length := 0;
        sv     := n;
        if sv > 0 then begin
            while begin
                length := length + 1;
                if showFirstAndLast then begin
                    if length <= 4 then first( length ) := sv;
                    for lPos := 1 until 3 do last( lPos ) := last( lPos + 1 );
                    last( 4 ) := sv
                    end
                else if sv <= upbHs and hs( sv ) not = 0 then begin
                    % have a known value                                 %
                    length := ( length + hs( sv ) ) - 1;
                    sv     := 1
                end ;
                sv not = 1
            end do begin
                sv := if odd( sv ) then ( 3 * sv ) + 1 else sv div 2
            end while_sv_ne_1 ;
            if n < upbHs then hs( n ) := length
        end if_sv_gt_0
    end hailstone ;
    begin
        % test the hailstone procedure                                       %
        integer HS_CACHE_SIZE;
        HS_CACHE_SIZE := 100000;
        begin
            integer array first, last ( 1 :: 4 );
            integer       length, maxLength, maxNumber;
            integer array hs          ( 1 :: HS_CACHE_SIZE );
            for i := 1 until HS_CACHE_SIZE do hs( i ) := 0;
            hailstone( 27, first, last, length, hs, HS_CACHE_SIZE, true );
            write( i_w := 1, s_w := 0
                 , "27: length ", length, ", first: ["
                 , first( 1 ), " ", first( 2 ), " ", first( 3 ), " ", first( 4 )
                 , "] last: ["
                 , last( 1 ), " ", last( 2 ), " ", last( 3 ), " ", last( 4 )
                 , "]"
                 );
            maxNumber := 0;
            maxLength := 0;
            for n := 1 until 100000 do begin
                hailstone( n, first, last, length, hs, HS_CACHE_SIZE, false );
                if length > maxLength then begin
                    maxNumber := n;
                    maxLength := length
                end if_length_gt_maxLength
            end for_n ;
            write( i_w := 1, s_w := 1, "Maximum sequence length: ", maxLength, " for: ", maxNumber )
        end
    end
end.
```

```txt

27: length 112, first: [27 82 41 124] last: [8 4 2 1]
Maximum sequence length: 351  for: 77031

```



## APL

```APL
seq←hailstone n;next
⍝ Returns the hailstone sequence for a given number

seq←n                   ⍝ Init the sequence
:While n≠1
    next←(n÷2) (1+3×n)  ⍝ Compute both possibilities
    n←next[1+2|n]       ⍝ Pick the appropriate next step
    seq,←n              ⍝ Append that to the sequence
:EndWhile
```

```APL
 5↑hailstone 27
27 82 41 124 62
 ¯5↑hailstone 27
16 8 4 2 1
 ⍴hailstone 27
112
 1↑{⍵[⍒↑(⍴∘hailstone)¨⍵]}⍳100000
77031
```



## AutoHotkey


```autohotkey
; Submitted by MasterFocus --- http://tiny.cc/iTunis

; [1] Generate the Hailstone Seq. for a number

List := varNum := 7 ; starting number is 7, not counting elements
While ( varNum > 1 )
  List .= ", " ( varNum := ( Mod(varNum,2) ? (varNum*3)+1 : varNum//2 ) )
MsgBox % List

; [2] Seq. for starting number 27 has 112 elements

Count := 1, List := varNum := 27 ; starting number is 27, counting elements
While ( varNum > 1 )
  Count++ , List .= ", " ( varNum := ( Mod(varNum,2) ? (varNum*3)+1 : varNum//2 ) )
MsgBox % "Sequence:`n" List "`n`nCount: " Count

; [3] Find number<100000 with longest seq. and show both values

MaxNum := Max := 0 ; reset the Maximum variables
TimesToLoop := 100000 ; limit number here is 100000
Offset := 70000 ; offset - use 0 to process from 0 to 100000
Loop, %TimesToLoop%
{
  If ( TimesToLoop < ( varNum := Index := A_Index+Offset ) )
    Break
  text := "Processing...`n-------------------`n"
  text .= "Current starting number: " Index "`n"
  text .= "Current sequence count: " Count
  text .= "`n-------------------`n"
  text .= "Maximum starting number: " MaxNum "`n"
  text .= "Maximum sequence count: " Max " <<" ; text split to avoid long code lines
  ToolTip, %text%
  Count := 1 ; going to count the elements, but no "List" required
  While ( varNum > 1 )
    Count++ , varNum := ( Mod(varNum,2) ? (varNum*3)+1 : varNum//2 )
  If ( Count > Max )
    Max := Count , MaxNum := Index ; set the new maximum values, if necessary
}
ToolTip
MsgBox % "Number: " MaxNum "`nCount: " Max
```


## AutoIt




```autoit

$Hail = Hailstone(27)
ConsoleWrite("Sequence-Lenght: "&$Hail&@CRLF)
$Big = -1
$Sequenzlenght = -1
For $I = 1 To 100000
	$Hail = Hailstone($i, False)
	If Number($Hail) > $Sequenzlenght Then
	$Sequenzlenght = Number($Hail)
	$Big = $i
	EndIf
Next
ConsoleWrite("Longest Sequence : "&$Sequenzlenght&" from number "&$Big&@CRLF)
Func Hailstone($int, $sequence = True)
	$Counter = 0
	While True
		$Counter += 1
		If $sequence = True Then ConsoleWrite($int & ",")
		If $int = 1 Then ExitLoop
		If Not Mod($int, 2) Then
			$int = $int / 2
		Else
			$int = 3 * $int + 1
		EndIf
		If Not Mod($Counter, 25) AND $sequence = True Then ConsoleWrite(@CRLF)
	WEnd
	If $sequence = True Then ConsoleWrite(@CRLF)
	Return $Counter
EndFunc   ;==>Hailstone

```

```txt
27,82,41,124,62,31,94,47,142,71,214,107,322,161,484,242,121,364,182,91,274,137,412,206,103,
310,155,466,233,700,350,175,526,263,790,395,1186,593,1780,890,445,1336,668,334,167,502,251,754,377,1132,
566,283,850,425,1276,638,319,958,479,1438,719,2158,1079,3238,1619,4858,2429,7288,3644,1822,911,2734,1367,4102,2051,
6154,3077,9232,4616,2308,1154,577,1732,866,433,1300,650,325,976,488,244,122,61,184,92,46,23,70,35,106,
53,160,80,40,20,10,5,16,8,4,2,1,
Sequence-Lenght: 112
Longest Sequence : 351 from number 77031

```



## AWK


```awk

#!/usr/bin/awk -f
function hailstone(v, verbose) {
	n = 1;
	u = v;
	while (1) {
		if (verbose) printf " "u;
		if (u==1) return(n);
		n++;
		if (u%2 > 0 )
			u = 3*u+1;
		else
			u = u/2;
	}
}

BEGIN {
	i = 27;
	printf("hailstone(%i) has %i elements\n",i,hailstone(i,1));
	ix=0;
	m=0;
	for (i=1; i<100000; i++) {
		n = hailstone(i,0);
		if (m<n) {
			m=n;
			ix=i;
		}
	}
	printf("longest hailstone sequence is %i and has %i elements\n",ix,m);
}

```

```txt

27 82 41 124 ....... 8 4 2 1
hailstone(27) has 112 elements
longest hailstone sequence is 77031 and has 351 elements

```



## BASIC

=
## Applesoft BASIC
=

```ApplesoftBasic
10 HOME

100 N = 27
110 GOSUB 400"HAILSTONE
120 DEF FN L(I) = E(I + 4 * (I < 0))
130IFL=112AND(S(0)=27ANDS(1)=82ANDS(2)=41ANDS(3)=124)AND(FNL(M-3)=8ANDFNL(M-2)=4ANDFNL(M-1)=2ANDFNL(M)=1)THENPRINT"THE HAILSTONE SEQUENCE FOR THE NUMBER 27 HAS 112 ELEMENTS STARTING WITH 27, 82, 41, 124 AND ENDING WITH 8, 4, 2, 1"
140 PRINT
150 V = PEEK(37) + 1

200 N = 1
210 GOSUB 400"HAILSTONE
220 MN = 1
230 ML = L
240 FOR I = 2 TO 99999
250     N = I
260     GOSUB 400"HAILSTONE
270     IFL>MLTHENMN=I:ML=L:VTABV:HTAB1:PRINT "THE NUMBER " MN " HAS A HAILSTONE SEQUENCE LENGTH OF "L" WHICH IS THE LONGEST HAILSTONE SEQUENCE OF NUMBERS LESS THAN ";:Y=PEEK(37)+1:X=PEEK(36)+1
280     IF Y THEN VTAB Y : HTAB X : PRINTI+1;
290 NEXT I

300 END

400 M = 0
410 FOR L = 1 TO 1E38
420     IF L < 5 THEN S(L-1) = N
430     M = (M + 1) * (M < 3)
440     E(M) = N
450     IF N = 1 THEN RETURN
460     EVEN = INT(N/2)=N/2
470     IF EVEN THEN N=N/2
480     IF NOT EVEN THEN N = (3 * N) + 1
490 NEXT L : STOP
```


=
## BBC BASIC
=

```bbcbasic
      seqlen% = FNhailstone(27, TRUE)
      PRINT '"Sequence length = "; seqlen%
      maxlen% = 0
      FOR number% = 2 TO 100000
        seqlen% = FNhailstone(number%, FALSE)
        IF seqlen% > maxlen% THEN
          maxlen% = seqlen%
          maxnum% = number%
        ENDIF
      NEXT
      PRINT "The number with the longest hailstone sequence is " ; maxnum%
      PRINT "Its sequence length is " ; maxlen%
      END

      DEF FNhailstone(N%, S%)
      LOCAL L%
      IF S% THEN PRINT N%;
      WHILE N% <> 1
        IF N% AND 1 THEN N% = 3 * N% + 1 ELSE N% DIV= 2
        IF S% THEN PRINT N%;
        L% += 1
      ENDWHILE
      = L% + 1
```

```txt

        27        82        41       124        62        31        94        47
       142        71       214       107       322       161       484       242
       121       364       182        91       274       137       412       206
       103       310       155       466       233       700       350       175
       526       263       790       395      1186       593      1780       890
       445      1336       668       334       167       502       251       754
       377      1132       566       283       850       425      1276       638
       319       958       479      1438       719      2158      1079      3238
      1619      4858      2429      7288      3644      1822       911      2734
      1367      4102      2051      6154      3077      9232      4616      2308
      1154       577      1732       866       433      1300       650       325
       976       488       244       122        61       184        92        46
        23        70        35       106        53       160        80        40
        20        10         5        16         8         4         2         1

Sequence length = 112
The number with the longest hailstone sequence is 77031
Its sequence length is 351

```


=
## Commodore BASIC
=

```QBASIC
100 PRINT : PRINT "HAILSTONE SEQUENCE FOR N = 27:"
110 N=27 : SHOW=1
120 GOSUB 1000
130 PRINT X"ELEMENTS"
140 PRINT : PRINT "FINDING N WITH THE LONGEST HAILSTONE SEQUENCE"
150 SHOW=0
160 T0 = TI
170 FOR N=2 TO 100000
180 : GOSUB 1000
190 : IF X>MAX THEN MAX=X : NMAX = N
200 : REM' PRINT N,X,MAX
210 NEXT
230 PRINT "LONGEST HAILSTONE SEQUENCE STARTS WITH "NMAX"."
240 PRINT "IT HAS"MAX"ELEMENTS"
260 END
1000 REM '*** HAILSTONE SEQUENCE SUBROUTINE ***
1010 X = 0 : S = N
1020 IF SHOW THEN PRINT S,
1030 X = X+1
1040 IF S=1 THEN RETURN
1050 IF INT(S/2)=S/2 THEN S = S/2 : GOTO 1020
1060 S = 3*S+1
1070 GOTO 1020

```

=
## FreeBASIC
=

```FreeBASIC
' version 17-06-2015
' compile with: fbc -s console

Function hailstone_fast(number As ULongInt) As ULongInt
    ' faster version
    ' only counts the sequence

    Dim As ULongInt count = 1

    While number <> 1
        If (number And 1) = 1 Then
            number += number Shr 1 + 1  ' 3*n+1 and n/2 in one
            count += 2
        Else
            number Shr= 1 ' divide number by 2
            count += 1
        End If
    Wend

    Return count

End Function

Sub hailstone_print(number As ULongInt)
    ' print the number and sequence

    Dim As ULongInt count = 1

    Print "sequence for number "; number
    Print Using "########"; number;   'starting number

    While number <> 1
        If (number And 1) = 1 Then
            number = number * 3 + 1   ' n * 3 + 1
            count += 1
        Else
            number = number \ 2       ' n \ 2
            count += 1
        End If
        Print Using "########"; number;
    Wend

    Print : Print
    Print "sequence length = "; count
    Print
    Print String(79,"-")

End Sub

Function hailstone(number As ULongInt) As ULongInt
    ' normal version
    ' only counts the sequence

    Dim As ULongInt count = 1

    While number <> 1
        If (number And 1) = 1 Then
            number = number * 3 + 1 ' n * 3 + 1
            count += 1
        End If
        number = number \ 2 ' divide number by 2
        count += 1
    Wend

    Return count

End Function

' ------=< MAIN >=------

Dim As ULongInt number
Dim As UInteger x, max_x, max_seq

hailstone_print(27)
Print

For x As UInteger = 1 To 100000
    number = hailstone(x)
    If number > max_seq Then
        max_x = x
        max_seq = number
    End If
Next

Print  "The longest sequence is for "; max_x; ", it has a sequence length of "; max_seq

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print : Print "hit any key to end program"
Sleep
End
```

```txt
sequence for number 27
     27      82      41     124      62      31      94      47     142      71
    214     107     322     161     484     242     121     364     182      91
    274     137     412     206     103     310     155     466     233     700
    350     175     526     263     790     395    1186     593    1780     890
    445    1336     668     334     167     502     251     754     377    1132
    566     283     850     425    1276     638     319     958     479    1438
    719    2158    1079    3238    1619    4858    2429    7288    3644    1822
    911    2734    1367    4102    2051    6154    3077    9232    4616    2308
   1154     577    1732     866     433    1300     650     325     976     488
    244     122      61     184      92      46      23      70      35     106
     53     160      80      40      20      10       5      16       8       4
      2       1

sequence length = 112
-------------------------------------------------------------------------------
The longest sequence is for 77031, it has a sequence length of 351
```


=
## Liberty BASIC
=

```lb
print "Part 1: Create a routine to generate the hailstone sequence for a number."
print ""
while hailstone < 1 or hailstone <> int(hailstone)
    input "Please enter a positive integer: "; hailstone
wend
print ""
print "The following is the 'Hailstone Sequence' for your number..."
print ""
print hailstone
while hailstone <> 1
    if hailstone / 2 = int(hailstone / 2) then hailstone = hailstone / 2 else hailstone = (3 * hailstone) + 1
    print hailstone
wend
print ""
input "Hit 'Enter' to continue to part 2...";dummy$
cls
print "Part 2: Use the routine to show that the hailstone sequence for the number 27 has 112 elements starting with 27, 82, 41, 124 and ending with 8, 4, 2, 1."
print ""
print "No. in Seq.","Hailstone Sequence Number for 27"
print ""
c = 1: hailstone = 27
print c, hailstone
while hailstone <> 1
    c = c + 1
    if hailstone / 2 = int(hailstone / 2) then hailstone = hailstone / 2 else hailstone = (3 * hailstone) + 1
    print c, hailstone
wend
print ""
input "Hit 'Enter' to continue to part 3...";dummy$
cls
print "Part 3: Show the number less than 100,000 which has the longest hailstone sequence together with that sequence's length.(But don't show the actual sequence)!"
print ""
print "Calculating result... Please wait... This could take a little while..."
print ""
print "Percent Done", "Start Number", "Seq. Length", "Maximum Sequence So Far"
print ""
for cc = 1 to 99999
    hailstone = cc: c = 1
    while hailstone <> 1
        c = c + 1
        if hailstone / 2 = int(hailstone / 2) then hailstone = hailstone / 2 else hailstone = (3 * hailstone) + 1
    wend
    if c > max then max = c: largesthailstone = cc
    locate 1, 7
    print "                                                                    "
    locate 1, 7
    print using("###.###", cc / 99999 * 100);"%", cc, c, max
    scan
next cc
print ""
print "The number less than 100,000 with the longest 'Hailstone Sequence' is "; largesthailstone;". It's sequence length is "; max;"."
end
```


=
## OxygenBasic
=

```oxygenbasic


function Hailstone(sys *n)
'
### ===================

if n and 1
  n=n*3+1
else
  n=n>>1
end if
end function

function HailstoneSequence(sys n) as sys
'
### =================================

count=1
do
  Hailstone n
  Count++
  if n=1 then exit do
end do
return count
end function

'MAIN
'====

maxc=0
maxn=0
e=100000
for n=1 to e
 c=HailstoneSequence n
  if c>maxc
    maxc=c
    maxn=n
  end if
next

print e ", " maxn ", " maxc

'result 100000, 77031, 351

```


=
## PureBasic
=

```PureBasic
NewList Hailstones.i() ; Make a linked list to use as we do not know the numbers of elements needed for an Array

Procedure.i FillHailstones(n) ; Fills the list & returns the amount of elements in the list
  Shared Hailstones()         ; Get access to the Hailstones-List
  ClearList(Hailstones())     ; Remove old data
  Repeat
    AddElement(Hailstones())  ; Add an element to the list
    Hailstones()=n            ; Fill current value in the new list element
    If n=1
      ProcedureReturn ListSize(Hailstones())
    ElseIf n%2=0
      n/2
    Else
      n=(3*n)+1
    EndIf
  ForEver
EndProcedure

If OpenConsole()
  Define i, l, maxl, maxi
  l=FillHailstones(27)
  Print("#27 has "+Str(l)+" elements and the sequence is: "+#CRLF$)
  ForEach Hailstones()
    If i=6
      Print(#CRLF$)
      i=0
    EndIf
    i+1
    Print(RSet(Str(Hailstones()),5))
    If Hailstones()<>1
      Print(", ")
    EndIf
  Next

  i=1
  Repeat
    l=FillHailstones(i)
    If l>maxl
      maxl=l
      maxi=i
    EndIf
    i+1
  Until i>=100000
  Print(#CRLF$+#CRLF$+"The longest sequence below 100000 is #"+Str(maxi)+", and it has "+Str(maxl)+" elements.")

  Print(#CRLF$+#CRLF$+"Press ENTER to exit."): Input()
  CloseConsole()
EndIf
```


```txt

 #27 has 112 elements and the sequence is:
    27,    82,    41,   124,    62,    31,
    94,    47,   142,    71,   214,   107,
   322,   161,   484,   242,   121,   364,
   182,    91,   274,   137,   412,   206,
   103,   310,   155,   466,   233,   700,
   350,   175,   526,   263,   790,   395,
  1186,   593,  1780,   890,   445,  1336,
   668,   334,   167,   502,   251,   754,
   377,  1132,   566,   283,   850,   425,
  1276,   638,   319,   958,   479,  1438,
   719,  2158,  1079,  3238,  1619,  4858,
  2429,  7288,  3644,  1822,   911,  2734,
  1367,  4102,  2051,  6154,  3077,  9232,
  4616,  2308,  1154,   577,  1732,   866,
   433,  1300,   650,   325,   976,   488,
   244,   122,    61,   184,    92,    46,
    23,    70,    35,   106,    53,   160,
    80,    40,    20,    10,     5,    16,
     8,     4,     2,     1

 The longest sequence found up to 100000 is #77031 which has 351 elements.

 Press ENTER to exit.

```


=
## Run BASIC
=

```runbasic
print "Part 1: Create a routine to generate the hailstone sequence for a number."
print ""

while hailstone < 1 or hailstone <> int(hailstone)
    input "Please enter a positive integer: "; hailstone
wend
count = doHailstone(hailstone,"Y")

print: print "Part 2: Use the routine to show that the hailstone sequence for the number 27 has 112 elements starting with 27, 82, 41, 124 and ending with 8, 4, 2, 1."
count = doHailstone(27,"Y")

print: print "Part 3: Show the number less than 100,000 which has the longest hailstone sequence together with that sequence's length.(But don't show the actual sequence)!"
print "Calculating result... Please wait... This could take a little while..."
print "Stone Percent Count"
for i = 1 to 99999
   count = doHailstone(i,"N")
	if count > maxCount then
	   theBigStone = i
	   maxCount	= count
     print using("#####",i);" ";using("###.#", i / 99999 * 100);"% ";using("####",count)
     end if
next i
end

'---------------------------------------------
' pass number and print (Y/N)
FUNCTION doHailstone(hailstone,prnt$)
if prnt$ = "Y" then
 print
 print "The following is the 'Hailstone Sequence' for number:";hailstone
end if
while hailstone <> 1
   if (hailstone and 1) then hailstone = (hailstone * 3) + 1 else hailstone = hailstone / 2
   doHailstone = doHailstone + 1
   if prnt$ = "Y" then
    print hailstone;chr$(9);
    if (doHailstone mod 10) = 0 then print
   end if
wend
END FUNCTION
```



## Batch File

''1. Create a routine to generate the hailstone sequence for a number. ''

''2. Show that the hailstone sequence for the number 27 has 112 elements... ''
<!--The third task is NOT included because task #3 will take several hours in processing...-->

```dos
@echo off
setlocal enabledelayedexpansion
echo.
::Task #1
call :hailstone 111
echo Task #1: (Start:!sav!)
echo !seq!
echo.
echo Sequence has !cnt! elements.
echo.
::Task #2
call :hailstone 27
echo Task #2: (Start:!sav!)
echo !seq!
echo.
echo Sequence has !cnt! elements.
echo.
pause>nul
exit /b 0

::The Function
:hailstone
set num=%1
set seq=%1
set sav=%1
set cnt=0

:loop
set /a cnt+=1
if !num! equ 1 goto :eof
set /a isodd=%num%%%2
if !isodd! equ 0 goto divideby2

set /a num=(3*%num%)+1
set seq=!seq! %num%
goto loop

:divideby2
set /a num/=2
set seq=!seq! %num%
goto loop
```

```txt
Task #1: (Start:111)
111 334 167 502 251 754 377 1132 566 283 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 1619 4858 2429 7288 3644 1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154 577 1732 866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35 106 53 160 80 40 20 10 5 16 8 4 2 1

Sequence has 70 elements.

Task #2: (Start:27)
27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274 137 412 206 103 310 155 466 233 700 350 175 526 263 790 395 1186 593 1780 890 445 1336 668 334 167 502 251 754 377 1132 566 283 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 1619 4858 2429 7288 3644 1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154 577 1732 866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35 106 53 160 80 40 20 10 5 16 8 4 2 1

Sequence has 112 elements.
```


The script above could only be used in '''smaller''' inputs. Thus, for the third task, a slightly different script will be used. However, this script is still '''slow'''. I tried this on a fast computer and it took about 40-45 minutes to complete.

```dos
@echo off
setlocal enableDelayedExpansion
if "%~1"=="test" (
  for /l %%. in () do (
    set /a "test1=num %% 2, cnt=cnt+1"
    if !test1! equ 0 (set /a num/=2 & if !num! equ 1 exit !cnt!) else (set /a num=3*num+1)
  )
)

set max=0
set record=0

for /l %%X in (2,1,100000) do (
	set num=%%X & cmd /c "%~f0" test
	if !errorlevel! gtr !max! (set /a "max=!errorlevel!,record=%%X")
)
set /a max+=1

echo.Number less than 100000 with longest sequence: %record%
echo.With length %max%.
pause>nul

exit /b 0
```

```txt
Number less than 100000 with longest sequence: 77031
With length 351.
```




## beeswax


This approach reuses the main hailstone sequence function for all three tasks.

'''The pure hailstone sequence function''', returning the sequence for any number entered in the console:


```beeswax>
@:N  q
>%"d3~@.PNp
d~2~pL~1F{<T_
```


'''Returning the sequence for the starting value <code>27</code>'''


```beeswax>
@:N  q
>%"d3~@.PNq
d~2~qL~1Ff{<BF3_
{NNgA<
```


Output of the sequence, followed by the length of the sequence:

<lang>
27
82
41
124
62
31
94
47

...

2158
1079
3238
1619
4858
2429
7288
3644
1822

...

16
8
4
2
1

112
```


'''Number below 100,000 with the longest hailstone sequence, and the length of that sequence:'''


```beeswax>
@:  q pf1_#
>%"d3~@.Pqf#{g?` `{gpK@~BP9~5@P@q'M<
d~2~pL~1Ff<         <            >?d
    >zAg?MM@1~y@~gLpz2~yg@~3~hAg?M d
                   >?~fz1~y?yg@hhAg?Mb
```


Output:

<lang>77031 351
```



## Befunge


```befunge
93*:.    v
> :2%v  >
v+1*3_2/
>" ",:.v   v<
<v v-1:< <
+1\_$1+v^ \
v .,+94<>^>::v
>" "03pv  :* p
v67:" "<  0: 1
>p78p25  *^*p0
  v!-1:  <<*^<
9$_:0\  ^-^< v
v01g00:< 1   4
>g"@"*+`v^  <+
v01/"@":_ $ ^,
>p"@"%00p\$:^.
vg01g00  ,+49<
>"@"*+.@

```

```txt

27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274 137 412 206 103 310 155 466 233 700 350 175 526 263 790 395 1186 593 1780 890 445 1336 668 334 167 502 251 754 377 1132 566 283 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 1619 4858 2429 7288 3644 1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154 577 1732 866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35 106 53 160 80 40 20 10 5 16 8 4 2 1
112
77031
351
```



## Bracmat


```bracmat
(
  ( hailstone
  =   L len
    .   !arg:?L
      &   whl
        ' ( !arg:~1
          & (!arg*1/2:~/|3*!arg+1):?arg
          & !arg !L:?L
          )
      & (!L:? [?len&!len.!L)
  )
& ( reverse
  =   L e
    .   :?L
      & whl'(!arg:%?e ?arg&!e !L:?L)
      & !L
  )
& hailstone$27:(?len.?list)
& reverse$!list:?first4 [4 ? [-5 ?last4
& put$"Hailstone sequence starting with "
& put$!first4
& put$(str$(" has " !len " elements and ends with "))
& put$(!last4 \n)
& 1:?N
& 0:?max:?Nmax
&   whl
  ' ( !N+1:<100000:?N
    &   hailstone$!N
      : (   >!max:?max&!N:?Nmax
          | ?
        . ?
        )
    )
&   out
  $ ( str
    $ ( "The number <100000 with the longest hailstone sequence is "
        !Nmax
        " with "
        !max
        " elements."
      )
    )
);
```


=={{header|Brainfuck}}==
Prints the number of terms required to map the input to 1. Does not count the first term of the sequence.
<lang Brainfuck>>,[
    [
        ----------[
            >>>[>>>>]+[[-]+<[->>>>++>>>>+[>>>>]++[->+<<<<<]]<<<]
            ++++++[>------<-]>--[>>[->>>>]+>+[<<<<]>-],<
        ]>
    ]>>>++>+>>[
        <<[>>>>[-]+++++++++<[>-<-]+++++++++>[-[<->-]+[<<<<]]<[>+<-]>]
        >[>[>>>>]+[[-]<[+[->>>>]>+<]>[<+>[<<<<]]+<<<<]>>>[->>>>]+>+[<<<<]]
        >[[>+>>[<<<<+>>>>-]>]<<<<[-]>[-<<<<]]>>>>>>>
    ]>>+[[-]++++++>>>>]<<<<[[<++++++++>-]<.[-]<[-]<[-]<]<,
]
```

<lang Brainfuck>27
111
```



## Brat


```brat
hailstone = { num |
  sequence = [num]
  while { num != 1 }
    { true? num % 2 == 0
      { num = num / 2 }
      { num = num * 3 + 1 }
      sequence << num
    }

  sequence
}

#Check sequence for 27
seq = hailstone 27
true? (seq[0,3] == [27 82 41 124] && seq[-1, -4] == [8 4 2 1])
  { p "Sequence for 27 is correct" }
  { p "Sequence for 27 is not correct!" }

#Find longest sequence for numbers < 100,000
longest = [number: 0 length: 0]

1.to 99999 { index |
    seq = hailstone index
    true? seq.length > longest[:length]
      { longest[:length] = seq.length
        longest[:number] = index
        p "Longest so far: #{index} @ #{longest[:length]} elements"
      }

    index = index + 1
  }

p "Longest was starting from #{longest[:number]} and was of length #{longest[:length]}"
```

```txt
Sequence for 27 is correct
Longest so far: 1 @ 1 elements
Longest so far: 2 @ 2 elements
Longest so far: 3 @ 8 elements
...
Longest so far: 52527 @ 340 elements
Longest so far: 77031 @ 351 elements
Longest was starting from 77031 and was of length 351
```



## Burlesque



```burlesque

blsq ) 27{^^^^2.%{3.*1.+}\/{2./}\/ie}{1!=}w!bx{\/+]}{\/isn!}w!L[
112

```



## C


```c
#include <stdio.h>
#include <stdlib.h>

int hailstone(int n, int *arry)
{
    int hs = 1;

    while (n!=1) {
        hs++;
        if (arry) *arry++ = n;
        n = (n&1) ? (3*n+1) : (n/2);
    }
    if (arry) *arry++ = n;
    return hs;
}

int main()
{
    int j, hmax = 0;
    int jatmax, n;
    int *arry;

    for (j=1; j<100000; j++) {
       n = hailstone(j, NULL);
       if (hmax < n) {
           hmax = n;
           jatmax = j;
       }
    }
    n = hailstone(27, NULL);
    arry = malloc(n*sizeof(int));
    n = hailstone(27, arry);

    printf("[ %d, %d, %d, %d, ...., %d, %d, %d, %d] len=%d\n",
        arry[0],arry[1],arry[2],arry[3],
        arry[n-4], arry[n-3], arry[n-2], arry[n-1], n);
    printf("Max %d at j= %d\n", hmax, jatmax);
    free(arry);

    return 0;
}
```

```txt
[ 27, 82, 41, 124, ...., 8, 4, 2, 1] len= 112
Max 351 at j= 77031
```



### With caching

Much faster if you want to go over a million or so.

```c
#include <stdio.h>

#define N 10000000
#define CS N	/* cache size */

typedef unsigned long ulong;
ulong cache[CS] = {0};

ulong hailstone(ulong n)
{
	int x;
	if (n == 1) return 1;
	if (n < CS && cache[n]) return cache[n];

	x = 1 + hailstone((n & 1) ? 3 * n + 1 : n / 2);
	if (n < CS) cache[n] = x;
	return x;
}

int main()
{
	int i, l, max = 0, mi;
	for (i = 1; i < N; i++) {
		if ((l = hailstone(i)) > max) {
			max = l;
			mi = i;
		}
	}
	printf("max below %d: %d, length %d\n", N, mi, max);
	return 0;
}
```


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Hailstone
{
    class Program
    {
        public static List<int> hs(int n,List<int> seq)
        {
            List<int> sequence = seq;
            sequence.Add(n);
            if (n == 1)
            {
                return sequence;
            }else{
                int newn = (n % 2 == 0) ? n / 2 : (3 * n) + 1;
                return hs(newn, sequence);
            }
        }

        static void Main(string[] args)
        {
            int n = 27;
            List<int> sequence = hs(n,new List<int>());
            Console.WriteLine(sequence.Count + " Elements");
            List<int> start = sequence.GetRange(0, 4);
            List<int> end = sequence.GetRange(sequence.Count - 4, 4);
            Console.WriteLine("Starting with : " + string.Join(",", start) + " and ending with : " + string.Join(",", end));
            int number = 0, longest = 0;
            for (int i = 1; i < 100000; i++)
            {
                int count = (hs(i, new List<int>())).Count;
                if (count > longest)
                {
                    longest = count;
                    number = i;
                }
            }
            Console.WriteLine("Number < 100000 with longest Hailstone seq.: " + number + " with length of " + longest);
       }
    }
}
```


```txt

112 Elements
Starting with : 27,82,41,124 and ending with : 8,4,2,1
Number < 100000 with longest Hailstone seq.: 77031 with length of 351

```



### With caching

As with the [[#C|C example]], much faster if you want to go over a million or so.

```c#
using System;
using System.Collections.Generic;

namespace ConsoleApplication1
{
    class Program
    {
        public static void Main()
        {
            int longestChain = 0, longestNumber = 0;

            var recursiveLengths = new Dictionary<int, int>();

            const int maxNumber = 100000;

            for (var i = 1; i <= maxNumber; i++)
            {
                var chainLength = Hailstone(i, recursiveLengths);
                if (longestChain >= chainLength)
                    continue;

                longestChain = chainLength;
                longestNumber = i;
            }
            Console.WriteLine("max below {0}: {1} ({2} steps)", maxNumber, longestNumber, longestChain);
        }

        private static int Hailstone(int num, Dictionary<int, int> lengths)
        {
            if (num == 1)
                return 1;

            while (true)
            {
                if (lengths.ContainsKey(num))
                    return lengths[num];

                lengths[num] = 1 + ((num%2 == 0) ? Hailstone(num/2, lengths) : Hailstone((3*num) + 1, lengths));
            }
        }
    }
}
```


```txt

max below 100000: 77031 (351 steps)

```



## C++


```cpp
#include <iostream>
#include <vector>
#include <utility>

std::vector<int> hailstone(int i)
{
    std::vector<int> v;
    while(true){
        v.push_back(i);
        if (1 == i) break;
        i = (i % 2) ? (3 * i + 1) : (i / 2);
    }
    return v;
}

std::pair<int,int> find_longest_hailstone_seq(int n)
{
    std::pair<int, int> maxseq(0, 0);
    int l;
    for(int i = 1; i < n; ++i){
        l = hailstone(i).size();
        if (l > maxseq.second) maxseq = std::make_pair(i, l);
    }
    return maxseq;
}

int main () {

// Use the routine to show that the hailstone sequence for the number 27
    std::vector<int> h27;
    h27 = hailstone(27);
// has 112 elements
    int l = h27.size();
    std::cout << "length of hailstone(27) is " << l;
// starting with 27, 82, 41, 124 and
    std::cout << " first four elements of hailstone(27) are ";
    std::cout << h27[0] << " " << h27[1] << " "
              << h27[2] << " " << h27[3] << std::endl;
// ending with 8, 4, 2, 1
    std::cout << " last four elements of hailstone(27) are "
              << h27[l-4] << " " << h27[l-3] << " "
              << h27[l-2] << " " << h27[l-1] << std::endl;

    std::pair<int,int> m = find_longest_hailstone_seq(100000);

    std::cout << "the longest hailstone sequence under 100,000 is " << m.first
              << " with " << m.second << " elements." <<std::endl;

    return 0;
}
```


```txt

 length of hailstone(27) is 112 first four elements of hailstone(27) are 27 82 41 124
 last four elements of hailstone(27) are 8 4 2 1
 the longest hailstone sequence under 100,000 is 77031 with 351 elements.

```


=== {{libheader|Qt}} ===
Templated solution works for all of Qt's sequential container classes (QLinkedList, QList, QVector).

```cpp

#include <QDebug>
#include <QVector>

template <class T>
T hailstone(typename T::value_type n)
{
    T seq;
    for (seq << n; n != 1; seq << n) {
        n = (n&1) ? (3*n)+1 : n/2;
    }
    return seq;
}

template <class T>
T longest_hailstone_seq(typename T::value_type n)
{
    T maxSeq;
    for (; n > 0; --n) {
        const auto seq = hailstone<T>(n);
        if (seq.size() > maxSeq.size()) {
            maxSeq = seq;
        }
    }
    return maxSeq;
}

int main(int, char *[]) {
    const auto seq = hailstone<QVector<uint_fast16_t>>(27);
    qInfo() << "hailstone(27):";
    qInfo() << "  length:" << seq.size() << "elements";
    qInfo() << "  first 4 elements:" << seq.mid(0,4);
    qInfo() << "  last 4 elements:" << seq.mid(seq.size()-4);

    const auto max = longest_hailstone_seq<QVector<uint_fast32_t>>(100000);
    qInfo() << "longest sequence with starting element under 100000:";
    qInfo() << "  length:" << max.size() << "elements";
    qInfo() << "  starting element:" << max.first();
}

```

```txt

hailstone(27):
  length: 112 elements
  first 4 elements: QVector(27, 82, 41, 124)
  last 4 elements: QVector(8, 4, 2, 1)
longest sequence with starting element under 100000:
  length: 351 elements
  starting element: 77031

```



## Ceylon


```ceylon
shared void run() {

	{Integer*} hailstone(variable Integer n) {
		variable [Integer*] stones = [n];
		while(n != 1) {
			n = if(n.even) then n / 2 else 3 * n + 1;
			stones = stones.append([n]);
		}
		return stones;
	}

	value hs27 = hailstone(27);
	print("hailstone sequence for 27 is ``hs27.take(3)``...``hs27.skip(hs27.size - 3).take(3)`` with length ``hs27.size``");

	variable value longest = hailstone(1);
	for(i in 2..100k - 1) {
		value current = hailstone(i);
		if(current.size > longest.size) {
			longest = current;
		}
	}
	print("the longest sequence under 100,000 starts with ``longest.first else "what?"`` and has length ``longest.size``");
}
```



## CLIPS


```clips
(deftemplate longest
  (slot bound)             ; upper bound for the range of values to check
  (slot next (default 2))  ; next value that needs to be checked
  (slot start (default 1)) ; starting value of longest sequence
  (slot len (default 1))   ; length of longest sequence
)

(deffacts startup
  (query 27)
  (longest (bound 100000))
)

(deffunction hailstone-next
  (?n)
  (if (evenp ?n)
    then (div ?n 2)
    else (+ (* 3 ?n) 1)
  )
)

(defrule extend-sequence
  ?hail <- (hailstone $?sequence ?tail&:(> ?tail 1))
  =>
  (retract ?hail)
  (assert (hailstone ?sequence ?tail (hailstone-next ?tail)))
)

(defrule start-query
  (query ?num)
  =>
  (assert (hailstone ?num))
)

(defrule result-query
  (query ?num)
  (hailstone ?num $?sequence 1)
  =>
  (bind ?sequence (create$ ?num ?sequence 1))
  (printout t "Hailstone sequence starting with " ?num ":" crlf)
  (bind ?len (length ?sequence))
  (printout t "  Length: " ?len crlf)
  (printout t "  First four: " (implode$ (subseq$ ?sequence 1 4)) crlf)
  (printout t "  Last four: " (implode$ (subseq$ ?sequence (- ?len 3) ?len)) crlf)
  (printout t crlf)
)

(defrule longest-create-next-hailstone
  (longest (bound ?bound) (next ?next))
  (test (<= ?next ?bound))
  (not (hailstone ?next $?))
  =>
  (assert (hailstone ?next))
)

(defrule longest-check-next-hailstone
  ?longest <- (longest (bound ?bound) (next ?next) (start ?start) (len ?len))
  (test (<= ?next ?bound))
  ?hailstone <- (hailstone ?next $?sequence 1)
  =>
  (retract ?hailstone)
  (bind ?thislen (+ 2 (length ?sequence)))
  (if (> ?thislen ?len) then
    (modify ?longest (start ?next) (len ?thislen) (next (+ ?next 1)))
    else
    (modify ?longest (next (+ ?next 1)))
  )
)

(defrule longest-finished
  (longest (bound ?bound) (next ?next) (start ?start) (len ?len))
  (test (> ?next ?bound))
  =>
  (printout t "The number less than " ?bound " that has the largest hailstone" crlf)
  (printout t "sequence is " ?start " with a length of " ?len "." crlf)
  (printout t crlf)
)
```


```txt
The number less than 100000 that has the largest hailstone
sequence is 77031 with a length of 351.

Hailstone sequence starting with 27:
  Length: 112
  First four: 27 82 41 124
  Last four: 8 4 2 1
```



## Clojure


```clojure
(defn hailstone-seq [n]
  {:pre [(pos? n)]}
  (lazy-seq
   (cond (= n 1)   '(1)
         (even? n) (cons n (hailstone-seq (/ n 2)))
         :else     (cons n (hailstone-seq (+ (* n 3) 1))))))

(let [hseq (hailstone-seq 27)]
  (->  hseq count      (= 112)            assert)
  (->> hseq (take 4)   (= [27 82 41 124]) assert)
  (->> hseq (drop 108) (= [8 4 2 1])      assert))

(let [{max-i :num, max-len :len}
      (reduce #(max-key :len %1 %2)
              (for [i (range 1 100000)]
                {:num i, :len (count (hailstone-seq i))}))]
  (println "Maximum length" max-len "was found for hailstone(" max-i ")."))
```



## COBOL

Testing with GnuCOBOL

```COBOL
       identification division.
       program-id. hailstones.
       remarks. cobc -x hailstones.cob.

       data division.
       working-storage section.
       01 most                 constant as 1000000.
       01 coverage             constant as 100000.
       01 stones               usage binary-long.
       01 n                    usage binary-long.
       01 storm                usage binary-long.

       01 show-arg             pic 9(6).
       01 show-default         pic 99 value 27.
       01 show-sequence        usage binary-long.
       01 longest              usage binary-long occurs 2 times.

       01 filler.
          05 hail              usage binary-long
                               occurs 0 to most depending on stones.
       01 show                 pic z(10).
       01 low-range            usage binary-long.
       01 high-range           usage binary-long.
       01 range                usage binary-long.


       01 remain               usage binary-long.
       01 unused               usage binary-long.

       procedure division.
       accept show-arg from command-line
       if show-arg less than 1 or greater than coverage then
           move show-default to show-arg
       end-if
       move show-arg to show-sequence

       move 1 to longest(1)
       perform hailstone varying storm
                         from 1 by 1 until storm > coverage
       display "Longest at: " longest(2) " with " longest(1) " elements"
       goback.

      *> **************************************************************
       hailstone.
       move 0 to stones
       move storm to n
       perform until n equal 1
           if stones > most then
               display "too many hailstones" upon syserr
               stop run
           end-if

           add 1 to stones
           move n to hail(stones)
           divide n by 2 giving unused remainder remain
           if remain equal 0 then
               divide 2 into n
           else
               compute n = 3 * n + 1
           end-if
       end-perform
       add 1 to stones
       move n to hail(stones)

       if stones > longest(1) then
           move stones to longest(1)
           move storm to longest(2)
       end-if

       if storm equal show-sequence then
           display show-sequence ": " with no advancing
           perform varying range from 1 by 1 until range > stones
               move 5 to low-range
               compute high-range = stones - 4
               if range < low-range or range > high-range then
                   move hail(range) to show
                   display function trim(show) with no advancing
                   if range < stones then
                       display ", " with no advancing
                   end-if
               end-if
               if range = low-range and stones > 8 then
                   display "..., " with no advancing
               end-if
           end-perform
           display ": " stones " elements"
       end-if
       .

       end program hailstones.
```


```txt

prompt$ cobc -x hailstones.cob
prompt$ ./hailstones
+0000000027: 27, 82, 41, 124, ..., 8, 4, 2, 1: +0000000112 elements
Longest at: +0000077031 with +0000000351 elements
prompt$ ./hailstones 42
+0000000042: 42, 21, 64, 32, ..., 8, 4, 2, 1: +0000000009 elements
Longest at: +0000077031 with +0000000351 elements

```



## CoffeeScript

Recursive version:

```coffeescript
hailstone = (n) ->
  if n is 1
    [n]

  else if n % 2 is 0
    [n].concat hailstone n/2

  else
    [n].concat hailstone (3*n) + 1

h27 = hailstone 27
console.log "hailstone(27) = #{h27[0..3]} ... #{h27[-4..]} (length: #{h27.length})"

maxlength = 0
maxnums = []

for i in [1..100000]
  seq = hailstone i

  if seq.length is maxlength
    maxnums.push i
  else if seq.length > maxlength
    maxlength = seq.length
    maxnums = [i]

console.log "Max length: #{maxlength}; numbers generating sequences of this length: #{maxnums}"
```


```txt
hailstone(27) = 27,82,41,124 ... 8,4,2,1 (length: 112)
Max length: 351; numbers generating sequences of this length: 77031
```



## Common Lisp


```lisp
(defun hailstone (n)
  (cond ((= n 1) '(1))
	((evenp n) (cons n (hailstone (/ n 2))))
	(t (cons n (hailstone (+ (* 3 n) 1))))))

(defun longest (n)
  (let ((k 0) (l 0))
    (loop for i from 1 below n do
	 (let ((len (length (hailstone i))))
	   (when (> len l) (setq l len k i)))
	 finally (format t "Longest hailstone sequence under ~A for ~A, having length ~A." n k l))))
```

Sample session:

```txt
ROSETTA> (length (hailstone 27))
112
ROSETTA> (subseq (hailstone 27) 0 4)
(27 82 41 124)
ROSETTA> (last (hailstone 27) 4)
(8 4 2 1)
ROSETTA> (longest-hailstone 100000)
Longest hailstone sequence under 100000 for 77031, having length 351.
NIL
```



## Crystal


```Ruby

def hailstone(n)
    seq = [n]
    until n == 1
        n = n.even? ? n / 2 : n * 3 + 1
        seq << n
    end
    seq
end

max_len = (1...100_000).max_by{|n| hailstone(n).size }
max = hailstone(max_len)
puts ([max_len, max.size, max.max, max.first(4), max.last(4)])
# => [77031, 351, 21933016, [77031, 231094, 115547, 346642], [8, 4, 2, 1]]

twenty_seven = hailstone(27)
puts ([twenty_seven.size, twenty_seven.first(4), max.last(4)])
# => [112, [27, 82, 41, 124], [8, 4, 2, 1]]

```



## D


### Basic Version


```d
import std.stdio, std.algorithm, std.range, std.typecons;

auto hailstone(uint n) pure nothrow {
  auto result = [n];
  while (n != 1) {
    n = (n & 1) ? (n * 3 + 1) : (n / 2);
    result ~= n;
  }
  return result;
}

void main() {
  enum M = 27;
  immutable h = M.hailstone;
  writeln("hailstone(", M, ")= ", h[0 .. 4], " ... " , h[$ - 4 .. $]);
  writeln("Length hailstone(", M, ")= ", h.length);

  enum N = 100_000;
  immutable p = iota(1, N)
                .map!(i => tuple(i.hailstone.length, i))
                .reduce!max;
  writeln("Longest sequence in [1,", N, "]= ",p[1]," with len ",p[0]);
}
```

```txt
hailstone(27)= [27, 82, 41, 124] ... [8, 4, 2, 1]
Length hailstone(27)= 112
Longest sequence in [1,100000]= 77031 with len 351
```



### Lazy Version

Same output.

```d
import std.stdio, std.algorithm, std.typecons, std.range;

auto hailstone(uint m) pure nothrow @nogc {
    return m
           .recurrence!q{ a[n - 1] & 1 ? a[n - 1] * 3 + 1 : a[n - 1]/2}
           .until!q{ a == 1 }(OpenRight.no);
}

void main() {
  enum M = 27;
  immutable h = M.hailstone.array;
  writeln("hailstone(", M, ")= ", h[0 .. 4], " ... " , h[$ - 4 .. $]);
  writeln("Length hailstone(", M, ")= ", h.length);

  enum N = 100_000;
  immutable p = iota(1, N)
                .map!(i => tuple(i.hailstone.walkLength, i))
                .reduce!max;
  writeln("Longest sequence in [1,", N, "]= ",p[1]," with len ",p[0]);
}
```



### Faster Lazy Version

Same output.

```d
struct Hailstone {
  uint n;
  bool empty() const pure nothrow @nogc { return n == 0; }
  uint front() const pure nothrow @nogc { return n; }
  void popFront() pure nothrow @nogc {
    n = n == 1 ? 0 : (n & 1 ? (n * 3 + 1) : n / 2);
  }
}

void main() {
  import std.stdio, std.algorithm, std.range, std.typecons;

  enum M = 27;
  immutable h = M.Hailstone.array;
  writeln("hailstone(", M, ")= ", h[0 .. 4], " ... " , h[$ - 4 .. $]);
  writeln("Length hailstone(", M, ")= ", h.length);

  enum N = 100_000;
  immutable p = iota(1, N)
                .map!(i => tuple(i.Hailstone.walkLength, i))
                .reduce!max;
  writeln("Longest sequence in [1,", N, "]= ",p[1]," with len ",p[0]);
}
```



### Lazy Version With Caching

Faster, same output.

```d
import std.stdio, std.algorithm, std.range, std.typecons;

struct Hailstone(size_t cacheSize = 500_000) {
  size_t n;
  __gshared static size_t[cacheSize] cache;

  bool empty() const pure nothrow @nogc { return n == 0; }
  size_t front() const pure nothrow @nogc { return n; }

  void popFront() nothrow {
    if (n >= cacheSize) {
      n = n == 1 ? 0 : (n & 1 ? n*3 + 1 : n/2);
    } else if (cache[n]) {
      n = cache[n];
    } else {
      immutable n2 = n == 1 ? 0 : (n & 1 ? n*3 + 1 : n/2);
      n = cache[n] = n2;
    }
  }
}

void main() {
  enum M = 27;
  const h = M.Hailstone!().array;
  writeln("hailstone(", M, ")= ", h[0 .. 4], " ... " , h[$ - 4 .. $]);
  writeln("Length hailstone(", M, ")= ", h.length);

  enum N = 100_000;
  immutable p = iota(1, N)
                .map!(i => tuple(i.Hailstone!().walkLength, i))
                .reduce!max;
  writeln("Longest sequence in [1,", N, "]= ",p[1]," with len ",p[0]);
}
```



### Generator Range Version


```d
import std.stdio, std.algorithm, std.range, std.typecons, std.concurrency;

auto hailstone(size_t n) {
    return new Generator!size_t({
        yield(n);
        while (n > 1) {
            n = (n & 1) ? (3 * n + 1) : (n / 2);
            yield(n);
        }
    });
}

void main() {
  enum M = 27;
  const h = M.hailstone.array;
  writeln("hailstone(", M, ")= ", h[0 .. 4], " ... " , h[$ - 4 .. $]);
  writeln("Length hailstone(", M, ")= ", h.length);

  enum N = 100_000;
  immutable p = iota(1, N)
                .map!(i => tuple(i.hailstone.walkLength, i))
                .reduce!max;
  writeln("Longest sequence in [1,", N, "]= ",p[1]," with len ",p[0]);
}
```


## Dart


```dart>List<int
 hailstone(int n) {
  if(n<=0) {
    throw new IllegalArgumentException("start value must be >=1)");
  }
  Queue<int> seq=new Queue<int>();
  seq.add(n);
  while(n!=1) {
    n=n%2==0?(n/2).toInt():3*n+1;
    seq.add(n);
  }
  return new List<int>.from(seq);
}

// apparently List is missing toString()
String iterableToString(Iterable seq) {
  String str="[";
  Iterator i=seq.iterator();
  while(i.hasNext()) {
    str+=i.next();
    if(i.hasNext()) {
      str+=",";
    }
  }
  return str+"]";
}

main() {
  for(int i=1;i<=10;i++) {
    print("h($i)="+iterableToString(hailstone(i)));
  }
  List<int> h27=hailstone(27);
  List<int> first4=h27.getRange(0,4);
  print("first 4 elements of h(27): "+iterableToString(first4));
  Expect.listEquals([27,82,41,124],first4);

  List<int> last4=h27.getRange(h27.length-4,4);
  print("last 4 elements of h(27): "+iterableToString(last4));
  Expect.listEquals([8,4,2,1],last4);

  print("length of sequence h(27): "+h27.length);
  Expect.equals(112,h27.length);

  int seq,max=0;
  for(int i=1;i<=100000;i++) {
    List<int> h=hailstone(i);
    if(h.length>max) {
      max=h.length;
      seq=i;
    }
  }
  print("up to 100000 the sequence h($seq) has the largest length ($max)");
}
```

```txt
h(1)=[1]
h(2)=[2,1]
h(3)=[3,10,5,16,8,4,2,1]
h(4)=[4,2,1]
h(5)=[5,16,8,4,2,1]
h(6)=[6,3,10,5,16,8,4,2,1]
h(7)=[7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
h(8)=[8,4,2,1]
h(9)=[9,28,14,7,22,11,34,17,52,26,13,40,20,10,5,16,8,4,2,1]
h(10)=[10,5,16,8,4,2,1]
first 4 elements of h(27): [27,82,41,124]
last 4 elements of h(27): [8,4,2,1]
length of sequence h(27): 112
up to 100000 the sequence h(77031) has the largest length (351)
```


## Dc

Firstly, this code takes the value from the stack, computes and prints the corresponding Hailstone sequence, and the length of the sequence.
The q procedure is for counting the length of the sequence.
The e and o procedure is for even and odd number respectively.
The x procedure is for overall control.

```Dc
27
[[--: ]nzpq]sq
[d 2/ p]se
[d 3*1+ p]so
[d2% 0=e d1=q d2% 1=o d1=q lxx]dsxx
```

```txt

82
41
124
62
(omitted)
8
4
2
1
--: 112

```


Then we could wrap the procedure x with a new procedure s, and call it with l which is loops the value of t from 1 to 100000, and cleaning up the stack after each time we finish up with a number.
Register L for the length of the longest sequence and T for the corresponding number.
Also, procedure q is slightly modified for storing L and T if needed, and all printouts in procedure e and o are muted.

```Dc
0dsLsT1st
[dsLltsT]sM
[[zdlL<M q]sq
[d 2/]se
[d 3*1+ ]so
[d2% 0=e d1=q d2% 1=o d1=q lxx]dsxx]ss
[lt1+dstlsxc lt100000>l]dslx
lTn[:]nlLp

```

{{out}} (Takes quite some time on a decent machine)

```txt
77031:351
```


## DCL


```DCL
$ n = f$integer( p1 )
$ i = 1
$ loop:
$  if p2 .nes. "QUIET" then $ s'i = n
$  if n .eq. 1 then $ goto done
$  i = i + 1
$  if .not. n
$  then
$   n = n / 2
$  else
$   if n .gt. 715827882 then $ exit  ! avoid overflowing
$   n = 3 * n + 1
$  endif
$  goto loop
$ done:
$ if p2 .nes. "QUIET"
$ then
$  penultimate_i = i - 1
$  antepenultimate_i = i - 2
$  preantepenultimate_i = i - 3
$  write sys$output "sequence has ", i, " elements starting with ", s1, ", ", s2, ", ", s3, ", ", s4, " and ending with ", s'preantepenultimate_i, ", ", s'antepenultimate_i, ", ", s'penultimate_i, ", ", s'i
$ endif
$ sequence_length == i
```

```txt
$ @hailstone 27
sequence has 112 elements starting with 27, 82, 41, 124 and ending with 8, 4, 2, 1
```


```DCL
$ limit = f$integer( p1 )
$ i = 1
$ max_so_far = 0
$ loop:
$  call hailstone 'i quiet
$  if sequence_length .gt. max_so_far
$  then
$   max_so_far = sequence_length
$   current_record_holder = i
$  endif
$  i = i + 1
$  if i .lt. limit then $ goto loop
$ write sys$output current_record_holder, " is the number less than ", limit, " which has the longest hailstone sequence which is ", max_so_far, " in length"
$ exit
$
$ hailstone: subroutine
$ n = f$integer( p1 )
$ i = 1
$ loop:
$  if p2 .nes. "QUIET" then $ s'i = n
$  if n .eq. 1 then $ goto done
$  i = i + 1
$  if .not. n
$  then
$   n = n / 2
$  else
$   if n .gt. 715827882 then $ exit  ! avoid overflowing
$   n = 3 * n + 1
$  endif
$  goto loop
$ done:
$ if p2 .nes. "QUIET"
$ then
$  penultimate_i = i - 1
$  antepenultimate_i = i - 2
$  preantepenultimate_i = i - 3
$  write sys$output "sequence has ", i, " elements starting with ", s1, ", ", s2, ", ", s3, ", ", s4, " and ending with ", s'preantepenultimate_i, ", ", s'antepenultimate_i, ", ", s'penultimate_i, ", ", s'i
$ endif
$ sequence_length == I
$ exit
$ endsubroutine
```

```txt
$ @longest_hailstone 100000
77031 is the number less than 100000 which has the longest hailstone sequence which is 351 in length
```

=={{header|Déjà Vu}}==

```dejavu
local hailstone:
	swap [ over ]
	while < 1 dup:
		if % over 2:
			#odd
			++ * 3
		else:
			#even
			/ swap 2
		swap push-through rot dup
	drop

if = (name) :(main):
	local :h27 hailstone 27
	!. = 112 len h27
	!. = 27 h27! 0
	!. = 82 h27! 1
	!. = 41 h27! 2
	!. = 124 h27! 3
	!. = 8 h27! 108
	!. = 4 h27! 109
	!. = 2 h27! 110
	!. = 1 h27! 111

	local :max 0
	local :maxlen 0
	for i range 1 99999:
		dup len hailstone i
		if < maxlen:
			set :maxlen
			set :max i
		else:
			drop
	!print( "number: " to-str max ", length: " to-str maxlen )
else:
	@hailstone
```

```txt
true
true
true
true
true
true
true
true
true
number: 77031, length: 351
```


## Delphi


```Delphi
program ShowHailstoneSequence;

{$APPTYPE CONSOLE}

uses SysUtils, Generics.Collections;

procedure GetHailstoneSequence(aStartingNumber: Integer; aHailstoneList: TList<Integer>);
var
  n: Integer;
begin
  aHailstoneList.Clear;
  aHailstoneList.Add(aStartingNumber);
  n := aStartingNumber;

  while n <> 1 do
  begin
    if Odd(n) then
      n := (3 * n) + 1
    else
      n := n div 2;
    aHailstoneList.Add(n);
  end;
end;

var
  i: Integer;
  lList: TList<Integer>;
  lMaxSequence: Integer;
  lMaxLength: Integer;
begin
  lList := TList<Integer>.Create;
  try
    GetHailstoneSequence(27, lList);
    Writeln(Format('27: %d elements', [lList.Count]));
    Writeln(Format('[%d,%d,%d,%d ... %d,%d,%d,%d]',
      [lList[0], lList[1], lList[2], lList[3],
      lList[lList.Count - 4], lList[lList.Count - 3], lList[lList.Count - 2], lList[lList.Count - 1]]));
    Writeln;

    lMaxSequence := 0;
    lMaxLength := 0;
    for i := 1 to 100000 do
    begin
      GetHailstoneSequence(i, lList);
      if lList.Count > lMaxLength then
      begin
        lMaxSequence := i;
        lMaxLength := lList.Count;
      end;
    end;
    Writeln(Format('Longest sequence under 100,000: %d with %d elements', [lMaxSequence, lMaxLength]));
  finally
    lList.Free;
  end;

  Readln;
end.
```

```txt
27: 112 elements
[27 82 41 124 ... 8 4 2 1]

Longest sequence under 100,000: 77031 with 351 elements
```



## EchoLisp


```scheme

(lib 'hash)
(lib 'sequences)
(lib 'compile)

(define (hailstone n)
(when (> n 1)
	(if (even? n) (/ n 2) (1+ (* n 3)))))

(define H (make-hash))

;; (iterator/f seed f) returns seed, (f seed) (f(f seed)) ...

(define (hlength seed)
	(define collatz (iterator/f hailstone seed))
	(or
	   (hash-ref H seed) ;; known ?
	   (hash-set H seed
	      (for ((i (in-naturals)) (h collatz))
              ;; add length of subsequence if already known
	      #:break (hash-ref H h) => (+ i (hash-ref H h))
	      (1+ i)))))

(define (task (nmax 100000))
	(for ((n [1 .. nmax])) (hlength n)) ;; fill hash table

	(define hmaxlength (apply max (hash-values H)))
	(define hmaxseed (hash-get-key H hmaxlength))
	(writeln 'maxlength= hmaxlength 'for hmaxseed))


```

```scheme

(define H27 (iterator/f hailstone 27))
(take H27 6)
   → (27 82 41 124 62 31)
(length H27)
   → 112
(list-tail (take H27 112) -6)
   → (5 16 8 4 2 1)

(task)
maxlength=     351     for     77031

;; more ...
(lib 'bigint)

(task 200000)
    maxlength=     383     for     156159
(task 300000)
    maxlength=     443     for     230631
(task 400000)
    maxlength=     443     for     230631
(task 500000)
    maxlength=     449     for     410011
(task 600000)
    maxlength=     470     for     511935
(task 700000)
    maxlength=     509     for     626331
(task 800000)
    maxlength=     509     for     626331
(task 900000)
    maxlength=     525     for     837799
(task 1000000)
    maxlength=     525     for     837799


```



## Egel


```Egel

import "prelude.eg"

namespace Hailstone (

    using System
    using List

    def even = [ N -> (N%2) == 0 ]

    def hailstone =
        [ 1 -> {1}
        | N -> if even N then cons N (hailstone (N/2))
               else cons N (hailstone (N * 3 + 1)) ]

    def hailpair =
        [ N -> (N, length (hailstone N)) ]

    def hailmax =
        [ (N, NMAX), (M, MMAX) -> if (NMAX < MMAX) then (M, MMAX) else (N, NMAX) ]

    def largest =
        [ 1 -> (1, 1)
        | N ->
            let M0 = hailpair N in
            let M1 = largest (N - 1) in
                hailmax M0 M1 ]
)

using System
using List
using Hailstone

def task0 = let H27 = hailstone 27 in length H27

def task1 =
    let H27 = hailstone 27 in
    let L   = length H27 in
        (take 4 H27, drop (L - 4) H27)

def task2 = largest 100000

def main = (task0, task1, task2)

```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
		local
			test: LINKED_LIST [INTEGER]
			count, number, te: INTEGER
		do
			create test.make
			test := hailstone_sequence (27)
			io.put_string ("There are " + test.count.out + " elements in the sequence for the number 27.")
			io.put_string ("%NThe first 4 elements are: ")
			across
				1 |..| 4 as t
			loop
				io.put_string (test [t.item].out + "%T")
			end
			io.put_string ("%NThe last 4 elements are: ")
			across
				(test.count - 3) |..| test.count as t
			loop
				io.put_string (test [t.item].out + "%T")
			end
			across
				1 |..| 99999 as c
			loop
				test := hailstone_sequence (c.item)
				te := test.count
				if te > count then
					count := te
					number := c.item
				end
			end
			io.put_string ("%NThe longest sequence for numbers below 100000 is " + count.out + " for the number " + number.out + ".")
		end

	hailstone_sequence (n: INTEGER): LINKED_LIST [INTEGER]
			-- Members of the Hailstone Sequence starting from 'n'.
		require
			n_is_positive: n > 0
		local
			seq: INTEGER
		do
			create Result.make
			from
				seq := n
			until
				seq = 1
			loop
				Result.extend (seq)
				if seq \\ 2 = 0 then
					seq := seq // 2
				else
					seq := ((3 * seq) + 1)
				end
			end
			Result.extend (seq)
		ensure
			sequence_terminated: Result.last = 1
		end

end

```

```txt

There are 112 elements in the sequence for the number 27.
The first 4 elements are: 27    82    41    124
The last 4 elements are: 8    4    2    1
The longest sequence for numbers below 100000 is 351 for the number 77031.

```


## Elena

ELENA 4.x :

```elena
import system'collections;
import extensions;

const int maxNumber = 100000;

Hailstone(int n,Map<int,int> lengths)
{
    if (n == 1)
    {
        ^ 1
    };

    while (true)
    {
        if (lengths.containsKey(n))
        {
            ^ lengths[n]
        }
        else
        {
            if (n.isEven())
            {
                lengths[n] := 1 + Hailstone(n/2, lengths)
            }
            else
            {
                lengths[n] := 1 + Hailstone(3*n + 1, lengths)
            }
        }
    }
}

public program()
{
    int longestChain := 0;
    int longestNumber := 0;
    auto recursiveLengths := new Map<int,int>(4096,4096);

    for(int i := 1, i < maxNumber, i+=1)
    {
        var chainLength := Hailstone(i, recursiveLengths);
        if (longestChain < chainLength)
        {
               longestChain := chainLength;
               longestNumber := i
        }
    };

    console.printFormatted("max below {0}: {1} ({2} steps)", maxNumber, longestNumber, longestChain)
}
```

```txt

max bellow 100000: 77031 (351 steps)

```



## Elixir


```elixir
defmodule Hailstone do
  require Integer

  def step(1)                        , do: 0
  def step(n) when Integer.is_even(n), do: div(n,2)
  def step(n)                        , do: n*3 + 1

  def sequence(n) do
    Stream.iterate(n, &step/1) |> Stream.take_while(&(&1 > 0)) |> Enum.to_list
  end

  def run do
    seq27 = sequence(27)
    len27 = length(seq27)
    repr = String.replace(inspect(seq27, limit: 4) <> inspect(Enum.drop(seq27,len27-4)), "][", ", ")
    IO.puts "Hailstone(27) has #{len27} elements: #{repr}"

    {len, start} = Enum.map(1..100_000, fn(n) -> {length(sequence(n)), n} end) |> Enum.max
    IO.puts "Longest sequence starting under 100000 begins with #{start} and has #{len} elements."
  end
end

Hailstone.run
```


```txt

Hailstone(27) has 112 elements: [27, 82, 41, 124, ..., 8, 4, 2, 1]
Longest sequence starting under 100000 begins with 77031 and has 351 elements.

```



## Erlang


```erlang
-module(hailstone).
-import(io).
-export([main/0]).

hailstone(1) -> [1];
hailstone(N) when N band 1 == 1 -> [N|hailstone(N * 3 + 1)];
hailstone(N) when N band 1 == 0 -> [N|hailstone(N div 2)].

max_length(Start, Stop) ->
    F = fun (N) -> {length(hailstone(N)), N} end,
    Lengths = lists:map(F, lists:seq(Start, Stop)),
    lists:max(Lengths).

main() ->
    io:format("hailstone(4): ~w~n", [hailstone(4)]),
    Seq27 = hailstone(27),
    io:format("hailstone(27) length: ~B~n", [length(Seq27)]),
    io:format("hailstone(27) first 4: ~w~n",
              [lists:sublist(Seq27, 4)]),
    io:format("hailstone(27) last 4: ~w~n",
              [lists:nthtail(length(Seq27) - 4, Seq27)]),
    io:format("finding maximum hailstone(N) length for 1 <= N <= 100000..."),
    {Length, N} = max_length(1, 100000),
    io:format(" done.~nhailstone(~B) length: ~B~n", [N, Length]).
```

```txt
Eshell V5.8.4  (abort with ^G)
1> c(hailstone).
{ok,hailstone}
2> hailstone:main().
hailstone(4): [4,2,1]
hailstone(27) length: 112
hailstone(27) first 4: [27,82,41,124]
hailstone(27) last 4: [8,4,2,1]
finding maximum hailstone(N) length for 1 <= N <= 100000... done.
hailstone(77031) length: 351
ok
```



'''Erlang 2'''

This version has one collatz function for just calculating totals (just for fun) and the second generating lists.


```erlang

-module(collatz).
-export([main/0,collatz/1,coll/1,max_atz_under/1]).

collatz(1) -> 1;
collatz(N) when N rem 2 == 0 -> 1 + collatz(N div 2);
collatz(N) when N rem 2 > 0 -> 1 + collatz(3 * N +1).

max_atz_under(N) ->
  F = fun (X) -> {collatz(X), X} end,
  {_, Index} = lists:max(lists:map(F, lists:seq(1, N))),
  Index.

coll(1) -> [1];
coll(N) when N rem 2 == 0 -> [N|coll(N div 2)];
coll(N) -> [N|coll(3 * N + 1)].

main() ->
    io:format("collatz(4) non-list total: ~w~n", [collatz(4)]),
    io:format("coll(4) with lists ~w~n",  [coll(4)] ),
    Seq27 = coll(27),
    Seq1000 = coll(max_atz_under(100000)),
    io:format("coll(27) length: ~B~n", [length(Seq27)]),
    io:format("coll(27) first 4: ~w~n", [lists:sublist(Seq27, 4)]),
    io:format("collatz(27) last 4: ~w~n",
              [lists:nthtail(length(Seq27) - 4, Seq27)]),
    io:format("maximum  N <= 100000..."),
    io:format("Max: ~w~n", [max_atz_under(100000)]),
    io:format("Total: ~w~n", [ length( Seq1000 ) ] ).

```

'''Output'''

```txt

64> collatz:main().
collatz(4) non-list total: 3
coll(4) with lists [4,2,1]
coll(27) length: 112
coll(27) first 4: [27,82,41,124]
collatz(27) last 4: [8,4,2,1]
maximum  N <= 100000...Max: 77031
Total: 351
ok

```



## ERRE

In Italy it's known also as "Ulam conjecture".

```ERRE

PROGRAM ULAM

!$DOUBLE

PROCEDURE HAILSTONE(X,PRT%->COUNT)
   COUNT=1
   IF PRT% THEN PRINT(X,) END IF
   REPEAT
      IF X/2<>INT(X/2) THEN
          X=X*3+1
        ELSE
          X=X/2
      END IF
      IF PRT% THEN PRINT(X,) END IF
      COUNT=COUNT+1
   UNTIL X=1
   IF PRT% THEN PRINT END IF
END PROCEDURE

BEGIN
   HAILSTONE(27,TRUE->COUNT)
   PRINT("Sequence length for 27:";COUNT)
   MAX_COUNT=2
   NMAX=2
   FOR I=3 TO 100000 DO
      HAILSTONE(I,FALSE->COUNT)
      IF COUNT>MAX_COUNT THEN NMAX=I MAX_COUNT=COUNT END IF
   END FOR
   PRINT("Max. number is";NMAX;" with";MAX_COUNT;"elements")
END PROGRAM

```

```txt

        27        82        41       124        62
        31        94        47       142        71
       214       107       322       161       484
       242       121       364       182        91
       274       137       412       206       103
       310       155       466       233       700
       350       175       526       263       790
       395      1186       593      1780       890
       445      1336       668       334       167
       502       251       754       377      1132
       566       283       850       425      1276
       638       319       958       479      1438
       719      2158      1079      3238      1619
      4858      2429      7288      3644      1822
       911      2734      1367      4102      2051
      6154      3077      9232      4616      2308
      1154       577      1732       866       433
      1300       650       325       976       488
       244       122        61       184        92
        46        23        70        35       106
        53       160        80        40        20
        10         5        16         8         4
         2         1

Sequence length for 27: 112
Max. number is 77031 with 351 elements
```



## Euler Math Toolbox



```Euler Math Toolbox

>function hailstone (n) ...
$  v=[n];
$  repeat
$    if mod(n,2) then n=3*n+1;
$    else n=n/2;
$    endif;
$    v=v|n;
$    until n==1;
$  end;
$  return v;
$  endfunction
>hailstone(27), length(%)
 [ 27  82  41  124  62  31  94  47  142  71  214  107  322  161  484  242
 121  364  182  91  274  137  412  206  103  310  155  466  233  700
 350  175  526  263  790  395  1186  593  1780  890  445  1336  668
 334  167  502  251  754  377  1132  566  283  850  425  1276  638  319
 958  479  1438  719  2158  1079  3238  1619  4858  2429  7288  3644
 1822  911  2734  1367  4102  2051  6154  3077  9232  4616  2308  1154
 577  1732  866  433  1300  650  325  976  488  244  122  61  184  92
 46  23  70  35  106  53  160  80  40  20  10  5  16  8  4  2  1 ]
 112
>function hailstonelength (n) ...
$  v=zeros(1,n);
$  v[1]=4; v[2]=2;
$  loop 3 to n;
$    count=1;
$    n=#;
$    repeat
$      if mod(n,2) then n=3*n+1;
$      else n=n/2;
$      endif;
$      if n<=cols(v) and v[n] then
$        v[#]=v[n]+count;
$        break;
$      endif;
$      count=count+1;
$    end;
$  end;
$  return v;
$  endfunction
>h=hailstonelength(100000);
>ex=extrema(h); ex[3], ex[4]
 351
 77031

```



## Euphoria


```euphoria
function hailstone(atom n)
    sequence s
    s = {n}
    while n != 1 do
        if remainder(n,2)=0 then
            n /= 2
        else
            n = 3*n + 1
        end if
        s &= n
    end while
    return s
end function

function hailstone_count(atom n)
    integer count
    count = 1
    while n != 1 do
        if remainder(n,2)=0 then
            n /= 2
        else
            n = 3*n + 1
        end if
        count += 1
    end while
    return count
end function

sequence s
s = hailstone(27)
puts(1,"hailstone(27) =\n")
? s
printf(1,"len = %d\n\n",length(s))

integer max,imax,count
max = 0
for i = 2 to 1e5-1 do
    count = hailstone_count(i)
    if count > max then
        max = count
        imax = i
    end if
end for

printf(1,"The longest hailstone sequence under 100,000 is %d with %d elements.\n",
    {imax,max})
```

```txt
hailstone(27) =
{27,82,41,124,62,31,94,47,142,71,214,107,322,161,484,242,121,364,182,
91,274,137,412,206,103,310,155,466,233,700,350,175,526,263,790,395,
1186,593,1780,890,445,1336,668,334,167,502,251,754,377,1132,566,283,
850,425,1276,638,319,958,479,1438,719,2158,1079,3238,1619,4858,2429,
7288,3644,1822,911,2734,1367,4102,2051,6154,3077,9232,4616,2308,1154,
577,1732,866,433,1300,650,325,976,488,244,122,61,184,92,46,23,70,35,
106,53,160,80,40,20,10,5,16,8,4,2,1}
len = 112

The longest hailstone sequence under 100,000 is 77031 with 351 elements.

```



## Excel

    In cell '''A1''', place the starting number.
    In cell '''A2''' enter this formula '''=IF(MOD(A1,2)=0,A1/2,A1*3+1)'''
    Drag and copy the formula down until 4, 2, 1

## Ezhil

Ezhil is a Tamil programming language, see [http://en.wikipedia.org/wiki/Ezhil_%28programming_language%29 | Wikipedia] entry.

<lang src="Python">
நிரல்பாகம்  hailstone ( எண் )
           பதிப்பி "=> ",எண் #hailstone seq
	    @( எண் == 1 )   ஆனால்
	        பின்கொடு எண்
	    முடி

	      @( (எண்%2) == 1 )  ஆனால்
	      	   hailstone( 3*எண் + 1)
              இல்லை
	           hailstone( எண்/2 )
              முடி
முடி


எண்கள் = [5,17,19,23,37]
@(எண்கள் இல் இவ்வெண்) ஒவ்வொன்றாக
   பதிப்பி "****** calculating hailstone seq for ",இவ்வெண்," *********"
   hailstone( இவ்வெண் )
   பதிப்பி "**********************************************"
முடி

```



## Factor


```factor
! rosetta/hailstone/hailstone.factor
USING: arrays io kernel math math.ranges prettyprint sequences vectors ;
IN: rosetta.hailstone

: hailstone ( n -- seq )
    [ 1vector ] keep
    [ dup 1 number= ]
    [
        dup even? [ 2 / ] [ 3 * 1 + ] if
        2dup swap push
    ] until
    drop ;

<PRIVATE
: main ( -- )
    27 hailstone dup dup
    "The hailstone sequence from 27:" print
    "  has length " write length .
    "  starts with " write 4 head [ unparse ] map ", " join print
    "  ends with " write 4 tail* [ unparse ] map ", " join print

    ! Maps n => { length n }, and reduces to longest Hailstone sequence.
    1 100000 [a,b)
    [ [ hailstone length ] keep 2array ]
    [ [ [ first ] bi@ > ] most ] map-reduce
    first2
    "The hailstone sequence from " write pprint
    " has length " write pprint "." print ;
PRIVATE>

MAIN: main
```

```txt
$ ./factor -run=rosetta.hailstone
Loading resource:work/rosetta/hailstone/hailstone.factor
The hailstone sequence from 27:
  has length 112
  starts with 27, 82, 41, 124
  ends with 8, 4, 2, 1
The hailstone sequence from 77031 has length 351.
```



## FALSE


```false
[$1&$[%3*1+0~]?~[2/]?]n:
[[$." "$1>][n;!]#%]s:
[1\[$1>][\1+\n;!]#%]c:
27s;! 27c;!."
"
0m:0f:
1[$100000\>][$c;!$m;>[m:$f:0]?%1+]#%
f;." has hailstone sequence length "m;.
```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Hailstone_sequence this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Forth


```forth
: hail-next ( n -- n )
  dup 1 and if 3 * 1+ else 2/ then ;
: .hail ( n -- )
  begin dup . dup 1 > while hail-next repeat drop ;
: hail-len ( n -- n )
  1 begin over 1 > while swap hail-next swap 1+ repeat nip ;

27 hail-len . cr
27 .hail cr

: longest-hail ( max -- )
  0 0 rot 1+ 1 do    ( n length )
    i hail-len 2dup < if
      nip nip i swap
    else drop then
  loop
  swap . ." has hailstone sequence length " . ;

100000 longest-hail
```



## Fortran

```fortran
program Hailstone
  implicit none

  integer :: i, maxn
  integer :: maxseqlen = 0, seqlen
  integer, allocatable :: seq(:)

  call hs(27, seqlen)
  allocate(seq(seqlen))
  call hs(27, seqlen, seq)
  write(*,"(a,i0,a)") "Hailstone sequence for 27 has ", seqlen, " elements"
  write(*,"(a,4(i0,a),3(i0,a),i0)") "Sequence = ", seq(1), ", ", seq(2), ", ", seq(3), ", ", seq(4), " ...., ",  &
                                     seq(seqlen-3), ", ", seq(seqlen-2), ", ", seq(seqlen-1), ", ", seq(seqlen)

  do i = 1, 99999
    call hs(i, seqlen)
    if (seqlen > maxseqlen) then
      maxseqlen = seqlen
      maxn = i
    end if
  end do
  write(*,*)
  write(*,"(a,i0,a,i0,a)") "Longest sequence under 100000 is for ", maxn, " with ", maxseqlen, " elements"

  deallocate(seq)

contains

subroutine hs(number, length, seqArray)
  integer, intent(in)  :: number
  integer, intent(out) :: length
  integer, optional, intent(inout) :: seqArray(:)
  integer :: n

  n = number
  length = 1
  if(present(seqArray)) seqArray(1) = n
  do while(n /= 1)
    if(mod(n,2) == 0) then
      n = n / 2
    else
      n = n * 3 + 1
    end if
    length = length + 1
    if(present(seqArray)) seqArray(length) = n
  end do
end subroutine

end program
```

```txt

Hailstone sequence for 27 has 112 elements
Sequence = 27, 82, 41, 124, ...., 8, 4, 2, 1

Longest sequence under 100000 is for 77031 with 351 elements
```



## Frege


```frege
module Hailstone where

import Data.List (maximumBy)

hailstone :: Int -> [Int]
hailstone 1             = [1]
hailstone n | even n    = n : hailstone (n `div` 2)
            | otherwise = n : hailstone (n * 3 + 1)

withResult :: (t -> t1) -> t -> (t1, t)
withResult f x = (f x, x)

main :: IO ()
main = do
 let h27 = hailstone 27
 putStrLn $ show $ length h27
 let h4 = show $ take 4 h27
 let t4 = show $ drop (length h27 - 4) h27
 putStrLn ("hailstone 27: " ++ h4 ++ " ... " ++ t4)
 putStrLn $ show $ maximumBy (comparing fst) $ map (withResult (length . hailstone)) [1..100000]
```


```txt

112
hailstone 27: [27, 82, 41, 124] ... [8, 4, 2, 1]
(351, 77031)
runtime 0.969 wallclock seconds.

```



## Frink


```frink

hailstone[n] :=
{
   results = new array

   while n != 1
   {
      results.push[n]
      if n mod 2 == 0    // n is even?
         n = n / 2
      else
         n = (3n + 1)
   }

   results.push[1]
   return results
}

longestLen = 0
longestN = 0
for n = 1 to 100000
{
   seq = hailstone[n]
   if length[seq] > longestLen
   {
      longestLen = length[seq]
      longestN = n
   }
}

println["$longestN has length $longestLen"]

```


=={{header|F_Sharp|F#}}==

```fsharp
let rec hailstone n = seq {
  match n with
  | 1                -> yield 1
  | n when n % 2 = 0 -> yield n; yield! hailstone (n / 2)
  | n                -> yield n; yield! hailstone (n * 3 + 1)
}

let hailstone27 = hailstone 27 |> Array.ofSeq
assert (Array.length hailstone27 = 112)
assert (hailstone27.[..3] = [|27;82;41;124|])
assert (hailstone27.[108..] = [|8;4;2;1|])

let maxLen, maxI = Seq.max <| seq { for i in 1..99999 -> Seq.length (hailstone i), i}
printfn "Maximum length %d was found for hailstone(%d)" maxLen maxI
```

```txt
Maximum length 351 was found for hailstone(77031)
```



## FunL


```funl
def
  hailstone( 1 ) = [1]
  hailstone( n ) = n # hailstone( if 2|n then n/2 else n*3 + 1 )

if _name_ == '-main-'
  h27 = hailstone( 27 )
  assert( h27.length() == 112 and h27.startsWith([27, 82, 41, 124]) and h27.endsWith([8, 4, 2, 1]) )

  val (n, len) = maxBy( snd, [(i, hailstone( i ).length()) | i <- 1:100000] )

  println( n, len )
```


```txt

77031, 351

```



## Futhark



```Futhark

fun hailstone_step(x: int): int =
  if (x % 2) == 0
  then x/2
  else (3*x) + 1

fun hailstone_seq(x: int): []int =
  let capacity = 100
  let i = 1
  let steps = replicate capacity (-1)
  let steps[0] = x
  loop ((capacity,i,steps,x)) = while x != 1 do
    let (steps, capacity) =
      if i == capacity then
        (concat steps (replicate capacity (-1)),
         capacity * 2)
      else (steps, capacity)
    let x = hailstone_step x
    let steps[i] = x
    in (capacity, i+1, steps, x)
  in #1 (split i steps)

fun hailstone_len(x: int): int =
  let i = 1
  loop ((i,x)) = while x != 1 do
    (i+1, hailstone_step x)
  in i

fun max (x: int) (y: int): int = if x < y then y else x

fun main (x: int) (n: int): ([]int, int) =
  (hailstone_seq x,
   reduce max 0 (map hailstone_len (map (1+) (iota (n-1)))))

```



## GAP


```gap
CollatzSequence := function(n)
  local v;
  v := [ n ];
  while n > 1 do
    if IsEvenInt(n) then
      n := QuoInt(n, 2);
    else
      n := 3*n + 1;
    fi;
    Add(v, n);
  od;
  return v;
end;

CollatzLength := function(n)
  local m;
  m := 1;
  while n > 1 do
    if IsEvenInt(n) then
      n := QuoInt(n, 2);
    else
      n := 3*n + 1;
    fi;
    m := m + 1;
  od;
  return m;
end;

CollatzMax := function(a, b)
  local n, len, nmax, lmax;
  lmax := 0;
  for n in [a .. b] do
    len := CollatzLength(n);
    if len > lmax then
      nmax := n;
      lmax := len;
    fi;
  od;
  return [ nmax, lmax ];
end;

CollatzSequence(27);
# [ 27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206,
#   103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502,
#   251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429,
#   7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300,
#   650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1 ]
CollatzLength(27);
# 112

CollatzMax(1, 100);
# [ 97, 119 ]
CollatzMax(1, 1000);
# [ 871, 179 ]
CollatzMax(1, 10000);
# [ 6171, 262 ]
CollatzMax(1, 100000);
# [ 77031, 351 ]
CollatzMax(1, 1000000);
# [ 837799, 525 ]
```



## Go


```go
package main

import "fmt"

// 1st arg is the number to generate the sequence for.
// 2nd arg is a slice to recycle, to reduce garbage.
func hs(n int, recycle []int) []int {
    s := append(recycle[:0], n)
    for n > 1 {
        if n&1 == 0 {
            n = n / 2
        } else {
            n = 3*n + 1
        }
        s = append(s, n)
    }
    return s
}

func main() {
    seq := hs(27, nil)
    fmt.Printf("hs(27): %d elements: [%d %d %d %d ... %d %d %d %d]\n",
        len(seq), seq[0], seq[1], seq[2], seq[3],
        seq[len(seq)-4], seq[len(seq)-3], seq[len(seq)-2], seq[len(seq)-1])

    var maxN, maxLen int
    for n := 1; n < 100000; n++ {
        seq = hs(n, seq)
        if len(seq) > maxLen {
            maxN = n
            maxLen = len(seq)
        }
    }
    fmt.Printf("hs(%d): %d elements\n", maxN, maxLen)
}
```

```txt

hs(27): 112 elements: [27 82 41 124 ... 8 4 2 1]
hs(77031): 351 elements

```

Alternate solution (inspired both by recent news of a new proof submitted for publication and by recent chat on #rosettacode about generators.)

This solution interprets the wording of the task differently, and takes the word "generate" to mean use a [[generator]].
This has the advantage of not storing the whole sequence in memory at once.
Elements are generated one at a time, counted and discarded.
A time optimization added for task 3 is to store the sequence lengths computed so far.

Output is the same as version above.

```go
package main

import "fmt"

// Task 1 implemented with a generator.  Calling newHg will "create
// a routine to generate the hailstone sequence for a number."
func newHg(n int) func() int {
    return func() (n0 int) {
        n0 = n
        if n&1 == 0 {
            n = n / 2
        } else {
            n = 3*n + 1
        }
        return
    }
}

func main() {
    // make generator for sequence starting at 27
    hg := newHg(27)
    // save first four elements for printing later
    s1, s2, s3, s4 := hg(), hg(), hg(), hg()
    // load next four elements in variables to use as shift register.
    e4, e3, e2, e1 := hg(), hg(), hg(), hg()
    // 4+4= 8 that we've generated so far
    ec := 8
    // until we get to 1, generate another value, shift, and increment.
    // note that intermediate elements--those shifted off--are not saved.
    for e1 > 1 {
        e4, e3, e2, e1 = e3, e2, e1, hg()
        ec++
    }
    // Complete task 2:
    fmt.Printf("hs(27): %d elements: [%d %d %d %d ... %d %d %d %d]\n",
        ec, s1, s2, s3, s4, e4, e3, e2, e1)

    // Task 3:  strategy is to not store sequences, but just the length
    // of each sequence.  as soon as the sequence we're currently working on
    // dips into the range that we've already computed, we short-circuit
    // to the end by adding the that known length to whatever length
    // we've accumulated so far.

    var nMaxLen int // variable holds n with max length encounted so far
    // slice holds sequence length for each n as it is computed
    var computedLen [1e5]int
    computedLen[1] = 1
    for n := 2; n < 1e5; n++ {
        var ele, lSum int
        for hg := newHg(n); ; lSum++ {
            ele = hg()
            // as soon as we get an element in the range we have already
            // computed, we're done...
            if ele < n {
                break
            }
        }
        // just add the sequence length already computed from this point.
        lSum += computedLen[ele]
        // save the sequence length for this n
        computedLen[n] = lSum
        // and note if it's the maximum so far
        if lSum > computedLen[nMaxLen] {
            nMaxLen = n
        }
    }
    fmt.Printf("hs(%d): %d elements\n", nMaxLen, computedLen[nMaxLen])
}
```



## Groovy


```groovy
def hailstone = { long start ->
    def sequence = []
    while (start != 1) {
        sequence << start
        start = (start % 2l == 0l) ? start / 2l : 3l * start + 1l
    }
    sequence << start
}
```

Test Code

```groovy
def sequence = hailstone(27)
assert sequence.size() == 112
assert sequence[0..3] == [27, 82, 41, 124]
assert sequence[-4..-1] == [8, 4, 2, 1]

def results = (1..100000).collect { [n:it, size:hailstone(it).size()] }.max { it.size }
println results
```

```txt
[n:77031, size:351]
```



## Haskell


```haskell
import Data.List (maximumBy)
import Data.Ord (comparing)

collatz :: Int -> Int
collatz n
  | even n = n `div` 2
  | otherwise = 3 * n + 1

hailstone :: Int -> [Int]
hailstone = takeWhile (/= 1) . iterate collatz

longestChain :: Int
longestChain =
  fst $
  maximumBy (comparing snd) $ (,) <*> (length . hailstone) <$> [1 .. 100000]

--TEST -------------------------------------------------------------------------
main :: IO ()
main =
  mapM_
    putStrLn
    [ "Collatz sequence for 27: "
    , (show . hailstone) 27
    , "The number " ++ show longestChain
    , "has the longest hailstone sequence for any number less then 100000. "
    , "The sequence has length: " ++ (show . length . hailstone $ longestChain)
    ]
```

```txt
Collatz sequence for 27:
[27,82,41,124,62,31,94,47,142,71,214,107,322,161,484,242,121,364,182,91,274,137,412,206,103,310,155,466,233,700,350,175,526,263,790,395,1186,593,1780,890,445,1336,668,334,167,502,251,754,377,1132,566,283,850,425,1276,638,319,958,479,1438,719,2158,1079,3238,1619,4858,2429,7288,3644,1822,911,2734,1367,4102,2051,6154,3077,9232,4616,2308,1154,577,1732,866,433,1300,650,325,976,488,244,122,61,184,92,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2]
The number 77031
has the longest hailstone sequence for any number less then 100000.
The sequence has length: 350
```


The following is an older version, which some of the language examples on this page are translated from:

```haskell
import Data.Ord (comparing)
import Data.List (maximumBy, intercalate)

hailstone :: Int -> [Int]
hailstone 1 = [1]
hailstone n
  | even n = n : hailstone (n `div` 2)
  | otherwise = n : hailstone (n * 3 + 1)

withResult :: (Int -> Int) -> Int -> (Int, Int)
withResult f x = (f x, x)

h27 :: [Int]
h27 = hailstone 27

main :: IO ()
main =
  mapM_
    putStrLn
    [ (show . length) h27
    , "hailstone 27: " ++
      intercalate " ... " (show <$> [take 4 h27, drop (length h27 - 4) h27])
    , show $
      maximumBy (comparing fst) $
      withResult (length . hailstone) <$> [1 .. 100000]
    ]
```

```txt
112
hailstone 27: [27,82,41,124] ... [8,4,2,1]
(351,77031)
```


Or, going back to basics, we can observe that the hailstone sequence is an 'anamorphism' – it builds up a list structure from a single integer value, which makes '''unfoldr''' the obvious first thing to reach for the first main task.

In turn, deriving the longest sequence for starting values below 100000 essentially involves a 'catamorphism' – it takes a list of hailstone sequences (or at least a list of their seed values and their lengths), and strips that structure down to a single (N, length) pair. This makes '''foldr''' the obvious recursion scheme to start with for the second main task.

One approach to using '''unfoldr''' and then '''foldr''' might be:


```haskell
import Data.List (unfoldr)

hailStones :: Int -> [Int]
hailStones =
  (++ [1]) .
  unfoldr
    (\x ->
        if x < 2
          then Nothing
          else Just
                 ( x
                 , if even x
                     then div x 2
                     else (3 * x) + 1))

mostStones :: Int -> (Int, Int)
mostStones n =
  foldr
    (\x (m, ml) ->
        let l = length (hailStones x)
        in if l > ml
             then (x, l)
             else (m, ml))
    (0, 0)
    [1 .. n]

-- GENERIC  -------------------------------------------------------------------
lastN_ :: Int -> [Int] -> [Int]
lastN_ = (foldr (const (drop 1)) <*>) . drop

-- TEST -----------------------------------------------------------------------
h27, start27, end27 :: [Int]
[h27, start27, end27] = [id, take 4, lastN_ 4] <*> [hailStones 27]

maxNum, maxLen :: Int
(maxNum, maxLen) = mostStones 100000

main :: IO ()
main =
  mapM_
    putStrLn
    [ "Sequence 27 length:"
    , show $ length h27
    , "Sequence 27 start:"
    , show start27
    , "Sequence 27 end:"
    , show end27
    , ""
    , "N with longest sequence where N <= 100000"
    , show maxNum
    , "length of this sequence:"
    , show maxLen
    ]
```

```txt
Sequence 27 length:
112
Sequence 27 start:
[27,82,41,124]
Sequence 27 end:
[8,4,2,1]

N with longest sequence where N <= 100000
77031
length of this sequence:
351
```



## HicEst


```HicEst
DIMENSION stones(1000)

H27 = hailstone(27)
ALIAS(stones,1, first4,4)
ALIAS(stones,H27-3,  last4,4)
WRITE(ClipBoard, Name) H27, first4, "...", last4

longest_sequence = 0
DO try = 1, 1E5
  elements = hailstone(try)
  IF(elements >= longest_sequence) THEN
      number = try
      longest_sequence = elements
      WRITE(StatusBar, Name) number, longest_sequence
  ENDIF
ENDDO
WRITE(ClipBoard, Name) number, longest_sequence
END

FUNCTION hailstone( n )
   USE : stones

   stones(1) = n
   DO i = 1, LEN(stones)
     IF(stones(i) == 1) THEN
         hailstone = i
         RETURN
     ELSEIF( MOD(stones(i),2) ) THEN
       stones(i+1) = 3*stones(i) + 1
     ELSE
       stones(i+1) = stones(i) / 2
     ENDIF
   ENDDO
END
```

H27=112; first4(1)=27; first4(2)=82; first4(3)=41; first4(4)=124; ...; last4(1)=8; last4(2)=4; last4(3)=2; last4(4)=1;
<br /> number=77031; longest_sequence=351;

=={{header|Icon}} and {{header|Unicon}}==
A simple solution that <i>generates</i> (in the Icon sense) the sequence is:

```icon
procedure hailstone(n)
    while n > 1 do {
        suspend n
        n := if n%2 = 0 then n/2 else 3*n+1
        }
    suspend 1
end
```

and a test program for this solution is:

```icon
procedure main(args)
    n := integer(!args) | 27
    every writes(" ",hailstone(n))
end
```

but this solution is computationally expensive when run repeatedly (task 3).

The following solution uses caching to improve performance on task 3 at the expense of space.

```icon
procedure hailstone(n)
    static cache
    initial {
        cache := table()
        cache[1] := [1]
        }
    /cache[n] := [n] ||| hailstone(if n%2 = 0 then n/2 else 3*n+1)
    return cache[n]
end
```


A test program is:

```icon
procedure main(args)
    n := integer(!args) | 27
    task2(n)
    write()
    task3()
end

procedure task2(n)
    count := 0
    every writes(" ",right(!(sequence := hailstone(n)),5)) do
        if (count +:= 1) % 15 = 0 then write()
    write()
    write(*sequence," value",(*sequence=1,"")|"s"," in the sequence.")
end

procedure task3()
    maxHS := 0
    every n := 1 to 100000 do {
        count := *hailstone(n)
        if maxHS <:= count then maxN := n
        }
    write(maxN," has a sequence of ",maxHS," values")
end
```

A sample run is:

```txt

->hs
    27    82    41   124    62    31    94    47   142    71   214   107   322   161   484
   242   121   364   182    91   274   137   412   206   103   310   155   466   233   700
   350   175   526   263   790   395  1186   593  1780   890   445  1336   668   334   167
   502   251   754   377  1132   566   283   850   425  1276   638   319   958   479  1438
   719  2158  1079  3238  1619  4858  2429  7288  3644  1822   911  2734  1367  4102  2051
  6154  3077  9232  4616  2308  1154   577  1732   866   433  1300   650   325   976   488
   244   122    61   184    92    46    23    70    35   106    53   160    80    40    20
    10     5    16     8     4     2     1
112 values in the sequence.

77031 has a sequence of 351 values
->

```



## Io

Here is a simple, brute-force approach:

```io

makeItHail := method(n,
  stones := list(n)
  while (n != 1,
    if(n isEven,
      n = n / 2,
      n = 3 * n + 1
    )
    stones append(n)
  )
  stones
)

out := makeItHail(27)
writeln("For the sequence beginning at 27, the number of elements generated is ", out size, ".")
write("The first four elements generated are ")
for(i, 0, 3,
  write(out at(i), " ")
)
writeln(".")

write("The last four elements generated are ")
for(i, out size - 4, out size - 1,
  write(out at(i), " ")
)
writeln(".")

numOfElems := 0
nn := 3
for(x, 3, 100000,
  out = makeItHail(x)
  if(out size > numOfElems,
    numOfElems = out size
    nn = x
  )
)

writeln("For numbers less than or equal to 100,000, ", nn,
" has the longest sequence of ", numOfElems, " elements.")

```


```txt

For the sequence beginning at 27, the number of elements generated is 112.
The first four elements generated are 27 82 41 124 .
The last four elements generated are 8 4 2 1 .
For numbers less than or equal to 100,000, 77031 has the longest sequence of 351 elements.

```



## Ioke

```ioke
collatz = method(n,
  n println
  unless(n <= 1,
    if(n even?, collatz(n / 2), collatz(n * 3 + 1)))
)
```



## Inform 7

This solution uses a cache to speed up the length calculation for larger numbers.
```inform7
Home is a room.

To decide which list of numbers is the hailstone sequence for (N - number):
	let result be a list of numbers;
	add N to result;
	while N is not 1:
		if N is even, let N be N / 2;
		otherwise let N be (3 * N) + 1;
		add N to result;
	decide on result.

Hailstone length cache relates various numbers to one number.

To decide which number is the hailstone sequence length for (N - number):
	let ON be N;
	let length so far be 0;
	while N is not 1:
		if N relates to a number by the hailstone length cache relation:
			let result be length so far plus the number to which N relates by the hailstone length cache relation;
			now the hailstone length cache relation relates ON to result;
			decide on result;
		if N is even, let N be N / 2;
		otherwise let N be (3 * N) + 1;
		increment length so far;
	let result be length so far plus 1;
	now the hailstone length cache relation relates ON to result;
	decide on result.

To say first and last (N - number) entry/entries in (L - list of values of kind K):
	let length be the number of entries in L;
	if length <= N * 2:
		say L;
	else:
		repeat with M running from 1 to N:
			if M > 1, say ", ";
			say entry M in L;
		say " ... ";
		repeat with M running from length - N + 1 to length:
			say entry M in L;
			if M < length, say ", ".

When play begins:
	let H27 be the hailstone sequence for 27;
	say "Hailstone sequence for 27 has [number of entries in H27] element[s]: [first and last 4 entries in H27].";
	let best length be 0;
	let best number be 0;
	repeat with N running from 1 to 99999:
		let L be the hailstone sequence length for N;
		if L > best length:
			let best length be L;
			let best number be N;
	say "The number under 100,000 with the longest hailstone sequence is [best number] with [best length] element[s].";
	end the story.
```


```txt
Hailstone sequence for 27 has 112 elements: 27, 82, 41, 124 ... 8, 4, 2, 1.
The number under 100,000 with the longest hailstone sequence is 77031 with 351 elements.
```



## J

'''Solution:'''

```j
hailseq=: -:`(1 3&p.)@.(2&|) ^:(1 ~: ]) ^:a:"0
```

'''Usage:'''

```j
   # hailseq 27                 NB. sequence length
112
   4 _4 {."0 1 hailseq 27       NB. first & last 4 numbers in sequence
27 82 41 124
 8  4  2   1
   (>:@(i. >./) , >./) #@hailseq }.i. 1e5  NB. number < 100000 with max seq length & its seq length
77031 351
```

See also the [[j:Essays/Collatz Conjecture|Collatz Conjecture essay on the J wiki]].


## Java

```java5
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

class Hailstone {

  public static List<Long> getHailstoneSequence(long n) {
    if (n <= 0)
      throw new IllegalArgumentException("Invalid starting sequence number");
    List<Long> list = new ArrayList<Long>();
    list.add(Long.valueOf(n));
    while (n != 1) {
      if ((n & 1) == 0)
        n = n / 2;
      else
        n = 3 * n + 1;
      list.add(Long.valueOf(n));
    }
    return list;
  }

  public static void main(String[] args) {
    List<Long> sequence27 = getHailstoneSequence(27);
    System.out.println("Sequence for 27 has " + sequence27.size() + " elements: " + sequence27);

    long MAX = 100000;
    // Simple way
    {
      long highestNumber = 1;
      int highestCount = 1;
      for (long i = 2; i < MAX; i++) {
        int count = getHailstoneSequence(i).size();
        if (count > highestCount) {
          highestCount = count;
          highestNumber = i;
        }
      }
      System.out.println("Method 1, number " + highestNumber + " has the longest sequence, with a length of " + highestCount);
    }

    // More memory efficient way
    {
      long highestNumber = 1;
      int highestCount = 1;
      for (long i = 2; i < MAX; i++) {
        int count = 1;
        long n = i;
        while (n != 1) {
          if ((n & 1) == 0)
            n = n / 2;
          else
            n = 3 * n + 1;
          count++;
        }
        if (count > highestCount) {
          highestCount = count;
          highestNumber = i;
        }
      }
      System.out.println("Method 2, number " + highestNumber + " has the longest sequence, with a length of " + highestCount);
    }

    // Efficient for analyzing all sequences
    {
      long highestNumber = 1;
      long highestCount = 1;
      Map<Long, Integer> sequenceMap = new HashMap<Long, Integer>();
      sequenceMap.put(Long.valueOf(1), Integer.valueOf(1));

      List<Long> currentList = new ArrayList<Long>();
      for (long i = 2; i < MAX; i++) {
        currentList.clear();
        Long n = Long.valueOf(i);
        Integer count = null;
        while ((count = sequenceMap.get(n)) == null) {
          currentList.add(n);
          long nValue = n.longValue();
          if ((nValue & 1) == 0)
            n = Long.valueOf(nValue / 2);
          else
            n = Long.valueOf(3 * nValue + 1);
        }
        int curCount = count.intValue();
        for (int j = currentList.size() - 1; j >= 0; j--)
          sequenceMap.put(currentList.get(j), Integer.valueOf(++curCount));
        if (curCount > highestCount) {
          highestCount = curCount;
          highestNumber = i;
        }
      }
      System.out.println("Method 3, number " + highestNumber + " has the longest sequence, with a length of " + highestCount);
    }
    return;
  }
}
```

```txt
Sequence for 27 has 112 elements: [27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1]
Method 1, number 77031 has the longest sequence, with a length of 351
Method 2, number 77031 has the longest sequence, with a length of 351
Method 3, number 77031 has the longest sequence, with a length of 351

```



## JavaScript


### ES5


### =Imperative=


```javascript
function hailstone (n) {
    var seq = [n];
    while (n > 1) {
        n = n % 2 ? 3 * n + 1 : n / 2;
        seq.push(n);
    }
    return seq;
}

// task 2: verify the sequence for n = 27
var h = hailstone(27), hLen = h.length;
print("sequence 27 is (" + h.slice(0, 4).join(", ") + " ... "
    + h.slice(hLen - 4, hLen).join(", ") + "). length: " + hLen);

// task 3: find the longest sequence for n < 100000
for (var n, max = 0, i = 100000; --i;) {
    var seq = hailstone(i), sLen = seq.length;
    if (sLen > max) {
        n = i;
        max = sLen;
    }
}
print("longest sequence: " + max + " numbers for starting point " + n);
```

```txt
sequence 27 is (27, 82, 41, 124 ... 8, 4, 2, 1). length: 112
longest sequence: 351 numbers for starting point 77031
```



### =Functional=


This simple problem turns out to be a good test of the constraints on composing
(ES5) JavaScript code in a functional style.

The first sub-problem falls easily within reach of a basic recursive definition
(translating one of the Haskell solutions).


```JavaScript
(function () {

  // Hailstone Sequence
  // n -> [n]
  function hailstone(n) {
    return n === 1 ? [1] : (
      [n].concat(
        hailstone(n % 2 ? n * 3 + 1 : n / 2)
      )
    )
  }

  var lstCollatz27 = hailstone(27);

  return {
    length: lstCollatz27.length,
    sequence: lstCollatz27
  };

})();
```


```JavaScript
{"length":112,"sequence":[27,82,41,124,62,31,94,47,142,71,214,
107,322,161,484,242,121,364,182,91,274,137,412,206,103,310,155,466,233,700,350,
175,526, 263,790,395,1186,593,1780,890,445,1336,668,334,167,502,251,754,377,
1132,566,283,850,425,1276,638,319,958,479,1438,719,2158,1079,3238,1619,4858,
2429,7288,3644,1822,911,2734,1367,4102,2051,6154,3077,9232,4616,2308,1154,577,
1732,866,433,1300,650,325,976,488,244,122,61,184,92,46,23,70,35,106,53,160,80,
40,20,10,5,16,8,4,2,1]}
```


Attempting to fold that recursive function over an array of 100,000 elements,
however, (to solve the second part of the problem) soon runs out of stack
space, at least on the system used here.

The stack problem can be quickly fixed, as often, by simply applying a memoized function,
which reuses previously calculated paths.


```JavaScript
(function () {

  function memoizedHailstone() {
    var dctMemo = {};

    return function hailstone(n) {
      var value = dctMemo[n];

      if (typeof value === "undefined") {
        dctMemo[n] = value = (n === 1) ?
          [1] : ([n].concat(hailstone(n % 2 ? n * 3 + 1 : n / 2)));
      }
      return value;
    }
  }

  // Derived a memoized version of the function,
  // which can reuse previously calculated paths
  var fnCollatz = memoizedHailstone();

  // Iterative version of range
  // [m..n]
  function range(m, n) {
    var a = Array(n - m + 1),
      i = n + 1;
    while (i--) a[i - 1] = i;
    return a;
  }

  // Fold/reduce over an array to find the maximum length
  function longestBelow(n) {
    return range(1, n).reduce(
      function (a, x, i) {
        var lng = fnCollatz(x).length;

        return lng > a.l ? {
          n: i + 1,
          l: lng
        } : a

      }, {
        n: 0,
        l: 0
      }
    )
  }

  return longestBelow(100000);

})();
```


```JavaScript
// Number, length of sequence
{"n":77031, "l":351}
```


For better time (as well as space) we can continue to memoize while falling back to a function which returns the
sequence length alone, and is iteratively implemented. This also proves more scaleable,
and we can still use a fold/reduce pattern over a list to find the longest collatz sequences
for integers below one million, or ten million and beyond, without hitting the limits of system resources.


```JavaScript
(function (n) {

  var dctMemo = {};

  // Length only of hailstone sequence
  // n -> n
  function collatzLength(n) {
    var i = 1,
      a = n,
      lng;

    while (a !== 1) {
      lng = dctMemo[a];
      if ('u' === (typeof lng)[0]) {
        a = (a % 2 ? 3 * a + 1 : a / 2);
        i++;
      } else return lng + i - 1;
    }
    return i;
  }

  // Iterative version of range
  // [m..n]
  function range(m, n) {
    var a = Array(n - m + 1),
      i = n + 1;
    while (i--) a[i - 1] = i;
    return a;
  }

  // Fold/reduce over an array to find the maximum length
  function longestBelow(n) {

    return range(1, n).reduce(
      function (a, x) {

        var lng = dctMemo[x] || (dctMemo[x] = collatzLength(x));

        return lng > a.l ? {
          n: x,
          l: lng
        } : a

      }, {
        n: 0,
        l: 0
      }
    )
  }

  return [100000, 1000000, 10000000].map(longestBelow);

})();
```


```JavaScript
[
  {"n":77031, "l":351},   // 100,000
  {"n":837799, "l":525},  // 1,000,000
  {"n":8400511, "l":686}  // 10,000,000
]
```



```JavaScript
longestBelow(100000000)
-> {"n":63728127, "l":950}
```



### ES6


```javascript
(() => {

    // hailstones :: Int -> [Int]
    const hailstones = x => {
        const collatz = memoized(n =>
            even(n) ? div(n, 2) : (3 * n) + 1);
        return reverse(until(
            xs => xs[0] === 1,
            xs => cons(collatz(xs[0]), xs), [x]
        ));
    };

    // collatzLength :: Int -> Int
    const collatzLength = n =>
        until(
            xi => xi[0] === 1,
            ([x, i]) => [(x % 2 ? 3 * x + 1 : x / 2), i + 1], //
            [n, 1]
        )[1];

    // GENERIC FUNCTIONS -----------------------------------------------------

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
        };

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) => [x].concat(xs);

    // div :: Int -> Int -> Int
    const div = (x, y) => Math.floor(x / y);

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: Math.floor(n - m) + 1
        }, (_, i) => m + i);

    // even :: Int -> Bool
    const even = n => n % 2 === 0;

    // fst :: (a, b) -> a
    const fst = pair => pair.length === 2 ? pair[0] : undefined;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // maximumBy :: (a -> a -> Ordering) -> [a] -> a
    const maximumBy = (f, xs) =>
        xs.length > 0 ? (
            xs.slice(1)
            .reduce((a, x) => f(x, a) > 0 ? x : a, xs[0])
        ) : undefined;

    // memoized :: (a -> b) -> (a -> b)
    const memoized = f => {
        const dctMemo = {};
        return x => {
            const v = dctMemo[x];
            return v !== undefined ? v : (dctMemo[x] = f(x));
        };
    };

    // reverse :: [a] -> [a]
    const reverse = xs =>
        xs.slice(0)
        .reverse();

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // MAIN ------------------------------------------------------------------
    const
        // ceiling :: Int
        ceiling = 100000,

        // (maxLen, maxNum) :: (Int, Int)
        [maxLen, maxNum] =
        maximumBy(
            comparing(fst),
            map(i => [collatzLength(i), i], enumFromTo(1, ceiling))
        );
    return unlines([
        'Collatz sequence for 27: ',
        `${hailstones(27)}`,
        '',
        `The number ${maxNum} has the longest hailstone sequence`,
        `for any starting number under ${ceiling}.`,
        '',
        `The length of that sequence is ${maxLen}.`
    ]);
})();
```

(Run in the Atom editor, through the Script package)

```txt
Collatz sequence for 27:
27,82,41,124,62,31,94,47,142,71,214,107,322,161,484,242,121,364,182,91,
274,137,412,206,103,310,155,466,233,700,350,175,526,263,790,395,1186,593,
1780,890,445,1336,668,334,167,502,251,754,377,1132,566,283,850,425,1276,
638,319,958,479,1438,719,2158,1079,3238,1619,4858,2429,7288,3644,1822,
911,2734,1367,4102,2051,6154,3077,9232,4616,2308,1154,577,1732,866,433,
1300,650,325,976,488,244,122,61,184,92,46,23,70,35,106,53,160,80,40,20,
10,5,16,8,4,2,1

The number 77031 has the longest hailstone sequence
for any starting number under 100000.

The length of that sequence is 351.

[Finished in 1.139s]

```



## jq

```jq
# Generate the hailstone sequence as a stream to save space (and time) when counting
def hailstone:
  recurse( if . > 1 then
              if . % 2 == 0 then ./2|floor else 3*. + 1 end
           else empty
           end );

def count(g): reduce g as $i (0; .+1);

# return [i, length] for the first maximal-length hailstone sequence where i is in [1 .. n]
def max_hailstone(n):
  # state: [i, length]
  reduce range(1; n+1) as $i
    ([0,0];
     ($i | count(hailstone)) as $l
     | if $l > .[1] then [$i, $l] else . end);
```

'''Examples''':

```jq
[27|hailstone] as $h
| "[27|hailstone]|length is \($h|length)",
  "The first four numbers: \($h[0:4])",
  "The last four numbers:  \($h|.[length-4:length])",
  "",
  max_hailstone(100000) as $m
  | "Maximum length for n|hailstone for n in 1..100000 is \($m[1]) (n == \($m[0]))"
```

```sh
$ jq -M -r -n -f hailstone.jq
[27|hailstone]|length is 112
The first four numbers: [27,82,41,124]
The last four numbers:  [8,4,2,1]

Maximum length for n|hailstone for n in 1..100000 is 351 (n == 77031)
```



## Julia

### Dynamic solution


```julia
function hailstonelength(n::Integer)
    len = 1
    while n > 1
        n = ifelse(iseven(n), n ÷ 2, 3n + 1)
        len += 1
    end
    return len
end

@show hailstonelength(27); nothing
@show findmax([hailstonelength(i) for i in 1:100_000]); nothing
```


```txt

hailstonelength(27) = 112
findmax((hailstonelength(i) for i = 1:100000)) = (351, 77031)

```



### Solution with iterator


### =Julia 1.0=

```julia
struct HailstoneSeq{T<:Integer}
    count::T
end

Base.eltype(::HailstoneSeq{T}) where T = T

function Base.iterate(h::HailstoneSeq, state=h.count)
    if state == 1
        (1, 0)
    elseif state < 1
        nothing
    elseif iseven(state)
        (state, state ÷ 2)
    elseif isodd(state)
        (state, 3state + 1)
    end
end

function Base.length(h::HailstoneSeq)
    len = 0
    for _ in h
        len += 1
    end
    return len
end

function Base.show(io::IO, h::HailstoneSeq)
    f5 = collect(Iterators.take(h, 5))
    print(io, "HailstoneSeq{", join(f5, ", "), "...}")
end

hs = HailstoneSeq(27)
println("Collection of the Hailstone sequence from 27: $hs")
cl = collect(hs)
println("First 5 elements: ", join(cl[1:5], ", "))
println("Last 5 elements: ", join(cl[end-4:end], ", "))

Base.isless(h::HailstoneSeq, s::HailstoneSeq) = length(h) < length(s)
println("The number with the longest sequence under 100,000 is: ", maximum(HailstoneSeq.(1:100_000)))
```


```txt
Collection of the Hailstone sequence from 27: HailstoneSeq{27, 82, 411, 124, 62...}
First 5 elements: 27, 82, 41, 124, 62
Last 5 elements: 16, 8, 4, 2, 1
The number with the longest sequence under 100,000 is: HailstoneSeq{777031, 231094, 115547, 346642, 173321...}
```



### =Julia 0.6=

```julia
struct HailstoneSeq{T<:Integer}
	start::T
end

Base.eltype(::HailstoneSeq{T}) where T = T

Base.start(hs::HailstoneSeq) = (-1, hs.start)
Base.done(::HailstoneSeq, state) = state == (1, 4)
function Base.next(::HailstoneSeq, state)
	_, s2 = state
	s1 = s2
	if iseven(s2)
		s2 = s2 ÷ 2
	else
		s2 = 3s2 + 1
	end
	return s1, (s1, s2)
end

function Base.length(hs::HailstoneSeq)
	r = 0
	for _ in hs
		r += 1
	end
	return r
end

function Base.show(io::IO, hs::HailstoneSeq)
	f5 = collect(Iterators.take(hs, 5))
	print(io, "HailstoneSeq(", join(f5, ", "), "...)")
end

hs = HailstoneSeq(27)
println("Collection of the Hailstone sequence from 27: $hs")
cl = collect(hs)
println("First 5 elements: ", join(cl[1:5], ", "))
println("Last 5 elements: ", join(cl[end-4:end], ", "))

Base.isless(h::HailstoneSeq, s::HailstoneSeq) = length(h) < length(s)
println("The number with the longest sequence under 100,000 is: ", maximum(HailstoneSeq.(1:100_000)))
```


```txt
Collection of the Hailstone sequence from 27: HailstoneSeq(27, 82, 41, 124, 62...)
First 5 elements: 27, 82, 41, 124, 62
Last 5 elements: 16, 8, 4, 2, 1
The number with the longest sequence under 100,000 is: HailstoneSeq(77031, 231094, 115547, 346642, 173321...)
```



## K


```k
  hail: (1<){:[x!2;1+3*x;_ x%2]}\
  seqn: hail 27

  #seqn
112
  4#seqn
27 82 41 124
  -4#seqn
8 4 2 1

  {m,x@s?m:|/s:{#hail x}'x}{x@&x!2}!:1e5
351 77031
```



## Kotlin


```kotlin
import java.util.ArrayDeque

fun hailstone(n: Int): ArrayDeque<Int> {
    val hails = when {
        n == 1 -> ArrayDeque<Int>()
        n % 2 == 0 -> hailstone(n / 2)
        else -> hailstone(3 * n + 1)
    }
    hails.addFirst(n)
    return hails
}

fun main(args: Array<String>) {
    val hail27 = hailstone(27)
    fun showSeq(s: List<Int>) = s.map { it.toString() }.reduce { a, b -> a + ", " + b }
    println("Hailstone sequence for 27 is " + showSeq(hail27.take(3)) + " ... "
            + showSeq(hail27.drop(hail27.size - 3)) + " with length ${hail27.size}.")

    var longestHail = hailstone(1)
    for (x in 1..99999)
        longestHail = arrayOf(hailstone(x), longestHail).maxBy { it.size } ?: longestHail
    println("${longestHail.first} is the number less than 100000 with " +
            "the longest sequence, having length ${longestHail.size}.")
}
```


```txt
Hailstone sequence for 27 is 27, 82, 41 ... 4, 2, 1 with length 112.
77031 is the number less than 100000 with the longest sequence, having length 351.
```



## Lasso


```Lasso
[
	define_tag("hailstone", -required="n", -type="integer", -copy);
		local("sequence") = array(#n);
		while(#n != 1);
			((#n % 2) == 0) ? #n = (#n / 2) | #n = (#n * 3 + 1);
			#sequence->insert(#n);
		/while;
		return(#sequence);
	/define_tag;

	local("result");
	#result = hailstone(27);
	while(#result->size > 8);
		#result->remove(5);
	/while;
	#result->insert("...",5);

	"Hailstone sequence for n = 27 -> { " + #result->join(", ") + " }";

	local("longest_sequence") = 0;
	local("longest_index") = 0;
	loop(-from=1, -to=100000);
		local("length") = hailstone(loop_count)->size;
		if(#length > #longest_sequence);
			#longest_index = loop_count;
			#longest_sequence = #length;
		/if;
	/loop;

	"<br/>";
	"Number with the longest sequence under 100,000: " #longest_index + ", with " + #longest_sequence + " elements.";
]
```



## Logo


```logo
to hail.next :n
  output ifelse equal? 0 modulo :n 2 [:n/2] [3*:n + 1]
end

to hail.seq :n
  if :n = 1 [output [1]]
  output fput :n hail.seq hail.next :n
end

show hail.seq 27
show count hail.seq 27

to max.hail :n
  localmake "max.n 0
  localmake "max.length 0
  repeat :n [if greater? count hail.seq repcount  :max.length [
    make "max.n repcount
    make "max.length count hail.seq repcount
  ] ]
  (print :max.n [has hailstone sequence length] :max.length)
end

max.hail 100000
```



## Limbo


<lang>implement Hailstone;

include "sys.m"; sys: Sys;
include "draw.m";

Hailstone: module {
	init: fn(ctxt: ref Draw->Context, args: list of string);
};

init(nil: ref Draw->Context, nil: list of string)
{
	sys = load Sys Sys->PATH;

	seq := hailstone(big 27);
	l := len seq;

	sys->print("hailstone(27):  ");
	for(i := 0; i < 4; i++) {
		sys->print("%bd, ", hd seq);
		seq = tl seq;
	}
	sys->print("⋯");

	while(len seq > 4)
		seq = tl seq;

	while(seq != nil) {
		sys->print(", %bd", hd seq);
		seq = tl seq;
	}
	sys->print(" (length %d)\n", l);

	max := 1;
	maxn := big 1;
	for(n := big 2; n < big 100000; n++) {
		cur := len hailstone(n);
		if(cur > max) {
			max = cur;
			maxn = n;
		}
	}
	sys->print("hailstone(%bd) has length %d\n", maxn, max);
}

hailstone(i: big): list of big
{
	if(i == big 1)
		return big 1 :: nil;
	if(i % big 2 == big 0)
		return i :: hailstone(i / big 2);
	return i :: hailstone((big 3 * i) + big 1);
}

```


```txt
hailstone(27):  27, 82, 41, 124, ⋯, 8, 4, 2, 1 (length 112)
hailstone(77031) has length 351

```



## Lingo


```lingo
on hailstone (n, sequenceList)
  len = 1
  repeat while n<>1
    if listP(sequenceList) then sequenceList.add(n)
    if n mod 2 = 0 then
      n = n / 2
    else
      n = 3 * n + 1
    end if
    len = len + 1
  end repeat
  if listP(sequenceList) then sequenceList.add(n)
  return len
end
```

Usage:

```lingo
sequenceList = []
hailstone(27, sequenceList)
put sequenceList
-- [27, 82, 41, 124, ... , 8, 4, 2, 1]

n = 0
maxLen = 0
repeat with i = 1 to 99999
  len = hailstone(i)
  if len>maxLen then
    n = i
    maxLen = len
  end if
end repeat
put n, maxLen
-- 77031 351
```



## Logtalk


```logtalk
:- object(hailstone).

	:- public(generate_sequence/2).
	:- mode(generate_sequence(+natural, -list(natural)), zero_or_one).
	:- info(generate_sequence/2, [
		comment is 'Generates the Hailstone sequence that starts with its first argument. Fails if the argument is not a natural number.',
		argnames is ['Start', 'Sequence']
	]).

	:- public(write_sequence/1).
	:- mode(write_sequence(+natural), zero_or_one).
	:- info(write_sequence/1, [
		comment is 'Writes to the standard output the Hailstone sequence that starts with its argument. Fails if the argument is not a natural number.',
		argnames is ['Start']
	]).

	:- public(sequence_length/2).
	:- mode(sequence_length(+natural, -natural), zero_or_one).
	:- info(sequence_length/2, [
		comment is 'Calculates the length of the Hailstone sequence that starts with its first argument. Fails if the argument is not a natural number.',
		argnames is ['Start', 'Length']
	]).

	:- public(longest_sequence/4).
	:- mode(longest_sequence(+natural, +natural, -natural, -natural), zero_or_one).
	:- info(longest_sequence/4, [
		comment is 'Calculates the longest Hailstone sequence in the interval [Start, End]. Fails if the interval is not valid.',
		argnames is ['Start', 'End', 'N', 'Length']
	]).

	generate_sequence(Start, Sequence) :-
		integer(Start),
		Start >= 1,
		sequence(Start, Sequence).

	sequence(1, [1]) :-
		!.
	sequence(N, [N| Sequence]) :-
		(	N mod 2 =:= 0 ->
			M is N // 2
		;	M is (3 * N) + 1
		),
		sequence(M, Sequence).

	write_sequence(Start) :-
		integer(Start),
		Start >= 1,
		sequence(Start).

	sequence(1) :-
		!,
		write(1), nl.
	sequence(N) :-
		write(N), write(' '),
		(	N mod 2 =:= 0 ->
			M is N // 2
		;	M is (3 * N) + 1
		),
		sequence(M).

	sequence_length(Start, Length) :-
		integer(Start),
		Start >= 1,
		sequence_length(Start, 1, Length).

	sequence_length(1, Length, Length) :-
		!.
	sequence_length(N, Length0, Length) :-
		Length1 is Length0 + 1,
		(	N mod 2 =:= 0 ->
			M is N // 2
		;	M is (3 * N) + 1
		),
		sequence_length(M, Length1, Length).

	longest_sequence(Start, End, N, Length) :-
		integer(Start),
		integer(End),
		Start >= 1,
		Start =< End,
		longest_sequence(Start, End, 1, N, 1, Length).

	longest_sequence(Current, End, N, N, Length, Length) :-
		Current > End,
		!.
	longest_sequence(Current, End, N0, N, Length0, Length) :-
		sequence_length(Current, 1, CurrentLength),
		Next is Current + 1,
		(	CurrentLength > Length0 ->
			longest_sequence(Next, End, Current, N, CurrentLength, Length)
		;	longest_sequence(Next, End, N0, N, Length0, Length)
		).

:- end_object.
```

Testing:

```logtalk
| ?- hailstone::write_sequence(27).
27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274 137 412 206 103 310 155 466 233 700 350 175 526 263 790 395 1186 593 1780 890 445 1336 668 334 167 502 251 754 377 1132 566 283 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 1619 4858 2429 7288 3644 1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154 577 1732 866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35 106 53 160 80 40 20 10 5 16 8 4 2 1
true

| ?- hailstone::sequence_length(27, Length).
Length = 112
true

| ?- hailstone::longest_sequence(1, 100000, N, Length).
N = 77031, Length = 351
true
```



## LOLCODE

There is presently no way to query a <tt>BUKKIT</tt> for the existence of a given key, thus making memoization infeasible. This solution takes advantage of prior knowledge to run in reasonable time.

```LOLCODE
HAI 1.3

HOW IZ I hailin YR stone
    I HAS A sequence ITZ A BUKKIT
    sequence HAS A length ITZ 1
    sequence HAS A SRS 0 ITZ stone

    IM IN YR stoner
        BOTH SAEM stone AN 1, O RLY?
            YA RLY, FOUND YR sequence
        OIC

        MOD OF stone AN 2, O RLY?
            YA RLY, stone R SUM OF PRODUKT OF stone AN 3 AN 1
            NO WAI, stone R QUOSHUNT OF stone AN 2
        OIC

        sequence HAS A SRS sequence'Z length ITZ stone
        sequence'Z length R SUM OF sequence'Z length AN 1
    IM OUTTA YR stoner
IF U SAY SO

I HAS A hail27 ITZ I IZ hailin YR 27 MKAY
VISIBLE "hail(27) = "!

IM IN YR first4 UPPIN YR i TIL BOTH SAEM i AN 4
    VISIBLE hail27'Z SRS i " "!
IM OUTTA YR first4
VISIBLE "..."!

IM IN YR last4 UPPIN YR i TIL BOTH SAEM i AN 4
    VISIBLE " " hail27'Z SRS SUM OF 108 AN i!
IM OUTTA YR last4
VISIBLE ", length = " hail27'Z length

I HAS A max, I HAS A len ITZ 0

BTW, DIS IZ RLY NOT FAST SO WE ONLY CHEK N IN [75000, 80000)
IM IN YR maxer UPPIN YR n TIL BOTH SAEM n AN 5000
    I HAS A n ITZ SUM OF n AN 75000
    I HAS A seq ITZ I IZ hailin YR n MKAY
    BOTH SAEM len AN SMALLR OF len AN seq'Z length, O RLY?
        YA RLY, max R n, len R seq'Z length
    OIC
IM OUTTA YR maxer

VISIBLE "len(hail(" max ")) = " len

KTHXBYE
```

```txt
hail(27) = 27 82 41 124 ... 8 4 2 1, length = 112
len(hail(77031)) = 351
```



## Lua


```lua
function hailstone( n, print_numbers )
    local n_iter = 1

    while n ~= 1 do
        if print_numbers then print( n ) end
        if n % 2 == 0 then
            n = n / 2
        else
            n = 3 * n + 1
        end

        n_iter = n_iter + 1
    end
    if print_numbers then print( n ) end

    return n_iter;
end

hailstone( 27, true )

max_i, max_iter = 0, 0
for i = 1, 100000 do
    num = hailstone( i, false )
    if num >= max_iter then
        max_i = i
        max_iter = num
    end
end

print( string.format( "Needed %d iterations for the number %d.\n", max_iter, max_i ) )
```


## M2000 Interpreter

Use of two versions of Hailstone, one which return each n, and another one which return only the length of sequence.

Also we use current stack as FIFO to get the last 4 numbers

```M2000 Interpreter

Module hailstone.Task {
      hailstone=lambda  (n as long)->{
            =lambda n  (&val) ->{
                  if n=1 then =false: exit
                  =true
                  if n mod 2=0 then n/=2 : val=n: exit
                  n*=3 : n++: val=n
            }
      }
      Count=Lambda (n) ->{
            m=lambda n ->{
                  if n=1 then =false: exit
                  =true :if n mod 2=0 then n/=2 :exit
                  n*=3 : n++
            }
            c=1
            While m() {c++}
            =c

      }
      k=Hailstone(27)
      counter=1
      x=0
      Print 27,
      While k(&x) {
            counter++
            Print x,
            if counter=4 then exit
      }
      Print
      Flush  ' empty current stack
      While k(&x) {
            counter++
            data x   ' send to end of stack -used as FIFO
            if stack.size>4 then drop
      }
      \\ [] return a stack object and leave empty current stack
      \\ Print use automatic iterator to print all values in columns.
      Print []
      Print "counter:";counter
      m=0
      For i=2 to 99999 {
            m1=max.data(count(i), m)
            if m1<>m then m=m1: im=i
      }
      Print Format$("Number {0} has then longest hailstone sequence of length {1}", im, m)
}
hailstone.Task

```

```txt

      27      82      41     124
       8       4       2       1
counter:112
Number 77031 has then longest hailstone sequence of length 351
</pre >


## Maple

Define the procedure:

```Maple

hailstone := proc( N )
    local n := N, HS := Array([n]);
    while n > 1 do
        if type(n,even) then
            n := n/2;
        else
            n := 3*n+1;
        end if;
        HS(numelems(HS)+1) := n;
    end do;
    HS;
end proc;

```

Run the command and show the appropriate portion of the result;

```Maple

> r := hailstone(27):
                              [ 1..112 1-D Array     ]
                         r := [ Data Type: anything  ]
                              [ Storage: rectangular ]
                              [ Order: Fortran_order ]
> r(1..4) ... r(-4..);
                       [27, 82, 41, 124] .. [8, 4, 2, 1]

```

Compute the first 100000 sequences:

```Maple

longest := 0; n := 0;
for i from 1 to 100000 do
    len := numelems(hailstone(i));
    if len > longest then
        longest := len;
        n := i;
    end if;
od:
printf("The longest Hailstone sequence in the first 100k is n=%d, with %d terms\n",n,longest);

```

```txt

The longest Hailstone sequence in the first 100k is n=77031, with 351 terms

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Here are four ways to generate the sequence.


###  Nested function call formulation


```Mathematica
HailstoneF[n_] := NestWhileList[If[OddQ@#, 3 # + 1, #/2] &, n, # > 1 &]
```


This is probably the most readable and shortest implementation.

=== Fixed-Point formulation ===

```Mathematica
HailstoneFP[n_] := Most@FixedPointList[Switch[#, 1, 1, _?OddQ , 3# + 1, _, #/2] &, n]
```



###  Recursive formulation


```Mathematica
HailstoneR[1] = {1}
HailstoneR[n_?OddQ] := Prepend[HailstoneR[3 n + 1], n]
HailstoneR[n_] := Prepend[HailstoneR[n/2], n]
```



###  Procedural implementation


```Mathematica
HailstoneP[n_] := Module[{x = {n}, s = n},
 While[s > 1, x = {x, s = If[OddQ@s, 3 s + 1, s/2]}]; Flatten@x]
```



###  Validation


I use this version to do the validation:

```Mathematica
Hailstone[n_] :=
 NestWhileList[Which[Mod[#, 2] == 0, #/2, True, ( 3*# + 1) ] &, n, # != 1 &];


c27 = Hailstone@27;
Print["Hailstone sequence for n = 27: [", c27[[;; 4]], "...", c27[[-4 ;;]], "]"]
Print["Length Hailstone[27] = ", Length@c27]

longest = -1; comp = 0;
Do[temp = Length@Hailstone@i;
 If[comp < temp, comp = temp; longest = i],
 {i, 100000}
 ]
Print["Longest Hailstone sequence at n = ", longest, "\nwith length = ", comp];
```

```txt

Hailstone sequence for n = 27: [{27,82,41,124}...{8,4,2,1}]
Length Hailstone[27] = 112
Longest Hailstone sequence at n = 77031
with length = 351

```

I think the fixed-point and the recursive piece-wise function formulations are more idiomatic for Mathematica


### = Sequence 27 =


```Mathematica
With[{seq = HailstoneFP[27]}, { Length[seq], Take[seq, 4], Take[seq, -4]}]
```

```txt
{112, {27, 82, 41, 124}, {8, 4, 2, 1}}
```


Alternatively,

```Mathematica
Short[HailstoneFP[27],0.45]
```


```txt
{27, 82, 41, 124, <<104>>, 8, 4, 2, 1}
```



### = Longest sequence length =


```Mathematica
MaximalBy[Table[{i, Length[HailstoneFP[i]]}, {i, 100000}], Last]
```

```txt
```


=={{header|MATLAB}} / {{header|Octave}}==

### Hailstone Sequence For N


```Matlab
function x = hailstone(n)
  x = n;
  while n > 1
       % faster than mod(n, 2)
    if n ~= floor(n / 2) * 2
      n = n * 3 + 1;
    else
      n = n / 2;
    end
    x(end + 1) = n; %#ok
  end
```

Show sequence of hailstone(27) and number of elements:

```Matlab
x = hailstone(27);
fprintf('hailstone(27): %d %d %d %d ... %d %d %d %d\nnumber of elements: %d\n', x(1:4), x(end-3:end), numel(x))
```

```txt
hailstone(27): 27 82 41 124 ... 8 4 2 1
number of elements: 112
```


### Longest Hailstone Sequence Under N

Show the number less than 100,000 which has the longest hailstone sequence together with that sequence's length:
====Basic Version (use the above routine)====

```Matlab
N = 1e5;
maxLen = 0;
for k = 1:N
  kLen = numel(hailstone(k));
  if kLen > maxLen
    maxLen = kLen;
    n = k;
  end
end
```

```txt
n = 77031
maxLen = 351
```


### =Faster Version=


```matlab
function [n, maxLen] = longestHailstone(N)
  maxLen = 0;
  for k = 1:N
    a = k;
    kLen = 1;
    while a > 1
      if a ~= floor(a / 2) * 2
        a = a * 3 + 1;
      else
        a = a / 2;
      end
      kLen = kLen + 1;
    end
    if kLen > maxLen
      maxLen = kLen;
      n = k;
    end
  end
```

```matlab>>
 [n, maxLen] = longestHailstone(1e5)
n = 77031
maxLen = 351
```


### =Much Faster Version With Caching=


```matlab
function [n, maxLen] = longestHailstone(N)
  lenList(N) = 0;
  lenList(1) = 1;
  maxLen = 0;
  for k = 2:N
    a = k;
    kLen = 0;
    while a >= k
      if a == floor(a / 2) * 2
        a = a / 2;
      else
        a = a * 3 + 1;
      end
      kLen = kLen + 1;
    end
    kLen = kLen + lenList(a);
    lenList(k) = kLen;
    if kLen > maxLen
      maxLen = kLen;
      n = k;
    end
  end
```

```matlab>>
 [n, maxLen] = longestHailstone(1e5)
n = 77031
maxLen = 351
```



## Maxima


```maxima
collatz(n) := block([L], L: [n], while n > 1 do
(n: if evenp(n) then n/2 else 3*n + 1, L: endcons(n, L)), L)$

collatz_length(n) := block([m], m: 1, while n > 1 do
(n: if evenp(n) then n/2 else 3*n + 1, m: m + 1), m)$

collatz_max(n) := block([j, m, p], m: 0,
for i from 1 thru n do
   (p: collatz_length(i), if p > m then (m: p, j: i)),
[j, m])$

collatz(27);           /* [27, 82, 41, ..., 4, 2, 1] */
length(%);             /* 112 */
collatz_length(27);    /* 112 */
collatz_max(100000);   /* [77031, 351] */
```



## Mercury

The actual calculation (including module ceremony)
providing both a function and a predicate implementation:

```mercury
:- module hailstone.

:- interface.

:- import_module int, list.

:- func hailstone(int) = list(int).
:- pred hailstone(int::in, list(int)::out) is det.

:- implementation.

hailstone(N) = S :- hailstone(N, S).

hailstone(N, [N|S]) :-
  ( N = 1 ->       S = []
  ; N mod 2 = 0 -> hailstone(N/2, S)
  ;                hailstone(3 * N + 1, S) ).

:- end_module hailstone.
```


The mainline test driver (making use of [http://en.wikipedia.org/wiki/Unification_(computer_science) unification] for more succinct tests):

```mercury
:- module test_hailstone.

:- interface.

:- import_module io.

:- pred main(io.state::di, io.state::uo) is det.

:- implementation.

:- import_module int, list.
:- import_module hailstone.

:- pred longest(int::in, int::out, int::out) is det.
:- pred longest(int::in, int::in, int::in, int::out, int::out) is det.

longest(M, N, L) :- longest(M, 0, 0, N, L).

longest(N, CN, CL, MN, ML) :-
  ( N > 1 ->
      L = list.length(hailstone(N)),
      ( L > CL -> longest(N - 1, N,  L,  MN, ML)
      ;           longest(N - 1, CN, CL, MN, ML) )
  ;   MN = CN, ML = CL ).


main(!IO) :-
  S = hailstone(27),
  ( list.length(S) = 112,
    list.append([27, 82, 41, 124], _, S),
    list.remove_suffix(S, [8, 4, 2, 1], _),
    longest(100000, 77031, 351) ->
      io.write_string("All tests succeeded.\n", !IO)
  ;   io.write_string("At least one test failed.\n", !IO) ).

:- end_module test_hailstone.
```


{{out}} of running this program is:
 All tests succeeded.

For those unused to logic programming languages it seems that nothing has been proved in terms of confirming anything, but if you look at the predicate declaration for <code>longest/3</code> …


```mercury
:- pred longest(int::in, int::out, int::out) is det.
```


… you see that the second and third parameters are '''output''' parameters.
This by calling <code>longest(100000, 77031, 351)</code> you prove,
through unification, that the longest sequence is with the
number 77031 and that it is 351 cycles long.

Similarly, using <code>list.append([27, 82, 41, 124], _, S)</code> automatically proves that the generated sequence begins with the provided sequence, etc.
Thus we know that the correct sequences and values were generated
without bothering to print them out.


## ML

=
## MLite
=

```ocaml
fun hail (x = 1) = [1]
       | (x rem 2 = 0) = x :: hail (x div 2)
       | x = x :: hail (x * 3 + 1)

fun hailstorm
		([], i, largest, largest_at) = (largest_at, largest)
	| 	(x :: xs, i, largest, largest_at) =
		let
			val k = len (hail x)
		in
			if k > largest then
				hailstorm (xs, i + 1, k, i)
			else
				hailstorm (xs, i + 1, largest, largest_at)
			end
	| 	(x :: xs) = hailstorm (x :: xs, 1, 0, 0)

 ;

val h27 = hail 27;
print "hailstone sequence for the number 27 has ";
print ` len (h27);
print " elements starting with ";
print ` sub (h27, 0, 4);
print " and ending with ";
print ` sub (h27, len(h27)-4, len h27);
println ".";

val biggest = hailstorm ` iota (100000 - 1);

print "The number less than 100,000 which has the longest ";
print "hailstone sequence is at element ";
print ` ref (biggest, 0);
print " and is of length ";
println ` ref (biggest, 1);
```

```txt
hailstone sequence for the number 27 has 112 elements starting with [27, 82, 41, 124] and ending with [8, 4, 2, 1].
The number less than 100,000 which has the longest hailstone sequence is at element 77031 and is of length 351
```


=={{header|Modula-2}}==

```modula2
MODULE hailst;

IMPORT  InOut;

CONST   maxCard         = MAX (CARDINAL) DIV 3;
TYPE    action          = (List, Count, Max);
VAR     a               : CARDINAL;

PROCEDURE HailStone (start  : CARDINAL;  type  : action) : CARDINAL;

VAR     n, max, count           : CARDINAL;

BEGIN
  count := 1;
  n := start;
  max := n;
  LOOP
    IF  type = List  THEN
      InOut.WriteCard (n, 12);
      IF  count MOD 6 = 0  THEN  InOut.WriteLn  END
    END;
    IF  n = 1  THEN  EXIT  END;
    IF  ODD (n)  THEN
      IF  n < maxCard  THEN
        n := 3 * n + 1;
        IF   n > max  THEN  max := n  END
      ELSE
        InOut.WriteString ("Exceeding max value for type CARDINAL at count = ");
        InOut.WriteCard (count, 10);
        InOut.WriteString (" for intermediate value ");
        InOut.WriteCard (n, 10);
        InOut.WriteString (". Aborting.");
        HALT
      END
    ELSE
      n := n DIV 2
    END;
    INC (count)
  END;
  IF  type = Max  THEN  RETURN  max  ELSE  RETURN  count  END
END HailStone;

PROCEDURE FindMax (num   : CARDINAL);

VAR     val, maxCount, maxVal, cnt      : CARDINAL;

BEGIN
  maxCount := 0;
  maxVal := 0;
  FOR  val := 2 TO num  DO
   cnt := HailStone (val, Count);
    IF  cnt > maxCount  THEN
      maxVal := val;
      maxCount := cnt
    END
  END;
  InOut.WriteString ("Longest sequence below ");        InOut.WriteCard (num, 1);
  InOut.WriteString (" is ");           InOut.WriteCard (HailStone (maxVal, Count), 1);
  InOut.WriteString (" for n = ");      InOut.WriteCard (maxVal, 1);
  InOut.WriteString (" with an intermediate maximum of ");
  InOut.WriteCard (HailStone (maxVal, Max), 1);
  InOut.WriteLn
END FindMax;

BEGIN
  a := HailStone (27, List);
  InOut.WriteLn;
  InOut.WriteString ("Iterations total = ");    InOut.WriteCard (HailStone (27, Count), 12);
  InOut.WriteString (" max value = ");          InOut.WriteCard (HailStone (27, Max)  , 12);
  InOut.WriteLn;
  FindMax (100000);
  InOut.WriteString ("Done.");          InOut.WriteLn
END hailst.
```

Producing:

```txt
jan@Beryllium:~/modula/rosetta$ hailst
          27          82          41         124          62          31
          94          47         142          71         214         107
         322         161         484         242         121         364
         182          91         274         137         412         206
         103         310         155         466         233         700
         350         175         526         263         790         395
        1186         593        1780         890         445        1336
         668         334         167         502         251         754
         377        1132         566         283         850         425
        1276         638         319         958         479        1438
         719        2158        1079        3238        1619        4858
        2429        7288        3644        1822         911        2734
        1367        4102        2051        6154        3077        9232
        4616        2308        1154         577        1732         866
         433        1300         650         325         976         488
         244         122          61         184          92          46
          23          70          35         106          53         160
          80          40          20          10           5          16
           8           4           2           1
Iterations total =          112 max value =         9232
Longest sequence below 100000 is 351 for n = 77031 with an intermediate maximum of 21933016
Done.
```
When trying the same for all values below 1 million:
```txt

Exceeding max value for type CARDINAL at n = 159487 , count = 60 and intermediate value 1699000271. Aborting.
```



## MUMPS


```MUMPS
hailstone(n)	;
	If n=1 Quit n
	If n#2 Quit n_" "_$$hailstone(3*n+1)
	Quit n_" "_$$hailstone(n\2)
Set x=$$hailstone(27) Write !,$Length(x," ")," terms in ",x,!
112 terms in 27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274 137 412 206 103 310 155 466 233 700 350 175 526 263 790 395 1186 593 1780 890 445 1336 668 334 167 502 251 754 377 1132 566 283 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 1619 4858 2429 7288 3644 1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154 577 1732 866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35 106 53 160 80 40 20 10 5 16 8 4 2 1
```



## NetRexx


```NetRexx
/* NetRexx */

options replace format comments java crossref savelog symbols binary

do
  start = 27
  hs = hailstone(start)
  hsCount = hs.words
  say 'The number' start 'has a hailstone sequence comprising' hsCount 'elements'
  say '  its first four elements are:' hs.subword(1, 4)
  say '   and last four elements are:' hs.subword(hsCount - 3)

  hsMax = 0
  hsCountMax = 0
  llimit = 100000
  loop x_ = 1 to llimit - 1
    hs = hailstone(x_)
    hsCount = hs.words
    if hsCount > hsCountMax then do
      hsMax = x_
      hsCountMax = hsCount
      end
    end x_

  say 'The number' hsMax 'has the longest hailstone sequence in the range 1 to' llimit - 1 'with a sequence length of' hsCountMax
catch ex = Exception
  ex.printStackTrace
end

return

method hailstone(hn = long) public static returns Rexx signals IllegalArgumentException

  hs = Rexx('')
  if hn <= 0 then signal IllegalArgumentException('Invalid start point.  Must be a positive integer greater than 0')

  loop label n_ while hn > 1
    hs = hs' 'hn
    if hn // 2 \= 0 then hn = hn * 3 + 1
                    else hn = hn % 2
    end n_
  hs = hs' 'hn

  return hs.strip
```

```txt

The number 27 has a hailstone sequence comprising 112 elements
  its first four elements are: 27 82 41 124
   and last four elements are: 8 4 2 1
The number 77031 has the longest hailstone sequence in the range 1 to 99999 with a sequence length of 351

```



## Nim

```nim
proc hailstone(n): auto =
  result = @[n]
  var n = n
  while n > 1:
    if (n and 1) == 1:
      n = 3 * n + 1
    else:
      n = n div 2
    result.add n

let h = hailstone 27
assert h.len == 112 and h[0..3] == @[27,82,41,124] and h[h.high-3..h.high] == @[8,4,2,1]
var m, mi = 0
for i in 1 .. <100_000:
  let n = hailstone(i).len
  if n > m:
    m = n
    mi = i
echo "Maximum length ", m, " was found for hailstone(", mi, ") for numbers <100,000"
```

```txt
Maximum length 351 was found for hailstone(77031) for numbers <100,000
```


=={{header|Oberon-2}}==

```oberon2
MODULE hailst;

IMPORT  Out;

CONST   maxCard         = MAX (INTEGER) DIV 3;
        List            = 1;
        Count           = 2;
        Max             = 3;

VAR     a               : INTEGER;

PROCEDURE HailStone (start, type  : INTEGER) : INTEGER;

VAR     n, max, count           : INTEGER;

BEGIN
  count := 1;
  n := start;
  max := n;
  LOOP
    IF  type = List  THEN
      Out.Int (n, 12);
      IF  count MOD 6 = 0  THEN  Out.Ln  END
    END;
    IF  n = 1  THEN  EXIT  END;
    IF  ODD (n)  THEN
      IF  n < maxCard  THEN
        n := 3 * n + 1;
        IF   n > max  THEN  max := n  END
      ELSE
        Out.String ("Exceeding max value for type INTEGER at: ");
        Out.String (" n = ");           Out.Int (start, 12);
        Out.String (" , count = ");     Out.Int (count, 12);
        Out.String (" and intermediate value ");
        Out.Int (n, 1);
        Out.String (". Aborting.");
        Out.Ln;
        HALT (2)
      END
    ELSE
      n := n DIV 2
    END;
    INC (count)
  END;
  IF  type = Max  THEN  RETURN  max  ELSE  RETURN  count  END
END HailStone;


PROCEDURE FindMax (num   : INTEGER);

VAR     val, maxCount, maxVal, cnt      : INTEGER;

BEGIN
  maxCount := 0;
  maxVal := 0;
  FOR  val := 2 TO num  DO
   cnt := HailStone (val, Count);
    IF  cnt > maxCount  THEN
      maxVal := val;
      maxCount := cnt
    END
  END;
  Out.String ("Longest sequence below ");       Out.Int (num, 1);
  Out.String (" is ");                          Out.Int (HailStone (maxVal, Count), 1);
  Out.String (" for n = ");                     Out.Int (maxVal, 1);
  Out.String (" with an intermediate maximum of ");
  Out.Int (HailStone (maxVal, Max), 1);
  Out.Ln
END FindMax;

BEGIN
  a := HailStone (27, List);
  Out.Ln;
  Out.String ("Iterations total = ");   Out.Int (HailStone (27, Count), 12);
  Out.String (" max value = ");         Out.Int (HailStone (27, Max)  , 12);
  Out.Ln;
  FindMax (1000000);
  Out.String ("Done.");
  Out.Ln
END hailst.
```

Producing

```txt

          27          82          41         124          62          31
          94          47         142          71         214         107
         322         161         484         242         121         364
         182          91         274         137         412         206
         103         310         155         466         233         700
         350         175         526         263         790         395
        1186         593        1780         890         445        1336
         668         334         167         502         251         754
         377        1132         566         283         850         425
        1276         638         319         958         479        1438
         719        2158        1079        3238        1619        4858
        2429        7288        3644        1822         911        2734
        1367        4102        2051        6154        3077        9232
        4616        2308        1154         577        1732         866
         433        1300         650         325         976         488
         244         122          61         184          92          46
          23          70          35         106          53         160
          80          40          20          10           5          16
           8           4           2           1

Iterations total = 112 max value =  9232

Exceeding max value for type INTEGER at:  n = 113383 , count = 120 and intermediate value 827370449. Aborting.
```



## OCaml


```ocaml
#load "nums.cma";;
open Num;;

(* generate Hailstone sequence *)
let hailstone n =
  let one = Int 1
  and two = Int 2
  and three = Int 3 in
  let rec g s x =
    if x =/ one
    then x::s
    else g (x::s) (if mod_num x two =/ one
                   then three */ x +/ one
                   else x // two)
  in
  g [] (Int n)
;;

(* compute only sequence length *)
let haillen n =
  let one = Int 1
  and two = Int 2
  and three = Int 3 in
  let rec g s x =
    if x =/ one
    then s+1
    else g (s+1) (if mod_num x two =/ one
                  then three */ x +/ one
                  else x // two)
  in
  g 0 (Int n)
;;

(* max length for starting values in 1..n *)
let hailmax =
  let rec g idx len = function
  | 0 -> (idx, len)
  | i ->
      let a = haillen i in
      if a > len
      then g i a (i-1)
      else g idx len (i-1)
  in
  g 0 0
;;

hailmax 100000 ;;
(* - : int * int = (77031, 351) *)

List.rev_map string_of_num (hailstone 27) ;;

(* - : string list =
["27"; "82"; "41"; "124"; "62"; "31"; "94"; "47"; "142"; "71"; "214"; "107";
 "322"; "161"; "484"; "242"; "121"; "364"; "182"; "91"; "274"; "137"; "412";
 "206"; "103"; "310"; "155"; "466"; "233"; "700"; "350"; "175"; "526"; "263";
 "790"; "395"; "1186"; "593"; "1780"; "890"; "445"; "1336"; "668"; "334";
 "167"; "502"; "251"; "754"; "377"; "1132"; "566"; "283"; "850"; "425";
 "1276"; "638"; "319"; "958"; "479"; "1438"; "719"; "2158"; "1079"; "3238";
 "1619"; "4858"; "2429"; "7288"; "3644"; "1822"; "911"; "2734"; "1367";
 "4102"; "2051"; "6154"; "3077"; "9232"; "4616"; "2308"; "1154"; "577";
 "1732"; "866"; "433"; "1300"; "650"; "325"; "976"; "488"; "244"; "122";
 "61"; "184"; "92"; "46"; "23"; "70"; "35"; "106"; "53"; "160"; "80"; "40";
 "20"; "10"; "5"; "16"; "8"; "4"; "2"; "1"] *)
```



## Oforth



```Oforth
: hailstone   // n -- [n]
| l |
   ListBuffer new ->l
   while(dup 1 <>) [ dup l add dup isEven ifTrue: [ 2 / ] else: [ 3 * 1+ ] ]
   l add l dup freeze ;

hailstone(27) dup size println dup left(4) println right(4) println
100000 seq map(#[ dup hailstone size swap Pair new ]) reduce(#maxKey) println
```


```txt

112
[27, 82, 41, 124]
[8, 4, 2, 1]
[351, 77031]

```



## ooRexx


```ooRexx

sequence = hailstone(27)
say "Hailstone sequence for 27 has" sequence~items "elements and is ["sequence~toString('l', ", ")"]"

highestNumber = 1
highestCount = 1

loop i = 2 to 100000
    sequence = hailstone(i)
    count = sequence~items
    if count > highestCount then do
        highestNumber = i
        highestCount = count
    end
end
say "Number" highestNumber "has the longest sequence with" highestCount "elements"

-- short routine to generate a hailstone sequence
::routine hailstone
  use arg n

  sequence = .array~of(n)
  loop while n \= 1
      if n // 2 == 0 then n = n / 2
      else n = 3 * n + 1
      sequence~append(n)
  end
  return sequence

```

```txt

Hailstone sequence for 27 has 112 elements and is [27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502, 251, 754, 77, 1132, 566, 283, 850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644, 1822, 911, 2734, 1367, 102, 051, 6154, 3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 0, 40, 20, 10, 5, 16, 8, 4, 2, 1]
Number 77031 has the longest sequence with 351 elements

```



## Order

To display the length, and first and last elements, of the hailstone sequence for 27, we could do this:

```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8hailstone ORDER_PP_FN(                  \
8fn(8N,                                                       \
    8cond((8equal(8N, 1), 8seq(1))                            \
          (8is_0(8remainder(8N, 2)),                          \
           8seq_push_front(8N, 8hailstone(8quotient(8N, 2)))) \
          (8else,                                             \
           8seq_push_front(8N, 8hailstone(8inc(8times(8N, 3))))))) )

ORDER_PP(
  8lets((8H, 8seq_map(8to_lit, 8hailstone(27)))
        (8S, 8seq_size(8H)),
        8print(8(h(27) - length:) 8to_lit(8S) 8comma 8space
               8(starts with:) 8seq_take(4, 8H) 8comma 8space
               8(ends with:) 8seq_drop(8minus(8S, 4), 8H))
        ) )
```

<lang>h(27) - length:112, starts with:(27)(82)(41)(124), ends with:(8)(4)(2)(1)
```


Unfortunately, the C preprocessor not really being designed with large amounts of garbage collection in mind, trying to compute the hailstone sequences up to 100000 is almost guaranteed to run out of memory (and take a very, very long time). If we wanted to try, we could add this to the program, which in most languages would use relatively little memory:

```c
#define ORDER_PP_DEF_8h_longest ORDER_PP_FN( \
8fn(8M, 8P, \
    8if(8is_0(8M), \
        8P, \
        8let((8L, 8seq_size(8hailstone(8M))), \
             8h_longest(8dec(8M), \
                        8if(8greater(8L, 8tuple_at_1(8P)), \
                            8pair(8M, 8L), 8P))))) )

ORDER_PP(
  8let((8P, 8h_longest(8nat(1,0,0,0,0,0), 8pair(0, 0))),
       8pair(8to_lit(8tuple_at_0(8P)), 8to_lit(8tuple_at_1(8P))))
)
```


...or even this "more elegant" version, which will run out of memory very quickly indeed (but in practice seems to work better for smaller ranges):

```c
ORDER_PP(
  8let((8P,
        8seq_head(
          8seq_sort(8fn(8P, 8Q, 8greater(8tuple_at_1(8P),
                                         8tuple_at_1(8Q))),
                    8seq_map(8fn(8N,
                                 8pair(8N, 8seq_size(8hailstone(8N)))),
                             8seq_iota(1, 8nat(1,0,0,0,0,0)))))),
       8pair(8to_lit(8tuple_at_0(8P)), 8to_lit(8tuple_at_1(8P)))) )
```


Notice that large numbers (>100) must be entered as digit sequences with <code>8nat</code>. <code>8to_lit</code> converts a digit sequence back to a readable number.


## Oz


```oz
declare
  fun {HailstoneSeq N}
     N > 0 = true %% assert
     if N == 1 then         [1]
     elseif {IsEven N} then N|{HailstoneSeq N div 2}
     else                   N|{HailstoneSeq 3*N+1}
     end
  end

  HSeq27 = {HailstoneSeq 27}
  {Length HSeq27} = 112
  {List.take HSeq27 4} = [27 82 41 124]
  {List.drop HSeq27 108} = [8 4 2 1]

  fun {MaxBy2nd A=A1#A2 B=B1#B2}
     if B2 > A2 then B else A end
  end

  Pairs = {Map {List.number 1 99999 1}
           fun {$ I} I#{Length {HailstoneSeq I}} end}

  MaxI#MaxLen = {List.foldL Pairs MaxBy2nd 0#0}
  {System.showInfo
   "Maximum length "#MaxLen#" was found for hailstone("#MaxI#")"}
```

```txt

Maximum length 351 was found for hailstone(77031)

```



## PARI/GP



### Version #1.


```parigp
show(n)={
  my(t=1);
  while(n>1,
    print1(n",");
    n=if(n%2,
      3*n+1
    ,
      n/2
    );
    t++
  );
  print(1);
  t
};

len(n)={
  my(t=1);
  while(n>1,
    if(n%2,
      t+=2;
      n+=(n>>1)+1
    ,
      t++;
      n>>=1
    )
  );
  t
};

show(27)
r=0;for(n=1,1e5,t=len(n);if(t>r,r=t;ra=n));print(ra"\t"r)
```

```txt
27,82,41,124,62,31,94,47,142,71,214,107,322,161,484,242,121,364,182,91,274,137,4
12,206,103,310,155,466,233,700,350,175,526,263,790,395,1186,593,1780,890,445,133
6,668,334,167,502,251,754,377,1132,566,283,850,425,1276,638,319,958,479,1438,719
,2158,1079,3238,1619,4858,2429,7288,3644,1822,911,2734,1367,4102,2051,6154,3077,
9232,4616,2308,1154,577,1732,866,433,1300,650,325,976,488,244,122,61,184,92,46,2
3,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1
```

and

```txt
77031   351
```



### Version #2.


Different kind of PARI scripts for Collatz sequences you can find in OEIS, e.g.:
[http://oeis.org/A070165 A070165]


```parigp

\\ Get vector with Collatz sequence for the specified starting number.
\\ Limit vector to the lim length, or less, if 1 (one) term is reached (when lim=0).
\\ 3/26/2016 aev
Collatz(n,lim=0)={
my(c=n,e=0,L=List(n)); if(lim==0, e=1; lim=n*10^6);
for(i=1,lim, if(c%2==0, c=c/2, c=3*c+1); listput(L,c); if(e&&c==1, break));
return(Vec(L)); }
Collatzmax(ns,nf)={
my(V,vn,mxn=1,mx,im=1);
print("Search range: ",ns,"..",nf);
for(i=ns,nf, V=Collatz(i); vn=#V; if(vn>mxn, mxn=vn; im=i); kill(V));
print("Hailstone/Collatz(",im,") has the longest length = ",mxn);
}

{
\\ Required tests:
print("Required tests:");
my(Vr,vrn);
Vr=Collatz(27); vrn=#Vr;
print("Hailstone/Collatz(27): ",Vr[1..4]," ... ",Vr[vrn-3..vrn],"; length = ",vrn);
Collatzmax(1,100000);
}

```


```txt

Required tests:
Hailstone/Collatz(27): [27, 82, 41, 124] ... [8, 4, 2, 1]; length = 112
Search range: 1..100000
Hailstone/Collatz(77031) has the longest length = 351

(15:32) gp > ##
  ***   last result computed in 15,735 ms.

```



## Pascal

See [[Hailstone_sequence#Delphi | Delphi]]
or try this transformed Delphi version without generics.Use of a static array.

```pascal
program ShowHailstoneSequence;
{$IFDEF FPC}
  {$MODE delphi} //or objfpc
{$Else}
  {$Apptype Console} // for delphi
{$ENDIF}
uses
  SysUtils;// format
const
  maxN = 10*1000*1000;// for output 1000*1000*1000

type
  tiaArr = array[0..1000] of Uint64;
  tIntArr = record
               iaMaxPos : integer;
               iaArr    : tiaArr
            end;
  tpiaArr = ^tiaArr;

function HailstoneSeqCnt(n: UInt64): NativeInt;
begin
  result := 0;
  //ensure n to be odd
  while not(ODD(n)) do
  Begin
    inc(result);
    n := n shr 1;
  end;

  IF n > 1 then
  repeat
    //now n == odd -> so two steps in one can be made
    repeat
      n := (3*n+1) SHR 1;inc(result,2);
    until NOT(Odd(n));
    //now n == even -> so only one step can be made
    repeat
      n := n shr 1;      inc(result);
    until odd(n);
  until n = 1;
end;

procedure GetHailstoneSequence(aStartingNumber: NativeUint;var aHailstoneList: tIntArr);
var
  maxPos: NativeInt;
  n: UInt64;
  pArr : tpiaArr;
begin
  with aHailstoneList do
  begin
    maxPos := 0;
    pArr := @iaArr;
  end;
  n  := aStartingNumber;
  pArr^[maxPos] := n;
  while n <> 1 do
  begin
    if odd(n) then
      n := (3*n+1)
    else
      n := n shr 1;
    inc(maxPos);
    pArr^[maxPos] := n;
  end;
  aHailstoneList.iaMaxPos  := maxPos;
end;

var
  i,Limit: NativeInt;
  lList: tIntArr;
  lAverageLength:Uint64;
  lMaxSequence: NativeInt;
  lMaxLength,lgth: NativeInt;
begin
  lList.iaMaxPos := 0;
  GetHailstoneSequence(27, lList);//319804831
  with lList do
  begin
    Limit := iaMaxPos;
    writeln(Format('sequence of %d has %d  elements',[iaArr[0],Limit+1]));
    write(iaArr[0],',',iaArr[1],',',iaArr[2],',',iaArr[3],'..');
    For i := iaMaxPos-3 to iaMaxPos-1 do
       write(iaArr[i],',');
    writeln(iaArr[iaMaxPos]);
  end;
  Writeln;

  lMaxSequence := 0;
  lMaxLength := 0;
  i := 1;
  limit := 10*i;
  writeln(' Limit      : number with max length | average length');
  repeat
    lAverageLength:= 0;
    repeat
      lgth:= HailstoneSeqCnt(i);
      inc(lAverageLength, lgth);
      if lgth >= lMaxLength then
      begin
        lMaxSequence := i;
        lMaxLength := lgth+1;
      end;
      inc(i);
    until i = Limit;
    Writeln(Format(' %10d : %9d    |  %4d   |      %7.3f',
                   [limit,lMaxSequence, lMaxLength,0.9*lAverageLength/Limit]));
    limit := limit*10;
  until Limit > maxN;
end.
```

```txt
sequence of 27 has 112  elements
27,82,41,124..8,4,2,1

 Limit      : number with max length | average length
         10 :         9    |    20   |        5.490
        100 :        97    |   119   |       27.504
       1000 :       871    |   179   |       50.683
      10000 :      6171    |   262   |       71.119
     100000 :     77031    |   351   |       89.137
    1000000 :    837799    |   525   |      108.613
   10000000 :   8400511    |   686   |      127.916
  100000000 :  63728127    |   950   |      147.337
 1000000000 : 670617279    |   987   |      166.780

real  6m22.968s // 32-bit compiled
real  3m56.573s // 64-bit compiled
```



## Perl


###  Straightforward


```Perl
#!/usr/bin/perl

use warnings;
use strict;

my @h = hailstone(27);
print "Length of hailstone(27) = " . scalar @h . "\n";
print "[" . join(", ", @h[0 .. 3], "...", @h[-4 .. -1]) . "]\n";

my ($max, $n) = (0, 0);
for my $x (1 .. 99_999) {
    @h = hailstone($x);
    if (scalar @h > $max) {
        ($max, $n) = (scalar @h, $x);
    }
}

print "Max length $max was found for hailstone($n) for numbers < 100_000\n";


sub hailstone {
    my ($n) = @_;

    my @sequence = ($n);

    while ($n > 1) {
        if ($n % 2 == 0) {
            $n = int($n / 2);
        } else {
            $n = $n * 3 + 1;
        }

        push @sequence, $n;
    }

    return @sequence;
}
```


```txt

Length of hailstone(27) = 112
[27, 82, 41, 124, ..., 8, 4, 2, 1]
Max length 351 was found for hailstone(77031) for numbers < 100_000

```



###  Compact

A more compact version:

```Perl
#!/usr/bin/perl
use strict;

sub hailstone {
    @_ = local $_ = shift;
    push @_, $_ = $_ % 2 ? 3 * $_ + 1 : $_ / 2 while $_ > 1;
    @_;
}

my @h = hailstone($_ = 27);
print "$_: @h[0 .. 3] ... @h[-4 .. -1] (".@h.")\n";

@h = ();
for (1 .. 99_999) { @h = ($_, $h[2]) if ($h[2] = hailstone($_)) > $h[1] }
printf "%d: (%d)\n", @h;
```



```txt

27: 27 82 41 124 ... 8 4 2 1 (112)
77031: (351)

```



## Perl 6



```perl6
sub hailstone($n) { $n, { $_ %% 2 ?? $_ div 2 !! $_ * 3 + 1 } ... 1 }

my @h = hailstone(27);
say "Length of hailstone(27) = {+@h}";
say ~@h;

my $m = max ( (1..99_999).race.map: { +hailstone($_) => $_ } );
say "Max length {$m.key} was found for hailstone({$m.value}) for numbers < 100_000";
```


```txt

Length of hailstone(27) = 112
27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274 137 412 206 103 310 155 466 233 700 350 175 526 263 790 395 1186 593 1780 890 445 1336 668 334 167 502 251 754 377 1132 566 283 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 1619 4858 2429 7288 3644 1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154 577 1732 866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35 106 53 160 80 40 20 10 5 16 8 4 2 1
Max length 351 was found for hailstone(77031) for numbers < 100_000

```



## Phix

Copy of [[Hailstone_sequence#Euphoria|Euphoria]]

```Phix
function hailstone(atom n)
sequence s = {n}
    while n!=1 do
        if remainder(n,2)=0 then
            n /= 2
        else
            n = 3*n+1
        end if
        s &= n
    end while
    return s
end function

function hailstone_count(atom n)
integer count = 1
    while n!=1 do
        if remainder(n,2)=0 then
            n /= 2
        else
            n = 3*n+1
        end if
        count += 1
    end while
    return count
end function

sequence s = hailstone(27)
integer ls = length(s)
s[5..-5] = {".."}
puts(1,"hailstone(27) = ")
? s
printf(1,"length = %d\n\n",ls)

integer hmax = 1, imax = 1,count
for i=2 to 1e5-1 do
    count = hailstone_count(i)
    if count>hmax then
        hmax = count
        imax = i
    end if
end for

printf(1,"The longest hailstone sequence under 100,000 is %d with %d elements.\n",{imax,hmax})
```

```txt

hailstone(27) = {27,82,41,124,"..",8,4,2,1}
length = 112

The longest hailstone sequence under 100,000 is 77031 with 351 elements.

```




## PHP


```php
function hailstone($n,$seq=array()){
	$sequence = $seq;
	$sequence[] = $n;
	if($n == 1){
		return $sequence;
	}else{
		$n = ($n%2==0) ? $n/2 : (3*$n)+1;
		return hailstone($n, $sequence);
	}
}

$result = hailstone(27);
echo count($result) . ' Elements.
';
echo 'Starting with : ' . implode(",",array_slice($result,0,4)) .' and ending with : ' . implode(",",array_slice($result,count($result)-4)) . '
';

$maxResult = array(0);

for($i=1;$i<=100000;$i++){
		$result = count(hailstone($i));
		if($result > max($maxResult)){
			$maxResult = array($i=>$result);
		}
}
foreach($maxResult as $key => $val){
echo 'Number < 100000 with longest Hailstone seq.: ' . $key . ' with length of ' . $val;
}
```


```txt

112 Elements.
Starting with : 27,82,41,124 and ending with : 8,4,2,1
Number < 100000 with longest Hailstone seq.: 77031 with length of 351

```



## PicoLisp


```PicoLisp
(de hailstone (N)
   (make
      (until (= 1 (link N))
         (setq N
            (if (bit? 1 N)
               (inc (* N 3))
               (/ N 2) ) ) ) ) )

(let L (hailstone 27)
   (println 27 (length L) (head 4 L) '- (tail 4 L)) )

(let N (maxi '((N) (length (hailstone N))) (range 1 100000))
   (println N (length (hailstone N))) )
```

```txt
27 112 (27 82 41 124) - (8 4 2 1)
77031 351
```



## Pike


```Pike
#!/usr/bin/env pike

int next(int n)
{
    if (n==1)
        return 0;
    if (n%2)
        return 3*n+1;
    else
        return n/2;
}

array(int) hailstone(int n)
{
    array seq = ({ n });
    while (n=next(n))
        seq += ({ n });
    return seq;
}

void main()
{
    array(int) two = hailstone(27);
    if (equal(two[0..3], ({ 27, 82, 41, 124 })) && equal(two[<3..], ({ 8,4,2,1 })))
        write("sizeof(({ %{%d, %}, ... %{%d, %} }) == %d\n", two[0..3], two[<3..], sizeof(two));

    mapping longest = ([ "length":0, "start":0 ]);

    foreach(allocate(100000); int start; )
    {
        int length = sizeof(hailstone(start));
        if (length > longest->length)
        {
            longest->length = length;
            longest->start = start;
        }
    }
    write("longest sequence starting at %d has %d elements\n", longest->start, longest->length);
}
```


```txt

 sizeof(({ 27, 82, 41, 124, , ... 8, 4, 2, 1,  }) == 112
 longest sequence starting at 77031 has 351 elements

```



## PL/I


```pli
test: proc options (main);
   declare (longest, n) fixed (15);
   declare flag bit (1);
   declare (i, value) fixed (15);

   /* Task 1: */
   flag = '1'b;
   put skip list ('The sequence for 27 is');
   i = hailstones(27);

   /* Task 2: */
   flag = '0'b;
   longest = 0;
   do i = 1 to 99999;
      if longest < hailstones(i) then
         do; longest = hailstones(i); value = i; end;
   end;
   put skip edit (value, ' has the longest sequence of ', longest) (a);

hailstones: procedure (n) returns ( fixed (15));
   declare n fixed (15) nonassignable;
   declare (m, p) fixed (15);

   m = n;
   p = 1;
   if flag then put skip list (m);
   do p = 1 by 1 while (m > 1);
      if iand(m, 1) = 0 then
         m = m/2;
      else
         m = 3*m + 1;
      if flag then put skip list (m);
   end;
   if flag then put skip list ('The hailstone sequence has length' || p);
   return (p);
end hailstones;

end test;
```

<pre style="height:30ex;overflow:scroll">
The sequence for 27 is
                27
                82
                41
               124
                62
                31
                94
                47
               142
                71
               214
               107
               322
               161
               484
               242
               121
               364
               182
                91
               274
               137
               412
               206
               103
               310
               155
               466
               233
               700
               350
               175
               526
               263
               790
               395
              1186
               593
              1780
               890
               445
              1336
               668
               334
               167
               502
               251
               754
               377
              1132
               566
               283
               850
               425
              1276
               638
               319
               958
               479
              1438
               719
              2158
              1079
              3238
              1619
              4858
              2429
              7288
              3644
              1822
               911
              2734
              1367
              4102
              2051
              6154
              3077
              9232
              4616
              2308
              1154
               577
              1732
               866
               433
              1300
               650
               325
               976
               488
               244
               122
                61
               184
                92
                46
                23
                70
                35
               106
                53
               160
                80
                40
                20
                10
                 5
                16
                 8
                 4
                 2
                 1
The hailstone sequence has length               112
             77031 has the longest sequence of                351

```



## plainTeX

The following code works with any TeX engine.

```tex
\newif\ifprint
\newcount\itercount
\newcount\currentnum
\def\hailstone#1{\itercount=0 \currentnum=#1 \hailstoneaux}
\def\hailstoneaux{%
	\advance\itercount1
	\ifprint\number\currentnum\space\space\fi
	\ifnum\currentnum>1
		\ifodd\currentnum
			\multiply\currentnum3 \advance\currentnum1
		\else
			\divide\currentnum2
		\fi
		\expandafter\hailstoneaux
	\fi
}

\parindent=0pt
\printtrue\hailstone{27}
Length = \number\itercount
\bigbreak

\newcount\ii \ii=1
\printfalse
\def\lenmax{0}
\def\seed{0}
\loop
	\ifnum\ii<100000
		\hailstone\ii
		\ifnum\itercount>\lenmax\relax
			\edef\lenmax{\number\itercount}%
			\edef\seed{\number\ii}%
		\fi
		\advance\ii1
\repeat
Seed max = \seed, length = \lenmax
\bye
```


pdf or dvi output:

```txt
27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274 137 412 206
103 310 155 466 233 700 350 175 526 263 790 395 1186 593 1780 890 445 1336 668 334 167
502 251 754 377 1132 566 283 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 1619
4858 2429 7288 3644 1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154 577 1732
866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35 106 53 160 80 40 20 10 5 16
8 4 2 1 Length = 112

Seed max = 77031, length = 351

```



## PowerShell

```Powershell


function Get-HailStone {
    param($n)

    switch($n) {
        1              {$n;return}
        {$n % 2 -eq 0} {$n; return Get-Hailstone ($n = $n / 2)}
        {$n % 2 -ne 0} {$n; return Get-Hailstone ($n = ($n * 3) +1)}
    }
}

function Get-HailStoneBelowLimit {
    param($UpperLimit)

    for ($i = 1; $i -lt $UpperLimit; $i++) {
        [pscustomobject]@{
            'Number' = $i
            'Count' = (Get-HailStone $i).count
        }
    }
}
```


```txt
PS C:\> Get-HailStone 27
27
82
41
...
8
4
2
1

PS C:\> (Get-HailStone 27).count
112

PS C:\> Get-HailStoneBelowLimit 100000 | Sort Count -Descending | Select -first 1
Number         Count
------         -----
 77031           351
```



## Prolog

1. Create a routine to generate the hailstone sequence for a number.

```prolog
hailstone(1,[1]) :- !.
hailstone(N,[N|S]) :- 0 is N mod 2, N1 is N / 2, hailstone(N1,S).
hailstone(N,[N|S]) :- 1 is N mod 2, N1 is (3 * N) + 1, hailstone(N1, S).
```


2. Use the routine to show that the hailstone sequence for the number 27 has 112 elements starting with 27, 82, 41, 124 and ending with 8, 4, 2, 1.

The following query performs the test.

```prolog
hailstone(27,X),
length(X,112),
append([27, 82, 41, 124], _, X),
append(_, [8, 4, 2, 1], X).
```


3. Show the number less than 100,000 which has the longest hailstone sequence together with that sequences length.

```prolog
longestHailstoneSequence(M, Seq, Len) :- longesthailstone(M, 1, 1, Seq, Len).
longesthailstone(1, Cn, Cl, Mn, Ml):- Mn = Cn,
	                               Ml = Cl.
longesthailstone(N, _, Cl, Mn, Ml) :- hailstone(N, X),
                                       length(X, L),
                                       Cl < L,
                                       N1 is N-1,
                                       longesthailstone(N1, N, L, Mn, Ml).
longesthailstone(N, Cn, Cl, Mn, Ml) :- N1 is N-1,
                                       longesthailstone(N1, Cn, Cl, Mn, Ml).
```

run this query.

```prolog
longestHailstoneSequence(100000, Seq, Len).
```

to get the following result

```txt

Seq = 77031,
Len = 351

```



### Constraint Handling Rules

CHR is a programming language created by '''Professor Thom Frühwirth'''.

Works with SWI-Prolog and module '''chr''' written by '''Tom Schrijvers''' and '''Jan Wielemaker'''



```Prolog
:- use_module(library(chr)).
:- chr_option(debug, off).
:- chr_option(optimize, full).

:- chr_constraint collatz/2, hailstone/1, clean/0.

% to remove all constraints hailstone/1 after computation
clean @ clean \ hailstone(_) <=> true.
clean @ clean <=> true.

% compute Collatz number
init @ collatz(1,X) <=>  X = 1 | true.
collatz @ collatz(N, C) <=> (N mod 2 =:= 0 -> C is N / 2; C is 3 * N + 1).

% Hailstone loop
hailstone(1) ==> true.
hailstone(N) ==> N \= 1 | collatz(N, H), hailstone(H).
```


Code for task one :

```Prolog
task1 :-
	hailstone(27),
	findall(X, find_chr_constraint(hailstone(X)), L),
	clean,
	% check the requirements
	(   (length(L, 112), append([27, 82, 41, 124 | _], [8,4,2,1], L)) -> writeln(ok); writeln(ko)).
```

```txt
 ?- task1.
ok
true.
```

Code for task two :

```Prolog
longest_sequence :-
	seq(2, 100000, 1-[1], Len-V),
	format('For ~w sequence has ~w len ! ~n', [V, Len]).


% walk through 2 to 100000 and compute the length of the sequences
% memorize the longest
seq(N, Max, Len-V, Len-V) :- N is Max + 1, !.
seq(N, Max, CLen - CV, FLen - FV) :-
	len_seq(N, Len - N),
	(   Len > CLen -> Len1 = Len, V1 = [N]
	;   Len = CLen -> Len1 = Len, V1 = [N | CV]
	;   Len1 = CLen, V1 = CV),
	N1 is N+1,
	seq(N1, Max, Len1 - V1, FLen - FV).

% compute the len of the Hailstone sequence for a number
len_seq(N, Len - N) :-
	hailstone(N),
	findall(hailstone(X), find_chr_constraint(hailstone(X)), L),
	length(L, Len),
	clean.
```

```txt
 ?- longest_sequence.
For [77031] sequence has 351 len !
true.

```



## Pure


```pure
// 1. Create a routine to generate the hailstone sequence for a number.
type odd x::int = x mod 2;
type even x::int = ~odd x;
odd x = typep odd x;
even x = typep even x;

hailstone 1       = [1];
hailstone n::even = n:hailstone (n div 2);
hailstone n::odd  = n:hailstone (3*n + 1);

// 2. Use the routine to show that the hailstone sequence for the number 27
//    has 112 elements starting with 27, 82, 41, 124 and ending with 8, 4, 2, 1
n = 27;
hs = hailstone n;
l = # hs;
using system;

printf
    ("the hailstone sequence for the number %d has %d elements " +
     "starting with %s and ending with %s\n")
    (n, l, __str__ (hs!!(0..3)), __str__ ( hs!!((l-4)..l)));

// 3. Show the number less than 100,000 which has the longest hailstone
//    sequence together with that sequences length.
printf ("the number under 100,000 with the longest sequence is %d " +
        "with a sequence length of %d\n")
       (foldr (\ (a,b) (c,d) -> if (b > d) then (a,b) else (c,d))
             (0,0)
             (map (\ x -> (x, # hailstone x)) (1..100000)));
```

```txt

the hailstone sequence for the number 27 has 112 elements starting with [27,82,41,124] and ending with [8,4,2,1]
the number under 100,000 with the longest sequence is 77031 with a sequence length of 351

```



## Python


### Procedural


```python
def hailstone(n):
    seq = [n]
    while n>1:
        n = 3*n + 1 if n & 1 else n//2
        seq.append(n)
    return seq

if __name__ == '__main__':
    h = hailstone(27)
    assert len(h)==112 and h[:4]==[27, 82, 41, 124] and h[-4:]==[8, 4, 2, 1]
    print("Maximum length %i was found for hailstone(%i) for numbers <100,000" %
          max((len(hailstone(i)), i) for i in range(1,100000)))
```


```txt
Maximum length 351 was found for hailstone(77031) for numbers <100,000
```



### Composition of pure functions

```python
'''Hailstone sequences'''

from itertools import (islice, takewhile)


# hailstone :: Int -> [Int]
def hailstone(x):
    '''Hailstone sequence starting with x.'''
    def p(n):
        return 1 != n
    return list(takewhile(p, iterate(collatz)(x))) + [1]


# collatz :: Int -> Int
def collatz(n):
    '''Next integer in the hailstone sequence.'''
    return 3 * n + 1 if 1 & n else n // 2


# TEST ----------------------------------------------------

# main :: IO ()
def main():
    '''Tests.'''

    n = 27
    xs = hailstone(n)
    print(unlines([
        f'The hailstone sequence for {n} has {len(xs)} elements,',
        f'starting with {take(4)(xs)},',
        f'and ending with {drop(len(xs) - 4)(xs)}.\n'
    ]))

    (a, b) = (1, 99999)
    (i, x) = max(
        enumerate(
            map(compose(len)(hailstone), enumFromTo(a)(b))
        ),
        key=snd
    )
    print(unlines([
        f'The number in the range {a}..{b} '
        f'which produces the longest sequence is {1 + i},',
        f'generating a hailstone sequence of {x} integers.'
    ]))


# GENERIC ------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Function composition.'''
    return lambda f: lambda x: g(f(x))


# drop :: Int -> [a] -> [a]
# drop :: Int -> String -> String
def drop(n):
    '''The sublist of xs beginning at (zero-based) index n.'''
    def go(xs):
        if isinstance(xs, list):
            return xs[n:]
        else:
            take(n)(xs)
            return xs
    return lambda xs: go(xs)


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated applications of f to x.'''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


# snd :: (a, b) -> b
def snd(tpl):
    '''Second component of a tuple.'''
    return tpl[1]


# take :: Int -> [a] -> [a]
# take :: Int -> String -> String
def take(n):
    '''The prefix of xs of length n,
       or xs itself if n > length xs.'''
    return lambda xs: (
        xs[0:n]
        if isinstance(xs, list)
        else list(islice(xs, n))
    )


# unlines :: [String] -> String
def unlines(xs):
    '''A single newline-delimited string derived
       from a list of strings.'''
    return '\n'.join(xs)


if __name__ == '__main__':
    main()
```

```txt
The hailstone sequence for 27 has 112 elements,
starting with [27, 82, 41, 124],
and ending with [8, 4, 2, 1].

The number in the range 1..99999 which produces the longest sequence is 77031,
generating a hailstone sequence of 351 integers.
```



## R


```r
### PART 1:
makeHailstone <- function(n){
  hseq <- n
  while (hseq[length(hseq)] > 1){
    current.value <- hseq[length(hseq)]
    if (current.value %% 2 == 0){
      next.value <- current.value / 2
    } else {
      next.value <- (3 * current.value) + 1
    }
    hseq <- append(hseq, next.value)
  }
  return(list(hseq=hseq, seq.length=length(hseq)))
}

### PART 2:
twenty.seven <- makeHailstone(27)
twenty.seven$hseq
twenty.seven$seq.length

### PART 3:
max.length <- 0;  lower.bound <- 1;  upper.bound <- 100000

for (index in lower.bound:upper.bound){
  current.hseq <- makeHailstone(index)
  if (current.hseq$seq.length > max.length){
    max.length <- current.hseq$seq.length
    max.index  <- index
  }
}

cat("Between ", lower.bound, " and ", upper.bound, ", the input of ",
    max.index, " gives the longest hailstone sequence, which has length ",
    max.length, ". \n", sep="")
```


```txt
> twenty.seven$hseq
  [1]   27   82   41  124   62   31   94   47  142   71  214  107  322  161  484
 [16]  242  121  364  182   91  274  137  412  206  103  310  155  466  233  700
 [31]  350  175  526  263  790  395 1186  593 1780  890  445 1336  668  334  167
 [46]  502  251  754  377 1132  566  283  850  425 1276  638  319  958  479 1438
 [61]  719 2158 1079 3238 1619 4858 2429 7288 3644 1822  911 2734 1367 4102 2051
 [76] 6154 3077 9232 4616 2308 1154  577 1732  866  433 1300  650  325  976  488
 [91]  244  122   61  184   92   46   23   70   35  106   53  160   80   40   20
[106]   10    5   16    8    4    2    1

> twenty.seven$seq.length
[1] 112

Between 1 and 1e+05, the input of 77031 gives the longest hailstone sequence,
which has length 351.
```



## Racket


```Racket

#lang racket

(define hailstone
  (let ([t (make-hasheq)])
    (hash-set! t 1 '(1))
    (λ(n) (hash-ref! t n
            (λ() (cons n (hailstone (if (even? n) (/ n 2) (+ (* 3 n) 1)))))))))

(define h27 (hailstone 27))
(printf "h(27) = ~s, ~s items\n"
        `(,@(take h27 4) ... ,@(take-right h27 4))
        (length h27))

(define N 100000)
(define longest
  (for/fold ([m #f]) ([i (in-range 1 (add1 N))])
    (define h (hailstone i))
    (if (and m (> (cdr m) (length h))) m (cons i (length h)))))
(printf "for x<=~s, ~s has the longest sequence with ~s items\n"
        N (car longest) (cdr longest))

```


```txt

h(27) = (27 82 41 124 ... 8 4 2 1), 112 items
for x<=100000, 77031 has the longest sequence with 351 items

```



## REBOL


```rebol

hail: func [
	"Returns the hailstone sequence for n"
	n [integer!]
	/local seq
] [
	seq: copy reduce [n]
	while [n <> 1] [
		append seq n: either n % 2 == 0 [n / 2] [3 * n + 1]
	]
	seq
]

hs27: hail 27
print [
	"the hail sequence of 27 has length" length? hs27
	"and has the form " copy/part hs27 3 "..."
	back back back tail hs27
]

maxN: maxLen: 0
repeat n 99999 [
	if (len: length? hail n) > maxLen [
		maxN: n
		maxLen: len
	]
]

print [
	"the number less than 100000 with the longest hail sequence is"
	maxN "with length" maxLen
]
```


```txt
the hail sequence of 27 has length 112 and has the form  27 82 41 ... 4 2 1
the number less than 100000 with the longest hail sequence is 77031 with length 351
```



## REXX

===non-optimized===

```REXX
/*REXX program tests a  number  and also a  range for  hailstone  (Collatz)  sequences. */
numeric digits 20                                /*be able to handle gihugeic numbers.  */
parse arg x y .                                  /*get optional arguments from the C.L. */
if x=='' | x==","   then x=     27               /*No  1st  argument?  Then use default.*/
if y=='' | y==","   then y= 100000 - 1           /* "  2nd      "        "   "     "    */
$=hailstone(x)      /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒task 1▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒*/
say  x   ' has a hailstone sequence of '         words($)
say      '    and starts with: '                 subword($, 1, 4)    " ∙∙∙"
say      '    and  ends  with:  ∙∙∙'             subword($, max(5, words($)-3))
if y==0  then exit  /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒task 2▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒*/
say
w=0;          do j=1  for y                      /*traipse through the range of numbers.*/
              call hailstone  j                  /*compute the hailstone sequence for J.*/
              if #hs<=w  then iterate            /*Not big 'nuff?   Then keep traipsing.*/
              bigJ=j;    w=#hs                   /*remember what # has biggest hailstone*/
              end   /*j*/
say '(between 1 ──►'   y") "        bigJ       ' has the longest hailstone sequence: '   w
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
hailstone: procedure expose #hs; parse arg n 1 s /*N and S: are set to the 1st argument.*/
                     do #hs=1   while  n\==1     /*keep loop while   N   isn't  unity.  */
                     if n//2  then n=n*3 + 1     /*N is odd ?   Then calculate  3*n + 1 */
                              else n=n%2         /*"  " even?   Then calculate  fast ÷  */
                     s=s n                       /* [↑]  %   is REXX integer division.  */
                     end   /*#hs*/               /* [↑]  append  N  to the sequence list*/
           return s                              /*return the  S  string to the invoker.*/
```

'''output'''   when using the default inputs:

```txt

27  has a hailstone sequence of  112
    and starts with:  27 82 41 124  ∙∙∙
    and  ends  with:  ∙∙∙ 8 4 2 1

(between 1 ──► 99999)  77031  has the longest hailstone sequence:  351

```



### optimized

This version is over fifteen times faster than the previous (unoptimized) version.

It makes use of:
::::*   previously calculated Collatz sequences (memoization)
::::*   a faster method of determining if an integer is even

```REXX
/*REXX program tests a  number  and also a  range for  hailstone  (Collatz)  sequences. */
!.=0;     !.0=1;  !.2=1;  !.4=1;  !.6=1;  !.8=1  /*assign even numerals to be  "true".  */
numeric digits 20;  @.=0                         /*handle big numbers; initialize array.*/
parse arg x y z .;  !.h=y                        /*get optional arguments from the C.L. */
if x=='' | x==","   then x=    27                /*No  1st  argument?  Then use default.*/
if y=='' | y==","   then y=100000 - 1            /* "  2nd      "        "   "     "    */
if z=='' | z==","   then z=    12                /*head/tail number?     "   "     "    */
hm=max(y, 40000)                                 /*use memoization (maximum num for  @.)*/
$=hailstone(x)      /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒task 1▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒*/
say  x   ' has a hailstone sequence of '         words($)
say      '    and starts with: '                 subword($, 1, z)    " ∙∙∙"
say      '    and  ends  with:  ∙∙∙'             subword($, max(z+1, words($)-z+1))
if y==0  then exit  /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒task 2▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒*/
say
w=0;         do j=1  for y;  $=hailstone(j)      /*traipse through the range of numbers.*/
             #hs=words($)                        /*find the length of the hailstone seq.*/
             if #hs<=w  then iterate             /*Not big enough?  Then keep traipsing.*/
             bigJ=j;    w=#hs                    /*remember what # has biggest hailstone*/
             end   /*j*/
say '(between 1 ──►'   y") "        bigJ       ' has the longest hailstone sequence: '   w
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
hailstone: procedure expose @. !. hm;  parse arg n 1 s 1 o,@.1  /*N,S,O: are the 1st arg*/
                       do  while @.n==0          /*loop while the residual is unknown.  */
                       parse var  n  ''  -1  L   /*extract the last decimal digit of  N.*/
                       if !.L  then n=n%2        /*N is even?   Then calculate  fast ÷  */
                               else n=n*3 + 1    /*"  " odd ?     "      "      3*n + 1 */
                       s=s  n                    /* [↑]  %: is the REXX integer division*/
                       end   /*while*/           /* [↑]  append  N  to the sequence list*/
           s=s  @.n                              /*append the number to a sequence list.*/
           @.o=subword(s, 2);    parse var s _ r /*use memoization for this hailstone #.*/
              do  while r\=='';  parse var r _ r /*obtain the next  hailstone sequence. */
              if @._\==0  then leave             /*Was number already found?  Return  S.*/
              if _>hm     then iterate           /*Is  number  out of range?  Ignore it.*/
              @._=r                              /*assign subsequence number to array.  */
              end   /*while*/
           return s
```

```txt

27  has a hailstone sequence of  112
    and starts with:  27 82 41 124 62 31 94 47 142 71 214 107  ∙∙∙
    and  ends  with:  ∙∙∙ 53 160 80 40 20 10 5 16 8 4 2 1

(between 1─► 99999)  77031  has the longest hailstone sequence:  351

```

'''output'''   when using the inputs:   <tt> ,   1000000 </tt>

```txt

27  has a hailstone sequence of  112
    and starts with:  27 82 41 124 62 31 94 47 142 71 214 107  ∙∙∙
    and  ends  with:  ∙∙∙ 53 160 80 40 20 10 5 16 8 4 2 1

(between 1 ──► 1000000)  837799  has the longest hailstone sequence:  525

```



## Ring


```ring

size = 27
aList = []
hailstone(size)

func hailstone n
     add(aList,n)
     while n != 1
           if n % 2 = 0  n = n / 2
           else n = 3 * n + 1 ok
           add(aList, n)
     end
     see "first 4 elements : "
     for i = 1 to 4
         see "" + aList[i]  + " "
     next
     see nl
     see "last 4 elements : "
     for i = len(aList) - 3 to len(aList)
         see "" + aList[i] + " "
     next

```



## Ruby

This program uses new methods (Integer#even? and Enumerable#max_by) from Ruby 1.8.7.
```ruby
def hailstone n
  seq = [n]
  until n == 1
    n = (n.even?) ? (n / 2) : (3 * n + 1)
    seq << n
  end
  seq
end

puts "for n = 27, show sequence length and first and last 4 elements"
hs27 = hailstone 27
p [hs27.length, hs27[0..3], hs27[-4..-1]]

# find the longest sequence among n less than 100,000
n = (1 ... 100_000).max_by{|n| hailstone(n).length}
puts "#{n} has a hailstone sequence length of #{hailstone(n).length}"
puts "the largest number in that sequence is #{hailstone(n).max}"
```

```txt

for n = 27, show sequence length and first and last 4 elements
[112, [27, 82, 41, 124], [8, 4, 2, 1]]
77031 has a hailstone sequence length of 351
the largest number in that sequence is 21933016

```



###  With shared structure

This version builds some linked lists with shared structure. ''Hailstone::ListNode'' is an adaptation of ListNode from [[Singly-linked list/Element definition#Ruby]].
When two sequences contain the same value, those two lists share a tail.
This avoids recomputing the end of the sequence.
```ruby
module Hailstone
  ListNode = Struct.new(:value, :size, :succ) do
    def each
      node = self
      while node
        yield node.value
        node = node.succ
      end
    end
  end

  @@sequence = {1 => ListNode[1,1]}

  module_function

  def sequence(n)
    unless @@sequence[n]
      m, ary = n, []
      until succ = @@sequence[m]
        ary << m
        m = m.even? ? (m / 2) : (3 * m + 1)
      end
      ary.reverse_each do |m|
        @@sequence[m] = succ = ListNode[m, succ.size + 1, succ]
      end
    end
    @@sequence[n]
  end
end

puts "for n = 27, show sequence length and first and last 4 elements"
hs27 = Hailstone.sequence(27).entries
p [hs27.size, hs27[0..3], hs27[-4..-1]]

# find the longest sequence among n less than 100,000
n = (1 ... 100_000).max_by{|n| Hailstone.sequence(n).size}
puts "#{n} has a hailstone sequence length of #{Hailstone.sequence(n).size}"
puts "the largest number in that sequence is #{Hailstone.sequence(n).max}"
```

output is the same as the above.


## Rust


```rust
fn hailstone(start : u32) -> Vec<u32> {
    let mut res = Vec::new();
    let mut next = start;

    res.push(start);

    while next != 1  {
        next = if next % 2 == 0 { next/2 } else { 3*next+1 };
        res.push(next);
    }
    res
}


fn main() {
    let test_num = 27;
    let test_hailseq = hailstone(test_num);

    println!("For {} number of elements is {} ", test_num, test_hailseq.len());

    let fst_slice = test_hailseq[0..4].iter()
                        .fold("".to_owned(), |acc, i| { acc + &*(i.to_string()).to_owned() + ", " });
    let last_slice = test_hailseq[test_hailseq.len()-4..].iter()
                        .fold("".to_owned(), |acc, i| { acc + &*(i.to_string()).to_owned() + ", " });

    println!("  hailstone starting with {} ending with {} ", fst_slice, last_slice);

    let max_range = 100000;
    let mut max_len = 0;
    let mut max_seed = 0;
    for i_seed in 1..max_range {
        let i_len = hailstone(i_seed).len();

        if i_len > max_len {
            max_len = i_len;
            max_seed = i_seed;
        }
    }
    println!("Longest sequence is {} element long for seed {}", max_len, max_seed);
}
```

```txt
For 27 number of elements is 112
  hailstone starting with 27, 82, 41, 124,  ending with 8, 4, 2, 1,
Longest sequence is 351 element long for seed 77031
```



=={{header|S-lang}}==
<lang S-lang>% lst=1, return list of elements; lst=0 just return length
define hailstone(n, lst)
{
  variable l;
  if (lst) l = {n};
  else l = 1;

  while (n > 1) {
    if (n mod 2)
      n = 3 * n + 1;
    else
      n /= 2;
    if (lst)
      list_append(l, n);
    else
      l++;
    % if (prn) () = printf("%d, ", n);
  }
  % if (prn) () = printf("\n");
  return l;
}

variable har = list_to_array(hailstone(27, 1)), more = 0;
() = printf("Hailstone(27) has %d elements starting with:\n\t", length(har));

foreach $1 (har[[0:3]])
  () = printf("%d, ", $1);

() = printf("\nand ending with:\n\t");
foreach $1 (har[[length(har)-4:]]) {
  if (more) () = printf(", ");
  more = printf("%d", $1);
}

() = printf("\ncalculating...\r");
variable longest, longlen = 0, h;
_for $1 (2, 99999, 1) {
  $2 = hailstone($1, 0);
  if ($2 > longlen) {
    longest = $1;
    longlen = $2;
    () = printf("longest sequence started w/%d and had %d elements  \r", longest, longlen);
  }
}
() = printf("\n");
```

```txt
Hailstone(27) has 112 elements starting with:
        27, 82, 41, 124,
and ending with:
        8, 4, 2, 1
longest sequence started w/77031 and had 351 elements
```



## SAS


```SAS

* Create a routine to generate the hailstone sequence for one number;
%macro gen_seq(n);
   data hailstone;
      array hs_seq(100000);
      n=&n;
      do until (n=1);
         seq_size + 1;
         hs_seq(seq_size) = n;
         if mod(n,2)=0 then n=n/2;
         else n=(3*n)+1;
      end;
	  seq_size + 1;
      hs_seq(seq_size)=n;
	  call symputx('seq_length',seq_size);
   run;

   proc sql;
      title "First and last elements of Hailstone Sequence for number &n";
	  select seq_size as sequence_length, hs_seq1, hs_seq2, hs_seq3, hs_seq4
			%do i=&seq_length-3 %to &seq_length;
				, hs_seq&i
			%end;
		from hailstone;
	quit;
%mend;

* Use the routine to output the first and last four numbers in the sequence for 27;
%gen_seq(27);

* Show the number less than 100,000 which has the longest hailstone sequence, and what that length is ;
%macro longest_hailstone(start_num, end_num);
	data hailstone_analysis;
	  do start=&start_num to &end_num;
	    n=start;
		length_of_sequence=1;
		do while (n>1);
		  length_of_sequence+1;
		  if mod(n,2)=0 then n=n/2;
		  else n=(3*n) + 1;
		end;
		output;
	  end;
	run;

	proc sort data=hailstone_analysis;
	  by descending length_of_sequence;
	run;

	proc print data=hailstone_analysis (obs=1) noobs;
	  title "Number from &start_num to &end_num with longest Hailstone sequence";
	  var start length_of_sequence;
	run;
%mend;
%longest_hailstone(1,99999);

```


```txt

                   First and last elements of Hailstone Sequence for number 27
    sequence_
       length   hs_seq1   hs_seq2   hs_seq3   hs_seq4  hs_seq109  hs_seq110  hs_seq111  hs_seq112
-------------------------------------------------------------------------------------------------
          112        27        82        41       124          8          4          2          1

                      Number from 1 to 99999 with longest Hailstone sequence
                                            length_of_
                                    start     sequence
                                    77031        351

```



## Scala

```Scala
object HailstoneSequence extends App {
  def hailstone(n: Int): Stream[Int] =
    n #:: (if (n == 1) Stream.empty else hailstone(if (n % 2 == 0) n / 2 else n * 3 + 1))

  val nr = args.headOption.map(_.toInt).getOrElse(27)
  val collatz = hailstone(nr)
  println(s"Use the routine to show that the hailstone sequence for the number: $nr.")
  println(collatz.toList)
  println(s"It has ${collatz.length} elements.")
  println
  println(
    "Compute the number < 100,000, which has the longest hailstone sequence with that sequence's length.")
  val (n, len) = (1 until 100000).map(n => (n, hailstone(n).length)).maxBy(_._2)
  println(s"Longest hailstone sequence length= $len occurring with number $n.")
}
```

```txt
Use the routine to show that the hailstone sequence for the number: 27.
List(27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1)
It has 112 elements.

Compute the number < 100,000, which has the longest hailstone sequence with that sequence's length.
Longest hailstone sequence length= 351 occurring with number 77031.
```



## Scheme


```scheme
(define (collatz n)
(if (= n 1) '(1)
(cons n (collatz (if (even? n) (/ n 2) (+ 1 (* 3 n)))))))

(define (collatz-length n)
(let aux ((n n) (r 1)) (if (= n 1) r
(aux (if (even? n) (/ n 2) (+ 1 (* 3 n))) (+ r 1)))))

(define (collatz-max a b)
(let aux ((i a) (j 0) (k 0))
(if (> i b) (list j k)
(let ((h (collatz-length i)))
(if (> h k) (aux (+ i 1) i h) (aux (+ i 1) j k))))))

(collatz 27)
; (27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182
; 91 274 137 412 206 103 310 155 466 233 700 350 175 526 263 790 395
; 1186 593 1780 890 445 1336 668 334 167 502 251 754 377 1132 566 283
; 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 1619 4858 2429
; 7288 3644 1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154
; 577 1732 866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35
; 106 53 160 80 40 20 10 5 16 8 4 2 1)

(collatz-length 27)
; 112

(collatz-max 1 100000)
; (77031 351)
```



## Scilab

<lang>function x=hailstone(n)
    // iterative definition
    // usage: global verbose; verbose=%T; hailstone(27)
    global verbose
    x=0; loop=%T
    while(loop)
        x=x+1
        if verbose then
            printf('%i ',n)
        end
        if n==1 then
            loop=%F
        elseif modulo(n,2)==1 then
            n=3*n+1
        else
            n=n/2
        end
    end
endfunction

global verbose;
verbose=1;
N=hailstone(27);
printf('\n\n%i\n',N);

global verbose;
verbose=0;
N=100000;
M=zeros(N,1);
for k=1:N
  M(k)=hailstone(k);
end;
[maxLength,n]=max(M)
```

```txt
27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274 137 412 206 103 310 155 466 233 700 350 175 526 263 790 395 1186 593 1780 890 445 1336 668 334 167 502 251 754 377 1132 566 283 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 1619 4858 2429 7288 3644 1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154 577 1732 866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35 106 53 160 80 40 20 10 5 16 8 4 2 1
112
 n  =      77031.
 maxLength  =      351.
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func array integer: hailstone (in var integer: n) is func
  result
    var array integer: hSequence is 0 times 0;
  begin
    while n <> 1 do
      hSequence &:= n;
      if odd(n) then
        n := 3 * n + 1;
      else
        n := n div 2;
      end if;
    end while;
    hSequence &:= n;
  end func;

const func integer: hailstoneSequenceLength (in var integer: n) is func
  result
    var integer: sequenceLength is 1;
  begin
    while n <> 1 do
      incr(sequenceLength);
      if odd(n) then
        n := 3 * n + 1;
      else
        n := n div 2;
      end if;
    end while;
  end func;

const proc: main is func
  local
    var integer: number is 0;
    var integer: length is 0;
    var integer: maxLength is 0;
    var integer: numberOfMaxLength is 0;
    var array integer: h27 is 0 times 0;
  begin
    for number range 1 to 99999 do
      length := hailstoneSequenceLength(number);
      if length > maxLength then
        maxLength := length;
        numberOfMaxLength := number;
      end if;
    end for;
    h27 := hailstone(27);
    writeln("hailstone(27):");
    for number range 1 to 4 do
      write(h27[number] <& ", ");
    end for;
    write("....");
    for number range length(h27) -3 to length(h27) do
      write(", " <& h27[number]);
    end for;
    writeln("  length=" <& length(h27));
    writeln("Maximum length " <& maxLength <& " at number=" <& numberOfMaxLength);
  end func;
```


```txt

hailstone(27):
27, 82, 41, 124, ...., 8, 4, 2, 1  length=112
Maximum length 351 at number=77031

```



## Sidef


```ruby
func hailstone (n) {
    var sequence = [n]
    while (n > 1) {
        sequence << (
            n.is_even ? n.div!(2)
                      : n.mul!(3).add!(1)
        )
    }
    return(sequence)
}
 
# The hailstone sequence for the number 27
var arr = hailstone(var nr = 27)
say "#{nr}: #{arr.first(4)} ... #{arr.last(4)} (#{arr.len})"
 
# The longest hailstone sequence for a number less than 100,000
var h = [0, 0]
for i (1 .. 99_999) {
    (var l = hailstone(i).len) > h[1] && (
        h = [i, l]
    )
}
 
printf("%d: (%d)\n", h...)
```



## Smalltalk

```smalltalk
Object subclass: Sequences [
  Sequences class >> hailstone: n [
      |seq|
      seq := OrderedCollection new.
      seq add: n.
      (n = 1) ifTrue: [ ^seq ].
      (n even) ifTrue: [ seq addAll: (Sequences hailstone: (n / 2)) ]
               ifFalse: [ seq addAll: (Sequences hailstone: ( (3*n) + 1 ) ) ].
      ^seq.
  ]

  Sequences class >> hailstoneCount: n [
      ^ (Sequences hailstoneCount: n num: 1)
  ]

  "this 'version' avoids storing the sequence, it just counts
   its length - no memoization anyway"
  Sequences class >> hailstoneCount: n num: m [
      (n = 1) ifTrue: [ ^m ].
      (n even) ifTrue: [ ^ Sequences hailstoneCount: (n / 2) num: (m + 1) ]
               ifFalse: [ ^ Sequences hailstoneCount: ( (3*n) + 1) num: (m + 1) ].
  ]
].
```



```smalltalk
|r|
r := Sequences hailstone: 27.  "hailstone 'from' 27"
(r size) displayNl.            "its length"

"test 'head' ..."
( (r first: 4) = #( 27  82  41  124 ) asOrderedCollection ) displayNl.

"... and 'tail'"
( ( (r last: 4 ) ) = #( 8 4 2 1 ) asOrderedCollection) displayNl.

|longest|
longest := OrderedCollection from: #( 1 1 ).
2 to: 100000 do: [ :c |
  |l|
  l := Sequences hailstoneCount: c.
  (l > (longest at: 2) ) ifTrue: [ longest replaceFrom: 1 to: 2 with: { c . l }  ].
].

('Sequence generator %1, sequence length %2' % { (longest at: 1) . (longest at: 2) })
   displayNl.
```



## SNUSP


```txt

   /@+@@@+++# 27
   |    halve odd   /===count<<\    /recurse\    #/?\ zero
$>@/===!/===-?\==>?!/-<+++\    \!/=!\@\>?!\@/<@\.!\-/
 /+<-\!>\?-<+>/++++<\?>+++/*6+4  |    |   \=/  \=itoa=@@@+@+++++#
 \=>?/<=!=\   |                  |    !     /+ !/+ !/+ !/+   \    mod10
        |//!==/
### ==
\         |    /<+> -\!?-\!?-\!?-\!?-\!
 /=>?\<=/\<+>!\->+>+<<?/>>=print@/\ln \?!\-?!\-?!\-?!\-?!\-?/\    div10
 \+<-/!<     ----------.++++++++++/      #  +/! +/! +/! +/! +/

```


## Swift


```Swift

func hailstone(var n:Int) -> [Int] {

    var arr = [n]

    while n != 1 {

        if n % 2 == 0 {
            n /= 2
        } else {
            n = (3 * n) + 1
        }

        arr.append(n)
    }

    return arr
}

let n = hailstone(27)

println("hailstone(27): \(n[0...3]) ... \(n[n.count-4...n.count-1]) for a count of \(n.count).")

var longest = (n: 1, len: 1)

for i in 1...100_000 {

    let new = hailstone(i)

    if new.count > longest.len {
        longest = (i, new.count)
    }
}

println("Longest sequence for numbers under 100,000 is with \(longest.n). Which has \(longest.len) items.")
```

```txt

hailstone(27): [27, 82, 41, 124] ... [8, 4, 2, 1] for a count of 112
Longest sequence for numbers under 100,000 is with 77031. Which has 351 items.

```



## Tcl

The core looping structure is an example of an [[Loops/N plus one half|n-plus-one-half loop]], except the loop is officially infinite here.

```tcl
proc hailstone n {
    while 1 {
	lappend seq $n
	if {$n == 1} {return $seq}
	set n [expr {$n & 1 ? $n*3+1 : $n/2}]
    }
}

set h27 [hailstone 27]
puts "h27 len=[llength $h27]"
puts "head4 = [lrange $h27 0 3]"
puts "tail4 = [lrange $h27 end-3 end]"

set maxlen [set max 0]
for {set i 1} {$i<100000} {incr i} {
    set l [llength [hailstone $i]]
    if {$l>$maxlen} {set maxlen $l;set max $i}
}
puts "max is $max, with length $maxlen"
```


```txt

h27 len=112
head4 = 27 82 41 124
tail4 = 8 4 2 1
max is 77031, with length 351

```


=={{header|TI-83 BASIC}}==

### Task 1


```ti83b
prompt N
N→M: 0→X: 1→L
While L=1
X+1→X
Disp M
If M=1
Then: 0→L
Else
If remainder(M,2)=1
Then: 3*M+1→M
Else: M/2→M
End
End
End
{N,X}
```

```txt
        10
         5
        16
         8
         4
         2
         1
{27,112}
```


### Task 2

As the calculator is quite slow, so the output is for N=200

```ti83b
prompt N
0→A:0→B
for(I,1,N)
I→M: 0→X: 1→L
While L=1
X+1→X
If M=1
Then: 0→L
Else
If remainder(M,2)=1
Then: 3*M+1→M
Else: M/2→M
End
End
End
If X>B: Then
I→A:X→B
End
Disp {I,X}
End
{A,B}
```

```txt
{171,125}
```



## TXR


```txr
@(do (defun hailstone (n)
       (cons n
             (gen (not (eq n 1))
                  (set n (if (evenp n)
                           (trunc n 2)
                           (+ (* 3 n) 1)))))))
@(next :list @(mapcar* (fun tostring) (hailstone 27)))
27
82
41
124
@(skip)
8
4
2
1
@(eof)
@(do (let ((max 0) maxi)
       (each* ((i (range 1 99999))
               (h (mapcar* (fun hailstone) i))
               (len (mapcar* (fun length) h)))
         (if (> len max)
           (progn
             (set max len)
             (set maxi i))))
       (format t "longest sequence is ~a for n = ~a\n" max maxi)))
```



```txt
$ txr -l hailstone.txr
longest sequence is 351 for n = 77031
```



## uBasic/4tH

<lang>' ------=< MAIN >=------

m = 0
Proc _hailstone_print(27)
Print

For x = 1 To 10000
    n = Func(_hailstone(x))
    If n > m Then
        t = x
        m = n
    EndIf
Next

Print  "The longest sequence is for "; t; ", it has a sequence length of "; m

End

_hailstone_print Param (1)
    ' print the number and sequence

    Local (1)
    b@ = 1

    Print "sequence for number "; a@
    Print Using "________"; a@;   'starting number

    Do While a@ # 1
        If (a@ % 2 ) = 1 Then
            a@ = a@ * 3 + 1   ' n * 3 + 1
        Else
            a@ = a@ / 2       ' n / 2
        EndIf

        b@ = b@ + 1
        Print Using "________"; a@;

        If (b@ % 10) = 0 Then Print
    Loop

    Print : Print
    Print "sequence length = "; b@
    Print

    For b@ = 0 To 79
      Print "-";
    Next

    Print
Return

_hailstone Param (1)
    ' normal version
    ' only counts the sequence

    Local (1)
    b@ = 1

    Do While a@ # 1
        If (a@ % 2) = 1 Then
            a@ = a@ * 3 + 1  ' n * 3 + 1
        Else
            a@ = a@ / 2      ' divide number by 2
        EndIf

        b@ = b@ + 1
    Loop

Return (b@)
```

uBasic is an interpreted language. Doing a sequence up to 100,000 would take over an hour, so we did up to 10,000 here.
```txt
sequence for number 27
      27      82      41     124      62      31      94      47     142      71
     214     107     322     161     484     242     121     364     182      91
     274     137     412     206     103     310     155     466     233     700
     350     175     526     263     790     395    1186     593    1780     890
     445    1336     668     334     167     502     251     754     377    1132
     566     283     850     425    1276     638     319     958     479    1438
     719    2158    1079    3238    1619    4858    2429    7288    3644    1822
     911    2734    1367    4102    2051    6154    3077    9232    4616    2308
    1154     577    1732     866     433    1300     650     325     976     488
     244     122      61     184      92      46      23      70      35     106
      53     160      80      40      20      10       5      16       8       4
       2       1

sequence length = 112

--------------------------------------------------------------------------------

The longest sequence is for 6171, it has a sequence length of 262

```



## UNIX Shell

The best way is to use a shell with built-in arrays and arithmetic, such as Bash.
```bash
#!/bin/bash
# seq is the array genereated by hailstone
# index is used for seq
declare -a seq
declare -i index

# Create a routine to generate the hailstone sequence for a number
hailstone () {
  unset seq index
  seq[$((index++))]=$((n=$1))
  while [ $n -ne 1 ]; do
    [ $((n % 2)) -eq 1 ] && ((n=n*3+1)) || ((n=n/2))
    seq[$((index++))]=$n
  done
}

# Use the routine to show that the hailstone sequence for the number 27
# has 112 elements starting with 27, 82, 41, 124 and ending with 8, 4, 2, 1
i=27
hailstone $i
echo "$i: ${#seq[@]}"
echo "${seq[@]:0:4} ... ${seq[@]:(-4):4}"

# Show the number less than 100,000 which has the longest hailstone
# sequence together with that sequences length.
# (But don't show the actual sequence)!
max=0
maxlen=0
for ((i=1;i<100000;i++)); do
  hailstone $i
  if [ $((len=${#seq[@]})) -gt $maxlen ]; then
    max=$i
    maxlen=$len
  fi
done

echo "${max} has a hailstone sequence length of ${maxlen}"
```


```txt
27: 112
27 82 41 124 ... 8 4 2 1
77031 has a hailstone sequence of 351
```



### Bourne Shell

This script follows tradition for the Bourne Shell; its hailstone() function writes the sequence to standard output, so the shell can capture or pipe this output.
This script is '''very slow''' because it forks many processes.
Each `command substitution` forks a subshell, and each expr(1) command forks a process.

* Therefore, this script only examines sequences '''from 1 to 1000''', not 100000. A fast computer might run this script in 45 to 120 seconds, using most time to run system calls in kernel mode. If the script went to 100000, it would need several hours.

```bash
# Outputs a hailstone sequence from $1, with one element per line.
# Clobbers $n.
hailstone() {
	n=`expr "$1" + 0`
	eval "test $? -lt 2 || return $?"  # $n must be integer.

	echo $n
	while test $n -ne 1; do
		if expr $n % 2 >/dev/null; then
			n=`expr 3 \* $n + 1`
		else
			n=`expr $n / 2`
		fi
		echo $n
	done
}

set -- `hailstone 27`
echo "Hailstone sequence from 27 has $# elements:"
first="$1, $2, $3, $4"
shift `expr $# - 4`
echo "  $first, ..., $1, $2, $3, $4"

i=1 max=0 maxlen=0
while test $i -lt 1000; do
	len=`hailstone $i | wc -l | tr -d ' '`
	test $len -gt $maxlen && max=$i maxlen=$len
	i=`expr $i + 1`
done
echo "Hailstone sequence from $max has $maxlen elements."
```


=
## C Shell
=
This script is several times faster than the previous Bourne Shell script, because it uses C Shell expressions, not the expr(1) command.
This script is '''slow''', but it can reach 100000, and a fast computer might run it in less than 15 minutes.


```csh
# Outputs a hailstone sequence from !:1, with one element per line.
# Clobbers $n.
alias hailstone eval \''@ n = \!:1:q		\\
	echo $n					\\
	while ( $n != 1 )			\\
		if ( $n % 2 ) then		\\
			@ n = 3 * $n + 1	\\
		else				\\
			@ n /= 2		\\
		endif				\\
		echo $n				\\
	end					\\
'\'

set sequence=(`hailstone 27`)
echo "Hailstone sequence from 27 has $#sequence elements:"
@ i = $#sequence - 3
echo "  $sequence[1-4] ... $sequence[$i-]"

# hailstone-length $i
#   acts like
# @ len = `hailstone $i | wc -l | tr -d ' '`
#   but without forking any subshells.
alias hailstone-length eval \''@ n = \!:1:q	\\
	@ len = 1				\\
	while ( $n != 1 )			\\
		if ( $n % 2 ) then		\\
			@ n = 3 * $n + 1	\\
		else				\\
			@ n /= 2		\\
		endif				\\
		@ len += 1			\\
	end					\\
'\'

@ i = 1
@ max = 0
@ maxlen = 0
while ($i < 100000)
	# XXX - I must run hailstone-length in a subshell, because my
	# C Shell has a bug when it runs hailstone-length inside this
	# while ($i < 1000) loop: it forgets about this loop, and
	# reports an error <<end: Not in while/foreach.>>
	@ len = `hailstone-length $i; echo $len`
	if ($len > $maxlen) then
		@ max = $i
		@ maxlen = $len
	endif
	@ i += 1
end
echo "Hailstone sequence from $max has $maxlen elements."
```


```txt
$ csh -f hailstone.csh
Hailstone sequence from 27 has 112 elements:
  27 82 41 124 ... 8 4 2 1
Hailstone sequence from 77031 has 351 elements.
```



## Ursa


### Implementation

<b>hailstone.u</b>

```ursa
import "math"

def hailstone (int n)
	decl int<> seq
	while (> n 1)
		append n seq
		if (= (mod n 2) 0)
			set n (floor (/ n 2))
		else
			set n (int (+ (* 3 n) 1))
		end if
	end while
	append n seq
	return seq
end hailstone
```



### Usage

```txt
> import "hailstone.u"
> out (hailstone 27) endl console
class java.lang.Integer<27, 82, 41, 124, 62, 31, 94, 47, 142, 71, 214, 107, 322, 161, 484, 242, 121, 364, 182, 91, 274, 137, 412, 206, 103, 310, 155, 466, 233, 700, 350, 175, 526, 263, 790, 395, 1186, 593, 1780, 890, 445, 1336, 668, 334, 167, 502, 251, 754, 377, 1132, 566, 283, 850, 425, 1276, 638, 319, 958, 479, 1438, 719, 2158, 1079, 3238, 1619, 4858, 2429, 7288, 3644, 1822, 911, 2734, 1367, 4102, 2051, 6154, 3077, 9232, 4616, 2308, 1154, 577, 1732, 866, 433, 1300, 650, 325, 976, 488, 244, 122, 61, 184, 92, 46, 23, 70, 35, 106, 53, 160, 80, 40, 20, 10, 5, 16, 8, 4, 2, 1>
> out (size (hailstone 27)) endl console
112
> decl int i max maxLoc
> for (set i 1) (< i 100000) (inc i)
..	decl int result
..	set result (size (hailstone i))
..
..	if (> result max)
..		set max result
..		set maxLoc i
..	end if
..end for
> out "hailstone(" maxLoc ")= " max endl console
hailstone(77031)= 351
> _
```



## Ursala


```Ursala
#import std
#import nat

hail = @iNC ~&h~=1->x ^C\~& @h ~&h?\~&t successor+ sum@iNiCX

#show+

main =

<
   ^T(@ixX take/$4; %nLP~~lrxPX; ^|TL/~& :/'...',' has length '--@h+ %nP+ length) hail 27,
   ^|TL(~&,:/' has sequence length ') %nP~~ nleq$^&r ^(~&,length+ hail)* nrange/1 100000>
```

The <code>hail</code> function computes the sequence as follows.
* Given a number as an argument, <code>@iNC</code> makes a list containing only that number before passing it to the rest of the function. The <code>i</code> in the expression stands for the identity function, <code>N</code> for the constant null function, and <code>C</code> for the cons operator.
* The iteration combinator (<code>-></code>) is used with a predicate of <code>~&h~=l</code> which tests the condition that the head (<code>~&h</code>) of its argument is not equal (<code>~=</code>) to 1. Iteration of the rest of the function continues while this predicate holds.
* The <code>x</code> suffix says to return the reversal of the list after the iteration finishes.
* The function being iterated builds a list using the cons operator (<code>^C</code>) with the identity function (<code>~&</code>) of the argument for the tail, and the result of the rest of the line for the head.
* The <code>@h</code> operator says that the function following will be applied to the head of the list.
* The conditional operator (<code>?</code>) has the head function (<code>~&h</code>) as its predicate, which tests whether the head of its argument is non-null.
* In this case, the argument is a natural number, but naturals are represented as lists of booleans, so taking the head of a number is the same as testing the least significant bit.
* If the condition is not met, the number has a 0 least significant bit, and therefore is even. In this case, the conditional predicate calls for taking its tail (<code>~&t</code>), effectively dividing it by 2 using a bit shift.
* If the condition is met, the number is odd, so the rest of the function computes the successor of the number multiplied by three.
* Rather than multiplying the hard way, the function <code>sum@iNiCX</code> computes the sum of the pair (<code>X</code>) of numbers given by the identity function (<code>i</code>) of the argument, and the doubling of the argument (<code>NiC</code>), also obtained by a bit shift, with a zero bit (<code>N</code>) consed (<code>C</code>) with the identity (<code>i</code>).
Most of the main expression pertains to less interesting printing and formatting, but the part that searches for the longest sequence in the range is <code>nleq$^&r ^(~&,length+ hail)* nrange/1 100000</code>.
* The expression <code>nrange/1 100000</code> evaluates to the list of the first 100000 positive integers.
* The map operator (<code>*</code>) causes a list to be made of the results of its operand applied to each number.
* The operand to the map operator, applied to an individual number in the list, constructs a pair (<code>^</code>) with the identity function (<code>~&</code>) of the number on the left, and the length of the <code>hail</code> sequence on the right.
* The maximizing operator (<code>$^</code>) with respect to the natural less or equal relation (<code>nleq</code>) applied to the right sides (<code>&r</code>) of its pair of arguments extracts the number with the maximum length sequence.

```txt
<27,82,41,124>...<8,4,2,1> has length 112
77031 has sequence length 351
```



## VBA

```vb
Private Function hailstone(ByVal n As Long) As Collection
    Dim s As New Collection
    s.Add CStr(n), CStr(n)
    i = 0
    Do While n <> 1
        If n Mod 2 = 0 Then
            n = n / 2
        Else
            n = 3 * n + 1
        End If
        s.Add CStr(n), CStr(n)
    Loop
    Set hailstone = s
End Function

Private Function hailstone_count(ByVal n As Long)
    Dim count As Long: count = 1
    Do While n <> 1
        If n Mod 2 = 0 Then
            n = n / 2
        Else
            n = 3 * n + 1
        End If
        count = count + 1
    Loop
    hailstone_count = count
End Function

Public Sub rosetta()
    Dim s As Collection, i As Long
    Set s = hailstone(27)
    Dim ls As Integer: ls = s.count
    Debug.Print "hailstone(27) = ";
    For i = 1 To 4
        Debug.Print s(i); ", ";
    Next i
    Debug.Print "... ";
    For i = s.count - 4 To s.count - 1
        Debug.Print s(i); ", ";
    Next i
    Debug.Print s(s.count)
    Debug.Print "length ="; ls
    Dim hmax As Long: hmax = 1
    Dim imax As Long: imax = 1
    Dim count As Integer
    For i = 2 To 100000# - 1
        count = hailstone_count(i)
        If count > hmax Then
            hmax = count
            imax = i
        End If
    Next i
    Debug.Print "The longest hailstone sequence under 100,000 is"; imax; "with"; hmax; "elements."
End Sub
```
```txt
hailstone(27) = 27, 82, 41, 124, ... 16, 8, 4, 2, 1
length = 112
The longest hailstone sequence under 100,000 is 77031 with 351 elements.
```


## VBScript


```vb

'function arguments: "num" is the number to sequence and "return" is the value to return - "s" for the sequence or
'"e" for the number elements.
Function hailstone_sequence(num,return)
    n = num
	sequence = num
	elements = 1
	Do Until n = 1
		If n Mod 2 = 0 Then
			n = n / 2
		Else
			n = (3 * n) + 1
		End If
		sequence = sequence & " " & n
		elements = elements + 1
	Loop
	Select Case return
		Case "s"
			hailstone_sequence = sequence
		Case "e"
			hailstone_sequence = elements
	End Select
End Function

'test driving.
'show sequence for 27
WScript.StdOut.WriteLine "Sequence for 27: " & hailstone_sequence(27,"s")
WScript.StdOut.WriteLine "Number of Elements: " & hailstone_sequence(27,"e")
WScript.StdOut.WriteBlankLines(1)
'show the number less than 100k with the longest sequence
count = 1
n_elements = 0
n_longest = ""
Do While count < 100000
	current_n_elements = hailstone_sequence(count,"e")
	If current_n_elements > n_elements Then
		n_elements = current_n_elements
		n_longest = "Number: " & count & " Length: " & n_elements
	End If
	count = count + 1
Loop
WScript.StdOut.WriteLine "Number less than 100k with the longest sequence: "
WScript.StdOut.WriteLine n_longest

```


```txt

Sequence for 27: 27 82 41 124 62 31 94 47 142 71 214 107 322 161 484 242 121 364 182 91 274 137 412 206 103 310 155 466 233 700 350 175 526 263 790 395 1186 593 1780 890 445 1336 668 334 167 502 251 754 377 1132 566 283 850 425 1276 638 319 958 479 1438 719 2158 1079 3238 1619 4858 2429 7288 3644 1822 911 2734 1367 4102 2051 6154 3077 9232 4616 2308 1154 577 1732 866 433 1300 650 325 976 488 244 122 61 184 92 46 23 70 35 106 53 160 80 40 20 10 5 16 8 4 2 1
Number of Elements: 112

Number less than 100k with the longest sequence:
Number: 77031 Length: 351

```



## Visual Basic

```vb
Option Explicit
Dim flag As Boolean ' true to print values
Sub main()
    Dim longest As Long, n As Long
    Dim i As Long, value As Long
    ' Task 1:
    flag = True
    i = 27
    Debug.Print "The hailstone sequence has length of "; i; " is "; hailstones(i)
    ' Task 2:
    flag = False
    longest = 0
    For i = 1 To 99999
        If longest < hailstones(i) Then
            longest = hailstones(i)
            value = i
        End If
    Next i
    Debug.Print value; " has the longest sequence of "; longest
End Sub 'main
Function hailstones(n As Long) As Long
    Dim m As Long, p As Long
    Dim m1 As Long, m2 As Long, m3 As Long, m4 As Long
    If flag Then Debug.Print "The sequence for"; n; "is: ";
    p = 1
    m = n
    If flag Then Debug.Print m;
    While m > 1
        p = p + 1
        If (m Mod 2) = 0 Then
            m = m / 2
        Else
            m = 3 * m + 1
        End If
        If p <= 4 Then If flag Then Debug.Print m;
        m4 = m3
        m3 = m2
        m2 = m1
        m1 = m
    Wend
    If flag Then
        If p <= 4 Then
            Debug.Print
        ElseIf p = 5 Then
            Debug.Print m1
        ElseIf p = 6 Then
            Debug.Print m2; m1
        ElseIf p = 7 Then
            Debug.Print m3; m2; m1
        ElseIf p = 8 Then
            Debug.Print m4; m3; m2; m1
        Else
            Debug.Print "..."; m4; m3; m2; m1
        End If
    End If
    hailstones = p
End Function 'hailstones
```

```txt
The sequence for 27 is:  27  82  41  124 ... 8  4  2  1
The hailstone sequence has length of  27  is  112
 77031  has the longest sequence of  351
```



## Visual Basic .NET

```vbnet
Module HailstoneSequence
    Sub Main()
        ' Checking sequence of 27.

        Dim l As List(Of Long) = HailstoneSequence(27)
        Console.WriteLine("27 has {0} elements in sequence:", l.Count())

        For i As Integer = 0 To 3 : Console.Write("{0}, ", l(i)) : Next
        Console.Write("... ")
        For i As Integer = l.Count - 4 To l.Count - 1 : Console.Write(", {0}", l(i)) : Next

        Console.WriteLine()

        ' Finding longest sequence for numbers below 100000.

        Dim max As Integer = 0
        Dim maxCount As Integer = 0

        For i = 1 To 99999
            l = HailstoneSequence(i)
            If l.Count > maxCount Then
                max = i
                maxCount = l.Count
            End If
        Next
        Console.WriteLine("Max elements in sequence for number below 100k: {0} with {1} elements.", max, maxCount)
        Console.ReadLine()
    End Sub

    Private Function HailstoneSequence(ByVal n As Long) As List(Of Long)
        Dim valList As New List(Of Long)()
        valList.Add(n)

        Do Until n = 1
            n = IIf(n Mod 2 = 0, n / 2, (3 * n) + 1)
            valList.Add(n)
        Loop

        Return valList
    End Function

End Module
```


```txt
27 has 112 elements in sequence:
27, 82, 41, 124, ... , 8, 4, 2, 1
Max elements in sequence for number below 100k: 77031 with 351 elements.

```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
int Seq(1000);          \more than enough for longest sequence

func Hailstone(N);      \Return length of Hailstone sequence starting at N
int  N;                 \ also fills Seq array with sequence
int  I;
[I:= 0;
loop [Seq(I):= N;  I:= I+1;
     if N=1 then return I;
     N:= if N&1 then N*3+1 else N/2;
     ];
];

int N, SN, Len, MaxLen;
[Len:= Hailstone(27);
Text(0, "27's Hailstone length = ");  IntOut(0, Len);  CrLf(0);

Text(0, "Sequence = ");
for N:= 0 to 3 do [IntOut(0, Seq(N));  ChOut(0, ^ )];
Text(0, "... ");
for N:= Len-4 to Len-1 do [IntOut(0, Seq(N));  ChOut(0, ^ )];
CrLf(0);

MaxLen:= 0;
for N:= 1 to 100_000-1 do
    [Len:= Hailstone(N);
    if Len > MaxLen then [MaxLen:= Len;  SN:= N];       \save N with max length
    ];
IntOut(0, SN);  Text(0, "'s Hailstone length = ");  IntOut(0, MaxLen);
]
```


```txt

27's Hailstone length = 112
Sequence = 27 82 41 124 ... 8 4 2 1
77031's Hailstone length = 351

```



## zkl


```zkl
fcn collatz(n,z=L()){ z.append(n); if(n==1) return(z);
   if(n.isEven) return(self.fcn(n/2,z)); return(self.fcn(n*3+1,z)) }
```

This uses tail recursion and thus is stack efficient.
```txt

var n=collatz(27)
n.len()
112
n[0,4]
L(27,82,41,124)
n[-4,*]
L(8,4,2,1)

```

Rather than write a function that calculates the length, just roll through all 100,000 sequences and save the largest (length,sequence start) pair. Creating all those Collatz lists isn't quick. This works by using a [mutable] list to hold state as the pump does the basic looping.

```zkl
[2..0d100_000].pump(Void,  // loop n from 2 to 100,000
   collatz,              // generate Collatz sequence(n)
   fcn(c,n){           // if new longest sequence, save length/C, return longest
      if(c.len()>n[0]) n.clear(c.len(),c[0]); n}.fp1(L(0,0)))
```

```txt

L(351,77031)  // length, hailstone

```



## ZX Spectrum Basic

```zxbasic
10 LET n=27: LET s=1
20 GO SUB 1000
30 PRINT '"Sequence length = ";seqlen
40 LET maxlen=0: LET s=0
50 FOR m=2 TO 100000
60 LET n=m
70 GO SUB 1000
80 IF seqlen>maxlen THEN LET maxlen=seqlen: LET maxnum=m
90 NEXT m
100 PRINT "The number with the longest hailstone sequence is ";maxnum
110 PRINT "Its sequence length is ";maxlen
120 STOP
1000 REM Hailstone
1010 LET l=0
1020 IF s THEN PRINT n;"  ";
1030 IF n=1 THEN LET seqlen=l+1: RETURN
1040 IF FN m(n,2)=0 THEN LET n=INT (n/2): GO TO 1060
1050 LET n=3*n+1
1060 LET l=l+1
1070 GO TO 1020
2000 DEF FN m(a,b)=a-INT (a/b)*b
```
