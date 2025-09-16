+++
title = "Generate Chess960 starting position"
description = ""
date = 2019-09-18T23:50:20Z
aliases = []
[extra]
id = 17594
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "autohotkey",
  "befunge",
  "c",
  "clojure",
  "cpp",
  "d",
  "echolisp",
  "elixir",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "objeck",
  "pari_gp",
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
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "tcl",
  "yabasic",
  "zkl",
]
+++

'''[[wp:Chess960|Chess960]]''' is a variant of chess created by world champion [[wp:Bobby Fischer|Bobby Fischer]].  Unlike other variants of the game, Chess960 does not require a different material, but instead relies on a random initial position, with a few constraints:

* as in the standard chess game, all eight white pawns must be placed on the second rank.
* White pieces must stand on the first rank as in the standard game, in random column order but with the two following constraints:
** the bishops must be placed on opposite color squares (i.e. they must be an odd number of spaces apart or there must be an even number of spaces between them)
** the King must be between two rooks (with any number of other pieces between them all)
* Black pawns and pieces must be placed respectively on the seventh and eighth ranks, mirroring the white pawns and pieces, just as in the standard game.  (That is, their positions are not independently randomized.)



With those constraints there are '''960''' possible starting positions, thus the name of the variant.


## Task

The purpose of this task is to write a program that can randomly generate any one of the 960 Chess960 initial positions. You will show the result as the first rank displayed with [[wp:Chess symbols in Unicode|Chess symbols in Unicode: ♔♕♖♗♘]] or with the letters '''K'''ing '''Q'''ueen '''R'''ook '''B'''ishop k'''N'''ight.




## AutoHotkey

```AutoHotkey
Loop, 5
	Out .= Chess960() "`n"
MsgBox, % RTrim(Out, "`n")

Chess960() {
	P := {}
	P[K := Rand(2, 7)] := Chr(0x2654)	; King
	P[Rand(1, K - 1)] := Chr(0x2656)	; Rook 1
	P[Rand(K + 1, 8)] := Chr(0x2656)	; Rook 2
	Loop, 8
		Remaining .= P[A_Index] ? "" : A_Index "`n"
	Sort, Remaining, Random N
	P[Bishop1 := SubStr(Remaining, 1, 1)] := Chr(0x2657)	; Bishop 1
	Remaining := SubStr(Remaining, 3)
	Loop, Parse, Remaining, `n
		if (Mod(Bishop1 - A_LoopField, 2))
			Odd .= A_LoopField "`n"
		else
			Even .= A_LoopField "`n"
	X := StrSplit(Odd Even, "`n")
	P[X.1] := Chr(0x2657)	; Bishop 2
	P[X.2] := Chr(0x2655)	; Queen
	P[X.3] := Chr(0x2658)	; Knight 1
	P[X.4] := Chr(0x2658)	; Knight 2
	for Key, Val in P
		Out .= Val
	return Out
}

Rand(Min, Max) {
	Random, n, Min, Max
	return n
}
```

<big><big>
```txt
♕♘♖♗♗♘♔♖
♗♖♔♕♘♖♘♗
♖♗♘♘♗♔♖♕
♗♗♘♖♔♕♘♖
♘♗♖♔♗♘♕♖
```
</big></big>


## Befunge

Similar to the [[Generate_Chess960_starting_position#Ruby:_Generate_from_SP-ID|Ruby SP-ID solution]], this generates the start position for a random number in the [[wp:Chess960 numbering scheme|Chess960 numbering scheme]].

```befunge
#.#.#.#.065*0#v_1-\>>?1v
v,":".:%*8"x"$<^!:\*2<+<
>48*,:4%2*1#v+#02#\3#g<<
v"B"*2%4:/4p<vg0:+1<\-1<
>\0p4/:6%0:0g>68*`#^_\:|
v"RKRNN"p11/6$p0\ "Q" \<
>"NRNKRRNNKRNRKNRRNKNR"v
v"NRNKRNRKNRNRKRNRNNKR"<
>"RKRNN"11g:!#v_\$\$\$\v
v _v#!`*86:g0:<^!:-1$\$<
>$\>,1+ :7`#@_^> v960v <
```

```txt
856 : RBKNBRNQ
```



## C

As noted in the C implementation for the [[Sparkline in unicode]] task, unicode output is reliable only on Linux/Unix systems. This implementation thus has compiler directives to check whether the underlying system is Windows or Linux, if Windows, only letters are printed, otherwise Unicode output is displayed. 9 rows are displayed.

```C
#include<stdlib.h>

#include<locale.h>
#include<wchar.h>
#include<stdio.h>
#include<time.h>

char rank[9];

int pos[8];

void swap(int i,int j){
	int temp = pos[i];
	pos[i] = pos[j];
	pos[j] = temp;
}

void generateFirstRank(){
	 int kPos,qPos,bPos1,bPos2,rPos1,rPos2,nPos1,nPos2,i;

	 for(i=0;i<8;i++){
		 rank[i] = 'e';
		 pos[i] = i;
	 }

	 do{
		 kPos = rand()%8;
		 rPos1 = rand()%8;
		 rPos2 = rand()%8;
	 }while((rPos1-kPos<=0 && rPos2-kPos<=0)||(rPos1-kPos>=0 && rPos2-kPos>=0)||(rPos1==rPos2 || kPos==rPos1 || kPos==rPos2));

	 rank[pos[rPos1]] = 'R';
	 rank[pos[kPos]] = 'K';
	 rank[pos[rPos2]] = 'R';

	 swap(rPos1,7);
	 swap(rPos2,6);
	 swap(kPos,5);

	 do{
		 bPos1 = rand()%5;
		 bPos2 = rand()%5;
	 }while(((pos[bPos1]-pos[bPos2])%2==0)||(bPos1==bPos2));

	 rank[pos[bPos1]] = 'B';
	 rank[pos[bPos2]] = 'B';

	 swap(bPos1,4);
	 swap(bPos2,3);

	 do{
		 qPos = rand()%3;
		 nPos1 = rand()%3;
	 }while(qPos==nPos1);

	 rank[pos[qPos]] = 'Q';
	 rank[pos[nPos1]] = 'N';

	 for(i=0;i<8;i++)
		 if(rank[i]=='e'){
			 rank[i] = 'N';
			 break;
		 }
}

void printRank(){
	int i;

	#ifdef _WIN32
		printf("%s\n",rank);
	#else
	{
		setlocale(LC_ALL,"");
		printf("\n");
		for(i=0;i<8;i++){
			if(rank[i]=='K')
				printf("%lc",(wint_t)9812);
			else if(rank[i]=='Q')
				printf("%lc",(wint_t)9813);
			else if(rank[i]=='R')
				printf("%lc",(wint_t)9814);
			else if(rank[i]=='B')
				printf("%lc",(wint_t)9815);
			if(rank[i]=='N')
				printf("%lc",(wint_t)9816);
		}
	}
	#endif
}

int main()
{
	int i;

	srand((unsigned)time(NULL));

	for(i=0;i<9;i++){
		generateFirstRank();
		printRank();
	}

	return 0;
}

```

Output on Linux :

```txt
♗♗♖♕♘♘♔♖
♘♖♕♔♗♗♖♘
♖♘♔♖♕♘♗♗
♘♘♖♗♗♔♖♕
♗♘♖♘♕♔♖♗
♕♗♗♘♖♔♘♖
♕♘♖♔♗♖♘♗
♗♘♘♕♖♔♖♗
♖♘♘♕♗♔♖♗
```

Output on Windows :

```txt
BRKNNQRB
RBNQNKBR
RNQKNBBR
RQNKNBBR
QBBNRKNR
BNRBQKNR
BRQKNNRB
RNKBNRBQ
QNRBBNKR
```



## C++


```cpp
#include <iostream>
#include <string>
#include <time.h>
using namespace std;

namespace
{
    void placeRandomly(char* p, char c)
    {
	int loc = rand() % 8;
	if (!p[loc])
	    p[loc] = c;
	else
	    placeRandomly(p, c);    // try again
    }
    int placeFirst(char* p, char c, int loc = 0)
    {
	while (p[loc]) ++loc;
	p[loc] = c;
        return loc;
    }

    string startPos()
    {
	char p[8]; memset( p, 0, 8 );

	// bishops on opposite color
	p[2 * (rand() % 4)] = 'B';
	p[2 * (rand() % 4) + 1] = 'B';

	// queen knight knight, anywhere
	for (char c : "QNN")
	    placeRandomly(p, c);

	// rook king rook, in that order
	placeFirst(p, 'R', placeFirst(p, 'K', placeFirst(p, 'R')));

	return string(p, 8);
    }
}   // leave local

namespace chess960
{
    void generate( int c )
    {
	for( int x = 0; x < c; x++ )
	    cout << startPos() << "\n";
    }
}

int main( int argc, char* argv[] )
{
    srand( time( NULL ) );
    chess960::generate( 10 );
    cout << "\n\n";
    return system( "pause" );
}

```

```txt
NQBRNBKR
RKBQNBNR
RKBRNNQB
QRBNNKRB
BRKNRBQN
QNRBBKNR
BQRBKNRN
RNBKQBNR
QRNKBBRN
QRBKNBRN
```



## Clojure


```clojure
(ns c960.core
  (:gen-class)
  (:require [clojure.string :as s]))

;; legal starting rank - unicode chars for rook, knight, bishop, queen, king, bishop, knight, rook
(def starting-rank [\♖ \♘ \♗ \♕ \♔ \♗ \♘ \♖])

(defn bishops-legal?
  "True if Bishops are odd number of indicies apart"
  [rank]
  (odd? (apply - (cons 0 (sort > (keep-indexed #(when (= \♗ %2) %1) rank))))))

(defn king-legal?
  "True if the king is between two rooks"
  [rank]
  (let [king-&-rooks (filter #{\♔ \♖} rank)]
    (and
     (= 3 (count king-&-rooks))
     (= \u2654 (second king-&-rooks)))))


(defn c960
  "Return a legal rank for c960 chess"
  ([] (c960 1))
  ([n]
   (->> #(shuffle starting-rank)
        repeatedly
        (filter #(and (king-legal? %) (bishops-legal? %)))
        (take n)
        (map #(s/join ", " %)))))


(c960)
;; => "♗, ♖, ♔, ♕, ♘, ♘, ♖, ♗"
(c960)
;; => "♖, ♕, ♘, ♔, ♗, ♗, ♘, ♖"
(c960 4)
;; => ("♘, ♖, ♔, ♘, ♗, ♗, ♖, ♕" "♗, ♖, ♔, ♘, ♘, ♕, ♖, ♗" "♘, ♕, ♗, ♖, ♔, ♗, ♘, ♖" "♖, ♔, ♘, ♘, ♕, ♖, ♗, ♗")
```



## D

### D: Indexing


```d
void main() {
    import std.stdio, std.range, std.algorithm, std.string, permutations2;

    const pieces = "KQRrBbNN";
    alias I = indexOf;
    auto starts = pieces.dup.permutations.filter!(p =>
            I(p, 'B') % 2 != I(p, 'b') % 2 && // Bishop constraint.
            // King constraint.
            ((I(p, 'r') < I(p, 'K') && I(p, 'K') < I(p, 'R')) ||
             (I(p, 'R') < I(p, 'K') && I(p, 'K') < I(p, 'r'))))
        .map!toUpper.array.sort().uniq;
    writeln(starts.walkLength, "\n", starts.front);
}
```

```txt
960
BBNNQRKR
```



### D: Regexp


```d
void main() {
    import std.stdio, std.regex, std.range, std.algorithm, permutations2;

    immutable pieces = "KQRRBBNN";
    immutable bish = r"B(|..|....|......)B";
    immutable king = r"R.*K.*R";
    auto starts3 = permutations(pieces.dup)
                   .filter!(p => p.match(bish) && p.match(king))
                   .array.sort().uniq;
    writeln(starts3.walkLength, "\n", starts3.front);
}
```

The output is the same.


### D: Correct by construction


```d
void main() {
    import std.stdio, std.random, std.array, std.range;

    // Subsequent order unchanged by insertions.
    auto start = "RKR".dup;
    foreach (immutable piece; "QNN")
        start.insertInPlace(uniform(0, start.length), piece);

    immutable bishpos = uniform(0, start.length);
    start.insertInPlace(bishpos, 'B');
    start.insertInPlace(iota(bishpos % 2, start.length, 2)[uniform(0,$)], 'B');
    start.writeln;
}
```

```txt
QBNNBRKR
```



## EchoLisp


```lisp
(define-values (K Q R B N) (iota 5))
(define *pos* (list R N B Q K B N R)) ;; standard starter

;; check opposite color bishops, and King between rooks
(define (legal-pos p)
    (and
            (> (list-index K p) (list-index R p))
            (> (list-index K (reverse p)) (list-index R (reverse p)))
            (even? (+ (list-index B p) (list-index B (reverse p))))))

;; random shuffle current position until a legal one is found
(define (c960)
	(set! *pos* (shuffle *pos*))
	(if (legal-pos *pos*)
		(map unicode-piece *pos*)  (c960)))

```

```txt
(define (unicode-piece i) (unicode->string (+ 0x2654 i)))

(legal-pos *pos*) → #t ;; starter is OK
(c960)
 (♗ ♖ ♔ ♗ ♕ ♘ ♘ ♖)
(c960)
 (♘ ♗ ♗ ♕ ♖ ♘ ♔ ♖)
(c960)
 (♖ ♘ ♗ ♘ ♔ ♕ ♖ ♗)
;; etc.
```



## Elixir

### Elixir: shuffle pieces until all regexes match


```elixir
defmodule Chess960 do
  @pieces   ~w(♔ ♕ ♘ ♘ ♗ ♗ ♖ ♖)             # ~w(K Q N N B B R R)
  @regexes  [~r/♗(..)*♗/, ~r/♖.*♔.*♖/]        # [~r/B(..)*B/, ~r/R.*K.*R/]

  def shuffle do
    row = Enum.shuffle(@pieces) |> Enum.join
    if Enum.all?(@regexes, &Regex.match?(&1, row)), do: row, else: shuffle
  end
end

Enum.each(1..5, fn _ -> IO.puts Chess960.shuffle end)
```

```txt
♘♗♘♖♗♔♕♖
♗♖♔♗♕♘♘♖
♗♗♕♖♔♖♘♘
♘♗♖♔♗♘♕♖
♖♕♘♘♗♗♔♖
```



### Elixir: Construct


```elixir
defmodule Chess960 do
  def construct do
    row = Enum.reduce(~w[♕ ♘ ♘], ~w[♖ ♔ ♖], fn piece,acc ->
            List.insert_at(acc, :rand.uniform(length(acc)+1)-1, piece)
          end)
    [Enum.random([0, 2, 4, 6]), Enum.random([1, 3, 5, 7])]
    |> Enum.sort
    |> Enum.reduce(row, fn pos,acc -> List.insert_at(acc, pos, "♗") end)
    |> Enum.join
  end
end

Enum.each(1..5, fn _ -> IO.puts Chess960.construct end)
```

```txt
♖♔♗♘♖♕♘♗
♘♗♘♕♖♔♗♖
♗♖♔♘♘♗♖♕
♖♗♘♘♕♔♗♖
♖♕♗♘♘♗♔♖
```


===Elixir: Generate from SP-ID===

```elixir
defmodule Chess960 do
  @krn  ~w(NNRKR NRNKR NRKNR NRKRN RNNKR RNKNR RNKRN RKNNR RKNRN RKRNN)

  def start_position, do: start_position(:rand.uniform(960)-1)

  def start_position(id) do
    pos = List.duplicate(nil, 8)
    q = div(id, 4)
    r = rem(id, 4)
    pos = List.replace_at(pos, r * 2 + 1, "B")
    q = div(q, 4)
    r = rem(q, 4)
    pos = List.replace_at(pos, r * 2, "B")
    q = div(q, 6)
    r = rem(q, 6)
    i = Enum.reject(0..7, &Enum.at(pos,&1)) |> Enum.at(r)
    pos = List.replace_at(pos, i, "Q")
    krn = Enum.at(@krn, q) |> String.codepoints
    Enum.reject(0..7, &Enum.at(pos,&1))
    |> Enum.zip(krn)
    |> Enum.reduce(pos, fn {i,x},acc -> List.replace_at(acc,i,x) end)
    |> Enum.join
  end
end

IO.puts "Generate Start Position from ID number"
Enum.each([0,518,959], fn id ->
  :io.format "~3w : ~s~n", [id, Chess960.start_position(id)]
end)
IO.puts "\nGenerate random Start Position"
Enum.each(1..5, fn _ -> IO.puts Chess960.start_position end)
```

```txt
Generate Start Position from ID number
  0 : BBQNNRKR
518 : BRNKNBRQ
959 : RKRQNNBB

Generate random Start Position
RQKBBNNR
RBBQKNNR
RQKNNRBB
RKRQBBNN
RNBNKQRB
```



## Factor


### Single die method

Using the single die method: https://en.wikipedia.org/wiki/Chess960_starting_position#Single_die_method

```factor
USING: io kernel math random sequences ;
IN: rosetta-code.chess960

: empty ( seq -- n ) 32 swap indices random ;           ! return a random empty index (i.e. equal to 32) of seq
: next ( seq -- n ) 32 swap index ;                     ! return the leftmost empty index of seq
: place ( seq elt n -- seq' ) rot [ set-nth ] keep ;    ! set nth member of seq to elt, keeping seq on the stack

: white-bishop ( -- elt n ) CHAR: ♗ 4 random 2 * ;
: black-bishop ( -- elt n ) white-bishop 1 + ;
: queen ( seq -- seq elt n ) CHAR: ♕ over empty ;
: knight ( seq -- seq elt n ) CHAR: ♘ over empty ;
: rook ( seq -- seq elt n ) CHAR: ♖ over next ;
: king ( seq -- seq elt n ) CHAR: ♔ over next ;

: chess960 ( -- str )
    "        " clone
    black-bishop place
    white-bishop place
    queen place
    knight place
    knight place
    rook place
    king place
    rook place ;

: chess960-demo ( -- ) 5 [ chess960 print ] times ;

MAIN: chess960-demo
```

```txt

♕♖♗♘♔♘♖♗
♕♗♖♘♗♘♔♖
♗♘♕♖♔♗♖♘
♘♖♗♕♔♗♘♖
♗♗♘♖♘♔♕♖

```


===Built-in===
Factor comes with a chess960 position generator:

```factor
USING: chess960 prettyprint ;

chess960-position .
```

```txt

{ rook bishop king knight bishop queen rook knight }

```



## Forth



```forth
\ make starting position for Chess960, constructive

\             0    1    2    3    4    5    6    7    8    9
create krn S" NNRKRNRNKRNRKNRNRKRNRNNKRRNKNRRNKRNRKNNRRKNRNRKRNN" mem,

create pieces 8 allot

: chess960 ( n -- )
  pieces 8 erase
  4 /mod swap  2* 1+ pieces + 'B swap c!
  4 /mod swap  2*    pieces + 'B swap c!
  6 /mod swap  pieces swap bounds begin dup c@ if swap 1+ swap then 2dup > while 1+ repeat drop 'Q swap c!
  5 * krn +  pieces 8 bounds do i c@ 0= if dup c@ i c! 1+ then loop drop
  cr pieces 8 type ;

0   chess960   \ BBQNNRKR ok
518 chess960   \ RNBQKBNR ok
959 chess960   \ RKRNNQBB ok

960 choose chess960    \ random position
```



## Fortran

This implementation simply iterates through all 960 positions.

```fortran
program chess960
    implicit none

    integer, pointer  :: a,b,c,d,e,f,g,h
    integer, target   :: p(8)
    a => p(1)
    b => p(2)
    c => p(3)
    d => p(4)
    e => p(5)
    f => p(6)
    g => p(7)
    h => p(8)

    king: do a=2,7                                        ! King on an internal square
        r1: do b=1,a-1                                    ! R1 left of the King
            r2: do c=a+1,8                                ! R2 right of the King
                b1: do d=1,7,2                            ! B1 on an odd square
                    if (skip_pos(d,4)) cycle
                    b2: do e=2,8,2                        ! B2 on an even square
                        if (skip_pos(e,5)) cycle
                        queen: do f=1,8                   ! Queen anywhere else
                            if (skip_pos(f,6)) cycle
                            n1: do g=1,7                  ! First knight
                                if (skip_pos(g,7)) cycle
                                n2: do h=g+1,8            ! Second knight (indistinguishable from first)
                                    if (skip_pos(h,8)) cycle
                                    if (sum(p) /= 36) stop 'Loop error'  ! Sanity check
                                    call write_position
                                end do n2
                            end do n1
                        end do queen
                    end do b2
                end do b1
            end do r2
        end do r1
    end do king

contains

    logical function skip_pos(i, n)
        integer, intent(in) :: i, n
        skip_pos = any(p(1:n-1) == i)
    end function skip_pos

    subroutine write_position
        integer           :: i, j
        character(len=15) :: position = ' '
        character(len=1), parameter  :: names(8) = ['K','R','R','B','B','Q','N','N']
        do i=1,8
            j = 2*p(i)-1
            position(j:j) = names(i)
        end do
        write(*,'(a)') position
    end subroutine write_position

end program chess960

```

The first ten positions:

```txt
R K R B B Q N N
R K R B B N Q N
R K R B B N N Q
R K R Q B B N N
R K R N B B Q N
R K R N B B N Q
R K R Q B N N B
R K R N B Q N B
R K R N B N Q B
R K R B Q N B N
```




## FreeBASIC

```freebasic

Randomize Timer
For i As Byte = 1 To 10
    Dim As String inicio = "RKR", pieza = "QNN"
    Dim As Byte posic

    For n As Byte = 1 To Len(pieza)
        posic = Int(Rnd*(Len(inicio) + 1)) + 1
        inicio = Left(inicio, posic-1) + _
        Mid(pieza, n, 1) +_
        Right(inicio, Len(inicio) - posic + 1)
    Next n
    posic = Int(Rnd*(Len(inicio) + 1)) + 1
    inicio = Left(inicio, posic-1) + "B" + Right(inicio, Len(inicio) - posic + 1)
    posic = posic + 1 + 2 * Int(Int(Rnd*(Len(inicio) - posic)) / 2)
    inicio = Left(inicio, posic-1) + "B" + Right(inicio, Len(inicio) - posic + 1)
    Print inicio
Next i

```




## Go

```go
package main

import (
    "fmt"
    "math/rand"
)

type symbols struct{ k, q, r, b, n rune }

var A = symbols{'K', 'Q', 'R', 'B', 'N'}
var W = symbols{'♔', '♕', '♖', '♗', '♘'}
var B = symbols{'♚', '♛', '♜', '♝', '♞'}

var krn = []string{
    "nnrkr", "nrnkr", "nrknr", "nrkrn",
    "rnnkr", "rnknr", "rnkrn",
    "rknnr", "rknrn",
    "rkrnn"}

func (sym symbols) chess960(id int) string {
    var pos [8]rune
    q, r := id/4, id%4
    pos[r*2+1] = sym.b
    q, r = q/4, q%4
    pos[r*2] = sym.b
    q, r = q/6, q%6
    for i := 0; ; i++ {
        if pos[i] != 0 {
            continue
        }
        if r == 0 {
            pos[i] = sym.q
            break
        }
        r--
    }
    i := 0
    for _, f := range krn[q] {
        for pos[i] != 0 {
            i++
        }
        switch f {
        case 'k':
            pos[i] = sym.k
        case 'r':
            pos[i] = sym.r
        case 'n':
            pos[i] = sym.n
        }
    }
    return string(pos[:])
}

func main() {
    fmt.Println(" ID  Start position")
    for _, id := range []int{0, 518, 959} {
        fmt.Printf("%3d  %s\n", id, A.chess960(id))
    }
    fmt.Println("\nRandom")
    for i := 0; i < 5; i++ {
        fmt.Println(W.chess960(rand.Intn(960)))
    }
}
```

```txt
 ID  Start position
  0  BBQNNRKR
518  RNBQKBNR
959  RKRNNQBB

Random
♗♘♖♗♘♔♕♖
♕♘♖♔♘♖♗♗
♖♘♗♔♖♕♘♗
♘♘♖♕♗♔♖♗
♗♕♘♗♘♖♔♖
```



## Haskell


```Haskell
import Data.List
import qualified Data.Set as Set

data Piece = K | Q | R | B | N deriving (Eq, Ord, Show)

isChess960 :: [Piece] -> Bool
isChess960 rank =
  (odd . sum $ findIndices (== B) rank) && king > rookA && king < rookB
  where
    Just king      = findIndex (== K) rank
    [rookA, rookB] = findIndices (== R) rank

main :: IO ()
main = mapM_ (putStrLn . concatMap show) . Set.toList . Set.fromList
       . filter isChess960 $ permutations [R,N,B,Q,K,B,N,R]
```

```txt
QRKRBBNN
QRKRBNNB
QRKRNBBN
QRKRNNBB
QRKBRNBN
...
```



## J

Build a table of the starting positions then pick one at random. There are 40320 distinct permutations of 8 items and 5040 distinct permutations of these chess pieces and (as the task name points out) only 960 permutations which also satisfy the constraints on bishop and rook position, so little memory is needed to generate the table. Also, since the table is built at "compile time", execution is fast (though "compilation" is reasonably fast also).


```J
row0=: u: 9812+2}.5|i.10
king=: u:9812
rook=: u:9814
bish=: u:9815
pos=: I.@e.
bishok=: 1=2+/ .| pos&bish
rookok=: pos&rook -: (<./,>./)@pos&(rook,king)
ok=: bishok*rookok
perm=: A.&i.~ !
valid=: (#~ ok"1) ~.row0{"1~perm 8
gen=: valid {~ ? bind 960
```


Example use:


```J
   gen''
♘♗♖♔♗♕♖♘
   gen''
♗♘♘♗♖♔♖♕
   gen''
♖♗♔♘♘♕♗♖
   gen''
♖♔♕♗♗♘♖♘
```



## Java

Regex inspired by (original) [[#Python: Regexp|Python Regexp]], prints ten examples.

```java5
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class Chess960{
	private static List<Character> pieces = Arrays.asList('R','B','N','Q','K','N','B','R');

	public static List<Character> generateFirstRank(){
		do{
			Collections.shuffle(pieces);
		}while(!check(pieces.toString().replaceAll("[^\\p{Upper}]", ""))); //List.toString adds some human stuff, remove that

		return pieces;
	}

	private static boolean check(String rank){
		if(!rank.matches(".*R.*K.*R.*")) return false;			//king between rooks
		if(!rank.matches(".*B(..|....|......|)B.*")) return false;	//all possible ways bishops can be placed
		return true;
	}

	public static void main(String[] args){
		for(int i = 0; i < 10; i++){
			System.out.println(generateFirstRank());
		}
	}
}
```

```txt
[R, N, K, N, R, B, B, Q]
[B, B, Q, R, N, K, N, R]
[R, K, Q, N, N, R, B, B]
[N, B, B, N, R, K, Q, R]
[R, Q, B, B, K, N, N, R]
[R, K, B, Q, N, B, N, R]
[N, N, R, K, Q, B, B, R]
[R, N, K, Q, N, B, B, R]
[N, R, B, K, Q, B, N, R]
[N, Q, N, R, K, B, B, R]
```



## JavaScript

This conforms to Altendörfer's single die method[https://en.wikipedia.org/wiki/Chess960_starting_position#Single_die_method], though the die will give no "needless" numbers.

```javaScript
function ch960startPos() {
  var rank = new Array(8),
      // randomizer (our die)
      d = function(num) { return Math.floor(Math.random() * ++num) },
      emptySquares = function() {
        var arr = [];
        for (var i = 0; i < 8; i++) if (rank[i] == undefined) arr.push(i);
        return arr;
      };
  // place one bishop on any black square
  rank[d(2) * 2] = "♗";
  // place the other bishop on any white square
  rank[d(2) * 2 + 1] = "♗";
  // place the queen on any empty square
  rank[emptySquares()[d(5)]] = "♕";
  // place one knight on any empty square
  rank[emptySquares()[d(4)]] = "♘";
  // place the other knight on any empty square
  rank[emptySquares()[d(3)]] = "♘";
  // place the rooks and the king on the squares left, king in the middle
  for (var x = 1; x <= 3; x++) rank[emptySquares()[0]] = x==2 ? "♔" : "♖";
  return rank;
}

// testing (10 times)
for (var x = 1; x <= 10; x++) console.log(ch960startPos().join(" | "));
```

<p>The test-output (exemplary each):</p>
♖ | ♗ | ♗ | ♔ | ♘ | ♖ | ♘ | ♕<br/>
♗ | ♗ | ♕ | ♖ | ♔ | ♘ | ♘ | ♖<br/>
♖ | ♕ | ♘ | ♗ | ♗ | ♔ | ♘ | ♖<br/>
♖ | ♗ | ♔ | ♘ | ♗ | ♕ | ♘ | ♖<br/>
♗ | ♖ | ♕ | ♔ | ♘ | ♗ | ♘ | ♖<br/>
♖ | ♗ | ♗ | ♕ | ♔ | ♘ | ♖ | ♘<br/>
♗ | ♘ | ♖ | ♗ | ♔ | ♘ | ♕ | ♖<br/>
♕ | ♘ | ♗ | ♖ | ♔ | ♗ | ♖ | ♘<br/>
♗ | ♘ | ♖ | ♘ | ♕ | ♗ | ♔ | ♖<br/>
♘ | ♗ | ♖ | ♔ | ♗ | ♘ | ♖ | ♕<br/>


## Julia

```julia
function generateposition()
    # Placeholder knights
    rank = ['♘', '♘', '♘', '♘', '♘', '♘', '♘', '♘']
    lrank = length(rank)

    # Check if a space is available
    isfree(x::Int) = rank[x] == '♘'

    # Place the King
    rank[indking = rand(2:lrank-1)] = '♔'

    # Place rooks
    rank[indrook = rand(filter(isfree, 1:lrank))] = '♖'
    if indrook > indking
        rank[rand(filter(isfree, 1:indking-1))] = '♖'
    else
        rank[rand(filter(isfree, indking+1:lrank))] = '♖'
    end

    # Place bishops
    rank[indbish = rand(filter(isfree, 1:8))] = '♗'
    pbish = filter(iseven(indbish) ? isodd : iseven, 1:lrank)
    rank[rand(filter(isfree, pbish))] = '♗'

    # Place queen
    rank[rand(filter(isfree, 1:lrank))] = '♕'
    return rank
end

@show generateposition()
```

```txt
generateposition() = ['♘', '♗', '♗', '♖', '♕', '♔', '♘', '♖']
```



## Kotlin


```scala
object Chess960 : Iterable<String>
 {
    override fun iterator() = patterns.iterator()

    private operator fun invoke(b: String, e: String) {
        if (e.length <= 1) {
            val s = b + e
            if (s.is_valid()) patterns += s
        } else {
            for (i in 0 until e.length) {
                invoke(b + e[i], e.substring(0, i) + e.substring(i + 1))
            }
        }
    }

    private fun String.is_valid(): Boolean {
        val k = indexOf('K')
        return indexOf('R') < k && k < lastIndexOf('R') &&
            indexOf('B') % 2 != lastIndexOf('B') % 2
    }

    private val patterns = sortedSetOf<String>()

    init {
        invoke("", "KQRRNNBB")
    }
}

fun main(args: Array<String>) {
    Chess960.forEachIndexed { i, s -> println("$i: $s") }
}
```

```txt
0: BBNNQRKR
1: BBNNRKQR
2: BBNNRKRQ
...
957: RQNNBKRB
958: RQNNKBBR
959: RQNNKRBB
```



## Lua


```Lua
-- Insert 'str' into 't' at a random position from 'left' to 'right'
function randomInsert (t, str, left, right)
    local pos
    repeat pos = math.random(left, right) until not t[pos]
    t[pos] = str
    return pos
end

-- Generate a random Chess960 start position for white major pieces
function chess960 ()
    local t, b1, b2 = {}
    local kingPos = randomInsert(t, "K", 2, 7)
    randomInsert(t, "R", 1, kingPos - 1)
    randomInsert(t, "R", kingPos + 1, 8)
    b1 = randomInsert(t, "B", 1, 8)
    b2 = randomInsert(t, "B", 1, 8)
    while (b2 - b1) % 2 == 0 do
        t[b2] = false
        b2 = randomInsert(t, "B", 1, 8)
    end
    randomInsert(t, "Q", 1, 8)
    randomInsert(t, "N", 1, 8)
    randomInsert(t, "N", 1, 8)
    return t
end

-- Main procedure
math.randomseed(os.time())
print(table.concat(chess960()))
```

```txt
NNRQBBKR
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Generates all possible initial conditions, filters for validity, and chooses a random element.

```Mathematica
Print[StringJoin[
   RandomChoice[
    Select[Union[
      Permutations[{"\[WhiteKing]", "\[WhiteQueen]", "\[WhiteRook]",
        "\[WhiteRook]", "\[WhiteBishop]", "\[WhiteBishop]",
        "\[WhiteKnight]", "\[WhiteKnight]"}]],
     MatchQ[#, {___, "\[WhiteRook]", ___, "\[WhiteKing]", ___,
         "\[WhiteRook]", ___}] &&
       OddQ[Subtract @@ Flatten[Position[#, "\[WhiteBishop]"]]] &]]]];
```



## Objeck

```objeck
﻿class Chess960 {
  function : Main(args : String[]) ~ Nil {
    Generate(10);
  }

  function : Generate(c : Int) ~ Nil {
    for(x := 0; x < c; x += 1;) {
      StartPos()->PrintLine();
    };
  }

  function : StartPos() ~ String {
    p := Char->New[8];

    # bishops
    b1 : Int; b2 : Int;
    while(true) {
      b1 := GetPosition(); b2 := GetPosition();

      b1c := b1 and 1; b2c := b2 and 1;
      c := b1c = 0 & b2c <> 0;
      if(c) {
        break;
      };
    };
    p[b1] := 0x2657; p[b2] := 0x2657;

    # queen, knight, knight
    q := false;
    for(x := 0; x < 3; x += 1;) {
      do {
        b1 := GetPosition();
      } while( p[b1] <> '\0');

      if(<>q) {
        p[b1] := 0x2655; q := true;
      }
      else {
        p[b1] := 0x2658;
      };
    };

    # rook king rook
    q := false;
    for(x := 0; x < 3; x += 1;) {
      a := 0;
      while(a < 8) {
        if(p[a] = '\0') {
          break;
        };
        a += 1;
      };

      if(<>q) {
        p[a] := 0x2656; q := true;
      }
      else {
        p[a] := 0x2654; q := false;
      };
    };

    s := "";
    for(x := 0; x < 8; x += 1;) { s->Append(p[x]); };
    return s;
  }

  function : GetPosition() ~ Int {
    return (Float->Random() * 1000)->As(Int) % 8;
  }
}
```


Output:
<big><big>
```txt
♗♖♕♔♖♗♘♘
♕♗♖♔♗♘♖♘
♖♘♔♘♕♖♗♗
♖♗♔♘♖♘♗♕
♖♔♖♘♕♗♗♘
♗♘♖♕♔♘♖♗
♗♖♔♕♖♘♘♗
♗♖♔♘♘♖♕♗
♖♕♔♖♘♘♗♗
♗♖♘♔♘♖♕♗
```
</big></big>


## PARI/GP


```parigp
chess960() =
{
  my (C = vector(8), i, j, r);

  C[random(4) * 2 + 1] = C[random(4) * 2 + 2] = "B";
  for (i = 1, 3, while (C[r = random(8) + 1],); C[r] = Vec("NNQ")[i]);
  for (i = 1, 8, if (!C[i], C[i] = Vec("RKR")[j++]));
  C
}
```


Output:
```txt
gp > for(i=1, 10, print(chess960()));
["N", "R", "Q", "K", "N", "R", "B", "B"]
["R", "K", "N", "B", "N", "R", "B", "Q"]
["B", "R", "K", "N", "R", "B", "Q", "N"]
["R", "B", "Q", "K", "B", "N", "R", "N"]
["R", "B", "K", "N", "N", "Q", "B", "R"]
["N", "Q", "N", "R", "B", "K", "R", "B"]
["N", "Q", "R", "B", "K", "R", "B", "N"]
["N", "R", "B", "K", "R", "N", "Q", "B"]
["R", "K", "Q", "N", "B", "N", "R", "B"]
["B", "B", "R", "N", "K", "R", "N", "Q"]

```


Alternatively with recent version of PARI/GP >= 2.9:
```txt
gp > M=Map(["B","♗";"K","♔";"N","♘";"Q","♕";"R","♖"]);
gp > for(i=1,10,print(concat(apply((c)->mapget(M,c),chess960()))));
♗♖♘♔♕♗♖♘
♕♖♘♔♖♗♗♘
♖♕♘♔♘♖♗♗
♘♘♗♖♕♗♔♖
♘♘♖♗♗♔♖♕
♗♖♘♔♘♕♖♗
♖♔♘♗♗♕♘♖
♘♖♗♘♔♗♕♖
♖♔♕♗♗♘♖♘
♕♗♖♔♗♘♘♖
```



## Perl

Directly generates a configuration by inserting pieces at random appropriate places.  Each config has an equal chance of being produced.

```perl
sub rnd($) { int(rand(shift)) }

sub empties { grep !$_[0][$_], 0 .. 7 }

sub chess960 {
	my @s = (undef) x 8;
	@s[2*rnd(4), 1 + 2*rnd(4)] = qw/B B/;

	for (qw/Q N N/) {
		my @idx = empties \@s;
		$s[$idx[rnd(@idx)]] = $_;
	}

	@s[empties \@s] = qw/R K R/;
	@s
}
print "@{[chess960]}\n" for 0 .. 10;
```

```txt
R N B K R N Q B
N N R K B R Q B
N N Q R K R B B
Q R N K B N R B
R K R B N Q B N
B R K B Q N R N
B R N B Q K N R
R B Q N N K B R
N R N Q K R B B
R Q N K R B B N
R K N Q B B R N
```



## Perl 6

First, using a list with three rooks and no king, we keep generating a random piece order until the two bishops are on opposite colors.  Then we sneakily promote the second of the three rooks to a king.

```perl6
repeat until m/ '♗' [..]* '♗' / { $_ = < ♖ ♖ ♖ ♕ ♗ ♗ ♘ ♘ >.pick(*).join }
s:2nd['♖'] = '♔';
say .comb;
```

<big><big><big><big>
```txt
♕ ♗ ♖ ♘ ♔ ♖ ♗ ♘
```
</big></big></big></big>
Here's a more "functional" solution that avoids side effects

```perl6
sub chess960 {
    .subst(:nth(2), /'♜'/, '♚') given
    first rx/ '♝' [..]* '♝' /,
    < ♛ ♜ ♜ ♜ ♝ ♝ ♞ ♞ >.pick(*).join xx *;
}

say chess960;
```

<big><big><big><big>
```txt
♛♝♜♚♝♞♞♜
```
</big></big></big></big>

We can also pregenerate the list of 960 positions, though the method we use below is a bit wasteful, since it
generates 40320 candidates only to throw most of them away. This is essentially the same filtering algorithm
but written in the form of a list comprehension rather than nested map and grep.  (The list comprehension is actually faster currently.)  Note that the constant is calculated at compile time, because, well, it's a constant.  Just a big fancy one.


```perl6
constant chess960 =
   < ♛ ♜ ♜ ♜ ♝ ♝ ♞ ♞ >.permutations».join.unique.grep( / '♝' [..]* '♝' / )».subst(:nth(2), /'♜'/, '♚');

.say for chess960;
```

Here's a much faster way (about 30x) to generate all 960 variants by construction.  No need to filter for uniqueness, since it produces exactly 960 entries.

```perl6
constant chess960 = gather for 0..3 -> $q {
    (my @q = <♜ ♚ ♜>).splice($q, 0, '♛');
    for 0 .. @q -> $n1 {
        (my @n1 = @q).splice($n1, 0, '♞');
        for $n1 ^.. @n1 -> $n2 {
            (my @n2 = @n1).splice($n2, 0, '♞');
            for 0 .. @n2 -> $b1 {
                (my @b1 = @n2).splice($b1, 0, '♝');
                for $b1+1, $b1+3 ...^ * > @b1 -> $b2 {
                    (my @b2 = @b1).splice($b2, 0, '♝');
                    take @b2.join;
                }
            }
        }
    }
}

CHECK { note "done compiling" }
note +chess960;
say chess960.pick;
```

```txt
done compiling
960
♜♚♝♜♞♛♞♝
```

If you run this you'll see that most of the time is spent in compilation, so in the case of separate precompilation the table of 960 entries merely needs to be deserialized back into memory.  Picking from those entries guarantees uniform distribution over all possible boards.


## Phix

Examines all 40320 permutations for validity and saves them in a list, which is easy to pick random entries from.

Using a dictionary (as commented out) is a little faster, but harder to extract random entries from.

For something faster, and truer to the task description, just use the commented out permute(rand(factorial(8) line,
and quit as soon as you find a valid one (but I wanted to check that I had found exactly 960).

```Phix
sequence solutions = {}
--integer d = new_dict()

for i=1 to factorial(8) do
    sequence s = permute(i,"RNBQKBNR")
--  sequence s = permute(rand(factorial(8),"RNBQKBNR")
    integer b1 = find('B',s),
            b2 = find('B',s,b1+1)
    if and_bits(b2-b1,1)=1 then
        integer k = find('K',s)
        integer r1 = find('R',s)
        integer r2 = find('R',s,r1+1)
        if r1<k and k<r2 then
            if find(s,solutions)=0 then
--          if getd_index(s,d)=0 then
--              setd(s,0,d)
                solutions = append(solutions,s)
            end if
        end if
    end if
end for
printf(1,"Found %d solutions\n",{length(solutions)})
for i=1 to 5 do
    ?solutions[rand(length(solutions))]
end for
```

```txt
Found 960 solutions
"QRNNKRBB"
"BQRNKBNR"
"BRQBNNKR"
"QBNRBKRN"
"RNKBBQRN"
```


## PicoLisp

```PicoLisp
(load "@lib/simul.l")

(seed (in "/dev/urandom" (rd 8)))

(loop
   (match
      '(@A B @B B @C)
      (shuffle '(Q B B N N 0 0 0)) )
   (NIL (bit? 1 (length @B))) )

(let Rkr '(R K R)
   (for I (append @A '(B) @B '(B) @C)
      (prin (if (=0 I) (pop 'Rkr) I)) )
   (prinl) )

(bye)
```


## PowerShell

```PowerShell
function Get-RandomChess960Start
    {
    $Starts = @()

    ForEach ( $Q  in       0..3 ) {
    ForEach ( $N1 in       0..4 ) {
    ForEach ( $N2 in ($N1+1)..5 ) {
    ForEach ( $B1 in       0..3 ) {
    ForEach ( $B2 in       0..3 ) {
        $BB = $B1 * 2 + ( $B1 -lt $B2 )
        $BW = $B2 * 2
        $Start = [System.Collections.ArrayList]( '♖', '♔', '♖' )
        $Start.Insert( $Q , '♕' )
        $Start.Insert( $N1, '♘' )
        $Start.Insert( $N2, '♘' )
        $Start.Insert( $BB, '♗' )
        $Start.Insert( $BW, '♗' )
        $Starts += ,$Start
        }}}}}

    $Index = Get-Random 960
    $StartString = $Starts[$Index] -join ''
    return $StartString
    }

Get-RandomChess960Start
Get-RandomChess960Start
Get-RandomChess960Start
Get-RandomChess960Start

```

```txt
♘♕♖♔♖♘♗♗
♗♕♘♖♔♗♖♘
♖♗♔♕♗♖♘♘
♘♖♔♖♕♗♗♘
```



## Python


### Python: Indexing

This uses indexing rather than regexps. Rooks and bishops are in upper and lower case to start with so they can be individually indexed to apply the constraints. This would lead to some duplication of start positions if not for the use of a set comprehension to uniquify the, (upper-cased), start positions.


```python
>>>
 from itertools import permutations
>>> pieces = 'KQRrBbNN'
>>> starts = {''.join(p).upper() for p in permutations(pieces)
                     if p.index('B') % 2 != p.index('b') % 2 		# Bishop constraint
                     and ( p.index('r') < p.index('K') < p.index('R')	# King constraint
                           or p.index('R') < p.index('K') < p.index('r') ) }
>>> len(starts)
960
>>> starts.pop()
'QNBRNKRB'
>>>
```



### Python: Regexp

This uses regexps to filter permutations of the start position pieces rather than indexing.

```python
>>>
 import re
>>> pieces = 'KQRRBBNN'
>>> bish = re.compile(r'B(|..|....|......)B').search
>>> king = re.compile(r'R.*K.*R').search
>>> starts3 = {p for p in (''.join(q) for q in permutations(pieces))
            if bish(p) and king(p)}
>>> len(starts3)
960
>>> starts3.pop()
'QRNKBNRB'
>>>
```



### Python: Correct by construction

Follows Perl algorithm of constructing one start position randomly, according to the rules.
(See talk page for tests).

```python
from random import choice

def random960():
    start = ['R', 'K', 'R']         # Subsequent order unchanged by insertions.
    #
    for piece in ['Q', 'N', 'N']:
        start.insert(choice(range(len(start)+1)), piece)
    #
    bishpos = choice(range(len(start)+1))
    start.insert(bishpos, 'B')
    start.insert(choice(range(bishpos + 1, len(start) + 1, 2)), 'B')
    return start
    return ''.join(start).upper()

print(random960())
```

```txt
['N', 'R', 'K', 'N', 'B', 'Q', 'R', 'B']
```



### Python: Generate all positions then choose one randomly


```python
from random import choice

def generate960():
    start = ('R', 'K', 'R')         # Subsequent order unchanged by insertions.

    # Insert QNN in all combinations of places
    starts = {start}
    for piece in ['Q', 'N', 'N']:
        starts2 = set()
        for s in starts:
            for pos in range(len(s)+1):
                s2 = list(s)
                s2.insert(pos, piece)
                starts2.add(tuple(s2))
        starts = starts2

    # For each of the previous starting positions insert the bishops in their 16 positions
    starts2 = set()
    for s in starts:
        for bishpos in range(len(s)+1):
            s2 = list(s)
            s2.insert(bishpos, 'B')
            for bishpos2 in range(bishpos+1, len(s)+2, 2):
                s3 = s2[::]
                s3.insert(bishpos2, 'B')
                starts2.add(tuple(s3))

    return  list(starts2)

gen = generate960()
print(''.join(choice(gen)))
```

```txt
NRBQNKRB
```



## R


```rsplus

pieces <- c("R","B","N","Q","K","N","B","R")

generateFirstRank <- function() {
  attempt <- paste0(sample(pieces), collapse = "")
  while (!check_position(attempt)) {
    attempt <- paste0(sample(pieces), collapse = "")
  }
  return(attempt)
}

check_position <- function(position) {
  if (regexpr('.*R.*K.*R.*', position) == -1) return(FALSE)
  if (regexpr('.*B(..|....|......|)B.*', position) == -1) return(FALSE)
  TRUE
}

convert_to_unicode <- function(s) {
  s <- sub("K","\u2654", s)
  s <- sub("Q","\u2655", s)
  s <- gsub("R","\u2656", s)
  s <- gsub("B","\u2657", s)
  s <- gsub("N","\u2658", s)
}

cat(convert_to_unicode(generateFirstRank()), "\n")

```

```txt

♘♗♘♖♗♕♔♖

```


## Racket


Constructive:


```racket
#lang racket
(define white (match-lambda ['P #\♙] ['R #\♖] ['B #\♗] ['N #\♘] ['Q #\♕] ['K #\♔]))
(define black (match-lambda ['P #\♟] ['R #\♜] ['B #\♝] ['N #\♞] ['Q #\♛] ['K #\♚]))

(define (piece->unicode piece colour)
  (match colour ('w white) ('b black)) piece)

(define (find/set!-random-slot vec val k (f values))
  (define r (f (random k)))
  (cond
    [(vector-ref vec r)
     (find/set!-random-slot vec val k f)]
    [else
     (vector-set! vec r val)
     r]))

(define (chess960-start-position)
  (define v (make-vector 8 #f))
  ;; Kings and Rooks
  (let ((k (find/set!-random-slot v (white 'K) 6 add1)))
    (find/set!-random-slot v (white 'R) k)
    (find/set!-random-slot v (white 'R) (- 7 k) (curry + k 1)))
  ;; Bishops -- so far only three squares allocated, so there is at least one of each colour left
  (find/set!-random-slot v (white 'B) 4 (curry * 2))
  (find/set!-random-slot v (white 'B) 4 (compose add1 (curry * 2)))
  ;; Everyone else
  (find/set!-random-slot v (white 'Q) 8)
  (find/set!-random-slot v (white 'N) 8)
  (find/set!-random-slot v (white 'N) 8)
  (list->string (vector->list v)))

(chess960-start-position)
```

```txt
"♖♘♗♕♔♗♘♖"
```

Well that's embarassing... the stupid thing has only gone and randomly generated a classic chess starting position.

Try again:

```txt
"♘♖♔♕♗♗♖♘"
```



## REXX

Random starting position is correct by construction   (both REXX entries).

### generates one random position


```rexx
/*REXX program generates a random starting position  for the  Chess960  game. */
parse arg seed .                       /*allow for (RANDOM BIF) repeatability.*/
if seed\==''  then call random ,,seed  /*if SEED was specified,  use the seed.*/
@.=.                                   /*define the first rank as being empty.*/
r1=random(1,6)                         /*generate the first rook:  rank 1.    */
@.r1='R'                               /*place the  first rook  on  rank1.    */
          do  until  r2\==r1  &  r2\==r1-1  &  r2\==r1+1
          r2=random(1,8)               /*find placement for the 2nd rook.     */
          end   /*forever*/
@.r2='r'                               /*place the second rook  on  rank 1.   */
k=random(min(r1, r2)+1, max(r1, r2)-1) /*find a random position for the king. */
@.k='K'                                /*place king between the two rooks.    */
          do _=0      ; b1=random(1,8);  if @.b1\==.  then iterate;  c=b1//2
            do forever; b2=random(1,8)       /* c=color of bishop ►──┘        */
            if @.b2\==. | b2==b1 | b2//2==c  then iterate /*is a bad position?*/
            leave _                    /*found position for the 2 clergy*/
            end   /*forever*/          /* [↑]  find a place for the 1st bishop*/
          end     /* _ */              /* [↑]    "  "   "    "   "  2nd    "  */
@.b1='B'                               /*place the  1st  bishop on  rank 1.   */
@.b2='b'                               /*  "    "   2nd     "    "    "  "    */
                                       /*place the two knights on rank 1.     */
   do  until @._='N';  _=random(1,8);   if @._\==.  then iterate; @._='N';   end
   do  until @.!='n';  !=random(1,8);   if @.!\==.  then iterate; @.!='n';   end
_=                                     /*only the queen is left to be placed. */
   do i=1  for 8;  _=_ || @.i;   end   /*construct the output: first rank only*/
say translate(translate(_, 'q', .))    /*stick a fork in it,  we're all done. */
```

'''output'''

```txt
NRQKBRNB
```



### generates all 960 positions randomly


```rexx
/*REXX program generates all random starting positions for the Chess960 game. */
parse arg seed .                       /*allow for (RANDOM BIF) repeatability.*/
if seed\==''  then call random ,,seed  /*if SEED was specified,  use the seed.*/
x.=0;  #=0;  rg='random generations: ' /*initialize game placeholder; # games.*/
       /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒*/
do t=1                                 /* [↓]  display every 1,000 generations*/   /*▒*/
if t//1000==0  then say  right(t,9)   rg     #     " unique starting positions."   /*▒*/
@.=.                                   /*define the first rank as being empty.*/   /*▒*/
r1=random(1,6)                         /*generate the first rook:  rank 1.    */   /*▒*/
@.r1='R'                               /*place the  first rook  on  rank1.    */   /*▒*/
          do  until  r2\==r1  &  r2\==r1-1  &  r2\==r1+1                           /*▒*/
          r2=random(1,8)               /*find placement for the 2nd rook.     */   /*▒*/
          end   /*forever*/                                                        /*▒*/
@.r2='r'                               /*place the second rook  on  rank 1.   */   /*▒*/
k=random(min(r1, r2)+1, max(r1, r2)-1) /*find a random position for the king. */   /*▒*/
@.k='K'                                /*place king between the two rooks.    */   /*▒*/
          do _=0      ; b1=random(1,8);  if @.b1\==.  then iterate;  c=b1//2       /*▒*/
            do forever; b2=random(1,8)       /* c=color of bishop ►──┘        */   /*▒*/
            if @.b2\==. | b2==b1 | b2//2==c  then iterate /*is a bad position?*/   /*▒*/
            leave _                    /*found position for the 2 clergy*/         /*▒*/
            end   /*forever*/          /* [↑]  find a place for the 1st bishop*/   /*▒*/
          end     /* _ */              /* [↑]    "  "   "    "   "  2nd    "  */   /*▒*/
@.b1='B'                               /*place the  1st  bishop on  rank 1.   */   /*▒*/
@.b2='b'                               /*  "    "   2nd     "    "    "  "    */   /*▒*/
                                       /*place the two knights on rank 1.     */   /*▒*/
   do  until @._='N';  _=random(1,8);   if @._\==.  then iterate; @._='N';   end   /*▒*/
   do  until @.!='n';  !=random(1,8);   if @.!\==.  then iterate; @.!='n';   end   /*▒*/
_=                                     /*only the queen is left to be placed. */   /*▒*/
   do i=1  for 8;  _=_ || @.i;   end   /*construct the output: first rank only*/   /*▒*/
upper _                                /*uppercase all the chess pieces.      */   /*▒*/
if x._  then iterate                   /*This position found before?  Skip it.*/   /*▒*/
x._=1                                  /*define this position as being found. */   /*▒*/
#=#+1                                  /*bump the # of unique positions found,*/   /*▒*/
if #==960  then leave                                                              /*▒*/
end   /*t ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒*/

say # 'unique starting positions found after '   t   "generations."
                                       /*stick a fork in it,  we're all done. */         /**/
```

'''output'''

```txt
     1000 random generations:  515  unique starting positions.
     2000 random generations:  707  unique starting positions.
     3000 random generations:  796  unique starting positions.
     4000 random generations:  849  unique starting positions.
     5000 random generations:  883  unique starting positions.
     6000 random generations:  900  unique starting positions.
     7000 random generations:  922  unique starting positions.
     8000 random generations:  935  unique starting positions.
     9000 random generations:  942  unique starting positions.
    10000 random generations:  946  unique starting positions.
    11000 random generations:  953  unique starting positions.
    12000 random generations:  957  unique starting positions.
    13000 random generations:  959  unique starting positions.
    14000 random generations:  959  unique starting positions.
960 unique starting positions found after  14639 generations.
```


### version 3 COMPUTE all possibilities


```rexx
/*---------------------------------------------------------------
* Compute the 960 possible solutions
* There must be at least one field between the rooks
* The king is positioned on any field between the rooks
* The queen is placed on any unoccupied field
* bishops are placed so that they are on different colored fields
* what remains are the kNights...
*--------------------------------------------------------------*/
cnt.=0
Call time 'R'
Do r1=1 To 6
  Do r2=r1+1 To 8
    Do kk=r1+1 To r2-1
      poss=space(translate('12345678',' ',r1||kk||r2),0)
      Call rest
      End

 End
  End
say cnt.1 'solutions'
Say time('E')
Exit

rest:
Do i=1 To 5
  q=substr(poss,i,1)
  br=space(translate(poss,' ',q),0)
  Do b1i=1 To 3
    Do b2i=b1i+1 To 4
      Call finish
      End
    End
  End
Return

finish:
  b1=substr(br,b1i,1)
  b2=substr(br,b2i,1)
  If (b1+b2)//2>0 Then
    Call out
  Return

out:
  pos.='N'
  pos.r1='R'
  pos.r2='R'
  pos.kk='K'
  pos.q='Q'
  pos.b1='B'
  pos.b2='B'
  ol=''
  Do k=1 To 8
    ol=ol||pos.k
    End
  cnt.1=cnt.1+1
  If cnt.1<4 |,
     cnt.1>957 Then
    Say format(cnt.1,3) poss r1 kk r2  ol
  If cnt.1=4 Then
    Say '    ...'
  Return
```

```txt
  1 45678 1 2 3 RKRQBBNN
  2 45678 1 2 3 RKRQBNNB
  3 45678 1 2 3 RKRQNBBN
    ...
958 12345 6 7 8 BNNBQRKR
959 12345 6 7 8 NBBNQRKR
960 12345 6 7 8 NNBBQRKR
960 solutions
```



## Ruby


### Ruby: shuffle pieces until all regexes match

Translation of Tcl.

```ruby
pieces = %i(♔ ♕ ♘ ♘ ♗ ♗ ♖ ♖)
regexes = [/♗(..)*♗/, /♖.*♔.*♖/]
row = pieces.shuffle.join until regexes.all?{|re| re.match(row)}
puts row
```

<big><big><big><big>
```txt
♕♖♗♘♔♖♘♗
```
</big></big></big></big>


### Ruby: Construct

Uses the Perl idea of starting with [R,K,R] and inserting the rest:

```ruby
row = [:♖, :♔, :♖]
[:♕, :♘, :♘].each{|piece| row.insert(rand(row.size+1), piece)}
[[0, 2, 4, 6].sample, [1, 3, 5, 7].sample].sort.each{|pos| row.insert(pos, :♗)}

puts row
```

<big><big><big><big>
```txt
♗♘♕♘♖♗♔♖
```
</big></big></big></big>

===Ruby: Generate from SP-ID===
'''[[wp:Chess960 numbering scheme|Chess960 numbering scheme]]'''

```ruby
KRN = %w(NNRKR NRNKR NRKNR NRKRN RNNKR RNKNR RNKRN RKNNR RKNRN RKRNN)

def chess960(id=rand(960))
  pos = Array.new(8)
  q, r = id.divmod(4)
  pos[r * 2 + 1] = "B"
  q, r = q.divmod(4)
  pos[r * 2] = "B"
  q, r = q.divmod(6)
  pos[pos.each_index.reject{|i| pos[i]}[r]] = "Q"
  krn = KRN[q].each_char
  pos.each_index {|i| pos[i] ||= krn.next}
  pos.join
end

puts "Generate Start Position from id number"
[0,518,959].each do |id|
  puts "%3d : %s" % [id, chess960(id)]
end

puts "\nGenerate random Start Position"
5.times {puts chess960}
```

```txt
Generate Start Position from id number
  0 : BBQNNRKR
518 : RNBQKBNR
959 : RKRNNQBB

Generate random Start Position
RNBNKBRQ
RKRNBBNQ
BBRNQKNR
NBRKNRBQ
BRKQNNRB
```


## Rust

```rust
use std::collections::BTreeSet;

struct Chess960 ( BTreeSet<String> );

impl Chess960 {
    fn invoke(&mut self, b: &str, e: &str) {
        if e.len() <= 1 {
            let s = b.to_string() + e;
            if Chess960::is_valid(&s) { self.0.insert(s); }
        } else {
            for (i, c) in e.char_indices() {
                let mut b = b.to_string();
                b.push(c);
                let mut e = e.to_string();
                e.remove(i);
                self.invoke(&b, &e);
            }
        }
    }

    fn is_valid(s: &str) -> bool {
        let k = s.find('K').unwrap();
        k > s.find('R').unwrap() && k < s.rfind('R').unwrap() && s.find('B').unwrap() % 2 != s.rfind('B').unwrap() % 2
    }
}

// Program entry point.
fn main() {
    let mut chess960 = Chess960(BTreeSet::new());
    chess960.invoke("", "KQRRNNBB");

    for (i, p) in chess960.0.iter().enumerate() {
        println!("{}: {}", i, p);
    }
}
```



## Scala

===Functional Programming, tail recursive, Unicode, RegEx===

```Scala
import scala.annotation.tailrec

object Chess960 extends App {

  private val pieces = List('♖', '♗', '♘', '♕', '♔', '♘', '♗', '♖')

  @tailrec
  private def generateFirstRank(pieces: List[Char]): List[Char] = {
    def check(rank: String) =
      rank.matches(".*♖.*♔.*♖.*") && rank.matches(".*♗(..|....|......|)♗.*")

    val p = scala.util.Random.shuffle(pieces)
    if (check(p.toString.replaceAll("[^\\p{Upper}]", "")))
      generateFirstRank(pieces)
    else p
  }

  loop(10)

  @tailrec
  private def loop(n: Int): Unit = {
    println(generateFirstRank(pieces))
    if (n <= 0) () else loop(n - 1)
  }
}
```

{{Out}}See it running in your browser by [https://scalafiddle.io/sf/AkvVAlG/0 ScalaFiddle (JavaScript, non JVM)] or by [https://scastie.scala-lang.org/qpKdhOc4SkuAbze8kgU6zQ Scastie (JVM)].

### Imperative Programming

```scala
object Chess960 extends App {
    private def apply(b: String, e: String) {
        if (e.length <= 1) {
            val s = b + e
            if (is_valid(s)) patterns += s
        } else
            for (i <- 0 until e.length)
                apply(b + e(i), e.substring(0, i) + e.substring(i + 1))
    }

    private def is_valid(s: String) = {
        val k = s.indexOf('K')
        if (k < s.indexOf('R')) false
        else k < s.lastIndexOf('R') && s.indexOf('B') % 2 != s.lastIndexOf('B') % 2
    }

    private val patterns = scala.collection.mutable.SortedSet[String]()

    apply("", "KQRRNNBB")
    for ((s, i) <- patterns.zipWithIndex) println(s"$i: $s")
}
```



## Scheme

```Scheme
(import (scheme base) (scheme write)
        (srfi 1)    ; list library
        (srfi 27))  ; random numbers

(random-source-randomize! default-random-source)

;; Random integer in [start, end)
(define (random-between start end)
  (let ((len (- end start 1)))
    (if (< len 2)
      start
      (+ start (random-integer len)))))

;; Random item in list
(define (random-pick lst)
  (if (= 1 (length lst))
    (car lst)
    (list-ref lst (random-integer (length lst)))))

;; Construct a random piece placement for Chess960
(define (random-piece-positions)
  (define (free-indices positions) ; return list of empty slot indices
    (let loop ((i 0)
               (free '()))
      (if (= 8 i)
        free
        (loop (+ 1 i)
              (if (string=? "." (vector-ref positions i))
                (cons i free)
                free)))))
  ;
  (define (place-king+rooks positions)
    (let ((king-posn (random-between 1 8)))
      (vector-set! positions king-posn "K")
      ; left-rook is between left-edge and king
      (vector-set! positions (random-between 0 king-posn) "R")
      ; right-rook is between right-edge and king
      (vector-set! positions (random-between (+ 1 king-posn) 8) "R")))
  ;
  (define (place-bishops positions)
    (let-values (((evens odds) (partition even? (free-indices positions))))
                (vector-set! positions (random-pick evens) "B")
                (vector-set! positions (random-pick odds) "B")))
  ;
  (let ((positions (make-vector 8 ".")))
    (place-king+rooks positions)
    (place-bishops positions)
    ;; place the queen in a random remaining slot
    (vector-set! positions (random-pick (free-indices positions)) "Q")
    ;; place the two knights in the remaining slots
    (for-each (lambda (idx) (vector-set! positions idx "N"))
              (free-indices positions))

    positions))

(display "First rank: ") (display (random-piece-positions)) (newline)

```

Ten sample runs:

```txt
First rank: #(R N N Q K R B B)
First rank: #(R K N N Q R B B)
First rank: #(Q R B N N B K R)
First rank: #(Q R B N N K R B)
First rank: #(R K N Q R B B N)
First rank: #(R K N B Q R B N)
First rank: #(R N K N B B R Q)
First rank: #(R B K Q B N R N)
First rank: #(B Q R N K N R B)
First rank: #(R B B Q N N K R)
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var string: start is "RKR";
    var char: piece is ' ';
    var integer: pos is 0;
  begin
    for piece range "QNN" do
      pos := rand(1, succ(length(start)));
      start := start[.. pred(pos)] & str(piece) & start[pos ..];
    end for;
    pos := rand(1, succ(length(start)));
    start := start[.. pred(pos)] & "B" & start[pos ..];
    pos := succ(pos) + 2 * rand(0, (length(start) - pos) div 2);
    start := start[.. pred(pos)] & "B" & start[pos ..];
    writeln(start);
  end func;
```

```txt
NQBNRBKR
```



## Sidef


```ruby
func is_valid_960 (backrank) {
    var king = backrank.index('♚')
    var (rook1, rook2) = backrank.indices_of('♜')...
    king.is_between(rook1, rook2) || return false
    var (bishop1, bishop2) = backrank.indices_of('♝')...
    bishop1+bishop2 -> is_odd
}

func random_960_position(pieces = <♛ ♚ ♜ ♜ ♝ ♝ ♞ ♞>) {
    pieces.shuffle.permutations {|*a|
        return a if is_valid_960(a)
    }
}

say random_960_position().join(' ')
```

```txt

♝ ♝ ♜ ♚ ♞ ♛ ♜ ♞

```



## Tcl

Using regular expressions to filter a random permutation.
```tcl
package require struct::list

proc chess960 {} {
    while true {
	set pos [join [struct::list shuffle {N N B B R R Q K}] ""]
	if {[regexp {R.*K.*R} $pos] && [regexp {B(..)*B} $pos]} {
	    return $pos
	}
    }
}

# A simple renderer
proc chessRender {position} {
    string map {P ♙ N ♘ B ♗ R ♖ Q ♕ K ♔} $position
}

# Output multiple times just to show scope of positions
foreach - {1 2 3 4 5} {puts [chessRender [chess960]]}
```

```txt
♕♖♘♔♗♗♘♖
♖♔♘♘♗♕♖♗
♘♖♗♗♕♔♘♖
♘♕♗♖♔♖♘♗
♘♘♖♔♗♗♕♖
```



## Yabasic

```Yabasic
start$ = "RKR"
piece$ = "QNN"

for piece = 1 to len(piece$)
    pos = int(ran(len(start$) + 1)) + 1
    start$ = left$(start$, pos-1) + mid$(piece$, piece, 1) + right$(start$, len(start$) - pos + 1)
next
pos = int(ran(len(start$) + 1)) + 1
start$ = left$(start$, pos-1) + "B" + right$(start$, len(start$) - pos + 1)
pos = pos + 1 + 2 * int(int(ran(len(start$) - pos)) / 2)
start$ = left$(start$, pos-1) + "B" + right$(start$, len(start$) - pos + 1)
print start$

```



## zkl

```zkl
const pieces="KQRrBbNN";
starts:=pieces:Utils.Helpers.permuteW(_).filter(fcn(p){
   I:=p.index;
   I("B") % 2 != I("b") % 2 and // Bishop constraint.
   // King constraint.
   ((I("r") < I("K") and I("K") < I("R")) or
    (I("R") < I("K") and I("K") < I("r")))
}).pump(List,"concat","toUpper"):Utils.Helpers.listUnique(_);
```


```zkl
N:=starts.len(); println(N);
glyphs:=Dictionary("K","\u2654", "Q","\u2655", "R","\u2656", "B","\u2657", "N","\u2658");
// pick some random starts and transform BBNRKQRN to glyphs
do(10){ starts[(0).random(N)].apply(glyphs.find).println() }
```

```txt
960
♗♕♘♖♘♔♖♗
♖♘♗♔♖♗♘♕
♖♗♘♔♗♕♖♘
♘♖♘♗♗♔♕♖
♘♘♗♖♕♔♖♗
♘♖♕♔♗♖♘♗
♘♖♗♘♕♔♖♗
♖♘♗♔♕♘♖♗
♖♔♖♕♘♘♗♗
♕♗♖♘♗♔♘♖
```

