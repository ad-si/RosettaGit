+++
title = "15 puzzle solver"
description = ""
date = 2019-02-05T16:59:30Z
aliases = []
[extra]
id = 21515
[taxonomies]
categories = ["Games", "task"]
tags = []
languages = [
  "cpp",
  "fsharp",
  "fortran",
  "go",
  "julia",
  "pascal",
  "phix",
  "python",
  "racket",
  "rust",
]
+++

## Task
Your task is to write a program that finds a solution in the fewest moves possible single moves to a random [Fifteen Puzzle Game](https://en.wikipedia.org/wiki/15_puzzle).<br />
For this task you will be using the following puzzle:<br />

```txt
15 14  1  6
 9 11  4 12
 0 10  7  3
13  8  5  2
```

<br />
Solution:
```txt

 1  2  3  4
 5  6  7  8
 9 10 11 12
13 14 15  0

```


The output must show the moves' directions, like so: left, left, left, down, right... and so on.<br />
There are two solutions, of fifty-two moves:

rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd

rrruldluuldrurdddluulurrrdlddruldluurddlulurruldrrdd

see: [Pretty Print of Optimal Solution](http://www.rosettacode.org/wiki/15_puzzle_solver/Optimal_solution)

Finding either one, or both is an acceptable result.


;Extra credit.
Solve the following problem:

```txt

  0 12  9 13
 15 11 10 14
  3  7  2  5
  4  8  6  1

```



;Related Task:
* [15 puzzle game](/tasks/15_Puzzle_Game)
* [A* search algorithm](/tasks/A* search algorithm)





## C++

[see](http://www.rosettacode.org/wiki/15_puzzle_solver/20_Random) for an analysis of 20 randomly generated 15 puzzles solved with this solver.

### =The Solver=


```cpp

// Solve Random 15 Puzzles : Nigel Galloway - October 18th., 2017
class fifteenSolver{
  const int Nr[16]{3,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3}, Nc[16]{3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2};
  int n{},_n{}, N0[100]{},N3[100]{},N4[100]{};
  unsigned long N2[100]{};
  const bool fY(){
    if (N2[n]==0x123456789abcdef0) {std::cout<<"Solution found in "<<n<<" moves :"; for (int g{1};g<=n;++g) std::cout<<(char)N3[g]; std::cout<<std::endl; return true;};
    if (N4[n]<=_n) return fN();
    return false;
  }
  const bool                     fN(){
    if (N3[n]!='u' && N0[n]/4<3){fI(); ++n; if (fY()) return true; --n;}
    if (N3[n]!='d' && N0[n]/4>0){fG(); ++n; if (fY()) return true; --n;}
    if (N3[n]!='l' && N0[n]%4<3){fE(); ++n; if (fY()) return true; --n;}
    if (N3[n]!='r' && N0[n]%4>0){fL(); ++n; if (fY()) return true; --n;}
    return false;
  }
  void fI(){
    const int           g = (11-N0[n])*4;
    const unsigned long a = N2[n]&((unsigned long)15<<g);
    N0[n+1]=N0[n]+4; N2[n+1]=N2[n]-a+(a<<16); N3[n+1]='d'; N4[n+1]=N4[n]+(Nr[a>>g]<=N0[n]/4?0:1);
  }
  void fG(){
    const int           g = (19-N0[n])*4;
    const unsigned long a = N2[n]&((unsigned long)15<<g);
    N0[n+1]=N0[n]-4; N2[n+1]=N2[n]-a+(a>>16); N3[n+1]='u'; N4[n+1]=N4[n]+(Nr[a>>g]>=N0[n]/4?0:1);
  }
  void fE(){
    const int           g = (14-N0[n])*4;
    const unsigned long a = N2[n]&((unsigned long)15<<g);
    N0[n+1]=N0[n]+1; N2[n+1]=N2[n]-a+(a<<4); N3[n+1]='r'; N4[n+1]=N4[n]+(Nc[a>>g]<=N0[n]%4?0:1);
  }
  void fL(){
    const int           g = (16-N0[n])*4;
    const unsigned long a = N2[n]&((unsigned long)15<<g);
    N0[n+1]=N0[n]-1; N2[n+1]=N2[n]-a+(a>>4); N3[n+1]='l'; N4[n+1]=N4[n]+(Nc[a>>g]>=N0[n]%4?0:1);
  }
public:
  fifteenSolver(int n, unsigned long g){N0[0]=n; N2[0]=g;}
  void Solve(){for(;not fY();++_n);}
};

```


### =The Task=


```cpp

int main (){
  fifteenSolver start(8,0xfe169b4c0a73d852);
  start.Solve();
}

```

### Output

```txt

Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd

real    0m0.795s
user    0m0.794s
sys     0m0.000s

```


## F#

### The Function


```fsharp

// A Naive 15 puzzle solver using no memory. Nigel Galloway: October 6th., 2017
let Nr,Nc = [|3;0;0;0;0;1;1;1;1;2;2;2;2;3;3;3|],[|3;0;1;2;3;0;1;2;3;0;1;2;3;0;1;2|]
type G= |N |I |G |E |L
type N={i:uint64;g:G list;e:int;l:int}
let fN     n=let g=(11-n.e)*4 in let a=n.i&&&(15UL<<<g)
             {i=n.i-a+(a<<<16);g=N::n.g;e=n.e+4;l=n.l+(if Nr.[int(a>>>g)]<=n.e/4 then 0 else 1)}
let fI     i=let g=(19-i.e)*4 in let a=i.i&&&(15UL<<<g)
             {i=i.i-a+(a>>>16);g=I::i.g;e=i.e-4;l=i.l+(if Nr.[int(a>>>g)]>=i.e/4 then 0 else 1)}
let fG     g=let l=(14-g.e)*4 in let a=g.i&&&(15UL<<<l)
             {i=g.i-a+(a<<<4) ;g=G::g.g;e=g.e+1;l=g.l+(if Nc.[int(a>>>l)]<=g.e%4 then 0 else 1)}
let fE     e=let l=(16-e.e)*4 in let a=e.i&&&(15UL<<<l)
             {i=e.i-a+(a>>>4) ;g=E::e.g;e=e.e-1;l=e.l+(if Nc.[int(a>>>l)]>=e.e%4 then 0 else 1)}
let fL=let l=[|[I;E];[I;G;E];[I;G;E];[I;G];[N;I;E];[N;I;G;E];[N;I;G;E];[N;I;G];[N;I;E];[N;I;G;E];[N;I;G;E];[N;I;G];[N;E];[N;G;E];[N;G;E];[N;G];|]
       (fun n g->List.except [g] l.[n] |> List.map(fun n->match n with N->fI |I->fN |G->fE |E->fG))
let solve n g l=let rec solve n=match n with // n is board, g is pos of 0, l is max depth
                                |n when n.i =0x123456789abcdef0UL->Some(n.g)
                                |n when n.l>l                    ->None
                                |g->let rec fN=function h::t->match solve h with None->fN t |n->n
                                                       |_->None
                                    fN (fL g.e (List.head n.g)|>List.map(fun n->n g))
                solve {i=n;g=[L];e=g;l=0}
  let n = Seq.collect fN n

```


### The Task


```fsharp

let test n g=match [1..15]|>Seq.tryPick(solve n g) with
               Some n->n|>List.rev|>List.iter(fun n->printf "%c" (match n with N->'d'|I->'u'|G->'r'|E->'l'|L->'\u0000'));printfn " (%n moves)" (List.length n)
              |_     ->printfn "No solution found"
test 0xfe169b4c0a73d852UL 8

```

### Output

```txt

rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd (52 moves)

```



## Fortran


### The Plan

There is a straightforward method for dealing with problems of this sort, the Red Tide. Imagine a maze and pouring red-dyed water into the "entry" - eventually, red water issues forth from the exit, or, back pressure will indicate that there is no such path. In other words, from the starting position find all possible positions that can be reached in one step, then from those positions, all possible positions that can be reached in one step from them, and so on. Eventually, either the stopping position will be reached, or, there will be no more new (still dry) positions to inspect. What is needed is some way of recording whether a position has been marked red or not, and an arrangement for identifying positions that are on the leading edge of the tide as it spreads. Keeping as well some sort of information identifying the path followed by each droplet, so that when a droplet spreads to the end position, its path from the source can be listed.

One could imagine an array FROM whose value for a given position identifies the position from which a step was taken to reach it. The value could be a number identifying that position or be a (sixteen element) list of the placement of the tiles possibly encoded into one number. An index for FROM would span the values zero to fifteen, and there would be sixteen dimensions... Alternatively, an array MOVE would record the move that led to its element's positiion: four possibilities would require just two bits for each element. But 16<sup>16</sup> is about 10<sup>20</sup> Avogadro's constant is 6·0221415x10<sup>23</sup>  Oh well.

Since there are only 16! possible board layouts, only about a millionth of the array would be in use (16!/16<sup>16</sup> = 0·0000011342267) which is rather a stiff price to pay for a convenient relationship between a board layout and the corresponding location in the array. Actually, calculating a sixteen-dimensional location in an array is not so trivial and earlier compilers would not allow so many dimensions anyway. An alternative calculation might serve. This is the method of "hash tables", whereby, given a key (here, the values of the tiles in the sixteen places of the board), calculate a number, H, by some expedient and use that to index into a table. Entries are added sequentially to the table of positions but the hash number for a position is arbitrary so an array is used: INDEX(H) for the first position stored is one, and for the second position with its value of H, INDEX(H) is two, and so on, Two different positions may well yield the same hash number in a variant of the "birthday paradox", so the value of INDEX(H) is really a pointer to the start of a linked-list of table entries for different positions that all have the same hash number. The length of such chains is expected to be short because the hash number selects within a large INDEX array. Searching should be swift as for a given position, either INDEX(H) will be zero (meaning that the position is unknown), or, there will be one or two entries to probe to see if their position matches.

Because a board position involves sixteen numbers, each in the range of zero to fifteen, it is irresistible to have the board layout described by <code>INTEGER*1 BOARD(16)</code> since eight-bit integers can be defined, though not four-bit integers, alas. Further, an EQUIVALENCE statement can make this storage area the same place that an array <code>INTEGER*4 BORED(4)</code> occupies: no data transfer operations are needed for four 32-bit integers to hold the contenet of sixteen 8-bit integers. Next, the calculations <code>BRD(1) = BORED(1)*16 + BORED(2)</code> and <code>BRD(2) = BORED(3)*16 + BORED(4)</code> will squeeze two four-bit fields into one eight-bit pair, and moreover, do so four pairs at a time in parallel. Thus, array BRD in two 32-bit integers describes a position. A 64-bit integer could be involved instead of two 32-bit integers, but the hash calculation uses BRD(1)*BRD(2) to encourage a good mix. In *The Art of Computer Programming*, Prof. Knuth advises that the wild hash number be reduced to the range 0:M - 1 by taking the remainder, modulo M, where M is a (large) prime number. The index array then is a fixed size, <code>INDEX(0:M - 1)</code> The output shows these calculations for two entries; notably the ordering of the bytes is peculiar, this being a "little-endian" cpu.

Accordingly, a table entry is defined by <code>TYPE AREC</code> with a NEXT (to link to the next table entry in a chain) and a BRD array to describe the board layout the entry is for. The payload for this problem consists of a PREV which identifies the entry from which this position was reached, and MOVE which identifies the move that had been made to do so.

The initial idea was to work from the given starting position, ascertaining all positions that could be reached in one step, then from each of those the positions reachable by a second step, and so on, until the "solved" state is reached. This is somewhat like the "all possible paths" of Quantum Electrodynamics when considering photon paths. Necessarily, on first contact the linked-list of PREV entries will be a minimum-length path. Loops are precluded by passing over any candidate new position that has already been reached and so already has an entry in the table: it can be reached by a path of the same or lesser length. In some games, loops are not possible, or are truncated by a special rule as in chess, when on the third attainment of a position a draw is declared. Then reading a remark in the Phix solution prompted the realisation that the flow could start from the "solved" position and stop on attainment of the specified position; the linked list via PREV could be reported in reverse order to go from the specified position back to the solved position. Some games are not symmetrical, as in chess where a pawn can only advance, but in this game, a move from A to B is just as allowable as a move from B to A - indeed, when checking the moves from a position, the reverse of the move that led to that position is skipped - it would just lead to a position that is already in the table and so be passed over. Similarly, the table does not record the possible moves from a position (there being two, three or four of them) but only the one move that led to the position. While there might be two, three, or four such moves possible (from different positions), only one move from one position is recorded, the one that got there first.

Starting a Red Tide from the "solved" or ZERO position has the advantage that if the table is retained, a new run for a different start position would take advantage of the previous effort. A table based around moves from one start position would be of little use when given a different start position.

Alas, the Red Tide ran into too much dry sand. A sixteen-dimensional volume has a *lot* of space in which it can possess a surface. The revised plan was to spread a Red Tide from the ZERO position and at the same time spread a Blue Tide from the specified start position, hoping that they would meet rather sooner. In *Numerical Methods that Work (Usually)*, F. S. Acton remarks upon "Perverse Formulations", such as "... who insists on solving a boundary-value problem in ordinary differential equations via initial-value techniques may get away with it for a while, but ..." One can easily imagine pouring red paint onto the floor in one place, and blue paint in another place: most of the extension of the puddles is in directions that will not meet. With a single puddle, very little of the expansion is towards the target. Details are shown in the results.

A great deal of computer effort could be avoided if the spread could be given good guidance. For instance, a function that for any given position, gives the minimum number of moves needed to reach the ZERO position from it. Thus, from the start position, evaluate the function for each of the positions that can be reached via the available moves (at most, four), and select that one with the smallest value; repeat until at ZERO. Such a function certainly exists, though our ingenuity may not bring it forth. Perhaps some thought may do so. Perhaps not any time soon. But in any case, there is a definite procedure for generating such a function, and it is simple. Analyse all the board positions (they are finite in number) as in the Red Tide process, for each entry retaining the number of moves needed to reach that position from ZERO. Clearly, this is computable by a Turing Machine and so the function is computable, and so exists.

Such a guide function is rather like a "distance" function, so along these lines, when a minimum-step move sequence is found, the distances of each position from ZERO are calculated by various distance functions and the results shown in the output. Notably, function ZDIST calculates an encodement of the board position by referring to the layout of the ZERO position. It relies on the first square of a position having sixteen possible values, then the second square has fifteen and so on down; the number of possible layouts is 16! rather than 16<sup>16</sup>. Given the list of values of the squares in the ZERO sequence, values that have been taken are marked off (in array AVAIL) and the count of possibilities remaining as each square is identified reduces. The ZDIST number is like an encodement of an integer from its digits, but with a successively-reducing base.

As an experiment, when the Red Tide was poured to completion for a board of three rows and four columns, every position in the stash was presented to ZDIST and the result written out. The filesystem presented difficulties: reading the stash file as a sequential file failed! So, a slog through reading specified record numbers, one after the other, taking half an hour. By contrast, the B6700 filesystem of the 1970s employed a feature called "diagonal I/O" whereby the style of the previous I/O for a file was retained and compared to the style of the new I/O operation: for matching styles, conclusions could be drawn. The first and last few ZDIST values are  0, 105, 1, 328449, 609, 632, 4, 3271809, 3312009, ... 287247191, 446727869, 287039663. That last is entry 23950800 which is 12!/2: every possible attainable position has been tested and stored in the stash (once each), since a parity condition means that half the layout combinations have one parity and half the other, the possible moves being unable to change the parity. Alas, the ZDIST figures do not show anything so simple as odd/even for this. Every value should be unique, but in the past it has suspiciously often been easy to find a proof of a desired situation, so a test: sort the values and check. UltraEdit ran for a long time, then reported that there was insufficient memory to maintain an "undo" list. Very well. And then it wiped the file! Fortunately, the stash was still intact. On restarting in Linux the "sort" utility was available, and after some impressive running of all six cpus at 100% in cyclic bursts, I was looking at a file in a mad order: 0, 1, 10, 100, 1000, 10000, ... Apparently some sort of "word" order, even though the text file used leading spaces so that the numbers were all aligned right. As usual, the "man"ual information was brief and vague, but option "g" for "general number" looked likely, and so it proved. In sorted order: 0, 1, 4, 8, 9, 11, 12, 13, ... 479001589, 479001590, 479001593, 479001594, 479001597, 479001598. And 12! = 479001600. All values were unique.

The ZDIST function would be a candidate for a hash function, as it looks to give a good spray. But it requires rather more computation. It is always possible to calculate a "perfect" hash function, one that not only gives a distinct integer value for every possible key but also produces no gaps, so if there are N keys, there are N hash values in the range 1:N (or 0:N - 1) and the mapping is one to one. There even exist "hyper perfect" hash functions, that possess useful ordering properties: an example is the conversion of dates to a [day number](/tasks/Calendar#Fortran), which is one-to-one, without gaps, and ordered by date. However, the calculation of such perfection may be lengthy, so a fast hash is preferable, except for special cases.

In the event, when running, TaskInfo showed that almost all of the time was spent in the filesystem's I/O procedures, no "user time" was visible. The hash calculation indeed was fast, but the I/O was slow. Probably because the disc file was scattered in blocks across the disc device (actually a solid-state drive) and finding the appropriate block took a lot of messing about. In the past, a large disc file would be in one or very few pieces on the actual disc, with a straightforward direct calculation between a record number and a disc's corresponding cylinder, surface, track and sector. These days, the operating system uses "spare" memory to hold the content of recently-used disc blocks, but alas, each index file is 781MB, and the stash files are over 3,000MB. Some systems have many gigabytes of memory, but this one has 4GB.


### The Code

The source code started off as a mainline only, but then facilities were added and service routines became helpful. This prompted the use of F90's MODULE facility so that the PARAMETER statement could be used to describe the shape of the board with this information available to each routine without the need for passing the values as parameters or messing with COMMON storage, or repeating the PARAMETER statement in each routine. Otherwise, literal constants such as 4 would appear in various places. These appearances could now be documented by using the appropriate name such as <code>NR</code> and <code>NC</code> rather than just <code>4</code> and similar. However, inside FORMAT statements the use of <code><NR - 1></code> (and not <code><NC - 1></code>) rather than 3 will succeed only if the compiler accepts this usage, and not all do. More complex calculations involving the board size have not been attempted. The PARAMETER facility is useful only for simple calculations, and the compiler typically does not allow the use of many functions, even library functions, in expressions. Considerations such as a 4x4 board having 16 squares is easy, but the consequences of this count fitting into a four-bit binary field are not, thus the equivalences involving <code>BOARD, BORED and BOAR</code> are not general for different board shapes, nor are the column headings adjustable. Similarly, subroutine UNPACK does not attempt to use a loop but employs explicit code.

This approach is also followed in the calculation of the hash code. Not using a loop (for two items only) but, by writing out the product rather than using the compiler's built-in <code>PRODUCT</code> function. A startling difference results:

```txt

59:         H = MOD(ABS(PRODUCT(BRD)),APRIME)
004016E3   mov         esi,1
004016E8   mov         ecx,1
004016ED   cmp         ecx,2
004016F0   jg          MAIN$SLIDESOLVE+490h (0040171e)
004016F2   cmp         ecx,1
004016F5   jl          MAIN$SLIDESOLVE+46Eh (004016fc)
004016F7   cmp         ecx,2
004016FA   jle         MAIN$SLIDESOLVE+477h (00401705)
004016FC   xor         eax,eax
004016FE   mov         dword ptr [ebp-54h],eax
00401701   dec         eax
00401702   bound       eax,qword ptr [ebp-54h]
00401705   imul        edx,ecx,4
00401708   mov         edx,dword ptr H (00473714)[edx]
0040170E   imul        edx,esi
00401711   mov         esi,edx
00401713   mov         eax,ecx
00401715   add         eax,1
0040171A   mov         ecx,eax
0040171C   jmp         MAIN$SLIDESOLVE+45Fh (004016ed)
0040171E   mov         eax,esi
00401720   cmp         eax,0
00401725   jge         MAIN$SLIDESOLVE+49Bh (00401729)
00401727   neg         eax
00401729   mov         edx,10549h
0040172E   mov         dword ptr [ebp-54h],edx
00401731   cdq
00401732   idiv        eax,dword ptr [ebp-54h]
00401735   mov         eax,edx
00401737   mov         dword ptr [H (00473714)],eax
60:         write (6,*) H,bored

```

Whereas by writing out the product,

```txt

59:         H = MOD(ABS(BRD(1)*BRD(2)),APRIME)
004016E3   mov         eax,dword ptr [BRD (00473718)]
004016E9   imul        eax,dword ptr [BRD+4 (0047371c)]
004016F0   cmp         eax,0
004016F5   jge         MAIN$SLIDESOLVE+46Bh (004016f9)
004016F7   neg         eax
004016F9   mov         esi,10549h
004016FE   cdq
004016FF   idiv        eax,esi
00401701   mov         eax,edx
00401703   mov         dword ptr [H (00473714)],eax
60:         write (6,*) H,bored

```

(The source below has the ABS outside the MOD: I don't care [[Talk:Carmichael_3_strong_pseudoprimes|what sort of mod]] is used here, just that negative numbers be as scrambled as positive) In both cases the array indexing is checkable by the compiler at compile time so that there need be no run-time checking on that. Such bound checking may be not as strict as might be expected when EQUIVALENCE tricks are involved. In tests with a variant board size of 3x4, the board position array was declared BOARD(N) where N = 12, but was still equivalenced to BORED(4) and so still allowed room for sixteen elements in BOARD. Subroutine UNPACK was not written to deal with anything other than a 4x4 board and so accessed elements 13, 14, 15, and 16 of BOARD that were outside its declared upper bound of 12, but no run-time objection was made. Similarly with a 3x3 board.

Granted a flexible pre-processor scheme (as in pl/i, say) one could imagine a menu of tricks being selected from according to the board shape specified, but without such a facility, the constants are merely named rather than literal. Any change, such as to a 3x4 board, can be made by adjusting the appropriate PARAMETER and many usages will adjust accordingly. Others will require adjustment by the diligent programmer for good results. In the absence of a pre-processor one could present the various possible code sequences surrounded by tests as in
```Fortran
IF (NR.EQ.4) THEN
 code specialised for NR = 4
ELSE IF (NR.EQ.3) THEN
 code specialised for NR = 3
END IF
```

and hope that the compiler would carry forward the actual value of <code>NR</code> into the IF-statement's conditional expression, recognise that the result is also constant (for the current compilation with a particular value of <code>NR</code>) and so generate code only for the case that the expression came out as *true*, without any test to select this being employed in the compiled code. This soon becomes a tangle of combinations, and has not been attempted. And there could be difficulties too. One wonders if, say, with NR = 3, the specialised code for the NR = 4 case could contain a mention of element 4 of an array which is actually of size NR. Would the compiler regard this as an error, given that it will be discarding this code anyway? Even if one used NR rather than a literal such as 4 there could still be problems, such as code calling for a division by (NR - 3).

Implementing the plan had its problems. Since the INDEX array is accessed randomly it should be in memory, but alas if it is large (and now there are two of them) the Compaq Visual Fortran 6.6 F90/95 compiler complains "total image size exceeds max (268435456): image may not run" - but if it is not so large, when many entries are made the hash separation will be heavily overloaded and the chains of entries with equal hash codes will not be short. One could possibly mess about with allocatable arrays as a different storage scheme is used for them, but instead, a disc file for the index array as well as a stash file for the entries. This also would mean that the stash and its index could survive for another run, otherwise if the index data were in memory only, some scheme would be needed to save the index, or to redevelop the index from the stash file on a restart. Both the stash and index files require random access, but the stash file grows sequentially. The index file however has to start full-sized, with all values zero. The obvious ploy of writing zero values to the file as a sequential file works well enough, but on re-opening the index file for random access, there are complaints about accessing a non-existing record. By initialising the file via random access, writing a zero to record one, two, three, etc. sequentially, no such complaint appeared and everything worked. But the initialisation was *very* slow, taking many minutes. Oddly, writing a zero to the *last* record without doing anything to intervening records not only worked but did so rapidly. It appeared that the intervening records were not prepared by the I/O subsystem at all, as very little I/O action took place. At a guess, only if a filesystem's allocation block was written to was that block made into a proper file piece with all zero values.

During these developments, a mistype produced an odd result. With F90, many array operations became possible, and accidentally, I typed <code>WRITE (WRK,REC = array) *etc*</code> but omitted to specify which element of the array was to be used, as in <code>WRITE (WRK,REC = array(1)) *etc*</code> A positive interpretation of this construction would be to hope that the I/O list (the *etc*) would be written to multiple records of I/O unit WRK, to array(1), array(2), and so on without re-evaluation of the I/O list. Alas, a file containing 19MB of zeroes resulted. Obviously some mistake. Similarly, one could hope that <code>WRITE (OUT,66) stuff</code> where OUT(1) = 6 (for standard output), and OUT(2) = 10 (a disc file, logging what has been written) would save on repeating WRITE statements, but alas, compatibility with older Fortran requires that such a statement places its output into the storage occupied by array OUT. This might be avoided by a variant form, <code>WRITE (UNIT = OUT,66) stuff</code> to signify that I/O unit numbers are being given, but alas, the compiler doesn't agree.


### Source


```Fortran
      SUBROUTINE PROUST(T)	!Remembrance of time passed.
       DOUBLE PRECISION T	!The time, in seconds. Positive only, please.
       DOUBLE PRECISION S	!A copy I can mess with.
       TYPE TIMEWARP		!Now prepare a whole lot of trickery for expressing the wait time.
        INTEGER LIMIT		!The upper limit for the usage.
        INTEGER STEP		!Conversion to the next unit.
        CHARACTER*4 NAME	!A suitable abbreviated name for the accepted unit.
       END TYPE TIMEWARP	!Enough of this.
       INTEGER CLOCKCRACK	!How many different units might I play with?
       PARAMETER (CLOCKCRACK = 5)	!This should so.
       TYPE(TIMEWARP) TIME(CLOCKCRACK)	!One set, please.
       PARAMETER (TIME = (/		!The mention of times lost has multiple registers.
     1  TIMEWARP(99, 60,"secs"),	!Beware 99.5+ rounding up to 100.
     2  TIMEWARP(99, 60,"mins"),	!Likewise with minutes.
     3  TIMEWARP(66, 24,"hrs!"),	!More than a few days might as well be in days.
     4  TIMEWARP(99,365,"days"),	!Too many days, and we will speak of years.
     5  TIMEWARP(99,100,"yrs!")/))	!And the last gasp converts to centuries.
       INTEGER CC		!A stepper for these selections.
       CHARACTER*4 U		!The selected unit.
       INTEGER MSG		!The mouthpiece.
       COMMON/IODEV/ MSG	!Used in common.
        S = T			!A working copy.
        DO CC = 1,CLOCKCRACK	!Now re-assess DT, with a view to announcing a small number.
          IF (S.LE.TIME(CC).LIMIT) THEN	!Too big a number?
            U = TIME(CC).NAME			!No, this unit will suffice.
            GO TO 10				!Make off to use it.
          END IF			!But if the number is too big,
          S = S/TIME(CC).STEP		!Escalate to the next larger unit.
        END DO 			!And see if that will suffice.
        U = "Cys!!"		!In case there are too many years, this is the last gasp.
   10   WRITE (MSG,11) S,U	!Say it.
   11   FORMAT (F7.1,A4,$)	!But don't finish the line.
       END SUBROUTINE PROUST	!A sigh.

       CHARACTER*15 FUNCTION HMS(T)	!Report the time of day.
Careful! Finite precision and binary/decimal/sexagesimal conversion could cause 2:30:00am. to appear as 2:29:60am.
        DOUBLE PRECISION S,T	!Seconds (completed) into the day.
        INTEGER H,M		!More traditional units are to be extracted.
        INTEGER SECONDSINDAY	!A canonical day.
        PARAMETER (SECONDSINDAY = 24*60*60)	!Of nominal seconds.
        CHARACTER*15 TEXT	!A scratchpad.
         H = T			!Truncate into an integer.
         S = T - (H - 1)/SECONDSINDAY*SECONDSINDAY	!Thus allow for midnight = hour 24.
         IF (S.EQ.SECONDSINDAY/2) THEN	!This might happen.
           TEXT = "High Noon!"	!Though the chances are thin.
         ELSE IF (S.EQ.SECONDSINDAY) THEN	!This is synonymous with the start of the next day.
           TEXT = "Midnight!"	!So this presumably won't happen.
         ELSE		!But more likely are miscellaneous values.
           H = S/3600		!Convert seconds into whole hours completed.
           S = S - H*3600	!The remaining time.
           M = S/60		!Seconds into minutes completed.
           S = S - M*60		!Remove them.
           IF (S .GE. 59.9995D0) THEN	!Via format F6.3, will this round up to 60?
             S = 0		!Yes. Curse recurring binary sequences for decimal.
             M = M + 1		!So, up the minute count.
             IF (M.GE.60) THEN	!Is there an overflow here too?
               M = 0		!Yes.
               H = H + 1	!So might appear 24:00:00.000 though it not be Midnight!
             END IF		!So much for twiddling the minutes.
           END IF		!And twiddling the hours.
           IF (H.LT.12) THEN	!A plague on the machine mentality.
             WRITE (TEXT,1) H,M,S,"am."	!Ante-meridian.
    1        FORMAT (I2,":",I2,":",F6.3,A3)	!Thus.
            ELSE		!For the post-meridian, H >= 12.
             IF (H.GT.12) H = H - 12	!Adjust to civil usage. NB! 12 appears.
             WRITE (TEXT,1) H,M,S,"pm."	!Thus. Post-meridian.
           END IF	!So much for those fiddles.
           IF (TEXT(4:4).EQ." ") TEXT(4:4) = "0"	!Now help hint that the
           IF (TEXT(7:7).EQ." ") TEXT(7:7) = "0"	! character string is one entity.
         END IF		!So much for preparation.
         HMS = TEXT	!The result.
       END FUNCTION HMS	!Possible compiler confusion if HMS is invoked in a WRITE statement.

       DOUBLE PRECISION FUNCTION NOWWAS(WOT)	!Ascertain the local time for interval assessment.
Compute with whole day numbers, to avoid day rollover annoyances.
Can't use single precision and expect much discrimination within a day.
C I'd prefer a TIMESTAMP(Local) and a TIMESTAMP(GMT) system function.
C Quite likely, the system separates its data to deliver the parameters, which I then re-glue.
        INTEGER WOT	!What sort of time do I want?
        REAL*8 TIME	!A real good time.
        INTEGER MARK(8)	!The computer's clock time will appear here, but fragmented.
         IF (WOT.LE.0) THEN	!Just the CPU time for this.
           CALL CPU_TIME(TIME)	!Apparently in seconds since starting.
          ELSE			!But normally, I want a time-of-day now.
           CALL DATE_AND_TIME(VALUES = MARK)	!Unpack info that I will repack.
c           WRITE (6,1) MARK
c    1      FORMAT ("The computer clock system reports:",
c     1      /"Year",I5,", Month",I3,", Day",I3,
c     2      /" Minutes from GMT",I5,
c     3      /" Hour",I3,", Minute",I3,",Seconds",I3,".",I3)
           TIME = (MARK(5)*60 + MARK(6))*60 + MARK(7) + MARK(8)/1000D0	!By the millisecond, to seconds.
           IF (WOT.GT.1) TIME = TIME - MARK(4)*60	!Shift back to GMT, which may cross a day boundary.
c          TIME = DAYNUM(MARK(1),MARK(2),MARK(3)) + TIME/SECONDSINDAY	!The fraction of a day is always less than 1 as MARK(5) is declared < 24.
           TIME = MARK(3)*24*60*60 + TIME	!Not bothering with DAYNUM, and converting to use seconds rather than days as the unit.
         END IF			!A simple number, but, multiple trickeries. The GMT shift includes daylight saving's shift...
         NOWWAS = TIME		!Thus is the finger of time found.
       END FUNCTION NOWWAS	!But the Hand of Time has already moved on.

      MODULE SLIDESOLVE		!Collect the details for messing with the game board.
       INTEGER NR,NC,N				!Give names to some sizes.
       PARAMETER (NR = 4, NC = 4, N = NR*NC)	!The shape of the board.
       INTEGER*1 BOARD(N),TARGET(N),ZERO(N)	!Some scratchpads.
       INTEGER BORED(4)				!A re-interpretation of the storage containing the BOARD.
       CHARACTER*(N) BOAR			!Another, since the INDEX function only accepts these.
       EQUIVALENCE (BORED,BOARD,BOAR)		!All together now!
       CHARACTER*1 DIGIT(0:35)			!This will help to translate numbers to characters.
       PARAMETER (DIGIT = (/"0","1","2","3","4","5","6","7","8","9",
     1  "A","B","C","D","E","F","G","H","I","J",	!I don't anticipate going beyond 15.
     2  "K","L","M","N","O","P","Q","R","S","T",	!But, for completeness...
     3  "U","V","W","X","Y","Z"/))			!Add a few more.
      CONTAINS
       SUBROUTINE SHOW(NR,NC,BOARD)	!The layout won't work for NC > 99...
        INTEGER NR,NC		!Number of rows and columns.
        INTEGER*1 BOARD(NC,NR)	!The board is stored transposed, in Furrytran!
        INTEGER R,C		!Steppers.
        INTEGER MSG		!Keep the compiler quiet.
        COMMON/IODEV/ MSG	!I talk to the trees...
         WRITE (MSG,1) (C,C = 1,NC)	!Prepare a heading.
    1    FORMAT ("Row|",9("__",I1,:),90("_",I2,:))	!This should suffice.
         DO R = 1,NR		!Chug down the rows, for each showing a succession of columns.
           WRITE (MSG,2) R,BOARD(1:NC,R)	!Thus, successive elements of storage. Storage style is BOARD(column,row).
    2      FORMAT (I3,"|",99I3)		!Could use parameters, but enough.
         END DO			!Show columns across and rows down, despite the storage order.
       END SUBROUTINE SHOW	!Remember to transpose the array an odd number of times.

       SUBROUTINE UNCRAM(IT,BOARD)	!Recover the board layout..
        INTEGER IT(2)		!Two 32-bit integers hold 16 four-bit fields in a peculiar order.
        INTEGER*1 BOARD(*)	!This is just a simple, orderly sequence of integers.
        INTEGER I,HIT		!Assistants.
         DO I = 0,8,8		!Unpack into the work BOARD.
           HIT = IT(I/8 + 1)		!Grab eight positions, in four bits each..
                                BOARD(I + 5) = IAND(HIT,15)	!The first is position 5.
           HIT = ISHFT(HIT,-4); BOARD(I + 1) = IAND(HIT,15)	!Hex 48372615
           HIT = ISHFT(HIT,-4); BOARD(I + 6) = IAND(HIT,15)	!and C0BFAE9D
           HIT = ISHFT(HIT,-4); BOARD(I + 2) = IAND(HIT,15)	!For BOARD(1) = 1, BOARD(2) = 2,...
           HIT = ISHFT(HIT,-4); BOARD(I + 7) = IAND(HIT,15)	!This computer is (sigh) little-endian.
           HIT = ISHFT(HIT,-4); BOARD(I + 3) = IAND(HIT,15)	!Rather than mess with more loops,
           HIT = ISHFT(HIT,-4); BOARD(I + 8) = IAND(HIT,15)	!Explicit code is less of a brain strain.
           HIT = ISHFT(HIT,-4); BOARD(I + 4) = IAND(HIT,15)	!And it should run swiftly, too...
         END DO				!Only two of them.
       END SUBROUTINE UNCRAM	!A different-sized board would be a problem too.

       INTEGER*8 FUNCTION ZDIST(BOARD)	!Encode the board's positions against the ZERO sequence.
        INTEGER*1 BOARD(N)	!The values of the squares.
        LOGICAL*1 AVAIL(N)	!The numbers will be used one-by-one to produce ZC.
        INTEGER BASE		!This is not a constant, such as ten.
        INTEGER M,IT		!Assistants.
         AVAIL = .TRUE.			!All numbers are available.
         BASE = N			!The first square has all choices.
         ZDIST = 0			!Start the encodement of choices.
         DO M = 1,N			!Step through the board's squares.
           IT = BOARD(M)			!Grab the square's number. It is the index into ZERO.
           IF (IT.EQ.0) IT = N			!But in ZERO, the zero is at the end, damnit.
           AVAIL(IT) = .FALSE.			!This number is now used.
           ZDIST = ZDIST*BASE + COUNT(AVAIL(1:IT - 1))	!The number of available values to skip to reach it.
           BASE = BASE - 1			!Option count for the next time around.
         END DO				!On to the next square.
       END FUNCTION ZDIST	!ZDIST(ZERO) = 0.

       SUBROUTINE REPORT(R,WHICH,MOVE,BRD)	!Since this is invoked in two places.
        INTEGER R		!The record number of the position.
        CHARACTER*(*) WHICH	!In which stash.
        CHARACTER*1 MOVE	!The move code.
        INTEGER BRD(2)		!The crammed board position.
        INTEGER*1 BOARD(N)	!Uncrammed for nicer presentation.
        INTEGER*8 ZC		!Encodes the position in a mixed base.
        INTEGER ZM,ZS		!Alternate forms of distance.
        DOUBLE PRECISION ZE	!This is Euclidean.
        INTEGER MSG		!Being polite about usage,
        COMMON/IODEV/MSG	!Rather than mysterious constants.
         CALL UNCRAM(BRD,BOARD)		!Isolate the details.
         ZM = MAXVAL(ABS(BOARD - ZERO))			!A norm. |(x,y)| = r gives a square shape.
         ZS = SUM(ABS(BOARD - ZERO))			!A norm. |(x,y)| = r gives a diamond shape.
         ZE = SQRT(DFLOAT(SUM((BOARD - ZERO)**2)))	!A norm. |(x,y)| = r gives a circle.
         ZC = ZDIST(BOARD)				!Encodement against ZERO.
         WRITE (MSG,1) R,WHICH,MOVE,DIGIT(BOARD),ZM,ZS,ZE,ZC	!After all that,
    1    FORMAT (I11,A6,A5,1X,"|",<NR - 1>(<NC>A1,"/"),<NC>A1,"|",	!Show the move and the board
     1    2I8,F12.3,I18)					!Plus four assorted distances.
       END SUBROUTINE REPORT	!Just one line is produced.

       SUBROUTINE PURPLE HAZE(BNAME)	!Spreads a red and a blue tide.
        CHARACTER*(*) BNAME		!Base name for the work files.
        CHARACTER*(N) BRAND		!Name part based on the board sequence.
        CHARACTER*(LEN(BNAME) + 1 + N + 4) FNAME	!The full name.
Collect the details for messing with the game board.
        CHARACTER*4 TIDE(2)			!Two tides will spread forth.
        PARAMETER (TIDE = (/" Red","Blue"/))	!With these names.
        INTEGER LZ,LOCZ(2),LOCI(2),ZR,ZC	!Location via row and column.
        EQUIVALENCE(LOCZ(1),ZR),(LOCZ(2),ZC)	!Sometimes separate, sometimes together.
        INTEGER WAY(4),HENCE,W,M,D,WAYS(2,4)	!Direction codes.
        PARAMETER (WAY = (/   +1,  -NC,   -1,  +NC/))	!Directions for the zero square, in one dimension.
        PARAMETER (WAYS = (/0,+1, -1,0, 0,-1, +1,0/))	!The same, but in (row,column) style.
        CHARACTER*1 WNAMEZ(0:4),WNAMEF(0:4)	!Names for the directions.
        PARAMETER (WNAMEZ = (/" ","R","U","L","D"/))	!The zero square's WAYS.
        PARAMETER (WNAMEF = (/" ","L","D","R","U"/))	!The moved square's ways are opposite.
Create two hashed stashes. A stash file and its index file, twice over.
        INTEGER APRIME				!Determines the size of the index.
        PARAMETER (APRIME = 199 999 991)	!Prime 11078917. Prime 6666666 = 116 743 349. Perhaps 1999999973?
        INTEGER HCOUNT(2),NINDEX(2)		!Counts the entries in the stashes and their indices.
        INTEGER P,HIT				!Fingers to entries in the stash.
        INTEGER SLOSH,HNEXT			!Advances from one surge to the next.
        INTEGER IST(2),LST(2),SURGE(2)		!Define the perimeter of a surge.
        INTEGER HEAD,LOOK			!For chasing along a linked-list of records.
        TYPE AREC				!Stores the board position, and helpful stuff.
         INTEGER NEXT					!Links to the next entry that has the same hash value.
         INTEGER PREV					!The entry from which this position was reached.
         INTEGER MOVE					!By moving the zero in this WAY.
         INTEGER BRD(2)					!Squeezed representation of the board position.
        END TYPE AREC				!Greater compaction (especially of MOVE) would require extra crunching.
        INTEGER LREC				!Record length, in INTEGER-size units. I do some counting.
        PARAMETER (LREC = 5)			!For the OPEN statement.
        TYPE(AREC) ASTASH,APROBE		!I need two scratchpads.
        INTEGER NCHECK				!Number of new positions considered.
        INTEGER NLOOK(2),PROBES(2),NP(2),MAXP(2)!Statistics for the primary and secondary searches resulting.
        LOGICAL SURGED(2)			!A SLOSH might not result in a SURGE.
Catch   the red/blue meetings, if any.
        INTEGER MANY,LONG			!They may be many, and, long.
        PARAMETER (MANY = 666,LONG = 66)	!This should do.
        INTEGER NMET,MET(2,MANY)		!Identify the meeting positions, in their own stash.
        INTEGER NTRAIL,TRAIL(LONG)		!Needed to follow the moves.
        INTEGER NS,LS(MANY)			!Count the shove sequences.
        CHARACTER*128 SHOVE(MANY)		!Record them.
Conglomeration of support stuff.
        LOGICAL EXIST				!For testing the presence of a disc file.
        INTEGER I,IT				!Assistants.
        DOUBLE PRECISION T1,T2,E1,E2,NOWWAS	!Time details.
        CHARACTER*15 HMS			!A clock.
        INTEGER MSG,KBD,WRK(2),NDX(2)	!I/O unit numbers.
        COMMON/IODEV/ MSG,KBD,WRK,NDX	!I talk to the trees...
         NS = 0	!No shove sequences have been found.
Concoct some disc files for storage areas, reserving the first record of each as a header.
   10    BOARD = ZERO	!The red tide spreads from "zero".
         DO W = 1,2	!Two work files are required.
           WRITE(MSG,11) TIDE(W)	!Which one this time?
   11      FORMAT (/,"Tide ",A)		!Might as well supply a name.
           DO I = 1,N		!Produce a text sequence for the board layout.
             BRAND(I:I) = DIGIT(BOARD(I))	!One by one...
           END DO		!BRAND = DIGIT(BOARD)
           FNAME = BNAME//"."//BRAND//".dat"	!It contains binary stuff, so what else but data?
           INQUIRE (FILE = FNAME, EXIST = EXIST)	!Perhaps it is lying about.
   20      IF (EXIST) THEN				!Well?
             WRITE (MSG,*) "Restarting from file ",FNAME	!One hopes its content is good.
             OPEN (WRK(W),FILE = FNAME,STATUS = "OLD",ACCESS = "DIRECT",	!Random access is intended.
     1        FORM = "UNFORMATTED",BUFFERED = "YES",RECL = LREC)		!Using record numbers as the key.
             FNAME = BNAME//"."//BRAND//".ndx"		!Now go for the accomplice.
             INQUIRE (FILE = FNAME, EXIST = EXIST)	!That contains the index.
             IF (.NOT.EXIST) THEN			!Well?
               WRITE (MSG,*) " ... except, no file ",FNAME	!Oh dear.
               CLOSE(WRK(W))				!So, no index for the work file. Abandon it.
               GO TO 20					!And thus jump into the ELSE clause below.
             END IF				!Seeing as an explicit GO TO would be regarded as improper...
             READ (WRK(W),REC = 1) HCOUNT(W),SURGE(W),IST(W),LST(W)!Get the header information.
             WRITE (MSG,22) HCOUNT(W),SURGE(W),IST(W),LST(W)	!Reveal.
   22        FORMAT (" Stashed ",I0,". At surge ",I0,		!Perhaps it will be corrupt.
     1        " with the boundary stashed in elements ",I0," to ",I0)	!If so, this might help the reader.
             OPEN (NDX(W),FILE = FNAME,STATUS = "OLD",ACCESS="DIRECT",	!Now for the accomplice.
     1        FORM = "UNFORMATTED",BUFFERED = "YES",RECL = 1)		!One INTEGER per record.
             READ(NDX(W), REC = 1) NINDEX(W)			!This count is maintained, to avoid a mass scan.
             WRITE (MSG,23) NINDEX(W),APRIME			!Exhibit the count.
   23        FORMAT (" Its index uses ",I0," of ",I0," entries.")	!Simple enough.
            ELSE			!But, if there is no stash, create a new one.
             WRITE (MSG,*) "Preparing a stash in file ",FNAME	!Start from scratch.
             OPEN (WRK(W),FILE = FNAME,STATUS="REPLACE",ACCESS="DIRECT",	!I intend non-sequential access...
     1        FORM = "UNFORMATTED",BUFFERED = "YES",RECL = LREC)		!And, not text.
             HCOUNT(W) = 1		!Just one position is known, the final position.
             SURGE(W) = 0		!It has not been clambered away from.
             IST(W) = 1			!The first to inspect at the current level.
             LST(W) = 1			!The last.
             WRITE (WRK(W),REC = 1) HCOUNT(W),SURGE(W),IST(W),LST(W),0	!The header.
             FNAME = BNAME//"."//BRAND//".ndx"	!Now for the associated index file..
             WRITE (MSG,*) "... with an index in file ",FNAME	!Announce before attempting access.
             OPEN (NDX(W),FILE = FNAME,STATUS = "REPLACE",ACCESS=	!Lest there be a mishap.
     1        "DIRECT",FORM = "UNFORMATTED",BUFFERED = "YES",RECL = 1)	!Yep. Just one value per record.
             WRITE (MSG,*) APRIME," zero values for an empty index."	!This may cause a pause.
             NINDEX(W) = 1				!The index will start off holding one used entry.
             WRITE (NDX(W),REC = 1) NINDEX(W)	!Save this count in the header record.
             WRITE (NDX(W),REC = 1 + APRIME) 0	!Zero values will also appear in the gap!
             ASTASH.NEXT = 0		!The first index emtry can never collide with another in an empty index.
             ASTASH.PREV = 0		!And it is created sufficient unto itself.
             ASTASH.MOVE = 0		!Thus, it is not a descendant, but immaculate.
             ASTASH.BRD(1) = BORED(1)*16 + BORED(2)	!Only four bits of the eight supplied are used.
             ASTASH.BRD(2) = BORED(3)*16 + BORED(4)	!So interleave them, pairwise.
             SLOSH = ASTASH.BRD(1)*ASTASH.BRD(2)	!Mash the bits together.
             HIT = ABS(MOD(SLOSH,APRIME)) + 2		!Make a hash. Add one since MOD starts with zero.
             WRITE (NDX(W),REC = HIT) HCOUNT(W)		!Adding another one to dodge the header as well.
             WRITE (MSG,24) BOARD,BORED,ASTASH.BRD,	!Reveal the stages.
     1        SLOSH,SLOSH,SLOSH,APRIME,HIT				!Of the mostly in-place reinterpretations.
   24        FORMAT (<N>Z2," is the board layout in INTEGER*1",/,	!Across the columns and down the rows.
     1        4Z8," is the board layout in INTEGER*4",/,		!Reinterpret as four integers.
     2        2(8X,Z8)," ..interleaved into two INTEGER*4",/,		!Action: Interleaved into two.
     3        Z32," multiplied together in INTEGER*4",/,		!Action: Their product.
     4        I32," as a decimal integer.",/,				!Abandoning hexadecimal.
     5        "ABS(MOD(",I0,",",I0,")) + 2 = ",I0,			!The final step.
     6        " is the record number for the first index entry.")	!The result.
             WRITE (WRK(W),REC = HCOUNT(W) + 1) ASTASH	!Record one is reserved as a header...
           END IF				!Either way, a workfile should be ready now.
           IF (W.EQ.1) BOARD = TARGET	!Thus go for the other work file.
         END DO		!Only two iterations, but a lot of blather.
         SLOSH = MINVAL(SURGE,DIM = 1)	!Find the laggard.

Cast forth a heading for the progress table to follow..
         WRITE (MSG,99)
   99    FORMAT (/,7X,"|",3X,"Tidewrack Boundary Positions  |",
     1    6X,"Positions",5X,"|",7X,"Primary Probes",9X,"Index Use",
     2    4X,"|",5X,"Secondary Probes",3X,"|"," Memory of Time Passed",/,
     3    "Surge",2X,"|",6X,"First",7X,"Last",6X,"Count|",
     4    4X,"Checked Deja vu%|",7X,"Made Max.L  Avg.L|   Used%",
     5    5X,"Load%|",7X,"Made Max.L  Avg.L|",6X,"CPU",8X,"Clock")

Chase along the boundaries of the red and the blue tides, each taking turns as primary and secondary interests.
  100    SLOSH = SLOSH + 1	!Another advance begins.
      WW:DO W = 1,2	!The advance is made in two waves, each with its own statistics.
           M = 3 - W		!Finger the other one.
           NMET = 0		!No meetings have happened yet.
           IF (SURGE(W).GE.SLOSH) CYCLE WW	!Prefer to proceed with matched surges.
           WRITE (MSG,101) SLOSH,TIDE(W),IST(W),LST(W),LST(W)-IST(W)+1	!The boundary to be searched.
  101      FORMAT (I2,1X,A4,"|",3I11,"|",$)				!This line will be continued.
           NCHECK = 0		!No new positions have been prepared.
           NLOOK = 0		!So the stashes have not been searched for any of them.
           PROBES = 0		!And no probes have been made in any such searches.
           MAXP = 0		!So the maximum length of all probe chains is zero so far.
           HNEXT = LST(W) + 1	!This will be where the first new position will be stashed.
           T1 = NOWWAS(0)	!Note the accumulated CPU time at the start of the boundary ride..
           E1 = NOWWAS(2)	!Time of day, in seconds. GMT style (thus not shifted by daylight saving)
        PP:DO P = IST(W),LST(W)	!These are on the edge of the tide. Spreading proceeds.
             READ (WRK(W),REC = P + 1) ASTASH	!Obtain a position, remembering to dodge the header record.
             HENCE = ASTASH.MOVE		!The move (from ASTASH.PREV) that reached this position.
             IF (HENCE.NE.0) HENCE = MOD(HENCE + 1,4) + 1	!The reverse of that direction. Only once zero. Sigh.
             CALL UNCRAM(ASTASH.BRD,BOARD)	!Unpack into the work BOARD.
             LZ = INDEX(BOAR,CHAR(0))	!Find the BOARD square with zero.
             ZR =    (LZ - 1)/NC + 1	!Convert to row and column in LOCZ to facilitate bound checking.
             ZC = MOD(LZ - 1,NC) + 1	!Two divisions, sigh. Add a special /\ syntax? (ZR,ZC) = (LZ - 1)/\NC + 1
Consider all possible moves from position P, If a new position is unknown, add it to the stash.
          DD:DO D = 1,4			!Step through the possible directions in which the zero square might move.
               IF (D.EQ.HENCE) CYCLE DD		!Don't try going back whence this came.
               LOCI = LOCZ + WAYS(1:2,D)	!Finger the destination of the zero square, (row,column) style.
               IF (ANY(LOCI.LE.0)) CYCLE DD	!No wrapping left/right or top/bottom.
               IF (ANY(LOCI.GT.(/NR,NC/))) CYCLE DD	!No .OR. to avoid the possibility of non-shortcut full evaluation.
               NCHECK = NCHECK + 1		!So, here is another position to inspect.
               NP = 0				!No probes of stashes W or M for it have been made.
               IT = WAY(D) + LZ			!Finger the square that is to move to the adjacent zero.
               BOARD(LZ) = BOARD(IT)		!Move that square's content to the square holding the zero.
               BOARD(IT) = 0			!It having departed.
               ASTASH.BRD(1) = BORED(1)*16 + BORED(2)	!Pack the position list
               ASTASH.BRD(2) = BORED(3)*16 + BORED(4)	!Without fussing over adjacency,
               HIT = ABS(MOD(ASTASH.BRD(1)*ASTASH.BRD(2),APRIME)) + 2	!Crunch the hash index.
               READ (NDX(W),REC = HIT) HEAD	!Refer to the index, which fingers the first place to look.
               LOOK = HEAD			!This may be the start of a linked-list.
               IF (LOOK.EQ.0) NINDEX(W) = NINDEX(W) + 1	!Or, a new index entry will be made.
               IF (LOOK.NE.0) NLOOK(1) = NLOOK(1) + 1	!Otherwise, we're looking at a linked-list, hopefully short.
               DO WHILE (LOOK.NE.0)		!Is there a stash entry to look at?
                 NP(1) = NP(1) + 1			!Yes. Count a probe of the W stash.
                 READ (WRK(W),REC = LOOK + 1) APROBE	!Do it. (Dodging the header record)
                 IF (ALL(ASTASH.BRD.EQ.APROBE.BRD)) GO TO 109	!Already seen? Ignore all such as previously dealt with.
                 LOOK = APROBE.NEXT			!Perhaps there follows another entry having the same index.
               END DO				!And eventually, if there was no matching entry,
               HCOUNT(W) = HCOUNT(W) + 1	!A new entry is to be added to stash W, linked from its index.
               IF (HCOUNT(W).LE.0) STOP "HCOUNT overflows!"	!Presuming the usual two's complement style.
               WRITE (NDX(W),REC = HIT) HCOUNT(W)	!The index now fingers the new entry in ASTASH.
               ASTASH.NEXT = HEAD			!Its follower is whatever the index had fingered before.
               ASTASH.PREV = P			!This is the position that led to it.
               ASTASH.MOVE = D			!Via this move.
               WRITE (WRK(W),REC = HCOUNT(W) + 1) ASTASH	!Place the new entry, dodging the header.
Check the other stash for this new position. Perhaps there, a meeting will be found!
               READ (NDX(M),REC = HIT) LOOK	!The other stash uses the same hash function but has its own index.
               IF (LOOK.NE.0) NLOOK(2) = NLOOK(2) + 1	!Perhaps stash M has something to look at.
               DO WHILE(LOOK.NE.0)		!Somewhere along a linked-list.
                 NP(2) = NP(2) + 1			!A thorough look may involve multiple probes.
                 READ(WRK(M),REC = LOOK + 1) APROBE	!Make one.
                 IF (ALL(ASTASH.BRD.EQ.APROBE.BRD)) THEN!A match?
                   IF (NMET.LT.MANY) THEN			!Yes! Hopefully, not too many already.
                     NMET = NMET + 1					!Count another.
                     MET(W,NMET) = HCOUNT(W)				!Save a finger to the new entry.
                     MET(M,NMET) = LOOK					!And to its matching counterparty.
                    ELSE						!But if too numerous for my list,
                     WRITE (MSG,108) TIDE(W),HCOUNT(W),TIDE(M),LOOK	!Announce each.
  108                FORMAT ("Can't save ",A,1X,I0," matching ",A,1X,I0)!Also wrecking my tabular layout.
                   END IF						!So much for recording a match.
                   GO TO 109					!Look no further for the new position; it is found..
                 END IF					!So much for a possible match.
                 LOOK = APROBE.NEXT			!Chase along the linked-list.
               END DO				!Thus checking all those hashing to the same index.
Completed the probe.
  109          MAXP = MAX(MAXP,NP)		!Track the maximum number of probes in any search..
               PROBES = PROBES + NP		!As well as their count.
               BOARD(IT) = BOARD(LZ)		!Finally, undo the move.
               BOARD(LZ) = 0			!To be ready for the next direction.
             END DO DD			!Consider another direction.
           END DO PP		!Advance P to the next spreading possibility.
Completed one perimeter sequence. Cast forth some statistics.
  110      T2 = NOWWAS(0)	!A boundary patrol has been completed.
           E2 = NOWWAS(2)	!And time has passed.
           HIT = HCOUNT(W) - HNEXT + 1	!The number of new positions to work from in the next layer.
           WRITE (MSG,111) NCHECK,100.0*(NCHECK - HIT)/NCHECK,	!Tested, and %already seen
     1      NLOOK(1),MAXP(1),FLOAT(PROBES(1))/MAX(NLOOK(1),1),	!Search statistics.
     2      100.0*NINDEX(W)/APRIME,100.0*HCOUNT(W)/APRIME,	!Index occupancy: used entries, and load.
     3      NLOOK(2),MAXP(2),FLOAT(PROBES(2))/MAX(NLOOK(2),1)	!Other stash's search statistics.
  111      FORMAT (I11,F9.2,"|",I11,I6,F7.3,"|",F8.3,F10.3,"|",	!Attempt to produce regular columns.
     1      I11,I6,F7.3,"|"$)					!To be continued...
           T1 = T2 - T1			!Thus, how much CPU time was used perusing the perimeter.
           E1 = E2 - E1			!Over the elapsed time.
           CALL PROUST(T1)		!Muse over the elapsed CPU time, in seconds.
           CALL PROUST(E1)		!And likewise the elapsed clock time.
           E2 = NOWWAS(1)		!Civil clock, possibly adjusted for daylight saving.
           IF (E1.LE.0) THEN		!The offered timing may be too coarse.
             WRITE (MSG,112) HMS(E2)		!So, just finish the line.
  112        FORMAT (8X,A)			!With a time of day.
            ELSE			!But if some (positive) clock time was measured as elapsing,
             WRITE (MSG,113) T1/E1*100,HMS(E2)	!Offer a ratio as well.
  113        FORMAT (F6.2,"%",1X,A)		!Percentages are usual.
           END IF			!Enough annotation.
Could there be new positions to check? HCOUNT will have been increased if so.
           SURGED(W) = HCOUNT(W).GE.HNEXT	!The boundary has been extended to new positions.
           IF (SURGED(W)) THEN		!But, are there any new positions?
             IST(W) = HNEXT			!Yes. The first new position would have been placed here.
             LST(W) = HCOUNT(W)			!This is where the last position was placed.
             SURGE(W) = SLOSH			!The new surge is ready.
             WRITE (WRK(W),REC = 1) HCOUNT(W),SURGE(W),IST(W),LST(W)	!Update the headers correspondingly..
             WRITE (NDX(W), REC = 1) NINDEX(W)	!Otherwise, a rescan would be needed on a restart.
           ELSE IF (SURGE(W) + 1 .EQ. SLOSH) THEN	!No new positions. First encounter?
             LOOK = LST(W) - IST(W) + 1		!Yes. How many dead ends are there?
             WRITE (MSG,114) LOOK		!Announce.
  114        FORMAT (/,"The boundary has not surged to new positions!",/
     1       "The now-static boundary has ",I0)
             LOOK = LOOK/666 + 1		!If there are many, don't pour forth every one.
             IF (LOOK.GT.1) WRITE (MSG,115) LOOK!Some restraint.
  115        FORMAT (6X,"... Listing step: ",I0)!Rather than rolling forth a horde.
             WRITE (MSG,121)			!Re-use the heading for the REPORT.
             DO P = IST(W),LST(W),LOOK		!Step through the dead ends, possibly sampling every one.
               READ (WRK(W),REC = P + 1) ASTASH		!Grab a position.
               CALL REPORT(P,TIDE(W),WNAMEF(ASTASH.MOVE),ASTASH.BRD)	!Describe it.
             END DO				!On to the next dead end.
           END IF			!Perhaps the universe has been filled.
Could the clouds have touched? If so, two trails have met.
  120   ML:DO P = 1,NMET		!Step through the meeting list.
             IF (NS.LT.MANY) NS = NS + 1!Note another shove sequence.
             LS(NS) = 0			!Details to be determined.
             WRITE (MSG,121)		!Announce, via a heading.
  121         FORMAT (/,5X,"Record Stash Move |Board layout by row|",	!Various details
     1         2X,"Max|d|  Sum|d|   Euclidean   Encoded vs Zero")	!Will be attached.
             NTRAIL = 1			!Every trail starts with its first step.
             TRAIL(1) = MET(2,P)	!This is the blue trail's position that met the red tide..
  122        READ(WRK(2),REC = TRAIL(NTRAIL) + 1) ASTASH	!Obtain details
             IF (ASTASH.PREV.NE.0) THEN	!Had this step arrived from somewhere?
               IF (NTRAIL.GE.LONG) STOP "The trail is too long!"	!Yes.
               NTRAIL = NTRAIL + 1		!Count another step.
               TRAIL(NTRAIL) = ASTASH.PREV	!Finger the preceding step.
               GO TO 122			!And investigate it in turn.
             END IF			!Thus follow the blue trail back to its origin.
  130        DO LOOK = NTRAIL,1,-1	!The end of the blue trail is the position in TARGET, the start position.
               READ(WRK(2),REC = TRAIL(LOOK) + 1) ASTASH	!Grab a position, dodging the header.
               CALL REPORT(TRAIL(LOOK),"Blue",WNAMEF(ASTASH.MOVE),	!Backwards*backwards = forwards.
     1          ASTASH.BRD)						!The board layout is always straightforward...
               IF (LOOK.NE.NTRAIL) THEN		!The start position has no move leading to it.
                 IF (LS(NS).LT.LEN(SHOVE(1))) LS(NS) = LS(NS) + 1	!But count all subsequent ssociated moves.
                 SHOVE(NS)(LS(NS):LS(NS)) = WNAMEF(ASTASH.MOVE)	!Place it.
               END IF				!So much for that move.
             END DO			!On to the next move away from the start position.
  140        HEAD = 0			!Syncopation. Prevent the first position of the red trail from being listed.
             LOOK = MET(1,P)		!It is the same position as the first in the TRAIL, but in the primary stash.
             DO WHILE(LOOK.NE.0)	!The red chain runs back to its starting position, which is the "solution" state..
               READ(WRK(1),REC = LOOK + 1) ASTASH	!Which is in the direction I want to list.
               IF (HEAD.NE.0) THEN			!Except that the moves are one step behind for this list.
                 CALL REPORT(LOOK,"Red",WNAMEZ(HEAD),ASTASH.BRD)	!As this sequence is not being reversed.
                 IF (LS(NS).LT.LEN(SHOVE(1))) LS(NS) = LS(NS) + 1	!This lists the moves in forwards order.
                 SHOVE(NS)(LS(NS):LS(NS)) = WNAMEZ(HEAD)	!But the directions are reversed....
               END IF				!This test avoids listing the "Red" position that is the same as the last "Blue" position.
               HEAD = ASTASH.MOVE		!This is the move that led to this position.
               LOOK = ASTASH.PREV		!From the next position, which will be listed next.
             END DO			!Thus, the listed position was led to by the previous position's move.
  150        DO I = 1,NS - 1		!Perhaps the move sequence has been found already.
               IF (SHOVE(I)(1:LS(I)).EQ.SHOVE(NS)(1:LS(NS))) THEN	!So, compare agains previous shoves.
                 WRITE (MSG,151) I				!It has been seen.
  151            FORMAT (6X,"... same as for sequence ",I0)	!Humm.
                 NS = NS - 1					!Eject the arriviste.
                 GO TO 159					!And carry on.
               END IF				!This shouldn't happen...
             END DO			!On to the next comparison.
             WRITE (MSG,152) LS(NS),SHOVE(NS)(1:LS(NS))	!Show the moves along a line.
  152        FORMAT (I4," moves: ",A)	!Surely plural? One-steps wouldn't be tried?
  159      END DO ML		!Perhaps another pair of snakes have met.
         END DO WW	!Advance W to the other one. M will be swapped correspondingly.

Could there be an end to it all?
         IF (.NOT.ANY(SURGED)) STOP "No progress!"	!Oh dear.
         IF (NMET.LE.0) GO TO 100			!Keep right on to the end of the road...
       END SUBROUTINE PURPLE HAZE	!That was fun!
      END MODULE SLIDESOLVE

      PROGRAM POKE
      USE SLIDESOLVE
      CHARACTER*(19) FNAME		!A base name for some files.
      INTEGER I,R,C			!Some steppers.
      INTEGER MSG,KBD,WRK(2),NDX(2)	!I/O unit numbers.
      COMMON/IODEV/ MSG,KBD,WRK,NDX	!I talk to the trees..
      KBD = 5			!Standard input. (Keyboard)
      MSG = 6			!Standard output.(Display screen)
      WRK = (/10,12/)		!I need two work files,
      NDX = WRK + 1		!Each with its associated index.
      WRITE (FNAME,1) NR,NC	!Now prepare the file name.
    1 FORMAT ("SlideSolveR",I1,"C",I1,".txt")	!Allowing for variation, somewhat.
      WRITE (MSG,2) NR,NC,FNAME			!Announce.
    2 FORMAT ("To play 'slide-square' with ",I0," rows and ",
     1 I0," columns.",/,"An initial layout will be read from file ",
     2 A,/,"The objective is to attain the nice orderly layout"
     3 " as follows:",/)
      FORALL(I = 1:N - 1) ZERO(I) = I	!Regard the final or "solution" state as ZERO.
      ZERO(N) = 0			!The zero sqiuare is at the end, damnit!
      CALL SHOW(NR,NC,ZERO)		!Show the squares in their "solved" arrangement: the "Red" stash.
      OPEN(WRK(1),FILE=FNAME,STATUS="OLD",ACTION="READ")	!For formatted input.
      DO R = 1,NR			!Chug down the rows, reading successive columns across a row..
        READ (WRK(1),*) (TARGET((R - 1)*NC + C), C = 1,NC)	!Into successive storage locations.
      END DO				!Furrytran's storage order is (column,row) for that, alas.
      CLOSE (WRK(1))			!A small input, but much effort follows.
      WRITE (MSG,3)			!Now show the supplied layout.
    3 FORMAT (/,"The starting position:")	!The target, working backwards.
      CALL SHOW(NR,NC,TARGET)		!This will be the starting point for the "Blue" stash.
      IF (ALL(TARGET.EQ.BOARD)) STOP "Huh? They're the same!"	!Surely not.
      WRITE (MSG,4)
    4 FORMAT (/'The plan is to spread a red tide from the "solved" ',
     1 "layout and a blue tide from the specified starting position.",/
     2 "The hope is that these floods will meet at some position,",
     3 " and the blue moves plus the red moves in reverse order,",/
     4 "will be the shortest sequence from the given starting",
     5 " position to the solution.")

      CALL PURPLE HAZE(FNAME(1:14))

      END
```



### The Results

An important feature of the stash file is that its sequential growth makes it easy to keep track of which entries are on the current boundary. Its positions are stored in entry First (<code>IST</code>) to Last (<code>LST</code>) inclusive, and as the new boundary is identified and checked, its accepted positions are placed in the stash following the last entry to become the First:Last span for the next surge. When a candidate new position is checked, it may be that it is already in the stash so it is declared "Already seen" and ignored. This check can be swift, as if the value of INDEX(H) is zero then that hash code has not been seen before and so the position can't be in the stash. As more and more INDEX entries are used, the proportion of zero INDEX values falls, as tracked by the column headed Used%, and different positions may fall upon the same index entry because their hash numbers are the same. Thus the column headed Load% shows the total number of positions stashed divided by the number of index entries available, the value of <code>APRIME</code>. Some index entries will doubtless remain unused even as others are overloaded. It is important that the hash calculation sprays evenly, avoiding clumps.

If an index value is non-zero, then the candidate position's layout must be compared to the layout of each of the positions that have been linked together as having that hash number. Each comparison is a probe of the stash (the disc record must be read) and hopefully, the average number of probes remains a small number, such as one. Perhaps a match will be found early in the chain, but hopefully, most chains are short. Thus the columns headed Max.L and Avg.L report on this. Only after the last linked entry is checked will it be known that the candidate position's layout is not in the stash, and if so, a new entry to hold it is made.

If a position is declared new and added to the primary stash (be it Red or Blue) as a new border element, a secondary search is made of the *other* stash (respectively, Blue or Red) to seek a match, and the same statistics are presented. No entries are added to the alternate stash: instead a match means that the two tides have met at this position, and so a step sequence is discovered!

The step sequence is shown along with a board layout, followed by various methods of calculating a position's distance from ZERO, the solved state. All are such that Dist(ZERO) = 0, but, they do not show an obvious direction to follow. Some steps along the path to ZERO raise the distance to ZERO.

### =Specified Problem=


```txt

To play 'slide-square' with 4 rows and 4 columns.
An initial layout will be read from file SlideSolveR4C4.txt
The objective is to attain the nice orderly layout as follows:

Row|__1__2__3__4
  1|  1  2  3  4
  2|  5  6  7  8
  3|  9 10 11 12
  4| 13 14 15  0

The starting position:
Row|__1__2__3__4
  1| 15 14  1  6
  2|  9 11  4 12
  3|  0 10  7  3
  4| 13  8  5  2

The plan is to spread a red tide from the "solved" layout and a blue tide from the specified starting position.
The hope is that these floods will meet at some position, and the blue moves plus the red moves in reverse order,
will be the shortest sequence from the given starting position to the solution.

Tide  Red
 Preparing a stash in file SlideSolveR4C4.123456789ABCDEF0.dat
 ... with an index in file SlideSolveR4C4.123456789ABCDEF0.ndx
   199999991  zero values for an empty index.
 1 2 3 4 5 6 7 8 9 A B C D E F 0 is the board layout in INTEGER*1
 4030201 8070605 C0B0A09   F0E0D is the board layout in INTEGER*4
        48372615        C0BFAE9D ..interleaved into two INTEGER*4
                        EF5FA0E1 multiplied together in INTEGER*4
                      -278945567 as a decimal integer.
ABS(MOD(-278945567,199999991)) + 2 = 78945578 is the record number for the first index entry.

Tide Blue
 Preparing a stash in file SlideSolveR4C4.FE169B4C0A73D852.dat
 ... with an index in file SlideSolveR4C4.FE169B4C0A73D852.ndx
   199999991  zero values for an empty index.
 F E 1 6 9 B 4 C 0 A 7 3 D 8 5 2 is the board layout in INTEGER*1
 6010E0F C040B09 3070A00 205080D is the board layout in INTEGER*4
        6C14EBF9        3275A80D ..interleaved into two INTEGER*4
                        B2B863A5 multiplied together in INTEGER*4
                     -1296538715 as a decimal integer.
ABS(MOD(-1296538715,199999991)) + 2 = 96538771 is the record number for the first index entry.

       |   Tidewrack Boundary Positions  |      Positions     |       Primary Probes         Index Use    |     Secondary Probes   | Memory of Time Passed
Surge  |      First       Last      Count|    Checked Deja vu%|       Made Max.L  Avg.L|   Used%     Load%|       Made Max.L  Avg.L|      CPU        Clock
 1  Red|          1          1          1|          2     0.00|          0     0  0.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.5secs  2.94%  1:07:03.093am.
 1 Blue|          1          1          1|          3     0.00|          0     0  0.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.5secs  3.22%  1:07:03.578am.
 2  Red|          2          3          2|          4     0.00|          0     0  0.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.0secs  0.00%  1:07:03.593am.
 2 Blue|          2          4          3|          6     0.00|          0     0  0.000|   0.000     0.000|          0     0  0.000|    0.1secs    0.5secs 11.75%  1:07:04.125am.
 3  Red|          4          7          4|         10     0.00|          0     0  0.000|   0.000     0.000|          0     0  0.000|    0.1secs    0.7secs 11.63%  1:07:04.812am.
 3 Blue|          5         10          6|         14     0.00|          0     0  0.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.0secs         1:07:04.812am.
 4  Red|          8         17         10|         24     0.00|          0     0  0.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.0secs  0.00%  1:07:04.875am.
 4 Blue|         11         24         14|         32     0.00|          0     0  0.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.1secs  0.00%  1:07:04.953am.
 5  Red|         18         41         24|         54     0.00|          0     0  0.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.0secs  0.00%  1:07:05.015am.
 5 Blue|         25         56         32|         66     0.00|          0     0  0.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.0secs 50.40%  1:07:05.046am.
 6  Red|         42         95         54|        108     0.93|          1     1  1.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.0secs 33.24%  1:07:05.093am.
 6 Blue|         57        122         66|        136     1.47|          2     1  1.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.1secs  0.00%  1:07:05.171am.
 7  Red|         96        202        107|        215     1.40|          3     1  1.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.1secs  0.00%  1:07:05.265am.
 7 Blue|        123        256        134|        285     1.75|          5     1  1.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.1secs  0.00%  1:07:05.375am.
 8  Red|        203        414        212|        456     2.19|         10     1  1.000|   0.000     0.000|          0     0  0.000|    0.0secs    0.2secs  8.36%  1:07:05.578am.
 8 Blue|        257        536        280|        601     2.66|         16     1  1.000|   0.001     0.001|          0     0  0.000|    0.0secs    0.2secs 20.03%  1:07:05.812am.
 9  Red|        415        860        446|        974     2.87|         28     1  1.000|   0.001     0.001|          0     0  0.000|    0.2secs    0.5secs 37.50%  1:07:06.312am.
 9 Blue|        537       1121        585|       1254     3.19|         41     1  1.000|   0.001     0.001|          0     0  0.000|    0.2secs    0.5secs 34.38%  1:07:06.812am.
10  Red|        861       1806        946|       2032     4.13|         88     1  1.000|   0.002     0.002|          0     0  0.000|    0.3secs    0.9secs 37.28%  1:07:07.734am.
10 Blue|       1122       2335       1214|       2578     4.50|        117     1  1.000|   0.002     0.002|          0     0  0.000|    0.3secs    1.1secs 25.00%  1:07:08.859am.
11  Red|       1807       3754       1948|       4124     4.51|        189     1  1.000|   0.004     0.004|          0     0  0.000|    0.5secs    1.8secs 29.66%  1:07:10.703am.
11 Blue|       2336       4797       2462|       5188     4.66|        249     1  1.000|   0.005     0.005|          0     0  0.000|    0.6secs    2.2secs 27.54%  1:07:12.859am.
12  Red|       3755       7692       3938|       8244     5.29|        440     1  1.000|   0.008     0.008|          0     0  0.000|    1.0secs    3.6secs 28.51%  1:07:16.421am.
12 Blue|       4798       9743       4946|      10442     5.56|        588     1  1.000|   0.010     0.010|          1     1  1.000|    1.6secs    4.2secs 38.20%  1:07:20.593am.
13  Red|       7693      15500       7808|      16470     5.62|        943     1  1.000|   0.016     0.016|          0     0  0.000|    2.2secs    6.5secs 33.65%  1:07:27.093am.
13 Blue|       9744      19604       9861|      20858     6.03|       1285     2  1.001|   0.020     0.020|          4     1  1.000|    2.8secs    7.5secs 37.00%  1:07:34.625am.
14  Red|      15501      31044      15544|      32950     6.46|       2175     2  1.000|   0.031     0.031|          5     1  1.000|    3.8secs   11.2secs 34.12%  1:07:45.843am.
14 Blue|      19605      39204      19600|      41448     6.66|       2813     2  1.001|   0.039     0.039|         14     1  1.000|    4.6secs   12.7secs 36.19%  1:07:58.578am.
15  Red|      31045      61865      30821|      65311     6.84|       4611     2  1.001|   0.061     0.061|         23     1  1.000|    6.8secs   17.2secs 39.62%  1:08:15.734am.
15 Blue|      39205      77892      38688|      81703     6.87|       5803     2  1.000|   0.077     0.077|         47     1  1.000|    7.2secs   18.2secs 39.64%  1:08:33.906am.
16  Red|      61866     122707      60842|     128412     7.33|       9742     2  1.002|   0.121     0.121|         77     2  1.013|   11.9secs   23.2secs 51.21%  1:08:57.093am.
16 Blue|      77893     153978      76086|     160446     7.49|      12421     2  1.001|   0.151     0.151|        172     1  1.000|   13.6secs   23.2secs 58.57%  1:09:20.250am.
17  Red|     122708     241707     119000|     250818     7.56|      20063     3  1.002|   0.236     0.237|        303     1  1.000|   20.5secs   30.5secs 67.33%  1:09:50.765am.
17 Blue|     153979     302413     148435|     312766     7.89|      26220     2  1.002|   0.294     0.295|        678     2  1.001|   24.3secs   32.3secs 75.17%  1:10:23.109am.
18  Red|     241708     473551     231844|     487982     8.33|      43582     3  1.003|   0.458     0.460|       1229     2  1.002|   38.0secs   46.2secs 82.36%  1:11:09.265am.
18 Blue|     302414     590511     288098|     606919     8.56|      56018     3  1.003|   0.570     0.573|       2440     2  1.004|   47.5secs   59.4secs 80.02%  1:12:08.687am.
19  Red|     473552     920893     447342|     942552     8.79|      93152     3  1.005|   0.883     0.890|       4672     2  1.005|   73.1secs   91.3secs 80.07%  1:13:40.046am.
19 Blue|     590512    1145481     554970|    1168265     9.04|     120435     3  1.006|   1.093     1.104|       8999     3  1.009|   90.6secs    1.9mins 81.12%  1:15:31.765am.
20  Red|     920894    1780637     859744|    1809752     9.52|     202081     3  1.007|   1.687     1.709|      17194     2  1.008|    2.4mins    3.0mins 79.31%  1:18:30.265am.
20 Blue|    1145482    2208109    1062628|    2235235     9.77|     262047     3  1.008|   2.080     2.112|      33025     3  1.014|    3.0mins    3.7mins 79.82%  1:22:14.375am.
21  Red|    1780638    3418020    1637383|    3444691    10.06|     451483     4  1.014|   3.183     3.258|      62538     3  1.015|    4.5mins    5.7mins 79.80%  1:27:56.468am.
21 Blue|    2208110    4224923    2016814|    4238343    10.33|     588823     4  1.017|   3.905     4.013|     118609     5  1.024|    5.7mins    7.1mins 80.08%  1:35:03.140am.
22  Red|    3418021    6516290    3098270|    6506982    10.83|    1022051     4  1.023|   5.926     6.159|     219923     4  1.025|    8.7mins   10.7mins 81.12%  1:45:44.328am.
22 Blue|    4224924    8025605    3800682|    7979315    11.11|    1353159     5  1.029|   7.218     7.559|     414055     4  1.039|   10.9mins   13.8mins 79.04%  1:59:31.875am.
23  Red|    6516291   12318701    5802411|   12178635    11.45|    2470087     6  1.048|  10.780    11.551|     765853     5  1.046|   17.3mins   24.1mins 71.78%  2:23:35.828am.
23 Blue|    8025606   15118814    7093209|   14877107    11.76|    3307729     5  1.058|  13.003    14.123|    1401940     6  1.071|   22.4mins   35.3mins 63.46%  2:58:53.375am.
24  Red|   12318702   23102481   10783780|   22603192    12.29|    6014769     6  1.083|  19.074    21.464|    2526552     6  1.084|   36.7mins   65.6mins 55.95%  4:04:29.375am.
24 Blue|   15118815   28246178   13127364|   27507742    12.56|    8148893     7  1.105|  22.682    26.150|    4539588     7  1.124|   49.4mins   96.5mins 51.22%  5:40:59.265am.
25  Red|   23102482   42928799   19826318|   41532426    12.98|   15508438     9  1.166|  32.086    39.535|    8144188     7  1.151|   84.5mins    3.0hrs! 47.06%  8:40:25.296am.
25 Blue|   28246179   52299632   24053454|   50352435    13.30|   21007872     9  1.207|  37.354    47.979|   13961187     8  1.232|    2.0hrs!    4.4hrs! 44.40%  1:07:12.546pm.
26  Red|   42928800   79070945   36142146|   75611538    13.85|   38688179    10  1.300|  50.548    72.103|   24060624     9  1.278|    3.5hrs!    8.3hrs! 42.77%  9:23:03.250pm.
26 Blue|   52299633   95957208   43657576|   91305518    14.15|   51817830    14  1.379|  57.098    87.170|   39429635    10  1.424|    5.3hrs!   12.5hrs! 42.37%  9:53:57.890am.

     Record Stash Move |Board layout by row|  Max|d|  Sum|d|   Euclidean   Encoded vs Zero
          1  Blue      |FE16/9B4C/0A73/D852|      14      86      27.055    19442940853367
          2  Blue    L |FE16/9B4C/A073/D852|      14      88      27.423    19442940844007
          5  Blue    L |FE16/9B4C/A703/D852|      14      88      27.677    19442940842087
         11  Blue    L |FE16/9B4C/A730/D852|      14      88      27.785    19442940841679
         25  Blue    D |FE16/9B40/A73C/D852|      14      80      26.000    19442940922295
         58  Blue    R |FE16/9B04/A73C/D852|      14      80      25.846    19442943220535
        126  Blue    U |FE16/9B34/A70C/D852|      14      80      26.306    19442940271895
        265  Blue    R |FE16/9B34/A07C/D852|      14      80      26.038    19442940274415
        554  Blue    D |FE16/9034/AB7C/D852|      14      72      24.290    19442951159375
       1156  Blue    D |F016/9E34/AB7C/D852|      14      64      21.863    19530129450575
       2409  Blue    R |0F16/9E34/AB7C/D852|      13      62      21.166    20837803818575
       4949  Blue    U |9F16/0E34/AB7C/D852|      13      70      22.804    11597104535375
      10052  Blue    L |9F16/E034/AB7C/D852|      13      72      23.409    11597064618575
      20229  Blue    D |9016/EF34/AB7C/D852|      10      64      20.688    11684242909775
      40459  Blue    L |9106/EF34/AB7C/D852|      10      64      20.736    10544698103375
      80368  Blue    U |9136/EF04/AB7C/D852|      10      64      21.307    10469454209615
     158888  Blue    U |9136/EF74/AB0C/D852|      11      64      22.583    10469452026935
     312029  Blue    U |9136/EF74/AB5C/D802|      15      64      23.452    10469452026423
     609335  Blue    R |9136/EF74/AB5C/D082|      14      64      23.108    10469452026425
    1182001  Blue    D |9136/EF74/A05C/DB82|      10      62      21.119    10469452028615
    2278389  Blue    D |9136/E074/AF5C/DB82|       9      54      18.055    10469455657415
    4359103  Blue    R |9136/0E74/AF5C/DB82|       8      52      17.263    10469531862215
    8279825  Blue    D |0136/9E74/AF5C/DB82|       8      44      15.033    19623012937415
   15596324  Blue    L |1036/9E74/AF5C/DB82|       8      44      15.100     1228393494215
   29135380  Blue    L |1306/9E74/AF5C/DB82|       8      46      15.297      169799958215
   53940860  Blue    L |1360/9E74/AF5C/DB82|       8      48      15.684      111840764615
   98956855  Blue    U |1364/9E70/AF5C/DB82|       8      48      16.673      106528120775
   47181586   Red    R |1364/9E07/AF5C/DB82|       8      48      16.248      106530419015
   25400927   Red    U |1364/9E57/AF0C/DB82|      11      48      17.436      106527470375
   13548279   Red    U |1364/9E57/AF8C/DB02|      15      48      19.183      106527469863
    7168799   Red    L |1364/9E57/AF8C/DB20|      13      44      17.550      106527469862
    3761169   Red    D |1364/9E57/AF80/DB2C|      13      68      24.413      106527469916
    1959965   Red    R |1364/9E57/AF08/DB2C|      13      68      24.083      106527470324
    1013648   Red    U |1364/9E57/AF28/DB0C|      15      68      24.413      106527469693
     521247   Red    R |1364/9E57/AF28/D0BC|      14      68      23.958      106527469696
     266034   Red    D |1364/9E57/A028/DFBC|      12      60      21.307      106527470416
     135063   Red    D |1364/9057/AE28/DFBC|      12      52      18.493      106534727296
      68120   Red    L |1364/9507/AE28/DFBC|      12      52      18.762      106504971136
      34188   Red    U |1364/9527/AE08/DFBC|      12      52      19.183      106501659736
      17049   Red    U |1364/9527/AEB8/DF0C|      15      52      21.354      106501659249
       8443   Red    R |1364/9527/AEB8/D0FC|      14      50      20.640      106501659251
       4119   Red    D |1364/9527/A0B8/DEFC|      12      42      17.720      106501660689
       1986   Red    R |1364/9527/0AB8/DEFC|      12      40      17.146      106501687329
        950   Red    D |1364/0527/9AB8/DEFC|      12      32      14.900      106781074689
        458   Red    L |1364/5027/9AB8/DEFC|      12      32      15.232      106414565889
        222   Red    L |1364/5207/9AB8/DEFC|      12      32      15.362      106381543809
        104   Red    D |1304/5267/9AB8/DEFC|      12      26      13.711      168648485889
         46   Red    R |1034/5267/9AB8/DEFC|      12      24      13.491     1227242021889
         20   Red    U |1234/5067/9AB8/DEFC|      12      24      14.071          36293889
          9   Red    L |1234/5607/9AB8/DEFC|      12      24      14.491           3271809
          4   Red    L |1234/5670/9AB8/DEFC|      12      24      14.967            328449
          2   Red    U |1234/5678/9AB0/DEFC|      12      24      16.971               105
          1   Red    U |1234/5678/9ABC/DEF0|       0       0       0.000                 0
  52 moves: LLLDRURDDRULDLUUURDDRDLLLURUULDRURDDLUURDRDLLDRULLUU

     Record Stash Move |Board layout by row|  Max|d|  Sum|d|   Euclidean   Encoded vs Zero
          1  Blue      |FE16/9B4C/0A73/D852|      14      86      27.055    19442940853367
          2  Blue    L |FE16/9B4C/A073/D852|      14      88      27.423    19442940844007
          5  Blue    L |FE16/9B4C/A703/D852|      14      88      27.677    19442940842087
         11  Blue    L |FE16/9B4C/A730/D852|      14      88      27.785    19442940841679
         25  Blue    D |FE16/9B40/A73C/D852|      14      80      26.000    19442940922295
         58  Blue    R |FE16/9B04/A73C/D852|      14      80      25.846    19442943220535
        126  Blue    U |FE16/9B34/A70C/D852|      14      80      26.306    19442940271895
        266  Blue    U |FE16/9B34/A75C/D802|      15      80      27.055    19442940271383
        558  Blue    R |FE16/9B34/A75C/D082|      14      80      26.758    19442940271385
       1163  Blue    D |FE16/9B34/A05C/D782|      14      80      25.690    19442940274293
       2420  Blue    D |FE16/9034/AB5C/D782|      14      72      23.917    19442951159253
       4967  Blue    D |F016/9E34/AB5C/D782|      14      64      21.448    19530129450453
      10090  Blue    R |0F16/9E34/AB5C/D782|      13      62      20.736    20837803818453
      20306  Blue    U |9F16/0E34/AB5C/D782|      13      70      22.405    11597104535253
      40605  Blue    L |9F16/E034/AB5C/D782|      13      72      23.022    11597064618453
      80644  Blue    D |9016/EF34/AB5C/D782|       9      64      20.248    11684242909653
     159412  Blue    L |9106/EF34/AB5C/D782|       9      64      20.298    10544698103253
     313039  Blue    U |9136/EF04/AB5C/D782|       9      64      20.881    10469454209493
     611272  Blue    U |9136/EF54/AB0C/D782|      11      64      21.817    10469451664053
    1185720  Blue    U |9136/EF54/AB8C/D702|      15      64      23.238    10469451663663
    2285440  Blue    L |9136/EF54/AB8C/D720|      13      60      21.909    10469451663662
    4372401  Blue    D |9136/EF54/AB80/D72C|      13      84      27.713    10469451663716
    8304687  Blue    R |9136/EF54/AB08/D72C|      13      84      27.423    10469451664028
   15642421  Blue    R |9136/EF54/A0B8/D72C|      13      82      27.019    10469451665948
   29220003  Blue    D |9136/E054/AFB8/D72C|      13      74      24.698    10469455294748
   54094941  Blue    R |9136/0E54/AFB8/D72C|      13      72      24.125    10469531499548
   99233939  Blue    D |0136/9E54/AFB8/D72C|      13      64      22.583    19623012574748
   53847416   Red    L |1036/9E54/AFB8/D72C|      13      64      22.627     1228393131548
   29052034   Red    L |1306/9E54/AFB8/D72C|      13      66      22.760      169799595548
   15525093   Red    L |1360/9E54/AFB8/D72C|      13      68      23.022      111840401948
    8231956   Red    U |1364/9E50/AFB8/D72C|      13      68      23.707      106527758108
    4326107   Red    U |1364/9E58/AFB0/D72C|      13      68      25.020      106527510356
    2258614   Red    R |1364/9E58/AF0B/D72C|      13      68      24.576      106527510668
    1169712   Red    U |1364/9E58/AF2B/D70C|      15      68      24.900      106527510037
     602597   Red    R |1364/9E58/AF2B/D07C|      14      68      24.617      106527510040
     308014   Red    D |1364/9E58/A02B/DF7C|      12      60      22.045      106527510760
     156652   Red    D |1364/9058/AE2B/DF7C|      12      52      19.339      106534767640
      79080   Red    L |1364/9508/AE2B/DF7C|      12      52      19.596      106505011480
      39747   Red    U |1364/9528/AE0B/DF7C|      12      52      20.000      106501700080
      19843   Red    U |1364/9528/AE7B/DF0C|      15      52      21.354      106501699449
       9866   Red    R |1364/9528/AE7B/D0FC|      14      50      20.640      106501699451
       4832   Red    D |1364/9528/A07B/DEFC|      12      42      17.720      106501700889
       2335   Red    R |1364/9528/0A7B/DEFC|      12      40      17.146      106501727529
       1114   Red    D |1364/0528/9A7B/DEFC|      12      32      14.900      106781114889
        534   Red    L |1364/5028/9A7B/DEFC|      12      32      15.232      106414606089
        259   Red    L |1364/5208/9A7B/DEFC|      12      32      15.362      106381584009
        124   Red    D |1304/5268/9A7B/DEFC|      12      26      13.711      168648526089
         56   Red    R |1034/5268/9A7B/DEFC|      12      24      13.491     1227242062089
         24   Red    U |1234/5068/9A7B/DEFC|      12      24      14.071          36334089
         10   Red    L |1234/5608/9A7B/DEFC|      12      24      14.491           3312009
          5   Red    U |1234/5678/9A0B/DEFC|      12      24      16.310               609
          2   Red    L |1234/5678/9AB0/DEFC|      12      24      16.971               105
          1   Red    U |1234/5678/9ABC/DEF0|       0       0       0.000                 0
  52 moves: LLLDRUURDDDRULDLUUULDRRDRDLLLUURURDDLUURDRDLLDRULULU

```

The two paths deviate on the seventh move, from entry 126 Blue either R to 265 Blue or U to 266 Blue. The Euclidean and Encoded distances to ZERO conflict: down and up for the first choice, up and down for the second.


### =Another Example=

Taking the first example from [15_puzzle_solver/20_Random](/tasks/15_puzzle_solver/20_Random) shows that with the red tide expansion in hand, only the new blue tide is poured into the maze:

```txt

To play 'slide-square' with 4 rows and 4 columns.
An initial layout will be read from file SlideSolveR4C4.txt
The objective is to attain the nice orderly layout as follows:

Row|__1__2__3__4
  1|  1  2  3  4
  2|  5  6  7  8
  3|  9 10 11 12
  4| 13 14 15  0

The starting position:
Row|__1__2__3__4
  1| 13 10 11  6
  2|  5  3  1  4
  3|  8  0 12  2
  4| 14  7  9 15

The plan is to spread a red tide from the "solved" layout and a blue tide from the specified starting position.
The hope is that these floods will meet at some position, and the blue moves plus the red moves in reverse order,
will be the shortest sequence from the given starting position to the solution.

Tide  Red
 Restarting from file SlideSolveR4C4.123456789ABCDEF0.dat
 Stashed 144206568. At surge 26 with the boundary stashed in elements 79070946 to 144206568
 Its index uses 101095844 of 199999991 entries.

Tide Blue
 Preparing a stash in file SlideSolveR4C4.DAB6531480C2E79F.dat
 ... with an index in file SlideSolveR4C4.DAB6531480C2E79F.ndx
   199999991  zero values for an empty index.
 D A B 6 5 3 1 4 8 0 C 2 E 7 9 F is the board layout in INTEGER*1
 60B0A0D 4010305 20C0008 F09070E is the board layout in INTEGER*4
        64B1A3D5        2FC9078E ..interleaved into two INTEGER*4
                        7340B326 multiplied together in INTEGER*4
                      1933620006 as a decimal integer.
ABS(MOD(1933620006,199999991)) + 2 = 133620089 is the record number for the first index entry.

       |   Tidewrack Boundary Positions  |      Positions     |       Primary Probes         Index Use    |     Secondary Probes   | Memory of Time Passed
Surge  |      First       Last      Count|    Checked Deja vu%|       Made Max.L  Avg.L|   Used%     Load%|       Made Max.L  Avg.L|      CPU        Clock
 1 Blue|          1          1          1|          4     0.00|          0     0  0.000|   0.000     0.000|          2     1  1.000|    0.0secs    0.0secs  0.00%  9:27:19.468am.
 2 Blue|          2          5          4|         10     0.00|          0     0  0.000|   0.000     0.000|          4     2  1.500|    0.0secs    0.5secs  3.33%  9:27:19.937am.
 3 Blue|          6         15         10|         20     0.00|          0     0  0.000|   0.000     0.000|          5     2  1.400|    0.0secs    0.4secs  0.00%  9:27:20.296am.
 4 Blue|         16         35         20|         38     0.00|          0     0  0.000|   0.000     0.000|         16     3  1.250|    0.0secs    0.3secs 11.75%  9:27:20.562am.
 5 Blue|         36         73         38|         80     0.00|          0     0  0.000|   0.000     0.000|         39     4  1.385|    0.0secs    0.1secs  0.00%  9:27:20.625am.
 6 Blue|         74        153         80|        178     2.25|          4     1  1.000|   0.000     0.000|         87     4  1.379|    0.0secs    0.1secs  0.00%  9:27:20.750am.
 7 Blue|        154        327        174|        380     2.11|          8     1  1.000|   0.000     0.000|        186     5  1.500|    0.2secs    0.3secs 52.79%  9:27:21.046am.
 8 Blue|        328        699        372|        790     3.54|         28     1  1.000|   0.001     0.001|        386     5  1.448|    0.2secs    0.4secs 53.51%  9:27:21.484am.
 9 Blue|        700       1461        762|       1590     3.14|         50     1  1.000|   0.002     0.002|        738     6  1.407|    0.3secs    0.9secs 37.50%  9:27:22.375am.
10 Blue|       1462       3001       1540|       3234     5.01|        168     1  1.000|   0.003     0.003|       1542     6  1.396|    0.6secs    2.0secs 29.60%  9:27:24.328am.
11 Blue|       3002       6073       3072|       6512     4.85|        318     1  1.000|   0.006     0.006|       3032     5  1.398|    1.2secs    3.9secs 30.77%  9:27:28.187am.
12 Blue|       6074      12269       6196|      13182     6.27|        841     2  1.001|   0.012     0.012|       6007     6  1.406|    2.5secs    7.5secs 33.96%  9:27:35.687am.
13 Blue|      12270      24625      12356|      26134     6.19|       1648     2  1.001|   0.025     0.025|      12261     6  1.410|    5.0secs   15.6secs 32.23%  9:27:51.296am.
14 Blue|      24626      49141      24516|      51646     6.71|       3581     2  1.000|   0.049     0.049|      23554     6  1.395|    9.2secs   24.8secs 37.14%  9:28:16.078am.
15 Blue|      49142      97320      48179|     101393     6.94|       7301     2  1.000|   0.096     0.096|      46888     7  1.411|   15.8secs   41.9secs 37.71%  9:28:58.031am.
16 Blue|      97321     191676      94356|     198950     7.80|      16489     2  1.002|   0.187     0.188|      89521     7  1.399|   27.3secs   62.8secs 43.45%  9:30:00.781am.
17 Blue|     191677     375108     183432|     386686     8.11|      33604     2  1.003|   0.363     0.365|     177033     8  1.418|   52.7secs    1.9mins 45.48%  9:31:56.734am.
18 Blue|     375109     730438     355330|     748406     8.84|      74407     3  1.005|   0.700     0.706|     333730     7  1.405|   96.8secs    3.7mins 43.38%  9:35:39.812am.
19 Blue|     730439    1412688     682250|    1434890     9.27|     154921     4  1.007|   1.340     1.357|     649155     8  1.416|    3.2mins    7.1mins 44.60%  9:42:44.031am.

     Record Stash Move |Board layout by row|  Max|d|  Sum|d|   Euclidean   Encoded vs Zero
          1  Blue      |DAB6/5314/80C2/E79F|      15      94      29.155    16535302211892
          4  Blue    R |DAB6/5314/08C2/E79F|      15      94      28.879    16535302234212
         12  Blue    D |DAB6/0314/58C2/E79F|      15      94      28.178    16535581621572
         30  Blue    L |DAB6/3014/58C2/E79F|      15      94      28.284    16535251400772
         62  Blue    L |DAB6/3104/58C2/E79F|      15      94      28.320    16535218378692
        125  Blue    D |DA06/31B4/58C2/E79F|      15      86      26.721    16560125373252
        266  Blue    R |D0A6/31B4/58C2/E79F|      15      84      26.344    16971108746052
        568  Blue    R |0DA6/31B4/58C2/E79F|      15      82      25.846    20719775267652
       1203  Blue    U |3DA6/01B4/58C2/E79F|      15      86      26.306     3626483421252
       2480  Blue    L |3DA6/10B4/58C2/E79F|      15      86      26.344     3626080624452
       5044  Blue    D |30A6/1DB4/58C2/E79F|      15      78      24.290     3887608240452
      10136  Blue    L |3A06/1DB4/58C2/E79F|      15      80      24.698     3395673597252
      20466  Blue    L |3A60/1DB4/58C2/E79F|      15      82      24.940     3343462422852
      40817  Blue    U |3A64/1DB0/58C2/E79F|      15      82      25.573     3338668697412
      81229  Blue    U |3A64/1DB2/58C0/E79F|      15      82      25.884     3338668369068
     160022  Blue    R |3A64/1DB2/580C/E79F|      15      80      25.417     3338668369380
     314204  Blue    R |3A64/1DB2/508C/E79F|      15      80      25.100     3338668372500
     612120  Blue    D |3A64/10B2/5D8C/E79F|      15      72      22.935     3338679257460
    1187839  Blue    D |3064/1AB2/5D8C/E79F|      15      64      21.119     3861730860660
    2285373  Blue    R |0364/1AB2/5D8C/E79F|      15      62      20.976    19815358150260
   53888966   Red    U |1364/0AB2/5D8C/E79F|      15      62      21.166      106797401460
   29074972   Red    U |1364/5AB2/0D8C/E79F|      15      62      22.091      106394277060
   15537643   Red    L |1364/5AB2/D08C/E79F|      15      64      22.672      106394263380
    8238754   Red    U |1364/5AB2/D78C/E09F|      15      64      23.875      106394258914
    4329748   Red    L |1364/5AB2/D78C/E90F|      15      64      24.249      106394258911
    2260520   Red    L |1364/5AB2/D78C/E9F0|       6      34      11.747      106394258910
    1170710   Red    D |1364/5AB2/D780/E9FC|      12      58      20.640      106394258989
     603117   Red    R |1364/5AB2/D708/E9FC|      12      58      20.248      106394259493
     308288   Red    D |1364/5A02/D7B8/E9FC|      12      50      17.944      106396078573
     156795   Red    L |1364/5A20/D7B8/E9FC|      12      50      18.055      106393135213
      79148   Red    U |1364/5A28/D7B0/E9FC|      12      50      19.748      106392847909
      39781   Red    R |1364/5A28/D70B/E9FC|      12      50      19.183      106392848317
      19862   Red    R |1364/5A28/D07B/E9FC|      12      50      18.815      106392852037
       9877   Red    U |1364/5A28/D97B/E0FC|      14      50      20.640      106392848411
       4838   Red    R |1364/5A28/D97B/0EFC|      13      48      19.950      106392848421
       2337   Red    D |1364/5A28/097B/DEFC|      12      40      17.146      106392863529
       1115   Red    L |1364/5A28/907B/DEFC|      12      40      17.664      106392836889
        534   Red    D |1364/5028/9A7B/DEFC|      12      32      15.232      106414606089
        259   Red    L |1364/5208/9A7B/DEFC|      12      32      15.362      106381584009
        124   Red    D |1304/5268/9A7B/DEFC|      12      26      13.711      168648526089
         56   Red    R |1034/5268/9A7B/DEFC|      12      24      13.491     1227242062089
         24   Red    U |1234/5068/9A7B/DEFC|      12      24      14.071          36334089
         10   Red    L |1234/5608/9A7B/DEFC|      12      24      14.491           3312009
          5   Red    U |1234/5678/9A0B/DEFC|      12      24      16.310               609
          2   Red    L |1234/5678/9AB0/DEFC|      12      24      16.971               105
          1   Red    U |1234/5678/9ABC/DEF0|       0       0       0.000                 0
  45 moves: RDLLDRRULDLLUURRDDRUULULLDRDLURRURDLDLDRULULU

     Record Stash Move |Board layout by row|  Max|d|  Sum|d|   Euclidean   Encoded vs Zero
          1  Blue      |DAB6/5314/80C2/E79F|      15      94      29.155    16535302211892
          4  Blue    R |DAB6/5314/08C2/E79F|      15      94      28.879    16535302234212
         12  Blue    D |DAB6/0314/58C2/E79F|      15      94      28.178    16535581621572
         30  Blue    L |DAB6/3014/58C2/E79F|      15      94      28.284    16535251400772
         62  Blue    L |DAB6/3104/58C2/E79F|      15      94      28.320    16535218378692
        125  Blue    D |DA06/31B4/58C2/E79F|      15      86      26.721    16560125373252
        266  Blue    R |D0A6/31B4/58C2/E79F|      15      84      26.344    16971108746052
        568  Blue    R |0DA6/31B4/58C2/E79F|      15      82      25.846    20719775267652
       1203  Blue    U |3DA6/01B4/58C2/E79F|      15      86      26.306     3626483421252
       2480  Blue    L |3DA6/10B4/58C2/E79F|      15      86      26.344     3626080624452
       5044  Blue    D |30A6/1DB4/58C2/E79F|      15      78      24.290     3887608240452
      10136  Blue    L |3A06/1DB4/58C2/E79F|      15      80      24.698     3395673597252
      20466  Blue    L |3A60/1DB4/58C2/E79F|      15      82      24.940     3343462422852
      40817  Blue    U |3A64/1DB0/58C2/E79F|      15      82      25.573     3338668697412
      81229  Blue    U |3A64/1DB2/58C0/E79F|      15      82      25.884     3338668369068
     160022  Blue    R |3A64/1DB2/580C/E79F|      15      80      25.417     3338668369380
     314204  Blue    R |3A64/1DB2/508C/E79F|      15      80      25.100     3338668372500
     612122  Blue    U |3A64/1DB2/578C/E09F|      15      80      26.192     3338668368034
    1187843  Blue    L |3A64/1DB2/578C/E90F|      15      80      26.533     3338668368031
    2285379  Blue    L |3A64/1DB2/578C/E9F0|       8      50      15.937     3338668368030
   53890406   Red    D |3A64/1DB2/5780/E9FC|      12      74      23.281     3338668368109
   29075781   Red    R |3A64/1DB2/5708/E9FC|      12      74      22.935     3338668368613
   15538085   Red    D |3A64/1D02/57B8/E9FC|      12      66      20.928     3338669819773
    8239002   Red    L |3A64/1D20/57B8/E9FC|      12      66      21.024     3338666876413
    4329887   Red    U |3A64/1D28/57B0/E9FC|      12      66      22.494     3338666634469
    2260599   Red    R |3A64/1D28/570B/E9FC|      12      66      22.000     3338666634877
    1170753   Red    R |3A64/1D28/507B/E9FC|      12      66      21.679     3338666638597
     603141   Red    D |3A64/1028/5D7B/E9FC|      12      58      19.131     3338677523557
     308302   Red    D |3064/1A28/5D7B/E9FC|      12      50      16.912     3861729126757
     156805   Red    R |0364/1A28/5D7B/E9FC|      12      48      16.733    19815356416357
      79152   Red    U |1364/0A28/5D7B/E9FC|      12      48      16.971      106795667557
      39783   Red    U |1364/5A28/0D7B/E9FC|      12      48      18.111      106392865717
      19862   Red    L |1364/5A28/D07B/E9FC|      12      50      18.815      106392852037
       9877   Red    U |1364/5A28/D97B/E0FC|      14      50      20.640      106392848411
       4838   Red    R |1364/5A28/D97B/0EFC|      13      48      19.950      106392848421
       2337   Red    D |1364/5A28/097B/DEFC|      12      40      17.146      106392863529
       1115   Red    L |1364/5A28/907B/DEFC|      12      40      17.664      106392836889
        534   Red    D |1364/5028/9A7B/DEFC|      12      32      15.232      106414606089
        259   Red    L |1364/5208/9A7B/DEFC|      12      32      15.362      106381584009
        124   Red    D |1304/5268/9A7B/DEFC|      12      26      13.711      168648526089
         56   Red    R |1034/5268/9A7B/DEFC|      12      24      13.491     1227242062089
         24   Red    U |1234/5068/9A7B/DEFC|      12      24      14.071          36334089
         10   Red    L |1234/5608/9A7B/DEFC|      12      24      14.491           3312009
          5   Red    U |1234/5678/9A0B/DEFC|      12      24      16.310               609
          2   Red    L |1234/5678/9AB0/DEFC|      12      24      16.971               105
          1   Red    U |1234/5678/9ABC/DEF0|       0       0       0.000                 0
  45 moves: RDLLDRRULDLLUURRULLDRDLURRDDRUULURDLDLDRULULU

     Record Stash Move |Board layout by row|  Max|d|  Sum|d|   Euclidean   Encoded vs Zero
          1  Blue      |DAB6/5314/80C2/E79F|      15      94      29.155    16535302211892
          4  Blue    R |DAB6/5314/08C2/E79F|      15      94      28.879    16535302234212
         12  Blue    D |DAB6/0314/58C2/E79F|      15      94      28.178    16535581621572
         31  Blue    D |0AB6/D314/58C2/E79F|      15      86      26.268    20458524891972
         65  Blue    L |A0B6/D314/58C2/E79F|      15      88      26.646    13048370139972
        133  Blue    U |A3B6/D014/58C2/E79F|      15      90      27.092    11995513736772
        281  Blue    L |A3B6/D104/58C2/E79F|      15      90      27.129    11995480714692
        599  Blue    D |A306/D1B4/58C2/E79F|      15      82      25.456    12026654646852
       1266  Blue    R |A036/D1B4/58C2/E79F|      15      80      25.338    13004296912452
       2610  Blue    U |A136/D0B4/58C2/E79F|      15      80      25.495    11777091184452
       5305  Blue    R |A136/0DB4/58C2/E79F|      15      78      24.980    11777203677252
      10671  Blue    D |0136/ADB4/58C2/E79F|      15      70      23.324    19623050301252
      21525  Blue    L |1036/ADB4/58C2/E79F|      15      70      23.367     1228430858052
      42928  Blue    L |1306/ADB4/58C2/E79F|      15      72      23.495      169837322052
      85377  Blue    L |1360/ADB4/58C2/E79F|      15      74      23.749      111878128452
     168152  Blue    U |1364/ADB0/58C2/E79F|      15      74      24.413      106565484612
     329996  Blue    U |1364/ADB2/58C0/E79F|      15      74      24.739      106565156268
     642726  Blue    R |1364/ADB2/580C/E79F|      15      72      24.249      106565156580
    1246294  Blue    R |1364/ADB2/508C/E79F|      15      72      23.917      106565159700
    2396768  Blue    D |1364/A0B2/5D8C/E79F|      15      64      21.633      106576044660
   53888966   Red    R |1364/0AB2/5D8C/E79F|      15      62      21.166      106797401460
   29074972   Red    U |1364/5AB2/0D8C/E79F|      15      62      22.091      106394277060
   15537643   Red    L |1364/5AB2/D08C/E79F|      15      64      22.672      106394263380
    8238754   Red    U |1364/5AB2/D78C/E09F|      15      64      23.875      106394258914
    4329748   Red    L |1364/5AB2/D78C/E90F|      15      64      24.249      106394258911
    2260520   Red    L |1364/5AB2/D78C/E9F0|       6      34      11.747      106394258910
    1170710   Red    D |1364/5AB2/D780/E9FC|      12      58      20.640      106394258989
     603117   Red    R |1364/5AB2/D708/E9FC|      12      58      20.248      106394259493
     308288   Red    D |1364/5A02/D7B8/E9FC|      12      50      17.944      106396078573
     156795   Red    L |1364/5A20/D7B8/E9FC|      12      50      18.055      106393135213
      79148   Red    U |1364/5A28/D7B0/E9FC|      12      50      19.748      106392847909
      39781   Red    R |1364/5A28/D70B/E9FC|      12      50      19.183      106392848317
      19862   Red    R |1364/5A28/D07B/E9FC|      12      50      18.815      106392852037
       9877   Red    U |1364/5A28/D97B/E0FC|      14      50      20.640      106392848411
       4838   Red    R |1364/5A28/D97B/0EFC|      13      48      19.950      106392848421
       2337   Red    D |1364/5A28/097B/DEFC|      12      40      17.146      106392863529
       1115   Red    L |1364/5A28/907B/DEFC|      12      40      17.664      106392836889
        534   Red    D |1364/5028/9A7B/DEFC|      12      32      15.232      106414606089
        259   Red    L |1364/5208/9A7B/DEFC|      12      32      15.362      106381584009
        124   Red    D |1304/5268/9A7B/DEFC|      12      26      13.711      168648526089
         56   Red    R |1034/5268/9A7B/DEFC|      12      24      13.491     1227242062089
         24   Red    U |1234/5068/9A7B/DEFC|      12      24      14.071          36334089
         10   Red    L |1234/5608/9A7B/DEFC|      12      24      14.491           3312009
          5   Red    U |1234/5678/9A0B/DEFC|      12      24      16.310               609
          2   Red    L |1234/5678/9AB0/DEFC|      12      24      16.971               105
          1   Red    U |1234/5678/9ABC/DEF0|       0       0       0.000                 0
  45 moves: RDDLULDRURDLLLUURRDRULULLDRDLURRURDLDLDRULULU

     Record Stash Move |Board layout by row|  Max|d|  Sum|d|   Euclidean   Encoded vs Zero
          1  Blue      |DAB6/5314/80C2/E79F|      15      94      29.155    16535302211892
          4  Blue    R |DAB6/5314/08C2/E79F|      15      94      28.879    16535302234212
         12  Blue    D |DAB6/0314/58C2/E79F|      15      94      28.178    16535581621572
         31  Blue    D |0AB6/D314/58C2/E79F|      15      86      26.268    20458524891972
         65  Blue    L |A0B6/D314/58C2/E79F|      15      88      26.646    13048370139972
        133  Blue    U |A3B6/D014/58C2/E79F|      15      90      27.092    11995513736772
        281  Blue    L |A3B6/D104/58C2/E79F|      15      90      27.129    11995480714692
        599  Blue    D |A306/D1B4/58C2/E79F|      15      82      25.456    12026654646852
       1266  Blue    R |A036/D1B4/58C2/E79F|      15      80      25.338    13004296912452
       2610  Blue    U |A136/D0B4/58C2/E79F|      15      80      25.495    11777091184452
       5305  Blue    R |A136/0DB4/58C2/E79F|      15      78      24.980    11777203677252
      10671  Blue    D |0136/ADB4/58C2/E79F|      15      70      23.324    19623050301252
      21525  Blue    L |1036/ADB4/58C2/E79F|      15      70      23.367     1228430858052
      42928  Blue    L |1306/ADB4/58C2/E79F|      15      72      23.495      169837322052
      85377  Blue    L |1360/ADB4/58C2/E79F|      15      74      23.749      111878128452
     168152  Blue    U |1364/ADB0/58C2/E79F|      15      74      24.413      106565484612
     329996  Blue    U |1364/ADB2/58C0/E79F|      15      74      24.739      106565156268
     642726  Blue    R |1364/ADB2/580C/E79F|      15      72      24.249      106565156580
    1246294  Blue    R |1364/ADB2/508C/E79F|      15      72      23.917      106565159700
    2396770  Blue    U |1364/ADB2/578C/E09F|      15      72      25.060      106565155234
   53890081   Red    L |1364/ADB2/578C/E90F|      15      72      25.417      106565155231
   29075603   Red    L |1364/ADB2/578C/E9F0|       7      42      14.000      106565155230
   15537991   Red    D |1364/ADB2/5780/E9FC|      12      66      22.000      106565155309
    8238950   Red    R |1364/ADB2/5708/E9FC|      12      66      21.633      106565155813
    4329857   Red    D |1364/AD02/57B8/E9FC|      12      58      19.494      106566606973
    2260581   Red    L |1364/AD20/57B8/E9FC|      12      58      19.596      106563663613
    1170743   Red    U |1364/AD28/57B0/E9FC|      12      58      21.166      106563421669
     603137   Red    R |1364/AD28/570B/E9FC|      12      58      20.640      106563422077
     308301   Red    R |1364/AD28/507B/E9FC|      12      58      20.298      106563425797
     156804   Red    D |1364/A028/5D7B/E9FC|      12      50      17.550      106574310757
      79152   Red    R |1364/0A28/5D7B/E9FC|      12      48      16.971      106795667557
      39783   Red    U |1364/5A28/0D7B/E9FC|      12      48      18.111      106392865717
      19862   Red    L |1364/5A28/D07B/E9FC|      12      50      18.815      106392852037
       9877   Red    U |1364/5A28/D97B/E0FC|      14      50      20.640      106392848411
       4838   Red    R |1364/5A28/D97B/0EFC|      13      48      19.950      106392848421
       2337   Red    D |1364/5A28/097B/DEFC|      12      40      17.146      106392863529
       1115   Red    L |1364/5A28/907B/DEFC|      12      40      17.664      106392836889
        534   Red    D |1364/5028/9A7B/DEFC|      12      32      15.232      106414606089
        259   Red    L |1364/5208/9A7B/DEFC|      12      32      15.362      106381584009
        124   Red    D |1304/5268/9A7B/DEFC|      12      26      13.711      168648526089
         56   Red    R |1034/5268/9A7B/DEFC|      12      24      13.491     1227242062089
         24   Red    U |1234/5068/9A7B/DEFC|      12      24      14.071          36334089
         10   Red    L |1234/5608/9A7B/DEFC|      12      24      14.491           3312009
          5   Red    U |1234/5678/9A0B/DEFC|      12      24      16.310               609
          2   Red    L |1234/5678/9AB0/DEFC|      12      24      16.971               105
          1   Red    U |1234/5678/9ABC/DEF0|       0       0       0.000                 0
  45 moves: RDDLULDRURDLLLUURRULLDRDLURRDRULURDLDLDRULULU

```

The first and second move sequences start the same, until position 314204 (the sixteenth move) and then advance D to 612120 or U to 612122. In the first case the Euclidean distance falls while the encoded distance rises but in the second case the Euclidean distance rises while the encoded distance falls. If there might be any guidance to be found in these figures, it is not immediately obvious. But, after an increase, the next step achieves a large reduction, beyond the previous distance. Humm.


### =Maximum Separation=

Just how far away can a position be from ZERO? Alas, 16! is rather large, but lesser board sizes can be perused to completion in a reasonable time. Changing the values in <code>NR</code> and <code>NC</code> is easy, though some other changes are helpful. The hash calculation that is nicely balanced for the 4x4 board doesn't give good spray for a lesser board, but an ad-hoc change to <code>BRD(1)*9 + BRD(2)</code> works well enough. The blue tide is plugged by suppressing its code, and the red tide flows until it can flow no more. For these runs, the output is slightly edited to omit redundant information such as that referring to the blue tide which isn't flowing.

```txt

To play 'slide-square' with 3 rows and 4 columns.

Tide  Red
 Preparing a stash in file SlideSolveR3C4.123456789AB0.dat
 ... with an index in file SlideSolveR3C4.123456789AB0.ndx
   199999991  zero values for an empty index.
 1 2 3 4 5 6 7 8 9 A B 0 is the board layout in INTEGER*1
 4030201 8070605   B0A09       0 is the board layout in INTEGER*4
        48372615          B0A090 ..interleaved into two INTEGER*4
                        8AA0F74D combined.
                     -1969162419 as a decimal integer.
ABS(MOD(-1969162419,199999991)) + 2 = 169162502 is the record number for the first index entry.

     | Tidewrack Boundary Positions|      Positions   |     Primary Probes        Index Use  |Memory of Time Passed
Surge|     First      Last    Count|  Checked Deja vu%|     Made Max.L  Avg.L|  Used%   Load%|    CPU      Clock
    1|         1         1        1|        2     0.00|        0     0  0.000|  0.000   0.000|  0.0secs  0.0secs  0.00%  7:57:19.187am.
    2|         2         3        2|        4     0.00|        0     0  0.000|  0.000   0.000|  0.0secs  0.1secs 16.62%  7:57:19.281am.
    3|         4         7        4|        9     0.00|        0     0  0.000|  0.000   0.000|  0.0secs  0.0secs  0.00%  7:57:19.296am.
    4|         8        16        9|       20     0.00|        0     0  0.000|  0.000   0.000|  0.0secs  0.0secs         7:57:19.312am.
    5|        17        36       20|       37     0.00|        0     0  0.000|  0.000   0.000|  0.0secs  0.0secs         7:57:19.312am.
    6|        37        73       37|       64     1.56|        1     1  1.000|  0.000   0.000|  0.0secs  0.2secs  0.00%  7:57:19.468am.
    7|        74       136       63|      125     2.40|        3     1  1.000|  0.000   0.000|  0.0secs  0.0secs  0.00%  7:57:19.515am.
    8|       137       258      122|      241     3.73|        9     1  1.000|  0.000   0.000|  0.0secs  0.1secs  0.00%  7:57:19.609am.
    9|       259       490      232|      451     4.43|       20     1  1.000|  0.000   0.000|  0.0secs  0.1secs  0.00%  7:57:19.671am.
   10|       491       921      431|      827     5.56|       46     1  1.000|  0.001   0.001|  0.0secs  0.1secs  0.00%  7:57:19.796am.
   11|       922      1702      781|     1481     6.01|       89     1  1.000|  0.002   0.002|  0.0secs  0.2secs 15.32%  7:57:20.000am.
   12|      1703      3094     1392|     2671     6.63|      177     1  1.000|  0.003   0.003|  0.0secs  0.4secs  8.33%  7:57:20.375am.
   13|      3095      5588     2494|     4770     6.88|      328     1  1.000|  0.005   0.005|  0.1secs  1.0secs 11.12%  7:57:21.359am.
   14|      5589     10030     4442|     8502     7.62|      652     1  1.000|  0.009   0.009|  0.4secs  1.8secs 21.06%  7:57:23.140am.
   15|     10031     17884     7854|    15135     8.17|     1237     2  1.001|  0.016   0.016|  0.6secs  2.8secs 22.90%  7:57:25.937am.
   16|     17885     31783    13899|    26554     8.81|     2351     1  1.000|  0.028   0.028|  0.9secs  5.0secs 17.65%  7:57:30.984am.
   17|     31784     55998    24215|    46180     9.48|     4409     2  1.000|  0.049   0.049|  1.9secs  7.4secs 26.21%  7:57:38.375am.
   18|     55999     97800    41802|    79668    10.67|     8584     2  1.000|  0.084   0.084|  3.1secs 11.7secs 26.87%  7:57:50.062am.
   19|     97801    168967    71167|   135867    11.76|    16270     3  1.001|  0.144   0.144|  5.1secs 15.7secs 32.40%  7:58:05.734am.
   20|    168968    288855   119888|   228379    13.14|    30689     2  1.001|  0.243   0.244|  9.4secs 21.4secs 43.75%  7:58:27.125am.
   21|    288856    487218   198363|   377240    14.32|    55757     2  1.002|  0.404   0.405| 15.0secs 26.5secs 56.39%  7:58:53.671am.
   22|    487219    810424   323206|   612123    15.74|   100162     2  1.002|  0.660   0.663| 21.7secs 42.4secs 51.16%  7:59:36.093am.
   23|    810425   1326202   515778|   977833    17.06|   176608     3  1.003|  1.060   1.069| 32.7secs 59.2secs 55.29%  8:00:35.296am.
   24|   1326203   2137202   811000|  1533678    18.63|   306896     3  1.005|  1.674   1.693| 56.0secs 91.9secs 60.91%  8:02:07.203am.
   25|   2137203   3385213  1248011|  2358532    20.07|   522581     4  1.008|  2.592   2.635| 85.7secs  2.1mins 67.00%  8:04:15.062am.
   26|   3385214   5270492  1885279|  3554164    21.71|   877235     4  1.012|  3.930   4.026|  2.1mins  3.4mins 60.44%  8:07:40.187am.
   27|   5270493   8052888  2782396|  5239108    23.47|  1452058     5  1.017|  5.824   6.031|  3.1mins  4.4mins 70.93%  8:12:05.062am.
   28|   8052889  12062610  4009722|  7534021    25.39|  2357673     5  1.025|  8.412   8.842|  4.4mins  6.8mins 64.81%  8:18:50.843am.
   29|  12062611  17683964  5621354| 10540894    27.45|  3736777     5  1.037| 11.814  12.666|  5.9mins  8.7mins 67.49%  8:27:35.375am.
   30|  17683965  25331836  7647872| 14308274    29.65|  5757092     5  1.052| 16.090  17.699|  8.1mins 12.1mins 66.43%  8:39:42.640am.
   31|  25331837  35397636 10065800| 18773730    32.03|  8547647     6  1.072| 21.203  24.079| 10.4mins 14.8mins 70.68%  8:54:28.562am.
   32|  35397637  48158049 12760413| 23759506    34.47| 12146155     7  1.097| 27.009  31.864| 13.2mins 19.0mins 69.20%  9:13:31.234am.
   33|  48158050  63728835 15570786| 28870409    37.06| 16437759     8  1.126| 33.226  40.950| 15.7mins 21.8mins 72.16%  9:35:16.421am.
   34|  63728836  81900441 18171606| 33626342    39.63| 20990966     9  1.157| 39.543  51.100| 18.8mins 31.2mins 60.26% 10:06:28.671am.
   35|  81900442 102200317 20299876| 37402396    42.28| 25357818     9  1.193| 45.566  61.894| 21.7mins 43.3mins 50.19% 10:49:46.296am.
   36| 102200318 123787565 21587248| 39666978    44.94| 28674025    11  1.222| 51.062  72.814| 24.1mins 55.6mins 43.32% 11:45:22.359am.
   37| 123787566 145628724 21841159| 39956652    47.68| 30596301    10  1.253| 55.742  83.268| 24.9mins 62.8mins 39.61% 12:48:08.968pm.
   38| 145628725 166535629 20906905| 38122360    50.42| 30382490    10  1.269| 59.612  92.717| 24.8mins 65.3mins 37.94%  1:53:27.953pm.
   39| 166535630 185434986 18899357| 34311262    53.20| 28383116    11  1.286| 62.576 100.747| 22.4mins 61.1mins 36.70%  2:54:31.156pm.
   40| 185434987 201493321 16058335| 29019233    55.99| 24579603    12  1.283| 64.796 107.133| 19.0mins 52.2mins 36.48%  3:46:40.171pm.
   41| 201493322 214265924 12772603| 23016150    58.66| 19948189    10  1.281| 66.330 111.891| 14.8mins 40.7mins 36.41%  4:27:20.140pm.
   42| 214265925 223781141  9515217| 17041788    61.37| 14979073    11  1.262| 67.361 115.182| 10.9mins 29.0mins 37.55%  4:56:23.062pm.
   43| 223781142 230364322  6583181| 11772739    63.96| 10506946    10  1.249| 67.994 117.304|  7.0mins 19.0mins 37.17%  5:15:21.000pm.
   44| 230364323 234607075  4242753|  7532690    66.76|  6789748    11  1.222| 68.366 118.555|  4.3mins 11.3mins 37.93%  5:26:38.500pm.
   45| 234607076 237110948  2503873|  4439978    69.59|  4054719    10  1.206| 68.558 119.231|  2.3mins  6.1mins 37.47%  5:32:47.218pm.
   46| 237110949 238461216  1350268|  2371273    72.87|  2185279    10  1.176| 68.651 119.552| 70.6secs  2.9mins 39.92%  5:35:44.015pm.
   47| 238461217 239104461   643245|  1130010    76.08|  1055623    10  1.160| 68.689 119.687| 29.4secs 74.9secs 39.21%  5:36:58.890pm.
   48| 239104462 239374764   270303|   466424    80.21|   440280    10  1.125| 68.702 119.734| 10.8secs 26.0secs 41.36%  5:37:24.921pm.
   49| 239374765 239467075    92311|   161691    83.23|   154374     8  1.111| 68.705 119.747|  3.4secs  7.6secs 45.08%  5:37:32.546pm.
   50| 239467076 239494191    27116|    44973    88.02|    43422    11  1.071| 68.706 119.750|  0.8secs  1.5secs 56.82%  5:37:34.031pm.
   51| 239494192 239499581     5390|     9553    88.33|     9259     6  1.074| 68.706 119.750|  0.1secs  0.4secs 33.32%  5:37:34.453pm.
   52| 239499582 239500696     1115|     1625    94.71|     1593     5  1.028| 68.706 119.750|  0.0secs  0.0secs104.17%  5:37:34.468pm.
   53| 239500697 239500782       86|      167    89.22|      162     3  1.049| 68.706 119.750|  0.0secs  0.0secs 97.66%  5:37:34.484pm.
   54| 239500783 239500800       18|       18   100.00|       18     1  1.000| 68.706 119.750|  0.0secs  0.0secs         5:37:34.484pm.

The boundary has not surged to new positions!
The now-static boundary has 18

     Record Stash Move | Board layout | Max|d| Sum|d| Euclidean Encoded vs Zero
  239500783   Red    D |0869/B725/43A1|      7     44    14.765       466581807
  239500784   Red    D |0869/B7A1/4325|      9     58    18.601       466582214
  239500785   Red    R |8759/43A2/0B61|      9     48    16.310       302860487
  239500786   Red    R |4325/8761/0BA9|      9     38    15.362       127427903
  239500787   Red    R |0821/B3A5/4769|      9     48    15.811       464885186
  239500788   Red    D |0B21/3765/48A9|      9     38    14.765       475738105
  239500789   Red    R |4321/8B65/07A9|      9     42    15.362       127389739
  239500790   Red    D |0861/B325/47A9|      9     48    15.811       466336825
  239500791   Red    D |0829/B365/47A1|      6     36    12.410       465127617
  239500792   Red    R |0829/B7A5/4361|      7     44    14.765       465130767
  239500793   Red    R |4321/8769/0BA5|      9     30    11.832       127387607
  239500794   Red    R |B821/37A5/0469|     10     58    19.799       424935162
  239500795   Red    R |4361/8725/0BA9|      9     42    15.362       128113223
  239500796   Red    R |4321/B765/08A9|      9     40    15.297       127402699
  239500797   Red    U |8369/4725/0BA1|      9     38    14.283       288340727
  239500798   Red    U |8329/476A/0B51|      9     36    14.213       287247191
  239500799   Red    R |0321/8765/4BA9|      9     30    11.832       446727869
  239500800   Red    R |8321/4765/0BA9|      9     38    15.362       287039663
No progress!

```


Whereas for four rows and three columns,

```txt

To play 'slide-square' with 4 rows and 3 columns.

Tide  Red
 Preparing a stash in file SlideSolveR4C3.123456789AB0.dat
 ... with an index in file SlideSolveR4C3.123456789AB0.ndx
   199999991  zero values for an empty index.
 1 2 3 4 5 6 7 8 9 A B 0 is the board layout in INTEGER*1
 4030201 8070605   B0A09       0 is the board layout in INTEGER*4
        48372615          B0A090 ..interleaved into two INTEGER*4
                        8AA0F74D combined.
                     -1969162419 as a decimal integer.
ABS(MOD(-1969162419,199999991)) + 2 = 169162502 is the record number for the first index entry.

     | Tidewrack Boundary Positions |    Positions     |    Primary Probes       Index Use   |Memory of Time Passed
Surge|     First       Last    Count|  Checked Deja vu%|     Made Max.L Avg.L|  Used%   Load%|    CPU      Clock
    1|         1          1        1|        2     0.00|        0     0 0.000|  0.000   0.000|  0.0secs  0.0secs  0.00%  7:50:20.203pm.
    2|         2          3        2|        4     0.00|        0     0 0.000|  0.000   0.000|  0.0secs  0.1secs 12.50%  7:50:20.328pm.
    3|         4          7        4|        9     0.00|        0     0 0.000|  0.000   0.000|  0.0secs  0.0secs         7:50:20.328pm.
    4|         8         16        9|       20     0.00|        0     0 0.000|  0.000   0.000|  0.0secs  0.0secs  0.00%  7:50:20.343pm.
    5|        17         36       20|       37     0.00|        0     0 0.000|  0.000   0.000|  0.0secs  0.1secs  0.00%  7:50:20.421pm.
    6|        37         73       37|       64     1.56|        1     1 1.000|  0.000   0.000|  0.0secs  0.0secs  0.00%  7:50:20.437pm.
    7|        74        136       63|      125     2.40|        3     1 1.000|  0.000   0.000|  0.0secs  0.2secs  8.31%  7:50:20.625pm.
    8|       137        258      122|      241     3.73|        9     1 1.000|  0.000   0.000|  0.0secs  0.1secs 50.40%  7:50:20.687pm.
    9|       259        490      232|      451     4.43|       20     1 1.000|  0.000   0.000|  0.0secs  0.1secs 49.87%  7:50:20.781pm.
   10|       491        921      431|      827     5.56|       46     1 1.000|  0.001   0.001|  0.1secs  0.2secs 63.59%  7:50:20.953pm.
   11|       922       1702      781|     1481     6.01|       89     1 1.000|  0.002   0.002|  0.1secs  0.3secs 35.06%  7:50:21.265pm.
   12|      1703       3094     1392|     2671     6.63|      177     1 1.000|  0.003   0.003|  0.2secs  0.6secs 41.63%  7:50:21.828pm.
   13|      3095       5588     2494|     4770     6.88|      328     1 1.000|  0.005   0.005|  0.5secs  1.5secs 36.19%  7:50:23.296pm.
   14|      5589      10030     4442|     8502     7.62|      648     1 1.000|  0.009   0.009|  0.8secs  2.2secs 34.50%  7:50:25.515pm.
   15|     10031      17884     7854|    15135     8.17|     1237     1 1.000|  0.016   0.016|  1.3secs  3.9secs 33.60%  7:50:29.421pm.
   16|     17885      31783    13899|    26554     8.81|     2344     1 1.000|  0.028   0.028|  2.2secs  5.9secs 36.77%  7:50:35.328pm.
   17|     31784      55998    24215|    46180     9.48|     4397     1 1.000|  0.049   0.049|  3.1secs  9.5secs 32.73%  7:50:44.781pm.
   18|     55999      97800    41802|    79668    10.67|     8574     2 1.000|  0.084   0.084|  6.0secs 14.5secs 41.25%  7:50:59.328pm.
   19|     97801     168967    71167|   135867    11.76|    16154     2 1.000|  0.144   0.144|  9.3secs 20.5secs 45.46%  7:51:19.812pm.
   20|    168968     288855   119888|   228379    13.14|    30520     2 1.000|  0.243   0.244| 14.7secs 27.9secs 52.72%  7:51:47.671pm.
   21|    288856     487218   198363|   377240    14.32|    55395     2 1.001|  0.404   0.405| 18.5secs 33.0secs 56.18%  7:52:20.656pm.
   22|    487219     810424   323206|   612123    15.74|    99611     2 1.002|  0.660   0.663| 22.4secs 41.7secs 53.62%  7:53:02.359pm.
   23|    810425    1326202   515778|   977833    17.06|   175049     2 1.003|  1.062   1.069| 35.9secs 61.9secs 57.96%  7:54:04.250pm.
   24|   1326203    2137202   811000|  1533678    18.63|   305322     3 1.004|  1.676   1.693| 55.4secs 94.5secs 58.68%  7:55:38.718pm.
   25|   2137203    3385213  1248011|  2358532    20.07|   519257     4 1.007|  2.596   2.635| 87.8secs  2.3mins 63.69%  7:57:56.562pm.
   26|   3385214    5270492  1885279|  3554164    21.71|   872963     3 1.010|  3.936   4.026|  2.2mins  3.4mins 63.19%  8:01:21.109pm.
   27|   5270493    8052888  2782396|  5239108    23.47|  1445680     4 1.016|  5.833   6.031|  3.2mins  4.8mins 66.44%  8:06:08.906pm.
   28|   8052889   12062610  4009722|  7534021    25.39|  2348797     4 1.024|  8.426   8.842|  4.5mins  6.9mins 65.49%  8:13:00.218pm.
   29|  12062611   17683964  5621354| 10540894    27.45|  3734408     5 1.036| 11.829  12.666|  6.4mins  9.1mins 70.67%  8:22:04.328pm.
   30|  17683965   25331836  7647872| 14308274    29.65|  5746564     5 1.051| 16.110  17.699|  8.3mins 12.3mins 67.88%  8:34:21.375pm.
   31|  25331837   35397636 10065800| 18773730    32.03|  8552447     6 1.071| 21.220  24.079| 10.9mins 15.7mins 69.40%  8:50:02.062pm.
   32|  35397637   48158049 12760413| 23759506    34.47| 12145305     7 1.096| 27.027  31.864| 13.8mins 19.5mins 70.96%  9:09:32.671pm.
   33|  48158050   63728835 15570786| 28870409    37.06| 16448462     7 1.125| 33.238  40.950| 16.7mins 23.2mins 71.94%  9:32:46.109pm.
   34|  63728836   81900441 18171606| 33626342    39.63| 21022994     9 1.158| 39.540  51.100| 20.1mins 32.4mins 62.17% 10:05:09.859pm.
   35|  81900442  102200317 20299876| 37402396    42.28| 25358874     8 1.192| 45.562  61.894| 21.9mins 44.7mins 49.02% 10:49:50.296pm.
   36| 102200318  123787565 21587248| 39666978    44.94| 28734429    10 1.224| 51.028  72.814| 25.6mins 56.6mins 45.33% 11:46:25.156pm.
   37| 123787566  145628724 21841159| 39956652    47.68| 30572287    11 1.252| 55.720  83.268| 27.1mins 64.5mins 41.99%  0:50:52.171am.
   38| 145628725  166535629 20906905| 38122360    50.42| 30446707    10 1.273| 59.558  92.717| 26.6mins 66.4mins 40.12%  1:57:16.468am.
   39| 166535630  185434986 18899357| 34311262    53.20| 28341786    10 1.283| 62.543 100.747| 23.7mins 62.2mins 38.16%  2:59:28.250am.
   40| 185434987  201493321 16058335| 29019233    55.99| 24605484    10 1.285| 64.750 107.133| 20.0mins 53.8mins 37.26%  3:53:15.640am.
   41| 201493322  214265924 12772603| 23016150    58.66| 19903099    11 1.276| 66.306 111.891| 15.5mins 41.1mins 37.86%  4:34:19.671am.
   42| 214265925  223781141  9515217| 17041788    61.37| 14979317    10 1.262| 67.337 115.182| 11.3mins 29.2mins 38.48%  5:03:33.937am.
   43| 223781142  230364322  6583181| 11772739    63.96| 10478430    12 1.241| 67.985 117.304|  7.4mins 19.1mins 38.79%  5:22:39.250am.
   44| 230364323  234607075  4242753|  7532690    66.76|  6785656    11 1.220| 68.358 118.555|  4.4mins 11.4mins 38.96%  5:34:04.359am.
   45| 234607076  237110948  2503873|  4439978    69.59|  4041159    10 1.197| 68.558 119.231|  2.4mins  6.2mins 38.76%  5:40:15.031am.
   46| 237110949  238461216  1350268|  2371273    72.87|  2182892    11 1.172| 68.652 119.552| 70.1secs  3.0mins 39.48%  5:43:12.625am.
   47| 238461217  239104461   643245|  1130010    76.08|  1051526    10 1.149| 68.691 119.687| 30.2secs 76.2secs 39.66%  5:44:28.812am.
   48| 239104462  239374764   270303|   466424    80.21|   439307    10 1.118| 68.705 119.734| 11.2secs 26.3secs 42.40%  5:44:55.125am.
   49| 239374765  239467075    92311|   161691    83.23|   153927     9 1.102| 68.708 119.747|  3.6secs  7.9secs 45.63%  5:45:03.000am.
   50| 239467076  239494191    27116|    44973    88.02|    43358     9 1.067| 68.709 119.750|  0.8secs  1.7secs 44.15%  5:45:04.734am.
   51| 239494192  239499581     5390|     9553    88.33|     9231     6 1.062| 68.709 119.750|  0.1secs  0.3secs 52.87%  5:45:05.000am.
   52| 239499582  239500696     1115|     1625    94.71|     1598     4 1.029| 68.709 119.750|  0.0secs  0.0secs100.81%  5:45:05.031am.
   53| 239500697  239500782       86|      167    89.22|      161     3 1.043| 68.709 119.750|  0.0secs  0.0secs         5:45:05.031am.
   54| 239500783  239500800       18|       18   100.00|       18     1 1.000| 68.709 119.750|  0.0secs  0.0secs         5:45:05.031am.

The boundary has not surged to new positions!
The now-static boundary has 18

     Record Stash Move |  Board layout | Max|d| Sum|d| Euclidean Encoded vs Zero
  239500783   Red    D |09A/B78/456/321|      9     52    17.720       471375815
  239500784   Red    L |A90/78B/456/123|      9     60    19.494       391824450
  239500785   Red    L |BA0/789/546/321|     10     56    18.974       435370175
  239500786   Red    D |BA0/789/452/361|     10     56    18.493       435370041
  239500787   Red    R |07A/98B/456/123|      9     56    18.221       464077890
  239500788   Red    R |09A/B87/465/321|      9     52    17.833       471380879
  239500789   Red    R |09A/B87/546/321|      9     52    17.833       471380975
  239500790   Red    R |09A/B87/564/312|     10     54    18.547       471380998
  239500791   Red    L |970/B8A/465/123|      9     60    19.287       344730714
  239500792   Red    L |AB0/789/546/123|      9     60    19.950       395453370
  239500793   Red    L |BA0/879/265/341|     10     56    18.601       435410157
  239500794   Red    R |09A/B78/546/123|      9     56    18.868       471375930
  239500795   Red    L |AB0/789/456/132|      9     58    19.339       395453251
  239500796   Red    R |09A/B78/465/123|      9     56    18.868       471375834
  239500797   Red    D |AB0/798/456/123|      9     60    19.950       395458290
  239500798   Red    L |AB0/789/456/213|     10     60    19.950       395453252
  239500799   Red    L |BA0/789/456/123|     10     60    19.950       435370050
  239500800   Red    R |0BA/789/456/123|      9     56    18.868       478915650
No progress!

```


Reducing to a 3x3 board encouraged a reduction in the size of the index. The run took about ten seconds.

```txt

To play 'slide-square' with 3 rows and 3 columns.

Tide  Red
 Preparing a stash in file SlideSolveR3C3.123456780.dat
 ... with an index in file SlideSolveR3C3.123456780.ndx
      199991  zero values for an empty index.
 1 2 3 4 5 6 7 8 0 is the board layout in INTEGER*1
 4030201 8070605       0       0 is the board layout in INTEGER*4
        48372615               0 ..interleaved into two INTEGER*4
                        89F056BD multiplied together in INTEGER*4
                     -1980737859 as a decimal integer.
ABS(MOD(-1980737859,199991)) + 2 = 26997 is the record number for the first index entry.

     |Tidewrack Boundary Positions|  Positions     |  Primary Probes       Index Use
Surge|   First       Last    Count|Checked Deja vu%|  Made Max.L  Avg.L|  Used%  Load%
    1|       1          1        1|      2     0.00|     0     0  0.000|  0.002  0.002
    2|       2          3        2|      4     0.00|     0     0  0.000|  0.004  0.004
    3|       4          7        4|      8     0.00|     0     0  0.000|  0.008  0.008
    4|       8         15        8|     16     0.00|     0     0  0.000|  0.016  0.016
    5|      16         31       16|     20     0.00|     0     0  0.000|  0.026  0.026
    6|      32         51       20|     40     2.50|     1     1  1.000|  0.045  0.045
    7|      52         90       39|     65     4.62|     3     1  1.000|  0.076  0.076
    8|      91        152       62|    124     6.45|     8     1  1.000|  0.134  0.134
    9|     153        268      116|    164     7.32|    12     1  1.000|  0.210  0.210
   10|     269        420      152|    304     5.92|    18     1  1.000|  0.353  0.353
   11|     421        706      286|    430     7.91|    38     1  1.000|  0.549  0.551
   12|     707       1102      396|    792     5.56|    53     1  1.000|  0.919  0.925
   13|    1103       1850      748|   1114     8.08|    97     2  1.010|  1.427  1.437
   14|    1851       2874     1024|   2048     7.57|   189     1  1.000|  2.357  2.384
   15|    2875       4767     1893|   2799    10.25|   356     2  1.006|  3.578  3.640
   16|    4768       7279     2512|   5024    10.73|   738     2  1.019|  5.721  5.882
   17|    7280      11764     4485|   6599    14.56|  1365     2  1.023|  8.338  8.701
   18|   11765      17402     5638|  11276    15.49|  2734     3  1.042| 12.610 13.466
   19|   17403      26931     9529|  13867    21.55|  4630     3  1.054| 17.228 18.905
   20|   26932      37809    10878|  21756    21.89|  8302     4  1.087| 23.956 27.402
   21|   37810      54802    16993|  24289    29.56| 11793     4  1.102| 30.204 35.958
   22|   54803      71912    17110|  34220    30.01| 18451     4  1.151| 38.089 47.934
   23|   71913      95864    23952|  33912    40.36| 21895     6  1.165| 44.097 58.047
   24|   95865     116088    20224|  40448    40.55| 27617     6  1.205| 50.513 70.071
   25|  116089     140135    24047|  33131    52.98| 25580     7  1.200| 54.289 77.860
   26|  140136     155713    15578|  31156    53.27| 24657     5  1.208| 57.539 85.140
   27|  155714     170273    14560|  19530    67.88| 16842     6  1.157| 58.883 88.277
   28|  170274     176547     6274|  12548    68.84| 10945     5  1.137| 59.684 90.233
   29|  176548     180457     3910|   4926    84.57|  4615     6  1.068| 59.840 90.613
   30|  180458     181217      760|   1520    85.46|  1424     4  1.064| 59.888 90.723
   31|  181218     181438      221|    265    99.25|   264     1  1.000| 59.888 90.724
   32|  181439     181440        2|      4   100.00|     4     1  1.000| 59.888 90.724

The boundary has not surged to new positions!
The now-static boundary has 2

     Record Stash Move |Board  plan| Max|d| Sum|d| Euclidean Encoded vs Zero
     181439   Red    D |647/850/321|      6     32    12.247          220175
     181440   Red    U |867/254/301|      8     32    13.038          311247
No progress!

```


Thus, for a 3x3 board the greatest separation is thirty-two steps to two positions, while the 3x4 and 4x3 boards both have eighteen positions fifty-two steps away from ZERO.


## Go

{{trans|C++}}

```go
package main

import "fmt"

var (
    Nr = [16]int{3, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3}
    Nc = [16]int{3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2}
)

var (
    n, _n      int
    N0, N3, N4 [85]int
    N2         [85]uint64
)

const (
    i = 1
    g = 8
    e = 2
    l = 4
)

func fY() bool {
    if N2[n] == 0x123456789abcdef0 {
        return true
    }
    if N4[n] <= _n {
        return fN()
    }
    return false
}

func fZ(w int) bool {
    if w&i > 0 {
        fI()
        if fY() {
            return true
        }
        n--
    }
    if w&g > 0 {
        fG()
        if fY() {
            return true
        }
        n--
    }
    if w&e > 0 {
        fE()
        if fY() {
            return true
        }
        n--
    }
    if w&l > 0 {
        fL()
        if fY() {
            return true
        }
        n--
    }
    return false
}

func fN() bool {
    switch N0[n] {
    case 0:
        switch N3[n] {
        case 'l':
            return fZ(i)
        case 'u':
            return fZ(e)
        default:
            return fZ(i + e)
        }
    case 3:
        switch N3[n] {
        case 'r':
            return fZ(i)
        case 'u':
            return fZ(l)
        default:
            return fZ(i + l)
        }
    case 1, 2:
        switch N3[n] {
        case 'l':
            return fZ(i + l)
        case 'r':
            return fZ(i + e)
        case 'u':
            return fZ(e + l)
        default:
            return fZ(l + e + i)
        }
    case 12:
        switch N3[n] {
        case 'l':
            return fZ(g)
        case 'd':
            return fZ(e)
        default:
            return fZ(e + g)
        }
    case 15:
        switch N3[n] {
        case 'r':
            return fZ(g)
        case 'd':
            return fZ(l)
        default:
            return fZ(g + l)
        }
    case 13, 14:
        switch N3[n] {
        case 'l':
            return fZ(g + l)
        case 'r':
            return fZ(e + g)
        case 'd':
            return fZ(e + l)
        default:
            return fZ(g + e + l)
        }
    case 4, 8:
        switch N3[n] {
        case 'l':
            return fZ(i + g)
        case 'u':
            return fZ(g + e)
        case 'd':
            return fZ(i + e)
        default:
            return fZ(i + g + e)
        }
    case 7, 11:
        switch N3[n] {
        case 'd':
            return fZ(i + l)
        case 'u':
            return fZ(g + l)
        case 'r':
            return fZ(i + g)
        default:
            return fZ(i + g + l)
        }
    default:
        switch N3[n] {
        case 'd':
            return fZ(i + e + l)
        case 'l':
            return fZ(i + g + l)
        case 'r':
            return fZ(i + g + e)
        case 'u':
            return fZ(g + e + l)
        default:
            return fZ(i + g + e + l)
        }
    }
}

func fI() {
    g := (11 - N0[n]) * 4
    a := N2[n] & uint64(15<<uint(g))
    N0[n+1] = N0[n] + 4
    N2[n+1] = N2[n] - a + (a << 16)
    N3[n+1] = 'd'
    N4[n+1] = N4[n]
    cond := Nr[a>>uint(g)] <= N0[n]/4
    if !cond {
        N4[n+1]++
    }
    n++
}

func fG() {
    g := (19 - N0[n]) * 4
    a := N2[n] & uint64(15<<uint(g))
    N0[n+1] = N0[n] - 4
    N2[n+1] = N2[n] - a + (a >> 16)
    N3[n+1] = 'u'
    N4[n+1] = N4[n]
    cond := Nr[a>>uint(g)] >= N0[n]/4
    if !cond {
        N4[n+1]++
    }
    n++
}

func fE() {
    g := (14 - N0[n]) * 4
    a := N2[n] & uint64(15<<uint(g))
    N0[n+1] = N0[n] + 1
    N2[n+1] = N2[n] - a + (a << 4)
    N3[n+1] = 'r'
    N4[n+1] = N4[n]
    cond := Nc[a>>uint(g)] <= N0[n]%4
    if !cond {
        N4[n+1]++
    }
    n++
}

func fL() {
    g := (16 - N0[n]) * 4
    a := N2[n] & uint64(15<<uint(g))
    N0[n+1] = N0[n] - 1
    N2[n+1] = N2[n] - a + (a >> 4)
    N3[n+1] = 'l'
    N4[n+1] = N4[n]
    cond := Nc[a>>uint(g)] >= N0[n]%4
    if !cond {
        N4[n+1]++
    }
    n++
}

func fifteenSolver(n int, g uint64) {
    N0[0] = n
    N2[0] = g
    N4[0] = 0
}

func solve() {
    if fN() {
        fmt.Print("Solution found in ", n, " moves: ")
        for g := 1; g <= n; g++ {
            fmt.Printf("%c", N3[g])
        }
        fmt.Println()
    } else {
        n = 0
        _n++
        solve()
    }
}

func main() {
    fifteenSolver(8, 0xfe169b4c0a73d852)
    solve()
}
```


### Output

```txt

Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd

```



## Julia

{{trans|C++}}

```julia
const Nr = [3, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3]
const Nc = [3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2]

const N0 = zeros(Int, 85)
const N2 = zeros(UInt64, 85)
const N3 = zeros(UInt8, 85)
const N4 = zeros(Int, 85)
const i = 1
const g = 8
const ee = 2
const l = 4
const _n = Vector{Int32}([0])

function fY(n::Int)
    if N2[n + 1] == UInt64(0x123456789abcdef0)
        return true, n
    end
    if N4[n + 1] <= _n[1]
        return fN(n)
    end
    false, n
end

function fZ(w, n)
    if w & i > 0
        n = fI(n)
        (y, n) = fY(n)
        if y return (true, n) end
        n -= 1
    end
    if w & g > 0
        n = fG(n)
        (y, n) = fY(n)
        if y return (true, n) end
        n -= 1
    end
    if w & ee > 0
        n = fE(n)
        (y, n) = fY(n)
        if y return (true, n) end
        n -= 1
    end
    if w & l > 0
        n = fL(n)
        (y, n) = fY(n)
        if y return (true, n) end
        n -= 1
    end
    false, n
end

function fN(n::Int)
    x = N0[n + 1]
    y = UInt8(N3[n + 1])
    if x == 0
        if y == UInt8('l')
            return fZ(i, n)
        elseif y == UInt8('u')
            return fZ(ee, n)
        else
            return fZ(i + ee, n)
        end
    elseif x == 3
        if y == UInt8('r')
            return fZ(i, n)
        elseif y == UInt8('u')
            return fZ(l, n)
        else
            return fZ(i + l, n)
        end
    elseif x == 1 || x == 2
        if y == UInt8('l')
            return fZ(i + l, n)
        elseif y == UInt8('r')
            return fZ(i + ee, n)
        elseif y == UInt8('u')
            return fZ(ee + l, n)
        else
            return fZ(l + ee + i, n)
        end
    elseif x == 12
        if y == UInt8('l')
            return fZ(g, n)
        elseif y == UInt8('d')
            return fZ(ee, n)
        else
            return fZ(ee + g, n)
        end
    elseif x == 15
        if y == UInt8('r')
            return fZ(g, n)
        elseif y == UInt8('d')
            return fZ(l, n)
        else
            return fZ(g + l, n)
        end
    elseif x == 13 || x == 14
        if y == UInt8('l')
            return fZ(g + l, n)
        elseif y == UInt8('r')
            return fZ(ee + g, n)
        elseif y == UInt8('d')
            return fZ(ee + l, n)
        else
            return fZ(g + ee + l, n)
        end
    elseif x == 4 || x == 8
        if y == UInt8('l')
            return fZ(i + g, n)
        elseif y == UInt8('u')
            return fZ(g + ee, n)
        elseif y == UInt8('d')
            return fZ(i + ee, n)
        else
            return fZ(i + g + ee, n)
        end
    elseif x == 7 || x == 11
        if y == UInt8('d')
            return fZ(i + l, n)
        elseif y == UInt8('u')
            return fZ(g + l, n)
        elseif y == UInt8('r')
                return fZ(i + g, n)
        else
            return fZ(i + g + l, n)
        end
    else
        if y == UInt8('d')
            return fZ(i + ee + l, n)
        elseif y == UInt8('l')
                return fZ(i + g + l, n)
        elseif y == UInt8('r')
            return fZ(i + g + ee, n)
        elseif y == UInt8('u')
            return fZ(g + ee + l, n)
        else
            return fZ(i + g + ee + l, n)
        end
    end
end

function fI(n)
    gg = (11 - N0[n + 1]) * 4
    a = N2[n + 1] & (UInt64(0xf) << UInt(gg))
    N0[n + 2] = N0[n + 1] + 4
    N2[n + 2] = N2[n + 1] - a + (a << 16)
    N3[n + 2] = UInt8('d')
    N4[n + 2] = N4[n + 1]
    cond = Nr[(a >> gg) + 1] <= div(N0[n + 1], 4)
    if !cond
        N4[n + 2] += 1
    end
    n += 1
    n
end

function fG(n)
    gg = (19 - N0[n + 1]) * 4
    a = N2[n + 1] & (UInt64(0xf) << UInt(gg))
    N0[n + 2] = N0[n + 1] - 4
    N2[n + 2] = N2[n + 1] - a + (a >> 16)
    N3[n + 2] = UInt8('u')
    N4[n + 2] = N4[n + 1]
    cond = Nr[(a >> gg) + 1] >= div(N0[n + 1], 4)
    if !cond
        N4[n + 2] += 1
    end
    n += 1
    n
end

function fE(n)
    gg = (14 - N0[n + 1]) * 4
    a = N2[n + 1] & (UInt64(0xf) << UInt(gg))
    N0[n + 2] = N0[n + 1] + 1
    N2[n + 2] = N2[n + 1] - a + (a << 4)
    N3[n + 2] = UInt8('r')
    N4[n + 2] = N4[n + 1]
    cond = Nc[(a >> gg) + 1] <= N0[n + 1] % 4
    if !cond
        N4[n + 2] += 1
    end
    n += 1
    n
end

function fL(n)
    gg = (16 - N0[n + 1]) * 4
    a = N2[n + 1] & (UInt64(0xf) << UInt(gg))
    N0[n + 2] = N0[n + 1] - 1
    N2[n + 2] = N2[n + 1] - a + (a >> 4)
    N3[n + 2] = UInt8('l')
    N4[n + 2] = N4[n + 1]
    cond = Nc[(a >> gg) + 1] >= N0[n + 1] % 4
    if !cond
        N4[n + 2] += 1
    end
    n += 1
    n
end

function solve(n)
    ans, n = fN(n)
    if ans
        println("Solution found in $n moves: ")
        for ch in N3[2:n+1] print(Char(ch)) end; println()
    else
        println("next iteration, _n[1] will be $(_n[1] + 1)...")
        n = 0; _n[1] += 1; solve(n)
    end
end

run() = (N0[1] = 8; _n[1] = 1; N2[1] = 0xfe169b4c0a73d852; solve(0))
run()

```
 ### Output
```txt

 next iteration, _n[1] will be 2...
 next iteration, _n[1] will be 3...
 next iteration, _n[1] will be 4...
 next iteration, _n[1] will be 5...
 next iteration, _n[1] will be 6...
 next iteration, _n[1] will be 7...
 next iteration, _n[1] will be 8...
 Solution found in 52 moves:
 rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd

```



## Pascal


### The Solver


```pascal

unit FifteenSolverT;
\\ Solve 15 Puzzle. Nigel Galloway; February 1st., 2019.
interface
type TN=record n:UInt64; i,g,e,l:shortint; end;
type TG=record found:boolean; path:array[0..99] of TN; end;
function solve15(const board : UInt64; const bPos:shortint; const d:shortint; const ng:shortint):TG;
const endPos:UInt64=$123456789abcdef0;
implementation
const N:array[0..15] of shortint=(3,0,0,0,0,1,1,1,1,2,2,2,2,3,3,3);
const I:array[0..15] of shortint=(3,0,1,2,3,0,1,2,3,0,1,2,3,0,1,2);
const G:array[0..15] of shortint=(5,13,13,9,7,15,15,11,7,15,15,11,6,14,14,10);
const E:array[0..15] of shortint=(0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4);
const L:array[0..4 ] of shortint=(0,11,19,14,16);
function solve15(const board:UInt64; const bPos:shortint; const d:shortint; const ng:shortint):TG;
var path:TG; P:^TN; Q:^TN; _g:shortint; _n:UInt64;
begin P:=@path.path; P^.n:=board; P^.i:=0; P^.g:=0; P^.e:=ng; P^.l:=bPos;
  while true do begin
    if P<@path.path then begin path.found:=false; exit(path); end;
    if P^.n=endPos  then begin path.found:=true; exit(path); end;
    if (P^.e=0) or (P^.i>d) then begin P-=1; continue; end else begin Q:=P+1; Q^.g:=E[P^.e]; end;
    Q^.i:=P^.i; _g:=(L[Q^.g]-P^.l)*4; _n:=P^.n and (UInt64($F)<<_g);
    case Q^.g of
      1:begin Q^.l:=P^.l+4; Q^.e:=G[Q^.l]-2; P^.e-=1; Q^.n:=P^.n-_n+(_n<<16); if N[_n>>_g]>=(Q^.l div 4) then Q^.i+=1; end;
      2:begin Q^.l:=P^.l-4; Q^.e:=G[Q^.l]-1; P^.e-=2; Q^.n:=P^.n-_n+(_n>>16); if N[_n>>_g]<=(Q^.l div 4) then Q^.i+=1; end;
      3:begin Q^.l:=P^.l+1; Q^.e:=G[Q^.l]-8; P^.e-=4; Q^.n:=P^.n-_n+(_n<< 4); if I[_n>>_g]>=(Q^.l mod 4) then Q^.i+=1; end;
      4:begin Q^.l:=P^.l-1; Q^.e:=G[Q^.l]-4; P^.e-=8; Q^.n:=P^.n-_n+(_n>> 4); if I[_n>>_g]<=(Q^.l mod 4) then Q^.i+=1; end;
    end;
    P+=1;
  end;
end;
end.

```


```pascal

// Threaded use of 15 solver Unit. Nigel Galloway; February 1st., 2019.
program testFifteenSolver;
uses {$IFDEF UNIX}cthreads,{$ENDIF}sysutils,strutils,FifteenSolverT;
var Tz:array[0..5] of TThreadID; Tr:array[0..5] of TG; Tc:array[0..5] of shortint; Tw:array[0..5] of shortint;
const N:array[0..4 ] of string=('','d','u','r','l');
const G:array[0..15] of string=('41','841','841','81','421','8421','8421','821','421','8421','8421','821','42','842','842','82');
var ip:string; x,y:UInt64; P,Q:^TN; bPos,v,w,z,f:shortint; time1, time2: TDateTime; c:char;
function T(a:pointer):ptrint;
begin
  Tr[uint32(a)]:=solve15(x,bPos,Tw[uint32(a)],Tc[uint32(a)]);
  if Tr[uint32(a)].found then f:=uint32(a);
  T:=0;
end;
begin
  ReadLn(ip);
  bPos:=Npos('0',ip,1)-1; w:=0; z:=0; f:=-1;
  y:=(UInt64(Hex2Dec(ip[9..17]))<<32)>>32; x:=UInt64(Hex2Dec(ip[1..8]))<<32+y;
  time1:=Now;
  for w:=0 to $7f do begin
    for c in G[bpos] do begin v:=z mod 6; Tc[v]:=integer(c)-48; Tw[v]:=w;
      Tz[v]:=BeginThread(@T,pointer(v));
      z+=1; if z>5 then waitforthreadterminate(Tz[z mod 6],$7fff);
    end;
    if f>=0 then break;
  end;
  for bpos:=0 to 5 do if Tw[bpos]>=Tw[f] then killthread(Tz[bpos]) else waitforthreadterminate(Tz[bpos],$7fff);
  time2:=Now; WriteLn('Solution(s) found in ' +  FormatDateTime('hh.mm.ss.zzz', time2-time1) + ' seconds');
  for bpos:=0 to 5 do if Tr[bpos].found then begin
    P:=@Tr[bpos].path; repeat Q:=P; Write(N[Q^.g]); P+=1; until Q^.n=endpos; WriteLn();
  end;
end.

```


### The Task

### Output

```txt

fe169b4c0a73d852
Solution(s) found in 00.00.00.423 seconds
rrruldluuldrurdddluulurrrdlddruldluurddlulurruldrrdd

```


### Extra Credit

### Output

```txt

0c9dfbae37254861
Solution(s) found in 12.58.46.194 seconds
rrrdldlluurrddluurrdlurdddllluurrdrdlluluurrdddluurddlluururdrddlluurrullldrrrdd
drrrdlluurrdddlluururddlurddlulldrrulluurrdrdllluurrdrdllldruururdddlluluurrrddd

```



## Phix


```Phix
--
-- demo\rosetta\Solve15puzzle.exw
--
constant STM = 0    -- single-tile metrics.
constant MTM = 0    -- multi-tile metrics.
if STM and MTM then ?9/0 end if -- both prohibited
--  0       0   -- fastest, but non-optimal
--  1       0   -- optimal in STM
--  0       1   -- optimal in MTM (slowest by far)

--Note: The fast method uses an inadmissible heuristic - see "not STM" in iddfs().
--      It explores mtm-style using the higher stm heuristic and may therefore
--      fail badly in some cases.

constant SIZE = 4

constant goal = { 1, 2, 3, 4,
                  5, 6, 7, 8,
                  9,10,11,12,
                 13,14,15, 0}

--
-- multi-tile-metric walking distance heuristic lookup (mmwd).
--
### ====================================================

-- Uses patterns of counts of tiles in/from row/col, eg the solved state
--  (ie goal above) could be represented by the following:
--      {{4,0,0,0},
--       {0,4,0,0},
--       {0,0,4,0},
--       {0,0,0,3}}
--  ie row/col 1 contains 4 tiles from col/row 1, etc. In this case
--  both are identical, but you can count row/col or col/row, and then
--  add them together. There are up to 24964 possible patterns. The
--  blank space is not counted. Note that a vertical move cannot change
--  a vertical pattern, ditto horizontal, and basic symmetry means that
--  row/col and col/row patterns will match (at least, that is, if they
--  are calculated sympathetically), halving the setup cost.
-- The data is just the number of moves made before this pattern was
--  first encountered, in a breadth-first search, backwards from the
--  goal state, until all patterns have been enumerated.
-- (The same ideas/vars are now also used for stm metrics when MTM=0)
--
sequence wdkey              -- one such 4x4 pattern
constant mmwd = new_dict()  -- lookup table, data is walking distance.


--
-- We use two to-do lists: todo is the current list, and everything
-- of walkingdistance+1 ends up on tdnx. Once todo is exhausted, we
-- swap the dictionary-ids, so tdnx automatically becomes empty.
-- Key is an mmwd pattern as above, and data is {distance,space_idx}.
--
integer todo = new_dict()
integer tdnx = new_dict()

--

enum UP = 1, DOWN = -1

procedure explore(integer space_idx, walking_distance, direction)
--
--  Given a space index, explore all the possible moves in direction,
--  setting the distance and extending the tdnx table.
--
integer tile_idx = space_idx+direction
    for group=1 to SIZE do
        if wdkey[tile_idx][group] then
            -- ie: check row tile_idx for tiles belonging to rows 1..4
            -- Swap one of those tiles with the space
            wdkey[tile_idx][group] -= 1
            wdkey[space_idx][group] += 1

            if getd_index(wdkey,mmwd)=0 then
                -- save the walking distance value
                setd(wdkey,walking_distance+1,mmwd)
                -- and add to the todo next list:
                if getd_index(wdkey,tdnx)!=0 then ?9/0 end if
                setd(wdkey,{walking_distance+1,tile_idx},tdnx)
            end if

if MTM then
            if tile_idx>1 and tile_idx<SIZE then
                -- mtm: same direction means same distance:
                explore(tile_idx, walking_distance, direction)
            end if
end if

            -- Revert the swap so we can look at the next candidate.
            wdkey[tile_idx][group] += 1
            wdkey[space_idx][group] -= 1
        end if
    end for
end procedure

procedure generate_mmwd()
-- Perform a breadth-first search begining with the solved puzzle state
--  and exploring from there until no more new patterns emerge.
integer walking_distance = 0, space = 4

    wdkey = {{4,0,0,0}, -- \
             {0,4,0,0}, --  } 4 tiles in correct row positions
             {0,0,4,0}, -- /
             {0,0,0,3}} --    3 tiles in correct row position
    setd(wdkey,walking_distance,mmwd)
    while 1 do
        if space<4 then explore(space, walking_distance, UP)    end if
        if space>1 then explore(space, walking_distance, DOWN) end if
        if dict_size(todo)=0 then
            if dict_size(tdnx)=0 then exit end if
            {todo,tdnx} = {tdnx,todo}
        end if
        wdkey = getd_partial_key(0,todo)
        {walking_distance,space} = getd(wdkey,todo)
        deld(wdkey,todo)
    end while
end procedure

function walking_distance(sequence puzzle)
sequence rkey = repeat(repeat(0,SIZE),SIZE),
         ckey = repeat(repeat(0,SIZE),SIZE)
    integer k = 1
    for i=1 to SIZE do  -- rows
        for j=1 to SIZE do  -- columns
            integer tile = puzzle[k]
            if tile!=0 then
                integer row = floor((tile-1)/4)+1,
                        col = mod(tile-1,4)+1
                rkey[i][row] += 1
                ckey[j][col] += 1
            end if
            k += 1
        end for
    end for
    if getd_index(rkey,mmwd)=0
    or getd_index(ckey,mmwd)=0 then
        ?9/0 -- sanity check
    end if
    integer rwd = getd(rkey,mmwd),
            cwd = getd(ckey,mmwd)
    return rwd+cwd
end function

sequence puzzle
string res = ""
atom t0 = time(),
     t1 = time()+1
atom tries = 0

constant ok = {{0,1,1,1,0,1,1,1,0,1,1,1,0,1,1,1},   -- left
               {0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1},   -- up
               {1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0},   -- down
               {1,1,1,0,1,1,1,0,1,1,1,0,1,1,1,0}}   -- right

function iddfs(integer step, lim, space, prevmv)
    if time()>t1 then
        printf(1,"working... (depth=%d, tries=%d, time=%3ds)\r",{lim,tries,time()-t0})
        t1 = time()+1
    end if
    tries += 1
    integer d = iff(step==lim?0:walking_distance(puzzle))
    if d=0 then

        return (puzzle==goal)

    elsif step+d<=lim then

        for mv=1 to 4 do -- l/u/d/r
            if prevmv!=(5-mv) -- not l after r or vice versa, ditto u/d
            and ok[mv][space] then
                integer nspace = space+{-1,-4,+4,+1}[mv]
                integer tile = puzzle[nspace]
                if puzzle[space]!=0 then ?9/0 end if    -- sanity check
                puzzle[space] = tile
                puzzle[nspace] = 0
                if iddfs(step+iff(MTM or not STM?(prevmv!=mv):1),lim,nspace,mv) then
                    res &= "ludr"[mv]
                    return true
                end if
                puzzle[nspace] = tile
                puzzle[space] = 0
            end if
        end for
    end if
    return false
end function

function pack(string s)
integer n = length(s), n0 = n
    for i=1 to 4 do
        integer ch = "lrud"[i], k
        while 1 do
            k = match(repeat(ch,3),s)
            if k=0 then exit end if
            s[k+1..k+2] = "3"
            n -= 2
        end while
        while 1 do
            k = match(repeat(ch,2),s)
            if k=0 then exit end if
            s[k+1] = '2'
            n -= 1
        end while
    end for
    return {n,iff(MTM?sprintf("%d",n):sprintf("%d(%d)",{n,n0})),s}
end function

procedure apply_moves(string moves, integer space)
integer move, ch, nspace
    puzzle[space] = 0
    for i=1 to length(moves) do
        ch = moves[i]
        if ch>'3' then
            move = find(ch,"ulrd")
        end if
        -- (hint: "r" -> the 'r' does 1
        --        "r2" -> the 'r' does 1, the '2' does 1
        --        "r3" -> the 'r' does 1, the '3' does 2!)
        for j=1 to 1+(ch='3') do
            nspace = space+{-4,-1,+1,4}[move]
            puzzle[space] = puzzle[nspace]
            space = nspace
            puzzle[nspace] = 0
        end for
    end for
end procedure

function solvable(sequence board)
integer n = length(board)
sequence positions = repeat(0,n)
    -- prepare the mapping from each tile to its position
    board[find(0,board)] = n
    for i=1 to n do
        positions[board[i]] = i
    end for

    -- check whether this is an even or odd state
    integer row = floor((positions[16]-1)/4),
            col = (positions[16]-1)-row*4
    bool even_state = (positions[16]==16) or (mod(row,2)==mod(col,2))

    -- count the even cycles
    integer even_count = 0
    sequence visited = repeat(false,16)
    for i=1 to n do
        if not visited[i] then
            -- a new cycle starts at i. Count its length..
            integer cycle_length = 0,
                    next_tile = i
            while not visited[next_tile] do
                cycle_length +=1
                visited[next_tile] = true
                next_tile = positions[next_tile]
            end while
            even_count += (mod(cycle_length,2)==0)
        end if
    end for
    return even_state == (mod(even_count,2)==0)
end function

procedure main()

    puzzle = {15,14, 1, 6,
               9,11, 4,12,
               0,10, 7, 3,
              13, 8, 5, 2}

    if not solvable(puzzle) then
        ?puzzle
        printf(1,"puzzle is not solveable\n")
    else

        generate_mmwd()

        sequence original = puzzle
        integer space = find(0,puzzle)

        for lim=walking_distance(puzzle) to iff(MTM?43:80) do
            if iddfs(0, lim, space, '-') then exit end if
        end for

        {integer n, string ns, string ans} = pack(reverse(res))

        printf(1,"\n\noriginal:")
        ?original
        atom t = time()-t0
        printf(1,"\n%soptimal solution of %s moves found in %s: %s\n\nresult: ",
                 {iff(MTM?"mtm-":iff(STM?"stm-":"non-")),ns,elapsed(t),ans})
        puzzle = original
        apply_moves(ans,space)
        ?puzzle
    end if
end procedure
main()
```

### Output

```txt

original:{15,14,1,6,9,11,4,12,0,10,7,3,13,8,5,2}
non-optimal solution of 35(60) moves found in 2.42s: u2r2d3ru2ld2ru3ld3l2u3r2d2l2dru2ldru2rd3lulur3dl2ur2d2
stm-optimal solution of 38(52) moves found in 1 minute and 54s: r3uldlu2ldrurd3lu2lur3dld2ruldlu2rd2lulur2uldr2d2
mtm-optimal solution of 31(60) moves found in 2 hours, 38 minutes and 28s: u2r2d3ru2ld2ru3ld3l2u3r2d2l2dru3rd3l2u2r3dl3dru2r2d2

```



## Python

### Iterative Depth A*
From https://codegolf.stackexchange.com/questions/6884/solve-the-15-puzzle-the-tile-sliding-puzzle

Solution titled "PyPy, 195 moves, ~12 seconds computation"

Modified to run this task's 52 move problem.


```Python

import random


class IDAStar:
    def __init__(self, h, neighbours):
        """ Iterative-deepening A* search.

        h(n) is the heuristic that gives the cost between node n and the goal node. It must be admissable, meaning that h(n) MUST NEVER OVERSTIMATE the true cost. Underestimating is fine.

        neighbours(n) is an iterable giving a pair (cost, node, descr) for each node neighbouring n
        IN ASCENDING ORDER OF COST. descr is not used in the computation but can be used to
        efficiently store information about the path edges (e.g. up/left/right/down for grids).
        """

        self.h = h
        self.neighbours = neighbours
        self.FOUND = object()


    def solve(self, root, is_goal, max_cost=None):
        """ Returns the shortest path between the root and a given goal, as well as the total cost.
        If the cost exceeds a given max_cost, the function returns None. If you do not give a
        maximum cost the solver will never return for unsolvable instances."""

        self.is_goal = is_goal
        self.path = [root]
        self.is_in_path = {root}
        self.path_descrs = []
        self.nodes_evaluated = 0

        bound = self.h(root)

        while True:
            t = self._search(0, bound)
            if t is self.FOUND: return self.path, self.path_descrs, bound, self.nodes_evaluated
            if t is None: return None
            bound = t

    def _search(self, g, bound):
        self.nodes_evaluated += 1

        node = self.path[-1]
        f = g + self.h(node)
        if f > bound: return f
        if self.is_goal(node): return self.FOUND

        m = None # Lower bound on cost.
        for cost, n, descr in self.neighbours(node):
            if n in self.is_in_path: continue

            self.path.append(n)
            self.is_in_path.add(n)
            self.path_descrs.append(descr)
            t = self._search(g + cost, bound)

            if t == self.FOUND: return self.FOUND
            if m is None or (t is not None and t < m): m = t

            self.path.pop()
            self.path_descrs.pop()
            self.is_in_path.remove(n)

        return m


def slide_solved_state(n):
    return tuple(i % (n*n) for i in range(1, n*n+1))

def slide_randomize(p, neighbours):
    for _ in range(len(p) ** 2):
        _, p, _ = random.choice(list(neighbours(p)))
    return p

def slide_neighbours(n):
    movelist = []
    for gap in range(n*n):
        x, y = gap % n, gap // n
        moves = []
        if x > 0: moves.append(-1)    # Move the gap left.
        if x < n-1: moves.append(+1)  # Move the gap right.
        if y > 0: moves.append(-n)    # Move the gap up.
        if y < n-1: moves.append(+n)  # Move the gap down.
        movelist.append(moves)

    def neighbours(p):
        gap = p.index(0)
        l = list(p)

        for m in movelist[gap]:
            l[gap] = l[gap + m]
            l[gap + m] = 0
            yield (1, tuple(l), (l[gap], m))
            l[gap + m] = l[gap]
            l[gap] = 0

    return neighbours

def slide_print(p):
    n = int(round(len(p) ** 0.5))
    l = len(str(n*n))
    for i in range(0, len(p), n):
        print(" ".join("{:>{}}".format(x, l) for x in p[i:i+n]))

def encode_cfg(cfg, n):
    r = 0
    b = n.bit_length()
    for i in range(len(cfg)):
        r |= cfg[i] << (b*i)
    return r


def gen_wd_table(n):
    goal = [[0] * i + [n] + [0] * (n - 1 - i) for i in range(n)]
    goal[-1][-1] = n - 1
    goal = tuple(sum(goal, []))

    table = {}
    to_visit = [(goal, 0, n-1)]
    while to_visit:
        cfg, cost, e = to_visit.pop(0)
        enccfg = encode_cfg(cfg, n)
        if enccfg in table: continue
        table[enccfg] = cost

        for d in [-1, 1]:
            if 0 <= e + d < n:
                for c in range(n):
                    if cfg[n*(e+d) + c] > 0:
                        ncfg = list(cfg)
                        ncfg[n*(e+d) + c] -= 1
                        ncfg[n*e + c] += 1
                        to_visit.append((tuple(ncfg), cost + 1, e+d))

    return table

def slide_wd(n, goal):
    wd = gen_wd_table(n)
    goals = {i : goal.index(i) for i in goal}
    b = n.bit_length()

    def h(p):
        ht = 0 # Walking distance between rows.
        vt = 0 # Walking distance between columns.
        d = 0
        for i, c in enumerate(p):
            if c == 0: continue
            g = goals[c]
            xi, yi = i % n, i // n
            xg, yg = g % n, g // n
            ht += 1 << (b*(n*yi+yg))
            vt += 1 << (b*(n*xi+xg))

            if yg == yi:
                for k in range(i + 1, i - i%n + n): # Until end of row.
                    if p[k] and goals[p[k]] // n == yi and goals[p[k]] < g:
                        d += 2

            if xg == xi:
                for k in range(i + n, n * n, n): # Until end of column.
                    if p[k] and goals[p[k]] % n == xi and goals[p[k]] < g:
                        d += 2

        d += wd[ht] + wd[vt]

        return d
    return h




if __name__ == "__main__":
    solved_state = slide_solved_state(4)
    neighbours = slide_neighbours(4)
    is_goal = lambda p: p == solved_state

    tests = [
        (15, 14, 1, 6, 9, 11, 4, 12, 0, 10, 7, 3, 13, 8, 5,  2),
    ]

    slide_solver = IDAStar(slide_wd(4, solved_state), neighbours)

    for p in tests:
        path, moves, cost, num_eval = slide_solver.solve(p, is_goal, 80)
        slide_print(p)
        print(", ".join({-1: "Left", 1: "Right", -4: "Up", 4: "Down"}[move[1]] for move in moves))
        print(cost, num_eval)

```


Output - this solution of the problem for this task is the same as the second solution:

rrruldluuldrurdddluulurrrdlddruldluurddlulurruldrrdd

Profiling with standard Python 3.7 took 30 seconds.


```txt

15 14  1  6
 9 11  4 12
 0 10  7  3
13  8  5  2
Right, Right, Right, Up, Left, Down, Left, Up, Up, Left, Down, Right, Up, Right, Down, Down, Down, Left, Up, Up, Left, Up, Right, Right, Right, Down, Left, Down, Down, Right, Up, Left, Down, Left, Up, Up, Right, Down, Down, Left, Up, Left, Up, Right, Right, Up, Left, Down, Right, Right, Down, Down
52 872794

```


### A* with good heuristic
;File - astar.py

```Python

"""

Python example for this Rosetta Code task:

http://rosettacode.org/wiki/15_puzzle_solver

Using A* Algorithm from Wikkipedia:

https://en.wikipedia.org/wiki/A*_search_algorithm

Need to use heuristic that guarantees a shortest path
solution.

"""

import heapq
import copy

# Hopefully this is larger than any fscore or gscore

integer_infinity = 1000000000

class Position(object):
    """Position class represents one position of a 15 puzzle"""

    def __init__(self, tiles):
        """
        Takes a tuple of tuples representing the tiles on a 4x4 puzzle board
        numbering 1-15 with 0 representing an empty square. For example:

        (( 1,  2,  3,  4),
         ( 5,  6,  7,  8),
         ( 9, 10, 11, 12),
         (13, 14, 15,  0))

        Converts list of lists representation into tuple of tuples.
        """
        if type(tiles) == type(list()):
            t = tiles
            self.tiles = ((t[0][0], t[0][1], t[0][2], t[0][3]),
                          (t[1][0], t[1][1], t[1][2], t[1][3]),
                          (t[2][0], t[2][1], t[2][2], t[2][3]),
                          (t[3][0], t[3][1], t[3][2], t[3][3]))
        else:
            self.tiles = tiles

        # fields for A* algorithm

        self.fscore = integer_infinity
        self.gscore = integer_infinity

        self.cameFrom = None

    def copy_tiles(self):
        """ returns list of lists version """
        t = self.tiles

        return [[t[0][0], t[0][1], t[0][2], t[0][3]],
                [t[1][0], t[1][1], t[1][2], t[1][3]],
                [t[2][0], t[2][1], t[2][2], t[2][3]],
                [t[3][0], t[3][1], t[3][2], t[3][3]]]


    def neighbors(self):
        """
        returns a list of neighbors
        returns a list position objects with their
        directiontomoveto set to the direction that the
        empty square moved.

        tiles is 4x4 tuple of tuples with
        0,0 as top left.

        tiles[y][x]

        """

        # find 0 - blank square

        x0 = None
        y0 = None

        for i in range(4):
            for j in range(4):
                if self.tiles[i][j] == 0:
                    y0 = i
                    x0 = j

        if x0 == None or y0 == None:
            return []

        neighbor_list = []

        # move 0 to the right
        if x0 < 3:
            new_tiles = self.copy_tiles()
            temp = new_tiles[y0][x0+1]
            new_tiles[y0][x0+1] = 0
            new_tiles[y0][x0] = temp
            new_pos = new_position(new_tiles)
            neighbor_list.append(new_pos)
        # move 0 to the left
        if x0 > 0:
            new_tiles = self.copy_tiles()
            temp = new_tiles[y0][x0-1]
            new_tiles[y0][x0-1] = 0
            new_tiles[y0][x0] = temp
            new_pos = new_position(new_tiles)
            neighbor_list.append(new_pos)
        # move 0 up
        if y0 > 0:
            new_tiles = self.copy_tiles()
            temp = new_tiles[y0-1][x0]
            new_tiles[y0-1][x0] = 0
            new_tiles[y0][x0] = temp
            new_pos = new_position(new_tiles)
            neighbor_list.append(new_pos)
        # move 0 down
        if y0 < 3:
            new_tiles = self.copy_tiles()
            temp = new_tiles[y0+1][x0]
            new_tiles[y0+1][x0] = 0
            new_tiles[y0][x0] = temp
            new_pos = new_position(new_tiles)
            neighbor_list.append(new_pos)

        return neighbor_list

    def __repr__(self):
        # printable version of self

        return str(self.tiles[0])+'\n'+str(self.tiles[1])+'\n'+str(self.tiles[2])+'\n'+str(self.tiles[3])+'\n'

# takes tuple of tuples tiles as key, Position object for that tiles as value

all_positions = dict()

def new_position(tiles):
    """ returns a new position or looks up existing one """
    global all_positions
    if type(tiles) == type(list()):
        t = tiles
        tuptiles =   ((t[0][0], t[0][1], t[0][2], t[0][3]),
                      (t[1][0], t[1][1], t[1][2], t[1][3]),
                      (t[2][0], t[2][1], t[2][2], t[2][3]),
                      (t[3][0], t[3][1], t[3][2], t[3][3]))
    else:
        tuptiles = tiles

    if tuptiles in all_positions:
        return 	all_positions[tuptiles]
    else:
        new_pos = Position(tiles)
        all_positions[tuptiles] = new_pos
        return new_pos

def reconstruct_path(current):
    """
    Uses the cameFrom members to follow the chain of moves backwards
    and then reverses the list to get the path in the correct order.
    """
    total_path = [current]

    while current.cameFrom != None:
        current = current.cameFrom
        total_path.append(current)

    total_path.reverse()

    return total_path

class PriorityQueue(object):
    """
    Priority queue using heapq.
    elements of queue are (fscore,tiles) for each position.
    If element is removed from queue and fscore doesn't match
    then that element is discarded.
    """

    def __init__(self, object_list):
        """
        Save a list in a heapq.
        Assume that each object only appears once
        in the list.
        """
        self.queue_length = 0
        self.qheap = []
        for e in object_list:
            self.qheap.append((e.fscore,e.tiles))
            self.queue_length += 1
        heapq.heapify(self.qheap)

    def push(self, new_object):
        """ save object in heapq """
        heapq.heappush(self.qheap,(new_object.fscore,new_object.tiles))
        self.queue_length += 1

    def pop(self):
        """ remove object from heap and return """
        if self.queue_length < 1:
            return None
        fscore, tiles = heapq.heappop(self.qheap)
        self.queue_length -= 1
        global all_positions
        pos = all_positions[tiles]
        if pos.fscore == fscore:
            return pos
        else:
            return self.pop()

    def __repr__(self):
        # printable version of self
        strrep = ""
        for e in self.qheap:
          fscore, tiles = e
          strrep += str(fscore)+":"+str(tiles)+"\n"

        return strrep

conflict_table = None

def build_conflict_table():
    global conflict_table
    conflict_table = dict()

    # assumes goal tuple has up to
    # for the given pattern it the start position
    # how much to add for linear conflicts
    # 2 per conflict - max of 6

    # goal tuple is ('g0', 'g1', 'g2', 'g3')

    conflict_table[('g0', 'g1', 'g2', 'g3')] = 0
    conflict_table[('g0', 'g1', 'g2', 'x')] = 0
    conflict_table[('g0', 'g1', 'g3', 'g2')] = 2
    conflict_table[('g0', 'g1', 'g3', 'x')] = 0
    conflict_table[('g0', 'g1', 'x', 'g2')] = 0
    conflict_table[('g0', 'g1', 'x', 'g3')] = 0
    conflict_table[('g0', 'g1', 'x', 'x')] = 0
    conflict_table[('g0', 'g2', 'g1', 'g3')] = 2
    conflict_table[('g0', 'g2', 'g1', 'x')] = 2
    conflict_table[('g0', 'g2', 'g3', 'g1')] = 4
    conflict_table[('g0', 'g2', 'g3', 'x')] = 0
    conflict_table[('g0', 'g2', 'x', 'g1')] = 2
    conflict_table[('g0', 'g2', 'x', 'g3')] = 0
    conflict_table[('g0', 'g2', 'x', 'x')] = 0
    conflict_table[('g0', 'g3', 'g1', 'g2')] = 4
    conflict_table[('g0', 'g3', 'g1', 'x')] = 2
    conflict_table[('g0', 'g3', 'g2', 'g1')] = 4
    conflict_table[('g0', 'g3', 'g2', 'x')] = 2
    conflict_table[('g0', 'g3', 'x', 'g1')] = 2
    conflict_table[('g0', 'g3', 'x', 'g2')] = 2
    conflict_table[('g0', 'g3', 'x', 'x')] = 0
    conflict_table[('g0', 'x', 'g1', 'g2')] = 0
    conflict_table[('g0', 'x', 'g1', 'g3')] = 0
    conflict_table[('g0', 'x', 'g1', 'x')] = 0
    conflict_table[('g0', 'x', 'g2', 'g1')] = 2
    conflict_table[('g0', 'x', 'g2', 'g3')] = 0
    conflict_table[('g0', 'x', 'g2', 'x')] = 0
    conflict_table[('g0', 'x', 'g3', 'g1')] = 2
    conflict_table[('g0', 'x', 'g3', 'g2')] = 2
    conflict_table[('g0', 'x', 'g3', 'x')] = 0
    conflict_table[('g0', 'x', 'x', 'g1')] = 0
    conflict_table[('g0', 'x', 'x', 'g2')] = 0
    conflict_table[('g0', 'x', 'x', 'g3')] = 0
    conflict_table[('g1', 'g0', 'g2', 'g3')] = 2
    conflict_table[('g1', 'g0', 'g2', 'x')] = 2
    conflict_table[('g1', 'g0', 'g3', 'g2')] = 4
    conflict_table[('g1', 'g0', 'g3', 'x')] = 2
    conflict_table[('g1', 'g0', 'x', 'g2')] = 2
    conflict_table[('g1', 'g0', 'x', 'g3')] = 2
    conflict_table[('g1', 'g0', 'x', 'x')] = 2
    conflict_table[('g1', 'g2', 'g0', 'g3')] = 4
    conflict_table[('g1', 'g2', 'g0', 'x')] = 4
    conflict_table[('g1', 'g2', 'g3', 'g0')] = 6
    conflict_table[('g1', 'g2', 'g3', 'x')] = 0
    conflict_table[('g1', 'g2', 'x', 'g0')] = 4
    conflict_table[('g1', 'g2', 'x', 'g3')] = 0
    conflict_table[('g1', 'g2', 'x', 'x')] = 0
    conflict_table[('g1', 'g3', 'g0', 'g2')] = 4
    conflict_table[('g1', 'g3', 'g0', 'x')] = 4
    conflict_table[('g1', 'g3', 'g2', 'g0')] = 6
    conflict_table[('g1', 'g3', 'g2', 'x')] = 0
    conflict_table[('g1', 'g3', 'x', 'g0')] = 4
    conflict_table[('g1', 'g3', 'x', 'g2')] = 2
    conflict_table[('g1', 'g3', 'x', 'x')] = 0
    conflict_table[('g1', 'x', 'g0', 'g2')] = 2
    conflict_table[('g1', 'x', 'g0', 'g3')] = 2
    conflict_table[('g1', 'x', 'g0', 'x')] = 2
    conflict_table[('g1', 'x', 'g2', 'g0')] = 4
    conflict_table[('g1', 'x', 'g2', 'g3')] = 0
    conflict_table[('g1', 'x', 'g2', 'x')] = 0
    conflict_table[('g1', 'x', 'g3', 'g0')] = 4
    conflict_table[('g1', 'x', 'g3', 'g2')] = 2
    conflict_table[('g1', 'x', 'g3', 'x')] = 0
    conflict_table[('g1', 'x', 'x', 'g0')] = 2
    conflict_table[('g1', 'x', 'x', 'g2')] = 0
    conflict_table[('g1', 'x', 'x', 'g3')] = 0
    conflict_table[('g2', 'g0', 'g1', 'g3')] = 4
    conflict_table[('g2', 'g0', 'g1', 'x')] = 4
    conflict_table[('g2', 'g0', 'g3', 'g1')] = 4
    conflict_table[('g2', 'g0', 'g3', 'x')] = 2
    conflict_table[('g2', 'g0', 'x', 'g1')] = 4
    conflict_table[('g2', 'g0', 'x', 'g3')] = 2
    conflict_table[('g2', 'g0', 'x', 'x')] = 2
    conflict_table[('g2', 'g1', 'g0', 'g3')] = 4
    conflict_table[('g2', 'g1', 'g0', 'x')] = 4
    conflict_table[('g2', 'g1', 'g3', 'g0')] = 6
    conflict_table[('g2', 'g1', 'g3', 'x')] = 2
    conflict_table[('g2', 'g1', 'x', 'g0')] = 4
    conflict_table[('g2', 'g1', 'x', 'g3')] = 2
    conflict_table[('g2', 'g1', 'x', 'x')] = 2
    conflict_table[('g2', 'g3', 'g0', 'g1')] = 4
    conflict_table[('g2', 'g3', 'g0', 'x')] = 4
    conflict_table[('g2', 'g3', 'g1', 'g0')] = 6
    conflict_table[('g2', 'g3', 'g1', 'x')] = 4
    conflict_table[('g2', 'g3', 'x', 'g0')] = 4
    conflict_table[('g2', 'g3', 'x', 'g1')] = 4
    conflict_table[('g2', 'g3', 'x', 'x')] = 0
    conflict_table[('g2', 'x', 'g0', 'g1')] = 4
    conflict_table[('g2', 'x', 'g0', 'g3')] = 2
    conflict_table[('g2', 'x', 'g0', 'x')] = 2
    conflict_table[('g2', 'x', 'g1', 'g0')] = 4
    conflict_table[('g2', 'x', 'g1', 'g3')] = 2
    conflict_table[('g2', 'x', 'g1', 'x')] = 2
    conflict_table[('g2', 'x', 'g3', 'g0')] = 4
    conflict_table[('g2', 'x', 'g3', 'g1')] = 4
    conflict_table[('g2', 'x', 'g3', 'x')] = 0
    conflict_table[('g2', 'x', 'x', 'g0')] = 2
    conflict_table[('g2', 'x', 'x', 'g1')] = 2
    conflict_table[('g2', 'x', 'x', 'g3')] = 0
    conflict_table[('g3', 'g0', 'g1', 'g2')] = 6
    conflict_table[('g3', 'g0', 'g1', 'x')] = 4
    conflict_table[('g3', 'g0', 'g2', 'g1')] = 6
    conflict_table[('g3', 'g0', 'g2', 'x')] = 4
    conflict_table[('g3', 'g0', 'x', 'g1')] = 4
    conflict_table[('g3', 'g0', 'x', 'g2')] = 4
    conflict_table[('g3', 'g0', 'x', 'x')] = 2
    conflict_table[('g3', 'g1', 'g0', 'g2')] = 6
    conflict_table[('g3', 'g1', 'g0', 'x')] = 4
    conflict_table[('g3', 'g1', 'g2', 'g0')] = 6
    conflict_table[('g3', 'g1', 'g2', 'x')] = 4
    conflict_table[('g3', 'g1', 'x', 'g0')] = 4
    conflict_table[('g3', 'g1', 'x', 'g2')] = 4
    conflict_table[('g3', 'g1', 'x', 'x')] = 2
    conflict_table[('g3', 'g2', 'g0', 'g1')] = 6
    conflict_table[('g3', 'g2', 'g0', 'x')] = 4
    conflict_table[('g3', 'g2', 'g1', 'g0')] = 6
    conflict_table[('g3', 'g2', 'g1', 'x')] = 4
    conflict_table[('g3', 'g2', 'x', 'g0')] = 4
    conflict_table[('g3', 'g2', 'x', 'g1')] = 4
    conflict_table[('g3', 'g2', 'x', 'x')] = 2
    conflict_table[('g3', 'x', 'g0', 'g1')] = 4
    conflict_table[('g3', 'x', 'g0', 'g2')] = 4
    conflict_table[('g3', 'x', 'g0', 'x')] = 2
    conflict_table[('g3', 'x', 'g1', 'g0')] = 4
    conflict_table[('g3', 'x', 'g1', 'g2')] = 4
    conflict_table[('g3', 'x', 'g1', 'x')] = 2
    conflict_table[('g3', 'x', 'g2', 'g0')] = 4
    conflict_table[('g3', 'x', 'g2', 'g1')] = 4
    conflict_table[('g3', 'x', 'g2', 'x')] = 2
    conflict_table[('g3', 'x', 'x', 'g0')] = 2
    conflict_table[('g3', 'x', 'x', 'g1')] = 2
    conflict_table[('g3', 'x', 'x', 'g2')] = 2
    conflict_table[('x', 'g0', 'g1', 'g2')] = 0
    conflict_table[('x', 'g0', 'g1', 'g3')] = 0
    conflict_table[('x', 'g0', 'g1', 'x')] = 0
    conflict_table[('x', 'g0', 'g2', 'g1')] = 2
    conflict_table[('x', 'g0', 'g2', 'g3')] = 0
    conflict_table[('x', 'g0', 'g2', 'x')] = 0
    conflict_table[('x', 'g0', 'g3', 'g1')] = 2
    conflict_table[('x', 'g0', 'g3', 'g2')] = 2
    conflict_table[('x', 'g0', 'g3', 'x')] = 0
    conflict_table[('x', 'g0', 'x', 'g1')] = 0
    conflict_table[('x', 'g0', 'x', 'g2')] = 0
    conflict_table[('x', 'g0', 'x', 'g3')] = 0
    conflict_table[('x', 'g1', 'g0', 'g2')] = 2
    conflict_table[('x', 'g1', 'g0', 'g3')] = 2
    conflict_table[('x', 'g1', 'g0', 'x')] = 2
    conflict_table[('x', 'g1', 'g2', 'g0')] = 4
    conflict_table[('x', 'g1', 'g2', 'g3')] = 0
    conflict_table[('x', 'g1', 'g2', 'x')] = 0
    conflict_table[('x', 'g1', 'g3', 'g0')] = 4
    conflict_table[('x', 'g1', 'g3', 'g2')] = 2
    conflict_table[('x', 'g1', 'g3', 'x')] = 0
    conflict_table[('x', 'g1', 'x', 'g0')] = 2
    conflict_table[('x', 'g1', 'x', 'g2')] = 0
    conflict_table[('x', 'g1', 'x', 'g3')] = 0
    conflict_table[('x', 'g2', 'g0', 'g1')] = 4
    conflict_table[('x', 'g2', 'g0', 'g3')] = 2
    conflict_table[('x', 'g2', 'g0', 'x')] = 2
    conflict_table[('x', 'g2', 'g1', 'g0')] = 4
    conflict_table[('x', 'g2', 'g1', 'g3')] = 2
    conflict_table[('x', 'g2', 'g1', 'x')] = 2
    conflict_table[('x', 'g2', 'g3', 'g0')] = 4
    conflict_table[('x', 'g2', 'g3', 'g1')] = 4
    conflict_table[('x', 'g2', 'g3', 'x')] = 0
    conflict_table[('x', 'g2', 'x', 'g0')] = 2
    conflict_table[('x', 'g2', 'x', 'g1')] = 2
    conflict_table[('x', 'g2', 'x', 'g3')] = 0
    conflict_table[('x', 'g3', 'g0', 'g1')] = 4
    conflict_table[('x', 'g3', 'g0', 'g2')] = 4
    conflict_table[('x', 'g3', 'g0', 'x')] = 2
    conflict_table[('x', 'g3', 'g1', 'g0')] = 4
    conflict_table[('x', 'g3', 'g1', 'g2')] = 4
    conflict_table[('x', 'g3', 'g1', 'x')] = 2
    conflict_table[('x', 'g3', 'g2', 'g0')] = 4
    conflict_table[('x', 'g3', 'g2', 'g1')] = 4
    conflict_table[('x', 'g3', 'g2', 'x')] = 2
    conflict_table[('x', 'g3', 'x', 'g0')] = 2
    conflict_table[('x', 'g3', 'x', 'g1')] = 2
    conflict_table[('x', 'g3', 'x', 'g2')] = 2
    conflict_table[('x', 'x', 'g0', 'g1')] = 0
    conflict_table[('x', 'x', 'g0', 'g2')] = 0
    conflict_table[('x', 'x', 'g0', 'g3')] = 0
    conflict_table[('x', 'x', 'g1', 'g0')] = 2
    conflict_table[('x', 'x', 'g1', 'g2')] = 0
    conflict_table[('x', 'x', 'g1', 'g3')] = 0
    conflict_table[('x', 'x', 'g2', 'g0')] = 2
    conflict_table[('x', 'x', 'g2', 'g1')] = 2
    conflict_table[('x', 'x', 'g2', 'g3')] = 0
    conflict_table[('x', 'x', 'g3', 'g0')] = 2
    conflict_table[('x', 'x', 'g3', 'g1')] = 2
    conflict_table[('x', 'x', 'g3', 'g2')] = 2

def linear_conflicts(start_list,goal_list):
    """
    calculates number of moves to add to the estimate of
    the moves to get from start to goal based on the number
    of conflicts on a given row or column. start_list
    represents the current location and goal_list represnts
    the final goal.
    """

    # Find which of the tiles in start_list have their goals on this line
    # build a pattern to use in a lookup table of this form:
    # g0, g1, g3, g3 fill in x where there is no goal for this line

    # all 'x' until we file a tile whose goal is in this line

    goal_pattern = ['x', 'x', 'x', 'x']

    for g in range(4):
        for s in range(4):
            start_tile_num = start_list[s]
            if start_tile_num == goal_list[g] and start_tile_num != 0:
                goal_pattern[s] = 'g' + str(g) # i.e. g0

    global conflict_table

    tup_goal_pattern = tuple(goal_pattern)

    if tup_goal_pattern in conflict_table:
        return conflict_table[tuple(goal_pattern)]
    else:
        return 0

class lcmap(dict):
    """
    Lets you return 0 if you look for an object that
    is not in the dictionary.
    """
    def __missing__(self, key):
        return 0

def listconflicts(goal_list):
    """
    list all possible start lists that will have at least
    one linear conflict.

    Possible goal tile configurations

    g g g g
    g g g x
    g g x g
    g x g g
    x g g g
    g g x x
    g x g x
    g x x g
    x g g x
    x g x g
    x x g g

    """

    all_tiles = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]

    non_goal_tiles = []

    for t in all_tiles:
        if t not in goal_list:
            non_goal_tiles.append(t)

    combinations = lcmap()

    # g g g g

    for i in goal_list:
        tile_list2 = goal_list[:]
        tile_list2.remove(i)
        for j in tile_list2:
            tile_list3 = tile_list2[:]
            tile_list3.remove(j)
            for k in tile_list3:
                tile_list4 = tile_list3[:]
                tile_list4.remove(k)
                for l in tile_list4:
                    start_list = (i, j, k, l)
                    conflictadd = linear_conflicts(start_list,goal_list)
                    if conflictadd > 0:
                        combinations[start_list]=conflictadd

    # g g g x

    for i in goal_list:
        tile_list2 = goal_list[:]
        tile_list2.remove(i)
        for j in tile_list2:
            tile_list3 = tile_list2[:]
            tile_list3.remove(j)
            for k in tile_list3:
                for l in non_goal_tiles:
                    start_list = (i, j, k, l)
                    conflictadd = linear_conflicts(start_list,goal_list)
                    if conflictadd > 0:
                        combinations[start_list]=conflictadd

    # g g x g

    for i in goal_list:
        tile_list2 = goal_list[:]
        tile_list2.remove(i)
        for j in tile_list2:
            tile_list3 = tile_list2[:]
            tile_list3.remove(j)
            for k in non_goal_tiles:
                for l in tile_list3:
                    start_list = (i, j, k, l)
                    conflictadd = linear_conflicts(start_list,goal_list)
                    if conflictadd > 0:
                        combinations[start_list]=conflictadd
    # g x g g

    for i in goal_list:
        tile_list2 = goal_list[:]
        tile_list2.remove(i)
        for j in non_goal_tiles:
            for k in tile_list2:
                tile_list3 = tile_list2[:]
                tile_list3.remove(k)
                for l in tile_list3:
                    start_list = (i, j, k, l)
                    conflictadd = linear_conflicts(start_list,goal_list)
                    if conflictadd > 0:
                        combinations[start_list]=conflictadd

    # x g g g

    for i in non_goal_tiles:
        for j in goal_list:
            tile_list2 = goal_list[:]
            tile_list2.remove(j)
            for k in tile_list2:
                tile_list3 = tile_list2[:]
                tile_list3.remove(k)
                for l in tile_list3:
                    start_list = (i, j, k, l)
                    conflictadd = linear_conflicts(start_list,goal_list)
                    if conflictadd > 0:
                        combinations[start_list]=conflictadd

    # g g x x

    for i in goal_list:
        tile_list2 = goal_list[:]
        tile_list2.remove(i)
        for j in tile_list2:
            tile_list3 = tile_list2[:]
            tile_list3.remove(j)
            for k in non_goal_tiles:
                tile_list4 = non_goal_tiles[:]
                tile_list4.remove(k)
                for l in tile_list4:
                    start_list = (i, j, k, l)
                    conflictadd = linear_conflicts(start_list,goal_list)
                    if conflictadd > 0:
                        combinations[start_list]=conflictadd

    # g x g x

    for i in goal_list:
        tile_list2 = goal_list[:]
        tile_list2.remove(i)
        for j in non_goal_tiles:
            tile_list3 = non_goal_tiles[:]
            tile_list3.remove(j)
            for k in tile_list2:
                for l in tile_list3:
                    start_list = (i, j, k, l)
                    conflictadd = linear_conflicts(start_list,goal_list)
                    if conflictadd > 0:
                        combinations[start_list]=conflictadd

    # g x x g

    for i in goal_list:
        tile_list2 = goal_list[:]
        tile_list2.remove(i)
        for j in non_goal_tiles:
            tile_list3 = non_goal_tiles[:]
            tile_list3.remove(j)
            for k in tile_list2:
                for l in tile_list3:
                    start_list = (i, j, k, l)
                    conflictadd = linear_conflicts(start_list,goal_list)
                    if conflictadd > 0:
                        combinations[start_list]=conflictadd

    # x g g x

    for i in non_goal_tiles:
        tile_list2 = non_goal_tiles[:]
        tile_list2.remove(i)
        for j in goal_list:
            tile_list3 = goal_list[:]
            tile_list3.remove(j)
            for k in tile_list3:
                for l in tile_list2:
                    start_list = (i, j, k, l)
                    conflictadd = linear_conflicts(start_list,goal_list)
                    if conflictadd > 0:
                        combinations[start_list]=conflictadd

    # x g x g

    for i in non_goal_tiles:
        tile_list2 = non_goal_tiles[:]
        tile_list2.remove(i)
        for j in goal_list:
            tile_list3 = goal_list[:]
            tile_list3.remove(j)
            for k in tile_list3:
                for l in tile_list2:
                    start_list = (i, j, k, l)
                    conflictadd = linear_conflicts(start_list,goal_list)
                    if conflictadd > 0:
                        combinations[start_list]=conflictadd

    # x x g g

    for i in non_goal_tiles:
        tile_list2 = non_goal_tiles[:]
        tile_list2.remove(i)
        for j in tile_list2:
            for k in goal_list:
                tile_list3 = goal_list[:]
                tile_list3.remove(k)
                for l in tile_list3:
                    start_list = (i, j, k, l)
                    conflictadd = linear_conflicts(start_list,goal_list)
                    if conflictadd > 0:
                        combinations[start_list]=conflictadd

    return combinations


class HeuristicObj(object):
    """ Object used to preprocess goal position for heuristic function """

    def __init__(self, goal):
        """
        Preprocess goal position to setup internal data structures
        that can be used to speed up heuristic.
        """

        build_conflict_table()

        self.goal_map = []
        for i in range(16):
            self.goal_map.append(i)

        self.goal_lists = goal.tiles

        # preprocess for manhattan distance

        for row in range(4):
            for col in range(4):
                self.goal_map[goal.tiles[row][col]] = (row, col)

        # make access faster by changing to a tuple

        self.goal_map = tuple(self.goal_map)

        # preprocess for linear conflicts

        self.row_conflicts = []
        for row in range(4):
            t = goal.tiles[row]
            conf_dict = listconflicts([t[0],t[1],t[2],t[3]])
            self.row_conflicts.append(conf_dict)

        self.col_conflicts = []
        for col in range(4):
            col_list =[]
            for row in range(4):
                col_list.append(goal.tiles[row][col])
            conf_dict = listconflicts(col_list)
            self.col_conflicts.append(conf_dict)

    def heuristic(self, start):
        """

        Estimates the number of moves from start to goal.
        The goal was preprocessed in __init__.

        """

        distance = 0

        # local variables for instance variables

        t = start.tiles
        g = self.goal_map
        rc = self.row_conflicts
        cc = self.col_conflicts

        # calculate manhattan distance

        for row in range(4):
            for col in range(4):
                start_tilenum = t[row][col]
                if start_tilenum != 0:
                    (grow, gcol) = g[start_tilenum]
                    distance += abs(row - grow) + abs(col - gcol)

        # add linear conflicts

        for row in range(4):
            curr_row = t[row]
            distance += rc[row][curr_row]

        for col in range(4):
            col_tuple = (t[0][col], t[1][col], t[2][col], t[3][col])
            distance += cc[col][col_tuple]

        return distance

# global variable for heuristic object

hob = None

def a_star(start_tiles, goal_tiles):
    """ Based on https://en.wikipedia.org/wiki/A*_search_algorithm """

    start = new_position(start_tiles)
    goal = new_position(goal_tiles)

    # Process goal position for use in heuristic

    global hob
    hob = HeuristicObj(goal)

    # The set of currently discovered nodes that are not evaluated yet.
    # Initially, only the start node is known.
    # For the first node, the fscore is completely heuristic.

    start.fscore = hob.heuristic(start)
    openSet = PriorityQueue([start])

    # The cost of going from start to start is zero.

    start.gscore = 0

    num_popped = 0

    while openSet.queue_length > 0:
        current = openSet.pop()
        if current == None: # tried to pop but only found old fscore values
            break
        num_popped += 1
        if num_popped % 100000 == 0:
            print(str(num_popped)+" positions examined")

        if current == goal:
            return reconstruct_path(current)

        for neighbor in current.neighbors():

            # The distance from start to a neighbor
            # All nodes are 1 move from their neighbors

            tentative_gScore = current.gscore + 1

            # update gscore and fscore if this is shorter path
            # to the neighbor node

            if tentative_gScore < neighbor.gscore:
                neighbor.cameFrom = current
                neighbor.gscore = tentative_gScore
                neighbor.fscore = neighbor.gscore + hob.heuristic(neighbor)
                openSet.push(neighbor) # add to open set every time


def find_zero(tiles):
    """ file the 0 tile """
    for row in range(4):
        for col in range(4):
            if tiles[row][col] == 0:
                return (row, col)

def path_as_0_moves(path):
    """
    Takes the path which is a list of Position
    objects and outputs it as a string of rlud
    directions to match output desired by
    Rosetta Code task.
    """
    strpath = ""
    if len(path) < 1:
        return ""
    prev_pos = path[0]
    p_row, p_col = find_zero(prev_pos.tiles)
    for i in range(1,len(path)):
        curr_pos = path[i]
        c_row, c_col = find_zero(curr_pos.tiles)
        if c_row > p_row:
            strpath += 'd'
        elif c_row < p_row:
            strpath += 'u'
        elif c_col > p_col:
            strpath += 'r'
        elif c_col < p_col:
            strpath += 'l'
        # reset for next loop
        prev_pos = curr_pos
        p_row = c_row
        p_col = c_col
    return strpath


```


;File - testone.py


```Python

"""

Runs one test of the solver passing a
start and goal position.

"""

from astar import *
import time

# Rosetta Code start position


start_tiles =    [[ 15, 14,  1,  6],
                  [ 9, 11,  4, 12],
                  [ 0, 10,  7,  3],
                  [13,  8,  5,  2]]

goal_tiles =        [[ 1,  2,  3,  4],
                     [ 5,  6,  7,  8],
                     [ 9, 10, 11, 12],
                     [13, 14, 15,  0]]


before = time.perf_counter()

result = a_star(start_tiles,goal_tiles)

after = time.perf_counter()

print(" ")
print("Path length = "+str(len(result) - 1))
print(" ")
print("Path using rlud:")
print(" ")
print(path_as_0_moves(result))
print(" ")
print("Run time in seconds: "+str(after - before))

```

Output:


```txt

C:\bobby\15puzzlesolver\my15puzzlesolver>testone.py
100000 positions examined
200000 positions examined
300000 positions examined
400000 positions examined
500000 positions examined
600000 positions examined
700000 positions examined
800000 positions examined

Path length = 52

Path using rlud:

rrruldluuldrurdddluulurrrdlddruldluurddlulurruldrrdd

Run time in seconds: 56.601139201

```



## Racket


```Racket

#lang racket

;;; Solution to the 15-puzzle game, based on the A* algorithm described
;;; at https://de.wikipedia.org/wiki/A*-Algorithmus

;;; Inspired by the Python solution of the rosetta code:
;;; http://rosettacode.org/wiki/15_puzzle_solver#Python

(require racket/set)
(require data/heap)

;; ----------------------------------------------------------------------------
;; Datatypes

;; A posn is a pair struct containing two integer for the row/col indices.
(struct posn (row col) #:transparent)

;; A state contains a vector and a posn describing the position of the empty slot.
(struct state (matrix empty-slot) #:transparent)

(define directions '(up down left right))

;; A node contains a state, a reference to the previous node, a g value (actual
;; costs until this node, and a f value (g value + heuristics).
(struct node (state prev cost f-value) #:transparent)

;; ----------------------------------------------------------------------------
;; Constants

(define side-size 4)

(define initial-state
  (state
    #(
      15  14  1  6
      9  11  4 12
      0  10  7  3
      13  8  5  2)
    (posn 2 0)))


(define goal-state
  (state
    #( 1  2  3  4
      5  6  7 8
      9 10 11 12
      13 14 15 0)
    (posn 3 3)))

;; ----------------------------------------------------------------------------
;; Functions

;; Matrices are simple vectors, abstracted by following functions.
(define (matrix-ref matrix row col)
  (vector-ref matrix (+ (* row side-size) col)))

(define (matrix-set! matrix row col val)
  (vector-set! matrix
               (+ (* row side-size) col)
               val))

(define (target-state? st)
  (equal? st goal-state))

;; Traverse all nodes until the initial state and generate a return a list
;; of symbols describing the path.
(define (reconstruct-movements leaf-node)
  ;; compute a pair describing the movement.
  (define (posn-diff p0 p1)
    (posn (- (posn-row p1) (posn-row p0))
          (- (posn-col p1) (posn-col p0))))

  ;; describe a single movement with a symbol (r, l, u d).
  (define (find-out-movement prev-st st)
    (let ([prev-empty-slot (state-empty-slot prev-st)]
          [this-empty-slot (state-empty-slot st)])
      (match (posn-diff prev-empty-slot this-empty-slot)
             [(posn  1  0) 'u]
             [(posn -1  0) 'd]
             [(posn  0  1) 'l]
             [(posn  0 -1) 'r]
             [#f 'invalid])))

  (define (iter n path)
    (if (or (not n) (not (node-prev n)))
        path
      (iter (node-prev n)
            (cons (find-out-movement (node-state n)
                                     (node-state (node-prev n)))
                  path))))
  (iter leaf-node '()))

(define (print-path path)
  (for ([dir (in-list (car path))])
       (display dir))
  (newline))

;; Return #t if direction is allowed for the given empty slot position.
(define (movement-valid? direction empty-slot)
  (match direction
         ['up (< (posn-row empty-slot) (- side-size 1))]
         ['down (> (posn-row empty-slot) 0)]
         ['left (< (posn-col empty-slot) (- side-size 1))]
         ['right (> (posn-col empty-slot) 0)]))

;; assumes move direction is valid (see movement-valid?).
;; Return a new state in the given direction.
(define (move st direction)
  (define m (vector-copy (state-matrix st)))
  (define empty-slot (state-empty-slot st))
  (define r (posn-row empty-slot))
  (define c (posn-col empty-slot))
  (define new-empty-slot
    (match direction
           ['up  (begin (matrix-set! m r c (matrix-ref m (+ r 1) c))
                        (matrix-set! m (+ r 1) c 0)
                        (posn (+ r 1) c))]
           ['down (begin (matrix-set! m r c (matrix-ref m (- r 1) c))
                         (matrix-set! m (- r 1) c 0)
                         (posn (- r 1) c))]
           ['left (begin (matrix-set! m r c (matrix-ref m r (+ c 1)))
                         (matrix-set! m r (+ c 1) 0)
                         (posn r (+ c 1)))]
           ['right (begin (matrix-set! m r c (matrix-ref m r (- c 1)))
                          (matrix-set! m r (- c 1) 0)
                          (posn r (- c 1)))]))
  (state m new-empty-slot))

(define (l1-distance posn0 posn1)
  (+ (abs (- (posn-row posn0) (posn-row posn1)))
     (abs (- (posn-col posn0) (posn-col posn1)))))

;; compute the L1 distance from the current position and the goal position for
;; the given val
(define (element-cost val current-posn)
  (if (= val 0)
      (l1-distance current-posn (posn 3 3))
    (let ([target-row (quotient (- val 1) side-size)]
          [target-col (remainder (- val 1) side-size)])
      (l1-distance current-posn (posn target-row target-col)))))

;; compute the l1 distance between this state and the goal-state
(define (state-l1-distance-to-goal st)
  (define m (state-matrix st))
  (for*/fold
    ([sum 0])
    ([i (in-range side-size)]
     [j (in-range side-size)])
    (let ([val (matrix-ref m i j)])
      (if (not (= val 0))
          (+ sum (element-cost val (posn i j)))
        sum))))

;; the heuristic used is the l1 distance to the goal-state + the number of
;; linear conflicts found
(define (state-heuristics st)
  (+ (state-l1-distance-to-goal st)
     (linear-conflicts st goal-state)))

;; given a list, return the number of values out of order (used for computing
;; linear conflicts).
(define (out-of-order-values lst)
  (define (iter val-lst sum)
    (if (empty? val-lst)
        sum
      (let* ([val (car val-lst)]
             [rst (cdr val-lst)]
             [following-smaller-values
               (filter (lambda (val2) (> val2 val))
                       rst)])
        (iter rst (+ sum (length following-smaller-values))))))
  (* 2 (iter lst 0)))

;; Number of conflicts in the given row. A conflict happens, when two elements
;; are already in the correct row, but in the wrong order.
;; For each conflicted pair add 2 to the value, but a maximum of 6.
(define (row-conflicts row st0 st1)
  (define m0 (state-matrix st0))
  (define m1 (state-matrix st1))

  (define values-in-correct-row
    (for/fold
      ([lst '()])
      ([col0 (in-range side-size)])
      (let* ([val0 (matrix-ref m0 row col0)]
             [in-goal-row?
               (for/first ([col1 (in-range side-size)]
                           #:when (= val0 (matrix-ref m1 row col1)))
                          #t)])
        (if in-goal-row? (cons val0 lst) lst))))

  (min 6 (out-of-order-values
           ; 0 doesn't lead to a linear conflict
           (filter positive? values-in-correct-row))))

;; Number of conflicts in the given row. A conflict happens, when two elements
;; are already in the correct column but in the wrong order.
;; For each conflicted pair add 2 to the value, but a maximum of 6, so that
;; the heuristic doesn't overestimate the actual costs.
(define (col-conflicts col st0 st1)
  (define m0 (state-matrix st0))
  (define m1 (state-matrix st1))

  (define values-in-correct-col
    (for/fold
      ([lst '()])
      ([row0 (in-range side-size)])
      (let* ([val0 (matrix-ref m0 row0 col)]
             [in-goal-col?
               (for/first ([row1 (in-range side-size)]
                           #:when (= val0 (matrix-ref m1 row1 col)))
                          #t)])
        (if in-goal-col? (cons val0 lst) lst))))
  (min 6 (out-of-order-values
           ; 0 doesn't lead to a linear conflict
           (filter positive? values-in-correct-col))))

(define (all-row-conflicts st0 st1)
  (for/fold ([sum 0])
            ([row (in-range side-size)])
            (+ (row-conflicts row st0 st1) sum)))

(define (all-col-conflicts st0 st1)
  (for/fold ([sum 0])
            ([col (in-range side-size)])
            (+ (col-conflicts col st0 st1) sum)))

(define (linear-conflicts st0 st1)
  (+ (all-row-conflicts st0 st1) (all-col-conflicts st0 st1)))

;; Return a list of pairs containing the possible next node and the movement
;; direction needed.
(define (next-state-dir-pairs current-node)
  (define st (node-state current-node))
  (define empty-slot (state-empty-slot st))
  (define valid-movements
    (filter (lambda (dir) (movement-valid? dir empty-slot))
            directions))
  (map (lambda (dir)
         (cons (move st dir) dir))
       valid-movements))

;; Helper function to pretty-print a state
(define (display-state st)
  (define m (state-matrix st))
  (begin
    (for ([i (in-range 0 side-size 1)])
         (newline)
         (for ([j (in-range 0 side-size 1)])
              (printf "~a\t" (matrix-ref m i j))))
    (newline)))


(define (A* initial-st)
  (define (compare-nodes n0 n1)
    (<= (node-f-value n0) (node-f-value n1)))
  (define open-lst (make-heap compare-nodes))
  (define initial-st-cost (state-heuristics initial-st))
  (heap-add! open-lst (node initial-st #f 0 (state-heuristics initial-st)))
  (define closed-set (mutable-set))

  (define (pick-next-node!)
    (define next-node (heap-min open-lst))
    (heap-remove-min! open-lst)
    next-node)

  (define (sort-lst lst)
    (sort lst
          (lambda (n0 n1)
            (< (node-f-value n0) (node-f-value n1)))))

  (define (expand-node n)

    (define n-cost (node-cost n))

    (define (iter lst)
      (if (empty? lst)
          '()
        (let* ([succ (car lst)]
               [succ-st (car succ)]
               [succ-dir (cdr succ)]
               [succ-cost (+ 1 n-cost)])
          (if (set-member? closed-set succ-st)
              (iter (cdr lst))

            (begin    (heap-add! open-lst
                                 (node succ-st
                                       n
                                       succ-cost
                                       (+ (state-heuristics succ-st)
                                          succ-cost)))
                   (iter (cdr lst)))))))

    (let ([successors (next-state-dir-pairs n)])
      (iter successors)))

  (define counter 0)
  (define (loop)
    (define current-node (pick-next-node!))
    (define current-state (node-state current-node))
    (set! counter (+ counter 1))
    (if (= (remainder counter 100000) 0)
        (printf "~a ~a ~a\n" counter
                (heap-count open-lst)
                (node-cost current-node))
      (void))

    (cond [(target-state? current-state)
           (let ([path (reconstruct-movements current-node)])
             (cons path (length path)))]
          [else
            (begin (set-add! closed-set (node-state current-node))
                   (expand-node current-node)
                   (if (= (heap-count open-lst) 0)
                       #f
                     (loop)))]))
  (loop))

(module+ main
         (print-path (A* initial-state)))

```



## Rust

{{trans|C++}}

```Rust
const NR: [i32; 16] = [3, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3];
const NC: [i32; 16] = [3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2];

const I: u8 = 1;
const G: u8 = 8;
const E: u8 = 2;
const L: u8 = 4;

struct FifteenSolver {
    n: usize,
    limit: usize,
    n0: [i32; 85],
    n3: [u8; 85],
    n4: [usize; 85],
    n2: [u64; 85],
}

impl FifteenSolver {
    fn f_y(&mut self) -> bool {
        if self.n2[self.n] == 0x123456789abcdef0 {
            return true;
        }
        if self.n4[self.n] <= self.limit {
            return self.f_n();
        }
        false
    }

    fn f_z(&mut self, w: u8) -> bool {
        if w & I != 0 {
            self.f_i();
            if self.f_y() {
                return true;
            }
            self.n -= 1;
        }
        if w & G != 0 {
            self.f_g();
            if self.f_y() {
                return true;
            }
            self.n -= 1;
        }
        if w & E != 0 {
            self.f_e();
            if self.f_y() {
                return true;
            }
            self.n -= 1;
        }
        if w & L != 0 {
            self.f_l();
            if self.f_y() {
                return true;
            }
            self.n -= 1;
        }
        false
    }

    fn f_n(&mut self) -> bool {
        self.f_z(match self.n0[self.n] {
            0 => match self.n3[self.n] {
                b'l' => I,
                b'u' => E,
                _ => I + E,
            },
            3 => match self.n3[self.n] {
                b'r' => I,
                b'u' => L,
                _ => I + L,
            },
            1 | 2 => match self.n3[self.n] {
                b'l' => I + L,
                b'r' => I + E,
                b'u' => E + L,
                _ => L + E + I,
            },
            12 => match self.n3[self.n] {
                b'l' => G,
                b'd' => E,
                _ => E + G,
            },
            15 => match self.n3[self.n] {
                b'r' => G,
                b'd' => L,
                _ => G + L,
            },
            13 | 14 => match self.n3[self.n] {
                b'l' => G + L,
                b'r' => E + G,
                b'd' => E + L,
                _ => G + E + L,
            },
            4 | 8 => match self.n3[self.n] {
                b'l' => I + G,
                b'u' => G + E,
                b'd' => I + E,
                _ => I + G + E,
            },
            7 | 11 => match self.n3[self.n] {
                b'd' => I + L,
                b'u' => G + L,
                b'r' => I + G,
                _ => I + G + L,
            },
            _ => match self.n3[self.n] {
                b'd' => I + E + L,
                b'l' => I + G + L,
                b'r' => I + G + E,
                b'u' => G + E + L,
                _ => I + G + E + L,
            },
        })
    }

    fn f_i(&mut self) {
        let g = (11 - self.n0[self.n]) * 4;
        let a = self.n2[self.n] & (15u64 << g);
        self.n0[self.n + 1] = self.n0[self.n] + 4;
        self.n2[self.n + 1] = self.n2[self.n] - a + (a << 16);
        self.n3[self.n + 1] = b'd';
        self.n4[self.n + 1] = self.n4[self.n];
        let cond = NR[(a >> g) as usize] <= self.n0[self.n] / 4;
        if !cond {
            self.n4[self.n + 1] += 1;;
        }
        self.n += 1;
    }

    fn f_g(&mut self) {
        let g = (19 - self.n0[self.n]) * 4;
        let a = self.n2[self.n] & (15u64 << g);
        self.n0[self.n + 1] = self.n0[self.n] - 4;
        self.n2[self.n + 1] = self.n2[self.n] - a + (a >> 16);
        self.n3[self.n + 1] = b'u';
        self.n4[self.n + 1] = self.n4[self.n];
        let cond = NR[(a >> g) as usize] >= self.n0[self.n] / 4;
        if !cond {
            self.n4[self.n + 1] += 1;
        }
        self.n += 1;
    }

    fn f_e(&mut self) {
        let g = (14 - self.n0[self.n]) * 4;
        let a = self.n2[self.n] & (15u64 << g);
        self.n0[self.n + 1] = self.n0[self.n] + 1;
        self.n2[self.n + 1] = self.n2[self.n] - a + (a << 4);
        self.n3[self.n + 1] = b'r';
        self.n4[self.n + 1] = self.n4[self.n];
        let cond = NC[(a >> g) as usize] <= self.n0[self.n] % 4;
        if !cond {
            self.n4[self.n + 1] += 1;
        }
        self.n += 1;
    }

    fn f_l(&mut self) {
        let g = (16 - self.n0[self.n]) * 4;
        let a = self.n2[self.n] & (15u64 << g);
        self.n0[self.n + 1] = self.n0[self.n] - 1;
        self.n2[self.n + 1] = self.n2[self.n] - a + (a >> 4);
        self.n3[self.n + 1] = b'l';
        self.n4[self.n + 1] = self.n4[self.n];
        let cond = NC[(a >> g) as usize] >= self.n0[self.n] % 4;
        if !cond {
            self.n4[self.n + 1] += 1;
        }
        self.n += 1;
    }

    fn new(n: i32, g: u64) -> Self {
        let mut solver = FifteenSolver {
            n: 0,
            limit: 0,
            n0: [0; 85],
            n3: [0; 85],
            n4: [0; 85],
            n2: [0; 85],
        };
        solver.n0[0] = n;
        solver.n2[0] = g;
        solver
    }

    fn solve(&mut self) {
        while !self.f_n() {
            self.n = 0;
            self.limit += 1;
        }
        println!(
            "Solution found in {} moves: {}",
            self.n,
            std::str::from_utf8(&self.n3[1..=self.n]).unwrap()
        );
    }
}

fn main() {
    FifteenSolver::new(8, 0xfe169b4c0a73d852).solve();
}
```


### Output

```txt
Solution found in 52 moves: rrrulddluuuldrurdddrullulurrrddldluurddlulurruldrdrd
```

