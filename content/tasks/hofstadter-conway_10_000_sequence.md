+++
title = "Hofstadter-Conway $10,000 sequence"
description = ""
date = 2019-03-08T19:13:41Z
aliases = []
[extra]
id = 6245
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "360_assembly",
  "ada",
  "algol_68",
  "autohotkey",
  "awk",
  "bbc_basic",
  "bracmat",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "eiffel",
  "erlang",
  "euler_math_toolbox",
  "fortran",
  "freebasic",
  "futurebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "nim",
  "objeck",
  "oforth",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "purebasic",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "scala",
  "scheme",
  "sidef",
  "swift",
  "tcl",
  "vba",
  "x86_assembly",
  "zkl",
  "zx_spectrum_basic",
]
+++

{{task}}[[Category:Memoization]]
The definition of the sequence is colloquially described as:
*   Starting with the list [1,1],
*   Take the last number in the list so far: 1, I'll call it x.
*   Count forward x places from the beginning of the list to find the first number to add (1)
*   Count backward x places from the end of the list to find the second number to add (1)
*   Add the two indexed numbers from the list and the result becomes the next number in the list (1+1)
*   This would then produce [1,1,2] where 2 is the third element of the sequence.


Note that indexing for the description above starts from alternately the left and right ends of the list and starts from an index of ''one''.

A less wordy description of the sequence is:
    a(1)=a(2)=1
    a(n)=a(a(n-1))+a(n-a(n-1))

The sequence begins:
    1, 1, 2, 2, 3, 4, 4, 4, 5, ...

Interesting features of the sequence are that:
*   a(n)/n   tends to   0.5   as   n   grows towards infinity.
*   a(n)/n   where   n   is a power of   2   is   0.5
*   For   n>4   the maximal value of   a(n)/n   between successive powers of 2 decreases.

[[File:Hofstadter conway 10K.gif|center|a(n) / n   for   n   in   1..256]]


The sequence is so named because [[wp:John Horton Conway|John Conway]] [http://www.nytimes.com/1988/08/30/science/intellectual-duel-brash-challenge-swift-response.html offered a prize] of $10,000 to the first person who could
find the first position,   p   in the sequence where
    │a(n)/n│ < 0.55  for all  n > p
It was later found that [[wp:Douglas Hofstadter|Hofstadter]] had also done prior work on the sequence.

The 'prize' was won quite quickly by [http://www.research.avayalabs.com/gcm/usa/en-us/people/all/mallows.htm Dr. Colin L. Mallows] who proved the properties of the sequence and allowed him to find the value of   n   (which is much smaller than the 3,173,375,556 quoted in the NYT article).


## Task

#   Create a routine to generate members of the Hofstadter-Conway $10,000 sequence.
#   Use it to show the maxima of   a(n)/n   between successive powers of two up to   2**20
#   As a stretch goal:   compute the value of   n   that would have won the prize and confirm it is true for   n   up to 2**20




;Also see:
*   [http://www.jstor.org/stable/2324028 Conways Challenge Sequence], Mallows' own account.
*   [http://mathworld.wolfram.com/Hofstadter-Conway10000-DollarSequence.html Mathworld Article].





## 360 Assembly

For maximum compatibility, this program uses only the basic instruction set (S/360)
with 2 ASSIST macros (XDECO,XPRNT).

The program addresses the problem for l=2**12 (4K). For l=2**20 (1M) you must
allocate dynamic storage instead using static storage.

```360asm
*        Hofstadter-Conway $10,000 sequence    07/05/2016
HOFSTADT START
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save registers
         ST     R13,4(R15)         link backward SA
         ST     R15,8(R13)         link forward SA
         LR     R13,R15            establish addressability
         USING  HOFSTADT,R13       set base register
         LA     R4,2               pow2=2
         LA     R8,4               p2=2**pow2
         MVC    A+0,=F'1'          a(1)=1
         MVC    A+4,=F'1'          a(2)=1
         LA     R6,3               n=3
LOOPN    C      R6,UPRDIM          do n=3 to uprdim
         BH     ELOOPN
         LR     R1,R6              n
         SLA    R1,2
         L      R5,A-8(R1)         a(n-1)
         LR     R1,R5
         SLA    R1,2
         L      R2,A-4(R1)         a(a(n-1))
         LR     R1,R6              n
         SR     R1,R5              n-a(n-1)
         SLA    R1,2
         L      R3,A-4(R1)         a(n-a(n-1)
         AR     R2,R3              a(a(n-1))+a(n-a(n-1))
         LR     R1,R6              n
         SLA    R1,2
         ST     R2,A-4(R1)         a(n)=a(a(n-1))+a(n-a(n-1))
         LR     R1,R6              n
         SLA    R1,2
         L      R2,A-4(R1)         a(n)
         MH     R2,=H'10000'       fixed point 4dec
         SRDA   R2,32
         DR     R2,R6              /n
         LR     R7,R3              r=a(n)/n
         C      R7,=F'5500'        if r>=0.55
         BL     EIF1
         LR     R9,R6              mallows=n
EIF1     C      R7,PEAK            if r>peak
         BNH    EIF2
         ST     R7,PEAK            peak=r
         ST     R6,PEAKPOS         peakpos=n
EIF2     CR     R6,R8              if n=p2
         BNE    EIF3
         LR     R1,R4              pow2
         BCTR   R1,0               pow2-1
         XDECO  R1,XDEC            edit pow2-1
         MVC    PG1+18(2),XDEC+10
         XDECO  R4,XDEC            edit pow2
         MVC    PG1+27(2),XDEC+10
         L      R1,PEAK            peak
         XDECO  R1,XDEC            edit peak
         MVC    PG1+35(4),XDEC+8
         L      R1,PEAKPOS         peakpos
         XDECO  R1,XDEC            edit peakpos
         MVC    PG1+45(5),XDEC+7
         XPRNT  PG1,80             print buffer
         LA     R4,1(R4)           pow2=pow2+1
         SLA    R8,1               p2=2**pow2
         MVC    PEAK,=F'5000'      peak=0.5
EIF3     LA     R6,1(R6)           n=n+1
         B      LOOPN
ELOOPN   L      R1,L               l
         XDECO  R1,XDEC            edit l
         MVC    PG2+6(2),XDEC+10
         XDECO  R9,XDEC            edit mallows
         MVC    PG2+29(5),XDEC+7
         XPRNT  PG2,80             print buffer
RETURN   L      R13,4(0,R13)       restore savearea pointer
         LM     R14,R12,12(R13)    restore registers
         XR     R15,R15            return code = 0
         BR     R14                return to caller
         LTORG
L        DC     F'12'
UPRDIM   DC     F'4096'            2^L
PEAK     DC     F'5000'            0.5 fixed point 4dec
PEAKPOS  DC     F'0'
XDEC     DS     CL12
PG1      DC CL80'maximum between 2^xx and 2^xx is 0.xxxx at n=xxxxx'
PG2      DC CL80'for l=xx : mallows number is xxxxx'
A        DS     4096F              array a(uprdim)
         REGEQU
         END    HOFSTADT
```

```txt

maximum between 2^ 1 and 2^ 2 is 0.6666 at n=    3
maximum between 2^ 2 and 2^ 3 is 0.6666 at n=    6
maximum between 2^ 3 and 2^ 4 is 0.6363 at n=   11
maximum between 2^ 4 and 2^ 5 is 0.6086 at n=   23
maximum between 2^ 5 and 2^ 6 is 0.5909 at n=   44
maximum between 2^ 6 and 2^ 7 is 0.5760 at n=   92
maximum between 2^ 7 and 2^ 8 is 0.5674 at n=  178
maximum between 2^ 8 and 2^ 9 is 0.5594 at n=  370
maximum between 2^ 9 and 2^10 is 0.5549 at n=  719
maximum between 2^10 and 2^11 is 0.5501 at n= 1487
maximum between 2^11 and 2^12 is 0.5474 at n= 2897
for l=12 : mallows number is  1489

```



## Ada


```Ada
-- Ada95 version
-- Allocation of arrays on the heap

with Ada.Text_IO; use Ada.Text_IO;
with Unchecked_Deallocation;

procedure Conway is

   package Real_io is new Float_IO (Float);

   Maxrange : constant := 2 ** 20;

   type Sequence is array (Positive range 1 .. Maxrange) of Positive;
   type Sequence_Ptr is access all Sequence;
   procedure Free is new Unchecked_Deallocation (Sequence, Sequence_Ptr);

   S : Sequence_Ptr := new Sequence;

   type Ratio_Array is array (Positive range 1 .. Maxrange) of Float;
   type Ratio_Ptr is access all Ratio_Array;
   procedure Free is new Unchecked_Deallocation (Ratio_Array, Ratio_Ptr);

   Ratio : Ratio_Ptr := new Ratio_Array;

   Mallows : Positive;
   M       : Natural := 0;
begin
   S (1) := 1;
   S (2) := 1;
   for K in 3 .. Maxrange loop
      S (K) := S (S (K - 1)) + S (K - S (K - 1));
   end loop;

   for k in 1 .. Maxrange loop
      Ratio (k) := Float (S (k)) / Float (k);
   end loop;

   for N in 1 .. 19 loop
      declare
         Max   : Float := 0.0;
         Where : Positive;
      begin
         for K in 2 ** N .. 2 ** (N + 1) loop
            if Max < Ratio (K) then
               Max   := Ratio (K);
               Where := K;
            end if;
         end loop;
         if (M = 0 and Max < 0.55) then
            M := N - 1;
         end if;
         Put
           ("Maximun of a(n)/n between 2^" &
            Integer'Image (N) &
            " and 2^" &
            Integer'Image (N + 1) &
            " was ");
         Real_io.Put (Max, Fore => 1, Aft => 8, Exp => 0);
         Put_Line (" at" & Integer'Image (Where));
      end;
   end loop;
   --  Calculate Mallows number
   for I in reverse 2 ** M .. 2 ** (M + 1) loop
      if (Ratio (I) > 0.55) then
         Mallows := I;
         exit;
      end if;
   end loop;
   Put_Line ("Mallows number" & Integer'Image (Mallows));
   Free (S);
   Free (Ratio);
end Conway;


```

Sample output:

```txt

Maximun of a(n)/n between 2^ 1 and 2^ 2 was 0.66666669 at 3
Maximun of a(n)/n between 2^ 2 and 2^ 3 was 0.66666669 at 6
Maximun of a(n)/n between 2^ 3 and 2^ 4 was 0.63636363 at 11
Maximun of a(n)/n between 2^ 4 and 2^ 5 was 0.60869563 at 23
Maximun of a(n)/n between 2^ 5 and 2^ 6 was 0.59090906 at 44
Maximun of a(n)/n between 2^ 6 and 2^ 7 was 0.57608694 at 92
Maximun of a(n)/n between 2^ 7 and 2^ 8 was 0.56741571 at 178
Maximun of a(n)/n between 2^ 8 and 2^ 9 was 0.55945945 at 370
Maximun of a(n)/n between 2^ 9 and 2^ 10 was 0.55493742 at 719
Maximun of a(n)/n between 2^ 10 and 2^ 11 was 0.55010086 at 1487
Maximun of a(n)/n between 2^ 11 and 2^ 12 was 0.54746288 at 2897
Maximun of a(n)/n between 2^ 12 and 2^ 13 was 0.54414475 at 5969
Maximun of a(n)/n between 2^ 13 and 2^ 14 was 0.54244268 at 11651
Maximun of a(n)/n between 2^ 14 and 2^ 15 was 0.54007107 at 22223
Maximun of a(n)/n between 2^ 15 and 2^ 16 was 0.53878403 at 45083
Maximun of a(n)/n between 2^ 16 and 2^ 17 was 0.53704363 at 89516
Maximun of a(n)/n between 2^ 17 and 2^ 18 was 0.53602004 at 181385
Maximun of a(n)/n between 2^ 18 and 2^ 19 was 0.53464544 at 353683
Maximun of a(n)/n between 2^ 19 and 2^ 20 was 0.53377920 at 722589
Mallows number 1489

```



## ALGOL 68


```algol68
PROC do sqnc = (INT max)INT:
BEGIN
    [max]INT a list;
    INT k1 := 2,
        lg2 := 1,
        v := a list[1] := a list[2] := 1; # Concurrent declaration and assignment in declarations are allowed #

    INT nmax;
    LONG REAL amax := 0.0;

    INT mallows number;

    FOR n FROM 3 TO max DO
      v := a list[n] := a list[v] + a list[n-v];

      ( amax < v/n | amax := v/n; nmax := n );  # When given a Boolean as the 1st expression, ( | ) is the short form of IF...THEN...FI #

      IF v/n >= 0.55 THEN                       # This is the equivalent full form of the above construct #
          mallows number := n
      FI;

      IF ABS(BIN k1 AND BIN n) = 0 THEN
      # 'BIN' converts an INT type to a BITS type; In this context, 'ABS' reverses that operation #
        printf(($"Maximum between 2^"g(0)" and 2^"g(0)" is about "g(-10,8)" at "g(0)l$, lg2,lg2+1, amax, nmax));
        amax := 0;
        lg2 PLUSAB 1   # 'PLUSAB' (plus-and-becomes) has the short form +:= #
      FI;
      k1 := n
    OD;
    mallows number   # the result of the last expression evaluated is returned as the result of the PROC #
END;

INT mallows number = do sqnc(2**20); # This definition of 'mallows number' does not clash with the variable
                                       of the same name inside PROC do sqnc - they are in different scopes#

printf(($"You too might have won $1000 with an answer of n = "g(0)$, mallows number))
```

Output:

```txt

Maximum between 2^1 and 2^2 is about 0.66666667 at 3
Maximum between 2^2 and 2^3 is about 0.66666667 at 6
Maximum between 2^3 and 2^4 is about 0.63636364 at 11
Maximum between 2^4 and 2^5 is about 0.60869565 at 23
Maximum between 2^5 and 2^6 is about 0.59090909 at 44
Maximum between 2^6 and 2^7 is about 0.57608696 at 92
Maximum between 2^7 and 2^8 is about 0.56741573 at 178
Maximum between 2^8 and 2^9 is about 0.55945946 at 370
Maximum between 2^9 and 2^10 is about 0.55493741 at 719
Maximum between 2^10 and 2^11 is about 0.55010087 at 1487
Maximum between 2^11 and 2^12 is about 0.54746289 at 2897
Maximum between 2^12 and 2^13 is about 0.54414475 at 5969
Maximum between 2^13 and 2^14 is about 0.54244271 at 11651
Maximum between 2^14 and 2^15 is about 0.54007110 at 22223
Maximum between 2^15 and 2^16 is about 0.53878402 at 45083
Maximum between 2^16 and 2^17 is about 0.53704366 at 89516
Maximum between 2^17 and 2^18 is about 0.53602007 at 181385
Maximum between 2^18 and 2^19 is about 0.53464543 at 353683
Maximum between 2^19 and 2^20 is about 0.53377923 at 722589
You too might have won $1000 with an answer of n = 1489

```



## AutoHotkey


```autohotkey
Progress, b2 w150 zh0 fs9, CreateLists ...
CreateLists(2 ** (Max:=20))

Progress,, Find Maxima ...
Loop, % Max - 1
    msg .= "Maximum between 2^" A_Index " and 2^" A_Index + 1
        .  " is " GetMax(2 ** A_Index, 2 ** (A_Index + 1), n)
        .  " for n = " n "`n"

Progress,, Find Mallows Number ...
Loop, % 2 ** Max
    If (n_%A_Index% > 0.55)
        MallowsNumber := A_Index
msg .= "Mallows Number = " MallowsNumber

Progress, Off
MsgBox, %msg%

;---------------------------------------------------------------------------
GetMax(a, b, ByRef Item) { ; return max value of a(n)/n between a and b
;---------------------------------------------------------------------------
    Loop {
        IfGreater, a, %b%, Break
        If (Maximum < n_%a%)
            Maximum := n_%a%, Item := a
        a++
    }
    Return, Maximum
}

;---------------------------------------------------------------------------
CreateLists(Lenght) { ; Hofstadter-Conway sequences (using lookups)
;---------------------------------------------------------------------------
    ; create the sequence  a_%A_Index%  [ a(n)   ]
    ;   and  the sequence  n_%A_Index%  [ a(n)/n ]
    ;-----------------------------------------------------------------------
    global
    a_1 := a_2 := n_1 := 1, n_2 := 1 / 2
    Loop, %Lenght% {
        IfLess, A_Index, 3, Continue
        n1 := A_Index - 1
        an1 := a_%n1%
        nan1 := A_Index - an1
        a_%A_Index% := a_%an1% + a_%nan1%
        n_%A_Index% := a_%A_Index% / A_Index
    }
}
```

Message box shows:

```txt
Maximum between 2^1 and 2^2 is 0.666667 for n = 3
Maximum between 2^2 and 2^3 is 0.666667 for n = 6
Maximum between 2^3 and 2^4 is 0.636364 for n = 11
Maximum between 2^4 and 2^5 is 0.608696 for n = 23
Maximum between 2^5 and 2^6 is 0.590909 for n = 44
Maximum between 2^6 and 2^7 is 0.576087 for n = 92
Maximum between 2^7 and 2^8 is 0.567416 for n = 178
Maximum between 2^8 and 2^9 is 0.559459 for n = 370
Maximum between 2^9 and 2^10 is 0.554937 for n = 719
Maximum between 2^10 and 2^11 is 0.550101 for n = 1487
Maximum between 2^11 and 2^12 is 0.547463 for n = 2897
Maximum between 2^12 and 2^13 is 0.544145 for n = 5969
Maximum between 2^13 and 2^14 is 0.542443 for n = 11651
Maximum between 2^14 and 2^15 is 0.540071 for n = 22223
Maximum between 2^15 and 2^16 is 0.538784 for n = 45083
Maximum between 2^16 and 2^17 is 0.537044 for n = 89516
Maximum between 2^17 and 2^18 is 0.536020 for n = 181385
Maximum between 2^18 and 2^19 is 0.534645 for n = 353683
Maximum between 2^19 and 2^20 is 0.533779 for n = 722589
Mallows Number = 1489
```



## AWK


Iterative approach:

```AWK
#!/usr/bin/awk -f
BEGIN {
  NN = 20;
  iterativeHCsequence(2^NN+1,Q);
  for (K=1; K<NN; K++) {
    m = 0;
    for (n=2^K+1; n<=2^(K+1); n++) {
        v = Q[n]/n;
	if (m < v) {nn=n; m = v};
    }
    printf "Maximum a(n)/n between 2^%i and 2^%i is %f at n=%i\n",K,K+1,m,nn;
  }
  print "number of Q(n)<Q(n+1) for n<=100000 : " NN;
}

function iterativeHCsequence(N,Q) {
  Q[1] = 1;
  Q[2] = 1;
  for (n=3; n<=N; n++) {
    Q[n] = Q[Q[n-1]]+Q[n-Q[n-1]];
  }
}
```


Recursive variant:


```AWK
#!/usr/bin/awk -f
BEGIN {
  Q[1] = 1;
  Q[2] = 1;
  S[1] = 1;
  S[2] = 1;

  NN = 20;
  for (K=1; K<NN; K++) {
    m = 0;
    for (n=2^K+1; n<=2^(K+1); n++) {
        v = HCsequence(n,Q,S)/n;
	if (m < v) {nn=n; m = v};
    }
    printf "Maximum between 2^%i and 2^%i is %f at n=%i\n",K,K+1,m,nn;
  }
}

function HCsequence(n,Q,S) {
  ## recursive definition
  if (S[n]==0) {

  k = n-1;
  if (S[k]==0) {
     HCsequence(k,Q,S);
  }
  k = Q[n-1];
  if (S[k]==0) {
     HCsequence(k,Q,S);
  }
  k = n-Q[n-1];
  if (S[k]==0) {
     HCsequence(k,Q,S);
  }

  }
  Q[n] = Q[Q[n-1]]+Q[n-Q[n-1]];
  S[n] = 1;
  return (Q[n]);
}
```


Output:

```txt
Maximum between 2^1 and 2^2 is 0.666667 at n=3
Maximum between 2^2 and 2^3 is 0.666667 at n=6
Maximum between 2^3 and 2^4 is 0.636364 at n=11
Maximum between 2^4 and 2^5 is 0.608696 at n=23
Maximum between 2^5 and 2^6 is 0.590909 at n=44
Maximum between 2^6 and 2^7 is 0.576087 at n=92
Maximum between 2^7 and 2^8 is 0.567416 at n=178
Maximum between 2^8 and 2^9 is 0.559459 at n=370
Maximum between 2^9 and 2^10 is 0.554937 at n=719
Maximum between 2^10 and 2^11 is 0.550101 at n=1487
Maximum between 2^11 and 2^12 is 0.547463 at n=2897
Maximum between 2^12 and 2^13 is 0.544145 at n=5969
Maximum between 2^13 and 2^14 is 0.542443 at n=11651
Maximum between 2^14 and 2^15 is 0.540071 at n=22223
Maximum between 2^15 and 2^16 is 0.538784 at n=45083
Maximum between 2^16 and 2^17 is 0.537044 at n=89516
Maximum between 2^17 and 2^18 is 0.536020 at n=181385
Maximum between 2^18 and 2^19 is 0.534645 at n=353683
Maximum between 2^19 and 2^20 is 0.533779 at n=722589

```



## BBC BASIC


```bbcbasic
HIMEM=LOMEM+1E7 : REM Reserve enough memory for a 4 MB array, plus other code
DIM a%(2^20)
a%(1)=1
a%(2)=1
pow2%=2
p2%=2^pow2%
peak=0.5
peakpos%=0
FOR n%=3 TO 2^20
   a%(n%)=a%(a%(n%-1))+a%(n%-a%(n%-1))
   r=a%(n%)/n%
   IF r>=0.55 THEN Mallows%=n%
   IF r>peak THEN peak=r:peakpos%=n%
   IF n%=p2% THEN
      PRINT "Maximum between 2^";pow2%-1;" and 2^";pow2%;" is ";peak;" at n=";peakpos%
      pow2%+=1
      p2%=2^pow2%
      peak=0.5
   ENDIF
NEXT n%
PRINT "Mallows number is ";Mallows%
```


Results

```txt

Maximum between 2^1 and 2^2 is 0.666666667 at n=3
Maximum between 2^2 and 2^3 is 0.666666667 at n=6
Maximum between 2^3 and 2^4 is 0.636363637 at n=11
Maximum between 2^4 and 2^5 is 0.608695652 at n=23
Maximum between 2^5 and 2^6 is 0.590909091 at n=44
Maximum between 2^6 and 2^7 is 0.576086957 at n=92
Maximum between 2^7 and 2^8 is 0.56741573 at n=178
Maximum between 2^8 and 2^9 is 0.55945946 at n=370
Maximum between 2^9 and 2^10 is 0.554937413 at n=719
Maximum between 2^10 and 2^11 is 0.550100874 at n=1487
Maximum between 2^11 and 2^12 is 0.547462893 at n=2897
Maximum between 2^12 and 2^13 is 0.544144748 at n=5969
Maximum between 2^13 and 2^14 is 0.542442709 at n=11651
Maximum between 2^14 and 2^15 is 0.540071098 at n=22223
Maximum between 2^15 and 2^16 is 0.538784021 at n=45083
Maximum between 2^16 and 2^17 is 0.537043657 at n=89516
Maximum between 2^17 and 2^18 is 0.536020068 at n=181385
Maximum between 2^18 and 2^19 is 0.534645431 at n=353683
Maximum between 2^19 and 2^20 is 0.53377923 at n=722589
Mallows number is 1489
```



## Bracmat


```bracmat
( ( a
  =
    .   !arg:(1|2)&1
      | (as..find)$!arg:(?.?arg)&!arg
      |     (as..insert)
          $ ( !arg
            . a$(a$(!arg+-1))+a$(!arg+-1*a$(!arg+-1)):?arg
            )
        & !arg
  )
& new$hash:?as
& 0:?n:?maxan/n
& 1:?pow
&   whl
  ' ( 1+!n:?n
    & !pow:~>20
    & ( 2^!pow:~!n
      |     out
          $ ( str
            $ ( "Between 2^"
                !pow+-1
                " and 2^"
                !pow
                " the maximum value of a(n)/n is reached for n = "
                !maxn
                " with the value "
                !maxan/n
              )
            )
        & 0:?maxan/n
        & 1+!pow:?pow
      )
    & a$!n*!n^-1:?an/n
    & (   !an/n:>!maxan/n:?maxan/n
        & !n:?maxn
      |
      )
    & ( !an/n:~<11/20:?Man/n&!n:?Mallows
      |
      )
    )
&   out
  $ ( str
    $ ( "Mallows number is "
        !Mallows
        ", where a("
        !Mallows
        ")/"
        !Mallows
        " == "
        !Man/n
        ", which is greater than 0.55 by "
        !Man/n+-11/20
      )
    )
)
```

Output:

```txt
Between 2^0 and 2^1 the maximum value of a(n)/n is reached for n = 1 with the value 1
Between 2^1 and 2^2 the maximum value of a(n)/n is reached for n = 3 with the value 2/3
Between 2^2 and 2^3 the maximum value of a(n)/n is reached for n = 6 with the value 2/3
Between 2^3 and 2^4 the maximum value of a(n)/n is reached for n = 11 with the value 7/11
Between 2^4 and 2^5 the maximum value of a(n)/n is reached for n = 23 with the value 14/23
Between 2^5 and 2^6 the maximum value of a(n)/n is reached for n = 44 with the value 13/22
Between 2^6 and 2^7 the maximum value of a(n)/n is reached for n = 92 with the value 53/92
Between 2^7 and 2^8 the maximum value of a(n)/n is reached for n = 178 with the value 101/178
Between 2^8 and 2^9 the maximum value of a(n)/n is reached for n = 370 with the value 207/370
Between 2^9 and 2^10 the maximum value of a(n)/n is reached for n = 719 with the value 399/719
Between 2^10 and 2^11 the maximum value of a(n)/n is reached for n = 1487 with the value 818/1487
Between 2^11 and 2^12 the maximum value of a(n)/n is reached for n = 2897 with the value 1586/2897
Between 2^12 and 2^13 the maximum value of a(n)/n is reached for n = 5969 with the value 3248/5969
Between 2^13 and 2^14 the maximum value of a(n)/n is reached for n = 11651 with the value 6320/11651
Between 2^14 and 2^15 the maximum value of a(n)/n is reached for n = 22223 with the value 12002/22223
Between 2^15 and 2^16 the maximum value of a(n)/n is reached for n = 45083 with the value 24290/45083
Between 2^16 and 2^17 the maximum value of a(n)/n is reached for n = 89516 with the value 24037/44758
Between 2^17 and 2^18 the maximum value of a(n)/n is reached for n = 181385 with the value 97226/181385
Between 2^18 and 2^19 the maximum value of a(n)/n is reached for n = 353683 with the value 189095/353683
Between 2^19 and 2^20 the maximum value of a(n)/n is reached for n = 722589 with the value 385703/722589
Mallows number is 1489, where a(1489)/1489 == 819/1489, which is greater than 0.55 by 1/29780
```



## C


```c
#include <stdio.h>
#include <stdlib.h>

int a_list[1<<20 + 1];

int doSqnc( int m)
{
    int max_df = 0;
    int p2_max = 2;
    int v, n;
    int k1 = 2;
    int lg2 = 1;
    double amax = 0;
    a_list[0] = -50000;
    a_list[1] = a_list[2] = 1;
    v = a_list[2];

    for (n=3; n <= m;  n++) {
        v = a_list[n] = a_list[v] + a_list[n-v];
        if ( amax < v*1.0/n) amax = v*1.0/n;
        if ( 0 == (k1&n)) {
            printf("Maximum between 2^%d and 2^%d was %f\n", lg2,lg2+1, amax);
            amax = 0;
            lg2++;
        }
        k1 = n;
    }
    return 1;
}
```

Results

```txt
Maximum between 2^1 and 2^2 was 0.666667
Maximum between 2^2 and 2^3 was 0.666667
Maximum between 2^3 and 2^4 was 0.636364
Maximum between 2^4 and 2^5 was 0.608696
....
Maximum between 2^18 and 2^19 was 0.534645
Maximum between 2^19 and 2^20 was 0.533779
```



## C++


```cpp

#include <deque>
#include <iostream>

int hcseq(int n)
{
  static std::deque<int> seq(2, 1);
  while (seq.size() < n)
  {
    int x = seq.back();
    seq.push_back(seq[x-1] + seq[seq.size()-x]);
  }
  return seq[n-1];
}

int main()
{
  int pow2 = 1;
  for (int i = 0; i < 20; ++i)
  {
    int pow2next = 2*pow2;
    double max = 0;
    for (int n = pow2; n < pow2next; ++n)
    {
      double anon = hcseq(n)/double(n);
      if (anon > max)
        max = anon;
    }
    std::cout << "maximum of a(n)/n between 2^" << i
              << " (" << pow2 << ") and 2^" << i+1
              << " (" << pow2next << ") is " << max << "\n";
    pow2 = pow2next;
  }
}

```

Output:

```txt

maximum of a(n)/n between 2^0 (1) and 2^1 (2) is 1
maximum of a(n)/n between 2^1 (2) and 2^2 (4) is 0.666667
maximum of a(n)/n between 2^2 (4) and 2^3 (8) is 0.666667
maximum of a(n)/n between 2^3 (8) and 2^4 (16) is 0.636364
maximum of a(n)/n between 2^4 (16) and 2^5 (32) is 0.608696
maximum of a(n)/n between 2^5 (32) and 2^6 (64) is 0.590909
maximum of a(n)/n between 2^6 (64) and 2^7 (128) is 0.576087
maximum of a(n)/n between 2^7 (128) and 2^8 (256) is 0.567416
maximum of a(n)/n between 2^8 (256) and 2^9 (512) is 0.559459
maximum of a(n)/n between 2^9 (512) and 2^10 (1024) is 0.554937
maximum of a(n)/n between 2^10 (1024) and 2^11 (2048) is 0.550101
maximum of a(n)/n between 2^11 (2048) and 2^12 (4096) is 0.547463
maximum of a(n)/n between 2^12 (4096) and 2^13 (8192) is 0.544145
maximum of a(n)/n between 2^13 (8192) and 2^14 (16384) is 0.542443
maximum of a(n)/n between 2^14 (16384) and 2^15 (32768) is 0.540071
maximum of a(n)/n between 2^15 (32768) and 2^16 (65536) is 0.538784
maximum of a(n)/n between 2^16 (65536) and 2^17 (131072) is 0.537044
maximum of a(n)/n between 2^17 (131072) and 2^18 (262144) is 0.53602
maximum of a(n)/n between 2^18 (262144) and 2^19 (524288) is 0.534645
maximum of a(n)/n between 2^19 (524288) and 2^20 (1048576) is 0.533779

```

## C#
```c#

using System;
using System.Linq;

namespace HofstadterConway
{
    class Program
    {
        static int[] GenHofstadterConway(int max)
        {
            int[] result = new int[max];
            result[0]=result[1]=1;
            for (int ix = 2; ix < max; ix++)
                result[ix] = result[result[ix - 1] - 1] + result[ix - result[ix - 1]];
            return result;
        }

        static void Main(string[] args)
        {
            double[] adiv = new double[1 << 20];
            {
                int[] a = GenHofstadterConway(1 << 20);
                for (int i = 0; i < 1 << 20; i++)
                    adiv[i] = a[i] / (double)(i + 1);
            }
            for (int p = 2; p <= 20; p++)
            {
                var max = Enumerable.Range(
                     (1 << (p - 1)) - 1,
                     (1 << p) - (1 << (p - 1))
                     )
                     .Select(ix => new { I = ix + 1, A = adiv[ix] })
                     .OrderByDescending(x => x.A)
                     .First();
                Console.WriteLine("Maximum from 2^{0} to 2^{1} is {2} at {3}",
                    p - 1, p, max.A, max.I);
            }
            Console.WriteLine("The winning number is {0}.",
                Enumerable.Range(0, 1 << 20)
                    .Last(i => (adiv[i] > 0.55)) + 1
                );
        }
    }
}

```

Output:-

```txt
Maximum from 2^1 to 2^2 is 0.666666666666667 at 3
Maximum from 2^2 to 2^3 is 0.666666666666667 at 6
Maximum from 2^3 to 2^4 is 0.636363636363636 at 11
Maximum from 2^4 to 2^5 is 0.608695652173913 at 23
Maximum from 2^5 to 2^6 is 0.590909090909091 at 44
Maximum from 2^6 to 2^7 is 0.576086956521739 at 92
Maximum from 2^7 to 2^8 is 0.567415730337079 at 178
Maximum from 2^8 to 2^9 is 0.559459459459459 at 370
Maximum from 2^9 to 2^10 is 0.554937413073713 at 719
Maximum from 2^10 to 2^11 is 0.550100874243443 at 1487
Maximum from 2^11 to 2^12 is 0.547462892647566 at 2897
Maximum from 2^12 to 2^13 is 0.544144747863964 at 5969
Maximum from 2^13 to 2^14 is 0.542442708780362 at 11651
Maximum from 2^14 to 2^15 is 0.540071097511587 at 22223
Maximum from 2^15 to 2^16 is 0.538784020584256 at 45083
Maximum from 2^16 to 2^17 is 0.537043656999866 at 89516
Maximum from 2^17 to 2^18 is 0.536020067811561 at 181385
Maximum from 2^18 to 2^19 is 0.534645431078112 at 353683
Maximum from 2^19 to 2^20 is 0.533779229963368 at 722589
The winning number is 1489.

```


## Clojure


```Clojure
(ns rosettacode.hofstader-conway
  (:use [clojure.math.numeric-tower :only [expt]]))

;; A literal transcription of the definition, with memoize doing the heavy lifting
(def conway
  (memoize
    (fn [x]
      (if (< x 3)
        1
        (+  (-> x dec conway conway)
           (->> x dec conway (- x) conway))))))

(let [N      (drop 1 (range))
      pow2   (map #(expt 2 %) N)
      ; Split the natural numbers into groups at each power of two
      groups (partition-by (fn [x] (filter #(> % x) pow2)) N)
      maxima (->> (map #(map conway %) groups)
                  (map #(map / %2 %1) groups)  ; Each conway number divided by its index
                  (map #(apply max %)))
      m20    (take 20 maxima)]
  (println
    (take 4 maxima) "\n"
    (apply >= m20)  "\n"
    (map double m20)))   ; output the decimal forms
```



## Common Lisp


```lisp
(defparameter *hof-con*
  (make-array '(2) :initial-contents '(1 1) :adjustable t
	      :element-type 'integer :fill-pointer 2))

(defparameter *hof-con-ratios*
  (make-array '(2) :initial-contents '(1.0 0.5) :adjustable t
	      :element-type 'single-float :fill-pointer 2))

(defun hof-con (n)
  (let ((l (length *hof-con*)))
    (if (<= n l) (aref *hof-con* (1- n))
	(extend-hof-con-sequence l n))))

(defun extend-hof-con-sequence (l n)
  (loop for i from l below n do
       (let* ((x (aref *hof-con* (1- i)))
	      (hc (+ (aref *hof-con* (1- x))
		     (aref *hof-con* (- i x)))))
	 (vector-push-extend hc *hof-con*)
	 (vector-push-extend (/ hc (+ i 1.0)) *hof-con-ratios*)))
  (aref *hof-con* (1- n)))

(defun max-in-array-range (arr id1 id2)
  (let ((m 0) (id 0))
    (loop for i from (1- id1) to (1- id2) do
	 (let ((n (aref arr i)))
	   (if (> n m) (setq m n id i))))
    (values m (1+ id))))

(defun maxima (po2)
  (hof-con (expt 2 po2))
  (loop for i from 1 below po2 do
       (let ((id1 (expt 2 i)) (id2 (expt 2 (1+ i))))
	 (multiple-value-bind (m id)
	     (max-in-array-range *hof-con-ratios* id1 id2)
	   (format t "Local maximum in [~A .. ~A]: ~A at n = ~A~%" id1 id2 m id)))))

(defun mallows (po2)
  (let ((n (expt 2 po2)))
    (hof-con n)
    (do ((i (1- n) (1- i)))
	((> (aref *hof-con-ratios* i) 0.55) (+ i 1)))))
```

Sample session:

```txt
ROSETTA> (maxima 20)
Local maximum in [2 .. 4]: 0.6666667 at n = 3
Local maximum in [4 .. 8]: 0.6666667 at n = 6
Local maximum in [8 .. 16]: 0.6363636 at n = 11
Local maximum in [16 .. 32]: 0.6086956 at n = 23
Local maximum in [32 .. 64]: 0.59090906 at n = 44
Local maximum in [64 .. 128]: 0.57608694 at n = 92
Local maximum in [128 .. 256]: 0.5674157 at n = 178
Local maximum in [256 .. 512]: 0.55945945 at n = 370
Local maximum in [512 .. 1024]: 0.5549374 at n = 719
Local maximum in [1024 .. 2048]: 0.55010086 at n = 1487
Local maximum in [2048 .. 4096]: 0.5474629 at n = 2897
Local maximum in [4096 .. 8192]: 0.54414475 at n = 5969
Local maximum in [8192 .. 16384]: 0.5424427 at n = 11651
Local maximum in [16384 .. 32768]: 0.54007107 at n = 22223
Local maximum in [32768 .. 65536]: 0.538784 at n = 45083
Local maximum in [65536 .. 131072]: 0.53704363 at n = 89516
Local maximum in [131072 .. 262144]: 0.53602004 at n = 181385
Local maximum in [262144 .. 524288]: 0.53464544 at n = 353683
Local maximum in [524288 .. 1048576]: 0.5337792 at n = 722589
NIL
ROSETTA> (mallows 20)
1489
```



## D


```d
import std.stdio, std.algorithm;

void hofstadterConwaySequence(in int m) {
    auto alist = new int[m + 1];
    alist[0 .. 2] = 1;
    auto v = alist[2];
    int k1 = 2, lg2 = 1;
    double amax = 0.0;

    foreach (n; 2 .. m + 1) {
        v = alist[n] = alist[v] + alist[n - v];
        amax = max(amax, v * 1.0 / n);
        if ((k1 & n) == 0) {
            writefln("Max in [2^%d, 2^%d]: %f", lg2, lg2 + 1, amax);
            amax = 0;
            lg2++;
        }
        k1 = n;
    }
}

void main() {
    hofstadterConwaySequence(2 ^^ 20);
}
```


Output:

```txt
Max in [2^1, 2^2]: 0.666667
Max in [2^2, 2^3]: 0.666667
Max in [2^3, 2^4]: 0.636364
Max in [2^4, 2^5]: 0.608696
Max in [2^5, 2^6]: 0.590909
Max in [2^6, 2^7]: 0.576087
Max in [2^7, 2^8]: 0.567416
Max in [2^8, 2^9]: 0.559459
Max in [2^9, 2^10]: 0.554937
Max in [2^10, 2^11]: 0.550101
Max in [2^11, 2^12]: 0.547463
Max in [2^12, 2^13]: 0.544145
Max in [2^13, 2^14]: 0.542443
Max in [2^14, 2^15]: 0.540071
Max in [2^15, 2^16]: 0.538784
Max in [2^16, 2^17]: 0.537044
Max in [2^17, 2^18]: 0.536020
Max in [2^18, 2^19]: 0.534645
Max in [2^19, 2^20]: 0.533779
```



## EchoLisp


```scheme

(decimals 4)
(cache-size 2000000)

(define (a n)
	(+ (a (a (1- n))) (a (- n (a (1- n))))))

(remember 'a #(0 1 1)) ;; memoize

;; prints max a(n)/n in [2**i 2**i+1] intervals
;; return Mallows number checked up to 2**20
(define (task  (maxv) (start 1) (end 2) (v) (mrange 0))
	(for ((i  (in-range 1 21)))
	(set! maxv 0)

	(for ((n  (in-range start end)))
	(set! v (// (a n) n))
	#:when (> v maxv)
		(set! maxv v))
	    (when (and (zero? mrange) (< maxv 0.55)) (set! mrange end))

	(printf "[%d .. %d]  →  max a(n)/n:  %d " start end maxv)
	(set! start end)
	(set! end (* start 2)))

;; mallows
	(for ((n (in-range mrange 2 -1)))
		#:break (> (// (a n) n) 0.55) => n )
	)

```

```txt

(task)
[1 .. 2] → max a(n)/n: 1
[2 .. 4] → max a(n)/n: 0.6667
[4 .. 8] → max a(n)/n: 0.6667
[8 .. 16] → max a(n)/n: 0.6364
[16 .. 32] → max a(n)/n: 0.6087
[32 .. 64] → max a(n)/n: 0.5909
[64 .. 128] → max a(n)/n: 0.5761
[128 .. 256] → max a(n)/n: 0.5674
[256 .. 512] → max a(n)/n: 0.5595
[512 .. 1024] → max a(n)/n: 0.5549
[1024 .. 2048] → max a(n)/n: 0.5501
[2048 .. 4096] → max a(n)/n: 0.5475
[4096 .. 8192] → max a(n)/n: 0.5441
[8192 .. 16384] → max a(n)/n: 0.5424
[16384 .. 32768] → max a(n)/n: 0.5401
[32768 .. 65536] → max a(n)/n: 0.5388
[65536 .. 131072] → max a(n)/n: 0.537
[131072 .. 262144] → max a(n)/n: 0.536
[262144 .. 524288] → max a(n)/n: 0.5346
[524288 .. 1048576] → max a(n)/n: 0.5338
   → 1489 ;; Mallows number

```




## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
			--Tests the feature sequence.
		local
			j, n, exp: INTEGER
			max: REAL_64
		do
			exp := 15
			n := (2 ^ exp).floor
			sequence (n)
			across
				1 |..| (exp - 1) as c
			loop
				max := 0
				from
					j := (2 ^ c.item).floor
				until
					j > 2 ^ (c.item + 1)
				loop
					if members [j] / j > max then
						max := members [j] / j
					end
					j := j + 1
				end
				io.put_string ("Between 2^" + c.item.out + "and 2^" + (c.item + 1).out + " the max is: " + max.out)
				io.new_line
			end
		end

feature {NONE}

	members: LINKED_LIST [INTEGER]
			-- Members of the Hofstadter Conway $10000 sequence.

	sequence (n: INTEGER)
			-- Hofstadter Conway $10000 sequence up to 'n' in 'members'.
		require
			n_positive: n > 0
		local
			last: INTEGER
		do
			create members.make
			members.extend (1)
			members.extend (1)
			across
				3 |..| n as c
			loop
				last := members.last
				members.extend (members [last] + members [c.item - last])
			end
		end

end

```

As the run time is quite slow, the test output is shown only up to 2^15.

```txt

Between 2^1 and 2^2 the max is: 0.66666666666666663 at 3.
Between 2^2 and 2^3 the max is: 0.66666666666666663 at 6.
Between 2^3 and 2^4 the max is: 0.63636363636363635 at 11.
Between 2^4 and 2^5 the max is: 0.60869565217391308 at 23.
Between 2^5 and 2^6 the max is: 0.59090909090909094 at 44.
Between 2^6 and 2^7 the max is: 0.57608695652173914 at 92.
Between 2^7 and 2^8 the max is: 0.56741573033707871 at 178.
Between 2^8 and 2^9 the max is: 0.55945945945945941 at 370.
Between 2^9 and 2^10 the max is: 0.55493741307371347 at 719.
Between 2^10 and 2^11 the max is: 0.55010087424344323 at 1487.
Between 2^11 and 2^12 the max is: 0.54746289264756642 at 2897.
Between 2^12 and 2^13 the max is: 0.54414474786396383 at 5969.
Between 2^13 and 2^14 the max is: 0.54244270878036216 at 11651.
Between 2^14 and 2^15 the max is: 0.54007109754458711 at 22223.

```



## Erlang


```Erlang

-module( hofstadter_conway ).

-export( [sequence/1, sequence_div_n/1, task/0] ).

-record( power_of_2, {div_n=0, max=4, min=2, n=0} ).

sequence( 1 ) -> [1];
sequence( 2 ) -> [1, 1];
sequence( Up_to ) when Up_to >= 3 ->
        From_3 = lists:seq( 3, Up_to ),
        Dict = lists:foldl( fun sequence_dict/2, dict:from_list([{1, 1}, {2, 1}]), From_3 ),
	[1, 1 | [dict:fetch(X, Dict) || X <- From_3]].

sequence_div_n( N ) ->
        Sequence = sequence( N ),
	[{X, Y / X} || {X, Y} <- lists:zip(lists:seq(1, N), Sequence)].

task() ->
       [_First | Rest] = sequence_div_n( erlang:round(math:pow(2, 20)) ),
       {_Power, Powers} = lists:foldl( fun max_between_power_of_2/2, {#power_of_2{}, []}, Rest ),
       [io:fwrite( "Maximum between ~p and ~p is ~p for n=~p~n", [X#power_of_2.min, X#power_of_2.max, X#power_of_2.div_n, X#power_of_2.n]) || X <- Powers].



max_between_power_of_2( {N, _Div_n}, {#power_of_2{max=N}=P, Acc} ) ->
        {#power_of_2{max=N * 2, min=N}, [P | Acc]};
max_between_power_of_2( {N, Larger_div_n}, {#power_of_2{div_n=Div_n}=P, Acc} ) when Larger_div_n > Div_n ->
        {P#power_of_2{n=N, div_n=Larger_div_n}, Acc};
max_between_power_of_2( _, Both ) -> Both.

sequence_dict( Key, Dict ) ->
        Last_number = dict:fetch( Key - 1, Dict ),
        At_begining = dict:fetch( Last_number, Dict ),
        At_end = dict:fetch( Key - Last_number, Dict ),
        dict:store( Key, At_begining + At_end, Dict ).

```

```txt

17> hofstadter_conway:task().
Maximum between 524288 and 1048576 is 0.5337792299633678 for n=722589
Maximum between 262144 and 524288 is 0.5346454310781124 for n=353683
Maximum between 131072 and 262144 is 0.5360200678115611 for n=181385
Maximum between 65536 and 131072 is 0.5370436569998659 for n=89516
Maximum between 32768 and 65536 is 0.5387840205842557 for n=45083
Maximum between 16384 and 32768 is 0.5400710975115871 for n=22223
Maximum between 8192 and 16384 is 0.5424427087803622 for n=11651
Maximum between 4096 and 8192 is 0.5441447478639638 for n=5969
Maximum between 2048 and 4096 is 0.5474628926475664 for n=2897
Maximum between 1024 and 2048 is 0.5501008742434432 for n=1487
Maximum between 512 and 1024 is 0.5549374130737135 for n=719
Maximum between 256 and 512 is 0.5594594594594594 for n=370
Maximum between 128 and 256 is 0.5674157303370787 for n=178
Maximum between 64 and 128 is 0.5760869565217391 for n=92
Maximum between 32 and 64 is 0.5909090909090909 for n=44
Maximum between 16 and 32 is 0.6086956521739131 for n=23
Maximum between 8 and 16 is 0.6363636363636364 for n=11
Maximum between 4 and 8 is 0.6666666666666666 for n=6
Maximum between 2 and 4 is 0.6666666666666666 for n=3

```



## Euler Math Toolbox



```Euler Math Toolbox

>function hofstadter (n) ...
$v=ones(1,n);
$  loop 2 to n-1
$    k=v{#};
$    v{#+1}=v{k}+v{#-k+1};
$  end
$  return v
$endfunction
>v=hofstadter(2^20);
>k=1:256; plot2d(v[k]/k):
>function hsmaxima (v,k) ...
$  w=zeros(1,k);
$  for j=1 to k
$    i=2^(j-1):2^j;
$    w[j]=max(v[i]/i);
$  end;
$  return w;
$endfunction
>w=hsmaxima(v,20)
 [ 1  0.666666666667  0.666666666667  0.636363636364  0.608695652174
 0.590909090909  0.576086956522  0.567415730337  0.559459459459
 0.554937413074  0.550100874243  0.547462892648  0.544144747864
 0.54244270878  0.540071097512  0.538784020584  0.537043657
 0.536020067812  0.534645431078  0.533779229963 ]
>v1=flipx(cummax(flipx(v/(1:cols(v)))));
>max(nonzeros(v1>0.55))
 1489

```

=={{header|F Sharp|F#}}==

```fsharp
let a = ResizeArray[0; 1; 1]
while a.Count <= (1 <<< 20) do
  a.[a.[a.Count - 1]] + a.[a.Count - a.[a.Count - 1]] |> a.Add
for p = 1 to 19 do
  Seq.max [|for i in 1 <<< p .. 1 <<< p+1 -> float a.[i] / float i|]
  |> printf "Maximum in %6d..%7d is %g\n" (1 <<< p) (1 <<< p+1)
let mallows, _ = a
                 |> List.ofSeq
                 |> List.mapi (fun i n -> i, n)
                 |> List.rev
                 |> List.find (fun (i, n)  -> float(n) / float(i) > 0.55)
printfn "Mallows number is %d" mallows
```

Outputs:

```txt
Maximum in      2..      4 is 0.666667
Maximum in      4..      8 is 0.666667
Maximum in      8..     16 is 0.636364
Maximum in     16..     32 is 0.608696
Maximum in     32..     64 is 0.590909
Maximum in     64..    128 is 0.576087
Maximum in    128..    256 is 0.567416
Maximum in    256..    512 is 0.559459
Maximum in    512..   1024 is 0.554937
Maximum in   1024..   2048 is 0.550101
Maximum in   2048..   4096 is 0.547463
Maximum in   4096..   8192 is 0.544145
Maximum in   8192..  16384 is 0.542443
Maximum in  16384..  32768 is 0.540071
Maximum in  32768..  65536 is 0.538784
Maximum in  65536.. 131072 is 0.537044
Maximum in 131072.. 262144 is 0.53602
Maximum in 262144.. 524288 is 0.534645
Maximum in 524288..1048576 is 0.533779
Mallows number is 1489
```


## Fortran


```Fortran

program conway
  implicit none
  integer :: a(2**20)  ! The sequence a(n)
  real    :: b(2**20)  ! The sequence a(n)/n
  real    :: v         ! Max value in the range [2*i, 2**(i+1)]
  integer :: nl(1)     ! The location of v in the array b(n)
  integer :: i, N, first, second, last, m

  ! Populate a(n) and b(n)
  a(1:2) = [1, 1]
  b(1:2) = [1.0e0, 0.5e0]
  N = 2
  do i=1,2**20
     last = a(N)
     first = a(last)
     second = a(N-last+1)
     N = N+1
     a(N:N) = first + second
     b(N:N) = a(N:N)/real(N)
  end do

  ! Calculate the max values in the logarithmic ranges
  m = 0
  do i=1,19
     v = maxval(b(2**i:2**(i+1)))
     nl = maxloc(b(2**i:2**(i+1)))
     write(*,'(2(a,i0),a,f8.6,a,i0)') &
          'Max. between 2**', i,      &
          ' and 2**', (i+1),          &
          ' is ', v,                  &
          ' at n = ', 2**i+nl(1)-1
     if (m == 0 .and. v < 0.55e0) then
        m = i-1
     end if
  end do

  ! Calculate Mallows number
  do i=2**(m+1), 2**m,-1
     if (b(i) > 0.55e0) then
        exit
     end if
  end do
  write(*,'(a,i0)') 'Mallows number = ',i

end program conway

```


Output:

```txt

Max. between 2**1 and 2**2 is 0.666667 at n = 3
Max. between 2**2 and 2**3 is 0.666667 at n = 6
Max. between 2**3 and 2**4 is 0.636364 at n = 11
Max. between 2**4 and 2**5 is 0.608696 at n = 23
Max. between 2**5 and 2**6 is 0.590909 at n = 44
Max. between 2**6 and 2**7 is 0.576087 at n = 92
Max. between 2**7 and 2**8 is 0.567416 at n = 178
Max. between 2**8 and 2**9 is 0.559459 at n = 370
Max. between 2**9 and 2**10 is 0.554937 at n = 719
Max. between 2**10 and 2**11 is 0.550101 at n = 1487
Max. between 2**11 and 2**12 is 0.547463 at n = 2897
Max. between 2**12 and 2**13 is 0.544145 at n = 5969
Max. between 2**13 and 2**14 is 0.542443 at n = 11651
Max. between 2**14 and 2**15 is 0.540071 at n = 22223
Max. between 2**15 and 2**16 is 0.538784 at n = 45083
Max. between 2**16 and 2**17 is 0.537044 at n = 89516
Max. between 2**17 and 2**18 is 0.536020 at n = 181385
Max. between 2**18 and 2**19 is 0.534645 at n = 353683
Max. between 2**19 and 2**20 is 0.533779 at n = 722589
Mallows number = 1489

```


## FreeBASIC

```freebasic
' version 13-07-2018
' compile with: fbc -s console

Dim As UInteger a(), pow2 = 2, p2 = 2 ^ pow2, peakpos, n, mallows
Dim As Double peak = 0.5, r
ReDim a(2 ^ 20)
a(1) = 1
a(2) = 1

For n = 3 To 2 ^ 20
    a(n) = a(a(n -1)) + a(n - a(n -1))
    r = a(n) / n
    If r >= 0.55 Then mallows = n
    If r > peak Then peak = r : peakpos = n
    If n = p2 Then
        Print Using "Maximum between 2 ^ ## and 2 ^ ## is"; pow2 -1; pow2;
        Print Using " #.##### at n = "; peak;
        Print peakpos
        pow2 += 1
        p2 = 2 ^ pow2
        peak = 0.5
    End If
Next

Print
Print "Mallows number is "; mallows

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Maximum between 2 ^  1 and 2 ^  2 is 0.66667 at n = 3
Maximum between 2 ^  2 and 2 ^  3 is 0.66667 at n = 6
Maximum between 2 ^  3 and 2 ^  4 is 0.63636 at n = 11
Maximum between 2 ^  4 and 2 ^  5 is 0.60870 at n = 23
Maximum between 2 ^  5 and 2 ^  6 is 0.59091 at n = 44
Maximum between 2 ^  6 and 2 ^  7 is 0.57609 at n = 92
Maximum between 2 ^  7 and 2 ^  8 is 0.56742 at n = 178
Maximum between 2 ^  8 and 2 ^  9 is 0.55946 at n = 370
Maximum between 2 ^  9 and 2 ^ 10 is 0.55494 at n = 719
Maximum between 2 ^ 10 and 2 ^ 11 is 0.55010 at n = 1487
Maximum between 2 ^ 11 and 2 ^ 12 is 0.54746 at n = 2897
Maximum between 2 ^ 12 and 2 ^ 13 is 0.54414 at n = 5969
Maximum between 2 ^ 13 and 2 ^ 14 is 0.54244 at n = 11651
Maximum between 2 ^ 14 and 2 ^ 15 is 0.54007 at n = 22223
Maximum between 2 ^ 15 and 2 ^ 16 is 0.53878 at n = 45083
Maximum between 2 ^ 16 and 2 ^ 17 is 0.53704 at n = 89516
Maximum between 2 ^ 17 and 2 ^ 18 is 0.53602 at n = 181385
Maximum between 2 ^ 18 and 2 ^ 19 is 0.53465 at n = 353683
Maximum between 2 ^ 19 and 2 ^ 20 is 0.53378 at n = 722589

Mallows number is 1489
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

// Set width of tab
def tab 9

dim as long Mallows, n, pow2, p2, pPos, uprLim
dim as double p
print

// Adjust array elements depending on size of sequence
_maxArrayElements = 1200000

input "Enter upper limit between 1 and 20 (Enter 20 gives 2^20): "; uprLim

dim as double r
dim as long a( _maxArrayElements )

if uprLim < 1 or uprLim > 20 then uprLim = 20

a(1) = 1
a(2) = 1
pow2 = 2
p2 = 2 ^ pow2
p = 0.5
pPos = 0

print

for n = 3 to 2 ^ uprLim
   a(n) = a( a( n-1 ) ) + a( n-a( n-1 ) )
   r = a(n) / n
      if r >= 0.55 then Mallows = n
      if r > p
         p = r
         pPos = n
      end if

      if n == p2
         print "Maximum of a(n)/n between", " 2^"; pow2-1; " and 2^"; pow2," is "; p;, " at n = "; pPos
         pow2 = pow2 + 1
         p2   = 2 ^ pow2
         p    = 0.5
      end if
next
print
print "Dr. Mallow's winning number is:"; Mallows

```


Output:

```txt

Enter upper limit between 1 and 20 (Enter 20 gives 2^20): 20

Maximum of a(n)/n between   2^ 1 and 2^ 2     is  0.6666666667  at n =  3
Maximum of a(n)/n between   2^ 2 and 2^ 3     is  0.6666666667  at n =  6
Maximum of a(n)/n between   2^ 3 and 2^ 4     is  0.6363636364  at n =  11
Maximum of a(n)/n between   2^ 4 and 2^ 5     is  0.6086956522  at n =  23
Maximum of a(n)/n between   2^ 5 and 2^ 6     is  0.5909090909  at n =  44
Maximum of a(n)/n between   2^ 6 and 2^ 7     is  0.5760869565  at n =  92
Maximum of a(n)/n between   2^ 7 and 2^ 8     is  0.5674157303  at n =  178
Maximum of a(n)/n between   2^ 8 and 2^ 9     is  0.5594594595  at n =  370
Maximum of a(n)/n between   2^ 9 and 2^ 10    is  0.5549374131  at n =  719
Maximum of a(n)/n between   2^ 10 and 2^ 11   is  0.5501008742  at n =  1487
Maximum of a(n)/n between   2^ 11 and 2^ 12   is  0.5474628926  at n =  2897
Maximum of a(n)/n between   2^ 12 and 2^ 13   is  0.5441447479  at n =  5969
Maximum of a(n)/n between   2^ 13 and 2^ 14   is  0.5424427088  at n =  11651
Maximum of a(n)/n between   2^ 14 and 2^ 15   is  0.5400710975  at n =  22223
Maximum of a(n)/n between   2^ 15 and 2^ 16   is  0.5387840206  at n =  45083
Maximum of a(n)/n between   2^ 16 and 2^ 17   is  0.537043657   at n =  89516
Maximum of a(n)/n between   2^ 17 and 2^ 18   is  0.5360200678  at n =  181385
Maximum of a(n)/n between   2^ 18 and 2^ 19   is  0.5346454311  at n =  353683
Maximum of a(n)/n between   2^ 19 and 2^ 20   is  0.53377923    at n =  722589

Dr. Mallow's winning number is: 1489

```



## Go


```go
package main

import (
    "fmt"
)

func main() {
    a := []int{0, 1, 1} // ignore 0 element. work 1 based.
    x := 1  // last number in list
    n := 2  // index of last number in list = len(a)-1
    mallow := 0
    for p := 1; p < 20; p++ {
        max := 0.
        for nextPot := n*2; n < nextPot; {
            n = len(a) // advance n
            x = a[x]+a[n-x]
            a = append(a, x)
            f := float64(x)/float64(n)
            if f > max {
                max = f
            }
            if f >= .55 {
                mallow = n
            }
        }
        fmt.Printf("max between 2^%d and 2^%d was %f\n", p, p+1, max)
    }
    fmt.Println("winning number", mallow)
}
```

Output:

```txt


max between 2^1 and 2^2 was 0.666667
max between 2^2 and 2^3 was 0.666667
max between 2^3 and 2^4 was 0.636364
max between 2^4 and 2^5 was 0.608696
max between 2^5 and 2^6 was 0.590909
max between 2^6 and 2^7 was 0.576087
max between 2^7 and 2^8 was 0.567416
max between 2^8 and 2^9 was 0.559459
max between 2^9 and 2^10 was 0.554937
max between 2^10 and 2^11 was 0.550101
max between 2^11 and 2^12 was 0.547463
max between 2^12 and 2^13 was 0.544145
max between 2^13 and 2^14 was 0.542443
max between 2^14 and 2^15 was 0.540071
max between 2^15 and 2^16 was 0.538784
max between 2^16 and 2^17 was 0.537044
max between 2^17 and 2^18 was 0.536020
max between 2^18 and 2^19 was 0.534645
max between 2^19 and 2^20 was 0.533779
winning number 1489

```



## Haskell


```haskell
import Data.List
import Data.Ord
import Data.Array
import Text.Printf

hc :: Int -> Array Int Int
hc n = arr
  where arr = listArray (1, n) $ 1 : 1 : map (f (arr!)) [3 .. n]
        f a i = a (a $ i - 1) + a (i - a (i - 1))

printMaxima :: (Int, (Int, Double)) -> IO ()
printMaxima (n, (pos, m)) =
    printf "Max between 2^%-2d and 2^%-2d is %1.5f at n = %6d\n"
                             n    (n + 1)        m        pos

main = do
    mapM_ printMaxima maxima
    printf "Mallows's number is %d\n" mallows
  where
    hca = hc $ 2^20
    hc' n  = fromIntegral (hca!n) / fromIntegral n
    maxima = zip [0..] $ map max powers
    max seq = maximumBy (comparing snd) $ zip seq (map hc' seq)
    powers = map (\n -> [2^n .. 2^(n + 1) - 1]) [0 .. 19]
    mallows = last.takeWhile ((< 0.55) . hc') $ [2^20, 2^20 - 1 .. 1]
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main(args)
    m       := integer(!args) | 20
    nextNum := create put(A := [], 1 | 1 | |A[A[*A]]+A[-A[*A]])[*A]
    p2      := 2 ^ (p := 1)
    maxv    := 0
    every n := 1 to (2^m) do {
        if maxv <:= (x := @nextNum / real(n)) then maxm := n
        if x >= 0.55 then mallows := n   # Want *this* n, not next one!
        if n = p2 then {
            write("Max between 2^",p-1," and 2^",p," is ",maxv," at n = ",maxm)
            p2 := 2 ^ (p +:= 1)
            maxv := 0
            }
        }
    write("Mallows's number is ",\mallows | "NOT found!")
end
```


Output:

```txt
->hc
Max between 2^0 and 2^1 is 1.0 at n = 1
Max between 2^1 and 2^2 is 0.6666666667 at n = 3
Max between 2^2 and 2^3 is 0.6666666667 at n = 6
Max between 2^3 and 2^4 is 0.6363636364 at n = 11
Max between 2^4 and 2^5 is 0.6086956522 at n = 23
Max between 2^5 and 2^6 is 0.5909090909 at n = 44
Max between 2^6 and 2^7 is 0.5760869565 at n = 92
Max between 2^7 and 2^8 is 0.5674157303 at n = 178
Max between 2^8 and 2^9 is 0.5594594595 at n = 370
Max between 2^9 and 2^10 is 0.5549374131 at n = 719
Max between 2^10 and 2^11 is 0.5501008742 at n = 1487
Max between 2^11 and 2^12 is 0.5474628926 at n = 2897
Max between 2^12 and 2^13 is 0.5441447479 at n = 5969
Max between 2^13 and 2^14 is 0.5424427088 at n = 11651
Max between 2^14 and 2^15 is 0.5400710975 at n = 22223
Max between 2^15 and 2^16 is 0.5387840206 at n = 45083
Max between 2^16 and 2^17 is 0.537043657 at n = 89516
Max between 2^17 and 2^18 is 0.5360200678 at n = 181385
Max between 2^18 and 2^19 is 0.5346454311 at n = 353683
Max between 2^19 and 2^20 is 0.53377923 at n = 722589
Mallow's number is 1489
->
```



## J

'''Solution''' (tacit):
```j
   hc10k =:  , ] +/@:{~ (,&<: -.)@{:    NB.  Actual sequence a(n)
   AnN   =:  % 1+i.@:#                  NB.  a(n)/n
   MxAnN =:  >./;.1~ 2 (=<.)@:^. 1+i.@# NB.  Maxima of a(n)/n between successive powers of 2
```

'''Alternative solution''' (exponential growth):

The first, naive, formulation of <code>hc10k</code> grows by a single term every iteration; in this one, the series grows exponentially in the iterations.

```j
   hc10kE     =:  1 1 , expand@tail
     expand   =:  2+I.@;
     tail     =:  copies&.>^:(<@>:`(<@,@2:))
       copies =:  >: |.@(#!.1 |.)~ 1 j. #;.1 #^:_1 ::1:~ ]~:{.
```


'''Example''':
```j
   ] A=:1 1 hc10k @]^:[~ 2^20x
1 1 2 2 3 4 4 4 5 6 7 7 8 8 8 8 9 ...
   AnN A
1 0.5 0.666667 0.5 0.6 0.666667 ...
   MxAnN@AnN A
1 0.666667 0.666667 0.636364 ...
   MxAnN@AnN@hc10kE 20
1 0.666667 0.666667 0.636364 ...
```


## Java

{{trans|C}} with corrections to 0 indexing.

```java
public class HofCon
{
 public static void main(final String[] args)
 {
  doSqnc(1<<20);
 }
 public static void doSqnc(int m)
 {
  int[] a_list = new int[m + 1];
  int max_df = 0;
  int p2_max = 2;
  int k1 = 2;
  int lg2 = 1;
  double amax = 0;
  a_list[0] = a_list[1] = 1;
  int v = a_list[2];
  for (int n = 2; n <= m; n++)
  {
   v = a_list[n] = a_list[v] + a_list[n - v];
   if (amax < v * 1.0 / n)
    amax = v * 1.0 / n;
   if (0 == (k1 & n))
   {
    System.out.printf("Maximum between 2^%d and 2^%d was %f\n", lg2, lg2 + 1, amax);
    amax = 0;
    lg2++;
   }
   k1 = n;
  }
 }
}
```

Output:

```txt
Maximum between 2^1 and 2^2 was 0.666667
Maximum between 2^2 and 2^3 was 0.666667
Maximum between 2^3 and 2^4 was 0.636364
Maximum between 2^4 and 2^5 was 0.608696
....
Maximum between 2^18 and 2^19 was 0.534645
Maximum between 2^19 and 2^20 was 0.533779
```


## JavaScript


```JavaScript
var hofst_10k = function(n) {
	var memo = [1, 1];

	var a = function(n) {
		var result = memo[n-1];
		if (typeof result !== 'number') {
			result = a(a(n-1))+a(n-a(n-1));
			memo[n-1] = result;
		}
		return result;
	}
	return a;
}();

var maxima_between_twos = function(exp) {
	var current_max = 0;
	for(var i = Math.pow(2,exp)+1; i < Math.pow(2,exp+1); i += 1) {
		current_max = Math.max(current_max, hofst_10k(i)/i);
	}
	return current_max;
}

for(var i = 1; i <= 20; i += 1) {
	console.log("Maxima between 2^"+i+"-2^"+(i+1)+" is: "+maxima_between_twos(i)+"\n");
}
```

Output:

```txt
Maxima between 2^1-2^2 is: 0.6666666666666666
Maxima between 2^2-2^3 is: 0.6666666666666666
Maxima between 2^3-2^4 is: 0.6363636363636364
Maxima between 2^4-2^5 is: 0.6086956521739131
Maxima between 2^5-2^6 is: 0.5909090909090909
...
Maxima between 2^18-2^19 is: 0.5346454310781124
Maxima between 2^19-2^20 is: 0.5337792299633678
Maxima between 2^20-2^21 is: 0.5326770563524978

```



## Julia


```julia
# v0.6

# Task 1
function hofstadterconway(n::Integer)::Vector{Int}
    rst = fill(1, n)
    for i in 3:n
        rst[i] = rst[rst[i - 1]] + rst[i - rst[i - 1]]
    end
    return rst
end

function hcfraction(n::Integer)::Vector{Float64}
    rst = Array{Float64}(n)
    for (i, a) in enumerate(hofstadterconway(n))
        rst[i] = abs(a) / i
    end
    return rst
end

# Task 2
seq = hcfraction(2 ^ 20)
for i in 1:20
    a, b = 2 ^ (i - 1), 2 ^ i
    @printf("max value of a(n)/n in %6i < n < %7i = %5.3f\n", a, b, maximum(seq[a:b]))
end

# Task 3
function lastindex(val::Float64)
    r, p = 1, 0
    # Find the range of 2 power in which the maximum is < val
    seq = hcfraction(2 ^ 15)
    while maximum(seq[2^p:2^(p+1)]) > val; p += 1 end
    r = 2 ^ (p + 1)
    while seq[r] < val; r -= 1 end
    return r
end

println("You too might have won \$1000 with the mallows number of ", lastindex(0.55))
```


```txt
max value of a(n)/n in      1 < n <       2 = 1.000
max value of a(n)/n in      2 < n <       4 = 0.667
max value of a(n)/n in      4 < n <       8 = 0.667
max value of a(n)/n in      8 < n <      16 = 0.636
max value of a(n)/n in     16 < n <      32 = 0.609
max value of a(n)/n in     32 < n <      64 = 0.591
max value of a(n)/n in     64 < n <     128 = 0.576
max value of a(n)/n in    128 < n <     256 = 0.567
max value of a(n)/n in    256 < n <     512 = 0.559
max value of a(n)/n in    512 < n <    1024 = 0.555
max value of a(n)/n in   1024 < n <    2048 = 0.550
max value of a(n)/n in   2048 < n <    4096 = 0.547
max value of a(n)/n in   4096 < n <    8192 = 0.544
max value of a(n)/n in   8192 < n <   16384 = 0.542
max value of a(n)/n in  16384 < n <   32768 = 0.540
max value of a(n)/n in  32768 < n <   65536 = 0.539
max value of a(n)/n in  65536 < n <  131072 = 0.537
max value of a(n)/n in 131072 < n <  262144 = 0.536
max value of a(n)/n in 262144 < n <  524288 = 0.535
max value of a(n)/n in 524288 < n < 1048576 = 0.534
You too might have won $1000 with the mallows number of 1489
```



## Kotlin


```scala
// version 1.1.2

fun main(args: Array<String>) {
    val limit = (1 shl 20) + 1
    val a = IntArray(limit)
    a[1] = 1
    a[2] = 1
    for (n in 3 until limit) {
        val p = a[n - 1]
        a[n] = a[p] + a[n - p]
    }

    println("     Range          Maximum")
    println("----------------   --------")
    var pow2 = 1
    var p = 1
    var max = a[1].toDouble()
    for (n in 2 until limit) {
        val r = a[n].toDouble() / n
        if (r > max) max = r
        if (n == pow2 * 2) {
            println("2 ^ ${"%2d".format(p - 1)} to 2 ^ ${"%2d".format(p)}   ${"%f".format(max)}")
            pow2 *= 2
            p++
            max = r
        }
    }

    var prize = 0
    for (n in limit - 1 downTo 1) {
        if (a[n].toDouble() / n >= 0.55) {
            prize = n
            break
        }
    }
    println("\nMallows' number = $prize")
}
```


```txt

     Range          Maximum
----------------   --------
2 ^  0 to 2 ^  1   1.000000
2 ^  1 to 2 ^  2   0.666667
2 ^  2 to 2 ^  3   0.666667
2 ^  3 to 2 ^  4   0.636364
2 ^  4 to 2 ^  5   0.608696
2 ^  5 to 2 ^  6   0.590909
2 ^  6 to 2 ^  7   0.576087
2 ^  7 to 2 ^  8   0.567416
2 ^  8 to 2 ^  9   0.559459
2 ^  9 to 2 ^ 10   0.554937
2 ^ 10 to 2 ^ 11   0.550101
2 ^ 11 to 2 ^ 12   0.547463
2 ^ 12 to 2 ^ 13   0.544145
2 ^ 13 to 2 ^ 14   0.542443
2 ^ 14 to 2 ^ 15   0.540071
2 ^ 15 to 2 ^ 16   0.538784
2 ^ 16 to 2 ^ 17   0.537044
2 ^ 17 to 2 ^ 18   0.536020
2 ^ 18 to 2 ^ 19   0.534645
2 ^ 19 to 2 ^ 20   0.533779

Mallows' number = 1489

```



## Lua

I solved it, using a coroutine to generate the Hofstadter numbers, because it's fun to do so. This can be done differently, but not with so much fun, I guess. It's counting from the first number 1, the second 1, the third 2 and so on. This doesn't change anything about the outcome, but I guess it's better like this and consistent with such things like fibonacci numbers.

```lua

local fmt, write=string.format,io.write
local hof=coroutine.wrap(function()
	local yield=coroutine.yield
	local a={1,1}
	yield(a[1], 1)
	yield(a[2], 2)
	local n=a[#a]
	repeat
		n=a[n]+a[1+#a-n]
		a[#a+1]=n
		yield(n, #a)
	until false
end)

local mallows, mdiv=0,0
for p=1,20 do
	local max, div, num, last, fdiv=0,0,0,0,0
	for i=2^(p-1),2^p-1 do
		h,n=hof()
		div=h/n
		if div>max then
			max=div
			num=n
		end
		if div>0.55 then
			last=n
			fdiv=div
		end
	end
	write(fmt("From 2^%-2d to 2^%-2d the max is %.4f the %6dth Hofstadter number.\n",
		p-1, p, max, num))
	if max>.55 and p>4 then
		mallows, mdiv=last, fdiv
	end
end
write("So Mallows number is ", mallows, " with ", fmt("%.4f",mdiv), ", yay, just wire me my $10000 now!\n")

```

```txt

From 2^0  to 2^1  the max is 1.0000 the      1th Hofstadter number.
From 2^1  to 2^2  the max is 0.6667 the      3th Hofstadter number.
From 2^2  to 2^3  the max is 0.6667 the      6th Hofstadter number.
From 2^3  to 2^4  the max is 0.6364 the     11th Hofstadter number.
From 2^4  to 2^5  the max is 0.6087 the     23th Hofstadter number.
From 2^5  to 2^6  the max is 0.5909 the     44th Hofstadter number.
From 2^6  to 2^7  the max is 0.5761 the     92th Hofstadter number.
From 2^7  to 2^8  the max is 0.5674 the    178th Hofstadter number.
From 2^8  to 2^9  the max is 0.5595 the    370th Hofstadter number.
From 2^9  to 2^10 the max is 0.5549 the    719th Hofstadter number.
From 2^10 to 2^11 the max is 0.5501 the   1487th Hofstadter number.
From 2^11 to 2^12 the max is 0.5475 the   2897th Hofstadter number.
From 2^12 to 2^13 the max is 0.5441 the   5969th Hofstadter number.
From 2^13 to 2^14 the max is 0.5424 the  11651th Hofstadter number.
From 2^14 to 2^15 the max is 0.5401 the  22223th Hofstadter number.
From 2^15 to 2^16 the max is 0.5388 the  45083th Hofstadter number.
From 2^16 to 2^17 the max is 0.5370 the  89516th Hofstadter number.
From 2^17 to 2^18 the max is 0.5360 the 181385th Hofstadter number.
From 2^18 to 2^19 the max is 0.5346 the 353683th Hofstadter number.
From 2^19 to 2^20 the max is 0.5338 the 722589th Hofstadter number.
So Mallows number is 1489 with 0.5500, yay, just wire me my $10000 now!

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
a[1] := 1; a[2] := 1;
a[n_] := a[n] = a[a[n-1]] + a[n-a[n-1]]

Map[Print["Max value: ",Max[Table[a[n]/n//N,{n,2^#,2^(#+1)}]]," for n between 2^",#," and 2^",(#+1)]& , Range[19]]
n=2^20; While[(a[n]/n//N)<0.55,n--]; Print["Mallows number: ",n]
```


Outputs:

```txt
Max value: 0.666667 for n between 2^1 and 2^2
Max value: 0.666667 for n between 2^2 and 2^3
Max value: 0.636364 for n between 2^3 and 2^4
Max value: 0.608696 for n between 2^4 and 2^5
Max value: 0.590909 for n between 2^5 and 2^6
Max value: 0.576087 for n between 2^6 and 2^7
Max value: 0.567416 for n between 2^7 and 2^8
Max value: 0.559459 for n between 2^8 and 2^9
Max value: 0.554937 for n between 2^9 and 2^10
Max value: 0.550101 for n between 2^10 and 2^11
Max value: 0.547463 for n between 2^11 and 2^12
Max value: 0.544145 for n between 2^12 and 2^13
Max value: 0.542443 for n between 2^13 and 2^14
Max value: 0.540071 for n between 2^14 and 2^15
Max value: 0.538784 for n between 2^15 and 2^16
Max value: 0.537044 for n between 2^16 and 2^17
Max value: 0.53602 for n between 2^17 and 2^18
Max value: 0.534645 for n between 2^18 and 2^19
Max value: 0.533779 for n between 2^19 and 2^20
Mallows number: 1489
```


=={{header|MATLAB}} / {{header|Octave}}==


```matlab
 function Q = HCsequence(N)
  Q = zeros(1,N);
  Q(1:2) = 1;
  for n = 3:N,
    Q(n) = Q(Q(n-1))+Q(n-Q(n-1));
  end;
end;
```


The function can be tested in this way:

```matlab
NN = 20;
Q = HCsequence(2^NN+1);
V = Q./(1:2^NN);
for k=1:NN,
  [m,i] = max(V(2^k:2^(k+1)));
  i = i + 2^k - 1;
  printf('Maximum between 2^%i and 2^%i is %f at n=%i\n',k,k+1,m,i);
end;
```


Output:

```txt
Maximum between 2^1 and 2^2 is 0.666667 at n=3
Maximum between 2^2 and 2^3 is 0.666667 at n=6
Maximum between 2^3 and 2^4 is 0.636364 at n=11
Maximum between 2^4 and 2^5 is 0.608696 at n=23
Maximum between 2^5 and 2^6 is 0.590909 at n=44
Maximum between 2^6 and 2^7 is 0.576087 at n=92
Maximum between 2^7 and 2^8 is 0.567416 at n=178
Maximum between 2^8 and 2^9 is 0.559459 at n=370
Maximum between 2^9 and 2^10 is 0.554937 at n=719
Maximum between 2^10 and 2^11 is 0.550101 at n=1487
Maximum between 2^11 and 2^12 is 0.547463 at n=2897
Maximum between 2^12 and 2^13 is 0.544145 at n=5969
Maximum between 2^13 and 2^14 is 0.542443 at n=11651
Maximum between 2^14 and 2^15 is 0.540071 at n=22223
Maximum between 2^15 and 2^16 is 0.538784 at n=45083
Maximum between 2^16 and 2^17 is 0.537044 at n=89516
Maximum between 2^17 and 2^18 is 0.536020 at n=181385
Maximum between 2^18 and 2^19 is 0.534645 at n=353683
Maximum between 2^19 and 2^20 is 0.533779 at n=722589
```



## Nim

```nim
import strutils

const last = 1 shl 20

var aList: array[last + 1, int]
aList[0..2] = [-50_000, 1, 1]
var
  v    = aList[2]
  k1   = 2
  lg2  = 1
  aMax = 0.0

for n in 3..last:
  v = aList[v] + aList[n-v]
  aList[n] = v
  aMax = max(aMax, v.float / n.float)
  if (k1 and n) == 0:
    echo "Maximum between 2^$# and 2^$# was $#".format(lg2, lg2+1, aMax)
    aMax = 0
    inc lg2
  k1 = n
```

Output:

```txt
Maximum between 2^1 and 2^2 was 0.6666666666666666
Maximum between 2^2 and 2^3 was 0.6666666666666666
Maximum between 2^3 and 2^4 was 0.6363636363636364
Maximum between 2^4 and 2^5 was 0.6086956521739131
...
Maximum between 2^18 and 2^19 was 0.5346454310781124
Maximum between 2^19 and 2^20 was 0.5337792299633678
```



## Objeck


```objeck
bundle Default {
  class HofCon {
    function : Main(args : String[]) ~ Nil {
      DoSqnc(1<<20);
    }

    function : native : DoSqnc(m : Int) ~ Nil {
      a_list := Int->New[m + 1];
      max_df := 0;
      p2_max := 2;
      k1 := 2;
      lg2 := 1;
      amax := 0.0;

      a_list[0] := 1;
      a_list[1] := 1;

      v := a_list[2];

      for(n := 2; n <= m; n+=1;) {
        r := a_list[v] + a_list[n - v];
        v := r;
        a_list[n] := r;

        if(amax < v * 1.0 / n) {
          amax := v * 1.0 / n;
        };

        if(0 = (k1 and n)) {
          IO.Console->Print("Maximum between 2^")->Print(lg2)
            ->Print(" and 2^")->Print(lg2 + 1)->Print(" was ")->PrintLine(amax);
          amax := 0;
          lg2+=1;
        };
        k1 := n;
      };
    }
  }
}

```



```txt

Maximum between 2^1 and 2^2 was 0.666666667
Maximum between 2^2 and 2^3 was 0.666666667
Maximum between 2^3 and 2^4 was 0.636363636
Maximum between 2^4 and 2^5 was 0.608695652
Maximum between 2^5 and 2^6 was 0.590909091
Maximum between 2^6 and 2^7 was 0.576086957
Maximum between 2^7 and 2^8 was 0.56741573
Maximum between 2^8 and 2^9 was 0.559459459
Maximum between 2^9 and 2^10 was 0.554937413
Maximum between 2^10 and 2^11 was 0.550100874
Maximum between 2^11 and 2^12 was 0.547462893
Maximum between 2^12 and 2^13 was 0.544144748
Maximum between 2^13 and 2^14 was 0.542442709
Maximum between 2^14 and 2^15 was 0.540071098
Maximum between 2^15 and 2^16 was 0.538784021
Maximum between 2^16 and 2^17 was 0.537043657
Maximum between 2^17 and 2^18 was 0.536020068
Maximum between 2^18 and 2^19 was 0.534645431
Maximum between 2^19 and 2^20 was 0.53377923

```




## Oforth



```Oforth
: hofstadter(n)
| l i |
   ListBuffer newSize(n) dup add(1) dup add(1) ->l
   n 2 - loop: i [ l at(l last) l at(l size l last - 1+ ) + l add ]
   l dup freeze ;

: hofTask
| h m i |
   2 20 pow ->m
   hofstadter(m) m seq zipWith(#[ tuck asFloat / swap Pair new ]) ->h

   19 loop: i [
      i . "^2 ==>" .
      h extract(2 i pow , 2 i 1+ pow) reduce(#maxKey) println
      ]

   "Mallows number ==>" . h reverse detect(#[ first 0.55 >= ], true) println
;
```


```txt

1^2 ==> [0.666666666666667, 3]
2^2 ==> [0.666666666666667, 6]
3^2 ==> [0.636363636363636, 11]
4^2 ==> [0.608695652173913, 23]
5^2 ==> [0.590909090909091, 44]
6^2 ==> [0.576086956521739, 92]
7^2 ==> [0.567415730337079, 178]
8^2 ==> [0.559459459459459, 370]
9^2 ==> [0.554937413073713, 719]
10^2 ==> [0.550100874243443, 1487]
11^2 ==> [0.547462892647566, 2897]
12^2 ==> [0.544144747863964, 5969]
13^2 ==> [0.542442708780362, 11651]
14^2 ==> [0.540071097511587, 22223]
15^2 ==> [0.538784020584256, 45083]
16^2 ==> [0.537043656999866, 89516]
17^2 ==> [0.536020067811561, 181385]
18^2 ==> [0.534645431078112, 353683]
19^2 ==> [0.533779229963368, 722589]
Mallows number ==> [0.550033579583613, 1489]

```



## Oz

A direct implementation of the recursive definition with explicit memoization using a mutable map (dictionary):

```oz
declare
  local
     Cache = {Dictionary.new}
     Cache.1 := 1
     Cache.2 := 1
  in
     fun {A N}
        if {Not {Dictionary.member Cache N}} then
           Cache.N := {A {A N-1}} + {A N-{A N-1}}
        end
        Cache.N
     end
  end

  Float = Int.toFloat

  for I in 0..19 do
     Range = {List.number {Pow 2 I} {Pow 2 I+1} 1}
     RelativeValues = {Map Range
                       fun {$ N}
                          {Float {A N}}
                          / {Float N}
                       end}
     Maximum = {FoldL RelativeValues Max 0.0}
  in
     {System.showInfo "Max. between 2^"#I#" and 2^"#I+1#": "#Maximum}
  end
```


Output:

```txt

Max. between 2^0 and 2^1: 1.0
Max. between 2^1 and 2^2: 0.66667
Max. between 2^2 and 2^3: 0.66667
Max. between 2^3 and 2^4: 0.63636
Max. between 2^4 and 2^5: 0.6087
Max. between 2^5 and 2^6: 0.59091
Max. between 2^6 and 2^7: 0.57609
Max. between 2^7 and 2^8: 0.56742
Max. between 2^8 and 2^9: 0.55946
Max. between 2^9 and 2^10: 0.55494
Max. between 2^10 and 2^11: 0.5501
Max. between 2^11 and 2^12: 0.54746
Max. between 2^12 and 2^13: 0.54414
Max. between 2^13 and 2^14: 0.54244
Max. between 2^14 and 2^15: 0.54007
Max. between 2^15 and 2^16: 0.53878
Max. between 2^16 and 2^17: 0.53704
Max. between 2^17 and 2^18: 0.53602
Max. between 2^18 and 2^19: 0.53465
Max. between 2^19 and 2^20: 0.53378

```



## PARI/GP


```parigp
HC(n)=my(a=vectorsmall(n));a[1]=a[2]=1;for(i=3,n,a[i]=a[a[i-1]]+a[i-a[i-1]]);a;
maxima(n)=my(a=HC(1<<n),m);vector(n-1,k,m=0;for(i=1<<k+1,1<<(k+1)-1,m=max(m,a[i]/i));m);
forstep(i=#a,1,-1,if(a[i]/i>=.55,return(i)))
```

Output:

```txt
%1 = [2/3, 2/3, 7/11, 14/23, 13/22, 53/92, 101/178, 207/370, 399/719, 818/1487, 1586/2897, 3248/5969, 6320/11651, 12002/22223, 24290/45083, 24037/44758, 97226/181385, 189095/353683, 385703/722589]
%2 = 1489
```


## Pascal

tested with freepascal 3.1.1 64 Bit.

```pascal

program HofStadterConway;
const
  Pot2 = 20;// tested with 30 -> 4 GB;
type
  tfeld = array[0..1 shl Pot2] of LongWord;
  tpFeld = ^tFeld;
  tMaxPos = record
              mpMax : double;
              mpValue,
              mpPos : longWord;
            end;
  tArrMaxPos = array[0..Pot2-1] of tMaxPos;
var
  a : tpFeld;
  MaxPos : tArrMaxPos;

procedure Init(a:tpFeld);
var
  n,k: LongWord;
begin
  a^[1]:= 1;
  a^[2]:= 1;
  //a[n] := a[a[n-1]]+a[n-a[n-1]];
  //k := a[n-1]
  k := a^[2];
  For n := 3 to High(a^) do
  Begin
    k := a^[k]+a^[n-k];
    a^[n] := k;
  end;
end;

function GetMax(a:tpFeld;starts,ends:LongWord):tMaxPos;
var
  posMax : LongWord;
  r,
  max : double;
Begin
  posMax:= starts;
  max := 0.0;
  repeat
    r := a^[starts]/ starts;
    IF max < r then
    Begin
      max := r;
      posMax := starts;
    end;
    inc(starts);
  until starts >= ends;
  with GetMax do
  Begin
    mpPos:= posMax;
    mpValue := a^[posMax];
    mpMax:= max;
  end;
end;

procedure SearchMax(a:tpFeld);
var
  ends,idx : LongWord;
begin
  idx := 0;
  ends := 2;
  while ends <=  High(a^) do
  Begin
    MaxPos[idx]:=GetMax(a,ends shr 1,ends);
    ends := 2*ends;
    inc(idx);
  end;
end;

procedure OutputMax;
var
  i : integer;
begin
  For i := Low(MaxPos) to High(MaxPOs)  do
    with MaxPos[i] do
    Begin
      Write('Max between 2^',i:2,' and 2^',i+1:2);
      writeln(mpMax:14:11,' at ',mpPos:9,' value :',mpValue:10);
    end;
  writeln;
end;

function SearchLastPos(a:tpFeld;limit: double):LongInt;
var
  i,l : LongInt;
Begin
  Limit := limit;
  IF (Limit>1.0 ) OR (Limit < 0.5) then
  Begin
    SearchLastPos := -1;
    EXIT;
  end;

  i := 0;
  while (i<=High(MaxPos)) AND  (MaxPos[i].mpMax > Limit) do
    inc(i);
  dec(i);
  l := MaxPos[i].mpPos;
  i := 1 shl (i+1);
  while (l< i) AND (a^[i]/i < limit)  do
    dec(i);
  SearchLastPos := i;
end;

var
  p : Pointer;
  l : double;
Begin
  //using getmem because FPCs new is limited to 2^31-1 Byte for the test 2^30 )
  getmem(p,SizeOf(tfeld));
  a := p;
  Init(a);
  SearchMax(a);
  outputMax;
  l:= 0.55;
  writeln('Mallows number with limit ',l:10:8,' at ',SearchLastPos(a,l));
  freemem(p);
end.
```

;output:

```txt

Max between 2^ 0 and 2^ 1 1.00000000000 at         1 value :         1
Max between 2^ 1 and 2^ 2 0.66666666667 at         3 value :         2
Max between 2^ 2 and 2^ 3 0.66666666667 at         6 value :         4
Max between 2^ 3 and 2^ 4 0.63636363636 at        11 value :         7
Max between 2^ 4 and 2^ 5 0.60869565217 at        23 value :        14
......
Max between 2^16 and 2^17 0.53704365700 at     89516 value :     48074
Max between 2^17 and 2^18 0.53602006781 at    181385 value :     97226
Max between 2^18 and 2^19 0.53464543108 at    353683 value :    189095
Max between 2^19 and 2^20 0.53377922996 at    722589 value :    385703

Mallows number with limit 0.55000000 at 1489


```



## Perl


```Perl
#!/usr/bin/perl
use warnings ;
use strict ;

my $limit = 2 ** 20 ;
my @numbers = ( 0 , 1 , 1 ) ;
my $mallows ;
my $max_i ;
foreach my $i ( 3..$limit ) {
   push ( @numbers , $numbers[ $numbers[ $i - 1 ]] + $numbers[ $i - $numbers[ $i - 1 ] ] ) ;
}
for ( my $rangelimit = 1 ; $rangelimit < 20 ; $rangelimit++ ) {
   my $max = 0 ;
   for ( my $i = 2 ** $rangelimit ; $i < ( 2 ** ( $rangelimit + 1 ) ) ; $i++ ) {
      my $rat = $numbers[ $i ] / $i ;
      $mallows = $i if $rat >= 0.55 ;
      if ( $rat > $max ) {
	 $max = $rat ;
	 $max_i = $i ;
      }
   }
   my $upperlimit = $rangelimit + 1 ;
   print "Between 2 ^ $rangelimit and 2 ^ $upperlimit the maximum value is $max at $max_i !\n" ;
}
print "The prize would have been won at $mallows !\n"

```

Output:

```txt
Between 2 ^ 1 and 2 ^ 2 the maximum value is 0.666666666666667 at 3 !
Between 2 ^ 2 and 2 ^ 3 the maximum value is 0.666666666666667 at 6 !
Between 2 ^ 3 and 2 ^ 4 the maximum value is 0.636363636363636 at 11 !
Between 2 ^ 4 and 2 ^ 5 the maximum value is 0.608695652173913 at 23 !
Between 2 ^ 5 and 2 ^ 6 the maximum value is 0.590909090909091 at 44 !
Between 2 ^ 6 and 2 ^ 7 the maximum value is 0.576086956521739 at 92 !
Between 2 ^ 7 and 2 ^ 8 the maximum value is 0.567415730337079 at 178 !
Between 2 ^ 8 and 2 ^ 9 the maximum value is 0.559459459459459 at 370 !
Between 2 ^ 9 and 2 ^ 10 the maximum value is 0.554937413073713 at 719 !
Between 2 ^ 10 and 2 ^ 11 the maximum value is 0.550100874243443 at 1487 !
Between 2 ^ 11 and 2 ^ 12 the maximum value is 0.547462892647566 at 2897 !
Between 2 ^ 12 and 2 ^ 13 the maximum value is 0.544144747863964 at 5969 !
Between 2 ^ 13 and 2 ^ 14 the maximum value is 0.542442708780362 at 11651 !
Between 2 ^ 14 and 2 ^ 15 the maximum value is 0.540071097511587 at 22223 !
Between 2 ^ 15 and 2 ^ 16 the maximum value is 0.538784020584256 at 45083 !
Between 2 ^ 16 and 2 ^ 17 the maximum value is 0.537043656999866 at 89516 !
Between 2 ^ 17 and 2 ^ 18 the maximum value is 0.536020067811561 at 181385 !
Between 2 ^ 18 and 2 ^ 19 the maximum value is 0.534645431078112 at 353683 !
Between 2 ^ 19 and 2 ^ 20 the maximum value is 0.533779229963368 at 722589 !
The prize would have been won at 1489 !

```



## Perl 6

Note that <tt>@a</tt> is a lazy array, and the Z variants are "zipwith" operators.

```perl6
my $n = 3;
my @a = (0,1,1, -> $p { @a[$p] + @a[$n++ - $p] } ... *);
@a[2**20]; # pre-calculate sequence

my $last55;
for 1..19 -> $power {
    my @range := 2**$power .. 2**($power+1)-1;
    my @ratios = (@a[@range] Z/ @range) Z=> @range;
    my $max = @ratios.max;
    ($last55 = .value if .key >= .55 for @ratios) if $max.key >= .55;
    say $power.fmt('%2d'), @range.min.fmt("%10d"), '..', @range.max.fmt("%-10d"),
        $max.key, ' at ', $max.value;
}
say "Mallows' number would appear to be ", $last55;
```

```txt
 1         2..3         0.666666666666667 at 3
 2         4..7         0.666666666666667 at 6
 3         8..15        0.636363636363636 at 11
 4        16..31        0.608695652173913 at 23
 5        32..63        0.590909090909091 at 44
 6        64..127       0.576086956521739 at 92
 7       128..255       0.567415730337079 at 178
 8       256..511       0.559459459459459 at 370
 9       512..1023      0.554937413073713 at 719
10      1024..2047      0.550100874243443 at 1487
11      2048..4095      0.547462892647566 at 2897
12      4096..8191      0.544144747863964 at 5969
13      8192..16383     0.542442708780362 at 11651
14     16384..32767     0.540071097511587 at 22223
15     32768..65535     0.538784020584256 at 45083
16     65536..131071    0.537043656999866 at 89516
17    131072..262143    0.536020067811561 at 181385
18    262144..524287    0.534645431078112 at 353683
19    524288..1048575   0.533779229963368 at 722589
Mallows' number would appear to be 1489
```



## Phix


```Phix
sequence a = {1,1}

function q(integer n)
    for l=length(a)+1 to n do
        a &= a[a[l-1]]+a[l-a[l-1]]
    end for
    return a[n]
end function

integer mallows = -1, max_n
for p=0 to 19 do
    atom max_an = 0.5
    integer l = power(2,p), h=l*2
    for n=l to h do
        atom an = q(n)/n
        if an>=max_an then
            max_an = an
            max_n = n
        end if
        if an>0.55 then
            mallows = n
        end if
    end for
    printf(1,"Maximum in range %6d to %-7d occurs at %6d: %f\n",{l,h,max_n,max_an})
end for
printf(1,"Mallows number is %d\n",{mallows})
```

In this particular case the for loop of q() only ever iterates 0 or 1 times.
```txt

Maximum in range      1 to 2       occurs at      1: 1.000000
Maximum in range      2 to 4       occurs at      3: 0.666667
Maximum in range      4 to 8       occurs at      6: 0.666667
Maximum in range      8 to 16      occurs at     11: 0.636364
Maximum in range     16 to 32      occurs at     23: 0.608696
Maximum in range     32 to 64      occurs at     44: 0.590909
Maximum in range     64 to 128     occurs at     92: 0.576087
Maximum in range    128 to 256     occurs at    178: 0.567416
Maximum in range    256 to 512     occurs at    370: 0.559459
Maximum in range    512 to 1024    occurs at    719: 0.554937
Maximum in range   1024 to 2048    occurs at   1487: 0.550101
Maximum in range   2048 to 4096    occurs at   2897: 0.547463
Maximum in range   4096 to 8192    occurs at   5969: 0.544145
Maximum in range   8192 to 16384   occurs at  11651: 0.542443
Maximum in range  16384 to 32768   occurs at  22223: 0.540071
Maximum in range  32768 to 65536   occurs at  45083: 0.538784
Maximum in range  65536 to 131072  occurs at  89516: 0.537044
Maximum in range 131072 to 262144  occurs at 181385: 0.536020
Maximum in range 262144 to 524288  occurs at 353683: 0.534645
Maximum in range 524288 to 1048576 occurs at 722589: 0.533779
Mallows number is 1489

```



## PicoLisp


```PicoLisp
(de hofcon (N)
   (cache '(NIL) N
      (if (>= 2 N)
         1
         (+
            (hofcon (hofcon (dec N)))
            (hofcon (- N (hofcon (dec N)))) ) ) ) )

(scl 20)

(de sequence (M)
   (let (Lim 4  Max 0  4k$ 0)
      (for (N 3 (>= M N) (inc N))
         (let V (*/ (hofcon N) 1.0 N)
            (setq Max (max Max V))
            (when (>= V 0.55)
               (setq 4k$ N) )
            (when (= N Lim)
               (prinl
                  "Maximum between " (/ Lim 2)
                  " and " Lim
                  " was " (format Max `*Scl) )
               (inc 'Lim Lim)
               (zero Max) ) ) )
      (prinl
         "Win with " 4k$
         " (the task requests 'n > p' now)" ) ) )

(sequence (** 2 20))
```

Output:

```txt
Maximum between 2 and 4 was 0.66666666666666666667
Maximum between 4 and 8 was 0.66666666666666666667
Maximum between 8 and 16 was 0.63636363636363636364
Maximum between 16 and 32 was 0.60869565217391304348
Maximum between 32 and 64 was 0.59090909090909090909
Maximum between 64 and 128 was 0.57608695652173913043
Maximum between 128 and 256 was 0.56741573033707865169
Maximum between 256 and 512 was 0.55945945945945945946
Maximum between 512 and 1024 was 0.55493741307371349096
Maximum between 1024 and 2048 was 0.55010087424344317418
Maximum between 2048 and 4096 was 0.54746289264756644805
Maximum between 4096 and 8192 was 0.54414474786396381303
Maximum between 8192 and 16384 was 0.54244270878036220067
Maximum between 16384 and 32768 was 0.54007109751158709445
Maximum between 32768 and 65536 was 0.53878402058425570614
Maximum between 65536 and 131072 was 0.53704365699986594575
Maximum between 131072 and 262144 was 0.53602006781156104419
Maximum between 262144 and 524288 was 0.53464543107811232092
Maximum between 524288 and 1048576 was 0.53377922996336783427
Win with 1489 (the task requests 'n > p' now)
```



## PL/I


```PL/I

/* First part: */

declare L (10000) fixed static initial ((1000) 0);
L(1), L(2) = 1;
do i = 3 to 10000;
   k = L(i);
   L(i) = L(i-k) + L(1+k);
end;

```



## PureBasic


```PureBasic
If OpenConsole()
  Define.i upperlim, i=1, k1=2, n=3, v=1
  Define.d Maximum
  Print("Enter limit (ENTER gives 2^20=1048576): "): upperlim=Val(Input())
  If upperlim<=0: upperlim=1048576: EndIf
  Dim tal(upperlim)
  If ArraySize(tal())=-1
    PrintN("Could not allocate needed memory!"): Input(): End
  EndIf
  tal(1)=1: tal(2)=1
  While n<=upperlim
    v=tal(v)+tal(n-v)
    tal(n)=v
    If Maximum<(v/n): Maximum=v/n: EndIf
    If Not n&k1
      PrintN("Maximum between 2^"+Str(i)+" and 2^"+Str(i+1)+" was "+StrD(Maximum,6))
      Maximum=0.0
      i+1
    EndIf
    k1=n
    n+1
  Wend

  Print(#CRLF$+"Press ENTER to exit."): Input()
  CloseConsole()
EndIf
```



## Python



```python
from __future__ import division

def maxandmallows(nmaxpower2):
    nmax = 2**nmaxpower2
    mx = (0.5, 2)
    mxpow2 = []
    mallows = None

    # Hofstadter-Conway sequence starts at hc[1],
    # hc[0] is not part of the series.
    hc = [None, 1, 1]

    for n in range(2, nmax + 1):
        ratio = hc[n] / n
        if ratio > mx[0]:
            mx = (ratio, n)
        if ratio >= 0.55:
            mallows = n
        if ratio == 0.5:
            print("In the region %7i < n <= %7i: max a(n)/n = %6.4f at  n = %i" %
		  (n//2, n, mx[0], mx[1]))
            mxpow2.append(mx[0])
            mx = (ratio, n)
        hc.append(hc[hc[n]] + hc[-hc[n]])

    return hc, mallows if mxpow2 and mxpow2[-1] < 0.55 and n > 4 else None

if __name__ == '__main__':
    hc, mallows = maxandmallows(20)
    if mallows:
        print("\nYou too might have won $1000 with the mallows number of %i" % mallows)

```


''Sample output''

```txt
In the region       1 < n <=       2: max a(n)/n = 0.5000 at  n = 2
In the region       2 < n <=       4: max a(n)/n = 0.6667 at  n = 3
In the region       4 < n <=       8: max a(n)/n = 0.6667 at  n = 6
In the region       8 < n <=      16: max a(n)/n = 0.6364 at  n = 11
In the region      16 < n <=      32: max a(n)/n = 0.6087 at  n = 23
In the region      32 < n <=      64: max a(n)/n = 0.5909 at  n = 44
In the region      64 < n <=     128: max a(n)/n = 0.5761 at  n = 92
In the region     128 < n <=     256: max a(n)/n = 0.5674 at  n = 178
In the region     256 < n <=     512: max a(n)/n = 0.5595 at  n = 370
In the region     512 < n <=    1024: max a(n)/n = 0.5549 at  n = 719
In the region    1024 < n <=    2048: max a(n)/n = 0.5501 at  n = 1487
In the region    2048 < n <=    4096: max a(n)/n = 0.5475 at  n = 2897
In the region    4096 < n <=    8192: max a(n)/n = 0.5441 at  n = 5969
In the region    8192 < n <=   16384: max a(n)/n = 0.5424 at  n = 11651
In the region   16384 < n <=   32768: max a(n)/n = 0.5401 at  n = 22223
In the region   32768 < n <=   65536: max a(n)/n = 0.5388 at  n = 45083
In the region   65536 < n <=  131072: max a(n)/n = 0.5370 at  n = 89516
In the region  131072 < n <=  262144: max a(n)/n = 0.5360 at  n = 181385
In the region  262144 < n <=  524288: max a(n)/n = 0.5346 at  n = 353683
In the region  524288 < n <= 1048576: max a(n)/n = 0.5338 at  n = 722589

You too might have won $1000 with the mallows number of 1489
```

If you don't create enough terms in the sequence, no mallows number is returned.


## R

A memoizing function to compute individual elements of the sequence could be written like this:


```r
f = local(
   {a = c(1, 1)
    function(n)
       {if (is.na(a[n]))
            a[n] <<- f(f(n - 1)) + f(n - f(n - 1))
        a[n]}})
```


But a more straightforward way to get the local maxima and the Mallows point is to begin by generating as much of the sequence as we need.


```r
hofcon = c(1, 1, rep(NA, 2^20 - 2))
for (n in 3 : (2^20))
  {hofcon[n] =
       hofcon[hofcon[n - 1]] +
       hofcon[n - hofcon[n - 1]]}
```


We can now quickly finish the task with vectorized operations.


```r
ratios = hofcon / seq_along(hofcon)

message("Maxima:")
print(sapply(1 : 20, function(pwr)
   max(ratios[2^(pwr - 1) : 2^pwr])))

message("Prize-winning point:")
print(max(which(ratios >= .55)))
```

```txt

Maxima:
 [1] 1.0000000 0.6666667 0.6666667 0.6363636 0.6086957
 [6] 0.5909091 0.5760870 0.5674157 0.5594595 0.5549374
[11] 0.5501009 0.5474629 0.5441447 0.5424427 0.5400711
[16] 0.5387840 0.5370437 0.5360201 0.5346454 0.5337792
Prize-winning point:
[1] 1489

```



## Racket


The macro define/memoize1 creates an 1-argument procedure and handles all the details about memorization. We use it to define (conway n) as a transcription of the definition.


```Racket
#lang racket/base

(define-syntax-rule (define/memoize1 (proc x) body ...)
  (define proc
    (let ([cache (make-hash)]
          [direct (lambda (x) body ...)])
      (lambda (x)
        (hash-ref! cache x (lambda () (direct x)))))))

(define/memoize1 (conway n)
  (if (< n 3)
      1
      (+ (conway (conway (sub1 n)))
         (conway (- n (conway (sub1 n)))))))
```


The macro for/max1 is like for, but the result is the maximum of the values produced by the body. The result also includes the position of the maximum in the sequence. We use this to find the maximum in each power-of-2-sector.

```Racket
(define-syntax-rule (for/max1 ([i sequence]) body ...)
  (for/fold ([max -inf.0] [arg-max #f]) ([i sequence])
    (define val (begin body ...))
    (if (< max val)
        (values val i)
        (values max arg-max))))

(for ([i (in-range 0 20)])
  (define low-b (expt 2 i))
  (define up-b (expt 2 (add1 i)))
  (define-values (max arg-max) (for/max1 ([k (in-range low-b up-b)])
                                 (/ (conway k) k)))
  (printf "Max. between 2^~a and 2^~a is ~a at ~a ~n" i (add1 i) (real->decimal-string max 5) arg-max))
```



The macro for/prev is like for/and, it stops when it finds the first #f, but the result is previous value produced by the body. We use this to find the first power-of-2-sector that has no ratio avobe .55. The previous result is the Mallows number.

```Racket
(define-syntax-rule (for/prev (sequences ...) body ...)
  (for/fold ([prev #f]) (sequences ...)
    (define val (let () body ...))
    #:break (not val)
    val))

(define mallows (for/prev ([i (in-naturals)])
                   (define low-b (expt 2 i))
                   (define up-b (expt 2 (add1 i)))
                   (for/last ([k (in-range low-b up-b)]
                              #:when (>= (/ (conway k) k) .55))
                     k)))

(printf "Mallows number: ~a~n" mallows)
```


'''Sample Output:'''

```txt
Max. between 2^0 and 2^1 is 1.00000 at 1
Max. between 2^1 and 2^2 is 0.66667 at 3
Max. between 2^2 and 2^3 is 0.66667 at 6
Max. between 2^3 and 2^4 is 0.63636 at 11
Max. between 2^4 and 2^5 is 0.60870 at 23
Max. between 2^5 and 2^6 is 0.59091 at 44
Max. between 2^6 and 2^7 is 0.57609 at 92
Max. between 2^7 and 2^8 is 0.56742 at 178
Max. between 2^8 and 2^9 is 0.55946 at 370
Max. between 2^9 and 2^10 is 0.55494 at 719
Max. between 2^10 and 2^11 is 0.55010 at 1487
Max. between 2^11 and 2^12 is 0.54746 at 2897
Max. between 2^12 and 2^13 is 0.54414 at 5969
Max. between 2^13 and 2^14 is 0.54244 at 11651
Max. between 2^14 and 2^15 is 0.54007 at 22223
Max. between 2^15 and 2^16 is 0.53878 at 45083
Max. between 2^16 and 2^17 is 0.53704 at 89516
Max. between 2^17 and 2^18 is 0.53602 at 181385
Max. between 2^18 and 2^19 is 0.53465 at 353683
Max. between 2^19 and 2^20 is 0.53378 at 722589
Mallows number: 1489
```



## REXX


```rexx
/*REXX program solves the   Hofstadter─Conway  sequence  $10,000   prize  (puzzle).     */
@pref= 'Maximum of    a(n) ÷ n     between '     /*a prologue for the text of message.  */
H.=.;   H.1=1;  H.2=1;   !.=0;     @.=0          /*initialize some REXX variables.      */
win=0
      do k=0  to 20;     p.k=2**k;  maxp=p.k     /*build an array of the powers of two. */
      end   /*k*/
r=1                                              /*R:  is the range of the power of two.*/
      do n=1  for maxp;  if n> p.r  then r=r+1   /*for golf coders, same as: r=r+(n>p.r)*/
      _=H(n)/n;          if _>=.55  then win=n   /*get next seq number; if ≥.55, a win? */
                         if _<=@.r  then iterate /*less than previous? Then keep looking*/
      @.r=_;      !.r=n                          /*@.r and  !.r  are like ginkgo biloba.*/
      end   /*n*/                                /*  ··· or in other words, memoization.*/

      do j=1  for 20;   range= '2**'right(j-1, 2)              "───► 2**"right(  j, 2)
      say @pref  range  '(inclusive)  is '    left(@.j, 9)     "  at  n="right(!.j, 7)
      end   /*j*/
say
say 'The winning number is: '    win             /*and the money shot is  ···           */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
H: procedure expose H.; parse arg z
                        if H.z==.  then do;  m=z-1;   $=H.m;   _=z-$;   H.z=H.$+H._;   end
                        return H.z
```

'''output'''

```txt

Maximum of    a(n) ÷ n     between  2** 0 ───► 2** 1 (inclusive)  is  1           at  n=      1
Maximum of    a(n) ÷ n     between  2** 1 ───► 2** 2 (inclusive)  is  0.6666666   at  n=      3
Maximum of    a(n) ÷ n     between  2** 2 ───► 2** 3 (inclusive)  is  0.6666666   at  n=      6
Maximum of    a(n) ÷ n     between  2** 3 ───► 2** 4 (inclusive)  is  0.6363636   at  n=     11
Maximum of    a(n) ÷ n     between  2** 4 ───► 2** 5 (inclusive)  is  0.6086956   at  n=     23
Maximum of    a(n) ÷ n     between  2** 5 ───► 2** 6 (inclusive)  is  0.5909090   at  n=     44
Maximum of    a(n) ÷ n     between  2** 6 ───► 2** 7 (inclusive)  is  0.5760869   at  n=     92
Maximum of    a(n) ÷ n     between  2** 7 ───► 2** 8 (inclusive)  is  0.5674157   at  n=    178
Maximum of    a(n) ÷ n     between  2** 8 ───► 2** 9 (inclusive)  is  0.5594594   at  n=    370
Maximum of    a(n) ÷ n     between  2** 9 ───► 2**10 (inclusive)  is  0.5549374   at  n=    719
Maximum of    a(n) ÷ n     between  2**10 ───► 2**11 (inclusive)  is  0.5501008   at  n=   1487
Maximum of    a(n) ÷ n     between  2**11 ───► 2**12 (inclusive)  is  0.5474628   at  n=   2897
Maximum of    a(n) ÷ n     between  2**12 ───► 2**13 (inclusive)  is  0.5441447   at  n=   5969
Maximum of    a(n) ÷ n     between  2**13 ───► 2**14 (inclusive)  is  0.5424427   at  n=  11651
Maximum of    a(n) ÷ n     between  2**14 ───► 2**15 (inclusive)  is  0.5400710   at  n=  22223
Maximum of    a(n) ÷ n     between  2**15 ───► 2**16 (inclusive)  is  0.5387840   at  n=  45083
Maximum of    a(n) ÷ n     between  2**16 ───► 2**17 (inclusive)  is  0.5370436   at  n=  89516
Maximum of    a(n) ÷ n     between  2**17 ───► 2**18 (inclusive)  is  0.5360200   at  n= 181385
Maximum of    a(n) ÷ n     between  2**18 ───► 2**19 (inclusive)  is  0.5346454   at  n= 353683
Maximum of    a(n) ÷ n     between  2**19 ───► 2**20 (inclusive)  is  0.5337792   at  n= 722589

The winning number is:  1489

```



## Ring


```ring

decimals(9)
size = 15
a = list(pow(2,size))
a[1]=1
a[2]=1
power=2
p2=pow(2,power)
peak=0.5
peakpos=0
for n=3 to pow(2,size)
    a[n]=a[a[n-1]]+a[n-a[n-1]]
    r=a[n]/n
    if r>=0.55 mallows=n ok
    if r>peak peak=r peakpos=n ok
    if n=p2
       see "maximum between 2^" + (power - 1) + " and 2^" + power + " is " + peak + " at n=" + peakpos + nl
       power += 1
       p2=pow(2,power)
       peak=0.5 ok
next
see "mallows number is : " + mallows + nl

```

Output:

```txt

maximum between 2^1 and 2^2 is 0.666666667 at n=3
maximum between 2^2 and 2^3 is 0.666666667 at n=6
maximum between 2^3 and 2^4 is 0.636363636 at n=11
maximum between 2^4 and 2^5 is 0.608695652 at n=23
maximum between 2^5 and 2^6 is 0.590909091 at n=44
maximum between 2^6 and 2^7 is 0.576086957 at n=92
maximum between 2^7 and 2^8 is 0.567415730 at n=178
maximum between 2^8 and 2^9 is 0.559459459 at n=370
maximum between 2^9 and 2^10 is 0.554937413 at n=719
maximum between 2^10 and 2^11 is 0.550100874 at n=1487
maximum between 2^11 and 2^12 is 0.547462893 at n=2897
maximum between 2^12 and 2^13 is 0.544144748 at n=5969
maximum between 2^13 and 2^14 is 0.542442709 at n=11651
maximum between 2^14 and 2^15 is 0.540071098 at n=22223
mallows number is : 1489

```



## Ruby


```ruby
class HofstadterConway10000
  def initialize
    @sequence = [nil, 1, 1]
  end

  def [](n)
    raise ArgumentError, "n must be >= 1" if n < 1
    a = @sequence
    a.length.upto(n) {|i| a[i] = a[a[i-1]] + a[i-a[i-1]] }
    a[n]
  end
end

hc = HofstadterConway10000.new

mallows = nil
(1...20).each do |i|
  j = i + 1
  max_n, max_v = -1, -1
  (2**i .. 2**j).each do |n|
    v = hc[n].to_f / n
    max_n, max_v = n, v if v > max_v
    # Mallows number
    mallows = n if v >= 0.55
  end
  puts "maximum between 2^%2d and 2^%2d occurs at%7d: %.8f" % [i, j, max_n, max_v]
end

puts "the mallows number is #{mallows}"
```


```txt

maximum between 2^ 1 and 2^ 2 occurs at      3: 0.66666667
maximum between 2^ 2 and 2^ 3 occurs at      6: 0.66666667
maximum between 2^ 3 and 2^ 4 occurs at     11: 0.63636364
maximum between 2^ 4 and 2^ 5 occurs at     23: 0.60869565
maximum between 2^ 5 and 2^ 6 occurs at     44: 0.59090909
maximum between 2^ 6 and 2^ 7 occurs at     92: 0.57608696
maximum between 2^ 7 and 2^ 8 occurs at    178: 0.56741573
maximum between 2^ 8 and 2^ 9 occurs at    370: 0.55945946
maximum between 2^ 9 and 2^10 occurs at    719: 0.55493741
maximum between 2^10 and 2^11 occurs at   1487: 0.55010087
maximum between 2^11 and 2^12 occurs at   2897: 0.54746289
maximum between 2^12 and 2^13 occurs at   5969: 0.54414475
maximum between 2^13 and 2^14 occurs at  11651: 0.54244271
maximum between 2^14 and 2^15 occurs at  22223: 0.54007110
maximum between 2^15 and 2^16 occurs at  45083: 0.53878402
maximum between 2^16 and 2^17 occurs at  89516: 0.53704366
maximum between 2^17 and 2^18 occurs at 181385: 0.53602007
maximum between 2^18 and 2^19 occurs at 353683: 0.53464543
maximum between 2^19 and 2^20 occurs at 722589: 0.53377923
the mallows number is 1489

```



## Run BASIC


```runbasic
input "Enter upper limit between 1 and 20 (ENTER 20 gives 2^20):"); uprLim
if uprLim < 1 or uprLim > 20 then uprLim = 20
dim a(2^uprLim)
a(1)	= 1
a(2)	= 1
pow2	= 2
p2	= 2^pow2
p	= 0.5
pPos	= 0
for n	= 3 TO 2^uprLim
   a(n)	= a(a(n-1)) + a(n-a(n-1))
   r	= a(n)/n
   if r >= 0.55 THEN Mallows = n
   if r > p  THEN
     p    = r
     pPos = n
   end if
   if n	= p2 THEN
      print "Maximum between";chr$(9);" 2^";pow2-1;" and 2^";pow2;chr$(9);" is ";p;chr$(9);" at n = ";pPos
      pow2 = pow2 + 1
      p2   = 2^pow2
      p	   = 0.5
   end IF
next n
print "Mallows number is ";Mallows
```


```txt
Enter upper limit between 1 and 20 (ENTER 20 gives 2^20): ?20
Maximum between	 2^1 and 2^2	 is 0.666666698	 at n = 3
Maximum between	 2^2 and 2^3	 is 0.666666698	 at n = 6
Maximum between	 2^3 and 2^4	 is 0.636363601	 at n = 11
Maximum between	 2^4 and 2^5	 is 0.608695602	 at n = 23
Maximum between	 2^5 and 2^6	 is 0.590909051	 at n = 44
Maximum between	 2^6 and 2^7	 is 0.57608695	 at n = 92
Maximum between	 2^7 and 2^8	 is 0.567415714	 at n = 178
Maximum between	 2^8 and 2^9	 is 0.559459447	 at n = 370
Maximum between	 2^9 and 2^10	 is 0.55493741	 at n = 719
Maximum between	 2^10 and 2^11	 is 0.550100851	 at n = 1487
Maximum between	 2^11 and 2^12	 is 0.547462892	 at n = 2897
Maximum between	 2^12 and 2^13	 is 0.544144725	 at n = 5969
Maximum between	 2^13 and 2^14	 is 0.542442655	 at n = 11651
Maximum between	 2^14 and 2^15	 is 0.540071058	 at n = 22223
Maximum between	 2^15 and 2^16	 is 0.538784027	 at n = 45083
Maximum between	 2^16 and 2^17	 is 0.537043619	 at n = 89516
Maximum between	 2^17 and 2^18	 is 0.53602004	 at n = 181385
Maximum between	 2^18 and 2^19	 is 0.534645414	 at n = 353683
Maximum between	 2^19 and 2^20	 is 0.533779191	 at n = 722589
Mallows number is 1489
```



## Scala


```scala
object HofstadterConway {
  def pow2(n: Int): Int = (Iterator.fill(n)(2)).product

  def makeHCSequence(max: Int): Seq[Int] =
    (0 to max - 1).foldLeft (Vector[Int]()) { (v, idx) =>
      if (idx <= 1) v :+ 1 else v :+ (v(v(idx - 1) - 1) + v(idx - v(idx - 1)))
    }

  val max = pow2(20)

  val maxSeq = makeHCSequence(max)

  def hcRatio(n: Int, seq: Seq[Int]): Double = seq(n - 1).toDouble / n

  def maximumHCRatioBetween(a: Int, b: Int): (Int, Double) =
    Iterator.range(a, b + 1) map (n => (n, hcRatio(n, maxSeq))) maxBy (_._2)

  lazy val mallowsNumber: Int =
    ((max to 1 by -1) takeWhile (hcRatio(_, maxSeq) < 0.55) last) - 1

  def main(args: Array[String]): Unit = {
    for (n <- 1 to 19) {
      val (value, ratio) = maximumHCRatioBetween(pow2(n), pow2(n+1))
      val message = "Maximum of a(n)/n between 2^%s and 2^%s was %s at %s"
      println(message.format(n, n+1, ratio, value))
    }
    println("Mallow's number = %s".format(mallowsNumber))
  }
}
```

'''Output'''

```txt
Maximum of a(n)/n between 2^1 and 2^2 was 0.6666666666666666 at 3
Maximum of a(n)/n between 2^2 and 2^3 was 0.6666666666666666 at 6
Maximum of a(n)/n between 2^3 and 2^4 was 0.6363636363636364 at 11
Maximum of a(n)/n between 2^4 and 2^5 was 0.6086956521739131 at 23
Maximum of a(n)/n between 2^5 and 2^6 was 0.5909090909090909 at 44
Maximum of a(n)/n between 2^6 and 2^7 was 0.5760869565217391 at 92
Maximum of a(n)/n between 2^7 and 2^8 was 0.5674157303370787 at 178
Maximum of a(n)/n between 2^8 and 2^9 was 0.5594594594594594 at 370
Maximum of a(n)/n between 2^9 and 2^10 was 0.5549374130737135 at 719
Maximum of a(n)/n between 2^10 and 2^11 was 0.5501008742434432 at 1487
Maximum of a(n)/n between 2^11 and 2^12 was 0.5474628926475664 at 2897
Maximum of a(n)/n between 2^12 and 2^13 was 0.5441447478639638 at 5969
Maximum of a(n)/n between 2^13 and 2^14 was 0.5424427087803622 at 11651
Maximum of a(n)/n between 2^14 and 2^15 was 0.5400710975115871 at 22223
Maximum of a(n)/n between 2^15 and 2^16 was 0.5387840205842557 at 45083
Maximum of a(n)/n between 2^16 and 2^17 was 0.5370436569998659 at 89516
Maximum of a(n)/n between 2^17 and 2^18 was 0.5360200678115611 at 181385
Maximum of a(n)/n between 2^18 and 2^19 was 0.5346454310781124 at 353683
Maximum of a(n)/n between 2^19 and 2^20 was 0.5337792299633678 at 722589
Mallow's number = 1489
```



## Scheme



```scheme

(import (scheme base)
        (scheme write)
        (only (srfi 1) iota))

;; maximum size of sequence to consider, as a power of 2
(define *max-power* 20)
(define *size* (expt 2 *max-power*))

;; Task 1: Generate members of the sequence
(define *seq* (make-vector (+ 1 *size*))) ; add 1, to use 1-indexing into sequence

(vector-set! *seq* 1 1)
(vector-set! *seq* 2 1)
(for-each
  (lambda (n)
    (let ((x (vector-ref *seq* (- n 1))))
      (vector-set! *seq* n (+ (vector-ref *seq* x)
                              (vector-ref *seq* (- n x))))))
  (iota (- *size* 2) 3))

;; Task 2: Show maxima of a(n)/n between successive powers of two
(for-each
  (lambda (power)
    (let ((start-idx (+ (expt 2 (- power 1)) 1))
          (end-idx (expt 2 power)))
      (do ((i start-idx (+ 1 i))
           (maximum 0 (max maximum (/ (vector-ref *seq* i)
                                      i))))
        ((> i end-idx)
         (display
           (string-append
             "Maximum between 2^" (number->string (- power 1))
             " and 2^" (number->string power)
             " = " (number->string (inexact maximum))
             "\n"))))))
  (iota (- *max-power* 1) 2))

;; Task 3: Find value of p where a(n)/n < 0.55 for all n > p (in our sequence)
(do ((idx *size* (- idx 1)))
  ((or (zero? idx) ; safety net
       (> (/ (vector-ref *seq* idx) idx)
          0.55))
    (display (string-append "\np=" (number->string idx) "\n"))))

```


```txt

Maximum between 2^1 and 2^2 = 0.6666666666666666
Maximum between 2^2 and 2^3 = 0.6666666666666666
Maximum between 2^3 and 2^4 = 0.6363636363636364
Maximum between 2^4 and 2^5 = 0.6086956521739131
Maximum between 2^5 and 2^6 = 0.5909090909090909
Maximum between 2^6 and 2^7 = 0.5760869565217391
Maximum between 2^7 and 2^8 = 0.5674157303370787
Maximum between 2^8 and 2^9 = 0.5594594594594594
Maximum between 2^9 and 2^10 = 0.5549374130737135
Maximum between 2^10 and 2^11 = 0.5501008742434432
Maximum between 2^11 and 2^12 = 0.5474628926475664
Maximum between 2^12 and 2^13 = 0.5441447478639638
Maximum between 2^13 and 2^14 = 0.5424427087803622
Maximum between 2^14 and 2^15 = 0.5400710975115871
Maximum between 2^15 and 2^16 = 0.5387840205842557
Maximum between 2^16 and 2^17 = 0.5370436569998659
Maximum between 2^17 and 2^18 = 0.5360200678115611
Maximum between 2^18 and 2^19 = 0.5346454310781124
Maximum between 2^19 and 2^20 = 0.5337792299633678

p=1489

```



## Sidef

```ruby
class HofstadterConway10000 {
  has sequence = [nil, 1, 1]
  method term(n {.is_pos}) {
    var a = sequence
    {|i| a[i] = a[a[i-1]]+a[i-a[i-1]] } << a.len..n
    a[n]
  }
}

var hc = HofstadterConway10000()

var mallows = nil
for i in (1..19) {
  var j = i+1
  var (max_n, max_v) = (-1, -1)
  for n in (1<<i .. 1<<j) {
    var v = (hc.term(n) / n)
    (max_n, max_v) = (n, v) if (v > max_v)
    mallows = n if (v >= 0.55)
  }
  say ("maximum between 2^%2d and 2^%2d occurs at%7d: %.8f" % (i, j, max_n, max_v))
}

say "the mallows number is #{mallows}"
```

```txt

maximum between 2^ 1 and 2^ 2 occurs at      3: 0.66666667
maximum between 2^ 2 and 2^ 3 occurs at      6: 0.66666667
maximum between 2^ 3 and 2^ 4 occurs at     11: 0.63636364
maximum between 2^ 4 and 2^ 5 occurs at     23: 0.60869565
maximum between 2^ 5 and 2^ 6 occurs at     44: 0.59090909
maximum between 2^ 6 and 2^ 7 occurs at     92: 0.57608696
maximum between 2^ 7 and 2^ 8 occurs at    178: 0.56741573
maximum between 2^ 8 and 2^ 9 occurs at    370: 0.55945946
maximum between 2^ 9 and 2^10 occurs at    719: 0.55493741
maximum between 2^10 and 2^11 occurs at   1487: 0.55010087
maximum between 2^11 and 2^12 occurs at   2897: 0.54746289
maximum between 2^12 and 2^13 occurs at   5969: 0.54414475
maximum between 2^13 and 2^14 occurs at  11651: 0.54244271
maximum between 2^14 and 2^15 occurs at  22223: 0.54007110
maximum between 2^15 and 2^16 occurs at  45083: 0.53878402
maximum between 2^16 and 2^17 occurs at  89516: 0.53704366
maximum between 2^17 and 2^18 occurs at 181385: 0.53602007
maximum between 2^18 and 2^19 occurs at 353683: 0.53464543
maximum between 2^19 and 2^20 occurs at 722589: 0.53377923
the mallows number is 1489

```



## Swift

```Swift
func doSqnc(m:Int) {
    var aList = [Int](count: m + 1, repeatedValue: 0)
    var k1 = 2
    var lg2 = 1
    var amax:Double = 0
    aList[0] = 1
    aList[1] = 1

    var v = aList[2]

    for n in 2...m {
        let add = aList[v] + aList[n - v]
        aList[n] = add
        v = aList[n]

        if amax < Double(v) * 1.0 / Double(n) {
            amax = Double(v) * 1.0 / Double(n)
        }

        if (k1 & n == 0) {
            println("Maximum between 2^\(lg2) and 2^\(lg2 + 1) was \(amax)")
            amax = 0
            lg2++
        }
        k1 = n
    }
}

doSqnc(1 << 20)
```

```txt

Maximum between 2^1 and 2^2 was 0.666666666666667
Maximum between 2^2 and 2^3 was 0.666666666666667
Maximum between 2^3 and 2^4 was 0.636363636363636
Maximum between 2^4 and 2^5 was 0.608695652173913
Maximum between 2^5 and 2^6 was 0.590909090909091
Maximum between 2^6 and 2^7 was 0.576086956521739
Maximum between 2^7 and 2^8 was 0.567415730337079
Maximum between 2^8 and 2^9 was 0.559459459459459
Maximum between 2^9 and 2^10 was 0.554937413073713
Maximum between 2^10 and 2^11 was 0.550100874243443
Maximum between 2^11 and 2^12 was 0.547462892647566
Maximum between 2^12 and 2^13 was 0.544144747863964
Maximum between 2^13 and 2^14 was 0.542442708780362
Maximum between 2^14 and 2^15 was 0.540071097511587
Maximum between 2^15 and 2^16 was 0.538784020584256
Maximum between 2^16 and 2^17 was 0.537043656999866
Maximum between 2^17 and 2^18 was 0.536020067811561
Maximum between 2^18 and 2^19 was 0.534645431078112
Maximum between 2^19 and 2^20 was 0.533779229963368

```



## Tcl

The routine to return the ''n''<sup>th</sup> member of the sequence.

```tcl
package require Tcl 8.5

set hofcon10k {1 1}
proc hofcon10k n {
    global hofcon10k
    if {$n < 1} {error "n must be at least 1"}
    if {$n <= [llength $hofcon10k]} {
	return [lindex $hofcon10k [expr {$n-1}]]
    }
    while {$n > [llength $hofcon10k]} {
	set i [lindex $hofcon10k end]
	set a [lindex $hofcon10k [expr {$i-1}]]
	# Don't use end-based indexing here; faster to compute manually
	set b [lindex $hofcon10k [expr {[llength $hofcon10k]-$i}]]
	lappend hofcon10k [set c [expr {$a + $b}]]
    }
    return $c
}
```

The code to explore the sequence, looking for maxima in the ratio.

```tcl
for {set p 1} {$p<20} {incr p} {
    set end [expr {2**($p+1)}]
    set maxI 0; set maxV 0
    for {set i [expr {2**$p}]} {$i<=$end} {incr i} {
	set v [expr {[hofcon10k $i] / double($i)}]
	if {$v > $maxV} {set maxV $v; set maxI $i}
    }
    puts "max in 2**$p..2**[expr {$p+1}] at $maxI : $maxV"
}
```

Output:

```txt

max in 2**1..2**2 at 3 : 0.6666666666666666
max in 2**2..2**3 at 6 : 0.6666666666666666
max in 2**3..2**4 at 11 : 0.6363636363636364
max in 2**4..2**5 at 23 : 0.6086956521739131
max in 2**5..2**6 at 44 : 0.5909090909090909
max in 2**6..2**7 at 92 : 0.5760869565217391
max in 2**7..2**8 at 178 : 0.5674157303370787
max in 2**8..2**9 at 370 : 0.5594594594594594
max in 2**9..2**10 at 719 : 0.5549374130737135
max in 2**10..2**11 at 1487 : 0.5501008742434432
max in 2**11..2**12 at 2897 : 0.5474628926475664
max in 2**12..2**13 at 5969 : 0.5441447478639638
max in 2**13..2**14 at 11651 : 0.5424427087803622
max in 2**14..2**15 at 22223 : 0.5400710975115871
max in 2**15..2**16 at 45083 : 0.5387840205842557
max in 2**16..2**17 at 89516 : 0.5370436569998659
max in 2**17..2**18 at 181385 : 0.5360200678115611
max in 2**18..2**19 at 353683 : 0.5346454310781124
max in 2**19..2**20 at 722589 : 0.5337792299633678

```



## VBA

{{trans|Phix}} Function q rewritten to sub.

```vb
Public q() As Long
Sub make_q()
    ReDim q(2 ^ 20)
    q(1) = 1
    q(2) = 1
    Dim l As Long
    For l = 3 To 2 ^ 20
        q(l) = q(q(l - 1)) + q(l - q(l - 1))
    Next l
End Sub

Public Sub hcsequence()
    Dim mallows As Long: mallows = -1
    Dim max_n As Long, n As Long
    Dim l As Long, h As Long
    make_q
    For p = 0 To 19
        max_an = 0.5
        l = 2 ^ p: h = l * 2
        For n = l To h
            an = q(n) / n
            If an >= max_an Then
                max_an = an
                max_n = n
            End If
            If an > 0.55 Then
                mallows = n
            End If
        Next n
        Debug.Print "Maximum in range"; Format(l, "@@@@@@@"); " to"; h; String$(7 - Len(CStr(h)), " ");
        Debug.Print "occurs at"; Format(max_n, "@@@@@@@"); ": "; Format(max_an, "0.000000")
    Next p
    Debug.Print "Mallows number is"; mallows
End Sub
```
```txt
Maximum in range      1 to 2       occurs at      1: 1,000000
Maximum in range      2 to 4       occurs at      3: 0,666667
Maximum in range      4 to 8       occurs at      6: 0,666667
Maximum in range      8 to 16      occurs at     11: 0,636364
Maximum in range     16 to 32      occurs at     23: 0,608696
Maximum in range     32 to 64      occurs at     44: 0,590909
Maximum in range     64 to 128     occurs at     92: 0,576087
Maximum in range    128 to 256     occurs at    178: 0,567416
Maximum in range    256 to 512     occurs at    370: 0,559459
Maximum in range    512 to 1024    occurs at    719: 0,554937
Maximum in range   1024 to 2048    occurs at   1487: 0,550101
Maximum in range   2048 to 4096    occurs at   2897: 0,547463
Maximum in range   4096 to 8192    occurs at   5969: 0,544145
Maximum in range   8192 to 16384   occurs at  11651: 0,542443
Maximum in range  16384 to 32768   occurs at  22223: 0,540071
Maximum in range  32768 to 65536   occurs at  45083: 0,538784
Maximum in range  65536 to 131072  occurs at  89516: 0,537044
Maximum in range 131072 to 262144  occurs at 181385: 0,536020
Maximum in range 262144 to 524288  occurs at 353683: 0,534645
Maximum in range 524288 to 1048576 occurs at 722589: 0,533779
Mallows number is 1489
```


## X86 Assembly

Using FASM syntax.

```asm
; Hofstadter-Conway $10,000 sequence
    call a.memorization
    call Mallows_Number
; ECX is the $1000 #
    int3

a.memorization:
; skip [a] to make it one based
    mov [a+1*4],1
    mov [a+2*4],1
    mov ecx,3
@@:
    mov eax,ecx
    mov edx,[a+(ecx-1)*4] ; a[n-1]
    sub eax,edx           ; n-a[n-1]
    mov eax,[a+eax*4]     ; a[n-a[n-1]]
    add eax,[a+edx*4]     ;+a[a[n-1]]
    mov [a+ecx*4],eax
    inc ecx
    cmp ecx,1 shl 20
    jnz @B
    retn

_0.55 equ ((55 shl 32)/100) ; Floor[55 * 2^N / 100], for N=32

Mallows_Number: ; $5D1
    mov ecx,1 shl 20
@@: dec ecx
    mov edx,[a+ecx*4]
    xor eax,eax
    div ecx
    cmp eax,_0.55 + 1
    jc @B
    retn

a rd 1 shl 20
```



## zkl


```zkl
fcn hofstadterConwaySequence(m){
   a:=List.createLong(m + 1,0);
   a[0]=a[1]=1;
   v,p2,lg2, amax, mallow := a[2],4,1, 0.0, Void;

   foreach n in ([2 .. m]){
      v=a[n]=a[v] + a[n - v];
      f:=1.0*v/n;
      if(f>=0.55) mallow=n;
      amax=amax.max(f);
      if(n==p2){
	 println("Max in [2^%d, 2^%d]: %f".fmt(lg2, lg2+1, amax));
	 amax,p2 = 0.0, (n+1).nextPowerOf2;
	 lg2+=1;
      }
   }
   if(mallow) println("Winning number = ",mallow);
}

hofstadterConwaySequence((2).pow(20));
```

```txt

Max in [2^1, 2^2]: 0.666667
Max in [2^2, 2^3]: 0.666667
Max in [2^3, 2^4]: 0.636364
Max in [2^4, 2^5]: 0.608696
Max in [2^5, 2^6]: 0.590909
Max in [2^6, 2^7]: 0.576087
Max in [2^7, 2^8]: 0.567416
Max in [2^8, 2^9]: 0.559459
Max in [2^9, 2^10]: 0.554937
Max in [2^10, 2^11]: 0.550101
Max in [2^11, 2^12]: 0.547463
Max in [2^12, 2^13]: 0.544145
Max in [2^13, 2^14]: 0.542443
Max in [2^14, 2^15]: 0.540071
Max in [2^15, 2^16]: 0.538784
Max in [2^16, 2^17]: 0.537044
Max in [2^17, 2^18]: 0.536020
Max in [2^18, 2^19]: 0.534645
Max in [2^19, 2^20]: 0.533779
Winning number = 1489

```



## ZX Spectrum Basic

Nine first results.

```zxbasic
10 DIM a(2000)
20 LET a(1)=1: LET a(2)=1
30 LET pow2=2: LET p2=2^pow2
40 LET peak=0.5: LET peakpos=0
50 FOR n=3 TO 2000
60 LET a(n)=a(a(n-1))+a(n-a(n-1))
70 LET r=a(n)/n
80 IF r>0.55 THEN LET Mallows=n
90 IF r>peak THEN LET peak=r: LET peakpos=n
100 IF n=p2 THEN PRINT "Maximum (2^";pow2-1;", 2^";pow2;") is ";peak;" at n=";peakpos: LET pow2=pow2+1: LET p2=2^pow2: LET peak=0.5
110 NEXT n
120 PRINT "Mallows number is ";Mallows
```

