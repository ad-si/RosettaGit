+++
title = "Stream Merge"
description = ""
date = 2019-05-14T23:32:01Z
aliases = []
[extra]
id = 20956
[taxonomies]
categories = []
tags = []
+++

{{task}}
; 2-stream merge
: Read two sorted streams of items from external source (e.g. disk, or network), and write one stream of sorted items to external sink.
: Common algorithm: keep 1 buffered item from each source, select minimal of them, write it, fetch another item from that stream from which the written item was.

; ''N''-stream merge
: The same as above, but reading from   <b> ''N'' </b>   sources.
: Common algorithm: same as above, but keep buffered items and their source descriptors in a [[heap]].


Assume streams are very big. You must not suck them whole in the memory, but read them as streams.





## 360 Assembly

No usage of tricks such as forbiden records in the streams.

```360asm
*        Stream Merge              07/02/2017
STRMERGE CSECT
         USING  STRMERGE,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
         OPEN   (OUTDCB,OUTPUT)    open the output file 
         LA     R6,1               n=1
         LA     R9,FILE            file(n)
LOOPN    C      R6,=A(NN)          do n=1 to nn
         BH     ELOOPN
         L      R2,0(R9)           @DCB
         OPEN   ((R2),INPUT)       open input file # n
         LR     R1,R6              n
         BAL    R14,READ           call read(n)
         LA     R6,1(R6)           n=n+1
         LA     R9,4(R9)           file(n++)
         B      LOOPN              end do n
ELOOPN   BCTR   R6,0               n=n-1
LOOP     SR     R8,R8              lowest=0
         LA     R7,1               k=1
LOOPK    CR     R7,R6              do k=1 to n
         BH     ELOOPK
         LA     R2,RECDEF-1(R7)    @recdef(k)
         CLI    0(R2),X'00'        if not recdef(k)
         BNE    ERECDEF
         LR     R1,R7              k
         BAL    R14,READ           call read(k)
ERECDEF  LR     R1,R7              k
         LA     R2,EOF-1(R1)       @eof(k)
         CLI    0(R2),X'00'        if not eof(k)
         BNE    EEOF
         LTR    R8,R8              if lowest<>0
         BZ     LOWEST0
         LR     R1,R7              k
         SLA    R1,6
         LA     R2,REC-64(R1)      @rec(k)
         CLC    0(64,R2),PG        if rec(k)<y
         BNL    RECLTY
         B      LOWEST0            optimization
RECLTY   B      EEOF
LOWEST0  LR     R1,R7              k
         SLA    R1,6
         LA     R2,REC-64(R1)      @rec(k)
         MVC    PG,0(R2)           y=rec(k)
         LR     R8,R7              lowest=k
EEOF     LA     R7,1(R7)           k=k+1
         B      LOOPK              end do k
ELOOPK   LTR    R8,R8              if lowest=0
         BZ     EXIT               goto exit
         BAL    R14,WRITE          call write
         LR     R1,R8              lowest
         BAL    R14,READ           call read(lowest)
         B      LOOP               
EXIT     LA     R7,1               k=1
         LA     R9,FILE            file(n)
LOOPKC   CR     R7,R6              do k=1 to n
         BH     ELOOPKC
         L      R2,0(R9)           @DCB
         CLOSE  ((R2))             close input file # k
         LA     R7,1(R7)           k=k+1
         LA     R9,4(R9)           file(n++)
         B      LOOPKC             end do k
ELOOPKC  CLOSE  (OUTDCB)           close output
         L      R13,4(0,R13)       epilog 
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
*------- ----   ----------------------------------------
READ     LR     R4,R1              z
         LA     R2,RECDEF-1(R1)    @recdef(z)
         MVI    0(R2),X'00'        recdef(z)=false
         LA     R2,EOF-1(R1)       @eof(z) 
         CLI    0(R2),X'00'        if not eof(z) 
         BNE    EOFZ
         LR     R1,R4              z
         SLA    R1,6
         LA     R3,REC-64(R1)      @rec(z)
         LR     R5,R4              z
         SLA    R5,2
         LA     R9,FILE-4(R5)      @file(z)
         L      R5,0(R9)           @DCB
         GET    (R5),(R3)          read record
         LA     R2,RECDEF-1(R4)    @recdef(z)
         MVI    0(R2),X'01'        recdef(z)=true
EOFZ     BR     R14                return
INEOF    LA     R2,EOF-1(R4)       @eof(z)
         MVI    0(R2),X'01'        eof(z)=true
         B      EOFZ
*------- ----   ----------------------------------------
WRITE    LR     R1,R8              lowest
         SLA    R1,6
         LA     R2,REC-64(R1)      @rec(lowest)
         PUT    OUTDCB,(R2)        write record
         BR     R14                return
*        ----   ----------------------------------------
IN1DCB   DCB   DSORG=PS,MACRF=PM,DDNAME=IN1DD,LRECL=64,                *
               RECFM=FT,EODAD=INEOF
IN2DCB   DCB   DSORG=PS,MACRF=PM,DDNAME=IN2DD,LRECL=64,                *
               RECFM=FT,EODAD=INEOF
IN3DCB   DCB   DSORG=PS,MACRF=PM,DDNAME=IN3DD,LRECL=64,                *
               RECFM=FT,EODAD=INEOF
IN4DCB   DCB   DSORG=PS,MACRF=PM,DDNAME=IN4DD,LRECL=64,                *
               RECFM=FT,EODAD=INEOF
OUTDCB   DCB   DSORG=PS,MACRF=PM,DDNAME=OUTDD,LRECL=64,                *
               RECFM=FT
FILE     DC     A(IN1DCB,IN2DCB,IN3DCB,IN4DCB)
NN       EQU    (*-FILE)/4
EOF      DC     (NN)X'00'
RECDEF   DC     (NN)X'00'
REC      DS     (NN)CL64
PG       DS     CL64
         YREGS
         END    STRMERGE
```

{{in}}
<pre style="height:20ex">
--File 1:
Line 001                                                     
Line 008                                                     
Line 017                                                     
--File 2:
Line 019                                                     
Line 033                                                     
Line 044                                                     
Line 055                                                     
--File 3:
Line 019                                                     
Line 029                                                     
Line 039                                                     
--File 4:
Line 023                                                     
Line 030                                                     

```

{{out}}
<pre style="height:20ex">
Line 001
Line 008
Line 017
Line 019
Line 019
Line 023
Line 029
Line 030
Line 033
Line 039
Line 044
Line 055

```



## ALGOL 68

NB, all the files (including the output files) must exist before running this. The output files are overwritten with the merged records.

```algol68
# merge a number of input files to an output file                             #
PROC mergenf = ( []REF FILE inf, REF FILE out )VOID:
     BEGIN
        INT        eof count := 0;
        BOOL       at eof    := FALSE;
        []REF FILE inputs     = inf[ AT 1 ];
        INT   number of files = UPB inputs;
        [ number of files ]BOOL eof;
        [ number of files ]STRING line;
        FOR f TO number of files DO
            eof[ f ] := FALSE;
            on logical file end( inf[ f ], ( REF FILE f )BOOL:
                                           BEGIN
                                               # note that we reached EOF on the latest read #
                                               # and return TRUE so processing can continue #
                                               at eof := TRUE
                                           END
                               )
        OD;
        # read a line from one of the input files                              #
        PROC read line = ( INT file number )VOID:
             BEGIN
                 at eof := FALSE;
                 get( inputs[ file number ], ( line[ file number ], newline ) );
                 eof[ file number ] := at eof;
                 IF at eof THEN
                     # reached eof on this file                                #
                     eof count +:= 1
                 FI
             END; # read line #
        # get the first line from each input file                              #
        FOR f TO number of files DO read line( f ) OD;
        # merge the files                                                      #
        WHILE eof count < number of files DO
            # find the lowest line in the current set                          #
            INT    low pos     := 0;
            STRING low line    := "";
            BOOL   first file  := TRUE;
            FOR file pos TO number of files DO
                IF eof[ file pos ] THEN
                    # file is at eof - ignore it                               #
                    SKIP
                ELIF first file THEN
                    # this is the first file not at eof                        #
                    low pos    := file pos;
                    low line   := line[ file pos ];
                    first file := FALSE
                ELIF line[ file pos ] < low line THEN
                    # this line is lower than the previous one                 #
                    low pos    := file pos;
                    low line   := line[ file pos ]
                FI
            OD;
            # write the record from the lowest file and get the next record    #
            # from it                                                          #
            put( out, ( line[ low pos ], newline ) );
            read line( low pos )
        OD
     END; # mergenf #

# merges the files named in input list, the results are written to the file     #
# named output name                                                             #
# the output file must already exist and will be overwritten                    #
PROC mergen = ( []STRING input list, STRING output name )VOID:
     BEGIN
        []STRING inputs       = input list[ AT 1 ];
        INT number of files   = UPB inputs;
        [ number of files ]REF FILE inf;
        # open the input files                                                  #
        FOR f TO number of files DO
             inf[ f ] := LOC FILE;
             IF  open( inf[ f ], inputs[ f ], stand in channel ) /= 0
             THEN
                 # failed to open the input file #
                 print( (  "Unable to open """ + input list[ f ] + """", newline ) );
                 stop
             FI
        OD;
        # open the output file (which must already exist & will be overwritten) #
        IF FILE output file;
           open( output file, output name, stand out channel ) /= 0
        THEN
            # failed to open the output file #
            print( (  "Unable to open """ + output name + """", newline ) );
            stop
        ELSE
            # files opened OK, merge them #
            mergenf( inf, output file );
            # close the files #
            close( output file );
            FOR f TO number of files DO close( inf[ f ] ) OD
        FI
     END; # mergen #

# merges the two files in1 and in2 to output file #
PROC merge2f = ( REF FILE in1, REF FILE in2, REF FILE output file )VOID: mergenf( ( in1, in2 ), output file );

# merges the two files named in1 and in2 to the file named output file #
PROC merge2 = ( STRING in1, STRING in2, STRING output file )VOID: mergen( ( in1, in2 ), output file );

# test the file merge #
merge2(   "in1.txt", "in2.txt",                         "out2.txt" );
mergen( ( "in1.txt", "in2.txt", "in3.txt", "in4.txt" ), "outn.txt" )
```

{{out}}

```txt


```



## ATS


```ATS

(* ****** ****** *)
//
// This is a memory-clean implementation:
// Every byte of allocated memory is freed
// before the program exits.
//
(* ****** ****** *)
//
#include
"share/atspre_define.hats"
#include
"share/atspre_staload.hats"
//
(*
#include
"share/HATS/atspre_staload_libats_ML.hats"
*)
//
(* ****** ****** *)

staload UN = $UNSAFE

(* ****** ****** *)

fun
streamize_fileptr_line
  (inp: FILEref) = let
//
val lines =
  streamize_fileref_line(inp)
//
val
closing =
$ldelay
(
(
fileref_close(inp);
stream_vt_nil((*void*))
)
,
fileref_close(inp)
)
//
in
//
stream_vt_append(lines, closing)
//
end // end of [streamize_fileptr_line]

(* ****** ****** *)
//
extern
fun
{a:vt@ype}
stream_merge_2
(
xs: stream_vt(a), ys: stream_vt(a)
) : stream_vt(a) // end-of-function
//
(* ****** ****** *)

implement
{a}(*tmp*)
stream_merge_2
  (xs, ys) =
  aux0(xs, ys) where
{
//
fun
aux0
(
xs: stream_vt(a)
,
ys: stream_vt(a)
) : stream_vt(a) = $ldelay
(
case+ !xs of
| ~stream_vt_nil() => !ys
| ~stream_vt_cons(x, xs) => !(aux1(x, xs, ys))
,
(~xs; ~ys)
)
//
and
aux1
(
x0: a
,
xs: stream_vt(a)
,
ys: stream_vt(a)
) : stream_vt(a) = $ldelay
(
case+ !ys of
| ~stream_vt_nil() => stream_vt_cons(x0, xs)
| ~stream_vt_cons(y, ys) => !(aux2(x0, xs, y, ys))
,
(gfree_val<a>(x0); ~xs; ~ys)
)
//
and
aux2
(
x0: a
,
xs: stream_vt(a)
,
y0: a
,
ys: stream_vt(a)
) : stream_vt(a) = $ldelay
(
let
//
var x0 = x0
and y0 = y0
//
val sgn = gcompare_ref_ref<a>(x0, y0)
//
in
//
if
(sgn <= 0)
then stream_vt_cons(x0, aux1(y0, ys, xs))
else stream_vt_cons(y0, aux1(x0, xs, ys))
//
end // end of [let]
,
(gfree_val<a>(x0); gfree_val<a>(y0); ~xs; ~ys)
)
//
} (* end of [stream_merge_2] *)

(* ****** ****** *)

implement
main0(argc, argv) =
{
//
val () = assertloc(argc >= 3)
//
val xs =
(
case+
fileref_open_opt
(
  argv[1], file_mode_r
) of // case+
| ~None_vt() => stream_vt_make_nil()
| ~Some_vt(inp) => streamize_fileptr_line(inp)
) : stream_vt(Strptr1)
//
val ys =
(
case+
fileref_open_opt
(
argv[2], file_mode_r
) of // case+
| ~None_vt() => stream_vt_make_nil()
| ~Some_vt(inp) => streamize_fileptr_line(inp)
) : stream_vt(Strptr1)
//
local
//
implement
(a:vt@ype)
gfree_val<a>(z) =
strptr_free($UN.castvwtp0{Strptr1}(z))
//
implement
(a:vt@ype)
gcompare_ref_ref<a>
  (x, y) =
(
compare($UN.castvwtp1{String}(x), $UN.castvwtp1{String}(y))
) (* end of [gcompare_ref_ref] *)
//
in
//
val zs = stream_merge_2<Strptr1>(xs, ys)
//
end // end of [local]
//
val ((*void*)) =
stream_vt_foreach_cloptr(zs, lam(z) => (println!(z); strptr_free(z)))
//
} (* end of [main0] *)

```



## AWK


```AWK

# syntax: GAWK -f STREAM_MERGE.AWK filename(s) >output
# handles 1 .. N files
#
# variable   purpose
# ---------- -------
# data_arr   holds last record read
# fn_arr     filenames on command line
# fnr_arr    record counts for each file
# status_arr file status: 1=more data, 0=EOF, -1=error
#
BEGIN {
    files = ARGC-1
# get filename, file status and first record
    for (i=1; i<=files; i++) {
      fn_arr[i] = ARGV[i]
      status_arr[i] = getline <fn_arr[i]
      if (status_arr[i] == 1) {
        nr++ # records read
        fnr_arr[i]++
        data_arr[i] = $0
      }
      else if (status_arr[i] < 0) {
        error(sprintf("FILENAME=%s, status=%d, file not found",fn_arr[i],status_arr[i]))
      }
    }
    while (1) { # until EOF in all files
# get file number of the first file still containing data
      fno = 0 # file number
      for (i=1; i<=files; i++) {
        if (status_arr[i] == 1) {
          fno = i
          break
        }
      }
      if (fno == 0) { # EOF in all files
        break
      }
# determine which file has the lowest record in collating sequence
      for (i=1; i<=files; i++) {
        if (status_arr[i] == 1) {
          if (data_arr[i] < data_arr[fno]) {
            fno = i
          }
        }
      }
# output record, get next record, if not EOF then check sequence
      printf("%s\n",data_arr[fno])
      status_arr[fno] = getline <fn_arr[fno] # get next record from this file
      if (status_arr[fno] == 1) {
        nr++
        fnr_arr[fno]++
        if (data_arr[fno] > $0) {
          error(sprintf("FILENAME=%s, FNR=%d, out of sequence",fn_arr[fno],fnr_arr[fno]))
        }
        data_arr[fno] = $0
      }
    }
# EOJ
    printf("input: %d files, %d records, %d errors\n",files,nr,errors) >"con"
    exit(0)
}
function error(message) {
    printf("error: %s\n",message) >"con"
    errors++
}

```


## C


```C
/*
 * Rosetta Code - stream merge in C.
 * 
 * Two streams (text files) with integer numbers, C89, Visual Studio 2010.
 *
 */

#define _CRT_SECURE_NO_WARNINGS
#include <stdio.h>
#include <stdlib.h>

#define GET(N) { if(fscanf(f##N,"%d",&b##N ) != 1) f##N = NULL; }
#define PUT(N) { printf("%d\n", b##N); GET(N) }

void merge(FILE* f1, FILE* f2, FILE* out)
{
    int b1;
    int b2;

    if(f1) GET(1)
    if(f2) GET(2)

    while ( f1 && f2 )
    {
        if ( b1 <= b2 ) PUT(1)
        else            PUT(2)
    }
    while (f1 ) PUT(1)
    while (f2 ) PUT(2)
}

int main(int argc, char* argv[])
{
    if ( argc < 3 || argc > 3 )
    {
        puts("streammerge filename1 filename2");
        exit(EXIT_FAILURE);
    }
    else
        merge(fopen(argv[1],"r"),fopen(argv[2],"r"),stdout);

    return EXIT_SUCCESS;
}

```



## C++

{{trans|C#}}

```cpp>//#include <functional

#include <iostream>
#include <vector>

template <typename C, typename A>
void merge2(const C& c1, const C& c2, const A& action) {
    auto i1 = std::cbegin(c1);
    auto i2 = std::cbegin(c2);

    while (i1 != std::cend(c1) && i2 != std::cend(c2)) {
        if (*i1 <= *i2) {
            action(*i1);
            i1 = std::next(i1);
        } else {
            action(*i2);
            i2 = std::next(i2);
        }
    }
    while (i1 != std::cend(c1)) {
        action(*i1);
        i1 = std::next(i1);
    }
    while (i2 != std::cend(c2)) {
        action(*i2);
        i2 = std::next(i2);
    }
}

template <typename A, typename C>
void mergeN(const A& action, std::initializer_list<C> all) {
    using I = typename C::const_iterator;
    using R = std::pair<I, I>;

    std::vector<R> vit;
    for (auto& c : all) {
        auto p = std::make_pair(std::cbegin(c), std::cend(c));
        vit.push_back(p);
    }

    bool done;
    R* least;
    do {
        done = true;

        auto it = vit.begin();
        auto end = vit.end();
        least = nullptr;

        // search for the first non-empty range to use for comparison
        while (it != end && it->first == it->second) {
            it++;
        }
        if (it != end) {
            least = &(*it);
        }
        while (it != end) {
            // search for the next non-empty range to use for comaprison
            while (it != end && it->first == it->second) {
                it++;
            }
            if (least != nullptr && it != end
                && it->first != it->second
                && *(it->first) < *(least->first)) {
                // found a smaller value
                least = &(*it);
            }
            if (it != end) {
                it++;
            }
        }
        if (least != nullptr && least->first != least->second) {
            done = false;
            action(*(least->first));
            least->first = std::next(least->first);
        }
    } while (!done);
}

void display(int num) {
    std::cout << num << ' ';
}

int main() {
    std::vector<int> v1{ 0, 3, 6 };
    std::vector<int> v2{ 1, 4, 7 };
    std::vector<int> v3{ 2, 5, 8 };

    merge2(v2, v1, display);
    std::cout << '\n';

    mergeN(display, { v1 });
    std::cout << '\n';

    mergeN(display, { v3, v2, v1 });
    std::cout << '\n';
}
```

{{out}}

```txt
0 1 3 4 6 7
0 3 6
0 1 2 3 4 5 6 7 8
```


=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace StreamMerge {
    class Program {
        static void Merge2<T>(IEnumerable<T> i1, IEnumerable<T> i2, Action<T> output) where T : IComparable {
            IEnumerator<T> e1 = i1.GetEnumerator();
            IEnumerator<T> e2 = i2.GetEnumerator();

            bool hasA = e1.MoveNext();
            bool hasB = e2.MoveNext();
            while (hasA || hasB) {
                if (hasA) {
                    if (hasB) {
                        IComparable a = e1.Current;
                        IComparable b = e2.Current;
                        if (a.CompareTo(b) < 0) {
                            output.Invoke(e1.Current);
                            hasA = e1.MoveNext();
                        }
                        else {
                            output.Invoke(e2.Current);
                            hasB = e2.MoveNext();
                        }
                    }
                    else {
                        output.Invoke(e1.Current);
                        hasA = e1.MoveNext();
                    }
                }
                else if (hasB) {
                    output.Invoke(e2.Current);
                    hasB = e2.MoveNext();
                }
            }
        }

        static void MergeN<T>(Action<T> output, params IEnumerable<T>[] enumerables) where T : IComparable {
            if (enumerables.Length == 0) {
                return;
            }
            if (enumerables.Length == 1) {
                IEnumerator<T> e = enumerables[0].GetEnumerator();
                while (e.MoveNext()) {
                    output.Invoke(e.Current);
                }
                return;
            }

            int count = enumerables.Length;
            IEnumerator<T>[] eArr = new IEnumerator<T>[count];
            bool[] hasN = new bool[count];
            for (int i = 0; i < count; i++) {
                eArr[i] = enumerables[i].GetEnumerator();
                hasN[i] = eArr[i].MoveNext();
            }

            while (hasN.Aggregate(false, (a, b) => a || b)) {
                int index = -1;
                T value = default(T);
                for (int i = 0; i < count; i++) {
                    if (hasN[i]) {
                        if (index == -1) {
                            value = eArr[i].Current;
                            index = i;
                        }
                        else if (eArr[i].Current.CompareTo(value) < 0) {
                            value = eArr[i].Current;
                            index = i;
                        }
                    }
                }

                output.Invoke(value);
                hasN[index] = eArr[index].MoveNext();
            }
        }

        static void Main(string[] args) {
            List<int> a = new List<int>() { 1, 4, 7, 10 };
            List<int> b = new List<int>() { 2, 5, 8, 11 };
            List<int> c = new List<int>() { 3, 6, 9, 12 };

            Merge2(a, b, m => Console.Write("{0} ", m));
            Console.WriteLine();
            MergeN(m => Console.Write("{0} ", m), a, b, c);
            Console.WriteLine();
        }
    }
}
```

{{out}}

```txt
1 2 4 5 7 8 10 11
1 2 3 4 5 6 7 8 9 10 11 12
```



## D


```D
import std.range.primitives;
import std.stdio;

// An output range for writing the elements of the example ranges
struct OutputWriter {
    void put(E)(E e) if (!isInputRange!E) {
        stdout.write(e);
    }
}

void main() {
    import std.range : only;
    merge2(OutputWriter(), only(1,3,5,7), only(2,4,6,8));
    writeln("\n---------------");
    mergeN(OutputWriter(), only(1,4,7), only(2,5,8), only(3,6,9));
    writeln("\n---------------");
    mergeN(OutputWriter(), only(1,2,3));
}

/+ Write the smallest element from r1 and r2 until both ranges are empty +/
void merge2(IN,OUT)(OUT sink, IN r1, IN r2)
if (isInputRange!IN && isOutputRange!(OUT, ElementType!IN)) {
    import std.algorithm : copy;

    while (!r1.empty && !r2.empty) {
        auto a = r1.front;
        auto b = r2.front;
        if (a<b) {
            sink.put(a);
            r1.popFront;
        } else {
            sink.put(b);
            r2.popFront;
        }
    }
    copy(r1, sink);
    copy(r2, sink);
}

/+ Write the smallest element from the sources until all ranges are empty +/
void mergeN(OUT,IN)(OUT sink, IN[] source ...)
if (isInputRange!IN && isOutputRange!(OUT, ElementType!IN)) {
    ElementType!IN value;
    bool done, hasValue;
    int idx;

    do {
        hasValue = false;
        done = true;
        idx = -1;

        foreach(i,r; source) {
            if (!r.empty) {
                if (hasValue) {
                    if (r.front < value) {
                        value = r.front;
                        idx = i;
                    }
                } else {
                    hasValue = true;
                    value = r.front;
                    idx = i;
                }
            }
        }

        if (idx > -1) {
            sink.put(source[idx].front);
            source[idx].popFront;
            done = false;
        }
    } while (!done);
}
```


{{out}}

```txt
12345678
---------------
123456789
---------------
123
```



## Elixir


```elixir
defmodule StreamMerge do
  def merge2(file1, file2), do: mergeN([file1, file2])
  
  def mergeN(files) do
    Enum.map(files, fn fname -> File.open!(fname) end)
    |> Enum.map(fn fd -> {fd, IO.read(fd, :line)} end)
    |> merge_loop
  end
  
  defp merge_loop([]), do: :ok
  defp merge_loop(fdata) do
    {fd, min} = Enum.min_by(fdata, fn {_,head} -> head end)
    IO.write min
    case IO.read(fd, :line) do
      :eof -> File.close(fd)
              List.delete(fdata, {fd, min}) |> merge_loop
      head -> List.keyreplace(fdata, fd, 0, {fd, head}) |> merge_loop
    end
  end
end

filenames = ~w[temp1.dat temp2.dat temp3.dat]
Enum.each(filenames, fn fname ->
  IO.puts "#{fname}: " <> File.read!(fname) |> String.replace("\n", " ")
end)
IO.puts "\n2-stream merge:"
StreamMerge.merge2("temp1.dat", "temp2.dat")
IO.puts "\nN-stream merge:"
StreamMerge.mergeN(filenames)
```


{{out}}
<pre style="height: 64ex; overflow: scroll">
temp1.dat:  1  3  9 14 15 17 28
temp2.dat:  7  8 14 14 23 26 28 29 30
temp3.dat:  9 23 25 29

2-stream merge:
 1
 3
 7
 8
 9
14
14
14
15
17
23
26
28
28
29
30

N-stream merge:
 1
 3
 7
 8
 9
 9
14
14
14
15
17
23
23
25
26
28
28
29
29
30

```



## Fortran

This is a classic problem, but even so, Fortran does not supply a library routine for this. So...
```Fortran
      SUBROUTINE FILEMERGE(N,INF,OUTF)	!Merge multiple inputs into one output.
       INTEGER N	!The number of input files.
       INTEGER INF(*)	!Their unit numbers.
       INTEGER OUTF	!The output file.
       INTEGER L(N)	!The length of each current record.
       INTEGER LIST(0:N)!In sorted order.
       LOGICAL LIVE(N)	!Until end-of-file.
       INTEGER ENUFF		!As ever, how long is a piece of string?
       PARAMETER (ENUFF = 666)	!Perhaps this will suffice.
       CHARACTER*(ENUFF) AREC(N)!One for each input file.
       INTEGER I,IT	!Assistants.
        LIST = 0	!LIST(0) fingers the leader.
        LIVE = .TRUE.	!All files are presumed live.
Charge the battery.
        DO I = 1,N	!Taste each.
          CALL GRAB(I)		!By obtaining the first record.
        END DO		!Also, preparing the LIST.
Chug away.
        DO WHILE(LIST(0).GT.0)	!Have we a leader?
          IT = LIST(0)		!Yes. Which is it?
          WRITE (OUTF,"(A)") AREC(IT)(1:L(IT))	!Send it forth.
          LIST(0) = LIST(IT)	!Head to the leader's follower.
          CALL GRAB(IT)		!Get the next candidate.
        END DO			!Try again.

       CONTAINS	!An assistant, called in two places.
        SUBROUTINE GRAB(IN)	!Get another record.
         INTEGER IN		!From this input file.
         INTEGER IT,P		!Linked-list stepping.
          IF (.NOT.LIVE(IN)) RETURN	!No more grist?
          READ (INF(IN),1,END = 10) L(IN),AREC(IN)(1:MIN(ENUFF,L(IN)))	!Burp.
    1     FORMAT (Q,A)		!Q = "length remaining", obviously.
Consider the place of AREC(IN) in the LIST. Entry LIST(IN) is to be linked back in.
          P = 0		!Finger the head of the LIST.
    2     IT = LIST(P)		!Which supplier is fingered?
          IF (IT.GT.0) THEN	!If we're not at the end,
            IF (AREC(IN)(1:L(IN)).GT.AREC(IT)(1:L(IT))) THEN	!Compare.
              P = IT			!The incomer follows this node.
              GO TO 2			!So, move to IT and check afresh.
            END IF		!So much for the comparison.
          END IF	!The record from supplier IN is to precede that from IT, fingered by LIST(P).
          LIST(IN) = IT		!So, IN's follower is IT.
          LIST(P) = IN		!And P's follower is now IN.
          RETURN	!Done.
   10     LIVE(IN) = .FALSE.	!No further input.
          LIST(IN) = -666	!This will cause trouble if accessed.
        END SUBROUTINE GRAB	!Grab input, and jostle for position.
      END SUBROUTINE FILEMERGE	!Simple...

      PROGRAM MASH
      INTEGER MANY
      PARAMETER (MANY = 4)	!Sufficient?
      INTEGER FI(MANY)
      CHARACTER*(28) FNAME(MANY)
      DATA FNAME/"FileAppend.for","FileChop.for",
     1 "FileExt.for","FileHack.for"/
      INTEGER I,F

      F = 10	!Safely past pre-defined unit numbers.
      OPEN (F,FILE="Merged.txt",STATUS="REPLACE",ACTION="WRITE")	!File for output.
      DO I = 1,MANY	!Go for the input files.
        FI(I) = F + I		!Choose another unit number.
        OPEN (FI(I),FILE=FNAME(I),STATUS="OLD",ACTION="READ")	!Hope.
      END DO		!On to the next.

      CALL FILEMERGE(MANY,FI,F)	!E pluribus unum.

      END	!That was easy.
```

Obviously, there would be variations according to the nature of the data streams being merged, and whatever sort key was involved. For this example, input from disc files will do and the sort key is the entire record's text. This means there is no need to worry over the case where, having written a record from stream S and obtained the next record from stream S, it proves to have equal precedence with the waiting record for some other stream. Which now should take precedence? With entirely-equal records it obviously doesn't matter but if the sort key is only partial then different record content could be deemed equal and then a choice has an effect.

The method is straightforward: with a linked-list of stream source identifiers (here, indices to an array INF of unit numbers, so the values are 1,2,3,...N) ordered by the current record content, send forth the head element and obtain the next record from that stream, inserting its entry into the linked-list according to precedence. There is no requirement that each input stream presents its records in sorted order. The key advantage of the linked-list is that when an input stream runs dry, its entry vanishes from the linked-list, having been unlinked when its record was written out. For the case N = 2, rather than write a special version with maddening compound tests, just use the general routine.

The problem with linked-lists is that each time a new record for stream S is to be positioned, the linked-list has to be searched linearly. One could instead maintain an array XLIST fingering the streams in sorted order, which array allows random access and thus (say) a binary search. However, each time, the entry for S must be removed and XLIST compacted for the search, then, when its position is determined, it must be re-inserted after space has been made. Alternatively, an insertion sort could be used and again, there would be many array accesses.

The source file style is F77 except for the usage of an array having an element zero. One could play about with offsets to achieve the effect with an array starting at one, but F90 standardised the availability of specified lower bounds. A further requirement for F90 is that subroutine FILEMERGE declares arrays of size N, to suit the size of the problem. Older Fortrans do not allow this as standard (despite Algol allowing it from the start in the 1960s) so either the arrays have to be declared "surely big enough" or else they could be supplied as additional parameters by the caller, whose problem that becomes. Similarly, the maximum record size is unknown, so ENUFF = 666 seems "surely big enough", at least for this test. Without the Q format code, annoyances expand for any attempt at generality.

The source for subroutine GRAB is within subroutine FILEMERGE for the convenience in sharing and messing with variables important to both, but not to outsiders. This facility is standard in Algol-following languages but often omitted and was not added to Fortran until F90. In its absence, either more parameters are required for the separate routines, or there will be messing with COMMON storage areas.


## Go

'''Using standard library binary heap for mergeN:'''

```go
package main

import (
    "container/heap"
    "fmt"
    "io"
    "log"
    "os"
    "strings"
)

var s1 = "3 14 15"
var s2 = "2 17 18"
var s3 = ""
var s4 = "2 3 5 7"

func main() {
    fmt.Print("merge2: ")
    merge2(
        os.Stdout,
        strings.NewReader(s1),
        strings.NewReader(s2))
    fmt.Println()

    fmt.Print("mergeN: ")
    mergeN(
        os.Stdout,
        strings.NewReader(s1),
        strings.NewReader(s2),
        strings.NewReader(s3),
        strings.NewReader(s4))
    fmt.Println()
}

func r1(r io.Reader) (v int, ok bool) {
    switch _, err := fmt.Fscan(r, &v); {
    case err == nil:
        return v, true
    case err != io.EOF:
        log.Fatal(err)
    }
    return
}

func merge2(m io.Writer, s1, s2 io.Reader) {
    v1, d1 := r1(s1)
    v2, d2 := r1(s2)
    var v int
    for d1 || d2 {
        if !d2 || d1 && v1 < v2 {
            v = v1
            v1, d1 = r1(s1)
        } else {
            v = v2
            v2, d2 = r1(s2)
        }
        fmt.Fprint(m, v, " ")
    }
}

type sv struct {
    s io.Reader
    v int
}

type sh []sv

func (s sh) Len() int            { return len(s) }
func (s sh) Less(i, j int) bool  { return s[i].v < s[j].v }
func (s sh) Swap(i, j int)       { s[i], s[j] = s[j], s[i] }
func (p *sh) Push(x interface{}) { *p = append(*p, x.(sv)) }
func (p *sh) Pop() interface{} {
    s := *p
    last := len(s) - 1
    v := s[last]
    *p = s[:last]
    return v
}

func mergeN(m io.Writer, s ...io.Reader) {
    var h sh
    for _, s := range s {
        if v, d := r1(s); d {
            h = append(h, sv{s, v})
        }
    }
    heap.Init(&h)
    for len(h) > 0 {
        p := heap.Pop(&h).(sv)
        fmt.Fprint(m, p.v, " ")
        if v, d := r1(p.s); d {
            heap.Push(&h, sv{p.s, v})
        }
    }
}
```

{{out}}

```txt

merge2: 2 3 14 15 17 18 
mergeN: 2 2 3 3 5 7 14 15 17 18 

```

'''MergeN using package from [[Fibonacci heap]] task:'''

```go
package main

import (
    "fmt"
    "io"
    "log"
    "os"
    "strings"

    "fib"
)

var s1 = "3 14 15"
var s2 = "2 17 18"
var s3 = ""
var s4 = "2 3 5 7"

func main() {
    mergeN(
        os.Stdout,
        strings.NewReader(s1),
        strings.NewReader(s2),
        strings.NewReader(s3),
        strings.NewReader(s4))
    fmt.Println()
}

func r1(r io.Reader) (v int, ok bool) {
    switch _, err := fmt.Fscan(r, &v); {
    case err == nil:
        return v, true
    case err != io.EOF:
        log.Fatal(err)
    }
    return
}

type sv struct {
    s io.Reader
    v int
}

func (i sv) LT(j fib.Value) bool { return i.v < j.(sv).v }

func mergeN(m io.Writer, s ...io.Reader) {
    h := &fib.Heap{}
    for _, s := range s {
        if v, d := r1(s); d {
            h.Insert(sv{s, v})
        }
    }
    for h.Node != nil {
        min, _ := h.ExtractMin()
        p := min.(sv)
        fmt.Fprint(m, p.v, " ")
        if v, d := r1(p.s); d {
            h.Insert(sv{p.s, v})
        }
    }
}
```

{{out}}

```txt

2 2 3 3 5 7 14 15 17 18 

```


== {{header|Haskell}} ==

There is no built-in iterator or stream type for file operations in Haskell. But several such libraries exist.


###  conduit 



```haskell
-- stack runhaskell --package=conduit-extra --package=conduit-merge

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8        as BS
import           Data.Conduit                 (($$), (=$=))
import           Data.Conduit.Binary          (sinkHandle, sourceFile)
import qualified Data.Conduit.Binary          as Conduit
import qualified Data.Conduit.List            as Conduit
import           Data.Conduit.Merge           (mergeSources)
import           System.Environment           (getArgs)
import           System.IO                    (stdout)

main :: IO ()
main = do
    inputFileNames <- getArgs
    let inputs = [sourceFile file =$= Conduit.lines | file <- inputFileNames]
    runResourceT $ mergeSources inputs $$ sinkStdoutLn
  where
    sinkStdoutLn = Conduit.map (`BS.snoc` '\n') =$= sinkHandle stdout
```


See implementation in https://github.com/cblp/conduit-merge/blob/master/src/Data/Conduit/Merge.hs


###  pipes 



```haskell
-- stack runhaskell --package=pipes-safe --package=pipes-interleave

import Pipes              (runEffect, (>->))
import Pipes.Interleave   (interleave)
import Pipes.Prelude      (stdoutLn)
import Pipes.Safe         (runSafeT)
import Pipes.Safe.Prelude (readFile)
import Prelude            hiding (readFile)
import System.Environment (getArgs)

main :: IO ()
main = do
    sourceFileNames <- getArgs
    let sources = map readFile sourceFileNames
    runSafeT . runEffect $ interleave compare sources >-> stdoutLn
```


See implementation in https://github.com/bgamari/pipes-interleave/blob/master/Pipes/Interleave.hs


## Java


```Java
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

public class StreamMerge {
    private static <T extends Comparable<T>> void merge2(Iterator<T> i1, Iterator<T> i2) {
        T a = null, b = null;

        while (i1.hasNext() || i2.hasNext()) {
            if (null == a && i1.hasNext()) {
                a = i1.next();
            }
            if (null == b && i2.hasNext()) {
                b = i2.next();
            }

            if (null != a) {
                if (null != b) {
                    if (a.compareTo(b) < 0) {
                        System.out.print(a);
                        a = null;
                    } else {
                        System.out.print(b);
                        b = null;
                    }
                } else {
                    System.out.print(a);
                    a = null;
                }
            } else if (null != b) {
                System.out.print(b);
                b = null;
            }
        }

        if (null != a) {
            System.out.print(a);
        }
        if (null != b) {
            System.out.print(b);
        }
    }

    @SuppressWarnings("unchecked")
    @SafeVarargs
    private static <T extends Comparable<T>> void mergeN(Iterator<T>... iter) {
        Objects.requireNonNull(iter);
        if (iter.length == 0) {
            throw new IllegalArgumentException("Must have at least one iterator");
        }

        Object[] pa = new Object[iter.length];
        boolean done;

        do {
            done = true;

            for (int i = 0; i < iter.length; i++) {
                Iterator<T> t = iter[i];
                if (null == pa[i] && t.hasNext()) {
                    pa[i] = t.next();
                }
            }

            T min = null;
            int idx = -1;
            for (int i = 0; i < pa.length; ++i) {
                T t = (T) pa[i];
                if (null != t) {
                    if (null == min) {
                        min = t;
                        idx = i;
                        done = false;
                    } else if (t.compareTo(min) < 0) {
                        min = t;
                        idx = i;
                        done = false;
                    }
                }
            }
            if (idx != -1) {
                System.out.print(min);
                pa[idx] = null;
            }
        } while (!done);
    }

    public static void main(String[] args) {
        List<Integer> l1 = List.of(1, 4, 7, 10);
        List<Integer> l2 = List.of(2, 5, 8, 11);
        List<Integer> l3 = List.of(3, 6, 9, 12);

        merge2(l1.iterator(), l2.iterator());
        System.out.println();

        mergeN(l1.iterator(), l2.iterator(), l3.iterator());
        System.out.println();
        System.out.flush();
    }
}
```

{{out}}

```txt
1245781011
123456789101112
```



## Julia

{{trans|C}}
The IOStream type in Julia encompasses any data stream, including file I/O and TCP/IP. The IOBuffer used here maps a stream to a buffer in memory, and so allows an easy simulation of two streams without opening files.

```Julia

function merge(stream1, stream2, T=Char)
    if !eof(stream1) && !eof(stream2)
        b1 = read(stream1, T)
        b2 = read(stream2, T)
        while !eof(stream1) && !eof(stream2)
            if b1 <= b2
                print(b1)
                if !eof(stream1)
                    b1 = read(stream1, T)
                end
            else
                print(b2)
                if !eof(stream2)
                    b2 = read(stream2, T)
                end
            end
        end
        while !eof(stream1)
            print(b1)
            b1 = read(stream1, T)
        end
        print(b1)
        while !eof(stream2)
            print(b2)
            b2 = read(stream2, T)
        end
        print(b2)
    end
end

const halpha1 = "acegikmoqsuwy"
const halpha2 = "bdfhjlnprtvxz"
const buf1 = IOBuffer(halpha1)
const buf2 = IOBuffer(halpha2)

merge(buf1, buf2, Char)
println("\nDone.")


```
{{output}}
```txt

abcdefghijklmnopqrstuvwyxz
Done.

```



## Kotlin

Uses the same data as the REXX entry. As Kotlin lacks a Heap class, when merging N files, we use a nullable MutableList instead. All comparisons are text based even when the files contain nothing but numbers.

```scala
// version 1.2.21

import java.io.File

fun merge2(inputFile1: String, inputFile2: String, outputFile: String) {
    val file1 = File(inputFile1)
    val file2 = File(inputFile2)
    require(file1.exists() && file2.exists()) { "Both input files must exist" }
    val reader1 = file1.bufferedReader()
    val reader2 = file2.bufferedReader()
    val writer  = File(outputFile).printWriter()
    var line1 = reader1.readLine()
    var line2 = reader2.readLine()
    while (line1 != null && line2 != null) {
        if (line1 <= line2) {
            writer.println(line1)
            line1 = reader1.readLine()
        }
        else {
            writer.println(line2)
            line2 = reader2.readLine()
        }
    }
    while (line1 != null) {
        writer.println(line1)
        line1 = reader1.readLine()
    }
    while (line2 != null) {
        writer.println(line2)
        line2 = reader2.readLine()
    }
    reader1.close()
    reader2.close()
    writer.close()
}

fun mergeN(inputFiles: List<String>, outputFile: String) {
    val files = inputFiles.map { File(it) }
    require(files.all { it.exists() }) { "All input files must exist" }
    val readers = files.map { it.bufferedReader() }
    val writer  = File(outputFile).printWriter()
    var lines = readers.map { it.readLine() }.toMutableList()
    while (lines.any { it != null }) {
        val line = lines.filterNotNull().min()
        val index = lines.indexOf(line)
        writer.println(line)
        lines[index] = readers[index].readLine()
    }
    readers.forEach { it.close() }
    writer.close()
}

fun main(args:Array<String>) {
    val files = listOf("merge1.txt", "merge2.txt", "merge3.txt", "merge4.txt")
    merge2(files[0], files[1], "merged2.txt")
    mergeN(files, "mergedN.txt")
    // check it worked
    println(File("merged2.txt").readText()) 
    println(File("mergedN.txt").readText())
}
```


{{out}}

```txt

1
17
19
33
500
8

1
17
19
1999
2999
2e3
3000
33
3999
500
8

```



## Perl

We make use of an iterator interface which String::Tokenizer provides. Credit: we obtained all the sample text from http://www.lipsum.com/.

```perl
use strict;
use English;
use String::Tokenizer;
use Heap::Simple;

my $stream1 = <<"END_STREAM_1";
Integer vel neque ligula. Etiam a ipsum a leo eleifend viverra sit amet ac
arcu. Suspendisse odio libero, ullamcorper eu sem vitae, gravida dignissim
ipsum. Aenean tincidunt commodo feugiat. Nunc viverra dolor a tincidunt porta.
Ut malesuada quis mauris eget vestibulum. Fusce sit amet libero id augue mattis
auctor et sit amet ligula.
END_STREAM_1

my $stream2 = <<"END_STREAM_2";
In luctus odio nulla, ut finibus elit aliquet in. In auctor vitae purus quis
tristique. Mauris sed erat pulvinar, venenatis lectus auctor, malesuada neque.
Integer a hendrerit tortor. Suspendisse aliquet pellentesque lorem, nec tincidunt
arcu aliquet non. Phasellus eu diam massa. Integer vitae volutpat augue. Nulla
condimentum consectetur ante, ut consequat lectus suscipit eget.
END_STREAM_2

my $stream3 = <<"END_STREAM_3";
In hendrerit eleifend mi nec ultricies. Vestibulum euismod, tellus sit amet
eleifend ultrices, velit nisi dignissim lectus, non vestibulum sem nisi sed mi.
Nulla scelerisque ut purus sed ultricies. Donec pulvinar eleifend malesuada. In
viverra faucibus enim a luctus. Vivamus tellus erat, congue quis quam in, lobortis
varius mi. Nulla ante orci, porttitor id dui ac, iaculis consequat ligula.
END_STREAM_3

my $stream4 = <<"END_STREAM_4";
Suspendisse elementum nunc ex, ac pulvinar mauris finibus sed. Ut non ex sed tortor
ultricies feugiat non at eros. Donec et scelerisque est. In vestibulum fringilla
metus eget varius. Aenean fringilla pellentesque massa, non ullamcorper mi commodo
non. Sed aliquam molestie congue. Nunc lobortis turpis at nunc lacinia, id laoreet
ipsum bibendum.
END_STREAM_4

my $stream5 = <<"END_STREAM_5";
Donec sit amet urna nulla. Duis nec consectetur lacus, et viverra ex. Aliquam
lobortis tristique hendrerit. Suspendisse viverra vehicula lorem id gravida.
Pellentesque at ligula lorem. Cras gravida accumsan lacus sit amet tincidunt.
Curabitur quam nisi, viverra vel nulla vel, rhoncus facilisis massa. Aliquam
erat volutpat.
END_STREAM_5

my $stream6 = <<"END_STREAM_6";
Curabitur nec enim eu nisi maximus suscipit rutrum non sem. Donec lobortis nulla
et rutrum bibendum. Duis varius, tellus in commodo gravida, lorem neque finibus
quam, sagittis elementum leo mauris sit amet justo. Sed vestibulum velit eget
sapien bibendum, sit amet porta lorem fringilla. Morbi bibendum in turpis ac
blandit. Mauris semper nibh nec dignissim dapibus. Proin sagittis lacus est.
END_STREAM_6

merge_two_streams(map {String::Tokenizer->new($ARG)->iterator()}
                      ($stream1, $stream2));
merge_N_streams(6, map {String::Tokenizer->new($ARG)->iterator()}
                       ($stream1, $stream2, $stream3,
                        $stream4, $stream5, $stream6));
exit 0;

sub merge_two_streams {
    my ($iter1, $iter2) = @ARG;
    print "Merge of 2 streams:\n";
    while (1) {
        if (!$iter1->hasNextToken() && !$iter2->hasNextToken()) {
            print "\n\n";
            last;
        }
        elsif (!$iter1->hasNextToken()) {
            print $iter2->nextToken(), q{ };
        }
        elsif (!$iter2->hasNextToken()) {
            print $iter1->nextToken(), q{ };
        }
        elsif ($iter1->lookAheadToken() lt $iter2->lookAheadToken()) {
            print $iter1->nextToken(), q{ };
        }
        else {
            print $iter2->nextToken(), q{ };
        }
    }
    return;
}

sub merge_N_streams {
    my $N = shift;
    print "Merge of $N streams:\n";
    my @iters = @ARG;
    my $heap = Heap::Simple->new(order => 'lt', elements => 'Array');
    for (my $i=0; $i<$N; $i++) {
        my $iter = $iters[$i];
        $iter->hasNextToken() or die "Each stream must have >= 1 element";
        $heap->insert([$iter->nextToken(), $i]);
    }
    $heap->count == $N or die "Problem with initial population of heap";
    while (1) {
        my ($token, $iter_idx) = @{ $heap->extract_top };
        print $token, q{ };
        # Attempt to read the next element from the same iterator where we
        # obtained the element we just extracted.
        my $to_insert = _fetch_next_element($iter_idx, $N, @iters);
        if (! $to_insert) {
            print join(q{ }, map {$ARG->[0]} $heap->extract_all), "\n\n";
            last;
        }
        $heap->insert($to_insert);
    }
    return;
}

sub _fetch_next_element {
    my $starting_idx = shift; my $N = shift; my @iters = @ARG;
    # Go round robin through every iterator exactly once, returning the first
    # element on offer.
    my @round_robin_idxs =
        map {$ARG % $N} ($starting_idx .. $starting_idx + $N - 1);
    foreach my $iter_idx (@round_robin_idxs) {
        my $iter = $iters[$iter_idx];
        if ($iter->hasNextToken()) {
            return [$iter->nextToken(), $iter_idx];
        }
    }
    # At this point every iterator has been exhausted.
    return;
}
```

{{out}}

```txt

Merge of 2 streams:
In Integer luctus odio nulla, ut finibus elit aliquet in. In auctor vel neque ligula. Etiam a ipsum a leo eleifend vitae purus quis tristique. Mauris sed erat pulvinar, venenatis lectus auctor, malesuada neque. Integer a hendrerit tortor. Suspendisse aliquet pellentesque lorem, nec tincidunt arcu aliquet non. Phasellus eu diam massa. Integer vitae viverra sit amet ac arcu. Suspendisse odio libero, ullamcorper eu sem vitae, gravida dignissim ipsum. Aenean tincidunt commodo feugiat. Nunc viverra dolor a tincidunt porta. Ut malesuada quis mauris eget vestibulum. Fusce sit amet libero id augue mattis auctor et sit amet ligula. volutpat augue. Nulla condimentum consectetur ante, ut consequat lectus suscipit eget.

Merge of 6 streams:
Curabitur Donec In In Integer Suspendisse elementum hendrerit eleifend luctus mi nec enim eu nec nisi maximus nunc ex, ac odio nulla, pulvinar mauris finibus sed. Ut non ex sed sit amet suscipit rutrum non sem. Donec lobortis nulla et rutrum bibendum. Duis tortor ultricies feugiat non at eros. Donec et scelerisque est. In ultricies. Vestibulum euismod, tellus sit amet eleifend ultrices, urna nulla. Duis nec consectetur lacus, et ut finibus elit aliquet in. In auctor varius, tellus in commodo gravida, lorem neque finibus quam, sagittis elementum leo mauris sit amet justo. Sed vel neque ligula. Etiam a ipsum a leo eleifend velit nisi dignissim lectus, non vestibulum fringilla metus eget varius. Aenean fringilla pellentesque massa, non ullamcorper mi commodo non. Sed aliquam molestie congue. Nunc lobortis turpis at nunc lacinia, id laoreet ipsum bibendum. ex. Aliquam lobortis tristique hendrerit. Suspendisse vestibulum velit eget sapien bibendum, sit amet porta lorem fringilla. Morbi bibendum in turpis ac blandit. Mauris semper nibh nec dignissim dapibus. Proin sagittis lacus est. sit amet ac arcu. Suspendisse odio libero, ullamcorper eu sem vestibulum sem nisi sed mi. Nulla scelerisque ut purus sed ultricies. Donec pulvinar eleifend malesuada. In vitae purus quis tristique. Mauris sed erat pulvinar, venenatis lectus auctor, malesuada neque. Integer a hendrerit tortor. Suspendisse aliquet pellentesque lorem, nec tincidunt arcu aliquet non. Phasellus eu diam massa. Integer vitae vitae, gravida dignissim ipsum. Aenean tincidunt commodo feugiat. Nunc viverra vehicula lorem id gravida. Pellentesque at ligula lorem. Cras gravida accumsan lacus sit amet tincidunt. Curabitur quam nisi, viverra dolor a tincidunt porta. Ut malesuada quis mauris eget vestibulum. Fusce sit amet libero id augue mattis auctor et sit amet ligula. augue. Nulla condimentum consectetur ante, ut consequat lectus suscipit eget. faucibus enim a luctus. Vivamus tellus erat, congue quis quam in, lobortis varius mi. Nulla ante orci, porttitor id dui ac, iaculis consequat ligula. vel nulla vel, rhoncus facilisis massa. Aliquam erat viverra viverra viverra viverra volutpat volutpat.

```



## Perl 6

{{works with|Rakudo|2018.02}}


```perl6
sub merge_streams ( @streams ) {
    my @s = @streams.map({ hash( STREAM => $_, HEAD => .get ) })\
                    .grep({ .<HEAD>.defined });

    return gather while @s {
        my $h = @s.min: *.<HEAD>;
        take $h<HEAD>;
        $h<HEAD> := $h<STREAM>.get
            orelse @s .= grep( { $_ !=== $h } );
    }
}

say merge_streams([ @*ARGS».&open ]);
```



## Phix

Using a priority queue

```Phix
include builtins/pqueue.e  -- (0.8.0+, not yet properly documented)

procedure add(integer fn, pq)
    object line = gets(fn)
    if line=-1 then
        close(fn)
    else
        pq_add({fn,line}, pq)
    end if
end procedure

-- setup (optional/remove if files already exist)
constant data = {"Line 001\nLine 008\nLine 017\n",
                 "Line 019\nLine 033\nLine 044\nLine 055\n",
                 "Line 019\nLine 029\nLine 039\n",
                 "Line 023\nLine 030\n"},
         filenames = {"file1.txt","file2.txt","file3.txt","file4.txt"}
                    -- (or command_line()[3..$] if you prefer)

for i=1 to length(filenames) do
    integer fn = open(filenames[i], "w")
    if fn<0 then crash("cannot open file") end if
    puts(fn, data[i])
    close(fn)
end for

-- initilisation
integer pq = pq_new()
for i=1 to length(filenames) do
    integer fn = open(filenames[i], "r")
    if fn<0 then crash("cannot open file") end if
    add(fn,pq)
end for

-- main loop
while not pq_empty(pq) do
    {integer fn, string line} = pq_pop(pq)
    puts(1,line)
    add(fn, pq)
end while
pq_destroy(pq)

-- cleanup (optional/remove if files already exist)
for i=1 to length(filenames) do
    {} = delete_file(filenames[i])
end for
```

{{out}}

```txt

Line 001
Line 008
Line 017
Line 019
Line 019
Line 023
Line 029
Line 030
Line 033
Line 039
Line 044
Line 055

```



## PicoLisp


```PicoLisp
(de streamMerge @
   (let Heap
      (make
         (while (args)
            (let? Fd (next)
               (if (in Fd (read))
                  (link (cons @ Fd))
                  (close Fd) ) ) ) )
      (make
         (while Heap
            (link (caar (setq Heap (sort Heap))))
            (if (in (cdar Heap) (read))
               (set (car Heap) @)
               (close (cdr (pop 'Heap))) ) ) ) ) )
```


```txt
$ cat a
3 14 15

$ cat b
2 17 18

$ cat c

$ cat d
2 3 5 7
```

Test:

```PicoLisp
(test (2 3 14 15 17 18)
   (streamMerge
      (open "a")
      (open "b") ) )

(test (2 2 3 3 5 7 14 15 17 18)
   (streamMerge
      (open "a")
      (open "b")
      (open "c")
      (open "d") ) )
```

'streamMerge' works with non-numeric data as well, and also - instead of calling
'open' on a file or named pipe - with the results of 'connect' or 'listen' (i.e.
sockets).

== {{header|Python}} ==

Built-in function <code>open</code> opens a file for reading and returns a line-by-line iterator (stream) over the file.

There exists a standard library function <code>heapq.merge</code> that takes any number of sorted stream iterators and merges them into one sorted iterator, using a [[heap]].


```python
import heapq
import sys

sources = sys.argv[1:]
for item in heapq.merge(open(source) for source in sources):
    print(item)
```



## Racket



```racket
;; This module produces a sequence that merges streams in order (by <)
#lang racket/base
(require racket/stream)

(define-values (tl-first tl-rest tl-empty?)
  (values stream-first stream-rest stream-empty?))

(define-struct merged-stream (< ss v ss′)
  #:mutable ; sadly, so we don't have to redo potentially expensive <
  #:methods gen:stream
  [(define (stream-empty? S)
     ;; andmap defined to be true when ss is null
     (andmap tl-empty? (merged-stream-ss S)))

   (define (cache-next-head S)
     (unless (box? (merged-stream-v S))
       (define < (merged-stream-< S))
       (define ss (merged-stream-ss S))
       (define-values (best-f best-i)
         (for/fold ((F #f) (I 0)) ((s (in-list ss)) (i (in-naturals)))
           (if (tl-empty? s) (values F I)
               (let ((f (tl-first s)))
                 (if (or (not F) (< f (unbox F))) (values (box f) i) (values F I))))))       
       (set-merged-stream-v! S best-f)
       (define ss′ (for/list ((s ss) (i (in-naturals)) #:unless (tl-empty? s))
                     (if (= i best-i) (tl-rest s) s)))
       (set-merged-stream-ss′! S ss′))
     S)

   (define (stream-first S)
     (cache-next-head S)
     (unbox (merged-stream-v S)))

   (define (stream-rest S)
     (cache-next-head S)
     (struct-copy merged-stream S [ss (merged-stream-ss′ S)] [v #f]))])

(define ((merge-sequences <) . sqs)
  (let ((strms (map sequence->stream sqs)))
    (merged-stream < strms #f #f)))

;; ---------------------------------------------------------------------------------------------------
(module+ main
  (require racket/string)
  ;; there are file streams and all sorts of other streams -- we can even read lines from strings
  (for ((l ((merge-sequences string<?)
            (in-lines (open-input-string "aardvark
dog
fox"))
            (in-list (string-split "cat donkey elephant"))                            
            (in-port read (open-input-string #<<<
"boy"
"emu"
"monkey"
<
                                             )))))
    (displayln l)))

;; ---------------------------------------------------------------------------------------------------
(module+ test
  (require rackunit)
  (define merge-sequences/< (merge-sequences <))
  
  (check-equal?
   (for/list ((i (in-stream (merge-sequences/< (in-list '(1 3 5)))))) i)
   '(1 3 5))
  ;; in-stream (and in-list) is optional (but may increase performance)
  (check-equal? (for/list ((i (merge-sequences/<))) i) null)
  (check-equal? (for/list ((i (merge-sequences/< '(1 3 5) '(2 4 6)))) i) '(1 2 3 4 5 6))
  (check-equal? (for/list ((i (merge-sequences/< '(1 3 5) '(2 4 6 7 8 9 10)))) i)
                '(1 2 3 4 5 6 7 8 9 10))
  (check-equal? (for/list ((i (merge-sequences/< '(2 4 6 7 8 9 10) '(1 3 5)))) i)
                '(1 2 3 4 5 6 7 8 9 10)))
```


{{out}}


```txt
aardvark
boy
cat
dog
donkey
elephant
emu
fox
monkey
```


== {{header|REXX}} ==

### version 1


```rexx
/**********************************************************************
* Merge 1.txt ... n.txt into m.txt
* 1.txt 2.txt 3.txt 4.txt
* 1     19    1999  2e3
* 17    33    2999  3000
* 8     500   3999
**********************************************************************/
n=4
high='ffff'x
p.=''
Do i=1 To n
  f.i=i'.txt'
  Call get i
  End
Do Forever
  min=high
  Do i=1 To n
    If x.i<<min Then Do    /* avoid numerical comparison */
      imin=i
      min=x.i
      End
    End
  If min<<high Then Do
    Call o x.imin
    Call get imin
    End
  Else Do
    Call lineout oid
    Leave
    End
  End
Exit
get: Procedure Expose f. x. high p.
  Parse Arg ii
  If lines(f.ii)=0 Then
    x.ii=high
  Else Do
    x.ii=linein(f.ii)
    If x.ii<<p.ii Then Do
      Say 'Input file' f.ii 'is not sorted ascendingly'
      Say p.ii 'precedes' x.ii
      Exit
      End
    p.ii=x.ii
    End
  Return
o: Say arg(1)
   Return lineout(oid,arg(1))
```

{{out}}

```txt
1
17
19
1999
2999
2e3
3000
33
3999
500
8
```



### version 2

This REXX version reads   (in numerical order)   ''any''   number of input files in the form of:     <big> nnn.TXT </big>     

and stops reading subsequent   ''new''   input files when it encounters an input file that doesn't exist   (or is empty).

The input files would/should be named:     '''1.TXT     2.TXT     3.TXT     4.TXT     ···'''

No   ''heap''   is needed to keep track of which record was written, nor needs replenishing from its input file. 

```rexx
/*REXX pgm reads sorted files (1.TXT, 2.TXT, ···),  and writes sorted data ───► ALL.TXT */
@.=copies('ff'x, 1e5)                            /*no value should be larger than this. */
     do n=1  until @.n==@.;    call rdr n;   end /*read any number of appropriate files.*/
n=n-1                                            /*adj. N; read from a non─existent file*/
     do forever;               y=@.;   #=0       /*find the lowest value for  N  values.*/
        do k=1  for n                            /*traipse through the stemmed  @ array.*/
        if @.k==@.  then       call rdr k        /*Not defined?  Then read a file record*/
        if @.k<<y   then do;   y=@.k;  #=k;  end /*Lowest so far? Then mark this as min.*/
        end   /*k*/                              /* [↑]  note use of << exact comparison*/
     if #==0  then exit                          /*stick a fork in it,  we're all done. */
     call lineout 'ALL.TXT', @.#;      say @.#   /*write value to a file; also display. */
     call rdr #                                  /*re-populate a value from the # file. */
     end   /*forever*/                           /*keep reading/merging until exhausted.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
rdr: parse arg z;  @.z=@.;   f=z'.TXT';     if lines(f)\==0  then @.z=linein(f);    return
```

{{out|output|text=  is the same as the 1<sup>st</sup> REXX version when using identical input files.}} 




## Ruby


```ruby
def stream_merge(*files)
  fio = files.map{|fname| open(fname)}
  merge(fio.map{|io| [io, io.gets]})
end

def merge(fdata)
  until fdata.empty?
    io, min = fdata.min_by{|_,data| data}
    puts min
    if (next_data = io.gets).nil?
      io.close
      fdata.delete([io, min])
    else
      i = fdata.index{|x,_| x == io}
      fdata[i] = [io, next_data]
    end
  end
end

files = %w(temp1.dat temp2.dat temp3.dat)
files.each do |fname|
  data = IO.read(fname).gsub("\n", " ")
  puts "#{fname}: #{data}"
end
stream_merge(*files)
```


{{out}}

```txt

temp1.dat:  1  3  9 14 15 17 28 
temp2.dat:  7  8 14 14 23 26 28 29 30 
temp3.dat:  9 23 25 29 
 1
 3
 7
 8
 9
 9
14
14
14
15
17
23
23
25
26
28
28
29
29
30

```



## Scala


```scala
def mergeN[A : Ordering](is: Iterator[A]*): Iterator[A] = is.reduce((a, b) => merge2(a, b))

def merge2[A : Ordering](i1: Iterator[A], i2: Iterator[A]): Iterator[A] = {
  merge2Buffered(i1.buffered, i2.buffered)
}

def merge2Buffered[A](i1: BufferedIterator[A], i2: BufferedIterator[A])(implicit ord: Ordering[A]): Iterator[A] = {
  if (!i1.hasNext) {
    i2
  } else if (!i2.hasNext) {
    i1
  } else {
    val nextHead = if (ord.lt(i1.head, i2.head)) {
      Iterator.single(i1.next) 
    } else {
      Iterator.single(i2.next)
    }
    nextHead ++ merge2Buffered(i1, i2)
  }
}
```


Example usage, demonstrating lazyness:


```scala
val i1 = Iterator.tabulate(5) { i =>
  val x = i * 3
  println(s"generating $x")
  x
}

val i2 = Iterator.tabulate(5) { i =>
  val x = i * 3 + 1
  println(s"generating $x")
  x
}

val i3 = Iterator.tabulate(5) { i =>
  val x = i * 3 + 2
  println(s"generating $x")
  x
}

val merged = mergeN(i1, i2, i3)

while (merged.hasNext) {
  val x = merged.next
  println(s"output: $x")
}
```


{{out}}

```txt
generating 0
generating 1
generating 2
output: 0
generating 3
output: 1
generating 4
output: 2
generating 5
output: 3
generating 6
output: 4
generating 7
output: 5
generating 8
output: 6
generating 9
output: 7
generating 10
output: 8
generating 11
output: 9
generating 12
output: 10
generating 13
output: 11
generating 14
output: 12
output: 13
output: 14
```



## Sidef

{{trans|Perl 6}}

```ruby
func merge_streams(streams) {
    var s = streams.map { |stream|
        Pair(stream, stream.readline)
    }.grep {|p| defined(p.value) }

    gather {
        while (s) {
            var p = s.min_by { .value }
            take(p.value)
            p.value = (p.key.readline \\ s.delete_if { _ == p })
        }
    }
}

say merge_streams(ARGV.map {|f| File(f).open_r }).join("\n")
```



## Tcl

Thw below script will merge an arbitrary number of files (which must be already sorted) specified on the command-line.

A careful reader will notice that '''$peeks''' is treated alternately as a dictionary ('''dict set''', '''dict get''') and as a list ('''lsort''', '''lassign'''), exploiting the fact that dictionaries are simply lists of even length.  For large dictionaries this would not be recommended, as it causes [https://wiki.tcl.tk/3033 "shimmering"], but in this example the impact is too small to matter.


```Tcl
#!/usr/bin/env tclsh
proc merge {args} {
    set peeks {}
    foreach chan $args {
        if {[gets $chan peek] > 0} {
            dict set peeks $chan $peek
        }
    }   
    set peeks [lsort -stride 2 -index 1 $peeks]
    while {[dict size $peeks]} {
        set peeks [lassign $peeks chan peek]
        puts $peek
        if {[gets $chan peek] > 0} {
            dict set peeks $chan $peek 
            set peeks [lsort -stride 2 -index 1 $peeks]
        }
    }       
}           

merge {*}[lmap f $::argv {open $f r}]

```


== {{header|UNIX Shell}} ==

 sort --merge source1 source2 sourceN > sink

== {{header|zkl}} ==
This solution uses iterators, doesn't care where the streams orginate and only keeps the head of the stream on hand.

```zkl
fcn mergeStreams(s1,s2,etc){  //-->Walker
   streams:=vm.arglist.pump(List(),fcn(s){ // prime and prune
      if( (w:=s.walker())._next() ) return(w);
      Void.Skip		// stream is dry
   });
   Walker().tweak(fcn(streams){
      if(not streams) return(Void.Stop);  // all streams are dry
      values:=streams.apply("value");	  // head of the streams
      v:=values.reduce('wrap(min,x){ if(min<=x) min else x });
      n:=values.find(v); w:=streams[n]; w._next();  // read next value from min stream
      if(w.atEnd) streams.del(n); // prune empty streams
      v
   }.fp(streams));
}
```

Using infinite streams:

```zkl
w:=mergeStreams([0..],[2..*,2],[3..*,3],T(5));
w.walk(20).println();
```

{{out}}

```txt

L(0,1,2,2,3,3,4,4,5,5,6,6,6,7,8,8,9,9,10,10)

```

Using files:

```zkl
w:=mergeStreams(File("unixdict.txt"),File("2hkprimes.txt"),File("/dev/null"));
do(10){ w.read().print() }
```

{{out}}

```txt

10th
1st
2
2nd
3
3rd
4th
5
5th
6th

```

Using the above example to squirt the merged stream to a file:

```zkl
mergeStreams(File("unixdict.txt"),File("2hkprimes.txt"),File("/dev/null"))
.pump(File("foo.txt","w"));
```

{{out}}

```txt

$ ls -l unixdict.txt 2hkprimes.txt foo.txt 
-rw-r--r-- 1 craigd craigd 1510484 Oct 29  2013 2hkprimes.txt
-rw-r--r-- 1 craigd craigd 1716887 Jun 16 23:34 foo.txt
-rw-r--r-- 1 craigd craigd  206403 Jun 11  2014 unixdict.txt

```

