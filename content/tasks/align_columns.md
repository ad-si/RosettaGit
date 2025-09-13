+++
title = "Align columns"
description = ""
date = 2019-08-11T11:03:41Z
aliases = []
[extra]
id = 3114
[taxonomies]
categories = ["Text processing", "task"]
tags = []
+++

## Task
Given a text file of many lines, where fields within a line
are delineated by a single 'dollar' character, write a program
that aligns each column of fields by ensuring that words in each
column are separated by at least one space.
Further, allow for each word in a column to be either left
justified, right justified, or center justified within its column.

<br clear=all>Use the following text to test your programs:

```txt
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
```


Note that:
# The example input texts lines may, or may not, have trailing dollar characters.
# All columns should share the same alignment.
# Consecutive space characters produced adjacent to the end of lines are insignificant for the purposes of the task.
# Output text will be viewed in a mono-spaced font on a plain text editor or basic terminal.
# The minimum space between columns should be computed from the text and not hard-coded.
# It is ''not'' a requirement to add separating characters between or around columns.





## 11l

{{trans|D}}

```11l
V txt = ‘Given$a$txt$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.’

V parts = txt.split("\n").map(line -> line.rtrim(‘$’).split(‘$’))
V max_widths = [0] * parts[0].len
L(line) parts
   L(word) line
      max_widths[L.index] = max(max_widths[L.index], word.len)

(String, Int -> String) ljust = (s, w) -> s‘’(‘ ’ * (w - s.len))
(String, Int -> String) centr = (s, w) -> (‘ ’ * (w - s.len - (w I/ 2 - s.len I/ 2)))‘’s‘’(‘ ’ * (w I/ 2 - s.len I/ 2))
(String, Int -> String) rjust = (s, w) -> (‘ ’ * (w - s.len))‘’s

L(justify) [ljust, centr, rjust]
   print([‘Left’, ‘Center’, ‘Right’][L.index]‘ column-aligned output:’)
   L(line) parts
      L(word) line
         print(justify(word, max_widths[L.index]), end' ‘ ’)
      print()
   print(‘- ’ * 52)
```



## 360 Assembly


```360asm
*        Align columns             12/04/2019
ALICOL   CSECT
         USING  ALICOL,R13         base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R8,1               i=1
       DO WHILE=(C,R8,LE,=A(NI))   do r=1 to hbound(t)
         LA     R7,1                 j=1
         LA     R6,L'T               i=length(t)
       DO WHILE=(C,R6,GE,=A(1))      do i=length(t) to 1 by -1
         LR     R1,R8                  r
         MH     R1,=AL2(L'T)           ~
         LA     R4,T-L'T(R1)           t(r)
         BCTR   R4,0                   -1
         AR     R4,R6                  +i
         MVC    CI,0(R4)               ci=substr(t(r),i,1)
         CLI    CI,C' '                if ci=' '
         BE     ITERI1                 then iterate i
         CLI    CI,C'$'                if ci='$'
         BE     ITERI1                 then iterate i
         LR     R7,R6                  j=i
         B      LEAVEI1                leave i
ITERI1   BCTR   R6,0                   i--
       ENDDO    ,                    enddo i
LEAVEI1  LR     R1,R8                r
         MH     R1,=AL2(L'T)         ~
         LA     R4,T-L'T(R1)         @t(r)
         LA     R2,WT                @wt
         LR     R5,R7                j
         ICM    R5,B'1000',=C' '     padding
         LA     R3,L'T               length(wt)
         MVCL   R2,R4                wt=substr(t(r),1,j)
         LA     R0,1                 1
         ST     R0,I0                i0=1
         SR     R9,R9                c=0
         LA     R6,1                 i=1
       DO WHILE=(CR,R6,LE,R7)        do i=1 to j
         LA     R4,WT-1                @wt
         AR     R4,R6                  i
         MVC    CI(1),0(R4)            ci=substr(wt,i,1)
       IF   CLI,CI,EQ,C'$' THEN        if ci='$' then
         BAL    R14,SEQ                  call seq
         LR     R2,R6                    i
         LA     R2,1(R2)                 +1
         ST     R2,I0                    i0=i+1
       ENDIF    ,                      endif
         LA     R6,1(R6)               i++
       ENDDO    ,                    enddo i
         BAL    R14,SEQ            call seq
       IF     C,R9,GT,COLS THEN    if c>cols then
         ST     R9,COLS              cols=c
       ENDIF    ,                  endif
         LA     R8,1(R8)             r++
       ENDDO    ,                  enddo r
         LR     R2,R8              r
         BCTR   R2,0               -1
         ST     R2,ROWS            rows=r-1
         LA     R7,1               j=1
       DO WHILE=(C,R7,LE,=A(3))    do j=1 to 3
         XPRNT  =C'--',2             print
         LA     R8,1                 r=1
       DO WHILE=(C,R8,LE,ROWS)       do r=1 to rows
         MVC    PG,=CL120' '           pg=' '
         LA     R0,1                   1
         ST     R0,IB                  ib=1
         LA     R9,1                   c=1
       DO WHILE=(C,R9,LE,COLS)         do c=1 to cols
         LR     R1,R8                    r
         BCTR   R1,0                     -1
         MH     R1,=AL2(NJ)              ~
         AR     R1,R9                    c
         MH     R1,=AL2(L'WOR)           ~
         LA     R4,WOR-L'WOR(R1)         @wor(r,c)
         MVC    W,0(R4)                  w=wor(r,c)
         LA     R6,L'W                   i=length(w)
       DO WHILE=(C,R6,GE,=A(1))          do i=length(w) to 1 by -1
         LA     R4,W-1                     @w
         AR     R4,R6                      i
         MVC    CI,0(R4)                   ci=substr(w,i,1)
         CLI    CI,C' '                    if ci^=' '
         BNE    LEAVEI2                    then goto leavei2;
         BCTR   R6,0                       i--
       ENDDO    ,                        enddo i
LEAVEI2  EQU    *                        ~
       IF   LTR,R6,Z,R6 THEN             if i=0 then
         LA     R10,1                      l=1
       ELSE     ,                        else
         LR     R10,R6                     l=i
       ENDIF    ,                        endif
       IF     C,R7,EQ,=F'1' THEN         if j=1 then
         L      R11,IB                     ibx=ib
       ENDIF    ,                        endif
       IF     C,R7,EQ,=F'2' THEN         if j=2 then
         LR     R1,R9                      c
         SLA    R1,2                       ~
         L      R11,WID-L'WID(R1)          wid(c)
         A      R11,IB                     +ib
         SR     R11,R10                    ibx=ib+wid(c)-l
       ENDIF    ,                        endif
       IF     C,R7,EQ,=F'3' THEN         if j=3 then
         LR     R1,R9                      c
         SLA    R1,2                       ~
         L      R11,WID-L'WID(R1)          wid(c)
         SR     R11,R10                    -l
         SRA    R11,1                      /2
         A      R11,IB                     ibx=ib+(wid(c)-l)/2
       ENDIF    ,                        endif
         LA     R2,PG-1                  @pg
         AR     R2,R11                   +ibx
         LR     R3,R10                   l
         LA     R4,W                     @w
         LR     R5,R10                   l
         MVCL   R2,R4                    substr(pg,ibx,l)=substr(w,1,l)
         LR     R1,R9                    c
         SLA    R1,2                     ~
         L      R2,WID-L'WID(R1)         wid(c)
         A      R2,IB                    +ib
         LA     R2,1(R2)                 +1
         ST     R2,IB                    ib=ib+wid(c)+1
         LA     R9,1(R9)                 c++
       ENDDO    ,                      enddo c
         XPRNT  PG,L'PG                print
         LA     R8,1(R8)               r++
       ENDDO    ,                    enddo r
         LA     R7,1(R7)             j++
       ENDDO    ,                  enddo j
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
SEQ      EQU    *                  --begin seq
         LA     R9,1(R9)           c=c+1
         LR     R10,R6             i
         S      R10,I0             l=i-i0
         LA     R4,WT-1            @wt
         A      R4,I0              +i0
         LR     R5,R10             l
         ICM    R5,B'1000',=C' '   padding
         LR     R1,R8              r
         BCTR   R1,0               -1
         MH     R1,=AL2(NJ)        ~
         AR     R1,R9              +c
         MH     R1,=AL2(L'WOR)     ~
         LA     R2,WOR-L'WOR(R1)   @wor(r,c)
         LA     R3,L'WOR           length(wor)
         MVCL   R2,R4              wor(r,c)=substr(wt,i0,l)
         LR     R1,R9              c
         SLA    R1,2               ~
         L      R2,WID-L'WID(R1)   wid(c)
       IF     CR,R2,LT,R10 THEN    if l>wid(c) then
         LR     R1,R9                c
         SLA    R1,2                 ~
         ST     R10,WID-L'WID(R1)    wid(c)=l
       ENDIF    ,                  endif
         BR     R14                --end seq
NI       EQU    6                  ni
NJ       EQU    12                 nj
T DC CL68'Given$a$text$file$of$many$lines,$where$fields$within$a$line$'
  DC CL68'are$delineated$by$a$single$''dollar''$character,$write$a$progX
               ramm'
  DC CL68'that$aligns$each$column$of$fields$by$ensuring$that$words$in$eX
               ach$'
  DC CL68'column$are$separated$by$at$least$one$space.'
  DC CL68'Further,$allow$for$each$word$in$a$column$to$be$either$left$'
  DC CL68'justified,$right$justified,$or$center$justified$within$its$coX
               lumn.'
WOR      DC     (NI*NJ)CL10' '     wor(ni,nj) char(10)
WID      DC     16F'0'             wid(16)
COLS     DC     F'0'
ROWS     DC     F'0'
WT       DS     CL(L'T)
W        DS     CL(L'WOR)
CI       DS     CL1
I0       DS     F
IB       DS     F
PG       DS     CL120
         REGEQU
         END    ALICOL
```

{{out}}

```txt

--
Given      a          text       file   of     many      lines,     where    fields  within   a      line
are        delineated by         a      single 'dollar'  character, write    a       programm
that       aligns     each       column of     fields    by         ensuring that    words    in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be       either left
justified, right      justified, or     center justified within     its      column.
--
     Given          a       text   file     of      many     lines,    where  fields   within      a line
       are delineated         by      a single  'dollar' character,    write       a programm
      that     aligns       each column     of    fields         by ensuring    that    words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to       be either left
justified,      right justified,     or center justified     within      its column.
--
  Given        a         text     file    of     many      lines,    where   fields   within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    programm
   that      aligns      each    column   of    fields       by     ensuring  that    words     in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to       be    either left
justified,   right    justified,   or   center justified   within     its    column.

```



## 8th


```Forth

quote | Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.|
var, raw-text

[] var, data
var width

: read-and-parse \ --
  raw-text @
  ( "$" s:/ data @ swap a:push drop )
  s:eachline ;

: find-widest \ -- n
  data @ ( ( swap s:len nip n:max ) swap a:reduce ) 0 a:reduce ;

: print-data \ fmt --
  width @ swap s:strfmt >r
  data @
  (
    nip
    (
      nip
      r@ s:strfmt .
    ) a:each drop

    cr
  ) a:each drop rdrop ;


: app:main
  read-and-parse

  \ find widest column, and add one for the space:
  find-widest n:1+ width !

  \ print the data
  cr "right:" . cr "%%>%ds" print-data
  cr "left:" . cr "%%<%ds" print-data
  cr "center:" . cr "%%|%ds" print-data
  bye ;


```

{{out}}

```txt

right:
      Given          a       text       file         of       many     lines,      where     fields     within          a       line
        are delineated         by          a     single   'dollar' character,      write          a    program
       that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
     column        are  separated         by         at      least        one     space.
   Further,      allow        for       each       word         in          a     column         to         be     either       left
 justified,      right justified,         or     center  justified     within        its    column.

left:
Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.

center:
   Given        a        text       file        of        many      lines,      where     fields     within        a        line
    are    delineated     by          a       single    'dollar'  character,    write        a       program
   that      aligns      each      column       of       fields       by      ensuring     that       words       in        each
  column       are     separated     by         at        least       one      space.
 Further,     allow       for       each       word        in          a       column       to         be       either      left
justified,    right   justified,     or       center    justified   within       its      column.

```



## ABAP


```ABAP
report z_align no standard page header.
start-of-selection.

data: lt_strings type standard table of string,
      lv_strings type string.
append: 'Given$a$text$file$of$many$lines,$where$fields$within$a$line$' to lt_strings,
        'are$delineated$by$a$single$''dollar''$character,$write$a$program' to lt_strings,
        'that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$' to lt_strings,
        'column$are$separated$by$at$least$one$space.' to lt_strings,
        'Further,$allow$for$each$word$in$a$column$to$be$either$left$' to lt_strings,
        'justified,$right$justified,$or$center$justified$within$its$column.' to lt_strings.
types ty_strings type standard table of string.

perform align_col using 'LEFT' lt_strings.
skip.
perform align_col using 'RIGHT' lt_strings.
skip.
perform align_col using 'CENTER' lt_strings.


form align_col using iv_just type string iv_strings type ty_strings.
  constants: c_del value '$'.
  data: lv_string type string,
        lt_strings type table of string,
        lt_tables like table of lt_strings,
        lv_first type string,
        lv_second type string,
        lv_longest type i value 0,
        lv_off type i value 0,
        lv_len type i.
  " Loop through the supplied text. It is expected at the input is a table of strings, with each
  " entry in the table representing a new line of the input.
  loop at iv_strings into lv_string.
    " Split the current line at the delimiter.
    split lv_string at c_del into lv_first lv_second.
    " Loop through the line splitting at every delimiter.
    do.
      append lv_first to lt_strings.
      lv_len = strlen( lv_first ).
      " Check if the length of the new string is greater than the currently stored length.
      if lv_len > lv_longest.
        lv_longest = lv_len.
      endif.
      if lv_second na c_del.
        " Check if the string is longer than the recorded maximum.
        lv_len = strlen( lv_second ).
        if lv_len > lv_longest.
          lv_longest = lv_len.
        endif.
        append lv_second to lt_strings.
        exit.
      endif.
      split lv_second at c_del into lv_first lv_second.
    enddo.

    append lt_strings to lt_tables.
    clear lt_strings.
  endloop.

  " Loop through each line of input.
  loop at lt_tables into lt_strings.
    " Loop through each word in the line (Separated by specified delimiter).
    loop at lt_strings into lv_string.
      lv_off = ( sy-tabix - 1 ) * ( lv_longest + 2 ).
      case iv_just.
        when 'LEFT'.
          write : at (lv_longest) lv_string left-justified.
        when 'RIGHT'.
          write at (lv_longest) lv_string right-justified.
        when 'CENTER'.
          write at (lv_longest) lv_string centered.
      endcase.
    endloop.
    skip.
    sy-linno = sy-linno - 1.
  endloop.
endform.
```


<pre style="height:15ex;overflow:scroll">Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.

     Given          a       text       file         of       many     lines,      where     fields     within          a       line
       are delineated         by          a     single   'dollar' character,      write          a    program
      that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
    column        are  separated         by         at      least        one     space.
  Further,      allow        for       each       word         in          a     column         to         be     either       left
justified,      right justified,         or     center  justified     within        its    column.

  Given        a         text       file        of        many      lines,     where      fields     within       a         line
   are     delineated     by         a        single    'dollar'  character,   write        a       program
   that      aligns      each      column       of       fields       by      ensuring     that      words        in        each
  column      are     separated      by         at       least       one       space.
 Further,    allow       for        each       word        in         a        column       to         be       either      left
justified,   right    justified,     or       center   justified    within      its      column.
```



## Ada

{{libheader|Simple components for Ada}}

```ada
with Ada.Characters.Latin_1;  use Ada.Characters.Latin_1;
with Ada.Text_IO;             use Ada.Text_IO;
with Strings_Edit;            use Strings_Edit;

procedure Column_Aligner is
   Text : constant String :=
      "Given$a$text$file$of$many$lines,$where$fields$within$a$line$" & NUL &
      "are$delineated$by$a$single$'dollar'$character,$write$a$program" & NUL &
      "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$" & NUL &
      "column$are$separated$by$at$least$one$space." & NUL &
      "Further,$allow$for$each$word$in$a$column$to$be$either$left$" & NUL &
      "justified,$right$justified,$or$center$justified$within$its$column." & NUL;
   File    : File_Type;
   Width   : array (1..1_000) of Natural := (others => 0);
   Line    : String (1..200);
   Column  : Positive := 1;
   Start   : Positive := 1;
   Pointer : Positive;
begin
   Create (File, Out_File, "columned.txt");
      -- Determining the widths of columns
   for I in Text'Range loop
      case Text (I) is
         when '$' | NUL =>
            Width (Column) := Natural'Max (Width (Column), I - Start + 1);
            Start  := I + 1;
            if Text (I) = NUL then
               Column := 1;
            else
               Column := Column + 1;
            end if;
         when others =>
            null;
      end case;
   end loop;
      -- Formatting
   for Align in Alignment loop
      Column  := 1;
      Start   := 1;
      Pointer := 1;
      for I in Text'Range loop
         case Text (I) is
            when '$' | NUL =>
               Put -- Formatted output of a word
               (  Destination => Line,
                  Pointer     => Pointer,
                  Value       => Text (Start..I - 1),
                  Field       => Width (Column),
                  Justify     => Align
               );
               Start  := I + 1;
               if Text (I) = NUL then
                  Put_Line (File, Line (1..Pointer - 1));
                  Pointer := 1;
                  Column := 1;
               else
                  Column := Column + 1;
               end if;
            when others =>
               null;
         end case;
      end loop;
   end loop;
   Close (File);
end Column_Aligner;
```

Formatted file sample:
<pre style="height:15ex;overflow:scroll">
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.
      Given          a       text   file     of      many     lines,    where  fields  within      a line
        are delineated         by      a single  'dollar' character,    write       a program
       that     aligns       each column     of    fields         by ensuring    that   words     in each
     column        are  separated     by     at     least        one   space.
   Further,      allow        for   each   word        in          a   column      to      be either left
 justified,      right justified,     or center justified     within      its column.
   Given        a        text     file    of      many     lines,     where   fields  within    a   line
    are    delineated     by        a   single  'dollar' character,   write     a    program
   that      aligns      each    column   of     fields      by     ensuring   that   words    in   each
  column       are     separated   by     at     least       one     space.
 Further,     allow       for     each   word      in         a      column     to      be   either left
justified,    right   justified,   or   center justified   within      its   column.

```



## Aime


```aime
data b;
file f;
text n, t;
list c, r, s;
integer a, i, k, m, w;

b = "Given$a$text$file$of$many$lines,$where$fields$within$a$line$\n"
    "are$delineated$by$a$single$'dollar'$character,$write$a$program\n"
    "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$\n"
    "column$are$separated$by$at$least$one$space.\n"
    "Further,$allow$for$each$word$in$a$column$to$be$either$left$\n"
    "justified,$right$justified,$or$center$justified$within$its$column.";

f.b_affix(b);

m = 0;

while (f.news(r, 0, 0, "$") ^ -1) {
    c.append(r);
    m = max(m, ~r);
}

i = 0;
while (i < m) {
    w = 0;
    for (, r in c) {
        if (i < ~r) {
            w = max(w, length(r[i]));
        }
    }
    s.append(w + 1);
    i += 1;
}

for (k, t in list("left", "center", "right")) {
    o_(t, " justified\n");
    for (, r in c) {
        for (i, n in r) {
            m = s[i] - ~n;
            o_form("/w~3/~/w~1/", a = (2 - k) * m >> 1, "", m - a, "", n);
        }
        o_newline();
    }
    o_newline();
}
```

{{Out}}

```txt
left justified
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

center justified
   Given        a         text     file    of     many      lines,    where   fields  within    a    line
    are     delineated     by       a    single 'dollar'  character,  write      a    program
    that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
   column      are     separated    by     at     least      one      space.
  Further,    allow       for      each   word     in         a       column    to      be    either left
 justified,   right    justified,   or   center justified   within     its    column.

right justified
      Given          a       text   file     of      many     lines,    where  fields  within      a line
        are delineated         by      a single  'dollar' character,    write       a program
       that     aligns       each column     of    fields         by ensuring    that   words     in each
     column        are  separated     by     at     least        one   space.
   Further,      allow        for   each   word        in          a   column      to      be either left
 justified,      right justified,     or center justified     within      its column.
```



## ALGOL 68


```algol68
STRING nl = REPR 10;
STRING text in list := "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"+nl+
  "are$delineated$by$a$single$'dollar'$character,$write$a$program"+nl+
  "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"+nl+
  "column$are$separated$by$at$least$one$space."+nl+
  "Further,$allow$for$each$word$in$a$column$to$be$either$left$"+nl+
  "justified,$right$justified,$or$center$justified$within$its$column.";

MODE PAGE = FLEX[0,0]STRING;
PAGE page;

PROC flex page = (PAGE in page, INT row, col)PAGE:(
  HEAP FLEX[row, col]STRING out page;
  out page[:1 UPB in page, :2 UPB in page] := in page;
  FOR r TO row DO
    FOR c FROM 2 UPB in page + 1 TO col DO out page[r,c]:="" OD
  OD;
  FOR r FROM 1 UPB in page + 1 TO row DO
    FOR c FROM 1 TO col DO out page[r,c]:="" OD
  OD;
  out page
);

FILE text in file;
associate(text in file, text in list);
make term(text in file, "$");

on physical file end(text in file, (REF FILE skip)BOOL: stop iteration);
on logical file end(text in file, (REF FILE skip)BOOL: stop iteration);
FOR row DO
  on line end(text in file, (REF FILE skip)BOOL: stop iteration);
  FOR col DO
    STRING tok;
    getf(text in file, ($gx$,tok));
    IF row > 1 UPB page THEN page := flex page(page, row, 2 UPB page) FI;
    IF col > 2 UPB page THEN page := flex page(page, 1 UPB page, col) FI;
    page[row,col]:=tok
  OD;
  stop iteration:
    SKIP
OD;
stop iteration:
  SKIP;

BEGIN
  PROC aligner = (PAGE in page, PROC (STRING,INT)STRING aligner)VOID:(
    PAGE page := in page;
    [2 UPB page]INT max width;
    FOR col TO 2 UPB page DO
      INT max len:=0; FOR row TO UPB page DO IF UPB page[row,col]>max len THEN max len:=UPB page[row,col] FI OD;
      FOR row TO UPB page DO page[row,col] := aligner(page[row,col], maxlen) OD
    OD;
    printf(($n(UPB page)(n(2 UPB page -1)(gx)gl)$,page))
  );

  PROC left = (STRING in, INT len)STRING: in + " "*(len - UPB in),
       right = (STRING in, INT len)STRING: " "*(len - UPB in) + in,
       centre = (STRING in, INT len)STRING: ( INT pad=len-UPB in;  pad%2*" "+ in + (pad-pad%2)*" " );

  []STRUCT(STRING name, PROC(STRING,INT)STRING align) aligners = (("Left",left), ("Left",right), ("Centre",centre));

  FOR index TO UPB aligners DO
    print((new line, "# ",name OF aligners[index]," Column-aligned output:",new line));
    aligner(page, align OF aligners[index])
  OD
END
```




## AppleScript


Probably not the first language in which you would really choose to do this kind of thing, but certainly possible, and can be readily easily assembled from a set of generic primitives.

{{trans|JavaScript}}

```AppleScript
-- COLUMN ALIGNMENTS ---------------------------------------------------------

property pstrLines : ¬
    "Given$a$text$file$of$many$lines,$where$fields$within$a$line$\n" & ¬
    "are$delineated$by$a$single$'dollar'$character,$write$a$program\n" & ¬
    "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$\n" & ¬
    "column$are$separated$by$at$least$one$space.\n" & ¬
    "Further,$allow$for$each$word$in$a$column$to$be$either$left$\n" & ¬
    "justified,$right$justified,$or$center$justified$within$its$column."

property eLeft : -1
property eCenter : 0
property eRight : 1

-- columnsAligned :: EnumValue -> [[String]] -> String
on columnsAligned(eAlign, lstCols)
    -- padwords :: Int -> [String] -> [[String]]
    script padwords
        on |λ|(n, lstWords)

            -- pad :: String -> String
            script pad
                on |λ|(str)
                    set lngPad to n - (length of str)
                    if eAlign = my eCenter then
                        set lngHalf to lngPad div 2
                        {replicate(lngHalf, space), str, ¬
                            replicate(lngPad - lngHalf, space)}
                    else
                        if eAlign = my eLeft then
                            {"", str, replicate(lngPad, space)}
                        else
                            {replicate(lngPad, space), str, ""}
                        end if
                    end if
                end |λ|
            end script

            map(pad, lstWords)
        end |λ|
    end script

    unlines(map(my unwords, ¬
        transpose(zipWith(padwords, ¬
            map(my widest, lstCols), lstCols))))
end columnsAligned

-- lineColumns :: String -> String -> String
on lineColumns(strColDelim, strText)
    -- _words :: Text -> [Text]
    script _words
        on |λ|(str)
            splitOn(strColDelim, str)
        end |λ|
    end script

    set lstRows to map(_words, splitOn(linefeed, pstrLines))
    set nCols to widest(lstRows)

    -- fullRow :: [[a]] -> [[a]]
    script fullRow
        on |λ|(lst)
            lst & replicate(nCols - (length of lst), {""})
        end |λ|
    end script

    transpose(map(fullRow, lstRows))
end lineColumns

-- widest [a] -> Int
on widest(xs)
    |length|(maximumBy(comparing(my |length|), xs))
end widest

-- TEST ----------------------------------------------------------------------
on run
    set lstCols to lineColumns("$", pstrLines)

    script testAlignment
        on |λ|(eAlign)
            columnsAligned(eAlign, lstCols)
        end |λ|
    end script

    intercalate(return & return, ¬
        map(testAlignment, {eLeft, eRight, eCenter}))
end run

-- GENERIC FUNCTIONS ---------------------------------------------------------

-- comparing :: (a -> b) -> (a -> a -> Ordering)
on comparing(f)
    set mf to mReturn(f)
    script
        on |λ|(a, b)
            set x to mf's |λ|(a)
            set y to mf's |λ|(b)
            if x < y then
                -1
            else
                if x > y then
                    1
                else
                    0
                end if
            end if
        end |λ|
    end script
end comparing

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- length :: [a] -> Int
on |length|(xs)
    length of xs
end |length|

-- Lift 2nd class handler function into 1st class script wrapper
-- mReturn :: Handler -> Script
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- maximumBy :: (a -> a -> Ordering) -> [a] -> a
on maximumBy(f, xs)
    set cmp to mReturn(f)
    script max
        on |λ|(a, b)
            if a is missing value or cmp's |λ|(a, b) < 0 then
                b
            else
                a
            end if
        end |λ|
    end script

    foldl(max, missing value, xs)
end maximumBy

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary
-- assembly of a target length

-- replicate :: Int -> a -> [a]
on replicate(n, a)
    if class of a is string then
        set out to ""
    else
        set out to {}
    end if
    if n < 1 then return out
    set dbl to a

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- Text -> Text -> [Text]
on splitOn(strDelim, strMain)
    set {dlm, my text item delimiters} to {my text item delimiters, strDelim}
    set lstParts to text items of strMain
    set my text item delimiters to dlm
    return lstParts
end splitOn

-- transpose :: [[a]] -> [[a]]
on transpose(xss)
    script column
        on |λ|(_, iCol)
            script row
                on |λ|(xs)
                    item iCol of xs
                end |λ|
            end script

            map(row, xss)
        end |λ|
    end script

    map(column, item 1 of xss)
end transpose

-- [Text] -> Text
on unlines(lstLines)
    intercalate(linefeed, lstLines)
end unlines

-- [Text] -> Text
on unwords(lstWords)
    intercalate(" ", lstWords)
end unwords

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f, xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, item i of ys)
        end repeat
        return lst
    end tell
end zipWith
```

{{Out}}

```txt
 Given        a            text         file     of       many        lines,       where      fields    within    a        line
 are          delineated   by           a        single   'dollar'    character,   write      a         program
 that         aligns       each         column   of       fields      by           ensuring   that      words     in       each
 column       are          separated    by       at       least       one          space.
 Further,     allow        for          each     word     in          a            column     to        be        either   left
 justified,   right        justified,   or       center   justified   within       its        column.

      Given            a         text     file       of        many       lines,      where    fields    within        a   line
        are   delineated           by        a   single    'dollar'   character,      write         a   program
       that       aligns         each   column       of      fields           by   ensuring      that     words       in   each
     column          are    separated       by       at       least          one     space.
   Further,        allow          for     each     word          in            a     column        to        be   either   left
 justified,        right   justified,       or   center   justified       within        its   column.

   Given          a           text       file      of       many        lines,      where     fields    within      a      line
    are       delineated       by         a      single   'dollar'    character,    write        a      program
    that        aligns        each      column     of      fields         by       ensuring    that      words      in     each
   column        are       separated      by       at       least        one        space.
  Further,      allow         for        each     word       in           a         column      to        be      either   left
 justified,     right      justified,     or     center   justified     within       its      column.
```



## AutoHotkey



```AutoHotkey
Alignment := "L"																				; Options: L, R, C
Text =
( LTrim
	Given$a$text$file$of$many$lines,$where$fields$within$a$line$
	are$delineated$by$a$single$'dollar'$character,$write$a$program
	that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
	column$are$separated$by$at$least$one$space.
	Further,$allow$for$each$word$in$a$column$to$be$either$left$
	justified,$right$justified,$or$center$justified$within$its$column.
)

Loop, Parse, Text																				; calculate column's width
	If A_LoopField in $,`n
		If (N > W)
			W := N, N := 0
		Else
			N := 0
	Else
		++N
Width := ++W

Loop, Parse, Text, `n																			; process each line
{
	Words := StrSplit(A_LoopField, "$")
	For i, Word in Words																		; process each word
		Line .= Align(Word, Alignment, Width)
	Result .= RTrim(Line) . "`n"
	Line := ""
}

Clipboard := Result																				; present results
MsgBox, The results are in the Clipboard

Align(Pal, How, Width) {																		; function for alignment
	Length := StrLen(Pal)
	If (How = "L")
		Return Pal . Spc(Width - Length)
	Else If (How = "R")
		Return Spc(Width - Length) . Pal
	Else If (How = "C")
		Return Spc((Width - Length)//2) . Pal . Spc(Width - Length - (Width - Length)//2)
}

Spc(Number) {																					; function to concatenate space characters
	Loop, %Number%
		Ret .= A_Space
	Return Ret
}

```



## AutoIt


```AutoIt

; == If the given text is in an file, it will read with:
#include <File.au3>
Global $aRead
_FileReadToArray($sPath, $aRead)  ; == $aRead[0] includes count of lines, every line stored in one item (without linebreak)

; == For example we get the same result with StringSplit()
Global $sText = _
"Given$a$text$file$of$many$lines,$where$fields$within$a$line$" & @CRLF & _
"are$delineated$by$a$single$'dollar'$character,$write$a$program" & @CRLF & _
"that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$" & @CRLF & _
"column$are$separated$by$at$least$one$space." & @CRLF & _
"Further,$allow$for$each$word$in$a$column$to$be$either$left$" & @CRLF & _
"justified,$right$justified,$or$center$justified$within$its$column." & @CRLF

$aRead = StringSplit($sText, @CRLF, 1)

; == strip leading and trailing "$" and trailing spaces, count remaining "$" to get max column number
Global $iMaxColumn = 0, $iLines = 0
For $i = 1 To $aRead[0]
	If $aRead[$i] = '' Then ContinueLoop   ; skip empty lines
	$iLines += 1
	$aRead[$i] = StringRegExpReplace(StringRegExpReplace(StringRegExpReplace($aRead[$i], '^\$', ''), '\$$', ''), '\s*$', '')
	StringReplace($aRead[$i], '$', '$')
	If @extended +1 > $iMaxColumn Then $iMaxColumn = @extended +1
Next

; == build array to store all fields and length of every item
Global $aFields[$iLines][$iMaxColumn +1][2]
; == and store the max. length of item in columns
Global $aColLen[$iMaxColumn]

; == fill the array
Global $aSplitLine
$iLines = 0
For $i = 1 To $aRead[0]
	If $aRead[$i] = '' Then ContinueLoop   ; skip empty lines
	$iMaxColLen = 0
	$aSplitLine = StringSplit($aRead[$i], '$')
	For $j = 1 To $aSplitLine[0]
		$aFields[$iLines][$j-1][0] = $aSplitLine[$j]
		$aFields[$iLines][$j-1][1] = StringLen($aSplitLine[$j])
		If $aFields[$iLines][$j-1][1] > $aColLen[$j-1] Then $aColLen[$j-1] = $aFields[$iLines][$j-1][1]
	Next
	$iLines += 1
Next

; == let the user select the alignment for every column
$sAlign = InputBox('Column alignment', 'There are ' & $iMaxColumn & ' columns.' & @LF & '0 = left    1 = center    2 = right' & @LF &  _
                   'Input alignment for all columns without delimiters.' & @LF & 'Let it empty, to align all left.')
If $sAlign = '' Then
	For $i = 1 To $iMaxColumn
		$sAlign &= '0'
	Next
EndIf
Global $aAlignment = StringSplit($sAlign, '', 2)

; == output all to console
Global $sLineOut
For $i = 0 To UBound($aFields) -1
	$sLineOut = ''
	For $j = 0 To $iMaxColumn -1
		If $aFields[$i][$j][0] = '' Then ContinueLoop
		$sLineOut &= _GetAligned($aFields[$i][$j][0], $aFields[$i][$j][1], $aAlignment[$j], $aColLen[$j])
	Next
	ConsoleWrite(StringTrimRight($sLineOut, 1) & @LF)
Next

Func _GetAligned($_sString, $_iLen, $_iAlign, $_iMaxLen)
	Local $sSpace = ''
	For $i = 1 To $_iMaxLen
		$sSpace &= ' '
	Next
	Switch $_iAlign
		Case 0
			Return $_sString & StringLeft($sSpace, $_iMaxLen - $_iLen +1)
		Case 1
			Local $iLenLeft = Int(($_iMaxLen - $_iLen)/2)
			Local $iLenRight = $_iMaxLen - $iLenLeft - $_iLen
			Return StringLeft($sSpace, $iLenLeft) & $_sString & StringLeft($sSpace, $iLenRight) & ' '
		Case 2
			Return StringLeft($sSpace, $_iMaxLen - $_iLen) & $_sString & ' '
	EndSwitch
EndFunc  ;==>_GetAligned

```

Example output in Alignment: left - center - right - left - center - right - left - center - right - left - center - right

```txt

Given          a            text file     of        many lines,      where    fields within    a    line
are        delineated         by a      single  'dollar' character,  write         a program
that         aligns         each column   of      fields by         ensuring    that words     in   each
column        are      separated by       at       least one         space.
Further,     allow           for each    word         in a           column       to be      either left
justified,   right    justified, or     center justified within       its    column.

```


## AWK


```AWK

# syntax: GAWK -f ALIGN_COLUMNS.AWK ALIGN_COLUMNS.TXT
BEGIN {
    colsep = " " # separator between columns
    report("raw data")
}
{   printf("%s\n",$0)
    arr[NR] = $0
    n = split($0,tmp_arr,"$")
    for (j=1; j<=n; j++) {
      width = max(width,length(tmp_arr[j]))
    }
}
END {
    report("left justified")
    report("right justified")
    report("center justified")
    exit(0)
}
function report(text,  diff,i,j,l,n,r,tmp_arr) {
    printf("\nreport: %s\n",text)
    for (i=1; i<=NR; i++) {
      n = split(arr[i],tmp_arr,"$")
      if (tmp_arr[n] == "") { n-- }
      for (j=1; j<=n; j++) {
        if (text ~ /^[Ll]/) { # left
          printf("%-*s%s",width,tmp_arr[j],colsep)
        }
        else if (text ~ /^[Rr]/) { # right
          printf("%*s%s",width,tmp_arr[j],colsep)
        }
        else if (text ~ /^[Cc]/) { # center
          diff = width - length(tmp_arr[j])
          l = r = int(diff / 2)
          if (diff != l + r) { r++ }
          printf("%*s%s%*s%s",l,"",tmp_arr[j],r,"",colsep)
        }
      }
      printf("\n")
    }
}
function max(x,y) { return((x > y) ? x : y) }

```

<p>Output:</p>

```txt

report: raw data
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.

report: left justified
Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.

report: right justified
     Given          a       text       file         of       many     lines,      where     fields     within          a       line
       are delineated         by          a     single   'dollar' character,      write          a    program
      that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
    column        are  separated         by         at      least        one     space.
  Further,      allow        for       each       word         in          a     column         to         be     either       left
justified,      right justified,         or     center  justified     within        its    column.

report: center justified
  Given        a         text       file        of        many      lines,     where      fields     within       a         line
   are     delineated     by         a        single    'dollar'  character,   write        a       program
   that      aligns      each      column       of       fields       by      ensuring     that      words        in        each
  column      are     separated      by         at       least       one       space.
 Further,    allow       for        each       word        in         a        column       to         be       either      left
justified,   right    justified,     or       center   justified    within      its      column.

```



## BaCon


```freebasic

DECLARE in$[] = { "Given$a$text$file$of$many$lines,$where$fields$within$a$line$", \
                  "are$delineated$by$a$single$'dollar'$character,$write$a$program", \
                  "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$", \
                  "column$are$separated$by$at$least$one$space.", \
                  "Further,$allow$for$each$word$in$a$column$to$be$either$left$", \
                  "justified,$right$justified,$or$center$justified$within$its$column." }

OPTION DELIM "$"

CONST items = 6

SUB Print_In_Columns(style)

    ' Find widest column
    FOR y = 0 TO items-1
        FOR x = 1 TO AMOUNT(in$[y])
            IF LEN(TOKEN$(in$[y], x)) > max THEN max = LEN(TOKEN$(in$[y], x))
        NEXT
    NEXT

    ' Print aligned
    FOR y = 0 TO items-1
        FOR x = 1 TO AMOUNT(in$[y])
            PRINT ALIGN$(TOKEN$(in$[y], x), max+1, style);
        NEXT
        PRINT
    NEXT
    PRINT

END SUB

Print_In_Columns(0)
Print_In_Columns(1)
Print_In_Columns(2)

```

{{out}}

```txt

Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.

      Given          a       text       file         of       many     lines,      where     fields     within          a       line
        are delineated         by          a     single   'dollar' character,      write          a    program
       that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
     column        are  separated         by         at      least        one     space.
   Further,      allow        for       each       word         in          a     column         to         be     either       left
 justified,      right justified,         or     center  justified     within        its    column.

   Given        a        text       file        of        many      lines,      where     fields     within        a        line
    are    delineated     by          a       single    'dollar'  character,    write        a       program
   that      aligns      each      column       of       fields       by      ensuring     that       words       in        each
  column       are     separated     by         at        least       one      space.
 Further,     allow       for       each       word        in          a       column       to         be       either      left
justified,    right   justified,     or       center    justified   within       its      column.


```



## Batch File


```dos
@echo off
setlocal enabledelayedexpansion
mode con cols=103

echo Given$a$text$file$of$many$lines,$where$fields$within$a$line$ >file.txt
echo are$delineated$by$a$single$'dollar'$character,$write$a$program! >>file.txt
echo that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$>>file.txt
echo column$are$separated$by$at$least$one$space.>>file.txt
echo Further,$allow$for$each$word$in$a$column$to$be$either$left$>>file.txt
echo justified,$right$justified,$or$center$justified$within$its$column.>>file.txt

for /f "tokens=1-13 delims=$" %%a in ('type file.txt') do (
 call:maxlen %%a %%b %%c %%d %%e %%f %%g %%h %%i %%j %%k %%l %%m  )
echo.
for /f "tokens=1-13 delims=$" %%a in ('type file.txt') do (
 call:align 1 %%a %%b %%c %%d %%e %%f %%g %%h %%i %%j %%k %%l %%m  )
echo.
for /f "tokens=1-13 delims=$" %%a in ('type file.txt') do (
 call:align 2 %%a %%b %%c %%d %%e %%f %%g %%h %%i %%j %%k %%l %%m  )
echo.
for /f "tokens=1-13 delims=$" %%a in ('type file.txt') do (
 call:align 3 %%a %%b %%c %%d %%e %%f %%g %%h %%i %%j %%k %%l %%m  )

exit /B

:maxlen     &::sets variables len1 to len13
  set "cnt=1"
:loop1
  if "%1"=="" exit /b
  call:strlen %1 length
  if !len%cnt%! lss !length! set len%cnt%=!length!
  set /a cnt+=1
  shift
  goto loop1

:align
  setlocal
  set cnt=1
  set print=
:loop2
  if "%2"=="" echo(%print%&endlocal & exit /b
  set /a width=len%cnt%,cnt+=1
  set arr=%2
  if %1 equ 1 call:left   %width% arr
  if %1 equ 2 call:right  %width% arr
  if %1 equ 3 call:center %width% arr
  set "print=%print%%arr% "
  shift /2
  goto loop2

:left %num% &string
  setlocal
   set "arr=!%2!                     "
   set arr=!arr:~0,%1!
  endlocal & set %2=%arr%
exit /b

:right %num% &string
  setlocal
   set "arr=                    !%2!"
   set arr=!arr:~-%1!
   endlocal & set %2=%arr%
exit /b

:center %num% &string
setlocal
  set /a width=%1-1
  set arr=!%2!
  :loop3
  if "!arr:~%width%,1!"=="" set "arr=%arr% "
  if "!arr:~%width%,1!"=="" set "arr= %arr%"
  if "!arr:~%width%,1!"=="" goto loop3
endlocal & set %2=%arr%
exit /b

:strlen  StrVar  &RtnVar
  setlocal EnableDelayedExpansion
  set "s=#%~1"
  set "len=0"
  for %%N in (4096 2048 1024 512 256 128 64 32 16 8 4 2 1) do (
    if "!s:~%%N,1!" neq "" set /a "len+=%%N" & set "s=!s:~%%N!"
  )
  endlocal & set %~2=%len%
exit /b
```

{{out}}

```txt
Given     a          text      file   of     many      lines     where    fields  within  a      line
are       delineated by        a      single 'dollar'  character write    a       program
that      aligns     each      column of     fields    by        ensuring that    words   in     each
column    are        separated by     at     least     one       space.
Further   allow      for       each   word   in        a         column   to      be      either left
justified right      justified or     center justified within    its      column.

    Given          a      text   file     of      many     lines    where  fields  within      a line
      are delineated        by      a single  'dollar' character    write       a program
     that     aligns      each column     of    fields        by ensuring    that   words     in each
   column        are separated     by     at     least       one   space.
  Further      allow       for   each   word        in         a   column      to      be either left
justified      right justified     or center justified    within      its column.

  Given       a        text     file    of     many      lines    where   fields  within    a    line
   are    delineated    by       a    single 'dollar'  character  write      a    program
  that      aligns     each    column   of    fields      by     ensuring  that    words    in   each
 column      are     separated   by     at     least      one     space.
 Further    allow       for     each   word     in         a      column    to      be    either left
justified   right    justified   or   center justified  within     its    column.
```



## BASIC


=
## BBC BASIC
=
{{works with|BBC BASIC for Windows}}

```bbcbasic
      DATA 6
      DATA "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
      DATA "are$delineated$by$a$single$'dollar'$character,$write$a$program"
      DATA "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
      DATA "column$are$separated$by$at$least$one$space."
      DATA "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
      DATA "justified,$right$justified,$or$center$justified$within$its$column."

      REM First find the maximum length of a 'word':
      max% = 0
      READ nlines%
      FOR Line% = 1 TO nlines%
        READ text$
        REPEAT
          word$ = FNword(text$, "$")
          IF LEN(word$) > max% THEN max% = LEN(word$)
        UNTIL word$ = ""
      NEXT Line%
      @% = max% : REM set column width

      REM Now display the aligned text:
      RESTORE
      READ nlines%
      FOR Line% = 1 TO nlines%
        READ text$
        REPEAT
          word$ = FNword(text$, "$")
          PRINT FNjustify(word$, max%, "left"),;
        UNTIL word$ = ""
        PRINT
      NEXT Line%

      END

      DEF FNword(text$, delim$)
      PRIVATE delim%
      LOCAL previous%
      IF delim% = 0 THEN
        previous% = 1
      ELSE
        previous% = delim% + LEN(delim$)
      ENDIF
      delim% = INSTR(text$+delim$, delim$, previous%)
      IF delim% = 0 THEN
        = ""
      ELSE
        = MID$(text$, previous%, delim%-previous%) + " "
      ENDIF

      DEF FNjustify(word$, field%, mode$)
      IF word$ = "" THEN = ""
      CASE mode$ OF
        WHEN "center": = STRING$((field%-LEN(word$)) DIV 2, " ") + word$
        WHEN "right": = STRING$(field%-LEN(word$), " ") + word$
      ENDCASE
      = word$
```


=
## Commodore BASIC
=

```basic
10 rem ********************************
20 rem print words in columns
30 rem commodore basic 2.0
40 rem ********************************
50 print chr$(14) : rem change to upper/lower case set
60 gosub 140      : rem find length of longest word
70 algn$ = "left"
80 gosub 260      : rem print aligned text
90 algn$ = "center"
100 gosub 260
110 algn$ = "right"
120 gosub 260
130 end
140 rem *** find length of longest word
150 mx=0
160 for i=1 to 6
170     read a$
180     n=1
190     for j=1 to len(a$)
200         if mid$(a$,j,1)<>"$" then n=n+1: goto 230
210         if mx<n then mx=n
220         n=1
230     next
240 next
250 return
260 rem print aligned text
270 restore : rem reset data read pointer
280 s$ = "                "
290 print : print algn$;"-aligned"
300 c=1 : rem column counter
310 for i=1 to 6
320     read a$
330     n=1
340     for j=1 to len(a$)
350         if mid$(a$,j,1)<>"$" then n=n+1 : goto 380
360         gosub 440 : rem print word
370         n=1
380     next
390     if n>1 then gosub 440
400 next
410 print
420 return
430 rem ********* print word **********
440 b$ = mid$(a$,j-n+1,n-1)
450 b = len(b$)
460 if algn$ = "center" then 520
470 if algn$ = "right" then 570
480 if c+b<40 and c+mx>40 then print b$: c=1: return
490 if c+mx>40 then print : c=1
500 print b$;left$(s$,mx-b);: c=c+mx
510 return
520 if c+mx>40 then print : c=1
530 bb=(mx-b)/2 : ba=bb
540 if bb>1 and int(bb)=bb then ba=bb-1
550 print left$(s$,ba);b$;left$(s$,bb);: c=c+mx
560 return
570 if c+mx>40 then print : c=1
580 print left$(s$,mx-b);b$;: c=c+mx
590 return
600 rem *********** the words *********
610 data "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
620 data "are$delineated$by$a$single$'dollar'$character,$write$a$program"
630 data "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
640 data "column$are$separated$by$at$least$one$space."
650 data "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
660 data "justified,$right$justified,$or$center$justified$within$its$column"
```



## C

See [[Column Aligner/C]]


## C#

Uses a delegate, which were added to the language in C# 2, to define left-, right-, or center-justified.

{{works with|C sharp|C#|2+}}


```c#
using System;
class ColumnAlignerProgram
{
    delegate string Justification(string s, int width);

    static string[] AlignColumns(string[] lines, Justification justification)
    {
        const char Separator = '$';
        // build input table and calculate columns count
        string[][] table = new string[lines.Length][];
        int columns = 0;
        for (int i = 0; i < lines.Length; i++)
        {
            string[] row = lines[i].TrimEnd(Separator).Split(Separator);
            if (columns < row.Length) columns = row.Length;
            table[i] = row;
        }
        // create formatted table
        string[][] formattedTable = new string[table.Length][];
        for (int i = 0; i < formattedTable.Length; i++)
        {
            formattedTable[i] = new string[columns];
        }
        for (int j = 0; j < columns; j++)
        {
            // get max column width
            int columnWidth = 0;
            for (int i = 0; i < table.Length; i++)
            {
                if (j < table[i].Length && columnWidth < table[i][j].Length)
                    columnWidth = table[i][j].Length;
            }
            // justify column cells
            for (int i = 0; i < formattedTable.Length; i++)
            {
                if (j < table[i].Length)
                    formattedTable[i][j] = justification(table[i][j], columnWidth);
                else
                    formattedTable[i][j] = new String(' ', columnWidth);
            }
        }
        // create result
        string[] result = new string[formattedTable.Length];
        for (int i = 0; i < result.Length; i++)
        {
            result[i] = String.Join(" ", formattedTable[i]);
        }
        return result;
    }

    static string JustifyLeft(string s, int width) { return s.PadRight(width); }
    static string JustifyRight(string s, int width) { return s.PadLeft(width); }
    static string JustifyCenter(string s, int width)
    {
        return s.PadLeft((width + s.Length) / 2).PadRight(width);
    }

    static void Main()
    {
        string[] input = {
            "Given$a$text$file$of$many$lines,$where$fields$within$a$line$",
            "are$delineated$by$a$single$'dollar'$character,$write$a$program",
            "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$",
            "column$are$separated$by$at$least$one$space.",
            "Further,$allow$for$each$word$in$a$column$to$be$either$left$",
            "justified,$right$justified,$or$center$justified$within$its$column.",
        };

        foreach (string line in AlignColumns(input, JustifyCenter))
        {
            Console.WriteLine(line);
        }
    }
}
```


{{out}} (centered):


```txt

  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

```



## C++

See [[Column Aligner/C++]]


## Clojure


```Clojure

(ns rosettacode.align-columns
  (:require [clojure.contrib.string :as str]))

(def data "Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.")

(def table (map #(str/split #"\$" %) (str/split-lines data)))

(defn col-width [n table] (reduce max (map #(try (count (nth % n))
                                               (catch Exception _  0))
                                           table)))
(defn spaces [n] (str/repeat n " "))
(defn add-padding
  "if the string is too big turncate it, else return a string with padding"
  [string width justification]
  (if (>= (count string) width) (str/take width string)
      (let [pad-len (int (- width (count string))) ;we don't want rationals
            half-pad-len (int (/ pad-len 2))]
        (case justification
              :right (str (spaces pad-len) string)
              :left  (str string (spaces pad-len))
              :center (str (spaces half-pad-len) string (spaces (- pad-len half-pad-len)))))))

(defn aligned-table
  "get the width of each column, then generate a new table with propper padding for eath item"
  ([table justification]
  (let [col-widths (map #(+ 2 (col-width % table)) (range (count(first table))))]
    (map
     (fn [row] (map #(add-padding %1 %2 justification) row col-widths))
     table))))

(defn print-table
  [table]
  (do (println)
      (print (str/join "" (flatten (interleave table (repeat "\n")))))))

(print-table (aligned-table table :center))

```


## COBOL


```cobol

       identification division.
       program-id. AlignColumns.

       data division.
       working-storage section.
      *>-> Constants
       78 MAX-LINES value 6.
       78 MAX-LINE-SIZE value 66.
       78 MAX-COLUMNS value 12.
       78 MAX-COLUMN-SIZE value 16.
      *>-> Indexes
       01 w-idx                   pic is 9(2).
       01 w-idy                   pic is 9(2).
       01 w-pos                   pic is 9(3).
      *>-> Data structures
       01 w-lines.
          05 w-line               pic is x(MAX-LINE-SIZE) occurs MAX-LINES.
       01 w-column-sizes.
          05 w-column-size        pic is 99 occurs MAX-COLUMNS value zeros.
       01 w-matrix.
          05 filler               occurs MAX-LINES.
             10 filler            occurs MAX-COLUMNS.
                15 w-content      pic is x(MAX-COLUMN-SIZE).
      *>-> Output
       01 w-line-out              pic is x(120).
      *>-> Data alignment
       01 w-alignment             pic is x(1).
          88 alignment-left       value is "L".
          88 alignment-center     value is "C".
          88 alignment-right      value is "R".

       procedure division.
       main.
           move "Given$a$text$file$of$many$lines,$where$fields$within$a$line$" to w-line(1)
           move "are$delineated$by$a$single$'dollar'$character,$write$a$program" to w-line(2)
           move "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$" to w-line(3)
           move "column$are$separated$by$at$least$one$space." to w-line(4)
           move "Further,$allow$for$each$word$in$a$column$to$be$either$left$" to w-line(5)
           move "justified,$right$justified,$or$center$justified$within$its$column." to w-line(6)
           perform calculate-size-columns
           set alignment-left to true
           perform show-content
           set alignment-center to true
           perform show-content
           set alignment-right to true
           perform show-content
           goback
           .
       calculate-size-columns.
           perform
              varying             w-idx from 1 by 1
                 until            w-idx > MAX-LINES
              unstring w-line(w-idx) delimited by "$" into w-content(w-idx, 1), w-content(w-idx, 2),
                  w-content(w-idx, 3), w-content(w-idx, 4), w-content(w-idx, 5), w-content(w-idx, 6),
                  w-content(w-idx, 7), w-content(w-idx, 8), w-content(w-idx, 9), w-content(w-idx, 10),
                  w-content(w-idx, 11), w-content(w-idx, 12),
              perform
                 varying          w-idy from 1 by 1
                    until         w-idy > MAX-COLUMNS
                 if function stored-char-length(w-content(w-idx, w-idy)) > w-column-size(w-idy)
                    move function stored-char-length(w-content(w-idx, w-idy)) to w-column-size(w-idy)
                 end-if
              end-perform
           end-perform
           .
       show-content.
           move all "-" to w-line-out
           display w-line-out
           perform
              varying             w-idx from 1 by 1
                 until            w-idx > MAX-LINES
              move spaces to w-line-out
              move 1 to w-pos
              perform
                 varying          w-idy from 1 by 1
                    until         w-idy > MAX-COLUMNS
                 call "C$JUSTIFY" using w-content(w-idx, w-idy)(1:w-column-size(w-idy)), w-alignment
                 move w-content(w-idx, w-idy) to w-line-out(w-pos:w-column-size(w-idy))
                 compute w-pos = w-pos + w-column-size(w-idy) + 1
              end-perform
              display w-line-out
           end-perform
           .

```

{{out}}
<lang>
------------------------------------------------------------------------------------------------------------------------
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.
------------------------------------------------------------------------------------------------------------------------
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.
------------------------------------------------------------------------------------------------------------------------
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

```



## CoffeeScript


```coffeescript

pad = (n) ->
  s = ''
  while n > 0
    s += ' '
    n -= 1
  s

align = (input, alignment = 'center') ->
  tokenized_lines = (line.split '$' for line in input)
  col_widths = {}
  for line in tokenized_lines
    for token, i in line
      if !col_widths[i]? or token.length > col_widths[i]
        col_widths[i] = token.length
  padders =
    center: (s, width) ->
      excess = width - s.length
      left = Math.floor excess / 2
      right = excess - left
      pad(left) + s + pad(right)

    right: (s, width) ->
      excess = width - s.length
      pad(excess) + s

    left: (s, width) ->
      excess = width - s.length
      s + pad(excess)

  padder = padders[alignment]

  for line in tokenized_lines
    padded_tokens = (padder(token, col_widths[i]) for token, i in line)
    console.log padded_tokens.join ' '


input = [
  "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
  "are$delineated$by$a$single$'dollar'$character,$write$a$program"
  "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
  "column$are$separated$by$at$least$one$space."
  "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
  "justified,$right$justified,$or$center$justified$within$its$column."
]

for alignment in ['center', 'right', 'left']
  console.log "\n----- #{alignment}"
  align input, alignment

```

{{out}}
<lang>
> coffee align_columns.coffee

----- center
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

----- right
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

----- left
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

```



## Common Lisp


```lisp
(defun nonempty (seq)
  (position-if (lambda (x) (declare (ignore x)) t) seq))

(defun split (delim seq)
  "Splits seq on delim into a list of subsequences. Trailing empty
subsequences are removed."
  (labels
      ((f (seq &aux (pos (position delim seq)))
         (if pos
             (cons
              (subseq seq 0 pos)
              (f (subseq seq (1+ pos))))
           (list seq))))
    (let* ((list (f seq))
           (end (position-if #'nonempty list :from-end t)))
      (subseq list 0 (1+ end)))))

(defun lengthen (list minlen filler-elem &aux (len (length list)))
  "Destructively pads list with filler-elem up to minlen."
  (if (< len minlen)
      (nconc list (make-list (- minlen len) :initial-element filler-elem))
    list))

(defun align-columns (text
                      &key (align :left)
                      &aux
                      (fmtmod (case align
                                (:left "@")
                                (:right ":")
                                (:center "@:")
                                (t (error "Invalid alignment."))))
                      (fields (mapcar (lambda (line) (split #\$ line))
                                      (split #\Newline text)))
                      (mostcols (loop for l in fields
                                      maximize (length l)))
                      widest)
  (setf fields (mapcar (lambda (l) (lengthen l mostcols ""))
                       fields))
  (setf widest (loop for col below (length (first fields))
                     collect (loop for row in fields
                                   maximize (length (elt row col)))))
  (format nil
          (with-output-to-string (s)
            (princ "~{~{" s)
            (dolist (w widest)
              (format s "~~~d~a<~~a~~>" (1+ w) fmtmod))
            (princ "~}~%~}" s))
          fields))
```



## D


```d
void main() {
    import std.stdio, std.string, std.algorithm, std.range, std.typetuple;

    immutable data =
"Given$a$txt$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column."
    .split.map!(r => r.chomp("$").split("$")).array;

    size_t[size_t] maxWidths;
    foreach (const line; data)
        foreach (immutable i, const word; line)
            maxWidths[i] = max(maxWidths.get(i, 0), word.length);

    foreach (immutable just; TypeTuple!(leftJustify, center, rightJustify))
        foreach (immutable line; data)
            writefln("%-(%s %)", line.length.iota
                     .map!(i => just(line[i], maxWidths[i], ' ')));
}
```

{{out}}

```txt
Given      a          txt        file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.
  Given        a         txt      file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.
     Given          a        txt   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.
```



## Delphi

{{libheader|Delphi StdCtrls, Classes, SysUtils, StrUtils, Contnrs}}

```Delphi

USES
   StdCtrls, Classes, SysUtils, StrUtils, Contnrs;

procedure AlignByColumn(Output: TMemo; Align: TAlignment);
const
   TextToAlign =
   'Given$a$text$file$of$many$lines,$where$fields$within$a$line$'#$D#$A +
   'are$delineated$by$a$single$''dollar''$character,$write$a$program'#$D#$A +
   'that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$'#$D#$A +
   'column$are$separated$by$at$least$one$space.'#$D#$A +
   'Further,$allow$for$each$word$in$a$column$to$be$either$left$'#$D#$A +
   'justified,$right$justified,$or$center$justified$within$its$column.';
var
   TextLine, TempTString: TStringlist;
   TextLines: TObjectList;
   MaxLength, i, j: Byte;
   OutPutString, EmptyString, Item: String;
begin
   TRY
      MaxLength := 0;
      TextLines := TObjectList.Create(True);
      TextLine := TStringList.Create;
      TextLine.text := TextToAlign;
      for i:= 0 to TextLine.Count - 1 do
      begin
         TempTString := TStringlist.create;
         TempTString.text :=AnsiReplaceStr(TextLine[i], '$', #$D#$A);
         TextLines.Add(TempTString);
      end;
      for i := 0 to TextLines.Count - 1 do
         for j := 0 to TStringList(TextLines.Items[i]).Count - 1 do
            If Length(TStringList(TextLines.Items[i])[j]) > MaxLength then
               MaxLength := Length(TStringList(TextLines.Items[i])[j]);
      If MaxLength > 0 then
         MaxLength := MaxLength + 2; // Add to empty spaces to it
      for i := 0 to TextLines.Count - 1 do
      begin
         OutPutString := '';
         for j := 0 to TStringList(TextLines.Items[i]).Count - 1 do
         begin
            EmptyString := StringOfChar(' ', MaxLength);
            Item := TStringList(TextLines.Items[i])[j];
            case Align of
               taLeftJustify: Move(Item[1], EmptyString[2], Length(Item));
               taRightJustify: Move(Item[1], EmptyString[MaxLength - Length(Item) + 1], Length(Item));
               taCenter: Move(Item[1], EmptyString[(MaxLength - Length(Item) + 1) div 2 + 1], Length(Item));
            end;
            OutPutString := OutPutString + EmptyString;
         end;
         Output.Lines.Add(OutPutString);
      end;
   FINALLY
      FreeAndNil(TextLine);
      FreeAndNil(TextLines);
   END;
end;

```



## E



```e
pragma.enable("accumulator")

def left(width, word) {
  return word + " " * (width - word.size())
}

def center(width, word) {
  def leftCount := (width - word.size()) // 2
  return " " * leftCount + word + " " * (width - word.size() - leftCount)
}

def right(width, word) {
  return " " * (width - word.size()) + word
}

def alignColumns(align, text) {
    def split := accum [] for line in text.split("\n") { _.with(line.split("$")) }
    var widths := []
    for line in split {
      for i => word in line {
        widths with= (i, widths.fetch(i, fn{0}).max(word.size()))
      }
    }
    return accum "" for line in split {
      _ + accum "" for i => word in line {
        _ + align(widths[i] + 1, word)
      } + "\n"
    }
}
```



```e
? def text := "Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column."; null

? println(alignColumns(left, text))
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

? println(alignColumns(center, text))
   Given        a        text     file    of      many     lines,     where   fields  within    a   line
    are    delineated     by        a   single  'dollar' character,   write     a    program
   that      aligns      each    column   of     fields      by     ensuring   that   words    in   each
  column       are     separated   by     at     least       one     space.
 Further,     allow       for     each   word      in         a      column     to      be   either left
justified,    right   justified,   or   center justified   within      its   column.

? println(alignColumns(right, text))
      Given          a       text   file     of      many     lines,    where  fields  within      a line
        are delineated         by      a single  'dollar' character,    write       a program
       that     aligns       each column     of    fields         by ensuring    that   words     in each
     column        are  separated     by     at     least        one   space.
   Further,      allow        for   each   word        in          a   column      to      be either left
 justified,      right justified,     or center justified     within      its column.
```



## Elixir

{{trans|Ruby}}
{{works with|Elixir|1.3}}
The String module of Elixir doesn't have the function of the center position adjusting.
It calls and processes the function of 'Erlang'.

```elixir
defmodule Align do
  def columns(text, alignment) do
    fieldsbyrow = String.split(text, "\n", trim: true)
                  |> Enum.map(fn row -> String.split(row, "$", trim: true) end)
    maxfields = Enum.map(fieldsbyrow, fn field -> length(field) end) |> Enum.max
    colwidths = Enum.map(fieldsbyrow, fn field -> field ++ List.duplicate("", maxfields - length(field)) end)
                |> List.zip
                |> Enum.map(fn column ->
                     Tuple.to_list(column) |> Enum.map(fn col-> String.length(col) end) |> Enum.max
                   end)
    Enum.each(fieldsbyrow, fn row ->
      Enum.zip(row, colwidths)
      |> Enum.map(fn {field, width} -> adjust(field, width, alignment) end)
      |> Enum.join(" ") |> IO.puts
    end)
  end

  defp adjust(field, width, :Left),  do: String.pad_trailing(field, width)
  defp adjust(field, width, :Right), do: String.pad_leading(field, width)
  defp adjust(field, width, _),      do: :string.centre(String.to_charlist(field), width)
end

text = """
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
"""

Enum.each([:Left, :Right, :Center], fn alignment ->
  IO.puts "\n# #{alignment} Column-aligned output:"
  Align.columns(text, alignment)
end)
```


{{out}}

```txt

# Left Column-aligned output:
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

# Right Column-aligned output:
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

# Center Column-aligned output:
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

```



## Erlang


```erlang

-module (align_columns).

-export([align_left/0, align_right/0, align_center/0]).
-define (Lines,
         ["Given\$a\$text\$file\$of\$many\$lines\$where\$fields\$within\$a\$line\$",
          "are\$delineated\$by\$a\$single\$'dollar'\$character,\$write\$a\$program",
          "that\$aligns\$each\$column\$of\$fields\$by\$ensuring\$that\$words\$in\$each\$",
          "column\$are\$separated\$by\$at\$least\$one\$space.",
          "Further,\$allow\$for\$each\$word\$in\$a\$column\$to\$be\$either\$left\$",
          "justified,\$right\$justified,\$or\$center\$justified\$within\$its\$column."].

align_left()-> align_columns(left).
align_right()-> align_columns(right).
align_center()-> align_columns(centre).
align_columns(Alignment) ->
    Words = [ string:tokens(Line, "\$") || Line <- ?Lines ],
    Words_length  = lists:foldl( fun max_length/2, [], Words),
    Result = [prepare_line(Words_line, Words_length, Alignment)
              || Words_line <- Words],

    [ io:fwrite("~s~n", [lists:flatten(Line)]) || Line <- Result],
    ok.

max_length(Words_of_a_line, Acc_maxlength) ->
    Line_lengths = [length(W) || W <- Words_of_a_line ],
    Max_nb_of_length = lists:max([length(Acc_maxlength), length(Line_lengths)]),
    Line_lengths_prepared = adjust_list (Line_lengths, Max_nb_of_length, 0),
    Acc_maxlength_prepared = adjust_list(Acc_maxlength, Max_nb_of_length, 0),
    Two_lengths =lists:zip(Line_lengths_prepared, Acc_maxlength_prepared),
    [ lists:max([A, B]) || {A, B} <- Two_lengths].
adjust_list(L, Desired_length, Elem) ->
    L++lists:duplicate(Desired_length - length(L), Elem).

prepare_line(Words_line, Words_length, Alignment) ->
    All_words = adjust_list(Words_line, length(Words_length), ""),
    Zipped = lists:zip (All_words, Words_length),
    [ apply(string, Alignment, [Word, Length + 1, $\s])
      || {Word, Length} <- Zipped].
```


{{out}}

```txt

1> c(align_columns).
{ok,align_columns}
2> align_columns:align_center().
   Given        a        text     file    of      many      lines     where   fields  within    a   line
    are    delineated     by        a   single  'dollar' character,   write     a    program
   that      aligns      each    column   of     fields      by     ensuring   that   words    in   each
  column       are     separated   by     at     least       one     space.
 Further,     allow       for     each   word      in         a      column     to      be   either left
justified,    right   justified,   or   center justified   within      its   column.
ok
3> align_columns:align_left().
Given      a          text       file   of     many      lines      where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.
ok
4> align_columns:align_right().
      Given          a       text   file     of      many      lines    where  fields  within      a line
        are delineated         by      a single  'dollar' character,    write       a program
       that     aligns       each column     of    fields         by ensuring    that   words     in each
     column        are  separated     by     at     least        one   space.
   Further,      allow        for   each   word        in          a   column      to      be either left
 justified,      right justified,     or center justified     within      its column.
ok

```



## Euphoria


```euphoria
constant data = {
    "Given$a$text$file$of$many$lines,$where$fields$within$a$line$",
    "are$delineated$by$a$single$'dollar'$character,$write$a$program",
    "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$",
    "column$are$separated$by$at$least$one$space.",
    "Further,$allow$for$each$word$in$a$column$to$be$either$left$",
    "justified,$right$justified,$or$center$justified$within$its$column."
}

function split(sequence s, integer c)
    sequence out
    integer first, delim
    out = {}
    first = 1
    while first<=length(s) do
        delim = find_from(c,s,first)
        if delim = 0 then
            delim = length(s)+1
        end if
        out = append(out,s[first..delim-1])
        first = delim + 1
    end while
    return out
end function

function align(sequence s, integer width, integer alignment)
    integer n
    n = width - length(s)
    if n <= 0 then
        return s
    elsif alignment < 0 then
        return s & repeat(' ', n)
    elsif alignment > 0 then
        return repeat(' ', n) & s
    else
        return repeat(' ', floor(n/2)) & s & repeat(' ', floor(n/2+0.5))
    end if
end function

integer maxlen
sequence lines
maxlen = 0
lines = repeat(0,length(data))
for i = 1 to length(data) do
    lines[i] = split(data[i],'$')
    for j = 1 to length(lines[i]) do
        if length(lines[i][j]) > maxlen then
            maxlen = length(lines[i][j])
        end if
    end for
end for

for a = -1 to 1 do
    for i = 1 to length(lines) do
        for j = 1 to length(lines[i]) do
            puts(1, align(lines[i][j],maxlen,a) & ' ')
        end for
        puts(1,'\n')
    end for
    puts(1,'\n')
end for
```


{{out}}

```txt
Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.

  Given        a         text       file        of        many      lines,     where      fields     within       a         line
   are     delineated     by         a        single    'dollar'  character,   write        a       program
   that      aligns      each      column       of       fields       by      ensuring     that      words        in        each
  column      are     separated      by         at       least       one       space.
 Further,    allow       for        each       word        in         a        column       to         be       either      left
justified,   right    justified,     or       center   justified    within      its      column.

     Given          a       text       file         of       many     lines,      where     fields     within          a       line
       are delineated         by          a     single   'dollar' character,      write          a    program
      that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
    column        are  separated         by         at      least        one     space.
  Further,      allow        for       each       word         in          a     column         to         be     either       left
justified,      right justified,         or     center  justified     within        its    column.


```


=={{header|F_Sharp|F#}}==

```fsharp
open System
open System.IO

let tableFromPath path =
    let lines =
        [ for line in File.ReadAllLines(path) -> (line.TrimEnd('$').Split('$')) ]
    let width = List.fold (fun max (line : string[]) -> if max < line.Length then line.Length else max) 0 lines
    List.map (fun (a : string[]) -> (List.init width (fun i -> if i < a.Length then a.[i] else ""))) lines

let rec trans m =
    match m with
    | []::_ -> []
    | _ -> (List.map List.head m) :: trans (List.map List.tail m)

let colWidth table =
    List.map (fun col -> List.max (List.map String.length col)) (trans table)

let left = (fun (s : string) n -> s.PadRight(n))
let right = (fun (s : string) n -> s.PadLeft(n))
let center = (fun (s : string) n -> s.PadLeft((n + s.Length) / 2).PadRight(n))

[<EntryPoint>]
let main argv =
    let table = tableFromPath argv.[0]
    let width = Array.ofList (colWidth table)
    let format table align =
        List.map (fun (row : string list) -> List.mapi (fun i s -> sprintf "%s" (align s width.[i])) row) table
        |> List.iter (fun row -> printfn "%s" (String.Join(" ", Array.ofList row)))

    for align in [ left; right; center ] do
        format table align
        printfn "%s" (new String('-', (Array.sum width) + width.Length - 1))
    0
```

Output, when called with a file containing the sample input

```txt
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.
--------------------------------------------------------------------------------------------------------
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.
--------------------------------------------------------------------------------------------------------
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.
--------------------------------------------------------------------------------------------------------
```



## Factor


```factor
USING: fry io kernel math math.functions math.order sequences
splitting strings ;
IN: rosetta.column-aligner

CONSTANT: example-text "Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column."

: split-and-pad ( text -- lines )
    "\n" split [ "$" split harvest ] map
    dup [ length ] [ max ] map-reduce
    '[ _ "" pad-tail ] map ;

: column-widths ( columns -- widths )
    [ [ length ] [ max ] map-reduce ] map ;

SINGLETONS: +left+ +middle+ +right+ ;

GENERIC: align-string ( str n alignment -- str' )

M: +left+ align-string  drop CHAR: space pad-tail ;
M: +right+ align-string drop CHAR: space pad-head ;

M: +middle+ align-string
    drop
    over length - 2 /
    [ floor CHAR: space <string> ]
    [ ceiling CHAR: space <string> ] bi surround ;

: align-columns ( columns alignment -- columns' )
    [ dup column-widths ] dip '[
        [ _ align-string ] curry map
    ] 2map ;

: print-aligned ( text alignment -- )
    [ split-and-pad flip ] dip align-columns flip
    [ [ write " " write ] each nl ] each ;
```


 example-text { +left+ +middle+ +right+ } [ print-aligned ] with each


## FBSL

Using a multiline string:

```qbasic
#APPTYPE CONSOLE

DIM s =	"Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column."

DIM lines[] = SPLIT(s, CRLF), tokens[], l, t, length, margin, justify = "center"

FOREACH l IN lines
	tokens = SPLIT(l, "$")
	FOREACH t IN tokens
		IF STRLEN(t) > length THEN length = INCR(STRLEN)
	NEXT
NEXT

FOREACH l IN lines
	tokens = SPLIT(l, "$")
	FOREACH t IN tokens
		SELECT CASE justify
			CASE "left"
				PRINT t, SPACE(length - STRLEN(t));
			CASE "center"
				margin = (length - STRLEN(t)) \ 2
				PRINT SPACE(margin), t, SPACE(length - STRLEN - margin);
			CASE "right"
				PRINT SPACE(length - STRLEN(t)), t;
		END SELECT
	NEXT
	PRINT
NEXT

PAUSE
```

{{out}}
    Given        a        text       file        of        many      lines,      where     fields     within        a        line
     are    delineated     by          a       single    'dollar'  character,    write        a       program
    that      aligns      each      column       of       fields       by      ensuring     that       words       in        each
   column       are     separated     by         at        least       one      space.
  Further,     allow       for       each       word        in          a       column       to         be       either      left
 justified,    right   justified,     or       center    justified   within       its      column.

 Press any key to continue...


## Forth

{{works with|GNU Forth}}

```forth
\ align columns

: split ( addr len char -- addr len1 addr len-len1 )
  >r 2dup r> scan 2swap 2 pick - ;

variable column

: for-each-line ( file len xt -- )
  >r begin #lf split r@ execute 1 /string dup 0<= until 2drop rdrop ;

: for-each-field ( line len xt -- )
  0 column !
  >r begin '$ split r@ execute 1 column +! 1 /string dup 0<= until 2drop rdrop ;

0 value num-columns

: count-columns ( line len -- )
  ['] 2drop for-each-field
  num-columns column @ max to num-columns ;
: find-num-columns ( file len -- )
  0 to num-columns
  ['] count-columns for-each-line ;

0 value column-widths

: column-width ( field len -- )
  column-widths column @ + c@
  max
  column-widths column @ + c!
  drop ;
: measure-widths ( line len -- )
  ['] column-width for-each-field ;
: find-column-widths ( file len -- )
  num-columns allocate throw to column-widths
  column-widths num-columns erase
  ['] measure-widths for-each-line ;

\ type aligned, same naming convention as standard numeric U.R, .R
: type.l ( addr len width -- )
  over -               >r type r>       spaces ;
: type.c ( addr len width -- )
  over - dup 2/ spaces >r type r> 1+ 2/ spaces ;
: type.r ( addr len width -- )
  over -        spaces    type ;

defer type.aligned

: print-field ( field len -- )
  column-widths column @ + c@ type.aligned space ;
: print-line   ( line len -- ) cr ['] print-field for-each-field ;
: print-fields ( file len -- )    ['] print-line  for-each-line ;


\ read file
s" columns.txt" slurp-file  ( file len )

\  scan once to determine num-columns
2dup find-num-columns

\  scan again to determine column-widths
2dup find-column-widths

\  print columns, once for each alignment type
' type.l is type.aligned  2dup print-fields cr
' type.c is type.aligned  2dup print-fields cr
' type.r is type.aligned  2dup print-fields cr

\ cleanup
nip free throw
column-widths free throw
```



## Fortran

For the general situation, a utility has no knowledge of the maximum number of fields in a record nor the maximum length of a record, still less knowledge of the sizes of the fields in the records. Decent file systems (for example, that of the Burroughs 6700) make available the MaxRecordLength of a file when a file is opened, but many systems are less than helpful. It is possible to open a file as UNFORMATTED and then simply read sequences of binary data to be interpreted to taste, but in the ASCII world that means deciding on record markers being CR, CRLF, LF, or LFCR and I have encountered files with mixtures... In any case, a file with a fixed record length will likely not contain any such markers.

The plan here is to read the file to find out. But, given that the maximum record length ''is'' unknown, reading records into some storage area is problematical. Some systems can create a "string" variable via the read statement that will be of the proper size, but older Fortran works always with storage sizes fixed at compile time. However, after F77 started being extended, a common extension was the Q-element in FORMAT statements, which instead of interpreting the content of a record, reports instead the number of characters yet unread in a record. Thus, a statement like <code>READ(in,"(Q)") L</code> will read a record of input and the first action is to place in variable L the number of characters yet to be processed of that record. Since the content of the record is not being transferred, there is no need for a CHARACTER area of sufficient size to receive all of it. Indeed here, nothing is read. As for the LF/CRLF/LFCR/CR issue, with FORMATTED input the file system makes its decision and presents record content without incorporating such out-of-context data.

When data are being read into a CHARACTER variable, once the receiver is filled subsequent data in a long record will be ignored, and if the input record was smaller than the variable, trailing spaces will be supplied to fill out the variable. It is for this reason that the read statement is <code>READ (IN,11,END = 20) L,ACARD(1:L)</code> so that, ''on-the-fly'', the length of the record is determined, and then only that number of characters are placed in ACARD with no (possibly large number of) trailing spaces being appended. Naturally, subsequent inspection of ACARD will have to be careful not to look beyond character L, where detritus from previous usage will languish.

Once the main programme has ascertained the maximum record length, subroutine RAKE can be invoked with this as a parameter. From F90 on, as introduced by Algol in the 1960s, functions and subroutines have been allowed to declare arrays of a size specified by a variable and so the "surely big enough" issue is reduced. Thus, given a maximum length of M, a record containing M field delimiters can represent M + 1 fields, all null. The last field's delimiter is the end-of-record, so one more. It is also possible to use ALLOCATE(...) to request storage of suitable dimension for a variable within a routine, but that introduces verbiage. So, a programme can request just enough storage for its particular tasks and thereby possibly succeed on a problem too large for an always-big-enough storage scheme, but on the other hand, the fixed storage allocation scheme doesn't suddenly run out in the middle of a job.

Accordingly, a second scan of the records of the text file can be made, with a view to ascertaining the maximum widths for the first, second, etc. columns. This is done by storing in array C the positions of the delimiters in the text, so the content of a field ''i'' on a record will be starting with C(I - 1) + 1, and ending with C(I) - 1, - that is, the first character following a delimiter to the last character before a delimiter. In other words, the delimiter is not a part of the field's content. For convenience in this, C(0) = 0, since the first field is number one, and the last field's delimiter is one past the end of the record. Having an array start with non-default bounds is a feature of F90, similarly the array assignment that tracks the maximum field widths in array W. The specification makes no call for leading or trailing spaces in a field's content to be trimmed; if it did then two sets of fingers would be used to mark the first and last position of each text. Happily, there is also no call to recognise quoted texts, that might contain delimiter characters that are not delimiters.

Equipped at last with knowledge of each column's maximum width, yet another pass can be made through the file to produce the output. A feature of the format code A''w'' is that text is aligned to the right within the width of ''w''. So, concoct a FORMAT text with suitable A''w'' entries, also augmenting the width by one for each column to meet the requirement that each column's content is separated from the others by at least one space. This concoction is easy enough, and could be improved by taking advantage of the "factoring" that format statements allow so that for example "A7,A7" can be replaced by "2A7", and further, bracketing of sequences is allowed and with repeat counts for them also. Alas, finding the minimum sequence is in general a very difficult problem, so no attempt is made. As for "sufficient storage" for the FORMAT variable, a two-digit width specification seems sufficient so each field's format code would be "A''dd''," - four characters. Only later Fortran allows the I0 format code, meaning that an integer will be written only with sufficient digits, not a fixed "sufficiency" of digits. A pity the code for this isn't just "I" rather than "I0" which implies a width of zero.

A difficulty now arises in terminating the format text with the closing bracket. This is done by searching for the sequence ", " and replacing the comma by the closing bracket. This is present only because format 21 specifies <MF> as a repeat count. If it were say 666 instead, after the last W value was written the format processing would cycle back to its next repetition, write out an "A", then stop since there is no W value to write via the "I0" format code. Thus, the format text ends ",A" and the INDEX fails to find the desired ", ". It could instead search for ",A " (just ",A" would stop on the second field's format code) but ", " seems clear. The <MF> usage is standard only for recent Fortran. Formatted output opportunities are many and options numerous. Other schemes could be used, such as the write statement specifying the text literals in its items, but that would require an implied-DO output list and it seemed nicer to be able to use the array specification W(1:MF) instead.

Given a FORMAT text that produces output in aligned columns makes it easy enough to meet the other requirements. To cause texts to be aligned left, append sufficient spaces to each output text, and for centred text, half that number. This relies on the special intrinsic function REPEAT(text,n) returning a varying number of characters - CHARACTER functions have to return a ''fixed'' number of characters, until the standardisation of varying-length strings in F2003 ''et seq''. Earlier Fortrans lack the REPEAT function, but its effect can be gained via something like CHARACTER*66 SPACE, where SPACE is set to spaces, and SPACE(1:N) is used where REPEAT(" ",N) is desired. And if messing with variable FORMAT is unwanted, the REPEAT scheme can be used for the right-justified output also.

```Fortran

      SUBROUTINE RAKE(IN,M,X,WAY)	!Casts forth text in fixed-width columns.
Collates column widths so that each column is wide enough for its widest member.
       INTEGER IN		!Fingers the input file.
       INTEGER M		!Maximum record length thereof.
       CHARACTER*1 X		!The delimiter, possibly a comma.
       INTEGER WAY		!Alignment style.
       INTEGER W(M + 1)		!If every character were X in the maximum-length record,
       INTEGER C(0:M + 1)	!Then M + 1 would be the maximum number of fields possible.
       CHARACTER*(M) ACARD	!A scratchpad big enough for the biggest.
       CHARACTER*(28 + 4*M) FORMAT	!Guess. Allow for "Ann," per field.
       INTEGER I		!A stepper.
       INTEGER L,LF		!Text fingers.
       INTEGER NF,MF		!Field counts.
       CHARACTER*6 WAYNESS(-1:+1)	!Some annotation may be helpful.
       PARAMETER (WAYNESS = (/"Left","Centre","Right"/))	!Using normal language.
       INTEGER LINPR	!The mouthpiece.
       COMMON LINPR	!Used all over.
        W = 0		!Maximum field widths so far seen.
        MF = 0		!Maximum number of fields to a record.
        C(0) = 0	!Syncopation for the first field's predecessor.
        WRITE (LINPR,*)	!Some separation.
        WRITE (LINPR,*) "Align ",WAYNESS(MIN(MAX(WAY,-1),+1))	!Explain, cautiously.

Chase through the file assessing the lengths of each field.
   10   READ (IN,11,END = 20) L,ACARD(1:L)	!Grab a record.
   11   FORMAT (Q,A)				!Working only up to its end.
        CALL LIZZIEBORDEN	!Find the chop points.
        W(1:NF) = MAX(W(1:NF),C(1:NF) - C(0:NF - 1) - 1)	!Thereby the lengths between.
        MF = MAX(MF,NF)		!Also want to know the most number of chops.
        GO TO 10		!Get the next record.

Concoct a FORMAT based on the maximum size of each field. Plus one.
   20   REWIND(IN)		!Back to the beginning.
        WRITE (FORMAT,21) W(1:MF) + 1	!Add one to meet the specified at least one space between columns.
   21   FORMAT ("(",<MF>("A",I0,","))	!Generates a sequence of An, items.
        LF = INDEX(FORMAT,", ")		!The last one has a trailing comma.
        IF (LF.LE.0) STOP "Format trouble!"	!Or, maybe not!
        FORMAT(LF:LF) = ")"			!Convert it to the closing bracket.
        WRITE (LINPR,*) "Format",FORMAT(1:LF)	!Present it.

Chug afresh, this time knowing the maximum length of each field.
   30   READ (IN,11,END = 40) L,ACARD(1:L)	!Place just the record's content.
        CALL LIZZIEBORDEN		!Find the chop points.
        SELECT CASE(WAY)	!What is to be done?
         CASE(-1)		!Shove leftwards by appending spaces.
          WRITE (LINPR,FORMAT) (ACARD(C(I - 1) + 1:C(I) - 1)//	!The chopped text.
     1     REPEAT(" ",W(I) - C(I) + C(I - 1) + 1),I = 1,NF)	!Some spaces.
         CASE( 0)		!Centre by appending half as many spaces.
          WRITE (LINPR,FORMAT) (ACARD(C(I - 1) + 1:C(I) - 1)//	!The chopped text.
     1     REPEAT(" ",(W(I) - C(I) + C(I - 1) + 1)/2),I = 1,NF)	!Some spaces.
         CASE(+1)		!Align rightwards is the default style.
          WRITE (LINPR,FORMAT) (ACARD(C(I - 1) + 1:C(I) - 1),I = 1,NF)	!So, just the texts.
         CASE DEFAULT		!This shouldn't happen.
         WRITE (LINPR,*) "Huh? WAY=",WAY	!But if it does,
         STOP "Unanticipated value for WAY!"	!Explain.
        END SELECT		!So much for that record.
        GO TO 30		!Go for another.
Closedown
   40   REWIND(IN)		!Be polite.
       CONTAINS	!This also marks the end of source for RAKE...
        SUBROUTINE LIZZIEBORDEN	!Take an axe to ACARD, chopping at X.
          NF = 0		!No strokes so far.
          DO I = 1,L		!So, step away.
            IF (ICHAR(ACARD(I:I)).EQ.ICHAR(X)) THEN	!Here?
              NF = NF + 1		!Yes!
              C(NF) = I		!The place!
            END IF		!So much for that.
          END DO		!On to the next.
          NF = NF + 1		!And the end of ACARD is also a chop point.
          C(NF) = L + 1		!As if here.
        END SUBROUTINE LIZZIEBORDEN	!She was aquitted.
      END SUBROUTINE RAKE	!So much raking over.

      INTEGER L,M,N	!To be determined the hard way.
      INTEGER LINPR,IN	!I/O unit numbers.
      COMMON LINPR	!Some of general note.
      LINPR = 6		!Standard output via this unit number.
      IN = 10		!Some unit number for the input file.
      OPEN (IN,FILE="Rake.txt",STATUS="OLD",ACTION="READ")	!For formatted input.
      N = 0		!No records read.
      M = 0		!Longest record so far.

    1 READ (IN,2,END = 10) L	!How long is this record?
    2 FORMAT (Q)	!Obviously, Q specifies the length, not a content field.
      N = N + 1		!Anyway, another record has been read.
      M = MAX(M,L)	!And this is the longest so far.
      GO TO 1		!Go back for more.

   10 REWIND (IN)	!We're ready now.
      WRITE (LINPR,*) N,"Recs, longest rec. length is ",M
      CALL RAKE(IN,M,"$",-1)	!Align left.
      CALL RAKE(IN,M,"$", 0)	!Centre.
      CALL RAKE(IN,M,"$",+1)	!Align right.
      END	!That's all.

```

Every line of output starts with a space, and if it were to be sent to a lineprinter, this would be used as the carriage control character (meaning, advance one line then print the rest) - the first column does not need to be set off by one space from the previous column,  but rather than devise special treatment it is spaced off anyway. The free-format output statements also start with a space. Output:

```txt

          6 Recs, longest rec. length is           66

Align Left
Format(A11,A11,A11,A7,A7,A10,A11,A9,A8,A8,A7,A5,A1)
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

Align Centre
Format(A11,A11,A11,A7,A7,A10,A11,A9,A8,A8,A7,A5,A1)
   Given        a        text     file    of      many     lines,     where   fields  within    a   line
    are    delineated     by        a   single  'dollar' character,   write     a    program
   that      aligns      each    column   of     fields      by     ensuring   that   words    in   each
  column       are     separated   by     at     least       one     space.
 Further,     allow       for     each   word      in         a      column     to      be   either left
justified,    right   justified,   or   center justified   within      its   column.

Align Right
Format(A11,A11,A11,A7,A7,A10,A11,A9,A8,A8,A7,A5,A1)
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Sub Split(s As String, sep As String, result() As String)
  Dim As Integer i, j, count = 0
  Dim temp As String
  Dim As Integer position(Len(s) + 1)
  position(0) = 0
  For i = 0 To Len(s) - 1
    For j = 0 To Len(sep) - 1
      If s[i] = sep[j] Then
        count += 1
        position(count) = i + 1
      End If
    Next j
  Next i
  position(count + 1) = Len(s) + 1
  Redim result(count)
  For i = 1 To count + 1
    result(i - 1) = Mid(s, position(i - 1) + 1, position(i) - position(i - 1) - 1)
  Next
End Sub

Sub CSet(buffer As String, s As Const String)
  Dim As Integer bLength = Len(buffer)
  Dim As Integer sLength = Len(s)
  Dim As Integer diff, lSpaces
  If sLength >= bLength Then
    LSet buffer, s
  Else
    diff = bLength - sLength
    lSpaces = diff \ 2
    LSet buffer, Space(lSpaces) + s
  End If
End Sub

Dim lines() As String
Dim count As Integer = 0

Open "align_columns.txt" For Input As #1

While Not Eof(1)
  Redim Preserve lines(count)
  Line Input #1, lines(count)
  count +=1
Wend

Close #1

Dim As Integer i,j, length, numColumns = 0
Dim As Integer numLines = UBound(lines) + 1
Dim fields() As String

' Work out the maximum number of columns
For i = 0 To numLines - 1
  Erase fields
  Split RTrim(lines(i), "$"), "$", fields()
  length = UBound(fields) + 1
  If length > numColumns Then numColumns = length
Next

' Split lines into fields and work out maximum size of each column
Dim matrix(numLines - 1, numColumns - 1) As String
Dim columnSizes(numColumns - 1) As Integer

For i = 0 To numLines - 1
  Erase fields
  Split RTrim(lines(i), "$"), "$", fields()
  For j = 0 To UBound(fields)
    matrix(i, j) = fields(j)
    length = Len(fields(j))
    If  length > columnSizes(j) Then columnSizes(j) = length
  Next j
Next i

Dim buffer As String

'Separate each column by 2 spaces
Open "align_left_columns.txt" For Output As #1
Open "align_right_columns.txt" For Output As #2
Open "align_center_columns.txt" For Output As #3

For i = 0 To UBound(matrix, 1)
  For j = 0 To UBound(matrix, 2)
    buffer = Space(columnSizes(j))
    LSet buffer, matrix(i, j)
    Print #1, buffer;
    RSet buffer, matrix(i, j)
    Print #2, buffer;
    CSet buffer, matrix(i, j)
    Print #3, buffer;
    If j < UBound(matrix, 2) Then
      Print #1, "  "; : Print #2, "  "; : Print #3, "  ";
    End If
  Next j
  Print #1, : Print #2, : Print #3,
Next i

Close #1 : Close #2 : Close #3
```


{{out}}

```txt
--- align_columns.txt

Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.

--- align_left_columns.txt

Given       a           text        file    of      many       lines,      where     fields   within   a       line
are         delineated  by          a       single  'dollar'   character,  write     a        program
that        aligns      each        column  of      fields     by          ensuring  that     words    in      each
column      are         separated   by      at      least      one         space.
Further,    allow       for         each    word    in         a           column    to       be       either  left
justified,  right       justified,  or      center  justified  within      its       column.

--- align_right_columns.txt

     Given           a        text    file      of       many      lines,     where   fields   within       a  line
       are  delineated          by       a  single   'dollar'  character,     write        a  program
      that      aligns        each  column      of     fields          by  ensuring     that    words      in  each
    column         are   separated      by      at      least         one    space.
  Further,       allow         for    each    word         in           a    column       to       be  either  left
justified,       right  justified,      or  center  justified      within       its  column.

--- align_center_columns.txt

  Given         a          text      file     of      many       lines,     where    fields   within     a     line
   are      delineated      by        a     single  'dollar'   character,   write       a     program
   that       aligns       each     column    of     fields        by      ensuring   that     words     in    each
  column       are      separated     by      at      least       one       space.
 Further,     allow        for       each    word      in          a        column     to       be     either  left
justified,    right     justified,    or    center  justified    within      its     column.

```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=023b4c5144d45e047abe02ebf5c4525a Click this link to run this code]'''

```gambas
Public Sub Main()                                                             'Written in Gambas 3.9.2 as a Command line Application - 15/03/2017
Dim siCount, siCounter, siLength As Short                                     'Counters
Dim siLongest As Short = -1                                                   'To store the longest 'Word'
Dim sLine, sRows As New String[]                                              'Arrays
Dim sTemp, sAlign As String                                                   'Temp strings
Dim sInput As String = "Given$a$text$file$of$many$lines, $where$fields$within$a$line$" & "\n"
"are$delineated$by$a$single$ 'dollar'$character,$write$a$program" & "\n"
"that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$" & "\n"
"column$are$separated$by$at$least$one$space." & "\n"
"Further, $allow$for$each$word$in$a$column$to$be$either$left$" & "\n"
"justified, $right$justified, $or$center$justified$within$its$column."        'Main string (with End Of Line characters added)

For Each sTemp In Split(sInput, "\n")                                         'For each Line (split by End of Line character)..
  sLine.add(sTemp)                                                            'Add the Line to sLine array
Next

For siCount = 0 To sLine.Max                                                  'For each of Lines in the array..
  For Each sTemp In Split(sLine[siCount], "$")                                'For each 'Word' in the Line (Split by the '$')
    siLength = Len(sTemp)                                                     'Store the length of the current 'Word'
    If siLength > siLongest Then siLongest = siLength                         'Make sure siLength has the length of the longest 'Word'
    sRows.add(Trim(sTemp))                                                    'Create an array of the 'Words'
  Next
  sRows.add("\n")                                                             'Add a End Of Line character to the sRows array
Next

For siCounter = 0 To 2                                                        'For each alignment (Left, Right and Centre)
  For Each sTemp In sRows                                                     'For each 'Word' in the sRows array..
    If sTemp = "\n" Then                                                      'If it's a End Of Line character then..
      Print                                                                   'Print
      Continue                                                                'Jump to the next iteration of the For Next Loop
    Endif
    If siCounter = 0 Then Print sTemp & Space(siLongest - Len(sTemp));        'Print control for Left align
    If siCounter = 1 Then Print Space(siLongest - Len(sTemp)) & sTemp;        'Print control for Right align
    If siCounter = 2 Then                                                     'Print control for Centre align
      siCount = (siLongest - Len(sTemp)) / 2                                  'Difference between the length of the longest 'Word' and the current 'Word' / 2
      sAlign = Space(siCount) & sTemp & Space(siCount)                        'Put the string together for printing
      If Len(sAlign) < siLongest Then sAlign &= " "                           'Check it's the correct length if not add a space on the end
      Print sAlign;                                                           'Print the 'Word'
    Endif
  Next
  Print                                                                       'Print an empty line between each alignment list
Next

End
```


{{out}}

```txt

Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.

     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

```



## Go


```go
package main

import (
    "fmt"
    "strings"
)

const text = `Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.`

type formatter struct {
    text  [][]string
    width []int
}

func newFormatter(text string) *formatter {
    var f formatter
    for _, line := range strings.Split(text, "\n") {
        words := strings.Split(line, "$")
        for words[len(words)-1] == "" {
            words = words[:len(words)-1]
        }
        f.text = append(f.text, words)
        for i, word := range words {
            if i == len(f.width) {
                f.width = append(f.width, len(word))
            } else if len(word) > f.width[i] {
                f.width[i] = len(word)
            }
        }
    }
    return &f
}

const (
    left = iota
    middle
    right
)

func (f formatter) print(j int) {
    for _, line := range f.text {
        for i, word := range line {
            fmt.Printf("%-*s ", f.width[i], fmt.Sprintf("%*s",
                len(word)+(f.width[i]-len(word))*j/2, word))
        }
        fmt.Println("")
    }
    fmt.Println("")
}

func main() {
    f := newFormatter(text)
    f.print(left)
    f.print(middle)
    f.print(right)
}
```


```txt

Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

```



## Groovy

Solution:

```groovy
def alignColumns = { align, rawText ->
    def lines = rawText.tokenize('\n')
    def words = lines.collect { it.tokenize(/\$/) }
    def maxLineWords = words.collect {it.size()}.max()
    words = words.collect { line -> line + [''] * (maxLineWords - line.size()) }
    def columnWidths = words.transpose().collect{ column -> column.collect { it.size() }.max() }

    def justify = [   Right  : { width, string -> string.padLeft(width) },
                            Left   : { width, string -> string.padRight(width) },
                            Center : { width, string -> string.center(width) }      ]
    def padAll = { pad, colWidths, lineWords -> [colWidths, lineWords].transpose().collect { pad(it) + ' ' } }

    words.each { padAll(justify[align], columnWidths, it).each { print it }; println() }
}
```


Test Program:

```groovy
def rawTextInput = '''Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.'''

['Left', 'Center', 'Right'].each { align ->
    println "${align} Justified:"
    alignColumns(align, rawTextInput)
    println()
}
```


{{out}}
<pre style="height:25ex;overflow:scroll">Left Justified:
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

Center Justified:
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

Right Justified:
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.
```



## Harbour



```visualfoxpro

PROCEDURE Main()
   LOCAL a := { "Given$a$text$file$of$many$lines,$where$fields$within$a$line$",;
                "are$delineated$by$a$single$'dollar'$character,$write$a$program",;
                "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$",;
                "column$are$separated$by$at$least$one$space.",;
                "Further,$allow$for$each$word$in$a$column$to$be$either$left$",;
                "justified,$right$justified,$or$center$justified$within$its$column." }
   LOCAL e, nMax

   // remove trailing dollars
   AEval( a, {|e,n| Iif( Right(e,1)=="$", a[n] := hb_StrShrink( e, 1 ), NIL ) } )

   // find max word length
   nMax := 0
   AEval( a, {|e| AEval( hb_Atokens( e, "$"), {|i| nMax := Max( nMax, Len(i) )} ) } )
   nMax++

   // start printing, padding words as needed
   ?
   ? "----Left aligned columns----"
   FOR EACH e IN a
      ?
      AEval( hb_Atokens( e, "$"), {|i| QQout( PadR(i, nMax) )} )
   NEXT

   ?
   ? "----Center aligned columns----"
   FOR EACH e IN a
      ?
      AEval( hb_Atokens( e, "$"), {|i| QQout( PadC(i, nMax) )} )
   NEXT

   ?
   ? "----Right aligned columns----"
   FOR EACH e IN a
      ?
      AEval( hb_Atokens( e, "$"), {|i| QQout( PadL(i, nMax) )} )
   NEXT

   RETURN

```

Output:

```text
----Left aligned columns----
Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.

----Center aligned columns----
   Given        a        text       file        of        many      lines,      where     fields     within        a        line
    are    delineated     by          a       single    'dollar'  character,    write        a       program
   that      aligns      each      column       of       fields       by      ensuring     that       words       in        each
  column       are     separated     by         at        least       one      space.
 Further,     allow       for       each       word        in          a       column       to         be       either      left
justified,    right   justified,     or       center    justified   within       its      column.

----Right aligned columns----
      Given          a       text       file         of       many     lines,      where     fields     within          a       line
        are delineated         by          a     single   'dollar' character,      write          a    program
       that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
     column        are  separated         by         at      least        one     space.
   Further,      allow        for       each       word         in          a     column         to         be     either       left
 justified,      right justified,         or     center  justified     within        its    column.
```



## Haskell


```haskell
import Data.List (unfoldr, transpose)
import Control.Arrow (second)

dat =
  "Given$a$text$file$of$many$lines,$where$fields$within$a$line$\n" ++
  "are$delineated$by$a$single$'dollar'$character,$write$a$program\n" ++
  "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$\n" ++
  "column$are$separated$by$at$least$one$space.\n" ++
  "Further,$allow$for$each$word$in$a$column$to$be$either$left$\n" ++
  "justified,$right$justified,$or$center$justified$within$its$column.\n"

brkdwn =
  takeWhile (not . null) . unfoldr (Just . second (drop 1) . span ('$' /=))

format j ls = map (unwords . zipWith align colw) rows
  where
    rows = map brkdwn $ lines ls
    colw = map (maximum . map length) . transpose $ rows
    align cw w =
      case j of
        'c' -> replicate l ' ' ++ w ++ replicate r ' '
        'r' -> replicate dl ' ' ++ w
        'l' -> w ++ replicate dl ' '
      where
        dl = cw - length w
        (l, r) = (dl `div` 2, dl - l)
```

{{out}}

```txt

*Main> mapM_ putStrLn $ format 'c' dat
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.
```



Or, using '''Text''' and its functions as an alternative to '''[Char]''' strings:


```haskell
import Prelude as P
import Data.Text as T
       (Text, pack, unpack, splitOn, unlines, unwords, length,
        justifyLeft, justifyRight, center)
import Data.List (transpose, zip, maximumBy)
import Data.Ord (comparing)

rows :: [[Text]]
rows =
  (splitOn (pack "$") . pack) <$>
  [ "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
  , "are$delineated$by$a$single$'dollar'$character,$write$a$program"
  , "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
  , "column$are$separated$by$at$least$one$space."
  , "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
  , "justified,$right$justified,$or$center$justified$within$its$column."
  ]

cols :: [[Text]]
cols =
  transpose $
  ((++) <*>
   (flip P.replicate (pack []) .
    (-) (maximum (P.length <$> rows)) . P.length)) <$>
  rows

main :: IO ()
main =
  mapM_ putStrLn $
  [ (\cols f ->
        (unpack . T.unlines) $
        T.unwords <$> transpose ((\(xs, n) -> f (n + 1) ' ' <$> xs) <$> cols))
      (zip cols ((T.length . maximumBy (comparing T.length)) <$> cols))
  ] <*>
  [justifyLeft, justifyRight, center]
```

{{Out}}

```txt
Given       a           text        file    of      many       lines,      where     fields   within   a       line
are         delineated  by          a       single  'dollar'   character,  write     a        program
that        aligns      each        column  of      fields     by          ensuring  that     words    in      each
column      are         separated   by      at      least      one         space.
Further,    allow       for         each    word    in         a           column    to       be       either  left
justified,  right       justified,  or      center  justified  within      its       column.

      Given           a        text    file      of       many      lines,     where   fields   within       a  line
        are  delineated          by       a  single   'dollar'  character,     write        a  program
       that      aligns        each  column      of     fields          by  ensuring     that    words      in  each
     column         are   separated      by      at      least         one    space.
   Further,       allow         for    each    word         in           a    column       to       be  either  left
 justified,       right  justified,      or  center  justified      within       its  column.

   Given         a          text      file     of      many       lines,     where    fields   within     a     line
    are      delineated      by        a     single  'dollar'   character,   write       a     program
    that       aligns       each     column    of     fields        by      ensuring   that     words     in    each
   column       are      separated     by      at      least       one       space.
  Further,     allow        for       each    word      in          a        column     to       be     either  left
 justified,    right     justified,    or    center  justified    within      its     column.
```



## HicEst

A file opened with a Format option describing the column format(s) can be addressed like a standard in-memory array. In addition the DLG function ([http://www.HicEst.com/MatrixExplorer.htm MatrixExplorer]) allows this text/numeric file to be edited or visualized in many ways, but string columns are always left adjusted while numeric columns are right adjusted. Export is possible.

```HicEst

CHARACTER Fnam = "\HicEst\Rosetta\Align columns.txt"

   OPEN(FIle=Fnam, Format="12$", LENgth=rows)
! call the DLG function in MatrixExplorer mode:
   DLG(Edit=Fnam, Format='12A10') ! left adjusted, 12 columns, 10 spaces each

! or the standard way:
   CALL Align( "LLLLLLLLLLL ", Fnam, rows)   ! left   align
   CALL Align( "CCCCCCCCCCC ", Fnam, rows)   ! center align
   CALL Align( "RRRRRRRRRRR ", Fnam, rows)   ! right  align
END

SUBROUTINE Align(picture, filename, rows)
   CHARACTER picture, filename
   CHARACTER out*400, txt*20

   W = LEN(picture)
   DO i = 1, rows
     out = " "
     DO j = 0, 100
       txt = filename(i, j+1, *9) ! on error branch to label 9
       WRITE(Text=out(j*W+1 : ), Format=picture) txt
     ENDDO
 9 CONTINUE
   WRITE() out
   ENDDO
END
```


```txt
Given       a           text        file        of          many        lines,      where       fields      within      a           line
are         delineated  by          a           single      'dollar'    character,  write       a           program
that        aligns      each        column      of          fields      by          ensuring    that        words       in          each
column      are         separated   by          at          least       one         space.
Further,    allow       for         each        word        in          a           column      to          be          either      left
justified,  right       justified,  or          center      justified   within      its         column.
   Given         a         text        file         of         many       lines,       where      fields      within         a         line
    are     delineated      by           a        single     'dollar'   character,     write         a        program
   that       aligns       each       column        of        fields        by       ensuring      that        words        in         each
  column        are      separated      by          at         least        one       space.
 Further,      allow        for        each        word         in           a        column        to          be        either       left
justified,     right    justified,      or        center     justified    within        its       column.
      Given           a        text        file          of        many      lines,       where      fields      within           a        line
        are  delineated          by           a      single    'dollar'  character,       write           a     program
       that      aligns        each      column          of      fields          by    ensuring        that       words          in        each
     column         are   separated          by          at       least         one      space.
   Further,       allow         for        each        word          in           a      column          to          be      either        left
 justified,       right  justified,          or      center   justified      within         its     column.
```


=={{header|Icon}} and {{header|Unicon}}==
An argument of <tt>left</tt>, <tt>center</tt>, or <tt>right</tt> controls the
column alignment.  The default is left-alignment.

```icon
global width

procedure main(args)
    lines := []
    width := 0
    format := left
    match("left"|"right"|"center", format <- !args)
    every put(lines,prepare(!&input))
    display(lines, proc(format,3))
end

procedure prepare(lines)
    line := []
    lines ? {
        while (not pos(0)) & (field := tab(upto('$')|0)) do {
            put(line, field)
            width <:= *field
            move(1)
            }
        }
    return line
end

procedure display(lines, format)
    width +:= 1
    every line := !lines do {
        every writes(format(!line, width))
        write()
        }
end
```


Sample run:

```txt
->align right <align.txt
      Given          a       text       file         of       many     lines,      where     fields     within          a       line
        are delineated         by          a     single   'dollar' character,      write          a    program
       that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
     column        are  separated         by         at      least        one     space.
   Further,      allow        for       each       word         in          a     column         to         be     either       left
 justified,      right justified,         or     center  justified     within        its    column.
->
```



## J

'''Solution'''

```j
'LEFT CENTER RIGHT'=: i.3                NB. justification constants

NB.* alignCols v Format delimited text in justified columns
NB. y:          text to format
NB.                 rows marked by last character in text
NB.                 columns marked by $
NB. optional x: justification. Default is LEFT
NB. result:     text table
alignCols=: verb define
  LEFT alignCols y                       NB. default
:
  global=. dyad def'9!:x y'each
  oldbox=. 6 16 global '';''             NB. save settings
  7 17 global (11#' ');,~x               NB. apply new settings
  result=. _2{:\ ": <;._2 @:,&'$';._2 y  NB. parse & format text
  7 17 global oldbox                     NB. restore settings
  result
)
```


'''Example''':

```j
   text=: noun define
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
)

   alignCols text           NB. default justification
 Given      a          text       file   of     many      lines,     where    fields  within  a      line
 are        delineated by         a      single 'dollar'  character, write    a       program
 that       aligns     each       column of     fields    by         ensuring that    words   in     each
 column     are        separated  by     at     least     one        space.
 Further,   allow      for        each   word   in        a          column   to      be      either left
 justified, right      justified, or     center justified within     its      column.

   CENTER alignCols text    NB. specify desired justification as left argument
   Given        a         text     file    of     many      lines,    where   fields  within    a    line
    are     delineated     by       a    single 'dollar'  character,  write      a    program
    that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
   column      are     separated    by     at     least      one      space.
  Further,    allow       for      each   word     in         a       column    to      be    either left
 justified,   right    justified,   or   center justified   within     its    column.
```



## Java

{{works with|Java|7}}

{{libheader|Apache Commons Lang}}


```Java
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

/**
 * Aligns fields into columns, separated by "|"
 */
public class ColumnAligner {
    private List<String[]> words = new ArrayList<>();
    private int columns = 0;
    private List<Integer> columnWidths = new ArrayList<>();

    /**
     * Initialize columns aligner from lines in a single string
     *
     * @param s
     *            lines in a single string. Empty string does form a column.
     */
    public ColumnAligner(String s) {
        String[] lines = s.split("\\n");
        for (String line : lines) {
            processInputLine(line);
        }
    }

    /**
     * Initialize columns aligner from lines in a list of strings
     *
     * @param lines
     *            lines in a single string. Empty string does form a column.
     */
    public ColumnAligner(List<String> lines) {
        for (String line : lines) {
            processInputLine(line);
        }
    }

    private void processInputLine(String line) {
        String[] lineWords = line.split("\\$");
        words.add(lineWords);
        columns = Math.max(columns, lineWords.length);
        for (int i = 0; i < lineWords.length; i++) {
            String word = lineWords[i];
            if (i >= columnWidths.size()) {
                columnWidths.add(word.length());
            } else {
                columnWidths.set(i, Math.max(columnWidths.get(i), word.length()));
            }
        }
    }

    interface AlignFunction {
        String align(String s, int length);
    }

    /**
     * Left-align all columns
     *
     * @return Lines, terminated by "\n" of columns, separated by "|"
     */
    public String alignLeft() {
        return align(new AlignFunction() {
            @Override
            public String align(String s, int length) {
                return StringUtils.rightPad(s, length);
            }
        });
    }

    /**
     * Right-align all columns
     *
     * @return Lines, terminated by "\n" of columns, separated by "|"
     */
    public String alignRight() {
        return align(new AlignFunction() {
            @Override
            public String align(String s, int length) {
                return StringUtils.leftPad(s, length);
            }
        });
    }

    /**
     * Center-align all columns
     *
     * @return Lines, terminated by "\n" of columns, separated by "|"
     */
    public String alignCenter() {
        return align(new AlignFunction() {
            @Override
            public String align(String s, int length) {
                return StringUtils.center(s, length);
            }
        });
    }

    private String align(AlignFunction a) {
        StringBuilder result = new StringBuilder();
        for (String[] lineWords : words) {
            for (int i = 0; i < lineWords.length; i++) {
                String word = lineWords[i];
                if (i == 0) {
                    result.append("|");
                }
                result.append(a.align(word, columnWidths.get(i)) + "|");
            }
            result.append("\n");
        }
        return result.toString();
    }

    public static void main(String args[]) throws IOException {
        if (args.length < 1) {
            System.out.println("Usage: ColumnAligner file [left|right|center]");
            return;
        }
        String filePath = args[0];
        String alignment = "left";
        if (args.length >= 2) {
            alignment = args[1];
        }
        ColumnAligner ca = new ColumnAligner(Files.readAllLines(Paths.get(filePath), StandardCharsets.UTF_8));
        switch (alignment) {
        case "left":
            System.out.print(ca.alignLeft());
            break;
        case "right":
            System.out.print(ca.alignRight());
            break;
        case "center":
            System.out.print(ca.alignCenter());
            break;
        default:
            System.err.println(String.format("Error! Unknown alignment: '%s'", alignment));
            break;
        }
    }
}
```



## JavaScript



### Imperative



```JavaScript

var justification="center",
input=["Given$a$text$file$of$many$lines,$where$fields$within$a$line$",
"are$delineated$by$a$single$'dollar'$character,$write$a$program",
"that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$",
"column$are$separated$by$at$least$one$space.",
"Further,$allow$for$each$word$in$a$column$to$be$either$left$",
"justified,$right$justified,$or$center$justified$within$its$column."],
x,y,cols,max,cols=0,diff,left,right

String.prototype.repeat=function(n){return new Array(1 + parseInt(n)).join(this);}

for(x=0;x<input.length;x++) {
 input[x]=input[x].split("$");
 if(input[x].length>cols) cols=input[x].length;
}
for(x=0;x<cols;x++) {
 max=0;
 for(y=0;y<input.length;y++) if(input[y][x]&&max<input[y][x].length) max=input[y][x].length;
 for(y=0;y<input.length;y++)
  if(input[y][x]) {
   diff=(max-input[y][x].length)/2;
   left=" ".repeat(Math.floor(diff));
   right=" ".repeat(Math.ceil(diff));
   if(justification=="left") {right+=left;left=""}
   if(justification=="right") {left+=right;right=""}
   input[y][x]=left+input[y][x]+right;
  }
}
for(x=0;x<input.length;x++) input[x]=input[x].join(" ");
input=input.join("\n");
document.write(input);
```



### Functional



```JavaScript
//break up each string by '$'. The assumption is that the user wants the trailing $.
var data = [
  "Given$a$text$file$of$many$lines,$where$fields$within$a$line$",
  "are$delineated$by$a$single$'dollar'$character,$write$a$program",
  "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$",
  "column$are$separated$by$at$least$one$space.",
  "Further,$allow$for$each$word$in$a$column$to$be$either$left$",
  "justified,$right$justified,$or$center$justified$within$its$column."
].map(function (str) { return str.split('$'); })

//boilerplate: get longest array or string in array
var getLongest = function (arr) {
  return arr.reduce(function (acc, item) { return acc.length > item.length ? acc : item; }, 0);
};

//boilerplate: this function would normally be in a library like underscore, lodash, or ramda
var zip = function (items, toInsert) {
  toInsert = (toInsert === undefined) ? null : toInsert;
  var longestItem = getLongest(items);
  return longestItem.map(function (_unused, index) {
    return items.map(function (item) {
      return item[index] === undefined ? toInsert : item[index];
    });
  });
};

//here's the part that's not boilerplate
var makeColumns = function (formatting, data) {
  var zipData = zip(data, '');
  var makeSpaces = function (num) { return new Array(num + 1).join(' '); };
  var formattedCols = zipData.map(function (column) {
    var maxLen = getLongest(column).length;//find the maximum word length
    if (formatting === 'left') {
      return column.map(function (word) { return word + makeSpaces(maxLen - word.length); });
    } else if (formatting === 'right') {
      return column.map(function (word) { return makeSpaces(maxLen - word.length) + word; });
    } else {
      return column.map(function (word) {
        var spaces = maxLen - word.length,
            first = ~~(spaces / 2),
            last = spaces - first;
        return makeSpaces(first) + word + makeSpaces(last);
      });
    }
  });

  return zip(formattedCols).map(function (row) { return row.join(' '); }).join('\n');
};
```




Or (ES5) using transpose and zipWith:


```JavaScript
(function (strText) {
    'use strict';

    // [[a]] -> [[a]]
    function transpose(lst) {
        return lst[0].map(function (_, iCol) {
            return lst.map(function (row) {
                return row[iCol];
            })
        });
    }

    // (a -> b -> c) -> [a] -> [b] -> [c]
    function zipWith(f, xs, ys) {
        return xs.length === ys.length ? (
            xs.map(function (x, i) {
                return f(x, ys[i]);
            })
        ) : undefined;
    }

    // (a -> a -> Ordering) -> [a] -> a
    function maximumBy(f, xs) {
        return xs.reduce(function (a, x) {
            return a === undefined ? x : (
                f(x) > f(a) ? x : a
            );
        }, undefined)
    }

    // [String] -> String
    function widest(lst) {
        return maximumBy(length, lst)
            .length;
    }

    // [[a]] -> [[a]]
    function fullRow(lst, n) {
        return lst.concat(Array.apply(null, Array(n - lst.length))
            .map(function () {
                return ''
            }));
    }

    // String -> Int -> String
    function nreps(s, n) {
        var o = '';
        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o += s;
            n >>= 1;
            s += s;
        }
        return o + s;
    }

    // [String] -> String
    function unwords(xs) {
        return xs.join('  ');
    }

    // [String] -> String
    function unlines(xs) {
        return xs.join('\n');
    }

    // [a] -> Int
    function length(xs) {
        return xs.length;
    }

    // -- Int -> [String] -> [[String]]
    function padWords(n, lstWords, eAlign) {
        return lstWords.map(function (w) {
            var lngPad = n - w.length;

            return (
                    (eAlign === eCenter) ? (function () {
                        var lngHalf = Math.floor(lngPad / 2);

                        return [
                            nreps(' ', lngHalf), w,
                            nreps(' ', lngPad - lngHalf)
                        ];
                    })() : (eAlign === eLeft) ?
                        ['', w, nreps(' ', lngPad)] :
                        [nreps(' ', lngPad), w, '']
                )
                .join('');
        });
    }

    // MAIN

    var eLeft = -1,
        eCenter = 0,
        eRight = 1;

    var lstRows = strText.split('\n')
        .map(function (x) {
            return x.split('$');
        }),

        lngCols = widest(lstRows),
        lstCols = transpose(lstRows.map(function (r) {
            return fullRow(r, lngCols)
        })),
        lstColWidths = lstCols.map(widest);

    // THREE PARAGRAPHS, WITH VARIOUS WORD COLUMN ALIGNMENTS:

    return [eLeft, eRight, eCenter]
        .map(function (eAlign) {
            var fPad = function (n, lstWords) {
                return padWords(n, lstWords, eAlign);
            };

            return transpose(
                    zipWith(fPad, lstColWidths, lstCols)
                )
                .map(unwords);
        })
        .map(unlines)
        .join('\n\n');

})(
    "Given$a$text$file$of$many$lines,$where$fields$within$a$line$\n\
are$delineated$by$a$single$'dollar'$character,$write$a$program\n\
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$\n\
column$are$separated$by$at$least$one$space.\n\
Further,$allow$for$each$word$in$a$column$to$be$either$left$\n\
justified,$right$justified,$or$center$justified$within$its$column."
);
```


{{Out}}


```txt
Given       a           text        file    of      many       lines,      where     fields   within   a       line
are         delineated  by          a       single  'dollar'   character,  write     a        program
that        aligns      each        column  of      fields     by          ensuring  that     words    in      each
column      are         separated   by      at      least      one         space.
Further,    allow       for         each    word    in         a           column    to       be       either  left
justified,  right       justified,  or      center  justified  within      its       column.

     Given           a        text    file      of       many      lines,     where   fields   within       a  line
       are  delineated          by       a  single   'dollar'  character,     write        a  program
      that      aligns        each  column      of     fields          by  ensuring     that    words      in  each
    column         are   separated      by      at      least         one    space.
  Further,       allow         for    each    word         in           a    column       to       be  either  left
justified,       right  justified,      or  center  justified      within       its  column.

  Given         a          text      file     of      many       lines,     where    fields   within     a     line
   are      delineated      by        a     single  'dollar'   character,   write       a     program
   that       aligns       each     column    of     fields        by      ensuring   that     words     in    each
  column       are      separated     by      at      least       one       space.
 Further,     allow        for       each    word      in          a        column     to       be     either  left
justified,    right     justified,    or    center  justified    within      its     column.
```



## jq

{{ Works with|jq|1.4}}
The key to the following implementation is the filter named "transpose", which is defined to work on a possibly jagged matrix.

```jq
# transpose a possibly jagged matrix
def transpose:
  if . == [] then []
  else (.[1:] | transpose) as $t
  | .[0] as $row
  | reduce range(0; [($t|length), (.[0]|length)] | max) as $i
      ([]; . + [ [ $row[$i] ] + $t[$i] ])
  end;

# left/right/center justification of strings:
def ljust(width): . + " " * (width - length);

def rjust(width): " " * (width - length) + .;

def center(width):
  (width - length) as $pad
  | if $pad <= 0 then .
    else ($pad / 2 | floor) as $half
    | $half * " " + . + ($pad-$half) * " "
    end ;

# input: a single string, which includes newlines to separate lines, and $ to separate phrases;
# method must be "left" "right" or anything else for central justification.
def format(method):
  def justify(width):
    if   method == "left"  then ljust(width)
    elif method == "right" then rjust(width)
    else center(width)
    end;

  # max_widths: input: an array of strings, each with "$" as phrase-separator;
  # return the appropriate column-wise maximum lengths
  def max_widths:
    map(split("$") | map(length))
    | transpose | map(max) ;

  split("\n") as $input
  | $input
  | (max_widths | map(.+1)) as $widths
  | map( split("$") | . as $line | reduce range(0; length) as $i
      (""; . + ($line[$i]|justify($widths[$i])) ))
  | join("\n")
;
```

'''Example''':

```jq
"Center:", format("center"), "",
"Left:",   format("left"),   "",
"Right:",  format("right")
```

{{Out}}
<div style="overflow:scroll; height:200px;">
```sh
$ jq -M -R -r -s -f Align_columns.jq Align_columns.txt
Center:
   Given        a        text     file    of      many     lines,     where   fields  within    a   line
    are    delineated     by        a   single  'dollar' character,   write     a    program
   that      aligns      each    column   of     fields      by     ensuring   that   words    in   each
  column       are     separated   by     at     least       one     space.
 Further,     allow       for     each   word      in         a      column     to      be   either left
justified,    right   justified,   or   center justified   within      its   column.

Left:
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

Right:
      Given          a       text   file     of      many     lines,    where  fields  within      a line
        are delineated         by      a single  'dollar' character,    write       a program
       that     aligns       each column     of    fields         by ensuring    that   words     in each
     column        are  separated     by     at     least        one   space.
   Further,      allow        for   each   word        in          a   column      to      be either left
 justified,      right justified,     or center justified     within      its column.

```
</div>


## Jsish

From Javascript ES5 entry.

```javascript
/* Align columns, in Jsish */
function alignColumns(phrases:array, just:string) {
    var x, y, max, diff, left, right, cols=0;

    for(x=0; x<phrases.length; x++) {
        phrases[x] = phrases[x].split("$");
        if (phrases[x].length>cols) cols=phrases[x].length;
    }

    for (x=0; x<cols; x++) {
        max = 0;
        for (y=0; y<phrases.length; y++) if (phrases[y][x] && max<phrases[y][x].length) max = phrases[y][x].length;
        for (y=0; y<phrases.length; y++) {
            if (phrases[y][x]) {
                diff = (max-phrases[y][x].length)/2;
                left = " ".repeat(Math.floor(diff));
                right = " ".repeat(Math.ceil(diff));
                if (just == "left") { right += left; left=""; }
                if (just == "right") { left += right; right=""; }
                phrases[y][x] = left + phrases[y][x] + right;
            }
        }
    }
    for (x=0; x<phrases.length; x++) phrases[x] = phrases[x].join(" ");
    phrases = phrases.join("\n");
    return phrases;
}

if (Interp.conf('unitTest')) {
    var phrases = ["Given$a$text$file$of$many$lines,$where$fields$within$a$line$",
        "are$delineated$by$a$single$'dollar'$character,$write$a$program",
        "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$",
        "column$are$separated$by$at$least$one$space.",
        "Further,$allow$for$each$word$in$a$column$to$be$either$left$",
        "justified,$right$justified,$or$center$justified$within$its$column."];

    for (var just of ['left', 'center', 'right']) {
        var trial = phrases.slice(0);
        puts(just);
        puts(alignColumns(trial, just), '\n');
    }
}
```


{{out}}

```txt
prompt$ jsish --U alignColumns.jsi
left
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

center
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

right
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.
```




## Julia

{{trans|Python}}

```julia
txt = """Given\$a\$txt\$file\$of\$many\$lines,\$where\$fields\$within\$a\$line\$
are\$delineated\$by\$a\$single\$'dollar'\$character,\$write\$a\$program
that\$aligns\$each\$column\$of\$fields\$by\$ensuring\$that\$words\$in\$each\$
column\$are\$separated\$by\$at\$least\$one\$space.
Further,\$allow\$for\$each\$word\$in\$a\$column\$to\$be\$either\$left\$
justified,\$right\$justified,\$or\$center\$justified\$within\$its\$column."""

# left/right/center justification of strings:
ljust(s, width) = s * " "^max(0, width - length(s))
rjust(s, width) = " "^max(0, width - length(s)) * s
function center(s, width)
  pad = width - length(s)
  if pad <= 0
    return s
  else
    pad2 = div(pad, 2)
    return " "^pad2 * s * " "^(pad - pad2)
  end
end

parts = [split(rstrip(line, '$'), '$') for line in split(txt, '\n')]

max_widths = zeros(Int, maximum(length, parts))
for line in parts
  for (i, word) in enumerate(line)
    max_widths[i] = max(max_widths[i], length(word))
  end
end
max_widths += 1 # separate cols by at least one space

for (label, justify) in (("Left", ljust), ("Right",rjust), ("Center",center))
  println(label, " column-aligned output:")
  for line in parts
    for (j, word) in enumerate(line)
      print(justify(word, max_widths[j]))
    end
    println()
  end
  println("-"^sum(max_widths))
end
```

{{out}}

```txt

Left column-aligned output:
Given      a          txt        file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.
---------------------------------------------------------------------------------------------------------
Right column-aligned output:
      Given          a        txt   file     of      many     lines,    where  fields  within      a line
        are delineated         by      a single  'dollar' character,    write       a program
       that     aligns       each column     of    fields         by ensuring    that   words     in each
     column        are  separated     by     at     least        one   space.
   Further,      allow        for   each   word        in          a   column      to      be either left
 justified,      right justified,     or center justified     within      its column.
---------------------------------------------------------------------------------------------------------
Center column-aligned output:
   Given        a         txt     file    of      many     lines,     where   fields  within    a   line
    are    delineated     by        a   single  'dollar' character,   write     a    program
   that      aligns      each    column   of     fields      by     ensuring   that   words    in   each
  column       are     separated   by     at     least       one     space.
 Further,     allow       for     each   word      in         a      column     to      be   either left
justified,    right   justified,   or   center justified   within      its   column.
---------------------------------------------------------------------------------------------------------

```



## Kotlin


```scala
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Paths

enum class AlignFunction {
    LEFT { override fun invoke(s: String, l: Int) = ("%-" + l + 's').format(("%" + s.length + 's').format(s)) },
    RIGHT { override fun invoke(s: String, l: Int) = ("%-" + l + 's').format(("%" + l + 's').format(s)) },
    CENTER { override fun invoke(s: String, l: Int) = ("%-" + l + 's').format(("%" + ((l + s.length) / 2) + 's').format(s)) };

    abstract operator fun invoke(s: String, l: Int): String
}

/** Aligns fields into columns, separated by "|".
 * @constructor Initializes columns aligner from lines in a list of strings.
 * @property lines Lines in a single string. Empty string does form a column.
 */
class ColumnAligner(val lines: List<String>) {
     operator fun invoke(a: AlignFunction) : String {
        var result = ""
        for (lineWords in words) {
            for (i in lineWords.indices) {
                if (i == 0)
                    result += '|'
                result += a(lineWords[i], column_widths[i])
                result += '|'
            }
            result += '\n'
        }
        return result
    }

    private val words = arrayListOf<Array<String>>()
    private val column_widths = arrayListOf<Int>()

    init {
        lines.forEach  {
            val lineWords = java.lang.String(it).split("\\$")
            words += lineWords
            for (i in lineWords.indices) {
                if (i >= column_widths.size) {
                    column_widths += lineWords[i].length
                } else {
                    column_widths[i] = Math.max(column_widths[i], lineWords[i].length)
                }
            }
        }
    }
}

fun main(args: Array<String>) {
    if (args.isEmpty()) {
        println("Usage: ColumnAligner file [L|R|C]")
        return
    }
    val ca = ColumnAligner(Files.readAllLines(Paths.get(args[0]), StandardCharsets.UTF_8))
    val alignment = if (args.size >= 2) args[1] else "L"
    when (alignment) {
        "L" -> print(ca(AlignFunction.LEFT))
        "R" -> print(ca(AlignFunction.RIGHT))
        "C" -> print(ca(AlignFunction.CENTER))
        else -> System.err.println("Error! Unknown alignment: " + alignment)
    }
}
```



## Lasso


```Lasso
#!/usr/bin/lasso9

local(text = "Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
")


define go_left(text::array, width::integer) => {
	local(output = string)
	with row in #text do {
		with word in #row do {
			#output -> append(string(#word) -> padtrailing(#width + 1)&)
		}
		#output -> append('\n')
	}
	return #output
}

define go_right(text::array, width::integer) => {
	local(output = string)
	with row in #text do {
		with word in #row do {
			#output -> append(string(#word) -> padleading(#width + 1)&)
		}
		#output -> append('\n')
	}
	return #output
}

define go_center(text::array, width::integer) => {
	local(output = string)
	with row in #text do {
		with word in #row do {
			local(
				padlength	= (#width + 1 - #word -> size),
				padleft		= (' ' * (#padlength / 2)),
				padright	= (' ' * (#padlength - #padleft -> size))
			)
			#output -> append(#padleft + string(#word) + #padright)
		}
		#output -> append('\n')
	}
	return #output
}

define prepcols(text::string) => {
	local(
		result		= array,
		maxwidth	= 0
	)
	with row in #text -> split('\n') do {
		#row -> removetrailing('$')
		#result -> insert(#row -> split('$'))
	}
	with word in delve(#result) do {
		#word -> size > #maxwidth ? #maxwidth = #word -> size
	}
	stdoutnl('Left aligned result: \n' + go_left(#result, #maxwidth))
	stdoutnl('Right aligned result: \n' + go_right(#result, #maxwidth))
	stdoutnl('Centered result: \n' + go_center(#result, #maxwidth))
}

prepcols(#text)
```


{{out}}

```txt
Left aligned result:
Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.


Right aligned result:
      Given          a       text       file         of       many     lines,      where     fields     within          a       line
        are delineated         by          a     single   'dollar' character,      write          a    program
       that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
     column        are  separated         by         at      least        one     space.
   Further,      allow        for       each       word         in          a     column         to         be     either       left
 justified,      right justified,         or     center  justified     within        its    column.


Centered result:
   Given        a        text       file        of        many      lines,      where     fields     within        a        line
    are    delineated     by          a       single    'dollar'  character,    write        a       program
   that      aligns      each      column       of       fields       by      ensuring     that       words       in        each
  column       are     separated     by         at        least       one      space.
 Further,     allow       for       each       word        in          a       column       to         be       either      left
justified,    right   justified,     or       center    justified   within       its      column.

```



## Liberty BASIC


```lb
mainwin 140 32

    CRLF$  =chr$( 13)
    maxlen =0

    read y

    Dim txt$( y)

    For i =1 To y
        Read i$
        print i$
        if right$( i$, 1) <>"$" then i$ =i$ +"$"
        txt$( i) =i$
        x  =max( CountDollars( txt$( i)), x)
    Next i

    print x

    Dim matrix$( x, y)

    Print CRLF$; "  ---- Left ----"
    For yy =1 To y
        For xx =1 To x
            matrix$( xx, yy) =word$( txt$( yy), xx, "$")
            print matrix$( xx, yy), "|";
            maxlen           =max( maxlen, Len( matrix$( xx, yy)))
        Next xx
        print ""
    Next yy

    Print CRLF$; "  ---- Right ----"
    For yy =1 To y
        For xx =1 To x
            Print right$( "                    " +matrix$( xx, yy), maxlen +1); "|";
            '   will truncate column words longer than 20. Change to use maxlen....
        Next xx
        Print ""
    Next yy

    Print CRLF$ +"  ---- Center ----"
    For yy =1 to y
        For xx =1 to x
            wordLen     =Len( matrix$( xx, yy))
            padNeeded   =maxlen -wordLen +4
            LeftSpaces  =padNeeded /2

            if LeftSpaces =int( LeftSpaces) then
                RightSpaces =LeftSpaces
            else
                RightSpaces =LeftSpaces -1
            end if

            Print space$( LeftSpaces); matrix$( xx, yy); space$( RightSpaces); "|";
        Next xx
        Print ""
    Next yy

    wait

    Data  6

    Data "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
    Data "are$delineated$by$a$single$'dollar'$character,$write$a$program"
    Data "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
    Data "column$are$separated$by$at$least$one$space."
    Data "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
    Data "justified,$right$justified,$or$center$justified$within$its$column."

    function CountDollars( src$)
        c =0
        for j =1 to len( src$)
            if mid$( src$, j, 1) ="$" then c =c +1
        next j
        CountDollars =c
    end function

    end
```



## Lua

{{works with|Lua|5.1}}


```lua

local tWord = {}        -- word table
local tColLen = {}      -- maximum word length in a column
local rowCount = 0      -- row counter
--store maximum column lengths at 'tColLen'; save words into 'tWord' table
local function readInput(pStr)
    for line in pStr:gmatch("([^\n]+)[\n]-") do  -- read until '\n' character
        rowCount = rowCount + 1
        tWord[rowCount] = {}                     -- create new row
        local colCount = 0
        for word in line:gmatch("[^$]+") do      -- read non '$' character
            colCount = colCount + 1
            tColLen[colCount] = math.max((tColLen[colCount] or 0), #word)   -- store column length
            tWord[rowCount][colCount] = word                                -- store words
        end--for word
    end--for line
end--readInput
--repeat space to align the words in the same column
local align = {
    ["left"] = function (pWord, pColLen)
        local n = (pColLen or 0) - #pWord + 1
        return pWord .. (" "):rep(n)
    end;--["left"]
    ["right"] = function (pWord, pColLen)
        local n = (pColLen or 0) - #pWord + 1
        return (" "):rep(n) .. pWord
    end;--["right"]
    ["center"] = function (pWord, pColLen)
        local n = (pColLen or 0) - #pWord + 1
        local n1 = math.floor(n/2)
        return (" "):rep(n1) .. pWord .. (" "):rep(n-n1)
    end;--["center"]
}
--word table padder
local function padWordTable(pAlignment)
    local alignFunc = align[pAlignment]                         -- selecting the spacer function
    for rowCount, tRow in ipairs(tWord) do
        for colCount, word in ipairs(tRow) do
            tRow[colCount] = alignFunc(word, tColLen[colCount]) -- save the padded words into the word table
        end--for colCount, word
    end--for rowCount, tRow
end--padWordTable
--main interface
--------------------------------------------------[]
function alignColumn(pStr, pAlignment, pFileName)
--------------------------------------------------[]
    readInput(pStr)                           -- store column lengths and words
    padWordTable(pAlignment or "left")        -- pad the stored words
    local output = ""
    for rowCount, tRow in ipairs(tWord) do
        local line = table.concat(tRow)       -- concatenate words in one row
        print(line)                           -- print the line
        output = output .. line .. "\n"       -- concatenate the line for output, add line break
    end--for rowCount, tRow
    if (type(pFileName) == "string") then
        local file = io.open(pFileName, "w+")
        file:write(output)                    -- write output to file
        file:close()
    end--if type(pFileName)
    return output
end--alignColumn

```


Usage Example:


```lua

input =
[[Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.]]


outputLeft = alignColumn(input)
outputRight = alignColumn(input, "right")
alignColumn(input, "center", "output.txt")

```


## M2000 Interpreter


```M2000 Interpreter

Module Align_Columns {
	a$={Given$a$text$file$of$many$lines,$where$fields$within$a$line$
		are$delineated$by$a$single$'dollar'$character,$write$a$program
		that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
		column$are$separated$by$at$least$one$space.
		Further,$allow$for$each$word$in$a$column$to$be$either$left$
		justified,$right$justified,$or$center$justified$within$its$column.
		}
	const cr$=chr$(13), lf$=chr$(10)
	def c1=0, cmax=0, p1=-1, i
	flush  ' empty stack
	for i=1 to len(a$)
		select case mid$(a$,i,1)
		case "$", cr$
			if p1<>-1 then data (p1, c1):  p1=-1: cmax=max(c1,cmax):c1=0
		case lf$
			data (-1,0)     ' push to end of  stack an array of two items (a tuple in m2000)
		else case
			if p1=-1 then p1=i :c1=1 else c1++
		end select
	next
	if p1<>-1 then push (p1, c1): cmax=max(c1,cmax):c1=0
	\\ so now stack of values hold all tuples.
	Dim Words(), AlignType$(1 to 3)
	AlignType$(1)=lambda$ (a$,wd)->field$(a$, wd)
	AlignType$(2)=lambda$ (a$,wd)->{
		a$=left$(a$, wd)
		=left$(string$(" ", (len(a$)-wd) div 2)+a$+string$(" ",wd),wd)
	}
	AlignType$(3)= lambda$ (a$,wd)->format$("{0:"+str$(-wd)+"}", a$)
	\\ [] return a stack object, reference and leave current stack of values a new stack
	\\ Array( stack_object) empty the stack object moving items to an array
	Words()=Array([])
	document export$
	def aline$
	cmax++  ' add one space
	For al=1 to 3
		For i=0 to len(Words())-1
			if Words(i)(0)=-1 then
				' we use rtrim$() to cut trailing spaces
				export$=rtrim$(aline$)+cr$+lf$ : aline$=""
			else
				aline$+=AlignType$(al)(mid$(a$,Words(i)(0), Words(i)(1)),cmax)
			end if
		next i
	next
	\\ export to clipboard
	Clipboard  export$
	Rem	Form 140, 60
	Rem	Print #-2, export$ ' render text to console without using console's columns
}
Align_Columns

```


{{out}}
<pre style="height:30ex;overflow:scroll">
Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.
   Given        a        text       file        of        many      lines,      where     fields     within        a        line
    are    delineated     by          a       single    'dollar'  character,    write        a       program
   that      aligns      each      column       of       fields       by      ensuring     that       words       in        each
  column       are     separated     by         at        least       one      space.
 Further,     allow       for       each       word        in          a       column       to         be       either      left
justified,    right   justified,     or       center    justified   within       its      column.
      Given          a       text       file         of       many     lines,      where     fields     within          a       line
        are delineated         by          a     single   'dollar' character,      write          a    program
       that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
     column        are  separated         by         at      least        one     space.
   Further,      allow        for       each       word         in          a     column         to         be     either       left
 justified,      right justified,         or     center  justified     within        its    column.
</pre >

## Maple

Assign the sample data.

```Maple

txt :=
"Given$a$text$file$of$many$lines,$where$fields$within$a$line$\n"
"are$delineated$by$a$single$'dollar'$character,$write$a$program\n"
"that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$\n"
"column$are$separated$by$at$least$one$space.\n"
"Further,$allow$for$each$word$in$a$column$to$be$either$left$\n"
"justified,$right$justified,$or$center$justified$within$its$column.\n":

```

The following procedure solves the problem.  It takes the string to be operated on as input, and an optional alignment parameter, which defaults to centred alignment.  The aligned text is returned, as a string, which can then be printed.

```Maple

AlignColumns := proc( txt, align :: { "left", "right", "centre" } := "centre" )
	uses StringTools;

	# Get a list of lists of fields
	local A := map( Split, Split( txt ), "$" );

	# Calculate the column width
	local width := 1 + max( map( L -> max( map( length, L ) ), A ) );

	# Add spacing according to the requested type of alignment
	if align = "left" then
		local J := map( line -> map( PadRight, line, width ), A )
	elif align = "right" then
		J := map( line -> map( PadLeft, line, width ), A )
	else
		J := map( line -> map( Center, line, width ), A )
	end if;

	# Join up the fields in each line.
	J := map( cat@op, J );

	# Re-assemble the lines into a single string.
	Join( J, "\n" )
end proc:

```

For the sample text, we get the following results.

```Maple

> printf( "%s\n", AlignColumns( txt ) ):
   Given        a         text       file        of        many      lines,     where      fields     within       a         line
    are     delineated     by         a        single    'dollar'  character,   write        a       program
    that      aligns      each      column       of       fields       by      ensuring     that      words        in        each
   column      are     separated      by         at       least       one       space.
  Further,    allow       for        each       word        in         a        column       to         be       either      left
 justified,   right    justified,     or       center   justified    within      its      column.
> printf( "%s\n", AlignColumns( txt, "center" ) ): # same as above
   Given        a         text       file        of        many      lines,     where      fields     within       a         line
    are     delineated     by         a        single    'dollar'  character,   write        a       program
    that      aligns      each      column       of       fields       by      ensuring     that      words        in        each
   column      are     separated      by         at       least       one       space.
  Further,    allow       for        each       word        in         a        column       to         be       either      left
 justified,   right    justified,     or       center   justified    within      its      column.
> printf( "%s\n", AlignColumns( txt, "left" ) ):
Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.
> printf( "%s\n", AlignColumns( txt, "right" ) ):
      Given          a       text       file         of       many     lines,      where     fields     within          a       line
        are delineated         by          a     single   'dollar' character,      write          a    program
       that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
     column        are  separated         by         at      least        one     space.
   Further,      allow        for       each       word         in          a     column         to         be     either       left
 justified,      right justified,         or     center  justified     within        its    column.

```

Alternatively, this could be printed to a file (using fprintf instead of printf).


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
TableForm[StringSplit[StringSplit[a,"\n"],"$"],TableAlignments -> Center]
```

Output with example text :
[[File:centeredtext.png]]


## ML/I

In this example, ML/I reads its macros first, then switches input to the file containing the data to be formatted. Output is to 'standard output' or similar.
Note the presetting of P102 to indicate the alignment required.


```ML/I
MCSKIP "WITH" NL
"" Align columns - assumes macros on input stream 1, data on stream 2
MCPVAR 102
"" Set P102 to alignment required:
""   1 = centre
""   2 = left
""   3 = right
MCSET P102 = 1
MCSKIP MT,<>
MCINS %.
MCSKIP SL WITH *
"" Assume no more than 100 columns - P101 used for max number of fields
"" Set P variables 1-101 to 0
MCDEF ZEROPS WITHS NL AS <MCSET T1=1
%L1.MCSET PT1=0
MCSET T1=T1+1
MCGO L1 UNLESS T1 EN 102
>
ZEROPS
"" First pass - macro to accumulate max columns, and max widths
MCDEF SL N1 OPT $ N1 OR $ WITHS NL OR SPACE WITHS NL OR NL ALL
AS <MCGO L3 UNLESS T1 GR P101
MCSET P101=T1
%L3.MCSET T2=1
%L1.MCGO L0 IF T2 GR T1
MCSET T3=MCLENG(%WBT2.)
MCGO L2 UNLESS T3 GR PT2
MCSET PT2=T3
%L2.MCSET T2=T2+1
MCGO L1
>
MCSET S1=1
*MCSET S10=2
*MCSET S1=0
MCSET S4=1
""MCNOTE Max field is %P101.
""MCDEF REP NL AS <MCSET T1=1
""%L1.%PT1. MCSET T1=T1+1
""MCGO L1 UNLESS T1 GR P101
"">
""REP
MCDEF SL N1 OPT $ N1 OR $ WITHS NL OR SPACE WITHS NL OR NL ALL
AS <MCSET T2=1
%L5.MCGO L6 IF T2 GR T1
MCGO LP102
%L1.MCSET T3=%%%PT2.-MCLENG(%WBT2.)./2.
MCGO L7 IF T3 EN 0
MCSUB(<                            >,1,T3)%L7.%WBT2.""
MCSUB(<                            >,1,PT2-T3-MCLENG(%WBT2.)+1)MCGO L4
%L2.MCSUB(%WBT2.<                              >,1,PT2)MCGO L4
%L3.MCSUB(<                              >%WBT2.,1-PT2,0)""
%L4. MCSET T2=T2+1
MCGO L5
%L6.
>
MCSET S1=1
*MCSET S10=102
```



## MUMPS


```MUMPS
columns(how)	; how = "Left", "Center" or "Right"
	New col,half,ii,max,spaces,word
	Set ii=0
	Set ii=ii+1,line(ii)="Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
	Set ii=ii+1,line(ii)="are$delineated$by$a$single$'dollar'$character,$write$a$program"
	Set ii=ii+1,line(ii)="that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
	Set ii=ii+1,line(ii)="column$are$separated$by$at$least$one$space."
	Set ii=ii+1,line(ii)="Further,$allow$for$each$word$in$a$column$to$be$either$left$"
	Set ii=ii+1,line(ii)="justified,$right$justified,$or$center$justified$within$its$column."
	Set ii="" For  Set ii=$Order(line(ii)) Quit:ii=""  Do
	. For col=1:1:$Length(line(ii),"$") Do
	. . Set max=$Length($Piece(line(ii),"$",col))
	. . Set:max>$Get(max(col)) max(col)=max
	. . Quit
	. Quit
	Set ii="" For  Set ii=$Order(line(ii)) Quit:ii=""  Do
	. Write ! For col=1:1:$Length(line(ii),"$") Do:$Get(max(col))
	. . Set word=$Piece(line(ii),"$",col)
	. . Set spaces=$Justify("",max(col)-$Length(word))
	. . If how="Left" Write word,spaces," " Quit
	. . If how="Right" Write spaces,word," " Quit
	. . Set half=$Length(spaces)\2
	. . Write $Extract(spaces,1,half),word,$Extract(spaces,half+1,$Length(spaces))," "
	. . Quit
	. Quit
	Write !
	Quit
Do columns("Left")

Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

Do columns("Center")

  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

Do columns("Right")

     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.
```



## Nim


```nim
from strutils import splitLines, split
from sequtils import mapIt
from strfmt import format, write

let textinfile = """Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column."""

var words = textinfile.splitLines.mapIt(it.split '$')
var maxs = newSeq[int](max words.mapIt(it.len))

for line in words:
  for j,w in line:
    maxs[j] = max(maxs[j], w.len+1)

for i, align in ["<",">","^"]:
  echo(["Left", "Right", "Center"][i], " column-aligned output:")
  for line in words:
    for j,w in line:
      stdout.write(w.format align & $maxs[j])
    stdout.write "\n"
  stdout.write "\n"
```

{{out}}

```txt
Left column-aligned output:
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

Right column-aligned output:
      Given          a       text   file     of      many     lines,    where  fields  within      a line
        are delineated         by      a single  'dollar' character,    write       a program
       that     aligns       each column     of    fields         by ensuring    that   words     in each
     column        are  separated     by     at     least        one   space.
   Further,      allow        for   each   word        in          a   column      to      be either left
 justified,      right justified,     or center justified     within      its column.

Center column-aligned output:
   Given        a        text     file    of      many     lines,     where   fields  within    a   line
    are    delineated     by        a   single  'dollar' character,   write     a    program
   that      aligns      each    column   of     fields      by     ensuring   that   words    in   each
  column       are     separated   by     at     least       one     space.
 Further,     allow       for     each   word      in         a      column     to      be   either left
justified,    right   justified,   or   center justified   within      its   column.
```



## Nit


Source: [https://github.com/nitlang/nit/blob/master/examples/rosettacode/align_columns.nit the official Nit’s repository]


```nit
# Task: Align columns
#
# Uses `Text::justify` from the standard library.
module align_columns

fun aligner(text: String, left: Float)
do
	# Each row is a sequence of fields
	var rows = new Array[Array[String]]
	for line in text.split('\n') do
		rows.add line.split("$")
	end

	# Compute the final length of each column
	var lengths = new Array[Int]
	for fields in rows do
		var i = 0
		for field in fields do
			var fl = field.length
			if lengths.length <= i or fl > lengths[i] then
				lengths[i] = fl
			end
			i += 1
		end
	end

	# Process each line and align each field
	for fields in rows do
		var line = new Array[String]
		var i = 0
		for field in fields do
			line.add field.justify(lengths[i], left)
			i += 1
		end
		print line.join(" ")
	end
end

var text = """
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column."""

aligner(text, 0.0)
aligner(text, 1.0)
aligner(text, 0.5)
```

{{out}}

```txt
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

```


=={{header|Oberon-2}}==
works with oo2c version 2.

```oberon2

MODULE Columns;
IMPORT
  NPCT:Tools,
  Object,
  Out;

TYPE
  Parts = ARRAY 32 OF STRING;
  Formatter = PROCEDURE (s: STRING; len: LONGINT): STRING;
VAR
  lines: ARRAY 6 OF STRING;
  words: ARRAY 6 OF Parts;
  columnWidth: ARRAY 128 OF INTEGER;
  lineIdx: INTEGER;

  (*
   * Size: returns de number of words in a line
   *)
  PROCEDURE Size(p: Parts): INTEGER;
  VAR
    i: INTEGER;
  BEGIN
    i := 0;
    WHILE (i < LEN(p)) & (p[i] # NIL) DO
      INC(i);
    END;
    RETURN i
  END Size;
  (*
   * Max: returns maximum number of words in the lines
   *)
  PROCEDURE Max(w: ARRAY OF Parts): INTEGER;
  VAR
    i, max, resp: INTEGER;
  BEGIN
    i := 0;resp := 0;
    WHILE (i < LEN(w)) DO
      max := Size(w[i]);
      IF (max > resp) THEN resp := max END;
      INC(i)
    END;
    RETURN resp;
  END Max;

  (*
   * MaxColumnWidth: returns the maximum width of a column
   *)
  PROCEDURE MaxColumnWidth(w: ARRAY OF Parts;column: INTEGER): INTEGER;
  VAR
    line,max: LONGINT;
  BEGIN
    line := 0;
    max := MIN(INTEGER);
    WHILE (line < LEN(w)) DO;
      IF (w[line,column]  # NIL) & (w[line,column](Object.String8).length > max) THEN max := w[line,column](Object.String8).length END;
      INC(line)
    END;
    RETURN SHORT(max)
  END MaxColumnWidth;

  (*
   * PrintWords: prints the words in 'w' using the formatter passed in 'format'
   *)
  PROCEDURE PrintWords(w: ARRAY OF Parts; format: Formatter);
  VAR
    i,j: INTEGER;
  BEGIN
    i := 0;
    WHILE (i < LEN(words)) DO
      j := 0;
      WHILE (j < Max(words)) & (words[i,j] # NIL) DO
        Out.Object(format(words[i,j],columnWidth[j] + 1));
        INC(j)
      END;
      Out.Ln;
      INC(i)
    END;
    Out.Ln
  END PrintWords;

BEGIN
lines[0] := "Given$a$text$file$of$many$lines,$where$fields$within$a$line$";
lines[1] := "are$delineated$by$a$single$'dollar'$character,$write$a$program";
lines[2] := "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$";
lines[3] := "column$are$separated$by$at$least$one$space.";
lines[4] := "Further,$allow$for$each$word$in$a$column$to$be$either$left$";
lines[5] := "justified,$right$justified,$or$center$justified$within$its$column.";

(* Split line in words *)
lineIdx := 0;
WHILE lineIdx < LEN(lines) DO
  Tools.Split(lines[lineIdx],"$",words[lineIdx]);
  INC(lineIdx)
END;

(* Calculate width of the column *)
lineIdx := 0;
WHILE (lineIdx < Max(words)) DO
  columnWidth[lineIdx] := MaxColumnWidth(words,lineIdx);
  INC(lineIdx)
END;

(* Print Results *)
PrintWords(words,Tools.AdjustLeft);
PrintWords(words,Tools.AdjustCenter);
PrintWords(words,Tools.AdjustRight);
END Columns.


```


```txt

Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

   Given        a         text     file    of     many      lines,    where   fields  within    a    line
    are     delineated     by       a    single 'dollar'  character,  write      a    program
    that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
   column      are     separated    by     at     least      one      space.
  Further,    allow       for      each   word     in         a       column    to      be    either left
 justified,   right    justified,   or   center justified   within     its    column.

      Given          a       text   file     of      many     lines,    where  fields  within      a line
        are delineated         by      a single  'dollar' character,    write       a program
       that     aligns       each column     of    fields         by ensuring    that   words     in each
     column        are  separated     by     at     least        one   space.
   Further,      allow        for   each   word        in          a   column      to      be either left
 justified,      right justified,     or center justified     within      its column.



```



## OCaml


```ocaml
#load "str.cma"
open Str

let input = "\
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column."

let () =
  let lines = split (regexp_string "\n") input in
  let fields_l = List.map (split (regexp_string "$")) lines in
  let fields_l = List.map Array.of_list fields_l in
  let n = (* number of columns *)
    List.fold_left
      (fun n fields -> max n (Array.length fields))
      0 fields_l
  in
  let pads = Array.make n 0 in
  List.iter (
    (* calculate the max padding for each column *)
    Array.iteri
      (fun i word -> pads.(i) <- max pads.(i) (String.length word))
  ) fields_l;

  let print f =
    List.iter (fun fields ->
      Array.iteri (fun i word ->
        f word (pads.(i) - (String.length word))
      ) fields;
      print_newline()
    ) fields_l;
  in

  (* left column-aligned output *)
  print (fun word pad ->
    let spaces = String.make pad ' ' in
    Printf.printf "%s%s " word spaces);

  (* right column-aligned output *)
  print (fun word pad ->
    let spaces = String.make pad ' ' in
    Printf.printf "%s%s " spaces word);

  (* center column-aligned output *)
  print (fun word pad ->
    let pad1 = pad / 2 in
    let pad2 = pad - pad1 in
    let sp1 = String.make pad1 ' ' in
    let sp2 = String.make pad2 ' ' in
    Printf.printf "%s%s%s " sp1 word sp2);
;;
```



## Oforth



```oforth
import: mapping
import: file

: <<nbl     \ stream n -- stream
   #[ ' ' <<c ] times ;

String method: justify( n just -- s )
| l m |
   n self size - dup ->l 2 / ->m
   String new
   just $RIGHT  if=: [ l <<nbl  self <<  return ]
   just $LEFT   if=: [ self <<  l <<nbl  return ]
   m <<nbl  self <<  l m - <<nbl
;

: align( file just -- )
| lines maxsize |
    #[ wordsWith( '$' ) ] file File new map   ->lines
    0 #[ apply( #[ size max ] ) ] lines apply ->maxsize
    #[ apply( #[ justify( maxsize , just) . ] ) printcr ] lines apply
;
```


{{out}}

```txt

>"align.txt" $LEFT align
Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.
ok
>

```



## ooRexx


```ooRexx

text = .array~of("Given$a$text$file$of$many$lines,$where$fields$within$a$line$", -
                 "are$delineated$by$a$single$'dollar'$character,$write$a$program", -
                 "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$", -
                 "column$are$separated$by$at$least$one$space.", -
                 "Further,$allow$for$each$word$in$a$column$to$be$either$left$", -
                 "justified,$right$justified,$or$center$justified$within$its$column.")

columns = 0
parsedText = .array~new
-- split each line of text into words and figure out how many columns we need
loop line over text
    parsedLine = line~makearray("$")
    parsedText~append(parsedLine)
    columns = max(columns, parsedLine~items)
end

-- now figure out how wide we need to make each column
columnWidths = .array~new(columns)
linelength = 0
loop i = 1 to columns
    width = 0
    loop line over parsedText
        word = line[i]
        if word \= .nil then width = max(width, word~length)
    end
    columnWidths[i] = width
    -- keep track of the total width, including space for a separator
    linelength += width + 1
end

say "align left:"
say
out = .mutableBuffer~new(linelength)
loop line over parsedText
  -- mutable buffers are more efficient than repeated string concats
  -- reset the working buffer to zero
  out~setbuffersize(0)
  loop col = 1 to line~items
      word = line[col]
      if word == .nil then word = ''
      out~append(word~left(columnwidths[col] + 1))
  end
  say out~string
end
say
say "align right:"
say

loop line over parsedText
  -- mutable buffers are more efficient than repeated string concats
  -- reset the working buffer to zero
  out~setbuffersize(0)
  loop col = 1 to line~items
      word = line[col]
      if word == .nil then word = ''
      out~append(word~right(columnwidths[col] + 1))
  end
  say out~string
end
say
say "align center:"
say

loop line over parsedText
  -- mutable buffers are more efficient than repeated string concats
  -- reset the working buffer to zero
  out~setbuffersize(0)
  loop col = 1 to line~items
      word = line[col]
      if word == .nil then word = ''
      out~append(word~center(columnwidths[col] + 1))
  end
  say out~string
end

```


```txt

align left:

Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

align right:

      Given          a       text   file     of      many     lines,    where  fields  within      a line
        are delineated         by      a single  'dollar' character,    write       a program
       that     aligns       each column     of    fields         by ensuring    that   words     in each
     column        are  separated     by     at     least        one   space.
   Further,      allow        for   each   word        in          a   column      to      be either left
 justified,      right justified,     or center justified     within      its column.

align center:

   Given        a        text     file    of      many     lines,     where   fields  within    a   line
    are    delineated     by        a   single  'dollar' character,   write     a    program
   that      aligns      each    column   of     fields      by     ensuring  that    words    in   each
  column       are     separated   by     at     least       one     space.
 Further,     allow       for     each   word      in         a      column     to      be   either left
justified,    right   justified,   or   center justified   within      its   column.

```



## OpenEdge/Progress


```progress
FUNCTION alignColumns RETURNS CHAR (
   i_c      AS CHAR,
   i_calign AS CHAR
):

   DEF VAR ipass     AS INT.
   DEF VAR iline     AS INT.
   DEF VAR icol      AS INT.
   DEF VAR iwidth    AS INT EXTENT.
   DEF VAR cword     AS CHAR.
   DEF VAR cspace    AS CHAR.
   DEF VAR cresult   AS CHAR.

   EXTENT( iwidth ) = NUM-ENTRIES( ENTRY( 1, i_c, "~n" ), "$" ).

   DO ipass = 0 TO 1:
      DO iline = 1 TO NUM-ENTRIES( i_c, "~n" ):
         DO icol = 1 TO NUM-ENTRIES( ENTRY( iline, i_c, "~n" ), "$" ):
            cword = ENTRY( icol, ENTRY( iline, i_c, "~n" ), "$" ).
            IF ipass = 0 THEN
               iwidth = MAXIMUM( LENGTH( cword ), iwidth[ icol ] ).
            ELSE DO:
               cspace = FILL( " ", iwidth[ icol ] - LENGTH( cword ) ).
               CASE i_calign:
                  WHEN "left"    THEN cresult = cresult + cword + cspace.
                  WHEN "right"   THEN cresult = cresult + cspace + cword.
                  WHEN "center"  THEN DO:
                     cword = FILL( " ", INTEGER( LENGTH( cspace ) / 2 ) ) + cword.
                     cresult = cresult + cword + FILL( " ", iwidth[icol] - LENGTH( cword ) ).
                  END.
               END CASE. /* i_calign */
               cresult = cresult + " ".
            END.
         END. /* DO icol = 1 TO ... */
         IF ipass = 1 THEN
            cresult = cresult + "~n".
      END. /* DO iline = 1 TO ... */
   END. /* DO ipass = 0 TO 1 */

   RETURN cresult.

END FUNCTION.

DEF VAR cc AS CHAR.

cc =  SUBSTITUTE(
         "&1~n&2~n&3~n&4~n&5~n&6",
         "Given$a$text$file$of$many$lines,$where$fields$within$a$line$",
         "are$delineated$by$a$single$'dollar'$character,$write$a$program",
         "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$",
         "column$are$separated$by$at$least$one$space.",
         "Further,$allow$for$each$word$in$a$column$to$be$either$left$",
         "justified,$right$justified,$or$center$justified$within$its$column."
      ).

MESSAGE
   alignColumns( cc, "left" )    SKIP
   alignColumns( cc, "right" )   SKIP
   alignColumns( cc, "center" )
VIEW-AS ALERT-BOX.
```


{{out}}

```txt

---------------------------
Message
---------------------------
Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.

     Given          a       text       file         of       many     lines,      where     fields     within          a       line
       are delineated         by          a     single   'dollar' character,      write          a    program
      that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
    column        are  separated         by         at      least        one     space.
  Further,      allow        for       each       word         in          a     column         to         be     either       left
justified,      right justified,         or     center  justified     within        its    column.

   Given        a        text       file        of        many      lines,      where     fields     within        a        line
    are    delineated     by          a       single    'dollar'  character,    write        a       program
   that      aligns      each      column       of       fields       by      ensuring     that       words       in        each
  column       are     separated     by         at        least       one      space.
 Further,     allow       for       each       word        in          a       column       to         be       either      left
justified,    right   justified,     or       center    justified   within       its      column.
---------------------------
OK
---------------------------
```



## OxygenBasic


```txt




'
### ==========

Class AlignedText
'
### ==========


indexbase 1

string  buf, bufo, cr, tab, jus
sys     Cols, Rows, ColWidth[200], TotWidth, ColPad

method SetText(string s)
cr=chr(13)+chr(10)
tab=chr(9)
jus=string 200,"L"
buf=s
measure
end method


method measure()
sys a, b, wa, wb, cm, c, cw
a=1 : b=1
Cols=0 : Rows=0 : ColPad=3
do
  wb=b
  a=instr b,buf,cr
  if a=0 then exit do
  cm=0
  c++
  do
    wa=instr wb,buf,"$"
    if wa=0 or wa>a then exit do
    cm++
    if cm>cols then cols=cm
    cw=wa-wb
    if cw > ColWidth[cm] then ColWidth[cm]=cw
    wb=wa+1
  end do
  b=a+len cr
end do
rows=c
'
c=0
for i=1 to cols
  ColWidth[ i ]+=ColPad
  c+=ColWidth[ i ]
next
TotWidth=c+len cr
'print ShowMetrics
end method


method ShowMetrics() as string
pr="METRICS:" cr cr
pr+=rows tab cols tab totwidth cr cr
pr+="column" tab "spacing" cr
for i=1 to cols
  pr+=i tab ColWidth[ i ] cr
next
return pr
end method


method justify(string j)
mid jus,1,j
end method


method layout() as string
sys a, b, wa, wb, wl, cm, lpos, cpos
bufo=space Rows*TotWidth
a=1 : b=1
do
  wb=b
  a=instr(b,buf,cr)
  if a=0 then exit do
  cm=0
  cpos=1
  do
    wa=instr(wb,buf,"$")
    if wa=0 or wa>a then exit do
    '
    cm++
    '
    'JUSTIFICATION
    '
    wl=wa-wb
    p=lpos+cpos 'default "L" LEFT ALIGN
    '
    select case asc(jus,cm)
      case "R" : p=lpos+cpos+ColWidth[cm]-wl-Colpad
      case "C" : p=lpos+cpos+( ColWidth[cm]-wl-Colpad )*.5
    end select
    '
    mid bufo,p, mid buf,wb,wl
    cpos+=colwidth[cm]
    wb=wa+1
  end do
  b=a+len cr
  lpos+=TotWidth
  if lpos<len(bufo) then mid bufo,lpos-1,cr
end do
return bufo
end method

end class

'#recordof AlignedText

'====
'TEST
'====

AlignedText tt
tt.SetText quote
"""
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
"""
'print tt.ShowMetrics
tt.justify "LLLLCCCRRRRR"
putfile "t.txt", tt.layout


```



## Oz


```oz
declare
  %% Lines: list of strings
  %% Alignment: function like fun {Left Txt ExtraSpace} ... end
  %% Returns: list of aligned (virtual) strings
  fun {Align Lines Alignment}
     ParsedLines = {Map Lines ParseLine}
     NumColumns = {Maximum {Map ParsedLines Record.width}}
     %% maps column index to column width:
     WidthOfColumn = {Record.map {TupleRange NumColumns}
                      fun {$ ColumnIndex}
                         fun {LengthOfThisColumn ParsedLine}
                            {Length {CondSelect ParsedLine ColumnIndex nil}}
                         end
                      in
                         {Maximum {Map ParsedLines LengthOfThisColumn}}
                      end}
  in
     {Map ParsedLines
      fun {$ Columns}
         {Record.mapInd Columns
          fun {$ ColumnIndex ColumnText}
             Extra = WidthOfColumn.ColumnIndex - {Length ColumnText}
          in
             {Alignment ColumnText Extra}#" "
          end}
      end}
  end

  %% A parsed line is a tuple of columns.
  %% "a$b$c" -> '#'(1:"a" 2:"b" 3:"c")
  fun {ParseLine Line}
     {List.toTuple '#' {String.tokens Line &$}}
  end

  %% possible alignments:

  fun {Left Txt Extra}
     Txt#{Spaces Extra}
  end

  fun {Right Txt Extra}
     {Spaces Extra}#Txt
  end

  fun {Center Txt Extra}
     Half = Extra div 2
  in
     {Spaces Half}#Txt#{Spaces Half + Extra mod 2}
  end

  %% helpers:

  %% 3 -> unit(1 2 3)
  fun {TupleRange Max}
     {List.toTuple unit {List.number 1 Max 1}}
  end

  fun {Maximum X|Xr}
     {FoldL Xr Value.max X}
  end

  fun {Spaces N}
     case N of 0 then nil
     else & |{Spaces N-1}
     end
  end

  Lines = ["Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
           "are$delineated$by$a$single$'dollar'$character,$write$a$program"
           "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
           "column$are$separated$by$at$least$one$space."
           "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
           "justified,$right$justified,$or$center$justified$within$its$column."]
in
  {ForAll {Align Lines Left} System.showInfo}
```



## Pascal

See [[Align_columns#Delphi | Delphi]]


## Perl


```Perl
#/usr/bin/perl -w
use strict ;

die "Call : perl columnaligner.pl <inputfile> <printorientation>!\n" unless
   @ARGV == 2 ; #$ARGV[ 0 ] contains example file , $ARGV[1] any of 'left' , 'right' or 'center'
die "last argument must be one of center, left or right!\n" unless
   $ARGV[ 1 ] =~ /center|left|right/ ;
sub printLines( $$$ ) ;
open INFILE , "<" , "$ARGV[ 0 ]" or die "Can't open $ARGV[ 0 ]!\n" ;
my @lines = <INFILE> ;
close INFILE ;
chomp @lines ;
my @fieldwidths = map length, split /\$/ , $lines[ 0 ] ;
foreach my $i ( 1..$#lines ) {
   my @words = split /\$/ , $lines[ $i ] ;
   foreach my $j ( 0..$#words ) {
      if ( $j <= $#fieldwidths ) {
         if ( length $words[ $j ] > $fieldwidths[ $j ] ) {
               $fieldwidths[ $j ] = length $words[ $j ] ;
         }
      }
      else {
         push @fieldwidths, length $words[ $j ] ;
      }
   }
}
printLine( $_ , $ARGV[ 1 ] , \@fieldwidths ) foreach @lines ;
##################################################################    ####
sub printLine {
   my $line = shift ;
   my $orientation = shift ;
   my $widthref = shift ;
   my @words = split /\$/, $line ;
   foreach my $k ( 0..$#words ) {
      my $printwidth = $widthref->[ $k ] + 1 ;
      if ( $orientation eq 'center' ) {
         $printwidth++ ;
      }
      if ( $orientation eq 'left' ) {
         print $words[ $k ] ;
         print " " x ( $printwidth - length $words[ $k ] ) ;
      }
      elsif ( $orientation eq 'right' ) {
         print " " x ( $printwidth - length $words[ $k ] ) ;
         print $words[ $k ] ;
      }
      elsif ( $orientation eq 'center' ) {
         my $left = int( ( $printwidth - length $words[ $k ] )     / 2 ) ;
         my $right = $printwidth - length( $words[ $k ] ) - $left      ;
         print " " x $left ;
         print $words[ $k ] ;
         print " " x $right ;
      }
   }
   print "\n" ;
}
```

a shorter solution

```perl
use List::Util qw(max);

sub columns {
    my @lines = map [split /\$/] => split /\n/ => shift;
    my $pos = {qw/left 0 center 1 right 2/}->{+shift};
    for my $col (0 .. max map {$#$_} @lines) {
        my $max = max my @widths = map {length $_->[$col]} @lines;
        for my $row (0 .. $#lines) {
            my @pad = map {' ' x $_, ' ' x ($_ + 0.5)} ($max - $widths[$row]) / 2;
            for ($lines[$row][$col])
                {$_ = join '' => @pad[0 .. $pos-1], $_, @pad[$pos .. $#pad]}
        }
    }
    join '' => map {"@$_\n"} @lines
}

print columns <<'END', $_ for qw(left right center);
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
END
```



## Perl 6

{{works with|Rakudo|2018.03}}

Call with parameter left (default), center or right.


```perl6
my @lines =
q|Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
|.lines;

my @widths;

for @lines { for .split('$').kv { @widths[$^key] max= $^word.chars; } }
for @lines { say |.split('$').kv.map: { (align @widths[$^key], $^word) ~ " "; } }

sub align($column_width, $word, $aligment = @*ARGS[0]) {
        my $lr = $column_width - $word.chars;
        my $c  = $lr / 2;
        given ($aligment) {
                when "center" { " " x $c.ceiling ~ $word ~ " " x $c.floor }
                when "right"  { " " x $lr        ~ $word                  }
                default       {                    $word ~ " " x $lr      }
        }
}
```


Or a more functional version, called like <code>./align.p6 left input.txt</code>, which however only supports left and right alignment (not center):


```perl6
sub MAIN ($alignment where 'left'|'right', $file) {
    my @lines := $file.IO.lines.map(*.split('$').cache).cache;
    my @widths = roundrobin(|@lines).map(*».chars.max);
    my $align  = {left=>'-', right=>''}{$alignment};
    my $format = @widths.map( '%' ~ ++$ ~ '$' ~ $align ~ * ~ 's' ).join(' ') ~ "\n";
    printf $format, |$_ for @lines;
}
```



## Phix


```Phix

constant data = {
    "Given$a$text$file$of$many$lines,$where$fields$within$a$line$",
    "are$delineated$by$a$single$'dollar'$character,$write$a$program",
    "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$",
    "column$are$separated$by$at$least$one$space.",
    "Further,$allow$for$each$word$in$a$column$to$be$either$left$",
    "justified,$right$justified,$or$center$justified$within$its$column."
}

function split(sequence s, integer c)
sequence out = {}
integer first = 1, delim
    while first<=length(s) do
        delim = find_from(c,s,first)
        if delim = 0 then
            delim = length(s)+1
        end if
        out = append(out,s[first..delim-1])
        first = delim + 1
    end while
    return out
end function

function align(sequence s, integer width, integer alignment)
integer n = width-length(s)
    if n<=0 then
        return s
    elsif alignment<0 then
        return s & repeat(' ', n)
    elsif alignment>0 then
        return repeat(' ', n) & s
    else
        -- (PL if I'd written this, I'd have n-floor(n/2) on the rhs)
        return repeat(' ', floor(n/2)) & s & repeat(' ', floor(n/2+0.5))
    end if
end function

procedure AlignColumns()
integer llij
sequence lines, li
sequence maxlens = {}
    lines = repeat(0,length(data))
    for i=1 to length(data) do
        li = split(data[i],'$')
        lines[i] = li
        if length(li)>length(maxlens) then
            maxlens &= repeat(0,length(li)-length(maxlens))
        end if
        for j=1 to length(li) do
            llij = length(li[j])
            if llij>maxlens[j] then maxlens[j] = llij end if
        end for
    end for

    for a=-1 to 1 do    -- (alignment = left/centre/right)
        for i=1 to length(lines) do
            for j=1 to length(lines[i]) do
                puts(1, align(lines[i][j],maxlens[j],a) & ' ')
            end for
            puts(1,'\n')
        end for
        puts(1,'\n')
    end for
    if getc(0) then end if
end procedure

    AlignColumns()


```

{{out}}

```txt

Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

```



## PHP


```php
<?php
$j2justtype = array('L' => STR_PAD_RIGHT,
                    'R' => STR_PAD_LEFT,
                    'C' => STR_PAD_BOTH);

/**
 Justify columns of textual tabular input where the record separator is the newline
 and the field separator is a 'dollar' character.
 justification can be L, R, or C; (Left, Right, or Centered).

 Return the justified output as a string
*/
function aligner($str, $justification = 'L') {
  global $j2justtype;
  assert(array_key_exists($justification, $j2justtype));
  $justtype = $j2justtype[$justification];

  $fieldsbyrow = array();
  foreach (explode("\n", $str) as $line)
    $fieldsbyrow[] = explode('$', $line);
  $maxfields = max(array_map('count', $fieldsbyrow));

  foreach (range(0, $maxfields-1) as $col) {
    $maxwidth = 0;
    foreach ($fieldsbyrow as $fields)
      $maxwidth = max($maxwidth, strlen($fields[$col]));
    foreach ($fieldsbyrow as &$fields)
      $fields[$col] = str_pad($fields[$col], $maxwidth, ' ', $justtype);
    unset($fields); // see http://bugs.php.net/29992
  }
  $result = '';
  foreach ($fieldsbyrow as $fields)
    $result .= implode(' ', $fields) . "\n";
  return $result;
}

$textinfile = 'Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$\'dollar\'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.';

foreach (array('L', 'R', 'C') as $j)
  echo aligner($textinfile, $j);

?>
```



## PicoLisp


```PicoLisp
(let Sizes NIL                         # Build a list of sizes
   (let Lines                          # and of lines
      (make
         (in "input.txt"                     # Reading input file
            (while (split (line) "$")        # delimited by '$'
               (let (L (link (mapcar pack @))  S Sizes)
                  (setq Sizes                   # Maintain sizes
                     (make
                        (while (or L S)
                           (link
                              (max
                                 (inc (length (pop 'L)))
                                 (pop 'S) ) ) ) ) ) ) ) ) )
      (for L Lines                                 # Print lines
         (prinl (apply align L (mapcar - Sizes))) )   # left aligned
      (prinl)
      (for L Lines
         (prinl (apply align L Sizes)) )              # right aligned
      (prinl)
      (for L Lines
         (prinl (apply center L Sizes)) ) ) )         # and centered
```

{{out}}

```txt
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

      Given          a       text   file     of      many     lines,    where  fields  within      a line
        are delineated         by      a single  'dollar' character,    write       a program
       that     aligns       each column     of    fields         by ensuring    that   words     in each
     column        are  separated     by     at     least        one   space.
   Further,      allow        for   each   word        in          a   column      to      be either left
 justified,      right justified,     or center justified     within      its column.

   Given        a        text     file    of      many     lines,     where   fields  within    a   line
    are    delineated     by        a   single  'dollar' character,   write     a    program
   that      aligns      each    column   of     fields      by     ensuring   that   words    in   each
  column       are     separated   by     at     least       one     space.
 Further,     allow       for     each   word      in         a      column     to      be   either left
justified,    right   justified,   or   center justified   within      its   column.
```



## PL/I


```PL/I

declare text character (300) varying;
declare word character (20) varying;
declare justification character (1);
declare k fixed binary;
declare input file, output file output;

open file (input)  title ( '/CENTER.DAT,type(text),recsize(1000)' );
open file (output) title ( '/OUT.TXT,type(text),recsize(1000)' );
on endfile (input) stop;

display ('Specify whether justification is left, centered, or right');
display ('Reply with a single letter: L, C, or R');
get edit (justification) (A(1));

do forever;
   get file (input) edit (text) (L);
   put skip list (text);
   text = trim(text, '$', '$');
   do until (k = 0);
      k = index(text, '$');
      if k = 0 then /* last word in line */
         word = text;
      else
         do;
            word = substr(text, 1, k-1);
            text = substr(text, k);
            text = trim(text, '$');
         end;
      select (justification);
         when ('C', 'c') word = center(word, maxlength(word));
         when ('R', 'r') word = right (word, maxlength(word));
         otherwise ; /* The default is left adjusted. */
      end;
      put file (output) edit (word) (a(maxlength(word)));
   end;
   put file (output) skip;
end;

```



## PowerShell


```PowerShell

$file =
@'
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
'@.Split("`n")

$arr = @()
$file | foreach {
    $line = $_
    $i = 0
    $hash = [ordered]@{}
    $line.split('$') | foreach{
        $hash["$i"] = "$_"
        $i++
     }
    $arr += @([pscustomobject]$hash)
}
$arr | Format-Table -HideTableHeaders -Wrap *

```

<b>Output:</b>

```txt

Given      a          text       file   of     many      lines,     where    fields  within   a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words    in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be       either left
justified, right      justified, or     center justified within     its      column.

```



## Prolog

Works with SWI-Prolog.

```Prolog
aligner :-
	L ="Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.",

	% read the lines and the words
	% compute the length of the longuest word.
	% LP is the list of lines,
	% each line is a list of words
	parse(L, 0, N, LP, []),

	% we need to add 1 to aligned
	N1 is N+1,
	% words will be left aligned
	sformat(AL, '~~w~~t~~~w|', [N1]),
	% words will be centered
	sformat(AC, '~~t~~w~~t~~~w|', [N1]),
	% words will be right aligned
	sformat(AR, '~~t~~w~~~w|', [N1]),

	write('Left justified :'), nl,
	maplist(affiche(AL), LP), nl,
	write('Centered justified :'), nl,
	maplist(affiche(AC), LP), nl,
	write('Right justified :'), nl,
	maplist(affiche(AR), LP), nl.

affiche(F, L) :-
	maplist(my_format(F), L),
	nl.

my_format(_F, [13]) :-
	nl.

my_format(F, W) :-
	string_to_atom(W,AW),
	sformat(AF, F, [AW]),
	write(AF).


parse([], Max, Max) --> [].

parse(T, N, Max) -->
	{ parse_line(T, 0, N1, T1, L, []),
	  (   N1 > N -> N2 = N1; N2 = N)},
	[L],
	parse(T1, N2, Max).

parse_line([], NF, NF, []) --> [].

parse_line([H|TF], NF, NF, TF) -->
	{code_type(H, end_of_line), !},
	[].


parse_line(T, N, NF, TF) -->
	{ parse_word(T, 0, N1, T1, W, []),
	  (   N1 > N -> N2 = N1; N2 = N)},
	[W],
	parse_line(T1, N2, NF, TF).

% 36 is the code of '$'
parse_word([36|T], N, N, T) -->
	{!},
	[].

parse_word([H|T], N, N, [H|T]) -->
	{code_type(H, end_of_line), !},
	[].

parse_word([], N, N, []) --> [].

parse_word([H|T], N1, NF, TF) -->
	[H],
	{N2 is  N1 + 1},
	parse_word(T, N2, NF, TF).

```


{{out}}
<FONT SIZE="2">
```txt
 ?- aligner.
Left justified :
Given      a          text       file       of         many       lines,     where      fields     within     a          line
are        delineated by         a          single     'dollar'   character, write      a          program
that       aligns     each       column     of         fields     by         ensuring   that       words      in         each
column     are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a          column     to         be         either     left
justified, right      justified, or         center     justified  within     its        column.

Centered justified :
   Given        a        text       file        of        many      lines,      where     fields     within        a        line
    are    delineated     by          a       single    'dollar'  character,    write        a       program
   that      aligns      each      column       of       fields       by      ensuring     that       words       in        each
  column       are     separated     by         at        least       one      space.
 Further,     allow       for       each       word        in          a       column       to         be       either      left
justified,    right   justified,     or       center    justified   within       its      column.

Right justified :
      Given          a       text       file         of       many     lines,      where     fields     within          a       line
        are delineated         by          a     single   'dollar' character,      write          a    program
       that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
     column        are  separated         by         at      least        one     space.
   Further,      allow        for       each       word         in          a     column         to         be     either       left
 justified,      right justified,         or     center  justified     within        its    column.

true .
```
</FONT>


## PureBasic

{{works with|PureBasic|4.41}}

```PureBasic
Declare max(a,b)

If OpenConsole()
  Define a, i, x, y, maxlen
  Dim txt.s(0)
  Restore lines             ; Get address of the first data block
  Read.i  a
  ReDim txt(a)
  For i=0 To a              ; Read the raw data lines
    Read.s txt(i)
    txt(i)=Trim(txt(i),"$") ; Remove any bad '$' that may be useless in the end...
    x=max(CountString(txt(i),"$"),x)
  Next
  y=a
  Dim matrix.s(x,y)         ; Set up a nice matrix to work with, each word cleanly separated
  For x=0 To ArraySize(matrix(),1)
    For y=0 To ArraySize(matrix(),2)
      matrix(x,y)=StringField(txt(y),x+1,"$")
      maxlen=max(maxlen,Len(matrix(x,y)))
    Next
  Next
  If maxlen%2
    maxlen+1                ; Just to make sure that 'centered' output looks nice....
  EndIf

  PrintN(#CRLF$+"---- Right ----")
  For y=0 To ArraySize(matrix(),2)
    For x=0 To ArraySize(matrix(),1)
      Print(RSet(matrix(x,y),maxlen+1))
    Next
    PrintN("")
  Next

  PrintN(#CRLF$+"---- Left ----")
  For y=0 To ArraySize(matrix(),2)
    For x=0 To ArraySize(matrix(),1)
      Print(LSet(matrix(x,y),maxlen+1))
    Next
    PrintN("")
  Next

  PrintN(#CRLF$+"---- Center ----")
  For y=0 To ArraySize(matrix(),2)
    For x=0 To ArraySize(matrix(),1)
      a=maxlen-Len(matrix(x,y))
      Print(LSet(RSet(matrix(x,y),maxlen-a/2),maxlen))
    Next
    PrintN("")
  Next

  PrintN(#CRLF$+#CRLF$+"Press ENTER to quit."): Input()
  CloseConsole()
EndIf


Procedure max(x,y)
  If x>=y
    ProcedureReturn x
  Else
    ProcedureReturn y
  EndIf
EndProcedure


DataSection
lines:
  Data.i  5 ; e.g. 6-1 since first line is equal to 'zero'.
text:
  Data.s "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
  Data.s "are$delineated$by$a$single$'dollar'$character,$write$a$program"
  Data.s "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
  Data.s "column$are$separated$by$at$least$one$space."
  Data.s "Further,$allow$for$each$word$in$a$column$oo$be$either$left$"
  Data.s "justified,$right$justified,$or$center$justified$within$its$column."
EndDataSection
```



## Python


### Procedural


### =Using StringIO=


```python
from StringIO import StringIO

textinfile = '''Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.'''

j2justifier = dict(L=str.ljust, R=str.rjust, C=str.center)

def aligner(infile, justification = 'L'):
  ''' \
  Justify columns of textual tabular input where the row separator is the newline
  and the field separator is a 'dollar' character.
  justification can be L, R, or C; (Left, Right, or Centered).

  Return the justified output as a string
  '''
  assert justification in j2justifier, "justification can be L, R, or C; (Left, Right, or Centered)."
  justifier = j2justifier[justification]

  fieldsbyrow= [line.strip().split('$') for line in infile]
  # pad to same number of fields per row
  maxfields = max(len(row) for row in fieldsbyrow)
  fieldsbyrow = [fields + ['']*(maxfields - len(fields))
                    for fields in fieldsbyrow]
  # rotate
  fieldsbycolumn = zip(*fieldsbyrow)
  # calculate max fieldwidth per column
  colwidths = [max(len(field) for field in column)
               for column in fieldsbycolumn]
  # pad fields in columns to colwidth with spaces
  fieldsbycolumn = [ [justifier(field, width) for field in column]
                     for width, column in zip(colwidths, fieldsbycolumn) ]
  # rotate again
  fieldsbyrow = zip(*fieldsbycolumn)

  return "\n".join( " ".join(row) for row in fieldsbyrow)


for align in 'Left Right Center'.split():
  infile = StringIO(textinfile)
  print "\n# %s Column-aligned output:" % align
  print aligner(infile, align[0])
```


{{out}}

```txt

# Left Column-aligned output:
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

# Right Column-aligned output:
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

# Center Column-aligned output:
  Given        a         text     file    of      many     lines,    where    fields  within   a    line
   are     delineated     by       a    single  'dollar' character,  write      a    program
   that      aligns      each    column   of     fields      by     ensuring   that   words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word      in        a       column     to      be   either left
justified,   right    justified,   or   center justified   within     its    column.
```



### =Brief native version=

Works with Python 2 and 3.


```python
'''
cat <<'EOF' > align_columns.dat
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
EOF
'''

for align in '<^>':
  rows = [ line.strip().split('$') for line in open('align_columns.dat') ]
  fmts = [ '{:%s%d}' % (align, max( len(row[i]) if i < len(row) else 0 for row in rows ))
           for i in range(max(map(len, rows))) ]
  for row in rows:
    print(' '.join(fmts).format(*(row + [''] * len(fmts))))
  print('')
```




### =Alternative=

{{trans|D}}

```python
txt = """Given$a$txt$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column."""

parts = [line.rstrip("$").split("$") for line in txt.splitlines()]

max_widths = {}
for line in parts:
    for i, word in enumerate(line):
        max_widths[i] = max(max_widths.get(i, 0), len(word))

for i, justify in enumerate([str.ljust, str.center, str.rjust]):
    print ["Left", "Center", "Right"][i], " column-aligned output:\n"
    for line in parts:
        for j, word in enumerate(line):
            print justify(word, max_widths[j]),
        print
    print "- " * 52
```



### Functional


### =As a fold=

A fold/'''reduce''' between two transpositions.

(Selection of string justification methods via '''getattr'''):
{{Works with|Python|3.7}}

```python
'''Variously aligned columns
   from delimited text.
'''

from functools import reduce
from itertools import repeat


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test of three alignments.'''

    txt = '''Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.'''

    rows = [x.split('$') for x in txt.splitlines()]
    table = paddedRows(max(map(len, rows)))('')(rows)

    print('\n\n'.join(map(
        alignedTable(table)('  '),
        [-1, 0, 1]  # Left, Center, Right
    )))


# alignedTable :: [[String]] -> Alignment -> String -> String
def alignedTable(rows):
    '''Tabulation of rows of cells, with cell alignment
       specified by:
           eAlign -1 = left
           eAlign  0 = center
           eAlign  1 = right
       and separator between columns
       supplied by the `sep` argument.
    '''
    def go(sep, eAlign):
        lcr = ['ljust', 'center', 'rjust'][1 + eAlign]

        # nextAlignedCol :: [[String]] -> [String] -> [[String]]
        def nextAlignedCol(cols, col):
            w = max(len(cell) for cell in col)
            return cols + [
                [getattr(s, lcr)(w, ' ') for s in col]
            ]

        return '\n'.join([
            sep.join(cells) for cells in
            zip(*reduce(nextAlignedCol, zip(*rows), []))
        ])
    return lambda sep: lambda eAlign: go(sep, eAlign)


# GENERIC -------------------------------------------------

# paddedRows :: Int -> a -> [[a]] -> [[a]]
def paddedRows(n):
    '''A list of rows of even length,
       in which each may be padded (but
       not truncated) to length n with
       appended copies of value v.'''
    def go(v, xs):
        def pad(x):
            d = n - len(x)
            return (x + list(repeat(v, d))) if 0 < d else x
        return [pad(row) for row in xs]
    return lambda v: lambda xs: go(v, xs) if xs else []


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
Given       a           text        file    of      many       lines,      where     fields   within   a       line
are         delineated  by          a       single  'dollar'   character,  write     a        program
that        aligns      each        column  of      fields     by          ensuring  that     words    in      each
column      are         separated   by      at      least      one         space.
Further,    allow       for         each    word    in         a           column    to       be       either  left
justified,  right       justified,  or      center  justified  within      its       column.

  Given         a          text      file     of       many      lines,     where     fields   within    a     line
   are      delineated      by        a     single   'dollar'  character,   write       a     program
   that       aligns       each     column    of      fields       by      ensuring    that    words     in    each
  column       are      separated     by      at      least       one       space.
 Further,     allow        for       each    word       in         a        column      to       be    either  left
justified,    right     justified,    or    center  justified    within      its     column.

     Given           a        text    file      of       many      lines,     where   fields   within       a  line
       are  delineated          by       a  single   'dollar'  character,     write        a  program
      that      aligns        each  column      of     fields          by  ensuring     that    words      in  each
    column         are   separated      by      at      least         one    space.
  Further,       allow         for    each    word         in           a    column       to       be  either  left
justified,       right  justified,      or  center  justified      within       its  column.
```



## R


```R
# Read in text
lines <- readLines(tc <- textConnection("Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.")); close(tc)

#Split words by the dollar
words <- strsplit(lines, "\\$")

#Reformat
maxlen <- max(sapply(words, length))
words <- lapply(words, function(x) {length(x) <- maxlen; x})
block <- matrix(unlist(words), byrow=TRUE, ncol=maxlen)
block[is.na(block)] <- ""
leftjust <- format(block)
rightjust <- format(block, justify="right")
centrejust <- format(block, justify="centre")

# Print
print0 <- function(x) invisible(apply(x, 1, function(x) cat(x, "\n")))
print0(leftjust)
print0(rightjust)
print0(centrejust)
```

Right justified output shown.
<div style="width:full;overflow:scroll">
```txt

     Given          a       text       file         of       many     lines,      where     fields     within          a       line
       are delineated         by          a     single   'dollar' character,      write          a    program
      that     aligns       each     column         of     fields         by   ensuring       that      words         in       each
    column        are  separated         by         at      least        one     space.
  Further,      allow        for       each       word         in          a     column         to         be     either       left
justified,      right justified,         or     center  justified     within        its    column.

```
</div>


## Racket


```Racket

#lang racket

(define (display-aligned text #:justify [justify 'left])
  (define lines
    (for/list ([line (regexp-split #rx"\n" text)])
      (regexp-split #rx"\\$" line)))
  (define width
    (add1 (for*/fold ([m 0]) ([line lines] [word line])
            (max m (string-length word)))))
  (define spaces (make-string width #\space))
  (for ([line lines])
    (for* ([word line]
           [strs (let ([spc (substring spaces (string-length word))])
                   (case justify
                     [(left)  (list word spc)]
                     [(right) (list spc word)]
                     [(center) (let ([i (quotient (string-length spc) 2)])
                                 (list (substring spc i)
                                       word
                                       (substring spc 0 i)))]))])
      (display strs))
    (newline)))

(define text
  "Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.")

(display-aligned text)
(display-aligned #:justify 'right text)
(display-aligned #:justify 'center text)

```



## RapidQ


```vb

Dim MText as QMemorystream
    MText.WriteLine "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
    MText.WriteLine "are$delineated$by$a$single$'dollar'$character,$write$a$program"
    MText.WriteLine "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
    MText.WriteLine "column$are$separated$by$at$least$one$space."
    MText.WriteLine "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
    MText.WriteLine "justified,$right$justified,$or$center$justified$within$its$column."

DefStr TextLeft, TextRight, TextCenter
DefStr MLine, LWord, Newline = chr$(13)+chr$(10)
DefInt ColWidth(100), ColCount
DefSng NrSpaces

'Find column widths
MText.position = 0
for x = 0 to MText.linecount -1
    MLine = MText.ReadLine
    for y = 0 to Tally(MLine, "$")
        LWord = Field$(MLine, "$", y+1)
        ColWidth(y) = iif (ColWidth(y) < len(LWord), len(LWord), ColWidth(y))
    next
next

'Create aligned wordlists
MText.position = 0
for x = 0 to MText.linecount -1
    MLine = MText.ReadLine
    for y = 0 to Tally(MLine, "$")
        LWord = Field$(MLine, "$", y+1)
        NrSpaces = ColWidth(y) - len(LWord)
        'left align
        TextLeft = TextLeft + LWord + Space$(NrSpaces+1)
        'Right align
        TextRight = TextRight + Space$(NrSpaces+1) + LWord
        'Center
        TextCenter = TextCenter + Space$(floor((NrSpaces)/2)+1) + LWord + Space$(Ceil((NrSpaces)/2))
    next
    TextLeft = TextLeft + Newline
    TextRight = TextRight + Newline
    TextCenter = TextCenter + Newline
next

```

{{out}}
<pre style="height:20ex;overflow:scroll">TextLeft contains:
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

TextRight contains:
      Given          a       text   file     of      many     lines,    where  fields  within      a line
        are delineated         by      a single  'dollar' character,    write       a program
       that     aligns       each column     of    fields         by ensuring    that   words     in each
     column        are  separated     by     at     least        one   space.
   Further,      allow        for   each   word        in          a   column      to      be either left
 justified,      right justified,     or center justified     within      its column.

TextCenter contains:
   Given        a         text     file    of     many      lines,    where   fields  within    a    line
    are     delineated     by       a    single 'dollar'  character,  write      a    program
    that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
   column      are     separated    by     at     least      one      space.
  Further,    allow       for      each   word     in         a       column    to      be    either left
 justified,   right    justified,   or   center justified   within     its    column.

```



## REBOL


```rebol
REBOL [
	Title: "Align Columns"
	URL: http://rosettacode.org/wiki/Align_columns
]

specimen: {Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.}

; Parse specimen into data grid.

data: copy []
foreach line parse specimen to-string lf [ ; Break into lines.
	append/only data parse line "$"        ; Break into columns.
]

; Compute independent widths for each column.

widths: copy []  insert/dup widths 0 length? data/1
foreach line data [
	forall line [
		i: index? line
		widths/:i: max widths/:i length? line/1
	]
]

pad: func [n /local x][x: copy ""  insert/dup x " " n  x]

; These formatting functions are passed as arguments to entable.

right: func [n s][rejoin [pad n - length? s  s]]

left: func [n s][rejoin [s  pad n - length? s]]

centre: func [n s /local h][
	h: round/down (n - length? s) / 2
	rejoin [pad h  s  pad n - h - length? s]
]

; Display data as table.

entable: func [data format] [
	foreach line data [
		forall line [
			prin rejoin [format  pick widths index? line  line/1  " "]
		]
		print ""
	]
]

; Format data table.

foreach i [left centre right] [
	print ["^/Align" i "...^/"]  entable data get i]

```


{{out}}
<pre style="height:15ex;overflow:scroll">Align left ...

Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

Align centre ...

  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

Align right ...

     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

```



## Red


```Red
Red [
  Title: "Align Columns"
  Original-Author: oofoe
]

text: {Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.}

; Parse specimen into data grid.

data: copy []
foreach line split text lf [
  append/only data split line "$"
]

; Compute independent widths for each column.

widths: copy []
foreach line data [
  forall line [
    i: index? line
    if i > length? widths [append widths 0]
    widths/:i: max widths/:i length? line/1
  ]
]

pad: function [n] [x: copy "" insert/dup x " " n x]

; These formatting functions are passed as arguments to entable.

right: func [n s][rejoin [pad n - length? s s]]

left: func [n s][rejoin [s pad n - length? s]]

centre: function [n s] [
  d: n - length? s
  h: round/down d / 2
  rejoin [pad h s pad d - h]
]

; Display data as table.

entable: func [data format] [
  foreach line data [
    forall line [
      prin rejoin [format pick widths index? line line/1 " "]
    ]
    print ""
  ]
]

; Format data table.

foreach i [left centre right] [
  print [newline "Align" i "..." newline]  entable data get i]
```



## REXX

===(no output)===

```rexx
/*REXX*/
z.1 = "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
z.2 = "are$delineated$by$a$single$'dollar'$character,$write$a$program"
z.3 = "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
z.4 = "column$are$separated$by$at$least$one$space."
z.5 = "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
z.6 = "justified,$right$justified,$or$center$justified$within$its$column."

word. = ""
width. = 0
maxcol = 0
do row = 1 to 6
  line = z.row
  do col = 1 by 1 until length(line) = 0
    parse var line word.row.col "$" line
    if length(word.row.col) > width.col then width.col = length(word.row.col)
  end
  if col > maxcol then maxcol = col
end

say "align left:"
say
do row = 1 to 6
  out = ""
  do col = 1 to maxcol
    out = out || left(word.row.col,width.col+1)
  end
  say out
end
say
say "align right:"
say
do row = 1 to 6
  out = ""
  do col = 1 to maxcol
    out = out || right(word.row.col,width.col+1)
  end
  say out
end
say
say "align center:"
say
do row = 1 to 6
  out = ""
  do col = 1 to maxcol
    out = out || center(word.row.col,width.col+1)
  end
  say out
end
```


===(with output)===

```rexx
/*REXX program displays  various alignments  for words in an array of  text strings.    */
cols=0;     size=0;     wid.=0;     t.=;     @.= /*zero or nullify some variables.      */
                                                 /* [↓]   some "text" lines.            */
t.1 = "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
t.2 = "are$delineated$by$a$single$'dollar'$character,$write$a$program"
t.3 = "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
t.4 = "column$are$separated$by$at$least$one$space."
t.5 = "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
t.6 = "justified,$right$justified,$or$center$justified$within$its$column."
                                                 /* [↑]  a null line is the end of text.*/
  do r=1  while  t.r\==''                        /* [↓]  process all the text lines.    */
  _=strip(t.r,,'$')                              /*strip leading & trailing dollar signs*/
                     do c=1  until _==''         /* [↓]  process each of the words.     */
                     parse  var  _    @.r.c  '$'  _
                     wid.c=max(wid.c, length(@.r.c))     /*find the maximum word width. */
                     end   /*c*/
  cols=max(cols,c)                               /*use the maximum COLS found.          */
  end    /*r*/

  do k=1  for cols;  size=size+wid.k;  end       /*find the width of the biggest line.  */
rows=r-1                                         /*adjust ROWS because of the  DO  loop.*/
  do j=1  for 3;     say;     say                /*show two blank lines for a separator.*/
  say center(word('left right center', j)  "aligned", size+cols-1, "═")     /*show title*/
                do    r=1  for rows;   _=                /*construct row by row.        */
                   do c=1  for cols;   x=@.r.c           /*     "    col  " col.        */
                   if j==1  then _=_   left(x, wid.c)    /*justified    left.           */
                   if j==2  then _=_  right(x, wid.c)    /*    "       right.           */
                   if j==3  then _=_ centre(x, wid.c)    /*    "      center.           */
                   end   /*c*/
                say substr(_, 2)                 /*ignore the leading extra blank.      */
                end      /*r*/
  end   /*j*/                                    /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

══════════════════════════════════════════════left aligned══════════════════════════════════════════════
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.


═════════════════════════════════════════════right aligned══════════════════════════════════════════════
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.


═════════════════════════════════════════════center aligned═════════════════════════════════════════════
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

```


===(boxed output)===
Note: This version boxes each column of output to better show the columns.

```rexx
/*REXX pgm displays various (boxed) alignments for words in an array of text strings.   */
cols=0;     size=0;     wid.=0;     t.=;     @.= /*zero or nullify some variables.      */
                                                 /* [↓]   some "text" lines.            */
t.1 = "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
t.2 = "are$delineated$by$a$single$'dollar'$character,$write$a$program"
t.3 = "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
t.4 = "column$are$separated$by$at$least$one$space."
t.5 = "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
t.6 = "justified,$right$justified,$or$center$justified$within$its$column."
                                                 /* [↑]  a null line is the end of text.*/
  do r=1  while  t.r\==''                        /* [↓]  process all the text lines.    */
  _=strip(t.r,,'$')                              /*strip leading & trailing dollar signs*/
                     do c=1  until _==''         /* [↓]  process each of the words.     */
                     parse  var  _    @.r.c  '$'  _
                     wid.c=max(wid.c, length(@.r.c))     /*find the maximum word width. */
                     end   /*c*/
  cols=max(cols,c)                               /*use the maximum COLS found.          */
  end    /*r*/

  do k=1  for cols;  size=size+wid.k;  end       /*find the width of the biggest line.  */
rows=r-1                                         /*adjust ROWS because of the  DO  loop.*/
  do j=1  for 3;     say;     say                /*show two blank lines for a separator.*/
  say center(word('left right center', j)  "aligned", size+cols+1, "═")     /*show title*/

                 do r=0  to rows;    _=;      !='│';           if r==0  then !='┬'
                       do c=1  for cols;                       x=@.r.c
                       if r==0  then x=copies("─", wid.c +1)
                       if j==1  then _=_  ||  !  ||    left(x, wid.c)
                       if j==2  then _=_  ||  !  ||   right(x, wid.c)
                       if j==3  then _=_  ||  !  ||  centre(x, wid.c)
                       end   /*c*/
                 if r==0  then do;    _= '┌'substr(_, 2, length(_) -1)"┐"
                                    bot= '└'substr(_, 2, length(_) -2)"┘"
                               end
                          else _=_ || !
                 say _
                 end         /*r*/               /* [↑]  shows words in boxes.          */
  say translate(bot, '┴', "┬")
  end   /*j*/                                    /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default input:}}

```txt

═══════════════════════════════════════════════left aligned═══════════════════════════════════════════════
┌──────────┬──────────┬──────────┬──────┬──────┬─────────┬──────────┬────────┬───────┬───────┬──────┬────┐
│Given     │a         │text      │file  │of    │many     │lines,    │where   │fields │within │a     │line│
│are       │delineated│by        │a     │single│'dollar' │character,│write   │a      │program│      │    │
│that      │aligns    │each      │column│of    │fields   │by        │ensuring│that   │words  │in    │each│
│column    │are       │separated │by    │at    │least    │one       │space.  │       │       │      │    │
│Further,  │allow     │for       │each  │word  │in       │a         │column  │to     │be     │either│left│
│justified,│right     │justified,│or    │center│justified│within    │its     │column.│       │      │    │
└──────────┴──────────┴──────────┴──────┴──────┴─────────┴──────────┴────────┴───────┴───────┴──────┴────┘


══════════════════════════════════════════════right aligned═══════════════════════════════════════════════
┌──────────┬──────────┬──────────┬──────┬──────┬─────────┬──────────┬────────┬───────┬───────┬──────┬────┐
│     Given│         a│      text│  file│    of│     many│    lines,│   where│ fields│ within│     a│line│
│       are│delineated│        by│     a│single│ 'dollar'│character,│   write│      a│program│      │    │
│      that│    aligns│      each│column│    of│   fields│        by│ensuring│   that│  words│    in│each│
│    column│       are│ separated│    by│    at│    least│       one│  space.│       │       │      │    │
│  Further,│     allow│       for│  each│  word│       in│         a│  column│     to│     be│either│left│
│justified,│     right│justified,│    or│center│justified│    within│     its│column.│       │      │    │
└──────────┴──────────┴──────────┴──────┴──────┴─────────┴──────────┴────────┴───────┴───────┴──────┴────┘


══════════════════════════════════════════════center aligned══════════════════════════════════════════════
┌──────────┬──────────┬──────────┬──────┬──────┬─────────┬──────────┬────────┬───────┬───────┬──────┬────┐
│  Given   │    a     │   text   │ file │  of  │  many   │  lines,  │ where  │fields │within │  a   │line│
│   are    │delineated│    by    │  a   │single│'dollar' │character,│ write  │   a   │program│      │    │
│   that   │  aligns  │   each   │column│  of  │ fields  │    by    │ensuring│ that  │ words │  in  │each│
│  column  │   are    │separated │  by  │  at  │  least  │   one    │ space. │       │       │      │    │
│ Further, │  allow   │   for    │ each │ word │   in    │    a     │ column │  to   │  be   │either│left│
│justified,│  right   │justified,│  or  │center│justified│  within  │  its   │column.│       │      │    │
└──────────┴──────────┴──────────┴──────┴──────┴─────────┴──────────┴────────┴───────┴───────┴──────┴────┘

```



## Ruby

{{works with|Ruby|1.9.3+}}

```ruby
J2justifier = {Left: :ljust, Right: :rjust, Center: :center}

=begin
Justify columns of textual tabular input where the record separator is the newline
and the field separator is a 'dollar' character.
justification can be Symbol; (:Left, :Right, or :Center).

Return the justified output as a string
=end
def aligner(infile, justification = :Left)
  fieldsbyrow = infile.map {|line| line.strip.split('$')}
  # pad to same number of fields per record
  maxfields = fieldsbyrow.map(&:length).max
  fieldsbyrow.map! {|row| row + ['']*(maxfields - row.length)}
  # calculate max fieldwidth per column
  colwidths = fieldsbyrow.transpose.map {|column|
    column.map(&:length).max
  }
  # pad fields in columns to colwidth with spaces
  justifier = J2justifier[justification]
  fieldsbyrow.map {|row|
    row.zip(colwidths).map {|field, width|
      field.send(justifier, width)
    }.join(" ")
  }.join("\n")
end

require 'stringio'

textinfile = <<END
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
END

for align in [:Left, :Right, :Center]
  infile = StringIO.new(textinfile)
  puts "\n# %s Column-aligned output:" % align
  puts aligner(infile, align)
end

```


{{out}}

```txt

# Left Column-aligned output:
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

# Right Column-aligned output:
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

# Center Column-aligned output:
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

```



## Run BASIC


```Runbasic
theString$ = "Given$a$text$file$of$many$lines,$where$fields$within$a$line$" _
+ "are$delineated$by$a$single$'dollar'$character,$write$a$program" _
+ "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"_
+ "column$are$separated$by$at$least$one$space." _
+ "Further,$allow$for$each$word$in$a$column$to$be$either$left$" _
+ "justified,$right$justified,$or$center$justified$within$its$column."

x = shoTable(theString$,"left",6)
x = shoTable(theString$,"right",6)
x = shoTable(theString$,"center",6)
end

FUNCTION shoTable(theString$,align$,across)
print "------------ align:";align$;" -- across:";across;" ------------"
dim siz(across)
b$ = "                                          "
while word$(theString$,i+1,"$") <> ""
	siz(i mod across) = max(siz(i mod across),len(word$(theString$,i + 1,"$")))
	i = i + 1
wend
for i = 0 to across - 1
	siz(i) = siz(i) + 1
	if siz(i) and 1 then siz(i) = siz(i) + 1
next i

i = 0
a$ = word$(theString$,i+1,"$")
while a$ <> ""
        s = siz(i mod across) - len(a$)
	if align$ = "right"   then a$ = left$(b$,s);a$
	if align$ = "left"    then a$ = a$;left$(b$,s)
	if align$ = "center"  then a$ = left$(b$,int(s / 2));a$;left$(b$,int(s / 2) + (s and 1))
	print "|";a$;
	i  = i + 1
	if i mod across = 0 then print "|"
	a$ = word$(theString$,i+1,"$")
wend
print
end function
```

{{out}}
<pre style="height:15ex;overflow:scroll">------------ align:left -- across:6 ------------
|Given       |a           |text    |file        |of              |many      |
|lines,      |where       |fields  |within      |a               |line      |
|are         |delineated  |by      |a           |single          |'dollar'  |
|character,  |write       |a       |programthat |aligns          |each      |
|column      |of          |fields  |by          |ensuring        |that      |
|words       |in          |each    |column      |are             |separated |
|by          |at          |least   |one         |space.Further,  |allow     |
|for         |each        |word    |in          |a               |column    |
|to          |be          |either  |left        |justified,      |right     |
|justified,  |or          |center  |justified   |within          |its       |
|column.
------------ align:right -- across:6 ------------
|       Given|           a|    text|        file|              of|      many|
|      lines,|       where|  fields|      within|               a|      line|
|         are|  delineated|      by|           a|          single|  'dollar'|
|  character,|       write|       a| programthat|          aligns|      each|
|      column|          of|  fields|          by|        ensuring|      that|
|       words|          in|    each|      column|             are| separated|
|          by|          at|   least|         one|  space.Further,|     allow|
|         for|        each|    word|          in|               a|    column|
|          to|          be|  either|        left|      justified,|     right|
|  justified,|          or|  center|   justified|          within|       its|
|     column.
------------ align:center -- across:6 ------------
|   Given    |     a      |  text  |    file    |       of       |   many   |
|   lines,   |   where    | fields |   within   |       a        |   line   |
|    are     | delineated |   by   |     a      |     single     | 'dollar' |
| character, |   write    |   a    |programthat |     aligns     |   each   |
|   column   |     of     | fields |     by     |    ensuring    |   that   |
|   words    |     in     |  each  |   column   |      are       |separated |
|     by     |     at     | least  |    one     | space.Further, |  allow   |
|    for     |    each    |  word  |     in     |       a        |  column  |
|     to     |     be     | either |    left    |   justified,   |  right   |
| justified, |     or     | center | justified  |     within     |   its    |
|  column.
```



## Rust


```rust
use std::iter::{Extend, repeat};

enum AlignmentType { Left, Center, Right }

fn get_column_widths(text: &str) -> Vec<usize> {
    let mut widths = Vec::new();
    for line in text.lines().map(|s| s.trim_matches(' ').trim_right_matches('$')) {
        let lens = line.split('$').map(|s| s.chars().count());
        for    (idx, len) in lens.enumerate() {
            if idx < widths.len() {
                widths[idx] = std::cmp::max(widths[idx], len);
            }
            else {
                widths.push(len);
            }
        }
    }
    widths
}

fn align_columns(text: &str, alignment: AlignmentType) -> String {
    let widths = get_column_widths(text);
    let mut result = String::new();
    for line in text.lines().map(|s| s.trim_matches(' ').trim_right_matches('$')) {
        for (s, w) in line.split('$').zip(widths.iter()) {
            let blank_count = w - s.chars().count();
            let (pre, post) = match alignment {
                AlignmentType::Left => (0, blank_count),
                AlignmentType::Center => (blank_count / 2, (blank_count + 1) / 2),
                AlignmentType::Right => (blank_count, 0),
            };
            result.extend(repeat(' ').take(pre));
            result.push_str(s);
            result.extend(repeat(' ').take(post));
            result.push(' ');
        }
        result.push_str("\n");
    }
    result
}

fn main() {
    let text = r#"Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column."#;

    println!("{}", align_columns(text, AlignmentType::Left));
    println!("{}", repeat('-').take(110).collect::<String>());
    println!("{}", align_columns(text, AlignmentType::Center));
    println!("{}", repeat('-').take(110).collect::<String>());
    println!("{}", align_columns(text, AlignmentType::Right));
}
```

{{out}}
<pre style="height:15ex;overflow:scroll">
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

---------------------------------------------------------------------------------------------------------
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

---------------------------------------------------------------------------------------------------------
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

```



## Scala

{{works with|scala|2.8.0.r18997-b20091009021954}}

For Scala 2.7, change from fromPath to fromFile, and remove the extra parameter to Source's getLines.


```scala
object ColumnAligner {
  val eol = System.getProperty("line.separator")
  def getLines(filename: String) = scala.io.Source.fromPath(filename).getLines(eol)
  def splitter(line: String) = line split '$'
  def getTable(filename: String) = getLines(filename) map splitter
  def fieldWidths(fields: Array[String]) = fields map (_ length)
  def columnWidths(txt: Iterator[Array[String]]) = (txt map fieldWidths).toList.transpose map (_ max)

  def alignField(alignment: Char)(width: Int)(field: String) = alignment match {
    case 'l' | 'L' => "%-"+width+"s" format field
    case 'r' | 'R' => "%"+width+"s" format field
    case 'c' | 'C' => val padding = (width - field.length) / 2; " "*padding+"%-"+(width-padding)+"s" format field
    case _ => throw new IllegalArgumentException
  }

  def align(aligners: List[String => String])(fields: Array[String]) =
    aligners zip fields map Function.tupled(_ apply _)

  def alignFile(filename: String, alignment: Char) = {
    def table = getTable(filename)
    val aligners = columnWidths(table) map alignField(alignment)
    table map align(aligners) map (_ mkString " ")
  }

  def printAlignedFile(filename: String, alignment: Char) {
    alignFile(filename, alignment) foreach println
  }
}
```


Another take:


```scala
def pad(s:String, i:Int, d:String) = {
  val padsize = (i-s.length).max(0)
  d match {
    case "left" => s+" "*padsize
    case "right" => " "*padsize+s
    case "center" => " "*(padsize/2) + s + " "*(padsize-padsize/2)
  }
}

val lines = scala.io.Source.fromFile("c:\\text.txt").getLines.map(_.trim())
val words = lines.map(_.split("\\$").toList).toList
val lens = words.map(l => l.map(_.length)).toList

var maxlens = Map[Int,Int]() withDefaultValue 0
lens foreach (l =>
  for(i <- (0 until l.length)){
    maxlens += i -> l(i).max(maxlens(i))
  }
)

val padded = words map ( _.zipWithIndex.map{case(s,i)=>pad(s,maxlens(i),"center")+" "} )
padded map (_.reduceLeft(_ + _)) foreach println
```



## Scheme



```scheme

(import (scheme base)
        (scheme write)
        (srfi 1)
        (except (srfi 13) string-for-each string-map)
        (srfi 14))

;; text is a list of lines, alignment is left/right/center
;; displays the aligned text in columns with a single space gap
(define (align-columns text alignment)
  (define (split line) ; splits string on $ into list of strings
    (string-tokenize line (char-set-complement (->char-set "$"))))
  (define (extend lst n) ; extends list to length n, by adding "" to end
    (append lst (make-list (- n (length lst)) "")))
  (define (align-word word width) ; align single word to fit width
    (case alignment
      ((left) (string-pad-right word width))
      ((right) (string-pad word width))
      ((center) (let ((rem (- width (string-length word))))
                  (string-pad-right (string-pad word (- width (truncate (/ rem 2))))
                                    width)))))
  ;
  (display alignment) (newline)
  (let* ((text-list (map split text))
         (max-line-len (fold (lambda (text val) (max (length text) val)) 0 text-list))
         (text-lines (map (lambda (line) (extend line max-line-len)) text-list))
         (min-col-widths (map (lambda (col)
                                (fold (lambda (line val)
                                        (max (string-length (list-ref line col))
                                             val))
                                      0
                                      text-lines))
                              (iota max-line-len))))
    (map (lambda (line)
           (map (lambda (word width)
                  (display (string-append (align-word word width)
                                          " ")))
                line min-col-widths)
           (newline))
         text-lines))
  (newline))

;; show example
(define *example*
  '("Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
    "are$delineated$by$a$single$'dollar'$character,$write$a$program"
    "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
    "column$are$separated$by$at$least$one$space."
    "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
    "justified,$right$justified,$or$center$justified$within$its$column."))

(align-columns *example* 'left)
(align-columns *example* 'center)
(align-columns *example* 'right)

```


{{out}}

```txt

left
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

center
   Given        a        text     file    of      many     lines,     where   fields  within    a   line
    are    delineated     by        a   single  'dollar' character,   write     a    program
   that      aligns      each    column   of     fields      by     ensuring   that   words    in   each
  column       are     separated   by     at     least       one     space.
 Further,     allow       for     each   word      in         a      column     to      be   either left
justified,    right   justified,   or   center justified   within      its   column.

right
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.


```




## sed

The code allows to left (by default) or right justify colums. Centering is not supported. Requires about 2x<size of input> bytes of memory (each line duplicated).

```sed

#!/bin/sed -nrf
# Format: <master-pattern>\n<line1>\n<line1-as-pattern>\n<line2>\n<line2-as-pattern>...
# After reading whole file <master-pattern> contains max number of fields of max width each.

# If no $ at start or end of a line -- add them
/^\$/! s/^/$/
/\$$/! s/$/$/

# First line saved as three lines in hold space:
# <line1-as-pattern>\n<line1>\n<line1-as-pattern>
1{
 h
 s/[^$]/ /g
 H
 G
 x
 # Restart -- go to next line
 b
}

# For lines 2,3,...
H
# Current line -> pattern
# (each character replaced by constant symbol (e.g. space) so that we can count them)
s/[^$]/ /g
H
G
# Add two markers
s/\$/1$/
s/(\n[^$]*)\$/\12$/

# Compare patterns
:cmp
	s/(1\$([^$\n]*)([^$\n]*)[^2]*2\$\2)/\1\3/
	/1\$\n/ bout
	# Advance markers
	s/1(\$[^12$\n]*)/\11/
	s/2(\$[^12$\n]*)/\12/
	# Add one more field
	/^[^2]*2\$\n/{ s/^([^2]*)2\$\n/\12$$\n/; }
bcmp
:out
# Remove first line
s/[^\n]*\n//
# Remove 2$-marker
s/2\$/$/
x

${
# We are on the last line -- start printing
	x;
	# Add a line for aligned string
	s/^/\n/
	:nextline
	# Add marker again (only one this time)
	s/\$/1$/
	:align
		# 1. look up missing spaces,
		# 2. put first word of 2nd line before first newline adding missing spaces
		# 3. cut first word of 2nd and 3rd lines.
		# Replace \5\3 by \3\5 for RIGHT ALIGNMENT
		s/(\n[^\n]*)1\$([^$\n]*)([^$\n]*)\$([^\n]*\n)\$([^$\n]*)([^\n]*\n)\$\2\$/\5\3 \1$\2\31$\4\6$/
	talign
	# We ate 2nd and 3rd lines completely, except newlines -- remove them
	s/\$\n\$\n\$\n/$\n/
	# Print the first line in pattern space
	P
	# ... and remove it
	s/^[^\n]*//
	# Remove marker
	s/1\$/$/
	# If no more lines -- exit
	/\$\n\$$/q
	bnextline
}

```

Example:

```txt

$ cat align.txt
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
$ ./align-columns.sed align.txt
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const array string: inputLines is [] (
    "Given$a$text$file$of$many$lines,$where$fields$within$a$line$",
    "are$delineated$by$a$single$'dollar'$character,$write$a$program",
    "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$",
    "column$are$separated$by$at$least$one$space.",
    "Further,$allow$for$each$word$in$a$column$to$be$either$left$",
    "justified,$right$justified,$or$center$justified$within$its$column.");

const func array integer: computeColumnWidths (in array string: inputLines) is func
  result
    var array integer: columnWidths is 0 times 0;
  local
    var string: line is "";
    var array string: lineFields is 0 times "";
    var integer: index is 0;
  begin
    for line range inputLines do
      lineFields := split(line, "$");
      if length(lineFields) > length(columnWidths) then
        columnWidths &:= (length(lineFields) - length(columnWidths)) times 0;
      end if;
      for index range 1 to length(lineFields) do
        if length(lineFields[index]) > columnWidths[index] then
          columnWidths[index] := length(lineFields[index]);
        end if;
      end for;
    end for;
  end func;

const func string: center (in string: stri, in integer: length) is
  return ("" lpad (length - length(stri)) div 2 <& stri) rpad length;

const proc: main is func
  local
    var array integer: columnWidths is 0 times 0;
    var string: line is "";
    var array string: lineFields is 0 times "";
    var integer: index is 0;
  begin
    columnWidths := computeColumnWidths(inputLines);
    for line range inputLines do
      lineFields := split(line, "$");
      for index range 1 to length(lineFields) do
        # write(lineFields[index] rpad columnWidths[index] <& " "); # Left justify
        # write(lineFields[index] lpad columnWidths[index] <& " "); # Right justify
        write(center(lineFields[index], columnWidths[index]) <& " ");
      end for;
      writeln;
    end for;
  end func;
```


{{out}}

```txt

  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

```



## Shiny


```shiny
text: 'Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$\'dollar\'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.'

align: action text; position;

    # split text into 2D array of lines and words
    lines : { for text.split ~\$?\r?\n~ { for a.split '$' a end } end }

    # calculate max required width for each column
    widths: { for lines for a here[b]: a.length.max here[b]? ends }

    spaces: action out ("%%%ds" in).format '' end

    # formatting functions
    left: action word; width;
        pad: width-word.length
        print "%s%s " word spaces pad
    end
    right: action word; width;
        pad: width-word.length
        print "%s%s " spaces pad word
    end
    center: action word; width;
        pad: (width-word.length)/2
        print "%s%s%s " spaces pad.floor word spaces pad.ceil
    end

    if position.match ~^(left|center|right)$~ for lines
        for a local[position] a widths[b] end say ''
    ends say ''
end

align text 'left'
align text 'center'
align text 'right'
```



```txt
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.
```



## Sidef


```ruby
class Format(text, width) {
    method align(j) {
        text.map { |row|
            row.range.map { |i|
                '%-*s ' % (width[i],
                  '%*s' % (row[i].len + (width[i]-row[i].len * j/2), row[i]));
            }.join("");
        }.join("\n") + "\n";
    }
}

func Formatter(text) {
    var textArr = [];
    var widthArr = [];

    text.each_line {
        var words = .split('$');
        textArr.append(words);

        words.each_kv { |i, word|
            if (i == widthArr.len) {
                widthArr.append(word.len);
            }
            elsif (word.len > widthArr[i]) {
                widthArr[i] = word.len;
            }
        }
    }

    return Format(textArr, widthArr);
}

enum |left, middle, right|;
const text = <<'EOT';
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
EOT

var f = Formatter(text);

say f.align(left);
say f.align(middle);
say f.align(right);
```



## Tcl


```tcl
package require Tcl 8.5

set text {Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.}

array set max {}
foreach line [split $text \n] {
    set col 0
    set thisline [split $line \$]
    lappend words $thisline
    foreach word $thisline {
        set max([incr col]) [expr {[info exists max($col)]
                                    ? max($max($col), [string length $word])
                                    : [string length $word]
                            }]
    }
}

proc justify {word position width} {
    switch -exact -- $position {
        left {
            return [format "%-*s" $width $word]
        }
        center {
            set lpadw [expr {($width - [string length $word])/2}]
            return [format "%s%-*s" [string repeat " " $lpadw] [incr width -$lpadw] $word]
        }
        right {
            return [format "%*s" $width $word]
        }
    }
}

foreach position {left center right} {
    foreach thisline $words {
        set col 0
        set line ""
        foreach word $thisline {
            append line [justify $word $position $max([incr col])] " "
        }
        puts [string trimright $line]
    }
    puts ""
}
```

{{out}}

```txt
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
MODE DATA
$$ SET exampletext=*
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
$$ MODE TUSCRIPT
SET nix=SPLIT (exampletext,":$:",c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12)
LOOP l1=1,12
SET colum=CONCAT ("c",l1)
SET newcolum=CONCAT ("new",l1)
SET @newcolum="", length=MAX LENGTH (@colum), space=length+2
 LOOP n,l2=@colum
 SET newcell=CENTER (l2,space)
 SET @newcolum=APPEND (@newcolum,"~",newcell)
 ENDLOOP
 SET @newcolum=SPLIT  (@newcolum,":~:")
ENDLOOP
SET exampletext=JOIN(new1,"$",new2,new3,new4,new5,new6,new7,new8,new9,new10,new11,new12)

```

{{out}}

```txt

   Given    $     a      $    text    $  file  $   of   $   many    $   lines,   $  where   $ fields  $ within  $   a    $ line$
    are     $ delineated $     by     $   a    $ single $ 'dollar'  $ character, $  write   $    a    $ program $        $
    that    $   aligns   $    each    $ column $   of   $  fields   $     by     $ ensuring $  that   $  words  $   in   $ each$
   column   $    are     $ separated  $   by   $   at   $   least   $    one     $  space.  $         $         $        $
  Further,  $   allow    $    for     $  each  $  word  $    in     $     a      $  column  $   to    $   be    $ either $ left$
 justified, $   right    $ justified, $   or   $ center $ justified $   within   $   its    $ column. $         $        $


```



## TXR



```txr
@(collect)
@  (coll)@{item /[^$]+/}@(end)
@(end)
@; nc = number of columns
@; pi = padded items (data with row lengths equalized with empty strings)
@; cw = vector of max column widths
@; ce = center padding
@(bind nc @[apply max [mapcar length item]])
@(bind pi @(mapcar (op append @1 (repeat '("") (- nc (length @1)))) item))
@(bind cw @(vector-list
             (mapcar (op apply max [mapcar length @1])
                     ;; matrix transpose trick cols become rows:
                     [apply mapcar [cons list pi]])))
@(bind ns "")
@(output)
@  (repeat)
@    (rep :counter i)@{pi @[cw i]} @(end)
@  (end)
@  (repeat)
@    (rep :counter i)@{pi @(- [cw i])} @(end)
@  (end)
@  (repeat)
@    (rep :counter i)@\
     @{ns @(trunc (- [cw i] (length pi)) 2)}@\
     @{pi @(- [cw i] (trunc (- [cw i] (length pi)) 2))} @(end)
@  (end)
@(end)
```



```txt
$ txr align-columns.txr align-columns.dat
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.
```



## UNIX Shell

This is a draft implementation of the "align columns" problem using Unix shell commands.  The key tool for left and right justified text is the "rs" command.  Centered text is a little more complex, since this is not a feature currently in "rs" (''The centered solution will be added later.'')

```bash

cat <<EOF_OUTER > just-nocenter.sh
#!/bin/sh

td() {
cat <<'EOF'
Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.
EOF
}

rows=$( td | wc -l )

# get the number of fields
fields=$(td | rs -c'$' -g1 -h | awk '{print $2}')

# get the max of the value widths
cwidth=$(td | rs -c'$' -g1 -w1 2>/dev/null | awk 'BEGIN{w=0}{if(length>w){w=length}}END{print w}')

# compute the minimum line width for the columns
lwidth=$(( (1 + cwidth) * fields ))

# left adjusted columns
td | rs -c'$' -g1 -zn -w$lwidth

echo ""

# right adjusted columns
td | rs -c'$' -g1 -znj -w$lwidth

echo ""

exit
EOF_OUTER

```

{{out}}

```sh

$ ./just-nocenter.sh
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

```


The centered output will be added later, when I've more time.  '' I did this in about 10 minutes.''


## Ursala

The algorithm is to lex the text to a list of lists of strings assuming $ as a separator,
then pad the lists out to the length of the maximum length list, transpose,
do the same with each column, and transpose again.
For left justification, nothing further but concatenation is needed.
For right justification, each word's string of trailing blanks is moved to the beginning,
and for center justification, the trailing blanks are divided equally between the beginning and end of each word.

```Ursala
#import std

text =

-[Given$a$text$file$of$many$lines,$where$fields$within$a$line$
are$delineated$by$a$single$'dollar'$character,$write$a$program
that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$
column$are$separated$by$at$least$one$space.
Further,$allow$for$each$word$in$a$column$to$be$either$left$
justified,$right$justified,$or$center$justified$within$its$column.]-

pad = sep`$*; @FS ~&rSSSK7+ (zipp` ^*D\~& leql$^)*rSSK7+ zipp0^*D/leql$^ ~&

just_left   = mat` *+ pad
just_right  = mat` *+ pad; ==` ~-rlT**
just_center = mat` *+ pad; ==` ~-rK30PlrK31PTT**

#show+

main = mat0 <.just_left,just_center,just_right> text
```

{{out}}
<pre style="height:17ex;overflow:scroll">
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.

```



## VBA

Call subroutine "TestSplit" with arguments ''align'' (one of: "left", "right", "center") and ''spacing'' (an integer) between columns.
Both arguments are optional and default to "left" and 1 respectively.


```vb

Public Sub TestSplit(Optional align As String = "left", Optional spacing As Integer = 1)
  Dim word() As String
  Dim colwidth() As Integer
  Dim ncols As Integer
  Dim lines(6) As String
  Dim nlines As Integer

  'check arguments
  If Not (align = "left" Or align = "right" Or align = "center") Then
    MsgBox "TestSplit: wrong argument 'align': " & align
    Exit Sub
  End If
  If spacing < 0 Then
    MsgBox "TestSplit: wrong argument: 'spacing' cannot be negative."
    Exit Sub
  End If

  ' Sample Input (should be from a file)
  nlines = 6
  lines(1) = "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
  lines(2) = "are$delineated$by$a$single$'dollar'$character,$write$a$program"
  lines(3) = "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
  lines(4) = "column$are$separated$by$at$least$one$space."
  lines(5) = "Further,$allow$for$each$word$in$a$column$to$be$either$left$"
  lines(6) = "justified,$right$justified,$or$center$justified$within$its$column."

  'first pass: count columns and column widths
  'the words are not kept in memory
  ncols = -1
  For l = 1 To nlines
    word = Split(RTrim(lines(l)), "$")
    If UBound(word) > ncols Then
      ncols = UBound(word)
      ReDim Preserve colwidth(ncols)
    End If
    For i = 0 To UBound(word)
      If Len(word(i)) > colwidth(i) Then colwidth(i) = Len(word(i))
    Next i
  Next l

  'discard possibly empty columns at the right
  '(this assumes there is at least one non-empty column)
  While colwidth(ncols) = 0
    ncols = ncols - 1
  Wend

  'second pass: print in columns
  For l = 1 To nlines
    word = Split(RTrim(lines(l)), "$")
    For i = 0 To UBound(word)
      a = word(i)
      w = colwidth(i)
      If align = "left" Then
        Debug.Print a + String$(w - Len(a), " ");
      ElseIf align = "right" Then
        Debug.Print String$(w - Len(a), " ") + a;
      ElseIf align = "center" Then
        d = Int((w - Len(a)) / 2)
        Debug.Print String$(d, " ") + a + String$(w - (d + Len(a)), " ");
      End If
      If i < ncols Then Debug.Print Spc(spacing);
    Next i
    Debug.Print
  Next l
End Sub

```


{{out}}

```txt

testsplit , 4 'default alignment, non-default spacing
Given         a             text          file      of        many         lines,        where       fields     within     a         line
are           delineated    by            a         single    'dollar'     character,    write       a          program
that          aligns        each          column    of        fields       by            ensuring    that       words      in        each
column        are           separated     by        at        least        one           space.
Further,      allow         for           each      word      in           a             column      to         be         either    left
justified,    right         justified,    or        center    justified    within        its         column.


testsplit "center" 'non-default alignment, default spacing
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

```



## VBScript

{{trans|Rexx}}

```vb
' Align columns - RC - VBScript
	Const nr=16, nc=16
	ReDim d(nc),t(nr), wor(nr,nc)
	i=i+1: t(i) = "Given$a$text$file$of$many$lines,$where$fields$within$a$line$"
	i=i+1: t(i) = "are$delineated$by$a$single$'dollar'$character,$write$a$program"
	i=i+1: t(i) = "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$"
	i=i+1: t(i) = "column$are$separated$by$at$least$one$space."
	i=i+1: t(i) = "Further,$allow$for$each$word$in$a$column$To$be$either$left$"
	i=i+1: t(i) = "justified,$right$justified,$or$center$justified$within$its$column."
	For r=1 to nr
		If t(r)="" Then Exit For
		w=xRTrim(t(r),"$")
		m=Split(w,"$")
		For c=1 To UBound(m)+1
			wor(r,c)=m(c-1)
			If Len(wor(r,c))>d(c) Then d(c)=Len(wor(r,c))
		Next 'c
		If c>cols Then cols=c
	Next 'r
	rows=r-1
	tt=Array("Left","Right","Center")
	For n=1 To 3
		Wscript.Echo
		Wscript.Echo "*****" & tt(n-1) & "*****"
		For r=1 To rows
			w=""
			For c=1 To cols
				x=wor(r,c): s=Space(d(c))
				Select Case n
					Case 1: w=w &" "& Left   (x & s,d(c))
					Case 2: w=w &" "& Right  (s & x,d(c))
					Case 3: w=w &" "& xCentre(x,d(c)," ")
				End Select 'n
			Next 'c
			Wscript.Echo Mid(w,2)
		Next 'r
	Next 'n

Function xCentre(c, n, Pad)
    Dim j
    If n > Len(c) Then
		j = (n - Len(c)) \  2
		If (n - Len(c)) Mod 2 <> 0 Then j = j + 1
		xCentre = Mid(String(j, Pad) & c & String(j, Pad), 1, n)
    Else
		xCentre = c
    End If
End Function 'xCentre

Function xRTrim(c, Pad)
	Dim i2, l, cc
	cc = "": l = Len(c)
	If l > 0 Then
		i2 = l
		Do While (Mid(c, i2, 1) = Pad And i2 > 1)
			i2 = i2 - 1
		Loop
		If i2 = 1 And Mid(c, i2, 1) = Pad Then i2 = 0
		If i2 > 0 Then cc = Mid(c, 1, i2)
	End If
	xRTrim = cc
End Function 'xRTrim

```

{{out}}

```txt

*****Left*****
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   To      be      either left
justified, right      justified, or     center justified within     its      column.

*****Right*****
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      To      be either left
justified,      right justified,     or center justified     within      its column.

*****Center*****
   Given        a        text     file    of      many     lines,     where   fields  within    a   line
    are    delineated     by        a   single  'dollar' character,   write     a    program
   that      aligns      each    column   of     fields      by     ensuring   that   words    in   each
  column       are     separated   by     at     least       one     space.
 Further,     allow       for     each   word      in         a      column     To      be   either left
justified,    right   justified,   or   center justified   within      its   column.

```



## Vedit macro language

This implementation converts the file currently being edited. The file can then be saved with different filename if required.

```vedit
RS(10, "$")		// Field separator
#11 = 1			// Align: 1 = left, 2 = center, 3 = right

// Reset column widths. Max 50 columns
for (#1=40; #1<90; #1++) { #@1 = 0 }

// Find max width of each column
BOF
Repeat(ALL) {
    for (#1=40; #1<90; #1++) {
        Match(@10, ADVANCE)			// skip field separator if any
	#2 = Cur_Pos
	Search("|{|@(10),|N}", NOERR)		// field separator or end of line
	#3 = Cur_Pos - #2			// width of text
	if (#3 > #@1) { #@1 = #3 }
	if (At_EOL) { Break }
    }
    Line(1, ERRBREAK)
}

// Convert lines
BOF
Repeat(ALL) {
    for (#1=40; #1<90; #1++) {
	#2 = Cur_Pos
	Search("|{|@(10),|N}", NOERR)
	if (At_EOL==0) { Del_Char(Chars_Matched) }
	#3 = #@1 - Cur_Pos + #2			// number of spaces to insert
	#4 = 0
	if (#11 == 2) { #4 = #3/2; #3 -= #4 }	// Center
	if (#11 == 3) { #4 = #3;   #3 = 0 }	// Right justify
	Set_Marker(1, Cur_Pos)
	Goto_Pos(#2)
	Ins_Char(' ', COUNT, #4)		// add spaces before the word
	Goto_Pos(Marker(1))
	Ins_Char(' ', COUNT, #3+1)		// add spaces after the word
	if (At_EOL) { Break }
    }
    Line(1, ERRBREAK)
}
```


{{out}}

```txt
-- Left:
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

-- Center:
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

-- Right:
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.
```



## Visual Basic


```vb
Sub AlignCols(Lines, Optional Align As AlignmentConstants, Optional Sep$ = "$", Optional Sp% = 1)
Dim i&, j&, D&, L&, R&: ReDim W(UBound(Lines)): ReDim C&(0)

  For j = 0 To UBound(W)
    W(j) = Split(Lines(j), Sep)
    If UBound(W(j)) > UBound(C) Then ReDim Preserve C(UBound(W(j)))
    For i = 0 To UBound(W(j)): If Len(W(j)(i)) > C(i) Then C(i) = Len(W(j)(i))
  Next i, j

  For j = 0 To UBound(W): For i = 0 To UBound(W(j))
    D = C(i) - Len(W(j)(i))
    L = Choose(Align + 1, 0, D, D \ 2)
    R = Choose(Align + 1, D, 0, D - L) + Sp
    Debug.Print Space(L); W(j)(i); Space(R); IIf(i < UBound(W(j)), "", vbLf);
  Next i, j
End Sub
```

Usage:
```vb
Sub Main() 'usage of the above
Const Text$ = "Given$a$text$file$of$many$lines,$where$fields$within$a$line$" & vbLf & _
              "are$delineated$by$a$single$'dollar'$character,$write$a$program" & vbLf & _
              "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$" & vbLf & _
              "column$are$separated$by$at$least$one$space." & vbLf & _
              "Further,$allow$for$each$word$in$a$column$to$be$either$left$" & vbLf & _
              "justified,$right$justified,$or$center$justified$within$its$column."

  Debug.Print vbLf; "-- Left:":   AlignCols Split(Text, vbLf), vbLeftJustify
  Debug.Print vbLf; "-- Center:": AlignCols Split(Text, vbLf), vbCenter
  Debug.Print vbLf; "-- Right:":  AlignCols Split(Text, vbLf), vbRightJustify
End Sub
```

{{out}}

```txt
-- Left:
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

-- Center:
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

-- Right:
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.
```



## Visual Basic .NET


```vb
Module Module1
    Private Delegate Function Justification(s As String, width As Integer) As String

    Private Function AlignColumns(lines As String(), justification As Justification) As String()
        Const Separator As Char = "$"c
        ' build input container table and calculate columns count
        Dim containerTbl As String()() = New String(lines.Length - 1)() {}
        Dim columns As Integer = 0
        For i As Integer = 0 To lines.Length - 1
            Dim row As String() = lines(i).TrimEnd(Separator).Split(Separator)
            If columns < row.Length Then
                columns = row.Length
            End If
            containerTbl(i) = row
        Next
        ' create formatted container table
        Dim formattedTable As String()() = New String(containerTbl.Length - 1)() {}
        For i As Integer = 0 To formattedTable.Length - 1
            formattedTable(i) = New String(columns - 1) {}
        Next
        For j As Integer = 0 To columns - 1
            ' get max column width
            Dim columnWidth As Integer = 0
            For i As Integer = 0 To containerTbl.Length - 1
                If j < containerTbl(i).Length AndAlso columnWidth < containerTbl(i)(j).Length Then
                    columnWidth = containerTbl(i)(j).Length
                End If
            Next
            ' justify column cells
            For i As Integer = 0 To formattedTable.Length - 1
                If j < containerTbl(i).Length Then
                    formattedTable(i)(j) = justification(containerTbl(i)(j), columnWidth)
                Else
                    formattedTable(i)(j) = New [String](" "c, columnWidth)
                End If
            Next
        Next
        ' create result
        Dim result As String() = New String(formattedTable.Length - 1) {}
        For i As Integer = 0 To result.Length - 1
            result(i) = [String].Join(" ", formattedTable(i))
        Next
        Return result
    End Function

    Private Function JustifyLeft(s As String, width As Integer) As String
        Return s.PadRight(width)
    End Function
    Private Function JustifyRight(s As String, width As Integer) As String
        Return s.PadLeft(width)
    End Function
    Private Function JustifyCenter(s As String, width As Integer) As String
        Return s.PadLeft((width + s.Length) / 2).PadRight(width)
    End Function

    Sub Main()
        Dim input As String() = {"Given$a$text$file$of$many$lines,$where$fields$within$a$line$", "are$delineated$by$a$single$'dollar'$character,$write$a$program", "that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$", "column$are$separated$by$at$least$one$space.", "Further,$allow$for$each$word$in$a$column$to$be$either$left$", "justified,$right$justified,$or$center$justified$within$its$column."}

        For Each line As String In AlignColumns(input, AddressOf JustifyLeft)
            Console.WriteLine(line)
        Next
        Console.ReadLine()
    End Sub

End Module
```


{{out}}

```txt
-- Left:
Given      a          text       file   of     many      lines,     where    fields  within  a      line
are        delineated by         a      single 'dollar'  character, write    a       program
that       aligns     each       column of     fields    by         ensuring that    words   in     each
column     are        separated  by     at     least     one        space.
Further,   allow      for        each   word   in        a          column   to      be      either left
justified, right      justified, or     center justified within     its      column.

-- Center:
  Given        a         text     file    of     many      lines,    where   fields  within    a    line
   are     delineated     by       a    single 'dollar'  character,  write      a    program
   that      aligns      each    column   of    fields       by     ensuring  that    words    in   each
  column      are     separated    by     at     least      one      space.
 Further,    allow       for      each   word     in         a       column    to      be    either left
justified,   right    justified,   or   center justified   within     its    column.

-- Right:
     Given          a       text   file     of      many     lines,    where  fields  within      a line
       are delineated         by      a single  'dollar' character,    write       a program
      that     aligns       each column     of    fields         by ensuring    that   words     in each
    column        are  separated     by     at     least        one   space.
  Further,      allow        for   each   word        in          a   column      to      be either left
justified,      right justified,     or center justified     within      its column.
```



## zkl


```zkl
fcn format(text,how){
   words:=text.split("$").apply("split").flatten();
   max:=words.reduce(fcn(p,n){ n=n.len(); n>p and n or p },0);
   wordsPerCol:=80/(max+1);
   fmt:=(switch(how){
      case(-1){ "%%-%ds ".fmt(max).fmt }
      case(0) { fcn(max,w){
            a:=(max-w.len())/2; b:=max-w.len() - a; String(" "*a,w," "*b);
         }.fp(max)
      }
      case(1){ "%%%ds ".fmt(max).fmt }
   });
   w:=words.walker(); d:=Data(0,Int);
   do{ w.pump(wordsPerCol,d,fmt).append("\n") } while(not w.atEnd);
   d.text;
}
```


```zkl
text:=
"Given$a$text$file$of$many$lines,$where$fields$within$a$line$\n"
"are$delineated$by$a$single$'dollar'$character,$write$a$program\n"
"that$aligns$each$column$of$fields$by$ensuring$that$words$in$each$\n"
"column$are$separated$by$at$least$one$space.\n"
"Further,$allow$for$each$word$in$a$column$to$be$either$left$\n"
"justified,$right$justified,$or$center$justified$within$its$column.\n";

format(text,-1).print();
format(text, 0).print();
format(text, 1).print();
```

Blow apart the text into a list of words, find max len of any word, calc how many words will fit on a 80 col line, format all words into a bit bucket line at a time. Formatting is "%-ms" or "%ms" for left & right justify (m is field width), calculated for center. fmt is the string format method or center calc function, depending. Where string formatting can be used, it would be better to format all words in a line in one go but the code would be longer.
{{out}}
<pre  style="height:20ex;overflow:scroll">
Given      a          text       file       of         many       lines,
where      fields     within     a          line       are        delineated
by         a          single     'dollar'   character, write      a
program    that       aligns     each       column     of         fields
by         ensuring   that       words      in         each       column
are        separated  by         at         least      one        space.
Further,   allow      for        each       word       in         a
column     to         be         either     left       justified, right
justified, or         center     justified  within     its        column.

  Given       a        text      file       of       many     lines,
  where     fields    within      a        line      are    delineated
    by        a       single   'dollar' character,  write       a
 program     that     aligns     each     column      of      fields
    by     ensuring    that     words       in       each     column
   are    separated     by        at      least      one      space.
 Further,   allow      for       each      word       in        a
  column      to        be      either     left   justified,  right
justified,    or      center  justified   within     its     column.

     Given          a       text       file         of       many     lines,
     where     fields     within          a       line        are delineated
        by          a     single   'dollar' character,      write          a
   program       that     aligns       each     column         of     fields
        by   ensuring       that      words         in       each     column
       are  separated         by         at      least        one     space.
  Further,      allow        for       each       word         in          a
    column         to         be     either       left justified,      right
justified,         or     center  justified     within        its    column.

```



## ZX Spectrum Basic

The max width (without 'hack') of ZX Spectrum screen is 32 characters. The text sample is adapted for this feature.

```zxbasic
 5 BORDER 2
10 DATA 6
20 DATA "The$problem$of$Speccy$"
30 DATA "is$the$screen.$"
40 DATA "Need$adapt$text$sample$"
50 DATA "for$show$the$result$"
60 DATA "without$problem$,right?$"
70 DATA "But$see$the$code.$"
80 REM First find the maximum length of a 'word'
90 LET max=0: LET d$="$"
100 READ nlines
110 FOR l=1 TO nlines
120 READ t$
130 GO SUB 1000
150 NEXT l
155 LET s$="                                "( TO max)
160 REM Now display the aligned text:
170 LET m$="l": GO SUB 2000: PRINT
180 LET m$="r": GO SUB 2000: PRINT
190 LET m$="c": GO SUB 2000
200 STOP
1000 REM Maximum length of a word
1010 LET lt=LEN t$: LET p=1: LET lw=0
1020 FOR i=1 TO lt
1030 IF t$(i)=d$ THEN LET lw=i-p: LET p=i: IF lw>max THEN LET max=lw
1040 NEXT i
1050 RETURN
2000 REM Show aligned text
2010 RESTORE 20
2020 FOR l=1 TO nlines
2030 READ t$
2040 GO SUB 3000
2050 NEXT l
2060 RETURN
3000 REM Show words
3010 LET lt=LEN t$: LET p=1: LET lw=0
3020 FOR i=1 TO lt
3030 IF t$(i)<>d$ THEN GO TO 3090
3035 LET lw=i-p
3040 LET p$=t$(p TO i-1): LET p=i+1: LET z$=s$
3050 IF m$="l" THEN LET z$( TO lw)=p$
3060 IF m$="r" THEN LET z$(max-lw+1 TO )=p$
3070 IF m$="c" THEN LET z$((max/2)-(lw/2) TO )=p$
3080 PRINT z$;
3090 NEXT i
3095 PRINT
3100 RETURN
```


{{out}}Left alignement example:

```txt
The     problem of      Speccy
is      the     screen.
Need    adapt   text    sample
for     show    the     result
without problem ,right?
But     see     the     code.
```


{{omit from|PARI/GP|No real capacity for string manipulation}}
