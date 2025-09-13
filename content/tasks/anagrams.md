+++
title = "Anagrams"
description = ""
date = 2019-08-26T17:35:59Z
aliases = []
[extra]
id = 3052
[taxonomies]
categories = ["Text processing", "task"]
tags = []
+++

## Task

When two or more words are composed of the same characters, but in a different order, they are called [anagrams](https://en.wikipedia.org/wiki/Anagram).

Using the word list at   http://wiki.puzzlers.org/pub/wordlists/unixdict.txt,

find the sets of words that share the same characters that contain the most words in them.

{{task heading|Related tasks}}

{{Related tasks/Word plays}}

<hr>


## 11l

{{trans|Python}}

```11l
DefaultDict[String, Array[String]] anagram
L(word) File(‘unixdict.txt’).read().split("\n")
   anagram[sorted(word).join(‘’)].append(word)

V count = max(anagram.values().map(ana -> ana.len))

L(ana) anagram.values()
   I ana.len == count
      print(ana)
```

{{out}}

```txt

[abel, able, bale, bela, elba]
[caret, carte, cater, crate, trace]
[angel, angle, galen, glean, lange]
[alger, glare, lager, large, regal]
[elan, lane, lean, lena, neal]
[evil, levi, live, veil, vile]

```



## 8th


```8th

\
\ anagrams.8th
\ Rosetta Code - Anagrams problem
\ Using the word list at:
\   http://www.puzzlers.org/pub/wordlists/unixdict.txt,
\ find the sets of words that share the same characters
\ that contain the most words in them.
\

ns: anagrams

m:new var, anamap
a:new var, anaptr
0 var, analen

\ sort a string
: s:sort \ s -- s                             \
  null s:/                                    \ a
  ' s:cmpi a:sort                             \ a
  "" a:join                                   \ s
  ;

: process-words \ word --                     \ word
  s:lc                                        \ word
  dup                                         \ word word
  >r                                          \ word | word
  \ 1. we create a sorted version of the curret word (sword)
  s:sort                                      \ sword | word
  \ We check if sword can be found in map anamap
  anamap @                                    \ sword anamap | word
  over                                        \ sword anamap sword | word
  m:exists?                                   \ sword anamap boolean | word
  if                                          \ sword anamap | word
  \ If sword already exists in anamap:
  \    - get mapvalue, which is an array
  \    - add the original word to that array
  \    - store the array in the map with key sword
    over                                      \ sword anamap sword | word
    m:@                                       \ sword anamap array | word
    r>                                        \ sword anamap array word
    a:push                                    \ sword anamap array
    rot                                       \ anamap array sword
    swap                                      \ anamap sword array
    m:!                                       \ anamap
  else                                        \ sword anamap | word
  \ If sword does not yet exist in anamap:
  \    - create empty array
  \    - put the original word into that array
  \    - store the array in the map with key sword
    swap                                      \ anamap sword | word
    a:new                                     \ anamap sword array | word
    r>                                        \ anamap sword array word
    a:push                                    \ anamap sword array
    m:!                                       \ anamap
  then
  drop                                        \
  ;

\ Read and check all words in array analist
: read-and-check-words \ --                   \
  "analist.txt"                               \ fname
  f:open-ro                                   \ f
  ' process-words f:eachline                  \ f
  f:close                                     \
  ;

: len< \ key array arraylen --                \ key array arraylen
  2drop                                       \ key
  drop                                        \
  ;

: len= \ key array arraylen --                \ key array arraylen
  2drop                                       \ key
  anaptr @                                    \ key anaptr
  swap                                        \ anaptr key
  a:push                                      \ anaptr
  drop                                        \
  ;

: len> \ key array arraylen --                \ key array arraylen
  analen                                      \ key array arraylen analen
  !                                           \ key array
  drop                                        \ key
  anaptr @                                    \ key anaptr
  a:clear                                     \ key anaptr
  swap                                        \ anaptr key
  a:push                                      \ anaptr
  drop                                        \
  ;

: fetch-longest-list \ key array --           \ key array
  a:len                                       \ key array arraylen
  analen @                                    \ key array arraylen analen
  2dup                                        \ key array arraylen analen arraylen analen
  n:cmp                                       \ key array arraylen analen value
  1 n:+                                       \ key array arraylen analen value
  nip                                         \ key array arraylen value
  [ ' len< , ' len= , ' len> ]                \ key array arraylen value swarr
  swap                                        \ key array arraylen swarr value
  caseof                                      \
  ;

: list-words-1 \ ix value --                  \ ix value
  nip                                         \ value
  "\t" . .                                    \
  ;

: list-words \ ix value --                    \ ix value
  nip                                         \ value
  anamap @                                    \ value anamap
  swap                                        \ anamap value
  m:@                                         \ anamap array
  nip                                         \ array
  ' list-words-1 a:each                       \ array
  cr                                          \ array
  drop                                        \
  ;

: app:main

  \ Create a map, where the values are arrays, containing all words
  \ which are the same when sorted (sword); sword is used as key
  read-and-check-words

  \ Create an array that holds the keys for anamap, for which the value,
  \ which is the array of anagrams, has the biggest length found.
  anamap @ ' fetch-longest-list m:each

  \ Dump the resulting words to the console
  anaptr @ ' list-words a:each drop
  bye
  ;

```



## ABAP


```ABAP
report zz_anagrams no standard page heading.
define update_progress.
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      text = &1.
end-of-definition.

" Selection screen segment allowing the person to choose which file will act as input.
selection-screen begin of block file_choice.
  parameters p_file type string lower case.
selection-screen end of block file_choice.

" When the user requests help with input, run the routine to allow them to navigate the presentation server.
at selection-screen on value-request for p_file.
  perform getfile using p_file.

at selection-screen output.
  %_p_file_%_app_%-text = 'Input File: '.

start-of-selection.
  data: gt_data type table of string.

  " Read the specified file from the presentation server into memory.
  perform readfile using p_file changing gt_data.
  " After the file has been read into memory, loop through it line-by-line and make anagrams.
  perform anagrams using gt_data.

" Subroutine for generating a list of anagrams.
" The supplied input is a table, with each entry corresponding to a word.
form anagrams using it_data like gt_data.
  types begin of ty_map.
    types key type string.
    types value type string.
  types end of ty_map.

  data: lv_char     type c,
        lv_len      type i,
        lv_string   type string,
        ls_entry    type ty_map,
        lt_anagrams type standard table of ty_map,
        lt_c_tab    type table of string.

  field-symbols: <fs_raw> type string.
  " Loop through each word in the table, and make an associative array.
  loop at gt_data assigning <fs_raw>.
    " First, we need to re-order the word alphabetically. This generated a key. All anagrams will use this same key.
    " Add each character to a table, which we will then sort alphabetically.
    lv_len = strlen( <fs_raw> ).
    refresh lt_c_tab.
    do lv_len times.
      lv_len = sy-index  - 1.
      append <fs_raw>+lv_len(1) to lt_c_tab.
    enddo.
    sort lt_c_tab as text.
    " Now append the characters to a string and add it as a key into the map.
    clear lv_string.
    loop at lt_c_tab into lv_char.
      concatenate lv_char lv_string into lv_string respecting blanks.
    endloop.
    ls_entry-key = lv_string.
    ls_entry-value = <fs_raw>.
    append ls_entry to lt_anagrams.
  endloop.
  " After we're done processing, output a list of the anagrams.
  clear lv_string.
  loop at lt_anagrams into ls_entry.
    " Is it part of the same key --> Output in the same line, else a new entry.
    if lv_string = ls_entry-key.
        write: ', ', ls_entry-value.
    else.
      if sy-tabix <> 1.
        write: ']'.
      endif.
      write:  / '[', ls_entry-value.
    endif.
    lv_string = ls_entry-key.
  endloop.
  " Close last entry.
  write ']'.
endform.

" Read a specified file from the presentation server.
form readfile using i_file type string changing it_raw like gt_data.
  data: l_datat type string,
        l_msg(2048),
        l_lines(10).

  " Read the file into memory.
  update_progress 'Reading file...'.
  call method cl_gui_frontend_services=>gui_upload
    exporting
      filename = i_file
    changing
      data_tab = it_raw
    exceptions
      others   = 1.
  " Output error if the file could not be uploaded.
  if sy-subrc <> 0.
    write : / 'Error reading the supplied file!'.
    return.
  endif.
endform.
```

{{out}}

```txt
[ angel ,  angle ,  galen ,  glean ,  lange ]
[ elan ,  lane ,  lean ,  lena ,  neal ]
[ alger ,  glare ,  lager ,  large ,  regal ]
[ abel ,  able ,  bale ,  bela ,  elba ]
[ evil ,  levi ,  live ,  veil ,  vile ]
[ caret ,  carte ,  cater ,  crate ,  trace ]
```



## Ada


```ada
with Ada.Text_IO;  use Ada.Text_IO;

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Containers.Indefinite_Ordered_Sets;

procedure Words_Of_Equal_Characters is
   package Set_Of_Words is new Ada.Containers.Indefinite_Ordered_Sets (String);
   use Ada.Containers, Set_Of_Words;
   package Anagrams is new Ada.Containers.Indefinite_Ordered_Maps (String, Set);
   use Anagrams;

   File   : File_Type;
   Result : Map;
   Max    : Count_Type := 1;

   procedure Put (Position : Anagrams.Cursor) is
      First : Boolean := True;
      List  : Set renames Element (Position);
      procedure Put (Position : Set_Of_Words.Cursor) is
      begin
         if First then
            First := False;
         else
            Put (',');
         end if;
         Put (Element (Position));
      end Put;
   begin
      if List.Length = Max then
         Iterate (List, Put'Access);
         New_Line;
      end if;
   end Put;

begin
   Open (File, In_File, "unixdict.txt");
   loop
      declare
         Word : constant String     := Get_Line (File);
         Key  : String (Word'Range) := (others => Character'Last);
         List : Set;
         Position : Anagrams.Cursor;
      begin
         for I in Word'Range loop
            for J in Word'Range loop
               if Key (J) > Word (I) then
                  Key (J + 1..I) := Key (J..I - 1);
                  Key (J) := Word (I);
                  exit;
               end if;
            end loop;
         end loop;
         Position := Find (Result, Key);
         if Has_Element (Position) then
            List := Element (Position);
            Insert (List, Word);
            Replace_Element (Result, Position, List);
         else
            Insert (List, Word);
            Include (Result, Key, List);
         end if;
         Max := Count_Type'Max (Max, Length (List));
      end;
   end loop;
exception
   when End_Error =>
      Iterate (Result, Put'Access);
      Close (File);
end Words_Of_Equal_Characters;
```

{{out}}

```txt

abel,able,bale,bela,elba
caret,carte,cater,crate,trace
angel,angle,galen,glean,lange
alger,glare,lager,large,regal
elan,lane,lean,lena,neal
evil,levi,live,veil,vile

```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}} Uses the "read" PRAGMA of Algol 68 G to include the associative array code from the [[Associative_array/Iteration]] task.

```algol68
# find longest list(s) of words that are anagrams in a list of words #
# use the associative array in the Associate array/iteration task    #
PR read "aArray.a68" PR

# returns the number of occurances of ch in text #
PROC count = ( STRING text, CHAR ch )INT:
     BEGIN
         INT result := 0;
         FOR c FROM LWB text TO UPB text DO IF text[ c ] = ch THEN result +:= 1 FI OD;
         result
     END # count # ;

# returns text with the characters sorted into ascending order #
PROC char sort = ( STRING text )STRING:
     BEGIN
         STRING sorted := text;
         FOR end pos FROM UPB sorted - 1 BY -1 TO LWB sorted
         WHILE
             BOOL swapped := FALSE;
             FOR pos FROM LWB sorted TO end pos DO
                 IF sorted[ pos ] > sorted[ pos + 1 ]
                 THEN
                     CHAR  t           := sorted[ pos     ];
                     sorted[ pos     ] := sorted[ pos + 1 ];
                     sorted[ pos + 1 ] := t;
                     swapped           := TRUE
                 FI
             OD;
             swapped
         DO SKIP OD;
         sorted
    END # char sort # ;

# read the list of words and store in an associative array           #

CHAR separator = "|"; # character that will separate the anagrams #

IF  FILE input file;
    STRING file name = "unixdict.txt";
    open( input file, file name, stand in channel ) /= 0
THEN
    # failed to open the file #
    print( (  "Unable to open """ + file name + """", newline ) )
ELSE
    # file opened OK #
    BOOL at eof := FALSE;
    # set the EOF handler for the file #
    on logical file end( input file, ( REF FILE f )BOOL:
                                     BEGIN
                                         # note that we reached EOF on the #
                                         # latest read #
                                         at eof := TRUE;
                                         # return TRUE so processing can continue #
                                         TRUE
                                     END
                       );
    REF AARRAY words := INIT LOC AARRAY;
    STRING word;
    WHILE NOT at eof
    DO
        STRING word;
        get( input file, ( word, newline ) );
        words // char sort( word ) +:= separator + word
    OD;
    # close the file #
    close( input file );

    # find the maximum number of anagrams #

    INT max anagrams := 0;

    REF AAELEMENT e := FIRST words;
    WHILE e ISNT nil element DO
        IF  INT anagrams := count( value OF e, separator );
            anagrams > max anagrams
        THEN
            max anagrams := anagrams
        FI;
        e := NEXT words
    OD;

    print( ( "Maximum number of anagrams: ", whole( max anagrams, -4 ), newline ) );
    # show the anagrams with the maximum number #
    e := FIRST words;
    WHILE e ISNT nil element DO
        IF  INT anagrams := count( value OF e, separator );
            anagrams = max anagrams
        THEN
            print( ( ( value OF e )[ ( LWB value OF e ) + 1: ], newline ) )
        FI;
        e := NEXT words
    OD
FI
```

{{out}}

```txt

Maximum number of anagrams:    5
abel|able|bale|bela|elba
elan|lane|lean|lena|neal
evil|levi|live|veil|vile
angel|angle|galen|glean|lange
alger|glare|lager|large|regal
caret|carte|cater|crate|trace

```





## APL

{{trans|J}}
{{works with|Dyalog APL}}
This is a rough translation of the J version, intermediate values are kept and verb trains are not used for clarity of data flow.


```APL

anagrams←{
    tie←⍵ ⎕NTIE 0
    dict←⎕NREAD tie 80(⎕NSIZE tie)0
    boxes←((⎕UCS 10)≠dict)⊆dict
    ana←(({⍵[⍋⍵]}¨boxes)({⍵}⌸)boxes)
    ({~' '∊¨(⊃/¯1↑[2]⍵)}ana)⌿ana ⋄ ⎕NUNTIE
}

```

On a unix system we can assume wget exists and can use it from dyalog to download the file.

The ]display command formats the output with boxes.

'''Example:'''

```APL

⎕SH'wget http://www.puzzlers.org/pub/wordlists/unixdict.txt'
]display anagrams 'unixdict.txt'

```

'''Output:'''

```txt

┌→────────────────────────────────────────┐
↓ ┌→───┐  ┌→───┐  ┌→───┐  ┌→───┐  ┌→───┐  │
│ │abel│  │able│  │bale│  │bela│  │elba│  │
│ └────┘  └────┘  └────┘  └────┘  └────┘  │
│ ┌→────┐ ┌→────┐ ┌→────┐ ┌→────┐ ┌→────┐ │
│ │alger│ │glare│ │lager│ │large│ │regal│ │
│ └─────┘ └─────┘ └─────┘ └─────┘ └─────┘ │
│ ┌→────┐ ┌→────┐ ┌→────┐ ┌→────┐ ┌→────┐ │
│ │angel│ │angle│ │galen│ │glean│ │lange│ │
│ └─────┘ └─────┘ └─────┘ └─────┘ └─────┘ │
│ ┌→────┐ ┌→────┐ ┌→────┐ ┌→────┐ ┌→────┐ │
│ │caret│ │carte│ │cater│ │crate│ │trace│ │
│ └─────┘ └─────┘ └─────┘ └─────┘ └─────┘ │
│ ┌→───┐  ┌→───┐  ┌→───┐  ┌→───┐  ┌→───┐  │
│ │elan│  │lane│  │lean│  │lena│  │neal│  │
│ └────┘  └────┘  └────┘  └────┘  └────┘  │
│ ┌→───┐  ┌→───┐  ┌→───┐  ┌→───┐  ┌→───┐  │
│ │evil│  │levi│  │live│  │veil│  │vile│  │
│ └────┘  └────┘  └────┘  └────┘  └────┘  │
└∊────────────────────────────────────────┘

```



## AutoHotkey

Following code should work for AHK 1.0.* and 1.1* versions:

```AutoHotkey
FileRead, Contents, unixdict.txt
Loop, Parse, Contents, % "`n", % "`r"
{ ; parsing each line of the file we just read
    Loop, Parse, A_LoopField ; parsing each letter/character of the current word
        Dummy .= "," A_LoopField
    Sort, Dummy, % "D," ; sorting those letters before removing the delimiters (comma)
    StringReplace, Dummy, Dummy, % ",", % "", All
    List .= "`n" Dummy " " A_LoopField , Dummy := ""
} ; at this point, we have a list where each line looks like <LETTERS><SPACE><WORD>
Count := 0, Contents := "", List := SubStr(List,2)
Sort, List
Loop, Parse, List, % "`n", % "`r"
{ ; now the list is sorted, parse it counting the consecutive lines with the same set of <LETTERS>
    Max := (Count > Max) ? Count : Max
    StringSplit, LinePart, A_LoopField, % " " ; (LinePart1 are the letters, LinePart2 is the word)
    If ( PreviousLinePart1 = LinePart1 )
        Count++ , WordList .= "," LinePart2
    Else
        var_Result .= ( Count <> Max ) ? "" ; don't append if the number of common words is too low
        : "`n" Count "`t" PreviousLinePart1 "`t" SubStr(WordList,2)
        , WordList := "" , Count := 0
    PreviousLinePart1 := LinePart1
}
List := "", var_Result := SubStr(var_Result,2)
Sort, var_Result, R N ; make the higher scores appear first
Loop, Parse, var_Result, % "`n", % "`r"
    If ( 1 == InStr(A_LoopField,Max) )
        var_Output .= "`n" A_LoopField
    Else ; output only those sets of letters that scored the maximum amount of common words
        Break
MsgBox, % ClipBoard := SubStr(var_Output,2) ; the result is also copied to the clipboard
```

{{out}}

```txt

4	aeln	lane,lean,lena,neal
4	aeglr	glare,lager,large,regal
4	aegln	angle,galen,glean,lange
4	acert	carte,cater,crate,trace
4	abel	able,bale,bela,elba
4	eilv	levi,live,veil,vile

```



## AWK


```AWK
# JUMBLEA.AWK - words with the most duplicate spellings
# syntax: GAWK -f JUMBLEA.AWK UNIXDICT.TXT
{   for (i=1; i<=NF; i++) {
      w = sortstr(toupper($i))
      arr[w] = arr[w] $i " "
      n = gsub(/ /,"&",arr[w])
      if (max_n < n) { max_n = n }
    }
}
END {
    for (w in arr) {
      if (gsub(/ /,"&",arr[w]) == max_n) {
        printf("%s\t%s\n",w,arr[w])
      }
    }
    exit(0)
}
function sortstr(str,  i,j,leng) {
    leng = length(str)
    for (i=2; i<=leng; i++) {
      for (j=i; j>1 && substr(str,j-1,1) > substr(str,j,1); j--) {
        str = substr(str,1,j-2) substr(str,j,1) substr(str,j-1,1) substr(str,j+1)
      }
    }
    return(str)
}
```

{{out}}

```txt

ABEL    abel able bale bela elba
ACERT   caret carte cater crate trace
AEGLN   angel angle galen glean lange
AEGLR   alger glare lager large regal
AELN    elan lane lean lena neal
EILV    evil levi live veil vile

```


Alternatively, non-POSIX version:
{{works with|gawk}}

```awk
#!/bin/gawk -f

{   patsplit($0, chars, ".")
    asort(chars)
    sorted = ""
    for (i = 1; i <= length(chars); i++)
	sorted = sorted chars[i]

    if (++count[sorted] > countMax) countMax++
    accum[sorted] = accum[sorted] " " $0
}

END {
    for (i in accum)
	if (count[i] == countMax)
	    print substr(accum[i], 2)
}
```



## BaCon


```freebasic
OPTION COLLAPSE TRUE

DECLARE idx$ ASSOC STRING

FOR w$ IN LOAD$("unixdict.txt") STEP NL$

    set$ = SORT$(EXPLODE$(w$, 1))

    idx$(set$) = APPEND$(idx$(set$), 0, w$)
    total = AMOUNT(idx$(set$))

    IF MaxCount < total THEN MaxCount = total
NEXT

PRINT "Analyzing took ", TIMER, " msecs.", NL$

LOOKUP idx$ TO n$ SIZE x
FOR y = 0 TO x-1
    IF MaxCount = AMOUNT(idx$(n$[y])) THEN PRINT n$[y], ": ", idx$(n$[y])
NEXT
```

{{out}}

```txt

Analyzing took 35 msecs.

a b e l: abel able bale bela elba
a e g l r: alger glare lager large regal
a e g l n: angel angle galen glean lange
a c e r t: caret carte cater crate trace
a e l n: elan lane lean lena neal
e i l v: evil levi live veil vile

```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      INSTALL @lib$+"SORTLIB"
      sort% = FN_sortinit(0,0)

      REM Count number of words in dictionary:
      nwords% = 0
      dict% = OPENIN("unixdict.txt")
      WHILE NOT EOF#dict%
        word$ = GET$#dict%
        nwords% += 1
      ENDWHILE
      CLOSE #dict%

      REM Create arrays big enough to contain the dictionary:
      DIM dict$(nwords%), sort$(nwords%)

      REM Load the dictionary and sort the characters in the words:
      dict% = OPENIN("unixdict.txt")
      FOR word% = 1 TO nwords%
        word$ = GET$#dict%
        dict$(word%) = word$
        sort$(word%) = FNsortchars(word$)
      NEXT word%
      CLOSE #dict%

      REM Sort arrays using the 'sorted character' words as a key:
      C% = nwords%
      CALL sort%, sort$(1), dict$(1)

      REM Count the longest sets of anagrams:
      max% = 0
      set% = 1
      FOR word% = 1 TO nwords%-1
        IF sort$(word%) = sort$(word%+1) THEN
          set% += 1
        ELSE
          IF set% > max% THEN max% = set%
          set% = 1
        ENDIF
      NEXT word%

      REM Output the results:
      set% = 1
      FOR word% = 1 TO nwords%-1
        IF sort$(word%) = sort$(word%+1) THEN
          set% += 1
        ELSE
          IF set% = max% THEN
            FOR anagram% = word%-max%+1 TO word%
              PRINT dict$(anagram%),;
            NEXT
            PRINT
          ENDIF
          set% = 1
        ENDIF
      NEXT word%
      END

      DEF FNsortchars(word$)
      LOCAL C%, char&()
      DIM char&(LEN(word$))
      $$^char&(0) = word$
      C% = LEN(word$)
      CALL sort%, char&(0)
      = $$^char&(0)
```

{{out}}

```txt

abel      able      bale      bela      elba
caret     carte     cater     crate     trace
angel     angle     galen     glean     lange
alger     glare     lager     large     regal
elan      lane      lean      lena      neal
evil      levi      live      veil      vile

```



## Bracmat


This solution makes extensive use of  Bracmat's computer algebra mechanisms. A trick is needed to handle words that are merely repetitions of a single letter, such as <code>iii</code>. That's why the variabe <code>sum</code> isn't initialised with <code>0</code>, but with a non-number, in this case the empty string. Also te correct handling of characters 0-9 needs a trick so that they are not numerically added: they are prepended with a non-digit, an <code>N</code> in this case. After completely traversing the word list, the program writes a file <code>product.txt</code> that can be visually inspected.
The program is not fast. (Minutes rather than seconds.)

```bracmat
( get$("unixdict.txt",STR):?list
& 1:?product
&   whl
  ' ( @(!list:(%?word:?w) \n ?list)
    & :?sum
    &   whl
      ' ( @(!w:%?let ?w)
        & (!let:~#|str$(N !let))+!sum:?sum
        )
    & !sum^!word*!product:?product
    )
& lst$(product,"product.txt",NEW)
& 0:?max
& :?group
& (   !product
    :   ?
      * ?^(%+%:?exp)
      * ( ?
        &   !exp
          :   ?
            + ( [>!max:[?max&!exp:?group
              | [~<!max&!group !exp:?group
              )
        & ~
        )
  | out$!group
  )
);
```

{{out}}

```txt
  abel+able+bale+bela+elba
  caret+carte+cater+crate+trace
  angel+angle+galen+glean+lange
  alger+glare+lager+large+regal
  elan+lane+lean+lena+neal
  evil+levi+live+veil+vile
```



## C


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

char *sortedWord(const char *word, char *wbuf)
{
    char *p1, *p2, *endwrd;
    char t;
    int swaps;

    strcpy(wbuf, word);
    endwrd = wbuf+strlen(wbuf);
    do {
       swaps = 0;
       p1 = wbuf; p2 = endwrd-1;
       while (p1<p2) {
          if (*p2 > *p1) {
             t = *p2; *p2 = *p1; *p1 = t;
             swaps = 1;
          }
          p1++; p2--;
       }
       p1 = wbuf; p2 = p1+1;
       while(p2 < endwrd) {
           if (*p2 > *p1) {
             t = *p2; *p2 = *p1; *p1 = t;
             swaps = 1;
           }
           p1++; p2++;
       }
    } while (swaps);
    return wbuf;
}

static
short cxmap[] = {
    0x06, 0x1f, 0x4d, 0x0c, 0x5c, 0x28, 0x5d, 0x0e, 0x09, 0x33, 0x31, 0x56,
    0x52, 0x19, 0x29, 0x53, 0x32, 0x48, 0x35, 0x55, 0x5e, 0x14, 0x27, 0x24,
    0x02, 0x3e, 0x18, 0x4a, 0x3f, 0x4c, 0x45, 0x30, 0x08, 0x2c, 0x1a, 0x03,
    0x0b, 0x0d, 0x4f, 0x07, 0x20, 0x1d, 0x51, 0x3b, 0x11, 0x58, 0x00, 0x49,
    0x15, 0x2d, 0x41, 0x17, 0x5f, 0x39, 0x16, 0x42, 0x37, 0x22, 0x1c, 0x0f,
    0x43, 0x5b, 0x46, 0x4b, 0x0a, 0x26, 0x2e, 0x40, 0x12, 0x21, 0x3c, 0x36,
    0x38, 0x1e, 0x01, 0x1b, 0x05, 0x4e, 0x44, 0x3d, 0x04, 0x10, 0x5a, 0x2a,
    0x23, 0x34, 0x25, 0x2f, 0x2b, 0x50, 0x3a, 0x54, 0x47, 0x59, 0x13, 0x57,
   };
#define CXMAP_SIZE (sizeof(cxmap)/sizeof(short))


int Str_Hash( const char *key, int ix_max )
{
   const char *cp;
   short mash;
   int  hash = 33501551;
   for (cp = key; *cp; cp++) {
      mash = cxmap[*cp % CXMAP_SIZE];
      hash = (hash >>4) ^ 0x5C5CF5C ^ ((hash<<1) + (mash<<5));
      hash &= 0x3FFFFFFF;
      }
   return  hash % ix_max;
}

typedef struct sDictWord  *DictWord;
struct sDictWord {
    const char *word;
    DictWord next;
};

typedef struct sHashEntry *HashEntry;
struct sHashEntry {
    const char *key;
    HashEntry next;
    DictWord  words;
    HashEntry link;
    short wordCount;
};

#define HT_SIZE 8192

HashEntry hashTable[HT_SIZE];

HashEntry mostPerms = NULL;

int buildAnagrams( FILE *fin )
{
    char buffer[40];
    char bufr2[40];
    char *hkey;
    int hix;
    HashEntry he, *hep;
    DictWord  we;
    int  maxPC = 2;
    int numWords = 0;

    while ( fgets(buffer, 40, fin)) {
        for(hkey = buffer; *hkey && (*hkey!='\n'); hkey++);
        *hkey = 0;
        hkey = sortedWord(buffer, bufr2);
        hix = Str_Hash(hkey, HT_SIZE);
        he = hashTable[hix]; hep = &hashTable[hix];
        while( he && strcmp(he->key , hkey) ) {
            hep = &he->next;
            he = he->next;
        }
        if ( ! he ) {
            he = malloc(sizeof(struct sHashEntry));
            he->next = NULL;
            he->key = strdup(hkey);
            he->wordCount = 0;
            he->words = NULL;
            he->link = NULL;
            *hep = he;
        }
        we = malloc(sizeof(struct sDictWord));
        we->word = strdup(buffer);
        we->next = he->words;
        he->words = we;
        he->wordCount++;
        if ( maxPC < he->wordCount) {
            maxPC = he->wordCount;
            mostPerms = he;
            he->link = NULL;
        }
        else if (maxPC == he->wordCount) {
            he->link = mostPerms;
            mostPerms = he;
        }

        numWords++;
    }
    printf("%d words in dictionary max ana=%d\n", numWords, maxPC);
    return maxPC;
}


int main( )
{
    HashEntry he;
    DictWord  we;
    FILE *f1;

    f1 = fopen("unixdict.txt","r");
    buildAnagrams(f1);
    fclose(f1);

    f1 = fopen("anaout.txt","w");
//    f1 = stdout;

    for (he = mostPerms; he; he = he->link) {
        fprintf(f1,"%d:", he->wordCount);
        for(we = he->words; we; we = we->next) {
            fprintf(f1,"%s, ", we->word);
        }
        fprintf(f1, "\n");
    }

    fclose(f1);
    return 0;
}
```

{{out}} (less than 1 second on old P500)

```txt
5:vile, veil, live, levi, evil,
5:trace, crate, cater, carte, caret,
5:regal, large, lager, glare, alger,
5:neal, lena, lean, lane, elan,
5:lange, glean, galen, angle, angel,
5:elba, bela, bale, able, abel,

```

A much shorter version with no fancy data structures:

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>
#include <string.h>

typedef struct { const char *key, *word; int cnt; } kw_t;

int lst_cmp(const void *a, const void *b)
{
	return strcmp(((const kw_t*)a)->key, ((const kw_t*)b)->key);
}

/* Bubble sort.  Faster than stock qsort(), believe it or not */
void sort_letters(char *s)
{
	int i, j;
	char t;
	for (i = 0; s[i] != '\0'; i++) {
		for (j = i + 1; s[j] != '\0'; j++)
			if (s[j] < s[i]) {
				t = s[j]; s[j] = s[i]; s[i] = t;
			}
	}
}

int main()
{
	struct stat s;
	char *words, *keys;
	size_t i, j, k, longest, offset;
	int n_word = 0;
	kw_t *list;

	int fd = open("unixdict.txt", O_RDONLY);
	if (fd == -1) return 1;
	fstat(fd, &s);
	words = malloc(s.st_size * 2);
	keys  = words + s.st_size;

	read(fd, words, s.st_size);
	memcpy(keys, words, s.st_size);

	/* change newline to null for easy use; sort letters in keys */
	for (i = j = 0; i < s.st_size; i++) {
		if (words[i] == '\n') {
			words[i] = keys[i] = '\0';
			sort_letters(keys + j);
			j = i + 1;
			n_word ++;
		}
	}

	list = calloc(n_word, sizeof(kw_t));

	/* make key/word pointer pairs for sorting */
	for (i = j = k = 0; i < s.st_size; i++) {
		if (words[i] == '\0') {
			list[j].key = keys + k;
			list[j].word = words + k;
			k = i + 1;
			j++;
		}
	}

	qsort(list, n_word, sizeof(kw_t), lst_cmp);

	/* count each key's repetition */
	for (i = j = k = offset = longest = 0; i < n_word; i++) {
		if (!strcmp(list[i].key, list[j].key)) {
			++k;
			continue;
		}

		/* move current longest to begining of array */
		if (k < longest) {
			k = 0;
			j = i;
			continue;
		}

		if (k > longest) offset = 0;

		while (j < i) list[offset++] = list[j++];
		longest = k;
		k = 0;
	}

	/* show the longest */
	for (i = 0; i < offset; i++) {
		printf("%s ", list[i].word);
		if (i < n_word - 1 && strcmp(list[i].key, list[i+1].key))
			printf("\n");
	}

	/* free(list); free(words); */
	close(fd);
	return 0;
}
```

{{out}}

```txt

abel able bale bela elba
caret carte cater crate trace
angel angle galen glean lange
alger glare lager large regal
elan lane lean lena neal
evil levi live veil vile

```



## C++


```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <map>
#include <vector>
#include <algorithm>
#include <iterator>

int main() {
  std::ifstream in("unixdict.txt");
  typedef  std::map<std::string, std::vector<std::string> > AnagramMap;
  AnagramMap anagrams;

  std::string word;
  size_t count = 0;
  while (std::getline(in, word)) {
    std::string key = word;
    std::sort(key.begin(), key.end());
    // note: the [] op. automatically inserts a new value if key does not exist
    AnagramMap::mapped_type & v = anagrams[key];
    v.push_back(word);
    count = std::max(count, v.size());
  }

  in.close();

  for (AnagramMap::const_iterator it = anagrams.begin(), e = anagrams.end();
       it != e; it++)
    if (it->second.size() >= count) {
      std::copy(it->second.begin(), it->second.end(),
                std::ostream_iterator<std::string>(std::cout, ", "));
      std::cout << std::endl;
    }
  return 0;
}
```

{{out}}
 abel, able, bale, bela, elba,
 caret, carte, cater, crate, trace,
 angel, angle, galen, glean, lange,
 alger, glare, lager, large, regal,
 elan, lane, lean, lena, neal,
 evil, levi, live, veil, vile,

## C#

```c#
using System;
using System.IO;
using System.Linq;
using System.Net;
using System.Text.RegularExpressions;

namespace Anagram
{
    class Program
    {
        const string DICO_URL = "http://www.puzzlers.org/pub/wordlists/unixdict.txt";

        static void Main( string[] args )
        {
            WebRequest request = WebRequest.Create(DICO_URL);
            string[] words;
            using (StreamReader sr = new StreamReader(request.GetResponse().GetResponseStream(), true)) {
                words = Regex.Split(sr.ReadToEnd(), @"\r?\n");
            }
            var groups = from string w in words
                         group w by string.Concat(w.OrderBy(x => x)) into c
                         group c by c.Count() into d
                         orderby d.Key descending
                         select d;
            foreach (var c in groups.First()) {
                Console.WriteLine(string.Join(" ", c));
            }
        }
    }
}
```

{{out}}

```txt

abel able bale bela elba
alger glare lager large regal
angel angle galen glean lange
caret carte cater crate trace
elan lane lean lena neal
evil levi live veil vile

```



## Clojure

Assume ''wordfile'' is the path of the local file containing the words. This code makes a map (''groups'') whose keys are sorted letters and values are lists of the key's anagrams. It then determines the length of the longest list, and prints out all the lists of that length.

```clojure
(require '[clojure.java.io :as io])

(def groups
  (with-open [r (io/reader wordfile)]
    (group-by sort (line-seq r))))

(let [wordlists (sort-by (comp - count) (vals groups))
      maxlength (count (first wordlists))]
  (doseq [wordlist (take-while #(= (count %) maxlength) wordlists)]
    (println wordlist))
```



```clojure

(->> (slurp "http://www.puzzlers.org/pub/wordlists/unixdict.txt")
     clojure.string/split-lines
     (group-by sort)
     vals
     (sort-by count >)  ;; sort in reverse
     (partition-by count)
     first)

;; (["caret" "carte" "cater" "crate" "trace"]
;;  ["angel" "angle" "galen" "glean" "lange"]
;;  ["elan" "lane" "lean" "lena" "neal"]
;;  ["alger" "glare" "lager" "large" "regal"]
;;  ["evil" "levi" "live" "veil" "vile"]
;;  ["abel" "able" "bale" "bela" "elba"])

```



## COBOL

Tested with GnuCOBOL 2.0.  ALLWORDS output display trimmed for width.


```COBOL
      *> TECTONICS
      *>   wget http://www.puzzlers.org/pub/wordlists/unixdict.txt
      *>   or visit https://sourceforge.net/projects/souptonuts/files
      *>   or snag ftp://ftp.openwall.com/pub/wordlists/all.gz
      *>      for a 5 million all language word file (a few phrases)
      *>   cobc -xj anagrams.cob [-DMOSTWORDS -DMOREWORDS -DALLWORDS]
      *> ***************************************************************
       identification division.
       program-id. anagrams.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       input-output section.
       file-control.
           select words-in
           assign to wordfile
           organization is line sequential
           status is words-status
           .

       REPLACE ==:LETTERS:== BY ==42==.

       data division.
       file section.
       fd words-in record is varying from 1 to :LETTERS: characters
                             depending on word-length.
       01 word-record.
          05 word-data         pic x occurs 0 to :LETTERS: times
                                     depending on word-length.

       working-storage section.
       >>IF ALLWORDS DEFINED
       01 wordfile     constant as "/usr/local/share/dict/all.words".
       01 max-words    constant as 4802100.

       >>ELSE-IF MOSTWORDS DEFINED
       01 wordfile     constant as "/usr/local/share/dict/linux.words".
       01 max-words    constant as 628000.

       >>ELSE-IF MOREWORDS DEFINED
       01 wordfile     constant as "/usr/share/dict/words".
       01 max-words    constant as 100000.

       >>ELSE
       01 wordfile     constant as "unixdict.txt".
       01 max-words    constant as 26000.
       >>END-IF

      *> The 5 million word file needs to restrict the word length
       >>IF ALLWORDS DEFINED
       01 max-letters          constant as 26.
       >>ELSE
       01 max-letters          constant as :LETTERS:.
       >>END-IF

       01 word-length          pic 99 comp-5.
       01 words-status         pic xx.
          88 ok-status         values '00' thru '09'.
          88 eof-status        value '10'.

      *> sortable word by letter table
       01 letter-index         usage index.
       01 letter-table.
          05 letters           occurs 1 to max-letters times
                               depending on word-length
                               ascending key letter
                               indexed by letter-index.
             10 letter         pic x.

      *> table of words
       01 sorted-index         usage index.
       01 word-table.
          05 word-list         occurs 0 to max-words times
                               depending on word-tally
                               ascending key sorted-word
                               indexed by sorted-index.
             10 match-count    pic 999 comp-5.
             10 this-word      pic x(max-letters).
             10 sorted-word    pic x(max-letters).
       01 sorted-display       pic x(10).

       01 interest-table.
          05 interest-list     pic 9(8) comp-5
                               occurs 0 to max-words times
                               depending on interest-tally.

       01 outer                pic 9(8) comp-5.
       01 inner                pic 9(8) comp-5.
       01 starter              pic 9(8) comp-5.
       01 ender                pic 9(8) comp-5.
       01 word-tally           pic 9(8) comp-5.
       01 interest-tally       pic 9(8) comp-5.
       01 tally-display        pic zz,zzz,zz9.

       01 most-matches         pic 99 comp-5.
       01 matches              pic 99 comp-5.
       01 match-display        pic z9.

      *> timing display
       01 time-stamp.
          05 filler            pic x(11).
          05 timer-hours       pic 99.
          05 filler            pic x.
          05 timer-minutes     pic 99.
          05 filler            pic x.
          05 timer-seconds     pic 99.
          05 filler            pic x.
          05 timer-subsec      pic v9(6).
       01 timer-elapsed        pic 9(6)v9(6).
       01 timer-value          pic 9(6)v9(6).
       01 timer-display        pic zzz,zz9.9(6).

      *> ***************************************************************
       procedure division.
       main-routine.

       >>IF ALLWORDS DEFINED
           display "** Words limited to " max-letters " letters **"
       >>END-IF

       perform show-time

       perform load-words
       perform find-most
       perform display-result

       perform show-time
       goback
       .

      *> ***************************************************************
       load-words.
       open input words-in
       if not ok-status then
           display "error opening " wordfile upon syserr
           move 1 to return-code
           goback
       end-if

       perform until exit
           read words-in
           if eof-status then exit perform end-if
           if not ok-status then
               display wordfile " read error: " words-status upon syserr
           end-if

           if word-length equal zero then exit perform cycle end-if

       >>IF ALLWORDS DEFINED
           move min(word-length, max-letters) to word-length
       >>END-IF

           add 1 to word-tally
           move word-record to this-word(word-tally) letter-table
           sort letters ascending key letter
           move letter-table to sorted-word(word-tally)
       end-perform

       move word-tally to tally-display
       display trim(tally-display) " words" with no advancing

       close words-in
       if not ok-status then
           display "error closing " wordfile upon syserr
           move 1 to return-code
       end-if

      *> sort word list by anagram check field
       sort word-list ascending key sorted-word
       .

      *> first entry in a list will end up with highest match count
       find-most.
       perform varying outer from 1 by 1 until outer > word-tally
           move 1 to matches
           add 1 to outer giving starter
           perform varying inner from starter by 1
                   until sorted-word(inner) not equal sorted-word(outer)
               add 1 to matches
           end-perform
           if matches > most-matches then
               move matches to most-matches
               initialize interest-table all to value
               move 0 to interest-tally
           end-if
           move matches to match-count(outer)
           if matches = most-matches then
               add 1 to interest-tally
               move outer to interest-list(interest-tally)
           end-if
       end-perform
       .

      *> only display the words with the most anagrams
       display-result.
       move interest-tally to tally-display
       move most-matches to match-display
       display ", most anagrams: " trim(match-display)
               ", with " trim(tally-display) " set" with no advancing
       if interest-tally not equal 1 then
           display "s" with no advancing
       end-if
       display " of interest"

       perform varying outer from 1 by 1 until outer > interest-tally
           move sorted-word(interest-list(outer)) to sorted-display
           display sorted-display
                   " [" trim(this-word(interest-list(outer)))
              with no advancing
           add 1 to interest-list(outer) giving starter
           add most-matches to interest-list(outer) giving ender
           perform varying inner from starter by 1
               until inner = ender
                   display ", " trim(this-word(inner))
                      with no advancing
           end-perform
           display "]"
       end-perform
       .

      *> elapsed time
       show-time.
       move formatted-current-date("YYYY-MM-DDThh:mm:ss.ssssss")
         to time-stamp
       compute timer-value = timer-hours * 3600 + timer-minutes * 60
                             + timer-seconds + timer-subsec
       if timer-elapsed = 0 then
           display time-stamp
           move timer-value to timer-elapsed
       else
           if timer-value < timer-elapsed then
               add 86400 to timer-value
           end-if
           subtract timer-elapsed from timer-value
           move timer-value to timer-display
           display time-stamp ", " trim(timer-display) " seconds"
       end-if
       .

       end program anagrams.
```


{{out}}

```txt
prompt$ time cobc -xjd anagrams.cob
2016-05-04T07:13:23.225147
25,104 words, most anagrams: 5, with 6 sets of interest
abel       [abel, able, bale, bela, elba]
acert      [caret, carte, cater, crate, trace]
aegln      [angel, angle, galen, glean, lange]
aeglr      [alger, glare, lager, large, regal]
aeln       [elan, lane, lean, lena, neal]
eilv       [evil, levi, live, veil, vile]
2016-05-04T07:13:23.262851, 0.037704 seconds

real    0m0.191s
user    0m0.152s
sys     0m0.024s

prompt$ cobc -xjd anagrams.cob -DMOSTWORDS
2016-05-04T07:13:42.570360
627,999 words, most anagrams: 17, with 1 set of interest
aerst      [arest, arets, aster, astre, earst, rates, reast, resat, serta, stare, stear, tares, tarse, taser, tears, teras, treas]
2016-05-04T07:13:43.832743, 1.262383 seconds

prompt$ cobc -xjd anagrams.cob -DALLWORDS
** Words limited to 26 letters **
2016-05-04T07:13:50.944146
4,802,017 words, most anagrams: 68, with 1 set of interest
aeinst     [aisnet, aniets, anites, antesi, anties, antise, ...
2016-05-04T07:14:02.475959, 11.531813 seconds
```



## CoffeeScript


```coffeescript
http = require 'http'

show_large_anagram_sets = (word_lst) ->
  anagrams = {}
  max_size = 0

  for word in word_lst
    key = word.split('').sort().join('')
    anagrams[key] ?= []
    anagrams[key].push word
    size = anagrams[key].length
    max_size = size if size > max_size

  for key, variations of anagrams
    if variations.length == max_size
      console.log variations.join ' '

get_word_list = (process) ->
  options =
    host: "www.puzzlers.org"
    path: "/pub/wordlists/unixdict.txt"

  req = http.request options, (res) ->
    s = ''
    res.on 'data', (chunk) ->
      s += chunk
    res.on 'end', ->
      process s.split '\n'
  req.end()

get_word_list show_large_anagram_sets
```

{{out}}

```coffeescript>
 coffee anagrams.coffee
[ 'abel', 'able', 'bale', 'bela', 'elba' ]
[ 'alger', 'glare', 'lager', 'large', 'regal' ]
[ 'angel', 'angle', 'galen', 'glean', 'lange' ]
[ 'caret', 'carte', 'cater', 'crate', 'trace' ]
[ 'elan', 'lane', 'lean', 'lena', 'neal' ]
[ 'evil', 'levi', 'live', 'veil', 'vile' ]
```



## Common Lisp

{{libheader|DRAKMA}} to retrieve the wordlist.

```lisp
(defun anagrams (&optional (url "http://www.puzzlers.org/pub/wordlists/unixdict.txt"))
  (let ((words (drakma:http-request url :want-stream t))
        (wordsets (make-hash-table :test 'equalp)))
    ;; populate the wordsets and close stream
    (do ((word (read-line words nil nil) (read-line words nil nil)))
        ((null word) (close words))
      (let ((letters (sort (copy-seq word) 'char<)))
        (multiple-value-bind (pair presentp)
            (gethash letters wordsets)
          (if presentp
           (setf (car pair) (1+ (car pair))
                 (cdr pair) (cons word (cdr pair)))
           (setf (gethash letters wordsets)
                 (cons 1 (list word)))))))
    ;; find and return the biggest wordsets
    (loop with maxcount = 0 with maxwordsets = '()
          for pair being each hash-value of wordsets
          if (> (car pair) maxcount)
          do (setf maxcount (car pair)
                   maxwordsets (list (cdr pair)))
          else if (eql (car pair) maxcount)
          do (push (cdr pair) maxwordsets)
          finally (return (values maxwordsets maxcount)))))
```

Evalutating

```lisp
(multiple-value-bind (wordsets count) (anagrams)
  (pprint wordsets)
  (print count))
```

{{out}}

```txt
(("vile" "veil" "live" "levi" "evil")
 ("regal" "large" "lager" "glare" "alger")
 ("lange" "glean" "galen" "angle" "angel")
 ("neal" "lena" "lean" "lane" "elan")
 ("trace" "crate" "cater" "carte" "caret")
 ("elba" "bela" "bale" "able" "abel"))
5
```

Another method, assuming file is local:

```lisp
(defun read-words (file)
  (with-open-file (stream file)
    (loop with w = "" while w collect (setf w (read-line stream nil)))))

(defun anagram (file)
  (let ((wordlist (read-words file))
	(h (make-hash-table :test #'equal))
	longest)
    (loop for w in wordlist with ws do
	  (setf ws (sort (copy-seq w) #'char<))
	  (setf (gethash ws h) (cons w (gethash ws h))))
    (loop for w being the hash-keys in h using (hash-value wl)
	  with max-len = 0 do
	  (let ((l (length wl)))
	    (if (> l max-len) (setf longest nil max-len l))
	    (if (= l max-len) (push wl longest))))
    longest))

(format t "~{~{~a ~}~^~%~}" (anagram "unixdict.txt"))
```

{{out}}

```txt
elba bela bale able abel
regal large lager glare alger
lange glean galen angle angel
trace crate cater carte caret
neal lena lean lane elan
vile veil live levi evil
```



## Component Pascal

BlackBox Component Builder

```oberon2

MODULE BbtAnagrams;
IMPORT StdLog,Files,Strings,Args;
CONST
	MAXPOOLSZ = 1024;

TYPE
	Node = POINTER TO LIMITED RECORD;
		count: INTEGER;
		word: Args.String;
		desc: Node;
		next: Node;
	END;

	Pool = POINTER TO LIMITED RECORD
		capacity,max: INTEGER;
		words: POINTER TO ARRAY OF Node;
	END;

	PROCEDURE NewNode(word: ARRAY OF CHAR): Node;
	VAR
		n: Node;
	BEGIN
		NEW(n);n.count := 0;n.word := word$;
		n.desc := NIL;n.next := NIL;
		RETURN n
	END NewNode;

	PROCEDURE Index(s: ARRAY OF CHAR;cap: INTEGER): INTEGER;
	VAR
		i,sum: INTEGER;
	BEGIN
		sum := 0;
		FOR i := 0 TO  LEN(s$) DO
			INC(sum,ORD(s[i]))
		END;
		RETURN sum MOD cap
	END Index;

	PROCEDURE ISort(VAR s: ARRAY OF CHAR);
	VAR
        i, j: INTEGER;
        t: CHAR;
	BEGIN
        FOR i := 0 TO LEN(s$) - 1 DO
			j := i;
			t := s[j];
			WHILE (j > 0) & (s[j -1] > t) DO
					s[j] := s[j - 1];
					DEC(j)
			END;
			s[j] := t
        END
	END ISort;

	PROCEDURE SameLetters(x,y: ARRAY OF CHAR): BOOLEAN;
	BEGIN
        ISort(x);ISort(y);
        RETURN x = y
	END SameLetters;

	PROCEDURE NewPoolWith(cap: INTEGER): Pool;
	VAR
		i: INTEGER;
		p: Pool;
	BEGIN
		NEW(p);
		p.capacity := cap;
		p.max := 0;
		NEW(p.words,cap);
		i := 0;
		WHILE i < p.capacity DO
			p.words[i] := NIL;
			INC(i);
		END;
		RETURN p
	END NewPoolWith;

	PROCEDURE NewPool(): Pool;
	BEGIN
		RETURN NewPoolWith(MAXPOOLSZ);
	END NewPool;

	PROCEDURE (p: Pool) Add(w: ARRAY OF CHAR), NEW;
	VAR
		idx: INTEGER;
		iter,n: Node;
	BEGIN
		idx := Index(w,p.capacity);
		iter := p.words[idx];
		n := NewNode(w);
		WHILE(iter # NIL) DO
			IF SameLetters(w,iter.word) THEN
				INC(iter.count);
				IF iter.count > p.max THEN p.max := iter.count END;
				n.desc := iter.desc;
				iter.desc := n;
				RETURN
			END;
			iter := iter.next
		END;
		ASSERT(iter = NIL);
		n.next := p.words[idx];p.words[idx] := n
	END Add;

	PROCEDURE ShowAnagrams(l: Node);
	VAR
		iter: Node;
	BEGIN
		iter := l;
		WHILE iter # NIL DO
			StdLog.String(iter.word);StdLog.String(" ");
			iter := iter.desc
		END;
		StdLog.Ln
	END ShowAnagrams;

	PROCEDURE (p: Pool) ShowMax(),NEW;
	VAR
		i: INTEGER;
		iter: Node;
	BEGIN
		FOR i := 0 TO LEN(p.words) - 1 DO
			IF p.words[i] # NIL THEN
				iter := p.words^[i];
				WHILE iter # NIL DO
					IF iter.count = p.max THEN
						ShowAnagrams(iter);
					END;
					iter := iter.next
				END
			END
		END
	END ShowMax;

	PROCEDURE GetLine(rd: Files.Reader; OUT str: ARRAY OF CHAR);
	VAR
		i: INTEGER;
		b: BYTE;
	BEGIN
		rd.ReadByte(b);i := 0;
		WHILE (~rd.eof) & (i < LEN(str)) DO
			IF (b = ORD(0DX)) OR (b = ORD(0AX)) THEN str[i] := 0X; RETURN  END;
			str[i] := CHR(b);
			rd.ReadByte(b);INC(i)
		END;
		str[LEN(str) - 1] := 0X
	END GetLine;

	PROCEDURE DoProcess*;
	VAR
		params : Args.Params;
		loc: Files.Locator;
		fd: Files.File;
		rd: Files.Reader;
		line: ARRAY 81 OF CHAR;
		p: Pool;
	BEGIN
		Args.Get(params);
		IF params.argc = 1 THEN
		  loc := Files.dir.This("Bbt");
			fd := Files.dir.Old(loc,params.args[0]$,FALSE);
			StdLog.String("Processing: " + params.args[0]);StdLog.Ln;StdLog.Ln;
			rd := fd.NewReader(NIL);
			p := NewPool();
			REPEAT
				GetLine(rd,line);
				p.Add(line);
			UNTIL rd.eof;
			p.ShowMax()
		ELSE
			StdLog.String("Error: Missing file to process");StdLog.Ln
		END;
	END DoProcess;

END BbtAnagrams.

```

Execute:^Q BbtAnagrams.DoProcess unixdict.txt~<br/>
{{out}}

```txt

Processing: unixdict.txt

abel elba bela bale able
elan neal lena lean lane
evil vile veil live levi
angel lange glean galen angle
alger regal large lager glare
caret trace crate cater carte

```

== {{header|D}} ==

### Short Functional Version


```d
import std.stdio, std.algorithm, std.string, std.exception, std.file;

void main() {
    string[][ubyte[]] an;
    foreach (w; "unixdict.txt".readText.splitLines)
        an[w.dup.representation.sort().release.assumeUnique] ~= w;
    immutable m = an.byValue.map!q{ a.length }.reduce!max;
    writefln("%(%s\n%)", an.byValue.filter!(ws => ws.length == m));
}
```

{{out}}

```txt
["caret", "carte", "cater", "crate", "trace"]
["evil", "levi", "live", "veil", "vile"]
["abel", "able", "bale", "bela", "elba"]
["elan", "lane", "lean", "lena", "neal"]
["alger", "glare", "lager", "large", "regal"]
["angel", "angle", "galen", "glean", "lange"]
```

Runtime: about 0.07 seconds.


### Faster Version

Less safe, same output.

```d
void main() {
    import std.stdio, std.algorithm, std.file, std.string;

    auto keys = "unixdict.txt".readText!(char[]);
    immutable vals = keys.idup;
    string[][string] anags;
    foreach (w; keys.splitter) {
        immutable k = w.representation.sort().release.assumeUTF;
        anags[k] ~= vals[k.ptr - keys.ptr .. k.ptr - keys.ptr + k.length];
    }
    //immutable m = anags.byValue.maxs!q{ a.length };
    immutable m = anags.byValue.map!q{ a.length }.reduce!max;
    writefln("%(%-(%s %)\n%)", anags.byValue.filter!(ws => ws.length == m));
}
```

Runtime: about 0.06 seconds.


## E


```e
println("Downloading...")
when (def wordText := <http://www.puzzlers.org/pub/wordlists/unixdict.txt> <- getText()) -> {
    def words := wordText.split("\n")

    def storage := [].asMap().diverge()
    def anagramTable extends storage {
        to get(key) { return storage.fetch(key, fn { storage[key] := [].diverge() }) }
    }

    println("Grouping...")
    var largestGroupSeen := 0
    for word in words {
        def anagramGroup := anagramTable[word.sort()]
        anagramGroup.push(word)
        largestGroupSeen max= anagramGroup.size()
    }

    println("Selecting...")
    for _ => anagramGroup ? (anagramGroup.size() == mostSeen) in anagramTable {
        println(anagramGroup.snapshot())
    }
}
```



## EchoLisp

For a change, we will use the french dictionary - '''(lib 'dico.fr)''' - delivered within EchoLisp.

```scheme

(require 'struct)
(require 'hash)
(require 'sql)
(require 'words)
(require 'dico.fr.no-accent)


(define mots-français (words-select #:any null 999999))
(string-delimiter "")

(define (string-sort str)
	(list->string (list-sort string<?  (string->list str))))

(define (ana-sort H words) ;; bump counter for each word
	(for ((w words))
		#:continue (< (string-length w) 4)
		(let [(key (string-sort w))]  (hash-set H key (1+ (hash-ref! H key 0))))))

;; input w word
;; output : list of matching words
(define (anagrams w words)
	(set! w (string-sort w))
	(make-set
	(for/list (( ana words))
		#:when (string=? w (string-sort ana))
		ana)))

(define (task words)
(define H (make-hash))
	(ana-sort H words) ;; build counters key= sorted-string, value = count
	(hash-get-keys H   ;; extract max count values
	    (for/fold (hmax 0) ((h H) )
	    #:when (>= (cdr h) hmax)
	    (cdr h))
	))

```

{{out}}

```scheme

(length mots-français)
    → 209315
(task mots-français)
    → (aeilns acenr) ;; two winners
(anagrams "acenr" mots-français)
    → { ancre caner caren carne ceran cerna encra nacre nerac rance renac }
(anagrams "aeilns" mots-français)
    → { alisen enlias enlisa ensila islaen islean laines lianes salien saline selina }


```



## Eiffel


```Eiffel

class
	ANAGRAMS

create
	make

feature

	make
			-- Set of Anagrams, containing most words.
		local
			count: INTEGER
		do
			read_wordlist
			across
				words as wo
			loop
				if wo.item.count > count then
					count := wo.item.count
				end
			end
			across
				words as wo
			loop
				if wo.item.count = count then
					across
						wo.item as list
					loop
						io.put_string (list.item + "%T")
					end
					io.new_line
				end
			end
		end

	original_list: STRING = "unixdict.txt"

feature {NONE}

	read_wordlist
			-- Preprocessed wordlist for finding Anagrams.
		local
			l_file: PLAIN_TEXT_FILE
			sorted: STRING
			empty_list: LINKED_LIST [STRING]
		do
			create l_file.make_open_read_write (original_list)
			l_file.read_stream (l_file.count)
			wordlist := l_file.last_string.split ('%N')
			l_file.close
			create words.make (wordlist.count)
			across
				wordlist as w
			loop
				create empty_list.make
				sorted := sort_letters (w.item)
				words.put (empty_list, sorted)
				if attached words.at (sorted) as ana then
					ana.extend (w.item)
				end
			end
		end

	wordlist: LIST [STRING]

	sort_letters (word: STRING): STRING
			--Sorted in alphabetical order.
		local
			letters: SORTED_TWO_WAY_LIST [STRING]
		do
			create letters.make
			create Result.make_empty
			across
				1 |..| word.count as i
			loop
				letters.extend (word.at (i.item).out)
			end
			across
				letters as s
			loop
				Result.append (s.item)
			end
		end

	words: HASH_TABLE [LINKED_LIST [STRING], STRING]

end

```

{{out}}

```txt

abel    able    bale    bela    elba
alger   glare   lager   large   regal
angel   angle   galen   glean   lange
caret   carte   cater   crate   trace
elan    lane    lean    lena    neal
evil    levi    live    veil    vile

```



## Ela

{{trans|Haskell}}

```ela
open monad io list string

groupon f x y = f x == f y

lines = split "\n" << replace "\n\n" "\n" << replace "\r" "\n"

main = do
  fh <- readFile "c:\\test\\unixdict.txt" OpenMode
  f <- readLines fh
  closeFile fh
  let words = lines f
  let wix = groupBy (groupon fst) << sort $ zip (map sort words) words
  let mxl = maximum $ map length wix
  mapM_ (putLn << map snd) << filter ((==mxl) << length) $ wix
```


{{out}}
```txt
["vile","veil","live","levi","evil"]
["neal","lena","lean","lane","elan"]
["regal","large","lager","glare","alger"]
["lange","glean","galen","angle","angel"]
["trace","crate","cater","carte","caret"]
["elba","bela","bale","able","abel"]

```



## Elena

ELENA 4.0:

```elena
import system'routines;
import system'io;
import system'collections;
import extensions;
import extensions'routines;
import extensions'text;

extension op
{
     string normalized()
         = self.toArray().ascendant().summarize(new StringWriter());
}

public program()
{
    auto dictionary := new Map<string,object>();

    File.assign("unixdict.txt").forEachLine:(word)
    {
        var key := word.normalized();
        var item := dictionary[key];
        if (nil == item)
        {
            item := new ArrayList();
            dictionary[key] := item
        };

        item.append:word
    };

    dictionary.Values
        .sort:(former,later => former.Item2.Length > later.Item2.Length )
        .top:20
        .forEach:(pair){ console.printLine(pair.Item2) };

    console.readChar()
}
```

{{out}}

```txt

evil,levi,live,veil,vile
elan,lane,lean,lena,neal
caret,carte,cater,crate,trace
angel,angle,galen,glean,lange
alger,glare,lager,large,regal
abel,able,bale,bela,elba
resin,rinse,risen,siren
pare,pear,rape,reap
nepal,panel,penal,plane
mate,meat,tame,team
manor,moran,norma,roman
lima,mail,mali,mila
lien,line,neil,nile
lemon,melon,menlo,monel
leapt,petal,plate,pleat
leap,pale,peal,plea
latus,sault,talus,tulsa
lascar,rascal,sacral,scalar
lament,mantel,mantle,mental
keats,skate,stake,steak

```



## Elixir


```Elixir
defmodule Anagrams do
  def find(file) do
    File.read!(file)
    |> String.split
    |> Enum.group_by(fn word -> String.codepoints(word) |> Enum.sort end)
    |> Enum.group_by(fn {_,v} -> length(v) end)
    |> Enum.max
    |> print
  end

  defp print({_,y}) do
    Enum.each(y, fn {_,e} -> Enum.sort(e) |> Enum.join(" ") |> IO.puts end)
  end
end

Anagrams.find("unixdict.txt")
```


{{out}}

```txt

caret carte cater crate trace
evil levi live veil vile
alger glare lager large regal
elan lane lean lena neal
angel angle galen glean lange
abel able bale bela elba

```


The same output, using <code>File.Stream!</code> to generate <code>tuples</code> containing the word and it's sorted value as <code>strings</code>.

```Elixir
File.stream!("unixdict.txt")
  |> Stream.map(&String.strip &1)
  |> Enum.group_by(&String.codepoints(&1) |> Enum.sort)
  |> Map.values
  |> Enum.group_by(&length &1)
  |> Enum.max
  |> elem(1)
  |> Enum.each(fn n -> Enum.sort(n) |> Enum.join(" ") |> IO.puts end)
```


{{out}}

```txt

caret carte cater crate trace
evil levi live veil vile
alger glare lager large regal
elan lane lean lena neal
angel angle galen glean lange
abel able bale bela elba

```



## Erlang

The function fetch/2 is used to solve [[Anagrams/Deranged_anagrams]]. Please keep backwards compatibility when editing. Or update the other module, too.

```erlang
-module(anagrams).
-compile(export_all).

play() ->
    {ok, P} = file:read_file('unixdict.txt'),
    D = dict:new(),
    E=fetch(string:tokens(binary_to_list(P), "\n"), D),
    get_value(dict:fetch_keys(E), E).

fetch([H|T], D) ->
    fetch(T, dict:append(lists:sort(H), H, D));
fetch([], D) ->
    D.

get_value(L, D) -> get_value(L,D,1,[]).
get_value([H|T], D, N, L) ->
    Var = dict:fetch(H,D),
    Len = length(Var),
    if
        Len > N ->
            get_value(T, D, Len, [Var]);
        Len == N ->
            get_value(T, D, Len, [Var | L]);
        Len < N ->
            get_value(T, D, N, L)
    end;

get_value([], _, _, L) ->
    L.

```

{{out}}

```txt

1> anagrams:play().
[["caret","carte","cater","crate","trace"],
 ["elan","lane","lean","lena","neal"],
 ["alger","glare","lager","large","regal"],
 ["angel","angle","galen","glean","lange"],
 ["evil","levi","live","veil","vile"],
 ["abel","able","bale","bela","elba"]]
2>

```



## Euphoria


```euphoria
include sort.e

function compare_keys(sequence a, sequence b)
    return compare(a[1],b[1])
end function

constant fn = open("unixdict.txt","r")
sequence words, anagrams
object word
words = {}
while 1 do
    word = gets(fn)
    if atom(word) then
        exit
    end if
    word = word[1..$-1] -- truncate new-line character
    words = append(words, {sort(word), word})
end while
close(fn)

integer maxlen
maxlen = 0
words = custom_sort(routine_id("compare_keys"), words)
anagrams = {words[1]}
for i = 2 to length(words) do
    if equal(anagrams[$][1],words[i][1]) then
        anagrams[$] = append(anagrams[$], words[i][2])
    elsif length(anagrams[$]) = 2 then
        anagrams[$] = words[i]
    else
        if length(anagrams[$]) > maxlen then
            maxlen = length(anagrams[$])
        end if
        anagrams = append(anagrams, words[i])
    end if
end for
if length(anagrams[$]) = 2 then
    anagrams = anagrams[1..$-1]
end if

for i = 1 to length(anagrams) do
    if length(anagrams[i]) = maxlen then
        for j = 2 to length(anagrams[i]) do
            puts(1,anagrams[i][j])
            puts(1,' ')
        end for
        puts(1,"\n")
    end if
end for
```

{{out}}

```txt
abel bela bale elba able
crate cater carte caret trace
angle galen glean lange angel
regal lager large alger glare
elan lean neal lane lena
live veil vile levi evil

```


=={{header|F Sharp|F#}}==
Read the lines in the dictionary, group by the sorted letters in each word, find the length of the longest sets of anagrams, extract the longest sequences of words sharing the same letters (i.e. anagrams):

```fsharp
let xss = Seq.groupBy (Array.ofSeq >> Array.sort) (System.IO.File.ReadAllLines "unixdict.txt")
Seq.map snd xss |> Seq.filter (Seq.length >> ( = ) (Seq.map (snd >> Seq.length) xss |> Seq.max))
```

Note that it is necessary to convert the sorted letters in each word from sequences to arrays because the groupBy function uses the default comparison and sequences do not compare structurally (but arrays do in F#).

Takes 0.8s to return:

```fsharp
val it : string seq seq =
  seq
    [seq ["abel"; "able"; "bale"; "bela"; "elba"];
     seq ["alger"; "glare"; "lager"; "large"; "regal"];
     seq ["angel"; "angle"; "galen"; "glean"; "lange"];
     seq ["caret"; "carte"; "cater"; "crate"; "trace"];
     seq ["elan"; "lane"; "lean"; "lena"; "neal"];
     seq ["evil"; "levi"; "live"; "veil"; "vile"]]
```



## FBSL

'''A little bit of cheating: literatim re-implementation of C solution in FBSL's Dynamic C layer.'''

```C
#APPTYPE CONSOLE

DIM gtc = GetTickCount()
Anagram()
PRINT "Done in ", (GetTickCount() - gtc) / 1000, " seconds"

PAUSE

DYNC Anagram()
	#include <windows.h>
	#include <stdio.h>

	char* sortedWord(const char* word, char* wbuf)
	{
		char* p1, *p2, *endwrd;
		char t;
		int swaps;

		strcpy(wbuf, word);
		endwrd = wbuf + strlen(wbuf);
		do {
			swaps = 0;
			p1 = wbuf; p2 = endwrd - 1;
			while (p1 < p2) {
				if (*p2 >* p1) {
					t = *p2; *p2 = *p1; *p1 = t;
					swaps = 1;
				}
				p1++; p2--;
			}
			p1 = wbuf; p2 = p1 + 1;
			while (p2 < endwrd) {
				if (*p2 >* p1) {
					t = *p2; *p2 = *p1; *p1 = t;
					swaps = 1;
				}
				p1++; p2++;
			}
		} while (swaps);
		return wbuf;
	}

	static short cxmap[] = {
		0x06, 0x1f, 0x4d, 0x0c, 0x5c, 0x28, 0x5d, 0x0e, 0x09, 0x33, 0x31, 0x56,
		0x52, 0x19, 0x29, 0x53, 0x32, 0x48, 0x35, 0x55, 0x5e, 0x14, 0x27, 0x24,
		0x02, 0x3e, 0x18, 0x4a, 0x3f, 0x4c, 0x45, 0x30, 0x08, 0x2c, 0x1a, 0x03,
		0x0b, 0x0d, 0x4f, 0x07, 0x20, 0x1d, 0x51, 0x3b, 0x11, 0x58, 0x00, 0x49,
		0x15, 0x2d, 0x41, 0x17, 0x5f, 0x39, 0x16, 0x42, 0x37, 0x22, 0x1c, 0x0f,
		0x43, 0x5b, 0x46, 0x4b, 0x0a, 0x26, 0x2e, 0x40, 0x12, 0x21, 0x3c, 0x36,
		0x38, 0x1e, 0x01, 0x1b, 0x05, 0x4e, 0x44, 0x3d, 0x04, 0x10, 0x5a, 0x2a,
		0x23, 0x34, 0x25, 0x2f, 0x2b, 0x50, 0x3a, 0x54, 0x47, 0x59, 0x13, 0x57,
	};
	#define CXMAP_SIZE (sizeof(cxmap) / sizeof(short))

	int Str_Hash(const char* key, int ix_max)
	{
		const char* cp;
		short mash;
		int hash = 33501551;
		for (cp = key; *cp; cp++) {
			mash = cxmap[*cp % CXMAP_SIZE];
			hash = (hash >>4) ^ 0x5C5CF5C ^ ((hash << 1) + (mash << 5));
			hash &= 0x3FFFFFFF;
		}
		return hash % ix_max;
	}

	typedef struct sDictWord* DictWord;
	struct sDictWord {
		const char* word;
		DictWord next;
	};

	typedef struct sHashEntry* HashEntry;
	struct sHashEntry {
		const char* key;
		HashEntry next;
		DictWord words;
		HashEntry link;
		short wordCount;
	};

	#define HT_SIZE 8192

	HashEntry hashTable[HT_SIZE];

	HashEntry mostPerms = NULL;

	int buildAnagrams(FILE* fin)
	{
		char buffer[40];
		char bufr2[40];
		char* hkey;
		int hix;
		HashEntry he, *hep;
		DictWord we;
		int maxPC = 2;
		int numWords = 0;

		while (fgets(buffer, 40, fin)) {
			for (hkey = buffer; *hkey && (*hkey != '\n'); hkey++);
			*hkey = 0;
			hkey = sortedWord(buffer, bufr2);
			hix = Str_Hash(hkey, HT_SIZE);
			he = hashTable[hix]; hep = &hashTable[hix];
			while (he && strcmp(he->key, hkey)) {
				hep = &he->next;
				he = he->next;
			}
			if (! he) {
				he = (HashEntry)malloc(sizeof(struct sHashEntry));
				he->next = NULL;
				he->key = strdup(hkey);
				he->wordCount = 0;
				he->words = NULL;
				he->link = NULL;
				*hep = he;
			}
			we = (DictWord)malloc(sizeof(struct sDictWord));
			we->word = strdup(buffer);
			we->next = he->words;
			he->words = we;
			he->wordCount++;
			if (maxPC < he->wordCount) {
				maxPC = he->wordCount;
				mostPerms = he;
				he->link = NULL;
			}
			else if (maxPC == he->wordCount) {
				he->link = mostPerms;
				mostPerms = he;
			}
			numWords++;
		}
		printf("%d words in dictionary max ana=%d\n", numWords, maxPC);
		return maxPC;
	}

	void main()
	{
		HashEntry he;
		DictWord we;
		FILE* f1;

		f1 = fopen("unixdict.txt", "r");
		buildAnagrams(f1);
		fclose(f1);

		f1 = fopen("anaout.txt", "w");

		for (he = mostPerms; he; he = he->link) {
			fprintf(f1, "%d: ", he->wordCount);
			for (we = he->words; we; we = we->next) {
				fprintf(f1, "%s, ", we->word);
			}
			fprintf(f1, "\n");
		}
		fclose(f1);
	}
END DYNC
```

{{out}} (2.2GHz Intel Core2 Duo)

```txt
25104 words in dictionary max ana=5
Done in 0.031 seconds

Press any key to continue...
```

'''"anaout.txt" listing:'''

```txt
5: vile, veil, live, levi, evil,
5: trace, crate, cater, carte, caret,
5: regal, large, lager, glare, alger,
5: neal, lena, lean, lane, elan,
5: lange, glean, galen, angle, angel,
5: elba, bela, bale, able, abel,
```


== {{header|Factor}} ==

```factor
 "resource:unixdict.txt" utf8 file-lines
 [ [ natural-sort >string ] keep ] { } map>assoc sort-keys
 [ [ first ] compare +eq+ = ] monotonic-split
 dup 0 [ length max ] reduce '[ length _ = ] filter [ values ] map .
```


```factor
{
    { "abel" "able" "bale" "bela" "elba" }
    { "caret" "carte" "cater" "crate" "trace" }
    { "angel" "angle" "galen" "glean" "lange" }
    { "alger" "glare" "lager" "large" "regal" }
    { "elan" "lane" "lean" "lena" "neal" }
    { "evil" "levi" "live" "veil" "vile" }
}
```



## Fantom


```fantom
class Main
{
  // take given word and return a string rearranging characters in order
  static Str toOrderedChars (Str word)
  {
    Str[] chars := [,]
    word.each |Int c| { chars.add (c.toChar) }
    return chars.sort.join("")
  }

  // add given word to anagrams map
  static Void addWord (Str:Str[] anagrams, Str word)
  {
    Str orderedWord := toOrderedChars (word)
    if (anagrams.containsKey (orderedWord))
      anagrams[orderedWord].add (word)
    else
      anagrams[orderedWord] = [word]
  }

  public static Void main ()
  {
    Str:Str[] anagrams := [:] // map Str -> Str[]
    // loop through input file, adding each word to map of anagrams
    File (`unixdict.txt`).eachLine |Str word|
    {
      addWord (anagrams, word)
    }
    // loop through anagrams, keeping the keys with values of largest size
    Str[] largestKeys := [,]
    anagrams.keys.each |Str k|
    {
      if ((largestKeys.size < 1) || (anagrams[k].size == anagrams[largestKeys[0]].size))
        largestKeys.add (k)
      else if (anagrams[k].size > anagrams[largestKeys[0]].size)
        largestKeys = [k]
    }
    largestKeys.each |Str k|
    {
      echo ("Key: $k -> " + anagrams[k].join(", "))
    }
  }
}
```


{{out}}

```txt
Key: abel -> abel, able, bale, bela, elba
Key: aeln -> elan, lane, lean, lena, neal
Key: eilv -> evil, levi, live, veil, vile
Key: aegln -> angel, angle, galen, glean, lange
Key: aeglr -> alger, glare, lager, large, regal
Key: acert -> caret, carte, cater, crate, trace

```


== {{header|Fortran}} ==
This program:

```fortran
!***************************************************************************************
	module anagram_routines
!***************************************************************************************
	implicit none

	!the dictionary file:
	integer,parameter :: file_unit = 1000
	character(len=*),parameter :: filename = 'unixdict.txt'

	!maximum number of characters in a word:
	integer,parameter :: max_chars = 50

	!maximum number of characters in the string displaying the anagram lists:
	integer,parameter :: str_len = 256

	type word
	  character(len=max_chars) :: str = repeat(' ',max_chars)    !the word from the dictionary
	  integer                  :: n = 0                          !length of this word
	  integer                  :: n_anagrams = 0	             !number of anagrams found
	  logical                  :: checked = .false.              !if this one has already been checked
	  character(len=str_len)   :: anagrams = repeat(' ',str_len) !the anagram list for this word
	end type word

	!the dictionary structure:
	type(word),dimension(:),allocatable,target :: dict

	contains
!***************************************************************************************

	!******************************************************************************
		function count_lines_in_file(fid) result(n_lines)
	!******************************************************************************
		implicit none

		integer             :: n_lines
		integer,intent(in)  :: fid
		character(len=1)    :: tmp
		integer             :: i
		integer             :: ios

		!the file is assumed to be open already.

		rewind(fid)	  !rewind to beginning of the file

		n_lines = 0
		do !read each line until the end of the file.
			read(fid,'(A1)',iostat=ios) tmp
			if (ios < 0) exit      !End of file
			n_lines = n_lines + 1  !row counter
		end do

		rewind(fid)   !rewind to beginning of the file

	!******************************************************************************
		end function count_lines_in_file
	!******************************************************************************

	!******************************************************************************
		pure elemental function is_anagram(x,y)
	!******************************************************************************
		implicit none
		character(len=*),intent(in) :: x
		character(len=*),intent(in) :: y
		logical :: is_anagram

		character(len=len(x)) :: x_tmp	!a copy of x
		integer :: i,j

		!a character not found in any word:
		character(len=1),parameter :: null = achar(0)

		!x and y are assumed to be the same size.

		x_tmp = x
		do i=1,len_trim(x)
			j = index(x_tmp, y(i:i)) !look for this character in x_tmp
			if (j/=0) then
				x_tmp(j:j) = null  !clear it so it won't be checked again
			else
				is_anagram = .false. !character not found: x,y are not anagrams
				return
			end if
		end do

		!if we got to this point, all the characters
		! were the same, so x,y are anagrams:
		is_anagram = .true.

	!******************************************************************************
		end function is_anagram
	!******************************************************************************

!***************************************************************************************
	end module anagram_routines
!***************************************************************************************

!***************************************************************************************
	program main
!***************************************************************************************
	use anagram_routines
	implicit none

	integer :: n,i,j,n_max
	type(word),pointer :: x,y
	logical :: first_word
	real :: start, finish

	call cpu_time(start)	!..start timer

	!open the dictionary and read in all the words:
	open(unit=file_unit,file=filename)      !open the file
	n = count_lines_in_file(file_unit)      !count lines in the file
	allocate(dict(n))                       !allocate dictionary structure
	do i=1,n                                !
		read(file_unit,'(A)') dict(i)%str   !each line is a word in the dictionary
		dict(i)%n = len_trim(dict(i)%str)   !saving length here to avoid trim's below
	end do
	close(file_unit)                        !close the file

	!search dictionary for anagrams:
	do i=1,n

		x => dict(i)	!pointer to simplify code
		first_word = .true.	!initialize

		do j=i,n

			y => dict(j)	!pointer to simplify code

			!checks to avoid checking words unnecessarily:
			if (x%checked .or. y%checked) cycle     !both must not have been checked already
			if (x%n/=y%n) cycle                     !must be the same size
			if (x%str(1:x%n)==y%str(1:y%n)) cycle   !can't be the same word

			! check to see if x,y are anagrams:
			if (is_anagram(x%str(1:x%n), y%str(1:y%n))) then
				!they are anagrams.
				y%checked = .true. 	!don't check this one again.
				x%n_anagrams = x%n_anagrams + 1
				if (first_word) then
					!this is the first anagram found for this word.
					first_word = .false.
					x%n_anagrams = x%n_anagrams + 1
					x%anagrams = trim(x%anagrams)//x%str(1:x%n)  !add first word to list
				end if
				x%anagrams = trim(x%anagrams)//','//y%str(1:y%n) !add next word to list
			end if

		end do
		x%checked = .true.  !don't check this one again

	end do

	!anagram groups with the most words:
	write(*,*) ''
	n_max = maxval(dict%n_anagrams)
	do i=1,n
		if (dict(i)%n_anagrams==n_max) write(*,'(A)') trim(dict(i)%anagrams)
	end do

	!anagram group containing longest words:
	write(*,*) ''
	n_max = maxval(dict%n, mask=dict%n_anagrams>0)
	do i=1,n
		if (dict(i)%n_anagrams>0 .and. dict(i)%n==n_max) write(*,'(A)') trim(dict(i)%anagrams)
	end do
	write(*,*) ''

	call cpu_time(finish)	!...stop timer
	write(*,'(A,F6.3,A)') '[Runtime = ',finish-start,' sec]'
	write(*,*) ''

!***************************************************************************************
	end program main
!***************************************************************************************
```


{{out}}

```txt

	abel,able,bale,bela,elba
	alger,glare,lager,large,regal
	angel,angle,galen,glean,lange
	caret,carte,cater,crate,trace
	elan,lane,lean,lena,neal
	evil,levi,live,veil,vile

	conservation,conversation

	[Runtime =  6.897 sec]

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type IndexedWord
  As String word
  As Integer index
End Type

' selection sort, quick enough for sorting small number of letters
Sub sortWord(s As String)
  Dim As Integer i, j, m, n = Len(s)
  For i = 0 To n - 2
    m = i
    For j = i + 1 To n - 1
      If s[j] < s[m] Then m = j
    Next j
    If m <> i Then Swap s[i], s[m]
  Next i
End Sub

' selection sort, quick enough for sorting small array of IndexedWord instances by index
Sub sortIndexedWord(iw() As IndexedWord)
  Dim As Integer i, j, m, n = UBound(iw)
  For i = 1 To n - 1
    m = i
    For j = i + 1 To n
      If iw(j).index < iw(m).index Then m = j
    Next j
    If m <> i Then Swap iw(i), iw(m)
  Next i
End Sub

' quicksort for sorting whole dictionary of IndexedWord instances by sorted word
Sub quicksort(a() As IndexedWord, first As Integer, last As Integer)
  Dim As Integer length = last - first + 1
  If length < 2 Then Return
  Dim pivot As String = a(first + length\ 2).word
  Dim lft As Integer = first
  Dim rgt As Integer = last
  While lft <= rgt
    While a(lft).word < pivot
      lft +=1
    Wend
    While a(rgt).word > pivot
      rgt -= 1
    Wend
    If lft <= rgt Then
       Swap a(lft), a(rgt)
       lft += 1
       rgt -= 1
    End If
  Wend
  quicksort(a(), first, rgt)
  quicksort(a(), lft, last)
End Sub

Dim t As Double = timer
Dim As String w()  '' array to hold actual words
Open "undict.txt" For Input As #1
Dim count As Integer = 0
While Not Eof(1)
  count +=1
  Redim Preserve w(1 To count)
  Line Input #1, w(count)
Wend
Close #1

Dim As IndexedWord iw(1 To count) '' array to hold sorted words and their index into w()
Dim word As String
For i As Integer = 1 To count
  word = w(i)
  sortWord(word)
  iw(i).word = word
  iw(i).index = i
Next
quickSort iw(), 1, count  '' sort the IndexedWord array by sorted word

Dim As Integer startIndex = 1, length = 1, maxLength = 1, ub = 1
Dim As Integer maxIndex(1 To ub)
maxIndex(ub) = 1
word = iw(1).word

For i As Integer = 2 To count
  If word = iw(i).word Then
    length += 1
  Else
    If length > maxLength Then
      maxLength = length
      Erase maxIndex
      ub = 1
      Redim maxIndex(1 To ub)
      maxIndex(ub) = startIndex
    ElseIf length = maxLength Then
      ub += 1
      Redim Preserve maxIndex(1 To ub)
      maxIndex(ub) = startIndex
    End If
    startIndex = i
    length = 1
    word = iw(i).word
  End If
Next

If length > maxLength Then
  maxLength = length
  Erase maxIndex
  Redim maxIndex(1 To 1)
  maxIndex(1) = startIndex
ElseIf length = maxLength Then
  ub += 1
  Redim Preserve maxIndex(1 To ub)
  maxIndex(ub) = startIndex
End If

Print Str(count); " words in the dictionary"
Print "The anagram set(s) with the greatest number of words (namely"; maxLength; ") is:"
Print
Dim iws(1 To maxLength) As IndexedWord  '' array to hold each anagram set
For i As Integer = 1 To UBound(maxIndex)
  For j As Integer = maxIndex(i) To maxIndex(i) + maxLength - 1
    iws(j - maxIndex(i) + 1) = iw(j)
  Next j
  sortIndexedWord iws()  '' sort anagram set before displaying it
  For j As Integer = 1 To maxLength
    Print w(iws(j).index); " ";
  Next j
  Print
Next i

Print
Print "Took ";
Print Using "#.###"; timer - t;
Print " seconds on i3 @ 2.13 GHz"

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

25104 words in the dictionary
The anagram set(s) with the greatest number of words (namely 5) is:

abel able bale bela elba
caret carte cater crate trace
angel angle galen glean lange
alger glare lager large regal
elan lane lean lena neal
evil levi live veil vile

Took 0.103 seconds on i3 @ 2.13 GHz

```



## Frink


```frink

d = new dict
for w = lines["http://www.puzzlers.org/pub/wordlists/unixdict.txt"]
{
   sorted = sort[charList[w]]
   curr = d@sorted
   if curr
      d@sorted.push[w]
   else
      d@sorted = [w]
}

most = sort[toArray[d], {|a,b| length[b@1] <=> length[a@1]}]
longest = length[most@0@1]

i = 0
while length[most@i@1] == longest
{
   println[most@i@1]
   i = i + 1
}

```



## FutureBasic

Applications in the latest versions of Macintosh OS X 10.x are sandboxed and require setting special permissions to link to internet files. For illustration purposes here, this code uses the internal Unix dictionary file available is all versions of OS X.

This first example is a hybrid using FB's native dynamic global array combined with Core Foundation functions:

```futurebasic

include "ConsoleWindow"

def tab 9

begin globals
dim dynamic gDictionary(_maxLong) as Str255
end globals

local fn IsAnagram( word1 as Str31, word2 as Str31 ) as Boolean
dim as long i, j, h, q
dim as Boolean result

if word1[0] != word2[0] then result = _false : exit fn

for i = 0 to word1[0]
  h = 0 : q = 0
  for j = 0 to word1[0]
    if word1[i] == word1[j] then h++
    if word1[i] == word2[j] then q++
  next
  if h != q then result = _false : exit fn
next
result = _true
end fn = result

local fn LoadDictionaryToArray
'~'1
dim as CFURLRef    url
dim as CFArrayRef  arr
dim as CFStringRef temp, cfStr
dim as CFIndex     elements
dim as Handle      h
dim as Str255      s
dim as long        fileLen, i

kill dynamic gDictionary
url = fn CFURLCreateWithFileSystemPath( _kCFAllocatorDefault, @"/usr/share/dict/words", _kCFURLPOSIXPathStyle, _false )
open "i", 2, url
  fileLen = lof(2, 1)
  h = fn NewHandleClear( fileLen )
    if ( h )
      read file 2, [h], fileLen
      cfStr = fn CFStringCreateWithBytes( _kCFAllocatorDefault, #[h], fn GetHandleSize(h), _kCFStringEncodingMacRoman, _false )
      if ( cfStr )
        arr = fn CFStringCreateArrayBySeparatingStrings( _kCFAllocatorDefault, cfStr, @"\n" )
        CFRelease( cfStr )
        elements = fn CFArrayGetCount( arr )
          for i = 0 to elements - 1
            temp = fn CFArrayGetValueAtIndex( arr, i )
            fn CFStringGetPascalString( temp, @s, 256, _kCFStringEncodingMacRoman )
            gDictionary(i) = s
          next
      CFRelease( arr )
      end if
    fn DisposeH( h )
   end if
close #2
CFRelease( url )
end fn

local fn FindAnagrams( whichWord as Str31 )
dim as long elements, i

print "Anagrams for "; UCase$(whichWord); ":",
elements = fn DynamicNextElement( dynamic( gDictionary ) )
for i = 0 to elements - 1
  if ( len( gDictionary(i) ) == whichWord[0] )
    if ( fn IsAnagram( whichWord, gDictionary(i) ) == _true )
      print gDictionary(i),
    end if
  end if
next
print
end fn

fn LoadDictionaryToArray

fn FindAnagrams( "bade"    )
fn FindAnagrams( "abet"    )
fn FindAnagrams( "beast"   )
fn FindAnagrams( "tuba"    )
fn FindAnagrams( "mace"    )
fn FindAnagrams( "scare"   )
fn FindAnagrams( "marine"  )
fn FindAnagrams( "antler"  )
fn FindAnagrams( "spare"   )
fn FindAnagrams( "leading" )
fn FindAnagrams( "alerted" )
fn FindAnagrams( "allergy" )
fn FindAnagrams( "research")
fn FindAnagrams( "hustle"  )
fn FindAnagrams( "oriental")
def tab 3
print
fn FindAnagrams( "creationism" )
fn FindAnagrams( "resistance"  )
fn FindAnagrams( "mountaineer" )

```

Output:

```txt

Anagrams for BADE:         abed     bade     bead
Anagrams for ABET:         abet     bate     beat     beta
Anagrams for BEAST:        baste    beast    tabes
Anagrams for TUBA:         abut     tabu     tuba
Anagrams for MACE:         acme     came     mace
Anagrams for SCARE:        carse    caser    ceras    scare    scrae
Anagrams for MARINE:       marine   remain
Anagrams for ANTLER:       altern   antler   learnt   rental   ternal
Anagrams for SPARE:        asper    parse    prase    spaer    spare    spear
Anagrams for LEADING:      adeling  dealing  leading
Anagrams for ALERTED:      delater  related  treadle
Anagrams for ALLERGY:      allergy  gallery  largely  regally
Anagrams for RESEARCH:     rechaser research searcher
Anagrams for HUSTLE:       hustle   sleuth
Anagrams for ORIENTAL:     oriental relation

Anagrams for CREATIONISM:  anisometric creationism miscreation ramisection reactionism
Anagrams for RESISTANCE:   resistance  senatrices
Anagrams for MOUNTAINEER:  enumeration mountaineer

```


This second example is pure Core Foundation:

```txt

include "ConsoleWindow"
include "Tlbx CFBag.incl"

local fn Dictionary as CFArrayRef
'~'1
dim as CFURLRef      url
dim as CFStringRef   string
dim as Handle        h
dim as long          fileLen

begin globals
dim as CFArrayRef sDictionary// static
end globals

if ( sDictionary == NULL )
url = fn CFURLCreateWithFileSystemPath( _kCFAllocatorDefault, @"/usr/share/dict/words", _kCFURLPOSIXPathStyle, _false )
open "i", 2, url
  fileLen = lof(2,1)
  h = fn NewHandleClear( fileLen )
    if ( h )
      read file 2, [h], fileLen
      string = fn CFStringCreateWithBytes( _kCFAllocatorDefault, #[h], fn GetHandleSize(h), _kCFStringEncodingMacRoman, _false )
        if ( string )
          sDictionary = fn CFStringCreateArrayBySeparatingStrings( _kCFAllocatorDefault, string, @"\n" )
          CFRelease( string )
       end if
       fn DisposeH( h )
    end if
close #2
CFRelease( url )
end if
end fn = sDictionary

local fn IsAnagram( wd1 as CFStringRef, wd2 as CFStringRef ) as Boolean
'~'1
dim as CFMutableBagRef   bag1, bag2
dim as CFStringRef       chr1, chr2
dim as CFIndex           length1, length2, i
dim as Boolean           result : result = _false

length1 = fn CFStringGetLength( wd1 )
length2 = fn CFStringGetLength( wd2 )
if ( length1 == length2 )
  bag1 = fn CFBagCreateMutable( _kCFAllocatorDefault, 0, @kCFCopyStringBagCallBacks )
  bag2 = fn CFBagCreateMutable( _kCFAllocatorDefault, 0, @kCFCopyStringBagCallBacks )

    for i = 0 to length1 - 1
      chr1 = fn CFStringCreateWithSubstring( _kCFAllocatorDefault, wd1, fn CFRangeMake(i,1) )
      chr2 = fn CFStringCreateWithSubstring( _kCFAllocatorDefault, wd2, fn CFRangeMake(i,1) )
      CFBagAddValue( bag1, chr1 )
      CFBagAddValue( bag2, chr2 )
      CFRelease( chr1 )
      CFRelease( chr2 )
    next

   result = fn CFEqual( bag1, bag2 )
   CFRelease( bag1 )
   CFRelease( bag2 )
end if
end fn = result

void local fn FindAnagrams( wd as CFStringRef )
'~'1
dim as CFMutableArrayRef    words
dim as CFMutableStringRef   wdUC
dim as CFLocaleRef          locale
dim as CFStringRef          string
dim as CFIndex              count, index
dim as CFArrayRef           dict

words = fn CFArrayCreateMutable( _kCFAllocatorDefault, 0, @kCFTypeArrayCallBacks )

wdUC = fn CFStringCreateMutableCopy( _kCFAllocatorDefault, 0, wd )
locale = fn CFLocaleCopyCurrent()
CFStringUppercase( wdUC, locale )
CFRelease( locale )

string = fn CFStringCreateWithFormat( _kCFAllocatorDefault, NULL, @"Anagrams for %@:", wdUC )
CFRelease( wdUC )
fn ConsolePrintCFString( string )
CFRelease( string )

dict = fn Dictionary()
count = fn CFArrayGetCount( dict )
for index = 0 to count - 1
  string = fn CFArrayGetValueAtIndex( dict, index )
     if ( fn IsAnagram( wd, string ) )
        CFArrayAppendValue( words, string )
     end if
next

string = fn CFStringCreateByCombiningStrings( _kCFAllocatorDefault, words, @", " )
CFRelease( words )
fn ConsolePrintCFString( string )
CFRelease( string )

fn ConsolePrintCFString( @"" )
end fn

fn FindAnagrams( @"bade" )
fn FindAnagrams( @"abet" )
fn FindAnagrams( @"beast" )
fn FindAnagrams( @"tuba" )
fn FindAnagrams( @"mace" )
fn FindAnagrams( @"scare" )
fn FindAnagrams( @"marine" )
fn FindAnagrams( @"antler")
fn FindAnagrams( @"spare" )
fn FindAnagrams( @"leading" )
fn FindAnagrams( @"alerted" )
fn FindAnagrams( @"allergy" )
fn FindAnagrams( @"research")
fn FindAnagrams( @"hustle" )
fn FindAnagrams( @"oriental")
fn FindAnagrams( @"creationism" )
fn FindAnagrams( @"resistance" )
fn FindAnagrams( @"mountaineer" )

```

Output:

```txt

Anagrams for BADE:
abed, bade, bead

Anagrams for ABET:
abet, bate, beat, beta

Anagrams for BEAST:
baste, beast, tabes

Anagrams for TUBA:
abut, tabu, tuba

Anagrams for MACE:
acme, came, mace

Anagrams for SCARE:
carse, caser, ceras, scare, scrae

Anagrams for MARINE:
marine, remain

Anagrams for ANTLER:
altern, antler, learnt, rental, ternal

Anagrams for SPARE:
asper, parse, prase, spaer, spare, spear

Anagrams for LEADING:
adeling, dealing, leading

Anagrams for ALERTED:
delater, related, treadle

Anagrams for ALLERGY:
allergy, gallery, largely, regally

Anagrams for RESEARCH:
rechaser, research, searcher

Anagrams for HUSTLE:
hustle, sleuth

Anagrams for ORIENTAL:
oriental, relation

Anagrams for CREATIONISM:
anisometric, creationism, miscreation, ramisection, reactionism

Anagrams for RESISTANCE:
resistance, senatrices

Anagrams for MOUNTAINEER:
enumeration, mountaineer

```



## GAP


```gap
Anagrams := function(name)
  local f, p, L, line, word, words, swords, res, cur, r;
  words := [ ];
  swords := [ ];
  f := InputTextFile(name);
  while true do
    line := ReadLine(f);
    if line = fail then
      break;
    else
      word := Chomp(line);
      Add(words, word);
      Add(swords, SortedList(word));
    fi;
  od;
  CloseStream(f);
  p := SortingPerm(swords);
  L := Permuted(words, p);
  r := "";
  cur := [ ];
  res := [ ];
  for word in L do
    if SortedList(word) = r then
      Add(cur, word);
    else
      if Length(cur) > 0 then
        Add(res, cur);
      fi;
      r := SortedList(word);
      cur := [ word ];
    fi;
  od;
  if Length(cur) > 0 then
    Add(res, cur);
  fi;
  return Filtered(res, v -> Length(v) > 1);
end;


ana := Anagrams("my/gap/unixdict.txt");;

# What is the longest anagram sequence ?
Maximum(List(ana, Length));
# 5

# Which are they ?
Filtered(ana, v -> Length(v) = 5);
# [ [ "abel", "able", "bale", "bela", "elba" ],
#   [ "caret", "carte", "cater", "crate", "trace" ],
#   [ "angel", "angle", "galen", "glean", "lange" ],
#   [ "alger", "glare", "lager", "large", "regal" ],
#   [ "elan", "lane", "lean", "lena", "neal" ],
#   [ "evil", "levi", "live", "veil", "vile" ] ]
```



## Go


```go
package main

import (
    "bytes"
    "fmt"
    "io/ioutil"
    "net/http"
    "sort"
)

func main() {
    r, err := http.Get("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
    if err != nil {
        fmt.Println(err)
        return
    }
    b, err := ioutil.ReadAll(r.Body)
    r.Body.Close()
    if err != nil {
        fmt.Println(err)
        return
    }
    var ma int
    var bs byteSlice
    m := make(map[string][][]byte)
    for _, word := range bytes.Fields(b) {
        bs = append(bs[:0], byteSlice(word)...)
        sort.Sort(bs)
        k := string(bs)
        a := append(m[k], word)
        if len(a) > ma {
            ma = len(a)
        }
        m[k] = a
    }
    for _, a := range m {
        if len(a) == ma {
            fmt.Printf("%s\n", a)
        }
    }
}

type byteSlice []byte

func (b byteSlice) Len() int           { return len(b) }
func (b byteSlice) Swap(i, j int)      { b[i], b[j] = b[j], b[i] }
func (b byteSlice) Less(i, j int) bool { return b[i] < b[j] }
```

{{out}}

```txt

[angel angle galen glean lange]
[elan lane lean lena neal]
[evil levi live veil vile]
[abel able bale bela elba]
[caret carte cater crate trace]
[alger glare lager large regal]

```


== {{header|Groovy}} ==
This program:

```groovy
def words = new URL('http://www.puzzlers.org/pub/wordlists/unixdict.txt').text.readLines()
def groups = words.groupBy{ it.toList().sort() }
def bigGroupSize = groups.collect{ it.value.size() }.max()
def isBigAnagram = { it.value.size() == bigGroupSize }
println groups.findAll(isBigAnagram).collect{ it.value }.collect{ it.join(' ') }.join('\n')
```

{{out}}

```txt

abel able bale bela elba
alger glare lager large regal
angel angle galen glean lange
caret carte cater crate trace
elan lane lean lena neal
evil levi live veil vile

```


== {{header|Haskell}} ==

```haskell
import Data.List

groupon f x y = f x == f y

main = do
  f <- readFile "./../Puzzels/Rosetta/unixdict.txt"
  let  words = lines f
       wix = groupBy (groupon fst) . sort $ zip (map sort words) words
       mxl = maximum $ map length wix
  mapM_ (print . map snd) . filter ((==mxl).length) $ wix
```

{{out}}

```haskell
*Main> main
["abel","able","bale","bela","elba"]
["caret","carte","cater","crate","trace"]
["angel","angle","galen","glean","lange"]
["alger","glare","lager","large","regal"]
["elan","lane","lean","lena","neal"]
["evil","levi","live","veil","vile"]
```


and we can noticeably speed up the second stage sorting and grouping by packing the String lists of Chars to the Text type:


```haskell
import Data.List (groupBy, maximumBy, sort)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Text (pack)

main :: IO ()
main = do
  f <- readFile "./unixdict.txt"
  let ws = groupBy (on (==) fst) (sort (((,) =<< pack . sort) <$> lines f))
  mapM_
    (print . fmap snd)
    (filter ((length (maximumBy (comparing length) ws) ==) . length) ws)
```

{{Out}}

```txt
["abel","able","bale","bela","elba"]
["caret","carte","cater","crate","trace"]
["angel","angle","galen","glean","lange"]
["alger","glare","lager","large","regal"]
["elan","lane","lean","lena","neal"]
["evil","levi","live","veil","vile"]
```


=={{header|Icon}} and {{header|Unicon}}==

```icon
procedure main(args)
    every writeSet(!getLongestAnagramSets())
end

procedure getLongestAnagramSets()
    wordSets := table()
    longestWSet := 0
    longSets := set()

    every word := !&input do {
        wChars := csort(word)
        /wordSets[wChars] := set()
        insert(wordSets[wChars], word)

        if 1 < *wordSets[wChars} == longestWSet then
            insert(longSets, wordSets[wChars])
        if 1 < *wordSets[wChars} > longestWSet then {
            longestWSet := *wordSets[wChars}
            longSets := set([wordSets[wChars]])
            }
        }

    return longSets
end

procedure writeSet(words)
    every writes("\t"|!words," ")
    write()
end

procedure csort(w)
    every (s := "") ||:= (find(c := !cset(w),w),c)
    return s
end
```

Sample run:

```txt
->an <unixdict.txt
         abel bale bela able elba
         lean neal elan lane lena
         angle galen lange angel glean
         alger glare lager large regal
         veil evil levi live vile
         caret cater crate carte trace
->
```


== {{header|J}} ==
If the unixdict file has been retrieved and saved in the current directory (for example, using wget):

```j
   (#~ a: ~: {:"1) (]/.~ /:~&>) <;._2 ] 1!:1 <'unixdict.txt'
+-----+-----+-----+-----+-----+
|abel |able |bale |bela |elba |
+-----+-----+-----+-----+-----+
|alger|glare|lager|large|regal|
+-----+-----+-----+-----+-----+
|angel|angle|galen|glean|lange|
+-----+-----+-----+-----+-----+
|caret|carte|cater|crate|trace|
+-----+-----+-----+-----+-----+
|elan |lane |lean |lena |neal |
+-----+-----+-----+-----+-----+
|evil |levi |live |veil |vile |
+-----+-----+-----+-----+-----+
```

Explanation:

```J
   <;._2 ] 1!:1 <'unixdict.txt'
```

This reads in the dictionary and produces a list of boxes.  Each box contains one line (one word) from the dictionary.

```J
   (]/.~ /:~&>)
```

This groups the words into rows where anagram equivalents appear in the same row.  In other words, creates a copy of the original list where the characters contained in each box have been sorted.  Then it organizes the contents of the original list in rows, with each new row keyed by the values in the new list.

```J
   (#~ a: ~: {:"1)
```

This selects rows whose last element is not an empty box.

(In the previous step we created an array of rows of boxes.  The short rows were automatically padded with empty boxes so that all rows would be the same length.)


## Java

The key to this algorithm is the sorting of the characters in each word from the dictionary. The line <tt>Arrays.sort(chars);</tt> sorts all of the letters in the word in ascending order using a built-in [[quicksort]], so all of the words in the first group in the result end up under the key "aegln" in the anagrams map.
{{works with|Java|1.5+}}

```java5
import java.net.*;
import java.io.*;
import java.util.*;

public class WordsOfEqChars {
    public static void main(String[] args) throws IOException {
        URL url = new URL("http://www.puzzlers.org/pub/wordlists/unixdict.txt");
        InputStreamReader isr = new InputStreamReader(url.openStream());
        BufferedReader reader = new BufferedReader(isr);

        Map<String, Collection<String>> anagrams = new HashMap<String, Collection<String>>();
        String word;
        int count = 0;
        while ((word = reader.readLine()) != null) {
            char[] chars = word.toCharArray();
            Arrays.sort(chars);
            String key = new String(chars);
            if (!anagrams.containsKey(key))
                anagrams.put(key, new ArrayList<String>());
            anagrams.get(key).add(word);
            count = Math.max(count, anagrams.get(key).size());
        }

        reader.close();

        for (Collection<String> ana : anagrams.values())
            if (ana.size() >= count)
                System.out.println(ana);
    }
}
```

{{works with|Java|1.8+}}

```java5
import java.net.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;

public interface Anagram {
  public static <AUTOCLOSEABLE extends AutoCloseable, OUTPUT> Supplier<OUTPUT> tryWithResources(Callable<AUTOCLOSEABLE> callable, Function<AUTOCLOSEABLE, Supplier<OUTPUT>> function, Supplier<OUTPUT> defaultSupplier) {
    return () -> {
      try (AUTOCLOSEABLE autoCloseable = callable.call()) {
        return function.apply(autoCloseable).get();
      } catch (Throwable throwable) {
        return defaultSupplier.get();
      }
    };
  }

  public static <INPUT, OUTPUT> Function<INPUT, OUTPUT> function(Supplier<OUTPUT> supplier) {
    return i -> supplier.get();
  }

  public static void main(String... args) {
    Map<String, Collection<String>> anagrams = new ConcurrentSkipListMap<>();
    int count = tryWithResources(
      () -> new BufferedReader(
        new InputStreamReader(
          new URL(
            "http://www.puzzlers.org/pub/wordlists/unixdict.txt"
          ).openStream()
        )
      ),
      reader -> () -> reader.lines()
        .parallel()
        .mapToInt(word -> {
          char[] chars = word.toCharArray();
          Arrays.parallelSort(chars);
          String key = Arrays.toString(chars);
          Collection<String> collection = anagrams.computeIfAbsent(
            key, function(ArrayList::new)
          );
          collection.add(word);
          return collection.size();
        })
        .max()
        .orElse(0),
      () -> 0
    ).get();
    anagrams.values().stream()
      .filter(ana -> ana.size() >= count)
      .forEach(System.out::println)
    ;
  }
}
```

{{out}}
 [angel, angle, galen, glean, lange]
 [elan, lane, lean, lena, neal]
 [alger, glare, lager, large, regal]
 [abel, able, bale, bela, elba]
 [evil, levi, live, veil, vile]
 [caret, carte, cater, crate, trace]


## JavaScript

{{Works with|Node.js}}

```javascript
var fs = require('fs');
var words = fs.readFileSync('unixdict.txt', 'UTF-8').split('\n');

var i, item, max = 0,
    anagrams = {};

for (i = 0; i < words.length; i += 1) {
  var key = words[i].split('').sort().join('');
  if (!anagrams.hasOwnProperty(key)) {//check if property exists on current obj only
      anagrams[key] = [];
  }
  var count = anagrams[key].push(words[i]); //push returns new array length
  max = Math.max(count, max);
}

//note, this returns all arrays that match the maximum length
for (item in anagrams) {
  if (anagrams.hasOwnProperty(item)) {//check if property exists on current obj only
    if (anagrams[item].length === max) {
        console.log(anagrams[item].join(' '));
    }
  }
}
```


{{Out}}

```txt
[ 'abel', 'able', 'bale', 'bela', 'elba' ]
[ 'alger', 'glare', 'lager', 'large', 'regal' ]
[ 'angel', 'angle', 'galen', 'glean', 'lange' ]
[ 'caret', 'carte', 'cater', 'crate', 'trace' ]
[ 'elan', 'lane', 'lean', 'lena', 'neal' ]
[ 'evil', 'levi', 'live', 'veil', 'vile' ]
```



### Alternative Using Reduce


```javascript
var fs = require('fs');
var dictionary = fs.readFileSync('unixdict.txt', 'UTF-8').split('\n');

//group anagrams
var sortedDict = dictionary.reduce(function (acc, word) {
  var sortedLetters = word.split('').sort().join('');
  if (acc[sortedLetters] === undefined) { acc[sortedLetters] = []; }
  acc[sortedLetters].push(word);
  return acc;
}, {});

//sort list by frequency
var keysSortedByFrequency = Object.keys(sortedDict).sort(function (keyA, keyB) {
  if (sortedDict[keyA].length < sortedDict[keyB].length) { return 1; }
  if (sortedDict[keyA].length > sortedDict[keyB].length) { return -1; }
  return 0;
});

//print first 10 anagrams by frequency
keysSortedByFrequency.slice(0, 10).forEach(function (key) {
  console.log(sortedDict[key].join(' '));
});
```



## jq


```jq
def anagrams:
  (reduce .[] as $word (
      {table: {}, max: 0};   # state
      ($word | explode | sort | implode) as $hash
      | .table[$hash] += [ $word ]
      | .max   = ([ .max, ( .table[$hash] | length) ] | max ) ))
  | .max as $max
  | .table | .[] | select(length == $max) ;

# The task:
split("\n") | anagrams

```

{{Out}}

```sh

$ jq -M -s -c -R -f anagrams.jq unixdict.txt
["abel","able","bale","bela","elba"]
["alger","glare","lager","large","regal"]
["angel","angle","galen","glean","lange"]
["caret","carte","cater","crate","trace"]
["elan","lane","lean","lena","neal"]
["evil","levi","live","veil","vile"]

```



## Jsish

From Javascript, nodejs entry.

```javascript
/* Anagrams, in Jsish */
var datafile = 'unixdict.txt';
if (console.args[0] == '-more' && Interp.conf('maxArrayList') > 500000)
    datafile = '/usr/share/dict/words';

var words = File.read(datafile).split('\n');
puts(words.length, 'words');

var i, item, max = 0, anagrams = {};

for (i = 0; i < words.length; i += 1) {
    var key = words[i].split('').sort().join('');
    if (!anagrams.hasOwnProperty(key)) {
        anagrams[key] = [];
    }
    var count = anagrams[key].push(words[i]);
    max = Math.max(count, max);
}

// display all arrays that match the maximum length
for (item in anagrams) {
    if (anagrams.hasOwnProperty(item)) {
        if (anagrams[item].length === max) {
            puts(anagrams[item].join(' '));
        }
    }
}

/*
=!EXPECTSTART!=
25108 words
abel able bale bela elba
caret carte cater crate trace
angel angle galen glean lange
alger glare lager large regal
elan lane lean lena neal
evil levi live veil vile
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish -u anagrams.jsi
[PASS] anagrams.jsi
```


To update the script to pass with a site local words file, just '''jsish -u -update true anagrams.jsi'''.


## Julia

{{works with|Julia|0.6}}

```julia
url = "http://www.puzzlers.org/pub/wordlists/unixdict.txt"
wordlist = open(readlines, download(url))

wsort(word::AbstractString) = join(sort(collect(word)))

function anagram(wordlist::Vector{<:AbstractString})
    dict = Dict{String, Set{String}}()
    for word in wordlist
        sorted = wsort(word)
        push!(get!(dict, sorted, Set{String}()), word)
    end
    wcnt = maximum(length, values(dict))
    return collect(Iterators.filter((y) -> length(y) == wcnt, values(dict)))
end

println.(anagram(wordlist))
```


{{out}}

```txt
Set(String["live", "vile", "veil", "evil", "levi"])
Set(String["abel", "able", "bale", "bela", "elba"])
Set(String["crate", "cater", "carte", "trace", "caret"])
Set(String["galen", "angel", "lange", "angle", "glean"])
Set(String["lager", "regal", "glare", "large", "alger"])
Set(String["neal", "elan", "lena", "lane", "lean"])
```



## K


```k
{x@&a=|/a:#:'x}{x g@&1<#:'g:={x@<x}'x}0::`unixdict.txt
```



## Kotlin

{{trans|Java}}

```scala
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.URL

fun main(args: Array<String>) {
    val url = URL("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
    val isr = InputStreamReader(url.openStream())
    val reader = BufferedReader(isr)
    val anagrams = mutableMapOf<String, MutableList<String>>()
    var count = 0
    var word = reader.readLine()
    while (word != null) {
        val chars = word.toCharArray()
        chars.sort()
        val key = chars.joinToString("")
        if (!anagrams.containsKey(key)) anagrams.put(key, mutableListOf())
        anagrams[key]!!.add(word)
        count = Math.max(count, anagrams[key]!!.size)
        word = reader.readLine()
    }
    reader.close()
    anagrams.values
        .filter { it.size == count }
        .forEach { println(it) }
}
```


{{out}}

```txt

[abel, able, bale, bela, elba]
[alger, glare, lager, large, regal]
[angel, angle, galen, glean, lange]
[caret, carte, cater, crate, trace]
[elan, lane, lean, lena, neal]
[evil, levi, live, veil, vile]

```



## Lasso


```lasso
local(
	anagrams	= map,
	words		= include_url('http://www.puzzlers.org/pub/wordlists/unixdict.txt')->split('\n'),
	key,
	max		= 0,
	findings	= array
)

with word in #words do {
	#key = #word -> split('') -> sort& -> join('')
	if(not(#anagrams >> #key)) => {
		#anagrams -> insert(#key = array)
	}
	#anagrams -> find(#key) -> insert(#word)
}
with ana in #anagrams
let ana_size = #ana -> size
do {
	if(#ana_size > #max) => {
		#findings = array(#ana -> join(', '))
		#max = #ana_size
	else(#ana_size == #max)
		#findings -> insert(#ana -> join(', '))
	}
}

#findings -> join('<br />\n')

```

{{out}}

```txt
abel, able, bale, bela, elba
caret, carte, cater, crate, trace
angel, angle, galen, glean, lange
alger, glare, lager, large, regal
elan, lane, lean, lena, neal
evil, levi, live, veil, vile
```



## Liberty BASIC


```lb
' count the word list
open "unixdict.txt" for input as #1
while not(eof(#1))
    line input #1,null$
    numWords=numWords+1
wend
close #1

'import to an array appending sorted letter set
open "unixdict.txt" for input as #1
dim wordList$(numWords,3)
dim chrSort$(45)
wordNum=1
while wordNum<numWords
    line input #1,actualWord$
    wordList$(wordNum,1)=actualWord$
    wordList$(wordNum,2)=sorted$(actualWord$)
    wordNum=wordNum+1
wend

'sort on letter set
sort wordList$(),1,numWords,2

'count and store number of anagrams found
wordNum=1
startPosition=wordNum
numAnagrams=0
currentChrSet$=wordList$(wordNum,2)
while wordNum < numWords
    while currentChrSet$=wordList$(wordNum,2)
        numAnagrams=numAnagrams+1
        wordNum=wordNum+1
    wend
    for n= startPosition to startPosition+numAnagrams
        wordList$(n,3)=right$("0000"+str$(numAnagrams),4)+wordList$(n,2)
    next
    startPosition=wordNum
    numAnagrams=0
    currentChrSet$=wordList$(wordNum,2)
wend

'sort on number of anagrams+letter set
sort wordList$(),numWords,1,3

'display the top anagram sets found
wordNum=1
while wordNum<150
    currentChrSet$=wordList$(wordNum,2)
    print "Anagram set";
    while currentChrSet$=wordList$(wordNum,2)
        print " : ";wordList$(wordNum,1);
        wordNum=wordNum+1
    wend
    print
    currentChrSet$=wordList$(wordNum,2)
wend

close #1
end

function sorted$(w$)
    nchr=len(w$)
    for chr = 1 to nchr
        chrSort$(chr)=mid$(w$,chr,1)
    next
    sort chrSort$(),1,nchr
    sorted$=""
    for chr = 1 to nchr
        sorted$=sorted$+chrSort$(chr)
    next
end function
```



## LiveCode

LiveCode could definitely use a sort characters command. As it is this code converts the letters into items and then sorts that. I wrote a merge sort for characters, but the conversion to items, built-in-sort, conversion back to string is about 10% faster, and certainly easier to write.


```LiveCode
on mouseUp
   put mostCommonAnagrams(url "http://www.puzzlers.org/pub/wordlists/unixdict.txt")
end mouseUp

function mostCommonAnagrams X
   put 0 into maxCount
   repeat for each word W in X
      get sortChars(W)
      put W & comma after A[it]
      add 1 to C[it]
      if C[it] >= maxCount then
         if C[it] > maxCount then
            put C[it] into maxCount
            put char 1 to -2 of A[it] into winnerList
         else
            put cr & char 1 to -2 of A[it] after winnerList
         end if
      end if
   end repeat
   return winnerList
end mostCommonAnagrams

function sortChars X
   get charsToItems(X)
   sort items of it
   return itemsToChars(it)
end sortChars

function charsToItems X
   repeat for each char C in X
      put C & comma after R
   end repeat
   return char 1 to -2 of R
end charsToItems

function itemsToChars X
   replace comma with empty in X
   return X
end itemsToChars
```

{{out}}

```txt
abel,able,bale,bela,elba
angel,angle,galen,glean,lange
elan,lane,lean,lena,neal
alger,glare,lager,large,regal
caret,carte,cater,crate,trace
evil,levi,live,veil,vile
```



## Lua

Lua's core library is very small and does not include built-in network functionality. If a networking library were imported, the local file in the following script could be replaced with the remote dictionary file.

```lua
function sort(word)
  local bytes = {word:byte(1, -1)}
  table.sort(bytes)
  return string.char(unpack(bytes))
end

-- Read in and organize the words.
-- word_sets[<alphabetized_letter_list>] = {<words_with_those_letters>}
local word_sets = {}
local max_size = 0
for word in io.lines('unixdict.txt') do
  local key = sort(word)
  if word_sets[key] == nil then word_sets[key] = {} end
  table.insert(word_sets[key], word)
  max_size = math.max(max_size, #word_sets[key])
end

-- Print out the answer sets.
for _, word_set in pairs(word_sets) do
  if #word_set == max_size then
    for _, word in pairs(word_set) do io.write(word .. ' ') end
    print('')  -- Finish with a newline.
  end
end
```

{{out}}

```txt
abel able bale bela elba
evil levi live veil vile
alger glare lager large regal
angel angle galen glean lange
caret carte cater crate trace
elan lane lean lena neal
```



## M4


```M4
divert(-1)
changequote(`[',`]')
define([for],
   [ifelse($#,0,[[$0]],
   [ifelse(eval($2<=$3),1,
   [pushdef([$1],$2)$4[]popdef([$1])$0([$1],incr($2),$3,[$4])])])])
define([_bar],include(t.txt))
define([eachlineA],
   [ifelse(eval($2>0),1,
      [$3(substr([$1],0,$2))[]eachline(substr([$1],incr($2)),[$3])])])
define([eachline],[eachlineA([$1],index($1,[
]),[$2])])
define([removefirst],
   [substr([$1],0,$2)[]substr([$1],incr($2))])
define([checkfirst],
   [ifelse(eval(index([$2],substr([$1],0,1))<0),1,
      0,
      [ispermutation(substr([$1],1),
            removefirst([$2],index([$2],substr([$1],0,1))))])])
define([ispermutation],
   [ifelse([$1],[$2],1,
      eval(len([$1])!=len([$2])),1,0,
      len([$1]),0,0,
      [checkfirst([$1],[$2])])])
define([_set],[define($1<$2>,$3)])
define([_get],[defn([$1<$2>])])
define([_max],1)
define([_n],0)
define([matchj],
   [_set([count],$2,incr(_get([count],$2)))[]ifelse(eval(_get([count],$2)>_max),
         1,[define([_max],incr(_max))])[]_set([list],$2,[_get([list],$2) $1])])
define([checkwordj],
   [ifelse(ispermutation([$1],_get([word],$2)),1,[matchj([$1],$2)],
         [addwordj([$1],incr($2))])])
define([_append],
   [_set([word],_n,[$1])[]_set([count],_n,1)[]_set([list],_n,
         [$1 ])[]define([_n],incr(_n))])
define([addwordj],
   [ifelse($2,_n,[_append([$1])],[checkwordj([$1],$2)])])
define([addword],
   [addwordj([$1],0)])
divert
eachline(_bar,[addword])
_max
for([x],1,_n,[ifelse(_get([count],x),_max,[_get([list],x)
])])
```


Memory limitations keep this program from working on the full-sized dictionary.
{{out}} (using only the first 100 words as input)

```txt

2
abel  able
aboard  abroad

```



## Maple

The first line downloads the specified dictionary.
(You could, instead, read it from a file, or use one of Maple's built-in word lists.)
Next, turn it into a list of words.
The assignment to T is where the real work is done (via Classify, in the ListTools package).
This creates sets of words all of which have the same "hash", which is, in this case, the sorted word.
The convert call discards the hashes, which have done their job, and leaves us with a list L of anagram sets.
Finally, we just note the size of the largest sets of anagrams, and pick those off.

```Maple

words := HTTP:-Get( "http://www.puzzlers.org/pub/wordlists/unixdict.txt" )[2]: # ignore errors
use StringTools, ListTools in
  T := Classify( Sort, map( Trim, Split( words ) ) )
end use:
L := convert( T, 'list' ):
m := max( map( nops, L ) ); # what is the largest set?
A := select( s -> evalb( nops( s ) = m ), L ); # get the maximal sets of anagrams

```

The result of running this code is

```Maple

A := [{"abel", "able", "bale", "bela", "elba"}, {"angel", "angle", "galen",
"glean", "lange"}, {"alger", "glare", "lager", "large", "regal"}, {"evil",
"levi", "live", "veil", "vile"}, {"caret", "carte", "cater", "crate", "trace"}
, {"elan", "lane", "lean", "lena", "neal"}];

```



## Mathematica

Download the dictionary, split the lines, split the word in characters and sort them. Now sort by those words, and find sequences of equal 'letter-hashes'. Return the longest sequences:

```Mathematica
list=Import["http://www.puzzlers.org/pub/wordlists/unixdict.txt","Lines"];
text={#,StringJoin@@Sort[Characters[#]]}&/@list;
text=SortBy[text,#[[2]]&];
splits=Split[text,#1[[2]]==#2[[2]]&][[All,All,1]];
maxlen=Max[Length/@splits];
Select[splits,Length[#]==maxlen&]
```

gives back:

```Mathematica
{{abel,able,bale,bela,elba},{caret,carte,cater,crate,trace},{angel,angle,galen,glean,lange},{alger,glare,lager,large,regal},{elan,lane,lean,lena,neal},{evil,levi,live,veil,vile}}
```

An alternative is faster, but requires version 7 (for <code>Gather</code>):

```Mathematica
splits = Gather[list, Sort[Characters[#]] == Sort[Characters[#2]] &];
maxlen = Max[Length /@ splits];
Select[splits, Length[#] == maxlen &]
```


Or using build-in functions for sorting and gathering elements in lists it can be implimented as:

```Mathematica
anagramGroups = GatherBy[SortBy[GatherBy[list,Sort[Characters[#]] &],Length],Length];
anagramGroups[[-1]]
```

Also, Mathematica's own word list is available; replacing the list definition with <code>list = WordData[];</code> and forcing <code>maxlen</code> to 5 yields instead this result:

 {{angered,derange,enraged,grandee,grenade},
  {anisometric,creationism,miscreation,reactionism,romanticise},
  {aper,pare,pear,rape,reap},
  {ardeb,barde,bared,beard,bread,debar},
  {aril,lair,lari,liar,lira,rail,rial},
  {aster,rates,stare,tears,teras},
  {caret,carte,cater,crate,react,trace},
  {east,eats,sate,seat,seta},
  {ester,reset,steer,teres,terse},
  {inert,inter,niter,nitre,trine},
  {latrine,ratline,reliant,retinal,trenail},
  {least,slate,stale,steal,stela,tesla},
  {luster,lustre,result,rustle,sutler,ulster},
  {merit,miter,mitre,remit,timer},
  {part,prat,rapt,tarp,trap},
  {resin,rinse,risen,serin,siren},
  {respect,scepter,sceptre,specter,spectre}}

Also if using Mathematica 10 it gets really concise:

```Mathematica
list=Import["http://www.puzzlers.org/pub/wordlists/unixdict.txt","Lines"];
MaximalBy[GatherBy[list, Sort@*Characters], Length]
```



## Maxima


```maxima
read_file(name) := block([file, s, L], file: openr(name), L: [],
while stringp(s: readline(file)) do L: cons(s, L), close(file), L)$

u: read_file("C:/my/mxm/unixdict.txt")$

v: map(lambda([s], [ssort(s), s]), u)$

w: sort(v, lambda([x, y], orderlessp(x[1], y[1])))$

ana(L) := block([m, n, p, r, u, v, w],
L: endcons(["", ""], L),
n: length(L),
r: "",
m: 0,
v: [ ],
w: [ ],
for i from 1 thru n do (
   u: L[i],
   if r = u[1] then (
      w: cons(u[2], w)
   ) else (
      p: length(w),
      if p >= m then (
         if p > m then (m: p, v: []),
         v: cons(w, v)
      ),
      w: [u[2]],
      r: u[1]
   )
),
v)$

ana(w);
/* [["evil", "levi", "live", "veil", "vile"],
    ["elan", "lane", "lean", "lena", "neal"],
    ["alger", "glare", "lager", "large", "regal"],
    ["angel", "angle", "galen", "glean", "lange"],
    ["caret", "carte", "cater", "crate", "trace"],
    ["abel", "able", "bale", "bela", "elba"]] */
```



## MUMPS


```MUMPS
Anagrams	New ii,file,longest,most,sorted,word
	Set file="unixdict.txt"
	Open file:"r" Use file
	For  Quit:$ZEOF  DO
	. New char,sort
	. Read word Quit:word=""
	. For ii=1:1:$Length(word) Do
	. . Set char=$ASCII(word,ii)
	. . If char>64,char<91 Set char=char+32
	. . Set sort(char)=$Get(sort(char))+1
	. . Quit
	. Set (sorted,char)="" For  Set char=$Order(sort(char)) Quit:char=""  Do
	. . For ii=1:1:sort(char) Set sorted=sorted_$Char(char)
	. . Quit
	. Set table(sorted,word)=1
	. Quit
	Close file
	Set sorted="" For  Set sorted=$Order(table(sorted)) Quit:sorted=""  Do
	. Set ii=0,word="" For  Set word=$Order(table(sorted,word)) Quit:word=""  Set ii=ii+1
	. Quit:ii<2
	. Set most(ii,sorted)=1
	. Quit
	Write !,"The anagrams with the most variations:"
	Set ii=$Order(most(""),-1)
	Set sorted="" For  Set sorted=$Order(most(ii,sorted)) Quit:sorted=""  Do
	. Write ! Set word="" For  Set word=$Order(table(sorted,word)) Quit:word=""  Write "  ",word
	. Quit
	Write !,"The longest anagrams:"
	Set ii=$Order(longest(""),-1)
	Set sorted="" For  Set sorted=$Order(longest(ii,sorted)) Quit:sorted=""  Do
	. Write ! Set word="" For  Set word=$Order(table(sorted,word)) Quit:word=""  Write "  ",word
	. Quit
	Quit

Do Anagrams
```


```txt

The anagrams with the most variations:
  abel  able  bale  bela  elba
  caret  carte  cater  crate  trace
  angel  angle  galen  glean  lange
  alger  glare  lager  large  regal
  elan  lane  lean  lena  neal
  evil  levi  live  veil  vile
The longest anagrams:
  conservation  conversation

```



## NetRexx

===Java&ndash;Like===
{{trans|Java}}

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

class RAnagramsV01 public

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method runSample(arg) public signals MalformedURLException, IOException
    parse arg localFile .
    isr = Reader
    if localFile = '' then do
      durl = URL("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
      dictFrom = durl.toString()
      isr = InputStreamReader(durl.openStream())
      end
    else do
      dictFrom = localFile
      isr = FileReader(localFile)
      end
    say 'Searching' dictFrom 'for anagrams'
    dictionaryReader = BufferedReader(isr)

    anagrams = Map HashMap()
    aWord = String
    count = 0
    loop label w_ forever
      aWord = dictionaryReader.readLine()
      if aWord = null then leave w_
      chars = aWord.toCharArray()
      Arrays.sort(chars)
      key = String(chars)
      if (\anagrams.containsKey(key)) then do
        anagrams.put(key, ArrayList())
        end
      (ArrayList anagrams.get(key)).add(Object aWord)
      count = Math.max(count, (ArrayList anagrams.get(key)).size())
      end w_
    dictionaryReader.close

    ani = anagrams.values().iterator()
    loop label a_ while ani.hasNext()
      ana = ani.next()
      if (ArrayList ana).size() >= count then do
        say ana
        end
      end a_

    return

  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  method main(args = String[]) public static

    arg = Rexx(args)
    Do
      ra = RAnagramsV01()
      ra.runSample(arg)
    Catch ex = Exception
      ex.printStackTrace()
    End

    return

```

{{out}}

```txt

Searching http://www.puzzlers.org/pub/wordlists/unixdict.txt for anagrams
[abel, able, bale, bela, elba]
[elan, lane, lean, lena, neal]
[evil, levi, live, veil, vile]
[angel, angle, galen, glean, lange]
[alger, glare, lager, large, regal]
[caret, carte, cater, crate, trace]

```


===Rexx&ndash;Like===
Implemented with more NetRexx idioms such as indexed strings, <tt>PARSE</tt> and the NetRexx &quot;built&ndash;in functions&quot;.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method findMostAnagrams(arg) public static signals MalformedURLException, IOException
  parse arg localFile .
  isr = Reader
  if localFile = '' then do
    durl = URL("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
    dictFrom = durl.toString()
    isr = InputStreamReader(durl.openStream())
    end
  else do
    dictFrom = localFile
    isr = FileReader(localFile)
    end
  say 'Searching' dictFrom 'for anagrams'
  dictionaryReader = BufferedReader(isr)

  anagrams = 0
  maxWords = 0
  loop label w_ forever
    aWord = dictionaryReader.readLine()
    if aWord = null then leave w_
    chars = aWord.toCharArray()
    Arrays.sort(chars)
    key = Rexx(chars)
    parse anagrams[key] count aWords
    aWords = (aWords aWord).space()
    maxWords = maxWords.max(aWords.words())
    anagrams[key] = aWords.words() aWords
    end w_
  dictionaryReader.close

  loop key over anagrams
    parse anagrams[key] count aWords
    if count >= maxWords then
      say aWords
    else
      anagrams[key] = null  -- remove unwanted elements from the indexed string
    end key

  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static

  Do
    findMostAnagrams(arg)
  Catch ex = Exception
    ex.printStackTrace()
  End

  Return

```

{{out}}

```txt

Searching http://www.puzzlers.org/pub/wordlists/unixdict.txt for anagrams
abel able bale bela elba
elan lane lean lena neal
evil levi live veil vile
angel angle galen glean lange
alger glare lager large regal
caret carte cater crate trace

```



## Nim


```nim

import tables, strutils, algorithm

proc main() =
    var
        count    = 0
        anagrams = initTable[string, seq[string]]()

    for word in "unixdict.txt".lines():
        var key = word
        key.sort(cmp[char])
        anagrams.mgetOrPut(key, newSeq[string]()).add(word)
        count = max(count, anagrams[key].len)

    for _, v in anagrams:
        if v.len == count:
            v.join(" ").echo

main()

```

{{out}}

```txt

evil levi live veil vile
caret carte cater crate trace
elan lane lean lena neal
alger glare lager large regal
abel able bale bela elba
angel angle galen glean lange

```


=={{header|Oberon-2}}==
Oxford Oberon-2

```oberon2

MODULE Anagrams;
IMPORT Files,Out,In,Strings;
CONST
	MAXPOOLSZ = 1024;

TYPE
	String = ARRAY 80 OF CHAR;

	Node = POINTER TO NodeDesc;
	NodeDesc = RECORD;
		count: INTEGER;
		word: String;
		desc: Node;
		next: Node;
	END;

	Pool = POINTER TO PoolDesc;
	PoolDesc = RECORD
		capacity,max: INTEGER;
		words: POINTER TO ARRAY OF Node;
	END;

	PROCEDURE InitNode(n: Node);
	BEGIN
		n^.count := 0;
		n^.word := "";
		n^.desc := NIL;
		n^.next := NIL;
	END InitNode;

	PROCEDURE Index(s: ARRAY OF CHAR;cap: INTEGER): INTEGER;
	VAR
		i,sum: INTEGER;
	BEGIN
		sum := 0;
		FOR i := 0 TO  Strings.Length(s) DO
			INC(sum,ORD(s[i]))
		END;
		RETURN sum MOD cap
	END Index;

	PROCEDURE ISort(VAR s: ARRAY OF CHAR);
	VAR
        i, j: INTEGER;
        t: CHAR;
	BEGIN
        FOR i := 0 TO Strings.Length(s) - 1 DO
			j := i;
			t := s[j];
			WHILE (j > 0) & (s[j -1] > t) DO
					s[j] := s[j - 1];
					DEC(j)
			END;
			s[j] := t
        END
	END ISort;

	PROCEDURE SameLetters(x,y: ARRAY OF CHAR): BOOLEAN;
	BEGIN
        ISort(x);ISort(y);
        RETURN (Strings.Compare(x,y) = 0)
	END SameLetters;

	PROCEDURE InitPool(p:Pool);
	BEGIN
		InitPoolWith(p,MAXPOOLSZ);
	END InitPool;

	PROCEDURE InitPoolWith(p:Pool;cap: INTEGER);
	VAR
		i: INTEGER;
	BEGIN
		p^.capacity := cap;
		p^.max := 0;
		NEW(p^.words,cap);
		i := 0;
		WHILE i < p^.capacity DO
			p^.words^[i] := NIL;
			INC(i);
		END;
	END InitPoolWith;

	PROCEDURE (p: Pool) Add(w: ARRAY OF CHAR);
	VAR
		idx: INTEGER;
		iter,n: Node;
	BEGIN
		idx := Index(w,p^.capacity);
		iter := p^.words^[idx];
		NEW(n);InitNode(n);COPY(w,n^.word);
		WHILE(iter # NIL) DO
			IF SameLetters(w,iter^.word) THEN
				INC(iter^.count);
				IF iter^.count > p^.max THEN p^.max := iter^.count END;
				n^.desc := iter^.desc;
				iter^.desc := n;
				RETURN
			END;
			iter := iter^.next
		END;
		ASSERT(iter = NIL);
		n^.next := p^.words^[idx];p^.words^[idx] := n
	END Add;

	PROCEDURE ShowAnagrams(l: Node);
	VAR
		iter: Node;
	BEGIN
		iter := l;
		WHILE iter # NIL DO
			Out.String(iter^.word);Out.String(" ");
			iter := iter^.desc
		END;
		Out.Ln
	END ShowAnagrams;

	PROCEDURE (p: Pool) ShowMax();
	VAR
		i: INTEGER;
		iter: Node;
	BEGIN
		FOR i := 0 TO LEN(p^.words^) - 1 DO
			IF p^.words^[i] # NIL THEN
				iter := p^.words^[i];
				WHILE iter # NIL DO
					IF iter^.count = p^.max THEN
						ShowAnagrams(iter);
					END;
					iter := iter^.next
				END
			END
		END
	END ShowMax;

	PROCEDURE DoProcess(fnm: ARRAY OF CHAR);
	VAR
		stdinBck,istream: Files.File;
		line: String;
		p: Pool;
	BEGIN
		istream := Files.Open(fnm,"r");
		stdinBck := Files.stdin;
		Files.stdin := istream;
		NEW(p);InitPool(p);
		WHILE In.Done DO
			In.Line(line);
			p.Add(line);
		END;
		Files.stdin := stdinBck;
		Files.Close(istream);
		p^.ShowMax();
	END DoProcess;

BEGIN
	DoProcess("unixdict.txt");
END Anagrams.

```

{{out}}

```txt

abel elba bela bale able
elan neal lena lean lane
evil vile veil live levi
angel lange glean galen angle
alger regal large lager glare
caret trace crate cater carte

```



## Objeck


```objeck
use HTTP;
use Collection;

class Anagrams {
  function : Main(args : String[]) ~ Nil {
    lines := HttpClient->New()->Get("http://www.puzzlers.org/pub/wordlists/unixdict.txt");
    anagrams := StringMap->New();
    count := 0;
    if(lines->Size() = 1) {
      line := lines->Get(0)->As(String);
      words := line->Split("\n");
      each(i : words) {
        word := words[i]->Trim();
        key := String->New(word->ToCharArray()->Sort());
        list := anagrams->Find(key)->As(Vector);
        if(list = Nil) {
          list := Vector->New();
          anagrams->Insert(key, list);
        };
        list->AddBack(word);
        count := count->Max(list->Size());
      };

      lists := anagrams->GetValues();
      each(i : lists) {
        list := lists->Get(i)->As(Vector);
        if(list->Size() >= count) {
          '['->Print();
          each(j : list) {
            list->Get(j)->As(String)->Print();
            if(j + 1 < list->Size()) {
              ','->Print();
            };
          };
          ']'->PrintLine();
        };
      };
    };
  }
}

```

{{out}}

```txt
[abel,able,bale,bela,elba]
[caret,carte,cater,crate,trace]
[angel,angle,galen,glean,lange]
[alger,glare,lager,large,regal]
[elan,lane,lean,lena,neal]
[evil,levi,live,veil,vile]
```



## OCaml


```ocaml
let explode str =
  let l = ref [] in
  let n = String.length str in
  for i = n - 1 downto 0 do
    l := str.[i] :: !l
  done;
  (!l)

let implode li =
  let n = List.length li in
  let s = String.create n in
  let i = ref 0 in
  List.iter (fun c -> s.[!i] <- c; incr i) li;
  (s)

let () =
  let h = Hashtbl.create 3571 in
  let ic = open_in "unixdict.txt" in
  try while true do
    let w = input_line ic in
    let k = implode (List.sort compare (explode w)) in
    let l =
      try Hashtbl.find h k
      with Not_found -> []
    in
    Hashtbl.replace h k (w::l);
  done with End_of_file -> ();
  let n = Hashtbl.fold (fun _ lw n -> max n (List.length lw)) h 0 in
  Hashtbl.iter (fun _ lw ->
    if List.length lw >= n then
    ( List.iter (Printf.printf " %s") lw;
      print_newline () )
  ) h
```



## Oforth



```Oforth
import: mapping
import: collect
import: quicksort

: anagrams
| m |
   "unixdict.txt" File new groupBy( #sort )
   dup sortBy( #[ second size] ) last second size ->m
   filter( #[ second size m == ] )
   apply ( #[ second .cr ] )
;
```


{{out}}

```txt

>anagrams
[abel, able, bale, bela, elba]
[alger, glare, lager, large, regal]
[angel, angle, galen, glean, lange]
[caret, carte, cater, crate, trace]
[elan, lane, lean, lena, neal]
[evil, levi, live, veil, vile]

```



## ooRexx

Two versions of this, using different collection classes.

### Version 1:  Directory of arrays


```ooRexx

-- This assumes you've already downloaded the following file and placed it
-- in the current directory: http://www.puzzlers.org/pub/wordlists/unixdict.txt

-- There are several different ways of reading the file.  I chose the
-- supplier method just because I haven't used it yet in any other examples.
source = .stream~new('unixdict.txt')~supplier
-- this holds our mappings of the anagrams
anagrams = .directory~new
count = 0    -- this is used to keep track of the maximums

loop while source~available
    word = source~item
    -- this produces a string consisting of the characters in sorted order
    -- Note: the ~~ used to invoke sort makes that message return value be
    -- the target array.  The sort method does not normally have a return value.
    key = word~makearray('')~~sort~tostring("l", "")

    -- make sure we have an accumulator collection for this key
    list = anagrams[key]
    if list == .nil then do
       list = .array~new
       anagrams[key] = list
    end
    -- this word is now associate with this key
    list~append(word)
    -- and see if this is a new highest count
    count = max(count, list~items)
    source~next
end

loop letters over anagrams
    list = anagrams[letters]
    if list~items >= count then
        say letters":" list~makestring("l", ", ")
end

```


### Version 2:  Using the relation class

This version appears to be the fastest.

```ooRexx

-- This assumes you've already downloaded the following file and placed it
-- in the current directory: http://www.puzzlers.org/pub/wordlists/unixdict.txt

-- There are several different ways of reading the file.  I chose the
-- supplier method just because I haven't used it yet in any other examples.
source = .stream~new('unixdict.txt')~supplier
-- this holds our mappings of the anagrams.  This is good use for the
-- relation class
anagrams = .relation~new
count = 0    -- this is used to keep track of the maximums

loop while source~available
    word = source~item
    -- this produces a string consisting of the characters in sorted order
    -- Note: the ~~ used to invoke sort makes that message return value be
    -- the target array.  The sort method does not normally have a return value.
    key = word~makearray('')~~sort~tostring("l", "")
    -- add this to our mapping.  This creates multiple entries for each
    -- word that uses the same key
    anagrams[key] = word
    source~next
end

-- now get the set of unique keys
keys = .set~new~~putall(anagrams~allIndexes)
count = 0    -- this is used to keep track of the maximums
most = .directory~new

loop key over keys
    words = anagrams~allAt(key)
    newCount = words~items
    if newCount > count then do
        -- throw away our old set
        most~empty
        count = newCount
        most[key] = words
    end
    -- matches our highest count, add it to the list
    else if newCount == count then
        most[key] = words
end

loop letters over most
    words = most[letters]
    say letters":" words~makestring("l", ", ")
end

```

Timings taken on my laptop:

```txt

Version 1   1.2 seconds
Version 2   0.4 seconds
Rexx       51.1 seconds (!) as of 04.08.2013  (using ooRexx after adapting the code
                                               for incompatibilities: @->y, a=, Upper)
REXX v1     1.7 seconds     as of 05.08.2013  -"- (improved version of REXX code)
REXX v1     1.2 seconds     09.08.2013        -"-
REXX v2     1.2 seconds     09.08.2013
PL/I        4.3 seconds
NetRexx v1   .2 seconds (using local file, 4 seconds with remote)
NetRexx v2   .09 seconds (using local file)

It probably should be noted that the REXX timings are actually for ooRexx executing a modified version of the REXX code.

Statistics:
 sets number of words
22022 1
 1089 2
  155 3
   31 4
    6 5

```



## Oz


```oz
declare
  %% Helper function
  fun {ReadLines Filename}
     File = {New class $ from Open.file Open.text end init(name:Filename)}
  in
     for collect:C break:B do
	case {File getS($)} of false then {File close} {B}
	[] Line then {C Line}
        end
     end
  end

  %% Groups anagrams by using a mutable dictionary
  %% with sorted words as keys
  WordDict = {Dictionary.new}
  for Word in {ReadLines "unixdict.txt"}  do
     Keyword = {String.toAtom {Sort Word Value.'<'}}
  in
     WordDict.Keyword := Word|{CondSelect WordDict Keyword nil}
  end
  Sets = {Dictionary.items WordDict}

  %% Filter such that only the largest sets remain
  MaxSetSize = {FoldL {Map Sets Length} Max 0}
  LargestSets = {Filter Sets fun {$ S} {Length S} == MaxSetSize end}
in
  %% Display result (make sure strings are shown as string, not as number lists)
  {Inspector.object configureEntry(widgetShowStrings true)}
  {Inspect LargestSets}
```



## Pascal


```pascal
Program Anagrams;

// assumes a local file

uses
  classes, math;

var
  i, j, k, maxCount: integer;
  sortedString:      string;
  WordList:          TStringList;
  SortedWordList:    TStringList;
  AnagramList:       array of TStringlist;

begin
  WordList := TStringList.Create;
  WordList.LoadFromFile('unixdict.txt');
  for i := 0 to WordList.Count - 1 do
  begin
    setLength(sortedString,Length(WordList.Strings[i]));
    sortedString[1] := WordList.Strings[i][1];

    // sorted assign
    j := 2;
    while j <=  Length(WordList.Strings[i]) do
    begin
      k := j - 1;
      while (WordList.Strings[i][j] < sortedString[k]) and (k > 0) do
      begin
        sortedString[k+1] := sortedString[k];
        k := k - 1;
      end;
      sortedString[k+1] :=  WordList.Strings[i][j];
      j := j + 1;
    end;

    // create the stringlists of the sorted letters and
    // the list of the original words
    if not assigned(SortedWordList) then
    begin
      SortedWordList := TStringList.Create;
      SortedWordList.append(sortedString);
      setlength(AnagramList,1);
      AnagramList[0] := TStringList.Create;
      AnagramList[0].append(WordList.Strings[i]);
    end
    else
    begin
      j := 0;
      while sortedString <> SortedWordList.Strings[j] do
      begin
        inc(j);
        if j = (SortedWordList.Count) then
        begin
          SortedWordList.append(sortedString);
          setlength(AnagramList,length(AnagramList) + 1);
          AnagramList[j] := TStringList.Create;
 	  break;
        end;
      end;
      AnagramList[j].append(WordList.Strings[i]);
    end;
  end;

  maxCount := 1;
  for i := 0 to length(AnagramList) - 1 do
    maxCount := max(maxCount, AnagramList[i].Count);

  // create output
  writeln('The largest sets of words have ', maxCount, ' members:');
  for i := 0 to length(AnagramList) - 1 do
  begin
    if AnagramList[i].Count = maxCount then
    begin
      write('"', SortedWordList.strings[i], '": ');
      for j := 0 to AnagramList[i].Count - 2 do
        write(AnagramList[i].strings[j], ', ');
      writeln(AnagramList[i].strings[AnagramList[i].Count - 1]);
    end;
  end;

  // Cleanup
  WordList.Destroy;
  SortedWordList.Destroy;
  for i := 0 to length(AnagramList) - 1 do
    AnagramList[i].Destroy;

end.
```

{{out}}

```txt

The largest sets of words have 5 members:
"abel": abel, able, bale, bela, elba
"aeglr": alger, glare, lager, large, regal
"aegln": angel, angle, galen, glean, lange
"acert": caret, carte, cater, crate, trace
"aeln": elan, lane, lean, lena, neal
"eilv": evil, levi, live, veil, vile

```



## Perl


```perl
use List::Util 'max';

my @words = split "\n", do { local( @ARGV, $/ ) = ( 'unixdict.txt' ); <> };
my %anagram;
for my $word (@words) {
    push @{ $anagram{join '', sort split '', $word} }, $word;
}

my $count = max(map {scalar @$_} values %anagram);
for my $ana (values %anagram) {
    print "@$ana\n" if @$ana == $count;
}
```

If we calculate <code>$max</code>, then we don't need the CPAN module:

```perl
push @{$anagram{ join '' => sort split '' }}, $_ for @words;
$max > @$_  or  $max = @$_    for values %anagram;
@$_ == $max and print "@$_\n" for values %anagram;
```

{{out}}
 alger glare lager large regal
 abel able bale bela elba
 evil levi live veil vile
 angel angle galen glean lange
 elan lane lean lena neal
 caret carte cater crate trace


## Perl 6


{{works with|Rakudo|2016.08}}

```perl6
my @anagrams = 'unixdict.txt'.IO.words.classify(*.comb.sort.join).values;

my $max = @anagrams».elems.max;

.put for @anagrams.grep(*.elems == $max);
```


{{out}}
 caret carte cater crate trace
 angel angle galen glean lange
 alger glare lager large regal
 elan lane lean lena neal
 evil levi live veil vile
 abel able bale bela elba

Just for the fun of it, here's a one-liner that uses no temporaries.  Since it would be rather long, we've oriented it vertically:

{{works with|Rakudo|2016.08}}

```perl6
.put for                         # print each element of the array made this way:
    'unixdict.txt'.IO.words      # load words from file
    .classify(*.comb.sort.join)  # group by common anagram
    .classify(*.value.elems)     # group by number of anagrams in a group
    .max(*.key).value            # get the group with highest number of anagrams
    .map(*.value)                # get all groups of anagrams in the group just selected
```



## Phix

copied from Euphoria and cleaned up slightly

```Phix

integer fn = open("unixdict.txt","r")
sequence words = {}, anagrams = {}, last="", letters
object word
integer maxlen = 1

    while 1 do
        word = trim(gets(fn))
        if atom(word) then exit end if
        if length(word) then
            letters = sort(word)
            words = append(words, {letters, word})
        end if
    end while
    close(fn)

    words = sort(words)
    for i=1 to length(words) do
        {letters,word} = words[i]
        if letters=last then
            anagrams[$] = append(anagrams[$],word)
            if length(anagrams[$])>maxlen then
                maxlen = length(anagrams[$])
            end if
        else
            last = letters
            anagrams = append(anagrams,{word})
        end if
    end for

    puts(1,"\nMost anagrams:\n")
    for i=1 to length(anagrams) do
        last = anagrams[i]
        if length(last)=maxlen then
            for j=1 to maxlen do
                if j>1 then puts(1,", ") end if
                puts(1,last[j])
            end for
            puts(1,"\n")
        end if
    end for

```

{{out}}

```txt

Most anagrams:
abel, able, bale, bela, elba
caret, carte, cater, crate, trace
angel, angle, galen, glean, lange
alger, glare, lager, large, regal
elan, lane, lean, lena, neal
evil, levi, live, veil, vile

```



## PHP


```php
<?php
$words = explode("\n", file_get_contents('http://www.puzzlers.org/pub/wordlists/unixdict.txt'));
foreach ($words as $word) {
    $chars = str_split($word);
    sort($chars);
    $anagram[implode($chars)][] = $word;
}

$best = max(array_map('count', $anagram));
foreach ($anagram as $ana)
    if (count($ana) == $best)
        print_r($ana);
?>
```



## PicoLisp

A straight-forward implementation using 'group' takes 48 seconds on a 1.7 GHz Pentium:

```PicoLisp
(flip
   (by length sort
      (by '((L) (sort (copy L))) group
         (in "unixdict.txt" (make (while (line) (link @)))) ) ) )
```

Using a binary tree with the 'idx' function, it takes only 0.42 seconds on the same machine, a factor of 100 faster:

```PicoLisp
(let Words NIL
   (in "unixdict.txt"
      (while (line)
         (let (Word (pack @)  Key (pack (sort @)))
            (if (idx 'Words Key T)
               (push (car @) Word)
               (set Key (list Word)) ) ) ) )
   (flip (by length sort (mapcar val (idx 'Words)))) )
```

{{out}}

```txt
-> (("vile" "veil" "live" "levi" "evil") ("trace" "crate" "cater" "carte" "caret
") ("regal" "large" "lager" "glare" "alger") ("neal" "lena" "lean" "lane" "elan"
) ("lange" "glean" "galen" "angle" "angel") ("elba" "bela" "bale" "able" "abel")
 ("tulsa" "talus" "sault" "latus") ...
```



## PL/I


```PL/I
/* Search a list of words, finding those having the same letters. */

word_test: proc options (main);
   declare words (50000) character (20) varying,
           frequency (50000) fixed binary;
   declare word character (20) varying;
   declare (i, k, wp, most) fixed binary (31);

   on endfile (sysin) go to done;

   words = ''; frequency = 0;
   wp = 0;
   do forever;
      get edit (word) (L);
      call search_word_list (word);
   end;

done:
   put skip list ('There are ' || wp || ' words');
   most = 0;
   /* Determine the word(s) having the greatest number of anagrams. */
   do i = 1 to wp;
      if most < frequency(i) then most = frequency(i);
   end;
   put skip edit ('The following word(s) have ', trim(most), ' anagrams:') (a);
   put skip;
   do i = 1 to wp;
      if most = frequency(i) then put edit (words(i)) (x(1), a);
   end;

search_word_list: procedure (word) options (reorder);
   declare word character (*) varying;
   declare i fixed binary (31);

   do i = 1 to wp;
      if length(words(i)) = length(word) then
         if is_anagram(word, words(i)) then
            do;
               frequency(i) = frequency(i) + 1;
               return;
            end;
   end;
   /* The word does not exist in the list, so add it. */
   if wp >= hbound(words,1) then return;
   wp = wp + 1;
   words(wp) = word;
   frequency(wp) = 1;
   return;
end search_word_list;

/* Returns true if the words are anagrams, otherwise returns false. */
is_anagram: procedure (word1, word2) returns (bit(1)) options (reorder);
   declare (word1, word2) character (*) varying;
   declare tword character (20) varying, c character (1);
   declare (i, j) fixed binary;

   tword = word2;
   do i = 1 to length(word1);
      c = substr(word1, i, 1);
      j = index(tword, c);
      if j = 0 then return ('0'b);
      substr(tword, j, 1) = ' ';
   end;
   return ('1'b);
end is_anagram;

end word_test;
```

{{out}}

```txt

There are          23565 words
The following word(s) have 5 anagrams:
 abel alger angel caret elan evil

```



## PowerShell

{{works with|PowerShell|2}}

```powershell
$c = New-Object Net.WebClient
$words = -split ($c.DownloadString('http://wiki.puzzlers.org/pub/wordlists/unixdict.txt'))
$top_anagrams = $words `
    | ForEach-Object {
          $_ | Add-Member -PassThru NoteProperty Characters `
                   (-join (([char[]] $_) | Sort-Object))
      } `
    | Group-Object Characters `
    | Group-Object Count `
    | Sort-Object Count `
    | Select-Object -First 1

$top_anagrams.Group | ForEach-Object { $_.Group -join ', ' }
```

{{out}}

```txt
abel, able, bale, bela, elba
alger, glare, lager, large, regal
angel, angle, galen, glean, lange
caret, carte, cater, crate, trace
elan, lane, lean, lena, neal
evil, levi, live, veil, vile
```

Another way with more .Net methods is quite a different style, but drops the runtime from 2 minutes to 1.5 seconds:

```powershell
$Timer = [System.Diagnostics.Stopwatch]::StartNew()

$uri = 'http://wiki.puzzlers.org/pub/wordlists/unixdict.txt'
$words = -split [Net.WebClient]::new().DownloadString($uri)

$anagrams = @{}
$maxAnagramCount = 0

foreach ($w in $words)
{
    # Sort the characters in the word into alphabetical order
    $chars=[char[]]$w
    [array]::sort($chars)
    $orderedChars = [string]::Join('', $chars)


    # If no anagrams list for these chars, make one
    if (-not $anagrams.ContainsKey($orderedChars))
    {
        $anagrams[$orderedChars] = [Collections.Generic.List[String]]::new()
    }


    # Add current word as an anagram of these chars,
    # in a way which keeps the list available
    ($list = $anagrams[$orderedChars]).Add($w)


    # Keep running score of max number of anagrams seen
    if ($list.Count -gt $maxAnagramCount)
    {
        $maxAnagramCount = $list.Count
    }

}

foreach ($entry in $anagrams.GetEnumerator())
{
    if ($entry.Value.Count -eq $maxAnagramCount)
    {
        [string]::join('', $entry.Value)
    }
}
```



## Processing


```processing
import java.util.Map;

void setup() {
  String[] words = loadStrings("http://wiki.puzzlers.org/pub/wordlists/unixdict.txt");
  topAnagrams(words);
}

void topAnagrams (String[] words){
  HashMap<String, StringList> anagrams = new HashMap<String, StringList>();
  int maxcount = 0;
  for (String word : words) {
    char[] chars = word.toCharArray();
    chars = sort(chars);
    String key = new String(chars);
    if (!anagrams.containsKey(key)) {
      anagrams.put(key, new StringList());
    }
    anagrams.get(key).append(word);
    maxcount = max(maxcount, anagrams.get(key).size());
  }
  for (StringList ana : anagrams.values()) {
    if (ana.size() >= maxcount) {
      println(ana);
    }
  }
}
```


{{out}}

```txt
StringList size=5 [ "evil", "levi", "live", "veil", "vile" ]
StringList size=5 [ "abel", "able", "bale", "bela", "elba" ]
StringList size=5 [ "elan", "lane", "lean", "lena", "neal" ]
StringList size=5 [ "angel", "angle", "galen", "glean", "lange" ]
StringList size=5 [ "alger", "glare", "lager", "large", "regal" ]
StringList size=5 [ "caret", "carte", "cater", "crate", "trace" ]
```




## Prolog

{{works with|SWI-Prolog|5.10.0}}

```Prolog
:- use_module(library( http/http_open )).

anagrams:-
        % we read the URL of the words
	http_open('http://www.puzzlers.org/pub/wordlists/unixdict.txt',	In, []),
	read_file(In, [], Out),
	close(In),

        % we get a list of pairs key-value where key = a-word value = <list-of-its-codes>
        % this list must be sorted
	msort(Out, MOut),

        % in order to gather values with the same keys
	group_pairs_by_key(MOut, GPL),

        % we sorted this list in decreasing order of the length of values
	predsort(my_compare, GPL, GPLSort),

	% we extract the first 6 items
        GPLSort = [_H1-T1, _H2-T2, _H3-T3, _H4-T4, _H5-T5, _H6-T6 | _],

        % Tnn are lists of codes (97 for 'a'), we create the strings
	maplist(maplist(atom_codes), L, [T1, T2, T3, T4, T5, T6] ),

	maplist(writeln, L).


read_file(In, L, L1) :-
	read_line_to_codes(In, W),
	(   W == end_of_file ->
               % the file is read
	       L1 = L
	       ;
               % we sort the list of codes of the line
	       msort(W, W1),

               % to create the key in alphabetic order
	       atom_codes(A, W1),

               % and we have the pair Key-Value in the result list
	       read_file(In, [A-W | L], L1)).

% predicate for sorting list of pairs Key-Values
% if the lentgh of values is the same
% we sort the keys in alhabetic order
my_compare(R, K1-V1, K2-V2) :-
	length(V1, L1),
	length(V2, L2),
	(   L1 < L2 -> R = >; L1 > L2 -> R = <; compare(R, K1, K2)).
```

The result is

```txt
[abel,able,bale,bela,elba]
[caret,carte,cater,crate,trace]
[angel,angle,galen,glean,lange]
[alger,glare,lager,large,regal]
[elan,lane,lean,lena,neal]
[evil,levi,live,veil,vile]
true
```



## PureBasic

{{works with|PureBasic|4.4}}

```PureBasic
InitNetwork()  ;
OpenConsole()

Procedure.s sortWord(word$)
  len.i = Len(word$)
  Dim CharArray.s (len)

  For n = 1 To len                                 ; Transfering each single character
     CharArray(n) = Mid(word$, n, 1)      ; of the word into an array.
  Next

  SortArray(CharArray(),#PB_Sort_NoCase ) ; Sorting the array.

  word$ =""
  For n = 1 To len                       ; Writing back each single
     word$ + CharArray(n)             ; character of the array.
  Next

  ProcedureReturn word$
EndProcedure

;for a faster and more advanced alternative replace the previous procedure with this code
; Procedure.s sortWord(word$) ;returns a string with the letters of the word sorted
;   Protected wordLength = Len(word$)
;   Protected Dim letters.c(wordLength)
;
;   PokeS(@letters(), word$) ;overwrite the array with the strings contents
;   SortArray(letters(), #PB_Sort_Ascending, 0, wordLength - 1)
;   ProcedureReturn PeekS(@letters(), wordLength) ;return the arrays contents
; EndProcedure


tmpdir$   = GetTemporaryDirectory()
filename$ = tmpdir$ + "unixdict.txt"
Structure ana
   isana.l
   anas.s
EndStructure

NewMap anaMap.ana()

If ReceiveHTTPFile("http://www.puzzlers.org/pub/wordlists/unixdict.txt", filename$)
  If ReadFile(1, filename$)
    Repeat
      word$ = (ReadString(1))             ; Reading a word from a file.
      key$  = (sortWord(word$))             ; Sorting the word and storing in key$.

      If FindMapElement(anaMap(), key$)   ; Looking up if a word already had the same key$.

                                          ; if yes
         anaMap()\anas  = anaMap()\anas+ ", " + word$   ; adding the word
         anaMap()\isana + 1
      Else
                                          ; if no
         anaMap(key$)\anas = word$        ; applying  a new record
         anaMap()\isana = 1
       EndIf

      If anaMap()\isana > maxAnagrams ;make note of maximum anagram count
        maxAnagrams = anaMap()\isana
      EndIf

    Until Eof(1)
    CloseFile(1)
    DeleteFile(filename$)

    ;----- output -----
    ForEach anaMap()
      If anaMap()\isana = maxAnagrams      ; only emit elements that have the most hits
        PrintN(anaMap()\anas)
      EndIf
    Next

    PrintN("Press any key"): Repeat: Until Inkey() <> ""
  EndIf
EndIf
```

{{out}}

```txt
evil, levi, live, veil, vile
angel, angle, galen, glean, lange
alger, glare, lager, large, regal
abel, able, bale, bela, elba
elan, lane, lean, lena, neal
caret, carte, cater, crate, trace
```



## Python


### Python 3.X Using defaultdict

Python 3.2 shell input (IDLE)

```python>>>
 import urllib.request
>>> from collections import defaultdict
>>> words = urllib.request.urlopen('http://www.puzzlers.org/pub/wordlists/unixdict.txt').read().split()
>>> anagram = defaultdict(list) # map sorted chars to anagrams
>>> for word in words:
	anagram[tuple(sorted(word))].append( word )


>>> count = max(len(ana) for ana in anagram.values())
>>> for ana in anagram.values():
	if len(ana) >= count:
		print ([x.decode() for x in ana])
```



### Python 2.7 version

Python 2.7 shell input (IDLE)

```python>>>
 import urllib
>>> from collections import defaultdict
>>> words = urllib.urlopen('http://www.puzzlers.org/pub/wordlists/unixdict.txt').read().split()
>>> len(words)
25104
>>> anagram = defaultdict(list) # map sorted chars to anagrams
>>> for word in words:
	anagram[tuple(sorted(word))].append( word )


>>> count = max(len(ana) for ana in anagram.itervalues())
>>> for ana in anagram.itervalues():
	if len(ana) >= count:
		print ana


['angel', 'angle', 'galen', 'glean', 'lange']
['alger', 'glare', 'lager', 'large', 'regal']
['caret', 'carte', 'cater', 'crate', 'trace']
['evil', 'levi', 'live', 'veil', 'vile']
['elan', 'lane', 'lean', 'lena', 'neal']
['abel', 'able', 'bale', 'bela', 'elba']
>>> count
5
>>>
```



### Python: Using groupby

{{trans|Haskell}}
{{works with|Python|2.6}} sort and then group using groupby()

```python>>>
 import urllib, itertools
>>> words = urllib.urlopen('http://www.puzzlers.org/pub/wordlists/unixdict.txt').read().split()
>>> len(words)
25104
>>> anagrams = [list(g) for k,g in itertools.groupby(sorted(words, key=sorted), key=sorted)]


>>> count = max(len(ana) for ana in anagrams)
>>> for ana in anagrams:
	if len(ana) >= count:
		print ana


['abel', 'able', 'bale', 'bela', 'elba']
['caret', 'carte', 'cater', 'crate', 'trace']
['angel', 'angle', 'galen', 'glean', 'lange']
['alger', 'glare', 'lager', 'large', 'regal']
['elan', 'lane', 'lean', 'lena', 'neal']
['evil', 'levi', 'live', 'veil', 'vile']
>>> count
5
>>>
```



Or, disaggregating, speeding up a bit by avoiding the slightly expensive use of ''sorted'' as a key, updating for Python 3, and using a local ''unixdict.txt'':
{{Works with|Python|3.7}}

```python
'''Largest anagram groups found in list of words.'''

from os.path import expanduser
from itertools import groupby
from operator import eq


# main :: IO ()
def main():
    '''Largest anagram groups in local unixdict.txt'''

    print(unlines(
        largestAnagramGroups(
            lines(readFile('unixdict.txt'))
        )
    ))


# largestAnagramGroups :: [String] -> [[String]]
def largestAnagramGroups(ws):
    '''A list of the anagram groups of
       of the largest size found in a
       given list of words.
    '''

    # wordChars :: String -> (String, String)
    def wordChars(w):
        '''A word paired with its
           AZ sorted characters
        '''
        return (''.join(sorted(w)), w)

    groups = list(map(
        compose(list)(snd),
        groupby(
            sorted(
                map(wordChars, ws),
                key=fst
            ),
            key=fst
        )
    ))

    intMax = max(map(len, groups))
    return list(map(
        compose(unwords)(curry(map)(snd)),
        filter(compose(curry(eq)(intMax))(len), groups)
    ))


# GENERIC -------------------------------------------------

# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# curry :: ((a, b) -> c) -> a -> b -> c
def curry(f):
    '''A curried function derived
       from an uncurried function.'''
    return lambda a: lambda b: f(a, b)


# fst :: (a, b) -> a
def fst(tpl):
    '''First member of a pair.'''
    return tpl[0]


# lines :: String -> [String]
def lines(s):
    '''A list of strings,
       (containing no newline characters)
       derived from a single new-line delimited string.'''
    return s.splitlines()


# from os.path import expanduser
# readFile :: FilePath -> IO String
def readFile(fp):
    '''The contents of any file at the path
       derived by expanding any ~ in fp.'''
    with open(expanduser(fp), 'r', encoding='utf-8') as f:
        return f.read()


# snd :: (a, b) -> b
def snd(tpl):
    '''Second member of a pair.'''
    return tpl[1]


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.'''
    return '\n'.join(xs)


# unwords :: [String] -> String
def unwords(xs):
    '''A space-separated string derived from
       a list of words.'''
    return ' '.join(xs)


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
caret carte cater crate creat creta react recta trace
angor argon goran grano groan nagor orang organ rogan
ester estre reest reset steer stere stree terse tsere
```



## QB64


```QB64

$CHECKING:OFF
' Warning: Keep the above line commented out until you know your newly edited code works.
'          You can NOT stop a program in mid run (using top right x button) with checkng off.
'
_TITLE "Rosetta Code Anagrams: mod #7 Best times yet w/o memory techniques by bplus 2017-12-12"
' This program now below .4 secs for average time to do 100 loops compared to 92 secs for 1
' loop on my "dinosaur" when I first coded a successful run.
'
' Steve McNeil at QB64.net has +7000 loops per sec on his machine with help of using
' memory techniques.  see page 3 @  http://www.qb64.net/forum/index.php?topic=14622.30
'
' Thanks Steve! I learned allot and am NOW very motivated to learn memory techniques.
'
' This program has timings for 1 loop broken into sections currently commented out and another
' set of timings for multiple loop testing currently set, now at 100 tests for a sort of average.
' But average is misleading, the first test is usually always the longest and really only one test
' is necessary to get the results from a data file that does not change.
'
' Breaking code into logical sections and timing those can help spot trouble areas or the difference
' in a small or great change.
'
' Here is review of speed tips commented as they occur in code:
'
DEFINT A-Z 'there are 25,105 words in the unixdict.txt file so main array index
'           and pointers in sort can all be integers.

' The letters from a word read in from the dictionary file (really just a word list in alpha order)
' are to be counted and coded into an alpha order sequence of letters:
'       eg.  eilv is the same code for words: evil, levi, live, veil, vile
' The longest word in the file had 22 letters, they are all lower case but there are other symbols
' in file like ' and digits we want to filter out.
TYPE wordData
    code AS STRING * 22
    theWord AS STRING * 22
END TYPE
' I originally was coding a word into the whole list (array) of letter counts as a string.
' Then realized I could drop all the zeros if I converted the numbers back to letters.
' I then attached THE word to the end of the coded word using ! to separate the 2 sections.
' That was allot of manipulation with INSTR to find the ! separator and then MID$ to extract the
' code or THE word when I needed the value. All this extra manipulation ended by using TYPE with
' the code part and the word part sharing the same index. Learned from Steve's example!

' Pick the lowest number type needed to cover the problem
DIM SHARED w(25105) AS wordData '  the main array
DIM anagramSetsCount AS _BYTE ' the Rosetta Code Challenge was to find only the largest sets of Anagrams
DIM codeCount AS _BYTE ' counting number of words with same code
DIM wordIndex AS _BYTE
DIM wordLength AS _BYTE
DIM flag AS _BIT 'flag used as true or false
DIM letterCounts(1 TO 26) AS _BYTE 'stores letter counts for coding word
' b$  always stands for building a string.
' For long and strings, I am using the designated suffix

t1# = TIMER: loops = 100
FOR test = 1 TO loops
    'reset these for multiple loop tests
    indexTop = 0 'indexTop for main data array
    anagramSetsCount = 0 'anagrams count if exceed 4 for any one code
    anagramList$ = "" 'list of anagrams

    'get the file data loaded in one pop, disk access is slow!
    OPEN "unixdict.txt" FOR BINARY AS #1
    ' http://www.puzzlers.org/pub/wordlists/unixdict.txt
    ' note: when I downloaded this file line breaks were by chr$(10) only.
    ' Steve had coded for either chr$(13) + chr$(10) or just chr$(10)

    fileLength& = LOF(1): buf$ = SPACE$(fileLength&)
    GET #1, , buf$
    CLOSE #1
    ' Getting the data into a big long string saved allot of time as compared to
    ' reading from the file line by line.

    'Process the file data by extracting the word from the long file string and then
    'coding each word of interest, loading up the w() array.
    filePosition& = 1
    WHILE filePosition& < fileLength&
        nextPosition& = INSTR(filePosition&, buf$, CHR$(10))
        wd$ = MID$(buf$, filePosition&, nextPosition& - filePosition&)
        wordLength = LEN(wd$)
        IF wordLength > 2 THEN
            'From Steve's example, changing from REDIM to ERASE saved an amzing amount of time!
            ERASE letterCounts: flag = 0: wordIndex = 1
            WHILE wordIndex <= wordLength
                'From Steve's example, I was not aware of this version of ASC with MID$ built-in
                ansciChar = ASC(wd$, wordIndex) - 96
                IF 0 < ansciChar AND ansciChar < 27 THEN letterCounts(ansciChar) = letterCounts(ansciChar) + 1 ELSE flag = 1: EXIT WHILE
                wordIndex = wordIndex + 1
            WEND
            'don't code and store a word unless all letters, no digits or apostrophes
            IF flag = 0 THEN
                b$ = "": wordIndex = 1
                WHILE wordIndex < 27
                    IF letterCounts(wordIndex) THEN b$ = b$ + STRING$(letterCounts(wordIndex), CHR$(96 + wordIndex))
                    wordIndex = wordIndex + 1
                WEND
                indexTop = indexTop + 1
                w(indexTop).code = b$
                w(indexTop).theWord = wd$
            END IF
        END IF
        IF nextPosition& THEN filePosition& = nextPosition& + 1 ELSE filePosition& = fileLength&
    WEND
    't2# = TIMER
    'PRINT t2# - t1#; " secs to load word array."

    'Sort using a recursive Quick Sort routine on the code key of wordData Type defined.
    QSort 0, indexTop
    't3# = TIMER
    'PRINT t3# - t2#; " secs to sort array."

    'Now find all the anagrams, word permutations, from the same word "code" that we sorted by.
    flag = 0: j = 0
    WHILE j < indexTop
        'Does the sorted code key match the next one on the list?
        IF w(j).code <> w(j + 1).code THEN ' not matched so stop counting and add to report
            IF codeCount > 4 THEN ' only want the largest sets of anagrams 5 or more
                anagramList$ = anagramList$ + b$ + CHR$(10)
                anagramSetsCount = anagramSetsCount + 1
            END IF
            codeCount = 0: b$ = "": flag = 0
        ELSEIF flag THEN ' match and match flag set so just add to count and build set
            b$ = b$ + ", " + RTRIM$(w(j + 1).theWord)
            codeCount = codeCount + 1
        ELSE ' no flag means first match, start counting and building a new set
            b$ = RTRIM$(w(j).theWord) + ", " + RTRIM$(w(j + 1).theWord)
            codeCount = 2: flag = 1
        END IF
        j = j + 1
    WEND
    't4# = TIMER
    'PRINT t4# - t3#; " secs to count matches from array."
NEXT
PRINT "Ave time per loop"; (TIMER - t1#) / loops; " secs, there were"; anagramSetsCount; " anagrams sets of 5 or more words."
PRINT anagramList$

'This sub modified for wordData Type, to sort by the .code key, the w() array is SHARED
SUB QSort (Start, Finish)
    i = Start: j = Finish: x$ = w(INT((i + j) / 2)).code
    WHILE i <= j
        WHILE w(i).code < x$: i = i + 1: WEND
        WHILE w(j).code > x$: j = j - 1: WEND
        IF i <= j THEN
            SWAP w(i), w(j)
            i = i + 1: j = j - 1
        END IF
    WEND
    IF j > Start THEN QSort Start, j
    IF i < Finish THEN QSort i, Finish
END SUB

```


'''2nd solution (by Steve McNeill):'''

```QB64

$CHECKING:OFF
SCREEN _NEWIMAGE(640, 480, 32)
_DELAY .5
_SCREENMOVE _MIDDLE

DEFLNG A-Z
TYPE DataType
    Word AS _UNSIGNED INTEGER
    Value AS STRING * 26
END TYPE

REDIM Words(0 TO 30000) AS DataType
REDIM WordList(0 TO 30000) AS STRING * 25
DIM Anagrams(0 TO 30000, 0 TO 10) AS LONG
DIM EndLine AS STRING, Endlength AS LONG
IF INSTR(temp$, CHR$(13)) THEN EndLine = CHR$(13) + CHR$(10) ELSE EndLine = CHR$(10)
Endlength = LEN(EndLine)
DIM t AS _FLOAT 'high precisition timer
DIM t1 AS _FLOAT
DIM letters(97 TO 122) AS _UNSIGNED _BYTE
DIM m1 AS _MEM, m2 AS _MEM, m3 AS _MEM
DIM a AS _UNSIGNED _BYTE
DIM matched(30000) AS _BYTE
m1 = _MEM(letters()): m2 = _MEM(Words()): m3 = _MEM(WordList())
blank$ = STRING$(26, 0)
t1 = TIMER
oldenter = 1

DO UNTIL TIMER - t1 > 1

    t = t1
    looper = looper + 1
    OPEN "unixdict.txt" FOR BINARY AS #1
    temp$ = SPACE$(LOF(1))
    GET #1, 1, temp$ 'just grab the whole datafile from the drive in one swoop
    CLOSE #1
    'PRINT USING "##.###### seconds to load data from disk."; TIMER - t
    t = TIMER

    index = -1 'we want our first word to be indexed at 0, for ease of array/mem swappage
    DO 'and parse it manually into our array
        skip:
        enter = INSTR(oldenter, temp$, EndLine)
        IF enter THEN
            l = enter - oldenter - 1
            wd$ = MID$(temp$, oldenter, l)
            oldenter = enter + Endlength
        ELSE
            wd$ = MID$(temp$, oldenter)
            l = LEN(wd$)
        END IF

        _MEMPUT m1, m1.OFFSET, blank$ 'ERASE letters

        j = 1
        DO UNTIL j > l
            a = ASC(wd$, j)
            IF a < 97 OR a > 122 GOTO skip
            letters(a) = letters(a) + 1 'and count them
            j = j + 1
        LOOP
        index = index + 1
        WordList(index) = wd$
        Words(index).Word = index

        _MEMCOPY m1, m1.OFFSET, 26 TO m2, m2.OFFSET + m2.ELEMENTSIZE * (index) + 2

    LOOP UNTIL enter = 0
    CLOSE #1
    'PRINT USING "##.###### seconds to parse data into array."; TIMER - t
    t = TIMER

    combsort Words(), index

    i = 1
    DO UNTIL i > index

        IF matched(i) = 0 THEN
            count = 0
            DO
                count = count + 1
                c = i + count
                IF c > index THEN EXIT DO
                IF _STRICMP(Words(i).Value, Words(c).Value) <> 0 THEN EXIT DO
                Anagrams(anagram_count, count) = c
                matched(c) = -1
            LOOP
            IF count > 1 THEN
                Anagrams(anagram_count, 0) = i
                Anagrams(anagram_count, 10) = count
                i = c - 1
                anagram_count = anagram_count + 1
            END IF
        END IF
        i = i + 1
    LOOP
    t2## = TIMER
    'PRINT USING "##.###### seconds to make matches."; t2## - t
    'PRINT USING "##.###### total time from start to finish."; t2## - t1
    'PRINT
LOOP
$CHECKING:ON
PRINT "LOOPER:"; looper; "executions from start to finish, in one second."
PRINT "Note, this is including disk access for new data each time."
PRINT
PRINT USING "#.################ seconds on average to run"; 1## / looper

INPUT "Anagram Pool Limit Size (Or larger) =>"; limit
IF limit < 1 THEN END
FOR i = 0 TO anagram_count - 1
    v = Anagrams(i, 10)
    IF v >= limit THEN
        FOR j = 0 TO v
            SELECT CASE j
                CASE 0
                CASE v: PRINT
                CASE ELSE: PRINT ", ";
            END SELECT
            PRINT LEFT$(WordList(Words(Anagrams(i, j)).Word), INSTR(WordList(Words(Anagrams(i, j)).Word), " "));
        NEXT
    END IF
NEXT
END

SUB combsort (array() AS DataType, index AS LONG)
    DIM gap AS LONG
    'This is the routine I tend to use personally and promote.
    'It's short, simple, and easy to implement into code.

    gap = index

    DO
        gap = INT(gap / 1.247330925103979)
        IF gap < 1 THEN gap = 1
        i = 0
        swapped = 0
        DO
            IF array(i).Value > array(i + gap).Value THEN
                SWAP array(i), array(i + gap)
                swapped = -1
            END IF
            i = i + 1
        LOOP UNTIL i + gap > index
    LOOP UNTIL gap = 1 AND swapped = 0
END SUB

```


'''Output:'''

```QB64

LOOPER: 7134 executions from start to finish, in one second.
Note, this is including disk access for new data each time.

0.000140138155313 seconds on average to run
Anagram Pool Limit Size (or larger) =>? 5
veil, levi, live, vile, evil
lane, neal, lean, lena, elan
alger, lager, large, glare, regal
glean, angel, galen, angle, lange
caret, trace, crate, carte, cater
bale, abel, able, elba, bela

```



## R


```R
words <- readLines("http://www.puzzlers.org/pub/wordlists/unixdict.txt")
word_group <- sapply(
    strsplit(words, split=""), # this will split all words to single letters...
    function(x) paste(sort(x), collapse="") # ...which we sort and paste again
)

counts <- tapply(words, word_group, length) # group words by class to get number of anagrams
anagrams <- tapply(words, word_group, paste, collapse=", ") # group to get string with all anagrams

# Results
table(counts)
counts
    1     2     3     4     5
22263  1111   155    31     6

anagrams[counts == max(counts)]
                               abel                               acert
     "abel, able, bale, bela, elba" "caret, carte, cater, crate, trace"
                              aegln                               aeglr
"angel, angle, galen, glean, lange" "alger, glare, lager, large, regal"
                               aeln                                eilv
     "elan, lane, lean, lena, neal"      "evil, levi, live, veil, vile"
```



## Racket


```racket

#lang racket

(require net/url)

(define (get-lines url-string)
  (define port (get-pure-port (string->url url-string)))
  (for/list ([l (in-lines port)]) l))

(define (hash-words words)
  (for/fold ([ws-hash (hash)]) ([w words])
    (hash-update ws-hash
                 (list->string (sort (string->list w) < #:key (λ (c) (char->integer c))))
                 (λ (ws) (cons w ws))
                 (λ () '()))))

(define (get-maxes h)
  (define max-ws (apply max (map length (hash-values h))))
  (define max-keys (filter (λ (k) (= (length (hash-ref h k)) max-ws)) (hash-keys h)))
  (map (λ (k) (hash-ref h k)) max-keys))

(get-maxes (hash-words (get-lines "http://www.puzzlers.org/pub/wordlists/unixdict.txt")))

```

{{out}}

```txt

'(("neal" "lena" "lean" "lane" "elan")
  ("trace" "crate" "cater" "carte" "caret")
  ("regal" "large" "lager" "glare" "alger")
  ("elba" "bela" "bale" "able" "abel")
  ("lange" "glean" "galen" "angle" "angel")
  ("vile" "veil" "live" "levi" "evil"))

```




## RapidQ


```vb

dim x as integer, y as integer
dim SortX as integer
dim StrOutPut as string
dim Count as integer
dim MaxCount as integer

dim AnaList as QStringlist
dim wordlist as QStringlist
dim Templist as QStringlist
dim Charlist as Qstringlist


function sortChars(expr as string) as string
    Charlist.clear
    for SortX = 1 to len(expr)
         Charlist.AddItems expr[SortX]
    next
    charlist.sort
    result = Charlist.text - chr$(10) - chr$(13)
end function

'--- Start main code
    wordlist.loadfromfile ("unixdict.txt")

    'create anagram list
    for x = 0 to wordlist.itemcount-1
        AnaList.AddItems sortChars(wordlist.item(x))
    next

    'Filter largest anagram lists
    analist.sort
    MaxCount = 0

    for x = 0 to AnaList.Itemcount-1
        Count = 0
        for y = x+1 to AnaList.Itemcount-1
            if AnaList.item(y) = AnaList.item(x) then
                inc(count)
            else
                if count > MaxCount then
                    Templist.clear
                    MaxCount = Count
                    Templist.AddItems AnaList.item(x)
                elseif count = MaxCount then
                    Templist.AddItems AnaList.item(x)
                end if
                exit for
            end if
        next
    next

    'Now get the words
    for x = 0 to Templist.Itemcount-1
        for y = 0 to wordlist.Itemcount-1
            if Templist.item(x) = sortChars(wordlist.item(y)) then
                StrOutPut = StrOutPut + wordlist.item(y) + "  "
            end if
        next
        StrOutPut = StrOutPut + chr$(13) + chr$(10)
    next

ShowMessage StrOutPut
End


```

{{out}}

```txt

abel  able  bale  bela  elba
caret  carte  cater  crate  trace
angel  angle  galen  glean  lange
alger  glare  lager  large  regal
elan  lane  lean  lena  neal
evil  levi  live  veil  vile

```



## Rascal


```rascal
import Prelude;

list[str] OrderedRep(str word){
	return sort([word[i] | i <- [0..size(word)-1]]);
}
public list[set[str]] anagram(){
	allwords = readFileLines(|http://www.puzzlers.org/pub/wordlists/unixdict.txt|);
	AnagramMap = invert((word : OrderedRep(word) | word <- allwords));
	longest = max([size(group) | group <- range(AnagramMap)]);
	return [AnagramMap[rep]| rep <- AnagramMap, size(AnagramMap[rep]) == longest];
}
```

Returns:

```rascal
value: [
  {"glean","galen","lange","angle","angel"},
  {"glare","lager","regal","large","alger"},
  {"carte","trace","crate","caret","cater"},
  {"lane","lena","lean","elan","neal"},
  {"able","bale","abel","bela","elba"},
  {"levi","live","vile","evil","veil"}
]
```


## Red


```Red
Red []

m: make map! [] 25000

maxx: 0
foreach word  read/lines http://www.puzzlers.org/pub/wordlists/unixdict.txt [
sword:  sort copy word ;; sorted characters of word

either find m sword [
    append   m/:sword word
    maxx: max maxx length?  m/:sword
   ] [
      put m sword append copy [] word
    ]
]
foreach v values-of m [ if maxx = length? v [print v] ]

```

{{out}}

```txt
abel able bale bela elba
alger glare lager large regal
angel angle galen glean lange
caret carte cater crate trace
elan lane lean lena neal
evil levi live veil vile
>>

```



## REXX

===version 1.1, idiomatic===
This version doesn't assume that the dictionary is in alphabetical order,   nor does it assume the

words are in any specific case   (lower/upper/mixed).

```rexx
/*REXX program  finds words  with the  largest set of  anagrams  (of the same size).    */
iFID= 'unixdict.txt'                             /*the dictionary input File IDentifier.*/
$=;     !.=;      ww=0;       uw=0;      most=0  /*initialize a bunch of REXX variables.*/
                                                 /* [↓]  read the entire file (by lines)*/
    do  while lines(iFID) \== 0                  /*Got any data?   Then read a record.  */
    parse value  linein(iFID)  with  @ .         /*obtain a word from an input line.    */
    len=length(@);  if len<3  then iterate       /*onesies and twosies words can't win. */
    if \datatype(@, 'M')      then iterate       /*ignore any  non─anagramable words.   */
    uw=uw + 1                                    /*count of the (useable) words in file.*/
    _=sortA(@)                                   /*sort the letters in the word.        */
    !._=!._ @;       #=words(!._)                /*append it to !._;  bump the counter. */
    if #==most  then $=$ _                       /*append the sorted word──► max anagram*/
                else if #>most  then do;   $=_;   most=#;   if len>ww  then ww=len;    end
    end   /*while*/                              /*$ ◄── list of high count anagrams.   */
say '─────────────────────────'    uw     "usable words in the dictionary file: "     iFID
say
     do m=1  for words($);   z=subword($, m, 1)  /*the high count of the anagrams.      */
     say '     '     left(word(!.z, 1),  ww)      '   [anagrams: '      subword(!.z, 2)"]"
     end   /*m*/                                 /*W   is the maximum width of any word.*/
say
say '───── Found'   words($)    "words  (each of which have"    words(!.z)-1  'anagrams).'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sortA: arg char 2 xx,@.                          /*get the first letter of arg;  @.=null*/
       @.char=char                               /*no need to concatenate the first char*/
                                                 /*[↓]  sort/put letters alphabetically.*/
                 do length(xx);   parse var xx char 2 xx;    @.char=@.char || char;    end
                                                 /*reassemble word with sorted letters. */
       return @.a || @.b || @.c || @.d || @.e || @.f||@.g||@.h||@.i||@.j||@.k||@.l||@.m||,
              @.n || @.o || @.p || @.q || @.r || @.s||@.t||@.u||@.v||@.w||@.x||@.y||@.z
```

Programming note:   the long (wide) assignment for     '''return @.a||'''...     could've been coded as an elegant   '''do'''   loop instead of hardcoding 26 letters,
but since the dictionary (word list) is rather large, a rather expaciated method was used for speed.

{{out|output|text=  when using the default input (dictionary):}}

```txt

───────────────────────── 24819 usable words in the dictionary file:  unixdict.txt

      abel     [anagrams:  able bale bela elba]
      angel    [anagrams:  angle galen glean lange]
      elan     [anagrams:  lane lean lena neal]
      alger    [anagrams:  glare lager large regal]
      caret    [anagrams:  carte cater crate trace]
      evil     [anagrams:  levi live veil vile]

───── Found 6 words  (each of which have 4 anagrams).

```


===version 1.2, optimized===
This optimized version eliminates the   '''sortA'''   subroutine and puts that subroutine's code in-line.

```rexx
/*REXX program  finds words  with the  largest set of  anagrams  (of the same size).    */
iFID= 'unixdict.txt'                             /*the dictionary input File IDentifier.*/
$=;     !.=;      ww=0;       uw=0;      most=0  /*initialize a bunch of REXX variables.*/
                                                 /* [↓]  read the entire file (by lines)*/
    do  while lines(iFID) \== 0                  /*Got any data?   Then read a record.  */
    parse value  linein(iFID)  with  @ .         /*obtain a word from an input line.    */
    len=length(@);  if len<3  then iterate       /*onesies and twosies words can't win. */
    if \datatype(@, 'M')      then iterate       /*ignore any  non─anagramable words.   */
    uw=uw + 1                                    /*count of the (useable) words in file.*/
    _=sortA(@)                                   /*sort the letters in the word.        */
    !._=!._ @;       #=words(!._)                /*append it to !._;  bump the counter. */
    if #==most  then $=$ _                       /*append the sorted word──► max anagram*/
                else if #>most  then do;   $=_;   most=#;   if len>ww  then ww=len;    end
    end   /*while*/                              /*$ ◄── list of high count anagrams.   */
say '─────────────────────────'    uw    "usable words in the dictionary file: "      iFID
say
     do m=1  for words($);   z=subword($, m, 1)  /*the high count of the anagrams.      */
     say '     '     left(word(!.z, 1),  ww)      '   [anagrams: '      subword(!.z, 2)"]"
     end   /*m*/                                 /*W   is the maximum width of any word.*/
say
say '───── Found'   words($)    "words  (each of which have"    words(!.z)-1  'anagrams).'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sortA: arg char 2 xx,@.                          /*get the first letter of arg;  @.=null*/
       @.char=char                               /*no need to concatenate the first char*/
                                                 /*[↓]  sort/put letters alphabetically.*/
                 do length(xx);   parse var xx char 2 xx;    @.char=@.char || char;    end
                                                 /*reassemble word with sorted letters. */
       return @.a || @.b || @.c || @.d || @.e || @.f||@.g||@.h||@.i||@.j||@.k||@.l||@.m||,
              @.n || @.o || @.p || @.q || @.r || @.s||@.t||@.u||@.v||@.w||@.x||@.y||@.z
```

{{out|output|text=  is the same as REXX version 1.1}}

Programming note:   the above REXX programs adopted the method that the REXX version 2 uses for extracting each character of a word.

The method is more obtuse, but when invoking the routine tens of thousands of times, this faster method lends itself to heavy use.


### annotated version using   PARSE

(This algorithm actually  utilizes a   ''bin''   sort,   one bin for each Latin letter.)

```rexx
u= 'Halloween'                                   /*word to be sorted by (Latin)  letter.*/
upper u                                          /*fast method to uppercase a variable. */
                                                 /*another:       u = translate(u)      */
                                                 /*another:       parse upper var u u   */
                                                 /*another:       u = upper(u)          */
                                                 /*not always available [↑]             */
say 'u=' u
_.=
       do  until  u==''                          /*keep truckin' until  U  is  null.    */
       parse var u y +1 u                        /*get the next (first) character in  U.*/
       xx='?'y                                   /*assign a prefixed character to   XX. */
       _.xx=_.xx || y                            /*append it to all the  Y  characters. */
       end   /*until*/                           /*U now has the first character elided.*/
                                                 /*Note:  the variable  U  is destroyed.*/

                                                 /* [↓]  constructs a sorted letter word*/

z=_.?a||_.?b||_.?c||_.?d||_.?e||_.?f||_.?g||_.?h||_.?i||_.?j||_.?k||_.?l||_.?m||,
  _.?n||_.?o||_.?p||_.?q||_.?r||_.?s||_.?t||_.?u||_.?v||_.?w||_.?x||_.?y||_.?z

                                   /*Note:  the  ?  is prefixed to the letter to avoid  */
                                   /*collisions with other REXX one-character variables.*/
say 'z=' z
```

{{out|output|:}}

```txt

u= HALLOWEEN
z= AEEHLLNOW

```



### annotated version using a   DO   loop


```rexx
u= 'Halloween'                                   /*word to be sorted by (Latin)  letter.*/
upper u                                          /*fast method to uppercase a variable. */
L=length(u)                                      /*get the length of the word (in bytes)*/
say 'u=' u
say 'L=' L
_.=
       do k=1  for L                             /*keep truckin'  for   L   characters. */
       parse var u =(k) y +1                     /*get the  Kth  character in  U string.*/
       xx='?'y                                   /*assign a prefixed character to   XX. */
       _.xx=_.xx || y                            /*append it to all the  Y  characters. */
       end   /*do k*/                            /*U now has the first character elided.*/

                                                 /* [↓]  construct a sorted letter word.*/

z=_.?a||_.?b||_.?c||_.?d||_.?e||_.?f||_.?g||_.?h||_.?i||_.?j||_.?k||_.?l||_.?m||,
  _.?n||_.?o||_.?p||_.?q||_.?r||_.?s||_.?t||_.?u||_.?v||_.?w||_.?x||_.?y||_.?z

say 'z=' z
```

{{out|output|:}}

```txt

u= HALLOWEEN
L= 9
z= AEEHLLNOW

```



### version 2


```rexx
/*REXX program finds words with the largest set of anagrams (same size)
* 07.08.2013 Walter Pachl
* sorta for word compression courtesy Gerard Schildberger,
*                            modified, however, to obey lowercase
* 10.08.2013 Walter Pachl take care of mixed case dictionary
*                         following Version 1's method
**********************************************************************/
Parse Value 'A B C D E F G H I J K L M N O P Q R S T U V W X Y Z',
       With  a b c d e f g h i j k l m n o p q r s t u v w x y z
Call time 'R'
ifid='unixdict.txt'              /* input file identifier          */
words=0                          /* number of usable words         */
maxl=0                           /* maximum number of anagrams     */
wl.=''                           /* wl.ws words that have ws       */
Do ri=1 By 1 While lines(ifid)\==0 /* read each word in file       */
  word=space(linein(ifid),0)     /* pick off a word from the input.*/
  If length(word)<3 Then         /* onesies and twosies can't win. */
    Iterate
  If\datatype(word,'M') Then     /* not an anagramable word        */
    Iterate
  words=words+1                  /* count of (useable) words.      */
  ws=sorta(word)                 /* sort the letters in the word.  */
  wl.ws=wl.ws word               /* add word to list of ws         */
  wln=words(wl.ws)               /* number of anagrams with ws     */
  Select
    When wln>maxl Then Do        /* a new maximum                  */
      maxl=wln                   /* use this                       */
      wsl=ws                     /* list of resulting ws values    */
      End
    When wln=maxl Then           /* same as the one found          */
      wsl=wsl ws                 /* add ws to the list             */
    Otherwise                    /* shorter                        */
      Nop                        /* not yet of interest            */
    End
  End
Say ' '
Say copies('-',10) ri-1 'words in the dictionary file: ' ifid
Say copies(' ',10) words 'thereof are anagram candidates'
Say ' '
Say 'There are' words(wsl) 'set(s) of anagrams with' maxl,
                                                       'elements each:'
Say ' '
Do while wsl<>''
  Parse Var wsl ws wsl
  Say '    'wl.ws
  End
Say time('E')
Exit
sorta:
/**********************************************************************
* sort the characters in word_p (lowercase translated to uppercase)
* 'chARa' -> 'AACHR'
**********************************************************************/
  Parse Upper Arg word_p
  c.=''
  Do While word_p>''
    Parse Var word_p cc +1 word_p
    c.cc=c.cc||cc
    End
  Return c.a||c.b||c.c||c.d||c.e||c.f||c.g||c.h||c.i||c.j||c.k||c.l||,
  c.m||c.n||c.o||c.p||c.q||c.r||c.s||c.t||c.u||c.v||c.w||c.x||c.y||c.z
```

{{out}}

```txt

---------- 25108 words in the dictionary file:  unixdict.txt
           24819 thereof are anagram candidates

There are 6 set(s) of anagrams with 5 elements each:

     abel able bale bela elba
     angel angle galen glean lange
     elan lane lean lena neal
     alger glare lager large regal
     caret carte cater crate trace
     evil levi live veil vile
1.170000

```



## Ring


```ring

# Project : Anagrams

load "stdlib.ring"
fn1 = "unixdict.txt"

fp = fopen(fn1,"r")
str = fread(fp, getFileSize(fp))
fclose(fp)
strlist = str2list(str)
anagram = newlist(len(strlist), 5)
anag = list(len(strlist))
result = list(len(strlist))
for x = 1 to len(result)
     result[x] = 0
next
for x = 1 to len(anag)
     anag[x] = 0
next
for x = 1 to len(anagram)
    for y = 1 to 5
         anagram[x][y] = 0
    next
next

for n = 1 to len(strlist)
     for m = 1 to len(strlist)
          sum = 0
          if len(strlist[n]) = 4 and len(strlist[m]) = 4 and n != m
             for p = 1 to len(strlist[m])
                  temp1 = count(strlist[n], strlist[m][p])
                  temp2 = count(strlist[m], strlist[m][p])
                  if temp1 = temp2
                     sum = sum + 1
                  ok
             next
             if sum = 4
                anag[n] = anag[n] + 1
                if anag[n] < 6 and result[n] = 0 and result[m] = 0
                   anagram[n][anag[n]] = strlist[m]
                   result[m] = 1
                ok
             ok
          ok
    next
    if anag[n] > 0
       result[n] = 1
    ok
next

for n = 1 to len(anagram)
     flag = 0
     for m = 1 to 5
         if anagram[n][m] != 0
            if m = 1
               see strlist[n] +  " "
               flag = 1
            ok
            see anagram[n][m] + " "
         ok
     next
     if flag = 1
        see nl
     ok
next

func getFileSize fp
       c_filestart = 0
       c_fileend = 2
       fseek(fp,0,c_fileend)
       nfilesize = ftell(fp)
       fseek(fp,0,c_filestart)
       return nfilesize

func count(astring,bstring)
       cnt = 0
       while substr(astring,bstring) > 0
               cnt = cnt + 1
               astring = substr(astring,substr(astring,bstring)+len(string(sum)))
       end
       return cnt

```

Output:

```txt

abbe babe
abed bade bead
abel able bale bela
abet bate beat beta
alai alia
alex axle
bail bali
bake beak
bane bean
bard brad
bare bear brae
barn bran
beam bema
blot bolt
blow bowl
blur burl
body boyd

```



## Ruby


```ruby
require 'open-uri'

anagram = Hash.new {|hash, key| hash[key] = []} # map sorted chars to anagrams

open('http://www.puzzlers.org/pub/wordlists/unixdict.txt') do |f|
  words = f.read.split
  for word in words
    anagram[word.split('').sort] << word
  end
end

count = anagram.values.map {|ana| ana.length}.max
anagram.each_value do |ana|
  if ana.length >= count
    p ana
  end
end
```

{{out}}

```txt

 ["evil", "levi", "live", "veil", "vile"]
 ["abel", "able", "bale", "bela", "elba"]
 ["elan", "lane", "lean", "lena", "neal"]
 ["alger", "glare", "lager", "large", "regal"]
 ["angel", "angle", "galen", "glean", "lange"]
 ["caret", "carte", "cater", "crate", "trace"]

```


Short version (with lexical ordered result).

```ruby
require 'open-uri'

anagrams = open('http://www.puzzlers.org/pub/wordlists/unixdict.txt'){|f| f.read.split.group_by{|w| w.each_char.sort} }
anagrams.values.group_by(&:size).max.last.each{|group| puts group.join(", ") }

```

{{Out}}

```txt

abel, able, bale, bela, elba
alger, glare, lager, large, regal
angel, angle, galen, glean, lange
caret, carte, cater, crate, trace
elan, lane, lean, lena, neal
evil, levi, live, veil, vile

```


## Run BASIC


```runbasic
sqliteconnect #mem, ":memory:"
mem$ = "CREATE TABLE anti(gram,ordr);
CREATE INDEX ord ON anti(ordr)"
#mem execute(mem$)
' read the file
a$ = httpGet$("http://www.puzzlers.org/pub/wordlists/unixdict.txt")

' break the file words apart
i = 1
while i <> 0
	j	= instr(a$,chr$(10),i+1)
	if j = 0 then exit while
	a1$	= mid$(a$,i,j-i)
	q	= instr(a1$,"'")
	if q > 0 then a1$ = left$(a1$,q) + mid$(a1$,q)
	ln	= len(a1$)
	s$	= a1$

	' Split the characters of the word and sort them
	s = 1
	while	s = 1
		s = 0
		for k = 1 to ln -1
			if mid$(s$,k,1) > mid$(s$,k+1,1) then
				h$	= mid$(s$,k,1)
				h1$	= mid$(s$,k+1,1)
				s$ 	= left$(s$,k-1) + h1$ + h$ + mid$(s$,k+2)
				s	= 1
			end if
		next k
	wend

	mem$	= "INSERT INTO anti VALUES('";a1$;"','";ord$;"')"
	#mem execute(mem$)
	i	= j +1
wend
' find all antigrams
mem$	= "SELECT count(*) as cnt,anti.ordr FROM anti GROUP BY ordr ORDER BY cnt desc"
#mem execute(mem$)
numDups = #mem ROWCOUNT()	'Get the number of rows
dim dups$(numDups)
for i = 1 to numDups
	#row	= #mem #nextrow()
	cnt	= #row cnt()
	if i = 1 then maxCnt = cnt
	if cnt < maxCnt then exit for
	dups$(i) = #row ordr$()
next i

for i = 1 to i -1
	mem$	= "SELECT anti.gram FROM anti
		   WHERE  anti.ordr = '";dups$(i);"'
		   ORDER BY anti.gram"
	#mem execute(mem$)
	rows = #mem ROWCOUNT()	'Get the number of rows

	for ii = 1 to rows
		#row	= #mem #nextrow()
		gram$	= #row gram$()
		print gram$;chr$(9);
	next ii
	print
next i
end
```


```txt

abel	able	bale	bela	elba
caret	carte	cater	crate	trace
angel	angle	galen	glean	lange
alger	glare	lager	large	regal
elan	lane	lean	lena	neal
evil	levi	live	veil	vile
```



## Rust


### Sorting

Unicode is hard so the solution depends on what you consider to be an anagram: two strings that have the same bytes, the same codepoints, or the same graphemes. The first two are easily accomplished in Rust proper, but the latter requires an external library. Graphemes are probably the most correct way, but it is also the least efficient since graphemes are variable size and thus require a heap allocation per grapheme.


```rust
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead,BufReader};
use std::borrow::ToOwned;

extern crate unicode_segmentation;
use unicode_segmentation::{UnicodeSegmentation};

fn main () {
    let file = BufReader::new(File::open("unixdict.txt").unwrap());
    let mut map = HashMap::new();
    for line in file.lines() {
        let s = line.unwrap();
        //Bytes:      let mut sorted = s.trim().bytes().collect::<Vec<_>>();
        //Codepoints: let mut sorted = s.trim().chars().collect::<Vec<_>>();
        let mut sorted = s.trim().graphemes(true).map(ToOwned::to_owned).collect::<Vec<_>>();
        sorted.sort();

        map.entry(sorted).or_insert_with(Vec::new).push(s);
    }

    if let Some(max_len) = map.values().map(|v| v.len()).max() {
        for anagram in map.values().filter(|v| v.len() == max_len) {
            for word in anagram {
                print!("{} ", word);
            }
            println!();
        }
    }
}
```

{{out}}

```txt

alger glare lager large regal
angel angle galen glean lange
elan lane lean lena neal
evil levi live veil vile
caret carte cater crate trace
abel able bale bela elba

```


### Using prime factors

If we assume an ASCII string, we can map each character to a prime number and multiply these together to create a number which uniquely maps to each anagram.


```rust
use std::collections::HashMap;
use std::path::Path;
use std::io::{self, BufRead, BufReader};
use std::fs::File;

fn main() {
    if let Ok(anagrams) = find_anagrams("unixdict.txt") {
        for anagram in anagrams {
            for word in anagram {
                print!("{} ", word);
            }
            println!();
        }
    }
}

const PRIMES: [u64; 256] = [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127
                           ,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257
                           ,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401
                           ,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,547,557,563
                           ,569,571,577,587,593,599,601,607,613,617,619,631,641,643,647,653,659,661,673,677,683,691,701,709
                           ,719,727,733,739,743,751,757,761,769,773,787,797,809,811,821,823,827,829,839,853,857,859,863,877
                           ,881,883,887,907,911,919,929,937,941,947,953,967,971,977,983,991,997,1009,1013,1019,1021,1031,1033
                           ,1039,1049,1051,1061,1063,1069,1087,1091,1093,1097,1103,1109,1117,1123,1129,1151,1153,1163,1171
                           ,1181,1187,1193,1201,1213,1217,1223,1229,1231,1237,1249,1259,1277,1279,1283,1289,1291,1297,1301
                           ,1303,1307,1319,1321,1327,1361,1367,1373,1381,1399,1409,1423,1427,1429,1433,1439,1447,1451,1453
                           ,1459,1471,1481,1483,1487,1489,1493,1499,1511,1523,1531,1543,1549,1553,1559,1567,1571,1579,1583
                           ,1597,1601,1607,1609,1613,1619];

fn find_anagrams<P: AsRef<Path>>(file: P) -> io::Result<Vec<Vec<String>>> {
    let file = BufReader::new(File::open(file)?);
    let mut map = HashMap::new();
    for line in file.lines() {
        let string = line?;
        let mut num = 1;
        for ch in string.trim().bytes() {
            num *= PRIMES[ch as usize];
        }
        map.entry(num).or_insert_with(Vec::new).push(string);
    }
    Ok(map.into_iter().map(|(_, entry)| entry).collect())
}
```



## Scala


```scala
val src = io.Source fromURL "http://www.puzzlers.org/pub/wordlists/unixdict.txt"
val vls = src.getLines.toList.groupBy(_.sorted).values
val max = vls.map(_.size).max
vls filter (_.size == max) map (_ mkString " ") mkString "\n"
```

{{out}}

```txt

abel able bale bela elba
angel angle galen glean lange
evil levi live veil vile
alger glare lager large regal
elan lane lean lena neal
caret carte cater crate trace

```

----
Another take:

```scala
Source
  .fromURL("http://www.puzzlers.org/pub/wordlists/unixdict.txt").getLines.toList
  .groupBy(_.sorted).values
  .groupBy(_.size).maxBy(_._1)._2
  .map(_.mkString("\t"))
  .foreach(println)
```

{{out}}

```txt

abel	able	bale	bela	elba
angel	angle	galen	glean	lange
evil	levi	live	veil	vile
alger	glare	lager	large	regal
elan	lane	lean	lena	neal
caret	carte	cater	crate	trace

```



## Scheme


Uses two SRFI libraries: SRFI 125 for hash tables and SRFI 132 for sorting.


```scheme

(import (scheme base)
        (scheme char)
        (scheme file)
        (scheme write)
        (srfi 125)  ; hash tables
        (srfi 132)) ; sorting library

;; read in the words
(define (read-groups)
  (with-input-from-file
    "unixdict.txt"
    (lambda ()
      (let ((groups (hash-table string=?)))
        (do ((line (read-line) (read-line)))
          ((eof-object? line) groups)
          (let* ((key (list->string (list-sort char<? (string->list line))))
                 (val (hash-table-ref/default groups key '())))
            (hash-table-set! groups key (cons line val))))))))

;; extract the longest values from given hash-table of groups
(define (largest-groups groups)
  (define (find-largest grps n sofar)
    (cond ((null? grps)
           sofar)
          ((> (length (car grps)) n)
           (find-largest (cdr grps) (length (car grps)) (list (car grps))))
          ((= (length (car grps)) n)
           (find-largest (cdr grps) n (cons (car grps) sofar)))
          (else
            (find-largest (cdr grps) n sofar))))
  (find-largest (hash-table-values groups) 0 '()))

;; print results
(for-each
  (lambda (group)
    (display "[ ")
    (for-each (lambda (word) (display word) (display " ")) group)
    (display "]\n"))
  (list-sort (lambda (a b) (string<? (car a) (car b)))
             (map (lambda (grp) (list-sort string<? grp))
                  (largest-groups (read-groups)))))

```


{{out}}

```txt

[ abel able bale bela elba ]
[ alger glare lager large regal ]
[ angel angle galen glean lange ]
[ caret carte cater crate trace ]
[ elan lane lean lena neal ]
[ evil levi live veil vile ]

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "gethttp.s7i";
  include "strifile.s7i";

const type: anagramHash is hash [string] array string;

const func string: sort (in string: stri) is func
  result
    var string: sortedStri is "";
  local
    var integer: i is 0;
    var integer: j is 0;
    var char: ch is ' ';
  begin
    sortedStri := stri;
    for i range 1 to length(sortedStri) do
      for j range succ(i) to length(sortedStri) do
        if sortedStri[i] > sortedStri[j] then
          ch := sortedStri[i];
          sortedStri @:= [i] sortedStri[j];
          sortedStri @:= [j] ch;
        end if;
      end for;
    end for;
  end func;

const proc: main is func
  local
    var file: dictFile is STD_NULL;
    var string: word is "";
    var string: sortedLetters is "";
    var anagramHash: anagrams is anagramHash.value;
    var integer: length is 0;
    var integer: maxLength is 0;
  begin
    dictFile := openStrifile(getHttp("www.puzzlers.org/pub/wordlists/unixdict.txt"));
    while hasNext(dictFile) do
      readln(dictFile, word);
      sortedLetters := sort(word);
      if sortedLetters in anagrams then
        anagrams[sortedLetters] &:= word;
      else
        anagrams @:= [sortedLetters] [] (word);
      end if;
      length := length(anagrams[sortedLetters]);
      if length > maxLength then
        maxLength := length;
      end if;
    end while;
    close(dictFile);
    for sortedLetters range sort(keys(anagrams)) do
      if length(anagrams[sortedLetters]) = maxLength then
        writeln(join(anagrams[sortedLetters], ", "));
      end if;
    end for;
  end func;
```


{{out}}

```txt

abel, able, bale, bela, elba
caret, carte, cater, crate, trace
angel, angle, galen, glean, lange
alger, glare, lager, large, regal
elan, lane, lean, lena, neal
evil, levi, live, veil, vile

```



## SETL


```SETL
h := open('unixdict.txt', "r");
anagrams := {};
while not eof(h) loop
  geta(h, word);
  if word = om or word = "" then
    continue;
  end if;
  sorted := insertion_sort(word);
  anagrams{sorted} with:= word;
end loop;

max_size := 0;
max_words := {};
for words = anagrams{sorted} loop
  size := #words;
  if size > max_size then
    max_size := size;
    max_words := {words};
  elseif size = max_size then
    max_words with:= words;
  end if;
end loop;

for w in max_words loop
  print(w);
end loop;

-- GNU SETL has no built-in sort()
procedure insertion_sort(A);
  for i in [2..#A] loop
    v := A(i);
    j := i-1;
    while j >= 1 and A(j) > v loop
      A(j+1) := A(j);
      j := j - 1;
    end loop;
    A(j+1) := v;
 end loop;
 return A;
end procedure;
```

{{out}}

```txt
{abel able bale bela elba}
{alger glare lager large regal}
{angel angle galen glean lange}
{caret carte cater crate trace}
{elan lane lean lena neal}
{evil levi live veil vile}
```




## Sidef


```ruby
func main(file) {
    file.open_r(\var fh, \var err) ->
        || die "Can't open file `#{file}' for reading: #{err}\n";

    var vls = fh.words.group_by{.sort}.values;
    var max = vls.map{.len}.max;
    vls.grep{.len == max}.each{.join("\t").say};
}

main(%f'/tmp/unixdict.txt');
```

{{out}}

```txt
alger	glare	lager	large	regal
abel	able	bale	bela	elba
angel	angle	galen	glean	lange
elan	lane	lean	lena	neal
evil	levi	live	veil	vile
caret	carte	cater	crate	trace
```


## Simula


```simula
COMMENT COMPILE WITH
$ cim -m64 anagrams-hashmap.sim
;
BEGIN

    COMMENT ----- CLASSES FOR GENERAL USE ;

    ! ABSTRACT HASH KEY TYPE ;
    CLASS HASHKEY;
    VIRTUAL:
        PROCEDURE HASH IS
            INTEGER PROCEDURE HASH;;
        PROCEDURE EQUALTO IS
            BOOLEAN PROCEDURE EQUALTO(K); REF(HASHKEY) K;;
    BEGIN
    END HASHKEY;

    ! ABSTRACT HASH VALUE TYPE ;
    CLASS HASHVAL;
    BEGIN
        ! THERE IS NOTHING REQUIRED FOR THE VALUE TYPE ;
    END HASHVAL;

    CLASS HASHMAP;
    BEGIN
        CLASS INNERHASHMAP(N); INTEGER N;
        BEGIN

            INTEGER PROCEDURE INDEX(K); REF(HASHKEY) K;
            BEGIN
                INTEGER I;
                IF K == NONE THEN
                    ERROR("HASHMAP.INDEX: NONE IS NOT A VALID KEY");
                I := MOD(K.HASH,N);
            LOOP:
                IF KEYTABLE(I) == NONE OR ELSE KEYTABLE(I).EQUALTO(K) THEN
                    INDEX := I
                ELSE BEGIN
                    I := IF I+1 = N THEN 0 ELSE I+1;
                    GO TO LOOP;
                END;
            END INDEX;

            ! PUT SOMETHING IN ;
            PROCEDURE PUT(K,V); REF(HASHKEY) K; REF(HASHVAL) V;
            BEGIN
                INTEGER I;
                IF V == NONE THEN
                    ERROR("HASHMAP.PUT: NONE IS NOT A VALID VALUE");
                I := INDEX(K);
                IF KEYTABLE(I) == NONE THEN BEGIN
                    IF SIZE = N THEN
                        ERROR("HASHMAP.PUT: TABLE FILLED COMPLETELY");
                    KEYTABLE(I) :- K;
                    VALTABLE(I) :- V;
                    SIZE := SIZE+1;
                END ELSE
                    VALTABLE(I) :- V;
            END PUT;

            ! GET SOMETHING OUT ;
            REF(HASHVAL) PROCEDURE GET(K); REF(HASHKEY) K;
            BEGIN
                INTEGER I;
                IF K == NONE THEN
                    ERROR("HASHMAP.GET: NONE IS NOT A VALID KEY");
                I := INDEX(K);
                IF KEYTABLE(I) == NONE THEN
                    GET :- NONE ! ERROR("HASHMAP.GET: KEY NOT FOUND");
                ELSE
                    GET :- VALTABLE(I);
            END GET;

            PROCEDURE CLEAR;
            BEGIN
                INTEGER I;
                FOR I := 0 STEP 1 UNTIL N-1 DO BEGIN
                    KEYTABLE(I) :- NONE;
                    VALTABLE(I) :- NONE;
                END;
                SIZE := 0;
            END CLEAR;

            ! DATA MEMBERS OF CLASS HASHMAP ;
            REF(HASHKEY) ARRAY KEYTABLE(0:N-1);
            REF(HASHVAL) ARRAY VALTABLE(0:N-1);
            INTEGER SIZE;

        END INNERHASHMAP;

        PROCEDURE PUT(K,V); REF(HASHKEY) K; REF(HASHVAL) V;
        BEGIN
            IF IMAP.SIZE >= 0.75 * IMAP.N THEN
            BEGIN
                COMMENT RESIZE HASHMAP ;
                REF(INNERHASHMAP) NEWIMAP;
                REF(ITERATOR) IT;
                NEWIMAP :- NEW INNERHASHMAP(2 * IMAP.N);
                IT :- NEW ITERATOR(THIS HASHMAP);
                WHILE IT.MORE DO
                BEGIN
                    REF(HASHKEY) KEY;
                    KEY :- IT.NEXT;
                    NEWIMAP.PUT(KEY, IMAP.GET(KEY));
                END;
                IMAP.CLEAR;
                IMAP :- NEWIMAP;
            END;
            IMAP.PUT(K, V);
        END;

        REF(HASHVAL) PROCEDURE GET(K); REF(HASHKEY) K;
            GET :- IMAP.GET(K);

        PROCEDURE CLEAR;
            IMAP.CLEAR;

        INTEGER PROCEDURE SIZE;
            SIZE := IMAP.SIZE;

        REF(INNERHASHMAP) IMAP;

        IMAP :- NEW INNERHASHMAP(16);
    END HASHMAP;

    CLASS ITERATOR(H); REF(HASHMAP) H;
    BEGIN
        INTEGER POS,KEYCOUNT;

        BOOLEAN PROCEDURE MORE;
            MORE := KEYCOUNT < H.SIZE;

        REF(HASHKEY) PROCEDURE NEXT;
        BEGIN
            INSPECT H.IMAP DO
            BEGIN
                WHILE KEYTABLE(POS) == NONE DO
                    POS := POS+1;
                NEXT :- KEYTABLE(POS);
                KEYCOUNT := KEYCOUNT+1;
                POS := POS+1;
            END;
        END NEXT;

    END ITERATOR;

    COMMENT ----- PROBLEM SPECIFIC CLASSES ;

    HASHKEY CLASS TEXTHASHKEY(T); VALUE T; TEXT T;
    BEGIN
        INTEGER PROCEDURE HASH;
        BEGIN
            INTEGER I;
            T.SETPOS(1);
            WHILE T.MORE DO
                I := 31*I+RANK(T.GETCHAR);
            HASH := I;
        END HASH;
        BOOLEAN PROCEDURE EQUALTO(K); REF(HASHKEY) K;
            EQUALTO := T = K QUA TEXTHASHKEY.T;
    END TEXTHASHKEY;

    HASHVAL CLASS TEXTVECTOR;
    BEGIN

        CLASS ARRAYHOLDER(N); INTEGER N;
        BEGIN
            TEXT ARRAY DATA(1:N);
        END ARRAYHOLDER;

        REF(ARRAYHOLDER) HOLDER;
        INTEGER SIZE;

        PROCEDURE DOUBLESIZE;
        BEGIN
            REF(ARRAYHOLDER) NEWHOLDER;
            INTEGER I;
            NEWHOLDER :- NEW ARRAYHOLDER(2 * HOLDER.N);
            FOR I := 1 STEP 1 UNTIL HOLDER.N DO
                NEWHOLDER.DATA(I) :- HOLDER.DATA(I);
            HOLDER :- NEWHOLDER;
        END;

        PROCEDURE ADD(WORD); TEXT WORD;
        BEGIN
            SIZE := SIZE + 1;
            IF SIZE > HOLDER.N THEN
                DOUBLESIZE;
            HOLDER.DATA(SIZE) :- WORD;
        END;

        HOLDER :- NEW ARRAYHOLDER(16);
    END TEXTVECTOR;

    TEXT PROCEDURE MAKEKEY(WORD); TEXT WORD;
    BEGIN
        TEXT KEY;
        INTEGER I;
        KEY :- BLANKS(WORD.LENGTH);
        KEY.SETPOS(1);
        FOR I := RANK('a') STEP 1 UNTIL RANK('z'),
                 RANK('0') STEP 1 UNTIL RANK('9') DO
        BEGIN
            WORD.SETPOS(1);
            WHILE WORD.MORE DO
              IF WORD.GETCHAR = CHAR(I) THEN
                  KEY.PUTCHAR(CHAR(I));
        END;
        MAKEKEY :- KEY;
    END MAKEKEY;

    REF(INFILE) INF;
    REF(HASHMAP) MAP;
    REF(HASHKEY) KEY;
    REF(TEXTVECTOR) TVEC;
    REF(ITERATOR) IT;
    TEXT WORD;
    INTEGER I, J, LONGEST;

    MAP :- NEW HASHMAP;

    INF :- NEW INFILE("unixdict.txt");
    INF.OPEN(BLANKS(132));
    WHILE NOT INF.LASTITEM DO
    BEGIN
        WORD :- COPY(INF.IMAGE).STRIP; INF.INIMAGE;
        KEY :- NEW TEXTHASHKEY(MAKEKEY(WORD));
        TVEC :- MAP.GET(KEY);
        IF TVEC == NONE THEN
        BEGIN
            TVEC :- NEW TEXTVECTOR;
            MAP.PUT(KEY, TVEC);
        END;
        TVEC.ADD(WORD);
    END;
    INF.CLOSE;

    COMMENT FIND LONGEST ENTRIES ;

    IT :- NEW ITERATOR(MAP);
    WHILE IT.MORE DO
    BEGIN
        TVEC :- MAP.GET(IT.NEXT);
        IF TVEC.SIZE > LONGEST THEN
            LONGEST := TVEC.SIZE;
    END;

    COMMENT OUTPUT LONGEST ENTRIES ;

    IT :- NEW ITERATOR(MAP);
    WHILE IT.MORE DO
    BEGIN
        KEY :- IT.NEXT;
        TVEC :- MAP.GET(KEY);
        IF TVEC.SIZE = LONGEST THEN
        BEGIN
            OUTTEXT(KEY QUA TEXTHASHKEY.T);
            OUTTEXT(":");
            FOR J := 1 STEP 1 UNTIL TVEC.SIZE DO
                INSPECT TVEC.HOLDER DO
                BEGIN
                    OUTCHAR(' ');
                    OUTTEXT(DATA(J));
                END;
            OUTIMAGE;
        END;
    END;

END

```

{{out}}

```txt

eilv: evil levi live veil vile
abel: abel able bale bela elba
aeln: elan lane lean lena neal
aegln: angel angle galen glean lange
aeglr: alger glare lager large regal
acert: caret carte cater crate trace

1 garbage collection(s) in 0.1 seconds.


```



## Smalltalk


```Smalltalk
list:= (FillInTheBlank request: 'myMessageBoxTitle') subStrings: String crlf.
dict:= Dictionary new.
list do: [:val|
	(dict at: val copy sort ifAbsent: [dict at: val copy sort put: OrderedCollection new])
		add: val.
	].
sorted:=dict asSortedCollection: [:a :b| a size > b size].
```

Documentation:

```txt

First ask the user for the list.
Then create an empty dictionary (a Map). Which maps strings as keys to OrderedCollections as values.
For each entry in the list add an entry to the OrderedCollection under the key of the sorted string
(and create a new empty OC if there was no previous entry).
Then create a SortedCollection sorting by comparing the sizes of the OrderedCollections.
The first 6 entries are:
an OrderedCollection('evil' 'levi' 'live' 'veil' 'vile')
an OrderedCollection('angel' 'angle' 'galen' 'glean' 'lange')
an OrderedCollection('alger' 'glare' 'lager' 'large' 'regal')
an OrderedCollection('caret' 'carte' 'cater' 'crate' 'trace')
an OrderedCollection('abel' 'able' 'bale' 'bela' 'elba')
an OrderedCollection('elan' 'lane' 'lean' 'lena' 'neal')

```

{{works with|Smalltalk/X}}
instead of asking for the strings, read the file:

```smalltalk
d := Dictionary new.
'unixdict.txt' asFilename
    readingLinesDo:[:eachWord |
      (d at:eachWord copy sort ifAbsentPut:[OrderedCollection new]) add:eachWord
    ].

((d values select:[:s | s size > 1])
  sortBySelector:#size)
    reverse
      do:[:s | s printCR]
```

{{out}}

```txt

OrderedCollection('angel' 'angle' 'galen' 'glean' 'lange')
OrderedCollection('abel' 'able' 'bale' 'bela' 'elba')
OrderedCollection('elan' 'lane' 'lean' 'lena' 'neal')
OrderedCollection('caret' 'carte' 'cater' 'crate' 'trace')
OrderedCollection('evil' 'levi' 'live' 'veil' 'vile')
OrderedCollection('alger' 'glare' 'lager' 'large' 'regal')
OrderedCollection('mate' 'meat' 'tame' 'team')
...
```

not sure if getting the dictionary via http is part of the task; if so, replace the file-reading with:

```smalltalk
'http://www.puzzlers.org/pub/wordlists/unixdict.txt' asURI contents asCollectionOfLines do:[:eachWord | ...
```



## SNOBOL4

{{works with|Macro Spitbol}}
Note: unixdict.txt is passed in locally via STDIN. Newlines must be converted for Win/DOS environment.

```SNOBOL4
*       # Sort letters of word
        define('sortw(str)a,i,j') :(sortw_end)
sortw   a = array(size(str))
sw1     i = i + 1; str len(1) . a<i> = :s(sw1)
        a = sort(a)
sw2     j = j + 1; sortw = sortw a<j> :s(sw2)f(return)
sortw_end

*       # Count words in string
        define('countw(str)') :(countw_end)
countw  str break(' ') span(' ') = :f(return)
        countw = countw + 1 :(countw)
countw_end

        ana = table()
L1      wrd = input :f(L2) ;* unixdict.txt from stdin
        sw = sortw(wrd); ana<sw> = ana<sw> wrd ' '
        cw = countw(ana<sw>); max = gt(cw,max) cw
        i = i + 1; terminal = eq(remdr(i,1000),0) wrd :(L1)
L2      kv = convert(ana,'array')
L3      j = j + 1; key = kv<j,1>; val = kv<j,2> :f(end)
        output = eq(countw(val),max) key ': ' val :(L3)
end
```

{{out}}

```txt
abel: abel able bale bela elba
aeglr: alger glare lager large regal
aegln: angel angle galen glean lange
acert: caret carte cater crate trace
aeln: elan lane lean lena neal
eilv: evil levi live veil vile
```



## Stata


```stata
import delimited http://www.puzzlers.org/pub/wordlists/unixdict.txt, clear
mata
a=st_sdata(.,.)
n=rows(a)
for (i=1; i<=n; i++) a[i]=char(sort(ascii(a[i])',1)')
st_addvar(st_vartype(1),"group")
st_sstore(.,2,a)
end

bysort group (v1): gen k=_N
qui sum k
keep if k==r(max)
by group: replace k=_n
reshape wide v1, i(k) j(group) string
drop k
list, noobs noheader
```


'''Output'''


```txt
  +--------------------------------------------------------+
  |   abel     caret     angel     alger     elan     evil |
  |   able     carte     angle     glare     lane     levi |
  |   bale     cater     galen     lager     lean     live |
  |   bela     crate     glean     large     lena     veil |
  |   elba     trace     lange     regal     neal     vile |
  +--------------------------------------------------------+
```



## SuperCollider


```SuperCollider
(
var text, words, sorted, dict = IdentityDictionary.new, findMax;
File.use("unixdict.txt".resolveRelative, "r", { |f| text = f.readAllString });
words = text.split(Char.nl);
sorted = words.collect { |each|
	var key = each.copy.sort.asSymbol;
	dict[key] ?? { dict[key] = [] };
	dict[key] = dict[key].add(each)
};
findMax = { |dict|
	var size = 0, max = [];
	dict.keysValuesDo { |key, val|
		if(val.size == size) { max = max.add(val) } {
			if(val.size > size) { max = []; size = val.size }
		}
	};
	max
};
findMax.(dict)
)
```


Answers:

```SuperCollider
[ [ angel, angle, galen, glean, lange ], [ caret, carte, cater, crate, trace ], [ elan, lane, lean, lena, neal ], [ evil, levi, live, veil, vile ], [ alger, glare, lager, large, regal ] ]
```



## Swift

{{works with|Swift 2.0}}


```swift
import Foundation

let wordsURL = NSURL(string: "http://www.puzzlers.org/pub/wordlists/unixdict.txt")!

let wordsstring = try NSString(contentsOfURL:wordsURL , encoding: NSUTF8StringEncoding)
let allwords = wordsstring.componentsSeparatedByString("\n")

let words = allwords//[0..<100] // used to limit the size while testing

extension String {
    var charactersAscending : String {
        return String(Array(characters).sort())
    }
}

var charsToWords = [String:Set<String>]()

var biggest = 0
var biggestlists = [Set<String>]()

for thisword in words {
    let chars = thisword.charactersAscending

    var knownwords = charsToWords[chars] ?? Set<String>()
    knownwords.insert(thisword)
    charsToWords[chars] = knownwords

    if knownwords.count > biggest {
        biggest = knownwords.count

        biggestlists = [knownwords]
    }
    else if knownwords.count == biggest {
        biggestlists.append(knownwords)
    }
}

print("Found \(biggestlists.count) sets of anagrams with \(biggest) members each")
for (i, thislist) in biggestlists.enumerate() {
    print("set \(i): \(thislist.sort())")
}

```


{{out}}

```txt

Found 6 sets of anagrams with 5 members each
set 0: [abel, able, bale, bela, elba]
set 1: [angel, angle, galen, glean, lange]
set 2: [elan, lane, lean, lena, neal]
set 3: [alger, glare, lager, large, regal]
set 4: [caret, carte, cater, crate, trace]
set 5: [evil, levi, live, veil, vile]
Program ended with exit code: 0

```



## Tcl


```tcl
package require Tcl 8.5
package require http

set url http://www.puzzlers.org/pub/wordlists/unixdict.txt
set response [http::geturl $url]
set data [http::data $response]
http::cleanup $response

set max 0
array set anagrams {}

foreach line [split $data \n] {
    foreach word [split $line] {
        set anagram [join [lsort [split $word ""]] ""]
        lappend anagrams($anagram) $word
        set max [::tcl::mathfunc::max $max [llength $anagrams($anagram)]]
    }
}

foreach key [array names anagrams] {
    if {[llength $anagrams($key)] == $max} {
        puts $anagrams($key)
    }
}
```

{{out}}

```txt
evil levi live veil vile
caret carte cater crate trace
abel able bale bela elba
elan lane lean lena neal
angel angle galen glean lange
alger glare lager large regal
```



## TUSCRIPT


```tuscript
$$ MODE TUSCRIPT,{}
requestdata = REQUEST ("http://www.puzzlers.org/pub/wordlists/unixdict.txt")

DICT anagramm CREATE 99999

COMPILE
 LOOP word=requestdata
  -> ? : any character
  charsInWord=STRINGS (word," ? ")
  charString =ALPHA_SORT (charsInWord)
  DICT anagramm APPEND/QUIET/COUNT charString,num,freq,word;" "
 ENDLOOP

DICT anagramm UNLOAD charString,all,freq,anagrams

index        =DIGIT_INDEX (freq)
reverseIndex =REVERSE (index)
freq         =INDEX_SORT (freq,reverseIndex)
anagrams     =INDEX_SORT (anagrams,reverseIndex)
charString   =INDEX_SORT (charString,reverseIndex)

mostWords=SELECT (freq,1), adjust=MAX_LENGTH (charString)
 LOOP cs=charString, f=freq, a=anagrams
  IF (f<mostWords) EXIT
   cs=CENTER (cs,-adjust)
   PRINT cs," ",f,": ",a
 ENDLOOP
ENDCOMPILE
```

{{out}}

```txt

e'i'l'v                                     5: evil levi live veil vile
a'e'l'n                                     5: elan lane lean lena neal
a'c'e'r't                                   5: caret carte cater crate trace
a'e'g'l'n                                   5: angel angle galen glean lange
a'e'g'l'r                                   5: alger glare lager large regal
a'b'e'l                                     5: abel able bale bela elba

```



## UNIX Shell

{{works with|bash|4}}
There's a bit of a cheat here: bash has no builtin way to sort.
I have to call out to the system's <tt>sort</tt> utility.

This code uses a [http://www.gnu.org/software/bash/manual/bashref.html#Process-Substitution process substitution].
Bash will execute each part of a command pipeline in a subshell.
That means if you set variables inside a while loop that's part of a pipeline, then those variables will disappear when the subshell exits.
Process substitutions eliminate the need for command pipelines.


```bash
http_get_body() {
    local host=$1
    local uri=$2
    exec 5<> /dev/tcp/$host/80
    printf >&5 "%s\r\n" "GET $uri HTTP/1.1" "Host: $host" "Connection: close" ""
    mapfile -t -u5
    local lines=( "${MAPFILE[@]//$'\r'}" )
    local i=0 found=0
    for (( ; found == 0; i++ )); do
        [[ -z ${lines[i]} ]] && (( found++ ))
    done
    printf "%s\n" "${lines[@]:i}"
    exec 5>&-
}

declare -A wordlist

while read -r word; do
    uniq_letters=( $(for ((i=0; i<${#word}; i++)); do echo "${word:i:1}"; done | sort) )
    wordlist["${uniq_letters[*]}"]+="$word "
done < <( http_get_body www.puzzlers.org  /pub/wordlists/unixdict.txt )

maxlen=0
maxwords=()

for key in "${!wordlist[@]}"; do
    words=( ${wordlist[$key]} )
    if (( ${#words[@]} > maxlen )); then
        maxlen=${#words[@]}
        maxwords=( "${wordlist["$key"]}" )
    elif (( ${#words[@]} == maxlen )); then
        maxwords+=( "${wordlist[$key]}" )
    fi
done

printf "%s\n" "${maxwords[@]}"
```


{{output}}

```txt
angel angle galen glean lange
alger glare lager large regal
evil levi live veil vile
elan lane lean lena neal
caret carte cater crate trace
abel able bale bela elba
```



## Ursala

Supplying the input file on the command line during compilation makes its contents accessible as a pre-declared identifier.
The algorithm is to group the words together that are made from the same unordered lists of letters, then collect the groups together that have the same number of words in
them, and then show the collection associated with the highest number.

```Ursala
#import std

#show+

anagrams = mat` * leql$^&h eql|=@rK2tFlSS ^(~&,-<&)* unixdict_dot_txt
```

{{out}}

```txt

evil levi live veil vile
caret carte cater crate trace
alger glare lager large regal
elan lane lean lena neal
angel angle galen glean lange
abel able bale bela elba
```



## Vedit macro language

This implementation first sorts characters of each word using Insertion sort in subroutine SORT_LETTERS.

Then the word list is sorted using built-in Sort function.

Finally, groups of words are analyzed and largest groups are recorded.

The word list is expected to be in the same directory as the script.

```vedit
File_Open("|(PATH_ONLY)\unixdict.txt")

Repeat(ALL) {
    Reg_Copy_Block(10, CP, EOL_Pos)     // original word
    Call("SORT_LETTERS")                // sort letters of the word
    EOL
    IC(' ') Reg_Ins(10)                 // add the original word at eol
    Line(1, ERRBREAK)
}

Sort(0, File_Size)                      // sort list according to anagrams

BOF
Search("|F") Search(' ')                // first word in the list
Reg_Copy_Block(10, BOL_Pos, CP+1)       // reg 10 = sorted anagram word
Reg_Copy_Block(11, CP, EOL_Pos)         // reg 11 = list of words in current group
Reg_Empty(12)                           // reg 12 = list of words in largest groups
Reg_Set(13, "
")
#1 = 1                                  // words in this group
#2 = 2                                  // words in largest group found
Repeat(ALL) {
    Line(1, ERRBREAK)
    if (Match(@10, ADVANCE) == 0) {     // same group as previous word?
        Reg_Copy_Block(11, CP-1, EOL_Pos, APPEND)  // add word to this group
        #1++
    } else {                            // different anagram group
        Search(" ", ERRBREAK)
        if (#1 == #2) {                 // same size as the largest?
            Reg_Set(12, @13, APPEND)    // append newline
            Reg_Set(12, @11, APPEND)    // append word list
        }
        if (#1 > #2) {                  // new larger size of group
            Reg_Set(12, @11)            // replace word list
            #2 = #1
        }
        Reg_Copy_Block(10, BOL_Pos, CP+1)
        Reg_Copy_Block(11, CP, EOL_Pos) // first word of new group
        #1 = 1
    }
}

Buf_Quit(OK)                    // close word list file
Buf_Switch(Buf_Free)            // output results in a new edit buffer
Reg_Ins(12)                     // display all groups of longest anagram words
Return

////////////////////////////////////////////////////////////////////
//
// Sort characters in current line using Insertion sort
//
:SORT_LETTERS:
GP(EOL_pos) #9 = Cur_Col-1
for (#1 = 2; #1 <= #9; #1++) {
    Goto_Col(#1) #8 = Cur_Char
    #2 = #1
    while (#2 > 1) {
        #7 = Cur_Char(-1)
        if (#7 <= #8) { break }
        Ins_Char(#7, OVERWRITE)
        #2--
        Goto_Col(#2)
    }
    Ins_Char(#8, OVERWRITE)
}
return
```

{{out}}

```txt

 abel able bale bela elba
 caret carte cater crate trace
 angel angle galen glean lange
 alger glare lager large regal
 elan lane lean lena neal
 evil levi live veil vile

```

{{omit from|PARI/GP|No real capacity for string manipulation}}


## VBA


```vb

Option Explicit

Public Sub Main_Anagram()
Dim varReturn
Dim temp
Dim strContent As String
Dim strFile As String
Dim Num As Long
Dim i As Long
Dim countTime As Single

    'Open & read txt file
    Num = FreeFile
    strFile = "C:\Users\" & Environ("Username") & "\Desktop\unixdict.txt"
    Open strFile For Input As #Num
        strContent = Input(LOF(1), #Num)
    Close #Num
    Debug.Print UBound(Split(strContent, vbCrLf)) + 1 & " words, in the dictionary"
    countTime = Timer
    'Compute
    varReturn = Anagrams(strContent)
    'Return
    Debug.Print "The anagram set(s) with the greatest number of words (namely " & UBound(varReturn, 2) & ") is : "
    Debug.Print ""
    For i = LBound(varReturn, 1) To UBound(varReturn, 1)
        ReDim temp(LBound(varReturn, 2) To UBound(varReturn, 2))
        For Num = LBound(varReturn, 2) To UBound(varReturn, 2)
            temp(Num) = varReturn(i, Num)
        Next
        SortOneDimArray temp, LBound(temp), UBound(temp)
        Debug.Print Mid(Join(temp, ", "), 3)
    Next i
    Debug.Print ""
    Debug.Print "Time to go : " & Timer - countTime & " seconds."
End Sub

Private Function Anagrams(strContent As String) As Variant
Dim arrList
Dim arrTemp() As String
Dim arrReturn() As String
Dim Num As Long
Dim lngCountTemp As Long
Dim lngCount As Long
Dim i As Long

    'Put the content of txt file in an One Dim Array
    arrList = Split(strContent, vbCrLf)
    ReDim arrTemp(0 To UBound(arrList, 1), 0 To 2)
    'Transfer Datas in a 2nd Array Multi-Dim
        'Col 0 = words with letters sorted
        'Col 1 = words
        'Col 2 = Number of same words with letters sorted in the list
    For Num = LBound(arrList) To UBound(arrList)
        arrTemp(Num, 0) = SortLetters(CStr(arrList(Num)), Chr(0))
        arrTemp(Num, 1) = CStr(arrList(Num))
    Next
    SortTwoDimArray arrTemp, LBound(arrTemp, 1), UBound(arrTemp, 1), 0
    For Num = LBound(arrTemp, 1) To UBound(arrTemp, 1)
        arrTemp(Num, 2) = NbIf(arrTemp(Num, 0), arrTemp, Num, 0)
        If arrTemp(Num, 2) > lngCountTemp Then lngCountTemp = arrTemp(Num, 2)
    Next
    'return
    ReDim arrReturn(0 To lngCountTemp, 0)
    For Num = LBound(arrTemp, 1) To UBound(arrTemp, 1)
        If lngCountTemp = arrTemp(Num, 2) Then
            ReDim Preserve arrReturn(0 To lngCountTemp, 0 To lngCount)
            For i = 0 To lngCountTemp - 1
                arrReturn(i, lngCount) = arrTemp(Num + i, 1)
            Next i
            lngCount = lngCount + 1
        End If
    Next Num
    Anagrams = Transposition(arrReturn)
End Function

Private Function SortLetters(s As String, sep As String) As String
Dim temp

    temp = Split(StrConv(s, vbUnicode), sep)
    SortOneDimArray temp, LBound(temp), UBound(temp)
    SortLetters = Join(temp, sep)
End Function

Private Function NbIf(strValue As String, arr As Variant, lngInd As Long, Optional lngColumn As Long) As Long
Dim i As Long
Dim lngCount As Long

    For i = lngInd To UBound(arr, 1)
        If arr(i, lngColumn) = strValue Then
            lngCount = lngCount + 1
        Else
            Exit For
        End If
    Next i
    NbIf = lngCount
End Function

Private Function Transposition(ByRef myArr As Variant) As Variant
Dim tabl
Dim i As Long
Dim j As Long

    ReDim tabl(LBound(myArr, 2) To UBound(myArr, 2), LBound(myArr, 1) To UBound(myArr, 1))
    For i = LBound(myArr, 1) To UBound(myArr, 1)
        For j = LBound(myArr, 2) To UBound(myArr, 2)
            tabl(j, i) = myArr(i, j)
        Next j
    Next i
    Transposition = tabl
    Erase tabl
End Function

Private Sub SortOneDimArray(ByRef myArr As Variant, mini As Long, Maxi As Long)
Dim i As Long
Dim j As Long
Dim Pivot As Variant
Dim temp As Variant

    On Error Resume Next
    i = mini: j = Maxi
    Pivot = myArr((mini + Maxi) \ 2)
    While i <= j
        While myArr(i) < Pivot And i < Maxi: i = i + 1: Wend
        While Pivot < myArr(j) And j > mini: j = j - 1: Wend
        If i <= j Then
            temp = myArr(i)
            myArr(i) = myArr(j)
            myArr(j) = temp
            i = i + 1: j = j - 1
        End If
    Wend
    If (mini < j) Then Call SortOneDimArray(myArr, mini, j)
    If (i < Maxi) Then Call SortOneDimArray(myArr, i, Maxi)
End Sub

Private Sub SortTwoDimArray(ByRef myArr As Variant, mini As Long, Maxi As Long, Optional Colonne As Long = 0)
Dim i As Long
Dim j As Long
Dim Pivot As Variant
Dim myArrTemp As Variant
Dim ColTemp As Long

    On Error Resume Next
    i = mini: j = Maxi
    Pivot = myArr((mini + Maxi) \ 2, Colonne)
    While i <= j
        While myArr(i, Colonne) < Pivot And i < Maxi: i = i + 1: Wend
        While Pivot < myArr(j, Colonne) And j > mini: j = j - 1: Wend
        If i <= j Then
            ReDim myArrTemp(LBound(myArr, 2) To UBound(myArr, 2))
            For ColTemp = LBound(myArr, 2) To UBound(myArr, 2)
                myArrTemp(ColTemp) = myArr(i, ColTemp)
                myArr(i, ColTemp) = myArr(j, ColTemp)
                myArr(j, ColTemp) = myArrTemp(ColTemp)
            Next ColTemp
            Erase myArrTemp
            i = i + 1: j = j - 1
        End If
    Wend
    If (mini < j) Then Call SortTwoDimArray(myArr, mini, j, Colonne)
    If (i < Maxi) Then Call SortTwoDimArray(myArr, i, Maxi, Colonne)
End Sub
```

{{out}}

```txt
25104 words, in the dictionary
The anagram set(s) with the greatest number of words (namely 5) is :

abel, able, bale, bela, elba
caret, carte, cater, crate, trace
angel, angle, galen, glean, lange
alger, glare, lager, large, regal
elan, lane, lean, lena, neal
evil, levi, live, veil, vile

Time to go : 2,464844 seconds.
```



## Visual Basic .NET


```vbnet
Imports System.IO
Imports System.Collections.ObjectModel

Module Module1

  Dim sWords As New Dictionary(Of String, Collection(Of String))

  Sub Main()

    Dim oStream As StreamReader = Nothing
    Dim sLines() As String = Nothing
    Dim sSorted As String = Nothing
    Dim iHighCount As Integer = 0
    Dim iMaxKeyLength As Integer = 0
    Dim sOutput As String = ""

    oStream = New StreamReader("unixdict.txt")
    sLines = oStream.ReadToEnd.Split(New String() {vbCrLf}, StringSplitOptions.RemoveEmptyEntries)
    oStream.Close()

    For i As Integer = 0 To sLines.GetUpperBound(0)
      sSorted = SortCharacters(sLines(i))

      If Not sWords.ContainsKey(sSorted) Then sWords.Add(sSorted, New Collection(Of String))

      sWords(sSorted).Add(sLines(i))

      If sWords(sSorted).Count > iHighCount Then
        iHighCount = sWords(sSorted).Count

        If sSorted.Length > iMaxKeyLength Then iMaxKeyLength = sSorted.Length
      End If
    Next

    For Each sKey As String In sWords.Keys
      If sWords(sKey).Count = iHighCount Then
        sOutput &= "[" & sKey.ToUpper & "]" & Space(iMaxKeyLength - sKey.Length + 1) & String.Join(", ", sWords(sKey).ToArray()) & vbCrLf
      End If
    Next

    Console.WriteLine(sOutput)
    Console.ReadKey()

  End Sub

  Private Function SortCharacters(ByVal s As String) As String

    Dim sReturn() As Char = s.ToCharArray()
    Dim sTemp As Char = Nothing

    For i As Integer = 0 To sReturn.GetUpperBound(0) - 1
      If (sReturn(i + 1)) < (sReturn(i)) Then
        sTemp = sReturn(i)
        sReturn(i) = sReturn(i + 1)
        sReturn(i + 1) = sTemp
        i = -1
      End If
    Next

    Return CStr(sReturn)

  End Function

End Module
```

{{out}}
<PRE>
[ABEL]  abel, able, bale, bela, elba
[AEGLR] alger, glare, lager, large, regal
[AEGLN] angel, angle, galen, glean, lange
[ACERT] caret, carte, cater, crate, trace
[AELN]  elan, lane, lean, lena, neal
[EILV]  evil, levi, live, veil, vile
</PRE>


## Yabasic


```Yabasic
filename$ = "unixdict.txt"
maxw = 0 : c = 0 : dimens(c)
i = 0
dim p(100)

if (not open(1,filename$)) error "Could not open '"+filename$+"' for reading"

print "Be patient, please ...\n"

while(not eof(1))
  line input #1 a$
  c = c + 1
  p$(c) = a$
  po$(c) = sort$(lower$(a$))
  count = 0
  head = 0
  insert(1)
  if not(mod(c, 10)) dimens(c)
wend

for n = 1 to i
	nw = p(n)
	repeat
		print p$(nw)," ";
		nw = d(nw,2)
	until(not nw)
	print
next n

print "\n", peek("secondsrunning"), " sec"

sub sort$(a$)
	local n, i, t$, c1$, c2$

	for n = 1 to len(a$) - 1
		for i = n + 1 to len(a$)
			c1$ = mid$(a$, n, 1) : c2$ = mid$(a$, i, 1)
			if c1$ > c2$ then
				t$ = c1$
				c1$ = c2$
				c2$ = t$
				mid$(a$, n, 1) = c1$ : mid$(a$, i, 1) = c2$
			end if
		next i
	next n
	return a$
end sub

sub dimens(c)
	redim p$(c + 10)
	redim po$(c + 10)
	redim d(c + 10, 3)
end sub

sub insert(j)
	local p

	if po$(c) < po$(j) then
		p = 1
	elseif po$(c) = po$(j) then
		p = 2
		if count = 0 head = j
		count = count + 1
		if count > maxw then
  			i = 1
  			p(i) = head
  			maxw = count
  		elseif count = maxw then
  			i = i + 1
  			p(i) = head
  		end if
	else
		p = 3
	end if

	if d(j,p) then
		insert(d(j,p))
	else
		d(j,p) = c
	end if
end sub
```



## zkl


```zkl
File("unixdict.txt").read(*)  // dictionary file to blob, copied from web
   // blob to dictionary: key is word "fuzzed", values are anagram words
   .pump(Void,T(fcn(w,d){
      key:=w.split("").sort().concat();  // fuzz word to key
      d.appendV(key,w);  // add or append w
   },d:=Dictionary(0d60_000)));

d.filter(fcn([(k,v)]){ v.len()>3 })  // prune to list of # words > 3
   .sort(fcn([(_,v1)],[(_,v2)]){ v1.len()>v2.len() }) // sort by word count
   [0,10].pump(Console.println,'wrap([(zz,v)]){    // and print 10 biggest
	"%d:%s: %s".fmt(v.len(),zz.strip(),
	    v.apply("strip").concat(","))
   });
```

{{out}}

```txt

5:aegln: angel,angle,galen,glean,lange
5:aeglr: alger,glare,lager,large,regal
5:eilv: evil,levi,live,veil,vile
5:abel: abel,able,bale,bela,elba
5:aeln: elan,lane,lean,lena,neal
5:acert: caret,carte,cater,crate,trace

4:aeirs: aires,aries,arise,raise
4:alstu: latus,sault,talus,tulsa
4:aekst: keats,skate,stake,steak
4:aelnp: nepal,panel,penal,plane

```

In the case where it is desirable to get the dictionary from the web, use this code:

```zkl
URL:="http://www.puzzlers.org/pub/wordlists/unixdict.txt";
var ZC=Import("zklCurl");
unixdict:=ZC().get(URL); //--> T(Data,bytes of header, bytes of trailer)
unixdict=unixdict[0].del(0,unixdict[1]);  // remove HTTP header
File("unixdict.txt","w").write(unixdict);
```

