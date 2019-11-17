+++
title = "ABC Problem"
description = ""
date = 2019-10-17T04:29:11Z
aliases = []
[extra]
id = 17061
[taxonomies]
categories = ["task"]
languages = [
  "11l",
  "360_assembly",
  "8th",
  "acurity_architect",
  "ada",
  "algol_68",
  "algol_w",
  "apex",
  "apl",
  "applescript",
  "astro",
  "autohotkey",
  "awk",
  "batch_file",
  "bacon",
  "basic",
  "commodore_basic",
  "sinclair_zx81_basic",
  "bbc_basic",
  "bracmat",
  "c",
  "c++",
  "c_sharp",
  "regex",
  "ceylon",
  "clojure",
  "coffeescript",
  "common_lisp",
  "component_pascal",
  "d",
  "delphi",
  "dyalect",
  "echolisp",
  "ela",
  "elena",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "factor",
  "fbsl",
  "fortran",
  "freebasic",
  "gambas",
  "go",
  "groovy",
  "harbour",
  "haskell",
  "j",
  "java",
  "javascript",
  "es5",
  "es6",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "liberty_basic",
  "logo",
  "lua",
  "maple",
  "m2000_interpreter",
  "mathematica",
  "wolfram_language",
  "matlab",
  "maxscript",
  "mercury",
  "miniscript",
  "nim",
  "oberon_2",
  "objeck",
  "ocaml",
  "oforth",
  "openedge_progress",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powerbasic",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "r",
  "racket",
  "rapidq",
  "red",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sequencel",
  "sidef",
  "simula",
  "smalltalk",
  "spad",
  "swift",
  "tcl",
  "tuscript",
  "txr",
  "unix_shell",
  "utfool",
  "vba",
  "yabasic",
  "zkl",
  "zonnon",
  "zx_spectrum_basic",
]
tags = ["game", "puzzle"]
+++

You are given a collection of ABC blocks
(maybe like the ones you had when you were a kid).

There are twenty blocks with two letters on each block.

A complete alphabet is guaranteed amongst all sides of the blocks.

The sample collection of blocks:

```txt
(B O)
(X K)
(D Q)
(C P)
(N A)
(G T)
(R E)
(T G)
(Q D)
(F S)
(J W)
(H U)
(V I)
(A N)
(O B)
(E R)
(F S)
(L Y)
(P C)
(Z M)
```

## Task

Write a function that takes a string (word)
and determines whether the word can be spelled
with the given collection of blocks.


The rules are simple:

1. Once a letter on a block is used that block cannot be used again
1. The function should be case-insensitive
1. Show the output on this page for the following 7 words
    in the following example


## Example

```python
>>> can_make_word("A")
True
>>> can_make_word("BARK")
True
>>> can_make_word("BOOK")
False
>>> can_make_word("TREAT")
True
>>> can_make_word("COMMON")
False
>>> can_make_word("SQUAD")
True
>>> can_make_word("CONFUSE")
True
```


## 11l

Translated from Python

```11l
F can_make_word(word)
   I word == ‘’
      R 0B

   V blocks_remaining = ‘BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM’.split(‘ ’)

   L(ch) word.uppercase()
      L(block) blocks_remaining
         I ch C block
            blocks_remaining.remove(block)
            L.break
         L.was_no_break
            R 0B
   R 1B

print([‘’, ‘a’, ‘baRk’, ‘booK’, ‘treat’, ‘COMMON’, ‘squad’, ‘Confused’].map(w -> ‘'’w‘': ’can_make_word(w)).join(‘, ’))
```



## 360 Assembly

The program uses one ASSIST macro (XPRNT) to keep the code as short as possible.

```360asm
*        ABC Problem               21/07/2016
ABC      CSECT
         USING  ABC,R13            base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    prolog
         ST     R13,4(R15)         " <-
         ST     R15,8(R13)         " ->
         LR     R13,R15            " addressability
         LA     R8,1               l=1
LOOPL    C      R8,=A(NN)          do l=1 to hbound(words)
         BH     ELOOPL
         LR     R1,R8              l
         MH     R1,=H'20'          *20
         LA     R10,WORDS-20(R1)   @words(l)
         MVC    STATUS,=CL5'true'  cflag='true'
         MVC    TBLOCKS,BLOCKS     tblocks=blocks
         MVC    CC(1),0(R10)       cc=substr(words(l),1,1)
         LA     R6,1               i=1
LOOPI    CLI    CC,C' '            do while cc<>' '
         BE     ELOOPI
         SR     R7,R7              k=0
         LH     R0,=H'1'           m=1
LOOPM    CH     R0,=AL2(L'TBLOCKS) do m=1 to length(tblocks)
         BH     ELOOPM
         LA     R5,TBLOCKS-1       @tblocks[0]
         AR     R5,R0              @tblocks[m]
         CLC    0(1,R5),CC         if substr(tblocks,m,1)=cc
         BNE    INDEXM
         LR     R7,R0              k=m=index(tblocks,cc)
         B      ELOOPM
INDEXM   AH     R0,=H'1'           m=m+1
         B      LOOPM
ELOOPM   LTR    R7,R7              if k=0
         BNZ    OKK
         MVC    STATUS,=CL5'false' cflag='false'
         B      EIFK0
OKK      LA     R4,TBLOCKS-2       @tblocks[-1]
         AR     R4,R7              +k
         CLI    0(R4),C'('         if substr(tblocks,k-1,1)='('
         BNE    SECOND
         LA     R0,1               j=1
         B      EIFBLOCK
SECOND   LA     R0,3               j=3
EIFBLOCK LR     R2,R7              k
         SR     R2,R0              k-j
         LA     R4,TBLOCKS-1       @tblocks[0]
         AR     R4,R2              @tblocks[k-j]
         MVC    0(5,R4),=CL5' '    substr(tblocks,k-j,5)='     '
EIFK0    LA     R6,1(R6)           i=i+1
         LR     R4,R10             @words
         AR     R4,R6              +i
         BCTR   R4,0               -1
         MVC    CC,0(R4)           cc=substr(words,i,1)
         B      LOOPI
ELOOPI   MVC    PG(20),0(R10)      tabword(l)
         MVC    PG+20(5),STATUS    status
         XPRNT  PG,80              print buffer
         LA     R8,1(R8)           l=l+1
         B      LOOPL
ELOOPL   L      R13,4(0,R13)       epilog
         LM     R14,R12,12(R13)    " restore
         XR     R15,R15            " rc=0
         BR     R14                exit
WORDS    DC     CL20'A',CL20'BARK',CL20'BOOK',CL20'TREAT',CL20'COMMON'
         DC     CL20'SQUAD',CL20'CONFUSE'
BLOCKS   DS     0CL122
 DC CL61'((B O) (X K) (D Q) (C P) (N A) (G T) (R E) (T G) (Q D) (F S) '
 DC CL61'(J W) (H U) (V I) (A N) (O B) (E R) (F S) (L Y) (P C) (Z M)) '
TBLOCKS  DS     CL(L'BLOCKS)       work blocks
CC       DS     CL1                letter to find
STATUS   DS     CL5                true/false
PG       DC     CL80' '            buffer
         YREGS
NN       EQU    (BLOCKS-WORDS)/L'WORDS  number of words
         END    ABC
```

Output:

```txt

A                   true
BARK                true
BOOK                false
TREAT               true
COMMON              false
SQUAD               true
CONFUSE             true

```



## 8th

```360asm
\
### ==================================================================================

\ You are given a collection of ABC blocks
\ There are twenty blocks with two letters on each block.
\ A complete alphabet is guaranteed amongst all sides of the blocks.
\
\ Write a function that takes a string (word) and determines whether
\ the word can be spelled with the given collection of blocks.
\
\ Rules:
\ 1. Once a letter on a block is used that block cannot be used again
\ 2. The function should be case-insensitive
\ 3. Show the output on this page for the following 7 words in the following example
\    can_make_word(???) where ??? is resp.:
\        "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"
\
\ NOTE:
\ to make the program readable for even n00bs, I have a comment at the end of each line.
\ The comments take the form of:
\                    \ <stack> | <rstack>
\ in order to be able to follow exactly what the program does.
\
### ==================================================================================


["BO","XK","DQ","CP","NA","GT","RE","TG","QD","FS","JW","HU","VI","AN","OB","ER","FS","LY","PC","ZM"] var, blks
["a", "AbBa", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"] var, chkwrds

needs stack/rstack

a:new var, paths \ Keeps the combinatory explosion of letter paths
var wrd
var success
var ix

: uni2char "" swap s:+ ;

: char2uni 0 s:@ nip ;

: rreset rstack st:clear drop ;

: addoneletter \ ix path --                   \ ix path | letter
  r@                                          \ ix path letter | letter
  s:+                                         \ ix newpath | letter
  paths @                                     \ ix newpath paths | letter
  -rot                                        \ paths ix newval | letter
  a:!                                         \ paths | letter
  drop                                        \ | letter
  ;

: oneletter \ letter --                       \ letter
  >r                                          \ | letter
  paths @ ' addoneletter a:each drop          \ | letter
  ;

: addtwoletters \ ix path --                  \ ix path | letter1 letter2 halflen
  swap                                        \ path ix | letter1 letter2 halflen
  dup                                         \ path ix ix | letter1 letter2 halflen
  r@                                          \ path ix ix halflen | letter1 letter2 halflen
  n:<                                         \ path ix bool | letter1 letter2 halflen
  if                                          \ path ix | letter1 letter2 halflen
    swap                                      \ ix path | letter1 letter2 halflen
    1 rpick                                   \ ix path letter | letter1 letter2 halflen
  else
    swap                                      \ ix path | letter1 letter2 halflen
    2 rpick                                   \ ix path letter | letter1 letter2 halflen
  then
  s:+                                         \ ix newpath | letter1 letter2 halflen
  paths @                                     \ ix newpath paths | letter1 letter2 halflen
  -rot                                        \ paths ix newpath | letter1 letter2 halflen
  a:!                                         \ paths | letter1 letter2 halflen
  drop                                        \ | letter1 letter2 halflen
  ;

: twoletters \ letters --                     \ letters
  \ fetch the 2 letters
  dup                                         \ letters letters
  1 s:lsub                                    \ letters letter1
  >r                                          \ letters | letter1
  1 s:rsub                                    \ letter2 | letter1
  >r                                          \ | letter1 letter2
  \ duplicate paths in itself
  paths @ dup a:+                             \ paths | letter1 letter2
  \ halfway length of array
  a:len                                       \ paths len | letter1 letter2
  2 /                                         \ paths halflen | letter1 letter2
  >r                                          \ paths | letter1 letter2 halflen
  \ add letters to paths
  ' addtwoletters a:each drop                 \ | letter1 letter2 halflen
  rreset                                      \
  ;

: chkletter \ letter -- letter                \ letter
  dup                                         \ letter letter
  wrd @                                       \ letter letter word
  swap uni2char                               \ letter word letter
  s:search                                    \ letter word index
  null?                                       \ letter word index bool
  nip                                         \ letter word bool
  if                                          \ letter word
    2drop                                     \
    ""                                        \ letter
  else                                        \ letter word
    drop                                      \ letter
  then                                        \
  ;

: buildpaths \ ix blk --                      \ ix blk
  nip                                         \ blk
  ' chkletter s:map                           \ resultletters
  s:len                                       \ resultletters len
  dup                                         \ resultletters len len
  0                                           \ resultletters len len 0
  n:=                                         \ resultletters len bool
  if                                          \ resultletters len
    \ This block contains no letters of current word
    2drop                                     \
    ;;                                        \ exit word
  then                                        \ resultletters len
  1                                           \ resultletters len 1
  n:=                                         \ resultletters bool
  if                                          \ resultletters
    oneletter                                 \
  else                                        \ resultletters
    twoletters                                \
  then
  ;

: chkokpath \ ix wrdch --                     \ ix wrdch | path
  swap                                        \ wrdch ix | path
  ix !                                        \ wrdch | path
  r@                                          \ wrdch path | path
  dup                                         \ wrdch path path | path
  ""                                          \ wrdch path path "" | path
  s:=                                         \ wrdch path bool | path
  if                                          \ wrdch path | path
    \ Path is empty - no match
    2drop                                     \ | path
    break                                     \ | path
    ;;                                        \ | path
  then
  swap                                        \ path wrdch | path
  uni2char                                    \ path wrdch | path
  s:search                                    \ path pos | path
  null?                                       \ path pos bool | path
  if                                          \ path pos | path
    \ Letter not found in path - no match
    2drop                                     \ | path
    break                                     \ | path
  else                                        \ path pos | path
    wrd @                                     \ path pos wrd | path
    s:len                                     \ path pos wrd len | path
    nip                                       \ path pos len | path
    n:1-                                      \ path pos cix | path
    ix @                                      \ path pos cix ix | path
    n:=                                       \ path pos bool | path
    if                                        \ path pos | path
      \ We have a match!
      true success !                          \ path pos | path
      2drop                                   \ | path
      break                                   \ | path
    else                                      \ path pos | path
      1                                       \ path pos len | path
      s:-                                     \ restpath | path
      rdrop >r                                \ | restpath
    then
  then
  ;

: chkpath \ ix path --                        \ ix path
  nip                                         \ path
  >r                                          \ | path
  wrd @                                       \ wrd | path
  ' chkokpath s:each                          \ | path
  rdrop                                       \
  success @                                   \ success
	if                                          \
	  break                                     \
  then
  ;

: chkwrd \ ix wrd --                          \ ix wrd
  nip                                         \ wrd
  s:uc                                        \ wrdupper
  "Word=" . dup .                             \ wrdupper
  wrd !                                       \
  \ other word - clear paths
  paths @ a:clear "" a:push drop              \
  \ create path tree for this word
  blks @ ' buildpaths a:each drop             \
  \ check if word can be made from a path
  false success !                             \
  paths @ ' chkpath a:each drop               \
  success @                                   \ success
  "\t\t" . . cr                               \
  ;

: app:main
  chkwrds @ ' chkwrd a:each drop              \ check if word can be made
  bye
  ;

```



## Acurity Architect


```txt

Using #HASH-OFF

```


```acurity architect

FUNCTION bCAN_MAKE_WORD(zWord: STRING): BOOLEAN
  VAR sBlockCount: SHORT
  VAR sWordCount: SHORT
  VAR sWordLength: SHORT
  VAR zLetter: STRING
  VAR zBlock: STRING
  VAR zBlockList: STRING
  VAR zUsedBlocks: STRING
  VAR zWord: STRING
  //
  SET zWord = UPPER(zWord)
  SET zBlockList = "BO,XK,DQ,CP,NA,GT,RE,TG,QD,FS,JW,HU,VI,AN,OB,ER,FS,LY,PC,ZM"
  SET sWordLength = LENGTH(zWord)
  //
  DO sWordCount = 1 TO sWordLength
    DO sBlockCount = 1 TO OCCURS(zBlockList, ",")
      SET zLetter = SUBSTR(zWord, sWordCount, 1)
      SET zBlock = GET_TOKEN(zBlockList, ",", sBlockCount)
      IF INDEX(zBlock, zLetter, 1) > 0 AND INDEX(zUsedBlocks, zBlock + STR(sBlockCount), 1) = 0
        SET zUsedBlocks = zUsedBlocks + zBlock + STR(sBlockCount) + ","
        BREAK
      ENDIF
    ENDDO
  ENDDO
  RETURN OCCURS(zUsedBlocks, ",") = sWordLength
ENDFUNCTION

```

Output:

```txt
bCAN_MAKE_WORD("A") returns TRUE
bCAN_MAKE_WORD("BARK") returns TRUE
bCAN_MAKE_WORD("BOOK") returns FALSE
bCAN_MAKE_WORD("TREAT") returns TRUE
bCAN_MAKE_WORD("COMMON") returns FALSE
bCAN_MAKE_WORD("SQUAD") returns TRUE
bCAN_MAKE_WORD("CONFUSE") returns TRUE
```



## Ada

```txt
Build with gnatchop abc.ada; gnatmake abc_problem
```

```ada
with Ada.Characters.Handling;
use Ada.Characters.Handling;


package Abc is
    type Block_Faces is array(1..2) of Character;
    type Block_List is array(positive range <>) of Block_Faces;
    function Can_Make_Word(W: String; Blocks: Block_List) return Boolean;
end Abc;


package body Abc is

function Can_Make_Word(W: String; Blocks: Block_List) return Boolean is
    Used : array(Blocks'Range) of Boolean := (Others => False);
    subtype wIndex is Integer range W'First..W'Last;
    wPos : wIndex;
begin
    if W'Length = 0 then
        return True;
    end if;
    wPos := W'First;
    while True loop
        declare
            C : Character := To_Upper(W(wPos));
            X : constant wIndex := wPos;
        begin
            for I in Blocks'Range loop
                if (not Used(I)) then
                    if C = To_Upper(Blocks(I)(1)) or C = To_Upper(Blocks(I)(2)) then
                        Used(I) := True;
                        if wPos = W'Last then
                            return True;
                        end if;
                        wPos := wIndex'Succ(wPos);
                        exit;
                    end if;
                end if;
            end loop;
            if X = wPos then
                return False;
            end if;
        end;
    end loop;
    return False;
end Can_Make_Word;

end Abc;

with Ada.Text_IO, Ada.Strings.Unbounded, Abc;
use Ada.Text_IO, Ada.Strings.Unbounded, Abc;

procedure Abc_Problem is
    Blocks : Block_List := (
          ('B','O'), ('X','K'), ('D','Q'), ('C','P')
        , ('N','A'), ('G','T'), ('R','E'), ('T','G')
        , ('Q','D'), ('F','S'), ('J','W'), ('H','U')
        , ('V','I'), ('A','N'), ('O','B'), ('E','R')
        , ('F','S'), ('L','Y'), ('P','C'), ('Z','M')
    );
    function "+" (S : String) return Unbounded_String renames To_Unbounded_String;
    words : array(positive range <>) of Unbounded_String := (
          +"A"
        , +"BARK"
        , +"BOOK"
        , +"TREAT"
        , +"COMMON"
        , +"SQUAD"
        , +"CONFUSE"
        -- Border cases:
        -- , +"CONFUSE2"
        -- , +""
    );
begin
    for I in words'Range loop
        Put_Line ( To_String(words(I)) & ": " & Boolean'Image(Can_Make_Word(To_String(words(I)),Blocks)) );
    end loop;
end Abc_Problem;

```


Output:

```txt

A: TRUE
BARK: TRUE
BOOK: FALSE
TREAT: TRUE
COMMON: FALSE
SQUAD: TRUE
CONFUSE: TRUE

```



## ALGOL 68

Works with ALGOL 68G (tested with release 2.8.win32)

```algol68
# determine whether we can spell words with a set of blocks                  #

# construct the list of blocks                                               #
[][]STRING blocks = ( ( "B", "O" ), ( "X", "K" ), ( "D", "Q" ), ( "C", "P" )
                    , ( "N", "A" ), ( "G", "T" ), ( "R", "E" ), ( "T", "G" )
                    , ( "Q", "D" ), ( "F", "S" ), ( "J", "W" ), ( "H", "U" )
                    , ( "V", "I" ), ( "A", "N" ), ( "O", "B" ), ( "E", "R" )
                    , ( "F", "S" ), ( "L", "Y" ), ( "P", "C" ), ( "Z", "M" )
                    );

# Returns TRUE if we can spell the word using the blocks, FALSE otherwise    #
# Returns TRUE for an empty string                                           #
PROC can spell = ( STRING word, [][]STRING blocks )BOOL:
    BEGIN

        # construct a set of flags to indicate whether the blocks are used   #
        # or not                                                             #
        [ 1 LWB blocks : 1 UPB blocks ]BOOL used;
        FOR block pos FROM LWB used TO UPB used
        DO
            used[ block pos ] := FALSE
        OD;

        # initialliy assume we can spell the word                            #
        BOOL result := TRUE;

        # check we can spell the word with the set of blocks                 #
        FOR word pos FROM LWB word TO UPB word WHILE result
        DO
            CHAR c = IF   is lower( word[ word pos ] )
                     THEN to upper( word[ word pos ] )
                     ELSE           word[ word pos ]
                     FI;

            # look through the unused blocks for the current letter          #
            BOOL found := FALSE;
            FOR block pos FROM 1 LWB blocks TO 1 UPB blocks
            WHILE NOT found
            DO
                IF  (  c = blocks[ block pos ][ 1 ][ 1 ]
                    OR c = blocks[ block pos ][ 2 ][ 1 ]
                    )
                AND NOT used[ block pos ]
                THEN
                    # found an unused block with the required letter         #
                    found             := TRUE;
                    used[ block pos ] := TRUE
                FI
            OD;

            result := found

        OD;

        result
    END; # can spell #


main: (

    # test the can spell procedure                                           #
    PROC test can spell = ( STRING word, [][]STRING blocks )VOID:
        write( ( ( "can spell: """
                 + word
                 + """ -> "
                 + IF can spell( word, blocks ) THEN "yes" ELSE "no" FI
                 )
               , newline
               )
             );

    test can spell( "A",       blocks );
    test can spell( "BaRK",    blocks );
    test can spell( "BOOK",    blocks );
    test can spell( "TREAT",   blocks );
    test can spell( "COMMON",  blocks );
    test can spell( "SQUAD",   blocks );
    test can spell( "CONFUSE", blocks )

)

```

Output:

```txt

can spell: "A" -> yes
can spell: "BaRK" -> yes
can spell: "BOOK" -> no
can spell: "TREAT" -> yes
can spell: "COMMON" -> no
can spell: "SQUAD" -> yes
can spell: "CONFUSE" -> yes

```



## ALGOL W


```algolw
% determine whether we can spell words with a set of blocks                  %
begin
    % Returns true  if we can spell the word using the blocks,               %
    %         false otherwise                                                %
    % As strings are fixed length in Algol W, the length of the string is    %
    % passed as a separate parameter                                         %
    logical procedure canSpell ( string(20) value word
                               ; integer    value wordLength
                               ) ;
    begin

        % convert a character to upper-case                                  %
        % assumes the letters are contiguous in the character set            %
        % as in ASCII and Unicode - not correct for EBCDIC                   %
        string(1) procedure toUpper( string(1) value c ) ;
            if c < "a" or c > "z" then c
                                  else code( ( decode( c ) - decode( "a" ) )
                                           + decode( "A" )
                                           ) ;

        logical       spellable;
        integer       wordPos,  blockPos;
        string(20)    letters1, letters2;

        % make local copies the faces so we can remove the used blocks       %
        letters1 := face1;
        letters2 := face2;

        % check we can spell the word with the set of blocks                 %
        spellable := true;
        wordPos   := 0;
        while wordPos < wordLength and spellable do begin
            string(1) letter;
            letter    := toUpper( word( wordPos // 1 ) );
            if letter not = " " then begin
                spellable := false;
                blockPos  := 0;
                while blockPos < 20 and not spellable do begin
                    if letter = letters1( blockPos // 1 )
                    or letter = letters2( blockPos // 1 )
                    then begin
                        % found the letter - remove the used block from the  %
                        % remaining blocks                                   %
                        letters1( blockPos // 1 ) := " ";
                        letters2( blockPos // 1 ) := " ";
                        spellable := true
                    end;
                    blockPos := blockPos + 1
                end
            end;
            wordPos := wordPos + 1;
        end;

        spellable
    end canSpell ;

    % the letters available on the faces of the blocks                       %
    string(20) face1, face2;
    face1 := "BXDCNGRTQFJHVAOEFLPZ";
    face2 := "OKQPATEGDSWUINBRSYCM";

    begin
        % test the can spell procedure                                       %
        procedure testCanSpell ( string(20) value word
                               ; integer    value wordLength
                               ) ;
            write( if canSpell( word, wordLength ) then "can   " else "cannot"
                 , " spell """
                 , word
                 , """"
                 );

        testCanSpell( "a",       1 );
        testCanSpell( "bark",    4 );
        testCanSpell( "BOOK",    4 );
        testCanSpell( "treat",   5 );
        testCanSpell( "commoN",  6 );
        testCanSpell( "Squad",   5 );
        testCanSpell( "confuse", 7 )
    end
end.
```

Output:

```txt

can    spell "a                   "
can    spell "bark                "
cannot spell "BOOK                "
can    spell "treat               "
cannot spell "commoN              "
can    spell "Squad               "
can    spell "confuse             "

```



## Apex


```Java
static Boolean canMakeWord(List<String> src_blocks, String word) {
    if (String.isEmpty(word)) {
        return true;
    }

    List<String> blocks = new List<String>();
    for (String block : src_blocks) {
        blocks.add(block.toUpperCase());
    }

    for (Integer i = 0; i < word.length(); i++) {
        Integer blockIndex = -1;
        String c = word.mid(i, 1).toUpperCase();

        for (Integer j = 0; j < blocks.size(); j++) {
            if (blocks.get(j).contains(c)) {
                blockIndex = j;
                break;
            }
        }

        if (blockIndex == -1) {
            return false;
        } else {
            blocks.remove(blockIndex);
        }
    }

    return true;
}

List<String> blocks = new List<String>{
    'BO', 'XK', 'DQ', 'CP', 'NA',
    'GT', 'RE', 'TG', 'QD', 'FS',
    'JW', 'HU', 'VI', 'AN', 'OB',
    'ER', 'FS', 'LY', 'PC', 'ZM'
};
System.debug('"": ' + canMakeWord(blocks, ''));
System.debug('"A": ' + canMakeWord(blocks, 'A'));
System.debug('"BARK": ' + canMakeWord(blocks, 'BARK'));
System.debug('"book": ' + canMakeWord(blocks, 'book'));
System.debug('"treat": ' + canMakeWord(blocks, 'treat'));
System.debug('"COMMON": ' + canMakeWord(blocks, 'COMMON'));
System.debug('"SQuAd": ' + canMakeWord(blocks, 'SQuAd'));
System.debug('"CONFUSE": ' + canMakeWord(blocks, 'CONFUSE'));
```

Output:

```txt
"": true
"A": true
"BARK": true
"book": false
"treat": true
"COMMON": false
"SQuAd": true
"CONFUSE": true
```



## APL

Works with Dyalog APL 16.0

```APL
abc←{{0=⍴⍵:1 ⋄ 0=⍴h←⊃⍵:0 ⋄ ∇(t←1↓⍵)~¨⊃h:1 ⋄ ∇(⊂1↓h),t}⍸¨↓⍵∘.∊⍺}
```

Output:

```txt
      )COPY dfns ucase
      b W←(≠∘' '⊆⊢)∘ucase¨'BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM' 'A BaRK BOoK tREaT COmMOn SqUAD CoNfuSE'
      b∘abc¨W
1 1 0 1 0 1 1

```



## AppleScript


### Imperative


```AppleScript
set blocks to {"bo", "xk", "dq", "cp", "na", "gt", "re", "tg", "qd", "fs", ¬
    "jw", "hu", "vi", "an", "ob", "er", "fs", "ly", "pc", "zm"}

canMakeWordWithBlocks("a", blocks)
canMakeWordWithBlocks("bark", blocks)
canMakeWordWithBlocks("book", blocks)
canMakeWordWithBlocks("treat", blocks)
canMakeWordWithBlocks("common", blocks)
canMakeWordWithBlocks("squad", blocks)
canMakeWordWithBlocks("confuse", blocks)

on canMakeWordWithBlocks(theString, constBlocks)
    copy constBlocks to theBlocks
    if theString = "" then return true
    set i to 1
    repeat
        if i > (count theBlocks) then exit repeat
        if character 1 of theString is in item i of theBlocks then
            set item i of theBlocks to missing value
            set theBlocks to strings of theBlocks
            if canMakeWordWithBlocks(rest of characters of theString as string, theBlocks) then
                return true
            end if
        end if
        set i to i + 1
    end repeat
    return false
end canMakeWordWithBlocks
```



### Functional composition


```AppleScript
use framework "Foundation"

-- SPELLING BY BLOCK ----------------------------------------------------------
on run
    set blocks to map(chars, ¬
        |words|("BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM"))

    script blockSpelled
        on |λ|(s)
            intercalate("  ->  ", ¬
                ap({my |quote|, compose({my |not|, my |null|, ¬
                    curry(my spellWith)'s |λ|(blocks), my toUpper})}, {s}))
        end |λ|
    end script

    unlines(map(blockSpelled, ¬
        {"", "A", "BARK", "BoOK", "TrEAT", "COmMoN", "SQUAD", "conFUsE"}))
end run

-- spellWith :: [(Char, Char)] -> String -> [[(Char, Char)]]
on spellWith(blocks, ccs)
    if |null|(ccs) then
        {{}}
    else
        set {c, cs} to uncons(ccs)

        script matchSequence
            on |λ|(pair)
                if elem(c, pair) then

                    script pairUsed
                        on |λ|(xs)
                            {{pair} & xs}
                        end |λ|
                    end script

                    concatMap(pairUsed, spellWith(|delete|(pair, blocks), cs))
                else
                    {}
                end if
            end |λ|
        end script

        concatMap(matchSequence, blocks)
    end if
end spellWith


-- GENERIC FUNCTIONS ----------------------------------------------------------

-- A list of functions applied to a list of arguments
-- (<*> | ap) :: [(a -> b)] -> [a] -> [b]
on ap(fs, xs)
    set lngFs to length of fs
    set lngXs to length of xs
    set lst to {}
    repeat with i from 1 to lngFs
        tell mReturn(item i of fs)
            repeat with j from 1 to lngXs
                set end of lst to |λ|(contents of (item j of xs))
            end repeat
        end tell
    end repeat
    return lst
end ap

-- chars :: String -> [Char]
on chars(s)
    characters of s
end chars

-- compose :: [(a -> a)] -> (a -> a)
on compose(fs)
    script
        on |λ|(x)
            script
                on |λ|(a, f)
                    mReturn(f)'s |λ|(a)
                end |λ|
            end script

            foldr(result, x, fs)
        end |λ|
    end script
end compose

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lst to {}
    set lng to length of xs
    tell mReturn(f)
        repeat with i from 1 to lng
            set lst to (lst & |λ|(contents of item i of xs, i, xs))
        end repeat
    end tell
    return lst
end concatMap

-- curry :: (Script|Handler) -> Script
on curry(f)
    script
        on |λ|(a)
            script
                on |λ|(b)
                    |λ|(a, b) of mReturn(f)
                end |λ|
            end script
        end |λ|
    end script
end curry

-- delete :: Eq a => a -> [a] -> [a]
on |delete|(x, xs)
    set mbIndex to elemIndex(x, xs)
    set lng to length of xs

    if mbIndex is not missing value then
        if lng > 1 then
            if mbIndex = 1 then
                items 2 thru -1 of xs
            else if mbIndex = lng then
                items 1 thru -2 of xs
            else
                tell xs to items 1 thru (mbIndex - 1) & ¬
                    items (mbIndex + 1) thru -1
            end if
        else
            {}
        end if
    else
        xs
    end if
end |delete|

-- elem :: Eq a => a -> [a] -> Bool
on elem(x, xs)
    xs contains x
end elem

-- elemIndex :: a -> [a] -> Maybe Int
on elemIndex(x, xs)
    set lng to length of xs
    repeat with i from 1 to lng
        if x = (item i of xs) then return i
    end repeat
    return missing value
end elemIndex

-- foldr :: (a -> b -> a) -> a -> [b] -> a
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldr

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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

-- null:: [a] -> Bool
on |null|(xs)
    if class of xs is string then
        xs = ""
    else
        xs = {}
    end if
end |null|

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

-- not :: Bool -> Bool
on |not|(x)
    not x
end |not|

-- quote :: String -> String
on |quote|(x)
    quoted form of x
end |quote|

-- toUpper :: String -> String
on toUpper(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        uppercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toUpper

-- uncons :: [a] -> Maybe (a, [a])
on uncons(xs)
    set lng to length of xs
    if lng > 0 then
        if class of xs is string then
            set cs to text items of xs
            {item 1 of cs, rest of cs}
        else
            {item 1 of xs, rest of xs}
        end if
    else
        missing value
    end if
end uncons

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines

-- words :: String -> [String]
on |words|(s)
    words of s
end |words|
```

Output:

```txt
''  ->  true
'A'  ->  true
'BARK'  ->  true
'BoOK'  ->  false
'TrEAT'  ->  true
'COmMoN'  ->  false
'SQUAD'  ->  true
'conFUsE'  ->  true
```



## Astro


```astro
fun abc(s, ls):
    if ls.isempty:
        return true
    for i in indices(list) where s[end] in list[i]:
        return abc(s[:end-1], remove!(copy(list), at: i))
    false

let test = ["A", "BARK","BOOK","TREAT","COMMON","SQUAD","CONFUSE"]
let ls = ["BO","XK","DQ","CP","NA","GT","RE","TG","QD","FS", "JW","HU","VI","AN","OB","ER","FS","LY","PC","ZM"]

for s in test:
    print "($|>8|{s} ${abc(s, list)})"
```


## AutoHotkey

```autohotkey
isWordPossible(blocks, word){
	o := {}
	loop, parse, blocks, `n, `r
		o.Insert(A_LoopField)
	loop, parse, word
		if !(r := isWordPossible_contains(o, A_LoopField, word))
			return 0
	return 1
}
isWordPossible_contains(byref o, letter, word){
	loop 2 {
		for k,v in o
			if Instr(v,letter)
			{
				StringReplace, op, v,% letter
				if RegExMatch(op, "[" word "]")
					sap := k
				else added := 1 , sap := k
				if added
					return "1" o.remove(sap)
			}
		added := 1
	}
}
```


'''Test Input''' (as per question)

```autohotkey
blocks := "
(
BO
XK
DQ
CP
NA
GT
RE
TG
QD
FS
JW
HU
VI
AN
OB
ER
FS
LY
PC
ZM
)"

wordlist := "
(
A
BARK
BOOK
TREAT
COMMON
SQUAD
CONFUSE
)"

loop, parse, wordlist, `n
	out .= A_LoopField " - " isWordPossible(blocks, A_LoopField) "`n"
msgbox % out
```


Output:

```txt
A - 1
BARK - 1
BOOK - 0
TREAT - 1
COMMON - 0
SQUAD - 1
CONFUSE - 1
```


## AWK

Here are 2 slightly different versions:

```txt

#!/usr/bin/awk -f
# tested with mawk 1.3.3 on Raspberry Pi 3
#        also GNU awk 3.1.5, busybox 1.21.1 and 1.27.1 on AMD Sempron 2800+
#
function setblocks() {
# key to the algorithm is the representation of a block
# each block is represented by 4 characters in the string "blocks"
# for example, the "BO" block becomes "-BO-"
#
blocks="-BO--XK--DQ--CP--NA--GT--RE--TG--QD--FS--JW--HU--VI--AN--OB--ER--FS--LY--PC--ZM-"
true=1
false=0
}
function found(letter){
#
# the function "found" scans for the letter on the top of a block
# using the pattern "-B", for example, to find a "B",
# returning "true" (or 1) if found
# if not found on the top, look on the bottoms using the pattern "B-"
# again returning "true" if found
# if the letter is found on either top or bottom, the 4 character block is set to "----"
# so that block is unavailable
# finally, if no available copy of letter is found,
# the function returns "false" (0)
position= index(blocks,"-" letter)
if (position > 0)
   {
  blocks = substr(blocks,1,position-1) "----" substr(blocks,position+4)
  return true
   }
position = index(blocks,letter "-")
if (position > 0)
   {blocks = substr(blocks,1,position-3) "----" substr(blocks,position+2)
     return true
    }
return false
}
# awk's BEGIN statement allows for initialization before processing input;
# in this case, initializing the string "blocks"
#
BEGIN{
setblocks()
}
# in awk, the input record is contained in the string variable "$0"
# the main process checks each letter in turn to see if it is on a usable block,
# summing the values returned by "found"
# if the sum equals the number of input characters the word can be spelled with the blocks
# otherwise it is not possible
#
{
nchars=length($0)
possible=false
for (i=1;i<=nchars;i++){
     possible=possible + found(substr($0,i,1))
}
if (possible==nchars) print $0 " is possible"
   else print $0 " is not possible"
setblocks()
}

```

and

```txt

#!/usr/bin/awk -f
# tested with mawk 1.3.3 on Raspberry Pi 3
#        also GNU awk 3.1.5, busybox 1.21.1 and 1.27.1 on AMD Sempron 2800+
#
function setblocks() {
#
#  key to the algorithm is the representation of the blocks
# each block is represented by 1 character in the string "tops"
# and by 1 character in the string "bottoms"
#
   tops="BXDCNGRTQFJHVAOEFLPZ"
bottoms="OKQPATEGDSWUINBRSYCM"
true=1
false=0
}
function found(letter){
#
# the function "found" scans first the string "tops" for a letter and
# then the string "bottoms" if the letter is not in "tops"
# if the letter is found, it marks "tops" and "bottoms" to show
# the block is unavailable by changing the letters on the block to "-"
# and returns "true" (1); if the letter is not found
# the function returns "false" (0)
#
position= index(tops,letter)
if (position > 0)
   {
  tops = substr(tops,1,position-1) "-" substr(tops,position+1)
  bottoms = substr(bottoms,1,position-1) "-" substr(bottoms,position+1)
  return true
   }
position = index(bottoms,letter)
if (position > 0)
   {bottoms = substr(bottoms,1,position-1) "-" substr(bottoms,position+1)
    tops = substr(tops,1,position-1) "-" substr(tops,position+1)
     return true
    }
return false
}
# awk's BEGIN statement allows for initialization before processing input;
# in this case, initializing the string "blocks"
#
BEGIN{
setblocks()
}
# in awk, the input record is contained in the string variable "$0"
# the main process checks each letter in turn to see if it is on a usable block,
# summing the values returned by "found"
# if the sum equals the number of input characters the word can be spelled with the blocks
# otherwise it is not possible
#
{
nchars=length($0)
possible=false
for (i=1;i<=nchars;i++){
     possible=possible + found(substr($0,i,1))
}
if (possible==nchars) print $0 " is possible"
   else print $0 " is not possible"
setblocks()
}

```

Output:

```txt

pi@raspberrypi:~/Documents/rosettacode $ ./abcProblem.awk
A
A is possible
BARK
BARK is possible
BOOK
BOOK is not possible
TREAT
TREAT is possible
COMMON
COMMON is not possible
SQUAD
SQUAD is possible
CONFUSE
CONFUSE is possible
^C
pi@raspberrypi:~/Documents/rosettacode $

```



## Batch File


```dos
@echo off
::abc.bat
::
::Batch file to evaluate if a given string can be represented with a set of
::20 2-faced blocks.
::

::Check if a string was provided
if "%1"=="" goto ERROR

::Define blocks. Separate blocks by ':', and terminat with '::'
set "FACES=BO:XK:DQ:CP:NA:GT:RE:TG:QD:FS:JW:HU:VI:AN:OB:ER:FS:LY:PC:ZM::"
set INPUT=%1
set "COUNTER=0"

::The main loop steps through the input string, checking if an available
::block exists for each character
:LOOP_MAIN

  ::Get character, increase counter, and test if there are still characters
  call set "char=%%INPUT:~%COUNTER%,1%%"
  set /a "COUNTER+=1"
  if "%CHAR%"=="" goto LOOP_MAIN_END

  set "OFFSET=0"
  :LOOP_2

    ::Read in two characters (one block)
    call set "BLOCK=%%FACES%:~%OFFSET%,2%%"

    ::Test if the all blocks were checked. If so, no match was found
    if "%BLOCK%"==":" goto FAIL

    ::Test if current input string character is in the current block
    if /i "%BLOCK:~0,1%"=="%CHAR%" goto FOUND
    if /i "%BLOCK:~1,1%"=="%CHAR%" goto FOUND

    ::Increase offset to point to the next block
    set /a "OFFSET+=3"

  goto LOOP_2
  :LOOP_2_END

  ::If found, blank out the block used
  :FOUND
  call set "FACES=%%FACES:%BLOCK%:=  :%%"

goto LOOP_MAIN
:LOOP_MAIN_END

echo %0: It is possible to write the '%INPUT%' with my blocks.
goto END

:FAIL
echo %0: It is NOT possible to write the '%INPUT%' with my blocks.
goto END

:ERROR
echo %0: Please enter a string to evaluate
echo.

:END
```



## BaCon


```qbasic
CONST info$ = "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM"

DATA "A", "BARK", "BOOK", "TREAT", "Common", "Squad", "Confuse"

WHILE TRUE
    READ word$

    IF NOT(LEN(word$)) THEN BREAK

    block$ = info$

    count = AMOUNT(block$)

    FOR y = 1 TO LEN(word$)
        FOR x = 1 TO AMOUNT(block$)
            IF TALLY(TOKEN$(block$, x), MID$(UCASE$(word$), y, 1)) THEN
                block$ = DEL$(block$, x)
                BREAK
            END IF
        NEXT
    NEXT

    PRINT word$, IIF$(LEN(word$) = count-AMOUNT(block$), "True", "False") FORMAT "%-10s: %s\n"
WEND
```

Output:

```txt
A         : True
BARK      : True
BOOK      : False
TREAT     : True
Common    : False
Squad     : True
Confuse   : True
```



## BASIC

Works with:VB-DOS, QB64, QBasic, QuickBASIC

```qbasic
' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '
' ABC_Problem                                       '
'                                                   '
' Developed by A. David Garza Marín in VB-DOS for   '
' RosettaCode. November 29, 2016.                   '
' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' ' '

' Comment the following line to run it in QB or QBasic
OPTION EXPLICIT  ' Modify to OPTION _EXPLICIT for QB64

' SUBs and FUNCTIONs
DECLARE SUB doCleanBlocks ()
DECLARE FUNCTION ICanMakeTheWord (WhichWord AS STRING) AS INTEGER
DECLARE SUB doReadBlocks ()

' rBlock Data Type
TYPE regBlock
  Block AS STRING * 2
  Used AS INTEGER
END TYPE

' Initialize
CONST False = 0, True = NOT False, HMBlocks = 20
DATA "BO", "XK", "DQ", "CP", "NA", "GT","RE", "TG"
DATA "QD", "FS", "JW", "HU", "VI", "AN", "OB", "ER"
DATA "FS", "LY", "PC","ZM"

DIM rBlock(1 TO HMBlocks) AS regBlock
DIM i AS INTEGER, aWord AS STRING, YorN AS STRING

doReadBlocks ' Read the data in the blocks

'-------------- Main program cycle ------------------
CLS
PRINT "This program has the following blocks: ";
FOR i = 1 TO HMBlocks
  PRINT rBlock(i).Block; "|";
NEXT i
PRINT : PRINT
PRINT "Please, write a word or a short sentence to see if the available"
PRINT "blocks can make it. If so, I will tell you."
DO
  doCleanBlocks ' Clean all blocks
  PRINT
  INPUT "Which is the word"; aWord
  aWord = LTRIM$(RTRIM$(aWord))

  IF aWord <> "" THEN
    IF ICanMakeTheWord(aWord) THEN
      PRINT "Yes, i can make it."
    ELSE
      PRINT "No, I can't make it."
    END IF
  ELSE
    PRINT "At least, you need to type a letter."
  END IF

  PRINT
  PRINT "Do you want to try again (Y/N) ";
  DO
    YorN = INPUT$(1)
    YorN = UCASE$(YorN)
  LOOP UNTIL YorN = "Y" OR YorN = "N"
  PRINT YorN

LOOP UNTIL YorN = "N"
' -------------- End of Main program ----------------
END

SUB doCleanBlocks ()
  ' Var
  SHARED rBlock() AS regBlock
  DIM i AS INTEGER

  ' Will clean the Used status of all blocks
  FOR i = 1 TO HMBlocks
    rBlock(i).Used = False
  NEXT i

END SUB

SUB doReadBlocks ()
  ' Var
  SHARED rBlock() AS regBlock
  DIM i AS INTEGER

  ' Will read the block values from DATA
  FOR i = 1 TO HMBlocks
    READ rBlock(i).Block
  NEXT i
END SUB

FUNCTION ICanMakeTheWord (WhichWord AS STRING) AS INTEGER ' Comment AS INTEGER to run in QBasic, QB64 and QuickBASIC
  ' Var
  SHARED rBlock() AS regBlock
  DIM i AS INTEGER, l AS INTEGER, j AS INTEGER, iYesICan AS INTEGER
  DIM c AS STRING, sUWord AS STRING

  ' Will evaluate if can make the word
  sUWord = UCASE$(WhichWord)
  l = LEN(sUWord)
  i = 0

  DO
    i = i + 1
    iYesICan = False
    c = MID$(sUWord, i, 1)
    j = 0
    DO
      j = j + 1
      IF NOT rBlock(j).Used THEN
        iYesICan = (INSTR(rBlock(j).Block, c) > 0)
        rBlock(j).Used = iYesICan
      END IF
    LOOP UNTIL j >= HMBlocks OR iYesICan

  LOOP UNTIL i >= l OR NOT iYesICan

  ' The result will depend on the last value of
  '  iYesICan variable. If the last value is True
  '  is because the function found even the last
  '  letter analyzed.
  ICanMakeTheWord = iYesICan

END FUNCTION
```


### Commodore BASIC

Based on the Sinclair ZX81 BASIC solution.
Indentations are for legibility only,
will not be preserved in real Commodore BASIC editor.

```basic
10 W$ = "A" : GOSUB 100
20 W$ = "BARK" : GOSUB 100
30 W$ = "BOOK" : GOSUB 100
40 W$ = "TREAT" : GOSUB 100
50 W$ = "COMMON" : GOSUB 100
60 W$ = "SQUAD" : GOSUB 100
70 W$ = "CONFUSE" : GOSUB 100
80 END
90 REM ********************************
100 B$="BOXKDQCPNAGTRETGQDFSJWHUVIANOBERFSLYPCZM"
110 FOR I=1 TO LEN(W$)
120    BL = LEN(B$)
130    FOR J=1 TO BL STEP 2
140      C$=MID$(B$,J,1): D$=MID$(B$,J+1,1)
150      X$=MID$(W$,I,1)
160      IF C$<>X$ AND D$<>X$ THEN GOTO 190
170      B$ = LEFT$(B$,J-1)+RIGHT$(B$,BL-J-1)
180      GOTO 210
190   NEXT J
200   IF J>BL-1 THEN GOTO 240
210 NEXT I
220 PRINT W$" -> YES"
230 RETURN
240 PRINT W$" -> NO"
250 RETURN
```

Output:

```txt
A -> YES
BARK -> YES
BOOK -> NO
TREAT -> YES
COMMON -> NO
SQUAD -> YES
CONFUSE -> YES
```


### Sinclair ZX81 BASIC

Works with 1k of RAM.
A nice unstructured algorithm.
Unfortunately the requirement that it be case-insensitive is moot,
because the ZX81 does not support lower-case letters.

```basic
 10 LET B$="BOXKDQCPNAGTRETGQDFSJWHUVIANOBERFSLYPCZM"
 20 INPUT W$
 30 FOR I=1 TO LEN W$
 40 FOR J=1 TO LEN B$ STEP 2
 50 IF B$(J)<>W$(I) AND B$(J+1)<>W$(I) THEN GOTO 100
 60 LET B$=B$( TO J-1)+B$(J+2 TO )
 70 NEXT I
 80 PRINT "YES"
 90 STOP
100 NEXT J
110 PRINT "NO"
```

Input:

```txt
A
```

Output:

```txt
YES
```

Input:

```txt
BARK
```

Output:

```txt
YES
```

Input:

```txt
BOOK
```

Output:

```txt
NO
```

Input:

```txt
TREAT
```

Output:

```txt
YES
```

Input:

```txt
COMMON
```

Output:

```txt
NO
```

Input:

```txt
SQUAD
```

Output:

```txt
YES
```

Input:

```txt
CONFUSE
```

Output:

```txt
YES
```



## BBC BASIC

Works with BBC BASIC for Windows}}

```bbcbasic
BLOCKS$="BOXKDQCPNAGTRETGQDFSJWHUVIANOBERFSLYPCZM"
PROCcan_make_word("A")
PROCcan_make_word("BARK")
PROCcan_make_word("BOOK")
PROCcan_make_word("TREAT")
PROCcan_make_word("COMMON")
PROCcan_make_word("SQUAD")
PROCcan_make_word("Confuse")
END

DEF PROCcan_make_word(word$)
LOCAL b$,p%
b$=BLOCKS$
PRINT word$ " -> ";
p%=INSTR(b$,CHR$(ASCword$ AND &DF))
WHILE p%>0 AND word$>""
  MID$(b$,p%-1+(p% MOD 2),2)=".."
  word$=MID$(word$,2)
  p%=INSTR(b$,CHR$(ASCword$ AND &DF))
ENDWHILE
IF word$>"" PRINT "False" ELSE PRINT "True"
ENDPROC
```


Output:

```txt
A -> True
BARK -> True
BOOK -> False
TREAT -> True
COMMON -> False
SQUAD -> True
Confuse -> True
```


## Bracmat

```bracmat
(
  ( can-make-word
  =   ABC blocks
    .       (B O)
          + (X K)
          + (D Q)
          + (C P)
          + (N A)
          + (G T)
          + (R E)
          + (T G)
          + (Q D)
          + (F S)
          + (J W)
          + (H U)
          + (V I)
          + (A N)
          + (O B)
          + (E R)
          + (F S)
          + (L Y)
          + (P C)
          + (Z M)
        : ?blocks
      & ( ABC
        =   letter blocks A Z
          .   !arg:(.?)
            |   !arg:(@(?:%?letter ?arg).?blocks)
              &   !blocks
                :   ?
                  + ?*(? !letter ?:?block)
                  + (?&ABC$(!arg.!blocks+-1*!block))
        )
      &   out
        $ ( !arg
            ( ABC$(upp$!arg.!blocks)&yes
            | no
            )
          )
  )
& can-make-word'A
& can-make-word'BARK
& can-make-word'BOOK
& can-make-word'TREAT
& can-make-word'COMMON
& can-make-word'SQUAD
& can-make-word'CONFUSE
);
```

Output:

```txt
A yes
BARK yes
BOOK no
TREAT yes
COMMON no
SQUAD yes
CONFUSE yes
```



## C

Recursive solution.
Empty string returns true.

```c
#include <stdio.h

#include <ctype.h>

int can_make_words(char **b, char *word)
{
	int i, ret = 0, c = toupper(*word);

#define SWAP(a, b) if (a != b) { char * tmp = a; a = b; b = tmp; }

	if (!c) return 1;
	if (!b[0]) return 0;

	for (i = 0; b[i] && !ret; i++) {
		if (b[i][0] != c && b[i][1] != c) continue;
		SWAP(b[i], b[0]);
		ret = can_make_words(b + 1, word + 1);
		SWAP(b[i], b[0]);
	}

	return ret;
}

int main(void)
{
	char* blocks[] = {
		"BO", "XK", "DQ", "CP", "NA",
		"GT", "RE", "TG", "QD", "FS",
		"JW", "HU", "VI", "AN", "OB",
		"ER", "FS", "LY", "PC", "ZM",
		0 };

	char *words[] = {
		"", "A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "Confuse", 0
	};

	char **w;
	for (w = words; *w; w++)
		printf("%s\t%d\n", *w, can_make_words(blocks, *w));

	return 0;
}
```

Output:

```txt
        1
A       1
BARK    1
BOOK    0
TREAT   1
COMMON  0
SQUAD   1
Confuse 1
```


## C++

Uses C++11. Build with g++-4.7 -Wall -std=c++0x abc.cpp

```cpp
#include <iostream

#include <vector>
#include <string>
#include <set>
#include <cctype>


typedef std::pair<char,char> item_t;
typedef std::vector<item_t> list_t;

bool can_make_word(const std::string& w, const list_t& vals) {
    std::set<uint32_t> used;
    while (used.size() < w.size()) {
        const char c = toupper(w[used.size()]);
        uint32_t x = used.size();
        for (uint32_t i = 0, ii = vals.size(); i < ii; ++i) {
            if (used.find(i) == used.end()) {
                if (toupper(vals[i].first) == c || toupper(vals[i].second) == c) {
                    used.insert(i);
                    break;
                }
            }
        }
        if (x == used.size()) break;
    }
    return used.size() == w.size();
}


int main() {
    list_t vals{ {'B','O'}, {'X','K'}, {'D','Q'}, {'C','P'}, {'N','A'}, {'G','T'}, {'R','E'}, {'T','G'}, {'Q','D'}, {'F','S'}, {'J','W'}, {'H','U'}, {'V','I'}, {'A','N'}, {'O','B'}, {'E','R'}, {'F','S'}, {'L','Y'}, {'P','C'}, {'Z','M'} };
    std::vector<std::string> words{"A","BARK","BOOK","TREAT","COMMON","SQUAD","CONFUSE"};
    for (const std::string& w : words) {
        std::cout << w << ": " << std::boolalpha << can_make_word(w,vals) << ".\n";
    }

}
```


Output:

```txt
A: true.
BARK: true.
BOOK: false.
TREAT: true.
COMMON: false.
SQUAD: true.
CONFUSE: true.
```


## C sharp

### Regex

This Method uses regular expressions to do the checking.
Given that n = length of blocks string and
m = length of word string,
then CheckWord's time complexity comes out to about m * (n - (m-1)/2).

```csharp
using System;
using System.IO;
// Needed for the method.
using System.Text.RegularExpressions;
using System.Collections.Generic;

void Main()
{
   string blocks = "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM";
   List<string> words = new List<string>() {
      "A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"
   };

   foreach(var word in words)
   {
      Console.WriteLine("{0}: {1}", word, CheckWord(blocks, word));
   }
}

bool CheckWord(string blocks, string word)
{
   for(int i = 0; i < word.Length; ++i)
   {
      int length = blocks.Length;
      Regex rgx = new Regex("([a-z]"+word[i]+"|"+word[i]+"[a-z])", RegexOptions.IgnoreCase);
      blocks = rgx.Replace(blocks, "", 1);
      if(blocks.Length == length) return false;
   }
   return true;
}
```

Output:

```txt
A: True
BARK: True
BOOK: False
TREAT: True
COMMON: False
SQUAD: True
CONFUSE: True
```

'''Unoptimized'''

```csharp
using System.Collections.Generic;
using System.Linq;

void Main()
{
	List<string> blocks =
	new List<string>() { "bo", "xk", "dq", "cp", "na", "gt", "re", "tg", "qd", "fs",
		"jw", "hu", "vi", "an", "ob", "er", "fs", "ly", "pc", "zm" };
	List<string> words = new List<string>() {
		"A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"};

	var solver = new ABC(blocks);

	foreach( var word in words)
	{
		Console.WriteLine("{0} :{1}", word, solver.CanMake(word));
	}
}

class ABC
{
	readonly Dictionary<char, List<int>> _blockDict = new Dictionary<char, List<int>>();
	bool[] _used;
	int _nextBlock;

	readonly List<string> _blocks;

	private void AddBlockChar(char c)
	{
		if (!_blockDict.ContainsKey(c))
		{
			_blockDict[c] = new List<int>();
		}
		_blockDict[c].Add(_nextBlock);
	}

	private void AddBlock(string block)
	{
		AddBlockChar(block[0]);
		AddBlockChar(block[1]);
		_nextBlock++;
	}

	public ABC(List<string> blocks)
	{
		_blocks = blocks;
		foreach (var block in blocks)
		{
			AddBlock(block);
		}
	}

	public bool CanMake(string word)
	{
		word = word.ToLower();
		if (word.Length > _blockDict.Count)
		{
			return false;
		}
		_used = new bool[_blocks.Count];
		return TryMake(word);
	}

	public bool TryMake(string word)
	{
		if (word == string.Empty)
		{
			return true;
		}
		var blocks = _blockDict[word[0]].Where(b => !_used[b]);
		foreach (var block in blocks)
		{
			_used[block] = true;
			if (TryMake(word.Substring(1)))
			{
				return true;
			}
			_used[block] = false;
		}
		return false;
	}
}
```

Output:

```txt
A :True
BARK :True
BOOK :False
TREAT :True
COMMON :False
SQUAD :True
CONFUSE :True
```


## Ceylon

Functional programming/recursive solution.
No variable values.

<b>module.ceylon</b>

```ceylon
module rosetta.abc "1.0.0" {}
```

<b>run.ceylon</b>

```ceylon
shared void run() {
    printAndCanMakeWord("A", blocks);
    //True
    printAndCanMakeWord("BARK", blocks);
    //True
    printAndCanMakeWord("BOOK", blocks);
    //False
    printAndCanMakeWord("TREAT", blocks);
    //True
    printAndCanMakeWord("COMMON", blocks);
    //False
    printAndCanMakeWord("SQUAD", blocks);
    //True
    printAndCanMakeWord("CONFUSE", blocks);
    //True
}

Block[] blocks =
    [
        Block('B','O'),
        Block('X','K'),
        Block('D','Q'),
        Block('C','P'),
        Block('N','A'),
        Block('G','T'),
        Block('R','E'),
        Block('T','G'),
        Block('Q','D'),
        Block('F','S'),
        Block('J','W'),
        Block('H','U'),
        Block('V','I'),
        Block('A','N'),
        Block('O','B'),
        Block('E','R'),
        Block('F','S'),
        Block('L','Y'),
        Block('P','C'),
        Block('Z','M')
    ];

void printAndCanMakeWord(String word, Block[] blocks) {
    print("``word``:``canMakeWord(word, blocks)``");
}

class Block(Character firstLetter, Character secondLetter) {
    shared Character firstLetterUpper = firstLetter.uppercased;
    shared Character secondLetterUpper = secondLetter.uppercased;

    shared Boolean containsLetter(Character letter)
        => let (letterUpper = letter.uppercased)
            firstLetterUpper == letterUpper || secondLetterUpper == letterUpper;

    shared actual String string = "``firstLetterUpper``,``secondLetterUpper``";
}

Boolean canMakeWord(String word, Block[] blocks)
    => canMakeWordRecursive(word.uppercased.sequence(), 0, blocks, word.indexes());

Boolean canMakeWordRecursive(Character[] word,
                             Integer index,
                             Block[] remainingBlocks,
                             Integer[] remainingLetterIndexes)
    => if (exists wordFirst = word.first, // first is the Ceylon attribute for head
           exists remainingBlock = remainingBlocks.find((remainingBlock) => remainingBlock.containsLetter(wordFirst)))
        then
            let (myRemainingLetterIndexes = remainingLetterIndexes.filter((theIndex) => index != theIndex).sequence())
             if (myRemainingLetterIndexes.empty)
                 then true
                 else canMakeWordRecursive(word.rest,// rest is the Ceylon attribute for tail
                                           index+1, // move through the letter indexes
                                           remainingBlocks.filter((block) => remainingBlock != block).sequence(), // one less block
                                           myRemainingLetterIndexes)
        else false;

```

Output:

```txt
A:true
BARK:true
BOOK:false
TREAT:true
COMMON:false
SQUAD:true
CONFUSE:true
```


## Clojure

A translation of the Haskell solution.

```clojure

(def blocks
  (-> "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM" (.split " ") vec))

(defn omit
  "return bs with (one instance of) b omitted"
  [bs b]
  (let [[before after] (split-with #(not= b %) bs)]
    (concat before (rest after))))

(defn abc
  "return lazy sequence of solutions (i.e. block lists)"
  [blocks [c & cs]]
  (if (some? c)
    (for [b blocks :when (some #(= c %) b)
          bs (abc (omit blocks b) cs)]
      (cons b bs))
    [[]]))


(doseq [word ["A" "BARK" "Book" "treat" "COMMON" "SQUAD" "CONFUSE"]]
  (->> word .toUpperCase (abc blocks) first (printf "%s: %b\n" word)))
```

Output:

```txt
A: true
BARK: true
Book: false
treat: true
COMMON: false
SQUAD: true
CONFUSE: true
```


## CoffeeScript

```CoffeeScript
blockList = [
  'BO', 'XK', 'DQ', 'CP', 'NA', 'GT', 'RE', 'TG', 'QD', 'FS', 'JW', 'HU',
  'VI', 'AN', 'OB', 'ER', 'FS', 'LY', 'PC', 'ZM'
]

canMakeWord = (word="") ->
    # Create a shallow clone of the master blockList
    blocks = blockList.slice 0
    # Check if blocks contains letter
    checkBlocks = (letter) ->
        # Loop through every remaining block
        for block, idx in blocks
            # If letter is in block, blocks.splice will return an array, which will evaluate as true
            return blocks.splice idx, 1 if letter.toUpperCase() in block
        false
    # Return true if there are no falsy values
    false not in (checkBlocks letter for letter in word)

# Expect true, true, false, true, false, true, true, true
for word in ["A", "BARK", "BOOK", "TREAT", "COMMON", "squad", "CONFUSE", "STORM"]
    console.log word + " -> " + canMakeWord(word)
```


Output:

```txt
A -> true
BARK -> true
BOOK -> false
TREAT -> true
COMMON -> false
squad -> true
CONFUSE -> true
STORM -> true
```


## Common Lisp

```lisp
(defun word-possible-p (word blocks)
  (cond
    ((= (length word) 0) t)
    ((null blocks) nil)
    (t (let*
         ((c (aref word 0))
          (bs (remove-if-not #'(lambda (b)
                                 (find c b :test #'char-equal))
                             blocks)))
         (some #'identity
               (loop for b in bs
                     collect (word-possible-p
                               (subseq word 1)
                               (remove b blocks))))))))
```


Output:

```txt
> (defparameter *blocks*
    '("BO" "XK" "DQ" "CP" "NA" "GT" "RE" "TG" "QD" "FS"
      "JW" "HU" "VI" "AN" "OB" "ER" "FS" "LY" "PC" "ZM"))
> (dolist (w '("" "A" "bArk" "BOOK" "trEAt" "CoMmoN" "squad" "conFUse"))
    (format t "~s is possible: ~a~%" w (word-possible-p w *blocks*)))
"" is possible: T
"A" is possible: T
"bArk" is possible: T
"BOOK" is possible: NIL
"trEAt" is possible: T
"CoMmoN" is possible: NIL
"squad" is possible: T
"conFUse" is possible: T
NIL
> (word-possible-p "abba" '("AB" "AB" "AC" "AC"))
T
```


## Component Pascal

Works with BlackBox Component Builder}}

```oberon2
MODULE ABCProblem;
IMPORT
	StdLog, DevCommanders, TextMappers;
CONST
	notfound = -1;
TYPE
	String = ARRAY 3 OF CHAR;
VAR
	blocks : ARRAY 20 OF String;

PROCEDURE Check(s: ARRAY OF CHAR): BOOLEAN;
VAR
	used: SET;
	i,blockIndex: INTEGER;

	PROCEDURE GetBlockFor(c: CHAR): INTEGER;
	VAR
		i: INTEGER;
	BEGIN
		c := CAP(c);
		i := 0;
		WHILE (i < LEN(blocks)) DO
			IF (c = blocks[i][0]) OR (c = blocks[i][1]) THEN
				IF ~(i IN used) THEN RETURN i END
			END;
			INC(i)
		END;
		RETURN notfound
	END GetBlockFor;

BEGIN
	used := {};
	FOR i := 0 TO LEN(s$) - 1 DO
		blockIndex := GetBlockFor(s[i]);
		IF blockIndex = notfound THEN
			RETURN FALSE
		ELSE
			INCL(used,blockIndex)
		END
	END;
	RETURN TRUE
END Check;

PROCEDURE CanMakeWord*;
VAR
	s: TextMappers.Scanner;
BEGIN
	s.ConnectTo(DevCommanders.par.text);
	s.SetPos(DevCommanders.par.beg);
	s.Scan;
	WHILE (~s.rider.eot) DO
		IF (s.type = TextMappers.char) & (s.char = '~') THEN
			RETURN
		ELSIF (s.type = TextMappers.string) THEN
			StdLog.String(s.string);StdLog.String(":> ");
			StdLog.Bool(Check(s.string));StdLog.Ln
		END;
		s.Scan
	END
END CanMakeWord;

BEGIN
	blocks[0] := "BO";
	blocks[1] := "XK";
	blocks[2] := "DQ";
	blocks[3] := "CP";
	blocks[4] := "NA";
	blocks[5] := "GT";
	blocks[6] := "RE";
	blocks[7] := "TG";
	blocks[8] := "QD";
	blocks[9] := "FS";
	blocks[10] := "JW";
	blocks[11] := "HU";
	blocks[12] := "VI";
	blocks[13] := "AN";
	blocks[14] := "OB";
	blocks[15] := "ER";
	blocks[16] := "FS";
	blocks[17] := "LY";
	blocks[18] := "PC";
	blocks[19] := "ZM";

END ABCProblem.
```

Execute: ^Q ABCProblem.CanMakeWord A BARK BOOK TREAT COMMON SQUAD confuse~
Output:

```txt
A:>  $TRUE
BARK:>  $TRUE
BOOK:>  $FALSE
TREAT:>  $TRUE
COMMON:>  $FALSE
SQUAD:>  $TRUE
confuse:>  $TRUE
```


## D

### Basic Version

Translated from Python
A simple greedy algorithm is enough for the given sequence of blocks.
canMakeWord is true on an empty word
because you can compose it using zero blocks.

```d
import std.stdio, std.algorithm, std.string;

bool canMakeWord(in string word, in string[] blocks) pure /*nothrow*/ @safe {
    auto bs = blocks.dup;
    outer: foreach (immutable ch; word.toUpper) {
        foreach (immutable block; bs)
            if (block.canFind(ch)) {
                bs = bs.remove(bs.countUntil(block));
                continue outer;
            }
        return false;
    }
    return true;
}

void main() @safe {
    immutable blocks = "BO XK DQ CP NA GT RE TG QD FS JW HU VI
                        AN OB ER FS LY PC ZM".split;

    foreach (word; "" ~ "A BARK BoOK TrEAT COmMoN SQUAD conFUsE".split)
        writefln(`"%s" %s`, word, canMakeWord(word, blocks));
}
```

Output:

```txt
"" true
"A" true
"BARK" true
"BoOK" false
"TrEAT" true
"COmMoN" false
"SQUAD" true
"conFUsE" true
```



### @nogc Version

The same as the precedent version, but it avoids all heap allocations
and it's lower-level and ASCII-only.

```d
import std.ascii, core.stdc.stdlib;

bool canMakeWord(in string word, in string[] blocks) nothrow @nogc
in {
    foreach (immutable char ch; word)
        assert(ch.isASCII);
    foreach (const block; blocks)
        assert(block.length == 2 && block[0].isASCII && block[1].isASCII);
} body {
    auto ptr = cast(string*)alloca(blocks.length * string.sizeof);
    if (ptr == null)
        exit(1);
    auto blocks2 = ptr[0 .. blocks.length];
    blocks2[] = blocks[];

    outer: foreach (immutable i; 0 .. word.length) {
        immutable ch = word[i].toUpper;
        foreach (immutable j; 0 .. blocks2.length) {
            if (blocks2[j][0] == ch || blocks2[j][1] == ch) {
                if (blocks2.length > 1)
                    blocks2[j] = blocks2[$ - 1];
                blocks2 = blocks2[0 .. $ - 1];
                continue outer;
            }
        }
        return false;
    }
    return true;
}

void main() {
    import std.stdio, std.string;

    immutable blocks = "BO XK DQ CP NA GT RE TG QD FS JW HU VI
                        AN OB ER FS LY PC ZM".split;

    foreach (word; "" ~ "A BARK BoOK TrEAT COmMoN SQUAD conFUsE".split)
        writefln(`"%s" %s`, word, canMakeWord(word, blocks));
}
```



### Recursive Version

This version is able to find the solution for the word "abba" given the blocks AB AB AC AC.
Translated from C

```d
import std.stdio, std.ascii, std.algorithm, std.array;

alias Block = char[2];

// Modifies the order of the given blocks.
bool canMakeWord(Block[] blocks, in string word) pure nothrow
in {
    assert(blocks.all!(w => w[].all!isAlpha));
    assert(word.all!isAlpha);
} body {
    if (word.empty)
        return true;

    immutable c = word[0].toUpper;
    foreach (ref b; blocks) {
        if (b[0].toUpper != c && b[1].toUpper != c)
            continue;
        blocks[0].swap(b);
        if (blocks[1 .. $].canMakeWord(word[1 .. $]))
            return true;
        blocks[0].swap(b);
    }

    return false;
}

void main() {
    enum Block[] blocks = "BO XK DQ CP NA GT RE TG QD FS
                           JW HU VI AN OB ER FS LY PC ZM".split;

    foreach (w; "" ~ "A BARK BoOK TrEAT COmMoN SQUAD conFUsE".split)
        writefln(`"%s" %s`, w, blocks.canMakeWord(w));

    // Extra test.
    Block[] blocks2 = ["AB", "AB", "AC", "AC"];
    immutable word = "abba";
    writefln(`"%s" %s`, word, blocks2.canMakeWord(word));
}
```

Output:

```txt
"" true
"A" true
"BARK" true
"BoOK" false
"TrEAT" true
"COmMoN" false
"SQUAD" true
"conFUsE" true
"abba" true
```



### Alternative Recursive Version

This version doesn't shuffle the input blocks, but it's more complex and it allocates an array of indexes.

```d
import std.stdio, std.ascii, std.algorithm, std.array, std.range;

alias Block = char[2];

bool canMakeWord(immutable Block[] blocks, in string word) pure nothrow
in {
    assert(blocks.all!(w => w[].all!isAlpha));
    assert(word.all!isAlpha);
} body {
    bool inner(size_t[] indexes, in string w) pure nothrow {
        if (w.empty)
            return true;

        immutable c = w[0].toUpper;
        foreach (ref idx; indexes) {
            if (blocks[idx][0].toUpper != c &&
                blocks[idx][1].toUpper != c)
                continue;
            indexes[0].swap(idx);
            if (inner(indexes[1 .. $], w[1 .. $]))
                return true;
            indexes[0].swap(idx);
        }

        return false;
    }

    return inner(blocks.length.iota.array, word);
}

void main() {
    enum Block[] blocks = "BO XK DQ CP NA GT RE TG QD FS
                           JW HU VI AN OB ER FS LY PC ZM".split;

    foreach (w; "" ~ "A BARK BoOK TrEAT COmMoN SQUAD conFUsE".split)
        writefln(`"%s" %s`, w, blocks.canMakeWord(w));

    // Extra test.
    immutable Block[] blocks2 = ["AB", "AB", "AC", "AC"];
    immutable word = "abba";
    writefln(`"%s" %s`, word, blocks2.canMakeWord(word));
}
```

The output is the same.


## Delphi

Just to be different I implemented a block as a set of (2) char rather than as an array of (2) char.

```Delphi
program ABC;
{$APPTYPE CONSOLE}

uses SysUtils;

type
  TBlock = set of char;

const
  TheBlocks : array [0..19] of TBlock =
  (
    [ 'B', 'O' ],    [ 'X', 'K' ],    [ 'D', 'Q' ],    [ 'C', 'P' ],    [ 'N', 'A' ],
    [ 'G', 'T' ],    [ 'R', 'E' ],    [ 'T', 'G' ],    [ 'Q', 'D' ],    [ 'F', 'S' ],
    [ 'J', 'W' ],    [ 'H', 'U' ],    [ 'V', 'I' ],    [ 'A', 'N' ],    [ 'O', 'B' ],
    [ 'E', 'R' ],    [ 'F', 'S' ],    [ 'L', 'Y' ],    [ 'P', 'C' ],    [ 'Z', 'M' ]
  );

function SolveABC(Target : string; Blocks : array of TBlock) : boolean;
var
  iChr : integer;
  Used : array [0..19] of boolean;

  function FindUnused(TargetChr : char) : boolean;  // Nested routine
  var
    iBlock : integer;
  begin
    Result := FALSE;
    for iBlock := low(Blocks) to high(Blocks) do
      if (not Used[iBlock]) and ( TargetChr in Blocks[iBlock] ) then
      begin
        Result := TRUE;
        Used[iBlock] := TRUE;
        Break;
      end;
  end;

begin
  FillChar(Used, sizeof(Used), ord(FALSE));
  Result := TRUE;
  iChr := 1;
  while Result and (iChr <= length(Target)) do
    if FindUnused(Target[iChr]) then inc(iChr)
                                else Result := FALSE;
end;

procedure CheckABC(Target : string);
begin
  if SolveABC(uppercase(Target), TheBlocks) then
    writeln('Can make ' + Target)
  else
    writeln('Can NOT make ' + Target);
end;

begin
  CheckABC('A');
  CheckABC('BARK');
  CheckABC('BOOK');
  CheckABC('TREAT');
  CheckABC('COMMON');
  CheckABC('SQUAD');
  CheckABC('CONFUSE');
  readln;
end.

```


Output:

```txt
Output:
Can make A
Can make BARK
Can NOT make BOOK
Can make TREAT
Can NOT make COMMON
Can make SQUAD
Can make CONFUSE

```



## Dyalect


{{trans|Swift}}


```dyalect
func blockable(str) {
    var blocks = [
        "BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS",
        "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM" ]

    var strUp = str.upper()
    var final = ""

    for c in strUp {
        for j in blocks.indices() {
            if blocks[j].startsWith(c) || blocks[j].endsWith(c) {
                final += c
                blocks[j] = ""
                break
            }
        }
    }

    return final == strUp
}

func canOrNot(can) {
    if can { "can" } else { "cannot" }
}

for str in [ "A", "BARK", "BooK", "TrEaT", "comMON", "sQuAd", "Confuse" ] {
    print("\"\(str)\" \(canOrNot(blockable(str))) be spelled with blocks.")
}
```


Output:


```txt
"A" can be spelled with blocks.
"BARK" can be spelled with blocks.
"BooK" cannot be spelled with blocks.
"TrEaT" can be spelled with blocks.
"comMON" cannot be spelled with blocks.
"sQuAd" can be spelled with blocks.
"Confuse" can be spelled with blocks.
```



## EchoLisp


```scheme

(lib 'list) ;; list-delete

(define BLOCKS '("BO" "XK" "DQ" "CP" "NA" "GT" "RE" "TG" "QD" "FS"
	     "JW" "HU" "VI" "AN" "OB" "ER" "FS" "LY" "PC" "ZM" ))

(define WORDS '("A" "BARK" "BOOK" "TREAT" "COMMON" "SQUAD" "CONFUSE"))

(define (spell word blocks)
    (cond
	((string-empty? word) #t)
	((empty? blocks) #f)
	(else
	(for/or [(block blocks)]
		#:continue (not (string-match block (string-first word)))
		(spell (string-rest word) (list-delete blocks block))))))


```

Output:

```txt

(for ((w WORDS))
  (writeln
    (string-randcase w)
    (spell (string-upcase w) BLOCKS)))

A     #t
bARK     #t
BooK     #f
TReAt     #t
ComMOn     #f
sqUAd     #t
COnfUSe     #t

```



## Ela

{{trans|Haskell}}

```ela
open list monad io char

:::IO

null = foldr (\_ _ -> false) true

mapM_ f = foldr ((>>-) << f) (return ())

abc _ [] = [[]]
abc blocks (c::cs) =
  [b::ans \\ b <- blocks | c `elem` b, ans <- abc (delete b blocks) cs]

blocks = ["BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS",
          "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM"]

mapM_ (\w -> putLn (w, not << null $ abc blocks (map char.upper w)))
  ["", "A", "BARK", "BoOK", "TrEAT", "COmMoN", "SQUAD", "conFUsE"]
```


Output:

```txt
("conFUsE",true)
("SQUAD",true)
("COmMoN",false)
("TrEAT",true)
("BoOK",false)
("BARK",true)
("A",true)
("",true)
```


## Elena

ELENA 4.1

```elena
import system'routines;
import system'collections;
import extensions;
import extensions'routines;

extension op
{
    canMakeWordFrom(blocks)
    {
        var list := ArrayList.load(blocks);

        ^ nil == (cast string(self)).upperCase().seekEach:(ch)
        {
            var index := list.indexOfElement
                ((word => word.indexOf(0, ch) != -1).asComparator());

            if (index>=0)
            {
                list.removeAt(index); ^ false
            }
            else
            {
                ^ true
            }
        }
    }
}

public program()
{
    var blocks := new::("BO", "XK", "DQ", "CP", "NA",
		"GT", "RE", "TG", "QD", "FS",
		"JW", "HU", "VI", "AN", "OB",
		"ER", "FS", "LY", "PC", "ZM");

    var words := new::("", "A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "Confuse");

    Enumerator e := words.enumerator();
    e.next();

    words.forEach:(word)
    {
        console.printLine("can make '",word,"' : ",word.canMakeWordFrom(blocks));
    }
}
```

Output:

```txt

can make '' : true
can make 'A' : true
can make 'BARK' : true
can make 'BOOK' : false
can make 'TREAT' : true
can make 'COMMON' : false
can make 'SQUAD' : true
can make 'Confuse' : true

```



## Elixir

{{trans|Erlang}}
Works with Elixir|1.3}}

```elixir
defmodule ABC do
  def can_make_word(word, avail) do
    can_make_word(String.upcase(word) |> to_charlist, avail, [])
  end

  defp can_make_word([], _, _), do: true
  defp can_make_word(_, [], _), do: false
  defp can_make_word([l|tail], [b|rest], tried) do
    (l in b and can_make_word(tail, rest++tried, []))
    or can_make_word([l|tail], rest, [b|tried])
  end
end

blocks = ~w(BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM)c
~w(A Bark Book Treat Common Squad Confuse) |>
Enum.map(fn(w) -> IO.puts "#{w}: #{ABC.can_make_word(w, blocks)}" end)
```


Output:

```txt

A: true
Bark: true
Book: false
Treat: true
Common: false
Squad: true
Confuse: true

```



## Erlang


```erlang
-module(abc).
-export([can_make_word/1, can_make_word/2, blocks/0]).

blocks() -> ["BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS",
             "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM"].

can_make_word(Word) -> can_make_word(Word, blocks()).
can_make_word(Word, Avail) -> can_make_word(string:to_upper(Word), Avail, []).
can_make_word([], _, _) -> true;
can_make_word(_, [], _) -> false;
can_make_word([L|Tail], [B|Rest], Tried) ->
  (lists:member(L,B) andalso can_make_word(Tail, lists:append(Rest, Tried),[]))
  orelse can_make_word([L|Tail], Rest, [B|Tried]).

main(_) -> lists:map(fun(W) -> io:fwrite("~s: ~s~n", [W, can_make_word(W)]) end,
                     ["A","Bark","Book","Treat","Common","Squad","Confuse"]).

```


Output:

```txt
A: true
Bark: true
Book: false
Treat: true
Common: false
Squad: true
Confuse: true

```



## ERRE


```ERRE

PROGRAM BLOCKS

!$INCLUDE="PC.LIB"

PROCEDURE CANMAKEWORD(WORD$)
   LOCAL B$,P%
   B$=BLOCKS$
   PRINT(WORD$;" -> ";)
   P%=INSTR(B$,CHR$(ASC(WORD$) AND $DF))
   WHILE P%>0 AND WORD$>"" DO
      CHANGE(B$,P%-1+(P% MOD 2),".."->B$)
      WORD$=MID$(WORD$,2)
      EXIT IF WORD$=""
      P%=INSTR(B$,CHR$(ASC(WORD$) AND $DF))
   END WHILE
   IF WORD$>"" THEN PRINT("False") ELSE PRINT("True") END IF
END PROCEDURE

BEGIN
  BLOCKS$="BOXKDQCPNAGTRETGQDFSJWHUVIANOBERFSLYPCZM"
  CANMAKEWORD("A")
  CANMAKEWORD("BARK")
  CANMAKEWORD("BOOK")
  CANMAKEWORD("TREAT")
  CANMAKEWORD("COMMON")
  CANMAKEWORD("SQUAD")
  CANMAKEWORD("Confuse")
END PROGRAM

```



## Euphoria

implemented using OpenEuphoria

```Euphoria

include std/text.e

sequence blocks = {{'B','O'},{'X','K'},{'D','Q'},{'C','P'},{'N','A'},
                   {'G','T'},{'R','E'},{'T','G'},{'Q','D'},{'F','S'},
                   {'J','W'},{'H','U'},{'V','I'},{'A','N'},{'O','B'},
                   {'E','R'},{'F','S'},{'L','Y'},{'P','C'},{'Z','M'}}
sequence words = {"A","BarK","BOOK","TrEaT","COMMON","SQUAD","CONFUSE"}

sequence current_word
sequence temp
integer matches

for i = 1 to length(words) do
	current_word = upper(words[i])
	temp = blocks
	matches = 0
	for j = 1 to length(current_word) do
		for k = 1 to length(temp) do
			if find(current_word[j],temp[k]) then
				temp = remove(temp,k)
				matches += 1
				exit
			end if
		end for
		if length(current_word) = matches then
			printf(1,"%s: TRUE\n",{words[i]})
			exit
		end if
	end for
	if length(current_word) != matches then
		printf(1,"%s: FALSE\n",{words[i]})
	end if
end for

if getc(0) then end if

```

Output:

```txt

A: TRUE
BarK: TRUE
BOOK: FALSE
TrEaT: TRUE
COMMON: FALSE
SQUAD: TRUE
CONFUSE: TRUE

..press Enter..

```


=={{header|F_Sharp|F#}}==
<p>This solution does not depend on the order of the blocks, neither on the symmetry of blocks we see in the example block set. (Symmetry: if AB is a block, an A comes only with another AB|BA)</p>

```fsharp
let rec spell_word_with blocks w =
    let rec look_for_right_candidate candidates noCandidates c rest =
        match candidates with
        | [] -> false
        | c0::cc ->
            if spell_word_with (cc@noCandidates) rest then true
            else look_for_right_candidate cc (c0::noCandidates) c rest

    match w with
    | "" -> true
    | w ->
        let c = w.[0]
        let rest = w.Substring(1)
        let (candidates, noCandidates) = List.partition(fun (c1,c2) -> c = c1 || c = c2) blocks
        look_for_right_candidate candidates noCandidates c rest

[<EntryPoint>]
let main argv =
    let default_blocks = "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM"
    let blocks =
        (if argv.Length > 0 then argv.[0] else default_blocks).Split()
        |> List.ofArray
        |> List.map(fun s -> s.ToUpper())
        |> List.map(fun s2 -> s2.[0], s2.[1])
    let words =
        (if argv.Length > 0 then List.ofArray(argv).Tail else [])
        |> List.map(fun s -> s.ToUpper())

    List.iter (fun w -> printfn "Using the blocks we can make the word '%s': %b" w (spell_word_with blocks w)) words
    0
```

Output:

```txt
h:\RosettaCode\ABC\Fsharp>RosettaCode "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM" a bark book threat common squad confuse
Using the blocks we can make the word 'A': true
Using the blocks we can make the word 'BARK': true
Using the blocks we can make the word 'BOOK': false
Using the blocks we can make the word 'THREAT': true
Using the blocks we can make the word 'COMMON': false
Using the blocks we can make the word 'SQUAD': true
Using the blocks we can make the word 'CONFUSE': true

h:\RosettaCode\ABC\Fsharp>RosettaCode  "aB aB Ac Ac" abba
Using the blocks we can make the word 'ABBA': true

h:\RosettaCode\ABC\Fsharp>RosettaCode "US TZ AO QA" Auto
Using the blocks we can make the word 'AUTO': true
```



## Factor


```factor
USING: assocs combinators.short-circuit formatting grouping io
kernel math math.statistics qw sequences sets unicode ;
IN: rosetta-code.abc-problem

!
###  CONSTANTS =============================================


CONSTANT: blocks qw{
    BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM
}

CONSTANT: input qw{ A BARK BOOK TREAT COMMON SQUAD CONFUSE }

!
###  PROGRAM LOGIC =========================================


: pare ( str -- seq )
    [ blocks ] dip [ intersects? ] curry filter ;

: enough-blocks? ( str -- ? ) dup pare [ length ] bi@ <= ;

: enough-letters? ( str -- ? )
    [ blocks concat ] dip dup [ within ] dip
    [ histogram values ] bi@ [ - ] 2map [ neg? ] any? not ;

: can-make-word? ( str -- ? )
    >upper { [ enough-blocks? ] [ enough-letters? ] } 1&& ;

!
###  OUTPUT ================================================


: show-blocks ( -- )
    "Available blocks:" print blocks [ 1 cut "(%s %s)" sprintf ]
    map 5 group [ [ write bl ] each nl ] each nl ;

: header ( -- )
    "Word" "Can make word from blocks?" "%-7s %s\n" printf
    "
### ==== =======================
" print ;

: result ( str -- )
    dup can-make-word? "Yes" "No" ? "%-7s %s\n" printf ;

!
###  MAIN ==================================================


: abc-problem ( -- )
    show-blocks header input [ result ] each ;

MAIN: abc-problem
```

Output:

```txt

Available blocks:
(B O) (X K) (D Q) (C P) (N A)
(G T) (R E) (T G) (Q D) (F S)
(J W) (H U) (V I) (A N) (O B)
(E R) (F S) (L Y) (P C) (Z M)

Word    Can make word from blocks?

### ==== =======================

A       Yes
BARK    Yes
BOOK    No
TREAT   Yes
COMMON  No
SQUAD   Yes
CONFUSE Yes

```



## FBSL

This approach uses a string, blanking out the pair previously found. Probably faster than array manipulation.

```qbasic

#APPTYPE CONSOLE
SUB MAIN()
	BlockCheck("A")
	BlockCheck("BARK")
	BlockCheck("BooK")
	BlockCheck("TrEaT")
	BlockCheck("comMON")
	BlockCheck("sQuAd")
	BlockCheck("Confuse")
	pause
END SUB

FUNCTION BlockCheck(str)
	print str " " iif( Blockable( str ), "can", "cannot" ) " be spelled with blocks."
END FUNCTION

FUNCTION Blockable(str AS STRING)
	DIM blocks AS STRING = "BOXKDQCPNAGTRETGQDFSJWHUVIANOBERFSLYPCZM"
	DIM C AS STRING = ""
	DIM POS AS INTEGER = 0

	FOR DIM I = 1 TO LEN(str)
		C = str{i}
		POS = INSTR(BLOCKS, C, 0, 1) 'case insensitive
		IF POS > 0 THEN
			'if the pos is odd, it's the first of the pair
			IF POS MOD 2 = 1 THEN
				'so clear the first and the second
				poke(@blocks + pos - 1," ")
				poke(@blocks + pos," ")
			'otherwise, it's the last of the pair
			ELSE
				'clear the second and the first
				poke(@blocks + pos - 1," ")
				poke(@blocks + pos - 2," ")
			END IF
		ELSE
		'not found, so can't be spelled
		RETURN FALSE
		END IF
	NEXT
	'got thru to here, so can be spelled
	RETURN TRUE
END FUNCTION

```

Output:

```txt

A can be spelled with blocks.
BARK can be spelled with blocks.
BooK cannot be spelled with blocks.
TrEaT can be spelled with blocks.
comMON cannot be spelled with blocks.
sQuAd can be spelled with blocks.
Confuse can be spelled with blocks.

Press any key to continue...

```



## Fortran

Attempts to write the word read from unit 5.  Please find the output, bash command, and gfortran compilation instructions as commentary at the start of the source, which starts right away!

```Fortran
!-*- mode: compilation; default-directory: "/tmp/" -*-
!Compilation started at Thu Jun  5 01:52:03
!
!make f && for a in '' a bark book treat common squad confuse ; do echo $a | ./f ; done
!gfortran -std=f2008 -Wall -fopenmp -ffree-form -fall-intrinsics -fimplicit-none -g f.f08 -o f
! T
! T  A                    NA
! T  BARK                 BO NA RE XK
! F  BOOK                 OB BO -- --
! T  TREAT                GT RE ER NA TG
! F  COMMON               PC OB ZM -- -- --
! T  SQUAD                FS DQ HU NA QD
! T  CONFUSE              CP BO NA FS HU FS RE
!
!Compilation finished at Thu Jun  5 01:52:03

program abc
  implicit none
  integer, parameter :: nblocks = 20
  character(len=nblocks) :: goal
  integer, dimension(nblocks) :: solution
  character(len=2), dimension(0:nblocks) :: blocks_copy, blocks = &
       &(/'--','BO','XK','DQ','CP','NA','GT','RE','TG','QD','FS','JW','HU','VI','AN','OB','ER','FS','LY','PC','ZM'/)
  logical :: valid
  integer :: i, iostat
  read(5,*,iostat=iostat) goal
  if (iostat .ne. 0) goal = ''
  call ucase(goal)
  solution = 0
  blocks_copy = blocks
  valid = assign_block(goal(1:len_trim(goal)), blocks, solution, 1)
  write(6,*) valid, ' '//goal, (' '//blocks_copy(solution(i)), i=1,len_trim(goal))

contains

  recursive function assign_block(goal, blocks, solution, n) result(valid)
    implicit none
    logical :: valid
    character(len=*), intent(in) :: goal
    character(len=2), dimension(0:), intent(inout) :: blocks
    integer, dimension(:), intent(out) :: solution
    integer, intent(in) :: n
    integer :: i
    character(len=2) :: backing_store
    valid = .true.
    if (len(goal)+1 .eq. n) return
    do i=1, size(blocks)
       if (index(blocks(i),goal(n:n)) .ne. 0) then
          backing_store = blocks(i)
          blocks(i) = ''
          solution(n) = i
          if (assign_block(goal, blocks, solution, n+1)) return
          blocks(i) = backing_store
       end if
    end do
    valid = .false.
    return
  end function assign_block

  subroutine ucase(a)
    implicit none
    character(len=*), intent(inout) :: a
    integer :: i, j
    do i = 1, len_trim(a)
       j = index('abcdefghijklmnopqrstuvwxyz',a(i:i))
       if (j .ne. 0) a(i:i) = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'(j:j)
    end do
  end subroutine ucase

end program abc
```



### But if backtracking might be needed

The example set does not exercise the possible need for backtracking, as when an initial selection of blocks prevents completion because available letters have been used up. This can only arise when the same letter appears on more than one block and does so with different partners. The example set does contain duplicated letters, but they appear only via blocks with the same letters. Suppose instead that the block collection was AB, BC, CD, ... XY, YZ so that every letter appears twice except for A and Z. If the target word was STOPPED then both OP and PQ would be needed to supply P, but if the O had been supplied via OP then the second P would be unavailable. If instead the O were to be supplied by NO then all would be well.

The method involves the stack-style usage of array MOVE, but there is no explicit attempt at recursion. The array contains the possible moves at each level, and if necessary, a move made can later be retracted and an alternative sought. This is the standard style of playing board games such as chess via developing a "game tree", but in this case the tree traversal is not a large task.

The following source begins with some support routines. Subroutine PLAY inspects the collection of blocks to make various remarks, and function CANBLOCK reports on whether a word can be spelled out with the supplied blocks. The source requires only a few of the F90 features. The MODULE protocol eases communication, but the key feature is that subprograms can now declare arrays of a size determined on entry via parameters. Previously, a constant with the largest-possible size would be required.

```Fortran

      MODULE PLAYPEN	!Messes with a set of alphabet blocks.
       INTEGER MSG		!Output unit number.
       PARAMETER (MSG = 6)	!Standard output.
       INTEGER MS		!I dislike unidentified constants...
       PARAMETER (MS = 2)	!So this is the maximum number of lettered sides.
       INTEGER LETTER(26),SUPPLY(26)	!For counting the alphabet.
       CONTAINS
        SUBROUTINE SWAP(I,J)	!This really should be known to the compiler.
         INTEGER I,J,K		!Which could generate in-place code,
          K = I			!Using registers, maybe.
          I = J			!Or maybe, there are special op-codes.
          J = K			!Rather than this clunkiness.
        END SUBROUTINE SWAP	!And it should be for any type of thingy.

        INTEGER FUNCTION LSTNB(TEXT)  !Sigh. Last Not Blank.
Concocted yet again by R.N.McLean (whom God preserve) December MM.
Code checking reveals that the Compaq compiler generates a copy of the string and then finds the length of that when using the latter-day intrinsic LEN_TRIM. Madness!
Can't   DO WHILE (L.GT.0 .AND. TEXT(L:L).LE.' ')	!Control chars. regarded as spaces.
Curse the morons who think it good that the compiler MIGHT evaluate logical expressions fully.
Crude GO TO rather than a DO-loop, because compilers use a loop counter as well as updating the index variable.
Comparison runs of GNASH showed a saving of ~3% in its mass-data reading through the avoidance of DO in LSTNB alone.
Crappy code for character comparison of varying lengths is avoided by using ICHAR which is for single characters only.
Checking the indexing of CHARACTER variables for bounds evoked astounding stupidities, such as calculating the length of TEXT(L:L) by subtracting L from L!
Comparison runs of GNASH showed a saving of ~25-30% in its mass data scanning for this, involving all its two-dozen or so single-character comparisons, not just in LSTNB.
         CHARACTER*(*),INTENT(IN):: TEXT	!The bumf. If there must be copy-in, at least there need not be copy back.
         INTEGER L		!The length of the bumf.
          L = LEN(TEXT)		!So, what is it?
    1     IF (L.LE.0) GO TO 2	!Are we there yet?
          IF (ICHAR(TEXT(L:L)).GT.ICHAR(" ")) GO TO 2	!Control chars are regarded as spaces also.
          L = L - 1		!Step back one.
          GO TO 1		!And try again.
    2     LSTNB = L		!The last non-blank, possibly zero.
         RETURN			!Unsafe to use LSTNB as a variable.
        END FUNCTION LSTNB	!Compilers can bungle it.

        SUBROUTINE LETTERCOUNT(TEXT)	!Count the occurrences of A-Z.
         CHARACTER*(*) TEXT	!The text to inspect.
         INTEGER I,K		!Assistants.
          DO I = 1,LEN(TEXT)		!Step through the text.
            K = ICHAR(TEXT(I:I)) - ICHAR("A") + 1	!This presumes that A-Z have contiguous codes!
            IF (K.GE.1 .AND. K.LE.26) LETTER(K) = LETTER(K) + 1	!Not so with EBCDIC!!
          END DO			!On to the next letter.
        END SUBROUTINE LETTERCOUNT	!Be careful with LETTER.

        SUBROUTINE UPCASE(TEXT)	!In the absence of an intrinsic...
Converts any lower case letters in TEXT to upper case...
Concocted yet again by R.N.McLean (whom God preserve) December MM.
Converting from a DO loop evades having both an iteration counter to decrement and an index variable to adjust.
         CHARACTER*(*) TEXT	!The stuff to be modified.
c        CHARACTER*26 LOWER,UPPER	!Tables. a-z may not be contiguous codes.
c        PARAMETER (LOWER = "abcdefghijklmnopqrstuvwxyz")
c        PARAMETER (UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
CAREFUL!! The below relies on a-z and A-Z being contiguous, as is NOT the case with EBCDIC.
         INTEGER I,L,IT		!Fingers.
          L = LEN(TEXT)		!Get a local value, in case LEN engages in oddities.
          I = L			!Start at the end and work back..
    1     IF (I.LE.0) RETURN 	!Are we there yet? Comparison against zero should not require a subtraction.
c         IT = INDEX(LOWER,TEXT(I:I))	!Well?
c         IF (IT .GT. 0) TEXT(I:I) = UPPER(IT:IT)	!One to convert?
          IT = ICHAR(TEXT(I:I)) - ICHAR("a")		!More symbols precede "a" than "A".
          IF (IT.GE.0 .AND. IT.LE.25) TEXT(I:I) = CHAR(IT + ICHAR("A"))	!In a-z? Convert!
          I = I - 1			!Back one.
          GO TO 1			!Inspect..
        END SUBROUTINE UPCASE	!Easy.

        SUBROUTINE ORDERSIDE(LETTER)	!Puts the letters into order.
         CHARACTER*(*) LETTER	!The letters.
         INTEGER I,N,H		!Assistants.
         CHARACTER*1 T		!A scratchpad.
         LOGICAL CURSE		!A bit.
          N = LEN(LETTER)	!So, how many letters?
          H = N - 1		!Last - First, and not +1.
          IF (H.LE.0) RETURN	!Ha ha.
    1     H = MAX(1,H*10/13)		!The special feature.
          IF (H.EQ.9 .OR. H.EQ.10) H = 11	!A twiddle.
          CURSE = .FALSE.		!So far, so good.
          DO I = N - H,1,-1		!If H = 1, this is a BubbleSort.
            IF (LETTER(I:I).LT.LETTER(I + H:I + H)) THEN	!One compare.
              T = LETTER(I:I)			!One swap.
              LETTER(I:I) = LETTER(I + H:I + H)	!Alas, no SWAP(A,B)
              LETTER(I + H:I + H) = T		!Is recognised by the compiler.
              CURSE = .TRUE.		!If once a tiger is seen...
            END IF			!So much for that comparison.
          END DO			!On to the next.
          IF (CURSE .OR. H.GT.1) GO TO 1!Another pass?
        END SUBROUTINE ORDERSIDE	!Simple enough.
        SUBROUTINE ORDERBLOCKS(N,SOME)	!Puts the collection of blocks into order.
         INTEGER N		!The number of blocks.
         CHARACTER*(*) SOME(:)	!Their lists of letters.
         INTEGER I,H		!Assistants.
         CHARACTER*(LEN(SOME(1))) T	!A scratchpad matching an element of SOME.
         LOGICAL CURSE			!Since there is still no SWAP(SOME(I),SOME(I + H)).
          H = N - 1		!So here comes another CombSort.
          IF (H.LE.0) RETURN	!With standard suspicion.
    1     H = MAX(1,H*10/13)		!This is the outer loop.
          IF (H.EQ.9 .OR. H.EQ.10) H = 11	!This is a fiddle.
          CURSE = .FALSE.		!Start the next pass in hope.
          DO I = N - H,1,-1		!Going backwards, just for fun.
            IF (SOME(I).LT.SOME(I + H)) THEN	!So then?
              T = SOME(I)		!Disorder.
              SOME(I) = SOME(I + H)	!So once again,
              SOME(I + H) = T		!Swap the two miscreants.
              CURSE = .TRUE.		!And remember.
            END IF			!So much for that comparison.
          END DO			!On to the next.
          IF (CURSE .OR. H.GT.1) GO TO 1!Are we there yet?
        END SUBROUTINE ORDERBLOCKS	!Not much code, but ringing the changes is still tedious.

        SUBROUTINE PLAY(N,SOME)	!Mess about with the collection of blocks.
         INTEGER N		!Their number.
         CHARACTER*(*) SOME(:)	!Their letters.
         INTEGER NH,HIT(N)	!A list of blocks.
         INTEGER B,I,J,K,L,M	!Assistants.
         CHARACTER*1 C		!A letter of the moment.
          L = LEN(SOME(1))	!The maximum number of letters to any block.
Cast the collection on to the floor.
          WRITE (MSG,1) N,L,SOME	!Announce the set as it is supplied.
    1     FORMAT (I7," blocks, with at most",I2," letters:",66(1X,A))
Change the "orientation" of some blocks.
          DO B = 1,N		!Step through each block.
            CALL UPCASE(SOME(B))	!Paranoia rules.
            CALL ORDERSIDE(SOME(B))	!Put its letter list into order.
          END DO		!On to the next block.
          WRITE (MSG,2) SOME	!Reveal the orderly array.
    2     FORMAT (6X,"... the letters in reverse order:",66(1X,A))
Collate the collection of blocks.
          CALL ORDERBLOCKS(N,SOME)	!Now order the blocks by their letters.
          WRITE (MSG,3) SOME		!Reveal them in neato order.
    3     FORMAT (7X,"... the blocks in reverse order:",66(1X,A))
Count the appearances of the letters of the alphabet.
          LETTER = 0		!Enough of shuffling blocks around.
          DO B = 1,N		!Now inspect their collective letters.
            CALL LETTERCOUNT(SOME(B))	!A block's worth at a go.
          END DO		!On to the next block.
          SUPPLY = LETTER	!Save the counts of supplied letters.
          WRITE (MSG,4) (CHAR(ICHAR("A") + I - 1),I = 1,26),SUPPLY	!Results.
    4     FORMAT (15X,"Letters of the alphabet:",26A<MS + 1>,/,	!First, a line with A ... Z.
     1     11X,"... number thereof supplied:",26I<MS + 1>)	!Then a line of the associated counts.
Check for blocks with duplicated letters.
          WRITE (MSG,5)		!Announce.
    5     FORMAT (8X,"Blocks with duplicated letters:",$)	!Further output impends.
          M = 0			!No duplication found.
          DO B = 1,N		!So step through each block.
         JJ:DO J = 2,L			!Inspecting successive letters of the block,
              IF (SOME(B)(J:J).LE." ") EXIT JJ	!Provided they've not run out.
              DO K = 1,J - 1			!To see if it has appeared earlier.
                IF (SOME(B)(K:K).LE." ") EXIT JJ!Reverse order means that spaces will be at the end!
                IF (SOME(B)(J:J).EQ.SOME(B)(K:K)) THEN	!Well?
                  M = M + 1		!A match!
                  WRITE (MSG,6) SOME(B)	!Name the block.
    6             FORMAT (1X,A,$)	!With further output still impending,
                  EXIT JJ		!And give up on this block.
                END IF			!One duplicated letter is sufficient for its downfall.
              END DO			!Next letter up.
            END DO JJ			!On to the next letter of the block.
          END DO		!On to the next block.
          CALL HIC(M)		!Show the count and end the line.
Check for duplicate blocks, knowing that the array of blocks is ordered.
          WRITE (MSG,7)		!Announce.
    7     FORMAT (21X,"Duplicated blocks:",$)	!Again, leave the line dangling.
          K = 0			!No duplication found.
          B = 1			!Syncopation.
   70     B = B + 1		!Advance one.
          IF (B.GT.N) GO TO 72	!Are we there yet?
          IF (SOME(B).NE.SOME(B - 1)) GO TO 70	!No match? Search on.
          K = K + 1		!A match is counted.
          WRITE (MSG,6) SOME(B)	!Name it.
   71     B = B + 1		!And speed through continued matching.
          IF (B.GT.N) GO TO 72	!Unless we're of the end.
          IF (SOME(B).EQ.SOME(B - 1)) GO TO 71	!Continued matching?
          GO TO 70		!Mismatch: resume the normal scan.
   72     CALL HIC(K)		!So much for that.
Check for duplicated letters across different blocks.
          IF (ALL(SUPPLY.LE.1)) RETURN	!Unless there are no duplicated letters.
          WRITE (MSG,8)		!Announce.
    8     FORMAT ("Duplicated letters on different blocks:",$)	!More to come.
          K = 0		!Start another count.
          DO I = 1,26		!A well-known span.
            IF (SUPPLY(I).LE.1) CYCLE	!Any duplicated letters?
            C = CHAR(ICHAR("A") + I - 1)!Yes. This is the character.
            NH = 0		!So, how many blocks contribute?
            DO B = 1,N		!Find out.
              IF (INDEX(SOME(B),C).GT.0) THEN	!On this block?
                NH = NH + 1		!Yes.
                HIT(NH) = B		!Keep track of which.
              END IF			!So much for that block.
            END DO		!On to the next.
            IF (ANY(SOME(HIT(2:NH)) .NE. SOME(HIT(1)))) THEN	!All have the same collection of letters?
              K = K + 1			!No!
              WRITE (MSG,9) C		!Name the heterogenously supported letter.
    9         FORMAT (A<MS + 1>,$)	!Use the same spacing even though one character only.
            END IF		!So much for that letter's search.
          END DO		!On to the next letter.
          CALL HIC(K)	!Finish the line with the count report.
         CONTAINS	!This is used often enough.
          SUBROUTINE HIC(N)	!But has very specific context.
           INTEGER N			!The count.
            IF (N.LE.0) WRITE (MSG,*) "None."	!Yes, we have no bananas.
            IF (N.GT.0) WRITE (MSG,*) N		!Either way, end the line.
          END SUBROUTINE HIC	!This service routine is not needed elsewhere.
        END SUBROUTINE PLAY	!Look mummy! All the blockses are neatened!

        LOGICAL FUNCTION CANBLOCK(WORD,N,SOME)	!Can the blocks spell out the word?
Creates a move tree based on the letters of WORD and for each, the blocks available.
         CHARACTER*(*) WORD	!The word to spell out.
         INTEGER N		!The number of blocks.
         CHARACTER*(*) SOME(:)	!The blocks and their letters.
         INTEGER NA,AVAIL(N)	!Say not the struggle naught availeth!
         INTEGER NMOVE(LEN(WORD))	!I need a list of acceptable blocks,
         INTEGER MOVE(LEN(WORD),N)	!One list for each letter of WORD.
         INTEGER I,L,S		!Assistants.
         CHARACTER*1 C		!The letter of the moment.
          CANBLOCK = .FALSE.		!Initial pessimism.
          L = LSTNB(WORD)		!Ignore trailing spaces.
          IF (L.GT.N) RETURN		!Enough blocks?
          LETTER = 0				!To make rabbit stew,
          CALL LETTERCOUNT(WORD(1:L))		!First catch your rabbit.
          IF (ANY(SUPPLY .LT. LETTER)) RETURN	!The larder is lacking.
          NA = N			!Prepare a list.
          FORALL (I = 1:N) AVAIL(I) = I	!That fingers every block.
          I = 0		!Step through the letters of the WORD.
Chug through the letters of the WORD.
    1     I = I + 1	!One letter after the other.
          IF (I.GT.L) GO TO 100	!Yay! We're through!
          C = WORD(I:I)		!The letter of the moment.
          NMOVE(I) = 0		!No moves known at this new level.
          DO S = 1,NA		!So, look for them amongst the available slots.
            IF (INDEX(SOME(AVAIL(S)),C) .GT. 0) THEN	!A hit?
              NMOVE(I) = NMOVE(I) + 1	!Yes! Count up another possible move.
              MOVE(I,NMOVE(I)) = S	!Remember its slot.
            END IF			!So much for that block.
          END DO		!On to the next.
    2     IF (NMOVE(I).GT.0) THEN	!Have we any moves?
            S = MOVE(I,NMOVE(I))	!Yes! Recover the last found.
            NMOVE(I) = NMOVE(I) - 1	!Uncount, as it is about to be used.
            IF (S.NE.NA) CALL SWAP(AVAIL(S),AVAIL(NA))	!This block is no longer available.
            NA = NA - 1			!Shift the boundary back.
            GO TO 1			!Try the next letter!
          END IF		!But if we can't find a move at that level...
          I = I - 1		!Retreat a level.
          IF (I.LE.0) RETURN	!Oh dear!
          S = MOVE(I,NMOVE(I) + 1)	!Undo the move that had been made at this level.
          NA = NA + 1			!And make its block is re-available.
          IF (S.NE.NA) CALL SWAP(AVAIL(S),AVAIL(NA))	!Move it back.
          GO TO 2		!See what moves remain at this level.
Completed!
  100     CANBLOCK = .TRUE.	!That's a relief.
        END FUNCTION CANBLOCK	!Some revisions might have been made.
      END MODULE PLAYPEN	!No sand here.

      USE PLAYPEN	!Just so.
      INTEGER HAVE,TESTS		!Parameters for the specified problem.
      PARAMETER (HAVE = 20, TESTS = 7)	!Number of blocks, number of tests.
      CHARACTER*(MS) BLOCKS(HAVE)	!Have blocks, will juggle.
      DATA BLOCKS/"BO","XK","DQ","CP","NA","GT","RE","TG","QD","FS",	!The specified set
     1            "JW","HU","VI","AN","OB","ER","FS","LY","PC","ZM"/	!Of letter blocks.
      CHARACTER*8 WORD(TESTS)		!Now for the specified test words.
      LOGICAL ANS(TESTS),T,F		!And the given results.
      PARAMETER (T = .TRUE., F = .FALSE.)	!Enable a more compact specification.
      DATA WORD/"A","BARK","BOOK","TREAT","COMMON","SQUAD","CONFUSE"/	!So that these
      DATA  ANS/ T ,    T ,    F ,     T ,      F ,     T ,       T /	!Can be aligned.
      LOGICAL YAY
      INTEGER I

      WRITE (MSG,1)
    1 FORMAT ("Arranges alphabet blocks, attending only to the ",
     1 "letters on the blocks, and ignoring case and orientation.",/)

      CALL PLAY(HAVE,BLOCKS)	!Some fun first.

      WRITE (MSG,'(/"Now to see if some words can be spelled out.")')
      DO I = 1,TESTS
        CALL UPCASE(WORD(I))
        YAY = CANBLOCK(WORD(I),HAVE,BLOCKS)
        WRITE (MSG,*) YAY,ANS(I),YAY.EQ.ANS(I),WORD(I)
      END DO
      END

```

Output: the first column of T/F is the report from CANBLOCK, the second is the expected answer from the example, and the third is whether the two are in agreement.

```txt

Arranges alphabet blocks, attending only to the letters on the blocks, and ignoring case and orientation.

     20 blocks, with at most 2 letters: BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM
      ... the letters in reverse order: OB XK QD PC NA TG RE TG QD SF WJ UH VI NA OB RE SF YL PC ZM
       ... the blocks in reverse order: ZM YL XK WJ VI UH TG TG SF SF RE RE QD QD PC PC OB OB NA NA
               Letters of the alphabet:  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z
           ... number thereof supplied:  2  2  2  2  2  2  2  1  1  1  1  1  1  2  2  2  2  2  2  2  1  1  1  1  1  1
        Blocks with duplicated letters: None.
                     Duplicated blocks: TG SF RE QD PC OB NA           7
Duplicated letters on different blocks: None.

Now to see if some words can be spelled out.
 T T T A
 T T T BARK
 F F T BOOK
 T T T TREAT
 F F T COMMON
 T T T SQUAD
 T T T CONFUSE

```


## FreeBASIC


```freebasic
' version 28-01-2019
' compile with: fbc -s console

Dim As String blocks(1 To 20, 1 To 2) => {{"B", "O"}, {"X", "K"}, {"D", "Q"}, _
{"C", "P"}, {"N", "A"}, {"G", "T"}, {"R", "E"}, {"T", "G"}, {"Q", "D"}, _
{"F", "S"}, {"J", "W"}, {"H", "U"}, {"V", "I"}, {"A", "N"}, {"O", "B"}, _
{"E", "R"}, {"F", "S"}, {"L", "Y"}, {"P", "C"}, {"Z", "M"}}

Dim As UInteger i, x, y, b()
Dim As String word, char
Dim As boolean possible

Do
    Read word
    If word = "" Then Exit Do
    word = UCase(word)
    ReDim b(1 To 20)
    possible = TRUE

    For i = 1 To Len(word)
        char = Mid(word, i, 1)

        For x = 1 To 20
            If b(x) = 0 Then
                If blocks(x, 1) = char Or blocks(x, 2) = char Then
                    b(x) = 1
                    Exit For
                End If
            End If
        Next
        If x = 21 Then possible = FALSE
    Next

    Print word, possible
Loop

Data  "A", "Bark", "Book", "Treat", "Common", "Squad", "Confuse", ""

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

Output:

```txt
A           true
BARK          true
BOOK          false
TREAT         true
COMMON        false
SQUAD         true
CONFUSE       true
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=ae860292d4588b3627d77c85bcc634ee Click this link to run this code]'''

```gambas
Public Sub Main()
Dim sCheck As String[] = ["A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"]
Dim sBlock As String[] = ["BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS", "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM"]
Dim sList As New String[]
Dim siCount, siLoop As Short
Dim sTemp, sAnswer As String

For Each sTemp In sCheck
  sAnswer = ""
  sList = sBlock.Copy()
  For siCount = 1 To Len(sTemp)
    For siLoop = 0 To sList.Max
      If InStr(sList[siLoop], Mid(sTemp, siCount, 1)) Then
        sList.Extract(siLoop, 1)
        sAnswer &= Mid(sTemp, siCount, 1)
        Break
      Endif
    Next
  Next

 If sAnswer = sTemp Then
   Print sTemp & " - True"
 Else
   Print sTemp & " - False"
 End If
Next

End
```

Output:

```txt

A - True
BARK - True
BOOK - False
TREAT - True
COMMON - False
SQUAD - True
CONFUSE - True

```



## Go


```go
package main

import (
	"fmt"
	"strings"
)

func newSpeller(blocks string) func(string) bool {
	bl := strings.Fields(blocks)
	return func(word string) bool {
		return r(word, bl)
	}
}

func r(word string, bl []string) bool {
	if word == "" {
		return true
	}
	c := word[0] | 32
	for i, b := range bl {
		if c == b[0]|32 || c == b[1]|32 {
			bl[i], bl[0] = bl[0], b
			if r(word[1:], bl[1:]) == true {
				return true
			}
			bl[i], bl[0] = bl[0], bl[i]
		}
	}
	return false
}

func main() {
	sp := newSpeller(
		"BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM")
	for _, word := range []string{
		"A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"} {
		fmt.Println(word, sp(word))
	}
}
```

Output:

```txt

A true
BARK true
BOOK false
TREAT true
COMMON false
SQUAD true
CONFUSE true

```



## Groovy

Solution:

```groovy
class ABCSolver {
    def blocks

    ABCSolver(blocks = []) { this.blocks = blocks }

    boolean canMakeWord(rawWord) {
        if (rawWord == '' || rawWord == null) { return true; }
        def word = rawWord.toUpperCase()
        def blocksLeft = [] + blocks
        word.every { letter -> blocksLeft.remove(blocksLeft.find { block -> block.contains(letter) }) }
    }
}
```


Test:

```groovy
def a = new ABCSolver(["BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS",
                      "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM"])

['', 'A', 'BARK', 'book', 'treat', 'COMMON', 'SQuAd', 'CONFUSE'].each {
    println "'${it}': ${a.canMakeWord(it)}"
}
```


Output:

```txt
'': true
'A': true
'BARK': true
'book': false
'treat': true
'COMMON': false
'SQuAd': true
'CONFUSE': true
```



## Harbour

Harbour Project implements a cross-platform Clipper/xBase compiler.

```visualfoxpro
PROCEDURE Main()

   LOCAL cStr

   FOR EACH cStr IN { "A", "BARK", "BooK", "TrEaT", "comMON", "sQuAd", "Confuse" }
      ? PadL( cStr, 10 ), iif( Blockable( cStr ), "can", "cannot" ), "be spelled with blocks."
   NEXT

   RETURN

STATIC FUNCTION Blockable( cStr )

   LOCAL blocks := { ;
      "BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS", ;
      "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM" }

   LOCAL cFinal := ""
   LOCAL i, j

   cStr := Upper( cStr )

   FOR i := 1 TO Len( cStr )
      FOR EACH j IN blocks
         IF SubStr( cStr, i, 1 ) $ j
            cFinal += SubStr( cStr, i, 1 )
            j := ""
            EXIT
         ENDIF
      NEXT
   NEXT

   RETURN cFinal == cStr
```

Output:

```txt

         A can be spelled with blocks.
      BARK can be spelled with blocks.
      BooK cannot be spelled with blocks.
     TrEaT can be spelled with blocks.
    comMON cannot be spelled with blocks.
     sQuAd can be spelled with blocks.
   Confuse can be spelled with blocks.
```



## Haskell


The following function returns a list of all the solutions. Since Haskell is lazy, testing whether the list is null will only do the minimal amount of work necessary to determine whether a solution exists.

```haskell
import Data.List (delete)
import Data.Char (toUpper)

-- returns list of all solutions, each solution being a list of blocks
abc :: (Eq a) => [[a]] -> [a] -> [[[a]]]
abc _ [] = [[]]
abc blocks (c:cs) = [b:ans | b <- blocks, c `elem` b,
                             ans <- abc (delete b blocks) cs]

blocks = ["BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS",
          "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM"]

main :: IO ()
main = mapM_ (\w -> print (w, not . null $ abc blocks (map toUpper w)))
         ["", "A", "BARK", "BoOK", "TrEAT", "COmMoN", "SQUAD", "conFUsE"]
```


Output:

```txt

("",True)
("A",True)
("BARK",True)
("BoOK",False)
("TrEAT",True)
("COmMoN",False)
("SQUAD",True)
("conFUsE",True)

```


Or, in terms of the bind operator:


```haskell
import Data.List (delete)
import Data.Char (toUpper)

spellWith :: [String] -> String -> [[String]]
spellWith _ [] = [[]]
spellWith blocks (x:xs) =
  let go b
        | x `elem` b = (b :) <$> spellWith (delete b blocks) xs
        | otherwise = []
  in blocks >>= go

blocks :: [String]
blocks = words "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM"

main :: IO ()
main =
  mapM_
    (print . ((,) <*>) (not . null . spellWith blocks . fmap toUpper))
    ["", "A", "BARK", "BoOK", "TrEAT", "COmMoN", "SQUAD", "conFUsE"]
```

Output:

```txt
("",True)
("A",True)
("BARK",True)
("BoOK",False)
("TrEAT",True)
("COmMoN",False)
("SQUAD",True)
("conFUsE",True)
```


=={{header|Icon}} and {{header|Unicon}}==
Translated from C

Works in both languages:

```unicon
procedure main(A)
    blocks := ["bo","xk","dq","cp","na","gt","re","tg","qd","fs",
               "jw","hu","vi","an","ob","er","fs","ly","pc","zm",&null]
    every write("\"",word := !A,"\" ",checkSpell(map(word),blocks)," with blocks.")
end

procedure checkSpell(w,blocks)
    blks := copy(blocks)
    w ? return if canMakeWord(blks) then "can be spelled"
                                    else "can not be spelled"
end

procedure canMakeWord(blks)
    c := move(1) | return
    if /blks[1] then fail
    every i := 1 to *blks do {
        if /blks[i] then (move(-1),fail)
        if c == !blks[i] then {
            blks[1] :=: blks[i]
            if canMakeWord(blks[2:0]) then return
            blks[1] :=: blks[i]
            }
        }
end
```


Sample run:

```txt

->abc "" A BARK BOOK TREAT COMMON SQUAD CONFUSE
"" can be spelled with blocks.
"A" can be spelled with blocks.
"BARK" can be spelled with blocks.
"BOOK" can not be spelled with blocks.
"TREAT" can be spelled with blocks.
"COMMON" can not be spelled with blocks.
"SQUAD" can be spelled with blocks.
"CONFUSE" can be spelled with blocks.
->

```



## J

'''Solution:'''

```j
reduce=: verb define
  'rows cols'=. i.&.> $y
  for_c. cols do.
    r=. 1 i.~ c {"1 y             NB. row idx of first 1 in col
    if. r = #rows do. continue. end.
    y=. 0 (<((r+1)}.rows);c) } y  NB. zero rest of col
    y=. 0 (<(r;(c+1)}.cols)) } y  NB. zero rest of row
  end.
)

abc=: *./@(+./)@reduce@(e."1~ ,)&toupper :: 0:
```

'''Examples:'''

```j
   Blocks=:  ];._2 'BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM '
   ExampleWords=: <;._2 'A BaRK BOoK tREaT COmMOn SqUAD CoNfuSE '

   Blocks&abc &> ExampleWords
1 1 0 1 0 1 1
   require 'format/printf'
   '%10s  %s' printf (dquote ; 'FT' {~ Blocks&abc) &> ExampleWords
       "A"  T
    "BaRK"  T
    "BOoK"  F
   "tREaT"  T
  "COmMOn"  F
   "SqUAD"  T
 "CoNfuSE"  T
```


'''Tacit version'''

```j
delElem=: {~<@<@<
uppc=:(-32*96&<*.123&>)&.(3&u:)
reduc=: ] delElem  1 i.~e."0 1
forms=:  (1 - '' -: (reduc L:0/ :: (a:"_)@(<"0@],<@[))&uppc) L:0
```


Output:

```txt
   (,.Blocks&forms) ExampleWords
┌───────┬─┐
│A      │1│
├───────┼─┤
│BaRK   │1│
├───────┼─┤
│BOoK   │0│
├───────┼─┤
│tREaT  │1│
├───────┼─┤
│COmMOn │0│
├───────┼─┤
│SqUAD  │1│
├───────┼─┤
│CoNfuSE│1│
└───────┴─┘
```



### Alternative Implementation


Another approach might be:


```J>Blocks=:
;:'BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM '
ExampleWords=: ;: 'A BaRK BOoK tREaT COmMOn SqUAD CoNfuSE '

canform=:4 :0
  word=: toupper y
  need=: #/.~ word,word
  relevant=: (x +./@e."1 word) # x
  candidates=: word,"1>,{{relevant
  +./(((#need){. #/.~)"1 candidates) */ .>:need
)
```


Example use:


```J
   Blocks canform 0{::ExampleWords
1
   Blocks canform 1{::ExampleWords
1
   Blocks canform 2{::ExampleWords
0
   Blocks canform 3{::ExampleWords
1
   Blocks canform 4{::ExampleWords
0
   Blocks canform 5{::ExampleWords
1
   Blocks canform 6{::ExampleWords
1
```


Explanation:

We only need to consider blocks which contain letters in common with a normalized (upper case) version of the desired word. But we do need to consider all possible combinations of letters from those blocks (see talk page discussion of words like 'ABBA' for more on this issue).

We can classify possibilities by counting how many of each letter occur. If a candidate has at least as many of the required letters as a test case constructed from the word itself, it's a valid candidate.

For example:


```J
   Blocks canform 0{::ExampleWords
1
   word
A
   need
2
   relevant
NA
AN
   candidates
ANA
ANN
AAA
AAN
```


Here, the word is simply 'A', and we have two blocks to consider for our word: AN and NA. So we form all possible combinations of the letters of those two bocks, prefix each of them with our word and test whether any of them contain two copies of the letters of our word. (As it happens, all of the candidates are valid, for this trivial example.)


## Java

Translated from C
Works with Java|1.6+}}

```java5
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class ABC {

    public static void main(String[] args) {
        List<String> blocks = Arrays.asList(
                "BO", "XK", "DQ", "CP", "NA",
                "GT", "RE", "TG", "QD", "FS",
                "JW", "HU", "VI", "AN", "OB",
                "ER", "FS", "LY", "PC", "ZM");

        for (String word : Arrays.asList("", "A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE")) {
            System.out.printf("%s: %s%n", word.isEmpty() ? "\"\"" : word, canMakeWord(word, blocks));
        }
    }

    public static boolean canMakeWord(String word, List<String> blocks) {
        if (word.isEmpty())
            return true;

        char c = word.charAt(0);
        for (int i = 0; i < blocks.size(); i++) {
            String b = blocks.get(i);
            if (b.charAt(0) != c && b.charAt(1) != c)
                continue;
            Collections.swap(blocks, 0, i);
            if (canMakeWord(word.substring(1), blocks.subList(1, blocks.size())))
                return true;
            Collections.swap(blocks, 0, i);
        }

        return false;
    }
}
```

Output:

```txt
"": true
A: true
BARK: true
book: false
treat: true
COMMON: false
SQuAd: true
CONFUSE: true
```



## JavaScript


### ES5


### =Imperative=

The following method uses regular expressions and the string replace function to allow more support for older browsers.

```javascript
var blocks = "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM";

function CheckWord(blocks, word) {
   // Makes sure that word only contains letters.
   if(word !== /([a-z]*)/i.exec(word)[1]) return false;
   // Loops through each character to see if a block exists.
   for(var i = 0; i < word.length; ++i)
   {
      // Gets the ith character.
      var letter = word.charAt(i);
      // Stores the length of the blocks to determine if a block was removed.
      var length = blocks.length;
      // The regexp gets constructed by eval to allow more browsers to use the function.
      var reg = eval("/([a-z]"+letter+"|"+letter+"[a-z])/i");
      // This does the same as above, but some browsers do not support...
      //var reg = new RegExp("([a-z]"+letter+"|"+letter+"[a-z])", "i");
      // Removes all occurrences of the match.
      blocks = blocks.replace(reg, "");
      // If the length did not change then a block did not exist.
      if(blocks.length === length) return false;
   }
   // If every character has passed then return true.
   return true;
};

var words = [
   "A",
   "BARK",
   "BOOK",
   "TREAT",
   "COMMON",
   "SQUAD",
   "CONFUSE"
];

for(var i = 0;i<words.length;++i)
   console.log(words[i] + ": " + CheckWord(blocks, words[i]));

```


Result:

```txt

A: true
BARK: true
BOOK: false
TREAT: true
COMMON: false
SQUAD: true
CONFUSE: true

```



### =Functional=


```JavaScript
(function (strWords) {

    var strBlocks =
        'BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM',
        blocks = strBlocks.split(' ');

    function abc(lstBlocks, strWord) {
        var lngChars = strWord.length;

        if (!lngChars) return [];

        var b = lstBlocks[0],
            c = strWord[0];

        return chain(lstBlocks, function (b) {
            return (b.indexOf(c.toUpperCase()) !== -1) ? [
                (b + ' ').concat(
                    abc(removed(b, lstBlocks), strWord.slice(1)))
            ] : [];
        })
    }

    // Monadic bind (chain) for lists
    function chain(xs, f) {
        return [].concat.apply([], xs.map(f));
    }

    // a -> [a] -> [a]
    function removed(x, xs) {
        var h = xs.length ? xs[0] : null,
            t = h ? xs.slice(1) : [];

        return h ? (
            h === x ? t : [h].concat(removed(x, t))
        ) : [];
    }

    function solution(strWord) {
        var strAttempt = abc(blocks, strWord)[0].split(',')[0];

        // two chars per block plus one space -> 3
        return strWord + ((strAttempt.length === strWord.length * 3) ?
            ' -> ' + strAttempt : ': [no solution]');
    }

    return strWords.split(' ').map(solution).join('\n');

})('A bark BooK TReAT COMMON squAD conFUSE');
```

Output:

```JavaScript
A -> NA
bark -> BO NA RE XK
BooK: [no solution]
TReAT -> GT RE ER NA TG
COMMON: [no solution]
squAD -> FS DQ HU NA QD
conFUSE -> CP BO NA FS HU FS RE
```



### ES6


### =Imperative=


```javascript
let characters = "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM";
let blocks = characters.split(" ").map(pair => pair.split(""));

function isWordPossible(word) {
  var letters = [...word.toUpperCase()];
  var length = letters.length;
  var copy = new Set(blocks);

  for (let letter of letters) {
    for (let block of copy) {
      let index = block.indexOf(letter);

      if (index !== -1) {
        length--;
        copy.delete(block);
        break;
      }
    }

  }
  return !length;
}

[
  "A",
  "BARK",
  "BOOK",
  "TREAT",
  "COMMON",
  "SQUAD",
  "CONFUSE"
].forEach(word => console.log(`${word}: ${isWordPossible(word)}`));

```


Result:

```txt
A: true
BARK: true
BOOK: false
TREAT: true
COMMON: false
SQUAD: true
CONFUSE: true
```




### =Functional=

{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // ABC BLOCKS -------------------------------------------------------------

    // spellWith :: [(Char, Char)] -> [Char] -> [[(Char, Char)]]
    const spellWith = (blocks, wordChars) =>
        (isNull(wordChars)) ? [
            []
        ] :
        (() => {
            const [x, xs] = uncons(wordChars);
            return concatMap(
                b => elem(x, b) ? concatMap(
                    bs => [cons(b, bs)],
                    spellWith(
                        deleteBy(
                            (p, q) => (p[0] === q[0]) && (p[1] === q[1]),
                            b, blocks
                        ),
                        xs
                    )
                ) : [],
                blocks
            );
        })();

    // GENERIC FUNCTIONS ------------------------------------------------------

    // compose :: [(a -> a)] -> (a -> a)
    const compose = fs => x => fs.reduceRight((a, f) => f(a), x);

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => [].concat.apply([], xs.map(f));

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) => [x].concat(xs);

    // curry :: Function -> Function
    const curry = (f, ...args) => {
        const go = xs => xs.length >= f.length ? (f.apply(null, xs)) :
            function () {
                return go(xs.concat([].slice.apply(arguments)));
            };
        return go([].slice.call(args, 1));
    };

    // deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
    const deleteBy = (f, x, xs) =>
        xs.length > 0 ? (
            f(x, xs[0]) ? (
                xs.slice(1)
            ) : [xs[0]].concat(deleteBy(f, x, xs.slice(1)))
        ) : [];

    // elem :: Eq a => a -> [a] -> Bool
    const elem = (x, xs) => xs.indexOf(x) !== -1;

    // isNull :: [a] -> Bool
    const isNull = xs => (xs instanceof Array) ? xs.length < 1 : undefined;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // not :: Bool -> Bool
    const not = b => !b;

    // show :: a -> String
    const show = x => JSON.stringify(x); //, null, 2);

    // stringChars :: String -> [Char]
    const stringChars = s => s.split('');

    // toUpper :: Text -> Text
    const toUpper = s => s.toUpperCase();

    // uncons :: [a] -> Maybe (a, [a])
    const uncons = xs => xs.length ? [xs[0], xs.slice(1)] : undefined;

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // words :: String -> [String]
    const words = s => s.split(/\s+/);

    // TEST -------------------------------------------------------------------
    // blocks :: [(Char, Char)]
    const blocks = words(
        "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM"
    );

    return unlines(map(
        x => show([x, compose(
            [not, isNull, curry(spellWith)(blocks), stringChars, toUpper]
        )(x)]), ["", "A", "BARK", "BoOK", "TrEAT", "COmMoN", "SQUAD", "conFUsE"]
    ));
})();
```

Output:

```txt
["",true]
["A",true]
["BARK",true]
["BoOK",false]
["TrEAT",true]
["COmMoN",false]
["SQUAD",true]
["conFUsE",true]
```



## jq

The problem description seems to imply that if a letter, X, appears on more than one block, its partner will be the same on all blocks. This makes the problem trivial.
```jq

# when_index(cond;ary) returns the index of the first element in ary
# that satisfies cond; it uses a helper function that takes advantage
# of tail-recursion optimization in recent versions of jq.
def index_when(cond; ary):
  # state variable: counter
  def when: if . >= (ary | length) then null
            elif ary[.] | cond then .
            else (.+1) | when
            end;
  0 | when;

# Attempt to match a single letter with a block;
# return null if no match, else the remaining blocks
def match_letter(letter):
  . as $ary | index_when( index(letter); $ary ) as $ix
  | if $ix == null then null
    else del( .[$ix] )
    end;

# Usage: string | abc(blocks)
def abc(blocks):
  if length == 0 then true
  else
    .[0:1] as $letter
    | (blocks | match_letter( $letter )) as $blks
    | if $blks == null then false
      else .[1:] | abc($blks)
      end
  end;
```

Task:
```jq
def task:
  ["BO","XK","DQ","CP","NA","GT","RE","TG","QD","FS",
   "JW","HU","VI","AN","OB","ER","FS","LY","PC","ZM"] as $blocks
  | ("A", "BARK","BOOK","TREAT","COMMON","SQUAD","CONFUSE")
  | "\(.) : \( .|abc($blocks) )" ;task
```

Output:
 A : true
 BARK : true
 BOOK : false
 TREAT : true
 COMMON : false
 SQUAD : true
 CONFUSE : true


## Jsish

Based on Javascript ES5 imperative solution.

```javascript
#!/usr/bin/env jsish
/* ABC problem, in Jsish.  Can word be spelled with the given letter blocks. */
var blocks = "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM";

function CheckWord(blocks, word) {
   var re = /([a-z]*)/i;
   if (word !== re.exec(word)[0]) return false;
   for (var i = 0; i < word.length; i++) {
      var letter = word.charAt(i);
      var length = blocks.length;
      // trying both sides
      var reg = new RegExp("([a-z]"+letter + "|" + letter+"[a-z])", "i");
      // remove block once a letter is used
      blocks = blocks.replace(reg, "");
      if (blocks.length === length) return false;
   }
   return true;
};

var words = [ "A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE" ];

puts("Using blocks:", blocks);
for(var i = 0; i<words.length; i++)
    puts(CheckWord(blocks, words[i]) ? "can" : "can't", "spell", words[i]);

/*
=!EXPECTSTART!=
Using blocks: BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM
can spell A
can spell BARK
can't spell BOOK
can spell TREAT
can't spell COMMON
can spell SQUAD
can spell CONFUSE
=!EXPECTEND!=
*/
```


Output:

```txt
prompt$ jsish ABCProblem.jsi
Using blocks: BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM
can spell A
can spell BARK
can't spell BOOK
can spell TREAT
can't spell COMMON
can spell SQUAD
can spell CONFUSE

prompt$ jsish -u ABCProblem.jsi
[PASS] ABCProblem.jsi
```



## Julia


```julia
using Printf

function abc(str::AbstractString, list)
    isempty(str) && return true
    for i in eachindex(list)
        str[end] in list[i] &&
            any([abc(str[1:end-1], deleteat!(copy(list), i))]) &&
            return true
    end
    return false
end

let test = ["A", "BARK","BOOK","TREAT","COMMON","SQUAD","CONFUSE"],
    list = ["BO","XK","DQ","CP","NA","GT","RE","TG","QD","FS",
            "JW","HU","VI","AN","OB","ER","FS","LY","PC","ZM"]
    for str in test
        @printf("%-8s |  %s\n", str, abc(str, list))
    end
end
```


Output:

```txt
A        |  true
BARK     |  true
BOOK     |  false
TREAT    |  true
COMMON   |  false
SQUAD    |  true
CONFUSE  |  true
```



## Kotlin

Translated from Java

```scala
object ABC_block_checker {
    fun run() {
        val blocks = arrayOf("BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS",
                "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM")

        println("\"\": " + blocks.canMakeWord(""))
        val words = arrayOf("A", "BARK", "book", "treat", "COMMON", "SQuAd", "CONFUSE")
        for (w in words)  println("$w: " + blocks.canMakeWord(w))
    }

    private fun Array<String>.swap(i: Int, j: Int) {
        val tmp = this[i]
        this[i] = this[j]
        this[j] = tmp
    }

    private fun Array<String>.canMakeWord(word: String): Boolean {
        if (word.isEmpty())
            return true

        val c = Character.toUpperCase(word.first())
        var i = 0
        forEach { b ->
            if (b.first().toUpperCase() == c || b[1].toUpperCase() == c) {
                swap(0, i)
                if (drop(1).toTypedArray().canMakeWord(word.substring(1)))
                    return true
                swap(0, i)
            }
            i++
        }

        return false
    }
}

fun main(args: Array<String>) = ABC_block_checker.run()
```

Output:

```txt
"": true
A: true
BARK: true
book: false
treat: true
COMMON: false
SQuAd: true
CONFUSE: true
```



## Liberty BASIC


### Recursive solution


```lb

print "RosettaGit - ABC problem (recursive solution)"
print
blocks$="BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM"
data "A"
data "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"
data "XYZZY"

do
    read text$
    if text$="XYZZY" then exit do
    print ">>> can_make_word("; chr$(34); text$; chr$(34); ")"
    if canDo(text$,blocks$) then print "True" else print "False"
loop while 1
print "Program complete."
end

function canDo(text$,blocks$)
    'endcase
    if len(text$)=1 then canDo=(instr(blocks$,text$)<>0): exit function
    'get next letter
    ltr$=left$(text$,1)
    'cut
    if instr(blocks$,ltr$)=0 then canDo=0: exit function
    'recursion
    text$=mid$(text$,2) 'rest
    'loop by all word in blocks. Need to make "newBlocks" - all but taken
    'optimisation: take only fitting blocks
    wrd$="*"
    i=0
    while wrd$<>""
        i=i+1
        wrd$=word$(blocks$, i)
        if instr(wrd$, ltr$) then
            'newblocks without wrd$
            pos=instr(blocks$,wrd$)
            newblocks$=left$(blocks$, pos-1)+mid$(blocks$, pos+3)
            canDo=canDo(text$,newblocks$)
            'first found cuts
            if canDo then exit while
        end if
    wend
end function

```

Output:

```txt

RosettaGit - ABC problem (recursive solution)

>>> can_make_word("A")
True
>>> can_make_word("BARK")
True
>>> can_make_word("BOOK")
False
>>> can_make_word("TREAT")
True
>>> can_make_word("COMMON")
False
>>> can_make_word("SQUAD")
True
>>> can_make_word("CONFUSE")
True
Program complete.

```


### Procedural solution


```lb

print "RosettaGit - ABC problem (procedural solution)"
print
w$(1)="A"
w$(2)="BARK"
w$(3)="BOOK"
w$(4)="TREAT"
w$(5)="COMMON"
w$(6)="SQUAD"
w$(7)="CONFUSE"

for x=1 to 7
    print ">>> can_make_word("; chr$(34); w$(x); chr$(34); ")"
    if CanMakeWord(w$(x)) then print "True" else print "False"
next x
print "Program complete."
end

function CanMakeWord(x$)
global DoneWithWord, BlocksUsed, LetterOK, Possibility
dim block$(20,2), block(20,2)
'numeric blocks, col 0 flags used block
block(1,1)=asc("B")-64: block(1,2)=asc("O")-64
block(2,1)=asc("X")-64: block(2,2)=asc("K")-64
block(3,1)=asc("D")-64: block(3,2)=asc("Q")-64
block(4,1)=asc("C")-64: block(4,2)=asc("P")-64
block(5,1)=asc("N")-64: block(5,2)=asc("A")-64
block(6,1)=asc("G")-64: block(6,2)=asc("T")-64
block(7,1)=asc("R")-64: block(7,2)=asc("E")-64
block(8,1)=asc("T")-64: block(8,2)=asc("G")-64
block(9,1)=asc("Q")-64: block(9,2)=asc("D")-64
block(10,1)=asc("F")-64: block(10,2)=asc("S")-64
block(11,1)=asc("J")-64: block(11,2)=asc("W")-64
block(12,1)=asc("H")-64: block(12,2)=asc("U")-64
block(13,1)=asc("V")-64: block(13,2)=asc("I")-64
block(14,1)=asc("A")-64: block(14,2)=asc("N")-64
block(15,1)=asc("O")-64: block(15,2)=asc("B")-64
block(16,1)=asc("E")-64: block(16,2)=asc("R")-64
block(17,1)=asc("F")-64: block(17,2)=asc("S")-64
block(18,1)=asc("L")-64: block(18,2)=asc("Y")-64
block(19,1)=asc("P")-64: block(19,2)=asc("C")-64
block(20,1)=asc("Z")-64: block(20,2)=asc("M")-64

x$=upper$(x$)
for x=1 to len(x$)
    y$=mid$(x$,x,1)
    if y$>="A" and y$<="Z" then w$=w$+y$
next x
if w$="" then exit function
DoneWithWord=0: BlocksUsed=0
l=len(w$)
dim LetterOK(l)
dim alphabet(26,1) 'clear letter-usage array
for x=1 to 20 'load block letters into letter-usage array col 0
    alphabet(block(x,1),0)+=1
    alphabet(block(x,2),0)+=1
next x
for x=1 to l 'load current word into letter-usage aray col 1
    wl$=mid$(w$,x,1): w=asc(wl$)-64
    alphabet(w,1)+=1
next x

for x=1 to 26 ' test for more of any letter in the word than in the blocks
    if alphabet(x,1)>alphabet(x,0) then exit function
next x

[NextLetter]
if wl<l then wl=wl+1 else goto [DoneWithWord]
wl$=mid$(w$,wl,1): w=asc(wl$)-64
LetterOK=0
' if there's only one of the letter in the blocks then you must use that block
if alphabet(w,0)=1 then
    call OnlyBlock w
    LetterOK(wl)=1
    if DoneWithWord then goto [DoneWithWord] else goto [NextLetter]
end if
' if more than one of the letter in the blocks, then try to use one that has
' an unused letter on other side (a "Free Block")
call FindFreeBlock w
if LetterOK then LetterOK(wl)=1
goto [NextLetter]

[DoneWithWord]
if BlocksUsed=l then CanMakeWord=1: exit function
if DoneWithWord then exit function
for x=1 to l
    if not(LetterOK(x)) then
        NumericLetter=asc(mid$(w$,x,1))-64
        LetterOK=0
        call OnlyBlock NumericLetter
        if LetterOK then LetterOK(x)=1 else exit for
    end if
next x
goto [DoneWithWord]
end function

sub OnlyBlock NumericLetter
    for x=1 to 20
        if (block(x, 1)=NumericLetter or block(x, 2)=NumericLetter) _
                and block(x, 0)=0 then
            call UseBlock x, NumericLetter
            exit sub
        end if
    next x
    DoneWithWord=1
end sub

sub FindFreeBlock NumericLetter
    Possibility=0
    for x=1 to 20
        if block(x, 0)=0 then 'block not used
            if block(x,1)=NumericLetter then
                if alphabet(block(x,2),1)=0 then
                    call UseBlock x, NumericLetter
                    exit sub
                end if
                Possibility=Possibility+1
            end if
            if block(x,2)=NumericLetter then
                if alphabet(block(x,1),1)=0 then
                    call UseBlock x, NumericLetter
                    exit sub
                end if
                Possibility=Possibility+1
            end if
        end if
    next x
end sub

sub UseBlock BlockNumber, NumericLetter
    block(BlockNumber, 0)=1 'Mark block as used
    BlocksUsed=BlocksUsed+1
    LetterOK=1
end sub

```

Output:

```txt

RosettaGit - ABC problem (procedural solution)

>>> can_make_word("A")
True
>>> can_make_word("BARK")
True
>>> can_make_word("BOOK")
False
>>> can_make_word("TREAT")
True
>>> can_make_word("COMMON")
False
>>> can_make_word("SQUAD")
True
>>> can_make_word("CONFUSE")
True
Program complete.

```



## Logo


```logo
make "blocks [[B O] [X K] [D Q] [C P] [N A] [G T] [R E] [T G] [Q D] [F S]
              [J W] [H U] [V I] [A N] [O B] [E R] [F S] [L Y] [P C] [Z M]]

to can_make? :word [:avail :blocks]
  if empty? :word [output "true]
  local "letter make "letter first :word
  foreach :avail [
    local "i     make "i     #
    local "block make "block ?
    if member? :letter :block [
      if (can_make? bf :word filter [notequal? # :i] :avail) [output "true]
    ]
  ]
  output "false
end

foreach [A BARK BOOK TREAT COMMON SQUAD CONFUSE] [
  print sentence word ? ": can_make? ?
]

bye
```


Output:

```txt
A: true
BARK: true
BOOK: false
TREAT: true
COMMON: false
SQUAD: true
CONFUSE: true
```



## Lua


```lua
blocks = {
	{"B","O"};	{"X","K"};	{"D","Q"};	{"C","P"};
	{"N","A"};	{"G","T"};	{"R","E"};	{"T","G"};
	{"Q","D"};	{"F","S"};	{"J","W"};	{"H","U"};
	{"V","I"};	{"A","N"};	{"O","B"};	{"E","R"};
	{"F","S"};	{"L","Y"};	{"P","C"};	{"Z","M"};
	};

function canUse(table, letter)
	for i,v in pairs(blocks) do
		if (v[1] == letter:upper() or v[2] == letter:upper())  and table[i] then
			table[i] = false;
			return true;
		end
	end
	return false;
end

function canMake(Word)
	local Taken = {};
	for i,v in pairs(blocks) do
		table.insert(Taken,true);
	end
	local found = true;
	for i = 1,#Word do
		if not canUse(Taken,Word:sub(i,i)) then
			found = false;
		end
	end
	print(found)
end
```


{{Output}}

```txt

canMake("A"): true
canMake("BARK"): true
canMake("BOOK"): false
canMake("TREAT"): true
canMake("COMMON"): false
canMake("SQUAD"): true
canMake("CONFUSE"): true
```



## Maple


```maple
canSpell := proc(w)
	local blocks, i, j, word, letterFound;
	blocks := Array([["B", "O"], ["X", "K"], ["D", "Q"], ["C", "P"], ["N", "A"], ["G", "T"], ["R", "E"], ["T", "G"],
                         ["Q", "D"], ["F", "S"], ["J", "W"], ["H", "U"], ["V", "I"], ["A", "N"], ["O", "B"], ["E", "R"],
                         ["F", "S"], ["L", "Y"], ["P", "C"], ["Z", "M"]]);
	word := StringTools[UpperCase](convert(w, string));
	for i to length(word) do
		letterFound := false;
		for j to numelems(blocks)/2 do
			if not letterFound and (substring(word, i) = blocks[j,1] or substring(word, i) = blocks[j,2]) then
				blocks[j,1] := undefined;
				blocks[j,2] := undefined;
				letterFound := true;
			end if;
		end do;
		if not letterFound then
			return false;
		end if;
	end do;
	return true;
end proc:

seq(printf("%a: %a\n", i, canSpell(i)), i in [a, Bark, bOok, treat, COMMON, squad, confuse]);
```

Output:

```txt

a: true
Bark: true
bOok: false
treat: true
COMMON: false
squad: true
confuse: true

```



## M2000 Interpreter

We use a subroutine inside a module. Subs are in the same namespace as the module which call them. Subs may exist in the end of module, or in the parent module (which module defined). We have to use Local to define new variables which shadow any module variable. When a sub exit all new variables which made there erased. Modules run on objects which "interprets" code, and subs use modules objects, so they are lighter than modules. A module hold a separate return stack for subs, gosub and for next structures ( a for {} use process stack, and is twice faster as the simple For Next). This return stack is a stack object, which is a collection of objects in heap, so we can use  '''Recursion.Limit 100000''' to set limit to 100000 calls for subs. Here we use a for next and a subroutine, using modules dedicated return stack. We can call can_make_word() using name or using Gosub. Gosub can call subs as labels, and expect Return to return from sub. These routines are more lighter than subs, because they run as code is in module, and any new variable stay until module exit. So we never make local variables or if we want locals we have to use Fopr This { }, the block for temporary definitions.



```M2000 Interpreter
Module ABC {
      can_make_word("A")
      can_make_word("BaRk")
      can_make_word("BOOK")
      can_make_word("TREAT")
      can_make_word("CommoN")
      can_make_word("SQUAD")
      Gosub can_make_word("CONFUSE")  ' we can use Gosub before
      Sub can_make_word(c$)
            local b$=ucase$(c$)
            local i, a$="BOXKDQCPNAGTRETGQDFSJWHUVIANOBERFSLYPCZM", m
            for i=1 to len(b$)
                  m=Instr(a$,mid$(b$, i, 1))
                  If m=0 Then Exit for
                  Insert binary.or(m-1, 1),2 a$=""   ' delete 2 chars
            Next i
            Print c$, m<>0
      End Sub
}
ABC
```


Output:

```txt
A          True
BaRk       True
BOOK      False
TREAT      True
CommoN    False
SQUAD      True
CONFUSE    True
```

## Mathematica / Wolfram Language

```Mathematica
blocks=Partition[Characters[ToLowerCase["BOXKDQCPNAGTRETGQDFSJWHUVIANOBERFSLYPCZM"]],2];
ClearAll[DoStep,ABCBlockQ]
DoStep[chars_List,blcks_List,chosen_List]:=Module[{opts},
 If[chars=!={},
  opts=Select[blcks,MemberQ[#,First[chars]]&];
  {Rest[chars],DeleteCases[blcks,#,1,1],Append[chosen,#]}&/@opts
 ,
  {{chars,blcks,chosen}}
 ]
]
DoStep[opts_List]:=Flatten[DoStep@@@opts,1]
ABCBlockQ[str_String]:=(FixedPoint[DoStep,{{Characters[ToLowerCase[str]],blocks,{}}}]=!={})
```

Output:

```txt
ABCBlockQ["A"]
ABCBlockQ["BARK"]
ABCBlockQ["BOOK"]
ABCBlockQ["TREAT"]
ABCBlockQ["COMMON"]
ABCBlockQ["SQUAD"]
ABCBlockQ["CONFUSE"]
True
True
False
True
False
True
True
```


## MATLAB

```MATLAB
function testABC
    combos = ['BO' ; 'XK' ; 'DQ' ; 'CP' ; 'NA' ; 'GT' ; 'RE' ; 'TG' ; 'QD' ; ...
        'FS' ; 'JW' ; 'HU' ; 'VI' ; 'AN' ; 'OB' ; 'ER' ; 'FS' ; 'LY' ; ...
        'PC' ; 'ZM'];
    words = {'A' 'BARK' 'BOOK' 'TREAT' 'COMMON' 'SQUAD' 'CONFUSE'};
    for k = 1:length(words)
        possible = canMakeWord(words{k}, combos);
        fprintf('Can%s make word %s.\n', char(~possible.*'NOT'), words{k})
    end
end

function isPossible = canMakeWord(word, combos)
    word = lower(word);
    combos = lower(combos);
    isPossible = true;
    k = 1;
    while isPossible && k <= length(word)
        [r, c] = find(combos == word(k), 1);
        if ~isempty(r)
            combos(r, :) = '';
        else
            isPossible = false;
        end
        k = k+1;
    end
end
```

Output:

```txt
Can make word A.
Can make word BARK.
CanNOT make word BOOK.
Can make word TREAT.
CanNOT make word COMMON.
Can make word SQUAD.
Can make word CONFUSE.
```



## MAXScript

###  Recursive


Recursively checks if the word is possible if a block is removed from the array.


```MAXScript

-- This is the blocks array
global GlobalBlocks = #("BO","XK","DQ","CP","NA", \
			"GT","RE","TG","QD","FS", \
			"JW","HU","VI","AN","OB", \
			"ER","FS","LY","PC","ZM")

-- This function returns true if "_str" is part of "_word", false otherwise
fn occurs _str _word =
(
	if _str != undefined and _word != undefined then
	(
	matchpattern _word pattern:("*"+_str+"*")
	) else return false
)

-- This is the main function
fn isWordPossible word blocks: = -- blocks is a keyword argument
(
	word = toupper word -- convert the string to upper case, to make it case insensitive
	if blocks == unsupplied do blocks = GlobalBlocks
	-- if blocks (keyword argument) is unsupplied, use the global blocks array (this is for recursion)

	blocks = deepcopy blocks

	local pos = 1 -- start at the beginning of the word
	local solvedLetters = #() -- this array stores the indices of solved letters

	while pos <= word.count do -- loop through every character in the word
	(
		local possibleBlocks = #() -- this array stores the blocks which can be used to make that letter
		for b = 1 to Blocks.count do -- this loop finds all the possible blocks that can be used to make that letter
		(
			if occurs word[pos] blocks[b] do
			(
				appendifunique possibleBlocks b
			)
		)
		if possibleBlocks.count > 0 then -- if it found any blocks
		(
			if possibleBlocks.count == 1 then -- if it found one block, then continue
			(
				appendifunique solvedLetters pos
				deleteitem blocks possibleblocks[1]
				pos += 1
			)
			else -- if it found more than one
			(
				for b = 1 to possibleBlocks.count do -- loop through every possible block
				(
					local possibleBlock = blocks[possibleBlocks[b]]
					local blockFirstLetter = possibleBlock[1]
					local blockSecondLetter = possibleBlock[2]
					local matchingLetter = if blockFirstLetter == word[pos] then 1 else 2
					-- ^ this is the index of the matching letter on the block

					local notMatchingIndex = if matchingLetter == 1 then 2 else 1
					local notMatchingLetter = possibleBlock[notMatchingIndex]
					-- ^ this is the other letter on the block

					if occurs notMatchingLetter (substring word (pos+1) -1) then
					( -- if the other letter occurs in the rest of the word
						local removedBlocks = deepcopy blocks -- copy the current blocks array
						deleteitem removedBlocks possibleBlocks[b] -- remove the item from the copied array

						-- recursively check if the word is possible if that block is taken away from the array:
						if (isWordPossible (substring word (pos+1) -1) blocks:removedBlocks) then
						( -- if it is, then remove the block and move to next character
							appendifunique solvedLetters pos
							deleteitem blocks possibleblocks[1]
							pos += 1
							exit
						)
						else
						( -- if it isn't and it looped through every possible block, then the word is not possible
							if b == possibleBlocks.count do return false
						)
					)
					else
					( -- if the other letter on this block doesn't occur in the rest of the word, then the letter is solved, continue
							appendifunique solvedLetters pos
							deleteitem blocks possibleblocks[b]
							pos += 1
							exit
					)
				)
			)
		) else return false -- if it didn't find any blocks, then return false
	)

	makeuniquearray solvedLetters -- make sure there are no duplicates in the solved array
	if solvedLetters.count != word.count then return false -- if number of solved letters is not equal to word length
		else
			( -- this checks if all the solved letters are the same as the word
				check = ""
				for bit in solvedLetters do append check word[bit]
				if check == word then return true else return false
			)
)

```

Output:

```MAXScript
iswordpossible "a"
true
iswordpossible "bark"
true
iswordpossible "book"
false
iswordpossible "treat"
true
iswordpossible "common"
false
iswordpossible "squad"
true
iswordpossible "confuse"
true
```



### Non-recursive

```MAXScript

fn isWordPossible2 word =
(
	Blocks = #("BO","XK","DQ","CP","NA", \
			   "GT","RE","TG","QD","FS", \
			   "JW","HU","VI","AN","OB", \
			   "ER","FS","LY","PC","ZM")
        word = toupper word
	local pos = 1
	local solvedLetters = #()
	while pos <= word.count do
	(
		for i = 1 to blocks.count do
		(
			if (matchpattern blocks[i] pattern:("*"+word[pos]+"*")) then
				(
					deleteitem blocks i
					appendifunique solvedLetters pos
					pos +=1
					exit
				)
			else if i == blocks.count do return false
		)
	)
	if solvedLetters.count == word.count then
	(
		local check = ""
		for bit in solvedLetters do append check word[bit]
		if check == word then return true else return false
	) else return false
)

```


Both versions are good for this example, but the non-recursive version won't work if the blocks are more random, because it just takes the first found block, and the recursive version decides which one to use.
For example, if blocks are: #("RT","WA","WO","TB","RE")
Then:


```MAXScript

iswordpossible "water"
true
iswordpossible2 "water"
false

```


Non-recursive version quickly decides that it's not possible, even though it clearly is.


## Mercury


```Mercury
:- module abc.
:- interface.
:- import_module io.
:- pred main(io::di, io::uo) is det.
:- implementation.
:- import_module list, string, char.

:- type block == {char, char}.

:- pred take(char, list(block), list(block)).
:- mode take(in, in, out) is nondet.
take(C, !Blocks) :-
    list.delete(!.Blocks, {A, B}, !:Blocks),
    ( A = C ; B = C ).

:- pred can_make_word(list(char)::in, list(block)::in) is semidet.
can_make_word([], _).
can_make_word([C|Cs], !.Blocks) :-
    take(C, !Blocks),
    can_make_word(Cs, !.Blocks).

main(!IO) :-
    Blocks = [
        {'B', 'O'}, {'X', 'K'}, {'D', 'Q'}, {'C', 'P'}, {'N', 'A'},
        {'G', 'T'}, {'R', 'E'}, {'T', 'G'}, {'Q', 'D'}, {'F', 'S'},
        {'J', 'W'}, {'H', 'U'}, {'V', 'I'}, {'A', 'N'}, {'O', 'B'},
        {'E', 'R'}, {'F', 'S'}, {'L', 'Y'}, {'P', 'C'}, {'Z', 'M'}
    ],
    Words = ["A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"],
    foldl((pred(W::in, !.IO::di, !:IO::uo) is det :-
            P = can_make_word(to_char_list(W), Blocks),
            io.format("can_make_word(""%s"") :- %s.\n",
                [s(W), s(if P then "true" else "fail")], !IO)),
        Words, !IO).
```


Note that 'P', in the foldl near the end, is not a boolean variable, but a zero-arity currying of can_make_word (i.e., it's a 'lambda' that takes no arguments and then calls can_make_word with all of the already-supplied arguments).


## MiniScript


```MiniScript
allBlocks = ["BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS", "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM"]

swap = function(list, index1, index2)
    tmp = list[index1]
    list[index1] = list[index2]
    list[index2] = tmp
end function

canMakeWord = function(str, blocks)
    if str == "" then return true
    c = str[0].upper
    for i in range(0, blocks.len - 1)
        bl = blocks[i]
        if c != bl[0] and c != bl[1] then continue
        swap blocks, 0, i
        if canMakeWord(str[1:], blocks[1:]) then return true
        swap blocks, 0, i
    end for
    return false
end function

for val in ["", "A", "BARK", "book", "Treat", "COMMON", "sQuAD", "CONFUSE"]
    out = """"""
    if val.len != 0 then out = val
    print out + ": " + canMakeWord(val, allBlocks)
end for

```



## Nim


```nim
from strutils import toUpperAscii, contains, format
from sequtils import delete

proc makeWord(s: string): bool =
  var
    abcs = @["BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS",
             "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM"]

  if s.len > abcs.len:
    return false

  for ch in s.toUpperAscii.items:
    block outer:
      for i, abc in abcs.pairs:
        if abc.contains(ch):
          abcs.delete(i)
          break outer
      return false
  return true

let words =  @["A", "bArK", "BOOK", "treat", "common", "sQuAd", "CONFUSE"]
for word in words:
  echo format("""Can the blocks make the word "$1"? $2 """, word,
              if makeWord(word): "yes" else: "no")
```

Output:

```txt
Can the blocks make the word "A"? yes
Can the blocks make the word "bArK"? yes
Can the blocks make the word "BOOK"? no
Can the blocks make the word "treat"? yes
Can the blocks make the word "common"? no
Can the blocks make the word "sQuAd"? yes
Can the blocks make the word "CONFUSE"? yes
```


## Oberon 2

Works with oo2c Version 2

```oberon2

MODULE ABCBlocks;
IMPORT
  Object,
  Out;

VAR
  blocks: ARRAY 20 OF STRING;

    PROCEDURE CanMakeWord(w: STRING): BOOLEAN;
    VAR
      used: ARRAY 20 OF LONGINT;
      wChars: Object.CharsLatin1;
      i,j: LONGINT;

      PROCEDURE IsUsed(i: LONGINT): BOOLEAN;
      VAR
        b: LONGINT;
      BEGIN
        b := 0;
        WHILE (b < LEN(used) - 1) & (used[b] # -1) DO
          IF used[b] = i THEN RETURN TRUE END;
          INC(b)
        END;
        RETURN FALSE
      END IsUsed;

      PROCEDURE GetBlockFor(blocks: ARRAY OF STRING; c: CHAR): LONGINT;
      VAR
        i: LONGINT;
      BEGIN
        i := 0;
        WHILE (i < LEN(blocks)) DO
          IF (blocks[i].IndexOf(c,0) >= 0) & (~IsUsed(i)) THEN RETURN i END;
          INC(i)
        END;

        RETURN -1;
      END GetBlockFor;

    BEGIN
      FOR i := 0 TO LEN(used) - 1 DO used[i] := -1 END;
      wChars := w(Object.String8).CharsLatin1();

      i := 0;
      WHILE (i < LEN(wChars^) - 1) DO
        j := GetBlockFor(blocks,CAP(wChars[i]));
        IF j < 0 THEN RETURN FALSE END;
        used[i] := j;
        INC(i)
      END;
      RETURN TRUE
    END CanMakeWord;

BEGIN
  blocks[0] := "BO";
  blocks[1] := "XK";
  blocks[2] := "DQ";
  blocks[3] := "CP";
  blocks[4] := "NA";
  blocks[5] := "GT";
  blocks[6] := "RE";
  blocks[7] := "TG";
  blocks[8] := "QD";
  blocks[9] := "FS";
  blocks[10] := "JW";
  blocks[11] := "HU";
  blocks[12] := "VI";
  blocks[13] := "AN";
  blocks[14] := "OB";
  blocks[15] := "ER";
  blocks[16] := "FS";
  blocks[17] := "LY";
  blocks[18] := "PC";
  blocks[19] := "ZM";

  Out.String("A: ");Out.Bool(CanMakeWord("A"));Out.Ln;
  Out.String("BARK: ");Out.Bool(CanMakeWord("BARK"));Out.Ln;
  Out.String("BOOK: ");Out.Bool(CanMakeWord("BOOK"));Out.Ln;
  Out.String("TREAT: ");Out.Bool(CanMakeWord("TREAT"));Out.Ln;
  Out.String("COMMON: ");Out.Bool(CanMakeWord("COMMON"));Out.Ln;
  Out.String("SQAD: ");Out.Bool(CanMakeWord("SQUAD"));Out.Ln;
  Out.String("confuse: ");Out.Bool(CanMakeWord("confuse"));Out.Ln;
END ABCBlocks.

```

Output:

```txt

A: TRUE
BARK: TRUE
BOOK: FALSE
TREAT: TRUE
COMMON: FALSE
SQAD: TRUE
confuse: TRUE

```



## Objeck

Translated from Java

```objeck
class Abc {
  function : Main(args : String[]) ~ Nil {
    blocks := ["BO", "XK", "DQ", "CP", "NA",
      "GT", "RE", "TG", "QD", "FS",
      "JW", "HU", "VI", "AN", "OB",
      "ER", "FS", "LY", "PC", "ZM"];

    IO.Console->Print("\"\": ")->PrintLine(CanMakeWord("", blocks));
    IO.Console->Print("A: ")->PrintLine(CanMakeWord("A", blocks));
    IO.Console->Print("BARK: ")->PrintLine(CanMakeWord("BARK", blocks));
    IO.Console->Print("book: ")->PrintLine(CanMakeWord("book", blocks));
    IO.Console->Print("treat: ")->PrintLine(CanMakeWord("treat", blocks));
    IO.Console->Print("COMMON: ")->PrintLine(CanMakeWord("COMMON", blocks));
    IO.Console->Print("SQuAd: ")->PrintLine(CanMakeWord("SQuAd", blocks));
    IO.Console->Print("CONFUSE: ")->PrintLine(CanMakeWord("CONFUSE", blocks));
  }

  function : CanMakeWord(word : String, blocks : String[]) ~ Bool {
    if(word->Size() = 0) {
          return true;
    };

      c := word->Get(0)->ToUpper();
      for(i := 0; i < blocks->Size(); i++;) {
      b := blocks[i];
      if(<>(b->Get(0)->ToUpper() <> c & b->Get(1)->ToUpper() <> c)) {
        Swap(0, i, blocks);
        new_word := word->SubString(1, word->Size() - 1);
        new_blocks := String->New[blocks->Size() - 1];
        Runtime->Copy(new_blocks, 0, blocks, 1, blocks->Size() - 1);
        if(CanMakeWord(new_word, new_blocks)) {
          return true;
        };
        Swap(0, i, blocks);
      };
    };

    return false;
  }

  function : native : Swap(i : Int, j : Int, arr : String[]) ~ Nil {
    tmp := arr[i];
    arr[i] := arr[j];
    arr[j] := tmp;
  }
}
```


```txt

"": true
A: true
BARK: true
book: false
treat: true
COMMON: false
SQuAd: true
CONFUSE: true

```



## OCaml


```ocaml
let blocks = [
  ('B', 'O');  ('X', 'K');  ('D', 'Q');  ('C', 'P');
  ('N', 'A');  ('G', 'T');  ('R', 'E');  ('T', 'G');
  ('Q', 'D');  ('F', 'S');  ('J', 'W');  ('H', 'U');
  ('V', 'I');  ('A', 'N');  ('O', 'B');  ('E', 'R');
  ('F', 'S');  ('L', 'Y');  ('P', 'C');  ('Z', 'M');
]

let find_letter blocks c =
  let found, remaining =
    List.partition (fun (c1, c2) -> c1 = c || c2 = c) blocks
  in
  match found with
  | _ :: res -> Some (res @ remaining)
  | _ -> None

let can_make_word w =
  let n = String.length w in
  let rec aux i _blocks =
    if i >= n then true else
      match find_letter _blocks w.[i] with
      | None -> false
      | Some rem_blocks ->
          aux (succ i) rem_blocks
  in
  aux 0 blocks

let test label f (word, should) =
  Printf.printf "- %s %S = %B  (should: %B)\n" label word (f word) should

let () =
  List.iter (test "can make word" can_make_word) [
    "A", true;
    "BARK", true;
    "BOOK", false;
    "TREAT", true;
    "COMMON", false;
    "SQUAD", true;
    "CONFUSE", true;
  ]
```


Output:

```txt

 $ ocaml canmakeword.ml
 - can make word "A" = true  (should: true)
 - can make word "BARK" = true  (should: true)
 - can make word "BOOK" = false  (should: false)
 - can make word "TREAT" = true  (should: true)
 - can make word "COMMON" = false  (should: false)
 - can make word "SQUAD" = true  (should: true)
 - can make word "CONFUSE" = true  (should: true)

```



## Oforth



```Oforth
import: mapping

["BO","XK","DQ","CP","NA","GT","RE","TG","QD","FS","JW","HU","VI","AN","OB","ER","FS","LY","PC","ZM"]
const: ABCBlocks

: canMakeWord(w, blocks)
| i |
   w empty? ifTrue: [ true return ]
   blocks size loop: i [
      w first >upper  blocks at(i) include? ifFalse: [ continue ]
      canMakeWord( w right( w size 1- ), blocks del(i, i) ) ifTrue: [ true return ]
      ]
   false
;
```


Output:

```txt

["A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"] map(#[ ABCBlocks canMakeWord]) .
[1, 1, 0, 1, 0, 1, 1]

```



## OpenEdge/Progress


```Progress (Openedge ABL)
FUNCTION canMakeWord RETURNS LOGICAL (INPUT pWord AS CHARACTER) FORWARD.

/* List of blocks */
DEFINE TEMP-TABLE ttBlocks NO-UNDO
    FIELD ttFaces AS CHARACTER FORMAT "x(1)" EXTENT 2
    FIELD ttUsed AS LOGICAL.

/* Fill in list of blocks */
RUN AddBlock("BO").
RUN AddBlock("XK").
RUN AddBlock("DQ").
RUN AddBlock("CP").
RUN AddBlock("NA").
RUN AddBlock("GT").
RUN AddBlock("Re").
RUN AddBlock("TG").
RUN AddBlock("QD").
RUN AddBlock("FS").
RUN AddBlock("JW").
RUN AddBlock("HU").
RUN AddBlock("VI").
RUN AddBlock("AN").
RUN AddBlock("OB").
RUN AddBlock("ER").
RUN AddBlock("FS").
RUN AddBlock("LY").
RUN AddBlock("PC").
RUN AddBlock("ZM").

DEFINE VARIABLE chWords AS CHARACTER EXTENT 7 NO-UNDO.
ASSIGN  chWords[1] = "A"
        chWords[2] = "BARK"
        chWords[3] = "BOOK"
        chWords[4] = "TREAT"
        chWords[5] = "COMMON"
        chWords[6] = "SQUAD"
        chWords[7] = "CONFUSE".

DEFINE FRAME frmResult
    WITH NO-LABELS 7 DOWN USE-TEXT.

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DO i = 1 TO 7:
    DISPLAY chWords[i] + " = " + STRING(canMakeWord(chWords[i])) FORMAT "x(25)" WITH FRAME frmResult.
    DOWN WITH FRAME frmResult.
END.


PROCEDURE AddBlock:
    DEFINE INPUT PARAMETER i-chBlockvalue AS CHARACTER NO-UNDO.

    IF (LENGTH(i-chBlockValue) <> 2)
        THEN RETURN ERROR.

    CREATE ttBlocks.
    ASSIGN  ttBlocks.ttFaces[1] = SUBSTRING(i-chBlockValue, 1, 1)
            ttBlocks.ttFaces[2] = SUBSTRING(i-chBlockValue, 2, 1).
END PROCEDURE.


FUNCTION blockInList RETURNS LOGICAL (pChar AS CHARACTER):
    /* Find first unused block in list */
    FIND FIRST ttBlocks WHERE (ttBlocks.ttFaces[1] = pChar
                               OR ttBlocks.ttFaces[2] = pChar)
                          AND NOT ttBlocks.ttUsed NO-ERROR.
    IF (AVAILABLE ttBlocks) THEN DO:
        /* found it! set to used and return true */
        ASSIGN ttBlocks.ttUsed = TRUE.
        RETURN TRUE.
    END.
    ELSE RETURN FALSE.
END FUNCTION.


FUNCTION canMakeWord RETURNS LOGICAL (INPUT pWord AS CHARACTER):
    DEFINE VARIABLE i AS INTEGER NO-UNDO.
    DEFINE VARIABLE chChar AS CHARACTER NO-UNDO.

    /* Word has to be valid */
    IF (LENGTH(pWord) = 0)
        THEN RETURN FALSE.

    DO i = 1 TO LENGTH(pWord):
        /* get the char */
        chChar = SUBSTRING(pWord, i, 1).

        /* Check to see if this is a letter? */
        IF ((ASC(chChar) < 65) OR (ASC(chChar) > 90) AND
            (ASC(chChar) < 97) OR (ASC(chChar) > 122))
            THEN RETURN FALSE.

        /* Is block is list (and unused) */
        IF NOT blockInList(chChar)
            THEN RETURN FALSE.
    END.

    /* Reset all blocks */
    FOR EACH ttBlocks:
        ASSIGN ttUsed = FALSE.
    END.
    RETURN TRUE.
END FUNCTION.

```


Output:

```txt

A = yes
BARK = yes
BOOK = no
TREAT = yes
COMMON = no
SQUAD = yes
CONFUSE = yes

```



## PARI/GP


```parigp
BLOCKS = "BOXKDQCPNAGTRETGQDFSJWHUVIANOBERFSLYPCZM";
WORDS  = ["A","Bark","BOOK","Treat","COMMON","SQUAD","conFUSE"];

can_make_word(w) = check(Vecsmall(BLOCKS), Vecsmall(w))

check(B,W,l=1,n=1) =
{
  if (l > #W, return(1), n > #B, return(0));

  forstep (i = 1, #B-2, 2,
    if (B[i] != bitand(W[l],223) && B[i+1] != bitand(W[l],223), next());
    B[i] = B[i+1] = 0;
    if (check(B, W, l+1, n+2), return(1))
  );
  0
}

for (i = 1, #WORDS, printf("%s\t%d\n", WORDS[i], can_make_word(WORDS[i])));
```


Output:
```txt
A	1
Bark	1
BOOK	0
Treat	1
COMMON	0
SQUAD	1
conFUSE	1
```




## Pascal


Works with Free Pascal|2.6.2}}


```Pascal

#!/usr/bin/instantfpc
//program ABCProblem;

{$mode objfpc}{$H+}

uses SysUtils, Classes;

const
  // every couple of chars is a block
  // remove one by replacing its 2 chars by 2 spaces
  Blocks =  'BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM';
  BlockSize = 3;

function can_make_word(Str: String): boolean;
var
  wkBlocks: string = Blocks;
  c: Char;
  iPos : Integer;
begin
  // all chars to uppercase
  Str := UpperCase(Str);
  Result := Str <> '';
  if Result then
  begin
    for c in Str do
    begin
      iPos := Pos(c, wkBlocks);
      if (iPos > 0) then
      begin
        // Char found
        wkBlocks[iPos] := ' ';
        // Remove the other face
        if (iPos mod BlockSize = 1) then
          wkBlocks[iPos + 1] := ' '
        else
          wkBlocks[iPos - 1] := ' ';
      end
      else
      begin
        //  missed
        Result := False;
        break;
      end;
    end;
  end;
  // Debug...
  //WriteLn(Blocks);
  //WriteLn(wkBlocks);
End;

procedure TestABCProblem(Str: String);
const
  boolStr : array[boolean] of String = ('False', 'True');
begin
  WriteLn(Format('>>> can_make_word("%s")%s%s', [Str, LineEnding, boolStr[can_make_word(Str)]]));
End;

begin
  TestABCProblem('A');
  TestABCProblem('BARK');
  TestABCProblem('BOOK');
  TestABCProblem('TREAT');
  TestABCProblem('COMMON');
  TestABCProblem('SQUAD');
  TestABCProblem('CONFUSE');
END.
```


Output:


```txt

./ABCProblem.pas
>>> can_make_word("A")
True
>>> can_make_word("BARK")
True
>>> can_make_word("BOOK")
False
>>> can_make_word("TREAT")
True
>>> can_make_word("COMMON")
False
>>> can_make_word("SQUAD")
True
>>> can_make_word("CONFUSE")
True

```



## Perl

Recursive solution that can handle characters appearing on different blocks:

```perl
#!/usr/bin/perl
use warnings;
use strict;


sub can_make_word {
    my ($word, @blocks) = @_;
    $_ = uc join q(), sort split // for @blocks;
    my %blocks;
    $blocks{$_}++ for @blocks;
    return _can_make_word(uc $word, %blocks)
}


sub _can_make_word {
    my ($word, %blocks) = @_;
    my $char = substr $word, 0, 1, q();

    my @candidates = grep 0 <= index($_, $char), keys %blocks;
    for my $candidate (@candidates) {
        next if $blocks{$candidate} <= 0;
        local $blocks{$candidate} = $blocks{$candidate} - 1;
        return 1 if q() eq $word or _can_make_word($word, %blocks);
    }
    return
}
```

<p>Testing:

```perl>use Test::More tests =
 8;

my @blocks1 = qw(BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM);
is(can_make_word("A",       @blocks1), 1);
is(can_make_word("BARK",    @blocks1), 1);
is(can_make_word("BOOK",    @blocks1), undef);
is(can_make_word("TREAT",   @blocks1), 1);
is(can_make_word("COMMON",  @blocks1), undef);
is(can_make_word("SQUAD",   @blocks1), 1);
is(can_make_word("CONFUSE", @blocks1), 1);

my @blocks2 = qw(US TZ AO QA);
is(can_make_word('auto', @blocks2), 1);

```



## Perl 6

Works with rakudo|6.0.c}}
Blocks are stored as precompiled regexes. We do an initial pass on the blockset to include in the list only those regexes that match somewhere in the current word.  Conveniently, regexes scan the word for us.

```perl6
multi can-spell-word(Str $word, @blocks) {
    my @regex = @blocks.map({ my @c = .comb; rx/<@c>/ }).grep: { .ACCEPTS($word.uc) }
    can-spell-word $word.uc.comb.list, @regex;
}

multi can-spell-word([$head,*@tail], @regex) {
    for @regex -> $re {
        if $head ~~ $re {
            return True unless @tail;
            return False if @regex == 1;
            return True if can-spell-word @tail, list @regex.grep: * !=== $re;
        }
    }
    False;
}

my @b = <BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM>;

for <A BaRK BOoK tREaT COmMOn SqUAD CoNfuSE> {
    say "$_     &can-spell-word($_, @b)";
}
```

Output:

```txt
A	True
BaRK	True
BOoK	False
tREaT	True
COmMOn	False
SqUAD	True
CoNfuSE	True
```



## Phix



```Phix

-- Here is my recursive solution which also solves the extra problems on the discussion page:

sequence blocks = {"BO","XK","DQ","CP","NA","GT","RE","TG","QD","FS",
                   "JW","HU","VI","AN","OB","ER","FS","LY","PC","ZM"}

sequence words = {"","A","BarK","BOOK","TrEaT","COMMON","SQUAD","CONFUSE"}

--sequence blocks = {"US","TZ","AO","QA"}
--sequence words = {"AuTO"}

--sequence blocks = {"AB","AB","AC","AC"}
--sequence words = {"abba"}

sequence used = repeat(0,length(blocks))

function ABC_Solve(sequence word, integer idx)
integer ch
integer res = 0
    if idx>length(word) then
        res = 1
    else
        ch = word[idx]
        for k=1 to length(blocks) do
            if used[k]=0
            and find(ch,blocks[k]) then
                used[k] = 1
                res = ABC_Solve(word,idx+1)
                used[k] = 0
                if res then exit end if
            end if
        end for
    end if
    return res
end function

constant TF = {"False","True"}
procedure ABC_Problem()
    for i=1 to length(words) do
        printf(1,"%s: %s\n",{words[i],TF[ABC_Solve(upper(words[i]),1)+1]})
    end for
    if getc(0) then end if
end procedure

    ABC_Problem()


```

Output:

```txt

: True
A: True
BarK: True
BOOK: False
TrEaT: True
COMMON: False
SQUAD: True
CONFUSE: True

```



## PHP



```PHP

<?php
$words = array("A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "Confuse");

function canMakeWord($word) {
    $word = strtoupper($word);
    $blocks = array(
            "BO", "XK", "DQ", "CP", "NA",
            "GT", "RE", "TG", "QD", "FS",
            "JW", "HU", "VI", "AN", "OB",
            "ER", "FS", "LY", "PC", "ZM",
    );

    foreach (str_split($word) as $char) {
        foreach ($blocks as $k => $block) {
            if (strpos($block, $char) !== FALSE) {
                unset($blocks[$k]);
                continue(2);
            }
        }
        return false;
    }
    return true;
}

foreach ($words as $word) {
    echo $word.': ';
    echo canMakeWord($word) ? "True" : "False";
    echo "\r\n";
}
```

Output:

```txt

A: True
BARK: True
BOOK: False
TREAT: True
COMMON: False
SQUAD: True
Confuse: True

```



## PicoLisp

Mapping and recursion.

```picolisp
(setq *Blocks
   '((B O) (X K) (D Q) (C P) (N A) (G T) (R E)
   (T G) (Q D) (F S) (J W) (H U) (V I) (A N)
   (O B) (E R) (F S) (L Y) (P C) (Z M) ) )
(setq *Words '("" "1" "A" "BARK" "BOOK" "TREAT"
               "Bbb" "COMMON" "SQUAD" "Confuse"
               "abba" "ANBOCPDQERSFTGUVWXLZ") )

(de abc (W B)
   (let Myblocks (copy B)
      (fully
         '((C)
            (when (seek '((Lst) (member C (car Lst))) Myblocks)
               (set @)
               T ) )
      (chop (uppc W)) ) ) )

(de abcR (W B)
   (nond
      ((car W) T)
      ((car B) NIL)
      (NIL
         (setq W (chop W))
         (let? I
            (find
               '((Lst) (member (uppc (car W)) Lst))
               B )
            (abcR (cdr W) (delete I B)) ) ) ) )

(for Word *Words
   (println Word (abc Word *Blocks) (abcR Word *Blocks)) )

(bye)
```



## PL/I


### version 1


```pli
ABC: procedure options (main);   /* 12 January 2014 */

   declare word character (20) varying, blocks character (200) varying initial
      ('((B O) (X K) (D Q) (C P) (N A) (G T) (R E) (T G) (Q D) (F S)
        (J W) (H U) (V I) (A N) (O B) (E R) (F S) (L Y) (P C) (Z M))');
   declare tblocks character (200) varying;
   declare (true value ('1'b), false value ('0'b), flag) bit (1);
   declare ch character (1);
   declare (i, k) fixed binary;

   do word = 'A', 'BARK', 'BOOK', 'TREAT', 'COMMON', 'SQuAd', 'CONFUSE';
      flag = true;
      tblocks = blocks;
      do i = 1 to length(word)
      while(flag = true);
         ch = substr(word, i, 1);
         k = index(tblocks, uppercase(ch));
         if k = 0 then
            flag = false;
         else /* Found a block with the letter on it. */
            substr(tblocks, k-1, 4) = '   '; /* Delete the block. */
      end;
      if flag then put skip list (word, 'true'); else put skip list (word, 'false');
   end;

end ABC;
```


```txt

A                       true
BARK                    true
BOOK                    false
TREAT                   true
COMMON                  false
SQuAd                   true
CONFUSE                 true

```



### version 2


```pli
*process source attributes xref or(!) options nest;
 abc: Proc Options(main);
 /* REXX --------------------------------------------------------------
 * 10.01.2013 Walter Pachl  counts the number of possible ways
 * translated from Rexx version 2
 *-------------------------------------------------------------------*/

 Dcl (ADDR,HBOUND,INDEX,LEFT,LENGTH,MAX,SUBSTR,TRANSLATE) builtin;
 Dcl sysprint Print;
 Dcl (i,j,k,m,mm,wi,wj,wlen,ways,lw) Bin Fixed(15);
 Dcl blocks(20) Char(2)
        Init('BO','XK','DQ','CP','NA','GT','RE','TG','QD','FS','JW',
             'HU','VI','AN','OB','ER','FS','LY','PC','ZM');
 Dcl blk Char(2);
 Dcl words(8) Char(7) Var
        Init('$','A','baRk','bOOk','trEat','coMMon','squaD','conFuse');
 Dcl word     Char(7) Var;
 Dcl c Char(1);
 Dcl (show,cannot) Bit(1) Init('0'b);
 Dcl poss(100,0:100) Pic'99';  poss=0;
 Dcl s(20,100) char(100) Var;
 Dcl str Char(100);
 Dcl 1 *(30) Based(addr(str)),
      2 strp Pic'99',
      2 * Char(1);
 Dcl ns(20) Bin Fixed(15) Init((20)0);
 Dcl ol(100) Char(100) Var;
 Dcl os      Char(100) Var;
 wlen=0;
 Dcl lower Char(26) Init('abcdefghijklmnopqrstuvwxyz');
 Dcl upper Char(26) Init('ABCDEFGHIJKLMNOPQRSTUVWXYZ');
 Do wi=1 To hbound(words);
   wlen=max(wlen,length(words(wi)));
   End;
 Do wi=1 To hbound(words);
   word = translate(words(wi),upper,lower);
   ways=0;
   lw=length(word);
   cannot='0'b;
   poss=0;
   ns=0;
   ol='';
 iloop:
   Do i=1 To lw;                       /* loop over the characters   */
     c=substr(word,i,1);               /* the current character      */
     Do j=1 To hbound(blocks);         /* loop over blocks           */
       blk=blocks(j);
       If index(blk,c)>0 Then Do;  /* block can be used in this pos( */
         poss(i,0)+=1;        /* number of possible blocks for pos i */
         poss(i,poss(i,0))=j;
         End;
       End;
     If poss(i,0)=0 Then Do;
       Leave iloop;
       End;
     End;
   If i>lw Then Do;                     /* no prohibitive character  */
     ns=0;
     Do j=1 To poss(1,0);       /* build possible strings for char 1 */
       ns(1)+=1;;
       s(1,j)=poss(1,j);
       End;
     Do m=2 To lw;        /* build possible strings for chars 1 to i */
       mm=m-1;
       Do j=1 To ns(mm);
         Do k=1 To poss(m,0);
           ns(m)+=1;
           s(m,ns(m))=s(mm,j)!!' '!!poss(m,k);
           End;
         End;
       End;
     Do m=1 To ns(lw);
       If valid(s(lw,m)) Then Do;
         ways+=1;
         str=s(lw,m);
         Do k=1 To lw;
           ol(ways)=ol(ways)!!blocks(strp(k))!!' ';
           End;
         End;
       End;
     End;
 /*--------------------------------------------------------------------
 * now show the result
 *-------------------------------------------------------------------*/
   os=left(''''!!word!!'''',wlen+2);
   Select;
     When(ways=0)
       os=os!!' cannot be spelt.';
     When(ways=1)
       os=os!!' can be spelt.';
     Otherwise
       os=os!!' can be spelt in'!!ways!!' ways.';
     End;
   Put Skip List(os);
   If show Then Do;
     Do wj=1 To ways;
       Put Edit('          '!!ol(wj))(Skip,a);
       End;
     End;
   End;
 Return;

 valid: Procedure(list) Returns(bit(1));
 /*--------------------------------------------------------------------
 * Check if the same block is used more than once -> 0
 * Else: the combination is valid
 *-------------------------------------------------------------------*/
 Dcl list Char(*) Var;
 Dcl i Bin Fixed(15);
 Dcl used(20) Bit(1);
 str=list;
 used='0'b;
 Do i=1 To lw;
   If used(strp(i)) Then
     Return('0'b);
   used(strp(i))='1'b;
   End;
 Return('1'b);
 End;

 End;
```

Output:

```txt
'$'       cannot be spelt.
'A'       can be spelt in        2 ways.
'BARK'    can be spelt in        8 ways.
'BOOK'    cannot be spelt.
'TREAT'   can be spelt in        8 ways.
'COMMON'  cannot be spelt.
'SQUAD'   can be spelt in        8 ways.
'CONFUSE' can be spelt in       32 ways.
```



## PowerBASIC

Works with PowerBASIC 6 Console Compiler


```PowerBASIC
#COMPILE EXE
#DIM ALL
'
' A B C p r o b l e m . b a s
'
' by  Geary Chopoff
' for Chopoff Consulting and RosettaCode.org
' on  2014Jul23
'
'2014Jul23
'
'You are given a collection of ABC blocks. Just like the ones you had when you were a kid.
'There are twenty blocks with two letters on each block. You are guaranteed to have a complete
'alphabet amongst all sides of the blocks. The sample blocks are:
'((B O) (X K) (D Q) (C P) (N A) (G T) (R E) (T G) (Q D) (F S) (J W) (H U) (V I) (A N) (O B) (E R) (F S) (L Y) (P C) (Z M))
'The goal of this task is to write a function that takes a string and can determine whether
'you can spell the word with the given collection of blocks.
'
'The rules are simple:
'1.Once a letter on a block is used that block cannot be used again
'2.The function should be case-insensitive
'3. Show your output on this page for the following words:
'	A, BARK, BOOK, TREAT, COMMON, SQUAD, CONFUSE
'-----------------------------------------------------------------------------
' G l o b a l   C o n s t a n t s
'
%Verbose = 0                'make this 1 to have a lot of feedback
%MAX_BLOCKS = 20            'total number of blocks
%MAX_SIDES = 2              'total number of sides containing a unique letter per block

%MAX_ASC = 255
%FALSE = 0                  'this is correct because there is ONLY ONE value for FALSE
%TRUE  = (NOT %FALSE)       'this is one of MANY values of TRUE!
$FLAG_TRUE = "1"
$FLAG_FALSE = "0"
'-----------------------------------------------------------------------------
' G l o b a l   V a r i a b l e s
'
GLOBAL blk() AS STRING
'-----------------------------------------------------------------------------
'i n i t B l o c k s
'
' as we will use this array only once we build it each time program is run
'
SUB initBlocks
 LOCAL j AS INTEGER
    j=1
    blk(j)="BO"
    j=j+1
    blk(j)="XK"
    j=j+1
    blk(j)="DQ"
    j=j+1
    blk(j)="CP"
    j=j+1
    blk(j)="NA"
    j=j+1
    blk(j)="GT"
    j=j+1
    blk(j)="RE"
    j=j+1
    blk(j)="TG"
    j=j+1
    blk(j)="QD"
    j=j+1
    blk(j)="FS"
    j=j+1
    blk(j)="JW"
    j=j+1
    blk(j)="HU"
    j=j+1
    blk(j)="VI"
    j=j+1
    blk(j)="AN"
    j=j+1
    blk(j)="OB"
    j=j+1
    blk(j)="ER"
    j=j+1
    blk(j)="FS"
    j=j+1
    blk(j)="LY"
    j=j+1
    blk(j)="PC"
    j=j+1
    blk(j)="ZM"
    IF j <> %MAX_BLOCKS THEN
        STDOUT "initBlocks:Error: j is not same as MAX_BLOCKS!",j,%MAX_BLOCKS
    END IF
END SUB
'-----------------------------------------------------------------------------
' m a k e W o r d
'
FUNCTION makeWord(tryWord AS STRING) AS BYTE
 LOCAL retTF AS BYTE
 LOCAL j AS INTEGER
 LOCAL s AS INTEGER         'which side of block we are looking at
 LOCAL k AS INTEGER
 LOCAL c AS STRING          'character in tryWord we are looking for


    FOR j = 1 TO LEN(tryWord)
        c = UCASE$(MID$(tryWord,j,1))   'character we want to show with block

        retTF = %FALSE                  'we assume this will fail

        FOR k = 1 TO %MAX_BLOCKS
            IF LEN(blk(k)) = %MAX_SIDES THEN
                FOR s = 1 TO %MAX_SIDES
                    IF c = MID$(blk(k),s,1) THEN
                        retTF = %TRUE   'this block has letter we want
                        blk(k) = ""     'remove this block from further consideration
                        EXIT FOR
                    END IF
                NEXT s
            END IF
            IF retTF THEN EXIT FOR      'can go on to next character in word
        NEXT k
        IF ISFALSE retTF THEN EXIT FOR  'if character not found then all is done
    NEXT j

    FUNCTION = retTF
END FUNCTION
'-----------------------------------------------------------------------------
' P B M A I N
'
FUNCTION PBMAIN () AS LONG
 DIM blk(1 TO %MAX_BLOCKS, 1 TO %MAX_SIDES) AS STRING
 LOCAL cmdLine AS STRING

    initBlocks              'setup global array of blocks

    cmdLine=COMMAND$
    IF LEN(cmdLine)= 0 THEN
        STDOUT "Useage for ABCproblem Version 1.00:"
        STDOUT ""
        STDOUT "     >ABCproblem tryThisWord"
        STDOUT ""
        STDOUT "Where tryThisWord is a word you want to see if"+STR$(%MAX_BLOCKS)+" blocks can make."
        STDOUT "If word can be made TRUE is returned."
        STDOUT "Otherwise FALSE is returned."
        EXIT FUNCTION
    END IF

    IF INSTR(TRIM$(cmdLine)," ") = 0 THEN
        IF makeWord(cmdLine) THEN
            STDOUT "TRUE"
        ELSE
            STDOUT "FALSE"
        END IF
    ELSE
        STDOUT "Error:Missing word to try to make with blocks!  <" & cmdLine & ">"
        EXIT FUNCTION
    END IF
END FUNCTION

```

Output:

```txt
$ FALSE
A TRUE
bark TRUE
bOOk FALSE
treAT TRUE
COmmon FALSE
sQuaD TRUE
CONFUSE TRUE
GearyChopoff TRUE

```



## PowerShell


```powershell
<#
.Synopsis
  ABC Problem
.DESCRIPTION
   You are given a collection of ABC blocks. Just like the ones you had when you were a kid.
   There are twenty blocks with two letters on each block. You are guaranteed to have a
   complete alphabet amongst all sides of the blocks
   blocks = "BO","XK","DQ","CP","NA","GT","RE","TG","QD","FS","JW","HU","VI","AN","OB","ER","FS","LY","PC","ZM"
   The goal of this task is to write a function that takes a string and can determine whether
   you can spell the word with the given collection of blocks.

   The rules are simple:
        1.Once a letter on a block is used that block cannot be used again
        2.The function should be case-insensitive
        3. Show your output on this page for the following words:
        >>> can_make_word("A")
        True
        >>> can_make_word("BARK")
        True
        >>> can_make_word("BOOK")
        False
        >>> can_make_word("TREAT")
        True
        >>> can_make_word("COMMON")
        False
        >>> can_make_word("SQUAD")
        True
        >>> can_make_word("CONFUSE")
        True

   Using the examples below  you can either see just the value or
   status and the values using the verbose switch

.EXAMPLE
   test-blocks -testword confuse

.EXAMPLE
   test-blocks -testword confuse -verbose

#>

function test-blocks
{
	[CmdletBinding()]
	#  [OutputType([int])]
	Param
	(
		# word to test against blocks
		[Parameter(Mandatory = $true,
				   ValueFromPipelineByPropertyName = $true)]
		$testword

	)

	$word = $testword

	#define array of blocks
	[System.Collections.ArrayList]$blockarray = "BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS", "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM"

	#send word to chararray
	$chararray = $word.ToCharArray()
	$chars = $chararray

	#get the character count
	$charscount = $chars.count

	#get the initial count of the blocks
	$blockcount = $blockarray.Count

	#find out how many blocks should be left from the difference
	#of the blocks and characters in the word - 1 letter/1 block
	$correctblockcount = $blockcount - $charscount

	#loop through the characters in the word
	foreach ($char in $chars)
	{

		#loop through the blocks
		foreach ($block in $blockarray)
		{

			#check the current character against each letter on the current block
			#and break if found so the array can reload
			if ($char -in $block[0] -or $char -in $block[1])
			{

				write-verbose "match for letter - $char - removing block $block"
				$blockarray.Remove($block)
				break

			}

		}

	}
	#get final count of blocks left in array to determine if the word was
	#correctly made
	$finalblockcount = $blockarray.count
	if ($finalblockcount -ne $correctblockcount)
	{
		write-verbose "$word : $false "
		return $false
	}
	else
	{
		write-verbose "$word : $true "
		return $true
	}

}

#loop all the words and pass them to the function
$wordlist = "a", "bark", "book", "treat", "common", "squad", "confuse"
foreach ($word in $wordlist)
{
	test-blocks -testword $word -Verbose
}
```

Output:

```txt

VERBOSE: match for letter - a - removing block NA
VERBOSE: a : True
True
VERBOSE: match for letter - b - removing block BO
VERBOSE: match for letter - a - removing block NA
VERBOSE: match for letter - r - removing block RE
VERBOSE: match for letter - k - removing block XK
VERBOSE: bark : True
True
VERBOSE: match for letter - b - removing block BO
VERBOSE: match for letter - o - removing block OB
VERBOSE: match for letter - k - removing block XK
VERBOSE: book : False
False
VERBOSE: match for letter - t - removing block GT
VERBOSE: match for letter - r - removing block RE
VERBOSE: match for letter - e - removing block ER
VERBOSE: match for letter - a - removing block NA
VERBOSE: match for letter - t - removing block TG
VERBOSE: treat : True
True
VERBOSE: match for letter - c - removing block CP
VERBOSE: match for letter - o - removing block BO
VERBOSE: match for letter - m - removing block ZM
VERBOSE: match for letter - o - removing block OB
VERBOSE: match for letter - n - removing block NA
VERBOSE: common : False
False
VERBOSE: match for letter - s - removing block FS
VERBOSE: match for letter - q - removing block DQ
VERBOSE: match for letter - u - removing block HU
VERBOSE: match for letter - a - removing block NA
VERBOSE: match for letter - d - removing block QD
VERBOSE: squad : True
True
VERBOSE: match for letter - c - removing block CP
VERBOSE: match for letter - o - removing block BO
VERBOSE: match for letter - n - removing block NA
VERBOSE: match for letter - f - removing block FS
VERBOSE: match for letter - u - removing block HU
VERBOSE: match for letter - s - removing block FS
VERBOSE: match for letter - e - removing block RE
VERBOSE: confuse : True
True

or without verbose

True
True
False
True
False
True
True


```



## Prolog



###  Traditional


Works with SWI-Prolog 6.5.3


```Prolog
abc_problem :-
	maplist(abc_problem, ['', 'A', bark, bOOk, treAT, 'COmmon', sQuaD, 'CONFUSE']).


abc_problem(Word) :-
	L = [[b,o],[x,k],[d,q],[c,p],[n,a],[g,t],[r,e],[t,g],[q,d],[f,s],
	     [j,w],[h,u],[v,i],[a,n],[o,b],[e,r],[f,s],[l,y],[p,c],[z,m]],

	(   abc_problem(L, Word)
	->  format('~w OK~n', [Word])
	;   format('~w KO~n', [Word])).

abc_problem(L, Word) :-
	atom_chars(Word, C_Words),
	maplist(downcase_atom, C_Words, D_Words),
	can_makeword(L, D_Words).

can_makeword(_L, []).

can_makeword(L, [H | T]) :-
	(   select([H, _], L, L1); select([_, H], L, L1)),
	can_makeword(L1, T).

```

Output:

```txt
 ?- abc_problem.
 OK
A OK
bark OK
bOOk KO
treAT OK
COmmon KO
sQuaD OK
CONFUSE OK
true.

```


###  Constraint Handling Rules


An approach using [CHR https://dtai.cs.kuleuven.be/CHR/] via SWI-Prolog's [library(chr) http://www.swi-prolog.org/pldoc/man?section=chr] and a module I'm working on for composing predicates [https://github.com/aBathologist/protelog/blob/master/composer.pl composer]:

Works with SWI Prolog 7}}


```Prolog
:- use_module([ library(chr),
                abathslib(protelog/composer) ]).

:- chr_constraint blocks, block/1, letter/1, word_built.

can_build_word(Word) :-
    maplist(block, [(b,o),(x,k),(d,q),(c,p),(n,a),(g,t),(r,e),(t,g),(q,d),(f,s),
                    (j,w),(h,u),(v,i),(a,n),(o,b),(e,r),(f,s),(l,y),(p,c),(z,m)]),
    maplist(letter) <- string_chars <- string_lower(Word),     %% using the `composer` module
    word_built,
    !.

'take letter and block'  @ letter(L), block((A,B)) <=> L == A ; L == B | true.
'fail if letters remain' @ word_built, letter(_)   <=> false.

%% These rules, removing remaining constraints from the store, are just cosmetic:
'clean up blocks' @ word_built \ block(_) <=> true.
'word was built'  @ word_built            <=> true.
```



Demonstration:


```Prolog
?- can_build_word("A").
true.
?- can_build_word("BARK").
true.
?- can_build_word("BOOK").
false.
?- can_build_word("TREAT").
true.
?- can_build_word("COMMON").
false.
?- can_build_word("SQUAD").
true.
?- can_build_word("CONFUSE").
true.
```



## PureBasic


### PureBasic: Iterative


```purebasic
EnableExplicit
#LETTERS = "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM "

Procedure.s can_make_word(word.s)
  Define letters.s = #LETTERS, buffer.s
  Define index1.i, index2.i
  Define match.b
  For index1=1 To Len(word)
    index2=1 : match=#False
    Repeat
      buffer=StringField(letters,index2,Space(1))
      If FindString(buffer,Mid(word,index1,1),1,#PB_String_NoCase)
        letters=RemoveString(letters,buffer+Chr(32),0,1,1)
        match=#True
        Break
      EndIf
      index2+1
    Until index2>CountString(letters,Space(1))
    If Not match : ProcedureReturn word+#TAB$+"FALSE" : EndIf
  Next
  ProcedureReturn word+#TAB$+"TRUE"
EndProcedure

OpenConsole()
PrintN(can_make_word("a"))
PrintN(can_make_word("BaRK"))
PrintN(can_make_word("BOoK"))
PrintN(can_make_word("TREAt"))
PrintN(can_make_word("cOMMON"))
PrintN(can_make_word("SqUAD"))
PrintN(can_make_word("COnFUSE"))
Input()
```



### PureBasic: Recursive


```purebasic
#LETTERS = "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM "

Macro test(t)
  Print(t+#TAB$+#TAB$+"= ") : If can_make_word(t) : PrintN("True") : Else : PrintN("False") : EndIf
EndMacro

Procedure.s residue(s$,n.i)
  ProcedureReturn Left(s$,Int(n/3)*3)+Mid(s$,Int(n/3)*3+4)
EndProcedure

Procedure.b can_make_word(word$,letters$=#LETTERS)
  n=FindString(letters$,Left(word$,1),1,#PB_String_NoCase)
  If Len(word$) And n : ProcedureReturn can_make_word(Mid(word$,2),residue(letters$,n)) : EndIf
  If Not Len(word$)   : ProcedureReturn #True : Else : ProcedureReturn #False           : EndIf
EndProcedure

OpenConsole()
test("a")         : test("BaRK")      : test("BOoK")      : test("TREAt")
test("cOMMON")    : test("SqUAD")     : test("COnFUSE")
Input()
```

Output:

```txt
a               = True
BaRK            = True
BOoK            = False
TREAt           = True
cOMMON          = False
SqUAD           = True
COnFUSE         = True
```



## Python


===Python: Iterative, with tests===

```python

'''
Note that this code is broken, e.g., it won't work when
blocks = [("A", "B"), ("A","C")] and the word is "AB", where the answer
should be True, but the code returns False.
'''
blocks = [("B", "O"),
          ("X", "K"),
          ("D", "Q"),
          ("C", "P"),
          ("N", "A"),
          ("G", "T"),
          ("R", "E"),
          ("T", "G"),
          ("Q", "D"),
          ("F", "S"),
          ("J", "W"),
          ("H", "U"),
          ("V", "I"),
          ("A", "N"),
          ("O", "B"),
          ("E", "R"),
          ("F", "S"),
          ("L", "Y"),
          ("P", "C"),
          ("Z", "M")]


def can_make_word(word, block_collection=blocks):
    """
    Return True if `word` can be made from the blocks in `block_collection`.

    >>> can_make_word("")
    False
    >>> can_make_word("a")
    True
    >>> can_make_word("bark")
    True
    >>> can_make_word("book")
    False
    >>> can_make_word("treat")
    True
    >>> can_make_word("common")
    False
    >>> can_make_word("squad")
    True
    >>> can_make_word("coNFused")
    True
    """
    if not word:
        return False

    blocks_remaining = block_collection[:]
    for char in word.upper():
        for block in blocks_remaining:
            if char in block:
                blocks_remaining.remove(block)
                break
        else:
            return False
    return True


if __name__ == "__main__":
    import doctest
    doctest.testmod()
    print(", ".join("'%s': %s" % (w, can_make_word(w)) for w in
                    ["", "a", "baRk", "booK", "treat",
                     "COMMON", "squad", "Confused"]))

```


Output:

```txt
'': False, 'a': True, 'baRk': True, 'booK': False, 'treat': True, 'COMMON': False, 'squad': True, 'Confused': True
```



### Python: Recursive


```python
BLOCKS = 'BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM'.split()

def _abc(word, blocks):
    for i, ch in enumerate(word):
        for blk in (b for b in blocks if ch in b):
            whatsleft = word[i + 1:]
            blksleft = blocks[:]
            blksleft.remove(blk)
            if not whatsleft:
                return True, blksleft
            if not blksleft:
                return False, blksleft
            ans, blksleft = _abc(whatsleft, blksleft)
            if ans:
                return ans, blksleft
        else:
            break
    return False, blocks

def abc(word, blocks=BLOCKS):
    return _abc(word.upper(), blocks)[0]

if __name__ == '__main__':
    for word in [''] + 'A BARK BoOK TrEAT COmMoN SQUAD conFUsE'.split():
        print('Can we spell %9r? %r' % (word, abc(word)))
```


Output:

```txt
Can we spell       ''? False
Can we spell       'A'? True
Can we spell    'BARK'? True
Can we spell    'BoOK'? False
Can we spell   'TrEAT'? True
Can we spell  'COmMoN'? False
Can we spell   'SQUAD'? True
Can we spell 'conFUsE'? True
```


===Python: Recursive, telling how===

```python
def mkword(w, b):
    if not w: return []

    c,w = w[0],w[1:]
    for i in range(len(b)):
        if c in b[i]:
            m = mkword(w, b[0:i] + b[i+1:])
            if m != None: return [b[i]] + m

def abc(w, blk):
    return mkword(w.upper(), [a.upper() for a in blk])

blocks = 'BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM'.split()

for w in ", A, bark, book, treat, common, SQUAD, conFUsEd".split(', '):
    print '\'' + w + '\'' + ' ->', abc(w, blocks)
```


Output:
Note the case of empty list returned for empty string; whether it means true or false is up to you.

```txt

'' -> []
'A' -> ['NA']
'bark' -> ['BO', 'NA', 'RE', 'XK']
'book' -> None
'treat' -> ['GT', 'RE', 'ER', 'NA', 'TG']
'common' -> None
'SQUAD' -> ['FS', 'DQ', 'HU', 'NA', 'QD']
'conFUsEd' -> ['CP', 'BO', 'NA', 'FS', 'HU', 'FS', 'RE', 'DQ']

```



## R



### With recursion

Vectorised function for R which will take a character vector and return a logical vector of equal length with TRUE and FALSE as appropriate for words which can/cannot be made with the blocks.


```R
blocks <- rbind(c("B","O"),
 c("X","K"),
 c("D","Q"),
 c("C","P"),
 c("N","A"),
 c("G","T"),
 c("R","E"),
 c("T","G"),
 c("Q","D"),
 c("F","S"),
 c("J","W"),
 c("H","U"),
 c("V","I"),
 c("A","N"),
 c("O","B"),
 c("E","R"),
 c("F","S"),
 c("L","Y"),
 c("P","C"),
 c("Z","M"))

canMake <- function(x) {
  x <- toupper(x)
  used <- rep(FALSE, dim(blocks)[1L])
  charList <- strsplit(x, character(0))
  tryChars <- function(chars, pos, used, inUse=NA) {
    if (pos > length(chars)) {
      TRUE
    } else {
      used[inUse] <- TRUE
      possible <- which(blocks == chars[pos] & !used, arr.ind=TRUE)[, 1L]
      any(vapply(possible, function(possBlock) tryChars(chars, pos + 1, used, possBlock), logical(1)))
    }
  }
  setNames(vapply(charList, tryChars, logical(1), 1L, used), x)
}
canMake(c("A",
           "BARK",
           "BOOK",
           "TREAT",
           "COMMON",
           "SQUAD",
           "CONFUSE"))
```


Output:

```txt
      A    BARK    BOOK   TREAT  COMMON   SQUAD CONFUSE
   TRUE    TRUE   FALSE    TRUE   FALSE    TRUE    TRUE
```



### Without recursion

Second version without recursion and giving every unique combination of blocks for each word:

```R
canMakeNoRecursion <- function(x) {
  x <- toupper(x)
  charList <- strsplit(x, character(0))
  getCombos <- function(chars) {
    charBlocks <-  data.matrix(expand.grid(lapply(chars, function(char) which(blocks == char, arr.ind=TRUE)[, 1L])))
    charBlocks <- charBlocks[!apply(charBlocks, 1, function(row) any(duplicated(row))), , drop=FALSE]
    if (dim(charBlocks)[1L] > 0L) {
      t(apply(charBlocks, 1, function(row) apply(blocks[row, , drop=FALSE], 1, paste, collapse="")))
    } else {
      character(0)
    }
  }
  setNames(lapply(charList, getCombos), x)
}
canMakeNoRecursion(c("A",
           "BARK",
           "BOOK",
           "TREAT",
           "COMMON",
           "SQUAD",
           "CONFUSE"))
```

Output:

```txt
$A
     [,1] [,2]
[1,] "AN" "NA"

$BARK
     [,1] [,2] [,3] [,4]
[1,] "BO" "AN" "RE" "XK"
[2,] "OB" "AN" "RE" "XK"
[3,] "BO" "NA" "RE" "XK"
[4,] "OB" "NA" "RE" "XK"
[5,] "BO" "AN" "ER" "XK"
[6,] "OB" "AN" "ER" "XK"
[7,] "BO" "NA" "ER" "XK"
[8,] "OB" "NA" "ER" "XK"

$BOOK
character(0)

$TREAT
     [,1] [,2] [,3] [,4] [,5]
[1,] "GT" "RE" "ER" "AN" "TG"
[2,] "GT" "ER" "RE" "AN" "TG"
[3,] "GT" "RE" "ER" "NA" "TG"
[4,] "GT" "ER" "RE" "NA" "TG"
[5,] "TG" "RE" "ER" "AN" "GT"
[6,] "TG" "ER" "RE" "AN" "GT"
[7,] "TG" "RE" "ER" "NA" "GT"
[8,] "TG" "ER" "RE" "NA" "GT"

$COMMON
character(0)

$SQUAD
     [,1] [,2] [,3] [,4] [,5]
[1,] "FS" "QD" "HU" "AN" "DQ"
[2,] "FS" "QD" "HU" "AN" "DQ"
[3,] "FS" "QD" "HU" "NA" "DQ"
[4,] "FS" "QD" "HU" "NA" "DQ"
[5,] "FS" "DQ" "HU" "AN" "QD"
[6,] "FS" "DQ" "HU" "AN" "QD"
[7,] "FS" "DQ" "HU" "NA" "QD"
[8,] "FS" "DQ" "HU" "NA" "QD"

$CONFUSE
      [,1] [,2] [,3] [,4] [,5] [,6] [,7]
 [1,] "CP" "OB" "NA" "FS" "HU" "FS" "ER"
 [2,] "PC" "OB" "NA" "FS" "HU" "FS" "ER"
 [3,] "CP" "BO" "NA" "FS" "HU" "FS" "ER"
 [4,] "PC" "BO" "NA" "FS" "HU" "FS" "ER"
 [5,] "CP" "OB" "AN" "FS" "HU" "FS" "ER"
 [6,] "PC" "OB" "AN" "FS" "HU" "FS" "ER"
 [7,] "CP" "BO" "AN" "FS" "HU" "FS" "ER"
 [8,] "PC" "BO" "AN" "FS" "HU" "FS" "ER"
 [9,] "CP" "OB" "NA" "FS" "HU" "FS" "ER"
[10,] "PC" "OB" "NA" "FS" "HU" "FS" "ER"
[11,] "CP" "BO" "NA" "FS" "HU" "FS" "ER"
[12,] "PC" "BO" "NA" "FS" "HU" "FS" "ER"
[13,] "CP" "OB" "AN" "FS" "HU" "FS" "ER"
[14,] "PC" "OB" "AN" "FS" "HU" "FS" "ER"
[15,] "CP" "BO" "AN" "FS" "HU" "FS" "ER"
[16,] "PC" "BO" "AN" "FS" "HU" "FS" "ER"
[17,] "CP" "OB" "NA" "FS" "HU" "FS" "RE"
[18,] "PC" "OB" "NA" "FS" "HU" "FS" "RE"
[19,] "CP" "BO" "NA" "FS" "HU" "FS" "RE"
[20,] "PC" "BO" "NA" "FS" "HU" "FS" "RE"
[21,] "CP" "OB" "AN" "FS" "HU" "FS" "RE"
[22,] "PC" "OB" "AN" "FS" "HU" "FS" "RE"
[23,] "CP" "BO" "AN" "FS" "HU" "FS" "RE"
[24,] "PC" "BO" "AN" "FS" "HU" "FS" "RE"
[25,] "CP" "OB" "NA" "FS" "HU" "FS" "RE"
[26,] "PC" "OB" "NA" "FS" "HU" "FS" "RE"
[27,] "CP" "BO" "NA" "FS" "HU" "FS" "RE"
[28,] "PC" "BO" "NA" "FS" "HU" "FS" "RE"
[29,] "CP" "OB" "AN" "FS" "HU" "FS" "RE"
[30,] "PC" "OB" "AN" "FS" "HU" "FS" "RE"
[31,] "CP" "BO" "AN" "FS" "HU" "FS" "RE"
[32,] "PC" "BO" "AN" "FS" "HU" "FS" "RE"
```



## Racket

I believe you can make an empty word by using no blocks.
So '(can-make-word? "")' is true for me.


```racket
#lang racket
(define block-strings
  (list "BO" "XK" "DQ" "CP" "NA"
        "GT" "RE" "TG" "QD" "FS"
        "JW" "HU" "VI" "AN" "OB"
        "ER" "FS" "LY" "PC" "ZM"))
(define BLOCKS (map string->list block-strings))

(define (can-make-word? w)
  (define (usable-block blocks word-char)
    (for/first ((b (in-list blocks)) #:when (memf (curry char-ci=? word-char) b)) b))

  (define (inner word-chars blocks tried-blocks)
    (cond
      [(null? word-chars) #t]
      [(usable-block blocks (car word-chars))
       =>
       (lambda (b)
         (or
          (inner (cdr word-chars) (append tried-blocks (remove b blocks)) null)
          (inner word-chars (remove b blocks) (cons b tried-blocks))))]
      [else #f]))
  (inner (string->list w) BLOCKS null))

(define WORD-LIST '("" "A" "BARK" "BOOK" "TREAT" "COMMON" "SQUAD" "CONFUSE"))
(define (report-word w)
  (printf "Can we make: ~a? ~a~%"
          (~s w #:min-width 9)
          (if (can-make-word? w) "yes" "no")))

(module+ main
  (for-each report-word WORD-LIST))

(module+ test
  (require rackunit)
  (check-true  (can-make-word? ""))
  (check-true  (can-make-word? "A"))
  (check-true  (can-make-word? "BARK"))
  (check-false (can-make-word? "BOOK"))
  (check-true  (can-make-word? "TREAT"))
  (check-false (can-make-word? "COMMON"))
  (check-true  (can-make-word? "SQUAD"))
  (check-true  (can-make-word? "CONFUSE")))
```


Output:

```txt
Can we make: ""       ? yes
Can we make: "A"      ? yes
Can we make: "BARK"   ? yes
Can we make: "BOOK"   ? no
Can we make: "TREAT"  ? yes
Can we make: "COMMON" ? no
Can we make: "SQUAD"  ? yes
Can we make: "CONFUSE"? yes
```



## RapidQ


```vb
dim Blocks as string
dim InWord as string

Function CanMakeWord (FInWord as string, FBlocks as string) as integer
    dim WIndex as integer, BIndex as integer

    FBlocks = UCase$(FBlocks) - " " - ","
    FInWord = UCase$(FInWord)

    for WIndex = 1 to len(FInWord)
        BIndex = instr(FBlocks, FInWord[WIndex])

        if BIndex then
            FBlocks = Replace$(FBlocks,"**",iif(BIndex mod 2,BIndex,BIndex-1))
        else
            Result = 0
            exit function
        end if
    next

    Result = 1
end function

InWord = "Confuse"
Blocks = "BO, XK, DQ, CP, NA, GT, RE, TG, QD, FS, JW, HU, VI, AN, OB, ER, FS, LY, PC, ZM"
showmessage "Can make: " + InWord + " = " + iif(CanMakeWord(InWord, Blocks), "True", "False")

```

Output:

```txt
Can make: A = TRUE
Can make: BARK = TRUE
Can make: BOOK = FALSE
Can make: TREAT = TRUE
Can make: COMMON = FALSE
Can make: SQUAD = TRUE
Can make: CONFUSE = TRUE

```


## Red


```Red
Red []
test: func [ s][
p: copy "BOXKDQCPNAGTRETGQDFSJWHUVIANOBERFSLYPCZM"
forever [
    if 0 = length? s [  return  'true   ]               ;; if string cleared, all chars found/removed
    if tail? p  [  return 'false   ]                      ;; if at end of search block - not found
    rule: reduce [  first p  '| second p]                 ;; construct parse rule from string
    either parse s [  to rule remove rule to end ] [    ;; remove found char from string
      remove/part p 2                                     ;;character found , remove block
      p: head p                                             ;;start from remaining string at beginning aka head
    ] [  p: skip p 2  ]                                      ;; else move to next block
]
]
foreach word split {A bark book TrEAT COmMoN SQUAD conFUsE}  space [
  print reduce [ pad copy word 8 ":" test word]
 ]

```

Output:

```txt

A        : true
bark     : true
book     : false
TrEAT    : true
COmMoN   : false
SQUAD    : true
conFUsE  : true

```


## REXX


### version 1


```rexx
/*REXX pgm finds if words can be spelt from a pool of toy blocks (each having 2 letters)*/
list= 'A bark bOOk treat common squaD conFuse'   /*words can be:  upper/lower/mixed case*/
blocks= 'BO  XK  DQ  CP  NA  GT  RE  TG  QD  FS  JW  HU  VI  AN  OB  ER  FS  LY  PC  ZM'
              do k=1  for  words(list)           /*traipse through a list of some words.*/
              call  spell  word(list, k)         /*display if word can be spelt (or not)*/
              end   /*k*/                        /* [↑]  tests each word in the list.   */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
spell: procedure expose blocks;  arg x           /*ARG  uppercases the word to be spelt.*/
                         L=length(x);    @.=0    /*get length of the word to be spelt.  */
           do try=1  for L;  z=blocks;   upper z /*use a fresh copy of the  "Z"  blocks.*/
             do n=1  for L;  y=substr(x, n, 1)   /*attempt another letter in the word.  */
             @.n=pos(y, z, 1 + @.n);     if @.n==0  then leave   /*not found?  Try again*/
             z=overlay(' ', z, @.n)              /*mutate the toy block  ───►  a onesy. */
                do q=1  for words(z);    if length(word(z, q))==1  then z=delword(z, q, 1)
                end   /*q*/                      /* [↑]  elide any existing onesy block.*/
             if n==L  then leave try             /*was last letter used in the spelling?*/
             end      /*n*/                      /* [↑]  end of a  toy block  usage.    */
           end        /*try*/                    /* [↑]  end of a  "TRY"  permute.      */
       say right(arg(1), 30)     right( word( "can't can", (n==L) +1), 6)      'be spelt.'
       return
```

{{out|output|text=  when using the default inputs:}}

```txt

                             A    can be spelt.
                          bark    can be spelt.
                          bOOk  can't be spelt.
                         treat    can be spelt.
                        common  can't be spelt.
                         squaD    can be spelt.
                       conFuse    can be spelt.

```



### version 2


```rexx
/* REXX ---------------------------------------------------------------
* 10.01.2014 Walter Pachl  counts the number of possible ways
* 12.01.2014 corrected date and output
*--------------------------------------------------------------------*/
show=(arg(1)<>'')
blocks = 'BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM'
list = '$ A baRk bOOk trEat coMMon squaD conFuse'
list=translate(list)
Do i=1 To words(blocks)
  blkn.i=word(blocks,i)'-'i
  blk.i=word(blocks,i)
  End
w.=''
wlen=0
Do i=1 To words(list)
  w.i=word(list,i)
  wlen=max(wlen,length(w.i))
  End
Do wi=0 To words(list)
  word = w.wi
  ways=0
  poss.=0
  lw=length(word)
  cannot=0
  Do i=1 To lw                         /* loop over the characters   */
    c=substr(word,i,1)                 /* the current character      */
    Do j=1 To words(blocks)            /* loop over blocks           */
      blk=word(blocks,j)
      If pos(c,blk)>0 Then Do  /* block can be used in this position */
        z=poss.i.0+1
        poss.i.z=j
        poss.i.0=z            /* number of possible blocks for pos i */
        End
      End
    If poss.i.0=0 Then Do
      cannot=1
      Leave i
      End
    End

  If cannot=0 Then Do                  /* no prohibitive character   */
    s.=0
    Do j=1 To poss.1.0          /* build possible strings for char 1 */
      z=s.1.0+1
      s.1.z=poss.1.j
      s.1.0=z
      End
    Do i=2 To lw          /* build possible strings for chars 1 to i */
      ii=i-1
      Do j=1 To poss.i.0
        Do k=1 To s.ii.0
          z=s.i.0+1
          s.i.z=s.ii.k poss.i.j
          s.i.0=z
          End
        End
      End
    Do p=1 To s.lw.0            /* loop through all possible strings */
      v=valid(s.lw.p)                  /* test if the string is valid*/
      If v Then Do                     /* it is                      */
        ways=ways+1                    /* increment number of ways   */
        way.ways=''                 /* and store the string's blocks */
        Do ii=1 To lw
          z=word(s.lw.p,ii)
          way.ways=way.ways blk.z
          End
        End
      End
    End
/*---------------------------------------------------------------------
* now show the result
*--------------------------------------------------------------------*/
  ol=left(''''word'''',wlen+2)
  Select
    When ways=0 Then
      ol=ol 'cannot be spelt'
    When ways=1 Then
      ol=ol 'can be spelt'
    Otherwise
      ol=ol 'can be spelt in' ways 'ways'
    End
  Say ol'.'
  If show Then Do
    Do wj=1 To ways
      Say copies(' ',10) way.wj
      End
    End
  End
Exit

valid: Procedure
/*---------------------------------------------------------------------
* Check if the same block is used more than once -> 0
* Else: the combination is valid
*--------------------------------------------------------------------*/
  Parse Arg list
  used.=0
  Do i=1 To words(list)
    w=word(list,i)
    If used.w Then Return 0
    used.w=1
    End
  Return 1
```

Output:

```txt
''        cannot be spelt.
'$'       cannot be spelt.
'A'       can be spelt in 2 ways.
'BARK'    can be spelt in 8 ways.
'BOOK'    cannot be spelt.
'TREAT'   can be spelt in 8 ways.
'COMMON'  cannot be spelt.
'SQUAD'   can be spelt in 8 ways.
'CONFUSE' can be spelt in 32 ways.
```

Output: extended

```txt
''        cannot be spelt.
'$'       cannot be spelt.
'A'       can be spelt in 2 ways.
            NA
            AN
'BARK'    can be spelt in 8 ways.
            BO NA RE XK
            OB NA RE XK
            BO AN RE XK
            OB AN RE XK
            BO NA ER XK
            OB NA ER XK
            BO AN ER XK
            OB AN ER XK
'BOOK'    cannot be spelt.
'TREAT'   can be spelt in 8 ways.
            TG ER RE NA GT
            TG RE ER NA GT
            TG ER RE AN GT
            TG RE ER AN GT
            GT ER RE NA TG
            GT RE ER NA TG
            GT ER RE AN TG
            GT RE ER AN TG
'COMMON'  cannot be spelt.
'SQUAD'   can be spelt in 8 ways.
            FS QD HU NA DQ
            FS QD HU NA DQ
            FS QD HU AN DQ
            FS QD HU AN DQ
            FS DQ HU NA QD
            FS DQ HU NA QD
            FS DQ HU AN QD
            FS DQ HU AN QD
'CONFUSE' can be spelt in 32 ways.
            CP BO NA FS HU FS RE
            PC BO NA FS HU FS RE
            CP OB NA FS HU FS RE
            PC OB NA FS HU FS RE
            CP BO AN FS HU FS RE
            PC BO AN FS HU FS RE
            CP OB AN FS HU FS RE
            PC OB AN FS HU FS RE
            CP BO NA FS HU FS RE
            PC BO NA FS HU FS RE
            CP OB NA FS HU FS RE
            PC OB NA FS HU FS RE
            CP BO AN FS HU FS RE
            PC BO AN FS HU FS RE
            CP OB AN FS HU FS RE
            PC OB AN FS HU FS RE
            CP BO NA FS HU FS ER
            PC BO NA FS HU FS ER
            CP OB NA FS HU FS ER
            PC OB NA FS HU FS ER
            CP BO AN FS HU FS ER
            PC BO AN FS HU FS ER
            CP OB AN FS HU FS ER
            PC OB AN FS HU FS ER
            CP BO NA FS HU FS ER
            PC BO NA FS HU FS ER
            CP OB NA FS HU FS ER
            PC OB NA FS HU FS ER
            CP BO AN FS HU FS ER
            PC BO AN FS HU FS ER
            CP OB AN FS HU FS ER
            PC OB AN FS HU FS ER
```



## Ring


```ring
Blocks = [ :BO, :XK, :DQ, :CP, :NA, :GT, :RE, :TG, :QD, :FS, :JW, :HU, :VI, :AN, :OB, :ER, :FS, :LY, :PC, :ZM ]
Words = [ :A, :BARK, :BOOK, :TREAT, :COMMON, :SQUAD, :CONFUSE ]

for x in words
see '>>> can_make_word("' + upper(x) + '")' + nl
if checkword(x,blocks) see "True" + nl
else see "False" + nl
ok
next

func CheckWord Word,Blocks
cBlocks = BLocks
for x in word
Found = false
for y = 1 to len(cblocks)
if x = cblocks[y][1] or x = cblocks[y][2]
cblocks[y] = "--"
found = true
exit
ok
next
if found = false return false ok
next
return true
```

Output:

```txt

>>> can_make_word("A")
True
>>> can_make_word("BARK")
True
>>> can_make_word("BOOK")
False
>>> can_make_word("TREAT")
True
>>> can_make_word("COMMON")
False
>>> can_make_word("SQUAD")
True
 >>> can_make_word("CONFUSE")
True

```



## Ruby

This one uses a case insensitive regular expression. The 'sub!' method substitutes the first substring it finds and returns nil if nothing is found.

```ruby
words = %w(A BaRK BOoK tREaT COmMOn SqUAD CoNfuSE) << ""

words.each do |word|
  blocks = "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM"
  res = word.each_char.all?{|c| blocks.sub!(/\w?#{c}\w?/i, "")}  #regexps can be interpolated like strings
  puts "#{word.inspect}: #{res}"
end

```

Output:

```txt

"A": true
"BaRK": true
"BOoK": false
"tREaT": true
"COmMOn": false
"SqUAD": true
"CoNfuSE": true
"": true

```



## Run BASIC


```unbasic
blocks$    = "BO,XK,DQ,CP,NA,GT,RE,TG,QD,FS,JW,HU,VI,AN,OB,ER,FS,LY,PC,ZM"
makeWord$  = "A,BARK,BOOK,TREAT,COMMON,SQUAD,Confuse"
b          = int((len(blocks$) /3) +  1)
dim blk$(b)

for i = 1 to len(makeWord$)
  wrd$ = word$(makeWord$,i,",")
  dim hit(b)
  n = 0
  if wrd$ = "" then exit for
  for k = 1 to len(wrd$)
    w$ = upper$(mid$(wrd$,k,1))
    for j = 1 to b
     if hit(j) = 0 then
      if w$ = left$(word$(blocks$,j,","),1) or w$ = right$(word$(blocks$,j,","),1) then
        hit(j) = 1
        n = n + 1
        exit for
      end if
     end if
    next j
  next k
  print wrd$;chr$(9);
  if n = len(wrd$) then print " True" else print " False"
next i
```


```txt
A	 True
BARK	 True
BOOK	 False
TREAT	 True
COMMON	 False
SQUAD	 True
Confuse	 True
```



## Rust

This implementation uses a backtracking search.

```rust
use std::iter::repeat;

fn rec_can_make_word(index: usize, word: &str, blocks: &[&str], used: &mut[bool]) -> bool {
    let c = word.chars().nth(index).unwrap().to_uppercase().next().unwrap();
    for i in 0..blocks.len() {
        if !used[i] && blocks[i].chars().any(|s| s == c) {
            used[i] = true;
            if index == 0 || rec_can_make_word(index - 1, word, blocks, used) {
                return true;
            }
            used[i] = false;
        }
    }
    false
}

fn can_make_word(word: &str, blocks: &[&str]) -> bool {
    return rec_can_make_word(word.chars().count() - 1, word, blocks,
                             &mut repeat(false).take(blocks.len()).collect::<Vec<_>>());
}

fn main() {
    let blocks = [("BO"), ("XK"), ("DQ"), ("CP"), ("NA"), ("GT"), ("RE"), ("TG"), ("QD"), ("FS"),
                  ("JW"), ("HU"), ("VI"), ("AN"), ("OB"), ("ER"), ("FS"), ("LY"), ("PC"), ("ZM")];
    let words = ["A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE"];
    for word in &words {
        println!("{} -> {}", word, can_make_word(word, &blocks))
    }
}

```

Output:

```txt

A -> true
BARK -> true
BOOK -> false
TREAT -> true
COMMON -> false
SQUAD -> true
CONFUSE -> true

```



## Scala

{{libheader|Scala}}
```Scala
object AbcBlocks extends App {

  protected class Block(face1: Char, face2: Char) {

    def isFacedWith(that: Char) = { that == face1 || that == face2 }
    override def toString() = face1.toString + face2
  }
  protected object Block {
    def apply(faces: String) = new Block(faces.head, faces.last)
  }

  type word = Seq[Block]

  private val blocks = List(Block("BO"), Block("XK"), Block("DQ"), Block("CP"), Block("NA"),
    Block("GT"), Block("RE"), Block("TG"), Block("QD"), Block("FS"),
    Block("JW"), Block("HU"), Block("VI"), Block("AN"), Block("OB"),
    Block("ER"), Block("FS"), Block("LY"), Block("PC"), Block("ZM"))

  private def isMakeable(word: String, blocks: word) = {

    def getTheBlocks(word: String, blocks: word) = {

      def inner(word: String, toCompare: word, rest: word, accu: word): word = {
        if (word.isEmpty || rest.isEmpty || toCompare.isEmpty) accu
        else if (toCompare.head.isFacedWith(word.head)) {
          val restant = rest diff List(toCompare.head)
          inner(word.tail, restant, restant, accu :+ toCompare.head)
        } else inner(word, toCompare.tail, rest, accu)
      }
      inner(word, blocks, blocks, Nil)
    }

    word.lengthCompare(getTheBlocks(word, blocks).size) == 0
  }

  val words = List("A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSED", "ANBOCPDQERSFTGUVWXLZ")
  // Automatic tests
  assert(isMakeable(words(0), blocks))
  assert(isMakeable(words(1), blocks))
  assert(!isMakeable(words(2), blocks)) // BOOK not
  assert(isMakeable(words(3), blocks))
  assert(!isMakeable(words(4), blocks)) // COMMON not
  assert(isMakeable(words(5), blocks))
  assert(isMakeable(words(6), blocks))
  assert(isMakeable(words(7), blocks))

  //words(7).mkString.permutations.foreach(s => assert(isMakeable(s, blocks)))

  words.foreach(w => println(s"$w can${if (isMakeable(w, blocks)) " " else "not "}be made."))
}
```



## Scheme

In R5RS:

```scheme
(define *blocks*
  '((#\B #\O) (#\X #\K) (#\D #\Q) (#\C #\P) (#\N #\A)
    (#\G #\T) (#\R #\E) (#\T #\G) (#\Q #\D) (#\F #\S)
    (#\J #\W) (#\H #\U) (#\V #\I) (#\A #\N) (#\O #\B)
    (#\E #\R) (#\F #\S) (#\L #\Y) (#\P #\C) (#\Z #\M)))

(define (exists p? li)
  (and (not (null? li))
       (or (p? (car li))
           (exists p? (cdr li)))))

(define (remove-one x li)
  (cond
    ((null? li) '())
    ((equal? (car li) x) (cdr li))
    (else (cons (car li) (remove-one x (cdr li))))))

(define (can-make-list? li blocks)
  (or (null? li)
      (exists
       (lambda (block)
         (and
          (member (char-upcase (car li)) block)
          (can-make-list? (cdr li) (remove-one block blocks))))
       blocks)))

(define (can-make-word? word)
  (can-make-list? (string->list word) *blocks*))


(define *words*
  '("A" "Bark" "book" "TrEaT" "COMMON" "squaD" "CONFUSE"))

(for-each
 (lambda (word)
   (display (if (can-make-word? word)
                "   Can make word: "
                "Cannot make word: "))
   (display word)
   (newline))
 *words*)
```

Output:

```txt

   Can make word: A
   Can make word: Bark
Cannot make word: book
   Can make word: TrEaT
Cannot make word: COMMON
   Can make word: squaD
   Can make word: CONFUSE

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func boolean: canMakeWords (in array string: blocks, in string: word) is func
  result
    var boolean: okay is FALSE;
  local
    var integer: index is 1;
  begin
    if word = "" then
      okay := TRUE;
    elsif length(blocks) <> 0 then
      while index <= length(blocks) and not okay do
        if blocks[index][1] = word[1] or blocks[index][2] = word[1] then
          okay := canMakeWords(blocks[.. pred(index)] & blocks[succ(index) ..], word[2 ..]);
        end if;
        incr(index);
      end while;
    end if;
  end func;

const array string: blocks is [] ("BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS",
                                  "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM");

const func boolean: canMakeWords (in string: word) is
  return canMakeWords(blocks, upper(word));

const proc: main is func
  local
    var string: word is "";
  begin
    for word range [] ("", "A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "Confuse") do
      writeln(word rpad 10 <& canMakeWords(word));
    end for;
  end func;
```


Output:

```txt

          TRUE
A         TRUE
BARK      TRUE
BOOK      FALSE
TREAT     TRUE
COMMON    FALSE
SQUAD     TRUE
Confuse   TRUE

```



## SequenceL


### Recursive Search Version


```sequencel>import <Utilities/Conversion.sl
;
import <Utilities/Sequence.sl>;

main(args(2)) :=
	let
		result[i] := args[i] ++ ": " ++ boolToString(can_make_word(args[i], InitBlocks));
	in
		delimit(result, '\n');

InitBlocks :=  ["BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS", "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM"];

can_make_word(word(1), blocks(2)) :=
	let
		choices[i] := i when some(blocks[i] = toUpper(head(word)));
		blocksAfterChoice[i] := blocks[(1 ... (choices[i] - 1)) ++ ((choices[i] + 1) ... size(blocks))];
	in
		true when size(word) = 0
	else
		false when size(choices) = 0
	else
		some(can_make_word(tail(word), blocksAfterChoice));

toUpper(letter(0)) :=
	let
		ascii := asciiToInt(letter);
	in
		letter when ascii >= 65 and ascii <= 90
	else
		intToAscii(ascii -  32);
```


Output:

```txt

cmd:> main.exe A BARK BOOK TREAT COMMON SQUAD CONFUSE
"A: true
BARK: true
BOOK: false
TREAT: true
COMMON: false
SQUAD: true
CONFUSE: true"

```



### RegEx Version


```sequencel>import <Utilities/Conversion.sl
;
import <Utilities/Sequence.sl>;
import <RegEx/RegEx.sl>;

main(args(2)) :=
	let
		result[i] := args[i] ++ ": " ++ boolToString(can_make_word(args[i], InitBlocks));
	in
		delimit(result, '\n');

InitBlocks :=  "BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM";

can_make_word(word(1), blocks(1)) :=
	let
		regEx := "(\\a" ++ [toUpper(head(word))] ++ "|" ++ [toUpper(head(word))] ++ "\\a)";

		newBlocks := replaceFirst(blocks, regEx, "");
	in
		true when size(word) = 0
	else
		false when size(newBlocks) = size(blocks)
	else
		can_make_word(tail(word), newBlocks);

toUpper(letter(0)) :=
	let
		ascii := asciiToInt(letter);
	in
		letter when ascii >= 65 and ascii <= 90
	else
		intToAscii(ascii -  32);
```



## Sidef

{{trans|Perl}}

```ruby
func can_make_word(word, blocks) {

    blocks.map! { |b| b.uc.chars.sort.join }.freq!

    func(word, blocks) {
        var char = word.shift
        var candidates = blocks.keys.grep { |k| 0 <= k.index(char) }

        for candidate in candidates {
            blocks{candidate} <= 0 && next;
            local blocks{candidate} = (blocks{candidate} - 1);
            return true if (word.is_empty || __FUNC__(word, blocks));
        }

        return false;
    }(word.uc.chars, blocks)
}
```


Tests:

```ruby
var b1 = %w(BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM)
var b2 = %w(US TZ AO QA)

var tests = [
    ["A", true, b1],
    ["BARK", true, b1],
    ["BOOK", false, b1],
    ["TREAT", true, b1],
    ["COMMON", false, b1],
    ["SQUAD", true, b1],
    ["CONFUSE", true, b1],
    ["auto", true, b2],
];

tests.each { |t|
    var bool = can_make_word(t[0], t[2]);
    say ("%7s -> %s" % (t[0], bool));
    assert(bool == t[1])
}
```


Output:

```txt

      A -> true
   BARK -> true
   BOOK -> false
  TREAT -> true
 COMMON -> false
  SQUAD -> true
CONFUSE -> true
   auto -> true

```


## Simula


```simula
COMMENT ABC PROBLEM;
BEGIN

    CLASS BLOCK(CH1, CH2); CHARACTER CH1, CH2;
    BEGIN
        BOOLEAN USED;
    END;

    CLASS GAME(WORD, POSSIBLE); TEXT WORD; BOOLEAN POSSIBLE;;

    BOOLEAN PROCEDURE CANMAKEWORD(WORD); TEXT WORD;
    BEGIN
        INTEGER I, NUMBLOCKS;
        BOOLEAN ALLPOSSIBLE, FOUND;
        NUMBLOCKS := UPPERBOUND(BLOCKS, 1);
        FOR I := 1 STEP 1 UNTIL NUMBLOCKS DO
            BLOCKS(I).USED := FALSE;
        ALLPOSSIBLE := TRUE;

        WORD.SETPOS(1);
        WHILE ALLPOSSIBLE AND WORD.MORE DO
        BEGIN
            CHARACTER WORDCHAR;
            WORDCHAR := WORD.GETCHAR;
            FOUND := FALSE;
            FOR I := 1 STEP 1 UNTIL NUMBLOCKS DO
            BEGIN
                INSPECT BLOCKS(I) DO
                BEGIN
                    IF (WORDCHAR = CH1 OR WORDCHAR = CH2) AND NOT USED THEN
                    BEGIN
                        USED := FOUND := TRUE;
                        GOTO L;
                    END;
                END;
            END;
        L:
            IF NOT FOUND THEN
                ALLPOSSIBLE := FALSE;
        END;
        CANMAKEWORD := ALLPOSSIBLE;
    END CANMAKEWORD;

    REF(BLOCK) ARRAY BLOCKS(1:20);
    REF(GAME) ARRAY GAMES(1:7);
    TEXT WORD;
    BEGIN
        INTEGER I;
        I := I+1; BLOCKS(I) :- NEW BLOCK('B', 'O');
        I := I+1; BLOCKS(I) :- NEW BLOCK('X', 'K');
        I := I+1; BLOCKS(I) :- NEW BLOCK('D', 'Q');
        I := I+1; BLOCKS(I) :- NEW BLOCK('C', 'P');
        I := I+1; BLOCKS(I) :- NEW BLOCK('N', 'A');
        I := I+1; BLOCKS(I) :- NEW BLOCK('G', 'T');
        I := I+1; BLOCKS(I) :- NEW BLOCK('R', 'E');
        I := I+1; BLOCKS(I) :- NEW BLOCK('T', 'G');
        I := I+1; BLOCKS(I) :- NEW BLOCK('Q', 'D');
        I := I+1; BLOCKS(I) :- NEW BLOCK('F', 'S');
        I := I+1; BLOCKS(I) :- NEW BLOCK('J', 'W');
        I := I+1; BLOCKS(I) :- NEW BLOCK('H', 'U');
        I := I+1; BLOCKS(I) :- NEW BLOCK('V', 'I');
        I := I+1; BLOCKS(I) :- NEW BLOCK('A', 'N');
        I := I+1; BLOCKS(I) :- NEW BLOCK('O', 'B');
        I := I+1; BLOCKS(I) :- NEW BLOCK('E', 'R');
        I := I+1; BLOCKS(I) :- NEW BLOCK('F', 'S');
        I := I+1; BLOCKS(I) :- NEW BLOCK('L', 'Y');
        I := I+1; BLOCKS(I) :- NEW BLOCK('P', 'C');
        I := I+1; BLOCKS(I) :- NEW BLOCK('Z', 'M');
    END;
    BEGIN
        INTEGER N, I; BOOLEAN ANSWER;
        N := N+1; GAMES(N) :- NEW GAME("A",       TRUE);
        N := N+1; GAMES(N) :- NEW GAME("BARK",    TRUE);
        N := N+1; GAMES(N) :- NEW GAME("BOOK",    FALSE);
        N := N+1; GAMES(N) :- NEW GAME("TREAT",   TRUE);
        N := N+1; GAMES(N) :- NEW GAME("COMMON",  FALSE);
        N := N+1; GAMES(N) :- NEW GAME("SQUAD",   TRUE);
        N := N+1; GAMES(N) :- NEW GAME("CONFUSE", TRUE);
        FOR I := 1 STEP 1 UNTIL N DO
        BEGIN
            INSPECT GAMES(I) DO
            BEGIN
                OUTTEXT(WORD);
                OUTTEXT(" => ");
                ANSWER := CANMAKEWORD(WORD);
                OUTCHAR(IF ANSWER THEN 'T' ELSE 'F');
                IF ANSWER EQV POSSIBLE
                    THEN OUTTEXT(" OK")
                    ELSE OUTTEXT(" ------------- WRONG!");
                OUTIMAGE;
            END;
        END;
    END;


END.

```

Output:

```txt
A => T OK
BARK => T OK
BOOK => F OK
TREAT => T OK
COMMON => F OK
SQUAD => T OK
CONFUSE => T OK

```



## Smalltalk

Recursive solution. Tested in Pharo.

```smalltalk

ABCPuzzle>>test
	#('A' 'BARK' 'BOOK' 'TreaT' 'COMMON' 'sQUAD' 'CONFuSE') do: [ :each |
		Transcript crShow: each, ': ', (self solveFor: each) asString ]

ABCPuzzle>>solveFor: letters
	| blocks |
	blocks := #('BO' 'XK' 'DQ' 'CP' 'NA' 'GT' 'RE' 'TG' 'QD' 'FS' 'JW' 'HU' 'VI' 'AN' 'OB' 'ER' 'FS' 'LY' 'PC' 'ZM').
	^ self solveFor: letters asUppercase with: blocks asOrderedCollection

ABCPuzzle>>solveFor: letters with: blocks
	| l ldash matches |
	letters isEmpty ifTrue: [ ^ true ].
	l := letters first.
	ldash := letters allButFirst.
	matches := blocks select: [  :b | b includes: l ].
	matches isEmpty ifTrue: [ ^ false ].
	matches do: [  :m | | bdash |
		bdash := blocks copy.
		bdash remove: m.
		(self solveFor: ldash with: bdash) ifTrue: [ ^ true ] ].
	^ false

```

Output:

```txt

ABCPuzzle new test

A: true
BARK: true
BOOK: false
TreaT: true
COMMON: false
sQUAD: true
CONFuSE: true

```



## SPAD

Works with FriCAS, OpenAxiom, Axiom}}

```SPAD

blocks:List Tuple Symbol:= _
  [(B,O),(X,K),(D,Q),(C,P),(N,A),(G,T),(R,E),(T,G),(Q,D),(F,S), _
   (J,W),(H,U),(V,I),(A,N),(O,B),(E,R),(F,S),(L,Y),(P,C),(Z,M)]


findComb(l:List List NNI):List List NNI ==
  #l=0 => []
  #l=1 => [[s] for s in first l]
  r:List List NNI:=[]
  for y in findComb(rest l) repeat
    r:=concat(r,[concat(x,y) for x in first l])
  return r

canMakeWord?(word,blocks) ==
  word:=upperCase word
  bchr:=[map(char,map(string,s::List(Symbol))) for s in blocks]
  c:=[[j for j in 1..#blocks | member?(word.k,bchr.j)] for k in 1..#word]
  reduce(_or,[test(#removeDuplicates(l)=#word) for l in findComb(c)])


Example:=["a","bark","book","treat","common","squad","confuse"]

[canMakeWord?(s,blocks) for s in Example]


```


Programming details:[http://fricas.github.io/book.pdf UserGuide]

Output:

```txt


  [true,true,false,true,false,true,true]
                                                     Type: List(Boolean)

```


There is optimization potential of course.



## Swift


```Swift
import Foundation

func Blockable(str: String) -> Bool {

    var blocks = [
        "BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS",
        "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM" ]

    var strUp = str.uppercaseString
    var final = ""

    for char: Character in strUp {
        var CharString: String = ""; CharString.append(char)
        for j in 0..<blocks.count {
            if blocks[j].hasPrefix(CharString) ||
               blocks[j].hasSuffix(CharString) {
                final.append(char)
                blocks[j] = ""
                break
            }
        }
    }

    return final == strUp
}

func CanOrNot(can: Bool) -> String {
    return can ? "can" : "cannot"
}

for str in [ "A", "BARK", "BooK", "TrEaT", "comMON", "sQuAd", "Confuse" ] {
    println("'\(str)' \(CanOrNot(Blockable(str))) be spelled with blocks.")
}
```

Output:

```txt

'A' can be spelled with blocks.
'BARK' can be spelled with blocks.
'BooK' cannot be spelled with blocks.
'TrEaT' can be spelled with blocks.
'comMON' cannot be spelled with blocks.
'sQuAd' can be spelled with blocks.
'Confuse' can be spelled with blocks.

```


Works with Swift|3.0.2}}

```Swift
import Swift

func canMake(word: String) -> Bool {
	var blocks = [
		"BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS",
		"JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM"
	]

	for letter in word.uppercased().characters {
		guard let index = blocks.index(where: { $0.characters.contains(letter) }) else {
			return false
		}

		blocks.remove(at: index)
	}

	return true
}

let words = ["a", "bARK", "boOK", "TreAt", "CoMmon", "SquAd", "CONFUse"]

words.forEach { print($0, canMake(word: $0)) }
```

Output:

```txt

A true
BARK true
BooK false
TrEaT true
comMON false
sQuAd true
Confuse true

```



## Tcl

Works with Tcl|8.6}}

```tcl
package require Tcl 8.6

proc abc {word {blocks {BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM}}} {
    set abc {{letters blocks abc} {
	set rest [lassign $letters ch]
	set i 0
	foreach blk $blocks {
	    if {$ch in $blk && (![llength $rest]
		    || [apply $abc $rest [lreplace $blocks $i $i] $abc])} {
		return true
	    }
	    incr i
	}
	return false
    }}
    return [apply $abc [split $word ""] [lmap b $blocks {split $b ""}] $abc]
}

foreach word {"" A BARK BOOK TREAT COMMON SQUAD CONFUSE} {
    puts [format "Can we spell %9s? %s" '$word' [abc $word]]
}
```

Output:

```txt

Can we spell        ''? false
Can we spell       'A'? true
Can we spell    'BARK'? true
Can we spell    'BOOK'? false
Can we spell   'TREAT'? true
Can we spell  'COMMON'? false
Can we spell   'SQUAD'? true
Can we spell 'CONFUSE'? true

```



## TUSCRIPT


```tuscript
set words = "A'BARK'BOOK'TREAT'COMMON'SQUAD'CONFUSE"
set result = *
loop word = words
   set blocks = "BO'XK'DQ'CP'NA'GT'RE'TG'QD'FS'JW'HU'VI'AN'OB'ER'FS'LY'PC'ZM"
   set wordx = split (word, |"~</~")
   set cond = "true"
   loop char = wordx
      set n = filter_index (blocks, "~*{char}*~", -)
      if (n.eq."") then
         set cond = "false"
         exit
      endif
      set n2 = select (n, 1)
      set n3 = select (blocks, #n2, blocks)
   endloop
   set out = concat (word, " ", cond)
   set result = append (result, out)
endloop
```

Output:

```txt
A true
BARK true
BOOK false
TREAT true
COMMON false
SQUAD true
CONFUSE true
```



## TXR



```txr
@(do
   (defvar blocks '((B O) (X K) (D Q) (C P) (N A) (G T) (R E) (T G)
                    (Q D) (F S) (J W) (H U) (V I) (A N) (O B) (E R)
                    (F S) (L Y) (P C) (Z M)))

   ;; Define and build hash which maps each letter that occurs in blocks
   ;; to a list of the blocks in which that letter occurs.

   (defvar alpha2blocks [hash-uni [group-by first blocks]
                                  [group-by second blocks]
                                  append])

   ;; convert, e.g. "abc" -> (A B C)
   ;; intern -- convert a string to an interned symbol "A" -> A
   ;; tuples -- turn string into 1-element tuples: "ABC" -> ("A" "B" "C")
   ;; square brackets around mapcar -- Lisp-1 style evaluation, allowing
   ;;   the intern function binding to be treated as a variable binding.

   (defun string-to-syms (str)
     [mapcar intern (tuples 1 (upcase-str str))])

   ;; Recursive part of algorithm working purely with Lisp symbols.
   ;; alpha -- single symbol denoting a letter
   ;; [alpha2blocks alpha] -- look up list of blocks for given letter
   ;; (memq item list) -- is item a member of list, under eq equality?
   ;; (remq item list) -- remove items from list which are eq to item.

   (defun can-make-word-guts (letters blocks)
     (cond
       ((null letters) t)
       ((null blocks) nil)
       (t (let ((alpha (first letters)))
            (each ((bl [alpha2blocks alpha]))
              (if (and (memq bl blocks)
                       (can-make-word-guts (rest letters)
                                           (remq bl blocks)))
                (return-from can-make-word-guts t)))))))

   (defun can-make-word (str)
     (can-make-word-guts (string-to-syms str) blocks)))
@(repeat)
@w
@(output)
>>> can_make_word("@(upcase-str w)")
@(if (can-make-word w) "True" "False")
@(end)
@(end)
```


Run:


```txt
$ cat abc-problem.data
a
bark
book
treat
common
squad
confuse
$ txr abc-problem.txr abc-problem.data
>>> can_make_word("A")
True
>>> can_make_word("BARK")
True
>>> can_make_word("BOOK")
False
>>> can_make_word("TREAT")
True
>>> can_make_word("COMMON")
False
>>> can_make_word("SQUAD")
True
>>> can_make_word("CONFUSE")
True
```



## UNIX Shell

Works with bash}}


```bash
can_build_word() {
    if [[ $1 ]]; then
        can_build_word_rec "$1" BO XK DQ CP NA GT RE TG QD FS JW HU VI AN OB ER FS LY PC ZM
    else
        return 1
    fi
}

can_build_word_rec() {
    [[ -z $1 ]] && return 0

    local -u word=$1       # uppercase the first parameter
    shift
    local blocks=("$@")

    # see if we have a block for the first letter
    local letter=${word:0:1} indices=() i
    for (( i=0; i<${#blocks[@]}; i++ )); do
        if [[ ${blocks[i]} == *$letter* ]]; then
            indices+=($i)
        fi
    done
    (( ${#indices[@]} == 0 )) && return 1

    local tmp
    for i in ${indices[@]}; do
        tmp=( "${blocks[@]}" )
        unset "tmp[$i]"
        can_build_word_rec "${word:1}" "${tmp[@]}" && return 0
    done

    return 1
}

words=( "" A BARK Book treat COMMON Squad confuse )
for word in "${words[@]}"; do
    can_build_word "$word" "${blocks[@]}" && ans=yes || ans=no
    printf "%s\t%s\n" "$word" $ans
done
```


Output:

```txt
	no
A	yes
BARK	yes
Book	no
treat	yes
COMMON	no
Squad	yes
confuse	yes
```



## UTFool


'''String-based solution'''


```UTFool

···
http://rosettacode.org/wiki/ABC_Problem
···
■ ABC
  § static
    blocks⦂ StringBuffer " BO XK DQ CP NA GT RE TG QD FS
                           JW HU VI AN OB ER FS LY PC ZM"
    ▶ main
    • args⦂ String[]
      for each word in ["A", "BARK", "BOOK", "TREAT",
                        "COMMON", "SQUAD", "CONFUSE"]⦂ String

          System.out.println "⸨word⸩: ⸨canMakeWord word⸩"

    ▶ canMakeWord⦂ boolean
    • word⦂ String
      solution⦂ boolean: word.isEmpty°
      if no solution
        i⦂ int: blocks.indexOf word.substring 0, 1
        🔁 until solution or i < 0
           i: i ÷ 3 × 3 · block index
           block⦂ String: blocks.substring i, i + 3
           blocks.delete i, i + 3 · remove block
           solution: canMakeWord word.substring 1
           blocks.insert i, block · restore block
           i: blocks.indexOf (word.substring 0, 1), i + 3
      return solution

```


'''Collection-based solution'''


```UTFool

···
http://rosettacode.org/wiki/ABC_Problem
···
import java.util.Arrays
import java.util.Collections
import java.util.List
■ ABC
  § static
    ▶ main
    • args⦂   String[]
      blocks⦂ List⟨String⟩:
              Arrays.asList "BO", "XK", "DQ", "CP", "NA",
                            "GT", "RE", "TG", "QD", "FS",
                            "JW", "HU", "VI", "AN", "OB",
                            "ER", "FS", "LY", "PC", "ZM"
      words⦂  List⟨String⟩:
              Arrays.asList "A", "BARK", "BOOK", "TREAT",
                            "COMMON", "SQUAD", "CONFUSE"
      for each word in words
        System.out.println "⸨word⸩: ⸨canMakeWord word, blocks⸩"

    ▶ canMakeWord⦂ boolean
    • word⦂ String
    • blocks⦂ List⟨String⟩
      if word.isEmpty°
         return true
      for each block #i in blocks⦂ String
          if 0 ≤ block.indexOf word.charAt 0
             Collections.swap blocks, 0, i
             if canMakeWord (word.substring 1),
                             blocks.subList 1, blocks.size°
                return true
             Collections.swap blocks, 0, i
      return false

```



## VBA



```vb

Option Explicit

Sub Main_ABC()
Dim Arr, i As Long

    Arr = Array("A", "BARK", "BOOK", "TREAT", "COMMON", "SQUAD", "CONFUSE")
    For i = 0 To 6
        Debug.Print ">>> can_make_word " & Arr(i) & " => " & ABC(CStr(Arr(i)))
    Next i
End Sub

Function ABC(myWord As String) As Boolean
Dim myColl As New Collection
Dim NbLoop As Long, NbInit As Long
Dim b As Byte, i As Byte
Const BLOCKS As String = "B,O;X,K;D,Q;C,P;N,A;G,T;R,E;T,G;Q,D;F,S;J,W;H,U;V,I;A,N;O,B;E,R;F,S;L,Y;P,C;Z,M"

    For b = 0 To 19
        myColl.Add Split(BLOCKS, ";")(b), Split(BLOCKS, ";")(b) & b
    Next b
    NbInit = myColl.Count
    NbLoop = NbInit
    For b = 1 To Len(myWord)
        For i = 1 To NbLoop
            If i > NbLoop Then Exit For
            If InStr(myColl(i), Mid(myWord, b, 1)) <> 0 Then
                myColl.Remove (i)
                NbLoop = NbLoop - 1
                Exit For
            End If
        Next
    Next b
    ABC = (NbInit = (myColl.Count + Len(myWord)))
End Function

```

Output:

```txt

>>> can_make_word A => True
>>> can_make_word BARK => True
>>> can_make_word BOOK => False
>>> can_make_word TREAT => True
>>> can_make_word COMMON => False
>>> can_make_word SQUAD => True
>>> can_make_word CONFUSE => True
```



## Yabasic


```Yabasic
letters$ = "BO,XK,DQ,CP,NA,GT,RE,TG,QD,FS,JW,HU,VI,AN,OB,ER,FS,LY,PC,ZM"

sub canMake(letters$, word$)
	local i, j, p, n, pairs$(1)

	n = token(letters$, pairs$(), ",")
	word$ = upper$(word$)

	for i = 1 to len(word$)
		for j = 1 to n
			p = instr(pairs$(j), mid$(word$, i, 1))
			if p then
				pairs$(j) = ""
				break
			end if
		next j
		if not p return false
	next i
	return true
end sub

print "a = ", canMake(letters$, "a")	// 1 = true
print "bark = ", canMake(letters$, "Bark")	// 1
print "book = ", canMake(letters$, "BooK")	// 0 = false
print "treat = ", canMake(letters$, "TREAt")	// 1
print "common = ", canMake(letters$, "common")	// 0
print "squad = ", canMake(letters$, "squad")	// 1
print "confuse = ", canMake(letters$, "confuse")	// 1
```



## zkl

Translated from C

```zkl
var blocks=T("BO", "XK", "DQ", "CP", "NA", "GT", "RE", "TG", "QD", "FS",
	     "JW", "HU", "VI", "AN", "OB", "ER", "FS", "LY", "PC", "ZM", );

fcn can_make_word(word){
   fcn(blks,word){
      if (not word) return(True);  // bottom of recursion
      foreach b in (blks){ n:=__bWalker.idx;
	 if(not b.holds(word[0])) continue; // letter not on this block
	 blks.del(n);		// remove this block from pile
	 if (self.fcn(blks,word[1,*])) return(True);  // try remaining blocks
	 blks.insert(n,b);	// put block back in pile: backtracking
      }
      False;   // out of blocks but not out of word
   }(blocks.copy(),word.toUpper())
}

foreach word in (T("","A","BarK","BOOK","TREAT","COMMON","SQUAD","Confuse","abba")){
   can_make_word(word).println(": ",word);
}
```

Output:

```txt

True:
True: A
True: BarK
False: BOOK
True: TREAT
False: COMMON
True: SQUAD
True: Confuse
True: abba

```



## zonnon


```zonnon

module Main;
type
	Block = record
		l,r: char;
		used: boolean;
	end Block;

var
	blocks: array 20 of Block;

procedure Exists(c: char): boolean;
var
	i: integer;
	r: boolean;
begin
	r := false;i := 0;
	while ~r & (i < len(blocks)) do
		if ~(blocks[i].used) then
			r := (blocks[i].l = cap(c)) or (blocks[i].r = cap(c));
			blocks[i].used := r;
		end;
		inc(i)
	end;
	return r
end Exists;

procedure CanMakeWord(s: string);
var
	i: integer;
	made: boolean;
begin
	made := true;
	for i := 0 to len(s) - 1 do
		made := made & Exists(s[i])
	end;
	writeln(s:20,"?",made);
	Clean()
end CanMakeWord;

procedure Clean();
var
	i: integer;
begin
	for i := 0 to len(blocks) - 1 do
		blocks[i].used := false
	end
end Clean;

procedure InitBlock(i:integer;l,r:char);
begin
	blocks[i].l := l;blocks[i].r := r;
	blocks[i].used := false;
end InitBlock;

procedure Init;
begin
	InitBlock(0,'B','O');
	InitBlock(1,'X','K');
	InitBlock(2,'D','Q');
	InitBlock(3,'C','Q');
	InitBlock(4,'N','A');
	InitBlock(5,'G','T');
	InitBlock(6,'R','E');
	InitBlock(7,'T','G');
	InitBlock(8,'Q','D');
	InitBlock(9,'F','S');
	InitBlock(10,'J','W');
	InitBlock(11,'H','U');
	InitBlock(12,'V','I');
	InitBlock(13,'A','N');
	InitBlock(14,'O','B');
	InitBlock(15,'E','R');
	InitBlock(16,'F','S');
	InitBlock(17,'L','Y');
	InitBlock(18,'P','C');
	InitBlock(19,'Z','M')
end Init;

begin
	Init();
	CanMakeWord("A");
	CanMakeWord("BARK");
	CanMakeWord("BOOK");
	CanMakeWord("TREAT");
	CanMakeWord("COMMON");
	CanMakeWord("confuse");
end Main.

```

Output:

```txt
                  A   ?  true
                BARK   ?  true
                BOOK   ? false
               TREAT   ?  true
              COMMON   ? false
             confuse   ?  true
```



## ZX Spectrum Basic


```zxbasic
10 LET b$="BOXKDQCPNAGTRETGQDFSJWHUVIANOBERFSLYPCZM"
20 READ p
30 FOR c=1 TO p
40 READ p$
50 GO SUB 100
60 NEXT c
70 STOP
80 DATA 7,"A","BARK","BOOK","TREAT","COMMON","SQUAD","CONFUSE"
90 REM Can make?
100 LET u$=b$
110 PRINT "Can make word ";p$;"? ";
120 FOR i=1 TO LEN p$
130 FOR j=1 TO LEN u$
140 IF p$(i)=u$(j) THEN GO SUB 200: GO TO 160
150 NEXT j
160 IF j>LEN u$ THEN PRINT "No": RETURN
170 NEXT i
180 PRINT "Yes": RETURN
190 REM Erase pair
200 IF j/2=INT (j/2) THEN LET u$(j-1 TO j)="  ": RETURN
210 LET u$(j TO j+1)="  ": RETURN
```

Output:

```txt
Can make word A? Yes
Can make word BARK? Yes
Can make word BOOK? No
Can make word TREAT? Yes
Can make word COMMON? No
Can make word SQUAD? Yes
Can make word CONFYUSE? Yes
```

