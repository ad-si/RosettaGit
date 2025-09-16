+++
title = "Forest fire"
description = ""
date = 2019-10-18T20:50:03Z
aliases = []
[extra]
id = 7734
[taxonomies]
categories = ["task", "Games"]
tags = []
languages = [
  "6502_assembly",
  "ada",
  "algol_68",
  "autohotkey",
  "basic",
  "basic256",
  "batch_file",
  "bbc_basic",
  "c",
  "ceylon",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "easylang",
  "emacs_lisp",
  "erlang",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "gfa_basic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "lua",
  "nim",
  "ocaml",
  "ol",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "postscript",
  "purebasic",
  "python",
  "racket",
  "realbasic",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sather",
  "scala",
  "sidef",
  "sinclair_zx81_basic",
  "t_ttttttttttttttttttt",
  "tcl",
  "tt_t_tttt",
  "ttttttttttt_tt",
  "ubasic_4th",
  "vedit_macro_language",
  "visual_basic_.net",
  "y_yy_y_y_yyy_y_yyy_y_y",
  "zx_spectrum_basic",
]
+++

{{task|Games}}{{wikipedia|Forest-fire model}}[[Category:Cellular automata]]



## Task

Implement the Drossel and Schwabl definition of the [[wp:Forest-fire model|forest-fire model]].


It is basically a 2D   [[wp:Cellular automaton|cellular automaton]]   where each cell can be in three distinct states (''empty'', ''tree'' and ''burning'') and evolves according to the following rules (as given by Wikipedia)

# A burning cell turns into an empty cell
# A tree will burn if at least one neighbor is burning
# A tree ignites with probability   <big>''f'' </big>   even if no neighbor is burning
# An empty space fills with a tree with probability   <big> ''p'' </big>


Neighborhood is the   [[wp:Moore neighborhood|Moore neighborhood]];   boundary conditions are so that on the boundary the cells are always empty ("fixed" boundary condition).

At the beginning, populate the lattice with empty and tree cells according to a specific probability (e.g. a cell has the probability 0.5 to be a tree). Then, let the system evolve.

Task's requirements do not include graphical display or the ability to change parameters (probabilities   <big> ''p'' </big>   and   <big> ''f'' </big>)   through a graphical or command line interface.


## Related tasks

*   See   [[Conway's Game of Life]]
*   See   [[Wireworld]].





## 6502 Assembly


```asm
	ORG	$4357
; SYS 17239 or CALL 17239

EMPTY2	=	$00
TREE2	=	$44
FIRE2	=	$99

; common available zero page


GBASL	=	$26
GBASH	=	$27

SEED2	=	$28
SEED0	=	$29
SEED1	=	$2A

H2	=	$2B
V2	=	$2C
PLOTC	=	$2D
COLOR	=	$2E
PAGE	=	$2F
TOPL	=	$30
TOPH	=	$31
MIDL	=	$32
MIDH	=	$33
BTML	=	$34
BTMH	=	$35
PLOTL	=	$36
PLOTH	=	$37
lastzp	=	$38

tablelo	=	$5000
tablehi = tablelo+25

	JSR START
	STA V2
	LDA #$4C ; JMP instruction
	STA SEED2 ; temporary JMP
	LDX #$00 ; y coord
table:
	TXA
	JSR SEED2 ; temporary JMP GBASCALC
	LDA GBASL
	STA tablelo,X
	LDA GBASH
	STA tablehi,X
	LDY #$00
	TYA
clrline:
	STA (GBASL),Y
	INY
	CPY #40
	BNE clrline

	INX
	CPX V2
	BNE table

	JSR sseed0
	JSR sseed2

	LDX #$60
	STX PAGE
	STX TOPH
	LDY #$00
	STY TOPL
	TYA
zero:	STA (TOPL),Y
	INY
	BNE zero
	INX
	STX TOPH
	CPX #$80
	BNE zero

loop3:
	LDX #0
	STX TOPL
	LDA #41
	STA MIDL
	STA PLOTL
	LDA #83
	STA BTML
	LDA PAGE
	STA TOPH
	STA MIDH
	STA BTMH
	EOR #$10
	STA PLOTH
	STA PAGE
loop2:
	TXA
	STX V2
	LSR       ; F800 PLOT-like...
	; PHP       ; F801
	TAY	 ; save A in Y without touching C
	LDA #$0F
	BCC over2
	ADC #$E0
over2:	STA PLOTC   ; PLOT...
	LDA tablelo,Y ; lookup instead of GBASCALC
	STA GBASL
	LDA tablehi,Y
	STA GBASH
	; PLP     ; continue PLOT
	LDY #$01 ; x coord
loop1:
	STY H2
	LDA (MIDL),Y
	STA (PLOTL),Y
	BEQ empty
	BPL tree
	LDA #EMPTY2
doplot:	LDY H2
	STA (PLOTL),Y
	DEY
	EOR (GBASL),Y
	AND PLOTC
	EOR (GBASL),Y
	STA (GBASL),Y
noplot:
	LDY H2
	INY
	CPY #41
	BNE loop1
	LDA MIDL
	STA TOPL
	LDA MIDH
	STA TOPH
	LDA BTML
	STA MIDL
	STA PLOTL
	CLC
	ADC #42
	STA BTML
	LDA BTMH
	EOR #$10
	STA PLOTH
	EOR #$10
	STA MIDH
	ADC #$00
	STA BTMH
	LDX V2
	INX
	CPX #48
	BNE loop2
	JSR QUIT
	JMP loop3
empty:
	DEC SEED2
	BNE noplot
	JSR sseed2 ; probability f
	LDA #TREE2
	BNE doplot
ignite:
	LDA #FIRE2
	BNE doplot
tree:
	DEC SEED0
	BNE check
	DEC SEED1
	BNE check
	JSR sseed0 ; probability p
	BNE ignite
check:
	LDA (TOPL),Y ; n
	ORA (BTML),Y ; s
	DEY
	ORA (TOPL),Y ; nw
	ORA (MIDL),Y ; w
	ORA (BTML),Y ; sw
	INY
	INY
	ORA (TOPL),Y ; ne
	ORA (MIDL),Y ; e
	ORA (BTML),Y ; se
	BMI ignite
	BPL noplot

sseed0:
	LDA #$17 ; 1 in 10007 (prime)
	STA SEED0
	LDA #$27
	STA SEED1
	RTS
sseed2:
	LDA #$65  ; 1 in 101 (prime)
	STA SEED2
	RTS

default:
	LDA #<GBASCALC ; setup GBASCALC
	STA SEED0
	LDA #>GBASCALC
	STA SEED1
	LDA #25 ; screen rows
	RTS
GBASCALC:
	LDY #$00
	STY GBASH
	ASL
	ASL
	ASL
	STA GBASL
	ASL
	ROL GBASH
	ASL
	ROL GBASH
	ADC GBASL
	STA GBASL
	LDA GBASH
	ADC #$04
	STA GBASH
	RTS

QUIT:
	LDA $E000

; APPLE II

	CMP #$4C
	BNE c64quit

	BIT $C000 ; apple ii keypress?
	BPL CONTINUE ; no keypressed then continue
	BIT $C010 ; clear keyboard strobe
	BIT $C051 ; text mode

; end APPLE II specific

ABORT:
	PLA
	PLA

	LDX #GBASL
restorzp:
	LDA $5100,X
	STA $00,X
	INX
	CPX #lastzp
	BNE restorzp

CONTINUE:
	RTS

START:
	LDX #GBASL
savezp:
	LDA $00,X
	STA $5100,X
	INX
	CPX #lastzp
	BNE savezp

; machine ???

	LDA $E000 ; terribly unreliable, oh well

; APPLE II

	CMP #$4C ; apple ii?
	BNE c64start ; nope, try another

	BIT $C056 ; low resolution
	BIT $C052 ; full screen
	BIT $C054 ; page one
	BIT $C050 ; graphics
; GBASCALC = $F847
	LDA #$47
	STA SEED0
	LDA #$F8
	STA SEED1
	LDA #24 ; screen rows
	RTS

; end APPLE II specific

; COMMODORE 64 specific

c64quit:

; COMMODORE 64

	CMP #$85 ; commodore 64?
	BNE CONTINUE ; nope, default to no keypress

	LDA $C6 ; commodore keyboard buffer length
	BEQ CONTINUE ; no keypressed then continue

	LDA #$00
	STA $C6
	LDA $D016 ; Screen control register #2
	AND #$EF  ; Bit #4: 0 = Multicolor mode off.
	STA $D016
	LDA #21 ; default character set
	STA $D018
	BNE ABORT

c64start:

	CMP #$85 ; commodore 64?
	BEQ c64yes ; yes
	JMP default ; no, default to boringness
c64yes:
	LDA #$00  ; black
	STA $D020 ; border
	LDA #$00  ; black
	STA $D021 ; background
	LDA #$05  ; dark green
	STA $D022 ; Extra background color #1
	LDA #$08  ; orange
	STA $D023 ; Extra background color #2
	LDA $D016 ; Screen control register #2
	ORA #$10  ; Bit #4: 1 = Multicolor mode on.
	STA $D016

	LDA #$30  ; 0011 0000 $3000 charset page
	STA PLOTH
	LSR
	LSR
	STA PLOTC ; 0000 1100 #$0C
; 53272 $D018
; POKE 53272,(PEEK(53272)AND240)+12: REM SET CHAR POINTER TO MEM. 12288
;     Bits #1-#3: In text mode, pointer to character memory
;    (bits #11-#13), relative to VIC bank, memory address $DD00
;     %110, 6: $3000-$37FF, 12288-14335.
	LDA $D018
	AND #$F0
	ORA PLOTC
	STA $D018
; setup nine characters

; 00- 00 00
	LDA #$00 ; chr(0) * 8
	STA PLOTL
	; --- LDA #$00 ; already zero
	TAX ; LDX #$00
	JSR charset

; 04- 00 55
	LDA #32 ; chr(4) * 8
	STA PLOTL
	LDA #$55
	; LDX #$00 ; already zero
	JSR charset

; 09- 00 AA
	LDA #72 ; chr(9) * 8
	STA PLOTL
	LDA #$AA
	; LDX #$00 ; already zero
	JSR charset

; 40- 55 00
	LDA PLOTH ; 512 = chr(64) * 8
	CLC
	ADC #$02
	STA PLOTH
	LDX #$00
	STX PLOTL
	LDA #$00
	LDX #$55
	JSR charset

; 44- 55 55
	LDA #32 ; chr(68) * 8
	STA PLOTL
	TXA ; LDA #$55
	; LDX #$55 ; already 55
	JSR charset

; 49- 55 AA
	LDA #72 ; chr(73) * 8
	STA PLOTL
	LDA #$AA
	; LDX #$55 ; already 55
	JSR charset

; 90- AA 00
	LDA PLOTH ; chr(144) * 8
	CLC
	ADC #$02
	STA PLOTH
	LDA #128
	STA PLOTL
	LDA #$00
	LDX #$AA
	JSR charset

; 94- AA 55
	LDA #160 ; chr(148) * 8
	STA PLOTL
	LDA #$55
	; LDX #$AA ; already AA
	JSR charset

; 99- AA AA
	LDA #200 ; chr(153) * 8
	STA PLOTL
	TXA ; LDA #$AA
	; LDX #$AA ; already AA
	JSR charset
	JMP default
charset:
	LDY #$00
chartop:
	STA (PLOTL),Y
	INY
	CPY #$04
	BNE chartop
	TXA
charbtm:
	STA (PLOTL),Y
	INY
	CPY #$08
	BNE charbtm
	RTS

; end COMMODORE 64 specific


```



## Ada


```Ada
with Ada.Numerics.Float_Random;  use Ada.Numerics.Float_Random;
with Ada.Text_IO;                use Ada.Text_IO;

procedure Forest_Fire is
   type Cell is (Empty, Tree, Fire);
   type Board is array (Positive range <>, Positive range <>) of Cell;
   procedure Step (S : in out Board; P, F : Float; Dice : Generator) is
      function "+" (Left : Boolean; Right : Cell) return Boolean is
      begin
         return Left or else Right = Fire;
      end "+";
      function "+" (Left, Right : Cell) return Boolean is
      begin
         return Left = Fire or else Right = Fire;
      end "+";
      Above : array (S'Range (2)) of Cell := (others => Empty);
      Left_Up, Up, Left : Cell;
   begin
      for Row in S'First (1) + 1..S'Last (1) - 1 loop
         Left_Up := Empty;
         Up      := Empty;
         Left    := Empty;
         for Column in S'First (2) + 1..S'Last (2) - 1 loop
            Left_Up := Up;
            Up      := Above (Column);
            Above (Column) := S (Row, Column);
            case S (Row, Column) is
               when Empty =>
                  if Random (Dice) < P then
                     S (Row, Column) := Tree;
                  end if;
               when Tree =>
                  if Left_Up                 + Up                  +      Above (Column + 1) +
                     Left                    + S (Row,     Column) + S (Row,     Column + 1) +
                     S (Row + 1, Column - 1) + S (Row + 1, Column) + S (Row + 1, Column + 1)
                  or else Random (Dice) < F then
                     S (Row, Column) := Fire;
                  end if;
               when Fire =>
                  S (Row, Column) := Empty;
            end case;
            Left := Above (Column);
         end loop;
      end loop;
   end Step;
   procedure Put (S : Board) is
   begin
      for Row in S'First (1) + 1..S'Last (1) - 1 loop
         for Column in S'First (2) + 1..S'Last (2) - 1 loop
            case S (Row, Column) is
               when Empty => Put (' ');
               when Tree  => Put ('Y');
               when Fire  => Put ('#');
            end case;
         end loop;
         New_Line;
      end loop;
   end Put;

   Dice   : Generator;
   Forest : Board := (1..10 => (1..40 => Empty));
begin
   Reset (Dice);
   for I in 1..10 loop
      Step (Forest, 0.3, 0.1, Dice);
      Put_Line ("-------------" & Integer'Image (I) & " -------------");
      Put (Forest);
   end loop;
end Forest_Fire;
```

Sample output:

```txt
------------- 1 -------------
Y  Y Y        Y YY   Y Y Y  Y        Y
Y Y  YYY   YY  Y      Y  Y  Y     Y
Y     YY Y  Y Y Y  Y   Y   Y   Y YY
          Y          Y Y     Y  YY
Y         YY YYY  Y Y        Y   Y   Y
Y Y         YY  Y    Y        Y    Y
       Y  Y    Y Y          Y Y Y Y
  Y   Y  Y     YYY  Y  Y  Y   Y  Y
------------- 2 -------------
Y  Y Y   YYYYY# YYY  Y Y Y  YYY  Y Y Y
YY#  YYY   YY  Y Y Y  Y  #Y Y    YY  Y
YYY   YY Y  YY# Y  YY  Y Y Y   Y YY
  Y Y     YY   Y  Y YYYY   Y Y YYY  YY
Y    Y   Y#Y YYY YYYY       YYY  Y Y Y
Y Y Y    Y YYY Y#    Y Y Y    Y    Y
       Y  Y   Y# Y   Y  Y Y Y YYY Y
  YY YY  Y  Y  YYY  YYYYY Y   Y YY  Y
------------- 3 -------------
YY # Y   YYYY#  YY#  Y Y #  YYY Y# Y Y
Y# Y YYYYY Y#  # YYY YYY  #YY Y  YY  Y
Y##  Y#Y Y  Y#  YY YY YY # Y   Y YYY
  # Y    Y##   #  YYYYYY YYY YYYYYY YY
Y Y YY   # # YY# #YYYYYY  YYYYY  YY# Y
YYY YYY Y# #YYY#     YYY Y    YY   Y
    Y  YY Y   # Y# Y YY YYY Y YYYYY
  YY #Y YYY Y  ###  YYYY# YY YYYYY YY
------------- 4 -------------
##   Y   YY## Y ## Y YYY  Y YYY #  Y Y
# Y# ###YYY#  Y  ### YYYY  #Y Y Y##  Y
#    # # #  #   #YYYY Y#  Y# YYYYYYYY
Y   YY YY#   YY Y ##YYYY ##Y YYYYY#Y#Y
YY#YYY  Y Y  Y# Y #YYYYYY YYYYY  Y#  Y
YY#YYY#Y#   ### Y YY #YY Y    YY   # Y
    #Y ## #    Y#  YYY# ##Y Y YYYYY  Y
 YYY  #YYY# YYY     YYY#  YYYYY#YY YY
------------- 5 -------------
  Y  #   Y#   Y    #YYYYY #Y#YYY  Y# Y
 Y# Y   ###   Y      Y###Y  # Y #    Y
 Y    Y Y Y      #### #   #  #YY#####
#   ## ##   Y##Y#   #Y##Y  # YYYY# # #
Y# #Y#YY#Y#Y # YY  ####Y# ##YYYY #   #
Y# ### #   Y    # ##Y ##Y#    YY  Y  Y
   Y #         #   Y##    # Y ###Y#  Y
 Y##   ###  YYY  Y  Y##   #YYY# #Y YYY
------------- 6 -------------
 Y#   YY #    Y  Y  #####Y # #Y#Y #  Y
 # Y#Y      Y YY  YYY#   #Y   #Y  Y  #
 #    # # #   Y               ##
           Y#  #   Y #  # Y  ####
# Y # ## # #Y  ## Y    # Y  #YYY   Y
#         Y#YYYY    #   #   Y ##YY#  #
 Y #    Y  Y Y    Y#        Y    # Y Y
 #  Y     Y YY# YY Y#      #Y# Y #Y#YY
------------- 7 -------------
Y# Y  YY      Y  Y       #  Y # #Y  Y#
   # # YY   # YY YY##  Y  #Y   #Y #Y
Y Y    Y Y    #Y  Y  YY     Y     Y
  Y   Y    # Y  Y YY     YYY         Y
  YY        #    YY   Y  #   ###Y  #
 Y        # ####YY       YY #   ##
Y#    Y Y  # #  Y #    YY  Y#Y     # #
  Y #     Y Y# YYYY#     Y  #  YY # #Y
------------- 8 -------------
#  #  #Y Y  Y YYYY Y   Y    Y    #  #
YYY    YY Y   ## Y#  Y Y Y #  Y #Y #Y
Y #Y  Y#YY     #Y #  #Y  Y  YY    #Y
  Y   Y      #  YYYYYY  Y##Y Y   Y   Y
YYYY   Y  Y  Y  YYY   Y Y Y Y   #   YY
 # YY  YY  Y    ##Y   YYY##Y YYY
#   Y Y Y       #Y   YY#Y  # # Y YY
  #Y Y YYY# # YY###      Y    YYY    #
------------- 9 -------------
     Y # YY YY####Y# YYYYYY #      Y
###    ## # Y    #   # Y Y    YY #  #
Y  #Y # #Y Y  Y #     # Y#  #Y Y   # Y
  #   # YYYY    ######YY#  # Y  Y#  YY
###Y  YY YYY #  ###   YY# # YY      YY
  YYY  YY  Y Y    # Y ###  # ### Y YY
    Y Y Y  Y   Y #Y  Y# #Y     Y Y#
   # Y YY#    Y#         # Y  #Y#  YY
------------- 10 -------------
  YYYY   ## Y#    #  ##YYYYY      Y#
   Y        Y       Y Y# #    YYY YY
#Y  #    # #  Y   Y  Y  # Y  # YYYY Y#
   Y   Y###Y          ##  Y Y# Y#   #Y
   #  ## YYY         Y##    ## YY   YY
  ##Y  YY YYY#      Y           Y# #Y
    # Y #  Y Y #  #  #   #     # #
 Y   YYY#     #           YY Y #   #Y

```


## ALGOL 68


### Textual version

{{trans|D}} Note: This specimen retains the original [[#D|D]] coding style.
```algol68
LONG REAL tree prob = 0.55, # original tree probability #
          f prob =    0.01, # new combustion probability #
          p prob =    0.01; # tree creation probability #
MODE CELL = CHAR; CELL empty=" ", tree="T", burning="#";
MODE WORLD = [6, 65]CELL;

PROC has burning neighbours = (WORLD world, INT r, c)BOOL:(
  FOR row shift FROM -1 TO 1 DO
    FOR col shift FROM -1 TO 1 DO
      INT rs = r + row shift, cs = c + col shift;
      IF rs >= LWB world AND rs <= UPB world AND
          cs >= 2 LWB world AND cs <= 2 UPB world THEN
        IF world[rs, cs] = burning THEN true exit FI
      FI
    OD
  OD;
  FALSE EXIT
  true exit: TRUE
);

PROC next state = (REF WORLD world, REF WORLD next world)VOID:(
  FOR r FROM LWB world TO UPB world DO
    REF[]CELL row = world[r, ];
    FOR c FROM LWB row TO UPB row DO
      REF CELL elem = row[c];
      next world[r, c] :=
        IF elem = empty THEN
          IF random<p prob THEN tree ELSE empty FI
        ELIF elem = tree THEN
          IF has burning neighbours(world, r, c) THEN
            burning
          ELSE
            IF random<f prob THEN burning ELSE tree FI
          FI
        ELIF elem = burning THEN
          empty
        FI
    OD
  OD;
  world := next world
);

main:(
  WORLD world; # create world #
  FOR r FROM LWB world TO UPB world DO
    REF []CELL row = world[r, ];
    FOR i FROM LWB row TO UPB row DO
      REF CELL el = row[i];
      el := IF random < tree prob THEN tree ELSE empty FI
    OD
  OD;

  WORLD next world;
  FOR i FROM 0 TO 4 DO
    next state(world, next world);
    printf(($n(2 UPB world)(a)l$, world)); # show world #
    printf(($gl$, 2 UPB world * "-"))
  OD
)
```

Output:

```txt

TTTT T TTTT TT  T T TTT TT TTT  TT TTT T TT T  T    TTT TT T   TT
 TTT TTTTT  T T    T TTTT T   T TTT  TT  T  T TT T   T T TTT  T T
T   T T T TT    T    #  T T   TTT T T  T  TTTTT T  TTT  TTTT TTTT
TT     T  TT TTTTTTTTT TT  TT  T T  TT  T TT TTT TTT TTTT TT  TTT
 TT    TTTTTT  T  T  T T T T TT TT      TT  #T TTT  TT #TTTTTTTT
TT  TTT TTTTTTTTTT TT TTTTTT  TT T TT T TTT T TT T  TT #  T   T
-----------------------------------------------------------------
TTTT T TTTT TT  T T TTT TT TTT  TT TTT T TT T  T    TTT TT T   TT
 TTT TTTTT  T T    T ##TT T   T TTT  TT  T  T TT T   T T TTT  T T
T   T T T TT    T       T T   TTT T T  T  TTTTT T  TTT  TTTT TTTT
TT     T  T# TTTTTTT## TT  TT  T T  TT  T T# #TT TTT T### TT  TTT
 TT    TTTTTT  T  T  T T T T TT TT      TTT  # TTT  TT  #TTTTTTT
TT  TTT TTTTTTTTTT TT TTTTTT  TT T TT T TTT # TT T  TT    T   T
-----------------------------------------------------------------
TTTT T TTTT TT  T T ### TT TTT  TT TTT T TT T  T    TTT TT T   TT
 TTT TTTTT  T T    T   #T T   T TTT  TT  T  T TT T   T T TT#  T T
T   T T T ##    T       T T   TTT T T  T  ##### T  TT#  ##TT TTTT
TT     T  #  TTTTTT#   TT  TT  T T  TT TT #   #T TTT #    TT  TTT
 TT    T#T###  T  T  # T T T TT TT      TT#    TTT  T#   #TTTTTT
TT  TTT TTTTTTTTTT TT TTTTTT  TT T TT T TTT   #T T  TTT   T   T
-----------------------------------------------------------------
TTTT T TTTT TT  T T     #T TTT  TT TTT T TT T  T    TTT TT #   TT
 TTT TTTT#  # T    #    # T   T TTT  TT  #  # ## T   # # ##   T T
T   T T T       T       # T   TTT T T  T        T  T#     ## TTTT
TT     #     #TTTT#    TT  TT  T T  TT TT      # TTT      #T  TTT
 TT    # #     T  #    T T T TT TT      T#     #TT  #     #TTTTT
TT  TTT ######TTTT T# #TTTTT  TT T TT T T##    # T  ###   #   T
-----------------------------------------------------------------
#TTT T T### ##  T #      # TTT  TT TTT T ## #  #    ### ##     TT
 TTT TTT#     T           T   T TTT  TT          T            T T
T   T # #       T         T   TTT T T  T       T#  #         TTTT
TT            #TT#     ##  TT  T T  TT T#        TT#       #  TTT
 TT            T       # T T TT TT      #       #T         #TTTT
TT  TT#       #TT# #   #TTTT  TT T TT T #        T            T
-----------------------------------------------------------------

```



## AutoHotkey

This implementation uses AutoHotkey's pseudo-arrays to contain each cell.
The size of the (square) map, probabilities, and characters which correspond to burning, tree, or empty can be edited at the beginning of the script.

```AutoHotkey

; The array Frame1%x%_%y% holds the current frame. frame2%x%_%y%
; is then calculated from this, and printed. frame2 is then copied to frame1.
; Two arrays are necessary so that each cell advances at the same time
; T=Tree, #=Fire, O=Empty cell
; Size holds the width and height of the map and is used as the # of iterations in loops
; This will save the map as forest_fire.txt in its working directory
;
### ================================================================================



Size := 10
Generation := 0
Tree := "T"
Fire := "#"
Cell := "O"


; --Define probabilities--
    New_Tree := 5
    ; 20 percent chance (1 in 5). A random number will be generated from 1 to New_tree. If this number is 1,
    ; A tree will be created in the current cell

    Spontaneous := 10
    ; 10 percent chance (1 in 10). A random number will be generated from 1 to Spontaneous. If this number is 1,
    ; and the current cell contains a tree, the tree in the current cell will become fire.


GoSub, Generate

; ----------------------Main Loop------------------------------
loop
{
    Generation++
    GoSub, Calculate
    GoSub, Copy
    GoSub, Display
    msgbox, 4, Forest Fire, At Generation %generation%. Continue?
    IfMsgbox, No
        ExitApp
}
return
; -------------------------------------------------------------

Generate:      ; Randomly initializes the map.
loop % size    ; % forces expression mode.
{
	x := A_Index
	Loop % size
	{
		Y := A_Index
		Random, IsTree, 1, 2 ;         -- Roughly half of the spaces will contain trees
		If ( IsTree = 1 )
			Frame1%x%_%y% := Tree
		Else
			Frame1%x%_%y% := Cell
	}
}
return

Calculate:
Loop % size
{
	x := A_Index
	Loop % size
	{
		Y := A_Index
		If ( Frame1%x%_%y% = Cell )
		{
			Random, tmp, 1, New_Tree
			If ( tmp = 1 )
				Frame2%x%_%y% := tree
			Else
				Frame2%x%_%y% := Cell
		}
		Else If ( Frame1%x%_%y% = Tree )
		{
			BoolCatch := PredictFire(x,y)
			If (BoolCatch)
				Frame2%x%_%y% := Fire
			Else
				Frame2%x%_%y% := Tree
		}
		Else If ( Frame1%x%_%y% = Fire )
			Frame2%x%_%y% := Cell
		Else
		{
			contents := Frame1%x%_%y%
			Msgbox Error! Cell %x% , %y% contains %contents% ; This has never happened
			ExitApp
		}
	}
}
return

Copy:
Loop % size
{
	x := A_Index
	Loop % size
	{
		y := A_Index
		frame1%x%_%y% := Frame2%x%_%y%
	}
}
return


Display:
ToPrint := ""
ToPrint .= "=====Generation " . Generation . "=====`n"
Loop % size
{
	x := A_Index
	Loop % size
	{
		y := A_Index
		ToPrint .= Frame1%x%_%y%
	}
	ToPrint .= "`n"
}
FileAppend, %ToPrint%, Forest_Fire.txt
Return


PredictFire(p_x,p_y){
    Global ; allows access to all frame1*_* variables (the pseudo-array)
    A := p_x-1
    B := p_y-1
    C := p_x+1
    D := p_y+1
    If ( Frame1%A%_%p_Y% = fire )
        return 1
    If ( Frame1%p_X%_%B% = fire )
        return 1
    If ( Frame1%C%_%p_Y% = fire )
        return 1
    If ( Frame1%p_X%_%D% = fire )
        return 1

    If ( Frame1%A%_%B% = Fire )
        return 1
    If ( Frame1%A%_%D% = fire )
        return 1
    If ( Frame1%C%_%B% = fire )
        return 1
    If ( Frame1%C%_%D% = Fire )
        return 1

    Random, tmp, 1, spontaneous
    if ( tmp = 1 )
        return 1
    return 0
}

```

Sample Output using the default settings:

```txt
### ==Generation 1==

OTTTOOTOOT
OTOOTTTTOT
TTOOOTTTO#
TOOTOTOOTT
OTTOTOOTTO
TOTTTTOOTO
TOTTT#OOOT
OT#OOTOOTT
TTO#TOOTTT
O#OOOTOTTT

### ==Generation 2==

OTTTOOTOOT
OTTOTTT#O#
TTOOOTTTOO
TOOTTTOT##
OTTOTTO##O
TOTT##OTTO
TO###OOOOT
T#OOO#OOTT
##TO#OOTTT
TOTOOTOTTT

### ==Generation 3==

OTT#OO#TO#
OTTOTT#OTO
TTOOOT##OO
TOOTTTT#OO
OTTO##OOOO
TO##OOO##O
#OOOOOOOOT
#OOOOOOOTT
OO#OOTOTTT
#O#TT#OTTT

### ==Generation 4==

OT#OOOO#OO
OT#O##OO#T
T#TOT#OOOO
TOO####OOT
O##OOOOOOO
#OOOOOOOOO
OOOOOOOOO#
OTOOOOOOTT
OOOOO#TTTT
OTO##OO#TT

### ==Generation 5==

O#OOOTOOOT
O#OOOOOOO#
#O#O#OOTOT
#OOOOOOOOT
OOOOOOOOOO
OTOOOOOOOO
TTOOOTOTTO
TTOOOTOO##
OOTTOO###T
OTOOOOOO#T

```



## BASIC

=
## BASIC256
=
[[File:Forest fire BASIC-256.gif|right|thumb|Forest fire animation: p=0.03, p/f=1000]]

```basic256
N = 150 : M = 150 : P = 0.03 : F = 0.00003

dim f(N+2,M+2) # 1 tree, 0 empty, 2 fire
dim fn(N+2,M+2)
graphsize N,M
fastgraphics

for x = 1 to N
	for y = 1 to M
		if rand<0.5 then f[x,y] = 1
	next y
next x

while True
	for x = 1 to N
		for y = 1 to M
			if not f[x,y] and rand<P then fn[x,y]=1
			if f[x,y]=2 then fn[x,y]=0
			if f[x,y]=1 then
				fn[x,y] = 1
				if f[x-1,y-1]=2 or f[x,y-1]=2 or f[x+1,y-1]=2 then fn[x,y]=2
				if f[x-1,y]=2 or f[x+1,y]=2 or rand<F then fn[x,y]=2
				if f[x-1,y+1]=2 or f[x,y+1]=2 or f[x+1,y+1]=2 then fn[x,y]=2
			end if
			# Draw
			if fn[x,y]=0 then color black
			if fn[x,y]=1 then color green
			if fn[x,y]=2 then color yellow
			plot x-1,y-1
		next y
	next x
	refresh
	for x = 1 to N
		for y = 1 to M
			f[x,y] = fn[x,y]
		next y
	next x
end while
```


=
## BBC BASIC
=
```bbcbasic
      VDU 23,22,400;400;16,16,16,128
      OFF

      DIM old&(200,200), new&(200,200)
      p = 0.01
      f = 0.0001

      REM 0 = empty, 1 = tree, 2 = burning
      REPEAT
        WAIT 10
        FOR x% = 1 TO 199
          FOR y% = 1 TO 199
            CASE old&(x%,y%) OF
              WHEN 0:
                IF p > RND(1) THEN
                  new&(x%,y%) = 1
                  GCOL 2
                  PLOT 4*x%,4*y%
                ENDIF
              WHEN 1:
                IF f > RND(1) OR old&(x%-1,y%)=2 OR old&(x%+1,y%)=2 OR \
                \ old&(x%-1,y%-1)=2 OR old&(x%,y%-1)=2 OR old&(x%+1,y%-1)=2 OR \
                \ old&(x%-1,y%+1)=2 OR old&(x%,y%+1)=2 OR old&(x%+1,y%+1)=2 THEN
                  new&(x%,y%) = 2
                  GCOL 1
                  PLOT 4*x%,4*y%
                ENDIF
              WHEN 2:
                new&(x%,y%) = 0
                GCOL 15
                PLOT 4*x%,4*y%
            ENDCASE
          NEXT
        NEXT x%
        old&() = new&()
      UNTIL FALSE
```

Output:
<p>
[[File:Forestbbc.gif|200px]]

=
## FreeBASIC
=

```freebasic
'[RC] Forest Fire
'written for FreeBASIC
'Program code based on BASIC256 from Rosettacode website
'http://rosettacode.org/wiki/Forest_fire#BASIC256
'06-10-2016 updated/tweaked the code
'compile with fbc -s gui

#Define M 400
#Define N 640

Dim As Double     p = 0.003
Dim As Double  fire = 0.00003
'Dim As Double number1
Dim As Integer gen, x, y
Dim As String press

'f0() and fn() use memory from the memory pool
Dim As UByte f0(), fn()
ReDim f0(-1 To N +2, -1 To M +2)
ReDim fn(-1 To N +2, -1 To M +2)

Dim As UByte white  = 15  'color 15 is white
Dim As UByte yellow = 14  'color 14 is yellow
Dim As UByte black  = 0   'color 0 is black
Dim As UByte green  = 2   'color 2 is green
Dim As UByte red    = 4   'color 4 is red

Screen 18 'Resolution 640x480 with at least 256 colors
Randomize Timer

Locate 28,1
Beep
Print " Welcome to Forest Fire"
Locate 29,1
Print " press any key to start"
Sleep
'Locate 28,1
'Print " Welcome to Forest Fire"
Locate 29,1
Print "                       "

' 1 tree, 0 empty, 2 fire
Color green ' this is green color for trees
For x = 1 To N
  For y = 1 To M
    If Rnd < 0.5 Then 'populate original tree density
      f0(x,y) = 1
      PSet (x,y)
    End If
  Next y
Next x

Color white
Locate 29,1
Print " Press any key to continue                        "
Sleep
Locate 29,1
Print " Press 'space bar' to continue/pause, ESC to stop "

Do
  press = InKey
  ScreenLock
  For x = 1 To N
    For y = 1 To M
      If Not f0(x,y) And Rnd<P Then fn(x,y)=1
      If f0(x,y)=2 Then fn(x,y)=0
      If f0(x,y)=1 Then
        fn(x,y) = 1
        If f0(x-1,y-1)=2 OrElse f0(x,y-1)=2 OrElse f0(x+1,y-1)=2 Then fn(x,y)=2
        If f0(x-1,y)=2 OrElse f0(x+1,y)=2 OrElse Rnd<fire Then fn(x,y)=2
        If f0(x-1,y+1)=2 OrElse f0(x,y+1)=2 OrElse f0(x+1,y+1)=2 Then fn(x,y)=2
      End If
      'set up color and drawing
      '0 empty (black),  1 tree (green), 2 fire (white)
      If fn(x,y)=0 Then Color black 'empty
      If fn(x,y)=1 Then Color green 'tree
      If fn(x,y)=2 Then Color red   'fire
      'plot x-1,y-1
      PSet (x-1,y-1)
    Next y
  Next x
  'print generation number
  gen = gen + 1
  Locate 28,1
  Color white 'this is white color
  Print " Generation number # ";gen
  'transfer new generation to current generation
  For x = 1 To N
    For y = 1 To M
      f0(x,y) = fn(x,y)
    Next y
  Next x
  ScreenUnlock

  ' amount for sleep is in milliseconds, 1 = ignore key press
  Sleep 50, 1  ' slow down a little ... goes too fast otherwise
  If press = " " Then Sleep : press = InKey
  If press = "s" Then Sleep
  ' return to do loop up top until "esc" key is pressed.
  ' clicking close windows "X", closes the window immediately
Loop Until press = Chr(27) OrElse press = Chr(255)+"k"
If press = Chr(255) + "k" Then End

Locate 28,1
Color white
Print " You entered ESC - goodbye                        "
Print " Press any key to exit                            "
Sleep
```


=
## GFA Basic
=


```basic

width%=80
height%=50
DIM world%(width%+2,height%+2,2)
clock%=0
'
empty%=0 ! some mnemonic codes for the different states
burning%=1
tree%=2
'
f=0.0003
p=0.03
max_clock%=100
'
@open_window
@setup_world
DO
  clock%=clock%+1
  EXIT IF clock%>max_clock%
  @display_world
  @update_world
LOOP
@close_window
'
' Setup the world
'
PROCEDURE setup_world
  LOCAL i%,j%
  '
  RANDOMIZE 0
  ARRAYFILL world%(),empty%
  ' with Probability 0.5, create tree in cells
  FOR i%=1 TO width%
    FOR j%=1 TO height%
      IF RND>0.5
        world%(i%,j%,0)=tree%
      ENDIF
    NEXT j%
  NEXT i%
  '
  cur%=0
  new%=1
RETURN
'
' Display world on window
'
PROCEDURE display_world
  LOCAL size%,i%,j%,offsetx%,offsety%,x%,y%
  '
  size%=5
  offsetx%=10
  offsety%=20
  '
  VSETCOLOR 0,15,15,15 ! colour for empty
  VSETCOLOR 1,15,0,0 ! colour for burning
  VSETCOLOR 2,0,15,0 ! colour for tree
  VSETCOLOR 3,0,0,0 ! colour for text
  DEFTEXT 3
  PRINT AT(1,1);"Clock: ";clock%
  '
  FOR i%=1 TO width%
    FOR j%=1 TO height%
      x%=offsetx%+size%*i%
      y%=offsety%+size%*j%
      SELECT world%(i%,j%,cur%)
      CASE empty%
        DEFFILL 0
      CASE tree%
        DEFFILL 2
      CASE burning%
        DEFFILL 1
      ENDSELECT
      PBOX x%,y%,x%+size%,y%+size%
    NEXT j%
  NEXT i%
RETURN
'
' Check if a neighbour is burning
'
FUNCTION neighbour_burning(i%,j%)
  LOCAL x%
  '
  IF world%(i%,j%-1,cur%)=burning%
    RETURN TRUE
  ENDIF
  IF world%(i%,j%+1,cur%)=burning%
    RETURN TRUE
  ENDIF
  FOR x%=-1 TO 1
    IF world%(i%-1,j%+x%,cur%)=burning% OR world%(i%+1,j%+x%,cur%)=burning%
      RETURN TRUE
    ENDIF
  NEXT x%
  RETURN FALSE
ENDFUNC
'
' Update the world state
'
PROCEDURE update_world
  LOCAL i%,j%
  '
  FOR i%=1 TO width%
    FOR j%=1 TO height%
      world%(i%,j%,new%)=world%(i%,j%,cur%)
      SELECT world%(i%,j%,cur%)
      CASE empty%
        IF RND>1-p
          world%(i%,j%,new%)=tree%
        ENDIF
      CASE tree%
        IF @neighbour_burning(i%,j%) OR RND>1-f
          world%(i%,j%,new%)=burning%
        ENDIF
      CASE burning%
        world%(i%,j%,new%)=empty%
      ENDSELECT
    NEXT j%
  NEXT i%
  '
  cur%=1-cur%
  new%=1-new%
RETURN
'
' open and clear window
'
PROCEDURE open_window
  OPENW 1
  CLEARW 1
  VSETCOLOR 4,8,8,0
  DEFFILL 4
  PBOX 0,0,500,400
RETURN
'
' close the window after keypress
'
PROCEDURE close_window
  ~INP(2)
  CLOSEW 1
RETURN

```


=
## PureBasic
=

```PureBasic
; Some systems reports high CPU-load while running this code.
; This may likely be due to the graphic driver used in the
; 2D-function Plot().
; If experiencing this problem, please reduce the #Width & #Height
; or activate the parameter #UnLoadCPU below with a parameter 1 or 2.
;
; This code should work with the demo version of PureBasic on both PC & Linux

; General parameters for the world
#f    = 1e-6
#p    = 1e-2
#SeedATree  = 0.005
#Width      = 400
#Height     = 400

; Setting up colours
#Fire       = $080CF7
#BackGround = $BFD5D3
#YoungTree  = $00E300
#NormalTree = $00AC00
#MatureTree = $009500
#OldTree    = $007600
#Black      = $000000

; Depending on your hardware, use this to control the speed/CPU-load.
; 0 = No load reduction
; 1 = Only active about every second frame
; 2 = '1' & release the CPU after each horizontal line.
#UnLoadCPU  = 0

Enumeration
  #Empty  =0
  #Ignited
  #Burning
  #Tree
  #Old=#Tree+20
EndEnumeration

Global Dim Forest.i(#Width, #Height)
Global Title$="Forest fire in PureBasic"
Global Cnt

Macro Rnd()
  (Random(2147483647)/2147483647.0)
EndMacro

Procedure Limit(n, min, max)
  If n<min
    n=min
  ElseIf n>max
    n=max
  EndIf
  ProcedureReturn n
EndProcedure

Procedure SpreadFire(x,y)
  Protected cnt=0, i, j
  For i=Limit(x-1, 0, #Width) To Limit(x+1, 0, #Width)
    For j=Limit(y-1, 0, #Height) To Limit(y+1, 0, #Height)
      If Forest(i,j)>=#Tree
        Forest(i,j)=#Ignited
      EndIf
    Next
  Next
EndProcedure

Procedure InitMap()
  Protected x, y, type
  For y=1 To #Height
    For x=1 To #Width
      If Rnd()<=#SeedATree
        type=#Tree
      Else
        type=#Empty
      EndIf
      Forest(x,y)=type
    Next
  Next
EndProcedure

Procedure UpdateMap()
  Protected x, y
  For y=1 To #Height
    For x=1 To #Width
      Select Forest(x,y)
        Case #Burning
          Forest(x,y)=#Empty
          SpreadFire(x,y)
        Case #Ignited
          Forest(x,y)=#Burning
        Case #Empty
          If Rnd()<=#p
            Forest(x,y)=#Tree
          EndIf
        Default
          If Rnd()<=#f
            Forest(x,y)=#Burning
          Else
            Forest(x,y)+1
          EndIf
      EndSelect
    Next
  Next
EndProcedure

Procedure PresentMap()
  Protected x, y, c
  cnt+1
  SetWindowTitle(0,Title$+", time frame="+Str(cnt))
  StartDrawing(ImageOutput(1))
  For y=0 To OutputHeight()-1
    For x=0 To OutputWidth()-1
      Select Forest(x,y)
        Case #Empty
          c=#BackGround
        Case #Burning, #Ignited
          c=#Fire
        Default
          If Forest(x,y)<#Tree+#Old
            c=#YoungTree
          ElseIf Forest(x,y)<#Tree+2*#Old
            c=#NormalTree
          ElseIf Forest(x,y)<#Tree+3*#Old
            c=#MatureTree
          ElseIf Forest(x,y)<#Tree+4*#Old
            c=#OldTree
          Else ; Tree died of old age
            Forest(x,y)=#Empty
            c=#Black
          EndIf
      EndSelect
      Plot(x,y,c)
    Next
    CompilerIf #UnLoadCPU>1
      Delay(1)
    CompilerEndIf
  Next
  StopDrawing()
  ImageGadget(1, 0, 0, #Width, #Height, ImageID(1))
EndProcedure

If OpenWindow(0, 10, 30, #Width, #Height, Title$, #PB_Window_MinimizeGadget)
  SmartWindowRefresh(0, 1)
  If CreateImage(1, #Width, #Height)
    Define Event, freq
    If ExamineDesktops() And DesktopFrequency(0)
      freq=DesktopFrequency(0)
    Else
      freq=60
    EndIf
    AddWindowTimer(0,0,5000/freq)
    InitMap()
    Repeat
      Event = WaitWindowEvent()
      Select Event
        Case #PB_Event_CloseWindow
          End
        Case #PB_Event_Timer
          CompilerIf #UnLoadCPU>0
            Delay(25)
          CompilerEndIf
          UpdateMap()
          PresentMap()
      EndSelect
    ForEver
  EndIf
EndIf
```

[[Image:Forest_Fire_in_PureBasic,_frame_300.png]]

=
## REALbasic
=
This example puts all of the forestry logic into a Thread class. This allows the UI to remain responsive while the Thread does all the work in the background. We create a Thread by subclassing the Thread object in the IDE, in this case creating ''forestfire'' as a subclass of the Thread object and put the following code in its ''Run()'' event:

```realbasic

Sub Run()
  //Handy named constants
  Const empty = 0
  Const tree = 1
  Const fire = 2
  Const ablaze = &cFF0000    //Using the &c numeric operator to indicate a color in hex
  Const alive = &c00FF00
  Const dead = &c804040

  //Our forest
  Dim worldPic As New Picture(480, 480, 32)
  Dim newWorld(120, 120) As Integer
  Dim oldWorld(120, 120) As Integer

  //Initialize forest
  Dim rand As New Random
  For x as Integer = 0 to 119
    For y as Integer = 0 to 119
      if rand.InRange(0, 2) = 0 Or x = 119 or y = 119 or x = 0 or y = 0 Then
        newWorld(x, y) = empty
        worldPic.Graphics.ForeColor = dead
        worldPic.Graphics.FillRect(x*4, y*4, 4, 4)
      Else
        newWorld(x, y) = tree
        worldPic.Graphics.ForeColor = alive
        worldPic.Graphics.FillRect(x*4, y*4, 4, 4)
      end if
    Next
  Next
  oldWorld = newWorld

  //Burn, baby burn!
  While Window1.stop = False
    For x as Integer = 0 To 119
      For y As Integer = 0 to 119
        Dim willBurn As Integer = rand.InRange(0, Window1.burnProb.Value)
        Dim willGrow As Integer = rand.InRange(0, Window1.growProb.Value)
        if x = 119 or y = 119 or x = 0 or y = 0 Then
          Continue
        end if
        Select Case oldWorld(x, y)
        Case empty
          If willGrow = (Window1.growProb.Value) Then
            newWorld(x, y) = tree
            worldPic.Graphics.ForeColor = alive
            worldPic.Graphics.FillRect(x*4, y*4, 4, 4)
          end if
        Case tree
          if oldWorld(x - 1, y) = fire Or oldWorld(x, y - 1) = fire Or oldWorld(x + 1, y) = fire Or oldWorld(x, y + 1) = fire Or oldWorld(x + 1, y + 1) = fire Or oldWorld(x - 1, y - 1) = fire Or oldWorld(x - 1, y + 1) = fire Or oldWorld(x + 1, y - 1) = fire Or willBurn = (Window1.burnProb.Value) Then
            newWorld(x, y) = fire
            worldPic.Graphics.ForeColor = ablaze
            worldPic.Graphics.FillRect(x*4, y*4, 4, 4)
          end if
        Case fire
          newWorld(x, y) = empty
          worldPic.Graphics.ForeColor = dead
          worldPic.Graphics.FillRect(x*4, y*4, 4, 4)
        End Select
      Next
    Next
    Window1.Canvas1.Graphics.DrawPicture(worldPic, 0, 0)
    oldWorld = newWorld
    me.Sleep(Window1.speed.Value)
  Wend
End Sub

```

As you can see, this Thread is expecting a Window object called Window1 with several other objects within it. The IDE will automatically create a Window object called Window1 when a new GUI application is created. Our Window1 has 5 objects (widgets) in it: a Canvas (for displaying graphics), three sliders, and a pushbutton.

```realbasic

Sub Open()
  //First method to run on the creation of a new Window. We instantiate an instance of our forestFire thread and run it.
  Dim fire As New forestFire
  fire.Run()
End Sub

stop As Boolean  //a globally accessible property of Window1. Boolean properties default to False.

Sub Pushbutton1.Action()
  stop = True
End Sub

```

[[Image:ForestFireRB.PNG]]

=
## Run BASIC
=

```runbasic
graphic #g, 200,200
dim preGen(200,200)
dim newGen(200,200)

for gen = 1 to 200
  for x = 1 to 199
    for y = 1 to 199
      select case preGen(x,y)
        case 0
          if rnd(0) > .99 then newGen(x,y) = 1  : #g "color green ; set "; x; " "; y
        case 2
          newGen(x,y) = 0                       : #g "color brown ; set "; x; " "; y
        case 1
          if preGen(x-1,y-1) = 2 or preGen(x-1,y)   = 2 or preGen(x-1,y+1) = 2 _
          or preGen(x,y-1)   = 2 or preGen(x,y+1)   = 2 or preGen(x+1,y-1) = 2 _
          or preGen(x+1,y)   = 2 or preGen(x+1,y+1) = 2 or rnd(0) > .999 then
              #g "color red ; set "; x; " "; y
              newGen(x,y) = 2
          end if
      end select
      preGen(x-1,y-1) = newGen(x-1,y-1)
    next y
  next x
next gen
render #g
```

[[File:ForestFire.png]]

=
## Sinclair ZX81 BASIC
=
Requires 16k of RAM.

In essence this is an enhanced version of my ZX Spectrum implementation (see below). The main improvement is that this version shows the ages of the trees: the age is represented using <code>0</code> to <code>9</code>, then <code>A</code> to <code>Z</code>, followed theoretically by the special characters <code>£$:?()><=+-*/;,.</code> (in that order) and only then cycling back to <code>0</code>. Realistically, no tree is likely to live that long.

The subroutine at line 1000 takes a number <code>N</code> and returns its inverse-video string representation as <code>I$</code>.

A couple of other notes on the listing:

(1) some characters need to be entered in <code>G</code>raphics mode, which is accessed using <code>SHIFT</code><code>9</code>. I have represented this using square brackets: so if the listing says <code>[ROSETTA CODE]</code>, you need to go into <code>G</code> mode and type <code>ROSETTA CODE</code> (which will be displayed on the ZX81 screen in inverse video). As a special case, <code>[a]</code> means for you to go into <code>G</code> mode and then type <code>SHIFT</code><code>A</code>. The ZX81 character set does not include either square brackets or lower-case letters, so I hope this convention will not lead to too much confusion.

(2) this program differs from most BASIC examples on Rosetta Code, but resembles most real BASIC programs of more than about 20 lines, in that the line numbers do not always go up smoothly in multiples of ten.

```basic
  10 DIM F$(20,30)
  20 DIM N$(20,30)
  30 LET INIT=.5
  40 LET F=.02
  50 LET P=.05
  60 PRINT AT 0,1;"[FOREST FIRE   FOR ROSETTA CODE]"
  70 FOR I=0 TO 21
  80 PRINT AT I,0;"[ ]"
  90 PRINT AT I,31;"[ ]"
 100 NEXT I
 110 FOR I=1 TO 30
 120 PRINT AT 21,I;"[ ]"
 130 NEXT I
 140 LET G=0
 150 LET T=0
 160 PRINT AT 21,1;"[GENERATION 0]"
 170 PRINT AT 21,20;"[COVER]"
 180 FOR I=1 TO 20
 190 FOR J=1 TO 30
 200 IF RND>=INIT THEN GOTO 240
 210 PRINT AT I,J;"0"
 220 LET F$(I,J)="0"
 230 LET T=T+1
 240 NEXT J
 250 NEXT I
 300 PRINT AT 21,26;"[      ]"
 310 LET N=INT (.5+T/6)
 320 GOSUB 1000
 330 PRINT AT 21,26;I$;"[ PC]"
 340 FOR I=1 TO 20
 350 PRINT AT I,0;"[>]"
 360 FOR J=1 TO 30
 380 IF F$(I,J)<>"[a]" THEN GOTO 410
 390 LET N$(I,J)=" "
 400 GOTO 530
 410 IF F$(I,J)<>" " THEN GOTO 433
 420 IF RND<=P THEN LET N$(I,J)="0"
 430 GOTO 530
 433 LET N$(I,J)=CHR$ (1+CODE F$(I,J))
 437 IF N$(I,J)>"Z" THEN LET N$(I,J)="£"
 440 FOR K=I-1 TO I+1
 450 FOR L=J-1 TO J+1
 460 IF K=0 OR L=0 OR K=21 OR L=21 THEN GOTO 480
 470 IF F$(K,L)="[a]" THEN GOTO 510
 480 NEXT L
 490 NEXT K
 500 GOTO 520
 510 LET N$(I,J)="[a]"
 520 IF RND<=F THEN LET N$(I,J)="[a]"
 530 NEXT J
 540 PRINT AT I,0;"[ ]"
 550 NEXT I
 552 LET G=G+1
 554 LET N=G
 556 GOSUB 1000
 558 PRINT AT 21,12;I$
 560 LET T=0
 570 FOR I=1 TO 20
 575 PRINT AT I,31;"[<]"
 580 FOR J=1 TO 30
 590 IF N$(I,J)<>"[a]" AND N$(I,J)<>" " THEN LET T=T+1
 600 NEXT J
 610 LET F$(I)=N$(I)
 620 PRINT AT I,1;F$(I)
 625 PRINT AT I,31;"[ ]"
 630 GOTO 300
1000 LET S$=STR$ N
1010 LET I$=""
1020 FOR K=1 TO LEN S$
1030 LET I$=I$+CHR$ (128+CODE S$(K))
1040 NEXT K
1050 RETURN
```

Screenshot [http://www.edmundgriffiths.com/zx81forest.jpg here].

=
## Visual Basic .NET
=

This program sits behind a Windows form with fixed borders, the only component of which is a timer (named Timer1, set to something like 50 or 100ms depending on the speed the user wants to see it).  Other constant values (the probabilities and the window dimensions) can be set at the top of the code.


```vbnet
Public Class ForestFire
    Private _forest(,) As ForestState
    Private _isBuilding As Boolean
    Private _bm As Bitmap
    Private _gen As Integer
    Private _sw As Stopwatch

    Private Const _treeStart As Double = 0.5
    Private Const _f As Double = 0.00001
    Private Const _p As Double = 0.001

    Private Const _winWidth As Integer = 300
    Private Const _winHeight As Integer = 300

    Private Enum ForestState
        Empty
        Burning
        Tree
    End Enum

    Private Sub ForestFire_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Me.ClientSize = New Size(_winWidth, _winHeight)
        ReDim _forest(_winWidth, _winHeight)

        Dim rnd As New Random()
        For i As Integer = 0 To _winHeight - 1
            For j As Integer = 0 To _winWidth - 1
                _forest(j, i) = IIf(rnd.NextDouble <= _treeStart, ForestState.Tree, ForestState.Empty)
            Next
        Next

        _sw = New Stopwatch
        _sw.Start()
        DrawForest()
        Timer1.Start()
    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        If _isBuilding Then Exit Sub

        _isBuilding = True
        GetNextGeneration()

        DrawForest()
        _isBuilding = False
    End Sub

    Private Sub GetNextGeneration()
        Dim forestCache(_winWidth, _winHeight) As ForestState
        Dim rnd As New Random()

        For i As Integer = 0 To _winHeight - 1
            For j As Integer = 0 To _winWidth - 1
                Select Case _forest(j, i)
                    Case ForestState.Tree
                        If forestCache(j, i) <> ForestState.Burning Then
                            forestCache(j, i) = IIf(rnd.NextDouble <= _f, ForestState.Burning, ForestState.Tree)
                        End If

                    Case ForestState.Burning
                        For i2 As Integer = i - 1 To i + 1
                            If i2 = -1 OrElse i2 >= _winHeight Then Continue For
                            For j2 As Integer = j - 1 To j + 1
                                If j2 = -1 OrElse i2 >= _winWidth Then Continue For
                                If _forest(j2, i2) = ForestState.Tree Then forestCache(j2, i2) = ForestState.Burning
                            Next
                        Next
                        forestCache(j, i) = ForestState.Empty

                    Case Else
                        forestCache(j, i) = IIf(rnd.NextDouble <= _p, ForestState.Tree, ForestState.Empty)
                End Select
            Next
        Next

        _forest = forestCache
        _gen += 1
    End Sub

    Private Sub DrawForest()
        Dim bmCache As New Bitmap(_winWidth, _winHeight)

        For i As Integer = 0 To _winHeight - 1
            For j As Integer = 0 To _winWidth - 1
                Select Case _forest(j, i)
                    Case ForestState.Tree
                        bmCache.SetPixel(j, i, Color.Green)

                    Case ForestState.Burning
                        bmCache.SetPixel(j, i, Color.Red)
                End Select
            Next
        Next

        _bm = bmCache
        Me.Refresh()
    End Sub

    Private Sub ForestFire_Paint(ByVal sender As System.Object, ByVal e As System.Windows.Forms.PaintEventArgs) Handles MyBase.Paint
        e.Graphics.DrawImage(_bm, 0, 0)

        Me.Text = "Gen " & _gen.ToString() & " @ " & (_gen / (_sw.ElapsedMilliseconds / 1000)).ToString("F02") & " FPS: Forest Fire"
    End Sub
End Class
```


=
## ZX Spectrum Basic
=
This isn't a graphical implementation, but it uses a bit of colour to make the display clearer. It runs very slowly. The variable <tt>init</tt> defines the initial likelihood that each square will hold a tree, and can take values between 0 (no trees) and 1 (a tree in every square) inclusive. This can be used to check that the program is running correctly, and using a value of 1 is probably the most dramatic: but it only makes a difference in the short term. After a few generations, any starting configuration using these values of <math>f</math> and <math>p</math> will end up fluctuating around 20% tree cover—sparse woodland, perhaps, rather than true forest.

A screenshot of the program running can be found [http://www.edmundgriffiths.com/spectrumforestfire.jpg here].

```zxbasic
 10 PAPER 6: CLS
 20 DIM n$(20,30)
 30 LET init=.5
 40 LET f=.02
 50 LET p=.05
 60 PAPER 0
 70 FOR i=0 TO 31
 80 PRINT AT 0,i;" "
 90 PRINT AT 21,i;" "
100 NEXT i
110 FOR i=0 TO 21
120 PRINT AT i,0;" "
130 PRINT AT i,31;" "
140 NEXT i
150 INK 7
160 PRINT AT 0,1;"FOREST FIRE   for Rosetta Code"
170 LET generation=0
180 PRINT AT 21,1;"Generation 0"
190 LET trees=0
200 PRINT AT 21,22;"Cover"
210 FOR i=1 TO 20
220 FOR j=1 TO 30
230 IF RND<init THEN PAPER 4: INK 7: PRINT AT i,j;"T": LET trees=trees+1
240 NEXT j
250 NEXT i
260 LET generation=generation+1
270 INK 7
280 PAPER 0
290 PRINT AT 21,12;generation
300 PRINT AT 21,28;"    "
310 PRINT AT 21,28;INT (trees/6+.5);"%"
320 FOR i=1 TO 20
330 FOR j=1 TO 30
340 LET n$(i,j)=SCREEN$ (i,j)
350 IF SCREEN$ (i,j)="B" THEN LET n$(i,j)=" ": GO TO 450
360 IF SCREEN$ (i,j)="T" THEN GO TO 390
370 IF RND<=p THEN LET n$(i,j)="T"
380 GO TO 450
390 FOR k=i-1 TO i+1
400 FOR l=j-1 TO j+1
410 IF SCREEN$ (k,l)="B" THEN LET n$(i,j)="B": LET k=i+2: LET l=j+2
420 NEXT l
430 NEXT k
440 IF RND<=f THEN LET n$(i,j)="B"
450 NEXT j
460 NEXT i
470 LET trees=0
480 FOR i=1 TO 20
490 FOR j=1 TO 30
500 IF n$(i,j)="T" THEN INK 7: PAPER 4: PRINT AT i,j;"T": LET trees=trees+1: GO TO 540
510 IF n$(i,j)="B" THEN INK 6: PAPER 2: PRINT AT i,j;"B": GO TO 540
520 PAPER 6
530 PRINT AT i,j;" "
540 NEXT j
550 NEXT i
560 GO TO 260
```



## Batch File

Accepts command line arguments in the form of <code>m p f i</code>
Where:

```txt

m - length and width of the array
p - probability of a tree growing
f - probability of a tree catching on fire
i - iterations to output

```

Default is <code> 10 50 5 5 </code>

```dos

@echo off
setlocal enabledelayedexpansion

if "%1"=="" (
  call:default
) else (
  call:setargs %*
)

call:createarray
call:fillarray
call:display
echo.
echo  -------------------
echo.

for /l %%i in (1,1,%i%) do (
  echo.
  echo  -------------------
  echo.
  call:evolve
  call:display
)
pause>nul

:default
set m=10
set n=11
set p=50
set f=5
set i=5
exit /b

:setargs
set m=%1
set n=%m%+1
set p=%2
set f=%3
set i=%4
exit /b

:createarray
for /l %%m in (0,1,%n%) do (
  for /l %%n in (0,1,%n%) do (
    set a%%m%%n=0
  )
)
exit /b

:fillarray
for /l %%m in (1,1,%m%) do (
  for /l %%n in (1,1,%m%) do (
    set /a treerandom=!random! %% 101
    if !treerandom! leq %p% set a%%m%%n=T
  )
)
exit /b

:display
for /l %%m in (1,1,%m%) do (
  set "line%%m="
  for /l %%n in (1,1,%m%) do (
    set line%%m=!line%%m! !a%%m%%n!
  )
  set line%%m=!line%%m:0= !
  echo.!line%%m!
)
exit /b

:evolve
for /l %%m in (1,1,%m%) do (
  for /l %%n in (1,1,%m%) do (
    call:nexttick !a%%m%%n! %%m %%n
    set newa%%m%%n=!errorlevel!
  )
)
call:update
exit /b

:nexttick

if %1==0 (
  set /a treerandom=!random! %% 101
  if !treerandom! leq %p% exit /b 1
  exit /b 0
)

if %1==T (
  set /a lowerm=%2-1
  set /a upperm=%2+1
  set /a lowern=%3-1
  set /a uppern=%3+1
  set burn=0
  for /l %%m in (!lowerm!,1,!upperm!) do (
    for /l %%n in (!lowern!,1,!uppern!) do (
      if !a%%m%%n!==# set burn=1
    )
  )
  if !burn!==1 exit /b 2

  set /a burnrandom=!random! %% 101
  if !burnrandom! leq %f% exit /b 2
  exit /b 1
)

if %1==# exit /b 0

:update
for /l %%m in (1,1,%m%) do (
  for /l %%n in (1,1,%m%) do (
    if !newa%%m%%n!==1 set newa%%m%%n=T
    if !newa%%m%%n!==2 set newa%%m%%n=#
    set a%%m%%n=!newa%%m%%n!
  )
)
exit /b

```

'''Sample Default Output'''

```txt

     T T           T
   T T T   T       T
       T T   T
 T T T T T T   T
     T   T       T T
 T     T T T T T T
   T   T     T T   T
 T T       T     T T
   T   T T T   T   T
 T     T T   T   T

 -------------------


 -------------------

 T T # T     T   T #
 T T T T   T   T T T
     T T T   T T T
 T T T T T T   T T T
 T T T   T       T T
 T T T T T T T T T T
   T T T T T T T T T
 T T     T T     T T
 T T T T T T T T   #
 T     T T T T T T

 -------------------

 T #   #     #   #
 T # # # T T T T # #
     T T T   T T T
 T T T T T T   T T T
 T T T T T T     T T
 T T T T T # T T T T
   T T T T T T T T #
 T T T   T T   T # #
 T T T T # T T T
 #     T T T T T #

 -------------------

 #   T             T
 #       # # # #
   T # # # T # # #
 T T T T T T T T T T
 T T T T # # T   T T
 T T T T #   # T # #
 T T T T # # # # #
 T T T   # #   #
 # # T #   # T # T T
   T   # # # T #   T

 -------------------

     T T T T T T   T
                 T
   #       #
 T # # # # # # # # #
 T # T #     # T # #
 T # T #       #
 T T T #
 # # # T           T
     #       #   # T
 T #         #     T

 -------------------

   T T T T T T T   T
     T     T T T T
 T     T           T
 #
 #   #   T     #
 #   #   T T
 # # #       T T   T
       #   T       #
 T     T T T       #
 #   T T         T #

```




## C

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <pthread.h>

#include <SDL.h>

// defaults
#define PROB_TREE 0.55
#define PROB_F 0.00001
#define PROB_P 0.001

#define TIMERFREQ 100

#ifndef WIDTH
#  define WIDTH 640
#endif
#ifndef HEIGHT
#  define HEIGHT 480
#endif
#ifndef BPP
#  define BPP 32
#endif

#if BPP != 32
  #warning This program could not work with BPP different from 32
#endif

uint8_t *field[2], swapu;
double prob_f = PROB_F, prob_p = PROB_P, prob_tree = PROB_TREE;

enum cell_state {
  VOID, TREE, BURNING
};

// simplistic random func to give [0, 1)
double prand()
{
  return (double)rand() / (RAND_MAX + 1.0);
}

// initialize the field
void init_field(void)
{
  int i, j;
  swapu = 0;
  for(i = 0; i < WIDTH; i++)
  {
    for(j = 0; j < HEIGHT; j++)
    {
      *(field[0] + j*WIDTH + i) = prand() > prob_tree ? VOID : TREE;
    }
  }
}

// the "core" of the task: the "forest-fire CA"
bool burning_neighbor(int, int);
pthread_mutex_t synclock = PTHREAD_MUTEX_INITIALIZER;
static uint32_t simulate(uint32_t iv, void *p)
{
  int i, j;

  /*
    Since this is called by SDL, "likely"(*) in a separated
    thread, we try to avoid corrupted updating of the display
    (done by the show() func): show needs the "right" swapu
    i.e. the right complete field. (*) what if it is not so?
    The following is an attempt to avoid unpleasant updates.
   */
  pthread_mutex_lock(&synclock);

  for(i = 0; i < WIDTH; i++) {
    for(j = 0; j < HEIGHT; j++) {
      enum cell_state s = *(field[swapu] + j*WIDTH + i);
      switch(s)
      {
      case BURNING:
	*(field[swapu^1] + j*WIDTH + i) = VOID;
	break;
      case VOID:
	*(field[swapu^1] + j*WIDTH + i) = prand() > prob_p ? VOID : TREE;
	break;
      case TREE:
	if (burning_neighbor(i, j))
	  *(field[swapu^1] + j*WIDTH + i) = BURNING;
	else
	  *(field[swapu^1] + j*WIDTH + i) = prand() > prob_f ? TREE : BURNING;
	break;
      default:
	fprintf(stderr, "corrupted field\n");
	break;
      }
    }
  }
  swapu ^= 1;
  pthread_mutex_unlock(&synclock);
  return iv;
}

// the field is a "part" of an infinite "void" region
#define NB(I,J) (((I)<WIDTH)&&((I)>=0)&&((J)<HEIGHT)&&((J)>=0) \
		 ? (*(field[swapu] + (J)*WIDTH + (I)) == BURNING) : false)
bool burning_neighbor(int i, int j)
{
  return NB(i-1,j-1) || NB(i-1, j) || NB(i-1, j+1) ||
    NB(i, j-1) || NB(i, j+1) ||
    NB(i+1, j-1) || NB(i+1, j) || NB(i+1, j+1);
}


// "map" the field into gfx mem
// burning trees are red
// trees are green
// "voids" are black;
void show(SDL_Surface *s)
{
  int i, j;
  uint8_t *pixels = (uint8_t *)s->pixels;
  uint32_t color;
  SDL_PixelFormat *f = s->format;

  pthread_mutex_lock(&synclock);
  for(i = 0; i < WIDTH; i++) {
    for(j = 0; j < HEIGHT; j++) {
      switch(*(field[swapu] + j*WIDTH + i)) {
      case VOID:
	color = SDL_MapRGBA(f, 0,0,0,255);
	break;
      case TREE:
	color = SDL_MapRGBA(f, 0,255,0,255);
	break;
      case BURNING:
	color = SDL_MapRGBA(f, 255,0,0,255);
	break;
      }
      *(uint32_t*)(pixels + j*s->pitch + i*(BPP>>3)) = color;
    }
  }
  pthread_mutex_unlock(&synclock);
}

int main(int argc, char **argv)
{
  SDL_Surface *scr = NULL;
  SDL_Event event[1];
  bool quit = false, running = false;
  SDL_TimerID tid;

  // add variability to the simulation
  srand(time(NULL));

  // we can change prob_f and prob_p
  // prob_f prob of spontaneous ignition
  // prob_p prob of birth of a tree
  double *p;
  for(argv++, argc--; argc > 0; argc--, argv++)
  {
    if ( strcmp(*argv, "prob_f") == 0 && argc > 1 )
    {
      p = &prob_f;
    } else if ( strcmp(*argv, "prob_p") == 0 && argc > 1 ) {
      p = &prob_p;
    } else if ( strcmp(*argv, "prob_tree") == 0 && argc > 1 ) {
      p = &prob_tree;
    } else  continue;


    argv++; argc--;
    char *s = NULL;
    double t = strtod(*argv, &s);
    if (s != *argv) *p = t;
  }

  printf("prob_f %lf\nprob_p %lf\nratio %lf\nprob_tree %lf\n",
	 prob_f, prob_p, prob_p/prob_f,
	 prob_tree);

  if ( SDL_Init(SDL_INIT_VIDEO|SDL_INIT_TIMER) != 0 ) return EXIT_FAILURE;
  atexit(SDL_Quit);

  field[0] = malloc(WIDTH*HEIGHT);
  if (field[0] == NULL) exit(EXIT_FAILURE);
  field[1] = malloc(WIDTH*HEIGHT);
  if (field[1] == NULL) { free(field[0]); exit(EXIT_FAILURE); }

  scr = SDL_SetVideoMode(WIDTH, HEIGHT, BPP, SDL_HWSURFACE|SDL_DOUBLEBUF);
  if (scr == NULL) {
    fprintf(stderr, "SDL_SetVideoMode: %s\n", SDL_GetError());
    free(field[0]); free(field[1]);
    exit(EXIT_FAILURE);
  }

  init_field();

  tid = SDL_AddTimer(TIMERFREQ, simulate, NULL); // suppose success
  running = true;

  event->type = SDL_VIDEOEXPOSE;
  SDL_PushEvent(event);

  while(SDL_WaitEvent(event) && !quit)
  {
    switch(event->type)
    {
    case SDL_VIDEOEXPOSE:
      while(SDL_LockSurface(scr) != 0) SDL_Delay(1);
      show(scr);
      SDL_UnlockSurface(scr);
      SDL_Flip(scr);
      event->type = SDL_VIDEOEXPOSE;
      SDL_PushEvent(event);
      break;
    case SDL_KEYDOWN:
      switch(event->key.keysym.sym)
      {
      case SDLK_q:
	quit = true;
	break;
      case SDLK_p:
	if (running)
	{
	  running = false;
	  pthread_mutex_lock(&synclock);
	  SDL_RemoveTimer(tid); // ignore failure...
	  pthread_mutex_unlock(&synclock);
	} else {
	  running = true;
	  tid = SDL_AddTimer(TIMERFREQ, simulate, NULL);
	  // suppose success...
	}
	break;
      }
    case SDL_QUIT:
      quit = true;
      break;
    }
  }

  if (running) {
    pthread_mutex_lock(&synclock);
    SDL_RemoveTimer(tid);
    pthread_mutex_unlock(&synclock);
  }
  free(field[0]); free(field[1]);
  exit(EXIT_SUCCESS);
}
```


### Console version

C99. Uncomment srand() for variaty, usleep() for slower speed.

```c
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h> // For time

enum { empty = 0, tree = 1, fire = 2 };
const char *disp[] = {"  ", "\033[32m/\\\033[m", "\033[07;31m/\\\033[m"};
double tree_prob = 0.01, burn_prob = 0.0001;

#define for_x for (int x = 0; x < w; x++)
#define for_y for (int y = 0; y < h; y++)
#define for_yx for_y for_x
#define chance(x) (rand() < RAND_MAX * x)
void evolve(int w, int h)
{
	unsigned univ[h][w], new[h][w];
	for_yx new[y][x] = univ[y][x] = chance(tree_prob) ? tree : empty;

show:	printf("\033[H");
	for_y {
		for_x printf("%s",disp[univ[y][x]]);
		printf("\033[E");
	}
	fflush(stdout);

	for_yx {
		switch (univ[y][x]) {
		case fire:	new[y][x] = empty;
				break;
		case empty:	if (chance(tree_prob)) new[y][x] = tree;
				break;
		default:
			for (int y1 = y - 1; y1 <= y + 1; y1++) {
				if (y1 < 0 || y1 >= h) continue;
				for (int x1 = x - 1; x1 <= x + 1; x1++) {
					if (x1 < 0 || x1 >= w) continue;
					if (univ[y1][x1] != fire) continue;

					new[y][x] = fire;
					goto burn;
				}
			}

			burn:
			if (new[y][x] == tree && chance(burn_prob))
				new[y][x] = fire;
		}
	}

	for_yx { univ[y][x] = new[y][x]; }
	//usleep(100000);
	goto show;
}

int main(int c, char **v)
{
	//srand(time(0));
	int w = 0, h = 0;

	if (c > 1) w = atoi(v[1]);
	if (c > 2) h = atoi(v[2]);
	if (w <= 0) w = 30;
	if (h <= 0) h = 30;

	evolve(w, h);
}
```



## C++

[[File:ForestFireCpp.png|300px]]

```cpp

#include <windows.h>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
enum states { NONE, TREE, FIRE };
const int MAX_SIDE = 500;

//--------------------------------------------------------------------------------------------------
class myBitmap
{
public:
    myBitmap() : pen( NULL ) {}
    ~myBitmap()
    {
	DeleteObject( pen );
	DeleteDC( hdc );
	DeleteObject( bmp );
    }

    bool create( int w, int h )
    {
	BITMAPINFO	bi;
	ZeroMemory( &bi, sizeof( bi ) );

	bi.bmiHeader.biSize	   = sizeof( bi.bmiHeader );
	bi.bmiHeader.biBitCount	   = sizeof( DWORD ) * 8;
	bi.bmiHeader.biCompression = BI_RGB;
	bi.bmiHeader.biPlanes	   = 1;
	bi.bmiHeader.biWidth	   =  w;
	bi.bmiHeader.biHeight	   = -h;

	HDC dc = GetDC( GetConsoleWindow() );
	bmp = CreateDIBSection( dc, &bi, DIB_RGB_COLORS, &pBits, NULL, 0 );
	if( !bmp ) return false;

	hdc = CreateCompatibleDC( dc );
	SelectObject( hdc, bmp );
	ReleaseDC( GetConsoleWindow(), dc );

	width = w; height = h;

	return true;
    }

    void clear()
    {
	ZeroMemory( pBits, width * height * sizeof( DWORD ) );
    }

    void setPenColor( DWORD clr )
    {
	if( pen ) DeleteObject( pen );
	pen = CreatePen( PS_SOLID, 1, clr );
	SelectObject( hdc, pen );
    }

    void saveBitmap( string path )
    {
	BITMAPFILEHEADER fileheader;
	BITMAPINFO	 infoheader;
	BITMAP		 bitmap;
	DWORD		 wb;

	GetObject( bmp, sizeof( bitmap ), &bitmap );

	DWORD* dwpBits = new DWORD[bitmap.bmWidth * bitmap.bmHeight];
	ZeroMemory( dwpBits, bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD ) );
	ZeroMemory( &infoheader, sizeof( BITMAPINFO ) );
	ZeroMemory( &fileheader, sizeof( BITMAPFILEHEADER ) );

	infoheader.bmiHeader.biBitCount = sizeof( DWORD ) * 8;
	infoheader.bmiHeader.biCompression = BI_RGB;
	infoheader.bmiHeader.biPlanes = 1;
	infoheader.bmiHeader.biSize = sizeof( infoheader.bmiHeader );
	infoheader.bmiHeader.biHeight = bitmap.bmHeight;
	infoheader.bmiHeader.biWidth = bitmap.bmWidth;
	infoheader.bmiHeader.biSizeImage = bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD );

	fileheader.bfType    = 0x4D42;
	fileheader.bfOffBits = sizeof( infoheader.bmiHeader ) + sizeof( BITMAPFILEHEADER );
	fileheader.bfSize    = fileheader.bfOffBits + infoheader.bmiHeader.biSizeImage;

	GetDIBits( hdc, bmp, 0, height, ( LPVOID )dwpBits, &infoheader, DIB_RGB_COLORS );

	HANDLE file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
	WriteFile( file, &fileheader, sizeof( BITMAPFILEHEADER ), &wb, NULL );
	WriteFile( file, &infoheader.bmiHeader, sizeof( infoheader.bmiHeader ), &wb, NULL );
	WriteFile( file, dwpBits, bitmap.bmWidth * bitmap.bmHeight * 4, &wb, NULL );
	CloseHandle( file );

	delete [] dwpBits;
    }

    HDC getDC() const     { return hdc; }
    int getWidth() const  { return width; }
    int getHeight() const { return height; }

private:
    HBITMAP bmp;
    HDC	    hdc;
    HPEN    pen;
    void	*pBits;
    int	    width, height;
};
//--------------------------------------------------------------------------------------------------
class forest
{
public:
    forest()
    {
	_bmp.create( MAX_SIDE, MAX_SIDE );
	initForest( 0.05f, 0.005f );
    }

    void initForest( float p, float f )
    {
	_p = p; _f = f;
	seedForest();
    }

    void mainLoop()
    {
	display();
	simulate();
    }

    void setHWND( HWND hwnd ) { _hwnd = hwnd; }

private:
    float probRand() { return ( float )rand() / 32768.0f; }

    void display()
    {
	HDC bdc = _bmp.getDC();
	DWORD clr;

	for( int y = 0; y < MAX_SIDE; y++ )
	{
	    for( int x = 0; x < MAX_SIDE; x++ )
	    {
		switch( _forest[x][y] )
		{
		    case FIRE: clr = 255; break;
		    case TREE: clr = RGB( 0, 255, 0 ); break;
		    default: clr = 0;
		}

		SetPixel( bdc, x, y, clr );
	    }
	}

	HDC dc = GetDC( _hwnd );
	BitBlt( dc, 0, 0, MAX_SIDE, MAX_SIDE, _bmp.getDC(), 0, 0, SRCCOPY );
	ReleaseDC( _hwnd, dc );
    }

    void seedForest()
    {
	ZeroMemory( _forestT, sizeof( _forestT ) );
	ZeroMemory( _forest, sizeof( _forest ) );
	for( int y = 0; y < MAX_SIDE; y++ )
	    for( int x = 0; x < MAX_SIDE; x++ )
		if( probRand() < _p ) _forest[x][y] = TREE;
    }

    bool getNeighbors( int x, int y )
    {
	int a, b;
	for( int yy = -1; yy < 2; yy++ )
	    for( int xx = -1; xx < 2; xx++ )
	    {
		if( !xx && !yy ) continue;
		a = x + xx; b = y + yy;
		if( a < MAX_SIDE && b < MAX_SIDE && a > -1 && b > -1 )
		if( _forest[a][b] == FIRE ) return true;
	    }

	return false;
    }

    void simulate()
    {
	for( int y = 0; y < MAX_SIDE; y++ )
	{
	    for( int x = 0; x < MAX_SIDE; x++ )
	    {
		switch( _forest[x][y] )
		{
		    case FIRE: _forestT[x][y] = NONE; break;
		    case NONE: if( probRand() < _p ) _forestT[x][y] = TREE; break;
		    case TREE: if( getNeighbors( x, y ) || probRand() < _f ) _forestT[x][y] = FIRE;
		}
	    }
	}

	for( int y = 0; y < MAX_SIDE; y++ )
	    for( int x = 0; x < MAX_SIDE; x++ )
		_forest[x][y] = _forestT[x][y];
    }

    myBitmap _bmp;
    HWND     _hwnd;
    BYTE     _forest[MAX_SIDE][MAX_SIDE], _forestT[MAX_SIDE][MAX_SIDE];
    float    _p, _f;
};
//--------------------------------------------------------------------------------------------------
class wnd
{
public:
    int wnd::Run( HINSTANCE hInst )
    {
	_hInst = hInst;
	_hwnd = InitAll();

	_ff.setHWND( _hwnd );
	_ff.initForest( 0.02f, 0.001f );

	ShowWindow( _hwnd, SW_SHOW );
	UpdateWindow( _hwnd );

	MSG msg;
	ZeroMemory( &msg, sizeof( msg ) );
	while( msg.message != WM_QUIT )
	{
	    if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) != 0 )
	    {
		TranslateMessage( &msg );
		DispatchMessage( &msg );
	    }
	    else
	    {
		_ff.mainLoop();
	    }
	}
	return UnregisterClass( "_FOREST_FIRE_", _hInst );
    }
private:
    static int WINAPI wnd::WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
    {
	switch( msg )
	{
	    case WM_DESTROY: PostQuitMessage( 0 ); break;
	    default:
		return DefWindowProc( hWnd, msg, wParam, lParam );
	}
	return 0;
    }

    HWND InitAll()
    {
	WNDCLASSEX wcex;
	ZeroMemory( &wcex, sizeof( wcex ) );
	wcex.cbSize	       = sizeof( WNDCLASSEX );
	wcex.style	       = CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc   = ( WNDPROC )WndProc;
	wcex.hInstance     = _hInst;
	wcex.hCursor       = LoadCursor( NULL, IDC_ARROW );
	wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
	wcex.lpszClassName = "_FOREST_FIRE_";

	RegisterClassEx( &wcex );

	return CreateWindow( "_FOREST_FIRE_", ".: Forest Fire -- PJorente :.", WS_SYSMENU, CW_USEDEFAULT, 0, MAX_SIDE, MAX_SIDE, NULL, NULL, _hInst, NULL );
    }

    HINSTANCE _hInst;
    HWND      _hwnd;
    forest    _ff;
};
//--------------------------------------------------------------------------------------------------
int APIENTRY _tWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow )
{
    srand( GetTickCount() );
    wnd myWnd;
    return myWnd.Run( hInstance );
}
//--------------------------------------------------------------------------------------------------

```


## C#


```c#
using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Threading;
using System.Windows.Forms;

namespace ForestFire
{
    class Program : Form
    {
        private static readonly Random rand = new Random();
        private Bitmap img;

        public Program(int w, int h, int f, int p)
        {
            Size = new Size(w, h);
            StartPosition = FormStartPosition.CenterScreen;

            Thread t = new Thread(() => fire(f, p));
            t.Start();

            FormClosing += (object sender, FormClosingEventArgs e) => { t.Abort(); t = null; };
        }

        private void fire(int f, int p)
        {
            int clientWidth = ClientRectangle.Width;
            int clientHeight = ClientRectangle.Height;
            int cellSize = 10;

            img = new Bitmap(clientWidth, clientHeight);
            Graphics g = Graphics.FromImage(img);

            CellState[,] state = InitializeForestFire(clientWidth, clientHeight);

            uint generation = 0;

            do
            {
                g.FillRectangle(Brushes.White, 0, 0, img.Width, img.Height);
                state = StepForestFire(state, f, p);

                for (int y = 0; y < clientHeight - cellSize; y += cellSize)
                {
                    for (int x = 0; x < clientWidth - cellSize; x += cellSize)
                    {
                        switch (state[y, x])
                        {
                            case CellState.Empty:
                                break;
                            case CellState.Tree:
                                g.FillRectangle(Brushes.DarkGreen, x, y, cellSize, cellSize);
                                break;
                            case CellState.Burning:
                                g.FillRectangle(Brushes.DarkRed, x, y, cellSize, cellSize);
                                break;
                        }
                    }
                }

                Thread.Sleep(500);

                Invoke((MethodInvoker)Refresh);

            } while (generation < uint.MaxValue);

            g.Dispose();
        }

        private CellState[,] InitializeForestFire(int width, int height)
        {
            // Create our state array, initialize all indices as Empty, and return it.
            var state = new CellState[height, width];
            state.Initialize();
            return state;
        }

        private enum CellState : byte
        {
            Empty = 0,
            Tree = 1,
            Burning = 2
        }

        private CellState[,] StepForestFire(CellState[,] state, int f, int p)
        {
            /* Clone our old state, so we can write to our new state
             * without changing any values in the old state. */
            var newState = (CellState[,])state.Clone();

            int numRows = state.GetLength(0);
            int numCols = state.GetLength(1);

            for (int r = 1; r < numRows - 1; r++)
            {
                for (int c = 1; c < numCols - 1; c++)
                {
                    /*
                     * Check the current cell.
                     *
                     * If it's empty, give it a 1/p chance of becoming a tree.
                     *
                     * If it's a tree, check to see if any neighbors are burning.
                     * If so, set the cell's state to burning, otherwise give it
                     * a 1/f chance of combusting.
                     *
                     * If it's burning, set it to empty.
                     */
                    switch (state[r, c])
                    {
                        case CellState.Empty:
                            if (rand.Next(0, p) == 0)
                                newState[r, c] = CellState.Tree;
                            break;

                        case CellState.Tree:
                            if (NeighborHasState(state, r, c, CellState.Burning) || rand.Next(0, f) == 0)
                                newState[r, c] = CellState.Burning;
                            break;

                        case CellState.Burning:
                            newState[r, c] = CellState.Empty;
                            break;
                    }
                }
            }

            return newState;
        }

        private bool NeighborHasState(CellState[,] state, int x, int y, CellState value)
        {
            // Check each cell within a 1 cell radius for the specified value.
            for (int r = -1; r <= 1; r++)
            {
                for (int c = -1; c <= 1; c++)
                {
                    if (r == 0 && c == 0)
                        continue;

                    if (state[x + r, y + c] == value)
                        return true;
                }
            }

            return false;
        }

        protected override void OnPaint(PaintEventArgs e)
        {
            base.OnPaint(e);
            e.Graphics.DrawImage(img, 0, 0);
        }

        [STAThread]
        static void Main(string[] args)
        {
            Application.Run(new Program(w: 500, h: 500, f: 2, p: 5));
        }
    }
}
```



## Ceylon


```ceylon
import ceylon.random { DefaultRandom }

abstract class Cell() of tree | dirt | burning {}
object tree extends Cell() { string => "A"; }
object dirt extends Cell() { string => " "; }
object burning extends Cell() { string => "#"; }

class Forest(Integer width, Integer height, Float f, Float p) {

    value random = DefaultRandom();
    function chance(Float probability) => random.nextFloat() < probability;
    value sparked => chance(f);
    value sprouted => chance(p);

    alias Point => Integer[2];
    interface Row => {Cell*};

    object doubleBufferedGrid satisfies
            Correspondence<Point, Cell> &
            KeyedCorrespondenceMutator<Point, Cell> {

        value grids = [
            Array {
                for (j in 0:height)
                Array {
                    for (i in 0:width)
                    chance(0.5) then tree else dirt
                }
            },
            Array {
                for (j in 0:height)
                Array.ofSize(width, dirt)
            }
        ];

        variable value showFirst = true;
        value currentState => showFirst then grids.first else grids.last;
        value nextState => showFirst then grids.last else grids.first;

        shared void swapStates() => showFirst = !showFirst;

        shared {Row*} rows => currentState;

        shared actual Boolean defines(Point key) =>
                let (x = key[0], y = key[1])
                0 <= x < width && 0 <= y < height;
        shared actual Cell? get(Point key) =>
                let (x = key[0], y = key[1])
                currentState.get(y)?.get(x);

        shared actual void put(Point key, Cell cell) {
            value [x, y] = key;
            nextState.get(y)?.set(x, cell);
        }
    }

    variable value evolutions = 0;
    shared Integer generation => evolutions + 1;

    shared void evolve() {

        evolutions++;

        function firesNearby(Integer x, Integer y) => {
            for (j in y - 1 : 3)
            for (i in x - 1 : 3)
            doubleBufferedGrid[[i, j]]
        }.coalesced.any(burning.equals);

        for(j->row in doubleBufferedGrid.rows.indexed) {
            for(i->cell in row.indexed) {
                switch (cell)
                case (burning) {
                    doubleBufferedGrid[[i, j]] = dirt;
                }
                case (dirt) {
                    doubleBufferedGrid[[i, j]] = sprouted then tree else dirt;
                }
                case (tree) {
                    doubleBufferedGrid[[i, j]] =
                            firesNearby(i, j) || sparked
                            then burning else tree;
                }
            }
        }

        doubleBufferedGrid.swapStates();
    }

    shared void display() {

        void drawLine() => print("-".repeat(width + 2));

        drawLine();
        for (row in doubleBufferedGrid.rows) {
            process.write("|");
            for (cell in row) {
                process.write(cell.string);
            }
            print("|");
        }
        drawLine();
    }
}

shared void run() {

    value forest = Forest(78, 38, 0.02, 0.03);

    while (true) {

        forest.display();

        print("Generation ``forest.generation``");
        print("Press enter for next generation or q and then enter to quit");

        value input = process.readLine();
        if (exists input, input.trimmed.lowercased == "q") {
            return;
        }

        forest.evolve();
    }
}
```



## Clojure


```Clojure

(def burn-prob 0.1)
(def new-tree-prob 0.5)

(defn grow-new-tree? [] (> new-tree-prob (rand)))
(defn burn-tree? [] (> burn-prob (rand)))
(defn tree-maker [] (if (grow-new-tree?) :tree :grass))

(defn make-forest
  ([] (make-forest 5))
  ([size]
  (take size (repeatedly #(take size (repeatedly tree-maker))))))

(defn tree-at [forest row col] (try (-> forest
                                   (nth row)
                                   (nth col))
                                    (catch Exception _ false)))

(defn neighbores-burning? [forest row col]
  (letfn [(burnt? [row col] (= :burnt (tree-at forest row col)))]
    (or
     (burnt? (inc row) col)
     (burnt? (dec row) col)
     (burnt? row (inc col))
     (burnt? row (dec col)))))

(defn lightning-strike [forest]
  (map (fn [forest-row]
         (map #(if (and (= % :tree) (burn-tree?))
                 :fire!
                 %)
              forest-row)
         )
       forest))

(defn burn-out-trees [forest]
  (map (fn [forest-row]
         (map #(case %
              :burnt :grass
              :fire! :burnt
              %)
              forest-row))
       forest))

(defn burn-neighbores [forest]
  (let [forest-size (count forest)
        indicies (partition forest-size (for [row (range forest-size) col (range forest-size)] (cons row (list col))))]
    (map (fn [forest-row indicies-row]
           (map #(if (and
                       (= :tree %)
                       (neighbores-burning? forest (first %2) (second %2)))
                    :fire!
                    %)
                forest-row indicies-row))
         forest indicies)))

(defn grow-new-trees [forest] (map (fn [forest-row]
                                     (map #(if (= % :grass)
                                             (tree-maker)
                                             %)
                                          forest-row))
                                     forest))

(defn forest-fire
  ([] (forest-fire 5))
  ([forest-size]
  (loop
      [forest (make-forest forest-size)]
    (pprint forest)
    (Thread/sleep 300)
    (-> forest
        (burn-out-trees)
        (lightning-strike)
        (burn-neighbores)
        (grow-new-trees)
        (recur)))))

(forest-fire)


```


example output

```txt

((:tree :tree :grass :tree :tree)
 (:tree :grass :tree :tree :tree)
 (:fire! :tree :tree :grass :tree)
 (:fire! :fire! :tree :tree :tree)
 (:burnt :tree :tree :fire! :grass))

((:tree :tree :grass :tree :tree)
 (:fire! :tree :tree :fire! :tree)
 (:burnt :fire! :tree :grass :tree)
 (:burnt :burnt :fire! :fire! :tree)
 (:grass :fire! :fire! :burnt :tree))

```



## COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. forest-fire.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       *> Probability represents a fraction of 10000.
       *> For instance, IGNITE-PROB means a tree has a 1 in 10000 chance
       *> of igniting.
       78  IGNITE-PROB                 VALUE 1.
       78  NEW-TREE-PROB               VALUE 100.

       78  EMPTY-PROB                  VALUE 3333.

       78  AREA-SIZE                   VALUE 40.

       01  sim-table.
           03  sim-row OCCURS AREA-SIZE TIMES INDEXED BY row-index.
               05  sim-area OCCURS AREA-SIZE TIMES
                   INDEXED BY col-index.
                   07  current-status  PIC 9.
                       *> The flags correspond to the colours they will
                       *> be displayed as.
                       88  empty       VALUE 0. *> Black
                       88  tree        VALUE 2. *> Green
                       88  burning     VALUE 4. *> Red

                   07  next-status     PIC 9.
                       88  empty       VALUE 0.
                       88  tree        VALUE 2.
                       88  burning     VALUE 4.

       01  rand-num                    PIC 9999.

       01  next-row                    PIC 9(4).
       01  next-col                    PIC 9(4).

       01  neighbours-row              PIC 9(4).
       01  neighbours-col              PIC 9(4).

       PROCEDURE DIVISION.
       main-line.
           *> Seed RANDOM with current time.
           MOVE FUNCTION RANDOM(FUNCTION CURRENT-DATE (9:8)) TO rand-num

           PERFORM initialise-table
           PERFORM FOREVER
               PERFORM show-simulation
               PERFORM step-simulation
           END-PERFORM

           GOBACK
           .

       initialise-table.
           PERFORM VARYING row-index FROM 1 BY 1
                   UNTIL AREA-SIZE < row-index
                   AFTER col-index FROM 1 BY 1
                       UNTIL AREA-SIZE < col-index
               PERFORM get-rand-num
               IF rand-num <= EMPTY-PROB
                   SET empty OF current-status (row-index, col-index)
                       TO TRUE
                   SET empty OF next-status (row-index, col-index)
                       TO TRUE
               ELSE
                   SET tree OF current-status (row-index, col-index)
                       TO TRUE
                   SET tree OF next-status (row-index, col-index)
                       TO TRUE
               END-IF
           END-PERFORM
           .

       show-simulation.
           PERFORM VARYING row-index FROM 1 BY 1
                   UNTIL AREA-SIZE < row-index
                   AFTER col-index FROM 1 BY 1
                       UNTIL AREA-SIZE < col-index
                DISPLAY SPACE AT LINE row-index COLUMN col-index
                    WITH BACKGROUND-COLOR
                        current-status (row-index, col-index)
           END-PERFORM
           .

       *> Updates the simulation.
       step-simulation.
            PERFORM VARYING row-index FROM 1 BY 1
                   UNTIL AREA-SIZE < row-index
                   AFTER col-index FROM 1 BY 1
                       UNTIL AREA-SIZE < col-index
                EVALUATE TRUE
                    WHEN empty OF current-status (row-index, col-index)
                        PERFORM get-rand-num
                        IF rand-num <= NEW-TREE-PROB
                            SET tree OF next-status
                                 (row-index, col-index) TO TRUE
                        END-IF

                    WHEN tree OF current-status (row-index, col-index)
                        PERFORM simulate-tree

                    WHEN burning OF current-status
                            (row-index, col-index)
                        SET empty OF next-status (row-index, col-index)
                            TO TRUE
                END-EVALUATE
            END-PERFORM

            PERFORM update-statuses.
            .

       *> Updates a tree tile, assuming row-index and col-index are at
       *> a tree area.
       simulate-tree.
           *> Find the row and column of the bottom-right neighbour.
           COMPUTE next-row = FUNCTION MIN(row-index + 1, AREA-SIZE)
           COMPUTE next-col = FUNCTION MIN(col-index + 1, AREA-SIZE)

           COMPUTE neighbours-row = FUNCTION MAX(row-index - 1, 1)
           COMPUTE neighbours-col = FUNCTION MAX(col-index - 1, 1)

           *> If a neighbour is burning, catch fire.
           PERFORM VARYING neighbours-row FROM neighbours-row BY 1
                   UNTIL next-row < neighbours-row
               *> Check if neighbours in a row are on fire.
               PERFORM VARYING neighbours-col FROM neighbours-col BY 1
                       UNTIL next-col < neighbours-col
                   IF neighbours-row = row-index
                           AND neighbours-col = col-index
                       EXIT PERFORM CYCLE
                   END-IF

                   IF burning OF current-status
                           (neighbours-row, neighbours-col)
                       SET burning OF next-status (row-index, col-index)
                           TO TRUE
                       EXIT PARAGRAPH
                   END-IF
               END-PERFORM

               *> Move neighbours-col back to starting position
               COMPUTE neighbours-col =
                   FUNCTION MAX(neighbours-col - 3, 1)
           END-PERFORM

           *> Otherwise, there is a random chance of
           *> catching fire.
           PERFORM get-rand-num
           IF rand-num <= IGNITE-PROB
               SET burning OF next-status (row-index, col-index) TO TRUE
           END-IF
           .

       update-statuses.
           PERFORM VARYING row-index FROM 1 BY 1
                   UNTIL AREA-SIZE < row-index
                   AFTER col-index FROM 1 BY 1
                       UNTIL AREA-SIZE < col-index
               MOVE next-status (row-index, col-index)
                   TO current-status (row-index, col-index)
           END-PERFORM
           .

       *> Puts a random value between 0 and 9999 in rand-num.
       get-rand-num.
           COMPUTE rand-num =
               FUNCTION MOD(FUNCTION RANDOM * 100000, 10000)
           .
```



## Common Lisp


```lisp
(defvar *dims* '(10 10))
(defvar *prob-t* 0.5)
(defvar *prob-f* 0.1)
(defvar *prob-p* 0.01)

(defmacro with-gensyms (names &body body)
  `(let ,(mapcar (lambda (n) (list n '(gensym))) names)
	 ,@body))

(defmacro traverse-grid (grid rowvar colvar (&rest after-cols) &body body)
  (with-gensyms (dims rows cols)
	`(let* ((,dims (array-dimensions ,grid))
		(,rows (car ,dims))
		(,cols (cadr ,dims)))
	   (dotimes (,rowvar ,rows ,grid)
		 (dotimes (,colvar ,cols ,after-cols)
		   ,@body)))))

(defun make-new-forest (&optional (dims *dims*))
  (let ((forest (make-array dims :element-type 'symbol :initial-element 'void)))
	(traverse-grid forest row col nil
	  (if (<= (random 1.0) *prob-t*)
		  (setf (aref forest row col) 'tree)))))

(defun print-forest (forest)
  (traverse-grid forest row col (terpri)
	(ecase (aref forest row col)
	  ((void) (write-char #\space))
	  ((tree) (write-char #\T))
	  ((fire) (write-char #\#))))
  (values))

(defvar *neighboring* '((-1 . -1) (-1 . 0) (-1 . 1)
			(0 . -1)           (0 . 1)
			(1 . -1)  (1 . 0)  (1 . 1)))

(defun neighbors (forest row col)
  (loop for n in *neighboring*
	for nrow = (+ row (car n))
        for ncol = (+ col (cdr n))
	when (array-in-bounds-p forest nrow ncol)
	collect (aref forest nrow ncol)))

(defun evolve-tree (forest row col)
  (let ((tree (aref forest row col)))
	(cond ((eq tree 'fire) ;; if the tree was on fire, it's dead Jim
		   'void)
	      ((and (eq tree 'tree) ;; if a neighbor is on fire, it's on fire too
		    (find 'fire (neighbors forest row col) :test #'eq))
		   'fire)
	      ((and (eq tree 'tree) ;; random chance of fire happening
		    (<= (random 1.0) *prob-f*))
		   'fire)
	      ((and (eq tree 'void) ;; random chance of empty space becoming a tree
		    (<= (random 1.0) *prob-p*))
		   'tree)
		  (t tree))))

(defun evolve-forest (forest)
  (let* ((dims (array-dimensions forest))
	 (new (make-array dims :element-type 'symbol :initial-element 'void)))
	(traverse-grid forest row col nil
	  (setf (aref new row col) (evolve-tree forest row col)))
	new))

(defun simulate (forest n &optional (print-all t))
  (format t "------ Initial forest ------~%")
  (print-forest forest)
  (dotimes (i n)
    (setf forest (evolve-forest forest))
    (when print-all
      (progn (format t "~%------ Generation ~d ------~%" (1+ i))
 	(print-forest forest)))))

```

Example results:

```lisp
CL-USER>(defparameter *forest* (make-new-forest))
CL-USER>(simulate *forest* 5)
------ Initial forest ------
TTTTT   TT
   TTT  TT
 TT T  T
 TTTT T TT
T TT  T  T
    T  TTT
  TTTT TTT
 T
 T T T T
TTT TTT  T

------ Generation 1 ------
TTTTT   TT
   TTT  TT
 TT T  T
 TTTT T TT
T TT  T  T
    T  TTT
  TTTT TTT
 T
 T T T T
TTT TTT  T

------ Generation 2 ------
TTTTT   TT
   TTT  TT
 TT T  T
 TTTT T TT
TTTT  T  T
    T  TTT
  TTT# TTT
 T
 T T T T
TTT TTT  T

------ Generation 3 ------
TTTTT   TT
   TTT  TT
 TT T  T
 TTTT T TT
TTTT  T  T
    #  TTT
  TT#  TTT
 T
 T T T T
TTT TTT  T

------ Generation 4 ------
TTTTT   TT
   TTT  TT
 TT T  TT
 TTTT T TT
TTT#  T  T
       TTT
  T#   TTT
 T
 T T T T
TTT TTT  T

------ Generation 5 ------
TTTTT   TT
   TTT  TT
 TT T  TT
 T### T TT
TT#   T  T
       TTT
  #    TTT
 T
 T T T T
TTT TTT  T
NIL

```



## D


### Textual Version


```d
import std.stdio, std.random, std.string, std.algorithm;

enum treeProb = 0.55; // Original tree probability.
enum fProb =    0.01; // Auto combustion probability.
enum cProb =    0.01; // Tree creation probability.

enum Cell : char { empty=' ', tree='T', fire='#' }
alias World = Cell[][];

bool hasBurningNeighbours(in World world, in ulong r, in ulong c)
pure nothrow @safe @nogc {
    foreach (immutable rowShift; -1 .. 2)
        foreach (immutable colShift; -1 .. 2)
            if ((r + rowShift) >= 0 && (r + rowShift) < world.length &&
                (c + colShift) >= 0 && (c + colShift) < world[0].length &&
                world[r + rowShift][c + colShift] == Cell.fire)
                return true;
    return false;
}

void nextState(in World world, World nextWorld) /*nothrow*/ @safe /*@nogc*/ {
    foreach (r, row; world)
        foreach (c, elem; row)
            final switch (elem) with (Cell) {
                case empty:
                    nextWorld[r][c]= (uniform01 < cProb) ? tree : empty;
                    break;

                case tree:
                    if (world.hasBurningNeighbours(r, c))
                        nextWorld[r][c] = fire;
                    else
                        nextWorld[r][c] = (uniform01 < fProb) ? fire : tree;
                    break;

                case fire:
                    nextWorld[r][c] = empty;
                    break;
            }
}

void main() @safe {
    auto world = new World(8, 65);
    foreach (row; world)
        foreach (ref el; row)
            el = (uniform01 < treeProb) ? Cell.tree : Cell.empty;
    auto nextWorld = new World(world.length, world[0].length);

    foreach (immutable i; 0 .. 4) {
        nextState(world, nextWorld);
        writefln("%(%(%c%)\n%)\n", nextWorld);
        world.swap(nextWorld);
    }
}
```

```txt
  T    T T#TT  T   TT  TT TTTT TT TTT T TT T# T T TT TT     TTTTT
T TT  TT T    TTTTTTTTTT T TTT T T    T    TT    TTTTTTTT TTTT #T
TT  T  TTTTTT TTTTT       TTT TTTT TTTT TTT T  T T T T  TT T TT
T TT T TT T TT T  TTTT   T T TT TTT    T  TT     T T   T TT    T
 TTT   T  TTTT  T#  T T T  TTT  TT  TTTTT T      T  TT  T  T TT T
 TT TTTT  TTT  TTTTT T T T  T  TT  T TTT   T  T T   TT    TTT T T
 T  TTT T TT   T TTT#TT  T TT  TTTTTTTT  TTTT  TTTTT TTTT TTT
TT TTTTT TTTTTT TT  TT T TT T   TT  T   TT T TT TT  TTTT   TTTTT

  T    T # #T  T   TT  TT TTTT TT TTT T TT #  T T TT TT     TT###
T TT  TT #    TTTTTTTTTT T TTT T T    T    ##    TTTTTTTT TTTT  #
TT  T  TTTTTT TTTTT       TTT TTTT TTTT TTT #  T T T T  TT T T#
T TT T TT T TT T  #TTT   T T TT TTT    T  TT     T T   T TTT   T
 TTT   T  TTTT  #   T T T  TTT  TT  TTTTT T      T  TT  T  T TT T
 TT TTTT  TTT  T#### # T T  T  TT  T TTT   T  T T   TT    TTT T T
 T  TTT T TT   T TT# #T  T TT  TTTTTTTT  TTTT  TTTTT TTTT TTT
TT TTTTT TTTTTT TT  ## T TT T   TT  T   TT T TT TT  TTTT   TTTTT

  T    T    #  T   TT  TT TTTT TT TTT T TT    T T TT TT     T#
T TT  TT      TTTTTTTTTT T TTT T T    T          TTTTTTTT TTT#
TT  T  T###TT TTT##       TTT TTTT TTTT TT#    T T T T  #T T #
T TT T TT T TT #   #TT   T T TT TTT    T  T#     T T   T TTT   #
 TTT   T  TTTT      # # T  TTT  TT  TTTTT T      T  TT  T  T TT T
 TT TTTT  TTT  #       T T  #  TT  T TTT   T  T T   TT    TTT T T
 T  TTT T TT   # ##   #  T TT  TTTTTTTT  TTTT  TTTTT TTTT TTT
TT TTTTT TT#TTT TT     T TT T   TT  T   TT T TT TT  TTTT   TTTTT

  T    T       T   TT  TT TTTT TT TTT T TT    T T TT TT     #
T TT  T#      TT####TTTT T TTT T T    T          TTTTTT## TT#
TT  T  #   #T ###         TTT TTTT TTTT T#     T T T T   # T
T TT T ## # TT      ##   T T TT TTT    T  #      T T   # #TT
 TTT   T  TTTT          T  ###  TT  TTTTT #      T  TT  T  T T# #
 TT TTTT  TTT          # T     TT  T TTT   T  T T   TT    TTT T T
 T  TTT T ##             T ##  TTTTTTTT  TTTT  TTTTT TTTT TTT
TT TTTTT T# #T# ##     # TT T   TT  T   TT T TT TT  TTTT   TTTTT

```



### Graphical Version

```d
import std.stdio, std.random, std.algorithm, std.typetuple,
       simpledisplay;

enum double TREE_PROB = 0.55; // Original tree probability.
enum double F_PROB =    0.01; // Auto combustion probability.
enum double P_PROB =    0.01; // Tree creation probability.
enum worldSide = 600;

enum Cell : ubyte { empty, tree, burning }
alias World = Cell[worldSide][];

immutable white = Color(255, 255, 255),
          red = Color(255, 0, 0),
          green = Color(0, 255, 0);

void nextState(ref World world, ref World nextWorld,
               ref Xorshift rnd, Image img) {
  immutable nr = world.length;
  immutable nc = world[0].length;
  foreach (immutable r, const row; world)
    foreach (immutable c, immutable elem; row)
      START: final switch (elem) with (Cell) {
        case empty:
          img.putPixel(c, r, white);
          nextWorld[r][c] = rnd.uniform01 < P_PROB ? tree : empty;
          break;

        case tree:
          img.putPixel(c, r, green);

          foreach (immutable rowShift; TypeTuple!(-1, 0, 1))
            foreach (immutable colShift; TypeTuple!(-1, 0, 1))
              if ((r + rowShift) >= 0 && (r + rowShift) < nr &&
                  (c + colShift) >= 0 && (c + colShift) < nc &&
                  world[r + rowShift][c + colShift] == Cell.burning) {
                nextWorld[r][c] = Cell.burning;
                break START;
              }

          nextWorld[r][c]= rnd.uniform01 < F_PROB ? burning : tree;
          break;

        case burning:
          img.putPixel(c, r, red);
          nextWorld[r][c] = empty;
          break;
      }

  swap(world, nextWorld);
}

void main() {
  auto rnd = Xorshift(1);
  auto world = new World(worldSide);
  foreach (ref row; world)
    foreach (ref el; row)
      el = rnd.uniform01 < TREE_PROB ? Cell.tree : Cell.empty;
  auto nextWorld = new World(world[0].length);

  auto w= new SimpleWindow(world.length,world[0].length,"ForestFire");
  auto img = new Image(w.width, w.height);

  w.eventLoop(1, {
    auto painter = w.draw;
    nextState(world, nextWorld, rnd, img);
    painter.drawImage(Point(0, 0), img);
  });
}
```


=={{header|Déjà Vu}}==

```dejavu
#chance of empty->tree
set :p 0.004
#chance of spontaneous tree combustion
set :f 0.001
#chance of tree in initial state
set :s 0.5
#height of world
set :H 10
#width of world
set :W 20

has-burning-neigbour state pos:
	for i range -- swap ++ dup &< pos:
		for j range -- swap ++ dup &> pos:
			& i j
			try:
				state!
			catch value-error:
				:empty
			if = :burning:
				return true
	false

evolve state pos:
	state! pos
	if = :tree dup:
		if has-burning-neigbour state pos:
			:burning drop
		elseif chance f:
			:burning drop
	elseif = :burning:
		:empty
	else:
		if chance p:
			:tree
		else:
			:empty

step state:
	local :next {}
	for k in keys state:
		set-to next k evolve state k
	next

local :(c) { :tree "T" :burning "B" :empty "." }
print-state state:
	for j range 0 H:
		for i range 0 W:
			!print\ (c)! state! & i j
		!print ""

init-state:
	local :first {}
	for j range 0 H:
		for i range 0 W:
			if chance s:
				:tree
			else:
				:empty
			set-to first & i j
	first

run:
	init-state
	while true:
		print-state dup
		!print ""
		step

run-slowly:
	init-state
	while true:
		print-state dup
		drop !prompt "Continue."
		step

run
```

```txt
T.T.T...T..T..TT.T.T.
.TT.T...T..T....TTTTT
......T.TTT.TTTTT....
..TTT...T.T..T..TTT..
....T.....TTT...TTTTT
..TTT..TTTTTTTTT....T
T....T..TT.TT.T...T..
TTT.TT.T..TT.TTT.TT..
.TT.TT.T...T.T..T.TTT
..TTTTT...TTTTTT..T.T
TT..T....T..T..TTTT..

TTT.T...T..T.TTT.T.T.
.TT.T...T..T..T.TTTTT
......T.TTT.TTTTT....
..TTT...T.T..T..TTT..
....TT....TTB...TTTTT
..TTT..TTTTTTTTT...TT
T....T.TTT.TT.T...T..
TTT.TT.T..TB.TTT.TT..
.TT.TT.T...T.T..T.TTT
..TTTTB...TTTTTT..T.T
TT..T....T..T..TTTT..

TTT.T...T..T.TTT.T.T.
.TTTT...T..T..T.TTTTT
......T.TTTTTTTTT....
..TTT...T.T..B..TTT..
....TT....TB....TTTTT
..TTT..TTTTBBBTT...TT
T....T.TTT.BB.T...T..
TTT.TT.T..B..TTT.TT..
.TTTTB.B...B.T..T.TTT
..TTTB....TTTTTT..T.T
TT..T....T..T..TTTT..
```



## EasyLang


[https://easylang.online/apps/forest-fire.html Run it]

<lang>subr init
  for r range 100
    for c range 100
      i = r * 102 + c + 103
      p[i] = -1
      if randomf < 0.55
        f[i] = 1
      else
        f[i] = 0
      .
    .
  .
  timer 0
.
subr show
  for r range 100
    for c range 100
      i = r * 102 + c + 103
      h = f[i]
      if h <> p[i]
        move c r
        if h = 0
          color 210
        elif h = 1
          color 161
        else
          color 960
        .
        rect 1 1
      .
    .
  .
.
subr update
  swap f[] p[]
  for r range 100
    for c range 100
      i = r * 102 + c + 103
      if p[i] = 0
        f[i] = 0
        if randomf < 0.003
          f[i] = 1
        .
      elif p[i] = 10
        f[i] = 0
      else
        f[i] = 1
        s = 0
        s += p[i - 103] + p[i - 102] + p[i - 101]
        s += p[i - 1] + p[i + 1]
        s += p[i + 101] + p[i + 102] + p[i + 103]
        if s >= 10 or randomf < 0.00001
          f[i] = 10
        .
      .
    .
  .
.
on timer
  call show
  call update
  timer 0.2
.
len f[] 102 * 102
len p[] len f[]
call init
```



## Emacs Lisp


```lisp
#!/usr/bin/env emacs -script
;; -*- lexical-binding: t -*-
;; run: ./forest-fire forest-fire.config
(require 'cl-lib)
;; (setq debug-on-error t)

(defmacro swap (a b)
  `(setq ,b (prog1 ,a (setq ,a ,b))))

(defconst burning ?B)
(defconst tree ?t)

(cl-defstruct world rows cols data)

(defun new-world (rows cols)
  ;; When allocating the vector add padding so the border will always be empty.
  (make-world :rows rows :cols cols :data (make-vector (* (1+ rows) (1+ cols)) nil)))

(defmacro world--rows (w)
  `(1+ (world-rows ,w)))

(defmacro world--cols (w)
  `(1+ (world-cols ,w)))

(defmacro world-pt (w r c)
  `(+ (* (mod ,r (world--rows ,w)) (world--cols ,w))
      (mod ,c (world--cols ,w))))

(defmacro world-ref (w r c)
  `(aref (world-data ,w) (world-pt ,w ,r ,c)))

(defun print-world (world)
  (dotimes (r (world-rows world))
    (dotimes (c (world-cols world))
      (let ((cell (world-ref world r c)))
        (princ (format "%c" (if (not (null cell))
                   cell
                 ?.)))))
    (terpri)))

(defun random-probability ()
  (/ (float (random 1000000)) 1000000))

(defun initialize-world (world p)
  (dotimes (r (world-rows world))
    (dotimes (c (world-cols world))
      (setf (world-ref world r c) (if (<= (random-probability) p) tree nil)))))

(defun neighbors-burning (world row col)
  (let ((n 0))
    (dolist (offset '((1 . 1) (1 . 0) (1 . -1) (0 . 1) (0 . -1) (-1 . 1) (-1 . 0) (-1 . -1)))
      (when (eq (world-ref world (+ row (car offset)) (+ col (cdr offset))) burning)
        (setq n (1+ n))))
    (> n 0)))

(defun advance (old new p f)
  (dotimes (r (world-rows old))
    (dotimes (c (world-cols old))
      (cond
       ((eq (world-ref old r c) burning)
        (setf (world-ref new r c) nil))
       ((null (world-ref old r c))
        (setf (world-ref new r c) (if (<= (random-probability) p) tree nil)))
       ((eq (world-ref old r c) tree)
        (setf (world-ref new r c) (if (or (neighbors-burning old r c)
                                          (<= (random-probability) f))
                                      burning
                                    tree)))))))

(defun read-config (file-name)
  (with-temp-buffer
    (insert-file-contents-literally file-name)
    (read (current-buffer))))

(defun get-config (key config)
  (let ((val (assoc key config)))
    (if (null val)
        (error (format "missing value for %s" key))
      (cdr val))))

(defun simulate-forest (file-name)
  (let* ((config (read-config file-name))
         (rows (get-config 'rows config))
         (cols (get-config 'cols config))
         (skip (get-config 'skip config))
         (a (new-world rows cols))
         (b (new-world rows cols)))
    (initialize-world a (get-config 'tree config))
    (dotimes (time (get-config 'time config))
      (when (or (and (> skip 0) (= (mod time skip) 0))
                (<= skip 0))
        (princ (format "* time %d\n" time))
        (print-world a))
      (advance a b (get-config 'p config) (get-config 'f config))
      (swap a b))))

(simulate-forest (elt command-line-args-left 0))

```


The configuration file controls the simulation.

```lisp
((rows . 10)
 (cols . 45)
 (time . 100)
 (skip . 10)
 (f . 0.001)   ;; probability tree ignites
 (p . 0.01)    ;; probability empty space fills with a tree
 (tree . 0.5)) ;; initial probability of tree in a new world
```

```txt
* time 0
.t...t..t.t.t...ttt...tttt..tt...t.t.t.t.t..t
.t.t.t..t.ttt.tt.tttt.tt....t.t.tt.t.t.tt.ttt
t..t.tttt..t..tt..tt.t.t.tt.....t..t..tt.tt.t
.tt.t.ttt.t...t...tt..t....tttttt.t..tt.tt.tt
.t..t..t.tt.t...tt...t.t.tt.t.t..ttttt.t..ttt
.tt.ttttt..t.t....tttt.t.t..tttttt.tt.t.t.t.t
ttt.....t.tttttttt.tt....ttt.t.....t.ttt..ttt
.tt..tt.tt.ttt...tt.t..ttt.t.tt.tt....tttt...
t.tt...tttt...t.t.tt.tt..ttt...t.tt.t.tttttt.
...t......t.t...tttt...ttttt.tttt..t..t.tttt.
* time 10
......................tttt..tt...t.B........B
....................B.tt...tt.t.tt..........B
.....................t.t.tt.....B...........B
...........t..........t...tttttB............B
.............t..tB...t.t.tt.t.tB.........t..B
.........t.......ttttt.t.tt.tttB.............
................Btttt....ttttt...........t...
t....B...t.......tt.t..ttt.t.tt.BB...........
.....B..........t.ttttt..ttt..tt.tt.t...t....
................tttt...ttttt.tttt..t.........
* time 20
..........t.....t.t................t........t
.....t...........t.t.........................
.........................t..............t....
..........tttt..........................t....
.t.........t.t...........................t...
....t....t......................t............
..t.tt.....t..............t........t.t...t...
tt.......t...................................
..t...t.........................t....t..t.t..
.....t.......................................
* time 30
......t...t.....t.t......t.........t.......tt
.....t.........t.t.t...............t...t.....
...........tt.........t..t..t......t....t....
..........tttt.......t.....t.........t..t....
.t.........t.t...t.tt..........t.........t...
....t....t......t.tt.t..........t.......t....
..t.tt.....t..t.....t.t...t..tB....t.t...t...
tt..t....t.......t..................t........
..t...t....t...........t........t....t..t.t..
.....t...t....t..t....tt.t.....t...........t.
* time 40
......t...t.....t.t......t......t..t.......tt
.....t.........t.t.tt..............tt..t.....
...........tt............t..t......t....tt...
t...t.....tttt.............tt.t.t....t..t....
.t.........t.t......t..........t.........t...
....t....t...t........t.t.......t.t..t..t....
..t.tt.....t..B....t......t....t...t.t...t...
tt..t....t.t.....tt.................t........
..t...t....t..t........tt.t.....t....t..t.t..
..t..t...t....t..t....tt.t.....t......t....t.
* time 50
......t...t.....t.t......t.....tt.t...t....tt
..t.tt.........t.t.tt.t......t.t...t...t.....
.........................t..t..t..t.....tt...
t...t......................tt.t.t....t..t...t
.t..................t.........tt.........t..t
....t....t..........t.t.t.......t.t..t..t....
..tttt...t.t.......t......t....t...ttt...tt.t
tt..t.t..t.t.....tt.................t........
..t...t....t..t....t...tt.t.....t....tt.t.t..
..t..t...t....t..t....tt.t.t...t....t.t....t.
* time 60
......t...t.t...t.t.t....t.....tt.t...t....tt
..t.tt.......t.t.t.tt.t......t.t...t...t..t.t
......t.t...............tt.tt..t..t.....tt...
t...t.t.............t......tt.t.t....t..t...t
tt...............tttt.........tt.........t..t
....tt.ttt..t.......t.t.t.t.....t.t..t..t....
..tttt...ttt.......t......t....t...ttt..ttt.t
tt..t.t.tt.t...t.tt.................t....t...
..tt..t....t..t..t.t...B........t....tt.t.t..
..t..t...t....t..t..t.tB...Bt..t...tt.t....t.
* time 70
......tt..t.t...t.t.t.t..t.....tt.t...t....tt
t.t.tt.......t.t.t.tt.t.....tttt...t...t..t.t
.t....t.t............t..tt.tt..t..t....ttt...
t...t.tt..t.....t...t...t..tt.t.tt...t..t.t.t
tt.....t.........tttt.........tt..t......tt.t
....ttttttt.t...t...t.t.t.t.....t.t..t..t.t..
..tttt...ttt..t....t......t...tt...ttt..ttt.t
ttt.t.t.tttt...t.tt....t.......t....tt..tt...
..tt..ttt.tt..t..ttt.....t......t...ttt.t.t..
..t..tt..t....t.tt..t..........t.t.tt.t...tt.
* time 80
....t.tt.tt.t...t.t.t.t..t....ttt.t.........B
t.t.tt.......t.t.t.tt.t.....ttttt..t........B
.t....t.t............t..tt.tt..t..t..........
t...t.ttt.t.....t...t...t..tt.tttt...t......t
tt.....t.......tttttt....t....ttt.ttt.......t
....ttttttt.t..tt..tt.t.t.t.....t.t..t.......
..tttt...ttt..tt...t......t...tt...ttt......t
ttt.t.t.tttt...t.tt....t......ttt...tt..BB...
..ttt.ttt.tt..t..tttt....tt.....t...ttt.t.t..
..t..tt..t....tttt.tt..........t.t.tt.t...tt.
* time 90
....t.tt.tt.t...t.t.t.t..t........B..........
t.t.tt.......t.ttt.tt.t.............B........
tt....t.t.t.......t..t..tt...................
t...t.ttt.t.....t.t.t.t.t............t..t...t
tt.tt.tt.......tttttt....t..........B...t...t
....ttttttt.t..ttt.tt.ttt.t..........t.....t.
..tttt...ttt..tt.t.t......t........Btt..t.t.t
ttt.t.t.tttt...t.tt..t.t...........ttt.......
..ttt.ttt.tt..t..tttt.t..tt.....BB..ttt......
..t..tt..t....tttt.tt.......t..t.t.tt.t......

```



## Erlang

Not even text graphics. Notice the use of random:seed/1 when creating a tree. Without it all calls to random:uniform/1 gave the same result for each tree.


```Erlang

-module( forest_fire ).

-export( [task/0] ).

-record( state, {neighbours=[], position, probability_burn, probability_grow, tree} ).

task() ->
       erlang:spawn( fun() ->
             Pid_positions = forest_create( 5, 5, 0.5, 0.3, 0.2 ),
             Pids = [X || {X, _} <- Pid_positions],
             [X ! {tree_pid_positions, Pid_positions} || X <- Pids],
             Start = forest_status( Pids ),
             Histories = [Start | [forest_step( Pids ) || _X <- lists:seq(1, 2)]],
             [io:fwrite("~p~n~n", [X]) || X <- Histories]
         end ).



forest_create( X_max, Y_max, Init, Grow, Burn ) ->
       [{tree_create(tree_init(Init, random:uniform()), X, Y, Grow, Burn), {X,Y}} || X <- lists:seq(1, X_max), Y<- lists:seq(1, Y_ma\
x)].

forest_status( Pids ) ->
       [X ! {status_request, erlang:self()} || X <- Pids],
       [receive {status, Tree, Position, X} -> {Tree, Position} end || X <- Pids].

forest_step( Pids ) ->
       [X ! {step} || X <- Pids],
       forest_status( Pids ).

is_neighbour({X, Y}, {X, Y} ) -> false; % Myself
is_neighbour({Xn, Yn}, {X, Y} ) when abs(Xn - X) =< 1, abs(Yn - Y) =< 1 -> true;
is_neighbour( _Position_neighbour, _Position ) -> false.

loop( State ) ->
        receive
        {tree_pid_positions, Pid_positions} ->
                loop( loop_neighbour(Pid_positions, State) );
        {step} ->
               [X ! {tree, State#state.tree, erlang:self()} || X <- State#state.neighbours],
               loop( loop_step(State) );
        {status_request, Pid} ->
                Pid ! {status, State#state.tree, State#state.position, erlang:self()},
                loop( State )
        end.

loop_neighbour(	Pid_positions, State ) ->
	My_position = State#state.position,
        State#state{neighbours=[Pid || {Pid, Position} <- Pid_positions, is_neighbour( Position, My_position)]}.

loop_step( State ) ->
        Is_burning = lists:any( fun loop_step_burning/1, [loop_step_receive(X) || X <- State#state.neighbours] ),
        Tree = loop_step_next( Is_burning, random:uniform(), State ),
        State#state{tree=Tree}.

loop_step_burning( Tree ) -> Tree =:= burning.

loop_step_next( _Is_burning, Probablility, #state{tree=empty, probability_grow=Grow} ) when Grow > Probablility -> tree;
loop_step_next( _Is_burning, _Probablility, #state{tree=empty} ) -> empty;
loop_step_next( _Is_burning, _Probablility, #state{tree=burning} ) -> empty;
loop_step_next( true, _Probablility, #state{tree=tree} ) -> burning;
loop_step_next( false, Probablility, #state{tree=tree, probability_burn=Burn} ) when Burn > Probablility  -> burning;
loop_step_next( false, _Probablility, #state{tree=tree} ) -> tree.

loop_step_receive( Pid ) -> receive {tree, Tree, Pid} -> Tree end.

tree_create( Tree, X, Y, Grow, Burn ) ->
        State = #state{position={X, Y}, probability_burn=Burn, probability_grow=Grow, tree=Tree},
        erlang:spawn_link( fun() -> random:seed( X, Y, 0 ), loop( State ) end ).

tree_init( Tree_probalility, Random ) when Tree_probalility > Random -> tree;
tree_init( _Tree_probalility, _Random ) -> empty.

```


29> forest_fire:task().

```txt

[{tree,{1,1}},
 {empty,{1,2}},
 {empty,{1,3}},
 {empty,{1,4}},
 {tree,{1,5}},
 {empty,{2,1}},
 {empty,{2,2}},
 {empty,{2,3}},
 {tree,{2,4}},
 {empty,{2,5}},
 {tree,{3,1}},
 {tree,{3,2}},
 {empty,{3,3}},
 {tree,{3,4}},
 {empty,{3,5}},
 {tree,{4,1}},
 {tree,{4,2}},
 {tree,{4,3}},
 {tree,{4,4}},
 {empty,{4,5}},
 {tree,{5,1}},
 {tree,{5,2}},
 {tree,{5,3}},
 {tree,{5,4}},
 {empty,{5,5}}]

[{burning,{1,1}},
 {tree,{1,2}},
 {tree,{1,3}},
 {tree,{1,4}},
 {burning,{1,5}},
 {tree,{2,1}},
 {tree,{2,2}},
 {tree,{2,3}},
 {burning,{2,4}},
 {tree,{2,5}},
 {burning,{3,1}},
 {burning,{3,2}},
 {tree,{3,3}},
 {burning,{3,4}},
 {tree,{3,5}},
 {burning,{4,1}},
 {burning,{4,2}},
 {burning,{4,3}},
 {burning,{4,4}},
 {tree,{4,5}},
 {burning,{5,1}},
 {burning,{5,2}},
 {burning,{5,3}},
 {burning,{5,4}},
 {tree,{5,5}}]

[{empty,{1,1}},
 {burning,{1,2}},
 {burning,{1,3}},
 {burning,{1,4}},
 {empty,{1,5}},
 {burning,{2,1}},
 {burning,{2,2}},
 {burning,{2,3}},
 {empty,{2,4}},
 {burning,{2,5}},
 {empty,{3,1}},
 {empty,{3,2}},
 {burning,{3,3}},
 {empty,{3,4}},
 {burning,{3,5}},
 {empty,{4,1}},
 {empty,{4,2}},
 {empty,{4,3}},
 {empty,{4,4}},
 {burning,{4,5}},
 {empty,{5,1}},
 {empty,{5,2}},
 {empty,{5,3}},
 {empty,{5,4}},
 {burning,{5,5}}]

```


=={{header|F_Sharp|F#}}==
This implementation can be compiled or run in the interactive F# shell.

```fsharp
open System
open System.Diagnostics
open System.Drawing
open System.Drawing.Imaging
open System.Runtime.InteropServices
open System.Windows.Forms

module ForestFire =

    type Cell = Empty | Tree | Fire

    let rnd = new System.Random()
    let initial_factor = 0.35
    let ignition_factor = 1e-5 // rate of lightning strikes (f)
    let growth_factor = 2e-3   // rate of regrowth (p)
    let width = 640            // width of the forest region
    let height = 480           // height of the forest region

    let make_forest =
        Array2D.init height width
            (fun _ _ -> if rnd.NextDouble() < initial_factor then Tree else Empty)

    let count (forest:Cell[,]) row col =
        let mutable n = 0
        let h,w = forest.GetLength 0, forest.GetLength 1
        for r in row-1 .. row+1 do
            for c in col-1 .. col+1 do
                if r >= 0 && r < h && c >= 0 && c < w && forest.[r,c] = Fire then
                    n <- n + 1
        if forest.[row,col] = Fire then n-1 else n

    let burn (forest:Cell[,]) r c =
        match forest.[r,c] with
        | Fire -> Empty
        | Tree -> if rnd.NextDouble() < ignition_factor then Fire
                    else if (count forest r c) > 0 then Fire else Tree
        | Empty -> if rnd.NextDouble() < growth_factor then Tree else Empty

    // All the functions below this point are drawing the generated images to screen.
    let make_image (pixels:int[]) =
        let bmp = new Bitmap(width, height)
        let bits = bmp.LockBits(Rectangle(0,0,width,height), ImageLockMode.WriteOnly, PixelFormat.Format32bppArgb)
        Marshal.Copy(pixels, 0, bits.Scan0, bits.Height*bits.Width) |> ignore
        bmp.UnlockBits(bits)
        bmp

    // This function is run asynchronously to avoid blocking the main GUI thread.
    let run (box:PictureBox) (label:Label) = async {
        let timer = new Stopwatch()
        let forest = make_forest |> ref
        let pixel = Array.create (height*width) (Color.Black.ToArgb())
        let rec update gen =
            timer.Start()
            forest := burn !forest |> Array2D.init height width
            for y in 0..height-1 do
                for x in 0..width-1 do
                    pixel.[x+y*width] <- match (!forest).[y,x] with
                                            | Empty -> Color.Gray.ToArgb()
                                            | Tree -> Color.Green.ToArgb()
                                            | Fire -> Color.Red.ToArgb()
            let img = make_image pixel
            box.Invoke(MethodInvoker(fun () -> box.Image <- img)) |> ignore
            let msg = sprintf "generation %d @ %.1f fps" gen (1000./timer.Elapsed.TotalMilliseconds)
            label.Invoke(MethodInvoker(fun () -> label.Text <- msg )) |> ignore
            timer.Reset()
            update (gen + 1)
        update 0 }

    let main args =
        let form = new Form(AutoSize=true,
                            Size=new Size(800,600),
                            Text="Forest fire cellular automata")
        let box = new PictureBox(Dock=DockStyle.Fill,Location=new Point(0,0),SizeMode=PictureBoxSizeMode.StretchImage)
        let label = new Label(Dock=DockStyle.Bottom, Text="Ready")
        form.FormClosed.Add(fun eventArgs -> Async.CancelDefaultToken()
                                             Application.Exit())
        form.Controls.Add(box)
        form.Controls.Add(label)
        run box label |> Async.Start
        form.Show()
        Application.Run()
        0

#if INTERACTIVE
ForestFire.main [|""|]
#else
[<System.STAThread>]
[<EntryPoint>]
let main args = ForestFire.main args
#endif
```

[[File:ForestFire-FSharp.png]]


## Factor

```factor
USING: combinators grouping kernel literals math math.matrices
math.vectors prettyprint random raylib.ffi sequences ;
IN: rosetta-code.forest-fire

! The following private vocab builds up to a useful combinator,
! matrix-map-neighbors, which takes a matrix, a quotation, and
! inside the quotation makes available each element of the
! matrix as well as its neighbors, mapping the result of the
! quotation to a new matrix.

<PRIVATE

CONSTANT: neighbors {
    { -1 -1 } { -1  0 } { -1  1 }
    {  0 -1 }           {  0  1 }
    {  1 -1 } {  1  0 } {  1  1 }
}

: ?i,j ( i j matrix -- elt/f ) swapd ?nth ?nth ;

: ?i,jths ( seq matrix -- newseq )
    [ [ first2 ] dip ?i,j ] curry map ;

: neighbor-coords ( loc -- seq )
    [ neighbors ] dip [ v+ ] curry map ;

: get-neighbors ( loc matrix -- seq )
    [ neighbor-coords ] dip ?i,jths ;

: matrix>neighbors ( matrix -- seq )
    dup dim matrix-coordinates concat
    [ swap get-neighbors sift ] with map ;

: matrix-map-neighbors ( ... matrix quot: ( ... neighbors elt -- ... newelt ) -- ... newmatrix )
    [ [ dim first ] [ matrix>neighbors ] [ concat ] tri ] dip
    2map swap group ; inline

PRIVATE>


! ##### Simulation code #####

! In our forest,
! 0 = empty
! 1 = tree
! 2 = fire

CONSTANT: ignite-probability 1/12000
CONSTANT: grow-probability 1/100

: make-forest ( m n probability -- matrix )
    [ random-unit > 1 0 ? ] curry make-matrix ;

: ?ignite ( -- 1/2 ) ignite-probability random-unit > 2 1 ? ;
: ?grow ( -- 0/1 ) grow-probability random-unit > 1 0 ? ;

: next-plot ( neighbors elt -- n )
    {
        { [ dup 2 = ] [ 2drop 0 ] }
        { [ 2dup [ [ 2 = ] any? ] [ 1 = ] bi* and ] [ 2drop 2 ] }
        { [ 1 = ] [ drop ?ignite ] }
        [ drop ?grow ]
    } cond ;

: next-forest ( forest -- newforest )
    [ next-plot ] matrix-map-neighbors ;


! ##### Display code #####

CONSTANT: colors ${ GRAY GREEN RED }

: draw-forest ( matrix -- )
    dup dim matrix-coordinates [ concat ] bi@ swap [
        [ first2 [ 5 * ] bi@ 5 5 ] dip colors nth draw-rectangle
    ] 2each ;

500 500 "Forest Fire" init-window 100 100 1/2 make-forest
60 set-target-fps
[ window-should-close ] [
    begin-drawing
        BLACK clear-background dup draw-forest
    end-drawing
    next-forest
] until drop close-window
```

[https://gfycat.com/boilingenviousboto]


## Forth

```forth
30             CONSTANT WIDTH
30             CONSTANT HEIGHT
WIDTH HEIGHT * CONSTANT SIZE

1 VALUE SEED
: (RAND) ( -- u)  \ xorshift generator
   SEED DUP 13 LSHIFT XOR
        DUP 17 RSHIFT XOR
        DUP  5 LSHIFT XOR
        DUP TO SEED ;
10000 CONSTANT RANGE
100   CONSTANT GROW
1     CONSTANT BURN
: RAND ( -- u)  (RAND) RANGE MOD ;

\ Create buffers for world state
CREATE A  SIZE ALLOT  A SIZE ERASE
CREATE B  SIZE ALLOT  B SIZE ERASE

0 CONSTANT NONE  1 CONSTANT TREE  2 CONSTANT FIRE
: NEARBY-FIRE? ( addr u -- t|f)
   2 -1 DO
     2 -1 DO
       J WIDTH * I + OVER +  \ calculate an offset
       DUP 0> OVER SIZE < AND IF
         >R OVER R> + C@     \ fetch state of the offset cell
         FIRE = IF UNLOOP UNLOOP DROP DROP TRUE EXIT THEN
       ELSE DROP THEN
     LOOP
   LOOP  DROP DROP FALSE ;
: GROW?   RAND GROW <= ;  \ spontaneously sprout?
: BURN?   RAND BURN <= ;  \ spontaneously combust?
: STEP ( prev next --)  \ Given state in PREV, put next in NEXT
   >R 0 BEGIN DUP SIZE <
   WHILE
      2DUP + C@ CASE
      FIRE OF NONE ENDOF
      TREE OF 2DUP NEARBY-FIRE? BURN? OR IF FIRE ELSE TREE THEN ENDOF
      NONE OF GROW? IF TREE ELSE NONE THEN ENDOF
      ENDCASE
      ( i next-cell-state) OVER R@ + C!        \ commit to next
   1+ REPEAT  R> DROP DROP DROP ;

: (ESCAPE)   27 EMIT [CHAR] [ EMIT ;
: ESCAPE"   POSTPONE (ESCAPE) POSTPONE S" POSTPONE TYPE ;  IMMEDIATE
: CLEAR   ESCAPE" H" ;
: RETURN   ESCAPE" E" ;
: RESET   ESCAPE" m" ;
: .FOREST ( addr --)  CLEAR
   HEIGHT 0 DO
     WIDTH 0 DO
       DUP C@ CASE
       NONE OF SPACE ENDOF
       TREE OF ESCAPE" 32m" [CHAR] T EMIT RESET ENDOF
       FIRE OF ESCAPE" 31m" [CHAR] # EMIT RESET ENDOF
       ENDCASE  1+
     LOOP  RETURN
   LOOP RESET DROP ;

: (GO) ( buffer buffer' -- buffer' buffer)
   2DUP STEP    \ step the simulation
   DUP .FOREST  \ print the current state
   SWAP ;       \ prepare for next iteration
: GO   A B  BEGIN (GO) AGAIN ;
```



## Fortran

```fortran
module ForestFireModel
  implicit none

  type :: forestfire
     integer, dimension(:,:,:), allocatable :: field
     integer :: width, height
     integer :: swapu
     real :: prob_tree, prob_f, prob_p
  end type forestfire

  integer, parameter :: &
       empty = 0, &
       tree = 1, &
       burning = 2

  private :: bcheck, set, oget, burning_neighbor ! cset, get

contains

  ! create and initialize the field(s)
  function forestfire_new(w, h, pt, pf, pp) result(res)
    type(forestfire) :: res
    integer, intent(in) :: w, h
    real, intent(in), optional :: pt, pf, pp

    integer :: i, j
    real :: r

    allocate(res%field(2,w,h)) ! no error check
    res%prob_tree = 0.5
    res%prob_f = 0.00001
    res%prob_p = 0.001
    if ( present(pt) ) res%prob_tree = pt
    if ( present(pf) ) res%prob_f = pf
    if ( present(pp) ) res%prob_p = pp

    res%width = w
    res%height = h
    res%swapu = 0

    res%field = empty

    do i = 1,w
       do j = 1,h
          call random_number(r)
          if ( r <= res%prob_tree ) call cset(res, i, j, tree)
       end do
    end do

  end function forestfire_new

  ! destroy the field(s)
  subroutine forestfire_destroy(f)
    type(forestfire), intent(inout) :: f

    if ( allocated(f%field) ) deallocate(f%field)

  end subroutine forestfire_destroy

  ! evolution
  subroutine forestfire_evolve(f)
    type(forestfire), intent(inout) :: f

    integer :: i, j
    real :: r

    do i = 1, f%width
       do j = 1, f%height
          select case ( get(f, i, j) )
          case (burning)
             call set(f, i, j, empty)
          case (empty)
             call random_number(r)
             if ( r > f%prob_p ) then
                call set(f, i, j, empty)
             else
                call set(f, i, j, tree)
             end if
          case (tree)
             if ( burning_neighbor(f, i, j) ) then
                call set(f, i, j, burning)
             else
                call random_number(r)
                if ( r > f%prob_f ) then
                   call set(f, i, j, tree)
                else
                   call set(f, i, j, burning)
                end if
             end if
          end select
       end do
    end do
    f%swapu = ieor(f%swapu, 1)
  end subroutine forestfire_evolve

  ! helper funcs/subs
  subroutine set(f, i, j, t)
    type(forestfire), intent(inout) :: f
    integer, intent(in) :: i, j, t

    if ( bcheck(f, i, j) ) then
       f%field(ieor(f%swapu,1), i, j) = t
    end if
  end subroutine set

  subroutine cset(f, i, j, t)
    type(forestfire), intent(inout) :: f
    integer, intent(in) :: i, j, t

    if ( bcheck(f, i, j) ) then
       f%field(f%swapu, i, j) = t
    end if
  end subroutine cset

  function bcheck(f, i, j)
    logical :: bcheck
    type(forestfire), intent(in) :: f
    integer, intent(in) :: i, j

    bcheck = .false.
    if ( (i >= 1) .and. (i <= f%width) .and. &
         (j >= 1) .and. (j <= f%height) ) bcheck = .true.

  end function bcheck


  function get(f, i, j) result(r)
    integer :: r
    type(forestfire), intent(in) :: f
    integer, intent(in) :: i, j

    if ( .not. bcheck(f, i, j) ) then
       r = empty
    else
       r = f%field(f%swapu, i, j)
    end if
  end function get

  function oget(f, i, j) result(r)
    integer :: r
    type(forestfire), intent(in) :: f
    integer, intent(in) :: i, j

    if ( .not. bcheck(f, i, j) ) then
       r = empty
    else
       r = f%field(ieor(f%swapu,1), i, j)
    end if
  end function oget

  function burning_neighbor(f, i, j) result(r)
    logical :: r
    type(forestfire), intent(in) :: f
    integer, intent(in) :: i, j

    integer, dimension(3,3) :: s

    s = f%field(f%swapu, i-1:i+1, j-1:j+1)
    s(2,2) = empty
    r = any(s == burning)
  end function burning_neighbor

  subroutine forestfire_print(f)
    type(forestfire), intent(in) :: f

    integer :: i, j

    do j = 1, f%height
       do i = 1, f%width
          select case(get(f, i, j))
          case (empty)
             write(*,'(A)', advance='no') '.'
          case (tree)
             write(*,'(A)', advance='no') 'Y'
          case (burning)
             write(*,'(A)', advance='no') '*'
          end select
       end do
       write(*,*)
    end do
  end subroutine forestfire_print

end module ForestFireModel
```



```fortran
program ForestFireTest
  use ForestFireModel
  implicit none

  type(forestfire) :: f
  integer :: i

  f = forestfire_new(74, 40)

  do i = 1, 1001
     write(*,'(A)', advance='no') achar(z'1b') // '[H' // achar(z'1b') // '[2J'
     call forestfire_print(f)
     call forestfire_evolve(f)
  end do

  call forestfire_destroy(f)

end program ForestFireTest
```



## Go

Text.  The program prints the configuration, waits for the Enter key, and prints the next.  It makes a pretty good animation to just hold down the Enter key.

```go
package main

import (
    "fmt"
    "math/rand"
    "strings"
)

const (
    rows = 20
    cols = 30
    p    = .01
    f    = .001
)

const rx = rows + 2
const cx = cols + 2

func main() {
    odd := make([]byte, rx*cx)
    even := make([]byte, rx*cx)
    for r := 1; r <= rows; r++ {
        for c := 1; c <= cols; c++ {
            if rand.Intn(2) == 1 {
                odd[r*cx+c] = 'T'
            }
        }
    }
    for {
        print(odd)
        step(even, odd)
        fmt.Scanln()

        print(even)
        step(odd, even)
        fmt.Scanln()
    }
}

func print(model []byte) {
    fmt.Println(strings.Repeat("__", cols))
    fmt.Println()
    for r := 1; r <= rows; r++ {
        for c := 1; c <= cols; c++ {
            if model[r*cx+c] == 0 {
                fmt.Print("  ")
            } else {
                fmt.Printf(" %c", model[r*cx+c])
            }
        }
        fmt.Println()
    }
}

func step(dst, src []byte) {
    for r := 1; r <= rows; r++ {
        for c := 1; c <= cols; c++ {
            x := r*cx + c
            dst[x] = src[x]
            switch dst[x] {
            case '#':
                // rule 1. A burning cell turns into an empty cell
                dst[x] = 0
            case 'T':
                // rule 2. A tree will burn if at least one neighbor is burning
                if src[x-cx-1]=='#'  || src[x-cx]=='#' || src[x-cx+1]=='#' ||
                    src[x-1] == '#'  ||                   src[x+1] == '#'  ||
                    src[x+cx-1]=='#' || src[x+cx]=='#' || src[x+cx+1] == '#' {
                    dst[x] = '#'

                    // rule 3. A tree ignites with probability f
                    // even if no neighbor is burning
                } else if rand.Float64() < f {
                    dst[x] = '#'
                }
            default:
                // rule 4. An empty space fills with a tree with probability p
                if rand.Float64() < p {
                    dst[x] = 'T'
                }
            }
        }
    }
}
```



## Haskell


```haskell
import Control.Monad (replicateM, unless)
import Data.List (tails, transpose)
import System.Random (randomRIO)

data Cell
  = Empty
  | Tree
  | Fire
  deriving (Eq)

instance Show Cell where
  show Empty = " "
  show Tree = "T"
  show Fire = "$"

randomCell :: IO Cell
randomCell = fmap ([Empty, Tree] !!) (randomRIO (0, 1) :: IO Int)

randomChance :: IO Double
randomChance = randomRIO (0, 1.0) :: IO Double

rim :: a -> [[a]] -> [[a]]
rim b = fmap (fb b) . (fb =<< rb)
  where
    fb = (.) <$> (:) <*> (flip (++) . return)
    rb = fst . unzip . zip (repeat b) . head

take3x3 :: [[a]] -> [[[a]]]
take3x3 = concatMap (transpose . fmap take3) . take3
  where
    take3 = init . init . takeWhile (not . null) . fmap (take 3) . tails

list2Mat :: Int -> [a] -> [[a]]
list2Mat n = takeWhile (not . null) . fmap (take n) . iterate (drop n)

evolveForest :: Int -> Int -> Int -> IO ()
evolveForest m n k = do
  let s = m * n
  fs <- replicateM s randomCell
  let nextState xs = do
        ts <- replicateM s randomChance
        vs <- replicateM s randomChance
        let rv [r1, [l, c, r], r3] newTree fire
              | c == Fire = Empty
              | c == Tree && Fire `elem` concat [r1, [l, r], r3] = Fire
              | c == Tree && 0.01 >= fire = Fire
              | c == Empty && 0.1 >= newTree = Tree
              | otherwise = c
        return $ zipWith3 rv xs ts vs
      evolve i xs =
        unless (i > k) $
        do let nfs = nextState $ take3x3 $ rim Empty $ list2Mat n xs
           putStrLn ("\n>>>>>> " ++ show i ++ ":")
           mapM_ (putStrLn . concatMap show) $ list2Mat n xs
           nfs >>= evolve (i + 1)
  evolve 1 fs

main :: IO ()
main = evolveForest 6 50 3
```

{{Out}} Sample:

```txt
>>>>>> 1:
   TTT  TT TT     TTT T TTT  T   TT  T  TT  TTTT
TTTT  T T TT T      T  TTTTTTT T    T  TT T TT  TT
TTTT TT   T TTTT T TT  T  TTTT T TT TT TT  T T TTT
T  TT TTTT TTT TTT TT TT   TTTTTT  TTTT  T TTT TTT
 T T  TTT  T T T TT T    TT     TT  TT   T TTT  TT
        T T TTT TT TT     T  TT  TTTTT  TT  TT  T

>>>>>> 2:
   TTT  TT TT     TTT T TTT  T T TT  T  T$  TTTT
TTTT  T T TTTT    T T  TTTTTTT T   TT  TT T TTT TT
TTTT TT   T TTTT T TT  T  TTTT TTT$ TT TT TT T TTT
T  TTTTTTT TTT TTTTTT TT   TTTTTTT TTTT  TTTTT TTT
 TTT TTTT TT T T TT T    TT     TT  TT   T TTT  TT
 T      T T TTT TT TT     T  TT  TTTTT  TT TTT  T
>>>>>> 3:
   TTT  TT TT     TTT T TTT  T T TT  T  $ TTTTTT
TTTT  T T TTTT    T TT TTTTTTTTT T $T  T$T$ TTT TT
TTTT TT   T TTTT T TT  T  TTTT TT$  TT TT TT T TTT
T TTTTTTTT TTT TTTTTT TT   TTTTTT$ $TTT  TTTTT TTT
 TTT TTTT TT T T TT TT T TT     TT  TT   T TTT  TT
 T      T T TTT TT TT   T T  TT  TTTTTT TT TTT  T
```


=={{header|Icon}} and {{header|Unicon}}==
[[File:Forestfire-Unicon.png|400px|thumb|right|Forest fire 400 x 400 rounds=500 p.initial=0.100000 p/f=0.010000/0.000200 fps=1.495256]]

```Icon
link graphics,printf

$define EDGE  0
$define EMPTY 1
$define TREE  2
$define FIRE  3

global Colours,Width,Height,ProbTree,ProbFire,ProbInitialTree,Forest,oldForest

procedure main()             # forest fire

    Height := 400            # Window height
    Width := 400             # Window width
    ProbInitialTree := .10   # intial probability of trees
    ProbTree := .01          # ongoing probability of trees
    ProbFire := ProbTree/50. # probability of fire
    Rounds := 500            # rounds to evolve

    setup_forest()
    every 1 to Rounds do {
       show_forest()
       evolve_forest()
       }
    printf("Forest fire %d x %d rounds=%d p.initial=%r p/f=%r/%r fps=%r\n",
           Width,Height,Rounds,ProbInitialTree,ProbTree,ProbFire,
           Rounds/(&time/1000.))  # stats
    WDone()
end

procedure setup_forest()     #: setup the forest

    Colours := table()       # define colours
    Colours[EDGE]  := "black"
    Colours[EMPTY] := "grey"
    Colours[TREE]  := "green"
    Colours[FIRE]  := "red"

    WOpen("label=Forest Fire", "bg=black",
          "size=" || Width+2 || "," || Height+2) | # add for border
             stop("Unable to open Window")
    every !(Forest := list(Height)) := list(Width,EMPTY)  # default
    every ( Forest[1,1 to Width]  | Forest[Height,1 to Width] |
            Forest[1 to Height,1] | Forest[1 to Height,Width] ) := EDGE
    every r := 2 to Height-1 & c := 2 to Width-1 do
       if probability(ProbInitialTree) then Forest[r,c] := TREE
end

procedure show_forest()      #: show Forest - drawn changes only
   every r := 2 to *Forest-1 & c := 2 to *Forest[r]-1 do
      if /oldForest | oldForest[r,c] ~= Forest[r,c] then {
         WAttrib("fg=" || Colours[Forest[r,c]])
         DrawPoint(r,c)
      }
end

procedure evolve_forest()    #: evolve forest
    old := oldForest := list(*Forest)     # freeze copy
    every old[i := 1 to *Forest] := copy(Forest[i])  # deep copy

    every r := 2 to *Forest-1 & c := 2 to *Forest[r]-1 do
       Forest[r,c] := case old[r,c] of {   # apply rules
          FIRE : EMPTY
          TREE : if probability(ProbFire) |
                  ( old[r-1, c-1 to c+1] |
                    old[r,c-1|c+1] |
                    old[r+1,c-1 to c+1] ) = FIRE then FIRE
          EMPTY: if probability(ProbTree) then TREE
          }
end

procedure probability(P)     #: succeed with probability P
if ?0 <= P then return
end
```


[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides printf]
[http://www.cs.arizona.edu/icon/library/src/procs/graphics.icn graphics.icn provides graphics]


## J


```j
NB. states: 0 empty, 1 tree, _1 fire
dims =:10 10

  tessellate=: 0,0,~0,.0,.~ 3 3 >./@,;._3 ]
  mask=: tessellate dims$1
  chance=: 1 :'(> ? bind (dims$0)) bind (mask*m)'

start=: 0.5 chance
grow =: 0.01 chance
fire =: 0.001 chance

  spread=: [: tessellate 0&>

  step=: grow [`]@.(|@])"0 >.&0 * _1 ^ fire +. spread

  run=:3 :0
    forest=. start''
    for.i.y do.
      smoutput ' #o' {~ forest=. step forest
    end.
  )
```


Example use:


```j
   run 2

 ##### #
    # #
 ### ####
  # # # #
  ##### #
 ##   # #
  #  #
  o##   #


 ##### #
    # #
 ### ####
  # # # #
  ##### #
 ##   # #
  o  #
   o#   #
```


Note that I have used an artificially small grid here, and that I ran this several times until I could find one that had a fire from the start.  Also, the current revision of this code does not show the starting state, though that would be easily changed.

Also, currently the parameters defining the size of the forest, and the probabilities are hard coded into the program and you need to rerun the program's script when they change.

Finally note that the grid size includes the one cell "border" which are blank.  If the border cells are meant to be outside of the represented dimensions, you can add 2 to them (or change the code to do so).


## Java

### Text


```java5
import java.util.Arrays;
import java.util.LinkedList;
import java.util.List;

public class Fire {
	private static final char BURNING = 'w'; //w looks like fire, right?
	private static final char TREE = 'T';
	private static final char EMPTY = '.';
	private static final double F = 0.2;
	private static final double P = 0.4;
	private static final double TREE_PROB = 0.5;

	private static List<String> process(List<String> land){
		List<String> newLand = new LinkedList<String>();
		for(int i = 0; i < land.size(); i++){
			String rowAbove, thisRow = land.get(i), rowBelow;
			if(i == 0){//first row
				rowAbove = null;
				rowBelow = land.get(i + 1);
			}else if(i == land.size() - 1){//last row
				rowBelow = null;
				rowAbove = land.get(i - 1);
			}else{//middle
				rowBelow = land.get(i + 1);
				rowAbove = land.get(i - 1);
			}
			newLand.add(processRows(rowAbove, thisRow, rowBelow));
		}
		return newLand;
	}

	private static String processRows(String rowAbove, String thisRow,
			String rowBelow){
		String newRow = "";
		for(int i = 0; i < thisRow.length();i++){
			switch(thisRow.charAt(i)){
			case BURNING:
				newRow+= EMPTY;
				break;
			case EMPTY:
				newRow+= Math.random() < P ? TREE : EMPTY;
				break;
			case TREE:
				String neighbors = "";
				if(i == 0){//first char
					neighbors+= rowAbove == null ? "" : rowAbove.substring(i, i + 2);
					neighbors+= thisRow.charAt(i + 1);
					neighbors+= rowBelow == null ? "" : rowBelow.substring(i, i + 2);
					if(neighbors.contains(Character.toString(BURNING))){
						newRow+= BURNING;
						break;
					}
				}else if(i == thisRow.length() - 1){//last char
					neighbors+= rowAbove == null ? "" : rowAbove.substring(i - 1, i + 1);
					neighbors+= thisRow.charAt(i - 1);
					neighbors+= rowBelow == null ? "" : rowBelow.substring(i - 1, i + 1);
					if(neighbors.contains(Character.toString(BURNING))){
						newRow+= BURNING;
						break;
					}
				}else{//middle
					neighbors+= rowAbove == null ? "" : rowAbove.substring(i - 1, i + 2);
					neighbors+= thisRow.charAt(i + 1);
					neighbors+= thisRow.charAt(i - 1);
					neighbors+= rowBelow == null ? "" : rowBelow.substring(i - 1, i + 2);
					if(neighbors.contains(Character.toString(BURNING))){
						newRow+= BURNING;
						break;
					}
				}
				newRow+= Math.random() < F ? BURNING : TREE;
			}
		}
		return newRow;
	}

	public static List<String> populate(int width, int height){
		List<String> land = new LinkedList<String>();
		for(;height > 0; height--){//height is just a copy anyway
			StringBuilder line = new StringBuilder(width);
			for(int i = width; i > 0; i--){
				line.append((Math.random() < TREE_PROB) ? TREE : EMPTY);
			}
			land.add(line.toString());
		}
		return land;
	}

	//process the land n times
	public static void processN(List<String> land, int n){
		for(int i = 0;i < n; i++){
			land = process(land);
		}
	}

	//process the land n times and print each step along the way
	public static void processNPrint(List<String> land, int n){
		for(int i = 0;i < n; i++){
			land = process(land);
			print(land);
		}
	}

	//print the land
	public static void print(List<String> land){
		for(String row: land){
			System.out.println(row);
		}
		System.out.println();
	}

	public static void main(String[] args){
		List<String> land = Arrays.asList(".TTT.T.T.TTTT.T",
				"T.T.T.TT..T.T..",
				"TT.TTTT...T.TT.",
				"TTT..TTTTT.T..T",
				".T.TTT....TT.TT",
				"...T..TTT.TT.T.",
				".TT.TT...TT..TT",
				".TT.T.T..T.T.T.",
				"..TTT.TT.T..T..",
				".T....T.....TTT",
				"T..TTT..T..T...",
				"TTT....TTTTTT.T",
				"......TwTTT...T",
				"..T....TTTTTTTT",
				".T.T.T....TT...");
		print(land);
		processNPrint(land, 10);

		System.out.println("Random land test:");

		land = populate(10, 10);
		print(land);
		processNPrint(land, 10);
	}
}
```


### Graphics

See: [[Forest fire/Java/Graphics]]

=={{header|JAMES II/Rule-based Cellular Automata}}==

```j2carules
@caversion 1;

dimensions 2;

state EMPTY, TREE, BURNING;

// an empty cell grows a tree with a chance of p = 5 %
rule{EMPTY} [0.05] : -> TREE;

// a burning cell turns to a burned cell
rule{BURNING}: -> EMPTY;

// a tree starts burning if there is at least one neighbor burning
rule{TREE} : BURNING{1,} -> BURNING;

// a tree is hit by lightning with a change of f = 0.006 %
rule{TREE} [0.00006] : -> BURNING;
```

The starting configuration cannot be given in the modeling language since the concepts of the ''model'' and its ''parameters'' (which includes the starting configuration) are separate in JAMES II.

=={{header|JavaScript}} Node ==

Functional approach using [https://lodash.com/ lodash]


```javascript
"use strict"

const _ = require('lodash');

const WIDTH_ARGUMENT_POSITION  = 2;
const HEIGHT_ARGUMENT_POSITION = 3;
const TREE_PROBABILITY         = 0.5;
const NEW_TREE_PROBABILITY     = 0.01;
const BURN_PROBABILITY         = 0.0001;
const CONSOLE_RED              = '\x1b[31m';
const CONSOLE_GREEN            = '\x1b[32m';
const CONSOLE_COLOR_CLOSE      = '\x1b[91m';
const CONSOLE_CLEAR            = '\u001B[2J\u001B[0;0f';
const NEIGHBOURS               = [
    [-1, -1],
    [-1,  0],
    [-1,  1],
    [ 0, -1],
    [ 0,  1],
    [ 1, -1],
    [ 1,  0],
    [ 1,  1]
];
const PRINT_DECODE             = {
    ' ': ' ',
    'T': `${CONSOLE_GREEN}T${CONSOLE_COLOR_CLOSE}`,
    'B': `${CONSOLE_RED}T${CONSOLE_COLOR_CLOSE}`,
};
const CONDITIONS = {
    'T': (forest, y, x) => Math.random() < BURN_PROBABILITY || burningNeighbour(forest, y, x) ? 'B' : 'T',
    ' ':  () => Math.random() < NEW_TREE_PROBABILITY ? 'T' : ' ',
    'B':  () => ' '
};

const WIDTH  = process.argv[WIDTH_ARGUMENT_POSITION]  || 20;
const HEIGHT = process.argv[HEIGHT_ARGUMENT_POSITION] || 10;

const update = forest => {
    return _.map(forest, (c, ci) => {
        return _.map(c, (r, ri) => {
            return CONDITIONS[r](forest, ci, ri);
        });
    });
}

const printForest = forest => {
    process.stdout.write(CONSOLE_CLEAR);
    _.each(forest, c => {
        _.each(c, r => {
            process.stdout.write(PRINT_DECODE[r]);
        });
        process.stdout.write('\n');
    })
}

const burningNeighbour = (forest, y, x) => {
    return _(NEIGHBOURS)
           .map(n => _.isUndefined(forest[y + n[0]]) ? null : forest[y + n[0]][x + n[1]])
           .any(_.partial(_.isEqual, 'B'));
};

let forest = _.times(HEIGHT, () => _.times(WIDTH, () => Math.random() < TREE_PROBABILITY ? 'T' : ' '));

setInterval(() => {
    forest = update(forest);
    printForest(forest)
}, 20);


```



## JavaScript


```javascript
var forest = {
    X: 50,
    Y: 50,
    propTree: 0.5,
    propTree2: 0.01,
    propBurn: 0.0001,
    t: [],
    c: ['rgb(255,255,255)', 'rgb(0,255,0)', 'rgb(255,0,0)']
};

for(var i = 0; i < forest.Y; i++) {
    forest.t[i] = [];
    for(var j = 0; j < forest.Y; j++) {
        forest.t[i][j] = Math.random() < forest.propTree ? 1 : 0;
    }
}

function afterLoad(forest) {
    var canvas = document.getElementById('canvas');
    var c = canvas.getContext('2d');
    for(var i = 0; i < forest.X; i++) {
        for(var j = 0; j < forest.Y; j++) {
            c.fillStyle = forest.c[forest.t[i][j]];
            c.fillRect(10*j, 10*i, 10*j+9, 10*i+9);
        }
    }
}

function doStep(forest) {
    var to = [];
    for(var i = 0; i < forest.Y; i++) {
        to[i] = forest.t[i].slice(0);
    }

    //indices outside the array are undefined; which converts to 0=empty on forced typecast
    for(var i = 0; i < forest.Y; i++) {
        for(var j = 0; j < forest.Y; j++) {
            if(0 == to[i][j]) {
                forest.t[i][j] = Math.random() < forest.propTree2 ? 1 : 0;
            } else if(1 == to[i][j]) {
                if(
                    ((i>0) && (2 == to[i-1][j])) ||
                    ((i<forest.Y-1) && (2 == to[i+1][j])) ||
                    ((j>0) && (2 == to[i][j-1])) ||
                    ((j<forest.X-1) && (2 == to[i][j+1]))
                    ) {
                    forest.t[i][j] = 2;
                } else {
                    forest.t[i][j] = Math.random() < forest.propBurn ? 2 : 1;
                }
            } else if(2 == to[i][j]) {
                //If it burns, it gets empty ...
                forest.t[i][j] = 0;
            }
        }
    }

}

window.setInterval(function(){
    doStep(forest);
    afterLoad(forest);
}, 100);

```


To actually see it work we need a small demo page with HTML5 compliant code:


```html5
<!DOCTYPE html>
<html>
<head>
<title>Forest Fire</title>
</head>
<body>
<canvas id="canvas" width="500" height="500">
Your browser doesn't support HTML5 Canvas.
</canvas>
<script language="JavaScript">//<![CDATA[<!--
// --> HERE COMES THE SCRIPT FROM ABOVE <--
//-->]]></script>
</body>
</html>

```


The output is a (mostly fluent) animation of the area.


## Julia


```julia
using Printf

@enum State empty tree fire


function evolution(nepoch::Int=100, init::Matrix{State}=fill(tree, 30, 50))
    # Single evolution
    function evolve!(forest::Matrix{State}; f::Float64=0.12, p::Float64=0.5)
        dir = [-1 -1; -1 0; -1 1; 0 -1; 0 1; 1 -1; 1 0; 1 1]
        # A tree will burn if at least one neighbor is burning
        for i in 1:size(forest, 1), j in 1:size(forest, 2)
            for k in 1:size(dir, 1)
                if checkbounds(Bool, forest, i + dir[k, 1], j + dir[k, 2]) &&
                    get(forest, i + dir[k, 1], j + dir[k, 2]) == fire
                    forest[i, j] = fire
                    break
                end
            end
        end
        for i in LinearIndices(forest)
            # A burning cell turns into an empty cell
            if forest[i] == fire forest[i] = empty end
            # A tree ignites with probability f even if no neighbor is burning
            if forest[i] == tree && rand() < f forest[i] = fire end
            # An empty space fills with a tree with probability p
            if forest[i] == empty && rand() < p forest[i] = tree end
        end
    end

    # Print functions
    function printforest(f::Matrix{State})
        for i in 1:size(f, 1)
            for j in 1:size(f, 2)
                print(f[i, j] == empty ? ' ' : f[i, j] == tree ? '🌲' : '🔥')
            end
            println()
        end
    end
    function printstats(f::Matrix{State})
        tot = length(f)
        nt  = count(x -> x in (tree, fire), f)
        nb  = count(x -> x == fire, f)
        @printf("\n%6i cell(s), %6i tree(s), %6i currently burning (%6.2f%%, %6.2f%%)\n",
                tot, nt, nb, nt / tot * 100, nb / nt * 100)
    end

    # Main
    printforest(init)
    printstats(init)
    for i in 1:nepoch
        # println("\33[2J")
        evolve!(init)
        # printforest(init)
        # printstats(init)
        # sleep(1)
    end
    printforest(init)
    printstats(init)
end

evolution()
```


Final output (epoch 100):

```txt
🌲🌲🔥🌲 🌲🌲🌲🌲 🔥🌲🌲🔥🌲🌲🌲🌲🌲🔥🌲🌲🌲 🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲 🌲 🌲🌲🌲🔥 🌲🌲🌲🌲🔥🌲🌲
🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲 🌲🌲🌲🌲 🔥🌲🌲 🌲🌲🔥🔥🌲🌲 🌲🌲🌲 🌲🌲🌲🌲
🌲🌲🌲🌲🌲🔥🔥🌲   🌲🌲🌲 🔥🔥🌲🌲🌲 🌲  🌲🌲🌲🌲🌲🌲🌲🌲🔥🌲 🌲🌲 🌲🌲🌲 🌲🌲🌲🌲🌲🌲🌲
🌲🔥🌲 🌲🌲 🌲 🌲🌲🌲🌲 🌲🌲   🌲 🌲🌲🌲 🌲🌲 🌲 🌲🔥🌲🌲🌲🔥🔥🔥🌲🌲🌲🌲🌲🌲🌲🌲🌲🔥🌲🌲
 🌲🌲🌲 🔥🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🔥🌲🌲🌲🌲🌲 🌲🌲🌲🌲🌲🌲🌲🌲🌲🔥🌲🌲🌲🌲🌲🌲 🌲🌲🌲  🌲🌲
🌲 🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲 🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲 🌲 🌲🌲🌲🌲🌲🌲🌲🔥🌲🌲🌲🌲🌲🌲🌲🌲 🌲🌲 🌲🌲
🌲🌲🌲 🌲🌲🌲🌲 🌲🌲 🌲🌲 🌲🌲🌲🌲🌲🌲 🌲🌲🌲🌲🌲🌲🔥🌲 🌲🌲🌲🌲🌲🌲 🌲🌲🌲 🌲 🔥🌲🌲🌲🌲🌲
🌲🌲🔥🔥🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲 🌲🌲🌲🌲 🌲🌲🌲 🌲🌲 🌲🌲🌲 🌲🌲🌲🌲🌲🌲🌲🌲 🌲🌲🌲🌲🌲🔥🌲🌲🌲🌲
🌲🌲🌲🌲🌲🌲🌲🌲 🌲🌲🌲🌲🌲🌲🌲🔥 🌲🌲🌲🌲 🌲🌲🌲🌲🌲 🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲
🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲 🌲🌲   🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🔥  🔥🌲🔥🌲🌲 🌲🌲🌲🌲🌲🌲🌲 🌲🌲🌲
 🌲 🔥 🌲🌲🌲🌲🌲🌲🌲🌲 🌲🌲 🌲🌲🌲🌲🌲🔥🌲 🌲 🌲🌲🌲 🌲🔥 🔥🔥🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲
🌲🌲🌲🔥🌲🌲🌲🌲🌲🌲🌲🔥🌲🌲🌲🌲🌲🌲🌲🌲🌲 🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🔥🌲🌲🔥🌲🌲 🌲 🌲🌲🌲🌲🌲🌲
🌲🌲🔥🌲🌲🌲 🌲 🔥🌲 🌲 🌲 🌲🌲🌲🔥🌲🌲🔥🌲🌲🌲🌲🔥🌲🌲🌲🌲🌲🌲🌲🌲 🌲🌲🌲🌲  🌲🌲🌲🌲🌲🌲
🌲 🌲🌲 🌲🌲🌲🌲  🌲 🔥🔥🌲🌲🌲🔥 🌲🌲 🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🔥 🌲🌲
🌲🌲🌲🔥 🌲🌲🌲🌲🔥🌲🔥🌲 🌲🌲🌲🌲🌲🌲🌲🔥🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲 🌲🌲🌲🌲🌲🌲🔥🌲🌲
 🌲🌲🌲🌲🌲 🌲🌲🌲🌲🌲🌲 🌲🌲🌲 🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🔥🌲🔥 🌲🌲🌲🌲🌲🌲🔥   🌲🌲🌲  🔥🔥
🌲 🌲🌲🌲🌲🌲🌲🌲🔥 🌲🌲🌲🌲 🌲🌲 🌲🌲🔥🌲🌲  🌲 🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🔥🌲  🔥🌲🔥🌲
🌲🌲🌲 🌲🌲🔥 🌲🌲🌲🌲🌲🌲🌲🌲🔥🌲🌲  🌲🔥 🌲🌲  🌲🌲🌲🔥🌲🌲🔥🌲 🔥🌲 🌲🌲🌲🌲🌲🌲🌲🌲 🌲
🌲🌲🌲 🔥🌲🌲🌲🌲  🌲🌲 🌲 🌲🌲🌲 🌲🌲🌲🌲🌲 🌲🌲 🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲 🌲 🌲🌲🌲🌲🌲🌲🌲
  🌲 🌲 🌲🌲  🌲🌲 🌲🌲  🌲 🌲🌲🌲🌲🌲🌲🌲  🌲  🌲  🌲🌲🌲 🌲🌲 🌲🌲🌲🌲 🌲 🌲
🌲🌲  🌲 🌲🌲🌲 🌲🌲    🌲  🌲🌲🌲🌲🌲 🌲🌲 🌲🌲  🌲🌲 🌲🌲 🌲🌲   🌲 🌲🌲🌲
 🌲🌲  🌲🌲 🌲🌲 🌲🌲  🌲🌲 🌲🌲🌲🌲🌲🌲🌲🌲🌲🌲  🌲  🌲   🌲🌲🌲 🌲
🌲🌲🌲  🌲🌲🌲🌲  🌲 🌲  🌲 🌲🌲🌲🌲🌲   🌲🌲 🌲   🌲 🌲🌲  🌲🌲 🌲 🌲🌲  🌲🌲
🌲🌲 🌲🌲🌲 🌲🌲🌲  🌲  🌲    🌲  🌲 🌲🌲   🌲  🌲🌲      🌲 🌲 🌲 🌲🌲
 🌲🌲 🌲      🌲🌲🌲🌲 🌲   🌲🌲  🌲🌲🌲🌲 🌲   🌲🌲🌲🌲  🌲    🌲🌲🌲🌲
🌲🌲  🌲🌲 🌲🌲🌲🌲🌲 🌲 🌲🌲 🌲🌲🌲🌲🌲  🌲 🌲 🌲🌲🌲 🌲🌲🌲   🌲 🌲🌲 🌲 🌲
      🌲 🌲🌲 🌲🌲   🌲   🌲🌲🌲🌲  🌲🌲 🌲 🌲🌲🌲 🌲🌲   🌲🌲🌲      🌲
🌲🌲 🌲🌲  🌲    🌲  🌲🌲🌲🌲🌲 🌲🌲 🌲   🌲 🌲 🌲🌲 🌲  🌲🌲🌲🌲🌲🌲    🌲🌲
 🌲   🌲  🌲🌲 🌲🌲   🌲 🌲🌲 🌲🌲🌲 🌲    🌲 🌲   🌲  🌲🌲🌲🌲      🌲
   🌲🌲🌲🌲🌲 🌲 🌲🌲   🌲  🌲 🌲🌲🌲   🌲🌲🌲     🌲 🌲🌲 🌲🌲 🌲 🌲🌲🌲🌲

  1500 cell(s),   1089 tree(s),     73 currently burning ( 72.60%,   6.70%)
```



## Lua

This program uses the Lua Curses library for graphics, although changing the code to avoid such dependency is easy.

```Lua

-- ForestFire automaton implementation
-- Rules: at each step:
-- 1) a burning tree disappears
-- 2) a non-burning tree starts burning if any of its neighbours is
-- 3) an empty spot may generate a tree with prob P
-- 4) a non-burning tree may ignite with prob F

local socket = require 'socket' -- needed for socket.sleep
local curses = require 'curses'

local p_spawn, p_ignite = 0.005, 0.0002
local naptime = 0.03 -- seconds
local forest_x, forest_y = 60, 30

local forest = (function (x, y)
	local wrl = {}
	for i = 1, y do
		wrl[i] = {}
		for j = 1, x do
			local rand = math.random()
			wrl[i][j] = (rand < 0.5) and 1 or 0
		end
	end
	return wrl
end)(forest_x, forest_y)

math.randomseed(os.time())

forest.step = function (self)
	for i = 1, #self do
		for j = 1, #self[i] do
			if self[i][j] == 0 then
				if math.random() < p_spawn then self[i][j] = 1 end
			elseif self[i][j] == 1 then
				if self:ignite(i, j) or math.random() < p_ignite then self[i][j] = 2 end
			elseif self[i][j] == 2 then self[i][j] = 0
			else error("Error: forest[" .. i .. "][" .. j .. "] is " .. self[i][j] .. "!")
			end
		end
	end
end

forest.draw = function (self)
	for i = 1, #self do
		for j = 1, #self[i] do
			if self[i][j] == 0 then win:mvaddch(i,j," ")
			elseif self[i][j] == 1 then
				win:attron(curses.color_pair(1))
				win:mvaddch(i,j,"Y")
				win:attroff(curses.color_pair(1))
			elseif self[i][j] == 2 then
				win:attron(curses.color_pair(2))
				win:mvaddch(i,j,"#")
				win:attroff(curses.color_pair(2))
			else error("self[" .. i .. "][" .. j .. "] is " .. self[i][j] .. "!")
			end
		end
	end
end

forest.ignite = function (self, i, j)
	for k = i - 1, i + 1 do
		if k < 1 or k > #self then goto continue1 end
		for l = j - 1, j + 1 do
			if 	l < 1 or
				l > #self[i] or
				math.abs((k - i) + (l - j)) ~= 1
			then
				goto continue2
			end
			if self[k][l] == 2 then return true end
			::continue2::
		end
		::continue1::
	end
	return false
end

local it = 1
curses.initscr()
curses.start_color()
curses.echo(false)
curses.init_pair(1, curses.COLOR_GREEN, curses.COLOR_BLACK)
curses.init_pair(2, curses.COLOR_RED, curses.COLOR_BLACK)
win = curses.newwin(forest_y + 2, forest_x, 0, 0)
win:clear()
win:mvaddstr(forest_y + 1, 0, "p_spawn = " .. p_spawn .. ", p_ignite = " .. p_ignite)
repeat
	forest:draw()
	win:move(forest_y, 0)
	win:clrtoeol()
	win:addstr("Iteration: " .. it .. ", nap = " .. naptime*1000 .. "ms")
	win:refresh()
	forest:step()
	it = it + 1
	socket.sleep(naptime)
until false

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica is good at working with cellular automata -- especially 2-color 1-dimensional cellular automata. The automaton function is awkward yet very powerful. This code implements a 3-color 2-dimensional cellular automaton with 9-cell neighbourhoods using a custom cell evolution function. There is probably a rule number specification that can replace the custom evolution function and make this simpler and faster. But this works well enough. The last line of code plots the state of the forest after the 300th step.


```Mathematica
evolve[nbhd_List, k_] := 0 /; nbhd[[2, 2]] == 2    (*burning->empty*)
evolve[nbhd_List, k_] := 2 /; nbhd[[2, 2]] == 1 && Max@nbhd == 2     (*near_burning&nonempty->burning*)
evolve[nbhd_List, k_] := RandomChoice[{f, 1 - f} -> {2, nbhd[[2, 2]]}] /; nbhd[[2, 2]] == 1 && Max@nbhd < 2   (*spontaneously combusting tree*)
evolve[nbhd_List, k_] := RandomChoice[{p, 1 - p} -> {1, nbhd[[2, 2]]}] /; nbhd[[2, 2]] == 0  (*random tree growth*)

r = 100; c = 100; p = 10^-2; f = 10^-4;
init = RandomInteger[BernoulliDistribution[0.05], {r, c}];
MatrixPlot[CellularAutomaton[{evolve, {}, {1, 1}}, {init, 0}, {{{300}}}], ColorRules -> {0 -> White, 1 -> Green, 2 -> Red}, Frame -> False]
```

[[File:ForestFire-Mathematica.png]]

=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function forest_fire(f,p,N,M)
% Forest fire
if nargin<4;
	M=200;
end
if nargin<3;
	N=200;
end
if nargin<2;
	p=.03;
end
if nargin<1;
	f=p*.0001;
end

% initialize;
F = (rand(M,N) < p)+1;  % tree with probability p
S = ones(3); S(2,2)=0;  % surrounding

textmap = ' T#';
colormap([.5,.5,.5;0,1,0;1,0,0]);
while(1)
	image(F); pause(.1)    % uncomment for graphical output
	% disp(textmap(F));	pause;		  % uncomment for textual output
	G = ((F==1).*((rand(M,N)<p)+1));  % grow tree
	G = G + (F==2) .* ((filter2(S,F==3)>0) + (rand(M,N)<f) + 2);  % burn tree if neighbor is burning or by chance f
	G = G + (F==3);						 % empty after burn
	F = G;
end;
```



## Nim

```nim
import math, os, strutils
randomize()

type State = enum Empty, Tree, Fire

const
  disp: array[State, string] = ["  ", "\e[32m/\\\e[m", "\e[07;31m/\\\e[m"]
  treeProb = 0.01
  burnProb = 0.001

proc chance(prob: float): bool = random(1.0) < prob

# Set the size
var w, h: int
if paramCount() >= 2:
  w = parseInt paramStr 1
  h = parseInt paramStr 2
if w <= 0: w = 30
if h <= 0: h = 30

# Iterate over fields in the universe
iterator fields(a = (0,0), b = (h-1,w-1)) =
  for y in max(a[0], 0) .. min(b[0], h-1):
    for x in max(a[1], 0) .. min(b[1], w-1):
      yield (y,x)

# Create a sequence with an initializer
proc newSeqWith[T](len: int, init: T): seq[T] =
  result = newSeq[T] len
  for i in 0 .. <len:
    result[i] = init

# Initialize
var univ, univNew = newSeqWith(h, newSeq[State] w)

while true:
  # Show
  stdout.write "\e[H"
  for y,x in fields():
    stdout.write disp[univ[y][x]]
    if x == 0: stdout.write "\e[E"
  stdout.flushFile

  # Evolve
  for y,x in fields():
    case univ[y][x]
    of Fire:
      univNew[y][x] = Empty
    of Empty:
      if chance treeProb: univNew[y][x] = Tree
    of Tree:
      for y1, x1 in fields((y-1,x-1), (y+1,x+1)):
        if univ[y1][x1] == Fire: univNew[y][x] = Fire
      if chance burnProb: univNew[y][x] = Fire
  univ = univNew
  sleep 200
```



## OCaml

This example uses a curses display (with the [http://www.nongnu.org/ocaml-tmk/ ocaml-curses] bindings).


```ocaml
open Curses

let ignite_prob = 0.02
let sprout_prob = 0.01

type cell = Empty | Burning | Tree

let get w x y =
  try w.(x).(y)
  with Invalid_argument _ -> Empty

let neighborhood_burning w x y =
  try
    for _x = pred x to succ x do
      for _y = pred y to succ y do
        if get w _x _y = Burning then raise Exit
      done
    done
    ; false
  with Exit -> true

let evolves w x y =
  match w.(x).(y) with
  | Burning -> Empty
  | Tree ->
      if neighborhood_burning w x y
      then Burning
      else begin
        if (Random.float 1.0) < ignite_prob
        then Burning
        else Tree
      end
  | Empty ->
      if (Random.float 1.0) < sprout_prob
      then Tree
      else Empty

let step width height w =
  for x = 0 to pred width do
    for y = 0 to pred height do
      w.(x).(y) <- evolves w x y
    done
  done

let i = int_of_char
let repr = function
  | Empty -> i ' ' | Burning -> i '#' | Tree -> i 't'

let draw width height w =
  for x = 0 to pred width do
    for y = 0 to pred height do
      ignore(move y x);
      ignore(delch ());
      ignore(insch (repr w.(x).(y)));
    done;
  done;
  ignore(refresh ())

let () =
  Random.self_init ();
  let wnd = initscr () in
  ignore(cbreak ());
  ignore(noecho ());
  let height, width = getmaxyx wnd in
  let w = Array.make_matrix width height Empty in
  clear ();
  ignore(refresh ());
  while true do
    draw width height w;
    step width height w;
    Unix.sleep 1;
  done;
  endwin()
```


You can execute this script with:
 $ ocaml unix.cma -I +curses curses.cma forest.ml


## Ol


```scheme

(import (lib gl))
(import (otus random!))

(define WIDTH 170)
(define HEIGHT 96)

; probabilities
(define p 20)
(define f 1000)

(gl:set-window-title "Drossel and Schwabl 'forest-fire'")
(import (OpenGL version-1-0))

   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)
   (glOrtho 0 WIDTH 0 HEIGHT 0 1)

(gl:set-userdata (make-vector (map (lambda (-) (make-vector (map (lambda (-) (rand! 2)) (iota WIDTH)))) (iota HEIGHT))))

(gl:set-renderer (lambda (mouse)
   (let ((forest (gl:get-userdata))
         (step (make-vector (map (lambda (-) (make-vector (repeat 0 WIDTH))) (iota HEIGHT)))))
      (glClear GL_COLOR_BUFFER_BIT)

      (glPointSize (/ 854 WIDTH))
      (glBegin GL_POINTS)
         (for-each (lambda (y)
               (for-each (lambda (x)
                     (case (ref (ref forest y) x)
                        (0 ; An empty space fills with a tree with probability "p"
                           (if (zero? (rand! p))
                              (set-ref! (ref step y) x 1)))
                        (1
                           (glColor3f 0.2 0.7 0.2)
                           (glVertex2f x y)
                           ; A tree will burn if at least one neighbor is burning
                           ; A tree ignites with probability "f" even if no neighbor is burning
                           (if (or (eq? (ref (ref forest (- y 1)) (- x 1)) 2)  (eq? (ref (ref forest (- y 1))    x)    2)  (eq? (ref (ref forest (- y 1)) (+ x 1)) 2)
                                   (eq? (ref (ref forest    y   ) (- x 1)) 2)                                              (eq? (ref (ref forest    y   ) (+ x 1)) 2)
                                   (eq? (ref (ref forest (+ y 1)) (- x 1)) 2)  (eq? (ref (ref forest (+ y 1))    x)    2)  (eq? (ref (ref forest (+ y 1)) (+ x 1)) 2)
                                   (zero? (rand! f)))
                              (set-ref! (ref step y) x 2)
                              (set-ref! (ref step y) x 1)))
                        (2
                           (glColor3f 0.7 0.7 0.1)
                           (glVertex2f x y))
                           ; A burning cell turns into an empty cell
                           (set-ref! (ref step y) x 0)))
                  (iota WIDTH)))
            (iota HEIGHT))
      (glEnd)
      (gl:set-userdata step))))

```



## PARI/GP


```parigp
step(M,p,f)={
	my(m=matsize(M)[1],n=matsize(M)[2]);
	matrix(m,n,i,j,
		if(M[i,j]=="*",
			" "
		,
			if(M[i,j]=="t",
				my(nbr="t");
				for(x=max(1,i-1),min(m,i+1),
					for(y=max(1,j-1),min(n,j+1),
						if(M[x,y]=="*",nbr="*";break(2))
					)
				);
				if(random(1.)<f,"*",nbr)
			,
				if(random(1.)<p,"t"," ")
			)
		)
	)
};
burn(n,p,f)={
	my(M=matrix(n,n,i,j,if(random(2)," ","t")),N);
	while(1,print(M=step(M,p,f)))
};
burn(5,.1,.03)
```



## Perl

Requires terminal that understands ANSI escape sequences:
```Perl

use 5.10.0;

my $w = `tput cols` - 1;
my $h = `tput lines` - 1;
my $r = "\033[H";

my ($green, $red, $yellow, $norm) = ("\033[32m", "\033[31m", "\033[33m", "\033[m");

my $tree_prob = .05;
my $burn_prob = .0002;

my @forest = map([ map((rand(1) < $tree_prob) ? 1 : 0, 1 .. $w) ], 1 .. $h);

sub iterate {
	my @new = map([ map(0, 1 .. $w) ], 1 .. $h);
	for my $i (0 .. $h - 1) {
	for my $j (0 .. $w - 1) {
		$new[$i][$j] = $forest[$i][$j];
		if ($forest[$i][$j] == 2) {
			$new[$i][$j] = 3;
			next;
		} elsif ($forest[$i][$j] == 1) {
			if (rand() < $burn_prob) {
				$new[$i][$j] = 2;
				next;
			}
			for (	[-1, -1], [-1, 0], [-1, 1],
				[ 0, -1], 	   [ 0, 1],
				[ 1, -1], [ 1, 0], [ 1, 1] )
			{
				my $y = $_->[0] + $i;
				next if $y < 0 || $y >= $h;
				my $x = $_->[1] + $j;
				next if $x < 0 || $x >= $w;
				if ($forest[$y][$x] == 2) {
					$new[$i][$j] = 2;
					last;
				}
			}
		} elsif (rand() < $tree_prob) {
			$new[$i][$j] = 1;
		} elsif ($forest[$i][$j] == 3) {
			$new[$i][$j] = 0;
		}
	}}
	@forest = @new;
}

sub forest {
	print $r;
	for (@forest) {
		for (@$_) {
			when(0) { print " "; }
			when(1) { print "${green}*"}
			when(2) { print "${red}&" }
			when(3) { print "${yellow}&" }
		}
		print "\033[E\033[1G";
	}
	iterate;
}

forest while (1);
```



### Alternate Perl Solution


```Perl
#!/usr/bin/perl

use strict;                       # http://www.rosettacode.org/wiki/Forest_fire
use warnings;

my $p = 0.01;                               # probability of empty -> tree
my $f = 0.0001;                             # probability of  tree -> burning

my ($high, $wide) = split ' ', qx(stty size);      # 135 174 tiny font in xterm
my $mask = 0 x $wide . (0 . 7 x ($wide - 2) . 0) x ($high - 5) . 0 x $wide;
my $forest = $mask =~ s/7/ rand() < 0.5 ? 2 : 1 /ger;

for( 1 .. 1e3 )
  {                                         # 0=border 1=empty 2=tree 3=burning
  print "\e[H", $forest =~ tr/0123/  ^#/r, "\n"; # ^=tree  #=burning tree
  my $n = $forest =~ tr/123/004/r;          # 4=a neighbor is burning
  $forest |= 0 x $_ . $n | substr $n, $_ for 1, $wide - 1 .. $wide + 1;
  $forest &= $mask;                         # clear borders and trim
  $forest =~ tr/1-7/et10e31/;               # step to next generation
  $forest =~ s/t/ rand() < $f ? 3 : 2 /ge;  # rule 3) tree cell to burning
  $forest =~ s/e/ rand() < $p ? 2 : 1 /ge;  # rule 4) empty cell to tree
  select undef, undef, undef, 0.1;          # comment out for full speed
  }
```


## Perl 6



### ANSI graphics

This version saves a lot of looking around by using four states instead of three; the <tt>Heating</tt> state does a lookahead to track trees that are being heated up by burning trees, so we only ever have to traverse the neighbors of burning trees, not all trees.  Also, by only checking the list of burning trees, we can avoid copying the entire forest each iteration, since real forests are mutable.

```perl6
my $RED = "\e[1;31m";
my $YELLOW = "\e[1;33m";
my $GREEN = "\e[1;32m";
my $CLEAR = "\e[0m";

enum Cell-State <Empty Tree Heating Burning>;
my @pix = '  ', $GREEN ~ '木', $YELLOW ~ '木', $RED ~ '木';

class Forest {
    has Rat $.p = 0.01;
    has Rat $.f = 0.001;
    has Int $!height;
    has Int $!width;
    has @!coords;
    has @!spot;
    has @!neighbors;

    method BUILD (Int :$!height, Int :$!width) {
	@!coords = ^$!height X ^$!width;
	@!spot = [ (Bool.pick ?? Tree !! Empty) xx $!width ] xx $!height;
        self!init-neighbors;
    }

    method !init-neighbors {
        for @!coords -> ($i, $j) {
            @!neighbors[$i][$j] = eager gather for
                    [-1,-1],[+0,-1],[+1,-1],
                    [-1,+0],        [+1,+0],
                    [-1,+1],[+0,+1],[+1,+1]
	    {
		take-rw @!spot[$i + .[0]][$j + .[1]] // next;
	    }
	}
    }

    method step {
	my @heat;
        for @!coords -> ($i, $j) {
            given @!spot[$i][$j] {
                when Empty   { $_ = Tree if rand < $!p }
                when Tree    { $_ = Heating if rand < $!f }
                when Heating { $_ = Burning; push @heat, ($i, $j); }
                when Burning { $_ = Empty }
            }
        }
	for @heat -> ($i,$j) {
	    $_ = Heating for @!neighbors[$i][$j].grep(Tree);
	}
    }

    method show {
        for ^$!height -> $i {
            say @pix[@!spot[$i].list].join;
        }
    }
}

my ($ROWS, $COLS) = qx/stty size/.words;

signal(SIGINT).act: { print "\e[H\e[2J"; exit }

sub MAIN (Int $height = $ROWS - 2, Int $width = +$COLS div 2 - 1) {
    my Forest $forest .= new(:$height, :$width);
    print "\e[2J";      # ANSI clear screen
    loop {
	print "\e[H";   # ANSI home
	say $++;
	$forest.show;
	$forest.step;
    }
}
```



### SDL2 Animation

An alternate version implemented in SDL2.


```perl6
use NativeCall;
use SDL2::Raw;

my ($width, $height) = 900, 900;

SDL_Init(VIDEO);
my SDL_Window $window = SDL_CreateWindow(
    "Forest Fire - Perl 6",
    SDL_WINDOWPOS_CENTERED_MASK, SDL_WINDOWPOS_CENTERED_MASK,
    $width, $height,
    RESIZABLE
);
my SDL_Renderer $renderer = SDL_CreateRenderer( $window, -1, ACCELERATED +| PRESENTVSYNC );

SDL_ClearError();

my int ($w, $h) = 200, 200;

my $forest_texture = SDL_CreateTexture($renderer, %PIXELFORMAT<RGB332>, STREAMING, $w, $h);

my $pixdatabuf  = CArray[int64].new(0, $w, $h, $w);
my $work-buffer = CArray[int64].new(0, $w, $h, $w);

my int $bare    = 0;    # Black
my int $tree    = 8;    # Green
my int $heating = -120; # Orange ( 132 but it's being passed into an int8 )
my int $burning = 128;  # Red
my int $buf = $w * $h;
my $humidity    = .7;  # Chance that a tree next to a burning tree will resist catching fire
my $tree-spawn  = .75; # Initial probability that a space will contain a tree. Probability
                       # will be adjusted (way down) once rendering starts.

sub render {

    # work-around to pass the pointer-pointer.
    my $pixdata = nativecast(Pointer[int64], $pixdatabuf);
    SDL_LockTexture($forest_texture, SDL_Rect, $pixdata, my int $pitch);

    $pixdata = nativecast(CArray[int8], Pointer.new($pixdatabuf[0]));

    loop (my int $row; $row < $h; $row = $row + 1) {
        my int $rs = $row * $w; # row start
        my int $re = $rs  + $w; # row end
        loop (my int $idx = $rs; $idx < $re; $idx = $idx + 1) {
            # Skip it if it is a tree
            next if $pixdata[$idx] == $tree;
            if $pixdata[$idx] == $bare {
                # Maybe spawn a tree on bare ground
                $work-buffer[$idx] = rand < $tree-spawn ?? $tree !! $bare;
            } elsif $pixdata[$idx] == $heating {
                # Check if there are trees around a hot spot and light them if humidity is low enough
                $work-buffer[$idx - $w - 1] = $heating if rand > $humidity && $pixdata[$idx - $w - 1] && $row > 0;
                $work-buffer[$idx - $w    ] = $heating if rand > $humidity && $pixdata[$idx - $w    ] && $row > 0;
                $work-buffer[$idx - $w + 1] = $heating if rand > $humidity && $pixdata[$idx - $w + 1] && $row > 0;
                $work-buffer[$idx - 1     ] = $heating if rand > $humidity && $pixdata[$idx -  1    ];
                $work-buffer[$idx + $w - 1] = $heating if rand > $humidity && $pixdata[$idx + $w - 1];
                $work-buffer[$idx + $w    ] = $heating if rand > $humidity && $pixdata[$idx + $w    ];
                $work-buffer[$idx + $w + 1] = $heating if rand > $humidity && $pixdata[$idx + $w + 1];
                $work-buffer[$idx + 1     ] = $heating if rand > $humidity && $pixdata[$idx +  1    ];

                # Hotspot becomes a flame
                $work-buffer[$idx] = $burning
            } else {
                # Extinguish a flame after fuel is gone
                $work-buffer[$idx] = $bare;
            }
        }
    }
    # copy working buffer to main texture buffer
    loop (my int $i; $i < $buf; $i = $i + 1) { $pixdata[$i] = $work-buffer[$i] }

    # start a fire maybe
    $pixdata[$buf.rand] = $heating if rand < .1;

    SDL_UnlockTexture($forest_texture);

    SDL_RenderCopy($renderer, $forest_texture, SDL_Rect, SDL_Rect.new(:x(0), :y(0), :w($width), :h($height)));
    SDL_RenderPresent($renderer);
    once $tree-spawn = .005;
}

my $event = SDL_Event.new;

enum KEY_CODES ( K_Q => 20 );

main: loop {

    while SDL_PollEvent($event) {
        my $casted_event = SDL_CastEvent($event);

        given $casted_event {
            when *.type == QUIT {
                last main;
            }
            when *.type == KEYDOWN {
                if KEY_CODES(.scancode) -> $comm {
                    given $comm {
                        when 'K_Q'      { last main }
                    }
                }
            }
            when *.type == WINDOWEVENT {
                if .event == RESIZED {
                    $width  = .data1;
                    $height = .data2;
                }
            }
        }
    }
    render();
    print fps;
}
say '';

sub fps {
    state $fps-frames = 0;
    state $fps-now    = now;
    state $fps        = '';
    $fps-frames++;
    if now - $fps-now >= 1 {
        $fps = [~] "\b" x 40, ' ' x 20, "\b" x 20 ,
            sprintf "FPS: %5.2f  ", ($fps-frames / (now - $fps-now)).round(.01);
        $fps-frames = 0;
        $fps-now = now;
    }
    $fps
}
```



## Phix

```Phix
--
-- demo\rosetta\Forest_fire.exw
--
include pGUI.e

Ihandle dlg, canvas, hTimer
cdCanvas cddbuffer, cdcanvas

constant TITLE = "Forest Fire"

sequence f = {}     -- the forest

atom P = 0.03       -- probability of new tree growing
atom F = 0.00003    -- probability of new fire starting

enum EMPTY,TREE,FIRE    -- (1,2,3)
constant colours = {CD_BLACK,CD_GREEN,CD_YELLOW}

function randomf()
    return rand(1000000)/1000000    -- returns 0.000001..1.000000
end function

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
integer {w, h} = IupGetIntInt(canvas, "DRAWSIZE")
    cdCanvasActivate(cddbuffer)
    if length(f)!=w+2
    or length(f[1])!=h+2 then
        f = sq_rand(repeat(repeat(2,h+2),w+2))  -- (EMPTY or TREE)
    end if
    sequence fn = f
    for x = 2 to w+1 do
        for y = 2 to h+1 do
            integer fnxy
            switch f[x,y] do
                case EMPTY:
                    fnxy = EMPTY+(randomf()<P)  -- (EMPTY or TREE)
                case TREE:
                    fnxy = TREE
                    if f[x-1,y-1]=FIRE or f[x,y-1]=FIRE or f[x+1,y-1]=FIRE
                    or f[x-1,y  ]=FIRE or (randomf()<F) or f[x+1,y  ]=FIRE
                    or f[x-1,y+1]=FIRE or f[x,y+1]=FIRE or f[x+1,y+1]=FIRE then
                        fnxy = FIRE
                    end if
                case FIRE:
                    fnxy = EMPTY
            end switch
            fn[x,y] = fnxy
            cdCanvasPixel(cddbuffer, x-2, y-2, colours[fnxy])
        end for
    end for
    f = fn
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    return IUP_DEFAULT
end function

function key_cb(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    return IUP_CONTINUE
end function

function timer_cb(Ihandle /*ih*/)
    IupUpdate(canvas)
    return IUP_IGNORE
end function

procedure main()
    IupOpen()

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "200x200") -- initial size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))

    dlg = IupDialog(canvas)
    IupSetAttribute(dlg, "TITLE", TITLE)
    IupSetAttribute(dlg, "MAXSIZE", "800x400")  -- (too slow any bigger)
    IupSetCallback(dlg, "K_ANY",     Icallback("key_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    hTimer = IupTimer(Icallback("timer_cb"), 100)

    IupMap(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL) -- release min limitation
    IupShow(dlg)
    IupMainLoop()
    IupClose()
end procedure

main()
```



## PHP


```PHP
<?php

define('WIDTH',      10);
define('HEIGHT',     10);

define('GEN_CNT',    10);
define('PAUSE',  250000);

define('TREE_PROB',  50);
define('GROW_PROB',   5);
define('FIRE_PROB',   1);

define('BARE',      ' ');
define('TREE',      'A');
define('BURN',      '/');


$forest = makeNewForest();

for ($i = 0; $i < GEN_CNT; $i++) {
    displayForest($forest, $i);
    $forest = getNextForest($forest);
}

displayForest($forest, 'done');
exit;


function makeNewForest() {
    return mapForest([
        'func' => function(){
            return isProb(TREE_PROB) ? TREE : BARE;
        }
    ]);
}


function displayForest($forest, $generationNum) {
    system("clear");
    echo PHP_EOL . "Generation: $generationNum" . PHP_EOL;
    mapForest(['forest' => $forest, 'func' => function($f, $x, $y){
            echo $f[$y][$x] . ($x == WIDTH - 1 ? PHP_EOL : '');
        }
    ]);
    echo PHP_EOL;
    usleep(PAUSE);
}


function getNextForest($oldForest) {
    return mapForest(['forest' => $oldForest, 'func' => function($f, $x, $y){
            switch ($f[$y][$x]) {
                case BURN:
                    return BARE;
                case BARE:
                    return isProb(GROW_PROB) ? TREE : BARE;
                case TREE:
                    $caughtFire = isProb(FIRE_PROB);
                    $ablaze = $caughtFire ? true : getNumBurningNeighbors($f, $x, $y) > 0;
                return $ablaze ? BURN : TREE;
            }
        }
    ]);
}


function getNumBurningNeighbors($forest, $x, $y) {
    $burningNeighbors = mapForest([
        'forest' => $forest,
        'x1' => $x - 1, 'x2' => $x + 2,
        'y1' => $y - 1, 'y2' => $y + 2,
        'default' => 0,
        'func' => function($f, $x, $y){
            return $f[$y][$x] == BURN ? 1 : 0;
        }
    ]);

    $numOnFire = 0;
    foreach ($burningNeighbors as $row) {
        $numOnFire += array_sum($row);
    }
    return $numOnFire;
}


function mapForest($params) {
    $p = array_merge([
        'forest' => [],
        'func' => function(){echo "default\n";},
        'x1' => 0,
        'x2' => WIDTH,
        'y1' => 0,
        'y2' => HEIGHT,
        'default' => BARE
    ], $params);

    $newForest = [];
    for ($y = $p['y1']; $y < $p['y2']; $y++) {
        $newRow = [];
        for ($x = $p['x1']; $x < $p['x2']; $x++) {
            $inBounds = ($x >= 0 && $x < WIDTH && $y >= 0 && $y < HEIGHT);
            $newRow[] = ($inBounds ? $p['func']($p['forest'], $x, $y) : $p['default']);
        }
        $newForest[] = $newRow;
    }
    return $newForest;
}


function isProb($prob) {
    return rand(0, 100) < $prob;
}

```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")

(scl 3)

(de forestFire (Dim ProbT ProbP ProbF)
   (let Grid (grid Dim Dim)
      (for Col Grid
         (for This Col
            (=: tree (> ProbT (rand 0 1.0))) ) )
      (loop
         (disp Grid NIL
            '((This)
               (cond
                  ((: burn) "# ")
                  ((: tree) "T ")
                  (T ". ") ) ) )
         (wait 1000)
         (for Col Grid
            (for This Col
               (=: next
                  (cond
                     ((: burn) NIL)
                     ((: tree)
                        (if
                           (or
                              (find  # Neighbor burning?
                                 '((Dir) (get (Dir This) 'burn))
                                 (quote
                                    west east south north
                                    ((X) (south (west X)))
                                    ((X) (north (west X)))
                                    ((X) (south (east X)))
                                    ((X) (north (east X))) ) )
                              (> ProbF (rand 0 1.0)) )
                           'burn
                           'tree ) )
                     (T (and (> ProbP (rand 0 1.0)) 'tree)) ) ) ) )
         (for Col Grid
            (for This Col
               (if (: next)
                  (put This @ T)
                  (=: burn)
                  (=: tree) ) ) ) ) ) )
```

Use:

```txt
(forestFire 26 0.5 0.01 0.001)
```



## PostScript


```PostScript
%!PS-Adobe-3.0
%%BoundingBox: 0 0 400 400

/size 400 def

/rand1 { rand 2147483647 div } def

/m { moveto } bind def
/l { rlineto} bind def
/drawforest {
        0 1 n 1 sub { /y exch def
        0 1 n 1 sub { /x exch def
                forest x get y get dup 0 eq { pop } {
                        1 eq { 0 1 0 } { 1 0 0 } ifelse setrgbcolor
                        x c mul y c mul m
                        c 0 l 0 c l c neg 0 l closepath fill
                } ifelse
        } for
        } for
} def

/r1n { dup 0 ge exch n lt and } def

/neighbors { /y exch def /x exch def /cnt 0 def
        [
        y 1 sub 1 y 1 add { /y1 exch def
                y1 r1n {
                        x 1 sub 1 x 1 add { /x1 exch def
                                x1 r1n { forest x1 get y1 get } if
                        } for
                } if
        } for]
} def

/iter {
        /nf [ n {[ n {0} repeat]} repeat ] def
        0 1 n 1 sub { /x exch def
        0 1 n 1 sub { /y exch def
                nf x get y
                forest x get y get dup
                0 eq { pop rand1 treeprob le {1}{0} ifelse
                } {
                        1 eq {  /fire false def
                                x y neighbors {
                                        -1 eq { /fire true def } if
                                } forall
                                fire {-1}{
                                        rand1 burnprob lt {-1}{1} ifelse
                                } ifelse
                        }{0} ifelse
                } ifelse
                put
        } for } for
        /forest nf def
} def

/n 200 def
/treeprob .05 def
/burnprob .0001 def
/c size n div def
/forest [ n {[ n { rand1 treeprob le {1}{0} ifelse } repeat]} repeat ] def

1000 { drawforest showpage iter } repeat
%%EOF
```



## Python

Just hit return to advance the simulation, or enter an integer to advance that integer amount of 'frames'.
Entering 'p' will print the grid, and 'q' will quit. A summary of the grids status is printed before each prompt for input.

```python
'''
Forest-Fire Cellular automation
 See: http://en.wikipedia.org/wiki/Forest-fire_model
'''

L = 15
# d = 2 # Fixed
initial_trees = 0.55
p = 0.01
f = 0.001

try:
    raw_input
except:
    raw_input = input

import random


tree, burning, space = 'TB.'
hood = ((-1,-1), (-1,0), (-1,1),
        (0,-1),          (0, 1),
        (1,-1),  (1,0),  (1,1))

def initialise():
    grid = {(x,y): (tree if random.random()<= initial_trees else space)
            for x in range(L)
            for y in range(L) }
    return grid

def gprint(grid):
    txt = '\n'.join(''.join(grid[(x,y)] for x in range(L))
                    for y in range(L))
    print(txt)

def quickprint(grid):
    t = b = 0
    ll = L * L
    for x in range(L):
        for y in range(L):
            if grid[(x,y)] in (tree, burning):
                t += 1
                if grid[(x,y)] == burning:
                    b += 1
    print(('Of %6i cells, %6i are trees of which %6i are currently burning.'
          + ' (%6.3f%%, %6.3f%%)')
          % (ll, t, b, 100. * t / ll, 100. * b / ll))


def gnew(grid):
    newgrid = {}
    for x in range(L):
        for y in range(L):
            if grid[(x,y)] == burning:
                newgrid[(x,y)] = space
            elif grid[(x,y)] == space:
                newgrid[(x,y)] = tree if random.random()<= p else space
            elif grid[(x,y)] == tree:
                newgrid[(x,y)] = (burning
                                   if any(grid.get((x+dx,y+dy),space) == burning
                                            for dx,dy in hood)
                                        or random.random()<= f
                                   else tree)
    return newgrid

if __name__ == '__main__':
    grid = initialise()
    iter = 0
    while True:
        quickprint(grid)
        inp = raw_input('Print/Quit/<int>/<return> %6i: ' % iter).lower().strip()
        if inp:
            if inp[0] == 'p':
                gprint(grid)
            elif inp.isdigit():
                for i in range(int(inp)):
                    iter +=1
                    grid = gnew(grid)
                    quickprint(grid)
            elif inp[0] == 'q':
                break
        grid = gnew(grid)
        iter +=1
```


'''Sample output'''

```txt
Of    225 cells,    108 are trees of which      0 are currently burning. (48.000%,  0.000%)
Print/Quit/<int>/<return>      0:
Of    225 cells,    114 are trees of which      1 are currently burning. (50.667%,  0.444%)
Print/Quit/<int>/<return>      1: p
.TTT.T.T.TTTT.T
T.T.T.TT..T.T..
TT.TTTT...T.TT.
TTT..TTTTT.T..T
.T.TTT....TT.TT
...T..TTT.TT.T.
.TT.TT...TT..TT
.TT.T.T..T.T.T.
..TTT.TT.T..T..
.T....T.....TTT
T..TTT..T..T...
TTT....TTTTTT.T
......TBTTT...T
..T....TTTTTTTT
.T.T.T....TT...
Of    225 cells,    115 are trees of which      6 are currently burning. (51.111%,  2.667%)
Print/Quit/<int>/<return>      2: p
.TTT.TTT.TTTT.T
T.T.T.TT..T.T..
TT.TTTT...T.TT.
TTT..TTTTT.T..T
.T.TTT....TT.TT
...T..TTT.TT.T.
.TT.TT...TT..TT
.TT.T.T..T.T.T.
..TTT.TT.T..T..
.T....T.....TTT
T..TTT..T..T...
TTT....BBTTTT.T
....T.B.BTT...T
..T....BBTTTTTT
.T.T.T....TT...
Of    225 cells,    113 are trees of which      4 are currently burning. (50.222%,  1.778%)
Print/Quit/<int>/<return>      3: p
.TTT.TTT.TTTT.T
T.T.T.TT..T.T..
TT.TTTT...T.TT.
TTT..TTTTT.T..T
.T.TTT...TTT.TT
...T..TTT.TTTTT
.TT.TT...TT..TT
.TT.T.T..T.T.T.
..TTT.TT.T..T..
.T.T..T.....TTT
T..TTT..B..T...
TTT......BTTT.T
....T....BT...T
..T......BTTTTT
.T.T.T....TT...
Of    225 cells,    110 are trees of which      4 are currently burning. (48.889%,  1.778%)
Print/Quit/<int>/<return>      4:
```



## Racket


```racket
#lang racket
(require 2htdp/universe)
(require 2htdp/image)

(define (initial-forest w p-tree)
  (for/vector #:length w ((rw w))
    (for/vector #:length w ((cl w))
      (if (< (random) p-tree) #\T #\_))))

(define (has-burning-neighbour? forest r# c# w)
  ;; note, this will check r# c#, too but it's not
  ;; worth checking that r=r# and c=c# each time in
  ;; this case
  (for*/first
      ((r (in-range (- r# 1) (+ r# 2)))
       #:when (< 0 r w)
       (c (in-range (- c# 1) (+ c# 2)))
       #:when (< 0 c w)
       #:when (equal? #\* (vector-ref (vector-ref forest r) c)))
    #t))

(define (fire-tick forest p-sprout f-combust w)
  (for/vector #:length w ((rw forest) (r# (in-naturals)))
    (for/vector #:length w ((cl rw) (c# (in-naturals)))
      (case cl
        ((#\_) (if (< (random) p-sprout) #\T #\_))
        ((#\*) #\_)
        ((#\T)
         (cond
           [(has-burning-neighbour? forest r# c# w) #\*]
           [(< (random) f-combust) #\*]
           [else #\T]))))))

(define (render-forest state)
  (for/fold
      ((scn (empty-scene
             (* (vector-length state) 8)
             (* (vector-length (vector-ref state 0)) 8)
             'black)))

    ((rw state) (r# (in-naturals)))
    (for/fold
        ((scn scn))
      ((cl rw) (c# (in-naturals)))
      (place-image (circle 4 'solid
                           (case cl
                             ((#\_) 'brown)
                             ((#\T) 'green)
                             ((#\*) 'red)))
                   (+ 4 (* c# 8)) (+ 4 (* r# 8)) scn))))

(define (forest-fire p-tree p-sprout f-combust w)
  (big-bang
   (initial-forest w p-tree) ;; initial state
   [on-tick (lambda (state)
              ;(displayln state)
              (fire-tick state p-sprout f-combust w))]
   [to-draw render-forest]))

(forest-fire 0 1/8 1/1024 50)
```


I'll tweak with the parameters for a bit, and when I have some nice
photos I'll post them!


## REXX

This version has been elided,   otherwise the size of the program (with all it's options and optional formatting) would

probably be on the big side for general viewing, and maybe a wee bit complex to demonstrate how to program for this task.

If repeatable results are desired, the   '''randSeed'''   variable can be set to a non-negative integer.

Glyphs were chosen in an attempt to pictorialize a tree   (<big>↑</big>)   and also a fire   (<big>▒</big>).

The choice of glyphs within the code page '''437'''   (DOS and/or under Windows) is rather limited.

There is one (OS) dependency:   use of the   '''CLS'''   (DOS) command which is used to clear the screen   (the original

version examined the host environment and used the correct command to clear the terminal screen).

```txt

             ┌───────────────────────────elided version──────────────────────────┐
             ├─── original version has many more options & enhanced displays. ───┤
             └───────────────────────────────────────────────────────────────────┘

```


```rexx
/*REXX program grows and displays a forest (with growth  and fires caused by lightning).*/
parse value scrSize()  with  sd sw .             /*the size of the terminal display.    */
parse arg generations birth lightning rSeed .    /*obtain the optional arguments from CL*/
if datatype(rSeed,'W')  then call random ,,rSeed /*do we want  RANDOM BIF repeatability?*/
generations = p(generations  100)                /*maybe use  one hundred  generations. */
      birth = p(strip(birth    , ,'%') 50 ) *100 /*calculate the percentage for births. */
  lightning = p(strip(lightning, ,'%') 1/8) *100 /*    "      "       "      " lightning*/
      bare! = ' '                                /*the glyph used to show a bare place. */
      fire! = '▒'                                /*glyph is close to a conflagration.   */
      tree! = '↑'                                /*this is an up─arrow [↑] glyph (tree).*/
       rows = max(12, sd-2)                      /*shrink the usable screen rows by two.*/
       cols = max(79, sw-1)                      /*   "    "     "      "   cols  " one.*/
      every = 999999999                          /*shows a snapshot every Nth generation*/
      field = min(100000, rows*cols)             /*the size of the forest area (field). */
$.=bare!                                         /*forest:  it is now a treeless field. */
@.=$.                                            /*ditto,   for the  "shadow"   forest. */
gens=abs(generations)                            /*use this for convenience.            */
signal on halt                                   /*handle any forest life interruptus.  */
              /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒observe the forest grow and/or burn. */
  do  life=1  for gens                           /*simulate a forest's life cycle.      */
    do   r=1  for rows;     rank=bare!           /*start a forest rank as being bare.   */
      do c=2  for cols;     ?=substr($.r, c, 1);              ??=?
        select                                   /*select the most likeliest choice 1st.*/
        when ?==tree!  then  if ignite?()                then ??=fire!     /*on fire ?  */
        when ?==bare!  then  if random(1, field)<=birth  then ??=tree!     /*new growth.*/
        otherwise                                             ??=bare!     /*it's barren*/
        end   /*select*/                         /* [↑]  when (↑)  if  ≡  short circuit.*/
      rank=rank || ??                            /*build rank:  1 forest "row" at a time*/
      end     /*c*/                              /*ignore column one, start with col two*/
    @.r=rank                                     /*and assign rank to alternate forest. */
    end       /*r*/                              /* [↓]  ··· and, later, yet back again.*/

      do r=1  for rows;   $.r=@.r;   end  /*r*/  /*assign alternate cells ──► real cells*/
  if \(life//every==0 | generations>0 | life==gens)   then iterate
  'CLS'                                          /* ◄─── change this command for your OS*/
        do r=rows  by -1  for rows;   say strip(substr($.r, 2), 'T')    /*a row of trees*/
        end   /*r*/                              /* [↑]  display forest to the terminal.*/
  say right(copies('▒', cols)life, cols)         /*show and tell for a stand of trees.  */
  end         /*life*/
              /*▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒stop observing the forest evolve.    */
halt: if life-1\==gens  then say 'Forest simulation interrupted.' /*was this pgm HALTed?*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
ignite?:           if substr($.r, c+1, 1) == fire!  then return 1   /*is  east on fire? */
         cm=c-1;   if substr($.r, cm , 1) == fire!  then return 1   /* "  west  "   "   */
                                rm=r-1;            rp=r+1           /*test north & south*/
         if pos(fire!, substr($.rm, cm, 3)substr($.rp, cm, 3)) \== 0   then return 1
         return  random(1, field) <= lightning                      /*lightning ignition*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
p:       return word(arg(1), 1)                  /*pick─a─word:  first  or  second word.*/
```

This REXX program makes use of   '''scrSize'''   REXX program (or BIF)   which is used to determine the screen size of the terminal (console).

The   '''SCRSIZE.REX'''   REXX program is included here   ──►    [[SCRSIZE.REX]].


'''output'''   when using the defaults of:
::::*   generations = 100
::::*   rows = 48
::::*   lightning rate = 12.5%
::::*   new growth rate = 50%
::::*   bare character = (a true blank)
::::*   fire character = ▒
::::*   tree character = <big>↑</big>

Shown below is the 10<sup>th</sup> generation   (out of 100).

```txt

↑↑↑↑↑↑↑↑▒▒▒▒▒▒▒▒▒ ↑↑↑↑↑↑  ▒↑↑↑↑▒     ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒     ▒↑↑↑↑↑↑↑↑↑↑↑↑↑▒▒▒↑↑↑
↑↑↑↑↑↑↑↑▒         ↑↑↑↑↑↑↑ ▒↑↑↑↑▒ ↑↑  ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒ ↑   ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
↑↑↑↑↑↑↑↑▒ ↑ ↑     ↑↑↑↑↑↑  ▒↑↑↑↑▒     ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒ ↑↑↑ ▒↑↑▒▒▒▒▒▒▒▒▒▒▒▒▒▒↑↑↑
↑↑↑↑↑↑↑↑▒  ↑↑↑↑  ↑↑  ↑↑↑↑ ▒↑↑↑↑▒ ↑↑↑ ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒ ↑   ▒▒▒▒            ▒↑↑↑
↑↑↑↑↑↑↑↑▒  ↑↑↑↑↑↑ ↑       ▒↑↑↑↑▒     ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒            ↑↑   ↑   ▒↑↑↑
↑↑↑↑↑↑↑↑▒ ↑↑↑↑↑↑↑↑↑ ▒▒▒▒▒▒▒↑↑↑↑▒▒▒▒▒▒▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒▒  ↑↑  ↑↑ ↑↑↑↑▒   ↑  ▒▒↑↑
↑↑↑↑↑↑↑↑▒   ↑↑↑↑↑↑↑ ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒▒▒▒▒▒▒▒▒▒▒▒  ↑↑↑↑↑↑↑   ↑▒ ▒↑↑↑  ▒↑↑
↑↑↑↑↑↑↑↑▒ ↑↑↑↑↑↑↑↑  ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒            ↑↑↑↑↑↑ ↑ ↑ ↑▒  ▒↑↑↑ ▒↑↑
↑↑↑↑↑↑↑↑▒ ↑↑↑↑↑↑↑↑  ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒ ↑  ↑↑↑↑↑↑  ↑↑↑↑↑↑ ↑↑▒▒▒ ↑   ↑↑ ▒↑↑
↑↑↑↑↑↑↑↑▒  ↑↑▒▒▒↑↑↑ ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒ ↑↑↑↑↑  ↑ ↑↑ ↑↑↑↑↑ ↑↑▒  ▒▒▒  ↑  ▒▒▒
↑↑▒↑↑↑↑↑▒  ▒▒▒ ▒ ↑  ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒  ↑  ▒▒▒↑▒↑▒↑ ↑↑ ↑ ↑ ▒   ↑  ↑ ↑
↑↑↑↑↑↑↑↑▒    ▒↑↑ ▒↑ ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒ ↑↑▒  ↑    ▒ ↑  ↑↑ ↑↑▒     ▒↑   ↑
▒▒▒▒▒▒▒▒▒  ↑▒▒  ▒↑↑ ▒▒▒▒▒▒▒▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒     ▒↑  ↑ ▒↑       ↑   ↑   ↑↑↑↑ ↑↑
           ↑ ▒▒▒▒↑         ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒  ▒ ↑↑ ▒▒  ▒   ▒▒   ▒▒      ↑ ↑↑↑↑↑
 ↑ ↑↑  ↑  ↑↑▒ ↑↑↑↑↑↑↑↑↑↑↑  ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒  ▒▒ ▒↑   ▒ ↑↑ ▒▒  ▒ ▒ ▒▒↑▒▒↑↑↑ ↑↑↑
↑↑↑↑↑↑      ↑↑ ↑↑  ↑↑  ↑ ↑ ▒↑↑↑↑↑↑↑↑ ↑↑↑↑↑↑▒  ↑↑ ↑↑   ▒↑↑↑ ▒▒  ▒▒ ↑▒↑↑ ↑↑↑↑↑↑↑↑
↑ ↑↑↑▒ ▒  ↑↑▒▒▒ ▒↑↑↑↑↑↑    ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒  ↑▒   ▒▒▒▒↑↑↑ ▒▒   ↑↑ ▒↑↑↑ ↑↑↑↑↑↑↑
 ↑▒▒ ↑  ↑↑ ▒ ↑▒ ▒▒▒↑       ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒  ↑▒▒▒↑▒↑↑ ↑↑  ▒▒   ↑↑▒▒↑↑↑ ↑↑↑↑↑↑↑
↑ ▒ ↑▒ ▒↑↑ ▒   ↑  ▒ ▒↑↑ ▒▒▒▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒ ↑↑ ↑↑↑↑↑↑↑↑↑  ▒▒▒ ↑↑  ▒↑↑↑↑↑↑↑↑↑↑
  ▒  ↑ ▒↑↑↑↑   ↑↑↑ ▒ ↑  ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒  ↑↑↑↑↑ ↑ ↑↑↑  ▒↑▒  ↑▒ ↑↑↑↑↑↑↑
↑↑↑▒   ▒↑↑↑▒▒  ▒▒       ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒  ↑↑↑ ↑↑↑ ↑↑↑  ▒↑▒  ↑↑↑↑↑↑↑↑↑
↑↑↑▒▒▒▒↑↑  ↑▒ ↑↑ ▒↑  ↑  ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒              ▒▒↑▒  ↑ ↑↑ ↑↑↑↑  ▒▒▒▒
↑↑↑↑↑↑↑↑ ↑↑↑     ↑↑  ↑  ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒↑↑▒ ↑↑↑↑↑↑↑↑↑↑  ▒↑↑↑
↑↑↑↑↑↑ ↑ ↑↑↑▒▒▒ ▒↑  ▒   ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒ ↑  ↑↑↑ ↑↑↑  ▒↑↑↑
↑↑↑↑↑↑↑↑↑  ↑↑↑▒ ↑ ↑▒ ↑  ▒▒▒▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒      ↑↑   ↑ ▒↑↑↑
↑↑↑↑↑↑    ↑ ↑↑↑▒ ▒ ↑↑ ↑    ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒▒            ▒↑↑↑
↑↑↑↑↑   ↑      ↑   ↑       ▒↑↑▒▒▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒▒▒▒▒▒▒▒▒▒▒▒▒▒↑↑↑
↑↑↑↑ ↑  ↑↑↑            ▒   ▒↑↑▒ ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
↑↑ ↑↑↑      ▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒↑↑▒▒▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
 ↑ ↑↑       ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑ ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
       ▒▒▒▒▒▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒▒▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
▒▒▒▒▒▒▒▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒ ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒↑↑↑↑↑▒▒▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒▒▒▒▒▒▒↑↑↑↑↑↑↑↑↑↑
↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒                    ▒▒↑↑↑↑↑↑↑↑↑↑↑▒▒▒▒▒▒▒↑↑↑↑↑↑▒     ▒↑↑↑↑↑↑↑↑↑↑
↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒ ↑↑↑    ↑↑↑↑  ↑↑ ↑   ▒↑↑↑↑↑↑↑↑↑↑↑▒     ▒▒▒▒▒▒▒▒ ↑   ▒↑↑↑↑↑↑↑↑↑↑
▒▒▒▒▒▒▒▒↑↑↑↑↑↑↑▒  ↑↑↑↑↑ ↑ ↑↑  ↑↑↑▒   ▒↑↑↑↑↑↑↑↑↑↑↑▒  ↑           ↑↑  ▒↑↑↑↑↑↑↑↑↑↑
       ▒↑↑↑↑↑↑↑▒  ↑↑↑   ↑↑↑↑    ↑↑▒↑ ▒↑↑↑↑↑↑↑↑↑↑↑▒ ↑↑↑    ↑  ↑  ↑↑  ▒↑▒▒▒▒▒▒▒↑↑
 ↑↑ ↑  ▒↑↑↑↑↑↑↑▒  ↑↑↑↑↑↑↑↑↑   ↑↑↑↑↑  ▒↑↑↑↑↑↑↑↑↑↑↑▒ ↑  ↑ ↑↑↑ ↑ ↑↑↑   ▒↑▒     ▒↑↑
  ↑ ↑↑ ▒↑↑↑↑↑↑↑▒ ↑↑  ↑ ↑  ↑   ↑↑↑↑↑↑ ▒↑↑↑↑↑↑↑↑↑↑↑▒     ↑ ↑↑↑↑↑↑↑  ▒▒▒↑▒↑  ↑ ▒↑↑
  ↑ ↑  ▒▒▒▒↑↑↑↑▒ ↑↑↑ ↑↑      ↑↑↑ ↑↑  ▒↑↑↑↑↑↑↑↑↑↑↑▒▒▒▒   ↑↑ ↑↑↑↑↑  ▒↑↑↑▒ ↑↑↑ ▒↑↑
 ↑↑↑↑     ▒↑↑↑↑▒  ↑↑↑  ↑ ▒▒▒ ↑   ↑↑↑ ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒   ↑ ↑↑↑↑↑↑  ▒↑↑↑▒  ↑  ▒↑↑
 ↑↑↑      ▒↑↑↑↑▒         ▒↑▒         ▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒▒▒  ↑↑↑↑↑↑↑  ▒↑↑↑▒     ▒↑↑
          ▒↑↑↑↑▒▒▒▒▒▒▒▒▒▒▒↑▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒↑↑↑↑↑↑↑↑↑▒  ↑ ↑↑↑↑ ↑ ▒↑↑↑▒▒▒▒▒▒▒↑↑
▒▒▒▒▒▒▒▒▒▒▒↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒     ▒↑▒▒▒↑↑↑↑↑▒   ↑ ↑↑↑↑  ▒↑↑↑↑↑↑↑↑↑↑↑↑
↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒  ↑  ▒↑▒ ▒↑↑↑↑↑▒ ↑↑  ↑ ↑   ▒↑↑↑ ↑↑↑↑↑↑↑↑
↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑▒ ↑   ▒↑▒▒▒↑↑↑↑↑▒           ▒↑↑↑↑↑↑↑↑↑↑↑↑
═════════════════════════════════════════════════════════════════════════════10

```



## Ring


```ring

# Project : Forest fire

load "guilib.ring"
load "stdlib.ring"

paint = null

new qapp
        {
        win1 = new qwidget() {
                  setwindowtitle("Forest fire")
                  setgeometry(100,100,500,600)
                  label1 = new qlabel(win1) {
                              setgeometry(10,10,400,400)
                              settext("")
                  }
                  new qpushbutton(win1) {
                          setgeometry(150,500,100,30)
                          settext("draw")
                          setclickevent("draw()")
                  }
                  show()
        }
        exec()
        }

func draw
        p1 = new qpicture()
               color = new qcolor() {
               setrgb(0,0,255,255)
        }
        pen = new qpen() {
                 setcolor(color)
                 setwidth(1)
        }
        paint = new qpainter() {
                  begin(p1)
                  setpen(pen)

pregen = newlist(200,200)
newgen = newlist(200,200)

 for gen = 1 to 20
      see "gen = " + gen + nl
      for x = 1 to 199
           for y = 1 to 199
                 switch pregen[x][y]
                            on 0
                                 if random(9)/10 > 0.099
                                    newgen[x][y] = 1
                                    color = new qcolor()
                                    color.setrgb(0,128,0,255)
                                    pen.setcolor(color)
                                    setpen(pen)
                                    drawpoint(x,y)
                                 ok
                            on 2
                                 newgen[x][y] = 0
                                 color = new qcolor()
                                 color.setrgb(165,42,42,255)
                                 pen.setcolor(color)
                                 setpen(pen)
                                 drawpoint(x,y)
                            on 1
                                 if pregen[x][y] = 2 or pregen[x][y]   = 2 or pregen[x][y+1] = 2 or
                                    pregen[x][y]   = 2 or pregen[x][y+1]   = 2 or pregen[x+1][y] = 2 or
                                    pregen[x+1][y]   = 2 or pregen[x+1][y+1] = 2 or random(9)/10 > 0.0999
                                    color = new qcolor()
                                    color.setrgb(255,0,0,255)
                                    pen.setcolor(color)
                                    setpen(pen)
                                    drawpoint(x,y)
                                    newgen[x][y] = 2
                                 ok
                 off
                 pregen[x][y] = newgen[x][y]
           next
      next
next

        endpaint()
        }
        label1 { setpicture(p1) show() }
        return

```

Output:

https://www.dropbox.com/s/6rjho62odzyqaqc/ForestFire.jpg?dl=0


## Ruby


```ruby
class Forest_Fire
  Neighborhood = [-1,0,1].product([-1,0,1]) - [0,0]
  States = {empty:" ", tree:"T", fire:"#"}

  def initialize(xsize, ysize=xsize, p=0.5, f=0.01)
    @xsize, @ysize, @p, @f = xsize, ysize, p, f
    @field = Array.new(xsize+1) {|i| Array.new(ysize+1, :empty)}
    @generation = 0
  end

  def evolve
    @generation += 1
    work = @field.map{|row| row.map{|cell| cell}}
    for i in 0...@xsize
      for j in 0...@ysize
        case cell=@field[i][j]
        when :empty
          cell = :tree  if rand < @p
        when :tree
          cell = :fire  if fire?(i,j)
        else
          cell = :empty
        end
        work[i][j] = cell
      end
    end
    @field = work
  end

  def fire?(i,j)
    rand < @f or Neighborhood.any? {|di,dj| @field[i+di][j+dj] == :fire}
  end

  def display
    puts "Generation : #@generation"
    puts @xsize.times.map{|i| @ysize.times.map{|j| States[@field[i][j]]}.join}
  end
end

forest = Forest_Fire.new(10,30)
10.times do |i|
  forest.evolve
  forest.display
end
```

Sample Output:

```txt
Generation : 1
 TT TTTT  TT    TT  T T  T TTT
 T TTT    T   TTT T  T T T  T
TT   T TTT T T   T    T TTT T
T  TT T     T   TT   TTT T T
 TTTT    TTTTTTT      TT   T
T  T  T  TT   T        TTT TT
 TT TT TTT   TT TTT     T T
T   TTTTT   TT  TT T TTT   TT
T TTT    T T  T T T      TT  T
 TTTTT T  TT    TTT TT T T   T
Generation : 2
TTTTTTTT TTTT TTTTTTTTT  TTTTT
T# TTTTTTTTT  TTTTTT T T T  TT
TT   # TTTTT T T TTTTTTTTTT TT
T  TTTTT T TT TTTTTT TTT TTT
TTTTTTT TTTTTTTT  T   TTT  TTT
TTTT  T  TTT TTT   TT  TTTTTTT
 TT TT TTTT TTT TTTT TTTT TT T
T T TTTTTT TTTTTTT T TTTTTTTTT
TTTTT TTTTTT  T T T TT TTTT TT
 TTTTTTT  TTTTT TTTTTT T T  TT
Generation : 3
###TTTTT TTTT TTTTTTTTTTTTTTTT
#  T####TTTTT TTTTTT TTTTTT TT
##TT   TTTTTTT TTTTTTTTTTTT TT
TT T###T T TTTTTTTTTTTTTTTTT
TTTTTTTTTTTTTTTTTTTT TTTTTTTTT
TTTTT T  TTTTTTT TTTT  TTTTTTT
 TT TTTTTTT TTT TTTT TTTTTTT T
TTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
TTTTT TTTTTT  T TTTTTTTT#TTTTT
TTTTTTTT  TTTTTT#TTTTT TTTT TT
Generation : 4
   ##### TTTT TTTTTTTTTTTTTTTT
 T #    #TTTT TTTTTTTTTTTTT TT
  ##   ##TTTTTTTTTTTTTTTTTTTTT
## #   # T TTTTTTTTTTTTTTTTTTT
TTT#####TTTTTTTTTTTT TTTTTTTTT
TTTTTTT TTTTTTTTTTTTT TTTTTTTT
 TTTTTTTTTTTTTTTTTTT TTTTTTTTT
TTTTTTTTTTTTTTTTTTTTTTT###TTTT
TTTTTTTTTTTTTTTT##TTTTT# #TTTT
TTTTTTTT  TTTTT# #TTTT ###TTTT
Generation : 5
        T#TTTTTTTTTTTTTTTTTTTT
 #T TTTT #TTT TTTTTTTTTTTTT TT
 T    T  #TTTTTTTTTTTTTTTTTTTT
    T    #TTTTT#TTTTTT#TTTTTTT
###     #TTTTTTTTTTT TTTTTTTTT
TT##### #TTTTTTTTTTTTTTTTTTTTT
TTTTTTTTTTTTTTTTTTTTTT#####TTT
TTTTTTTTTTTTTTT####TTT#   #TTT
TTTTTTTTTTTTTT##  #TTT# T #TTT
TTTTTTTT  TTTT# T #TTT    #TTT
Generation : 6
   T T  # ##TTTTTTTTTTTTTTTTTT
T #TTTTT  #TT TTTTTTTTTTTTT TT
T#TT  TTT #TTT###TTTT###TTTTTT
TTTTTT  T #TTT# #TTTT# #TTTTTT
   TTT   ##TTT###TTTT###TTTTTT
##       #TTTTTTTTTTT#######TT
T#########TTT#######T#     #TT
TTTTTTTTTTTTT##    #T#   T #TT
TTTTTTTTTTTTT#  TT #T# TTT #TT
TTTTTTTT  TTT#  #T #T#T    #TT
Generation : 7
TT # T   T  #TTTTTTTTTTTTTTTTT
#T #TTT# T ## ####TT#####TTTTT
# ##TTTTTT #T#   #TT#   #TTTTT
###TTTTT#  #T# T #TT# T #TTTTT
   TTTT T  #T#   #TT#   #####T
  T TTT T ###########       #T
#         #T#       # T TTT #T
###########T#   TT  #    T  #T
TTTTTTTTTTTT# TT##T #  TTT  #T
TTTTTT#T TTT#    #T # #T TT #T
Generation : 8
##  TT   T   #############TTTT
 #  #T# TT   T    ##     #TTTT
 T  #T####  #  T  ##   T #TTTT
   ##TT#  T #  TT ## TTT #####
  T#TTTT#TT # TT  ## TT      #
T T TTTTT            TTT T   #
 TT   TTTT #  TTT T   T TTT  #
           #    ##T   TTTT T #
############  T#  #T   #TTTT #
TTTTT# #TTT# TT   #T T # TT  #
Generation : 9
  TT##T  TTT              #TTT
T TT #  ##TTT#T       TTT #TTT
 #T  #    T    TTT   TTT  ####
  T  ##  T#T  TTTT  TTTT
TT# #### #T   TT    TTT T T T
T # #TT## T    TTT T TTT TT T
 TTT TTTTT  T T## #T  T TTT
    TT    T TT    #TTT###TTT
             T# T  #TT  #TTTT
#####   ###  T#T   # TT TTT
Generation : 10
  T#  # T##T  TTTT TTT     #TT
#T##T     #T# #TTTTT  TTT  ###
T # T TTTT#T  TTTT TTTTTTT
  # T  T # #T TTTT  TTTTT
T# T    T #TT TTT   TT# TTT T
T    ##   #TT  ###T#TTTTTTTTTT
T### #####TTTT#  T #T # ##T TT
   TTTTTTTTTT#TTT  ###   #TT
TTTT  TTTT T # TT T ## T #TTT
      T      # # TT  TT ##TTT

```


## Rust

Inspired by the perl6 implementation, this runs in the terminal, printing a colored ASCII rendition of the forest (and it's fires!). You can configure the size of the forest, frame delay, and various probabilities.
```rust
extern crate rand;
extern crate ansi_term;

#[derive(Copy, Clone, PartialEq)]
enum Tile {
    Empty,
    Tree,
    Burning,
    Heating,
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let output = match *self {
            Empty => Black.paint(" "),
            Tree => Green.bold().paint("T"),
            Burning => Red.bold().paint("B"),
            Heating => Yellow.bold().paint("T"),
        };
        write!(f, "{}", output)
    }
}

// This has been added to the nightly rust build as of March 24, 2016
// Remove when in stable branch!
trait Contains<T> {
    fn contains(&self, T) -> bool;
}

impl<T: PartialOrd> Contains<T> for std::ops::Range<T> {
    fn contains(&self, elt: T) -> bool {
        self.start <= elt && elt < self.end
    }
}

const NEW_TREE_PROB: f32 = 0.01;
const INITIAL_TREE_PROB: f32 = 0.5;
const FIRE_PROB: f32 = 0.001;

const FOREST_WIDTH: usize = 60;
const FOREST_HEIGHT: usize = 30;

const SLEEP_MILLIS: u64 = 25;

use std::fmt;
use std::io;
use std::io::prelude::*;
use std::io::BufWriter;
use std::io::Stdout;
use std::process::Command;
use std::time::Duration;
use rand::Rng;
use ansi_term::Colour::*;

use Tile::{Empty, Tree, Burning, Heating};

fn main() {
    let sleep_duration = Duration::from_millis(SLEEP_MILLIS);
    let mut forest = [[Tile::Empty; FOREST_WIDTH]; FOREST_HEIGHT];

    prepopulate_forest(&mut forest);
    print_forest(forest, 0);

    std::thread::sleep(sleep_duration);

    for generation in 1.. {

        for row in forest.iter_mut() {
            for tile in row.iter_mut() {
                update_tile(tile);
            }
        }

        for y in 0..FOREST_HEIGHT {
            for x in 0..FOREST_WIDTH {
                if forest[y][x] == Burning {
                    heat_neighbors(&mut forest, y, x);
                }
            }
        }

        print_forest(forest, generation);

        std::thread::sleep(sleep_duration);
    }
}

fn prepopulate_forest(forest: &mut [[Tile; FOREST_WIDTH]; FOREST_HEIGHT]) {
    for row in forest.iter_mut() {
        for tile in row.iter_mut() {
            *tile = if prob_check(INITIAL_TREE_PROB) {
                Tree
            } else {
                Empty
            };
        }
    }
}

fn update_tile(tile: &mut Tile) {
    *tile = match *tile {
        Empty => {
            if prob_check(NEW_TREE_PROB) == true {
                Tree
            } else {
                Empty
            }
        }
        Tree => {
            if prob_check(FIRE_PROB) == true {
                Burning
            } else {
                Tree
            }
        }
        Burning => Empty,
        Heating => Burning,
    }
}

fn heat_neighbors(forest: &mut [[Tile; FOREST_WIDTH]; FOREST_HEIGHT], y: usize, x: usize) {
    let neighbors = [(-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1)];

    for &(xoff, yoff) in neighbors.iter() {
        let nx: i32 = (x as i32) + xoff;
        let ny: i32 = (y as i32) + yoff;
        if (0..FOREST_WIDTH as i32).contains(nx) && (0..FOREST_HEIGHT as i32).contains(ny) &&
           forest[ny as usize][nx as usize] == Tree {
            forest[ny as usize][nx as usize] = Heating
        }
    }
}

fn prob_check(chance: f32) -> bool {
    let roll = rand::thread_rng().gen::<f32>();
    if chance - roll > 0.0 {
        true
    } else {
        false
    }
}

fn print_forest(forest: [[Tile; FOREST_WIDTH]; FOREST_HEIGHT], generation: u32) {
    let mut writer = BufWriter::new(io::stdout());
    clear_screen(&mut writer);
    writeln!(writer, "Generation: {}", generation + 1).unwrap();
    for row in forest.iter() {
        for tree in row.iter() {
            write!(writer, "{}", tree).unwrap();
        }
        writer.write(b"\n").unwrap();
    }
}

fn clear_screen(writer: &mut BufWriter<Stdout>) {
    let output = Command::new("clear").output().unwrap();
    write!(writer, "{}", String::from_utf8_lossy(&output.stdout)).unwrap();
}

```



## Sather


```sather
class FORESTFIRE is
  private attr fields:ARRAY{ARRAY{INT}};
  private attr swapu:INT;
  private attr rnd:RND;
  private attr verbose:BOOL;
  private attr generation:INT;
  readonly attr width, height:INT;
  const empty:INT := 0;
  const tree:INT := 1;
  const burning:INT := 2;

  attr prob_tree, prob_p, prob_f :FLT;

  create(w, h:INT, v:BOOL):SAME is
    res:FORESTFIRE := new;
    res.fields := #(2);
    res.fields[0] := #(w*h);
    res.fields[1] := #(w*h);
    res.width := w; res.height := h;
    res.swapu := 0;
    res.prob_tree := 0.55;
    res.prob_p := 0.001;
    res.prob_f := 0.00001;
    res.rnd := #RND;
    res.verbose := v;
    res.generation := 0;
    res.initfield;
    return res;
  end;

  -- to give variability
  seed(i:INT) is
    rnd.seed(i);
  end;

  create(w, h:INT):SAME is
    res ::= create(w, h, false);
    return res;
  end;

  initfield is
    n ::= 0;
    swapu := 0;
    if verbose and generation > 0 then
      #ERR + "Previous generation " + generation + "\n";
    end;
    generation := 0;
    loop i ::= 0.upto!(width-1);
      loop j ::= 0.upto!(height-1);
        if rnd.uniform > prob_tree.fltd then
          cset(i, j, empty);
        else
	  n := n + 1;
          cset(i, j, tree);
        end;
      end;
    end;
    if verbose then
      #ERR + #FMT("Field size is %dx%d (%d)", width, height, size) + "\n";
      #ERR + "There are " + n + " trees (" + (100.0*n.flt/size.flt) + "%)\n";
      #ERR + "prob_tree = " + prob_tree + "\n";
      #ERR + "prob_f = " + prob_f + "\n";
      #ERR + "prob_p = " + prob_p + "\n";
      #ERR + "ratio = " + prob_p/prob_f + "\n";
    end;
  end;

  field:ARRAY{INT} is
    return fields[swapu];
  end;

  ofield:ARRAY{INT} is
    return fields[swapu.bxor(1)];
  end;

  size:INT is
    return width*height;
  end;

  set(i, j, t:INT)
    pre bcheck(i, j)
  is
    ofield[j*width + i] := t;
  end;

  cset(i, j, t:INT)
    pre bcheck(i, j)
  is
    field[j*width + i] := t;
  end;

  private bcheck(i, j:INT):BOOL is
    if i.is_between(0, width-1) and j.is_between(0, height-1) then
      return true; -- is inside
    else
      return false; -- is outside
    end;
  end;

  get(i, j:INT):INT is
    if ~bcheck(i, j) then
      return empty;
    end;
    return field[j*width + i];
  end;

  oget(i, j:INT):INT is
    if ~bcheck(i, j) then
      return empty;
    end;
    return ofield[j*width + i];
  end;

  burning_neighbor(i, j:INT):BOOL is
    loop x ::= (-1).upto!(1);
      loop y ::= (-1).upto!(1);
        if x /= y then
          if get(i+x, j+y) = burning then return true; end;
        end;
      end;
    end;
    return false;
  end;

  evolve is
    bp ::= 0;
    loop i ::= 0.upto!(width-1);
      loop j ::= 0.upto!(height-1);
	case get(i, j)
        when burning then set(i, j, empty); bp := bp + 1;
        when empty then
          if rnd.uniform > prob_p.fltd then
            set(i, j, empty);
          else
            set(i, j, tree);
          end;
        when tree then
          if burning_neighbor(i, j) then
            set(i, j, burning);
          else
            if rnd.uniform > prob_f.fltd then
              set(i, j, tree);
            else
              set(i, j, burning);
            end;
          end;
        else
          #ERR + "corrupted field\n";
        end;
      end;
    end;
    generation := generation + 1;
    if verbose then
      if bp > 0 then
        #ERR + #FMT("Burning at gen %d: %d\n", generation-1, bp);
      end;
    end;
    swapu := swapu.bxor(1);
  end;

  str:STR is
    s ::= "";
    loop j ::= 0.upto!(height -1);
      loop i ::= 0.upto!(width -1);
        case get(i, j)
          when empty then s := s + ".";
          when tree then s := s + "Y";
          when burning then s := s + "*";
        end;
      end;
      s := s + "\n";
    end;
    s := s + "\n";
    return s;
  end;

end;

class MAIN is

  main is
    forestfire ::= #FORESTFIRE(74, 40);
    -- #FORESTFIRE(74, 40, true) to have some extra info
    -- (redirecting stderr to a file is a good idea!)

    #OUT + forestfire.str;
    -- evolve 1000 times
    loop i ::= 1000.times!;
      forestfire.evolve;
      -- ANSI clear screen sequence
      #OUT + 0x1b.char + "[H" + 0x1b.char + "[2J";
      #OUT + forestfire.str;
    end;
  end;

end;
```



## Scala


```scala
import scala.util.Random

class Forest(matrix:Array[Array[Char]]){
  import Forest._
  val f=0.01;	 // auto combustion probability
  val p=0.1;	 // tree creation probability
  val rows=matrix.size
  val cols=matrix(0).size

  def evolve():Forest=new Forest(Array.tabulate(rows, cols){(y,x)=>
    matrix(y)(x) match {
      case EMPTY => if (Random.nextDouble<p) TREE else EMPTY
      case BURNING => EMPTY
      case TREE => if (neighbours(x, y).exists(_==BURNING)) BURNING
                  else if (Random.nextDouble<f) BURNING else TREE
    }
  })

  def neighbours(x:Int, y:Int)=matrix slice(y-1, y+2) map(_.slice(x-1, x+2)) flatten
  override def toString()=matrix map (_.mkString("")) mkString "\n"
}

object Forest{
  val TREE='T'
  val BURNING='#'
  val EMPTY='.'
  def apply(x:Int=30, y:Int=15)=new Forest(Array.tabulate(y, x)((y,x)=> if (Random.nextDouble<0.5) TREE else EMPTY))
}
```



```scala
object ForestFire{
  def main(args: Array[String]): Unit = {
    var l=Forest()
    for(i <- 0 until 20){
      println(l+"\n-----------------------")
      l=l.evolve
    }
  }
}
```

Sample output:

```txt
.T..TTT.TT    .T..TTT.TT    TT..TTT.TT    TT..TTTTTT    TT..TTTTTT
TTT.TTTT..    TTT.TTTTT.    TTT.TTTTT.    TTT.TTTTT.    TTT.TTTTT.
.T...T..T.    .TT..T..T.    .TT..T.TT.    .TT.TT.TTT    .TT.##.TTT
T...TT.T.T    T...TT.T.T    T...TT.T.T    T.TT##.T.T    T.T#...T.T
.T..TTTTTT    .T..TTTTTT    .T..#TTTTT    .T...#TTTT    .T....#TTT
TTT..TTTT.    TTT..TTTT.    TTT..TTTT.    TTT..#TTT.    ###...##T.
TT.TTTTTTT    TT.TTTTTTT    TT.TTTTTTT    ##.TTT#TTT    ...###.#TT
......TT..    T.....TT..    #.T.TTTT..    .T#.TTTT..    .#..####..
.TTT.TTTTT    .#TT.TTTTT    ..#T.TTTTT    ...#.TTTTT    .....TTTTT
T.T.TTT.T.    TTT.TTT.T.    ###.TTT.T.    ...TTTT.T.    T..##TT.T.
```



## Sidef

```ruby
define w = `tput cols`.to_i-1
define h = `tput lines`.to_i-1
define r = "\033[H"

define red = "\033[31m"
define green = "\033[32m"
define yellow = "\033[33m"

define chars = [' ', green+'*', yellow+'&', red+'&']

define tree_prob = 0.05
define burn_prob = 0.0002

enum |Empty, Tree, Heating, Burning|

define dirs = [
    %n(-1 -1), %n(-1 0), %n(-1 1), %n(0 -1),
    %n(0   1), %n(1 -1), %n(1  0), %n(1  1),
]

var forest = h.of { w.of { 1.rand < tree_prob ? Tree : Empty } }

var range_h = h.range
var range_w = w.range

func iterate {
    var new = h.of{ w.of(0) }
    for i in range_h {
        for j in range_w {
            given (new[i][j] = forest[i][j]) {
              when (Tree) {
                1.rand < burn_prob && (new[i][j] = Heating; next)
                dirs.each { |pair|
                    var y = pair[0]+i
                    range_h.contains(y) || next
                    var x = pair[1]+j
                    range_w.contains(x) || next
                    forest[y][x] == Heating && (new[i][j] = Heating; break)
                }
              }
              when (Heating)            { new[i][j] = Burning }
              when (Burning)            { new[i][j] = Empty   }
              case (1.rand < tree_prob) { new[i][j] = Tree    }
            }
        }
    }
    forest = new
}

STDOUT.autoflush(true)

func init_forest {
    print r
    forest.each { |row|
        print chars[row]
        print "\033[E\033[1G"
    }
    iterate()
}

loop { init_forest() }
```


OO approach:

```ruby
define RED = "\e[1;31m"
define YELLOW = "\e[1;33m"
define GREEN = "\e[1;32m"
 
define DIRS = [
    [-1, -1], [0, -1], [1, -1],
    [-1,  0],          [1,  0],
    [-1,  1], [0,  1], [1,  1],
]
 
enum (Empty, Tree, Heating, Burning)
define pix = [' ', GREEN + "*", YELLOW + "*", RED + "*"]
 
class Forest(p=0.01, f=0.001, height, width) {
 
    has coords = []
    has spot = []
    has neighbors = []
 
    method init {
        coords = (0..height ~X 0..width)
        spot = height.of { width.of { [true, false].pick ? Tree : Empty } }
        self.init_neighbors
    }
 
    method init_neighbors {
        for i,j in coords {
            neighbors[i][j] = gather {
                for dir in DIRS {
                    take(\(spot[i + dir[0]][j + dir[1]] \\ next))
                 }
            }
        }
    }
 
    method step {
        var heat = []
 
        for i,j in coords {
            given (spot[i][j]) {
                when Empty   { spot[i][j] = Tree    if (1.rand < p) }
                when Tree    { spot[i][j] = Heating if (1.rand < f) }
                when Heating { spot[i][j] = Burning; heat << [i, j] }
                when Burning { spot[i][j] = Empty }
            }
        }
 
        for i,j in heat {
            neighbors[i][j].each { |ref|
                *ref = Heating if (*ref == Tree)
            }
        }
    }
 
    method show {
        for i in ^height {
            say pix[spot[i]]
        }
    }
}

STDOUT.autoflush(true)
var(height, width) = `stty size`.nums.map{.dec}...
 
var forest = Forest(height: height, width: width)
print "\e[2J"

loop {
    print "\e[H"
    forest.show
    forest.step
}
```



## Tcl


```tcl
package require Tcl 8.5

# Build a grid
proc makeGrid {w h {treeProbability 0.5}} {
    global grid gridW gridH
    set gridW $w
    set gridH $h
    set grid [lrepeat $h [lrepeat $w " "]]
    for {set x 0} {$x < $w} {incr x} {
	for {set y 0} {$y < $h} {incr y} {
	    if {rand() < $treeProbability} {
		lset grid $y $x "#"
	    }
	}
    }
}

# Evolve the grid (builds a copy, then overwrites)
proc evolveGrid {{fireProbability 0.01} {plantProbability 0.05}} {
    global grid gridW gridH
    set newGrid {}
    for {set y 0} {$y < $gridH} {incr y} {
	set row {}
	for {set x 0} {$x < $gridW} {incr x} {
	    switch -exact -- [set s [lindex $grid $y $x]] {
		" " {
		    if {rand() < $plantProbability} {
			set s "#"
		    }
		}
		"#" {
		    if {[burningNeighbour? $x $y] || rand() < $fireProbability} {
			set s "o"
		    }
		}
		"o" {
		    set s " "
		}
	    }
	    lappend row $s
	}
	lappend newGrid $row
    }
    set grid $newGrid
}

# We supply the neighbourhood model as an optional parameter (not used...)
proc burningNeighbour? {
    x y
    {neighbourhoodModel {-1 -1  -1 0  -1 1  0 -1  0 1  1 -1  1 0  1 1}}
} {
    global grid gridW gridH
    foreach {dx dy} $neighbourhoodModel {
	set i [expr {$x + $dx}]
	if {$i < 0 || $i >= $gridW} continue
	set j [expr {$y + $dy}]
	if {$j < 0 || $j >= $gridH} continue
	if {[lindex $grid $j $i] eq "o"} {
	    return 1
	}
    }
    return 0
}

proc printGrid {} {
    global grid
    foreach row $grid {
	puts [join $row ""]
    }
}

# Simple main loop; press Return for the next step or send an EOF to stop
makeGrid 70 8
while 1 {
    evolveGrid
    printGrid
    if {[gets stdin line] < 0} break
}
```

Sample output:

```txt

###  #     ####### ##  #  ## #####     # # # ###   ## #
#  #      ##   #   ##### # ## #   #   ##   o ###  #  # #### # # #### #
  # #######  ###   #####  ###  ####  #######  ###   ##  ## ####  # ##
# ###   ## ####       #     ##  #        #  #### # ### #  # ##  #####
 # #    ##  #     ##### ###  # ## # ##    ######    # ####     ## # #
    ### ### #   #####  # ###  ## # ### # ####### #### # # # #   #  #
 # # # # #  ####  ### #  ##  ##  ### #  ## # #   # #    # ## #   ## ##
#####    ## ## #  #  # # ##   # ##  ###   # # #   ### ##    ## # ### #

#  ### # ### #####  #  #  ####### ##  #  #o o####     # # # ###   ## #
#  #  #   #o   #   ##### # ## ##  #   ##     ###  #  # #### # # #### #
  # #######  ###   #####  ###  ####  #####oo  ###   ### ## ####  # ##
# ###   ## ####       #     ##  #        #  #### # ### #  # ##  #####
 # #    ##  #     ##### ###  # ## # ##    ######    # #o##     ## # #
    ### ### #   ###### # ###  ## # ### # ####### #### # # # #   #  #
 # # # # #  ####  ### #  ##  ##  ### #  ## # #   # #    # ## #   ## ##
o####    ## ## #  #  # # ##   # ##  ###   # # #   ### ##  # ## # ### #

#  ### # #oo o####  #  # ######## ##  #  o   o###    ## # # #o#   ## #
#  #  #   o    #   ##### # ## ##  #   ##     o##  #  # #### # # ##o# #
  # ######o  ###   #####  #### ####  ####o    ### # ### ## #### ## ##
#####   ## ####       #     ##  #     #  o  o### # ### o  # ##  #####
 # #    ##  ##    ##### ###  # ## # ##    ######    # o o#     ## # #
    ### #####   ###### # ###  ## # ### # ####### #### o o # # # o  #
 o # # # #  ####  #####  ## ###  ### #  ## # #   # #    # ## #   ## ##
 o###    ## ## #  #  # # ##   # ##  ###   # ###   ### ##  # ## # #o# #

#  ### # o    o###  #  # ######## ##  #       o##    ## # # o o   oo##
#  #  # #   #  #   ##### # ## ##  #   ##      o#  #  # #### o o #o o #
  # #####o   ###   #####  #### ####  ###o     o## ####o o# #### #o o#
#####   #o o###       #    ###  #     #      o## # ##o    # ##  ######
 # #    ##  ##    ##### ###  # ## # ##    oooo##    #    o     oo # #
    ### #####   ###### #####  ## # ### # ####### ####     # # o    #
   # # # #  ####  #####  ## ###  ### #  ## # #   # #    o### #   oo ##
  o##    ## ## #  #  # # ##   # o#  ###   # ###   ######  # ## # o o #

# #### #       o##  #  # ######## ##  #        o#    ## # #     #   o#
#  #  # o   #  o   ##### # ## ##  #   #o       o  #  o ooo#     o #  #
  # ####o    ###   #####  o### ####  ##o     # o# ##oo   o oooo#o   o#
######  o   o##    #  #    ###  #     #       oo # #o     o ##  ooooo#
 # #    oo  o# #  ##### ###  # ## # ##        o#   #o   #         # #
    ### #####   ###### #####  ## # ### # oooooo# ####     o #    # o
   o # # #  ####  #####  ## ###  o## #  ## # #   # #     o## o#    #o#
   o#    ## ## #  #  # # ##  ##  o  ###   # ### # #####o  # ## #     #

```



## uBasic/4tH

It's a small forest, since it's a small interpreter.
<lang>B = 1                                  ' A burning tree
E = 16                                 ' An empty space
T = 256                                ' A living tree

Input "%Chance a tree will burn: ";F   ' Enter chance of combustion
Input "%Chance a tree will grow: ";P   ' Enter chance of a new tree

Proc _CreateForest                     ' Now create a new forest

Do
  Proc _PrintForest                    ' Print the current forest
  Input "Press '1' to continue, '0' to quit: ";A
  Proc _BurnForest                     ' See what happens
  Proc _UpdateForest                   ' Update from buffer
  While A                              ' Until the user has enough
Loop                                   ' and answers with zero

End


_CreateForest                          ' Create an entire new forest
  Local(1)

  For a@ = 0 to 120                    ' For each main cell determine
    If RND(100) < P Then               ' if a tree will grow here
      @(a@) = T                        ' Ok, we got a tree
    Else                               ' Otherwise it remains empty
      @(a@) = E
    EndIf
  Next
Return


_BurnForest                            ' Now the forest starts to burn
  Local(2)

  For a@ = 0 To 10                     ' Loop vertical
    For b@ = 0 To 10                   ' Loop horizontal
      If @((a@ * 11) + b@) = B Then @((a@ * 11) + b@ + 121) = E
                                       ' A tree has been burned flat
      If @((a@ * 11) + b@) = E Then    ' For each open space determine
        If RND(100) < P Then           ' if a tree will grow here
          @((a@ * 11) + b@ + 121) = T
        Else                           ' Otherwise it remains an empty space
          @((a@ * 11) + b@ + 121) = E
        EndIf
      EndIf

      If @((a@ * 11) + b@) = T Then    ' A tree grows here
        If RND(100) < F Then           ' See if it will spontaneously combust
          @((a@ * 11) + b@ + 121) = B
        Else                           ' No, then see if it got any burning
          @((a@ * 11) + b@ + 121) = FUNC(_BurningTrees(a@, b@))
        EndIf                          ' neighbors that will set it ablaze
      EndIf

    Next
  Next
Return


_UpdateForest                          ' Update the main buffer
  Local(1)

  For a@ = 0 To 120                    ' Move from temporary buffer to main
    @(a@) = @(a@+121)
  Next
Return


_PrintForest                           ' Print the forest on screen
  Local(2)
  Print                                ' Let's make a little space

  For a@ = 0 To 10                     ' Loop vertical
    For b@ = 0 To 10                   ' Loop horizontal
      If @((a@ * 11) + b@) = B Then    ' This is a burning tree
        Print " *";
      Else                             ' Otherwise..
        If @((a@ * 11) + b@) = E Then  ' It may be an empty space
          Print "  ";
        Else                           ' Otherwise
          Print " @";                  ' It has to be a tree
        EndIf
      EndIf
    Next
    Print                              ' Terminate row
  Next

  Print                                ' Terminate map
Return


_BurningTrees Param(2)                 ' Check the trees environment
  Local(2)

  For c@ = a@-1 To a@+1                ' Loop vertical -1/+1
    If c@ < 0 Then Continue            ' Skip top edge
  Until c@ > 10                        ' End at bottom edge
    For d@ = b@-1 To b@+1              ' Loop horizontal -1/+1
      If d@ < 0 Then Continue          ' Skip left edge
    Until d@ > 10                      ' End at right edge
      If @((c@ * 11) + d@) = B Then Unloop : Unloop : Return (B)
    Next                               ' We found a burning tree, exit!
  Next                                 ' Try next row

Return (T)                             ' No burning trees found
```

```txt
   @ @   @       @
 @ @   @   @   @ @
 @ @           @ @
         @ @ @
 @         @ @ @   @
                 @
 @             @
 @ *           *
   *
   *       @       @


Press '1' to continue, '0' to quit: 1

   @ @   @       @
 @ @   @   @   @ @
 @ @           @ @
         @ @ @
 @         @ @ @   @
           @     @
 *             *
 *         @
               @
           @     @ @
     @

Press '1' to continue, '0' to quit: 0

0 OK, 0:1236
```



## Vedit macro language

This macro shows an example of using search in columnar block.
Instead of checking all the 8 neighboring cells separately, a search in 3x3 character block is performed to check if there is fire.

Note: In order to display the graphics characters correctly, use DOS (OEM) font such as "Terminal".

```vedit
#1 = 25         // height of the grid
#2 = 60         // width of the grid
#3 = 2          // probability of random fire, per 1000
#4 = 40         // probability of new tree, per 1000

#5 = #2+2+Newline_Chars                 // total length of a line
#90 = Time_Tick                         // seed for random number generator
#91 = 1000                              // get random numbers in range 0 to 999

// Fill the grid and draw border
Buf_Switch(Buf_Free)
Ins_Char('-', COUNT, #2+2)
Ins_Newline
for (#11=0; #11<#1; #11++) {
    Ins_Char('|')
    for (#12=0; #12<#2; #12++) {
        Call("RANDOM")
        if (Return_Value < 500) {               // 50% propability for a tree
            Ins_Char('♠')
        } else {
            Ins_Char(' ')
        }
    }
    Ins_Char('|')
    Ins_Newline
}
Ins_Char('-', COUNT, #2+2)

#8=1
Repeat(10) {
    BOF
    Update()
    // calculate one generation
    for (#11=1; #11<#1+2; #11++) {
        Goto_Line(#11)
        for (#12=1; #12<#2+2; #12++) {
            Goto_Col(#12)
            #14=Cur_Pos
            Call("RANDOM")
            #10 = Return_Value
            if (Cur_Char == '♠') {                      // tree?
                if (#10 < #3) {
                    Ins_Char('*', OVERWRITE)            // random combustion
                } else {
                    if (Search_Block("░", CP-#5-1, CP+#5+2, COLUMN+BEGIN+NOERR)) {
                        Goto_Pos(#14)
                        Ins_Char('*', OVERWRITE)        // combustion
                    }
                }
            } else {
                if (Cur_Char == ' ') {                  // empty space?
                    if (#10 < #4) {
                        Ins_Char('+', OVERWRITE)        // new tree
                    }
                }
            }
        }
    }
    // convert tmp symbols
    Replace("░"," ", BEGIN+ALL+NOERR)           // old fire goes out
    Replace("*","░", BEGIN+ALL+NOERR)           // new fire
    Replace("+","♠", BEGIN+ALL+NOERR)           // new tree
}
Return

//--------------------------------------------------------------
// Generate random numbers in range 0 <= Return_Value < #91
//  #90 = Seed    (0 to 0x7fffffff)
//  #91 = Scaling (0 to 0xffff)

:RANDOM:
#92 = 0x7fffffff / 48271
#93 = 0x7fffffff % 48271
#90 = (48271 * (#90 % #92) - #93 * (#90 / #92)) & 0x7fffffff
return ((#90 & 0xffff) * #91 / 0x10000)
```


Sample output, 10th generation:
```txt
--------------------------------------------------------------
|       ♠♠♠♠ ♠♠ ♠ ♠♠♠♠        ♠    ♠♠♠♠♠♠ ♠♠♠♠♠ ♠♠♠♠ ♠♠♠♠ ♠♠♠|
|         ░♠♠♠ ♠   ♠♠          ♠  ░♠♠♠♠ ♠   ♠   ♠ ♠♠♠  ♠ ♠♠░♠|
|♠  ♠     ░ ♠    ♠♠ ♠ ♠♠           ♠   ♠♠♠ ♠ ♠ ♠♠♠ ♠♠♠♠♠♠♠♠ ♠|
|    ♠    ░♠♠♠  ♠  ♠  ♠░       ♠ ░░♠♠♠♠♠  ♠♠♠♠ ♠ ♠♠♠♠♠  ♠♠♠♠♠|
|    ♠    ░♠♠♠ ♠♠  ♠  ♠ ░       ░♠ ♠ ♠░░░░░░░░ ♠  ♠♠♠♠♠♠♠♠♠♠ |
|          ♠♠ ♠ ♠♠♠ ♠♠♠        ░ ♠ ♠♠ ░        ♠♠  ♠ ♠♠♠♠♠♠♠♠|
|♠        ░♠♠♠♠♠♠♠♠ ♠♠ ♠♠♠░ ░░░ ░░░░ ♠            ░ ░ ░░ ░  ♠|
|    ♠    ░       ♠  ♠ ♠   ♠♠♠♠░             ░               |
|         ░♠ ♠♠♠♠♠♠♠♠♠ ♠ ♠♠♠  ♠   ♠      ♠                   |
|  ♠    ♠ ░♠ ♠♠ ♠♠♠♠♠♠♠♠♠♠  ░░ ░                             |
|    ♠    ░♠♠♠      ♠♠ ░░░    ♠░ ░          ♠♠♠   ░░░░   ♠   |
|        ░░♠♠♠    ♠  ♠░        ♠   ♠   ♠ ♠ ♠      ♠ ░        |
|     ♠ ♠♠♠♠♠♠  ♠♠ ♠♠♠░         ♠                  ░ ♠       |
|░ ░   ♠ ♠♠ ♠ ♠ ♠  ♠♠♠               ♠ ♠♠                    |
|  ♠ ♠♠  ░░░░♠  ♠♠ ♠♠♠░♠       ♠ ♠      ♠             ░     ░|
|♠♠♠    ♠   ░ ♠♠♠♠  ♠         ♠        ♠♠            ░♠░    ♠|
|♠  ♠♠♠♠░     ♠♠  ♠♠♠ ░        ♠         ♠  ♠         ░ ░    |
|♠ ♠♠♠♠♠░    ♠♠ ♠ ♠ ♠♠      ♠        ♠  ♠             ░♠♠♠  ♠|
|♠♠♠♠♠ ♠ ░░░░♠♠♠♠♠  ♠♠░    ♠                    ♠      ♠♠ ♠ ♠|
|      ♠♠♠♠♠♠♠  ♠ ♠♠ ♠░  ♠   ♠ ♠                      ░♠♠♠ ♠ |
| ♠ ♠♠♠♠♠ ♠♠ ♠    ♠ ♠♠              ♠    ♠            ░♠♠  ♠ |
|♠♠♠ ♠♠♠♠♠♠  ♠  ♠♠♠  ♠░                  ░            ░♠♠ ♠♠♠|
| ♠♠♠♠♠ ♠♠♠♠♠  ♠♠♠ ♠♠ ♠░ ♠          ♠  ░♠   ░  ♠      ░♠ ♠ ♠♠|
| ♠ ♠♠ ♠ ♠♠  ♠♠♠♠ ♠♠♠♠♠░          ░♠♠ ♠♠♠♠♠♠░     ♠      ♠♠  |
|  ♠ ♠ ♠♠♠♠♠♠♠    ♠♠♠ ♠░░ ░░ ♠░ ░░░♠♠♠   ♠  ♠♠ ♠♠ ♠    ♠♠ ♠  |
--------------------------------------------------------------
```

