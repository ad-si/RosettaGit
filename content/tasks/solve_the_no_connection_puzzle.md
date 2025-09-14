+++
title = "Solve the no connection puzzle"
description = ""
date = 2019-10-06T23:36:04Z
aliases = []
[extra]
id = 18017
[taxonomies]
categories = ["task", "Puzzles"]
tags = []
languages = [
  "ada",
  "apl",
  "arm_assembly",
  "autohotkey",
  "chapel",
  "d",
  "elixir",
  "factor",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "m2000_interpreter",
  "mathematica",
  "perl",
  "perl_6",
  "phix",
  "picat",
  "prolog",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ruby",
  "scala",
  "tcl",
  "xpl0",
  "zkl",
]
+++

You are given a box with eight holes labelled   A-<small>to</small>-H,   connected by fifteen straight lines in the pattern as shown below:

              '''A'''   '''B'''
             /│\ /│\
            / │ X │ \
           /  │/ \│  \
          '''C''' ─ '''D''' ─ '''E''' ─ '''F'''
           \  │\ /│  /
            \ │ X │ /
             \│/ \│/
              '''G'''   '''H'''

You are also given eight pegs numbered   1-<small>to</small>-8.


;Objective:
Place the eight pegs in the holes so that the (absolute) difference between any two numbers connected by any line is <u>greater</u> than one.


;Example:
In this attempt:

              '''4'''   '''7'''
             /│\ /│\
            / │ X │ \
           /  │/ \│  \
          '''8''' ─ '''1''' ─ '''6''' ─ '''2'''
           \  │\ /│  /
            \ │ X │ /
             \│/ \│/
              '''3'''   '''5'''

Note that   '''7'''   and   '''6'''   are connected and have a difference of   '''1''',   so it is   ''not''   a solution.


## Task

Produce and show here   ''one''   solution to the puzzle.


## Related tasks

:*   [[A* search algorithm]]
:*   [[Solve a Holy Knight's tour]]
:*   [[Knight's tour]]
:*   [[N-queens problem]]
:*   [[Solve a Hidato puzzle]]
:*   [[Solve a Holy Knight's tour]]
:*   [[Solve a Hopido puzzle]]
:*   [[Solve a Numbrix puzzle]]
:*   [[4-rings or 4-squares puzzle]]



## See also

[https://www.youtube.com/watch?v=AECElyEyZBQ No Connection Puzzle] (youtube).




## Ada

This solution is a bit longer than it actually needs to be; however, it uses tasks to find the solution and the used types and solution-generating functions are well-separated, making it more amenable to other solutions or altering it to display all solutions.

```Ada

With
Ada.Text_IO,
Connection_Types,
Connection_Combinations;

procedure main is
   Result : Connection_Types.Partial_Board renames Connection_Combinations;
begin
   Ada.Text_IO.Put_Line( Connection_Types.Image(Result) );
end;
```


```Ada
Pragma Ada_2012;

Package Connection_Types with Pure is

   -- Name of the nodes.
   Type Node is (A, B, C, D, E, F, G, H);

   -- Type for indicating if a node is connected.
   Type Connection_List is array(Node) of Boolean
     with Size => 8, Object_Size => 8, Pack;

   Function "&"( Left : Connection_List; Right : Node ) return Connection_List;
   
   -- The actual map of the network connections.
   Network : Constant Array (Node) of Connection_List:=
     (
      A => (C|D|E	=> True, others => False),
      B => (D|E|F	=> True, others => False),
      C => (A|D|G	=> True, others => False),
      D => (C|A|B|E|H|G	=> True, others => False),
      E => (D|A|B|F|H|G => True, others => False),
      F => (B|E|H	=> True, others => False),
      G => (C|D|E	=> True, others => False),
      H => (D|E|F	=> True, others => False)
     );

   -- Values of the nodes.
   Type Peg is range 1..8;

   -- Indicator for which values have been assigned.
   Type Used_Peg is array(Peg) of Boolean
     with Size => 8, Object_Size => 8, Pack;

   Function "&"( Left : Used_Peg; Right : Peg ) return Used_Peg;


   -- Type describing the layout of the network.
   Type Partial_Board is array(Node range <>) of Peg;
   Subtype Board is Partial_Board(Node);

   -- Determines if the given board is a solution or partial-solution.
   Function Is_Solution	( Input : Partial_Board ) return Boolean;

   -- Displays the board as text.
   Function Image	( Input : Partial_Board ) Return String;

End Connection_Types;
```


```Ada

Pragma Ada_2012;

with Connection_Types;
use  Connection_Types;

Function Connection_Combinations return Partial_Board;

```


```Ada
Pragma Ada_2012;

Package Body Connection_Types is

   New_Line : Constant String := ASCII.CR & ASCII.LF;

   ---------------------
   --  Solution Test  --
   ---------------------
   
   Function Is_Solution( Input : Partial_Board ) return Boolean is
     (for all Index in Input'Range =>
        (for all Connection in Node'Range =>
             (if Network(Index)(Connection) and Connection in Input'Range
              then abs (Input(Index) - Input(Connection)) > 1
             )
        )
     );
   
   ------------------------
   --  Concat Operators  --
   ------------------------
   
   Function "&"( Left : Used_Peg; Right : Peg ) return Used_Peg is
   begin
      return Result : Used_Peg := Left do
         Result(Right):= True;
      end return;
   end "&";

   Function "&"(Left : Connection_List; Right : Node) return Connection_List is
   begin
      Return Result : Connection_List := Left do
         Result(Right):= True;
      end return;        
   end "&";   

   -----------------------
   --  IMAGE FUNCTIONS  --
   -----------------------

   Function Image(Input : Peg) Return Character is
     ( Peg'Image(Input)(2) );

   Function Image(Input : Peg) Return String is
     ( 1 => Image(Input) );

   Function Image(Input : Partial_Board; Item : Node) Return String is
     ( 1 => (if Item not in Input'Range then '*' else Image(Input(Item)) ));

   Function Image( Input : Partial_Board ) Return String is
      A : String renames Image(Input, Connection_Types.A);
      B : String renames Image(Input, Connection_Types.B);
      C : String renames Image(Input, Connection_Types.C);
      D : String renames Image(Input, Connection_Types.D);
      E : String renames Image(Input, Connection_Types.E);
      F : String renames Image(Input, Connection_Types.F);
      G : String renames Image(Input, Connection_Types.G);
      H : String renames Image(Input, Connection_Types.H);
   begin
      return
	"        "&A&"   "&B			& New_Line &
	"       /|\ /|\"			& New_Line &
	"      / | X | \"			& New_Line &
	"     /  |/ \|  \"			& New_Line &
	"    "&C&" - "&D&" - "&E&" - "&F	& New_Line &
	"     \  |\ /|  /"			& New_Line &
	"      \ | X | /"			& New_Line &
	"       \|/ \|/"			& New_Line &
	"        "&G&"   "&H			& New_Line;
   end Image;

End Connection_Types;
```


```Ada
Function Connection_Combinations return Partial_Board is

begin
   Return Result : Board do
      declare
         
         -- The Generate task takes two parameters
         --   (1) a list of pegs already in use, and
         --   (2) a partial-board
         -- and, if the state given is a viable yet incomplete solution, it
         -- takes a peg and adds it to the state creating a new task with
         -- that peg in its used list.
         --
         -- When a complete solution is found it is copied into result.
         task type Generate(
                            Pegs	: not null access Used_Peg:= new Used_Peg'(others => False);
                            State	: not null access Partial_Board:= new Partial_Board'(Node'Last..Node'First => <>)
                           ) is
         end Generate;

         -- An access to Generate and array thereof, for creating the
         -- children tasks.
         type Generator  is access all Generate;
         type Generators is array(Peg range <>) of Generator;
         
         -- Gen handles the actual creation of a new task and state.
         Function Gen(P : Peg; G : not null access Generate) return Generator is
         begin
            return (if G.Pegs(P) then null
                    else new Generate(
                      Pegs     => new Used_Peg'(G.Pegs.all & P),
                      State    => New Partial_Board'(G.All.State.All & P)
                     )
                   );
         end;

         task body Generate is
         begin
            if Is_Solution(State.All) then
               -- If the state is a partial board, we make children to
               -- complete the calculations.
               if State'Length <= Node'Pos(Node'Last) then
                  declare
                     Subtasks : Constant Generators:=
                       (
                        Gen(1, Generate'Access),
                        Gen(2, Generate'Access),
                        Gen(3, Generate'Access),
                        Gen(4, Generate'Access),
                        Gen(5, Generate'Access),
                        Gen(6, Generate'Access),
                        Gen(7, Generate'Access),
                        Gen(8, Generate'Access)
                       );
                  begin
                     null;
                  end;
               else
                  Result:= State.All;
               end if;
            else
               -- The current state is not a solution, so we do not continue it.
               Null;
            end if;
            
         end Generate;
         
         Master : Generate;
      begin
         null;
      end;
   End return;
End Connection_Combinations;

```

```txt
        4   5
       /|\ /|\
      / | X | \
     /  |/ \|  \
    7 - 1 - 8 - 2
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        3   6

```



## APL


```APL



     perms←{
     ⍝∇ 20100513/20140818 ra⌈ --()--
        1=⍴⍴⍵:⍵[∇ ''⍴⍴⍵]
       ↑{0∊⍴⍵:⍵ ⋄ (⍺[1]⌷⍵),(1↓⍺)∇ ⍵~⍺[1]⌷⍵}∘(⍳⍵)¨↓⍉1+(⌽⍳⍵)⊤¯1+⍳!⍵
   }

solution←{
    links←  (3 4 5) (4 5 6) (1 4 7) (1 2 3 5 7 8) (1 2 4 6 7 8) (2 5 8) (3 4 5) (4 5 6) ⍝ node i connects with nodes i⊃links
    tries←8 perms 8
    fails←{1∊{1∊⍵∊¯1 0 1}¨|⍺-¨⍺∘{⍺[⍵]}¨⍵}
  ⍝    ⍴⍸~tries fails ⍤1⊢links
  ⍝ 16
   solns←⍸~tries fails ⍤1⊢links
   tries[''⍴solns;]
   }
 
```


## ARM Assembly

```ARM Assembly


/* ARM assembly Raspberry PI  */
/*  program noconnpuzzle.s   */
 
/************************************/
/* Constantes                       */
/************************************/
.equ STDOUT, 1     @ Linux output console
.equ EXIT,   1     @ Linux syscall
.equ WRITE,  4     @ Linux syscall

.equ NBBOX,  8
.equ POSA,   5

/*********************************/
/* Initialized data              */
/*********************************/
.data
sMessDeb:           .ascii "a="
sMessValeur_a:     .fill 11, 1, ' '            @ size => 11
                    .ascii "b="
sMessValeur_b:     .fill 11, 1, ' '            @ size => 11
                    .ascii "c="
sMessValeur_c:     .fill 11, 1, ' '            @ size => 11
                    .ascii "d="
sMessValeur_d:     .fill 11, 1, ' '            @ size => 11
                    .ascii "\n"
                    .ascii "e="
sMessValeur_e:     .fill 11, 1, ' '            @ size => 11
                    .ascii "f="
sMessValeur_f:     .fill 11, 1, ' '            @ size => 11
                    .ascii "g="
sMessValeur_g:     .fill 11, 1, ' '            @ size => 11
                    .ascii "h="
sMessValeur_h:     .fill 11, 1, ' '            @ size => 11

szCarriageReturn:   .asciz "\n************************\n"

szMessLine1:            .asciz "               \n"
szMessLine2:            .asciz "    /|\\ /|\\  \n"
szMessLine3:            .asciz "   / | X | \\     \n"
szMessLine4:            .asciz "  /  |/ \\|  \\     \n"
szMessLine5:            .asciz "   -   - | -     \n"
szMessLine6:            .asciz "  \\  |\\ /|  /  \n"
szMessLine7:            .asciz "   \\ | X | /  \n"
szMessLine8:            .asciz "    \\|/ \\|/   \n"
/*********************************/
/* UnInitialized data            */
/*********************************/
.bss  
.align 4
iValues_a:                .skip 4 * NBBOX
iValues_b:                .skip 4 * NBBOX - 1
iValues_c:                .skip 4 * NBBOX - 2
iValues_d:                .skip 4 * NBBOX - 3
iValues_e:                .skip 4 * NBBOX - 4
iValues_f:                .skip 4 * NBBOX - 5
iValues_g:                .skip 4 * NBBOX - 6
iValues_h:                .skip 4 * NBBOX - 7
sConvValue:               .skip 12
/*********************************/
/*  code section                 */
/*********************************/
.text
.global main 
main:                                             @ entry of program 
    mov r0,#1
    mov r1,#8
    bl searchPb

100:                                              @ standard end of the program 
    mov r0, #0                                    @ return code
    mov r7, #EXIT                                 @ request to exit program
    svc #0                                        @ perform the system call
 
iAdrszCarriageReturn:            .int szCarriageReturn

/******************************************************************/
/*     search problem  unique solution                            */ 
/******************************************************************/
/* r0 contains start digit */
/* r1 contains end digit */
searchPb:
    push {r0-r12,lr}                                  @ save  registers
    @ init
    ldr r3,iAdriValues_a                              @ area value a
    mov r4,#0
1:                                                    @ loop init value a
    str r0,[r3,r4,lsl #2]
    add r4,#1
    add r0,#1
    cmp r0,r1
    ble 1b

    mov r12,#-1
2:
    add r12,#1                                        @ increment indice a
    cmp r12,#NBBOX-1
    bgt 90f
    ldr r0,iAdriValues_a                              @ area value a
    ldr r1,iAdriValues_b                              @ area value b
    mov r2,r12                                        @ indice  a
    mov r3,#NBBOX                                     @ number of origin values 
    bl prepValues
    mov r11,#-1
3:
    add r11,#1                                        @ increment indice b
    cmp r11,#NBBOX - 2
    bgt 2b
    ldr r0,iAdriValues_b                              @ area value b
    ldr r1,iAdriValues_c                              @ area value c
    mov r2,r11                                        @ indice b
    mov r3,#NBBOX -1                                  @ number of origin values
    bl prepValues
    mov r10,#-1
4:
    add r10,#1
    cmp r10,#NBBOX - 3
    bgt 3b
    ldr r0,iAdriValues_a
    ldr r0,[r0,r12,lsl #2]
    ldr r1,iAdriValues_c
    ldr r1,[r1,r10,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 4b
    ldr r0,iAdriValues_c
    ldr r1,iAdriValues_d
    mov r2,r10
    mov r3,#NBBOX - 2
    bl prepValues
    mov r9,#-1
5:
    add r9,#1
    cmp r9,#NBBOX - 4
    bgt 4b
    @ control d   / a b c
    ldr r0,iAdriValues_d
    ldr r0,[r0,r9,lsl #2]
    ldr r1,iAdriValues_a
    ldr r1,[r1,r12,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 5b
    ldr r1,iAdriValues_b
    ldr r1,[r1,r11,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 5b
    ldr r1,iAdriValues_c
    ldr r1,[r1,r10,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 5b

    ldr r0,iAdriValues_d
    ldr r1,iAdriValues_e
    mov r2,r9
    mov r3,#NBBOX - 3
    bl prepValues
    mov r8,#-1
6:
    add r8,#1
    cmp r8,#NBBOX - 5
    bgt 5b
    @ control e   / a b d
    ldr r0,iAdriValues_e
    ldr r0,[r0,r8,lsl #2]
    ldr r1,iAdriValues_a
    ldr r1,[r1,r12,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 6b
    ldr r1,iAdriValues_b
    ldr r1,[r1,r11,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 6b
    ldr r1,iAdriValues_d
    ldr r1,[r1,r9,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 6b

    ldr r0,iAdriValues_e
    ldr r1,iAdriValues_f
    mov r2,r8
    mov r3,#NBBOX - 4
    bl prepValues
    mov r7,#-1
7:
    add r7,#1
    cmp r7,#NBBOX - 6
    bgt 6b
    @ control f   / b e
    ldr r0,iAdriValues_f
    ldr r0,[r0,r7,lsl #2]
    ldr r1,iAdriValues_b
    ldr r1,[r1,r11,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 7b
    ldr r1,iAdriValues_e
    ldr r1,[r1,r8,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 7b

    ldr r0,iAdriValues_f
    ldr r1,iAdriValues_g
    mov r2,r7
    mov r3,#NBBOX - 5
    bl prepValues
    mov r6,#-1
8:
    add r6,#1
    cmp r6,#NBBOX - 7
    bgt 7b
    @ control g   / c d e
    ldr r0,iAdriValues_g
    ldr r0,[r0,r6,lsl #2]
    ldr r1,iAdriValues_c
    ldr r1,[r1,r10,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 8b
    ldr r1,iAdriValues_d
    ldr r1,[r1,r9,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 8b
    ldr r1,iAdriValues_e
    ldr r1,[r1,r8,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 8b
    ldr r0,iAdriValues_g
    ldr r1,iAdriValues_h
    mov r2,r6
    mov r3,#NBBOX - 6
    bl prepValues
    mov r5,#-1
9:
    add r5,#1
    cmp r5,#NBBOX - 8
    bgt 8b
    @ control h   / d e f
    ldr r0,iAdriValues_h
    ldr r0,[r0,r5,lsl #2]
    ldr r1,iAdriValues_d
    ldr r1,[r1,r9,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 9b
    ldr r1,iAdriValues_e
    ldr r1,[r1,r8,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 9b
    ldr r1,iAdriValues_f
    ldr r1,[r1,r7,lsl #2]
    subs r2,r1,r0
    mvnlt r2,r2
    addlt r2,#1
    cmp r2,#1
    beq 9b
    @ solution ok   display text
    ldr r0,iAdriValues_a
    ldr r0,[r0,r12,lsl #2]
    ldr r1,iAdrsMessValeur_a
    bl conversion10
    ldr r0,iAdriValues_b
    ldr r0,[r0,r11,lsl #2]
    ldr r1,iAdrsMessValeur_b
    bl conversion10
    ldr r0,iAdriValues_c
    ldr r0,[r0,r10,lsl #2]
    ldr r1,iAdrsMessValeur_c
    bl conversion10
    ldr r0,iAdriValues_d
    ldr r0,[r0,r9,lsl #2]
    ldr r1,iAdrsMessValeur_d
    bl conversion10
    ldr r0,iAdriValues_e
    ldr r0,[r0,r8,lsl #2]
    ldr r1,iAdrsMessValeur_e
    bl conversion10
    ldr r0,iAdriValues_f
    ldr r0,[r0,r7,lsl #2]
    ldr r1,iAdrsMessValeur_f
    bl conversion10
    ldr r0,iAdriValues_g
    ldr r0,[r0,r6,lsl #2]
    ldr r1,iAdrsMessValeur_g
    bl conversion10
    ldr r0,iAdriValues_h
    ldr r0,[r0,r5,lsl #2]
    ldr r1,iAdrsMessValeur_h
    bl conversion10
    ldr r0,iAdrsMessDeb
    bl affichageMess

    @ display design
    ldr r0,iAdriValues_a
    ldr r0,[r0,r12,lsl #2]
    ldr r1,iAdrsConvValue
    bl conversion10
    ldrb r2,[r1]
    ldr r0,iAdrszMessLine1
    strb r2,[r0,#POSA]
    ldr r0,iAdriValues_b
    ldr r0,[r0,r11,lsl #2]
    ldr r1,iAdrsConvValue
    bl conversion10
    ldrb r2,[r1]
    ldr r0,iAdrszMessLine1
    strb r2,[r0,#POSA+4]
    bl affichageMess
    ldr r0,iAdrszMessLine2
    bl affichageMess
    ldr r0,iAdrszMessLine3
    bl affichageMess
    ldr r0,iAdrszMessLine4
    bl affichageMess
    ldr r0,iAdriValues_c
    ldr r0,[r0,r10,lsl #2]
    ldr r1,iAdrsConvValue
    bl conversion10
    ldrb r2,[r1]
    ldr r0,iAdrszMessLine5
    strb r2,[r0,#POSA-4]
    ldr r0,iAdriValues_d
    ldr r0,[r0,r9,lsl #2]
    ldr r1,iAdrsConvValue
    bl conversion10
    ldrb r2,[r1]
    ldr r0,iAdrszMessLine5
    strb r2,[r0,#POSA]
    ldr r0,iAdriValues_e
    ldr r0,[r0,r8,lsl #2]
    ldr r1,iAdrsConvValue
    bl conversion10
    ldrb r2,[r1]
    ldr r0,iAdrszMessLine5
    strb r2,[r0,#POSA+4]
    ldr r0,iAdriValues_f
    ldr r0,[r0,r7,lsl #2]
    ldr r1,iAdrsConvValue
    bl conversion10
    ldrb r2,[r1]
    ldr r0,iAdrszMessLine5
    strb r2,[r0,#POSA+8]
    bl affichageMess
    ldr r0,iAdrszMessLine6
    bl affichageMess
    ldr r0,iAdrszMessLine7
    bl affichageMess
    ldr r0,iAdrszMessLine8
    bl affichageMess
    ldr r0,iAdriValues_g
    ldr r0,[r0,r6,lsl #2]
    ldr r1,iAdrsConvValue
    bl conversion10
    ldrb r2,[r1]
    ldr r0,iAdrszMessLine1
    strb r2,[r0,#POSA]
    ldr r0,iAdriValues_h
    ldr r0,[r0,r5,lsl #2]
    ldr r1,iAdrsConvValue
    bl conversion10
    ldrb r2,[r1]
    ldr r0,iAdrszMessLine1
    strb r2,[r0,#POSA+4]
    bl affichageMess

    //b 9b                   @ loop for other solution
90:

100:
    pop {r0-r12,lr}                               @ restaur registers 
    bx lr                                         @return
iAdriValues_a:                   .int iValues_a
iAdriValues_b:                   .int iValues_b
iAdriValues_c:                   .int iValues_c
iAdriValues_d:                   .int iValues_d
iAdriValues_e:                   .int iValues_e
iAdriValues_f:                   .int iValues_f
iAdriValues_g:                   .int iValues_g
iAdriValues_h:                   .int iValues_h

iAdrsMessValeur_a:               .int sMessValeur_a
iAdrsMessValeur_b:               .int sMessValeur_b
iAdrsMessValeur_c:               .int sMessValeur_c
iAdrsMessValeur_d:               .int sMessValeur_d
iAdrsMessValeur_e:               .int sMessValeur_e
iAdrsMessValeur_f:               .int sMessValeur_f
iAdrsMessValeur_g:               .int sMessValeur_g
iAdrsMessValeur_h:               .int sMessValeur_h
iAdrsMessDeb:                    .int sMessDeb

iAdrsConvValue:                  .int sConvValue
iAdrszMessLine1:                 .int szMessLine1
iAdrszMessLine2:                 .int szMessLine2
iAdrszMessLine3:                 .int szMessLine3
iAdrszMessLine4:                 .int szMessLine4
iAdrszMessLine5:                 .int szMessLine5
iAdrszMessLine6:                 .int szMessLine6
iAdrszMessLine7:                 .int szMessLine7
iAdrszMessLine8:                 .int szMessLine8
/******************************************************************/
/*     copy value area  and substract value of indice             */ 
/******************************************************************/
/* r0 contains the address of values origin */
/* r1 contains the address of values destination */
/* r2 contains value indice to substract     */
/* r3 contains origin values number          */
prepValues:
    push {r1-r6,lr}                                @ save  registres
    mov r4,#0                                      @ indice origin value
    mov r5,#0                                      @ indice destination value
1:
    cmp r4,r2                                      @ substract indice ?
    beq 2f                                         @ yes -> jump
    ldr r6,[r0,r4,lsl #2]                          @ no -> copy value
    str r6,[r1,r5,lsl #2]
    add r5,#1                                      @ increment destination indice
2:
   add r4,#1                                       @ increment origin indice
   cmp r4,r3                                       @ end ?
   blt 1b
100:
    pop {r1-r6,lr}                                 @ restaur registres 
    bx lr                                          @return
/******************************************************************/
/*     display text with size calculation                         */ 
/******************************************************************/
/* r0 contains the address of the message */
affichageMess:
    push {r0,r1,r2,r7,lr}                          @ save  registres
    mov r2,#0                                      @ counter length 
1:                                                 @ loop length calculation 
    ldrb r1,[r0,r2]                                @ read octet start position + index 
    cmp r1,#0                                      @ if 0 its over 
    addne r2,r2,#1                                 @ else add 1 in the length 
    bne 1b                                         @ and loop 
                                                   @ so here r2 contains the length of the message 
    mov r1,r0                                      @ address message in r1 
    mov r0,#STDOUT                                 @ code to write to the standard output Linux 
    mov r7, #WRITE                                 @ code call system "write" 
    svc #0                                         @ call systeme 
    pop {r0,r1,r2,r7,lr}                           @ restaur des  2 registres */ 
    bx lr                                          @ return  
/******************************************************************/
/*     Converting a register to a decimal unsigned                */ 
/******************************************************************/
/* r0 contains value and r1 address area   */
/* r0 return size of result (no zero final in area) */
/* area size => 11 bytes          */
.equ LGZONECAL,   10
conversion10:
    push {r1-r4,lr}                                 @ save registers 
    mov r3,r1
    mov r2,#LGZONECAL
1:                                                  @ start loop
    bl divisionpar10U                               @ unsigned  r0 <- dividende. quotient ->r0 reste -> r1
    add r1,#48                                      @ digit
    strb r1,[r3,r2]                                 @ store digit on area
    cmp r0,#0                                       @ stop if quotient = 0 
    subne r2,#1                                     @ else previous position
    bne 1b                                          @ and loop
                                                    @ and move digit from left of area
    mov r4,#0
2:
    ldrb r1,[r3,r2]
    strb r1,[r3,r4]
    add r2,#1
    add r4,#1
    cmp r2,#LGZONECAL
    ble 2b
                                                      @ and move spaces in end on area
    mov r0,r4                                         @ result length 
    mov r1,#' '                                       @ space
3:
    strb r1,[r3,r4]                                   @ store space in area
    add r4,#1                                         @ next position
    cmp r4,#LGZONECAL
    ble 3b                                            @ loop if r4 <= area size
 
100:
    pop {r1-r4,lr}                                    @ restaur registres 
    bx lr                                             @return
 
/***************************************************/
/*   division par 10   unsigned                    */
/***************************************************/
/* r0 dividende   */
/* r0 quotient    */
/* r1 remainder   */
divisionpar10U:
    push {r2,r3,r4, lr}
    mov r4,r0                                          @ save value
    ldr r3,iMagicNumber                                @ r3 <- magic_number    raspberry 1 2
    umull r1, r2, r3, r0                               @ r1<- Lower32Bits(r1*r0) r2<- Upper32Bits(r1*r0) 
    mov r0, r2, LSR #3                                 @ r2 <- r2 >> shift 3
    add r2,r0,r0, lsl #2                               @ r2 <- r0 * 5 
    sub r1,r4,r2, lsl #1                               @ r1 <- r4 - (r2 * 2)  = r4 - (r0 * 10)
    pop {r2,r3,r4,lr}
    bx lr                                              @ leave function 
iMagicNumber:  	.int 0xCCCCCCCD


```


```txt

a=3          b=4          c=7          d=1
e=8          f=2          g=5          h=6
************************
     3   4
    /|\ /|\
   / | X | \
  /  |/ \|  \
 7 - 1 - 8 - 2
  \  |\ /|  /
   \ | X | /
    \|/ \|/
     5   6


```




## AutoHotkey


```AutoHotkey
oGrid := [[ "", "X", "X"]							; setup oGrid
	,[ "X", "X", "X", "X"]
	,[  "", "X", "X"]] 

oNeighbor := [], oCell := [], 	oRoute := [] , oVisited := []			; initialize objects

for row, oRow in oGrid
	for col, val in oRow
		if val								; for each valid cell in oGrid
			oNeighbor[row, col] := Neighbors(row, col, oGrid)	; list valid no-connection neighbors

Solve:
for row, oRow in oGrid
	for col , val in oRow
		if val								; for each valid cell in oGrid
			if (oSolution := SolveNoConnect(row, col, 1)).8		; solve for this cell
				break, Solve					; if solution found stop

; show solution
for i , val in oSolution
	oCell[StrSplit(val, ":").1 , StrSplit(val, ":").2] := i

		  A := oCell[1, 2]	, B := oCell[1, 3]
C := oCell[2, 1], D := oCell[2, 2]	, E := oCell[2, 3], 	F := oCell[2, 4]
		  G := oCell[3, 2]	, H := oCell[3, 3]
sol =
(

    %A%   %B%
   /|\ /|\
  / | X | \
 /  |/ \|  \
%C% - %D% - %E% - %F%
 \  |\ /|  /
  \ | X | /
   \|/ \|/
    %G%   %H%
)
MsgBox % sol
return
;-----------------------------------------------------------------------
SolveNoConnect(row, col, val){
	global
	oRoute.push(row ":" col)						; save route
	oVisited[row, col] := true						; mark this cell visited
	
	if oRoute[8]								; if solution found
		return true							; end recursion

	for each, nn in StrSplit(oNeighbor[row, col], ",") 			; for each no-connection neighbor of cell
	{
		rowX := StrSplit(nn, ":").1	, colX := StrSplit(nn, ":").2	; get coords of this neighbor
		if !oVisited[rowX, colX]					; if not previously visited
		{
			oVisited[rowX, colX] := true				; mark this cell visited
			val++							; increment
			if (SolveNoConnect(rowX, colX, val))			; recurse 
				return oRoute					; if solution found return route
		}
	}
	oRoute.pop()								; Solution not found, backtrack oRoute
	oVisited[row, col] := false						; Solution not found, remove mark
}
;-----------------------------------------------------------------------
Neighbors(row, col, oGrid){							; return distant neighbors of oGrid[row,col]
	for r , oRow in oGrid
		for c, v in oRow
			if (v="X") && (abs(row-r) > 1 || abs(col-c) > 1)
				list .= r ":"c ","
	if (row<>2) && oGrid[row, col]
		list .= oGrid[row, col+1] ? row ":" col+1 "," : oGrid[row, col-1] ? row ":" col-1 "," : ""
	return Trim(list, ",")
}
```

Outputs:
```txt

    3   5
   /|\ /|\
  / | X | \
 /  |/ \|  \
7 - 1 - 8 - 2
 \  |\ /|  /
  \ | X | /
   \|/ \|/
    4   6
```



## Chapel


```chapel
type hole = int;
param A : hole = 1;
param B : hole = A+1;
param C : hole = B+1;
param D : hole = C+1;
param E : hole = D+1;
param F : hole = E+1;
param G : hole = F+1;
param H : hole = G+1;
param starting : int = 0;
const holes : domain(hole) = { A,B,C,D,E,F,G,H };
const graph : [holes] domain(hole) = [  A => { C,D,E },
                                        B => { D,E,F },
                                        C => { A,D,G },
                                        D => { A,B,C,E,G,H },
                                        E => { A,B,D,F,G,H },
                                        F => { B,E,H },
                                        G => { C,D,E },
                                        H => { D,E,F } 
                                      ];

proc check( configuration : [] int, idx : hole ) : bool {
  var good = true;
  for adj in graph[idx] {
    if adj >= idx then continue;
    if abs( configuration[idx] - configuration[adj] ) <= 1 {
      good = false;
      break;
    }
  }
  
  return good;
}

proc solve( configuration : [] int, pegs : domain(int), idx : hole = A ) : bool {
  for value in pegs {
    configuration[idx] = value;
    if check( configuration, idx ) {
      if idx < holes.size {
        var prePegs = pegs;
        if solve( configuration, prePegs - value, idx + 1 ){
          return true;  
        }
      } else {
        return true;
      }
    }
  }
  configuration[idx] = starting;
  return false;
}

proc printBoard( configuration : [] int ){
return 
"\n       " + configuration[A] + "   " + configuration[B]+ "\n" +
"      /|\\ /|\\ \n"+
"     / | X | \\ \n"+
"    /  |/ \\|  \\ \n"+
"   " + configuration[C] +" - " + configuration[D] + " - " + configuration[E] + " - " + configuration[F] + " \n"+
"    \\  |\\ /|  / \n"+
"     \\ | X | / \n"+
"      \\|/ \\|/ \n"+
"       " + configuration[G] + "   " + configuration[H]+ "\n";

}



proc main(){
  var configuration : [holes] int;
  for idx in holes do configuration[idx] = starting;
  
  var pegs : domain(int) = {1,2,3,4,5,6,7,8};
  solve( configuration, pegs );

  writeln( printBoard( configuration ) );
  
}

```


```txt

       4   5
      /|\ /|\ 
     / | X | \ 
    /  |/ \|  \ 
   7 - 1 - 8 - 2 
    \  |\ /|  / 
     \ | X | / 
      \|/ \|/ 
       3   6

```



## D


```d
void main() @safe {
    import std.stdio, std.math, std.algorithm, std.traits, std.string;

    enum Peg { A, B, C, D, E, F, G, H }
    immutable Peg[2][15] connections =
            [[Peg.A, Peg.C], [Peg.A, Peg.D], [Peg.A, Peg.E],
             [Peg.B, Peg.D], [Peg.B, Peg.E], [Peg.B, Peg.F],
             [Peg.C, Peg.D], [Peg.D, Peg.E], [Peg.E, Peg.F],
             [Peg.G, Peg.C], [Peg.G, Peg.D], [Peg.G, Peg.E],
             [Peg.H, Peg.D], [Peg.H, Peg.E], [Peg.H, Peg.F]];

    immutable board = r"
        A   B
       /|\ /|\
      / | X | \
     /  |/ \|  \
    C - D - E - F
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        G   H";

    Peg[EnumMembers!Peg.length] perm = [EnumMembers!Peg];
    do if (connections[].all!(con => abs(perm[con[0]] - perm[con[1]]) > 1))
        return board.tr("ABCDEFGH", "%(%d%)".format(perm)).writeln;
    while (perm[].nextPermutation);
}
```

```txt

        2   3
       /|\ /|\
      / | X | \
     /  |/ \|  \
    6 - 0 - 7 - 1
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        4   5

```



### Alternative version

Using a simple backtracking.
```d
import std.stdio, std.algorithm, std.conv, std.string, std.typecons;

// Holes A=0, B=1, ..., H=7
// With connections:
const board = r"
       A   B
      /|\ /|\
     / | X | \
    /  |/ \|  \
   C - D - E - F
    \  |\ /|  /
     \ | X | /
      \|/ \|/
       G   H";

struct Connection { uint a, b; }

immutable Connection[] connections = [
    {0, 2}, {0, 3}, {0, 4}, // A to C,D,E
    {1, 3}, {1, 4}, {1, 5}, // B to D,E,F
    {6, 2}, {6, 3}, {6, 4}, // G to C,D,E
    {7, 3}, {7, 4}, {7, 5}, // H to D,E,F
    {2, 3}, {3, 4}, {4, 5}, // C-D, D-E, E-F
];

alias Pegs = uint[8];

int absDiff(in uint a, in uint b) pure nothrow @safe @nogc {
    return (a > b) ? (a - b) : (b - a);
}

/** Solution is a simple recursive brute force solver,
it stops at the first found solution.
It returns the solution, the number of positions tested,
and the number of pegs swapped. */
Tuple!(Pegs,"p", uint,"tests", uint,"swaps") solve() pure nothrow @safe @nogc {
    uint tests = 0, swaps = 0;
    Pegs p = [1, 2, 3, 4, 5, 6, 7, 8];

    bool recurse(in uint i) nothrow @safe @nogc {
        if (i >= p.length.signed - 1) {
            tests++;
            return connections.all!(c => absDiff(p[c.a], p[c.b]) > 1);
        }

        // Try each remain peg from.
        foreach (immutable j;  i .. p.length) {
            swaps++;
            swap(p[i], p[j]);
            if (recurse(i + 1))
                return true;
            swap(p[i], p[j]);
        }
        return false;
    }

    recurse(0);
    return typeof(return)(p, tests, swaps);
}

void main() {
    immutable sol = solve();
    board.tr("ABCDEFGH", "%(%d%)".format(sol.p)).writeln;
    writeln("Tested ", sol.tests, " positions and did ", sol.swaps, " swaps.");
}
```

```txt

       3   4
      /|\ /|\
     / | X | \
    /  |/ \|  \
   7 - 1 - 8 - 2
    \  |\ /|  /
     \ | X | /
      \|/ \|/
       5   6
Tested 12094 positions and did 20782 swaps.
```



## Elixir

This solution uses HLPsolver from [[Solve_a_Hidato_puzzle#Elixir | here]]

```elixir
# It solved if connected A and B, connected G and H (according to the video).

# require HLPsolver

adjacent = for i <- -2..2, j <- -2..2, not(i in -1..1 and j in -1..1), do: {i,j}
layout = ~S"""
       A - B
      /|\ /|\ 
     / | X | \ 
    /  |/ \|  \ 
   C - D - E - F
    \  |\ /|  /
     \ | X | /
      \|/ \|/
       G - H
"""
board = """
  . 0 0 .
  0 1 0 0 
  . 0 0 .
"""
HLPsolver.solve(board, adjacent, false)
|> Enum.sort |> Enum.map(fn {_,cell} -> cell.value end)
|> Enum.zip(~w[A B C D E F G H])
|> Enum.reduce(layout, fn {n,c},acc -> String.replace(acc, c, to_string(n)) end)
|> IO.puts
```


```txt

       4 - 6
      /|\ /|\
     / | X | \
    /  |/ \|  \
   7 - 1 - 8 - 2
    \  |\ /|  /
     \ | X | /
      \|/ \|/
       3 - 5

```



## Factor


```factor
USING: assocs interpolate io kernel math math.combinatorics
math.ranges math.parser multiline pair-rocket sequences
sequences.generalizations ;

STRING: diagram
    ${}   ${}
   /|\ /|\
  / | X | \
 /  |/ \|  \
${} - ${} - ${} - ${}
 \  |\ /|  /
  \ | X | /
   \|/ \|/
    ${}   ${}
;

CONSTANT: adjacency
H{
    0 => { 2 3 4 }
    1 => { 3 4 5 }
    2 => { 0 3 6 }
    3 => { 0 1 2 4 6 7 }
    4 => { 0 1 3 5 6 7 }
    5 => { 1 4 7 }
    6 => { 2 3 4 }
    7 => { 3 4 5 }
}

: any-consecutive? ( seq n -- ? ) [ - abs 1 = ] curry any? ;

: neighbors ( elt seq i -- seq elt )
    adjacency at swap nths swap ;

: solution? ( permutation-seq -- ? )
    dup [ neighbors any-consecutive? ] with find-index nip not ;
    
: find-solution ( -- seq )
    8 [1,b] [ solution? ] find-permutation ;
    
: display-solution ( seq -- )
    [ number>string ] map 8 firstn diagram interpolate>string
    print ;
    
: main ( -- ) find-solution display-solution ;

MAIN: main
```

```txt

    3   4
   /|\ /|\
  / | X | \
 /  |/ \|  \
7 - 1 - 8 - 2
 \  |\ /|  /
  \ | X | /
   \|/ \|/
    5   6

```



## Go

A simple recursive brute force solution.

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	p, tests, swaps := Solution()
	fmt.Println(p)
	fmt.Println("Tested", tests, "positions and did", swaps, "swaps.")
}

// Holes A=0, B=1, …, H=7
// With connections:
const conn = `
       A   B
      /|\ /|\
     / | X | \
    /  |/ \|  \
   C - D - E - F
    \  |\ /|  /
     \ | X | /
      \|/ \|/
       G   H`

var connections = []struct{ a, b int }{
	{0, 2}, {0, 3}, {0, 4}, // A to C,D,E
	{1, 3}, {1, 4}, {1, 5}, // B to D,E,F
	{6, 2}, {6, 3}, {6, 4}, // G to C,D,E
	{7, 3}, {7, 4}, {7, 5}, // H to D,E,F
	{2, 3}, {3, 4}, {4, 5}, // C-D, D-E, E-F
}

type pegs [8]int

// Valid checks if the pegs are a valid solution.
// If the absolute difference between any pair of connected pegs is
// greater than one it is a valid solution.
func (p *pegs) Valid() bool {
	for _, c := range connections {
		if absdiff(p[c.a], p[c.b]) <= 1 {
			return false
		}
	}
	return true
}

// Solution is a simple recursive brute force solver,
// it stops at the first found solution.
// It returns the solution, the number of positions tested,
// and the number of pegs swapped.
func Solution() (p *pegs, tests, swaps int) {
	var recurse func(int) bool
	recurse = func(i int) bool {
		if i >= len(p)-1 {
			tests++
			return p.Valid()
		}
		// Try each remain peg from p[i:] in p[i]
		for j := i; j < len(p); j++ {
			swaps++
			p[i], p[j] = p[j], p[i]
			if recurse(i + 1) {
				return true
			}
			p[i], p[j] = p[j], p[i]
		}
		return false
	}
	p = &pegs{1, 2, 3, 4, 5, 6, 7, 8}
	recurse(0)
	return
}

func (p *pegs) String() string {
	return strings.Map(func(r rune) rune {
		if 'A' <= r && r <= 'H' {
			return rune(p[r-'A'] + '0')
		}
		return r
	}, conn)
}

func absdiff(a, b int) int {
	if a > b {
		return a - b
	}
	return b - a
}
```

```txt


       3   4
      /|\ /|\
     / | X | \
    /  |/ \|  \
   7 - 1 - 8 - 2
    \  |\ /|  /
     \ | X | /
      \|/ \|/
       5   6
Tested 12094 positions and did 20782 swaps.

```



## Haskell


```haskell
import Data.List (intercalate, permutations)

solution :: [Int]
solution@(a:b:c:d:e:f:g:h:_) = head $ filter isSolution (permutations [1 .. 8])
  where
    isSolution :: [Int] -> Bool
    isSolution (a:b:c:d:e:f:g:h:_) =
      all ((> 1) . abs) $
      zipWith
        (-)
        [a, c, g, e, a, c, g, e, b, d, h, f, b, d, h, f]
        [d, d, d, d, c, g, e, a, e, e, e, e, d, h, f, b]

main :: IO ()
main =
  (putStrLn . unlines) $
  let rightShift s
        | length s > 3 = s
        | otherwise = "  " ++ s
  in intercalate
       "\n"
       (zipWith (\x y -> x : (" = " ++ show y)) ['A' .. 'H'] solution) :
     ((rightShift . unwords . fmap show) <$> [[], [a, b], [c, d, e, f], [g, h]])
```

<pre style="font-size:80%">A = 3
B = 4
C = 7
D = 1
E = 8
F = 2
G = 5
H = 6

  3 4
7 1 8 2
  5 6 
```



## J


Supporting code:


```J
holes=:;:'A B C D E F G H'

connections=:".;._2]0 :0
 holes e.;:'C D E'          NB. A
 holes e.;:'D E F'          NB. B
 holes e.;:'A D G'          NB. C
 holes e.;:'A B C E G H'    NB. D
 holes e.;:'A B D F G H'    NB. E
 holes e.;:'B E H'          NB. F
 holes e.;:'C D E'          NB. G
 holes e.;:'D E F'          NB. H
)
assert (-:|:) connections NB. catch typos

pegs=: 1+(A.&i.~ !)8

attempt=: [: <./@(-.&0)@,@:| connections * -/~


box=:0 :0
        A   B
       /|\ /|\
      / | X | \
     /  |/ \|  \
    C - D - E - F
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        G   H
)

disp=:verb define
  rplc&(,holes;&":&>y) box
)
```


Intermezzo:


```J
   (#~ 1<attempt"1) pegs
3 4 7 1 8 2 5 6
3 5 7 1 8 2 4 6
3 6 7 1 8 2 4 5
3 6 7 1 8 2 5 4
4 3 2 8 1 7 6 5
4 5 2 8 1 7 6 3
4 5 7 1 8 2 3 6
4 6 7 1 8 2 3 5
5 3 2 8 1 7 6 4
5 4 2 8 1 7 6 3
5 4 7 1 8 2 3 6
5 6 7 1 8 2 3 4
6 3 2 8 1 7 4 5
6 3 2 8 1 7 5 4
6 4 2 8 1 7 5 3
6 5 2 8 1 7 4 3
```


Since there's more than one arrangement where the pegs satisfy the task constraints, and since the task calls for one solution, we will need to pick one of them. We can use the "first" function to satisfy this important constraint.


```J
   disp {. (#~ 1<attempt"1) pegs
        3   4
       /|\ /|\
      / | X | \
     /  |/ \|  \
    7 - 1 - 8 - 2
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        5   6


```


'''Video'''

If we follow the video and also connect A and B as well as G and H, we get only four solutions (which we can see are reflections / rotations of each other):


```J
   (#~ 1<attempt"1) pegs
3 5 7 1 8 2 4 6
4 6 7 1 8 2 3 5
5 3 2 8 1 7 6 4
6 4 2 8 1 7 5 3
```


The first of these looks like this:


```J
   disp {. (#~ 1<attempt"1) pegs
        3 - 5
       /|\ /|\
      / | X | \
     /  |/ \|  \
    7 - 1 - 8 - 2
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        4 - 6

```


For this puzzle, we can also see that the solution can be described as: put the starting and ending numbers in the middle - everything else follows from there. It's perhaps interesting that we get this solution even if we do not explicitly put that logic into our code - it's built into the puzzle itself and is still the only solution no matter how we arrive there.


## Java

The backtracking is getting tiresome, we'll try a stochastic solution for a change.

```java
import static java.lang.Math.abs;
import java.util.*;
import static java.util.stream.Collectors.toList;
import static java.util.stream.IntStream.range;

public class NoConnection {

    // adopted from Go
    static int[][] links = {
        {2, 3, 4}, // A to C,D,E
        {3, 4, 5}, // B to D,E,F
        {2, 4},    // D to C, E
        {5},       // E to F
        {2, 3, 4}, // G to C,D,E
        {3, 4, 5}, // H to D,E,F
    };

    static int[] pegs = new int[8];

    public static void main(String[] args) {

        List<Integer> vals = range(1, 9).mapToObj(i -> i).collect(toList());
        do {
            Collections.shuffle(vals);
            for (int i = 0; i < pegs.length; i++)
                pegs[i] = vals.get(i);

        } while (!solved());

        printResult();
    }

    static boolean solved() {
        for (int i = 0; i < links.length; i++)
            for (int peg : links[i])
                if (abs(pegs[i] - peg) == 1)
                    return false;
        return true;
    }

    static void printResult() {
        System.out.printf("  %s %s%n", pegs[0], pegs[1]);
        System.out.printf("%s %s %s %s%n", pegs[2], pegs[3], pegs[4], pegs[5]);
        System.out.printf("  %s %s%n", pegs[6], pegs[7]);
    }
}
```

(takes about 500 shuffles on average)

```txt
       4  5       
    2  8  1  7    
       6  3     
```



## JavaScript


### ES6

```JavaScript
(() => {
    'use strict';

    // GENERIC FUNCTIONS ------------------------------------------------------

    // abs :: Num a => a -> a
    const abs = Math.abs;

    // all :: (a -> Bool) -> [a] -> Bool
    const all = (f, xs) => xs.every(f);

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) => [].concat.apply([], xs.map(f));

    // delete_ :: Eq a => a -> [a] -> [a]
    const delete_ = (x, xs) =>
        deleteBy((a, b) => a === b, x, xs);

    // deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
    const deleteBy = (f, x, xs) =>
        xs.length > 0 ? (
            f(x, xs[0]) ? (
                xs.slice(1)
            ) : [xs[0]].concat(deleteBy(f, x, xs.slice(1)))
        ) : [];

    // enumFromTo :: Enum a => a -> a -> [a]
    const enumFromTo = (m, n) => {
        const [tm, tn] = [typeof m, typeof n];
        return tm !== tn ? undefined : (() => {
            const
                blnS = (tm === 'string'),
                [base, end] = [m, n].map(blnS ? (s => s.codePointAt(0)) : id);
            return Array.from({
                length: Math.floor(end - base) + 1
            }, (_, i) => blnS ? String.fromCodePoint(base + i) : m + i);
        })();
    };

    // id :: a -> a
    const id = x => x;

    // justifyRight :: Int -> Char -> Text -> Text
    const justifyRight = (n, cFiller, strText) =>
        n > strText.length ? (
            (cFiller.repeat(n) + strText)
            .slice(-n)
        ) : strText;

    // permutations :: [a] -> [[a]]
    const permutations = xs =>
        xs.length ? concatMap(x => concatMap(ys => [
                [x].concat(ys)
            ],
            permutations(delete_(x, xs))), xs) : [
            []
        ];

    // show :: a -> String
    const show = x => JSON.stringify(x);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // until :: (a -> Bool) -> (a -> a) -> a -> a
    const until = (p, f, x) => {
        let v = x;
        while (!p(v)) v = f(v);
        return v;
    };

    // unwords :: [String] -> String
    const unwords = xs => xs.join(' ');

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const ny = ys.length;
        return (xs.length <= ny ? xs : xs.slice(0, ny))
            .map((x, i) => f(x, ys[i]));
    };


    // CONNECTION PUZZLE ------------------------------------------------------

    // universe :: [[Int]]
    const universe = permutations(enumFromTo(1, 8));

    // isSolution :: [Int] -> Bool
    const isSolution = ([a, b, c, d, e, f, g, h]) =>
        all(x => abs(x) > 1, [a - d, c - d, g - d, e - d, a - c, c - g, g - e,
            e - a, b - e, d - e, h - e, f - e, b - d, d - h, h - f, f - b
        ]);

    // firstSolution :: [Int]
    const firstSolution = universe[until(
        i => isSolution(universe[i]),
        i => i + 1,
        0
    )];

    // TEST -------------------------------------------------------------------

    // [Int]
    const [a, b, c, d, e, f, g, h] = firstSolution;

    return unlines(
        zipWith(
            (a, n) => a + ' = ' + n.toString(),
            enumFromTo('A', 'H'),
            firstSolution
        )
        .concat(
            [
                [],
                [a, b],
                [c, d, e, f],
                [g, h]
            ].map(xs => justifyRight(5, ' ', unwords(xs.map(show))))
        )
    );
})();
```

```txt
A = 3
B = 4
C = 7
D = 1
E = 8
F = 2
G = 5
H = 6
     
  3 4
7 1 8 2
  5 6
```



## jq

We present a generate-and-test solver for a slightly more general version of the problem, in which there are N pegs and holes, and in which the connectedness of holes is defined by an array such that holes i and j are connected if and only if [i,j] is a member of the
array.

The jq index origin is 0, and so in the following, the pegs and
holes are internally numbered from 0 to (N-1) inclusive. That is, we interpret a permutation, p, of 0 .. (N-1) as meaning that the i-th peg is numbered p[i], for i in 0 .. (N-1).

However the pretty-print function shows solutions using the 1-to-8 numbering scheme for pegs, and the A-to-H lettering scheme for holes.

'''Part 1: Generic functions'''

```jq
# Short-circuit determination of whether (a|condition)
# is true for all a in array:
def forall(array; condition):
  def check:
    . as $ix
    | if $ix == (array|length) then true
      elif (array[$ix] | condition) then ($ix + 1) | check
      else false
      end;
  0 | check;

# permutations of 0 .. (n-1)
def permutations(n):
  # Given a single array, generate a stream by inserting n at different positions:
  def insert(m;n):
     if m >= 0 then (.[0:m] + [n] + .[m:]), insert(m-1;n) else empty end;
  if n==0 then []
  elif n == 1 then [1]
  else
    permutations(n-1) | insert(n-1; n)
  end;

# Count the number of items in a stream
def count(f): reduce f as $_ (0; .+1);
```


'''Part 2: The no-connections puzzle for N pegs and holes'''

```jq
# Generate a stream of solutions.
# Input should be the connections array, i.e. an array of [i,j] pairs;
# N is the number of pegs and holds.
def solutions(N):
  def abs: if . < 0 then -. else . end;

  # Is the proposed permutation (the input) ok?
  def ok(connections):
    . as $p
    | forall( connections; 
              (($p[.[0]] - $p[.[1]])|abs) != 1 );

   . as $connections | permutations(N) | select(ok($connections);
```

'''Part 3: The 8-peg no-connection puzzle'''

```jq
# The connectedness matrix:
# In this table, 0 represents "A", etc, and an entry [i,j]
# signifies that the holes with indices i and j are connected.
def connections: 
  [[0, 2], [0, 3], [0, 4],
   [1, 3], [1, 4], [1, 5],
   [6, 2], [6, 3], [6, 4],
   [7, 3], [7, 4], [7, 5],
   [2, 3], [3, 4], [4, 5]]
;

def solve:
  connections | solutions(8);

# pretty-print a solution for the 8-peg puzzle
def pp:
  def pegs: ["A", "B", "C", "D", "E", "F", "G", "H"];
  . as $in
  | ("
         A   B
        /|\\ /|\\
       / | X | \\
      /  |/ \\|  \\
     C - D - E - F
      \\  |\\ /|  /
       \\ | X | /
        \\|/ \\|/
         G   H
"   | explode) as $board
    | (pegs | map(explode)) as $letters
    | $letters
    | reduce range(0;length) as $i ($board; index($letters[$i]) as $ix | .[$ix] = $in[$i] + 48)
    | implode;
```

'''Examples''':

```jq
# To print all the solutions:
# solve | pp

# To count the number of solutions:
# count(solve)

# jq 1.4 lacks facilities for harnessing generators, 
# but the following will suffice here:
def one(f): reduce f as $s
  (null; if . == null then $s else . end);

one(solve) | pp

```

```sh
$ jq -n -r -f no_connection.jq

         5   6
        /|\ /|\
       / | X | \
      /  |/ \|  \
     7 - 1 - 8 - 2
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         3   4
```




## Julia


```julia

using Combinatorics

const HOLES = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H']
const PEGS = [1, 2, 3, 4, 5, 6, 7, 8]
const EDGES = [('A', 'C'), ('A', 'D'), ('A', 'E'),
               ('B', 'D'), ('B', 'E'), ('B', 'F'),
               ('C', 'G'), ('C', 'D'), ('D', 'G'),
               ('D', 'E'), ('D', 'H'), ('E', 'F'),
               ('E', 'G'), ('E', 'H'), ('F', 'H')]

goodperm(p) = all(e->abs(p[e[1]-'A'+1] - p[e[2]-'A'+1]) > 1, EDGES)

goodplacements() = [p for p in permutations(PEGS) if goodperm(p)]

const BOARD = raw"""
        A   B
       /|\ /|\
      / | X | \
     /  |/ \|  \
    C - D - E - F
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        G   H
"""

function printsolutions()
    solutions = goodplacements()
    println("Found $(length(solutions)) solutions.")
    for soln in solutions
        board = BOARD
        for (i, n) in enumerate(soln)
            board = replace(board, string('A' + i - 1) => string(n))
        end
        println(board); exit(1) # remove this exit for all solutions
    end
end

printsolutions()

```
 {{output}} 
```txt

Found 16 solutions.
        3   4
       /|\ /|\
      / | X | \
     /  |/ \|  \
    7 - 1 - 8 - 2
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        5   6

```



## Kotlin

```scala
// version 1.2.0

import kotlin.math.abs

// Holes A=0, B=1, …, H=7
// With connections:
const val conn = """
       A   B
      /|\ /|\
     / | X | \
    /  |/ \|  \
   C - D - E - F
    \  |\ /|  /
     \ | X | /
      \|/ \|/
       G   H
"""

val connections = listOf(
    0 to 2, 0 to 3, 0 to 4,   // A to C, D, E
    1 to 3, 1 to 4, 1 to 5,   // B to D, E, F
    6 to 2, 6 to 3, 6 to 4,   // G to C, D, E
    7 to 3, 7 to 4, 7 to 5,   // H to D, E, F
    2 to 3, 3 to 4, 4 to 5    // C-D, D-E, E-F
)

// 'isValid' checks if the pegs are a valid solution.
// If the absolute difference between any pair of connected pegs is
// greater than one it is a valid solution.
fun isValid(pegs: IntArray): Boolean {
   for ((a, b) in connections) {
       if (abs(pegs[a] - pegs[b]) <= 1) return false
   }
   return true
}

fun swap(pegs: IntArray, i: Int, j: Int) {
    val tmp = pegs[i]
    pegs[i] = pegs[j]
    pegs[j] = tmp
}

// 'solve' is a simple recursive brute force solver,
// it stops at the first found solution.
// It returns the solution, the number of positions tested,
// and the number of pegs swapped.

fun solve(): Triple<IntArray, Int, Int> {
    val pegs = IntArray(8) { it + 1 }
    var tests = 0
    var swaps = 0

    fun recurse(i: Int): Boolean {
        if (i >= pegs.size - 1) {
            tests++
            return isValid(pegs)
        }
        // Try each remaining peg from pegs[i] onwards
        for (j in i until pegs.size) {
            swaps++
            swap(pegs, i, j)
            if (recurse(i + 1)) return true
            swap(pegs, i, j)
        }
        return false
    }

    recurse(0)
    return Triple(pegs, tests, swaps)
}

fun pegsAsString(pegs: IntArray): String {
    val ca = conn.toCharArray()
    for ((i, c) in ca.withIndex()) {
        if (c in 'A'..'H') ca[i] = '0' + pegs[c - 'A']
    }
    return String(ca)
}

fun main(args: Array<String>) {
    val (p, tests, swaps) = solve()
    println(pegsAsString(p))
    println("Tested $tests positions and did $swaps swaps.")
}
```


```txt

       3   4
      /|\ /|\
     / | X | \
    /  |/ \|  \
   7 - 1 - 8 - 2
    \  |\ /|  /
     \ | X | /
      \|/ \|/
       5   6

Tested 12094 positions and did 20782 swaps.

```



## M2000 Interpreter

Final Version, print all solutions (16 from 40320 permutations)

Press space bar to see solutions so far.

```M2000 Interpreter

Module no_connection_puzzle {
      \\ Holes
      Inventory Connections="A":="CDE","B":="DEF","C":="ADG", "D":="ABCEGH"
      Append Connections, "E":="ABDFGH","F":="HEB", "G":="CDE","H":="DEF"
      Inventory ToDelete, Solutions
      \\ eliminate double connnections
      con=each(Connections)
      While con {
            m$=eval$(con, con^)
            c$=eval$(con)
            If c$="*" Then continue
            For i=1 to len(C$) {
                 d$=mid$(c$,i,1)
                 r$=Filter$(Connections$(d$), m$)
                 If r$<>"" Then  {
                         Return connections, d$:=r$
                  }  else   {
                        If m$=connections$(d$) Then {
                              Return connections, d$:="*"  : If not exist(todelete, d$)  Then  Append todelete, d$
                        }
                  }
            }
      }
      con=each(todelete)
      While con {
            Delete Connections, eval$(con)
      }
      Inventory Holes
      For i=0 to 7 : Append Holes, Chr$(65+i):=i : Next i
      CheckValid=lambda Holes, Connections (a$, arr) -> {
            val=Array(arr, Holes(a$))
            con$=Connections$(a$)
            res=True
            For i=1 to Len(con$) {
                 If Abs(Array(Arr, Holes(mid$(con$,i,1)))-val)<2 Then res=False: Exit
            }
            =res
      }
      a=(1,2,3,4,5,6,7,8)
      h=(,)
      solution=(,)
      done=False
      counter=0
      Print "Wait..."
      P(h, a)
      sol=Each(Solutions)
      While sol {
            Print "Solution:";sol^+1
            Disp(Eval(Solutions))
            aa$=Key$
      }
      Sub P(h, a)
      If len(a)<=1 Then process(cons(h, a)) : Exit Sub
      local b=cons(a)
      For i=1 to len(b) {
                  b=cons(cdr(b),car(b))
                  P(cons(h,car(b)), cdr(b))
      }
      End Sub
      Sub Process(a)
            counter++
            Print counter
            If keypress(32) Then {
            local  sol=Each(Solutions)
                        aa$=Key$
                        While sol {
                                    Print "Solution:";sol^+1
                                    Disp(Eval(Solutions))
                                    aa$=Key$
                        }  
            }
            hole=each(Connections)
            done=True
            While hole {
                  If not CheckValid(Eval$(hole, hole^), a) Then done=False : Exit
            }
            If done Then Append Solutions, Len(Solutions):=a : Print a
      End Sub
      Sub Disp(a)
            Print format$("    {0}   {1}", array(a), array(a,1))
            Print "   /|\ /|\"
            Print "  / | X | \"
            Print " /  |/ \|  \"
            Print Format$("{0} - {1} - {2} - {3}", array(a,2),array(a,3), array(a,4), array(a,5))
            Print " \  |\ /|  /"
            Print "  \ | X | /"
            Print "   \|/ \|/"
            Print Format$("    {0}   {1}", array(a,6), array(a,7))
      End Sub
}
no_connection_puzzle


```

```txt

    3   5
   /|\ /|\
  / | X | \
 /  |/ \|  \
7 - 1 - 8 - 2
 \  |\ /|  /
  \ | X | /
   \|/ \|/
    4   6
</pre >


## Mathematica

This one simply takes all permutations of the pegs and filters out invalid solutions.

```Mathematica
sol = Fold[
    Select[#, 
      Function[perm, Abs[perm[[#2[[1]]]] - perm[[#2[[2]]]]] > 1]] &, 
    Permutations[
     Range[8]], {{1, 3}, {1, 4}, {1, 5}, {2, 4}, {2, 5}, {2, 6}, {3, 
      4}, {3, 7}, {4, 5}, {4, 7}, {4, 8}, {5, 6}, {5, 7}, {5, 8}, {6, 
      8}}][[1]];
Print[StringForm[
   "    ``   ``\n   /|\\ /|\\\n  / | X | \\\n /  |/ \\|  \\\n`` - `` \
- `` - ``\n \\  |\\ /|  /\n  \\ | X | /\n   \\|/ \\|/\n    ``   ``", 
   Sequence @@ sol]];
```

```txt
    3   4
   /|\ /|\
  / | X | \
 /  |/ \|  \
7 - 1 - 8 - 2
 \  |\ /|  /
  \ | X | /
   \|/ \|/
    5   6
```



## Perl


```perl
#!/usr/bin/perl

use strict;
use warnings;

my $gap = qr/.{3}/s;

find( <<terminator );
-AB-
CDEF
-GH-
terminator

sub find
  {
  my $p = shift;
  $p =~ /(\d)$gap.{0,2}(\d)(??{abs $1 - $2 <= 1 ? '' : '(*F)'})/ ||
    $p =~ /^.*\n.*(\d)(\d)(??{abs $1 - $2 <= 1 ? '' : '(*F)'})/ and return;
  if( $p =~ /[A-H]/ )
    {
    find( $p =~ s/[A-H]/$_/r ) for grep $p !~ $_, 1 .. 8;
    }
  else
    {
    print $p =~ tr/-/ /r;
    exit;
    }
  }
```

```txt

 34 
7182
 56

```


## Perl 6

This uses a Warnsdorff solver, which cuts down the number of tries by more than a factor of six over the brute force approach. This same solver is used in:

* [[Solve a Hidato puzzle#Perl_6|Solve a Hidato puzzle]]
* [[Solve a Hopido puzzle#Perl_6|Solve a Hopido puzzle]]
* [[Solve a Holy Knight's tour#Perl_6|Solve a Holy Knight's tour]]
* [[Solve a Numbrix puzzle#Perl_6|Solve a Numbrix puzzle]]
* [[Solve the no connection puzzle#Perl_6|Solve the no connection puzzle]]

The idiosyncratic adjacency diagram is dealt with by the simple expedient of bending the two vertical lines <tt>||</tt> into two bows <tt>)(</tt>, such that adjacency can be calculated simply as a distance of 2 or less.

```perl6
my @adjacent = gather -> $y, $x {
    take [$y,$x] if abs($x|$y) > 2;
} for flat -5 .. 5 X -5 .. 5;

solveboard q:to/END/;
    . _ . . _ .
    . . . . . .
    _ . _ 1 . _
    . . . . . .
    . _ . . _ .
    END
 
sub solveboard($board) {
    my $max = +$board.comb(/\w+/);
    my $width = $max.chars;

    my @grid;
    my @known;
    my @neigh;
    my @degree;
 
    @grid = $board.lines.map: -> $line {
        [ $line.words.map: { /^_/ ?? 0 !! /^\./ ?? Rat !! $_ } ]
    }
 
    sub neighbors($y,$x --> List) {
        eager gather for @adjacent {
            my $y1 = $y + .[0];
            my $x1 = $x + .[1];
            take [$y1,$x1] if defined @grid[$y1][$x1];
        }
    }

    for ^@grid -> $y {
        for ^@grid[$y] -> $x {
            if @grid[$y][$x] -> $v {
                @known[$v] = [$y,$x];
            }
            if @grid[$y][$x].defined {
                @neigh[$y][$x] = neighbors($y,$x);
                @degree[$y][$x] = +@neigh[$y][$x];
            }
        }
    }
    print "\e[0H\e[0J";

    my $tries = 0;

    try_fill 1, @known[1];

    sub try_fill($v, $coord [$y,$x] --> Bool) {
        return True if $v > $max;
        $tries++;

        my $old = @grid[$y][$x];

        return False if +$old and $old != $v;
        return False if @known[$v] and @known[$v] !eqv $coord;

        @grid[$y][$x] = $v;               # conjecture grid value

        print "\e[0H";                    # show conjectured board
        for @grid -> $r {
            say do for @$r {
                when Rat { ' ' x $width }
                when 0   { '_' x $width }
                default  { .fmt("%{$width}d") }
            }
        }


        my @neighbors = @neigh[$y][$x][];

        my @degrees;
        for @neighbors -> \n [$yy,$xx] {
            my $d = --@degree[$yy][$xx];  # conjecture new degrees
            push @degrees[$d], n;         # and categorize by degree
        }

        for @degrees.grep(*.defined) -> @ties {
            for @ties.reverse {           # reverse works better for this hidato anyway
                return True if try_fill $v + 1, $_;
            }
        }

        for @neighbors -> [$yy,$xx] {
            ++@degree[$yy][$xx];          # undo degree conjectures
        }

        @grid[$y][$x] = $old;             # undo grid value conjecture
        return False;
    }
     
    say "$tries tries";
}
```


```txt
  4     3  
           
2   8 1   7
           
  6     5  
18 tries
```



## Phix

Brute force solution. I ordered the links highest letter first, then grouped by start letter to eliminate things asap. Nothing
to eliminate when placing A and B, when placing C, check that CA>1, when placing D, check that DA,DB,DC are all >1, etc. 

```Phix

constant txt = """
        A   B
       /|\ /|\
      / | X | \
     /  |/ \|  \
    C - D - E - F
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        G   H"""
--constant links = "CA DA DB DC EA EB ED FB FE GC GD GE HD HE HF"
constant links = {"","","A","ABC","ABD","BE","CDE","DEF"}

function solve(sequence s, integer idx, sequence part)
object res
integer v, p
    for i=1 to length(s) do
        v = s[i]
        for j=1 to length(links[idx]) do
            p = links[idx][j]-'@'
            if abs(v-part[p])<2 then v=0 exit end if
        end for
        if v then
            if length(s)=1 then return part&v end if
            res = solve(s[1..i-1]&s[i+1..$],idx+1,part&v)
            if sequence(res) then return res end if
        end if
    end for
    return 0
end function

printf(1,substitute_all(txt,"ABCDEFGH",solve("12345678",1,"")))
```

```txt

        3   4
       /|\ /|\
      / | X | \
     /  |/ \|  \
    7 - 1 - 8 - 2
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        5   6

```



## Picat


```Picat
import cp.

no_connection_puzzle(X) =>
  N = 8,
  X = new_list(N),
  X :: 1..N,
  Graph = 
    {{1,2}, {1,3}, {1,4},
     {2,1}, {2,3}, {2,5}, {2,6},
     {3,2}, {3,4}, {3,6}, {3,7},
     {4,1}, {4,3}, {4,6}, {4,7},
     {5,2}, {5,3}, {5,6}, {5,8},
     {6,2}, {6,3}, {6,4}, {6,5}, {6,7}, {6,8},
     {7,3}, {7,4}, {7,6}, {7,8},
     {8,5}, {8,6}, {8,7}},

  all_distinct(X),
  foreach(I in 1..Graph.length) 
     abs(X[Graph[I,1]]-X[Graph[I,2]]) #> 1 
  end,

  % symmetry breaking
  X[1] #< X[N],
  
  solve(X),
  println(X),
  nl,
  [A,B,C,D,E,F,G,H] = X,
  Solution = to_fstring(
  "    %d   %d       \n"++
  "   /|\\ /|\\      \n"++
  "  / | X | \\      \n"++
  " /  |/ \\|  \\    \n"++
  "%d - %d - %d - %d \n"++
  " \\  |\\ /|  /    \n"++
  "  \\ | X | /      \n"++
  "   \\|/ \\|/      \n"++
  "    %d   %d       \n",
  A,B,C,D,E,F,G,H),
  println(Solution).

```

Test:

```txt

Picat> no_connection_puzzle(_X)
[2,5,8,6,3,1,4,7]

    2   5       
   /|\ /|\      
  / | X | \      
 /  |/ \|  \    
8 - 6 - 3 - 1 
 \  |\ /|  /    
  \ | X | /      
   \|/ \|/      
    4   7       



```



## Prolog

Works with SWi-Prolog with module clpfd written by '''Markus Triska'''

We first compute a list of nodes, with sort this list, and we attribute a value at the nodes.

```Prolog
:- use_module(library(clpfd)).

edge(a, c).
edge(a, d).
edge(a, e).
edge(b, d).
edge(b, e).
edge(b, f).
edge(c, d).
edge(c, g).
edge(d, e).
edge(d, g).
edge(d, h).
edge(e, f).
edge(e, g).
edge(e, h).
edge(f, h).

connected(A, B) :-
	(   edge(A,B); edge(B, A)).

no_connection_puzzle(Vs) :-
	% construct the arranged list of the nodes
	bagof(A, B^(edge(A,B); edge(B, A)), Lst),
	sort(Lst, L),
	length(L, Len),

	% construct the list of the values
	length(Vs, Len),
	Vs ins 1..Len,
	all_distinct(Vs),

	% two connected nodes must have values different for more than 1
	set_constraints(L, Vs),
	label(Vs).


set_constraints([], []).

set_constraints([H | T], [VH | VT]) :-
	set_constraint(H, T, VH, VT),
	set_constraints(T, VT).



set_constraint(_, [], _, []).
set_constraint(H, [H1 | T1], V, [VH | VT]) :-
	connected(H, H1),
	(   V - VH #> 1; VH - V #> 1),
	set_constraint(H, T1, V, VT).

set_constraint(H, [H1 | T1], V, [_VH | VT]) :-
	\+connected(H, H1),
	set_constraint(H, T1, V, VT).


```

Output :

```txt
 ?- no_connection_puzzle(Vs).
Vs = [4, 3, 2, 8, 1, 7, 6, 5] .

 27 ?- setof(Vs, no_connection_puzzle(Vs), R), length(R, Len).
R = [[3, 4, 7, 1, 8, 2, 5, 6], [3, 5, 7, 1, 8, 2, 4|...], [3, 6, 7, 1, 8, 2|...], [3, 6, 7, 1, 8|...], [4, 3, 2, 8|...], [4, 5, 2|...], [4, 5|...], [4|...], [...|...]|...],
Len = 16.


```



## Python

A brute force search solution.

```python
from __future__ import print_function
from itertools import permutations
from enum import Enum

A, B, C, D, E, F, G, H = Enum('Peg', 'A, B, C, D, E, F, G, H')

connections = ((A, C), (A, D), (A, E),
               (B, D), (B, E), (B, F),
               (G, C), (G, D), (G, E),
               (H, D), (H, E), (H, F),
               (C, D), (D, E), (E, F))


def ok(conn, perm):
    """Connected numbers ok?"""
    this, that = (c.value - 1 for c in conn)
    return abs(perm[this] - perm[that]) != 1


def solve():
    return [perm for perm in permutations(range(1, 9))
            if all(ok(conn, perm) for conn in connections)]


if __name__ == '__main__':
    solutions = solve()
    print("A, B, C, D, E, F, G, H =", ', '.join(str(i) for i in solutions[0]))
```


```txt
A, B, C, D, E, F, G, H = 3, 4, 7, 1, 8, 2, 5, 6
```



;All solutions pretty printed:

Add the following code after that above:

```python
def pp(solution):
    """Prettyprint a solution"""
    boardformat = r"""
         A   B
        /|\ /|\
       / | X | \
      /  |/ \|  \
     C - D - E - F
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         G   H"""
    for letter, number in zip("ABCDEFGH", solution):
        boardformat = boardformat.replace(letter, str(number))
    print(boardformat)


if __name__ == '__main__':
    for i, s in enumerate(solutions, 1):
        print("\nSolution", i, end='')
        pp(s)
```


;Extra output:
<pre style="height:35ex;overflow:scroll">Solution 1
         3   4
        /|\ /|\
       / | X | \
      /  |/ \|  \
     7 - 1 - 8 - 2
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         5   6

Solution 2
         3   5
        /|\ /|\
       / | X | \
      /  |/ \|  \
     7 - 1 - 8 - 2
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         4   6

Solution 3
         3   6
        /|\ /|\
       / | X | \
      /  |/ \|  \
     7 - 1 - 8 - 2
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         4   5

Solution 4
         3   6
        /|\ /|\
       / | X | \
      /  |/ \|  \
     7 - 1 - 8 - 2
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         5   4

Solution 5
         4   3
        /|\ /|\
       / | X | \
      /  |/ \|  \
     2 - 8 - 1 - 7
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         6   5

Solution 6
         4   5
        /|\ /|\
       / | X | \
      /  |/ \|  \
     2 - 8 - 1 - 7
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         6   3

Solution 7
         4   5
        /|\ /|\
       / | X | \
      /  |/ \|  \
     7 - 1 - 8 - 2
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         3   6

Solution 8
         4   6
        /|\ /|\
       / | X | \
      /  |/ \|  \
     7 - 1 - 8 - 2
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         3   5

Solution 9
         5   3
        /|\ /|\
       / | X | \
      /  |/ \|  \
     2 - 8 - 1 - 7
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         6   4

Solution 10
         5   4
        /|\ /|\
       / | X | \
      /  |/ \|  \
     2 - 8 - 1 - 7
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         6   3

Solution 11
         5   4
        /|\ /|\
       / | X | \
      /  |/ \|  \
     7 - 1 - 8 - 2
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         3   6

Solution 12
         5   6
        /|\ /|\
       / | X | \
      /  |/ \|  \
     7 - 1 - 8 - 2
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         3   4

Solution 13
         6   3
        /|\ /|\
       / | X | \
      /  |/ \|  \
     2 - 8 - 1 - 7
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         4   5

Solution 14
         6   3
        /|\ /|\
       / | X | \
      /  |/ \|  \
     2 - 8 - 1 - 7
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         5   4

Solution 15
         6   4
        /|\ /|\
       / | X | \
      /  |/ \|  \
     2 - 8 - 1 - 7
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         5   3

Solution 16
         6   5
        /|\ /|\
       / | X | \
      /  |/ \|  \
     2 - 8 - 1 - 7
      \  |\ /|  /
       \ | X | /
        \|/ \|/
         4   3
```



## Racket



```racket
#lang racket
;; Solve the no connection puzzle. Tim Brown Oct. 2014

;; absolute difference of a and b if they are both true
(define (and- a b) (and a b (abs (- a b))))

;; Finds the differences of all established connections in the network
(define (network-diffs (A #f) (B #f) (C #f) (D #f) (E #f) (F #f) (G #f) (H #f))
  (list (and- A C) (and- A D) (and- A E)
        (and- B D) (and- B E) (and- B F)
        (and- C D) (and- C G)
        (and- D E) (and- D G) (and- D H)
        (and- E F) (and- E G) (and- E H)
        (and- F G)))

;; Make sure there is “no connection” in the network N; return N if good
(define (good-network? N)
  (and (for/and ((d (filter values (apply network-diffs N)))) (> d 1)) N))

;; possible optimisation is to reverse the arguments to network-diffs, reverse the return value from
;; this function and make this a cons but we're pretty quick here as it is.              
(define (find-good-network pegs (n/w null))
  (if (null? pegs) n/w
      (for*/or ((p pegs))
        (define n/w+ (append n/w (list p)))
        (and (good-network? n/w+)
             (find-good-network (remove p pegs =) n/w+)))))

(define (render-puzzle pzl)
  (apply printf (regexp-replace* "O" #<<EOS
    O   O
   /|\ /|\
  / | X | \
 /  |/ \|  \
O - O - O - O
 \  |\ /|  /
  \ | X | /
   \|/ \|/
    O   O~%
EOS
                                 "~a") pzl))

(render-puzzle (find-good-network '(1 2 3 4 5 6 7 8)))
```


```txt
    3   4
   /|\ /|\
  / | X | \
 /  |/ \|  \
7 - 1 - 8 - 2
 \  |\ /|  /
  \ | X | /
   \|/ \|/
    5   6
```



## REXX


### unannotated solutions
 

```rexx
/*REXX program  solves  the  "no-connection"  puzzle   (the puzzle has eight pegs).     */
parse arg limit .    /*number of solutions wanted.*/   /* ╔═══════════════════════════╗ */
if limit=='' | limit=="."  then limit=1                /* ║          A    B           ║ */
                                                       /* ║         /│\  /│\          ║ */
@.  =                                                  /* ║        / │ \/ │ \         ║ */
@.1 = 'A   C D E'                                      /* ║       /  │ /\ │  \        ║ */
@.2 = 'B   D E F'                                      /* ║      /   │/  \│   \       ║ */
@.3 = 'C   A D G'                                      /* ║     C────D────E────F      ║ */
@.4 = 'D   A B C E G'                                  /* ║      \   │\  /│   /       ║ */
@.5 = 'E   A B D F H'                                  /* ║       \  │ \/ │  /        ║ */
@.6 = 'F   B E H'                                      /* ║        \ │ /\ │ /         ║ */
@.7 = 'G   C D E'                                      /* ║         \│/  \│/          ║ */
@.8 = 'H   D E F'                                      /* ║          G    H           ║ */
cnt=0                                                  /* ╚═══════════════════════════╝ */
                  do pegs=1  while  @.pegs\=='';    _=word(@.pegs,1)
                  subs=0
                             do #=1  for  words(@.pegs) -1  /*create list of node paths.*/
                             __=word(@.pegs, # + 1);    if __>_  then iterate
                             subs=subs + 1;             !._.subs=__
                             end  /*#*/
                  !._.0=subs                     /*assign the number of the node paths. */
                  end   /*pegs*/
pegs=pegs-1                                      /*the number of pegs to be seated.     */
_='    '                                         /*_   is used for indenting the output.*/
        do        a=1  for pegs;     if ?('A')  then iterate
         do       b=1  for pegs;     if ?('B')  then iterate
          do      c=1  for pegs;     if ?('C')  then iterate
           do     d=1  for pegs;     if ?('D')  then iterate
            do    e=1  for pegs;     if ?('E')  then iterate
             do   f=1  for pegs;     if ?('F')  then iterate
              do  g=1  for pegs;     if ?('G')  then iterate
               do h=1  for pegs;     if ?('H')  then iterate
               say _ 'a='a _  'b='||b _  'c='c _  'd='d _  'e='e _  'f='f _  'g='g _ 'h='h
               cnt=cnt+1;        if cnt==limit  then leave a
               end   /*h*/
              end    /*g*/
             end     /*f*/
            end      /*e*/
           end       /*d*/
          end        /*c*/
         end         /*b*/
        end          /*a*/
say                                              /*display a blank line to the terminal.*/
s= left('s', cnt\==1)                            /*handle the case of plurals  (or not).*/
say 'found '   cnt   " solution"s'.'             /*display the number of solutions found*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
?: parse arg node;  nn=value(node)
   nH=nn+1
             do cn=c2d('A')  to c2d(node) - 1;    if value( d2c(cn) )==nn  then return 1
             end   /*cn*/                       /* [↑]  see if there any are duplicates.*/
   nL=nn-1
             do ch=1  for !.node.0              /* [↓]  see if there any  ¬= ±1  values.*/
             $=!.node.ch;        fn=value($)    /*the node name  and  its current peg #.*/
             if nL==fn | nH==fn  then return 1  /*if ≡ ±1,  then the node can't be used.*/
             end   /*ch*/                       /* [↑]  looking for suitable number.    */
   return 0                                     /*the subroutine arg value passed is OK.*/
```

'''output'''   when using the default input:

```txt

     a=3      b=4      c=7      d=1      e=8      f=2      g=5      h=6

found  1  solution.

```


'''output'''   when using the input of:   <tt> 999 </tt>

```txt

     a=3      b=4      c=7      d=1      e=8      f=2      g=5      h=6
     a=3      b=5      c=7      d=1      e=8      f=2      g=4      h=6
     a=3      b=6      c=7      d=1      e=8      f=2      g=4      h=5
     a=3      b=6      c=7      d=1      e=8      f=2      g=5      h=4
     a=4      b=3      c=2      d=8      e=1      f=7      g=6      h=5
     a=4      b=5      c=2      d=8      e=1      f=7      g=6      h=3
     a=4      b=5      c=7      d=1      e=8      f=2      g=3      h=6
     a=4      b=6      c=7      d=1      e=8      f=2      g=3      h=5
     a=5      b=3      c=2      d=8      e=1      f=7      g=6      h=4
     a=5      b=4      c=2      d=8      e=1      f=7      g=6      h=3
     a=5      b=4      c=7      d=1      e=8      f=2      g=3      h=6
     a=5      b=6      c=7      d=1      e=8      f=2      g=3      h=4
     a=6      b=3      c=2      d=8      e=1      f=7      g=4      h=5
     a=6      b=3      c=2      d=8      e=1      f=7      g=5      h=4
     a=6      b=4      c=2      d=8      e=1      f=7      g=5      h=3
     a=6      b=5      c=2      d=8      e=1      f=7      g=4      h=3

found  16  solutions.

```



### annotated solutions
 
Usage note:   if the   '''limit'''   (the 1<sup>st</sup> argument)   is negative, a diagram (node graph) is shown.  

```rexx
/*REXX program  solves  the  "no-connection"  puzzle   (the puzzle has eight pegs).     */
@abc='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
parse arg limit .    /*number of solutions wanted.*/   /* ╔═══════════════════════════╗ */
if limit=='' | limit=="."  then limit=1                /* ║          A    B           ║ */
oLimit=limit;                   limit=abs(limit)       /* ║         /│\  /│\          ║ */
@.  =                                                  /* ║        / │ \/ │ \         ║ */
@.1 = 'A   C D E'                                      /* ║       /  │ /\ │  \        ║ */
@.2 = 'B   D E F'                                      /* ║      /   │/  \│   \       ║ */
@.3 = 'C   A D G'                                      /* ║     C────D────E────F      ║ */
@.4 = 'D   A B C E G'                                  /* ║      \   │\  /│   /       ║ */
@.5 = 'E   A B D F H'                                  /* ║       \  │ \/ │  /        ║ */
@.6 = 'F   B E H'                                      /* ║        \ │ /\ │ /         ║ */
@.7 = 'G   C D E'                                      /* ║         \│/  \│/          ║ */
@.8 = 'H   D E F'                                      /* ║          G    H           ║ */
cnt=0                                                  /* ╚═══════════════════════════╝ */
                  do pegs=1  while  @.pegs\=='';    _=word(@.pegs, 1)
                  subs=0
                             do #=1  for  words(@.pegs) -1  /*create list of node paths.*/
                             __=word(@.pegs, #+1);      if __>_  then iterate
                             subs=subs + 1;             !._.subs=__
                             end  /*#*/
                  !._.0=subs                    /*assign the number of the node paths.  */
                  end   /*pegs*/
pegs=pegs - 1                                   /*the number of pegs to be seated.      */
_='    '                                        /*_   is used for indenting the output. */
        do        a=1  for pegs;     if ?('A')  then iterate
         do       b=1  for pegs;     if ?('B')  then iterate
          do      c=1  for pegs;     if ?('C')  then iterate
           do     d=1  for pegs;     if ?('D')  then iterate
            do    e=1  for pegs;     if ?('E')  then iterate
             do   f=1  for pegs;     if ?('F')  then iterate
              do  g=1  for pegs;     if ?('G')  then iterate
               do h=1  for pegs;     if ?('H')  then iterate
               call showNodes
               cnt=cnt+1;        if cnt==limit  then leave a
               end   /*h*/
              end    /*g*/
             end     /*f*/
            end      /*e*/
           end       /*d*/
          end        /*c*/
         end         /*b*/
        end          /*a*/
say                                              /*display a blank line to the terminal.*/
s=left('s', cnt\==1)                             /*handle the case of plurals  (or not).*/
say 'found '   cnt   " solution"s'.'             /*display the number of solutions found*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
?: parse arg node;  nn=value(node)
   nH=nn+1
             do cn=c2d('A')  to c2d(node)-1;  if value( d2c(cn) )==nn  then return 1
             end   /*cn*/                        /* [↑]  see if there're any duplicates.*/
   nL=nn-1
             do ch=1  for !.node.0               /* [↓]  see if there any ¬= ±1  values.*/
             $=!.node.ch;        fn=value($)     /*the node name  and its current peg #.*/
             if nL==fn | nH==fn  then return 1   /*if ≡ ±1, then the node can't be used.*/
             end   /*ch*/                        /* [↑]  looking for suitable number.   */
   return 0                                      /*the subroutine arg value passed is OK*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
showNodes: _=left('', 5)                         /*_   is used for padding the output.  */
show=0                                           /*indicates no graph has been found yet*/
      do box=1  for sourceline()  while oLimit<0 /*Negative?  Then display the diagram. */
      xw=sourceline(box)                         /*get a source line of this program.   */
      p2=lastpos('*', xw)                        /*the position of    last     asterisk.*/
      p1=lastpos('*', xw, max(1, p2-1) )         /* "      "     " penultimate     "    */
      if pos('╔', xw)\==0  then show=1           /*Have found the top-left box corner ? */
      if \show             then iterate          /*Not found?  Then skip this line.     */
      xb=substr(xw, p1+1, p2-p1-2)               /*extract the  "box"  part of line.    */
      xt=xb                                      /*get a working copy of the box.       */
                     do jx=1  for pegs           /*do a substitution for all the pegs.  */
                     @=substr(@abc, jx, 1)       /*get the name of the peg  (A ──► Z).  */
                     xt=translate(xt,value(@),@) /*substitute the peg name with a value.*/
                     end   /*jx*/                /* [↑]    graph is limited to 26 nodes.*/
      say _ xb _ _ xt                            /*display one line of the graph.       */
      if pos('╝', xw)\==0  then return           /*Is this last line of graph? Then stop*/
      end   /*box*/
say _  'a='a _    'b='||b _    'c='c _    'd='d _   ' e='e _    'f='f _    'g='g _   'h='h
return
```

'''output''' when using the input of:   <tt> -3 </tt>

```txt

       ╔═══════════════════════════╗              ╔═══════════════════════════╗
       ║          A    B           ║              ║          3    4           ║
       ║         /│\  /│\          ║              ║         /│\  /│\          ║
       ║        / │ \/ │ \         ║              ║        / │ \/ │ \         ║
       ║       /  │ /\ │  \        ║              ║       /  │ /\ │  \        ║
       ║      /   │/  \│   \       ║              ║      /   │/  \│   \       ║
       ║     C────D────E────F      ║              ║     7────1────8────2      ║
       ║      \   │\  /│   /       ║              ║      \   │\  /│   /       ║
       ║       \  │ \/ │  /        ║              ║       \  │ \/ │  /        ║
       ║        \ │ /\ │ /         ║              ║        \ │ /\ │ /         ║
       ║         \│/  \│/          ║              ║         \│/  \│/          ║
       ║          G    H           ║              ║          5    6           ║
       ╚═══════════════════════════╝              ╚═══════════════════════════╝
       ╔═══════════════════════════╗              ╔═══════════════════════════╗
       ║          A    B           ║              ║          3    5           ║
       ║         /│\  /│\          ║              ║         /│\  /│\          ║
       ║        / │ \/ │ \         ║              ║        / │ \/ │ \         ║
       ║       /  │ /\ │  \        ║              ║       /  │ /\ │  \        ║
       ║      /   │/  \│   \       ║              ║      /   │/  \│   \       ║
       ║     C────D────E────F      ║              ║     7────1────8────2      ║
       ║      \   │\  /│   /       ║              ║      \   │\  /│   /       ║
       ║       \  │ \/ │  /        ║              ║       \  │ \/ │  /        ║
       ║        \ │ /\ │ /         ║              ║        \ │ /\ │ /         ║
       ║         \│/  \│/          ║              ║         \│/  \│/          ║
       ║          G    H           ║              ║          4    6           ║
       ╚═══════════════════════════╝              ╚═══════════════════════════╝
       ╔═══════════════════════════╗              ╔═══════════════════════════╗
       ║          A    B           ║              ║          3    6           ║
       ║         /│\  /│\          ║              ║         /│\  /│\          ║
       ║        / │ \/ │ \         ║              ║        / │ \/ │ \         ║
       ║       /  │ /\ │  \        ║              ║       /  │ /\ │  \        ║
       ║      /   │/  \│   \       ║              ║      /   │/  \│   \       ║
       ║     C────D────E────F      ║              ║     7────1────8────2      ║
       ║      \   │\  /│   /       ║              ║      \   │\  /│   /       ║
       ║       \  │ \/ │  /        ║              ║       \  │ \/ │  /        ║
       ║        \ │ /\ │ /         ║              ║        \ │ /\ │ /         ║
       ║         \│/  \│/          ║              ║         \│/  \│/          ║
       ║          G    H           ║              ║          4    5           ║
       ╚═══════════════════════════╝              ╚═══════════════════════════╝

found  3  solutions.

```



## Ruby

Be it Golden Frogs jumping on trancendental lilly pads, or a Knight on a board, or square pegs into round holes this is essentially a Hidato Like Problem, so I use [http://rosettacode.org/wiki/Solve_a_Hidato_puzzle#With_Warnsdorff HLPSolver]:

```ruby

#  Solve No Connection Puzzle
#
#  Nigel_Galloway
#  October 6th., 2014
require 'HLPSolver'
ADJACENT = [[0,0]]
A,B,C,D,E,F,G,H = [0,1],[0,2],[1,0],[1,1],[1,2],[1,3],[2,1],[2,2]

board1 = <<EOS
  . 0 0 .
  0 0 1 0 
  . 0 0 .

EOS
g = HLPsolver.new(board1)
g.board[A[0]][A[1]].adj = [B,G,H,F]            
g.board[B[0]][B[1]].adj = [A,C,G,H]            
g.board[C[0]][C[1]].adj = [B,E,F,H]          
g.board[D[0]][D[1]].adj = [F]                
g.board[E[0]][E[1]].adj = [C]                
g.board[F[0]][F[1]].adj = [A,C,D,G]          
g.board[G[0]][G[1]].adj = [A,B,F,H]            
g.board[H[0]][H[1]].adj = [A,B,C,G]            
g.solve

```

```txt

Problem:
     0  0
  0  0  1  0
     0  0

Solution:
     5  3
  2  8  1  7
     6  4

```



## Scala

{{Out}}Best seen in running your browser either by [https://scalafiddle.io/sf/Ub2LEup/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/ZXGSJLFEQe21Frh4Vwp0WA Scastie (remote JVM)].

```Scala
object NoConnection extends App {

  private def links = Seq(
    Seq(2, 3, 4), // A to C,D,E
    Seq(3, 4, 5), // B to D,E,F
    Seq(2, 4), // D to C, E
    Seq(5), // E to F
    Seq(2, 3, 4), // G to C,D,E
    Seq(3, 4, 5)) // H to D,E,F

  private def genRandom: LazyList[Seq[Int]] = util.Random.shuffle((1 to 8).toList) #:: genRandom

  private def notSolved(links: Seq[Seq[Int]], pegs: Seq[Int]): Boolean =
    links.indices.forall(
      i => !links(i).forall(peg => math.abs(pegs(i) - peg) == 1))

  private def printResult(pegs: Seq[Int]) = {
    println(f"${pegs(0)}%3d${pegs(1)}%2d")
    println(f"${pegs(2)}%1d${pegs(3)}%2d${pegs(4)}%2d${pegs(5)}%2d")
    println(f"${pegs(6)}%3d${pegs(7)}%2d")
  }

  printResult(genRandom.dropWhile(!notSolved(links, _)).head)
}
```



## Tcl

```tcl
package require Tcl 8.6
package require struct::list

proc haveAdjacent {a b c d e f g h} {
    expr {
	[edge $a $c] ||
	[edge $a $d] ||
	[edge $a $e] ||
	[edge $b $d] ||
	[edge $b $e] ||
	[edge $b $f] ||
	[edge $c $d] ||
	[edge $c $g] ||
	[edge $d $e] ||
	[edge $d $g] ||
	[edge $d $h] ||
	[edge $e $f] ||
	[edge $e $g] ||
	[edge $e $h] ||
	[edge $f $h]
    }
}
proc edge {x y} {
    expr {abs($x-$y) == 1}
}

set layout [string trim {
        A   B
       /|\ /|\ 
      / | X | \ 
     /  |/ \|  \ 
    C - D - E - F
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        G   H
} \n]
struct::list foreachperm p {1 2 3 4 5 6 7 8} {
    if {![haveAdjacent {*}$p]} {
	puts [string map [join [
	    lmap name {A B C D E F G H} val $p {list $name $val}
	]] $layout]
	break
    }
}
```

```txt
        3   4
       /|\ /|\ 
      / | X | \ 
     /  |/ \|  \ 
    7 - 1 - 8 - 2
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        5   6
```



## XPL0


```XPL0
include c:\cxpl\codes;

int     Hole, Max, I;
char    Box(8), Str;
def     A, B, C, D, E, F, G, H;
[for Hole:= 0 to 7 do Box(Hole):= Hole+1;
Max:= 7;
while   abs(Box(D)-Box(A)) < 2  or  abs(Box(D)-Box(C)) < 2  or
        abs(Box(D)-Box(G)) < 2  or  abs(Box(D)-Box(E)) < 2  or
        abs(Box(A)-Box(C)) < 2  or  abs(Box(C)-Box(G)) < 2  or
        abs(Box(G)-Box(E)) < 2  or  abs(Box(E)-Box(A)) < 2  or
        abs(Box(E)-Box(B)) < 2  or  abs(Box(E)-Box(H)) < 2  or
        abs(Box(E)-Box(F)) < 2  or  abs(Box(B)-Box(D)) < 2  or
        abs(Box(D)-Box(H)) < 2  or  abs(Box(H)-Box(F)) < 2  or
        abs(Box(F)-Box(B)) < 2  do
           loop [I:= Box(0);                    \next permutation
                for Hole:= 0 to Max-1 do Box(Hole):= Box(Hole+1);
                Box(Max):= I;
                if I # Max+1 then [Max:= 7;  quit]
                else Max:= Max-1];
Str:= "
    #   #
   /|\ /|\
  / | X | \
 /  |/ \|  \
# - # - # - #
 \  |\ /|  /
  \ | X | /
   \|/ \|/
    #   #
";
Hole:= 0;  I:= 0;
repeat  if Str(I)=^# then [Str(I):= Box(Hole)+^0;  Hole:= Hole+1];
        I:= I+1;
until   Hole = 8;
Text(0, Str);
]
```


```txt

    5   6
   /|\ /|\
  / | X | \
 /  |/ \|  \
7 - 1 - 8 - 2
 \  |\ /|  /
  \ | X | /
   \|/ \|/
    3   4

```



## zkl

```zkl
const PegA=0, PegB=1, PegC=2, PegD=3, PegE=4, PegF=5, PegG=6, PegH=7;
connections:=T( 
   T(PegA, PegC), T(PegA, PegD), T(PegA, PegE),
   T(PegB, PegD), T(PegB, PegE), T(PegB, PegF),
   T(PegC, PegD), T(PegD, PegE), T(PegE, PegF),
   T(PegG, PegC), T(PegG, PegD), T(PegG, PegE),
   T(PegH, PegD), T(PegH, PegE), T(PegH, PegF) );
CZ:=connections.len();
#<<<   // Use "raw" string in a "here doc" so \ isn't a quote char
board:=
0'$        A   B
       /|\ /|\
      / | X | \
     /  |/ \|  \
    C - D - E - F
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        G   H$;
#<<<	// end "here doc"

perm:=T(PegA,PegB,PegC,PegD,PegE,PegF,PegG,PegH); // Peg[8]
foreach p in (Utils.Helpers.permuteW(perm)){ // permutation iterator
   if(connections.filter1('wrap([(a,b)]){ (p[a] - p[b]).abs()<=1 })) continue;
   board.translate("ABCDEFGH",p.apply('+(1)).concat()).println(); 
   break;  // comment out to see all 16 solutions
}
```

The filter1 method stops on the first True, so it acts like a conditional or.
```txt

        5   6
       /|\ /|\
      / | X | \
     /  |/ \|  \
    7 - 1 - 8 - 2
     \  |\ /|  /
      \ | X | /
       \|/ \|/
        3   4

```

