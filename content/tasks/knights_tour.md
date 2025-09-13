+++
title = "Knight's tour"
description = ""
date = 2019-10-19T20:18:42Z
aliases = []
[extra]
id = 9802
[taxonomies]
categories = ["task"]
tags = []
+++

[[File:Knight's_tour_7x7.png|400px||right]]

## Task

[[wp:Knight%27s_tour|Problem]]: you have a standard 8x8 chessboard, empty but for a single knight on some square.   Your task is to emit a series of legal knight moves that result in the knight visiting every square on the chessboard exactly once. Note that it is ''not'' a requirement that the tour be "closed"; that is, the knight need not end within a single move of its start position.

Input and output may be textual or graphical, according to the conventions of the programming environment.  If textual, squares should be indicated in [http://en.wikipedia.org/wiki/Algebraic_chess_notation algebraic notation].  The output should indicate the order in which the knight visits the squares, starting with the initial position.  The form of the output may be a diagram of the board with the squares numbered according to visitation sequence, or a textual list of algebraic coordinates in order, or even an actual animation of the knight moving around the chessboard.

Input: starting square

Output: move sequence


## Related tasks

* [[A* search algorithm]]
* [[N-queens problem]]
* [[Solve a Hidato puzzle]]
* [[Solve a Holy Knight's tour]]
* [[Solve a Hopido puzzle]]
* [[Solve a Numbrix puzzle]]
* [[Solve the no connection puzzle]]





## 360 Assembly

```360asm
*        Knight's tour             20/03/2017
KNIGHT   CSECT
         USING  KNIGHT,R13         base registers
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         MVC    PG(20),=CL20'Knight''s tour ..x..'
         L      R1,NN              n
         XDECO  R1,XDEC            edit
         MVC    PG+14(2),XDEC+10   n
         MVC    PG+17(2),XDEC+10   n
         XPRNT  PG,L'PG            print buffer
         LA     R0,1               1
         ST     R0,X               x=1
         ST     R0,Y               y=1
         SR     R0,R0              0
         ST     R0,TOTAL           total=0
LOOP     EQU    *                  do loop
         L      R1,X                 x
         BCTR   R1,0                 -1
         MH     R1,NNH               *n
         L      R0,Y                 y
         BCTR   R0,0                 -1
         AR     R1,R0                (x-1)*n+y-1
         SLA    R1,1                 ((x-1)*n+y-1)*2
         LA     R0,1                 1
         STH    R0,BOARD(R1)         board(x,y)=1
         L      R2,TOTAL             total
         LA     R2,1(R2)             total+1
         STH    R2,DISP(R1)          disp(x,y)=total+1
         ST     R2,TOTAL             total=total+1
         L      R1,X                 x
         L      R2,Y                 y
         BAL    R14,CHOOSEMV         call choosemv(x,y)
         C      R0,=F'0'           until(choosemv(x,y)=0)
         BNE    LOOP               loop
         LA     R2,KN*KN           n*n
       IF C,R2,NE,TOTAL THEN       if total<>n*n then
         XPRNT  =C'error!!',7        print error
       ENDIF    ,                  endif
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,NN)       do i=1 to n
         MVC    PG,=CL128' '         init buffer
         LA     R10,PG               pgi=0
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,NN)         do j=1 to n
         LR     R1,R6                  i
         BCTR   R1,0                   -1
         MH     R1,NNH                 *n
         LR     R0,R7                  j
         BCTR   R0,0                   -1
         AR     R1,R0                  (i-1)*n+j-1
         SLA    R1,1                   ((i-1)*n+j-1)*2
         LH     R2,DISP(R1)            disp(i,j)
         XDECO  R2,XDEC                edit
         MVC    0(4,R10),XDEC+8        output
         LA     R10,4(R10)             pgi+=4
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         XPRNT  PG,L'PG              print buffer
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            return_code=0
         BR     R14                exit
*------- ----   ----------------------------------------
CHOOSEMV EQU    *                  choosemv(xc,yc)
         ST     R14,SAVEACMV       save return point
         ST     R1,XC              store xc
         ST     R2,YC              store yc
         MVC    MM,=F'9'           m=9
         L      R1,XC              xc
         LA     R1,1(R1)
         L      R2,YC              yc
         LA     R2,2(R2)
         BAL    R14,TRYMV          call trymv(xc+1,yc+2)
         L      R1,XC              xc
         LA     R1,1(R1)
         L      R2,YC              yc
         SH     R2,=H'2'
         BAL    R14,TRYMV          call trymv(xc+1,yc-2)
         L      R1,XC              xc
         BCTR   R1,0
         L      R2,YC              yc
         LA     R2,2(R2)
         BAL    R14,TRYMV          call trymv(xc-1,yc+2)
         L      R1,XC              xc
         BCTR   R1,0
         L      R2,YC              yc
         SH     R2,=H'2'
         BAL    R14,TRYMV          call trymv(xc-1,yc-2)
         L      R1,XC              xc
         LA     R1,2(R1)
         L      R2,YC              yc
         LA     R2,1(R2)
         BAL    R14,TRYMV          call trymv(xc+2,yc+1)
         L      R1,XC              xc
         LA     R1,2(R1)
         L      R2,YC              yc
         BCTR   R2,0
         BAL    R14,TRYMV          call trymv(xc+2,yc-1)
         L      R1,XC              xc
         SH     R1,=H'2'
         L      R2,YC              yc
         LA     R2,1(R2)
         BAL    R14,TRYMV          call trymv(xc-2,yc+1)
         L      R1,XC              xc
         SH     R1,=H'2'
         L      R2,YC              yc
         BCTR   R2,0
         BAL    R14,TRYMV          call trymv(xc-2,yc-1)
         L      R4,MM              m
       IF C,R4,EQ,=F'9' THEN       if m=9 then
         LA     R0,0                 return(0)
       ELSE     ,                  else
         MVC    X,NEWX               x=newx
         MVC    Y,NEWY               y=newy
         LA     R0,1                 return(1)
       ENDIF    ,                  endif
         L      R14,SAVEACMV       restore return point
         BR     R14                return
SAVEACMV DS     A                  return point
*------- ----   ----------------------------------------
TRYMV    EQU    *                  trymv(xt,yt)
         ST     R14,SAVEATMV       save return point
         ST     R1,XT              store xt
         ST     R2,YT              store yt
         SR     R10,R10            n=0
         BAL    R14,VALIDMV
       IF LTR,R0,Z,R0 THEN         if validmv(xt,yt)=0 then
         LA     R0,0                 return(0)
         B      RETURTMV
       ENDIF    ,                  endif
         L      R1,XT
         LA     R1,1(R1)           xt+1
         L      R2,YT
         LA     R2,2(R2)           yt+2
         BAL    R14,VALIDMV
       IF C,R0,EQ,=F'1' THEN       if validmv(xt+1,yt+2)=1 then
         LA     R10,1(R10)           n=n+1;
       ENDIF    ,                  endif
         L      R1,XT
         LA     R1,1(R1)           xt+1
         L      R2,YT
         SH     R2,=H'2'           yt-2
         BAL    R14,VALIDMV
       IF C,R0,EQ,=F'1' THEN       if validmv(xt+1,yt-2)=1 then
         LA     R10,1(R10)           n=n+1;
       ENDIF    ,                  endif
         L      R1,XT
         BCTR   R1,0               xt-1
         L      R2,YT
         LA     R2,2(R2)           yt+2
         BAL    R14,VALIDMV
       IF C,R0,EQ,=F'1' THEN       if validmv(xt-1,yt+2)=1 then
         LA     R10,1(R10)           n=n+1;
       ENDIF    ,                  endif
         L      R1,XT
         BCTR   R1,0               xt-1
         L      R2,YT
         SH     R2,=H'2'           yt-2
         BAL    R14,VALIDMV
       IF C,R0,EQ,=F'1' THEN       if validmv(xt-1,yt-2)=1 then
         LA     R10,1(R10)           n=n+1;
       ENDIF    ,                  endif
         L      R1,XT
         LA     R1,2(R1)           xt+2
         L      R2,YT
         LA     R2,1(R2)           yt+1
         BAL    R14,VALIDMV
       IF C,R0,EQ,=F'1' THEN       if validmv(xt+2,yt+1)=1 then
         LA     R10,1(R10)           n=n+1;
       ENDIF    ,                  endif
         L      R1,XT
         LA     R1,2(R1)           xt+2
         L      R2,YT
         BCTR   R2,0               yt-1
         BAL    R14,VALIDMV
       IF C,R0,EQ,=F'1' THEN       if validmv(xt+2,yt-1)=1 then
         LA     R10,1(R10)           n=n+1;
       ENDIF    ,                  endif
         L      R1,XT
         SH     R1,=H'2'           xt-2
         L      R2,YT
         LA     R2,1(R2)           yt+1
         BAL    R14,VALIDMV
       IF C,R0,EQ,=F'1' THEN       if validmv(xt-2,yt+1)=1 then
         LA     R10,1(R10)           n=n+1;
       ENDIF    ,                  endif
         L      R1,XT
         SH     R1,=H'2'           xt-2
         L      R2,YT
         BCTR   R2,0               yt-1
         BAL    R14,VALIDMV
       IF C,R0,EQ,=F'1' THEN       if validmv(xt-2,yt-1)=1 then
         LA     R10,1(R10)           n=n+1;
       ENDIF    ,                  endif
       IF C,R10,LT,MM THEN         if n<m then
         ST     R10,MM               m=n
         MVC    NEWX,XT              newx=xt
         MVC    NEWY,YT              newy=yt
       ENDIF    ,                  endif
RETURTMV L      R14,SAVEATMV       restore return point
         BR     R14                return
SAVEATMV DS     A                  return point
*------- ----   ----------------------------------------
VALIDMV  EQU    *                  validmv(xv,yv)
         C      R1,=F'1'           if xv<1  then
         BL     RET0
         C      R1,NN              if xv>nn then
         BH     RET0
         C      R2,=F'1'           if yv<1  then
         BL     RET0
         C      R2,NN              if yv>nn then
         BNH    OK
RET0     SR     R0,R0              return(0)
         B      RETURVMV
OK       LR     R3,R1              xv
         BCTR   R3,0
         MH     R3,NNH             *n
         LR     R0,R2              yv
         BCTR   R0,0
         AR     R3,R0
         SLA    R3,1
         LH     R4,BOARD(R3)       board(xv,yv)
       IF LTR,R4,Z,R4 THEN         if board(xv,yv)=0 then
         LA     R0,1                 return(1)
       ELSE     ,                  else
         SR     R0,R0                return(0)
       ENDIF    ,                  endif
RETURVMV BR     R14                return
*        ----   ----------------------------------------
KN       EQU    8                  n  compile-time
NN       DC     A(KN)              n  fullword
NNH      DC     AL2(KN)            n  halfword
BOARD    DC     (KN*KN)H'0'        dim board(n,n) init 0
DISP     DC     (KN*KN)H'0'        dim  disp(n,n) init 0
X        DS     F
Y        DS     F
TOTAL    DS     F
XC       DS     F
YC       DS     F
MM       DS     F
NEWX     DS     F
NEWY     DS     F
XT       DS     F
YT       DS     F
XDEC     DS     CL12
PG       DC     CL128' '           buffer
         YREGS
         END    KNIGHT
```

```txt

Knight's tour  8x 8
   1   4  57  20  47   6  49  22
  34  19   2   5  58  21  46   7
   3  56  35  60  37  48  23  50
  18  33  38  55  52  59   8  45
  39  14  53  36  61  44  51  24
  32  17  40  43  54  27  62   9
  13  42  15  30  11  64  25  28
  16  31  12  41  26  29  10  63

```




## Ada


First, we specify a naive implementation the package Knights_Tour with naive backtracking. It is a bit more general than required for this task, by providing a mechanism '''not''' to visit certain coordinates. This mechanism is actually useful for the task [[Solve a Holy Knight's tour#Ada]], which also uses the package Knights_Tour.


```Ada
generic
   Size: Integer;
package Knights_Tour is

   subtype Index is Integer range 1 .. Size;
   type Tour is array  (Index, Index) of Natural;
   Empty: Tour := (others => (others => 0));

   function Get_Tour(Start_X, Start_Y: Index; Scene: Tour := Empty) return Tour;
   -- finds tour via backtracking
   -- either no tour has been found, i.e., Get_Tour returns Scene
   -- or the Result(X,Y)=K if and only if I,J is visited at the K-th move
   -- for all X, Y, Scene(X,Y) must be either 0 or Natural'Last,
   --   where Scene(X,Y)=Natural'Last means "don't visit coordiates (X,Y)!"

   function Count_Moves(Board: Tour) return Natural;
   -- counts the number of possible moves, i.e., the number of 0's on the board

   procedure Tour_IO(The_Tour: Tour; Width: Natural := 4);
   -- writes The_Tour to the output using Ada.Text_IO;

end Knights_Tour;
```


Here is the implementation:


```Ada
with Ada.Text_IO, Ada.Integer_Text_IO;

package body Knights_Tour is


   type Pair is array(1..2) of Integer;
   type Pair_Array is array (Positive range <>) of Pair;

   Pairs: constant Pair_Array (1..8)
     := ((-2,1),(-1,2),(1,2),(2,1),(2,-1),(1,-2),(-1,-2),(-2,-1));
   -- places for the night to go (relative to the current position)

   function Count_Moves(Board: Tour) return Natural is
      N: Natural := 0;
   begin
      for I in Index loop
	 for J in Index loop
	    if Board(I,J) < Natural'Last then
	       N := N + 1;
	    end if;
	 end loop;
      end loop;
      return N;
   end Count_Moves;

   function Get_Tour(Start_X, Start_Y: Index; Scene: Tour := Empty)
		    return Tour is
      Done: Boolean;
      Move_Count: Natural := Count_Moves(Scene);
      Visited: Tour;

      -- Visited(I, J) = 0: not yet visited
      -- Visited(I, J) = K: visited at the k-th move
      -- Visited(I, J) = Integer'Last: never visit

      procedure Visit(X, Y: Index; Move_Number: Positive; Found: out Boolean) is
         XX, YY: Integer;
      begin
         Found := False;
         Visited(X, Y) := Move_Number;
         if Move_Number = Move_Count then
            Found := True;
         else
            for P in Pairs'Range loop
               XX := X + Pairs(P)(1);
               YY := Y + Pairs(P)(2);
               if (XX in Index) and then (YY in Index)
                                and then Visited(XX, YY) = 0 then
                  Visit(XX, YY, Move_Number+1, Found); -- recursion
                  if Found then
                     return; -- no need to search further
                  end if;
               end if;
            end loop;
            Visited(X, Y) := 0; -- undo previous mark
         end if;
      end Visit;

   begin
      Visited := Scene;
      Visit(Start_X, Start_Y, 1, Done);
      if not Done then
         Visited := Scene;
      end if;
      return Visited;
   end Get_Tour;

   procedure Tour_IO(The_Tour: Tour; Width: Natural := 4) is
   begin
      for I in Index loop
         for J in Index loop
	    if The_Tour(I, J) < Integer'Last then
	       Ada.Integer_Text_IO.Put(The_Tour(I, J), Width);
	    else
	       for W in 1 .. Width-1 loop
		  Ada.Text_IO.Put(" ");
	       end loop;
	       Ada.Text_IO.Put("-"); -- deliberately not visited
	    end if;
         end loop;
         Ada.Text_IO.New_Line;
      end loop;
   end Tour_IO;

end Knights_Tour;
```


Here is the main program:


```Ada
with Knights_Tour, Ada.Command_Line;

procedure Test_Knight is

   Size: Positive := Positive'Value(Ada.Command_Line.Argument(1));

   package KT is new Knights_Tour(Size => Size);

begin
   KT.Tour_IO(KT.Get_Tour(1, 1));
end Test_Knight;
```


For small sizes, this already works well (< 1 sec for size 8). Sample output:

```txt
>./test_knight 8
   1  38  55  34   3  36  19  22
  54  47   2  37  20  23   4  17
  39  56  33  46  35  18  21  10
  48  53  40  57  24  11  16   5
  59  32  45  52  41  26   9  12
  44  49  58  25  62  15   6  27
  31  60  51  42  29   8  13  64
  50  43  30  61  14  63  28   7
```


For larger sizes we'll use Warnsdorff's heuristic (without any thoughtful tie breaking). We enhance the specification adding a function Warnsdorff_Get_Tour. This enhancement of the package Knights_Tour will also be used for the task [[Solve a Holy Knight's tour#Ada]]. The specification of Warnsdorff_Get_Tour is the following.

```Ada

   function Warnsdorff_Get_Tour(Start_X, Start_Y: Index; Scene: Tour := Empty)
			       return Tour;
   -- uses Warnsdorff heurisitic to find a tour faster
   -- same interface as Get_Tour
```


Its implementation is as follows.


```Ada
   function Warnsdorff_Get_Tour(Start_X, Start_Y: Index;  Scene: Tour := Empty)
			       return Tour is
      Done: Boolean;
      Visited: Tour; -- see comments from Get_Tour above
      Move_Count: Natural := Count_Moves(Scene);

      function Neighbors(X, Y: Index) return Natural is
         Result: Natural := 0;
      begin
         for P in Pairs'Range loop
            if X+Pairs(P)(1) in Index and then Y+Pairs(P)(2) in Index and then
              Visited(X+Pairs(P)(1),  Y+Pairs(P)(2)) = 0 then
               Result := Result + 1;
            end if;
         end loop;
         return Result;
      end Neighbors;

      procedure Sort(Options: in out Pair_Array) is
         N_Bors: array(Options'Range) of Natural;
         K: Positive range Options'Range;
         N: Natural;
         P: Pair;
      begin
         for Opt in Options'Range loop
            N_Bors(Opt) := Neighbors(Options(Opt)(1), Options(Opt)(2));
         end loop;
         for Opt in Options'Range loop
            K := Opt;
            for Alternative in Opt+1 .. Options'Last loop
               if N_Bors(Alternative) < N_Bors(Opt) then
                  K := Alternative;
               end if;
            end loop;
            N           := N_Bors(Opt);
            N_Bors(Opt) := N_Bors(K);
            N_Bors(K)   := N;
            P            := Options(Opt);
            Options(Opt) := Options(K);
            Options(K)   := P;
         end loop;
      end Sort;

      procedure Visit(X, Y: Index; Move: Positive; Found: out Boolean) is
         Next_Count: Natural range 0 .. 8 := 0;
         Next_Steps: Pair_Array(1 .. 8);
         XX, YY: Integer;
      begin
         Found := False;
         Visited(X, Y) := Move;
         if Move = Move_Count then
            Found := True;
         else
            -- consider all possible places to go
            for P in Pairs'Range loop
               XX := X + Pairs(P)(1);
               YY := Y + Pairs(P)(2);
               if (XX in Index) and then (YY in Index)
                 and then Visited(XX, YY) = 0 then
                  Next_Count := Next_Count+1;
                  Next_Steps(Next_Count) := (XX, YY);
               end if;
            end loop;

            Sort(Next_Steps(1 .. Next_Count));

            for N in 1 .. Next_Count loop
               Visit(Next_Steps(N)(1), Next_Steps(N)(2), Move+1, Found);
               if Found then
                  return; -- no need to search further
            end if;
            end loop;

            -- if we didn't return above, we have to undo our move
            Visited(X, Y) := 0;
         end if;
      end Visit;

   begin
      Visited := Scene;
      Visit(Start_X, Start_Y, 1, Done);
      if not Done then
         Visited := Scene;
      end if;
      return Visited;
   end Warnsdorff_Get_Tour;
```


The modification for the main program is trivial:

```Ada
with Knights_Tour, Ada.Command_Line;

procedure Test_Fast is

   Size: Positive := Positive'Value(Ada.Command_Line.Argument(1));

   package KT is new Knights_Tour(Size => Size);

begin
   KT.Tour_IO(KT.Warnsdorff_Get_Tour(1, 1));
end Test_Fast;
```


This works still well for somewhat larger sizes:

```txt
>./test_fast 24
   1 108  45  52   3 112  57  60   5  62 131 144   7  64 147 170   9  66 187 192  11  68  71 190
  46  51   2 111  56  53   4 113 130  59   6  63 146 169   8  65 186 215  10  67 188 191  12  69
 107  44 109  54 123 114 129  58  61 132 145 168 143 148 185 214 171 198 225 216 193  70 189  72
  50  47 122 115 110  55 140 133 128 167 142 149 184 213 172 199 226 255 246 197 224 217 194  13
  43 106  49 124 139 134 127 166 141 150 183 212 173 200 227 254 247 242 223 256 245 196  73 218
  48 121 116 135 126 165 138 151 182 211 174 201 228 253 248 241 290 263 304 243 222 257  14 195
 105  42 125 164 137 152 181 210 175 202 229 252 249 240 289 264 329 308 291 262 303 244 219  74
 120 117 136 153 180 163 176 203 230 267 250 239 288 265 328 309 334 345 330 305 292 221 258  15
  41 104 119 160 177 204 231 268 209 238 287 266 251 310 335 344 357 332 307 346 261 302  75 220
 118 159 154 205 162 179 208 237 286 269 324 311 336 327 438 333 418 347 356 331 306 293  16 259
 103  40 161 178 207 232 285 270 323 312 337 326 483 416 343 422 437 358 419 298 349 260 301  76
 158 155 206 233 284 271 236 313 338 325 482 415 342 439 484 417 420 423 348 355 360 299 294  17
  39 102 157 272 235 314 339 322 481 414 341 492 497 514 421 440 485 436 359 424 297 350  77 300
 156 273 234 315 276 283 478 413 340 493 480 513 530 491 498 515 452 441 454 435 354 361  18 295
 101  38 275 282 397 412 321 494 479 512 557 496 543 534 529 490 499 486 451 442 425 296 351  78
 274 279 316 277 320 477 410 511 570 495 554 535 556 531 542 533 516 453 444 455 434 353 362  19
  37 100 281 398 411 396 575 476 567 558 561 544 553 536 521 528 489 500 487 450 443 426  79 352
 280 317 278 319 402 409 510 569 560 571 566 555 550 541 532 537 522 517 460 445 456 433  20 363
  99  36 389 378 399 576 395 574 475 568 559 562 545 552 525 520 527 488 501 462 449 364 427  80
  94 379 318 401 388 403 408 509 572 565 474 551 540 549 538 523 518 461 446 459 432 457 366  21
  35  98  93 390 377 400 573 394 375 508 563 546 373 524 519 526 371 502 463 466 365 448  81 428
 380  95 382 385 404 387 376 407 564 473 374 507 548 539 372 503 464 467 370 447 458 431  22 367
 383  34  97  92 391  32 405  90 393  30 547  88 471  28 505  86 469  26 465  84 369  24 429  82
  96 381 384  33 386  91 392  31 406  89 472  29 506  87 470  27 504  85 468  25 430  83 368  23
```



## ALGOL 68

```algol68
# Non-recursive Knight's Tour with Warnsdorff's algorithm                #
# If there are multiple choices, backtrack if the first choice doesn't   #
# find a solution                                                        #

# the size of the board                                                  #
INT board size = 8;


# directions for moves #
INT nne = 1, nee = 2, see = 3, sse = 4, ssw = 5, sww = 6, nww = 7, nnw = 8;

INT lowest move  = nne;
INT highest move = nnw;

# the vertical position changes of the moves                             #
#                  nne, nee, see, sse, ssw, sww, nww, nnw                #
[]INT offset v = (  -2,  -1,   1,   2,   2,   1,  -1,  -2 );
# the horizontal position changes of the moves                           #
#                  nne, nee, see, sse, ssw, sww, nww, nnw                #
[]INT offset h = (   1,   2,   2,   1,  -1,  -2,  -2,  -1 );


MODE SQUARE = STRUCT( INT move      # the number of the move that caused #
                                    # the knight to reach this square    #
                    , INT direction # the direction of the move that     #
                                    # brought the knight here - one of   #
                                    # nne, nee, see, sse, ssw, sww, nww  #
                                    # or nnw - used for backtracking     #
                                    # zero for the first move            #
                    );

# the board #
[ board size, board size ]SQUARE board;

# initialises the board so there are no used squares #
PROC initialise board = VOID:
    FOR row FROM 1 LWB board TO 1 UPB board
    DO
        FOR col FROM 2 LWB board TO 2 UPB board
        DO
            board[ row, col ] := ( 0, 0 )
        OD
    OD; # initialise board #


INT iterations := 0;
INT backtracks := 0;

# prints the board #
PROC print tour = VOID:
BEGIN

    print( ( "       a   b   c   d   e   f   g   h", newline ) );
    print( ( "   +--------------------------------", newline ) );

    FOR row FROM 1 UPB board BY -1 TO 1 LWB board
    DO
        print( ( whole( row, -3 ) ) );
        print( ( "|" ) );

        FOR col FROM 2 LWB board TO 2 UPB board
        DO
            print( ( " " ) );
            print( ( whole( move OF board[ row, col ], -3 ) ) )
        OD;
        print( ( newline ) )
    OD

END; # print tour #


# determines whether a move to the specified row and column is possible #
PROC can move to = ( INT row, INT col )BOOL:
    IF row > 1 UPB board
    OR row < 1 LWB board
    OR col > 2 UPB board
    OR col < 2 LWB board
    THEN
        # the position is not on the board                              #
        FALSE
    ELSE
        # the move is legal, check the square is unoccupied             #
        move OF board[ row, col ] = 0
    FI;


# used to hold counts of the number of moves that could be made in each #
# direction from the current square                                     #
[ lowest move : highest move ]INT possible move count;


# sets the elements of possible move count to the number of moves that  #
# could be made in each direction from the specified row and col        #
PROC count moves in each direction from = ( INT row, INT col )VOID:
    FOR move direction FROM lowest move TO highest move
    DO

        INT new row = row + offset v[ move direction ];
        INT new col = col + offset h[ move direction ];

        IF NOT can move to( new row, new col )
        THEN
            # can't move to this square #
            possible move count[ move direction ] := -1
        ELSE
            # a move in this direction is possible #
            # - count the number of moves that could be made from it #

            possible move count[ move direction ] := 0;

            FOR subsequent move FROM lowest move TO highest move
            DO
                IF can move to( new row + offset v[ subsequent move ]
                              , new col + offset h[ subsequent move ]
                              )
                THEN
                    # have a possible subsequent move #
                    possible move count[ move direction ] +:= 1
                FI
            OD
        FI

    OD;



# update the board to the first knight's tour found starting from       #
# "start row" and "start col".                                          #
# return TRUE if one was found, FALSE otherwise                         #
PROC find tour = ( INT start row, INT start col )BOOL:
BEGIN

    initialise board;

    BOOL result := TRUE;

    INT  move number  := 1;
    INT  row          := start row;
    INT  col          := start col;

    # the tour will be complete when we have made as many moves            #
    # as there squares on the board                                        #
    INT  final move    = ( ( ( 1 UPB board ) + 1 ) - 1 LWB board )
                       * ( ( ( 2 UPB board ) + 1 ) - 2 LWB board )
                       ;

    # the first move is to place the knight on the starting square         #
    board[ row, col ]  := ( move number, lowest move - 1 );
    # start off with an unknown direction for the best move                #
    INT best direction := lowest move - 1;

    # attempt to find a sequence of moves that will reach each square once #
    WHILE
        move number < final move AND result
    DO

        iterations +:= 1;

        # count the number of moves possible from each possible move       #
        # from this square                                                 #
        count moves in each direction from( row, col );

        # find the direction with the lowest number of subsequent moves    #

        IF best direction < lowest move
        THEN
            # must find the best direction to move in                      #

            INT lowest move count := highest move + 1;

            FOR move direction FROM lowest move TO highest move
            DO
                IF  possible move count[ move direction ] >= 0
                AND possible move count[ move direction ] <  lowest move count
                THEN
                    # have a move with fewer possible subsequent moves     #
                    best direction    := move direction;
                    lowest move count := possible move count[ move direction ]
                FI
            OD

        ELSE
            # following a backtrack - find an alternative with the same    #
            # lowest number of possible moves - if there are any           #
            # if there aren't, we will backtrack again                     #

            INT lowest move count := possible move count[ best direction ];

            WHILE
                best direction +:= 1;
                IF best direction > highest move
                THEN
                    # no more possible moves with the lowest number of     #
                    # subsequent moves                                     #
                    FALSE
                ELSE
                    # keep looking if the number of moves from this square #
                    # isn't the lowest                                     #
                    possible move count[ best direction ] /= lowest move count
                FI
            DO
                SKIP
            OD

        FI;

        IF best direction  <= highest move
        AND best direction >= lowest move
        THEN
            # we found a best possible move #

            INT new row = row + offset v[ best direction ];
            INT new col = col + offset h[ best direction ];

            row               := new row;
            col               := new col;
            move number      +:= 1;
            board[ row, col ] := ( move number, best direction );

            best direction    := lowest move - 1

        ELSE
            # no more moves from this position - backtrack #

            IF move number = 1
            THEN
                # at the starting position - no solution #
                result := FALSE

            ELSE
                # not at the starting position - undo the latest move #

                backtracks  +:= 1;

                move number -:= 1;

                INT curr row := row;
                INT curr col := col;

                best direction := direction OF board[ curr row, curr col ];

                row -:= offset v[ best direction ];
                col -:= offset h[ best direction ];

                # reset the square we just backtracked from #
                board[ curr row, curr col ] := ( 0, 0 )

            FI

        FI

    OD;

    result
END; # find tour #


main:(

    # get the starting position #

    CHAR  row;
    CHAR  col;

    WHILE
        print( ( "Enter starting row(1-8) and col(a-h): " ) );
        read ( ( row, col, newline ) );
        row < "1" OR row > "8" OR col < "a" OR col > "h"
    DO
        SKIP
    OD;

    # calculate the tour from that position, if possible #

    IF find tour( ABS row - ABS "0", ( ABS col - ABS "a" ) + 1 )
    THEN
        # found a solution #
        print tour
    ELSE
        # couldn't find a solution #
        print( ( "Solution not found - iterations: ", iterations
               , ", backtracks: ", backtracks
               , newline
               )
             )
    FI

)
```

```txt

Enter starting row(1-8) and col(a-h): 5d
       a   b   c   d   e   f   g   h
   +--------------------------------
  8|  51  18  53  20  41  44   3   6
  7|  54  21  50  45   2   5  40  43
  6|  17  52  19  58  49  42   7   4
  5|  22  55  64   1  46  57  48  39
  4|  33  16  23  56  59  38  29   8
  3|  24  13  34  63  30  47  60  37
  2|  15  32  11  26  35  62   9  28
  1|  12  25  14  31  10  27  36  61

```


## ANSI Standard BASIC

[[File:Knights_Tour.gif|right]]

ANSI BASIC doesn't allow function parameters to be passed by reference so X and Y were made global variables.


```ANSI Standard BASIC
100 DECLARE EXTERNAL FUNCTION choosemove
110 !
120 RANDOMIZE
130 PUBLIC NUMERIC X, Y, TRUE, FALSE
140 LET TRUE = -1
150 LET FALSE = 0
160 !
170 SET WINDOW 1,512,1,512
180 SET AREA COLOR "black"
190 FOR x=0 TO 512-128 STEP 128
200    FOR y=0 TO 512-128 STEP 128
210       PLOT AREA:x+64,y;x+128,y;x+128,y+64;x+64,y+64
220       PLOT AREA:x,y+64;x+64,y+64;x+64,y+128;x,y+128
230    NEXT y
240 NEXT x
250 !
260 SET LINE COLOR "red"
270 SET LINE WIDTH 6
280 !
290 PUBLIC NUMERIC Board(0 TO 7,0 TO 7)
300 LET X = 0
310 LET Y = 0
320 LET Total = 0
330 DO
340    LET Board(X,Y) = TRUE
350    PLOT LINES: X*64+32,Y*64+32;
360    LET Total = Total + 1
370 LOOP UNTIL choosemove(X, Y) = FALSE
380 IF Total <> 64 THEN STOP
390 END
400 !
410 EXTERNAL FUNCTION choosemove(X1, Y1)
420 DECLARE EXTERNAL SUB trymove
430 LET M = 9
440 CALL trymove(X1+1, Y1+2, M, newx, newy)
450 CALL trymove(X1+1, Y1-2, M, newx, newy)
460 CALL trymove(X1-1, Y1+2, M, newx, newy)
470 CALL trymove(X1-1, Y1-2, M, newx, newy)
480 CALL trymove(X1+2, Y1+1, M, newx, newy)
490 CALL trymove(X1+2, Y1-1, M, newx, newy)
500 CALL trymove(X1-2, Y1+1, M, newx, newy)
510 CALL trymove(X1-2, Y1-1, M, newx, newy)
520 IF M=9 THEN
530    LET choosemove = FALSE
540    EXIT FUNCTION
550 END IF
560 LET X = newx
570 LET Y = newy
580 LET choosemove = TRUE
590 END FUNCTION
600 !
610 EXTERNAL SUB trymove(X, Y, M, newx, newy)
620 !
630 DECLARE EXTERNAL FUNCTION validmove
640 IF validmove(X,Y) = 0 THEN EXIT SUB
650 IF validmove(X+1,Y+2) <> 0 THEN LET N = N + 1
660 IF validmove(X+1,Y-2) <> 0 THEN LET N = N + 1
670 IF validmove(X-1,Y+2) <> 0 THEN LET N = N + 1
680 IF validmove(X-1,Y-2) <> 0 THEN LET N = N + 1
690 IF validmove(X+2,Y+1) <> 0 THEN LET N = N + 1
700 IF validmove(X+2,Y-1) <> 0 THEN LET N = N + 1
710 IF validmove(X-2,Y+1) <> 0 THEN LET N = N + 1
720 IF validmove(X-2,Y-1) <> 0 THEN LET N = N + 1
730 IF N>M THEN EXIT SUB
740 IF N=M AND RND<.5 THEN EXIT SUB
750 LET M = N
760 LET newx = X
770 LET newy = Y
780 END SUB
790 !
800 EXTERNAL FUNCTION validmove(X,Y)
810 LET validmove = FALSE
820 IF X<0 OR X>7 OR Y<0 OR Y>7 THEN EXIT FUNCTION
830 IF Board(X,Y)=FALSE THEN LET validmove = TRUE
840 END FUNCTION
```



## AutoHotkey

```AutoHotkey
#SingleInstance, Force
#NoEnv
SetBatchLines, -1
; Uncomment if Gdip.ahk is not in your standard library
;#Include, Gdip.ahk
If !pToken := Gdip_Startup(){
   MsgBox, 48, Gdiplus error!, Gdiplus failed to start. Please ensure you have Gdiplus on your system.
   ExitApp
}
; I've added a simple new function here, just to ensure if anyone is having any problems then to make sure they are using the correct library version
if (Gdip_LibraryVersion() < 1.30)
{
	MsgBox, 48, Version error!, Please download the latest version of the gdi+ library
	ExitApp
}
OnExit, Exit
tour := "a1 b3 d2 c4 a5 b7 d8 e6 d4 b5 c7 a8 b6 c8 a7 c6 b8 a6 b4 d5 e3 d1 b2 a4 c5 d7 f8 h7 f6 g8 h6 f7 h8 g6 e7 f5 h4 g2 e1 d3 e5 g4 f2 h1 g3 f1 h2 f3 g1 h3 g5 e4 d6 e8 g7 h5 f4 e2 c1 a2 c3 b1 a3 c2 "
; Knight's tour with maximum symmetry by George Jelliss, http://www.mayhematics.com/t/8f.htm
; I know, I know, but I followed the task outline to the letter! Besides, this path is the prettiest.

; Input: starting square
InputBox, start, Knight's Tour Start, Enter Knight's starting location in algebraic notation:, , , , , , , , b3
i := InStr(tour, start)
If i=0
{
	Msgbox Error, please try again.
	Reload
}
; Output: move sequence
Msgbox % tour := SubStr(tour, i) . SubStr(tour, 1, i-1)

; Animation
tour .= SubStr(tour, 1, 3)
, CellSize := 30 ; pixels
, Width := Height := 9*CellSize
, TopLeftX := (A_ScreenWidth - Width) // 2
, TopLeftY := (A_ScreenHeight - Height) // 2
Gui, -Caption +E0x80000 +LastFound +AlwaysOnTop +ToolWindow +OwnDialogs
Gui, Show, NA ; show board (currently transparent)
hwnd1 := WinExist() ; required for Gdip
OnMessage(0x201, "WM_LBUTTONDOWN")
, hbm := CreateDIBSection(Width, Height)
, hdc := CreateCompatibleDC()
, obm := SelectObject(hdc, hbm)
, G := Gdip_GraphicsFromHDC(hdc)
, Gdip_SetSmoothingMode(G, 4)

Loop 1 ; remove '1' and uncomment next line to loop infinitely
{
;Gdip_GraphicsClear(G) ; uncomment to loop infinitely
cOdd := "0xFFFFCE9E" ; create brushes
, cEven := "0xFFD18B47"
, pBrushOdd := Gdip_BrushCreateSolid(cOdd)
, pBrushEven := Gdip_BrushCreateSolid(cEven)

Loop 64 ; layout board
{
	Row := mod(A_Index-1,8)+1
	, Col := (A_Index-1)//8+1
	, Gdip_FillRectangle(G, mod(Row+Col,2) ? pBrushOdd : pBrushEven, Col * CellSize + 1, Row * CellSize + 1, CellSize - 2, CellSize - 2)
}
Gdip_DeleteBrush(pBrushOdd) ; cleanup memory
, Gdip_DeleteBrush(pBrushEven)
, UpdateLayeredWindow(hwnd1, hdc, TopLeftX, TopLeftY, Width, Height) ; update board

, pPen := Gdip_CreatePen(0x66FF0000, CellSize/10) ; create pen
, Algebraic := SubStr(tour,1,2) ; get starting coordinates
, x := (Asc(SubStr(Algebraic, 1, 1))-96+0.5)*CellSize
, y := (9.5-SubStr(Algebraic, 2, 1))*CellSize

Loop 64 ; trace path
{
	Sleep, 0.5*1000
	xold := x, yold := y ; a line has start and end points
	, Algebraic := SubStr(tour,(A_Index)*3+1,2) ; get new coordinates
	, x := (Asc(SubStr(Algebraic, 1, 1))-96+0.5)*CellSize
	, y := (9.5-SubStr(Algebraic, 2, 1))*CellSize
	, Gdip_DrawLine(G, pPen, xold, yold, x, y)
	, UpdateLayeredWindow(hwnd1, hdc, TopLeftX, TopLeftY, Width, Height) ; update board
}
Gdip_DeletePen(pPen)
}
Return

GuiEscape:
	ExitApp

Exit:
	Gdip_Shutdown(pToken)
	ExitApp

WM_LBUTTONDOWN()
{
	If (A_Gui = 1)
	PostMessage, 0xA1, 2
}
```

For start at b3

```txt
b3 d2 c4 a5 b7 d8 e6 d4 b5 c7 a8 b6 c8 a7 c6 b8 a6 b4 d5 e3 d1 b2 a4 c5 d7 f8 h7 f6 g8 h6 f7 h8 g6 e7 f5 h4 g2 e1 d3 e5 g4 f2 h1 g3 f1 h2 f3 g1 h3 g5 e4 d6 e8 g7 h5 f4 e2 c1 a2 c3 b1 a3 c2 a1
```

... plus an animation.


## AWK


```AWK

# syntax: GAWK -f KNIGHTS_TOUR.AWK [-v sr=x] [-v sc=x]
#
# examples:
#   GAWK -f KNIGHTS_TOUR.AWK                   (default)
#   GAWK -f KNIGHTS_TOUR.AWK -v sr=1 -v sc=1   start at top left (default)
#   GAWK -f KNIGHTS_TOUR.AWK -v sr=1 -v sc=8   start at top right
#   GAWK -f KNIGHTS_TOUR.AWK -v sr=8 -v sc=8   start at bottom right
#   GAWK -f KNIGHTS_TOUR.AWK -v sr=8 -v sc=1   start at bottom left
#
BEGIN {
    N = 8 # board size
    if (sr == "") { sr = 1 } # starting row
    if (sc == "") { sc = 1 } # starting column
    split("2  2 -2 -2 1  1 -1 -1",X," ")
    split("1 -1  1 -1 2 -2  2 -2",Y," ")
    printf("\n%dx%d board: starting row=%d col=%d\n",N,N,sr,sc)
    move(sr,sc,0)
    exit(1)
}
function move(x,y,m) {
    if (cantMove(x,y)) {
      return(0)
    }
    P[x,y] = ++m
    if (m == N ^ 2) {
      printBoard()
      exit(0)
    }
    tryBestMove(x,y,m)
}
function cantMove(x,y) {
    return( P[x,y] || x<1 || x>N || y<1 || y>N )
}
function tryBestMove(x,y,m,  i) {
    i = bestMove(x,y)
    move(x+X[i],y+Y[i],m)
}
function bestMove(x,y,  arg1,arg2,c,i,min,out) {
# Warnsdorff's rule: go to where there are fewest next moves
    min = N ^ 2 + 1
    for (i in X) {
      arg1 = x + X[i]
      arg2 = y + Y[i]
      if (!cantMove(arg1,arg2)) {
        c = countNext(arg1,arg2)
        if (c < min) {
          min = c
          out = i
        }
      }
    }
    return(out)
}
function countNext(x,y,  i,out) {
    for (i in X) {
      out += (!cantMove(x+X[i],y+Y[i]))
    }
    return(out)
}
function printBoard(  i,j,leng) {
    leng = length(N*N)
    for (i=1; i<=N; i++) {
      for (j=1; j<=N; j++) {
        printf(" %*d",leng,P[i,j])
      }
      printf("\n")
    }
}

```

<p>output:</p>

```txt

8x8 board: starting row=1 col=1
  1 50 15 32 61 28 13 30
 16 33 64 55 14 31 60 27
 51  2 49 44 57 62 29 12
 34 17 56 63 54 47 26 59
  3 52 45 48 43 58 11 40
 18 35 20 53 46 41  8 25
 21  4 37 42 23  6 39 10
 36 19 22  5 38  9 24  7

```



## BBC BASIC

[[Image:knights_tour_bbc.gif|right]]

```bbcbasic
      VDU 23,22,256;256;16,16,16,128
      VDU 23,23,4;0;0;0;
      OFF
      GCOL 4,15
      FOR x% = 0 TO 512-128 STEP 128
        RECTANGLE FILL x%,0,64,512
      NEXT
      FOR y% = 0 TO 512-128 STEP 128
        RECTANGLE FILL 0,y%,512,64
      NEXT
      GCOL 9

      DIM Board%(7,7)
      X% = 0
      Y% = 0
      Total% = 0
      REPEAT
        Board%(X%,Y%) = TRUE
        IF Total% DRAW X%*64+32,Y%*64+32 ELSE MOVE X%*64+32,Y%*64+32
        Total% += 1
      UNTIL NOT FNchoosemove(X%, Y%)
      IF Total%<>64 STOP
      REPEAT WAIT 1 : UNTIL FALSE
      END

      DEF FNchoosemove(RETURN X%, RETURN Y%)
      LOCAL M%, newx%, newy%
      M% = 9
      PROCtrymove(X%+1, Y%+2, M%, newx%, newy%)
      PROCtrymove(X%+1, Y%-2, M%, newx%, newy%)
      PROCtrymove(X%-1, Y%+2, M%, newx%, newy%)
      PROCtrymove(X%-1, Y%-2, M%, newx%, newy%)
      PROCtrymove(X%+2, Y%+1, M%, newx%, newy%)
      PROCtrymove(X%+2, Y%-1, M%, newx%, newy%)
      PROCtrymove(X%-2, Y%+1, M%, newx%, newy%)
      PROCtrymove(X%-2, Y%-1, M%, newx%, newy%)
      IF M%=9 THEN = FALSE
      X% = newx% : Y% = newy%
      = TRUE

      DEF PROCtrymove(X%, Y%, RETURN M%, RETURN newx%, RETURN newy%)
      LOCAL N%
      IF NOT FNvalidmove(X%,Y%) THEN ENDPROC
      IF FNvalidmove(X%+1,Y%+2) N% += 1
      IF FNvalidmove(X%+1,Y%-2) N% += 1
      IF FNvalidmove(X%-1,Y%+2) N% += 1
      IF FNvalidmove(X%-1,Y%-2) N% += 1
      IF FNvalidmove(X%+2,Y%+1) N% += 1
      IF FNvalidmove(X%+2,Y%-1) N% += 1
      IF FNvalidmove(X%-2,Y%+1) N% += 1
      IF FNvalidmove(X%-2,Y%-1) N% += 1
      IF N%>M% THEN ENDPROC
      IF N%=M% IF RND(2)=1 THEN ENDPROC
      M% = N%
      newx% = X% : newy% = Y%
      ENDPROC

      DEF FNvalidmove(X%,Y%)
      IF X%<0 OR X%>7 OR Y%<0 OR Y%>7 THEN = FALSE
      = NOT(Board%(X%,Y%))
```



## Bracmat


```bracmat
  ( knightsTour
  =     validmoves WarnsdorffSort algebraicNotation init solve
      , x y fieldsToVisit
    .   ~
      |   ( validmoves
          =   x y jumps moves
            .   !arg:(?x.?y)
              & :?moves
              & ( jumps
                =   dx dy Fs fxs fys fx fy
                  .   !arg:(?dx.?dy)
                    & 1 -1:?Fs
                    & !Fs:?fxs
                    &   whl
                      ' ( !fxs:%?fx ?fxs
                        & !Fs:?fys
                        &   whl
                          ' ( !fys:%?fy ?fys
                            &     (   (!x+!fx*!dx.!y+!fy*!dy)
                                    : (>0:<9.>0:<9)
                                  |
                                  )
                                  !moves
                              : ?moves
                            )
                        )
                )
              & jumps$(1.2)
              & jumps$(2.1)
              & !moves
          )
        & ( init
          =   fields x y
            .   :?fields
              & 0:?x
              &   whl
                ' ( 1+!x:<9:?x
                  & 0:?y
                  &   whl
                    ' ( 1+!y:<9:?y
                      & (!x.!y) !fields:?fields
                      )
                  )
              & !fields
          )
        & init$:?fieldsToVisit
        & ( WarnsdorffSort
          =   sum moves elm weightedTerms
            .   ( weightedTerms
                =   pos alts fieldsToVisit moves move weight
                  .     !arg:(%?pos ?alts.?fieldsToVisit)
                      &   (   !fieldsToVisit:!pos
                            & (0.!pos)
                          |   !fieldsToVisit:? !pos ?
                            & validmoves$!pos:?moves
                            & 0:?weight
                            &   whl
                              ' ( !moves:%?move ?moves
                                & (   !fieldsToVisit:? !move ?
                                    & !weight+1:?weight
                                  |
                                  )
                                )
                            & (!weight.!pos)
                          | 0
                          )
                        + weightedTerms$(!alts.!fieldsToVisit)
                    | 0
                )
              & weightedTerms$!arg:?sum
              & :?moves
              &   whl
                ' ( !sum:(#.?elm)+?sum
                  & !moves !elm:?moves
                  )
              & !moves
          )
        & ( solve
          =   pos alts fieldsToVisit A Z tailOfSolution
            .   !arg:(%?pos ?alts.?fieldsToVisit)
              &   (   !fieldsToVisit:?A !pos ?Z
                    & ( !A !Z:&
                      |   solve
                        $ ( WarnsdorffSort$(validmoves$!pos.!A !Z)
                          . !A !Z
                          )
                      )
                  | solve$(!alts.!fieldsToVisit)
                  )
                : ?tailOfSolution
              & !pos !tailOfSolution
          )
        & ( algebraicNotation
          =   x y
            .     !arg:(?x.?y) ?arg
                &   str$(chr$(asc$a+!x+-1) !y " ")
                    algebraicNotation$!arg
              |
          )
        & @(!arg:?x #?y)
        & asc$!x+-1*asc$a+1:?x
        &   str
          $ (algebraicNotation$(solve$((!x.!y).!fieldsToVisit)))
  )
& out$(knightsTour$a1);
```



```txt
a1 b3 a5 b7 d8 f7 h8 g6 f8 h7 g5 h3 g1 e2 c1 a2 b4 a6 b8 c6 a7 c8 e7 g8 h6 g4 h2 f1 d2 b1 a3 c2 e1 f3 h4 g2 e3 d1 b2 a4 c3 b5 d4 f5 d6 c4 e5 d3 f2 h1 g3 e4 c5 d7 b6 a8 c7 d5 f4 e6 g7 e8 f6 h5
```



## C

For an animated version using OpenGL, see [[Knight's tour/C]].

The following draws on console the progress of the horsie.  Specify board size on commandline, or use default 8.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

typedef unsigned char cell;
int dx[] = { -2, -2, -1, 1, 2,  2,  1, -1 };
int dy[] = { -1,  1,  2, 2, 1, -1, -2, -2 };

void init_board(int w, int h, cell **a, cell **b)
{
	int i, j, k, x, y, p = w + 4, q = h + 4;
	/* b is board; a is board with 2 rows padded at each side */
	a[0] = (cell*)(a + q);
	b[0] = a[0] + 2;

	for (i = 1; i < q; i++) {
		a[i] = a[i-1] + p;
		b[i] = a[i] + 2;
	}

	memset(a[0], 255, p * q);
	for (i = 0; i < h; i++) {
		for (j = 0; j < w; j++) {
			for (k = 0; k < 8; k++) {
				x = j + dx[k], y = i + dy[k];
				if (b[i+2][j] == 255) b[i+2][j] = 0;
				b[i+2][j] += x >= 0 && x < w && y >= 0 && y < h;
			}
		}
	}
}

#define E "\033["
int walk_board(int w, int h, int x, int y, cell **b)
{
	int i, nx, ny, least;
	int steps = 0;
	printf(E"H"E"J"E"%d;%dH"E"32m[]"E"m", y + 1, 1 + 2 * x);

	while (1) {
		/* occupy cell */
		b[y][x] = 255;

		/* reduce all neighbors' neighbor count */
		for (i = 0; i < 8; i++)
			b[ y + dy[i] ][ x + dx[i] ]--;

		/* find neighbor with lowest neighbor count */
		least = 255;
		for (i = 0; i < 8; i++) {
			if (b[ y + dy[i] ][ x + dx[i] ] < least) {
				nx = x + dx[i];
				ny = y + dy[i];
				least = b[ny][nx];
			}
		}

		if (least > 7) {
			printf(E"%dH", h + 2);
			return steps == w * h - 1;
		}

		if (steps++) printf(E"%d;%dH[]", y + 1, 1 + 2 * x);
		x = nx, y = ny;
		printf(E"%d;%dH"E"31m[]"E"m", y + 1, 1 + 2 * x);
		fflush(stdout);
		usleep(120000);
	}
}

int solve(int w, int h)
{
	int x = 0, y = 0;
	cell **a, **b;
	a = malloc((w + 4) * (h + 4) + sizeof(cell*) * (h + 4));
	b = malloc((h + 4) * sizeof(cell*));

	while (1) {
		init_board(w, h, a, b);
		if (walk_board(w, h, x, y, b + 2)) {
			printf("Success!\n");
			return 1;
		}
		if (++x >= w) x = 0, y++;
		if (y >= h) {
			printf("Failed to find a solution\n");
			return 0;
		}
		printf("Any key to try next start position");
		getchar();
	}
}

int main(int c, char **v)
{
	int w, h;
	if (c < 2 || (w = atoi(v[1])) <= 0) w = 8;
	if (c < 3 || (h = atoi(v[2])) <= 0) h = w;
	solve(w, h);

	return 0;
}
```



## C++

Uses Warnsdorff's rule and (iterative) backtracking if that fails.


```cpp
#include <iostream>
#include <iomanip>
#include <array>
#include <string>
#include <tuple>
#include <algorithm>
using namespace std;

template<int N = 8>
class Board
{
public:
    array<pair<int, int>, 8> moves;
    array<array<int, N>, N> data;

    Board()
    {
        moves[0] = make_pair(2, 1);
        moves[1] = make_pair(1, 2);
        moves[2] = make_pair(-1, 2);
        moves[3] = make_pair(-2, 1);
        moves[4] = make_pair(-2, -1);
        moves[5] = make_pair(-1, -2);
        moves[6] = make_pair(1, -2);
        moves[7] = make_pair(2, -1);
    }

    array<int, 8> sortMoves(int x, int y) const
    {
        array<tuple<int, int>, 8> counts;
        for(int i = 0; i < 8; ++i)
        {
            int dx = get<0>(moves[i]);
            int dy = get<1>(moves[i]);

            int c = 0;
            for(int j = 0; j < 8; ++j)
            {
                int x2 = x + dx + get<0>(moves[j]);
                int y2 = y + dy + get<1>(moves[j]);

                if (x2 < 0 || x2 >= N || y2 < 0 || y2 >= N)
                    continue;
                if(data[y2][x2] != 0)
                    continue;

                c++;
            }

            counts[i] = make_tuple(c, i);
        }

        // Shuffle to randomly break ties
        random_shuffle(counts.begin(), counts.end());

        // Lexicographic sort
        sort(counts.begin(), counts.end());

        array<int, 8> out;
        for(int i = 0; i < 8; ++i)
            out[i] = get<1>(counts[i]);
        return out;
    }

    void solve(string start)
    {
        for(int v = 0; v < N; ++v)
            for(int u = 0; u < N; ++u)
                data[v][u] = 0;

        int x0 = start[0] - 'a';
        int y0 = N - (start[1] - '0');
        data[y0][x0] = 1;

        array<tuple<int, int, int, array<int, 8>>, N*N> order;
        order[0] = make_tuple(x0, y0, 0, sortMoves(x0, y0));

        int n = 0;
        while(n < N*N-1)
        {
            int x = get<0>(order[n]);
            int y = get<1>(order[n]);

            bool ok = false;
            for(int i = get<2>(order[n]); i < 8; ++i)
            {
                int dx = moves[get<3>(order[n])[i]].first;
                int dy = moves[get<3>(order[n])[i]].second;

                if(x+dx < 0 || x+dx >= N || y+dy < 0 || y+dy >= N)
                    continue;
                if(data[y + dy][x + dx] != 0)
                    continue;

                ++n;
                get<2>(order[n]) = i + 1;
                data[y+dy][x+dx] = n + 1;
                order[n] = make_tuple(x+dx, y+dy, 0, sortMoves(x+dx, y+dy));
                ok = true;
                break;
            }

            if(!ok) // Failed. Backtrack.
            {
                data[y][x] = 0;
                --n;
            }
        }
    }

    template<int N>
    friend ostream& operator<<(ostream &out, const Board<N> &b);
};

template<int N>
ostream& operator<<(ostream &out, const Board<N> &b)
{
    for (int v = 0; v < N; ++v)
    {
        for (int u = 0; u < N; ++u)
        {
            if (u != 0) out << ",";
            out << setw(3) << b.data[v][u];
        }
        out << endl;
    }
    return out;
}

int main()
{
    Board<5> b1;
    b1.solve("c3");
    cout << b1 << endl;

    Board<8> b2;
    b2.solve("b5");
    cout << b2 << endl;

    Board<31> b3; // Max size for <1000 squares
    b3.solve("a1");
    cout << b3 << endl;
    return 0;
}
```


Output:

```txt

 23, 16, 11,  6, 21
 10,  5, 22, 17, 12
 15, 24,  1, 20,  7
  4,  9, 18, 13,  2
 25, 14,  3,  8, 19

 63, 20,  3, 24, 59, 36,  5, 26
  2, 23, 64, 37,  4, 25, 58, 35
 19, 62, 21, 50, 55, 60, 27,  6
 22,  1, 54, 61, 38, 45, 34, 57
 53, 18, 49, 44, 51, 56,  7, 28
 12, 15, 52, 39, 46, 31, 42, 33
 17, 48, 13, 10, 43, 40, 29,  8
 14, 11, 16, 47, 30,  9, 32, 41

275,112, 19,116,277,604, 21,118,823,770, 23,120,961,940, 25,122,943,926, 27,124,917,898, 29,126,911,872, 31,128,197,870, 33
 18,115,276,601, 20,117,772,767, 22,119,958,851, 24,121,954,941, 26,123,936,925, 28,125,912,899, 30,127,910,871, 32,129,198
111,274,113,278,605,760,603,822,771,824,769,948,957,960,939,944,953,942,927,916,929,918,897,908,913,900,873,196,875, 34,869
114, 17,600,273,602,775,766,773,768,949,850,959,852,947,952,955,932,937,930,935,924,915,920,905,894,909,882,901,868,199,130
271,110,279,606,759,610,761,776,821,764,825,816,951,956,853,938,945,934,923,928,919,896,893,914,907,904,867,874,195,876, 35
 16,581,272,599,280,607,774,765,762,779,950,849,826,815,946,933,854,931,844,857,890,921,906,895,886,883,902,881,200,131,194
109,270,281,580,609,758,611,744,777,820,763,780,817,848,827,808,811,846,855,922,843,858,889,892,903,866,885,192,877, 36,201
282, 15,582,269,598,579,608,757,688,745,778,819,754,783,814,847,828,807,810,845,856,891,842,859,884,887,880,863,202,193,132
267,108,283,578,583,612,689,614,743,756,691,746,781,818,753,784,809,812,829,806,801,840,835,888,865,862,203,878,191,530, 37
 14,569,268,585,284,597,576,619,690,687,742,755,692,747,782,813,752,785,802,793,830,805,860,841,836,879,864,529,204,133,190
107,266,285,570,577,584,613,686,615,620,695,684,741,732,711,748,739,794,751,786,803,800,839,834,861,528,837,188,531, 38,205
286, 13,568,265,586,575,596,591,618,685,616,655,696,693,740,733,712,749,738,795,792,831,804,799,838,833,722,527,206,189,134
263,106,287,508,571,590,587,574,621,592,639,694,683,656,731,710,715,734,787,750,737,796,791,832,721,798,207,532,187,474, 39
 12,417,264,567,288,509,572,595,588,617,654,657,640,697,680,713,730,709,716,735,788,727,720,797,790,723,526,473,208,135,186
105,262,289,416,507,566,589,512,573,622,593,638,653,682,659,698,679,714,729,708,717,736,789,726,719,472,533,184,475, 40,209
290, 11,418,261,502,415,510,565,594,513,562,641,658,637,652,681,660,699,678,669,728,707,718,675,724,525,704,471,210,185,136
259,104,291,414,419,506,503,514,511,564,623,548,561,642,551,636,651,670,661,700,677,674,725,706,703,534,211,476,183,396, 41
 10,331,260,493,292,501,420,495,504,515,498,563,624,549,560,643,662,635,650,671,668,701,676,673,524,705,470,395,212,137,182
103,258,293,330,413,494,505,500,455,496,547,516,485,552,625,550,559,644,663,634,649,672,667,702,535,394,477,180,397, 42,213
294,  9,332,257,492,329,456,421,490,499,458,497,546,517,484,553,626,543,558,645,664,633,648,523,666,469,536,393,220,181,138
255,102,295,328,333,412,491,438,457,454,489,440,459,486,545,518,483,554,627,542,557,646,665,632,537,478,221,398,179,214, 43
  8,319,256,335,296,345,326,409,422,439,436,453,488,441,460,451,544,519,482,555,628,541,522,647,468,631,392,219,222,139,178
101,254,297,320,327,334,411,346,437,408,423,368,435,452,487,442,461,450,445,520,481,556,629,538,479,466,399,176,215, 44,165
298,  7,318,253,336,325,344,349,410,347,360,407,424,383,434,427,446,443,462,449,540,521,480,467,630,391,218,223,164,177,140
251,100,303,300,321,316,337,324,343,350,369,382,367,406,425,384,433,428,447,444,463,430,539,390,465,400,175,216,169,166, 45
  6,299,252,317,304,301,322,315,348,361,342,359,370,381,366,405,426,385,432,429,448,389,464,401,174,217,224,163,150,141,168
 99,250,241,302,235,248,307,338,323,314,351,362,341,358,371,380,365,404,377,386,431,402,173,388,225,160,153,170,167, 46,143
240,  5, 98,249,242,305,234,247,308,339,232,313,352,363,230,357,372,379,228,403,376,387,226,159,154,171,162,149,142,151, 82
 63,  2,239, 66, 97,236,243,306,233,246,309,340,231,312,353,364,229,356,373,378,227,158,375,172,161,148,155,152, 83,144, 47
  4, 67, 64, 61,238, 69, 96, 59,244, 71, 94, 57,310, 73, 92, 55,354, 75, 90, 53,374, 77, 88, 51,156, 79, 86, 49,146, 81, 84
  1, 62,  3, 68, 65, 60,237, 70, 95, 58,245, 72, 93, 56,311, 74, 91, 54,355, 76, 89, 52,157, 78, 87, 50,147, 80, 85, 48,145

```



## C#


```c#
using System;
using System.Collections.Generic;

namespace prog
{
	class MainClass
	{
		const int N = 8;

		readonly static int[,] moves = { {+1,-2},{+2,-1},{+2,+1},{+1,+2},
			                         {-1,+2},{-2,+1},{-2,-1},{-1,-2} };
		struct ListMoves
		{
			public int x, y;
			public ListMoves( int _x, int _y ) { x = _x; y = _y; }
		}

		public static void Main (string[] args)
		{
			int[,] board = new int[N,N];
			board.Initialize();

			int x = 0,						// starting position
			    y = 0;

			List<ListMoves> list = new List<ListMoves>(N*N);
			list.Add( new ListMoves(x,y) );

			do
			{
				if ( Move_Possible( board, x, y ) )
				{
					int move = board[x,y];
					board[x,y]++;
					x += moves[move,0];
					y += moves[move,1];
					list.Add( new ListMoves(x,y) );
				}
				else
				{
					if ( board[x,y] >= 8 )
					{
						board[x,y] = 0;
						list.RemoveAt(list.Count-1);
						if ( list.Count == 0 )
						{
							Console.WriteLine( "No solution found." );
							return;
						}
						x = list[list.Count-1].x;
						y = list[list.Count-1].y;
					}
					board[x,y]++;
				}
			}
			while( list.Count < N*N );

			int last_x = list[0].x,
			    last_y = list[0].y;
			string letters = "ABCDEFGH";
			for( int i=1; i<list.Count; i++ )
			{
				Console.WriteLine( string.Format("{0,2}:  ", i) + letters[last_x] + (last_y+1) + " - " + letters[list[i].x] + (list[i].y+1) );

				last_x = list[i].x;
				last_y = list[i].y;
			}
		}

		static bool Move_Possible( int[,] board, int cur_x, int cur_y )
		{
			if ( board[cur_x,cur_y] >= 8 )
				return false;

			int new_x = cur_x + moves[board[cur_x,cur_y],0],
			    new_y = cur_y + moves[board[cur_x,cur_y],1];

			if ( new_x >= 0 && new_x < N && new_y >= 0 && new_y < N && board[new_x,new_y] == 0 )
				return true;

			return false;
		}
	}
}
```



## CoffeeScript

This algorithm finds 100,000 distinct solutions to the 8x8 problem in about 30 seconds.  It precomputes knight moves up front, so it turns into a pure graph traversal problem.  The program uses iteration and backtracking to find solutions.

```coffeescript

graph_tours = (graph, max_num_solutions) ->
  # graph is an array of arrays
  # graph[3] = [4, 5] means nodes 4 and 5 are reachable from node 3
  #
  # Returns an array of tours (up to max_num_solutions in size), where
  # each tour is an array of nodes visited in order, and where each
  # tour visits every node in the graph exactly once.
  #
  complete_tours = []
  visited = (false for node in graph)
  dead_ends = ({} for node in graph)
  tour = [0]

  valid_neighbors = (i) ->
    arr = []
    for neighbor in graph[i]
      continue if visited[neighbor]
      continue if dead_ends[i][neighbor]
      arr.push neighbor
    arr

  next_square_to_visit = (i) ->
    arr = valid_neighbors i
    return null if arr.length == 0

    # We traverse to our neighbor who has the fewest neighbors itself.
    fewest_neighbors = valid_neighbors(arr[0]).length
    neighbor = arr[0]
    for i in [1...arr.length]
      n = valid_neighbors(arr[i]).length
      if n < fewest_neighbors
        fewest_neighbors = n
        neighbor = arr[i]
    neighbor

  while tour.length > 0
    current_square = tour[tour.length - 1]
    visited[current_square] = true
    next_square = next_square_to_visit current_square
    if next_square?
      tour.push next_square
      if tour.length == graph.length
        complete_tours.push (n for n in tour) # clone
        break if complete_tours.length == max_num_solutions
      # pessimistically call this a dead end
      dead_ends[current_square][next_square] = true
      current_square = next_square
    else
      # we backtrack
      doomed_square = tour.pop()
      dead_ends[doomed_square] = {}
      visited[doomed_square] = false
  complete_tours


knight_graph = (board_width) ->
  # Turn the Knight's Tour into a pure graph-traversal problem
  # by precomputing all the legal moves.  Returns an array of arrays,
  # where each element in any subarray is the index of a reachable node.
  index = (i, j) ->
    # index squares from 0 to n*n - 1
    board_width * i + j

  reachable_squares = (i, j) ->
    deltas = [
      [ 1,  2]
      [ 1, -2]
      [ 2,  1]
      [ 2, -1]
      [-1,  2]
      [-1, -2]
      [-2,  1]
      [-2, -1]
    ]
    neighbors = []
    for delta in deltas
      [di, dj] = delta
      ii = i + di
      jj = j + dj
      if 0 <= ii < board_width
        if 0 <= jj < board_width
          neighbors.push index(ii, jj)
    neighbors

  graph = []
  for i in [0...board_width]
    for j in [0...board_width]
      graph[index(i, j)] = reachable_squares i, j
  graph

illustrate_knights_tour = (tour, board_width) ->
  pad = (n) ->
    return " _" if !n?
    return " " + n if n < 10
    "#{n}"

  console.log "\n------"
  moves = {}
  for square, i in tour
    moves[square] = i + 1
  for i in [0...board_width]
    s = ''
    for j in [0...board_width]
      s += "  " + pad moves[i*board_width + j]
    console.log s

BOARD_WIDTH = 8
MAX_NUM_SOLUTIONS = 100000

graph = knight_graph BOARD_WIDTH
tours = graph_tours graph, MAX_NUM_SOLUTIONS
console.log "#{tours.length} tours found (showing first and last)"
illustrate_knights_tour tours[0], BOARD_WIDTH
illustrate_knights_tour tours.pop(), BOARD_WIDTH

```


output
<lang>
> time coffee knight.coffee
100000 tours found (showing first and last)

------
   1   4  57  20  47   6  49  22
  34  19   2   5  58  21  46   7
   3  56  35  60  37  48  23  50
  18  33  38  55  52  59   8  45
  39  14  53  36  61  44  51  24
  32  17  40  43  54  27  62   9
  13  42  15  30  11  64  25  28
  16  31  12  41  26  29  10  63

------
   1   4  41  20  63   6  61  22
  34  19   2   5  42  21  44   7
   3  40  35  64  37  62  23  60
  18  33  38  47  56  43   8  45
  39  14  57  36  49  46  59  24
  32  17  48  55  58  27  50   9
  13  54  15  30  11  52  25  28
  16  31  12  53  26  29  10  51

real	0m29.741s
user	0m25.656s
sys	0m0.253s

```



## Clojure

Using warnsdorff's rule

```Clojure

(defn isin? [x li]
  (not= [] (filter #(= x %) li)))

(defn options [movements pmoves n]
  (let [x (first (last movements)) y (second (last movements))
        op (vec (map #(vector (+ x (first %)) (+ y (second %))) pmoves))
        vop (filter #(and (>= (first %) 0) (>= (last %) 0)) op)
        vop1 (filter #(and (< (first %) n) (< (last %) n)) vop)]
    (vec (filter #(not (isin? % movements)) vop1))))

(defn next-move [movements pmoves n]
  (let [op (options movements pmoves n)
        sp (map #(vector % (count (options (conj movements %) pmoves n))) op)
        m (apply min (map last sp))]
    (first (rand-nth (filter #(= m (last %)) sp)))))

(defn jumps [n pos]
  (let [movements (vector pos)
        pmoves [[1 2] [1 -2] [2 1] [2 -1]
                [-1 2] [-1 -2] [-2 -1] [-2 1]]]
    (loop [mov movements x 1]
      (if (= x (* n n))
        mov
        (let [np (next-move mov pmoves n)]
          (recur (conj mov np) (inc x)))))))

```

```txt

(jumps 5 [0 0])
[[0 0] [1 2] [0 4] [2 3] [4 4] [3 2] [4 0] [2 1] [1 3] [0 1] [2 0] [4 1] [3 3] [1 4] [0 2] [1 0] [3 1] [4 3] [2 4] [0 3] [1 1] [3 0] [4 2] [3 4] [2 2]]

(jumps 8 [0 0])
[[0 0] [2 1] [4 0] [6 1] [7 3] [6 5] [7 7] [5 6] [3 7] [1 6] [0 4] [1 2] [2 0] [0 1] [1 3] [0 5] [1 7] [2 5] [0 6] [2 7] [4 6] [6 7] [7 5] [6 3] [7 1] [5 0] [3 1] [1 0] [0 2] [1 4] [3 5] [4 7] [6 6] [7 4] [6 2] [7 0] [5 1] [7 2] [6 0] [4 1] [5 3] [3 2] [4 4] [5 2] [3 3] [5 4] [4 2] [2 3] [1 1] [3 0] [2 2] [0 3] [2 4] [4 3] [6 4] [4 5] [2 6] [0 7] [1 5] [3 4] [5 5] [7 6] [5 7] [3 6]]

(let [j (jumps 40 [0 0])]        ;; are
  (and (distinct? j)             ;; all squares only once? and
       (= (count j) (* 40 40)))) ;; 40*40 squares?
true

```



## D


### Fast Version

```d
import std.stdio, std.algorithm, std.random, std.range,
       std.conv, std.typecons, std.typetuple;

int[N][N] knightTour(size_t N=8)(in string start)
in {
    assert(start.length >= 2);
} body {
    static struct P { int x, y; }

    immutable P[8] moves = [P(2,1), P(1,2), P(-1,2), P(-2,1),
                            P(-2,-1), P(-1,-2), P(1,-2), P(2,-1)];
    int[N][N] data;

    int[8] sortMoves(in int x, in int y) {
        int[2][8] counts;
        foreach (immutable i, immutable ref d1; moves) {
            int c = 0;
            foreach (immutable ref d2; moves) {
                immutable p = P(x + d1.x + d2.x, y + d1.y + d2.y);
                if (p.x >= 0 && p.x < N && p.y >= 0 && p.y < N &&
                    data[p.y][p.x] == 0)
                    c++;
            }
            counts[i] = [c, i];
        }

        counts[].randomShuffle; // Shuffle to randomly break ties.
        counts[].sort(); // Lexicographic sort.

        int[8] result = void;
        transversal(counts[], 1).copy(result[]);
        return result;
    }

    immutable p0 = P(start[0] - 'a', N - to!int(start[1 .. $]));
    data[p0.y][p0.x] = 1;

    Tuple!(int, int, int, int[8])[N * N] order;
    order[0] = tuple(p0.x, p0.y, 0, sortMoves(p0.x, p0.y));

    int n = 0;
    while (n < (N * N - 1)) {
        immutable int x = order[n][0];
        immutable int y = order[n][1];
        bool ok = false;
        foreach (immutable i; order[n][2] .. 8) {
            immutable P d = moves[order[n][3][i]];
            if (x+d.x < 0 || x+d.x >= N || y+d.y < 0 || y+d.y >= N)
                continue;

            if (data[y + d.y][x + d.x] == 0) {
                order[n][2] = i + 1;
                n++;
                data[y + d.y][x + d.x] = n + 1;
                order[n] = tuple(x+d.x,y+d.y,0,sortMoves(x+d.x,y+d.y));
                ok = true;
                break;
            }
        }

        if (!ok) { // Failed. Backtrack.
            data[y][x] = 0;
            n--;
        }
    }

    return data;
}

void main() {
    foreach (immutable i, side; TypeTuple!(5, 8, 31, 101)) {
        immutable form = "%(%" ~ text(side ^^ 2).length.text ~ "d %)";
        foreach (ref row; ["c3", "b5", "a1", "a1"][i].knightTour!side)
            writefln(form, row);
        writeln();
    }
}
```

```txt
23 16 11  6 21
10  5 22 17 12
15 24  1 20  7
 4  9 18 13  2
25 14  3  8 19

63 20  3 24 59 36  5 26
 2 23 64 37  4 25 58 35
19 62 21 50 55 60 27  6
22  1 54 61 38 45 34 57
53 18 49 44 51 56  7 28
12 15 52 39 46 31 42 33
17 48 13 10 43 40 29  8
14 11 16 47 30  9 32 41

275 112  19 116 277 604  21 118 823 770  23 120 961 940  25 122 943 926  27 124 917 898  29 126 911 872  31 128 197 870  33
 18 115 276 601  20 117 772 767  22 119 958 851  24 121 954 941  26 123 936 925  28 125 912 899  30 127 910 871  32 129 198
111 274 113 278 605 760 603 822 771 824 769 948 957 960 939 944 953 942 927 916 929 918 897 908 913 900 873 196 875  34 869
114  17 600 273 602 775 766 773 768 949 850 959 852 947 952 955 932 937 930 935 924 915 920 905 894 909 882 901 868 199 130
271 110 279 606 759 610 761 776 821 764 825 816 951 956 853 938 945 934 923 928 919 896 893 914 907 904 867 874 195 876  35
 16 581 272 599 280 607 774 765 762 779 950 849 826 815 946 933 854 931 844 857 890 921 906 895 886 883 902 881 200 131 194
109 270 281 580 609 758 611 744 777 820 763 780 817 848 827 808 811 846 855 922 843 858 889 892 903 866 885 192 877  36 201
282  15 582 269 598 579 608 757 688 745 778 819 754 783 814 847 828 807 810 845 856 891 842 859 884 887 880 863 202 193 132
267 108 283 578 583 612 689 614 743 756 691 746 781 818 753 784 809 812 829 806 801 840 835 888 865 862 203 878 191 530  37
 14 569 268 585 284 597 576 619 690 687 742 755 692 747 782 813 752 785 802 793 830 805 860 841 836 879 864 529 204 133 190
107 266 285 570 577 584 613 686 615 620 695 684 741 732 711 748 739 794 751 786 803 800 839 834 861 528 837 188 531  38 205
286  13 568 265 586 575 596 591 618 685 616 655 696 693 740 733 712 749 738 795 792 831 804 799 838 833 722 527 206 189 134
263 106 287 508 571 590 587 574 621 592 639 694 683 656 731 710 715 734 787 750 737 796 791 832 721 798 207 532 187 474  39
 12 417 264 567 288 509 572 595 588 617 654 657 640 697 680 713 730 709 716 735 788 727 720 797 790 723 526 473 208 135 186
105 262 289 416 507 566 589 512 573 622 593 638 653 682 659 698 679 714 729 708 717 736 789 726 719 472 533 184 475  40 209
290  11 418 261 502 415 510 565 594 513 562 641 658 637 652 681 660 699 678 669 728 707 718 675 724 525 704 471 210 185 136
259 104 291 414 419 506 503 514 511 564 623 548 561 642 551 636 651 670 661 700 677 674 725 706 703 534 211 476 183 396  41
 10 331 260 493 292 501 420 495 504 515 498 563 624 549 560 643 662 635 650 671 668 701 676 673 524 705 470 395 212 137 182
103 258 293 330 413 494 505 500 455 496 547 516 485 552 625 550 559 644 663 634 649 672 667 702 535 394 477 180 397  42 213
294   9 332 257 492 329 456 421 490 499 458 497 546 517 484 553 626 543 558 645 664 633 648 523 666 469 536 393 220 181 138
255 102 295 328 333 412 491 438 457 454 489 440 459 486 545 518 483 554 627 542 557 646 665 632 537 478 221 398 179 214  43
  8 319 256 335 296 345 326 409 422 439 436 453 488 441 460 451 544 519 482 555 628 541 522 647 468 631 392 219 222 139 178
101 254 297 320 327 334 411 346 437 408 423 368 435 452 487 442 461 450 445 520 481 556 629 538 479 466 399 176 215  44 165
298   7 318 253 336 325 344 349 410 347 360 407 424 383 434 427 446 443 462 449 540 521 480 467 630 391 218 223 164 177 140
251 100 303 300 321 316 337 324 343 350 369 382 367 406 425 384 433 428 447 444 463 430 539 390 465 400 175 216 169 166  45
  6 299 252 317 304 301 322 315 348 361 342 359 370 381 366 405 426 385 432 429 448 389 464 401 174 217 224 163 150 141 168
 99 250 241 302 235 248 307 338 323 314 351 362 341 358 371 380 365 404 377 386 431 402 173 388 225 160 153 170 167  46 143
240   5  98 249 242 305 234 247 308 339 232 313 352 363 230 357 372 379 228 403 376 387 226 159 154 171 162 149 142 151  82
 63   2 239  66  97 236 243 306 233 246 309 340 231 312 353 364 229 356 373 378 227 158 375 172 161 148 155 152  83 144  47
  4  67  64  61 238  69  96  59 244  71  94  57 310  73  92  55 354  75  90  53 374  77  88  51 156  79  86  49 146  81  84
  1  62   3  68  65  60 237  70  95  58 245  72  93  56 311  74  91  54 355  76  89  52 157  78  87  50 147  80  85  48 145
```



### Shorter Version

```d
import std.stdio, std.math, std.algorithm, std.range, std.typecons;

alias Square = Tuple!(int,"x", int,"y");

const(Square)[] knightTour(in Square[] board, in Square[] moves) pure @safe nothrow {
    enum findMoves = (in Square sq) pure nothrow @safe =>
        cartesianProduct([1, -1, 2, -2], [1, -1, 2, -2])
        .filter!(ij => ij[0].abs != ij[1].abs)
        .map!(ij => Square(sq.x + ij[0], sq.y + ij[1]))
        .filter!(s => board.canFind(s) && !moves.canFind(s));
    auto newMoves = findMoves(moves.back);
    if (newMoves.empty)
        return moves;
    //alias warnsdorff = min!(s => findMoves(s).walkLength);
    //immutable newSq = newMoves.dropOne.fold!warnsdorff(newMoves.front);
    auto pairs = newMoves.map!(s => tuple(findMoves(s).walkLength, s));
    immutable newSq = reduce!min(pairs.front, pairs.dropOne)[1];
    return board.knightTour(moves ~ newSq);
}

void main(in string[] args) {
    enum toSq = (in string xy) => Square(xy[0] - '`', xy[1] - '0');
    immutable toAlg = (in Square s) => [dchar(s.x + '`'), dchar(s.y + '0')];
    immutable sq = toSq((args.length == 2) ? args[1] : "e5");
    const board = iota(1, 9).cartesianProduct(iota(1, 9)).map!Square.array;
    writefln("%(%-(%s -> %)\n%)", board.knightTour([sq]).map!toAlg.chunks(8));
}
```

```txt
e5 -> d7 -> b8 -> a6 -> b4 -> a2 -> c1 -> b3
a1 -> c2 -> a3 -> b1 -> d2 -> f1 -> h2 -> g4
h6 -> g8 -> e7 -> c8 -> a7 -> c6 -> a5 -> b7
d8 -> f7 -> h8 -> g6 -> f8 -> h7 -> f6 -> e8
g7 -> h5 -> g3 -> h1 -> f2 -> d1 -> b2 -> a4
b6 -> a8 -> c7 -> b5 -> c3 -> d5 -> e3 -> c4
d6 -> e4 -> c5 -> d3 -> e1 -> g2 -> h4 -> f5
d4 -> e2 -> f4 -> e6 -> g5 -> f3 -> g1 -> h3
```



## EchoLisp


The algorithm uses iterative backtracking and Warnsdorff's heuristic. It can output closed or non-closed tours.

```lisp

(require 'plot)
(define *knight-moves*
	'((2 . 1)(2 . -1 ) (1 . -2) (-1 . -2  )(-2 . -1) (-2 . 1) (-1 . 2) (1 . 2)))
(define *hit-squares* null)
(define *legal-moves* null)
(define *tries* 0)

(define (square x y n ) (+ y (* x n)))
(define (dim n) (1- (* n n))) ; n^2 - 1

;; check legal knight move from sq
;; return null or (list destination-square)

(define (legal-disp n sq k-move)
 (let ((x (+ (quotient sq n) (first k-move)))
 	   (y (+  (modulo sq n)  (rest k-move))))
 	   (if (and (>= x 0) (< x n) (>= y 0) (< y n))
 	       (list (square x y n))  null)))

 ;; list of legal destination squares from sq
 (define (legal-moves  sq  k-moves n )
           (if (null? k-moves) null
           (append (legal-moves sq (rest k-moves) n) (legal-disp n sq (first k-moves)))))

;; square freedom = number of destination squares not already reached
(define (freedom sq)
		(for/sum ((dest (vector-ref *legal-moves* sq)))
				(if (vector-ref *hit-squares* dest) 0 1)))

;; The chess adage" A knight on the rim is dim" is false here :
;; choose to move to square with smallest freedom : Warnsdorf's rule
(define (square-sort a b)
	(< (freedom a) (freedom b)))

;; knight tour engine
(define (play sq step starter last-one wants-open)
(set! *tries* (1+ *tries*))
		(vector-set! *hit-squares* sq step) ;; flag used square
		(if (= step last-one) (throw 'HIT last-one)) ;; stop on first path found

		(when (or wants-open ;; cut search iff closed path
		(and  (< step last-one) (> (freedom starter) 0))) ;; this ensures a closed path

		(for ((target (list-sort square-sort (vector-ref *legal-moves* sq))))
			 (unless (vector-ref *hit-squares* target)
			         (play target (1+ step)  starter last-one wants-open))))
		(vector-set! *hit-squares* sq #f)) ;; unflag used square

(define (show-steps n wants-open)
	(string-delimiter "")
	(if wants-open
		(printf "♘-tour: %d tries."  *tries*)
		(printf "♞-closed-tour: %d tries."  *tries*))
	(for ((x n))
		(writeln)
		(for((y n))
        	(write (string-pad-right (vector-ref *hit-squares*  (square x y n)) 4)))))


(define (k-tour (n  8) (starter 0) (wants-open #t))
(set! *hit-squares* (make-vector (* n n) #f))
;; build vector of legal moves for squares 0..n^2-1
(set! *legal-moves*
		(build-vector (* n n) (lambda(sq) (legal-moves sq *knight-moves* n))))
(set! *tries* 0) ; counter
	(try
		(play starter 0 starter (dim n) wants-open)
		(catch (hit mess) (show-steps n wants-open))))

```



```lisp

(k-tour 8 0 #f)
♞-closed-tour: 66 tries.
0   47  14  31  62  27  12  29
15  32  63  54  13  30  57  26
48  1   46  61  56  59  28  11
33  16  55  50  53  44  25  58
2   49  42  45  60  51  10  39
17  34  19  52  43  40  7   24
20  3   36  41  22  5   38  9
35  18  21  4   37  8   23  6

(k-tour 20 57)
♘-tour: 400 tries.
31  34  29  104 209 36  215 300 211 38  213 354 343 40  345 386 383 42  1   388
28  103 32  35  216 299 210 37  214 335 342 39  346 385 382 41  390 387 396 43
33  30  105 208 201 308 301 336 323 212 353 340 355 344 391 384 395 0   389 2
102 27  202 219 298 217 322 309 334 341 356 347 358 351 376 381 378 399 44  397
203 106 207 200 307 228 311 302 337 324 339 352 373 364 379 392 375 394 3   368
26  101 220 229 218 297 304 321 310 333 348 357 350 359 374 377 380 367 398 45
107 204 199 206 227 306 231 312 303 338 325 330 363 372 365 328 393 254 369 4
100 25  122 221 230 233 296 305 320 313 332 349 326 329 360 371 366 251 46  253
121 108 205 198 145 226 237 232 295 286 319 314 331 362 327 316 255 370 5   178
24  99  144 123 222 129 234 279 236 281 294 289 318 315 256 361 250 179 252 47
109 120 111 130 197 146 225 238 285 278 287 272 293 290 317 180 257 162 177 6
98  23  124 143 128 223 276 235 280 239 282 291 288 265 270 249 176 181 48  161
115 110 119 112 131 196 147 224 277 284 273 266 271 292 245 258 163 174 7   58
22  97  114 125 142 127 140 275 194 267 240 283 264 269 248 175 182 59  160 49
87  116 95  118 113 132 195 148 187 274 263 268 191 244 259 246 173 164 57  8
96  21  88  133 126 141 150 139 262 193 190 241 260 247 172 183 60  159 50  65
77  86  117 94  89  138 135 188 149 186 261 192 171 184 243 156 165 64  9   56
20  81  78  85  134 93  90  151 136 189 170 185 242 155 166 61  158 53  66  51
79  76  83  18  91  74  137 16  169 72  153 14  167 70  157 12  63  68  55  10
82  19  80  75  84  17  92  73  152 15  168 71  154 13  62  69  54  11  52  67

```


;Plotting:
64 shades of gray. We plot the move sequence in shades of gray, from black to white. The starting square is red. The ending square is green. One can observe that the squares near the border are played first (dark squares).

```lisp

(define (step-color x y n last-one)
		(letrec ((sq (square (floor x) (floor y) n))
		(step (vector-ref *hit-squares* sq) n n))
		(cond ((= 0 step) (rgb 1 0 0)) ;; red starter
			  ((= last-one step) (rgb 0 1 0)) ;; green end
			  (else (gray (// step n n))))))

(define  ( k-plot n)
	(plot-rgb (lambda (x y) (step-color x y n (dim n))) (- n epsilon) (- n epsilon)))

```



Closed path on a 12x12 board: [http://www.echolalie.org/echolisp/images/k-tour-12.png]

Open path on a 24x24 board: [http://www.echolalie.org/echolisp/images/k-tour-24.png]


## Elixir

```elixir
defmodule Board do
  import Integer, only: [is_odd: 1]

  defmodule Cell do
    defstruct [:value, :adj]
  end

  @adjacent  [[-1,-2],[-2,-1],[-2,1],[-1,2],[1,2],[2,1],[2,-1],[1,-2]]

  defp initialize(rows, cols) do
    board = for i <- 1..rows, j <- 1..cols, into: %{}, do: {{i,j}, true}
    for i <- 1..rows, j <- 1..cols, into: %{} do
      adj = for [di,dj] <- @adjacent, board[{i+di, j+dj}], do: {i+di, j+dj}
      {{i,j}, %Cell{value: 0, adj: adj}}
    end
  end

  defp solve(board, ij, num, goal) do
    board = Map.update!(board, ij, fn cell -> %{cell | value: num} end)
    if num == goal do
      throw({:ok, board})
    else
      wdof(board, ij)
      |> Enum.each(fn k -> solve(board, k, num+1, goal) end)
    end
  end

  defp wdof(board, ij) do               # Warnsdorf's rule
    board[ij].adj
    |> Enum.filter(fn k -> board[k].value == 0 end)
    |> Enum.sort_by(fn k ->
         Enum.count(board[k].adj, fn x -> board[x].value == 0 end)
       end)
  end

  defp to_string(board, rows, cols) do
    width = to_string(rows * cols) |> String.length
    format = String.duplicate("~#{width}w ", cols)
    Enum.map_join(1..rows, "\n", fn i ->
      :io_lib.fwrite format, (for j <- 1..cols, do: board[{i,j}].value)
    end)
  end

  def knight_tour(rows, cols, sx, sy) do
    IO.puts "\nBoard (#{rows} x #{cols}), Start: [#{sx}, #{sy}]"
    if is_odd(rows*cols) and is_odd(sx+sy) do
      IO.puts "No solution"
    else
      try do
        initialize(rows, cols)
        |> solve({sx,sy}, 1, rows*cols)
        IO.puts "No solution"
      catch
        {:ok, board} -> IO.puts to_string(board, rows, cols)
      end
    end
  end
end

Board.knight_tour(8,8,4,2)
Board.knight_tour(5,5,3,3)
Board.knight_tour(4,9,1,1)
Board.knight_tour(5,5,1,2)
Board.knight_tour(12,12,2,2)
```


```txt

Board (8 x 8), Start: [4, 2]
23 20  3 32 25 10  5  8
 2 35 24 21  4  7 26 11
19 22 33 36 31 28  9  6
34  1 50 29 48 37 12 27
51 18 53 44 61 30 47 38
54 43 56 49 58 45 62 13
17 52 41 60 15 64 39 46
42 55 16 57 40 59 14 63

Board (5 x 5), Start: [3, 3]
19  8  3 14 25
 2 13 18  9  4
 7 20  1 24 15
12 17 22  5 10
21  6 11 16 23

Board (4 x 9), Start: [1, 1]
 1 34  3 28 13 24  9 20 17
 4 29  6 33  8 27 18 23 10
35  2 31 14 25 12 21 16 19
30  5 36  7 32 15 26 11 22

Board (5 x 5), Start: [1, 2]
No solution

Board (12 x 12), Start: [2, 2]
 87  24  59   2  89  26  61   4  39   8  31   6
 58   1  88  25  60   3  92  27  30   5  38   9
 23  86  83  90 103  98  29  62  93  40   7  32
 82  57 102  99  84  91 104  97  28  37  10  41
101  22  85 114 105 116 111  94  63  96  33  36
 56  81 100 123 128 113 106 117 110  35  42  11
 21 122 141  80 115 124 127 112  95  64 109  34
144  55  78 121 142 129 118 107 126 133  12  43
 51  20 143 140  79 120 125 138  69 108  65 134
 54  73  52  77 130 139  70 119 132 137  44  13
 19  50  75  72  17  48 131  68  15  46 135  66
 74  53  18  49  76  71  16  47 136  67  14  45

```



## Elm


```elm
import List exposing (concatMap, foldl, head,member,filter,length,minimum,concat,map,map2,tail)
import List.Extra exposing (minimumBy, andThen)
import String exposing (join)
import Html as H
import Html.Attributes as HA
import Html.App exposing (program)
import Time exposing (Time,every, second)
import Svg exposing (rect, line, svg, g)
import Svg.Events exposing (onClick)
import Svg.Attributes exposing (version, viewBox, x, y, x1, y1, x2, y2, fill, style, width, height)

w = 450
h = 450
rowCount=20
colCount=20
dt = 0.03

type alias Cell = (Int, Int)

type alias Model =
    { path : List Cell
    , board : List Cell
    }

type Msg = NoOp | Tick Time | SetStart Cell

init : (Model,Cmd Msg)
init =
    let board = [0..rowCount-1] `andThen` \r ->
                [0..colCount-1] `andThen` \c ->
                [(r, c)]
        path = []
    in (Model path board, Cmd.none)

view : Model -> H.Html Msg
view model =
    let
        showChecker row col =
            rect [ x <| toString col
                 , y <| toString row
                 , width "1"
                 , height "1"
                 , fill <| if (row + col) % 2 == 0 then "blue" else "grey"
                 , onClick <| SetStart (row, col)
                 ]
                 []

        showMove (row0,col0) (row1,col1) =
            line [ x1 <| toString ((toFloat col0) + 0.5)
                 , y1 <| toString ((toFloat row0) + 0.5)
                 , x2 <| toString ((toFloat col1) + 0.5)
                 , y2 <| toString ((toFloat row1) + 0.5)
                 , style "stroke:yellow;stroke-width:0.05"
                 ]
                 []

        render model =
            let checkers = model.board `andThen` \(r,c) ->
                           [showChecker r c]
                moves = case List.tail model.path of
                        Nothing -> []
                        Just tl -> List.map2 showMove model.path tl
            in checkers ++ moves

        unvisited = length model.board - length model.path

        center = HA.style [ ( "text-align", "center") ]

    in
        H.div
          []
          [ H.h2 [center] [H.text "Knight's Tour"]
          , H.h2 [center] [H.text <| "Unvisited count : " ++ toString unvisited ]
          , H.h2 [center] [H.text "(pick a square)"]
          , H.div
              [center]
              [ svg
                  [ version "1.1"
                  , width (toString w)
                  , height (toString h)
                  , viewBox (join " "
                               [ toString 0
                               , toString 0
                               , toString colCount
                               , toString rowCount ])
                  ]
                  [ g [] <| render model]
              ]
          ]

nextMoves : Model -> Cell -> List Cell
nextMoves model (stRow,stCol) =
  let c = [ 1,  2, -1, -2]

      km = c `andThen` \cRow ->
           c `andThen` \cCol ->
           if abs(cRow) == abs(cCol) then [] else [(cRow,cCol)]

      jumps = List.map (\(kmRow,kmCol) -> (kmRow + stRow, kmCol + stCol)) km

  in List.filter (\j -> List.member j model.board && not (List.member j model.path) ) jumps

bestMove : Model -> Maybe Cell
bestMove model =
    case List.head (model.path) of
        Nothing -> Nothing
        Just mph -> minimumBy (List.length << nextMoves model) (nextMoves model mph)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    let mo = case msg of
                 SetStart start ->
                     {model |  path = [start]}
                 Tick t ->
                     case model.path of
                         [] -> model
                         _ -> case bestMove model of
                                  Nothing -> model
                                  Just best -> {model | path = best::model.path }
                 NoOp -> model
    in (mo, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (dt * second) Tick

main =
  program
      { init = init
      , view = view
      , update = update
      , subscriptions = subscriptions
      }
```


Link to live demo: http://dc25.github.io/knightsTourElm/


## Erlang

Again I use backtracking. It seemed easier this time.

```Erlang

-module( knights_tour ).

-export( [display/1, solve/1, task/0] ).

display( Moves ) ->
	%% The knigh walks the moves {Position, Step_nr} order.
	%% Top left corner is {$a, 8}, Bottom right is {$h, 1}.
	io:fwrite( "Moves:" ),
	lists:foldl( fun display_moves/2, erlang:length(Moves), lists:keysort(2, Moves) ),
	io:nl(),
	[display_row(Y, Moves) || Y <- lists:seq(8, 1, -1)].

solve( First_square ) ->
    try
    bt_loop( 1, next_moves(First_square), [{First_square, 1}] )

    catch
    _:{ok, Moves} -> Moves

    end.

task() ->
	io:fwrite( "Starting {a, 1}~n" ),
	Moves = solve( {$a, 1} ),
	display( Moves ).



bt( N, Move, Moves ) -> bt_reject( is_not_allowed_knight_move(Move, Moves), N, Move, [{Move, N} | Moves] ).

bt_accept( true, _N, _Move, Moves ) -> erlang:throw( {ok, Moves} );
bt_accept( false, N, Move, Moves ) -> bt_loop( N, next_moves(Move), Moves ).

bt_loop( N, New_moves, Moves ) -> [bt( N+1, X, Moves ) || X <- New_moves].

bt_reject( true, _N, _Move, _Moves ) -> backtrack;
bt_reject( false, N, Move, Moves ) -> bt_accept( is_all_knights(Moves), N, Move, Moves ).

display_moves( {{X, Y}, 1}, Max ) ->
	io:fwrite(" ~p. N~c~p", [1, X, Y]),
	Max;
display_moves( {{X, Y}, Max}, Max ) ->
	io:fwrite(" N~c~p~n", [X, Y]),
	Max;
display_moves( {{X, Y}, Step_nr}, Max ) when Step_nr rem 8 =:= 0 ->
	io:fwrite(" N~c~p~n~p. N~c~p", [X, Y, Step_nr, X, Y]),
	Max;
display_moves( {{X, Y}, Step_nr}, Max ) ->
	io:fwrite(" N~c~p ~p. N~c~p", [X, Y, Step_nr, X, Y]),
	Max.

display_row( Row, Moves ) ->
	[io:fwrite(" ~2b", [proplists:get_value({X, Row}, Moves)]) || X <- [$a, $b, $c, $d, $e, $f, $g, $h]],
	io:nl().

is_all_knights( Moves ) when erlang:length(Moves) =:= 64 -> true;
is_all_knights( _Moves ) -> false.

is_asymetric( Start_column, Start_row, Stop_column, Stop_row ) ->
	erlang:abs( Start_column - Stop_column ) =/= erlang:abs( Start_row - Stop_row ).

is_not_allowed_knight_move( Move, Moves ) ->
	no_such_move =/= proplists:get_value( Move, Moves, no_such_move ).

next_moves( {Column, Row} ) ->
	[{X, Y} || X <- next_moves_column(Column), Y <- next_moves_row(Row), is_asymetric(Column, Row, X, Y)].

next_moves_column( $a ) -> [$b, $c];
next_moves_column( $b ) -> [$a, $c, $d];
next_moves_column( $g ) -> [$e, $f, $h];
next_moves_column( $h ) -> [$g, $f];
next_moves_column( C ) -> [C - 2, C - 1, C + 1, C + 2].

next_moves_row( 1 ) -> [2, 3];
next_moves_row( 2 ) -> [1, 3, 4];
next_moves_row( 7 ) -> [5, 6, 8];
next_moves_row( 8 ) -> [6, 7];
next_moves_row( N ) -> [N - 2, N - 1, N + 1, N + 2].

```

```txt

17> knights_tour:task().
Starting {a, 1}
Moves: 1. Na1 Nb3 2. Nb3 Na5 3. Na5 Nb7 4. Nb7 Nc5 5. Nc5 Na4 6. Na4 Nb2 7. Nb2 Nc4
8. Nc4 Na3 9. Na3 Nb1 10. Nb1 Nc3 11. Nc3 Na2 12. Na2 Nb4 13. Nb4 Na6 14. Na6 Nb8 15. Nb8 Nc6
16. Nc6 Na7 17. Na7 Nb5 18. Nb5 Nc7 19. Nc7 Na8 20. Na8 Nb6 21. Nb6 Nc8 22. Nc8 Nd6 23. Nd6 Ne4
24. Ne4 Nd2 25. Nd2 Nf1 26. Nf1 Ne3 27. Ne3 Nc2 28. Nc2 Nd4 29. Nd4 Ne2 30. Ne2 Nc1 31. Nc1 Nd3
32. Nd3 Ne1 33. Ne1 Ng2 34. Ng2 Nf4 35. Nf4 Nd5 36. Nd5 Ne7 37. Ne7 Ng8 38. Ng8 Nh6 39. Nh6 Nf5
40. Nf5 Nh4 41. Nh4 Ng6 42. Ng6 Nh8 43. Nh8 Nf7 44. Nf7 Nd8 45. Nd8 Ne6 46. Ne6 Nf8 47. Nf8 Nd7
48. Nd7 Ne5 49. Ne5 Ng4 50. Ng4 Nh2 51. Nh2 Nf3 52. Nf3 Ng1 53. Ng1 Nh3 54. Nh3 Ng5 55. Ng5 Nh7
56. Nh7 Nf6 57. Nf6 Ne8 58. Ne8 Ng7 59. Ng7 Nh5 60. Nh5 Ng3 61. Ng3 Nh1 62. Nh1 Nf2 63. Nf2 Nd1

 20 15 22 45 58 47 38 43
 17  4 19 48 37 44 59 56
 14 21 16 23 46 57 42 39
  3 18  5 36 49 40 55 60
  6 13  8 29 24 35 50 41
  9  2 11 32 27 52 61 54
 12  7 28 25 30 63 34 51
  1 10 31 64 33 26 53 62

```



## ERRE

Taken from ERRE distribution disk. Comments are in Italian.

```ERRE

! **********************************************************************
! *                                                                    *
! *     IL GIRO DEL CAVALLO - come collocare un cavallo su di una      *
! *                           scacchiera n*n passando una sola volta   *
! *                           per ogni casella.                        *
! *                                                                    *
! **********************************************************************
! ----------------------------------------------------------------------
!                   Inizializzazione dei parametri
! ----------------------------------------------------------------------

PROGRAM KNIGHT

!$INTEGER
!$KEY

DIM H[25,25],A[8],B[8],P0[8],P1[8]

!$INCLUDE="PC.LIB"

PROCEDURE INIT_SCACCHIERA
! **********************************************************************
! *         Routine di inizializzazione scacchiera                     *
! **********************************************************************
     FOR I1=1 TO 8 DO
         U=X+A[I1]  V=Y+B[I1]
         IF (U>0 AND U<=N) AND (V>0 AND V<=N) THEN
             H[U,V]=H[U,V]-1
         END IF
     END FOR
END PROCEDURE

PROCEDURE MOSTRA_SCACCHIERA
! *********************************************************************
! *         Routine di visualizzazione della scacchiera               *
! *********************************************************************
     LOCATE(5,1)  COLOR(0,7) PRINT(" Mossa num.";NMOS) COLOR(7,0)
     L2=N
     FOR I2=1 TO N DO
         PRINT
         FOR L1=1 TO N DO
             IF H[L1,L2]>0 THEN COLOR(15,0) END IF
             WRITE("####";H[L1,L2];)
             COLOR(7,0)
         END FOR
         L2=L2-1
     END FOR
END PROCEDURE

PROCEDURE AGGIORNA_SCACCHIERA
! *********************************************************************
! *        Routine di Aggiornamento Scacchiera                        *
! *********************************************************************
     B=1
     FOR I1=1 TO 8 DO
         U=X+A[I1] V=Y+B[I1]
         IF (U>0 AND U<=N) AND (V>0 AND V<=N) THEN
             IF H[U,V]<=0 THEN
                 H[U,V]=H[U,V]+1 B=0
             END IF
         END IF
      END FOR
      IF B=1 THEN Q1=0 END IF
END PROCEDURE

PROCEDURE MOSSA_MAX_PESO
! *********************************************************************
! *         Cerca la prossima mossa con il massimo peso               *
! *********************************************************************
     M1=0  RO=1
     FOR W=1 TO 8 DO
         U=Z1+A[W] V=Z2+B[W]
         IF (U>0 AND U<=N) AND (V>0 AND V<=N) THEN
              IF H[U,V]<=0 AND H[U,V]<=M1 THEN
                  IF H[U,V]=M1 THEN
                      RO=RO+1 P0[RO]=W
                   ELSE
                      M1=H[U,V] Q1=1  T1=U T2=V RO=1 P0[1]=W
                  END IF
              END IF
         END IF
     END FOR
END PROCEDURE

PROCEDURE MOSSA_MIN_PESO
! *********************************************************************
! *          Cerca la prossima mossa con il minimo peso               *
! *********************************************************************
     M1=-9 RO=1
     FOR W=1 TO 8 DO
        U=Z1+A[W]  V=Z2+B[W]
        IF (U>0 AND U<=N) AND (V>0 AND V<=N) THEN
              IF H[U,V]<=0 AND H[U,V]>=M1 THEN
                   IF H[U,V]=M1 THEN
                        RO=RO+1 P0[RO]=W
                     ELSE
                        M1=H[U,V] Q1=1  T1=U T2=V RO=1 P0[1]=W
                   END IF
              END IF
        END IF
     END FOR
END PROCEDURE

BEGIN
     A[1]=1     A[2]=2   A[3]=2   A[4]=1
     A[5]=-1    A[6]=-2  A[7]=-2  A[8]=-1
     B[1]=2     B[2]=1   B[3]=-1  B[4]=-2
     B[5]=-2    B[6]=-1  B[7]=1   B[8]=2

     CLS
     PRINT("            ***    LA GALOPPATA DEL CAVALIERE    ***")
     PRINT
     PRINT("Inserire la dimensione della scacchiera (max. 25)";)
     INPUT(N)
     PRINT("Inserire la caselle di partenza (x,y) ";)
     INPUT(X1,Y1)
     NMOS=1  A1=1  N1=N*N  ESCAPE=FALSE
! ----------------------------------------------------------------------
!                  Set della scacchiera
! ----------------------------------------------------------------------
     WHILE NOT ESCAPE DO
          FOR I=1 TO N DO
             FOR J=1 TO N DO
                H[I,J]=0
             END FOR
          END FOR
          FOR I=1 TO N DO
             FOR J=1 TO N DO
                X=I  Y=J
                INIT_SCACCHIERA
             END FOR
          END FOR

! ----------------------------------------------------------------------
!                       Effettua la prima mossa
! ----------------------------------------------------------------------
          X=X1  Y=Y1  H[X,Y]=1   L=2
          AGGIORNA_SCACCHIERA
          Q1=1  Q2=1
! -----------------------------------------------------------------------
!                        Trova la prossima mossa
! -----------------------------------------------------------------------
          WHILE Q1<>0 AND Q2<>0 DO
               Q1=0 Z1=X Z2=Y
               MOSSA_MIN_PESO
               IF RO<=1 THEN
                   C1=T1    C2=T2
                ELSE
! ------------------------------------------------------------------------
!                         Esamina tutti i vincoli
! ------------------------------------------------------------------------
                   FOR K=1 TO RO DO
                     P1[K]=P0[K]
                   END FOR
                   R1=RO
                   IF A1=1 THEN M2=-9 ELSE M2=0 END IF
                   FOR K=1 TO R1 DO
                      F1=P1[K]   Z1=X+A[F1]   Z2=Y+B[F1]
                      IF A1=1 THEN
                          MOSSA_MAX_PESO
                          IF M1<=M2 THEN
                              !$NULL
                            ELSE
                              M2=M1 C1=Z1 C2=Z2
                           END IF
                        ELSE
                           MOSSA_MIN_PESO
                           IF M1>=M2 THEN
                               !$NULL
                             ELSE
                               M2=M1  C1=Z1  C2=Z2
                            END IF
                        END IF
                   END FOR
! ------------------------------------------------------------------------
!          Prossima mossa trovata:aggiorna la scacchiera
! ------------------------------------------------------------------------
               END IF
               IF Q1<>0 THEN
                     X=C1  Y=C2 H[X,Y]=L
                     AGGIORNA_SCACCHIERA
                     IF L=N1 THEN Q2=0 END IF
                END IF
                L=L+1
                MOSTRA_SCACCHIERA
                NMOS=NMOS+1
          END WHILE
! ------------------------------------------------------------------------
!           La ricerca è terminata: visualizza i risultati
! ------------------------------------------------------------------------
          PRINT PRINT
          IF Q2<>1 THEN
              PRINT("*** Trovata la soluzione! ***")
              MOSTRA_SCACCHIERA
              ESCAPE=TRUE
            ELSE
              IF A1=0 THEN
                  PRINT("Nessuna soluzione.")
                  ESCAPE=TRUE
                ELSE
                  BEEP
                  A1=0
              END IF
           END IF
      END WHILE
      REPEAT
         GET(A$)
      UNTIL A$<>""
END PROGRAM

```

```txt
            ***    LA GALOPPATA DEL CAVALIERE    ***

Inserire la dimensione della scacchiera (max. 25)? 8
Inserire la caselle di partenza (x,y) ? 1,1
 Mossa num. 64

  64   7  54  41  60   9  48  39
  53  42  61   8  55  40  35  10
   6  63  44  59  34  49  38  47
  43  52  21  62  45  56  11  36
  20   5  58  33  50  37  46  25
  31   2  51  22  57  26  15  12
   4  19  32  29  14  17  24  27
   1  30   3  18  23  28  13  16

*** Trovata la soluzione! ***

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Knight%27s_tour this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## FreeBASIC


```freebasic

Dim Shared As Integer tamano, xc, yc, nm
Dim As Integer f, qm, nmov, n = 0
Dim As String posini

Cls : Color 11
Input "Tamaño tablero:  ", tamano
Input "Posicion inicial: ", posini

Dim As Integer x = Asc(Mid(posini,1,1))-96
Dim As Integer y = Val(Mid(posini,2,1))
Dim Shared As Integer tablero(tamano,tamano), dx(8), dy(8)
For f = 1 To 8 : Read dx(f), dy(f) : Next f
Data 2,1,1,2,-1,2,-2,1,-2,-1,-1,-2,1,-2,2,-1

Sub FindMoves()
    Dim As Integer i, xt, yt
    If xc < 1 Or yc < 1 Or xc > tamano Or yc > tamano Then nm = 1000: Return
    If tablero(xc,yc) Then nm = 2000: Return
    nm = 0
    For i = 1 To 8
        xt = xc+dx(i)
        yt = yc+dy(i)
        If xt < 1 Or yt < 1 Or xt > tamano Or yt > tamano Then 'Salta este movimiento
        Elseif tablero(xt,yt) Then 'Salta este movimiento
        Else
            nm += 1
        End If
    Next i
End Sub

Color 4, 7 'Pinta tablero
For f = 1 To tamano
    Locate 15-tamano, 3*f: Print "  "; Chr(96+f); " ";
    Locate 17-f, 3*(tamano+1)+1: Print Using "##"; f;
Next f

Color 15, 0
Do
    n += 1
    tablero(x,y) = n
    Locate 17-y, 3*x: Print Using "###"; n;
    If n = tamano*tamano Then Exit Do
    nmov = 100
    For f = 1 To 8
        xc = x+dx(f)
        yc = y+dy(f)
        FindMoves()
        If nm < nmov Then nmov = nm: qm = f
    Next f
    x = x+dx(qm)
    y = y+dy(qm)
    Sleep 1
Loop
Color 14 : Locate Csrlin+tamano, 1
Print " Pulsa cualquier tecla para finalizar..."
Sleep
End

```

[https://www.dropbox.com/s/s3bpwechpoueum4/Knights%20Tour%20FreeBasic.png?dl=0 Knights Tour FreeBasic image]

```txt

Tamaño tablero:  8
Posicion inicial: c3


  a  b  c  d  e  f  g  h

 24 11 22 19 26  9 38 47  8
 21 18 25 10 39 48 27  8  7
 12 23 20 53 28 37 46 49  6
 17 52 29 40 59 50  7 36  5
 30 13 58 51 54 41 62 45  4
 57 16  1 42 63 60 35  6  3
  2 31 14 55  4 33 44 61  2
 15 56  3 32 43 64  5 34  1


Pulsa cualquier tecla para finalizar...

```



## Go

===Warnsdorf's rule===

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

// input, 0-based start position
const startRow = 0
const startCol = 0

func main() {
    rand.Seed(time.Now().Unix())
    for !knightTour() {
    }
}

var moves = []struct{ dr, dc int }{
    {2, 1},
    {2, -1},
    {1, 2},
    {1, -2},
    {-1, 2},
    {-1, -2},
    {-2, 1},
    {-2, -1},
}

// Attempt knight tour starting at startRow, startCol using Warnsdorff's rule
// and random tie breaking.  If a tour is found, print it and return true.
// Otherwise no backtracking, just return false.
func knightTour() bool {
    // 8x8 board.  squares hold 1-based visit order.  0 means unvisited.
    board := make([][]int, 8)
    for i := range board {
        board[i] = make([]int, 8)
    }
    r := startRow
    c := startCol
    board[r][c] = 1 // first move
    for move := 2; move <= 64; move++ {
        minNext := 8
        var mr, mc, nm int
    candidateMoves:
        for _, cm := range moves {
            cr := r + cm.dr
            if cr < 0 || cr >= 8 { // off board
                continue
            }
            cc := c + cm.dc
            if cc < 0 || cc >= 8 { // off board
                continue
            }
            if board[cr][cc] > 0 { // already visited
                continue
            }
            // cr, cc candidate legal move.
            p := 0 // count possible next moves.
            for _, m2 := range moves {
                r2 := cr + m2.dr
                if r2 < 0 || r2 >= 8 {
                    continue
                }
                c2 := cc + m2.dc
                if c2 < 0 || c2 >= 8 {
                    continue
                }
                if board[r2][c2] > 0 {
                    continue
                }
                p++
                if p > minNext { // bail out as soon as it's eliminated
                    continue candidateMoves
                }
            }
            if p < minNext { // it's better.  keep it.
                minNext = p // new min possible next moves
                nm = 1      // number of candidates with this p
                mr = cr     // best candidate move
                mc = cc
                continue
            }
            // it ties for best so far.
            // keep it with probability 1/(number of tying moves)
            nm++                    // number of tying moves
            if rand.Intn(nm) == 0 { // one chance to keep it
                mr = cr
                mc = cc
            }
        }
        if nm == 0 { // no legal move
            return false
        }
        // make selected move
        r = mr
        c = mc
        board[r][c] = move
    }
    // tour complete.  print board.
    for _, r := range board {
        for _, m := range r {
            fmt.Printf("%3d", m)
        }
        fmt.Println()
    }
    return true
}
```

```txt

  1  4 39 20 23  6 63 58
 40 19  2  5 62 57 22  7
  3 38 41 48 21 24 59 64
 18 43 32 37 56 61  8 25
 31 14 47 42 49 36 53 60
 46 17 44 33 52 55 26  9
 13 30 15 50 11 28 35 54
 16 45 12 29 34 51 10 27

```


### Ant colony


```go
/* Adapted from "Enumerating Knight's Tours using an Ant Colony Algorithm"
by Philip Hingston and Graham Kendal,
PDF at http://www.cs.nott.ac.uk/~gxk/papers/cec05knights.pdf. */

package main

import (
    "fmt"
    "math/rand"
    "sync"
    "time"
)

const boardSize = 8
const nSquares = boardSize * boardSize
const completeTour = nSquares - 1

// task input: starting square.  These are 1 based, but otherwise 0 based
// row and column numbers are used througout the program.
const rStart = 2
const cStart = 3

// pheromone representation read by ants
var tNet = make([]float64, nSquares*8)

// row, col deltas of legal moves
var drc = [][]int{{1, 2}, {2, 1}, {2, -1}, {1, -2},
    {-1, -2}, {-2, -1}, {-2, 1}, {-1, 2}}

// get square reached by following edge k from square (r, c)
func dest(r, c, k int) (int, int, bool) {
    r += drc[k][0]
    c += drc[k][1]
    return r, c, r >= 0 && r < boardSize && c >= 0 && c < boardSize
}

// struct represents a pheromone amount associated with a move
type rckt struct {
    r, c, k int
    t       float64
}

func main() {
    fmt.Println("Starting square:  row", rStart, "column", cStart)
    // initialize board
    for r := 0; r < boardSize; r++ {
        for c := 0; c < boardSize; c++ {
            for k := 0; k < 8; k++ {
                if _, _, ok := dest(r, c, k); ok {
                    tNet[(r*boardSize+c)*8+k] = 1e-6
                }
            }
        }
    }

    // waitGroups for ant release clockwork
    var start, reset sync.WaitGroup
    start.Add(1)
    // channel for ants to return tours with pheremone updates
    tch := make(chan []rckt)

    // create an ant for each square
    for r := 0; r < boardSize; r++ {
        for c := 0; c < boardSize; c++ {
            go ant(r, c, &start, &reset, tch)
        }
    }

    // accumulator for new pheromone amounts
    tNew := make([]float64, nSquares*8)

    // each iteration is a "cycle" as described in the paper
    for {
        // evaporate pheromones
        for i := range tNet {
            tNet[i] *= .75
        }

        reset.Add(nSquares) // number of ants to release
        start.Done()        // release them
        reset.Wait()        // wait for them to begin searching
        start.Add(1)        // reset start signal for next cycle

        // gather tours from ants
        for i := 0; i < nSquares; i++ {
            tour := <-tch
            // watch for a complete tour from the specified starting square
            if len(tour) == completeTour &&
                tour[0].r == rStart-1 && tour[0].c == cStart-1 {

                // task output:  move sequence in a grid.
                seq := make([]int, nSquares)
                for i, sq := range tour {
                    seq[sq.r*boardSize+sq.c] = i + 1
                }
                last := tour[len(tour)-1]
                r, c, _ := dest(last.r, last.c, last.k)
                seq[r*boardSize+c] = nSquares
                fmt.Println("Move sequence:")
                for r := 0; r < boardSize; r++ {
                    for c := 0; c < boardSize; c++ {
                        fmt.Printf(" %3d", seq[r*boardSize+c])
                    }
                    fmt.Println()
                }
                return // task only requires finding a single tour
            }
            // accumulate pheromone amounts from all ants
            for _, move := range tour {
                tNew[(move.r*boardSize+move.c)*8+move.k] += move.t
            }
        }

        // update pheromone amounts on network, reset accumulator
        for i, tn := range tNew {
            tNet[i] += tn
            tNew[i] = 0
        }
    }
}

type square struct {
    r, c int
}

func ant(r, c int, start, reset *sync.WaitGroup, tourCh chan []rckt) {
    rnd := rand.New(rand.NewSource(time.Now().UnixNano()))
    tabu := make([]square, nSquares)
    moves := make([]rckt, nSquares)
    unexp := make([]rckt, 8)
    tabu[0].r = r
    tabu[0].c = c

    for {
        // cycle initialization
        moves = moves[:0]
        tabu = tabu[:1]
        r := tabu[0].r
        c := tabu[0].c

        // wait for start signal
        start.Wait()
        reset.Done()

        for {
            // choose next move
            unexp = unexp[:0]
            var tSum float64
        findU:
            for k := 0; k < 8; k++ {
                dr, dc, ok := dest(r, c, k)
                if !ok {
                    continue
                }
                for _, t := range tabu {
                    if t.r == dr && t.c == dc {
                        continue findU
                    }
                }
                tk := tNet[(r*boardSize+c)*8+k]
                tSum += tk
                // note:  dest r, c stored here
                unexp = append(unexp, rckt{dr, dc, k, tk})
            }
            if len(unexp) == 0 {
                break // no moves
            }
            rn := rnd.Float64() * tSum
            var move rckt
            for _, move = range unexp {
                if rn <= move.t {
                    break
                }
                rn -= move.t
            }

            // move to new square
            move.r, r = r, move.r
            move.c, c = c, move.c
            tabu = append(tabu, square{r, c})
            moves = append(moves, move)
        }

        // compute pheromone amount to leave
        for i := range moves {
            moves[i].t = float64(len(moves)-i) / float64(completeTour-i)
        }

        // return tour found for this cycle
        tourCh <- moves
    }
}
```

Output:

```txt

Starting square:  row 2 column 3
Move sequence:
  64  33  36   3  54  49  38  51
  35   4   1  30  37  52  55  48
  32  63  34  53   2  47  50  39
   5  18  31  46  29  20  13  56
  62  27  44  19  14  11  40  21
  17   6  15  28  45  22  57  12
  26  61   8  43  24  59  10  41
   7  16  25  60   9  42  23  58

```



## Haskell


```Haskell
{-# LANGUAGE TupleSections #-}

import Data.List (minimumBy, (\\), intercalate, sort)
import Data.Ord (comparing)
import Data.Char (ord, chr)
import Data.Bool (bool)

type Square = (Int, Int)

knightTour :: [Square] -> [Square]
knightTour moves
  | null possibilities = reverse moves
  | otherwise = knightTour $ newSquare : moves
  where
    newSquare = minimumBy (comparing (length . findMoves)) possibilities
    possibilities = findMoves $ head moves
    findMoves = (\\ moves) . knightOptions

knightOptions :: Square -> [Square]
knightOptions (x, y) =
  knightMoves >>=
  (\(i, j) ->
      let a = x + i
          b = y + j
      in bool [] [(a, b)] (onBoard a && onBoard b))

knightMoves :: [(Int, Int)]
knightMoves =
  let deltas = [id, negate] <*> [1, 2]
  in deltas >>=
     (\i -> deltas >>= (bool [] . return . (i, )) <*> ((abs i /=) . abs))

onBoard :: Int -> Bool
onBoard = (&&) . (0 <) <*> (9 >)

-- TEST ---------------------------------------------------
startPoint = "e5"

algebraic :: (Int, Int) -> String
algebraic (x, y) = [chr (x + 96), chr (y + 48)]

main :: IO ()
main =
  printTour $
  algebraic <$> knightTour [(\[x, y] -> (ord x - 96, ord y - 48)) startPoint]
  where
    printTour [] = return ()
    printTour tour = do
      putStrLn $ intercalate " -> " $ take 8 tour
      printTour $ drop 8 tour
```

```txt
e5 -> f7 -> h8 -> g6 -> h4 -> g2 -> e1 -> f3
g1 -> h3 -> g5 -> h7 -> f8 -> d7 -> b8 -> a6
b4 -> a2 -> c1 -> d3 -> b2 -> d1 -> f2 -> h1
g3 -> h5 -> g7 -> e8 -> f6 -> g8 -> h6 -> g4
h2 -> f1 -> e3 -> f5 -> e7 -> c8 -> a7 -> c6
d8 -> b7 -> a5 -> b3 -> a1 -> c2 -> d4 -> e2
f4 -> e6 -> c5 -> a4 -> b6 -> a8 -> c7 -> d5
c3 -> e4 -> d6 -> b5 -> a3 -> b1 -> d2 -> c4
```


=={{header|Icon}} and {{header|Unicon}}==
This implements Warnsdorff's algorithm using unordered sets.
* The board must be square (it has only been tested on 8x8 and 7x7).  Currently the maximum size board (due to square notation) is 26x26.
* Tie breaking is selectable with 3 variants supplied (first in list, random, and Roth's distance heuristic).
* A debug log can be generated showing the moves and choices considered for tie breaking.

The algorithm doesn't always generate a complete tour.

```Icon
link printf

procedure main(A)
ShowTour(KnightsTour(Board(8)))
end

procedure KnightsTour(B,sq,tbrk,debug)  #: Warnsdorff’s algorithm

/B := Board(8)                          # create 8x8 board if none given
/sq := ?B.files || ?B.ranks             # random initial position (default)
sq2fr(sq,B)                             # validate initial sq
if type(tbrk) == "procedure" then
   B.tiebreak := tbrk                   # override tie-breaker
if \debug then write("Debug log : move#, move : (accessibility) choices")

choices := []                           # setup to track moves and choices
every (movesto := table())[k := key(B.movesto)] := copy(B.movesto[k])

B.tour := []                            # new tour
repeat {
   put(B.tour,sq)                       # record move

   ac := 9                              # accessibility counter > maximum
   while get(choices)                   # empty choices for tiebreak
   every delete(movesto[nextsq := !movesto[sq]],sq) do {  # make sq unavailable
      if ac >:= *movesto[nextsq] then   # reset to lower accessibility count
         while get(choices)             # . re-empty choices
      if ac = *movesto[nextsq] then
         put(choices,nextsq)            # keep least accessible sq and any ties
      }

   if \debug then {                     # move#, move, (accessibility), choices
      writes(sprintf("%d. %s : (%d) ",*B.tour,sq,ac))
      every writes(" ",!choices|"\n")
      }
   sq := B.tiebreak(choices,B) | break  # choose next sq until out of choices
   }
return B
end

procedure RandomTieBreaker(S,B)                   # random choice
return ?S
end

procedure FirstTieBreaker(S,B)                    # first one in the list
return !S
end

procedure RothTieBreaker(S,B)                    # furthest from the center
if *S = 0 then fail                              # must fail if []
every fr := sq2fr(s := !S,B) do {
   d := sqrt(abs(fr[1]-1 - (B.N-1)*0.5)^2 + abs(fr[2]-1 - (B.N-1)*0.5)^2)
   if (/md := d) | ( md >:= d) then msq := s     # save sq
   }
return msq
end

record board(N,ranks,files,movesto,tiebreak,tour)  # structure for board

procedure Board(N)                      #: create board
N := *&lcase >=( 0 < integer(N)) | stop("N=",image(N)," is out of range.")
B := board(N,[],&lcase[1+:N],table(),RandomTieBreaker)       # setup
every put(B.ranks,N to 1 by -1)                              # add rank #s
every sq := !B.files || !B.ranks do                          # for each sq add
   every insert(B.movesto[sq] := set(), KnightMoves(sq,B))   # moves to next sq
return B
end

procedure sq2fr(sq,B)                   #: return numeric file & rank
f := find(sq[1],B.files)                | runerr(205,sq)
r := integer(B.ranks[sq[2:0]])          | runerr(205,sq)
return [f,r]
end

procedure KnightMoves(sq,B)         #: generate all Kn accessible moves from sq
fr := sq2fr(sq,B)
every ( i := -2|-1|1|2 ) & ( j := -2|-1|1|2 ) do
   if (abs(i)~=abs(j)) & (0<(ri:=fr[2]+i)<=B.N) & (0<(fj:=fr[1]+j)<=B.N) then
      suspend B.files[fj]||B.ranks[ri]
end

procedure ShowTour(B)                   #: show the tour
write("Board size = ",B.N)
write("Tour length = ",*B.tour)
write("Tie Breaker = ",image(B.tiebreak))

every !(squares := list(B.N)) := list(B.N,"-")
every fr := sq2fr(B.tour[m := 1 to *B.tour],B) do
   squares[fr[2],fr[1]] := m

every (hdr1 := "     ") ||:= right(!B.files,3)
every (hdr2 := "    +") ||:= repl((1 to B.N,"-"),3) | "-+"

every write(hdr1|hdr2)
every r := 1 to B.N do {
   writes(right(B.ranks[r],3)," |")
   every writes(right(squares[r,f := 1 to B.N],3))
   write(" |",right(B.ranks[r],3))
   }
every write(hdr2|hdr1|&null)
end
```


The following can be used when debugging to validate the board structure and to image the available moves on the board.

```Icon
procedure DumpBoard(B)  #: Dump Board internals
write("Board size=",B.N)
write("Available Moves at start of tour:", ImageMovesTo(B.movesto))
end

procedure ImageMovesTo(movesto)  #: image of available moves
every put(K := [],key(movesto))
every (s := "\n") ||:= (k := !sort(K)) || " : " do
   every s ||:= " " || (!sort(movesto[k])|"\n")
return s
end
```



Sample output:
```txt
Board size = 8
Tour length = 64
Tie Breaker = procedure RandomTieBreaker
       a  b  c  d  e  f  g  h
    +-------------------------+
  8 | 53 10 29 26 55 12 31 16 |  8
  7 | 28 25 54 11 30 15 48 13 |  7
  6 |  9 52 27 62 47 56 17 32 |  6
  5 | 24 61 38 51 36 45 14 49 |  5
  4 | 39  8 63 46 57 50 33 18 |  4
  3 | 64 23 60 37 42 35 44  3 |  3
  2 |  7 40 21 58  5  2 19 34 |  2
  1 | 22 59  6 41 20 43  4  1 |  1
    +-------------------------+
       a  b  c  d  e  f  g  h
```


Two 7x7 boards:
```txt
Board size = 7
Tour length = 33
Tie Breaker = procedure RandomTieBreaker
       a  b  c  d  e  f  g
    +----------------------+
  7 | 33  4 15  - 29  6 17 |  7
  6 | 14  - 30  5 16  - 28 |  6
  5 |  3 32  -  -  - 18  7 |  5
  4 |  - 13  - 31  - 27  - |  4
  3 | 23  2  -  -  -  8 19 |  3
  2 | 12  - 24 21 10  - 26 |  2
  1 |  1 22 11  - 25 20  9 |  1
    +----------------------+
       a  b  c  d  e  f  g

Board size = 7
Tour length = 49
Tie Breaker = procedure RothTieBreaker
       a  b  c  d  e  f  g
    +----------------------+
  7 | 35 14 21 46  7 12  9 |  7
  6 | 20 49 34 13 10 23  6 |  6
  5 | 15 36 45 22 47  8 11 |  5
  4 | 42 19 48 33 40  5 24 |  4
  3 | 37 16 41 44 27 32 29 |  3
  2 | 18 43  2 39 30 25  4 |  2
  1 |  1 38 17 26  3 28 31 |  1
    +----------------------+
       a  b  c  d  e  f  g
```



## J

'''Solution:'''

[[j:Essays/Knight's Tour|The Knight's tour essay on the Jwiki]] shows a couple of solutions including one using [[wp:Knight's_tour#Warnsdorff.27s_algorithm|Warnsdorffs algorithm]].

```j
NB. knight moves for each square of a (y,y) board
kmoves=: monad define
 t=. (>,{;~i.y) +"1/ _2]\2 1 2 _1 1 2 1 _2 _1 2 _1 _2 _2 1 _2 _1
 (*./"1 t e. i.y) <@#"1 y#.t
)

ktourw=: monad define
 M=. >kmoves y
 p=. k=. 0
 b=. 1 $~ *:y
 for. i.<:*:y do.
  b=. 0 k}b
  p=. p,k=. ((i.<./) +/"1 b{~j{M){j=. ({&b # ]) k{M
 end.
 assert. ~:p
 (,~y)$/:p
)
```


'''Example Use:'''

```j
   ktourw 8    NB. solution for an 8 x 8 board
 0 25 14 23 28 49 12 31
15 22 27 50 13 30 63 48
26  1 24 29 62 59 32 11
21 16 51 58 43 56 47 60
 2 41 20 55 52 61 10 33
17 38 53 42 57 44  7 46
40  3 36 19 54  5 34  9
37 18 39  4 35  8 45  6

   9!:37]0 64 4 4  NB. truncate lines longer than 64 characters and only show first and last four lines

   ktourw 202 NB. 202x202 board -- this implementation failed for 200 and 201
    0   401   414   405   398   403   424   417   396   419   43...
  413   406   399   402   425   416   397   420   439   430   39...
  400     1   426   415   404   423   448   429   418   437 4075...
  409   412   407   446   449   428   421   440 40739 40716   43...
...
  550    99   560   569  9992   779   786   773 10002  9989   78...
  555   558   553   778   563   570   775   780   785   772 1000...
  100   551   556   561   102   777   572   771   104   781   57...
  557   554   101   552   571   562   103   776   573   770   10...
```



## Java

```java
import java.util.*;

public class KnightsTour {
    private final static int base = 12;
    private final static int[][] moves = {{1,-2},{2,-1},{2,1},{1,2},{-1,2},
        {-2,1},{-2,-1},{-1,-2}};
    private static int[][] grid;
    private static int total;

    public static void main(String[] args) {
        grid = new int[base][base];
        total = (base - 4) * (base - 4);

        for (int r = 0; r < base; r++)
            for (int c = 0; c < base; c++)
                if (r < 2 || r > base - 3 || c < 2 || c > base - 3)
                    grid[r][c] = -1;

        int row = 2 + (int) (Math.random() * (base - 4));
        int col = 2 + (int) (Math.random() * (base - 4));

        grid[row][col] = 1;

        if (solve(row, col, 2))
            printResult();
        else System.out.println("no result");

    }

    private static boolean solve(int r, int c, int count) {
        if (count > total)
            return true;

        List<int[]> nbrs = neighbors(r, c);

        if (nbrs.isEmpty() && count != total)
            return false;

        Collections.sort(nbrs, new Comparator<int[]>() {
            public int compare(int[] a, int[] b) {
                return a[2] - b[2];
            }
        });

        for (int[] nb : nbrs) {
            r = nb[0];
            c = nb[1];
            grid[r][c] = count;
            if (!orphanDetected(count, r, c) && solve(r, c, count + 1))
                return true;
            grid[r][c] = 0;
        }

        return false;
    }

    private static List<int[]> neighbors(int r, int c) {
        List<int[]> nbrs = new ArrayList<>();

        for (int[] m : moves) {
            int x = m[0];
            int y = m[1];
            if (grid[r + y][c + x] == 0) {
                int num = countNeighbors(r + y, c + x);
                nbrs.add(new int[]{r + y, c + x, num});
            }
        }
        return nbrs;
    }

    private static int countNeighbors(int r, int c) {
        int num = 0;
        for (int[] m : moves)
            if (grid[r + m[1]][c + m[0]] == 0)
                num++;
        return num;
    }

    private static boolean orphanDetected(int cnt, int r, int c) {
        if (cnt < total - 1) {
            List<int[]> nbrs = neighbors(r, c);
            for (int[] nb : nbrs)
                if (countNeighbors(nb[0], nb[1]) == 0)
                    return true;
        }
        return false;
    }

    private static void printResult() {
        for (int[] row : grid) {
            for (int i : row) {
                if (i == -1) continue;
                System.out.printf("%2d ", i);
            }
            System.out.println();
        }
    }
}
```


```txt
34 17 20  3 36  7 22  5
19  2 35 40 21  4 37  8
16 33 18 51 44 39  6 23
 1 50 43 46 41 56  9 38
32 15 54 61 52 45 24 57
49 62 47 42 55 60 27 10
14 31 64 53 12 29 58 25
63 48 13 30 59 26 11 28
```

===More efficient non-trackback solution===
<lang>
package com.knight.tour;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class KT {

	private int baseSize = 12; // virtual board size including unreachable out-of-board nodes. i.e. base 12 = 8X8 board
	int actualBoardSize = baseSize - 4;
	private static final int[][] moves = { { 1, -2 }, { 2, -1 }, { 2, 1 }, { 1, 2 }, { -1, 2 }, { -2, 1 }, { -2, -1 },
			{ -1, -2 } };
	private static int[][] grid;
	private static int totalNodes;
	private ArrayList<int[]> travelledNodes = new ArrayList<>();
	public KT(int baseNumber) {
		this.baseSize = baseNumber;
		this.actualBoardSize = baseSize - 4;
	}

	public static void main(String[] args) {
		new KT(12).tour(); // find a solution for 8X8 board
//		new KT(24).tour(); // then for 20X20 board
//		new KT(104).tour(); // then for 100X100 board
	}

	private void tour() {
		totalNodes = actualBoardSize * actualBoardSize;
		travelledNodes.clear();
		grid = new int[baseSize][baseSize];
		for (int r = 0; r < baseSize; r++)
			for (int c = 0; c < baseSize; c++) {
				if (r < 2 || r > baseSize - 3 || c < 2 || c > baseSize - 3) {
					grid[r][c] = -1; // mark as out-of-board nodes
				} else {
					grid[r][c] = 0; // nodes within chess board.
				}
			}
		// start from a random node
		int startRow = 2 + (int) (Math.random() * actualBoardSize);
		int startCol = 2 + (int) (Math.random() * actualBoardSize);
		int[] start = { startRow, startCol, 0, 1 };
		grid[startRow][startCol] = 1; // mark the first traveled node
		travelledNodes.add(start); // add to partial solution chain, which will only have one node.

		// Start traveling forward
		autoKnightTour(start, 2);
	}

	// non-backtracking touring methods. Re-chain the partial solution when all neighbors are traveled to avoid back-tracking.
	private void autoKnightTour(int[] start, int nextCount) {
		List<int[]> nbrs = neighbors(start[0], start[1]);
		if (nbrs.size() > 0) {
			Collections.sort(nbrs, new Comparator<int[]>() {
				public int compare(int[] a, int[] b) {
					return a[2] - b[2];
				}
			}); // sort the list
			int[] next = nbrs.get(0); // the one with the less available neighbors - Warnsdorff's algorithm
			next[3] = nextCount;
			travelledNodes.add(next);
			grid[next[0]][next[1]] = nextCount;
			if (travelledNodes.size() == totalNodes) {
				System.out.println("Found a path for " + actualBoardSize + " X " + actualBoardSize + " chess board.");
				StringBuilder sb = new StringBuilder();
				sb.append(System.lineSeparator());
				for (int idx = 0; idx < travelledNodes.size(); idx++) {
					int[] item = travelledNodes.get(idx);
					sb.append("->(" + (item[0] - 2) + "," + (item[1] - 2) + ")");
					if ((idx + 1) % 15 == 0) {
						sb.append(System.lineSeparator());
					}
				}
				System.out.println(sb.toString() + "\n");
			} else { // continuing the travel
				autoKnightTour(next, ++nextCount);
			}
		} else { // no travelable neighbors next - need to rechain the partial chain
			int[] last = travelledNodes.get(travelledNodes.size() - 1);
			travelledNodes = reChain(travelledNodes);
			if (travelledNodes.get(travelledNodes.size() - 1).equals(last)) {
				travelledNodes = reChain(travelledNodes);
				if (travelledNodes.get(travelledNodes.size() - 1).equals(last)) {
					System.out.println("Re-chained twice but no travllable node found. Quiting...");
				} else {
					int[] end = travelledNodes.get(travelledNodes.size() - 1);
					autoKnightTour(end, nextCount);
				}
			} else {
				int[] end = travelledNodes.get(travelledNodes.size() - 1);
				autoKnightTour(end, nextCount);
			}
		}
	}

	private ArrayList<int[]> reChain(ArrayList<int[]> alreadyTraveled) {
		int[] last = alreadyTraveled.get(alreadyTraveled.size() - 1);
		List<int[]> candidates = neighborsInChain(last[0], last[1]);
		int cutIndex;
		int[] randomPicked = candidates.get((int) Math.random() * candidates.size());
		cutIndex = grid[randomPicked[0]][randomPicked[1]] - 1;
		ArrayList<int[]> result = new ArrayList<int[]>(); //create empty list to copy already traveled nodes to
		for (int k = 0; k <= cutIndex; k++) {
			result.add(result.size(), alreadyTraveled.get(k));
		}
		for (int j = alreadyTraveled.size() - 1; j > cutIndex; j--) {
			alreadyTraveled.get(j)[3] = result.size();
			result.add(result.size(), alreadyTraveled.get(j));
		}
		return result; // re-chained partial solution with different end node
	}

	private List<int[]> neighborsInChain(int r, int c) {
		List<int[]> nbrs = new ArrayList<>();
		for (int[] m : moves) {
			int x = m[0];
			int y = m[1];
			if (grid[r + y][c + x] > 0 && grid[r + y][c + x] != grid[r][c] - 1) {
				int num = countNeighbors(r + y, c + x);
				nbrs.add(new int[] { r + y, c + x, num, 0 });
			}
		}
		return nbrs;
	}

	private static List<int[]> neighbors(int r, int c) {
		List<int[]> nbrs = new ArrayList<>();
		for (int[] m : moves) {
			int x = m[0];
			int y = m[1];
			if (grid[r + y][c + x] == 0) {
				int num = countNeighbors(r + y, c + x);
				nbrs.add(new int[] { r + y, c + x, num, 0 }); // not-traveled neighbors and number of their neighbors
			}
		}
		return nbrs;

	}

	private List<int[]> extendableNeighbors(List<int[]> neighbors) {
		List<int[]> nbrs = new ArrayList<>();
		for (int[] node : neighbors) {
			if (node[2] > 0)
				nbrs.add(node);
		}
		return nbrs;
	}

	private static int countNeighbors(int r, int c) {
		int num = 0;
		for (int[] m : moves) {
			if (grid[r + m[1]][c + m[0]] == 0) {
				num++;
			}
		}
		return num;
	}
}

```


```txt

Found a path for 8 X 8 chess board.

->(2,1)->(0,0)->(1,2)->(0,4)->(1,6)->(3,7)->(5,6)->(7,7)->(6,5)->(5,7)->(7,6)->(6,4)->(7,2)->(6,0)->(4,1)
->(2,0)->(0,1)->(1,3)->(0,5)->(1,7)->(3,6)->(2,4)->(0,3)->(1,1)->(3,0)->(2,2)->(1,0)->(0,2)->(1,4)->(0,6)
->(2,7)->(1,5)->(0,7)->(2,6)->(4,7)->(6,6)->(4,5)->(3,3)->(2,5)->(4,6)->(6,7)->(7,5)->(5,4)->(3,5)->(2,3)
->(4,4)->(3,2)->(4,0)->(5,2)->(7,3)->(6,1)->(5,3)->(3,4)->(4,2)->(6,3)->(7,1)->(5,0)->(3,1)->(4,3)->(5,5)
->(7,4)->(6,2)->(7,0)->(5,1)

```



## Javascript


### Procedural

Using Warnsdorff rule and Backtracking.

You can test it [http://paulo-jorente.de/webgames/repos/knightsTour/ here].


```javascript

class KnightTour {
 constructor() {
  this.width = 856;
  this.height = 856;
  this.cellCount = 8;
  this.size = 0;
  this.knightPiece = "\u2658";
  this.knightPos = {
   x: 0,
   y: 0
  };
  this.ctx = null;
  this.step = this.width / this.cellCount;
  this.lastTime = 0;
  this.wait;
  this.delay;
  this.success;
  this.jumps;
  this.directions = [];
  this.visited = [];
  this.path = [];
  document.getElementById("start").addEventListener("click", () => {
   this.startHtml();
  });
  this.init();
  this.drawBoard();
 }

 drawBoard() {
  let a = false, xx, yy;
  for (let y = 0; y < this.cellCount; y++) {
   for (let x = 0; x < this.cellCount; x++) {
    if (a) {
     this.ctx.fillStyle = "#607db8";
    } else {
     this.ctx.fillStyle = "#aecaf0";
    }
    a = !a;
    xx = x * this.step;
    yy = y * this.step;
    this.ctx.fillRect(xx, yy, xx + this.step, yy + this.step);
   }
   if (!(this.cellCount & 1)) a = !a;
  }
  if (this.path.length) {
   const s = this.step >> 1;
   this.ctx.lineWidth = 3;
   this.ctx.fillStyle = "black";
   this.ctx.beginPath();
   this.ctx.moveTo(this.step * this.knightPos.x + s, this.step * this.knightPos.y + s);
   let a, b, v = this.path.length - 1;
   for (; v > -1; v--) {
    a = this.path[v].pos.x * this.step + s;
    b = this.path[v].pos.y * this.step + s;
    this.ctx.lineTo(a, b);
    this.ctx.fillRect(a - 5, b - 5, 10, 10);
   }
   this.ctx.stroke();
  }
 }

 createMoves(pos) {
  const possibles = [];
  let x = 0,
   y = 0,
   m = 0,
   l = this.directions.length;
  for (; m < l; m++) {
   x = pos.x + this.directions[m].x;
   y = pos.y + this.directions[m].y;
   if (x > -1 && x < this.cellCount && y > -1 && y < this.cellCount && !this.visited[x + y * this.cellCount]) {
    possibles.push({
     x,
     y
    })
   }
  }
  return possibles;
 }

 warnsdorff(pos) {
  const possibles = this.createMoves(pos);
  if (possibles.length < 1) return [];
  const moves = [];
  for (let p = 0, l = possibles.length; p < l; p++) {
   let ps = this.createMoves(possibles[p]);
   moves.push({
    len: ps.length,
    pos: possibles[p]
   });
  }
  moves.sort((a, b) => {
   return b.len - a.len;
  });
  return moves;
 }

 startHtml() {
  this.cellCount = parseInt(document.getElementById("cellCount").value);
  this.size = Math.floor(this.width / this.cellCount)
  this.wait = this.delay = parseInt(document.getElementById("delay").value);
  this.step = this.width / this.cellCount;
  this.ctx.font = this.size + "px Arial";
  document.getElementById("log").innerText = "";
  document.getElementById("path").innerText = "";
  this.path = [];
  this.jumps = 1;
  this.success = true;
  this.visited = [];
  const cnt = this.cellCount * this.cellCount;
  for (let a = 0; a < cnt; a++) {
   this.visited.push(false);
  }
  const kx = parseInt(document.getElementById("knightx").value),
   ky = parseInt(document.getElementById("knighty").value);
  this.knightPos = {
   x: (kx > this.cellCount || kx < 0) ? Math.floor(Math.random() * this.cellCount) : kx,
   y: (ky > this.cellCount || ky < 0) ? Math.floor(Math.random() * this.cellCount) : ky
  };
  this.mainLoop = (time = 0) => {
   const dif = time - this.lastTime;
   this.lastTime = time;
   this.wait -= dif;
   if (this.wait > 0) {
    requestAnimationFrame(this.mainLoop);
    return;
   }
   this.wait = this.delay;
   let moves;
   if (this.success) {
    moves = this.warnsdorff(this.knightPos);
   } else {
    if (this.path.length > 0) {
     const path = this.path[this.path.length - 1];
     moves = path.m;
     if (moves.length < 1) this.path.pop();
     this.knightPos = path.pos
     this.visited[this.knightPos.x + this.knightPos.y * this.cellCount] = false;
     this.jumps--;
     this.wait = this.delay;
    } else {
     document.getElementById("log").innerText = "Can't find a solution!";
     return;
    }
   }
   this.drawBoard();
   const ft = this.step - (this.step >> 3);
   this.ctx.fillStyle = "#000";
   this.ctx.fillText(this.knightPiece, this.knightPos.x * this.step, this.knightPos.y * this.step + ft);
   if (moves.length < 1) {
    if (this.jumps === this.cellCount * this.cellCount) {
     document.getElementById("log").innerText = "Tour finished!";
     let str = "";
     for (let z of this.path) {
      str += `${1 + z.pos.x + z.pos.y * this.cellCount}, `;
     }
     str += `${1 + this.knightPos.x + this.knightPos.y * this.cellCount}`;
     document.getElementById("path").innerText = str;
     return;
    } else {
     this.success = false;
    }
   } else {
    this.visited[this.knightPos.x + this.knightPos.y * this.cellCount] = true;
    const move = moves.pop();
    this.path.push({
     pos: this.knightPos,
     m: moves
    });
    this.knightPos = move.pos
    this.success = true;
    this.jumps++;
   }
   requestAnimationFrame(this.mainLoop);
  };
  this.mainLoop();
 }

 init() {
  const canvas = document.createElement("canvas");
  canvas.id = "cv";
  canvas.width = this.width;
  canvas.height = this.height;
  this.ctx = canvas.getContext("2d");
  document.getElementById("out").appendChild(canvas);
  this.directions = [{
    x: -1,
    y: -2
   }, {
    x: -2,
    y: -1
   }, {
    x: 1,
    y: -2
   }, {
    x: 2,
    y: -1
   },
   {
    x: -1,
    y: 2
   }, {
    x: -2,
    y: 1
   }, {
    x: 1,
    y: 2
   }, {
    x: 2,
    y: 1
   }
  ];
 }
}
new KnightTour();

```

To test it, you'll need an index.html

```txt

<!DOCTYPE html>
<html>
<head>
 <meta charset="UTF-8">
 <title>Knight's Tour</title>
 <link rel="stylesheet" type="text/css" media="screen" href="style.css" />
</head>
<body>
 <div id='out'></div>
 <div id='ctrls'>
  <span>Cells: </span><input id="cellCount" value="8" type="number" max="250" min="5"><br />
  <span>Delay: </span><input id="delay" value="500" type="number" max="2000" min="0"><br />
  <span>Knight X: </span><input id="knightx" value="-1" type="number" max="250" min="-1"><br />
  <span>Knight Y: </span><input id="knighty" value="-1" type="number" max="250" min="-1"><br />
  <button id="start">Start</button>
  <div id='log'></div>
  <div id="path"></div>
 </div>
 <script src="tour_bt.js" type="module"></script>
</body>
</html>

```

And a style.css

```txt

body {
  font-family: verdana;
  color: white;
  font-size: 36px;
  background-color: #001f33
}
button {
  width: 100%;
  height: 40px;
  margin: 20px 0px 20px 0px;
  font-size: 28px
}
canvas {
  border: 4px solid #000;
  margin: 40px;
}
#out {
  float: left;
}
#ctrls {
  margin-top: 40px;
  text-align: left;
  width: 280px;
  line-height: 40px;
  float: left;
}
#ctrls input {
  float: right;
  width: 80px;
  height: 24px;
  margin-top: 6px;
  font-size: 22px;
}
#path {
  margin-top: 10px;
  font-size: 12px;
  line-height: 16px;
}
```



### Functional


A composition of values, drawing on generic abstractions:
```javascript
(() => {
    'use strict';

    // knightsTour :: Int -> [(Int, Int)] -> [(Int, Int)]
    const knightsTour = rowLength => moves => {
        const go = path => {
            const
                findMoves = xy => difference(knightMoves(xy), path),
                warnsdorff = minimumBy(
                    comparing(compose(length, findMoves))
                ),
                options = findMoves(path[0]);
            return 0 < options.length ? (
                go([warnsdorff(options)].concat(path))
            ) : reverse(path);
        };

        // board :: [[(Int, Int)]]
        const board = concatMap(
            col => concatMap(
                row => [
                    [col, row]
                ],
                enumFromTo(1, rowLength)),
            enumFromTo(1, rowLength)
        );

        // knightMoves :: (Int, Int) -> [(Int, Int)]
        const knightMoves = ([x, y]) =>
            concatMap(
                ([dx, dy]) => {
                    const ab = [x + dx, y + dy];
                    return elem(ab, board) ? (
                        [ab]
                    ) : [];
                }, [
                    [-2, -1],
                    [-2, 1],
                    [-1, -2],
                    [-1, 2],
                    [1, -2],
                    [1, 2],
                    [2, -1],
                    [2, 1]
                ]
            );
        return go(moves);
    };

    // TEST -----------------------------------------------
    // main :: IO()
    const main = () => {

        // boardSize :: Int
        const boardSize = 8;

        // tour :: [(Int, Int)]
        const tour = knightsTour(boardSize)(
            [fromAlgebraic('e5')]
        );

        // report :: String
        const report = '(Board size ' +
            boardSize + '*' + boardSize + ')\n\n' +
            'Route: \n\n' +
            showRoute(boardSize)(tour) + '\n\n' +
            'Coverage and order: \n\n' +
            showCoverage(boardSize)(tour) + '\n\n';
        return (
            console.log(report),
            report
        );
    }

    // DISPLAY --------------------------------------------

    // algebraic :: (Int, Int) -> String
    const algebraic = ([x, y]) =>
        chr(x + 96) + y.toString();

    // fromAlgebraic :: String -> (Int, Int)
    const fromAlgebraic = s =>
        2 <= s.length ? (
            [ord(s[0]) - 96, parseInt(s.slice(1))]
        ) : undefined;

    // showCoverage :: Int -> [(Int, Int)] -> String
    const showCoverage = rowLength => xys => {
        const
            intMax = xys.length,
            w = 1 + intMax.toString().length
        return unlines(map(concat,
            chunksOf(
                rowLength,
                map(composeList([justifyRight(w, ' '), str, fst]),
                    sortBy(
                        mappendComparing([
                            compose(fst, snd),
                            compose(snd, snd)
                        ]),
                        zip(enumFromTo(1, intMax), xys)
                    )
                )
            )
        ));
    };

    // showRoute :: Int -> [(Int, Int)] -> String
    const showRoute = rowLength => xys => {
        const w = 1 + rowLength.toString().length;
        return unlines(map(
            xs => xs.join(' -> '),
            chunksOf(
                rowLength,
                map(compose(justifyRight(w, ' '), algebraic), xys)
            )
        ));
    };


    // GENERIC FUNCTIONS ----------------------------------


    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // chr :: Int -> Char
    const chr = x => String.fromCodePoint(x);

    // chunksOf :: Int -> [a] -> [[a]]
    const chunksOf = (n, xs) =>
        enumFromThenTo(0, n, xs.length - 1)
        .reduce(
            (a, i) => a.concat([xs.slice(i, (n + i))]),
            []
        );

    // compare :: a -> a -> Ordering
    const compare = (a, b) =>
        a < b ? -1 : (a > b ? 1 : 0);

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
        };

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (f, g) => x => f(g(x));

    // composeList :: [(a -> a)] -> (a -> a)
    const composeList = fs =>
        x => fs.reduceRight((a, f) => f(a), x, fs);

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        xs.reduce((a, x) => a.concat(f(x)), []);


    // difference :: Eq a => [a] -> [a] -> [a]
    const difference = (xs, ys) => {
        const s = new Set(ys.map(str));
        return xs.filter(x => !s.has(str(x)));
    };

    // elem :: Eq a => a -> [a] -> Bool
    const elem = (x, xs) => xs.some(eq(x))


    // enumFromThenTo :: Int -> Int -> Int -> [Int]
    const enumFromThenTo = (x1, x2, y) => {
        const d = x2 - x1;
        return Array.from({
            length: Math.floor(y - x2) / d + 2
        }, (_, i) => x1 + (d * i));
    };

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        Array.from({
            length: 1 + n - m
        }, (_, i) => m + i);

    // eq (==) :: Eq a => a -> a -> Bool
    const eq = a => b => {
        const t = typeof a;
        return t !== typeof b ? (
            false
        ) : 'object' !== t ? (
            'function' !== t ? (
                a === b
            ) : a.toString() === b.toString()
        ) : (() => {
            const kvs = Object.entries(a);
            return kvs.length !== Object.keys(b).length ? (
                false
            ) : kvs.every(([k, v]) => eq(v)(b[k]));
        })();
    };

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // justifyRight :: Int -> Char -> String -> String
    const justifyRight = (n, cFiller) => s =>
        n > s.length ? (
            s.padStart(n, cFiller)
        ) : s;


    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) =>
        (Array.isArray(xs) ? (
            xs
        ) : xs.split('')).map(f);

    // mappendComparing :: [(a -> b)] -> (a -> a -> Ordering)
    const mappendComparing = fs =>
        (x, y) => fs.reduce(
            (ordr, f) => (ordr || compare(f(x), f(y))),
            0
        );

    // minimumBy :: (a -> a -> Ordering) -> [a] -> a
    const minimumBy = f => xs =>
        xs.reduce((a, x) => undefined === a ? x : (
            0 > f(x, a) ? x : a
        ), undefined);

    // ord :: Char -> Int
    const ord = c => c.codePointAt(0);

    // reverse :: [a] -> [a]
    const reverse = xs =>
        'string' !== typeof xs ? (
            xs.slice(0).reverse()
        ) : xs.split('').reverse().join('');

    // snd :: (a, b) -> b
    const snd = tpl => tpl[1];

    // sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    const sortBy = (f, xs) =>
        xs.slice()
        .sort(f);

    // str :: a -> String
    const str = x => x.toString();

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        xs.slice(0, n);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // Use of `take` and `length` here allows for zipping with non-finite
    // lists - i.e. generators like cycle, repeat, iterate.

    // zip :: [a] -> [b] -> [(a, b)]
    const zip = (xs, ys) => {
        const lng = Math.min(length(xs), length(ys));
        const bs = take(lng, ys);
        return take(lng, xs).map((x, i) => Tuple(x, bs[i]));
    };

    // MAIN ---
    return main();
})();
```

```txt
(Board size 8*8)

Route:

e5 -> d7 -> b8 -> a6 -> b4 -> a2 -> c1 -> b3
a1 -> c2 -> a3 -> b1 -> d2 -> f1 -> h2 -> g4
h6 -> g8 -> e7 -> c8 -> a7 -> c6 -> a5 -> b7
d8 -> f7 -> h8 -> g6 -> f8 -> h7 -> f6 -> e8
g7 -> h5 -> g3 -> h1 -> f2 -> d1 -> b2 -> a4
b6 -> a8 -> c7 -> b5 -> c3 -> d5 -> e3 -> c4
d6 -> e4 -> c5 -> d3 -> e1 -> g2 -> h4 -> f5
d4 -> e2 -> f4 -> e6 -> g5 -> f3 -> g1 -> h3

Coverage and order:

  9  6 11 40 23  4 21 42
 12 39  8  5 44 41 24  3
  7 10 45 48 51 22 43 20
 38 13 52 57 46 49  2 25
 53 58 47 50  1 60 19 32
 14 37 62 59 56 31 26 29
 63 54 35 16 61 28 33 18
 36 15 64 55 34 17 30 27

```



## Julia

Uses the Hidato puzzle solver module, which has its source code listed [[Solve_a_Hidato_puzzle#Julia | here]]  in the Hadato task.

```julia
using .Hidato       # Note that the . here means to look locally for the module rather than in the libraries

const chessboard = """
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0 """

const knightmoves = [[-2, -1], [-2, 1], [-1, -2], [-1, 2], [1, -2], [1, 2], [2, -1], [2, 1]]

board, maxmoves, fixed, starts = hidatoconfigure(chessboard)
printboard(board, " 0", "  ")
hidatosolve(board, maxmoves, knightmoves, fixed, starts[1][1], starts[1][2], 1)
printboard(board)

```
```txt

 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0
 0 0 0 0 0 0 0 0

  1 12  9  6  3 14 17 20
 10  7  2 13 18 21  4 15
 31 28 11  8  5 16 19 22
 64 25 32 29 36 23 48 45
 33 30 27 24 49 46 37 58
 26 63 52 35 40 57 44 47
 53 34 61 50 55 42 59 38
 62 51 54 41 60 39 56 43

```





## Kotlin

```scala
data class Square(val x : Int, val y : Int)

val board = Array(8 * 8, { Square(it / 8 + 1, it % 8 + 1) })
val axisMoves = arrayOf(1, 2, -1, -2)

fun <T> allPairs(a: Array<T>) = a.flatMap { i -> a.map { j -> Pair(i, j) } }

fun knightMoves(s : Square) : List<Square> {
    val moves = allPairs(axisMoves).filter{ Math.abs(it.first) != Math.abs(it.second) }
    fun onBoard(s : Square) = board.any {it == s}
    return moves.map { Square(s.x + it.first, s.y + it.second) }.filter(::onBoard)
}

fun knightTour(moves : List<Square>) : List<Square> {
    fun findMoves(s: Square) = knightMoves(s).filterNot { m -> moves.any { it == m } }
    val newSquare = findMoves(moves.last()).minBy { findMoves(it).size }
    return if (newSquare == null) moves else knightTour(moves + newSquare)
}

fun knightTourFrom(start : Square) = knightTour(listOf(start))

fun main(args : Array<String>) {
    var col = 0
    for ((x, y) in knightTourFrom(Square(1, 1))) {
        System.out.print("$x,$y")
        System.out.print(if (col == 7) "\n" else " ")
        col = (col + 1) % 8
    }
}
```


```txt
1,1 2,3 3,1 1,2 2,4 1,6 2,8 4,7
6,8 8,7 7,5 8,3 7,1 5,2 7,3 8,1
6,2 4,1 2,2 1,4 2,6 1,8 3,7 5,8
7,7 8,5 6,6 7,8 8,6 7,4 8,2 6,1
4,2 2,1 3,3 5,4 3,5 4,3 5,1 6,3
8,4 7,2 6,4 5,6 4,8 2,7 1,5 3,6
1,7 3,8 5,7 4,5 5,3 6,5 4,4 3,2
1,3 2,5 4,6 3,4 5,5 6,7 8,8 7,6
```



## Locomotive Basic


Influenced by the Python version, although computed tours are different.


```locobasic
10 mode 1:defint a-z
20 input "Board size: ",size
30 input "Start position: ",a$
40 x=asc(mid$(a$,1,1))-96
50 y=val(mid$(a$,2,1))
60 dim play(size,size)
70 for q=1 to 8
80 read dx(q),dy(q)
90 next
100 data 2,1,1,2,-1,2,-2,1,-2,-1,-1,-2,1,-2,2,-1
110 pen 0:paper 1
120 for q=1 to size
130 locate 3*q+1,24-size
140 print chr$(96+q);
150 locate 3*(size+1)+1,26-q
160 print using "#"; q;
170 next
180 pen 1:paper 0
190 ' main loop
200 n=n+1
210 play(x,y)=n
220 locate 3*x,26-y
230 print using "##"; n;
240 if n=size*size then call &bb06:end
250 nmov=100
260 for q=1 to 8
270 xc=x+dx(q)
280 yc=y+dy(q)
290 gosub 360
300 if nm<nmov then nmov=nm:qm=q
310 next
320 x=x+dx(qm)
330 y=y+dy(qm)
340 goto 200
350 ' find moves
360 if xc<1 or yc<1 or xc>size or yc>size then nm=1000:return
370 if play(xc,yc) then nm=2000:return
380 nm=0
390 for q2=1 to 8
400 xt=xc+dx(q2)
410 yt=yc+dy(q2)
420 if xt<1 or yt<1 or xt>size or yt>size then 460
430 if play(xt,yt) then 460
440 nm=nm+1
450 ' skip this move
460 next
470 return
```


[[File:Knights tour Locomotive Basic.png]]


## Lua


```lua
N = 8

moves = { {1,-2},{2,-1},{2,1},{1,2},{-1,2},{-2,1},{-2,-1},{-1,-2} }

function Move_Allowed( board, x, y )
    if board[x][y] >= 8 then return false end

    local new_x, new_y = x + moves[board[x][y]+1][1], y + moves[board[x][y]+1][2]
    if new_x >= 1 and new_x <= N and new_y >= 1 and new_y <= N and board[new_x][new_y] == 0 then return true end

    return false
end


board = {}
for i = 1, N do
    board[i] = {}
    for j = 1, N do
        board[i][j] = 0
    end
end

x, y = 1, 1

lst = {}
lst[1] = { x, y }

repeat
    if Move_Allowed( board, x, y ) then
        board[x][y] = board[x][y] + 1
        x, y = x+moves[board[x][y]][1], y+moves[board[x][y]][2]
        lst[#lst+1] = { x, y }
    else
        if board[x][y] >= 8 then
            board[x][y] = 0
            lst[#lst] = nil
            if #lst == 0 then
                print "No solution found."
                os.exit(1)
            end
            x, y = lst[#lst][1], lst[#lst][2]
        end
        board[x][y] = board[x][y] + 1
    end
until #lst == N^2

last = lst[1]
for i = 2, #lst do
    print( string.format( "%s%d - %s%d", string.sub("ABCDEFGH",last[1],last[1]), last[2], string.sub("ABCDEFGH",lst[i][1],lst[i][1]), lst[i][2] ) )
    last = lst[i]
end
```



## Mathematica


'''Solution'''

```Mathematica

knightsTourMoves[start_] :=
  Module[{
    vertexLabels = (# -> ToString@c[[Quotient[# - 1, 8] + 1]] <>  ToString[Mod[# - 1, 8] + 1]) & /@ Range[64], knightsGraph,
       hamiltonianCycle, end},
    knightsGraph = KnightTourGraph[i, i, VertexLabels -> vertexLabels,  ImagePadding -> 15];
    hamiltonianCycle = ((FindHamiltonianCycle[knightsGraph] /. UndirectedEdge -> DirectedEdge) /. labels)[[1]];
    end = Cases[hamiltonianCycle, (x_ \[DirectedEdge] start) :> x][[1]];
    FindShortestPath[g, start, end]]

```


'''Usage'''

```Mathematica

 knightsTourMoves["d8"]

(* out *)
  {"d8", "e6", "d4", "c2", "a1", "b3", "a5", "b7", "c5", "a4", "b2", "c4", "a3", "b1", "c3", "a2", "b4", "a6", "b8", "c6", "a7", "b5", \
"c7", "a8", "b6", "c8", "d6", "e4", "d2", "f1", "e3", "d1", "f2", "h1", "g3", "e2", "c1", "d3", "e1", "g2", "h4", "f5", "e7", "d5", \
"f4", "h5", "g7", "e8", "f6", "g8", "h6", "g4", "h2", "f3", "g1", "h3", "g5", "h7", "f8", "d7", "e5", "g6", "h8", "f7"}

```


'''Analysis'''

'''vertexLabels''' replaces the default vertex (i.e. square) names of the chessboard with the standard algebraic names "a1", "a2",...,"h8".

```Mathematica

  vertexLabels = (# -> ToString@c[[Quotient[# - 1, 8] + 1]] <>  ToString[Mod[# - 1, 8] + 1]) & /@ Range[64]

(* out *)
 {1 -> "a1", 2 -> "a2", 3 -> "a3", 4 -> "a4", 5 -> "a5", 6 -> "a6", 7 -> "a7", 8 -> "a8",
  9 -> "b1", 10 -> "b2", 11 -> "b3", 12 -> "b4", 13 -> "b5", 14 -> "b6", 15 -> "b7", 16 -> "b8",
 17 -> "c1", 18 -> "c2", 19 -> "c3", 20 -> "c4", 21 -> "c5", 22 -> "c6", 23 -> "c7", 24 -> "c8",
 25 -> "d1", 26 -> "d2", 27 -> "d3",  28 -> "d4", 29 -> "d5", 30 -> "d6", 31 -> "d7", 32 -> "d8",
 33 -> "e1", 34 -> "e2", 35 -> "e3", 36 -> "e4", 37 -> "e5", 38 -> "e6", 39 -> "e7", 40 -> "e8",
 41 -> "f1", 42 -> "f2", 43 -> "f3", 44 -> "f4", 45 -> "f5", 46 -> "f6", 47 -> "f7", 48 -> "f8",
 49 -> "g1", 50 -> "g2", 51 -> "g3", 52 -> "g4", 53 -> "g5", 54 -> "g6",55 -> "g7", 56 -> "g8",
 57 -> "h1", 58 -> "h2", 59 -> "h3", 60 -> "h4", 61 -> "h5", 62 -> "h6",  63 -> "h7", 64 -> "h8"}


```


'''knightsGraph''' creates a graph of the solution space.

```Mathematica

knightsGraph = KnightTourGraph[i, i, VertexLabels -> vertexLabels,  ImagePadding -> 15];

```

[[File:KnightsTour-3.png]]

Find a Hamiltonian cycle (a path that visits each square exactly one time.)


```Mathematica

    hamiltonianCycle = ((FindHamiltonianCycle[knightsGraph] /. UndirectedEdge -> DirectedEdge) /. labels)[[1]];

```


Find the end square:


```Mathematica

    end = Cases[hamiltonianCycle, (x_ \[DirectedEdge] start) :> x][[1]];

```


Find shortest path from the start square to the end square.


```Mathematica

   FindShortestPath[g, start, end]]

```



## Mathprog

While a little slower than using Warnsdorff this solution is interesting:

1. It shows that [[Hidato]] and Knights Tour are essentially the same problem.

2. It is possible to specify which square is used for any Knights Move.

<lang>
/*Knights.mathprog

  Find a Knights Tour

  Nigel_Galloway
  January 11th., 2012
*/

param ZBLS;
param ROWS;
param COLS;
param D := 2;
set ROWSR := 1..ROWS;
set COLSR := 1..COLS;
set ROWSV := (1-D)..(ROWS+D);
set COLSV := (1-D)..(COLS+D);
param Iz{ROWSR,COLSR}, integer, default 0;
set ZBLSV := 1..(ZBLS+1);
set ZBLSR := 1..ZBLS;

var BR{ROWSV,COLSV,ZBLSV}, binary;

void0{r in ROWSV, z in ZBLSR,c in (1-D)..0}: BR[r,c,z] = 0;
void1{r in ROWSV, z in ZBLSR,c in (COLS+1)..(COLS+D)}: BR[r,c,z] = 0;
void2{c in COLSV, z in ZBLSR,r in (1-D)..0}: BR[r,c,z] = 0;
void3{c in COLSV, z in ZBLSR,r in (ROWS+1)..(ROWS+D)}: BR[r,c,z] = 0;
void4{r in ROWSV,c in (1-D)..0}: BR[r,c,ZBLS+1] = 1;
void5{r in ROWSV,c in (COLS+1)..(COLS+D)}: BR[r,c,ZBLS+1] = 1;
void6{c in COLSV,r in (1-D)..0}: BR[r,c,ZBLS+1] = 1;
void7{c in COLSV,r in (ROWS+1)..(ROWS+D)}: BR[r,c,ZBLS+1] = 1;

Izfree{r in ROWSR, c in COLSR, z in ZBLSR : Iz[r,c] = -1}: BR[r,c,z] = 0;
Iz1{Izr in ROWSR, Izc in COLSR, r in ROWSR, c in COLSR, z in ZBLSR : Izr=r and Izc=c and Iz[Izr,Izc]=z}: BR[r,c,z] = 1;

rule1{z in ZBLSR}: sum{r in ROWSR, c in COLSR} BR[r,c,z] = 1;
rule2{r in ROWSR, c in COLSR}: sum{z in ZBLSV} BR[r,c,z] = 1;
rule3{r in ROWSR, c in COLSR, z in ZBLSR}: BR[0,0,z+1] + BR[r-1,c-2,z+1] + BR[r-1,c+2,z+1] + BR[r-2,c-1,z+1] + BR[r-2,c+1,z+1] + BR[r+1,c+2,z+1] + BR[r+1,c-2,z+1] + BR[r+2,c-1,z+1] + BR[r+2,c+1,z+1] - BR[r,c,z] >= 0;

solve;

for {r in ROWSR} {
    for {c in COLSR} {
        printf " %2d", sum{z in ZBLSR} BR[r,c,z]*z;
    }
    printf "\n";
}
data;

param ROWS := 5;
param COLS := 5;
param ZBLS := 25;
param
Iz: 1   2   3   4  5 :=
 1  .   .   .   .  .
 2  .  19   2   .  .
 3  .   .   .   .  .
 4  .   .   .   .  .
 5  .   .   .   .  .
 ;

 end;

```


Produces:

<lang>
GLPSOL: GLPK LP/MIP Solver, v4.47
Parameter(s) specified in the command line:
 --minisat --math Knights.mathprog
Reading model section from Knights.mathprog...
Reading data section from Knights.mathprog...
62 lines were read
Generating void0...
Generating void1...
Generating void2...
Generating void3...
Generating void4...
Generating void5...
Generating void6...
Generating void7...
Generating Izfree...
Generating Iz1...
Generating rule1...
Generating rule2...
Generating rule3...
Model has been successfully generated
Will search for ANY feasible solution
Translating to CNF-SAT...
Original problem has 2549 rows, 2106 columns, and 9349 non-zeros
575 covering inequalities
1924 partitioning equalities
Solving CNF-SAT problem...
Instance has 3356 variables, 10874 clauses, and 34549 literals

### ============================
[MINISAT]
### =============================

| Conflicts |     ORIGINAL     |              LEARNT              | Progress |
|           | Clauses Literals |   Limit Clauses Literals  Lit/Cl |          |

### ========================================================================

|         0 |    9000    32675 |    3000       0        0     0.0 |  0.000 % |
|       101 |    6025    21551 |    3300      93     1620    17.4 | 57.688 % |
|       251 |    6025    21551 |    3630     243     4961    20.4 | 57.688 % |

### ========================================================================

SATISFIABLE
Objective value =  0.000000000e+000
Time used:   0.0 secs
Memory used: 6.5 Mb (6775701 bytes)
  1 12  7 18  3
  6 19  2 13  8
 11 22 15  4 17
 20  5 24  9 14
 23 10 21 16 25
Model has been successfully processed

```


and

<lang>
/*Knights.mathprog

  Find a Knights Tour

  Nigel_Galloway
  January 11th., 2012
*/

param ZBLS;
param ROWS;
param COLS;
param D := 2;
set ROWSR := 1..ROWS;
set COLSR := 1..COLS;
set ROWSV := (1-D)..(ROWS+D);
set COLSV := (1-D)..(COLS+D);
param Iz{ROWSR,COLSR}, integer, default 0;
set ZBLSV := 1..(ZBLS+1);
set ZBLSR := 1..ZBLS;

var BR{ROWSV,COLSV,ZBLSV}, binary;

void0{r in ROWSV, z in ZBLSR,c in (1-D)..0}: BR[r,c,z] = 0;
void1{r in ROWSV, z in ZBLSR,c in (COLS+1)..(COLS+D)}: BR[r,c,z] = 0;
void2{c in COLSV, z in ZBLSR,r in (1-D)..0}: BR[r,c,z] = 0;
void3{c in COLSV, z in ZBLSR,r in (ROWS+1)..(ROWS+D)}: BR[r,c,z] = 0;
void4{r in ROWSV,c in (1-D)..0}: BR[r,c,ZBLS+1] = 1;
void5{r in ROWSV,c in (COLS+1)..(COLS+D)}: BR[r,c,ZBLS+1] = 1;
void6{c in COLSV,r in (1-D)..0}: BR[r,c,ZBLS+1] = 1;
void7{c in COLSV,r in (ROWS+1)..(ROWS+D)}: BR[r,c,ZBLS+1] = 1;

Izfree{r in ROWSR, c in COLSR, z in ZBLSR : Iz[r,c] = -1}: BR[r,c,z] = 0;
Iz1{Izr in ROWSR, Izc in COLSR, r in ROWSR, c in COLSR, z in ZBLSR : Izr=r and Izc=c and Iz[Izr,Izc]=z}: BR[r,c,z] = 1;

rule1{z in ZBLSR}: sum{r in ROWSR, c in COLSR} BR[r,c,z] = 1;
rule2{r in ROWSR, c in COLSR}: sum{z in ZBLSV} BR[r,c,z] = 1;
rule3{r in ROWSR, c in COLSR, z in ZBLSR}: BR[0,0,z+1] + BR[r-1,c-2,z+1] + BR[r-1,c+2,z+1] + BR[r-2,c-1,z+1] + BR[r-2,c+1,z+1] + BR[r+1,c+2,z+1] + BR[r+1,c-2,z+1] + BR[r+2,c-1,z+1] + BR[r+2,c+1,z+1] - BR[r,c,z] >= 0;

solve;

for {r in ROWSR} {
    for {c in COLSR} {
        printf " %2d", sum{z in ZBLSR} BR[r,c,z]*z;
    }
    printf "\n";
}
data;

param ROWS := 8;
param COLS := 8;
param ZBLS := 64;
param
Iz: 1   2   3   4  5  6  7  8 :=
 1  .   .   .   .  .  .  .  .
 2  .   .   .   .  .  . 48  .
 3  .   .   .   .  .  .  .  .
 4  .   .   .   .  .  .  .  .
 5  .   .   .   .  .  .  .  .
 6  .   .   .   .  .  .  .  .
 7  .  58   .   .  .  .  .  .
 8  .   .   .   .  .  .  .  .
 ;

 end;

```


Produces:

<lang>
GLPSOL: GLPK LP/MIP Solver, v4.47
Parameter(s) specified in the command line:
 --minisat --math Knights.mathprog
Reading model section from Knights.mathprog...
Reading data section from Knights.mathprog...
65 lines were read
Generating void0...
Generating void1...
Generating void2...
Generating void3...
Generating void4...
Generating void5...
Generating void6...
Generating void7...
Generating Izfree...
Generating Iz1...
Generating rule1...
Generating rule2...
Generating rule3...
Model has been successfully generated
Will search for ANY feasible solution
Translating to CNF-SAT...
Original problem has 10466 rows, 9360 columns, and 55330 non-zeros
3968 covering inequalities
6370 partitioning equalities
Solving CNF-SAT problem...
Instance has 15056 variables, 46754 clauses, and 149794 literals

### ============================
[MINISAT]
### =============================

| Conflicts |     ORIGINAL     |              LEARNT              | Progress |
|           | Clauses Literals |   Limit Clauses Literals  Lit/Cl |          |

### ========================================================================

|         0 |   40512   143552 |   13504       0        0     0.0 |  0.000 % |
|       100 |   32458   114610 |   14854      89     5138    57.7 | 46.633 % |
|       250 |   32458   114610 |   16340     239    18544    77.6 | 46.633 % |
|       475 |   27499   102956 |   17974     424    42212    99.6 | 46.892 % |
|       813 |   27366   102490 |   19771     757    73184    96.7 | 51.541 % |
|      1322 |   27366   102490 |   21748    1264   137991   109.2 | 52.245 % |
|      2083 |   23226    92730 |   23923    2010   250286   124.5 | 53.620 % |
|      3227 |   22239    90284 |   26315    3138   460582   146.8 | 53.620 % |
|      4937 |   22239    90284 |   28947    4848   769486   158.7 | 53.620 % |
|      7499 |   22206    90168 |   31842    7404  1258240   169.9 | 55.167 % |
|     11346 |   21067    87284 |   35026   11248  2085553   185.4 | 55.167 % |
|     17113 |   21067    87284 |   38528   17015  3625910   213.1 | 55.167 % |
|     25763 |   21067    87284 |   42381   25665  5906283   230.1 | 55.167 % |
|     38738 |   21051    87252 |   46619   38638  9316878   241.1 | 55.679 % |
|     58199 |   21051    87252 |   51281   16434  3967196   241.4 | 55.685 % |
|     87393 |   20707    86474 |   56410   45624 13013357   285.2 | 56.277 % |
|    131184 |   20180    84834 |   62051   37252  8996727   241.5 | 56.542 % |
|    196871 |   20180    84834 |   68256   49392 13807861   279.6 | 56.542 % |
|    295399 |   20180    84834 |   75081   22688  5827696   256.9 | 56.542 % |

### ========================================================================

SATISFIABLE
Objective value =  0.000000000e+000
Time used:   333.0 secs
Memory used: 28.2 Mb (29609617 bytes)
 51 24 31  6 49 26 33 64
 30  5 50 25 32 63 48 43
 23 52  7  4 27 44 15 34
  8 29 60 45 62 47 42 17
 59 22 53 28  3 16 35 14
 54  9 56 61 46 39 18 41
 21 58 11 38 19  2 13 36
 10 55 20 57 12 37 40  1
Model has been successfully processed

```



## Perl

Knight's tour using [[wp:Knight's_tour#Warnsdorff.27s_algorithm|Warnsdorffs algorithm]]

```perl
use strict;
use warnings;
# Find a knight's tour

my @board;

# Choose starting position - may be passed in on command line; if
# not, choose random square.
my ($i, $j);
if (my $sq = shift @ARGV) {
  die "$0: illegal start square '$sq'\n" unless ($i, $j) = from_algebraic($sq);
} else {
  ($i, $j) = (int rand 8, int rand 8);
}

# Move sequence
my @moves = ();

foreach my $move (1..64) {
  # Record current move
  push @moves, to_algebraic($i,$j);
  $board[$i][$j] = $move;

  # Get list of possible next moves
  my @targets = possible_moves($i,$j);

  # Find the one with the smallest degree
  my @min = (9);
  foreach my $target (@targets) {
      my ($ni, $nj) = @$target;
      my $next = possible_moves($ni,$nj);
      @min = ($next, $ni, $nj) if $next < $min[0];
  }

  # And make it
  ($i, $j) = @min[1,2];
}

# Print the move list
for (my $i=0; $i<4; ++$i) {
  for (my $j=0; $j<16; ++$j) {
    my $n = $i*16+$j;
    print $moves[$n];
    print ', ' unless $n+1 >= @moves;
  }
  print "\n";
}
print "\n";

# And the board, with move numbers
for (my $i=0; $i<8; ++$i) {
  for (my $j=0; $j<8; ++$j) {
    # Assumes (1) ANSI sequences work, and (2) output
    # is light text on a dark background.
    print "\e[7m" if ($i%2==$j%2);
    printf " %2d", $board[$i][$j];
    print "\e[0m";
  }
  print "\n";
}

# Find the list of positions the knight can move to from the given square
sub possible_moves
{
  my ($i, $j) = @_;
  return grep { $_->[0] >= 0 && $_->[0] < 8
                    && $_->[1] >= 0 && $_->[1] < 8
                    && !$board[$_->[0]][$_->[1]] } (
                    [$i-2,$j-1], [$i-2,$j+1], [$i-1,$j-2], [$i-1,$j+2],
                    [$i+1,$j-2], [$i+1,$j+2], [$i+2,$j-1], [$i+2,$j+1]);
}

# Return the algebraic name of the square identified by the coordinates
# i=rank, 0=black's home row; j=file, 0=white's queen's rook
sub to_algebraic
{
   my ($i, $j) = @_;
   chr(ord('a') + $j) . (8-$i);
}

# Return the coordinates matching the given algebraic name
sub from_algebraic
{
   my $square = shift;
   return unless $square =~ /^([a-h])([1-8])$/;
   return (8-$2, ord($1) - ord('a'));
}
```


Sample output (start square c3):

[[File:perl_knights_tour.png]]


## Perl 6

```perl6
my @board;

my $I = 8;
my $J = 8;
my $F = $I*$J > 99 ?? "%3d" !! "%2d";

# Choose starting position - may be passed in on command line; if
# not, choose random square.
my ($i, $j);

if my $sq = shift @*ARGS {
  die "$*PROGRAM_NAME: illegal start square '$sq'\n" unless ($i, $j) = from_algebraic($sq);
}
else {
  ($i, $j) = (^$I).pick, (^$J).pick;
}

# Move sequence
my @moves = ();

for 1 .. $I * $J -> $move {
  # Record current move
  push @moves, to_algebraic($i,$j);
  # @board[$i] //= [];	 # (uncomment if autoviv is broken)
  @board[$i][$j] = $move;

  # Find move with the smallest degree
  my @min = (9);
  for possible_moves($i,$j) -> @target {
      my ($ni, $nj) = @target;
      my $next = possible_moves($ni,$nj);
      @min = $next, $ni, $nj if $next < @min[0];
  }

  # And make it
  ($i, $j) = @min[1,2];
}

# Print the move list
for @moves.kv -> $i, $m {
    print ',', $i %% 16 ?? "\n" !! " " if $i;
    print $m;
}
say "\n";

# And the board, with move numbers
for ^$I -> $i {
  for ^$J -> $j {
    # Assumes (1) ANSI sequences work, and (2) output
    # is light text on a dark background.
    print "\e[7m" if $i % 2 == $j % 2;
    printf $F, @board[$i][$j];
    print "\e[0m";
  }
  print "\n";
}

# Find the list of positions the knight can move to from the given square
sub possible_moves($i,$j) {
  grep -> [$ni, $nj] { $ni ~~ ^$I and $nj ~~ ^$J and !@board[$ni][$nj] },
    [$i-2,$j-1], [$i-2,$j+1], [$i-1,$j-2], [$i-1,$j+2],
    [$i+1,$j-2], [$i+1,$j+2], [$i+2,$j-1], [$i+2,$j+1];
}

# Return the algebraic name of the square identified by the coordinates
# i=rank, 0=black's home row; j=file, 0=white's queen's rook
sub to_algebraic($i,$j) {
    chr(ord('a') + $j) ~ ($I - $i);
}

# Return the coordinates matching the given algebraic name
sub from_algebraic($square where /^ (<[a..z]>) (\d+) $/) {
   $I - $1, ord(~$0) - ord('a');
}
```

(Output identical to Perl's above.)


## Phix

This is pretty fast (<<1s) up to size 48, before some sizes start to take quite some time to complete. It will even solve a 200x200 in 0.67s

```Phix
constant size = 8
constant nchars = length(sprintf(" %d",size*size))
constant fmt = sprintf(" %%%dd",nchars-1)
constant blank = repeat(' ',nchars)

-- to simplify output, each square is nchars
sequence board = repeat(repeat(' ',size*nchars),size)
-- keep current counts, immediately backtrack if any hit 0
-- (in line with the above, we only use every nth entry)
sequence warnsdorffs = repeat(repeat(0,size*nchars),size)

constant ROW = 1, COL = 2
constant moves = {{-1,-2},{-2,-1},{-2,1},{-1,2},{1,2},{2,1},{2,-1},{1,-2}}

function onboard(integer row, integer col)
    return row>=1 and row<=size and col>=nchars and col<=nchars*size
end function

procedure init_warnsdorffs()
integer nrow,ncol
    for row=1 to size do
        for col=nchars to nchars*size by nchars do
            for move=1 to length(moves) do
                nrow = row+moves[move][ROW]
                ncol = col+moves[move][COL]*nchars
                if onboard(nrow,ncol) then
                    warnsdorffs[row][col] += 1
                end if
            end for
        end for
    end for
end procedure

atom t0 = time()
integer tries = 0
atom t1 = time()+1
function solve(integer row, integer col, integer n)
integer nrow, ncol
if time()>t1 then
    ?{row,floor(col/nchars),n,tries}
    puts(1,join(board,"\n"))
    t1 = time()+1
--  if wait_key()='!' then ?9/0 end if
end if
    tries+= 1
    if n>size*size then return 1 end if
    sequence wmoves = {}
    for move=1 to length(moves) do
        nrow = row+moves[move][ROW]
        ncol = col+moves[move][COL]*nchars
        if onboard(nrow,ncol)
        and board[nrow][ncol]=' ' then
            wmoves = append(wmoves,{warnsdorffs[nrow][ncol],nrow,ncol})
        end if
    end for
    wmoves = sort(wmoves)
    -- avoid creating orphans
    if length(wmoves)<2 or wmoves[2][1]>1 then
        for m=1 to length(wmoves) do
            {?,nrow,ncol} = wmoves[m]
            warnsdorffs[nrow][ncol] -= 1
        end for
        for m=1 to length(wmoves) do
            {?,nrow,ncol} = wmoves[m]
            board[nrow][ncol-nchars+1..ncol] = sprintf(fmt,n)
            if solve(nrow,ncol,n+1) then return 1 end if
            board[nrow][ncol-nchars+1..ncol] = blank
        end for
        for m=1 to length(wmoves) do
            {?,nrow,ncol} = wmoves[m]
            warnsdorffs[nrow][ncol] += 1
        end for
    end if
    return 0
end function

init_warnsdorffs()
board[1][nchars] = '1'
if solve(1,nchars,2) then
    puts(1,join(board,"\n"))
    printf(1,"\nsolution found in %d tries (%3.2fs)\n",{tries,time()-t0})
else
    puts(1,"no solutions found\n")
end if

{} = wait_key()
```

```txt

"started"
  1 16 31 40  3 18 21 56
 30 39  2 17 42 55  4 19
 15 32 41 46 53 20 57 22
 38 29 48 43 58 45 54  5
 33 14 37 52 47 60 23 62
 28 49 34 59 44 63  6  9
 13 36 51 26 11  8 61 24
 50 27 12 35 64 25 10  7
solution found in 64 tries (0.00s)

```



## PicoLisp


```PicoLisp
(load "@lib/simul.l")

# Build board
(grid 8 8)

# Generate legal moves for a given position
(de moves (Tour)
   (extract
      '((Jump)
         (let? Pos (Jump (car Tour))
            (unless (memq Pos Tour)
               Pos ) ) )
      (quote  # (taken from "games/chess.l")
         ((This) (: 0 1  1  0 -1  1  0 -1  1))        # South Southwest
         ((This) (: 0 1  1  0 -1  1  0  1  1))        # West Southwest
         ((This) (: 0 1  1  0 -1 -1  0  1  1))        # West Northwest
         ((This) (: 0 1  1  0 -1 -1  0 -1 -1))        # North Northwest
         ((This) (: 0 1 -1  0 -1 -1  0 -1 -1))        # North Northeast
         ((This) (: 0 1 -1  0 -1 -1  0  1 -1))        # East Northeast
         ((This) (: 0 1 -1  0 -1  1  0  1 -1))        # East Southeast
         ((This) (: 0 1 -1  0 -1  1  0 -1  1)) ) ) )  # South Southeast

# Build a list of moves, using Warnsdorff’s algorithm
(let Tour '(b1)  # Start at b1
   (while
      (mini
         '((P) (length (moves (cons P Tour))))
         (moves Tour) )
      (push 'Tour @) )
   (flip Tour) )
```

Output:

```txt
-> (b1 a3 b5 a7 c8 b6 a8 c7 a6 b8 d7 f8 h7 g5 h3 g1 e2 c1 a2 b4 c2 a1 b3 a5 b7
d8 c6 d4 e6 c5 a4 c3 d1 b2 c4 d2 f1 h2 f3 e1 d3 e5 f7 h8 g6 h4 g2 f4 d5 e7 g8
h6 g4 e3 f5 d6 e8 g7 h5 f6 e4 g3 h1 f2)
```



## PostScript

You probably shouldn't send this to a printer. Solution using Warnsdorffs algorithm.

```postscript
%!PS-Adobe-3.0
%%BoundingBox: 0 0 300 300

/s { 300 n div } def
/l { rlineto } def

% draws a square
/bx { s mul exch s mul moveto s 0 l 0 s l s neg 0 l 0 s neg l } def

% draws checker board
/xbd {  1 setgray
        0 0 moveto 300 0 l 0 300 l -300 0 l fill
        .7 1 .6 setrgbcolor
        0 1 n1 { dup 2 mod 2 n1 { 1 index bx fill } for pop } for
        0 setgray
} def

/ar1 { [ exch { 0 } repeat ] } def
/ar2 { [ exch dup { dup ar1 exch } repeat pop ] } def

/neighbors {
        -1  2 0
         1  2 0
         2  1 0
         2 -1 0
         1 -2 0
        -1 -2 0
        -2 -1 0
        -2  1 0
        %24 x y add 3 mul roll
} def

/func { 0 dict begin mark } def
/var { counttomark -1 1 { 2 add -1 roll def } for cleartomark } def

% x y can_goto -> bool
/can_goto {
        func /x /y var
        x 0     ge
        x n     lt
        y 0     ge
        y n     lt
        and and and {
                occupied x get y get 0 eq
        } { false } ifelse
        end
} def

% x y num_access -> number of cells reachable from (x,y)
/num_access {
        func /x /y var
        /count 0 def
        x y can_goto {
                neighbors
                8 { pop y add exch x add exch can_goto {
                                /count count 1 add def
                        } if
                } repeat
                count 0 gt { count } { 9 } ifelse
        } { 10 } ifelse
        end
} def

% a circle
/marker { x s mul y s mul s 20 div 0 360 arc fill } def

% n solve -> draws board of size n x n, calcs path and draws it
/solve {
        func /n var
        /n1 n 1 sub def

        /c false def

        8 n div setlinewidth
        gsave

        0 1 n1 { /x exch def c not {
        0 1 n1 {
                /occupied n ar2 def
                c not {
                        /c true def
                        /y exch def
                        grestore xbd gsave
                        s 2 div dup translate
                        n n mul 2 sub -1 0 { /iter exch def
                                c {
                                0 setgray marker x s mul y s mul moveto
                                occupied x get y 1 put
                                neighbors
                                8 { pop y add exch x add exch 2 copy num_access 24 3 roll } repeat
                                7 { dup 4 index lt { 6 3 roll } if pop pop pop } repeat

                                9 ge iter 0 gt and { /c false def } if
                                /y exch def
                                /x exch def
                                .2 setgray x s mul y s mul lineto stroke
                        } if } for
                        % to be nice, draw box at final position
                        .5 0 0 setrgbcolor marker
                        y .5 sub x .5 sub bx 1 setlinewidth stroke
                        stroke
                } if
        } for } if } for showpage
        grestore
        end
} def

3 1 100 { solve } for

%%EOF
```



## Prolog

Knights tour using [[wp:Knight's_tour#Warnsdorff.27s_algorithm|Warnsdorffs algorithm]]


```Prolog
% N is the number of lines of the chessboard
knight(N) :-
	Max is N * N,
	length(L, Max),
	knight(N, 0, Max, 0, 0, L),
	display(N, 0, L).

% knight(NbCol, Coup, Max, Lig, Col, L),
% NbCol : number of columns per line
% Coup  : number of the current move
% Max   : maximum number of moves
% Lig/ Col : current position of the knight
% L : the "chessboard"

% the game is over
knight(_, Max, Max, _, _, _) :- !.

knight(NbCol, N, MaxN, Lg, Cl, L) :-
	% Is the move legal
	Lg >= 0, Cl >= 0, Lg < NbCol, Cl < NbCol,

	Pos is Lg * NbCol + Cl,
	N1 is N+1,
	% is the place free
	nth0(Pos, L, N1),

	LgM1 is Lg - 1, LgM2 is Lg - 2, LgP1 is Lg + 1, LgP2 is Lg + 2,
	ClM1 is Cl - 1, ClM2 is Cl - 2, ClP1 is Cl + 1, ClP2 is Cl + 2,
	maplist(best_move(NbCol, L),
		[(LgP1, ClM2), (LgP2, ClM1), (LgP2, ClP1),(LgP1, ClP2),
		 (LgM1, ClM2), (LgM2, ClM1), (LgM2, ClP1),(LgM1, ClP2)],
		R),
	sort(R, RS),
	pairs_values(RS, Moves),

	move(NbCol, N1, MaxN, Moves, L).

move(NbCol, N1, MaxN, [(Lg, Cl) | R], L) :-
	knight(NbCol, N1, MaxN, Lg, Cl, L);
	move(NbCol, N1, MaxN,  R, L).

%% An illegal move is scored 1000
best_move(NbCol, _L, (Lg, Cl), 1000-(Lg, Cl)) :-
	(   Lg < 0 ; Cl < 0; Lg >= NbCol; Cl >= NbCol), !.

best_move(NbCol, L, (Lg, Cl), 1000-(Lg, Cl)) :-
	Pos is Lg*NbCol+Cl,
	nth0(Pos, L, V),
	\+var(V), !.

%% a legal move is scored with the number of moves a knight can make
best_move(NbCol, L, (Lg, Cl), R-(Lg, Cl)) :-
	LgM1 is Lg - 1, LgM2 is Lg - 2, LgP1 is Lg + 1, LgP2 is Lg + 2,
	ClM1 is Cl - 1, ClM2 is Cl - 2, ClP1 is Cl + 1, ClP2 is Cl + 2,
	include(possible_move(NbCol, L),
		[(LgP1, ClM2), (LgP2, ClM1), (LgP2, ClP1),(LgP1, ClP2),
		 (LgM1, ClM2), (LgM2, ClM1), (LgM2, ClP1),(LgM1, ClP2)],
		Res),
	length(Res, Len),
	(   Len = 0 -> R = 1000; R = Len).

% test if a place is enabled
possible_move(NbCol, L, (Lg, Cl)) :-
	% move must be legal
	Lg >= 0, Cl >= 0, Lg < NbCol, Cl < NbCol,
	Pos is Lg * NbCol + Cl,
	% place must be free
	nth0(Pos, L, V),
	var(V).


display(_, _, []).
display(N, N, L) :-
	nl,
	display(N, 0, L).

display(N, M, [H | T]) :-
	writef('%3r', [H]),
	M1 is M + 1,
	display(N, M1, T).

```


Output :

```txt
 ?- knight(8).
   1  16  31  40   3  18  21  56
  30  39   2  17  42  55   4  19
  15  32  41  46  53  20  57  22
  38  29  48  43  58  45  54   5
  33  14  37  52  47  60  23  62
  28  49  34  59  44  63   6   9
  13  36  51  26  11   8  61  24
  50  27  12  35  64  25  10   7
true .

 ?- knight(20).
   1  40  81  90   3  42  77  94   5  44  73 102   7  46  69  62   9  48  51  60
  82  89   2  41  92  95   4  43  76 101   6  45  72 103   8  47  68  61  10  49
  39  80  91  96 153  78  93 100 129  74 109 104 123  70 111 120  63  50  59  52
  88  83 154  79  98 159 152  75 108 105 128  71 110 121 124  67 112 119  64  11
 155  38  97 160 157 200  99 162 151 130 107 122 127 132 141 118 125  66  53  58
  84  87 156 199 176 161 158 201 106 163 150 131 142 145 126 133 140 113  12  65
  37 182  85 178 207 198 175 164 173 216 143 166 149 222 139 146 117 134  57  54
  86 179 206 197 204 177 208 217 202 165 172 221 144 167 148 223 138  55 114  13
 183  36 181 212 209 218 203 174 215 220 227 170 281 224 303 168 147 116 135  56
 180 211 196 205 230 213 238 219 228 171 280 225 302 169 282 343 304 137  14 115
  35 184 231 210 237 246 229 214 279 226 301 298 283 342 367 308 347 344 305 136
 232 195 236 245 234 239 278 247 300 297 284 359 366 309 348 345 368 307 350  15
 185  34 233 240 261 248 287 296 285 358 299 310 341 378 365 384 349 346 369 306
 194 241 250 235 244 277 260 313 294 311 360 373 364 383 354 379 370 385  16 351
  33 186 243 262 249 288 295 286 361 316 357 340 377 372 395 386 353 380 333 388
 242 193 254 251 276 259 314 293 312 321 374 363 398 355 382 371 394 387 352  17
 187  32 263 258 267 252 289 322 315 362 317 356 339 376 399 396 381 334 389 332
 192 255 190 253 264 275 268 271 292 323 320 375 326 397 338 335 390 393  18  21
  31 188 257 266  29 270 273 290  27 318 327 324  25 336 329 400  23  20 331 392
 256 191  30 189 274 265  28 269 272 291  26 319 328 325  24 337 330 391  22  19
true .
```



### Alternative version

```prolog
:- initialization(main).


board_size(8).
in_board(X*Y) :- board_size(N), between(1,N,Y), between(1,N,X).


% express jump-graph in dynamic "move"-rules
make_graph :-
    findall(_, (in_board(P), assert_moves(P)), _).

    % where
    assert_moves(P) :-
        findall(_, (can_move(P,Q), asserta(move(P,Q))), _).

    can_move(X*Y,Q) :-
        ( one(X,X1), two(Y,Y1) ; two(X,X1), one(Y,Y1) )
      , Q = X1*Y1, in_board(Q)
      . % where
        one(M,N) :- succ(M,N)  ; succ(N,M).
        two(M,N) :- N is M + 2 ; N is M - 2.



hamiltonian(P,Pn) :-
    board_size(N), Size is N * N
  , hamiltonian(P,Size,[],Ps), enumerate(Size,Ps,Pn)
  .
    % where
    enumerate(_, []    , []      ).
    enumerate(N, [P|Ps], [N:P|Pn]) :- succ(M,N), enumerate(M,Ps,Pn).


hamiltonian(P,N,Ps,Res) :-
    N =:= 1 -> Res = [P|Ps]
  ; warnsdorff(Ps,P,Q), succ(M,N)
  , hamiltonian(Q,M,[P|Ps],Res)
  .
    % where
    warnsdorff(Ps,P,Q) :-
        moves(Ps,P,Qs), maplist(next_moves(Ps), Qs, Xs)
      , keysort(Xs,Ys), member(_-Q,Ys)
      .
    next_moves(Ps,Q,L-Q) :- moves(Ps,Q,Rs), length(Rs,L).

    moves(Ps,P,Qs) :-
        findall(Q, (move(P,Q), \+ member(Q,Ps)), Qs).



show_path(Pn)  :- findall(_, (in_board(P), show_cell(Pn,P)), _).
    % where
    show_cell(Pn,X*Y) :-
        member(N:X*Y,Pn), format('%3.0d',[N]), board_size(X), nl.


main :- make_graph, hamiltonian(5*3,Pn), show_path(Pn), halt.
```

```txt
  5 18 35 22  3 16 55 24
 36 21  4 17 54 23  2 15
 19  6 59 34  1 14 25 56
 60 37 20 53 62 57 32 13
  7 52 61 58 33 30 63 26
 38 49 40 29 64 45 12 31
 41  8 51 48 43 10 27 46
 50 39 42  9 28 47 44 1
```

[http://ideone.com/jnFTT3 20x20 board runs in: time: 0.91 memory: 68608.]


## Python

Knights tour using [[wp:Knight's_tour#Warnsdorff.27s_algorithm|Warnsdorffs algorithm]]

```python
import copy

boardsize=6
_kmoves = ((2,1), (1,2), (-1,2), (-2,1), (-2,-1), (-1,-2), (1,-2), (2,-1))


def chess2index(chess, boardsize=boardsize):
    'Convert Algebraic chess notation to internal index format'
    chess = chess.strip().lower()
    x = ord(chess[0]) - ord('a')
    y = boardsize - int(chess[1:])
    return (x, y)

def boardstring(board, boardsize=boardsize):
    r = range(boardsize)
    lines = ''
    for y in r:
        lines += '\n' + ','.join('%2i' % board[(x,y)] if board[(x,y)] else '  '
                                 for x in r)
    return lines

def knightmoves(board, P, boardsize=boardsize):
    Px, Py = P
    kmoves = set((Px+x, Py+y) for x,y in _kmoves)
    kmoves = set( (x,y)
                  for x,y in kmoves
                  if 0 <= x < boardsize
                     and 0 <= y < boardsize
                     and not board[(x,y)] )
    return kmoves

def accessibility(board, P, boardsize=boardsize):
    access = []
    brd = copy.deepcopy(board)
    for pos in knightmoves(board, P, boardsize=boardsize):
        brd[pos] = -1
        access.append( (len(knightmoves(brd, pos, boardsize=boardsize)), pos) )
        brd[pos] = 0
    return access

def knights_tour(start, boardsize=boardsize, _debug=False):
    board = {(x,y):0 for x in range(boardsize) for y in range(boardsize)}
    move = 1
    P = chess2index(start, boardsize)
    board[P] = move
    move += 1
    if _debug:
        print(boardstring(board, boardsize=boardsize))
    while move <= len(board):
        P = min(accessibility(board, P, boardsize))[1]
        board[P] = move
        move += 1
        if _debug:
            print(boardstring(board, boardsize=boardsize))
            input('\n%2i next: ' % move)
    return board

if __name__ == '__main__':
    while 1:
        boardsize = int(input('\nboardsize: '))
        if boardsize < 5:
            continue
        start = input('Start position: ')
        board = knights_tour(start, boardsize)
        print(boardstring(board, boardsize=boardsize))
```


;Sample runs

```txt
boardsize: 5
Start position: c3

19,12,17, 6,21
 2, 7,20,11,16
13,18, 1,22, 5
 8, 3,24,15,10
25,14, 9, 4,23

boardsize: 8
Start position: h8

38,41,18, 3,22,27,16, 1
19, 4,39,42,17, 2,23,26
40,37,54,21,52,25,28,15
 5,20,43,56,59,30,51,24
36,55,58,53,44,63,14,29
 9, 6,45,62,57,60,31,50
46,35, 8,11,48,33,64,13
 7,10,47,34,61,12,49,32

boardsize: 10
Start position: e6

29, 4,57,24,73, 6,95,10,75, 8
58,23,28, 5,94,25,74, 7,100,11
 3,30,65,56,27,72,99,96, 9,76
22,59, 2,63,68,93,26,81,12,97
31,64,55,66, 1,82,71,98,77,80
54,21,60,69,62,67,92,79,88,13
49,32,53,46,83,70,87,42,91,78
20,35,48,61,52,45,84,89,14,41
33,50,37,18,47,86,39,16,43,90
36,19,34,51,38,17,44,85,40,15

boardsize: 200
Start position: a1

510,499,502,101,508,515,504,103,506,5021 ... 195,8550,6691,6712,197,6704,201,6696,199
501,100,509,514,503,102,507,5020,5005,10 ... 690,6713,196,8553,6692,6695,198,6703,202
498,511,500,4989,516,5019,5004,505,5022, ... ,30180,8559,6694,6711,8554,6705,200,6697
99,518,513,4992,5003,4990,5017,5044,5033 ... 30205,8552,30181,8558,6693,6702,203,6706
512,497,4988,517,5018,5001,5034,5011,504 ... 182,30201,30204,8555,6710,8557,6698,6701
519,98,4993,5002,4991,5016,5043,5052,505 ... 03,30546,30183,30200,30185,6700,6707,204
496,4987,520,5015,5000,5035,5012,5047,51 ... 4,30213,30202,31455,8556,6709,30186,6699
97,522,4999,4994,5013,5042,5051,5060,505 ... 7,31456,31329,30184,30199,30190,205,6708
4986,495,5014,521,5036,4997,5048,5101,50 ... 1327,31454,30195,31472,30187,30198,30189
523,96,4995,4998,5041,5074,5061,5050,507 ... ,31330,31471,31328,31453,30196,30191,206

...

404,731,704,947,958,1013,966,1041,1078,1 ... 9969,39992,39987,39996,39867,39856,39859
 5,706,735,960,955,972,957,1060,1025,106 ... ,39978,39939,39976,39861,39990,297,39866
724,403,730,705,946,967,1012,971,1040,10 ... 9975,39972,39991,39868,39863,39860,39855
707, 4,723,736,729,956,973,996,1061,1026 ... ,39850,39869,39862,39973,39852,39865,298
402,725,708,943,968,945,970,1011,978,997 ... 6567,39974,39851,39864,36571,39854,36573
 3,722,737,728,741,942,977,974,995,1010, ... ,39800,39849,36570,39853,36574,299,14088
720,401,726,709,944,969,742,941,980,975, ... ,14091,36568,36575,14084,14089,36572,843
711, 2,721,738,727,740,715,976,745,940,9 ... 65,36576,14083,14090,36569,844,14087,300
400,719,710,713,398,717,746,743,396,981, ... ,849,304,14081,840,847,302,14085,842,845
 1,712,399,718,739,714,397,716,747,744,3 ... 4078,839,848,303,14082,841,846,301,14086
```


The 200x200 example warmed my study in its computation but did return a tour.

P.S. There is a slight deviation to a strict interpretation of Warnsdorff's algorithm in that as a convenience, tuples of the length of the knight moves followed by the position are minimized so knights moves with the same length will try and break the ties based on their minimum x,y position. In practice, it seems to give comparable results to the original algorithm.

boardsize: 5
Start position: a3
Traceback (most recent call last):
  File "rosettacodekt.py", line 65, in <module>
    board = knights_tour(start, boardsize)
  File "rosettacodekt.py", line 51, in knights_tour
    P = min(accessibility(board, P, boardsize))[1]
ValueError: min() arg is an empty sequence


## R

Based on a slight modification of [[wp:Knight%27s_tour#Warnsdorff.27s_rule|Warnsdorff's algorithm]], in that if a dead-end is reached, the program backtracks to the next best move.


```r
#!/usr/bin/Rscript

# M x N Chess Board.
M = 8; N = 8; board = matrix(0, nrow = M, ncol = N)

# Get/Set value on a board position.
getboard = function (position)    { board[position[1], position[2]] }
setboard = function (position, x) { board[position[1], position[2]] <<- x }

# (Relative) Hops of a Knight.
hops = cbind(c(-2, -1), c(-1, -2), c(+1, -2), c(+2, -1),
             c(+2, +1), c(+1, +2), c(-1, +2), c(-2, +1))

# Validate a move.
valid = function (move) {
    all(1 <= move & move <= c(M, N)) && (getboard(move) == 0)
}

# Moves possible from a given position.
explore = function (position) {
    moves = position + hops
    cbind(moves[, apply(moves, 2, valid)])
}

# Possible moves sorted according to their Wornsdorff cost.
candidates = function (position) {
    moves = explore(position)

    # No candidate moves available.
    if (ncol(moves) == 0) { return(moves) }

    wcosts = apply(moves, 2, function (position) { ncol(explore(position)) })
    cbind(moves[, order(wcosts)])
}

# Recursive function for touring the chess board.
knightTour = function (position, moveN) {

    # Tour Complete.
    if (moveN > (M * N)) {
        print(board)
        quit()
    }

    # Available moves.
    moves = candidates(position)

    # None possible. Backtrack.
    if (ncol(moves) == 0) { return() }

    # Make a move, and continue the tour.
    apply(moves, 2, function (position) {
                        setboard(position, moveN)
                        knightTour(position, moveN + 1)
                        setboard(position, 0)
                    })
}

# User Input: Starting position (in algebraic notation).
square = commandArgs(trailingOnly = TRUE)

# Convert into board co-ordinates.
row      = M + 1 - as.integer(substr(square, 2, 2))
ascii    = function (ch) { as.integer(charToRaw(ch)) }
col      = 1 + ascii(substr(square, 1, 1)) - ascii('a')
position = c(row, col)

# Begin tour.
setboard(position, 1); knightTour(position, 2)
```


Output:

```txt

./knight.R e3

     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
[1,]    6    9   24   55   62   11   26   29
[2,]   23   54    7   10   25   28   63   12
[3,]    8    5   50   61   56   59   30   27
[4,]   37   22   53   58   43   48   13   64
[5,]    4   51   38   49   60   57   44   31
[6,]   21   36   19   52    1   42   47   14
[7,]   18    3   34   39   16   45   32   41
[8,]   35   20   17    2   33   40   15   46

```



## Racket


```racket

#lang racket
(define N 8)
(define nexts ; construct the graph
  (let ([ds (for*/list ([x 2] [x* '(+1 -1)] [y* '(+1 -1)])
              (cons (* x* (+ 1 x)) (* y* (- 2 x))))])
    (for*/vector ([i N] [j N])
      (filter values (for/list ([d ds])
                       (let ([i (+ i (car d))] [j (+ j (cdr d))])
                         (and (< -1 i N) (< -1 j N) (+ j (* N i)))))))))
(define (tour x y)
  (define xy (+ x (* N y)))
  (let loop ([seen (list xy)] [ns (vector-ref nexts xy)] [n (sub1 (* N N))])
    (if (zero? n) (reverse seen)
        (for/or ([next (sort (map (λ(n) (cons n (remq* seen (vector-ref nexts n)))) ns)
                             < #:key length #:cache-keys? #t)])
          (loop (cons (car next) seen) (cdr next) (sub1 n))))))
(define (draw tour)
  (define v (make-vector (* N N)))
  (for ([n tour] [i (in-naturals 1)]) (vector-set! v n i))
  (for ([i N])
    (displayln (string-join (for/list ([j (in-range i (* N N) N)])
                              (~a (vector-ref v j) #:width 2 #:align 'right))
                            " "))))
(draw (tour (random N) (random N)))

```

```txt

56 11 36 33 52 13 38 17
35 32 55 12 37 16 51 14
10 57 34 53 48 45 18 39
31 54 43 64 41 50 15 46
60  9 58 49 44 47 40 19
27 30 61 42 63 22  1  4
 8 59 28 25  6  3 20 23
29 26  7 62 21 24  5  2

```



## REXX

This REXX version is modeled after the XPL0 example.

The size of the chessboard may be specified as well as the knight's starting position.

This is an   ''open tour''   solution.   (See this task's   ''discussion''   page for an explanation, the section is   ''The 7x7 problem''.)

```rexx
/*REXX program solves the  knight's tour  problem   for a  (general)   NxN   chessboard.*/
parse arg  N  sRank sFile .                      /*obtain optional arguments from the CL*/
if     N=='' |     N==","  then     N=8          /*No boardsize specified?  Use default.*/
if sRank=='' | sRank==","  then sRank=N          /*No starting rank given?   "      "   */
if sFile=='' | sFile==","  then sFile=1          /* "     "    file   "      "      "   */
NN=N**2;            NxN='a ' N"x"N ' chessboard' /*file [↓]           [↓]   r=rank      */
@.=;    do r=1  for N;  do f=1  for N;  @.r.f=.;  end  /*f*/;   end  /*r*/
beg= '-1-'                                       /*[↑]  create an empty  NxN chessboard.*/
               Kr =  '2  1 -1 -2  -2 -1  1  2'   /*the legal "rank"  moves for a knight.*/
               Kf =  '1  2  2  1  -1 -2 -2 -1'   /* "    "   "file"    "    "  "    "   */
kr.M=words(Kr)                                   /*number of possible moves for a Knight*/
parse var  Kr  Kr.1 Kr.2 Kr.3 Kr.4 Kr.5 Kr.6 Kr.7 Kr.8   /*parse the legal moves by hand*/
parse var  Kf  Kf.1 Kf.2 Kf.3 Kf.4 Kf.5 Kf.6 Kf.7 Kf.8   /*  "    "    "     "    "   " */
@.sRank.sFile= beg                               /*the knight's starting position.      */
@kt= "knight's tour"                             /*a handy-dandy literal for the  SAYs. */
if \move(2, sRank, sFile)  &  \(N==1)  then say 'No'   @kt   "solution for"         NxN'.'
                                       else say 'A solution for the'   @kt   "on"   NxN':'
!=left('', 9 * (n<18) )                          /*used for indentation of chessboard.  */
_=substr(copies("┼───",N),2);   say;   say  ! translate('┌'_"┐", '┬', "┼")   /*a square.*/
                                                 /* [↓]  build a display for chessboard.*/
     do   r=N  for N  by -1;    if r\==N  then say ! '├'_"┤";        L=@.
       do f=1  for N; ?=@.r.f;  if ?==NN  then ?='end';  L=L'│'center(?, 3)  /*is "end"?*/
       end   /*f*/                               /*done with   rank   of the chessboard.*/
     say ! translate(L'│', , .)                  /*display a     "     "  "       "     */
     end     /*r*/                               /*19x19 chessboard can be shown 80 cols*/

say  !  translate('└'_"┘", '┴', "┼")             /*show the last rank of the chessboard.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
move: procedure expose @. Kr. Kf. NN;    parse arg #,rank,file  /*obtain move,rank,file.*/
         do t=1  for Kr.M;   nr=rank+Kr.t;       nf=file+Kf.t   /*position of the knight*/
         if @.nr.nf==.  then do;                 @.nr.nf=#      /*Empty? Knight can move*/
                             if #==NN            then return 1  /*is this the last move?*/
                             if move(#+1,nr,nf)  then return 1  /* "   "   "    "    "  */
                             @.nr.nf=.                          /*undo the above move.  */
                             end                                /*try different move.   */
         end   /*t*/                                            /* [↑]  all moves tried.*/
      return 0                                                  /*tour is not possible. */
```

'''output'''   when using the default input:

```txt

A solution for the knight's tour on a  8x8  chessboard:

          ┌───┬───┬───┬───┬───┬───┬───┬───┐
          │-1-│38 │55 │34 │ 3 │36 │19 │22 │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │54 │47 │ 2 │37 │20 │23 │ 4 │17 │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │39 │56 │33 │46 │35 │18 │21 │10 │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │48 │53 │40 │57 │24 │11 │16 │ 5 │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │59 │32 │45 │52 │41 │26 │ 9 │12 │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │44 │49 │58 │25 │62 │15 │ 6 │27 │
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │31 │60 │51 │42 │29 │ 8 │13 │end│
          ├───┼───┼───┼───┼───┼───┼───┼───┤
          │50 │43 │30 │61 │14 │63 │28 │ 7 │
          └───┴───┴───┴───┴───┴───┴───┴───┘

```



## Ruby

Knights tour using [[wp:Knight's_tour#Warnsdorff.27s_rule|Warnsdorffs rule]]

```ruby
class Board
  Cell = Struct.new(:value, :adj) do
    def self.end=(end_val)
      @@end = end_val
    end

    def try(seq_num)
      self.value = seq_num
      return true  if seq_num==@@end
      a = []
      adj.each_with_index do |cell, n|
        a << [wdof(cell.adj)*10+n, cell]  if cell.value.zero?
      end
      a.sort.each {|_, cell| return true  if cell.try(seq_num+1)}
      self.value = 0
      false
    end

    def wdof(adj)
      adj.count {|cell| cell.value.zero?}
    end
  end

  def initialize(rows, cols)
    @rows, @cols = rows, cols
    unless defined? ADJACENT                      # default move (Knight)
      eval("ADJACENT = [[-1,-2],[-2,-1],[-2,1],[-1,2],[1,2],[2,1],[2,-1],[1,-2]]")
    end
    frame = ADJACENT.flatten.map(&:abs).max
    @board = Array.new(rows+frame) do |i|
      Array.new(cols+frame) do |j|
        (i<rows and j<cols) ? Cell.new(0) : nil   # frame (Sentinel value : nil)
      end
    end
    rows.times do |i|
      cols.times do |j|
        @board[i][j].adj = ADJACENT.map{|di,dj| @board[i+di][j+dj]}.compact
      end
    end
    Cell.end = rows * cols
    @format = " %#{(rows * cols).to_s.size}d"
  end

  def solve(sx, sy)
    if (@rows*@cols).odd? and (sx+sy).odd?
      puts "No solution"
    else
      puts (@board[sx][sy].try(1) ? to_s : "No solution")
    end
  end

  def to_s
    (0...@rows).map do |x|
      (0...@cols).map{|y| @format % @board[x][y].value}.join
    end
  end
end

def knight_tour(rows=8, cols=rows, sx=rand(rows), sy=rand(cols))
  puts "\nBoard (%d x %d), Start:[%d, %d]" % [rows, cols, sx, sy]
  Board.new(rows, cols).solve(sx, sy)
end

knight_tour(8,8,3,1)

knight_tour(5,5,2,2)

knight_tour(4,9,0,0)

knight_tour(5,5,0,1)

knight_tour(12,12,1,1)
```

Which produces:

```txt

Board (8 x 8), Start:[3, 1]
 23 20  3 32 25 10  5  8
  2 35 24 21  4  7 26 11
 19 22 33 36 31 28  9  6
 34  1 50 29 48 37 12 27
 51 18 53 44 61 30 47 38
 54 43 56 49 58 45 62 13
 17 52 41 60 15 64 39 46
 42 55 16 57 40 59 14 63

Board (5 x 5), Start:[2, 2]
 19  8  3 14 25
  2 13 18  9  4
  7 20  1 24 15
 12 17 22  5 10
 21  6 11 16 23

Board (4 x 9), Start:[0, 0]
  1 34  3 28 13 24  9 20 17
  4 29  6 33  8 27 18 23 10
 35  2 31 14 25 12 21 16 19
 30  5 36  7 32 15 26 11 22

Board (5 x 5), Start:[0, 1]
No solution

Board (12 x 12), Start:[1, 1]
  87  24  59   2  89  26  61   4  39   8  31   6
  58   1  88  25  60   3  92  27  30   5  38   9
  23  86  83  90 103  98  29  62  93  40   7  32
  82  57 102  99  84  91 104  97  28  37  10  41
 101  22  85 114 105 116 111  94  63  96  33  36
  56  81 100 123 128 113 106 117 110  35  42  11
  21 122 141  80 115 124 127 112  95  64 109  34
 144  55  78 121 142 129 118 107 126 133  12  43
  51  20 143 140  79 120 125 138  69 108  65 134
  54  73  52  77 130 139  70 119 132 137  44  13
  19  50  75  72  17  48 131  68  15  46 135  66
  74  53  18  49  76  71  16  47 136  67  14  45

```

cf. [[Solve a Holy Knight's tour#Ruby|Solve a Holy Knight's tour]]:


## Rust


```rust
use std::fmt;

const SIZE: usize = 8;
const MOVES: [(i32, i32); 8] = [
    (2, 1),
    (1, 2),
    (-1, 2),
    (-2, 1),
    (-2, -1),
    (-1, -2),
    (1, -2),
    (2, -1),
];

#[derive(Copy, Clone, Eq, PartialEq, PartialOrd, Ord)]
struct Point {
    x: i32,
    y: i32,
}

impl Point {
    fn mov(&self, &(dx, dy): &(i32, i32)) -> Self {
        Self {
            x: self.x + dx,
            y: self.y + dy,
        }
    }
}

struct Board {
    field: [[i32; SIZE]; SIZE],
}

impl Board {
    fn new() -> Self {
        Self {
            field: [[0; SIZE]; SIZE],
        }
    }

    fn available(&self, p: Point) -> bool {
        0 <= p.x
            && p.x < SIZE as i32
            && 0 <= p.y
            && p.y < SIZE as i32
            && self.field[p.x as usize][p.y as usize] == 0
    }

    // calculate the number of possible moves
    fn count_degree(&self, p: Point) -> i32 {
        let mut count = 0;
        for dir in MOVES.iter() {
            let next = p.mov(dir);
            if self.available(next) {
                count += 1;
            }
        }
        count
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in self.field.iter() {
            for x in row.iter() {
                write!(f, "{:3} ", x)?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

fn knights_tour(x: i32, y: i32) -> Option<Board> {
    let mut board = Board::new();
    let mut p = Point { x: x, y: y };
    let mut step = 1;
    board.field[p.x as usize][p.y as usize] = step;
    step += 1;

    while step <= (SIZE * SIZE) as i32 {
        // choose next square by Warnsdorf's rule
        let mut candidates = vec![];
        for dir in MOVES.iter() {
            let adj = p.mov(dir);
            if board.available(adj) {
                let degree = board.count_degree(adj);
                candidates.push((degree, adj));
            }
        }
        match candidates.iter().min() {
            // move to next square
            Some(&(_, adj)) => p = adj,
            // can't move
            None => return None,
        };
        board.field[p.x as usize][p.y as usize] = step;
        step += 1;
    }
    Some(board)
}

fn main() {
    let (x, y) = (3, 1);
    println!("Board size: {}", SIZE);
    println!("Starting position: ({}, {})", x, y);
    match knights_tour(x, y) {
        Some(b) => print!("{}", b),
        None => println!("Fail!"),
    }
}
```

```txt

Board size: 8
Starting position: (3, 1)
 23  20   3  32  25  10   5   8
  2  33  24  21   4   7  26  11
 19  22  51  34  31  28   9   6
 50   1  40  29  54  35  12  27
 41  18  55  52  61  30  57  36
 46  49  44  39  56  53  62  13
 17  42  47  60  15  64  37  58
 48  45  16  43  38  59  14  63

```



## Scala


```Scala

val b=Seq.tabulate(8,8,8,8)((x,y,z,t)=>(1L<<(x*8+y),1L<<(z*8+t),f"${97+z}%c${49+t}%c",(x-z)*(x-z)+(y-t)*(y-t)==5)).flatten.flatten.flatten.filter(_._4).groupBy(_._1)
def f(p:Long,s:Long,v:Any){if(-1L!=s)b(p).foreach(x=>if((s&x._2)==0)f(x._2,s|x._2,v+x._3))else println(v)}
f(1,1,"a1")

```


```txt

a1b3a5b7c5a4b2c4a3b1c3a2b4a6b8c6a7b5c7a8b6c8d6e4d2f1e3c2d4e2c1d3e1g2f4d5e7g8h6f5h4g6h8f7d8e6f8d7e5g4h2f3g1h3g5h7f6e8g7h5g3h1f2d1

```


## Scheme


```scheme

;;/usr/bin/petite
;;encoding:utf-8
;;Author:Panda
;;Mail:panbaoxiang@hotmail.com
;;Created Time:Thu 29 Jan 2015 10:18:49 AM CST
;;Description:

;;size of the chessboard
(define X 8)
(define Y 8)
;;position is an integer that could be decoded into the x coordinate and y coordinate
 (define(decode position)
  (cons (div position Y) (remainder position Y)))
 ;;record the paths and number of territories you have conquered
 (define dictionary '())
 (define counter 1)
 ;;define the forbiddend territories(conquered and cul-de-sac)
 (define forbiddened '())
 ;;renew when havn't conquered the world.
 (define (renew position)
  (define possible
   (let ((rules (list (+ (* 2 Y) 1 position)
                      (+ (* 2 Y) -1 position)
                      (+ (* -2 Y) 1 position)
                      (+ (* -2 Y) -1 position)
                      (+ Y 2 position)
                      (+ Y -2 position)
                      (- position Y 2)
                      (- position Y -2))))
    (filter (lambda(x) (not (or (member x forbiddened) (< x 0) (>= x (* X Y))))) rules)))
  (if (null? possible)
   (begin (set! forbiddened (cons (car dictionary) forbiddened))
          (set! dictionary (cdr dictionary))
          (set! counter (- counter 1))
          (car dictionary))
   (begin (set! dictionary (cons (car possible) dictionary))
          (set! forbiddened dictionary)
          (set! counter (+ counter 1))
          (car possible))))
;;go to search
(define (go position)
 (if (= counter (* X Y))
  (begin
  (set! result (reverse dictionary))
  (display (map (lambda(x) (decode x)) result)))
  (go (renew position))))

```

```txt

 (go 35)
((6 . 4) (4 . 5) (6 . 6) (4 . 7) (7 . 0) (5 . 1) (7 . 2) (5 . 3) (7 . 4) (5 . 5) (7 . 6) (5 . 7) (4 . 0) (6 . 1) (4 . 2) (6 . 3) (4 . 4) (6 . 5) (4 . 6) (6 . 7) (5 . 0) (7 . 1) (5 . 2) (7 . 3) (5 . 4) (7 . 5) (5 . 6) (7 . 7) (6 . 0) (4 . 1) (6 . 2) (4 . 3) (2 . 4) (0 . 5) (2 . 6) (0 . 7) (3 . 0) (1 . 1) (3 . 2) (1 . 3) (3 . 4) (1 . 5) (3 . 6) (1 . 7) (0 . 0) (2 . 1) (0 . 2) (2 . 3) (0 . 4) (2 . 5) (0 . 6) (2 . 7) (1 . 0) (3 . 1) (1 . 2) (3 . 3) (1 . 4) (3 . 5) (1 . 6) (3 . 7) (2 . 0) (0 . 1) (2 . 2))

```



## SequenceL

Knights tour using [[wp:Knight's_tour#Warnsdorff.27s_rule|Warnsdorffs rule]] (No Backtracking)

```sequenceL

import <Utilities/Sequence.sl>;
import <Utilities/Conversion.sl>;

main(args(2)) :=
	let
		N := stringToInt(args[1]) when size(args) > 0 else 8;
		M := stringToInt(args[2]) when size(args) > 1 else N;
		startX := stringToInt(args[3]) when size(args) > 2 else 1;
		startY := stringToInt(args[4]) when size(args) > 3 else 1;
		board[i,j] := 0 foreach i within 1 ... N, j within 1 ... M;
		spacing := size(toString(N*M)) + 1;
	in
		join(printRow(
		        tour(setBoard(board, startX, startX, 1), [startX,startY], 2),
		    spacing));

potentialMoves := [[2,1], [2,-1], [1,2], [1,-2], [-1,2], [-1,-2], [-2,1], [-2,-1]];

printRow(row(1), spacing) := join(printSquare(row, spacing)) ++ "\n";

printSquare(val, spacing) :=
	let
		str := toString(val);
	in
		duplicate(' ', spacing - size(str)) ++ str;

tour(board(2), current(1), move) :=
	let
		validMoves := validMove(board, current + potentialMoves);
		numMoves[i] := size(validMove(board, validMoves[i] + potentialMoves));
		chosenMove := minPosition(numMoves);
	in
	board when move > size(board) * size(board[1]) else
	[] when size(validMoves) = 0 else
	[] when move < size(board) * size(board[1]) and numMoves[chosenMove] = 0 else
	tour(setBoard(board, validMoves[chosenMove][1], validMoves[chosenMove][2], move), validMoves[chosenMove], move + 1);

validMove(board(2), position(1)) :=
	(position when board[position[1], position[2]] = 0)
		when position[1] >= 1 and position[1] <= size(board) and position[2] >= 1 and position[2] <= size(board);

minPosition(x(1)) := minPositionHelper(x, 2, 1, x[1]);
minPositionHelper(x(1), i, minPos, minVal) :=
	minPos when i > size(x) else
	minPositionHelper(x, i + 1, minPos, minVal) when x[i] > minVal else
	minPositionHelper(x, i + 1, i, x[i]);

setBoard(board(2), x, y, value)[i,j] :=
	value when x = i and y = j else
	board[i,j] foreach i within 1 ... size(board), j within 1 ... size(board[1]);

```

8 X 8 board:

```txt

  1 16 31 40  3 18 21 56
 30 39  2 17 42 55  4 19
 15 32 41 46 53 20 57 22
 38 29 48 43 58 45 54  5
 33 14 37 52 47 60 23 62
 28 49 34 59 44 63  6  9
 13 36 51 26 11  8 61 24
 50 27 12 35 64 25 10  7

```

20 X 20 board:

```txt

   1  40  81  90   3  42  77  94   5  44  73 102   7  46  69  62   9  48  51  60
  82  89   2  41  92  95   4  43  76 101   6  45  72 103   8  47  68  61  10  49
  39  80  91  96 153  78  93 100 129  74 109 104 123  70 111 120  63  50  59  52
  88  83 154  79  98 159 152  75 108 105 128  71 110 121 124  67 112 119  64  11
 155  38  97 160 157 200  99 162 151 130 107 122 127 132 141 118 125  66  53  58
  84  87 156 199 176 161 158 201 106 163 150 131 142 145 126 133 140 113  12  65
  37 182  85 178 207 198 175 164 173 216 143 166 149 222 139 146 117 134  57  54
  86 179 206 197 204 177 208 217 202 165 172 221 144 167 148 223 138  55 114  13
 183  36 181 212 209 218 203 174 215 220 227 170 281 224 303 168 147 116 135  56
 180 211 196 205 230 213 238 219 228 171 280 225 302 169 282 343 304 137  14 115
  35 184 231 210 237 246 229 214 279 226 301 298 283 342 367 308 347 344 305 136
 232 195 236 245 234 239 278 247 300 297 284 359 366 309 348 345 368 307 350  15
 185  34 233 240 261 248 287 296 285 358 299 310 341 378 365 384 349 346 369 306
 194 241 250 235 244 277 260 313 294 311 360 373 364 383 354 379 370 385  16 351
  33 186 243 262 249 288 295 286 361 316 357 340 377 372 395 386 353 380 333 388
 242 193 254 251 276 259 314 293 312 321 374 363 398 355 382 371 394 387 352  17
 187  32 263 258 267 252 289 322 315 362 317 356 339 376 399 396 381 334 389 332
 192 255 190 253 264 275 268 271 292 323 320 375 326 397 338 335 390 393  18  21
  31 188 257 266  29 270 273 290  27 318 327 324  25 336 329 400  23  20 331 392
 256 191  30 189 274 265  28 269 272 291  26 319 328 325  24 337 330 391  22  19

```


## Sidef

```ruby
var board = []
var I = 8
var J = 8
var F = (I*J > 99 ? '%3d' : '%2d')

var (i, j) = (I.irand, J.irand)

func from_algebraic(square) {
     if (var match = square.match(/^([a-z])([0-9])\z/)) {
         return(I - Num(match[1]), match[0].ord - 'a'.ord)
     }
     die "Invalid block square: #{square}"
}

func possible_moves(i,j) {
    gather {
        for ni,nj in [
            [i-2,j-1], [i-2,j+1], [i-1,j-2], [i-1,j+2],
            [i+1,j-2], [i+1,j+2], [i+2,j-1], [i+2,j+1],
        ] {
            if ((ni ~~ ^I) && (nj ~~ ^J) && !board[ni][nj]) {
                take([ni, nj])
            }
        }
    }
}

func to_algebraic(i,j) {
    ('a'.ord + j).chr + Str(I - i)
}

if (ARGV[0]) {
    (i, j) = from_algebraic(ARGV[0])
}

var moves = []
for move in (1 .. I*J) {
    moves << to_algebraic(i, j)
    board[i][j] = move
    var min = [9]
    for target in possible_moves(i, j) {
        var (ni, nj) = target...
        var nxt = possible_moves(ni, nj).len
        if (nxt < min[0]) {
            min = [nxt, ni, nj]
        }
    }

    (i, j) = min[1,2]
}

say (moves/4 -> map { .join(', ') }.join("\n") + "\n")

for i in ^I {
    for j in ^J {
        (i%2 == j%2) && print "\e[7m"
        F.printf(board[i][j])
        print "\e[0m"
    }
    print "\n"
}
```



## Swift


```swift
public struct CPoint {
  public var x: Int
  public var y: Int

  public init(x: Int, y: Int) {
    (self.x, self.y) = (x, y)
  }

  public func move(by: (dx: Int, dy: Int)) -> CPoint {
    return CPoint(x: self.x + by.dx, y: self.y + by.dy)
  }
}

extension CPoint: Comparable {
  public static func <(lhs: CPoint, rhs: CPoint) -> Bool {
    if lhs.x == rhs.x {
      return lhs.y < rhs.y
    } else {
      return lhs.x < rhs.x
    }
  }
}

public class KnightsTour {
  public var size: Int { board.count }

  private var board: [[Int]]

  public init(size: Int) {
    board = Array(repeating: Array(repeating: 0, count: size), count: size)
  }

  public func countMoves(forPoint point: CPoint) -> Int {
    return KnightsTour.knightMoves.lazy
      .map(point.move)
      .reduce(0, {count, movedTo in
        return squareAvailable(movedTo) ? count + 1 : count
    })
  }

  public func printBoard() {
    for row in board {
      for x in row {
        print("\(x) ", terminator: "")
      }

      print()
    }

    print()
  }

  private func reset() {
    for i in 0..<size {
      for j in 0..<size {
        board[i][j] = 0
      }
    }
  }

  public func squareAvailable(_ p: CPoint) -> Bool {
    return 0 <= p.x
      && p.x < size
      && 0 <= p.y
      && p.y < size
      && board[p.x][p.y] == 0
  }

  public func tour(startingAt point: CPoint = CPoint(x: 0, y: 0)) -> Bool {
    var step = 2
    var p = point

    reset()

    board[p.x][p.y] = 1

    while step <= size * size {
      let candidates = KnightsTour.knightMoves.lazy
        .map(p.move)
        .map({moved in (moved, self.countMoves(forPoint: moved), self.squareAvailable(moved)) })
        .filter({ $0.2 })

      guard let bestMove = candidates.sorted(by: bestChoice).first else {
        return false
      }

      p = bestMove.0
      board[p.x][p.y] = step

      step += 1
    }

    return true
  }
}

private func bestChoice(_ choice1: (CPoint, Int, Bool), _ choice2: (CPoint, Int, Bool)) -> Bool {
  if choice1.1 == choice2.1 {
    return choice1.0 < choice2.0
  }

  return choice1.1 < choice2.1
}

extension KnightsTour {
  fileprivate static let knightMoves = [
    (2, 1),
    (1, 2),
    (-1, 2),
    (-2, 1),
    (-2, -1),
    (-1, -2),
    (1, -2),
    (2, -1),
  ]
}

let b = KnightsTour(size: 8)

print()

let completed = b.tour(startingAt: CPoint(x: 3, y: 1))

if completed {
  print("Completed tour")
} else {
  print("Did not complete tour")
}

b.printBoard()
```


```txt
Completed tour
23 20 3 32 25 10 5 8
2 33 24 21 4 7 26 11
19 22 51 34 31 28 9 6
50 1 40 29 54 35 12 27
41 18 55 52 61 30 57 36
46 49 44 39 56 53 62 13
17 42 47 60 15 64 37 58
48 45 16 43 38 59 14 63
```



## Tcl


```tcl
package require Tcl 8.6;    # For object support, which makes coding simpler

oo::class create KnightsTour {
    variable width height visited

    constructor {{w 8} {h 8}} {
	set width $w
	set height $h
	set visited {}
    }

    method ValidMoves {square} {
	lassign $square c r
	set moves {}
	foreach {dx dy} {-1 -2  -2 -1  -2 1  -1 2  1 2  2 1  2 -1  1 -2} {
	    set col [expr {($c % $width) + $dx}]
	    set row [expr {($r % $height) + $dy}]
	    if {$row >= 0 && $row < $height && $col >=0 && $col < $width} {
		lappend moves [list $col $row]
	    }
	}
	return $moves
    }

    method CheckSquare {square} {
	set moves 0
	foreach site [my ValidMoves $square] {
	    if {$site ni $visited} {
		incr moves
	    }
	}
	return $moves
    }

    method Next {square} {
	set minimum 9
	set nextSquare {-1 -1}
	foreach site [my ValidMoves $square] {
	    if {$site ni $visited} {
		set count [my CheckSquare $site]
		if {$count < $minimum} {
		    set minimum $count
		    set nextSquare $site
		} elseif {$count == $minimum} {
		    set nextSquare [my Edgemost $nextSquare $site]
		}
	    }
	}
	return $nextSquare
    }

    method Edgemost {a b} {
	lassign $a ca ra
	lassign $b cb rb
	# Calculate distances to edge
	set da [expr {min($ca, $width - 1 - $ca, $ra, $height - 1 - $ra)}]
	set db [expr {min($cb, $width - 1 - $cb, $rb, $height - 1 - $rb)}]
	if {$da < $db} {return $a} else {return $b}
    }

    method FormatSquare {square} {
	lassign $square c r
	format %c%d [expr {97 + $c}] [expr {1 + $r}]
    }

    method constructFrom {initial} {
	while 1 {
	    set visited [list $initial]
	    set square $initial
	    while 1 {
		set square [my Next $square]
		if {$square eq {-1 -1}} {
		    break
		}
		lappend visited $square
	    }
	    if {[llength $visited] == $height*$width} {
		return
	    }
	    puts stderr "rejecting path of length [llength $visited]..."
	}
    }

    method constructRandom {} {
	my constructFrom [list \
		[expr {int(rand()*$width)}] [expr {int(rand()*$height)}]]
    }

    method print {} {
	set s "      "
	foreach square $visited {
	    puts -nonewline "$s[my FormatSquare $square]"
	    if {[incr i]%12} {
		set s " -> "
	    } else {
		set s "\n   -> "
	    }
	}
	puts ""
    }

    method isClosed {} {
	set a [lindex $visited 0]
	set b [lindex $visited end]
	expr {$a in [my ValidMoves $b]}
    }
}
```

Demonstrating:

```tcl
set kt [KnightsTour new]
$kt constructRandom
$kt print
if {[$kt isClosed]} {
    puts "This is a closed tour"
} else {
    puts "This is an open tour"
}
```

Sample output:

```txt

      e6 -> f8 -> h7 -> g5 -> h3 -> g1 -> e2 -> c1 -> a2 -> b4 -> a6 -> b8
   -> d7 -> b6 -> a8 -> c7 -> e8 -> g7 -> h5 -> g3 -> h1 -> f2 -> d1 -> b2
   -> a4 -> c3 -> b1 -> a3 -> b5 -> a7 -> c8 -> e7 -> g8 -> h6 -> f7 -> h8
   -> g6 -> h4 -> g2 -> f4 -> d5 -> f6 -> g4 -> h2 -> f1 -> e3 -> f5 -> d6
   -> e4 -> d2 -> c4 -> a5 -> b7 -> d8 -> c6 -> e5 -> f3 -> e1 -> d3 -> c5
   -> b3 -> a1 -> c2 -> d4
This is a closed tour

```

The above code supports other sizes of boards and starting from nominated locations:

```tcl
set kt [KnightsTour new 7 7]
$kt constructFrom {0 0}
$kt print
if {[$kt isClosed]} {
    puts "This is a closed tour"
} else {
    puts "This is an open tour"
}
```

Which could produce this output:

```txt

      a1 -> c2 -> e1 -> g2 -> f4 -> g6 -> e7 -> f5 -> g7 -> e6 -> g5 -> f7
   -> d6 -> b7 -> a5 -> b3 -> c1 -> a2 -> b4 -> a6 -> c7 -> b5 -> a7 -> c6
   -> d4 -> e2 -> g1 -> f3 -> d2 -> f1 -> g3 -> e4 -> f2 -> g4 -> f6 -> d7
   -> e5 -> d3 -> c5 -> a4 -> b2 -> d1 -> e3 -> d5 -> b6 -> c4 -> a3 -> b1
   -> c3
This is an open tour

```



## XPL0


```XPL0
int     Board(8+2+2, 8+2+2);            \board array with borders
int     LegalX, LegalY;                 \arrays of legal moves
def     IntSize=4;                      \number of bytes in an integer (4 or 2)
include c:\cxpl\codes;                  \intrinsic 'code' declarations


func    Try(I, X, Y);                   \Make a tentative move from X,Y
int     I, X, Y;
int     K, U, V;
[for K:= 0 to 8-1 do                    \for all possible moves...
    [U:= X + LegalX(K);                 \U and V are next square
     V:= Y + LegalY(K);
     if Board(U,V) = 0 then             \if square has not been visited then
        [Board(U,V):= I;                \ mark square with sequence number
        if I = 8*8 then return true;
        if Try(I+1, U, V) then return true      \led to solution?
        else Board(U,V):= 0;                    \no, undo tenative move
        ];
    ];
return false;
];      \Try


int  I, J;
[LegalX:= [2, 1, -1, -2, -2, -1, 1, 2];
 LegalY:= [1, 2, 2, 1, -1, -2, -2, -1];

for J:= 0 to 8+2+2-1 do                 \set up surrounding border for speed
    for I:= 0 to 8+2+2-1 do
        Board(I,J):= 1;
for J:= 0 to 8+2+2-1 do                 \reposition Board(0,0) to Board(2,2)
        Board(J):= Board(J) + 2*IntSize;
Board:= Board + 2*IntSize;
for J:= 0 to 8-1 do                     \empty board
    for I:= 0 to 8-1 do
        Board(I,J):= 0;
Text(0, "Starting square (1-8,1-8): ");  I:= IntIn(0)-1;  J:= IntIn(0)-1;
Board(I,J):= 1;                         \starting location is 0,0

if Try(2, I, J) then                    \try to find second square
        [for J:= 0 to 8-1 do            \draw board with knight's move sequence
            [for I:= 0 to 8-1 do
                [if Board(I,J) < 10 then ChOut(0, ^ );
                IntOut(0, Board(I,J));
                ChOut(0, ^ );
                ];
            CrLf(0);
            ];
        ]
else    Text(0, "No Solution.^M^J");
]
```


Example output:

```txt

Starting square (1-8,1-8): 1 1
 1 38 59 36 43 48 57 52
60 35  2 49 58 51 44 47
39 32 37 42  3 46 53 56
34 61 40 27 50 55  4 45
31 10 33 62 41 26 23 54
18 63 28 11 24 21 14  5
 9 30 19 16  7 12 25 22
64 17  8 29 20 15  6 13

```



## XSLT

This solution is for XSLT 3.0 Working Draft 10 (July 2012). This solution, originally reported on [http://seanbdurkin.id.au/pascaliburnus2/archives/10 this blog post], will be updated or removed when the final version of XSLT 3.0 is released.

First we build a generic package for solving any kind of tour over the chess board. Here it is…
<lang>
<xsl:package xsl:version="3.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:fn="http://www.w3.org/2005/xpath-functions"
  xmlns:tour="http://www.seanbdurkin.id.au/tour"
  name="tour:tours">
<xsl:stylesheet>
  <xsl:function name="tour:manufacture-square"
       as="element(square)" visibility="public">
    <xsl:param name="rank" as="xs:integer" />
    <xsl:param name="file" as="xs:integer" />
    <square file="$file" rank="$rank" />
  </xsl:function>

  <xsl:function name="tour:on-board" as="xs:boolean" visibility="public">
    <xsl:param name="rank" as="xs:integer" />
    <xsl:param name="file" as="xs:integer" />
    <xsl:copy-of select="($rank ge 1) and ($rank le 8) and
                         ($file ge 1) and ($file le 8)" />
  </xsl:function>

  <xsl:function name="tour:solve-tour" as="item()*" visibility="public">
    <!-- Solves the tour for any specified piece. -->
    <!-- Outputs either a full solution of 64 squares, of if fail,
         a copy of the $state input. -->
    <xsl:param name="state" as="item()+" />
    <xsl:variable name="compute-possible-moves"
      select="$state[. instance of function(*)]"
      as="function(element(square)) as element(square)*">
    <xsl:variable name="way-points" select="$state/self::square" />
    <xsl:choose>
      <xsl:when test="count($way-points) eq 64">
        <xsl:sequence ="$state" />
      </xsl:when>
      <xsl:otherwise>
        <xsl:sequence select="
      let $try-move := function( $state as item()*, $move as item()) as item()*)
            {
             if $state/self::square[@file=$move/@file]
                                   [@rank=$move/@rank]
               then $state
               else tour:solve-tour( ( $state, $move) )
                },
              $possible-moves := $compute-possible-moves( $way-points[last()])
          return if empty( $possible-moves) then $state
                     else fn:fold-left( $try-move, $state, $possible-moves)" />
      </xsl:otherwise>
    </xsl:choose>
  </xsl:variable></xsl:function>
</xsl:stylesheet>

<xsl:expose component="function"
  names="tour:manufacture-square tour:on-board tour:solve-tour"
  visibility="public" />

</xsl:package>

```


And now for the style-sheet to solve the Knight’s tour…

<lang>
<xsl:stylesheet version="3.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:fn="http://www.w3.org/2005/xpath-functions"
  xmlns:tour="http://www.seanbdurkin.id.au/tour"
  exclude-result-prefixes="xsl fn xs tour">
<xsl:use-package name="tour:tours" />
<xsl:output indent="yes" encoding="UTF-8" omit-xml-declaration="yes" />
<xsl:mode on-no-match="shallow-copy" streamable="yes"/>

<xsl:template match="knight[square]">
  <xsl:variable name="error">
    <error>Failed to find solution to Knight's Tour.</error>
  </xsl:variable>
  <xsl:copy>
    <xsl:copy-of select="
    let $final-state := tour:solve-tour((
    function( $piece-position as element(square)) as element(square)*
      { (: This function defines a knight's move. :)
        let $r0 := number( $piece-position/@rank),
        let $f0 := number( $piece-position/@file),
        for $r in -2..2, $f in -2..2 return
          if (abs($r) + abs($f) eq 3) and
             tour:on-board($r+$r0, $f+$f0) then
            tour:manufacture-square($r+$r0, $f+$f0)
          else ()
      }
      , current()/square)),
     $solution := $final-state/self::square
    return if count($solution) eq 64 then $solution
           else $error/*" />
  </xsl:copy>
</xsl:template>

<!-- Add templates for other piece types if you want to solve
     their tours too. Solve by calling tour:solve-tour() .    -->

</xsl:stylesheet>

```


So an input like this…

<lang>
<tt>
 <knight>
   <square file="1" rank="1" />
 </knight>
</tt>

```


…should be transformed in something like this…

<lang>
<tt>
 <knight>
   <square file="1" rank="1" />
   <square file="2" rank="3" />
   <square file="1" rank="5" />
   ... etc for 64 squares.
 </knight>
</tt>

```



## zkl


```zkl
   // Use Warnsdorff's rule to perform a knights tour of a 8x8 board in
   // linear time.
   // See Pohl, Ira (July 1967),
   //   "A method for finding Hamilton paths and Knight's tours"
   //   http://portal.acm.org/citation.cfm?id=363463
   // Uses back tracking as a tie breaker (for the few cases in a 8x8 tour)
class Board{
   var[const]deltas=[[(dx,dy); T(-2,2); T(-1,1); _]].extend(
                    [[(dx,dy); T(-1,1); T(-2,2); _]]);
   fcn init{
      var board=L();
      (0).pump(64,board.append.fpM("1-",Void)); // fill board with Void
   }
   fcn idx(x,y)     { x*8+y }
   fcn isMoveOK(x,y){ (0<=x<8) and (0<=y<8) and Void==board[idx(x,y)] }
   fcn gyrate(x,y,f){   // walk all legal moves from (a,b)
      deltas.pump(List,'wrap([(dx,dy)]){
         x+=dx; y+=dy; if(isMoveOK(x,y)) f(x,y); else Void.Skip
      });
   }
   fcn count(x,y){ n:=Ref(0); gyrate(x,y,n.inc); n.value }
   fcn moves(x,y){ gyrate(x,y,fcn(x,y){ T(x,y,count(x,y)) })}
   fcn knightsTour(x=0,y=0,n=1){  // using Warnsdorff's rule
      board[idx(x,y)]=n;
      while(m:=moves(x,y)){
         min:=m.reduce('wrap(pc,[(_,_,c)]){ (pc<c) and pc or c },9);
	 m=m.filter('wrap([(_,_,c)]){ c==min });  // moves with same min moves
	 if(m.len()>1){ // tie breaker time, may need to backtrack
	    bs:=board.copy();
	    if (64==m.pump(Void,'wrap([(a,b)]){
	       board[idx(a,b)]=n;
	       n2:=knightsTour(a,b,n+1);
	       if (n2==64) return(Void.Stop,n2); // found a solution
	       board=bs.copy();
	    })) return(64);
	    return(0);
         }
	 else{
	    x,y=m[0]; n+=1;
	    board[idx(x,y)]=n;
	 }
      } //while
      return(n);
   }
   fcn toString{ board.pump(String,T(Void.Read,7),
         fcn(ns){ vm.arglist.apply("%2s".fmt).concat(",")+"\n" });
   }
}
```



```zkl
b:=Board(); b.knightsTour(3,3);
b.println();
```

```txt

 3,34, 5,54,19,36,29,50
 6,21, 2,35,56,49,18,37
33, 4,55,20,53,30,51,28
22, 7,32, 1,48,57,38,17
11,44,23,62,31,52,27,58
 8,63,10,45,60,47,16,39
43,12,61,24,41,14,59,26
64, 9,42,13,46,25,40,15

```

Check that a solution for all squares is found:

```zkl
[[(x,y); [0..7]; [0..7];
   { b:=Board(); n:=b.knightsTour(x,y); if(n!=64) b.println(">>>",x,",",y) } ]];
```

{{out}}Nada

