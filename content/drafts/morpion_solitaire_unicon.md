+++
title = "Morpion solitaire/Unicon"
description = ""
date = 2012-02-21T04:37:41Z
aliases = []
[extra]
id = 11356
[taxonomies]
categories = []
tags = []
+++

This example goes beyond the task goal of playing a random game of Morpion solitaire.  The code is divided into two sections (a) to support the basic task and (b) extensions.  The program was designed as a first cut/test bed more for understanding and exploring the problem space rather than for high efficiency.  The program is structured to allow its behaviour to be configured (see options and M_ vars).  Some of the features and extensions include:  
* Playing single games or running multigame simulations to find and capture the best games
* Producing re-loadable game records and the ability to replay a saved game from a given position forward
* The ability to reproduce purely random games from the random number seed 
* Limiting the initial random moves to 1 of 4 cases (inside corners (1 type) x 8, outside corners (2 types) x 8, outside valley (1 type x 4)) not 1 of 28.  In the case of this program these are all in the North East quadrant.
* Plays both 5T (by default) and 5D variants 
* Plugable modules for functions including applying player strategy, position evaluation (fitness/heuristics)to facilitate quick experimentation, changing game read/write functions, etc., etc.

Using random play in many summary runs, the highest (5T) score achieved was 92. While it was fairly easy to push random games into the low 80's running simulations of as little as a few hundred games, going beyond the highest score without adding some smarts will require some very long runs and is unlikely to result in any significant progress.  Observing multigame runs show that play advances fairly quickly before progress grinds to a virtual halt.  Many runs peak in the first few hundred or thousand games.

The ability to replay recorded games provides a way to study the behaviour of the problem.  Selecting successful random games, truncating them and running multigame simulations using a successful base shows that the seeds of success are sown early.  It was possible to better the results of good random game scores (mid 80s) pushing the new scores up into the mid 90s.  Unfortunately this technique when applied to Chris Rosin's 177 move record game did not produce any new records  :(. Surprisingly however, it was possible to produce games in the high 160's! using random play with a truncated (approx 60%) version of this game.  

The observation that a game becomes bad or good fairly early suggests that finding an intelligent forward looking position fitness evaluator is going to be a challenge to say the least.

It's also possible to capture bad games. This might provide some kind of comparison against good games when considering fitness evaluators.  Or not.

Internally, the grid is automatically expanded as needed whenever a cell is played on an outer edge.  When this happens only the affected side is expanded.  As a side effect the grid numbering will seem unusual. The game log shows all row/col coordinates relative to the 1,1 origin  located at the intersection of the lines containing top and left most edges of the cross.  A fixed grid might be more efficient.  With the ability to detect an off page condition and save and replay the game later this could be easier to deal with.  The issue of grid origin comes up all the time when working with the code and debugging output.

The program produces a crude ASCII art grid; however and more importantly it produces a game log that can be replayed in the Pentasol free player.  This can be used to capture graphical grids as well as providing independent validation of the games.

Most of the [http://www.morpionsolitaire.com/English/RecordsGrids5T.htm Record Games] have downloadable games in this notation.  For example [http://www.morpionsolitaire.com/Grid5T178Rosin.txt Chris Rosin's 178 move grid].

For more see: Morpion -h|-help

== Basic Solution ==
The code need to solve the task requirements is found in this section.  Just delete the $define below.  The basic code will play a single random game, show an ASCII art grid, and produce a log in Pentasol compatible notation.  On any given run your most likely to see a game with scores in the 20's or 60's.

###  Main and Core Game Play Procedures 


```Unicon
link printf,strings,options

$define MORPVER "1.7g"                    # version 
$define EXTENDED 1                        # delete line for basic

procedure main(A)                         # Morphion 
$ifdef EXTENDED
   MorpionConf(A)                         # conf extended version
   if \M_ReplayFile then ReplayMorpion() 
   else if \M_Limit === 1 then ShowGame(SingleMorpion())
   else MultiMorphion(\M_Limit)
$else
   printf("--- Morpion Solitaire 5 (v%s) (single random game)---\n\n",MORPVER)  
   M_Strategy     := RandomPlayer
   M_Mvalid       := ValidMove5T          # can be changed to 5D
   M_WriteGame    := WriteMoveLogPS
   M_Output       := &output 
   ShowGame(SingleMorpion())
$endif
end

$define XEMPTY "."                        # symbols used in Grid
$define XINIT  "*"
$define XUSED  "+"
$define DHOR "-"                          # Directions for moves
$define DVER "|"
$define DRD  "\\"
$define DLD  "/"
$define DALL "-|/\\"

global  M_Strategy,M_Mvalid               # Pluggable procedures
global  M_Output                          # output files
global  M_WriteGame                       # for logger
 
record morpiongame(grid,score,            # the grid & score
                  log,history,            # move log and replayable history log
                  roff,coff,center,       # origin expansion offsets and center 
                  pool,                   # pool of avail moves
                  move,                   # selected move
                  count,                  # game number (multi-game)
                  rseed)                  # &random at start of random play  
record morpionmove(direction,move,line,roff,coff)  # move & line data   
record morpioncell(symbol,direction,row,col)       # a grid cell                
record MorpionGameRecord(ref,row,col,darow,dacol)  # record of game

procedure SingleMorpion(MG,N)                      #: Play a game silently                        
   /MG := SetupM5Grid()                            # start game with new grid?
   while MorphionMove(M_Strategy(MG)) do           # keep moving     
      if MG.score >=  \N then break                # unless truncated   
   return MG
end

procedure MorphionMove(MG)                         #: make a move   
   (M := MG.move).roff := MG.roff                  # copy offsets
   M.coff := MG.coff                  
   put(MG.history,M)                               # save history
   MG.score +:= 1                                  # and score
   \M_LogDetails(MG,M)                             # for analysis
   every x := !M.line do {                         # draw the line 
      g := MG.grid[x[1],x[2]]
      g.direction ||:= M.direction       
      /g.symbol := XUSED                           # remove / for all XUSED
      }
   return
end

procedure ScanGrid(MG)                             #: Scan all grid lines
   G := ExpandGrid(MG).grid                        # expand the grid if needed
   MG.pool := []                                   # candidate moves   
   every c := 1 & r := 1 to *G do                  # horizontal
      ScanGridLine(G,r,c,0,+1,DHOR,MG.pool)
   every r := 1 & c := 1 to *G[1] do               # vertical
      ScanGridLine(G,r,c,+1,0,DVER,MG.pool)   
   every ( c := 1 & r := 1 to *G-4 ) |
         ( c := 2 to *G[r := 1] ) do               # down & right
      ScanGridLine(G,r,c,+1,+1,DRD,MG.pool)  
   every ( r := 2 to *G-4 & c := *G[r] ) |
         ( c := 5 to *G[r := 1] ) do               # down & left
   ScanGridLine(G,r,c,+1,-1,DLD,MG.pool)  
   if MG.score = 0 & M_Strategy ~=== Replayer then {  # move 1 special case
      every put(pool1 := [], MG.pool[2|19|20|26])  # cor. o(2), i(1), val o(1) 
      MG.pool := pool1       
      }
   if *MG.pool > 0 then return MG                 
end      

procedure ScanGridLine(G,r0,c0,ri,ci,dir,pool)     #: scan 1 grid line (5T/D)
   local L5,M,r,c,x
   L5 := []                          
   r := r0 - ri & c := c0 -ci                      # one step back
   while put(L5,G[r +:= ri,c +:= ci]) do {          
      while *L5 > 5 do pop(L5)                     # too long ? 
      if *L5 < 5 then next                         # too short ?
      if M_Mvalid(L5,dir) then {                   # just right, but valid?
         put(pool,M := morpionmove(dir,,[]))       # add to pool of valid moves               
         every x := L5[i := 1 to *L5] do  {
            put(M.line,[x.row,x.col])
            if /x.symbol then M.move := i
            }
         }
      }
   return pool
end

procedure ValidMove5T(L,dir)                 #: Succeed if L has valid 5T move
   local i,j
   if *L ~= 5 then fail                      # wrong count
   every (i := 0) +:= (/(!L).symbol,1)  
   if i ~= 1 then fail                       # more than 1 avail space
   every (j := 0) +:= ( find(dir,(!L).direction), 1) 
   if j > 1 then fail                        # no overlap, =1 implies at an end
   return                                    # that's it!
end

procedure ValidMove5D(L,dir)                 #: Succeed if L has valid 5D move  #@@
   local i,j
   if *L ~= 5 then fail                      # wrong count
   every (i := 0) +:= (/(!L).symbol,1)  
   if i ~= 1 then fail                       # more than 1 avail space
   every (j := 0) +:= ( find(dir,(!L).direction), 1) 
   if j > 0 then fail                        # no overlap, =1 implies at an end
   return                                    # that's it!
end

procedure SetupM5Grid()                      #: construct 5T/D grid & cross 
   local G,r,c,s
   every !(G := list(10)) := list(10)                          # Grid
   every G[r := 1 to 10, c := 1 to 10] := morpioncell(,"",r,c) # Empties  
   every s := ![[1,4],[4,1],[4,7],[7,1],[7,7],[10,4]] do {     # Cross
      every r := s[1] & c := s[2] + (0 to 3) do
         G[r,c] := morpioncell(XINIT,"",r,c)    
      every r := s[2] + (0 to 3) & c := s[1] do         
         G[r,c] := morpioncell(XINIT,"",r,c)    
      }                                                     
   return morpiongame(G,0,[],[],0,0,1 + (*G-1)/2.)    # Create game
end

procedure ExpandGrid(MG)                     #: expand any touching sides  
   local r,c,rn,cn,C
   rn := *(G := MG.grid)                     # capture ...
   cn := *G[1]                               # ... entry dimensions
   if \(!G)[1].symbol then {                 # left edge  
      MG.coff +:= 1   
      every (cn | (!!G).col) +:= 1
      every push(G[r := 1 to rn],morpioncell(,"",r,1))  
      }   
   if \(!G)[cn].symbol then  {               # right edge
      cn +:= 1   
      every put(G[r := 1 to rn],morpioncell(,"",r,cn))  
      }
   if \(!G[1]).symbol then {                 # top edge
      MG.roff +:= 1
      every (rn | (!!G).row) +:= 1
      push(G,C := list(cn))
      every C[c := 1 to cn] := morpioncell(,"",1,c)     
      }   
   if \(!G[rn]).symbol then {                # bottom edge
      rn +:= 1
      put(G,C := list(cn)) 
      every C[c := 1 to cn] := morpioncell(,"",rn,c)  
      }
   return MG
end

procedure ShowGame(MG)                 #: show games
   if M_Output === &output then   
      every (\(PrintGrid|WriteMoveLog|M_PrintDetails))(MG) 
   else                                # header first to output, game saved
      every (\(WriteMoveLog|PrintGrid|M_PrintDetails))(MG)                 
end

procedure PrintGrid(MG)                      #: print the current Grid  
   G := MG.grid
   every (ruler := "   ") ||:= (1 to *G[1]) % 10     
   fprintf(M_Output,"\nMorphion Solitare Grid (move=%i):\n%s\n",MG.score,ruler)                                              
   every r :=  1 to *G do {
      fprintf(M_Output,"%s ",right(r%100,2))                                  
      every c := 1 to *(G[r]) do 
         fprintf(M_Output,"%s",\G[r,c].symbol | XEMPTY)    
      fprintf(M_Output,"\n")
      }   
   fprintf(M_Output,"%s\n",ruler)                                                
   return MG
end

procedure RandomPlayer(MG)                      #: Simulate random player   
   if &random := \M_Rseed then M_Rseed := &null # set seed if given only once 
   /MG.rseed := &random                         # seed for this game          
   if MG.move := ?ScanGrid(MG).pool then return MG
end
```



###  Game Logging 


```Unicon
procedure WriteMoveLog(MG)                     #: write move log wrapper    
   if \M_GameSave then {
      savegame := sprintf("Games/%s/%i-L%i",M_GameType,*MG.history,M_Limit) 
      if M_Limit ~= 1 then 
         savegame ||:= sprintf("(%i)",MG.count)
      if \M_ReplayFile then {
         fn := map(M_ReplayFile,"/\\","__")
         fn ? (="Games/", savegame ||:= "-RF" || tab(find(".txt")))
         savegame ||:= "-RN" || (0 < \M_ReplayAfter) 
         }
      savegame ||:= sprintf("_%s-%s-F%s.txt",deletec(&date,'/'),deletec(&clock,':'),M_GameFmt)  
      M_GameSave  := savegame                                 
      fprintf(M_Output,WriteMoveLogHeader(MG))  # write header, game is saved
      f := open(savegame,"w") | stop("Unable to open ",savegame," for writing")
      fprintf(f,M_WriteGame(MG),M_Config)       # call desired writer for output/save
      close(f)
      }
   else
      fprintf(M_Output,M_WriteGame(MG))
end

procedure WriteMoveLogHeader(MG)              #: write common header comments
   return sprintf("#\n# Game Record for Morphion %s game of %i moves\n_
                  # Date: %s\n# Saved: %s\n# &random: %i\n_
                  # ReplayFile: %s (%s moves)\n#\n",
                  \M_GameType|"5T",MG.score,&date,\M_GameSave|"* none *",
                  MG.rseed,\M_ReplayFile|"* none *",\M_ReplayAfter|"* all *")
end

procedure WriteMoveLogPS(MG)                  #: write pentasol style move log
   l := WriteMoveLogHeader(MG)
   l ||:= sprintf("# Pentasol compatible format\n#\n#\n_
                  # Morpion Solitaire game\n#\n_
                  #     XXXX\n#     X  X\n#     X  X\n_
                  #  XXXR  XXXX\n#  X        X\n#  X        X\n_
                  #  XXXX  XXXX\n#     X  X\n#     X  X\n#     XXXX\n#\n_
                  # R = reference point\n_
                  # List of moves starts with reference point (col,row)\n_
                  # Lines are\n#    (col,row) <direction> <+/-centerdist>\n_
                  #    distance to center is left side or top edge\n#\n")           
   l ||:= sprintf("(%i,%i)\n",4+MG.coff,4+MG.roff)                   
   every l ||:= FormatMoveLogPS(MG,!MG.history)
   return l || "#"
end

procedure FormatMoveLogPS(MG,m)                 #: format a PS move
   d := if m.direction == "/" then m.move-3 else 3-m.move
   return sprintf("(%i,%i) %s %s%d\n",
      m.line[m.move,2]-m.coff+MG.coff,m.line[m.move,1]-m.roff+MG.roff,
      m.direction,(d < 0,"-")|(d = 0,"")|"+",abs(d))
end
```


== Extended Framework ==
None of the code below is needed to satisfy the basic task.  It supports the extended framework which includes, command line options, reading and replaying games from a saved file, running mass simulations to glean the best games, and some code for detailed analysis if I ever get around to experimenting with other than random strategy.
=== Interface, Parameters, Globals ===


```Unicon
$ifdef EXTENDED

#  --- Interface, Parameters, Additional Globals --- 

global  M_Eval                                  # Pluggable procedure
global  M_SrchWid,M_SrchDep                     # For strategy modules
global  M_LogDetails,M_PrintDetails             # Misc. 
global  M_CommandLine,M_Config,M_GameType,M_GameSave
global  M_Limit,M_StatUpd,M_BestL,M_WorstL      # Multi-game simulation options
global  M_ReplayFile,M_ReplayAfter,M_Rseed      # For game replay
global  M_ReadGame,M_GameFmt                    # Game formats to use 
global  M_ChartW,M_ChartG                       # histogram 

# --- Beginning of Non-core (Extended) code ---

procedure MorpionConf(A)                           # Configure the Solver
   M_CommandLine := copy(A)                        # preserve
   os := "-Q! -L+ -limit+ -V: -variant: -seed+ "                        
   os ||:= "-R! -replay! -RF: -RN+ -save! -RW: -RR: "   
   os ||:= "-histwidth+ -histgroup+ -HW+ -HG+ "               
   os ||:= "-UN+ -SW+ -SD+ -A: -E+ -B+ -W+"
   os ||:= "-details! "
   opt := options(A,os,Usage)                      # -<anything else> gets help
   M_Limit     := ( 0 <= integer(\opt["limit"|"L"])) | 1  
   M_Rseed := \opt["seed"]   
   M_Mvalid := case opt["V"|"variant"] of {
      "5D"     :  (M_GameType := "5D", ValidMove5D)
      default  :  (M_GameType := "5T", ValidMove5T)   # also 5T  
      }                                           
   M_ReadGame  := case map(\opt["RR"]) | &null of { 
      default  : (M_GameFmt := "ps", ReadMoveLogPS)
      "0"      : (M_GameFmt := "0",  ReadMoveLogOrig) # deprecated
      }  
   M_WriteGame := case map(\opt["RW"]) | &null  of { 
      default  : (M_GameFmt := "ps", WriteMoveLogPS)
      "0"      : (M_GameFmt := "0",  WriteMoveLogOrig) # deprecated
      }  
   M_Strategy := case opt["A"] of {                    
      "A1"     : (def_un := 50,  PlayerA1) 
      default  : (def_un := 500, RandomPlayer)
      }       
   M_Eval := case opt["E"] of { 
      "1"      :  Score1         # test
      default  :  &null          # also "0"      
      }
   M_ChartW  := (40 <= \opt["histwidth"|"HW"]) | 80
   M_ChartG  := \opt["histgroup"|"HG"]         | 5
   M_LogDetails := if \opt["details"] then LogDetails else 1
   M_PrintDetails := if \opt["details"] then PrintDetails else 1  
   M_StatUpd      := (0 < \opt["UN"]) | def_un   
   M_BestL        := (0 < \opt["B"])  | 5  
   M_WorstL       := (0 < \opt["W"])  | 0    
   M_SrchWid      := (0 < \opt["SW"]) | 5   
   M_SrchDep      := (0 < \opt["SD"]) | 5    
   if \opt["R"|"replay"] then {
      M_ReplayFile   := \opt["RF"]       | "Games/5T/177-5T-rosin.txt" 
      M_ReplayAfter  := (0 < \opt["RN"]) | &null
      }
   else M_ReplayFile   := &null     
   if \(M_GameSave  := opt["save"]) then {
      fn := sprintf("Runs/%s-L%i-",M_GameType,M_Limit)
      fn ||:= sprintf("RF%s-",map(\M_ReplayFile,"/\\","__"))
      fn ||:= sprintf("RN%i-",\M_ReplayAfter)
      fn ||:= sprintf("%s-%s.txt",deletec(&date,'/'),deletec(&clock,':'))
      M_Output := open(fn,"w") 
      }
   /M_Output := &output 
   
   c := sprintf("# --- Morpion Solitaire 5 (v%s) ---\n#\n",MORPVER)  
   c ||:= "# Command line options :"
   every c ||:= " " || !A
   c ||:=         "\n# Summary of Morpion Configuration:\n"  
   c ||:= sprintf("#   Variant (5T/D) move validation      = %i\n",M_Mvalid)    
   c ||:= sprintf("#   Games to play                       = %s\n",(
                   0 < M_Limit) | "* unlimited *")     
   c ||:=         "#   Multi-game options:\n"
   c ||:= sprintf("#   - Status Updates                    = %i\n",M_StatUpd) 
   c ||:= sprintf("#   - Keep best                         = %i\n",M_BestL)   
   c ||:= sprintf("#   - Keep worst                        = %i\n",M_WorstL)  
   c ||:= sprintf("#   - Histogram width                   = %i\n",M_ChartW)  
   c ||:= sprintf("#   - Histogram grouping                = %i\n",M_ChartG)  
   c ||:= sprintf("#   Games will be saved                 = %s\n",
                  (\M_GameSave, "Yes") | "* No *")
   c ||:= sprintf("#   - Format for game file (write)      = %i\n",\M_WriteGame)    
   c ||:=         "#   Replaying\n"
   c ||:= sprintf("#   - Format for game file (read)       = %i\n",\M_ReadGame)   
   c ||:= sprintf("#   - Game file to be replayed          = %s\n",
                  \M_ReplayFile | "* None *")    
   c ||:= sprintf("#   - Moves to replay                   = %i\n",
                  0 ~= \M_ReplayAfter)  
   c ||:= sprintf("#   Player Strategy                     = %i\n",M_Strategy)  
   c ||:= sprintf("#   - Seed for &random                  = %i\n",\M_Rseed)     
   c ||:= sprintf("#   - Position Fitness Evaluator        = %s\n",
                  image(\M_Eval) | "* None *")     
   c ||:= sprintf("#   - Search Width (strategy dependant) = %i\n",M_SrchWid)   
   c ||:= sprintf("#   - Search Depth (strategy dependant) = %i\n",M_SrchDep)  
   c ||:= sprintf("#   Log Details for analysis            = %s\n",
                  if M_LogDetails === 1 then "No" else "Yes")  
   c ||:=         "#\n"
   M_Config := c
   if \opt["Q"] then stop(M_Config,"-Q Stops run after processing options")
   else fprintf(M_Output,M_Config)
   /M_Eval := Scorefail          
end

procedure Usage()
   fprintf(&errout,"_
      Morphion [options]  Plays the 5T/D variant of Morphion Solitaire\n_ 
      Arguments : ")  
   every fprintf(&errout," %s",!M_CommandLine)
   fprintf(&errout,"_                                                         
      Morphion [options]  Plays the 5T/D variant of Morphion Solitaire\n_ 
      Where options are:\n_                           
      \t-A\tchoose player strategy approach (default=random, future)\n_
      \t-E\tSpecifiy an position fitness evaluation function (default none, future)\n_       
      \t-SW\tSearch width (strategy dependent, future)\n_
      \t-SD\tSearch depth (strategy dependent, future)\n_    
      \t-V|-variant\t5D or 5T (default)\n_
      \t-R|-replay\treplay\n_
      \t-RF\tfile containing game record to be replayed\n_
      \t-RN\tnumber of moves to replay (0=all)\n_
      \t-RR\tgame recording format to read (ps=pentasol(default), 0=original)\n_
      \t-RW\tgame recording format to write ps=pentasol(default), 0=original)\n_
      \t-save\tsave the best (-B) and worst (-W) games\n_
      \t-seed\tstart the seed of the random number at this value\n_  
      \t-L|-limit\tgames to play (if 0 or less then play until any of 'XxQq' is pressed\n_
      \t\tnote for larger n this benefits from larger BLKSIZE, STRSIZE environment variables\n_   
      \t-HW|-histwidth\twidth (cols) of histogram of scores\n_
      \t-HG|-histgroup\tsize of groups (buckets) for histogram\n_
      \t-UN\tGive status update notifications every n simulations\n_  
      \t-B\tKeep best n games of unique length (default 5)\n_        
      \t-W\tKeep worst n games of unique length (default 3)\n_
      \t-details\tLog game details for analysis\n_
      \t-Q\t(debugging) terminates after options processing\n_
      \t-?|-h|-help\tthis help text")  
   stop()
end
```



###  Multigame Simulation and Monitoring Support 


```Unicon

procedure MultiMorphion(N,MG)                      #: Simulate N games using MG
      etime := -&time 
      scores := table(n := 0)   
      every bestL|worstL  := []        
      if N <= 0 then N := "unlimited"         
      repeat {
         if n >= numeric(N) then break else n +:= 1  
         mg := SingleMorpion(deepcopy(\MG)|&null)  # play out game
         scores[mg.score] +:= 1                    # count score
         mg.count := n                             # game number
         if short := ( /short | short.score >= mg.score, mg) then {   
            push(worstL,short)                     # keep worst 
            if *worstL > M_WorstL then pull(worstL)
            }
         if ( /long | long.score <= mg.score) then {                 
            bestcnt := if (\long).score = mg.score then bestcnt + 1 else 1 
            long := mg                                                 
            put(bestL,long)                        # keep best
            if *bestL > M_BestL then get(bestL)
            fprintf(M_Output,"Longest game %i after %i simulations &random=%i.\n",                
                   long.score,n,long.rseed)                                       
            fprintf(&errout,"\r%i of %s simulations, long=%i(%i) (%s %s)",                  
                            n,N,long.score,bestcnt,&date,&clock)                              
            }            
         if (n % M_StatUpd) = 0 then               # say we're alive & working
            fprintf(&errout,"\r%i of %s simulations, long=%i(%i) (%s %s)",       
                            n,N,long.score,bestcnt,&date,&clock)                  
         if kbhit() & getch() == !"QqXx" then      # exit if any q/x
            break fprintf(&errout,"\nExiting after %i simulations.\n",n)  
         }
      etime +:= &time   
      avg := 0.0
      short := key(scores) \ 1                  # 1 key only
      every i := key(scores) do {               # summarize stats
         short >:= i
         avg +:= i * scores[i]
         }       
      fprintf(M_Output,"\nResults from Sample of %i games of Morpion 5T/D:\n",n)
      fprintf(M_Output,"Shortest game was %i moves.\n",short)
      fprintf(M_Output,"Average game was %i moves.\n",avg /:= n)
      fprintf(M_Output,"Longest game was %i moves.\n",long.score)
      fprintf(M_Output,"Average time/game is %i ms.\n",etime/real(n))
      fprintf(M_Output,"&random is now %i.\n",&random)                     
      GraphScores(scores)              # graph results
      fprintf(M_Output,"\nLongest (%i) Game(s):\n",M_BestL)
      every ShowGame(!reverse(bestL))  # show longest game(s) and log
      fprintf(M_Output,"\nShortest (%i) Game(s):\n",M_WorstL) 
      every ShowGame(!worstL)          # show longest game(s) and log     
      MemUsage()                       # diagnostic        
end

procedure GraphScores(S)               #: graph results
   chart := []
   every s := key(S) do {              # by score 
      n := s/M_ChartG+1                  # chunks of ...
      until chart[n] do put(chart,0)   # grow chart to need
      chart[n] +:= S[s]
      }
   s := (1 < max!chart/M_ChartW | 1)     # scale 
   fprintf(M_Output,"\nSummary of Results every '*' = %d games\n",s)
   every n := 1 to *chart do 
      fprintf(M_Output,"%3d | (%6d) %s\n",(n-1)*M_ChartG,chart[n],repl("*",chart[n]/s))
end

procedure MemUsage()                   #: monitor usage
      fprintf(M_Output,"\nTotal run time = %i ms\n",&time)
      fprintf(M_Output,"&allocated   (Total,static,string,block) : ")
      every fprintf(M_Output," %i",&allocated) ;  fprintf(M_Output,"\n")      
      fprintf(M_Output,"&collections (Total,static,string,block) : ")
      every fprintf(M_Output," %i",&collections) ;  fprintf(M_Output,"\n")
      fprintf(M_Output,"&regions     ( -   , -    ,string,block) : ")
      every fprintf(M_Output," %s","-"|&regions) ;  fprintf(M_Output,"\n")          
      fprintf(M_Output,"&storage     ( -   , -    ,string,block) : ")
      every fprintf(M_Output," %s","-"|&storage) ;  fprintf(M_Output,"\n\n")  
      fprintf(M_Output,"Icon/Unicon version %s\n",&version)       
end 
```



###  Game Replayer 


```Unicon
procedure ReplayMorpion()                    #: Handle recorded games
   Replayer(M := ReadMoveLog(M_ReplayFile))  # read game and save data 
   M_Strategy := Replayer  
   if /M_ReplayAfter | (M_ReplayAfter > *M) then {
      fprintf(M_Output,"Single game replay\n")
      ShowGame(SingleMorpion())
      }
   else {                                       # truncation replay  
      MG := SingleMorpion(,M_ReplayAfter)       # play shortened game
      M_Strategy := RandomPlayer     
      if M_Limit === 1 then 
         ShowGame(SingleMorpion(MG))            # single game
      else
         MultiMorphion(M_Limit,MG)              # simulate many games from here
      }
   return
end

procedure Replayer(MG)                    #: feed replayed moves from list/game
static ML,radj,cadj
   if type(MG[1]) == "MorpionGameRecord" then return ML := MG # setup list
   if not ScanGrid(MG) then fail          # out of moves ?
   x := get(ML) | fail                    # get next move
   if x.ref = 0 then x := get(ML) | fail  # skip move 0 if any
   xr := x.row + MG.roff                  # adjust move for grid expansion
   xc := x.col + MG.coff      
   dr := \x.darow + MG.roff               # adjust end for grid expansion
   dc := \x.dacol + MG.coff 
   pool := []
   every m := !MG.pool do {               # find possible moves here
      mr := m.line[m.move,1]  
      mc := m.line[m.move,2]
      if xr=mr & xc=mc then    
         if \dr & \dc then {              # info to disambiguate?
            every p := (m.line)[1|5] do   # try endpoints
               if p[1] = dr & p[2] = dc then 
                  put(pool,m)             # save matching move
            }
         else 
            put(pool,m)                   # save matching move(s)
      } 
   if *pool = 1 then                      # unique move?
      return ( MG.move := pool[1], MG)    # set unique move and return MG  
   else {                                 # we have a problem
      ShowGame(MG)
      fprintf(M_Output,"Problem encountered replaying game at move #%i, %i choices.\n",MG.score,*pool)    
      every m := !pool do 
         fprintf(M_Output,"   %s\n",FormatMoveLogPS(MG,m))        
      &dump := 0
      stop()
      }         
end
```



###  Game Reader 


```Unicon
procedure ReadMoveLog(MG)                     #: read move log wrapper  
   fprintf(M_Output,"Reading recorded game from: %s\n",M_ReplayFile)
   f :=  open(M_ReplayFile ,"r") | 
         stop("Unable to open file ",M_ReplayFile ," for read.")
   R := []
   while b := trim(read(f)) do {
      if b ? ="$end" then break               # allow pre-mature end of file
      b ?:= tab(find("#")|0)                  # strip comments
      b := deletec(b,' \t')                   # strip whitespace
      if *b > 0 then put(R,b)                 # save move for reader
      }
   close(f)
   return M_ReadGame(R)                       # call reader, return move list
end

procedure ReadMoveLogPS(R)                    #: read pentasol style move log
static off
initial {                                     # precalc center offsets
   off := table()
   off[-2] := -4
   off[-1] := -3
   off[0]  := 2
   off[1]  := -1
   off[2]  := 4
   }
   M := []
   n := 0                                     # move number
   get(R) ? ( ="(", coff := integer(tab(many(&digits))) - 4, 
              =",", roff := integer(tab(many(&digits))) - 4, 
              =")", pos(0) )  |               # Reference Cell (c,r)
              stop(&output,"Syntax error in reference line.")              
   while b := get(R) do {                     # Line   (c,r) d o       
      b ? ( ="(", c := integer(tab(many(&digits))), 
            =",", r := integer(tab(many(&digits))), 
            =")", d := tab(any(DALL)),
            o := integer(=("-2"|"-1"|0|"+1"|"+2")) )  |
            stop(&output,"Syntax error in line above.")
      x := MorpionGameRecord()                # new move /  line
      x.ref := n +:= 1
      x.darow := x.row := r - roff
      x.dacol := x.col := c - coff
      case d of {                             # adjust based on direction
         DHOR : x.dacol +:= off[o]
         DVER : x.darow +:= off[o]
         DRD  : ( x.darow +:= off[o],  x.dacol +:= off[o])
         DLD  : ( x.darow -:= off[o],  x.dacol +:= off[o])
         }                                             
      put(M,x)
      }
   return M
end
```


=== Detailed Move Logging (for analysis) ===

```Unicon
procedure PrintDetails(MG)                      #: print the log
   fprintf(M_Output,"Detailed Move Log\n")
   every fprintf(M_Output,"%i : %s\n",i := 1 to *MG.log,MG.log[i])
end

procedure LogFormatMove(M,roff,coff)            #: format a log entry
   /M.roff := \roff | 0  
   /M.coff := \coff | 0 
   log := sprintf("\"%s\" [%i,%i] : ",M.direction,
                  M.line[M.move,1]-M.roff,M.line[M.move,2]-M.coff)  
   every x := !M.line do                           
      log ||:= sprintf("[%i,%i] ",x[1]-M.roff,x[2]-M.coff)    
   return log
end

procedure LogDetails(MG,M)                       #: Record details
   log := LogFormatMove(M)            
   log ||:= sprintf(" - of %i choices.",*MG.pool)  # append # choices  
   log ||:= sprintf(" Metric=%i",M_Eval(MG))       # append score (opt)
   put(MG.log,log)                                 # log the move
end
```



###  Strategy Support 


```Unicon
# No useful examples at this time

procedure Scorefail(MG);end                     #: dummy M_Eval always fails
$endif
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting] 
[http://www.cs.arizona.edu/icon/library/src/procs/strings.icn strings.icn provides deletec] 
[http://www.cs.arizona.edu/icon/library/src/procs/options.icn options.icn provides options processing] 
<br/><br/>'''Other RosettaCode pages used'''  [[Deepcopy]]

== SampleOutput ==


###  Help 


```txt
Morphion [options]  Plays the 5T/D variant of Morphion Solitaire
Arguments :  -helpMorphion [options]  Plays the 5T/D variant of Morphion Solitaire
Where options are:
	-A	choose player strategy approach (default=random, future)
	-E	Specifiy an position fitness evaluation function (default none, future)
	-SW	Search width (strategy dependent, future)
	-SD	Search depth (strategy dependent, future)
	-V|-variant	5D or 5T (default)
	-R|-replay	replay
	-RF	file containing game record to be replayed
	-RN	number of moves to replay (0=all)
	-RR	game recording format to read (ps=pentasol(default), 0=original)
	-RW	game recording format to write ps=pentasol(default), 0=original)
	-save	save the best (-B) and worst (-W) games
	-seed	start the seed of the random number at this value
	-L|-limit	games to play (if 0 or less then play until any of 'XxQq' is pressed
		note for larger n this benefits from larger BLKSIZE, STRSIZE environment variables
	-HW|-histwidth	width (cols) of histogram of scores
	-HG|-histgroup	size of groups (buckets) for histogram
	-UN	Give status update notifications every n simulations
	-B	Keep best n games of unique length (default 5)
	-W	Keep worst n games of unique length (default 3)
	-details	Log game details for analysis
	-Q	(debugging) terminates after options processing
	-?|-h|-help	this help text
```




###  Random Game Record 

The following image was obtained by replaying the game in Pentasol and saving the picture. The original was generated by a multigame simulation and represents the best game generated by this game from the starting position.

[[File:Morpion 5T92 unicon.PNG]]


This the game record used to produce the above image:

```txt
#
# Game Record for Morphion 5T game of 92 moves
# Date: 2012/02/18
# Saved: Games/5T/92-L1_20120218-163208-Fps.txt
# &random: 1617565851
# ReplayFile: * none * (* all * moves)
#
# Pentasol compatible format
#
#
# Morpion Solitaire game
#
#     XXXX
#     X  X
#     X  X
#  XXXR  XXXX
#  X        X
#  X        X
#  XXXX  XXXX
#     X  X
#     X  X
#     XXXX
#
# R = reference point
# List of moves starts with reference point (col,row)
# Lines are
#    (col,row) <direction> <+/-centerdist>
#    distance to center is left side or top edge
#
(9,7)
(12,8) | -2
(16,7) - -2
(9,8) | -2
(13,11) / 0
(9,9) | +2
(15,11) | -2
(13,13) - -2
(6,11) | -2
(16,10) - -2
(12,14) | -2
(8,4) - +2
(5,7) - +2
(13,6) \ 0
(10,12) \ 0
(14,8) \ 0
(14,12) / 0
(10,10) - -2
(11,9) / 0
(8,6) / 0
(10,8) \ +1
(8,11) \ 0
(11,7) / -1
(13,9) \ 0
(11,11) \ 0
(14,11) - -1
(14,9) | +1
(13,12) | -1
(12,9) - +1
(16,9) / -2
(17,6) / -2
(11,12) - +1
(16,6) / -2
(16,8) | 0
(11,10) | +1
(10,9) \ +1
(11,8) / 0
(13,8) - -2
(15,6) / -2
(13,5) | +2
(10,7) \ +2
(14,6) \ 0
(10,6) | +2
(16,11) \ -2
(11,6) - +2
(11,5) | +2
(8,8) / +2
(8,14) / +2
(8,9) | -1
(7,9) - +2
(4,6) \ +2
(5,12) / +2
(13,4) / -2
(10,5) - +1
(10,11) \ 0
(7,8) / +2
(7,6) | +2
(7,11) - +1
(10,14) | -2
(9,14) / +2
(7,3) \ +2
(17,8) - -2
(14,5) \ +1
(11,14) - -1
(8,12) \ 0
(5,8) - +2
(8,13) | -1
(14,4) | +2
(8,5) / -1
(7,14) / +2
(8,3) \ +2
(6,6) - +2
(5,5) \ +2
(8,2) | +2
(6,4) / 0
(9,3) \ +1
(7,5) / 0
(5,3) \ +2
(6,3) - +1
(6,5) - +1
(6,2) | +2
(7,4) \ +1
(7,2) | +2
(5,4) \ +2
(5,6) | 0
(9,2) / -2
(10,2) - -2
(4,5) \ +2
(10,3) | +1
(3,6) / +2
(4,4) - +2
(2,6) - +2
(3,5) / +1
#
```



###  Multigame Run 


The multigame simulation of completely random play includes a histogram showing game scores clustering in the 20's and 60's.  This result is similar to results obtained by [http://www.morpionsolitaire.com/English/RecordsGrids5T.htm Jean-Jacques Sibil] (you will have to scroll down that page to find the reference) who has run nearly a billion such simulations achieving a high score of 102 as of 2010.

The following is a summary file:

```txt
# --- Morpion Solitaire 5 (v1.7e) ---
#
# Command line options :
# Summary of Morpion Configuration:
#   Variant (5T/D) move validation      = procedure ValidMove5T
#   Games to play                       = * unlimited *
#   Multi-game options:
#   - Status Updates                    = 500
#   - Keep best                         = 5
#   - Keep worst                        = 0
#   - Histogram width                   = 80
#   - Histogram grouping                = 5
#   Games will be saved                 = Yes
#   - Format for game file (write)      = procedure WriteMoveLogPS
#   Replaying
#   - Format for game file (read)       = procedure ReadMoveLogPS
#   - Game file to be replayed          = * None *
#   Player Strategy                     = procedure RandomPlayer
#   - Position Fitness Evaluator        = * None *
#   - Search Width (strategy dependant) = 5
#   - Search Depth (strategy dependant) = 5
#   Log Details for analysis            = No
#
Longest game 77 after 1 simulations &random=20122297.
Longest game 77 after 85 simulations &random=274082001.
Longest game 78 after 118 simulations &random=559240181.
Longest game 78 after 123 simulations &random=1682993637.
Longest game 81 after 292 simulations &random=826134037.
Longest game 84 after 1181 simulations &random=1936506737.
Longest game 86 after 4584 simulations &random=1266457499.
Longest game 86 after 44424 simulations &random=1725594333.
Longest game 86 after 47918 simulations &random=1686351259.
Longest game 86 after 50600 simulations &random=665807725.
Longest game 87 after 60841 simulations &random=152917603.
Longest game 87 after 74778 simulations &random=1037682795.
Longest game 88 after 173368 simulations &random=72059739.
Longest game 88 after 241134 simulations &random=2095899781.

Results from Sample of 242921 games of Morpion 5T/D:
Shortest game was 20 moves.
Average game was 53.65064774144681 moves.
Longest game was 88 moves.
Average time/game is 133.1678282239905 ms.
&random is now 452165683.

Summary of Results every '*' = 940 games
  0 | (     0) 
  5 | (     0) 
 10 | (     0) 
 15 | (     0) 
 20 | ( 38637) *****************************************
 25 | ( 13790) **************
 30 | (  6657) *******
 35 | (  2604) **
 40 | (  1306) *
 45 | (  1088) *
 50 | (  3448) ***
 55 | ( 22481) ***********************
 60 | ( 75207) ********************************************************************************
 65 | ( 61501) *****************************************************************
 70 | ( 13902) **************
 75 | (  1922) **
 80 | (   349) 
 85 | (    29) 

Longest (5) Game(s):
#
# Game Record for Morphion 5T game of 88 moves
# Date: 2012/02/18
# Saved: Games/5T/88-L0(241134)_20120218-083009-Fps.txt
# &random: 2095899781
# ReplayFile: * none * (* all * moves)
#

Morphion Solitare Grid (move=88):
   1234567890123456
 1 ................
 2 ....+.+.........
 3 .....+++........
 4 ..+++++++.......
 5 .+++++++++......
 6 ..++++****+.....
 7 .+++++*++*+.....
 8 ..++++*++*++....
 9 ...****++****+..
10 ...*++++++++*+..
11 .++*++++++++*++.
12 ...****++****+..
13 ....++*++*++++..
14 .....+*++*+++...
15 .....+****+.....
16 .......++.......
17 ........+.......
18 ................
   1234567890123456
#
# Game Record for Morphion 5T game of 88 moves
# Date: 2012/02/18
# Saved: Games/5T/88-L0(173368)_20120218-083009-Fps.txt
# &random: 72059739
# ReplayFile: * none * (* all * moves)
#

Morphion Solitare Grid (move=88):
   1234567890123456
 1 ................
 2 .......+........
 3 ......++...+....
 4 .....+****+.....
 5 ..++++*++*+.....
 6 ..++++*++*+++...
 7 ...****++****+..
 8 ...*++++++++*+..
 9 .++*++++++++*+..
10 ...****++****+..
11 ..++++*++*+++++.
12 .+...+*++*+++++.
13 .....+****++++..
14 ....+..+++++++..
15 .........+++++..
16 ..........+.....
17 ...........+....
18 ................
   1234567890123456
#
# Game Record for Morphion 5T game of 87 moves
# Date: 2012/02/18
# Saved: Games/5T/87-L0(74778)_20120218-083009-Fps.txt
# &random: 1037682795
# ReplayFile: * none * (* all * moves)
#

Morphion Solitare Grid (move=87):
   123456789012345
 1 ...............
 2 ....+.+........
 3 .....+.........
 4 ....+++++......
 5 ...++++++......
 6 .+++++****+....
 7 ..++++*++*+....
 8 .+++++*++*++...
 9 ..+****++****..
10 ..+*++++++++*+.
11 ...*++++++++*..
12 ..+****++****+.
13 ...+++*++*++++.
14 ..++++*++*++...
15 .....+****+.+..
16 ......+++++....
17 ........+......
18 ...............
   123456789012345
#
# Game Record for Morphion 5T game of 87 moves
# Date: 2012/02/18
# Saved: Games/5T/87-L0(60841)_20120218-083009-Fps.txt
# &random: 152917603
# ReplayFile: * none * (* all * moves)
#

Morphion Solitare Grid (move=87):
   123456789012345678
 1 ..................
 2 .........+.+..+...
 3 .....+...++++++...
 4 ......****+++++...
 5 .....+*++*+++++...
 6 ....++*++*+++++++.
 7 ..+****++****+.+..
 8 .++*++++++++*+....
 9 .++*++++++++*+....
10 ..+****++****+....
11 .+++++*++*++++....
12 ....++*++*++......
13 ....++****........
14 ....+++...........
15 ..................
   123456789012345678
#
# Game Record for Morphion 5T game of 86 moves
# Date: 2012/02/18
# Saved: Games/5T/86-L0(50600)_20120218-083009-Fps.txt
# &random: 665807725
# ReplayFile: * none * (* all * moves)
#

Morphion Solitare Grid (move=86):
   1234567890123456
 1 ................
 2 .....++..+++....
 3 .....+****+.....
 4 ....+.*++*+.....
 5 ...+.+*++*+..+..
 6 ..+****++****...
 7 ..+*++++++++*...
 8 ..+*++++++++*...
 9 ..+****++****++.
10 .+++++*++*+++++.
11 ....++*++*+++++.
12 .....+****+++++.
13 .....++++++++++.
14 ..........+++++.
15 ..........+.....
16 ................
   1234567890123456

Shortest (0) Game(s):

Total run time = 32349320 ms
&allocated   (Total,static,string,block) :  1152124804 0 144396168 1007728636
&collections (Total,static,string,block) :  4623 0 0 4623
&regions     ( -   , -    ,string,block) :  - 0 41859440 41859440
&storage     ( -   , -    ,string,block) :  - 0 37784 13290772
```


== Other Interesting Results ==
One of things I did to get a feel for the game and perhaps get some insight in to strategies was to replay published record games.  These universally look smoother and more organized than even the best random games.  Somewhere it occurred to me to truncate these record games and play forward randomly to see what happened.  I probably expected them to go off the rails fairly quickly.  And while that happens, I was surprised how close these could get to the original record.  This lead to my earlier observation that the seeds of success are set very early in morpion. It may also be due to the number of possible moves falling off more quickly than I might expect.
* Using Bruneau's 170 game truncated at 112 moves the program produced the following:

```txt
Longest game 168 after 175790 simulations &random=1821730183.
Longest game 169 after 279675 simulations &random=873864083.
Longest game 170 after 1073380 simulations &random=2014543635.
Longest game 170 after 1086106 simulations &random=1319746023.


Results from Sample of 1091265 games of Morpion 5T/D:
Shortest game was 122 moves.
Average game was 127.0640165312733 moves.
Longest game was 170 moves.
Average time/game is 77.48127814967033 ms.
&random is now 609048351.

Summary of Results every '*' = 6761 games
...
115 | (     0) 
120 | (324190) ***********************************************
125 | (540953) ********************************************************************************
130 | (193255) ****************************
135 | ( 17723) **
140 | ( 13447) *
145 | (  1577) 
150 | (    83) 
155 | (     0) 
160 | (    28) 
165 | (     7) 
170 | (     2) 
```

: The two games of 170 are both different and not just transpositions.  However the difference produced is in a single move.
* Using Rosin's 177 move (A) grid, the program produced the following,
:* from move 129:

```txt
Longest game 145 after 1 simulations.
Longest game 153 after 2 simulations.
Longest game 154 after 20 simulations.
Longest game 155 after 40 simulations.
Longest game 164 after 50 simulations.
Longest game 168 after 2203 simulations.

Results from Sample of 78393 games of Morpion 5T:
Shortest game was 143 moves.
Average game was 147.2826145191535 moves.
Longest game was 168 moves.
Average time/game is 115.6193155001084 ms.

Summary of Results every '*' = 973 games
... 
120 | (     0) 
140 | ( 77901) ********************************************************************************
160 | (   492)
```

:* from move 112:

```txt
Longest game 140 after 1 simulations.
Longest game 146 after 10 simulations.
Longest game 148 after 441 simulations.
Longest game 151 after 2029 simulations.
Longest game 153 after 7167 simulations.
Longest game 157 after 34601 simulations.
Longest game 168 after 41977 simulations.

Results from Sample of 524157 games of Morpion 5T:
Shortest game was 126 moves.
Average game was 136.6568528131838 moves.
Longest game was 168 moves.
Average time/game is 138.334270075569 ms.

Summary of Results every '*' = 5643 games
...
100 | (     0) 
120 | (451482) ********************************************************************************
140 | ( 72673) ************
160 | (     2)
```

: Unfortunately that earlier version of the program was not logging the random number seed used for these games nor was it recording games in a notation that I have a converter for (at this time).  
The above were run under "Unicon Version 12.0.  July 13, 2011" on Windows 7/x64.
