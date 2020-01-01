+++
title = "Stable marriage problem"
description = ""
date = 2018-05-27T19:15:28Z
aliases = []
[extra]
id = 7892
[taxonomies]
categories = []
tags = []
+++

{{task|Classic CS problems and programs}}
Solve the [[wp:Stable marriage problem|Stable marriage problem]] using the Gale/Shapley algorithm.


'''Problem description'''

Given an equal number of men and women to be paired for marriage, each man ranks all the women in order of his preference and each woman ranks all the men in order of her preference.

A stable set of engagements for marriage is one where no man prefers a woman over the one he is engaged to, where that other woman ''also'' prefers that man over the one she is engaged to. I.e. with consulting marriages, there would be no reason for the engagements between the people to change.

Gale and Shapley proved that there is a stable set of engagements for any set of preferences and the first link above gives their algorithm for finding a set of stable engagements.


'''Task Specifics'''

Given ten males:
    abe, bob, col, dan, ed, fred, gav, hal, ian, jon
And ten females:
    abi, bea, cath, dee, eve, fay, gay, hope, ivy, jan

And a complete list of ranked preferences, where the most liked is to the left:
   abe: abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay
   bob: cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay
   col: hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan
   dan: ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi
    ed: jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay
  fred: bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay
   gav: gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay
   hal: abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee
   ian: hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve
   jon: abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope

   abi: bob, fred, jon, gav, ian, abe, dan, ed, col, hal
   bea: bob, abe, col, fred, gav, dan, ian, ed, jon, hal
  cath: fred, bob, ed, gav, hal, col, ian, abe, dan, jon
   dee: fred, jon, col, abe, ian, hal, gav, dan, bob, ed
   eve: jon, hal, fred, dan, abe, gav, col, ed, ian, bob
   fay: bob, abe, ed, ian, jon, dan, fred, gav, col, hal
   gay: jon, gav, hal, fred, bob, abe, col, ed, dan, ian
  hope: gav, jon, bob, abe, ian, dan, hal, ed, col, fred
   ivy: ian, col, hal, gav, fred, bob, abe, ed, jon, dan
   jan: ed, hal, gav, abe, bob, jon, col, ian, fred, dan

# Use the Gale Shapley algorithm to find a stable set of engagements
# Perturb this set of engagements to form an unstable set of engagements then check this new set for stability.


'''References'''
#  [http://www.cs.columbia.edu/~evs/intro/stable/writeup.html The Stable Marriage Problem]. (Eloquent description and background information).
# [http://sephlietz.com/gale-shapley/ Gale-Shapley Algorithm Demonstration].
# [http://mathsite.math.berkeley.edu/smp/smp.html Another Gale-Shapley Algorithm Demonstration].
# [https://www.youtube.com/watch?v=Qcv1IqHWAzg Stable Marriage Problem - Numberphile] (Video).
# [https://www.youtube.com/watch?v=LtTV6rIxhdo Stable Marriage Problem (the math bit)] (Video).
# [http://www.ams.org/samplings/feature-column/fc-2015-03 The Stable Marriage Problem and School Choice]. (Excellent exposition)





## AutoHotkey

{{works with|AutoHotkey L}}

```autohotkey
; Given a complete list of ranked preferences, where the most liked is to the left:
abe := ["abi", "eve", "cath", "ivy", "jan", "dee", "fay", "bea", "hope", "gay"]
bob := ["cath", "hope", "abi", "dee", "eve", "fay", "bea", "jan", "ivy", "gay"]
col := ["hope", "eve", "abi", "dee", "bea", "fay", "ivy", "gay", "cath", "jan"]
dan := ["ivy", "fay", "dee", "gay", "hope", "eve", "jan", "bea", "cath", "abi"]
ed := ["jan", "dee", "bea", "cath", "fay", "eve", "abi", "ivy", "hope", "gay"]
fred := ["bea", "abi", "dee", "gay", "eve", "ivy", "cath", "jan", "hope", "fay"]
gav := ["gay", "eve", "ivy", "bea", "cath", "abi", "dee", "hope", "jan", "fay"]
hal := ["abi", "eve", "hope", "fay", "ivy", "cath", "jan", "bea", "gay", "dee"]
ian := ["hope", "cath", "dee", "gay", "bea", "abi", "fay", "ivy", "jan", "eve"]
jon := ["abi", "fay", "jan", "gay", "eve", "bea", "dee", "cath", "ivy", "hope"]
abi := ["bob", "fred", "jon", "gav", "ian", "abe", "dan", "ed", "col", "hal"]
bea := ["bob", "abe", "col", "fred", "gav", "dan", "ian", "ed", "jon", "hal"]
cath := ["fred", "bob", "ed", "gav", "hal", "col", "ian", "abe", "dan", "jon"]
dee := ["fred", "jon", "col", "abe", "ian", "hal", "gav", "dan", "bob", "ed"]
eve := ["jon", "hal", "fred", "dan", "abe", "gav", "col", "ed", "ian", "bob"]
fay := ["bob", "abe", "ed", "ian", "jon", "dan", "fred", "gav", "col", "hal"]
gay := ["jon", "gav", "hal", "fred", "bob", "abe", "col", "ed", "dan", "ian"]
hope := ["gav", "jon", "bob", "abe", "ian", "dan", "hal", "ed", "col", "fred"]
ivy := ["ian", "col", "hal", "gav", "fred", "bob", "abe", "ed", "jon", "dan"]
jan := ["ed", "hal", "gav", "abe", "bob", "jon", "col", "ian", "fred", "dan"]

; of ten males:
males := ["abe", "bob", "col", "dan", "ed", "fred", "gav", "hal", "ian", "jon"]

; and ten females:
females := ["abi", "bea", "cath", "dee", "eve", "fay", "gay", "hope", "ivy", "jan"]

; and an empty set of engagements:
engagements := Object()
freemales := males.Clone()
,s := "Engagements:`n"

; use the Gale Shapley algorithm to find a stable set of engagements:
For i, male in freemales ; i=index of male (not needed)
{
	j:=1 ; index of female
	While (engagements[female:=%male%[j]] != "" and index(%female%, male) > index(%female%, engagements[female]))
		j++ ; each male loops through all females in order of his preference until one accepts him
	If (engagements[female] != "") ; if she was previously engaged
		freemales.insert(engagements[female]) ; her old male goes to the bottom of the list
		,s .= female . " dumped " . engagements[female] . "`n"
	engagements[female] := male ; the new engagement is registered
	,s .= female . " accepted " . male . "`n"
}

; summarize results:
s .= "`nCouples:`n"
For female, male in engagements
	s .= female . " is engaged to " . male . "`n"
s .= Stable(engagements, females)

; then perturb this set of engagements to form an unstable set of engagements then check this new set for stability:
s .= "`nWhat if cath and ivy swap?`n"
engagements["cath"]:="abe", engagements["ivy"]:="bob"

; summarize results:
s .= "`nCouples:`n"
For female, male in engagements
	s .= female . " is engaged to " . male . "`n"
s .= Stable(engagements, females)

Msgbox % clipboard := s
Return

; Functions:
Index(obj, value) {
	For key, val in obj
		If (val = value)
			Return, key, ErrorLevel := 0
	Return, False, Errorlevel := 1
}

Stable(engagements, females) {
	For female, male in engagements
	{
		For j, female2 in females ; j=index of female (not needed)
		{
			If (index(%male%, female) > index(%male%, female2)
				and index(%female2%, male2:=engagements[female2]) > index(%female2%, male))
				s .= male . " is engaged to " . female . " but would prefer " . female2
					. " and " . female2 . " is engaged to " . male2 . " but would prefer " . male . "`n"
		}
	}
	If s
		Return "`nThese couples are not stable.`n" . s
	Else
		Return "`nThese couples are stable.`n"
}
```

{{out}}

```txt
Engagements:
abi accepted abe
cath accepted bob
hope accepted col
ivy accepted dan
jan accepted ed
bea accepted fred
gay accepted gav
eve accepted hal
hope dumped col
hope accepted ian
abi dumped abe
abi accepted jon
dee accepted col
ivy dumped dan
ivy accepted abe
fay accepted dan

Couples:
abi is engaged to jon
bea is engaged to fred
cath is engaged to bob
dee is engaged to col
eve is engaged to hal
fay is engaged to dan
gay is engaged to gav
hope is engaged to ian
ivy is engaged to abe
jan is engaged to ed

These couples are stable.

What if cath and ivy swap?

Couples:
abi is engaged to jon
bea is engaged to fred
cath is engaged to abe
dee is engaged to col
eve is engaged to hal
fay is engaged to dan
gay is engaged to gav
hope is engaged to ian
ivy is engaged to bob
jan is engaged to ed

These couples are not stable.
bob is engaged to ivy but would prefer abi and abi is engaged to jon but would prefer bob
bob is engaged to ivy but would prefer bea and bea is engaged to fred but would prefer bob
bob is engaged to ivy but would prefer cath and cath is engaged to abe but would prefer bob
bob is engaged to ivy but would prefer fay and fay is engaged to dan but would prefer bob
bob is engaged to ivy but would prefer hope and hope is engaged to ian but would prefer bob
```



## Batch File


```dos
:: Stable Marriage Problem in Rosetta Code
:: Batch File Implementation

@echo off
setlocal enabledelayedexpansion

:: Initialization (Index Starts in 0)
set "male=abe bob col dan ed fred gav hal ian jon"
set "femm=abi bea cath dee eve fay gay hope ivy jan"

set "abe[]=abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay"
set "bob[]=cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay"
set "col[]=hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan"
set "dan[]=ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi"
set "ed[]=jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay"
set "fred[]=bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay"
set "gav[]=gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay"
set "hal[]=abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee"
set "ian[]=hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve"
set "jon[]=abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope"

set "abi[]=bob, fred, jon, gav, ian, abe, dan, ed, col, hal"
set "bea[]=bob, abe, col, fred, gav, dan, ian, ed, jon, hal"
set "cath[]=fred, bob, ed, gav, hal, col, ian, abe, dan, jon"
set "dee[]=fred, jon, col, abe, ian, hal, gav, dan, bob, ed"
set "eve[]=jon, hal, fred, dan, abe, gav, col, ed, ian, bob"
set "fay[]=bob, abe, ed, ian, jon, dan, fred, gav, col, hal"
set "gay[]=jon, gav, hal, fred, bob, abe, col, ed, dan, ian"
set "hope[]=gav, jon, bob, abe, ian, dan, hal, ed, col, fred"
set "ivy[]=ian, col, hal, gav, fred, bob, abe, ed, jon, dan"
set "jan[]=ed, hal, gav, abe, bob, jon, col, ian, fred, dan"

rem variable notation:
rem    <boy>{<index>} = <girl>
rem    <boy>[<girl>] = <index>
for %%M in (%male%) do (
   set cnt=0
   for %%. in (!%%M[]!) do (
      set "%%M{!cnt!}=%%."
      set "%%M[%%.]=!cnt!"
      set /a cnt+=1
   )
)
for %%F in (%femm%) do (
   set cnt=0
   for %%. in (!%%F[]!) do (
      set "%%F[%%.]=!cnt!"
      set /a cnt+=1
   )
)

:: The Main Thing
echo(HISTORY:
call :stableMatching
echo(
echo(NEWLYWEDS:
call :display
echo(
call :isStable
echo(
echo(What if ed and hal swapped?
call :swapper ed hal
echo(
echo(NEW-NEWLYWEDS:
call :display
echo(
call :isStable
pause>nul
exit /b 0

:: The Algorithm
:stableMatching
set "free_men=%male%"
set "free_fem=%femm%"
for %%M in (%male%) do set "%%M_tried=0"

:match_loop
if "%free_men%"=="" goto :EOF
for /f "tokens=1* delims= " %%m in ("%free_men%") do (
   rem get woman not yet proposed to, but if man's tries exceeds the number
   rem of women (poor guy), he starts again to his most preferred woman (#0).
   for /f %%x in ("!%%m_tried!") do if not defined %%m{%%x} (
      set "%%m_tried=0" & set "w=!%%m{0}!"
   ) else set "w=!%%m{%%x}!"
   set "m=%%m"

   for /f %%x in ("free_fem:!w!=") do (
      if not "!free_fem!"=="!%%x!" (
         rem accept because !w! (the woman) is free
         set "!m!_=!w!" & set "!w!_=!m!"
         set "free_men=%%n" & set "free_fem=!%%x!"
         echo(    !w! ACCEPTED !m!.
      ) else (
         rem here, !w! already has a pair; get his name and rank.
         for /f %%. in ("!w!") do set "cur_man=!%%._!"
         for /f %%. in ("!w![!cur_man!]") do set "rank_cur=!%%.!"
         rem also, get the rank of current proposing man.
         for /f %%. in ("!w![!m!]") do set "rank_new=!%%.!"

         if !rank_new! lss !rank_cur! (
            rem here, !w! will leave her pair, and choose !m!.
            set "free_men=%%n !cur_man!"
            echo(    !w! LEFT !cur_man!.
            rem pair them up now!
            set "!m!_=!w!" & set "!w!_=!m!"
            echo(    !w! ACCEPTED !m!.
         )
      )
   )
   set /a "!m!_tried+=1"
)
goto :match_loop

:: Output the Couples
:display
for %%S in (%femm%) do echo.    %%S and !%%S_!.
goto :EOF

:: Stability Checking
:isStable
for %%f in (%femm%) do (
   for %%g in (%male%) do (
      for /f %%. in ("%%f[!%%f_!]") do set "girl_cur=!%%.!"
      set "girl_aboy=!%%f[%%g]!"
      for /f %%. in ("%%g[!%%g_!]") do set "boy_cur=!%%.!"
      set "boy_agirl=!%%g[%%f]!"

      if !boy_cur! gtr !boy_agirl! (
         if !girl_cur! gtr !girl_aboy! (
            echo(STABILITY = FALSE.
            echo(%%f and %%g would rather be together than their current partners.
            goto :EOF
         )
      )
   )
)
echo(STABILITY = TRUE.
goto :EOF

:: Swapper
:swapper
set %~1.tmp=!%~1_!
set %~2.tmp=!%~2_!
set "%~1_=!%~2.tmp!"
set "%~2_=!%~1.tmp!"
set "!%~1.tmp!_=%~2"
set "!%~2.tmp!_=%~1"
goto :EOF
```

{{Out}}

```txt
HISTORY:
    abi ACCEPTED abe.
    cath ACCEPTED bob.
    hope ACCEPTED col.
    ivy ACCEPTED dan.
    jan ACCEPTED ed.
    bea ACCEPTED fred.
    gay ACCEPTED gav.
    eve ACCEPTED hal.
    hope LEFT col.
    hope ACCEPTED ian.
    abi LEFT abe.
    abi ACCEPTED jon.
    dee ACCEPTED col.
    ivy LEFT dan.
    ivy ACCEPTED abe.
    fay ACCEPTED dan.

NEWLYWEDS:
    abi and jon.
    bea and fred.
    cath and bob.
    dee and col.
    eve and hal.
    fay and dan.
    gay and gav.
    hope and ian.
    ivy and abe.
    jan and ed.

STABILITY = TRUE.

What if ed and hal swapped?

NEW-NEWLYWEDS:
    abi and jon.
    bea and fred.
    cath and bob.
    dee and col.
    eve and ed.
    fay and dan.
    gay and gav.
    hope and ian.
    ivy and abe.
    jan and hal.

STABILITY = FALSE.
eve and abe would rather be together than their current partners.
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      N = 10
      DIM mname$(N), wname$(N), mpref$(N), wpref$(N), mpartner%(N), wpartner%(N)
      DIM proposed&(N,N)
      mname$() = "", "Abe","Bob","Col","Dan","Ed","Fred","Gav","Hal","Ian","Jon"
      wname$() = "", "Abi","Bea","Cath","Dee","Eve","Fay","Gay","Hope","Ivy","Jan"
      mpref$() = "", "AECIJDFBHG","CHADEFBJIG","HEADBFIGCJ","IFDGHEJBCA","JDBCFEAIHG",\
      \              "BADGEICJHF","GEIBCADHJF","AEHFICJBGD","HCDGBAFIJE","AFJGEBDCIH"
      wpref$() = "", "BFJGIADECH","BACFGDIEJH","FBEGHCIADJ","FJCAIHGDBE","JHFDAGCEIB",\
      \              "BAEIJDFGCH","JGHFBACEDI","GJBAIDHECF","ICHGFBAEJD","EHGABJCIFD"

      REM The Gale-Shapley algorithm:
      REPEAT
        FOR m% = 1 TO N
          REPEAT
            IF mpartner%(m%) EXIT REPEAT
            FOR i% = 1 TO N
              w% = ASCMID$(mpref$(m%),i%) - 64
              IF proposed&(m%,w%) = 0 EXIT FOR
            NEXT i%
            IF i% > N EXIT REPEAT
            proposed&(m%,w%) = 1
            IF wpartner%(w%) = 0 THEN
              mpartner%(m%) = w% : REM Get engaged
              wpartner%(w%) = m%
            ELSE
              o% = wpartner%(w%)
              IF INSTR(wpref$(w%), LEFT$(mname$(m%),1)) < \
              \  INSTR(wpref$(w%), LEFT$(mname$(o%),1)) THEN
                mpartner%(o%) = 0  : REM Split up
                mpartner%(m%) = w% : REM Get engaged
                wpartner%(w%) = m%
              ENDIF
            ENDIF
          UNTIL TRUE
        NEXT m%
      UNTIL SUM(mpartner%()) = (N*(N+1))/2

      FOR m% = 1 TO N
        PRINT mname$(m%) " is engaged to " wname$(mpartner%(m%))
      NEXT
      PRINT "Relationships are ";
      IF FNstable PRINT "stable." ELSE PRINT "unstable."

      a% = RND(N)
      REPEAT b% = RND(N) : UNTIL b%<>a%
      PRINT '"Now swapping " mname$(a%) "'s and " mname$(b%) "'s partners:"
      SWAP mpartner%(a%), mpartner%(b%)
      PRINT mname$(a%) " is engaged to " wname$(mpartner%(a%))
      PRINT mname$(b%) " is engaged to " wname$(mpartner%(b%))
      PRINT "Relationships are ";
      IF FNstable PRINT "stable." ELSE PRINT "unstable."
      END

      DEF FNstable
      LOCAL m%, w%, o%, p%
      FOR m% = 1 TO N
        w% = mpartner%(m%)
        FOR o% = 1 TO N
          p% = wpartner%(o%)
          IF INSTR(mpref$(m%), LEFT$(wname$(o%),1)) < \
          \  INSTR(mpref$(m%), LEFT$(wname$(w%),1)) AND \
          \  INSTR(wpref$(o%), LEFT$(mname$(m%),1)) < \
          \  INSTR(wpref$(o%), LEFT$(mname$(p%),1)) THEN
            = FALSE
          ENDIF
        NEXT o%
      NEXT m%
      = TRUE
```

'''Output:'''

```txt

Abe is engaged to Ivy
Bob is engaged to Cath
Col is engaged to Dee
Dan is engaged to Fay
Ed is engaged to Jan
Fred is engaged to Bea
Gav is engaged to Gay
Hal is engaged to Eve
Ian is engaged to Hope
Jon is engaged to Abi
Relationships are stable.

Now swapping Hal's and Ian's partners:
Hal is engaged to Hope
Ian is engaged to Eve
Relationships are unstable.

```



## Bracmat


```bracmat
(     (abe.abi eve cath ivy jan dee fay bea hope gay)
      (bob.cath hope abi dee eve fay bea jan ivy gay)
      (col.hope eve abi dee bea fay ivy gay cath jan)
      (dan.ivy fay dee gay hope eve jan bea cath abi)
      (ed.jan dee bea cath fay eve abi ivy hope gay)
      (fred.bea abi dee gay eve ivy cath jan hope fay)
      (gav.gay eve ivy bea cath abi dee hope jan fay)
      (hal.abi eve hope fay ivy cath jan bea gay dee)
      (ian.hope cath dee gay bea abi fay ivy jan eve)
      (jon.abi fay jan gay eve bea dee cath ivy hope)
  : ?Mplan
  : ?M
&     (abi.bob fred jon gav ian abe dan ed col hal)
      (bea.bob abe col fred gav dan ian ed jon hal)
      (cath.fred bob ed gav hal col ian abe dan jon)
      (dee.fred jon col abe ian hal gav dan bob ed)
      (eve.jon hal fred dan abe gav col ed ian bob)
      (fay.bob abe ed ian jon dan fred gav col hal)
      (gay.jon gav hal fred bob abe col ed dan ian)
      (hope.gav jon bob abe ian dan hal ed col fred)
      (ivy.ian col hal gav fred bob abe ed jon dan)
      (jan.ed hal gav abe bob jon col ian fred dan)
  : ?W
& :?engaged
&   whl
  ' (   !Mplan
      :   ?A
          (?m&~(!engaged:? (!m.?) ?).%?w ?ws)
          ( ?Z
          & (   (   ~(!engaged:?a (?m`.!w) ?z)
                  & (!m.!w) !engaged
                |   !W:? (!w.? !m ? !m` ?) ?
                  & !a (!m.!w) !z
                )
              : ?engaged
            |
            )
          & !Z !A (!m.!ws):?Mplan
          )
    )
& ( unstable
  =   m1 m2 w1 w2
    .   !arg
      :   ?
          (?m1.?w1)
          ?
          (?m2.?w2)
          ( ?
          & (   !M:? (!m1.? !w2 ? !w1 ?) ?
              & !W:? (!w2.? !m1 ? !m2 ?) ?
            |   !M:? (!m2.? !w1 ? !w2 ?) ?
              & !W:? (!w1.? !m2 ? !m1 ?) ?
            )
          )
  )
& ( unstable$!engaged&out$unstable
  | out$stable
  )
& out$!engaged
& !engaged:(?m1.?w1) (?m2.?w2) ?others
& out$(swap !w1 for !w2)
& (   unstable$((!m1.!w2) (!m2.!w1) !others)
    & out$unstable
  | out$stable
  )
);
```

{{out}}

```txt
stable
  (dan.fay)
  (col.dee)
  (hal.eve)
  (gav.gay)
  (fred.bea)
  (ed.jan)
  (abe.ivy)
  (ian.hope)
  (bob.cath)
  (jon.abi)
swap fay for dee
unstable
```



## C

Oddly enough (or maybe it should be that way, only that I don't know): if the women were proposing instead of the men, the resulting pairs are exactly the same.

```c
#include <stdio.h>

int verbose = 0;
enum {
	clown = -1,
	abe, bob, col, dan, ed, fred, gav, hal, ian, jon,
	abi, bea, cath, dee, eve, fay, gay, hope, ivy, jan,
};
const char *name[] = {
	"Abe", "Bob", "Col",  "Dan", "Ed",  "Fred", "Gav", "Hal",  "Ian", "Jon",
	"Abi", "Bea", "Cath", "Dee", "Eve", "Fay",  "Gay", "Hope", "Ivy", "Jan"
};
int pref[jan + 1][jon + 1] = {
	{ abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay },
	{ cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay },
	{ hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan },
	{ ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi },
	{ jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay },
	{ bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay },
	{ gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay },
	{ abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee },
	{ hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve },
	{ abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope },

	{ bob, fred, jon, gav, ian, abe, dan, ed, col, hal   },
	{ bob, abe, col, fred, gav, dan, ian, ed, jon, hal   },
	{ fred, bob, ed, gav, hal, col, ian, abe, dan, jon   },
	{ fred, jon, col, abe, ian, hal, gav, dan, bob, ed   },
	{ jon, hal, fred, dan, abe, gav, col, ed, ian, bob   },
	{ bob, abe, ed, ian, jon, dan, fred, gav, col, hal   },
	{ jon, gav, hal, fred, bob, abe, col, ed, dan, ian   },
	{ gav, jon, bob, abe, ian, dan, hal, ed, col, fred   },
	{ ian, col, hal, gav, fred, bob, abe, ed, jon, dan   },
	{ ed, hal, gav, abe, bob, jon, col, ian, fred, dan   },
};
int pairs[jan + 1], proposed[jan + 1];

void engage(int man, int woman)
{
	pairs[man] = woman;
	pairs[woman] = man;
	if (verbose) printf("%4s is engaged to %4s\n", name[man], name[woman]);
}

void dump(int woman, int man)
{
	pairs[man] = pairs[woman] = clown;
	if (verbose) printf("%4s dumps %4s\n", name[woman], name[man]);
}

/* how high this person ranks that: lower is more preferred */
int rank(int this, int that)
{
	int i;
	for (i = abe; i <= jon && pref[this][i] != that; i++);
	return i;
}

void propose(int man, int woman)
{
	int fiance = pairs[woman];
	if (verbose) printf("%4s proposes to %4s\n", name[man], name[woman]);

	if (fiance == clown) {
		engage(man, woman);
	} else if (rank(woman, man) < rank(woman, fiance)) {
		dump(woman, fiance);
		engage(man, woman);
	}
}

int covet(int man1, int wife2)
{
	if (rank(man1, wife2) < rank(man1, pairs[man1]) &&
			rank(wife2, man1) < rank(wife2, pairs[wife2])) {
		printf( "    %4s (w/ %4s) and %4s (w/ %4s) prefer each other"
			" over current pairing.\n",
			name[man1], name[pairs[man1]], name[wife2], name[pairs[wife2]]
		);
		return 1;
	}
	return 0;
}

int thy_neighbors_wife(int man1, int man2)
{	/* +: force checking all pairs; "||" would shortcircuit */
	return covet(man1, pairs[man2]) + covet(man2, pairs[man1]);
}

int unstable()
{
	int i, j, bad = 0;
	for (i = abe; i < jon; i++) {
		for (j = i + 1; j <= jon; j++)
			if (thy_neighbors_wife(i, j)) bad = 1;
	}
	return bad;
}

int main()
{
	int i, unengaged;
	/* init: everyone marries the clown */
	for (i = abe; i <= jan; i++)
		pairs[i] = proposed[i] = clown;

	/* rounds */
	do {
		unengaged = 0;
		for (i = abe; i <= jon; i++) {
		//for (i = abi; i <= jan; i++) { /* could let women propose */
			if (pairs[i] != clown) continue;
			unengaged = 1;
			propose(i, pref[i][++proposed[i]]);
		}
	} while (unengaged);

	printf("Pairing:\n");
	for (i = abe; i <= jon; i++)
		printf("  %4s - %s\n", name[i],
			pairs[i] == clown ? "clown" : name[pairs[i]]);

	printf(unstable()
		? "Marriages not stable\n" /* draw sad face here */
		: "Stable matchup\n");

	printf("\nBut if Bob and Fred were to swap:\n");
	i = pairs[bob];
	engage(bob, pairs[fred]);
	engage(fred, i);
	printf(unstable() ? "Marriages not stable\n" : "Stable matchup\n");

	return 0;
}
```

{{out}}

```txt
Pairing:
   Abe - Ivy
   Bob - Cath
   Col - Dee
   Dan - Fay
    Ed - Jan
  Fred - Bea
   Gav - Gay
   Hal - Eve
   Ian - Hope
   Jon - Abi
Stable matchup

But if Bob and Fred were to swap:
    Fred (w/ Cath) and  Ivy (w/  Abe) prefer each other over current pairing.
     Bob (w/  Bea) and  Fay (w/  Dan) prefer each other over current pairing.
     Bob (w/  Bea) and Hope (w/  Ian) prefer each other over current pairing.
     Bob (w/  Bea) and  Abi (w/  Jon) prefer each other over current pairing.
    Fred (w/ Cath) and  Dee (w/  Col) prefer each other over current pairing.
    Fred (w/ Cath) and  Abi (w/  Jon) prefer each other over current pairing.
Marriages not stable
```



## C++


```cpp
#include <algorithm>
#include <iostream>
#include <map>
#include <queue>
#include <string>
#include <vector>
using namespace std;

const char *men_data[][11] = {
    { "abe",  "abi","eve","cath","ivy","jan","dee","fay","bea","hope","gay" },
    { "bob",  "cath","hope","abi","dee","eve","fay","bea","jan","ivy","gay" },
    { "col",  "hope","eve","abi","dee","bea","fay","ivy","gay","cath","jan" },
    { "dan",  "ivy","fay","dee","gay","hope","eve","jan","bea","cath","abi" },
    { "ed",   "jan","dee","bea","cath","fay","eve","abi","ivy","hope","gay" },
    { "fred", "bea","abi","dee","gay","eve","ivy","cath","jan","hope","fay" },
    { "gav",  "gay","eve","ivy","bea","cath","abi","dee","hope","jan","fay" },
    { "hal",  "abi","eve","hope","fay","ivy","cath","jan","bea","gay","dee" },
    { "ian",  "hope","cath","dee","gay","bea","abi","fay","ivy","jan","eve" },
    { "jon",  "abi","fay","jan","gay","eve","bea","dee","cath","ivy","hope" }
};

const char *women_data[][11] = {
    { "abi",  "bob","fred","jon","gav","ian","abe","dan","ed","col","hal" },
    { "bea",  "bob","abe","col","fred","gav","dan","ian","ed","jon","hal" },
    { "cath", "fred","bob","ed","gav","hal","col","ian","abe","dan","jon" },
    { "dee",  "fred","jon","col","abe","ian","hal","gav","dan","bob","ed" },
    { "eve",  "jon","hal","fred","dan","abe","gav","col","ed","ian","bob" },
    { "fay",  "bob","abe","ed","ian","jon","dan","fred","gav","col","hal" },
    { "gay",  "jon","gav","hal","fred","bob","abe","col","ed","dan","ian" },
    { "hope", "gav","jon","bob","abe","ian","dan","hal","ed","col","fred" },
    { "ivy",  "ian","col","hal","gav","fred","bob","abe","ed","jon","dan" },
    { "jan",  "ed","hal","gav","abe","bob","jon","col","ian","fred","dan" }
};

typedef vector<string> PrefList;
typedef map<string, PrefList> PrefMap;
typedef map<string, string> Couples;

// Does 'first' appear before 'second' in preference list?
bool prefers(const PrefList &prefer, const string &first, const string &second)
{
    for (PrefList::const_iterator it = prefer.begin(); it != prefer.end(); ++it)
    {
        if (*it == first) return true;
        if (*it == second) return false;
    }
    return false; // no preference
}

void check_stability(const Couples &engaged, const PrefMap &men_pref, const PrefMap &women_pref)
{
    cout << "Stablility:\n";
    bool stable = true;
    for (Couples::const_iterator it = engaged.begin(); it != engaged.end(); ++it)
    {
        const string &bride = it->first;
        const string &groom = it->second;
        const PrefList &preflist = men_pref.at(groom);

        for (PrefList::const_iterator it = preflist.begin(); it != preflist.end(); ++it)
        {
            if (*it == bride) // he prefers his bride
                break;
            if (prefers(preflist, *it, bride) && // he prefers another woman
                prefers(women_pref.at(*it), groom, engaged.at(*it))) // other woman prefers him
            {
                cout << "\t" << *it <<
                    " prefers " << groom <<
                    " over " << engaged.at(*it) <<
                    " and " << groom <<
                    " prefers " << *it <<
                    " over " << bride << "\n";
                stable = false;
            }
        }
    }
    if (stable) cout << "\t(all marriages stable)\n";
}

int main()
{
    PrefMap men_pref, women_pref;
    queue<string> bachelors;

    // init data structures
    for (int i = 0; i < 10; ++i) // person
    {
        for (int j = 1; j < 11; ++j) // preference
        {
              men_pref[  men_data[i][0]].push_back(  men_data[i][j]);
            women_pref[women_data[i][0]].push_back(women_data[i][j]);
        }
        bachelors.push(men_data[i][0]);
    }

    Couples engaged; // <woman,man>

    cout << "Matchmaking:\n";
    while (!bachelors.empty())
    {
        const string &suitor = bachelors.front();
        const PrefList &preflist = men_pref[suitor];

        for (PrefList::const_iterator it = preflist.begin(); it != preflist.end(); ++it)
        {
            const string &bride = *it;

            if (engaged.find(bride) == engaged.end()) // she's available
            {
                cout << "\t" << bride << " and " << suitor << "\n";
                engaged[bride] = suitor; // hook up
                break;
            }

            const string &groom = engaged[bride];

            if (prefers(women_pref[bride], suitor, groom))
            {
                cout << "\t" << bride << " dumped " << groom << " for " << suitor << "\n";
                bachelors.push(groom); // dump that zero
                engaged[bride] = suitor; // get a hero
                break;
            }
        }
        bachelors.pop(); // pop at the end to not invalidate suitor reference
    }

    cout << "Engagements:\n";
    for (Couples::const_iterator it = engaged.begin(); it != engaged.end(); ++it)
    {
        cout << "\t" << it->first << " and " << it->second << "\n";
    }

    check_stability(engaged, men_pref, women_pref);

    cout << "Perturb:\n";
    std::swap(engaged["abi"], engaged["bea"]);
    cout << "\tengage abi with " << engaged["abi"] << " and bea with " << engaged["bea"] << "\n";

    check_stability(engaged, men_pref, women_pref);
}
```

{{out}}

```txt

Matchmaking:
	abi and abe
	cath and bob
	hope and col
	ivy and dan
	jan and ed
	bea and fred
	gay and gav
	eve and hal
	hope dumped col for ian
	abi dumped abe for jon
	dee and col
	ivy dumped dan for abe
	fay and dan
Engagements:
	abi and jon
	bea and fred
	cath and bob
	dee and col
	eve and hal
	fay and dan
	gay and gav
	hope and ian
	ivy and abe
	jan and ed
Stablility:
	(all marriages stable)
Perturb:
	engage abi with fred and bea with jon
Stablility:
	bea prefers fred over jon and fred prefers bea over abi
	fay prefers jon over dan and jon prefers fay over bea
	gay prefers jon over gav and jon prefers gay over bea
	eve prefers jon over hal and jon prefers eve over bea

```


=={{header|C sharp|C#}}==
(This is a straight-up translation of the Objective-C version.)

```csharp
using System;
using System.Collections.Generic;

namespace StableMarriage
{
    class Person
    {
        private int _candidateIndex;
        public string Name { get; set; }
        public List<Person> Prefs { get; set; }
        public Person Fiance { get; set; }

        public Person(string name) {
            Name = name;
            Prefs = null;
            Fiance = null;
            _candidateIndex = 0;
        }
        public bool Prefers(Person p) {
            return Prefs.FindIndex(o => o == p) < Prefs.FindIndex(o => o == Fiance);
        }
        public Person NextCandidateNotYetProposedTo() {
            if (_candidateIndex >= Prefs.Count) return null;
            return Prefs[_candidateIndex++];
        }
        public void EngageTo(Person p) {
            if (p.Fiance != null) p.Fiance.Fiance = null;
            p.Fiance = this;
            if (Fiance != null) Fiance.Fiance = null;
            Fiance = p;
        }
    }

    static class MainClass
    {
        static public bool IsStable(List<Person> men) {
            List<Person> women = men[0].Prefs;
            foreach (Person guy in men) {
                foreach (Person gal in women) {
                    if (guy.Prefers(gal) && gal.Prefers(guy))
                        return false;
                }
            }
            return true;
        }

        static void DoMarriage() {
            Person abe  = new Person("abe");
            Person bob  = new Person("bob");
            Person col  = new Person("col");
            Person dan  = new Person("dan");
            Person ed   = new Person("ed");
            Person fred = new Person("fred");
            Person gav  = new Person("gav");
            Person hal  = new Person("hal");
            Person ian  = new Person("ian");
            Person jon  = new Person("jon");
            Person abi  = new Person("abi");
            Person bea  = new Person("bea");
            Person cath = new Person("cath");
            Person dee  = new Person("dee");
            Person eve  = new Person("eve");
            Person fay  = new Person("fay");
            Person gay  = new Person("gay");
            Person hope = new Person("hope");
            Person ivy  = new Person("ivy");
            Person jan  = new Person("jan");

            abe.Prefs  = new List<Person>() {abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay};
            bob.Prefs  = new List<Person>() {cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay};
            col.Prefs  = new List<Person>() {hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan};
            dan.Prefs  = new List<Person>() {ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi};
            ed.Prefs   = new List<Person>() {jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay};
            fred.Prefs = new List<Person>() {bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay};
            gav.Prefs  = new List<Person>() {gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay};
            hal.Prefs  = new List<Person>() {abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee};
            ian.Prefs  = new List<Person>() {hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve};
            jon.Prefs  = new List<Person>() {abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope};
            abi.Prefs  = new List<Person>() {bob, fred, jon, gav, ian, abe, dan, ed, col, hal};
            bea.Prefs  = new List<Person>() {bob, abe, col, fred, gav, dan, ian, ed, jon, hal};
            cath.Prefs = new List<Person>() {fred, bob, ed, gav, hal, col, ian, abe, dan, jon};
            dee.Prefs  = new List<Person>() {fred, jon, col, abe, ian, hal, gav, dan, bob, ed};
            eve.Prefs  = new List<Person>() {jon, hal, fred, dan, abe, gav, col, ed, ian, bob};
            fay.Prefs  = new List<Person>() {bob, abe, ed, ian, jon, dan, fred, gav, col, hal};
            gay.Prefs  = new List<Person>() {jon, gav, hal, fred, bob, abe, col, ed, dan, ian};
            hope.Prefs = new List<Person>() {gav, jon, bob, abe, ian, dan, hal, ed, col, fred};
            ivy.Prefs  = new List<Person>() {ian, col, hal, gav, fred, bob, abe, ed, jon, dan};
            jan.Prefs  = new List<Person>() {ed, hal, gav, abe, bob, jon, col, ian, fred, dan};

            List<Person> men = new List<Person>(abi.Prefs);

            int freeMenCount = men.Count;
            while (freeMenCount > 0) {
                foreach (Person guy in men) {
                    if (guy.Fiance == null) {
                        Person gal = guy.NextCandidateNotYetProposedTo();
                        if (gal.Fiance == null) {
                            guy.EngageTo(gal);
                            freeMenCount--;
                        } else if (gal.Prefers(guy)) {
                            guy.EngageTo(gal);
                        }
                    }
                }
            }

            foreach (Person guy in men) {
                Console.WriteLine("{0} is engaged to {1}", guy.Name, guy.Fiance.Name);
            }
            Console.WriteLine("Stable = {0}", IsStable(men));

            Console.WriteLine("\nSwitching fred & jon's partners");
            Person jonsFiance = jon.Fiance;
            Person fredsFiance = fred.Fiance;
            fred.EngageTo(jonsFiance);
            jon.EngageTo(fredsFiance);
            Console.WriteLine("Stable = {0}", IsStable(men));
        }

        public static void Main(string[] args)
        {
            DoMarriage();
        }
    }
}
```


{{out}}

```txt
bob is engaged to cath
fred is engaged to bea
jon is engaged to abi
gav is engaged to gay
ian is engaged to hope
abe is engaged to ivy
dan is engaged to fay
ed is engaged to jan
col is engaged to dee
hal is engaged to eve
Stable = True

Switching fred & jon's partners
Stable = False
```



## Ceylon


```ceylon
abstract class Single(name) of Gal | Guy {

    shared String name;
    shared late Single[] preferences;

    shared variable Single? fiance = null;
    shared Boolean free => fiance is Null;

    shared variable Integer currentProposalIndex = 0;

    "Does this single prefer this other single over their fiance?"
    shared Boolean prefers(Single otherSingle) =>
            let (p1 = preferences.firstIndexWhere(otherSingle.equals), f = fiance)
            if (!exists p1)
            then false
            else if (!exists f)
            then true
            else if (exists p2 = preferences.firstIndexWhere(f.equals))
            then p1 < p2
            else false;

    string => name;
}

abstract class Guy(String name) of abe | bob | col | dan | ed | fred | gav | hal | ian | jon extends Single(name) {}

object abe extends Guy("Abe") {}
object bob extends Guy("Bob") {}
object col extends Guy("Col") {}
object dan extends Guy("Dan") {}
object ed extends Guy("Ed") {}
object fred extends Guy("Fred") {}
object gav extends Guy("Gav") {}
object hal extends Guy("Hal") {}
object ian extends Guy("Ian") {}
object jon extends Guy("Jon") {}

abstract class Gal(String name) of abi | bea | cath | dee | eve | fay | gay | hope | ivy | jan extends Single(name) {}

object abi extends Gal("Abi") {}
object bea extends Gal("Bea") {}
object cath extends Gal("Cath") {}
object dee extends Gal("Dee") {}
object eve extends Gal("Eve") {}
object fay extends Gal("Fay") {}
object gay extends Gal("Gay") {}
object hope extends Gal("Hope") {}
object ivy extends Gal("Ivy") {}
object jan extends Gal("Jan") {}

Guy[] guys = `Guy`.caseValues;
Gal[] gals = `Gal`.caseValues;

"The main function. Run this one."
shared void run() {

    abe.preferences = [ abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay ];
    bob.preferences = [ cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay ];
    col.preferences = [ hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan ];
    dan.preferences = [ ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi ];
    ed.preferences = [ jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay ];
    fred.preferences = [ bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay ];
    gav.preferences = [ gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay ];
    hal.preferences = [ abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee ];
    ian.preferences = [ hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve ];
    jon.preferences = [ abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope ];

    abi.preferences = [ bob, fred, jon, gav, ian, abe, dan, ed, col, hal ];
    bea.preferences = [ bob, abe, col, fred, gav, dan, ian, ed, jon, hal ];
    cath.preferences = [ fred, bob, ed, gav, hal, col, ian, abe, dan, jon ];
    dee.preferences = [ fred, jon, col, abe, ian, hal, gav, dan, bob, ed ];
    eve.preferences = [ jon, hal, fred, dan, abe, gav, col, ed, ian, bob ];
    fay.preferences = [ bob, abe, ed, ian, jon, dan, fred, gav, col, hal ];
    gay.preferences = [ jon, gav, hal, fred, bob, abe, col, ed, dan, ian ];
    hope.preferences = [ gav, jon, bob, abe, ian, dan, hal, ed, col, fred ];
    ivy.preferences = [ ian, col, hal, gav, fred, bob, abe, ed, jon, dan ];
    jan.preferences = [ ed, hal, gav, abe, bob, jon, col, ian, fred, dan ];

    print("------ the matchmaking process ------");
    matchmake();
    print("------ the final engagements ------");
    for (guy in guys) {
        print("``guy`` is engaged to ``guy.fiance else "no one"``");
    }
    print("------ is it stable? ------");
    checkStability();
    value temp = jon.fiance;
    jon.fiance = fred.fiance;
    fred.fiance = temp;
    print("------ is it stable after switching jon and fred's partners? ------");
    checkStability();
}

"Match up all the singles with the Gale/Shapley algorithm."
void matchmake() {
    while (true) {
        value singleGuys = guys.filter(Guy.free);
        if (singleGuys.empty) {
            return;
        }
        for (guy in singleGuys) {
            if (exists gal = guy.preferences[guy.currentProposalIndex]) {
                guy.currentProposalIndex++;
                value fiance = gal.fiance;
                if (!exists fiance) {
                    print("``guy`` and ``gal`` just got engaged!");
                    guy.fiance = gal;
                    gal.fiance = guy;
                }
                else {
                    if (gal.prefers(guy)) {
                        print("``gal`` dumped ``fiance`` for ``guy``!");
                        fiance.fiance = null;
                        gal.fiance = guy;
                        guy.fiance = gal;
                    }
                    else {
                        print("``gal`` turned down ``guy`` and stayed with ``fiance``!");
                    }
                }
            }
        }
    }
}

void checkStability() {
    variable value stabilityFlag = true;
    for (gal in gals) {
        for (guy in guys) {
            if (guy.prefers(gal) && gal.prefers(guy)) {
                stabilityFlag = false;
                print("``guy`` prefers ``gal`` over ``guy.fiance else "nobody"``
                       and ``gal`` prefers ``guy`` over ``gal.fiance else "nobody"``!".normalized);
            }
        }
    }
    print("``if(!stabilityFlag) then "Not " else ""``Stable!");
}
```

{{out}}

```txt
------ the matchmaking process ------
Abe and Abi just got engaged!
Bob and Cath just got engaged!
Col and Hope just got engaged!
Dan and Ivy just got engaged!
Ed and Jan just got engaged!
Fred and Bea just got engaged!
Gav and Gay just got engaged!
Abi turned down Hal and stayed with Abe!
Hope dumped Col for Ian!
Abi dumped Abe for Jon!
Abe and Eve just got engaged!
Eve turned down Col and stayed with Abe!
Eve dumped Abe for Hal!
Cath turned down Abe and stayed with Bob!
Abi turned down Col and stayed with Jon!
Ivy dumped Dan for Abe!
Col and Dee just got engaged!
Dan and Fay just got engaged!
------ the final engagements ------
Abe is engaged to Ivy
Bob is engaged to Cath
Col is engaged to Dee
Dan is engaged to Fay
Ed is engaged to Jan
Fred is engaged to Bea
Gav is engaged to Gay
Hal is engaged to Eve
Ian is engaged to Hope
Jon is engaged to Abi
------ is it stable? ------
Stable!
------ is it stable after switching jon and fred's partners? ------
Jon prefers Eve over Bea and Eve prefers Jon over Hal!
Jon prefers Fay over Bea and Fay prefers Jon over Dan!
Jon prefers Gay over Bea and Gay prefers Jon over Gav!
Not Stable!
```



## CoffeeScript


```coffeescript
class Person
  constructor: (@name, @preferences) ->
    @mate = null
    @best_mate_rank = 0
    @rank = {}
    for preference, i in @preferences
      @rank[preference] = i

  preferred_mate_name: =>
    @preferences[@best_mate_rank]

  reject: =>
    @best_mate_rank += 1

  set_mate: (mate) =>
    @mate = mate

  offer_mate: (free_mate, reject_mate_cb) =>
    if @mate
      if @covets(free_mate)
        console.log "#{free_mate.name} steals #{@name} from #{@mate.name}"
        reject_mate_cb @mate
        free_mate.set_mate @
        @set_mate free_mate
      else
        console.log "#{free_mate.name} cannot steal #{@name} from #{@mate.name}"
        reject_mate_cb free_mate
    else
      console.log "#{free_mate.name} gets #{@name} first"
      free_mate.set_mate @
      @set_mate free_mate

  happiness: =>
    @rank[@mate.name]

  covets: (other_mate) =>
    @rank[other_mate.name] <= @rank[@mate.name]

persons_by_name = (persons) ->
  hsh = {}
  for person in persons
    hsh[person.name] = person
  hsh

mate_off = (guys, gals) ->
  free_pursuers = (guy for guy in guys)
  guys_by_name = persons_by_name guys
  gals_by_name = persons_by_name gals

  while free_pursuers.length > 0
    free_pursuer = free_pursuers.shift()
    gal_name = free_pursuer.preferred_mate_name()
    gal = gals_by_name[gal_name]
    reject_mate_cb = (guy) ->
      guy.reject()
      free_pursuers.push guy
    gal.offer_mate free_pursuer, reject_mate_cb


report_on_mates = (guys) ->
  console.log "\n----Marriage Report"
  for guy, i in guys
    throw Error("illegal marriage") if guy.mate.mate isnt guy
    console.log guy.name, guy.mate.name, \
      "(his choice #{guy.happiness()}, her choice #{guy.mate.happiness()} )"

report_potential_adulteries = (guys) ->
  for guy1, i in guys
    gal1 = guy1.mate
    for j in [0...i]
      guy2 = guys[j]
      gal2 = guy2.mate
      if guy1.covets(gal2) and gal2.covets(guy1)
        console.log "#{guy1.name} and #{gal2.name} would stray"
      if guy2.covets(gal1) and gal1.covets(guy2)
        console.log "#{guy2.name} and #{gal1.name} would stray"

perturb = (guys) ->
  # mess up marriages by swapping two couples...this is mainly to drive
  # out that report_potential_adulteries will actually work
  guy0 = guys[0]
  guy1 = guys[1]
  gal0 = guy0.mate
  gal1 = guy1.mate
  console.log "\nPerturbing with #{guy0.name}, #{gal0.name}, #{guy1.name}, #{gal1.name}"
  guy0.set_mate gal1
  guy1.set_mate gal0
  gal1.set_mate guy0
  gal0.set_mate guy1


Population = ->
  guy_preferences =
   abe:  ['abi', 'eve', 'cath', 'ivy', 'jan', 'dee', 'fay', 'bea', 'hope', 'gay']
   bob:  ['cath', 'hope', 'abi', 'dee', 'eve', 'fay', 'bea', 'jan', 'ivy', 'gay']
   col:  ['hope', 'eve', 'abi', 'dee', 'bea', 'fay', 'ivy', 'gay', 'cath', 'jan']
   dan:  ['ivy', 'fay', 'dee', 'gay', 'hope', 'eve', 'jan', 'bea', 'cath', 'abi']
   ed:   ['jan', 'dee', 'bea', 'cath', 'fay', 'eve', 'abi', 'ivy', 'hope', 'gay']
   fred: ['bea', 'abi', 'dee', 'gay', 'eve', 'ivy', 'cath', 'jan', 'hope', 'fay']
   gav:  ['gay', 'eve', 'ivy', 'bea', 'cath', 'abi', 'dee', 'hope', 'jan', 'fay']
   hal:  ['abi', 'eve', 'hope', 'fay', 'ivy', 'cath', 'jan', 'bea', 'gay', 'dee']
   ian:  ['hope', 'cath', 'dee', 'gay', 'bea', 'abi', 'fay', 'ivy', 'jan', 'eve']
   jon:  ['abi', 'fay', 'jan', 'gay', 'eve', 'bea', 'dee', 'cath', 'ivy', 'hope']

  gal_preferences =
   abi:  ['bob', 'fred', 'jon', 'gav', 'ian', 'abe', 'dan', 'ed', 'col', 'hal']
   bea:  ['bob', 'abe', 'col', 'fred', 'gav', 'dan', 'ian', 'ed', 'jon', 'hal']
   cath: ['fred', 'bob', 'ed', 'gav', 'hal', 'col', 'ian', 'abe', 'dan', 'jon']
   dee:  ['fred', 'jon', 'col', 'abe', 'ian', 'hal', 'gav', 'dan', 'bob', 'ed']
   eve:  ['jon', 'hal', 'fred', 'dan', 'abe', 'gav', 'col', 'ed', 'ian', 'bob']
   fay:  ['bob', 'abe', 'ed', 'ian', 'jon', 'dan', 'fred', 'gav', 'col', 'hal']
   gay:  ['jon', 'gav', 'hal', 'fred', 'bob', 'abe', 'col', 'ed', 'dan', 'ian']
   hope: ['gav', 'jon', 'bob', 'abe', 'ian', 'dan', 'hal', 'ed', 'col', 'fred']
   ivy:  ['ian', 'col', 'hal', 'gav', 'fred', 'bob', 'abe', 'ed', 'jon', 'dan']
   jan:  ['ed', 'hal', 'gav', 'abe', 'bob', 'jon', 'col', 'ian', 'fred', 'dan']

  guys = (new Person(name, preferences) for name, preferences of guy_preferences)
  gals = (new Person(name, preferences) for name, preferences of gal_preferences)
  [guys, gals]

do ->
  [guys, gals] = Population()
  mate_off guys, gals
  report_on_mates guys
  report_potential_adulteries guys
  perturb guys
  report_on_mates guys
  report_potential_adulteries guys
```

{{out}}

```txt

> coffee stable_marriage.coffee
abe gets abi first
bob gets cath first
col gets hope first
dan gets ivy first
ed gets jan first
fred gets bea first
gav gets gay first
hal cannot steal abi from abe
ian steals hope from col
jon steals abi from abe
hal gets eve first
col cannot steal eve from hal
abe cannot steal eve from hal
col cannot steal abi from jon
abe cannot steal cath from bob
col gets dee first
abe steals ivy from dan
dan gets fay first

----Marriage Report
abe ivy (his choice 3, her choice 6 )
bob cath (his choice 0, her choice 1 )
col dee (his choice 3, her choice 2 )
dan fay (his choice 1, her choice 5 )
ed jan (his choice 0, her choice 0 )
fred bea (his choice 0, her choice 3 )
gav gay (his choice 0, her choice 1 )
hal eve (his choice 1, her choice 1 )
ian hope (his choice 0, her choice 4 )
jon abi (his choice 0, her choice 2 )

Perturbing with abe, ivy, bob, cath

----Marriage Report
abe cath (his choice 2, her choice 7 )
bob ivy (his choice 8, her choice 5 )
col dee (his choice 3, her choice 2 )
dan fay (his choice 1, her choice 5 )
ed jan (his choice 0, her choice 0 )
fred bea (his choice 0, her choice 3 )
gav gay (his choice 0, her choice 1 )
hal eve (his choice 1, her choice 1 )
ian hope (his choice 0, her choice 4 )
jon abi (his choice 0, her choice 2 )
bob and cath would stray
bob and fay would stray
bob and bea would stray
bob and hope would stray
bob and abi would stray

```



## ColdFusion


```cfm

PERSON.CFC

component displayName="Person" accessors="true" {
    property name="Name" type="string";
    property name="MrOrMrsGoodEnough" type="Person";
    property name="UnrealisticExpectations" type="array";
    property name="PersonalHistory" type="array";

    public Person function init( required String name ) {
        setName( arguments.name );
        setPersonalHistory([ getName() & " is on the market." ]);
        this.HotnessScale = 0;
        return this;
    }

    public Boolean function hasSettled() {
        // if we have settled, return true;
        return isInstanceOf( getMrOrMrsGoodEnough(), "Person" );
    }

    public Person function getBestOfWhatIsLeft() {
        // increment the hotness scale...1 is best, 10 is...well...VERY settling.
        this.HotnessScale++;
        // get the match from the current rung in the barrel
        var bestChoice = getUnrealisticExpectations()[ this.HotnessScale ];
        return bestChoice;
    }

    public Boolean function wouldRatherBeWith( required Person person ) {
        // only compare if we've already settled on a potential mate
        if( isInstanceOf( this.getMrOrMrsGoodEnough(), "Person" ) ) {
            // if the new person's hotness is greater (numerically smaller) than our current beau...
            return getHotness( this, arguments.person ) < getHotness( this, this.getMrOrMrsGoodEnough() );
        }
        return false;
    }

    public Void function settle( required Person person ) {
        if( person.hasSettled() ) {
            // this is the match we want. Force a break up of a previous relationship (sorry!)
            dumpLikeATonOfBricks( person );
        }
        person.setMrOrMrsGoodEnough( this );
        if( hasSettled() ) {
            // this is the match we want, so write a dear john to our current match
            dumpLikeATonOfBricks( this );
        }
        logHookup( arguments.person );
        // we've found the mate of our dreams!
        setMrOrMrsGoodEnough( arguments.person );
    }

    public Void function swing( required Person person ) {
        // get our spouses
        var mySpouse = getMrOrMrsGoodEnough();
        var notMySpouse = arguments.person.getMrOrMrsGoodEnough();
        // swap em'
        setMrOrMrsGoodEnough( notMySpouse );
        person.setMrOrMrsGoodEnough( mySpouse );
    }

    public Void function dumpLikeATonOfBricks( required Person person ) {
        logBreakup( arguments.person );
        person.getMrOrMrsGoodEnough().setMrOrMrsGoodEnough( JavaCast( "null", "" ) );
    }

    public String function psychoAnalyze() {
        logNuptuals();
        logRegrets();
        var personalJourney = "";
        for( var entry in getPersonalHistory() ) {
            personalJourney = personalJourney & entry & "<br />";
        }
        return personalJourney;
    }

    private Numeric function getHotness( required Person pursuer, required Person pursued ) {
        var pursuersExpectations = pursuer.getUnrealisticExpectations();
        var hotnessFactor = 1;
        for( var hotnessFactor=1; hotnessFactor<=arrayLen( pursuersExpectations ); hotnessFactor++ ) {
            if( pursuersExpectations[ hotnessFactor ].getName()==arguments.pursued.getName() ) {
                return hotnessFactor;
            }
        }
    }

    private Void function logRegrets() {
        var spouse = getMrOrMrsGoodEnough();
        var spouseHotness = getHotness( this, spouse );
        var myHotness = getHotness( spouse, this );
        if( spouseHotness == 1 && myHotness == 1 ) {
            arrayAppend( getPersonalHistory(), "Yes, yes, the beautiful people always find happy endings: #getName()# (her ###myHotness#), #spouse.getName()# (his ###spouseHotness#)");
        }
        else if( spouseHotness == myHotness ) {
            arrayAppend( getPersonalHistory(), "#getName()# (her ###myHotness#) was made for #spouse.getName()# (his ###spouseHotness#). How precious.");
        }
        else if( spouseHotness > myHotness ) {
            arrayAppend( getPersonalHistory(), "#getName()# (her ###myHotness#) could have done better than #spouse.getName()# (his ###spouseHotness#). Poor slob.");
        }
        else {
            arrayAppend( getPersonalHistory(), "#getName()# (her ###myHotness#) is a lucky bastard to have landed #spouse.getName()# (his ###spouseHotness#).");
        }
    }

    private Void function logNuptuals() {
        arrayAppend( getPersonalHistory(), "#getName()# has settled for #getMrOrMrsGoodEnough().getName()#." );
    }

    private Void function logHookup( required Person person ) {
        var winnerHotness = getHotness( this, arguments.person );
        var myHotness = getHotness( arguments.person, this );
        arrayAppend( getPersonalHistory(), "#getName()# (her ###myHotness#) is checking out #arguments.person.getName()# (his ###winnerHotness#), but wants to keep his options open.");
    }

    private Void function logBreakup( required Person person ) {
        var scrub = person.getMrOrMrsGoodEnough();
        var scrubHotness = getHotness( person, scrub );
        var myHotness = getHotness( person, this );
        arrayAppend( getPersonalHistory(), "#getName()# is so hot (her ###myHotness#) that #person.getName()# is dumping #scrub.getName()# (her ###scrubHotness#)");
    }
}

```


```cfm

INDEX.CFM

<cfscript>
    /**
     * Let's get these crazy kids married!
     * @men.hint The men who want to get married
     */
    function doCreepyMassMarriages( required Array men ) {
        marriagesAreStable = false;
        while( !marriagesAreStable ) {
            marriagesAreStable = true;
            for( man in men ) {
                if( !man.hasSettled() ) {
                    marriagesAreStable = false;
                    sexyLady = man.getBestOfWhatIsLeft();
                    if( !sexyLady.hasSettled() || sexyLady.wouldRatherBeWith( man ) ) {
                        man.settle( sexyLady );
                    }
                }
            }
        }
        return men;
    }

    /**
     * We played God...now let's see if society is going to survive
     * @men.hint The married men
     * @women.hint The married women
     */
    function isSocietyStable( required Array men, required Array women ) {
        // loop over married men
        for( var man in arguments.men ) {
            // loop over married women
            for( var woman in arguments.women ) {
                // if the man does not prefer this woman to his current spouse, and the women
                // doesn't prefer the man to her current spouse, this is the best possible match
                if( man.wouldRatherBeWith( woman ) && woman.wouldRatherBeWith( man ) ) {
                    return false;
                }
            }
        }
        return true;
    }

    // the men
    abe = new Person( "Abe" );
    bob = new Person( "Bob" );
    col = new Person( "Col" );
    dan = new Person( "Dan" );
    ed = new Person( "Ed" );
    fred = new Person( "Fred" );
    gav = new Person( "Gav" );
    hal = new Person( "Hal" );
    ian = new Person( "Ian" );
    jon = new Person( "Jon" );

    men = [ abe, bob, col, dan, ed, fred, gav, hal, ian, jon ];

    // the women
    abi = new Person( "Abi" );
    bea = new Person( "Bea" );
    cath = new Person( "Cath" );
    dee = new Person( "Dee" );
    eve = new Person( "Eve" );
    fay = new Person( "Fay" );
    gay = new Person( "Gay" );
    hope = new Person( "Hope" );
    ivy = new Person( "Ivy" );
    jan = new Person( "Jan" );

    women = [ abi, bea, cath, dee, eve, fay, gay, hope, ivy, jan ];

    // set unrealistic expectations for the men
    abe.setUnrealisticExpectations([ abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay ]);
    bob.setUnrealisticExpectations([ cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay ]);
    col.setUnrealisticExpectations([ hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan ]);
    dan.setUnrealisticExpectations([ ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi ]);
    ed.setUnrealisticExpectations([ jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay ]);
    fred.setUnrealisticExpectations([ bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay ]);
    gav.setUnrealisticExpectations([ gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay ]);
    hal.setUnrealisticExpectations([ abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee ]);
    ian.setUnrealisticExpectations([ hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve ]);
    jon.setUnrealisticExpectations([ abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope ]);
    // set unrealistic expectations for the women
    abi.setUnrealisticExpectations([ bob, fred, jon, gav, ian, abe, dan, ed, col, hal ]);
    bea.setUnrealisticExpectations([ bob, abe, col, fred, gav, dan, ian, ed, jon, hal ]);
    cath.setUnrealisticExpectations([ fred, bob, ed, gav, hal, col, ian, abe, dan, jon ]);
    dee.setUnrealisticExpectations([ fred, jon, col, abe, ian, hal, gav, dan, bob, ed ]);
    eve.setUnrealisticExpectations([ jon, hal, fred, dan, abe, gav, col, ed, ian, bob ]);
    fay.setUnrealisticExpectations([ bob, abe, ed, ian, jon, dan, fred, gav, col, hal ]);
    gay.setUnrealisticExpectations([ jon, gav, hal, fred, bob, abe, col, ed, dan, ian ]);
    hope.setUnrealisticExpectations([ gav, jon, bob, abe, ian, dan, hal, ed, col, fred ]);
    ivy.setUnrealisticExpectations([ ian, col, hal, gav, fred, bob, abe, ed, jon, dan ]);
    jan.setUnrealisticExpectations([ ed, hal, gav, abe, bob, jon, col, ian, fred, dan ]);

    // here comes the bride, duhn, duhn, duh-duhn
    possiblyHappilyMarriedMen = doCreepyMassMarriages( men );
    // let's see who shacked up!
    for( man in possiblyHappilyMarriedMen ) {
        writeoutput( man.psychoAnalyze() & "<br />" );
    }
    // check if society is stable
    if( isSocietyStable( men, women ) ) {
        writeoutput( "Hey, look at that. Creepy social engineering works. Sort of...<br /><br />" );
    }
    // what happens if couples start swingin'?
    jon.swing( fred );
    writeoutput( "Swapping Jon and Fred's wives...will society survive?<br /><br />" );
    // check if society is still stable after the swingers
    if( !isSocietyStable( men, women ) ) {
        writeoutput( "Nope, now everything is broken. Sharing spouses doesn't work, kids.<br />" );
    }
</cfscript>

```

{{out}}

```txt

Abe is on the market.
Abe (her #6) is checking out Abi (his #1), but wants to keep his options open.
Abe (her #5) is checking out Eve (his #2), but wants to keep his options open.
Abe is so hot (her #7) that Ivy is dumping Dan (her #10)
Abe (her #7) is checking out Ivy (his #4), but wants to keep his options open.
Abe has settled for Ivy.
Abe (her #7) is lucky to have landed Ivy (his #4).

Bob is on the market.
Bob (her #2) is checking out Cath (his #1), but wants to keep his options open.
Bob has settled for Cath.
Bob (her #2) is lucky to have landed Cath (his #1).

Col is on the market.
Col (her #9) is checking out Hope (his #1), but wants to keep his options open.
Col (her #3) is checking out Dee (his #4), but wants to keep his options open.
Col has settled for Dee.
Col (her #3) could have done better than Dee (his #4).

Dan is on the market.
Dan (her #10) is checking out Ivy (his #1), but wants to keep his options open.
Dan (her #6) is checking out Fay (his #2), but wants to keep his options open.
Dan has settled for Fay.
Dan (her #6) is lucky to have landed Fay (his #2).

Ed is on the market.
Ed (her #1) is checking out Jan (his #1), but wants to keep his options open.
Ed has settled for Jan.
Yes, yes, the beautiful people always find happy endings: Ed (her #1), Jan (his #1)

Fred is on the market.
Fred (her #4) is checking out Bea (his #1), but wants to keep his options open.
Fred has settled for Bea.
Fred (her #4) is lucky to have landed Bea (his #1).

Gav is on the market.
Gav (her #2) is checking out Gay (his #1), but wants to keep his options open.
Gav has settled for Gay.
Gav (her #2) is lucky to have landed Gay (his #1).

Hal is on the market.
Hal is so hot (her #2) that Eve is dumping Abe (her #5)
Hal (her #2) is checking out Eve (his #2), but wants to keep his options open.
Hal has settled for Eve.
Hal (her #2) was made for Eve (his #2). How precious.

Ian is on the market.
Ian is so hot (her #5) that Hope is dumping Col (her #9)
Ian (her #5) is checking out Hope (his #1), but wants to keep his options open.
Ian has settled for Hope.
Ian (her #5) is lucky to have landed Hope (his #1).

Jon is on the market.
Jon is so hot (her #3) that Abi is dumping Abe (her #6)
Jon (her #3) is checking out Abi (his #1), but wants to keep his options open.
Jon has settled for Abi.
Jon (her #3) is lucky to have landed Abi (his #1).
// Is society stable?
Hey, look at that. Creepy social engineering works. Sort of...

Swapping Jon and Fred's wives...will society survive?
// How about now? Still stable?
Nope, now everything is broken. Sharing spouses doesn't work, kids.

```


## D

From the Python and Java versions:

```d
import std.stdio, std.array, std.algorithm, std.string;


string[string] matchmaker(string[][string] guyPrefers,
                          string[][string] girlPrefers) /*@safe*/ {
    string[string] engagedTo;
    string[] freeGuys = guyPrefers.keys;

    while (freeGuys.length) {
        const string thisGuy = freeGuys[0];
        freeGuys.popFront();
        const auto thisGuyPrefers = guyPrefers[thisGuy];
        foreach (girl; thisGuyPrefers) {
            if (girl !in engagedTo) { // girl is free
                engagedTo[girl] = thisGuy;
                break;
            } else {
                string otherGuy = engagedTo[girl];
                string[] thisGirlPrefers = girlPrefers[girl];
                if (thisGirlPrefers.countUntil(thisGuy) <
                    thisGirlPrefers.countUntil(otherGuy)) {
                    // this girl prefers this guy to
                    // the guy she's engagedTo to.
                    engagedTo[girl] = thisGuy;
                    freeGuys ~= otherGuy;
                    break;
                }
                // else no change, keep looking for this guy
            }
        }
    }

    return engagedTo;
}


bool check(bool doPrint=false)(string[string] engagedTo,
                               string[][string] guyPrefers,
                               string[][string] galPrefers) @safe {
    enum MSG = "%s likes %s better than %s and %s " ~
               "likes %s better than their current partner";
    string[string] inverseEngaged;
    foreach (k, v; engagedTo)
        inverseEngaged[v] = k;

    foreach (she, he; engagedTo) {
        auto sheLikes = galPrefers[she];
        auto sheLikesBetter = sheLikes[0 .. sheLikes.countUntil(he)];
        auto heLikes = guyPrefers[he];
        auto heLikesBetter = heLikes[0 .. heLikes.countUntil(she)];
        foreach (guy; sheLikesBetter) {
            auto guysGirl = inverseEngaged[guy];
            auto guyLikes = guyPrefers[guy];

            if (guyLikes.countUntil(guysGirl) >
                guyLikes.countUntil(she)) {
                static if (doPrint)
                    writefln(MSG, she, guy, he, guy, she);
                return false;
            }
        }

        foreach (gal; heLikesBetter) {
            auto girlsGuy = engagedTo[gal];
            auto galLikes = galPrefers[gal];

            if (galLikes.countUntil(girlsGuy) >
                galLikes.countUntil(he)) {
                static if (doPrint)
                    writefln(MSG, he, gal, she, gal, he);
                return false;
            }
        }
    }

    return true;
}


void main() /*@safe*/ {
    auto guyData = "abe  abi eve cath ivy jan dee fay bea hope gay
                    bob  cath hope abi dee eve fay bea jan ivy gay
                    col  hope eve abi dee bea fay ivy gay cath jan
                    dan  ivy fay dee gay hope eve jan bea cath abi
                    ed   jan dee bea cath fay eve abi ivy hope gay
                    fred bea abi dee gay eve ivy cath jan hope fay
                    gav  gay eve ivy bea cath abi dee hope jan fay
                    hal  abi eve hope fay ivy cath jan bea gay dee
                    ian  hope cath dee gay bea abi fay ivy jan eve
                    jon  abi fay jan gay eve bea dee cath ivy hope";

    auto galData = "abi  bob fred jon gav ian abe dan ed col hal
                    bea  bob abe col fred gav dan ian ed jon hal
                    cath fred bob ed gav hal col ian abe dan jon
                    dee  fred jon col abe ian hal gav dan bob ed
                    eve  jon hal fred dan abe gav col ed ian bob
                    fay  bob abe ed ian jon dan fred gav col hal
                    gay  jon gav hal fred bob abe col ed dan ian
                    hope gav jon bob abe ian dan hal ed col fred
                    ivy  ian col hal gav fred bob abe ed jon dan
                    jan  ed hal gav abe bob jon col ian fred dan";

    string[][string] guyPrefers, galPrefers;
    foreach (line; guyData.splitLines())
        guyPrefers[split(line)[0]] = split(line)[1..$];
    foreach (line; galData.splitLines())
        galPrefers[split(line)[0]] = split(line)[1..$];

    writeln("Engagements:");
    auto engagedTo = matchmaker(guyPrefers, galPrefers);

    writeln("\nCouples:");
    string[] parts;
    foreach (k; engagedTo.keys.sort())
        writefln("%s is engagedTo to %s", k, engagedTo[k]);
    writeln();

    bool c = check!(true)(engagedTo, guyPrefers, galPrefers);
    writeln("Marriages are ", c ? "stable" : "unstable");

    writeln("\n\nSwapping two fiances to introduce an error");
    auto gals = galPrefers.keys.sort();
    swap(engagedTo[gals[0]], engagedTo[gals[1]]);
    foreach (gal; gals[0 .. 2])
        writefln("  %s is now engagedTo to %s", gal, engagedTo[gal]);
    writeln();

    c = check!(true)(engagedTo, guyPrefers, galPrefers);
    writeln("Marriages are ", c ? "stable" : "unstable");
}
```

{{out}}

```txt
Engagements:

Couples:
abi is engagedTo to jon
bea is engagedTo to fred
cath is engagedTo to bob
dee is engagedTo to col
eve is engagedTo to hal
fay is engagedTo to dan
gay is engagedTo to gav
hope is engagedTo to ian
ivy is engagedTo to abe
jan is engagedTo to ed

Marriages are stable


Swapping two fiances to introduce an error
  abi is now engagedTo to fred
  bea is now engagedTo to jon

fred likes bea better than abi and bea likes fred better than their current partner
Marriages are unstable
```


### Stronger Version


```d
import std.stdio, std.algorithm, std.array;

enum F { abi, bea, cath, dee, eve, fay, gay, hope, ivy, jan }
enum M { abe, bob, col, dan, ed, fred, gav, hal, ian, jon }

alias PrefMapF = M[][F];
alias PrefMapM = F[][M];
alias Couples = M[F];

immutable PrefMapF womenPref;
immutable PrefMapM menPref;

static this() pure nothrow @safe {
    with (F) with (M) {
        womenPref = [
             abi:  [bob, fred, jon, gav, ian, abe, dan, ed, col, hal],
             bea:  [bob, abe, col, fred, gav, dan, ian, ed, jon, hal],
             cath: [fred, bob, ed, gav, hal, col, ian, abe, dan, jon],
             dee:  [fred, jon, col, abe, ian, hal, gav, dan, bob, ed],
             eve:  [jon, hal, fred, dan, abe, gav, col, ed, ian, bob],
             fay:  [bob, abe, ed, ian, jon, dan, fred, gav, col, hal],
             gay:  [jon, gav, hal, fred, bob, abe, col, ed, dan, ian],
             hope: [gav, jon, bob, abe, ian, dan, hal, ed, col, fred],
             ivy:  [ian, col, hal, gav, fred, bob, abe, ed, jon, dan],
             jan:  [ed, hal, gav, abe, bob, jon, col, ian, fred, dan]
        ];

        menPref = [
             abe:  [abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay],
             bob:  [cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay],
             col:  [hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan],
             dan:  [ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi],
             ed:   [jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay],
             fred: [bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay],
             gav:  [gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay],
             hal:  [abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee],
             ian:  [hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve],
             jon:  [abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope]
        ];
    }
}

/// Does 'first' appear before 'second' in preference list?
bool prefers(T)(in T[] preference, in T first, in T second)
pure nothrow @safe @nogc if (is(T == F) || is(T == M)) {
    //const found = preference.findAmong([first, second]);
    immutable T[2] two = [first, second];
    const found = preference.findAmong(two[]);
    return !(found.empty || found.front == second);
}

void checkStability(in Couples engaged, in PrefMapM menPref,
                    in PrefMapF womenPref) @safe {
    "Stablility:".writeln;
    bool stable = true;
    foreach (immutable bride, immutable groom; engaged) {
        const prefList = menPref[groom];

        foreach (immutable pr; prefList) {
            if (pr == bride) // He prefers his bride.
                break;

            if (prefers(prefList, pr, bride) &&
                // He prefers another woman.
                prefers(womenPref[pr], groom, engaged[pr])) {
                // Other woman prefers him.
                writeln("\t", pr, " prefers ", groom, " over ",
                        engaged[pr], " and ", groom, " prefers ",
                        pr, " over ", bride);
                stable = false;
            }
        }
    }

    if (stable)
        "\t(all marriages stable)".writeln;
}

void main() /*@safe*/ {
    auto bachelors = menPref.keys.sort().release;// No queue in Phobos.
    Couples engaged;

    "Matchmaking:".writeln;
    while (!bachelors.empty) {
        immutable suitor = bachelors[0];
        bachelors.popFront;
        immutable prefList = menPref[suitor];

        foreach (immutable bride; prefList) {
            if (bride !in engaged) { // She's available.
                writeln("\t", bride, " and ", suitor);
                engaged[bride] = suitor; // Hook up.
                break;
            }

            immutable groom = engaged[bride];
            if (prefers(womenPref[bride], suitor, groom)) {
                writeln("\t", bride, " dumped ", groom,
                        " for ", suitor);
                bachelors ~= groom; // Dump that zero.
                engaged[bride] = suitor; // Get a hero.
                break;
            }
        }
    }

    "Engagements:".writeln;
    foreach (immutable first, immutable second; engaged)
        writeln("\t", first, " and ", second);

    checkStability(engaged, menPref, womenPref);

    "Perturb:".writeln;
    engaged[F.abi].swap(engaged[F.bea]);
    writeln("\tengage abi with ", engaged[F.abi],
            " and bea with ", engaged[F.bea]);

    checkStability(engaged, menPref, womenPref);
}

```

{{out}}

```txt
Matchmaking:
    abi and abe
    cath and bob
    hope and col
    ivy and dan
    jan and ed
    bea and fred
    gay and gav
    eve and hal
    hope dumped col for ian
    abi dumped abe for jon
    dee and col
    ivy dumped dan for abe
    fay and dan
Engagements:
    abi and jon
    ivy and abe
    eve and hal
    jan and ed
    bea and fred
    fay and dan
    cath and bob
    gay and gav
    hope and ian
    dee and col
Stablility:
    (all marriages stable)
Perturb:
    engage abi with fred and bea with jon
Stablility:
    bea prefers fred over jon and fred prefers bea over abi
    fay prefers jon over dan and jon prefers fay over bea
    gay prefers jon over gav and jon prefers gay over bea
    eve prefers jon over hal and jon prefers eve over bea
```



## EchoLisp


```scheme

(lib 'hash)
;; input data
(define M-RANKS
'(( abe abi eve cath ivy jan dee fay bea hope gay)
(  bob cath hope abi dee eve fay bea jan ivy gay)
(  col hope eve abi dee bea fay ivy gay cath jan)
(  dan ivy fay dee gay hope eve jan bea cath abi)
(   ed jan dee bea cath fay eve abi ivy hope gay)
( fred bea abi dee gay eve ivy cath jan hope fay)
(  gav gay eve ivy bea cath abi dee hope jan fay)
(  hal abi eve hope fay ivy cath jan bea gay dee)
(  ian hope cath dee gay bea abi fay ivy jan eve)
(  jon abi fay jan gay eve bea dee cath ivy hope)))

(define W-RANKS
'((  abi bob fred jon gav ian abe dan ed col hal)
(  bea bob abe col fred gav dan ian ed jon hal)
( cath fred bob ed gav hal col ian abe dan jon)
(  dee fred jon col abe ian hal gav dan bob ed)
(  eve jon hal fred dan abe gav col ed ian bob)
(  fay bob abe ed ian jon dan fred gav col hal)
(  gay jon gav hal fred bob abe col ed dan ian)
( hope gav jon bob abe ian dan hal ed col fred)
(  ivy ian col hal gav fred bob abe ed jon dan)
(  jan ed hal gav abe bob jon col ian fred dan)))

;; build preferences hash
(define (set-prefs ranks  prefs)
    (for/list ((r ranks))
        (hash-set prefs (first r) (rest r))
        (first r)))

(define (engage  m w)    (hash-set ENGAGED m w) (hash-set ENGAGED w m) (writeln  m w '👫 ))
(define (disengage  m w) (hash-remove! ENGAGED m ) (hash-remove! ENGAGED w) (writeln '💔 m w))
(define (engaged x)      (hash-ref ENGAGED x))
(define (free? x)        (not (engaged x)))
(define (free-man men)   (for ((man men)) #:break (free? man) => man  #f))


(define (prefers? prefs x a b) (member b  (member a (hash-ref prefs x))))
;; get first choice and remove it from prefs list
(define (first-choice prefs m)
    (define w (first (hash-ref prefs m)))
    (hash-set prefs m (rest (hash-ref prefs m)))
    w)

;; sets ENGAGED couples
;;  https//en.wikipedia.org/wiki/Stable_marriage_problem

(define (stableMatching  (prefs (make-hash)) (m) (w))
(define-global 'ENGAGED (make-hash))
  (define men   (set-prefs  M-RANKS prefs))
  (define women (set-prefs  W-RANKS prefs))
    (while (setv! m (free-man men))
        (set! w (first-choice prefs m))
        (if (free? w)
            (engage m w)
            (let [(dumped (engaged w))]
            (when (prefers? prefs w m dumped)
                (disengage w dumped)
                (engage w m)))))
 (hash->list ENGAGED))

;; input : ENGAGED couples
(define (checkStable (prefs (make-hash)))
  (define men   (set-prefs  M-RANKS  prefs))
  (define women (set-prefs  W-RANKS  prefs))
	(for* [(man men) (woman women)]
	#:continue (equal? woman (engaged man))
			(when (and
					(prefers? prefs man woman (engaged man))
					(prefers? prefs woman man (engaged woman)))
					(error 'not-stable (list man woman)))))


```

{{out}}

```txt

(stableMatching)
👫     abe     abi
👫     bob     cath
👫     col     hope
👫     dan     ivy
👫     ed     jan
👫     fred     bea
👫     gav     gay
👫     hal     eve
💔     hope     col
👫     hope     ian
👫     col     dee
💔     abi     abe
👫     abi     jon
💔     ivy     dan
👫     ivy     abe
👫     dan     fay
  ((abe . ivy) (abi . jon) (bob . cath) (cath . bob) (col . dee) (hope . ian) (dan . fay) (ivy . abe) (ed . jan)
 (jan . ed) (fred . bea) (bea . fred) (gav . gay) (gay . gav) (hal . eve) (eve . hal) (ian . hope) (dee . col) (jon . abi) (fay . dan))

(disengage 'abe 'ivy)
(disengage 'hope 'ian)
(engage 'abe 'hope)
(engage 'ivy 'ian)
(checkStable)

💔     abe     ivy
💔     hope     ian
abe     hope     👫
ivy     ian     👫
😡 error: not-stable (abe bea)

```


=={{header|F_Sharp|F#}}==

```fsharp
let menPrefs =
  Map.ofList
            ["abe",  ["abi";"eve";"cath";"ivy";"jan";"dee";"fay";"bea";"hope";"gay"];
             "bob",  ["cath";"hope";"abi";"dee";"eve";"fay";"bea";"jan";"ivy";"gay"];
             "col",  ["hope";"eve";"abi";"dee";"bea";"fay";"ivy";"gay";"cath";"jan"];
             "dan",  ["ivy";"fay";"dee";"gay";"hope";"eve";"jan";"bea";"cath";"abi"];
             "ed",   ["jan";"dee";"bea";"cath";"fay";"eve";"abi";"ivy";"hope";"gay"];
             "fred", ["bea";"abi";"dee";"gay";"eve";"ivy";"cath";"jan";"hope";"fay"];
             "gav",  ["gay";"eve";"ivy";"bea";"cath";"abi";"dee";"hope";"jan";"fay"];
             "hal",  ["abi";"eve";"hope";"fay";"ivy";"cath";"jan";"bea";"gay";"dee"];
             "ian",  ["hope";"cath";"dee";"gay";"bea";"abi";"fay";"ivy";"jan";"eve"];
             "jon",  ["abi";"fay";"jan";"gay";"eve";"bea";"dee";"cath";"ivy";"hope"];
            ]

let womenPrefs =
   Map.ofList
              ["abi",  ["bob";"fred";"jon";"gav";"ian";"abe";"dan";"ed";"col";"hal"];
               "bea",  ["bob";"abe";"col";"fred";"gav";"dan";"ian";"ed";"jon";"hal"];
               "cath", ["fred";"bob";"ed";"gav";"hal";"col";"ian";"abe";"dan";"jon"];
               "dee",  ["fred";"jon";"col";"abe";"ian";"hal";"gav";"dan";"bob";"ed"];
               "eve",  ["jon";"hal";"fred";"dan";"abe";"gav";"col";"ed";"ian";"bob"];
               "fay",  ["bob";"abe";"ed";"ian";"jon";"dan";"fred";"gav";"col";"hal"];
               "gay",  ["jon";"gav";"hal";"fred";"bob";"abe";"col";"ed";"dan";"ian"];
               "hope", ["gav";"jon";"bob";"abe";"ian";"dan";"hal";"ed";"col";"fred"];
               "ivy",  ["ian";"col";"hal";"gav";"fred";"bob";"abe";"ed";"jon";"dan"];
               "jan",  ["ed";"hal";"gav";"abe";"bob";"jon";"col";"ian";"fred";"dan"];
              ]

let men = menPrefs |> Map.toList |> List.map fst |> List.sort
let women = womenPrefs |> Map.toList |> List.map fst |> List.sort


type Configuration =
 {
   proposed: Map<string,string list>; // man -> list of women
   wifeOf: Map<string, string>; // man -> woman
   husbandOf: Map<string, string>;  // woman -> man
 }


// query functions

let isFreeMan config man = config.wifeOf.TryFind man = None

let isFreeWoman config woman = config.husbandOf.TryFind woman = None

let hasProposedTo config man woman =
  defaultArg (config.proposed.TryFind(man)) []
  |> List.exists ((=) woman)

// helper
let negate f = fun x -> not (f x)

// returns those 'women' who 'man' has not proposed to before
let notProposedBy config man women = List.filter (negate (hasProposedTo config man)) women

let prefers (prefs:Map<string,string list>) w m1 m2 =
  let order = prefs.[w]
  let m1i = List.findIndex ((=) m1) order
  let m2i = List.findIndex ((=) m2) order
  m1i < m2i

let womanPrefers = prefers womenPrefs
let manPrefers = prefers menPrefs

// returns the women that m likes better than his current fiancée
let preferredWomen config m =
  let w = config.wifeOf.[m]
  women
  |> List.filter (fun w' -> manPrefers m w' w)  // '

// whether there is a woman who m likes better than his current fiancée
// and who also likes him better than her current fiancé
let prefersAWomanWhoAlsoPrefersHim config m =
  preferredWomen config m
  |> List.exists (fun w -> womanPrefers w m config.husbandOf.[w])

let isStable config =
  not (List.exists (prefersAWomanWhoAlsoPrefersHim config) men)


// modifiers (return new configurations)

let engage config man woman =
  { config with wifeOf = config.wifeOf.Add(man, woman);
                husbandOf = config.husbandOf.Add(woman, man) }

let breakOff config man =
  let woman = config.wifeOf.[man]
  { config with wifeOf = config.wifeOf.Remove(man);
                husbandOf = config.husbandOf.Remove(woman) }

let propose config m w =
  // remember the proposition
  let proposedByM = defaultArg (config.proposed.TryFind m) []
  let proposed' = config.proposed.Add(m, w::proposedByM) // '
  let config = { config with proposed = proposed'}  // '
  // actually try to engage
  if isFreeWoman config w then engage config m w
  else
    let m' = config.husbandOf.[w] // '
    if womanPrefers w m m' then // '
      let config = breakOff config m' // '
      engage config m w
    else
      config

// do one step of the algorithm; returns None if no more steps are possible
let step config : Configuration option =
  let freeMen = men |> List.filter (isFreeMan config)
  let menWhoCanPropose =
    freeMen |>
    List.filter (fun man -> (notProposedBy config man women) <> [] )
  match menWhoCanPropose with
  | [] -> None
  | m::_ -> let unproposedByM = menPrefs.[m] |> notProposedBy config m
            // w is automatically the highest ranked because menPrefs.[m] is the source
            let w = List.head unproposedByM
            Some( propose config m w )

let rec loop config =
  match step config with
  | None -> config
  | Some config' -> loop config' // '


// find solution and print it
let solution = loop { proposed = Map.empty<string, string list>;
                      wifeOf = Map.empty<string, string>;
                      husbandOf = Map.empty<string, string> }

for woman, man in Map.toList solution.husbandOf do
  printfn "%s is engaged to %s" woman man

printfn "Solution is stable: %A" (isStable solution)


// create unstable configuration by perturbing the solution
let perturbed =
  let gal0 = women.[0]
  let gal1 = women.[1]
  let guy0 = solution.husbandOf.[gal0]
  let guy1 = solution.husbandOf.[gal1]
  { solution with wifeOf = solution.wifeOf.Add( guy0, gal1 ).Add( guy1, gal0 );
                  husbandOf = solution.husbandOf.Add( gal0, guy1 ).Add( gal1, guy0 ) }

printfn "Perturbed is stable: %A" (isStable perturbed)
```

{{out}}

```txt

abi is engaged to jon
bea is engaged to fred
cath is engaged to bob
dee is engaged to col
eve is engaged to hal
fay is engaged to dan
gay is engaged to gav
hope is engaged to ian
ivy is engaged to abe
jan is engaged to ed
Solution is stable: true
Perturbed is stable: false

```



## Go


```go
package main

import "fmt"

// Asymetry in the algorithm suggests different data structures for the
// map value types of the proposers and the recipients.  Proposers go down
// their list of preferences in order, and do not need random access.
// Recipients on the other hand must compare their preferences to arbitrary
// proposers.  A slice is adequate for proposers, but a map allows direct
// lookups for recipients and avoids looping code.
type proposers map[string][]string

var mPref = proposers{
    "abe": []string{
        "abi", "eve", "cath", "ivy", "jan",
        "dee", "fay", "bea", "hope", "gay"},
    "bob": []string{
        "cath", "hope", "abi", "dee", "eve",
        "fay", "bea", "jan", "ivy", "gay"},
    "col": []string{
        "hope", "eve", "abi", "dee", "bea",
        "fay", "ivy", "gay", "cath", "jan"},
    "dan": []string{
        "ivy", "fay", "dee", "gay", "hope",
        "eve", "jan", "bea", "cath", "abi"},
    "ed": []string{
        "jan", "dee", "bea", "cath", "fay",
        "eve", "abi", "ivy", "hope", "gay"},
    "fred": []string{
        "bea", "abi", "dee", "gay", "eve",
        "ivy", "cath", "jan", "hope", "fay"},
    "gav": []string{
        "gay", "eve", "ivy", "bea", "cath",
        "abi", "dee", "hope", "jan", "fay"},
    "hal": []string{
        "abi", "eve", "hope", "fay", "ivy",
        "cath", "jan", "bea", "gay", "dee"},
    "ian": []string{
        "hope", "cath", "dee", "gay", "bea",
        "abi", "fay", "ivy", "jan", "eve"},
    "jon": []string{
        "abi", "fay", "jan", "gay", "eve",
        "bea", "dee", "cath", "ivy", "hope"},
}

type recipients map[string]map[string]int

var wPref = recipients{
    "abi": map[string]int{
        "bob": 1, "fred": 2, "jon": 3, "gav": 4, "ian": 5,
        "abe": 6, "dan": 7, "ed": 8, "col": 9, "hal": 10},
    "bea": map[string]int{
        "bob": 1, "abe": 2, "col": 3, "fred": 4, "gav": 5,
        "dan": 6, "ian": 7, "ed": 8, "jon": 9, "hal": 10},
    "cath": map[string]int{
        "fred": 1, "bob": 2, "ed": 3, "gav": 4, "hal": 5,
        "col": 6, "ian": 7, "abe": 8, "dan": 9, "jon": 10},
    "dee": map[string]int{
        "fred": 1, "jon": 2, "col": 3, "abe": 4, "ian": 5,
        "hal": 6, "gav": 7, "dan": 8, "bob": 9, "ed": 10},
    "eve": map[string]int{
        "jon": 1, "hal": 2, "fred": 3, "dan": 4, "abe": 5,
        "gav": 6, "col": 7, "ed": 8, "ian": 9, "bob": 10},
    "fay": map[string]int{
        "bob": 1, "abe": 2, "ed": 3, "ian": 4, "jon": 5,
        "dan": 6, "fred": 7, "gav": 8, "col": 9, "hal": 10},
    "gay": map[string]int{
        "jon": 1, "gav": 2, "hal": 3, "fred": 4, "bob": 5,
        "abe": 6, "col": 7, "ed": 8, "dan": 9, "ian": 10},
    "hope": map[string]int{
        "gav": 1, "jon": 2, "bob": 3, "abe": 4, "ian": 5,
        "dan": 6, "hal": 7, "ed": 8, "col": 9, "fred": 10},
    "ivy": map[string]int{
        "ian": 1, "col": 2, "hal": 3, "gav": 4, "fred": 5,
        "bob": 6, "abe": 7, "ed": 8, "jon": 9, "dan": 10},
    "jan": map[string]int{
        "ed": 1, "hal": 2, "gav": 3, "abe": 4, "bob": 5,
        "jon": 6, "col": 7, "ian": 8, "fred": 9, "dan": 10},
}

func main() {
    // get parings by Gale/Shapley algorithm
    ps := pair(mPref, wPref)
    // show results
    fmt.Println("\nresult:")
    if !validateStable(ps, mPref, wPref) {
        return
    }
    // perturb
    for {
        i := 0
        var w2, m2 [2]string
        for w, m := range ps {
            w2[i] = w
            m2[i] = m
            if i == 1 {
                break
            }
            i++
        }
        fmt.Println("\nexchanging partners of", m2[0], "and", m2[1])
        ps[w2[0]] = m2[1]
        ps[w2[1]] = m2[0]
        // validate perturbed parings
        if !validateStable(ps, mPref, wPref) {
            return
        }
        // if those happened to be stable as well, perturb more
    }
}

type parings map[string]string // map[recipient]proposer (or map[w]m)

// Pair implements the Gale/Shapley algorithm.
func pair(pPref proposers, rPref recipients) parings {
    // code is destructive on the maps, so work with copies
    pFree := proposers{}
    for k, v := range pPref {
        pFree[k] = append([]string{}, v...)
    }
    rFree := recipients{}
    for k, v := range rPref {
        rFree[k] = v
    }
    // struct only used in this function.
    // preferences must be saved in case engagement is broken.
    type save struct {
        proposer string
        pPref    []string
        rPref    map[string]int
    }
    proposals := map[string]save{} // key is recipient (w)

    // WP pseudocode comments prefaced with WP: m is proposer, w is recipient.
    // WP: while ∃ free man m who still has a woman w to propose to
    for len(pFree) > 0 { // while there is a free proposer,
        var proposer string
        var ppref []string
        for proposer, ppref = range pFree {
            break // pick a proposer at random, whatever range delivers first.
        }
        if len(ppref) == 0 {
            continue // if proposer has no possible recipients, skip
        }
        // WP: w = m's highest ranked such woman to whom he has not yet proposed
        recipient := ppref[0] // highest ranged is first in list.
        ppref = ppref[1:]     // pop from list
        var rpref map[string]int
        var ok bool
        // WP: if w is free
        if rpref, ok = rFree[recipient]; ok {
            // WP: (m, w) become engaged
            // (common code follows if statement)
        } else {
            // WP: else some pair (m', w) already exists
            s := proposals[recipient] // get proposal saved preferences
            // WP: if w prefers m to m'
            if s.rPref[proposer] < s.rPref[s.proposer] {
                fmt.Println("engagement broken:", recipient, s.proposer)
                // WP: m' becomes free
                pFree[s.proposer] = s.pPref // return proposer to the map
                // WP: (m, w) become engaged
                rpref = s.rPref
                // (common code follows if statement)
            } else {
                // WP: else (m', w) remain engaged
                pFree[proposer] = ppref // update preferences in map
                continue
            }
        }
        fmt.Println("engagement:", recipient, proposer)
        proposals[recipient] = save{proposer, ppref, rpref}
        delete(pFree, proposer)
        delete(rFree, recipient)
    }
    // construct return value
    ps := parings{}
    for recipient, s := range proposals {
        ps[recipient] = s.proposer
    }
    return ps
}

func validateStable(ps parings, pPref proposers, rPref recipients) bool {
    for r, p := range ps {
        fmt.Println(r, p)
    }
    for r, p := range ps {
        for _, rp := range pPref[p] {
            if rp == r {
                break
            }
            rprefs := rPref[rp]
            if rprefs[p] < rprefs[ps[rp]] {
                fmt.Println("unstable.")
                fmt.Printf("%s and %s would prefer each other over"+
                    " their current pairings.\n", p, rp)
                return false
            }
        }
    }
    fmt.Println("stable.")
    return true
}
```

{{out}}

```txt

engagement: hope col
engagement: bea fred
engagement: ivy dan
engagement: cath bob
engagement: abi abe
engagement broken: abi abe
engagement: abi jon
engagement: gay gav
engagement: eve abe
engagement: jan ed
engagement broken: hope col
engagement: hope ian
engagement: dee col
engagement broken: eve abe
engagement: eve hal
engagement broken: ivy dan
engagement: ivy abe
engagement: fay dan

result:
fay dan
dee col
cath bob
hope ian
eve hal
jan ed
abi jon
gay gav
ivy abe
bea fred
stable.

exchanging partners of fred and dan
ivy abe
bea dan
fay fred
dee col
cath bob
hope ian
eve hal
jan ed
abi jon
gay gav
unstable.
dan and fay would prefer each other over their current pairings.

```



## Groovy

{{trans|Java}} (more or less) Uses explicit maps for preference ranking rather than list position. Uses Man and Woman enumerated types instead of string names, in order to take advantage of compile time type and constant checking to help keep the playas straight without a scorecard.

"Stable Matching" Solution:

```groovy
import static Man.*
import static Woman.*

Map<Woman,Man> match(Map<Man,Map<Woman,Integer>> guysGalRanking, Map<Woman,Map<Man,Integer>> galsGuyRanking) {
    Map<Woman,Man> engagedTo = new TreeMap()
    List<Man> freeGuys = (Man.values()).clone()
    while(freeGuys) {
        Man thisGuy = freeGuys[0]
        freeGuys -= thisGuy
        List<Woman> guyChoices = Woman.values().sort{ she -> - guysGalRanking[thisGuy][she] }
        for(Woman girl in guyChoices) {
            if(! engagedTo[girl]) {
                engagedTo[girl] = thisGuy
                break
            } else {
                Man thatGuy = engagedTo[girl]
                if (galsGuyRanking[girl][thisGuy] > galsGuyRanking[girl][thatGuy]) {
                    engagedTo[girl] = thisGuy
                    freeGuys << thatGuy
                    break
                }
            }
        }
    }
    engagedTo
}
```


"Stability Checking" Solution:
(Could do more to eliminate common code. Maybe later.)

```groovy
boolean isStable(Map<Woman,Man> matches, Map<Man,Map<Woman,Integer>> guysGalRanking, Map<Woman,Map<Man,Integer>> galsGuyRanking) {
    matches.collect{ girl, guy ->
        int guysRank = galsGuyRanking[girl][guy]
        List<Man> sheLikesBetter = Man.values().findAll{ he -> galsGuyRanking[girl][he] > guysRank }
        for(Man otherGuy : sheLikesBetter) {
            Woman otherGuyFiancee = matches.find{ pair -> pair.value == otherGuy }.key
            if(guysGalRanking[otherGuy][girl] > guysGalRanking[otherGuy][otherGuyFiancee]) {
                println """O. M. G. ... ${otherGuy} likes ${girl} better than ${otherGuyFiancee}, and ${girl} likes ${otherGuy} better than ${guy}!
                            I am TOTALLY 'shipping ${girl} and ${otherGuy} now!"""
                return false
            }
        }

        int galsRank = guysGalRanking[guy][girl]
        List<Woman> heLikesBetter = Woman.values().findAll{ she -> guysGalRanking[guy][she] > galsRank }
        for(Woman otherGal : heLikesBetter) {
            Man otherGalFiance = matches[otherGal]
            if(galsGuyRanking[otherGal][guy] > galsGuyRanking[otherGal][otherGalFiance]) {
                println """O. M. G. ... ${otherGal} likes ${guy} better than ${otherGalFiance}, and ${guy} likes ${otherGal} better than ${girl}!
                            I am TOTALLY 'shipping ${guy} and ${otherGal} now!"""
                return false
            }
        }
        true
    }.every()
}
```


Test (Stable and Perturbed):

```groovy
enum Man {
    abe, bob, col, dan, ed, fred, gav, hal, ian, jon
}

enum Woman {
    abi, bea, cath, dee, eve, fay, gay, hope, ivy, jan
}

Map<Man,Map<Woman,Integer>> mansWomanRanking = [
    (abe): [(abi):10, (eve):9, (cath):8, (ivy):7, (jan):6, (dee):5, (fay):4, (bea):3, (hope):2, (gay):1],
    (bob): [(cath):10, (hope):9, (abi):8, (dee):7, (eve):6, (fay):5, (bea):4, (jan):3, (ivy):2, (gay):1],
    (col): [(hope):10, (eve):9, (abi):8, (dee):7, (bea):6, (fay):5, (ivy):4, (gay):3, (cath):2, (jan):1],
    (dan): [(ivy):10, (fay):9, (dee):8, (gay):7, (hope):6, (eve):5, (jan):4, (bea):3, (cath):2, (abi):1],
    (ed):  [(jan):10, (dee):9, (bea):8, (cath):7, (fay):6, (eve):5, (abi):4, (ivy):3, (hope):2, (gay):1],
    (fred):[(bea):10, (abi):9, (dee):8, (gay):7, (eve):6, (ivy):5, (cath):4, (jan):3, (hope):2, (fay):1],
    (gav): [(gay):10, (eve):9, (ivy):8, (bea):7, (cath):6, (abi):5, (dee):4, (hope):3, (jan):2, (fay):1],
    (hal): [(abi):10, (eve):9, (hope):8, (fay):7, (ivy):6, (cath):5, (jan):4, (bea):3, (gay):2, (dee):1],
    (ian): [(hope):10, (cath):9, (dee):8, (gay):7, (bea):6, (abi):5, (fay):4, (ivy):3, (jan):2, (eve):1],
    (jon): [(abi):10, (fay):9, (jan):8, (gay):7, (eve):6, (bea):5, (dee):4, (cath):3, (ivy):2, (hope):1],
]

Map<Woman,List<Man>> womansManRanking = [
    (abi): [(bob):10, (fred):9, (jon):8, (gav):7, (ian):6, (abe):5, (dan):4, (ed):3, (col):2, (hal):1],
    (bea): [(bob):10, (abe):9, (col):8, (fred):7, (gav):6, (dan):5, (ian):4, (ed):3, (jon):2, (hal):1],
    (cath):[(fred):10, (bob):9, (ed):8, (gav):7, (hal):6, (col):5, (ian):4, (abe):3, (dan):2, (jon):1],
    (dee): [(fred):10, (jon):9, (col):8, (abe):7, (ian):6, (hal):5, (gav):4, (dan):3, (bob):2, (ed):1],
    (eve): [(jon):10, (hal):9, (fred):8, (dan):7, (abe):6, (gav):5, (col):4, (ed):3, (ian):2, (bob):1],
    (fay): [(bob):10, (abe):9, (ed):8, (ian):7, (jon):6, (dan):5, (fred):4, (gav):3, (col):2, (hal):1],
    (gay): [(jon):10, (gav):9, (hal):8, (fred):7, (bob):6, (abe):5, (col):4, (ed):3, (dan):2, (ian):1],
    (hope):[(gav):10, (jon):9, (bob):8, (abe):7, (ian):6, (dan):5, (hal):4, (ed):3, (col):2, (fred):1],
    (ivy): [(ian):10, (col):9, (hal):8, (gav):7, (fred):6, (bob):5, (abe):4, (ed):3, (jon):2, (dan):1],
    (jan): [(ed):10, (hal):9, (gav):8, (abe):7, (bob):6, (jon):5, (col):4, (ian):3, (fred):2, (dan):1],
]

// STABLE test
Map<Woman,Man> matches = match(mansWomanRanking, womansManRanking)
matches.each { w, m ->
    println "${w} (his '${mansWomanRanking[m][w]}' girl) is engaged to ${m} (her '${womansManRanking[w][m]}' guy)"
}
assert matches.keySet() == Woman.values() as Set
assert matches.values() as Set == Man.values() as Set
println ''

assert isStable(matches, mansWomanRanking, womansManRanking)

// PERTURBED test
println 'Swapping partners now ...'
def temp = matches[abi]
matches[abi] = matches[bea]
matches[bea] = temp
matches.each { w, m ->
    println "${w} (his '${mansWomanRanking[m][w]}' girl) is engaged to ${m} (her '${womansManRanking[w][m]}' guy)"
}
println ''

assert ! isStable(matches, mansWomanRanking, womansManRanking)
```

{{out}}

```txt
abi (his '10' girl) is engaged to jon (her '8' guy)
bea (his '10' girl) is engaged to fred (her '7' guy)
cath (his '10' girl) is engaged to bob (her '9' guy)
dee (his '7' girl) is engaged to col (her '8' guy)
eve (his '9' girl) is engaged to hal (her '9' guy)
fay (his '9' girl) is engaged to dan (her '5' guy)
gay (his '10' girl) is engaged to gav (her '9' guy)
hope (his '10' girl) is engaged to ian (her '6' guy)
ivy (his '7' girl) is engaged to abe (her '4' guy)
jan (his '10' girl) is engaged to ed (her '10' guy)

Swapping partners now ...
abi (his '9' girl) is engaged to fred (her '9' guy)
bea (his '5' girl) is engaged to jon (her '2' guy)
cath (his '10' girl) is engaged to bob (her '9' guy)
dee (his '7' girl) is engaged to col (her '8' guy)
eve (his '9' girl) is engaged to hal (her '9' guy)
fay (his '9' girl) is engaged to dan (her '5' guy)
gay (his '10' girl) is engaged to gav (her '9' guy)
hope (his '10' girl) is engaged to ian (her '6' guy)
ivy (his '7' girl) is engaged to abe (her '4' guy)
jan (his '10' girl) is engaged to ed (her '10' guy)

O. M. G. ... bea likes fred better than jon, and fred likes bea better than abi!
                            I am TOTALLY 'shipping fred and bea now!
O. M. G. ... fred likes bea better than abi, and bea likes fred better than jon!
                            I am TOTALLY 'shipping bea and fred now!
O. M. G. ... jon likes eve better than bea, and eve likes jon better than hal!
                            I am TOTALLY 'shipping eve and jon now!
O. M. G. ... jon likes fay better than bea, and fay likes jon better than dan!
                            I am TOTALLY 'shipping fay and jon now!
O. M. G. ... jon likes gay better than bea, and gay likes jon better than gav!
                            I am TOTALLY 'shipping gay and jon now!

```



## Haskell



###  The solution


The Gale/Shapley algorithm is formulated via iterative changing of the state. In Haskell it is possible to implement this approach by pure function iterations.

The state here consists of the list of free guys and associative preferences lists for guys and girls correspondingly. In order to simplify the access to elements of the state we use lenses.


```haskell
{-# LANGUAGE TemplateHaskell #-}
import Lens.Micro
import Lens.Micro.TH
import Data.List (union, delete)

type Preferences a = (a, [a])
type Couple a = (a,a)
data State a = State { _freeGuys :: [a]
                     , _guys :: [Preferences a]
                     , _girls :: [Preferences a]}

makeLenses ''State
```


Lenses allow us to get access to each person in the state, and even to the associated preference list:


```Haskell
name n = lens get set
  where get = head . dropWhile ((/= n).fst)
        set assoc (_,v) = let (prev, _:post) = break ((== n).fst) assoc
                      in prev ++ (n, v):post

fianceesOf n = guys.name n._2
fiancesOf n = girls.name n._2
```


Note that in following we use lens operators:

  ^.  -- access to a field
  %~  -- modification of a field
  .~  -- setting a field the value

Further we use a trick: guys list girls in a descending order of preference (the most liked is the first), while girls expect guys in opposite order -- the most liked is the last. In any case, we assume that the current best choice for guys and for girls is expected to appear on the top of their preference lists.

With these tools and notes we are ready to implement the Gale/Shapley algorithm and the stability test as they are given in a textbook:


```Haskell>stableMatching :: Eq a =
 State a -> [Couple a]
stableMatching = getPairs . iterateUntil (null._freeGuys) step
  where
    iterateUntil p f = head . dropWhile (not . p) . iterate f
    getPairs s = map (_2 %~ head) $ s^.guys

step :: Eq a => State a -> State a
step s = foldl propose s (s^.freeGuys)
  where
    propose s guy =
      let girl                = s^.fianceesOf guy & head
          bestGuy : otherGuys = s^.fiancesOf girl
          modify
            | guy == bestGuy       = freeGuys %~ delete guy
            | guy `elem` otherGuys = (fiancesOf girl %~ dropWhile (/= guy)) .
                                     (freeGuys %~ guy `replaceBy` bestGuy)
            | otherwise            = fianceesOf guy %~ tail
      in modify s

    replaceBy x y [] = []
    replaceBy x y (h:t) | h == x = y:t
                        | otherwise = h:replaceBy x y t

unstablePairs :: Eq a => State a -> [Couple a] -> [(Couple a, Couple a)]
unstablePairs s pairs =
  [ ((m1, w1), (m2,w2)) | (m1, w1) <- pairs
                        , (m2,w2) <- pairs
                        , m1 /= m2
                        , let fm = s^.fianceesOf m1
                        , elemIndex w2 fm < elemIndex w1 fm
                        , let fw = s^.fiancesOf w2
                        , elemIndex m2 fw < elemIndex m1 fw ]
```


This solution works not only for strings, but for any equable data.


###  The task


Here are the given preferences:


```Haskell
guys0 =
  [("abe", ["abi", "eve", "cath", "ivy", "jan", "dee", "fay", "bea", "hope", "gay"]),
   ("bob", ["cath", "hope", "abi", "dee", "eve", "fay", "bea", "jan", "ivy", "gay"]),
   ("col", ["hope", "eve", "abi", "dee", "bea", "fay", "ivy", "gay", "cath", "jan"]),
   ("dan", ["ivy", "fay", "dee", "gay", "hope", "eve", "jan", "bea", "cath", "abi"]),
   ("ed",  ["jan", "dee", "bea", "cath", "fay", "eve", "abi", "ivy", "hope", "gay"]),
   ("fred",["bea", "abi", "dee", "gay", "eve", "ivy", "cath", "jan", "hope", "fay"]),
   ("gav", ["gay", "eve", "ivy", "bea", "cath", "abi", "dee", "hope", "jan", "fay"]),
   ("hal", ["abi", "eve", "hope", "fay", "ivy", "cath", "jan", "bea", "gay", "dee"]),
   ("ian", ["hope", "cath", "dee", "gay", "bea", "abi", "fay", "ivy", "jan", "eve"]),
   ("jon", ["abi", "fay", "jan", "gay", "eve", "bea", "dee", "cath", "ivy", "hope"])]

girls0 =
  [("abi",  ["bob", "fred", "jon", "gav", "ian", "abe", "dan", "ed", "col", "hal"]),
   ("bea",  ["bob", "abe", "col", "fred", "gav", "dan", "ian", "ed", "jon", "hal"]),
   ("cath", ["fred", "bob", "ed", "gav", "hal", "col", "ian", "abe", "dan", "jon"]),
   ("dee",  ["fred", "jon", "col", "abe", "ian", "hal", "gav", "dan", "bob", "ed"]),
   ("eve",  ["jon", "hal", "fred", "dan", "abe", "gav", "col", "ed", "ian", "bob"]),
   ("fay",  ["bob", "abe", "ed", "ian", "jon", "dan", "fred", "gav", "col", "hal"]),
   ("gay",  ["jon", "gav", "hal", "fred", "bob", "abe", "col", "ed", "dan", "ian"]),
   ("hope", ["gav", "jon", "bob", "abe", "ian", "dan", "hal", "ed", "col", "fred"]),
   ("ivy",  ["ian", "col", "hal", "gav", "fred", "bob", "abe", "ed", "jon", "dan"]),
   ("jan",  ["ed", "hal", "gav", "abe", "bob", "jon", "col", "ian", "fred", "dan"])]
```


The initial state:


```Haskell
s0 = State (fst <$> guys0) guys0 ((_2 %~ reverse) <$> girls0)
```


And the solution:

```txt
λ> let pairs = stableMatching s0
λ> mapM_ print pairs
("abe","ivy")
("bob","cath")
("col","dee")
("dan","fay")
("ed","jan")
("fred","bea")
("gav","gay")
("hal","eve")
("ian","hope")
("jon","abi")
λ> unstablePairs s0 pairs
[]
```


Lets' make some perturbations: swap fiancees of abe and bob:


```txt
λ> let fiance n = name n._2
λ> let pairs' = pairs & (fiance "abe" .~ "cath") & (fiance "bob" .~ "ivy")
λ> mapM_ print $ unstablePairs s0 pairs'
(("bob","ivy"),("abe","cath"))
(("bob","ivy"),("dan","fay"))
(("bob","ivy"),("fred","bea"))
(("bob","ivy"),("ian","hope"))
(("bob","ivy"),("jon","abi"))
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link printf

procedure main()
   smd := IsStable(ShowEngaged(StableMatching(setup())))
   IsStable(ShowEngaged(Swap(\smd,smd.women[1],smd.women[2])))
end

procedure index(L,x)                         #: return index of value or fail
   return ( L[i := 1 to *L] === x, i)
end

procedure ShowEngaged(smd)                   #: Show who's hooked up
   printf("\nEngagements:\n")
   every w := !smd.women do
      printf("%s is engaged to %s\n",w,smd.engaged[w])
   return smd
end

procedure Swap(smd,x0,x1)                    #: swap two couples by m or w
   printf("\nSwapping %s and %s\n",x0,x1)
   e := smd.engaged
   e[x0] :=: e[x1]                           # swap partners
   e[e[x0]] := e[e[x1]]
   return smd
end

procedure IsStable(smd)                      #: validate stability
   stable := 1                                               # assumption
   printf("\n")
   every mp := smd.prefs[m := !smd.men] &                    # man & pref
         w := mp[index(mp,smd.engaged[m])-1 to 1 by -1] do { # better choices
      wp := smd.prefs[w]                                     # her choices
      if index(wp,smd.engaged[w]) > index(wp,m) then {
         printf("Engagement of %s to %s is unstable.\n",w,m)
         stable := &null                                     # broken
         }
      }
   if \stable then {
      printf("Engagments are all stable.\n")
      return smd
      }
end

procedure StableMatching(smd)                #: match making
   freemen   := copy(smd.men)                # Initialize all m memberof M
   freewomen := set(smd.women)               # ... and w memberof W to free
   every (prefmen := table())[m := !freemen] := copy(smd.prefs[m])
   smd.engaged := engaged := table()
   printf("\nMatching:\n")
   while m := get(freemen) do {                 # next freeman
      while w := get(prefmen[m]) do  {          # . with prpoposals left
         if member(freewomen,w) then {          # . . is she free?
            engaged[m] := w                     # . . . (m, w)
            engaged[w] := m
            delete(freewomen,w)
            printf("%s accepted %s's proposal\n",w,m)
            break
            }
         else {                                 # . . no, she's engaged
            m0 := engaged[w]                    #     to m0
            if index(smd.prefs[w],m) < index(smd.prefs[w],m0) then {
               engaged[m] := w                  # (m, w) become engaged
               engaged[w] := m
               delete(freewomen,w)
               engaged[m0] := &null             # m' becomes free
               put(freemen,m0)
               printf("%s chose %s over %s\n",w,m,m0)
               break
               }
            else next                          # she's happier as is
         }
      }
   }
   return smd
end

record sm_data(men,women,prefs,engaged)  #: everyones data

procedure setup()                        #: setup everyones data
   X := sm_data()
   X.men   := ["abe","bob","col","dan","ed","fred","gav","hal","ian","jon"]
   X.women := ["abi","bea","cath","dee","eve","fay","gay","hope","ivy","jan"]

   if *X.men ~= *(M := set(X.men)) then runerr(500,X.men)       # duplicate?
   if *X.women ~= *(W := set(X.women)) then runerr(500,X.women) # duplicate?
   if *(B := M**W) ~= 0 then runerr(500,B)                      # intersect?

   X.prefs := p := table()

   p["abe"]  := ["abi","eve","cath","ivy","jan","dee","fay","bea","hope","gay"]
   p["bob"]  := ["cath","hope","abi","dee","eve","fay","bea","jan","ivy","gay"]
   p["col"]  := ["hope","eve","abi","dee","bea","fay","ivy","gay","cath","jan"]
   p["dan"]  := ["ivy","fay","dee","gay","hope","eve","jan","bea","cath","abi"]
   p["ed"]   := ["jan","dee","bea","cath","fay","eve","abi","ivy","hope","gay"]
   p["fred"] := ["bea","abi","dee","gay","eve","ivy","cath","jan","hope","fay"]
   p["gav"]  := ["gay","eve","ivy","bea","cath","abi","dee","hope","jan","fay"]
   p["hal"]  := ["abi","eve","hope","fay","ivy","cath","jan","bea","gay","dee"]
   p["ian"]  := ["hope","cath","dee","gay","bea","abi","fay","ivy","jan","eve"]
   p["jon"]  := ["abi","fay","jan","gay","eve","bea","dee","cath","ivy","hope"]

   p["abi"]  := ["bob","fred","jon","gav","ian","abe","dan","ed","col","hal"]
   p["bea"]  := ["bob","abe","col","fred","gav","dan","ian","ed","jon","hal"]
   p["cath"] := ["fred","bob","ed","gav","hal","col","ian","abe","dan","jon"]
   p["dee"]  := ["fred","jon","col","abe","ian","hal","gav","dan","bob","ed"]
   p["eve"]  := ["jon","hal","fred","dan","abe","gav","col","ed","ian","bob"]
   p["fay"]  := ["bob","abe","ed","ian","jon","dan","fred","gav","col","hal"]
   p["gay"]  := ["jon","gav","hal","fred","bob","abe","col","ed","dan","ian"]
   p["hope"] := ["gav","jon","bob","abe","ian","dan","hal","ed","col","fred"]
   p["ivy"]  := ["ian","col","hal","gav","fred","bob","abe","ed","jon","dan"]
   p["jan"]  := ["ed","hal","gav","abe","bob","jon","col","ian","fred","dan"]

   return X
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides formatting]
{{out}}

```txt

Matching:
abi accepted abe's proposal
cath accepted bob's proposal
hope accepted col's proposal
ivy accepted dan's proposal
jan accepted ed's proposal
bea accepted fred's proposal
gay accepted gav's proposal
eve accepted hal's proposal
hope chose ian over col
abi chose jon over abe
dee accepted col's proposal
ivy chose abe over dan
fay accepted dan's proposal

Engagements:
abi is engaged to jon
bea is engaged to fred
cath is engaged to bob
dee is engaged to col
eve is engaged to hal
fay is engaged to dan
gay is engaged to gav
hope is engaged to ian
ivy is engaged to abe
jan is engaged to ed

Engagments are all stable.

Swapping abi and bea

Engagements:
abi is engaged to fred
bea is engaged to jon
cath is engaged to bob
dee is engaged to col
eve is engaged to hal
fay is engaged to dan
gay is engaged to gav
hope is engaged to ian
ivy is engaged to abe
jan is engaged to ed

Engagement of bea to fred is unstable.
```



## J


```j
Mraw=: ;: ;._2 noun define -. ':,'
  abe: abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay
  bob: cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay
  col: hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan
  dan: ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi
   ed: jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay
 fred: bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay
  gav: gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay
  hal: abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee
  ian: hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve
  jon: abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope
)

Fraw=: ;: ;._2 noun define -. ':,'
  abi: bob, fred, jon, gav, ian, abe, dan, ed, col, hal
  bea: bob, abe, col, fred, gav, dan, ian, ed, jon, hal
 cath: fred, bob, ed, gav, hal, col, ian, abe, dan, jon
  dee: fred, jon, col, abe, ian, hal, gav, dan, bob, ed
  eve: jon, hal, fred, dan, abe, gav, col, ed, ian, bob
  fay: bob, abe, ed, ian, jon, dan, fred, gav, col, hal
  gay: jon, gav, hal, fred, bob, abe, col, ed, dan, ian
 hope: gav, jon, bob, abe, ian, dan, hal, ed, col, fred
  ivy: ian, col, hal, gav, fred, bob, abe, ed, jon, dan
  jan: ed, hal, gav, abe, bob, jon, col, ian, fred, dan
)

GuyNames=: {."1 Mraw
GalNames=: {."1 Fraw

Mprefs=: GalNames i. }."1 Mraw
Fprefs=: GuyNames i. }."1 Fraw

propose=: dyad define
  engaged=. x
  'guy gal'=. y
  if. gal e. engaged do.
    fiance=. engaged i. gal
    if. guy <&((gal{Fprefs)&i.) fiance do.
      engaged=. gal guy} _ fiance} engaged
    end.
  else.
    engaged=. gal guy} engaged
  end.
  engaged
)

matchMake=: monad define
  engaged=. _"0 GuyNames NB. initially no one is engaged
  fallback=. 0"0 engaged NB. and each guy will first propose to his favorite
  whilst. _ e. engaged do.
    for_guy. I. _ = engaged do.
      next=. guy{fallback
      gal=. (<guy,next){Mprefs
      engaged=. engaged propose guy,gal
      fallback=. (next+1) guy} fallback
    end.
  end.
  GuyNames,:engaged{GalNames
)

checkStable=: monad define
  'guys gals'=. (GuyNames,:GalNames) i."1 y
  satisfied=. ] >: (<0 1) |: ]
  guyshappy=. satisfied (guys{Mprefs) i."1 0/ gals
  galshappy=. satisfied (gals{Fprefs) i."1 0/ guys
  unstable=. 4$.$.-. guyshappy +. |:galshappy
  if. bad=. 0 < #unstable do.
    smoutput 'Engagements preferred by both members to their current ones:'
    smoutput y {~"1 0"2 1 unstable
  end.
  assert-.bad
)
```


For most of this, males and females are both represented by indices.  Rows of <code>Mprefs</code> are indexed by a male index and each contains a list female indices, in priority order.  Rows of <code>Fprefs</code> are indexed by a female index and each contains a list male indices in priority order.  These indices select the corresponding names from <code>GuyNames</code> and <code>GalNames</code>.

In <code>matchMake</code> (and <code>propose</code>), <code>engaged</code> identifies the gal each guy is engaged to (or <code>_</code> if that guy is not engaged).  And, <code>fallback</code> identifies the column which has the next gal, in <code>Mprefs</code>, for that guy to propose to.

Example use:


```j
   matchMake ''
┌───┬────┬───┬───┬───┬────┬───┬───┬────┬───┐
│abe│bob │col│dan│ed │fred│gav│hal│ian │jon│
├───┼────┼───┼───┼───┼────┼───┼───┼────┼───┤
│ivy│cath│dee│fay│jan│bea │gay│eve│hope│abi│
└───┴────┴───┴───┴───┴────┴───┴───┴────┴───┘
```


Stability check:


```j
   checkStable matchMake''

```


(no news is good news)

An altered result, and a stability check on it (showing what would happen for a bogus result):


```j
   0 105 A."_1 matchMake ''  NB. swap abi and bea
┌───┬────┬───┬───┬───┬────┬───┬───┬────┬───┐
│abe│bob │col│dan│ed │fred│gav│hal│ian │jon│
├───┼────┼───┼───┼───┼────┼───┼───┼────┼───┤
│ivy│cath│dee│fay│jan│abi │gay│eve│hope│bea│
└───┴────┴───┴───┴───┴────┴───┴───┴────┴───┘
   checkStable 0 105 A."_1 matchMake ''
Engagements preferred by both members to their current ones:
┌────┬───┐
│fred│bea│
├────┼───┤
│jon │fay│
├────┼───┤
│jon │gay│
├────┼───┤
│jon │eve│
└────┴───┘
|assertion failure: assert
|       assert-.bad
```

As an aside, note that the guys fared much better than the gals here, with over half of the guys getting their first preference and only one gal getting her first preference.  The worst match for any guy was fourth preference where the worst for any gal was seventh preference.


## Java

This is not a direct translation of [[#Python|Python]], but it's fairly close (especially the stability check).

```java5
import java.util.*;

public class Stable {
    static List<String> guys = Arrays.asList(
            new String[]{
        "abe", "bob", "col", "dan", "ed", "fred", "gav", "hal", "ian", "jon"});
    static List<String> girls = Arrays.asList(
            new String[]{
        "abi", "bea", "cath", "dee", "eve", "fay", "gay", "hope", "ivy", "jan"});
    static Map<String, List<String>> guyPrefers =
            new HashMap<String, List<String>>(){{
        put("abe",
            Arrays.asList("abi", "eve", "cath", "ivy", "jan", "dee", "fay",
            "bea", "hope", "gay"));
        put("bob",
            Arrays.asList("cath", "hope", "abi", "dee", "eve", "fay", "bea",
            "jan", "ivy", "gay"));
        put("col",
            Arrays.asList("hope", "eve", "abi", "dee", "bea", "fay", "ivy",
            "gay", "cath", "jan"));
        put("dan",
            Arrays.asList("ivy", "fay", "dee", "gay", "hope", "eve", "jan",
            "bea", "cath", "abi"));
        put("ed",
            Arrays.asList("jan", "dee", "bea", "cath", "fay", "eve", "abi",
            "ivy", "hope", "gay"));
        put("fred",
            Arrays.asList("bea", "abi", "dee", "gay", "eve", "ivy", "cath",
            "jan", "hope", "fay"));
        put("gav",
            Arrays.asList("gay", "eve", "ivy", "bea", "cath", "abi", "dee",
            "hope", "jan", "fay"));
        put("hal",
            Arrays.asList("abi", "eve", "hope", "fay", "ivy", "cath", "jan",
            "bea", "gay", "dee"));
        put("ian",
            Arrays.asList("hope", "cath", "dee", "gay", "bea", "abi", "fay",
            "ivy", "jan", "eve"));
        put("jon",
            Arrays.asList("abi", "fay", "jan", "gay", "eve", "bea", "dee",
            "cath", "ivy", "hope"));
    }};
    static Map<String, List<String>> girlPrefers =
            new HashMap<String, List<String>>(){{
        put("abi",
            Arrays.asList("bob", "fred", "jon", "gav", "ian", "abe", "dan",
            "ed", "col", "hal"));
        put("bea",
            Arrays.asList("bob", "abe", "col", "fred", "gav", "dan", "ian",
            "ed", "jon", "hal"));
        put("cath",
            Arrays.asList("fred", "bob", "ed", "gav", "hal", "col", "ian",
            "abe", "dan", "jon"));
        put("dee",
            Arrays.asList("fred", "jon", "col", "abe", "ian", "hal", "gav",
            "dan", "bob", "ed"));
        put("eve",
            Arrays.asList("jon", "hal", "fred", "dan", "abe", "gav", "col",
            "ed", "ian", "bob"));
        put("fay",
            Arrays.asList("bob", "abe", "ed", "ian", "jon", "dan", "fred",
            "gav", "col", "hal"));
        put("gay",
            Arrays.asList("jon", "gav", "hal", "fred", "bob", "abe", "col",
            "ed", "dan", "ian"));
        put("hope",
            Arrays.asList("gav", "jon", "bob", "abe", "ian", "dan", "hal",
            "ed", "col", "fred"));
        put("ivy",
            Arrays.asList("ian", "col", "hal", "gav", "fred", "bob", "abe",
            "ed", "jon", "dan"));
        put("jan",
            Arrays.asList("ed", "hal", "gav", "abe", "bob", "jon", "col",
            "ian", "fred", "dan"));
    }};
    public static void main(String[] args){
        Map<String, String> matches = match(guys, guyPrefers, girlPrefers);
        for(Map.Entry<String, String> couple:matches.entrySet()){
            System.out.println(
                    couple.getKey() + " is engaged to " + couple.getValue());
        }
        if(checkMatches(guys, girls, matches, guyPrefers, girlPrefers)){
            System.out.println("Marriages are stable");
        }else{
            System.out.println("Marriages are unstable");
        }
        String tmp = matches.get(girls.get(0));
        matches.put(girls.get(0), matches.get(girls.get(1)));
        matches.put(girls.get(1), tmp);
        System.out.println(
                girls.get(0) +" and " + girls.get(1) + " have switched partners");
        if(checkMatches(guys, girls, matches, guyPrefers, girlPrefers)){
            System.out.println("Marriages are stable");
        }else{
            System.out.println("Marriages are unstable");
        }
    }

    private static Map<String, String> match(List<String> guys,
            Map<String, List<String>> guyPrefers,
            Map<String, List<String>> girlPrefers){
        Map<String, String> engagedTo = new TreeMap<String, String>();
        List<String> freeGuys = new LinkedList<String>();
        freeGuys.addAll(guys);
        while(!freeGuys.isEmpty()){
            String thisGuy = freeGuys.remove(0); //get a load of THIS guy
            List<String> thisGuyPrefers = guyPrefers.get(thisGuy);
            for(String girl:thisGuyPrefers){
                if(engagedTo.get(girl) == null){//girl is free
                    engagedTo.put(girl, thisGuy); //awww
                    break;
                }else{
                    String otherGuy = engagedTo.get(girl);
                    List<String> thisGirlPrefers = girlPrefers.get(girl);
                    if(thisGirlPrefers.indexOf(thisGuy) <
                            thisGirlPrefers.indexOf(otherGuy)){
                        //this girl prefers this guy to the guy she's engaged to
                        engagedTo.put(girl, thisGuy);
                        freeGuys.add(otherGuy);
                        break;
                    }//else no change...keep looking for this guy
                }
            }
        }
        return engagedTo;
    }

    private static boolean checkMatches(List<String> guys, List<String> girls,
            Map<String, String> matches, Map<String, List<String>> guyPrefers,
            Map<String, List<String>> girlPrefers) {
        if(!matches.keySet().containsAll(girls)){
            return false;
        }

        if(!matches.values().containsAll(guys)){
            return false;
        }

        Map<String, String> invertedMatches = new TreeMap<String, String>();
        for(Map.Entry<String, String> couple:matches.entrySet()){
            invertedMatches.put(couple.getValue(), couple.getKey());
        }

        for(Map.Entry<String, String> couple:matches.entrySet()){
            List<String> shePrefers = girlPrefers.get(couple.getKey());
            List<String> sheLikesBetter = new LinkedList<String>();
            sheLikesBetter.addAll(shePrefers.subList(0, shePrefers.indexOf(couple.getValue())));
            List<String> hePrefers = guyPrefers.get(couple.getValue());
            List<String> heLikesBetter = new LinkedList<String>();
            heLikesBetter.addAll(hePrefers.subList(0, hePrefers.indexOf(couple.getKey())));

            for(String guy : sheLikesBetter){
                String guysFinace = invertedMatches.get(guy);
                List<String> thisGuyPrefers = guyPrefers.get(guy);
                if(thisGuyPrefers.indexOf(guysFinace) >
                        thisGuyPrefers.indexOf(couple.getKey())){
                    System.out.printf("%s likes %s better than %s and %s"
                            + " likes %s better than their current partner\n",
                       couple.getKey(), guy, couple.getValue(),
                       guy, couple.getKey());
                    return false;
                }
            }

            for(String girl : heLikesBetter){
                String girlsFinace = matches.get(girl);
                List<String> thisGirlPrefers = girlPrefers.get(girl);
                if(thisGirlPrefers.indexOf(girlsFinace) >
                        thisGirlPrefers.indexOf(couple.getValue())){
                    System.out.printf("%s likes %s better than %s and %s"
                            + " likes %s better than their current partner\n",
                       couple.getValue(), girl, couple.getKey(),
                       girl, couple.getValue());
                    return false;
                }
            }
        }
        return true;
    }
}
```

{{out}}

```txt
abi is engaged to jon
bea is engaged to fred
cath is engaged to bob
dee is engaged to col
eve is engaged to hal
fay is engaged to dan
gay is engaged to gav
hope is engaged to ian
ivy is engaged to abe
jan is engaged to ed
Marriages are stable
abi and bea have switched partners
fred likes bea better than abi and bea likes fred better than their current partner
Marriages are unstable
```



## JavaScript


```javascript
function Person(name) {

    var candidateIndex = 0;

    this.name = name;
    this.fiance = null;
    this.candidates = [];

    this.rank = function(p) {
        for (i = 0; i < this.candidates.length; i++)
            if (this.candidates[i] === p) return i;
        return this.candidates.length + 1;
    }

    this.prefers = function(p) {
        return this.rank(p) < this.rank(this.fiance);
    }

    this.nextCandidate = function() {
        if (candidateIndex >= this.candidates.length) return null;
        return this.candidates[candidateIndex++];
    }

    this.engageTo = function(p) {
        if (p.fiance) p.fiance.fiance = null;
        p.fiance = this;
        if (this.fiance) this.fiance.fiance = null;
        this.fiance = p;
    }

    this.swapWith = function(p) {
        console.log("%s & %s swap partners", this.name, p.name);
        var thisFiance = this.fiance;
        var pFiance = p.fiance;
        this.engageTo(pFiance);
        p.engageTo(thisFiance);
    }
}

function isStable(guys, gals) {
    for (var i = 0; i < guys.length; i++)
        for (var j = 0; j < gals.length; j++)
            if (guys[i].prefers(gals[j]) && gals[j].prefers(guys[i]))
                return false;
    return true;
}

function engageEveryone(guys) {
    var done;
    do {
        done = true;
        for (var i = 0; i < guys.length; i++) {
            var guy = guys[i];
            if (!guy.fiance) {
                done = false;
                var gal = guy.nextCandidate();
                if (!gal.fiance || gal.prefers(guy))
                    guy.engageTo(gal);
            }
        }
    } while (!done);
}

function doMarriage() {

    var abe  = new Person("Abe");
    var bob  = new Person("Bob");
    var col  = new Person("Col");
    var dan  = new Person("Dan");
    var ed   = new Person("Ed");
    var fred = new Person("Fred");
    var gav  = new Person("Gav");
    var hal  = new Person("Hal");
    var ian  = new Person("Ian");
    var jon  = new Person("Jon");
    var abi  = new Person("Abi");
    var bea  = new Person("Bea");
    var cath = new Person("Cath");
    var dee  = new Person("Dee");
    var eve  = new Person("Eve");
    var fay  = new Person("Fay");
    var gay  = new Person("Gay");
    var hope = new Person("Hope");
    var ivy  = new Person("Ivy");
    var jan  = new Person("Jan");

    abe.candidates  = [abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay];
    bob.candidates  = [cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay];
    col.candidates  = [hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan];
    dan.candidates  = [ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi];
    ed.candidates   = [jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay];
    fred.candidates = [bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay];
    gav.candidates  = [gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay];
    hal.candidates  = [abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee];
    ian.candidates  = [hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve];
    jon.candidates  = [abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope];
    abi.candidates  = [bob, fred, jon, gav, ian, abe, dan, ed, col, hal];
    bea.candidates  = [bob, abe, col, fred, gav, dan, ian, ed, jon, hal];
    cath.candidates = [fred, bob, ed, gav, hal, col, ian, abe, dan, jon];
    dee.candidates  = [fred, jon, col, abe, ian, hal, gav, dan, bob, ed];
    eve.candidates  = [jon, hal, fred, dan, abe, gav, col, ed, ian, bob];
    fay.candidates  = [bob, abe, ed, ian, jon, dan, fred, gav, col, hal];
    gay.candidates  = [jon, gav, hal, fred, bob, abe, col, ed, dan, ian];
    hope.candidates = [gav, jon, bob, abe, ian, dan, hal, ed, col, fred];
    ivy.candidates  = [ian, col, hal, gav, fred, bob, abe, ed, jon, dan];
    jan.candidates  = [ed, hal, gav, abe, bob, jon, col, ian, fred, dan];

    var guys = [abe, bob, col, dan, ed, fred, gav, hal, ian, jon];
    var gals = [abi, bea, cath, dee, eve, fay, gay, hope, ivy, jan];

    engageEveryone(guys);

    for (var i = 0; i < guys.length; i++) {
        console.log("%s is engaged to %s", guys[i].name, guys[i].fiance.name);
    }
    console.log("Stable = %s", isStable(guys, gals) ? "Yes" : "No");
    jon.swapWith(fred);
    console.log("Stable = %s", isStable(guys, gals) ? "Yes" : "No");
}

doMarriage();

```


{{out}}

```txt
Abe is engaged to Ivy
Bob is engaged to Cath
Col is engaged to Dee
Dan is engaged to Fay
Ed is engaged to Jan
Fred is engaged to Bea
Gav is engaged to Gay
Hal is engaged to Eve
Ian is engaged to Hope
Jon is engaged to Abi
Stable = Yes
Jon & Fred swap partners
Stable = No
```



## Julia


```Julia

# This is not optimized, but tries to follow the pseudocode given the Wikipedia entry below.
# Reference: https://en.wikipedia.org/wiki/Stable_marriage_problem#Algorithm

const males = ["abe", "bob", "col", "dan", "ed", "fred", "gav", "hal", "ian", "jon"]
const females = ["abi", "bea", "cath", "dee", "eve", "fay", "gay", "hope", "ivy", "jan"]

const malepreferences = Dict(
  "abe" => ["abi", "eve", "cath", "ivy", "jan", "dee", "fay", "bea", "hope", "gay"],
  "bob" => ["cath", "hope", "abi", "dee", "eve", "fay", "bea", "jan", "ivy", "gay"],
  "col" => ["hope", "eve", "abi", "dee", "bea", "fay", "ivy", "gay", "cath", "jan"],
  "dan" => ["ivy", "fay", "dee", "gay", "hope", "eve", "jan", "bea", "cath", "abi"],
   "ed" => ["jan", "dee", "bea", "cath", "fay", "eve", "abi", "ivy", "hope", "gay"],
 "fred" => ["bea", "abi", "dee", "gay", "eve", "ivy", "cath", "jan", "hope", "fay"],
  "gav" => ["gay", "eve", "ivy", "bea", "cath", "abi", "dee", "hope", "jan", "fay"],
  "hal" => ["abi", "eve", "hope", "fay", "ivy", "cath", "jan", "bea", "gay", "dee"],
  "ian" => ["hope", "cath", "dee", "gay", "bea", "abi", "fay", "ivy", "jan", "eve"],
  "jon" => ["abi", "fay", "jan", "gay", "eve", "bea", "dee", "cath", "ivy", "hope"]
)

const femalepreferences = Dict(
  "abi"=> ["bob", "fred", "jon", "gav", "ian", "abe", "dan", "ed", "col", "hal"],
  "bea"=> ["bob", "abe", "col", "fred", "gav", "dan", "ian", "ed", "jon", "hal"],
 "cath"=> ["fred", "bob", "ed", "gav", "hal", "col", "ian", "abe", "dan", "jon"],
  "dee"=> ["fred", "jon", "col", "abe", "ian", "hal", "gav", "dan", "bob", "ed"],
  "eve"=> ["jon", "hal", "fred", "dan", "abe", "gav", "col", "ed", "ian", "bob"],
  "fay"=> ["bob", "abe", "ed", "ian", "jon", "dan", "fred", "gav", "col", "hal"],
  "gay"=> ["jon", "gav", "hal", "fred", "bob", "abe", "col", "ed", "dan", "ian"],
 "hope"=> ["gav", "jon", "bob", "abe", "ian", "dan", "hal", "ed", "col", "fred"],
  "ivy"=> ["ian", "col", "hal", "gav", "fred", "bob", "abe", "ed", "jon", "dan"],
  "jan"=> ["ed", "hal", "gav", "abe", "bob", "jon", "col", "ian", "fred", "dan"]
)

function pshuf(d)
    ret = Dict()
    for (k,v) in d
        ret[k] = shuffle(v)
    end
    ret
end

# helper functions for the verb: p1 "prefers" p2 over p3
pindexin(a, p) = ([i for i in 1:length(a) if a[i] == p])[1]
prefers(d, p1, p2, p3) = (pindexin(d[p1], p2) < pindexin(d[p1], p3))

function isstable(mmatchup, fmatchup, mpref, fpref)
    for (mmatch, fmatch) in mmatchup
        for f in mpref[mmatch]
            if(f != fmatch && prefers(mpref, mmatch, f, fmatch)
                           && prefers(fpref, f, mmatch, fmatchup[f]))
                println("$mmatch prefers $f and $f prefers $mmatch over their current partners.")
                return false
            end
        end
    end
    true
end

function galeshapley(men, women, malepref, femalepref)
    # Initialize all m ∈ M and w ∈ W to free
    mfree = Dict([(p, true) for p in men])
    wfree = Dict([(p, true) for p in women])
    mpairs = Dict()
    wpairs = Dict()
    while true                    # while ∃ free man m who still has a woman w to propose to
        bachelors = [p for p in keys(mfree) if mfree[p]]
        if(length(bachelors) == 0)
            return mpairs, wpairs
        end
        for m in bachelors
            for w in malepref[m]  # w = first woman on m’s list to whom m has not yet proposed
                if(wfree[w])      # if w is free (else some pair (m', w) already exists)
                    #println("Free match: $m, $w")
                    mpairs[m] = w # (m, w) become engaged
                    wpairs[w] = m # double entry bookeeping
                    mfree[m] = false
                    wfree[w] = false
                    break
                elseif(prefers(femalepref, w, m, wpairs[w])) # if w prefers m to m'
                    #println("Unmatch $(wpairs[w]), match: $m, $w")
                    mfree[wpairs[w]] = true # m' becomes free
                    mpairs[m] = w           # (m, w) become engaged
                    wpairs[w] = m
                    mfree[m] = false
                    break
                end                         # else (m', w) remain engaged, so continue
            end
        end
    end
end

function tableprint(txt, ans, stab)
    println(txt)
    println("   Man     Woman")
    println("   -----   -----")
    show(STDOUT, "text/plain", ans)
    if(stab)
        println("\n  ----STABLE----\n\n")
    else
        println("\n  ---UNSTABLE---\n\n")
    end
end

println("Use the Gale Shapley algorithm to find a stable set of engagements.")
answer = galeshapley(males, females, malepreferences, femalepreferences)
stabl = isstable(answer[1], answer[2], malepreferences, femalepreferences)
tableprint("Original Data Table", answer[1], stabl)

println("To check this is not a one-off solution, run the function on a randomized sample.")
newmpref = pshuf(malepreferences)
newfpref = pshuf(femalepreferences)
answer = galeshapley(males, females, newmpref, newfpref)
stabl = isstable(answer[1], answer[2], newmpref, newfpref)
tableprint("Shuffled Preferences", answer[1], stabl)

# trade abe with bob
println("Perturb this set of engagements to form an unstable set of engagements then check this new set for stability.")
answer = galeshapley(males, females, malepreferences, femalepreferences)
fia1 = (answer[1])["abe"]
fia2 = (answer[1])["bob"]
answer[1]["abe"] = fia2
answer[1]["bob"] = fia1
answer[2][fia1] = "bob"
answer[2][fia2] = "abe"
stabl = isstable(answer[1], answer[2], malepreferences, femalepreferences)
tableprint("Original Data With Bob and Abe Switched", answer[1], stabl)


```

{{out}}

```txt

Use the Gale Shapley algorithm to find a stable set of engagements.
Original Data Table
   Man     Woman
   -----   -----
Dict{Any,Any} with 10 entries:
  "bob" => "cath"
  "dan" => "fay"
  "fred" => "bea"
  "jon" => "abi"
  "ian" => "hope"
  "gav" => "gay"
  "ed" => "jan"
  "col" => "dee"
  "hal" => "eve"
  "abe" => "ivy"
  ----STABLE----


To check this is not a one-off solution, run the function on a randomized sample.
Shuffled Preferences
   Man     Woman
   -----   -----
Dict{Any,Any} with 10 entries:
  "bob" => "abi"
  "dan" => "bea"
  "fred" => "jan"
  "jon" => "dee"
  "ian" => "fay"
  "gav" => "ivy"
  "ed" => "gay"
  "col" => "cath"
  "hal" => "hope"
  "abe" => "eve"
  ----STABLE----


Perturb this set of engagements to form an unstable set of engagements then check this new set for stability.
bob prefers cath and cath prefers bob over their current partners.
Original Data With Bob and Abe Switched
   Man     Woman
   -----   -----
Dict{Any,Any} with 10 entries:
  "bob" => "ivy"
  "dan" => "fay"
  "fred" => "bea"
  "jon" => "abi"
  "ian" => "hope"
  "gav" => "gay"
  "ed" => "jan"
  "col" => "dee"
  "hal" => "eve"
  "abe" => "cath"
  ---UNSTABLE---

```



## Kotlin



```java

data class Person(val name: String) {
    val preferences = mutableListOf<Person>()
    var matchedTo: Person? = null

    fun trySwap(p: Person) {
        if (prefers(p)) {
            matchedTo?.matchedTo = null
            matchedTo = p
            p.matchedTo = this
        }
    }

    fun prefers(p: Person) = when (matchedTo) {
        null -> true
        else -> preferences.indexOf(p) < preferences.indexOf(matchedTo!!)
    }

    fun showMatch() = "$name <=> ${matchedTo?.name}"
}

fun match(males: Collection<Person>) {
    while (males.find { it.matchedTo == null }?.also { match(it) } != null) {
    }
}

fun match(male: Person) {
    while (male.matchedTo == null) {
        male.preferences.removeAt(0).trySwap(male)
    }
}

fun isStableMatch(males: Collection<Person>, females: Collection<Person>): Boolean {
    return males.all { isStableMatch(it, females) }
}

fun isStableMatch(male: Person, females: Collection<Person>): Boolean {

    val likesBetter = females.filter { !male.preferences.contains(it) }
    val stable = !likesBetter.any { it.prefers(male) }

    if (!stable) {
        println("#### Unstable pair: ${male.showMatch()}")
    }
    return stable
}


fun main(args: Array<String>) {
    val inMales = mapOf(
            "abe" to mutableListOf("abi", "eve", "cath", "ivy", "jan", "dee", "fay", "bea", "hope", "gay"),
            "bob" to mutableListOf("cath", "hope", "abi", "dee", "eve", "fay", "bea", "jan", "ivy", "gay"),
            "col" to mutableListOf("hope", "eve", "abi", "dee", "bea", "fay", "ivy", "gay", "cath", "jan"),
            "dan" to mutableListOf("ivy", "fay", "dee", "gay", "hope", "eve", "jan", "bea", "cath", "abi"),
            "ed" to mutableListOf("jan", "dee", "bea", "cath", "fay", "eve", "abi", "ivy", "hope", "gay"),
            "fred" to mutableListOf("bea", "abi", "dee", "gay", "eve", "ivy", "cath", "jan", "hope", "fay"),
            "gav" to mutableListOf("gay", "eve", "ivy", "bea", "cath", "abi", "dee", "hope", "jan", "fay"),
            "hal" to mutableListOf("abi", "eve", "hope", "fay", "ivy", "cath", "jan", "bea", "gay", "dee"),
            "ian" to mutableListOf("hope", "cath", "dee", "gay", "bea", "abi", "fay", "ivy", "jan", "eve"),
            "jon" to mutableListOf("abi", "fay", "jan", "gay", "eve", "bea", "dee", "cath", "ivy", "hope"))

    val inFemales = mapOf(
            "abi" to listOf("bob", "fred", "jon", "gav", "ian", "abe", "dan", "ed", "col", "hal"),
            "bea" to listOf("bob", "abe", "col", "fred", "gav", "dan", "ian", "ed", "jon", "hal"),
            "cath" to listOf("fred", "bob", "ed", "gav", "hal", "col", "ian", "abe", "dan", "jon"),
            "dee" to listOf("fred", "jon", "col", "abe", "ian", "hal", "gav", "dan", "bob", "ed"),
            "eve" to listOf("jon", "hal", "fred", "dan", "abe", "gav", "col", "ed", "ian", "bob"),
            "fay" to listOf("bob", "abe", "ed", "ian", "jon", "dan", "fred", "gav", "col", "hal"),
            "gay" to listOf("jon", "gav", "hal", "fred", "bob", "abe", "col", "ed", "dan", "ian"),
            "hope" to listOf("gav", "jon", "bob", "abe", "ian", "dan", "hal", "ed", "col", "fred"),
            "ivy" to listOf("ian", "col", "hal", "gav", "fred", "bob", "abe", "ed", "jon", "dan"),
            "jan" to listOf("ed", "hal", "gav", "abe", "bob", "jon", "col", "ian", "fred", "dan"))


    fun buildPrefs(person: Person, stringPrefs: List<String>, population: List<Person>) {
        person.preferences.addAll(
                stringPrefs.map { name -> population.single { it.name == name } }
        )
    }

    val males = inMales.keys.map { Person(it) }
    val females = inFemales.keys.map { Person(it) }

    males.forEach { buildPrefs(it, inMales[it.name]!!, females) }
    females.forEach { buildPrefs(it, inFemales[it.name]!!, males) }


    match(males)
    males.forEach { println(it.showMatch()) }
    println("#### match is stable: ${isStableMatch(males, females)}")


    fun swapMatch(male1: Person, male2: Person) {
        val female1 = male1.matchedTo!!
        val female2 = male2.matchedTo!!

        male1.matchedTo = female2
        male2.matchedTo = female1

        female1.matchedTo = male2
        female2.matchedTo = male1
    }

    swapMatch(males.single { it.name == "fred" }, males.single { it.name == "jon" })
    males.forEach { println(it.showMatch()) }
    println("#### match is stable: ${isStableMatch(males, females)}")
}

```

{{out}}

```txt

abe <=> ivy
bob <=> cath
col <=> dee
dan <=> fay
ed <=> jan
fred <=> bea
gav <=> gay
hal <=> eve
ian <=> hope
jon <=> abi
##### match is stable: true
abe <=> ivy
bob <=> cath
col <=> dee
dan <=> fay
ed <=> jan
fred <=> abi
gav <=> gay
hal <=> eve
ian <=> hope
jon <=> bea
#### Unstable pair: fred <=> abi
##### match is stable: false

```



## Lua

{{trans|C#}}


```lua
local Person = {}
Person.__index = Person

function Person.new(inName)
    local o = {
        name            = inName,
        prefs           = nil,
        fiance          = nil,
        _candidateIndex = 1,
    }
    return setmetatable(o, Person)
end

function Person:indexOf(other)
    for i, p in pairs(self.prefs) do
        if p == other then return i end
    end
    return 999
end

function Person:prefers(other)
    return self:indexOf(other) < self:indexOf(self.fiance)
end

function Person:nextCandidateNotYetProposedTo()
    if self._candidateIndex >= #self.prefs then return nil end
    local c = self.prefs[self._candidateIndex];
    self._candidateIndex = self._candidateIndex + 1
    return c;
end

function Person:engageTo(other)
    if other.fiance then
        other.fiance.fiance = nil
    end
    other.fiance = self
    if self.fiance then
        self.fiance.fiance = nil
    end
    self.fiance = other;
end

local function isStable(men)
    local women = men[1].prefs
    local stable = true
    for _, guy in pairs(men) do
        for _, gal in pairs(women) do
            if guy:prefers(gal) and gal:prefers(guy) then
                stable = false
                print(guy.name .. ' and ' .. gal.name ..
                      ' prefer each other over their partners ' ..
                      guy.fiance.name .. ' and ' .. gal.fiance.name)
            end
        end
    end
    return stable
end

local abe  = Person.new("Abe")
local bob  = Person.new("Bob")
local col  = Person.new("Col")
local dan  = Person.new("Dan")
local ed   = Person.new("Ed")
local fred = Person.new("Fred")
local gav  = Person.new("Gav")
local hal  = Person.new("Hal")
local ian  = Person.new("Ian")
local jon  = Person.new("Jon")

local abi  = Person.new("Abi")
local bea  = Person.new("Bea")
local cath = Person.new("Cath")
local dee  = Person.new("Dee")
local eve  = Person.new("Eve")
local fay  = Person.new("Fay")
local gay  = Person.new("Gay")
local hope = Person.new("Hope")
local ivy  = Person.new("Ivy")
local jan  = Person.new("Jan")

abe.prefs  = { abi,  eve,  cath, ivy,  jan,  dee,  fay,  bea,  hope, gay  }
bob.prefs  = { cath, hope, abi,  dee,  eve,  fay,  bea,  jan,  ivy,  gay  }
col.prefs  = { hope, eve,  abi,  dee,  bea,  fay,  ivy,  gay,  cath, jan  }
dan.prefs  = { ivy,  fay,  dee,  gay,  hope, eve,  jan,  bea,  cath, abi  }
ed.prefs   = { jan,  dee,  bea,  cath, fay,  eve,  abi,  ivy,  hope, gay  }
fred.prefs = { bea,  abi,  dee,  gay,  eve,  ivy,  cath, jan,  hope, fay  }
gav.prefs  = { gay,  eve,  ivy,  bea,  cath, abi,  dee,  hope, jan,  fay  }
hal.prefs  = { abi,  eve,  hope, fay,  ivy,  cath, jan,  bea,  gay,  dee  }
ian.prefs  = { hope, cath, dee,  gay,  bea,  abi,  fay,  ivy,  jan,  eve  }
jon.prefs  = { abi,  fay,  jan,  gay,  eve,  bea,  dee,  cath, ivy,  hope }

abi.prefs  = { bob,  fred, jon,  gav,  ian,  abe,  dan,  ed,   col,  hal  }
bea.prefs  = { bob,  abe,  col,  fred, gav,  dan,  ian,  ed,   jon,  hal  }
cath.prefs = { fred, bob,  ed,   gav,  hal,  col,  ian,  abe,  dan,  jon  }
dee.prefs  = { fred, jon,  col,  abe,  ian,  hal,  gav,  dan,  bob,  ed   }
eve.prefs  = { jon,  hal,  fred, dan,  abe,  gav,  col,  ed,   ian,  bob  }
fay.prefs  = { bob,  abe,  ed,   ian,  jon,  dan,  fred, gav,  col,  hal  }
gay.prefs  = { jon,  gav,  hal,  fred, bob,  abe,  col,  ed,   dan,  ian  }
hope.prefs = { gav,  jon,  bob,  abe,  ian,  dan,  hal,  ed,   col,  fred }
ivy.prefs  = { ian,  col,  hal,  gav,  fred, bob,  abe,  ed,   jon,  dan  }
jan.prefs  = { ed,   hal,  gav,  abe,  bob,  jon,  col,  ian,  fred, dan  }

local men = abi.prefs
local freeMenCount = #men
while freeMenCount > 0 do
    for _, guy in pairs(men) do
        if not guy.fiance then
            local gal = guy:nextCandidateNotYetProposedTo()
            if not gal.fiance then
                guy:engageTo(gal)
                freeMenCount = freeMenCount - 1
            elseif gal:prefers(guy) then
                guy:engageTo(gal)
            end
        end
    end
end

print(' ')
for _, guy in pairs(men) do
    print(guy.name .. ' is engaged to ' .. guy.fiance.name)
end
print('Stable: ', isStable(men))

print(' ')
print('Switching ' .. fred.name .. "'s & " .. jon.name .. "'s partners")
jon.fiance, fred.fiance = fred.fiance, jon.fiance
print('Stable: ', isStable(men))

```


{{out}}

```txt

Bob is engaged to Cath
Fred is engaged to Bea
Jon is engaged to Abi
Gav is engaged to Gay
Ian is engaged to Hope
Abe is engaged to Ivy
Dan is engaged to Fay
Ed is engaged to Jan
Col is engaged to Dee
Hal is engaged to Eve
Stable:   true

Switching Fred's & Jon's partners
Jon and Eve prefer each other over their partners Bea and Hal
Jon and Fay prefer each other over their partners Bea and Dan
Jon and Gay prefer each other over their partners Bea and Gav
Stable:   false

```


=={{header|Objective-C}}==
{{Works with|XCode 4.5.1}}
(The C# version is essentially the same as this.)

```objc
//--------------------------------------------------------------------
// Person class

@interface Person : NSObject {
    NSUInteger _candidateIndex;
}
@property (nonatomic, strong)   NSString*   name;
@property (nonatomic, strong)   NSArray*    prefs;
@property (nonatomic, weak)     Person*     fiance;
@end

@implementation Person
+ (instancetype)named:(NSString *)name {
    return [[self alloc] initWithName:name];
}
- (instancetype)initWithName:(NSString *)name {
    if ((self = [super init])) {
        _name = name;
        _prefs = nil;
        _fiance = nil;
        _candidateIndex = 0;
    }
    return self;
}
- (BOOL)prefers:(Person *)p {
    return [_prefs indexOfObject:p] < [_prefs indexOfObject:_fiance];
}
- (Person *)nextCandidateNotYetProposedTo {
    if (_candidateIndex >= _prefs.count) return nil;
    return _prefs[_candidateIndex++];
}
- (void)engageTo:(Person *)p {
    if (p.fiance) p.fiance.fiance = nil;
    p.fiance = self;
    if (self.fiance) self.fiance.fiance = nil;
    self.fiance = p;
}
- (NSString *)description {
    return _name;
}
@end

//--------------------------------------------------------------------

BOOL isStable(NSArray *men) {
    NSArray *women = ((Person *)men[0]).prefs;
    for (Person *guy in men) {
        for (Person *gal in women) {
            if ([guy prefers:gal] && [gal prefers:guy])
                return NO;
        }
    }
    return YES;
}

//--------------------------------------------------------------------

void doMarriage() {
    Person *abe  = [Person named:@"abe"];
    Person *bob  = [Person named:@"bob"];
    Person *col  = [Person named:@"col"];
    Person *dan  = [Person named:@"dan"];
    Person *ed   = [Person named:@"ed"];
    Person *fred = [Person named:@"fred"];
    Person *gav  = [Person named:@"gav"];
    Person *hal  = [Person named:@"hal"];
    Person *ian  = [Person named:@"ian"];
    Person *jon  = [Person named:@"jon"];
    Person *abi  = [Person named:@"abi"];
    Person *bea  = [Person named:@"bea"];
    Person *cath = [Person named:@"cath"];
    Person *dee  = [Person named:@"dee"];
    Person *eve  = [Person named:@"eve"];
    Person *fay  = [Person named:@"fay"];
    Person *gay  = [Person named:@"gay"];
    Person *hope = [Person named:@"hope"];
    Person *ivy  = [Person named:@"ivy"];
    Person *jan  = [Person named:@"jan"];

    abe.prefs  = @[ abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay ];
    bob.prefs  = @[ cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay ];
    col.prefs  = @[ hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan ];
    dan.prefs  = @[ ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi ];
    ed.prefs   = @[ jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay ];
    fred.prefs = @[ bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay ];
    gav.prefs  = @[ gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay ];
    hal.prefs  = @[ abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee ];
    ian.prefs  = @[ hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve ];
    jon.prefs  = @[ abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope ];
    abi.prefs  = @[ bob, fred, jon, gav, ian, abe, dan, ed, col, hal ];
    bea.prefs  = @[ bob, abe, col, fred, gav, dan, ian, ed, jon, hal ];
    cath.prefs = @[ fred, bob, ed, gav, hal, col, ian, abe, dan, jon ];
    dee.prefs  = @[ fred, jon, col, abe, ian, hal, gav, dan, bob, ed ];
    eve.prefs  = @[ jon, hal, fred, dan, abe, gav, col, ed, ian, bob ];
    fay.prefs  = @[ bob, abe, ed, ian, jon, dan, fred, gav, col, hal ];
    gay.prefs  = @[ jon, gav, hal, fred, bob, abe, col, ed, dan, ian ];
    hope.prefs = @[ gav, jon, bob, abe, ian, dan, hal, ed, col, fred ];
    ivy.prefs  = @[ ian, col, hal, gav, fred, bob, abe, ed, jon, dan ];
    jan.prefs  = @[ ed, hal, gav, abe, bob, jon, col, ian, fred, dan ];

    NSArray *men = abi.prefs;

    NSUInteger freeMenCount = men.count;
    while (freeMenCount > 0) {
        for (Person *guy in men) {
            if (guy.fiance == nil) {
                Person *gal = [guy nextCandidateNotYetProposedTo];
                if (gal.fiance == nil) {
                    [guy engageTo:gal];
                    freeMenCount--;
                } else if ([gal prefers:guy]) {
                    [guy engageTo:gal];
                }
            }
        }
    }

    for (Person *guy in men) {
        printf("%s is engaged to %s\n", [guy.name UTF8String], [guy.fiance.name UTF8String]);
    }
    printf("Stable = %d\n", (int)isStable(men));

    printf("\nSwitching fred & jon's partners\n");
    [fred engageTo:abi];
    [jon engageTo:bea];
    printf("Stable = %d\n", (int)isStable(men));
}

//--------------------------------------------------------------------

int main(int argc, const char * argv[])
{
    @autoreleasepool {
        doMarriage();
    }
    return 0;
}
```


{{out}}

```txt
bob is engaged to cath
fred is engaged to bea
jon is engaged to abi
gav is engaged to gay
ian is engaged to hope
abe is engaged to ivy
dan is engaged to fay
ed is engaged to jan
col is engaged to dee
hal is engaged to eve
Stable = 1

Switching fred & jon's partners
Stable = 0
```



## OCaml


```ocaml
let men = [
  "abe",  ["abi";"eve";"cath";"ivy";"jan";"dee";"fay";"bea";"hope";"gay"];
  "bob",  ["cath";"hope";"abi";"dee";"eve";"fay";"bea";"jan";"ivy";"gay"];
  "col",  ["hope";"eve";"abi";"dee";"bea";"fay";"ivy";"gay";"cath";"jan"];
  "dan",  ["ivy";"fay";"dee";"gay";"hope";"eve";"jan";"bea";"cath";"abi"];
  "ed",   ["jan";"dee";"bea";"cath";"fay";"eve";"abi";"ivy";"hope";"gay"];
  "fred", ["bea";"abi";"dee";"gay";"eve";"ivy";"cath";"jan";"hope";"fay"];
  "gav",  ["gay";"eve";"ivy";"bea";"cath";"abi";"dee";"hope";"jan";"fay"];
  "hal",  ["abi";"eve";"hope";"fay";"ivy";"cath";"jan";"bea";"gay";"dee"];
  "ian",  ["hope";"cath";"dee";"gay";"bea";"abi";"fay";"ivy";"jan";"eve"];
  "jon",  ["abi";"fay";"jan";"gay";"eve";"bea";"dee";"cath";"ivy";"hope"];
]

let women = [
  "abi",  ["bob";"fred";"jon";"gav";"ian";"abe";"dan";"ed";"col";"hal"];
  "bea",  ["bob";"abe";"col";"fred";"gav";"dan";"ian";"ed";"jon";"hal"];
  "cath", ["fred";"bob";"ed";"gav";"hal";"col";"ian";"abe";"dan";"jon"];
  "dee",  ["fred";"jon";"col";"abe";"ian";"hal";"gav";"dan";"bob";"ed"];
  "eve",  ["jon";"hal";"fred";"dan";"abe";"gav";"col";"ed";"ian";"bob"];
  "fay",  ["bob";"abe";"ed";"ian";"jon";"dan";"fred";"gav";"col";"hal"];
  "gay",  ["jon";"gav";"hal";"fred";"bob";"abe";"col";"ed";"dan";"ian"];
  "hope", ["gav";"jon";"bob";"abe";"ian";"dan";"hal";"ed";"col";"fred"];
  "ivy",  ["ian";"col";"hal";"gav";"fred";"bob";"abe";"ed";"jon";"dan"];
  "jan",  ["ed";"hal";"gav";"abe";"bob";"jon";"col";"ian";"fred";"dan"];
]

type woman_name = string
type man_name = string

type man =
  { m_name: man_name;
    mutable free: bool;
    women_rank: woman_name list;
    has_proposed: (woman_name, unit) Hashtbl.t (* a set *)
  }

type woman =
  { w_name: woman_name;
    men_rank: man_name list;
    mutable engaged: man_name option
  }


let prefers w m1 m2 =
  (* returns true if w has a lower (better) rank for m1 than m2 *)
  let rec aux = function
  | [] -> invalid_arg "rank_cmp"
  | x::_ when x = m1 -> true
  | x::_ when x = m2 -> false
  | _::xs -> aux xs
  in
  aux w.men_rank

let take_while f lst =
  let rec aux acc = function
  | x::xs when f x -> aux (x::acc) xs
  | _ -> List.rev acc
  in
  aux [] lst

let more_ranked_than name =
  take_while ((<>) name)

let build_structs ~men ~women =
  List.map (fun (name, rank) ->
    { m_name = name;
      women_rank = rank;
      free = true;
      has_proposed = Hashtbl.create 42 }
  ) men,
  List.map (fun (name, rank) ->
    { w_name = name;
      men_rank = rank;
      engaged = None }
  ) women


let _stable_matching ms ws =
  let men_by_name = Hashtbl.create 42 in
  List.iter (fun m -> Hashtbl.add men_by_name m.m_name m) ms;
  let women_by_name = Hashtbl.create 42 in
  List.iter (fun w -> Hashtbl.add women_by_name w.w_name w) ws;
  try
    while true do
      (*TODO free men who still has some w to propose to *)
      let m = List.find (fun m -> m.free) ms in
      (* highest ranked woman who the man has not proposed to yet *)
      let w_name =
        List.find (fun w -> not (Hashtbl.mem m.has_proposed w)) m.women_rank in
      Hashtbl.add m.has_proposed w_name ();
      let w = Hashtbl.find women_by_name w_name in
      match w.engaged with
      | None -> (* w is free *)
          (* (m, w) become engaged *)
          w.engaged <- Some m.m_name;
          m.free <- false
      | Some m'_name -> (* some pair (m', w) already exists *)
          if prefers w m.m_name m'_name
          then begin (* w prefers m to m' *)
            w.engaged <- Some m.m_name;
            let m' = Hashtbl.find men_by_name m'_name in
            m'.free <- true;
            m.free <- false
          end
    done;
    assert false
  with Not_found -> ()


let stable_matching ~men ~women =
  let ms, ws = build_structs ~men ~women in
  _stable_matching ms ws;
  let some = function Some v -> v | None -> "" in
  List.map (fun w -> w.w_name, some w.engaged) ws


let is_stable ~men ~women eng =
  let ms, ws = build_structs ~men ~women in
  not (List.exists (fun (wn, mn) ->
      let m = List.find (fun m -> m.m_name = mn) ms in
      let prefered_women = more_ranked_than wn m.women_rank in
      List.exists (fun pref_w ->
        let w = List.find (fun w -> w.w_name = pref_w) ws in
        let eng_m = List.assoc pref_w eng in
        let prefered_men = more_ranked_than eng_m w.men_rank in
        List.mem m.m_name prefered_men (* exists unstable engagement *)
      ) prefered_women
    ) eng)


let perturb_engagements eng =
  Random.self_init();
  let eng = Array.of_list eng in
  let len = Array.length eng in
  for n = 1 to 3 do
    let i = Random.int len
    and j = Random.int len in
    let w1, m1 = eng.(i)
    and w2, m2 = eng.(j) in
    eng.(i) <- (w1, m2);
    eng.(j) <- (w2, m1);
  done;
  Array.to_list eng


let print engs =
  List.iter (fun (w,m) ->
    Printf.printf " %4s is engaged with %s\n" w m) engs;
  Printf.printf "# Engagements %s stable\n"
    (if is_stable ~men ~women engs then "are" else "are not")

let () =
  let engagements = stable_matching ~men ~women in
  print engagements;
  print_endline "
### ==================
";
  let engagements = perturb_engagements engagements in
  print engagements;
;;
```

{{out}}

```txt
  fay is engaged with dan
  ivy is engaged with abe
 cath is engaged with bob
  eve is engaged with hal
  abi is engaged with jon
  dee is engaged with col
 hope is engaged with ian
  gay is engaged with gav
  bea is engaged with fred
  jan is engaged with ed
# Engagements are stable

### ==================

  fay is engaged with abe
  ivy is engaged with dan
 cath is engaged with hal
  eve is engaged with bob
  abi is engaged with jon
  dee is engaged with col
 hope is engaged with ed
  gay is engaged with gav
  bea is engaged with fred
  jan is engaged with ian
# Engagements are not stable
```



## Perl


```Perl
#!/usr/bin/env perl
use strict;
use warnings;
use feature qw/say/;
use List::Util qw(first);

my %Likes = (
  M => {
    abe  => [qw/ abi eve cath ivy jan dee fay bea hope gay /],
    bob  => [qw/ cath hope abi dee eve fay bea jan ivy gay /],
    col  => [qw/ hope eve abi dee bea fay ivy gay cath jan /],
    dan  => [qw/ ivy fay dee gay hope eve jan bea cath abi /],
    ed   => [qw/ jan dee bea cath fay eve abi ivy hope gay /],
    fred => [qw/ bea abi dee gay eve ivy cath jan hope fay /],
    gav  => [qw/ gay eve ivy bea cath abi dee hope jan fay /],
    hal  => [qw/ abi eve hope fay ivy cath jan bea gay dee /],
    ian  => [qw/ hope cath dee gay bea abi fay ivy jan eve /],
    jon  => [qw/ abi fay jan gay eve bea dee cath ivy hope /],
  },

  W => {
    abi  => [qw/ bob fred jon gav ian abe dan ed col hal /],
    bea  => [qw/ bob abe col fred gav dan ian ed jon hal /],
    cath => [qw/ fred bob ed gav hal col ian abe dan jon /],
    dee  => [qw/ fred jon col abe ian hal gav dan bob ed /],
    eve  => [qw/ jon hal fred dan abe gav col ed ian bob /],
    fay  => [qw/ bob abe ed ian jon dan fred gav col hal /],
    gay  => [qw/ jon gav hal fred bob abe col ed dan ian /],
    hope => [qw/ gav jon bob abe ian dan hal ed col fred /],
    ivy  => [qw/ ian col hal gav fred bob abe ed jon dan /],
    jan  => [qw/ ed hal gav abe bob jon col ian fred dan /],
  },
);

my %Engaged;
my %Proposed;

match_them();
check_stability();
perturb();
check_stability();

sub match_them {
    say 'Matchmaking:';
    while(my $man = unmatched_man()) {
        my $woman = preferred_choice($man);
        $Proposed{$man}{$woman} = 1;
        if(! $Engaged{W}{$woman}) {
            engage($man, $woman);
            say "\t$woman and $man";
        }
        else {
            if(woman_prefers($woman, $man)) {
                my $engaged_man = $Engaged{W}{$woman};
                engage($man, $woman);
                undef $Engaged{M}{$engaged_man};
                say "\t$woman dumped $engaged_man for $man";
            }
        }
    }
}

sub check_stability {
    say 'Stablility:';
    my $stable = 1;
    foreach my $m (men()) {
        foreach my $w (women()) {
            if(man_prefers($m, $w) && woman_prefers($w, $m)) {
                say "\t$w prefers $m to $Engaged{W}{$w} and $m prefers $w to $Engaged{M}{$m}";
                $stable = 0;
            }
        }
    }
    if($stable) {
        say "\t(all marriages stable)";
    }
}

sub unmatched_man {
    return first { ! $Engaged{M}{$_} } men();
}

sub preferred_choice {
    my $man = shift;
    return first { ! $Proposed{$man}{$_} } @{ $Likes{M}{$man} };
}

sub engage {
    my ($man, $woman) = @_;
    $Engaged{W}{$woman} = $man;
    $Engaged{M}{$man} = $woman;
}

sub prefers {
    my $sex = shift;
    return sub {
        my ($person, $prospect) = @_;

        my $choices = join ' ', @{ $Likes{$sex}{$person} };
        return index($choices, $prospect) < index($choices, $Engaged{$sex}{$person});
    }
}

BEGIN {
    *woman_prefers = prefers('W');
    *man_prefers   = prefers('M');
}

sub perturb {
    say 'Perturb:';
    say "\tengage abi with fred and bea with jon";
    engage('fred' => 'abi');
    engage('jon'  => 'bea');
}

sub men   { keys %{ $Likes{M} } }
sub women { keys %{ $Likes{W} } }
```

{{out}}

```txt

Matchmaking:
        abi and abe
        ivy and dan
        abi dumped abe for jon
        eve and abe
        eve dumped abe for hal
        cath and abe
        gay and gav
        jan and ed
        hope and ian
        dee and col
        cath dumped abe for bob
        ivy dumped dan for abe
        fay and dan
        bea and fred
Stablility:
        (all marriages stable)
Perturb:
        engage abi with fred and bea with jon
Stablility:
        eve prefers jon to hal and jon prefers eve to bea
        gay prefers jon to gav and jon prefers gay to bea
        fay prefers jon to dan and jon prefers fay to bea
        bea prefers fred to jon and fred prefers bea to abi

```


## Perl 6

{{Works with|rakudo|2016.10}}
{{trans|Perl}}

```perl6
my %he-likes =
    abe  => < abi eve cath ivy jan dee fay bea hope gay >,
    bob  => < cath hope abi dee eve fay bea jan ivy gay >,
    col  => < hope eve abi dee bea fay ivy gay cath jan >,
    dan  => < ivy fay dee gay hope eve jan bea cath abi >,
    ed   => < jan dee bea cath fay eve abi ivy hope gay >,
    fred => < bea abi dee gay eve ivy cath jan hope fay >,
    gav  => < gay eve ivy bea cath abi dee hope jan fay >,
    hal  => < abi eve hope fay ivy cath jan bea gay dee >,
    ian  => < hope cath dee gay bea abi fay ivy jan eve >,
    jon  => < abi fay jan gay eve bea dee cath ivy hope >,
;

my %she-likes =
    abi  => < bob fred jon gav ian abe dan ed col hal >,
    bea  => < bob abe col fred gav dan ian ed jon hal >,
    cath => < fred bob ed gav hal col ian abe dan jon >,
    dee  => < fred jon col abe ian hal gav dan bob ed >,
    eve  => < jon hal fred dan abe gav col ed ian bob >,
    fay  => < bob abe ed ian jon dan fred gav col hal >,
    gay  => < jon gav hal fred bob abe col ed dan ian >,
    hope => < gav jon bob abe ian dan hal ed col fred >,
    ivy  => < ian col hal gav fred bob abe ed jon dan >,
    jan  => < ed hal gav abe bob jon col ian fred dan >,
;

my %fiancé;
my %fiancée;
my %proposed;

sub she-prefers ($her, $hottie) { .index($hottie) < .index(%fiancé{$her}) given ~%she-likes{$her} }
sub he-prefers  ($him, $hottie) { .index($hottie) < .index(%fiancée{$him}) given ~%he-likes{$him} }

match'em;
check-stability;

perturb'em;
check-stability;

sub match'em {                                          #'
    say 'Matchmaking:';
    while unmatched-guy() -> $guy {
        my $gal = preferred-choice($guy);
        %proposed{"$guy $gal"} = '❤';
        if not %fiancé{$gal} {
            engage($guy, $gal);
            say "\t$gal and $guy";
        }
        elsif she-prefers($gal, $guy) {
	    my $engaged-guy = %fiancé{$gal};
	    engage($guy, $gal);
	    %fiancée{$engaged-guy} = '';
	    say "\t$gal dumped $engaged-guy for $guy";
	}
    }
}

sub check-stability {
    my @instabilities = gather for flat %he-likes.keys X %she-likes.keys -> $m, $w {
	if he-prefers($m, $w) and she-prefers($w, $m) {
	    take "\t$w prefers $m to %fiancé{$w} and $m prefers $w to %fiancée{$m}";
	}
    }

    say 'Stablility:';
    if @instabilities {
	.say for @instabilities;
    }
    else {
        say "\t(all marriages stable)";
    }
}

sub unmatched-guy { %he-likes.keys.first: { not %fiancée{$_} } }

sub preferred-choice($guy) { %he-likes{$guy}.first: { not %proposed{"$guy $_" } } }

sub engage($guy, $gal) {
    %fiancé{$gal} = $guy;
    %fiancée{$guy} = $gal;
}

sub perturb'em {                                            #'
    say 'Perturb:';
    say "\tengage abi with fred and bea with jon";
    engage('fred', 'abi');
    engage('jon', 'bea');
}
```

{{out}}

```txt
Matchmaking:
        cath and bob
        ivy and dan
        abi and jon
        hope and ian
        jan and ed
        eve and abe
        eve dumped abe for hal
        ivy dumped dan for abe
        fay and dan
        gay and gav
        bea and fred
        dee and col
Stablility:
        (all marriages stable)
Perturb:
        engage abi with fred and bea with jon
Stablility:
        fay prefers jon to dan and jon prefers fay to bea
        eve prefers jon to hal and jon prefers eve to bea
        gay prefers jon to gav and jon prefers gay to bea
        bea prefers fred to jon and fred prefers bea to abi
```



## Phix

{{trans|AutoHotkey}}

```Phix
constant men = {"abe","bob","col","dan","ed","fred","gav","hal","ian","jon"}
enum             abe , bob , col , dan , ed , fred , gav , hal , ian , jon
constant hen = {"abi","bea","cath","dee","eve","fay","gay","hope","ivy","jan"}
enum             abi , bea , cath , dee , eve , fay , gay , hope , ivy , jan

-- Given a complete list of ranked preferences, where the most liked is to the left:
sequence mpref = repeat(0,length(men))
    mpref[abe]  = {abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay}
    mpref[bob]  = {cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay}
    mpref[col]  = {hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan}
    mpref[dan]  = {ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi}
    mpref[ed]   = {jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay}
    mpref[fred] = {bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay}
    mpref[gav]  = {gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay}
    mpref[hal]  = {abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee}
    mpref[ian]  = {hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve}
    mpref[jon]  = {abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope}

sequence hpref = repeat(0,length(hen))
    hpref[abi]  = {bob, fred, jon, gav, ian, abe, dan, ed, col, hal}
    hpref[bea]  = {bob, abe, col, fred, gav, dan, ian, ed, jon, hal}
    hpref[cath] = {fred, bob, ed, gav, hal, col, ian, abe, dan, jon}
    hpref[dee]  = {fred, jon, col, abe, ian, hal, gav, dan, bob, ed}
    hpref[eve]  = {jon, hal, fred, dan, abe, gav, col, ed, ian, bob}
    hpref[fay]  = {bob, abe, ed, ian, jon, dan, fred, gav, col, hal}
    hpref[gay]  = {jon, gav, hal, fred, bob, abe, col, ed, dan, ian}
    hpref[hope] = {gav, jon, bob, abe, ian, dan, hal, ed, col, fred}
    hpref[ivy]  = {ian, col, hal, gav, fred, bob, abe, ed, jon, dan}
    hpref[jan]  = {ed, hal, gav, abe, bob, jon, col, ian, fred, dan}

sequence engagements := repeat(0,length(hen))
sequence freemen = tagset(length(men))
printf(1,"Engagements:\n")

-- use the Gale Shapley algorithm to find a stable set of engagements:
while length(freemen) do
    integer man = freemen[1]
    freemen = freemen[2..$]
    integer fem, dumpee
    -- each male loops through all females in order of his preference until one accepts him
    for j=1 to length(mpref[man]) do
        fem = mpref[man][j]
        dumpee = engagements[fem]
        if dumpee=0
        or find(man,hpref[fem])<find(dumpee,hpref[fem]) then
            exit
        end if
    end for
    if dumpee!=0 then       -- if she was previously engaged
        freemen &= dumpee   -- he goes to the bottom of the list
        printf(1,"%s dumped %s and accepted %s\n",{hen[fem],men[dumpee],men[man]})
    else
        printf(1,"%s accepted %s\n",{hen[fem],men[man]})
    end if
    engagements[fem] := man -- the new engagement is registered
end while

printf(1,"\nCouples:\n")
for fem=1 to length(hen) do
    printf(1,"%s is engaged to %s\n",{hen[fem],men[engagements[fem]]})
end for

procedure stable()
bool unstable = false
    for fem=1 to length(hen) do
        integer man = engagements[fem]
        for j=1 to length(hen) do
            if find(fem,mpref[man])>find(j,mpref[man])
            and find(engagements[j],hpref[j])>find(man,hpref[j]) then
                if unstable=false then
                    printf(1,"\nThese couples are not stable.\n")
                    unstable = true
                end if
                printf(1,"%s is engaged to %s but would prefer %s and %s is engaged to %s but would prefer %s\n",
                         {men[man],hen[fem],hen[j],hen[j],men[engagements[j]],men[man]})
            end if
        end for
    end for
    if not unstable then
        printf(1,"\nThese couples are stable.\n")
    end if
end procedure

stable()
printf(1,"\nWhat if cath and ivy swap?\n")
engagements[cath]:=abe; engagements[ivy]:=bob
stable()
```

{{Out}}

```txt

Engagements:
abi accepted abe
cath accepted bob
hope accepted col
ivy accepted dan
jan accepted ed
bea accepted fred
gay accepted gav
eve accepted hal
hope dumped col and accepted ian
abi dumped abe and accepted jon
dee accepted col
ivy dumped dan and accepted abe
fay accepted dan

Couples:
abi is engaged to jon
bea is engaged to fred
cath is engaged to bob
dee is engaged to col
eve is engaged to hal
fay is engaged to dan
gay is engaged to gav
hope is engaged to ian
ivy is engaged to abe
jan is engaged to ed

These couples are stable.

What if cath and ivy swap?

These couples are not stable.
bob is engaged to ivy but would prefer abi and abi is engaged to jon but would prefer bob
bob is engaged to ivy but would prefer bea and bea is engaged to fred but would prefer bob
bob is engaged to ivy but would prefer cath and cath is engaged to abe but would prefer bob
bob is engaged to ivy but would prefer fay and fay is engaged to dan but would prefer bob
bob is engaged to ivy but would prefer hope and hope is engaged to ian but would prefer bob

```



## PicoLisp


```PicoLisp
(setq
   *Boys (list
      (de abe  abi eve cath ivy jan dee fay bea hope gay)
      (de bob  cath hope abi dee eve fay bea jan ivy gay)
      (de col  hope eve abi dee bea fay ivy gay cath jan)
      (de dan  ivy fay dee gay hope eve jan bea cath abi)
      (de ed   jan dee bea cath fay eve abi ivy hope gay)
      (de fred bea abi dee gay eve ivy cath jan hope fay)
      (de gav  gay eve ivy bea cath abi dee hope jan fay)
      (de hal  abi eve hope fay ivy cath jan bea gay dee)
      (de ian  hope cath dee gay bea abi fay ivy jan eve)
      (de jon  abi fay jan gay eve bea dee cath ivy hope) )
   *Girls (list
      (de bi   bob fred jon gav ian abe dan ed col hal)
      (de bea  bob abe col fred gav dan ian ed jon hal)
      (de cath fred bob ed gav hal col ian abe dan jon)
      (de dee  fred jon col abe ian hal gav dan bob ed)
      (de eve  jon hal fred dan abe gav col ed ian bob)
      (de fay  bob abe ed ian jon dan fred gav col hal)
      (de gay  jon gav hal fred bob abe col ed dan ian)
      (de hope gav jon bob abe ian dan hal ed col fred)
      (de ivy  ian col hal gav fred bob abe ed jon dan)
      (de jan  ed hal gav abe bob jon col ian fred dan) )
   *Couples NIL )

(bind *Boys
   (while
      (find
         '((Boy) (and (val Boy) (not (asoq Boy *Couples))))
         *Boys )
      (let (Boy @  Girl (pop Boy)  Pair (find '((P) (== Girl (cdr P))) *Couples))
         (nond
            (Pair (push '*Couples (cons Boy Girl)))   # Girl is free
            ((memq Boy (memq (car Pair) (val Girl)))  # Girl prefers Boy
               (set Pair Boy) ) ) ) ) )

(for Pair *Couples
   (prinl (cdr Pair) " is engaged to " (car Pair)) )

(de checkCouples ()
   (unless
      (filter
         '((Pair)
            (let (Boy (car Pair)  Girl (cdr Pair))
               (find
                  '((B)
                     (and
                        (memq Boy (cdr (memq B (val Girl))))  # Girl prefers B
                        (memq
                           (cdr (asoq B *Couples))            # and B prefers Girl
                           (cdr (memq Girl (val B))) )
                        (prinl
                           Girl " likes " B " better than " Boy " and "
                           B " likes " Girl " better than "
                           (cdr (asoq B *Couples)) ) ) )
                  (val Girl) ) ) )
         *Couples )
      (prinl "All marriages are stable") ) )

(checkCouples)
(prinl)
(prinl "Engage fred with abi and jon with bea")
(con (asoq 'fred *Couples) 'abi)
(con (asoq 'jon *Couples) 'bea)
(checkCouples)
```

{{out}}

```txt
dee is engaged to col
fay is engaged to dan
eve is engaged to hal
gay is engaged to gav
bea is engaged to fred
jan is engaged to ed
ivy is engaged to abe
hope is engaged to ian
cath is engaged to bob
abi is engaged to jon
All marriages are stable

Engage fred with abi and jon with bea
fay likes jon better than dan and jon likes fay better than bea
eve likes jon better than hal and jon likes eve better than bea
gay likes jon better than gav and jon likes gay better than bea
bea likes fred better than jon and fred likes bea better than abi
```



## Prolog

{{Works with|SWI-Prolog}}{{libheader|XPCE}}
XPCE is used for its integrated messaging system.

```Prolog
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% facts
prefere(abe,[ abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay]).
prefere(  bob,[ cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay]).
prefere(  col,[ hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan]).
prefere(  dan,[ ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi]).
prefere(   ed,[ jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay]).
prefere( fred,[ bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay]).
prefere(  gav,[ gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay]).
prefere(  hal,[ abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee]).
prefere(  ian,[ hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve]).
prefere(  jon,[ abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope]).

prefere(  abi,[ bob, fred, jon, gav, ian, abe, dan, ed, col, hal]).
prefere(  bea,[ bob, abe, col, fred, gav, dan, ian, ed, jon, hal]).
prefere( cath,[ fred, bob, ed, gav, hal, col, ian, abe, dan, jon]).
prefere(  dee,[ fred, jon, col, abe, ian, hal, gav, dan, bob, ed]).
prefere(  eve,[ jon, hal, fred, dan, abe, gav, col, ed, ian, bob]).
prefere(  fay,[ bob, abe, ed, ian, jon, dan, fred, gav, col, hal]).
prefere(  gay,[ jon, gav, hal, fred, bob, abe, col, ed, dan, ian]).
prefere( hope,[ gav, jon, bob, abe, ian, dan, hal, ed, col, fred]).
prefere(  ivy,[ ian, col, hal, gav, fred, bob, abe, ed, jon, dan]).
prefere(  jan,[ ed, hal, gav, abe, bob, jon, col, ian, fred, dan]).


man(abe).
man(bob).
man(col).
man(dan).
man(ed).
man(fred).
man(gav).
man(hal).
man(ian).
man(jon).

woman(abi).
woman(bea).
woman(cath).
woman(dee).
woman(eve).
woman(fay).
woman(gay).
woman(hope).
woman(ivy).
woman(jan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rules

stable_mariage :-
	new(LstMan, chain),
	forall(man(X),
	       (   prefere(X, Lst),
		   new(P, man(X, Lst)),
		   send(LstMan, append, P))),

	new(LstWoman, chain),
	forall(woman(X),
	       (   prefere(X, Lst),
		   new(P, woman(X, Lst)),
		   send(LstWoman, append, P))),
	send(LstMan, for_all, message(@arg1, init_liste, LstWoman)),
	send(LstWoman, for_all, message(@arg1, init_liste, LstMan)),

	round(LstMan, LstWoman),
	new(LstCouple, chain),
	% creation of the couple.
	send(LstWoman, for_all, and(message(@prolog, create_couple, @arg1, LstCouple),
				   message(@pce, write_ln, @arg1?name, with, @arg1?elu?name))),

	nl,

	% test of the stability of couples
	stability(LstCouple),
	nl,

	% Perturbation of couples
	get(LstCouple, size, Len),
	get_two_random_couples(Len, V1, V2),

	get(LstCouple, nth0, V1, C1),
	get(LstCouple, nth0, V2, C2),
	new(NC1, tuple(C1?first, C2?second)),
	new(NC2, tuple(C2?first, C1?second)),
	send(LstCouple, nth0, V1, NC1),
	send(LstCouple, nth0, V2, NC2),

	send(@pce, write_ln, 'perturbation of couples'),
	send(@pce, write_ln, NC1?second, with, NC1?first),
	send(@pce, write_ln, NC2?second, with, NC2?first),
	nl,

	stability(LstCouple).

get_two_random_couples(Len, C1, C2) :-
	C1 is random(Len),
	repeat,
	C2 is random(Len),
	C1 \= C2.

create_couple(Woman, LstCouple ) :-
	send(LstCouple, append, new(_, tuple(Woman?elu?name, Woman?name))).

% iterations of the algorithm
round(LstMan, LstWoman) :-
	send(LstMan, for_some, message(@arg1, propose)),
	send(LstWoman, for_some, message(@arg1, dispose)),
	(   \+send(LstWoman, for_all, @arg1?status == maybe)
	->
	    round(LstMan, LstWoman)
	;
	    true
	).

:-pce_begin_class(person, object, "description of a person").
variable(name, object, both, "name of the person").
variable(preference, chain, both, "list of priority").
variable(status, object, both, "statut of engagement : maybe / free").

initialise(P, Name, Pref) :->
	send(P, send_super, initialise),
	send(P, slot, name, Name),
	send(P, slot, preference, Pref),
	send(P, slot, status, free).

% reception of the list of partners
init_liste(P, Lst) :->
	% we replace the list of name of partners
	% with the list of persons partners.
	new(NLP, chain),
	get(P, slot, preference, LP),
	send(LP, for_all, message(@prolog, find_person,@arg1, Lst, NLP)),
	send(P, slot, preference, NLP).

:- pce_end_class(person).



find_person(Name, LstPerson, LstPref) :-
	get(LstPerson, find, @arg1?name == Name, Elem),
	send(LstPref, append, Elem).

:-pce_begin_class(man, person, "description of a man").

initialise(P, Name, Pref) :->
	send(P, send_super, initialise, Name, Pref).

% a man propose "la botte" to a woman
propose(P) :->
	get(P, slot, status, free),
	get(P, slot, preference, XPref),
	get(XPref, delete_head, Pref),
	send(P, slot, preference, XPref),
	send(Pref, proposition, P).

refuse(P) :->
	send(P, slot, status, free).

accept(P) :->
	send(P, slot, status, maybe).

:- pce_end_class(man).

:-pce_begin_class(woman, person, "description of a woman").
variable(elu, object, both, "name of the elu").
variable(contact, chain, both, "men that have contact this woman").

initialise(P, Name, Pref) :->
	send(P, send_super, initialise, Name, Pref),
	send(P, slot, contact, new(_, chain)),
	send(P, slot, elu, @nil).

% a woman decide Maybe/No
dispose(P) :->
	get(P, slot, contact, Contact),
	get(P, slot, elu, Elu),

	(   Elu \= @nil
	->
	    send(Contact, append, Elu)
	;
	    true),

	new(R, chain),
	send(Contact, for_all, message(P, fetch, @arg1, R)),
	send(R, sort, ?(@arg1?first, compare, @arg2?first)),
	get(R, delete_head, Tete),
	send(Tete?second, accept),
	send(P, slot, status, maybe),
	send(P, slot, elu, Tete?second),
	send(R, for_some, message(@arg1?second, refuse)),
	send(P, slot, contact, new(_, chain)) .


% looking for the person of the given name  Contact
% Adding it in the chain Chain
fetch(P, Contact, Chain) :->
	get(P, slot, preference, Lst),
	get(Lst, find, @arg1?name == Contact?name, Elem),
	get(Lst, index, Elem, Ind),
	send(Chain, append, new(_, tuple(Ind, Contact))).

% a woman receive a proposition from a man
proposition(P, Name) :->
	get(P, slot, contact, C),
	send(C, append, Name),
	send(P, slot, contact, C).

:- pce_end_class(woman).

% computation of the stability od couple
stability(LstCouple) :-
	chain_list(LstCouple, LstPceCouple),
	maplist(transform, LstPceCouple, PrologLstCouple),
	study_couples(PrologLstCouple, [], UnstableCouple),
	(   UnstableCouple = []
	->
	    writeln('Couples are stable')
	;
	    sort(UnstableCouple, SortUnstableCouple),
	    writeln('Unstable couples are'),
	    maplist(print_unstable_couple, SortUnstableCouple),
	    nl
	).


print_unstable_couple((C1, C2)) :-
	format('~w and ~w~n', [C1, C2]).

transform(PceCouple, couple(First, Second)):-
	get(PceCouple?first, value, First),
	get(PceCouple?second, value, Second).

study_couples([], UnstableCouple, UnstableCouple).

study_couples([H | T], CurrentUnstableCouple, UnstableCouple):-
	include(unstable_couple(H), T, Lst),
	(   Lst \= []
	->
	    maplist(build_one_couple(H), Lst, Lst1),
	    append(CurrentUnstableCouple, Lst1,CurrentUnstableCouple1)
	;
	    CurrentUnstableCouple1 = CurrentUnstableCouple
	),
	study_couples(T, CurrentUnstableCouple1, UnstableCouple).


build_one_couple(C1, C2, (C1, C2)).

unstable_couple(couple(X1, Y1), couple(X2, Y2)) :-
	prefere(X1, PX1),
	prefere(X2, PX2),
	prefere(Y1, PY1),
	prefere(Y2, PY2),

	% index of women for X1
	nth0(IY12, PX1, Y2),
	nth0(IY11, PX1, Y1),
	% index of men for Y2
	nth0(IX21, PY2, X1),
	nth0(IX22, PY2, X2),

	% index of women for X2
	nth0(IY21, PX2, Y1),
	nth0(IY22, PX2, Y2),
	% index of men for Y1
	nth0(IX11, PY1, X1),
	nth0(IX12, PY1, X2),

	% A couple is unstable
	( (IY12 < IY11 , IX21 < IX22);
	  (IY21 < IY22 , IX12 < IX11)).

```

{{out}}

```txt
 ?- stable_mariage.
abi with jon
bea with fred
cath with bob
dee with col
eve with hal
fay with dan
gay with gav
hope with ian
ivy with abe
jan with ed

Couples are stable

perturbation of couples
gay with jon
abi with gav

Unstable couples are
couple(gav,abi) and couple(abe,ivy)
couple(jon,gay) and couple(dan,fay)
couple(jon,gay) and couple(gav,abi)

true
```

A more Prolog-ish version (working with SWI-Prolog) could be :

```Prolog
:- dynamic person/4, prop/2.
% person(Name, Preference, Status, Candidate)
% prop(Name, List_of_Candidates) (for a woman)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% facts
prefere(abe,[ abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay]).
prefere(  bob,[ cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay]).
prefere(  col,[ hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan]).
prefere(  dan,[ ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi]).
prefere(   ed,[ jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay]).
prefere( fred,[ bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay]).
prefere(  gav,[ gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay]).
prefere(  hal,[ abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee]).
prefere(  ian,[ hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve]).
prefere(  jon,[ abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope]).

prefere(  abi,[ bob, fred, jon, gav, ian, abe, dan, ed, col, hal]).
prefere(  bea,[ bob, abe, col, fred, gav, dan, ian, ed, jon, hal]).
prefere( cath,[ fred, bob, ed, gav, hal, col, ian, abe, dan, jon]).
prefere(  dee,[ fred, jon, col, abe, ian, hal, gav, dan, bob, ed]).
prefere(  eve,[ jon, hal, fred, dan, abe, gav, col, ed, ian, bob]).
prefere(  fay,[ bob, abe, ed, ian, jon, dan, fred, gav, col, hal]).
prefere(  gay,[ jon, gav, hal, fred, bob, abe, col, ed, dan, ian]).
prefere( hope,[ gav, jon, bob, abe, ian, dan, hal, ed, col, fred]).
prefere(  ivy,[ ian, col, hal, gav, fred, bob, abe, ed, jon, dan]).
prefere(  jan,[ ed, hal, gav, abe, bob, jon, col, ian, fred, dan]).


man(abe).
man(bob).
man(col).
man(dan).
man(ed).
man(fred).
man(gav).
man(hal).
man(ian).
man(jon).

woman(abi).
woman(bea).
woman(cath).
woman(dee).
woman(eve).
woman(fay).
woman(gay).
woman(hope).
woman(ivy).
woman(jan).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rules
stable_mariage :-
	%initialization
	retractall(person(_,_,_,_)),
	retractall(prop(_,_)),
	forall(prefere(P, Pref), assert(person(P, Pref, free, none))),
	bagof(X, man(X), LstMen),
	bagof(Y, woman(Y), LstWomen),
	forall(member(Z, LstWomen), assert(prop(Z, []))),

	% compute the mariages
	iteration(LstMen, LstWomen, LstCouple),
	maplist(print_couple,LstCouple),
	nl,

	% test of the stability of couples
	stability(LstCouple),
	nl,

	% Perturbation of couples
	length(LstCouple, Len),
	get_two_random_couples(Len, V1, V2),

	nth0(V1, LstCouple, C1),
	select(C1, LstCouple, Lst1),
	(   V2 > 0 -> V22 is V2 - 1; V22 = V2),
	nth0(V22, Lst1, C2),
	select(C2, Lst1, Lst2),
	C1 = couple(M1, W1),
	C2 = couple(M2, W2),

	writeln('perturbation of couples'),
	format('~w with ~w~n', [W1, M2]),
	format('~w with ~w~n', [W2, M1]),
	nl,
	stability([couple(M1, W2), couple(M2, W1)| Lst2]).


% the algorithm
iteration(Men, Women, LstCouples) :-
	% Men propose
	bagof(M,  X^Y^(member(M, Men),person(M, X, free, Y)), LM),
	forall(member(X, LM),
	       (   retract(person(X, [W|Pref], free, Elu)),
		   assert(person(X, Pref, free, Elu)),
		   retract(prop(W, Prop)),
		   assert(prop(W, [X| Prop])))),

	% women dispose
	bagof(W, L^(prop(W, L), L \= []), LW),
	forall(member(W, LW),
	       (   retract(prop(W, Prop)),
	           retract(person(W, Pref, _, Elu)),
		   (   Elu = none -> Prop1 = Prop; Prop1 = [Elu|Prop]),
	           order_prop(Pref, Prop1, [M | Prop2]),
		   retract(person(M, PrefM, _, _)),
		   assert(person(M, PrefM, maybe, W)),
		   forall(member(Y, Prop2),
			  (   retract(person(Y, Pref1, _, _TE)),
			      assert(person(Y, Pref1, free, none)))),
		   assert(prop(W, [])),
		   assert(person(W, Pref, maybe, M))
	       )),

	% finished ?
	(   bagof(X, T^Z^(member(X, Women), person(X, T, free, Z)), _LW1) ->
	    iteration(Men, Women, LstCouples)
	;
	    make_couple(Women, LstCouples)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compute order preference of candidates.
order_prop(Pref, Prop, Res) :-
	maplist(index(Pref), Prop, Rtemp),
	sort(Rtemp, Rtemp1),
	maplist(simplifie,Rtemp1, Res).

index(Lst, Value, [Ind, Value]) :-
	nth0(Ind, Lst, Value).

simplifie([_, V], V).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_couple(couple(M, W)) :-
	format('~w with ~w~n', [W, M]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% creation of couples
make_couple([], []).

make_couple([W | LW], [couple(M, W) | LC]) :-
	make_couple(LW, LC),
	person(W, _, _, M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% miscellaneous
get_two_random_couples(Len, C1, C2) :-
	C1 is random(Len),
	repeat,
	C2 is random(Len),
	C1 \= C2.

print_unstable_couple((C1, C2)) :-
	format('~w and ~w~n', [C1, C2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% test the stability of couples
stability(LstCouple) :-
	study_couples(LstCouple, [], CoupleInstable),
	(   CoupleInstable = []
	->
	    writeln('Couples are stable')
	;
	    sort(CoupleInstable, SortCoupleInstable),
	    writeln('Unstable couples are'),
	    maplist(print_unstable_couple, SortCoupleInstable),
	    nl
	).


% compute the stability od couple
study_couples([], UnstableCouple, UnstableCouple).

study_couples([H | T], CurrentUnstableCouple, UnstableCouple):-
	include(unstable_couple(H), T, Lst),
	(   Lst \= []
	->
	    maplist(build_one_couple(H), Lst, Lst1),
	   append(CurrentUnstableCouple, Lst1,CurrentUnstableCouple1)
	;
	   CurrentUnstableCouple1 = CurrentUnstableCouple
	),
	study_couples(T, CurrentUnstableCouple1, UnstableCouple).


build_one_couple(C1, C2, (C1, C2)).

unstable_couple(couple(X1, Y1), couple(X2, Y2)) :-
	prefere(X1, PX1),
	prefere(X2, PX2),
	prefere(Y1, PY1),
	prefere(Y2, PY2),

	% index of women for X1
	nth0(IY12, PX1, Y2),
	nth0(IY11, PX1, Y1),
	% index of men for Y2
	nth0(IX21, PY2, X1),
	nth0(IX22, PY2, X2),

	% index of women for X2
	nth0(IY21, PX2, Y1),
	nth0(IY22, PX2, Y2),
	% index of men for Y1
	nth0(IX11, PY1, X1),
	nth0(IX12, PY1, X2),

	% A couple is unstable
	( (IY12 < IY11 , IX21 < IX22);
	  (IY21 < IY22 , IX12 < IX11)).
```



## PureBasic

This approach uses a messaging system to pass messages between prospective partners.

```PureBasic
#coupleCount = 10

DataSection
  ;guys
  Data.s "abe: abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay"
  Data.s "bob: cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay"
  Data.s "col: hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan"
  Data.s "dan: ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi"
  Data.s "ed: jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay"
  Data.s "fred: bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay"
  Data.s "gav: gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay"
  Data.s "hal: abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee"
  Data.s "ian: hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve"
  Data.s "jon: abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope"
  ;gals
  Data.s "abi: bob, fred, jon, gav, ian, abe, dan, ed, col, hal"
  Data.s "bea: bob, abe, col, fred, gav, dan, ian, ed, jon, hal"
  Data.s "cath: fred, bob, ed, gav, hal, col, ian, abe, dan, jon"
  Data.s "dee: fred, jon, col, abe, ian, hal, gav, dan, bob, ed"
  Data.s "eve: jon, hal, fred, dan, abe, gav, col, ed, ian, bob"
  Data.s "fay: bob, abe, ed, ian, jon, dan, fred, gav, col, hal"
  Data.s "gay: jon, gav, hal, fred, bob, abe, col, ed, dan, ian"
  Data.s "hope: gav, jon, bob, abe, ian, dan, hal, ed, col, fred"
  Data.s "ivy: ian, col, hal, gav, fred, bob, abe, ed, jon, dan"
  Data.s "jan: ed, hal, gav, abe, bob, jon, col, ian, fred, dan"
EndDataSection

Structure message
  source.s ;person that message is from
  dest.s   ;person that message is for
  action.s ;{'P', 'A', 'D', 'B'} for proposal, accept, decline, break-up
EndStructure

Structure person
  name.s
  isEngagedTo.s
  List prefs.s()
EndStructure

Global NewList messages.message()

Procedure setupPersons(List persons.person(), count)
  Protected i, j, start, pref$
  For i = 1 To count
    Read.s pref$
    pref$ = LCase(pref$)
    start = FindString(pref$, ":", 1)
    AddElement(persons())
    persons()\name = Left(pref$, start - 1)
    pref$ = Trim(Right(pref$, Len(pref$) - start))
    For j = 1 To count
      AddElement(persons()\prefs())
      persons()\prefs() = Trim(StringField(pref$, j, ","))
    Next
  Next
EndProcedure

Procedure sendMessage(source.s, dest.s, action.s)
  LastElement(messages())
  AddElement(messages())
  With messages()
    \source = source
    \dest = dest
    \action = action
  EndWith
  ResetList(messages())
EndProcedure

Procedure selectPerson(name.s, List persons.person())
  ForEach persons()
    If persons()\name = name
      Break
    EndIf
  Next
EndProcedure

Procedure rankPerson(name.s, List prefs.s())
  ForEach prefs()
    If prefs() = name
      ProcedureReturn #coupleCount - ListIndex(prefs()) ;higher is better
    EndIf
  Next
  ProcedureReturn -1 ;no rank, shouldn't occur
EndProcedure

Procedure stabilityCheck(List guys.person(), List gals.person())
  Protected isStable = #True
  ForEach guys()
    rankPerson(guys()\isEngagedTo, guys()\prefs())
    While PreviousElement(guys()\prefs())
      selectPerson(guys()\prefs(), gals())
      If rankPerson(guys()\name, gals()\prefs()) > rankPerson(gals()\isEngagedTo, gals()\prefs())
        Print("  " + gals()\name + " loves " + guys()\name + " more than " + gals()\isEngagedTo + ",")
        PrintN(" And " + guys()\name + " loves " + gals()\name + " more than " + guys()\isEngagedTo + ".")
        isStable = #False
      EndIf
    Wend
  Next
  If isStable
    PrintN(#CRLF$ + "Marriage stability check PASSED.")
  Else
    PrintN(#CRLF$ + "Marriage stability check FAILED.")
  EndIf
EndProcedure

NewList guys.person()
NewList gals.person()
setupPersons(guys(), #coupleCount)
setupPersons(gals(), #coupleCount)

;make initial round of proposals
ForEach guys()
  FirstElement(guys()\prefs())
  sendMessage(guys()\name, guys()\prefs(), "P")
Next

;dispatch messages
Define source.s, dest.s, action.s
ForEach messages()
  source = messages()\source
  dest = messages()\dest
  action = messages()\action

  DeleteElement(messages())
  Select action
    Case "P" ;propose ;only message received by gals
      selectPerson(dest, gals())
      selectPerson(source, guys())
      If rankPerson(guys()\name, gals()\prefs()) < rankPerson(gals()\isEngagedTo, gals()\prefs())
        sendMessage(dest, source, "D") ;decline proposal
      ElseIf rankPerson(guys()\name, gals()\prefs()) > rankPerson(gals()\isEngagedTo, gals()\prefs())
        If gals()\isEngagedTo
          sendMessage(dest, gals()\isEngagedTo, "B")  ;break-up engagement
        EndIf
        gals()\isEngagedTo = source
        sendMessage(dest, source, "A") ;accept proposal
      EndIf
    Case "A", "D", "B" ;messages received by guys
      selectPerson(dest, guys())
      If action = "A" ;proposal accepted
        guys()\isEngagedTo = source
      Else
        If action = "B" ;broke-up
          guys()\isEngagedTo = ""
        EndIf
        NextElement(guys()\prefs())
        sendMessage(dest, guys()\prefs(),"P") ;propose to next pref
      EndIf
  EndSelect
Next

If OpenConsole()
  PrintN("Marriages:")
  ForEach guys()
    PrintN("  " + guys()\name + " And " + guys()\isEngagedTo + ".")
  Next
  stabilityCheck(guys(), gals())

  Define *person_1.person, *person_2.person
  PrintN(#CRLF$ + "Introducing an error by swapping partners of abi and bea.")
  selectPerson("abi", gals()): *person_1 = @gals()
  selectPerson("bea", gals()): *person_2 = @gals()
  Swap *person_1\isEngagedTo, *person_2\isEngagedTo
  selectPerson(*person_1\isEngagedTo, guys()): *person_1 = @guys()
  selectPerson(*person_1\isEngagedTo, guys()): *person_2 = @guys()
  Swap *person_1\isEngagedTo, *person_2\isEngagedTo
  PrintN("  " + *person_1\name + " is now with " + *person_1\isEngagedTo + ".")
  PrintN("  " + *person_2\name + " is now with " + *person_2\isEngagedTo + ".")
  PrintN("")
  stabilityCheck(guys(), gals())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt
Marriages:
  abe And ivy.
  bob And cath.
  col And dee.
  dan And fay.
  ed And jan.
  fred And bea.
  gav And gay.
  hal And eve.
  ian And hope.
  jon And abi.

Marriage stability check PASSED.

Introducing an error by swapping partners of abi and bea.
  fred is now with abi.
  jon is now with bea.

  bea loves fred more than jon, And fred loves bea more than abi.
  eve loves jon more than hal, And jon loves eve more than bea.
  gay loves jon more than gav, And jon loves gay more than bea.
  fay loves jon more than dan, And jon loves fay more than bea.

Marriage stability check FAILED.
```



## Python


```python
import copy

guyprefers = {
 'abe':  ['abi', 'eve', 'cath', 'ivy', 'jan', 'dee', 'fay', 'bea', 'hope', 'gay'],
 'bob':  ['cath', 'hope', 'abi', 'dee', 'eve', 'fay', 'bea', 'jan', 'ivy', 'gay'],
 'col':  ['hope', 'eve', 'abi', 'dee', 'bea', 'fay', 'ivy', 'gay', 'cath', 'jan'],
 'dan':  ['ivy', 'fay', 'dee', 'gay', 'hope', 'eve', 'jan', 'bea', 'cath', 'abi'],
 'ed':   ['jan', 'dee', 'bea', 'cath', 'fay', 'eve', 'abi', 'ivy', 'hope', 'gay'],
 'fred': ['bea', 'abi', 'dee', 'gay', 'eve', 'ivy', 'cath', 'jan', 'hope', 'fay'],
 'gav':  ['gay', 'eve', 'ivy', 'bea', 'cath', 'abi', 'dee', 'hope', 'jan', 'fay'],
 'hal':  ['abi', 'eve', 'hope', 'fay', 'ivy', 'cath', 'jan', 'bea', 'gay', 'dee'],
 'ian':  ['hope', 'cath', 'dee', 'gay', 'bea', 'abi', 'fay', 'ivy', 'jan', 'eve'],
 'jon':  ['abi', 'fay', 'jan', 'gay', 'eve', 'bea', 'dee', 'cath', 'ivy', 'hope']}
galprefers = {
 'abi':  ['bob', 'fred', 'jon', 'gav', 'ian', 'abe', 'dan', 'ed', 'col', 'hal'],
 'bea':  ['bob', 'abe', 'col', 'fred', 'gav', 'dan', 'ian', 'ed', 'jon', 'hal'],
 'cath': ['fred', 'bob', 'ed', 'gav', 'hal', 'col', 'ian', 'abe', 'dan', 'jon'],
 'dee':  ['fred', 'jon', 'col', 'abe', 'ian', 'hal', 'gav', 'dan', 'bob', 'ed'],
 'eve':  ['jon', 'hal', 'fred', 'dan', 'abe', 'gav', 'col', 'ed', 'ian', 'bob'],
 'fay':  ['bob', 'abe', 'ed', 'ian', 'jon', 'dan', 'fred', 'gav', 'col', 'hal'],
 'gay':  ['jon', 'gav', 'hal', 'fred', 'bob', 'abe', 'col', 'ed', 'dan', 'ian'],
 'hope': ['gav', 'jon', 'bob', 'abe', 'ian', 'dan', 'hal', 'ed', 'col', 'fred'],
 'ivy':  ['ian', 'col', 'hal', 'gav', 'fred', 'bob', 'abe', 'ed', 'jon', 'dan'],
 'jan':  ['ed', 'hal', 'gav', 'abe', 'bob', 'jon', 'col', 'ian', 'fred', 'dan']}

guys = sorted(guyprefers.keys())
gals = sorted(galprefers.keys())


def check(engaged):
    inverseengaged = dict((v,k) for k,v in engaged.items())
    for she, he in engaged.items():
        shelikes = galprefers[she]
        shelikesbetter = shelikes[:shelikes.index(he)]
        helikes = guyprefers[he]
        helikesbetter = helikes[:helikes.index(she)]
        for guy in shelikesbetter:
            guysgirl = inverseengaged[guy]
            guylikes = guyprefers[guy]
            if guylikes.index(guysgirl) > guylikes.index(she):
                print("%s and %s like each other better than "
                      "their present partners: %s and %s, respectively"
                      % (she, guy, he, guysgirl))
                return False
        for gal in helikesbetter:
            girlsguy = engaged[gal]
            gallikes = galprefers[gal]
            if gallikes.index(girlsguy) > gallikes.index(he):
                print("%s and %s like each other better than "
                      "their present partners: %s and %s, respectively"
                      % (he, gal, she, girlsguy))
                return False
    return True

def matchmaker():
    guysfree = guys[:]
    engaged  = {}
    guyprefers2 = copy.deepcopy(guyprefers)
    galprefers2 = copy.deepcopy(galprefers)
    while guysfree:
        guy = guysfree.pop(0)
        guyslist = guyprefers2[guy]
        gal = guyslist.pop(0)
        fiance = engaged.get(gal)
        if not fiance:
            # She's free
            engaged[gal] = guy
            print("  %s and %s" % (guy, gal))
        else:
            # The bounder proposes to an engaged lass!
            galslist = galprefers2[gal]
            if galslist.index(fiance) > galslist.index(guy):
                # She prefers new guy
                engaged[gal] = guy
                print("  %s dumped %s for %s" % (gal, fiance, guy))
                if guyprefers2[fiance]:
                    # Ex has more girls to try
                    guysfree.append(fiance)
            else:
                # She is faithful to old fiance
                if guyslist:
                    # Look again
                    guysfree.append(guy)
    return engaged


print('\nEngagements:')
engaged = matchmaker()

print('\nCouples:')
print('  ' + ',\n  '.join('%s is engaged to %s' % couple
                          for couple in sorted(engaged.items())))
print()
print('Engagement stability check PASSED'
      if check(engaged) else 'Engagement stability check FAILED')

print('\n\nSwapping two fiances to introduce an error')
engaged[gals[0]], engaged[gals[1]] = engaged[gals[1]], engaged[gals[0]]
for gal in gals[:2]:
    print('  %s is now engaged to %s' % (gal, engaged[gal]))
print()
print('Engagement stability check PASSED'
      if check(engaged) else 'Engagement stability check FAILED')
```

{{out}}

```txt
Engagements:
  abe and abi
  bob and cath
  col and hope
  dan and ivy
  ed and jan
  fred and bea
  gav and gay
  hope dumped col for ian
  abi dumped abe for jon
  hal and eve
  col and dee
  ivy dumped dan for abe
  dan and fay

Couples:
  abi is engaged to jon,
  bea is engaged to fred,
  cath is engaged to bob,
  dee is engaged to col,
  eve is engaged to hal,
  fay is engaged to dan,
  gay is engaged to gav,
  hope is engaged to ian,
  ivy is engaged to abe,
  jan is engaged to ed

Engagement stability check PASSED


Swapping two fiances to introduce an error
  abi is now engaged to fred
  bea is now engaged to jon

fay and jon like each other better than their present partners: dan and bea, respectively
Engagement stability check FAILED
```



## Ruby


```ruby
class Person
  def initialize(name)
    @name = name
    @fiance = nil
    @preferences = []
    @proposals = []
  end
  attr_reader :name, :proposals
  attr_accessor :fiance, :preferences

  def to_s
    @name
  end

  def free
    @fiance = nil
  end

  def single?
    @fiance == nil
  end

  def engage(person)
    self.fiance = person
    person.fiance = self
  end

  def better_choice?(person)
    @preferences.index(person) < @preferences.index(@fiance)
  end

  def propose_to(person)
    puts "#{self} proposes to #{person}" if $DEBUG
    @proposals << person
    person.respond_to_proposal_from(self)
  end

  def respond_to_proposal_from(person)
    if single?
      puts "#{self} accepts proposal from #{person}" if $DEBUG
      engage(person)
    elsif better_choice?(person)
      puts "#{self} dumps #{@fiance} for #{person}" if $DEBUG
      @fiance.free
      engage(person)
    else
      puts "#{self} rejects proposal from #{person}" if $DEBUG
    end
  end
end

########################################################################
# initialize data

prefs = {
  'abe'  => %w[abi eve cath ivy jan dee fay bea hope gay],
  'bob'  => %w[cath hope abi dee eve fay bea jan ivy gay],
  'col'  => %w[hope eve abi dee bea fay ivy gay cath jan],
  'dan'  => %w[ivy fay dee gay hope eve jan bea cath abi],
  'ed'   => %w[jan dee bea cath fay eve abi ivy hope gay],
  'fred' => %w[bea abi dee gay eve ivy cath jan hope fay],
  'gav'  => %w[gay eve ivy bea cath abi dee hope jan fay],
  'hal'  => %w[abi eve hope fay ivy cath jan bea gay dee],
  'ian'  => %w[hope cath dee gay bea abi fay ivy jan eve],
  'jon'  => %w[abi fay jan gay eve bea dee cath ivy hope],
  'abi'  => %w[bob fred jon gav ian abe dan ed col hal],
  'bea'  => %w[bob abe col fred gav dan ian ed jon hal],
  'cath' => %w[fred bob ed gav hal col ian abe dan jon],
  'dee'  => %w[fred jon col abe ian hal gav dan bob ed],
  'eve'  => %w[jon hal fred dan abe gav col ed ian bob],
  'fay'  => %w[bob abe ed ian jon dan fred gav col hal],
  'gay'  => %w[jon gav hal fred bob abe col ed dan ian],
  'hope' => %w[gav jon bob abe ian dan hal ed col fred],
  'ivy'  => %w[ian col hal gav fred bob abe ed jon dan],
  'jan'  => %w[ed hal gav abe bob jon col ian fred dan],
}

@men = Hash[
  %w[abe bob col dan ed fred gav hal ian jon].collect do |name|
    [name, Person.new(name)]
  end
]

@women = Hash[
  %w[abi bea cath dee eve fay gay hope ivy jan].collect do |name|
    [name, Person.new(name)]
  end
]

@men.each {|name, man| man.preferences = @women.values_at(*prefs[name])}
@women.each {|name, woman| woman.preferences = @men.values_at(*prefs[name])}

########################################################################
# perform the matching

def match_couples(men, women)
  men.each_value {|man| man.free}
  women.each_value {|woman| woman.free}

  while m = men.values.find {|man| man.single?} do
    puts "considering single man #{m}" if $DEBUG
    w = m.preferences.find {|woman| not m.proposals.include?(woman)}
    m.propose_to(w)
  end
end

match_couples @men, @women

@men.each_value.collect {|man| puts "#{man} + #{man.fiance}"}

########################################################################
# check for stability

class Person
  def more_preferable_people
    ( @preferences.partition {|p| better_choice?(p)} ).first
  end
end

require 'set'

def stability(men)
  unstable = Set.new
  men.each_value do |man|
    woman = man.fiance
    puts "considering #{man} and #{woman}" if $DEBUG

    man.more_preferable_people.each do |other_woman|
      if other_woman.more_preferable_people.include?(man)
        puts "an unstable pairing: #{man} and #{other_woman}" if $DEBUG
        unstable << [man, other_woman]
      end
    end
    woman.more_preferable_people.each do |other_man|
      if other_man.more_preferable_people.include?(woman)
        puts "an unstable pairing: #{woman} and #{other_man}" if $DEBUG
        unstable << [other_man, woman]
      end
    end
  end

  if unstable.empty?
    puts "these couples are stable"
  else
    puts "uh oh"
    unstable.each do |a,b|
      puts "#{a} is engaged to #{a.fiance} but would prefer #{b}, and #{b} is engaged to #{b.fiance} but would prefer #{a}"
    end
  end
end

stability @men

########################################################################
# perturb

puts "\nwhat if abe and bob swap..."

def swap(m1, m2)
  w1 = m1.fiance
  w2 = m2.fiance
  m1.fiance = w2
  w1.fiance = m2
  m2.fiance = w1
  w2.fiance = m1
end

swap *@men.values_at('abe','bob')

@men.each_value.collect {|man| puts "#{man} + #{man.fiance}"}
stability @men
```

{{out}}

```txt
abe + ivy
bob + cath
col + dee
dan + fay
ed + jan
fred + bea
gav + gay
hal + eve
ian + hope
jon + abi
these couples are stable

what if abe and bob swap...
abe + cath
bob + ivy
col + dee
dan + fay
ed + jan
fred + bea
gav + gay
hal + eve
ian + hope
jon + abi
uh oh
bob is engaged to ivy but would prefer cath, and cath is engaged to abe but would prefer bob
bob is engaged to ivy but would prefer hope, and hope is engaged to ian but would prefer bob
bob is engaged to ivy but would prefer abi, and abi is engaged to jon but would prefer bob
bob is engaged to ivy but would prefer fay, and fay is engaged to dan but would prefer bob
bob is engaged to ivy but would prefer bea, and bea is engaged to fred but would prefer bob
```


Hmm, turns out Bob is a popular guy...


## Racket


```Racket

#lang racket

(define MEN
  '([abe  abi  eve  cath ivy  jan  dee  fay  bea  hope gay ]
    [bob  cath hope abi  dee  eve  fay  bea  jan  ivy  gay ]
    [col  hope eve  abi  dee  bea  fay  ivy  gay  cath jan ]
    [dan  ivy  fay  dee  gay  hope eve  jan  bea  cath abi ]
    [ed   jan  dee  bea  cath fay  eve  abi  ivy  hope gay ]
    [fred bea  abi  dee  gay  eve  ivy  cath jan  hope fay ]
    [gav  gay  eve  ivy  bea  cath abi  dee  hope jan  fay ]
    [hal  abi  eve  hope fay  ivy  cath jan  bea  gay  dee ]
    [ian  hope cath dee  gay  bea  abi  fay  ivy  jan  eve ]
    [jon  abi  fay  jan  gay  eve  bea  dee  cath ivy  hope]))
(define WOMEN
  '([abi  bob  fred jon  gav  ian  abe  dan  ed   col  hal ]
    [bea  bob  abe  col  fred gav  dan  ian  ed   jon  hal ]
    [cath fred bob  ed   gav  hal  col  ian  abe  dan  jon ]
    [dee  fred jon  col  abe  ian  hal  gav  dan  bob  ed  ]
    [eve  jon  hal  fred dan  abe  gav  col  ed   ian  bob ]
    [fay  bob  abe  ed   ian  jon  dan  fred gav  col  hal ]
    [gay  jon  gav  hal  fred bob  abe  col  ed   dan  ian ]
    [hope gav  jon  bob  abe  ian  dan  hal  ed   col  fred]
    [ivy  ian  col  hal  gav  fred bob  abe  ed   jon  dan ]
    [jan  ed   hal  gav  abe  bob  jon  col  ian  fred dan ]))

;; x is better than y according to the l order
(define (better? x y l) (memq y (cdr (memq x l))))

(define (stable-matching Mprefs Wprefs)
  (define M (map car Mprefs))
  (define engagements (make-hasheq))
  (define preferences (make-hasheq))
  (define (engage! m w)
    (hash-set! engagements m w)
    (hash-set! engagements w m))
  (for ([m Mprefs]) (hash-set! preferences (car m) (cdr m)))
  (for ([w Wprefs]) (hash-set! preferences (car w) (cdr w)))
  (let loop ()
    (define m+w
      (for/or ([m M])
        (and (not (hash-ref engagements m #f))   ; m is free
             (let ([p (hash-ref preferences m)]) ; can propose
               (and (pair? p)
                    (let ([w (car p)])
                      (hash-set! preferences m (cdr p)) ; drop w from m
                      (cons m w)))))))
    (when m+w
      (define m (car m+w))
      (define w (cdr m+w))
      (define m* (hash-ref engagements w #f))    ; m* is w's prev engagement
      (cond [(not m*) (engage! m w)]             ; w is free
            [(better? m m* (hash-ref preferences w)) ; w prefers m over m*
             (engage! m w)
             (hash-set! engagements m* #f)])     ; m* becomes free
      (loop)))
  engagements)

(define (find-unstable Mprefs Wprefs matches)
  (for*/or ([m (map car Mprefs)] [w (map car Wprefs)])
    (define w* (hash-ref matches m))
    (define m* (hash-ref matches w))
    (and (not (eq? m m*))
         (better? w w* (cdr (assq m Mprefs)))
         (better? m m* (cdr (assq w Wprefs)))
         (cons m w))))

(define (check-stability)
  (let ([u (find-unstable MEN WOMEN matches)])
    (if u
      (printf "Unstable: ~a and ~a prefer each other over partners.\n"
              (car u) (cdr u))
      (printf "The match is stable.\n"))))
(define matches (stable-matching MEN WOMEN))
(printf "Found matches:\n")
(for ([m (map car MEN)]) (printf "  ~a, ~a\n" m (hash-ref matches m)))
(check-stability)
(let ([M (map car (take (shuffle MEN) 2))])
  (printf "Swapping wives of ~a and ~a\n" (car M) (cadr M))
  (define (swap! x y)
    (define t (hash-ref matches x))
    (hash-set! matches x (hash-ref matches y))
    (hash-set! matches y t))
  (swap! (car M) (cadr M))
  (swap! (hash-ref matches (car M)) (hash-ref matches (cadr M))))
(check-stability)

```


Sample run:

```txt

Found matches:
  abe, ivy
  bob, cath
  col, dee
  dan, fay
  ed, jan
  fred, bea
  gav, gay
  hal, eve
  ian, hope
  jon, abi
The match is stable.
Swapping wives of bob and gav
Unstable: bob and abi prefer each other over partners.

```



## REXX

Algorithm used: see link https://www.youtube.com/watch?v=Qcv1IqHWAzg

```REXX
/*- REXX --------------------------------------------------------------
* pref.b  Preferences of boy b
* pref.g  Preferences of girl g
* boys    List of boys
* girls   List of girls
* plist   List of proposals
* mlist   List of (current) matches
* glist   List of girls to be matched
* glist.b List of girls that proposed to boy b
* blen    maximum length of boys' names
* glen    maximum length of girls' names
---------------------------------------------------------------------*/

pref.Charlotte=translate('Bingley Darcy Collins Wickham ')
pref.Elisabeth=translate('Wickham Darcy Bingley Collins ')
pref.Jane     =translate('Bingley Wickham Darcy Collins ')
pref.Lydia    =translate('Bingley Wickham Darcy Collins ')

pref.Bingley  =translate('Jane Elisabeth Lydia Charlotte')
pref.Collins  =translate('Jane Elisabeth Lydia Charlotte')
pref.Darcy    =translate('Elisabeth Jane Charlotte Lydia')
pref.Wickham  =translate('Lydia Jane Elisabeth Charlotte')

pref.ABE='ABI EVE CATH IVY JAN DEE FAY BEA HOPE GAY'
pref.BOB='CATH HOPE ABI DEE EVE FAY BEA JAN IVY GAY'
pref.COL='HOPE EVE ABI DEE BEA FAY IVY GAY CATH JAN'
pref.DAN='IVY FAY DEE GAY HOPE EVE JAN BEA CATH ABI'
pref.ED='JAN DEE BEA CATH FAY EVE ABI IVY HOPE GAY'
pref.FRED='BEA ABI DEE GAY EVE IVY CATH JAN HOPE FAY'
pref.GAV='GAY EVE IVY BEA CATH ABI DEE HOPE JAN FAY'
pref.HAL='ABI EVE HOPE FAY IVY CATH JAN BEA GAY DEE'
pref.IAN='HOPE CATH DEE GAY BEA ABI FAY IVY JAN EVE'
pref.JON='ABI FAY JAN GAY EVE BEA DEE CATH IVY HOPE'

pref.ABI='BOB FRED JON GAV IAN ABE DAN ED COL HAL'
pref.BEA='BOB ABE COL FRED GAV DAN IAN ED JON HAL'
pref.CATH='FRED BOB ED GAV HAL COL IAN ABE DAN JON'
pref.DEE='FRED JON COL ABE IAN HAL GAV DAN BOB ED'
pref.EVE='JON HAL FRED DAN ABE GAV COL ED IAN BOB'
pref.FAY='BOB ABE ED IAN JON DAN FRED GAV COL HAL'
pref.GAY='JON GAV HAL FRED BOB ABE COL ED DAN IAN'
pref.HOPE='GAV JON BOB ABE IAN DAN HAL ED COL FRED'
pref.IVY='IAN COL HAL GAV FRED BOB ABE ED JON DAN'
pref.JAN='ED HAL GAV ABE BOB JON COL IAN FRED DAN'

If arg(1)>'' Then Do
  Say 'Input from task description'
  boys='ABE BOB COL DAN ED FRED GAV HAL IAN JON'
  girls='ABI BEA CATH DEE EVE FAY GAY HOPE IVY JAN'
  End
Else Do
  Say 'Input from link'
  girls=translate('Charlotte Elisabeth Jane  Lydia')
  boys =translate('Bingley   Collins   Darcy Wickham')
  End

debug=0
blen=0
Do i=1 To words(boys)
  blen=max(blen,length(word(boys,i)))
  End
glen=0
Do i=1 To words(girls)
  glen=max(glen,length(word(girls,i)))
  End
glist=girls
mlist=''
Do ri=1 By 1 Until glist=''            /* as long as there are girls */
  Call dbg 'Round' ri
  plist=''                             /* no proposals in this round */
  glist.=''
  Do gi=1 To words(glist)              /* loop over free girls       */
    gg=word(glist,gi)                  /* an unmathed girl           */
    b=word(pref.gg,1)                  /* her preferred boy          */
    plist=plist gg'-'||b               /* remember this proposal     */
    glist.b=glist.b gg                 /* add girl to the boy's list */
    Call dbg left(gg,glen) 'proposes to' b  /* tell the user              */
    End
  Do bi=1 To words(boys)               /* loop over all boys         */
    b=word(boys,bi)                    /* one of them                */
    If glist.b>'' Then                 /* if he's got proposals      */
      Call dbg b 'has these proposals' glist.b /* show them               */
    End
  Do bi=1 To words(boys)               /* loop over all boys         */
    b=word(boys,bi)                    /* one of them                */
    bm=pos(b'*',mlist)                 /* has he been matched yet?   */
    Select
      When words(glist.b)=1 Then Do    /* one girl proposed for him  */
        gg=word(glist.b,1)             /* the proposing girl         */
        If bm=0 Then Do                /* no, he hasn't              */
          Call dbg b 'accepts' gg           /* is accepted                */
          Call set_mlist 'A',mlist b||'*'||gg /* add match to mlist  */
          Call set_glist 'A',remove(gg,glist) /* remove gg from glist*/
          pref.gg=remove(b,pref.gg)    /* remove b from gg's preflist*/
          End
        Else Do                        /* boy has been matched       */
          Parse Var mlist =(bm) '*' go ' ' /* to girl go             */
          If wordpos(gg,pref.b)<wordpos(go,pref.b) Then Do
               /* the proposing girl is preferred to the current one */
                                       /* so we replace go by gg     */
            Call set_mlist 'B',repl(mlist,b||'*'||gg,b||'*'||go)
            Call dbg b 'releases' go
            Call dbg b 'accepts ' gg
            Call set_glist 'B',glist go  /* add go to list of girls  */
            Call set_glist 'C',remove(gg,glist) /* and remove gg     */
            End
          pref.gg=remove(b,pref.gg)    /* remove b from gg's preflist*/
          End
        End
      When words(glist.b)>1 Then
        Call pick_1
      Otherwise Nop
      End
    End
  Call dbg 'Matches   :' mlist
  Call dbg 'free girls:' glist
  Call check 'L'
  End
Say 'Success at round' (ri-1)
Do While mlist>''
  Parse Var mlist boy '*' girl mlist
  Say left(boy,blen) 'matches' girl
  End
Exit

pick_1:
  If bm>0 Then Do                /* boy has been matched       */
    Parse Var mlist =(bm) '*' go ' ' /* to girl go             */
    pmin=wordpos(go,pref.b)
    End
  Else Do
    go=''
    pmin=99
    End
  Do gi=1 To words(glist.b)
    gt=word(glist.b,gi)
    gp=wordpos(gt,pref.b)
    If gp<pmin Then Do
      pmin=gp
      gg=gt
      End
    End
  If bm=0 Then Do
    Call dbg b 'accepts' gg           /* is accepted                */
    Call set_mlist 'A',mlist b||'*'||gg /* add match to mlist  */
    Call set_glist 'A',remove(gg,glist) /* remove gg from glist*/
    pref.gg=remove(b,pref.gg)    /* remove b from gg's preflist*/
    End
  Else Do
    If gg<>go Then Do
      Call set_mlist 'B',repl(mlist,b||'*'||gg,b||'*'||go)
      Call dbg b 'releases' go
      Call dbg b 'accepts ' gg
      Call set_glist 'B',glist go  /* add go to list of girls  */
      Call set_glist 'C',remove(gg,glist) /* and remove gg     */
      pref.gg=remove(b,pref.gg)  /* remove b from gg's preflist*/
      End
    End
  Return

remove:
  Parse Arg needle,haystack
  pp=pos(needle,haystack)
  If pp>0 Then
    res=left(haystack,pp-1) substr(haystack,pp+length(needle))
  Else
    res=haystack
  Return space(res)

set_mlist:
  Parse Arg where,new_mlist
  Call dbg 'set_mlist' where':' mlist
  mlist=space(new_mlist)
  Call dbg 'set_mlist ->' mlist
  Call dbg ''
  Return

set_glist:
  Parse Arg where,new_glist
  Call dbg 'set_glist' where':' glist
  glist=new_glist
  Call dbg 'set_glist ->' glist
  Call dbg ''
  Return

check:
  If words(mlist)+words(glist)<>words(boys) Then Do
    Call dbg 'FEHLER bei' arg(1) (words(mlist)+words(glist))'<>10'
    say 'match='mlist'<'
    say '   glist='glist'<'
    End
  Return

dbg:
  If debug Then
    Call dbg arg(1)
  Return
repl: Procedure
  Parse Arg s,new,old
  Do i=1 To 100 Until p=0
    p=pos(old,s)
    If p>0 Then
      s=left(s,p-1)||new||substr(s,p+length(old))
    End
  Return s
```

{{out}}

```txt

D:\>rexx smxa
Input from link
Success at round 5
BINGLEY matches LYDIA
WICKHAM matches JANE
DARCY   matches ELISABETH
COLLINS matches CHARLOTTE

D:\>rexx smxa .
Input from task description
Success at round 22
BOB  matches CATH
ED   matches JAN
FRED matches BEA
GAV  matches GAY
IAN  matches HOPE
JON  matches ABI
HAL  matches EVE
ABE  matches IVY
COL  matches DEE
DAN  matches FAY
```



## Scala

{{trans|Java}}

```scala
object SMP extends App {
    private def checkMarriages(): Unit =
        if (check)
            println("Marriages are stable")
        else
            println("Marriages are unstable")

    private def swap() {
        val girl1 = girls.head
        val girl2 = girls(1)
        val tmp = girl2 -> matches(girl1)
        matches += girl1 -> matches(girl2)
        matches += tmp
        println(girl1 + " and " + girl2 + " have switched partners")
    }

    private type TM = scala.collection.mutable.TreeMap[String, String]

    private def check: Boolean = {
        if (!girls.toSet.subsetOf(matches.keySet) || !guys.toSet.subsetOf(matches.values.toSet))
            return false

        val invertedMatches = new TM
        matches foreach { invertedMatches += _.swap }

        for ((k, v) <- matches) {
            val shePrefers = girlPrefers(k)
            val sheLikesBetter = shePrefers.slice(0, shePrefers.indexOf(v))
            val hePrefers = guyPrefers(v)
            val heLikesBetter = hePrefers.slice(0, hePrefers.indexOf(k))

            for (guy <- sheLikesBetter) {
                val fiance = invertedMatches(guy)
                val guy_p = guyPrefers(guy)
                if (guy_p.indexOf(fiance) > guy_p.indexOf(k)) {
                    println(s"$k likes $guy better than $v and $guy likes $k better than their current partner")
                    return false
                }
            }

            for (girl <- heLikesBetter) {
                val fiance = matches(girl)
                val girl_p = girlPrefers(girl)
                if (girl_p.indexOf(fiance) > girl_p.indexOf(v)) {
                    println(s"$v likes $girl better than $k and $girl likes $v better than their current partner")
                    return false
                }
            }
        }
        true
    }

    private val guys = "abe" :: "bob" :: "col" :: "dan" :: "ed" :: "fred" :: "gav" :: "hal" :: "ian" :: "jon" :: Nil
    private val girls = "abi" :: "bea" :: "cath" :: "dee" :: "eve" :: "fay" :: "gay" :: "hope" :: "ivy" :: "jan" :: Nil
    private val guyPrefers = Map("abe" -> List("abi", "eve", "cath", "ivy", "jan", "dee", "fay", "bea", "hope", "gay"),
        "bob" -> List("cath", "hope", "abi", "dee", "eve", "fay", "bea", "jan", "ivy", "gay"),
        "col" -> List("hope", "eve", "abi", "dee", "bea", "fay", "ivy", "gay", "cath", "jan"),
        "dan" -> List("ivy", "fay", "dee", "gay", "hope", "eve", "jan", "bea", "cath", "abi"),
        "ed" -> List("jan", "dee", "bea", "cath", "fay", "eve", "abi", "ivy", "hope", "gay"),
        "fred" -> List("bea", "abi", "dee", "gay", "eve", "ivy", "cath", "jan", "hope", "fay"),
        "gav" -> List("gay", "eve", "ivy", "bea", "cath", "abi", "dee", "hope", "jan", "fay"),
        "hal" -> List("abi", "eve", "hope", "fay", "ivy", "cath", "jan", "bea", "gay", "dee"),
        "ian" -> List("hope", "cath", "dee", "gay", "bea", "abi", "fay", "ivy", "jan", "eve"),
        "jon" -> List("abi", "fay", "jan", "gay", "eve", "bea", "dee", "cath", "ivy", "hope"))
    private val girlPrefers = Map("abi" -> List("bob", "fred", "jon", "gav", "ian", "abe", "dan", "ed", "col", "hal"),
        "bea" -> List("bob", "abe", "col", "fred", "gav", "dan", "ian", "ed", "jon", "hal"),
        "cath" -> List("fred", "bob", "ed", "gav", "hal", "col", "ian", "abe", "dan", "jon"),
        "dee" -> List("fred", "jon", "col", "abe", "ian", "hal", "gav", "dan", "bob", "ed"),
        "eve" -> List("jon", "hal", "fred", "dan", "abe", "gav", "col", "ed", "ian", "bob"),
        "fay" -> List("bob", "abe", "ed", "ian", "jon", "dan", "fred", "gav", "col", "hal"),
        "gay" -> List("jon", "gav", "hal", "fred", "bob", "abe", "col", "ed", "dan", "ian"),
        "hope" -> List("gav", "jon", "bob", "abe", "ian", "dan", "hal", "ed", "col", "fred"),
        "ivy" -> List("ian", "col", "hal", "gav", "fred", "bob", "abe", "ed", "jon", "dan"),
        "jan" -> List("ed", "hal", "gav", "abe", "bob", "jon", "col", "ian", "fred", "dan"))

    private lazy val matches = {
        val engagements = new TM
        val freeGuys = scala.collection.mutable.Queue.empty ++ guys
        while (freeGuys.nonEmpty) {
            val guy = freeGuys.dequeue()
            val guy_p = guyPrefers(guy)
            var break = false
            for (girl <- guy_p)
                if (!break)
                    if (!engagements.contains(girl)) {
                        engagements(girl) = guy
                        break = true
                    }
                    else {
                        val other_guy = engagements(girl)
                        val girl_p = girlPrefers(girl)
                        if (girl_p.indexOf(guy) < girl_p.indexOf(other_guy)) {
                            engagements(girl) = guy
                            freeGuys += other_guy
                            break = true
                        }
                    }
        }

        engagements foreach { e => println(s"${e._1} is engaged to ${e._2}") }
        engagements
    }

    checkMarriages()
    swap()
    checkMarriages()
}
```

{{out}}
See Java output.


## Seed7


```seed7
$ include "seed7_05.s7i";

const type: preferences is hash [string] array string;
const type: engaged is hash [string] string;

const func integer: pos (in array string: area, in string: searched) is func
  result
    var integer: pos is 1;
  begin
    while pos <= length(area) and area[pos] <> searched do
      incr(pos);
    end while;
    if pos > length(area) then
      pos := 0;
    end if;
  end func;

const func engaged: matchmaker (in preferences: guyPrefers, in preferences: girlPrefers) is func
  result
    var engaged: engagedTo is engaged.value;
  local
    var array string: freeGuys is 0 times "";
    var string: guy is "";
    var string: girl is "";
    var string: fiance is "";
    var array string: guyPreferencList is 0 times "";
    var array string: girlPreferenceList is 0 times "";
    var boolean: searching is TRUE;
  begin
    freeGuys := sort(keys(guyPrefers));
    while length(freeGuys) <> 0 do
      guy := freeGuys[1];
      freeGuys := freeGuys[2 ..];
      guyPreferencList := guyPrefers[guy];
      searching := TRUE;
      while searching and length(guyPreferencList) <> 0 do
        girl := guyPreferencList[1];
        guyPreferencList := guyPreferencList[2 ..];
        if girl not in engagedTo then
          engagedTo @:= [girl] guy;
          writeln("  " <& girl <& " and " <& guy);
          searching := FALSE;
        else
          fiance := engagedTo[girl];
          girlPreferenceList := girlPrefers[girl];
          if pos(girlPreferenceList, guy) < pos(girlPreferenceList, fiance) then
            # She prefers new guy
            engagedTo @:= [girl] guy;
            freeGuys &:= fiance;
            writeln("  " <& girl <& " dumped " <& fiance <& " for " <& guy);
            searching := FALSE;
          end if;
        end if;
      end while;
    end while;
  end func;

const func boolean: check (in engaged: engagedTo,
    in preferences: guyPrefers, in preferences: girlPrefers) is func
  result
    var boolean: stable is TRUE;
  local
    var string: he is "";
    var string: she is "";
    var string: guy is "";
    var string: girl is "";
    var engaged: inverseEngaged is engaged.value;
    var array string: sheLikes is 0 times "";
    var array string: sheLikesBetter is 0 times "";
    var array string: heLikes is 0 times "";
    var array string: heLikesBetter is 0 times "";
    var string: guysGirl is "";
    var array string: guyLikes is 0 times "";
    var string: girlsGuy is "";
    var array string: girlLikes is 0 times "";
  begin
    for he key she range engagedTo do
      inverseEngaged @:= [he] she;
    end for;
    for he key she range engagedTo do
      sheLikes := girlPrefers[she];
      sheLikesBetter := sheLikes[.. pred(pos(sheLikes, he))];
      heLikes := guyPrefers[he];
      heLikesBetter := heLikes[.. pred(pos(heLikes, she))];
      for guy range sheLikesBetter do
        guysGirl := inverseEngaged[guy];
        guyLikes := guyPrefers[guy];
        if pos(guyLikes, guysGirl) > pos(guyLikes, she) and stable then
          writeln(she <& " likes " <& guy <& " better than " <& he <& " and " <&
                  guy <& " likes " <& she <& " better than their current partner");
          stable := FALSE;
        end if;
      end for;
      for girl range heLikesBetter do
        girlsGuy := engagedTo[girl];
        girlLikes := girlPrefers[girl];
        if pos(girlLikes, girlsGuy) > pos(girlLikes, he) and stable then
          writeln(he <& " likes " <& girl <& " better than " <& she <& " and " <&
                  girl <& " likes " <& he <& " better than their current partner");
          stable := FALSE;
        end if;
      end for;
    end for;
  end func;

var preferences: guyPrefers is preferences.value;
var preferences: girlPrefers is preferences.value;
guyPrefers @:= ["abe"]  [] ("abi", "eve", "cath", "ivy", "jan", "dee", "fay", "bea", "hope", "gay");
guyPrefers @:= ["bob"]  [] ("cath", "hope", "abi", "dee", "eve", "fay", "bea", "jan", "ivy", "gay");
guyPrefers @:= ["col"]  [] ("hope", "eve", "abi", "dee", "bea", "fay", "ivy", "gay", "cath", "jan");
guyPrefers @:= ["dan"]  [] ("ivy", "fay", "dee", "gay", "hope", "eve", "jan", "bea", "cath", "abi");
guyPrefers @:= ["ed"]   [] ("jan", "dee", "bea", "cath", "fay", "eve", "abi", "ivy", "hope", "gay");
guyPrefers @:= ["fred"] [] ("bea", "abi", "dee", "gay", "eve", "ivy", "cath", "jan", "hope", "fay");
guyPrefers @:= ["gav"]  [] ("gay", "eve", "ivy", "bea", "cath", "abi", "dee", "hope", "jan", "fay");
guyPrefers @:= ["hal"]  [] ("abi", "eve", "hope", "fay", "ivy", "cath", "jan", "bea", "gay", "dee");
guyPrefers @:= ["ian"]  [] ("hope", "cath", "dee", "gay", "bea", "abi", "fay", "ivy", "jan", "eve");
guyPrefers @:= ["jon"]  [] ("abi", "fay", "jan", "gay", "eve", "bea", "dee", "cath", "ivy", "hope");
girlPrefers @:= ["abi"]  [] ("bob", "fred", "jon", "gav", "ian", "abe", "dan", "ed", "col", "hal");
girlPrefers @:= ["bea"]  [] ("bob", "abe", "col", "fred", "gav", "dan", "ian", "ed", "jon", "hal");
girlPrefers @:= ["cath"] [] ("fred", "bob", "ed", "gav", "hal", "col", "ian", "abe", "dan", "jon");
girlPrefers @:= ["dee"]  [] ("fred", "jon", "col", "abe", "ian", "hal", "gav", "dan", "bob", "ed");
girlPrefers @:= ["eve"]  [] ("jon", "hal", "fred", "dan", "abe", "gav", "col", "ed", "ian", "bob");
girlPrefers @:= ["fay"]  [] ("bob", "abe", "ed", "ian", "jon", "dan", "fred", "gav", "col", "hal");
girlPrefers @:= ["gay"]  [] ("jon", "gav", "hal", "fred", "bob", "abe", "col", "ed", "dan", "ian");
girlPrefers @:= ["hope"] [] ("gav", "jon", "bob", "abe", "ian", "dan", "hal", "ed", "col", "fred");
girlPrefers @:= ["ivy"]  [] ("ian", "col", "hal", "gav", "fred", "bob", "abe", "ed", "jon", "dan");
girlPrefers @:= ["jan"]  [] ("ed", "hal", "gav", "abe", "bob", "jon", "col", "ian", "fred", "dan");

const proc: main is func
  local
    var engaged: engagedTo is engaged.value;
    var string: girl is "";
  begin
    writeln("Matchmaking:");
    engagedTo := matchmaker(guyPrefers, girlPrefers);
    writeln;
    writeln("Engagements:");
    for girl range sort(keys(engagedTo)) do
      writeln("  " <& girl <& " and " <& engagedTo[girl]);
    end for;
    writeln;
    writeln("Marriages are " <& [] ("unstable", "stable") [succ(ord(check(engagedTo, guyPrefers, girlPrefers)))]);
    writeln;
    writeln("Perturb:");
    engagedTo @:= ["abi"] "fred";
    engagedTo @:= ["bea"] "jon";
    writeln("engage abi with fred and bea with jon");
    writeln;
    writeln("Marriages are " <& [] ("unstable", "stable") [succ(ord(check(engagedTo, guyPrefers, girlPrefers)))]);
  end func;
```


Output:

```txt

Matchmaking:
  abi and abe
  cath and bob
  hope and col
  ivy and dan
  jan and ed
  bea and fred
  gay and gav
  eve and hal
  hope dumped col for ian
  abi dumped abe for jon
  dee and col
  ivy dumped dan for abe
  fay and dan

Engagements:
  abi and jon
  bea and fred
  cath and bob
  dee and col
  eve and hal
  fay and dan
  gay and gav
  hope and ian
  ivy and abe
  jan and ed

Marriages are stable

Perturb:
engage abi with fred and bea with jon

bea likes fred better than jon and fred likes bea better than their current partner
Marriages are unstable

```



## Sidef

{{trans|Perl 6}}

```ruby
var he_likes = Hash(
    abe  => < abi eve cath ivy jan dee fay bea hope gay >,
    bob  => < cath hope abi dee eve fay bea jan ivy gay >,
    col  => < hope eve abi dee bea fay ivy gay cath jan >,
    dan  => < ivy fay dee gay hope eve jan bea cath abi >,
    ed   => < jan dee bea cath fay eve abi ivy hope gay >,
    fred => < bea abi dee gay eve ivy cath jan hope fay >,
    gav  => < gay eve ivy bea cath abi dee hope jan fay >,
    hal  => < abi eve hope fay ivy cath jan bea gay dee >,
    ian  => < hope cath dee gay bea abi fay ivy jan eve >,
    jon  => < abi fay jan gay eve bea dee cath ivy hope >,
);

var she_likes = Hash(
    abi  => < bob fred jon gav ian abe dan ed col hal >,
    bea  => < bob abe col fred gav dan ian ed jon hal >,
    cath => < fred bob ed gav hal col ian abe dan jon >,
    dee  => < fred jon col abe ian hal gav dan bob ed >,
    eve  => < jon hal fred dan abe gav col ed ian bob >,
    fay  => < bob abe ed ian jon dan fred gav col hal >,
    gay  => < jon gav hal fred bob abe col ed dan ian >,
    hope => < gav jon bob abe ian dan hal ed col fred >,
    ivy  => < ian col hal gav fred bob abe ed jon dan >,
    jan  => < ed hal gav abe bob jon col ian fred dan >,
);

var guys = he_likes.keys;
var gals = she_likes.keys;

var (:fiancé, :fiancée, :proposed);

func she_prefers (her, hottie) { var a = she_likes{her}; a.index(hottie) < a.index(fiancé{her}) }
func he_prefers  (him, hottie) { var a =  he_likes{him}; a.index(hottie) < a.index(fiancée{him}) }

func unmatched_guy { guys.first {|k| !defined fiancée{k} } }
func preferred_choice(guy) { he_likes{guy}.first {|k| !defined proposed{guy}{k} } }

func engage(guy, gal) {
    fiancé{gal} = guy;
    fiancée{guy} = gal;
}

func match_em {
    say 'Matchmaking:';
    while (defined(var guy = unmatched_guy())) {
        var gal = preferred_choice(guy);
        proposed{guy}{gal} = '❤';
        if (!defined fiancé{gal}) {
            engage(guy, gal);
            say "\t#{gal} and #{guy}";
        }
        elsif (she_prefers(gal, guy)) {
            var engaged_guy = fiancé{gal};
            engage(guy, gal);
            fiancée{engaged_guy} = nil;
            say "\t#{gal} dumped #{engaged_guy} for #{guy}";
        }
    }
}

func check_stability {
    var instabilities = gather {
        guys.each { |m|
            gals.each { |w|
                if (he_prefers(m, w) && she_prefers(w, m)) {
                    take "\t#{w} prefers #{m} to #{fiancé{w}} and #{m} prefers #{w} to #{fiancée{m}}";
                }
            }
        }
    }

    say 'Stablility:';
    instabilities.is_empty
        ? say "\t(all marriages stable)"
        : instabilities.each { |i| say i };
}

func perturb_em {
    say 'Perturb:';
    say "\tengage abi with fred and bea with jon";
    engage('fred', 'abi');
    engage('jon', 'bea');
}

match_em();
check_stability();

perturb_em();
check_stability();
```

{{out}}

```txt

Matchmaking:
	abi and jon
	eve and abe
	bea and fred
	cath and bob
	hope and ian
	eve dumped abe for hal
	ivy and abe
	dee and col
	jan and ed
	fay and dan
	gay and gav
Stablility:
	(all marriages stable)
Perturb:
	engage abi with fred and bea with jon
Stablility:
	gay prefers jon to gav and jon prefers gay to bea
	eve prefers jon to hal and jon prefers eve to bea
	fay prefers jon to dan and jon prefers fay to bea
	bea prefers fred to jon and fred prefers bea to abi

```


=={{header|SPARK}} / {{header|Ada}}==
{{works with|SPARK GPL}}
{{works with|GNAT}}
This solution works for Ada, too, since SPARK code is correct Ada code.

The data set package:

```ada
package Preferences
is

   type Guy_X is (no_guy, abe, bob, col, dan, ed, fred, gav, hal, ian, jon);
   subtype Guy is Guy_X range Guy_X'Succ(Guy_X'First) .. Guy_X'Last;

   type Girl_X is (no_girl, abi, bea, cath, dee, eve, fay, gay, hope, ivy, jan);
   subtype Girl is Girl_X range Girl_X'Succ(Girl_X'First) .. Girl_X'Last;

   type Extended_Rank is range 0 .. 10;
   subtype Rank is Extended_Rank range 1 .. Extended_Rank'Last;

   type His_Rank is array (Rank) of Girl;
   type He_Prefers is array (Guy) of His_Rank;

   Guys_Like : constant He_Prefers := He_Prefers'(
     abe  => His_Rank'(abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay),
     bob  => His_Rank'(cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay),
     col  => His_Rank'(hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan),
     dan  => His_Rank'(ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi),
     ed   => His_Rank'(jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay),
     fred => His_Rank'(bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay),
     gav  => His_Rank'(gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay),
     hal  => His_Rank'(abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee),
     ian  => His_Rank'(hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve),
     jon  => His_Rank'(abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope));

   type Her_Rank is array(Rank) of Guy;
   type She_Prefers is array (Girl) of Her_Rank;

   Girls_Like : constant She_Prefers := She_Prefers'(
     abi  => Her_Rank'(bob, fred, jon, gav, ian, abe, dan, ed, col, hal),
     bea  => Her_Rank'(bob, abe, col, fred, gav, dan, ian, ed, jon, hal),
     cath => Her_Rank'(fred, bob, ed, gav, hal, col, ian, abe, dan, jon),
     dee  => Her_Rank'(fred, jon, col, abe, ian, hal, gav, dan, bob, ed),
     eve  => Her_Rank'(jon, hal, fred, dan, abe, gav, col, ed, ian, bob),
     fay  => Her_Rank'(bob, abe, ed, ian, jon, dan, fred, gav, col, hal),
     gay  => Her_Rank'(jon, gav, hal, fred, bob, abe, col, ed, dan, ian),
     hope => Her_Rank'(gav, jon, bob, abe, ian, dan, hal, ed, col, fred),
     ivy  => Her_Rank'(ian, col, hal, gav, fred, bob, abe, ed, jon, dan),
     jan  => Her_Rank'(ed, hal, gav, abe, bob, jon, col, ian, fred, dan));

end Preferences;

```

The package for creating the engagements and checking stability. This package can be analysed by the SPARK tools and proved to be free of any run-time error.

```ada
with Preferences;
--# inherit Preferences;
package Propose
is

   type Engagements is array (Preferences.Girl) of Preferences.Guy_X;

   procedure Engage (Pairs :    out Engagements);
   --# derives Pairs from ;

   procedure Check_Stable (Pairs      : in     Engagements;
                           OK         :    out Boolean;
                           Other_Girl :    out Preferences.Girl_X;
                           Other_Guy  :    out Preferences.Guy_X);
   --# derives OK,
   --#         Other_Girl,
   --#         Other_Guy from Pairs;

end Propose;

```


```ada
with Preferences;
use type Preferences.Extended_Rank;
use type Preferences.Girl;
use type Preferences.Guy;
package body Propose
is

   --  renaming subtypes:
   subtype Guy  is Preferences.Guy;
   subtype Girl is Preferences.Girl;

   function Her_Rank_Of_Him (Her : Girl;
                             Him : Guy) return Preferences.Rank
   is
      R : Preferences.Rank;
   begin
      R := Preferences.Rank'First;
      while Preferences.Girls_Like(Her)(R) /= Him
        and R /= Preferences.Rank'Last
      loop
         R := Preferences.Rank'Succ(R);
      end loop;
      return R;
   end Her_Rank_Of_Him;

   function His_Rank_Of_Her (Him : Guy;
                             Her : Girl) return Preferences.Rank
   is
      R : Preferences.Rank;
   begin
      R := Preferences.Rank'First;
      while Preferences.Guys_Like(Him)(R) /= Her
        and R /= Preferences.Rank'Last
      loop
         R := Preferences.Rank'Succ(R);
      end loop;
      return R;
   end His_Rank_Of_Her;

   procedure Engage (Pairs :    out Engagements)
   is
      type Free_Guy  is array (Guy)  of Boolean;
      type Free_Girl is array (Girl) of Boolean;
      type Last_Proposals is array (Guy) of Preferences.Extended_Rank;

      --  Initialize all M in M_Free and W in W_Free to free.
      M_Free : Free_Guy  := Free_Guy'(others => True);
      W_Free : Free_Girl := Free_Girl'(others => True);
      Last_Proposal : Last_Proposals :=
        Last_Proposals'(others => Preferences.Extended_Rank'First);

      All_Paired : Boolean := False;
      W  : Girl;
      M1 : Preferences.Guy_X;

   begin
      --  Initially set all engagements to null.
      Pairs := Engagements'(others => Preferences.No_Guy);
      --  while there is a free man M who still has a woman W to propose to
      while not All_Paired loop
         All_Paired := True;
         for M in Guy loop
            if M_Free(M) and Last_Proposal(M) < Preferences.Rank'Last then
               All_Paired := False;
               --  W = M's highest ranked such woman who he has not
               --            proposed to yet
               Last_Proposal(M) := Preferences.Rank'Succ(Last_Proposal(M));
               W := Preferences.Guys_Like(M)(Last_Proposal(M));
               --  if W is free
               if W_Free(W) then
                  --  (M, W) become engaged
                  M_Free(M) := False;
                  W_Free(W) := False;
                  Pairs(W)  := M;
               else
                  --  else some pair (M1, W) already exists
                  M1 := Pairs(W);
                  if M1 > Preferences.no_guy and then
                  --  if W prefers M to M1
                    Her_Rank_Of_Him (Her => W, Him => M)
                    < Her_Rank_Of_Him (Her => W, Him => M1)
                  then
                     --  (M, W) become engaged
                     M_Free(M) := False;
                     Pairs(W)  := M;
                     --  M1 becomes free
                     M_Free(M1) := True;
                  else
                     --  (M1, W) remain engaged
                     null;
                  end if;
               end if;
            end if;
         end loop;
      end loop;
   end Engage;

   procedure Check_Stable (Pairs      : in     Engagements;
                           OK         :    out Boolean;
                           Other_Girl :    out Preferences.Girl_X;
                           Other_Guy  :    out Preferences.Guy_X)
   is
      M      : Preferences.Guy_X;
      W_Rank : Preferences.Rank;
   begin
      OK := True;
      Other_Girl := Preferences.No_Girl;
      Other_Guy  := Preferences.No_Guy;
      --  Loop over all girls.
      for W in Girl loop
         if Pairs(W) > Preferences.no_guy then
            W_Rank := Her_Rank_Of_Him (W, Pairs(W));
            --  Loop over all guys she prefers to her current guy.
            for WR in Preferences.Rank range 1 .. W_Rank - 1 loop
               M := Preferences.Girls_Like(W)(WR);
               if M > Preferences.no_guy then
                  --  Loop over all girls for this guy in preference order.
                  for MR in Preferences.Rank
                  --# assert M > Preferences.no_guy;
                  loop
                     --  Exit if his current girl found before this girl.
                     exit when M = Pairs(Preferences.Guys_Like(M)(MR));
                     --  Unstable if this girl found before his current girl.
                     if Preferences.Guys_Like(M)(MR) = W then
                        OK := False;
                        Other_Girl := W;
                        Other_Guy  := M;
                     end if;
                  end loop;
               end if;
               exit when not OK;
            end loop;
         end if;
         exit when not OK;
      end loop;
   end Check_Stable;

end Propose;

```

The test program tests all pairwise exchanges. This is Ada, it is not SPARK.

(Text IO is quite tedious in SPARK - it's not what the language was designed for.)

```ada
------------------------------------
--  Test program.
--
--  This is Ada, it is not SPARK.
------------------------------------

with Ada.Text_IO;
with Preferences;
with Propose;
procedure Matchmaker
is
   --  renaming subtypes:
   subtype Guy  is Preferences.Guy;
   subtype Girl is Preferences.Girl;

   Marriages : Propose.Engagements;
   Stable    : Boolean;
   Him       : Preferences.Guy_X;
   Her       : Preferences.Girl_X;
   Stable_Marriages : Propose.Engagements;

   procedure Report_Stable
   is
   begin
      if Stable then
         Ada.Text_IO.Put_Line ("Pairs are Stable");
      else
         Ada.Text_IO.Put ("Pairs are Unstable: ");
         Ada.Text_IO.Put_Line
           (Guy'Image(Him) & " and " & Girl'Image(Her) & " prefer each other.");
      end if;
   end Report_Stable;

begin

   Propose.Engage(Pairs => Marriages);
   for W in Girl loop
      Ada.Text_IO.Put_Line (Girl'Image(W) &
                            " marries " &
                            Guy'Image(Marriages(W)));
   end loop;
   Propose.Check_Stable (Pairs      => Marriages,
                         OK         => Stable,
                         Other_Girl => Her,
                         Other_Guy  => Him);
   Report_Stable;
   Stable_Marriages := Marriages;

   for W1 in Girl range Girl'First .. Girl'Pred(Girl'Last) loop
      for W2 in Girl range Girl'Succ(W1) .. Girl'Last loop
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Exchange " & Guy'Image(Marriages(W1)) &
                               " with " & Guy'Image(Marriages(W2)));
         Him := Marriages(W1);
         Marriages(W1) := Marriages(W2);
         Marriages(W2) := Him;
         Propose.Check_Stable (Pairs      => Marriages,
                               OK         => Stable,
                               Other_Girl => Her,
                               Other_Guy  => Him);
         Report_Stable;
         Marriages := Stable_Marriages;
      end loop;
   end loop;

end MatchMaker;

```

The begining of the output from the test. All pairwise exchanges create unstable pairings.

```txt
ABI marries JON
BEA marries FRED
CATH marries BOB
DEE marries COL
EVE marries HAL
FAY marries DAN
GAY marries GAV
HOPE marries IAN
IVY marries ABE
JAN marries ED
Pairs are Stable

Exchange JON with FRED
Pairs are Unstable: FRED and BEA prefer each other.

Exchange JON with BOB
Pairs are Unstable: BOB and CATH prefer each other.

Exchange JON with COL
Pairs are Unstable: JON and ABI prefer each other.

```


## Swift

{{trans|JavaScript}}

```Swift
class Person {
    let name:String
    var candidateIndex = 0
    var fiance:Person?
    var candidates = [Person]()

    init(name:String) {
        self.name = name
    }

    func rank(p:Person) -> Int {
        for (i, candidate) in enumerate(self.candidates) {
            if candidate === p {
                return i
            }
        }
        return self.candidates.count + 1
    }

    func prefers(p:Person) -> Bool {
        if let fiance = self.fiance {
            return self.rank(p) < self.rank(fiance)
        }
        return false
    }

    func nextCandidate() -> Person? {
        if self.candidateIndex >= self.candidates.count {
            return nil
        }
        return self.candidates[candidateIndex++]
    }

    func engageTo(p:Person) {
        p.fiance?.fiance = nil
        p.fiance = self
        self.fiance?.fiance = nil
        self.fiance = p
    }

    func swapWith(p:Person) {
        let thisFiance = self.fiance
        let pFiance = p.fiance
        println("\(self.name) swapped partners with \(p.name)")
        if pFiance != nil && thisFiance != nil {
            self.engageTo(pFiance!)
            p.engageTo(thisFiance!)
        }
    }
}

func isStable(guys:[Person], gals:[Person]) -> Bool {
    for guy in guys {
        for gal in gals {
            if guy.prefers(gal) && gal.prefers(guy) {
                return false
            }
        }
    }
    return true
}

func engageEveryone(guys:[Person]) {
    var done = false
    while !done {
        done = true
        for guy in guys {
            if guy.fiance == nil {
                done = false
                if let gal = guy.nextCandidate() {
                    if gal.fiance == nil || gal.prefers(guy) {
                        guy.engageTo(gal)
                    }
                }
            }
        }
    }
}

func doMarriage() {
    let abe  = Person(name: "Abe")
    let bob  = Person(name: "Bob")
    let col  = Person(name: "Col")
    let dan  = Person(name: "Dan")
    let ed   = Person(name: "Ed")
    let fred = Person(name: "Fred")
    let gav  = Person(name: "Gav")
    let hal  = Person(name: "Hal")
    let ian  = Person(name: "Ian")
    let jon  = Person(name: "Jon")
    let abi  = Person(name: "Abi")
    let bea  = Person(name: "Bea")
    let cath = Person(name: "Cath")
    let dee  = Person(name: "Dee")
    let eve  = Person(name: "Eve")
    let fay  = Person(name: "Fay")
    let gay  = Person(name: "Gay")
    let hope = Person(name: "Hope")
    let ivy  = Person(name: "Ivy")
    let jan  = Person(name: "Jan")

    abe.candidates  = [abi, eve, cath, ivy, jan, dee, fay, bea, hope, gay]
    bob.candidates  = [cath, hope, abi, dee, eve, fay, bea, jan, ivy, gay]
    col.candidates  = [hope, eve, abi, dee, bea, fay, ivy, gay, cath, jan]
    dan.candidates  = [ivy, fay, dee, gay, hope, eve, jan, bea, cath, abi]
    ed.candidates   = [jan, dee, bea, cath, fay, eve, abi, ivy, hope, gay]
    fred.candidates = [bea, abi, dee, gay, eve, ivy, cath, jan, hope, fay]
    gav.candidates  = [gay, eve, ivy, bea, cath, abi, dee, hope, jan, fay]
    hal.candidates  = [abi, eve, hope, fay, ivy, cath, jan, bea, gay, dee]
    ian.candidates  = [hope, cath, dee, gay, bea, abi, fay, ivy, jan, eve]
    jon.candidates  = [abi, fay, jan, gay, eve, bea, dee, cath, ivy, hope]
    abi.candidates  = [bob, fred, jon, gav, ian, abe, dan, ed, col, hal]
    bea.candidates  = [bob, abe, col, fred, gav, dan, ian, ed, jon, hal]
    cath.candidates = [fred, bob, ed, gav, hal, col, ian, abe, dan, jon]
    dee.candidates  = [fred, jon, col, abe, ian, hal, gav, dan, bob, ed]
    eve.candidates  = [jon, hal, fred, dan, abe, gav, col, ed, ian, bob]
    fay.candidates  = [bob, abe, ed, ian, jon, dan, fred, gav, col, hal]
    gay.candidates  = [jon, gav, hal, fred, bob, abe, col, ed, dan, ian]
    hope.candidates = [gav, jon, bob, abe, ian, dan, hal, ed, col, fred]
    ivy.candidates  = [ian, col, hal, gav, fred, bob, abe, ed, jon, dan]
    jan.candidates  = [ed, hal, gav, abe, bob, jon, col, ian, fred, dan]

    let guys = [abe, bob, col, dan, ed, fred, gav, hal, ian, jon]
    let gals = [abi, bea, cath, dee, eve, fay, gay, hope, ivy, jan]

    engageEveryone(guys)

    for guy in guys {
        println("\(guy.name) is engaged to \(guy.fiance!.name)")
    }

    println("Stable = \(isStable(guys, gals))")
    jon.swapWith(fred)
    println("Stable = \(isStable(guys, gals))")

}

doMarriage()
```

{{out}}

```txt

Abe is engaged to Ivy
Bob is engaged to Cath
Col is engaged to Dee
Dan is engaged to Fay
Ed is engaged to Jan
Fred is engaged to Bea
Gav is engaged to Gay
Hal is engaged to Eve
Ian is engaged to Hope
Jon is engaged to Abi
Stable = true
Jon swapped partners with Fred
Stable = false
```



## Tcl

{{trans|Python}}

```tcl
package require Tcl 8.5

# Functions as aliases to standard commands
interp alias {} tcl::mathfunc::pos {} ::lsearch -exact
interp alias {} tcl::mathfunc::nonempty {} ::llength

# The stability check
proc check engaged {
    global preferences
    set inverse [lreverse $engaged]
    set errmsg "%s and %s like each other better than their present partners,\
	    %s and %s respectively"
    dict for {she he} $engaged {
	set shelikes [dict get $preferences $she]
	set shelikesbetter [lrange $shelikes 0 [expr {pos($shelikes,$he)}]]
	set helikes [dict get $preferences $he]
	set helikesbetter [lrange $helikes 0 [expr {pos($helikes,$she)}]]
	foreach guy $shelikesbetter {
	    set guysgirl [dict get $inverse $guy]
	    set guylikes [dict get $preferences $guy]
	    if {pos($guylikes,$guysgirl) > pos($guylikes,$she)} {
		puts [format $errmsg $she $guy $he $guysgirl]
		return 0
	    }
	}
	foreach gal $helikesbetter {
	    set galsguy [dict get $engaged $gal]
	    set gallikes [dict get $preferences $gal]
	    if {pos($gallikes,$galsguy) > pos($gallikes,$he)} {
		puts [format $errmsg $he $gal $she $galsguy]
		return 0
	    }
	}
    }
    return 1
}

# The match-making algorithm
proc matchmaker {} {
    global guys gals preferences
    set guysfree $guys
    set engaged {}
    array set p $preferences
    while {nonempty($guysfree)} {
	set guysfree [lassign $guysfree guy]
	set p($guy) [set guyslist [lassign $p($guy) gal]]
	if {![dict exists $engaged $gal]} {
	    # She's free
	    dict set engaged $gal $guy
	    puts "  $guy and $gal"
	    continue
	}
	# The bounder proposes to an engaged lass!
	set fiance [dict get $engaged $gal]
	if {pos($p($gal), $fiance) > pos($p($gal), $guy)} {
	    # She prefers the new guy
	    dict set engaged $gal $guy
	    puts "  $gal dumped $fiance for $guy"
	    set guy $fiance
	}
	if {nonempty($p($guy))} {
	    lappend guysfree $guy
	}
    }
    return $engaged
}

# Problem dataset; preferences unified since all names distinct
set guys {abe bob col  dan ed  fred gav hal  ian jon}
set gals {abi bea cath dee eve fay  gay hope ivy jan}
set preferences {
    abe  {abi  eve  cath ivy  jan  dee  fay  bea  hope gay}
    bob  {cath hope abi  dee  eve  fay  bea  jan  ivy  gay}
    col  {hope eve  abi  dee  bea  fay  ivy  gay  cath jan}
    dan  {ivy  fay  dee  gay  hope eve  jan  bea  cath abi}
    ed   {jan  dee  bea  cath fay  eve  abi  ivy  hope gay}
    fred {bea  abi  dee  gay  eve  ivy  cath jan  hope fay}
    gav  {gay  eve  ivy  bea  cath abi  dee  hope jan  fay}
    hal  {abi  eve  hope fay  ivy  cath jan  bea  gay  dee}
    ian  {hope cath dee  gay  bea  abi  fay  ivy  jan  eve}
    jon  {abi  fay  jan  gay  eve  bea  dee  cath ivy  hope}

    abi  {bob  fred jon  gav  ian  abe dan  ed  col  hal}
    bea  {bob  abe  col  fred gav  dan ian  ed  jon  hal}
    cath {fred bob  ed   gav  hal  col ian  abe dan  jon}
    dee  {fred jon  col  abe  ian  hal gav  dan bob  ed}
    eve  {jon  hal  fred dan  abe  gav col  ed  ian  bob}
    fay  {bob  abe  ed   ian  jon  dan fred gav col  hal}
    gay  {jon  gav  hal  fred bob  abe col  ed  dan  ian}
    hope {gav  jon  bob  abe  ian  dan hal  ed  col  fred}
    ivy  {ian  col  hal  gav  fred bob abe  ed  jon  dan}
    jan  {ed   hal  gav  abe  bob  jon col  ian fred dan}
}

# The demonstration code
puts "Engagements:"
set engaged [matchmaker]

puts "\nCouples:"
set pfx ""
foreach gal $gals {
    puts -nonewline "$pfx  $gal is engaged to [dict get $engaged $gal]"
    set pfx ",\n"
}
puts "\n"
puts "Engagement stability check [lindex {FAILED PASSED} [check $engaged]]"

puts "\n\nSwapping two fiances to introduce an error"
set tmp [dict get $engaged [lindex $gals 0]]
dict set engaged [lindex $gals 0] [dict get $engaged [lindex $gals 1]]
dict set engaged [lindex $gals 1] $tmp
foreach gal [lrange $gals 0 1] {
    puts "  $gal is now engaged to [dict get $engaged $gal]"
}
puts ""
puts "Engagement stability check [lindex {FAILED PASSED} [check $engaged]]"
```

{{out}}

```txt

Engagements:
  abe and abi
  bob and cath
  col and hope
  dan and ivy
  ed and jan
  fred and bea
  gav and gay
  hope dumped col for ian
  abi dumped abe for jon
  hal and eve
  col and dee
  ivy dumped dan for abe
  dan and fay

Couples:
  abi is engaged to jon,
  bea is engaged to fred,
  cath is engaged to bob,
  dee is engaged to col,
  eve is engaged to hal,
  fay is engaged to dan,
  gay is engaged to gav,
  hope is engaged to ian,
  ivy is engaged to abe,
  jan is engaged to ed

Engagement stability check PASSED


Swapping two fiances to introduce an error
  abi is now engaged to fred
  bea is now engaged to jon

fred and bea like each other better than their present partners, abi and jon respectively
Engagement stability check FAILED

```



## UNIX Shell

{{works with|Bourne Again Shell|4.0}}
{{trans|AutoHotkey}}

```shell
#!/usr/bin/env bash
main() {
  # Our ten males:
  local males=(abe bob col dan ed fred gav hal ian jon)

  # And ten females:
  local females=(abi bea cath dee eve fay gay hope ivy jan)

  # Everyone's preferences, ranked most to least desirable:
  local  abe=( abi  eve  cath ivy  jan  dee  fay  bea  hope gay )
  local  abi=( bob  fred jon  gav  ian  abe  dan  ed   col  hal )
  local  bea=( bob  abe  col  fred gav  dan  ian  ed   jon  hal )
  local  bob=(cath  hope abi  dee  eve  fay  bea  jan  ivy  gay )
  local cath=(fred  bob  ed   gav  hal  col  ian  abe  dan  jon )
  local  col=(hope  eve  abi  dee  bea  fay  ivy  gay  cath jan )
  local  dan=( ivy  fay  dee  gay  hope eve  jan  bea  cath abi )
  local  dee=(fred  jon  col  abe  ian  hal  gav  dan  bob  ed  )
  local   ed=( jan  dee  bea  cath fay  eve  abi  ivy  hope gay )
  local  eve=( jon  hal  fred dan  abe  gav  col  ed   ian  bob )
  local  fay=( bob  abe  ed   ian  jon  dan  fred gav  col  hal )
  local fred=( bea  abi  dee  gay  eve  ivy  cath jan  hope fay )
  local  gav=( gay  eve  ivy  bea  cath abi  dee  hope jan  fay )
  local  gay=( jon  gav  hal  fred bob  abe  col  ed   dan  ian )
  local  hal=( abi  eve  hope fay  ivy  cath jan  bea  gay  dee )
  local hope=( gav  jon  bob  abe  ian  dan  hal  ed   col  fred)
  local  ian=(hope  cath dee  gay  bea  abi  fay  ivy  jan  eve )
  local  ivy=( ian  col  hal  gav  fred bob  abe  ed   jon  dan )
  local  jan=( ed   hal  gav  abe  bob  jon  col  ian  fred dan )
  local  jon=( abi  fay  jan  gay  eve  bea  dee  cath ivy  hope)

  # A place to store the engagements:
  local -A engagements=()

  # Our list of free males, initially comprised of all of them:
  local freemales=( "${males[@]}" )

  # Now we use the Gale-Shapley algorithm to find a stable set of engagements

  # Loop over the free males. Note that we can't use for..in because the body
  # of the loop may modify the array we're looping over
  local -i m=0
  while (( m < ${#freemales[@]} )); do
    local male=${freemales[m]}
    let m+=1

    # This guy's preferences
    eval 'local his=("${'"$male"'[@]}")'

    # Starting with his favorite
    local -i f=0
    local female=${his[f]}

    # Find her preferences
    eval 'local hers=("${'"$female"'[@]}")'

    # And her current fiancé, if any
    local fiance=${engagements[$female]}

    # If she has a fiancé and prefers him to this guy, look for this guy's next
    # best choice
    while [[ -n $fiance ]] &&
      (( $(index "$male" "${hers[@]}") > $(index "$fiance" "${hers[@]}") )); do
      let f+=1
      female=${his[f]}
      eval 'hers=("${'"$female"'[@]}")'
      fiance=${engagements[$female]}
    done

    # If we're still on someone who's engaged, it means she prefers this guy
    # to her current fiancé. Dump him and put him at the end of the free list.
    if [[ -n $fiance ]]; then
      freemales+=("$fiance")
      printf '%-4s rejected %-4s\n' "$female" "$fiance"
    fi

    # We found a match! Record it
    engagements[$female]=$male
    printf '%-4s accepted %-4s\n' "$female" "$male"
  done

  # Display the final result, which should be stable
  print_couples engagements

  # Verify its stability
  print_stable engagements "${females[@]}"

  # Try a swap
  printf '\nWhat if cath and ivy swap partners?\n'
  local temp=${engagements[cath]}
  engagements[cath]=${engagements[ivy]}
  engagements[ivy]=$temp

  # Display the new result, which should be unstable
  print_couples engagements

  # Verify its instability
  print_stable engagements "${females[@]}"
}

# utility function - get index of an item in an array
index() {
  local needle=$1
  shift
  local haystack=("$@")
  local -i i
  for i in "${!haystack[@]}"; do
    if [[ ${haystack[i]} == $needle ]]; then
      printf '%d\n' "$i"
      return 0
    fi
  done
  return 1
}

# print the couples from the engagement array; takes name of array as argument
print_couples() {
  printf '\nCouples:\n'
  local keys
  mapfile -t keys < <(eval 'printf '\''%s\n'\'' "${!'"$1"'[@]}"' | sort)
  local female
  for female in "${keys[@]}"; do
    eval 'local male=${'"$1"'["'"$female"'"]}'
    printf '%-4s is engaged to %-4s\n' "$female" "$male"
  done
  printf '\n'
}

# print whether a set of engagements is stable; takes name of engagement array
# followed by the list of females
print_stable() {
  if stable "$@"; then
    printf 'These couples are stable.\n'
  else
    printf 'These couples are not stable.\n'
  fi
}

# determine if a set of engagements is stable; takes name of engagement array
# followed by the list of females
stable() {
  local dict=$1
  shift
  eval 'local shes=("${!'"$dict"'[@]}")'
  eval 'local hes=("${'"$dict"'[@]}")'
  local -i i
  local -i result=0
  for (( i=0; i<${#shes[@]}; ++i )); do
    local she=${shes[i]} he=${hes[i]}
    eval 'local his=("${'"$he"'[@]}")'
    local alt
    for alt in "$@"; do
      eval 'local fiance=${'"$dict"'["'"$alt"'"]}'
      eval 'local hers=("${'"$alt"'[@]}")'
      if (( $(index "$she" "${his[@]}") > $(index "$alt" "${his[@]}")
         && $(index "$fiance" "${hers[@]}") > $(index "$he" "${hers[@]}") ))
       then
        printf '%-4s is engaged to %-4s but prefers %4s, ' "$he" "$she" "$alt"
        printf 'while %-4s is engaged to %-4s but prefers %4s.\n' "$alt" "$fiance" "$he"
        result=1
      fi
    done
  done
  if (( result )); then printf '\n'; fi
  return $result
}

main "$@"
```

{{Out}}

```txt
abi  accepted abe
cath accepted bob
hope accepted col
ivy  accepted dan
jan  accepted ed
bea  accepted fred
gay  accepted gav
eve  accepted hal
hope rejected col
hope accepted ian
abi  rejected abe
abi  accepted jon
dee  accepted col
ivy  rejected dan
ivy  accepted abe
fay  accepted dan

Couples:
abi  is engaged to jon
bea  is engaged to fred
cath is engaged to bob
dee  is engaged to col
eve  is engaged to hal
fay  is engaged to dan
gay  is engaged to gav
hope is engaged to ian
ivy  is engaged to abe
jan  is engaged to ed

These couples are stable.

What if cath and ivy swap partners?

Couples:
abi  is engaged to jon
bea  is engaged to fred
cath is engaged to abe
dee  is engaged to col
eve  is engaged to hal
fay  is engaged to dan
gay  is engaged to gav
hope is engaged to ian
ivy  is engaged to bob
jan  is engaged to ed

bob  is engaged to ivy  but prefers  abi, while abi  is engaged to jon  but prefers  bob.
bob  is engaged to ivy  but prefers  bea, while bea  is engaged to fred but prefers  bob.
bob  is engaged to ivy  but prefers cath, while cath is engaged to abe  but prefers  bob.
bob  is engaged to ivy  but prefers  fay, while fay  is engaged to dan  but prefers  bob.
bob  is engaged to ivy  but prefers hope, while hope is engaged to ian  but prefers  bob.

These couples are not stable.

```



## Ursala


```Ursala
men =

{
   'abe': <'abi','eve','cath','ivy','jan','dee','fay','bea','hope','gay'>,
   'bob': <'cath','hope','abi','dee','eve','fay','bea','jan','ivy','gay'>,
   'col': <'hope','eve','abi','dee','bea','fay','ivy','gay','cath','jan'>,
   'dan': <'ivy','fay','dee','gay','hope','eve','jan','bea','cath','abi'>,
   'ed': <'jan','dee','bea','cath','fay','eve','abi','ivy','hope','gay'>,
   'fred': <'bea','abi','dee','gay','eve','ivy','cath','jan','hope','fay'>,
   'gav': <'gay','eve','ivy','bea','cath','abi','dee','hope','jan','fay'>,
   'hal': <'abi','eve','hope','fay','ivy','cath','jan','bea','gay','dee'>,
   'ian': <'hope','cath','dee','gay','bea','abi','fay','ivy','jan','eve'>,
   'jon': <'abi','fay','jan','gay','eve','bea','dee','cath','ivy','hope'>}

women =

{
   'abi': <'bob','fred','jon','gav','ian','abe','dan','ed','col','hal'>,
   'bea': <'bob','abe','col','fred','gav','dan','ian','ed','jon','hal'>,
   'cath': <'fred','bob','ed','gav','hal','col','ian','abe','dan','jon'>,
   'dee': <'fred','jon','col','abe','ian','hal','gav','dan','bob','ed'>,
   'eve': <'jon','hal','fred','dan','abe','gav','col','ed','ian','bob'>,
   'fay': <'bob','abe','ed','ian','jon','dan','fred','gav','col','hal'>,
   'gay': <'jon','gav','hal','fred','bob','abe','col','ed','dan','ian'>,
   'hope': <'gav','jon','bob','abe','ian','dan','hal','ed','col','fred'>,
   'ivy': <'ian','col','hal','gav','fred','bob','abe','ed','jon','dan'>,
   'jan': <'ed','hal','gav','abe','bob','jon','col','ian','fred','dan'>}

match = # finds a stable list of engagements from data as given above

-+
   ^=rrmhPnXS ^/~&l ^/~&rl ^|DlrHSs/~& ^TlK2hlPrSLPAS\~&r -+
      ~&lK2hlPrSXS,
      *=rnmihBPK12D ~&mmhPnXNCB^rnPrmPllSPcA\~&r ~&lrrPwZK17@rnPlX+-,
   ^|rrPlrlPXX/~& ^/~&nNAS ^/~&l+ ^|H\~&+ -:+ * ^|/~& -<+ \/-=+ ~&DSL@tK33+-

preferred = # finds non-couples that would prefer each other to their betrothed

~&lSLPrSLrlXS2c^|DlrHS\~& ~~irlXX+ ^D/~&l+ ^|H\~&+ -:+ *T ^|/~& //~=;+ -~l;+ \/~&H

#cast %sWLm

main = # stable, perturbed, and preferred alternatives to the perturbed

<
   'stable': match/men women,
   'perturbed': ~&lSrSxPp match/men women,
   'preferred': preferred/(men,women) ~&lSrSxPp match/men women>
```

The matches are perturbed by reversing the order of the women.
{{out}}

```txt
<
   'stable': <
      ('jon','abi'),
      ('fred','bea'),
      ('bob','cath'),
      ('col','dee'),
      ('hal','eve'),
      ('dan','fay'),
      ('gav','gay'),
      ('ian','hope'),
      ('abe','ivy'),
      ('ed','jan')>,
   'perturbed': <
      ('jon','jan'),
      ('fred','ivy'),
      ('bob','hope'),
      ('col','gay'),
      ('hal','fay'),
      ('dan','eve'),
      ('gav','dee'),
      ('ian','cath'),
      ('abe','bea'),
      ('ed','abi')>,
   'preferred': <
      ('jon','abi'),
      ('jon','fay'),
      ('fred','abi'),
      ('fred','dee'),
      ('fred','gay'),
      ('fred','eve'),
      ('bob','cath'),
      ('col','dee'),
      ('col','fay'),
      ('col','ivy'),
      ('hal','eve'),
      ('dan','fay'),
      ('gav','gay'),
      ('gav','ivy'),
      ('gav','cath'),
      ('gav','abi'),
      ('abe','abi'),
      ('abe','jan'),
      ('abe','dee'),
      ('abe','fay'),
      ('ed','jan'),
      ('ed','cath'),
      ('ed','fay')>>
```


{{omit from|GUISS}}


## VBA



```txt
2 methods will be shown here:
1 - using basic VBA-features for strings
2 - using the scripting.dictionary library
```


'''The string approach'''<br/>

```vb
Sub M_snb()
  c00 = "_abe abi eve cath ivy jan dee fay bea hope gay " & _
        "_bob cath hope abi dee eve fay bea jan ivy gay " & _
        "_col hope eve abi dee bea fay ivy gay cath jan " & _
        "_dan ivy fay dee gay hope eve jan bea cath abi " & _
        "_ed jan dee bea cath fay eve abi ivy hope gay " & _
        "_fred bea abi dee gay eve ivy cath jan hope fay " & _
        "_gav gay eve ivy bea cath abi dee hope jan fay " & _
        "_hal abi eve hope fay ivy cath jan bea gay dee " & _
        "_ian hope cath dee gay bea abi fay ivy jan eve " & _
        "_jon abi fay jan gay eve bea dee cath ivy hope " & _
        "_abi bob fred jon gav ian abe dan ed col hal " & _
        "_bea bob abe col fred gav dan ian ed jon hal " & _
        "_cath fred bob ed gav hal col ian abe dan jon " & _
        "_dee fred jon col abe ian hal gav dan bob ed " & _
        "_eve jon hal fred dan abe gav col ed ian bob " & _
        "_fay bob abe ed ian jon dan fred gav col hal " & _
        "_gay jon gav hal fred bob abe col ed dan ian " & _
        "_hope gav jon bob abe ian dan hal ed col fred " & _
        "_ivy ian col hal gav fred bob abe ed jon dan " & _
        "_jan ed hal gav abe bob jon col ian fred dan "

  sn = Filter(Filter(Split(c00), "_"), "-", 0)
  Do
    c01 = Mid(c00, InStr(c00, sn(0) & " "))
    st = Split(Left(c01, InStr(Mid(c01, 2), "_")))
      For j = 1 To UBound(st) - 1
        If InStr(c00, "_" & st(j) & " ") > 0 Then
          c00 = Replace(Replace(c00, sn(0), sn(0) & "-" & st(j)), "_" & st(j), "_" & st(j) & "." & Mid(sn(0), 2))
          Exit For
        Else
          c02 = Filter(Split(c00, "_"), st(j) & ".")(0)
          c03 = Split(Split(c02)(0), ".")(1)
          If InStr(c02, " " & Mid(sn(0), 2) & " ") < InStr(c02, " " & c03 & " ") Then
            c00 = Replace(Replace(Replace(c00, c03 & "-" & st(j), c03), sn(0), sn(0) & "-" & st(j)), "_" & st(j), "_" & st(j) & "." & Mid(sn(0), 2))
            Exit For
          End If
        End If
     Next
     sn = Filter(Filter(Filter(Split(c00), "_"), "-", 0), ".", 0)
   Loop Until UBound(sn) = -1

   MsgBox Replace(Join(Filter(Split(c00), "-"), vbLf), "_", "")
End Sub
```


'''The Dictionary approach'''


```vb
Sub M_snb()
  Set d_00 = CreateObject("scripting.dictionary")
  Set d_01 = CreateObject("scripting.dictionary")
  Set d_02 = CreateObject("scripting.dictionary")

  sn = Split("abe abi eve cath ivy jan dee fay bea hope gay _" & _
       "bob cath hope abi dee eve fay bea jan ivy gay _" & _
       "col hope eve abi dee bea fay ivy gay cath jan _" & _
       "dan ivy fay dee gay hope eve jan bea cath abi _" & _
       "ed jan dee bea cath fay eve abi ivy hope gay _" & _
       "fred bea abi dee gay eve ivy cath jan hope fay _" & _
       "gav gay eve ivy bea cath abi dee hope jan fay _" & _
       "hal abi eve hope fay ivy cath jan bea gay dee _" & _
       "ian hope cath dee gay bea abi fay ivy jan eve _" & _
       "jon abi fay jan gay eve bea dee cath ivy hope ", "_")

  sp = Split("abi bob fred jon gav ian abe dan ed col hal _" & _
       "bea bob abe col fred gav dan ian ed jon hal _" & _
       "cath fred bob ed gav hal col ian abe dan jon _" & _
       "dee fred jon col abe ian hal gav dan bob ed _" & _
       "eve jon hal fred dan abe gav col ed ian bob _" & _
       "fay bob abe ed ian jon dan fred gav col hal _" & _
       "gay jon gav hal fred bob abe col ed dan ian _" & _
       "hope gav jon bob abe ian dan hal ed col fred _" & _
       "ivy ian col hal gav fred bob abe ed jon dan _" & _
       "jan ed hal gav abe bob jon col ian fred dan ", "_")

  For j = 0 To UBound(sn)
    d_00(Split(sn(j))(0)) = ""
    d_01(Split(sp(j))(0)) = ""
    d_02(Split(sn(j))(0)) = sn(j)
    d_02(Split(sp(j))(0)) = sp(j)
  Next

  Do
    For Each it In d_00.keys
      If d_00.Item(it) = "" Then
        st = Split(d_02.Item(it))
        For jj = 1 To UBound(st)
          If d_01(st(jj)) = "" Then
            d_00(st(0)) = st(0) & vbTab & st(jj)
            d_01(st(jj)) = st(0)
            Exit For
          ElseIf InStr(d_02.Item(st(jj)), " " & st(0) & " ") < InStr(d_02.Item(st(jj)), " " & d_01(st(jj)) & " ") Then
            d_00(d_01(st(jj))) = ""
            d_00(st(0)) = st(0) & vbTab & st(jj)
            d_01(st(jj)) = st(0)
            Exit For
          End If
        Next
      End If
    Next
  Loop Until UBound(Filter(d_00.items, vbTab)) = d_00.Count - 1

  MsgBox Join(d_00.items, vbLf)
End Sub
```


{{out}}

```txt

abe  - ivy
bob  - cath
col  - dee
dan  - fay
ed   - jan
fred - bea
gav  - gay
hal  - eve
ian  - hope
jan  - abi

```



## XSLT 2.0


Assume that the input is in XML form as listed [[Stable marriage problem/XSLT input|here]]. The following XSLT 2.0 style-sheet...

<lang><xsl:stylesheet version="2.0"
  xmlns:xsl="http://www.w3.org/1999/XSL/Transform"
  xmlns:fn="http://www.w3.org/2005/xpath-functions"
  xmlns:xs="http://www.w3.org/2001/XMLSchema"
  xmlns:m="http://rosettacode.org/wiki/Stable_marriage_problem"
  xmlns:t="http://rosettacode.org/wiki/Stable_marriage_problem/temp"
  exclude-result-prefixes="xsl xs fn t m">
<xsl:output indent="yes" encoding="UTF-8" omit-xml-declaration="yes" />
<xsl:strip-space elements="*" />

<xsl:template match="*">
  <xsl:copy>
    <xsl:apply-templates select="@*,node()" />
  </xsl:copy>
</xsl:template>

<xsl:template match="@*|comment()|processing-instruction()">
  <xsl:copy />
</xsl:template>

<xsl:template match="m:interest" mode="match-making">
  <m:engagement>
    <m:dude name="{../@name}" /><m:maid name="{.}" />
  </m:engagement>
</xsl:template>

<xsl:template match="m:dude" mode="match-making">
  <!-- 3. Reject suitors cross-off the maids that spurned them. -->
  <xsl:param name="eliminations" select="()" />
  <m:dude name="{@name}">
	<xsl:copy-of select="for $b in @name return
	  m:interest[not(. = $eliminations[m:dude/@name=$b]/m:maid/@name)]" />
  </m:dude>
</xsl:template>

<xsl:template match="*" mode="perturbation">
  <xsl:copy>
    <xsl:apply-templates select="@*,node()" mode="perturbation"/>
  </xsl:copy>
</xsl:template>

<xsl:template match="@*" mode="perturbation">
  <xsl:copy />
</xsl:template>

<xsl:template match="m:engagement[position() lt 3]/m:maid/@name" mode="perturbation">
  <!-- Swap maids 1 and 2. -->
  <xsl:copy-of select="for $c in count(../../preceding-sibling::m:engagement)
       return ../../../m:engagement[2 - $c]/m:maid/@name" />
</xsl:template>

<xsl:template match="m:stable-marriage-problem">
  <xsl:variable name="population" select="m:dude|m:maid" />
  <xsl:variable name="solution">
    <xsl:call-template name="solve-it">
      <xsl:with-param name="dudes" select="m:dude" />
      <xsl:with-param name="maids" select="m:maid" tunnel="yes" />
    </xsl:call-template>
  </xsl:variable>
  <xsl:variable name="perturbed">
   <xsl:apply-templates select="$solution/*" mode="perturbation" />
  </xsl:variable>
  <m:stable-marriage-problem-result>
    <m:solution is-stable="{t:is-stable( $population, $solution/*)}">
	  <xsl:copy-of select="$solution/*" />
    </m:solution>
	<m:message>Perturbing the matches! Swapping <xsl:value-of select="$solution/*[1]/m:maid/@name" /> for <xsl:value-of select="$solution/*[2]/m:maid/@name" /></m:message>
	<m:message><xsl:choose>
	  <xsl:when test="t:is-stable( $population, $perturbed/*)">
	    <xsl:text>The perturbed configuration is stable.</xsl:text>
	  </xsl:when>
	    <xsl:otherwise>The perturbed configuration is unstable.</xsl:otherwise>
	</xsl:choose></m:message>
  </m:stable-marriage-problem-result>
</xsl:template>

<xsl:template name="solve-it">
  <xsl:param name="dudes" as="element()*" /> <!-- Sequence of m:dude -->
  <xsl:param name="maids" as="element()*" tunnel="yes" />  <!-- Sequence of m:maid -->
  <xsl:param name="engagements" as="element()*" select="()" /> <!-- Sequence of m:engagement -->

  <!-- 1. For each dude not yet engaged, and has a preference, propose to his top preference. -->
  <xsl:variable name="fresh-proposals">
    <xsl:apply-templates select="$dudes[not(@name = $engagements/m:dude/@name)]/m:interest[1]" mode="match-making" />
  </xsl:variable>
  <xsl:variable name="proposals" select="$engagements | $fresh-proposals/m:engagement" />

  <!-- 2. For each maid with conflicting suitors, reject all but the most attractive (for her) proposal. -->
  <xsl:variable name="acceptable" select="$proposals[
    for $g in m:maid/@name, $b in m:dude/@name, $this-interest in $maids[@name=$g]/m:interest[.=$b]
	  return every
	      $interest
		in
		  for $other-b in $proposals[m:maid[@name=$g]]/m:dude/@name[. ne $b]
		    return $maids[@name=$g]/m:interest[.=$other-b]
		satisfies
		  $interest >> $this-interest
    ]" />

  <!-- 3. Reject suitors cross-off the maids that spurned them. -->
  <xsl:variable name="new-dudes">
    <xsl:apply-templates select="$dudes" mode="match-making">
      <xsl:with-param name="eliminations" select="$fresh-proposals/m:engagement" />
    </xsl:apply-templates>
  </xsl:variable>

  <!-- 4. Test for finish. If not, loop back for another round of proposals. -->
  <xsl:choose>
    <xsl:when test="$dudes[not(for $b in @name return $acceptable[m:dude/@name=$b])]">
  	  <xsl:call-template name="solve-it">
        <xsl:with-param name="dudes" select="$new-dudes/m:dude" />
        <xsl:with-param name="engagements" select="$acceptable" />
	  </xsl:call-template>
    </xsl:when>
	<xsl:otherwise>
      <xsl:copy-of select="$acceptable" />
	</xsl:otherwise>
  </xsl:choose>
</xsl:template>

<xsl:function name="t:is-stable" as="xs:boolean">
  <xsl:param name="population" as="element()*" />
  <xsl:param name="engagements" as="element()*" />
  <xsl:sequence select="
    every $e in $engagements,
	      $b in string($e/m:dude/@name), $g in string($e/m:maid/@name),
		  $desired-g in $population/self::m:dude[@name=$b]/m:interest[$g=following-sibling::m:interest],
		  $desired-maid in $population/self::m:maid[@name=$desired-g]
	  satisfies
          not(
		    $desired-maid/m:interest[.=$b] &lt;&lt;
		    $desired-maid/m:interest[.=$engagements[m:maid[@name=$desired-g]]/m:dude/@name])
  " />
</xsl:function>

</xsl:stylesheet>
```


...when applied to the said input document will yield...

<lang><t>
   <m:stable-marriage-problem-result xmlns:m="http://rosettacode.org/wiki/Stable_marriage_problem">
      <m:solution is-stable="true">
         <m:engagement>
            <m:dude name="bob"/>
            <m:maid name="cath"/>
         </m:engagement>
         <m:engagement>
            <m:dude name="ed"/>
            <m:maid name="jan"/>
         </m:engagement>
         <m:engagement>
            <m:dude name="fred"/>
            <m:maid name="bea"/>
         </m:engagement>
         <m:engagement>
            <m:dude name="gav"/>
            <m:maid name="gay"/>
         </m:engagement>
         <m:engagement>
            <m:dude name="ian"/>
            <m:maid name="hope"/>
         </m:engagement>
         <m:engagement>
            <m:dude name="jon"/>
            <m:maid name="abi"/>
         </m:engagement>
         <m:engagement>
            <m:dude name="hal"/>
            <m:maid name="eve"/>
         </m:engagement>
         <m:engagement>
            <m:dude name="abe"/>
            <m:maid name="ivy"/>
         </m:engagement>
         <m:engagement>
            <m:dude name="col"/>
            <m:maid name="dee"/>
         </m:engagement>
         <m:engagement>
            <m:dude name="dan"/>
            <m:maid name="fay"/>
         </m:engagement>
      </m:solution>
      <m:message>Perturbing the matches! Swapping cath for jan</m:message>
      <m:message>The perturbed configuration is unstable.</m:message>
   </m:stable-marriage-problem-result>
</t>
```



## zkl

{{trans|PicoLisp}}

```zkl
var
   Boys=Dictionary(
      "abe", "abi eve cath ivy jan dee fay bea hope gay".split(),
      "bob", "cath hope abi dee eve fay bea jan ivy gay".split(),
      "col", "hope eve abi dee bea fay ivy gay cath jan".split(),
      "dan", "ivy fay dee gay hope eve jan bea cath abi".split(),
      "ed",  "jan dee bea cath fay eve abi ivy hope gay".split(),
      "fred","bea abi dee gay eve ivy cath jan hope fay".split(),
      "gav", "gay eve ivy bea cath abi dee hope jan fay".split(),
      "hal", "abi eve hope fay ivy cath jan bea gay dee".split(),
      "ian", "hope cath dee gay bea abi fay ivy jan eve".split(),
      "jon", "abi fay jan gay eve bea dee cath ivy hope".split(), ),
   Girls=Dictionary(
      "abi", "bob fred jon gav ian abe dan ed col hal".split(),
      "bea", "bob abe col fred gav dan ian ed jon hal".split(),
      "cath","fred bob ed gav hal col ian abe dan jon".split(),
      "dee", "fred jon col abe ian hal gav dan bob ed".split(),
      "eve", "jon hal fred dan abe gav col ed ian bob".split(),
      "fay", "bob abe ed ian jon dan fred gav col hal".split(),
      "gay", "jon gav hal fred bob abe col ed dan ian".split(),
      "hope","gav jon bob abe ian dan hal ed col fred".split(),
      "ivy", "ian col hal gav fred bob abe ed jon dan".split(),
      "jan", "ed hal gav abe bob jon col ian fred dan".split(), ),
   Couples=List();  // ( (boy,girl),(boy,girl),...)

Boyz:=Boys.pump(Dictionary(),fcn([(b,gs)]){ return(b,gs.copy()) }); // make writable copy
while( bgs:=Boyz.filter1( 'wrap([(Boy,gs)]){ // while unattached boy
             gs and (not Couples.filter1("holds",Boy))
     }) )
{
   Boy,Girl:=bgs; Girl=Girl.pop(0);
   Pair:=Couples.filter1("holds",Girl);  // is Girl part of a couple?
   if(not Pair) Couples.append(List(Boy,Girl));	# no, Girl was free
   else{			  // yes, Girl is engaged to Pair[0]
      bsf,nBoy,nB:=Girls[Girl].index, bsf(Boy),bsf(Pair[0]);
      if(nBoy<nB) Pair[0]=Boy;		# Girl prefers Boy, change Couples
   }
}
foreach Boy,Girl in (Couples){ println(Girl," is engaged to ",Boy) }

fcn checkCouples(Couples){
   Couples.filter(fcn([(Boy,Girl)]){
      Girls[Girl].filter1('wrap(B){
         // is B before Boy in Girls preferences?
         bsf,nBoy,nB:=Girls[Girl].index, bsf(Boy),bsf(B);
	 // Does B prefer Girl (over his partner)?
	 _,G:=Couples.filter1("holds",B); // (B,G)
	 gsf,nGirl,nG:=Boys[B].index, gsf(Girl),gsf(G);
	 ( nB<nBoy and nGirl<nG and
	    println(Girl," likes ",B," better than ",Boy," and ",
                    B," likes ",Girl," better than ",G) )
      })
   }) or println("All marriages are stable");
}

checkCouples(Couples);
println();

println("Engage fred with abi and jon with bea");
Couples.filter1("holds","fred")[1]="abi";
Couples.filter1("holds","jon") [1]="bea";
checkCouples(Couples);
```

{{out}}

```txt

gay is engaged to gav
ivy is engaged to abe
abi is engaged to jon
cath is engaged to bob
hope is engaged to ian
bea is engaged to fred
eve is engaged to hal
fay is engaged to dan
jan is engaged to ed
dee is engaged to col
All marriages are stable

Engage fred with abi and jon with bea
gay likes jon better than gav and jon likes gay better than bea
bea likes fred better than jon and fred likes bea better than abi
eve likes jon better than hal and jon likes eve better than bea
fay likes jon better than dan and jon likes fay better than bea

```

