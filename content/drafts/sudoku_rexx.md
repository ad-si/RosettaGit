+++
title = "Sudoku/REXX"
description = ""
date = 2018-10-15T20:33:57Z
aliases = []
[extra]
id = 12868
[taxonomies]
categories = []
tags = []
+++

{{collection|Sudoku}}


## REXX


###  REXX version 1 

This is the   '''$SUDOKU.REX'''   (REXX) program and is used to solve the Rosetta Code task of "sudoku".


This REXX program was originally written to assist in sudoku puzzle solving (by giving strong hints), and not to solve the puzzle outright.

The REXX program was written to give increasing better hints and also show the possibilities (of what is possible solution for any cell),

and to partially solve the puzzle using distinct strategies (separately or in combination).   One option is to solve the puzzle. 

The help for the   '''$SUDOKU'''   REXX program is included here   ───►   [[$SUDOKU.HEL]].

The   '''$SUDOKU.REX'''   REXX program makes use of   '''$ERR.REX'''   REXX program which is used to display error messages (via   '''$T.REX'''). 

The   '''$ERR.REX'''   REXX program is included here   ───►   [[$ERR.REX]].

The   '''$SUDOKU.REX'''   REXX program makes use of   '''$T.REX'''   REXX program which is used to display text messages. 

The   '''$T.REX'''   REXX program is included here   ───►   [[$T.REX]].

Some older REXXes don't have a   '''changestr'''   BIF, so one is included here   ───►   [[CHANGESTR.REX]]. 

REXX programs   ''not''   included are   '''$H'''   which shows/displays '''help''' and other documentation.

```rexx
/*REXX program displays, can give hints, and/or solve a sudoku puzzle.  */
trace off
parse arg !
if !all(arg())  then exit
if !cms         then address ''
signal on halt
signal on noValue
signal on syntax

ops=!                                        /*remove extraneous blanks.*/
numeric digits 20
combos=1
@.=' '                                       /*initialize grid to blanks*/
!.=                                          /*nullify valid empty# list*/
@abc='abcdefghijklmnopqrstuvwxyz'
@abcU=@abc
upper @abcU
colors=!cms | !pcrexx | !r4 | !roo           /*Are colors supported ?   */
clear=1                                      /*option: clear the screen.*/
highLight=0                                  /*option: highlight singles*/
pruneall=0                                   /*option: prune all.       */
prunemats=0                                  /*option: prune matches.   */
prunesing=0                                  /*option: prune singles.   */
pruneexcl=0                                  /*option: prune exclusives.*/
pruneline=0                                  /*option: prune lines.     */
pruneonly=0                                  /*option: prune onlys.     */
simple=0                                     /*option: show simple boxes*/
showoneline=0                                /*option: show grid as1line*/
showgrid=1                                   /*option: show the grid.   */
showinfo=1                                   /*option: show informatiion*/
showposs=0                                   /*option: show possible val*/
showcomb=0                                   /*option: show combinations*/
showrow=                                     /*option: SHOWPOSS for rowN*/
showcol=                                     /*option: SHOWPOSS for colN*/
showbox=                                     /*option: SHOWPOSS for boxN*/
showcell=                                    /*option: SHOWPOSS cellRC  */
short=0
solve=0                                      /*option: solve the puzzle.*/
sod=lower(translate(!fn,,'$'))               /*name of the puzzle.      */
tellinvalid=1                                /*tell err msg if invalid X*/
tops=                                        /*option: used for $T opts.*/

gridindents=3                                /*# spaces grid is indented*/
gridindent=left('',gridindents)              /*spaces indented for grid.*/
gridwidth=7                                  /*grid cell interior width.*/
gridbar='b3'x                                /*bar for the grid (cells).*/
gridlt='da'x                                 /*grid cell  left top.     */
gridrt='bf'x                                 /*grid cell right top.     */
gridlb='c0'x                                 /*grid cell  left bottom.  */
gridrb='d9'x                                 /*grid cell right bottom.  */
gridline='c4'x                               /*grid cell line (hyphen). */
gridlin=copies(gridline,gridwidth)           /*grid cell total line.    */
gridemp=left('',gridwidth)                   /*grid cell empty (spaces).*/
griddj='c2'x                                 /*grid cell  down junction.*/
griduj='c1'x                                 /*grid cell    up junction.*/
gridlj='c3'x                                 /*grid cell  left junction.*/
gridrj='b4'x                                 /*grid cell right junction.*/
gridcross='c5'x                              /*grid cell cross junction.*/

  do  while ops\==''                         /*parse any and all options*/
  parse var ops _1 2 1 _ . 1 _o ops
  upper _

    select
    when _==','                    then nop
    when _1=='.' & pos("=",_)\==0  then tops=tops _o

    when  abb('PUZzle')            then      /*do PUZZ (whole) placement*/
         do
         puzz=na()
         if length(puzz)>81  then call er 30,puzz 'PUZZLE 1───►81'

                         do j=1  for length(puzz)
                         q=substr(puzz,j,1)
                         if q=='.'  then iterate
                         call vern q,'PUZZLE_digit'
                         c=j//9
                         if c==0  then c=9
                         r=(j-1)%9 + 1
                         @.r.c=q
                         end   /*j*/
         end

    when _=='CELL' then                      /*do CELL (grid) placement.*/
         do
         rc=nai()
         if length(rc)\==2                   then call er 30,y 'CELL'rc 2
         y=na()
         if length(y)>1                      then call er 30,y 'CELL'rc 1
         r=left(rc,1)
         c=right(rc,1)
         call vern r,'CELLrow'
         call vern c,'CELLcolumn'
         call vern y,'CELLdigit'
         @.r.c=y
         end

    when  abb('COLumn')  then                /*do ROW (grid) placement. */
         do
         n=nai()
         y=na()
         call vern n,'column'
         ly=length(y)
         if ly>9  then call er 30,y 'column'n '1───>9'

                         do j=1  for ly
                         x=substr(y,j,1)
                         if x=='' | x=="_" | x=='*' | x=="."  then iterate
                         if \isInt(x)  then call er 92,x 'cell_for_column'n
                         @.j.n=x
                         end   /*j*/
         end

    when  abb('ROW')  then                   /*do ROW (grid) placement. */
         do
         n=nai()
         y=na()
         call vern n,'row'
         ly=length(y)
         if ly>9  then call er 30,y 'row'n '1───>9'

                         do j=1  for ly
                         x=substr(y,j,1)
                         if x=='' | x=="_" | x=='*' | x=="."  then iterate
                         if \isInt(x)  then call er 92,x 'cell_for_row_'n
                         @.n.j=x
                         end   /*j*/
         end

    when abbn('CLearscreen')        then clear=no()
    when abbn('HIGHLightsingles')   then highLight=no()
    when abbn('PRUNEALL')           then pruneall=no()
    when abbn('PRUNEONLYs')         then pruneonly=no()
    when abbn('PRUNEEXclusives')    then pruneexcl=no()
    when abbn('PRUNELINEs')         then pruneline=no()
    when abbn('PRUNEMATches')       then prunemats=no()
    when abbn('PRUNESINGles')       then prunesing=no()
    when abbn('SIMPle')             then simple=no()
    when  abb('SHOWBOXes')|,
          abb('SHOWBOXs')           then showbox=nai()
    when  abb('SHOWCELLs')          then showcell=translate(na(),,',')
    when  abb('SHOWCOLs')           then showcol=nai()
    when abbn('SHOWCOMBinations')   then showcomb=no()
    when abbn('SHOWGrid')           then showgrid=no()
    when abbn('SHOWINFOrmation')    then showinfo=no()
    when abbn('SHOWONELINE')        then showoneline=no()
    when abbn('SHOWPOSSibles') then showposs=no()
    when  abb('SHOWROWs')           then showrow=nai()
    when abbn('SHortgrid')          then short=no()
    when abbn('SOLvepuzzle')        then solve=no()

    otherwise                       call er 55,_o
    end   /*select*/
  end     /*while ops¬==''*/

if solve  then pruneall=1                    /*if solving, use PRUNEALL.*/

if pruneall  then do                         /*if pruneAll, set ON other*/
                  pruneexcl=1
                  pruneonly=1
                  pruneline=1
                  prunemats=1
                  prunesing=1
                  end

aprune = ,                                   /*is there a PRUNExxx on ? */
                  pruneexcl |,
                  pruneonly |,
                  pruneline |,
                  prunemats |,
                  prunesing

if highLight  then do                         /*HIGHLIGHTSINGLES opt on? */
                   hLl='-'
                   hLr='-'

                   if colors  then do
                                   hLl='('
                                   hLr=')'
                                   tops='.H=yell' tops
                                   end
                   end

tops=space(tops)
box.=

  do j=1  for 9                              /*build the box bounds.    */
  rr=(((j*3)%10)+1)*3-2                      /*compute row lower bound. */
  cc=(((j-1)//3)+1)*3-2                      /*compute col lower bound. */
  boxr.j=rr
  boxc.j=cc

                        do   r=rr  to rr+2   /*build boxes with cell #s.*/
                          do c=cc  to cc+2
                          rc=r || c
                          box.j=box.j rc
                          box.rc=j
                          end   /*c*/
                        end     /*r*/

  box.j=strip(box.j)
  end   /*j*/

rowlb.=10                                    /*row R,  low box number=b.*/
collb.=10                                    /*col R,  low box number=b.*/
boxlr.=10                                    /*box B,  low row number=r.*/
boxlc.=10                                    /*box B,  low col number=c.*/

  do   r=1  for 9
    do c=1  for 9
    rc=r || c
    b=box.rc                                 /*what box is this R,C in ?*/
    rowlb.r=min(rowlb.r,b)                   /*find min box # for row R.*/
    collb.c=min(collb.c,b)                   /*find min box # for col C.*/
    boxlr.b=min(boxlr.b,r)                   /*find min row # for box B.*/
    boxlc.b=min(boxlc.b,c)                   /*find min col # for box B.*/
    end   /*c*/
  end     /*r*/

 do j=1  to 9                                /*for each box, row, col...*/
 rowhb.j=rowlb.j+2                           /*compute row's high box #.*/
 colhb.j=collb.j+6                           /*compute col's high box #.*/
 boxhr.j=boxlr.j+2                           /*compute box's high row #.*/
 boxhc.j=boxlc.j+6                           /*compute box's high col #.*/
 end   /*j*/

if showgrid  then call showgrid 'the puzzle' /*show the grid to screen ?*/
if \validall()  then exit                    /*validate specified digits*/
tellinvalid=0                                /*don't tell err messages. */
!.=                                          /*nullify valid empty# list*/
call buildposs                               /*build possible values.   */
if showposs  then call showgrid 'puzzle possibles' /*show 1st possibles?*/
if \validate(1)  then exit                   /*validate the puzzle.     */

if showoneline  then do                      /*show grid as line line ? */
                     _=                      /*start with a clean slate.*/
                           do   r=1  for 9
                             do c=1  for 9
                             _=_ || @.r.c    /*build the string ...     */
                             end   /*c*/
                           end     /*r*/

                     _=translate(strip(_,'T'),".",' ')
                     if showinfo  then call $T 'one-line grid:'
                     call $T _
                     end

if aprune |,
   showposs then do
                 call pruneposs              /*go build poss, then prune*/
                 if showposs then call showgrid 'possibles' /*show grid.*/
                 if \validate(1) then exit   /*validate the puzzle.     */
                 end

if combos==1  then call $t sod 'puzzle solved.'
              else  if showcomb  then call $t 'combinations='comma(combos)
exit                                   /*stick a fork in it, we're done.*/

/*─────────────────────────────vern subroutine──────────────────────────*/
vern: parse arg v,w                          /*verify a digit for an opt*/
if v==''      then call er 35,v w
if \isInt(v)  then call er 92,v w
if v<1 | v>9  then call er 81,1 9 v w
return

/*─────────────────────────────buildposs subroutine─────────────────────*/
buildposs: !.=                               /*nullify possibilities.   */
combos=1

  do   rp=1  for 9                           /*build table of valid #s. */
    do cp=1  for 9                           /*step through each column.*/
    if @.rp.cp\==' '  then iterate           /*not blank?  Keep looking.*/

                          do jd=1  for 9     /*try each digit.          */
                          @.rp.cp=jd
                          if validx(rp,cp) then !.rp.cp=!.rp.cp || jd
                          end   /*jd*/

    combos=combos*length(!.rp.cp)            /*calculate # combinations.*/
    @.rp.cp=' '                              /*restore the point (blank)*/
    end      /*cp*/
  end        /*rp*/

return

/*─────────────────────────────showgrid subroutine──────────────────────*/
showgrid: parse arg title
if clear  then !cls                          /*clear the screen ?       */
if title\=='' & showinfo  then call $t !fn 'is showing' title
gtail=copies3(gridlb || gridlin || copies2(griduj || gridlin) || gridrb)
ghead=copies3(gridlt || gridlin || copies2(griddj || gridlin) || gridrt)
call tg ghead
gemp=copies3(copies3(gridbar || gridemp)gridbar)
grid=copies3(gridlj || gridlin || copies2(gridcross || gridlin)gridrj)
anyshow= \ ((showcell || showcol || showrow || showbox)\=='')

  do jr=1  for 9
  if \short  then call tg gemp
  gnum=

    do jc=1  for 9
    _=@.jr.jc
    if _\==' ' & highLight  then _=hLl || _ || hLr

    if _==' ' & ,
       showposs  then do
                      jrjc=jr || jc
                      showit=anyshow
                      if showcell\=='' then if wordpos(jrjc,showcell)\==0 then showit=1
                      if showcol\==''  then if pos(jc,showcol)\==0  then showit=1
                      if showrow\==''  then if pos(jr,showrow)\==0  then showit=1

                             do jb=1  while  showbox\==''
                             b=substr(showbox,jb,1)
                             if b==' '                   then leave
                             if wordpos(jrjc,box.b)\==0  then showit=1
                             end   /*jb*/

                      if showit  then _=strip(left(!.jr.jc,gridwidth),'T')
                      end

    gnum=gnum || gridbar || centre(_,gridwidth)
    if jc//3==0  then gnum=gnum || gridbar
    end   /*jc*/

  call tg gnum
  if \short  then call tg gemp

  if jr//3==0 then do
                   call tg gtail
                   if jr\==9  then call tg ghead
                   end
              else call tg grid
  end   /*jr*/

call $t
return

/*─────────────────────────────validate subroutine──────────────────────*/
validate:                                    /*are all empties possible?*/

  do   r=1  for 9                            /*step through each row.   */
    do c=1  for 9                            /*step through each column.*/

    if @.r.c==' ' & ,
       !.r.c==''  then do                     /*no legal digit here.     */
                       if arg(1)==1  then call $t sod "puzzle isn't valid !"
                       return 0
                       end
    end   /*c*/
  end     /*r*/                              /*sub requires possibles.  */

return 1                                     /*indicate puzzle is valid.*/

/*─────────────────────────────validall subroutine──────────────────────*/
validall:                                    /*validate all Q specified.*/

  do   r=1  for 9                            /*step through each row.   */
    do c=1  for 9                            /*step through each column.*/
    if @.r.c==' '  then iterate              /*if blank, then it's ok.  */
    y=                                       /*the rest of the row.     */
    rc=r||c
                do kc=1  for 9               /*compare to #s in column. */
                if kc\==c  then y=y|| @.r.kc /*build the rest of the row*/
                end   /*kc*/
    q=@.r.c
    if pos(q,y)\==0  then return tem(r,c,'row')    /*same # in same row?*/
    y=                                       /*the rest of the column.  */
                do kr=1  for 9               /*compare to #s in column. */
                if kr\==r then y=y || @.kr.c /*build the rest of the col*/
                end      /*kr*/

    if pos(q,y)\==0  then return tem(r,c,'col')    /*same # in same col?*/
    y=                                       /*the rest of the box.     */
    b=box.rc

      do   br=boxr.b  to boxr.b+2            /*compare to #s of the box.*/
        do bc=boxc.b  to boxc.b+2            /*build the rest of the box*/
        if br\==r & bc\==c  then y=y || @.br.bc
        end  /*bc*/
      end    /*br*/

    if pos(q,y)\==0  then return tem(r,c,'box')    /*same # in same box?*/
    end   /*c*/
  end     /*r*/

return 1                                     /*indicate  all  are valid.*/

/*─────────────────────────────validx subroutine────────────────────────*/
validx: arg r,c
rc=r || c
y=                                           /*the rest of the row.     */
           do kc=1  for 9                    /*compare to #s in column. */
           if kc\==c  then y=y || @.r.kc     /*build the rest of the row*/
           end   /*kc*/

q=@.r.c                                      /*get the digit at  r,c    */
if pos(q,y)\==0 then return tem(r,c,'row')   /*same number in same row ?*/
y=                                           /*the rest of the column.  */
      do kr=1  for 9                         /*compare to #s in column. */
      if kr\==r  then y=y || @.kr.c          /*build the rest of the col*/
      end   /*kr*/

if pos(q,y)\==0  then return tem(r,c,'col')  /*same  #  in same column ?*/
y=                                           /*the rest of the box.     */
b=box.rc

           do   br=boxr.b  to boxr.b+2       /*compare to #s of the box.*/
             do bc=boxc.b  to boxc.b+2       /*build the rest of the box*/
             if br==r & bc==c  then iterate
             y=y || @.br.bc
             end   /*br*/
           end     /*bc*/

if pos(q,y)\==0 then return tem(r,c,'box')   /*same # in same box ? */
return 1                                     /*indicate X (r,c) is valid*/

/*─────────────────────────────pruneposs subroutine─────────────────────*/
pruneposs: if \(prunesing | pruneexcl | prunemats | pruneline) then return
call buildposs

  do prunes=1
  call $t !fn 'is starting prune pass #' prunes
  found=0                                    /*indicate no prunes so far*/

  if prunesing then do                       /*prune puzzle for singles.*/
                    _=prunesing()            /*find any singles ?       */
                    found=found | _          /*track if anything found. */
                    if _ then if showgrid then call showgrid /*show grid*/
                    end

  if pruneexcl then do                       /*prune puzzle for singles.*/
                    _=pruneexcl()            /*find any excluives ?     */
                    found=found | _          /*track if anything found. */
                    if _ then if showgrid then call showgrid /*show grid*/
                    end

  if pruneonly then do                       /*prune puzzle for onlys.  */
                    _=pruneonly()            /*find any onlys ?         */
                    found=found | _          /*track if anything found. */
                    if _ then if showgrid then call showgrid /*show grid*/
                    end

  if prunemats then do jpm=2 to 8            /*prune puzzle for matches.*/
                    _=prunemats(jpm)         /*find any matches (len=j)?*/
                    found=found | _          /*track if anything found. */
                    if _ then if showgrid then call showgrid /*show grid*/
                    end

  if pruneline then do                       /*prune puzzle for lines.  */
                    _=pruneline()            /*find 2 or more on a line?*/
                    found=found | _          /*track if anything found. */
                    if _ then if showgrid then call showgrid /*show grid*/
                    end

  if \found then leave                       /*nothing found this time ?*/
  end    /*prunes*/

return

/*─────────────────────────────prunesing subroutine─────────────────────*/
prunesing: foundsing=0

    do   r=1  for 9
      do c=1  for 9
      _=length(!.r.c)                        /*get length of possible.  */
      if _==0   then iterate                 /*if null, then ignore it. */
      if _\==1  then iterate                 /*if not one digit, ignore.*/
      @.r.c=!.r.c                            /*it's 1 digit, a solution.*/
      !.r.c=                                 /*erase the old possible.  */
      foundsing=1
      call $t !fn 'found a single digit at cell' drc(r,c,@.r.c)
      end   /*c*/
    end     /*r*/

if foundsing  then call buildposs            /*re-build the possibles.  */
return foundsing

/*─────────────────────────────pruneexcl subroutine─────────────────────*/
pruneexcl: foundexcl=0

  do exclusives=1                            /*keep building possibles. */
    do   r=1  for 9
      do c=1  for 9
      z=!.r.c
      lz=length(z)                           /*get length of possible.  */
      if lz==0  then iterate                 /*if null, then ignore it. */
      y=
      rc=r || c
      b=box.rc

                do   br=boxr.b  to boxr.b+2  /*compare to #s of the box.*/
                  do bc=boxc.b  to boxc.b+2  /*build the rest of the box*/
                  if br==r & bc==c  then iterate
                  y=y || @.br.bc || !.br.bc
                  end   /*bc*/
                end     /*br*/

                                             /*test for reduction.      */
       do t=1  for lz
       q=substr(z,t,1)

       if pos(q,y)==0 then do
                           foundexcl=1
                           @.r.c=q           /*it's a singularity, a sol*/
                           !.r.c=            /*erase old possibleity.   */
                           call $t !fn 'found the digit' q,
                                  "by exclusiveness at cell" drc(r,c,z)
                           call buildposs    /*re-build the possibles.  */
                           iterate exclusives
                           end
       end  /*t*/
      end   /*c*/
    end     /*r*/

  leave
  end       /*exclusives*/

return foundexcl

/*─────────────────────────────prunemats subroutine─────────────────────*/
prunemats: foundmatch=0                      /*no matches found so far. */
parse arg L                                  /*length of match, L=2,pair*/

  do matches=1
    do   r=1  for 9
      do c=1  for 9
      _=length(!.r.c)                        /*get length of possible.  */
      if _==0   then iterate                 /*if null, then ignore it. */
      if _\==L  then iterate                 /*not right length, ignore.*/
      qq=!.r.c
      m=0                                    /*count of matches so far. */
             do _c=1  for 9                  /*nother match in same row?*/
             if qq==!.r._c  then m=m+1       /*up count if it's a match.*/
             end  /*_c*/

      if m>=L then do pc=1  for 9            /*squish other possibles.  */
                   old=!.r.pc                /*save the "old" value.    */
                   if old==qq   then iterate /*if match, then ignore it.*/
                   if old==''   then iterate /*if null poss, then ignore*/
                   new=squish(old,qq)        /*remove mat's digs from X.*/
                   if new==old  then iterate /*if no change,keep looking*/
                   !.r.pc=new                /*store new value into old.*/
                   foundmatch=1              /*indicate match was found.*/
                   call $t !fn 'is removing a'  old "from"  drc(r,pc,old),
                               'because of a match at'     drc(r,c,qq)
                   if length(new)==1 then do             /*reduce if L=1*/
                                          @.r.pc=new     /*store single.*/
                                          !.r.pc=        /*delete poss. */
                                          call buildposs /*re-build poss*/
                                          iterate matches  /*start over.*/
                                          end
                   end    /*pc*/
      m=0                                    /*count of matches so far. */

             do _r=1  for 9                  /*nother match in same col?*/
             if qq==!._r.c  then m=m+1       /*up count if it's a match.*/
             end   /*_r*/

      if m>=L then do pr=1  for 9            /*squish other possibles.  */
                   old=!.pr.c                /*save the "old" value.    */
                   if old==qq then iterate   /*if match, then ignore it.*/
                   if old=='' then iterate   /*if null poss, then ignore*/
                   new=squish(old,qq)        /*remove mat's digs from X.*/
                   if new==old then iterate  /*if no change,keep looking*/
                   !.pr.c=new                /*store new value into old.*/
                   foundmatch=1              /*indicate match was found.*/
                   call $t !fn 'is removing a'  old  "from" drc(pr,c,old),
                               'because of a match at' drc(r,c,qq)
                   if length(new)==1 then do             /*reduce if L=1*/
                                          @.pr.c=new     /*store single.*/
                                          !.pr.c=        /*delete poss. */
                                          call buildposs /*re-build poss*/
                                          iterate matches  /*start over.*/
                                          end
                   end   /*pr*/
      end                /*c*/
    end                  /*r*/

  leave
  end                    /*matches*/

return foundmatch

/*─────────────────────────────pruneonly subroutine─────────────────────*/
pruneonly: foundmatch=0                      /*no matches found so far. */

  do findonlys=1                             /*keep searching ...       */
  _row.=                                     /*build str for each row . */

           do   r=1  for 9
             do c=1  for 9
             if !.r.c\==''  then _row.r=_row.r !.r.c
             end   /*c*/
           end     /*r*/

  _col.=                                     /*build str for each boxcol*/

           do   c=1  for 9
             do r=1  for 9
             if !.r.c\==''  then _col.c=_col.c !.r.c
             end   /*r*/
           end     /*c*/

    do   r=1  for 9
      do c=1  for 9
      q=!.r.c
      if q==''  then iterate                 /*if empty, then ignore it.*/

        do j=1  to length(q)                 /*step through each digit. */
        k=substr(q,j,1)

        if kount1(k,_row.r) |,               /*is this the ONLY digit K?*/
           kount1(k,_col.c) then do i=1  to length(q)    /*prune others.*/
                                 foundmatch=1
                                 _=substr(q,i,1)
                                 if _==k  then iterate   /*if=K, ignore.*/
                                 o=squish(q,_)           /*remove others*/
                                 !.r.c=o
                                 call $t !fn  'removed part of an only',
                                              _   "from cell"   drc(r,c,q)
                                 if length(o)==1  then   /*reduce if L=1*/
                                   do
                                   @.r.c=o               /*store single.*/
                                   !.r.c=                /*delete poss. */
                                   call buildposs        /*re-build poss*/
                                   iterate findonlys     /*start over.  */
                                   end
                                 end   /*i*/
        end   /*j*/
      end     /*c*/
    end       /*r*/

  leave
  end     /*findonlys*/

return foundmatch

/*─────────────────────────────pruneline subroutine─────────────────────*/
pruneline: foundmatch=0                      /*no matches found so far. */

 do findlines=1                              /*keep searching ...       */
 _boxr.=                                     /*build str for each boxrow*/

           do   r=1  for 9
             do c=1  for 9
             rc=r || c
             b=box.rc
             if !.r.c\==''  then _boxr.r.b=strip(_boxr.r.b !.r.c)
             end   /*c*/
           end     /*r*/

  _boxc.=                                    /*build str for each boxcol*/

           do   c=1  for 9
             do r=1  for 9
             rc=r || c
             b=box.rc
             if !.r.c\==''  then _boxc.c.b=strip(_boxc.c.b !.r.c)
             end   /*r*/
           end     /*c*/

  do r=1  for 9                              /*search all rows for twins*/

    do b=rowlb.r  to rowhb.r                 /*for each row, search box.*/
    aline=_boxr.r.b                          /*get a  row  in the box.  */
    if aline==''  then iterate               /*if empty, ignore the line*/
    w=words(aline)                           /*W  is # of words in aline*/
    if w<2        then iterate               /*if < 2 words, ignore line*/

      do k=1  for 9                          /*search for each digit.   */
      f=pos(k,aline)                         /*pos of the 1st digit:  k */
      if f==0  then  iterate                 /*no dig k, so keep looking*/
      s=pos(k,aline,f+1)                     /*pos of the 2nd digit:  k */
      if s==0  then  iterate                 /*no 2nd k, so keep looking*/

        do jr=rowlb.r  to rowhb.r            /*look at the other 2 rows.*/
        if jr==r  then  iterate              /*if the same row, ignore. */
        if pos(k,_boxr.jr.b)\==0  then iterate k /*if no digit K, ignore*/
        end   /*jr*/
                                             /*found 2 Ks in row R box B*/
         do jb=rowlb.r  to rowhb.r           /*search boxes row R for K.*/
         if jb==b  then iterate              /*ignore if in the same box*/
         if pos(k,_boxr.r.jb)==0  then iterate
         foundmatch=1                        /*found a K in col C box JB*/

           do kc=1  for 9                    /*find which cell  K is in.*/
           rc=r || kc
           if box.rc==b        then iterate  /*ignore if in the same box*/
           _=!.r.kc
           if _==''            then iterate  /*ignore if no possible.   */
           if pos(k,_)==0      then iterate  /*if no digit  K,  ignore. */
           call $t  !fn   'is row-line pruning digit' k,
                          'from cell'    drc(r,kc,!.r.kc)
           !.r.kc=squish(_,k)                /*remove mat's digs from X.*/
           if length(!.r.kc)==1  then do     /*pruned down to one digit?*/
                                      @.r.kc=!.r.kc  /*make a true digit*/
                                      !.r.kc=        /*erase possibility*/
                                      call buildposs /*rebuild possibles*/.
                                      iterate findlines
                                      end
           end   /*kc*/
         end     /*jb*/
      end        /*k*/
    end          /*b*/
  end            /*r*/

  do c=1  for 9                              /*search all cols for twins*/

    do b=collb.c  to colhb.c  by 3           /*for each col, search box.*/
    aline=_boxc.c.b                          /*get a  column in the box.*/
    if aline==''  then iterate               /*if empty, ignore line*/
    w=words(aline)
    if w<2  then iterate                     /*if < 2 words, ignore line*/

      do k=1  for 9                          /*search for each digit.   */
      f=pos(k,aline)                         /*pos of the 1st digit:  k */
      if f==0  then iterate                  /*no dig k, so keep looking*/
      s=pos(k,aline,f+1)                     /*pos of the 2nd digit:  k */
      if s==0  then iterate                  /*no 2nd k, so keep looking*/

        do jc=boxlc.b  to boxhc.b            /*look at the other 2 cols.*/
        if jc==c  then iterate               /*if the same col, ignore. */
        if pos(k,_boxc.jc.b)\==0  then iterate k /*if no digit K, ignore*/
        end   /*jc*/
                                             /*found 2 Ks in col C box B*/
         do jb=collb.c  to colhb.c by 3      /*search boxes col C for K.*/
         if jb==b  then iterate              /*ignore if in the same box*/
         if pos(k,_boxc.c.jb)==0  then iterate
         foundmatch=1                        /*found a K in col C box JB*/

           do kr=1  for 9                    /*find which cell  K is in.*/
           rc=kr || c
           if box.rc==b       then iterate   /*ignore if in the same box*/
           _=!.kr.c
           if _==''           then iterate   /*ignore if no possible.   */
           if pos(k,_)==0     then iterate   /*if no digit  K,  ignore. */
           call $t !fn  'is col-line pruning digit'  k,
                        'from cell'    drc(kr,c,!.kr.c)
           !.kr.c=squish(_,k)                /*remove mat's digs from X.*/
           if length(!.kr.c)==1  then do     /*pruned down to one digit?*/
                                      @.kr.c=!.kr.c  /*make a true digit*/
                                      !.kr.c=        /*erase possibility*/
                                      call buildposs /*rebuild possibles*/.
                                      iterate findlines
                                      end
           end   /*kr*/
         end     /*jb*/
      end        /*k*/
    end          /*b*/
  end            /*c*/

 leave
 end     /*findlines*/

return foundmatch

/*────────────────────────────────────────────────────────────────────────────*/
commas: procedure;  parse arg _;   n=_'.9';    #=123456789;    b=verify(n,#,"M")
        e=verify(n,#'0',,verify(n,#"0.",'M'))-4
           do j=e  to b  by -3;   _=insert(',',_,j);    end  /*j*/;     return _
/*═════════════════════════════general 1-line subs══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════*/
!all:  !!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:   if symbol('!CALL')\=="VAR"  then !call=;   return !call
!env:   !env='ENVIRONMENT'; if !sys=='MSDOS' | !brexx | !r4 | !roo  then !env='SYSTEM'; if !os2  then !env='OS2'!env; !ebcdic=1=='f0'x; if !crx  then !env='DOS'; return
!fid:   parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .; call !sys; if !dos  then do;  _=lastpos('\',!fn); !fm=left(!fn,_); !fn=substr(!fn,_+1); parse var !fn !fn '.' !ft; end;   return word(0 !fn !ft !fm,1+('0'arg(1)))
!rex:   parse upper version !ver !vernum !verdate .; !brexx='BY'==!vernum; !kexx='KEXX'==!ver; !pcrexx='REXX/PERSONAL'==!ver | 'REXX/PC'==!ver; !r4='REXX-R4'==!ver; !regina='REXX-REGINA'==left(!ver,11); !roo='REXX-ROO'==!ver; call !env; return
!sys:   !cms=!sys=='CMS'; !os2=!sys=='OS2'; !tso=!sys=='TSO' | !sys=='MVS'; !vse=!sys=='VSE'; !dos=pos('DOS',!sys)\==0 | pos('WIN',!sys)\==0 | !sys=='CMD'; !crx=left(!sys,6)=='DOSCRX'; call !rex; return
!var:   call !fid; if !kexx  then return space(dosenv(arg(1))); return space(value(arg(1),,!env))
$fact!: procedure; parse arg x _ .; l=length(x); n=l-length(strip(x,'T',"!")); if n<=-n | _\=='' | arg()\==1  then return x; z=left(x,l-n); if z<0 | \isInt(z) then return x; return $fact(z,n)
$fact:  procedure; parse arg x _ .; arg ,n ! .; n=p(n 1); if \isInt(n)  then n=0; if x<-n | \isInt(x) | n<1 | _ || !\=='' | arg()>2 then return x || copies("!",max(1,n)); !=1; s=x//n; if s==0  then s=n;  do j=s  to x  by n; !=!*j; end;  return !
$sfxa:  parse arg ,s,m; arg u,c; if pos(left(s,2),u)\==0  then do j=length(s) to compare(s,c)-1 by -1; if right(u,j)\==left(c,j) then iterate; _=left(u,length(u)-j); if isNum(_)  then return m*_; leave; end;  return arg(1)
$sfxf:  parse arg y; if right(y,1)=='!'  then y=$fact!(y); if \isNum(y)  then y=$sfxz(); if isNum(y) then return y; return $sfxm(y)
$sfxm:  parse arg z; arg w; b=1000; if right(w,1)=='I'  then do; z=shorten(z); w=z; upper w; b=1024; end; p=pos(right(w,1),'KMGTPEZYXWVU'); if p==0 then return arg(1); n=shorten(z); r=num(n,f,1); if isNum(r)  then return r*b**p; return arg(1)
$sfxz:  return $sfxa($sfxa($sfxa($sfxa($sfxa($sfxa(y,'PAIRs',2),'DOZens',12),'SCore',20),'GREATGRoss',1728),'GRoss',144),'GOOGOLs',1e100)
$t:     if tops==''  then say arg(1);  else do; !call=']$T'; call "$T" tops arg(1); !call=; end;   return
ab:     arg ab,abl; return abbrev(ab,_,abl)
abb:    arg abbu; parse arg abb; return abbrev(abbu,_,abbl(abb))
abbl:   return verify(arg(1)'a',@abc,'M')-1
abbn:   parse arg abbn; return abb(abbn) | abb('NO'abbn)
abn:    arg ab,abl; return abbrev(ab,_,abl) | abbrev('NO'ab,_,abl+2)
copies2: return copies(arg(1),2)
copies3: return copies(arg(1),3)
drc:    procedure; parse arg r,c,p; _=r","c; if p\==''  then _=_ "("p')';   return _
er:     parse arg _1,_2; call '$ERR' "14"p(_1) p(word(_1,2) !fid(1)) _2; if _1<0  then return _1;   exit result
err:    call er '-'arg(1),arg(2); return ''
erx:    call er '-'arg(1),arg(2); exit ''
halt:   call er .1
int:    int=num(arg(1),arg(2)); if \isInt(int)  then call er 92,arg(1) arg(2);    return int/1
isInt:  return datatype(arg(1),'W')
isNum:  return datatype(arg(1),'N')
kount1: parse arg qd,string; k1=pos(qd,string); if k1==0  then return 0;   return pos(qd,string,k1+1)==0
lower:  return translate(arg(1),@abc,translate(@abc))
na:     if arg(1)\==''  then call er 01,arg(2); parse var ops na ops;    if na==''  then call er 35,_o;   return na
nai:    return int(na(),_o)
nail:   return squish(int(translate(na(),0,','),_o))
no:     if arg(1)\==''  then call er 01,arg(2); return left(_,2)\=='NO'
noValue:!sigl=sigl; call er 17,!fid(2) !fid(3) !sigl condition('D') sourceline(!sigl)
num:    procedure; parse arg x .,f,q; if x=='' then return x; if isNum(x) then return x/1; x=space(translate(x,,','),0); if \isNum(x) then x=$sfxf(x); if isNum(x) then return x/1; if q==1  then return x; if q==''  then call er 53,x f; call erx 53,x f
p:      return word(arg(1),1)
shorten:procedure; parse arg a,n; return left(a,max(0,length(a)-p(n 1)))
simple: return translate(arg(1),'.||--%<>AV'copies('+',25),"·│║─═☼◄►↑↓┤┐└┴┬├┼┘┌╔╗╚╝╟╢╞╡╫╪╤╧╥╨╠╣")
squish: return space(translate(arg(1),,word(arg(2) ',',1)),0)
syntax: !sigl=sigl; call er 13,!fid(2) !fid(3) !sigl !cal() condition('D') sourceline(!sigl)
tem:    parse arg r,c,w; if tellInvalid  then say '***error!*** row' r", col" c '('@.r.c") is a duplicate of another in the same" w'.';   return 0
tg:     arg tg; if simple  then tg=simple(tg); call $t gridindent || tg;   return
```



This REXX program makes use of   '''$ERR.REX'''   REXX program
which is used to write (display) error messages to the terminal screen,  with
supplemental text that identifies what program issued the error, and in some
cases, also identifies the failing REXX statement and some particulars about
the failure.

The   '''$ERR.T.REX'''   REXX program can be found here   ───►   [[$ERR.REX]].

changestr $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$@@@@@@@@@@@@@@@@@@@@@@@@##############%%%%%%%%%%%%%%%%%%%

{{out|output|   when using the input of: 

<tt> row 1 .5..7.89 row 2 9...3 row 3 1...89.4 row 4 ..9.....1 row 5 ..13.52 row 6 6.....5 row 7 .6.89...3 row 8 ....5...7 row 9 .98.2..5 pruneALL </tt>}}
<pre style="height:130ex">
$SUDOKU is showing the puzzle
   ┌───────┬───────┬───────┐┌───────┬───────┬───────┐┌───────┬───────┬───────┐
   │       │       │       ││       │       │       ││       │       │       │
   │       │   5   │       ││       │   7   │       ││   8   │   9   │       │
   │       │       │       ││       │       │       ││       │       │       │
   ├───────┼───────┼───────┤├───────┼───────┼───────┤├───────┼───────┼───────┤
   │       │       │       ││       │       │       ││       │       │       │
   │   9   │       │       ││       │   3   │       ││       │       │       │
   │       │       │       ││       │       │       ││       │       │       │
   ├───────┼───────┼───────┤├───────┼───────┼───────┤├───────┼───────┼───────┤
   │       │       │       ││       │       │       ││       │       │       │
   │   1   │       │       ││       │   8   │   9   ││       │   4   │       │
   │       │       │       ││       │       │       ││       │       │       │
   └───────┴───────┴───────┘└───────┴───────┴───────┘└───────┴───────┴───────┘
   ┌───────┬───────┬───────┐┌───────┬───────┬───────┐┌───────┬───────┬───────┐
   │       │       │       ││       │       │       ││       │       │       │
   │       │       │   9   ││       │       │       ││       │       │   1   │
   │       │       │       ││       │       │       ││       │       │       │
   ├───────┼───────┼───────┤├───────┼───────┼───────┤├───────┼───────┼───────┤
   │       │       │       ││       │       │       ││       │       │       │
   │       │       │   1   ││   3   │       │   5   ││   2   │       │       │
   │       │       │       ││       │       │       ││       │       │       │
   ├───────┼───────┼───────┤├───────┼───────┼───────┤├───────┼───────┼───────┤
   │       │       │       ││       │       │       ││       │       │       │
   │   6   │       │       ││       │       │       ││   5   │       │       │
   │       │       │       ││       │       │       ││       │       │       │
   └───────┴───────┴───────┘└───────┴───────┴───────┘└───────┴───────┴───────┘
   ┌───────┬───────┬───────┐┌───────┬───────┬───────┐┌───────┬───────┬───────┐
   │       │       │       ││       │       │       ││       │       │       │
   │       │   6   │       ││   8   │   9   │       ││       │       │   3   │
   │       │       │       ││       │       │       ││       │       │       │
   ├───────┼───────┼───────┤├───────┼───────┼───────┤├───────┼───────┼───────┤
   │       │       │       ││       │       │       ││       │       │       │
   │       │       │       ││       │   5   │       ││       │       │   7   │
   │       │       │       ││       │       │       ││       │       │       │
   ├───────┼───────┼───────┤├───────┼───────┼───────┤├───────┼───────┼───────┤
   │       │       │       ││       │       │       ││       │       │       │
   │       │   9   │   8   ││       │   2   │       ││       │   5   │       │
   │       │       │       ││       │       │       ││       │       │       │
   └───────┴───────┴───────┘└───────┴───────┴───────┘└───────┴───────┴───────┘

$SUDOKU is starting prune pass # 1
$SUDOKU found the digit 8 by exclusiveness at cell 2,2 (2478)
$SUDOKU found the digit 3 by exclusiveness at cell 3,7 (367)
$SUDOKU found the digit 5 by exclusiveness at cell 4,1 (234578)
$SUDOKU found the digit 8 by exclusiveness at cell 5,1 (478)
$SUDOKU found the digit 9 by exclusiveness at cell 6,4 (12479)
$SUDOKU found the digit 9 by exclusiveness at cell 5,9 (469)
$SUDOKU found the digit 5 by exclusiveness at cell 7,3 (2457)
$SUDOKU found the digit 1 by exclusiveness at cell 8,2 (1234)
$SUDOKU found the digit 9 by exclusiveness at cell 8,7 (469)
$SUDOKU found the digit 8 by exclusiveness at cell 8,8 (268)
$SUDOKU found the digit 8 by exclusiveness at cell 6,9 (48)
$SUDOKU found the digit 8 by exclusiveness at cell 4,6 (24678)
$SUDOKU found the digit 4 by exclusiveness at cell 4,7 (467)
$SUDOKU found the digit 2 by exclusiveness at cell 7,8 (12)
$SUDOKU found the digit 4 by exclusiveness at cell 9,9 (46)
$SUDOKU found the digit 6 by exclusiveness at cell 9,7 (16)
$SUDOKU found the digit 1 by exclusiveness at cell 7,7 (1)
$SUDOKU found the digit 1 by exclusiveness at cell 2,8 (167)
$SUDOKU found the digit 7 by exclusiveness at cell 2,7 (7)
 ∙
 ∙
 ∙
   some output elided ∙∙∙
 ∙
 ∙
 ∙ 
   ┌───────┬───────┬───────┐┌───────┬───────┬───────┐┌───────┬───────┬───────┐
   │       │       │       ││       │       │       ││       │       │       │
   │   4   │   5   │   3   ││   1   │   7   │   6   ││   8   │   9   │   2   │
   │       │       │       ││       │       │       ││       │       │       │
   ├───────┼───────┼───────┤├───────┼───────┼───────┤├───────┼───────┼───────┤
   │       │       │       ││       │       │       ││       │       │       │
   │   9   │   8   │   6   ││   4   │   3   │   2   ││   7   │   1   │   5   │
   │       │       │       ││       │       │       ││       │       │       │
   ├───────┼───────┼───────┤├───────┼───────┼───────┤├───────┼───────┼───────┤
   │       │       │       ││       │       │       ││       │       │       │
   │   1   │   2   │   7   ││   5   │   8   │   9   ││   3   │   4   │   6   │
   │       │       │       ││       │       │       ││       │       │       │
   └───────┴───────┴───────┘└───────┴───────┴───────┘└───────┴───────┴───────┘
   ┌───────┬───────┬───────┐┌───────┬───────┬───────┐┌───────┬───────┬───────┐
   │       │       │       ││       │       │       ││       │       │       │
   │   5   │   3   │   9   ││   2   │   6   │   8   ││   4   │   7   │   1   │
   │       │       │       ││       │       │       ││       │       │       │
   ├───────┼───────┼───────┤├───────┼───────┼───────┤├───────┼───────┼───────┤
   │       │       │       ││       │       │       ││       │       │       │
   │   8   │   7   │   1   ││   3   │   4   │   5   ││   2   │   6   │   9   │
   │       │       │       ││       │       │       ││       │       │       │
   ├───────┼───────┼───────┤├───────┼───────┼───────┤├───────┼───────┼───────┤
   │       │       │       ││       │       │       ││       │       │       │
   │   6   │   4   │   2   ││   9   │   1   │   7   ││   5   │   3   │   8   │
   │       │       │       ││       │       │       ││       │       │       │
   └───────┴───────┴───────┘└───────┴───────┴───────┘└───────┴───────┴───────┘
   ┌───────┬───────┬───────┐┌───────┬───────┬───────┐┌───────┬───────┬───────┐
   │       │       │       ││       │       │       ││       │       │       │
   │   7   │   6   │   5   ││   8   │   9   │   4   ││   1   │   2   │   3   │
   │       │       │       ││       │       │       ││       │       │       │
   ├───────┼───────┼───────┤├───────┼───────┼───────┤├───────┼───────┼───────┤
   │       │       │       ││       │       │       ││       │       │       │
   │   2   │   1   │   4   ││   6   │   5   │   3   ││   9   │   8   │   7   │
   │       │       │       ││       │       │       ││       │       │       │
   ├───────┼───────┼───────┤├───────┼───────┼───────┤├───────┼───────┼───────┤
   │       │       │       ││       │       │       ││       │       │       │
   │   3   │   9   │   8   ││   7   │   2   │   1   ││   6   │   5   │   4   │
   │       │       │       ││       │       │       ││       │       │       │
   └───────┴───────┴───────┘└───────┴───────┴───────┘└───────┴───────┴───────┘

$SUDOKU is starting prune pass # 4
 sudoku puzzle solved.

```



###  REXX version 2 

{{trans|PL/I]]

```rexx
  Parse Arg g.0fid
  Select
    When g.0fid='?' Then Do
      Say 'This program solves any (valid) SUDOKU puzzle'
      Say 'Specify the name of the file containing the puzzle as argument'
      Exit
      End
    When g.0fid='' Then
      Call exit 'no input specified'
    When lines(g.0fid)=0 Then
      Call exit 'specified input does not exist'
    Otherwise
      Nop
    End
  instr=''
  Do While lines(g.0fid)>0
    instr=instr||linein(g.0fid)
    End
  Call lineout g.0fid
  digits='123456789'
  buffer=translate(instr,digits'000',digits'0.x'||xrange('00'x,'ff'x))
  buffer=space(buffer,0)
  If length(buffer)<>81 Then
    Call exit 'invalid input from file' g.0fid
  Call set_geometry

  posbit.=copies('0',9)
  z=posbit.0
  d.z=0

  Do i=1 To 9
    posbit.i=overlay('1',posbit.i,i,1)
    z=posbit.i
    d.z=i
    End

  Do r=1 To 9
    Do c=1 To 9
      Parse Var buffer d +1 buffer
      matrix.r.c=posbit.d
      End
    End

  nn=0
  Call show_matrix 'input from' g.0fid
  res=solve()
  If res Then Do
    Call dbg 'nn='format(nn,5) 'res='res
    Call show_matrix 'solution'
    End
  Else
    Say 'impossible'
  Exit

solve: Procedure Expose g. matrix. posbit. nn box. boxlr. boxlc.
  nn=nn+1
  Call dbg 'solve nn='format(nn,5)
  do i = 1 to 9
    do j = 1 to 9
      if matrix.i.j=posbit.0 Then
        Leave i
      End
    End
  If i>9 Then Do
    do i = 1 to 9
      do j = 1 to 9
        k = pos('1',matrix.i.j)
        Call dbg 'sudoku',
                       Format(nn,9) Format(i,9) Format(j,9) Format(k,9)
        matrix.i.j=posbit.0
        result_=neg(or(any_col(i),any_row(j),any_box(i,j)))
        If substr(result_,k,1)=0 Then
          Return 0
        matrix.i.j=posbit.k
        End
      End
    Return 1
    End
  Else Do
    result_=neg(or(any_col(i),any_row(j),any_box(i,j)))
    Call dbg 'resulta='result_
    k=0;
    do Until k=0
      Call dbg 'resultb='result_
      k=pos('1',result_,k+1)
      Call dbg 'k='Format(k,2)Format(i,2)Format(j,2)
      if k>0 then Do;
        matrix.i.j=posbit.k
        Call dbg 'setting matrix('i','j')->'k
        res=solve()
        Call dbg 'A nn='format(nn,5) 'res='res
        if res then
          return 1
        else Do;
          matrix.i.j=posbit.0
          Call dbg 'setting matrix('i','j')->'0
          End;
        end;
      end;
    return 0
    end;

set_geometry:
  box.=''
  Do j=1 To 9                       /* build the box bounds.         */
    rr=(((j*3)%10)+1)*3-2           /* compute row lower bound.      */
    cc=(((j-1)//3)+1)*3-2           /* compute col lower bound.      */
    boxr.j=rr
    boxc.j=cc
    Do r=rr To rr+2                 /* build boxes with cell #s.     */
      Do c=cc To cc+2
        box.r.c=j
        End
      End
    End                             /* j                             */
  rowlb.=10                         /* row R,  low box number=b.     */
  collb.=10                         /* col R,  low box number=b.     */
  boxlr.=10                         /* box B,  low row number=r.     */
  boxlc.=10                         /* box B,  low col number=c.     */

  Do r=1 To 9
    Do c=1 To 9
      b=box.r.c                     /* what box is this R,C in ?     */
      rowlb.r=min(rowlb.r,b)        /* find min box # for row R.     */
      collb.c=min(collb.c,b)        /* find min box # for col C.     */
      boxlr.b=min(boxlr.b,r)        /* find min row # for box B.     */
      boxlc.b=min(boxlc.b,c)        /* find min col # for box B.     */
      End
    End
Return

any_col: Procedure Expose matrix.
  Parse Arg r
  res='000000000'
  Do c=1 To 9
    p=pos('1',matrix.r.c)
    If p>0 Then
      res=overlay('1',res,p,1)
    End
  Return res

any_row: Procedure Expose matrix.
  Parse Arg c
  res='000000000'
  Do r=1 To 9
    p=pos('1',matrix.r.c)
    If p>0 Then
      res=overlay('1',res,p,1)
    End
  Return res

any_box: Procedure Expose matrix. box. boxlr. boxlc.
  Parse Arg r,c
  b=box.r.c
  res='000000000'
  Do r=boxlr.b For 3
    Do c=boxlc.b For 3
      p=pos('1',matrix.r.c)
      If p>0 Then
        res=overlay('1',res,p,1)
      End
    End
  Return res

or: Procedure
  res='000000000'
  Do ia=1 To 3
    a=arg(ia)
    Do p=1 To 9
      If substr(a,p,1)=1 Then
        res=overlay('1',res,p,1)
      End
    End
  Return res

neg: Procedure
  Parse Arg s
  res=''
  Do p=1 To 9
    If substr(s,p,1)=1 Then
      res=res'0'
    Else
      res=res'1'
    End
  Return res

o: Say arg(1)
   Return

show_matrix:
  Call o arg(1)
  Do r=1 To 9
    ol=''
    Do c=1 To 9
      m=matrix.r.c
      ol=ol||d.m' '
      If c//3=0 Then
        ol=ol' '
      End
    Call o ol
    If r//3=0 Then
      Call o ' '
    End
  Return

dbg:
  If debug=1 Then
    Say arg(1)
  Return

exit: Say '*ERROR*' arg(1)
```

{{out}}

```txt
input from d:\_sudoku\in\sdk001.in
4 6 0  0 0 1  0 0 0
0 0 2  0 9 6  0 0 0
0 3 0  0 0 0  0 6 8

0 0 0  0 0 0  0 3 7
0 0 0  6 0 7  0 0 0
5 1 0  0 0 0  0 0 0

8 4 0  0 0 0  0 5 0
0 0 0  7 1 0  9 0 0
0 0 0  3 0 0  0 2 4

solution
4 6 5  8 3 1  2 7 9
7 8 2  4 9 6  3 1 5
1 3 9  5 7 2  4 6 8

6 9 4  1 2 5  8 3 7
3 2 8  6 4 7  5 9 1
5 1 7  9 8 3  6 4 2

8 4 1  2 6 9  7 5 3
2 5 3  7 1 4  9 8 6
9 7 6  3 5 8  1 2 4
```



###  REXX version 3 

This is version 1 (thanks) cut to the essentials, restructured, and modified

```rexx
/* REXX ---------------------------------------------------------------
* program to solve nearly every SUDOKU puzzle
* using a number of strategies learned from REXX version 1
*           and one rather efficient algorithm created by me: prunewalt
* see solve: for details
* Tested with Regina and ooRexx
* See version 2 for a program that solves every valid SUDOKU
*--------------------------------------------------------------------*/
  Signal on Halt
  Signal on Novalue
  Signal on Syntax
  Parse Arg fid debug
  Select
    When fid='?' Then Do
      Say 'This program solves many (nearly every?) SUDOKU puzzle'
      Say 'rexx sudoku file [DEBUG]'
      Say 'Input: file.in'
      Say 'Debug: file.dbg'
      Say 'Known: file.sol'
      Say 'Incomplete solution (if applicable): fileF.in'
      Say 'Output: on screen'
      Say 'Adapt subroutine get_input if necessary!'
      Say 'See version 2 for a brute force program',
                                           'solving EVERY valid SUDOKU'
      Exit
      End
    When fid='' Then Do
      Say 'Input file not specified'
      Say 'Enter "rexx sudoku ?" for help'
      Exit
      End
    Otherwise
      Nop
    End

  g.=0
  g.0debug=(translate(debug)='DEBUG')

  Call get_input fid            /* get input and set up file names   */
                                /* Please adapt to your environment  */
  Numeric Digits 50             /* because of huge # of combinations */

  Call set_geometry

  Call show_aa 'the puzzle'         /* show the grid to screen       */
  Call build_poss                   /* build possible values         */
  g.0todo_init=g.0todo
  Call show_poss 'puzzle possibles' /* show 1st possibles            */

  Call solve                        /* now try to solve it           */

  If g.0todo=0 Then Do              /* no cell left empty            */
    Call o g.0fid 'puzzle solved.'  /* tell it                       */
    Call o left(g.0fid,12) 'puzzle solved.'
    Call show_aa 'solved'           /* show the solution             */
    End
  Else Do                           /* some cells couldn't be filled */
    Call show_poss 'failed'         /* show the possibilities left   */
    Call o left(g.0fid,12) 'puzzle failed g.0todo='g.0todo
    Call show_aa 'failed','.'       /* show the partly solved puzzle */
    End

  Call write_summary

  Exit

build_poss: Procedure Expose g. s. aa. poss.,
                                         box. boxr. boxc. boxlr. boxlc.
/*---------------------------------------------------------------------
* aa.r.c contains the known digits
* we determine which digits are possible for empty positions
* and put them into poss.r.c
*--------------------------------------------------------------------*/
  all='123456789'
  Parse Value '' With dr. dc. db.   /* initialize strings built here */
  poss.=''
  Do r=1 To 9
    Do c=1 To 9
      dr.r=dr.r||aa.r.c             /* all digits in row r           */
      End
    End
  Do c=1 To 9
    Do r=1 To 9
      dc.c=dc.c||aa.r.c             /* all digits in col c           */
      End
    End
  Do b=1 To 9
    Do r=boxlr.b For 3
      Do c=boxlc.b For 3
        db.b=db.b||aa.r.c           /* all digits in box b           */
        End
      End
    End

  g.0tot=0                          /* total # of possible digits    */
  g.0todo=0                         /* number of cells to be filled  */
  g.0comb=1                         /* # of possible combinations    */

  Do r=1 To 9
    Do c=1 To 9                     /* do this for every r.c         */
      b=box.r.c                     /* the box this cell is in       */
      If aa.r.c='' Then Do          /* cell not yet known            */
        used=compress(dr.r||dc.c||db.b) /* all digits already used   */
        poss.r.c=diff(all,used)     /* all others are still possible */
        g.0todo=g.0todo+1           /* number of cells yet to fill   */
        g.0tot=g.0tot+length(poss.r.c)
        g.0comb=g.0comb*length(poss.r.c)
        End
      End
    End
  If g.0sol<>'' Then                /* if we know the solution       */
    Call check_all                  /* check if everything fits      */
  Return

solve:
/*---------------------------------------------------------------------
* Use several algorithms to determine which cell(s) can safely be set
* prunewalt: if a digit occurs just once
*                    in a row's, col's or box's list of possible digits
* prunesing: if there is only one possible digit in a cell
* pruneexcl ) Algorithms of version 1 only partly understood (by me!)
* prunemats ) but faithfully restructured to avoid many Iterate
* pruneline )                                             instructions.
*--------------------------------------------------------------------*/
  Call build_poss                   /* re-build the possibles        */
  Do g.0pass=1 By 1 Until g.0todo=0
    Call o g.0fn 'is starting prune pass #' g.0pass
    found_pass=0

    found=prunewalt()               /* find any singles ?            */
    found_pass=found_pass+found
    If g.0todo=0 Then Leave
    If found>0 Then
      Call show_grid 'after prunewalt'

    found=prunesing()               /* find any singles ?            */
    found_pass=found_pass+found
    If g.0todo=0 Then Leave
    If found>0 Then
      Call show_grid 'after prunesing'

    found=pruneexcl()               /* find any excluives ?          */
    found_pass=found_pass+found
    If g.0todo=0 Then Leave
    If found>0 Then
      Call show_grid 'after pruneexcl'

    found=prunemats(2)              /* find any matches (len=2)      */
    found_pass=found_pass+found
    If g.0todo=0 Then Leave
    If found>0 Then
      Call show_grid 'after prunemats'

    found=pruneline()               /* find 2 or more on a line?     */
    found_pass=found_pass+found
    If g.0todo=0 Then Leave
    If found>0 Then
      Call show_grid 'after pruneline'

    If found_pass>0 Then Do
      Call o found_pass 'hits in g.0pass' g.0pass
      If g.0debug Then
        Call write_summary
      End
    Else Do
      Call o 'Nothing found in g.0pass' g.0pass
      Leave
      End
    End                             /* prunes                        */
  Return

prunewalt: Call o '>>>>>> prunewalt tot='g.0tot 'todo='g.0todo
/*---------------------------------------------------------------------
* find digits that have only one occurrence in a row or column
* row_poss.r digits in row r
* col_poss.c digits in column c
* box_poss.b digits in box b
*--------------------------------------------------------------------*/
  foundwalt=0                       /* no matches found so far.      */
  Do Until changed=0                /* keep searching ...            */
    changed=0                       /* changes made in this routine  */
    row_poss.=''                    /* build str for each row        */
    col_poss.=''                    /* build str for each column     */
    box_poss.=''                    /* build str for each box        */

    Do r=1 To 9
      Do c=1 To 9
        b=box.r.c
        If poss.r.c\=='' Then Do
          row_poss.r=row_poss.r poss.r.c
          col_poss.c=col_poss.c poss.r.c
          box_poss.b=box_poss.b poss.r.c
          End
        End
      End
    rl=''
    Do r=1 To 9
      ol='row'r':'
      Do d=1 To 9
        cnt=count(d,row_poss.r)
        ol=ol cnt
        If cnt=1 Then Do
          rl=rl r
          dr.r=d
          End
        End
      End
    cl=''
    Do c=1 To 9
      ol='col'c':'
      Do d=1 To 9
        cnt=count(d,col_poss.c)
        ol=ol cnt
        If cnt=1 Then Do
          dc.c=d
          cl=cl c
          End
        End
      End

    bl=''
    Do b=1 To 9
      ol='box'||b':'
      Do d=1 To 9
        cnt=count(d,box_poss.b)
        ol=ol cnt
        If cnt=1 Then Do
          z=r'.'c
          db.z=d
          bl=bl z
          End
        End
      End

    Do While rl<>''
      Parse Var rl r rl
      Do c=1 To 9
        If pos(dr.r,poss.r.c)>0 Then Do
          Call set_aa r,c,dr.r,'prunewalt new R'
          changed=changed+1
          foundwalt=foundwalt+1
          Call build_poss           /* re-build the possibles        */
          End
        End
      End
    Do While cl<>''
      Parse Var cl c cl
      Do r=1 To 9
        If pos(dc.c,poss.r.c)>0 Then Do
          Call set_aa r,c,dc.c,'prunewalt new C'
          changed=changed+1
          foundwalt=foundwalt+1
          Call build_poss           /* re-build the possibles        */
          End
        End
      End
    Do While bl<>''
      Parse Var bl z cb bl
      Parse Var z rb '.' cb
      Do r=boxlr.b For 3
        Do c=boxlc.b For 3
          If r=rb &,
             c=cb &,
             pos(db.z,poss.r.c)>0 Then Do
            Say 'z='r 'c='c 'poss.'r'.'c'='poss.r.c 'db.b='db.b
            Call set_aa r,c,db.b,'prunewalt new B'
            changed=changed+1
            foundwalt=foundwalt+1
            Call build_poss         /* re-build the possibles        */
            End
          End
        End
      End
    End
  Call show_poss 'after prunewalt'

  If foundwalt>0 Then
    Call o '>>>>>> prunewalt foundwalt='foundwalt
  Else
    Call o '>>>>>> prunewalt found nothing'
  g.0foundwalt=g.0foundwalt+foundwalt
  Return foundwalt

prunesing: Call o '>>>>>> prunesing tot='g.0tot 'todo='g.0todo
/*---------------------------------------------------------------------
* look if there are cells with a single possible digit and put these
* into the grid. Return the number of changes made.
*--------------------------------------------------------------------*/
  foundsing=0
  Do r=1 To 9
    Do c=1 To 9
      If length(poss.r.c)=1 Then Do /* only possible digit           */
        Call set_aa r,c,poss.r.c,'prunesing' /* put it into the cell */
        foundsing=foundsing+1       /* indicate success              */
        End
      End
    End
  If foundsing>0 Then Do
    Call build_poss                 /* re-build the possibles        */
    Call o '>>>>>> prunesing foundsing='foundsing
    End
  Else
    Call o '>>>>>> prunesing found nothing'
  g.0foundsing=g.0foundsing+foundsing
  Return foundsing

pruneexcl: Call o '>>>>>> pruneexcl tot='g.0tot 'todo='g.0todo
/*---------------------------------------------------------------------
*
*--------------------------------------------------------------------*/
  foundexcl=0
  Do exclusives=1                   /* keep building possibles.      */
    Do r=1 For 9
      Do c=1 For 9
        z=poss.r.c
        lz=length(z)                /* get length of possible.       */
        If lz>0 Then Do
          y=''
          b=box.r.c
          Do br=boxr.b For 3
            Do bc=boxc.b For 3      /* for every cell in box b       */
              If br'.'bc<>r'.'c Then
                y=y||aa.br.bc||poss.br.bc
              End
            End
          Do t=1 For lz
            q=substr(z,t,1)
            If pos(q,y)==0 Then Do
              foundexcl=foundexcl+1
              If aa.r.c=q Then
                Call o 'pruneexcl ??? aa.'r'.'c'='q 'already set'
              Call o 'foundexcl='foundexcl
              Call set_aa r,c,q,'pruneexcl' /* a singularity, a sol  */
              Call o 'pruneexcl found the digit' q,
                                  'by exclusiveness at cell' drc(r,c,z)
              Call build_poss       /* re-build the possibles        */
              Iterate exclusives
              End
            End
          End
        End
      End
    Leave
    End
  If foundexcl>0 Then Do
    Call o '>>>>>> pruneexcl foundexcl='foundexcl
    End
  Else
    Call o '>>>>>> prunesing found nothing'
  g.0foundexcl=g.0foundexcl+foundexcl
  Return foundexcl

prunemats: Call o '>>>>>> prunemats tot='g.0tot 'todo='g.0todo
/*---------------------------------------------------------------------
* This example illustrates the working of this strategy:
* Column 1    2    3    4    5    6    7    8    9
* Row 7: .    .    1369 29   26   29   137  .    136
* remove 29 from drc 7.3=1369 giving drc 7.3=136 (matches 7.4 7.6)
* Row 7: .    .    136  29   26   29   137  .    136
* remove 29 from drc 7.5=26   giving drc 7.5=6   (matches 7.4 7.6) HIT
* Row 7: .    .    136  29   6    29   137  .    136
* Row 7: .    .    139  29   .    29   137  .    13
* remove 29 from drc 7.3=139  giving drc 7.3=13  (matches 7.4 7.6)
* Row 7: .    .    13   29   .    29   137  .    13
* remove 13 from drc 7.7=137  giving drc 7.7=7   (matches 7.9 7.3) HIT
* Row 7: .    .    13   29   .    29   7    .    13
* Row 7: .    .    139  29   .    29   .    .    13
*--------------------------------------------------------------------*/
  setmats=0
  foundmats=0                       /* no matches found so far.      */
  Parse Arg l                       /* length of match, L=2,pair     */
  Do matches=1
    Do r=1 For 9
      Do c=1 For 9
        _=length(poss.r.c)          /* get length of possible.       */
        If _=l Then Do
          qq=poss.r.c
          m=0                       /* count of matches so far.      */
          mla=r'.'c
          Do _c=1 For 9             /* a match in same row?          */
            If _c<>c &,
               qq==poss.r._c Then Do
              m=m+1                 /* up count if it's a match.     */
              mla=mla r'.'_c
              End
            End
          If m>0 Then Do
            Call o 'AAAA mla='mla
            Call show_poss_r r
            Do pc=1 For 9           /* remove other possibles.       */
              old=poss.r.pc         /* save the "old" value.         */
              If old<>qq & old<>'' Then Do
                new=diff(old,qq)    /* remove mat's digs from X.     */
                Call o 'AAAA' r'.'pc':'old '-' qq '-->' new
                If new<>old Then Do
                  If length(new)=1 Then tag='HIT'; Else tag=''
                  Call o 'remove' qq 'from' drc(r,pc,old),
                           'giving' drc(r,pc,new) '(matches' mla')' tag
                  poss.r.pc=new     /* store new value into old.     */
                  Call show_poss 'AAAA1'
                  Call show_poss_r r
                  setmats=setmats+1 /* indicate match was found.     */
                  If length(new)==1 Then Do /*reduce if L=1*/
                    Call set_aa r,pc,new,'prunemats R' /*store single*/
                    foundmats=foundmats+1 /* indicate match was found*/
                    Call build_poss /* re-build the possibles        */
                    Call show_poss 'AAAA2'
                    Call show_poss_r r
                    Iterate matches       /* start over.             */
                    End
                  End
                End
              End
            End
          m=0
          mlb=r'.'c
          Do _r=1 For 9
            If _r<>r &,
               qq==poss._r.c Then Do
              m=m+1
              mlb=_r'.'c
              End
            End

          If m>0 Then Do
            Call o 'BBBB mlb='mlb
            Call show_poss_r r
            Do pr=1 For 9
              old=poss.pr.c
              If old<>qq & old<>'' Then Do
                new=diff(old,qq)
                Call o 'BBBB' pr'.'c':'old '-' qq '-->' new
                If new<>old Then Do
                  If length(new)=1 Then tag='HIT'; Else tag=''
                  Call o 'remove' qq 'from' drc(pr,c,old),
                           'giving' drc(pr,c,new) '(matches' mlb')' tag
                  poss.pr.c=new
                  Call show_poss_r r
                  Call show_poss 'BBBB1'
                  setmats=setmats+1
                  If length(new)==1 Then Do
                    foundmats=foundmats+1
                    Call set_aa pr,c,new,'prunemats C'
                    Call build_poss /* re-build the possibles        */
                    Call show_poss 'BBBB2'
                    Call show_poss_r r
                    Iterate matches
                    End
                  End
                End
              End
            End
          End
        End
      End
    Leave
    End

  If foundmats>0 Then Do
    Call o '>>>>>> prunemats foundmats='foundmats
    End
  Else
    Call o '>>>>>> prunesing found nothing'
  g.0foundmats=g.0foundmats+foundmats
  Return setmats

pruneline: Call o '>>>>>> pruneline tot='g.0tot 'todo='g.0todo
/*---------------------------------------------------------------------
*
*--------------------------------------------------------------------*/
  Call show_poss ' vor pruneline'
  pruned=0
  foundline=0                       /* no matches found so far.      */
  Do Until changes=0                /* terminate if no changes made  */
    changes=0                       /* initialize number of changes  */
    poss_boxr.=''                   /* build str for each boxrow     */
    poss_boxc.=''                   /* build str for each boxcol     */
    Do r=1 To 9
      Do c=1 To 9
        b=box.r.c
        If poss.r.c\=='' Then Do
          poss_boxr.r.b=strip(poss_boxr.r.b poss.r.c)
          poss_boxc.c.b=strip(poss_boxc.c.b poss.r.c)
          End
        End
      End
    Do r=1 To 9                     /* search all rows for twins     */
      Do cb=1 To 7 By 3             /* 3 boxes containing row r      */
        b=box.r.cb
        aline=poss_boxr.r.b         /* all poss strings: row r box b */
        If words(aline)>=2 Then Do  /* more than one                 */
          Call o 'aline' r'.'||b'='aline '(cb='cb')'
          Do k=1 To 9               /* search for each digit.        */
            If count(k,aline)>=2 Then Do /* more than one occurrence */
              Do jr=rowlb.r For 3   /* look at the other 2 rows.     */
                If jr<>r &,
                   pos(k,poss_boxr.jr.b)>0 Then /* digit k found     */
                  Iterate k         /* continue with the next digit  */
                End
              Do jb=rowlb.r For 3   /* search boxes of row R for K.  */
                If jb<>b &,
                   pos(k,poss_boxr.r.jb)>0 Then Do
                  Do kc=1 To 9      /* find which cell  K is in.     */
                    If box.r.kc<>b Then Do
                      If poss.r.kc<>'' &,
                         pos(k,poss.r.kc)>0 Then Do
                        old=drc(r,kc,poss.r.kc)
                        row_a=poss_r(r)
                        poss.r.kc=diff(poss.r.kc,k) /* remove digit k*/
                        Call o g.0fn 'row' r': removing' k 'from' old,
                                     'resulting in' drc(r,kc,poss.r.kc)
                        row_b=poss_r(r)
                        Call o '  ' row_a
                        Call o '>>' row_b
                        pruned=pruned+1
                        If length(poss.r.kc)==1 Then Do
                          Call set_aa r,kc,poss.r.kc,'pruneline R'
                          foundline=foundline+1
                          Call build_poss /* re-build the possibles  */
                          changes=changes+1
                          End
                        End
                      End
                    End
                  End
                End
              End
            End
          End
        End
      End

    Do c=1 To 9                     /* search all cols for twins     */
      Do b=collb.c By 3 For 3       /* for each col, search box.     */
        aline=poss_boxc.c.b
        If words(aline)>=2 Then Do
          Do k=1 To 9               /* search for each digit.        */
            If count(k,aline)>=2 Then Do
              Do jc=boxlc.b For 3   /* look at the other 2 cols.     */
                If jc<>c&pos(k,poss_boxc.jc.b)<>0 Then
                  Iterate k         /* if no digit K, ignore         */
                End                 /* jc                            */
                                    /*found 2 Ks in col C box B      */
              Do jb=collb.c By 3 For 3 /*search boxes col C for K.   */
                If jb<>b&pos(k,poss_boxc.c.jb)<>0 Then Do
                  Do kr=1 To 9      /* find which cell  K is in.     */
                    If box.kr.c<>b Then Do
                      If poss.kr.c>''&,
                         pos(k,poss.kr.c)>0 Then Do
                        old=drc(kr,c,poss.kr.c)
                        col_a=poss_c(c)
                        poss.kr.c=diff(poss.kr.c,k) /* remove digit k*/
                        Call o g.0fn 'col' c': removing' k 'from' old,
                                     'resulting in' drc(kr,c,poss.kr.c)
                        col_b=poss_c(c)
                        Call o '  ' col_a
                        Call o '>>' col_b
                        pruned=pruned+1
                        If length(poss.kr.c)==1 Then Do
                          Call set_aa kr,c,poss.kr.c,'pruneline C'
                          foundline=foundline+1
                          Call build_poss /* re-build the possibles  */
                          changes=changes+1
                          End
                        End
                      End
                    End
                  End
                End
              End
            End
          End
        End
      End
    End
  Call show_poss 'nach pruneline'
  If foundline>0 Then
    Call o '>>>>>> pruneline new foundline='foundline 'pruned='pruned
  Else
    Call o '>>>>>> pruneline new found nothing' 'pruned='pruned
  g.0foundline=g.0foundline+foundline
  Return foundline

show_grid:
/*---------------------------------------------------------------------
* show what's known so far
* and what's still to be done
*--------------------------------------------------------------------*/
  Parse Arg title
  Call show_aa title
  Call show_poss title
  Return

show_aa: Procedure Expose g. aa. s.
/*---------------------------------------------------------------------
* Show all cells that are known already
* and determine the number of cells yet to be filled (g.0todo)
*--------------------------------------------------------------------*/
  Parse Arg txt
  blank='.'
  Select
    When txt='the puzzle' |,        /* initial call                  */
         txt='solved' Then          /* final call (success)          */
      g.0say=1                      /* show on screen                */
    When txt='failed' Then Do       /* final call (failure)          */
      g.0say=1                      /* show on screen                */
      g.0fail=1                     /* write to incomplete solution  */
      End
    Otherwise
      g.0say=0                      /* don't show on screen          */
    End
  Call o txt                        /* write to dbg/screen/inco      */
  g.0todo=0
  Do r=1 To 9                       /* for all rows                  */
    ol=''
    Do c=1 To 9                     /* build a line                  */
      If aa.r.c='' Then Do
        g.0todo=g.0todo+1
        ol=ol blank
        End
      Else
        ol=ol aa.r.c
      If c//3=0 Then                /* a blank column                */
        ol=ol' '
      End
    Call o ol
    If r//3=0 Then                  /* a blank line                  */
     Call o ' '
    End
  g.0say=0                          /* reset the flags               */
  g.0fail=0
  If g.0todo>0 Then
    Call o right('to be done:',40) g.0todo
  Else
    Call o 'all done'
  Return

show_poss: Procedure Expose poss. g. s.
/*---------------------------------------------------------------------
* show all possible digits of the grid
*--------------------------------------------------------------------*/
  Parse Arg txt
  If g.0todo=0 Then
    Return
  Call o copies('-',70) 'todo='g.0todo
  Call o txt
  Do r=1 To 9
    ol=r
    Do c=1 To 9
      ol=ol left(poss.r.c,7)
      If c//3=0 Then
        ol=ol '|'
      End
    Call o ol
    If r//3=0 Then
      Call o ' '
    End
  Call o '       tot='g.0tot 'todo='g.0todo
  Call o 'combinations:' g.0comb
  Return

show_poss_r: Procedure Expose g. poss.
/*---------------------------------------------------------------------
* show possible digits in row r
'--------------------------------------------------------------------*/
  Parse Arg r
  Call o poss_r(r)
  Return

poss_r: Procedure Expose g. poss.
/*---------------------------------------------------------------------
* compute possible digits in row r
'--------------------------------------------------------------------*/
  Parse Arg r
  ol='Row' r':'
  Do c=1 To 9
    prc=poss.r.c
    If prc='' Then prc='.'
    ol=ol left(prc,6)
    End
  Return ol

show_poss_c: Procedure Expose g. poss.
/*---------------------------------------------------------------------
* show possible digits in column c
'--------------------------------------------------------------------*/
  Parse Arg c
  Call o poss_c(c)
  Return

poss_c: Procedure Expose g. poss.
/*---------------------------------------------------------------------
* compute possible digits in column c
'--------------------------------------------------------------------*/
  Parse Arg c
  ol='Col' c':'
  Do r=1 To 9
    prc=poss.r.c
    If prc='' Then prc='.'
    ol=ol left(prc,6)
    End
  Return ol

compress: Procedure
/*---------------------------------------------------------------------
* build a string containing the digits found in s
* Example: compress('11 9 33 55') -> '1359'
*--------------------------------------------------------------------*/
  Parse Arg s
  res=''
  Do d=1 To 9
    If pos(d,s)>0 Then
      res=res||d
    End
  Return left(res,9)

diff:
/*---------------------------------------------------------------------
* build the 'difference' of two strings  (same as squish in version 1)
* Return a string of digits contained in arg(1) not existant in arg(2)
* Example: diff('13895','35') -> '189'
*--------------------------------------------------------------------*/
  Return space(translate(arg(1),,word(arg(2) ',',1)),0)

check_all:
/*---------------------------------------------------------------------
* check the current status against the target (if this is known)
*--------------------------------------------------------------------*/
  error=0
  Do r=1 To 9
    Do c=1 To 9
      If aa.r.c=''|aa.r.c=s.r.c Then
        Nop
      Else Do
        Call o 'r='r 'c='c 'soll='s.r.c 'ist='aa.r.c
        error=1
        End
      End
    End
  Do r=1 To 9
    Do c=1 To 9
      Select
        When poss.r.c='' Then
          Nop
        When pos(s.r.c,poss.r.c)>0 Then
          Nop
        Otherwise Do
          Call o 'r='r 'c='c aa.r.c 'not in poss:'poss.r.c
          error=1
          End
        End
      End
    End
  If error Then
    Call exit 'an error in check_all'
  Return

o:
/*---------------------------------------------------------------------
* write to the debug file           (when g.0debug is true)
* and, if applicable, to the screen (when g.0say is true)
* and to the incomplete solution    (when g.0fail is true)
*--------------------------------------------------------------------*/
   If g.0say Then
     Say arg(1)
   If g.0fail Then
     Call lineout g.0inco,arg(1)
   If g.0debug Then
     Call lineout g.0dbg,arg(1)
   Return

set_aa: Procedure Expose g. aa. poss. box. boxr. boxc. boxlr. boxlc.,
                         s. sigl
/*---------------------------------------------------------------------
* put a digit into the cell r.c and show the text given
*--------------------------------------------------------------------*/
  Parse Arg r,c,d,text
  from=sigl
  If s.r.c<>'*' &,
     d<>s.r.c Then Do
    call o 'Trying t set aa.'r'.'c 'to' d 'but should be' s.r.c
    Call o 'from='from
    Exit
    End

  Call o 'setting aa.'r'.'c' to d='d '('text')'
  If g.0done.r.c=1 Then Do
    Call o 'cell' r'.'c'='aa.r.c '>' d '?????' 'called_from='sigl,
                                                      'in pass' g.0pass
    End
  aa.r.c=d                          /* put the digit into the cell   */
  poss.r.c=''                       /* remove cell's possible digits */
  g.0done.r.c=1                     /* note that cell was set        */
  Return

count: Procedure
/*---------------------------------------------------------------------
* Return the number of occurrences of d in s (all digits)
* Example: count(3,'123 567 399 13') -> 3
*--------------------------------------------------------------------*/
  Parse Arg d,s
  s=translate(s,'*',d)
  s=translate(s,'','123456789')
  s=space(s,0)
  Return length(s)

drc: Procedure
/*---------------------------------------------------------------------
* return coordinates and contents of a cell as r.c=string
*--------------------------------------------------------------------*/
  Parse Arg r,c,s
  Return 'drc' r'.'c'='s

set_geometry:
/*---------------------------------------------------------------------
* set miscellaneous relations and limits
*--------------------------------------------------------------------*/
  box.=''
  Do b=1 For 9                      /* build the box bounds.         */
    rr=(((b*3)%10)+1)*3-2           /* compute row lower bound.      */
    cc=(((b-1)//3)+1)*3-2           /* compute col lower bound.      */
    boxr.b=rr
    boxc.b=cc
    Do r=rr To rr+2                 /* build boxes with cell #s.     */
      Do c=cc To cc+2
        rc=r||c
        box.b=box.b rc
        box.r.c=b
        End
      End
    box.b=strip(box.b)
    End

  rowlb.=9                          /* row R,  low box number=b.     */
  collb.=9                          /* col R,  low box number=b.     */
  boxlr.=9                          /* box B,  low row number=r.     */
  boxlc.=9                          /* box B,  low col number=c.     */
  Do r=1 To 9
    Do c=1 To 9
      b=box.r.c                     /* what box is this R,C in ?     */
      rowlb.r=min(rowlb.r,b)        /* find min box # for row R.     */
      collb.c=min(collb.c,b)        /* find min box # for col C.     */
      boxlr.b=min(boxlr.b,r)        /* find min row # for box B.     */
      boxlc.b=min(boxlc.b,c)        /* find min col # for box B.     */
      End                           /* c                             */
    End                             /* r                             */
  Return

get_input: Procedure Expose g. aa. s.
/*---------------------------------------------------------------------
* get the given puzzle
* 9 rows with 9 columns each containing a digit or a place holder (.x0)
* set the miscellaneous file-ids
* and get the known solution (if available) for checking in get_sol
*--------------------------------------------------------------------*/
  Parse Arg g.0fid
  Parse Var g.0fid g.0fn '.'
  If g.0debug Then Do
    g.0dbg=g.0fn'.dbg'              /* file to contain debug output  */
    /*********************************
    Call lineout g.0dbg
    If lines(g.0dbg)>0 Then         /* if the file exists            */
      'erase' g.0dbg                /*   erase it                    */
    *********************************/
    End
  If pos('.',g.0fid)=0 Then
    g.0fid=g.0fid'.in'
  digits='123456789'
  g.0fidx=g.0fid
  Say 'process file' g.0fidx
  If lines(g.0fidx)=0 Then
    Call exit 'Input file does not exist'
  instr=''
  Do While lines(g.0fidx)>0
    instr=instr linein(g.0fidx)
    End
  Call lineout g.0fidx
  instr=translate(instr,digits'000',digits'.x0'||xrange('00'x,'ff'x))
  instr=space(instr,0)
  Select
    When length(instr)<81 Then Do
      Say 'instr='instr'<'
      Call exit 'Incorrect input - not enough data'
      End
    When length(instr)>81 Then Do
      Say 'instr='instr'<'
      Call exit 'Incorrect input - too much data'
      End
    Otherwise Do
      Call o '   instr='instr'<'
      instr=translate(instr,' ','0')
      End
    End
  Do r=1 To 9
    Do c=1 To 9
      Parse Var instr aa.r.c +1 instr
      End
    End
  g.0inco=g.0fn'f.in'               /* file to contain failed res    */
  if lines(g.0inco)>0 Then          /* if the file exists            */
    'erase' g.0inco                 /*   erase it                    */
  g.0summ='sudoku.summary'          /* file to get statistics        */
  g.0sol= 'sol\'g.0fn'.sol'         /* known solution for checking   */
  If lines(g.0sol)>0 Then           /* if that file is found         */
    Call get_sol                    /*   get its data                */
  Else Do                           /* otherwise                     */
    g.0sol=''                       /*   don't check                 */
    s.='*'
    End
  Say 'Input from         ' g.0fidx
  Say 'Debug output to    ' g.0dbg
  If lines(g.0sol)>0 Then           /* if that file is found         */
    Say 'Given solution from' g.0sol
  Say 'Statistics to      ' g.0summ
  Say 'Incomplete solution' g.0inco '(if applicable)'
  Say 'Hit enter to proceed'
  Return

get_sol: Procedure Expose g. s.
/*---------------------------------------------------------------------
* get the known solution
* (9 rows with 9 columns each containing a digit)
*--------------------------------------------------------------------*/
  solvstr=''
  If lines(g.0sol)>0 Then Do
    Do While lines(g.0sol)>0
      solvstr=solvstr linein(g.0sol)
      End
    Call lineout g.0sol
    solvstr=space(solvstr,0)
    Call o 'solution='solvstr
    Do r=1 To 9
      Do c=1 To 9
        Parse Var solvstr s.r.c +1 solvstr
        End
      End
    Do r=1 To 9
      ol=s.r.1
      Do c=2 To 9
        ol=ol s.r.c
        If c//3=0 Then ol=ol' '
        End
      Call o ol
      If r//3=0 Then
        Call o ' '
      End
    End
  Return

exit: Say 'EXIT' arg(1)
      Exit

write_summary: Procedure Expose g.
/*---------------------------------------------------------------------
* add a line to the statistics
* file       init walt sing excl mats line todo pass
* sdk002.in    56   56    0    0    0    0    0    1
* sdk007.in    61   16    0    0    1    5   39    1 <---
* sdk007.in    61   55    0    0    1    5    0    2 solved
* sdk088.in    50   14    2   34    0    0    0    1
* sdk093.in    55    2    2    1    0    0   50    2 <---
* sdk093.in    55    2    2    1    0    0   50    2 <---  no success
*--------------------------------------------------------------------*/
  If lines(g.0summ)=0 Then          /* write header line             */
    Call lineout g.0summ,,
                   'file       init walt sing excl mats line todo pass'
  If g.0todo>0 Then tag='<---'      /* mark a failure                */
               Else tag=''
                                 /* show # of hits for each strategy */
  summline=left(g.0fid,10) right(g.0todo_init,4),
                           right(g.0foundwalt,4),
                           right(g.0foundsing,4),
                           right(g.0foundexcl,4),
                           right(g.0foundline,4),
                           right(g.0foundmats,4),
                           right(g.0todo,4),
                           right(g.0pass,4) tag
  /*
  Say summline
  */
  Call lineout g.0summ,summline
  Call lineout g.0summ              /* close the file                */
  Return

novalue:
  Say 'Novalue raised in line' sigl
  Say sourceline(sigl)
  Say 'Variable' condition('D')
  Signal lookaround

syntax:
  Say 'Syntax raised in line' sigl
  Say sourceline(sigl)
  Say 'rc='rc '('errortext(rc)')'

halt:
lookaround:
  If fore() Then Do
    Say 'You can look around now.'
    Trace ?R
    Nop
    End
  Exit 12
```

{{out}}

```txt

process file sdk087.in
Input from          sdk087.in
Debug output to     0
Given solution from
Statistics to       sudoku.summary
Incomplete solution sdk087f.in (if applicable)
Hit enter to proceed
the puzzle
 . . .  . . .  3 . .
 . . .  . 7 1  5 . .
 . . 2  4 . 6  . 1 8

 . . .  . . 9  . 4 6
 . 9 .  6 1 8  . 3 .
 6 1 .  7 . .  . . 9

 4 3 .  8 . 7  6 . .
 . . 8  1 4 .  . . .
 . . 9  . . .  . . .

solved
 7 4 1  9 8 5  3 6 2
 3 8 6  2 7 1  5 9 4
 9 5 2  4 3 6  7 1 8

 8 2 7  3 5 9  1 4 6
 5 9 4  6 1 8  2 3 7
 6 1 3  7 2 4  8 5 9

 4 3 5  8 9 7  6 2 1
 2 6 8  1 4 3  9 7 5
 1 7 9  5 6 2  4 8 3

```

