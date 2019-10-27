+++
title = "Go Fish/Locomotive Basic"
description = ""
date = 2011-12-05T09:30:26Z
aliases = []
[extra]
id = 10998
[taxonomies]
categories = []
tags = []
+++


```locobasic
10 cls:randomize time:defint a-z
20 dim play(13),compu(13),deck(13),guess(13),poss(13),asked(13)
30 card$="A234567890JQK"
40 remca=4*13
50 for i=1 to 13:deck(i)=4:next
60 for i=1 to 9
70 gosub 1250
80 deck(k)=deck(k)-1
90 compu(k)=compu(k)+1
100 gosub 1250
110 deck(k)=deck(k)-1
120 play(k)=play(k)+1
130 next i
140 print "Go Fish"
150 print "
### =
"
160 print
170 input "What is your name";name$
180 ' PRINT_HAND
190 print "Score: "name$;" ";score(0);"  Computer ";score(1)
200 print ,remca"cards remaining"
210 print "Your hand: ";
220 for i=1 to 13
230 if play(i)=0 then 280
240 for j=1 to play(i)
250 print mid$(card$,i,1);" ";
260 next j
270 ' NEXT_CARD
280 next i
290 print
300 ' ASK_CARD
310 gosub 1100
320 s$=name$
330 input "Which card do you ask for";c$
340 if c$="" then 310
350 cn=instr(card$,upper$(c$))
360 if cn=0 then print "Sorry, that is not a valid choice.":goto 310
370 if play(cn)=0 then print "You do not have that card!":goto 310
380 guess(cn)=1
390 if compu(cn)=0 then print s$", go fish!":print s$" draws a ";:gosub 800:gosub 920:goto 470
400 v=compu(cn)
410 compu(cn)=0
420 play(cn)=play(cn)+v
430 print s$" gets"v"more cards."
440 gosub 920
450 goto 190
460 ' COMPUTER_TURN_1
470 s$="Computer"
480 for i=1 to 13
490 asked(i)=0
500 next
510 ' COMPUTER_TURN_2
520 gosub 1100
530 po=0
540 for i=1 to 13
550 if (compu(i)>0) and (guess(i)>0) then poss(i)=1:po=po+1
560 next
570 if po=0 then 650
580 ' DRAW_GUESS
590 k=rnd*12+1
600 if poss(k)=0 then 590
610 guess(k)=0
620 asked(k)=1
630 goto 680
640 ' DRAW_RAND
650 k=rnd*12+1
660 if compu(k)=0 or asked(k) then 650
670 ' MAKE_TURN
680 print s$" wants your "mid$(card$,k,1)"'s."
690 asked(k)=1
700 if play(k)=0 then print s$", go fish!":print s$" draws a ";:gosub 860:gosub 1010:goto 190
710 v=play(k)
720 play(k)=0
730 compu(k)=compu(k)+v
740 print s$" gets"v"more cards."
750 gosub 1010
760 goto 520
770 goto 190
780 end
790 ' DRAW_CARD_P
800 gosub 1250
810 print mid$(card$,k,1)"."
820 deck(k)=deck(k)-1
830 play(k)=play(k)+1
840 return
850 ' DRAW_CARD_C
860 gosub 1250
870 print "card."
880 deck(k)=deck(k)-1
890 compu(k)=compu(k)+1
900 return
910 ' CHECK_BOOK_P
920 for i=1 to 13
930 if play(i)<>4 then 980
940 print s$" completes book of "mid$(card$,i,1)"'s."
950 play(i)=0
960 score(0)=score(0)+1
970 ' NEXT_LOOP_P
980 next i
990 return
1000 ' CHECK_BOOK_C
1010 for i=1 to 13
1020 if compu(i)<>4 then 1070
1030 print s$" completes book of "mid$(card$,i,1)"'s."
1040 compu(i)=0
1050 score(1)=score(1)+1
1060 ' NEXT_LOOP_C
1070 next i
1080 return
1090 ' CHECK_END
1100 np=0:nc=0
1110 for i=1 to 13
1120 np=np+play(i)
1130 nc=nc+compu(i)
1140 next i
1150 if remca=0 or np=0 or nc=0 then 1180
1160 return
1170 ' END_GAME
1180 print
1190 print "*** Game over! ***"
1200 print
1210 if score(0)>score(1) then print name$" has won.":end
1220 if score(0)<score(1) then print "The computer has won.":end
1230 print "It's a tie!":end
1240 ' DEAL_CARD
1250 remca=remca-1
1260 sc=remca*rnd+1
1270 for k=1 to 13
1280 sc=sc-deck(k)
1290 if sc<=0 then return
1300 next
```


Note: 10's are called 0's here to get single-letter rank names.
