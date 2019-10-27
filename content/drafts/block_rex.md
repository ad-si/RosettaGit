+++
title = "$BLOCK.REX"
description = ""
date = 2017-09-06T23:56:12Z
aliases = []
[extra]
id = 17238
[taxonomies]
categories = []
tags = []
+++

==$BLOCK.REX==
This Classic REXX program is used by the   '''$T.REX'''   program to generate character strings used to display (big) blocked letters such as:

```txt

     $$       BBBBBBBBBBB   LLLL           OOOOOOOOOO    CCCCCCCCCC   KKKK     KKK                RRRRRRRRRRR   EEEEEEEEEEEE  XXXX    XXXX
 $$$$$$$$$$   BBBBBBBBBBBB  LLLL          OOOOOOOOOOOO  CCCCCCCCCCCC  KKKK     KK                 RRRRRRRRRRRR  EEEEEEEEEEEE  XXXX    XXXX
$$$$$$$$$$$$   BB       BB   LL           OOO      OOO  CCC       CC   KK     KK                   RR       RR  EE        EE   XX      XX
$$   $$        BB       BB   LL           OO        OO  CC        CC   KK    KK                    RR       RR  EE              XX    XX
$$   $$        BB       BB   LL           OO        OO  CC             KK   KK                     RR       RR  EE   EE          XX  XX
$$$$$$$$$$$    BBBBBBBBBBB   LL           OO        OO  CC             KKKKKK                      RRRRRRRRRRR  EEEEEEE           XXXX
 $$$$$$$$$$$   BBBBBBBBBB    LL           OO        OO  CC             KKKKKK                      RRRRRRRRRR   EEEEEEE           XXXX
     $$   $$   BB       BB   LL     LLLL  OO        OO  CC             KK   KK                     RR   RR      EE   EE          XX  XX
     $$   $$   BB       BB   LL     LLLL  OO        OO  CC        CC   KK    KK                    RR    RR     EE              XX    XX
$$$$$$$$$$$$   BB       BB   LL      LL   OOO      OOO  CCC       CC   KK     KK                   RR     RR    EE        EE   XX      XX
 $$$$$$$$$$   BBBBBBBBBBBB   LLLLLLLLLL   OOOOOOOOOOOO  CCCCCCCCCCCC  KKKK     KK        ..       RRRR     RR   EEEEEEEEEEEE  XXXX    XXXX
     $$       BBBBBBBBBBB    LLLLLLLLLL    OOOOOOOOOO    CCCCCCCCCC   KKKK     KKK       ..       RRRR      RR  EEEEEEEEEEEE  XXXX    XXXX

```


```rexx
/**/trace o;parse arg !;if !all(arg()) then exit;if !cms then address '';signal on halt;signal on novalue;signal on syntax
/*                      This REXX program was created by $BLOCK__ REX C:\$\ */
/*                      created: Tuesday 05/11/2010 22:04:45 */

_=space(!!)                      /*get 1st arg, preserve case.*/
                                 /*don't support ? for $BLOCK.*/
if _=='' then _=' '
if words(_)>1 then call er 59
c2bb='character-to-be-blocked'
if length(_)==2 then do
                     if \datatype(_,'X') then call er 40,_ c2bb
                     _=x2c(_)
                     end
if length(_)\==1 then call er 30,_ c2bb 1
?=0
_=c2x(_)

if _=='01' /* 01 */ then ?='7FEFFFC03D9BD9BC03C03CF3C63C03FFF7FE'
if _=='02' /* 02 */ then ?='7FEFFFFFFE67E67FFFFFFF0FF9FFFFFFF7FE'
if _=='03' /* 03 */ then ?='30C79EFFFFFFFFFFFF7FE7FE3FC1F80F006'
if _=='04' /* 04 */ then ?='0600F01F83FC7FEFFFFFF7FE3FC1F80F006'
if _=='05' /* 05 */ then ?='0F01F81F80F036C7FEFFFFFF7FE36C0600F'
if _=='06' /* 06 */ then ?='0600F01F83FC7FEFFFFFFFFF7FE36C0600F'
if _=='07' /* 07 */ then ?='0000000000F01F81F81F81F80F'
if _=='08' /* 08 */ then ?='FFFFFFFFFF0FE07E07E07E07F0FFFFFFFFFF'
if _=='09' /* 09 */ then ?='0003FC7FE70E60660660660670E7FE3FC'
if _=='0A' /* 0A */ then ?='FFFFFFE07CF3DFBDFBDFBDFBCF3E07FFFFFF'
if _=='0B' /* 0B */ then ?='03F01F00F01B1F93FC30C30C30C30C3FC1F8'
if _=='0C' /* 0C */ then ?='1F83FC30C30C30C3FC1F80600603FC3FC06'
if _=='0D' /* 0D */ then ?='7FF7FF6036037FF7FF600600600600E00C'
if _=='0E' /* 0E */ then ?='7FF7FF6036037FF7FF603603603607E06C'
if _=='0F' /* 0F */ then ?='8F1CF36F63FCFFFF9FF9FFFF3FC6F6CF38F1'
if _=='10' /* 10 */ then ?='C00F00FC0FF0FFCFFFFFFFFCFF0FC0F00C'
if _=='11' /* 11 */ then ?='00300F03F0FF3FFFFFFFF3FF0FF03F00F003'
if _=='12' /* 12 */ then ?='0600F01F83FC7FE0600607FE3FC1F80F006'
if _=='13' /* 13 */ then ?='30C79E79E79E79E79E30C30C00000030C30C'
if _=='14' /* 14 */ then ?='7FFFFFC33C33C33FF37F3033033033033033'
if _=='15' /* 15 */ then ?='0FC1FE1800F01F81981981F80F00187F83F'
if _=='16' /* 16 */ then ?=jr('1F81F81F81F81F81F8')
if _=='17' /* 17 */ then ?='0600F01F83FC7FE0607FE3FC1F80F0060FFF'
if _=='18' /* 18 */ then ?='0600F01F836C666C6306006006006006006'
if _=='19' /* 19 */ then ?='060060060060060060C6366636C1F80F006'
if _=='1A' /* 1A */ then ?='06003001800C006FFFFFF00600C01803006'
if _=='1B' /* 1B */ then ?='0600C0180300600FFFFFF6003001800C006'
if _=='1C' /* 1C */ then ?='0000000000001801801801F81F8'
if _=='1D' /* 1D */ then ?='00009019839C79EFFFFFF79E39C19809'
if _=='1E' /* 1E */ then ?='0600600F00F01F81F83FC3FC7FE7FEFFFFFF'
if _=='1F' /* 1F */ then ?='FFFFFF7FE7FE3FC3FC1F81F80F00F006006'
if _=='21' /*  ! */ then ?='0600F00F00F00F00F00F006006000006006'
if _=='22' /*  " */ then ?='198198'
if _=='23' /*  # */ then ?='198198198FFFFFF198198FFFFFF198198198'
if _=='24' /*  $ */ then ?='0607FEFFFC60C60FFE7FF063063FFF7FE06'
if _=='25' /*  % */ then ?='703F86D8CF987300600C018E31F61BC1F80E'
if _=='26' /*  & */ then ?='3F87FC60C6183301F03E0633C1BC0FFFE7FF'
if _=='27' /*  ' */ then ?='0600600C'
if _=='28' /*  ( */ then ?='0600C01803003006006003003001800C006'
if _=='29' /*  ) */ then ?='0C006003001801800C00C0180180300600C'
if _=='2A' /*  * */ then ?='C0360630C1980F0FFFFFF0F019830C606C03'
if _=='2B' /*  + */ then ?='0000000600600603FC3FC06006006'
if _=='2C' /*  , */ then ?=jr('600600C0')
if _=='2D' /*  - */ then ?='0000000000000003FC3FC'
if _=='2E' /*  . */ then ?=jr('60060')
if _=='2F' /*  / */ then ?='00300600C0180300600C0180300600C008'
if _=='30' /*  0 */ then ?='3FC7FE70E60660666666660660670E7FE3FC'
if _=='31' /*  1 */ then ?='0600E01E00600600600600600600601F81F8'
if _=='32' /*  2 */ then ?='3FC7FE6066060180300C03006006067FE7FE'
if _=='33' /*  3 */ then ?='3FC7FE60600600C03803800C0066067FE3FC'
if _=='34' /*  4 */ then ?='0380780D81983187FE7FE01801801803C03C'
if _=='35' /*  5 */ then ?='7FE7FE6006006007F07F800C0066067FE3FC'
if _=='36' /*  6 */ then ?='1FC3FE6066006006FC7FE7066066063FC1F8'
if _=='37' /*  7 */ then ?='7FE7FE60600600C01803006006006006006'
if _=='38' /*  8 */ then ?='1F83FC60660630C1F81F830C6066063FC1F8'
if _=='39' /*  9 */ then ?='1FC3FE60660660E3FE1F60060066063FC1F8'
if _=='3A' /*  : */ then ?=jr('60060000000000000060060000')
if _=='3B' /*  ; */ then ?=jr('600600000000000000600600C0')
if _=='3C' /*  < */ then ?='00300C0300C0300C00C003000C003000C003'
if _=='3D' /*  = */ then ?='0000000003FC3FC0000003FC3FC'
if _=='3E' /*  > */ then ?='C003000C003000C00300300C0300C0300C'
if _=='3F' /*  ? */ then ?='0F01F830C60600600C01803006006000006'
if _=='40' /*  @ */ then ?='7FEFFFC03CFBDFBD9BDFECFCC00C007FE3FE'
if _=='41' /*  A */ then ?='0600F019830C6066067FE7FE606606F0FF0F'
if _=='42' /*  B */ then ?='FFEFFF6036036037FF7FE603603603FFFFFE'
if _=='43' /*  C */ then ?='7FEFFFE03C03C00C00C00C00C03E03FFF7FE'
if _=='44' /*  D */ then ?='FFCFFE607603603603603603603607FFEFFC'
if _=='45' /*  E */ then ?='FFFFFFC03C00C60FE0FE0C60C00C03FFFFFF'
if _=='46' /*  F */ then ?='FFFFFFC03C00C60FE0FE0C60C00C00C00C'
if _=='47' /*  G */ then ?='7FEFFFE03C00C00C00C7FC7FC63E07FFF7FC'
if _=='48' /*  H */ then ?='F0FF0F6066066067FE7FE606606606F0FF0F'
if _=='49' /*  I */ then ?='1F81F80600600600600600600600601F81F8'
if _=='4A' /*  J */ then ?='0FF0FF018018018018C18C18C18E387F03E'
if _=='4B' /*  K */ then ?='F07F0660C6186307E07E063061860CF06F07'
if _=='4C' /*  L */ then ?='F00F0060060060060060060F60F6067FE7FE'
if _=='4D' /*  M */ then ?='E07E0770E79E7FE6F6666606606606F0FF0F'
if _=='4E' /*  N */ then ?='E0FE0F7067867C66E667663E61E60EF07F07'
if _=='4F' /*  O */ then ?='7FEFFFE07C03C03C03C03C03C03E07FFF7FE'
if _=='50' /*  P */ then ?='FFEFFF6036036037FF7FE600600600F00F'
if _=='51' /*  Q */ then ?='7FEFFFE07C03C03C03C33C33C33E1BFFF7FE'
if _=='52' /*  R */ then ?='FFEFFF6036036037FF7FE63061860CF06F03'
if _=='53' /*  S */ then ?='3FC7FE70660F6003F81FC00EF066067FE3FC'
if _=='54' /*  T */ then ?='FFFFFFC63C630600600600600600600F00F'
if _=='55' /*  U */ then ?='F0FF0F60660660660660660660670E7FE3FC'
if _=='56' /*  V */ then ?='F0FF0F60660660660630C30C1981980F006'
if _=='57' /*  W */ then ?='F0FF0F6066066066666F67FE79E70E606606'
if _=='58' /*  X */ then ?='F0FF0F60630C1980F00F019830C606F0FF0F'
if _=='59' /*  Y */ then ?='F0FF0F60630C1980F00600600600600F00F'
if _=='5A' /*  Z */ then ?='FFFFFFC0EC1C0380700E01C0383703FFFFFF'
if _=='5B' /*  [ */ then ?='0F00F00C00C00C00C00C00C00C00C00F00F'
if _=='5C' /*  \ */ then ?='C006003001800C006003001800C006003001'
if _=='5D' /*  ] */ then ?='1E01E00600600600600600600600601E01E'
if _=='5E' /*  ^ */ then ?='0600F019830C606C03801'
if _=='5F' /*  _ */ then ?=jr('7FE7FE')
if _=='60' /*  ` */ then ?='06006003'
if _=='61' /*  a */ then ?=jr('1FC1FE0063FE7FE6067FE3FF000')
if _=='62' /*  b */ then ?='6006006007FC7FE6066066066067FE7FC'
if _=='63' /*  c */ then ?=jr('3FC7FE6066006006067FE3FC000')
if _=='64' /*  d */ then ?='0060060063FE7FE6066066066067FE3FE'
if _=='65' /*  e */ then ?=jr('3FC7FE6067FE7FE6007FC3F8000')
if _=='66' /*  f */ then ?='0780FC0CC0C03F03F00C00C00C00C00C'
if _=='67' /*  g */ then ?=jr('3FE7FE6066067FE3FE0067FE3FC')
if _=='68' /*  h */ then ?='6006006006F87FC70E606606606606606'
if _=='69' /*  i */ then ?='0000600000F00600600600600600600F'
if _=='6A' /*  j */ then ?='00000C00000C00C00C00C00C30C30C1F80F'
if _=='6B' /*  k */ then ?='6006006186306606C07E073061860C606'
if _=='6C' /*  l */ then ?='0E00600600600600600600600600600F'
if _=='6D' /*  m */ then ?=jr('79C7FE666666666666666666000')
if _=='6E' /*  n */ then ?=jr('77C7FE70E606606606606606000')
if _=='6F' /*  o */ then ?=jr('3FC7FE6066066066067FE3FC000')
if _=='70' /*  p */ then ?=jr('7FC7FE6066067FE7FC600600600')
if _=='71' /*  q */ then ?=jr('3FE7FE6066067FE3FE006006006')
if _=='72' /*  r */ then ?=jr('6FC7FE706600600600600600000')
if _=='73' /*  s */ then ?=jr('3FC7FE6007FC3FE0067FE3FC000')
if _=='74' /*  t */ then ?='0C00C07F87F80C00C00C00C00CC0FC078'
if _=='75' /*  u */ then ?=jr('6066066066066066067FE3FB000')
if _=='76' /*  v */ then ?=jr('60660660660630C1980F0060000')
if _=='77' /*  w */ then ?=jr('C03C03C63CF3D9BF0FE07C03000')
if _=='78' /*  x */ then ?=jr('70E30C1980F00F019830C70E000')
if _=='79' /*  y */ then ?=jr('6066066066063FE1FE0067FE3FC')
if _=='7A' /*  z */ then ?=jr('7FE7FE00C0700E03007FE7FE000')
if _=='7B' /*  { */ then ?='0700F00C00C01803003001800C00C00F007'
if _=='7C' /*  | */ then ?='06006006006006000000006006006006006'
if _=='7D' /*  } */ then ?='1C01E00600600300180180300600601E01C'
if _=='7E' /*  ~ */ then ?='0000E03F7F3EE1C'
if _=='7F' /*   */ then ?='0600F019830C606C03C03C03C03C03FFFFFF'
if _=='9B' /*  ¢ */ then ?='0600603FC7FE6666606606667FE3FC06006'
if _=='9C' /*  £ */ then ?='0F81FC38E306300FC0FC0300300703DFF7FE'
if _=='9D' /*  ¥ */ then ?='F0FF0F60630C1980F07FE7FE0607FE7FE06'
if _=='9E' /*  ₧ */ then ?='FF0FF8C18C18FF8FF3CC6DE6CC3CC3CC3CC6'
if _=='9F' /*  ƒ */ then ?='03C07E0660601F81F80600600606607E03C'
if _=='A8' /*  ¿ */ then ?='0600000600600C018030060060630C1F80F'
if _=='A9' /*  ⌐ */ then ?=jr('3FC3FC300300300000000')
if _=='AA' /*  ¬ */ then ?=jr('3FC3FC00C00C00C000000')
if _=='AB' /*  ½ */ then ?='403C0640C41843046EED1182304608C1081F'
if _=='AC' /*  ¼ */ then ?='403C0640C418430462EC618A31263FC02802'
if _=='AD' /*  ¡ */ then ?='0600600000600600F00F00F00F00F00F006'
if _=='AE' /*  « */ then ?='0630C618C318630C60C6063031818C0C6063'
if _=='AF' /*  » */ then ?='C6063031818C0C60630630C618C318630C6'
if _=='B0' /*  ░ */ then ?='6DB9246DB9246DB9246DB9246DB9246DB924'
if _=='B1' /*  ▒ */ then ?='6DBDB66DBDB66DBDB66DBDB66DBDB66DBDB6'
if _=='B2' /*  ▓ */ then ?='EEE777EEE777EEE777EEE777EEE777EEE777'
if _=='B3' /*  │ */ then ?='06006006006006006006006006006006006'
if _=='C4' /*  ─ */ then ?='000000000000000FFFFFF'
if _=='DB' /*  █ */ then ?='FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
if _=='DC' /*  ▄ */ then ?=jr('FFFFFFFFFFFFFFFFFF')
if _=='DD' /*  ▌ */ then ?='FC0FC0FC0FC0FC0FC0FC0FC0FC0FC0FC0FC'
if _=='DE' /*  ▐ */ then ?='03F03F03F03F03F03F03F03F03F03F03F03F'
if _=='DF' /*  ▀ */ then ?='FFFFFFFFFFFFFFFFFF'
if _=='E0' /*  α */ then ?=jr('3F37FFC1EC0CC0CC1C7FE3F3000')
if _=='E1' /*  ß */ then ?='3FC7FE6066067FE7FC6066067FE7FC6006'
if _=='E2' /*  Γ */ then ?='FFFFFFC03C03C00C00C00C00C00C00C00C'
if _=='E3' /*  π */ then ?='FFFFFF30C30C30C30C30C30C30C30C30E30E'
if _=='E4' /*  Σ */ then ?='FFF7FF3031800C00600600C01803037FFFFF'
if _=='E5' /*  σ */ then ?=jr('3FF7FF6066066067FE3FC000')
if _=='E6' /*  µ */ then ?=jr('60660660660660660670E7FC600C00')
if _=='E7' /*  τ */ then ?='773FFFC6E06006006006006006006006006'
if _=='E8' /*  Φ */ then ?='0607FEFFFC63C63C63C63C63C63FFF7FE06'
if _=='E9' /*  Θ */ then ?='0001F87FEC03C03DFBDFBC03C037FE1F8'
if _=='EA' /*  Ω */ then ?='1F87FEC03C03C03C03C0360630C198F9FF9F'
if _=='EA' /*  Ω */ then ?='1F87FEC03C03C03C03C0360630C198F9FF9F'
if _=='EB' /*  δ */ then ?='3FC7003800E00387FEFFFC03C03C03FFF7FE'
if _=='EC' /*  ∞ */ then ?='00000000039C6F6C63C636F639C'
if _=='ED' /*  φ */ then ?='0000000603FC7FE6666667FE3FC06'
if _=='EE' /*  ε */ then ?='0000000F03FC6007E07E06003FC1F8'
if _=='EF' /*  ∩ */ then ?='0000F01F830C606606606606606606'
if _=='F0' /*  ≡ */ then ?='0000003FC3FC0003FC3FC0003FC3FC'
if _=='F1' /*  ± */ then ?='0600600603FC3FC0600600600003FC3FC'
if _=='F2' /*  ≥ */ then ?='7001C007001C00701C0701C07000007FF7FF'
if _=='F3' /*  ≤ */ then ?='00E0380E0380E003800E003800E000FFEFFE'
if _=='F6' /*  ÷ */ then ?='000000060060000FFFFFF00006006'
if _=='F7' /*  ≈ */ then ?='0000E03F7F3EE1C0000E03F7F3EE1C'
if _=='F8' /*  ° */ then ?='0F01F81981981F80F'
if _=='F9' /*  ∙ */ then ?='0000000000F01F81F81F81F80F'
if _=='FB' /*  √ */ then ?='03F03F030030030030E30F301B00F007003'
if _=='FC' /*  ⁿ */ then ?='77C7FE70E606606606606'
if _=='FD' /*  ² */ then ?='3FC7FE70E0380C03007FE7FE'
if _=='FE' /*  ■ */ then ?='0000000001F81F81F81F81F81F8'
return left(?,36,0)


/*═════════════════════════════general 1-line subs══════════════════════*/
!all:!!=!;!=space(!);upper !;call !fid;!nt=right(!var('OS'),2)=='NT';!cls=word('CLS VMFCLEAR CLRSCREEN',1+!cms+!tso*2);if arg(1)\==1 then return 0;if wordpos(!,'? ?SAMPLES ?AUTHOR ?FLOW')==0 then return 0;!call=']$H';call '$H' !fn !;!call=;return 1
!cal:if symbol('!CALL')\=="VAR" then !call=;return !call
!env:!env='ENVIRONMENT';if !sys=='MSDOS'|!brexx|!r4|!roo then !env='SYSTEM';if !os2 then !env='OS2'!env;!ebcdic=1=='f0'x;if !crx then !env='DOS';return
!fid:parse upper source !sys !fun !fid . 1 . . !fn !ft !fm .;call !sys;if !dos then do;_=lastpos('\',!fn);!fm=left(!fn,_);!fn=substr(!fn,_+1);parse var !fn !fn '.' !ft;end;return word(0 !fn !ft !fm,1+('0'arg(1)))
!rex:parse upper version !ver !vernum !verdate .;!brexx='BY'==!vernum;!kexx='KEXX'==!ver;!pcrexx='REXX/PERSONAL'==!ver|'REXX/PC'==!ver;!r4='REXX-R4'==!ver;!regina='REXX-REGINA'==left(!ver,11);!roo='REXX-ROO'==!ver;call !env;return
!sys:!cms=!sys=='CMS';!os2=!sys=='OS2';!tso=!sys=='TSO'|!sys=='MVS';!vse=!sys=='VSE';!dos=pos('DOS',!sys)\==0|pos('WIN',!sys)\==0|!sys=='CMD';!crx=left(!sys,6)=='DOSCRX';call !rex;return
!var:call !fid;if !kexx then return space(dosenv(arg(1)));return space(value(arg(1),,!env))
er:parse arg _1,_2;call '$ERR' "14"p(_1) p(word(_1,2) !fid(1)) _2;if _1<0 then return _1;exit result
p:return word(arg(1),1)
halt:call er .1
jr:return right(arg(1),36,0)
novalue:!sigl=sigl;call er 17,!fid(2) !fid(3) !sigl condition('D') sourceline(!sigl)
syntax:!sigl=sigl;call er 13,!fid(2) !fid(3) !sigl !cal() condition('D') sourceline(!sigl)
/* ♦♦♦ end-of-program. ♦♦♦ */
```

The   '''$BLOCK__.REX'''   Classic REXX program is included here ──► [["$BLOCK--.REX"]].


The Classic REXX program   '''$BLOCK__.REX'''   is used to generate the   ''' $BLOCK.REX'''   program.



[[Category:REXX library routines]]
