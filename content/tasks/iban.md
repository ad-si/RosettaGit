+++
title = "IBAN"
description = ""
date = 2019-10-15T10:42:22Z
aliases = []
[extra]
id = 13227
[taxonomies]
categories = ["task", "Checksums"]
tags = []
languages = [
  "ada",
  "arturo",
  "autohotkey",
  "awk",
  "bbc_basic",
  "befunge",
  "bracmat",
  "c",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "factor",
  "forth",
  "fortran",
  "freebasic",
  "go",
  "groovy",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "jsish",
  "julia",
  "kotlin",
  "logtalk",
  "lua",
  "m2000_interpreter",
  "matlab",
  "newlisp",
  "nim",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "snobol4",
  "standard_ml",
  "tcl",
  "unix_shell",
  "vba",
  "vbscript",
  "yabasic",
  "zkl",
  "zx_spectrum_basic",
]
+++

The   [[wp:International_Bank_Account_Number|International Bank Account Number (IBAN)]]   is an internationally agreed means of identifying bank accounts across national borders with a reduced risk of propagating [[wp:Transcription_error|transcription errors]].

The IBAN consists of up to '''34''' alphanumeric characters:
::*   first the two-letter ISO 3166-1 alpha-2 country code,
::*   then two check digits, and
::*   finally a country-specific Basic Bank Account Number (BBAN).


The check digits enable a sanity check of the bank account number to confirm its integrity even before submitting a transaction.


## Task

Validate the following fictitious IBAN:   <tt> GB82 WEST 1234 5698 7654 32 </tt>


Details of the algorithm can be found on the Wikipedia page.





## Ada


```Ada
package Iban_Code is
   function Is_Legal(Iban : String) return Boolean;
end Iban_Code;
```

```Ada
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;

package body Iban_Code is

   subtype Nation is String (1..2);

   package String_Integer is new Ada.Containers.Hashed_Maps
     (Nation, Integer, Ada.Strings.Hash, Equivalent_Keys => "=");

   Nations : String_Integer.Map;

   function Is_Legal(Iban : String) return Boolean
   is
      Temp  : String(Iban'Range) := (others => ' ');
      Count : Integer;
      Ch    : Character;
      Num   : Integer := 0;
   begin
      -- remove blank spaces and check characters
      Count := Temp'First;
      for I in Iban'Range loop
	 case Iban(I) is
	    when ' ' => null;
	    when 'a'..'z' =>
	       Temp(Count) := To_Upper(Iban(I));
	       Count := Count + 1;
	    when 'A'..'Z'|'0'..'9' =>
	       Temp(Count) := Iban(I);
	       Count := Count + 1;
	    when others => return False;
	 end case;
      end loop;
      -- check nation code and length
      if not Nations.Contains (Temp(1..2)) or else
	Nations.Element (Temp(1..2))/= Count - 1 then
	 return False;
      end if;
      -- move the 4 initial characters to the end
      Temp(Temp'First..Count-1) := Temp(5..Count-1) & Temp(Temp'First..4);
      -- compute remainder modulo 97
      for I in Temp'First..Count-1 loop
	 Ch := Temp(I);
	 if Ch in '0'..'9' then
	    Num := Integer'Value(Integer'Image(Num) & Ch) mod 97;
	 else
	    Num := (Num * 100 +
		      (Character'Pos(Ch) - Character'Pos('A') + 10)) mod 97;
	 end if;
      end loop;
      return Num = 1;
   end Is_Legal;

begin
   Nations.insert("AL", 28);     Nations.insert("AD", 24);
   Nations.insert("AT", 20);     Nations.insert("AZ", 28);
   Nations.insert("BE", 16);     Nations.insert("BH", 22);
   Nations.insert("BA", 20);     Nations.insert("BR", 29);
   Nations.insert("BG", 22);     Nations.insert("CR", 21);
   Nations.insert("HR", 21);     Nations.insert("CY", 28);
   Nations.insert("CZ", 24);     Nations.insert("DK", 18);
   Nations.insert("DO", 28);     Nations.insert("EE", 20);
   Nations.insert("FO", 18);     Nations.insert("FI", 18);
   Nations.insert("FR", 27);     Nations.insert("GE", 22);
   Nations.insert("DE", 22);     Nations.insert("GI", 23);
   Nations.insert("GR", 27);     Nations.insert("GL", 18);
   Nations.insert("GT", 28);     Nations.insert("HU", 28);
   Nations.insert("IS", 26);     Nations.insert("IE", 22);
   Nations.insert("IL", 23);     Nations.insert("IT", 27);
   Nations.insert("KZ", 20);     Nations.insert("KW", 30);
   Nations.insert("LV", 21);     Nations.insert("LB", 28);
   Nations.insert("LI", 21);     Nations.insert("LT", 20);
   Nations.insert("LU", 20);     Nations.insert("MK", 19);
   Nations.insert("MT", 31);     Nations.insert("MR", 27);
   Nations.insert("MU", 30);     Nations.insert("MC", 27);
   Nations.insert("MD", 24);     Nations.insert("ME", 22);
   Nations.insert("NL", 18);     Nations.insert("NO", 15);
   Nations.insert("PK", 24);     Nations.insert("PS", 29);
   Nations.insert("PL", 28);     Nations.insert("PT", 25);
   Nations.insert("RO", 24);     Nations.insert("SM", 27);
   Nations.insert("SA", 24);     Nations.insert("RS", 22);
   Nations.insert("SK", 24);     Nations.insert("SI", 19);
   Nations.insert("ES", 24);     Nations.insert("SE", 24);
   Nations.insert("CH", 21);     Nations.insert("TN", 24);
   Nations.insert("TR", 26);     Nations.insert("AE", 23);
   Nations.insert("GB", 22);     Nations.insert("VG", 24);
end Iban_Code;
```
Testing:

```Ada
with Ada.Text_Io;             use Ada.Text_Io;
with Iban_Code;

procedure Check_Iban is

   procedure Check(Iban : String) is
   begin
      if Iban_Code.Is_Legal(Iban) then
	 Put_Line(Iban & " is valid.");
      else
	 Put_Line(Iban & " is not valid.");
      end if;
   end Check;

begin
   Check("GB82 WEST 1234 5698 7654 32");
   Check("GB82WEST12345698765432");
   Check("gb82 west 1234 5698 7654 32");
   Check("GB82 TEST 1234 5698 7654 32");
   Check("GB82 WEST 1243 5698 7654 32");
end Check_Iban;
```
 GB82 WEST 1234 5698 7654 32 is valid.
 GB82WEST12345698765432 is valid.
 gb82 west 1234 5698 7654 32 is valid.
 GB82 TEST 1234 5698 7654 32 is not valid.
 GB82 WEST 1243 5698 7654 32 is not valid.


## Arturo



```arturo
CountryIbanSizes #{
    AL 28, AD 24, AT 20, AZ 28, BE 16
    BH 22, BA 20, BR 29, BG 22, CR 21, HR 21, CY 28
    CZ 24, DK 18, DO 28, EE 20, FO 18, FI 18, FR 27
    GE 22, DE 22, GI 23, GR 27, GL 18, GT 28, HU 28
    IS 26, IE 22, IL 23, IT 27, KZ 20, KW 30, LV 21
    LB 28, LI 21, LT 20, LU 20, MK 19, MT 31, MR 27
    MU 30, MC 27, MD 24, ME 22, NL 18, NO 15, PK 24
    PS 29, PL 28, PT 25, RO 24, SM 27, SA 24, RS 22
    SK 24, SI 19, ES 24, SE 24, CH 21, TN 24, TR 26
    AE 23, GB 22, VG 24
}

Base36 $(map $(range 0 9) { toString & }) + $(map $(range 97 122) { uppercase|char & })

validIban [iban]{
    iban $(replace iban " " "")

    if $(not|isMatch iban "/[\dA-Z]+$/") { return false }

    if $(size iban)!=CountryIbanSizes.[$(slice iban 0 2)] { return false }

    iban $(slice iban 4 $(size iban)) + $(slice iban 0 4)
    iban $(join $(map $(characters iban) { toString $(find Base36 &)}) "")
    iban $(toNumber iban)

    return iban%97=1
}

loop #("GB82 WEST 1234 5698 7654 32" "GB82 TEST 1234 5698 7654 32") {
    print & + " => validation: " + $(validIban &)
}

```


```txt
GB82 WEST 1234 5698 7654 32 => validation: true
GB82 TEST 1234 5698 7654 32 => validation: false
```



## AutoHotkey

```AutoHotkey
IBANs := ["GB82 WEST 1234 5698 7654 32"
	, "gb82 WEST 1234 5698 7654 32"
	, "GB82WEST12345698765432"
	, "GB82 WEST 234 5698 7654 32"
	, "GB82 WEST 1234 5698 7654 33"
	, "AE82 WEST 1234 5698 7654 32"]
for k, v in IBANs
	Output .= v " is" (ValidIBAN(v) ? "" : " not") " valid.`n"
MsgBox, % Output

ValidIBAN(n) {
	static CC := {AL:28, AD:24, AT:20, AZ:28, BH:22, BE:16, BA:20, BR:29, BG:22, CR:21
		    , HR:21, CY:28, CZ:24, DK:18, DO:28, EE:20, FO:18, FI:18, FR:27, GE:22
		    , DE:22, GI:23, GR:27, GL:18, GT:28, HU:28, IS:26, IE:22, IL:23, IT:27
		    , JO:30, KZ:20, KW:30, LV:21, LB:28, LI:21, LT:20, LU:20, MK:19, MT:31
		    , MR:27, MU:30, MC:27, MD:24, ME:22, NL:18, NO:15, PK:24, PS:29, PL:28
		    , PT:25, QA:29, RO:24, SM:27, SA:24, RS:22, SK:24, SI:19, ES:24, SE:24
		    , CH:21, TN:24, TR:26, AE:23, GB:22, VG:24}
	StringReplace, n, n, % A_Space,, A
	;Check that the total IBAN length is correct as per the country
	if (StrLen(n) != CC[SubStr(n, 1, 2)])
		return false
	StringUpper, n, n
	;Move the four initial characters to the end of the string
	n := SubStr(n, 5) SubStr(n, 1, 4)
	;Replace each letter in the string with two digits
	Loop, Parse, n
	{
		if A_LoopField is alpha
			nn .= Asc(A_LoopField) - 55
		else
			nn .= A_LoopField
	}
	return Mod97(nn) = 1
}

Mod97(a) {
	while a {
		rem := Mod(rem SubStr(a, 1, 15), 97)
		a := SubStr(a, 16)
	}
	return rem
}
```
 GB82 WEST 1234 5698 7654 32 is valid.
 gb82 WEST 1234 5698 7654 32 is valid.
 GB82WEST12345698765432 is valid.
 GB82 WEST 234 5698 7654 32 is not valid.
 GB82 WEST 1234 5698 7654 33 is not valid.
 AE82 WEST 1234 5698 7654 32 is not valid.


## AWK

This requires a gawk with extensions and GNU MP+MPFR support - it's usually the case. Some country codes are missing, the output is itself parsable.


```awk

@load "ordchr"

function invalid()  { print("INVALID " $0); next }
function valid()    { print("VALID__ " $0) }

BEGIN {
    ccibanlen["AL"] = 28; ccibanlen["AD"] = 24; ccibanlen["AT"] = 20;
    ccibanlen["AZ"] = 28; ccibanlen["BH"] = 22; ccibanlen["BA"] = 20;
    ccibanlen["BR"] = 29; ccibanlen["BG"] = 22; ccibanlen["CR"] = 21;
    ccibanlen["HR"] = 21; ccibanlen["CY"] = 28; ccibanlen["CZ"] = 24;
    ccibanlen["DK"] = 18; ccibanlen["DO"] = 28; ccibanlen["EE"] = 20;
    ccibanlen["FO"] = 18; ccibanlen["FI"] = 18; ccibanlen["FR"] = 27;
    ccibanlen["GE"] = 22; ccibanlen["DE"] = 22; ccibanlen["GI"] = 23;
    ccibanlen["GR"] = 27; ccibanlen["GL"] = 18; ccibanlen["GT"] = 28;
    ccibanlen["HU"] = 28; ccibanlen["IS"] = 26; ccibanlen["IE"] = 22;
    ccibanlen["IT"] = 27; ccibanlen["KZ"] = 20; ccibanlen["KW"] = 30;
    ccibanlen["LV"] = 21; ccibanlen["LB"] = 28; ccibanlen["LI"] = 21;
    ccibanlen["LT"] = 20; ccibanlen["LU"] = 20; ccibanlen["MK"] = 19;
    ccibanlen["MT"] = 31; ccibanlen["MR"] = 27; ccibanlen["MU"] = 30;
    ccibanlen["MC"] = 27; ccibanlen["MD"] = 24; ccibanlen["ME"] = 22;
    ccibanlen["NL"] = 18; ccibanlen["NO"] = 15; ccibanlen["PK"] = 24;
    ccibanlen["PS"] = 29; ccibanlen["PL"] = 28; ccibanlen["PT"] = 25;
    ccibanlen["RO"] = 24; ccibanlen["SM"] = 27; ccibanlen["SA"] = 24;
    ccibanlen["RS"] = 22; ccibanlen["SK"] = 24; ccibanlen["SI"] = 19;
    ccibanlen["ES"] = 24; ccibanlen["SE"] = 24; ccibanlen["CH"] = 21;
    ccibanlen["TN"] = 24; ccibanlen["TR"] = 26; ccibanlen["AE"] = 23;
    ccibanlen["GB"] = 22; ccibanlen["VG"] = 24; ccibanlen["BE"] = 16;
}

{
    iban = toupper($0)
    gsub(/\s+/, "", iban)
    ccode = substr(iban, 1, 2)

    if (    ! match(iban, /^[A-Z0-9]+$/) ||
            ! (ccode in ccibanlen) ||
            length(iban) != ccibanlen[ccode])
        invalid()

    ibanrev = gensub(/^(.{4})(.+)/, "\\2\\1", 1, iban)
    ibancsum = ""
    for (i = 1; i <= length(ibanrev); i++) {
        currchar = substr(ibanrev, i, 1)
        if (match(currchar, /[A-Z]/))
            currchar = ord(currchar) - 55
        ibancsum = ibancsum currchar
    }

    ibancsum % 97 == 1 ? valid() : invalid()
}
```

Creating a test file and launching the script:
<lang>
cat > test.iban
FR33 ^__^ 0BAD
AA11 1234 6543 1212
FR33 1234 5432
CH93 0076 2011      6238 5295 7
GB82 WEST 1234 5698 7654 32
GB82 TEST 1234 5698 7654 32
^D
gawk -Mf iban.gawk test.iban

```


Output:

<lang>
INVALID FR33 ^__^ 0BAD
INVALID AA11 1234 6543 1212
INVALID FR33 1234 5432
VALID__ CH93 0076 2011      6238 5295 7
VALID__ GB82 WEST 1234 5698 7654 32
INVALID GB82 TEST 1234 5698 7654 32

```



## BBC BASIC

```bbcbasic
      REM Used the following as official standard:
      REM  http://www.cnb.cz/cs/platebni_styk/iban/download/EBS204.pdf

      REM Pairs of ISO 3166 country code & expected IBAN length for this country
      COULEN$="AL28 AD24 AT20 AZ28 BE16 BH22 BA20 BR29 BG22 CR21 HR21 CY28 CZ24 DK18 DO28 EE20 "+\
      \       "FO18 FI18 FR27 GE22 DE22 GI23 GR27 GL18 GT28 HU28 IS26 IE22 IL23 IT27 KZ20 KW30 "+\
      \       "LV21 LB28 LI21 LT20 LU20 MK19 MT31 MR27 MU30 MC27 MD24 ME22 NL18 NO15 PK24 PS29 "+\
      \       "PL28 PT25 RO24 SM27 SA24 RS22 SK24 SI19 ES24 SE24 CH21 TN24 TR26 AE23 GB22 VG24"

      PROCIBANcheck("GB82 WEST 1234 5698 7654 32"):REM Paper IBAN notation (with the spaces)
      PROCIBANcheck("GB82WEST12345698765432")     :REM Digital IBAN notation (without the spaces)
      PROCIBANcheck("gb82 west 1234 5698 7654 32")
      PROCIBANcheck("GB82 TEST 1234 5698 7654 32")
      PROCIBANcheck("GR16 0110 1250 0000 0001 2300 695")
      PROCIBANcheck("GB29 NWBK 6016 1331 9268 19")
      PROCIBANcheck("SA03 8000 0000 6080 1016 7519")
      PROCIBANcheck("CH93 0076 2011 6238 5295 7")
      PROCIBANcheck("IL62 0108 0000 0009 9999 999")
      PROCIBANcheck("IL62-0108-0000-0009-9999-999")
      PROCIBANcheck("US12 3456 7890 0987 6543 210")
      PROCIBANcheck("GR16 0110 1250 0000 0001 2300 695X")
      END

      DEF PROCIBANcheck(iban$)
      LOCAL err$,i%,match%,explen%,digiban$,tmpiban$,bignum$,c%,kk%

      REM Search for country code and fetch expected length
      i%=1:explen%=0
      WHILE explen%=0 AND i%<LENCOULEN$
        IF LEFT$(iban$,2)=MID$(COULEN$,i%,2) explen%=VALMID$(COULEN$,i%+2,2)
        i%+=5
      ENDWHILE
      match%=explen%>0

      REM Continue if country code found
      IF match% THEN
        REM Remove space = convert to digital IBAN
        digiban$=""
        FOR i%=1TOLENiban$
          IF MID$(iban$,i%,1)>" " digiban$+=MID$(iban$,i%,1)
        NEXT
        REM Compare length with expected length
        match%=explen%=LENdigiban$

        REM Continue if length is correct
        IF match% THEN
          REM Create temporary string with country code appended
          tmpiban$=MID$(digiban$,5)+MID$(digiban$,1,2)
          REM Make big number, replacing letters by numbers using next conversion table: A=10 ... Z=35
          bignum$=""
          FOR i%=1TOLENtmpiban$
            c%=ASCMID$(tmpiban$,i%,1)
            IF c%>57 bignum$+=STR$(c%-55) ELSE bignum$+=STR$(c%-48)
          NEXT
          REM MOD 97 on bignum$+"00" and subtract result from 98 to obtain control number
          kk%=98-FNmod97(bignum$+"00")
          REM Compare with control number in IBAN
          match%=VALMID$(iban$,3,2)=kk%

          REM Continue if control number matches
          IF match% THEN
            REM Append kk% to bignum$ and determine if MOD 97 results in 1
            match%=FNmod97(bignum$+RIGHT$("0"+STR$kk%,2))=1

            REM Continue if MOD 97
            IF match% THEN
              REM Was last test
            ELSE
              err$="result from modulo 97"
            ENDIF
          ELSE
            err$="check digits, should be: "+STR$kk%
          ENDIF
        ELSE
          err$="code length, expected length: "+STR$explen%
        ENDIF
      ELSE
        err$="country code: "+LEFT$(iban$,2)
      ENDIF

      IF match% PRINT "  "; ELSE PRINT "in";:err$="***error!*** invalid "+err$
      PRINT "valid IBAN: ";iban$TAB(50)err$
      ENDPROC

      DEF FNmod97(num$)
      LOCAL mod$
      mod$=LEFT$(num$,2)
      num$=MID$(num$,3)
      WHILE num$>""
        mod$=RIGHT$("0"+STR$(VAL(mod$+LEFT$(num$,7))MOD97),2)
        num$=MID$(num$,8)
      ENDWHILE
      =VALmod$
```
   valid IBAN: GB82 WEST 1234 5698 7654 32
   valid IBAN: GB82WEST12345698765432
 invalid IBAN: gb82 west 1234 5698 7654 32         ***error!*** invalid country code: gb
 invalid IBAN: GB82 TEST 1234 5698 7654 32         ***error!*** invalid check digits, should be: 78
   valid IBAN: GR16 0110 1250 0000 0001 2300 695
   valid IBAN: GB29 NWBK 6016 1331 9268 19
   valid IBAN: SA03 8000 0000 6080 1016 7519
   valid IBAN: CH93 0076 2011 6238 5295 7
   valid IBAN: IL62 0108 0000 0009 9999 999
 invalid IBAN: IL62-0108-0000-0009-9999-999        ***error!*** invalid code length, expected length: 23
 invalid IBAN: US12 3456 7890 0987 6543 210        ***error!*** invalid country code: US
 invalid IBAN: GR16 0110 1250 0000 0001 2300 695X  ***error!*** invalid code length, expected length: 27


## Befunge



```befunge>>
" :NABI">:#,_>:~:"`"`48**-:55+-#v_$0::6g0>>8g-!\7g18g-!*!v>v>>
<<_#v:#78#`8#+<^+!!*-*84\-9:g8:p8\<oo>1#$-#<0$>>>#v_"dilav" ^#<<
>>^ >*-:20p9`9*1+55+**20g+"a"%10g1+00g%:4-!|^g6::_v#-*88:+1_vv>>
<<^+`"Z"\+`\"0"\*`\"A"\`"9":::\-*86:g8p01:<<40p00_v#!--+99g5<v<<
>>" si rebmun tahT">:#,_                55+".",,@ >0"dilavni">>>
"-(&(/$$*$(*.-*.('$)*%0-&**.$'(.'/.+,''&&*.)**&,-.&.(*(*-!(%01-)
BFBBBBFFTNRRGGCCGCGGSSKSKSSDDHDHCPLPTLLLLPPAAVEIIAAAIEMMMNGMMMMI
ERGAHRIONLSOTRZYBRLIKIWMZAEOKREUHSBTRIVTUKLEDGESTLTZESEDCOEKUTRL
```


```txt
IBAN: GB82 WEST 1234 5698 7654 32
That number is valid.
```



```txt
IBAN: GB82 EAST 1234 5698 7654 32
That number is invalid.
```



## Bracmat


```bracmat
( ( IBAN-check
  =   table country cd len N c
    .       (AL.28) (AD.24) (AT.20) (AZ.28) (BE.16) (BH.22) (BA.20) (BR.29)
            (BG.22) (CR.21) (HR.21) (CY.28) (CZ.24) (DK.18) (DO.28) (EE.20)
            (FO.18) (FI.18) (FR.27) (GE.22) (DE.22) (GI.23) (GR.27) (GL.18)
            (GT.28) (HU.28) (IS.26) (IE.22) (IL.23) (IT.27) (KZ.20) (KW.30)
            (LV.21) (LB.28) (LI.21) (LT.20) (LU.20) (MK.19) (MT.31) (MR.27)
            (MU.30) (MC.27) (MD.24) (ME.22) (NL.18) (NO.15) (PK.24) (PS.29)
            (PL.28) (PT.25) (RO.24) (SM.27) (SA.24) (RS.22) (SK.24) (SI.19)
            (ES.24) (SE.24) (CH.21) (TN.24) (TR.26) (AE.23) (GB.22) (VG.24)
        : ?table
      & @(!arg:?country [2 ?cd [4 ?arg)
      & str$(!arg !country !cd):?arg
      & (   !table:? (!country.?len) ?
          & :?N
          & ( @( !arg
               :   ?
                   ( %@?c ?
                   & ( !c:#
                     |   !c:~<A:~>Z
                       & asc$!c+-1*asc$A+10:?c
                       & 1+!len:?len
                     | !c:" "&:?c
                     |
                     )
                   & !N !c:?N
                   & ~
                   )
               )
            |   str$!N:?N:#
              & (   @(!N:? [!len)
                  & ( mod$(!N,97):1&out$OK
                    | out$"wrong check digits"
                    )
                | out$"wrong length"
                )
            |   @(!N:? ~#%?c ?)
              & out$(str$("invalid character: '" !c "'"))
            )
        | out$(str$("invalid country code: '" !country "'"))
        )
  )
& IBAN-check$"GB82 WEST 1234 5698 7654 32 9"
& IBAN-check$"GX82 WEST 1234 5698 7654 32"
& IBAN-check$"GB82 WEST 1234 5698 7654 32"
& IBAN-check$GB82WEST12345698765432
& IBAN-check$"gb82 west 1234 5698 7654 32"
& IBAN-check$"GB82 TEST 1234 5698 7654 32"
& IBAN-check$"GB82 WEST 1243 5698 7654 32"
& IBAN-check$"GB82 west 1243 5698 7654 32"
);
```
 wrong length
 invalid country code: 'GX'
 OK
 OK
 invalid country code: 'gb'
 wrong check digits
 wrong check digits
 invalid character: 'w'


## C


```C>#include <alloca.h

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define V(cc, exp) if (!strncmp(iban, cc, 2)) return len == exp

/* Validate country code against expected length. */
int valid_cc(const char *iban, int len)
{
    V("AL", 28); V("AD", 24); V("AT", 20); V("AZ", 28); V("BE", 16); V("BH", 22); V("BA", 20); V("BR", 29);
    V("BG", 22); V("CR", 21); V("HR", 21); V("CY", 28); V("CZ", 24); V("DK", 18); V("DO", 28); V("EE", 20);
    V("FO", 18); V("FI", 18); V("FR", 27); V("GE", 22); V("DE", 22); V("GI", 23); V("GR", 27); V("GL", 18);
    V("GT", 28); V("HU", 28); V("IS", 26); V("IE", 22); V("IL", 23); V("IT", 27); V("KZ", 20); V("KW", 30);
    V("LV", 21); V("LB", 28); V("LI", 21); V("LT", 20); V("LU", 20); V("MK", 19); V("MT", 31); V("MR", 27);
    V("MU", 30); V("MC", 27); V("MD", 24); V("ME", 22); V("NL", 18); V("NO", 15); V("PK", 24); V("PS", 29);
    V("PL", 28); V("PT", 25); V("RO", 24); V("SM", 27); V("SA", 24); V("RS", 22); V("SK", 24); V("SI", 19);
    V("ES", 24); V("SE", 24); V("CH", 21); V("TN", 24); V("TR", 26); V("AE", 23); V("GB", 22); V("VG", 24);

    return 0;
}

/* Remove blanks from s in-place, return its new length. */
int strip(char *s)
{
    int i = -1, m = 0;

    while(s[++i]) {
        s[i - m] = s[i];
        m += s[i] <= 32;
    }

    s[i - m] = 0;
    return i - m;
}

/* Calculate the mod 97 of an arbitrarily large number (as a string). */
int mod97(const char *s, int len)
{
    int i, j, parts = len / 7;
    char rem[10] = "00";

    for (i = 1; i <= parts + (len % 7 != 0); ++i) {
        strncpy(rem + 2, s + (i - 1) * 7, 7);
        j = atoi(rem) % 97;
        rem[0] = j / 10 + '0';
        rem[1] = j % 10 + '0';
    }

    return atoi(rem) % 97;
}

int valid_iban(char *iban)
{
    int i, j, l = 0, sz = strip(iban);
    char *rot, *trans;

    /* Ensure upper alphanumeric input and count letters. */
    for (i = 0; i < sz; ++i) {
        if (!isdigit(iban[i]) && !isupper(iban[i]))
            return 0;
        l += !!isupper(iban[i]);
    }

    if (!valid_cc(iban, sz))
        return 0;

    /* Move the first four characters to the end. */
    rot = alloca(sz);
    strcpy(rot, iban + 4);
    strncpy(rot + sz - 4, iban, 4);

    /* Allocate space for the transformed IBAN. */
    trans = alloca(sz + l + 1);
    trans[sz + l] = 0;

    /* Convert A to 10, B to 11, etc. */
    for (i = j = 0; i < sz; ++i, ++j) {
        if (isdigit(rot[i]))
            trans[j] = rot[i];
        else {
            trans[j]   = (rot[i] - 55) / 10 + '0';
            trans[++j] = (rot[i] - 55) % 10 + '0';
        }
    }

    return mod97(trans, sz + l) == 1;
}

int main(int _, char **argv)
{
    while (--_, *++argv)
        printf("%s is %svalid.\n", *argv, valid_iban(*argv) ? "" : "in");

    return 0;
}
```
 iban 'GB82 WEST 1234 5698 7654 32' GB82TEST12345698765432
 GB82WEST12345698765432 is valid.
 GB82TEST12345698765432 is invalid.


## C++


```cpp
#include <string>
#include <iostream>
#include <boost/algorithm/string.hpp>
#include <map>
#include <algorithm>
#include <cctype>
using namespace boost::algorithm ;

bool isValid ( const std::string &ibanstring )
{
   static std::map<std::string, int> countrycodes
                           { {"AL" , 28} , {"AD" , 24} , {"AT" , 20} , {"AZ" , 28 } ,
			   {"BE" , 16} , {"BH" , 22} , {"BA" , 20} , {"BR" , 29 } ,
			   {"BG" , 22} , {"CR" , 21} , {"HR" , 21} , {"CY" , 28 } ,
			   {"CZ" , 24} , {"DK" , 18} , {"DO" , 28} , {"EE" , 20 } ,
			   {"FO" , 18} , {"FI" , 18} , {"FR" , 27} , {"GE" , 22 } ,
                           {"DE" , 22} , {"GI" , 23} , {"GR" , 27} , {"GL" , 18 } ,
                           {"GT" , 28} , {"HU" , 28} , {"IS" , 26} , {"IE" , 22 } ,
			   {"IL" , 23} , {"IT" , 27} , {"KZ" , 20} , {"KW" , 30 } ,
			   {"LV" , 21} , {"LB" , 28} , {"LI" , 21} , {"LT" , 20 } ,
			   {"LU" , 20} , {"MK" , 19} , {"MT" , 31} , {"MR" , 27 } ,
			   {"MU" , 30} , {"MC" , 27} , {"MD" , 24} , {"ME" , 22 } ,
			   {"NL" , 18} , {"NO" , 15} , {"PK" , 24} , {"PS" , 29 } ,
			   {"PL" , 28} , {"PT" , 25} , {"RO" , 24} , {"SM" , 27 } ,
			   {"SA" , 24} , {"RS" , 22} , {"SK" , 24} , {"SI" , 19 } ,
			   {"ES" , 24} , {"SE" , 24} , {"CH" , 21} , {"TN" , 24 } ,
			   {"TR" , 26} , {"AE" , 23} , {"GB" , 22} , {"VG" , 24 } } ;
   std::string teststring( ibanstring ) ;
   erase_all( teststring , " " ) ; //defined in boost/algorithm/string.hpp
   if ( countrycodes.find( teststring.substr(0 , 2 )) == countrycodes.end( ) )
      return false ;
   if ( teststring.length( ) != countrycodes[ teststring.substr( 0 , 2 ) ] )
      return false ;
   if (!all(teststring, is_alnum()))
      return false ;
   to_upper( teststring ) ;
   std::rotate(teststring.begin(), teststring.begin() + 4, teststring.end());

   std::string numberstring ;//will contain the letter substitutions
   for (const auto& c : teststring)
   {
      if (std::isdigit(c))
	 numberstring += c  ;
      if (std::isupper(c))
	 numberstring += std::to_string(static_cast<int>(c) - 55);
   }
   //implements a stepwise check for mod 97 in chunks of 9 at the first time
   // , then in chunks of seven prepended by the last mod 97 operation converted
   //to a string
   int segstart = 0 ;
   int step = 9 ;
   std::string prepended ;
   long number = 0 ;
   while ( segstart  < numberstring.length( ) - step ) {
      number = std::stol( prepended + numberstring.substr( segstart , step ) ) ;
      int remainder = number % 97 ;
      prepended =  std::to_string( remainder ) ;
      if ( remainder < 10 )
	 prepended = "0" + prepended ;
      segstart = segstart + step ;
      step = 7 ;
   }
   number = std::stol( prepended + numberstring.substr( segstart )) ;
   return ( number % 97 == 1 ) ;
}

void SayValidity(const std::string& iban)
{
    std::cout << iban << (isValid(iban) ? " is " : " is not ") << "valid\n";
}

int main( )
{
   SayValidity("GB82 WEST 1234 5698 7654 32");
   SayValidity("GB82TEST12345698765432");
   return 0 ;
}
```
 GB82 WEST 1234 5698 7654 32 is valid!
 GB82TEST12345698765432 is not valid!


## C#


```c#
    public class IbanValidator : IValidateTypes
    {
        public ValidationResult Validate(string value)
        {
            // Check if value is missing
            if (string.IsNullOrEmpty(value))
                return ValidationResult.ValueMissing;

            if (value.Length < 2)
                return ValidationResult.ValueTooSmall;

            var countryCode = value.Substring(0, 2).ToUpper();

            int lengthForCountryCode;

            var countryCodeKnown = Lengths.TryGetValue(countryCode, out lengthForCountryCode);
            if (!countryCodeKnown)
            {
                return ValidationResult.CountryCodeNotKnown;
            }

            // Check length.
            if (value.Length < lengthForCountryCode)
                return ValidationResult.ValueTooSmall;

            if (value.Length > lengthForCountryCode)
                return ValidationResult.ValueTooBig;

            value = value.ToUpper();
            var newIban = value.Substring(4) + value.Substring(0, 4);

            newIban = Regex.Replace(newIban, @"\D", match => (match.Value[0] - 55).ToString());

            var remainder = BigInteger.Parse(newIban) % 97;

            if (remainder != 1)
                return ValidationResult.ValueFailsModule97Check;

            return ValidationResult.IsValid;
        }

        public enum ValidationResult
        {
            IsValid,
            ValueMissing,
            ValueTooSmall,
            ValueTooBig,
            ValueFailsModule97Check,
            CountryCodeNotKnown
        }

        private static readonly IDictionary<string, int> Lengths = new Dictionary<string, int>
        {
            {"AL", 28},
            {"AD", 24},
            {"AT", 20},
            {"AZ", 28},
            {"BE", 16},
            {"BH", 22},
            {"BA", 20},
            {"BR", 29},
            {"BG", 22},
            {"CR", 21},
            {"HR", 21},
            {"CY", 28},
            {"CZ", 24},
            {"DK", 18},
            {"DO", 28},
            {"EE", 20},
            {"FO", 18},
            {"FI", 18},
            {"FR", 27},
            {"GE", 22},
            {"DE", 22},
            {"GI", 23},
            {"GR", 27},
            {"GL", 18},
            {"GT", 28},
            {"HU", 28},
            {"IS", 26},
            {"IE", 22},
            {"IL", 23},
            {"IT", 27},
            {"KZ", 20},
            {"KW", 30},
            {"LV", 21},
            {"LB", 28},
            {"LI", 21},
            {"LT", 20},
            {"LU", 20},
            {"MK", 19},
            {"MT", 31},
            {"MR", 27},
            {"MU", 30},
            {"MC", 27},
            {"MD", 24},
            {"ME", 22},
            {"NL", 18},
            {"NO", 15},
            {"PK", 24},
            {"PS", 29},
            {"PL", 28},
            {"PT", 25},
            {"RO", 24},
            {"SM", 27},
            {"SA", 24},
            {"RS", 22},
            {"SK", 24},
            {"SI", 19},
            {"ES", 24},
            {"SE", 24},
            {"CH", 21},
            {"TN", 24},
            {"TR", 26},
            {"AE", 23},
            {"GB", 22},
            {"VG", 24}
        };
    }
```
Demonstrating:

```c#
    public class When_the_IbanValidator_is_told_to_Validate
    {
        [Fact]
        public void It_should_return_an_error_when_there_is_no_value_provided()
        {
            // Assert
            const string value = "";
            var validator = new IbanValidator();

            // Act
            var result = validator.Validate(value);

            // Assert
            Assert.Equal(ValidationResult.ValueMissing, result);
        }

        [Fact]
        public void It_should_return_an_error_when_the_value_length_is_to_short()
        {
            // Assert
            const string value = "BE1800165492356";
            var validator = new IbanValidator();

            // Act
            var result = validator.Validate(value);

            // Assert
            Assert.Equal(ValidationResult.ValueTooSmall, result);
        }

        [Fact]
        public void It_should_return_an_error_when_the_value_length_is_to_big()
        {
            // Assert
            const string value = "BE180016549235656";
            var validator = new IbanValidator();

            // Act
            var result = validator.Validate(value);

            // Assert
            Assert.Equal(ValidationResult.ValueTooBig, result);
        }

        [Fact]
        public void It_should_return_an_error_when_the_value_fails_the_module_check()
        {
            // Assert
            const string value = "BE18001654923566";
            var validator = new IbanValidator();

            // Act
            var result = validator.Validate(value);

            // Assert
            Assert.Equal(ValidationResult.ValueFailsModule97Check, result);
        }

        [Fact]
        public void It_should_return_an_error_when_an_unkown_country_prefix_used()
        {
            // Assert
            const string value = "XX82WEST12345698765432";
            var validator = new IbanValidator();

            // Act
            var result = validator.Validate(value);

            // Assert
            Assert.Equal(ValidationResult.CountryCodeNotKnown, result);
        }

        [Fact]
        public void It_should_return_valid_when_a_valid_value_is_provided()
        {
            // Assert
            const string value = "BE18001654923565";
            var validator = new IbanValidator();

            // Act
            var result = validator.Validate(value);

            // Assert
            Assert.Equal(ValidationResult.IsValid, result);
        }

        [Fact]
        public void It_should_return_valid_when_a_valid_foreign_value_is_provided()
        {
            // Assert
            const string value = "GB82WEST12345698765432";
            var validator = new IbanValidator();

            // Act
            var result = validator.Validate(value);

            // Assert
            Assert.Equal(ValidationResult.IsValid, result);
        }
    }
```


=={{header|Caché ObjectScript}}==


```cos
Class Utils.Validate [ Abstract ]
{

ClassMethod VerifyIBAN(pIBAN As %String = "") As %Boolean
{
	// remove spaces and define parts
	Set iban=$Translate(pIBAN, " ")
	Set cc=$Extract(iban, 1, 2)
	Set cd=$Extract(iban, 3, 4)
	Set bban=$Extract(iban, 5, *)

	// ensure IBAN is correct format
	If $Match(iban, ..GetIBANPattern(cc))=0 Quit 0

	// compare result and return
	Quit cd=..GetIBANCheckDigit(cc, bban)
}

ClassMethod GetIBANCheckDigit(pCC As %String, pBBAN As %String) As %Integer [ Internal, Private ]
{
	Set str=pBBAN_pCC_"00"
	For i=1:1 {
		Set chr=$Extract(str, i) If chr="" Quit
		If chr?1U Set $Extract(str, i)=$ASCII(chr)-55
	}
	Set cd=98-..GetModulus(str, 97)
	Quit $Select($Length(cd)=2: cd, 1: "0"_cd)
}

ClassMethod GetModulus(pNum As %Integer, pDiv As %Integer) As %Integer [ Internal, Private ]
{
	While $Length(pNum)>9 {
		Set $Extract(pNum, 1, 9)=$Extract(pNum, 1, 9)#pDiv
	}
	Quit pNum#pDiv
}

ClassMethod GetIBANPattern(pCC As %String = "") As %String [ Internal, Private ]
{
	Quit $Case(pCC,
		"AL": "^AL\d{10}[0-9A-Z]{16}$",
		"AD": "^AD\d{10}[0-9A-Z]{12}$",
		"AT": "^AT\d{18}$",
		"BH": "^BH\d{2}[A-Z]{4}[0-9A-Z]{14}$",
		"BE": "^BE\d{14}$",
		"BA": "^BA\d{18}$",
		"BG": "^BG\d{2}[A-Z]{4}\d{6}[0-9A-Z]{8}$",
		"HR": "^HR\d{19}$",
		"CY": "^CY\d{10}[0-9A-Z]{16}$",
		"CZ": "^CZ\d{22}$",
		"DK": "^DK\d{16}$|^FO\d{16}$|^GL\d{16}$",
		"DO": "^DO\d{2}[0-9A-Z]{4}\d{20}$",
		"EE": "^EE\d{18}$",
		"FI": "^FI\d{16}$",
		"FR": "^FR\d{12}[0-9A-Z]{11}\d{2}$",
		"GE": "^GE\d{2}[A-Z]{2}\d{16}$",
		"DE": "^DE\d{20}$",
		"GI": "^GI\d{2}[A-Z]{4}[0-9A-Z]{15}$",
		"GR": "^GR\d{9}[0-9A-Z]{16}$",
		"HU": "^HU\d{26}$",
		"IS": "^IS\d{24}$",
		"IE": "^IE\d{2}[A-Z]{4}\d{14}$",
		"IL": "^IL\d{21}$",
		"IT": "^IT\d{2}[A-Z]\d{10}[0-9A-Z]{12}$",
		"KZ": "^[A-Z]{2}\d{5}[0-9A-Z]{13}$",
		"KW": "^KW\d{2}[A-Z]{4}22!$",
		"LV": "^LV\d{2}[A-Z]{4}[0-9A-Z]{13}$",
		"LB": "^LB\d{6}[0-9A-Z]{20}$",
		"LI": "^LI\d{7}[0-9A-Z]{12}$",
		"LT": "^LT\d{18}$",
		"LU": "^LU\d{5}[0-9A-Z]{13}$",
		"MK": "^MK\d{5}[0-9A-Z]{10}\d{2}$",
		"MT": "^MT\d{2}[A-Z]{4}\d{5}[0-9A-Z]{18}$",
		"MR": "^MR13\d{23}$",
		"MU": "^MU\d{2}[A-Z]{4}\d{19}[A-Z]{3}$",
		"MC": "^MC\d{12}[0-9A-Z]{11}\d{2}$",
		"ME": "^ME\d{20}$",
		"NL": "^NL\d{2}[A-Z]{4}\d{10}$",
		"NO": "^NO\d{13}$",
		"PL": "^PL\d{10}[0-9A-Z]{,16}n$",
		"PT": "^PT\d{23}$",
		"RO": "^RO\d{2}[A-Z]{4}[0-9A-Z]{16}$",
		"SM": "^SM\d{2}[A-Z]\d{10}[0-9A-Z]{12}$",
		"SA": "^SA\d{4}[0-9A-Z]{18}$",
		"RS": "^RS\d{20}$",
		"SK": "^SK\d{22}$",
		"SI": "^SI\d{17}$",
		"ES": "^ES\d{22}$",
		"SE": "^SE\d{22}$",
		"CH": "^CH\d{7}[0-9A-Z]{12}$",
		"TN": "^TN59\d{20}$",
		"TR": "^TR\d{7}[0-9A-Z]{17}$",
		"AE": "^AE\d{21}$",
		"GB": "^GB\d{2}[A-Z]{4}\d{14}$",
		: " ")
}

}
```
```txt
USER>For  { Read iban Quit:iban=""  Write " => ", ##class(Utils.Validate).VerifyIBAN(iban), ! }
GB82 WEST 1234 5698 7654 32 => 1
GB82 TEST 1234 5698 7654 32 => 0
GR16 0110 1250 0000 0001 2300 695 => 1
GB29 NWBK 6016 1331 9268 19 => 1
SA03 8000 0000 6080 1016 7519 => 1
CH93 0076 2011 6238 5295 7 => 1
IL62 0108 0000 0009 9999 999 => 1

USER>
```



## Clojure


```Clojure
(def explen
  {"AL" 28 "AD" 24 "AT" 20 "AZ" 28 "BE" 16 "BH" 22 "BA" 20 "BR" 29
   "BG" 22 "CR" 21 "HR" 21 "CY" 28 "CZ" 24 "DK" 18 "DO" 28 "EE" 20
   "FO" 18 "FI" 18 "FR" 27 "GE" 22 "DE" 22 "GI" 23 "GR" 27 "GL" 18
   "GT" 28 "HU" 28 "IS" 26 "IE" 22 "IL" 23 "IT" 27 "KZ" 20 "KW" 30
   "LV" 21 "LB" 28 "LI" 21 "LT" 20 "LU" 20 "MK" 19 "MT" 31 "MR" 27
   "MU" 30 "MC" 27 "MD" 24 "ME" 22 "NL" 18 "NO" 15 "PK" 24 "PS" 29
   "PL" 28 "PT" 25 "RO" 24 "SM" 27 "SA" 24 "RS" 22 "SK" 24 "SI" 19
   "ES" 24 "SE" 24 "CH" 21 "TN" 24 "TR" 26 "AE" 23 "GB" 22 "VG" 24})

(defn valid-iban? [iban]
  (let [iban (apply str (remove #{\space \tab} iban))]
    (cond
      ; Ensure upper alphanumeric input.
      (not (re-find #"^[\dA-Z]+$" iban)) false
      ; Validate country code against expected length.
      (not= (explen (subs iban 0 2)) (count iban)) false
      :else
      (let [rot   (flatten (apply conj (split-at 4 iban)))
            trans (map #(read-string (str "36r" %)) rot)]
        (= 1 (mod (bigint (apply str trans)) 97))))))

(prn (valid-iban? "GB82 WEST 1234 5698 7654 32")  ; true
     (valid-iban? "GB82 TEST 1234 5698 7654 32")) ; false
```



## COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. iban-main.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  iban                    PIC X(50).
       01  iban-flag               PIC X.
           88  is-valid            VALUE "Y", FALSE "N".

       PROCEDURE DIVISION.
       main-line.
           MOVE "GB82 WEST 1234 5698 7654 32" TO iban
           PERFORM display-validity

           MOVE "GB82 TEST 1234 5698 7654 32" TO iban
           PERFORM display-validity

           GOBACK
           .
       display-validity.
           CALL "validate-iban" USING CONTENT iban, REFERENCE iban-flag
           IF is-valid
               DISPLAY FUNCTION TRIM(iban) " is valid."
           ELSE
               DISPLAY FUNCTION TRIM(iban) " is not valid."
           END-IF
           .
       END PROGRAM iban-main.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. validate-iban.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  country-lengths-area    VALUE "AD24AE23AL28AT20AZ28BA20BE16"
           & "BG22BH22BR29CH21CR21CY28CZ24DE22DK18DO28EE20ES24FI18FO18F"
           & "R27GB22GE22GI23GL18GR27GT28HR21HU28IE22IL23IS26IT27KW30KZ"
           & "20LB28LI21LT20LU20LV21MC27MD24ME22MK19MR27MT31MU30NL18NO1"
           & "5PK24PL28PS29PT25RO24RS22SA24SE24SI19SK24SM27TN24TR26VG24"
           .
           03  country-lengths     OCCURS 64 TIMES
                                   INDEXED BY country-lengths-idx.
               05  country-code    PIC XX.
               05  country-len     PIC 99.

       01  offset                  PIC 99.

       01  i                       PIC 99.

       01  len                     PIC 99.

       LINKAGE SECTION.
       01  iban                    PIC X(50).

       01  valid-flag              PIC X.
           88  is-valid            VALUE "Y", FALSE "N".

       PROCEDURE DIVISION USING iban, valid-flag.
           MOVE FUNCTION UPPER-CASE(iban) TO iban
           CALL "remove-spaces" USING iban

           *> Check if country-code and length are correct
           INITIALIZE len
           INSPECT iban TALLYING len FOR CHARACTERS BEFORE SPACE
           SET country-lengths-idx TO 1
           SEARCH country-lengths
               AT END
                   SET is-valid TO FALSE
                   GOBACK

               WHEN country-code (country-lengths-idx) = iban (1:2)
                   IF country-len (country-lengths-idx) NOT = len
                       SET is-valid TO FALSE
                       GOBACK
                   END-IF
           END-SEARCH

           CALL "create-iban-number" USING CONTENT len, REFERENCE iban

           *> Mod 97 number formed.
           IF FUNCTION MOD(iban, 97) = 1
               SET is-valid TO TRUE
           ELSE
               SET is-valid TO FALSE
           END-IF
           .

       IDENTIFICATION DIVISION.
       PROGRAM-ID. remove-spaces.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  i                       PIC 99.
       01  offset                  PIC 99.

       LINKAGE SECTION.
       01  str                     PIC X(50).

       PROCEDURE DIVISION USING str.
           INITIALIZE offset
           PERFORM VARYING i FROM 1 BY 1 UNTIL i > 50
               EVALUATE TRUE
                   WHEN str (i:1) = SPACE
                       ADD 1 TO offset

                   WHEN offset NOT = ZERO
                       MOVE str (i:1) TO str (i - offset:1)
               END-EVALUATE
           END-PERFORM
           MOVE SPACES TO str (50 - offset + 1:)
           .
       END PROGRAM remove-spaces.


       IDENTIFICATION DIVISION.
       PROGRAM-ID. create-iban-number.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  first-four              PIC X(4).

       01  iban-num                PIC X(50).
       01  digit-num               PIC 99 VALUE 1.

       01  i                       PIC 99.

       01  letter-num              PIC 99.

       LINKAGE SECTION.
       01  len                     PIC 99.

       01  iban                    PIC X(50).

       PROCEDURE DIVISION USING len, iban.
           *> Move characters into final positions.
           MOVE iban (1:4) TO first-four
           MOVE iban (5:) TO iban
           MOVE first-four TO iban (len - 3:)

           *> Convert letters to numbers.
           INITIALIZE iban-num, digit-num ALL TO VALUE
           PERFORM VARYING i FROM 1 BY 1
                   UNTIL i > len OR iban (i:1) = SPACE
               IF iban (i:1) IS NUMERIC
                   MOVE iban (i:1) TO iban-num (digit-num:1)
                   ADD 1 TO digit-num
               ELSE
                   COMPUTE letter-num =
                       FUNCTION ORD(iban (i:1)) - FUNCTION ORD("A") + 10
                   MOVE letter-num TO iban-num (digit-num:2)
                   ADD 2 TO digit-num
               END-IF
           END-PERFORM

           MOVE iban-num TO iban
           .

       END PROGRAM create-iban-number.

       END PROGRAM validate-iban.
```
```txt

GB82 WEST 1234 5698 7654 32 is valid.
GB82 TEST 1234 5698 7654 32 is not valid.

```



## Common Lisp


```lisp

;;
;; List of the IBAN code lengths per country.
;;
(defvar *IBAN-code-length* '((15 . ("NO"))
                             (16 . ("BE"))
                             (18 . ("DK" "FO" "FI" "GL" "NL"))
                             (19 . ("MK" "SI"))
                             (20 . ("AT" "BA" "EE" "KZ" "LT" "LU"))
                             (21 . ("CR" "HR" "LV" "LI" "CH"))
                             (22 . ("BH" "BG" "GE" "DE" "IE" "ME" "RS" "GB"))
                             (23 . ("GI" "IL" "AE"))
                             (24 . ("AD" "CZ" "MD" "PK" "RO" "SA" "SK" "ES" "SE" "TN" "VG"))
                             (25 . ("PT"))
                             (26 . ("IS" "TR"))
                             (27 . ("FR" "GR" "IT" "MR" "MC" "SM"))
                             (28 . ("AL" "AZ" "CY" "DO" "GT" "HU" "LB" "PL"))
                             (29 . ("BR" "PS"))
                             (30 . ("KW" "MU"))
                             (31 . ("MT"))))

;;
;; The IBAN-character function verifies whether the number contains the correct characters only. There is
;; a built in function to verify for alphanumeric characters, but it includes characters beyond ASCII range.
;;
(defun IBAN-characters (iban)
  (flet ((valid-alphanum (ch)
           (or (and (char<= #\A ch)
                    (char>= #\Z ch))
               (and (char<= #\0 ch)
                    (char>= #\9 ch)))))
    (loop for char across iban
          always (valid-alphanum char))))

;;
;; The function IBAN-length verifies that the length of the number is correct. The code lengths
;; are retrieved from the table *IBAN-code-lengths*.
;;
(defun IBAN-length (iban)
  (loop :for  (len . country) :in *IBAN-code-length*
        :with iban-country = (subseq iban 0 2)
        :do
    (when (find iban-country country :test #'string=) (return (= len (length iban))))))

;;
;; The function IBAN-to-integer converts an IBAN code into an integer number.
;; Note: The conversion follows the rules stated in the wiki page.
;;
(defun IBAN-to-integer (iban)
  (let ((character-base (- (char-code #\A) 10)))
    (parse-integer
      (format nil "~{~a~}" (map 'list #'(lambda(X) (if (alpha-char-p X) (- (char-code X) character-base) X ))
                                      (concatenate 'string (subseq iban 4) (subseq iban 0 4)))))))
;;
;; The function IBAN-verify checks that the code contains right character set, has the
;; country specific length and has the correct check sum.
;;
(defun IBAN-verify (iban)
  (flet ((validp (X) (and (IBAN-characters X)
                          (IBAN-length X)
                          (= 1 (mod (IBAN-to-integer X) 97)))))
    (validp (remove #\Space iban))))

```

'''Output:'''

```txt

* (iban-verify "GB82 WEST 1234 5698 7654 32")

T
* (iban-verify "GB82 TEST 1234 5698 7654 32")

NIL

```



## D

```d
import std.stdio, std.string, std.regex, std.conv, std.bigint,
       std.algorithm, std.ascii;

immutable int[string] country2len;
static this() {
    country2len = ["AL":28, "AD":24, "AT":20, "AZ":28, "BE":16,
    "BH":22, "BA":20, "BR":29, "BG":22, "CR":21, "HR":21, "CY":28,
    "CZ":24, "DK":18, "DO":28, "EE":20, "FO":18, "FI":18, "FR":27,
    "GE":22, "DE":22, "GI":23, "GR":27, "GL":18, "GT":28, "HU":28,
    "IS":26, "IE":22, "IL":23, "IT":27, "KZ":20, "KW":30, "LV":21,
    "LB":28, "LI":21, "LT":20, "LU":20, "MK":19, "MT":31, "MR":27,
    "MU":30, "MC":27, "MD":24, "ME":22, "NL":18, "NO":15, "PK":24,
    "PS":29, "PL":28, "PT":25, "RO":24, "SM":27, "SA":24, "RS":22,
    "SK":24, "SI":19, "ES":24, "SE":24, "CH":21, "TN":24, "TR":26,
    "AE":23, "GB":22, "VG":24];
}

bool validIBAN(string iban) {
    // Ensure upper alphanumeric input.
    iban = iban.removechars(whitespace);
    if (!iban.match(r"^[\dA-Z]+$"))
        return false;

    // Validate country code against expected length.
    if (iban.length != country2len[iban[0 .. 2]])
        return false;

    // Shift and convert. BASE 36: 0..9,A..Z -> 0..35.
    iban = iban[4 .. $] ~ iban[0 .. 4];
    return iban.map!(c => [c].to!int(36).text).join.BigInt % 97 == 1;
}

void main() {
    foreach (account; ["GB82 WEST 1234 5698 7654 32",
                       "GB82 TEST 1234 5698 7654 32"])
        writefln("%s validation is: %s", account, account.validIBAN);
}
```
```txt
GB82 WEST 1234 5698 7654 32 validation is: true
GB82 TEST 1234 5698 7654 32 validation is: false
```



## Elixir

```elixir
defmodule IBAN do
  @len %{ AL: 28, AD: 24, AT: 20, AZ: 28, BE: 16, BH: 22, BA: 20, BR: 29,
          BG: 22, CR: 21, HR: 21, CY: 28, CZ: 24, DK: 18, DO: 28, EE: 20,
          FO: 18, FI: 18, FR: 27, GE: 22, DE: 22, GI: 23, GR: 27, GL: 18,
          GT: 28, HU: 28, IS: 26, IE: 22, IL: 23, IT: 27, KZ: 20, KW: 30,
          LV: 21, LB: 28, LI: 21, LT: 20, LU: 20, MK: 19, MT: 31, MR: 27,
          MU: 30, MC: 27, MD: 24, ME: 22, NL: 18, NO: 15, PK: 24, PS: 29,
          PL: 28, PT: 25, RO: 24, SM: 27, SA: 24, RS: 22, SK: 24, SI: 19,
          ES: 24, SE: 24, CH: 21, TN: 24, TR: 26, AE: 23, GB: 22, VG: 24 }

  def valid?(iban) do
    iban = String.replace(iban, ~r/\s/, "")
    if Regex.match?(~r/^[\dA-Z]+$/, iban) do
      cc = String.slice(iban, 0..1) |> String.to_atom
      if String.length(iban) == @len[cc] do
        {left, right} = String.split_at(iban, 4)
        num = String.codepoints(right <> left)
              |> Enum.map_join(fn c -> String.to_integer(c,36) end)
              |> String.to_integer
        rem(num,97) == 1
      else
        false
      end
    else
      false
    end
  end
end

[ "GB82 WEST 1234 5698 7654 32",
  "gb82 west 1234 5698 7654 32",
  "GB82 WEST 1234 5698 7654 320",
  "GB82WEST12345698765432",
  "GB82 TEST 1234 5698 7654 32",
  "ZZ12 3456 7890 1234 5678 12"  ]
|> Enum.each(fn iban -> IO.puts "#{IBAN.valid?(iban)}\t#{iban}" end)
```


```txt

true    GB82 WEST 1234 5698 7654 32
false   gb82 west 1234 5698 7654 32
false   GB82 WEST 1234 5698 7654 320
true    GB82WEST12345698765432
false   GB82 TEST 1234 5698 7654 32
false   ZZ12 3456 7890 1234 5678 12

```


=={{header|F_Sharp|F#}}==

```fsharp
open System
open System.Text.RegularExpressions

// A little utility to thread a negative test result (Option.None) through a
// pipeline of tests
let inline (|~>) valOption proc =
    match valOption with
    | Some(value) -> proc value
    | None -> None

[<EntryPoint>]
let main argv =
    let iban = if argv.Length = 0 then "" else argv.[0]
    iban
    |> (fun iban ->     // Check for illegal characters
            if Regex.IsMatch(iban, @"[^0-9A-Za-z ]") then None else Some(iban.ToUpper().Replace(" ", "")))
    |~> (fun iban ->    // Check length per country code
            let lengthPerCountry =
                dict [
                    ("AL", 28); ("AD", 24); ("AT", 20); ("AZ", 28); ("BE", 16); ("BH", 22); ("BA", 20); ("BR", 29);
                    ("BG", 22); ("CR", 21); ("HR", 21); ("CY", 28); ("CZ", 24); ("DK", 18); ("DO", 28); ("EE", 20);
                    ("FO", 18); ("FI", 18); ("FR", 27); ("GE", 22); ("DE", 22); ("GI", 23); ("GR", 27); ("GL", 18);
                    ("GT", 28); ("HU", 28); ("IS", 26); ("IE", 22); ("IL", 23); ("IT", 27); ("KZ", 20); ("KW", 30);
                    ("LV", 21); ("LB", 28); ("LI", 21); ("LT", 20); ("LU", 20); ("MK", 19); ("MT", 31); ("MR", 27);
                    ("MU", 30); ("MC", 27); ("MD", 24); ("ME", 22); ("NL", 18); ("NO", 15); ("PK", 24); ("PS", 29);
                    ("PL", 28); ("PT", 25); ("RO", 24); ("SM", 27); ("SA", 24); ("RS", 22); ("SK", 24); ("SI", 19);
                    ("ES", 24); ("SE", 24); ("CH", 21); ("TN", 24); ("TR", 26); ("AE", 23); ("GB", 22); ("VG", 24);
                ]
            let country = iban.Substring(0, Math.Min(2, iban.Length))
            match lengthPerCountry.TryGetValue(country) with
            | true, length ->   // country should have iban of this length
                if length = iban.Length then Some(iban) else None
            | _ -> None     // country not known
        )
    |~> (fun iban -> Some(iban.Substring(4) + iban.Substring(0,4)))
    |~> (fun iban ->
            let replaceBase36LetterWithBase10String (s : string) (c :char) = s.Replace(c.ToString(), ((int)c - (int)'A' + 10).ToString())
            Some(List.fold replaceBase36LetterWithBase10String iban [ 'A' .. 'Z' ]))
    |~> (fun iban ->    // iban mod 97
            // We could have used BigInteger, but with a loop by 7 char each
            // over the long digit string we get away with Int32 arithmetic
            // (as described in the Wikipedia article)
            let reduceOnce r n = Int32.Parse(r.ToString() + n) % 97
            let rest =
                Regex.Matches(iban.Substring(2), @"\d{1,7}") |> Seq.cast |> Seq.map (fun x -> x.ToString())
                |> Seq.fold reduceOnce (reduceOnce 0 (iban.Substring(0,2)))
            // an iban needs a rest of 1
            if rest = 1 then Some(1) else None
        )
    |> function | Some(_) -> "a valid IBAN" | None -> "an invalid IBAN"
    |> printfn "%s is %s" iban
    0
```
 >Rosetta.exe "GB82 WEST 1234 5698 7654 32"
 GB82 WEST 1234 5698 7654 32 is a valid IBAN

 >Rosetta.exe "GB82 TEST 1234 5698 7654 32"
 GB82 TEST 1234 5698 7654 32 is an invalid IBAN


## Factor


```factor
USING: assocs combinators.short-circuit formatting kernel math
math.parser regexp sequences sets qw unicode ;
IN: rosetta-code.iban

<PRIVATE

CONSTANT: countries H{
    { 15 qw{ NO } }
    { 16 qw{ BE } }
    { 18 qw{ DK FO FI GL NL } }
    { 19 qw{ MK SI } }
    { 20 qw{ AT BA EE KZ LT LU } }
    { 21 qw{ CR HR LV LI CH } }
    { 22 qw{ BH BG GE DE IE ME RS GB } }
    { 23 qw{ GI IL AE } }
    { 24 qw{ AD CZ MD PK RO SA SK ES SE TN VG } }
    { 25 qw{ PT } }
    { 26 qw{ IS TR } }
    { 27 qw{ FR GR IT MR MC SM } }
    { 28 qw{ AL AZ CY DO GT HU LB PL } }
    { 29 qw{ BR PS } }
    { 30 qw{ KW MU } }
    { 31 qw{ MT } }
}

: valid-chars? ( str -- ? ) R/ [A-Z0-9]+/ matches? ;

: valid-length? ( str -- ? )
    [ 2 head ] [ length ] bi countries at member? ;

: valid-checksum? ( str -- ? )
    4 cut swap append [ digit> number>string ] { } map-as
    concat string>number 97 mod 1 = ;

PRIVATE>

: valid-iban? ( str -- ? )
    " " without {
        [ valid-chars? ] [ valid-length? ] [ valid-checksum? ]
    } 1&& ;

: iban-demo ( -- )
    "GB82 WEST 1234 5698 7654 32"
    "GB82 TEST 1234 5698 7654 32"
    [
        dup valid-iban? "may be a valid" "is an invalid" ?
        "%s %s IBAN\n" printf
    ] bi@ ;

MAIN: iban-demo
```

```txt

GB82 WEST 1234 5698 7654 32 may be a valid IBAN
GB82 TEST 1234 5698 7654 32 is an invalid IBAN

```



## Forth

<lang>include lib/ulcase.4th                 \ for S>UPPER
include lib/triple.4th                 \ for UT/MOD
include lib/cstring.4th                \ for C/STRING
include lib/todbl.4th                  \ for S>DOUBLE

0 constant ud>t                        \ convert unsigned double to triple
88529281 constant 97^4                 \ first stage modulus
char A 10 - negate +constant c>u       \ convert character to IBAN digit

: bank>t u>d rot 3 - 0 ?do 10 mu* loop 1000000000 ut* ;
                                       \ convert country part to unsigned
: country>u                            ( a n -- u)
  c/string c>u 10000 * >r c/string c>u 100 * >r number 100 mod abs r> + r> +
;
                                       \ convert bank part to unsigned
: bank>u                               \ a n -- u)
  c/string c>u 1000000 * >r            \ get first digit and shift
  c/string c>u 10000 * >r              \ get second digit and shift
  c/string c>u 100 * >r                \ get third digit and shift
  drop c@ c>u r> + r> + r> +           \ combine all digits to number
;

: iban>t                               ( a n -- triple)
  s>upper                              \ convert to upper case and get country
  over 4 country>u >r 4 /string        \ get bank part, save length, convert
  over 4 bank>u >r 4 /string tuck s>double
  1000000 mu* r> -rot r> u>d d+ 2>r    \ now assemble everything except bank
  bank>t 2r> ud>t t+                   \ shift bank part and convert to triple
;
                                       ( a n -- f)
: iban? iban>t 97^4 ut/mod 2drop 97 mod 1 = ;
                                       \ perform modulus 97 in two stages
: checkiban                            ( --)
  ." Enter your IBAN: " refill drop 0 parse -trailing iban?
  if ." Valid" else ." Invalid" then cr
;

checkiban
```

```txt
linux:~> pp4th -x chkiban.4th
Enter your IBAN: GB82WEST12345698765432
Valid
linux:~> pp4th -x chkiban.4th
Enter your IBAN: GB82TEST12345698765432
Invalid
```



## Fortran


```fortran

program ibancheck

   use ISO_FORTRAN_ENV

   implicit none

   character(4), dimension(75) :: cc = (/ &
            "AD24","AE23","AL28","AT20","AZ28","BA20","BE16","BG22","BH22","BR29", &
            "BY28","CH21","CR22","CY28","CZ24","DE22","DK18","DO28","EE20","ES24", &
            "FI18","FO18","FR27","GB22","GE22","GI23","GL18","GR27","GT28","HR21", &
            "HU28","IE22","IL23","IQ23","IS26","IT27","JO30","KW30","KZ20","LB28", &
            "LC32","LI21","LT20","LU20","LV21","MC27","MD24","ME22","MK19","MR27", &
            "MT31","MU30","NL18","NO15","PK24","PL28","PS29","PT25","QA29","RO24", &
            "RS22","SA24","SC31","SE24","SI19","SK24","SM27","ST25","SV28","TL23", &
            "TN24","TR26","UA29","VG24","XK20" /)

    character(34), dimension(12) :: ibans = (/ "GB82 WEST 1234 5698 7654 32       ", &
                                               "GB82WEST12345698765432            ", &
                                               "gb82 west 1234 5698 7654 32       ", &
                                               "GB82 TEST 1234 5698 7654 32       ", &
                                               "GR16 0110 1250 0000 0001 2300 695 ", &
                                               "GB29 NWBK 6016 1331 9268 19       ", &
                                               "SA03 8000 0000 6080 1016 7519     ", &
                                               "CH93 0076 2011 6238 5295 7        ", &
                                               "IL62 0108 0000 0009 9999 999      ", &
                                               "IL62-0108-0000-0009-9999-999      ", &
                                               "US12 3456 7890 0987 6543 210      ", &
                                               "GR16 0110 1250 0000 0001 2300 695X" /)

    integer :: i

    do i=1, size(ibans)
        if (checkIBAN(trim(ibans(i)))) then
            print *, "  valid IBAN: ", trim(ibans(i))
        else
            print *, "invalid IBAN: ", trim(ibans(i))
        end if
    end do

    return

contains

    function checkIBAN(ibancode) result(valid)
        character(len=*), intent(in) :: ibancode
        character(len=len(ibancode)) :: iban
        logical :: valid
        integer(int32) :: j, ascii, ibanSize
        character(100) :: ibanRearrange, ibantoint
        character(2) :: temp
        valid = .false.

        iban = remove_blanks(ibancode)
        ibanSize = checkCountryCode(iban)
        if (ibanSize == len(trim(iban))) then
            ibanRearrange = iban(5:ibanSize)//iban(1:4)
            ibantoint = ""
            do j=1, ibanSize
                ascii = ichar(ibanRearrange(j:j))
                if ((ascii >= 65) .and. (ascii<=90)) then
                    write (temp,fmt='(I2)') ascii-55
                    ibantoint = trim(ibantoint) // temp
                else
                    ibantoint = trim(ibantoint) // ibanRearrange(j:j)
                end if
            end do
            if (mod97(ibantoint) == 1) then
                valid = .true.
            end if
        end if
    end function checkIBAN

    function mod97(strint) result(res)
        character(len=*), intent(in) :: strint
        integer :: i, num, res
        res = 0
        do  i=1, len(trim(strint))
            read(strint(i:i),*) num
            res = mod((res*10 + num),97);
        end do
    end function mod97

    function checkCountryCode(iban) result(ibanlength)
        character(len=*), intent(in) :: iban
        integer(int16) :: ibanlength, i
        ibanlength = 0
        do i=1, size(cc)
            if (iban(1:2) == cc(i)(1:2)) then
                read(cc(i)(3:4),*) ibanlength
                exit
            end if
        end do
    end function checkCountryCode

    Recursive Function Stripper(string,ch) Result(stripped)
        Implicit None
        character(len=*), intent(in) :: string
        character, intent(in) :: ch
        character(:), allocatable :: stripped

        IF (LEN(string)==1) THEN
           IF (string==ch) THEN
              stripped = ''
           ELSE
              stripped = string
           END IF
        ELSE
           IF (string(1:1)==ch) THEN
              stripped = stripper(string(2:),ch)
           ELSE
              stripped = string(1:1)//stripper(string(2:),ch)
           END IF
        END IF
    END Function stripper

    Function Remove_Blanks(string) Result(stripped)
        Implicit None
        character(len=*), intent(in) ::   string
        character(:), allocatable :: stripped

        stripped = trim(Stripper(trim(Stripper(string,' ')),achar(9)))
    END Function Remove_Blanks

end program ibancheck

```


```txt

   valid IBAN: GB82 WEST 1234 5698 7654 32
   valid IBAN: GB82WEST12345698765432
 invalid IBAN: gb82 west 1234 5698 7654 32
 invalid IBAN: GB82 TEST 1234 5698 7654 32
   valid IBAN: GR16 0110 1250 0000 0001 2300 695
   valid IBAN: GB29 NWBK 6016 1331 9268 19
   valid IBAN: SA03 8000 0000 6080 1016 7519
   valid IBAN: CH93 0076 2011 6238 5295 7
   valid IBAN: IL62 0108 0000 0009 9999 999
 invalid IBAN: IL62-0108-0000-0009-9999-999
 invalid IBAN: US12 3456 7890 0987 6543 210
 invalid IBAN: GR16 0110 1250 0000 0001 2300 695X

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' List updated to release 72, 25 November 2016, of IBAN Registry (75 countries)
Dim Shared countryCodes As String
countryCodes = _
    "AD24 AE23 AL28 AT20 AZ28 BA20 BE16 BG22 BH22 BR29 BY28 CH21 CR22 CY28 CZ24 DE22 " _
    "DK18 DO28 EE20 ES24 FI18 FO18 FR27 GB22 GE22 GI23 GL18 GR27 GT28 HR21 HU28 IE22 " _
    "IL23 IQ23 IS26 IT27 JO30 KW30 KZ20 LB28 LC32 LI21 LT20 LU20 LV21 MC27 MD24 ME22 " _
    "MK19 MR27 MT31 MU30 NL18 NO15 PK24 PL28 PS29 PT25 QA29 RO24 RS22 SA24 SC31 SE24 " _
    "SI19 SK24 SM27 ST25 SV28 TL23 TN24 TR26 UA29 VG24 XK20"

Function checkCountryCode(cc As String) As Boolean
  Return Instr(countryCodes, cc)
End Function

' To avoid having to use the GMP library, a piece-wise calculation is used
Function mod97(s As String) As UInteger
  Dim r As UInteger = ValULng(Left(s, 9)) Mod 97
  Dim start As UInteger = 10
  While start < Len(s)
    r = ValULng(r & Mid(s, start, 7)) Mod 97
    start += 7
  Wend
  Return r
End Function

Function validateIban(iban As Const String) As Boolean
  ' remove spaces from IBAN
  Dim s As String = iban
  Dim count As Integer = 0
  For i As Integer = 0 To Len(s) - 1
    If s[i] = 32 Then
      For j As Integer = i + 1 To Len(s) - 1
         s[j - 1] = s[j]
      Next
      count += 1
    End If
    If i = Len(s) - 1 - count Then Exit For
  Next i
  If count > 0 Then
    s[Len(s) - count] = 0
    Dim p As UInteger Ptr = CPtr(UInteger Ptr, @s)
    *(p + 1) = Len(s) - count ''change length of string in descriptor
  End If

  ' check country code
  Dim isValid As Boolean = checkCountryCode(Left(s, 2) + Str(Len(s)))
  If Not isValid Then Return False

  ' move first 4 characters to end
  s = Mid(s, 5) + Left(s, 4)

  ' replace A to Z with numbers 10 To 35
  For i As Integer = Len(s) To 1 Step -1
    If s[i - 1] >= 65 AndAlso s[i - 1] <= 90 Then
      s = Left(s, i - 1) + Str(s[i - 1] - 55) + Mid(s, i + 1)
    End If
  Next

  ' do mod97 calculation
  Return mod97(s) = 1  '' remainder needs to be 1 for validity
End Function

Dim As String ibans(1 To 2) = {"GB82 WEST 1234 5698 7654 32", "GB82 TEST 1234 5698 7654 32"}
For i As Integer = 1 To 2
  Dim isValid As Boolean = validateIban(ibans(i))
  Print ibans(i); IIf(isValid, " : may be valid", " : is not valid")
Next

Print
Print "Press any key to quit"
Sleep
```


```txt

GB82 WEST 1234 5698 7654 32 : may be valid
GB82 TEST 1234 5698 7654 32 : is not valid

```



## Go



```Go

package main

import (
	"fmt"
	"strings"
	"strconv"
	"math/big"
)

var lCode = map[string]int {
	"AL": 28, "AD": 24, "AT": 20, "AZ": 28, "BE": 16, "BH": 22, "BA": 20, "BR": 29,
  	"BG": 22, "CR": 21, "HR": 21, "CY": 28, "CZ": 24, "DK": 18, "DO": 28, "EE": 20,
  	"FO": 18, "FI": 18, "FR": 27, "GE": 22, "DE": 22, "GI": 23, "GR": 27, "GL": 18,
  	"GT": 28, "HU": 28, "IS": 26, "IE": 22, "IL": 23, "IT": 27, "KZ": 20, "KW": 30,
  	"LV": 21, "LB": 28, "LI": 21, "LT": 20, "LU": 20, "MK": 19, "MT": 31, "MR": 27,
  	"MU": 30, "MC": 27, "MD": 24, "ME": 22, "NL": 18, "NO": 15, "PK": 24, "PS": 29,
  	"PL": 28, "PT": 25, "RO": 24, "SM": 27, "SA": 24, "RS": 22, "SK": 24, "SI": 19,
  	"ES": 24, "SE": 24, "CH": 21, "TN": 24, "TR": 26, "AE": 23, "GB": 22, "VG": 24,
}

var sCode = map[string]int {
	"1": 1, "2": 2, "3": 3, "4": 4, "5": 5, "6": 6, "7": 7, "8": 8, "9": 9,
	"A": 10, "B": 11, "C": 12, "D": 13, "E": 14, "F": 15, "G": 16,
	"H": 17, "I": 18, "J": 19, "K": 20, "L": 21, "M": 22, "N": 23,
	"O": 24, "P": 25, "Q": 26, "R": 27, "S": 28, "T": 29, "U": 30,
	"V": 31, "W": 32, "X": 33, "Y": 34, "Z": 35,
}

func main() {

	var iban string
	var r, s, t, st []string
	u := new(big.Int)
	v := new(big.Int)
	w := new(big.Int)

	iban = "GB82 TEST 1234 5698 7654 32"
	r = strings.Split(iban, " ")
	s = strings.Split(r[0], "")
	t = strings.Split(r[1], "")

	st = []string{ strconv.Itoa(sCode[t[0]]),
					strconv.Itoa(sCode[t[1]]),
					strconv.Itoa(sCode[t[2]]),
					strconv.Itoa(sCode[t[3]]),
					strings.Join(r[2:6], ""),
					strconv.Itoa(sCode[s[0]]),
					strconv.Itoa(sCode[s[1]]),
					strings.Join(s[2:4], ""),
	}

	u.SetString(strings.Join(st, ""), 10)
	v.SetInt64(97)
	w.Mod(u, v)

	if w.Uint64() == 1 && lCode[strings.Join(s[0:2], "")] == len(strings.Join(r, "")) {
		fmt.Printf("IBAN %s looks good!\n", iban)
	} else {
		fmt.Printf("IBAN %s looks wrong!\n", iban)
	}
}

```



```txt

IBAN GB82 WEST 1234 5698 7654 32 looks good!
IBAN GB82 TEST 1234 5698 7654 32 looks wrong!
IBAN CH93 0076 2011 6238 5295 7 looks good!

```



## Groovy


```groovy
def validateIBAN(String iban) {
    def iso = [AL: 28, AD: 24, AT: 20, AZ: 28, BE: 16, BH: 22, BA: 20, BR: 29, BG: 22,
            HR: 21, CY: 28, CZ: 24, DK: 18, DO: 28, EE: 20, FO: 18, FI: 18, FR: 27, GE: 22, DE: 22, GI: 23,
            GL: 18, GT: 28, HU: 28, IS: 26, IE: 22, IL: 23, IT: 27, KZ: 20, KW: 30, LV: 21, LB: 28, LI: 21,
            LT: 20, LU: 20, MK: 19, MT: 31, MR: 27, MU: 30, MC: 27, MD: 24, ME: 22, NL: 18, NO: 15, PK: 24,
            PS: 29, PL: 28, PT: 25, RO: 24, SM: 27, SA: 24, RS: 22, SK: 24, SI: 19, ES: 24, SE: 24, CH: 21,
            TN: 24, TR: 26, AE: 23, GB: 22, VG: 24, GR: 27, CR: 21]

    iban = iban.replaceAll(/\s/, '').toUpperCase()
    if (iban.size() < 4 || iso[iban[0..1]] != iban.size()) return false

    iban = iban[4..-1] + iban[0..<4]

    def number = iban.collect { Character.digit(it as char, 36) }.join('')
    (number as BigInteger).mod(97) == 1
}
```


Testing:

```groovy
[ 'GB82 WEST 1234 5698 7654 32',
  'GB82 TEST 1234 5698 7654 32',
  'GB81 WEST 1234 5698 7654 32',
  'SA03 8000 0000 6080 1016 7519',
  'CH93 0076 2011 6238 5295 7' ].each { iban ->
    println "$iban is ${validateIBAN(iban) ? 'valid' : 'invalid'}"
}
```

```txt
GB82 WEST 1234 5698 7654 32 is valid
GB82 TEST 1234 5698 7654 32 is invalid
GB81 WEST 1234 5698 7654 32 is invalid
SA03 8000 0000 6080 1016 7519 is valid
CH93 0076 2011 6238 5295 7 is valid
```



## Haskell

This program uses the Maybe and Either monads to handle failures. Values of type 'Maybe a' can contain 'Nothing' (no value) or 'Just a' (a value of type 'a'). Values of type 'Either a b' contain 'Left b' (usually indicating failure) or 'Right c' (usually indicating success).

```Haskell
import Data.Char (toUpper)
import Data.Maybe (fromJust)

validateIBAN :: String -> Either String String
validateIBAN [] = Left "No IBAN number."
validateIBAN xs =
    case lookupCountry of
        Nothing -> invalidBecause "Country does not exist."
        Just l  -> if length normalized /= l
                        then invalidBecause "Number length does not match."
                        else check
    where
        -- remove blanks and make all letters uppercase
        normalized = map toUpper $ concat $ words xs
        -- get the country code
        country = take 2 normalized
        -- search number length
        lookupCountry = lookup country countries
        countries :: [(String, Int)]
        countries = zip (words "AL AT BE BA BG HR CZ DO FO FR DE GR GT \
            \IS IL KZ LV LI LU MT MU MD NL PK PL RO SA SK ES CH TR GB \
            \AD AZ BH BR CR CY DK EE FI GE GI GL HU IE IT KW LB LT MK \
            \MR MC ME NO PS PT SM RS SI SE TN AE VG")
            [28,20,16,20,22,21,24,28,18,27,22,27,28,26,23,20,21,21,20,
            31,30,24,18,24,28,24,24,24,24,21,26,22,24,28,22,29,21,28,18,
            20,18,22,23,18,28,22,27,30,28,20,19,27,27,22,15,29,25,27,22,
            19,24,24,23,24]
        digits = ['0'..'9']
        letters = ['A'..'Z']
        -- letters to be replaced
        replDigits = zip letters $ map show [10..35]
        -- digits and letters allowed in the IBAN number
        validDigits = digits ++ letters
        -- see if all digits and letters in the IBAN number are allowed
        sane = all (`elem` validDigits) normalized
        -- take the first 4 digits from the number and put them at its end
        (p1, p2) = splitAt 4 normalized
        p3 = p2 ++ p1
        -- convert the letters to numbers and
        -- convert the result to an integer
        p4 :: Integer
        p4 = read $ concat $ map convertLetter p3
        convertLetter x | x `elem` digits = [x]
                        | otherwise       = fromJust $ lookup x replDigits
        -- see if the number is valid
        check = if sane
                    then if p4 `mod` 97 == 1
                            then Right xs
                            else invalidBecause "Validation failed."
                    else invalidBecause "Number contains illegal digits."

        invalidBecause reason = Left $ "Invalid IBAN number " ++ xs ++
            ": " ++ reason
```
```txt

validateIBAN "GB82 WEST 1234 5698 7654 32"
Right "GB82 WEST 1234 5698 7654 32"

validateIBAN "gb82 West 1234 5698 7654 32"
Right "gb82 West 1234 5698 7654 32"

validateIBAN "GB82 WEST 1234 5698 7654 31"
Left "Invalid IBAN number GB82 WEST 1234 5698 7654 31: Validation failed."

validateIBAN "GW82 WEST 1234 5698 7654 32"
Left "Invalid IBAN number GW82 WEST 1234 5698 7654 32: Country does not exist."

validateIBAN "GB82 WEST 1234 5698 7654 3"
Left "Invalid IBAN number GB82 WEST 1234 5698 7654 3: Number length does not match."

validateIBAN  "GB82 _EST 1234 5698 7654 32"
Left "Invalid IBAN number GB82 _EST 1234 5698 7654 32: Number contains illegal digits."

```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "IBAN.bas"
110 STRING CO$(1 TO 93)*2
120 NUMERIC LG(1 TO 93)
130 FOR I=1 TO 93
140   READ CO$(I),LG(I)
150 NEXT
160 DO
170   PRINT :PRINT "IBAN code: ":INPUT PROMPT ">":IB$
180   IF IB$="" THEN EXIT DO
190   IF IBAN(IB$) THEN
200     PRINT "CRC ok."
210   ELSE
220     SET #102:INK 3:PRINT "CRC error.":SET #102:INK 1
230   END IF
240 LOOP
250 DEF TRIM$(S$)
260   LET T$=""
270   FOR I=1 TO LEN(S$)
280     IF S$(I)>CHR$(47) AND S$(I)<CHR$(91) THEN LET T$=T$&S$(I)
290   NEXT
300   LET TRIM$=T$
310 END DEF
320 DEF IBAN(IB$)
330   LET IB$=TRIM$(UCASE$(IB$)):LET T$="":LET M,IBAN=0:LET N=FIND(IB$(1:2))
340   IF N=0 THEN PRINT "Invalid country code.":EXIT DEF
350   IF LEN(IB$)<>LG(N) THEN PRINT "Number length does not match.":EXIT DEF
360   LET IB$=IB$&IB$(1:4):LET IB$=IB$(5:)
370   CALL CONVERT(IB$)
380   FOR I=1 TO LEN(IB$)
390     LET T$=STR$(M)&IB$(I)
400     LET M=MOD(VAL(T$),97)
410   NEXT
420   IF M=1 THEN LET IBAN=-1
430 END DEF
440 DEF FIND(S$)
450   LET FIND=0
460   LET BO=LBOUND(CO$):LET UP=UBOUND(CO$)
470   DO
480     LET K=INT((BO+UP)/2)
490     IF CO$(K)<S$ THEN LET BO=K+1
500     IF CO$(K)>S$ THEN LET UP=K-1
510   LOOP WHILE BO<=UP AND CO$(K)<>S$
520   IF BO<=UP THEN LET FIND=K
530 END DEF
540 DEF CONVERT(REF S$)
550   LET T$=""
560   FOR I=1 TO LEN(S$)
570     IF S$(I)>CHR$(64) AND S$(I)<CHR$(91) THEN
580       LET T$=T$&STR$(ORD(S$(I))-55)
590     ELSE
600       LET T$=T$&S$(I)
610     END IF
620   NEXT
630   LET S$=T$
640 END DEF
650 DATA AD,24,AE,23,AL,28,AO,25,AT,20,AZ,28,BA,20,BE,16,BF,28,BG,22,BH,22,BI,16,BJ,28,BR,29,BY,28,CG,27,CH,21,CI,28,CM,27,CR,22,CV,25,CY,28,CZ,24,DE,22,DK,18,DO,28,DZ,24,EE,20,EG,27,ES,24,FI,18,FO,18,FR,27,GA,27,GB,22,GE,22,GI,23,GL,18
660 DATA GR,27,GT,28,HN,28,HR,21,HU,28,IE,22,IL,23,IR,26,IS,26,IT,27,JO,30,KM,27,KW,30,KZ,20,LB,28,LI,21,LT,20,LU,20,LV,21,MA,28,MC,27,MD,24,ME,22,MG,27,MK,19,ML,28,MR,27,MT,31,MU,30,MZ,25,NE,28,NI,32,NL,18,NO,15,PK,24,PL,28,PS,29,PT,25
670 DATA QA,29,RO,24,RS,22,SA,24,SE,24,SI,19,SK,24,SM,27,SN,28,TD,27,TG,28,TL,23,TN,24,TR,26,UA,29,VG,24,XK,20
```



## J


```J
NB. delete any blank characters
delblk =. #~ ' '&~:
NB. rearrange
rot =. '00' ,~ 2&}. @: (2&|.)
NB. characters -> "digits"
dig =. a. {~ (a.i.'0')+i.10
dig =. dig,a. {~ (a.i.'A')+i.26
todig =. dig&i.
coded =. [: ". 'x' ,~ delblk @: ": @: todig

NB. calculate check sum
cs =: 98 - 97 | coded @: rot @: delblk f.

NB. check sum as text
cstxt =. _2{. '0', [: ": cs
NB. replace first two characters
chgps =. [,2}.]
NB. shift country code
rotlc =. 2&|.
NB. insert check digits (position 3 and 4)
insertps =. chgps &.rotlc

NB. IBAN with newly calculated check digits
ibancd =: (cstxt insertps ]) f.

NB. check / generate check digits
ibancheck =: ] (]`('ok'"_) @. -:) ibancd

NB. groups of four characters
insertblk =. #~ # $ 1 1 1 1j1"_
quads =: insertblk @: delblk f.

NB. IBAN
iban =: quads @: ibancheck

```
```txt

   iban 'GB82 WEST 1234 5698 7654 32'
ok
   iban 'GB99 WEST 1234 5698 7654 32'
GB82 WEST 1234 5698 7654 32
   iban 'GB?? WEST 1234 5698 7654 32'
GB82 WEST 1234 5698 7654 32

   iban 'GB??WEST12345698765432'     NB. blank characters don't matter
GB82 WEST 1234 5698 7654 32

```



## Java

```java
import java.math.BigInteger;
import java.util.*;

public class IBAN {
    private static final String DEFSTRS = ""
            + "AL28 AD24 AT20 AZ28 BE16 BH22 BA20 BR29 BG22 "
            + "HR21 CY28 CZ24 DK18 DO28 EE20 FO18 FI18 FR27 GE22 DE22 GI23 "
            + "GL18 GT28 HU28 IS26 IE22 IL23 IT27 KZ20 KW30 LV21 LB28 LI21 "
            + "LT20 LU20 MK19 MT31 MR27 MU30 MC27 MD24 ME22 NL18 NO15 PK24 "
            + "PS29 PL28 PT25 RO24 SM27 SA24 RS22 SK24 SI19 ES24 SE24 CH21 "
            + "TN24 TR26 AE23 GB22 VG24 GR27 CR21";
    private static final Map<String, Integer> DEFINITIONS = new HashMap<>();

    static {
        for (String definition : DEFSTRS.split(" "))
            DEFINITIONS.put(definition.substring(0, 2), Integer.parseInt(definition.substring(2)));
    }

    public static void main(String[] args) {
        String[] ibans = {
                "GB82 WEST 1234 5698 7654 32",
                "GB82 TEST 1234 5698 7654 32",
                "GB81 WEST 1234 5698 7654 32",
                "SA03 8000 0000 6080 1016 7519",
                "CH93 0076 2011 6238 5295 7",
                "XX00 0000",
                "",
                "DE",
                "DE13 äöü_ 1234 1234 1234 12"};
        for (String iban : ibans)
            System.out.printf("%s is %s.%n", iban, validateIBAN(iban) ? "valid" : "not valid");
    }

    static boolean validateIBAN(String iban) {
        iban = iban.replaceAll("\\s", "").toUpperCase(Locale.ROOT);

        int len = iban.length();
        if (len < 4 || !iban.matches("[0-9A-Z]+") || DEFINITIONS.getOrDefault(iban.substring(0, 2), 0) != len)
            return false;

        iban = iban.substring(4) + iban.substring(0, 4);

        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < len; i++)
            sb.append(Character.digit(iban.charAt(i), 36));

        BigInteger bigInt = new BigInteger(sb.toString());

        return bigInt.mod(BigInteger.valueOf(97)).intValue() == 1;
    }
}
```
```txt
GB82 WEST 1234 5698 7654 32 is valid.
GB82 TEST 1234 5698 7654 32 is not valid.
GB81 WEST 1234 5698 7654 32 is not valid.
SA03 8000 0000 6080 1016 7519 is valid.
CH93 0076 2011 6238 5295 7 is valid.
XX00 0000 is not valid.
 is not valid.
DE is not valid.
DE13 äöü_ 1234 1234 1234 12 is not valid.
```



## JavaScript


```JavaScript
var ibanLen = {
	NO:15, BE:16, DK:18, FI:18, FO:18, GL:18, NL:18, MK:19,
	SI:19, AT:20, BA:20, EE:20, KZ:20, LT:20, LU:20, CR:21,
	CH:21, HR:21, LI:21, LV:21, BG:22, BH:22, DE:22, GB:22,
	GE:22, IE:22, ME:22, RS:22, AE:23, GI:23, IL:23, AD:24,
	CZ:24, ES:24, MD:24, PK:24, RO:24, SA:24, SE:24, SK:24,
	VG:24, TN:24, PT:25, IS:26, TR:26, FR:27, GR:27, IT:27,
	MC:27, MR:27, SM:27, AL:28, AZ:28, CY:28, DO:28, GT:28,
	HU:28, LB:28, PL:28, BR:29, PS:29, KW:30, MU:30, MT:31
}

function isValid(iban) {
	iban = iban.replace(/\s/g, '')
	if (!iban.match(/^[\dA-Z]+$/)) return false
	var len = iban.length
	if (len != ibanLen[iban.substr(0,2)]) return false
	iban = iban.substr(4) + iban.substr(0,4)
	for (var s='', i=0; i<len; i+=1) s+=parseInt(iban.charAt(i),36)
	for (var m=s.substr(0,15)%97, s=s.substr(15); s; s=s.substr(13)) m=(m+s.substr(0,13))%97
	return m == 1
}

document.write(isValid('GB82 WEST 1234 5698 7654 32'), '
') // true
document.write(isValid('GB82 WEST 1.34 5698 7654 32'), '
') // false
document.write(isValid('GB82 WEST 1234 5698 7654 325'), '
') // false
document.write(isValid('GB82 TEST 1234 5698 7654 32'), '
') // false
document.write(isValid('SA03 8000 0000 6080 1016 7519'), '
') // true

```

 true
 false
 false
 false
 true


## jq

This implementation requires a version of jq with <tt>gsub</tt>.

The heart of the matter consists of just four lines of straightforward jq code:
```jq

# strip the input string of spaces and tabs:
gsub("[ \t]";"")
# check the string is ALPHAnumeric
| test("^[A-Z0-9]+$")
  # check its length is as determined by the country code:
  and length == $lengths[.[0:2]]
  # check the mod 97 criterion:
  and ( (.[4:] + .[0:4]) | letters2digits | remainder) == 1

```

This conciseness is achieved courtesy of the helper functions: <tt>letters2digits</tt> and <tt>remainder</tt>.  These could be implemented as inner functions of the main function,
but for clarity they are shown as top-level functions here.

```jq
def letters2digits:
  65 as $A | 90 as $Z
  | ($A - 10) as $ten
  | explode
  | map( if $A <= . and . <= $Z
         then (. - $ten) | tostring
         else [.] | implode
         end )
  | join("");

# jq currently does not have unlimited-precision integer arithmetic
# and so we define a special-purpose "mod 97" filter:
# input: a string representing a decimal
# output: its remainder modulo 97 as a number
def remainder:
  if length < 15 then (.|tonumber) % 97
  else (.[0:14] | remainder | tostring) as $r1
       | ($r1 + .[14:]) | remainder
  end;

def is_valid_iban:
  {
    "AL": 28, "AD": 24, "AT": 20, "AZ": 28, "BE": 16, "BH": 22, "BA": 20, "BR": 29,
    "BG": 22, "CR": 21, "HR": 21, "CY": 28, "CZ": 24, "DK": 18, "DO": 28, "EE": 20,
    "FO": 18, "FI": 18, "FR": 27, "GE": 22, "DE": 22, "GI": 23, "GR": 27, "GL": 18,
    "GT": 28, "HU": 28, "IS": 26, "IE": 22, "IL": 23, "IT": 27, "KZ": 20, "KW": 30,
    "LV": 21, "LB": 28, "LI": 21, "LT": 20, "LU": 20, "MK": 19, "MT": 31, "MR": 27,
    "MU": 30, "MC": 27, "MD": 24, "ME": 22, "NL": 18, "NO": 15, "PK": 24, "PS": 29,
    "PL": 28, "PT": 25, "RO": 24, "SM": 27, "SA": 24, "RS": 22, "SK": 24, "SI": 19,
    "ES": 24, "SE": 24, "CH": 21, "TN": 24, "TR": 26, "AE": 23, "GB": 22, "VG": 24
  } as $lengths
  # Ignore spaces and tabs, and check input is ALPHAnumeric:
  | gsub("[ \t]";"")
  | test("^[A-Z0-9]+$")
      # Validate country code against expected length:
      and length == $lengths[.[0:2]]
      # Shift and convert:
      and ( (.[4:] + .[0:4]) | letters2digits | remainder) == 1 ;
```
Examples
```jq

"GB82 WEST 1234 5698 7654 32" | is_valid_iban #=> true
"GB82 TEST 1234 5698 7654 32" | is_valid_iban #=> false
```



## Jsish

From the Javascript entry.

```javascript
var ibanLen = {
    NO:15, BE:16, DK:18, FI:18, FO:18, GL:18, NL:18, MK:19,
    SI:19, AT:20, BA:20, EE:20, KZ:20, LT:20, LU:20, CR:21,
    CH:21, HR:21, LI:21, LV:21, BG:22, BH:22, DE:22, GB:22,
    GE:22, IE:22, ME:22, RS:22, AE:23, GI:23, IL:23, AD:24,
    CZ:24, ES:24, MD:24, PK:24, RO:24, SA:24, SE:24, SK:24,
    VG:24, TN:24, PT:25, IS:26, TR:26, FR:27, GR:27, IT:27,
    MC:27, MR:27, SM:27, AL:28, AZ:28, CY:28, DO:28, GT:28,
    HU:28, LB:28, PL:28, BR:29, PS:29, KW:30, MU:30, MT:31
};

function isIBAN(iban) {
    var i,m,s;
    iban = iban.replace(/\s/g, '');
    if (!iban.match(/^[0-9A-Z]+$/)) return false;
    var len = iban.length;
    if (len != ibanLen[iban.substr(0,2)]) return false;
    iban = iban.substr(4) + iban.substr(0,4);
    for (s = '', i = 0; i < len; i += 1) s += parseInt(iban.charAt(i),36);
    for (m = s.substr(0, 15) % 97, s = s.substr(15); s; s = s.substr(13)) m = (m + s.substr(0,13)) % 97;
    return m == 1;
}

;isIBAN('GB82 WEST 1234 5698 7654 32');
;isIBAN('GB82 WEST 1.34 5698 7654 32');
;isIBAN('GB82 WEST 1234 5698 7654 325');
;isIBAN('GB82 TEST 1234 5698 7654 32');
;isIBAN('SA03 8000 0000 6080 1016 7519');

/*
=!EXPECTSTART!=
isIBAN('GB82 WEST 1234 5698 7654 32') ==> true
isIBAN('GB82 WEST 1.34 5698 7654 32') ==> false
isIBAN('GB82 WEST 1234 5698 7654 325') ==> false
isIBAN('GB82 TEST 1234 5698 7654 32') ==> false
isIBAN('SA03 8000 0000 6080 1016 7519') ==> true
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -u IBAN.jsi
[PASS] IBAN.jsi
```



## Julia

```julia
function validiban(iban::AbstractString)
    country2length = Dict(
        "AL" => 28, "AD" => 24, "AT" => 20, "AZ" => 28, "BE" => 16, "BH" => 22, "BA" => 20, "BR" => 29,
        "BG" => 22, "CR" => 21, "HR" => 21, "CY" => 28, "CZ" => 24, "DK" => 18, "DO" => 28, "EE" => 20,
        "FO" => 18, "FI" => 18, "FR" => 27, "GE" => 22, "DE" => 22, "GI" => 23, "GR" => 27, "GL" => 18,
        "GT" => 28, "HU" => 28, "IS" => 26, "IE" => 22, "IL" => 23, "IT" => 27, "KZ" => 20, "KW" => 30,
        "LV" => 21, "LB" => 28, "LI" => 21, "LT" => 20, "LU" => 20, "MK" => 19, "MT" => 31, "MR" => 27,
        "MU" => 30, "MC" => 27, "MD" => 24, "ME" => 22, "NL" => 18, "NO" => 15, "PK" => 24, "PS" => 29,
        "PL" => 28, "PT" => 25, "RO" => 24, "SM" => 27, "SA" => 24, "RS" => 22, "SK" => 24, "SI" => 19,
        "ES" => 24, "SE" => 24, "CH" => 21, "TN" => 24, "TR" => 26, "AE" => 23, "GB" => 22, "VG" => 24)

    # Ensure upper alphanumeric input.
    iban = replace(iban, r"\s", "")

    rst = ismatch(r"^[\dA-Z]+$", iban)
    # Validate country code against expected length.
    rst = rst && length(iban) == country2length[iban[1:2]]
    # Shift and convert.
    iban = iban[5:end] * iban[1:4]
    digs = parse(BigInt, join(parse(Int, ch, 36) for ch in iban))
    return rst && digs % 97 == 1
end
```


```txt
validiban("GB82 WEST 1234 5698 7654 32") = true
validiban("GB82 TEST 1234 5698 7654 32") = false
```



## Kotlin


```scala
// version 1.0.6

import java.math.BigInteger

object Iban {
    /* List updated to release 73, January 2017, of IBAN Registry (75 countries) */
    private val countryCodes =
        "AD24 AE23 AL28 AT20 AZ28 BA20 BE16 BG22 BH22 BR29 BY28 CH21 CR22 CY28 CZ24 DE22 " +
        "DK18 DO28 EE20 ES24 FI18 FO18 FR27 GB22 GE22 GI23 GL18 GR27 GT28 HR21 HU28 IE22 " +
        "IL23 IQ23 IS26 IT27 JO30 KW30 KZ20 LB28 LC32 LI21 LT20 LU20 LV21 MC27 MD24 ME22 " +
        "MK19 MR27 MT31 MU30 NL18 NO15 PK24 PL28 PS29 PT25 QA29 RO24 RS22 SA24 SC31 SE24 " +
        "SI19 SK24 SM27 ST25 SV28 TL23 TN24 TR26 UA29 VG24 XK20"

    private fun checkCountryCode(cc: String) = cc in countryCodes

    fun validate(iban: String): Boolean {
        // remove spaces from IBAN
        var s = iban.replace(" ", "")

        // check country code
        if (!checkCountryCode(s.substring(0, 2) + s.length)) return false

        // move first 4 characters to end
        s = s.substring(4) + s.substring(0, 4)

        // replace A to Z with numbers 10 To 35
        for (ch in 'A'..'Z') s = s.replace(ch.toString(), (ch - 55).toInt().toString())

        // check whether mod 97 calculation gives a remainder of 1
        return BigInteger(s) % BigInteger.valueOf(97L) == BigInteger.ONE
    }
}

fun main(args: Array<String>) {
    val ibans = arrayOf("GB82 WEST 1234 5698 7654 32", "GB82 TEST 1234 5698 7654 32")
    for (iban in ibans) {
         val isValid = Iban.validate(iban)
         println(iban + if(isValid) " may be valid" else " is not valid")
    }
}
```


```txt

GB82 WEST 1234 5698 7654 32 may be valid
GB82 TEST 1234 5698 7654 32 is not valid

```



## Logtalk


```logtalk

:- object(iban).

	:- info([
		version is 0.1,
		author is 'Paulo Moura',
		date is 2015/10/11,
		comment is 'IBAN validation example using DCG rules.'
	]).

	:- public(valid/1).

	valid(IBAN) :-
		phrase(iban, IBAN), !.

	iban -->
		country_code(Code), check_digits(Check), bban(BBAN),
		{(BBAN*1000000 + Code*100 + Check) mod 97 =:= 1}.

	country_code(Code) -->
		letter_digits(L1, D3, D2), letter_digits(L0, D1, D0),
		{country_code([L1, L0]), Code is D3*1000 + D2*100 + D1*10 + D0}.

	check_digits(Check) -->
		digit(D1), digit(D0),
		{Check is D1*10 + D0}.

	bban(BBAN) -->
		bban_codes(Digits),
		{digits_to_integer(Digits, BBAN, Count), Count =< 30}.

	bban_codes(Ds) -->
		" ", bban_codes(Ds).
	bban_codes([D| Ds]) -->
		digit(D), bban_codes(Ds).
	bban_codes([D1, D0| Ds]) -->
		letter_digits(_, D1, D0), bban_codes(Ds).
	bban_codes([]) -->
		[].

	digit(D) -->
		[C],
		{0'0 =< C, C =< 0'9, D is C - 0'0}.

	letter_digits(C, D1, D0) -->
		[C],
		{	(	0'A =< C, C =< 0'Z ->
				D is C - 0'A + 10
			;	0'a =< C, C =< 0'z,
				D is C - 0'a + 10
			),
			D1 is D div 10,
			D0 is D mod 10
		}.

	digits_to_integer(Digits, BBAN, Count) :-
		digits_to_integer(Digits, 0, BBAN, 0, Count).

	digits_to_integer([], BBAN, BBAN, Count, Count).
	digits_to_integer([Digit| Digits], BBAN0, BBAN, Count0, Count) :-
		BBAN1 is BBAN0 * 10 + Digit,
		Count1 is Count0 + 1,
		digits_to_integer(Digits, BBAN1, BBAN, Count1, Count).

	country_code("AL").
	country_code("AD").
	country_code("AT").
	country_code("AZ").
	country_code("BE").
	country_code("BH").
	country_code("BA").
	country_code("BR").
	country_code("BG").
	country_code("CR").
	country_code("HR").
	country_code("CY").
	country_code("CZ").
	country_code("DK").
	country_code("DO").
	country_code("EE").
	country_code("FO").
	country_code("FI").
	country_code("FR").
	country_code("GE").
	country_code("DE").
	country_code("GI").
	country_code("GR").
	country_code("GL").
	country_code("GT").
	country_code("HU").
	country_code("IS").
	country_code("IE").
	country_code("IL").
	country_code("IT").
	country_code("KZ").
	country_code("KW").
	country_code("LV").
	country_code("LB").
	country_code("LI").
	country_code("LT").
	country_code("LU").
	country_code("MK").
	country_code("MT").
	country_code("MR").
	country_code("MU").
	country_code("MC").
	country_code("MD").
	country_code("ME").
	country_code("NL").
	country_code("NO").
	country_code("PK").
	country_code("PS").
	country_code("PL").
	country_code("PT").
	country_code("RO").
	country_code("SM").
	country_code("SA").
	country_code("RS").
	country_code("SK").
	country_code("SI").
	country_code("ES").
	country_code("SE").
	country_code("CH").
	country_code("TN").
	country_code("TR").
	country_code("AE").
	country_code("GB").
	country_code("VG").

:- end_object.

```

Testing:

```logtalk

| ?- iban::valid("GB82 WEST 1234 5698 7654 32").
yes

```



## Lua


```lua
local length=
{
  AL=28, AD=24, AT=20, AZ=28, BH=22, BE=16, BA=20, BR=29, BG=22, CR=21,
  HR=21, CY=28, CZ=24, DK=18, DO=28, EE=20, FO=18, FI=18, FR=27, GE=22,
  DE=22, GI=23, GR=27, GL=18, GT=28, HU=28, IS=26, IE=22, IL=23, IT=27,
  JO=30, KZ=20, KW=30, LV=21, LB=28, LI=21, LT=20, LU=20, MK=19, MT=31,
  MR=27, MU=30, MC=27, MD=24, ME=22, NL=18, NO=15, PK=24, PS=29, PL=28,
  PT=25, QA=29, RO=24, SM=27, SA=24, RS=22, SK=24, SI=19, ES=24, SE=24,
  CH=21, TN=24, TR=26, AE=23, GB=22, VG=24
}

function validate(iban)
  iban=iban:gsub("%s","")
  local l=length[iban:sub(1,2)]
  if not l or l~=#iban or iban:match("[^%d%u]") then
    return false -- invalid character, country code or length
  end
  local mod=0
  local rotated=iban:sub(5)..iban:sub(1,4)
  for c in rotated:gmatch(".") do
    mod=(mod..tonumber(c,36)) % 97
  end
  return mod==1
end
```



## M2000 Interpreter

We make a lambda function which return a string, with the input IBAN plus (Valid) or (Invalid) at the end.


```M2000 Interpreter

\\ IBAN checker
Function MakeIBANfun$ {
      Inventory countrylength = "AL" := 28, "AD" := 24, "AT" := 20, "AZ" := 28, "BE" := 16, "BH" := 22, "BA" := 20, "BR" := 29
      Append  countrylength, "BG" := 22, "CR" := 21, "HR" := 21, "CY" := 28, "CZ" := 24, "DK" := 18, "DO" := 28, "EE" := 20
      Append  countrylength, "FO" := 18, "FI" := 18, "FR" := 27, "GE" := 22, "DE" := 22, "GI" := 23, "GR" := 27, "GL" := 18
      Append  countrylength, "GT" := 28, "HU" := 28, "IS" := 26, "IE" := 22, "IL" := 23, "IT" := 27, "KZ" := 20, "KW" := 30
      Append  countrylength, "LV" := 21, "LB" := 28, "LI" := 21, "LT" := 20, "LU" := 20, "MK" := 19, "MT" := 31, "MR" := 27
      Append  countrylength, "MU" := 30, "MC" := 27, "MD" := 24, "ME" := 22, "NL" := 18, "NO" := 15, "PK" := 24, "PS" := 29
      Append  countrylength, "PL" := 28, "PT" := 25, "RO" := 24, "SM" := 27, "SA" := 24, "RS" := 22, "SK" := 24, "SI" := 19
      Append  countrylength, "ES" := 24, "SE" := 24, "CH" := 21, "TN" := 24, "TR" := 26, "AE" := 23, "GB" := 22, "VG" := 24

     =Lambda$ countrylength (Iban0$)->{
            Iban$=Filter$(Ucase$(Iban0$), " ")
            Iban$=Filter$(Iban$, Filter$(Iban$,"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
            Def Decimal ch, c
            {
                  If Not Exist(countrylength, Left$(Iban$,2)) Then Exit
                  length=Eval(countrylength)
                  If Not Len(Iban$)=length Then exit
                  Buffer ScanChar as Integer*length
                  Return ScanChar, 0:=Mid$(Iban$,5), length-4:=Mid$(Iban$,1,4)

                  For i=0 to length-1 {
                        ch=Eval(ScanChar, i)
                        if ch>=48 and ch<=57 then {
                              c = c*10+ch-48
                        } else.if ch>=65 and ch<=90 then {
                              c = c*100+ch-55
                        } else c=-1: exit
                  }
                  c = c mod 97
            }
            =Iban0$ + If$(c=1 ->" (Valid)", " (Invalid)")
      }
}
IbanCheck$=MakeIBANfun$()
Print IbanCheck$("GB82 WEST 1234 5698 7654 32")    ' valid
Print IbanCheck$("GB82 TEST 1234 5698 7654 32")
Print IbanCheck$("SA03 8000 0000 6080 1016 7519")   ' valid
Print IbanCheck$("GR16 0110 1250 0000 0001 2300 695X")
Print IbanCheck$("MK11 2222 3333 4444 555")

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
CountryCodes={{"AL",28},{"AD",24},{"AT",20},{"AZ",28},{"BE",16},{"BH",22},{"BA",20},{"BR",29},{"BG",22},{"CR",21},{"HR",21},{"CY",28},{"CZ",24},{"DK",18},{"DO",28},{"EE",20},{"FO",18},{"FI",18},{"FR",27},{"GE",22},{"DE",22},{"GI",23},{"GR",27},{"GL",18},{"GT",28},{"HU",28},{"IS",26},{"IE",22},{"IL",23},{"IT",27},{"KZ",20},{"KW",30},{"LV",21},{"LB",28},{"LI",21},{"LT",20},{"LU",20},{"MK",19},{"MT",31},{"MR",27},{"MU",30},{"MC",27},{"MD",24},{"ME",22},{"NL",18},{"NO",15},{"PK",24},{"PS",29},{"PL",28},{"PT",25},{"RO",24},{"SM",27},{"SA",24},{"RS",22},{"SK",24},{"SI",19},{"ES",24},{"SE",24},{"CH",21},{"TN",24},{"TR",26},{"AE",23},{"GB",22},{"VG",24}};
ClearAll[IBANVerify]
IBANVerify[input_String]:=Module[{i,cc,rules},
 i=StringReplace[StringTrim[input],{" "->"","\t"->""}];
 cc=StringTake[i,2];
 If[MemberQ[CountryCodes[[All,1]],cc]
 ,
  cc=Select[CountryCodes,First[#]==cc&][[1,2]];
  If[cc==StringLength[i]
  ,
   i=StringRotateLeft[i,4];
   i=Characters[ToUpperCase[i]];
   rules=Rule@@@({CharacterRange["A","Z"],Range[10,35]}\[Transpose]);
   i=i/.rules;
   i=ToExpression/@i;
   i=FromDigits[Flatten[IntegerDigits/@i]];
   If[Mod[i,97]===1
   ,
    True
   ,
    False
   ]
  ,
   False
  ]
 ,
  False
 ]
]
```

Trying out the function:

```Mathematica
IBANVerify["GB82 WEST 1234 5698 7654 32"]
IBANVerify["GB82 WEST 1234 5698 7654 323"]
IBANVerify["GB82 WEST 1234 5698 7654 31"]
IBANVerify["XX82 WEST 1234 5698 7654 323"]
```
 True
 False
 False
 False


## MATLAB

Didn't check country codes, lengths, or consistent checksums to keep this applicable to any new countries.

```MATLAB
function valid = validateIBAN(iban)
% Determine if International Bank Account Number is valid IAW ISO 13616
% iban - string containing account number
    if length(iban) < 5
        valid = false;
    else
        iban(iban == ' ') = '';                     % Remove spaces
        iban = lower([iban(5:end) iban(1:4)])+0;	% Rearrange and convert
        iban(iban > 96 & iban < 123) = iban(iban > 96 & iban < 123)-87; % Letters
        iban(iban > 47 & iban < 58) = iban(iban > 47 & iban < 58)-48;   % Numbers
        valid = piecewiseMod97(iban) == 1;
    end
end

function result = piecewiseMod97(x)
% Conduct a piecewise version of mod(x, 97) to support large integers
% x is a vector of integers
    x = sprintf('%d', x);	% Get to single-digits per index
    nDig = length(x);
    i1 = 1;
    i2 = min(9, nDig);
    prefix = '';
    while i1 <= nDig
        y = str2double([prefix x(i1:i2)]);
        result = mod(y, 97);
        prefix = sprintf('%d', result);
        i1 = i2+1;
        i2 = min(i1+8, nDig);
    end
end
```
Usage:

```MATLAB
tests = {'GB82 WEST 1234 5698 7654 32' ;
'GB82 TEST 1234 5698 7654 32' ;
'CH93 0076 2011 6238 5295 7' ;
'SA03 8000 0000 6080 1016 7519' ;
'SA03 1234 5678 9101 1121 3141' ;
'GB29 NWBK 6016 1331 9268 19' ;
'GB29' ;
'GR16 0110 1250 0000 0001 2300 695'};
for k = 1:length(tests)
fprintf('%s -> %svalid\n', tests{k}, char(~validateIBAN(tests{k}).*'in'))
end
```
```txt
GB82 WEST 1234 5698 7654 32 -> valid
GB82 TEST 1234 5698 7654 32 -> invalid
CH93 0076 2011 6238 5295 7 -> valid
SA03 8000 0000 6080 1016 7519 -> valid
SA03 1234 5678 9101 1121 3141 -> invalid
GB29 NWBK 6016 1331 9268 19 -> valid
GB29 -> invalid
GR16 0110 1250 0000 0001 2300 695 -> valid
```



## NewLISP


```NewLISP

(setq *iban-code-length* '((15  ("NO"))
                             (16  ("BE"))
                             (18  ("DK" "FO" "FI" "GL" "NL"))
                             (19  ("MK" "SI"))
                             (20  ("AT" "BA" "EE" "KZ" "LT" "LU"))
                             (21  ("CR" "HR" "LV" "LI" "CH"))
                             (22  ("BH" "BG" "GE" "DE" "IE" "ME" "RS" "GB"))
                             (23  ("GI" "IL" "AE"))
                             (24  ("AD" "CZ" "MD" "PK" "RO" "SA" "SK" "ES" "SE" "TN" "VG"))
                             (25  ("PT"))
                             (26  ("IS" "TR"))
                             (27  ("FR" "GR" "IT" "MR" "MC" "SM"))
                             (28  ("AL" "AZ" "CY" "DO" "GT" "HU" "LB" "PL"))
                             (29  ("BR" "PS"))
                             (30  ("KW" "MU"))
                             (31  ("MT"))))


;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; Remove spaces and set upper case.
(define (sanitize-iban iban)
   (upper-case (replace " " iban ""))
)

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; Check that only A-Z and 0-9 are used.
(define (valid-chars? iban)
	(setq rx (string "[A-Z0-9]{" (length iban) "}" ))
	(regex rx iban 1)
)

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; Check that the length is correct for the country.
(define (valid-length? iban)
	(setq countries-found (lookup (int (length iban)) *iban-code-length*))
	(if (not (nil? countries-found))
		(member (0 2 iban) countries-found)
	)
)

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; Convert the IBAN to integer following the rules from Wikipedia.
(define (iban-to-integer iban)
    (setq country-code (0 2 iban))
    (setq checksum (2 2 iban))
    (setq iban (string (4 iban) country-code))
    (setq iban (join (map (lambda (x) (if (regex "[0-9]" x) x (string (- (char x) 55)))) (explode iban))))
    (bigint (string iban checksum))
)

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

;; Test if IBAN is correct (true) or not (nil):
;; (valid-iban? "GB82 WEST 1234 5698 7654 32") ==> true
;; (valid-iban? "GB82 TEST 1234 5698 7654 32") ==> nil
(define (valid-iban? iban)
    (setq iban (sanitize-iban iban))
    (and
        (valid-chars? iban)
        (valid-length? iban)
        (= 1L (% (iban-to-integer iban) 97))
    )
)

```

Output:

```txt

(valid-iban? "GB82 WEST 1234 5698 7654 32")
true

(valid-iban? "GB82 TEST 1234 5698 7654 32")
nil

```



## Nim

```nim
import tables, strutils, re, bigints

let countryLen = toTable({
  "AL": 28, "AD": 24, "AT": 20, "AZ": 28, "BE": 16, "BH": 22, "BA": 20, "BR": 29,
  "BG": 22, "CR": 21, "HR": 21, "CY": 28, "CZ": 24, "DK": 18, "DO": 28, "EE": 20,
  "FO": 18, "FI": 18, "FR": 27, "GE": 22, "DE": 22, "GI": 23, "GR": 27, "GL": 18,
  "GT": 28, "HU": 28, "IS": 26, "IE": 22, "IL": 23, "IT": 27, "KZ": 20, "KW": 30,
  "LV": 21, "LB": 28, "LI": 21, "LT": 20, "LU": 20, "MK": 19, "MT": 31, "MR": 27,
  "MU": 30, "MC": 27, "MD": 24, "ME": 22, "NL": 18, "NO": 15, "PK": 24, "PS": 29,
  "PL": 28, "PT": 25, "RO": 24, "SM": 27, "SA": 24, "RS": 22, "SK": 24, "SI": 19,
  "ES": 24, "SE": 24, "CH": 21, "TN": 24, "TR": 26, "AE": 23, "GB": 22, "VG": 24})

proc validIban(iban: string): bool =
  # Ensure upper alphanumeric input
  var iban = iban.replace(" ","").replace("\t","")
  if not iban.match(re"^[\dA-Z]+$"):
    return false

  # Validate country code against expected length
  if iban.len != countryLen[iban[0..1]]:
    return false

  # Shift and convert
  iban = iban[4..iban.high] & iban[0..3]
  var digits = ""
  for ch in iban:
    case ch
      of '0'..'9': digits.add($(ch.ord - '0'.ord))
      of 'A'..'Z': digits.add($(ch.ord - 'A'.ord + 10))
      else: discard
  result = initBigInt(digits) mod 97 == 1

for account in ["GB82 WEST 1234 5698 7654 32", "GB82 TEST 1234 5698 7654 32"]:
  echo account, " validation is: ", validIban account
```
 GB82 WEST 1234 5698 7654 32 validation is: true
 GB82 TEST 1234 5698 7654 32 validation is: false

=={{header|Oberon-2}}==
Works with oo2c Version 2

```oberon2

MODULE IBAN;
IMPORT
  Out,
  Err,
  ADT:Dictionary,
  Object:Boxed,
  Object:BigInt,
  Object,
  Strings,
  IntStr;
TYPE
  IBANLen = Boxed.LongInt;
VAR
  nations: Dictionary.Dictionary(STRING,IBANLen);

  PROCEDURE Check*(iban: ARRAY OF CHAR): BOOLEAN;
  VAR
    country,ibanStr: Object.String;
    nLetter: ARRAY 3 OF CHAR;
    block: ARRAY 5 OF CHAR;
    numIban: ARRAY 256 OF CHAR;
    num,den,res: BigInt.BigInt;
    ibanLen: IBANLen;
    i: LONGINT;
  BEGIN
    ibanStr := Object.NewLatin1(iban);
    country := ibanStr.Substring(0,2);
    IF ~nations.HasKey(country) THEN
      Err.Object("Country " + country + " has not IBAN codes. ");
      RETURN FALSE;
    END;
    ibanLen := nations.Get(country);

    IF SHORT(ibanLen.value) # Strings.Length(iban) THEN
      Err.Object("IBAN length incorrect for " + country +". ");
      RETURN FALSE
    END;

    block[0] := 0X;
    Strings.Extract(iban,0,4,block);
    Strings.Delete(iban,0,4);Strings.Append(block,iban);

    numIban[0] := 0X;
    FOR i := 0 TO LEN(iban) - 1 DO
      nLetter[0] := 0X;
      IF (iban[i] >= 'A') & (iban[i] <= 'Z') THEN
        IntStr.IntToStr(ORD(iban[i]) - ORD('A') + 10,nLetter);
      ELSE
        nLetter[0] := iban[i];
        nLetter[1] := 0X
      END;
      Strings.Append(nLetter,numIban);
    END;
    Strings.Append(0X,numIban);

    num := BigInt.New(Object.NewLatin1(numIban),10);
    den := BigInt.New("97",10);
    res := num.Mod(den);
    IF res.Equals(BigInt.one) THEN
      RETURN TRUE
    ELSE
      Err.String("IBAN code check failed. ");
      RETURN FALSE
    END
  END Check;

  PROCEDURE CodeLengthFor*(country: ARRAY OF CHAR): LONGINT;
  VAR
    countryStr: Object.String;
    ibanLen: IBANLen;
  BEGIN
    countryStr := Object.NewLatin1(country);
    ibanLen := Boxed.zeroLongInt;
    IF nations.HasKey(countryStr) THEN
      ibanLen := nations.Get(countryStr)
    END;
    RETURN ibanLen.value
  END CodeLengthFor;

  PROCEDURE Test*;
    PROCEDURE DoCheck(iban: ARRAY OF CHAR);
    BEGIN
      Out.String("IBAN[");Out.String(iban);Out.String("]=");Out.Bool(Check(iban));
      Out.Ln
    END DoCheck;
  BEGIN
    DoCheck("CH9300762011623852957");
    DoCheck("GB82WEST12345698765432");
    DoCheck("SA0380000000608010167519");
    DoCheck("XX0380000000608010167519");
  END Test;

BEGIN
  nations := NEW(Dictionary.Dictionary(STRING,Boxed.LongInt));
  nations.Set("AL",NEW(IBANLen,28));
  nations.Set("AD",NEW(IBANLen,24));
  nations.Set("AT",NEW(IBANLen,20));
  nations.Set("AZ",NEW(IBANLen,28));
  nations.Set("BE",NEW(IBANLen,16));
  nations.Set("BH",NEW(IBANLen,22));
  nations.Set("BA",NEW(IBANLen,20));
  nations.Set("BR",NEW(IBANLen,29));
  nations.Set("BG",NEW(IBANLen,22));
  nations.Set("CR",NEW(IBANLen,21));
  nations.Set("HR",NEW(IBANLen,21));
  nations.Set("CY",NEW(IBANLen,28));
  nations.Set("CZ",NEW(IBANLen,24));
  nations.Set("DK",NEW(IBANLen,18));
  nations.Set("DO",NEW(IBANLen,28));
  nations.Set("EE",NEW(IBANLen,20));
  nations.Set("FO",NEW(IBANLen,18));
  nations.Set("FI",NEW(IBANLen,18));
  nations.Set("FR",NEW(IBANLen,27));
  nations.Set("GE",NEW(IBANLen,22));
  nations.Set("DE",NEW(IBANLen,22));
  nations.Set("GI",NEW(IBANLen,23));
  nations.Set("GR",NEW(IBANLen,27));
  nations.Set("GL",NEW(IBANLen,18));
  nations.Set("GT",NEW(IBANLen,28));
  nations.Set("HU",NEW(IBANLen,28));
  nations.Set("IS",NEW(IBANLen,26));
  nations.Set("IE",NEW(IBANLen,22));
  nations.Set("IL",NEW(IBANLen,23));
  nations.Set("IT",NEW(IBANLen,27));
  nations.Set("KZ",NEW(IBANLen,20));
  nations.Set("KW",NEW(IBANLen,30));
  nations.Set("LV",NEW(IBANLen,21));
  nations.Set("LB",NEW(IBANLen,28));
  nations.Set("LI",NEW(IBANLen,21));
  nations.Set("LT",NEW(IBANLen,20));
  nations.Set("LU",NEW(IBANLen,20));
  nations.Set("MK",NEW(IBANLen,19));
  nations.Set("MT",NEW(IBANLen,31));
  nations.Set("MR",NEW(IBANLen,27));
  nations.Set("MU",NEW(IBANLen,30));
  nations.Set("MC",NEW(IBANLen,27));
  nations.Set("MD",NEW(IBANLen,24));
  nations.Set("ME",NEW(IBANLen,22));
  nations.Set("NL",NEW(IBANLen,18));
  nations.Set("NO",NEW(IBANLen,15));
  nations.Set("PK",NEW(IBANLen,24));
  nations.Set("PS",NEW(IBANLen,29));
  nations.Set("PL",NEW(IBANLen,28));
  nations.Set("PT",NEW(IBANLen,25));
  nations.Set("RO",NEW(IBANLen,24));
  nations.Set("SM",NEW(IBANLen,27));
  nations.Set("SA",NEW(IBANLen,24));
  nations.Set("RS",NEW(IBANLen,22));
  nations.Set("SK",NEW(IBANLen,24));
  nations.Set("SI",NEW(IBANLen,19));
  nations.Set("ES",NEW(IBANLen,24));
  nations.Set("SE",NEW(IBANLen,24));
  nations.Set("CH",NEW(IBANLen,21));
  nations.Set("TN",NEW(IBANLen,24));
  nations.Set("TR",NEW(IBANLen,26));
  nations.Set("AE",NEW(IBANLen,23));
  nations.Set("GB",NEW(IBANLen,22));
  nations.Set("VG",NEW(IBANLen,24));

  Test;
END IBAN.

```

Output:

```txt

IBAN[CH9300762011623852957]=TRUE
IBAN[GB82WEST12345698765432]=TRUE
IBAN[SA0380000000608010167519]=TRUE
IBAN[XX0380000000608010167519]=Country XX not has IBAN codes. FALSE

```




## OCaml

```ocaml

#load "str.cma"
#load "nums.cma"  (* for module Big_int *)


(* Countries and length of their IBAN. *)
(* Taken from https://en.wikipedia.org/wiki/International_Bank_Account_Number#IBAN_formats_by_country *)
let countries = [
  ("AL", 28); ("AD", 24); ("AT", 20); ("AZ", 28); ("BH", 22); ("BE", 16);
  ("BA", 20); ("BR", 29); ("BG", 22); ("CR", 21); ("HR", 21); ("CY", 28);
  ("CZ", 24); ("DK", 18); ("DO", 28); ("TL", 23); ("EE", 20); ("FO", 18);
  ("FI", 18); ("FR", 27); ("GE", 22); ("DE", 22); ("GI", 23); ("GR", 27);
  ("GL", 18); ("GT", 28); ("HU", 28); ("IS", 26); ("IE", 22); ("IL", 23);
  ("IT", 27); ("JO", 30); ("KZ", 20); ("XK", 20); ("KW", 30); ("LV", 21);
  ("LB", 28); ("LI", 21); ("LT", 20); ("LU", 20); ("MK", 19); ("MT", 31);
  ("MR", 27); ("MU", 30); ("MC", 27); ("MD", 24); ("ME", 22); ("NL", 18);
  ("NO", 15); ("PK", 24); ("PS", 29); ("PL", 28); ("PT", 25); ("QA", 29);
  ("RO", 24); ("SM", 27); ("SA", 24); ("RS", 22); ("SK", 24); ("SI", 19);
  ("ES", 24); ("SE", 24); ("CH", 21); ("TN", 24); ("TR", 26); ("AE", 23);
  ("GB", 22); ("VG", 24); ("DZ", 24); ("AO", 25); ("BJ", 28); ("BF", 27);
  ("BI", 16); ("CM", 27); ("CV", 25); ("IR", 26); ("CI", 28); ("MG", 27);
  ("ML", 28); ("MZ", 25); ("SN", 28); ("UA", 29)
]
(* Put the countries in a Hashtbl for faster search... *)
let tbl_countries =
  let htbl = Hashtbl.create (List.length countries) in
  let _ = List.iter (fun (k, v) -> Hashtbl.add htbl k v) countries in
  htbl


(* Delete spaces and put all letters in upper case. *)
let clean_iban iban =
  Str.global_replace (Str.regexp " +") "" iban
  |> String.uppercase_ascii


(* Each country has an IBAN with a specific length. *)
let check_length ib =
  let iso_length = List.hd countries |> fst |> String.length in
  let country_code = String.sub ib 0 iso_length in
  try
    Hashtbl.find tbl_countries country_code = String.length ib
  with
    Not_found -> false


(* Convert a string into a list of chars. *)
let charlist_of_string s =
  let l = String.length s in
  let rec doloop i =
    if i >= l then []
    else s.[i] :: doloop (i + 1)
  in
  doloop 0


(* Letters are associated to values: A=10, B=11, ..., Z=35. *)
let val_of_char c =
  match c with
  | '0' .. '9' -> int_of_char c - int_of_char '0'
  | 'A' .. 'Z' -> int_of_char c - int_of_char 'A' + 10
  | _ -> failwith (Printf.sprintf "Character not allowed: %c" c)


(* Compute the mod-97 value and check it is equal to 1. *)
let check_mod97 ib =
  let l = String.length ib
  and taken = 4 in
  let prefix = String.sub ib 0 taken
  and rest = String.sub ib taken (l - taken) in
  let newval = rest ^ prefix (* move the 4 initial characters to the end of the string *)
            |> charlist_of_string  (* convert the string into a list of chars *)
            |> List.map val_of_char  (* convert each char into its integer value *)
            |> List.map string_of_int  (* convert the integers into strings... *)
            |> List.fold_left (^) "" in  (* ...and concatenate said strings *)
  (* Now compute the mod-97 using the Big Integers provided by OCaml, and
   * compare the result to 1. *)
  Big_int.eq_big_int
    (Big_int.mod_big_int (Big_int.big_int_of_string newval)
                         (Big_int.big_int_of_int 97))
    (Big_int.big_int_of_int 1)


(* Do the validation as described in the Wikipedia article at
 * https://en.wikipedia.org/wiki/International_Bank_Account_Number#Validating_the_IBAN *)
let validate iban =
  let ib = clean_iban iban in
  check_length ib && check_mod97 ib


let () =
  let ibans = [
    ("GB82 WEST 1234 5698 7654 32", true);
    ("GB82 TEST 1234 5698 7654 32", false);
    ("GB81 WEST 1234 5698 7654 32", false);
    ("GB82 WEST 1234 5698 7654 3", false);
    ("SA03 8000 0000 6080 1016 7519", true);
    ("CH93 0076 2011 6238 5295 7", true);
    ("\"Completely incorrect iban\"", false)
  ] in
  let testit (ib, exp) =
    let res = validate ib in
    Printf.printf "%s is %svalid. Expected %b [%s]\n"
                  ib (if res then "" else "not ")
                  exp (if res = exp then "PASS" else "FAIL")
  in
  List.iter (fun pair -> testit pair) ibans

```

 GB82 WEST 1234 5698 7654 32 is valid. Expected true [PASS]
 GB82 TEST 1234 5698 7654 32 is not valid. Expected false [PASS]
 GB81 WEST 1234 5698 7654 32 is not valid. Expected false [PASS]
 GB82 WEST 1234 5698 7654 3 is not valid. Expected false [PASS]
 SA03 8000 0000 6080 1016 7519 is valid. Expected true [PASS]
 CH93 0076 2011 6238 5295 7 is valid. Expected true [PASS]
 "Completely incorrect iban" is not valid. Expected false [PASS]



## Perl


```Perl
#!/usr/bin/perl
use strict ;
use warnings ;
use Math::BigInt ;

my %countrycodelengths = ( "AL" =>  28, "AD" =>  24, "AT" =>  20, "AZ" =>  28,
                           "BE" =>  16, "BH" =>  22, "BA" =>  20, "BR" =>  29,
                           "BG" =>  22, "CR" =>  21, "HR" =>  21, "CY" =>  28,
			   "CZ" =>  24, "DK" =>  18, "DO" =>  28, "EE" =>  20,
	                   "FO" =>  18, "FI" =>  18, "FR" =>  27, "GE" =>  22,
	                   "DE" =>  22, "GI" =>  23, "GR" =>  27, "GL" =>  18,
	                   "GT" =>  28, "HU" =>  28, "IS" =>  26, "IE" =>  22,
		           "IL" =>  23, "IT" =>  27, "KZ" =>  20, "KW" =>  30,
		           "LV" =>  21, "LB" =>  28, "LI" =>  21, "LT" =>  20,
		           "LU" =>  20, "MK" =>  19, "MT" =>  31, "MR" =>  27,
		           "MU" =>  30, "MC" =>  27, "MD" =>  24, "ME" =>  22,
			   "NL" =>  18, "NO" =>  15, "PK" =>  24, "PS" =>  29,
			   "PL" =>  28, "PT" =>  25, "RO" =>  24, "SM" =>  27,
			   "SA" =>  24, "RS" =>  22, "SK" =>  24, "SI" =>  19,
			   "ES" =>  24, "SE" =>  24, "CH" =>  21, "TN" =>  24,
			   "TR" =>  26, "AE" =>  23, "GB" =>  22, "VG" =>  24 ) ;
sub validate_iban {
   my $ibanstring = shift ;
   $ibanstring =~ s/\s+//g ;
   return 0 unless $ibanstring =~ /[0-9a-zA-Z]+/ ;
   $ibanstring = uc $ibanstring ;
   return 0 if ( not exists $countrycodelengths{ substr( $ibanstring , 0 , 2 ) }  );
   return 0 if length ( $ibanstring ) != $countrycodelengths{ substr( $ibanstring , 0 , 2 ) } ;
   $ibanstring =~ s/(.{4})(.+)/$2$1/ ;
   $ibanstring =~ s/([A-Z])/ord( $1 ) - 55/eg ;
   my $number = Math::BigInt->new( $ibanstring ) ;
   if ( $number->bmod( 97 ) == 1 ) {
      return 1 ;
   }
   else {
      return 0 ;
   }
}

if ( validate_iban( "GB82 WEST 1234 5698 7654 32" ) ) {
   print "GB82 WEST 1234 5698 7654 32 is a valid IBAN number!\n" ;
}
else {
   print "Sorry! GB82 WEST 1234 5698 7654 32 is not valid!\n" ;
}
if ( validate_iban( "GB82TEST12345698765432" ) ) {
   print "GB82TEST12345698765432 is valid!\n" ;
}
```
 GB82 WEST 1234 5698 7654 32 is a valid IBAN number!
 GB82TEST12345698765432 is invalid!


## Perl 6


```perl6
subset IBAN of Str where sub ($_ is copy) {
    s:g/\s//;
    return False if m/<-[ 0..9 A..Z a..z ]>/ or .chars != <
        AD 24 AE 23 AL 28 AT 20 AZ 28 BA 20 BE 16 BG 22 BH 22 BR 29 CH 21
        CR 21 CY 28 CZ 24 DE 22 DK 18 DO 28 EE 20 ES 24 FI 18 FO 18 FR 27
        GB 22 GE 22 GI 23 GL 18 GR 27 GT 28 HR 21 HU 28 IE 22 IL 23 IS 26
        IT 27 KW 30 KZ 20 LB 28 LI 21 LT 20 LU 20 LV 21 MC 27 MD 24 ME 22
        MK 19 MR 27 MT 31 MU 30 NL 18 NO 15 PK 24 PL 28 PS 29 PT 25 RO 24
        RS 22 SA 24 SE 24 SI 19 SK 24 SM 27 TN 24 TR 26 VG 24
    >.hash{.substr(0,2).uc};

    s/(.**4)(.+)/$1$0/;
    return .subst(:g, /\D/, { :36(~$_) }) % 97 == 1;
}

say "$_ is {$_ ~~ IBAN ?? 'valid' !! 'invalid' }" for
'GB82 WEST 1234 5698 7654 32',
'gb82 west 1234 5698 7654 32',
'GB82 TEST 1234 5698 7654 32';
```
 GB82 WEST 1234 5698 7654 32 is valid.
 gb82 west 1234 5698 7654 32 is valid.
 GB82 TEST 1234 5698 7654 32 is invalid.


## Phix


```Phix
--
-- demo\rosetta\IBAN.exw
--
### ===============

--
constant nations = {{"AD",24},  -- Andorra
                    -- (full list in distro)
                    {"CH",21},  -- Switzerland
                    {"GB",22},  -- United Kingdom
                    {"SA",24},  -- Saudi Arabia
                    {"XK",20}}  -- Kosovo

constant {countries,lengths} = columnize(nations)

function iban(string code)
-- This routine does and should reject codes containing spaces etc.
-- Use iban_s() below for otherwise.
    integer country = find(code[1..2],countries)
    if country!=0
    and length(code)=lengths[country] then
        code = code[5..$]&code[1..4]
        integer c = 0
        for i=1 to length(code) do
            integer ch=code[i]
            if ch>='0' and ch<='9' then
                c = c*10+ch-'0'
            elsif ch>='A' and ch<='Z' then
                c = c*100+ch-55
            else
                return false
            end if
            c = mod(c,97)
        end for
        return c=1
    end if
    return false
end function

function iban_s(string code)
-- strips any embedded spaces and hyphens before validating.
    code = substitute_all(code,{" ","-"},{"",""})
    return iban(code)
end function

procedure test(string code, bool expected)
    bool valid = iban_s(code)
    string state
    if valid=expected then
        state = iff(valid?"ok":"invalid (as expected)")
    else
        state = iff(valid?"OK!!":"INVALID!!")&" (NOT AS EXPECTED)"
    end if
    printf(1,"%-34s :%s\n",{code,state})
end procedure

test("GB82 WEST 1234 5698 7654 32",true)
test("GB82 TEST 1234 5698 7654 32",false)
test("GB81 WEST 1234 5698 7654 32",false)
test("SA03 8000 0000 6080 1016 7519",true)
test("CH93 0076 2011 6238 5295 7",true)
```

```txt

GB82 WEST 1234 5698 7654 32        :ok
GB82 TEST 1234 5698 7654 32        :invalid (as expected)
GB81 WEST 1234 5698 7654 32        :invalid (as expected)
SA03 8000 0000 6080 1016 7519      :ok
CH93 0076 2011 6238 5295 7         :ok

```



## PHP


```php

<?php

function piece_wise($iban_all_digits) {

    $remainder = NULL;
    $slice = 9;

    for ($i=0; $i<strlen($iban_all_digits); $i=$i+$slice)
    {
        if ($i>0)
        {
            $slice = 7;
        }

        $part = $remainder . substr($iban_all_digits, $i, $slice);
        //echo "REMAINDER: " . $remainder . "
";
        //echo "PART: $part" . "
";
        $remainder = intval($part) % 97;
    }

return $remainder;

}


$iban = "GB82 WEST 1234 5698 7654 32";

//remove space
$iban = str_replace(' ', '', $iban);

//echo $iban; echo '
';
$iban_length = strlen($iban);
$country_code = substr($iban, 0, 2);

/*
    IBAN lengths are country specific
    full list available at
    https://en.wikipedia.org/wiki/International_Bank_Account_Number#IBAN_formats_by_country
*/
$lengths = ['GB' => 22];


if ($lengths[$country_code] != $iban_length)
{
    exit ("IBAN length not valid for $country_code");
}


// 2. move first four characters to the end
$iban = substr($iban, 4) . substr($iban, 0, 4);


//3. Replace letters in IBAN with digits
//(A=10, B=11 ... Z=35)

$iban_arr = str_split($iban, 1);


$iban_all_digits = '';

foreach ($iban_arr as $key=>$value)
{
    if (ctype_alpha($value))
    {
        $value = ord($value) - 55;
    }
    $iban_all_digits = $iban_all_digits . $value;
}


if (piece_wise($iban_all_digits) === 1)
{
    echo "VALID IBAN!";
}

else
{
    echo "IBAN NOT VALID";
}

```



## PicoLisp


```Picolisp
(setq *Sizes '((GB . 22) (CH . 21) (SA . 24)))

(de iban (Str)
   (let Lst
      (filter
         '((X) (not (sp? X)))
         (chop (uppc Str)) )
      (when
         (=
            (cdr (assoc (pack (head 2 Lst)) *Sizes))
            (length Lst) )
         (%
            (format
               (mapcar
                  '((X)
                     (if (upp? X)
                        (- (char X) 55)
                        X ) )
                  (append (nth Lst 5) (head 4 Lst)) ) )
            97 ) ) ) )

(for I '("sa03 8000 0000 6080 1016 7519"
         "CH9300762011623852957"
         "gb82west1234 56987654 32"
         "GB82WEST000")
   (if (= 1 (iban I))
      (println 'Valid)
      (println 'Invalid) ) )

(bye)
```



## PowerShell


```PowerShell

function verifIBAN ([string]$ibanS)
{
if ($ibanS.Length -ne 27) {return $false} else
{
$ibanI="$($ibanS.Substring(4,23))$($ibanS.Substring(0,4))".ToUpper()
[int]$comptIBAN=0
$NumIBAN=""
while ($comptIBAN -lt 27)
    {
    if ([byte]$ibanI[$comptIBAN] -ge 65 -and [byte]$ibanI[$comptIBAN] -le 90)
        {
        $NumIban+=([byte]$ibanI[$comptIBAN]-55)
        } #pour transformer les lettres en chiffres (A=10, B=11...)
        else
        {
        $NumIban+=$ibanI[$comptIBAN]
        }
    $comptIBAN++
    }
    #cela fait un nombre de 30 chiffres : trop pour powershell, je découpe donc les 9 premiers caractères :
if ("$($NumIBAN.Substring(0,9)%97)$($NumIBAN.Substring(9,$NumIBAN.Length-9))"%97 -eq 1)
    {return $true}
    else
    {return $false}
}
} #fin fonction vérification IBAN / Stéphane RABANY 2018

```


Ancien texte qui ne sert à rien selon moi :
I have heard that Regex should not be used with IBAN codes.  Regex does seem to work, however.

```PowerShell

@'
"Country","Length","Example"
"Albania",28,"AL47212110090000000235698741"
"Andorra",24,"AD1200012030200359100100"
"Austria",20,"AT611904300235473201"
"Belgium",16,"BE68539007547034"
"Bosnia and Herzegovina",20,"BA391290079401028494"
"Bulgaria",22,"BG80BNBG96611020345678"
"Croatia",21,"HR1210010051863000160"
"Cyprus",28,"CY17002001280000001200527600"
"Czech Republic",24,"CZ6508000000192000145399"
"Denmark",18,"DK5000400440116243"
"Estonia",20,"EE382200221020145685"
"Faroe Islands",18,"FO1464600009692713"
"Finland",18,"FI2112345600000785"
"France",27,"FR1420041010050500013M02606"
"Georgia",22,"GE29NB0000000101904917"
"Germany",22,"DE89370400440532013000"
"Gibraltar",23,"GI75NWBK000000007099453"
"Greece",27,"GR1601101250000000012300695"
"Greenland",18,"GL8964710001000206"
"Hungary",28,"HU42117730161111101800000000"
"Iceland",26,"IS140159260076545510730339"
"Ireland",22,"IE29AIBK93115212345678"
"Italy",27,"IT60X0542811101000000123456"
"Kosovo",20,"XK051212012345678906"
"Latvia",21,"LV80BANK0000435195001"
"Liechtenstein",21,"LI21088100002324013AA"
"Lithuania",20,"LT121000011101001000"
"Luxembourg",20,"LU280019400644750000"
"Macedonia",19,"MK07300000000042425"
"Malta",31,"MT84MALT011000012345MTLCAST001S"
"Moldova",24,"MD24AG000225100013104168"
"Monaco",27,"MC5813488000010051108001292"
"Montenegro",22,"ME25505000012345678951"
"Netherlands",18,"NL91ABNA0417164300"
"Norway",15,"NO9386011117947"
"Poland",28,"PL27114020040000300201355387"
"Portugal",25,"PT50000201231234567890154"
"Romania",24,"RO49AAAA1B31007593840000"
"San Marino",27,"SM86U0322509800000000270100"
"Serbia",22,"RS35260005601001611379"
"Slovakia",24,"SK3112000000198742637541"
"Slovenia",19,"SI56191000000123438"
"Spain",24,"ES9121000418450200051332"
"Sweden",24,"SE3550000000054910000003"
"Switzerland",21,"CH9300762011623852957"
"Ukraine",29,"UA573543470006762462054925026"
"United Kingdom",22,"GB29NWBK60161331926819"
'@ -split "`r`n" | Set-Content -Path .\IBAN.csv -Force

$ibans = foreach ($iban in Import-Csv -Path .\IBAN.csv)
{
    $iban | Select-Object -Property Country,
                                    @{Name='Code'  ; Expression={$iban.Example.Substring(0,2)}},
                                    @{Name='Length'; Expression={[int]$iban.Length}},
                                    Example
}

$ibans

```

```txt

Country                Code Length Example
-------                ---- ------ -------
Albania                AL       28 AL47212110090000000235698741
Andorra                AD       24 AD1200012030200359100100
Austria                AT       20 AT611904300235473201
Belgium                BE       16 BE68539007547034
Bosnia and Herzegovina BA       20 BA391290079401028494
Bulgaria               BG       22 BG80BNBG96611020345678
Croatia                HR       21 HR1210010051863000160
Cyprus                 CY       28 CY17002001280000001200527600
Czech Republic         CZ       24 CZ6508000000192000145399
Denmark                DK       18 DK5000400440116243
Estonia                EE       20 EE382200221020145685
Faroe Islands          FO       18 FO1464600009692713
Finland                FI       18 FI2112345600000785
France                 FR       27 FR1420041010050500013M02606
Georgia                GE       22 GE29NB0000000101904917
Germany                DE       22 DE89370400440532013000
Gibraltar              GI       23 GI75NWBK000000007099453
Greece                 GR       27 GR1601101250000000012300695
Greenland              GL       18 GL8964710001000206
Hungary                HU       28 HU42117730161111101800000000
Iceland                IS       26 IS140159260076545510730339
Ireland                IE       22 IE29AIBK93115212345678
Italy                  IT       27 IT60X0542811101000000123456
Kosovo                 XK       20 XK051212012345678906
Latvia                 LV       21 LV80BANK0000435195001
Liechtenstein          LI       21 LI21088100002324013AA
Lithuania              LT       20 LT121000011101001000
Luxembourg             LU       20 LU280019400644750000
Macedonia              MK       19 MK07300000000042425
Malta                  MT       31 MT84MALT011000012345MTLCAST001S
Moldova                MD       24 MD24AG000225100013104168
Monaco                 MC       27 MC5813488000010051108001292
Montenegro             ME       22 ME25505000012345678951
Netherlands            NL       18 NL91ABNA0417164300
Norway                 NO       15 NO9386011117947
Poland                 PL       28 PL27114020040000300201355387
Portugal               PT       25 PT50000201231234567890154
Romania                RO       24 RO49AAAA1B31007593840000
San Marino             SM       27 SM86U0322509800000000270100
Serbia                 RS       22 RS35260005601001611379
Slovakia               SK       24 SK3112000000198742637541
Slovenia               SI       19 SI56191000000123438
Spain                  ES       24 ES9121000418450200051332
Sweden                 SE       24 SE3550000000054910000003
Switzerland            CH       21 CH9300762011623852957
Ukraine                UA       29 UA573543470006762462054925026
United Kingdom         GB       22 GB29NWBK60161331926819

```


```PowerShell

$regex = [regex]'[a-zA-Z]{2}[0-9]{2}[a-zA-Z0-9]{6}[0-9]{5}([a-zA-Z0-9]?){0,16}'

foreach ($iban in $ibans)
{
    [PSCustomObject]@{
        Country = $iban.Country
        Example = $iban.Example
        IsValid = $regex.IsMatch($iban.Example)
    }
}

```

```txt

Country                Example                         IsValid
-------                -------                         -------
Albania                AL47212110090000000235698741       True
Andorra                AD1200012030200359100100           True
Austria                AT611904300235473201               True
Belgium                BE68539007547034                   True
Bosnia and Herzegovina BA391290079401028494               True
Bulgaria               BG80BNBG96611020345678             True
Croatia                HR1210010051863000160              True
Cyprus                 CY17002001280000001200527600       True
Czech Republic         CZ6508000000192000145399           True
Denmark                DK5000400440116243                 True
Estonia                EE382200221020145685               True
Faroe Islands          FO1464600009692713                 True
Finland                FI2112345600000785                 True
France                 FR1420041010050500013M02606        True
Georgia                GE29NB0000000101904917             True
Germany                DE89370400440532013000             True
Gibraltar              GI75NWBK000000007099453            True
Greece                 GR1601101250000000012300695        True
Greenland              GL8964710001000206                 True
Hungary                HU42117730161111101800000000       True
Iceland                IS140159260076545510730339         True
Ireland                IE29AIBK93115212345678             True
Italy                  IT60X0542811101000000123456        True
Kosovo                 XK051212012345678906               True
Latvia                 LV80BANK0000435195001              True
Liechtenstein          LI21088100002324013AA              True
Lithuania              LT121000011101001000               True
Luxembourg             LU280019400644750000               True
Macedonia              MK07300000000042425                True
Malta                  MT84MALT011000012345MTLCAST001S    True
Moldova                MD24AG000225100013104168           True
Monaco                 MC5813488000010051108001292        True
Montenegro             ME25505000012345678951             True
Netherlands            NL91ABNA0417164300                 True
Norway                 NO9386011117947                    True
Poland                 PL27114020040000300201355387       True
Portugal               PT50000201231234567890154          True
Romania                RO49AAAA1B31007593840000           True
San Marino             SM86U0322509800000000270100        True
Serbia                 RS35260005601001611379             True
Slovakia               SK3112000000198742637541           True
Slovenia               SI56191000000123438                True
Spain                  ES9121000418450200051332           True
Sweden                 SE3550000000054910000003           True
Switzerland            CH9300762011623852957              True
Ukraine                UA573543470006762462054925026      True
United Kingdom         GB29NWBK60161331926819             True

```



## PureBasic


```purebasic
EnableExplicit
Enumeration IBAN
  #IBAN_VAL
  #IBAN_SUM
  #IBAN_NOSPACE
  #IBAN_VAL_FORM
  #IBAN_SUM_FORM
EndEnumeration

NewMap CData.i()
Macro CCD(SIGN,LENGTH)
  CData(SIGN)=LENGTH
EndMacro

Procedure.s IBANForm(iban.s,form.i)
  Define fn.s, c.i
  fn=RemoveString(UCase(iban),Chr(32))
  If form=#IBAN_NOSPACE   :   ProcedureReturn fn  :   EndIf
  fn=Mid(fn,5)+Mid(fn,1,4)
  For c=65 To 90
    fn=ReplaceString(fn,Chr(c),Str(c-55))
  Next c
  If form=#IBAN_VAL_FORM  :   ProcedureReturn fn  :   EndIf
  fn=Left(fn,Len(fn)-2)+"00"
  If form=#IBAN_SUM_FORM  :   ProcedureReturn fn  :   EndIf
EndProcedure

Procedure.s m97iban(iban.s,calculate.i)
  Define i.i, part.s, rest.s
  Select calculate
    Case #IBAN_VAL : iban=IBANForm(iban,#IBAN_VAL_FORM)
    Case #IBAN_SUM : iban=IBANForm(iban,#IBAN_SUM_FORM)
  EndSelect
  For i=1 To Len(iban)  ; Validierung der Prüfsumme
    part+Mid(iban,i,1)
    If Val(rest+part)<97 : Continue : EndIf
    rest=Str((Val(rest+part)) %97)  : part=""
  Next
  Select calculate
    Case #IBAN_VAL : ProcedureReturn rest
    Case #IBAN_SUM : ProcedureReturn RSet(Str(98-Val(rest+part)),2,"0")
  EndSelect
EndProcedure

CCD("AL",28) : CCD("AD",24) : CCD("AT",20) : CCD("AZ",28) : CCD("BE",16) : CCD("BH",22) : CCD("BA",20)
CCD("BR",29) : CCD("BG",22) : CCD("CR",21) : CCD("HR",21) : CCD("CY",28) : CCD("CZ",24) : CCD("DK",18)
CCD("DO",28) : CCD("EE",20) : CCD("FO",18) : CCD("FI",18) : CCD("FR",27) : CCD("GE",22) : CCD("DE",22)
CCD("GI",23) : CCD("GR",27) : CCD("GL",18) : CCD("GT",28) : CCD("HU",28) : CCD("IS",26) : CCD("IE",22)
CCD("IL",23) : CCD("IT",27) : CCD("KZ",20) : CCD("KW",30) : CCD("LV",21) : CCD("LB",28) : CCD("LI",21)
CCD("LT",20) : CCD("LU",20) : CCD("MK",19) : CCD("MT",31) : CCD("MR",27) : CCD("MU",30) : CCD("MC",27)
CCD("MD",24) : CCD("ME",22) : CCD("NL",18) : CCD("NO",15) : CCD("PK",24) : CCD("PS",29) : CCD("PL",28)
CCD("PT",25) : CCD("RO",24) : CCD("SM",27) : CCD("SA",24) : CCD("RS",22) : CCD("SK",24) : CCD("SI",19)
CCD("ES",24) : CCD("SE",24) : CCD("CH",21) : CCD("TN",24) : CCD("TR",26) : CCD("AE",23) : CCD("GB",22)
CCD("VG",24)

DataSection
  IBANData:
  Data.s "GB82 WEST 1234 5698 7654 32"
  Data.s "GB82WEST12345698765432"
  Data.s "gb82 west 1234 5698 7654 32"
  Data.s "GB82 TEST 1234 5698 7654 32"
  Data.s "GR16 0110 1250 0000 0001 2300 695"
  Data.s "GB29 NWBK 6016 1331 9268 19"
  Data.s "SA03 8000 0000 6080 1016 7519"
  Data.s "CH93 0076 2011 6238 5295 7"
  Data.s "IL62 0108 0000 0009 9999 999"
  Data.s "IL62-0108-0000-0009-9999-999"
  Data.s "US12 3456 7890 0987 6543 210"
  Data.s "GR16 0110 1250 0000 0001 2300 695X"
  Data.s Chr(0)
EndDataSection

Define iban.s, cc.s
OpenConsole()
Restore IBANData
Repeat
  Read.s iban : If iban=Chr(0) : Break : EndIf
  Print("IBAN"+#TAB$+": "+LSet(iban,35,Chr(32))+#TAB$)
  cc=Left(IBANForm(iban,#IBAN_NOSPACE),2)
  If CData(cc)
    If Not CData()=Len(IBANForm(iban,#IBAN_NOSPACE)) : PrintN("[INCORRECT: LENGTH]") : Continue : EndIf
  Else
    PrintN("[INCORRECT: COUNTRY]") : Continue
  EndIf
  If Not Val(m97iban(iban,#IBAN_VAL))=1 : PrintN("[INCORRECT: MOD97]") : Continue : EndIf
  If Not Right(IBANForm(iban,#IBAN_VAL_FORM),2)=m97iban(iban,#IBAN_SUM)
    PrintN("[INCORRECT: CHECKSUM]") : Continue
  EndIf
  PrintN("[CORRECT]")
ForEver
Input()
End
```

```txt

IBAN    : GB82 WEST 1234 5698 7654 32           [CORRECT]
IBAN    : GB82WEST12345698765432                [CORRECT]
IBAN    : gb82 west 1234 5698 7654 32           [CORRECT]
IBAN    : GB82 TEST 1234 5698 7654 32           [INCORRECT: MOD97]
IBAN    : GR16 0110 1250 0000 0001 2300 695     [CORRECT]
IBAN    : GB29 NWBK 6016 1331 9268 19           [CORRECT]
IBAN    : SA03 8000 0000 6080 1016 7519         [CORRECT]
IBAN    : CH93 0076 2011 6238 5295 7            [CORRECT]
IBAN    : IL62 0108 0000 0009 9999 999          [CORRECT]
IBAN    : IL62-0108-0000-0009-9999-999          [INCORRECT: LENGTH]
IBAN    : US12 3456 7890 0987 6543 210          [INCORRECT: COUNTRY]
IBAN    : GR16 0110 1250 0000 0001 2300 695X    [INCORRECT: LENGTH]

```



## Python

```python
import re

_country2length = dict(
    AL=28, AD=24, AT=20, AZ=28, BE=16, BH=22, BA=20, BR=29,
    BG=22, CR=21, HR=21, CY=28, CZ=24, DK=18, DO=28, EE=20,
    FO=18, FI=18, FR=27, GE=22, DE=22, GI=23, GR=27, GL=18,
    GT=28, HU=28, IS=26, IE=22, IL=23, IT=27, KZ=20, KW=30,
    LV=21, LB=28, LI=21, LT=20, LU=20, MK=19, MT=31, MR=27,
    MU=30, MC=27, MD=24, ME=22, NL=18, NO=15, PK=24, PS=29,
    PL=28, PT=25, RO=24, SM=27, SA=24, RS=22, SK=24, SI=19,
    ES=24, SE=24, CH=21, TN=24, TR=26, AE=23, GB=22, VG=24 )

def valid_iban(iban):
    # Ensure upper alphanumeric input.
    iban = iban.replace(' ','').replace('\t','')
    if not re.match(r'^[\dA-Z]+$', iban):
        return False
    # Validate country code against expected length.
    if len(iban) != _country2length[iban[:2]]:
        return False
    # Shift and convert.
    iban = iban[4:] + iban[:4]
    digits = int(''.join(str(int(ch, 36)) for ch in iban)) #BASE 36: 0..9,A..Z -> 0..35
    return digits % 97 == 1

if __name__ == '__main__':
    for account in ["GB82 WEST 1234 5698 7654 32", "GB82 TEST 1234 5698 7654 32"]:
        print('%s validation is: %s' % (account, valid_iban(account)))
```
 GB82 WEST 1234 5698 7654 32 validation is: True
 GB82 TEST 1234 5698 7654 32 validation is: False


## Racket


```racket
#lang racket
(define lens
  '([AL 28] [AD 24] [AT 20] [AZ 28] [BH 22] [BE 16] [BA 20] [BR 29] [BG 22]
    [CR 21] [HR 21] [CY 28] [CZ 24] [DK 18] [DO 28] [EE 20] [FO 18] [FI 18]
    [FR 27] [GE 22] [DE 22] [GI 23] [GR 27] [GL 18] [GT 28] [HU 28] [IS 26]
    [IE 22] [IL 23] [IT 27] [KZ 20] [KW 30] [LV 21] [LB 28] [LI 21] [LT 20]
    [LU 20] [MK 19] [MT 31] [MR 27] [MU 30] [MC 27] [MD 24] [ME 22] [NL 18]
    [NO 15] [PK 24] [PS 29] [PL 28] [PT 25] [RO 24] [SM 27] [SA 24] [RS 22]
    [SK 24] [SI 19] [ES 24] [SE 24] [CH 21] [TN 24] [TR 26] [AE 23] [GB 22]
    [VG 24]))
(define (valid-iban? str)
  (define str* (regexp-replace* #px"\\s+" str ""))
  (define c (cond [(regexp-match #rx"^[A-Z][A-Z]" str*)
                   => (λ(x) (assq (string->symbol (car x)) lens))]
                  [else #f]))
  (define (letter c)
    (number->string (+ (char->integer (string-ref c 0)) -65 10)))
  (and c (= (cadr c) (string-length str*))
       (regexp-match? #rx"[A-Z0-9]" str*)
       (let* ([x (string-append (substring str* 4) (substring str* 0 4))]
              [x (string->number (regexp-replace* #rx"[A-Z]" x letter))])
         (= 1 (modulo x 97)))))

(valid-iban? "GB82 WEST 1234 5698 7654 32") ; => #t
(valid-iban? "GB82 TEST 1234 5698 7654 32") ; => #f
```



## REXX

These REXX programs can validate an IBAN  specified on the command line or from an internal list.

### basic checking


```rexx
/*REXX program validates an  IBAN  (International Bank Account Number).                 */
@.=;             @.1  =  'GB82 WEST 1234 5698 7654 32        '
                 @.2  =  'Gb82 West 1234 5698 7654 32        '
                 @.3  =  'GB82 TEST 1234 5698 7654 32        '
                 @.4  =  'GR16 0110 1250 0000 0001 2300 695  '
                 @.5  =  'GB29 NWBK 6016 1331 9268 19        '
                 @.6  =  'SA03 8000 0000 6080 1016 7519      '
                 @.7  =  'CH93 0076 2011 6238 5295 7         '
                 @.8  =  'IL62 0108 0000 0009 9999 999       '
                 @.9  =  'IL62-0108-0000-0009-9999-999       '
                 @.10 =  'US12 3456 7890 0987 6543 210       '
                 @.11 =  'GR16 0110 1250 0000 0001 2300 695X '
parse arg @.0                                             /*get optional 1st arg from CL*/
                 do k=0 + (arg()==0)  while @.k\==''      /*either:   0  or  1  ──►  n  */
                 r = val_IBAN(@.k)
                 if r==0  then say '  valid IBAN:'    @.k
                          else say 'invalid IBAN:'    @.k      "  "      r
                 if k==0  then leave             /*User specified IBAN?  Then we're done*/
                 end   /*k*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
val_IBAN:  procedure; arg x;  numeric digits 200 /*allow for big numbers in the IBAN's. */
x=space(x,0);                 L=length(x)        /*elide blanks;  determine the length. */
cc= 'AD 24 AE 23 AL 28 AT 20 AZ 28 BA 20 BE 16 BG 22 BH 22 BR 29 CH 21 CR 21 CY 28 CZ 24',
    'DE 22 DK 18 DO 28 EE 20 ES 24 FI 18 FO 18 FR 27 GB 22 GE 22 GI 23 GL 18 GR 27 GT 28',
    'HR 21 HU 28 IE 22 IL 23 IS 26 IT 27 KW 30 KZ 20 LB 28 LI 21 LT 20 LU 20 LV 21 MC 27',
    'MD 24 ME 22 MK 19 MR 27 MT 31 MU 30 NL 18 NO 15 PK 24 PL 28 PS 29 PT 25 RO 24 RS 22',
    'SA 24 SE 24 SI 19 SK 24 SM 27 TN 24 TR 26 VG 24'      /*a list of valid countries. */
@ABC# = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'   /*the Latin alphabet and decimal digits*/
cc_=left(x, 2);   kk=substr(x, 3, 2)             /*get IBAN country code and checkDigits*/
c#=wordpos(cc_, cc)                              /*find the country code index.         */
cL=word(cc, c# + 1)                              /*get the length of the country's IBAN.*/
e= '***error***  invalid IBAN'                   /*literal used when displaying an error*/
if c#==0               then return  e  'country code:'   cc_
if \datatype(x, 'A')   then return  e  'character:'      substr(x, verify(x, @ABC#), 1)
if cL\==L              then return  e  'length:'         L     ' (should be'   cL")"
y=substr(x, 5)left(x, 4)                         /*put four digs in front ───► the back.*/
z=                                               /* [↓]  translate characters ──► digits*/
     do j=1  for L;      _=substr(y, j, 1)
     if datatype(_, 'U')  then z=z || pos(_, @ABC#) + 9        /*if uppercase, then ··· */
                          else z=z || _
     end   /*j*/

if z//97==1  then return 0                       /*check if correct remainder (modulus).*/
                  return e   'check digits.'
```

```txt

  valid IBAN: GB82 WEST 1234 5698 7654 32
  valid IBAN: Gb82 West 1234 5698 7654 32
invalid IBAN: GB82 TEST 1234 5698 7654 32            ***error***  invalid check digits.
  valid IBAN: GR16 0110 1250 0000 0001 2300 695
  valid IBAN: GB29 NWBK 6016 1331 9268 19
  valid IBAN: SA03 8000 0000 6080 1016 7519
  valid IBAN: CH93 0076 2011 6238 5295 7
  valid IBAN: IL62 0108 0000 0009 9999 999
invalid IBAN: IL62-0108-0000-0009-9999-999           ***error***  invalid IBAN character: -
invalid IBAN: US12 3456 7890 0987 6543 210           ***error***  invalid IBAN country code: US
invalid IBAN: GR16 0110 1250 0000 0001 2300 695X     ***error***  invalid IBAN length: 28  (should be 27)

```



### more checking

This version of the REXX program has more error checking:
:* checks for two countries that may not be valid (as per their entry date into the IBAN system)
:* checks some countries to make sure their check digits match a specific value

```rexx
/*REXX pgm validates an IBAN (International Bank Account Number), including date ranges.*/
@.=;             @.1  =  'GB82 WEST 1234 5698 7654 32        '
                 @.2  =  'Gb82 West 1234 5698 7654 32        '
                 @.3  =  'GB82 TEST 1234 5698 7654 32        '
                 @.4  =  'GR16 0110 1250 0000 0001 2300 695  '
                 @.5  =  'GB29 NWBK 6016 1331 9268 19        '
                 @.6  =  'SA03 8000 0000 6080 1016 7519      '
                 @.7  =  'CH93 0076 2011 6238 5295 7         '
                 @.8  =  'IL62 0108 0000 0009 9999 999       '
                 @.9  =  'IL62-0108-0000-0009-9999-999       '
                 @.10 =  'US12 3456 7890 0987 6543 210       '
                 @.11 =  'GR16 0110 1250 0000 0001 2300 695X '
                 @.12 =  'GT11 2222 3333 4444 5555 6666 7777 '
                 @.13 =  'MK11 2222 3333 4444 555            '
parse arg @.0                                             /*get optional 1st arg from CL*/
                 do k=0 + (arg()==0)  while @.k\==''      /*either:   0  or  1  ──►  n  */
                 r = val_IBAN(@.k)
                 if r==0  then say '  valid IBAN:'    @.k
                          else say 'invalid IBAN:'    @.k      "  "      r
                 if k==0  then leave             /*User specified IBAN?  Then we're done*/
                 end   /*k*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
val_IBAN:  procedure; arg x;  numeric digits 200 /*allow for big numbers in the IBAN's. */
x=space(x,0);                 L=length(x)        /*elide blanks;  determine the length. */
cc= 'AD 24 AE 23 AL 28 AT 20 AZ 28 BA 20 BE 16 BG 22 BH 22 BR 29 CH 21 CR 21 CY 28 CZ 24',
    'DE 22 DK 18 DO 28 EE 20 ES 24 FI 18 FO 18 FR 27 GB 22 GE 22 GI 23 GL 18 GR 27 GT 28',
    'HR 21 HU 28 IE 22 IL 23 IS 26 IT 27 KW 30 KZ 20 LB 28 LI 21 LT 20 LU 20 LV 21 MC 27',
    'MD 24 ME 22 MK 19 MR 27 MT 31 MU 30 NL 18 NO 15 PK 24 PL 28 PS 29 PT 25 RO 24 RS 22',
    'SA 24 SE 24 SI 19 SK 24 SM 27 TN 24 TR 26 VG 24'      /*a list of valid countries. */
@ABC# = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'   /*the Latin alphabet and decimal digits*/
cc_=left(x, 2);   kk=substr(x, 3, 2)             /*get IBAN country code and checkDigits*/
c#=wordpos(cc_, cc)                              /*find the country code index.         */
cL=word(cc, c# + 1)                              /*get the length of the country's IBAN.*/
e= '***error***  invalid IBAN'                   /*literal used when displaying an error*/
if c#==0               then return  e  'country code:'   cc_
if \datatype(x, 'A')   then return  e  'character:'      substr(x, verify(x, @ABC#), 1)
if cL\==L              then return  e  'length:'         L     ' (should be'   cL")"
if cc_=='BR' & date("S")<20130701  then return e "country, Brazil isn't valid until 1-July-2013."
if cc_=='GT' & date("S")<20140701  then return e "country, Guatemala isn't valid until 1-July-2014."
if cc_=='BA' & kk\==39             then return e "check digits for Bosnia and Herzegovina:"  kk
if cc_=='MK' & kk\==07             then return e "check digits for Macedonia:"               kk
if cc_=='ME' & kk\==25             then return e "check digits for Montenegro:"              kk
if cc_=='PT' & kk\==50             then return e "check digits for Portugal:"                kk
if cc_=='SI' & kk\==56             then return e "check digits for Slovenia:"                kk
y=substr(x, 5)left(x, 4)                         /*put four digs in front ───► the back.*/
z=                                               /* [↓]  translate characters ──► digits*/
     do j=1  for L;      _=substr(y, j, 1)
     if datatype(_, 'U')  then z=z || pos(_, @ABC#) + 9        /*if uppercase, then ··· */
                          else z=z || _
     end   /*j*/

if z//97==1  then return 0                       /*check if correct remainder (modulus).*/
                  return e    'check digits.'
```

```txt

  valid IBAN: GB82 WEST 1234 5698 7654 32
  valid IBAN: Gb82 West 1234 5698 7654 32
invalid IBAN: GB82 TEST 1234 5698 7654 32            ***error***  invalid IBAN check digits.
  valid IBAN: GR16 0110 1250 0000 0001 2300 695
  valid IBAN: GB29 NWBK 6016 1331 9268 19
  valid IBAN: SA03 8000 0000 6080 1016 7519
  valid IBAN: CH93 0076 2011 6238 5295 7
  valid IBAN: IL62 0108 0000 0009 9999 999
invalid IBAN: IL62-0108-0000-0009-9999-999           ***error***  invalid IBAN character: -
invalid IBAN: US12 3456 7890 0987 6543 210           ***error***  invalid IBAN country code: US
invalid IBAN: GR16 0110 1250 0000 0001 2300 695X     ***error***  invalid IBAN length: 28  (should be 27)
invalid IBAN: GT11 2222 3333 4444 5555 6666 7777     ***error***  invalid IBAN country, Guatemala isn't valid until 1-July-2014.
invalid IBAN: MK11 2222 3333 4444 555                ***error***  invalid IBAN check digits for Macedonia: 11

```



## Ring


```ring

# Project : IBAN

codes = list(5)
codes[1] = "GB82 WEST 1234 5698 7654 32"
codes[2] = "GB82 TEST 1234 5698 7654 32"
codes[3] = "GB81 WEST 1234 5698 7654 32"
codes[4] = "SA03 8000 0000 6080 1016 7519"
codes[5] = "CH93 0076 2011 6238 5295 7"

for y = 1 to len(codes)
      see codes[y]
      flag = 1
      codes[y] = substr(codes[y], " ", "")
      checkcode(codes[y])
      check = checkiban(codes[y])
      if check = 1
         see " is valid" + nl
      else
         see " is invalid" + nl
      ok
next

func checkcode(code)
        for n = 1 to 2
              if ascii(code[n]) < 65 or ascii(code[n]) > 90
                 flag = 0
              ok
        next
        for m = 3 to len(code)
              if (ascii(code[m]) > 64 and ascii(code[m]) < 91) or (ascii(code[m]) > 47 and ascii(code[m]) < 58)
              else
                  flag = 0
              ok
        next

func checkiban(code)
        code= substr(code, 5, len(code) - 4) + left(code, 4)
        for x = 1 to len(code)
              if ascii(code[x]) > 64 and ascii(code[x]) < 91
                 code = left(code, x-1) + string(ascii(code[x]) - 55) + right(code, len(code) - x)
              ok
        next
        modold = left(code,9) % 97
        for p = 1 to floor((len(code)-9)/7)
              modnew = string(modold) + substr(code, 10 + (p-1) * 7, 7)
              modnew = number(modnew) % 97
              modold = modnew
        next
        modrest = right(code, len(code) - ((p-1)*7 + 9))
        modnew = string(modold) + modrest
        modnew = number(modnew) % 97
        return modnew

```

Output:

```txt

GB82 WEST 1234 5698 7654 32 is valid
GB82 TEST 1234 5698 7654 32 is invalid
GB81 WEST 1234 5698 7654 32 is invalid
SA03 8000 0000 6080 1016 7519 is valid
CH93 0076 2011 6238 5295 7 is valid

```



## Ruby

```Ruby
def valid_iban? iban
  len = {
    AL: 28, AD: 24, AT: 20, AZ: 28, BE: 16, BH: 22, BA: 20, BR: 29,
    BG: 22, CR: 21, HR: 21, CY: 28, CZ: 24, DK: 18, DO: 28, EE: 20,
    FO: 18, FI: 18, FR: 27, GE: 22, DE: 22, GI: 23, GR: 27, GL: 18,
    GT: 28, HU: 28, IS: 26, IE: 22, IL: 23, IT: 27, KZ: 20, KW: 30,
    LV: 21, LB: 28, LI: 21, LT: 20, LU: 20, MK: 19, MT: 31, MR: 27,
    MU: 30, MC: 27, MD: 24, ME: 22, NL: 18, NO: 15, PK: 24, PS: 29,
    PL: 28, PT: 25, RO: 24, SM: 27, SA: 24, RS: 22, SK: 24, SI: 19,
    ES: 24, SE: 24, CH: 21, TN: 24, TR: 26, AE: 23, GB: 22, VG: 24
  }

  # Ensure upper alphanumeric input.
  iban.delete! " \t"
  return false unless iban =~ /^[\dA-Z]+$/

  # Validate country code against expected length.
  cc = iban[0, 2].to_sym
  return false unless iban.size == len[cc]

  # Shift and convert.
  iban = iban[4..-1] + iban[0, 4]
  iban.gsub!(/./) { |c| c.to_i(36) }

  iban.to_i % 97 == 1
end

p valid_iban? "GB82 WEST 1234 5698 7654 32" #=> true
p valid_iban? "GB82 TEST 1234 5698 7654 32" #=> false
```



## Rust


```Rust

fn main() {
    for iban in [
        "",
        "x",
        "QQ82",
        "QQ82W",
        "GB82 TEST 1234 5698 7654 322",
        "gb82 WEST 1234 5698 7654 32",
        "GB82 WEST 1234 5698 7654 32",
        "GB82 TEST 1234 5698 7654 32",
        "GB81 WEST 1234 5698 7654 32",
        "SA03 8000 0000 6080 1016 7519",
        "CH93 0076 2011 6238 5295 7",
    ].iter()
    {
        println!(
            "'{}' is {}valid",
            iban,
            if validate_iban(iban) { "" } else { "NOT " }
        );
    }
}

fn validate_iban(iban: &str) -> bool {
    let iso_len = [
        ("AL", 28), ("AD", 24), ("AT", 20), ("AZ", 28), ("BE", 16), ("BH", 22),
        ("BA", 20), ("BR", 29), ("BG", 22), ("HR", 21), ("CY", 28), ("CZ", 24),
        ("DK", 18), ("DO", 28), ("EE", 20), ("FO", 18), ("FI", 18), ("FR", 27),
        ("GE", 22), ("DE", 22), ("GI", 23), ("GL", 18), ("GT", 28), ("HU", 28),
        ("IS", 26), ("IE", 22), ("IL", 23), ("IT", 27), ("KZ", 20), ("KW", 30),
        ("LV", 21), ("LB", 28), ("LI", 21), ("LT", 20), ("LU", 20), ("MK", 19),
        ("MT", 31), ("MR", 27), ("MU", 30), ("MC", 27), ("MD", 24), ("ME", 22),
        ("NL", 18), ("NO", 15), ("PK", 24), ("PS", 29), ("PL", 28), ("PT", 25),
        ("RO", 24), ("SM", 27), ("SA", 24), ("RS", 22), ("SK", 24), ("SI", 19),
        ("ES", 24), ("SE", 24), ("CH", 21), ("TN", 24), ("TR", 26), ("AE", 23),
        ("GB", 22), ("VG", 24), ("GR", 27), ("CR", 21),
    ];
    let trimmed_iban = iban.chars()
        .filter(|&ch| ch != ' ')
        .collect::<String>()
        .to_uppercase();
    if trimmed_iban.len() < 4 {
        return false;
    }
    let prefix = &trimmed_iban[0..2];
    if let Some(pair) = iso_len.iter().find(|&&(code, _)| code == prefix) {
        if pair.1 != trimmed_iban.len() {
            return false;
        }
    } else {
        return false;
    }
    let reversed_iban = format!("{}{}", &trimmed_iban[4..], &trimmed_iban[0..4]);
    let mut expanded_iban = String::new();
    for ch in reversed_iban.chars() {
        expanded_iban.push_str(&if ch.is_numeric() {
            format!("{}", ch)
        } else {
            format!("{}", ch as u8 - 'A' as u8 + 10u8)
        });
    }
    expanded_iban.bytes().fold(0, |acc, ch| {
        (acc * 10 + ch as u32 - '0' as u32) % 97
    }) == 1
}

```



## Scala

```Scala
import scala.collection.immutable.SortedMap

class Iban(val iban: String) {
  // Isolated tests
  def isAllUpperCase = iban.toUpperCase == iban

  def isValidpattern = (Iban.pattern findFirstIn iban).nonEmpty

  def isNationalSize = {
    Iban.ccVsLength.getOrElse(iban.take(2), 0) == iban.size
  }

  def isCheckNumberOK = {
    def rearrange = (iban.drop(4) + iban.take(4)). // Move left country code part to end
      // continue with each char converted to Int
      map(ch => if (ch.isDigit) ch.toInt - '0' else ch - 'A' + 10).mkString

    (BigInt(rearrange) mod 97) == 1
  }

  def isValidIban = {
    isAllUpperCase &&
      isValidpattern &&
      isNationalSize &&
      isCheckNumberOK
  }
}

object Iban {
  // IBAN length database
  lazy val ccVsLength: SortedMap[String, Int] = SortedMap[String, Int]() ++
    """AD24 AE23 AL28 AO25 AT20 AZ28 BA20 BE16 BF27 BG22 BH22 BI16
      |BJ28 BR29 CG27 CH21 CI28 CM27 CR21 CV25 CY28 CZ24 DE22 DK18
      |DO28 DZ24 EE20 EG27 ES24 FI18 FO18 FR27 GA27 GB22 GE22 GI23
      |GL18 GR27 GT28 HR21 HU28 IE22 IL23 IR26 IS26 IT27 JO30 KW30
      |KZ20 LB28 LI21 LT20 LU20 LV21 MC27 MD24 ME22 MG27 MK19 ML28
      |MR27 MT31 MU30 MZ25 NL18 NO15 PK24 PL28 PS29 PT25 QA29 RO24
      |RS22 SA24 SE24 SI19 SK24 SM27 SN28 TN24 TR26 UA29 VG24""".
      stripMargin.replaceAll( """\s""", " ").split(' ').
      map(v => (v.take(2), if (v.isEmpty) 0 else v.slice(2, 4).toInt))

  lazy val pattern = "([A-Z]{2})([0-9]{2})([A-Z0-9]{4})([A-Z0-9]{0,2})([0-9]{7})(([A-Z0-9]?){0,16})".r

  def apply(s: String) = new Iban(s.replaceAll( """\s""", ""))
}
```
The test program:

```Scala
object IbanTest extends App {
  def blackCases = """AT611904300235473201
                     |GB82TEST12345698765432
                     |GB81WEST12345698765432""".stripMargin

  def whiteCases = """AD1200012030200359100100
                     |AE26 0211 0000 0023 0064 016
                     |AL47 2121 1009 0000 0002 3569 8741
                     |AO06000600000100037131174
                     |AZ21NABZ00000000137010001944
                     |BA391290079401028494
                     |BE68539007547034
                     |BF1030134020015400945000643
                     |BG80BNBG96611020345678
                     |BH29BMAG1299123456BH00
                     |BI43201011067444
                     |BJ11B00610100400271101192591
                     |BR9700360305000010009795493P1
                     |CG5230011000202151234567890
                     |CH9300762011623852957
                     |CI05A00060174100178530011852
                     |CM2110003001000500000605306
                     |CR0515202001026284066
                     |CV64000300004547069110176
                     |CY17002001280000001200527600
                     |CZ6508000000192000145399
                     |DE89370400440532013000
                     |DK5000400440116243
                     |DO28BAGR00000001212453611324
                     |DZ4000400174401001050486
                     |EE382200221020145685
                     |EG1100006001880800100014553
                     |ES9121000418450200051332
                     |FI2112345600000785
                     |FO1464600009692713
                     |FR1420041010050500013M02606
                     |FR7630007000110009970004942
                     |GA2140002000055602673300064
                     |GB29NWBK60161331926819
                     |GE29NB0000000101904917
                     |GI75NWBK000000007099453
                     |GL8964710001000206
                     |GR1601101250000000012300695
                     |GT82TRAJ01020000001210029690
                     |HR1210010051863000160
                     |HU42117730161111101800000000
                     |IE29AIBK93115212345678
                     |IL620108000000099999999
                     |IR580540105180021273113007
                     |IS140159260076545510730339
                     |IT60X0542811101000000123456
                     |JO94CBJO0010000000000131000302
                     |KW74NBOK0000000000001000372151
                     |KZ176010251000042993
                     |LB30099900000001001925579115
                     |LI21088100002324013AA
                     |LT121000011101001000
                     |LU280019400644750000
                     |LV80BANK0000435195001
                     |MC5813488000010051108001292
                     |MD24AG000225100013104168
                     |ME25505000012345678951
                     |MG4600005030010101914016056
                     |MK07300000000042425
                     |ML03D00890170001002120000447
                     |MR1300012000010000002037372
                     |MT84MALT011000012345MTLCAST001S
                     |MU17BOMM0101101030300200000MUR
                     |MZ59000100000011834194157
                     |NL91ABNA0417164300
                     |NL81 TRIO 0212 4710 66
                     |NO9386011117947
                     |PK24SCBL0000001171495101
                     |PL27114020040000300201355387
                     |PS92PALS000000000400123456702
                     |PT50000200000163099310355
                     |PT50000201231234567890154
                     |QA58 DOHB 0000 1234 5678 90AB CDEF G
                     |RO49 AAAA 1B31 0075 9384 0000
                     |RS35260005601001611379
                     |SA0380000000608010167519
                     |SE3550000000054910000003
                     |SI56191000000123438
                     |SK3112000000198742637541
                     |SM86U0322509800000000270100
                     |SN12K00100152000025690007542
                     |TN5914207207100707129648
                     |TR330006100519786457841326
                     |UA57 3543 4700 0676 2462 0549 2502 6
                     |VG96 VPVG 0000 0123 4567 8901
                     |GB82 WEST 1234 5698 7654 32
                     |SA03 8000 0000 6080 1016 7519
                     |CH93 0076 2011 6238 5295 7""".stripMargin

  whiteCases.lines.foreach(l => assert(Iban(l).isValidIban))
  blackCases.lines.foreach(l => assert(!Iban(l).isValidIban))
  println(s"Successfully completed; ${whiteCases.lines.size + blackCases.lines.size} cases tested, no errors.")
}
```

 Successfully completed; 91 cases tested, no errors.


## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";

const type: countryHash is hash [string] integer;

const func countryHash: initCountryCode is func
  result
    var countryHash: cc is countryHash.value;
  begin
    cc @:= ["AL"] 28; cc @:= ["AD"] 24; cc @:= ["AT"] 20; cc @:= ["AZ"] 28; cc @:= ["BE"] 16; cc @:= ["BH"] 22;
    cc @:= ["BA"] 20; cc @:= ["BR"] 29; cc @:= ["BG"] 22; cc @:= ["CR"] 21; cc @:= ["HR"] 21; cc @:= ["CY"] 28;
    cc @:= ["CZ"] 24; cc @:= ["DK"] 18; cc @:= ["DO"] 28; cc @:= ["EE"] 20; cc @:= ["FO"] 18; cc @:= ["FI"] 18;
    cc @:= ["FR"] 27; cc @:= ["GE"] 22; cc @:= ["DE"] 22; cc @:= ["GI"] 23; cc @:= ["GR"] 27; cc @:= ["GL"] 18;
    cc @:= ["GT"] 28; cc @:= ["HU"] 28; cc @:= ["IS"] 26; cc @:= ["IE"] 22; cc @:= ["IL"] 23; cc @:= ["IT"] 27;
    cc @:= ["KZ"] 20; cc @:= ["KW"] 30; cc @:= ["LV"] 21; cc @:= ["LB"] 28; cc @:= ["LI"] 21; cc @:= ["LT"] 20;
    cc @:= ["LU"] 20; cc @:= ["MK"] 19; cc @:= ["MT"] 31; cc @:= ["MR"] 27; cc @:= ["MU"] 30; cc @:= ["MC"] 27;
    cc @:= ["MD"] 24; cc @:= ["ME"] 22; cc @:= ["NL"] 18; cc @:= ["NO"] 15; cc @:= ["PK"] 24; cc @:= ["PS"] 29;
    cc @:= ["PL"] 28; cc @:= ["PT"] 25; cc @:= ["RO"] 24; cc @:= ["SM"] 27; cc @:= ["SA"] 24; cc @:= ["RS"] 22;
    cc @:= ["SK"] 24; cc @:= ["SI"] 19; cc @:= ["ES"] 24; cc @:= ["SE"] 24; cc @:= ["CH"] 21; cc @:= ["TN"] 24;
    cc @:= ["TR"] 26; cc @:= ["AE"] 23; cc @:= ["GB"] 22; cc @:= ["VG"] 24;
  end func;

const countryHash: countryCode is initCountryCode;

const func boolean: isLegal (in var string: iban) is func
  result
    var boolean: legal is FALSE;
  local
    var char: ch is ' ';
    var string: converted is "";
  begin
    iban := upper(replace(iban, " ", ""));
    legal := iban[.. 2] in countryCode and countryCode[iban[.. 2]] = length(iban);
    iban := iban[5 ..] & iban[.. 4];
    for ch range iban do
      case ch of
        when {'0' .. '9'}: converted &:= ch;
        when {'A' .. 'Z'}: converted &:= str(ord(ch) - ord('A') + 10);
        otherwise: legal := FALSE;
      end case;
    end for;
    legal := legal and (bigInteger parse converted) rem 97_ = 1_;
  end func;

const proc: check (in string: iban) is func
  begin
    writeln("Valid " <& iban <& ": " <& isLegal(iban));
  end func;

const proc: main is func
  begin
    check("GB82 WEST 1234 5698 7654 32");
    check("GB82WEST12345698765432");
    check("gb82 west 1234 5698 7654 32");
    check("GB82 TEST 1234 5698 7654 32");
    check("GB82 WEST 1243 5698 7654 32");
  end func;
```

```txt

Valid GB82 WEST 1234 5698 7654 32: TRUE
Valid GB82WEST12345698765432: TRUE
Valid gb82 west 1234 5698 7654 32: TRUE
Valid GB82 TEST 1234 5698 7654 32: FALSE
Valid GB82 WEST 1243 5698 7654 32: FALSE

```



## Sidef


```ruby
func valid_iban(iban) {
  static len = Hash(
    AD=>24, AE=>23, AL=>28, AO=>25, AT=>20, AZ=>28, BA=>20, BE=>16, BF=>27,
    BG=>22, BH=>22, BI=>16, BJ=>28, BR=>29, CG=>27, CH=>21, CI=>28, CM=>27,
    CR=>21, CV=>25, CY=>28, CZ=>24, DE=>22, DK=>18, DO=>28, DZ=>24, EE=>20,
    EG=>27, ES=>24, FI=>18, FO=>18, FR=>27, GA=>27, GB=>22, GE=>22, GI=>23,
    GL=>18, GR=>27, GT=>28, HR=>21, HU=>28, IE=>22, IL=>23, IR=>26, IS=>26,
    IT=>27, JO=>30, KW=>30, KZ=>20, LB=>28, LI=>21, LT=>20, LU=>20, LV=>21,
    MC=>27, MD=>24, ME=>22, MG=>27, MK=>19, ML=>28, MR=>27, MT=>31, MU=>30,
    MZ=>25, NL=>18, NO=>15, PK=>24, PL=>28, PS=>29, PT=>25, QA=>29, RO=>24,
    RS=>22, SA=>24, SE=>24, SI=>19, SK=>24, SM=>27, SN=>28, TN=>24, TR=>26,
    UA=>29, VG=>24,
  )

  # Ensure upper alphanumeric input.
  iban -= /\s+/g
  iban.uc! ~~ /^[0-9A-Z]+\z/ || return false

  # Validate country code against expected length.
  var cc = iban.substr(0, 2)
  iban.len == len{cc} || return false

  # Shift and convert.
  iban.sub!(/(.{4})(.+)/, {|a,b| b+a})
  iban.gsub!(/([A-Z])/,   {|a| a.ord - 55})

  iban.to_i % 97 == 1
}

say valid_iban("GB82 WEST 1234 5698 7654 32") #=> true
say valid_iban("GB82 TEST 1234 5698 7654 32") #=> false
```



## SNOBOL4


```SNOBOL4
* IBAN - International Bank Account Number validation
      DEFINE('ibantable()')                           :(iban_table_end)
ibantable
      ibantable = TABLE(70)
      ibancodes =
+        'AL28AD24AT20AZ28BE16BH22BA20BR29BG22CR21'
+        'HR21CY28CZ24DK18DO28EE20FO18FI18FR27GE22'
+        'DE22GI23GR27GL18GT28HU28IS26IE22IL23IT27'
+        'KZ20KW30LV21LB28LI21LT20LU20MK19MT31MR27'
+        'MU30MC27MD24ME22NL18NO15PK24PS29PL28PT25'
+        'RO24SM27SA24RS22SK24SI19ES24SE24CH21TN24'
+        'TR26AE23GB22VG24'
      nordeacodes =
+        'DZ24AO25BJ28FB27BI16CM27CV25IR26CI28MG27'
+        'ML28MZ25SN28UA29'
      allcodes = ibancodes nordeacodes
iban1 allcodes LEN(2) . country LEN(2) . length =     :F(return)
      ibantable<country> = length                     :(iban1)
iban_table_end

      DEFINE('tonumbers(tonumbers)letter,p')          :(tonumbers_end)
tonumbers
      tonumbers ANY(&UCASE) . letter                  :f(RETURN)
      &UCASE @p letter
      tonumbers letter = p + 10                       :(tonumbers)
tonumbers_end

* modulo for long integers
*
      DEFINE('mod(m,n)')                              :(mod_end)
mod   m LEN(9) . r =                                  :f(modresult)
      mod = REMDR(CONVERT(r,"INTEGER"), n)
mod0  m LEN(7) . r =                                  :f(modresult)
      mod = mod r
      mod = REMDR(mod, n)                             :(mod0)
modresult
      mod = GT(SIZE(m), 0) REMDR(mod m, n)            :(RETURN)
mod_end

      DEFINE('invalid(l,t)')                          :(invalid_end)
invalid
      OUTPUT = "Invalid IBAN: " l ": " t              :(RETURN)
invalid_end

***** main *****
      ibant  = ibantable()
      FREEZE(ibant)

      INPUT(.INPUT, 28,,'iban.dat')
read  line   = INPUT                                  :f(END)
      country = checkdigits = line2 =
**    GB82 WEST 1234 5698 7654 32
**    Uppercase line2 and remove spaces.
      line2 = REPLACE(line, &LCASE, &UCASE)
space line2 ANY(" ") =                                :s(space)

**    GB82WEST12345698765432
**    Capture country code and checkdigits.
      line2 LEN(2) . country LEN(2) . checkdigits

**    1. Is the country code known?
      IDENT(ibant<country>)
+       invalid(line, "unlisted country: " country)   :s(read)

**    2. Is the length correct for the country?
      NE(SIZE(line2), ibant<country>)
+       invalid(line, "length: " SIZE(line2)
+       " not " ibant<country>)                       :s(read)

**    3. Move first four chars to end of line.
**    Convert line2 letters to numbers.
**    3214282912345698765432161182
      line2 = SUBSTR(line2,5) SUBSTR(line2,1,4)
      line2 = tonumbers(line2)
**    Mod_97 of line2 = 1?
      modsum  = mod(line2, 97)
      NE(modsum, 1)
+       invalid(line, "mod_97 " modsum " not = 1")    :s(read)

      OUTPUT = "valid IBAN: " line                    :(read)
END
```


'''Output:'''

```txt
valid IBAN: GB82 WEST 1234 5698 7654 32
valid IBAN: GB82WEST12345698765432
valid IBAN: gb82 west 1234 5698 7654 32
Invalid IBAN: GB82 TEST 1234 5698 7654 3244: length: 24 not 22
Invalid IBAN: US12 3456 7890 0987 6543 210: unlisted country: US
Invalid IBAN: GB82 WEST 1234 5698 7654 33: mod_97 28 not = 1
Invalid IBAN: GB81 WEST 1234 5698 7654 32: mod_97 0 not = 1
valid IBAN: GB29 NWBK 6016 1331 9268 19
valid IBAN: SA03 8000 0000 6080 1016 7519
valid IBAN: CH93 0076 2011 6238 5295 7
valid IBAN: IL62 0108 0000 0009 9999 999
Invalid IBAN: IL62-0108-0000-0009-9999-999: length: 28 not 23
Invalid IBAN: GR16 0110 1250 0000 0001 2300 695X: length: 28 not 27
```





## Standard ML

```sml
(* country_code : string -> int *)
(* Get the length of a valid IBAN given the two chars long country code *)
fun country_code (str : string) : int =
  case str of
    "AL" => 28 | "AD" => 24 | "AT" => 20 | "AZ" => 28
  | "BE" => 16 | "BH" => 22 | "BA" => 20 | "BR" => 29
  | "BG" => 22 | "CR" => 21 | "HR" => 21 | "CY" => 28
  | "CZ" => 24 | "DK" => 18 | "DO" => 28 | "EE" => 20
  | "FO" => 18 | "FI" => 18 | "FR" => 27 | "GE" => 22
  | "DE" => 22 | "GI" => 23 | "GR" => 27 | "GL" => 18
  | "GT" => 28 | "HU" => 28 | "IS" => 26 | "IE" => 22
  | "IL" => 23 | "IT" => 27 | "KZ" => 20 | "KW" => 30
  | "LV" => 21 | "LB" => 28 | "LI" => 21 | "LT" => 20
  | "LU" => 20 | "MK" => 19 | "MT" => 31 | "MR" => 27
  | "MU" => 30 | "MC" => 27 | "MD" => 24 | "ME" => 22
  | "NL" => 18 | "NO" => 15 | "PK" => 24 | "PS" => 29
  | "PL" => 28 | "PT" => 25 | "RO" => 24 | "SM" => 27
  | "SA" => 24 | "RS" => 22 | "SK" => 24 | "SI" => 19
  | "ES" => 24 | "SE" => 24 | "CH" => 21 | "TN" => 24
  | "TR" => 26 | "AE" => 23 | "GB" => 22 | "VG" => 24
  | _ => raise Domain


(* removespace : string -> string *)
(* Removes all spaces from a string *)
fun removespace s = String.translate (fn #" " => "" | c => str c) s

(* to_upper : string -> string *)
(* Convert every char to upper of a string *)
fun to_upper (s : string) : string =
  String.translate (fn c => str (Char.toUpper c)) s

(* convert_to_number : char -> string *)
(* Covert a alphanumeric char into a numerical string *)
fun convert_to_number (c : char) : string =
  if Char.isDigit c then str c else
    if Char.isUpper c then Int.toString (10 + ord c - ord #"A") else
      raise Domain


(* verify_iban : string -> bool *)
(* Check weather a string is a valid IBAN *)
fun verify_iban str =
  let
    (* Remove spaces and make upper case *)
    val str = to_upper (removespace str)
    (* Fetch first two chars (country code) *)
    val country = String.substring (str, 0, 2)
    val len = country_code country
  in
    (* size test *)
    String.size str = len
    andalso
    (* Every char must be alphanumeric *)
    List.all Char.isAlphaNum (explode str)
    andalso
    let
      (* Reorder *)
      val str = String.substring (str, 4, String.size str - 4) ^ String.substring (str, 0, 4)
      (* Convert into digits *)
      val str = String.translate convert_to_number str
      (* Convert into a big number *)
      val number = valOf (IntInf.fromString str)
    in
      IntInf.mod (number, 97) = 1
    end
  end handle Subscript => false | Domain => false
```


```txt
- verify_iban "GB82 WEST 1234 5698 7654 32";
val it = true : bool
- verify_iban "GB82 TEST 1234 5698 7654 32";
val it = false : bool
```



## Tcl


```tcl
proc verifyIBAN {iban} {
    # Normalize by up-casing and stripping illegal chars (e.g., space)
    set iban [regsub -all {[^A-Z0-9]+} [string toupper $iban] ""]
    # Get the expected length from the country-code part
    switch [string range $iban 0 1] {
	NO { set len 15 }
	BE { set len 16 }
	DK - FI - FO - GL - NL { set len 18}
	MK - SI { set len 19 }
	AT - BA - EE - KZ - LT - LU { set len 20 }
	CH - CR - HR - LI - LV { set len 21 }
	BG - BH - DE - GB - GE - IE - ME - RS { set len 22 }
	AE - GI - IL { set len 23 }
	AD - CZ - ES - MD - PK - RO - SA - SE - SK - TN - VG { set len 24 }
	PT { set len 25 }
	IS - TR { set len 26 }
	FR - GR - IT - MC - MR - SM { set len 27 }
	AL - AZ - CY - DO - GT - HU - LB - PL { set len 28 }
	BR - PS { set len 29 }
	KW - MU { set len 30 }
	MT { set len 31 }
	default {
	    # unsupported country code
	    return false
	}
    }
    # Convert to number
    set num [string map {
	A 10 B 11 C 12 D 13 E 14 F 15 G 16 H 17 I 18 J 19 K 20 L 21 M 22
	N 23 O 24 P 25 Q 26 R 27 S 28 T 29 U 30 V 31 W 32 X 33 Y 34 Z 35
    } [string range $iban 4 end][string range $iban 0 3]]
    # Verify length and modulus
    return [expr {[string length $iban] == $len && $num % 97 == 1}]
}
```
Demonstrating:

```tcl
set iban "GB82 WEST 1234 5698 7654 32"
puts "$iban is [expr {[verifyIBAN $iban] ? {verified} : {unverified}}]"
set not "GB42 WEST 1234 5698 7654 32"
puts "$not is [expr {[verifyIBAN $not] ? {verified} : {unverified}}]"
```
```txt
GB82 WEST 1234 5698 7654 32 is verified
GB42 WEST 1234 5698 7654 32 is unverified
```



## UNIX Shell

This does not verify the country code or the length.

```bash
declare -A base36=(
    [A]=10 [B]=11 [C]=12 [D]=13 [E]=14 [F]=15 [G]=16 [H]=17 [I]=18
    [J]=19 [K]=20 [L]=21 [M]=22 [N]=23 [O]=24 [P]=25 [Q]=26 [R]=27
    [S]=28 [T]=29 [U]=30 [V]=31 [W]=32 [X]=33 [Y]=34 [Z]=35
)

function is_iban {
    local -u acct=${1//[^[:alnum:]]/}
    acct=${acct:4}${acct:0:4}
    local i char digits=""
    for ((i=0; i<${#acct}; i++)); do
        char=${acct:i:1}
        digits+=${base36[$char]:-$char}
    done
    local mod=$(mod97 $digits)
    (( mod == 1 ))
}

function mod97 {
    local D=$1
    N=${D:0:9}
    D=${D:9}
    while [[ -n $D ]]; do
        mod=$(( N % 97 ))
        N=$(printf "%02d%s" $mod ${D:0:7})
        D=${D:7}
    done
    echo $(( N % 97 ))
}

for test in "GB82 WEST 1234 5698 7654 32" "GB42 WEST 1234 5698 7654 32"; do
    printf "%s : " "$test"
    is_iban "$test" && echo yes || echo no
done
```


```txt
GB82 WEST 1234 5698 7654 32 : yes
GB42 WEST 1234 5698 7654 32 : no
```



## VBA

```vb
Public nations As New Collection
Private Sub init()
    nations.Add 24, "AD"
    nations.Add 21, "CH"
    nations.Add 22, "GB"
    nations.Add 24, "SA"
    nations.Add 20, "XK"
End Sub

Private Function mod97(ByVal c As String) As Integer
    Dim n As Long
    n = Val(Mid(c, 1, 9))
    c = Mid(c, 10, Len(c) - 9)
    n = n Mod 97
    Do While Len(c) > 6
        n = Val(Str(n) & Mid(c, 1, 7))
        n = n Mod 97
        c = Mid(c, 8, Len(c) - 7)
    Loop
    n = Val(Str(n) & c)
    mod97 = n Mod 97
End Function

Private Function iban(ByVal code As String) As Boolean
'-- This routine does and should reject codes containing spaces etc.
'-- Use iban_s() below for otherwise.
    On Error GoTo 1
    lengths = nations(Mid(code, 1, 2))
    If Len(code) = lengths Then
        code = code & Mid(code, 1, 4)
        code = Mid(code, 5, lengths)
        Dim c As String: c = ""
        Dim ch As String
        For i = 1 To lengths
            ch = Mid(code, i, 1)
            If ch >= "0" And ch <= "9" Then
                c = c & ch
            Else
                If ch >= "A" And ch <= "Z" Then
                    c = c & Str(Asc(ch) - 55)
                Else
                    iban = False
                End If
            End If
        Next i
        c = Replace(c, " ", "")
        iban = mod97(c) = 1
    End If
    Exit Function
1:
    iban = False
End Function

Private Function iban_s(code As String) As Boolean
'-- strips any embedded spaces and hyphens before validating.
    code = Replace(code, " ", "")
    code = Replace(code, "-", "")
    iban_s = iban(code)
End Function

Private Sub test(code As String, expected As Boolean)
    Dim valid As Boolean
    valid = iban_s(code)
    Dim state As String
    If valid = expected Then
        state = IIf(valid, "ok", "invalid (as expected)")
    Else
        state = IIf(valid, "OK!!", "INVALID!!") & " (NOT AS EXPECTED)"
    End If
    Debug.Print code, state
End Sub

Public Sub main()
    init
    test "GB82 WEST 1234 5698 7654 32", True
    test "GB82 TEST 1234 5698 7654 32", False
    test "GB81 WEST 1234 5698 7654 32", False
    test "SA03 8000 0000 6080 1016 7519", True
    test "CH93 0076 2011 6238 5295 7", True
End Sub
```
```txt
GB82WEST12345698765432      ok
GB82TEST12345698765432      invalid (as expected)
GB81WEST12345698765432      invalid (as expected)
SA0380000000608010167519    ok
CH9300762011623852957       ok
```


## VBScript


```vb

Function validate_iban(s)
	validate_iban = Chr(34) & s & Chr(34) & " is NOT valid."
	Set cn_len = CreateObject("Scripting.Dictionary")
	With cn_len
		.Add "AL",28 : .Add "AD",24 : .Add "AT",20 : .Add "AZ",28 : .Add "BH",22 : .Add "BE",16
		.Add "BA",20 : .Add "BR",29 : .Add "BG",22 : .Add "CR",21 : .Add "HR",21 : .Add "CY",28
		.Add "CZ",24 : .Add "DK",18 : .Add "DO",28 : .Add "EE",20 : .Add "FO",18 : .Add "FI",18
		.Add "FR",27 : .Add "GE",22 : .Add "DE",22 : .Add "GI",23 : .Add "GR",27 : .Add "GL",18
		.Add "GT",28 : .Add "HU",28 : .Add "IS",26 : .Add "IE",22 : .Add "IL",23 : .Add "IT",27
		.Add "JO",30 : .Add "KZ",20 : .Add "KW",30 : .Add "LV",21 : .Add "LB",28 : .Add "LI",21
		.Add "LT",20 : .Add "LU",20 : .Add "MK",19 : .Add "MT",31 : .Add "MR",27 : .Add "MU",30
		.Add "MC",27 : .Add "MD",24 : .Add "ME",22 : .Add "NL",18 : .Add "NO",15 : .Add "PK",24
		.Add "PS",29 : .Add "PL",28 : .Add "PT",25 : .Add "QA",29 : .Add "RO",24 : .Add "SM",27
		.Add "SA",24 : .Add "RS",22 : .Add "SK",24 : .Add "SI",19 : .Add "ES",24 : .Add "SE",24
		.Add "CH",21 : .Add "TN",24 : .Add "TR",26 : .Add "AE",23 : .Add "GB",22 : .Add "VG",24
	End With
	iban = Replace(s," ","")
	If cn_len.Exists(Left(iban,2)) And Len(iban) = cn_len.Item(Left(iban,2)) Then
		'move the first 4 characters to the end
		iban = Mid(iban,5,Len(iban)-4) & Left(iban,4)
		'convert letters to numbers A=10 to Z=35
		D = ""
		For i = 1 To Len(iban)
			If Asc(Mid(iban,i,1)) >= 65 And Asc(Mid(iban,i,1)) <= 90 Then
				D = D & CStr(Asc(Mid(iban,i,1)) - 55)
			Else
				D = D & Mid(iban,i,1)
			End If
		Next
		'piece-wise modulo calculation
		Do
			If Len(D) > 9 Then
				N = CLng(Left(D,9)) Mod 97
				D = CStr(N) & Mid(D,10,Len(D)-9)
			Else
				N = CLng(Left(D,9)) Mod 97
				Exit Do
			End If
		Loop
		If N = 1 Then
			validate_iban = Chr(34) & s & Chr(34) & " is valid."
		End If
	End If
End Function

'test several scenarios
WScript.StdOut.WriteLine validate_iban("GB82 WEST 1234 5698 7654 32")
WScript.StdOut.WriteLine validate_iban("GB82WEST12345698765432")
WScript.StdOut.WriteLine validate_iban("gb82 west 1234 5698 7654 32")
WScript.StdOut.WriteLine validate_iban("GB82 TEST 1234 5698 7654 32")
WScript.StdOut.WriteLine validate_iban("GR16 0110 1250 0000 0001 2300 695")
WScript.StdOut.WriteLine validate_iban("GB29 NWBK 6016 1331 9268 19")
WScript.StdOut.WriteLine validate_iban("SA03 8000 0000 6080 1016 7519")
WScript.StdOut.WriteLine validate_iban("CH93 0076 2011 6238 5295 7")
WScript.StdOut.WriteLine validate_iban("IL62 0108 0000 0009 9999 999")
WScript.StdOut.WriteLine validate_iban("IL62-0108-0000-0009-9999-999")
WScript.StdOut.WriteLine validate_iban("US12 3456 7890 0987 6543 210")
WScript.StdOut.WriteLine validate_iban("GR16 0110 1250 0000 0001 2300 695X")

```


```txt

"GB82 WEST 1234 5698 7654 32" is valid.
"GB82WEST12345698765432" is valid.
"gb82 west 1234 5698 7654 32" is NOT valid.
"GB82 TEST 1234 5698 7654 32" is NOT valid.
"GR16 0110 1250 0000 0001 2300 695" is valid.
"GB29 NWBK 6016 1331 9268 19" is valid.
"SA03 8000 0000 6080 1016 7519" is valid.
"CH93 0076 2011 6238 5295 7" is valid.
"IL62 0108 0000 0009 9999 999" is valid.
"IL62-0108-0000-0009-9999-999" is NOT valid.
"US12 3456 7890 0987 6543 210" is NOT valid.
"GR16 0110 1250 0000 0001 2300 695X" is NOT valid.

```



## Yabasic

```Yabasic
// List updated to release 72, 25 November 2016, of IBAN Registry (75 countries)
countryCodes$ = "AD24 AE23 AL28 AT20 AZ28 BA20 BE16 BG22 BH22 BR29 BY28 CH21 CR22 CY28 CZ24 DE22 "
countryCodes$ = countryCodes$ + "DK18 DO28 EE20 ES24 FI18 FO18 FR27 GB22 GE22 GI23 GL18 GR27 GT28 HR21 HU28 IE22 "
countryCodes$ = countryCodes$ + "IL23 IQ23 IS26 IT27 JO30 KW30 KZ20 LB28 LC32 LI21 LT20 LU20 LV21 MC27 MD24 ME22 "
countryCodes$ = countryCodes$ + "MK19 MR27 MT31 MU30 NL18 NO15 PK24 PL28 PS29 PT25 QA29 RO24 RS22 SA24 SC31 SE24 "
countryCodes$ = countryCodes$ + "SI19 SK24 SM27 ST25 SV28 TL23 TN24 TR26 UA29 VG24 XK20"

sub iban(code$)
// This routine does and should reject codes containing spaces etc.
// Use iban_s() below for otherwise.
    local country, lcode, c, i, ch$

    lcode = len(code$)
    country = instr(countryCodes$, upper$(left$(code$, 2)))

    if country and lcode = val(mid$(countryCodes$, country + 2, 2)) then
        code$ = right$(code$, lcode - 4) + left$(code$, 4)
        for i = 1 to lcode
            ch$ = mid$(code$, i, 1)
            if ch$ >= "0" and ch$ <= "9" then
                c = c * 10 + asc(ch$) - asc("0")
            elsif ch$ >= "A" and ch$ <= "Z" then
                c = c * 100 + asc(ch$) - 55
            else
                return false
            end if
            c = mod(c, 97)
        next i
        return c = 1
    end if
    return false
end sub

sub iban_s(code$)
// strips any embedded spaces and hyphens before validating.
	local i, t$(1), n

	i = token(code$, t$(), " -")
	code$ = ""
	for n = 1 to i
		code$ = code$ + t$(n)
	next n

    return iban(code$)
end sub

sub test(code$, expected)
    local valid, state$

    valid = iban_s(code$)

    if valid = expected then
    	if valid then
    		state$ = "ok"
    	else
    		state$ = "invalid (as expected)"
    	end if
    else
    	if valid then
    		state$ = "OK!!"
    	else
    		state$ = "INVALID!!"
    	end if
    	state$ = state$ + " (NOT AS EXPECTED)"
    end if
    print code$, "\t ", state$
end sub

test("GB82 WEST 1234 5698 7654 32", true)
test("GB82 TEST 1234 5698 7654 32", false)
test("GB81 WEST 1234 5698 7654 32", false)
test("SA03 8000 0000 6080 1016 7519", true)
test("CH93 0076 2011 6238 5295 7", true)
```



## zkl

Using GMP big nums:

```zkl
var BN=Import("zklBigNum");
fcn validateIBAN(iban){
   iban=iban-" \t";
   alphaNums.matches(iban) and (ibans.find(iban[0,2])==iban.len()) and
   ( BN((iban[4,*]+iban[0,4]).apply("toInt",36)) % 97 == 1 )
}
```
Without using big nums:

```zkl
fcn validateIBAN(iban){
   iban=iban-" \t";
   alphaNums.matches(iban) and (ibans.find(iban[0,2])==iban.len()) and
   ( (iban[4,*]+iban[0,4]).apply("toInt",36) : mod97(_) == 1 )
}
fcn mod97(str){
   n:=0; m:=9; M:=0; while(N:=str[n,m]){
      M=((M.toString()+N).toInt()) % 97;
      n+=m; m=7;
   }
   M
}
```

```zkl
var alphaNums=RegExp("^[0-9A-Z]+$");
var ibans=  // Dictionary("AD":24, ...)
  ("AD24 AE23 AL28 AO25 AT20 AZ28 BA20 BE16 BF27 BG22 BH22 BI16 "
   "BJ28 BR29 CG27 CH21 CI28 CM27 CR21 CV25 CY28 CZ24 DE22 DK18 "
   "DO28 DZ24 EE20 EG27 ES24 FI18 FO18 FR27 GA27 GB22 GE22 GI23 "
   "GL18 GR27 GT28 HR21 HU28 IE22 IL23 IR26 IS26 IT27 JO30 KW30 "
   "KZ20 LB28 LI21 LT20 LU20 LV21 MC27 MD24 ME22 MG27 MK19 ML28 "
   "MR27 MT31 MU30 MZ25 NL18 NO15 PK24 PL28 PS29 PT25 QA29 RO24 "
   "RS22 SA24 SE24 SI19 SK24 SM27 SN28 TN24 TR26 UA29 VG24")
   .split(" ").pump(D(),fcn(w){return(w[0,2],w[2,*].toInt())});
```
Testing 1 2 3

```zkl
    // all valid
T("GB82 WEST 1234 5698 7654 32","GB82WEST12345698765432",
  "GR16 0110 1250 0000 0001 2300 695","GB29 NWBK 6016 1331 9268 19",
   "SA03 8000 0000 6080 1016 7519","CH93 0076 2011 6238 5295 7",
   "IL62 0108 0000 0009 9999 999")
.apply(validateIBAN).println();

  // all invalid
T("gb82 west 1234 5698 7654 32","GB82 TEST 1234 5698 7654 32",
  "GB82 WEST 1243 5698 7654 32","AE82 WEST 1234 5698 7654 32"
  "IL62-0108-0000-0009-9999-999","US12 3456 7890 0987 6543 210",
  "GR16 0110 1250 0000 0001 2300 695X","GT11 2222 3333 4444 5555 6666 7777",
  "MK11 2222 3333 4444 555")
.apply(validateIBAN).println();

   // white list from Scala
("AD1200012030200359100100 AE260211000000230064016 AL47212110090000000235698741 "
...
"CH9300762011623852957").split()
.apply(validateIBAN).toString(*).println();
```

```txt

L(True,True,True,True,True,True,True)
L(False,False,False,False,False,False,False,False)
L(True,True,True,True,True,True,True,True,True,True,True,True,
  True,True,True,True,True,True,True,True,True,True,True,True,True,
  True,True,True,True,True,True,True,True,True,True,True,True,True,
  True,True,True,True,True,True,True,True,True,True,True,True,True,
  True,True,True,True,True,True,True,True,True,True,True,True,
  Tre,True,True,True,True,True,True,True,True,True,True,True,
  True,True,True,True,True,True,True,True,True,True,True,True)

```



## ZX Spectrum Basic

```zxbasic
10 REM REM Used the following as official standard:
20 REM REM  http://www.cnb.cz/cs/platebni_styk/iban/download/EBS204.pdf
30 REM REM Pairs of ISO 3166 country code & expected IBAN length for this country
40 LET c$="AL28 AD24 AT20 AZ28 BE16 BH22 BA20 BR29 BG22 CR21 HR21 CY28 CZ24 DK18 DO28 EE20 "
50 LET c$=c$+"FO18 FI18 FR27 GE22 DE22 GI23 GR27 GL18 GT28 HU28 IS26 IE22 IL23 IT27 KZ20 KW30 "
60 LET c$=c$+"LV21 LB28 LI21 LT20 LU20 MK19 MT31 MR27 MU30 MC27 MD24 ME22 NL18 NO15 PK24 PS29 "
70 LET c$=c$+"PL28 PT25 RO24 SM27 SA24 RS22 SK24 SI19 ES24 SE24 CH21 TN24 TR26 AE23 GB22 VG24 "
80 LET e$=""
100 LET i$="GB82 WEST 1234 5698 7654 32": GO SUB 1000
110 LET i$="gb82 west 1234 5698 7654 32": GO SUB 1000
120 LET i$="GB82 TEST 1234 5698 7654 32": GO SUB 1000
130 LET i$="GR16 0110 1250 0000 0001 2300 695": GO SUB 1000
140 LET i$="IL62-0108-0000-0009-9999-999": GO SUB 1000
900 STOP
1000 REM IBAN check routine
1010 LET explen=0: LET lenc=LEN c$
1020 FOR i=1 TO lenc STEP 5
1030 IF i$( TO 2)=c$(i TO i+1) THEN LET explen=VAL c$(i+2 TO i+3): GO TO 2000
1040 NEXT i
2000 LET match=explen>0
2010 REM Continue if country code found
2020 IF NOT match THEN LET e$="country code: "+i$( TO 2): GO TO 3000
2030 REM Remove espace = convert to digital IBAN
2040 LET d$=""
2050 FOR i=1 TO LEN i$
2060 IF i$(i)>" " THEN LET d$=d$+i$(i)
2070 NEXT i
2080 LET match=explen=LEN d$
2090 REM Continue if length is correct
2100 IF NOT match THEN LET e$="code length, expected length: "+STR$ explen: GO TO 3000
2110 REM Create temporary string with country code appended
2120 LET t$=d$(5 TO )+d$(1 TO 2)
2130 REM Make big number, replacing letters by numbers using next conversion table: A=10 ... Z=35
2140 LET b$=""
2150 FOR i=1 TO LEN t$
2160 LET c= CODE t$(i)
2170 IF (c>64 AND c<91) THEN LET b$=b$+STR$ (c-55): GO TO 2200
2190 IF (c>47 AND c<58) THEN LET b$=b$+t$(i)
2200 NEXT i
2210 REM MOD 97 on bignum$+"00" and subtract result from 98 to obtain control number
2220 LET k$=b$+"00": GO SUB 4000
2230 LET kk=98-mod97
2240 REM Compare with control number in IBAN
2250 LET match=VAL i$(3 TO 4)=kk
2260 REM Continue if control number matches
2270 IF NOT match THEN LET e$="check digits, should be: "+STR$ kk: GO TO 3000
2280 REM Append kk to bignum$ and determine if MOD 97 results in 1
2285 LET k$="0"+STR$ kk
2290 LET k$=b$+k$(LEN k$-1 TO ): GO SUB 4000
2300 LET match=mod97=1
2310 REM Continue if mod 97
2320 IF NOT match THEN LET e$="result from modulo 97"
3000 IF match THEN PRINT "  ": GO TO 3100
3010 PRINT "in";: LET e$=" ***error!*** invalid "+e$
3100 PRINT "valid IBAN: ";i$;e$: LET e$=""
3110 RETURN
4000 REM Modulo 97
4010 LET mod97=0
4020 FOR i=1 TO LEN k$
4030 LET d$=STR$ (mod97)+k$(i)
4040 LET mod97=FN m(VAL (d$),97)
4050 NEXT i
4060 RETURN
5000 DEF FN m(a,b)=a-INT (a/b)*b: REM modulo
```

