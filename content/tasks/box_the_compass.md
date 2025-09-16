+++
title = "Box the compass"
description = ""
date = 2019-09-24T02:58:40Z
aliases = []
[extra]
id = 9399
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "ada",
  "algol_68",
  "applescript",
  "autohotkey",
  "autoit",
  "awk",
  "bbc_basic",
  "befunge",
  "c",
  "clojure",
  "cobol",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "euphoria",
  "factor",
  "fortran",
  "freebasic",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "huginn",
  "j",
  "java",
  "javascript",
  "julia",
  "k",
  "kotlin",
  "lasso",
  "liberty_basic",
  "llvm",
  "logo",
  "lua",
  "m2000_interpreter",
  "mumps",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oorexx",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "seed7",
  "sidef",
  "smart_basic",
  "taskavast_me_hearties",
  "tcl",
  "ubasic_4th",
  "unix_shell",
  "vba",
  "visual_basic_.net",
  "yabasic",
  "zkl",
  "zx_spectrum_basic",
]
+++

## TaskAvast me hearties!

There be many a [http://talklikeapirate.com/wordpress/how-to/ land lubber] that knows [https://dictionary.cambridge.org/dictionary/english/naught naught] of the pirate ways and gives direction by degree!
They know not how to [[wp:Boxing the compass|box the compass]]!


;Task description:
# Create a function that takes a heading in degrees and returns the correct 32-point compass heading.
# Use the function to print and display a table of Index, Compass point, and Degree; rather like the corresponding columns from, the first table of the [[wp:Boxing the compass|wikipedia article]], but use only the following 33 headings as input:
:<code>[0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38]</code>. (They should give the same order of points but are spread throughout the ranges of acceptance).


;Notes;
* The headings and indices can be calculated from this pseudocode:

```txt
for i in 0..32 inclusive:
    heading = i * 11.25
    case i %3:
      if 1: heading += 5.62; break
      if 2: heading -= 5.62; break
    end
    index = ( i mod 32) + 1
```

* The column of indices can be thought of as an enumeration of the thirty two cardinal points (see [[Talk:Box the compass#Direction, index, and angle|talk page]])..





## 11l

{{trans|Python}}

```11l
V majors = ‘north east south west’.split(‘ ’)
majors *= 2
V quarter1 = ‘N,N by E,N-NE,NE by N,NE,NE by E,E-NE,E by N’.split(‘,’)
V quarter2 = quarter1.map(p -> p.replace(‘NE’, ‘EN’))

F degrees2compasspoint(=d)
   d = (d % 360) + 360 / 64
   V majorindex = Int(d / 90)
   V minorindex = Int((d % 90 * 4) I/ 45)
   V p1 = :majors[majorindex]
   V p2 = :majors[majorindex + 1]
   [String] q
   I p1 C (‘north’, ‘south’)
      q = :quarter1
   E
      q = :quarter2
   R q[minorindex].replace(‘N’, p1).replace(‘E’, p2).capitalize()

:start:
L(i) 33
   V d = i * 11.25
   S i % 3
      1
         d += 5.62
      2
         d -= 5.62
   V n = i % 32 + 1
   print(‘#2.0 #<18 #4.2°’.format(n, degrees2compasspoint(d), d))
```

{{out}}

```txt

 1 North                 0.00°
 2 North by east        16.87°
 3 North-northeast      16.88°
...
31 North-northwest     337.50°
32 North by west       354.37°
 1 North               354.38°

```



## Ada


Inspired by [[#C++|the C++ program]], but without the need for a specific library.


```Ada
with Ada.Text_IO;

procedure Box_The_Compass is

   type Degrees is digits 5 range 0.00 .. 359.99;
   type Index_Type is mod 32;

   function Long_Name(Short: String) return String is

      function Char_To_Name(Char: Character) return String is
      begin
         case Char is
            when 'N' | 'n' => return Char & "orth";
            when 'S' | 's' => return Char & "outh";
            when 'E' | 'e' => return Char & "ast";
            when 'W' | 'w' => return Char & "est";
            when 'b' => return " by ";
            when '-' => return "-";
            when others => raise Constraint_Error;
         end case;
      end Char_To_Name;

   begin
      if Short'Length = 0 or else Short(Short'First)=' ' then
         return "";
      else
         return Char_To_Name(Short(Short'First))
           & Long_Name(Short(Short'First+1 .. Short'Last));
      end if;
   end Long_Name;

   procedure Put_Line(Angle: Degrees) is

      function Index(D: Degrees) return Index_Type is
      begin
         return Index_Type(Integer(Degrees'Rounding(D/11.25)) mod 32);
      end Index;

      I: Integer := Integer(Index(Angle))+1;
      package DIO is new Ada.Text_IO.Float_IO(Degrees);
      Abbr: constant array(Index_Type) of String(1 .. 4)
        := ("N   ", "Nbe ", "N-ne", "Nebn", "Ne  ", "Nebe", "E-ne", "Ebn ",
            "E   ", "Ebs ", "E-se", "Sebe", "Se  ", "Sebs", "S-se", "Sbe ",
            "S   ", "Sbw ", "S-sw", "Swbs", "Sw  ", "Swbw", "W-sw", "Wbs ",
            "W   ", "Wbn ", "W-nw", "Nwbw", "Nw  ", "Nwbn", "N-nw", "Nbw ");

   begin
      DIO.Put(Angle, Fore => 3, Aft => 2, Exp => 0); -- format "zzx.xx"
      Ada.Text_IO.Put(" |");
      if I <= 9 then
         Ada.Text_IO.Put(" ");
      end if;
      Ada.Text_IO.Put_Line(" "  & Integer'Image(I) & " | "
                             & Long_Name(Abbr(Index(Angle))));
   end Put_Line;

   Difference: constant array(0..2) of Degrees'Base
     := (0=> 0.0, 1=> +5.62, 2=> - 5.62);

begin
   Ada.Text_IO.Put_Line(" angle | box | compass point");
   Ada.Text_IO.Put_Line(" ---------------------------------");
   for I in 0 .. 32 loop
      Put_Line(Degrees(Degrees'Base(I) * 11.25 + Difference(I mod 3)));
   end loop;
end Box_The_Compass;
```


Output:


```txt
 angle | box | compass point
 ---------------------------------
  0.00 |   1 | North
 16.87 |   2 | North by east
 16.88 |   3 | North-northeast
 33.75 |   4 | Northeast by north
 50.62 |   5 | Northeast
 50.63 |   6 | Northeast by east
 67.50 |   7 | East-northeast
 84.37 |   8 | East by north
 84.38 |   9 | East
101.25 |  10 | East by south
118.12 |  11 | East-southeast
118.13 |  12 | Southeast by east
135.00 |  13 | Southeast
151.87 |  14 | Southeast by south
151.88 |  15 | South-southeast
168.75 |  16 | South by east
185.62 |  17 | South
185.63 |  18 | South by west
202.50 |  19 | South-southwest
219.37 |  20 | Southwest by south
219.38 |  21 | Southwest
236.25 |  22 | Southwest by west
253.12 |  23 | West-southwest
253.13 |  24 | West by south
270.00 |  25 | West
286.87 |  26 | West by north
286.88 |  27 | West-northwest
303.75 |  28 | Northwest by west
320.62 |  29 | Northwest
320.63 |  30 | Northwest by north
337.50 |  31 | North-northwest
354.37 |  32 | North by west
354.38 |   1 | North
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}

```algol68
#!/usr/local/bin/a68g --script #

[]STRING
  long  by nesw = (" by ", "North", "East", "South", "West"),
  short by nesw = ("b", "N", "E", "S", "W");

MODE MINUTES = REAL; # minutes type #
INT last minute=360*60;
INT point width=last minute OVER 32;

PROC direction name = (REAL direction in minutes, []STRING locale directions)STRING: (

  STRING by = locale directions[1];
  []STRING nesw = locale directions[@-1];

  PRIO MASK = 7; # same PRIOrity as * and / #
  OP MASK = (INT n, BITS lower)INT: ABS (BIN n AND NOT lower),
     DECAP = (STRING s)STRING: IF UPB s > 1 THEN REPR (ABS s[1]-ABS "A"+ABS "a")+s[2:] ELSE s FI;

  PROC point name = (INT in point)STRING: (

    INT point = in point MOD 32 # 32 points of a compass #;
    IF point MOD 8 = 0 THEN
# name the principle point: eg. N, E, S or W #
      nesw[point OVER 8]

    ELIF point MOD 4 = 0 THEN
# name the half: eg. NE, SE, SW or NW #
      point name((point+8)MASK 2r1111)+DECAP point name(point MASK 2r1111 + 8)

    ELIF point MOD 2 = 0 THEN
# name the quarter: eg. N-NE, E-NE, E-SE, S-SE, S-SW, W-SW, W-NW or N-NW #
      point name((point+4)MASK 2r111)+"-"+point name(point MASK 2r111 + 4)

    ELSE # Name the sixteenth point: #
# eg. NbE,NEbN,NEbE,EbN,EbS,SEbE,SEbS,SbE,SbW,SWbS,SWbW,WbS,WbN,NWbW,NWbN,NbW #
      INT opp point = point OVER 8 + ABS NOT ODD (point OVER 2);
      point name((point+2)MASK 2r11)+ by +nesw(opp point MOD 4)
    FI
  );
  point name(ROUND(direction in minutes/point width))
);

PROC traditional name = (MINUTES minutes)STRING: (
  INT degrees = ROUND(minutes / 60);
  degrees=0  |"Tramontana"          |:
  degrees=45 |"Greco or Bora"       |:
  degrees=90 |"Levante"             |:
  degrees=135|"Sirocco"             |:
  degrees=180|"Ostro"               |:
  degrees=225|"Libeccio"            |:
  degrees=270|"Poniente or Zephyrus"|:
  degrees=315|"Mistral"             |:
  degrees=360|"Tramontana"          |""
);

# First generate the test set results #
test:(
  printf($"Test:"l$);
  FOR i FROM 0 TO 32 DO
    REAL heading = i * 11.25 +
      CASE i MOD 3 IN
        +5.62,
        -5.62
        OUT 0
      ESAC;
    INT index = ( i MOD 32) + 1;
    printf(($zd" ", g23k, zzd.zzl$, index , direction name(heading*60, long by nesw), heading))
  OD
);

table:(
  OP OVER = (REAL r, INT base)INT: ENTIER ABS r OVER  base,
     MOD = (REAL r, INT base)REAL: ( INT i = ENTIER r; i MOD base + r - i);

  printf(
    $l"Table:"l
    " #|Compass point"22k"|Abbr|Traditional wind point| Lowest°′ | Middle°′ | Highest°′"l$
  );

  OP DEGMIN = (REAL minutes)STRUCT(INT d, REAL m): (minutes MOD last minute OVER 60, minutes MOD 60);

  FOR point FROM 1 TO 32 DO
    REAL centre = (point-1) * point width;
    REAL from =  centre - point width/2,
         to   =  centre + point width/2-1/120;
    printf((
            $g(-2)"|"$, point,
            $g$, direction name(centre, long by nesw),
            $22k"|"g$, direction name(centre, short by nesw),
            $27k"|"g$, traditional name(centre),
            $50k$, $"|"g(-3)"°", dd.dd"′"$, DEGMIN from, DEGMIN centre, DEGMIN to,
            $l$
          ))
  OD
)
```

Output:

```txt

Test:
 1 North                0.00
 2 North by East       16.87
 3 North-Northeast     16.88
 4 Northeast by North  33.75
 5 Northeast           50.62
 6 Northeast by East   50.63
 7 East-Northeast      67.50
 8 East by North       84.37
 9 East                84.38
10 East by South      101.25
11 East-Southeast     118.12
12 Southeast by East  118.13
13 Southeast          135.00
14 Southeast by South 151.87
15 South-Southeast    151.88
16 South by East      168.75
17 South              185.62
18 South by West      185.63
19 South-Southwest    202.50
20 Southwest by South 219.37
21 Southwest          219.38
22 Southwest by West  236.25
23 West-Southwest     253.12
24 West by South      253.13
25 West               270.00
26 West by North      286.87
27 West-Northwest     286.88
28 Northwest by West  303.75
29 Northwest          320.62
30 Northwest by North 320.63
31 North-Northwest    337.50
32 North by West      354.37
 1 North              354.38

Table:
 #|Compass point     |Abbr|Traditional wind point| Lowest°′ | Middle°′ | Highest°′
 1|North             |N   |Tramontana            |354°22.50′|  0°00.00′|  5°37.49′
 2|North by East     |NbE |                      |  5°37.50′| 11°15.00′| 16°52.49′
 3|North-Northeast   |N-NE|                      | 16°52.50′| 22°30.00′| 28°07.49′
 4|Northeast by North|NEbN|                      | 28°07.50′| 33°45.00′| 39°22.49′
 5|Northeast         |NE  |Greco or Bora         | 39°22.50′| 45°00.00′| 50°37.49′
 6|Northeast by East |NEbE|                      | 50°37.50′| 56°15.00′| 61°52.49′
 7|East-Northeast    |E-NE|                      | 61°52.50′| 67°30.00′| 73°07.49′
 8|East by North     |EbN |                      | 73°07.50′| 78°45.00′| 84°22.49′
 9|East              |E   |Levante               | 84°22.50′| 90°00.00′| 95°37.49′
10|East by South     |EbS |                      | 95°37.50′|101°15.00′|106°52.49′
11|East-Southeast    |E-SE|                      |106°52.50′|112°30.00′|118°07.49′
12|Southeast by East |SEbE|                      |118°07.50′|123°45.00′|129°22.49′
13|Southeast         |SE  |Sirocco               |129°22.50′|135°00.00′|140°37.49′
14|Southeast by South|SEbS|                      |140°37.50′|146°15.00′|151°52.49′
15|South-Southeast   |S-SE|                      |151°52.50′|157°30.00′|163°07.49′
16|South by East     |SbE |                      |163°07.50′|168°45.00′|174°22.49′
17|South             |S   |Ostro                 |174°22.50′|180°00.00′|185°37.49′
18|South by West     |SbW |                      |185°37.50′|191°15.00′|196°52.49′
19|South-Southwest   |S-SW|                      |196°52.50′|202°30.00′|208°07.49′
20|Southwest by South|SWbS|                      |208°07.50′|213°45.00′|219°22.49′
21|Southwest         |SW  |Libeccio              |219°22.50′|225°00.00′|230°37.49′
22|Southwest by West |SWbW|                      |230°37.50′|236°15.00′|241°52.49′
23|West-Southwest    |W-SW|                      |241°52.50′|247°30.00′|253°07.49′
24|West by South     |WbS |                      |253°07.50′|258°45.00′|264°22.49′
25|West              |W   |Poniente or Zephyrus  |264°22.50′|270°00.00′|275°37.49′
26|West by North     |WbN |                      |275°37.50′|281°15.00′|286°52.49′
27|West-Northwest    |W-NW|                      |286°52.50′|292°30.00′|298°07.49′
28|Northwest by West |NWbW|                      |298°07.50′|303°45.00′|309°22.49′
29|Northwest         |NW  |Mistral               |309°22.50′|315°00.00′|320°37.49′
30|Northwest by North|NWbN|                      |320°37.50′|326°15.00′|331°52.49′
31|North-Northwest   |N-NW|                      |331°52.50′|337°30.00′|343°07.49′
32|North by West     |NbW |                      |343°07.50′|348°45.00′|354°22.49′

```



## AppleScript


OS X Yosemite onwards (uses Foundation classes for record handling etc)

{{Trans|JavaScript}}
(ES6 version)

Functional composition, allowing for additional languages, and different numbers of compass points – see the test section)


```AppleScript
use framework "Foundation"
use scripting additions

-- BOXING THE COMPASS --------------------------------------------------------

property plstLangs : [{|name|:"English"} & ¬
    {expansions:{N:"north", S:"south", E:"east", W:"west", b:" by "}} & ¬
    {|N|:"N", |NNNE|:"NbE", |NNE|:"N-NE", |NNENE|:"NEbN", |NE|:"NE", |NENEE|:"NEbE"} & ¬
    {|NEE|:"E-NE", |NEEE|:"EbN", |E|:"E", |EEES|:"EbS", |EES|:"E-SE", |EESES|:"SEbE"} & ¬
    {|ES|:"SE", |ESESS|:"SEbS", |ESS|:"S-SE", |ESSS|:"SbE", |S|:"S", |SSSW|:"SbW"} & ¬
    {|SSW|:"S-SW", |SSWSW|:"SWbS", |SW|:"SW", |SWSWW|:"SWbW", |SWW|:"W-SW"} & ¬
    {|SWWW|:"WbS", |W|:"W", |WWWN|:"WbN", |WWN|:"W-NW", |WWNWN|:"NWbW"} & ¬
    {|WN|:"NW", |WNWNN|:"NWbN", |WNN|:"N-NW", |WNNN|:"NbW"}, ¬
    ¬
        {|name|:"Chinese", |N|:"北", |NNNE|:"北微东", |NNE|:"东北偏北"} & ¬
    {|NNENE|:"东北微北", |NE|:"东北", |NENEE|:"东北微东", |NEE|:"东北偏东"} & ¬
    {|NEEE|:"东微北", |E|:"东", |EEES|:"东微南", |EES|:"东南偏东", |EESES|:"东南微东"} & ¬
    {|ES|:"东南", |ESESS|:"东南微南", |ESS|:"东南偏南", |ESSS|:"南微东", |S|:"南"} & ¬
    {|SSSW|:"南微西", |SSW|:"西南偏南", |SSWSW|:"西南微南", |SW|:"西南"} & ¬
    {|SWSWW|:"西南微西", |SWW|:"西南偏西", |SWWW|:"西微南", |W|:"西"} & ¬
    {|WWWN|:"西微北", |WWN|:"西北偏西", |WWNWN|:"西北微西", |WN|:"西北"} & ¬
    {|WNWNN|:"西北微北", |WNN|:"西北偏北", |WNNN|:"北微西"}]

--  Scale invariant keys for points of the compass
-- (allows us to look up a translation for one scale of compass (32 here)
-- for use in another size of compass (8 or 16 points)
-- (Also semi-serviceable as more or less legible keys without translation)

-- compassKeys :: Int -> [String]
on compassKeys(intDepth)
    -- Simplest compass divides into two hemispheres
    -- with one peak of ambiguity to the left,
    -- and one to the right  (encoded by the commas in this list):
    set urCompass to ["N", "S", "N"]

    -- Necessity drives recursive subdivision of broader directions, shrinking
    -- boxes down to a workable level of precision:
    script subdivision
        on |λ|(lstCompass, N)
            if N ≤ 1 then
                lstCompass
            else
                script subKeys
                    on |λ|(a, x, i, xs)
                        -- Borders between N and S engender E and W.
                        -- further subdivisions (boxes)
                        -- concatenate their two parent keys.
                        if i > 1 then
                            cond(N = intDepth, ¬
                                a & {cond(x = "N", "W", "E")} & x, ¬
                                a & {item (i - 1) of xs & x} & x)
                        else
                            a & x
                        end if
                    end |λ|
                end script

                |λ|(foldl(subKeys, {}, lstCompass), N - 1)
            end if
        end |λ|
    end script

    tell subdivision to items 1 thru -2 of |λ|(urCompass, intDepth)
end compassKeys

-- pointIndex :: Int -> Num -> String
on pointIndex(power, degrees)
    set nBoxes to 2 ^ power
    set i to round (degrees + (360 / (nBoxes * 2))) ¬
        mod 360 * nBoxes / 360 rounding up
    cond(i > 0, i, 1)
end pointIndex

-- pointNames :: Int -> Int -> [String]
on pointNames(precision, iBox)
    set k to item iBox of compassKeys(precision)

    script translation
        on |λ|(recLang)
            set maybeTrans to keyValue(recLang, k)
            set strBrief to cond(maybeTrans is missing value, k, maybeTrans)

            set recExpand to keyValue(recLang, "expansions")

            if recExpand is not missing value then
                script expand
                    on |λ|(c)
                        set t to keyValue(recExpand, c)
                        cond(t is not missing value, t, c)
                    end |λ|
                end script
                set strName to (intercalate(cond(precision > 5, " ", ""), ¬
                    map(expand, characters of strBrief)))
                toUpper(text item 1 of strName) & text items 2 thru -1 of strName
            else
                strBrief
            end if
        end |λ|
    end script

    map(translation, plstLangs)
end pointNames

-- maxLen :: [String] -> Int
on maxLen(xs)
    length of maximumBy(comparing(my |length|), xs)
end maxLen

-- alignRight :: Int -> String -> String
on alignRight(nWidth, x)
    justifyRight(nWidth, space, x)
end alignRight

-- alignLeft :: Int -> String -> String
on alignLeft(nWidth, x)
    justifyLeft(nWidth, space, x)
end alignLeft

-- show :: asString => a -> Text
on show(x)
    x as string
end show

-- compassTable :: Int -> [Num] -> Maybe String
on compassTable(precision, xs)
    if precision < 1 then
        missing value
    else
        set intPad to 2
        set rightAligned to curry(alignRight)
        set leftAligned to curry(alignLeft)
        set join to curry(my intercalate)

        -- INDEX COLUMN
        set lstIndex to map(|λ|(precision) of curry(pointIndex), xs)
        set lstStrIndex to map(show, lstIndex)
        set nIndexWidth to maxLen(lstStrIndex)
        set colIndex to map(|λ|(nIndexWidth + intPad) of rightAligned, lstStrIndex)

        -- ANGLES COLUMN
        script degreeFormat
            on |λ|(x)
                set {c, m} to splitOn(".", x as string)
                c & "." & (text 1 thru 2 of (m & "0")) & "°"
            end |λ|
        end script
        set lstAngles to map(degreeFormat, xs)
        set nAngleWidth to maxLen(lstAngles) + intPad
        set colAngles to map(|λ|(nAngleWidth) of rightAligned, lstAngles)

        -- NAMES COLUMNS
        script precisionNames
            on |λ|(iBox)
                pointNames(precision, iBox)
            end |λ|
        end script

        set lstTrans to transpose(map(precisionNames, lstIndex))
        set lstTransWidths to map(maxLen, lstTrans)

        script spacedNames
            on |λ|(lstLang, i)
                map(|λ|((item i of lstTransWidths) + 2) of leftAligned, lstLang)
            end |λ|
        end script

        set colsTrans to map(spacedNames, lstTrans)

        -- TABLE
        intercalate(linefeed, ¬
            map(|λ|("") of join, ¬
                transpose({colIndex} & {colAngles} & ¬
                    {replicate(length of lstIndex, "  ")} & colsTrans)))
    end if
end compassTable



-- TEST ----------------------------------------------------------------------
on run
    set xs to [0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, ¬
        84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, ¬
        185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, ¬
        270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, ¬
        354.38]

    --   If we supply other precisions, like 4 or 6, (2^n -> 16 or 64 boxes)
    --    the bearings will be divided amongst smaller or larger numbers of boxes,
    --    either using name translations retrieved by the generic hash
    --    or using the keys of the hash itself (combined with any expansions)
    --    to substitute for missing names for very finely divided boxes.

    compassTable(5, xs) -- // 2^5  -> 32 boxes
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- comparing :: (a -> b) -> (a -> a -> Ordering)
on comparing(f)
    set mf to mReturn(f)
    script
        on |λ|(a, b)
            set x to mf's |λ|(a)
            set y to mf's |λ|(b)
            if x < y then
                -1
            else
                if x > y then
                    1
                else
                    0
                end if
            end if
        end |λ|
    end script
end comparing

-- cond :: Bool -> a -> a -> a
on cond(bool, f, g)
    if bool then
        f
    else
        g
    end if
end cond

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

-- foldl :: (a -> b -> a) -> a -> [b] -> a
on foldl(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from 1 to lng
            set v to |λ|(v, item i of xs, i, xs)
        end repeat
        return v
    end tell
end foldl

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

-- justifyLeft :: Int -> Char -> Text -> Text
on justifyLeft(N, cFiller, strText)
    if N > length of strText then
        text 1 thru N of (strText & replicate(N, cFiller))
    else
        strText
    end if
end justifyLeft

-- justifyRight :: Int -> Char -> Text -> Text
on justifyRight(N, cFiller, strText)
    if N > length of strText then
        text -N thru -1 of ((replicate(N, cFiller) as text) & strText)
    else
        strText
    end if
end justifyRight

-- keyValue :: Record -> String -> Maybe String
on keyValue(rec, strKey)
    set ca to current application
    set v to (ca's NSDictionary's dictionaryWithDictionary:rec)'s objectForKey:strKey
    if v is not missing value then
        item 1 of ((ca's NSArray's arrayWithObject:v) as list)
    else
        missing value
    end if
end keyValue

-- length :: [a] -> Int
on |length|(xs)
    length of xs
end |length|

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

-- maximumBy :: (a -> a -> Ordering) -> [a] -> a
on maximumBy(f, xs)
    set cmp to mReturn(f)
    script max
        on |λ|(a, b)
            if a is missing value or cmp's |λ|(a, b) < 0 then
                b
            else
                a
            end if
        end |λ|
    end script

    foldl(max, missing value, xs)
end maximumBy

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

-- replicate :: Int -> a -> [a]
on replicate(N, a)
    set out to {}
    if N < 1 then return out
    set dbl to {a}

    repeat while (N > 1)
        if (N mod 2) > 0 then set out to out & dbl
        set N to (N div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- splitOn :: Text -> Text -> [Text]
on splitOn(strDelim, strMain)
    set {dlm, my text item delimiters} to {my text item delimiters, strDelim}
    set xs to text items of strMain
    set my text item delimiters to dlm
    return xs
end splitOn

-- transpose :: [[a]] -> [[a]]
on transpose(xss)
    script column
        on |λ|(_, iCol)
            script row
                on |λ|(xs)
                    item iCol of xs
                end |λ|
            end script

            map(row, xss)
        end |λ|
    end script

    map(column, item 1 of xss)
end transpose

-- toLower :: String -> String
on toLower(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        lowercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toLower

-- toTitle :: String -> String
on toTitle(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        capitalizedStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toTitle

-- toUpper :: String -> String
on toUpper(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        uppercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toUpper
```

{{Out}}

```txt
   1    0.00°  North               北
   2   16.87°  North by east       北微东
   3   16.88°  North-northeast     东北偏北
   4   33.75°  Northeast by north  东北微北
   5   50.62°  Northeast           东北
   6   50.63°  Northeast by east   东北微东
   7   67.50°  East-northeast      东北偏东
   8   84.37°  East by north       东微北
   9   84.38°  East                东
  10  101.25°  East by south       东微南
  11  118.12°  East-southeast      东南偏东
  12  118.13°  Southeast by east   东南微东
  13  135.00°  Southeast           东南
  14  151.87°  Southeast by south  东南微南
  15  151.88°  South-southeast     东南偏南
  16  168.75°  South by east       南微东
  17  185.62°  South               南
  18  185.63°  South by west       南微西
  19  202.50°  South-southwest     西南偏南
  20  219.37°  Southwest by south  西南微南
  21  219.38°  Southwest           西南
  22  236.25°  Southwest by west   西南微西
  23  253.12°  West-southwest      西南偏西
  24  253.13°  West by south       西微南
  25  270.00°  West                西
  26  286.87°  West by north       西微北
  27  286.88°  West-northwest      西北偏西
  28  303.75°  Northwest by west   西北微西
  29  320.62°  Northwest           西北
  30  320.63°  Northwest by north  西北微北
  31  337.50°  North-northwest     西北偏北
  32  354.37°  North by west       北微西
   1  354.38°  North               北
```



## AutoHotkey

{{trans|C++}}
{{works with|AutoHotkey_L}}

```AHK
get_Index(angle){
    return Mod(floor(angle / 11.25 +0.5), 32) + 1
}

get_Abbr_From_Index(i){
    static points
       := [ "N", "NbE", "NNE", "NEbN", "NE", "NEbE", "ENE", "EbN"
           ,"E", "EbS", "ESE", "SEbE", "SE", "SEbS", "SSE", "SbE"
           ,"S", "SbW", "SSW", "SWbS", "SW", "SWbW", "WSW", "WbS"
           ,"W", "WbN", "WNW", "NWbW", "NW", "NWbN", "NNW", "NbW" ]
    return points[i]
}

Build_Name_From_Abbr(a){
    Loop Parse, a
    {
        i := A_Index
        if ((i = 2) && (SubStr(a, i, 1) != "b") && (StrLen(a) == 3))
            retval .= "-"
        retval .= {N: "north", S: "south", E: "east"
                 , W: "west" , b: " by "}[A_LoopField]
    }
    return Chr(Asc(SubStr(retval, 1, 1))-32) . SubStr(retval, 2)
}

; test

headings:= [0.00, 16.87, 16.88, 33.75, 50.62, 50.63, 67.50, 84.37, 84.38, 101.25
          , 118.12, 118.13, 135.00, 151.87, 151.88, 168.75, 185.62, 185.63
          , 202.50, 219.37, 219.38, 236.25, 253.12, 253.13, 270.00, 286.87
          , 286.88, 303.75, 320.62, 320.63, 337.50, 354.37, 354.38]
For n, a in headings
{
    i := get_Index(a)
    out .= SubStr(" " i, -1) " "
        . SubStr(Build_Name_From_Abbr(get_Abbr_From_Index(i))
        . "                    ", 1, 24) . SubStr("  " a, -5)  . "`r`n" ;
}
clipboard := out
```

;Output

```txt
 1 North                     0.00
 2 North by east            16.87
 3 North-northeast          16.88
 4 Northeast by north       33.75
 5 Northeast                50.62
 6 Northeast by east        50.63
 7 East-northeast           67.50
 8 East by north            84.37
 9 East                     84.38
10 East by south           101.25
11 East-southeast          118.12
12 Southeast by east       118.13
13 Southeast               135.00
14 Southeast by south      151.87
15 South-southeast         151.88
16 South by east           168.75
17 South                   185.62
18 South by west           185.63
19 South-southwest         202.50
20 Southwest by south      219.37
21 Southwest               219.38
22 Southwest by west       236.25
23 West-southwest          253.12
24 West by south           253.13
25 West                    270.00
26 West by north           286.87
27 West-northwest          286.88
28 Northwest by west       303.75
29 Northwest               320.62
30 Northwest by north      320.63
31 North-northwest         337.50
32 North by west           354.37
 1 North                   354.38
```



## AutoIt



```AutoIt

Local $avArray[33] = [0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, _
		151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, _
		303.75, 320.62, 320.63, 337.5, 354.37, 354.38]

For $i = 0 To UBound($avArray) - 1
	Boxing_the_compass($avArray[$i])
Next

Func Boxing_the_compass($Degree)
	Local $namearray[33] = ["North", "North by east", "North-northeast", "Northeast by north", "Northeast", _
			"Northeast by east", "East-northeast", "East by north", "East", "East by south", "East-southeast", _
			"Southeast by east", "Southeast", "Southeast by south", "South-southeast", "South by east", "South", _
			"South by west", "South-southwest", "Southwest by south", "Southwest", "Southwest by west", "West-southwest", _
			"West by south", "West", "West by north", "West-northwest", "Northwest by west", "Northwest", "Northwest by north", _
			"North-northwest", "North by west", "North"]
	ConsoleWrite(StringFormat("%-2s", Mod(Floor($Degree / 11.25 + 0.5), 32)) & " : " & _
			StringFormat("%-20s", $namearray[Mod(Floor($Degree / 11.25 + 0.5), 32)]) & " : " & $Degree & @CRLF)
EndFunc   ;==>Boxing_the_compass

```


Output :
```txt
0  : North                : 0
1  : North by east        : 16.87
2  : North-northeast      : 16.88
3  : Northeast by north   : 33.75
4  : Northeast            : 50.62
5  : Northeast by east    : 50.63
6  : East-northeast       : 67.5
7  : East by north        : 84.37
8  : East                 : 84.38
9  : East by south        : 101.25
10 : East-southeast       : 118.12
11 : Southeast by east    : 118.13
12 : Southeast            : 135
13 : Southeast by south   : 151.87
14 : South-southeast      : 151.88
15 : South by east        : 168.75
16 : South                : 185.62
17 : South by west        : 185.63
18 : South-southwest      : 202.5
19 : Southwest by south   : 219.37
20 : Southwest            : 219.38
21 : Southwest by west    : 236.25
22 : West-southwest       : 253.12
23 : West by south        : 253.13
24 : West                 : 270
25 : West by north        : 286.87
26 : West-northwest       : 286.88
27 : Northwest by west    : 303.75
28 : Northwest            : 320.62
29 : Northwest by north   : 320.63
30 : North-northwest      : 337.5
31 : North by west        : 354.37
0  : North                : 354.38
```



## AWK


```awk
#!/usr/bin/awk -f
BEGIN {
  split("N NbE NNE NEbN NE NEbE ENE EbN E EbS ESE SEbE SE SEbS SSE SbE S SbW SSW SWbS SW SWbW WSW WbS W WbN WNW NWbW NW NWbN NNW NbW",A," ");
}

function ceil(x) {
	y = int(x)
	return y < x ? y + 1 : y
}

function compassbox(d) {
    return ceil( ( (d + 360 / 64) % 360) * 32 / 360);
}

{
    box = compassbox($1);
    printf "%6.2f : %2d\t%s\n",$1,box,A[box];
}

```

Output:

```txt
  0.00 :  1	N
 16.87 :  2	NbE
 16.88 :  3	NNE
 33.75 :  4	NEbN
 50.62 :  5	NE
 50.63 :  6	NEbE
 67.50 :  7	ENE
 84.37 :  8	EbN
 84.38 :  9	E
101.25 : 10	EbS
118.12 : 11	ESE
118.13 : 12	SEbE
135.00 : 13	SE
151.87 : 14	SEbS
151.88 : 15	SSE
168.75 : 16	SbE
185.62 : 17	S
185.63 : 18	SbW
202.50 : 19	SSW
219.37 : 20	SWbS
219.38 : 21	SW
236.25 : 22	SWbW
253.12 : 23	WSW
253.13 : 24	WbS
270.00 : 25	W
286.87 : 26	WbN
286.88 : 27	WNW
303.75 : 28	NWbW
320.62 : 29	NW
320.63 : 30	NWbN
337.50 : 31	NNW
354.37 : 32	NbW
354.38 :  1	N
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      DIM bearing(32)
      bearing() = 0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, \
      \ 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, \
      \ 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, \
      \ 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38

      FOR i% = 0 TO 32
        box% = FNcompassbox(bearing(i%), compass$)
        PRINT ; bearing(i%), ; box%, compass$
      NEXT
      END

      DEF FNcompassbox(bearing, RETURN box$)
      LOCAL pt%
      pt% = INT(bearing / 360 * 32 + 0.5) MOD 32
      box$ = FNpt(pt%)
      LEFT$(box$,1) = CHR$(ASC(LEFT$(box$,1))-32)
      = pt% + 1

      DEF FNpt(pt%)
      LOCAL pt$() : DIM pt$(3)
      IF pt% AND 1 THEN = FNpt((pt% + 1) AND 28) + " by " + \
      \                   FNpt(((2 - (pt% AND 2)) * 4) + pt% AND 24)
      IF pt% AND 2 THEN = FNpt((pt% + 2) AND 24) + "-" + FNpt((pt% OR 4) AND 28)
      IF pt% AND 4 THEN = FNpt((pt% + 8) AND 16) + FNpt((pt% OR 8) AND 24)
      pt$() = "north", "east", "south", "west"
      = pt$(pt% DIV 8)

```

Output:

```txt
0         1         North
16.87     2         North by east
16.88     3         North-northeast
33.75     4         Northeast by north
50.62     5         Northeast
50.63     6         Northeast by east
67.5      7         East-northeast
84.37     8         East by north
84.38     9         East
101.25    10        East by south
118.12    11        East-southeast
118.13    12        Southeast by east
135       13        Southeast
151.87    14        Southeast by south
151.88    15        South-southeast
168.75    16        South by east
185.62    17        South
185.63    18        South by west
202.5     19        South-southwest
219.37    20        Southwest by south
219.38    21        Southwest
236.25    22        Southwest by west
253.12    23        West-southwest
253.13    24        West by south
270       25        West
286.87    26        West by north
286.88    27        West-northwest
303.75    28        Northwest by west
320.62    29        Northwest
320.63    30        Northwest by north
337.5     31        North-northwest
354.37    32        North by west
354.38    1         North
```



## Befunge


```befunge
>
::"}"9**\4+3%79*9*5-*79*9*5--+:5>>>06p:55+%68*+v
^_@#!`*84:+1<v*9"}"*+55,,,".",,,$$_^#!:-1g60/+55\<
>_06g:v>55+,^>/5+55+/48*::::,,,,%:1+.9,:06p48*\-0v
|p60-1<|<!p80:<N|Ev"northwest"0"North by west"0p8<
>:#,_>>>_08g1-^W|S>"-htroN"0"htron yb tsewhtroN"0v
#v"est-northwest"0"Northwest by west"0"Northwest"<
N>"W"0"htron yb tseW"0"tseW"0"htuos yb tseW"0"ts"v
#v0"Southwest"0"Southwest by west"0"West-southwe"<
S>"htuos yb tsewhtuoS"0"tsewhtuos-htuoS"0"tsew y"v
#v"h-southeast"0"South by east"0"South"0"South b"<
E>"tuoS"0"htuos yb tsaehtuoS"0"tsaehtuoS"0"tsae "v
#v"East by south"0"East-southeast"0"Southeast by"<
W>0"tsaE"0"htron yb tsaE"0"tsaehtron-tsaE"0"tsae"v
#v"rtheast by north"0"Northeast"0"Northeast by "<<
^>"oN"0"tsaehtron-htroN"0"tsae yb htroN"0"htroN"01
```


{{output}}

```txt
000.00    1     North
016.87    2     North by east
016.88    3     North-northeast
033.75    4     Northeast by north
050.62    5     Northeast
050.63    6     Northeast by east
067.50    7     East-northeast
084.37    8     East by north
084.38    9     East
101.25    10    East by south
118.12    11    East-southeast
118.13    12    Southeast by east
135.00    13    Southeast
151.87    14    Southeast by south
151.88    15    South-southeast
168.75    16    South by east
185.62    17    South
185.63    18    South by west
202.50    19    South-southwest
219.37    20    Southwest by south
219.38    21    Southwest
236.25    22    Southwest by west
253.12    23    West-southwest
253.13    24    West by south
270.00    25    West
286.87    26    West by north
286.88    27    West-northwest
303.75    28    Northwest by west
320.62    29    Northwest
320.63    30    Northwest by north
337.50    31    North-northwest
354.37    32    North by west
354.38    1     North
```



## C

Like [[wp:Box the compass|Wikipedia's article]], this program uses indexes to count the headings. There are now 33 headings, from 1 to 33, because 0.0 and 354.38 are different angles. (This differs from the task pseudocode, which mapped the 32 compass points to indexes.)


```c
#include <stdio.h>

int main()
{
        int i, j;
        double degrees[] = { 0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5,
                84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88,
                168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12,
                253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5,
                354.37, 354.38 };
        const char * names =  "North                 "
                              "North by east         "
                              "North-northeast       "
                              "Northeast by north    "
                              "Northeast             "
                              "Northeast by east     "
                              "East-northeast        "
                              "East by north         "
                              "East                  "
                              "East by south         "
                              "East-southeast        "
                              "Southeast by east     "
                              "Southeast             "
                              "Southeast by south    "
                              "South-southeast       "
                              "South by east         "
                              "South                 "
                              "South by west         "
                              "South-southwest       "
                              "Southwest by south    "
                              "Southwest             "
                              "Southwest by west     "
                              "West-southwest        "
                              "West by south         "
                              "West                  "
                              "West by north         "
                              "West-northwest        "
                              "Northwest by west     "
                              "Northwest             "
                              "Northwest by north    "
                              "North-northwest       "
                              "North by west         "
                              "North                 ";

        for (i = 0; i < 33; i++) {
                j = .5 + degrees[i] * 32 / 360;

                printf("%2d  %.22s  %6.2f\n", (j % 32) + 1, names + (j % 32) * 22,
                        degrees[i]);
        }

        return 0;
}
```
Output:

```txt

 1  North                     0.00
 2  North by east            16.87
 3  North-northeast          16.88
 4  Northeast by north       33.75
 5  Northeast                50.62
 6  Northeast by east        50.63
 7  East-northeast           67.50
 8  East by north            84.37
 9  East                     84.38
10  East by south           101.25
11  East-southeast          118.12
12  Southeast by east       118.13
13  Southeast               135.00
14  Southeast by south      151.87
15  South-southeast         151.88
16  South by east           168.75
17  South                   185.62
18  South by west           185.63
19  South-southwest         202.50
20  Southwest by south      219.37
21  Southwest               219.38
22  Southwest by west       236.25
23  West-southwest          253.12
24  West by south           253.13
25  West                    270.00
26  West by north           286.87
27  West-northwest          286.88
28  Northwest by west       303.75
29  Northwest               320.62
30  Northwest by north      320.63
31  North-northwest         337.50
32  North by west           354.37
 1  North                   354.38

```



## C++

Using the Boost libraries
{{libheader|Boost}}

```cpp
#include <string>
#include <boost/array.hpp>
#include <boost/assign/list_of.hpp>
#include <boost/format.hpp>
#include <boost/foreach.hpp>
#include <iostream>
#include <math.h>
using std::string;
using namespace boost::assign;

int get_Index(float angle)
{
   return static_cast<int>(floor(angle / 11.25 +0.5 )) % 32 + 1;
}

string get_Abbr_From_Index(int i)
{
    static boost::array<std::string, 32> points(list_of
            ("N")("NbE")("NNE")("NEbN")("NE")("NEbE")("ENE")("EbN")
            ("E")("EbS")("ESE")("SEbE")("SE")("SEbS")("SSE")("SbE")
            ("S")("SbW")("SSW")("SWbS")("SW")("SWbW")("WSW")("WbS")
            ("W")("WbN")("WNW")("NWbW")("NW")("NWbN")("NNW")("NbW"));
    return points[i-1];
}

string Build_Name_From_Abbreviation(string a)
{
    string retval;
    for (int i = 0; i < a.size(); ++i){
        if ((1 == i) && (a[i] != 'b') && (a.size() == 3)) retval += "-";
        switch (a[i]){
            case 'N' : retval += "north"; break;
            case 'S' : retval += "south"; break;
            case 'E' : retval += "east";  break;
            case 'W' : retval += "west";  break;
            case 'b' : retval += " by ";  break;
        }
    }
    retval[0] = toupper(retval[0]);
    return retval;
}

int main()
{
    boost::array<float,33> headings(list_of
            (0.0)(16.87)(16.88)(33.75)(50.62)(50.63)(67.5)(84.37)(84.38)(101.25)
            (118.12)(118.13)(135.0)(151.87)(151.88)(168.75)(185.62)(185.63)(202.5)
            (219.37)(219.38)(236.25)(253.12)(253.13)(270.0)(286.87)(286.88)(303.75)
            (320.62)(320.63)(337.5)(354.37)(354.38));
    int i;
    boost::format f("%1$4d %2$-20s %3$_7.2f");

    BOOST_FOREACH(float a, headings)
    {
        i = get_Index(a);
        std::cout << f % i %  Build_Name_From_Abbreviation(get_Abbr_From_Index(i)) % a << std::endl;
    }
    return 0;
}
```

Output:

```txt

   1 North                   0.00
   2 North by east          16.87
   3 North-northeast        16.88
   4 Northeast by north     33.75
   5 Northeast              50.62
   6 Northeast by east      50.63
   7 East-northeast         67.50
   8 East by north          84.37
   9 East                   84.38
  10 East by south         101.25
  11 East-southeast        118.12
  12 Southeast by east     118.13
  13 Southeast             135.00
  14 Southeast by south    151.87
  15 South-southeast       151.88
  16 South by east         168.75
  17 South                 185.62
  18 South by west         185.63
  19 South-southwest       202.50
  20 Southwest by south    219.37
  21 Southwest             219.38
  22 Southwest by west     236.25
  23 West-southwest        253.12
  24 West by south         253.13
  25 West                  270.00
  26 West by north         286.87
  27 West-northwest        286.88
  28 Northwest by west     303.75
  29 Northwest             320.62
  30 Northwest by north    320.63
  31 North-northwest       337.50
  32 North by west         354.37
   1 North                 354.38

```


## C#

```c#

using System;
using System.Collections.Generic;

namespace BoxTheCompass
{
    class Compass
    {
        string[] cp = new string[] {"North", "North by east", "North-northeast", "Northeast by north", "Northeast","Northeast by east",
	    "East-northeast", "East by north", "East", "East by south", "East-southeast", "Southeast by east", "Southeast",
            "Southeast by south", "South-southeast", "South by east", "South", "South by west", "South-southwest", "Southwest by south",
            "Southwest", "Southwest by west", "West-southwest", "West by south", "West", "West by north", "West-northwest",
            "Northwest by west", "Northwest", "Northwest by north", "North-northwest", "North by west", "North"};

        public void compassHeading(float a)
        {
            int h = Convert.ToInt32(Math.Floor(a / 11.25f + .5f)) % 32;
            Console.WriteLine( "{0,2}: {1,-22} : {2,6:N}",h + 1, cp[h], a );
        }
    };
    class Program
    {
        static void Main(string[] args)
       {
            Compass c = new Compass();
            float[] degs = new float[] {0.0f, 16.87f, 16.88f, 33.75f, 50.62f, 50.63f, 67.5f, 84.37f, 84.38f, 101.25f,
                118.12f, 118.13f, 135.0f, 151.87f, 151.88f, 168.75f, 185.62f, 185.63f, 202.5f, 219.37f, 219.38f, 236.25f,
                253.12f, 253.13f, 270.0f, 286.87f, 286.88f, 303.75f, 320.62f, 320.63f, 337.5f, 354.37f, 354.38f};

            foreach (float d in degs)
                c.compassHeading(d);

            Console.WriteLine("\nPress any key to continue...");
            Console.ReadKey();
        }
    }
}

```

{{out}}

```txt

 1: North                  :   0.00
 2: North by east          :  16.87
 3: North-northeast        :  16.88
 4: Northeast by north     :  33.75
 5: Northeast              :  50.62
 6: Northeast by east      :  50.63
 7: East-northeast         :  67.50
 8: East by north          :  84.37
 9: East                   :  84.38
10: East by south          : 101.25
11: East-southeast         : 118.12
12: Southeast by east      : 118.13
13: Southeast              : 135.00
14: Southeast by south     : 151.87
15: South-southeast        : 151.88
16: South by east          : 168.75
17: South                  : 185.62
18: South by west          : 185.63
19: South-southwest        : 202.50
20: Southwest by south     : 219.37
21: Southwest              : 219.38
22: Southwest by west      : 236.25
23: West-southwest         : 253.12
24: West by south          : 253.13
25: West                   : 270.00
26: West by north          : 286.87
27: West-northwest         : 286.88
28: Northwest by west      : 303.75
29: Northwest              : 320.62
30: Northwest by north     : 320.63
31: North-northwest        : 337.50
32: North by west          : 354.37
 1: North                  : 354.38

```



## Clojure

{{trans|Tcl}}

```lisp
(ns boxing-the-compass
  (:use [clojure.string :only [capitalize]]))

(def headings
     (for [i (range 0 (inc 32))]
       (let [heading (* i 11.25)]
	 (case (mod i 3)
	       1 (+ heading 5.62)
	       2 (- heading 5.62)
	       heading))))

(defn angle2compass
  [angle]
  (let [dirs ["N" "NbE" "N-NE" "NEbN" "NE" "NEbE" "E-NE" "EbN"
	      "E" "EbS" "E-SE" "SEbE" "SE" "SEbS" "S-SE" "SbE"
	      "S" "SbW" "S-SW" "SWbS" "SW" "SWbW" "W-SW" "WbS"
	      "W" "WbN" "W-NW" "NWbW" "NW" "NWbN" "N-NW" "NbW"]
	unpack {\N "north" \E "east" \W "west" \S "south" \b " by " \- "-"}
	sep  (/ 360 (count dirs))
	dir  (int (/ (mod (+ angle (/ sep 2)) 360) sep))]
    (capitalize (apply str (map unpack (dirs dir))))))

(print
 (apply str (map-indexed #(format "%2s %-18s %7.2f\n"
				  (inc (mod %1 32)) (angle2compass %2) %2)
			 headings)))
```

Output:

```txt
 1 North                 0.00
 2 North by east        16.87
 3 North-northeast      16.88
 4 Northeast by north   33.75
 5 Northeast            50.62
 6 Northeast by east    50.63
 7 East-northeast       67.50
 8 East by north        84.37
 9 East                 84.38
10 East by south       101.25
11 East-southeast      118.12
12 Southeast by east   118.13
13 Southeast           135.00
14 Southeast by south  151.87
15 South-southeast     151.88
16 South by east       168.75
17 South               185.62
18 South by west       185.63
19 South-southwest     202.50
20 Southwest by south  219.37
21 Southwest           219.38
22 Southwest by west   236.25
23 West-southwest      253.12
24 West by south       253.13
25 West                270.00
26 West by north       286.87
27 West-northwest      286.88
28 Northwest by west   303.75
29 Northwest           320.62
30 Northwest by north  320.63
31 North-northwest     337.50
32 North by west       354.37
 1 North               354.38
```



## COBOL

Works with GnuCOBOL

```cobol
       identification division.
       program-id. box-compass.
       data division.
       working-storage section.
       01 point                pic 99.
       01 degrees              usage float-short.
       01 degrees-rounded      pic 999v99.
       01 show-degrees         pic zz9.99.
       01 box                  pic z9.
       01 fudge                pic 9.
       01 compass              pic x(4).
       01 compass-point        pic x(18).
       01 shortform            pic x.
       01 short-names.
          05 short-name        pic x(4) occurs 33 times.
       01 overlay.
          05 value "N   " & "NbE " & "N-NE" & "NEbN" & "NE  " &
                   "NEbE" & "E-NE" & "EbN " & "E   " & "EbS " &
                   "E-SE" & "SEbE" & "SE  " & "SEbS" & "S-SE" &
                   "SbE " & "S   " & "SbW " & "S-SW" & "SWbS" &
                   "SW  " & "SWbW" & "W-SW" & "WbS " & "W   " &
                   "WbN " & "W-NW" & "NWbW" & "NW  " & "NWbN" &
                   "N-NW" & "NbW " & "N   ".

       procedure division.
       display "Index Compass point      Degree"

       move overlay to short-names.
       perform varying point from 0 by 1 until point > 32
           compute box = function mod(point 32) + 1
           compute degrees = point * 11.25
           compute fudge = function mod(point 3)
           evaluate fudge
              when equal 1
                  add 5.62 to degrees
              when equal 2
                  subtract 5.62 from degrees
           end-evaluate

           compute degrees-rounded rounded = degrees
           move degrees-rounded to show-degrees
           inspect show-degrees replacing trailing '00' by '0 '
           inspect show-degrees replacing trailing '50' by '5 '

           move short-name(point + 1) to compass
           move spaces to compass-point
           display space box space space space with no advancing
           perform varying tally from 1 by 1 until tally > 4
               move compass(tally:1) to shortform
               move function concatenate(function trim(compass-point),
                    function substitute(shortform,
                        "N", "North",
                        "E", "East",
                        "S", "South",
                        "W", "West",
                        "b", " byZ",
                        "-", "-"))
                 to compass-point
           end-perform
           move function substitute(compass-point, "Z", " ")
             to compass-point
           move function lower-case(compass-point) to compass-point
           move function upper-case(compass-point(1:1))
             to compass-point(1:1)
           display compass-point space show-degrees
       end-perform
       goback.
```


{{out}}

```txt

Index Compass point      Degree
  1   North                0.0
  2   North by east       16.87
  3   North-northeast     16.88
  4   Northeast by north  33.75
  5   Northeast           50.62
  6   Northeast by east   50.63
  7   East-northeast      67.5
  8   East by north       84.37
  9   East                84.38
 10   East by south      101.25
 11   East-southeast     118.12
 12   Southeast by east  118.13
 13   Southeast          135.0
 14   Southeast by south 151.87
 15   South-southeast    151.88
 16   South by east      168.75
 17   South              185.62
 18   South by west      185.63
 19   South-southwest    202.5
 20   Southwest by south 219.37
 21   Southwest          219.38
 22   Southwest by west  236.25
 23   West-southwest     253.12
 24   West by south      253.13
 25   West               270.0
 26   West by north      286.87
 27   West-northwest     286.88
 28   Northwest by west  303.75
 29   Northwest          320.62
 30   Northwest by north 320.63
 31   North-northwest    337.5
 32   North by west      354.37
  1   North              354.38

```



## D

{{trans|Java}}

```d
import std.stdio, std.string, std.math, std.array;

struct boxTheCompass {
    immutable static string[32] points;

    pure nothrow static this() {
        immutable cardinal = ["north", "east", "south", "west"];
        immutable desc = ["1", "1 by 2", "1-C", "C by 1", "C",
                          "C by 2", "2-C", "2 by 1"];

        foreach (immutable i; 0 .. 4) {
            immutable s1 = cardinal[i];
            immutable s2 = cardinal[(i + 1) % 4];
            immutable sc = (s1 == "north" || s1 == "south") ?
                           (s1 ~ s2) : (s2 ~ s1);
            foreach (immutable j; 0 .. 8)
                points[i * 8 + j] = desc[j].replace("1", s1).
                                    replace("2", s2).replace("C", sc);
        }
    }

    static string opCall(in double degrees) pure /*nothrow*/ {
        immutable testD = (degrees / 11.25) + 0.5;
        return points[cast(int)floor(testD % 32)].capitalize;
    }
}

void main() {
    foreach (immutable i; 0 .. 33) {
        immutable heading = i * 11.25 + [0, 5.62, -5.62][i % 3];
        writefln("%s\t%18s\t%s", i % 32 + 1,
                 heading.boxTheCompass, heading);
    }
}
```

{{out}}

```txt
1                North  0
2        North by east  16.87
3      North-northeast  16.88
4   Northeast by north  33.75
5            Northeast  50.62
6    Northeast by east  50.63
7       East-northeast  67.5
8        East by north  84.37
9                 East  84.38
10       East by south  101.25
11      East-southeast  118.12
12   Southeast by east  118.13
13           Southeast  135
14  Southeast by south  151.87
15     South-southeast  151.88
16       South by east  168.75
17               South  185.62
18       South by west  185.63
19     South-southwest  202.5
20  Southwest by south  219.37
21           Southwest  219.38
22   Southwest by west  236.25
23      West-southwest  253.12
24       West by south  253.13
25                West  270
26       West by north  286.87
27      West-northwest  286.88
28   Northwest by west  303.75
29           Northwest  320.62
30  Northwest by north  320.63
31     North-northwest  337.5
32       North by west  354.37
1                North  354.38
```



### Alternative version


```d
void main() {
    import std.stdio;

    immutable box = [
        "North", "North by east", "North-northeast", "Northeast by north",
        "Northeast", "Northeast by east", "East-northeast", "East by north",
        "East", "East by south", "East-southeast", "Southeast by east",
        "Southeast", "Southeast by south", "South-southeast", "South by east",
        "South", "South by west", "South-southwest", "Southwest by south",
        "Southwest", "Southwest by west", "West-southwest", "West by south",
        "West", "West by north", "West-northwest", "Northwest by west",
        "Northwest", "Northwest by north", "North-northwest", "North by west"];

    immutable angles = [
        0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38,
        101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62,
        185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0,
        286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38];

    foreach (immutable phi; angles) {
        immutable i = (cast(int)(phi * 32.0 / 360.0 + 0.5)) % 32;
        writefln("%2d %18s %6.2f", i + 1, box[i], phi);
    }
}
```

{{out}}

```txt
 1              North   0.00
 2      North by east  16.87
 3    North-northeast  16.88
 4 Northeast by north  33.75
 5          Northeast  50.62
 6  Northeast by east  50.63
 7     East-northeast  67.50
 8      East by north  84.37
 9               East  84.38
10      East by south 101.25
11     East-southeast 118.12
12  Southeast by east 118.13
13          Southeast 135.00
14 Southeast by south 151.87
15    South-southeast 151.88
16      South by east 168.75
17              South 185.62
18      South by west 185.63
19    South-southwest 202.50
20 Southwest by south 219.37
21          Southwest 219.38
22  Southwest by west 236.25
23     West-southwest 253.12
24      West by south 253.13
25               West 270.00
26      West by north 286.87
27     West-northwest 286.88
28  Northwest by west 303.75
29          Northwest 320.62
30 Northwest by north 320.63
31    North-northwest 337.50
32      North by west 354.37
 1              North 354.38
```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Box do
  defp head do
    Enum.chunk(~w(north east south west north), 2, 1)
    |> Enum.flat_map(fn [a,b] ->
         c = if a=="north" or a=="south", do: "#{a}#{b}", else: "#{b}#{a}"
         [ a, "#{a} by #{b}", "#{a}-#{c}", "#{c} by #{a}",
           c, "#{c} by #{b}", "#{b}-#{c}", "#{b} by #{a}" ]
       end)
    |> Enum.with_index
    |> Enum.map(fn {s, i} -> {i+1, String.capitalize(s)} end)
    |> Map.new
  end

  def compass do
    header = head()
    angles = Enum.map(0..32, fn i -> i * 11.25 + elem({0, 5.62, -5.62}, rem(i, 3)) end)
    Enum.each(angles, fn degrees ->
      index = rem(round(32 * degrees / 360), 32) + 1
      :io.format "~2w  ~-20s ~6.2f~n", [index, header[index], degrees]
    end)
  end
end

Box.compass
```


{{out}}

```txt

 1  North                  0.00
 2  North by east         16.87
 3  North-northeast       16.88
 4  Northeast by north    33.75
 5  Northeast             50.62
 6  Northeast by east     50.63
 7  East-northeast        67.50
 8  East by north         84.37
 9  East                  84.38
10  East by south        101.25
11  East-southeast       118.12
12  Southeast by east    118.13
13  Southeast            135.00
14  Southeast by south   151.87
15  South-southeast      151.88
16  South by east        168.75
17  South                185.62
18  South by west        185.63
19  South-southwest      202.50
20  Southwest by south   219.37
21  Southwest            219.38
22  Southwest by west    236.25
23  West-southwest       253.12
24  West by south        253.13
25  West                 270.00
26  West by north        286.87
27  West-northwest       286.88
28  Northwest by west    303.75
29  Northwest            320.62
30  Northwest by north   320.63
31  North-northwest      337.50
32  North by west        354.37
 1  North                354.38

```



## Euphoria


```euphoria
constant names = {"North","North by east","North-northeast","Northeast by north",
    "Northeast","Northeast by east","East-northeast","East by north","East",
    "East by south","East-southeast","Southeast by east","Southeast","Southeast by south",
    "South-southeast","South by east","South","South by west","South-southwest",
    "Southwest by south","Southwest","Southwest by west","West-southwest",
    "West by south","West","West by north","West-northwest","Northwest by west",
    "Northwest","Northwest by north","North-northwest","North by west"}

function deg2ind(atom degree)
    return remainder(floor(degree*32/360+.5),32)+1
end function

sequence degrees
degrees = {}
for i = 0 to 32 do
    degrees &= i*11.25 + 5.62*(remainder(i+1,3)-1)
end for

integer j
for i = 1 to length(degrees) do
    j = deg2ind(degrees[i])
    printf(1, "%6.2f  %2d  %-22s\n", {degrees[i], j, names[j]})
end for
```


Output:

```txt
  0.00   1  North
 16.87   2  North by east
 16.88   3  North-northeast
 33.75   4  Northeast by north
 50.62   5  Northeast
 50.63   6  Northeast by east
 67.50   7  East-northeast
 84.37   8  East by north
 84.38   9  East
101.25  10  East by south
118.12  11  East-southeast
118.13  12  Southeast by east
135.00  13  Southeast
151.87  14  Southeast by south
151.88  15  South-southeast
168.75  16  South by east
185.62  17  South
185.63  18  South by west
202.50  19  South-southwest
219.37  20  Southwest by south
219.38  21  Southwest
236.25  22  Southwest by west
253.12  23  West-southwest
253.13  24  West by south
270.00  25  West
286.87  26  West by north
286.88  27  West-northwest
303.75  28  Northwest by west
320.62  29  Northwest
320.63  30  Northwest by north
337.50  31  North-northwest
354.37  32  North by west
354.38   1  North
```


=={{header|F_Sharp|F#}}==
{{trans|Perl}}

```fsharp
let box = [|
    "North"; "North by east"; "North-northeast"; "Northeast by north";
    "Northeast"; "Northeast by east"; "East-northeast"; "East by north";
    "East"; "East by south"; "East-southeast"; "Southeast by east";
    "Southeast"; "Southeast by south"; "South-southeast"; "South by east";
    "South"; "South by west"; "South-southwest"; "Southwest by south";
    "Southwest"; "Southwest by west"; "West-southwest"; "West by south";
    "West"; "West by north"; "West-northwest"; "Northwest by west";
    "Northwest"; "Northwest by north"; "North-northwest"; "North by west" |]

[ 0.0; 16.87; 16.88; 33.75; 50.62; 50.63; 67.5; 84.37; 84.38;
101.25; 118.12; 118.13; 135.0; 151.87; 151.88; 168.75; 185.62;
185.63; 202.5; 219.37; 219.38; 236.25; 253.12; 253.13; 270.0;
286.87; 286.88; 303.75; 320.62; 320.63; 337.5; 354.37; 354.38 ]
|> List.iter (fun phi ->
    let i =  (int (phi * 32. / 360. + 0.5)) % 32
    printf "%3d %18s %6.2f°\n" (i + 1) box.[i] phi)
```

{{out}}

```txt
  1              North   0.00°
  2      North by east  16.87°
  3    North-northeast  16.88°
  4 Northeast by north  33.75°
  5          Northeast  50.62°
  6  Northeast by east  50.63°
  7     East-northeast  67.50°
  8      East by north  84.37°
  9               East  84.38°
 10      East by south 101.25°
 11     East-southeast 118.12°
 12  Southeast by east 118.13°
 13          Southeast 135.00°
 14 Southeast by south 151.87°
 15    South-southeast 151.88°
 16      South by east 168.75°
 17              South 185.62°
 18      South by west 185.63°
 19    South-southwest 202.50°
 20 Southwest by south 219.37°
 21          Southwest 219.38°
 22  Southwest by west 236.25°
 23     West-southwest 253.12°
 24      West by south 253.13°
 25               West 270.00°
 26      West by north 286.87°
 27     West-northwest 286.88°
 28  Northwest by west 303.75°
 29          Northwest 320.62°
 30 Northwest by north 320.63°
 31    North-northwest 337.50°
 32      North by west 354.37°
  1              North 354.38°
```



## Factor


```Factor
USING: formatting kernel math sequences ;

CONSTANT: box
{
    "North" "North by east" "North-northeast"
    "Northeast by north" "Northeast" "Northeast by east"
    "East-northeast" "East by north" "East" "East by south"
    "East-southeast" "Southeast by east" "Southeast"
    "Southeast by south" "South-southeast" "South by east"
    "South" "South by west" "South-southwest"
    "Southwest by south" "Southwest" "Southwest by west"
    "West-southwest" "West by south" "West" "West by north"
    "West-northwest" "Northwest by west" "Northwest"
    "Northwest by north" "North-northwest" "North by west"
}

{
    0 16.87 16.88 33.75 50.62 50.63 67.5 84.37 84.38 101.25
    118.12 118.13 135 151.87 151.88 168.75 185.62 185.63 202.5
    219.37 219.38 236.25 253.12 253.13 270 286.87 286.88 303.75
    320.62 320.63 337.5 354.37 354.38
}

[
    dup 32 * 360 /f 0.5 + >integer 32 mod [ 1 + ] [ box nth ] bi
    "%6.2f°  %2d  %s\n" printf
] each
```

{{out}}

```txt

  0.00°   1  North
 16.87°   2  North by east
 16.88°   3  North-northeast
 33.75°   4  Northeast by north
 50.62°   5  Northeast
 50.63°   6  Northeast by east
 67.50°   7  East-northeast
 84.37°   8  East by north
 84.38°   9  East
101.25°  10  East by south
118.12°  11  East-southeast
118.13°  12  Southeast by east
135.00°  13  Southeast
151.87°  14  Southeast by south
151.88°  15  South-southeast
168.75°  16  South by east
185.62°  17  South
185.63°  18  South by west
202.50°  19  South-southwest
219.37°  20  Southwest by south
219.38°  21  Southwest
236.25°  22  Southwest by west
253.12°  23  West-southwest
253.13°  24  West by south
270.00°  25  West
286.87°  26  West by north
286.88°  27  West-northwest
303.75°  28  Northwest by west
320.62°  29  Northwest
320.63°  30  Northwest by north
337.50°  31  North-northwest
354.37°  32  North by west
354.38°   1  North

```



## Fortran

{{works with|Fortran|90 and later}}

```fortran
Program Compass
  implicit none

  integer :: i, ind
  real :: heading

  do i = 0, 32
    heading = i * 11.25
    if (mod(i, 3) == 1) then
      heading = heading + 5.62
    else if (mod(i, 3) == 2) then
            heading = heading - 5.62
    end if
    ind = mod(i, 32) + 1
    write(*, "(i2, a20, f8.2)") ind, compasspoint(heading), heading
  end do

contains

function compasspoint(h)
  character(18) :: compasspoint
  character(18) :: points(32) = (/ "North             ", "North by east     ", "North-northeast   ", &
             "Northeast by north", "Northeast         ", "Northeast by east ", "East-northeast    ", &
             "East by north     ", "East              ", "East by south     ", "East-southeast    ", &
             "Southeast by east ", "Southeast         ", "Southeast by south", "South-southeast   ", &
             "South by east     ", "South             ", "South by west     ", "South-southwest   ", &
             "Southwest by south", "Southwest         ", "Southwest by west ", "West-southwest    ", &
             "West by south     ", "West              ", "West by north     ", "West-northwest    ", &
             "Northwest by west ", "Northwest         ", "Northwest by north", "North-northwest   ", &
             "North by west     "  /)
  real, intent(in) :: h
  real :: x

  x = h / 11.25 + 1.5
  if (x >= 33.0) x = x - 32.0
  compasspoint = points(int(x))
end function compasspoint
end program Compass
```

Output:

```txt

 1  North                 0.00
 2  North by east        16.87
 3  North-northeast      16.88
 4  Northeast by north   33.75
 5  Northeast            50.62
 6  Northeast by east    50.63
 7  East-northeast       67.50
 8  East by north        84.37
 9  East                 84.38
10  East by south       101.25
11  East-southeast      118.12
12  Southeast by east   118.13
13  Southeast           135.00
14  Southeast by south  151.87
15  South-southeast     151.88
16  South by east       168.75
17  South               185.62
18  South by west       185.63
19  South-southwest     202.50
20  Southwest by south  219.37
21  Southwest           219.38
22  Southwest by west   236.25
23  West-southwest      253.12
24  West by south       253.13
25  West                270.00
26  West by north       286.87
27  West-northwest      286.88
28  Northwest by west   303.75
29  Northwest           320.62
30  Northwest by north  320.63
31  North-northwest     337.50
32  North by west       354.37
 1  North               354.38
```


## FreeBASIC


```freebasic
' version 04-11-2016
' compile with: fbc -s console

Dim As String names(0 To ...) = { "North", "North by east", "North-northeast", _
     "Northeast by north", "Northeast", "Northeast by east", "East-northeast", _
                   "East by north", "East", "East by south", "East-southeast", _
    "Southeast by east", "Southeast", "Southeast by south", "South-southeast", _
                 "South by east", "South", "South by west", "South-southwest", _
     "Southwest by south", "Southwest", "Southwest by west", "West-southwest", _
                   "West by south", "West", "West by north", "West-northwest", _
    "Northwest by west", "Northwest", "Northwest by north", "North-northwest", _
                                                     "North by west", "North" }

Dim As Double degrees(0 To ...) = { 0, 16.87, 16.88, 33.75, 50.62, 50.63, _
 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135, 151.87, 151.88, 168.75, _
      185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270, _
           286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38 }

Dim As ULong i, j

For i = LBound(degrees) To UBound(degrees)
    j = Int((degrees(i) + 5.625) / 11.25)
    If j > 31 Then j = j - 32
    Print Using "####.##  ##  "; degrees(i); j;
    Print names(j)
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
   0.00   0  North
  16.87   1  North by east
  16.88   2  North-northeast
  33.75   3  Northeast by north
  50.62   4  Northeast
  50.63   5  Northeast by east
  67.50   6  East-northeast
  84.37   7  East by north
  84.38   8  East
 101.25   9  East by south
 118.12  10  East-southeast
 118.13  11  Southeast by east
 135.00  12  Southeast
 151.87  13  Southeast by south
 151.88  14  South-southeast
 168.75  15  South by east
 185.62  16  South
 185.63  17  South by west
 202.50  18  South-southwest
 219.37  19  Southwest by south
 219.38  20  Southwest
 236.25  21  Southwest by west
 253.12  22  West-southwest
 253.13  23  West by south
 270.00  24  West
 286.87  25  West by north
 286.88  26  West-northwest
 303.75  27  Northwest by west
 320.62  28  Northwest
 320.63  29  Northwest by north
 337.50  30  North-northwest
 354.37  31  North by west
 354.38   0  North
```



## Gambas

'''[https://gambas-playground.proko.eu/?gist=823a56ac094b8963cf11f792b381fbcc Click this link to run this code]'''

```gambas
Public Sub Main()
Dim fDeg As Float[] = [0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38]
Dim cHeading As Collection = ["N": "North", "S": "South", "W": "West", "E": "East", "b": "by"]
Dim sHeading As String[] = ["N", "NbE", "NNE", "NEbE", "NE", "NEbE", "ENE", "EbN", "E", "EbS", "ESE", "SEbE", "SE", "SEbS", "SSE", "SbE", "S", "SbW", "SSW", "SWbS", "SW", "SWbW", "WSW", "WbS", "W", "WbN", "WNW", "NWbW", "NW", "NWbN", "NNW", "NbW"]
Dim siLoop As Short
Dim sDirection As String
Dim fCount, fTemp As Float

For Each fCount In fDeg
   fTemp = Round(fCount / 11.25)
   If fTemp = 32 Then fTemp = 0
    For siLoop = 0 To Len(sHeading[fTemp])
      sDirection &= cHeading[Mid(sHeading[fTemp], siLoop + 1, 1)] & " "
    Next
  Print "Index=" & Format(fTemp + 1, "#0") & "  " & Format(Str(fCount), "##0.00") & " degrees = " & sDirection
  sDirection = ""
Next

End
```

Output:

```txt

Index= 1    0.00 degrees = North
Index= 2   16.87 degrees = North by East
Index= 3   16.88 degrees = North North East
Index= 4   33.75 degrees = North East by East
Index= 5   50.62 degrees = North East
Index= 6   50.63 degrees = North East by East
Index= 7   67.50 degrees = East North East
Index= 8   84.37 degrees = East by North
Index= 9   84.38 degrees = East
Index=10  101.25 degrees = East by South
Index=11  118.12 degrees = East South East
Index=12  118.13 degrees = South East by East
Index=13  135.00 degrees = South East
Index=14  151.87 degrees = South East by South
Index=15  151.88 degrees = South South East
Index=16  168.75 degrees = South by East
Index=17  185.62 degrees = South
Index=18  185.63 degrees = South by West
Index=19  202.50 degrees = South South West
Index=20  219.37 degrees = South West by South
Index=21  219.38 degrees = South West
Index=22  236.25 degrees = South West by West
Index=23  253.12 degrees = West South West
Index=24  253.13 degrees = West by South
Index=25  270.00 degrees = West
Index=26  286.87 degrees = West by North
Index=27  286.88 degrees = West North West
Index=28  303.75 degrees = North West by West
Index=29  320.62 degrees = North West
Index=30  320.63 degrees = North West by North
Index=31  337.50 degrees = North North West
Index=32  354.37 degrees = North by West
Index= 1  354.38 degrees = North

```



## Go


```go
package main

import "fmt"

// function required by task
func degrees2compasspoint(h float32) string {
    return compassPoint[cpx(h)]
}

// cpx returns integer index from 0 to 31 corresponding to compass point.
// input heading h is in degrees.  Note this index is a zero-based index
// suitable for indexing into the table of printable compass points,
// and is not the same as the index specified to be printed in the output.
func cpx(h float32) int {
    x := int(h/11.25+.5) % 32
    if x < 0 {
        x += 32
    }
    return x
}

// printable compass points
var compassPoint = []string{
    "North",
    "North by east",
    "North-northeast",
    "Northeast by north",
    "Northeast",
    "Northeast by east",
    "East-northeast",
    "East by north",
    "East",
    "East by south",
    "East-southeast",
    "Southeast by east",
    "Southeast",
    "Southeast by south",
    "South-southeast",
    "South by east",
    "South",
    "South by west",
    "South-southwest",
    "Southwest by south",
    "Southwest",
    "Southwest by west",
    "West-southwest",
    "West by south",
    "West",
    "West by north",
    "West-northwest",
    "Northwest by west",
    "Northwest",
    "Northwest by north",
    "North-northwest",
    "North by west",
}

func main() {
    fmt.Println("Index  Compass point         Degree")
    for i, h := range []float32{0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5,
        84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75,
        185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0,
        286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38} {
        index := i%32 + 1 // printable index computed per pseudocode
        fmt.Printf("%4d   %-19s %7.2f°\n", index, degrees2compasspoint(h), h)
    }
}
```


```txt

Index  Compass point         Degree
   1   North                  0.00°
   2   North by east         16.87°
   3   North-northeast       16.88°
   4   Northeast by north    33.75°
   5   Northeast             50.62°
   6   Northeast by east     50.63°
   7   East-northeast        67.50°
   8   East by north         84.37°
   9   East                  84.38°
  10   East by south        101.25°
  11   East-southeast       118.12°
  12   Southeast by east    118.13°
  13   Southeast            135.00°
  14   Southeast by south   151.87°
  15   South-southeast      151.88°
  16   South by east        168.75°
  17   South                185.62°
  18   South by west        185.63°
  19   South-southwest      202.50°
  20   Southwest by south   219.37°
  21   Southwest            219.38°
  22   Southwest by west    236.25°
  23   West-southwest       253.12°
  24   West by south        253.13°
  25   West                 270.00°
  26   West by north        286.87°
  27   West-northwest       286.88°
  28   Northwest by west    303.75°
  29   Northwest            320.62°
  30   Northwest by north   320.63°
  31   North-northwest      337.50°
  32   North by west        354.37°
   1   North                354.38°

```



## Groovy


```groovy
def asCompassPoint(angle) {
    def cardinalDirections = ["north", "east", "south", "west"]

    int index = Math.floor(angle / 11.25 + 0.5)
    int cardinalIndex = (index / 8)

    def c1 = cardinalDirections[cardinalIndex % 4]
    def c2 = cardinalDirections[(cardinalIndex + 1) % 4]
    def c3 = (cardinalIndex == 0 || cardinalIndex == 2) ? "$c1$c2" : "$c2$c1"

    def point = [
        "$c1", "$c1 by $c2", "$c1-$c3", "$c3 by $c1", "$c3", "$c3 by $c2", "$c2-$c3", "$c2 by $c1"
    ][index % 8]
    point.substring(0, 1).toUpperCase() + point.substring(1)
}
Number.metaClass.asCompassPoint =  { asCompassPoint(delegate) }

[0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75,
 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5,
 354.37, 354.38].eachWithIndex { angle, index ->
    println "${(index % 32) + 1}".padRight(3) + "${angle.asCompassPoint().padLeft(20)}    $angle\u00b0"
}
```

Output:

```txt
1                 North    0.0°
2         North by east    16.87°
3       North-northeast    16.88°
4    Northeast by north    33.75°
5             Northeast    50.62°
6     Northeast by east    50.63°
7        East-northeast    67.5°
8         East by north    84.37°
9                  East    84.38°
10        East by south    101.25°
11       East-southeast    118.12°
12    Southeast by east    118.13°
13            Southeast    135.0°
14   Southeast by south    151.87°
15      South-southeast    151.88°
16        South by east    168.75°
17                South    185.62°
18        South by west    185.63°
19      South-southwest    202.5°
20   Southwest by south    219.37°
21            Southwest    219.38°
22    Southwest by west    236.25°
23       West-southwest    253.12°
24        West by south    253.13°
25                 West    270.0°
26        West by north    286.87°
27       West-northwest    286.88°
28    Northwest by west    303.75°
29            Northwest    320.62°
30   Northwest by north    320.63°
31      North-northwest    337.5°
32        North by west    354.37°
1                 North    354.38°
```



## Haskell


```haskell
import Data.Char (toUpper)

import Data.Maybe (fromMaybe)

import Text.Printf (PrintfType, printf)

dirs :: [String]
dirs =
  [ "N"
  , "NbE"
  , "N-NE"
  , "NEbN"
  , "NE"
  , "NEbE"
  , "E-NE"
  , "EbN"
  , "E"
  , "EbS"
  , "E-SE"
  , "SEbE"
  , "SE"
  , "SEbS"
  , "S-SE"
  , "SbE"
  , "S"
  , "SbW"
  , "S-SW"
  , "SWbS"
  , "SW"
  , "SWbW"
  , "W-SW"
  , "WbS"
  , "W"
  , "WbN"
  , "W-NW"
  , "NWbW"
  , "NW"
  , "NWbN"
  , "N-NW"
  , "NbW"
  ]

-- Index between 0 and 31 ->  the corresponding compass point name.
pointName :: Int -> String
pointName = capitalize . concatMap (fromMaybe "?" . fromChar) . (dirs !!)
  where
    fromChar c =
      lookup
        c
        [ ('N', "north")
        , ('S', "south")
        , ('E', "east")
        , ('W', "west")
        , ('b', " by ")
        , ('-', "-")
        ]
    capitalize (c:cs) = toUpper c : cs

-- Degrees -> compass point index between 0 and 31.
pointIndex :: Double -> Int
pointIndex d = (round (d * 1000) + 5625) `mod` 360000 `div` 11250

printPointName :: PrintfType t => String -> t
printPointName d =
  let deg = read d :: Double
      idx = pointIndex deg
  in printf "%2d  %-18s  %6.2f°\n" (idx + 1) (pointName idx) deg

main :: IO ()
main = mapM_ (printPointName . show) [0 .. 31]
```

Output:

```txt
 1  North                 0.00°
 2  North by east        16.87°
 3  North-northeast      16.88°
 4  Northeast by north   33.75°
 5  Northeast            50.62°
 6  Northeast by east    50.63°
 7  East-northeast       67.50°
 8  East by north        84.37°
 9  East                 84.38°
10  East by south       101.25°
11  East-southeast      118.12°
12  Southeast by east   118.13°
13  Southeast           135.00°
14  Southeast by south  151.87°
15  South-southeast     151.88°
16  South by east       168.75°
17  South               185.62°
18  South by west       185.63°
19  South-southwest     202.50°
20  Southwest by south  219.37°
21  Southwest           219.38°
22  Southwest by west   236.25°
23  West-southwest      253.12°
24  West by south       253.13°
25  West                270.00°
26  West by north       286.87°
27  West-northwest      286.88°
28  Northwest by west   303.75°
29  Northwest           320.62°
30  Northwest by north  320.63°
31  North-northwest     337.50°
32  North by west       354.37°
 1  North               354.38°
```



## Huginn


```huginn
import Algorithms as algo;
import Text as text;

class Compass {
  _majors = none;
  _quarter1 = none;
  _quarter2 = none;
  constructor() {
    _majors = algo.materialize( text.split( "north east south west", " " ), tuple );
    _majors += _majors;
    _quarter1 = text.split( "N,N by E,N-NE,NE by N,NE,NE by E,E-NE,E by N", "," );
    _quarter2 = algo.materialize( algo.map( _quarter1, @( s ){ copy( s ).replace( "NE", "EN" ); } ), list );
  }
  degrees_to_compasspoint( d ) {
    d = d % 360. + 360. / 64.;
    majorindex, minor = ( integer( d ) / 90, d % 90. );
    minorindex  = integer( minor * 4. ) / 45;
    p1, p2 = _majors[majorindex: majorindex + 2];
    q = none;
    if ( p1 ∈ { "north", "south" } ) {
      q = _quarter1;
    } else {
      q = _quarter2;
    }
    return ( text.capitalize( copy( q[minorindex] ).replace( "N", p1 ).replace( "E", p2 ) ) );
  }
}

main() {
  print(
    " # |  Angle  | Compass point\n"
    "---+---------|-------------------\n"
  );
  c = Compass();
  for ( i : algo.range( 33 ) ) {
    d = real( i ) * 11.25;
    m = i % 3;
    if ( m == 1 ) {
      d += 5.62;
    } else if ( m == 2 ) {
      d -= 5.62;
    }
    n = i % 32 + 1;
    print( "{:2d} | {:6.2f}° | {}\n".format( n, d, c.degrees_to_compasspoint( d ) ) );
  }
}
```

Output:
```txt
 # |  Angle  | Compass point
---+---------|-------------------
 1 |   0.00° | North
 2 |  16.87° | North by east
 3 |  16.88° | North-northeast
 4 |  33.75° | Northeast by north
 5 |  50.62° | Northeast
 6 |  50.63° | Northeast by east
 7 |  67.50° | East-northeast
 8 |  84.37° | East by north
 9 |  84.38° | East
10 | 101.25° | East by south
11 | 118.12° | East-southeast
12 | 118.13° | Southeast by east
13 | 135.00° | Southeast
14 | 151.87° | Southeast by south
15 | 151.88° | South-southeast
16 | 168.75° | South by east
17 | 185.62° | South
18 | 185.63° | South by west
19 | 202.50° | South-southwest
20 | 219.37° | Southwest by south
21 | 219.38° | Southwest
22 | 236.25° | Southwest by west
23 | 253.12° | West-southwest
24 | 253.13° | West by south
25 | 270.00° | West
26 | 286.87° | West by north
27 | 286.88° | West-northwest
28 | 303.75° | Northwest by west
29 | 320.62° | Northwest
30 | 320.63° | Northwest by north
31 | 337.50° | North-northwest
32 | 354.37° | North by west
 1 | 354.38° | North
```


=={{header|Icon}} and {{header|Unicon}}==
{{incomplete|Icon|354.38?}}

```Icon
link strings,numbers

procedure main()

every heading := 11.25 * (i := 0 to 32) do {
   case i%3 of {
      1: heading +:= 5.62
      2: heading -:= 5.62
      }
   write(right(i+1,3)," ",left(direction(heading),20)," ",fix(heading,,7,2))
   }
end

procedure direction(d)  # compass heading given +/- degrees
static dirs
initial {
   every put(dirs := [],
             replacem(!["N","NbE","N-NE","NEbN","NE","NEbE","E-NE","EbN",
                        "E","EbS","E-SE","SEbE","SE","SEbS","S-SE","SbE",
	                     "S","SbW","S-SW","SWbS","SW","SWbW","W-SW","WbS",
	                    "W","WbN","W-NW","NWbW","NW","NWbN","N-NW","NbW"],
                       "N","north","E","east","W","west","S","south","b"," by "))
   }

   return dirs[round(((((d%360)+360)%360)/11.25)%32 + 1)]
end
```


{{libheader|Icon Programming Library}}
[http://www.cs.arizona.edu/icon/library/procs/strings.htm strings for replacem]
[http://www.cs.arizona.edu/icon/library/procs/numbers.htm numbers for round, fix]

Output:
```txt
  1 north                   0.00
  2 north by east          16.87
  3 north-northeast        16.88
  4 northeast by north     33.75
  5 northeast              50.62
  6 northeast by east      50.63
  7 east-northeast         67.50
  8 east by north          84.37
  9 east                   84.38
 10 east by south         101.25
 11 east-southeast        118.12
 12 southeast by east     118.13
 13 southeast             135.00
 14 southeast by south    151.87
 15 south-southeast       151.88
 16 south by east         168.75
 17 south                 185.62
 18 south by west         185.63
 19 south-southwest       202.50
 20 southwest by south    219.37
 21 southwest             219.38
 22 southwest by west     236.25
 23 west-southwest        253.12
 24 west by south         253.13
 25 west                  270.00
 26 west by north         286.87
 27 west-northwest        286.88
 28 northwest by west     303.75
 29 northwest             320.62
 30 northwest by north    320.63
 31 north-northwest       337.50
 32 north by west         354.37
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Compass.bas"
110 STRING DR$(1 TO 33)*18
120 FOR I=1 TO 33
130   READ DR$(I)
140 NEXT
150 DO
160   READ IF MISSING EXIT DO:D
170   LET DIR=COMP(D)
180   PRINT D;TAB(12);DIR,DR$(DIR)
190 LOOP
200 DEF COMP(D)=CEIL(MOD((D+360/64),360)*32/360)
210 DATA North,North by east,North-northeast,Northeast by north,Northeast,Northeast by east,East-northeast,East by north,East,East by south,East-southeast,Southeast by east,Southeast,Southeast by south,South-southeast,South by east
220 DATA South,South by west,South-southwest,Southwest by south,Southwest,Southwest by west,West-southwest,West by south,West,West by north,West-northwest,Northwest by west,Northwest,Northwest by north,North-northwest,North by west,North
230 DATA 0.0,16.87,16.88,33.75,50.62,50.63,67.5,84.37,84.38,101.25,118.12,118.13,135.0,151.87,151.88,168.75,185.62,185.63,202.5,219.37,219.38,236.25,253.12,253.13,270.0,286.87,286.88,303.75,320.62,320.63,337.5,354.37,354.38
```


Output:
```txt
 0 	    1   North
 16.87      2   North by east
 16.88      3   North-northeast
 33.75      4   Northeast by north
 50.62      5   Northeast
 50.63      6   Northeast by east
 67.5       7   East-northeast
 84.37      8   East by north
 84.38      9   East
 101.25     10  East by south
 118.12     11  East-southeast
 118.13     12  Southeast by east
 135        13  Southeast
 151.87     14  Southeast by south
 151.88     15  South-southeast
 168.75     16  South by east
 185.62     17  South
 185.63     18  South by west
 202.5      19  South-southwest
 219.37     20  Southwest by south
 219.38     21  Southwest
 236.25     22  Southwest by west
 253.12     23  West-southwest
 253.13     24  West by south
 270        25  West
 286.87     26  West by north
 286.88     27  West-northwest
 303.75     28  Northwest by west
 320.62     29  Northwest
 320.63     30  Northwest by north
 337.5      31  North-northwest
 354.37     32  North by west
 354.38     1   North
```



## J



```j
require'strings'
subs=: 'N,north,S,south,E,east,W,west,b, by ,'
dirs=: subs (toupper@{., }.)@rplc~L:1 0&(<;._2) 0 :0 -. ' ',LF
  N,NbE,N-NE,NEbN,NE,NEbE,E-NE,EbN,E,EbS,E-SE,SEbE,SE,SEbS,S-SE,SbE,
  S,SbW,S-SW,SWbS,SW,SWbW,W-SW,WbS,W,WbN,W-NW,NWbW,NW,NWbN,N-NW,NbW,
)
indice=: 32 | 0.5 <.@+ %&11.25
deg2pnt=: dirs {~ indice
```


Example use:


```j
   i.10
0 1 2 3 4 5 6 7 8 9
   deg2pnt i.10
┌─────┬─────┬─────┬─────┬─────┬─────┬─────────────┬─────────────┬─────────────┬─────────────┐
│North│North│North│North│North│North│North by east│North by east│North by east│North by east│
└─────┴─────┴─────┴─────┴─────┴─────┴─────────────┴─────────────┴─────────────┴─────────────┘
```


Required example:


```j
   (":@>:@indice,.' ',.>@deg2pnt,.' ',.":@,.)(*&11.25 + 5.62 * 0 1 _1 {~ 3&|) i.33
1  North                   0
2  North by east       16.87
3  North-northeast     16.88
4  Northeast by north  33.75
5  Northeast           50.62
6  Northeast by east   50.63
7  East-northeast       67.5
8  East by north       84.37
9  East                84.38
10 East by south      101.25
11 East-southeast     118.12
12 Southeast by east  118.13
13 Southeast             135
14 Southeast by south 151.87
15 South-southeast    151.88
16 South by east      168.75
17 South              185.62
18 South by west      185.63
19 South-southwest     202.5
20 Southwest by south 219.37
21 Southwest          219.38
22 Southwest by west  236.25
23 West-southwest     253.12
24 West by south      253.13
25 West                  270
26 West by north      286.87
27 West-northwest     286.88
28 Northwest by west  303.75
29 Northwest          320.62
30 Northwest by north 320.63
31 North-northwest     337.5
32 North by west      354.37
1  North              354.38
```



## Java

{{trans|Visual Basic .NET}}

```java
public class BoxingTheCompass{
    private static String[] points = new String[32];

    public static void main(String[] args){
        buildPoints();

        double heading = 0;

        for(int i = 0; i<= 32;i++){
            heading = i * 11.25;
            switch(i % 3){
                case 1:
                    heading += 5.62;
                    break;
                case 2:
                    heading -= 5.62;
                    break;
                default:
            }

            System.out.printf("%s\t%18s\t%s°\n",(i % 32) + 1, initialUpper(getPoint(heading)), heading);
        }
    }

    private static void buildPoints(){
        String[] cardinal = {"north", "east", "south", "west"};
        String[] pointDesc = {"1", "1 by 2", "1-C", "C by 1", "C", "C by 2", "2-C", "2 by 1"};

        String str1, str2, strC;

        for(int i = 0;i <= 3;i++){
            str1 = cardinal[i];
            str2 = cardinal[(i + 1) % 4];
            strC = (str1.equals("north") || str1.equals("south")) ? (str1 + str2): (str2 + str1);
            for(int j = 0;j <= 7;j++){
                points[i * 8 + j] = pointDesc[j].replace("1", str1).replace("2", str2).replace("C", strC);
            }
        }
    }

    private static String initialUpper(String s){
        return s.substring(0, 1).toUpperCase() + s.substring(1);
    }

    private static String getPoint(double degrees){
        double testD = (degrees / 11.25) + 0.5;
        return points[(int)Math.floor(testD % 32)];
    }
}
```

Output:

```txt
1	             North	0.0°
2	     North by east	16.87°
3	   North-northeast	16.88°
4	Northeast by north	33.75°
5	         Northeast	50.62°
6	 Northeast by east	50.63°
7	    East-northeast	67.5°
8	     East by north	84.37°
9	              East	84.38°
10	     East by south	101.25°
11	    East-southeast	118.12°
12	 Southeast by east	118.13°
13	         Southeast	135.0°
14	Southeast by south	151.87°
15	   South-southeast	151.88°
16	     South by east	168.75°
17	             South	185.62°
18	     South by west	185.63°
19	   South-southwest	202.5°
20	Southwest by south	219.37°
21	         Southwest	219.38°
22	 Southwest by west	236.25°
23	    West-southwest	253.12°
24	     West by south	253.13°
25	              West	270.0°
26	     West by north	286.87°
27	    West-northwest	286.88°
28	 Northwest by west	303.75°
29	         Northwest	320.62°
30	Northwest by north	320.63°
31	   North-northwest	337.5°
32	     North by west	354.37°
1	             North	354.38°
```



## JavaScript


### ES5

An iterative, web-based approach:


```javascript
function createRow(i, point, heading) {
    var tr = document.createElement('tr'),
        td;

    td = document.createElement('td');
    td.appendChild(document.createTextNode(i));
    tr.appendChild(td);

    td = document.createElement('td');
    point = point.substr(0, 1).toUpperCase() + point.substr(1);
    td.appendChild(document.createTextNode(point));
    tr.appendChild(td);

    td = document.createElement('td');
    td.appendChild(document.createTextNode(heading));
    tr.appendChild(td);

    return tr;
}

function getPoint(i) {
    var j = i % 8,
        i = Math.floor(i / 8) % 4,
        cardinal = ['north', 'east', 'south', 'west'],
        pointDesc = ['1', '1 by 2', '1-C', 'C by 1', 'C', 'C by 2', '2-C', '2 by 1'],
        str1, str2, strC;

    str1 = cardinal[i];
    str2 = cardinal[(i + 1) % 4];
    strC = (str1 === 'north' || str1 === 'south') ? str1 + str2 : str2 + str1;
    return pointDesc[j].replace('1', str1).replace('2', str2).replace('C', strC);
}

var i,
    heading,
    table = document.createElement('table'),
    tbody = document.createElement('tbody'),
    tr;
for (i = 0; i <= 32; i += 1) {
    heading = i * 11.25 + [0, 5.62, -5.62][i % 3];
    tr = createRow(i % 32 + 1, getPoint(i), heading + '°');
    tbody.appendChild(tr);
}
table.appendChild(tbody);
document.body.appendChild(table);

```

Output:

```txt
1	North			0°
2	North by east		16.87°
3	North-northeast		16.88°
4	Northeast by north	33.75°
5	Northeast		50.62°
6	Northeast by east	50.63°
7	East-northeast		67.5°
8	East by north		84.37°
9	East			84.38°
10	East by south		101.25°
11	East-southeast		118.12°
12	Southeast by east	118.13°
13	Southeast		135°
14	Southeast by south	151.87°
15	South-southeast		151.88°
16	South by east		168.75°
17	South			185.62°
18	South by west		185.63°
19	South-southwest		202.5°
20	Southwest by south	219.37°
21	Southwest		219.38°
22	Southwest by west	236.25°
23	West-southwest		253.12°
24	West by south		253.13°
25	West			270°
26	West by north		286.87°
27	West-northwest		286.88°
28	Northwest by west	303.75°
29	Northwest		320.62°
30	Northwest by north	320.63°
31	North-northwest		337.5°
32	North by west		354.37°
1	North			354.38°
```



### ES6


Functional composition, allowing for additional languages (and different numbers of compass points)

```JavaScript
(() => {
    'use strict';

    // GENERIC FUNCTIONS

    // toTitle :: String -> String
    let toTitle = s => s.length ? (s[0].toUpperCase() + s.slice(1)) : '';

    // COMPASS DATA AND FUNCTIONS

    // Scale invariant keys for points of the compass
    // (allows us to look up a translation for one scale of compass (32 here)
    // for use in another size of compass (8 or 16 points)
    // (Also semi-serviceable as more or less legible keys without translation)

    // compassKeys :: Int -> [String]
    let compassKeys = depth => {
        let urCompass = ['N', 'S', 'N'],
            subdivision = (compass, n) => n <= 1 ? (
                compass
            ) : subdivision( // Borders between N and S engender E and W.
                // other new boxes concatenate their parent keys.
                compass.reduce((a, x, i, xs) => {
                    if (i > 0) {
                        return (n === depth) ? (
                            a.concat([x === 'N' ? 'W' : 'E'], x)
                        ) : a.concat([xs[i - 1] + x, x]);
                    } else return a.concat(x);
                }, []),
                n - 1
            );
        return subdivision(urCompass, depth)
            .slice(0, -1);
    };

    // https://zh.wikipedia.org/wiki/%E7%BD%97%E7%9B%98%E6%96%B9%E4%BD%8D
    let lstLangs = [{
        'name': 'English',
        expansions: {
            N: 'north',
            S: 'south',
            E: 'east',
            W: 'west',
            b: ' by ',
            '-': '-'
        },
        'N': 'N',
        'NNNE': 'NbE',
        'NNE': 'N-NE',
        'NNENE': 'NEbN',
        'NE': 'NE',
        'NENEE': 'NEbE',
        'NEE': 'E-NE',
        'NEEE': 'EbN',
        'E': 'E',
        'EEES': 'EbS',
        'EES': 'E-SE',
        'EESES': 'SEbE',
        'ES': 'SE',
        'ESESS': 'SEbS',
        'ESS': 'S-SE',
        'ESSS': 'SbE',
        'S': 'S',
        'SSSW': 'SbW',
        'SSW': 'S-SW',
        'SSWSW': 'SWbS',
        'SW': 'SW',
        'SWSWW': 'SWbW',
        'SWW': 'W-SW',
        'SWWW': 'WbS',
        'W': 'W',
        'WWWN': 'WbN',
        'WWN': 'W-NW',
        'WWNWN': 'NWbW',
        'WN': 'NW',
        'WNWNN': 'NWbN',
        'WNN': 'N-NW',
        'WNNN': 'NbW'
    }, {
        'name': 'Chinese',
        'N': '北',
        'NNNE': '北微东',
        'NNE': '东北偏北',
        'NNENE': '东北微北',
        'NE': '东北',
        'NENEE': '东北微东',
        'NEE': '东北偏东',
        'NEEE': '东微北',
        'E': '东',
        'EEES': '东微南',
        'EES': '东南偏东',
        'EESES': '东南微东',
        'ES': '东南',
        'ESESS': '东南微南',
        'ESS': '东南偏南',
        'ESSS': '南微东',
        'S': '南',
        'SSSW': '南微西',
        'SSW': '西南偏南',
        'SSWSW': '西南微南',
        'SW': '西南',
        'SWSWW': '西南微西',
        'SWW': '西南偏西',
        'SWWW': '西微南',
        'W': '西',
        'WWWN': '西微北',
        'WWN': '西北偏西',
        'WWNWN': '西北微西',
        'WN': '西北',
        'WNWNN': '西北微北',
        'WNN': '西北偏北',
        'WNNN': '北微西'
    }];

    // pointIndex :: Int -> Num -> Int
    let pointIndex = (power, degrees) => {
        let nBoxes = (power ? Math.pow(2, power) : 32);
        return Math.ceil(
            (degrees + (360 / (nBoxes * 2))) % 360 * nBoxes / 360
        ) || 1;
    };

    // pointNames :: Int -> Int -> [String]
    let pointNames = (precision, iBox) => {
        let k = compassKeys(precision)[iBox - 1];
        return lstLangs.map(dctLang => {
            let s = dctLang[k] || k, // fallback to key if no translation
                dctEx = dctLang.expansions;

            return dctEx ? toTitle(s.split('')
                    .map(c => dctEx[c])
                    .join(precision > 5 ? ' ' : ''))
                .replace(/  /g, ' ') : s;
        });
    };

    // maximumBy :: (a -> a -> Ordering) -> [a] -> a
    let maximumBy = (f, xs) =>
        xs.reduce((a, x) => a === undefined ? x : (
            f(x, a) > 0 ? x : a
        ), undefined);

    // justifyLeft :: Int -> Char -> Text -> Text
    let justifyLeft = (n, cFiller, strText) =>
        n > strText.length ? (
            (strText + replicate(n, cFiller)
                .join(''))
            .substr(0, n)
        ) : strText;

    // justifyRight :: Int -> Char -> Text -> Text
    let justifyRight = (n, cFiller, strText) =>
        n > strText.length ? (
            (replicate(n, cFiller)
                .join('') + strText)
            .slice(-n)
        ) : strText;

    // replicate :: Int -> a -> [a]
    let replicate = (n, a) => {
        let v = [a],
            o = [];
        if (n < 1) return o;
        while (n > 1) {
            if (n & 1) o = o.concat(v);
            n >>= 1;
            v = v.concat(v);
        }
        return o.concat(v);
    };

    // transpose :: [[a]] -> [[a]]
    let transpose = xs =>
        xs[0].map((_, iCol) => xs.map((row) => row[iCol]));

    // length :: [a] -> Int
    // length :: Text -> Int
    let length = xs => xs.length;

    // compareByLength = (a, a) -> (-1 | 0 | 1)
    let compareByLength = (a, b) => {
        let [na, nb] = [a, b].map(length);
        return na < nb ? -1 : na > nb ? 1 : 0;
    };

    // maxLen :: [String] -> Int
    let maxLen = xs => maximumBy(compareByLength, xs)
        .length;

    // compassTable :: Int -> [Num] -> Maybe String
    let compassTable = (precision, xs) => {
        if (precision < 1) return undefined;
        else {
            let intPad = 2;

            let lstIndex = xs.map(x => pointIndex(precision, x)),
                lstStrIndex = lstIndex.map(x => x.toString()),
                nIndexWidth = maxLen(lstStrIndex),
                colIndex = lstStrIndex.map(
                    x => justifyRight(nIndexWidth, ' ', x)
                );

            let lstAngles = xs.map(x => x.toFixed(2) + '°'),
                nAngleWidth = maxLen(lstAngles) + intPad,
                colAngles = lstAngles.map(x => justifyRight(nAngleWidth, ' ', x));

            let lstTrans = transpose(
                    lstIndex.map(i => pointNames(precision, i))
                ),
                lstTransWidths = lstTrans.map(x => maxLen(x) + 2),
                colsTrans = lstTrans
                .map((lstLang, i) => lstLang
                    .map(x => justifyLeft(lstTransWidths[i], ' ', x))
                );

            return transpose([colIndex]
                    .concat([colAngles], [replicate(lstIndex.length, "  ")])
                    .concat(colsTrans))
                .map(x => x.join(''))
                .join('\n');
        }
    }

    // TEST
    let xs = [0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37,
        84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75,
        185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13,
        270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37,
        354.38
    ];

    // If we supply other precisions, like 4 or 6, (2^n -> 16 or 64 boxes)
    // the bearings will be divided amongst smaller or larger numbers of boxes,
    // either using name translations retrieved by the generic hash
    // or using the hash itself (combined with any expansions)
    // to substitute for missing names for very finely divided boxes.

    return compassTable(5, xs); // 2^5 -> 32 boxes
})();
```


{{Out}}

```txt
 1    0.00°  North               北
 2   16.87°  North by east       北微东
 3   16.88°  North-northeast     东北偏北
 4   33.75°  Northeast by north  东北微北
 5   50.62°  Northeast           东北
 6   50.63°  Northeast by east   东北微东
 7   67.50°  East-northeast      东北偏东
 8   84.37°  East by north       东微北
 9   84.38°  East                东
10  101.25°  East by south       东微南
11  118.12°  East-southeast      东南偏东
12  118.13°  Southeast by east   东南微东
13  135.00°  Southeast           东南
14  151.87°  Southeast by south  东南微南
15  151.88°  South-southeast     东南偏南
16  168.75°  South by east       南微东
17  185.62°  South               南
18  185.63°  South by west       南微西
19  202.50°  South-southwest     西南偏南
20  219.37°  Southwest by south  西南微南
21  219.38°  Southwest           西南
22  236.25°  Southwest by west   西南微西
23  253.12°  West-southwest      西南偏西
24  253.13°  West by south       西微南
25  270.00°  West                西
26  286.87°  West by north       西微北
27  286.88°  West-northwest      西北偏西
28  303.75°  Northwest by west   西北微西
29  320.62°  Northwest           西北
30  320.63°  Northwest by north  西北微北
31  337.50°  North-northwest     西北偏北
32  354.37°  North by west       北微西
 1  354.38°  North               北
```



## Julia

{{works with|Julia|1.2}}
{{trans|Python}}


```julia
using Printf

function degree2compasspoint(d::Float64)
    majors = ("north", "east", "south", "west", "north", "east", "south", "west")
    quarter1 = ("N", "N by E", "N-NE", "NE by N", "NE", "NE by E", "E-NE", "E by N")
    quarter2 = map(p -> replace(p, "NE" => "EN"), quarter1)

    d = d % 360 + 360 / 64
    imajor, minor = divrem(d, 90)
    iminor = div(minor * 4, 45)
    imajor += 1
    iminor += 1
    p1, p2 = majors[imajor:imajor+1]
    q = p1 in ("north", "south") ? quarter1 : quarter2
    titlecase(replace(replace(q[iminor], 'N' => p1), 'E' => p2))
end

for i in 0:32
    d = i * 11.25
    i % 3 == 1 && (d += 5.62)
    i % 3 == 2 && (d -= 5.62)
    @printf("%2i %-17s %10.2f°\n", i % 32 + 1, degree2compasspoint(d), d)
end
```


{{out}}

```txt
 1 North                   0.00°
 2 North By East          16.87°
 3 North-Northeast        16.88°
 4 Northeast By North      33.75°
 5 Northeast              50.62°
 6 Northeast By East      50.63°
 7 East-Northeast         67.50°
 8 East By North          84.37°
 9 East                   84.38°
10 East By South         101.25°
11 East-Southeast        118.12°
12 Southeast By East     118.13°
13 Southeast             135.00°
14 Southeast By South     151.87°
15 South-Southeast       151.88°
16 South By East         168.75°
17 South                 185.62°
18 South By West         185.63°
19 South-Southwest       202.50°
20 Southwest By South     219.37°
21 Southwest             219.38°
22 Southwest By West     236.25°
23 West-Southwest        253.12°
24 West By South         253.13°
25 West                  270.00°
26 West By North         286.87°
27 West-Northwest        286.88°
28 Northwest By West     303.75°
29 Northwest             320.62°
30 Northwest By North     320.63°
31 North-Northwest       337.50°
32 North By West         354.37°
 1 North                 354.38°
```



## Kotlin


```scala
// version 1.1.2

fun expand(cp: String): String {
    val sb = StringBuilder()
    for (c in cp) {
        sb.append(when (c) {
            'N'  -> "north"
            'E'  -> "east"
            'S'  -> "south"
            'W'  -> "west"
            'b'  -> " by "
            else -> "-"
        })
    }
    return sb.toString().capitalize()
}

fun main(args: Array<String>) {
    val cp = arrayOf(
        "N", "NbE", "N-NE", "NEbN", "NE", "NEbE", "E-NE", "EbN",
        "E", "EbS", "E-SE", "SEbE", "SE", "SEbS", "S-SE", "SbE",
        "S", "SbW", "S-SW", "SWbS", "SW", "SWbW", "W-SW", "WbS",
        "W", "WbN", "W-NW", "NWbW", "NW", "NWbN", "N-NW", "NbW"
    )
    println("Index  Degrees  Compass point")
    println("-----  -------  -------------")
    val f = "%2d     %6.2f   %s"
    for (i in 0..32) {
        val index  = i % 32
        var heading = i * 11.25
        when (i % 3) {
            1 -> heading += 5.62
            2 -> heading -= 5.62
        }
        println(f.format(index + 1, heading, expand(cp[index])))
    }
}
```


{{out}}

```txt

Index  Degrees  Compass point
-----  -------  -------------
 1       0.00   North
 2      16.87   North by east
 3      16.88   North-northeast
 4      33.75   Northeast by north
 5      50.62   Northeast
 6      50.63   Northeast by east
 7      67.50   East-northeast
 8      84.37   East by north
 9      84.38   East
10     101.25   East by south
11     118.12   East-southeast
12     118.13   Southeast by east
13     135.00   Southeast
14     151.87   Southeast by south
15     151.88   South-southeast
16     168.75   South by east
17     185.62   South
18     185.63   South by west
19     202.50   South-southwest
20     219.37   Southwest by south
21     219.38   Southwest
22     236.25   Southwest by west
23     253.12   West-southwest
24     253.13   West by south
25     270.00   West
26     286.87   West by north
27     286.88   West-northwest
28     303.75   Northwest by west
29     320.62   Northwest
30     320.63   Northwest by north
31     337.50   North-northwest
32     354.37   North by west
 1     354.38   North

```



## Liberty BASIC


```lb
dim point$( 32)

for i =1 to 32
    read d$: point$( i) =d$
next i

for i = 0 to 32
    heading = i *11.25
    if ( i mod 3) =1 then
        heading = heading +5.62
    else
        if ( i mod 3) =2 then heading = heading -5.62
    end if
    ind = i mod 32 +1
    print ind, compasspoint$( heading), heading
next i

end

function compasspoint$( h)
    x = h /11.25 +1.5
    if (x >=33.0) then x =x -32.0
    compasspoint$ = point$( int( x))
end function

data  "North             ", "North by east     ", "North-northeast   "
data  "Northeast by north", "Northeast         ", "Northeast by east ", "East-northeast    "
data  "East by north     ", "East              ", "East by south     ", "East-southeast    "
data  "Southeast by east ", "Southeast         ", "Southeast by south", "South-southeast   "
data  "South by east     ", "South             ", "South by west     ", "South-southwest   "
data  "Southwest by south", "Southwest         ", "Southwest by west ", "West-southwest    "
data  "West by south     ", "West              ", "West by north     ", "West-northwest    "
data  "Northwest by west ", "Northwest         ", "Northwest by north", "North-northwest   "
data  "North by west
```


Output:

```txt
1             North                       0
2             North by east               16.87
3             North-northeast             16.88
4             Northeast by north          33.75
5             Northeast                   50.62
6             Northeast by east           50.63
7             East-northeast              67.5
8             East by north               84.37
9             East                        84.38
10            East by south               101.25
11            East-southeast              118.12
12            Southeast by east           118.13
13            Southeast                   135
14            Southeast by south          151.87
15            South-southeast             151.88
16            South by east               168.75
17            South                       185.62
18            South by west               185.63
19            South-southwest             202.5
20            Southwest by south          219.37
21            Southwest                   219.38
22            Southwest by west           236.25
23            West-southwest              253.12
24            West by south               253.13
25            West                        270
26            West by north               286.87
27            West-northwest              286.88
28            Northwest by west           303.75
29            Northwest                   320.62
30            Northwest by north          320.63
31            North-northwest             337.5
32            North by west               354.37
1             North                       354.38
```



## K

The representation of the names was inspired by Tcl (etc.).


```K
   d:("N;Nbe;N-ne;Nebn;Ne;Nebe;E-ne;Ebn;")
   d,:("E;Ebs;E-se;Sebe;Se;Sebs;S-se;Sbe;")
   d,:("S;Sbw;S-sw;Swbs;Sw;Swbw;W-sw;Wbs;")
   d,:("W;Wbn;W-nw;Nwbw;Nw;Nwbn;N-nw;Nbw;N")

   split:{1_'(&x=y)_ x:y,x}
   dd:split[d;";"]

   / lookup table
   s1:"NEWSnewsb-"
   s2:("North";"East";"West";"South";"north";"east";"west";"south";" by ";"-")
   c:.({`$x}'s1),'{`$x}'s2   / create the dictionary
   cc:{,/{$c[`$$x]}'x}       / lookup function

   / calculate the degrees
   f:{m:x!3;(11.25*x)+:[1=m;+5.62;2=m;-5.62;0]}
```


The table:

```K
   `0:{((2$(1+x!32)),"  ",(-19$cc@dd[x]),(6.2$f@x))}'!#dd
 1  North                0.00
 2  North by east       16.87
 3  North-northeast     16.88
 4  Northeast by north  33.75
 5  Northeast           50.62
 6  Northeast by east   50.63
 7  East-northeast      67.50
 8  East by north       84.37
 9  East                84.38
10  East by south      101.25
11  East-southeast     118.12
12  Southeast by east  118.13
13  Southeast          135.00
14  Southeast by south 151.87
15  South-southeast    151.88
16  South by east      168.75
17  South              185.62
18  South by west      185.63
19  South-southwest    202.50
20  Southwest by south 219.37
21  Southwest          219.38
22  Southwest by west  236.25
23  West-southwest     253.12
24  West by south      253.13
25  West               270.00
26  West by north      286.87
27  West-northwest     286.88
28  Northwest by west  303.75
29  Northwest          320.62
30  Northwest by north 320.63
31  North-northwest    337.50
32  North by west      354.37
 1  North              354.38
```



## Lasso


```Lasso
define pointsarray() => {
	local(points = array)
	loop(-from=0,-to=32) => {
		local(heading = loop_count * 11.25)
		if(loop_count % 3 == 1) => {
			#heading += 5.62
		else(loop_count % 3 == 2)
			#heading -= 5.62
		}
		#points->insert(#heading)
	}
	return #points
}
define compassShort => array(
	'N','Nbe','N-ne','Nebn','Ne','Nebe','E-ne','Ebn',
	'E','Ebs','E-se','Sebe','Se','Sebs','S-se','Sbe',
	'S','Sbw','S-sw','Swbs','Sw','Swbw','W-sw','Wbs',
	'W','Wbn','W-nw','Nwbw','Nw','Nwbn','N-nw','Nbw', 'N')
define compassLong(short::string) => {
	local(o = string)
	with i in #short->values do => { #o->append(compassLongProcessor(#i)) }
	return #o
}
define compassLongProcessor(char::string) => {
	#char == 'N' ? return #char + 'orth'
	#char == 'S' ? return #char + 'outh'
	#char == 'E' ? return #char + 'ast'
	#char == 'W' ? return #char + 'est'
	#char == 'b' ? return ' by '
	#char == '-' ? return '-'
}
// test output points as decimals
//pointsarray

// test output the array of text values
//compassShort

// test output the long names of the text values
//with s in compassShort do => {^ compassLong(#s) + '\r' ^}

'angle  | box  | compass point
---------------------------------
'
local(counter = 0)
with p in pointsarray do => {^
	local(pformatted = #p->asString(-precision=2))
	while(#pformatted->size < 6) => { #pformatted->append(' ') }
	#counter += 1
	#counter > 32 ? #counter = 1
	#pformatted + ' |  ' + (#counter < 10 ? ' ') + #counter + '  | ' + compassLong(compassShort->get(#counter)) + '\r'

^}
```


{{out}}

```txt
angle  | box  | compass point
---------------------------------
0.00   |   1  | North
16.87  |   2  | North by east
16.88  |   3  | North-northeast
33.75  |   4  | Northeast by north
50.62  |   5  | Northeast
50.63  |   6  | Northeast by east
67.50  |   7  | East-northeast
84.37  |   8  | East by north
84.38  |   9  | East
101.25 |  10  | East by south
118.12 |  11  | East-southeast
118.13 |  12  | Southeast by east
135.00 |  13  | Southeast
151.87 |  14  | Southeast by south
151.88 |  15  | South-southeast
168.75 |  16  | South by east
185.62 |  17  | South
185.63 |  18  | South by west
202.50 |  19  | South-southwest
219.37 |  20  | Southwest by south
219.38 |  21  | Southwest
236.25 |  22  | Southwest by west
253.12 |  23  | West-southwest
253.13 |  24  | West by south
270.00 |  25  | West
286.87 |  26  | West by north
286.88 |  27  | West-northwest
303.75 |  28  | Northwest by west
320.62 |  29  | Northwest
320.63 |  30  | Northwest by north
337.50 |  31  | North-northwest
354.37 |  32  | North by west
354.38 |   1  | North
```



## LLVM

{{trans|C}}

```llvm
; This is not strictly LLVM, as it uses the C library function "printf".
; LLVM does not provide a way to print values, so the alternative would be
; to just load the string into memory, and that would be boring.

; Additional comments have been inserted, as well as changes made from the output produced by clang such as putting more meaningful labels for the jumps

;--- The declarations for the external C functions
declare i32 @printf(i8*, ...)

$"COMPASS_STR" = comdat any
$"OUTPUT_STR" = comdat any

@main.degrees = private unnamed_addr constant [33 x double] [double 0.000000e+00, double 1.687000e+01, double 1.688000e+01, double 3.375000e+01, double 5.062000e+01, double 5.063000e+01, double 6.750000e+01, double 8.437000e+01, double 0x40551851EB851EB8, double 1.012500e+02, double 1.181200e+02, double 1.181300e+02, double 1.350000e+02, double 1.518700e+02, double 1.518800e+02, double 1.687500e+02, double 1.856200e+02, double 1.856300e+02, double 2.025000e+02, double 2.193700e+02, double 2.193800e+02, double 2.362500e+02, double 2.531200e+02, double 2.531300e+02, double 2.700000e+02, double 2.868700e+02, double 2.868800e+02, double 3.037500e+02, double 3.206200e+02, double 3.206300e+02, double 3.375000e+02, double 3.543700e+02, double 3.543800e+02], align 16
@"COMPASS_STR" = linkonce_odr unnamed_addr constant [727 x i8] c"North                 North by east         North-northeast       Northeast by north    Northeast             Northeast by east     East-northeast        East by north         East                  East by south         East-southeast        Southeast by east     Southeast             Southeast by south    South-southeast       South by east         South                 South by west         South-southwest       Southwest by south    Southwest             Southwest by west     West-southwest        West by south         West                  West by north         West-northwest        Northwest by west     Northwest             Northwest by north    North-northwest       North by west         North                 \00", comdat, align 1
@"OUTPUT_STR" = linkonce_odr unnamed_addr constant [19 x i8] c"%2d  %.22s  %6.2f\0A\00", comdat, align 1

; Function Attrs: noinline nounwind optnone uwtable
define i32 @main() #0 {
  %1 = alloca i32, align 4                  ;-- allocate i
  %2 = alloca i32, align 4                  ;-- allocate j
  %3 = alloca [33 x double], align 16       ;-- allocate degrees
  %4 = alloca i8*, align 8                  ;-- allocate names
  %5 = bitcast [33 x double]* %3 to i8*
  call void @llvm.memcpy.p0i8.p0i8.i64(i8* %5, i8* bitcast ([33 x double]* @main.degrees to i8*), i64 264, i32 16, i1 false)
  store i8* getelementptr inbounds ([727 x i8], [727 x i8]* @"COMPASS_STR", i32 0, i32 0), i8** %4, align 8
  store i32 0, i32* %1, align 4             ;-- i = 0
  br label %loop

loop:
  %6 = load i32, i32* %1, align 4           ;-- load i
  %7 = icmp slt i32 %6, 33                  ;-- i < 33
  br i1 %7, label %loop_body, label %exit

loop_body:
  %8 = load i32, i32* %1, align 4           ;-- load i
  %9 = sext i32 %8 to i64                   ;-- sign extend i
  %10 = getelementptr inbounds [33 x double], [33 x double]* %3, i64 0, i64 %9  ;-- calculate index of degrees[i]
  %11 = load double, double* %10, align 8   ;-- load degrees[i]
  %12 = fmul double %11, 3.200000e+01       ;-- degrees[i] * 32
  %13 = fdiv double %12, 3.600000e+02       ;-- degrees[i] * 32 / 360.0
  %14 = fadd double 5.000000e-01, %13       ;-- 0.5 + degrees[i] * 32 / 360.0
  %15 = fptosi double %14 to i32            ;-- convert floating point to integer
  store i32 %15, i32* %2, align 4           ;-- write result to j

  %16 = load i8*, i8** %4, align 8          ;-- load names
  %17 = load i32, i32* %2, align 4          ;-- load j
  %18 = srem i32 %17, 32                    ;-- j % 32
  %19 = mul nsw i32 %18, 22                 ;-- (j % 32) * 22
  %20 = sext i32 %19 to i64                 ;-- sign extend the result
  %21 = getelementptr inbounds i8, i8* %16, i64 %20 ;-- load names at the calculated offset
  %22 = load i32, i32* %2, align 4          ;-- load j
  %23 = srem i32 %22, 32                    ;-- j % 32
  %24 = add nsw i32 %23, 1                  ;-- (j % 32) + 1
  %25 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([19 x i8], [19 x i8]* @"OUTPUT_STR", i32 0, i32 0), i32 %24, i8* %21, double %11)

  %26 = load i32, i32* %1, align 4          ;-- load i
  %27 = add nsw i32 %26, 1                  ;-- increment i
  store i32 %27, i32* %1, align 4           ;-- store i
  br label %loop

exit:
  ret i32 0
}

; Function Attrs: argmemonly nounwind
declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i32, i1) #1

attributes #0 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="false" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { argmemonly nounwind }
```

{{out}}

```txt
 1  North                     0.00
 2  North by east            16.87
 3  North-northeast          16.88
 4  Northeast by north       33.75
 5  Northeast                50.62
 6  Northeast by east        50.63
 7  East-northeast           67.50
 8  East by north            84.37
 9  East                     84.38
10  East by south           101.25
11  East-southeast          118.12
12  Southeast by east       118.13
13  Southeast               135.00
14  Southeast by south      151.87
15  South-southeast         151.88
16  South by east           168.75
17  South                   185.62
18  South by west           185.63
19  South-southwest         202.50
20  Southwest by south      219.37
21  Southwest               219.38
22  Southwest by west       236.25
23  West-southwest          253.12
24  West by south           253.13
25  West                    270.00
26  West by north           286.87
27  West-northwest          286.88
28  Northwest by west       303.75
29  Northwest               320.62
30  Northwest by north      320.63
31  North-northwest         337.50
32  North by west           354.37
 1  North                   354.38
```



## Logo


```logo
; List of abbreviated compass point labels
make "compass_points [ N NbE N-NE NEbN NE NEbE E-NE EbN
                       E EbS E-SE SEbE SE SEbS S-SE SbE
                       S SbW S-SW SWbS SW SWbW W-SW WbS
                       W WbN W-NW NWbW NW NWbN N-NW NbW ]

; List of angles to test
make "test_angles [  0.00  16.87  16.88  33.75  50.62  50.63  67.50
                    84.37  84.38 101.25 118.12 118.13 135.00 151.87
                   151.88 168.75 185.62 185.63 202.50 219.37 219.38
                   236.25 253.12 253.13 270.00 286.87 286.88 303.75
                   320.62 320.63 337.50 354.37 354.38 ]

; make comparisons case-sensitive
make "caseignoredp "false

; String utilities: search and replace
to replace_in :src :from :to
  output map [ ifelse equalp ? :from [:to] [?] ] :src
end

; pad with spaces
to pad :string :length
  output cascade [lessp :length count ?] [word ? "\ ] :string
end

; capitalize first letter
to capitalize :string
  output word (uppercase first :string) butfirst :string
end

; convert compass point abbreviation to full text of label
to expand_point :abbr
  foreach [[N north] [E east] [S south] [W west] [b \ by\ ]] [
    make "abbr replace_in :abbr (first ?) (last ?)
  ]
  output capitalize :abbr
end

; modulus function that returns 1..N instead of 0..N-1
to adjusted_modulo :n :d
  output sum 1 modulo (difference :n 1) :d
end

; convert a compass angle from degrees into a box index (1..32)
to compass_point :degrees
  make "degrees modulo :degrees 360
  output adjusted_modulo (sum 1 int quotient (sum :degrees 5.625) 11.25) 32
end

;  Now output the table of test data
print (sentence (pad "Degrees 7) "\| (pad "Closest\ Point 18) "\| "Index )
foreach :test_angles [
  local "index
  make "index compass_point ?
  local "abbr
  make "abbr item :index :compass_points
  local "label
  make "label expand_point :abbr
  print (sentence (form ? 7 2) "\| (pad :label 18) "\| (form :index 2 0) )
]

; and exit
bye

```


Output:

```txt
Degrees | Closest Point      | Index
   0.00 | North              |  1
  16.87 | North by east      |  2
  16.88 | North-northeast    |  3
  33.75 | Northeast by north |  4
  50.62 | Northeast          |  5
  50.63 | Northeast by east  |  6
  67.50 | East-northeast     |  7
  84.37 | East by north      |  8
  84.38 | East               |  9
 101.25 | East by south      | 10
 118.12 | East-southeast     | 11
 118.13 | Southeast by east  | 12
 135.00 | Southeast          | 13
 151.87 | Southeast by south | 14
 151.88 | South-southeast    | 15
 168.75 | South by east      | 16
 185.62 | South              | 17
 185.63 | South by west      | 18
 202.50 | South-southwest    | 19
 219.37 | Southwest by south | 20
 219.38 | Southwest          | 21
 236.25 | Southwest by west  | 22
 253.12 | West-southwest     | 23
 253.13 | West by south      | 24
 270.00 | West               | 25
 286.87 | West by north      | 26
 286.88 | West-northwest     | 27
 303.75 | Northwest by west  | 28
 320.62 | Northwest          | 29
 320.63 | Northwest by north | 30
 337.50 | North-northwest    | 31
 354.37 | North by west      | 32
 354.38 | North              |  1
```



## Lua

{{trans|Logo}}

```lua
-- List of abbreviated compass point labels
compass_points = { "N", "NbE", "N-NE", "NEbN", "NE", "NEbE", "E-NE", "EbN",
                   "E", "EbS", "E-SE", "SEbE", "SE", "SEbS", "S-SE", "SbE",
                   "S", "SbW", "S-SW", "SWbS", "SW", "SWbW", "W-SW", "WbS",
                   "W", "WbN", "W-NW", "NWbW", "NW", "NWbN", "N-NW", "NbW" }

-- List of angles to test
test_angles = {  0.00,  16.87,  16.88,  33.75,  50.62,  50.63,  67.50,
                84.37,  84.38, 101.25, 118.12, 118.13, 135.00, 151.87,
               151.88, 168.75, 185.62, 185.63, 202.50, 219.37, 219.38,
               236.25, 253.12, 253.13, 270.00, 286.87, 286.88, 303.75,
               320.62, 320.63, 337.50, 354.37, 354.38 }


-- capitalize a string
function capitalize(s)
  return s:sub(1,1):upper() .. s:sub(2)
end

-- convert compass point abbreviation to full text of label
function expand_point(abbr)
  for from, to in pairs( { N="north", E="east", S="south", W="west",
                             b=" by " }) do
    abbr = abbr:gsub(from, to)
  end
  return capitalize(abbr)
end

-- modulus function that returns 1..N instead of 0..N-1
function adjusted_modulo(n, d)
  return 1 + (n - 1) % d
end

-- convert a compass angle from degrees into a box index (1..32)
function compass_point(degrees)
  degrees = degrees % 360
  return adjusted_modulo(1 + math.floor( (degrees+5.625) / 11.25), 32)
end

--  Now output the table of test data
header_format = "%-7s | %-18s | %s"
row_format = "%7.2f | %-18s | %2d"
print(header_format:format("Degrees", "Closest Point", "Index"))
for i, angle in ipairs(test_angles) do
  index = compass_point(angle)
  abbr  = compass_points[index]
  label  = expand_point(abbr)
  print(row_format:format(angle, label, index))
end
```


Output:

```txt
Degrees | Closest Point      | Index
   0.00 | North              |  1
  16.87 | North by east      |  2
  16.88 | North-northeast    |  3
  33.75 | Northeast by north |  4
  50.62 | Northeast          |  5
  50.63 | Northeast by east  |  6
  67.50 | East-northeast     |  7
  84.37 | East by north      |  8
  84.38 | East               |  9
 101.25 | East by south      | 10
 118.12 | East-southeast     | 11
 118.13 | Southeast by east  | 12
 135.00 | Southeast          | 13
 151.87 | Southeast by south | 14
 151.88 | South-southeast    | 15
 168.75 | South by east      | 16
 185.62 | South              | 17
 185.63 | South by west      | 18
 202.50 | South-southwest    | 19
 219.37 | Southwest by south | 20
 219.38 | Southwest          | 21
 236.25 | Southwest by west  | 22
 253.12 | West-southwest     | 23
 253.13 | West by south      | 24
 270.00 | West               | 25
 286.87 | West by north      | 26
 286.88 | West-northwest     | 27
 303.75 | Northwest by west  | 28
 320.62 | Northwest          | 29
 320.63 | Northwest by north | 30
 337.50 | North-northwest    | 31
 354.37 | North by west      | 32
 354.38 | North              |  1
```



## M2000 Interpreter

In a For Next loop we can change loop variable inside block, but the actual  loop hidden variable can't change so for next iteration we get the proper value.


```M2000 Interpreter

Module CheckIt {
      Locale 1033   'change decimal point char to dot.
      Form 80,50    ' set console to 80 characters by 50 lines
      \\ Function heading() get a positive double as degrees and return the  compass index (1 for North)
      Function heading(d) {
            d1=d div 11.25
            if d1 mod 3= 1 then d+=5.62 :d1=d div 11.25
            =d1 mod 32 +1
      }
      Dim wind$(1 to 32)
      wind$(1)="North", "North by east", "North-northeast", "Northeast by north", "Northeast"
      wind$(6)="Northeast by east", "East-northeast", "East by north", "East", "East by south", "East-southeast"
      wind$(12)="Southeast by east", "Southeast", "Southeast by south", "South-southeast", "South by east", "South"
      wind$(18)="South by west", "South-southwest", "Southwest by south", "Southwest", "Southwest by west", "West-southwest"
      wind$(24)="West by south", "West", "West by north", "West-northwest", "Northwest by west", "Northwest", "Northwest by north"
      wind$(31)="North-northwest", "North by west"
      oldvalue=-2
      newvalue=2
      Print " angle | box | compass point"
      Print "-------+-----+---------------------"
      For i=0 to 360 step 0.005
            newvalue=heading(i)
            if (newvalue mod 3) =2 then i+=5.62: newvalue=heading(i)
            if oldvalue<>newvalue then Print format$("{0:2:-6}°|  {1::-2} | {2}",i, newvalue, wind$(newvalue)) : oldvalue=newvalue : refresh
      Next i
}
CheckIt

```


{{out}}

```txt
 angle | box | compass point
-------+-----+---------------------
  0.00°|   1 | North
 16.87°|   2 | North by east
 16.88°|   3 | North-northeast
 33.75°|   4 | Northeast by north
 50.62°|   5 | Northeast
 50.63°|   6 | Northeast by east
 67.50°|   7 | East-northeast
 84.37°|   8 | East by north
 84.38°|   9 | East
101.25°|  10 | East by south
118.12°|  11 | East-southeast
118.13°|  12 | Southeast by east
135.00°|  13 | Southeast
151.87°|  14 | Southeast by south
151.88°|  15 | South-southeast
168.75°|  16 | South by east
185.62°|  17 | South
185.63°|  18 | South by west
202.50°|  19 | South-southwest
219.37°|  20 | Southwest by south
219.38°|  21 | Southwest
236.25°|  22 | Southwest by west
253.12°|  23 | West-southwest
253.13°|  24 | West by south
270.00°|  25 | West
286.87°|  26 | West by north
286.88°|  27 | West-northwest
303.75°|  28 | Northwest by west
320.62°|  29 | Northwest
320.63°|  30 | Northwest by north
337.50°|  31 | North-northwest
354.37°|  32 | North by west
354.38°|   1 | North
</pre >

=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
Map[List[Part[#,1], dirs[[Part[#,1]]], ToString@Part[#,2]<>"°"]&,
  Map[{Floor[Mod[ #+5.625 , 360]/11.25]+1,#}&,input] ]//TableForm
```


```txt
1	North			0.°
2	North by east		16.87°
3	North-northeast		16.88°
4	Northeast by north	33.75°
5	Northeast		50.62°
6	Northeast by east	50.63°
7	East-northeast		67.5°
8	East by north		84.37°
9	East			84.38°
10	East by Southeast	101.25°
11	East-southeast		118.12°
12	Southeast by east	118.13°
13	Southeast		135.°
14	Southeast by south	151.87°
15	South-southeast		151.88°
16	South by east		168.75°
17	South			185.62°
18	South by West		185.63°
19	South-southwest		202.5°
20	Southwest by south	219.37°
21	Southwest		219.38°
22	Southwest by west	236.25°
23	West-southwest		253.12°
24	West by south		253.13°
25	West			270.°
26	West by north		286.87°
27	West-northwest		286.88°
28	Northwest by west	303.75°
29	Northwest		320.62°
30	Northwest by north	320.63°
31	North-northwest		337.5°
32	North by west		354.37°
1	North			354.38°
```


=={{header|MATLAB}} / {{header|Octave}}==

```MATLAB
function b = compassbox(d)
    b = ceil(mod(d+360/64,360)*32/360);
end;
```

Output:

```txt
>> x=[0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38]';
printf(' angle : box\n'); printf('%6.2f : %2i\n',[x,compassbox(x)]');
 angle : box
  0.00 :  1
 16.87 :  2
 16.88 :  3
 33.75 :  4
 50.62 :  5
 50.63 :  6
 67.50 :  7
 84.37 :  8
 84.38 :  9
101.25 : 10
118.12 : 11
118.13 : 12
135.00 : 13
151.87 : 14
151.88 : 15
168.75 : 16
185.62 : 17
185.63 : 18
202.50 : 19
219.37 : 20
219.38 : 21
236.25 : 22
253.12 : 23
253.13 : 24
270.00 : 25
286.87 : 26
286.88 : 27
303.75 : 28
320.62 : 29
320.63 : 30
337.50 : 31
354.37 : 32
354.38 :  1
```


=={{header|Modula-2}}==

```modula2
MODULE BoxTheCompass;
FROM FormatString IMPORT FormatString;
FROM RealStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,Write,ReadChar;

PROCEDURE expand(cp : ARRAY OF CHAR);
VAR i : INTEGER = 0;
BEGIN
    WHILE cp[i] # 0C DO
        IF i=0 THEN
            CASE cp[i] OF
                'N': WriteString("North") |
                'E': WriteString("East")  |
                'S': WriteString("South") |
                'W': WriteString("West")  |
                'b': WriteString(" by ")
            ELSE
                WriteString("-");
            END;
        ELSE
            CASE cp[i] OF
                'N': WriteString("north") |
                'E': WriteString("east")  |
                'S': WriteString("south") |
                'W': WriteString("west")  |
                'b': WriteString(" by ")
            ELSE
                WriteString("-");
            END;
        END;
        INC(i)
    END;
END expand;

PROCEDURE FormatReal(r : REAL);
VAR
    buf : ARRAY[0..63] OF CHAR;
    u,v : INTEGER;
    w : REAL;
BEGIN
    u := TRUNC(r);
    w := r - FLOAT(u);
    v := TRUNC(100.0 * w);

    FormatString("%6i.%'02i", buf, u, v);
    WriteString(buf);
END FormatReal;

VAR
    cp : ARRAY[0..31] OF ARRAY[0..4] OF CHAR = {
        "N", "NbE", "N-NE", "NEbN", "NE", "NEbE", "E-NE", "EbN",
        "E", "EbS", "E-SE", "SEbE", "SE", "SEbS", "S-SE", "SbE",
        "S", "SbW", "S-SW", "SWbS", "SW", "SWbW", "W-SW", "WbS",
        "W", "WbN", "W-NW", "NWbW", "NW", "NWbN", "N-NW", "NbW"
    };
    buf : ARRAY[0..63] OF CHAR;
    i,index : INTEGER;
    heading : REAL;
BEGIN
    WriteString("Index  Degrees  Compass point");
    WriteLn;
    WriteString("-----  -------  -------------");
    WriteLn;
    FOR i:=0 TO 32 DO
        index := i MOD 32;
        heading := FLOAT(i) * 11.25;
        CASE i MOD 3 OF
            1: heading := heading + 5.62; |
            2: heading := heading - 5.62;
        ELSE
            (* empty *)
        END;
        FormatString("%2i  ", buf, index+1);
        WriteString(buf);
        FormatReal(heading);
        WriteString("   ");
        expand(cp[index]);
        WriteLn
    END;

    ReadChar
END BoxTheCompass.
```



## MUMPS

The TCL implementation was the starting point, but this isn't an exact translation.

```MUMPS
BOXING(DEGREE)
 ;This takes in a degree heading, nominally from 0 to 360, and returns the compass point name.
 QUIT:((DEGREE<0)||(DEGREE>360)) "land lubber can't read a compass"
 NEW DIRS,UNP,UNPACK,SEP,DIR,DOS,D,DS,I,F
 SET DIRS="N^NbE^N-NE^NEbN^NE^NEbE^E-NE^EbN^E^EbS^E-SE^SEbE^SE^SEbS^S-SE^SbE^"
 SET DIRS=DIRS_"S^SbW^S-SW^SWbS^SW^SWbW^W-SW^WbS^W^WbN^W-NW^NWbW^NW^NWbN^N-NW^NbW"
 SET UNP="NESWb"
 SET UNPACK="north^east^south^west^ by "
 SET SEP=360/$LENGTH(DIRS,"^")
 SET DIR=(DEGREE/SEP)+1.5
 SET DIR=$SELECT((DIR>33):DIR-32,1:DIR)
 SET DOS=$NUMBER(DIR-.5,0)
 SET D=$PIECE(DIRS,"^",DIR)
 SET DS=""
 FOR I=1:1:$LENGTH(D) DO
 . SET F=$FIND(UNP,$EXTRACT(D,I)) SET DS=DS_$SELECT((F>0):$PIECE(UNPACK,"^",F-1),1:$E(D,I))
 KILL DIRS,UNP,UNPACK,SEP,DIR,D,I,F
 QUIT DOS_"^"_DS

BOXWRITE
 NEW POINTS,UP,LO,DIR,P,X
 SET POINTS="0.0,16.87,16.88,33.75,50.62,50.63,67.5,84.37,84.38,101.25,118.12,118.13,135.0,151.87,"
 SET POINTS=POINTS_"151.88,168.75,185.62,185.63,202.5,219.37,219.38,236.25,253.12,253.13,270.0,286.87,"
 SET POINTS=POINTS_"286.88,303.75,320.62,320.63,337.5,354.37,354.38"
 SET UP="ABCDEFGHIJKLMNOPQRSTUVWXYZ"
 SET LO="abcdefghijklmnopqrstuvwxyz"
 FOR P=1:1:$LENGTH(POINTS,",") DO
 . SET X=$$BOXING($PIECE(POINTS,",",P))
 . ;Capitalize the initial letter of the direction
 . SET DIR=$PIECE(X,"^",2)
 . SET DIR=$TRANSLATE($EXTRACT(DIR,1),LO,UP)_$EXTRACT(DIR,2,$LENGTH(DIR))
 . WRITE $PIECE(X,"^"),?5,DIR,?40,$JUSTIFY($PIECE(POINTS,",",P),10,2),!
 KILL POINTS,UP,LO,DIR,P,X
 QUIT
```

<p>Output:
```txt

Debugger executing 'BOXWRITE^COMPASS'
1    North                                    0.00
2    North by east                           16.87
3    North-northeast                         16.88
4    Northeast by north                      33.75
5    Northeast                               50.62
6    Northeast by east                       50.63
7    East-northeast                          67.50
8    East by north                           84.37
9    East                                    84.38
10   East by south                          101.25
11   East-southeast                         118.12
12   Southeast by east                      118.13
13   Southeast                              135.00
14   Southeast by south                     151.87
15   South-southeast                        151.88
16   South by east                          168.75
17   South                                  185.62
18   South by west                          185.63
19   South-southwest                        202.50
20   Southwest by south                     219.37
21   Southwest                              219.38
22   Southwest by west                      236.25
23   West-southwest                         253.12
24   West by south                          253.13
25   West                                   270.00
26   West by north                          286.87
27   West-northwest                         286.88
28   Northwest by west                      303.75
29   Northwest                              320.62
30   Northwest by north                     320.63
31   North-northwest                        337.50
32   North by west                          354.37
1    North                                  354.38
```
</p>


## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref savelog symbols nobinary utf8

class RCBoxTheCompass

properties public constant
  _FULL = '_FULL'

properties indirect
  headings = Rexx
  rosepoints = Rexx

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
method RCBoxTheCompass() public

  setHeadings(makeHeadings)
  setRosepoints(makeRosepoints)

  return

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
method main(args = String[]) public static

  box = RCBoxTheCompass()

  cp = box.getCompassPoints
  loop r_ = 1 to cp[0]
    say cp[r_]
    end r_
  say

  hx = box.getHeadingsReport(box.getHeadings)
  loop r_ = 1 to hx[0]
    say hx[r_]
    end r_
  say

  return

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
method getDecimalAngle(degrees, minutes, seconds) public static returns Rexx

  degrees = degrees * 10 % 10
  minutes = minutes * 10 % 10
  angle = degrees + (minutes / 60 + (seconds / 60 / 60))

  return angle

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
method getDegreesMinutesSeconds(angle) public static returns Rexx

  degrees = angle * 100 % 100
  minutes = ((angle - degrees) * 60) * 100 % 100
  seconds = ((((angle - degrees) * 60) - minutes) * 60) * 100 % 100

  return degrees minutes seconds

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
method getHeadingsReport(bearings) public returns Rexx

  r_ = 0
  table = ''
  r_ = r_ + 1; table[0] = r_; table[r_] = 'Idx' -
                                          'Abbr' -
                                          'Compass Point'.left(20) -
                                          'Degrees'.right(10)

  loop h_ = 1 to bearings[0]
    heading = bearings[h_]
    index = getRosepointIndex(heading)
    parse getRosepoint(index) p_abbrev p_full

    r_ = r_ + 1; table[0] = r_; table[r_] = index.right(3) -
                                            p_abbrev.left(4) -
                                            p_full.left(20) -
                                            heading.format(6, 3).right(10)
    end h_

  return table

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
method getRosepointIndex(heading) public returns Rexx

  one_pt = 360 / 32
  hn = heading // 360
  hi = hn % one_pt

  if hn > hi * one_pt + one_pt / 2 then do
    hi = hi + 1 -- greater than max range for this point; bump to next point
    end

  idx = hi // 32 + 1 -- add one to get index into rosepoints indexed string

  return idx

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
method getRosepoint(index) public returns Rexx

  rp = getRosepoints
  return rp[index] rp[index, _FULL]

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
method makeRosepoints() private returns Rexx

  p_ = 0
  rp = ''
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'N';    rp[p_, _FULL] = 'North'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'NbE';  rp[p_, _FULL] = 'North by East'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'NNE';  rp[p_, _FULL] = 'North-Northeast'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'NEbn'; rp[p_, _FULL] = 'Northeast by North'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'NE';   rp[p_, _FULL] = 'Northeast'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'NEbE'; rp[p_, _FULL] = 'Northeast by East'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'ENE';  rp[p_, _FULL] = 'East-Northeast'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'EbN';  rp[p_, _FULL] = 'East by North'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'E';    rp[p_, _FULL] = 'East'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'EbS';  rp[p_, _FULL] = 'East by South'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'ESE';  rp[p_, _FULL] = 'East-Southeast'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'SEbE'; rp[p_, _FULL] = 'Southeast by East'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'SE';   rp[p_, _FULL] = 'Southeast'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'SEbS'; rp[p_, _FULL] = 'Southeast by South'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'SSE';  rp[p_, _FULL] = 'South-Southeast'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'SbE';  rp[p_, _FULL] = 'South by East'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'S';    rp[p_, _FULL] = 'South'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'SbW';  rp[p_, _FULL] = 'South by West'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'SSW';  rp[p_, _FULL] = 'South-Southwest'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'SWbS'; rp[p_, _FULL] = 'Southwest by South'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'SW';   rp[p_, _FULL] = 'Southwest'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'SWbW'; rp[p_, _FULL] = 'Southwest by West'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'WSW';  rp[p_, _FULL] = 'Southwest'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'WbS';  rp[p_, _FULL] = 'West by South'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'W';    rp[p_, _FULL] = 'West'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'WbN';  rp[p_, _FULL] = 'West by North'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'WNW';  rp[p_, _FULL] = 'West-Northwest'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'NWbW'; rp[p_, _FULL] = 'Northwest by West'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'NW';   rp[p_, _FULL] = 'Northwest'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'NWbN'; rp[p_, _FULL] = 'Northwest by North'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'NNW';  rp[p_, _FULL] = 'North-Northwest'
  p_ = p_ + 1; rp[0] = p_; rp[p_] = 'NbW';  rp[p_, _FULL] = 'North by West'

  return rp

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
method makeHeadings() private returns Rexx

  hdg = ''
  hdg[0] = 0
  points = 32
  loop i_ = 0 to points
    heading = i_ * 360 / points

    select case i_ // 3
      when 1 then heading_h = heading + 5.62
      when 2 then heading_h = heading - 5.62
      otherwise   heading_h = heading
      end

    ix = hdg[0] + 1; hdg[0] = ix; hdg[ix] = heading_h
    end i_

  return hdg

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
method getCompassPoints() public returns Rexx

  r_ = 0
  table = ''
  r_ = r_ + 1; table[0] = r_; table[r_] = 'Idx' -
                                          'Abbr' -
                                          'Compass Point'.left(20) -
                                          'Low (Deg.)'.right(10) -
                                          'Mid (Deg.)'.right(10) -
                                          'Hi (Deg.)'.right(10)

  -- one point of the compass is 360 / 32 (11.25) degrees
  -- using functions to calculate this just for fun
  one_pt = 360 / 32
  parse getDegreesMinutesSeconds(one_pt / 2) ad am as .
  points = 32
  loop h_ = 0 to points - 1
    heading = h_ * 360 / points
    hmin = heading - getDecimalAngle(ad, am, as)
    hmax = heading + getDecimalAngle(ad, am, as)

    if hmin < 0 then do
      hmin = hmin + 360
      end
    if hmax >= 360 then do
      hmax = hmax - 360
      end

    index = getRosepointIndex(heading)
    parse getRosepoint(index) p_abbrev p_full
    r_ = r_ + 1; table[0] = r_; table[r_] = index.right(3) -
                                            p_abbrev.left(4) -
                                            p_full.left(20) -
                                            hmin.format(6, 3).right(10) -
                                            heading.format(6, 3).right(10) -
                                            hmax.format(6, 3).right(10)
    end h_

  return table

```


;Output

```txt
Idx Abbr Compass Point        Low (Deg.) Mid (Deg.)  Hi (Deg.)
  1 N    North                   354.375      0.000      5.625
  2 NbE  North by East             5.625     11.250     16.875
  3 NNE  North-Northeast          16.875     22.500     28.125
  4 NEbn Northeast by North       28.125     33.750     39.375
  5 NE   Northeast                39.375     45.000     50.625
  6 NEbE Northeast by East        50.625     56.250     61.875
  7 ENE  East-Northeast           61.875     67.500     73.125
  8 EbN  East by North            73.125     78.750     84.375
  9 E    East                     84.375     90.000     95.625
 10 EbS  East by South            95.625    101.250    106.875
 11 ESE  East-Southeast          106.875    112.500    118.125
 12 SEbE Southeast by East       118.125    123.750    129.375
 13 SE   Southeast               129.375    135.000    140.625
 14 SEbS Southeast by South      140.625    146.250    151.875
 15 SSE  South-Southeast         151.875    157.500    163.125
 16 SbE  South by East           163.125    168.750    174.375
 17 S    South                   174.375    180.000    185.625
 18 SbW  South by West           185.625    191.250    196.875
 19 SSW  South-Southwest         196.875    202.500    208.125
 20 SWbS Southwest by South      208.125    213.750    219.375
 21 SW   Southwest               219.375    225.000    230.625
 22 SWbW Southwest by West       230.625    236.250    241.875
 23 WSW  Southwest               241.875    247.500    253.125
 24 WbS  West by South           253.125    258.750    264.375
 25 W    West                    264.375    270.000    275.625
 26 WbN  West by North           275.625    281.250    286.875
 27 WNW  West-Northwest          286.875    292.500    298.125
 28 NWbW Northwest by West       298.125    303.750    309.375
 29 NW   Northwest               309.375    315.000    320.625
 30 NWbN Northwest by North      320.625    326.250    331.875
 31 NNW  North-Northwest         331.875    337.500    343.125
 32 NbW  North by West           343.125    348.750    354.375

Idx Abbr Compass Point           Degrees
  1 N    North                     0.000
  2 NbE  North by East            16.870
  3 NNE  North-Northeast          16.880
  4 NEbn Northeast by North       33.750
  5 NE   Northeast                50.620
  6 NEbE Northeast by East        50.630
  7 ENE  East-Northeast           67.500
  8 EbN  East by North            84.370
  9 E    East                     84.380
 10 EbS  East by South           101.250
 11 ESE  East-Southeast          118.120
 12 SEbE Southeast by East       118.130
 13 SE   Southeast               135.000
 14 SEbS Southeast by South      151.870
 15 SSE  South-Southeast         151.880
 16 SbE  South by East           168.750
 17 S    South                   185.620
 18 SbW  South by West           185.630
 19 SSW  South-Southwest         202.500
 20 SWbS Southwest by South      219.370
 21 SW   Southwest               219.380
 22 SWbW Southwest by West       236.250
 23 WSW  Southwest               253.120
 24 WbS  West by South           253.130
 25 W    West                    270.000
 26 WbN  West by North           286.870
 27 WNW  West-Northwest          286.880
 28 NWbW Northwest by West       303.750
 29 NW   Northwest               320.620
 30 NWbN Northwest by North      320.630
 31 NNW  North-Northwest         337.500
 32 NbW  North by West           354.370
  1 N    North                   354.380

```



## Nim


```nim
import strfmt

const names = [
  "North", "North by east", "North-northeast", "Northeast by north",
  "Northeast", "Northeast by east", "East-northeast", "East by north",
  "East", "East by south", "East-southeast", "Southeast by east",
  "Southeast", "Southeast by south","South-southeast", "South by east",
  "South", "South by west", "South-southwest", "Southwest by south",
  "Southwest", "Southwest by west", "West-southwest", "West by south",
  "West", "West by north", "West-northwest", "Northwest by west",
  "Northwest", "Northwest by north", "North-northwest", "North by west", "North"]

for i in 0..32:
  let j = i mod 32
  var d = float(i) * 11.25
  if i mod 3 == 1: d += 5.62
  if i mod 3 == 2: d -= 5.62
  printlnfmt "{:2} {:18} {:>6.2f}", j + 1, names[j], d
```

Output:

```txt
 1 North                0
 2 North by east       16.87
 3 North-northeast     16.88
 4 Northeast by north  33.75
 5 Northeast           50.62
 6 Northeast by east   50.63
 7 East-northeast      67.50
 8 East by north       84.37
 9 East                84.38
10 East by south      101.25
11 East-southeast     118.12
12 Southeast by east  118.13
13 Southeast          135.00
14 Southeast by south 151.87
15 South-southeast    151.88
16 South by east      168.75
17 South              185.62
18 South by west      185.63
19 South-southwest    202.50
20 Southwest by south 219.37
21 Southwest          219.38
22 Southwest by west  236.25
23 West-southwest     253.12
24 West by south      253.13
25 West               270.00
26 West by north      286.87
27 West-northwest     286.88
28 Northwest by west  303.75
29 Northwest          320.62
30 Northwest by north 320.63
31 North-northwest    337.50
32 North by west      354.37
 1 North              354.38
```



## Objeck


```objeck

﻿class BoxCompass {
  function : Main(args : String[]) ~ Nil {
    points := [
      "North             ", "North by east     ", "North-northeast   ",
      "Northeast by north", "Northeast         ", "Northeast by east ", "East-northeast    ",
      "East by north     ", "East              ", "East by south     ", "East-southeast    ",
      "Southeast by east ", "Southeast         ", "Southeast by south", "South-southeast   ",
      "South by east     ", "South             ", "South by west     ", "South-southwest   ",
      "Southwest by south", "Southwest         ", "Southwest by west ", "West-southwest    ",
      "West by south     ", "West              ", "West by north     ", "West-northwest    ",
      "Northwest by west ", "Northwest         ", "Northwest by north", "North-northwest   ",
      "North by west     " ];

    for(i := 0; i<= 32; i += 1;) {
      heading := i * 11.25;
      select(i % 3) {
        label 1: {
          heading += 5.62;
        }

        label 2: {
          heading -= 5.62;
        }
      };

      IO.Console->Print((i % 32) + 1)->Print('\t')->Print(points[GetPoint(heading)])
        ->Print('\t')->PrintLine(heading);
    };
  }

  function : GetPoint(degrees : Float) ~ Int {
    return (degrees / 11.25 + 0.5)->Floor()->As(Int) % 32;
  }
}

```


Output

```txt

1	North             	0
2	North by east     	16.87
3	North-northeast   	16.88
4	Northeast by north	33.75
5	Northeast         	50.62
6	Northeast by east 	50.63
7	East-northeast    	67.5
8	East by north     	84.37
9	East              	84.38
10	East by south     	101.25
11	East-southeast    	118.12
12	Southeast by east 	118.13
13	Southeast         	135
14	Southeast by south	151.87
15	South-southeast   	151.88
16	South by east     	168.75
17	South             	185.62
18	South by west     	185.63
19	South-southwest   	202.5
20	Southwest by south	219.37
21	Southwest         	219.38
22	Southwest by west 	236.25
23	West-southwest    	253.12
24	West by south     	253.13
25	West              	270
26	West by north     	286.87
27	West-northwest    	286.88
28	Northwest by west 	303.75
29	Northwest         	320.62
30	Northwest by north	320.63
31	North-northwest   	337.5
32	North by west     	354.37
1	North             	354.38

```



## OCaml


```ocaml

let test_cases = [0.0; 16.87; 16.88; 33.75; 50.62; 50.63; 67.5;
                  84.37; 84.38; 101.25; 118.12; 118.13; 135.0;
                  151.87; 151.88; 168.75; 185.62; 185.63; 202.5;
                  219.37; 219.38; 236.25; 253.12; 253.13; 270.0;
                  286.87; 286.88; 303.75; 320.62; 320.63; 337.5;
                  354.37; 354.38];;

let directions = ["North"; "North by East"; "North-Northeast";
                  "Northeast by North"; "Northeast";
                  "Northeast by East"; "East-Northeast"; "East by North";
                  "East"; "East by South"; "East-Southeast";
                  "Southeast by East"; "Southeast"; "Southeast by South";
                  "South-Southeast"; "South by East"; "South";
                  "South by West"; "South-Southwest"; "Southwest by South";
                  "Southwest"; "Southwest by West"; "West-Southwest";
                  "West by South"; "West"; "West by North";
                  "West-Northwest"; "Northwest by West"; "Northwest";
                  "Northwest by North"; "North-Northwest"; "North by West";];;

let get_direction_index input =
  let shifted = (input +. 5.6201) in
  let shifted = if shifted > 360. then shifted -. 360. else shifted in
  int_of_float (shifted /. 11.25);;

let print_direction input =
  let index = get_direction_index input in
  let direction = List.nth directions index in
  let test = Printf.printf "%3d %-20s %.2f\n" (index + 1) direction in
  test input;;

List.iter (print_direction) test_cases;;


```

Sample output:

```txt

  1 North                0.00
  2 North by East        16.87
  3 North-Northeast      16.88
  4 Northeast by North   33.75
  5 Northeast            50.62
  6 Northeast by East    50.63
  7 East-Northeast       67.50
  8 East by North        84.37
  9 East                 84.38
 10 East by South        101.25
 11 East-Southeast       118.12
 12 Southeast by East    118.13
 13 Southeast            135.00
 14 Southeast by South   151.87
 15 South-Southeast      151.88
 16 South by East        168.75
 17 South                185.62
 18 South by West        185.63
 19 South-Southwest      202.50
 20 Southwest by South   219.37
 21 Southwest            219.38
 22 Southwest by West    236.25
 23 West-Southwest       253.12
 24 West by South        253.13
 25 West                 270.00
 26 West by North        286.87
 27 West-Northwest       286.88
 28 Northwest by West    303.75
 29 Northwest            320.62
 30 Northwest by North   320.63
 31 North-Northwest      337.50
 32 North by West        354.37
  1 North                354.38

```



## OoRexx


```OOREXX
/* Rexx */

Do
  globs = '!DEG !MIN !SEC !FULL'
  Drop !DEG !MIN !SEC !FULL
  sign. = ''
  sign.!DEG = 'c2b0'x     -- degree sign  : U+00B0
  sign.!MIN = 'e280b2'x   -- prime        : U+2032
  sign.!SEC = 'e280b3'x   -- double prime : U+2033
  points. = ''

  Call display_compass_points

  Call display_sample
  Say

  headings. = ''
  headings.0 = 0
  Call make_headings

  Call flush_queue
  Do h_ = 1 to headings.0
    Queue headings.h_
    End h_

  Call display_sample
  Say

  Return
End
Exit

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
flush_queue:
Procedure
Do
  Do q_ = 1 to queued()
    Parse pull .
    End q_
  Return
End
Exit

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
display_sample:
Procedure Expose points. sign. (globs)
Do

  Do q_ = 1 to queued()
    Parse pull heading
    index = get_index(heading)
    Parse Value get_point(index) with p_abbrev p_full
    Say index~right(3),
        p_abbrev~left(4) p_full~left(20),
        heading~format(5, 3) || sign.!DEG '('format_degrees_minutes_seconds(heading)')',
        ''
    End q_

  Return
End
Exit

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
display_compass_points:
Procedure Expose points. sign. (globs)
Do

  points = 32
  one_pt = 360 / points

  Do h_ = 0 to points - 1
    heading = h_ * 360 / points
    hmin = heading - one_pt / 2
    hmax = heading + one_pt / 2

    If hmin < 0 then Do
      hmin = hmin + 360
      End
    If hmax >= 360 then Do
      hmax = hmax - 360
      End

    index = (h_ // points) + 1
    Parse Value get_point(index) with p_abbrev p_full

    Say index~right(3),
        p_abbrev~left(4) p_full~left(20),
        hmin~format(5, 3)    || sign.!DEG '('format_degrees_minutes_seconds(hmin)')',
        heading~format(5, 3) || sign.!DEG '('format_degrees_minutes_seconds(heading)')',
        hmax~format(5, 3)    || sign.!DEG '('format_degrees_minutes_seconds(hmax)')',
        ''
    End h_
  Say

  Return
End
Exit

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
make_headings:
Procedure Expose headings.
Do
  points = 32
  Do i_ = 0 to points
    heading = i_ * 360 / 32

    it = i_ // 3
    Select
      When it = 1 then Do
        heading_h = heading + 5.62
        End

      When it = 2 then Do
        heading_h = heading - 5.62
        End

      Otherwise Do
        heading_h = heading
        End
      End

    index = (i_ // points) + 1
    ix = headings.0 + 1; headings.0 = ix; headings.ix = heading_h
    End i_

  Return
End
Exit

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
get_index:
Procedure
Do
  Parse Arg heading .

  one_pt = 360 / 32
  hn = heading // 360
  hi = hn % one_pt

  If hn > hi * one_pt + one_pt / 2 then Do
    hi = hi + 1 -- greater than max range for this point; bump to next point
    End

  idx = hi // 32 + 1 -- add one to get index into points. stem

  Return idx
End
Exit

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
get_point:
Procedure Expose points. sign. (globs)
Do
  Parse arg index .

  Call get_points

  Return points.index points.index.!FULL
End
Exit

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
get_points:
Procedure Expose points. sign. (globs)
Do

  Drop !FULL

  points. = ''
  p_ = 0
  p_ = p_ + 1; points.0 = p_; points.p_ = 'N';    points.p_.!FULL = 'North'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'NbE';  points.p_.!FULL = 'North by East'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'NNE';  points.p_.!FULL = 'North-Northeast'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'NEbn'; points.p_.!FULL = 'Northeast by North'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'NE';   points.p_.!FULL = 'Northeast'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'NEbE'; points.p_.!FULL = 'Northeast by East'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'ENE';  points.p_.!FULL = 'East-Northeast'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'EbN';  points.p_.!FULL = 'East by North'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'E';    points.p_.!FULL = 'East'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'EbS';  points.p_.!FULL = 'East by South'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'ESE';  points.p_.!FULL = 'East-Southeast'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'SEbE'; points.p_.!FULL = 'Southeast by East'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'SE';   points.p_.!FULL = 'Southeast'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'SEbS'; points.p_.!FULL = 'Southeast by South'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'SSE';  points.p_.!FULL = 'South-Southeast'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'SbE';  points.p_.!FULL = 'South by East'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'S';    points.p_.!FULL = 'South'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'SbW';  points.p_.!FULL = 'South by West'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'SSW';  points.p_.!FULL = 'South-Southwest'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'SWbS'; points.p_.!FULL = 'Southwest by South'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'SW';   points.p_.!FULL = 'Southwest'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'SWbW'; points.p_.!FULL = 'Southwest by West'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'WSW';  points.p_.!FULL = 'Southwest'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'WbS';  points.p_.!FULL = 'West by South'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'W';    points.p_.!FULL = 'West'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'WbN';  points.p_.!FULL = 'West by North'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'WNW';  points.p_.!FULL = 'West-Northwest'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'NWbW'; points.p_.!FULL = 'Northwest by West'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'NW';   points.p_.!FULL = 'Northwest'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'NWbN'; points.p_.!FULL = 'Northwest by North'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'NNW';  points.p_.!FULL = 'North-Northwest'
  p_ = p_ + 1; points.0 = p_; points.p_ = 'NbW';  points.p_.!FULL = 'North by West'

  Return
End
Exit

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
get_decimal_angle:
Procedure Expose sign. (globs)
Do
  Parse Arg degrees ., minutes ., seconds .

  degrees = degrees * 10 % 10
  minutes = minutes * 10 % 10

  angle = degrees + minutes / 60 + seconds / 60 / 60

  Return angle
End
Exit

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
format_decimal_angle:
Procedure Expose sign. (globs)
Do
  Parse Arg degrees ., minutes ., seconds .

  Return get_decimal_angle(degrees, minutes, seconds)~format(5, 3) || sign.!DEG
End
Exit

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
get_degrees_minutes_seconds:
Procedure Expose sign. (globs)
Do
  Parse arg angle .

  degrees = angle * 100 % 100
  minutes = ((angle - degrees) * 60) * 100 % 100
  seconds = ((((angle - degrees) * 60) - minutes) * 60) * 100 % 100

  Return degrees minutes seconds
End
Exit

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
format_degrees_minutes_seconds:
Procedure Expose  sign. (globs)
Do
  Parse arg angle .

  Parse Value get_degrees_minutes_seconds(angle) with degrees minutes seconds .

  formatted = degrees~right(3) || sign.!DEG || minutes~right(2, 0) || sign.!MIN || seconds~right(2, 0) || sign.!SEC

  Return formatted
End
Exit

```


;Output

```txt
  1 N    North                  354.375° (354°22′30″)     0.000° (  0°00′00″)     5.625° (  5°37′30″)
  2 NbE  North by East            5.625° (  5°37′30″)    11.250° ( 11°15′00″)    16.875° ( 16°52′30″)
  3 NNE  North-Northeast         16.875° ( 16°52′30″)    22.500° ( 22°30′00″)    28.125° ( 28°07′30″)
  4 NEbn Northeast by North      28.125° ( 28°07′30″)    33.750° ( 33°45′00″)    39.375° ( 39°22′30″)
  5 NE   Northeast               39.375° ( 39°22′30″)    45.000° ( 45°00′00″)    50.625° ( 50°37′30″)
  6 NEbE Northeast by East       50.625° ( 50°37′30″)    56.250° ( 56°15′00″)    61.875° ( 61°52′30″)
  7 ENE  East-Northeast          61.875° ( 61°52′30″)    67.500° ( 67°30′00″)    73.125° ( 73°07′30″)
  8 EbN  East by North           73.125° ( 73°07′30″)    78.750° ( 78°45′00″)    84.375° ( 84°22′30″)
  9 E    East                    84.375° ( 84°22′30″)    90.000° ( 90°00′00″)    95.625° ( 95°37′30″)
 10 EbS  East by South           95.625° ( 95°37′30″)   101.250° (101°15′00″)   106.875° (106°52′30″)
 11 ESE  East-Southeast         106.875° (106°52′30″)   112.500° (112°30′00″)   118.125° (118°07′30″)
 12 SEbE Southeast by East      118.125° (118°07′30″)   123.750° (123°45′00″)   129.375° (129°22′30″)
 13 SE   Southeast              129.375° (129°22′30″)   135.000° (135°00′00″)   140.625° (140°37′30″)
 14 SEbS Southeast by South     140.625° (140°37′30″)   146.250° (146°15′00″)   151.875° (151°52′30″)
 15 SSE  South-Southeast        151.875° (151°52′30″)   157.500° (157°30′00″)   163.125° (163°07′30″)
 16 SbE  South by East          163.125° (163°07′30″)   168.750° (168°45′00″)   174.375° (174°22′30″)
 17 S    South                  174.375° (174°22′30″)   180.000° (180°00′00″)   185.625° (185°37′30″)
 18 SbW  South by West          185.625° (185°37′30″)   191.250° (191°15′00″)   196.875° (196°52′30″)
 19 SSW  South-Southwest        196.875° (196°52′30″)   202.500° (202°30′00″)   208.125° (208°07′30″)
 20 SWbS Southwest by South     208.125° (208°07′30″)   213.750° (213°45′00″)   219.375° (219°22′30″)
 21 SW   Southwest              219.375° (219°22′30″)   225.000° (225°00′00″)   230.625° (230°37′30″)
 22 SWbW Southwest by West      230.625° (230°37′30″)   236.250° (236°15′00″)   241.875° (241°52′30″)
 23 WSW  Southwest              241.875° (241°52′30″)   247.500° (247°30′00″)   253.125° (253°07′30″)
 24 WbS  West by South          253.125° (253°07′30″)   258.750° (258°45′00″)   264.375° (264°22′30″)
 25 W    West                   264.375° (264°22′30″)   270.000° (270°00′00″)   275.625° (275°37′30″)
 26 WbN  West by North          275.625° (275°37′30″)   281.250° (281°15′00″)   286.875° (286°52′30″)
 27 WNW  West-Northwest         286.875° (286°52′30″)   292.500° (292°30′00″)   298.125° (298°07′30″)
 28 NWbW Northwest by West      298.125° (298°07′30″)   303.750° (303°45′00″)   309.375° (309°22′30″)
 29 NW   Northwest              309.375° (309°22′30″)   315.000° (315°00′00″)   320.625° (320°37′30″)
 30 NWbN Northwest by North     320.625° (320°37′30″)   326.250° (326°15′00″)   331.875° (331°52′30″)
 31 NNW  North-Northwest        331.875° (331°52′30″)   337.500° (337°30′00″)   343.125° (343°07′30″)
 32 NbW  North by West          343.125° (343°07′30″)   348.750° (348°45′00″)   354.375° (354°22′30″)


  1 N    North                    0.000° (  0°00′00″)
  2 NbE  North by East           16.870° ( 16°52′12″)
  3 NNE  North-Northeast         16.880° ( 16°52′48″)
  4 NEbn Northeast by North      33.750° ( 33°45′00″)
  5 NE   Northeast               50.620° ( 50°37′12″)
  6 NEbE Northeast by East       50.630° ( 50°37′48″)
  7 ENE  East-Northeast          67.500° ( 67°30′00″)
  8 EbN  East by North           84.370° ( 84°22′12″)
  9 E    East                    84.380° ( 84°22′48″)
 10 EbS  East by South          101.250° (101°15′00″)
 11 ESE  East-Southeast         118.120° (118°07′12″)
 12 SEbE Southeast by East      118.130° (118°07′48″)
 13 SE   Southeast              135.000° (135°00′00″)
 14 SEbS Southeast by South     151.870° (151°52′12″)
 15 SSE  South-Southeast        151.880° (151°52′48″)
 16 SbE  South by East          168.750° (168°45′00″)
 17 S    South                  185.620° (185°37′12″)
 18 SbW  South by West          185.630° (185°37′48″)
 19 SSW  South-Southwest        202.500° (202°30′00″)
 20 SWbS Southwest by South     219.370° (219°22′12″)
 21 SW   Southwest              219.380° (219°22′48″)
 22 SWbW Southwest by West      236.250° (236°15′00″)
 23 WSW  Southwest              253.120° (253°07′12″)
 24 WbS  West by South          253.130° (253°07′48″)
 25 W    West                   270.000° (270°00′00″)
 26 WbN  West by North          286.870° (286°52′12″)
 27 WNW  West-Northwest         286.880° (286°52′48″)
 28 NWbW Northwest by West      303.750° (303°45′00″)
 29 NW   Northwest              320.620° (320°37′12″)
 30 NWbN Northwest by North     320.630° (320°37′48″)
 31 NNW  North-Northwest        337.500° (337°30′00″)
 32 NbW  North by West          354.370° (354°22′12″)
  1 N    North                  354.380° (354°22′48″)

```



## PARI/GP


```parigp
box(x)={["North","North by east","North-northeast","Northeast by north","Northeast",
"Northeast by east","East-northeast","East by north","East","East by south","East-southeast",
"Southeast by east","Southeast","Southeast by south","South-southeast","South by east","South",
"South by west","South-southwest","Southwest by south","Southwest","Southwest by west","West-southwest",
"West by south","West","West by north","West-northwest","Northwest by west","Northwest",
"Northwest by north","North-northwest","North by west"][round(x*4/45)%32+1]};
for(i=0,32,print(i%32+1" "box(x=i*11.25+if(i%3==1,5.62,if(i%3==2,-5.62)))" "x))
```


Output:

```txt
1 North 0
2 North by east 16.8700000
3 North-northeast 16.8800000
4 Northeast by north 33.7500000
5 Northeast 50.6200000
6 Northeast by east 50.6300000
7 East-northeast 67.5000000
8 East by north 84.3700000
9 East 84.3800000
10 East by south 101.250000
11 East-southeast 118.120000
12 Southeast by east 118.130000
13 Southeast 135.000000
14 Southeast by south 151.870000
15 South-southeast 151.880000
16 South by east 168.750000
17 South 185.620000
18 South by west 185.630000
19 South-southwest 202.500000
20 Southwest by south 219.370000
21 Southwest 219.380000
22 Southwest by west 236.250000
23 West-southwest 253.120000
24 West by south 253.130000
25 West 270.000000
26 West by north 286.870000
27 West-northwest 286.880000
28 Northwest by west 303.750000
29 Northwest 320.620000
30 Northwest by north 320.630000
31 North-northwest 337.500000
32 North by west 354.370000
1 North 354.380000
```



## Pascal

{{trans|Fortran}}

```pascal
program BoxTheCompass(output);

function compasspoint(angle: real): string;
  const
    points: array [1..32] of string =
      ('North             ', 'North by east     ', 'North-northeast   ', 'Northeast by north',
       'Northeast         ', 'Northeast by east ', 'East-northeast    ', 'East by north     ',
       'East              ', 'East by south     ', 'East-southeast    ', 'Southeast by east ',
       'Southeast         ', 'Southeast by south', 'South-southeast   ', 'South by east     ',
       'South             ', 'South by west     ', 'South-southwest   ', 'Southwest by south',
       'Southwest         ', 'Southwest by west ', 'West-southwest    ', 'West by south     ',
       'West              ', 'West by north     ', 'West-northwest    ', 'Northwest by west ',
       'Northwest         ', 'Northwest by north', 'North-northwest   ', 'North by west     '
      );
  var
    index: integer;
  begin
    index := round (angle / 11.25);
    index := index mod 32 + 1;
    compasspoint := points[index];
  end;

var
  i:       integer;
  heading: real;

begin
  for i := 0 to 32 do
  begin
    heading := i * 11.25;
    case (i mod 3) of
      1: heading := heading + 5.62;
      2: heading := heading - 5.62;
    end;
    writeln((i mod 32) + 1:2, ' ', compasspoint(heading), ' ', heading:8:4);
  end;
end.
```

Output:

```txt

:> ./BoxTheCompass
 1 North                0.0000
 2 North by east       16.8700
 3 North-northeast     16.8800
 4 Northeast by north  33.7500
 5 Northeast           50.6200
 6 Northeast by east   50.6300
 7 East-northeast      67.5000
 8 East by north       84.3700
 9 East                84.3800
10 East by south      101.2500
11 East-southeast     118.1200
12 Southeast by east  118.1300
13 Southeast          135.0000
14 Southeast by south 151.8700
15 South-southeast    151.8800
16 South by east      168.7500
17 South              185.6200
18 South by west      185.6300
19 South-southwest    202.5000
20 Southwest by south 219.3700
21 Southwest          219.3800
22 Southwest by west  236.2500
23 West-southwest     253.1200
24 West by south      253.1300
25 West               270.0000
26 West by north      286.8700
27 West-northwest     286.8800
28 Northwest by west  303.7500
29 Northwest          320.6200
30 Northwest by north 320.6300
31 North-northwest    337.5000
32 North by west      354.3700
 1 North              354.3800

```



## Perl

Don't waste brain cells calculating names, not worth the effort.  Code is probably shorter, faster, and easier to read this way.

```Perl
use utf8;

my @names = (
	"North",
	"North by east",
	"North-northeast",
	"Northeast by north",
	"Northeast",
	"Northeast by east",
	"East-northeast",
	"East by north",
	"East",
	"East by south",
	"East-southeast",
	"Southeast by east",
	"Southeast",
	"Southeast by south",
	"South-southeast",
	"South by east",
	"South",
	"South by west",
	"South-southwest",
	"Southwest by south",
	"Southwest",
	"Southwest by west",
	"West-southwest",
	"West by south",
	"West",
	"West by north",
	"West-northwest",
	"Northwest by west",
	"Northwest",
	"Northwest by north",
	"North-northwest",
	"North by west",
);
my @angles = (0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38,
	101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62,
	185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0,
	286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38);

for (@angles) {
	my $i = int(($_ * 32 / 360) + .5) % 32;
	printf "%3d %18s %6.2f°\n", $i + 1, $names[$i], $_;
}
```
output<lang>  1              North   0.00°
  2      North by east  16.87°
  3    North-northeast  16.88°
  4 Northeast by north  33.75°
  5          Northeast  50.62°
  6  Northeast by east  50.63°
  7     East-northeast  67.50°
  8      East by north  84.37°
  9               East  84.38°
 10      East by south 101.25°
 11     East-southeast 118.12°
 12  Southeast by east 118.13°
 13          Southeast 135.00°
 14 Southeast by south 151.87°
 15    South-southeast 151.88°
 16      South by east 168.75°
 17              South 185.62°
 18      South by west 185.63°
 19    South-southwest 202.50°
 20 Southwest by south 219.37°
 21          Southwest 219.38°
 22  Southwest by west 236.25°
 23     West-southwest 253.12°
 24      West by south 253.13°
 25               West 270.00°
 26      West by north 286.87°
 27     West-northwest 286.88°
 28  Northwest by west 303.75°
 29          Northwest 320.62°
 30 Northwest by north 320.63°
 31    North-northwest 337.50°
 32      North by west 354.37°
  1              North 354.38°
```



## Perl 6

{{works with|Rakudo|2015.12}}

```perl6
sub point (Int $index) {
    my $ix = $index % 32;
    if $ix +& 1
        { "&point(($ix + 1) +& 28) by &point(((2 - ($ix +& 2)) * 4) + $ix +& 24)" }
    elsif $ix +& 2
        { "&point(($ix + 2) +& 24)-&point(($ix +| 4) +& 28)" }
    elsif $ix +& 4
        { "&point(($ix + 8) +& 16)&point(($ix +| 8) +& 24)" }
    else
        { <north east south west>[$ix div 8]; }
}

sub test-angle ($ix) { $ix * 11.25 + (0, 5.62, -5.62)[ $ix % 3 ] }
sub angle-to-point(\𝜽) { floor 𝜽 / 360 * 32 + 0.5 }

for 0 .. 32 -> $ix {
    my \𝜽 = test-angle($ix);
    printf "  %2d %6.2f° %s\n",
              $ix % 32 + 1,
                  𝜽,
                         tc point angle-to-point 𝜽;
}
```


Output:

```txt
   1   0.00° North
   2  16.87° North by east
   3  16.88° North-northeast
   4  33.75° Northeast by north
   5  50.62° Northeast
   6  50.63° Northeast by east
   7  67.50° East-northeast
   8  84.37° East by north
   9  84.38° East
  10 101.25° East by south
  11 118.12° East-southeast
  12 118.13° Southeast by east
  13 135.00° Southeast
  14 151.87° Southeast by south
  15 151.88° South-southeast
  16 168.75° South by east
  17 185.62° South
  18 185.63° South by west
  19 202.50° South-southwest
  20 219.37° Southwest by south
  21 219.38° Southwest
  22 236.25° Southwest by west
  23 253.12° West-southwest
  24 253.13° West by south
  25 270.00° West
  26 286.87° West by north
  27 286.88° West-northwest
  28 303.75° Northwest by west
  29 320.62° Northwest
  30 320.63° Northwest by north
  31 337.50° North-northwest
  32 354.37° North by west
   1 354.38° North
```



## Phix

Unlike the Perl guy, for me the maths is the boring bit, building those strings is the fun!


```Phix
function get225(integer d, string p1, string p2, string p4)
string p3
    p3 = p1&'-'&lower(p2)
    p2 &= " by "&lower(p1)
    p1 &= " by "&lower(p4)
    if d then
        return {p1,p3,p2}       -- eg {North by east,North-northeast,Northeast by north}
    else
        return {p2,p3,p1}       -- eg {Northeast by east,East-northeast,East by north}
    end if
end function

function get45(sequence res, integer d, string p1, string p2)
string p3
    res = append(res,p1)        -- North/East/South/West
    if d then
        p3 = p1&lower(p2)       -- Northeast/Southwest
    else
        p3 = p2&lower(p1)       -- Southeast/Northwest
    end if
    res &= get225(1,p1,p3,p2)   -- eg get225(1,North,Northeast,East)
                                -- -> {North by east,North-northeast,Northeast by north}
    res = append(res,p3)        -- Northeast/Southeast/Southwest/Northwest
    res &= get225(0,p2,p3,p1)   -- eg get225(0,East,Northeast,North)
                                -- -> {Northeast by east,East-northeast,East by north}
    return res
end function

function get90(sequence points)
sequence res = {}
    for i=1 to length(points) do
        res = get45(res,remainder(i,2),points[i],points[remainder(i,4)+1])
    end for -- ie get45(1,North,East)
            --    get45(0,East,South)
            --    get45(1,South,West)
            --    get45(0,West,North)
    return res
end function

constant compass_points = get90({"North","East","South","West"})

atom test_point
integer compass_point
for i = 1 to 33 do
    test_point = (i-1)*11.25 + 5.62*(remainder(i,3)-1)
    compass_point = remainder(floor(test_point*32/360+0.5),32)+1
    printf(1, "%2d  %-22s  %6.2f\n", {compass_point, compass_points[compass_point], test_point})
end for
```

If you like, you can regard d=1 (both of them) as "clockwise-name-inherit" and d=0 as "anti-clockwise-name-inherit".

{{out}}

```txt

 1  North                     0.00
 2  North by east            16.87
 3  North-northeast          16.88
 4  Northeast by north       33.75
 5  Northeast                50.62
 6  Northeast by east        50.63
 7  East-northeast           67.50
 8  East by north            84.37
 9  East                     84.38
10  East by south           101.25
11  East-southeast          118.12
12  Southeast by east       118.13
13  Southeast               135.00
14  Southeast by south      151.87
15  South-southeast         151.88
16  South by east           168.75
17  South                   185.62
18  South by west           185.63
19  South-southwest         202.50
20  Southwest by south      219.37
21  Southwest               219.38
22  Southwest by west       236.25
23  West-southwest          253.12
24  West by south           253.13
25  West                    270.00
26  West by north           286.87
27  West-northwest          286.88
28  Northwest by west       303.75
29  Northwest               320.62
30  Northwest by north      320.63
31  North-northwest         337.50
32  North by west           354.37
 1  North                   354.38

```

of course the following (more sensible but less fun) way works just as well, and produces the same output

```Phix
constant compass_points = { "North",        "North by east",        "North-northeast",      "Northeast by north",
                            "Northeast",    "Northeast by east",    "East-northeast",       "East by north",
                            "East",         "East by south",        "East-southeast",       "Southeast by east",
                            "Southeast",    "Southeast by south",   "South-southeast",      "South by east",
                            "South",        "South by west",        "South-southwest",      "Southwest by south",
                            "Southwest",    "Southwest by west",    "West-southwest",       "West by south",
                            "West",         "West by north",        "West-northwest",       "Northwest by west",
                            "Northwest",    "Northwest by north",   "North-northwest",      "North by west"},
        test_points = {    0.0,  16.87,  16.88,  33.75,  50.62,  50.63,  67.5,   84.37,  84.38, 101.25, 118.12,
                        118.13, 135.0,  151.87, 151.88, 168.75, 185.62, 185.63, 202.5,  219.37, 219.38, 236.25,
                        253.12, 253.13, 270.0,  286.87, 286.88, 303.75, 320.62, 320.63, 337.5,  354.37, 354.38}
for i=1 to length(test_points) do
    integer compass_point = remainder(floor(test_points[i]*32/360+0.5),32)+1
    printf(1, "%2d  %-22s  %6.2f\n", {compass_point, compass_points[compass_point], test_points[i]})
end for
```



## PicoLisp


```PicoLisp
(scl 3)

(setq *Compass                      # Build lookup table
   (let H -16.875
      (mapcar
         '((Str)
            (cons
               (inc 'H 11.25)       # Heading in degrees
               (pack                # Compass point
                  (replace (chop Str)
                     "N" "north"
                     "E" "east"
                     "S" "south"
                     "W" "west"
                     "b" " by " ) ) ) )
         '("N" "NbE" "N-NE" "NEbN" "NE" "NEbE" "E-NE" "EbN"
            "E" "EbS" "E-SE" "SEbE" "SE" "SEbS" "S-SE" "SbE"
            "S" "SbW" "S-SW" "SWbS" "SW" "SWbW" "W-SW" "WbS"
            "W" "WbN" "W-NW" "NWbW" "NW" "NWbN" "N-NW" "NbW"
            "N" ) ) ) )

(de heading (Deg)
   (rank (% Deg 360.00) *Compass) )

(for I (range 0 32)
   (let H (* I 11.25)
      (case (% I 3)
         (1 (inc 'H 5.62))
         (2 (dec 'H 5.62)) )
      (tab (3 1 -18 8)
         (inc (% I 32))
         NIL
         (cdr (heading H))
         (round H 2) ) ) )
```

Output:

```txt
  1 north                 0.00
  2 north by east        16.87
  3 north-northeast      16.88
  4 northeast by north   33.75
  5 northeast            50.62
  6 northeast by east    50.63
  7 east-northeast       67.50
  8 east by north        84.37
  9 east                 84.38
 10 east by south       101.25
 11 east-southeast      118.12
 12 southeast by east   118.13
 13 southeast           135.00
 14 southeast by south  151.87
 15 south-southeast     151.88
 16 south by east       168.75
 17 south               185.62
 18 south by west       185.63
 19 south-southwest     202.50
 20 southwest by south  219.37
 21 southwest           219.38
 22 southwest by west   236.25
 23 west-southwest      253.12
 24 west by south       253.13
 25 west                270.00
 26 west by north       286.87
 27 west-northwest      286.88
 28 northwest by west   303.75
 29 northwest           320.62
 30 northwest by north  320.63
 31 north-northwest     337.50
 32 north by west       354.37
  1 north               354.38
```



## PowerShell


```powershell
function Convert-DegreeToDirection ( [double]$Degree )
    {

    $Directions = @(    'n','n by e','n-ne','ne by n','ne','ne by e','e-ne','e by n',
                        'e','e by s','e-se','se by e','se','se by s','s-se','s by e',
                        's','s by w','s-sw','sw by s','sw','sw by w','w-sw','w by s',
                        'w','w by n','w-nw','nw by w','nw','nw by n','n-nw','n by w',
                        'n'
                    ).Replace( 's', 'south' ).Replace( 'e', 'east' ).Replace( 'n', 'north' ).Replace( 'w', 'west' )

    $Directions[[math]::floor(( $Degree % 360 ) / 11.25 + 0.5 )]
    }

$x = 0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38

$x | % { Convert-DegreeToDirection -Degree $_ }
```

{{out}}

```txt
north
north by east
north-northeast
northeast by north
northeast
northeast by east
east-northeast
east by north
east
east by south
east-southeast
southeast by east
southeast
southeast by south
south-southeast
south by east
south
south by west
south-southwest
southwest by south
southwest
southwest by west
west-southwest
west by south
west
west by north
west-northwest
northwest by west
northwest
northwest by north
north-northwest
north by west
north
```

A more general solution allowing you to choose whether to use 4, 8, 16, or 32 compass points.

```powershell
function Convert-DegreeToDirection ( [double]$Degree, [int]$Points )
    {

    $Directions = @(    'n','n by e','n-ne','ne by n','ne','ne by e','e-ne','e by n',
                        'e','e by s','e-se','se by e','se','se by s','s-se','s by e',
                        's','s by w','s-sw','sw by s','sw','sw by w','w-sw','w by s',
                        'w','w by n','w-nw','nw by w','nw','nw by n','n-nw','n by w',
                        'n'
                    ).Replace( 's', 'south' ).Replace( 'e', 'east' ).Replace( 'n', 'north' ).Replace( 'w', 'west' )

    $Directions[[math]::floor((( $Degree % 360 ) * $Points / 360 + 0.5 )) * 32 / $Points ]
    }

$x = 0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38


$Values = @()
ForEach ( $Degree in $X ) { $Values += [pscustomobject]@{  Degree = $Degree
                                                               32 = ( Convert-DegreeToDirection -Degree $Degree -Points 32 )
                                                               16 = ( Convert-DegreeToDirection -Degree $Degree -Points 16 )
                                                                8 = ( Convert-DegreeToDirection -Degree $Degree -Points  8 )
                                                                4 = ( Convert-DegreeToDirection -Degree $Degree -Points  4 ) } }
$Values | Format-Table
```

{{out}}

```txt
Degree 32                 16              8         4
------ --                 --              -         -
     0 north              north           north     north
 16.87 north by east      north-northeast north     north
 16.88 north-northeast    north-northeast north     north
 33.75 northeast by north northeast       northeast north
 50.62 northeast          northeast       northeast east
 50.63 northeast by east  northeast       northeast east
  67.5 east-northeast     east-northeast  east      east
 84.37 east by north      east            east      east
 84.38 east               east            east      east
101.25 east by south      east-southeast  east      east
118.12 east-southeast     east-southeast  southeast east
118.13 southeast by east  east-southeast  southeast east
   135 southeast          southeast       southeast south
151.87 southeast by south south-southeast southeast south
151.88 south-southeast    south-southeast southeast south
168.75 south by east      south           south     south
185.62 south              south           south     south
185.63 south by west      south           south     south
 202.5 south-southwest    south-southwest southwest south
219.37 southwest by south southwest       southwest south
219.38 southwest          southwest       southwest south
236.25 southwest by west  west-southwest  southwest west
253.12 west-southwest     west-southwest  west      west
253.13 west by south      west-southwest  west      west
   270 west               west            west      west
286.87 west by north      west-northwest  west      west
286.88 west-northwest     west-northwest  west      west
303.75 northwest by west  northwest       northwest west
320.62 northwest          northwest       northwest north
320.63 northwest by north northwest       northwest north
 337.5 north-northwest    north-northwest north     north
354.37 north by west      north           north     north
354.38 north              north           north     north
```



## PureBasic


```PureBasic
DataSection
  Data.s "N", "north", "E", "east", "W", "west", "S", "south", "b", " by "   ;abbreviations, expansions
  Data.s "N NbE N-NE NEbN NE NEbE E-NE EbN E EbS E-SE SEbE SE SEbS S-SE SbE" ;dirs
  Data.s "S SbW S-SW SWbS SW SWbW W-SW WbS W WbN W-NW NWbW NW NWbN N-NW NbW"
EndDataSection

;initialize data
NewMap dirSubst.s()
Define i, abbr.s, expansion.s
For i = 1 To 5
  Read.s abbr
  Read.s expansion
  dirSubst(abbr) = expansion
Next

Dim dirs.s(32)
Define j, s.s
For j = 0 To 1
  Read.s s.s
  For i = 0 To 15
    abbr.s = StringField(s, i + 1, " ")
    dirs(j * 16 + i) = abbr
  Next
Next

;expand abbreviated compass point and capitalize
Procedure.s abbr2compassPoint(abbr.s)
  Shared dirSubst()
  Protected i, compassPoint.s, key.s

  For i = 1 To Len(abbr)
    key.s = Mid(abbr, i, 1)
    If FindMapElement(dirSubst(), key)
      compassPoint + dirSubst(key)
    Else
      compassPoint + key
    EndIf
  Next
  ProcedureReturn UCase(Left(compassPoint, 1)) + Mid(compassPoint, 2)
EndProcedure

Procedure.s angle2compass(angle.f)
  Shared dirs()
  Static segment.f = 360.0 / 32 ;width of each compass segment
  Protected dir

  ;work out which segment contains the compass angle
  dir = Int((Mod(angle, 360) / segment) + 0.5)

  ;convert to a named direction
  ProcedureReturn abbr2compassPoint(dirs(dir))
EndProcedure

;box the compass
If OpenConsole()

  Define i, heading.f, index
  For i = 0 To 32
    heading = i * 11.25
    If i % 3 = 1
      heading + 5.62
    EndIf
    If i % 3 = 2
      heading - 5.62
    EndIf
    index = i % 32 + 1

    PrintN(RSet(Str(index), 2) + " " + LSet(angle2compass(heading), 18) + RSet(StrF(heading, 2), 7))
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
 1 North                0.00
 2 North by east       16.87
 3 North-northeast     16.88
 4 Northeast by north  33.75
 5 Northeast           50.62
 6 Northeast by east   50.63
 7 East-northeast      67.50
 8 East by north       84.37
 9 East                84.38
10 East by south      101.25
11 East-southeast     118.12
12 Southeast by east  118.13
13 Southeast          135.00
14 Southeast by south 151.87
15 South-southeast    151.88
16 South by east      168.75
17 South              185.62
18 South by west      185.63
19 South-southwest    202.50
20 Southwest by south 219.37
21 Southwest          219.38
22 Southwest by west  236.25
23 West-southwest     253.12
24 West by south      253.13
25 West               270.00
26 West by north      286.87
27 West-northwest     286.88
28 Northwest by west  303.75
29 Northwest          320.62
30 Northwest by north 320.63
31 North-northwest    337.50
32 North by west      354.37
 1                    354.38
```



## Prolog

Part 1 : The following knowledge base takes a heading in degrees and returns the correct 32-point compass heading. It can also go in the other direction.

```prolog

compassangle(1, 'North',n, 0.00).
compassangle(2, 'North by east', nbe, 11.25).
compassangle(3, 'North-northeast', nne,22.50).
compassangle(4, 'Northeast by north', nebn,33.75).
compassangle(5, 'Northeast', ne,45.00).
compassangle(6, 'Norteast by east', nebe,56.25).
compassangle(7, 'East-northeast', ene,67.50).
compassangle(8, 'East by North', ebn,78.75).
compassangle(9, 'East', e,90.00).
compassangle(10, 'East by south', ebs, 101.25).
compassangle(11, 'East-southeast', ese,112.50).
compassangle(12, 'Southeast by east', sebe, 123.75).
compassangle(13, 'Southeast', se, 135.00).
compassangle(14, 'Southeast by south', sebs, 146.25).
compassangle(15, 'South-southeast',sse, 157.50).
compassangle(16, 'South by east', sbe, 168.75).
compassangle(17, 'South', s, 180.00).
compassangle(18, 'South by west', sbw, 191.25).
compassangle(19, 'South-southwest', ssw, 202.50).
compassangle(20, 'Southwest by south', swbs, 213.75).
compassangle(21, 'Southwest', sw, 225.00).
compassangle(22, 'Southwest by west', swbw, 236.25).
compassangle(23, 'West-southwest', wsw, 247.50).
compassangle(24, 'West by south', wbs, 258.75).
compassangle(25, 'West', w, 270.00).
compassangle(26, 'West by north', wbn, 281.25).
compassangle(27, 'West-northwest', wnw, 292.50).
compassangle(28, 'Northwest by west', nwbw, 303.75).
compassangle(29, 'Northwest', nw, 315.00).
compassangle(30, 'Northwest by north', nwbn, 326.25).
compassangle(31, 'North-northwest', nnw, 337.50).
compassangle(32, 'North by west', nbw, 348.75).
compassangle(1, 'North', n, 360.00).
compassangle(Index , Name, Heading, Angle) :- nonvar(Angle), resolveindex(Angle, Index),
                                               compassangle(Index,Name, Heading, _).

resolveindex(Angle, Index) :- N is Angle / 11.25 + 0.5, I is floor(N),Index is (I mod 32) + 1.

```

Part 2 : The following rules print a table of indexes.

```prolog

printTableRow(Angle) :- compassangle(Index, Name, _, Angle),
                        write(Index), write('    '),
                        write(Name), write('   '),
                        write(Angle).

printTable([X|Xs]) :- printTableRow(X), nl, printTable(Xs),!.
printTable([]).

```

The following query prints the required table.

```prolog

?- printTable([0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62,
                       185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38]).
1    North   0.0
2    North by east   16.87
3    North-northeast   16.88
4    Northeast by north   33.75
5    Northeast   50.62
6    Norteast by east   50.63
7    East-northeast   67.5
8    East by North   84.37
9    East   84.38
10    East by south   101.25
11    East-southeast   118.12
12    Southeast by east   118.13
13    Southeast   135.0
14    Southeast by south   151.87
15    South-southeast   151.88
16    South by east   168.75
17    South   185.62
18    South by west   185.63
19    South-southwest   202.5
20    Southwest by south   219.37
21    Southwest   219.38
22    Southwest by west   236.25
23    West-southwest   253.12
24    West by south   253.13
25    West   270.0
26    West by north   286.87
27    West-northwest   286.88
28    Northwest by west   303.75
29    Northwest   320.62
30    Northwest by north   320.63
31    North-northwest   337.5
32    North by west   354.37
1    North   354.38
true.

```



## Python


```python
majors   = 'north east south west'.split()
majors   *= 2 # no need for modulo later
quarter1 = 'N,N by E,N-NE,NE by N,NE,NE by E,E-NE,E by N'.split(',')
quarter2 = [p.replace('NE','EN') for p in quarter1]

def degrees2compasspoint(d):
    d = (d % 360) + 360/64
    majorindex, minor = divmod(d, 90.)
    majorindex = int(majorindex)
    minorindex  = int( (minor*4) // 45 )
    p1, p2 = majors[majorindex: majorindex+2]
    if p1 in {'north', 'south'}:
        q = quarter1
    else:
        q = quarter2
    return q[minorindex].replace('N', p1).replace('E', p2).capitalize()

if __name__ == '__main__':
    for i in range(33):
        d = i * 11.25
        m = i % 3
        if   m == 1: d += 5.62
        elif m == 2: d -= 5.62
        n = i % 32 + 1
        print( '%2i %-18s %7.2f°' % (n, degrees2compasspoint(d), d) )
```


;Output

```txt
 1 North                 0.00°
 2 North by east        16.87°
 3 North-northeast      16.88°
 4 Northeast by north   33.75°
 5 Northeast            50.62°
 6 Northeast by east    50.63°
 7 East-northeast       67.50°
 8 East by north        84.37°
 9 East                 84.38°
10 East by south       101.25°
11 East-southeast      118.12°
12 Southeast by east   118.13°
13 Southeast           135.00°
14 Southeast by south  151.87°
15 South-southeast     151.88°
16 South by east       168.75°
17 South               185.62°
18 South by west       185.63°
19 South-southwest     202.50°
20 Southwest by south  219.37°
21 Southwest           219.38°
22 Southwest by west   236.25°
23 West-southwest      253.12°
24 West by south       253.13°
25 West                270.00°
26 West by north       286.87°
27 West-northwest      286.88°
28 Northwest by west   303.75°
29 Northwest           320.62°
30 Northwest by north  320.63°
31 North-northwest     337.50°
32 North by west       354.37°
 1 North               354.38°
```



## Racket


```racket
#lang racket

;;; Generate the headings and boxes
(define (i->heading/box i)
  (values (let ((heading (* i #e11.25)))
            (case (modulo i 3)
             ((1) (+ heading #e5.62))
             ((2) (- heading #e5.62))
             (else heading)))
   (add1 (modulo i 32))))
(define-values (headings-list box-list)
  (for/lists (h-lst i-lst) ((i (in-range 0 (add1 32))))
             (i->heading/box i)))

(define box-names
  (list "North" "North by east" "North-northeast"
        "Northeast by north" "Northeast" "Northeast by east"
        "East-northeast" "East by north" "East" "East by south" "East-southeast"
        "Southeast by east" "Southeast" "Southeast by south" "South-southeast"
        "South by east" "South" "South by west" "South-southwest"
        "Southwest by south" "Southwest" "Southwest by west"
        "West-southwest" "West by south" "West" "West by north" "West-northwest"
        "Northwest by west" "Northwest" "Northwest by north" "North-northwest"
        "North by west"))

(define (heading->box h)
 (let* ((n-boxes (length box-names))
        (box-width (/ 360 n-boxes)))
  (add1 (modulo (ceiling (- (/ h box-width) 1/2)) n-boxes))))

;;; displays a single row of the table, can also be used for titles
(define (display-row b a p)
  (printf "~a | ~a | ~a~%"
          (~a b #:width 2 #:align 'right)
          (~a a #:width 6 #:align 'right)
          (~a p)))

;;; display the table
(display-row "#" "Angle" "Point")
(displayln "---+--------+-------------------")
(for ((heading headings-list))
     (let* ((box-number (heading->box heading)))
       ;; by coincidence, default value of the second argument to
       ;; real->decimal-string "decimal-digits" is 2,... just what we want!
       (display-row box-number
                    (real->decimal-string heading)
                    (list-ref box-names (sub1 box-number)))))

(module+ test
 (require rackunit)
 ;;; unit test heading->box (the business end of the implementation)
 (check-= (heading->box 354.38) 1 0)
 (check-= (heading->box 5.62) 1 0)
 (check-= (heading->box 5.63) 2 0)
 (check-= (heading->box 16.87) 2 0))
```


Output:

```txt
 # |  Angle | Point
---+--------+-------------------
 1 |   0.00 | North
 2 |  16.87 | North by east
 3 |  16.88 | North-northeast
 4 |  33.75 | Northeast by north
 5 |  50.62 | Northeast
 6 |  50.63 | Northeast by east
 7 |  67.50 | East-northeast
 8 |  84.37 | East by north
 9 |  84.38 | East
10 | 101.25 | East by south
11 | 118.12 | East-southeast
12 | 118.13 | Southeast by east
13 | 135.00 | Southeast
14 | 151.87 | Southeast by south
15 | 151.88 | South-southeast
16 | 168.75 | South by east
17 | 185.62 | South
18 | 185.63 | South by west
19 | 202.50 | South-southwest
20 | 219.37 | Southwest by south
21 | 219.38 | Southwest
22 | 236.25 | Southwest by west
23 | 253.12 | West-southwest
24 | 253.13 | West by south
25 | 270.00 | West
26 | 286.87 | West by north
27 | 286.88 | West-northwest
28 | 303.75 | Northwest by west
29 | 320.62 | Northwest
30 | 320.63 | Northwest by north
31 | 337.50 | North-northwest
32 | 354.37 | North by west
 1 | 354.38 | North
```



## REXX

This version does normalization of the (degree) heading and can also handle negative headings.

```rexx
/*REXX program  "boxes the compass"   [from degree (º) headings  ───►  a 32 point set]. */
parse arg $                                      /*allow   º   headings to be specified.*/
if $=''  then $= 0 16.87 16.88 33.75 50.62 50.63 67.5 84.37 84.38 101.25 118.12 118.13   ,
                 135 151.87 151.88 168.75 185.62 185.63 202.5 219.37 219.38 236.25       ,
                 253.12 253.13 270 286.87 286.88 303.75 320.62 320.63 337.5 354.37 354.38
                                                 /* [↑]  use default, they're in degrees*/
@pts= 'n nbe n-ne nebn ne nebe e-ne ebn e ebs e-se sebe se sebs s-se sbe',
      "s sbw s-sw swbs sw swbw w-sw wbs w wbn w-nw nwbw nw nwbn n-nw nbw"

#= words(@pts) + 1                               /*#:  used for integer ÷ remainder (//)*/
dirs= 'north south east west'                    /*define cardinal compass directions.  */
                                                 /* [↓]  choose a glyph for degree  (°).*/
if 4=='f4'x  then degSym= "a1"x                  /*is this system an  EBCDIC  system?   */
             else degSym= "a7"x                  /*'f8'x  is the degree symbol: °  vs º */
                                                 /*──────────────────────────── f8 vs a7*/
say right(degSym'heading', 30)     center("compass heading", 20)
say right(     '════════', 30)     copies(              "═", 20)
pad= ' '                                         /*used to interject a blank for output.*/
         do j=1  for words($);   x= word($, j)   /*obtain one of the degree headings.   */
         say right( format(x, , 2)degSym, 30-1)          pad         boxHeading(x)
         end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
boxHeading: y= arg(1)//360; if y<0  then y=360-y /*normalize heading within unit circle.*/
            z= word(@pts, trunc(max(1, (y/11.25 +1.5) // #))) /*convert degrees─►heading*/
                                               do k=1  for words(dirs);   d= word(dirs, k)
                                               z=changestr( left(d, 1),  z,  d)
                                               end   /*k*/    /* [↑]  old, haystack, new*/
            return changestr('b',  z,  " by ")                /*expand  "b" ───► " by ".*/
```

Some older REXXes don't have a   '''changestr'''   BIF,   so one is included here   ──►   [[CHANGESTR.REX]].



'''output'''

```txt

                      ºheading   compass heading
                      ════════ ════════════════════
                        0.00º   north
                       16.87º   north by east
                       16.88º   north-northeast
                       33.75º   northeast by north
                       50.62º   northeast
                       50.63º   northeast by east
                       67.50º   east-northeast
                       84.37º   east by north
                       84.38º   east
                      101.25º   east by south
                      118.12º   east-southeast
                      118.13º   southeast by east
                      135.00º   southeast
                      151.87º   southeast by south
                      151.88º   south-southeast
                      168.75º   south by east
                      185.62º   south
                      185.63º   south by west
                      202.50º   south-southwest
                      219.37º   southwest by south
                      219.38º   southwest
                      236.25º   southwest by west
                      253.12º   west-southwest
                      253.13º   west by south
                      270.00º   west
                      286.87º   west by north
                      286.88º   west-northwest
                      303.75º   northwest by west
                      320.62º   northwest
                      320.63º   northwest by north
                      337.50º   north-northwest
                      354.37º   north by west
                      354.38º   north

```



## Ring


```ring

# Project : Box the compass

names = ["North", "North by east", "North-northeast",
               "Northeast by north", "Northeast", "Northeast by east", "East-northeast",
               "East by north", "East", "East by south", "East-southeast",
               "Southeast by east", "Southeast", "Southeast by south", "South-southeast",
               "South by east", "South", "South by west", "South-southwest",
               "Southwest by south", "Southwest", "Southwest by west", "West-southwest",
               "West by south", "West", "West by north", "West-northwest",
               "Northwest by west", "Northwest", "Northwest by north", "North-northwest",
               "North by west", "North"]

degrees = [0, 16.87, 16.88, 33.75, 50.62, 50.63,
                67.5, 84.37, 84.38, 101.25, 118.12, 118.13, 135, 151.87, 151.88, 168.75,
                185.62, 185.63, 202.5, 219.37, 219.38, 236.25, 253.12, 253.13, 270,
                286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38]

for i = 1 to len(degrees)
     j = floor((degrees[i] + 5.625) / 11.25)
    if j > 31
       j = j - 32
    ok
    see "" + degrees[i] + " " + j + " "
    if j != 0
       see "" + names[j+1] + nl
    else
       see "" + names[len(names)] + nl
    ok
next

```

Output:

```txt

0 0 North
16.87 1 North by east
16.88 2 North-northeast
33.75 3 Northeast by north
50.62 4 Northeast
50.63 5 Northeast by east
67.50 6 East-northeast
84.37 7 East by north
84.38 8 East
101.25 9 East by south
118.12 10 East-southeast
118.13 11 Southeast by east
135 12 Southeast
151.87 13 Southeast by south
151.88 14 South-southeast
168.75 15 South by east
185.62 16 South
185.63 17 South by west
202.50 18 South-southwest
219.37 19 Southwest by south
219.38 20 Southwest
236.25 21 Southwest by west
253.12 22 West-southwest
253.13 23 West by south
270 24 West
286.87 25 West by north
286.88 26 West-northwest
303.75 27 Northwest by west
320.62 28 Northwest
320.63 29 Northwest by north
337.50 30 North-northwest
354.37 31 North by west
354.38 0 North

```



## Ruby



```ruby
Headings = %w(north east south west north).each_cons(2).flat_map do |a, b|
  [a,
  "#{a} by #{b}",
  "#{a}-#{a}#{b}",
  "#{a}#{b} by #{a}",
  "#{a}#{b}",
  "#{a}#{b} by #{b}",
  "#{b}-#{a}#{b}",
  "#{b} by #{a}"]
end
Headings.prepend nil

def heading(degrees)
  i = degrees.quo(360).*(32).round.%(32).+(1)
  [i, Headings[i]]
end

# an array of angles, in degrees
angles = (0..32).map { |i| i * 11.25 + [0, 5.62, -5.62][i % 3] }

angles.each do |degrees|
  index, name = heading degrees
  printf "%2d %20s %6.2f\n", index, name.center(20), degrees
end
```


{{output}}

```txt
 1        north           0.00
 2    north by east      16.87
 3   north-northeast     16.88
 4  northeast by north   33.75
 5      northeast        50.62
 6  northeast by east    50.63
 7    east-northeast     67.50
 8    east by north      84.37
 9         east          84.38
10    east by south     101.25
11    east-southeast    118.12
12  southeast by east   118.13
13      southeast       135.00
14  southeast by south  151.87
15   south-southeast    151.88
16    south by east     168.75
17        south         185.62
18    south by west     185.63
19   south-southwest    202.50
20  southwest by south  219.37
21      southwest       219.38
22  southwest by west   236.25
23    west-southwest    253.12
24    west by south     253.13
25         west         270.00
26    west by north     286.87
27    west-northwest    286.88
28  northwest by west   303.75
29      northwest       320.62
30  northwest by north  320.63
31   north-northwest    337.50
32    north by west     354.37
 1        north         354.38
```



## Run BASIC


```runbasic
global direct$
dim direct$(22)
direct$(1)  = "y"      'by
direct$(4)  = "ast"    'East
direct$(13) = "orth"   'North
direct$(18) = "outh"   'Soutn
direct$(22) = "est"    'West

dim point$(32)
for i =1 to 32: read point$(i) :next i

html "<table border=1>"
for i = 0 to 32
    hdng = i * 11.25
    if (i mod 3) = 1 then
        hdng = hdng +5.62
    else
        if (i mod 3) = 2 then hdng = hdng -5.62
    end if
    pointer = i mod 32 + 1
    html "<TR><TD align=right>";pointer;"</td><td>";compas$(hdng);"</td><td>";hdng;"</td></tr>"
next i
html "</table>"
end

function compas$(hd)
x = hd /11.25 + 1.5
if (x >=33.0) then x =x -32.0
  p$ = point$(int(x))
  for i = 1 to len(p$)
    compas$ = compas$ + mid$(p$,i,1) + direct$(max(asc(upper$(mid$(p$,i,1)))-65,0))
  next i
end function

data "N","N b e","N-ne","Ne b n","Ne","Ne b e","E-ne","E b n","E","E b s","E-se"
data "Se b e","Se","Se b s","S-se","S b e","S","S b w","S-sw","Sw b s","Sw","Sw b w"
data "W-sw","W b s","W","W b n","W-nw","Nw b w","Nw","Nw b n","N-nw","N b w"
```

<code><table border=1>
<TR><TD align=right>1</td><td>North</td><td>0.0</td></tr>
<TR><TD align=right>2</td><td>North by east</td><td>16.87</td></tr>
<TR><TD align=right>3</td><td>North-northeast</td><td>16.88</td></tr>
<TR><TD align=right>4</td><td>Northeast by north</td><td>33.75</td></tr>
<TR><TD align=right>5</td><td>Northeast</td><td>50.62</td></tr>
<TR><TD align=right>6</td><td>Northeast by east</td><td>50.63</td></tr>
<TR><TD align=right>7</td><td>East-northeast</td><td>67.5</td></tr>
<TR><TD align=right>8</td><td>East by north</td><td>84.37</td></tr>
<TR><TD align=right>9</td><td>East</td><td>84.38</td></tr>
<TR><TD align=right>10</td><td>East by south</td><td>101.25</td></tr>
<TR><TD align=right>11</td><td>East-southeast</td><td>118.12</td></tr>
<TR><TD align=right>12</td><td>Southeast by east</td><td>118.13</td></tr>
<TR><TD align=right>13</td><td>Southeast</td><td>135.0</td></tr>
<TR><TD align=right>14</td><td>Southeast by south</td><td>151.87</td></tr>
<TR><TD align=right>15</td><td>South-southeast</td><td>151.88</td></tr>
<TR><TD align=right>16</td><td>South by east</td><td>168.75</td></tr>
<TR><TD align=right>17</td><td>South</td><td>185.62</td></tr>
<TR><TD align=right>18</td><td>South by west</td><td>185.63</td></tr>
<TR><TD align=right>19</td><td>South-southwest</td><td>202.5</td></tr>
<TR><TD align=right>20</td><td>Southwest by south</td><td>219.37</td></tr>
<TR><TD align=right>21</td><td>Southwest</td><td>219.38</td></tr>
<TR><TD align=right>22</td><td>Southwest by west</td><td>236.25</td></tr>
<TR><TD align=right>23</td><td>West-southwest</td><td>253.12</td></tr>
<TR><TD align=right>24</td><td>West by south</td><td>253.13</td></tr>
<TR><TD align=right>25</td><td>West</td><td>270.0</td></tr>
<TR><TD align=right>26</td><td>West by north</td><td>286.87</td></tr>
<TR><TD align=right>27</td><td>West-northwest</td><td>286.88</td></tr>
<TR><TD align=right>28</td><td>Northwest by west</td><td>303.75</td></tr>
<TR><TD align=right>29</td><td>Northwest</td><td>320.62</td></tr>
<TR><TD align=right>30</td><td>Northwest by north</td><td>320.63</td></tr>
<TR><TD align=right>31</td><td>North-northwest</td><td>337.5</td></tr>
<TR><TD align=right>32</td><td>North by west</td><td>354.37</td></tr>
<TR><TD align=right>1</td><td>North</td><td>354.38</td></tr>
</table>
</code>


## Rust

{{trans|Kotlin}}

```Rust
fn expand(cp: &str) -> String {
    let mut out = String::new();
    for c in cp.chars() {
        out.push_str(match c {
            'N' => "north",
            'E' => "east",
            'S' => "south",
            'W' => "west",
            'b' => " by ",
            _ => "-",
        });
    }
    out
}

fn main() {
    let cp = [
        "N", "NbE", "N-NE", "NEbN", "NE", "NEbE", "E-NE", "EbN",
        "E", "EbS", "E-SE", "SEbE", "SE", "SEbS", "S-SE", "SbE",
        "S", "SbW", "S-SW", "SWbS", "SW", "SWbW", "W-SW", "WbS",
        "W", "WbN", "W-NW", "NWbW", "NW", "NWbN", "N-NW", "NbW"
    ];
    println!("Index  Degrees  Compass point");
    println!("-----  -------  -------------");
    for i in 0..=32 {
        let index = i % 32;
        let heading = i as f32 * 11.25
            + match i % 3 {
                1 => 5.62,
                2 => -5.62,
                _ => 0.0,
            };
        println!(
            "{:2}     {:6.2}   {}",
            index + 1,
            heading,
            expand(cp[index])
        );
    }
}
```

{{out}}

```txt
Index  Degrees  Compass point
-----  -------  -------------
 1       0.00   north
 2      16.87   north by east
 3      16.88   north-northeast
 4      33.75   northeast by north
 5      50.62   northeast
 6      50.63   northeast by east
 7      67.50   east-northeast
 8      84.37   east by north
 9      84.38   east
10     101.25   east by south
11     118.12   east-southeast
12     118.13   southeast by east
13     135.00   southeast
14     151.87   southeast by south
15     151.88   south-southeast
16     168.75   south by east
17     185.62   south
18     185.63   south by west
19     202.50   south-southwest
20     219.37   southwest by south
21     219.38   southwest
22     236.25   southwest by west
23     253.12   west-southwest
24     253.13   west by south
25     270.00   west
26     286.87   west by north
27     286.88   west-northwest
28     303.75   northwest by west
29     320.62   northwest
30     320.63   northwest by north
31     337.50   north-northwest
32     354.37   north by west
 1     354.38   north
```



## Scala

Inspired by Java version

```Scala
object BoxingTheCompass extends App {
  val cardinal = List("north", "east", "south", "west")
  val pointDesc = List("1", "1 by 2", "1-C", "C by 1", "C", "C by 2", "2-C", "2 by 1")

  val pointDeg: Int => Double = i => {
	val fswitch: Int => Int = i => i match {case 1 => 1; case 2 => -1; case _ => 0}
	i*11.25+fswitch(i%3)*5.62
  }

  val deg2ind: Double => Int = deg => (deg*32/360+.5).toInt%32+1

  val pointName: Int => String = ind => {
    val i = ind - 1
    val str1 = cardinal(i%32/8)
    val str2 = cardinal((i%32/8+1)%4)
    val strC = if ((str1 == "north") || (str1 == "south")) str1+str2 else str2+str1
    pointDesc(i%32%8).replace("1", str1).replace("2", str2).replace("C", strC).capitalize
  }

  (0 to 32).map(i=>Triple(pointDeg(i),deg2ind(pointDeg(i)),pointName(deg2ind(pointDeg(i)))))
      .map{t=>(printf("%s\t%18s\t%s°\n",t._2,t._3,t._1))}
}
```


Output:

```txt

1	             North	0.0°
2	     North by east	16.87°
3	   North-northeast	16.88°
4	Northeast by north	33.75°
5	         Northeast	50.62°
6	 Northeast by east	50.63°
7	    East-northeast	67.5°
8	     East by north	84.37°
9	              East	84.38°
10	     East by south	101.25°
11	    East-southeast	118.12°
12	 Southeast by east	118.13°
13	         Southeast	135.0°
14	Southeast by south	151.87°
15	   South-southeast	151.88°
16	     South by east	168.75°
17	             South	185.62°
18	     South by west	185.63°
19	   South-southwest	202.5°
20	Southwest by south	219.37°
21	         Southwest	219.38°
22	 Southwest by west	236.25°
23	    West-southwest	253.12°
24	     West by south	253.13°
25	              West	270.0°
26	     West by north	286.87°
27	    West-northwest	286.88°
28	 Northwest by west	303.75°
29	         Northwest	320.62°
30	Northwest by north	320.63°
31	   North-northwest	337.5°
32	     North by west	354.37°
1	             North	354.38°

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";

const array string: names is [] ("North", "North by east", "North-northeast", "Northeast by north",
   "Northeast", "Northeast by east", "East-northeast", "East by north", "East", "East by south",
   "East-southeast", "Southeast by east", "Southeast", "Southeast by south", "South-southeast",
   "South by east", "South", "South by west", "South-southwest", "Southwest by south", "Southwest",
   "Southwest by west", "West-southwest", "West by south", "West", "West by north", "West-northwest",
   "Northwest by west", "Northwest", "Northwest by north", "North-northwest", "North by west", "North");

const proc: main is func
  local
    const array float: degrees is [] (0.0, 16.87, 16.88, 33.75, 50.62, 50.63, 67.5, 84.37, 84.38,
        101.25, 118.12, 118.13, 135.0, 151.87, 151.88, 168.75, 185.62, 185.63, 202.5, 219.37, 219.38,
        236.25, 253.12, 253.13, 270.0, 286.87, 286.88, 303.75, 320.62, 320.63, 337.5, 354.37, 354.38);
    var integer: index is 0;
    var integer: nameIndex is 0;
  begin
    for key index range degrees do
      nameIndex := round(degrees[index] * 32.0 / 360.0);
      writeln(succ(pred(index) rem 32) lpad 2 <& "  " <& names[succ(nameIndex rem 32)] rpad 22 <&
          degrees[index] digits 2 lpad 6);
    end for;
  end func;
```


Output:

```txt

 1  North                   0.00
 2  North by east          16.87
 3  North-northeast        16.88
 4  Northeast by north     33.75
 5  Northeast              50.62
 6  Northeast by east      50.63
 7  East-northeast         67.50
 8  East by north          84.37
 9  East                   84.38
10  East by south         101.25
11  East-southeast        118.12
12  Southeast by east     118.13
13  Southeast             135.00
14  Southeast by south    151.87
15  South-southeast       151.88
16  South by east         168.75
17  South                 185.62
18  South by west         185.63
19  South-southwest       202.50
20  Southwest by south    219.37
21  Southwest             219.38
22  Southwest by west     236.25
23  West-southwest        253.12
24  West by south         253.13
25  West                  270.00
26  West by north         286.87
27  West-northwest        286.88
28  Northwest by west     303.75
29  Northwest             320.62
30  Northwest by north    320.63
31  North-northwest       337.50
32  North by west         354.37
 1  North                 354.38

```



## Sidef

{{trans|Perl 6}}

```ruby
func point (index) {
    var ix = (index % 32);
    if    (ix & 1) { "#{point((ix + 1) & 28)} by #{point(((2 - (ix & 2)) * 4) + ix & 24)}" }
    elsif (ix & 2) { "#{point((ix + 2) & 24)}-#{point((ix | 4) & 28)}" }
    elsif (ix & 4) { "#{point((ix + 8) & 16)}#{point((ix | 8) & 24)}" }
    else           { <north east south west>[ix / 8] }
}

func test_angle (ix) { ix * 11.25 + [0, 5.62, -5.62][ ix % 3 ] };
func angle_to_point(𝜽) { (𝜽 / 360 * 32) + 0.5 -> floor };

for ix in range(0, 32) {
    var 𝜽 = test_angle(ix);
    printf("  %2d %6.2f° %s\n", ix % 32 + 1, 𝜽, point(angle_to_point(𝜽)).tc);
}
```

{{out}}

```txt

   1   0.00° North
   2  16.87° North by east
   3  16.88° North-northeast
   4  33.75° Northeast by north
   5  50.62° Northeast
   6  50.63° Northeast by east
   7  67.50° East-northeast
   8  84.37° East by north
   9  84.38° East
  10 101.25° East by south
  11 118.12° East-southeast
  12 118.13° Southeast by east
  13 135.00° Southeast
  14 151.87° Southeast by south
  15 151.88° South-southeast
  16 168.75° South by east
  17 185.62° South
  18 185.63° South by west
  19 202.50° South-southwest
  20 219.37° Southwest by south
  21 219.38° Southwest
  22 236.25° Southwest by west
  23 253.12° West-southwest
  24 253.13° West by south
  25 270.00° West
  26 286.87° West by north
  27 286.88° West-northwest
  28 303.75° Northwest by west
  29 320.62° Northwest
  30 320.63° Northwest by north
  31 337.50° North-northwest
  32 354.37° North by west
   1 354.38° North

```



## smart BASIC


```smart BASIC
/*Boxing The Compass by rbytes December 2017*/
GET SCREEN SIZE sw,sh
OPTION BASE 1
DIM point$(32,5)
FOR i=1 TO 32
FOR j=1 TO 5
READ point$(i,j)
NEXT j
NEXT i
html$= "<table border=1>"
html$&= "<TR><TD align=right></td><td>Boxing The Compass</td><td>Abbr</td><td>Min</td><td>Med</td><td>Max</td></tr>"
FOR i =1 TO 32
  html$&= "<TR><TD align=right>"&i&"</td><td>"&point$(i,1)&"</td><td>"&point$(i,2)&"</td><td>"&point$(i,3)&"</td><td>"&point$(i,4)&"</td><td>"&point$(i,5)&"</td></tr>"
NEXT i
html$&= "</table>"
BROWSER "compass" AT 0,0 SIZE sw-20,sh-20
BROWSER "compass" TEXT html$
PAUSE 20
END

DATA "North", "N",  "354.38", "0", "5.62", "North by east", "NbE", "5.63",  "11.25", "16.87"
DATA "North-northeast",  "NNE",  "16.88",  "22.50",  "28.12", "Northeast by north",  "NEbN",  "28.13", "33.75", "39.37"
DATA "Northeast", "NE", "39.38", "45.00", "50.62", "Northeast by east", "NEbE", "50.63", "56.25", "61.87"
DATA "East-northeast", "ENE", "61.88", "67.50",  "73.12", "East by north", "EbN", "73.13", "78.75", "84.37"
DATA "East", "E", "84.38", "90.00", "95.62", "East by south", "EbS", "95.63", "101.25", "106.87"
DATA "East-southeast", "ESE", "106.88", "112.50", "118.12", "Southeast by east", "SEbE", "118.13", "123.75", "129.37"
DATA "Southeast", "SE", "129.38", "135.00", "140.62", "Southeast by south", "SEbS", "140.63", "146.25", "151.87"
DATA "South-southeast", "SSE", "151.88", "157.50", "163.12", "South by east", "SbE", "163.13", "168.75", "174.37"
DATA "South", "S", "174.38", "180.00", "185.62", "South by west", "SbW", "185.63", "191.25", "196.87"
DATA "South-southwest", "SSW", "196.88", "202.50", "208.12", "Southwest by south", "SWbS", "208.13", "213.75", "219.37"
DATA "Southwest", "SW", "219.38", "225.00", "230.62", "Southwest by west", "SWbW", "230.63", "236.25", "241.87"
DATA "West-southwest", "WSW", "241.88", "247.50", "253.12", "West by south", "WbS", "253.13", "258.75", "264.37"
DATA "West", "W", "264.38", "270.00", "275.62", "West by north", "WbN", "275.63", "281.25", "286.87"
DATA "West-northwest", "WNW", "286.88", "292.50", "298.12", "Northwest by west", "NWbW", "298.13", "303.75", "309.37"
DATA "Northwest", "NW", "309.38", "315.00", "320.62", "Northwest by north",  "NWbN", "320.63", "326.25", "331.87"
DATA "North-northwest", "NNW", "331.88", "337.50", "343.12", "North by west", "NbW", "343.13", "348.75", "354.37"

```

Output:

```txt

This chart actually displays as a table in a browser object running in smart Basic

   Boxing The Compass   Abbr  Min     Med     Max
 1 North                N     354.38  0       5.62
 2 North by east        NbE   5.63    11.25   16.87
 3 North-northeast      NNE   16.88   22.50   28.12
 4 Northeast by north   NEbN  28.13   33.75   39.37
 5 Northeast            NE    39.38   45.00   50.62
 6 Northeast by east    NEbE  50.63   56.25   61.87
 7 East-northeast       ENE   61.88   67.50   73.12
 8 East by north        EbN   73.13   78.75   84.37
 9 East                 E     84.38   90.00   95.62
10 East by south        EbS   95.63   101.25  106.87
11 East-southeast       ESE   106.88  112.50  118.12
12 Southeast by east    SEbE  118.13  123.75  129.37
13 Southeast            SE    129.38  135.00  140.62
14 Southeast by south   SEbS  140.63  146.25  151.87
15 South-southeast      SSE   151.88  157.50  163.12
16 South by east        SbE   163.13  168.75  174.37
17 South                S     174.38  180.00  185.62
18 South by west        SbW   185.63  191.25  196.87
19 South-southwest      SSW   196.88  202.50  208.12
20 Southwest by south   SWbS  208.13  213.75  219.37
21 Southwest            SW    219.38  225.00  230.62
22 Southwest by west    SWbW  230.63  236.25  241.87
23 West-southwest       WSW   241.88  247.50  253.12
24 West by south        WbS   253.13  258.75  264.37
25 West                 W     264.38  270.00  275.62
26 West by north        WbN   275.63  281.25  286.87
27 West-northwest       WNW   286.88  292.50  298.12
28 Northwest by west    NWbW  298.13  303.75  309.37
29 Northwest            NW    309.38  315.00  320.62
30 Northwest by north   NWbN  320.63  326.25  331.87
31 North-northwest      NNW   331.88  337.50  343.12
32 North by west        NbW   343.13  348.75  354.37

```



## Tcl


```tcl
proc angle2compass {angle} {
    set dirs {
	N NbE N-NE NEbN NE NEbE E-NE EbN E EbS E-SE SEbE SE SEbS S-SE SbE
	S SbW S-SW SWbS SW SWbW W-SW WbS W WbN W-NW NWbW NW NWbN N-NW NbW
    }
    set unpack {N "north" E "east" W "west" S "south" b " by "}

    # Compute the width of each compass segment
    set sep [expr {360.0 / [llength $dirs]}]

    # Work out which segment contains the compass angle
    set dir [expr {round((fmod($angle + $sep/2, 360) - $sep/2) / $sep)}]

    # Convert to a named direction, capitalized as in the wikipedia article
    return [string totitle [string map $unpack [lindex $dirs $dir]]]
}

# Box the compass, using the variable generation algorithm described
for {set i 0} {$i < 33} {incr i} {
    set heading [expr {$i * 11.25}]
    if {$i % 3 == 1} {set heading [expr {$heading + 5.62}]}
    if {$i % 3 == 2} {set heading [expr {$heading - 5.62}]}
    set index [expr {$i % 32 + 1}]

    # Pretty-print the results of converting an angle to a compass heading
    puts [format "%2i %-18s %7.2f°" $index [angle2compass $heading] $heading]
}
```

Output:

```txt

 1 North                 0.00°
 2 North by east        16.87°
 3 North-northeast      16.88°
 4 Northeast by north   33.75°
 5 Northeast            50.62°
 6 Northeast by east    50.63°
 7 East-northeast       67.50°
 8 East by north        84.37°
 9 East                 84.38°
10 East by south       101.25°
11 East-southeast      118.12°
12 Southeast by east   118.13°
13 Southeast           135.00°
14 Southeast by south  151.87°
15 South-southeast     151.88°
16 South by east       168.75°
17 South               185.62°
18 South by west       185.63°
19 South-southwest     202.50°
20 Southwest by south  219.37°
21 Southwest           219.38°
22 Southwest by west   236.25°
23 West-southwest      253.12°
24 West by south       253.13°
25 West                270.00°
26 West by north       286.87°
27 West-northwest      286.88°
28 Northwest by west   303.75°
29 Northwest           320.62°
30 Northwest by north  320.63°
31 North-northwest     337.50°
32 North by west       354.37°
 1 North               354.38°

```



## uBasic/4tH

{{trans|C}}
Since uBasic is an integer interpreter, we have to do some scaling to perform this task.
<lang>Push 0, 1687, 1688, 3375, 5062, 5063, 6750, 8437, 8438, 10125, 11812, 11813
Push 13500, 15187, 15188, 16875, 18562, 18563, 20250, 21937, 21938, 23625
Push 25312, 25313, 27000, 28687, 28688, 30375, 32062, 32063, 33750, 35437
Push 35438                             ' Use the stack as a DATA statement

For x = 32 To 0 Step -1                ' Now read the values, but reverse
  @(x) = Pop()                         ' Since the last value is on top
Next                                   ' of the data stack

For x = 0 To 32                        ' Here comes the payload

  j = ((@(x) * 32 / 3600) + 5) / 10    ' Scale by ten, then correct

  Print Using "_#";(j % 32) + 1;"  ";  ' Print heading
  GoSub 100 + ((j % 32) * 10)          ' Now get the compass point
  Print Using "__#.##"; @(x)           ' Finally, print the angle
                                       ' which is scaled by 100
Next

End
                                       ' All compass points
100 Print "North                 "; : Return
110 Print "North by east         "; : Return
120 Print "North-northeast       "; : Return
130 Print "Northeast by north    "; : Return
140 Print "Northeast             "; : Return
150 Print "Northeast by east     "; : Return
160 Print "East-northeast        "; : Return
170 Print "East by north         "; : Return
180 Print "East                  "; : Return
190 Print "East by south         "; : Return
200 Print "East-southeast        "; : Return
210 Print "Southeast by east     "; : Return
220 Print "Southeast             "; : Return
230 Print "Southeast by south    "; : Return
240 Print "South-southeast       "; : Return
250 Print "South by east         "; : Return
260 Print "South                 "; : Return
270 Print "South by west         "; : Return
280 Print "South-southwest       "; : Return
290 Print "Southwest by south    "; : Return
300 Print "Southwest             "; : Return
310 Print "Southwest by west     "; : Return
320 Print "West-southwest        "; : Return
330 Print "West by south         "; : Return
340 Print "West                  "; : Return
350 Print "West by north         "; : Return
360 Print "West-northwest        "; : Return
370 Print "Northwest by west     "; : Return
380 Print "Northwest             "; : Return
390 Print "Northwest by north    "; : Return
400 Print "North-northwest       "; : Return
410 Print "North by west         "; : Return
420 Print "North                 "; : Return
```

{{out}}

```txt

 1  North                   0.00
 2  North by east          16.87
 3  North-northeast        16.88
 4  Northeast by north     33.75
 5  Northeast              50.62
 6  Northeast by east      50.63
 7  East-northeast         67.50
 8  East by north          84.37
 9  East                   84.38
10  East by south         101.25
11  East-southeast        118.12
12  Southeast by east     118.13
13  Southeast             135.00
14  Southeast by south    151.87
15  South-southeast       151.88
16  South by east         168.75
17  South                 185.62
18  South by west         185.63
19  South-southwest       202.50
20  Southwest by south    219.37
21  Southwest             219.38
22  Southwest by west     236.25
23  West-southwest        253.12
24  West by south         253.13
25  West                  270.00
26  West by north         286.87
27  West-northwest        286.88
28  Northwest by west     303.75
29  Northwest             320.62
30  Northwest by north    320.63
31  North-northwest       337.50
32  North by west         354.37
 1  North                 354.38

0 OK, 0:900

```



## UNIX Shell

{{works with|Bourne Again SHell}}
{{trans|Logo}}

Requires the standard POSIX bc(1) and sed(1) commands to function.


```sh
# List of abbreviated compass point labels
compass_points=( N NbE N-NE NEbN NE NEbE E-NE EbN
                 E EbS E-SE SEbE SE SEbS S-SE SbE
                 S SbW S-SW SWbS SW SWbW W-SW WbS
                 W WbN W-NW NWbW NW NWbN N-NW NbW )

# List of angles to test
test_angles=(  0.00  16.87  16.88  33.75  50.62  50.63  67.50
              84.37  84.38 101.25 118.12 118.13 135.00 151.87
             151.88 168.75 185.62 185.63 202.50 219.37 219.38
             236.25 253.12 253.13 270.00 286.87 286.88 303.75
             320.62 320.63 337.50 354.37 354.38 )


# capitalize a string
function capitalize {
  echo "$1" | sed 's/^./\U&/'
}

# convert compass point abbreviation to full text of label
function expand_point {
  local label="$1"
  set -- N north E east S south W west b " by "
  while (( $# )); do
    label="${label//$1/$2}"
    shift 2
  done
  capitalize "$label"
}

# modulus function that returns 1..N instead of 0..N-1
function amod {
  echo $(( ($1 - 1) % $2 + 1 ))
}

# convert a compass angle from degrees into a box index (1..32)
function compass_point {
  #amod $(dc <<<"$1 5.625 + 11.25 / 1 + p") 32
  amod $(bc <<<"($1 + 5.625) / 11.25 + 1") 32
}

#  Now output the table of test data
header_format="%-7s | %-18s | %s\n"
row_format="%7.2f | %-18s | %2d\n"
printf "$header_format" "Degrees" "Closest Point" "Index"
for angle in ${test_angles[@]}; do
  let index=$(compass_point $angle)
  abbr=${compass_points[index-1]}
  label="$(expand_point $abbr)"
  printf "$row_format" $angle "$label" $index
done
```


Output:
```txt
Degrees | Closest Point      | Index
   0.00 | North              |  1
  16.87 | North by east      |  2
  16.88 | North-northeast    |  3
  33.75 | Northeast by north |  4
  50.62 | Northeast          |  5
  50.63 | Northeast by east  |  6
  67.50 | East-northeast     |  7
  84.37 | East by north      |  8
  84.38 | East               |  9
 101.25 | East by south      | 10
 118.12 | East-southeast     | 11
 118.13 | Southeast by east  | 12
 135.00 | Southeast          | 13
 151.87 | Southeast by south | 14
 151.88 | South-southeast    | 15
 168.75 | South by east      | 16
 185.62 | South              | 17
 185.63 | South by west      | 18
 202.50 | South-southwest    | 19
 219.37 | Southwest by south | 20
 219.38 | Southwest          | 21
 236.25 | Southwest by west  | 22
 253.12 | West-southwest     | 23
 253.13 | West by south      | 24
 270.00 | West               | 25
 286.87 | West by north      | 26
 286.88 | West-northwest     | 27
 303.75 | Northwest by west  | 28
 320.62 | Northwest          | 29
 320.63 | Northwest by north | 30
 337.50 | North-northwest    | 31
 354.37 | North by west      | 32
 354.38 | North              |  1

```



## VBA


```vb
Public Sub box_the_compass()
    Dim compass_point As Integer
    Dim compass_points_all As New Collection
    Dim test_points_all As New Collection
    Dim compass_points(8) As Variant
    Dim test_points(3) As Variant
    compass_points(1) = [{ "North",        "North by east",        "North-northeast",      "Northeast by north"}]
    compass_points(2) = [{ "Northeast",    "Northeast by east",    "East-northeast",       "East by north"}]
    compass_points(3) = [{ "East",         "East by south",        "East-southeast",       "Southeast by east"}]
    compass_points(4) = [{ "Southeast",    "Southeast by south",   "South-southeast",      "South by east"}]
    compass_points(5) = [{ "South",        "South by west",        "South-southwest",      "Southwest by south"}]
    compass_points(6) = [{ "Southwest",    "Southwest by west",    "West-southwest",       "West by south"}]
    compass_points(7) = [{ "West",         "West by north",        "West-northwest",       "Northwest by west"}]
    compass_points(8) = [{ "Northwest",    "Northwest by north",   "North-northwest",      "North by west"}]
    test_points(1) = [{ 0.0,  16.87,  16.88,  33.75,  50.62,  50.63,  67.5,   84.37,  84.38, 101.25, 118.12}]
    test_points(2) = [{ 118.13, 135.0,  151.87, 151.88, 168.75, 185.62, 185.63, 202.5,  219.37, 219.38, 236.25}]
    test_points(3) = [{ 253.12, 253.13, 270.0,  286.87, 286.88, 303.75, 320.62, 320.63, 337.5,  354.37, 354.38}]
    For i = 1 To 3
        For Each t In test_points(i)
            test_points_all.Add t
        Next t
    Next i
    For i = 1 To 8
        For Each c In compass_points(i)
            compass_points_all.Add c
        Next c
    Next i
    For i = 1 To test_points_all.Count
        compass_point = (WorksheetFunction.Floor(test_points_all(i) * 32 / 360 + 0.5, 1) Mod 32) + 1
        Debug.Print Format(compass_point, "@@"); "  "; compass_points_all(compass_point);
        Debug.Print String$(20 - Len(compass_points_all(compass_point)), " ");
        Debug.Print test_points_all(i)
    Next i
End Sub
```
{{out}}

```txt
 1  North                0
 2  North by east        16,87
 3  North-northeast      16,88
 4  Northeast by north   33,75
 5  Northeast            50,62
 6  Northeast by east    50,63
 7  East-northeast       67,5
 8  East by north        84,37
 9  East                 84,38
10  East by south        101,25
11  East-southeast       118,12
12  Southeast by east    118,13
13  Southeast            135
14  Southeast by south   151,87
15  South-southeast      151,88
16  South by east        168,75
17  South                185,62
18  South by west        185,63
19  South-southwest      202,5
20  Southwest by south   219,37
21  Southwest            219,38
22  Southwest by west    236,25
23  West-southwest       253,12
24  West by south        253,13
25  West                 270
26  West by north        286,87
27  West-northwest       286,88
28  Northwest by west    303,75
29  Northwest            320,62
30  Northwest by north   320,63
31  North-northwest      337,5
32  North by west        354,37
 1  North                354,38
```


## Visual Basic .NET



```vbnet
Module BoxingTheCompass
    Dim _points(32) As String

    Sub Main()
        BuildPoints()

        Dim heading As Double = 0D

        For i As Integer = 0 To 32
            heading = i * 11.25
            Select Case i Mod 3
                Case 1
                    heading += 5.62
                Case 2
                    heading -= 5.62
            End Select

            Console.WriteLine("{0,2}: {1,-18} {2,6:F2}°", (i Mod 32) + 1, InitialUpper(GetPoint(heading)), heading)
        Next
    End Sub

    Private Sub BuildPoints()
        Dim cardinal As String() = New String() {"north", "east", "south", "west"}
        Dim pointDesc As String() = New String() {"1", "1 by 2", "1-C", "C by 1", "C", "C by 2", "2-C", "2 by 1"}

        Dim str1, str2, strC As String

        For i As Integer = 0 To 3
            str1 = cardinal(i)
            str2 = cardinal((i + 1) Mod 4)
            strC = IIf(str1 = "north" Or str1 = "south", str1 & str2, str2 & str1)
            For j As Integer = 0 To 7
                _points(i * 8 + j) = pointDesc(j).Replace("1", str1).Replace("2", str2).Replace("C", strC)
            Next
        Next
    End Sub

    Private Function InitialUpper(ByVal s As String) As String
        Return s.Substring(0, 1).ToUpper() & s.Substring(1)
    End Function

    Private Function GetPoint(ByVal Degrees As Double) As String
        Dim testD As Double = (Degrees / 11.25) + 0.5
        Return _points(CInt(Math.Floor(testD Mod 32)))
    End Function
End Module

```

Output:

```txt

 1: North                0.00°
 2: North by east       16.87°
 3: North-northeast     16.88°
 4: Northeast by north  33.75°
 5: Northeast           50.62°
 6: Northeast by east   50.63°
 7: East-northeast      67.50°
 8: East by north       84.37°
 9: East                84.38°
10: East by south      101.25°
11: East-southeast     118.12°
12: Southeast by east  118.13°
13: Southeast          135.00°
14: Southeast by south 151.87°
15: South-southeast    151.88°
16: South by east      168.75°
17: South              185.62°
18: South by west      185.63°
19: South-southwest    202.50°
20: Southwest by south 219.37°
21: Southwest          219.38°
22: Southwest by west  236.25°
23: West-southwest     253.12°
24: West by south      253.13°
25: West               270.00°
26: West by north      286.87°
27: West-northwest     286.88°
28: Northwest by west  303.75°
29: Northwest          320.62°
30: Northwest by north 320.63°
31: North-northwest    337.50°
32: North by west      354.37°
 1: North              354.38°

```



## Yabasic

{{trans|Liberty BASIC}}

```Yabasic

dim point$(32)

for i =1 to 32
    read point$(i)
next i

print "Index\tClosest Point\t\tDegrees"
print "
### ==\t=============\t\t====
"
for i = 0 to 32
    heading = i *11.25
    resto=mod(i,3)
    if resto=1 then
        heading = heading +5.62
    elseif resto=2 then
    	heading = heading -5.62
    end if
    ind = mod(i,32)+1
    print ind,"\t",compasspoint$( heading),"\t",heading
next i

end

sub compasspoint$(h)
    x = h / 11.25 + 1.5
    if (x >= 33.0)  x = x - 32.0
    return point$(int(x))
end sub

10 data  "North             ", "North by east     ", "North-northeast   "
20 data  "Northeast by north", "Northeast         ", "Northeast by east ", "East-northeast    "
30 data  "East by north     ", "East              ", "East by south     ", "East-southeast    "
40 data  "Southeast by east ", "Southeast         ", "Southeast by south", "South-southeast   "
50 data  "South by east     ", "South             ", "South by west     ", "South-southwest   "
60 data  "Southwest by south", "Southwest         ", "Southwest by west ", "West-southwest    "
70 data  "West by south     ", "West              ", "West by north     ", "West-northwest    "
80 data  "Northwest by west ", "Northwest         ", "Northwest by north", "North-northwest   "
90 data  "North by west     "
```



## zkl

{{trans|AWK}}

```zkl
A:=("X N NbE NNE NEbN NE NEbE ENE EbN E EbS ESE "  // one based array
        "SEbE SE SEbS SSE SbE S SbW SSW SWbS SW SWbW "
        "WSW WbS W WbN WNW NWbW NW NWbN NNW NbW").split(" ");

fcn compassBox(d){
    return(( ( (d + 360.0 / 64.0) % 360.0) * 32.0 / 360.0).ceil());
}

foreach i in ([0..32]){
   heading:=11.25*i;
   switch(i%3){  // or heading+=5.62*((i+1).divr(3)[1]-1);
      case(1){ heading+=5.62 }
      case(2){ heading-=5.62 }
   }
   box:=compassBox(heading);
   println("%6.2f\UB0; : %2d\t%s".fmt(heading,box,A[box]));
}
```

{{out}}

```txt

  0.00 :  1	N
 16.87 :  2	NbE
 16.88 :  3	NNE
 33.75 :  4	NEbN
 50.62 :  5	NE
... same as AWK output
337.50 : 31	NNW
354.37 : 32	NbW
354.38 :  1	N

```



## ZX Spectrum Basic

{{trans|Liberty BASIC}}

```zxbasic
10 DATA "North","North by east","North-northeast"
20 DATA "Northeast by north","Northeast","Northeast by east","East-northeast"
30 DATA "East by north","East","East by south","East-southeast"
40 DATA "Southeast by east","Southeast","Southeast by south","South-southeast"
50 DATA "South by east","South","South by west","South-southwest"
60 DATA "Southwest by south","Southwest","Southwest by west","West-southwest"
70 DATA "West by south","West","West by north","West-northwest"
80 DATA "Northwest by west","Northwest","Northwest by north","North-northwest"
90 DATA "North by west"
100 DIM p$(32,18)
110 FOR i=1 TO 32
120 READ p$(i)
130 NEXT i
140 FOR i=0 TO 32
150 LET h=i*11.25
160 LET r=FN m(i,3)
170 IF r=1 THEN LET h=h+5.62: GO TO 190
180 IF r=2 THEN LET h=h-5.62
190 LET ind=FN m(i,32)+1
200 PRINT ind;TAB 4;
210 LET x=h/11.25+1.5
220 IF x>=33 THEN LET x=x-32
230 PRINT p$(INT x);TAB 25;h
240 NEXT i
250 STOP
260 DEF FN m(i,n)=((i/n)-INT (i/n))*n : REM modulus function

```

