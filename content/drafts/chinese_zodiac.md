+++
title = "Chinese zodiac"
description = ""
date = 2019-07-14T13:32:42Z
aliases = []
[extra]
id = 21289
[taxonomies]
categories = []
tags = []
+++

{{task}}Determine the Chinese zodiac sign and related associations for a given year.

Traditionally, the Chinese have counted years using two simultaneous cycles, one of length 10 (the "celestial stems") and one of length 12 (the "terrestrial branches"); the combination results in a repeating 60-year pattern. Mapping the branches to twelve traditional animal deities results in the well-known "Chinese zodiac", assigning each year to a given animal. For example, Saturday, January 28, 2017 CE (in the common Gregorian calendar) begins the lunisolar year of the Rooster.

The celestial stems have no one-to-one mapping like that of the branches to animals; however, the five pairs of consecutive stems each belong to one of the five traditional Chinese elements (Wood, Fire, Earth, Metal, and Water). Further, one of the two years within each element's governance is associated with yin, the other with yang.

Thus, 2017 is also the yin year of Fire. Note that since 12 is an even number, the association between animals and yin/yang doesn't change. Consecutive Years of the Rooster will cycle through the five elements, but will always be yin, despite the apparent conceptual mismatch between the male animals and the female aspect.

;Task: Create a subroutine or program that will return or output the animal, yin/yang association, and element for the lunisolar year that begins in a given CE year.

You may optionally provide more information in the form of the year's numerical position within the 60-year cycle and/or its actual Chinese stem-branch name (in Han characters or Pinyin transliteration).

;Requisite information:
* The animal cycle runs in this order: Rat, Ox, Tiger, Rabbit, Dragon, Snake, Horse, Goat, Monkey, Rooster, Dog, Pig.
* The element cycle runs in this order: Wood, Fire, Earth, Metal, Water.
* The yang year precedes the yin year within each element.
* The current 60-year cycle began in 1984 CE; the first cycle of the Common Era began in 4 CE.

Thus, 1984 was the year of the Wood Rat (yang), 1985 was the year of the Wood Ox (yin), and 1986 the year of the Fire Tiger (yang); 2017 - which, as already noted, is the year of the Fire Rooster (yin) - is the 34th year of the current cycle.

;Information for optional task:
* The ten celestial stems are '''甲''' ''jiă'', '''乙''' ''yĭ'', '''丙''' ''bĭng'', '''丁''' ''dīng'', '''戊''' ''wù'', '''己''' ''jĭ'', '''庚''' ''gēng'', '''辛''' ''xīn'', '''壬''' ''rén'', and '''癸''' ''gŭi''. With the ASCII version of Pinyin tones, the names are written "jia3", "yi3", "bing3", "ding1", "wu4", "ji3", "geng1", "xin1", "ren2", and "gui3".
* The twelve terrestrial branches are '''子''' ''zĭ'', '''丑''' ''chŏu'', '''寅''' ''yín'', '''卯''' ''măo'', '''辰''' ''chén'', '''巳''' ''sì'', '''午''' ''wŭ'', '''未''' ''wèi'', '''申''' ''shēn'', '''酉''' ''yŏu'', '''戌''' ''xū'', '''亥''' ''hài''. In ASCII Pinyin, those are "zi3", "chou3", "yin2", "mao3", "chen2", "si4", "wu3", "wei4", "shen1", "you3", "xu1", and "hai4".

Therefore 1984 was '''甲子''' (''jiă-zĭ'', or jia3-zi3).  2017 is the 34th year of the current cycle, '''丁酉''' (''dīng-yŏu'' or ding1-you3).



## 360 Assembly

{{trans|VBScript}}
<lang>*        Chinese zodiac            10/03/2019
CHINEZOD CSECT
         USING  CHINEZOD,R13       base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         SAVE   (14,12)            save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,=A(NY))   do i=1 to hbound(years)
         LR     R1,R6                i
         SLA    R1,2                 *4
         L      R2,YEARS-4(R1)       ~
         ST     R2,YEAR              year=years(i)
         SH     R2,=H'4'             -4
         LR     R7,R2                year-4
         SRDA   R2,32                ~
         D      R2,=F'10'            /10
         SRA    R2,1                 /2
         MH     R2,=H'6'             *6
         LA     R1,ELEMENTS(R2)      ~
         MVC    ELEMENT,0(R1)      element=elements(mod(year-4,10)/2+1)
         LR     R2,R7                year-4
         SRDA   R2,32                ~
         D      R2,=F'12'            /12
         SLA    R2,3                 *8
         LA     R1,ANIMALS(R2)       ~
         MVC    ANIMAL,0(R1)         animal=animals(mod(year-4,12)+1)
         L      R2,YEAR              year
         SRDA   R2,32                ~
         D      R2,=F'2'             /2
         SLA    R2,2                 *4
         LA     R1,YINYANGS(R2)      ~
         MVC    YINYANG,0(R1)        yinyang=yinyangs(mod(year,2)+1)
         LR     R2,R7                year-4
         SRDA   R2,32                ~
         D      R2,=F'60'            /60
         LA     R2,1(R2)             nn=mod(year-4,60)+1
         L      R1,YEAR              year
         XDECO  R1,XDEC              edit year
         MVC    PG+00(4),XDEC+8      output year
         MVC    PG+24(6),ELEMENT     output element
         MVC    PG+31(8),ANIMAL      output animal
         MVC    PG+41(4),YINYANG     output yinyang
         XDECO  R2,XDEC              edit nn
         MVC    PG+49(2),XDEC+10     output nn
         XPRNT  PG,L'PG              print buffer
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         L      R13,4(0,R13)       restore previous savearea pointer
         RETURN (14,12),RC=0       restore registers from calling sav
NY       EQU    (ANIMAL-YEARS)/4
ANIMALS  DC     CL8'Rat',CL8'Ox',CL8'Tiger',CL8'Rabbit'
         DC     CL8'Dragon',CL8'Snake',CL8'Horse',CL8'Goat'
         DC     CL8'Monkey',CL8'Rooster',CL8'Dog',CL8'Pig'
ELEMENTS DC     CL6'Wood',CL6'Fire',CL6'Earth',CL6'Metal',CL6'Water'
YINYANGS DC     CL4'Yang',CL4'Yin'
YEARS    DC     F'1935',F'1938',F'1968',F'1972',F'1976',F'1984',F'2017'
ANIMAL   DS     CL8
ELEMENT  DS     CL6
YINYANG  DS     CL4
YEAR     DS     F
PG    DC   CL80':::: is the year of the :::::: :::::::: (::::).  ::/60'
XDEC     DS     CL12               temp for xdeco
         REGEQU
         END    CHINEZOD
```

{{out}}

```txt

1935 is the year of the Wood   Pig      (Yin ).  12/60
1938 is the year of the Earth  Tiger    (Yang).  15/60
1968 is the year of the Earth  Monkey   (Yang).  45/60
1972 is the year of the Water  Rat      (Yang).  49/60
1976 is the year of the Fire   Dragon   (Yang).  53/60
1984 is the year of the Wood   Rat      (Yang).   1/60
2017 is the year of the Fire   Rooster  (Yin ).  34/60

```



## AppleScript

{{Trans|JavaScript}}
{{Trans|Haskell}}

```AppleScript
on run
    
    -- TRADITIONAL STRINGS ---------------------------------------------------
    
    -- ts :: Array Int (String, String)            -- 天干 tiangan – 10 heavenly stems
    set ts to zip(chars("甲乙丙丁戊己庚辛壬癸"), ¬
        |words|("jiă yĭ bĭng dīng wù jĭ gēng xīn rén gŭi"))
    
    -- ds :: Array Int (String, String)            -- 地支 dizhi – 12 terrestrial branches
    set ds to zip(chars("子丑寅卯辰巳午未申酉戌亥"), ¬
        |words|("zĭ chŏu yín măo chén sì wŭ wèi shēn yŏu xū hài"))
    
    -- ws :: Array Int (String, String, String)    -- 五行 wuxing – 5 elements
    set ws to zip3(chars("木火土金水"), ¬
        |words|("mù huǒ tǔ jīn shuǐ"), ¬
        |words|("wood fire earth metal water"))
    
    -- xs :: Array Int (String, String, String)    -- 十二生肖 shengxiao – 12 symbolic animals
    set xs to zip3(chars("鼠牛虎兔龍蛇馬羊猴鸡狗豬"), ¬
        |words|("shǔ niú hǔ tù lóng shé mǎ yáng hóu jī gǒu zhū"), ¬
        |words|("rat ox tiger rabbit dragon snake horse goat monkey rooster dog pig"))
    
    -- ys :: Array Int (String, String)            -- 阴阳 yinyang
    set ys to zip(chars("阳阴"), |words|("yáng yīn"))
    
    
    -- TRADITIONAL CYCLES ----------------------------------------------------
    
    script cycles
        on |λ|(y)
            set iYear to y - 4
            set iStem to iYear mod 10
            set iBranch to iYear mod 12
            set {hStem, pStem} to item (iStem + 1) of ts
            set {hBranch, pBranch} to item (iBranch + 1) of ds
            set {hElem, pElem, eElem} to item ((iStem div 2) + 1) of ws
            set {hAnimal, pAnimal, eAnimal} to item (iBranch + 1) of xs
            set {hYinyang, pYinyang} to item ((iYear mod 2) + 1) of ys
            
            {{show(y), hStem & hBranch, hElem, hAnimal, hYinyang}, ¬
                {"", pStem & pBranch, pElem, pAnimal, pYinyang}, ¬
                {"", show((iYear mod 60) + 1) & "/60", eElem, eAnimal, ""}}
        end |λ|
    end script
    
    -- FORMATTING ------------------------------------------------------------
    
    -- fieldWidths :: [[Int]]
    set fieldWidths to {{6, 10, 7, 8, 3}, {6, 11, 8, 8, 4}, {6, 11, 8, 8, 4}}
    
    script showYear
        script widthStringPairs
            on |λ|(nscs)
                set {ns, cs} to nscs
                zip(ns, cs)
            end |λ|
        end script
        
        script justifiedRow
            on |λ|(row)
                script
                    on |λ|(ns)
                        set {n, s} to ns
                        justifyLeft(n, space, s)
                    end |λ|
                end script
                
                concat(map(result, row))
            end |λ|
        end script
        
        on |λ|(y)
            unlines(map(justifiedRow, ¬
                map(widthStringPairs, ¬
                    zip(fieldWidths, |λ|(y) of cycles))))
        end |λ|
    end script
    
    -- TEST OUTPUT -----------------------------------------------------------
    intercalate("\n\n", map(showYear, {1935, 1938, 1968, 1972, 1976, 1984, 2017}))
end run


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- chars :: String -> [String]
on chars(s)
    characters of s
end chars

-- concat :: [[a]] -> [a] | [String] -> String
on concat(xs)
    if length of xs > 0 and class of (item 1 of xs) is string then
        set acc to ""
    else
        set acc to {}
    end if
    repeat with i from 1 to length of xs
        set acc to acc & item i of xs
    end repeat
    acc
end concat

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
on justifyLeft(n, cFiller, strText)
    if n > length of strText then
        text 1 thru n of (strText & replicate(n, cFiller))
    else
        strText
    end if
end justifyLeft

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

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- minimum :: [a] -> a 
on minimum(xs)
    script min
        on |λ|(a, x)
            if x < a or a is missing value then
                x
            else
                a
            end if
        end |λ|
    end script
    
    foldl(min, missing value, xs)
end minimum

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

-- Egyptian multiplication - progressively doubling a list, appending
-- stages of doubling to an accumulator where needed for binary 
-- assembly of a target length

-- replicate :: Int -> a -> [a]
on replicate(n, a)
    set out to {}
    if n < 1 then return out
    set dbl to {a}
    
    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- show :: a -> String
on show(e)
    set c to class of e
    if c = list then
        script serialized
            on |λ|(v)
                show(v)
            end |λ|
        end script
        
        "[" & intercalate(", ", map(serialized, e)) & "]"
    else if c = record then
        script showField
            on |λ|(kv)
                set {k, ev} to kv
                "\"" & k & "\":" & show(ev)
            end |λ|
        end script
        
        "{" & intercalate(", ", ¬
            map(showField, zip(allKeys(e), allValues(e)))) & "}"
    else if c = date then
        "\"" & iso8601Z(e) & "\""
    else if c = text then
        "\"" & e & "\""
    else if (c = integer or c = real) then
        e as text
    else if c = class then
        "null"
    else
        try
            e as text
        on error
            ("«" & c as text) & "»"
        end try
    end if
end show

-- unlines :: [String] -> String
on unlines(xs)
    intercalate(linefeed, xs)
end unlines

-- words :: String -> [String]
on |words|(s)
    words of s
end |words|

-- zip :: [a] -> [b] -> [(a, b)]
on zip(xs, ys)
    set lng to min(length of xs, length of ys)
    set lst to {}
    repeat with i from 1 to lng
        set end of lst to {item i of xs, item i of ys}
    end repeat
    return lst
end zip

-- zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
on zip3(xs, ys, zs)
    script
        on |λ|(x, i)
            [x, item i of ys, item i of zs]
        end |λ|
    end script
    
    map(result, items 1 thru ¬
        minimum({length of xs, length of ys, length of zs}) of xs)
end zip3
```

{{Out}}

```txt
1935  乙亥        木      豬       阴  
      yĭhài      mù      zhū     yīn 
      12/60      wood    pig         

1938  戊寅        土      虎       阳  
      wùyín      tǔ      hǔ      yáng
      15/60      earth   tiger       

1968  戊申        土      猴       阳  
      wùshēn     tǔ      hóu     yáng
      45/60      earth   monkey      

1972  壬子        水      鼠       阳  
      rénzĭ      shuǐ    shǔ     yáng
      49/60      water   rat         

1976  丙辰        火      龍       阳  
      bĭngchén   huǒ     lóng    yáng
      53/60      fire    dragon      

1984  甲子        木      鼠       阳  
      jiăzĭ      mù      shǔ     yáng
      1/60       wood    rat         

2017  丁酉        火      鸡       阴  
      dīngyŏu    huǒ     jī      yīn 
      34/60      fire    rooster     
```


## AWK


```AWK

# syntax: GAWK -f CHINESE_ZODIAC.AWK
BEGIN {
    print("year element animal  aspect")
    split("Rat,Ox,Tiger,Rabbit,Dragon,Snake,Horse,Goat,Monkey,Rooster,Dog,Pig",animal_arr,",")
    split("Wood,Fire,Earth,Metal,Water",element_arr,",")
    n = split("1935,1938,1968,1972,1976,1984,1985,2017",year_arr,",")
    for (i=1; i<=n; i++) {
      year = year_arr[i]
      element = element_arr[int((year-4)%10/2)+1]
      animal = animal_arr[(year-4)%12+1]
      yy = (year%2 == 0) ? "Yang" : "Yin"
      printf("%4d %-7s %-7s %s\n",year,element,animal,yy)
    }
    exit(0)
}

```

{{out}}

```txt

year element animal  aspect
1935 Wood    Pig     Yin
1938 Earth   Tiger   Yang
1968 Earth   Monkey  Yang
1972 Water   Rat     Yang
1976 Fire    Dragon  Yang
1984 Wood    Rat     Yang
1985 Wood    Ox      Yin
2017 Fire    Rooster Yin

```



## BASIC256


```BASIC256

# Chinese zodiac

elementos = {"Wood", "Fire", "Earth", "Metal", "Water"}
animales = {"Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig"}
aspectos = {"Yang","Yin"}
tallo_celestial = {'甲', '乙', '丙', '丁', '戊', '己', '庚', '辛', '壬', '癸'}
rama_terrestre = {'子','丑','寅','卯','辰','巳','午','未','申','酉','戌','亥'}
tallos_pinyin = {"jiă","yĭ","bĭng","dīng","wù","jĭ","gēng","xīn","rén","gŭi"}
ramas_pinyin = {"zĭ","chŏu","yín","măo","chén","sì","wŭ","wèi","shēn","yŏu","xū","hài"}
years = {1935, 1938, 1968, 1972, 1976, 1984, 2017}

For i = 0 To years[?]-1
	xYear    = years[i]
	yElemento = elementos[((xYear - 4) % 10) \ 2]
	yAnimal   = animales[  (xYear - 4) % 12     ]
	yAspectos = aspectos[   xYear      %  2     ]
	ytallo_celestial  = tallo_celestial[((xYear - 4) % 10)]
	yrama_terrestre   = rama_terrestre[  (xYear - 4) % 12 ]
	ytallos_pinyin    = tallos_pinyin[  ((xYear - 4) % 10)]
	yramas_pinyin     = ramas_pinyin[    (xYear - 4) % 12 ]
	ciclo = ((xYear - 4) % 60) + 1
	Print xYear & ": " & ytallo_celestial & yrama_terrestre & " (" & ytallos_pinyin & "-" & yramas_pinyin & ", " & yElemento & " " &  yAnimal & "; " &  yAspectos & " - ciclo " &ciclo & "/60)"
Next i
End

```

{{out}}

```txt

1935: 乙亥 (yĭ-hài, Wood Pig; Yin - ciclo 12/60)
1938: 戊寅 (wù-yín, Earth Tiger; Yang - ciclo 15/60)
1968: 戊申 (wù-shēn, Earth Monkey; Yang - ciclo 45/60)
1972: 壬子 (rén-zĭ, Water Rat; Yang - ciclo 49/60)
1976: 丙辰 (bĭng-chén, Fire Dragon; Yang - ciclo 53/60)
1984: 甲子 (jiă-zĭ, Wood Rat; Yang - ciclo 1/60)
2017: 丁酉 (dīng-yŏu, Fire Rooster; Yin - ciclo 34/60)

```



## Befunge



```befunge
0"    :raeY">:#,_55+"< /8"&>+:66+%00p:55+v
v"Aspect:  "0++88*5%2\0\+1%"<":p01++66/2%<
>00g5g7-0"  :laminA"10g5g"<"+0" :tnemelE"v
v!:,+55$_v#!-*84,:g+5/<  >:#,_$.,,.,@ >0#<
_>>:#,_$>>1+::"("%\"("^  ^"Cycle:   " <<<<
 $'-4;AGLS[_ %*06yang yin Rat Ox Tiger R |
abbit Dragon Snake Horse Goat Monkey Roo |
ster Dog Pig Wood Fire Earth Metal Water |
```


{{out}}


```txt
Year:    2018
Element: Earth
Animal:  Dog
Aspect:  yang
Cycle:   35 / 60
```



## C

{{trans|C++}}

```c>#include <math.h

#include <stdio.h>

const char* animals[] = { "Rat","Ox","Tiger","Rabbit","Dragon","Snake","Horse","Goat","Monkey","Rooster","Dog","Pig" };
const char* elements[] = { "Wood","Fire","Earth","Metal","Water" };

const char* getElement(int year) {
    int element = (int)floor((year - 4) % 10 / 2);
    return elements[element];
}

const char* getAnimal(int year) {
    return animals[(year - 4) % 12];
}

const char* getYY(int year) {
    if (year % 2 == 0) {
        return "yang";
    } else {
        return "yin";
    }
}

int main() {
    int years[] = { 1935, 1938, 1968, 1972, 1976, 2017 };
    int i;

    //the zodiac cycle didnt start until 4 CE, so years <4 shouldnt be valid
    for (i = 0; i < 6; ++i) {
        int year = years[i];
        printf("%d is the year of the %s %s (%s).\n", year, getElement(year), getAnimal(year), getYY(year));
    }

    return 0;
}
```

{{out}}

```txt
1935 is the year of the Wood Pig (yin).
1938 is the year of the Earth Tiger (yang).
1968 is the year of the Earth Monkey (yang).
1972 is the year of the Water Rat (yang).
1976 is the year of the Fire Dragon (yang).
2017 is the year of the Fire Rooster (yin).
```



## C++


```cpp>#include <iostream

#include <cmath>

using namespace std;

const string animals[]={"Rat","Ox","Tiger","Rabbit","Dragon","Snake","Horse","Goat","Monkey","Rooster","Dog","Pig"};
const string elements[]={"Wood","Fire","Earth","Metal","Water"};

string getElement(int year)
{
    int element = floor((year-4)%10/2);
    return elements[element];
}

string getAnimal(int year)
{
    return animals[(year-4)%12];
}

string getYY(int year)
{
    if(year%2==0)
    {
        return "yang";
    }
    else
    {
        return "yin";
    }
}

int main()
{
    int years[]={1935,1938,1968,1972,1976,2017};
    //the zodiac cycle didnt start until 4 CE, so years <4 shouldnt be valid
    for(int i=0;i<6;i++)
    {
        cout << years[i] << " is the year of the " << getElement(years[i]) << " " << getAnimal(years[i]) << " (" << getYY(years[i]) << ")." << endl;
    }
    return 0;
}
```

{{out}}

```txt

1935 is the year of the Wood Pig (yin).
1938 is the year of the Earth Tiger (yang).
1968 is the year of the Earth Monkey (yang).
1972 is the year of the Water Rat (yang).
1976 is the year of the Fire Dragon (yang).
2017 is the year of the Fire Rooster (yin).

```


=={{header|C#|C sharp}}==
{{trans|Java}}

```csharp
using System;

namespace ChineseZodiac {
    class Program {
        static string[] animals = { "Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig" };
        static string[] elements = { "Wood", "Fire", "Earth", "Metal", "Water" };
        static string[] animalChars = { "子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥" };
        static string[,] elementChars = { { "甲", "丙", "戊", "庚", "壬" }, { "乙", "丁", "己", "辛", "癸" } };

        static string getYY(int year) {
            if (year % 2 == 0) {
                return "yang";
            }
            return "yin";
        }

        static void Main(string[] args) {
            Console.OutputEncoding = System.Text.Encoding.UTF8;
            int[] years = { 1935, 1938, 1968, 1972, 1976, 1984, 1985, 2017 };
            for (int i = 0; i < years.Length; i++) {
                int ei = (int)Math.Floor((years[i] - 4.0) % 10 / 2);
                int ai = (years[i] - 4) % 12;
                Console.WriteLine("{0} is the year of the {1} {2} ({3}). {4}{5}", years[i], elements[ei], animals[ai], getYY(years[i]), elementChars[years[i] % 2, ei], animalChars[(years[i] - 4) % 12]);
            }
        }
    }
}
```

{{out}}

```txt
1935 is the year of the Wood Pig (yin). 乙亥
1938 is the year of the Earth Tiger (yang). 戊寅
1968 is the year of the Earth Monkey (yang). 戊申
1972 is the year of the Water Rat (yang). 壬子
1976 is the year of the Fire Dragon (yang). 丙辰
1984 is the year of the Wood Rat (yang). 甲子
1985 is the year of the Wood Ox (yin). 乙丑
2017 is the year of the Fire Rooster (yin). 丁酉
```



## Clojure


```clojure
(def base-year 4)
(def celestial-stems ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(def terrestrial-branches ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])
(def zodiac-animals ["Rat" "Ox" "Tiger" "Rabbit" "Dragon" "Snake" "Horse" "Goat" "Monkey" "Rooster" "Dog" "Pig"])
(def elements ["Wood" "Fire" "Earth" "Metal" "Water"])
(def aspects ["yang" "yin"])
(def pinyin (zipmap (concat celestial-stems terrestrial-branches)
                 '("jiă" "yĭ" "bĭng" "dīng" "wù" "jĭ" "gēng" "xīn" "rén" "gŭi"
                   "zĭ" "chŏu" "yín" "măo" "chén" "sì" "wŭ" "wèi" "shēn" "yŏu" "xū" "hài")))

(defn chinese-zodiac [year]
  (let [cycle-year (- year base-year)
        cycle-position (inc (mod cycle-year 60))
        stem-number (mod cycle-year 10)
        stem-han (nth celestial-stems stem-number)
        stem-pinyin (get pinyin stem-han)
        element-number (int (Math/floor (/ stem-number 2)))
        element (nth elements element-number)
        branch-number (mod cycle-year 12)
        branch-han (nth terrestrial-branches branch-number)
        branch-pinyin (get pinyin branch-han)
        zodiac-animal (nth zodiac-animals branch-number)
        aspect-number (mod cycle-year 2)
        aspect (nth aspects aspect-number)]
    (println (format "%s: %s%s (%s-%s, %s %s; %s - cycle %s/60)"
                     year stem-han branch-han stem-pinyin branch-pinyin element zodiac-animal aspect cycle-position))))

(defn -main [& args]
  (doseq [years (map read-string args)]
    (chinese-zodiac years)))
```


{{out}}

```txt

$ lein run 1935 1938 1968 1972 1976 1984 2017
1935: 乙亥 (yĭ-hài, Wood Pig; yin - cycle 12/60)
1938: 戊寅 (wù-yín, Earth Tiger; yang - cycle 15/60)
1968: 戊申 (wù-shēn, Earth Monkey; yang - cycle 45/60)
1972: 壬子 (rén-zĭ, Water Rat; yang - cycle 49/60)
1976: 丙辰 (bĭng-chén, Fire Dragon; yang - cycle 53/60)
1984: 甲子 (jiă-zĭ, Wood Rat; yang - cycle 1/60)
2017: 丁酉 (dīng-yŏu, Fire Rooster; yin - cycle 34/60)

```



## Common Lisp

{{trans|Ruby}}

```lisp
; Any CE Year that was the first of a 60-year cycle
(defconstant base-year 1984)

(defconstant celestial-stems
  '("甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"))

(defconstant terrestrial-branches
  '("子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"))

(defconstant zodiac-animals
  '("Rat"   "Ox"   "Tiger"  "Rabbit"  "Dragon" "Snake"
    "Horse" "Goat" "Monkey" "Rooster" "Dog"    "Pig"))

(defconstant elements '("Wood" "Fire" "Earth" "Metal" "Water"))

(defconstant aspects '("yang" "yin"))

(defconstant pinyin
  (pairlis (append celestial-stems terrestrial-branches)
    '("jiă" "yĭ" "bĭng" "dīng" "wù" "jĭ" "gēng" "xīn" "rén" "gŭi"
      "zĭ" "chŏu" "yín" "măo" "chén" "sì" "wŭ" "wèi" "shēn" "yŏu" "xū" "hài")))

(defun this-year () (nth 5 (multiple-value-list (get-decoded-time))))

(defun pinyin-for (han) (cdr (assoc han pinyin :test #'string=)))

(defun chinese-zodiac (&rest years)
 (loop for ce-year in (if (null years) (list (this-year)) years) collecting
   (let* ((cycle-year (- ce-year base-year))
          (stem-number (mod cycle-year 10))
          (stem-han    (nth stem-number celestial-stems))
          (stem-pinyin (pinyin-for stem-han))

          (element-number (floor stem-number 2))
          (element        (nth element-number elements))

          (branch-number (mod cycle-year 12))
          (branch-han    (nth branch-number terrestrial-branches))
          (branch-pinyin (pinyin-for branch-han))
          (zodiac-animal (nth branch-number zodiac-animals))

          (aspect-number (mod cycle-year 2))
          (aspect        (nth aspect-number aspects)))
          (cons ce-year (list stem-han branch-han stem-pinyin branch-pinyin element zodiac-animal aspect)))))

(defun get-args ()
  (or
   #+CLISP *args*
   #+SBCL (cdr *posix-argv*)
   #+LISPWORKS system:*line-arguments-list*
   #+CMU extensions:*command-line-words*
   nil))

(loop for cz in (apply #'chinese-zodiac (mapcar #'read-from-string (get-args)))
 doing
  (format t "~{~a: ~a~a (~a-~a, ~a ~a; ~a)~%~}" cz))
```


{{Out}}

```txt
$ sbcl --script cz.cl
2017: 丁酉 (dīng-yŏu, Fire Rooster; yin)
$ clisp cz.cl  193{5,8} 194{1,7} 1968 197{2,6}
1935: 乙亥 (yĭ-hài, Wood Pig; yin)
1938: 戊寅 (wù-yín, Earth Tiger; yang)
1941: 辛巳 (xīn-sì, Metal Snake; yin)
1947: 丁亥 (dīng-hài, Fire Pig; yin)
1968: 戊申 (wù-shēn, Earth Monkey; yang)
1972: 壬子 (rén-zĭ, Water Rat; yang)
1976: 丙辰 (bĭng-chén, Fire Dragon; yang)

```



## D

{{trans|haskell}}

```D
import std.stdio;

// 10 heavenly stems
immutable tiangan=[
    ["甲","乙","丙","丁","戊","己","庚","辛","壬","癸"],
    ["jiă","yĭ","bĭng","dīng","wù","jĭ","gēng","xīn","rén","gŭi"]
];

// 12 terrestrial branches
immutable dizhi=[
    ["子","丑","寅","卯","辰","巳","午","未","申","酉","戌","亥"],
    ["zĭ","chŏu","yín","măo","chén","sì","wŭ","wèi","shēn","yŏu","xū","hài"]
];

// 5 elements
immutable wuxing=[
    ["木","火","土","金","水"],
    ["mù","huǒ","tǔ","jīn","shuǐ"],
    ["wood","fire","earth","metal","water"]
];

// 12 symbolic animals
immutable shengxiao=[
    ["鼠","牛","虎","兔","龍","蛇","馬","羊","猴","鸡","狗","豬"],
    ["shǔ","niú","hǔ","tù","lóng","shé","mǎ","yáng","hóu","jī","gǒu","zhū"],
    ["rat","ox","tiger","rabbit","dragon","snake","horse","goat","monkey","rooster","dog","pig"]
];

// yin yang
immutable yinyang=[
    ["阳","阴"],
    ["yáng","yīn"]
];

void main(string[] args) {
    process(args[1..$]);
}

void process(string[] years) {
    import std.conv;
    foreach(yearStr; years) {
        try {
            auto year = to!int(yearStr);

            auto cy = year - 4;
            auto stem = cy % 10;
            auto branch = cy % 12;

            writefln("%4s  %-11s  %-7s  %-10s%s", year,tiangan[0][stem]~dizhi[0][branch], wuxing[0][stem/2], shengxiao[0][branch], yinyang[0][year%2]);
            writefln("      %-12s%-8s%-10s%s",      tiangan[1][stem]~dizhi[1][branch],    wuxing[1][stem/2], shengxiao[1][branch], yinyang[1][year%2]);
            writefln("      %2s/60     %-7s%s", cy%60+1,                                  wuxing[2][stem/2], shengxiao[2][branch]);
            writeln;
        } catch (ConvException e) {
            stderr.writeln("Not a valid year: ", yearStr);
        }
    }
}

```


{{Out}}

```txt
1935  乙亥        木      豬       阴  
      yĭhài      mù      zhū     yīn 
      12/60      wood    pig         

1938  戊寅        土      虎       阳  
      wùyín      tǔ      hǔ      yáng
      15/60      earth   tiger       

1968  戊申        土      猴       阳  
      wùshēn     tǔ      hóu     yáng
      45/60      earth   monkey      

1972  壬子        水      鼠       阳  
      rénzĭ      shuǐ    shǔ     yáng
      49/60      water   rat         

1976  丙辰        火      龍       阳  
      bĭngchén   huǒ     lóng    yáng
      53/60      fire    dragon      

1984  甲子        木      鼠       阳  
      jiăzĭ      mù      shǔ     yáng
      1/60       wood    rat         

2017  丁酉        火      鸡       阴  
      dīngyŏu    huǒ     jī      yīn 
      34/60      fire    rooster     
```



=={{header|F Sharp|F#}}==

```fsharp

open System

let animals = ["Rat";"Ox";"Tiger";"Rabbit";"Dragon";"Snake";"Horse";"Goat";"Monkey";"Rooster";"Dog";"Pig"]
let elements = ["Wood";"Fire";"Earth";"Metal";"Water"]
let years = [1935;1938;1968;1972;1976;1984;1985;2017]

let getZodiac(year: int) = 
    let animal = animals.Item((year-4)%12)
    let element = elements.Item(((year-4)%10)/2)
    let yy = if year%2 = 0 then "(Yang)" else "(Yin)"
    
    String.Format("{0} is the year of the {1} {2} {3}", year, element, animal, yy)

[<EntryPoint>]
let main argv =
    let mutable string = ""
    for i in years do 
        string <- getZodiac(i)
        printf "%s" string
        Console.ReadLine() |> ignore
    0 // return an integer exit code

```

{{out}}

```txt

1935 is the year of the Wood Pig (Yin)
1938 is the year of the Earth Tiger (Yang)
1968 is the year of the Earth Monkey (Yang)
1972 is the year of the Water Rat (Yang)
1976 is the year of the Fire dragon (Yang)
1984 is the year of the Wood Rat (Yang)
1985 is the year of the Wood Ox (Yin)
2017 is the year of the Fire Rooster (Yin)

```



## Factor


```factor
USING: circular formatting io kernel math qw sequences
sequences.repeating ;
IN: rosetta-code.zodiac

<PRIVATE

! Offset start index by -4 because first cycle started on 4 CE.
: circularize ( seq -- obj )
    [ -4 ] dip <circular> [ change-circular-start ] keep ;

: animals ( -- obj )
    qw{
        Rat Ox Tiger Rabbit Dragon Snake Horse Goat Monkey
        Rooster Dog Pig
    } circularize ;

: elements ( -- obj )
    qw{ Wood Fire Earth Metal Water } 2 <repeats> circularize ;

PRIVATE>

: zodiac ( n -- str )
    dup [ elements nth ] [ animals nth ]
    [ even? "yang" "yin" ? ] tri
    "%d is the year of the %s %s (%s)." sprintf ;

: zodiac-demo ( -- )
    { 1935 1938 1968 1972 1976 1984 1985 2017 }
    [ zodiac print ] each ;

MAIN: zodiac-demo
```

{{out}}

```txt

1935 is the year of the Wood Pig (yin).
1938 is the year of the Earth Tiger (yang).
1968 is the year of the Earth Monkey (yang).
1972 is the year of the Water Rat (yang).
1976 is the year of the Fire Dragon (yang).
1984 is the year of the Wood Rat (yang).
1985 is the year of the Wood Ox (yin).
2017 is the year of the Fire Rooster (yin).

```



## Go


```go
package main

import "fmt"

var (
    animalString = []string{"Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake",
        "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig"}
    stemYYString  = []string{"Yang", "Yin"}
    elementString = []string{"Wood", "Fire", "Earth", "Metal", "Water"}
    stemCh        = []rune("甲乙丙丁戊己庚辛壬癸")
    branchCh      = []rune("子丑寅卯辰巳午未申酉戌亥")
)

func cz(yr int) (animal, yinYang, element, stemBranch string, cycleYear int) {
    yr -= 4
    stem := yr % 10
    branch := yr % 12
    return animalString[branch],
        stemYYString[stem%2],
        elementString[stem/2],
        string([]rune{stemCh[stem], branchCh[branch]}),
        yr%60 + 1
}

func main() {
    for _, yr := range []int{1935, 1938, 1968, 1972, 1976} {
        a, yy, e, sb, cy := cz(yr)
        fmt.Printf("%d: %s %s, %s, Cycle year %d %s\n",
            yr, e, a, yy, cy, sb)
    }
}
```

{{out}}

```txt

1935: Wood Pig, Yin, Cycle year 12 乙亥
1938: Earth Tiger, Yang, Cycle year 15 戊寅
1968: Earth Monkey, Yang, Cycle year 45 戊申
1972: Water Rat, Yang, Cycle year 49 壬子
1976: Fire Dragon, Yang, Cycle year 53 丙辰

```



## Haskell

(We can use Chinese characters in Haskell names, as long as the first character is lower-case alphabetic)

```haskell
import Data.Array (Array, listArray, (!))

-- TRADITIONAL STRINGS  ------------------------------------------------------
ats :: Array Int (Char, String)
ats =
  listArray (0, 9) $
  zip
    "甲乙丙丁戊己庚辛壬癸" -- 天干 tiangan – 10 heavenly stems
    (words "jiă yĭ bĭng dīng wù jĭ gēng xīn rén gŭi")

ads :: Array Int (String, String)
ads =
  listArray (0, 11) $
  zip
    (chars "子丑寅卯辰巳午未申酉戌亥") -- 地支 dizhi – 12 terrestrial branches
    (words "zĭ chŏu yín măo chén sì wŭ wèi shēn yŏu xū hài")

aws :: Array Int (String, String, String)
aws =
  listArray (0, 4) $
  zip3
    (chars "木火土金水") -- 五行 wuxing – 5 elements
    (words "mù huǒ tǔ jīn shuǐ")
    (words "wood fire earth metal water")

axs :: Array Int (String, String, String)
axs =
  listArray (0, 11) $
  zip3
    (chars "鼠牛虎兔龍蛇馬羊猴鸡狗豬") -- 十二生肖 shengxiao – 12 symbolic animals
    (words "shǔ niú hǔ tù lóng shé mǎ yáng hóu jī gǒu zhū")
    (words "rat ox tiger rabbit dragon snake horse goat monkey rooster dog pig")

ays :: Array Int (String, String)
ays = listArray (0, 1) $ zip (chars "阳阴") (words "yáng yīn") -- 阴阳 yinyang

chars :: String -> [String]
chars = (flip (:) [] <$>)

-- TRADITIONAL CYCLES --------------------------------------------------------
f生肖五行年份 y =
  let i年份 = y - 4
      i天干 = rem i年份 10
      i地支 = rem i年份 12
      (h天干, p天干) = ats ! i天干
      (h地支, p地支) = ads ! i地支
      (h五行, p五行, e五行) = aws ! quot i天干 2
      (h生肖, p生肖, e生肖) = axs ! i地支
      (h阴阳, p阴阳) = ays ! rem i年份 2
  in [ [show y, h天干 : h地支, h五行, h生肖, h阴阳] -- 汉子 Chinese characters
     , [[], p天干 ++ p地支, p五行, p生肖, p阴阳] -- Pinyin roman transcription
     , [[], show (rem i年份 60 + 1) ++ "/60", e五行, e生肖, []] -- English
     ]

-- FORMATTING ----------------------------------------------------------------
fieldWidths :: [[Int]]
fieldWidths = [[6, 10, 7, 8, 3], [6, 11, 8, 8, 4], [6, 11, 8, 8, 4]]

showYear :: Int -> String
showYear y =
  unlines $
  (\(ns, xs) -> concat $ uncurry (`justifyLeft` ' ') <$> zip ns xs) <$>
  zip fieldWidths (f生肖五行年份 y)
  where
    justifyLeft n c s = take n (s ++ replicate n c)

-- TEST OUTPUT ---------------------------------------------------------------
main :: IO ()
main = mapM_ putStrLn $ showYear <$> [1935, 1938, 1968, 1972, 1976, 1984, 2017]
```

{{Out}}

```txt
1935  乙亥        木      豬       阴  
      yĭhài      mù      zhū     yīn 
      12/60      wood    pig         

1938  戊寅        土      虎       阳  
      wùyín      tǔ      hǔ      yáng
      15/60      earth   tiger       

1968  戊申        土      猴       阳  
      wùshēn     tǔ      hóu     yáng
      45/60      earth   monkey      

1972  壬子        水      鼠       阳  
      rénzĭ      shuǐ    shǔ     yáng
      49/60      water   rat         

1976  丙辰        火      龍       阳  
      bĭngchén   huǒ     lóng    yáng
      53/60      fire    dragon      

1984  甲子        木      鼠       阳  
      jiăzĭ      mù      shǔ     yáng
      1/60       wood    rat         

2017  丁酉        火      鸡       阴  
      dīngyŏu    huǒ     jī      yīn 
      34/60      fire    rooster   
```



## J


```J
   ELEMENTS=: _4 |. 2 # ;:'Wood Fire Earth Metal Water'
   YEARS=: 1935 1938 1968 1972 1976 2017

   ANIMALS=: _4 |. ;:'Rat Ox Tiger Rabbit Dragon Snake Horse Goat Monkey Rooster Dog Pig'
   YINYANG=: ;:'yang yin'

   cz=: (|~ #)~ { [

   ANIMALS cz YEARS
┌───┬─────┬──────┬───┬──────┬───────┐
│Pig│Tiger│Monkey│Rat│Dragon│Rooster│
└───┴─────┴──────┴───┴──────┴───────┘

   YINYANG cz YEARS
┌───┬────┬────┬────┬────┬───┐
│yin│yang│yang│yang│yang│yin│
└───┴────┴────┴────┴────┴───┘

   chinese_zodiac =: 3 : ';:inv(<":y),(ELEMENTS cz y),(ANIMALS cz y),(<''(''),(YINYANG cz y),(<'')'')'

   chinese_zodiac&>YEARS
1935 Wood Pig ( yin )     
1938 Earth Tiger ( yang ) 
1968 Earth Monkey ( yang )
1972 Water Rat ( yang )   
1976 Fire Dragon ( yang ) 
2017 Fire Rooster ( yin )


   'CELESTIAL TERRESTRIAL'=:7&u:&.>{&a.&.> 16be7 16b94 16bb2 16be4 16bb9 16b99 16be4 16bb8 16b99 16be4 16bb8 16b81 16be6 16b88 16b8a 16be5 16bb7 16bb1 16be5 16bba 16b9a 16be8 16bbe 16b9b 16be5 16ba3 16bac 16be7 16b99 16bb8; 16be5 16bad 16b90 16be4 16bb8 16b91 16be5 16baf 16b85 16be5 16b8d 16baf 16be8 16bbe 16bb0 16be5 16bb7 16bb3 16be5 16b8d 16b88 16be6 16b9c 16baa 16be7 16b94 16bb3 16be9 16b85 16b89 16be6 16b88 16b8c 16be4 16bba 16ba5

   ANIMALS=: ;/ _4 |. TERRESTRIAL
   ELEMENTS=: ;/ _4 |. CELESTIAL

   chinese_zodiac&>YEARS
1935 乙 亥 ( yin ) 
1938 戊 寅 ( yang )
1968 戊 申 ( yang )
1972 壬 子 ( yang )
1976 丙 辰 ( yang )
2017 丁 酉 ( yin ) 

```



## Java


```Java
public class Zodiac {

	final static String animals[]={"Rat","Ox","Tiger","Rabbit","Dragon","Snake","Horse","Goat","Monkey","Rooster","Dog","Pig"};
	final static String elements[]={"Wood","Fire","Earth","Metal","Water"};
	final static String animalChars[]={"子","丑","寅","卯","辰","巳","午","未","申","酉","戌","亥"};
	static String elementChars[][]={{"甲","丙","戊","庚","壬"},{"乙","丁","己","辛","癸"}};

	static String getYY(int year)
	{
	    if(year%2==0)
	    {
	        return "yang";
	    }
	    else
	    {
	        return "yin";
	    }
	}

	public static void main(String[] args)
	{
		int years[]={1935,1938,1968,1972,1976,1984,1985,2017};
		for(int i=0;i<years.length;i++)
		{
			System.out.println(years[i]+" is the year of the "+elements[(int) Math.floor((years[i]-4)%10/2)]+" "+animals[(years[i]-4)%12]+" ("+getYY(years[i])+"). "+elementChars[years[i]%2][(int) Math.floor((years[i]-4)%10/2)]+animalChars[(years[i]-4)%12]);
		}
	}
}

```

{{out}}

```txt

1935 is the year of the Wood Pig (yin). 乙亥
1938 is the year of the Earth Tiger (yang). 戊寅
1968 is the year of the Earth Monkey (yang). 戊申
1972 is the year of the Water Rat (yang). 壬子
1976 is the year of the Fire Dragon (yang). 丙辰
1984 is the year of the Wood Rat (yang). 甲子
1985 is the year of the Wood Ox (yin). 乙丑
2017 is the year of the Fire Rooster (yin). 丁酉

```



## JavaScript



### ES6

{{Trans|Haskell}}

```JavaScript
(() => {
    'use strict';

    // GENERIC FUNCTIONS -----------------------------------------------------

    // chars :: String -> [Char]
    const chars = s => s.split('');

    // concat :: [[a]] -> [a] | [String] -> String
    const concat = xs =>
        xs.length > 0 ? (() => {
            const unit = typeof xs[0] === 'string' ? '' : [];
            return unit.concat.apply(unit, xs);
        })() : [];

    // intercalate :: String -> [a] -> String
    const intercalate = (s, xs) => xs.join(s);

    // justifyLeft :: Int -> Char -> Text -> Text
    const justifyLeft = (n, cFiller, strText) =>
        n > strText.length ? (
            (strText + cFiller.repeat(n))
            .substr(0, n)
        ) : strText;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // quot :: Int -> Int -> Int
    const quot = (n, m) => Math.floor(n / m);

    // show :: Int -> a -> Indented String
    // show :: a -> String
    const show = (...x) =>
        JSON.stringify.apply(
            null, x.length > 1 ? [x[1], null, x[0]] : x
        );

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // words :: String -> [String]
    const words = s => s.split(/\s+/);

    // zip :: [a] -> [b] -> [(a,b)]
    const zip = (xs, ys) =>
        xs.slice(0, Math.min(xs.length, ys.length))
        .map((x, i) => [x, ys[i]]);

    // zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
    const zip3 = (xs, ys, zs) =>
        xs.slice(0, Math.min(xs.length, ys.length, zs.length))
        .map((x, i) => [x, ys[i], zs[i]]);

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) =>
        Array.from({
            length: Math.min(xs.length, ys.length)
        }, (_, i) => f(xs[i], ys[i]));

    // TRADITIONAL STRINGS ---------------------------------------------------
    // ats :: Array Int (String, String)
    const ats = zip(
        chars("甲乙丙丁戊己庚辛壬癸"), // 天干 tiangan – 10 heavenly stems
        words("jiă yĭ bĭng dīng wù jĭ gēng xīn rén gŭi")
    );

    // ads :: Array Int (String, String)
    const ads = zip(
        chars("子丑寅卯辰巳午未申酉戌亥"), // 地支 dizhi – 12 terrestrial branches
        words("zĭ chŏu yín măo chén sì wŭ wèi shēn yŏu xū hài")
    );

    // aws :: Array Int (String, String, String)
    const aws = zip3(
        chars("木火土金水"), // 五行 wuxing – 5 elements
        words("mù huǒ tǔ jīn shuǐ"),
        words("wood fire earth metal water")
    );

    // axs :: Array Int (String, String, String)
    const axs = zip3(
        chars("鼠牛虎兔龍蛇馬羊猴鸡狗豬"), // 十二生肖 shengxiao – 12 symbolic animals
        words("shǔ niú hǔ tù lóng shé mǎ yáng hóu jī gǒu zhū"),
        words("rat ox tiger rabbit dragon snake horse goat monkey rooster dog pig")
    );

    // ays :: Array Int (String, String)
    const ays = zip(
        chars("阳阴"), // 阴阳 yinyang
        words("yáng yīn")
    );

    // TRADITIONAL CYCLES ----------------------------------------------------
    const zodiac = y => {
        const
            iYear = y - 4,
            iStem = iYear % 10,
            iBranch = iYear % 12,
            [hStem, pStem] = ats[iStem],
            [hBranch, pBranch] = ads[iBranch],
            [hElem, pElem, eElem] = aws[quot(iStem, 2)],
            [hAnimal, pAnimal, eAnimal] = axs[iBranch],
            [hYinyang, pYinyang] = ays[iYear % 2];
        return [
            [show(y), hStem + hBranch, hElem, hAnimal, hYinyang],
            ['', pStem + pBranch, pElem, pAnimal, pYinyang],
            ['', show((iYear % 60) + 1) + '/60', eElem, eAnimal, '']
        ];
    };

    // FORMATTING ------------------------------------------------------------
    // fieldWidths :: [[Int]]
    const fieldWidths = [
        [6, 10, 7, 8, 3],
        [6, 11, 8, 8, 4],
        [6, 11, 8, 8, 4]
    ];

    // showYear :: Int -> String
    const showYear = y =>
        unlines(map(
            row => concat(map(([n, s]) => justifyLeft(n, ' ', s), row)),
            zipWith(zip, fieldWidths, zodiac(y))
        ));

    // TEST OUTPUT -----------------------------------------------------------
    return intercalate(
        '\n\n',
        map(
            showYear, [1935, 1938, 1968, 1972, 1976, 1984, new Date()
                .getFullYear()
            ]
        )
    );
})();
```

{{Out}}

```txt
1935  乙亥        木      豬       阴  
      yĭhài      mù      zhū     yīn 
      12/60      wood    pig         

1938  戊寅        土      虎       阳  
      wùyín      tǔ      hǔ      yáng
      15/60      earth   tiger       

1968  戊申        土      猴       阳  
      wùshēn     tǔ      hóu     yáng
      45/60      earth   monkey      

1972  壬子        水      鼠       阳  
      rénzĭ      shuǐ    shǔ     yáng
      49/60      water   rat         

1976  丙辰        火      龍       阳  
      bĭngchén   huǒ     lóng    yáng
      53/60      fire    dragon      

1984  甲子        木      鼠       阳  
      jiăzĭ      mù      shǔ     yáng
      1/60       wood    rat         

2017  丁酉        火      鸡       阴  
      dīngyŏu    huǒ     jī      yīn 
      34/60      fire    rooster   
```



## Julia

{{works with|Julia|0.6}}
{{trans|Python}}


```julia
function chinese(year::Int)
    pinyin = Dict(
        "甲" => "jiă",
        "乙" => "yĭ",
        "丙" => "bĭng",
        "丁" => "dīng",
        "戊" => "wù",
        "己" => "jĭ",
        "庚" => "gēng",
        "辛" => "xīn",
        "壬" => "rén",
        "癸" => "gŭi",
        "子" => "zĭ",
        "丑" => "chŏu",
        "寅" => "yín",
        "卯" => "măo",
        "辰" => "chén",
        "巳" => "sì",
        "午" => "wŭ",
        "未" => "wèi",
        "申" => "shēn",
        "酉" => "yŏu",
        "戌" => "xū",
        "亥" => "hài",
    )
    elements    = ["Wood", "Fire", "Earth", "Metal", "Water"]
    animals     = ["Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake",
                   "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig"]
    celestial   = ["甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"]
    terrestrial = ["子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"]
    aspects     = ["yang", "yin"]
    base = 4

    cycleyear = year - base

    stemnumber = cycleyear % 10 + 1
    stemhan    = celestial[stemnumber]
    stempinyin = pinyin[stemhan]

    elementnumber = div(stemnumber, 2) + 1
    element       = elements[elementnumber]

    branchnumber = cycleyear % 12 + 1
    branchhan    = terrestrial[branchnumber]
    branchpinyin = pinyin[branchhan]
    animal       = animals[branchnumber]

    aspectnumber = cycleyear % 2 + 1
    aspect       = aspects[aspectnumber]

    index = cycleyear % 60 + 1

    return "$year: $stemhan$branchhan ($stempinyin-$branchpinyin, $element $animal; $aspect - year $index of the cycle)"
end

curryr = Dates.year(now())
yrs = [1935, 1938, 1968, 1972, 1976, curryr]
foreach(println, map(chinese, yrs))
```


{{out}}

```txt
1935: 乙亥 (yĭ-hài, Fire Pig; yin - year 12 of the cycle)
1938: 戊寅 (wù-yín, Earth Tiger; yang - year 15 of the cycle)
1968: 戊申 (wù-shēn, Earth Monkey; yang - year 45 of the cycle)
1972: 壬子 (rén-zĭ, Water Rat; yang - year 49 of the cycle)
1976: 丙辰 (bĭng-chén, Fire Dragon; yang - year 53 of the cycle)
2018: 戊戌 (wù-xū, Earth Dog; yang - year 35 of the cycle)
```



## Kotlin


```scala
// version 1.1.2

class ChineseZodiac(val year: Int) {
    val stem   : Char
    val branch : Char
    val sName  : String
    val bName  : String
    val element: String
    val animal : String
    val aspect : String
    val cycle  : Int
               
    private companion object {
        val animals  = listOf("Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake",
                              "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig")
        val aspects  = listOf("Yang","Yin")
        val elements = listOf("Wood", "Fire", "Earth", "Metal", "Water")
        val stems    = listOf('甲', '乙', '丙', '丁', '戊', '己', '庚', '辛', '壬', '癸')
        val branches = listOf('子', '丑', '寅', '卯', '辰', '巳', '午', '未', '申', '酉', '戌', '亥')
        val sNames   = listOf("jiă", "yĭ", "bĭng", "dīng", "wù", "jĭ", "gēng", "xīn", "rén", "gŭi")
        val bNames   = listOf("zĭ", "chŏu", "yín", "măo", "chén", "sì", "wŭ", "wèi", "shēn", "yŏu",  "xū", "hài")
        val fmt      = "%d    %c%c   %-9s  %-7s  %-7s   %-6s %02d/60"
    } 

    init {
        val y = year - 4
        val s = y % 10
        val b = y % 12
        stem    = stems[s]
        branch  = branches[b]
        sName   = sNames[s]
        bName   = bNames[b]
        element = elements[s / 2]
        animal  = animals[b]
        aspect  = aspects[s % 2]
        cycle   = y % 60 + 1 
    }

    override fun toString() = 
        fmt.format(year, stem, branch, sName + "-" + bName, element, animal, aspect, cycle)
}

fun main(args: Array<String>) {
    val years = intArrayOf(1935, 1938, 1968, 1972, 1976, 1984, 2017)
    println("Year  Chinese  Pinyin     Element  Animal   Aspect  Cycle")
    println("----  -------  ---------  -------  -------  ------  -----") 
    for (year in years) println(ChineseZodiac(year))
}
```


{{out}}

```txt

Year  Chinese  Pinyin     Element  Animal   Aspect  Cycle
----  -------  ---------  -------  -------  ------  -----
1935    乙亥   yĭ-hài     Wood     Pig       Yin    12/60
1938    戊寅   wù-yín     Earth    Tiger     Yang   15/60
1968    戊申   wù-shēn    Earth    Monkey    Yang   45/60
1972    壬子   rén-zĭ     Water    Rat       Yang   49/60
1976    丙辰   bĭng-chén  Fire     Dragon    Yang   53/60
1984    甲子   jiă-zĭ     Wood     Rat       Yang   01/60
2017    丁酉   dīng-yŏu   Fire     Rooster   Yin    34/60

```



## Lua

{{trans|C}}

```lua
local ANIMALS = {"Rat","Ox","Tiger","Rabbit","Dragon","Snake","Horse","Goat","Monkey","Rooster","Dog","Pig"}
local ELEMENTS = {"Wood","Fire","Earth","Metal","Water"}

function element(year)
    local idx = math.floor(((year - 4) % 10) / 2)
    return ELEMENTS[idx + 1]
end

function animal(year)
    local idx = (year - 4) % 12
    return ANIMALS[idx + 1]
end

function yy(year)
    if year % 2 == 0 then
        return "yang"
    else
        return "yin"
    end
end

function zodiac(year)
    local e = element(year)
    local a = animal(year)
    local y = yy(year)
    print(year.." is the year of the "..e.." "..a.." ("..y..")")
end

zodiac(1935)
zodiac(1938)
zodiac(1968)
zodiac(1972)
zodiac(1976)
zodiac(2017)
```

{{out}}

```txt
1935 is the year of the Wood Pig (yin)
1938 is the year of the Earth Tiger (yang)
1968 is the year of the Earth Monkey (yang)
1972 is the year of the Water Rat (yang)
1976 is the year of the Fire Dragon (yang)
2017 is the year of the Fire Rooster (yin)
```



## Maple


```Maple

zodiac:=proc(year::integer)
	local year60,yinyang,animal,element;
	year60:= (year-3) mod 60;								 
	yinyang:=["Yin","Yang"];
	animal:=["Pig","Rat","Ox","Tiger","Rabbit","Dragon","Snake","Horse","Goat","Monkey","Rooster","Dog"];
	element:=["Water","Wood","Wood","Fire","Fire","Earth","Earth","Metal","Metal","Water"];										 
	return sprintf("%a",cat(year," is the year of the ",element[(year60 mod 10)+1]," ",animal[(year60 mod 12)+1]," (",yinyang[(year60 mod 2)+1],")"));
end proc:

```


=={{header|Modula-2}}==
{{trans|C++}}

```modula2
MODULE ChineseZodiac;
FROM FormatString IMPORT FormatString;
FROM Terminal IMPORT WriteString,ReadChar;

TYPE Str = ARRAY[0..7] OF CHAR;

TYPE AA = ARRAY[0..11] OF Str;
CONST ANIMALS = AA{"Rat","Ox","Tiger","Rabbit","Dragon","Snake","Horse","Goat","Monkey","Rooster","Dog","Pig"};

TYPE EA = ARRAY[0..4] OF Str;
CONST ELEMENTS = EA{"Wood","Fire","Earth","Metal","Water"};

PROCEDURE element(year : INTEGER) : Str;
VAR idx : CARDINAL;
BEGIN
    idx := ((year - 4) MOD 10) / 2;
    RETURN ELEMENTS[idx];
END element;

PROCEDURE animal(year : INTEGER) : Str;
VAR idx : CARDINAL;
BEGIN
    idx := (year - 4) MOD 12;
    RETURN ANIMALS[idx];
END animal;

PROCEDURE yy(year : INTEGER) : Str;
BEGIN
    IF year MOD 2 = 0 THEN
        RETURN "yang"
    ELSE
        RETURN "yin"
    END
END yy;

PROCEDURE print(year : INTEGER);
VAR buf : ARRAY[0..63] OF CHAR;
BEGIN
    FormatString("%i is the year of the %s %s (%s)\n", buf, year, element(year), animal(year), yy(year));
    WriteString(buf);
END print;

(* main *)
BEGIN
    print(1935);
    print(1938);
    print(1968);
    print(1972);
    print(1976);
    print(2017);

    ReadChar
END ChineseZodiac.
```



## Perl


```perl
sub zodiac {
  my $year = shift;
  my @animals = qw/Rat Ox Tiger Rabbit Dragon Snake Horse Goat Monkey Rooster Dog Pig/;
  my @elements = qw/Wood Fire Earth Metal Water/;
  my @terrestrial_han = qw/子 丑 寅 卯 辰 巳 午 未 申 酉 戌 亥/;
  my @terrestrial_pinyin = qw/zĭ chŏu yín măo chén sì wŭ wèi shēn yŏu xū hài/;
  my @celestial_han = qw/甲 乙 丙 丁 戊 己 庚 辛 壬 癸/;
  my @celestial_pinyin = qw/jiă yĭ bĭng dīng wù jĭ gēng xīn rén gŭi/;
  my @aspect = qw/yang yin/;

  my $cycle_year = ($year-4) % 60;
  my($i2, $i10, $i12) = ($cycle_year % 2, $cycle_year % 10, $cycle_year % 12);

  ($year,
   $celestial_han[$i10], $terrestrial_han[$i12],
   $celestial_pinyin[$i10], $terrestrial_pinyin[$i12],
   $elements[$i10 >> 1], $animals[$i12], $aspect[$i2], $cycle_year+1);
}

printf("%4d: %s%s (%s-%s) %s %s; %s - year %d of the cycle\n", zodiac($_))
  for (1935, 1938, 1968, 1972, 1976, 2017);
```

{{out}}

```txt

1935: 乙亥 (yĭ-hài) Wood Pig; yin - year 12 of the cycle
1938: 戊寅 (wù-yín) Earth Tiger; yang - year 15 of the cycle
1968: 戊申 (wù-shēn) Earth Monkey; yang - year 45 of the cycle
1972: 壬子 (rén-zĭ) Water Rat; yang - year 49 of the cycle
1976: 丙辰 (bĭng-chén) Fire Dragon; yang - year 53 of the cycle
2017: 丁酉 (dīng-yŏu) Fire Rooster; yin - year 34 of the cycle

```



## Perl 6

{{works with|Rakudo|2017.01}}
{{trans|Ruby}}


```perl6
sub Chinese-zodiac ( Int $year ) {
    my @heaven  = <甲 jiă 乙 yĭ 丙 bĭng 丁 dīng 戊 wù 己 jĭ 庚 gēng 辛 xīn 壬 rén 癸 gŭi>.pairup;
    my @earth   = <子 zĭ 丑 chŏu 寅 yín 卯 măo 辰 chén 巳 sì 午 wŭ 未 wèi 申 shēn 酉 yŏu 戌 xū 亥 hài>.pairup;
    my @animal  = <Rat Ox Tiger Rabbit Dragon Snake Horse Goat Monkey Rooster Dog Pig>;
    my @element = <Wood Fire Earth Metal Water>;
    my @aspect  = <yang yin>;

    my $cycle_year = ($year - 4) % 60;
    my $i2         = $cycle_year % 2;
    my $i10        = $cycle_year % 10;
    my $i12        = $cycle_year % 12;

    %(
        'Han'     => @heaven[$i10].key ~ @earth[$i12].key,
        'pinyin'  => @heaven[$i10].value ~ @earth[$i12].value,
        'heaven'  => @heaven[$i10],
        'earth'   => @earth[$i12],
        'element' => @element[$i10 div 2],
        'animal'  => @animal[$i12],
        'aspect'  => @aspect[$i2],
        'cycle'   => $cycle_year + 1
    )
}

# TESTING
printf "%d: %s (%s, %s %s; %s - year %d in the cycle)\n",
    $_, .&Chinese-zodiac<Han pinyin element animal aspect cycle>
    for 1935, 1938, 1968, 1972, 1976, 1984, Date.today.year;
```


{{out}}

```txt
1935: 乙亥 (yĭhài, Wood Pig; yin - year 12 in the cycle)
1938: 戊寅 (wùyín, Earth Tiger; yang - year 15 in the cycle)
1968: 戊申 (wùshēn, Earth Monkey; yang - year 45 in the cycle)
1972: 壬子 (rénzĭ, Water Rat; yang - year 49 in the cycle)
1976: 丙辰 (bĭngchén, Fire Dragon; yang - year 53 in the cycle)
1984: 甲子 (jiăzĭ, Wood Rat; yang - year 1 in the cycle)
2017: 丁酉 (dīngyŏu, Fire Rooster; yin - year 34 in the cycle)
```



## Phix


```Phix
constant animals = {"Rat","Ox","Tiger","Rabbit","Dragon","Snake","Horse","Goat","Monkey","Rooster","Dog","Pig"},
         elements = {"Wood","Fire","Earth","Metal","Water"},
         yinyang = {"yang","yin"},
         years = {1935,1938,1968,1972,1976,2018}
 
for i=1 to length(years) do
    integer year = years[i],
            cycle = mod(year-4,60)
    string element = elements[floor(mod(cycle,10)/2)+1],
           animal = animals[mod(cycle,12)+1],
           yy = yinyang[mod(cycle,2)+1]        
    printf(1,"%d: %s %s; %s, year %d of the cycle.\n",
             {year,element,animal,yy,cycle+1})
end for
```

{{out}}

```txt

1935: Wood Pig; yin, year 12 of the cycle.
1938: Earth Tiger; yang, year 15 of the cycle.
1968: Earth Monkey; yang, year 45 of the cycle.
1972: Water Rat; yang, year 49 of the cycle.
1976: Fire Dragon; yang, year 53 of the cycle.
2018: Earth Dog; yang, year 35 of the cycle.

```



## PowerShell

{{trans|Ruby}}

```PowerShell

function Get-ChineseZodiac
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$false,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [ValidateRange(1,9999)]
        [int]
        $Year = (Get-Date).Year
    )

    Begin
    {
        $pinyin = @{
            '甲' = 'jiă'
            '乙' = 'yĭ'
            '丙' = 'bĭng'
            '丁' = 'dīng'
            '戊' = 'wù'
            '己' = 'jĭ'
            '庚' = 'gēng'
            '辛' = 'xīn'
            '壬' = 'rén'
            '癸' = 'gŭi'
            '子' = 'zĭ'
            '丑' = 'chŏu'
            '寅' = 'yín'
            '卯' = 'măo'
            '辰' = 'chén'
            '巳' = 'sì'
            '午' = 'wŭ'
            '未' = 'wèi'
            '申' = 'shēn'
            '酉' = 'yŏu'
            '戌' = 'xū'
            '亥' = 'hài'
        }

        $celestial   = '甲', '乙', '丙', '丁', '戊', '己', '庚', '辛', '壬', '癸'
        $terrestrial = '子', '丑', '寅', '卯', '辰', '巳', '午', '未', '申', '酉', '戌', '亥'
        $animals     = 'Rat', 'Ox', 'Tiger', 'Rabbit', 'Dragon', 'Snake', 'Horse', 'Goat', 'Monkey', 'Rooster', 'Dog', 'Pig'
        $elements    = 'Wood', 'Fire', 'Earth', 'Metal', 'Water'
        $aspects     = 'yang', 'yin'

        $base = 4
    }
    Process
    {
        foreach ($ce_year in $Year)
        {
            $cycle_year     = $ce_year - $base

            $stem_number    = $cycle_year % 10
            $stem_han       = $celestial[$stem_number]
            $stem_pinyin    = $pinyin[$stem_han]

            $element_number = [Math]::Floor($stem_number / 2)
            $element        = $elements[$element_number]

            $branch_number  = $cycle_year % 12
            $branch_han     = $terrestrial[$branch_number]
            $branch_pinyin  = $pinyin[$branch_han]
            $animal         = $animals[$branch_number]

            $aspect_number  = $cycle_year % 2
            $aspect         = $aspects[$aspect_number]

            $index          = $cycle_year % 60 + 1

            [PSCustomObject]@{
                Year        = $Year
                Element     = $element
                Animal      = $animal
                Aspect      = $aspect
                YearOfCycle = $index
                ASCII       = "$stem_pinyin-$branch_pinyin"
                Chinese     = "$stem_han$branch_han"
            }
        }
    }
}

```


```PowerShell

1935, 1938, 1968, 1972, 1976 | Get-ChineseZodiac | Format-Table

```

{{Out}}

```txt

Year Element Animal Aspect YearOfCycle ASCII     Chinese
---- ------- ------ ------ ----------- -----     -------
1935 Wood    Pig    yin             12 yĭ-hài    乙亥     
1938 Earth   Tiger  yang            15 wù-yín    戊寅     
1968 Earth   Monkey yang            45 wù-shēn   戊申     
1972 Water   Rat    yang            49 rén-zĭ    壬子     
1976 Fire    Dragon yang            53 bĭng-chén 丙辰     

```

Defaults to the current year:

```PowerShell

Get-ChineseZodiac

```

{{Out}}

```txt

Year        : 2017
Element     : Fire
Animal      : Rooster
Aspect      : yin
YearOfCycle : 34
ASCII       : dīng-yŏu
Chinese     : 丁酉

```

Using the '''Year''' property of a <code>[DateTime]</code> object:

```PowerShell

Get-Date "11/8/2016" | Get-ChineseZodiac

```

{{Out}}

```txt

Year        : 2016
Element     : Fire
Animal      : Monkey
Aspect      : yang
YearOfCycle : 33
ASCII       : bĭng-shēn
Chinese     : 丙申

```

Emulate the Ruby script's output:

```PowerShell

$zodiacs = 1935, 1938, 1968, 1972, 1976 | Get-ChineseZodiac

foreach ($zodiac in $zodiacs)
{
    "{0}: {1} ({2}, {3} {4}; {5} - year {6} of the cycle)" -f $zodiac.Year,
                                                              $zodiac.Chinese,
                                                              $zodiac.ASCII,
                                                              $zodiac.Element,
                                                              $zodiac.Animal,
                                                              $zodiac.Aspect,
                                                              $zodiac.YearOfCycle
}

```

{{Out}}

```txt

1935: 乙亥 (yĭ-hài, Wood Pig; yin - year 12 of the cycle)
1938: 戊寅 (wù-yín, Earth Tiger; yang - year 15 of the cycle)
1968: 戊申 (wù-shēn, Earth Monkey; yang - year 45 of the cycle)
1972: 壬子 (rén-zĭ, Water Rat; yang - year 49 of the cycle)
1976: 丙辰 (bĭng-chén, Fire Dragon; yang - year 53 of the cycle)

```



## PureBasic


```pureBasic
EnableExplicit
#BASE=4
#SPC=Chr(32)

Procedure.s ChineseZodiac(n.i)
  Define cycle_year.i=n-#BASE,
         stem_number.i    = cycle_year%10+1,
         element_number.i = Round(stem_number/2,#PB_Round_Nearest),
         branch_number.i  = cycle_year%12+1,
         aspect_number.i  = cycle_year%2+1,
         index.i          = cycle_year%60+1,
         celestial$       = Chr(PeekU(?Celestial_stem+SizeOf(Character)*(stem_number-1))),
         c_pinyin$        = StringField(PeekS(?Stem),stem_number,"\"),
         element$         = StringField(PeekS(?Element),element_number,"\"),
         branch_han$      = Chr(PeekU(?Terrestrial_branch+SizeOf(Character)*(branch_number-1))),
         b_pinyin$        = StringField(PeekS(?Branch),branch_number,"\"),
         animal$          = StringField(PeekS(?Zodiac_animal),branch_number,"\"),
         aspect$          = StringField(PeekS(?Aspect),aspect_number,"\"),
         YearOfCycle$     = Str(index)  
  ProcedureReturn Str(n)+#SPC+
                         LSet(element$,7,#SPC)+#SPC+
                         LSet(animal$,7,#SPC)+#SPC+
                         LSet(aspect$,6,#SPC)+#SPC+
                         RSet(YearOfCycle$,11)+#SPC+
                         LSet(c_pinyin$+"-"+b_pinyin$,9,#SPC)+#SPC+
                         celestial$+branch_han$
EndProcedure

LoadFont(0,"Consolas",12)
If OpenWindow(0,#PB_Ignore,#PB_Ignore,600,400,"Chinese Zodiac",#PB_Window_ScreenCentered|#PB_Window_SystemMenu)
  EditorGadget(0, 8, 8, 600-16, 400-16) : SetGadgetFont(0,FontID(0))
  Define header$="Year Element Animal  Aspect YearOfCycle ASCII     Chinese"
  AddGadgetItem(0,-1,header$)
  AddGadgetItem(0,-1,ChineseZodiac(1935))
  AddGadgetItem(0,-1,ChineseZodiac(1938))
  AddGadgetItem(0,-1,ChineseZodiac(1968))
  AddGadgetItem(0,-1,ChineseZodiac(1972))  
  AddGadgetItem(0,-1,ChineseZodiac(1976))
  AddGadgetItem(0,-1,ChineseZodiac(1984))
  AddGadgetItem(0,-1,ChineseZodiac(Year(Date()))) 
  Repeat : Until WaitWindowEvent() = #PB_Event_CloseWindow
EndIf

DataSection
  Celestial_stem: : Data.u $7532, $4E59, $4E19, $4E01, $620A, $5DF1, $5E9A, $8F9B, $58EC, $7678  
  Terrestrial_branch: : Data.u $5B50, $4E11, $5BC5, $536F, $8FB0, $5DF3, $5348, $672A, $7533, $9149, $620C, $4EA5  
  Zodiac_animal: : Data.s "Rat\Ox\Tiger\Rabbit\Dragon\Snake\Horse\Goat\Monkey\Rooster\Dog\Pig"  
  Element: : Data.s "Wood\Fire\Earth\Metal\Water"  
  Aspect: : Data.s "yang\yin"  
  Stem: : Data.s "jiă\yĭ\bĭng\dīng\wù\jĭ\gēng\xīn\rén\gŭi"  
  Branch: : Data.s "zĭ\chŏu\yín\măo\chén\sì\wŭ\wèi\shēn\yŏu\xū\hài"  
EndDataSection
```

{{Out}}

```txt
Year Element Animal  Aspect YearOfCycle ASCII     Chinese
1935 Wood    Pig     yin             12 yĭ-hài    乙亥
1938 Earth   Tiger   yang            15 wù-yín    戊寅
1968 Earth   Monkey  yang            45 wù-shēn   戊申
1972 Water   Rat     yang            49 rén-zĭ    壬子
1976 Fire    Dragon  yang            53 bĭng-chén 丙辰
1984 Wood    Rat     yang             1 jiă-zĭ    甲子
2017 Fire    Rooster yin             34 dīng-yŏu  丁酉
```



## Python

{{trans|Ruby}}

```Python

# coding: utf-8

from __future__ import print_function
from datetime import datetime

pinyin = {
  '甲': 'jiă',
  '乙': 'yĭ',
  '丙': 'bĭng',
  '丁': 'dīng',
  '戊': 'wù',
  '己': 'jĭ',
  '庚': 'gēng',
  '辛': 'xīn',
  '壬': 'rén',
  '癸': 'gŭi',

  '子': 'zĭ',
  '丑': 'chŏu',
  '寅': 'yín',
  '卯': 'măo',
  '辰': 'chén',
  '巳': 'sì',
  '午': 'wŭ',
  '未': 'wèi',
  '申': 'shēn',
  '酉': 'yŏu',
  '戌': 'xū',
  '亥': 'hài'
}

animals = ['Rat', 'Ox', 'Tiger', 'Rabbit', 'Dragon', 'Snake',
           'Horse', 'Goat', 'Monkey', 'Rooster', 'Dog', 'Pig']
elements = ['Wood', 'Fire', 'Earth', 'Metal', 'Water']

celestial = ['甲', '乙', '丙', '丁', '戊', '己', '庚', '辛', '壬', '癸']
terrestrial = ['子', '丑', '寅', '卯', '辰', '巳', '午', '未', '申', '酉', '戌', '亥']
aspects = ['yang', 'yin']


def calculate(year):
    BASE = 4
    year = int(year)
    cycle_year = year - BASE
    stem_number = cycle_year % 10
    stem_han = celestial[stem_number]
    stem_pinyin = pinyin[stem_han]
    element_number = stem_number // 2
    element = elements[element_number]
    branch_number = cycle_year % 12
    branch_han = terrestrial[branch_number]
    branch_pinyin = pinyin[branch_han]
    animal = animals[branch_number]
    aspect_number = cycle_year % 2
    aspect = aspects[aspect_number]
    index = cycle_year % 60 + 1
    print("{}: {}{} ({}-{}, {} {}; {} - year {} of the cycle)"
          .format(year, stem_han, branch_han,
                  stem_pinyin, branch_pinyin, element, animal, aspect, index))


current_year = datetime.now().year
years = [1935, 1938, 1968, 1972, 1976, current_year]
for year in years:
    calculate(year)
```



Or, segmenting tokens just in time, and writing out wiki tables:
{{Works with|Python|3.7}}

```python
'''Chinese zodiac'''

from functools import (reduce)
from datetime import datetime


# TRADITIONAL STRINGS -------------------------------------

# zodiacNames :: Dict
def zodiacNames():
    '''天干 tiangan – 10 heavenly stems
       地支 dizhi – 12 terrestrial branches
       五行 wuxing – 5 elements
       生肖 shengxiao – 12 symbolic animals
       阴阳 yinyang - dark and light
    '''
    return dict(
        zip(
            ['tian', 'di', 'wu', 'sx', 'yy'],
            map(
                lambda tpl: list(
                    zip(* [tpl[0]] + list(
                        map(
                            lambda x: x.split(),
                            tpl[1:])
                    ))
                ),
                [
                    # 天干 tiangan – 10 heavenly stems
                    ('甲乙丙丁戊己庚辛壬癸',
                     'jiă yĭ bĭng dīng wù jĭ gēng xīn rén gŭi'),

                    # 地支 dizhi – 12 terrestrial branches
                    ('子丑寅卯辰巳午未申酉戌亥',
                     'zĭ chŏu yín măo chén sì wŭ wèi shēn yŏu xū hài'),

                    # 五行 wuxing – 5 elements
                    ('木火土金水',
                     'mù huǒ tǔ jīn shuǐ',
                     'wood fire earth metal water'),

                    # 十二生肖 shengxiao – 12 symbolic animals
                    ('鼠牛虎兔龍蛇馬羊猴鸡狗豬',
                     'shǔ niú hǔ tù lóng shé mǎ yáng hóu jī gǒu zhū',
                     'rat ox tiger rabbit dragon snake horse goat ' +
                     'monkey rooster dog pig'
                     ),

                    # 阴阳 yinyang
                    ('阳阴', 'yáng yīn')
                ]
            )))


# zodiacYear :: Dict -> [[String]]
def zodiacYear(dct):
    '''A string of strings containing the
       Chinese zodiac tokens for a given year.
    '''
    def tokens(y):
        iYear = y - 4
        iStem = iYear % 10
        iBranch = iYear % 12
        (hStem, pStem) = dct['tian'][iStem]
        (hBranch, pBranch) = dct['di'][iBranch]
        yy = iYear % 2
        return [
            [str(y), '', ''],
            [
                hStem + hBranch,
                pStem + pBranch,
                str((iYear % 60) + 1) + '/60'
            ],
            list(dct['wu'][iStem // 2]),
            list(dct['sx'][iBranch]),
            list(dct['yy'][int(yy)]) + ['dark' if yy else 'light']
        ]
    return lambda year: tokens(year)


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Writing out wiki tables displaying Chinese zodiac
       details for a given list of years.
    '''
    print('\n'.join(
        list(map(
            zodiacTable(zodiacNames()),
            [
                1935, 1938, 1949,
                1968, 1972, 1976,
                datetime.now().year
            ]
        ))
    ))


# WIKI TABLES  --------------------------------------------

# zodiacTable :: Dict -> Int -> String
def zodiacTable(tokens):
    '''A wiki table displaying Chinese zodiac
       details for a a given year.
    '''
    return lambda y: wikiTable({
        'class': 'wikitable',
        'colwidth': '70px'
    })(transpose(zodiacYear(tokens)(y)))


# wikiTable :: Dict -> [[a]] -> String
def wikiTable(opts):
    '''List of lists rendered as a wiki table string.'''
    def colWidth():
        return 'width:' + opts['colwidth'] + '; ' if (
            'colwidth' in opts
        ) else ''

    def cellStyle():
        return opts['cell'] if 'cell' in opts else ''

    return lambda rows: '{| ' + reduce(
        lambda a, k: (
            a + k + '="' + opts[k] + '" ' if k in opts else a
        ),
        ['class', 'style'],
        ''
    ) + '\n' + '\n|-\n'.join(
        '\n'.join(
            ('|' if (0 != i and ('cell' not in opts)) else (
                '|style="' + colWidth() + cellStyle() + '"|'
            )) + (
                str(x) or ' '
            ) for x in row
        ) for i, row in enumerate(rows)
    ) + '\n|}\n\n'


# GENERIC -------------------------------------------------

# transpose :: Matrix a -> Matrix a
def transpose(m):
    '''The rows and columns of the argument transposed.
       (The matrix containers and rows can be lists or tuples).'''
    if m:
        inner = type(m[0])
        z = zip(*m)
        return (type(m))(
            map(inner, z) if tuple != inner else z
        )
    else:
        return m


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}
{| class="wikitable" 
|style="width:70px; "|1935
|style="width:70px; "|乙亥
|style="width:70px; "|木
|style="width:70px; "|豬
|style="width:70px; "|阴
|-
| 
|yĭhài
|mù
|zhū
|yīn
|-
| 
|12/60
|wood
|pig
|dark
|}


{| class="wikitable" 
|style="width:70px; "|1938
|style="width:70px; "|戊寅
|style="width:70px; "|土
|style="width:70px; "|虎
|style="width:70px; "|阳
|-
| 
|wùyín
|tǔ
|hǔ
|yáng
|-
| 
|15/60
|earth
|tiger
|light
|}


{| class="wikitable" 
|style="width:70px; "|1949
|style="width:70px; "|己丑
|style="width:70px; "|土
|style="width:70px; "|牛
|style="width:70px; "|阴
|-
| 
|jĭchŏu
|tǔ
|niú
|yīn
|-
| 
|26/60
|earth
|ox
|dark
|}


{| class="wikitable" 
|style="width:70px; "|1968
|style="width:70px; "|戊申
|style="width:70px; "|土
|style="width:70px; "|猴
|style="width:70px; "|阳
|-
| 
|wùshēn
|tǔ
|hóu
|yáng
|-
| 
|45/60
|earth
|monkey
|light
|}


{| class="wikitable" 
|style="width:70px; "|1972
|style="width:70px; "|壬子
|style="width:70px; "|水
|style="width:70px; "|鼠
|style="width:70px; "|阳
|-
| 
|rénzĭ
|shuǐ
|shǔ
|yáng
|-
| 
|49/60
|water
|rat
|light
|}


{| class="wikitable" 
|style="width:70px; "|1976
|style="width:70px; "|丙辰
|style="width:70px; "|火
|style="width:70px; "|龍
|style="width:70px; "|阳
|-
| 
|bĭngchén
|huǒ
|lóng
|yáng
|-
| 
|53/60
|fire
|dragon
|light
|}


{| class="wikitable" 
|style="width:70px; "|2019
|style="width:70px; "|己亥
|style="width:70px; "|土
|style="width:70px; "|豬
|style="width:70px; "|阴
|-
| 
|jĭhài
|tǔ
|zhū
|yīn
|-
| 
|36/60
|earth
|pig
|dark
|}


## Racket

{{trans|Common Lisp}}


```racket
#lang racket

(require racket/date)

; Any CE Year that was the first of a 60-year cycle
(define base-year 1984)
 
(define celestial-stems '("甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"))
 
(define terrestrial-branches '("子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"))
 
(define zodiac-animals
  '("Rat" "Ox" "Tiger" "Rabbit" "Dragon" "Snake" "Horse" "Goat" "Monkey" "Rooster" "Dog" "Pig"))
 
(define elements '("Wood" "Fire" "Earth" "Metal" "Water"))
 
(define aspects '("yang" "yin"))
 
(define pinyin
  (map cons
       (append celestial-stems terrestrial-branches)
       (list "jiă" "yĭ" "bĭng" "dīng" "wù" "jĭ" "gēng" "xīn" "rén" "gŭi"
             "zĭ" "chŏu" "yín" "măo" "chén" "sì" "wŭ" "wèi" "shēn" "yŏu" "xū" "hài")))
 
(define (this-year) (date-year (current-date)))

(define (pinyin-for han) (cdr (assoc han pinyin)))

(define (han/pinyin-nth n hans) (let ((han (list-ref hans n))) (values han (pinyin-for han))))

(define (chinese-zodiac ce-year)
  (let* ((cycle-year (- ce-year base-year))
         (stem-number    (modulo cycle-year (length celestial-stems)))
         (element-number (quotient stem-number 2))
         (aspect-number  (modulo cycle-year (length aspects)))
         (branch-number  (modulo cycle-year (length terrestrial-branches)))
         (element        (list-ref elements element-number)) 
         (zodiac-animal  (list-ref zodiac-animals branch-number))
         (aspect         (list-ref aspects aspect-number)))
    (let-values (([stem-han stem-pinyin]     (han/pinyin-nth stem-number celestial-stems))                  
                 ([branch-han branch-pinyin] (han/pinyin-nth branch-number terrestrial-branches)))
      (list ce-year stem-han branch-han stem-pinyin branch-pinyin element zodiac-animal aspect))))

(module+ test
  (for ((ce-year (in-list '(1935 1938 1941 1947 1968 1972 1976))))
  (apply printf "~a: ~a~a (~a-~a, ~a ~a; ~a)~%" (chinese-zodiac ce-year))))
```


{{out}}

```txt
1935: 乙亥 (yĭ-hài, Wood Pig; yin)
1938: 戊寅 (wù-yín, Earth Tiger; yang)
1941: 辛巳 (xīn-sì, Metal Snake; yin)
1947: 丁亥 (dīng-hài, Fire Pig; yin)
1968: 戊申 (wù-shēn, Earth Monkey; yang)
1972: 壬子 (rén-zĭ, Water Rat; yang)
1976: 丙辰 (bĭng-chén, Fire Dragon; yang)
```



## Ruby

This is written as a command-line tool that takes a list of CE year numbers as arguments and outputs their information; if no arguments are supplied, it displays the information for the current year. 


```ruby
# encoding: utf-8
pinyin = {
  '甲' => 'jiă',
  '乙' => 'yĭ',
  '丙' => 'bĭng',
  '丁' => 'dīng',
  '戊' => 'wù',
  '己' => 'jĭ',
  '庚' => 'gēng',
  '辛' => 'xīn',
  '壬' => 'rén',
  '癸' => 'gŭi',

  '子' => 'zĭ',
  '丑' => 'chŏu',
  '寅' => 'yín',
  '卯' => 'măo',
  '辰' => 'chén',
  '巳' => 'sì',
  '午' => 'wŭ',
  '未' => 'wèi',
  '申' => 'shēn',
  '酉' => 'yŏu',
  '戌' => 'xū',
  '亥' => 'hài'
}
celestial     = %w(甲 乙 丙 丁 戊 己 庚 辛 壬 癸)
terrestrial   = %w(子 丑 寅 卯 辰 巳 午 未 申 酉 戌 亥)
animals       = %w(Rat   Ox   Tiger  Rabbit  Dragon Snake
                   Horse Goat Monkey Rooster Dog    Pig)
elements      = %w(Wood Fire Earth Metal Water)
aspects       = %w(yang yin)

BASE = 4

args = if !ARGV.empty?
         ARGV
       else
         [Time.new.year]
       end

args.each do |arg|
  ce_year = Integer(arg)
  print "#{ce_year}: " if ARGV.length > 1
  cycle_year     = ce_year - BASE

  stem_number    = cycle_year % 10
  stem_han       = celestial[stem_number]
  stem_pinyin    = pinyin[stem_han]

  element_number = stem_number / 2
  element        = elements[element_number]

  branch_number  = cycle_year % 12
  branch_han     = terrestrial[branch_number]
  branch_pinyin  = pinyin[branch_han]
  animal         = animals[branch_number]

  aspect_number = cycle_year % 2
  aspect        = aspects[aspect_number]

  index         = cycle_year % 60 + 1

  print stem_han, branch_han
  puts " (#{stem_pinyin}-#{branch_pinyin}, #{element} #{animal}; #{aspect} - year #{index} of the cycle)"
end
```


{{Output}}
Given arguments 1935 1938 1968 1972 1976:

```txt
1935: 乙亥 (yĭ-hài, Wood Pig; yin - year 12 of the cycle)
1938: 戊寅 (wù-yín, Earth Tiger; yang - year 15 of the cycle)
1968: 戊申 (wù-shēn, Earth Monkey; yang - year 45 of the cycle)
1972: 壬子 (rén-zĭ, Water Rat; yang - year 49 of the cycle)
1976: 丙辰 (bĭng-chén, Fire Dragon; yang - year 53 of the cycle)
```


Given no arguments and run during the year 2017:

```txt
丁酉 (dīng-yŏu, Fire Rooster; yin - year 34 of the cycle)
```



## Rust

{{trans|Kotlin}}

```Rust
fn chinese_zodiac(year: usize) -> String {
    static ANIMALS: [&str; 12] = [
        "Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake",
        "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig",
    ];
    static ASPECTS: [&str; 2] = ["Yang", "Yin"];
    static ELEMENTS: [&str; 5] = ["Wood", "Fire", "Earth", "Metal", "Water"];
    static STEMS: [char; 10] = [
        '甲', '乙', '丙', '丁', '戊', '己', '庚', '辛', '壬', '癸',
    ];
    static BRANCHES: [char; 12] = [
        '子', '丑', '寅', '卯', '辰', '巳', '午', '未', '申', '酉', '戌', '亥',
    ];
    static S_NAMES: [&str; 10] = [
        "jiă", "yĭ", "bĭng", "dīng", "wù", "jĭ", "gēng", "xīn", "rén", "gŭi",
    ];
    static B_NAMES: [&str; 12] = [
        "zĭ", "chŏu", "yín", "măo", "chén", "sì",
        "wŭ", "wèi", "shēn", "yŏu", "xū", "hài",
    ];

    let y = year - 4;
    let s = y % 10;
    let b = y % 12;

    let stem = STEMS[s];
    let branch = BRANCHES[b];
    let s_name = S_NAMES[s];
    let b_name = B_NAMES[b];
    let element = ELEMENTS[s / 2];
    let animal = ANIMALS[b];
    let aspect = ASPECTS[s % 2];
    let cycle = y % 60 + 1;

    format!(
        "{}    {}{}    {:9}  {:7}  {:7}  {:6}  {:02}/60",
        year,
        stem,
        branch,
        format!("{}-{}", s_name, b_name),
        element,
        animal,
        aspect,
        cycle
    )
}

fn main() {
    let years = [1935, 1938, 1968, 1972, 1976, 1984, 2017];
    println!("Year  Chinese  Pinyin     Element  Animal   Aspect  Cycle");
    println!("----  -------  ---------  -------  -------  ------  -----");
    for &year in &years {
        println!("{}", chinese_zodiac(year));
    }
}
```

{{out}}

```txt
Year  Chinese  Pinyin     Element  Animal   Aspect  Cycle
----  -------  ---------  -------  -------  ------  -----
1935    乙亥    yĭ-hài     Wood     Pig      Yin     12/60
1938    戊寅    wù-yín     Earth    Tiger    Yang    15/60
1968    戊申    wù-shēn    Earth    Monkey   Yang    45/60
1972    壬子    rén-zĭ     Water    Rat      Yang    49/60
1976    丙辰    bĭng-chén  Fire     Dragon   Yang    53/60
1984    甲子    jiă-zĭ     Wood     Rat      Yang    01/60
2017    丁酉    dīng-yŏu   Fire     Rooster  Yin     34/60
```



## Scala


```Scala
object Zodiac extends App {
  val years = Seq(1935, 1938, 1968, 1972, 1976, 1984, 1985, 2017, 2018)

  private def animals =
    Seq("Rat",
      "Ox",
      "Tiger",
      "Rabbit",
      "Dragon",
      "Snake",
      "Horse",
      "Goat",
      "Monkey",
      "Rooster",
      "Dog",
      "Pig")

  private def animalChars =
    Seq("子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥")

  private def elements = Seq("Wood", "Fire", "Earth", "Metal", "Water")

  private def elementChars =
    Seq(Array("甲", "丙", "戊", "庚", "壬"), Array("乙", "丁", "己", "辛", "癸"))

  private def getYY(year: Int) = if (year % 2 == 0) "yang" else "yin"

  for (year <- years) {
    println(year
      + " is the year of the " + elements(math.floor((year - 4) % 10 / 2).toInt) + " " 
      + animals((year - 4) % 12)
      + " (" + getYY(year) + "). "
      + elementChars(year % 2)(math.floor((year - 4) % 10 / 2).toInt)
      + animalChars((year - 4) % 12))
  }
}
```

{{Out}}See it in running in your browser by [https://scalafiddle.io/sf/O6MUr27/0 ScalaFiddle (JavaScript)] or by [https://scastie.scala-lang.org/KXC0j71ORFaQxNZSCCZ1Aw Scastie (JVM)].


## Seed7

The standard output file of Seed7 accepts only Latin-1 (byte) output.
Therefore the output with the chinese characters is written to
[http://seed7.sourceforge.net/libraries/console.htm#STD_CONSOLE STD_CONSOLE].
The library [http://seed7.sourceforge.net/libraries/console.htm console.s7i] defines STD_CONSOLE.


```seed7
$ include "seed7_05.s7i";
  include "console.s7i";

const array string: animals is [0] ("Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig");
const array string: elements is [0] ("Wood", "Fire", "Earth", "Metal", "Water");
const array string: animalChars is [0] ("子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥");
const array array string: elementChars is [0] ([0] ("甲", "丙", "戊", "庚", "壬"), [0] ("乙", "丁", "己", "辛", "癸"));

const proc: main is func
  local
    var integer: year is 0;
    var integer: eIdx is 0;
    var integer: aIdx is 0;
  begin
    OUT := STD_CONSOLE;
    for year range {1935, 1938, 1968, 1972, 1976, 1984, 1985, 2017} do
      eIdx := (year - 4) rem 10 div 2;
      aIdx := (year - 4) rem 12;
      writeln(year <& " is the year of the " <& elements[eIdx] <& " " <& animals[aIdx] <& " (" <&
              ([0] ("yang", "yin"))[year rem 2] <& "). " <& elementChars[year rem 2][eIdx] <& animalChars[aIdx]);
    end for;
  end func;
```


{{out}}

```txt

1935 is the year of the Wood Pig (yin). 乙亥
1938 is the year of the Earth Tiger (yang). 戊寅
1968 is the year of the Earth Monkey (yang). 戊申
1972 is the year of the Water Rat (yang). 壬子
1976 is the year of the Fire Dragon (yang). 丙辰
1984 is the year of the Wood Rat (yang). 甲子
1985 is the year of the Wood Ox (yin). 乙丑
2017 is the year of the Fire Rooster (yin). 丁酉

```



## Sidef

{{trans|Perl}}

```ruby
func zodiac(year) {
  var animals = %w(Rat Ox Tiger Rabbit Dragon Snake Horse Goat Monkey Rooster Dog Pig)
  var elements = %w(Wood Fire Earth Metal Water)
  var terrestrial_han = %w(子 丑 寅 卯 辰 巳 午 未 申 酉 戌 亥)
  var terrestrial_pinyin = %w(zĭ chŏu yín măo chén sì wŭ wèi shēn yŏu xū hài)
  var celestial_han = %w(甲 乙 丙 丁 戊 己 庚 辛 壬 癸)
  var celestial_pinyin = %w(jiă yĭ bĭng dīng wù jĭ gēng xīn rén gŭi)
  var aspect = %w(yang yin)

  var cycle_year = ((year-4) % 60)
  var (i2, i10, i12) = (cycle_year%2, cycle_year%10, cycle_year%12)

  (year,
   celestial_han[i10],    terrestrial_han[i12],
   celestial_pinyin[i10], terrestrial_pinyin[i12],
   elements[i10 >> 1], animals[i12], aspect[i2], cycle_year+1)
}

[1935, 1938, 1968, 1972, 1976, 2017].each { |year|
    printf("%4d: %s%s (%s-%s) %s %s; %s - year %d of the cycle\n", zodiac(year))
}
```

{{out}}

```txt

1935: 乙亥 (yĭ-hài) Wood Pig; yin - year 12 of the cycle
1938: 戊寅 (wù-yín) Earth Tiger; yang - year 15 of the cycle
1968: 戊申 (wù-shēn) Earth Monkey; yang - year 45 of the cycle
1972: 壬子 (rén-zĭ) Water Rat; yang - year 49 of the cycle
1976: 丙辰 (bĭng-chén) Fire Dragon; yang - year 53 of the cycle
2017: 丁酉 (dīng-yŏu) Fire Rooster; yin - year 34 of the cycle

```



## tbas


```qbasic

	DATA "甲","乙","丙","丁","戊","己","庚","辛","壬","癸"
	DECLARE celestial$(10)
	MAT READ celestial$
	
	DATA "子","丑","寅","卯","辰","巳","午","未","申","酉","戌","亥"
	DECLARE terrestrial$(12)
	MAT READ terrestrial$
	
	DATA "Rat","Ox","Tiger","Rabbit","Dragon","Snake","Horse","Goat","Monkey","Rooster","Dog","Pig"
	DECLARE animals$(12)
	MAT READ animals$

        DATA "Wood","Fire","Earth","Metal","Water"
	DECLARE elements$(5)
	MAT READ elements$

	DATA "yang","yin"
	DECLARE aspects$(2)
	MAT READ aspects$
	
	DATA "jiă","yĭ","bĭng","dīng","wù","jĭ","gēng","xīn","rén","gŭi"
	DATA "zĭ","chŏu","yín","măo","chén","sì","wŭ","wèi","shēn","yŏu","xū","hài"
	DECLARE celestialpinyin$(UBOUND(celestial$(),1))
	DECLARE terrestrialpinyin$(UBOUND(terrestrial$(),1))
	MAT READ celestialpinyin$
	MAT READ terrestrialpinyin$
	
	DATA 1935,1938,1931,1961,1963,1991,1993,1996,2001
	DECLARE years(9)
	MAT READ years
	
	DECLARE _base = 4	 
	DECLARE _year 
	DECLARE cycleyear 
	DECLARE stemnumber 
	DECLARE stemhan$    
	DECLARE stempinyin$ 
	DECLARE elementnumber 
	DECLARE element$       
	DECLARE branchnumber 
	DECLARE branchhan$    
	DECLARE branchpinyin$ 
	DECLARE animal$       
	DECLARE aspectnumber 
	DECLARE aspect$       
	DECLARE index 
	
	DECLARE i 
	DECLARE top = ubound(years(),1)
	FOR i = 1 TO top
		_year = years(i)
		cycleyear = _year - _base
		stemnumber = MOD(cycleyear, 10) 
		stemhan$    = celestial$(stemnumber + 1)
		stempinyin$ = celestialpinyin$(stemnumber + 1)
		elementnumber = div(stemnumber, 2) + 1
		element$       = elements$(elementnumber)
		branchnumber = MOD(cycleyear, 12)		
		branchhan$    = terrestrial$(branchnumber + 1)
		branchpinyin$ = terrestrialpinyin$(branchnumber + 1)
		animal$       = animals$(branchnumber + 1)
		aspectnumber = MOD(cycleyear, 2)
		aspect$       = aspects$(aspectnumber + 1)
		index = MOD(cycleyear, 60) + 1		
		PRINT _year; 
		PRINT TAB(5);stemhan$+branchhan$;
		PRINT TAB(12);stempinyin$;"-";branchpinyin$;
		PRINT TAB(25);element$;" ";animal$;" ("+aspect$+")";
		PRINT TAB(50);"year";index;"of the cycle"		
	NEXT

```


```txt

$ tbas chinZod.bas
 1935 乙亥 yĭ-hài     Wood Pig (yin)           year 12 of the cycle
 1938 戊寅 wù-yín     Earth Tiger (yang)       year 15 of the cycle
 1931 辛未 xīn-wèi    Metal Goat (yin)         year 8 of the cycle
 1961 辛丑 xīn-chŏu   Metal Ox (yin)           year 38 of the cycle
 1963 癸卯 gŭi-măo    Water Rabbit (yin)       year 40 of the cycle
 1991 辛未 xīn-wèi    Metal Goat (yin)         year 8 of the cycle
 1993 癸酉 gŭi-yŏu    Water Rooster (yin)      year 10 of the cycle
 1996 丙子 bĭng-zĭ    Fire Rat (yang)          year 13 of the cycle
 2001 辛巳 xīn-sì     Metal Snake (yin)        year 18 of the cycle


```


## Tcl


```Tcl

proc cn_zodiac year {
   set year0 [expr $year-4]
   set animals {Rat Ox Tiger Rabbit Dragon Snake Horse Goat Monkey Rooster Dog Pig}
   set elements {Wood Fire Earth Metal Water}
   set stems {jia3 yi3 bing3 ding1 wu4 ji3 geng1 xin1 ren2 gui3}
   set gan {\u7532 \u4E59 \u4E19 \u4E01 \u620A \u5DF1 \u5E9A \u8F9B \u58EC \u7678}
   set branches {zi3 chou3 yin2 mao3 chen2 si4 wu3 wei4 shen1 you3 xu1 hai4}
   set zhi {\u5B50 \u4E11 \u5BC5 \u536F \u8FB0 \u5DF3 \u5348 \u672A \u7533 \u9149 \u620C \u4EA5}
   set m10 [expr $year0%10]
   set m12 [expr $year0%12]
   set res [lindex $gan $m10][lindex $zhi $m12]
   lappend res [lindex $stems $m10]-[lindex $branches $m12]
   lappend res [lindex $elements [expr $m10/2]]
   lappend res [lindex $animals $m12] ([expr {$year0%2 ? "yin" : "yang"}])
   lappend res year [expr $year0%60+1]
   return $res
}

```

```txt

% cn_zodiac 1984
甲子 jia3-zi3 Wood Rat (yang) year 1
% cn_zodiac 2017
丁酉 ding1-you3 Fire Rooster (yin) year 34

```



## UTFool



```UTFool

···
http://rosettacode.org/wiki/Chinese_zodiac
···
■ ChineseZodiac
  § static
    tiangan⦂ String[][]: ¤ · 10 celestial stems
     ¤ "甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"
     ¤ "jiă", "yĭ", "bĭng", "dīng", "wù", "jĭ", "gēng", "xīn", "rén", "gŭi"
     
    dizhi⦂ String[][]: ¤ · 12 terrestrial branches
     ¤ "子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"
     ¤ "zĭ", "chŏu", "yín", "măo", "chén", "sì", "wŭ", "wèi", "shēn", "yŏu", "xū", "hài"
     
    wuxing⦂ String[][]: ¤ · 5 traditional elements
     ¤ "木", "火", "土", "金", "水"
     ¤ "mù", "huǒ", "tǔ", "jīn", "shuǐ"
     ¤ "wood", "fire", "earth", "metal", "water"
     
    shengxiao⦂ String[][]: ¤ · 12 animal deities
     ¤ "鼠", "牛", "虎", "兔", "龍", "蛇", "馬", "羊", "猴", "鸡", "狗", "豬"
     ¤ "shǔ", "niú", "hǔ", "tù", "lóng", "shé", "mǎ", "yáng", "hóu", "jī", "gǒu", "zhū"
     ¤ "rat", "ox", "tiger", "rabbit", "dragon", "snake", "horse", "goat", "monkey", "rooster", "dog", "pig"
     
    yinyang⦂ String[][]: ¤ · 2 fundamental principles
     ¤ "阳", "阴"
     ¤ "yáng", "yīn"

    ▶ main
    • args⦂ String[]
      for each year ∈ [1935, 1938, 1968, 1972, 1976, 1984, 1985, 1986, 2017]⦂ int
          cycle⦂  int: year - 4
          stem⦂   int: cycle \ 10
          branch⦂ int: cycle \ 12
          System.out.printf "%4s  %-8s %-6s %-6s %s\n", year, tiangan[0][stem] ⊕ dizhi[0][branch],
            wuxing[0][stem / 2], shengxiao[0][branch], yinyang[0][year \ 2]
          System.out.printf "      %-9s %-7s %-7s %s\n", tiangan[1][stem] ⊕ dizhi[1][branch],
            wuxing[1][stem / 2], shengxiao[1][branch], yinyang[1][year \ 2]
          System.out.printf "      %-2s/60     %-7s %s\n\n", cycle \ 60 + 1,
            wuxing[2][stem / 2], shengxiao[2][branch]

```



## VBScript

To run in console mode with cscript.

```vb
' Chinese zodiac - VBS

Animals  = array( "Rat","Ox","Tiger","Rabbit","Dragon","Snake","Horse","Goat","Monkey","Rooster","Dog","Pig" )
Elements = array( "Wood","Fire","Earth","Metal","Water" )
YinYang  = array( "Yang","Yin" )
Years    = array( 1935, 1938, 1968, 1972, 1976, 1984, 2017 )

for i = LBound(Years) to UBound(Years)
	xYear    = Years(i)
	yElement = Elements(((xYear - 4) mod 10) \ 2)
	yAnimal  = Animals(  (xYear - 4) mod 12     )
	yYinYang = YinYang(   xYear      mod  2     )
	nn       =          ((xYear - 4) mod 60) + 1
	msgbox xYear & " is the year of the " & yElement & " " &  yAnimal & " (" &  yYinYang & ").",, _
		   xYear & " : " & nn & "/60"
next 
```

{{out}}

```txt

1935 is the year of the Wood Pig (Yin).
1938 is the year of the Earth Tiger (Yang).
1968 is the year of the Earth Monkey (Yang).
1972 is the year of the Water Rat (Yang).
1976 is the year of the Fire Dragon (Yang).
1984 is the year of the Wood Rat (Yang).
2017 is the year of the Fire Rooster (Yin).

```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    ReadOnly ANIMALS As String() = {"Rat", "Ox", "Tiger", "Rabbit", "Dragon", "Snake", "Horse", "Goat", "Monkey", "Rooster", "Dog", "Pig"}
    ReadOnly ELEMENTS As String() = {"Wood", "Fire", "Earth", "Metal", "Water"}
    ReadOnly ANIMAL_CHARS As String() = {"子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"}
    ReadOnly ELEMENT_CHARS As String(,) = {{"甲", "丙", "戊", "庚", "壬"}, {"乙", "丁", "己", "辛", "癸"}}

    Function GetYY(year As Integer) As String
        If year Mod 2 = 0 Then
            Return "yang"
        End If
        Return "yin"
    End Function

    Sub Main()
        Console.OutputEncoding = System.Text.Encoding.UTF8
        Dim years = {1935, 1938, 1968, 1972, 1976, 1984, 1985, 2017}
        For i = 0 To years.Length - 1
            Dim t0 = years(i)
            Dim t1 = t0 - 4.0
            Dim t2 = t1 Mod 10
            Dim t3 = t2 / 2
            Dim t4 = Math.Floor(t3)

            Dim ei As Integer = Math.Floor(((years(i) - 4.0) Mod 10) / 2)
            Dim ai = (years(i) - 4) Mod 12
            Console.WriteLine("{0} is the year of the {1} {2} ({3}). {4}{5}", years(i), ELEMENTS(ei), ANIMALS(ai), GetYY(years(i)), ELEMENT_CHARS(years(i) Mod 2, ei), ANIMAL_CHARS((years(i) - 4) Mod 12))
        Next
    End Sub

End Module
```

{{out}}

```txt
1935 is the year of the Wood Pig (yin). 乙亥
1938 is the year of the Earth Tiger (yang). 戊寅
1968 is the year of the Earth Monkey (yang). 戊申
1972 is the year of the Water Rat (yang). 壬子
1976 is the year of the Fire Dragon (yang). 丙辰
1984 is the year of the Wood Rat (yang). 甲子
1985 is the year of the Wood Ox (yin). 乙丑
2017 is the year of the Fire Rooster (yin). 丁酉
```



## zkl

{{trans|Ruby}}

```zkl
fcn ceToChineseZodiac(ce_year){	// --> list of strings
   # encoding: utf-8
   var [const] pinyin=SD(  // create some static variables, SD is small fixed dictionary
     "甲","jiă",  "乙","yĭ",  "丙","bĭng", "丁","dīng", "戊","wù", "己","jĭ",
     "庚","gēng", "辛","xīn", "壬","rén",  "癸","gŭi",

     "子","zĭ",  "丑","chŏu", "寅","yín", "卯","măo",  "辰","chén", "巳","sì",
     "午","wŭ",  "未","wèi",  "申","shén","酉","yŏu",  "戌","xū",   "亥","hài"
   ),

   celestial  =T("甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸"),
   terrestrial=T("子", "丑", "寅", "卯", "辰", "巳", "午", "未", "申", "酉", "戌", "亥"),
   animals    =T("Rat",   "Ox",   "Tiger",  "Rabbit",  "Dragon", "Snake",
		 "Horse", "Goat", "Monkey", "Rooster", "Dog",    "Pig"),
   elements   =T("Wood", "Fire", "Earth", "Metal", "Water"),
   aspects    =T("yang","yin"),
   ;

   const BASE=4;
 
   cycle_year:=ce_year - BASE;

   cycle_year,aspect    := ce_year - BASE,         aspects[cycle_year%2];
   stem_number,element  := cycle_year%10,          elements[stem_number/2];
   stem_han,stem_pinyin := celestial[stem_number], pinyin[stem_han];

   branch_number,animal     := cycle_year%12,      animals[branch_number];
   branch_han,branch_pinyin := terrestrial[branch_number], pinyin[branch_han];

   return(stem_han,branch_han,
	  stem_pinyin,branch_pinyin, element,animal,aspect)
}
```


```zkl
foreach ce_year in (T(1935,1938,1968,1972,1976,Time.Clock.UTC[0])){
   println("%d: %s%s (%s-%s, %s %s; %s)"
           .fmt(ce_year,ceToChineseZodiac(ce_year).xplode()));
}
```

{{out}}

```txt

1935: 乙亥 (yĭ-hài, Wood Pig; yin)
1938: 戊寅 (wù-yín, Earth Tiger; yang)
1968: 戊申 (wù-shén, Earth Monkey; yang)
1972: 壬子 (rén-zĭ, Water Rat; yang)
1976: 丙辰 (bĭng-chén, Fire Dragon; yang)
2017: 丁酉 (dīng-yŏu, Fire Rooster; yin)

```

