+++
title = "Idiomatically determine all the lowercase and uppercase letters"
description = ""
date = 2019-09-12T18:05:09Z
aliases = []
[extra]
id = 17424
[taxonomies]
categories = ["task"]
tags = []
+++

[[File:AbcxyzABCXYZ.jpg|100px||right]]

Idiomatically determine all the lowercase and uppercase letters   (of the Latin [English] alphabet)   being used currently by a computer programming language.
The method should find the letters regardless of the hardware architecture that is being used (ASCII, EBCDIC, or other).


;Task requirements

Display the set of all:
::::::*   lowercase letters
::::::*   uppercase letters


that can be used (allowed) by the computer program,


where   ''letter''   is a member of the Latin (English) alphabet:     '''a''' ──► '''z'''     and     '''A''' ──► '''Z'''.


You may want to mention what hardware architecture is being used, and if applicable, the operating system.


## See also

* [[Idiomatically_determine_all_the_characters_that_can_be_used_for_symbols|Idiomatically determine all the characters that can be used for symbols]].





## ALGOL 68

Uses the non-standard is lower and is upper procedures provided by Algol 68G.

```algol68
STRING lc := "";
STRING uc := "";
FOR c FROM 0 TO max abs char DO
    CHAR ch := REPR c;
    IF is lower( ch ) THEN lc +:= ch FI;
    IF is upper( ch ) THEN uc +:= ch FI
OD;
print( ( "lower: """ + lc + """", newline ) );
print( ( "upper: """ + uc + """", newline ) )
```

```txt

lower: "abcdefghijklmnopqrstuvwxyz"
upper: "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

```




## AWK


```AWK

# syntax: GAWK -f IDIOMATICALLY_DETERMINE_ALL_THE_LOWERCASE_AND_UPPERCASE_LETTERS.AWK
BEGIN {
    for (i=0; i<=255; i++) {
      c = sprintf("%c",i)
      if (c ~ /[[:lower:]]/) {
        lower_chars = lower_chars c
      }
      if (c ~ /[[:upper:]]/) {
        upper_chars = upper_chars c
      }
    }
    printf("%s\n",ARGV[0])
    printf("lowercase %d: %s\n",length(lower_chars),lower_chars)
    printf("uppercase %d: %s\n",length(upper_chars),upper_chars)
    exit(0)
}

```

<p>output using Microsoft Windows 8 version 6.2.9200 with code page 437 in a DOS window</p>

```txt

gawk_3_1_8
lowercase 26: abcdefghijklmnopqrstuvwxyz
uppercase 26: ABCDEFGHIJKLMNOPQRSTUVWXYZ

gawk_4_1_0
lowercase 65: abcdefghijklmnopqrstuvwxyzƒsozªµºßàáâaäåæçèéêëìíîïdñòóôoöoùúûüy_ÿ
uppercase 60: ABCDEFGHIJKLMNOPQRSTUVWXYZSOZYAAAAÄÅÆÇEÉEEIIIIDÑOOOOÖOUUUÜY_

```




## C


```c
#include <stdio.h>

int main(int argc, char const *argv[]) {
  for (char c = 0x41; c < 0x5b; c ++) putchar(c);
  putchar('\n');
  for (char c = 0x61; c < 0x7b; c ++) putchar(c);
  putchar('\n');
  return 0;
}
```

```txt

ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz

```



## COBOL

Tested with GnuCOBOL 3.

Of note: COBOL is a one relative ordinal language.  CHAR(1) is the NUL byte, value 0 in memory.
The COBOL ''ALPHABETIC-LOWER'' and ''ALPHABETIC-UPPER'' category tests both include space as in the set.

NATIONAL character sets not tested here, code is commented out to satisfy the task spec of Latin/ENGLISH but left in listing for the benefit of the reader.


```cobol
       identification division.
       program-id. determine.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       data division.
       working-storage section.
       01 tx pic x.
       01 lower-8bit pic x(256).
       01 upper-8bit pic x(256).

      *> 01 tn pic n.
      *> 01 lower-set pic n(65536).
      *> 01 upper-set pic n(65536).

       01 low-slide usage index.
       01 high-slide usage index.

       procedure division.
       determining.

      *> COBOL pic x, an 8 bit data encoding
       set low-slide to 0
       set high-slide to 0
       perform varying tally from 1 by 1 until tally > 256
           move char(tally) to tx
           if tx is alphabetic-lower then
               set low-slide up by 1
               move tx to lower-8bit(low-slide:1)
           end-if
           if tx is alphabetic-upper then
               set high-slide up by 1
               move tx to upper-8bit(high-slide:1)
           end-if
       end-perform
       if low-slide equal 0 then
           display "no lower case letters detected" upon syserr
       else
           display lower-8bit(1:low-slide)
       end-if
       if high-slide equal 0 then
           display "no upper case letters detected" upon syserr
       else
           display upper-8bit(1:high-slide)
       end-if

      *> COBOL standard NATIONAL data type, a 16 bit encoding
      *> commented out: task description may not want extended encodings

      *> set low-slide to 0
      *> set high-slide to 0
      *> perform varying tally from 1 by 1 until tally > 65536
      *>     move char-national(tally) to tn
      *>     if tn is alphabetic-lower then
      *>         set low-slide up by 1
      *>         move tn to lower-set(low-slide:1)
      *>     end-if
      *>     if tn is alphabetic-upper then
      *>         set high-slide up by 1
      *>         move tn to upper-set(high-slide:1)
      *>     end-if
      *> end-perform
      *> if low-slide equal 0 then
      *>     display "no lower case letters detected" upon syserr
      *> else
      *>     display lower-set(1:low-slide)
      *> end-if
      *> if high-slide equal 0 then
      *>     display "no upper case letters detected" upon syserr
      *> else
      *>     display upper-set(1:high-slide)
      *> end-if

       goback.
       end program determine.
```


```txt
prompt$ cobc -xj determine-letters.cob
 abcdefghijklmnopqrstuvwxyz
 ABCDEFGHIJKLMNOPQRSTUVWXYZ
```

As noted, space is included in the category tests.

=={{header|F_Sharp|F#}}==

```fsharp

seq{(char)0..(char)127} |> Seq.filter(System.Char.IsUpper) |> Seq.iter (string >> printf "%s"); printfn ""
seq{(char)0..(char)127} |> Seq.filter(System.Char.IsLower) |> Seq.iter (string >> printf "%s"); printfn ""

```

```txt

ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz

```



## Factor


```factor

USE: math.ranges
CHAR: A CHAR: Z [a,b] >string print
CHAR: a CHAR: z [a,b] >string print

```

```txt

ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz

```



## Go

Shown are ASCII and Unicode lower and upper case.  Relevant to Unicode definitions of lower and upper case is not the hardware or operating system but the Unicode version implemented.

```go
package main

import (
	"fmt"
	"unicode"
)

const (
	lcASCII = "abcdefghijklmnopqrstuvwxyz"
	ucASCII = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
)

func main() {
	fmt.Println("ASCII lower case:")
	fmt.Println(lcASCII)
	for l := 'a'; l <= 'z'; l++ {
		fmt.Print(string(l))
	}
	fmt.Println()

	fmt.Println("\nASCII upper case:")
	fmt.Println(ucASCII)
	for l := 'A'; l <= 'Z'; l++ {
		fmt.Print(string(l))
	}
	fmt.Println()

	fmt.Println("\nUnicode version " + unicode.Version)
	showRange16("Lower case 16-bit code points:", unicode.Lower.R16)
	showRange32("Lower case 32-bit code points:", unicode.Lower.R32)
	showRange16("Upper case 16-bit code points:", unicode.Upper.R16)
	showRange32("Upper case 32-bit code points:", unicode.Upper.R32)
}

func showRange16(hdr string, rList []unicode.Range16) {
	fmt.Print("\n", hdr, "\n")
	fmt.Printf("%d ranges:\n", len(rList))
	for _, rng := range rList {
		fmt.Printf("%U: ", rng.Lo)
		for r := rng.Lo; r <= rng.Hi; r += rng.Stride {
			fmt.Printf("%c", r)
		}
		fmt.Println()
	}
}

func showRange32(hdr string, rList []unicode.Range32) {
	fmt.Print("\n", hdr, "\n")
	fmt.Printf("%d ranges:\n", len(rList))
	for _, rng := range rList {
		fmt.Printf("%U: ", rng.Lo)
		for r := rng.Lo; r <= rng.Hi; r += rng.Stride {
			fmt.Printf("%c", r)
		}
		fmt.Println()
	}
}
```

```txt

ASCII lower case:
abcdefghijklmnopqrstuvwxyz
abcdefghijklmnopqrstuvwxyz

ASCII upper case:
ABCDEFGHIJKLMNOPQRSTUVWXYZ
ABCDEFGHIJKLMNOPQRSTUVWXYZ

Unicode version 7.0.0

Lower case 16-bit code points:
113 ranges:
U+0061: abcdefghijklmnopqrstuvwxyz
U+00B5: µß
U+00E0: àáâãäåæçèéêëìíîïðñòóôõö
U+00F8: øùúûüýþÿ
...output trimed...
U+FB00: ﬀﬁﬂﬃﬄﬅﬆ
U+FB13: ﬓﬔﬕﬖﬗ
U+FF41: ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ

Lower case 32-bit code points:
30 ranges:
U+10428: 𐐨𐐩𐐪𐐫𐐬𐐭𐐮𐐯𐐰𐐱𐐲𐐳𐐴𐐵𐐶𐐷𐐸𐐹𐐺𐐻𐐼𐐽𐐾𐐿𐑀𐑁𐑂𐑃𐑄𐑅𐑆𐑇𐑈𐑉𐑊𐑋𐑌𐑍𐑎𐑏
U+118C0: 𑣀𑣁𑣂𑣃𑣄𑣅𑣆𑣇𑣈𑣉𑣊𑣋𑣌𑣍𑣎𑣏𑣐𑣑𑣒𑣓𑣔𑣕𑣖𑣗𑣘𑣙𑣚𑣛𑣜𑣝𑣞𑣟
U+1D41A: 𝐚𝐛𝐜𝐝𝐞𝐟𝐠𝐡𝐢𝐣𝐤𝐥𝐦𝐧𝐨𝐩𝐪𝐫𝐬𝐭𝐮𝐯𝐰𝐱𝐲𝐳
U+1D44E: 𝑎𝑏𝑐𝑑𝑒𝑓𝑔
...output trimmed...
U+1D7C4: 𝟄𝟅𝟆𝟇𝟈𝟉
U+1D7CB: 𝟋

Upper case 16-bit code points:
101 ranges:
U+0041: ABCDEFGHIJKLMNOPQRSTUVWXYZ
U+00C0: ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ
...output trimmed...
U+A7B0: ꞰꞱ
U+FF21: ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ

Upper case 32-bit code points:
32 ranges:
U+10400: 𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈𐐉𐐊𐐋𐐌𐐍𐐎𐐏𐐐𐐑𐐒𐐓𐐔𐐕𐐖𐐗𐐘𐐙𐐚𐐛𐐜𐐝𐐞𐐟𐐠𐐡𐐢𐐣𐐤𐐥𐐦𐐧
U+118A0: 𑢠𑢡𑢢𑢣𑢤𑢥𑢦𑢧𑢨𑢩𑢪𑢫𑢬𑢭𑢮𑢯𑢰𑢱𑢲𑢳𑢴𑢵𑢶𑢷𑢸𑢹𑢺𑢻𑢼𑢽𑢾𑢿
U+1D400: 𝐀𝐁𝐂𝐃𝐄𝐅𝐆𝐇𝐈𝐉𝐊𝐋𝐌𝐍𝐎𝐏𝐐𝐑𝐒𝐓𝐔𝐕𝐖𝐗𝐘𝐙
U+1D434: 𝐴𝐵𝐶𝐷𝐸𝐹𝐺𝐻𝐼𝐽𝐾𝐿𝑀𝑁𝑂𝑃𝑄𝑅𝑆𝑇𝑈𝑉𝑊𝑋𝑌𝑍
...ouput trimmed...
U+1D790: 𝞐𝞑𝞒𝞓𝞔𝞕𝞖𝞗𝞘𝞙𝞚𝞛𝞜𝞝𝞞𝞟𝞠𝞡𝞢𝞣𝞤𝞥𝞦𝞧𝞨
U+1D7CA: 𝟊

```



## Haskell

The specification seems pretty explicit that it only wants the English letters from <i>a</i> to <i>z</i> and from <i>A</i> to <i>Z</i>, so we don't need to worry about Unicode this time.

```haskell
main = do putStrLn $ "Lower: " ++ ['a'..'z']
          putStrLn $ "Upper: " ++ ['A'..'Z']
```

```txt
Lower: abcdefghijklmnopqrstuvwxyz
Upper: ABCDEFGHIJKLMNOPQRSTUVWXYZ
```



And if we want to generalise a little beyond a narrowly Anglo-Saxon notion of the alphabet:

```Haskell
import Data.List (partition)
import Data.Char (chr, isPrint, isUpper, isLower)

uppersAndLowers :: (String, String)
uppersAndLowers =
  let glyphs = filter isPrint (chr <$> [1 .. 0x10ffff])
      (us, rs) = partition isUpper glyphs
      (ls, _) = partition isLower rs
  in (us, ls)

chunksOf :: Int -> [a] -> [[a]]
chunksOf i xs = take i <$> ($ (:)) (splits xs) []
  where
    splits [] _ n = []
    splits l c n = l `c` splits (drop i l) c n

main :: IO ()
main = do
  mapM_ putStrLn ("Upper:" : chunksOf 70 (fst uppersAndLowers))
  putStrLn ""
  mapM_ putStrLn ("Lower:" : chunksOf 70 (snd uppersAndLowers))
```

```txt
Upper:
ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞĀĂĄĆĈĊČĎĐĒĔĖĘĚ
ĜĞĠĢĤĦĨĪĬĮİĲĴĶĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŸŹŻŽƁƂƄƆƇƉƊƋƎƏƐƑƓƔƖƗƘƜƝƟƠ
ƢƤƦƧƩƬƮƯƱƲƳƵƷƸƼǄǅǇǈǊǋǍǏǑǓǕǗǙǛǞǠǢǤǦǨǪǬǮǱǲǴǶǷǸǺǼǾȀȂȄȆȈȊȌȎȐȒȔȖȘȚȜȞȠȢȤȦȨȪȬ
ȮȰȲȺȻȽȾɁɃɄɅɆɈɊɌɎͰͲͶͿΆΈΉΊΌΎΏΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΪΫϏϒϓϔϘϚϜϞϠϢϤϦϨϪϬϮϴ
ϷϹϺϽϾϿЀЁЂЃЄЅІЇЈЉЊЋЌЍЎЏАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯѠѢѤѦѨѪѬѮѰѲѴѶѸѺѼѾ
ҀҊҌҎҐҒҔҖҘҚҜҞҠҢҤҦҨҪҬҮҰҲҴҶҸҺҼҾӀӁӃӅӇӉӋӍӐӒӔӖӘӚӜӞӠӢӤӦӨӪӬӮӰӲӴӶӸӺӼӾԀԂԄԆԈԊԌԎԐԒ
ԔԖԘԚԜԞԠԢԤԦԨԪԬԮԱԲԳԴԵԶԷԸԹԺԻԼԽԾԿՀՁՂՃՄՅՆՇՈՉՊՋՌՍՎՏՐՑՒՓՔՕՖႠႡႢႣႤႥႦႧႨႩႪႫႬႭႮႯႰႱ
ႲႳႴႵႶႷႸႹႺႻႼႽႾႿჀჁჂჃჄჅჇჍḀḂḄḆḈḊḌḎḐḒḔḖḘḚḜḞḠḢḤḦḨḪḬḮḰḲḴḶḸḺḼḾṀṂṄṆṈṊṌṎṐṒṔṖṘṚṜṞ
ṠṢṤṦṨṪṬṮṰṲṴṶṸṺṼṾẀẂẄẆẈẊẌẎẐẒẔẞẠẢẤẦẨẪẬẮẰẲẴẶẸẺẼẾỀỂỄỆỈỊỌỎỐỒỔỖỘỚỜỞỠỢỤỦỨỪỬỮỰỲ
ỴỶỸỺỼỾἈἉἊἋἌἍἎἏἘἙἚἛἜἝἨἩἪἫἬἭἮἯἸἹἺἻἼἽἾἿὈὉὊὋὌὍὙὛὝὟὨὩὪὫὬὭὮὯᾈᾉᾊᾋᾌᾍᾎᾏᾘᾙᾚᾛᾜᾝᾞᾟ
ᾨᾩᾪᾫᾬᾭᾮᾯᾸᾹᾺΆᾼῈΈῊΉῌῘῙῚΊῨῩῪΎῬῸΌῺΏῼℂℇℋℌℍℐℑℒℕℙℚℛℜℝℤΩℨKÅℬℭℰℱℲℳℾℿⅅↃⰀⰁⰂⰃⰄⰅⰆⰇⰈ
ⰉⰊⰋⰌⰍⰎⰏⰐⰑⰒⰓⰔⰕⰖⰗⰘⰙⰚⰛⰜⰝⰞⰟⰠⰡⰢⰣⰤⰥⰦⰧⰨⰩⰪⰫⰬⰭⰮⱠⱢⱣⱤⱧⱩⱫⱭⱮⱯⱰⱲⱵⱾⱿⲀⲂⲄⲆⲈⲊⲌⲎⲐⲒⲔⲖⲘⲚⲜⲞⲠ
ⲢⲤⲦⲨⲪⲬⲮⲰⲲⲴⲶⲸⲺⲼⲾⳀⳂⳄⳆⳈⳊⳌⳎⳐⳒⳔⳖⳘⳚⳜⳞⳠⳢⳫⳭⳲꙀꙂꙄꙆꙈꙊꙌꙎꙐꙒꙔꙖꙘꙚꙜꙞꙠꙢꙤꙦꙨꙪꙬꚀꚂꚄꚆꚈꚊꚌꚎꚐꚒꚔ
ꚖꚘꚚꜢꜤꜦꜨꜪꜬꜮꜲꜴꜶꜸꜺꜼꜾꝀꝂꝄꝆꝈꝊꝌꝎꝐꝒꝔꝖꝘꝚꝜꝞꝠꝢꝤꝦꝨꝪꝬꝮꝹꝻꝽꝾꞀꞂꞄꞆꞋꞍꞐꞒꞖꞘꞚꞜꞞꞠꞢꞤꞦꞨꞪꞫꞬꞭꞰꞱＡ
ＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ𐐀𐐁𐐂𐐃𐐄𐐅𐐆𐐇𐐈𐐉𐐊𐐋𐐌𐐍𐐎𐐏𐐐𐐑𐐒𐐓𐐔𐐕𐐖𐐗𐐘𐐙𐐚𐐛𐐜𐐝𐐞𐐟𐐠𐐡𐐢𐐣𐐤𐐥𐐦𐐧𑢠𑢡𑢢𑢣𑢤
𑢥𑢦𑢧𑢨𑢩𑢪𑢫𑢬𑢭𑢮𑢯𑢰𑢱𑢲𑢳𑢴𑢵𑢶𑢷𑢸𑢹𑢺𑢻𑢼𑢽𑢾𑢿𝐀𝐁𝐂𝐃𝐄𝐅𝐆𝐇𝐈𝐉𝐊𝐋𝐌𝐍𝐎𝐏𝐐𝐑𝐒𝐓𝐔𝐕𝐖𝐗𝐘𝐙𝐴𝐵𝐶𝐷𝐸𝐹𝐺𝐻𝐼𝐽𝐾𝐿𝑀𝑁𝑂𝑃𝑄
𝑅𝑆𝑇𝑈𝑉𝑊𝑋𝑌𝑍𝑨𝑩𝑪𝑫𝑬𝑭𝑮𝑯𝑰𝑱𝑲𝑳𝑴𝑵𝑶𝑷𝑸𝑹𝑺𝑻𝑼𝑽𝑾𝑿𝒀𝒁𝒜𝒞𝒟𝒢𝒥𝒦𝒩𝒪𝒫𝒬𝒮𝒯𝒰𝒱𝒲𝒳𝒴𝒵𝓐𝓑𝓒𝓓𝓔𝓕𝓖𝓗𝓘𝓙𝓚𝓛𝓜𝓝𝓞𝓟𝓠
𝓡𝓢𝓣𝓤𝓥𝓦𝓧𝓨𝓩𝔄𝔅𝔇𝔈𝔉𝔊𝔍𝔎𝔏𝔐𝔑𝔒𝔓𝔔𝔖𝔗𝔘𝔙𝔚𝔛𝔜𝔸𝔹𝔻𝔼𝔽𝔾𝕀𝕁𝕂𝕃𝕄𝕆𝕊𝕋𝕌𝕍𝕎𝕏𝕐𝕬𝕭𝕮𝕯𝕰𝕱𝕲𝕳𝕴𝕵𝕶𝕷𝕸𝕹𝕺𝕻𝕼𝕽𝕾𝕿𝖀
𝖁𝖂𝖃𝖄𝖅𝖠𝖡𝖢𝖣𝖤𝖥𝖦𝖧𝖨𝖩𝖪𝖫𝖬𝖭𝖮𝖯𝖰𝖱𝖲𝖳𝖴𝖵𝖶𝖷𝖸𝖹𝗔𝗕𝗖𝗗𝗘𝗙𝗚𝗛𝗜𝗝𝗞𝗟𝗠𝗡𝗢𝗣𝗤𝗥𝗦𝗧𝗨𝗩𝗪𝗫𝗬𝗭𝘈𝘉𝘊𝘋𝘌𝘍𝘎𝘏𝘐𝘑𝘒𝘓𝘔
𝘕𝘖𝘗𝘘𝘙𝘚𝘛𝘜𝘝𝘞𝘟𝘠𝘡𝘼𝘽𝘾𝘿𝙀𝙁𝙂𝙃𝙄𝙅𝙆𝙇𝙈𝙉𝙊𝙋𝙌𝙍𝙎𝙏𝙐𝙑𝙒𝙓𝙔𝙕𝙰𝙱𝙲𝙳𝙴𝙵𝙶𝙷𝙸𝙹𝙺𝙻𝙼𝙽𝙾𝙿𝚀𝚁𝚂𝚃𝚄𝚅𝚆𝚇𝚈𝚉𝚨𝚩𝚪𝚫𝚬
𝚭𝚮𝚯𝚰𝚱𝚲𝚳𝚴𝚵𝚶𝚷𝚸𝚹𝚺𝚻𝚼𝚽𝚾𝚿𝛀𝛢𝛣𝛤𝛥𝛦𝛧𝛨𝛩𝛪𝛫𝛬𝛭𝛮𝛯𝛰𝛱𝛲𝛳𝛴𝛵𝛶𝛷𝛸𝛹𝛺𝜜𝜝𝜞𝜟𝜠𝜡𝜢𝜣𝜤𝜥𝜦𝜧𝜨𝜩𝜪𝜫𝜬𝜭𝜮𝜯𝜰𝜱𝜲𝜳𝜴
𝝖𝝗𝝘𝝙𝝚𝝛𝝜𝝝𝝞𝝟𝝠𝝡𝝢𝝣𝝤𝝥𝝦𝝧𝝨𝝩𝝪𝝫𝝬𝝭𝝮𝞐𝞑𝞒𝞓𝞔𝞕𝞖𝞗𝞘𝞙𝞚𝞛𝞜𝞝𝞞𝞟𝞠𝞡𝞢𝞣𝞤𝞥𝞦𝞧𝞨𝟊

Lower:
abcdefghijklmnopqrstuvwxyzµßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿāăąćĉċčďđēĕ
ėęěĝğġģĥħĩīĭįıĳĵķĸĺļľŀłńņňŉŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷźżžſƀƃƅƈƌƍƒƕƙƚƛƞơƣƥƨ
ƪƫƭưƴƶƹƺƽƾƿǆǉǌǎǐǒǔǖǘǚǜǝǟǡǣǥǧǩǫǭǯǰǳǵǹǻǽǿȁȃȅȇȉȋȍȏȑȓȕȗșțȝȟȡȣȥȧȩȫȭȯȱȳȴȵȶȷȸ
ȹȼȿɀɂɇɉɋɍɏɐɑɒɓɔɕɖɗɘəɚɛɜɝɞɟɠɡɢɣɤɥɦɧɨɩɪɫɬɭɮɯɰɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿʀʁʂʃʄʅʆʇʈʉʊʋ
ʌʍʎʏʐʑʒʓʕʖʗʘʙʚʛʜʝʞʟʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯͱͳͷͻͼͽΐάέήίΰαβγδεζηθικλμνξοπρςστυφχ
ψωϊϋόύώϐϑϕϖϗϙϛϝϟϡϣϥϧϩϫϭϯϰϱϲϳϵϸϻϼабвгдежзийклмнопрстуфхцчшщъыьэюяѐёђѓєѕ
іїјљњћќѝўџѡѣѥѧѩѫѭѯѱѳѵѷѹѻѽѿҁҋҍҏґғҕҗҙқҝҟҡңҥҧҩҫҭүұҳҵҷҹһҽҿӂӄӆӈӊӌӎӏӑӓӕӗәӛӝӟ
ӡӣӥӧөӫӭӯӱӳӵӷӹӻӽӿԁԃԅԇԉԋԍԏԑԓԕԗԙԛԝԟԡԣԥԧԩԫԭԯաբգդեզէըթժիլխծկհձղճմյնշոչպջռսվ
տրցւփքօֆևᴀᴁᴂᴃᴄᴅᴆᴇᴈᴉᴊᴋᴌᴍᴎᴏᴐᴑᴒᴓᴔᴕᴖᴗᴘᴙᴚᴛᴜᴝᴞᴟᴠᴡᴢᴣᴤᴥᴦᴧᴨᴩᴪᴫᵫᵬᵭᵮᵯᵰᵱᵲᵳᵴᵵᵶᵷᵹᵺᵻᵼ
ᵽᵾᵿᶀᶁᶂᶃᶄᶅᶆᶇᶈᶉᶊᶋᶌᶍᶎᶏᶐᶑᶒᶓᶔᶕᶖᶗᶘᶙᶚḁḃḅḇḉḋḍḏḑḓḕḗḙḛḝḟḡḣḥḧḩḫḭḯḱḳḵḷḹḻḽḿṁṃṅṇṉṋṍṏ
ṑṓṕṗṙṛṝṟṡṣṥṧṩṫṭṯṱṳṵṷṹṻṽṿẁẃẅẇẉẋẍẏẑẓẕẖẗẘẙẚẛẜẝẟạảấầẩẫậắằẳẵặẹẻẽếềểễệỉịọỏốồ
ổỗộớờởỡợụủứừửữựỳỵỷỹỻỽỿἀἁἂἃἄἅἆἇἐἑἒἓἔἕἠἡἢἣἤἥἦἧἰἱἲἳἴἵἶἷὀὁὂὃὄὅὐὑὒὓὔὕὖὗὠὡὢὣ
ὤὥὦὧὰάὲέὴήὶίὸόὺύὼώᾀᾁᾂᾃᾄᾅᾆᾇᾐᾑᾒᾓᾔᾕᾖᾗᾠᾡᾢᾣᾤᾥᾦᾧᾰᾱᾲᾳᾴᾶᾷιῂῃῄῆῇῐῑῒΐῖῗῠῡῢΰῤῥῦῧῲ
ῳῴῶῷℊℎℏℓℯℴℹℼℽⅆⅇⅈⅉⅎↄⰰⰱⰲⰳⰴⰵⰶⰷⰸⰹⰺⰻⰼⰽⰾⰿⱀⱁⱂⱃⱄⱅⱆⱇⱈⱉⱊⱋⱌⱍⱎⱏⱐⱑⱒⱓⱔⱕⱖⱗⱘⱙⱚⱛⱜⱝⱞⱡⱥⱦⱨ
ⱪⱬⱱⱳⱴⱶⱷⱸⱹⱺⱻⲁⲃⲅⲇⲉⲋⲍⲏⲑⲓⲕⲗⲙⲛⲝⲟⲡⲣⲥⲧⲩⲫⲭⲯⲱⲳⲵⲷⲹⲻⲽⲿⳁⳃⳅⳇⳉⳋⳍⳏⳑⳓⳕⳗⳙⳛⳝⳟⳡⳣⳤⳬⳮⳳⴀⴁⴂⴃⴄ
ⴅⴆⴇⴈⴉⴊⴋⴌⴍⴎⴏⴐⴑⴒⴓⴔⴕⴖⴗⴘⴙⴚⴛⴜⴝⴞⴟⴠⴡⴢⴣⴤⴥⴧⴭꙁꙃꙅꙇꙉꙋꙍꙏꙑꙓꙕꙗꙙꙛꙝꙟꙡꙣꙥꙧꙩꙫꙭꚁꚃꚅꚇꚉꚋꚍꚏꚑꚓꚕꚗ
ꚙꚛꜣꜥꜧꜩꜫꜭꜯꜰꜱꜳꜵꜷꜹꜻꜽꜿꝁꝃꝅꝇꝉꝋꝍꝏꝑꝓꝕꝗꝙꝛꝝꝟꝡꝣꝥꝧꝩꝫꝭꝯꝱꝲꝳꝴꝵꝶꝷꝸꝺꝼꝿꞁꞃꞅꞇꞌꞎꞑꞓꞔꞕꞗꞙꞛꞝꞟꞡꞣ
ꞥꞧꞩꟺꬰꬱꬲꬳꬴꬵꬶꬷꬸꬹꬺꬻꬼꬽꬾꬿꭀꭁꭂꭃꭄꭅꭆꭇꭈꭉꭊꭋꭌꭍꭎꭏꭐꭑꭒꭓꭔꭕꭖꭗꭘꭙꭚꭤꭥﬀﬁﬂﬃﬄﬅﬆﬓﬔﬕﬖﬗａｂｃｄｅｆｇｈｉ
ｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ𐐨𐐩𐐪𐐫𐐬𐐭𐐮𐐯𐐰𐐱𐐲𐐳𐐴𐐵𐐶𐐷𐐸𐐹𐐺𐐻𐐼𐐽𐐾𐐿𐑀𐑁𐑂𐑃𐑄𐑅𐑆𐑇𐑈𐑉𐑊𐑋𐑌𐑍𐑎𐑏𑣀𑣁𑣂𑣃𑣄𑣅𑣆𑣇𑣈𑣉𑣊𑣋𑣌
𑣍𑣎𑣏𑣐𑣑𑣒𑣓𑣔𑣕𑣖𑣗𑣘𑣙𑣚𑣛𑣜𑣝𑣞𑣟𝐚𝐛𝐜𝐝𝐞𝐟𝐠𝐡𝐢𝐣𝐤𝐥𝐦𝐧𝐨𝐩𝐪𝐫𝐬𝐭𝐮𝐯𝐰𝐱𝐲𝐳𝑎𝑏𝑐𝑑𝑒𝑓𝑔𝑖𝑗𝑘𝑙𝑚𝑛𝑜𝑝𝑞𝑟𝑠𝑡𝑢𝑣𝑤𝑥𝑦𝑧
𝒂𝒃𝒄𝒅𝒆𝒇𝒈𝒉𝒊𝒋𝒌𝒍𝒎𝒏𝒐𝒑𝒒𝒓𝒔𝒕𝒖𝒗𝒘𝒙𝒚𝒛𝒶𝒷𝒸𝒹𝒻𝒽𝒾𝒿𝓀𝓁𝓂𝓃𝓅𝓆𝓇𝓈𝓉𝓊𝓋𝓌𝓍𝓎𝓏𝓪𝓫𝓬𝓭𝓮𝓯𝓰𝓱𝓲𝓳𝓴𝓵𝓶𝓷𝓸𝓹𝓺𝓻𝓼𝓽𝓾
𝓿𝔀𝔁𝔂𝔃𝔞𝔟𝔠𝔡𝔢𝔣𝔤𝔥𝔦𝔧𝔨𝔩𝔪𝔫𝔬𝔭𝔮𝔯𝔰𝔱𝔲𝔳𝔴𝔵𝔶𝔷𝕒𝕓𝕔𝕕𝕖𝕗𝕘𝕙𝕚𝕛𝕜𝕝𝕞𝕟𝕠𝕡𝕢𝕣𝕤𝕥𝕦𝕧𝕨𝕩𝕪𝕫𝖆𝖇𝖈𝖉𝖊𝖋𝖌𝖍𝖎𝖏𝖐𝖑𝖒
𝖓𝖔𝖕𝖖𝖗𝖘𝖙𝖚𝖛𝖜𝖝𝖞𝖟𝖺𝖻𝖼𝖽𝖾𝖿𝗀𝗁𝗂𝗃𝗄𝗅𝗆𝗇𝗈𝗉𝗊𝗋𝗌𝗍𝗎𝗏𝗐𝗑𝗒𝗓𝗮𝗯𝗰𝗱𝗲𝗳𝗴𝗵𝗶𝗷𝗸𝗹𝗺𝗻𝗼𝗽𝗾𝗿𝘀𝘁𝘂𝘃𝘄𝘅𝘆𝘇𝘢𝘣𝘤𝘥𝘦
𝘧𝘨𝘩𝘪𝘫𝘬𝘭𝘮𝘯𝘰𝘱𝘲𝘳𝘴𝘵𝘶𝘷𝘸𝘹𝘺𝘻𝙖𝙗𝙘𝙙𝙚𝙛𝙜𝙝𝙞𝙟𝙠𝙡𝙢𝙣𝙤𝙥𝙦𝙧𝙨𝙩𝙪𝙫𝙬𝙭𝙮𝙯𝚊𝚋𝚌𝚍𝚎𝚏𝚐𝚑𝚒𝚓𝚔𝚕𝚖𝚗𝚘𝚙𝚚𝚛𝚜𝚝𝚞𝚟𝚠
𝚡𝚢𝚣𝚤𝚥𝛂𝛃𝛄𝛅𝛆𝛇𝛈𝛉𝛊𝛋𝛌𝛍𝛎𝛏𝛐𝛑𝛒𝛓𝛔𝛕𝛖𝛗𝛘𝛙𝛚𝛜𝛝𝛞𝛟𝛠𝛡𝛼𝛽𝛾𝛿𝜀𝜁𝜂𝜃𝜄𝜅𝜆𝜇𝜈𝜉𝜊𝜋𝜌𝜍𝜎𝜏𝜐𝜑𝜒𝜓𝜔𝜖𝜗𝜘𝜙𝜚𝜛𝜶𝜷𝜸
𝜹𝜺𝜻𝜼𝜽𝜾𝜿𝝀𝝁𝝂𝝃𝝄𝝅𝝆𝝇𝝈𝝉𝝊𝝋𝝌𝝍𝝎𝝐𝝑𝝒𝝓𝝔𝝕𝝰𝝱𝝲𝝳𝝴𝝵𝝶𝝷𝝸𝝹𝝺𝝻𝝼𝝽𝝾𝝿𝞀𝞁𝞂𝞃𝞄𝞅𝞆𝞇𝞈𝞊𝞋𝞌𝞍𝞎𝞏𝞪𝞫𝞬𝞭𝞮𝞯𝞰𝞱𝞲𝞳𝞴
𝞵𝞶𝞷𝞸𝞹𝞺𝞻𝞼𝞽𝞾𝞿𝟀𝟁𝟂𝟄𝟅𝟆𝟇𝟈𝟉𝟋
```



## J


This is somewhat smoke and mirrors, since J is based on ASCII (it's a successor to APL which gives up on the idea of supporting a non-ASCII character set).

That said:


```J
   (#~ tolower ~: toupper) a.
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
```


Note that, as hinted at above, we get the same result using UCS-2:


```J
   (#~ tolower ~: toupper) u: i.65536
ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz
```



## Java

```java
import java.util.stream.IntStream;

public class Letters {
    public static void main(String[] args) throws Exception {
        System.out.print("Upper case: ");
        IntStream.rangeClosed(0, 0x10FFFF)
                 .filter(Character::isUpperCase)
                 .limit(72)
                 .forEach(n -> System.out.printf("%c", n));
        System.out.println("...");

        System.out.print("Lower case: ");
        IntStream.rangeClosed(0, 0x10FFFF)
                 .filter(Character::isLowerCase)
                 .limit(72)
                 .forEach(n -> System.out.printf("%c", n));
        System.out.println("...");
    }
}
```



```txt
Upper case: ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞ...
Lower case: abcdefghijklmnopqrstuvwxyzªµºßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿāăąćĉċčďđēĕ...
```


==jq==
We first define a function for generating a JSON string of all the Unicode characters that have codepoints within a specified range and which belong to a specified "\p" character class.

```jq
# The range of codepoints is from m up to but excluding n;
# "class" should be a character class, e.g. Ll or Lu for lower/upper case respectively.
def generate(class; m; n):
  reduce (range(m;n) | [.] | implode | select( test( "\\p{" + class + "}" ))) as $c
    (""; . + $c);
```


'''The number of lowercase Unicode characters:'''

```jq
def maxu: 1114112;  # The number of Unicode codepoints
generate("Ll"; 0; maxu) | length  #=> 1607
```


'''The number of uppercase Unicode characters:'''

```jq
generate("Lu"; 0; maxu) | length  #=> 1296
```


'''Examples''':

Lowercase letters amongst the first 1024 Unicode codepoints:

```jq
generate("Ll"; 0; 1024)
```

```sh
abcdefghijklmnopqrstuvwxyzµºßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿāăąćĉċčďđēĕėęěĝğġģĥħĩīĭįıĳĵķĸĺļľŀłńņňŉŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷźżžſƀƃƅƈƌƍƒƕƙƚƛƞơƣƥƨƪƫƭưƴƶƹƺƽƾƿǆǉǌǎǐǒǔǖǘǚǜǝǟǡǣǥǧǩǫǭǯǰǳǵǹǻǽǿȁȃȅȇȉȋȍȏȑȓȕȗșțȝȟȡȣȥȧȩȫȭȯȱȳȴȵȶȷȸȹȼȿɀɐɑɒɓɔɕɖɗɘəɚɛɜɝɞɟɠɡɢɣɤɥɦɧɨɩɪɫɬɭɮɯɰɱɲɳɴɵɶɷɸɹɺɻɼɽɾɿʀʁʂʃʄʅʆʇʈʉʊʋʌʍʎʏʐʑʒʓʔʕʖʗʘʙʚʛʜʝʞʟʠʡʢʣʤʥʦʧʨʩʪʫʬʭʮʯΐάέήίΰαβγδεζηθικλμνξοπρςστυφχψωϊϋόύώϐϑϕϖϗϙϛϝϟϡϣϥϧϩϫϭϯϰϱϲϳϵϸϻϼ
```



Uppercase letters amongst the first 1024 Unicode codepoints:

```jq
generate("Lu"; 0; 1024)
```

```sh
ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮİĲĴĶĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŸŹŻŽƁƂƄƆƇƉƊƋƎƏƐƑƓƔƖƗƘƜƝƟƠƢƤƦƧƩƬƮƯƱƲƳƵƷƸƼǄǇǊǍǏǑǓǕǗǙǛǞǠǢǤǦǨǪǬǮǱǴǶǷǸǺǼǾȀȂȄȆȈȊȌȎȐȒȔȖȘȚȜȞȠȢȤȦȨȪȬȮȰȲȺȻȽȾɁΆΈΉΊΌΎΏΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΪΫϒϓϔϘϚϜϞϠϢϤϦϨϪϬϮϴϷϹϺϽϾϿ
```



## Julia

Julia supports Unicode natively: the Char type in Julia is 32 bits, and Julia code base supports Unicode characters up to 3 bytes in length. This means that unless ascii coding or the English alphabet is specified there are too many valid Chars to print here.
It is common to manipulate characters in varying ways, as below, as integers, unicode Chars, and strings.

```julia

function countunicode()
    englishlettercodes = [Int(c) for c in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"]
    count = 0
    az = ""
    AZ = ""
    for i in 0:0xffffff
        if is_assigned_char(i)
            count += 1
        end
        if i in englishlettercodes
            c = Char(i)
            if islower(c)
                az *= "$c"
            else
                AZ *= "$c"
            end
        end
    end
    count, az, AZ
end

unicodecount, lcletters, ucletters = countunicode()

print("There are $unicodecount valid Chars and the English ones are ")
println("lowercase: $lcletters and uppercase: $ucletters.")


```

There are 267753 valid Chars and the English ones are lowercase: abcdefghijklmnopqrstuvwxyz and uppercase: ABCDEFGHIJKLMNOPQRSTUVWXYZ.


## Kotlin


```scala
// version 1.0.6

fun main(args: Array<String>) {
    print("Lower case : ")
    for (ch in 'a'..'z') print(ch)
    print("\nUpper case : ")
    for (ch in 'A'..'Z') print(ch)
    println()
}
```


```txt

Lower case : abcdefghijklmnopqrstuvwxyz
Upper case : ABCDEFGHIJKLMNOPQRSTUVWXYZ

```



## Lua

The function below makes use of Lua's pattern matching system via the in-built string.match() function.  All characters matching the supplied pattern argument are concatenated to a string, which is returned as the result after all ASCII characters have been tested.

```Lua
function ASCIIstring (pattern)
    local matchString, ch = ""
    for charNum = 0, 255 do
        ch = string.char(charNum)
        if string.match(ch, pattern) then
            matchString = matchString .. ch
        end
    end
    return matchString
end

print(ASCIIstring("%l"))
print(ASCIIstring("%u"))

```

```txt
abcdefghijklmnopqrstuvwxyz
ABCDEFGHIJKLMNOPQRSTUVWXYZ
```



## MiniScript

MiniScript natively supports Unicode, but the task clearly specifies only English (unaccented) letters, so a solution is:

```MiniScript
toChars = function(seq)
    for i in seq.indexes
        seq[i] = char(seq[i])
    end for
    return seq.join("")
end function

print toChars(range(code("a"), code("z")))
print toChars(range(code("A"), code("Z")))
```


```txt
abcdefghijklmnopqrstuvwxyz
ABCDEFGHIJKLMNOPQRSTUVWXYZ
```



## PARI/GP

GP has no support for Unicode and does not even have reliable cross-platform support for upper ASCII. The only letters in lower ASCII are

```parigp
apply(Strchr, concat([65..90], [97..122]))
```

Upper ASCII (not to be confused with ISO Latin-1) has the following letters:

```parigp
apply(Strchr, concat(concat([128..154], [160..165]),concat(concat([181,182,183,198,199],[208..216]),[224..237])))
```



## Perl


```perl
for $i (0..2**8-1) {
    $c = chr $i;
    $lower .= $c if $c =~ /[[:lower:]]/;
    $upper .= $c if $c =~ /[[:upper:]]/;
}

print "$lower\n";
print "$upper\n";
```

```txt
abcdefghijklmnopqrstuvwxyz
ABCDEFGHIJKLMNOPQRSTUVWXYZ
```


## Perl 6

Perl 6 supports all the Unicode categories of characters natively.  The constant <tt>0x1fffd</tt> is <em>not</em> a typo for <tt>0x10ffff</tt>: we're restricting the range of characters to the first two Unicode planes, since the 3rd plane is reserved for ideographs (category Lo, "letter other"), and the subsequent planes contain no letters (yet).

```perl6
given (0..0x1fffd).chrs {
    say "Lowercase: ", .comb(/<:Ll>/);
    say "Uppercase: ", .comb(/<:Lu>/);
    say "Titlecase: ", .comb(/<:Lt>/);
}
```


```txt
Lowercase: a b c d e f g h i j k l m n o p q r s t u v w x y z µ ß à á â ã ä å æ ç è é ê ë ì í î ï ð ñ ò ó ô õ ö ø ù ú û ü ý þ ÿ ā ă ą ć ĉ ċ č ď đ ē ĕ ė ę ě ĝ ğ ġ ģ ĥ ħ ĩ ī ĭ į ı ĳ ĵ ķ ĸ ĺ ļ ľ ŀ ł ń ņ ň ŉ ŋ ō ŏ ő œ ŕ ŗ ř ś ŝ ş š ţ ť ŧ ũ ū ŭ ů ű ų ŵ ŷ ź ż ž ſ ƀ ƃ ƅ ƈ ƌ ƍ ƒ ƕ ƙ ƚ ƛ ƞ ơ ƣ ƥ ƨ ƪ ƫ ƭ ư ƴ ƶ ƹ ƺ ƽ ƾ ƿ ǆ ǉ ǌ ǎ ǐ ǒ ǔ ǖ ǘ ǚ ǜ ǝ ǟ ǡ ǣ ǥ ǧ ǩ ǫ ǭ ǯ ǰ ǳ ǵ ǹ ǻ ǽ ǿ ȁ ȃ ȅ ȇ ȉ ȋ ȍ ȏ ȑ ȓ ȕ ȗ ș ț ȝ ȟ ȡ ȣ ȥ ȧ ȩ ȫ ȭ ȯ ȱ ȳ ȴ ȵ ȶ ȷ ȸ ȹ ȼ ȿ ɀ ɂ ɇ ɉ ɋ ɍ ɏ ɐ ɑ ɒ ɓ ɔ ɕ ɖ ɗ ɘ ə ɚ ɛ ɜ ɝ ɞ ɟ ɠ ɡ ɢ ɣ ɤ ɥ ɦ ɧ ɨ ɩ ɪ ɫ ɬ ɭ ɮ ɯ ɰ ɱ ɲ ɳ ɴ ɵ ɶ ɷ ɸ ɹ ɺ ɻ ɼ ɽ ɾ ɿ ʀ ʁ ʂ ʃ ʄ ʅ ʆ ʇ ʈ ʉ ʊ ʋ ʌ ʍ ʎ ʏ ʐ ʑ ʒ ʓ ʕ ʖ ʗ ʘ ʙ ʚ ʛ ʜ ʝ ʞ ʟ ʠ ʡ ʢ ʣ ʤ ʥ ʦ ʧ ʨ ʩ ʪ ʫ ʬ ʭ ʮ ʯ ͱ ͳ ͷ ͻ ͼ ͽ ΐ ά έ ή ί ΰ α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ ς σ τ υ φ χ ψ ω ϊ ϋ ό ύ ώ ϐ ϑ ϕ ϖ ϗ ϙ ϛ ϝ ϟ ϡ ϣ ϥ ϧ ϩ ϫ ϭ ϯ ϰ ϱ ϲ ϳ ϵ ϸ ϻ ϼ а б в г д е ж з и й к л м н о п р с т у ф х ц ч ш щ ъ ы ь э ю я ѐ ё ђ ѓ є ѕ і ї ј љ њ ћ ќ ѝ ў џ ѡ ѣ ѥ ѧ ѩ ѫ ѭ ѯ ѱ ѳ ѵ ѷ ѹ ѻ ѽ ѿ ҁ ҋ ҍ ҏ ґ ғ ҕ җ ҙ қ ҝ ҟ ҡ ң ҥ ҧ ҩ ҫ ҭ ү ұ ҳ ҵ ҷ ҹ һ ҽ ҿ ӂ ӄ ӆ ӈ ӊ ӌ ӎ ӏ ӑ ӓ ӕ ӗ ә ӛ ӝ ӟ ӡ ӣ ӥ ӧ ө ӫ ӭ ӯ ӱ ӳ ӵ ӷ ӹ ӻ ӽ ӿ ԁ ԃ ԅ ԇ ԉ ԋ ԍ ԏ ԑ ԓ ԕ ԗ ԙ ԛ ԝ ԟ ԡ ԣ ԥ ԧ ա բ գ դ ե զ է ը թ ժ ի լ խ ծ կ հ ձ ղ ճ մ յ ն շ ո չ պ ջ ռ ս վ տ ր ց ւ փ ք օ ֆ և ᴀ ᴁ ᴂ ᴃ ᴄ ᴅ ᴆ ᴇ ᴈ ᴉ ᴊ ᴋ ᴌ ᴍ ᴎ ᴏ ᴐ ᴑ ᴒ ᴓ ᴔ ᴕ ᴖ ᴗ ᴘ ᴙ ᴚ ᴛ ᴜ ᴝ ᴞ ᴟ ᴠ ᴡ ᴢ ᴣ ᴤ ᴥ ᴦ ᴧ ᴨ ᴩ ᴪ ᴫ ᵫ ᵬ ᵭ ᵮ ᵯ ᵰ ᵱ ᵲ ᵳ ᵴ ᵵ ᵶ ᵷ ᵹ ᵺ ᵻ ᵼ ᵽ ᵾ ᵿ ᶀ ᶁ ᶂ ᶃ ᶄ ᶅ ᶆ ᶇ ᶈ ᶉ ᶊ ᶋ ᶌ ᶍ ᶎ ᶏ ᶐ ᶑ ᶒ ᶓ ᶔ ᶕ ᶖ ᶗ ᶘ ᶙ ᶚ ḁ ḃ ḅ ḇ ḉ ḋ ḍ ḏ ḑ ḓ ḕ ḗ ḙ ḛ ḝ ḟ ḡ ḣ ḥ ḧ ḩ ḫ ḭ ḯ ḱ ḳ ḵ ḷ ḹ ḻ ḽ ḿ ṁ ṃ ṅ ṇ ṉ ṋ ṍ ṏ ṑ ṓ ṕ ṗ ṙ ṛ ṝ ṟ ṡ ṣ ṥ ṧ ṩ ṫ ṭ ṯ ṱ ṳ ṵ ṷ ṹ ṻ ṽ ṿ ẁ ẃ ẅ ẇ ẉ ẋ ẍ ẏ ẑ ẓ ẕ ẖ ẗ ẘ ẙ ẚ ẛ ẜ ẝ ẟ ạ ả ấ ầ ẩ ẫ ậ ắ ằ ẳ ẵ ặ ẹ ẻ ẽ ế ề ể ễ ệ ỉ ị ọ ỏ ố ồ ổ ỗ ộ ớ ờ ở ỡ ợ ụ ủ ứ ừ ử ữ ự ỳ ỵ ỷ ỹ ỻ ỽ ỿ ἀ ἁ ἂ ἃ ἄ ἅ ἆ ἇ ἐ ἑ ἒ ἓ ἔ ἕ ἠ ἡ ἢ ἣ ἤ ἥ ἦ ἧ ἰ ἱ ἲ ἳ ἴ ἵ ἶ ἷ ὀ ὁ ὂ ὃ ὄ ὅ ὐ ὑ ὒ ὓ ὔ ὕ ὖ ὗ ὠ ὡ ὢ ὣ ὤ ὥ ὦ ὧ ὰ ά ὲ έ ὴ ή ὶ ί ὸ ό ὺ ύ ὼ ώ ᾀ ᾁ ᾂ ᾃ ᾄ ᾅ ᾆ ᾇ ᾐ ᾑ ᾒ ᾓ ᾔ ᾕ ᾖ ᾗ ᾠ ᾡ ᾢ ᾣ ᾤ ᾥ ᾦ ᾧ ᾰ ᾱ ᾲ ᾳ ᾴ ᾶ ᾷ ι ῂ ῃ ῄ ῆ ῇ ῐ ῑ ῒ ΐ ῖ ῗ ῠ ῡ ῢ ΰ ῤ ῥ ῦ ῧ ῲ ῳ ῴ ῶ ῷ ℊ ℎ ℏ ℓ ℯ ℴ ℹ ℼ ℽ ⅆ ⅇ ⅈ ⅉ ⅎ ↄ ⰰ ⰱ ⰲ ⰳ ⰴ ⰵ ⰶ ⰷ ⰸ ⰹ ⰺ ⰻ ⰼ ⰽ ⰾ ⰿ ⱀ ⱁ ⱂ ⱃ ⱄ ⱅ ⱆ ⱇ ⱈ ⱉ ⱊ ⱋ ⱌ ⱍ ⱎ ⱏ ⱐ ⱑ ⱒ ⱓ ⱔ ⱕ ⱖ ⱗ ⱘ ⱙ ⱚ ⱛ ⱜ ⱝ ⱞ ⱡ ⱥ ⱦ ⱨ ⱪ ⱬ ⱱ ⱳ ⱴ ⱶ ⱷ ⱸ ⱹ ⱺ ⱻ ⲁ ⲃ ⲅ ⲇ ⲉ ⲋ ⲍ ⲏ ⲑ ⲓ ⲕ ⲗ ⲙ ⲛ ⲝ ⲟ ⲡ ⲣ ⲥ ⲧ ⲩ ⲫ ⲭ ⲯ ⲱ ⲳ ⲵ ⲷ ⲹ ⲻ ⲽ ⲿ ⳁ ⳃ ⳅ ⳇ ⳉ ⳋ ⳍ ⳏ ⳑ ⳓ ⳕ ⳗ ⳙ ⳛ ⳝ ⳟ ⳡ ⳣ ⳤ ⳬ ⳮ ⳳ ⴀ ⴁ ⴂ ⴃ ⴄ ⴅ ⴆ ⴇ ⴈ ⴉ ⴊ ⴋ ⴌ ⴍ ⴎ ⴏ ⴐ ⴑ ⴒ ⴓ ⴔ ⴕ ⴖ ⴗ ⴘ ⴙ ⴚ ⴛ ⴜ ⴝ ⴞ ⴟ ⴠ ⴡ ⴢ ⴣ ⴤ ⴥ ⴧ ⴭ ꙁ ꙃ ꙅ ꙇ ꙉ ꙋ ꙍ ꙏ ꙑ ꙓ ꙕ ꙗ ꙙ ꙛ ꙝ ꙟ ꙡ ꙣ ꙥ ꙧ ꙩ ꙫ ꙭ ꚁ ꚃ ꚅ ꚇ ꚉ ꚋ ꚍ ꚏ ꚑ ꚓ ꚕ ꚗ ꜣ ꜥ ꜧ ꜩ ꜫ ꜭ ꜯ ꜰ ꜱ ꜳ ꜵ ꜷ ꜹ ꜻ ꜽ ꜿ ꝁ ꝃ ꝅ ꝇ ꝉ ꝋ ꝍ ꝏ ꝑ ꝓ ꝕ ꝗ ꝙ ꝛ ꝝ ꝟ ꝡ ꝣ ꝥ ꝧ ꝩ ꝫ ꝭ ꝯ ꝱ ꝲ ꝳ ꝴ ꝵ ꝶ ꝷ ꝸ ꝺ ꝼ ꝿ ꞁ ꞃ ꞅ ꞇ ꞌ ꞎ ꞑ ꞓ ꞡ ꞣ ꞥ ꞧ ꞩ ꟺ ﬀ ﬁ ﬂ ﬃ ﬄ ﬅ ﬆ ﬓ ﬔ ﬕ ﬖ ﬗ ａ ｂ ｃ ｄ ｅ ｆ ｇ ｈ ｉ ｊ ｋ ｌ ｍ ｎ ｏ ｐ ｑ ｒ ｓ ｔ ｕ ｖ ｗ ｘ ｙ ｚ 𐐨 𐐩 𐐪 𐐫 𐐬 𐐭 𐐮 𐐯 𐐰 𐐱 𐐲 𐐳 𐐴 𐐵 𐐶 𐐷 𐐸 𐐹 𐐺 𐐻 𐐼 𐐽 𐐾 𐐿 𐑀 𐑁 𐑂 𐑃 𐑄 𐑅 𐑆 𐑇 𐑈 𐑉 𐑊 𐑋 𐑌 𐑍 𐑎 𐑏 𝐚 𝐛 𝐜 𝐝 𝐞 𝐟 𝐠 𝐡 𝐢 𝐣 𝐤 𝐥 𝐦 𝐧 𝐨 𝐩 𝐪 𝐫 𝐬 𝐭 𝐮 𝐯 𝐰 𝐱 𝐲 𝐳 𝑎 𝑏 𝑐 𝑑 𝑒 𝑓 𝑔 𝑖 𝑗 𝑘 𝑙 𝑚 𝑛 𝑜 𝑝 𝑞 𝑟 𝑠 𝑡 𝑢 𝑣 𝑤 𝑥 𝑦 𝑧 𝒂 𝒃 𝒄 𝒅 𝒆 𝒇 𝒈 𝒉 𝒊 𝒋 𝒌 𝒍 𝒎 𝒏 𝒐 𝒑 𝒒 𝒓 𝒔 𝒕 𝒖 𝒗 𝒘 𝒙 𝒚 𝒛 𝒶 𝒷 𝒸 𝒹 𝒻 𝒽 𝒾 𝒿 𝓀 𝓁 𝓂 𝓃 𝓅 𝓆 𝓇 𝓈 𝓉 𝓊 𝓋 𝓌 𝓍 𝓎 𝓏 𝓪 𝓫 𝓬 𝓭 𝓮 𝓯 𝓰 𝓱 𝓲 𝓳 𝓴 𝓵 𝓶 𝓷 𝓸 𝓹 𝓺 𝓻 𝓼 𝓽 𝓾 𝓿 𝔀 𝔁 𝔂 𝔃 𝔞 𝔟 𝔠 𝔡 𝔢 𝔣 𝔤 𝔥 𝔦 𝔧 𝔨 𝔩 𝔪 𝔫 𝔬 𝔭 𝔮 𝔯 𝔰 𝔱 𝔲 𝔳 𝔴 𝔵 𝔶 𝔷 𝕒 𝕓 𝕔 𝕕 𝕖 𝕗 𝕘 𝕙 𝕚 𝕛 𝕜 𝕝 𝕞 𝕟 𝕠 𝕡 𝕢 𝕣 𝕤 𝕥 𝕦 𝕧 𝕨 𝕩 𝕪 𝕫 𝖆 𝖇 𝖈 𝖉 𝖊 𝖋 𝖌 𝖍 𝖎 𝖏 𝖐 𝖑 𝖒 𝖓 𝖔 𝖕 𝖖 𝖗 𝖘 𝖙 𝖚 𝖛 𝖜 𝖝 𝖞 𝖟 𝖺 𝖻 𝖼 𝖽 𝖾 𝖿 𝗀 𝗁 𝗂 𝗃 𝗄 𝗅 𝗆 𝗇 𝗈 𝗉 𝗊 𝗋 𝗌 𝗍 𝗎 𝗏 𝗐 𝗑 𝗒 𝗓 𝗮 𝗯 𝗰 𝗱 𝗲 𝗳 𝗴 𝗵 𝗶 𝗷 𝗸 𝗹 𝗺 𝗻 𝗼 𝗽 𝗾 𝗿 𝘀 𝘁 𝘂 𝘃 𝘄 𝘅 𝘆 𝘇 𝘢 𝘣 𝘤 𝘥 𝘦 𝘧 𝘨 𝘩 𝘪 𝘫 𝘬 𝘭 𝘮 𝘯 𝘰 𝘱 𝘲 𝘳 𝘴 𝘵 𝘶 𝘷 𝘸 𝘹 𝘺 𝘻 𝙖 𝙗 𝙘 𝙙 𝙚 𝙛 𝙜 𝙝 𝙞 𝙟 𝙠 𝙡 𝙢 𝙣 𝙤 𝙥 𝙦 𝙧 𝙨 𝙩 𝙪 𝙫 𝙬 𝙭 𝙮 𝙯 𝚊 𝚋 𝚌 𝚍 𝚎 𝚏 𝚐 𝚑 𝚒 𝚓 𝚔 𝚕 𝚖 𝚗 𝚘 𝚙 𝚚 𝚛 𝚜 𝚝 𝚞 𝚟 𝚠 𝚡 𝚢 𝚣 𝚤 𝚥 𝛂 𝛃 𝛄 𝛅 𝛆 𝛇 𝛈 𝛉 𝛊 𝛋 𝛌 𝛍 𝛎 𝛏 𝛐 𝛑 𝛒 𝛓 𝛔 𝛕 𝛖 𝛗 𝛘 𝛙 𝛚 𝛜 𝛝 𝛞 𝛟 𝛠 𝛡 𝛼 𝛽 𝛾 𝛿 𝜀 𝜁 𝜂 𝜃 𝜄 𝜅 𝜆 𝜇 𝜈 𝜉 𝜊 𝜋 𝜌 𝜍 𝜎 𝜏 𝜐 𝜑 𝜒 𝜓 𝜔 𝜖 𝜗 𝜘 𝜙 𝜚 𝜛 𝜶 𝜷 𝜸 𝜹 𝜺 𝜻 𝜼 𝜽 𝜾 𝜿 𝝀 𝝁 𝝂 𝝃 𝝄 𝝅 𝝆 𝝇 𝝈 𝝉 𝝊 𝝋 𝝌 𝝍 𝝎 𝝐 𝝑 𝝒 𝝓 𝝔 𝝕 𝝰 𝝱 𝝲 𝝳 𝝴 𝝵 𝝶 𝝷 𝝸 𝝹 𝝺 𝝻 𝝼 𝝽 𝝾 𝝿 𝞀 𝞁 𝞂 𝞃 𝞄 𝞅 𝞆 𝞇 𝞈 𝞊 𝞋 𝞌 𝞍 𝞎 𝞏 𝞪 𝞫 𝞬 𝞭 𝞮 𝞯 𝞰 𝞱 𝞲 𝞳 𝞴 𝞵 𝞶 𝞷 𝞸 𝞹 𝞺 𝞻 𝞼 𝞽 𝞾 𝞿 𝟀 𝟁 𝟂 𝟄 𝟅 𝟆 𝟇 𝟈 𝟉 𝟋
Uppercase: A B C D E F G H I J K L M N O P Q R S T U V W X Y Z À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï Ð Ñ Ò Ó Ô Õ Ö Ø Ù Ú Û Ü Ý Þ Ā Ă Ą Ć Ĉ Ċ Č Ď Đ Ē Ĕ Ė Ę Ě Ĝ Ğ Ġ Ģ Ĥ Ħ Ĩ Ī Ĭ Į İ Ĳ Ĵ Ķ Ĺ Ļ Ľ Ŀ Ł Ń Ņ Ň Ŋ Ō Ŏ Ő Œ Ŕ Ŗ Ř Ś Ŝ Ş Š Ţ Ť Ŧ Ũ Ū Ŭ Ů Ű Ų Ŵ Ŷ Ÿ Ź Ż Ž Ɓ Ƃ Ƅ Ɔ Ƈ Ɖ Ɗ Ƌ Ǝ Ə Ɛ Ƒ Ɠ Ɣ Ɩ Ɨ Ƙ Ɯ Ɲ Ɵ Ơ Ƣ Ƥ Ʀ Ƨ Ʃ Ƭ Ʈ Ư Ʊ Ʋ Ƴ Ƶ Ʒ Ƹ Ƽ Ǆ Ǉ Ǌ Ǎ Ǐ Ǒ Ǔ Ǖ Ǘ Ǚ Ǜ Ǟ Ǡ Ǣ Ǥ Ǧ Ǩ Ǫ Ǭ Ǯ Ǳ Ǵ Ƕ Ƿ Ǹ Ǻ Ǽ Ǿ Ȁ Ȃ Ȅ Ȇ Ȉ Ȋ Ȍ Ȏ Ȑ Ȓ Ȕ Ȗ Ș Ț Ȝ Ȟ Ƞ Ȣ Ȥ Ȧ Ȩ Ȫ Ȭ Ȯ Ȱ Ȳ Ⱥ Ȼ Ƚ Ⱦ Ɂ Ƀ Ʉ Ʌ Ɇ Ɉ Ɋ Ɍ Ɏ Ͱ Ͳ Ͷ Ά Έ Ή Ί Ό Ύ Ώ Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο Π Ρ Σ Τ Υ Φ Χ Ψ Ω Ϊ Ϋ Ϗ ϒ ϓ ϔ Ϙ Ϛ Ϝ Ϟ Ϡ Ϣ Ϥ Ϧ Ϩ Ϫ Ϭ Ϯ ϴ Ϸ Ϲ Ϻ Ͻ Ͼ Ͽ Ѐ Ё Ђ Ѓ Є Ѕ І Ї Ј Љ Њ Ћ Ќ Ѝ Ў Џ А Б В Г Д Е Ж З И Й К Л М Н О П Р С Т У Ф Х Ц Ч Ш Щ Ъ Ы Ь Э Ю Я Ѡ Ѣ Ѥ Ѧ Ѩ Ѫ Ѭ Ѯ Ѱ Ѳ Ѵ Ѷ Ѹ Ѻ Ѽ Ѿ Ҁ Ҋ Ҍ Ҏ Ґ Ғ Ҕ Җ Ҙ Қ Ҝ Ҟ Ҡ Ң Ҥ Ҧ Ҩ Ҫ Ҭ Ү Ұ Ҳ Ҵ Ҷ Ҹ Һ Ҽ Ҿ Ӏ Ӂ Ӄ Ӆ Ӈ Ӊ Ӌ Ӎ Ӑ Ӓ Ӕ Ӗ Ә Ӛ Ӝ Ӟ Ӡ Ӣ Ӥ Ӧ Ө Ӫ Ӭ Ӯ Ӱ Ӳ Ӵ Ӷ Ӹ Ӻ Ӽ Ӿ Ԁ Ԃ Ԅ Ԇ Ԉ Ԋ Ԍ Ԏ Ԑ Ԓ Ԕ Ԗ Ԙ Ԛ Ԝ Ԟ Ԡ Ԣ Ԥ Ԧ Ա Բ Գ Դ Ե Զ Է Ը Թ Ժ Ի Լ Խ Ծ Կ Հ Ձ Ղ Ճ Մ Յ Ն Շ Ո Չ Պ Ջ Ռ Ս Վ Տ Ր Ց Ւ Փ Ք Օ Ֆ Ⴀ Ⴁ Ⴂ Ⴃ Ⴄ Ⴅ Ⴆ Ⴇ Ⴈ Ⴉ Ⴊ Ⴋ Ⴌ Ⴍ Ⴎ Ⴏ Ⴐ Ⴑ Ⴒ Ⴓ Ⴔ Ⴕ Ⴖ Ⴗ Ⴘ Ⴙ Ⴚ Ⴛ Ⴜ Ⴝ Ⴞ Ⴟ Ⴠ Ⴡ Ⴢ Ⴣ Ⴤ Ⴥ Ⴧ Ⴭ Ḁ Ḃ Ḅ Ḇ Ḉ Ḋ Ḍ Ḏ Ḑ Ḓ Ḕ Ḗ Ḙ Ḛ Ḝ Ḟ Ḡ Ḣ Ḥ Ḧ Ḩ Ḫ Ḭ Ḯ Ḱ Ḳ Ḵ Ḷ Ḹ Ḻ Ḽ Ḿ Ṁ Ṃ Ṅ Ṇ Ṉ Ṋ Ṍ Ṏ Ṑ Ṓ Ṕ Ṗ Ṙ Ṛ Ṝ Ṟ Ṡ Ṣ Ṥ Ṧ Ṩ Ṫ Ṭ Ṯ Ṱ Ṳ Ṵ Ṷ Ṹ Ṻ Ṽ Ṿ Ẁ Ẃ Ẅ Ẇ Ẉ Ẋ Ẍ Ẏ Ẑ Ẓ Ẕ ẞ Ạ Ả Ấ Ầ Ẩ Ẫ Ậ Ắ Ằ Ẳ Ẵ Ặ Ẹ Ẻ Ẽ Ế Ề Ể Ễ Ệ Ỉ Ị Ọ Ỏ Ố Ồ Ổ Ỗ Ộ Ớ Ờ Ở Ỡ Ợ Ụ Ủ Ứ Ừ Ử Ữ Ự Ỳ Ỵ Ỷ Ỹ Ỻ Ỽ Ỿ Ἀ Ἁ Ἂ Ἃ Ἄ Ἅ Ἆ Ἇ Ἐ Ἑ Ἒ Ἓ Ἔ Ἕ Ἠ Ἡ Ἢ Ἣ Ἤ Ἥ Ἦ Ἧ Ἰ Ἱ Ἲ Ἳ Ἴ Ἵ Ἶ Ἷ Ὀ Ὁ Ὂ Ὃ Ὄ Ὅ Ὑ Ὓ Ὕ Ὗ Ὠ Ὡ Ὢ Ὣ Ὤ Ὥ Ὦ Ὧ Ᾰ Ᾱ Ὰ Ά Ὲ Έ Ὴ Ή Ῐ Ῑ Ὶ Ί Ῠ Ῡ Ὺ Ύ Ῥ Ὸ Ό Ὼ Ώ ℂ ℇ ℋ ℌ ℍ ℐ ℑ ℒ ℕ ℙ ℚ ℛ ℜ ℝ ℤ Ω ℨ K Å ℬ ℭ ℰ ℱ Ⅎ ℳ ℾ ℿ ⅅ Ↄ Ⰰ Ⰱ Ⰲ Ⰳ Ⰴ Ⰵ Ⰶ Ⰷ Ⰸ Ⰹ Ⰺ Ⰻ Ⰼ Ⰽ Ⰾ Ⰿ Ⱀ Ⱁ Ⱂ Ⱃ Ⱄ Ⱅ Ⱆ Ⱇ Ⱈ Ⱉ Ⱊ Ⱋ Ⱌ Ⱍ Ⱎ Ⱏ Ⱐ Ⱑ Ⱒ Ⱓ Ⱔ Ⱕ Ⱖ Ⱗ Ⱘ Ⱙ Ⱚ Ⱛ Ⱜ Ⱝ Ⱞ Ⱡ Ɫ Ᵽ Ɽ Ⱨ Ⱪ Ⱬ Ɑ Ɱ Ɐ Ɒ Ⱳ Ⱶ Ȿ Ɀ Ⲁ Ⲃ Ⲅ Ⲇ Ⲉ Ⲋ Ⲍ Ⲏ Ⲑ Ⲓ Ⲕ Ⲗ Ⲙ Ⲛ Ⲝ Ⲟ Ⲡ Ⲣ Ⲥ Ⲧ Ⲩ Ⲫ Ⲭ Ⲯ Ⲱ Ⲳ Ⲵ Ⲷ Ⲹ Ⲻ Ⲽ Ⲿ Ⳁ Ⳃ Ⳅ Ⳇ Ⳉ Ⳋ Ⳍ Ⳏ Ⳑ Ⳓ Ⳕ Ⳗ Ⳙ Ⳛ Ⳝ Ⳟ Ⳡ Ⳣ Ⳬ Ⳮ Ⳳ Ꙁ Ꙃ Ꙅ Ꙇ Ꙉ Ꙋ Ꙍ Ꙏ Ꙑ Ꙓ Ꙕ Ꙗ Ꙙ Ꙛ Ꙝ Ꙟ Ꙡ Ꙣ Ꙥ Ꙧ Ꙩ Ꙫ Ꙭ Ꚁ Ꚃ Ꚅ Ꚇ Ꚉ Ꚋ Ꚍ Ꚏ Ꚑ Ꚓ Ꚕ Ꚗ Ꜣ Ꜥ Ꜧ Ꜩ Ꜫ Ꜭ Ꜯ Ꜳ Ꜵ Ꜷ Ꜹ Ꜻ Ꜽ Ꜿ Ꝁ Ꝃ Ꝅ Ꝇ Ꝉ Ꝋ Ꝍ Ꝏ Ꝑ Ꝓ Ꝕ Ꝗ Ꝙ Ꝛ Ꝝ Ꝟ Ꝡ Ꝣ Ꝥ Ꝧ Ꝩ Ꝫ Ꝭ Ꝯ Ꝺ Ꝼ Ᵹ Ꝿ Ꞁ Ꞃ Ꞅ Ꞇ Ꞌ Ɥ Ꞑ Ꞓ Ꞡ Ꞣ Ꞥ Ꞧ Ꞩ Ɦ Ａ Ｂ Ｃ Ｄ Ｅ Ｆ Ｇ Ｈ Ｉ Ｊ Ｋ Ｌ Ｍ Ｎ Ｏ Ｐ Ｑ Ｒ Ｓ Ｔ Ｕ Ｖ Ｗ Ｘ Ｙ Ｚ 𐐀 𐐁 𐐂 𐐃 𐐄 𐐅 𐐆 𐐇 𐐈 𐐉 𐐊 𐐋 𐐌 𐐍 𐐎 𐐏 𐐐 𐐑 𐐒 𐐓 𐐔 𐐕 𐐖 𐐗 𐐘 𐐙 𐐚 𐐛 𐐜 𐐝 𐐞 𐐟 𐐠 𐐡 𐐢 𐐣 𐐤 𐐥 𐐦 𐐧 𝐀 𝐁 𝐂 𝐃 𝐄 𝐅 𝐆 𝐇 𝐈 𝐉 𝐊 𝐋 𝐌 𝐍 𝐎 𝐏 𝐐 𝐑 𝐒 𝐓 𝐔 𝐕 𝐖 𝐗 𝐘 𝐙 𝐴 𝐵 𝐶 𝐷 𝐸 𝐹 𝐺 𝐻 𝐼 𝐽 𝐾 𝐿 𝑀 𝑁 𝑂 𝑃 𝑄 𝑅 𝑆 𝑇 𝑈 𝑉 𝑊 𝑋 𝑌 𝑍 𝑨 𝑩 𝑪 𝑫 𝑬 𝑭 𝑮 𝑯 𝑰 𝑱 𝑲 𝑳 𝑴 𝑵 𝑶 𝑷 𝑸 𝑹 𝑺 𝑻 𝑼 𝑽 𝑾 𝑿 𝒀 𝒁 𝒜 𝒞 𝒟 𝒢 𝒥 𝒦 𝒩 𝒪 𝒫 𝒬 𝒮 𝒯 𝒰 𝒱 𝒲 𝒳 𝒴 𝒵 𝓐 𝓑 𝓒 𝓓 𝓔 𝓕 𝓖 𝓗 𝓘 𝓙 𝓚 𝓛 𝓜 𝓝 𝓞 𝓟 𝓠 𝓡 𝓢 𝓣 𝓤 𝓥 𝓦 𝓧 𝓨 𝓩 𝔄 𝔅 𝔇 𝔈 𝔉 𝔊 𝔍 𝔎 𝔏 𝔐 𝔑 𝔒 𝔓 𝔔 𝔖 𝔗 𝔘 𝔙 𝔚 𝔛 𝔜 𝔸 𝔹 𝔻 𝔼 𝔽 𝔾 𝕀 𝕁 𝕂 𝕃 𝕄 𝕆 𝕊 𝕋 𝕌 𝕍 𝕎 𝕏 𝕐 𝕬 𝕭 𝕮 𝕯 𝕰 𝕱 𝕲 𝕳 𝕴 𝕵 𝕶 𝕷 𝕸 𝕹 𝕺 𝕻 𝕼 𝕽 𝕾 𝕿 𝖀 𝖁 𝖂 𝖃 𝖄 𝖅 𝖠 𝖡 𝖢 𝖣 𝖤 𝖥 𝖦 𝖧 𝖨 𝖩 𝖪 𝖫 𝖬 𝖭 𝖮 𝖯 𝖰 𝖱 𝖲 𝖳 𝖴 𝖵 𝖶 𝖷 𝖸 𝖹 𝗔 𝗕 𝗖 𝗗 𝗘 𝗙 𝗚 𝗛 𝗜 𝗝 𝗞 𝗟 𝗠 𝗡 𝗢 𝗣 𝗤 𝗥 𝗦 𝗧 𝗨 𝗩 𝗪 𝗫 𝗬 𝗭 𝘈 𝘉 𝘊 𝘋 𝘌 𝘍 𝘎 𝘏 𝘐 𝘑 𝘒 𝘓 𝘔 𝘕 𝘖 𝘗 𝘘 𝘙 𝘚 𝘛 𝘜 𝘝 𝘞 𝘟 𝘠 𝘡 𝘼 𝘽 𝘾 𝘿 𝙀 𝙁 𝙂 𝙃 𝙄 𝙅 𝙆 𝙇 𝙈 𝙉 𝙊 𝙋 𝙌 𝙍 𝙎 𝙏 𝙐 𝙑 𝙒 𝙓 𝙔 𝙕 𝙰 𝙱 𝙲 𝙳 𝙴 𝙵 𝙶 𝙷 𝙸 𝙹 𝙺 𝙻 𝙼 𝙽 𝙾 𝙿 𝚀 𝚁 𝚂 𝚃 𝚄 𝚅 𝚆 𝚇 𝚈 𝚉 𝚨 𝚩 𝚪 𝚫 𝚬 𝚭 𝚮 𝚯 𝚰 𝚱 𝚲 𝚳 𝚴 𝚵 𝚶 𝚷 𝚸 𝚹 𝚺 𝚻 𝚼 𝚽 𝚾 𝚿 𝛀 𝛢 𝛣 𝛤 𝛥 𝛦 𝛧 𝛨 𝛩 𝛪 𝛫 𝛬 𝛭 𝛮 𝛯 𝛰 𝛱 𝛲 𝛳 𝛴 𝛵 𝛶 𝛷 𝛸 𝛹 𝛺 𝜜 𝜝 𝜞 𝜟 𝜠 𝜡 𝜢 𝜣 𝜤 𝜥 𝜦 𝜧 𝜨 𝜩 𝜪 𝜫 𝜬 𝜭 𝜮 𝜯 𝜰 𝜱 𝜲 𝜳 𝜴 𝝖 𝝗 𝝘 𝝙 𝝚 𝝛 𝝜 𝝝 𝝞 𝝟 𝝠 𝝡 𝝢 𝝣 𝝤 𝝥 𝝦 𝝧 𝝨 𝝩 𝝪 𝝫 𝝬 𝝭 𝝮 𝞐 𝞑 𝞒 𝞓 𝞔 𝞕 𝞖 𝞗 𝞘 𝞙 𝞚 𝞛 𝞜 𝞝 𝞞 𝞟 𝞠 𝞡 𝞢 𝞣 𝞤 𝞥 𝞦 𝞧 𝞨 𝟊
Titlecase: ǅ ǈ ǋ ǲ ᾈ ᾉ ᾊ ᾋ ᾌ ᾍ ᾎ ᾏ ᾘ ᾙ ᾚ ᾛ ᾜ ᾝ ᾞ ᾟ ᾨ ᾩ ᾪ ᾫ ᾬ ᾭ ᾮ ᾯ ᾼ ῌ ῼ
```



## Phix

Phix supports utf8 source files, so theoretically (but not necessarily in practice) identifiers can contain any utf8-representable characters.

The compiler source ptok.e contains:

```Phix
charset['A'..'Z'] = LETTER
charset[#80] = LETTER   -- more unicode
charset[#88] = LETTER   -- more unicode
charset[#94] = LETTER   -- for rosettacode/unicode (as ptok.e is not stored in utf8)
charset[#9A] = LETTER   -- for rosettacode/unicode
charset[#A3] = LETTER   -- for rosettacode/unicode
charset[#BB] = LETTER   -- for rosettacode/unicode
charset[#CE] = LETTER   -- for rosettacode/unicode
charset[#CF] = LETTER
charset[#E2] = LETTER
charset['_'] = LETTER
charset['a'..'z'] = LETTER
```

So the language itself supports A-Z, a-z, and a handful of ad-hoc non-Latin characters, which can easily be extended.

The builtin routines islower and isupper (see builtins\pcase.e and/or builtins\pcase8.e) currently test values in the range 1..255 thus:

```Phix
sequence lc = {}, uc = {}
for ch=1 to 255 do
    if islower(ch) then lc &= ch end if
    if isupper(ch) then uc &= ch end if
end for
lc = utf32_to_utf8(lc)&"\n"
uc = utf32_to_utf8(uc)&"\n"
puts(1,lc)
puts(1,uc)
```

```txt

abcdefghijklmnopqrstuvwxyzàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿ
ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞ

```

That code is now part of demo\HelloUTF8.exw, which has the necessary Windows console code page setting code, though it has
clearly fouled up on 2 or 3 characters somehow - probably quite easy to fix, should you care enough. (I also tried displaying
them in a MessageBox, which hid the 2/3 glitches, but got exactly the same results on copy and paste, btw.)


## Python

Python defines [http://docs.python.org/3.1/library/stdtypes.html#str.isalnum eleven string classes] for the Unicode characters in the range 0 to 0x10FFFF which include lowercase and uppercase. The following gives information on all the classes with the two asked for coming first but only printing out the first up to 100 characters to spare long meaningless printouts.


```python
classes = (str.isupper, str.islower, str.isalnum, str.isalpha, str.isdecimal,
           str.isdigit, str.isidentifier, str.isnumeric, str.isprintable,
           str.isspace, str.istitle)

for stringclass in classes:
    chars = ''.join(chr(i) for i in range(0x10FFFF+1) if stringclass(chr(i)))
    print('\nString class %s has %i characters the first of which are:\n  %r'
          % (stringclass.__name__, len(chars), chars[:100]))
```


```txt
String class isupper has 1483 characters the first of which are:
  'ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮİĲĴĶĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘ'

String class islower has 1934 characters the first of which are:
  'abcdefghijklmnopqrstuvwxyzªµºßàáâãäåæçèéêëìíîïðñòóôõöøùúûüýþÿāăąćĉċčďđēĕėęěĝğġģĥħĩīĭįıĳĵķĸĺļľŀłńņňŉŋ'

String class isalnum has 102157 characters the first of which are:
  '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzª²³µ¹º¼½¾ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝ'

String class isalpha has 101013 characters the first of which are:
  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyzªµºÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëìí'

String class isdecimal has 460 characters the first of which are:
  '0123456789٠١٢٣٤٥٦٧٨٩۰۱۲۳۴۵۶۷۸۹߀߁߂߃߄߅߆߇߈߉०१२३४५६७८९০১২৩৪৫৬৭৮৯੦੧੨੩੪੫੬੭੮੯૦૧૨૩૪૫૬૭૮૯୦୧୨୩୪୫୬୭୮୯௦௧௨௩௪௫௬௭௮௯'

String class isdigit has 588 characters the first of which are:
  '0123456789²³¹٠١٢٣٤٥٦٧٨٩۰۱۲۳۴۵۶۷۸۹߀߁߂߃߄߅߆߇߈߉०१२३४५६७८९০১২৩৪৫৬৭৮৯੦੧੨੩੪੫੬੭੮੯૦૧૨૩૪૫૬૭૮૯୦୧୨୩୪୫୬୭୮୯௦௧௨௩௪௫௬'

String class isidentifier has 101218 characters the first of which are:
  'ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyzªµºÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞßàáâãäåæçèéêëì'

String class isnumeric has 1225 characters the first of which are:
  '0123456789²³¹¼½¾٠١٢٣٤٥٦٧٨٩۰۱۲۳۴۵۶۷۸۹߀߁߂߃߄߅߆߇߈߉०१२३४५६७८९০১২৩৪৫৬৭৮৯৴৵৶৷৸৹੦੧੨੩੪੫੬੭੮੯૦૧૨૩૪૫૬૭૮૯୦୧୨୩୪୫୬୭'

String class isprintable has 109958 characters the first of which are:
  ' !"#$%&\'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~¡¢£¤¥'

String class isspace has 30 characters the first of which are:
  '\t\n\x0b\x0c\r\x1c\x1d\x1e\x1f \x85\xa0\u1680\u180e\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u2028\u2029\u202f\u205f\u3000'

String class istitle has 1514 characters the first of which are:
  'ABCDEFGHIJKLMNOPQRSTUVWXYZÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖØÙÚÛÜÝÞĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮİĲĴĶĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘ'
```



## Racket

Character sets and utils are defined in [http://docs.racket-lang.org/srfi/srfi-std/srfi-14.html SRFI 14: Character-set Library].

```racket
#lang racket
(require srfi/14)
(printf "System information: ~a~%" (map system-type (list 'os 'word 'machine)))
(printf "All lowercase characters: ~a~%" (char-set->string char-set:lower-case))
(newline)
(printf "All uppercase characters: ~a~%" (char-set->string char-set:upper-case))
```


```txt
System information: (windows 64 Windows NT 6.1 (Build 7601) Service Pack 1)
All lowercase characters: 𝟋𝟉𝟈𝟇𝟆𝟅𝟄𝟂𝟁𝟀𝞿𝞾𝞽𝞼𝞻𝞺𝞹𝞸𝞷𝞶𝞵𝞴𝞳𝞲𝞱𝞰𝞯𝞮𝞭𝞬𝞫𝞪𝞏𝞎𝞍𝞌𝞋𝞊𝞈𝞇𝞆𝞅𝞄𝞃𝞂𝞁𝞀𝝿𝝾𝝽𝝼𝝻𝝺𝝹𝝸𝝷𝝶𝝵𝝴𝝳𝝲𝝱𝝰𝝕𝝔𝝓𝝒𝝑𝝐𝝎𝝍𝝌𝝋𝝊𝝉𝝈𝝇𝝆𝝅𝝄𝝃𝝂𝝁𝝀𝜿𝜾𝜽𝜼𝜻𝜺𝜹𝜸𝜷𝜶𝜛𝜚𝜙𝜘𝜗𝜖𝜔𝜓𝜒𝜑𝜐𝜏𝜎𝜍𝜌𝜋𝜊𝜉𝜈𝜇𝜆𝜅𝜄𝜃𝜂𝜁𝜀𝛿𝛾𝛽𝛼𝛡𝛠𝛟𝛞𝛝𝛜𝛚𝛙𝛘𝛗𝛖𝛕𝛔𝛓𝛒𝛑𝛐𝛏𝛎𝛍𝛌𝛋𝛊𝛉𝛈𝛇𝛆𝛅𝛄𝛃𝛂𝚥𝚤𝚣𝚢𝚡𝚠𝚟𝚞𝚝𝚜𝚛𝚚𝚙𝚘𝚗𝚖𝚕𝚔𝚓𝚒𝚑𝚐𝚏𝚎𝚍𝚌𝚋𝚊𝙯𝙮𝙭𝙬𝙫𝙪𝙩𝙨𝙧𝙦𝙥𝙤𝙣𝙢𝙡𝙠𝙟𝙞𝙝𝙜𝙛𝙚𝙙𝙘𝙗𝙖𝘻𝘺𝘹𝘸𝘷𝘶𝘵𝘴𝘳𝘲𝘱𝘰𝘯𝘮𝘭𝘬𝘫𝘪𝘩𝘨𝘧𝘦𝘥𝘤𝘣𝘢𝘇𝘆𝘅𝘄𝘃𝘂𝘁𝘀𝗿𝗾𝗽𝗼𝗻𝗺𝗹𝗸𝗷𝗶𝗵𝗴𝗳𝗲𝗱𝗰𝗯𝗮𝗓𝗒𝗑𝗐𝗏𝗎𝗍𝗌𝗋𝗊𝗉𝗈𝗇𝗆𝗅𝗄𝗃𝗂𝗁𝗀𝖿𝖾𝖽𝖼𝖻𝖺𝖟𝖞𝖝𝖜𝖛𝖚𝖙𝖘𝖗𝖖𝖕𝖔𝖓𝖒𝖑𝖐𝖏𝖎𝖍𝖌𝖋𝖊𝖉𝖈𝖇𝖆𝕫𝕪𝕩𝕨𝕧𝕦𝕥𝕤𝕣𝕢𝕡𝕠𝕟𝕞𝕝𝕜𝕛𝕚𝕙𝕘𝕗𝕖𝕕𝕔𝕓𝕒𝔷𝔶𝔵𝔴𝔳𝔲𝔱𝔰𝔯𝔮𝔭𝔬𝔫𝔪𝔩𝔨𝔧𝔦𝔥𝔤𝔣𝔢𝔡𝔠𝔟𝔞𝔃𝔂𝔁𝔀𝓿𝓾𝓽𝓼𝓻𝓺𝓹𝓸𝓷𝓶𝓵𝓴𝓳𝓲𝓱𝓰𝓯𝓮𝓭𝓬𝓫𝓪𝓏𝓎𝓍𝓌𝓋𝓊𝓉𝓈𝓇𝓆𝓅𝓃𝓂𝓁𝓀𝒿𝒾𝒽𝒻𝒹𝒸𝒷𝒶𝒛𝒚𝒙𝒘𝒗𝒖𝒕𝒔𝒓𝒒𝒑𝒐𝒏𝒎𝒍𝒌𝒋𝒊𝒉𝒈𝒇𝒆𝒅𝒄𝒃𝒂𝑧𝑦𝑥𝑤𝑣𝑢𝑡𝑠𝑟𝑞𝑝𝑜𝑛𝑚𝑙𝑘𝑗𝑖𝑔𝑓𝑒𝑑𝑐𝑏𝑎𝐳𝐲𝐱𝐰𝐯𝐮𝐭𝐬𝐫𝐪𝐩𝐨𝐧𝐦𝐥𝐤𝐣𝐢𝐡𝐠𝐟𝐞𝐝𝐜𝐛𝐚𐑏𐑎𐑍𐑌𐑋𐑊𐑉𐑈𐑇𐑆𐑅𐑄𐑃𐑂𐑁𐑀𐐿𐐾𐐽𐐼𐐻𐐺𐐹𐐸𐐷𐐶𐐵𐐴𐐳𐐲𐐱𐐰𐐯𐐮𐐭𐐬𐐫𐐪𐐩𐐨ｚｙｘｗｖｕｔｓｒｑｐｏｎｍｌｋｊｉｈｇｆｅｄｃｂａﬗﬖﬕﬔﬓﬆﬅﬄﬃﬂﬁﬀⴥⴤⴣⴢⴡⴠⴟⴞⴝⴜⴛⴚⴙⴘⴗⴖⴕⴔⴓⴒⴑⴐⴏⴎⴍⴌⴋⴊⴉⴈⴇⴆⴅⴄⴃⴂⴁⴀⳤⳣⳡⳟⳝⳛⳙⳗⳕⳓⳑⳏⳍⳋⳉⳇⳅⳃⳁⲿⲽⲻⲹⲷⲵⲳⲱⲯⲭⲫⲩⲧⲥⲣⲡⲟⲝⲛⲙⲗⲕⲓⲑⲏⲍⲋⲉⲇⲅⲃⲁⱷⱶⱴⱬⱪⱨⱦⱥⱡⱞⱝⱜⱛⱚⱙⱘⱗⱖⱕⱔⱓⱒⱑⱐⱏⱎⱍⱌⱋⱊⱉⱈⱇⱆⱅⱄⱃⱂⱁⱀⰿⰾⰽⰼⰻⰺⰹⰸⰷⰶⰵⰴⰳⰲⰱⰰⓩⓨⓧⓦⓥⓤⓣⓢⓡⓠⓟⓞⓝⓜⓛⓚⓙⓘⓗⓖⓕⓔⓓⓒⓑⓐↄⅿⅾⅽⅼⅻⅺⅹⅸⅷⅶⅵⅴⅳⅲⅱⅰⅎⅉⅈⅇⅆℽℼℹℴℯℓℏℎℊₔₓₒₑₐⁿⁱῷῶῴῳῲῧῦῥῤΰῢῡῠῗῖΐῒῑῐῇῆῄῃῂιᾷᾶᾴᾳᾲᾱᾰᾧᾦᾥᾤᾣᾢᾡᾠᾗᾖᾕᾔᾓᾒᾑᾐᾇᾆᾅᾄᾃᾂᾁᾀώὼύὺόὸίὶήὴέὲάὰὧὦὥὤὣὢὡὠὗὖὕὔὓὒὑὐὅὄὃὂὁὀἷἶἵἴἳἲἱἰἧἦἥἤἣἢἡἠἕἔἓἒἑἐἇἆἅἄἃἂἁἀỹỷỵỳựữửừứủụợỡởờớộỗổồốỏọịỉệễểềếẽẻẹặẵẳằắậẫẩầấảạẛẚẙẘẗẖẕẓẑẏẍẋẉẇẅẃẁṿṽṻṹṷṵṳṱṯṭṫṩṧṥṣṡṟṝṛṙṗṕṓṑṏṍṋṉṇṅṃṁḿḽḻḹḷḵḳḱḯḭḫḩḧḥḣḡḟḝḛḙḗḕḓḑḏḍḋḉḇḅḃḁᶿᶾᶽᶼᶻᶺᶹᶸᶷᶶᶵᶴᶳᶲᶱᶰᶯᶮᶭᶬᶫᶪᶩᶨᶧᶦᶥᶤᶣᶢᶡᶠᶟᶞᶝᶜᶛᶚᶙᶘᶗᶖᶕᶔᶓᶒᶑᶐᶏᶎᶍᶌᶋᶊᶉᶈᶇᶆᶅᶄᶃᶂᶁᶀᵿᵾᵽᵼᵻᵺᵹᵸᵷᵶᵵᵴᵳᵲᵱᵰᵯᵮᵭᵬᵫᵪᵩᵨᵧᵦᵥᵤᵣᵢᵡᵠᵟᵞᵝᵜᵛᵚᵙᵘᵗᵖᵕᵔᵓᵒᵑᵐᵏᵎᵍᵌᵋᵊᵉᵈᵇᵆᵅᵄᵃᵂᵁᵀᴿᴾᴽᴼᴻᴺᴹᴸᴷᴶᴵᴴᴳᴲᴱᴰᴯᴮᴭᴬᴫᴪᴩᴨᴧᴦᴥᴤᴣᴢᴡᴠᴟᴞᴝᴜᴛᴚᴙᴘᴗᴖᴕᴔᴓᴒᴑᴐᴏᴎᴍᴌᴋᴊᴉᴈᴇᴆᴅᴄᴃᴂᴁᴀևֆօքփւցրտվսռջպչոշնյմճղձհկծխլիժթըէզեդգբաԓԑԏԍԋԉԇԅԃԁӿӽӻӹӷӵӳӱӯӭӫөӧӥӣӡӟӝӛәӗӕӓӑӏӎӌӊӈӆӄӂҿҽһҹҷҵҳұүҭҫҩҧҥңҡҟҝқҙҗҕғґҏҍҋҁѿѽѻѹѷѵѳѱѯѭѫѩѧѥѣѡџўѝќћњљјїіѕєѓђёѐяюэьыъщшчцхфутсрпонмлкйизжедгвбаϼϻϸϵϳϲϱϰϯϭϫϩϧϥϣϡϟϝϛϙϗϖϕϑϐώύόϋϊωψχφυτσςρποξνμλκιθηζεδγβαΰίήέάΐͽͼͻͺͅˤˣˢˡˠˁˀʸʷʶʵʴʳʲʱʰʯʮʭʬʫʪʩʨʧʦʥʤʣʢʡʠʟʞʝʜʛʚʙʘʗʖʕʓʒʑʐʏʎʍʌʋʊʉʈʇʆʅʄʃʂʁʀɿɾɽɼɻɺɹɸɷɶɵɴɳɲɱɰɯɮɭɬɫɪɩɨɧɦɥɤɣɢɡɠɟɞɝɜɛɚəɘɗɖɕɔɓɒɑɐɏɍɋɉɇɂɀȿȼȹȸȷȶȵȴȳȱȯȭȫȩȧȥȣȡȟȝțșȗȕȓȑȏȍȋȉȇȅȃȁǿǽǻǹǵǳǰǯǭǫǩǧǥǣǡǟǝǜǚǘǖǔǒǐǎǌǉǆƿƾƽƺƹƶƴưƭƫƪƨƥƣơƞƛƚƙƕƒƍƌƈƅƃƀſžżźŷŵųűůŭūũŧťţšşŝśřŗŕœőŏōŋŉňņńłŀľļĺĸķĵĳıįĭīĩħĥģġğĝěęėĕēđďčċĉćąăāÿþýüûúùøöõôóòñðïîíìëêéèçæåäãâáàßºµªzyxwvutsrqponmlkjihgfedcba

All uppercase characters: 𝟊𝞨𝞧𝞦𝞥𝞤𝞣𝞢𝞡𝞠𝞟𝞞𝞝𝞜𝞛𝞚𝞙𝞘𝞗𝞖𝞕𝞔𝞓𝞒𝞑𝞐𝝮𝝭𝝬𝝫𝝪𝝩𝝨𝝧𝝦𝝥𝝤𝝣𝝢𝝡𝝠𝝟𝝞𝝝𝝜𝝛𝝚𝝙𝝘𝝗𝝖𝜴𝜳𝜲𝜱𝜰𝜯𝜮𝜭𝜬𝜫𝜪𝜩𝜨𝜧𝜦𝜥𝜤𝜣𝜢𝜡𝜠𝜟𝜞𝜝𝜜𝛺𝛹𝛸𝛷𝛶𝛵𝛴𝛳𝛲𝛱𝛰𝛯𝛮𝛭𝛬𝛫𝛪𝛩𝛨𝛧𝛦𝛥𝛤𝛣𝛢𝛀𝚿𝚾𝚽𝚼𝚻𝚺𝚹𝚸𝚷𝚶𝚵𝚴𝚳𝚲𝚱𝚰𝚯𝚮𝚭𝚬𝚫𝚪𝚩𝚨𝚉𝚈𝚇𝚆𝚅𝚄𝚃𝚂𝚁𝚀𝙿𝙾𝙽𝙼𝙻𝙺𝙹𝙸𝙷𝙶𝙵𝙴𝙳𝙲𝙱𝙰𝙕𝙔𝙓𝙒𝙑𝙐𝙏𝙎𝙍𝙌𝙋𝙊𝙉𝙈𝙇𝙆𝙅𝙄𝙃𝙂𝙁𝙀𝘿𝘾𝘽𝘼𝘡𝘠𝘟𝘞𝘝𝘜𝘛𝘚𝘙𝘘𝘗𝘖𝘕𝘔𝘓𝘒𝘑𝘐𝘏𝘎𝘍𝘌𝘋𝘊𝘉𝘈𝗭𝗬𝗫𝗪𝗩𝗨𝗧𝗦𝗥𝗤𝗣𝗢𝗡𝗠𝗟𝗞𝗝𝗜𝗛𝗚𝗙𝗘𝗗𝗖𝗕𝗔𝖹𝖸𝖷𝖶𝖵𝖴𝖳𝖲𝖱𝖰𝖯𝖮𝖭𝖬𝖫𝖪𝖩𝖨𝖧𝖦𝖥𝖤𝖣𝖢𝖡𝖠𝖅𝖄𝖃𝖂𝖁𝖀𝕿𝕾𝕽𝕼𝕻𝕺𝕹𝕸𝕷𝕶𝕵𝕴𝕳𝕲𝕱𝕰𝕯𝕮𝕭𝕬𝕐𝕏𝕎𝕍𝕌𝕋𝕊𝕆𝕄𝕃𝕂𝕁𝕀𝔾𝔽𝔼𝔻𝔹𝔸𝔜𝔛𝔚𝔙𝔘𝔗𝔖𝔔𝔓𝔒𝔑𝔐𝔏𝔎𝔍𝔊𝔉𝔈𝔇𝔅𝔄𝓩𝓨𝓧𝓦𝓥𝓤𝓣𝓢𝓡𝓠𝓟𝓞𝓝𝓜𝓛𝓚𝓙𝓘𝓗𝓖𝓕𝓔𝓓𝓒𝓑𝓐𝒵𝒴𝒳𝒲𝒱𝒰𝒯𝒮𝒬𝒫𝒪𝒩𝒦𝒥𝒢𝒟𝒞𝒜𝒁𝒀𝑿𝑾𝑽𝑼𝑻𝑺𝑹𝑸𝑷𝑶𝑵𝑴𝑳𝑲𝑱𝑰𝑯𝑮𝑭𝑬𝑫𝑪𝑩𝑨𝑍𝑌𝑋𝑊𝑉𝑈𝑇𝑆𝑅𝑄𝑃𝑂𝑁𝑀𝐿𝐾𝐽𝐼𝐻𝐺𝐹𝐸𝐷𝐶𝐵𝐴𝐙𝐘𝐗𝐖𝐕𝐔𝐓𝐒𝐑𝐐𝐏𝐎𝐍𝐌𝐋𝐊𝐉𝐈𝐇𝐆𝐅𝐄𝐃𝐂𝐁𝐀𐐧𐐦𐐥𐐤𐐣𐐢𐐡𐐠𐐟𐐞𐐝𐐜𐐛𐐚𐐙𐐘𐐗𐐖𐐕𐐔𐐓𐐒𐐑𐐐𐐏𐐎𐐍𐐌𐐋𐐊𐐉𐐈𐐇𐐆𐐅𐐄𐐃𐐂𐐁𐐀ＺＹＸＷＶＵＴＳＲＱＰＯＮＭＬＫＪＩＨＧＦＥＤＣＢＡⳢⳠⳞⳜⳚⳘⳖⳔⳒⳐⳎⳌⳊⳈⳆⳄⳂⳀⲾⲼⲺⲸⲶⲴⲲⲰⲮⲬⲪⲨⲦⲤⲢⲠⲞⲜⲚⲘⲖⲔⲒⲐⲎⲌⲊⲈⲆⲄⲂⲀⱵⱫⱩⱧⱤⱣⱢⱠⰮⰭⰬⰫⰪⰩⰨⰧⰦⰥⰤⰣⰢⰡⰠⰟⰞⰝⰜⰛⰚⰙⰘⰗⰖⰕⰔⰓⰒⰑⰐⰏⰎⰍⰌⰋⰊⰉⰈⰇⰆⰅⰄⰃⰂⰁⰀⓏⓎⓍⓌⓋⓊⓉⓈⓇⓆⓅⓄⓃⓂⓁⓀⒿⒾⒽⒼⒻⒺⒹⒸⒷⒶↃⅯⅮⅭⅬⅫⅪⅩⅨⅧⅦⅥⅤⅣⅢⅡⅠⅅℿℾℳℲℱℰℭℬÅKℨΩℤℝℜℛℚℙℕℒℑℐℍℌℋℇℂΏῺΌῸῬΎῪῩῨΊῚῙῘΉῊΈῈΆᾺᾹᾸὯὮὭὬὫὪὩὨὟὝὛὙὍὌὋὊὉὈἿἾἽἼἻἺἹἸἯἮἭἬἫἪἩἨἝἜἛἚἙἘἏἎἍἌἋἊἉἈỸỶỴỲỰỮỬỪỨỦỤỢỠỞỜỚỘỖỔỒỐỎỌỊỈỆỄỂỀẾẼẺẸẶẴẲẰẮẬẪẨẦẤẢẠẔẒẐẎẌẊẈẆẄẂẀṾṼṺṸṶṴṲṰṮṬṪṨṦṤṢṠṞṜṚṘṖṔṒṐṎṌṊṈṆṄṂṀḾḼḺḸḶḴḲḰḮḬḪḨḦḤḢḠḞḜḚḘḖḔḒḐḎḌḊḈḆḄḂḀჅჄჃჂჁჀႿႾႽႼႻႺႹႸႷႶႵႴႳႲႱႰႯႮႭႬႫႪႩႨႧႦႥႤႣႢႡႠՖՕՔՓՒՑՐՏՎՍՌՋՊՉՈՇՆՅՄՃՂՁՀԿԾԽԼԻԺԹԸԷԶԵԴԳԲԱԒԐԎԌԊԈԆԄԂԀӾӼӺӸӶӴӲӰӮӬӪӨӦӤӢӠӞӜӚӘӖӔӒӐӍӋӉӇӅӃӁӀҾҼҺҸҶҴҲҰҮҬҪҨҦҤҢҠҞҜҚҘҖҔҒҐҎҌҊҀѾѼѺѸѶѴѲѰѮѬѪѨѦѤѢѠЯЮЭЬЫЪЩШЧЦХФУТСРПОНМЛКЙИЗЖЕДГВБАЏЎЍЌЋЊЉЈЇІЅЄЃЂЁЀϿϾϽϺϹϷϴϮϬϪϨϦϤϢϠϞϜϚϘϔϓϒΫΪΩΨΧΦΥΤΣΡΠΟΞΝΜΛΚΙΘΗΖΕΔΓΒΑΏΎΌΊΉΈΆɎɌɊɈɆɅɄɃɁȾȽȻȺȲȰȮȬȪȨȦȤȢȠȞȜȚȘȖȔȒȐȎȌȊȈȆȄȂȀǾǼǺǸǷǶǴǱǮǬǪǨǦǤǢǠǞǛǙǗǕǓǑǏǍǊǇǄƼƸƷƵƳƲƱƯƮƬƩƧƦƤƢƠƟƝƜƘƗƖƔƓƑƐƏƎƋƊƉƇƆƄƂƁŽŻŹŸŶŴŲŰŮŬŪŨŦŤŢŠŞŜŚŘŖŔŒŐŎŌŊŇŅŃŁĿĽĻĹĶĴĲİĮĬĪĨĦĤĢĠĞĜĚĘĖĔĒĐĎČĊĈĆĄĂĀÞÝÜÛÚÙØÖÕÔÓÒÑÐÏÎÍÌËÊÉÈÇÆÅÄÃÂÁÀZYXWVUTSRQPONMLKJIHGFEDCBA
```



## REXX

Both versions will work correctly on an ASCII or EBCDIC system.
===non-spaced list===

```rexx
/*REXX program determines what characters are  lowercase and uppercase  (Latin) letters.*/
$L=                                              /*set lowercase alphabet string to null*/
$U=                                              /* "  uppercase     "       "    "   " */
    do #=0  for 2**8                             /*traipse through  all  the characters.*/
                                       _=d2c(#)  /*convert decimal number to character. */
    if datatype(_, 'L')  then $L=$L || _         /*Lowercase?  Then add char to the list*/
    if datatype(_, 'U')  then $U=$U || _         /*Uppercase?    "   "   "    "  "    " */
    end   /*#*/                                  /* [↑]  put all the letters into a list*/

say '    lowercase letters: '   $L               /*display all the  lowercase  letters. */
say '    uppercase letters: '   $U               /*   "     "   "   uppercase     "     */
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

    lowercase letters:  abcdefghijklmnopqrstuvwxyz
    uppercase letters:  ABCDEFGHIJKLMNOPQRSTUVWXYZ

```



### a spaced list


```rexx
/*REXX program determines what characters are  lowercase and uppercase  (Latin) letters.*/
$L=                                              /*set lowercase alphabet string to null*/
$U=                                              /* "  uppercase     "       "    "   " */
    do #=0  for 2**8                             /*traipse through  all  the characters.*/
                                    _=d2c(#)     /*convert decimal number to character. */
    if datatype(_, 'L')  then $L=$L _            /*Lowercase?  Then add char to the list*/
    if datatype(_, 'U')  then $U=$U _            /*Uppercase?    "   "   "    "  "    " */
    end   /*#*/                                  /* [↑]  put all the letters into a list*/

say '    lowercase letters: '   $L               /*display all the  lowercase  letters. */
say '    uppercase letters: '   $U               /*   "     "   "   uppercase     "     */
                                                 /*stick a fork in it,  we're all done. */
```

'''output'''

```txt

    lowercase letters:   a b c d e f g h i j k l m n o p q r s t u v w x y z
    uppercase letters:   A B C D E F G H I J K L M N O P Q R S T U V W X Y Z

```



## Ring


```ring
# Project : Idiomatically determine all the lowercase and uppercase letters

aList = "a":"z"
see "Lower case : "
for n=1 to len(aList)
    see aList[n]
next
see nl
see "Upper case : "
aList = "A":"Z"
for n=1 to len(aList)
    see aList[n]
next
```

Output:
 Lower case : abcdefghijklmnopqrstuvwxyz
 Upper case : ABCDEFGHIJKLMNOPQRSTUVWXYZ


## Ruby



```Ruby
puts "Lowercase:", [*"a".."z"].join, "Uppercase:", [*"A".."Z"].join
```

```txt

Lowercase:
abcdefghijklmnopqrstuvwxyz
Uppercase:
ABCDEFGHIJKLMNOPQRSTUVWXYZ

```



## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/p0kJmlW/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/EkNsvoHyREG2izH4GcKiuQ Scastie (remote JVM)].

```Scala
object IdiomaticallyDetermineLowercaseUppercase extends App {

  println("Upper case: "
    + (0 to 0x10FFFF).map(_.toChar).filter(_.isUpper).take(72).mkString + "...")

  println("Lower case: "
    + (0 to 0x10FFFF).map(_.toChar).filter(_.isLower).take(72).mkString + "...")

}
```



## Tcl

Tcl has supported the Basic Multilingual Plane of Unicode since Tcl 8.1.

```tcl
for {set c 0} {$c <= 0xffff} {incr c} {
    set ch [format "%c" $c]
    if {[string is upper $ch]} {lappend upper $ch}
    if {[string is lower $ch]} {lappend lower $ch}
}
puts "Upper: $upper"
puts "Lower: $lower"
```

```txt

Upper: A B C D E F G H I J K L M N O P Q R S T U V W X Y Z À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï Ð Ñ Ò Ó Ô Õ Ö Ø Ù Ú Û Ü Ý Þ Ā Ă Ą Ć Ĉ Ċ Č Ď Đ Ē Ĕ Ė Ę Ě Ĝ Ğ Ġ Ģ Ĥ Ħ Ĩ Ī Ĭ Į İ Ĳ Ĵ Ķ Ĺ Ļ Ľ Ŀ Ł Ń Ņ Ň Ŋ Ō Ŏ Ő Œ Ŕ Ŗ Ř Ś Ŝ Ş Š Ţ Ť Ŧ Ũ Ū Ŭ Ů Ű Ų Ŵ Ŷ Ÿ Ź Ż Ž Ɓ Ƃ Ƅ Ɔ Ƈ Ɖ Ɗ Ƌ Ǝ Ə Ɛ Ƒ Ɠ Ɣ Ɩ Ɨ Ƙ Ɯ Ɲ Ɵ Ơ Ƣ Ƥ Ʀ Ƨ Ʃ Ƭ Ʈ Ư Ʊ Ʋ Ƴ Ƶ Ʒ Ƹ Ƽ Ǆ Ǉ Ǌ Ǎ Ǐ Ǒ Ǔ Ǖ Ǘ Ǚ Ǜ Ǟ Ǡ Ǣ Ǥ Ǧ Ǩ Ǫ Ǭ Ǯ Ǳ Ǵ Ƕ Ƿ Ǹ Ǻ Ǽ Ǿ Ȁ Ȃ Ȅ Ȇ Ȉ Ȋ Ȍ Ȏ Ȑ Ȓ Ȕ Ȗ Ș Ț Ȝ Ȟ Ƞ Ȣ Ȥ Ȧ Ȩ Ȫ Ȭ Ȯ Ȱ Ȳ Ⱥ Ȼ Ƚ Ⱦ Ɂ Ƀ Ʉ Ʌ Ɇ Ɉ Ɋ Ɍ Ɏ Ͱ Ͳ Ͷ Ά Έ Ή Ί Ό Ύ Ώ Α Β Γ Δ Ε Ζ Η Θ Ι Κ Λ Μ Ν Ξ Ο Π Ρ Σ Τ Υ Φ Χ Ψ Ω Ϊ Ϋ Ϗ ϒ ϓ ϔ Ϙ Ϛ Ϝ Ϟ Ϡ Ϣ Ϥ Ϧ Ϩ Ϫ Ϭ Ϯ ϴ Ϸ Ϲ Ϻ Ͻ Ͼ Ͽ Ѐ Ё Ђ Ѓ Є Ѕ І Ї Ј Љ Њ Ћ Ќ Ѝ Ў Џ А Б В Г Д Е Ж З И Й К Л М Н О П Р С Т У Ф Х Ц Ч Ш Щ Ъ Ы Ь Э Ю Я Ѡ Ѣ Ѥ Ѧ Ѩ Ѫ Ѭ Ѯ Ѱ Ѳ Ѵ Ѷ Ѹ Ѻ Ѽ Ѿ Ҁ Ҋ Ҍ Ҏ Ґ Ғ Ҕ Җ Ҙ Қ Ҝ Ҟ Ҡ Ң Ҥ Ҧ Ҩ Ҫ Ҭ Ү Ұ Ҳ Ҵ Ҷ Ҹ Һ Ҽ Ҿ Ӏ Ӂ Ӄ Ӆ Ӈ Ӊ Ӌ Ӎ Ӑ Ӓ Ӕ Ӗ Ә Ӛ Ӝ Ӟ Ӡ Ӣ Ӥ Ӧ Ө Ӫ Ӭ Ӯ Ӱ Ӳ Ӵ Ӷ Ӹ Ӻ Ӽ Ӿ Ԁ Ԃ Ԅ Ԇ Ԉ Ԋ Ԍ Ԏ Ԑ Ԓ Ԕ Ԗ Ԙ Ԛ Ԝ Ԟ Ԡ Ԣ Ԥ Ԧ Ա Բ Գ Դ Ե Զ Է Ը Թ Ժ Ի Լ Խ Ծ Կ Հ Ձ Ղ Ճ Մ Յ Ն Շ Ո Չ Պ Ջ Ռ Ս Վ Տ Ր Ց Ւ Փ Ք Օ Ֆ Ⴀ Ⴁ Ⴂ Ⴃ Ⴄ Ⴅ Ⴆ Ⴇ Ⴈ Ⴉ Ⴊ Ⴋ Ⴌ Ⴍ Ⴎ Ⴏ Ⴐ Ⴑ Ⴒ Ⴓ Ⴔ Ⴕ Ⴖ Ⴗ Ⴘ Ⴙ Ⴚ Ⴛ Ⴜ Ⴝ Ⴞ Ⴟ Ⴠ Ⴡ Ⴢ Ⴣ Ⴤ Ⴥ Ⴧ Ⴭ Ḁ Ḃ Ḅ Ḇ Ḉ Ḋ Ḍ Ḏ Ḑ Ḓ Ḕ Ḗ Ḙ Ḛ Ḝ Ḟ Ḡ Ḣ Ḥ Ḧ Ḩ Ḫ Ḭ Ḯ Ḱ Ḳ Ḵ Ḷ Ḹ Ḻ Ḽ Ḿ Ṁ Ṃ Ṅ Ṇ Ṉ Ṋ Ṍ Ṏ Ṑ Ṓ Ṕ Ṗ Ṙ Ṛ Ṝ Ṟ Ṡ Ṣ Ṥ Ṧ Ṩ Ṫ Ṭ Ṯ Ṱ Ṳ Ṵ Ṷ Ṹ Ṻ Ṽ Ṿ Ẁ Ẃ Ẅ Ẇ Ẉ Ẋ Ẍ Ẏ Ẑ Ẓ Ẕ ẞ Ạ Ả Ấ Ầ Ẩ Ẫ Ậ Ắ Ằ Ẳ Ẵ Ặ Ẹ Ẻ Ẽ Ế Ề Ể Ễ Ệ Ỉ Ị Ọ Ỏ Ố Ồ Ổ Ỗ Ộ Ớ Ờ Ở Ỡ Ợ Ụ Ủ Ứ Ừ Ử Ữ Ự Ỳ Ỵ Ỷ Ỹ Ỻ Ỽ Ỿ Ἀ Ἁ Ἂ Ἃ Ἄ Ἅ Ἆ Ἇ Ἐ Ἑ Ἒ Ἓ Ἔ Ἕ Ἠ Ἡ Ἢ Ἣ Ἤ Ἥ Ἦ Ἧ Ἰ Ἱ Ἲ Ἳ Ἴ Ἵ Ἶ Ἷ Ὀ Ὁ Ὂ Ὃ Ὄ Ὅ Ὑ Ὓ Ὕ Ὗ Ὠ Ὡ Ὢ Ὣ Ὤ Ὥ Ὦ Ὧ Ᾰ Ᾱ Ὰ Ά Ὲ Έ Ὴ Ή Ῐ Ῑ Ὶ Ί Ῠ Ῡ Ὺ Ύ Ῥ Ὸ Ό Ὼ Ώ ℂ ℇ ℋ ℌ ℍ ℐ ℑ ℒ ℕ ℙ ℚ ℛ ℜ ℝ ℤ Ω ℨ K Å ℬ ℭ ℰ ℱ Ⅎ ℳ ℾ ℿ ⅅ Ↄ Ⰰ Ⰱ Ⰲ Ⰳ Ⰴ Ⰵ Ⰶ Ⰷ Ⰸ Ⰹ Ⰺ Ⰻ Ⰼ Ⰽ Ⰾ Ⰿ Ⱀ Ⱁ Ⱂ Ⱃ Ⱄ Ⱅ Ⱆ Ⱇ Ⱈ Ⱉ Ⱊ Ⱋ Ⱌ Ⱍ Ⱎ Ⱏ Ⱐ Ⱑ Ⱒ Ⱓ Ⱔ Ⱕ Ⱖ Ⱗ Ⱘ Ⱙ Ⱚ Ⱛ Ⱜ Ⱝ Ⱞ Ⱡ Ɫ Ᵽ Ɽ Ⱨ Ⱪ Ⱬ Ɑ Ɱ Ɐ Ɒ Ⱳ Ⱶ Ȿ Ɀ Ⲁ Ⲃ Ⲅ Ⲇ Ⲉ Ⲋ Ⲍ Ⲏ Ⲑ Ⲓ Ⲕ Ⲗ Ⲙ Ⲛ Ⲝ Ⲟ Ⲡ Ⲣ Ⲥ Ⲧ Ⲩ Ⲫ Ⲭ Ⲯ Ⲱ Ⲳ Ⲵ Ⲷ Ⲹ Ⲻ Ⲽ Ⲿ Ⳁ Ⳃ Ⳅ Ⳇ Ⳉ Ⳋ Ⳍ Ⳏ Ⳑ Ⳓ Ⳕ Ⳗ Ⳙ Ⳛ Ⳝ Ⳟ Ⳡ Ⳣ Ⳬ Ⳮ Ⳳ Ꙁ Ꙃ Ꙅ Ꙇ Ꙉ Ꙋ Ꙍ Ꙏ Ꙑ Ꙓ Ꙕ Ꙗ Ꙙ Ꙛ Ꙝ Ꙟ Ꙡ Ꙣ Ꙥ Ꙧ Ꙩ Ꙫ Ꙭ Ꚁ Ꚃ Ꚅ Ꚇ Ꚉ Ꚋ Ꚍ Ꚏ Ꚑ Ꚓ Ꚕ Ꚗ Ꜣ Ꜥ Ꜧ Ꜩ Ꜫ Ꜭ Ꜯ Ꜳ Ꜵ Ꜷ Ꜹ Ꜻ Ꜽ Ꜿ Ꝁ Ꝃ Ꝅ Ꝇ Ꝉ Ꝋ Ꝍ Ꝏ Ꝑ Ꝓ Ꝕ Ꝗ Ꝙ Ꝛ Ꝝ Ꝟ Ꝡ Ꝣ Ꝥ Ꝧ Ꝩ Ꝫ Ꝭ Ꝯ Ꝺ Ꝼ Ᵹ Ꝿ Ꞁ Ꞃ Ꞅ Ꞇ Ꞌ Ɥ Ꞑ Ꞓ Ꞡ Ꞣ Ꞥ Ꞧ Ꞩ Ɦ Ａ Ｂ Ｃ Ｄ Ｅ Ｆ Ｇ Ｈ Ｉ Ｊ Ｋ Ｌ Ｍ Ｎ Ｏ Ｐ Ｑ Ｒ Ｓ Ｔ Ｕ Ｖ Ｗ Ｘ Ｙ Ｚ
Lower: a b c d e f g h i j k l m n o p q r s t u v w x y z µ ß à á â ã ä å æ ç è é ê ë ì í î ï ð ñ ò ó ô õ ö ø ù ú û ü ý þ ÿ ā ă ą ć ĉ ċ č ď đ ē ĕ ė ę ě ĝ ğ ġ ģ ĥ ħ ĩ ī ĭ į ı ĳ ĵ ķ ĸ ĺ ļ ľ ŀ ł ń ņ ň ŉ ŋ ō ŏ ő œ ŕ ŗ ř ś ŝ ş š ţ ť ŧ ũ ū ŭ ů ű ų ŵ ŷ ź ż ž ſ ƀ ƃ ƅ ƈ ƌ ƍ ƒ ƕ ƙ ƚ ƛ ƞ ơ ƣ ƥ ƨ ƪ ƫ ƭ ư ƴ ƶ ƹ ƺ ƽ ƾ ƿ ǆ ǉ ǌ ǎ ǐ ǒ ǔ ǖ ǘ ǚ ǜ ǝ ǟ ǡ ǣ ǥ ǧ ǩ ǫ ǭ ǯ ǰ ǳ ǵ ǹ ǻ ǽ ǿ ȁ ȃ ȅ ȇ ȉ ȋ ȍ ȏ ȑ ȓ ȕ ȗ ș ț ȝ ȟ ȡ ȣ ȥ ȧ ȩ ȫ ȭ ȯ ȱ ȳ ȴ ȵ ȶ ȷ ȸ ȹ ȼ ȿ ɀ ɂ ɇ ɉ ɋ ɍ ɏ ɐ ɑ ɒ ɓ ɔ ɕ ɖ ɗ ɘ ə ɚ ɛ ɜ ɝ ɞ ɟ ɠ ɡ ɢ ɣ ɤ ɥ ɦ ɧ ɨ ɩ ɪ ɫ ɬ ɭ ɮ ɯ ɰ ɱ ɲ ɳ ɴ ɵ ɶ ɷ ɸ ɹ ɺ ɻ ɼ ɽ ɾ ɿ ʀ ʁ ʂ ʃ ʄ ʅ ʆ ʇ ʈ ʉ ʊ ʋ ʌ ʍ ʎ ʏ ʐ ʑ ʒ ʓ ʕ ʖ ʗ ʘ ʙ ʚ ʛ ʜ ʝ ʞ ʟ ʠ ʡ ʢ ʣ ʤ ʥ ʦ ʧ ʨ ʩ ʪ ʫ ʬ ʭ ʮ ʯ ͱ ͳ ͷ ͻ ͼ ͽ ΐ ά έ ή ί ΰ α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ ς σ τ υ φ χ ψ ω ϊ ϋ ό ύ ώ ϐ ϑ ϕ ϖ ϗ ϙ ϛ ϝ ϟ ϡ ϣ ϥ ϧ ϩ ϫ ϭ ϯ ϰ ϱ ϲ ϳ ϵ ϸ ϻ ϼ а б в г д е ж з и й к л м н о п р с т у ф х ц ч ш щ ъ ы ь э ю я ѐ ё ђ ѓ є ѕ і ї ј љ њ ћ ќ ѝ ў џ ѡ ѣ ѥ ѧ ѩ ѫ ѭ ѯ ѱ ѳ ѵ ѷ ѹ ѻ ѽ ѿ ҁ ҋ ҍ ҏ ґ ғ ҕ җ ҙ қ ҝ ҟ ҡ ң ҥ ҧ ҩ ҫ ҭ ү ұ ҳ ҵ ҷ ҹ һ ҽ ҿ ӂ ӄ ӆ ӈ ӊ ӌ ӎ ӏ ӑ ӓ ӕ ӗ ә ӛ ӝ ӟ ӡ ӣ ӥ ӧ ө ӫ ӭ ӯ ӱ ӳ ӵ ӷ ӹ ӻ ӽ ӿ ԁ ԃ ԅ ԇ ԉ ԋ ԍ ԏ ԑ ԓ ԕ ԗ ԙ ԛ ԝ ԟ ԡ ԣ ԥ ԧ ա բ գ դ ե զ է ը թ ժ ի լ խ ծ կ հ ձ ղ ճ մ յ ն շ ո չ պ ջ ռ ս վ տ ր ց ւ փ ք օ ֆ և ᴀ ᴁ ᴂ ᴃ ᴄ ᴅ ᴆ ᴇ ᴈ ᴉ ᴊ ᴋ ᴌ ᴍ ᴎ ᴏ ᴐ ᴑ ᴒ ᴓ ᴔ ᴕ ᴖ ᴗ ᴘ ᴙ ᴚ ᴛ ᴜ ᴝ ᴞ ᴟ ᴠ ᴡ ᴢ ᴣ ᴤ ᴥ ᴦ ᴧ ᴨ ᴩ ᴪ ᴫ ᵫ ᵬ ᵭ ᵮ ᵯ ᵰ ᵱ ᵲ ᵳ ᵴ ᵵ ᵶ ᵷ ᵹ ᵺ ᵻ ᵼ ᵽ ᵾ ᵿ ᶀ ᶁ ᶂ ᶃ ᶄ ᶅ ᶆ ᶇ ᶈ ᶉ ᶊ ᶋ ᶌ ᶍ ᶎ ᶏ ᶐ ᶑ ᶒ ᶓ ᶔ ᶕ ᶖ ᶗ ᶘ ᶙ ᶚ ḁ ḃ ḅ ḇ ḉ ḋ ḍ ḏ ḑ ḓ ḕ ḗ ḙ ḛ ḝ ḟ ḡ ḣ ḥ ḧ ḩ ḫ ḭ ḯ ḱ ḳ ḵ ḷ ḹ ḻ ḽ ḿ ṁ ṃ ṅ ṇ ṉ ṋ ṍ ṏ ṑ ṓ ṕ ṗ ṙ ṛ ṝ ṟ ṡ ṣ ṥ ṧ ṩ ṫ ṭ ṯ ṱ ṳ ṵ ṷ ṹ ṻ ṽ ṿ ẁ ẃ ẅ ẇ ẉ ẋ ẍ ẏ ẑ ẓ ẕ ẖ ẗ ẘ ẙ ẚ ẛ ẜ ẝ ẟ ạ ả ấ ầ ẩ ẫ ậ ắ ằ ẳ ẵ ặ ẹ ẻ ẽ ế ề ể ễ ệ ỉ ị ọ ỏ ố ồ ổ ỗ ộ ớ ờ ở ỡ ợ ụ ủ ứ ừ ử ữ ự ỳ ỵ ỷ ỹ ỻ ỽ ỿ ἀ ἁ ἂ ἃ ἄ ἅ ἆ ἇ ἐ ἑ ἒ ἓ ἔ ἕ ἠ ἡ ἢ ἣ ἤ ἥ ἦ ἧ ἰ ἱ ἲ ἳ ἴ ἵ ἶ ἷ ὀ ὁ ὂ ὃ ὄ ὅ ὐ ὑ ὒ ὓ ὔ ὕ ὖ ὗ ὠ ὡ ὢ ὣ ὤ ὥ ὦ ὧ ὰ ά ὲ έ ὴ ή ὶ ί ὸ ό ὺ ύ ὼ ώ ᾀ ᾁ ᾂ ᾃ ᾄ ᾅ ᾆ ᾇ ᾐ ᾑ ᾒ ᾓ ᾔ ᾕ ᾖ ᾗ ᾠ ᾡ ᾢ ᾣ ᾤ ᾥ ᾦ ᾧ ᾰ ᾱ ᾲ ᾳ ᾴ ᾶ ᾷ ι ῂ ῃ ῄ ῆ ῇ ῐ ῑ ῒ ΐ ῖ ῗ ῠ ῡ ῢ ΰ ῤ ῥ ῦ ῧ ῲ ῳ ῴ ῶ ῷ ℊ ℎ ℏ ℓ ℯ ℴ ℹ ℼ ℽ ⅆ ⅇ ⅈ ⅉ ⅎ ↄ ⰰ ⰱ ⰲ ⰳ ⰴ ⰵ ⰶ ⰷ ⰸ ⰹ ⰺ ⰻ ⰼ ⰽ ⰾ ⰿ ⱀ ⱁ ⱂ ⱃ ⱄ ⱅ ⱆ ⱇ ⱈ ⱉ ⱊ ⱋ ⱌ ⱍ ⱎ ⱏ ⱐ ⱑ ⱒ ⱓ ⱔ ⱕ ⱖ ⱗ ⱘ ⱙ ⱚ ⱛ ⱜ ⱝ ⱞ ⱡ ⱥ ⱦ ⱨ ⱪ ⱬ ⱱ ⱳ ⱴ ⱶ ⱷ ⱸ ⱹ ⱺ ⱻ ⲁ ⲃ ⲅ ⲇ ⲉ ⲋ ⲍ ⲏ ⲑ ⲓ ⲕ ⲗ ⲙ ⲛ ⲝ ⲟ ⲡ ⲣ ⲥ ⲧ ⲩ ⲫ ⲭ ⲯ ⲱ ⲳ ⲵ ⲷ ⲹ ⲻ ⲽ ⲿ ⳁ ⳃ ⳅ ⳇ ⳉ ⳋ ⳍ ⳏ ⳑ ⳓ ⳕ ⳗ ⳙ ⳛ ⳝ ⳟ ⳡ ⳣ ⳤ ⳬ ⳮ ⳳ ⴀ ⴁ ⴂ ⴃ ⴄ ⴅ ⴆ ⴇ ⴈ ⴉ ⴊ ⴋ ⴌ ⴍ ⴎ ⴏ ⴐ ⴑ ⴒ ⴓ ⴔ ⴕ ⴖ ⴗ ⴘ ⴙ ⴚ ⴛ ⴜ ⴝ ⴞ ⴟ ⴠ ⴡ ⴢ ⴣ ⴤ ⴥ ⴧ ⴭ ꙁ ꙃ ꙅ ꙇ ꙉ ꙋ ꙍ ꙏ ꙑ ꙓ ꙕ ꙗ ꙙ ꙛ ꙝ ꙟ ꙡ ꙣ ꙥ ꙧ ꙩ ꙫ ꙭ ꚁ ꚃ ꚅ ꚇ ꚉ ꚋ ꚍ ꚏ ꚑ ꚓ ꚕ ꚗ ꜣ ꜥ ꜧ ꜩ ꜫ ꜭ ꜯ ꜰ ꜱ ꜳ ꜵ ꜷ ꜹ ꜻ ꜽ ꜿ ꝁ ꝃ ꝅ ꝇ ꝉ ꝋ ꝍ ꝏ ꝑ ꝓ ꝕ ꝗ ꝙ ꝛ ꝝ ꝟ ꝡ ꝣ ꝥ ꝧ ꝩ ꝫ ꝭ ꝯ ꝱ ꝲ ꝳ ꝴ ꝵ ꝶ ꝷ ꝸ ꝺ ꝼ ꝿ ꞁ ꞃ ꞅ ꞇ ꞌ ꞎ ꞑ ꞓ ꞡ ꞣ ꞥ ꞧ ꞩ ꟺ ﬀ ﬁ ﬂ ﬃ ﬄ ﬅ ﬆ ﬓ ﬔ ﬕ ﬖ ﬗ ａ ｂ ｃ ｄ ｅ ｆ ｇ ｈ ｉ ｊ ｋ ｌ ｍ ｎ ｏ ｐ ｑ ｒ ｓ ｔ ｕ ｖ ｗ ｘ ｙ ｚ

```


## ZX Spectrum Basic


```zxbasic
10 FOR x=CODE "a" TO CODE "z"
20 PRINT CHR$ x;
30 NEXT x
40 PRINT
50 FOR x=CODE "A" TO CODE "Z"
60 PRINT CHR$ x;
70 NEXT x
```

```txt
abcdefghijklmnopqrstuvwxyz
ABCDEFGHIJKLMNOPQRSTUVWXYZ

0 OK, 70:1
```

