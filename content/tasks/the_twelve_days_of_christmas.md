+++
title = "The Twelve Days of Christmas"
description = ""
date = 2019-10-18T20:29:35Z
aliases = []
[extra]
id = 16950
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "actionscript",
  "ada",
  "algol_68",
  "applescript",
  "autohotkey",
  "awk",
  "batch_file",
  "befunge",
  "bracmat",
  "c",
  "clojure",
  "cobol",
  "common_lisp",
  "cpp",
  "crystal",
  "csharp",
  "d",
  "dc",
  "eiffel",
  "elena",
  "elixir",
  "erlang",
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
  "logo",
  "lolcode",
  "lua",
  "maple",
  "mathematica",
  "miniscript",
  "nim",
  "objeck",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "racket",
  "related_tasks",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "scala",
  "seed7",
  "self",
  "sidef",
  "simula",
  "smalltalk",
  "smart_basic",
  "snobol",
  "sql",
  "swift",
  "tailspin",
  "tcl",
  "unix_shell",
  "vba",
  "vbscript",
  "visual_basic_dotnet",
  "zkl",
]
+++

## Task

Write a program that outputs the lyrics of the Christmas carol ''The Twelve Days of Christmas''.
The lyrics can be found [http://www.lyricsmode.com/lyrics/c/christmas_carols/the_twelve_days_of_christmas.html here].

(You must reproduce the words in the correct order, but case, format, and punctuation are left to your discretion.)


## Related tasks

*   [[99 Bottles of Beer]]
*   [[Old_lady_swallowed_a_fly]]
*   [[Comma quibbling]]





## ActionScript

This program outputs the lyrics to a TextField object.
The text field can be scrolled using the mouse wheel (Windows only) or by using the up/down arrow keys on the keyboard.

{{works with|Adobe AIR|AIR|1.5}} (Although the code can work in Flash Player 9 by replacing the Vectors with Arrays)

```ActionScript
package {

    import flash.display.Sprite;
    import flash.events.Event;
    import flash.events.KeyboardEvent;
    import flash.events.MouseEvent;
    import flash.text.TextField;
    import flash.text.TextFieldAutoSize;
    import flash.text.TextFormat;
    import flash.ui.Keyboard;

    public class TwelveDaysOfChristmas extends Sprite {

        private var _textArea:TextField = new TextField();

        public function TwelveDaysOfChristmas() {
            if ( stage ) _init();
            else addEventListener(Event.ADDED_TO_STAGE, _init);
        }

        private function _init(e:Event = null):void {

            removeEventListener(Event.ADDED_TO_STAGE, _init);

            _textArea = new TextField();
            _textArea.x = 10;
            _textArea.y = 10;
            _textArea.autoSize = TextFieldAutoSize.LEFT;
            _textArea.wordWrap = true;
            _textArea.width = stage.stageWidth - 20;
            _textArea.height = stage.stageHeight - 10;
            _textArea.multiline = true;

            var format:TextFormat = new TextFormat();
            format.size = 14;
            _textArea.defaultTextFormat = format;

            var verses:Vector.<String> = new Vector.<String>(12, true);
            var lines:Vector.<String>;

            var days:Vector.<String> = new Vector.<String>(12, true);
            var gifts:Vector.<String> = new Vector.<String>(12, true);

            days[0] = 'first';
            days[1] = 'second';
            days[2] = 'third';
            days[3] = 'fourth';
            days[4] = 'fifth';
            days[5] = 'sixth';
            days[6] = 'seventh';
            days[7] = 'eighth';
            days[8] = 'ninth';
            days[9] = 'tenth';
            days[10] = 'eleventh';
            days[11] = 'twelfth';

            gifts[0] = 'A partridge in a pear tree';
            gifts[1] = 'Two turtle doves';
            gifts[2] = 'Three french hens';
            gifts[3] = 'Four calling birds';
            gifts[4] = 'Five golden rings';
            gifts[5] = 'Six geese a-laying';
            gifts[6] = 'Seven swans a-swimming';
            gifts[7] = 'Eight maids a-milking';
            gifts[8] = 'Nine ladies dancing';
            gifts[9] = 'Ten lords a-leaping';
            gifts[10] = 'Eleven pipers piping';
            gifts[11] = 'Twelve drummers drumming';

            var i:uint, j:uint, k:uint, line:String;

            for ( i = 0; i < 12; i++ ) {

                lines = new Vector.<String>(i + 2, true);
                lines[0] = "On the " + days[i] + " day of Christmas, my true love gave to me";

                j = i + 1;
                k = 0;
                while ( j-- > 0 )
                    lines[++k] = gifts[j];

                verses[i] = lines.join('\n');

                if ( i == 0 )
                    gifts[0] = 'And a partridge in a pear tree';

            }

            var song:String = verses.join('\n\n');
            _textArea.text = song;
            addChild(_textArea);

            _textArea.addEventListener(MouseEvent.MOUSE_WHEEL, _onMouseWheel);
            stage.addEventListener(KeyboardEvent.KEY_DOWN, _onKeyDown);

        }

        private function _onKeyDown(e:KeyboardEvent):void {
            if ( e.keyCode == Keyboard.DOWN )
                _textArea.y -= 40;
            else if ( e.keyCode == Keyboard.UP )
                _textArea.y += 40;
        }

        private function _onMouseWheel(e:MouseEvent):void {
            _textArea.y += 20 * e.delta;
        }

    }

}
```



## ALGOL 68

```algol68
BEGIN
  []STRING labels = ("first", "second", "third",    "fourth",
                     "fifth", "sixth",  "seventh",  "eighth",
                     "ninth", "tenth",  "eleventh", "twelfth");

  []STRING gifts = ("A partridge in a pear tree.",
                    "Two turtle doves, and",
                    "Three French hens,",
                    "Four calling birds,",
                    "Five gold rings,",
                    "Six geese a-laying,",
                    "Seven swans a-swimming,",
                    "Eight maids a-milking,",
                    "Nine ladies dancing,",
                    "Ten lords a-leaping,",
                    "Eleven pipers piping,",
                    "Twelve drummers drumming,");
  FOR day TO 12 DO
    print(("On the ", labels[day],
           " day of Christmas, my true love sent to me:", newline));
    FOR gift FROM day BY -1 TO 1 DO
      print((gifts[gift], newline))
    OD;
    print(newline)
  OD
END
```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves, and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves, and
A partridge in a pear tree.
```



## AppleScript


### Iterative


```applescript
set gifts to {"A partridge in a pear tree.", "Two turtle doves, and", ¬
	"Three French hens,", "Four calling birds,", ¬
	"Five gold rings,", "Six geese a-laying,", ¬
	"Seven swans a-swimming,", "Eight maids a-milking,", ¬
	"Nine ladies dancing,", "Ten lords a-leaping,", ¬
	"Eleven pipers piping,", "Twelve drummers drumming"}

set labels to {"first", "second", "third", "fourth", "fifth", "sixth", ¬
	"seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"}

repeat with day from 1 to 12
	log "On the " & item day of labels & " day of Christmas, my true love sent to me:"
	repeat with gift from day to 1 by -1
		log item gift of gifts
	end repeat
	log ""
end repeat
```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves, and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves, and
A partridge in a pear tree.
```




### Functional composition


Drawing on some functional primitives, and post-Yosemite AppleScript's ability to import Foundation classes:


```AppleScript
use framework "Foundation"

property pstrGifts : "A partridge in a pear tree, Two turtle doves, Three French hens, " & ¬
    "Four calling birds, Five golden rings, Six geese a-laying, " & ¬
    "Seven swans a-swimming, Eight maids a-milking, Nine ladies dancing, " & ¬
    "Ten lords a-leaping, Eleven pipers piping, Twelve drummers drumming"

property pstrOrdinals : "first, second, third, fourth, fifth, " & ¬
    "sixth, seventh, eighth, ninth, tenth, eleventh, twelfth"

-- DAYS OF XMAS ------------------------------------------------------------

-- daysOfXmas :: () -> String
on daysOfXmas()

    -- csv :: String -> [String]
    script csv
        on |λ|(str)
            splitOn(", ", str)
        end |λ|
    end script

    set {gifts, ordinals} to map(csv, [pstrGifts, pstrOrdinals])

    -- verseOfTheDay :: Int -> String
    script verseOfTheDay

        -- dayGift :: Int -> String
        script dayGift
            on |λ|(n, i)
                set strGift to item n of gifts
                if n = 1 then
                    set strFirst to strGift & " !"
                    if i is not 1 then
                        "And " & toLower(text 1 of strFirst) & text 2 thru -1 of strFirst
                    else
                        strFirst
                    end if
                else if n = 5 then
                    toUpper(strGift)
                else
                    strGift
                end if
            end |λ|
        end script

        on |λ|(intDay)
            "On the " & item intDay of ordinals & " day of Xmas, my true love gave to me ..." & ¬
                linefeed & intercalate("," & linefeed, ¬
                map(dayGift, enumFromTo(intDay, 1)))

        end |λ|
    end script

    intercalate(linefeed & linefeed, ¬
        map(verseOfTheDay, enumFromTo(1, length of ordinals)))

end daysOfXmas

-- TEST ---------------------------------------------------------------------
on run

    daysOfXmas()

end run


-- GENERIC FUNCTIONS --------------------------------------------------------

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m > n then
        set d to -1
    else
        set d to 1
    end if
    set lst to {}
    repeat with i from m to n by d
        set end of lst to i
    end repeat
    return lst
end enumFromTo

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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

-- splitOn :: Text -> Text -> [Text]
on splitOn(strDelim, strMain)
    set {dlm, my text item delimiters} to {my text item delimiters, strDelim}
    set lstParts to text items of strMain
    set my text item delimiters to dlm
    return lstParts
end splitOn

-- toLower :: String -> String
on toLower(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        lowercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toLower

-- toUpper :: String -> String
on toUpper(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        uppercaseStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toUpper
```


```txt
On the first day of Xmas, my true love gave to me ...
A partridge in a pear tree !

On the second day of Xmas, my true love gave to me ...
Two turtle doves,
And a partridge in a pear tree !

...

On the twelfth day of Xmas, my true love gave to me ...
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three French hens,
Two turtle doves,
And a partridge in a pear tree !
```



## Ada


```Ada
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

procedure Twelve_Days_Of_Christmas is
   type Days is (First, Second, Third, Fourth, Fifth, Sixth,
		 Seventh, Eighth, Ninth, Tenth, Eleventh, Twelfth);

   package E_IO is new Ada.Text_IO.Enumeration_IO(Days);
   use E_IO;

   Gifts : array (Days) of Unbounded_String :=
     (To_Unbounded_String(" A partridge in a pear-tree."),
      To_Unbounded_String(" Two turtle doves"),
      To_Unbounded_String(" Three French hens"),
      To_Unbounded_String(" Four calling birds"),
      To_Unbounded_String(" Five golden rings"),
      To_Unbounded_String(" Six geese a-laying"),
      To_Unbounded_String(" Seven swans a-swimming"),
      To_Unbounded_String(" Eight maids a-milking"),
      To_Unbounded_String(" Nine ladies dancing"),
      To_Unbounded_String(" Ten lords a-leaping"),
      To_Unbounded_String(" Eleven pipers piping"),
      To_Unbounded_String(" Twelve drummers drumming"));
begin
   for Day in Days loop
      Put("On the "); Put(Day, Set => Lower_Case); Put(" day of Christmas,");
      New_Line; Put_Line("My true love gave to me:");
      for D in reverse Days'First..Day loop
	 Put_Line(To_String(Gifts(D)));
      end loop;
      if Day = First then
	 Replace_Slice(Gifts(Day), 2, 2, "And a");
      end if;
      New_Line;
   end loop;
end Twelve_Days_Of_Christmas;

```



## AutoHotkey


```AutoHotkey
nth := ["first","second","third","fourth","fifth","sixth","seventh","eighth","ninth","tenth","eleventh","twelfth"]
lines := ["A partridge in a pear tree."
		,"Two turtle doves and"
		,"Three french hens"
		,"Four calling birds"
		,"Five golden rings"
		,"Six geese a-laying"
		,"Seven swans a-swimming"
		,"Eight maids a-milking"
		,"Nine ladies dancing"
		,"Ten lords a-leaping"
		,"Eleven pipers piping"
		,"Twelve drummers drumming"]

full:="", mid:=""
loop % lines.MaxIndex()
{
	top:="On the " . nth[A_Index] . " day of Christmas,`nMy true love gave to me:"
	mid:= lines[A_Index] . "`n" . mid
	full:= full . top . "`n" . mid . ((A_Index<lines.MaxIndex())?"`n":"")
}
MsgBox % full
```


## AWK


```AWK

# syntax: GAWK -f THE_TWELVE_DAYS_OF_CHRISTMAS.AWK
BEGIN {
    gifts[++i] = "a partridge in a pear tree."
    gifts[++i] = "two turtle doves, and"
    gifts[++i] = "three french hens,"
    gifts[++i] = "four calling birds,"
    gifts[++i] = "five golden rings,"
    gifts[++i] = "six geese a-laying,"
    gifts[++i] = "seven swans a-swimming,"
    gifts[++i] = "eight maids a-milking,"
    gifts[++i] = "nine ladies dancing,"
    gifts[++i] = "ten lords a-leaping,"
    gifts[++i] = "eleven pipers piping,"
    gifts[++i] = "twelve drummers drumming,"
    split("first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth",days_arr," ")
    for (i=1; i<=12; i++) {
      printf("On the %s day of Christmas,\n",days_arr[i])
      print("my true love gave to me:")
      for (j=i; j>0; j--) {
        printf("%s\n",gifts[j])
      }
      print("")
    }
    exit(0)
}

```

```txt

On the first day of Christmas,
my true love gave to me:
a partridge in a pear tree.

On the second day of Christmas,
my true love gave to me:
two turtle doves, and
a partridge in a pear tree.

...

On the twelfth day of Christmas,
my true love gave to me:
twelve drummers drumming,
eleven pipers piping,
ten lords a-leaping,
nine ladies dancing,
eight maids a-milking,
seven swans a-swimming,
six geese a-laying,
five golden rings,
four calling birds,
three french hens,
two turtle doves, and
a partridge in a pear tree.

```



## Batch File


```dos
:: The Twelve Days of Christmas
:: Batch File Implementation

@echo off

::Pseudo-array for Days
set "day1=First"
set "day2=Second"
set "day3=Third"
set "day4=Fourth"
set "day5=Fifth"
set "day6=Sixth"
set "day7=Seventh"
set "day8=Eighth"
set "day9=Nineth"
set "day10=Tenth"
set "day11=Eleventh"
set "day12=Twelveth"

::Pseudo-array for Gifts
set "gift12=Twelve drummers drumming"
set "gift11=Eleven pipers piping"
set "gift10=Ten loards a-leaping"
set "gift9=Nine ladies dancing"
set "gift8=Eight maids a-milking"
set "gift7=Seven swans a-swimming"
set "gift6=Six geese a-laying"
set "gift5=Five golden rings"
set "gift4=Four calling birds"
set "gift3=Three french hens"
set "gift2=Two turtle doves"
set "gift1=A partridge in a pear tree"

::Display It!
setlocal enabledelayedexpansion
for /l %%i in (1,1,12) do (
	echo On the !day%%i! day of Christmas
        echo My true love gave to me:

	for /l %%j in (%%i,-1,1) do (
		if %%j equ 1 (
			if %%i neq 1 <nul set /p ".=And "
			echo !gift1!.
		) else (
			echo !gift%%j!,
		)
	)
	echo(
)
exit /b
```



## Bracmat


```bracmat
(     first
      second
      third
      fourth
      fifth
      sixth
      seventh
      eighth
      ninth
      tenth
      eleventh
      twelveth
  : ?days
&     "A partridge in a pear tree."
      "Two turtle doves and"
      "Three french hens,"
      "Four calling birds,"
      "Five golden rings,"
      "Six geese a-laying,"
      "Seven swans a-swimming,"
      "Eight maids a-milking,"
      "Nine ladies dancing,"
      "Ten lords a-leaping,"
      "Eleven pipers piping,"
      "Twelve drummers drumming,"
  : ?gifts
& :?given
&   whl
  ' ( !gifts:%?gift ?gifts
    & !gift \n !given:?given
    & !days:%?day ?days
    &   out
      $ ( str
        $ ("\nOn the " !day " day of Christmas my true love gave to me:
" !given)
        )
    )
);
```



## Befunge

This is essentially the same algorithm as [[Old_lady_swallowed_a_fly#Befunge|Old lady swallowed a fly]] - just a different set of phrases and a simpler song pattern.

```befunge
0246*+00p20#v_:#`2#g+#0:#0<>\#%"O"/#:3#:+#< g48*- >1-:!#v_\1+::"O"%\"O"/v
>-#11#\0#50#< g2-:00p4v   >\#%"O"/#::$#<3#$+g48*-v^\,+*+ 55!:*!!-"|":g+3<
             ^02_>#`>#< 2 5 3 1 0 \1-:#^\_^#:-1\+<00_@#:>#<$<
(On the ?|A partridge in a pear tree.||&first% andL day of Christmas,|My true l
ove gave to me:2|Two turtle doves'second3|Three french hens&third4|Four calling
 birds'fourth3|Five golden rings&fifth4|Six geese a-laying&sixth8|Seven swans a
-swimming(seventh7|Eight maids a-milking'eighth5|Nine ladies dancing&ninth5|Ten
 lords a-leaping&tenth6|Eleven pipers piping)eleventh:|Twelve drummers drumming
(twelfth
```



## C


```C

#include<stdio.h>

int main()
{
    int i,j;

    char days[12][10] =
    {
        "First",
        "Second",
        "Third",
        "Fourth",
        "Fifth",
        "Sixth",
        "Seventh",
        "Eighth",
        "Ninth",
        "Tenth",
        "Eleventh",
        "Twelfth"
    };

    char gifts[12][33] =
    {
        "Twelve drummers drumming",
        "Eleven pipers piping",
        "Ten lords a-leaping",
        "Nine ladies dancing",
        "Eight maids a-milking",
        "Seven swans a-swimming",
        "Six geese a-laying",
        "Five golden rings",
        "Four calling birds",
        "Three french hens",
        "Two turtle doves",
        "And a partridge in a pear tree."
    };

    for(i=0;i<12;i++)
    {
        printf("\n\nOn the %s day of Christmas\nMy true love gave to me:",days[i]);

        for(j=i;j>=0;j--)
        {
            (i==0)?printf("\nA partridge in a pear tree."):printf("\n%s%c",gifts[11-j],(j!=0)?',':' ');
        }
    }

    return 0;
}
```



## C++


```cpp
#include <iostream>
#include <array>
#include <string>
using namespace std;

int main()
{
    const array<string, 12> days
    {
        "first",
        "second",
        "third",
        "fourth",
        "fifth",
        "sixth",
        "seventh",
        "eighth",
        "ninth",
        "tenth",
        "eleventh",
        "twelfth"
    };

    const array<string, 12> gifts
    {
        "And a partridge in a pear tree",
        "Two turtle doves",
        "Three french hens",
        "Four calling birds",
        "FIVE GOLDEN RINGS",
        "Six geese a-laying",
        "Seven swans a-swimming",
        "Eight maids a-milking",
        "Nine ladies dancing",
        "Ten lords a-leaping",
        "Eleven pipers piping",
        "Twelve drummers drumming"
    };

    for(int i = 0; i < days.size(); ++i)
    {
        cout << "On the " << days[i] << " day of christmas, my true love gave"
             << " to me\n";

        if(i == 0)
        {
            cout << "A partridge in a pear tree\n";
        }
        else
        {
            int j = i + 1;
            while(j-- > 0) cout << gifts[j] << "\n";
        }

        cout << "\n";
    }

    return 0;
}
```


## C#

```c#
using System;

public class TwelveDaysOfChristmas {

    public static void Main() {

        string[] days = new string[12] {
            "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth",
            "tenth", "eleventh", "twelfth",
        };

        string[] gifts = new string[12] {
            "A partridge in a pear tree",
            "Two turtle doves",
            "Three french hens",
            "Four calling birds",
            "Five golden rings",
            "Six geese a-laying",
            "Seven swans a-swimming",
            "Eight maids a-milking",
            "Nine ladies dancing",
            "Ten lords a-leaping",
            "Eleven pipers piping",
            "Twelve drummers drumming"
        };

        for ( int i = 0; i < 12; i++ ) {

            Console.WriteLine("On the " + days[i] + " day of Christmas, my true love gave to me");

            int j = i + 1;
            while ( j-- > 0 )
                Console.WriteLine(gifts[j]);

            Console.WriteLine();

            if ( i == 0 )
                gifts[0] = "And a partridge in a pear tree";
        }

    }

}
```



## Clojure

```clojure
(let [numbers '(first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth)

      gifts   ["And a partridge in a pear tree",   "Two turtle doves",     "Three French hens",
               "Four calling birds",               "Five gold rings",      "Six geese a-laying",
               "Seven swans a-swimming",           "Eight maids a-miling", "Nine ladies dancing",
               "Ten lords a-leaping",              "Eleven pipers piping", "Twelve drummers drumming"]

       day     (fn [n] (printf "On the %s day of Christmas, my true love sent to me\n" (nth numbers n)))]

     (day 0)
     (println  (clojure.string/replace (first gifts) "And a" "A"))
     (dorun (for [d (range 1 12)] (do
       (println)
       (day d)
       (dorun (for [n (range d -1 -1)]
         (println (nth gifts n))))))))
```

```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## COBOL

```cobol
       >>
SOURCE FREE
PROGRAM-ID. twelve-days-of-christmas.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  gifts-area VALUE "partridge in a pear tree    "
    & "Two turtle doves              "
    & "Three french hens             "
    & "Four calling birds            "
    & "FIVE GOLDEN RINGS             "
    & "Six geese a-laying            "
    & "Seven swans a-swimming        "
    & "Eight maids a-milking         "
    & "Nine ladies dancing           "
    & "Ten lords a-leaping           "
    & "Eleven pipers piping          "
    & "Twelve drummers drumming      ".
    03  gifts                           PIC X(30) OCCURS 12 TIMES
                                        INDEXED BY gift-idx.

01  ordinals-area VALUE "first     second    third     fourth    fifth     "
    & "sixth     seventh   eighth    ninth     tenth     eleventh  twelfth   ".
    03  ordinals                        PIC X(10) OCCURS 12 TIMES.

01  day-num                             PIC 99 COMP.

PROCEDURE DIVISION.
    PERFORM VARYING day-num FROM 1 BY 1 UNTIL day-num > 12
        DISPLAY "On the " FUNCTION TRIM(ordinals (day-num)) " day of Christmas,"
            " my true love gave to me"

        IF day-num = 1
            DISPLAY "A " gifts (1)
        ELSE
            PERFORM VARYING gift-idx FROM day-num BY -1 UNTIL gift-idx = 1
                DISPLAY gifts (gift-idx)
            END-PERFORM
            DISPLAY "And a " gifts (1)
        END-IF

        DISPLAY SPACE
    END-PERFORM
    .
END PROGRAM twelve-days-of-christmas.
```



## Common Lisp



```lisp
(let
   ((names '(first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth))
    (gifts '( "A partridge in a pear tree." "Two turtle doves and"    "Three French hens,"
              "Four calling birds,"         "Five gold rings,"        "Six geese a-laying,"
              "Seven swans a-swimming,"     "Eight maids a-milking,"  "Nine ladies dancing,"
              "Ten lords a-leaping,"        "Eleven pipers piping,"   "Twelve drummers drumming," )))

   (loop for day in names for i from 0 doing
     (format t "On the ~a day of Christmas, my true love sent to me:" (string-downcase day))
     (loop for g from i downto 0 doing (format t "~a~%" (nth g gifts)))
     (format t "~%~%")))
```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## Crystal

```ruby
days = "first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth".split " "
gifts = "A partridge in a pear tree
Two turtle doves and
Three french hens
Four calling birds
Five golden rings
Six geese a-laying
Seven swans a-swimming
Eight maids a-milking
Nine ladies dancing
Ten lords a-leaping
Eleven pipers piping
Twelve drummers drumming".split "\n"

days.each_with_index do |day, i|
    puts "On the #{day} day of Christmas\nMy true love gave to me:"
    gifts[0, i + 1].reverse.each &->puts(String)
    puts
end
```



## D

```d
immutable gifts =
"A partridge in a pear tree.
Two turtle doves
Three french hens
Four calling birds
Five golden rings
Six geese a-laying
Seven swans a-swimming
Eight maids a-milking
Nine ladies dancing
Ten lords a-leaping
Eleven pipers piping
Twelve drummers drumming";

immutable days = "first second third fourth fifth
                  sixth seventh eighth ninth tenth
                  eleventh twelfth";

void main() @safe {
    import std.stdio, std.string, std.range;

    foreach (immutable n, immutable day; days.split) {
        auto g = gifts.splitLines.take(n + 1).retro;
        writeln("On the ", day,
                " day of Christmas\nMy true love gave to me:\n",
                g[0 .. $ - 1].join('\n'),
                (n > 0 ? " and\n" ~ g.back : g.back.capitalize), '\n');
    }
}
```



## dc


```dc
0

d [first]                       r :n
d [A partridge in a pear tree.] r :g 1 +

d [second]                      r :n
d [Two turtle doves and]        r :g 1 +

d [third]                       r :n
d [Three French hens,]          r :g 1 +

d [fourth]                      r :n
d [Four calling birds,]         r :g 1 +

d [fifth]                       r :n
d [Five gold rings,]            r :g 1 +

d [sixth]                       r :n
d [Six geese a-laying,]         r :g 1 +

d [seventh]                     r :n
d [Seven swans a-swimming,]     r :g 1 +

d [eighth]                      r :n
d [Eight maids a-milking,]      r :g 1 +

d [ninth]                       r :n
d [Nine ladies dancing,]        r :g 1 +

d [tenth]                       r :n
d [Ten lords a-leaping,]        r :g 1 +

d [eleventh]                    r :n
d [Eleven pipers piping,]       r :g 1 +

d [twelfth]                     r :n
  [Twelve drummers drumming,]   r :g

[
  d
  ;g n
  10 P
] sp

[
  d
  0 r !<p
  1 -
  d
  0 r !<r
] sr

[
  [On the ] n
  d ;n n
  [ day of Christmas, my true love sent to me:] n
  10 P
  d
  lr x s_
  10 P
  1 +
  d
  12 r <l
] sl

0 ll x

```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## Eiffel


```Eiffel

class
	APPLICATION

create
	make

feature

	make
		do
			twelve_days_of_christmas
		end

feature {NONE}

	twelve_days_of_christmas
			-- Christmas carol: Twelve days of christmas.
		local
			i, j: INTEGER
		do
			create gifts.make_empty
			create days.make_empty
			gifts := <<"A partridge in a pear tree.", "Two turtle doves and", "Three french hens", "Four calling birds", "Five golden rings", "Six geese a-laying", "Seven swans a-swimming", "Eight maids a-milking", "Nine ladies dancing", "Ten lords a-leaping", "Eleven pipers piping", "Twelve drummers drumming", "And a partridge in a pear tree.", "Two turtle doves">>
			days := <<"first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "Twelfth">>
			from
				i := 1
			until
				i > days.count
			loop
				io.put_string ("On the " + days [i] + " day of Christmas.%N")
				io.put_string ("My true love gave to me:%N")
				from
					j := i
				until
					j <= 0
				loop
					if i = 12 and j = 2 then
						io.put_string (gifts [14] + "%N")
						io.put_string (gifts [13] + "%N")
						j := j - 1
					else
						io.put_string (gifts [j] + "%N")
					end
					j := j - 1
				end
				io.new_line
				i := i + 1
			end
		end

	gifts: ARRAY [STRING]

	days: ARRAY [STRING]

end

```


## Elena

ELENA 4.1 :

```elena
import extensions;

public program()
{
    var days := new string[]::(
            "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth",
            "tenth", "eleventh", "twelfth"
            );

    var gifts := new string[]::(
            "And a partridge in a pear tree",
            "Two turtle doves",
            "Three french hens",
            "Four calling birds",
            "Five golden rings",
            "Six geese a-laying",
            "Seven swans a-swimming",
            "Eight maids a-milking",
            "Nine ladies dancing",
            "Ten lords a-leaping",
            "Eleven pipers piping",
            "Twelve drummers drumming"
        );

    for(int i := 0, i < 12, i += 1)
    {
        console.printLine("On the ", days[i], " day of Christmas, my true love gave to me");

        if (i == 0)
        {
            console.printLine("A partridge in a pear tree")
        }
        else
        {
            for(int j := i, j >= 0, j -= 1)
            {
                console.printLine(gifts[j])
            }
        };

        console.printLine()
    }
}
```



## Elixir


```elixir
gifts = """
A partridge in a pear tree
Two turtle doves and
Three french hens
Four calling birds
Five golden rings
Six geese a-laying
Seven swans a-swimming
Eight maids a-milking
Nine ladies dancing
Ten lords a-leaping
Eleven pipers piping
Twelve drummers drumming
""" |> String.split("\n", trim: true)

days = ~w(first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth)

Enum.with_index(days) |> Enum.each(fn {day, i} ->
  IO.puts "On the #{day} day of Christmas"
  IO.puts "My true love gave to me:"
  Enum.take(gifts, i+1) |> Enum.reverse |> Enum.each(&IO.puts &1)
  IO.puts ""
end)
```


```txt

On the first day of Christmas
My true love gave to me:
A partridge in a pear tree

On the second day of Christmas
My true love gave to me:
Two turtle doves and
A partridge in a pear tree

...

On the twelfth day of Christmas
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree
```



## Erlang


```erlang
-module(twelve_days).
-export([gifts_for_day/1]).

names(N) -> lists:nth(N,
              ["first",   "second", "third", "fourth", "fifth",    "sixth",
               "seventh", "eighth", "ninth", "tenth",  "eleventh", "twelfth" ]).

gifts() -> [ "A partridge in a pear tree.", "Two turtle doves and",
             "Three French hens,",          "Four calling birds,",
             "Five gold rings,",            "Six geese a-laying,",
             "Seven swans a-swimming,",     "Eight maids a-milking,",
             "Nine ladies dancing,",        "Ten lords a-leaping,",
             "Eleven pipers piping,",       "Twelve drummers drumming," ].

gifts_for_day(N) ->
  "On the " ++ names(N) ++ " day of Christmas, my true love sent to me:\n" ++
  string:join(lists:reverse(lists:sublist(gifts(), N)), "\n").

main(_) -> lists:map(fun(N) -> io:fwrite("~s~n~n", [gifts_for_day(N)]) end,
                     lists:seq(1,12)).

```

```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



=={{header|F Sharp|F#}}==

```fsharp
let gifts = [
    "And a partridge in a pear tree";
    "Two turtle doves";
    "Three french hens";
    "Four calling birds";
    "FIVE GOLDEN RINGS";
    "Six geese a-laying";
    "Seven swans a-swimming";
    "Eight maids a-milking";
    "Nine ladies dancing";
    "Ten lords a-leaping";
    "Eleven pipers piping";
    "Twelve drummers drumming"
]

let days = [
    "first"; "second"; "third"; "fourth"; "fifth"; "sixth"; "seventh"; "eighth";
    "ninth"; "tenth"; "eleventh"; "twelfth"
]

let displayGifts day =
    printfn "On the %s day of Christmas, my true love gave to me" days.[day]
    if day = 0 then
        printfn "A partridge in a pear tree"
    else
        List.iter (fun i -> printfn "%s" gifts.[i]) [day..(-1)..0]
    printf "\n"

List.iter displayGifts [0..11]
```



## Factor


```factor
USING: formatting io kernel math math.ranges qw sequences ;
IN: rosetta-code.twelve-days-of-christmas

CONSTANT: opener
    "On the %s day of Christmas, my true love sent to me:\n"

CONSTANT: ordinals qw{
    first second third fourth fifth sixth seventh eighth ninth
    tenth eleventh twelfth
}

CONSTANT: gifts {
    "A partridge in a pear tree."
    "Two turtle doves, and"
    "Three french hens,"
    "Four calling birds,"
    "Five golden rings,"
    "Six geese a-laying,"
    "Seven swans a-swimming,"
    "Eight maids a-milking,"
    "Nine ladies dancing,"
    "Ten lords a-leaping,"
    "Eleven pipers piping,"
    "Twelve drummers drumming,"
}

: descend ( n -- ) 0 [a,b] [ gifts nth print ] each nl ;

: verse ( n -- )
    1 - [ ordinals nth opener printf ] [ descend ] bi ;

: twelve-days-of-christmas ( -- ) 12 [1,b] [ verse ] each ;

MAIN: twelve-days-of-christmas
```

```txt

On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves, and
A partridge in a pear tree.

...

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

```



## Forth

```forth
create ordinals s" first" 2, s" second" 2, s" third"    2, s" fourth" 2,
                s" fifth" 2, s" sixth"  2, s" seventh"  2, s" eighth" 2,
                s" ninth" 2, s" tenth"  2, s" eleventh" 2, s" twelfth" 2,
: ordinal ordinals swap 2 * cells + 2@ ;

create gifts s" A partridge in a pear tree." 2,
             s" Two turtle doves and" 2,
             s" Three French hens," 2,
             s" Four calling birds," 2,
             s" Five gold rings," 2,
             s" Six geese a-laying," 2,
             s" Seven swans a-swimming," 2,
             s" Eight maids a-milking," 2,
             s" Nine ladies dancing," 2,
             s" Ten lords a-leaping," 2,
             s" Eleven pipers piping," 2,
             s" Twelve drummers drumming," 2,
: gift gifts swap 2 * cells + 2@ ;

: day
  s" On the " type
  dup ordinal type
  s"  day of Christmas, my true love sent to me:" type
  cr
  -1 swap -do
    i gift type cr
  1 -loop
  cr
  ;

: main
  12 0 do i day loop
;

main
bye

```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## Fortran

```fortran
      program twelve_days

      character days(12)*8
      data days/'first', 'second', 'third',    'fourth',
     c          'fifth', 'sixth',  'seventh',  'eighth',
     c          'ninth', 'tenth',  'eleventh', 'twelfth'/

      character gifts(12)*27
      data gifts/'A partridge in a pear tree.',
     c           'Two turtle doves and',
     c           'Three French hens,',
     c           'Four calling birds,',
     c           'Five gold rings,',
     c           'Six geese a-laying,',
     c           'Seven swans a-swimming,',
     c           'Eight maids a-milking,',
     c           'Nine ladies dancing,',
     c           'Ten lords a-leaping,',
     c           'Eleven pipers piping,',
     c           'Twelve drummers drumming,'/

      integer day, gift

      do 10 day=1,12
        write (*,*) 'On the ', trim(days(day)),
     c              ' day of Christmas, my true love sent to me:'
        do 20 gift=day,1,-1
          write (*,*) trim(gifts(gift))
  20    continue
        write(*,*)
  10  continue
      end
```


 {{Out}}

```txt
 On the first day of Christmas, my true love sent to me:
 A partridge in a pear tree.

 On the second day of Christmas, my true love sent to me:
 Two turtle doves and
 A partridge in a pear tree.

 [...]

 On the twelfth day of Christmas, my true love sent to me:
 Twelve drummers drumming,
 Eleven pipers piping,
 Ten lords a-leaping,
 Nine ladies dancing,
 Eight maids a-milking,
 Seven swans a-swimming,
 Six geese a-laying,
 Five gold rings,
 Four calling birds,
 Three French hens,
 Two turtle doves and
 A partridge in a pear tree.
```


## FreeBASIC


```freebasic
' version 10-01-2017
' compile with: fbc -s console

Dim As ULong d, r

Dim As String days(1 To ...) = { "first", "second", "third", "fourth", _
                                "fifth", "sixth", "seventh", "eighth", _
                              "ninth", "tenth", "eleventh", "twelfth" }

Dim As String gifts(1 To ...) = { "", " Two turtle doves", _
              " Three french hens", " Four calling birds", _
              " Five golden rings", " Six geese a-laying", _
      " Seven swans a-swimming", " Eight maids a-milking", _
           " Nine ladies dancing", " Ten lords a-leaping", _
     " Eleven pipers piping", " Twelve drummers drumming" }

For d = 1 To 12
    Print " On the " + days(d) + " day of Christmas"
    Print " My true love gave to me:"
    For r = d To 3 Step -1
        Print gifts(r)
    Next
    ' print " Two turtle doves" for the twelfth day and add "and" for the other days
    If d > 1 Then
        Print gifts(2); iif(d = 12, "", " and")
    End If
    ' print "A partridge...", on the twelfth day print "And a partrige..."
    Print " A" & IIf(d = 12, "nd a", "" ) & " partridge in a pear tree"
    Print
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : 'Print "hit any key to end program"
Sleep
End
```

```txt
 On the first day of Christmas
 My true love gave to me:
 A partridge in a pear tree

 On the second day of Christmas
 My true love gave to me:
 Two turtle doves and
 A partridge in a pear tree

 '''

 On the twelfth day of Christmas
 My true love gave to me:
 Twelve drummers drumming
 Eleven pipers piping
 Ten lords a-leaping
 Nine ladies dancing
 Eight maids a-milking
 Seven swans a-swimming
 Six geese a-laying
 Five golden rings
 Four calling birds
 Three french hens
 Two turtle doves
 And a partridge in a pear tree
```



## Go

[https://play.golang.org/p/dnDyx8ee_G Go Playground]

```go
package main

import (
	"fmt"
)

func main() {
	days := []string{"first", "second", "third", "fourth", "fifth", "sixth",
		"seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"}

	gifts := []string{"A Patridge in a Pear Tree", "Two Turtle Doves and", "Three French Hens",
		"Four Calling Birds", "Five Golden Rings", "Six Geese a Laying",
		"Seven Swans a Swimming", "Eight Maids a Milking", "Nine Ladies Dancing",
		"Ten Lords a Leaping", "Eleven Pipers Piping", "Twelve Drummers Drumming"}

	for i := 0; i < 12; i++ {
		fmt.Printf("On the %s day of Christmas,\n", days[i])
		fmt.Println("My true love gave to me:")

		for j := i; j >= 0; j-- {
			fmt.Println(gifts[j])
		}
		fmt.Println()
	}
}
```



## Groovy


```groovy
def presents = ['A partridge in a pear tree.', 'Two turtle doves', 'Three french hens', 'Four calling birds',
        'Five golden rings', 'Six geese a-laying', 'Seven swans a-swimming', 'Eight maids a-milking',
        'Nine ladies dancing', 'Ten lords a-leaping', 'Eleven pipers piping', 'Twelve drummers drumming']
['first', 'second', 'third', 'forth', 'fifth', 'sixth', 'seventh', 'eight', 'ninth', 'tenth', 'eleventh', 'Twelfth'].eachWithIndex{ day, dayIndex ->
    println "On the $day day of Christmas"
    println 'My true love gave to me:'
    (dayIndex..0).each { p ->
        print presents[p]
        println p == 1 ? ' and' : ''
    }
    println()
}
```



## Haskell

```haskell
gifts :: [String]
gifts = [
    "And a partridge in a pear tree!",
    "Two turtle doves,",
    "Three french hens,",
    "Four calling birds,",
    "FIVE GOLDEN RINGS,",
    "Six geese a-laying,",
    "Seven swans a-swimming,",
    "Eight maids a-milking,",
    "Nine ladies dancing,",
    "Ten lords a-leaping,",
    "Eleven pipers piping,",
    "Twelve drummers drumming," ]

days :: [String]
days = [
    "first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth",
    "ninth", "tenth", "eleventh", "twelfth" ]

verseOfTheDay :: Int -> IO ()
verseOfTheDay day = do
    putStrLn $ "On the " ++ days !! day ++ " day of Christmas my true love gave to me... "
    mapM_ putStrLn [dayGift day d | d <- [day, day-1..0]]
    putStrLn ""
    where dayGift 0 _ = "A partridge in a pear tree!"
          dayGift _ gift = gifts !! gift

main :: IO ()
main = mapM_ verseOfTheDay [0..11]

```

<pre style="font-size:80%">On the first day of Christmas my true love gave to me...
A partridge in a pear tree!

On the second day of Christmas my true love gave to me...
Two turtle doves,
And a partridge in a pear tree!

On the third day of Christmas my true love gave to me...
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the fourth day of Christmas my true love gave to me...
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the fifth day of Christmas my true love gave to me...
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the sixth day of Christmas my true love gave to me...
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the seventh day of Christmas my true love gave to me...
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the eighth day of Christmas my true love gave to me...
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the ninth day of Christmas my true love gave to me...
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the tenth day of Christmas my true love gave to me...
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the eleventh day of Christmas my true love gave to me...
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!

On the twelfth day of Christmas my true love gave to me...
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree!


```


=={{header|Icon}} and {{header|Unicon}}==

Works in both languages.

```unicon
procedure main()
    days := ["first","second","third","fourth","fifth","sixth","seventh",
             "eighth","ninth","tenth","eleventh","twelveth"]
    gifts := ["A partridge in a pear tree.", "Two turtle doves and",
              "Three french hens,", "Four calling birds,",
              "Five golden rings,", "Six geese a-laying,",
              "Seven swans a-swimming,", "Eight maids a-milking,",
              "Nine ladies dancing,", "Ten lords a-leaping,",
              "Eleven pipers piping,", "Twelve drummers drumming,"]

    every write("\nOn the ",days[day := 1 to 12]," day of Christmas my true love gave to me:") do
        every write(" ",gifts[day to 1 by -1])
end
```



## J


```j
require 'strings'  NB. not necessary for versions > j6

days=: ;:'first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth'

gifts=: <;.2 ] 0 : 0
And a partridge in a pear tree.
Two turtle doves,
Three french hens,
Four calling birds,
Five golden rings,
Six geese a-laying,
Seven swans a-swimming,
Eight maids a-milking,
Nine ladies dancing,
Ten lords a-leaping,
Eleven pipers piping,
Twelve drummers drumming,
)

firstline=: 'On the ' , ,&(' day of Christmas, my true love gave to me',LF)

chgFirstVerse=: rplc&('nd a';'')&.>@{. , }.

makeVerses=: [: chgFirstVerse (firstline&.> days) ,&.> [: <@;@|.\ gifts"_

singCarol=: LF joinstring makeVerses
```



## Java


```java
public class TwelveDaysOfChristmas {

    final static String[] gifts = {
        "A partridge in a pear tree.", "Two turtle doves and",
        "Three french hens", "Four calling birds",
        "Five golden rings", "Six geese a-laying",
        "Seven swans a-swimming", "Eight maids a-milking",
        "Nine ladies dancing", "Ten lords a-leaping",
        "Eleven pipers piping", "Twelve drummers drumming",
        "And a partridge in a pear tree.", "Two turtle doves"
    };

    final static String[] days = {
        "first", "second", "third", "fourth", "fifth", "sixth", "seventh",
        "eighth", "ninth", "tenth", "eleventh", "Twelfth"
    };

    public static void main(String[] args) {
        for (int i = 0; i < days.length; i++) {
            System.out.printf("%nOn the %s day of Christmas%n", days[i]);
            System.out.println("My true love gave to me:");
            for (int j = i; j >= 0; j--)
                System.out.println(gifts[i == 11 && j < 2 ? j + 12 : j]);
        }
    }
}
```



## JavaScript


```JavaScript

var days = [
    'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth',
    'tenth', 'eleventh', 'twelfth',
];

var gifts = [
    "A partridge in a pear tree",
    "Two turtle doves",
    "Three french hens",
    "Four calling birds",
    "Five golden rings",
    "Six geese a-laying",
    "Seven swans a-swimming",
    "Eight maids a-milking",
    "Nine ladies dancing",
    "Ten lords a-leaping",
    "Eleven pipers piping",
    "Twelve drummers drumming"
];

var lines, verses = [], song;

for ( var i = 0; i < 12; i++ ) {

    lines = [];
    lines[0] = "On the " + days[i] + " day of Christmas, my true love gave to me";

    var j = i + 1;
    var k = 0;
    while ( j-- > 0 )
        lines[++k] = gifts[j];


    verses[i] = lines.join('\n');

    if ( i == 0 )
        gifts[0] = "And a partridge in a pear tree";

}

song = verses.join('\n\n');
document.write(song);

```



Alternatively, in a functional style of JavaScript, we can define the ancient song "strPrepn the lstOrdinal[i] strUnit of strHoliday" as an expression, and return that expression in a human-legible and machine-parseable JSON string translation, for further analysis and processing :-)


```JavaScript
JSON.stringify(
  (function (
    strPrepn,
    strHoliday,
    strUnit,
    strRole,
    strProcess,
    strRecipient
  ) {
    var lstOrdinal =
      'first second third fourth fifth sixth\
           seventh eighth ninth tenth eleventh twelfth'
      .split(/\s+/),
      lngUnits = lstOrdinal.length,

      lstGoods =
      'A partridge in a pear tree.\
           Two turtle doves\
           Three french hens\
           Four calling birds\
           Five golden rings\
           Six geese a-laying\
           Seven swans a-swimming\
           Eight maids a-milking\
           Nine ladies dancing\
           Ten lords a-leaping\
           Eleven pipers piping\
           Twelve drummers drumming'
      .split(/\s{2,}/),

      lstReversed = (function () {
        var lst = lstGoods.slice(0);
        return (lst.reverse(), lst);
      })(),

      strProvenance = [strRole, strProcess, strRecipient + ':'].join(' '),

      strPenultimate = lstReversed[lngUnits - 2] + ' and',
      strFinal = lstGoods[0];

    return lstOrdinal.reduce(
      function (sofar, day, i) {
        return sofar.concat(
          [
            [
              [ // abstraction of line 1
                strPrepn,
                'the',
                lstOrdinal[i],
                strUnit,
                'of',
                strHoliday
              ].join(' '),
              strProvenance
            ].concat( // reversed descent through memory
              (i > 1 ? [lstGoods[i]] : []).concat(
                lstReversed.slice(
                  lngUnits - i,
                  lngUnits - 2
                )
              ).concat( // penultimate line ends with 'and'
                [
                  strPenultimate,
                  strFinal
                ].slice(i ? 0 : 1)
              )
            )
          ]
        );
      }, []
    );
  })(
    'On', 'Christmas', 'day', 'my true love', 'gave to', 'me'
  ), null, 2
);
```


Note that the Google Closure compiler's translation of this would be half the size, but rather less legible.
(It does make interesting suggestions though – the semi-colon segmentation of the verses below is a trick that might be worth remembering).


```JavaScript
JSON.stringify(function (h, k, l, f, m, n) {
  var c =
    "first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth"
    .split(" "),
    d = c.length,
    e =
    "A partridge in a pear tree.;Two turtle doves;Three french hens;Four calling birds;Five golden rings;Six geese a-laying;Seven swans a-swimming;Eight maids a-milking;Nine ladies dancing;Ten lords a-leaping;Eleven pipers piping;Twelve drummers drumming"
    .split(";"),
    g = function () {
      var b = e.slice(0);
      return b.reverse(), b;
    }(),
    p = [f, m, n + ":"].join(" "),
    q = g[d - 2] + " and",
    r = e[0];

  return c.reduce(function (b, f, a) {
    return b.concat([[[h, "the", c[a], l, "of", k].join(" "), p].concat((1 <
      a ? [e[a]] : []).concat(g.slice(d - a, d - 2)).concat([q, r].slice(a ?
      0 : 1)))]);
  }, []);
}("On", "Christmas", "day", "my true love", "gave to", "me"), null, 2);
```


Formatted JSON output (the expanded and Closure-compiled versions above both yield the same output).


```JavaScript
[
  [
    "On the first day of Christmas",
    "my true love gave to me:",
    "A partridge in a pear tree."
  ],
  [
    "On the second day of Christmas",
    "my true love gave to me:",
    "Two turtle doves and",
    "A partridge in a pear tree."
  ],
  [
    "On the third day of Christmas",
    "my true love gave to me:",
    "Three french hens",
    "Two turtle doves and",
    "A partridge in a pear tree."
  ],
  [
    "On the fourth day of Christmas",
    "my true love gave to me:",
    "Four calling birds",
    "Three french hens",
    "Two turtle doves and",
    "A partridge in a pear tree."
  ],
  [
    "On the fifth day of Christmas",
    "my true love gave to me:",
    "Five golden rings",
    "Four calling birds",
    "Three french hens",
    "Two turtle doves and",
    "A partridge in a pear tree."
  ]

//... etc.

]
```



## jq


```jq
[ "st","nd","rd" ] as $nth |
[ "a partridge in a pear tree.", "turtle doves", "French hens", "calling birds", "gold rings", "geese a-laying", "swans a-swimming", "maids a-milking", "ladies dancing", "lords a-leaping", "pipers piping", "drummers drumming" ] as $gifts |
range(12) | . as $i |
"On the " + (.+1|tostring)+if $i < ($nth|length) then $nth[$i] else "th" end + " day of Christmas, my true love gave to me\n" + if $i > 0 then [[range($i)]|reverse[]|((.+2|tostring) + " " + $gifts[.+1] + if $i > 1 then "," else "" end +"\n")]|join("") + "and " else "" end + $gifts[0] + "\n"
```


Run with
```txt
jq -rnf programfile.jq
```
 to yield this result:

```txt
On the 1st day of Christmas, my true love gave to me
a partridge in a pear tree.

On the 2nd day of Christmas, my true love gave to me
2 turtle doves
and a partridge in a pear tree.

On the 3rd day of Christmas, my true love gave to me
3 French hens,
2 turtle doves,
and a partridge in a pear tree.

[...]

On the 12th day of Christmas, my true love gave to me
12 drummers drumming,
11 pipers piping,
10 lords a-leaping,
9 ladies dancing,
8 maids a-milking,
7 swans a-swimming,
6 geese a-laying,
5 gold rings,
4 calling birds,
3 French hens,
2 turtle doves,
and a partridge in a pear tree.
```



## Jsish

Based on Javascript entry, almost identical, added unitTest.

```javascript
#!/usr/bin/env jsish
"use strict";

/* Twelve Days Of Christmas, in Jsish */
var days = [
    'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth',
    'tenth', 'eleventh', 'twelfth'
];

var gifts = [
    "A partridge in a pear tree",
    "Two turtle doves",
    "Three french hens",
    "Four calling birds",
    "Five golden rings",
    "Six geese a-laying",
    "Seven swans a-swimming",
    "Eight maids a-milking",
    "Nine ladies dancing",
    "Ten lords a-leaping",
    "Eleven pipers piping",
    "Twelve drummers drumming"
];

var lines, verses = [], song;

for ( var i = 0; i < 12; i++ ) {
    lines = [];
    lines[0] = "On the " + days[i] + " day of Christmas, my true love gave to me";

    var j = i + 1;
    var k = 0;
    while ( j-- > 0 )
        lines[++k] = gifts[j];

    verses[i] = lines.join('\n');

    if ( i == 0 )
        gifts[0] = "And a partridge in a pear tree";
}

song = verses.join('\n\n');
;song;

/*
=!EXPECTSTART!=
song ==> On the first day of Christmas, my true love gave to me
A partridge in a pear tree

On the second day of Christmas, my true love gave to me
Two turtle doves
And a partridge in a pear tree

On the third day of Christmas, my true love gave to me
Three french hens
Two turtle doves
And a partridge in a pear tree

On the fourth day of Christmas, my true love gave to me
Four calling birds
Three french hens
Two turtle doves
And a partridge in a pear tree

On the fifth day of Christmas, my true love gave to me
Five golden rings
Four calling birds
Three french hens
Two turtle doves
And a partridge in a pear tree

On the sixth day of Christmas, my true love gave to me
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
And a partridge in a pear tree

On the seventh day of Christmas, my true love gave to me
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
And a partridge in a pear tree

On the eighth day of Christmas, my true love gave to me
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
And a partridge in a pear tree

On the ninth day of Christmas, my true love gave to me
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
And a partridge in a pear tree

On the tenth day of Christmas, my true love gave to me
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
And a partridge in a pear tree

On the eleventh day of Christmas, my true love gave to me
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
And a partridge in a pear tree

On the twelfth day of Christmas, my true love gave to me
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
And a partridge in a pear tree
=!EXPECTEND!=
*/
```


The verses are in the unitTest. --U mode to show the echo mode output skipped here.

```txt
prompt$ jsish -u twelveDaysOfChristmas.jsi
[PASS] twelveDaysOfChristmas.jsi
```



## Julia


```julia
# v0.6.0

function printlyrics()
    const gifts = split("""
    A partridge in a pear tree.
    Two turtle doves
    Three french hens
    Four calling birds
    Five golden rings
    Six geese a-laying
    Seven swans a-swimming
    Eight maids a-milking
    Nine ladies dancing
    Ten lords a-leaping
    Eleven pipers piping
    Twelve drummers drumming
    """, '\n')
    const days = split("""
    first second third fourth fifth
    sixth seventh eighth ninth tenth
    eleventh twelfth""")
    for (n, day) in enumerate(days)
        g = gifts[n:-1:1]
        print("\nOn the $day day of Christmas\nMy true love gave to me:\n")
        if n == 1
            print(join(g[1:end], '\n'), '\n')
        else
            print(join(g[1:end-1], '\n'), " and\n", g[end], '\n')
        end
    end
end

printlyrics()
```


```txt
On the first day of Christmas
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas
My true love gave to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree.
```



## Kotlin

```scala
enum class Day {
    first, second, third, fourth, fifth, sixth, seventh, eighth, ninth, tenth, eleventh, twelfth;
    val header = "On the " + this + " day of Christmas, my true love sent to me\n\t"
}

fun main(x: Array<String>) {
    val gifts = listOf("A partridge in a pear tree",
                       "Two turtle doves and",
                       "Three french hens",
                       "Four calling birds",
                       "Five golden rings",
                       "Six geese a-laying",
                       "Seven swans a-swimming",
                       "Eight maids a-milking",
                       "Nine ladies dancing",
                       "Ten lords a-leaping",
                       "Eleven pipers piping",
                       "Twelve drummers drumming")

    Day.values().forEachIndexed { i, d -> println(d.header + gifts.slice(0..i).asReversed().joinToString("\n\t")) }
}
```



## Logo



```logo
make "numbers [first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth]
make "gifts   [[And a partridge in a pear tree] [Two turtle doves]     [Three French hens]
               [Four calling birds]             [Five gold rings]      [Six geese a-laying]
               [Seven swans a-swimming]         [Eight maids a-miling] [Nine ladies dancing]
               [Ten lords a-leaping]            [Eleven pipers piping] [Twelve drummers drumming]]

to nth :n
  print (sentence [On the] (item :n :numbers) [day of Christmas, my true love sent to me])
end

nth 1
print [A partridge in a pear tree]

for [d 2 12] [
  print []
  nth :d
  for [g :d 1] [
    print item :g gifts
  ]
]
bye
```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## LOLCODE

<lang>CAN HAS STDIO?
HAI 1.2

I HAS A Dayz ITZ A BUKKIT
Dayz HAS A SRS  1 ITZ "first"
Dayz HAS A SRS  2 ITZ "second"
Dayz HAS A SRS  3 ITZ "third"
Dayz HAS A SRS  4 ITZ "fourth"
Dayz HAS A SRS  5 ITZ "fifth"
Dayz HAS A SRS  6 ITZ "sixth"
Dayz HAS A SRS  7 ITZ "seventh"
Dayz HAS A SRS  8 ITZ "eighth"
Dayz HAS A SRS  9 ITZ "ninth"
Dayz HAS A SRS 10 ITZ "tenth"
Dayz HAS A SRS 11 ITZ "eleventh"
Dayz HAS A SRS 12 ITZ "twelfth"

I HAS A Prezents ITZ A BUKKIT
Prezents HAS A SRS  1 ITZ "A partridge in a pear tree"
Prezents HAS A SRS  2 ITZ "Two turtle doves"
Prezents HAS A SRS  3 ITZ "Three French hens"
Prezents HAS A SRS  4 ITZ "Four calling birds"
Prezents HAS A SRS  5 ITZ "Five gold rings"
Prezents HAS A SRS  6 ITZ "Six geese a-laying"
Prezents HAS A SRS  7 ITZ "Seven swans a-swimming"
Prezents HAS A SRS  8 ITZ "Eight maids a-milking"
Prezents HAS A SRS  9 ITZ "Nine ladies dancing"
Prezents HAS A SRS 10 ITZ "Ten lords a-leaping"
Prezents HAS A SRS 11 ITZ "Eleven pipers piping"
Prezents HAS A SRS 12 ITZ "Twelve drummers drumming"

IM IN YR Outer UPPIN YR i WILE DIFFRINT i AN 12
  I HAS A Day ITZ SUM OF i AN 1
  VISIBLE "On the " !
  VISIBLE Dayz'Z SRS Day !
  VISIBLE " day of Christmas, my true love sent to me"
  IM IN YR Inner UPPIN YR j WILE DIFFRINT j AN  Day
    I HAS A Count ITZ DIFFERENCE OF Day AN j
    VISIBLE Prezents'Z SRS Count
  IM OUTTA YR Inner
  BOTH SAEM i AN 0
  O RLY?
    YA RLY
      Prezents'Z SRS 1 R "And a partridge in a pear tree"
  OIC
  DIFFRINT i AN 11
    O RLY?
      YA RLY
        VISIBLE ""
  OIC
IM OUTTA YR Outer

KTHXBYE
```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## Lua


```Lua

local days = {
    'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth',
    'tenth', 'eleventh', 'twelfth',
}

local gifts = {
    "A partridge in a pear tree",
    "Two turtle doves",
    "Three french hens",
    "Four calling birds",
    "Five golden rings",
    "Six geese a-laying",
    "Seven swans a-swimming",
    "Eight maids a-milking",
    "Nine ladies dancing",
    "Ten lords a-leaping",
    "Eleven pipers piping",
    "Twelve drummers drumming",
}

local verses = {}

for i = 1, 12 do
    local lines = {}
    lines[1] = "On the " .. days[i] .. " day of Christmas, my true love gave to me"

    local j = i
    local k = 2
    repeat
        lines[k] = gifts[j]
        k = k + 1
        j = j - 1
    until j == 0

    verses[i] = table.concat(lines, '\n')
end

print(table.concat(verses, '\n\n'))

```



## Maple


```maple
gifts := ["Twelve drummers drumming",
		"Eleven pipers piping", "Ten lords a-leaping",
		"Nine ladies dancing", "Eight maids a-milking",
		"Seven swans a-swimming", "Six geese a-laying",
		"Five golden rings", "Four calling birds",
		"Three french hens", "Two turtle doves and", "A partridge in a pear tree"]:
days := ["first", "second", "third", "fourth", "fifth", "sixth",
		"seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"]:
for i to 12 do
	printf("On the %s day of Christmas\nMy true love gave to me:\n", days[i]);
	for j from 13-i to 12 do
		printf("%s\n", gifts[j]);
	end do;
	printf("\n");
end do;
```

```txt

On the first day of Christmas
My true love gave to me:
A partridge in a pear tree

On the second day of Christmas
My true love gave to me:
Two turtle doves and
A partridge in a pear tree

On the third day of Christmas
My true love gave to me:
Three french hens
Two turtle doves and
A partridge in a pear tree

...

On the eleventh day of Christmas
My true love gave to me:
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the twelfth day of Christmas
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

```



## Mathematica


```Mathematica

daysarray = {"first", "second", "third", "fourth", "fifth", "sixth",
   "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"};
giftsarray = {"And a partridge in a pear tree.", "Two turtle doves",
   "Three french hens", "Four calling birds", "FIVE GOLDEN RINGS",
   "Six geese a-laying", "Seven swans a-swimming,",
   "Eight maids a-milking", "Nine ladies dancing",
   "Ten lords a-leaping", "Eleven pipers piping",
   "Twelve drummers drumming"};
Do[Print[StringForm[
   "On the `1` day of Christmas, my true love gave to me: `2`",
   daysarray[[i]],
   If[i == 1, "A partridge in a pear tree.",
    Row[Reverse[Take[giftsarray, i]], ","]]]], {i, 1, 12}]

```

<pre style="height:55ex;overflow:scroll">
On the first day of Christmas, my true love gave to me:
A partridge in a pear tree.

On the second day of Christmas, my true love gave to me:
Two turtle doves,
And a partridge in a pear tree.

On the third day of Christmas, my true love gave to me:
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the fourth day of Christmas, my true love gave to me:
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the fifth day of Christmas, my true love gave to me:
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the sixth day of Christmas, my true love gave to me:
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the seventh day of Christmas, my true love gave to me:
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the eighth day of Christmas, my true love gave to me:
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the ninth day of Christmas, my true love gave to me:
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the tenth day of Christmas, my true love gave to me:
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the eleventh day of Christmas, my true love gave to me:
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the twelfth day of Christmas, my true love gave to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GOLDEN RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

```



## MiniScript


```MiniScript
days = ["first","second","third", "fourth","fifth","sixth",
        "seventh","eigth","nineth","tenth","eleventh","twelfth"]
gifts = ["A partridge in a pear tree.","Two turtle doves, and",
        "Three French hens,","Four calling birds,",
        "Five gold rings,","Six geese a-laying,",
        "Seven swans a-swimming,","Eight maids a-milking,",
        "Nine ladies dancing,","Ten lords a-leaping,",
        "Eleven pipers piping,","Twelve drummers drumming,"]

for i in range(0,11)
    print "On the " + days[i] + " day of Christmas,"
    print "my true love gave to me,"
    for j in range(i,0)
        print " " + gifts[j]
    end for
    print "     ----------"
end for

```

```txt

On the first day of Christmas,
my true love gave to me,
A partridge in a pear tree.
----------

<and so on until the last>

On the twelfth day of Christmas,
my true love gave to me,
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves, and
A partridge in a pear tree.
----------

```



## Nim

```nim
import strutils, algorithm

const
  gifts = """A partridge in a pear tree.
Two turtle doves
Three french hens
Four calling birds
Five golden rings
Six geese a-laying
Seven swans a-swimming
Eight maids a-milking
Nine ladies dancing
Ten lords a-leaping
Eleven pipers piping
Twelve drummers drumming""".splitLines()

  days = "first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth".split(' ')

for n, day in days:
  var g = (gifts[0..n])
  reverse(g)
  echo "\nOn the ", day, " day of Christmas\nMy true love gave to me:\n" &
    g[0 .. -2].join("\n") &
    (if n > 0: " and\n" & g[g.high] else: capitalize(g[g.high]))
```



## Objeck

```objeck

class TwelveDaysOfChristmas  {
  function : Main(args : String[]) ~ Nil {
    days := ["first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth",
            "tenth", "eleventh", "twelfth"];

        gifts := ["A partridge in a pear tree",
            "Two turtle doves",
            "Three french hens",
            "Four calling birds",
            "Five golden rings",
            "Six geese a-laying",
            "Seven swans a-swimming",
            "Eight maids a-milking",
            "Nine ladies dancing",
            "Ten lords a-leaping",
            "Eleven pipers piping",
            "Twelve drummers drumming"];

        for(i := 0; i < days->Size(); i+=1;) {
          IO.Console->Print("On the ")->Print(days[i])->PrintLine(" day of Christmas, my true love gave to me");

          j := i + 1;
          while(j > 0 ) {
            j -= 1;
            gifts[j]->PrintLine();
          };

           IO.Console->PrintLine();

           if (i = 0) {
             gifts[0] := "And a partridge in a pear tree";
           };
        };
    }
}

```



## PARI/GP


```parigp
days=["first","second","third","fourth","fifth","sixth","seventh","eighth","ninth","tenth","eleventh","twelfth"];
gifts=["And a partridge in a pear tree.", "Two turtle doves", "Three french hens", "Four calling birds", "Five golden rings", "Six geese a-laying", "Seven swans a-swimming", "Eight maids a-milking", "Nine ladies dancing", "Ten lords a-leaping", "Eleven pipers piping", "Twelve drummers drumming"];
{
for(i=1,#days,
  print("On the "days[i]" day of Christmas, my true love gave to me:");
  forstep(j=i,2,-1,print("\t"gifts[j]", "));
  print(if(i==1,"\tA partridge in a pear tree.",Str("\t",gifts[1])))
)
}
```

```txt
On the first day of Christmas, my true love gave to me:
        A partridge in a pear tree.
On the second day of Christmas, my true love gave to me:
        Two turtle doves,
        And a partridge in a pear tree.
[...]
On the twelfth day of Christmas, my true love gave to me:
        Twelve drummers drumming,
        Eleven pipers piping,
        Ten lords a-leaping,
        Nine ladies dancing,
        Eight maids a-milking,
        Seven swans a-swimming,
        Six geese a-laying,
        Five golden rings,
        Four calling birds,
        Three french hens,
        Two turtle doves,
        And a partridge in a pear tree.
```



## Pascal

This should work with any modern Pascal implementation that has a '''string''' type, e.g. [[Free Pascal]].


```pascal
program twelve_days(output);

const
  days:  array[1..12] of string =
    ( 'first',   'second', 'third', 'fourth', 'fifth',    'sixth',
      'seventh', 'eighth', 'ninth', 'tenth',  'eleventh', 'twelfth' );

  gifts: array[1..12] of string =
    ( 'A partridge in a pear tree.',
      'Two turtle doves and',
      'Three French hens,',
      'Four calling birds,',
      'Five gold rings,',
      'Six geese a-laying,',
      'Seven swans a-swimming,',
      'Eight maids a-milking,',
      'Nine ladies dancing,',
      'Ten lords a-leaping,',
      'Eleven pipers piping,',
      'Twelve drummers drumming,' );

var
   day, gift: integer;

begin
   for day := 1 to 12 do begin
     writeln('On the ', days[day], ' day of Christmas, my true love sent to me:');
     for gift := day downto 1 do
       writeln(gifts[gift]);
     writeln
   end
end.
```

```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```


Here's a version that works in ISO Standard Pascal, albeit with extraneous spaces in the output:


```pascal
program twelve_days_iso(output);

const
  days:  array[1..12, 1..8] of char =
    ( 'first   ', 'second  ', 'third   ', 'fourth  ',
      'fifth   ', 'sixth   ', 'seventh ', 'eighth  ',
      'ninth   ', 'tenth   ', 'eleventh', 'twelfth ' );

  gifts: array[1..12, 1..27] of char =
    ( 'A partridge in a pear tree.',
      'Two turtle doves and       ',
      'Three French hens,         ',
      'Four calling birds,        ',
      'Five gold rings,           ',
      'Six geese a-laying,        ',
      'Seven swans a-swimming,    ',
      'Eight maids a-milking,     ',
      'Nine ladies dancing,       ',
      'Ten lords a-leaping,       ',
      'Eleven pipers piping,      ',
      'Twelve drummers drumming,  ' );

var
   day, gift: integer;

begin
   for day := 1 to 12 do begin
     writeln('On the ', days[day], ' day of Christmas, my true love gave to me:');
     for gift := day downto 1 do
       writeln(gifts[gift]);
     writeln
   end
end.
```


```txt
On the first    day of Christmas, my true love gave to me:
A partridge in a pear tree.

On the second   day of Christmas, my true love gave to me:
Two turtle doves and
A partridge in a pear tree.
```


[...]


```txt
On the twelfth  day of Christmas, my true love gave to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## Perl


```perl
use v5.10;

my @days = qw{ first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth };

chomp ( my @gifts = grep { /\S/ } <DATA> );

while ( my $day = shift @days ) {
    say "On the $day day of Christmas,\nMy true love gave to me:";
    say for map { $day eq 'first' ? s/And a/A/r : $_ } @gifts[@days .. @gifts-1];
    say "";
}

__DATA__
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
And a partridge in a pear tree.
```

```txt
On the first day of Christmas,
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas,
My true love gave to me:
Two turtle doves
And a partridge in a pear tree.

...

On the twelfth day of Christmas,
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
And a partridge in a pear tree.
```



## Perl 6


```perl6
my @days = <first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth>
;

my @gifts = lines q:to/END/;
  And a partridge in a pear tree.
  Two turtle doves,
  Three french hens,
  Four calling birds,
  Five golden rings,
  Six geese a-laying,
  Seven swans a-swimming,
  Eight maids a-milking,
  Nine ladies dancing,
  Ten lords a-leaping,
  Eleven pipers piping,
  Twelve drummers drumming,
END

sub nth($n) { say "On the @days[$n] day of Christmas, my true love gave to me:" }

nth(0);
say @gifts[0].subst('And a','A');

for 1 ... 11 -> $d {
    say '';
    nth($d);
    say @gifts[$_] for $d ... 0;
}
```

```txt
On the first day of Christmas, my true love gave to me:
  A partridge in a pear tree.

On the second day of Christmas, my true love gave to me:
  Two turtle doves,
  And a partridge in a pear tree.

On the third day of Christmas, my true love gave to me:
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.

On the fourth day of Christmas, my true love gave to me:
  Four calling birds,
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.
.
.
.
On the twelfth day of Christmas, my true love gave to me:
  Twelve drummers drumming,
  Eleven pipers piping,
  Ten lords a-leaping,
  Nine ladies dancing,
  Eight maids a-milking,
  Seven swans a-swimming,
  Six geese a-laying,
  Five golden rings,
  Four calling birds,
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.
```



## Phix


```Phix
constant days = {"first", "second", "third", "fourth", "fifth", "sixth",
                 "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"},
         gifts = {"A partridge in a pear tree.\n",
                  "Two turtle doves, and\n",
                  "Three French hens,\n",
                  "Four calling birds,\n",
                  "Five gold rings,\n",
                  "Six geese a-laying,\n",
                  "Seven swans a-swimming,\n",
                  "Eight maids a-milking,\n",
                  "Nine ladies dancing,\n",
                  "Ten lords a-leaping,\n",
                  "Eleven pipers piping,\n",
                  "Twelve drummers drumming,\n"}
for i=1 to 12 do
    printf(1,"On the %s day of Christmas,\nmy true love gave to me:\n",{days[i]})
    for j=i to 1 by -1 do
        printf(1,gifts[j])
    end for
end for
```



## PicoLisp


```PicoLisp
(de days
   first second third fourth fifth sixth
   seventh eight ninth tenth eleventh twelfth )

(de texts
   "A partridge in a pear tree."
   "Two turtle doves and"
   "Three french hens"
   "Four calling birds"
   "Five golden rings"
   "Six geese a-laying"
   "Seven swans a-swimming"
   "Eight maids a-milking"
   "Nine ladies dancing"
   "Ten lords a-leaping"
   "Eleven pipers piping"
   "Twelve drummers drumming" )

(for (X . Day) days
   (prinl "On the " Day " day of Christmas")
   (prinl "My true love game to me:")
   (for (Y X (gt0 Y) (dec Y))
      (prinl
         (if (and (= 12 X) (= 1 Y))
            "And a partridge in a pear tree."
            (get texts Y)) ) )
   (prinl) )

(bye)
```



## PHP


```PHP

<?php

header("Content-Type: text/plain; charset=UTF-8");

$days = array(
    'first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth',
    'tenth', 'eleventh', 'twelfth',
);

$gifts = array(
    "A partridge in a pear tree",
    "Two turtle doves",
    "Three french hens",
    "Four calling birds",
    "Five golden rings",
    "Six geese a-laying",
    "Seven swans a-swimming",
    "Eight maids a-milking",
    "Nine ladies dancing",
    "Ten lords a-leaping",
    "Eleven pipers piping",
    "Twelve drummers drumming"
);

$verses = [];

for ( $i = 0; $i < 12; $i++ ) {
    $lines = [];
    $lines[0] = "On the {$days[$i]} day of Christmas, my true love gave to me";

    $j = $i;
    $k = 0;
    while ( $j >= 0 ) {
        $lines[++$k] = $gifts[$j];
        $j--;
    }

    $verses[$i] = implode(PHP_EOL, $lines);

    if ( $i == 0 )
        $gifts[0] = "And a partridge in a pear tree";
}

echo implode(PHP_EOL . PHP_EOL, $verses);

?>

```


Or using recursion:


```PHP
<?php

$gifts = array(
    'first'    => "A partridge in a pear tree",
    'second'   => "Two turtle doves",
    'third'    => "Three french hens",
    'fourth'   => "Four calling birds",
    'fifth'    => "Five golden rings",
    'sixth'    => "Six geese a-laying",
    'seventh'  => "Seven swans a-swimming",
    'eighth'   => "Eight maids a-milking",
    'ninth'    => "Nine ladies dancing",
    'tenth'    => "Ten lords a-leaping",
    'eleventh' => "Eleven pipers piping",
    'twelfth'  => "Twelve drummers drumming"
);

function print_day( $gifts ) {
    echo "On the ", key( $gifts ), " day of Xmas, my true love gave to me", PHP_EOL;
    foreach( $gifts as $day => $gift ) {
        echo $gift, $day == 'second' ? ' and' : '', PHP_EOL;
    }
    echo PHP_EOL;
}

function twelve_days( $gifts ) {
    if ( ! empty( $gifts ) ) {
        twelve_days( array_slice( $gifts, 1, null, true ) );
        print_day( $gifts );
    }
}

twelve_days( array_reverse( $gifts, true ) );


```

<pre style="font-size:84%;height:55ex">
On the first day of Xmas, my true love gave to me
A partridge in a pear tree

On the second day of Xmas, my true love gave to me
Two turtle doves and
A partridge in a pear tree

On the third day of Xmas, my true love gave to me
Three french hens
Two turtle doves and
A partridge in a pear tree

On the fourth day of Xmas, my true love gave to me
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the fifth day of Xmas, my true love gave to me
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the sixth day of Xmas, my true love gave to me
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the seventh day of Xmas, my true love gave to me
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the eighth day of Xmas, my true love gave to me
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the ninth day of Xmas, my true love gave to me
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the tenth day of Xmas, my true love gave to me
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the eleventh day of Xmas, my true love gave to me
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

On the twelfth day of Xmas, my true love gave to me
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

```



## PowerShell


```PowerShell
$days = @{
    1 = "first";
    2 = "second";
    3 = "third";
    4 = "fourth";
    5 = "fifth";
    6 = "sixth";
    7 = "seventh";
    8 = "eight";
    9 = "ninth";
    10 = "tenth";
    11 = "eleventh";
    12 = "twelfth";
}

$gifts = @{
    1 = 'A partridge in a pear tree';
    2 = 'Two turtle doves';
    3 = 'Three french hens';
    4 = 'Four calling birds';
    5 = 'Five golden rings';
    6 = 'Six geese a-laying';
    7 = 'Seven swans a-swimming';
    8 = 'Eight maids a-milking';
    9 = 'Nine ladies dancing';
    10 = 'Ten lords a-leaping';
    11 = 'Eleven pipers piping';
    12 = 'Twelve drummers drumming';
}

1 .. 12 | % {
    "On the $($days[$_]) day of Christmas`nMy true love gave to me"
    $_ .. 1 | % {
        $gift = $gifts[$_]
        if ($_ -eq 2) { $gift += " and" }
        $gift
    }
    ""
}
```


```txt
On the first day of Christmas
My true love gave to me
A partridge in a pear tree

On the second day of Christmas
My true love gave to me
Two turtle doves and
A partridge in a pear tree

...

On the twelfth day of Christmas
My true love gave to me
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree

```



## Prolog


```prolog
day(1, 'first').
day(2, 'second').
day(3, 'third').
day(4, 'fourth').
day(5, 'fifth').
day(6, 'sixth').
day(7, 'seventh').
day(8, 'eighth').
day(9, 'ninth').
day(10, 'tenth').
day(11, 'eleventh').
day(12, 'twelfth').

gift(1, 'A partridge in a pear tree.').
gift(2, 'Two turtle doves and').
gift(3, 'Three French hens,').
gift(4, 'Four calling birds,').
gift(5, 'Five gold rings,').
gift(6, 'Six geese a-laying,').
gift(7, 'Seven swans a-swimming,').
gift(8, 'Eight maids a-milking,').
gift(9, 'Nine ladies dancing,').
gift(10, 'Ten lords a-leaping,').
gift(11, 'Eleven pipers piping,').
gift(12, 'Twelve drummers drumming,').

giftsFor(0, []) :- !.
giftsFor(N, [H|T]) :- gift(N, H), M is N-1, giftsFor(M,T).

writeln(S) :- write(S), write('\n').

writeList([])    :- writeln(''), !.
writeList([H|T]) :- writeln(H), writeList(T).

writeGifts(N) :- day(N, Nth), write('On the '), write(Nth),
    writeln(' day of Christmas, my true love sent to me:'),
    giftsFor(N,L), writeList(L).

writeLoop(0) :- !.
writeLoop(N) :- Day is 13 - N, writeGifts(Day), M is N - 1, writeLoop(M).

main :- writeLoop(12), halt.
```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## PureBasic


```PureBasic
#TXT$ = "On the * day of Christmas, my true love sent to me:"
days$ = ~"first\nsecond\nthird\nfourth\nfifth\nsixth\nseventh\neighth\nninth\ntenth\neleventh\ntwelfth\n"
gifts$= ~"Twelve drummers drumming,\nEleven pipers piping,\nTen lords a-leaping,\nNine ladies dancing,\n"+
        ~"Eight maids a-milking,\nSeven swans a-swimming,\nSix geese a-laying,\nFive golden rings,\n"+
        ~"Four calling birds,\nThree french hens,\nTwo turtle doves,\nA partridge in a pear tree.\n"
Define  I.i, J.i

If OpenConsole("The twelve days of Christmas")
  For I = 1 To 12
    PrintN(ReplaceString(#TXT$,"*",StringField(days$,I,~"\n")))
    For J = 13-I To 12
      PrintN(" -> "+StringField(gifts$,J,~"\n"))
    Next J
  Next I
  Input()
EndIf
```

```txt
On the first day of Christmas, my true love sent to me:
 -> A partridge in a pear tree.
On the second day of Christmas, my true love sent to me:
 -> Two turtle doves,
 -> A partridge in a pear tree.
.
.
.
On the eleventh day of Christmas, my true love sent to me:
 -> Eleven pipers piping,
 -> Ten lords a-leaping,
 -> Nine ladies dancing,
 -> Eight maids a-milking,
 -> Seven swans a-swimming,
 -> Six geese a-laying,
 -> Five golden rings,
 -> Four calling birds,
 -> Three french hens,
 -> Two turtle doves,
 -> A partridge in a pear tree.
On the twelfth day of Christmas, my true love sent to me:
 -> Twelve drummers drumming,
 -> Eleven pipers piping,
 -> Ten lords a-leaping,
 -> Nine ladies dancing,
 -> Eight maids a-milking,
 -> Seven swans a-swimming,
 -> Six geese a-laying,
 -> Five golden rings,
 -> Four calling birds,
 -> Three french hens,
 -> Two turtle doves,
 -> A partridge in a pear tree.
```



## Python


```python
gifts = '''\
A partridge in a pear tree.
Two turtle doves
Three french hens
Four calling birds
Five golden rings
Six geese a-laying
Seven swans a-swimming
Eight maids a-milking
Nine ladies dancing
Ten lords a-leaping
Eleven pipers piping
Twelve drummers drumming'''.split('\n')

days = '''first second third fourth fifth
          sixth seventh eighth ninth tenth
          eleventh twelfth'''.split()

for n, day in enumerate(days, 1):
    g = gifts[:n][::-1]
    print(('\nOn the %s day of Christmas\nMy true love gave to me:\n' % day) +
          '\n'.join(g[:-1]) +
          (' and\n' + g[-1] if n > 1 else g[-1].capitalize()))
```


```txt
On the first day of Christmas
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas
My true love gave to me:
Two turtle doves and
A partridge in a pear tree.

On the third day of Christmas
My true love gave to me:
Three french hens
Two turtle doves and
A partridge in a pear tree.

On the fourth day of Christmas
My true love gave to me:
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree.
.
.
.

On the twelfth day of Christmas
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree.
```



## Racket


This version:
* doesn't capitalise the word '''Twelfth'''
* capitalises the '''French'''   (in French hen)
* capitalised '''True Love''' as it may save me a lot of grief when I get home.
* British Variant: changes '''golden''' to '''go-old''' rings. Anyone who still has enough breath left to sing the second syllable after sustaining the first syllable of '''golden''' simply wasn't making enough effort in the first place.
* British Variant: capitalises '''FIVE GO-OLD RINGS''' since it needs to be sung at top volume. If you want to change this back; the source is there. But I guarantee you won't have as much fun singing it.


```racket
#lang racket
(define (ordinal-text d)
  (vector-ref
   (vector
    "zeroth" "first" "second" "third" "fourth"
    "fifth" "sixth" "seventh" "eighth" "ninth"
    "tenth" "eleventh" "twelfth")
   d))

(define (on-the... day)
  (printf "On the ~a day of Christmas,~%" (ordinal-text day))
  (printf "My True Love gave to me,~%"))

(define (prezzy prezzy-line day)
  (match prezzy-line
    [1 (string-append (if (= day 1) "A " "And a")" partridge in a pear tree")]
    [2 "Two turtle doves"]
    [3 "Three French hens"]
    [4 "Four calling birds"]
    [5 "FIVE GO-OLD RINGS"]
    [6 "Six geese a-laying"]
    [7 "Seven swans a-swimming"]
    [8 "Eight maids a-milking"]
    [9 "Nine ladies dancing"]
    [10 "Ten lords a-leaping"]
    [11 "Eleven pipers piping"]
    [12 "Twelve drummers drumming"]))

(define (line-end prezzy-line day)
  (match* (day prezzy-line) [(12 1) "."] [(x 1) ".\n"] [(_ _) ","]))

(for ((day (sequence-map add1 (in-range 12)))
      #:when (on-the... day)
      (prezzy-line (in-range day 0 -1)))
  (printf "~a~a~%" (prezzy prezzy-line day) (line-end prezzy-line day)))
```


```txt
On the first day of Christmas,
My True Love gave to me,
A  partridge in a pear tree.

On the second day of Christmas,
My True Love gave to me,
Two turtle doves,
And a partridge in a pear tree.

On the third day of Christmas,
My True Love gave to me,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the fourth day of Christmas,
My True Love gave to me,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the fifth day of Christmas,
My True Love gave to me,
FIVE GO-OLD RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the sixth day of Christmas,
My True Love gave to me,
Six geese a-laying,
FIVE GO-OLD RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.

On the seventh day of Christmas, ...
On the eighth day of Christmas, ...
On the ninth day of Christmas, ...
On the tenth day of Christmas, ...
On the eleventh day of Christmas, ...

On the twelfth day of Christmas,
My True Love gave to me,
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
FIVE GO-OLD RINGS,
Four calling birds,
Three french hens,
Two turtle doves,
And a partridge in a pear tree.
```



## REXX

This version:
::*   doesn't capitalize the word   '''Twelfth'''
::*   capitalizes the   '''French'''   (in French hen)
::*   capitalized   '''True Love'''   as it (may) refer to a deity
::*   added indentation to make verses resemble song lyrics

```rexx
/*REXX program displays the verses of the song:    "The 12 days of Christmas".          */
ordD= 'first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth'
pad= left('', 20)                                /*used for indenting the shown verses. */
                   @.1= 'A partridge in a pear-tree.';   @.7 = "Seven swans a-swimming,"
                   @.2= 'Two Turtle Doves, and'      ;   @.8 = "Eight maids a-milking,"
                   @.3= 'Three French Hens,'         ;   @.9 = "Nine ladies dancing,"
                   @.4= 'Four Calling Birds,'        ;   @.10= "Ten lords a-leaping,"
                   @.5= 'Five Golden Rings,'         ;   @.11= "Eleven pipers piping,"
                   @.6= 'Six geese a-laying,'        ;   @.12= "Twelve drummers drumming,"
  do day=1  for 12
  say pad  'On the'   word(ordD, day)   "day of Christmas"    /*display line 1 prologue.*/
  say pad  'My True Love gave to me:'                         /*   "      "  2     "    */
              do j=day  by -1  to 1;       say pad @.j        /*   "    the daily gifts.*/
              end   /*j*/
  say                                            /*add a blank line between the verses. */
  end               /*day*/                      /*stick a fork in it,  we're all done. */
```

(Shown at three-quarters size.)

<pre style="font-size:75%;height:55ex">
                     On the first day of Christmas
                     My True Love gave to me:
                     A partridge in a pear-tree.

                     On the second day of Christmas
                     My True Love gave to me:
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the third day of Christmas
                     My True Love gave to me:
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the fourth day of Christmas
                     My True Love gave to me:
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the fifth day of Christmas
                     My True Love gave to me:
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the sixth day of Christmas
                     My True Love gave to me:
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the seventh day of Christmas
                     My True Love gave to me:
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the eighth day of Christmas
                     My True Love gave to me:
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the ninth day of Christmas
                     My True Love gave to me:
                     Nine ladies dancing,
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the tenth day of Christmas
                     My True Love gave to me:
                     Ten lords a-leaping,
                     Nine ladies dancing,
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the eleventh day of Christmas
                     My True Love gave to me:
                     Eleven pipers piping,
                     Ten lords a-leaping,
                     Nine ladies dancing,
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

                     On the twelfth day of Christmas
                     My True Love gave to me:
                     Twelve drummers drumming,
                     Eleven pipers piping,
                     Ten lords a-leaping,
                     Nine ladies dancing,
                     Eight maids a-milking,
                     Seven swans a-swimming,
                     Six geese a-laying,
                     Five Golden Rings,
                     Four Calling Birds,
                     Three French Hens,
                     Two Turtle Doves, and
                     A partridge in a pear-tree.

```



## Ring


```ring

# Project : The Twelve Days of Christmas

gifts = "A partridge in a pear tree,Two turtle doves,Three french hens,Four calling birds,Five golden rings,Six geese a-laying,Seven swans a-swimming,Eight maids a-milking,Nine ladies dancing,Ten lords a-leaping,Eleven pipers piping,Twelve drummers drumming"
days = "first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth"
lstgifts = str2list(substr(gifts,",", nl))
lstdays = str2list(substr(days, " ", nl))
for i = 1 to 12
     see "On the "+ lstdays[i]+ " day of Christmas" + nl
     see "My true love gave to me:" + nl
     for j = i to 1 step -1
          if i > 1 and j = 1
             see "and " + nl
          ok
          see "" + lstgifts[j] + nl
     next
     see nl
next


```

Output:

```txt

On the first day of Christmas
My true love gave to me:
A partridge in a pear tree

On the second day of Christmas
My true love gave to me:
Two turtle doves
and
A partridge in a pear tree

On the third day of Christmas
My true love gave to me:
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the fourth day of Christmas
My true love gave to me:
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the fifth day of Christmas
My true love gave to me:
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the sixth day of Christmas
My true love gave to me:
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the seventh day of Christmas
My true love gave to me:
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the eighth day of Christmas
My true love gave to me:
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the ninth day of Christmas
My true love gave to me:
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the tenth day of Christmas
My true love gave to me:
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the eleventh day of Christmas
My true love gave to me:
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

On the twelfth day of Christmas
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and
A partridge in a pear tree

```



## Ruby


```ruby
gifts = "A partridge in a pear tree
Two turtle doves and
Three french hens
Four calling birds
Five golden rings
Six geese a-laying
Seven swans a-swimming
Eight maids a-milking
Nine ladies dancing
Ten lords a-leaping
Eleven pipers piping
Twelve drummers drumming".split("\n")

days = %w(first second third fourth fifth sixth
seventh eighth ninth tenth eleventh twelfth)

days.each_with_index do |day, i|
  puts "On the #{day} day of Christmas"
  puts "My true love gave to me:"
  puts gifts[0, i+1].reverse
  puts
end
```



## Run BASIC


```Runbasic
gifts$ = "
A partridge in a pear tree.,
Two turtle doves,
Three french hens,
Four calling birds,
Five golden rings,
Six geese a-laying,
Seven swans a-swimming,
Eight maids a-milking,
Nine ladies dancing,
Ten lords a-leaping,
Eleven pipers piping,
Twelve drummers drumming"

days$ = "first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth"

for i = 1 to 12
    print "On the ";word$(days$,i," ");" day of Christmas"
    print "My true love gave to me:"
    for j = i to 1 step -1
    if i > 1 and j = 1 then print "and ";
    print mid$(word$(gifts$,j,","),2)
    next j
    print
next i

```
Output:

```txt
On the first day of Christmas
My true love gave to me:
A partridge in a pear tree.
.
.
On the twelfth day of Christmas
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
and A partridge in a pear tree.
```



## Rust

[https://play.rust-lang.org/?gist=773d4af97e7c4b374574a3e1656b5029&version=stable&backtrace=0 Rust Playground]

```rust
fn main() {
    let days = ["first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth",
                "ninth", "tenth", "eleventh", "twelfth"];

    let gifts = ["A Patridge in a Pear Tree",
                 "Two Turtle Doves and",
                 "Three French Hens",
                 "Four Calling Birds",
                 "Five Golden Rings",
                 "Six Geese a Laying",
                 "Seven Swans a Swimming",
                 "Eight Maids a Milking",
                 "Nine Ladies Dancing",
                 "Ten Lords a Leaping",
                 "Eleven Pipers Piping",
                 "Twelve Drummers Drumming"];

    for i in 0..12 {
        println!("On the {} day of Christmas,", days[i]);
        println!("My true love gave to me:");

        for j in (0..i + 1).rev() {
            println!("{}", gifts[j]);
        }
        println!()
    }
}
```



## Scala


```scala
val gifts = Array(
    "A partridge in a pear tree.",
    "Two turtle doves and",
    "Three French hens,",
    "Four calling birds,",
    "Five gold rings,",
    "Six geese a-laying,",
    "Seven swans a-swimming,",
    "Eight maids a-milking,",
    "Nine ladies dancing,",
    "Ten lords a-leaping,",
    "Eleven pipers piping,",
    "Twelve drummers drumming,"
  )

val days = Array(
    "first",   "second", "third", "fourth", "fifth",    "sixth",
    "seventh", "eighth", "ninth", "tenth",  "eleventh", "twelfth"
  )

val giftsForDay = (day: Int) =>
    "On the %s day of Christmas, my true love gave to me:\n".format(days(day)) +
      gifts.take(day+1).reverse.mkString("\n") + "\n"

(0 until 12).map(giftsForDay andThen println)
```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    const array string: gifts is [] (
        "A partridge in a pear tree.", "Two turtle doves and",
        "Three french hens", "Four calling birds",
        "Five golden rings", "Six geese a-laying",
        "Seven swans a-swimming", "Eight maids a-milking",
        "Nine ladies dancing", "Ten lords a-leaping",
        "Eleven pipers piping", "Twelve drummers drumming");
    const array string: days is [] (
        "first", "second", "third", "fourth", "fifth", "sixth",
        "seventh", "eighth", "ninth", "tenth", "eleventh", "Twelfth");
    var integer: day is 0;
    var integer: gift is 0;
  begin
    for day range 1 to length(days) do
      writeln;
      writeln("On the " <& days[day] <& " day of Christmas,");
      writeln("My true love gave to me:");
      for gift range day downto 1 do
        writeln(gifts[gift]);
      end for;
    end for;
  end func;
```


```txt

On the first day of Christmas,
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas,
My true love gave to me:
Two turtle doves and
A partridge in a pear tree.

On the third day of Christmas,
My true love gave to me:
Three french hens
Two turtle doves and
A partridge in a pear tree.

...

On the Twelfth day of Christmas,
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree.

```



## Self

Nicely factored:

```self
(|
parent* = traits oddball.

gifts = (
   'And a partridge in a pear tree' &
   'Two turtle doves' &
   'Three french hens' &
   'Four calling birds' &
   'FIVE GO-OLD RINGS' &
   'Six geese a-laying' &
   'Seven swans a-swimming' &
   'Eight maids a-milking' &
   'Nine ladies dancing' &
   'Ten lords a-leaping' &
   'Eleven pipers piping' &
   'Twelve drummers drumming'
) asSequence.

days = (
   'first' & 'second' & 'third'    & 'fourth'  &
   'fifth' & 'sixth'  & 'seventh'  & 'eighth'  &
   'ninth' & 'tenth'  & 'eleventh' & 'twelfth'
) asSequence.

intro: i = ( 'On the ', (days at: i), ' day of Christmas, my true love gave to me:').
gifts: i = (  i = 0 ifTrue: [sequence copyAddFirst: 'A partridge in a pear tree' ]
                     False: [(gifts slice: 0@(i + 1)) reverse ]).
verse: i = ( ((sequence copyAddFirst: intro: i) addAll: gifts: i) addLast: '' ).
value    = ( (days gather: [|:d. :i| verse: i ]) asSequence joinUsing: '\n' )

|) value printLine

```



## Sidef

```ruby
var days = <first second third fourth fifth sixth seventh eighth ninth tenth eleventh twelfth>
;

var gifts = <<'EOT'.lines;
  And a partridge in a pear tree.
  Two turtle doves,
  Three french hens,
  Four calling birds,
  Five golden rings,
  Six geese a-laying,
  Seven swans a-swimming,
  Eight maids a-milking,
  Nine ladies dancing,
  Ten lords a-leaping,
  Eleven pipers piping,
  Twelve drummers drumming,
EOT

func nth(n) { say "On the #{days[n]} day of Christmas, my true love gave to me:" };

nth(0);
say gifts[0].sub('And a', 'A');

range(1, 11).each { |d|
    say '';
    nth(d);
    d.downto(0).each { |i|
        say gifts[i];
    }
}
```


```txt

On the first day of Christmas, my true love gave to me:
  A partridge in a pear tree.

On the second day of Christmas, my true love gave to me:
  Two turtle doves,
  And a partridge in a pear tree.

On the third day of Christmas, my true love gave to me:
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.

On the fourth day of Christmas, my true love gave to me:
  Four calling birds,
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.
.
.
.
On the twelfth day of Christmas, my true love gave to me:
  Twelve drummers drumming,
  Eleven pipers piping,
  Ten lords a-leaping,
  Nine ladies dancing,
  Eight maids a-milking,
  Seven swans a-swimming,
  Six geese a-laying,
  Five golden rings,
  Four calling birds,
  Three french hens,
  Two turtle doves,
  And a partridge in a pear tree.

```



## Simula

```simula
Begin
  Text Array days(1:12), gifts(1:12);
  Integer day, gift;

  days(1)  :- "first";
  days(2)  :- "second";
  days(3)  :- "third";
  days(4)  :- "fourth";
  days(5)  :- "fifth";
  days(6)  :- "sixth";
  days(7)  :- "seventh";
  days(8)  :- "eighth";
  days(9)  :- "ninth";
  days(10) :- "tenth";
  days(11) :- "eleventh";
  days(12) :- "twelfth";


  gifts(1)  :- "A partridge in a pear tree.";
  gifts(2)  :- "Two turtle doves and";
  gifts(3)  :- "Three French hens,";
  gifts(4)  :- "Four calling birds,";
  gifts(5)  :- "Five gold rings,";
  gifts(6)  :- "Six geese a-laying,";
  gifts(7)  :- "Seven swans a-swimming,";
  gifts(8)  :- "Eight maids a-milking,";
  gifts(9)  :- "Nine ladies dancing,";
  gifts(10) :- "Ten lords a-leaping,";
  gifts(11) :- "Eleven pipers piping,";
  gifts(12) :- "Twelve drummers drumming,";

  For day := 1 Step 1 Until 12 Do Begin
    outtext("On the "); outtext(days(day));
    outtext(" day of Christmas, my true love sent to me:"); outimage;
    For gift := day Step -1 Until 1 Do Begin
      outtext(gifts(gift)); outimage
    End;
    outimage
  End
End

```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## Smalltalk

```smalltalk
Object subclass: TwelveDays [
  Ordinals := #('first'   'second' 'third' 'fourth' 'fifth'    'sixth'
                'seventh' 'eighth' 'ninth' 'tenth'  'eleventh' 'twelfth').

  Gifts := #( 'A partridge in a pear tree.' 'Two turtle doves and'
              'Three French hens,'          'Four calling birds,'
              'Five gold rings,'            'Six geese a-laying,'
              'Seven swans a-swimming,'     'Eight maids a-milking,'
              'Nine ladies dancing,'        'Ten lords a-leaping,'
              'Eleven pipers piping,'       'Twelve drummers drumming,' ).
]

TwelveDays class extend [
  giftsFor: day [
    |newLine ordinal giftList|
    newLine := $<10> asString.
    ordinal := Ordinals at: day.
    giftList := (Gifts first: day) reverse.

    ^'On the ', ordinal, ' day of Christmas, my true love sent to me:',
      newLine, (giftList join: newLine), newLine.
  ]
]

1 to: 12 do: [:i |
  Transcript show: (TwelveDays giftsFor: i); cr.
].
```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```




## Smart BASIC


```smart BASIC
' by rbytes
dim d$(12),x$(15)!s=15
for t=0 to 11!read d$(t)!next t
for t=0 to 14!read x$(t)!next t
for u=0 to 11!s-=1
print x$(0)&d$(u)&x$(1)&chr$(10)&x$(2)
for t=s to 14!print x$(t)!next t
print!next u!data "first","second","third","fourth","fifth","sixth","seventh","eight","ninth","tenth","eleventh","Twelfth","On the "," day of Christmas","My true love gave to me:","Twelve drummers drumming","Eleven pipers piping","Ten lords a-leaping","Nine ladies dancing","Eight maids a-milking","Seven swans a-swimming,","Six geese a-laying","Five golden rings","Four calling birds","Three french hens","Two turtle doves and","A partridge in a pear tree."
```

```txt
On the first day of Christmas
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas
My true love gave to me:
Two turtle doves and
A partridge in a pear tree.

On the third day of Christmas
My true love gave to me:
Three french hens
Two turtle doves and
A partridge in a pear tree.

[ ... ]

On the eleventh day of Christmas
My true love gave to me:
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming,
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree.

On the Twelfth day of Christmas
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming,
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves and
A partridge in a pear tree.
```



## Snobol

```snobol
	DAYS = ARRAY('12')
	DAYS<1> = 'first'
	DAYS<2> = 'second'
	DAYS<3> = 'third'
	DAYS<4> = 'fourth'
	DAYS<5> = 'fifth'
	DAYS<6> = 'sixth'
	DAYS<7> = 'seventh'
	DAYS<8> = 'eighth'
	DAYS<9> = 'ninth'
	DAYS<10> = 'tenth'
	DAYS<11> = 'eleventh'
	DAYS<12> = 'twelfth'

	GIFTS = ARRAY('12')
	GIFTS<1> = 'A partridge in a pear tree.'
	GIFTS<2> = 'Two turtle doves and'
	GIFTS<3> = 'Three French hens,'
	GIFTS<4> = 'Four calling birds,'
	GIFTS<5> = 'Five gold rings,'
	GIFTS<6> = 'Six geese a-laying,'
	GIFTS<7> = 'Seven swans a-swimming,'
	GIFTS<8> = 'Eight maids a-milking,'
	GIFTS<9> = 'Nine ladies dancing,'
	GIFTS<10> = 'Ten lords a-leaping,'
	GIFTS<11> = 'Eleven pipers piping,'
	GIFTS<12> = 'Twelve drummers drumming,'

       DAY = 1
OUTER  LE(DAY,12)            :F(END)
       INTRO = 'On the NTH day of Christmas, my true love sent to me:'
       INTRO 'NTH' = DAYS<DAY>
       OUTPUT = INTRO
       GIFT = DAY
INNER  GE(GIFT,1)            :F(NEXT)
       OUTPUT = GIFTS<GIFT>
       GIFT = GIFT - 1       :(INNER)
NEXT   OUTPUT = ''
       DAY = DAY + 1         :(OUTER)
END
```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## SQL

Demonstration of Oracle 12c "with" clause enhancement.


```SQL

with
function nl ( s in varchar2 )
return varchar2
is
begin
        return chr(10) || s;
end nl;
function v ( d number, x number, g in varchar2 )
return varchar2
is
begin
        return
        case when d >= x then nl (g) end;
end v;
select 'On the '
        || to_char(to_date(level,'j'),'jspth' )
        || ' day of Christmas,'
        || nl( 'my true love sent to me:')
        || v ( level, 12, 'Twelve drummers drumming,' )
        || v ( level, 11, 'Eleven pipers piping,' )
        || v ( level, 10, 'Ten lords a-leaping,' )
        || v ( level, 9, 'Nine ladies dancing,' )
        || v ( level, 8, 'Eight maids a-milking,' )
        || v ( level, 7, 'Seven swans a-swimming,' )
        || v ( level, 6, 'Six geese a-laying,' )
        || v ( level, 5, 'Five golden rings!' )
        || v ( level, 4, 'Four calling birds,' )
        || v ( level, 3, 'Three French hens,' )
        || v ( level, 2, 'Two turtle doves,' )
        || v ( level, 1, case level when 1 then 'A' else 'And a' end || ' partridge in a pear tree.' )
        || nl(null)
        "The Twelve Days of Christmas"
from dual
connect by level <= 12
/

```

output:

```txt

The Twelve Days of Christmas
--------------------------------------------------------------------------------
On the first day of Christmas,
my true love sent to me:
A partridge in a pear tree.

...

On the twelfth day of Christmas,
my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings!
Four calling birds,
Three French hens,
Two turtle doves,
And a partridge in a pear tree.

```



## Swift

```swift
let gifts = [ "partridge in a pear tree", "Two turtle doves",
              "Three French hens",        "Four calling birds",
              "Five gold rings",          "Six geese a-laying",
              "Seven swans a-swimming",   "Eight maids a-milking",
              "Nine ladies dancing",      "Ten lords a-leaping",
              "Eleven pipers piping",     "Twelve drummers drumming" ]

let nth = [ "first",   "second", "third", "fourth", "fifth",    "sixth",
            "seventh", "eighth", "ninth", "tenth",  "eleventh", "twelfth" ]

func giftsForDay(day: Int) -> String {
  var result = "On the \(nth[day-1]) day of Christmas, my true love sent to me:\n"
  if day > 1 {
    for again in 1...day-1 {
      let n = day - again
      result += gifts[n]
      if n > 1 { result += "," }
      result += "\n"
    }
    result += "And a "
  } else {
    result += "A "
  }
  return result + gifts[0] + ".\n";
}

for day in 1...12 {
  print(giftsForDay(day))
}
```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## Tailspin


```tailspin

def ordinal: ['first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh', 'eighth', 'ninth', 'tenth', 'eleventh', 'twelfth'];
def gift: [
  'a partridge in a pear tree',
  'two turtle-doves',
  'three French hens',
  'four calling birds',
  'five golden rings',
  'six geese a-laying',
  'seven swans a-swimming',
  'eight maids a-milking',
  'nine ladies dancing',
  'ten lords a-leaping',
  'eleven pipers piping',
  'twelve drummers drumming'
];
templates punctuation
  <1> '.' !
  <2> ' and' !
  <5> ';' !
  <> ',' !
end punctuation

1..12 -> (
  'On the $ordinal($); day of Christmas,
my true love gave to me:
' !
  $..1:-1 -> '$gift($);$->punctuation;
' !
'
' !
) -> !OUT::write

```

```txt

On the first day of Christmas,
my true love gave to me:
a partridge in a pear tree.

On the second day of Christmas,
my true love gave to me:
two turtle-doves and
a partridge in a pear tree.

...

On the twelfth day of Christmas,
my true love gave to me:
twelve drummers drumming,
eleven pipers piping,
ten lords a-leaping,
nine ladies dancing,
eight maids a-milking,
seven swans a-swimming,
six geese a-laying,
five golden rings;
four calling birds,
three French hens,
two turtle-doves and
a partridge in a pear tree.



```



## Tcl

```tcl
set days {
    first second third fourth fifth sixth
    seventh eighth ninth tenth eleventh twelfth
}
set gifts [lreverse {
    "A partridge in a pear tree."
    "Two turtle doves, and"
    "Three french hens,"
    "Four calling birds,"
    "Five golden rings,"
    "Six geese a-laying,"
    "Seven swans a-swimming,"
    "Eight maids a-milking,"
    "Nine ladies dancing,"
    "Ten lords a-leaping,"
    "Eleven pipers piping,"
    "Twelve drummers drumming,"
}]

set n -1;puts [join [lmap day $days {
    format "On the $day day of Christmas,\nMy true love gave to me:\n%s" \
	    [join [lrange $gifts end-[incr n] end] \n]
}] \n\n]
```

<pre style="height:55ex;overflow:scroll">
On the first day of Christmas,
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas,
My true love gave to me:
Two turtle doves, and
A partridge in a pear tree.

On the third day of Christmas,
My true love gave to me:
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the fourth day of Christmas,
My true love gave to me:
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the fifth day of Christmas,
My true love gave to me:
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the sixth day of Christmas,
My true love gave to me:
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the seventh day of Christmas,
My true love gave to me:
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the eighth day of Christmas,
My true love gave to me:
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the ninth day of Christmas,
My true love gave to me:
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the tenth day of Christmas,
My true love gave to me:
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the eleventh day of Christmas,
My true love gave to me:
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

On the twelfth day of Christmas,
My true love gave to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five golden rings,
Four calling birds,
Three french hens,
Two turtle doves, and
A partridge in a pear tree.

```



## UNIX Shell

```bash
#!/usr/bin/env bash
ordinals=(first   second third fourth fifth    sixth
          seventh eighth ninth tenth  eleventh twelfth)

gifts=( "A partridge in a pear tree." "Two turtle doves and"
        "Three French hens,"          "Four calling birds,"
        "Five gold rings,"            "Six geese a-laying,"
        "Seven swans a-swimming,"     "Eight maids a-milking,"
        "Nine ladies dancing,"        "Ten lords a-leaping,"
        "Eleven pipers piping,"       "Twelve drummers drumming," )

echo_gifts() {
  local i day=$1
  echo "On the ${ordinals[day]} day of Christmas, my true love sent to me:"
  for (( i=day; i >=0; --i )); do
    echo "${gifts[i]}"
  done
  echo
}

for (( day=0; day < 12; ++day )); do
  echo_gifts $day
done
```


The above will also work in zsh if the index range is changed from 0..11 to 1..12.

(requires the '''seq''' command)


```sh
#!/bin/sh
ordinal() {
  n=$1
  set first   second third fourth fifth    sixth \
      seventh eighth ninth tenth  eleventh twelfth
  shift $n
  echo $1
}

gift() {
  n=$1
  set "A partridge in a pear tree." "Two turtle doves and"      \
      "Three French hens,"          "Four calling birds,"       \
      "Five gold rings,"            "Six geese a-laying,"       \
      "Seven swans a-swimming,"     "Eight maids a-milking,"    \
      "Nine ladies dancing,"        "Ten lords a-leaping,"      \
      "Eleven pipers piping,"       "Twelve drummers drumming,"
  shift $n
  echo "$1"
}

echo_gifts() {
  day=$1
  echo "On the `ordinal $day` day of Christmas, my true love sent to me:"
  for i in `seq $day 0`; do
    gift $i
  done
  echo
}

for day in `seq 0 11`; do
  echo_gifts $day
done
```


```txt
On the first day of Christmas, my true love sent to me:
A partridge in a pear tree.

On the second day of Christmas, my true love sent to me:
Two turtle doves and
A partridge in a pear tree.

[...]

On the twelfth day of Christmas, my true love sent to me:
Twelve drummers drumming,
Eleven pipers piping,
Ten lords a-leaping,
Nine ladies dancing,
Eight maids a-milking,
Seven swans a-swimming,
Six geese a-laying,
Five gold rings,
Four calling birds,
Three French hens,
Two turtle doves and
A partridge in a pear tree.
```



## VBA


```vb
Sub Main()
Dim i As Integer, c As Integer, j As Integer, strReturn() As String
Dim s, n
   s = Split(SING_, "$")
   n = Split(NUMBERS_, " ")
   ReDim strReturn(UBound(s))
   For i = LBound(s) To UBound(s)
      strReturn(i) = Replace(BASE_, "(X)", n(i))
      For j = c To 0 Step -1
         strReturn(i) = strReturn(i) & s(j) & vbCrLf
      Next
      c = c + 1
   Next i
   strReturn(UBound(strReturn)) = Replace(strReturn(UBound(strReturn)), "and" & vbCrLf & "A", vbCrLf & "And a")
   Debug.Print Join(strReturn, vbCrLf)
End Sub
```

```txt
On the first day of Christmas,
My true love gave to me:
A partridge in a pear tree.

On the second day of Christmas,
My true love gave to me:
Two turtle doves and
A partridge in a pear tree.

On the third day of Christmas,
My true love gave to me:
Three french hens
Two turtle doves and
A partridge in a pear tree.
[...]
On the twelfth day of Christmas,
My true love gave to me:
Twelve drummers drumming
Eleven pipers piping
Ten lords a-leaping
Nine ladies dancing
Eight maids a-milking
Seven swans a-swimming
Six geese a-laying
Five golden rings
Four calling birds
Three french hens
Two turtle doves
And a partridge in a pear tree.
```



## VBScript


```vb
days = Array("first","second","third","fourth","fifth","sixth",_
		"seventh","eight","ninth","tenth","eleventh","twelfth")

gifts = Array("A partridge in a pear tree","Two turtle doves","Three french hens",_
	"Four calling birds","Five golden rings","Six geese a-laying","Seven swans a-swimming",_
	"Eight maids a-milking","Nine ladies dancing","Ten lords a-leaping","Eleven pipers piping",_
	"Twelve drummers drumming")

For i = 0 To 11
	WScript.StdOut.Write "On the " & days(i) & " day of Christmas"
	WScript.StdOut.WriteLine
	WScript.StdOut.Write "My true love sent to me:"
	WScript.StdOut.WriteLine
	If i = 0 Then
		WScript.StdOut.Write gifts(i)
	Else
		For j = i To 0 Step - 1
			If j = 0 Then
				WScript.StdOut.Write "and " & gifts(0)
			Else
				WScript.StdOut.Write gifts(j)
				WScript.StdOut.WriteLine
			End If
		Next
	End If
		WScript.StdOut.WriteBlankLines(2)
Next
```



## Visual Basic .NET

'''Compiler:''' Roslyn Visual Basic (language version >= 14, e.g. with Visual Studio 2015)


```vbnet
Module Program
    Sub Main()
        Dim days = New String(11) {"first", "second", "third", "fourth", "fifth", "sixth", "seventh", "eighth", "ninth", "tenth", "eleventh", "twelfth"}
        Dim gifts = New String(11) {
            "A partridge in a pear tree",
            "Two turtle doves",
            "Three french hens",
            "Four calling birds",
            "Five golden rings",
            "Six geese a-laying",
            "Seven swans a-swimming",
            "Eight maids a-milking",
            "Nine ladies dancing",
            "Ten lords a-leaping",
            "Eleven pipers piping",
            "Twelve drummers drumming"
        }

        For i = 0 To 11
            Console.WriteLine($"On the {days(i)} day of Christmas, my true love gave to me")

            For j = i To 0 Step -1
                Console.WriteLine(gifts(j))
            Next

            Console.WriteLine()

            If i = 0 Then gifts(0) = "And a partridge in a pear tree"
        Next
    End Sub
End Module
```



## zkl

```zkl
gifts:=
#<<<
"A beer, in a tree.; Two turtlenecks; Three french toast;
Four pounds of backbacon; Five golden touques; Six packs of two-four;
Seven packs of smokes; Eight comic books; Nine back up singers;
Ten feet of snow; Eleven hosers hosing; Twelve dozen donuts"
#<<<
.split(";").apply("strip");

days:=("first second third fourth fifth sixth seventh eighth ninth tenth "
      "eleventh twelfth").split();

foreach n,day in (days.enumerate()){ n+=1;
   g:=gifts[0,n].reverse();
   println("On the %s day of Christmas\nMy true love gave to me:\n".fmt(day),
         g[0,-1].concat("\n"), (n>1) and " and\n" or "", g[-1], "\n");
}
```


<pre style="height:25ex;overflow:scroll">
On the first day of Christmas
My true love gave to me:
A beer, in a tree.

On the second day of Christmas
My true love gave to me:
Two turtlenecks and
A beer, in a tree.

On the third day of Christmas
My true love gave to me:
Three french toast
Two turtlenecks and
A beer, in a tree.

On the fourth day of Christmas
My true love gave to me:
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the fifth day of Christmas
My true love gave to me:
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the sixth day of Christmas
My true love gave to me:
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the seventh day of Christmas
My true love gave to me:
Seven packs of smokes
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the eighth day of Christmas
My true love gave to me:
Eight comic books
Seven packs of smokes
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the ninth day of Christmas
My true love gave to me:
Nine back up singers
Eight comic books
Seven packs of smokes
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the tenth day of Christmas
My true love gave to me:
Ten feet of snow
Nine back up singers
Eight comic books
Seven packs of smokes
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the eleventh day of Christmas
My true love gave to me:
Eleven hosers hosing
Ten feet of snow
Nine back up singers
Eight comic books
Seven packs of smokes
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.

On the twelfth day of Christmas
My true love gave to me:
Twelve dozen donuts
Eleven hosers hosing
Ten feet of snow
Nine back up singers
Eight comic books
Seven packs of smokes
Six packs of two-four
Five golden touques
Four pounds of backbacon
Three french toast
Two turtlenecks and
A beer, in a tree.


```

