+++
title = "Show Ascii table"
description = ""
date = 2019-10-21T05:06:43Z
aliases = []
[extra]
id = 21946
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Show   [http://kepkezelo.com/images/anuscoqy8ejyr5bnhvnu.jpg ASCII character set]   from values   '''32'''   to   '''127'''   (decimal)   in a table format.





## Ada


```ada
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Ascii_Table is
   N : Integer;
begin
   for R in 0 .. 15 loop
      for C in 0 .. 5 loop
         N := 32 + 16 * C + R;
         Put (N, 3);
         Put (" : ");
         case N is
            when 32 =>
               Put ("Spc  ");
            when 127 =>
               Put ("Del  ");
            when others =>
               Put (Character'Val (N) & "    ");
         end case;
      end loop;
      New_Line;
   end loop;
end Ascii_Table;
```

{{out}}

```txt
 32 : Spc   48 : 0     64 : @     80 : P     96 : `    112 : p
 33 : !     49 : 1     65 : A     81 : Q     97 : a    113 : q
 34 : "     50 : 2     66 : B     82 : R     98 : b    114 : r
 35 : #     51 : 3     67 : C     83 : S     99 : c    115 : s
 36 : $     52 : 4     68 : D     84 : T    100 : d    116 : t
 37 : %     53 : 5     69 : E     85 : U    101 : e    117 : u
 38 : &     54 : 6     70 : F     86 : V    102 : f    118 : v
 39 : '     55 : 7     71 : G     87 : W    103 : g    119 : w
 40 : (     56 : 8     72 : H     88 : X    104 : h    120 : x
 41 : )     57 : 9     73 : I     89 : Y    105 : i    121 : y
 42 : *     58 : :     74 : J     90 : Z    106 : j    122 : z
 43 : +     59 : ;     75 : K     91 : [    107 : k    123 : {
 44 : ,     60 : <     76 : L     92 : \    108 : l    124 : |
 45 : -     61 : =     77 : M     93 : ]    109 : m    125 : }
 46 : .     62 : >     78 : N     94 : ^    110 : n    126 : ~
 47 : /     63 : ?     79 : O     95 : _    111 : o    127 : Del
```



## ALGOL 68


```algol68
BEGIN
    # generate an ascii table for characters 32 - 127 #
    INT    char count := 1;
    FOR c FROM 32 TO 127 DO
        print( ( whole( c, -4 )
               , ": "
               , IF   c =  32 THEN "SPC"
                 ELIF c = 127 THEN "DEL"
                 ELSE " " + REPR c + " "
                 FI
               )
             );
        IF char count = 0 THEN print( ( newline ) ) FI;
        ( char count PLUSAB 1 ) MODAB 6
    OD
END
```

{{out}}

```txt
  32: SPC  33:  !   34:  "   35:  #   36:  $   37:  %
  38:  &   39:  '   40:  (   41:  )   42:  *   43:  +
  44:  ,   45:  -   46:  .   47:  /   48:  0   49:  1
  50:  2   51:  3   52:  4   53:  5   54:  6   55:  7
  56:  8   57:  9   58:  :   59:  ;   60:  <   61:  =
  62:  >   63:  ?   64:  @   65:  A   66:  B   67:  C
  68:  D   69:  E   70:  F   71:  G   72:  H   73:  I
  74:  J   75:  K   76:  L   77:  M   78:  N   79:  O
  80:  P   81:  Q   82:  R   83:  S   84:  T   85:  U
  86:  V   87:  W   88:  X   89:  Y   90:  Z   91:  [
  92:  \   93:  ]   94:  ^   95:  _   96:  `   97:  a
  98:  b   99:  c  100:  d  101:  e  102:  f  103:  g
 104:  h  105:  i  106:  j  107:  k  108:  l  109:  m
 110:  n  111:  o  112:  p  113:  q  114:  r  115:  s
 116:  t  117:  u  118:  v  119:  w  120:  x  121:  y
 122:  z  123:  {  124:  |  125:  }  126:  ~  127: DEL
```



## ALGOL W


```algolw
begin
    % generate an ascii table for chars 32 - 127            %
    integer cPos;
    cPos := 0;
    for i := 32 until 127 do begin
        if cPos = 0 then write();
        cPos := ( cPos + 1 ) rem 6;
        writeon( i_w := 3, s_w := 0, i, ": " );
        if      i =  32 then writeon( "Spc ")
        else if i = 127 then writeon( "Del " )
        else                 writeon( code( i ), "   " )
    end for_i
end.
```

{{out}}

```txt
 32: Spc  33: !    34: "    35: #    36: $    37: %
 38: &    39: '    40: (    41: )    42: *    43: +
 44: ,    45: -    46: .    47: /    48: 0    49: 1
 50: 2    51: 3    52: 4    53: 5    54: 6    55: 7
 56: 8    57: 9    58: :    59: ;    60: <    61: =
 62: >    63: ?    64: @    65: A    66: B    67: C
 68: D    69: E    70: F    71: G    72: H    73: I
 74: J    75: K    76: L    77: M    78: N    79: O
 80: P    81: Q    82: R    83: S    84: T    85: U
 86: V    87: W    88: X    89: Y    90: Z    91: [
 92: \    93: ]    94: ^    95: _    96: `    97: a
 98: b    99: c   100: d   101: e   102: f   103: g
104: h   105: i   106: j   107: k   108: l   109: m
110: n   111: o   112: p   113: q   114: r   115: s
116: t   117: u   118: v   119: w   120: x   121: y
122: z   123: {   124: |   125: }   126: ~   127: Del
```



## AppleScript


```applescript
-- asciiTable :: () -> String
on asciiTable()
    script row
        on |λ|(ln)
            script cell
                on |λ|(x)
                    justifyLeft(12, space, x)
                end |λ|
            end script

            concat(map(cell, ln))
        end |λ|
    end script

    unlines(map(row, ¬
        transpose(chunksOf(16, map(my asciiEntry, ¬
            enumFromTo(32, 127))))))
end asciiTable

on run

    asciiTable()

end run

------------------------------------------------------------------

-- asciiEntry :: Int -> String
on asciiEntry(n)
    set k to asciiName(n)
    if "" ≠ k then
        justifyRight(4, space, n as string) & " : " & k
    else
        k
    end if
end asciiEntry

-- asciiName :: Int -> String
on asciiName(n)
    if 32 > n or 127 < n then
        ""
    else if 32 = n then
        "Spc"
    else if 127 = n then
        "Del"
    else
        chr(n)
    end if
end asciiName


-- GENERIC FUNCTIONS ---------------------------------------------------------

-- chr :: Int -> Char
on chr(n)
    character id n
end chr

-- chunksOf :: Int -> [a] -> [[a]]
on chunksOf(k, xs)
    script
        on go(ys)
            set ab to splitAt(k, ys)
            set a to |1| of ab
            if isNull(a) then
                {}
            else
                {a} & go(|2| of ab)
            end if
        end go
    end script
    result's go(xs)
end chunksOf

-- comparing :: (a -> b) -> (a -> a -> Ordering)
on comparing(f)
    script
        on |λ|(a, b)
            tell mReturn(f)
                set fa to |λ|(a)
                set fb to |λ|(b)
                if fa < fb then
                    -1
                else if fa > fb then
                    1
                else
                    0
                end if
            end tell
        end |λ|
    end script
end comparing

-- concat :: [[a]] -> [a]
-- concat :: [String] -> String
on concat(xs)
    set lng to length of xs
    if 0 < lng and string is class of (item 1 of xs) then
        set acc to ""
    else
        set acc to {}
    end if
    repeat with i from 1 to lng
        set acc to acc & item i of xs
    end repeat
    acc
end concat

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lng to length of xs
    if 0 < lng and class of xs is string then
        set acc to ""
    else
        set acc to {}
    end if
    tell mReturn(f)
        repeat with i from 1 to lng
            set acc to acc & |λ|(item i of xs, i, xs)
        end repeat
    end tell
    return acc
end concatMap

-- enumFromTo :: Int -> Int -> [Int]
on enumFromTo(m, n)
    if m ≤ n then
        set lst to {}
        repeat with i from m to n
            set end of lst to i
        end repeat
        return lst
    else
        return {}
    end if
end enumFromTo

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

-- isNull :: [a] -> Bool
-- isNull :: String -> Bool
on isNull(xs)
    if class of xs is string then
        "" = xs
    else
        {} = xs
    end if
end isNull

-- justifyLeft :: Int -> Char -> String -> String
on justifyLeft(n, cFiller, strText)
    if n > length of strText then
        text 1 thru n of (strText & replicate(n, cFiller))
    else
        strText
    end if
end justifyLeft

-- justifyRight :: Int -> Char -> String -> String
on justifyRight(n, cFiller, strText)
    if n > length of strText then
        text -n thru -1 of ((replicate(n, cFiller) as text) & strText)
    else
        strText
    end if
end justifyRight

-- length :: [a] -> Int
on |length|(xs)
    length of xs
end |length|

-- max :: Ord a => a -> a -> a
on max(x, y)
    if x > y then
        x
    else
        y
    end if
end max

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
-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    if class of f is script then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

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

-- splitAt :: Int -> [a] -> ([a],[a])
on splitAt(n, xs)
    if n > 0 and n < length of xs then
        if class of xs is text then
            Tuple(items 1 thru n of xs as text, items (n + 1) thru -1 of xs as text)
        else
            Tuple(items 1 thru n of xs, items (n + 1) thru -1 of xs)
        end if
    else
        if n < 1 then
            Tuple({}, xs)
        else
            Tuple(xs, {})
        end if
    end if
end splitAt

-- If some of the rows are shorter than the following rows,
-- their elements are skipped:
-- transpose({{10,11},{20},{},{30,31,32}}) -> {{10, 20, 30}, {11, 31}, {32}}
-- transpose :: [[a]] -> [[a]]
on transpose(xxs)
    set intMax to |length|(maximumBy(comparing(my |length|), xxs))
    set gaps to replicate(intMax, {})
    script padded
        on |λ|(xs)
            set lng to |length|(xs)
            if lng < intMax then
                xs & items (lng + 1) thru -1 of gaps
            else
                xs
            end if
        end |λ|
    end script
    set rows to map(padded, xxs)

    script cols
        on |λ|(_, iCol)
            script cell
                on |λ|(row)
                    item iCol of row
                end |λ|
            end script
            concatMap(cell, rows)
        end |λ|
    end script
    map(cols, item 1 of rows)
end transpose

-- Tuple (,) :: a -> b -> (a, b)
on Tuple(a, b)
    {type:"Tuple", |1|:a, |2|:b, length:2}
end Tuple

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines
```

{{Out}}

```txt
  32 : Spc    48 : 0      64 : @      80 : P      96 : `     112 : p
  33 : !      49 : 1      65 : A      81 : Q      97 : a     113 : q
  34 : "      50 : 2      66 : B      82 : R      98 : b     114 : r
  35 : #      51 : 3      67 : C      83 : S      99 : c     115 : s
  36 : $      52 : 4      68 : D      84 : T     100 : d     116 : t
  37 : %      53 : 5      69 : E      85 : U     101 : e     117 : u
  38 : &      54 : 6      70 : F      86 : V     102 : f     118 : v
  39 : '      55 : 7      71 : G      87 : W     103 : g     119 : w
  40 : (      56 : 8      72 : H      88 : X     104 : h     120 : x
  41 : )      57 : 9      73 : I      89 : Y     105 : i     121 : y
  42 : *      58 : :      74 : J      90 : Z     106 : j     122 : z
  43 : +      59 : ;      75 : K      91 : [     107 : k     123 : {
  44 : ,      60 : <      76 : L      92 : \     108 : l     124 : |
  45 : -      61 : =      77 : M      93 : ]     109 : m     125 : }
  46 : .      62 : >      78 : N      94 : ^     110 : n     126 : ~
  47 : /      63 : ?      79 : O      95 : _     111 : o     127 : Del
```



## AWK

{{works with|GAWK}}
{{works with|MAWK}}

```AWK
# syntax: GAWK -f SHOW_ASCII_TABLE.AWK
# syntax: MAWK -f SHOW_ASCII_TABLE.AWK
BEGIN {
    n00_15 = "NUL,SOH,STX,ETX,EOT,ENQ,ACK,BEL,BS,HT,LF,VT,FF,CR,SO,SI"
    n16_31 = "DLE,DC1,DC2,DC3,DC4,NAK,SYN,ETB,CAN,EM,SUB,ESC,FS,GS,RS,US"
    split(n00_15 "," n16_31,arr,",")
    for (i=0; i<16; i++) {
      for (j=0+i; j<128; j+=16) {
        if (j <= 31) {
          x = arr[j+1]
          continue # don't show values 0 - 31
        }
        else if (j == 32) { x = "SP" }
        else if (j == 127) { x = "DEL" }
        else { x = sprintf("%c",j) }
        printf("%3d: %-5s",j,x)
      }
      printf("\n")
    }
    exit(0)
}
```

{{out}}

```txt
 32: SP    48: 0     64: @     80: P     96: `    112: p
 33: !     49: 1     65: A     81: Q     97: a    113: q
 34: "     50: 2     66: B     82: R     98: b    114: r
 35: #     51: 3     67: C     83: S     99: c    115: s
 36: $     52: 4     68: D     84: T    100: d    116: t
 37: %     53: 5     69: E     85: U    101: e    117: u
 38: &     54: 6     70: F     86: V    102: f    118: v
 39: '     55: 7     71: G     87: W    103: g    119: w
 40: (     56: 8     72: H     88: X    104: h    120: x
 41: )     57: 9     73: I     89: Y    105: i    121: y
 42: *     58: :     74: J     90: Z    106: j    122: z
 43: +     59: ;     75: K     91: [    107: k    123: {
 44: ,     60: <     76: L     92: \    108: l    124: |
 45: -     61: =     77: M     93: ]    109: m    125: }
 46: .     62: >     78: N     94: ^    110: n    126: ~
 47: /     63: ?     79: O     95: _    111: o    127: DEL
```



## C

{{trans|Go}}

```c
#include <stdio.h>

int main() {
    int i, j;
    char k[4];
    for (i = 0; i < 16; ++i) {
        for (j = 32 + i; j < 128; j += 16) {
            switch (j) {
                default:  sprintf(k, "%c", j); break;
                case 32:  sprintf(k, "Spc"); break;
                case 127: sprintf(k, "Del"); break;
            }
            printf("%3d : %-3s   ", j, k);
        }
        printf("\n");
    }
    return 0;
}
```

{{out}}

```txt
 32 : Spc    48 : 0      64 : @      80 : P      96 : `     112 : p
 33 : !      49 : 1      65 : A      81 : Q      97 : a     113 : q
 34 : "      50 : 2      66 : B      82 : R      98 : b     114 : r
 35 : #      51 : 3      67 : C      83 : S      99 : c     115 : s
 36 : $      52 : 4      68 : D      84 : T     100 : d     116 : t
 37 : %      53 : 5      69 : E      85 : U     101 : e     117 : u
 38 : &      54 : 6      70 : F      86 : V     102 : f     118 : v
 39 : '      55 : 7      71 : G      87 : W     103 : g     119 : w
 40 : (      56 : 8      72 : H      88 : X     104 : h     120 : x
 41 : )      57 : 9      73 : I      89 : Y     105 : i     121 : y
 42 : *      58 : :      74 : J      90 : Z     106 : j     122 : z
 43 : +      59 : ;      75 : K      91 : [     107 : k     123 : {
 44 : ,      60 : <      76 : L      92 : \     108 : l     124 : |
 45 : -      61 : =      77 : M      93 : ]     109 : m     125 : }
 46 : .      62 : >      78 : N      94 : ^     110 : n     126 : ~
 47 : /      63 : ?      79 : O      95 : _     111 : o     127 : Del
```



## C#


```c#
using static System.Console;
using static System.Linq.Enumerable;

public class Program
{
    static void Main()
    {
        for (int start = 32; start + 16 * 5 < 128; start++) {
            WriteLine(string.Concat(Range(0, 6).Select(i => $"{start+16*i, 3} : {Text(start+16*i), -6}")));
        }

        string Text(int index) => index == 32 ? "Sp" : index == 127 ? "Del" : (char)index + "";
    }
}
```

{{out}}

```txt
 32 : Sp     48 : 0      64 : @      80 : P      96 : `     112 : p
 33 : !      49 : 1      65 : A      81 : Q      97 : a     113 : q
 34 : "      50 : 2      66 : B      82 : R      98 : b     114 : r
 35 : #      51 : 3      67 : C      83 : S      99 : c     115 : s
 36 : $      52 : 4      68 : D      84 : T     100 : d     116 : t
 37 : %      53 : 5      69 : E      85 : U     101 : e     117 : u
 38 : &      54 : 6      70 : F      86 : V     102 : f     118 : v
 39 : '      55 : 7      71 : G      87 : W     103 : g     119 : w
 40 : (      56 : 8      72 : H      88 : X     104 : h     120 : x
 41 : )      57 : 9      73 : I      89 : Y     105 : i     121 : y
 42 : *      58 : :      74 : J      90 : Z     106 : j     122 : z
 43 : +      59 : ;      75 : K      91 : [     107 : k     123 : {
 44 : ,      60 : <      76 : L      92 : \     108 : l     124 : |
 45 : -      61 : =      77 : M      93 : ]     109 : m     125 : }
 46 : .      62 : >      78 : N      94 : ^     110 : n     126 : ~
 47 : /      63 : ?      79 : O      95 : _     111 : o     127 : Del
```


=={{header|Caché ObjectScript}}==
<lang Caché ObjectScript>SHOWASCII
  ; this is 96 characters, so do 6 columns of 16
  for i = 32:1:127 {
    ; get remainder when div by 6, sort columns by remainder 2 3 4 5 0 1
    set rem = i # 6
    if rem = 2 {
      write !
    }

    ; spacing (tabs)
    set x = $case(rem,2:0,3:8,4:16,5:24,0:32,:40)

    ; char to write
    set wrtchr = $case(i,32:"Spc",127:"Del",:$char(i))
    write ?x,$justify(i,3)_": "_wrtchr
  }

  quit
```


{{out}}

```txt
SAMPLES>do ^SHOWASCII

 32: Spc 33: !   34: "   35: #   36: $   37: %
 38: &   39: '   40: (   41: )   42: *   43: +
 44: ,   45: -   46: .   47: /   48: 0   49: 1
 50: 2   51: 3   52: 4   53: 5   54: 6   55: 7
 56: 8   57: 9   58: :   59: ;   60: <   61: =
 62: >   63: ?   64: @   65: A   66: B   67: C
 68: D   69: E   70: F   71: G   72: H   73: I
 74: J   75: K   76: L   77: M   78: N   79: O
 80: P   81: Q   82: R   83: S   84: T   85: U
 86: V   87: W   88: X   89: Y   90: Z   91: [
 92: \   93: ]   94: ^   95: _   96: `   97: a
 98: b   99: c  100: d  101: e  102: f  103: g
104: h  105: i  106: j  107: k  108: l  109: m
110: n  111: o  112: p  113: q  114: r  115: s
116: t  117: u  118: v  119: w  120: x  121: y
122: z  123: {  124: |  125: }  126: ~  127: Del
```



## Common Lisp


```lisp
(setq startVal 32)
(setq endVal 127)
(setq cols 6)

(defun print-val (val) "Prints the value for that ascii number"
	(cond
		((= val 32) (format t " 32: SPC "))
		((= val 127) (format t "127: DEL~%"))
		((and (zerop (mod (- val startVal) cols)) (< val 100)) (format t "~% ~d: ~a   " val (int-char val)))
		((and (zerop (mod (- val startVal) cols)) (>= val 100)) (format t "~%~d: ~a   " val (int-char val)))
		((< val 100) (format t " ~d:  ~a   " val (int-char val)))
		((>= val 100) (format t "~d:  ~a   " val (int-char val)))
		(t nil)))

        (defun get-range (lower upper) "Returns a list of range lower to upper"
	(if (> lower upper) '() (cons lower (get-range (+ 1 lower) upper))))

(mapcar #'print-val (get-range startVal endVal))
```

{{out}}

```txt
 32: SPC  33:  !    34:  "    35:  #    36:  $    37:  %
 38: &    39:  '    40:  (    41:  )    42:  *    43:  +
 44: ,    45:  -    46:  .    47:  /    48:  0    49:  1
 50: 2    51:  3    52:  4    53:  5    54:  6    55:  7
 56: 8    57:  9    58:  :    59:  ;    60:  <    61:  =
 62: >    63:  ?    64:  @    65:  A    66:  B    67:  C
 68: D    69:  E    70:  F    71:  G    72:  H    73:  I
 74: J    75:  K    76:  L    77:  M    78:  N    79:  O
 80: P    81:  Q    82:  R    83:  S    84:  T    85:  U
 86: V    87:  W    88:  X    89:  Y    90:  Z    91:  [
 92: \    93:  ]    94:  ^    95:  _    96:  `    97:  a
 98: b    99:  c   100:  d   101:  e   102:  f   103:  g
104: h   105:  i   106:  j   107:  k   108:  l   109:  m
110: n   111:  o   112:  p   113:  q   114:  r   115:  s
116: t   117:  u   118:  v   119:  w   120:  x   121:  y
122: z   123:  {   124:  |   125:  }   126:  ~   127: DEL
```


## Factor


### Idiomatic version

{{Works with|Factor|0.98}}

```factor
USING: combinators formatting io kernel math math.ranges
pair-rocket sequences ;
IN: rosetta-code.ascii-table

: row-values ( n -- seq ) [ 32 + ] [ 112 + ] bi 16 <range> ;

: ascii>output ( n -- str )
    { 32 => [ "Spc" ] 127 => [ "Del" ] [ "" 1sequence ] } case ;

: print-row ( n -- )
    row-values [ dup ascii>output "%3d : %-3s   " printf ] each nl ;

: print-ascii-table ( -- ) 16 <iota> [ print-row ] each ;

MAIN: print-ascii-table
```


### Go translation

{{Trans|Go}}
{{Works with|Factor|0.98}}

```factor
USING: combinators formatting io kernel math math.ranges
pair-rocket sequences ;
IN: rosetta-code.ascii-table

: main ( -- )
    16 <iota> [
        32 + 127 16 <range> [
            dup {
                32  => [ "Spc" ]
                127 => [ "Del" ]
                [ "" 1sequence ]
            } case
            "%3d : %-3s   " printf
        ] each
        nl
    ] each
;

MAIN: main
```

{{out}}

```txt
 32 : Spc    48 : 0      64 : @      80 : P      96 : `     112 : p
 33 : !      49 : 1      65 : A      81 : Q      97 : a     113 : q
 34 : "      50 : 2      66 : B      82 : R      98 : b     114 : r
 35 : #      51 : 3      67 : C      83 : S      99 : c     115 : s
 36 : $      52 : 4      68 : D      84 : T     100 : d     116 : t
 37 : %      53 : 5      69 : E      85 : U     101 : e     117 : u
 38 : &      54 : 6      70 : F      86 : V     102 : f     118 : v
 39 : '      55 : 7      71 : G      87 : W     103 : g     119 : w
 40 : (      56 : 8      72 : H      88 : X     104 : h     120 : x
 41 : )      57 : 9      73 : I      89 : Y     105 : i     121 : y
 42 : *      58 : :      74 : J      90 : Z     106 : j     122 : z
 43 : +      59 : ;      75 : K      91 : [     107 : k     123 : {
 44 : ,      60 : <      76 : L      92 : \     108 : l     124 : |
 45 : -      61 : =      77 : M      93 : ]     109 : m     125 : }
 46 : .      62 : >      78 : N      94 : ^     110 : n     126 : ~
 47 : /      63 : ?      79 : O      95 : _     111 : o     127 : Del
```



## Forth

Idiomatic Forth version is factored differently than conventional languages, allowing each factor to be tested independently at the console, bottom up.

```forth
DECIMAL
: ###: ( c -- ) 3 .R ." : " ;

: .CHAR  ( c -- )
        DUP
        CASE
         BL OF  ###: ." spc"  ENDOF
        127 OF  ###: ." del"  ENDOF
            DUP ###: EMIT  2 SPACES
        ENDCASE
        3 SPACES ;

: .ROW ( n2 n1 -- )
       CR DO   I .CHAR   16 +LOOP ;

: ASCII.TABLE ( -- )
        16 0 DO   113 I +   32 I +  .ROW     LOOP ;
```


Test Output at the console

```forth
ASCII.TABLE
 32: spc    48: 0      64: @      80: P      96: `     112: p
 33: !      49: 1      65: A      81: Q      97: a     113: q
 34: "      50: 2      66: B      82: R      98: b     114: r
 35: #      51: 3      67: C      83: S      99: c     115: s
 36: $      52: 4      68: D      84: T     100: d     116: t
 37: %      53: 5      69: E      85: U     101: e     117: u
 38: &      54: 6      70: F      86: V     102: f     118: v
 39: '      55: 7      71: G      87: W     103: g     119: w
 40: (      56: 8      72: H      88: X     104: h     120: x
 41: )      57: 9      73: I      89: Y     105: i     121: y
 42: *      58: :      74: J      90: Z     106: j     122: z
 43: +      59: ;      75: K      91: [     107: k     123: {
 44: ,      60: <      76: L      92: \     108: l     124: |
 45: -      61: =      77: M      93: ]     109: m     125: }
 46: .      62: >      78: N      94: ^     110: n     126: ~
 47: /      63: ?      79: O      95: _     111: o     127: del    ok
```



## Free Pascal


{{trans|Go}}


```pascal
program ShowASCIITable;

// https://rosettacode.org/wiki/Show_Ascii_table

uses
  sysutils, strutils;

var
  i, j: Integer;
  s: String;
begin
  i := 0;
  j := 0;
  s := '';

  for i := 0 to 15 do begin
    j := 32 + i;
    while j < 128 do begin
      case j of
        32: s := 'Spc';
        127: s := 'Del';
        otherwise s := chr(j);
      end;
      write(format('%s : %s', [padLeft(IntToStr(j), 3), padRight(s, 5)]));
      Inc(j, 16);
    end;
    writeln(' ');
  end;

end.
```


{{out}}

```txt
 32 : Spc   48 : 0     64 : @     80 : P     96 : `    112 : p
 33 : !     49 : 1     65 : A     81 : Q     97 : a    113 : q
 34 : "     50 : 2     66 : B     82 : R     98 : b    114 : r
 35 : #     51 : 3     67 : C     83 : S     99 : c    115 : s
 36 : $     52 : 4     68 : D     84 : T    100 : d    116 : t
 37 : %     53 : 5     69 : E     85 : U    101 : e    117 : u
 38 : &     54 : 6     70 : F     86 : V    102 : f    118 : v
 39 : '     55 : 7     71 : G     87 : W    103 : g    119 : w
 40 : (     56 : 8     72 : H     88 : X    104 : h    120 : x
 41 : )     57 : 9     73 : I     89 : Y    105 : i    121 : y
 42 : *     58 : :     74 : J     90 : Z    106 : j    122 : z
 43 : +     59 : ;     75 : K     91 : [    107 : k    123 : {
 44 : ,     60 : <     76 : L     92 : \    108 : l    124 : |
 45 : -     61 : =     77 : M     93 : ]    109 : m    125 : }
 46 : .     62 : >     78 : N     94 : ^    110 : n    126 : ~
 47 : /     63 : ?     79 : O     95 : _    111 : o    127 : Del
```



## Go


```go
package main

import "fmt"

func main() {
    for i := 0; i < 16; i++ {
        for j := 32 + i; j < 128; j += 16 {
            k := string(j)
            switch j {
            case 32:
                k = "Spc"
            case 127:
                k = "Del"
            }
            fmt.Printf("%3d : %-3s   ", j, k)
        }
        fmt.Println()
    }
}
```


{{out}}

```txt
 32 : Spc    48 : 0      64 : @      80 : P      96 : `     112 : p
 33 : !      49 : 1      65 : A      81 : Q      97 : a     113 : q
 34 : "      50 : 2      66 : B      82 : R      98 : b     114 : r
 35 : #      51 : 3      67 : C      83 : S      99 : c     115 : s
 36 : $      52 : 4      68 : D      84 : T     100 : d     116 : t
 37 : %      53 : 5      69 : E      85 : U     101 : e     117 : u
 38 : &      54 : 6      70 : F      86 : V     102 : f     118 : v
 39 : '      55 : 7      71 : G      87 : W     103 : g     119 : w
 40 : (      56 : 8      72 : H      88 : X     104 : h     120 : x
 41 : )      57 : 9      73 : I      89 : Y     105 : i     121 : y
 42 : *      58 : :      74 : J      90 : Z     106 : j     122 : z
 43 : +      59 : ;      75 : K      91 : [     107 : k     123 : {
 44 : ,      60 : <      76 : L      92 : \     108 : l     124 : |
 45 : -      61 : =      77 : M      93 : ]     109 : m     125 : }
 46 : .      62 : >      78 : N      94 : ^     110 : n     126 : ~
 47 : /      63 : ?      79 : O      95 : _     111 : o     127 : Del
```



## Haskell


```haskell
import Data.List (intercalate, transpose)
import Data.Char (chr)

asciiTable :: String
asciiTable =
  unlines $
  (justifyLeft 12 ' ' =<<) <$>
  transpose (chunksOf 16 $ asciiEntry <$> [32 .. 127])

main :: IO ()
main = putStrLn asciiTable

-------------------------------------------------------------
asciiEntry :: Int -> String
asciiEntry n =
  let k = asciiName n
  in case k of
       [] -> k
       _ -> concat [justifyRight 4 ' ' (show n), " : ", k]

asciiName :: Int -> String
asciiName n
  | 32 > n = []
  | 127 < n = []
  | 32 == n = "Spc"
  | 127 == n = "Del"
  | otherwise = [chr n]

chunksOf :: Int -> [a] -> [[a]]
chunksOf k = go
  where
    go t =
      case splitAt k t of
        (a, b)
          | null a -> []
          | otherwise -> a : go b

justifyLeft, justifyRight :: Int -> Char -> String -> String
justifyLeft n c s = take n (s ++ replicate n c)

justifyRight n c s = drop (length s) (replicate n c ++ s)
```

{{Out}}

```txt
  32 : Spc    48 : 0      64 : @      80 : P      96 : `     112 : p
  33 : !      49 : 1      65 : A      81 : Q      97 : a     113 : q
  34 : "      50 : 2      66 : B      82 : R      98 : b     114 : r
  35 : #      51 : 3      67 : C      83 : S      99 : c     115 : s
  36 : $      52 : 4      68 : D      84 : T     100 : d     116 : t
  37 : %      53 : 5      69 : E      85 : U     101 : e     117 : u
  38 : &      54 : 6      70 : F      86 : V     102 : f     118 : v
  39 : '      55 : 7      71 : G      87 : W     103 : g     119 : w
  40 : (      56 : 8      72 : H      88 : X     104 : h     120 : x
  41 : )      57 : 9      73 : I      89 : Y     105 : i     121 : y
  42 : *      58 : :      74 : J      90 : Z     106 : j     122 : z
  43 : +      59 : ;      75 : K      91 : [     107 : k     123 : {
  44 : ,      60 : <      76 : L      92 : \     108 : l     124 : |
  45 : -      61 : =      77 : M      93 : ]     109 : m     125 : }
  46 : .      62 : >      78 : N      94 : ^     110 : n     126 : ~
  47 : /      63 : ?      79 : O      95 : _     111 : o     127 : Del
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 TEXT 80
110 FOR R=0 TO 15
120   FOR C=32+R TO 112+R STEP 16
130     PRINT USING "###":C;:PRINT ": ";CHR$(C),
140   NEXT
150   PRINT
160 NEXT
```



## J

a. is the 256 ASCII character set.  We'll do a bit of work to make it pretty as the other examples.

```txt
   32}._129}.a.
 !"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~

   NB. A are the decimal ASCII values
   [A =: 10 |."1 ] 12 ": |: _16 [\ 32 }. i. 128
32          48          64          80          96         112
33          49          65          81          97         113
34          50          66          82          98         114
35          51          67          83          99         115
36          52          68          84         100         116
37          53          69          85         101         117
38          54          70          86         102         118
39          55          71          87         103         119
40          56          72          88         104         120
41          57          73          89         105         121
42          58          74          90         106         122
43          59          75          91         107         123
44          60          76          92         108         124
45          61          77          93         109         125
46          62          78          94         110         126
47          63          79          95         111         127

   NB. B are the corresponding ASCII characters
   [B =: |:_16[\32}._128}.a.
 0@P`p
!1AQaq
"2BRbr
#3CScs
$4DTdt
%5EUeu
&6FVfv
'7GWgw
(8HXhx
)9IYiy
*:JZjz
+;K[k{
,<L\l|
-=M]m}
.>N^n~
/?O_o

   NB. stuff the characters into the text array of numbers
   B [`((4 12 p. i. 6)"_)`]}"1 A
32          48  0       64  @       80  P       96  `      112  p
33  !       49  1       65  A       81  Q       97  a      113  q
34  "       50  2       66  B       82  R       98  b      114  r
35  #       51  3       67  C       83  S       99  c      115  s
36  $       52  4       68  D       84  T      100  d      116  t
37  %       53  5       69  E       85  U      101  e      117  u
38  &       54  6       70  F       86  V      102  f      118  v
39  '       55  7       71  G       87  W      103  g      119  w
40  (       56  8       72  H       88  X      104  h      120  x
41  )       57  9       73  I       89  Y      105  i      121  y
42  *       58  :       74  J       90  Z      106  j      122  z
43  +       59  ;       75  K       91  [      107  k      123  {
44  ,       60  <       76  L       92  \      108  l      124  |
45  -       61  =       77  M       93  ]      109  m      125  }
46  .       62  >       78  N       94  ^      110  n      126  ~
47  /       63  ?       79  O       95  _      111  o      127  
```



## JavaScript


```javascript
(() => {

    // asciiTable :: String
    const asciiTable = () =>
        unlines(
            map(xs => concat(
                    map(justifyLeft(12, ' '),
                        xs
                    )
                ),
                transpose(
                    chunksOf(
                        16,
                        map(asciiEntry,
                            enumFromTo(32, 127)
                        )
                    )
                )
            )
        );

    // asciiEntry :: Int -> String
    const asciiEntry = n => {
        const k = asciiName(n);
        return '' === k ? (
            ''
        ) : (justifyRight(4, ' ', n.toString()) + ' : ' + k);
    };

    // asciiName :: Int -> String
    const asciiName = n =>
        32 > n || 127 < n ? (
            ''
        ) : 32 === n ? (
            'Spc'
        ) : 127 === n ? (
            'Del'
        ) : chr(n);

    // GENERIC FUNCTIONS ----------------------------------

    // chunksOf :: Int -> [a] -> [[a]]
    const chunksOf = (n, xs) =>
        xs.reduce((a, _, i, xs) =>
            i % n ? a : a.concat([xs.slice(i, i + n)]), []);

    // chr :: Int -> Char
    const chr = String.fromCodePoint;

    // comparing :: (a -> b) -> (a -> a -> Ordering)
    const comparing = f =>
        (x, y) => {
            const
                a = f(x),
                b = f(y);
            return a < b ? -1 : (a > b ? 1 : 0);
        };

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = (f, xs) =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs.map(f))
        })() : [];

    // enumFromTo :: Int -> Int -> [Int]
    const enumFromTo = (m, n) =>
        m <= n ? iterateUntil(
            x => n <= x,
            x => 1 + x,
            m
        ) : [];

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        const vs = [x];
        let h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // justifyLeft :: Int -> Char -> String -> String
    const justifyLeft = (n, cFiller) => strText =>
        n > strText.length ? (
            (strText + cFiller.repeat(n))
            .substr(0, n)
        ) : strText;

    // justifyRight :: Int -> Char -> String -> String
    const justifyRight = (n, cFiller, strText) =>
        n > strText.length ? (
            (cFiller.repeat(n) + strText)
            .slice(-n)
        ) : strText;

    // length :: [a] -> Int
    const length = xs => xs.length;

    // map :: (a -> b) -> [a] -> [b]
    const map = (f, xs) => xs.map(f);

    // maximumBy :: (a -> a -> Ordering) -> [a] -> a
    const maximumBy = (f, xs) =>
        0 < xs.length ? (
            xs.slice(1)
            .reduce((a, x) => 0 < f(x, a) ? x : a, xs[0])
        ) : undefined;

    // replicate :: Int -> a -> [a]
    const replicate = (n, x) =>
        Array.from({
            length: n
        }, () => x);

    // transpose :: [[a]] -> [[a]]
    const transpose = tbl => {
        const
            gaps = replicate(
                length(maximumBy(comparing(length), tbl)), []
            ),
            rows = map(xs => xs.concat(gaps.slice(xs.length)), tbl);
        return map(
            (_, col) => concatMap(row => [row[col]], rows),
            rows[0]
        );
    };

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN -----------------------------------------------
    return asciiTable();
})();
```

{{Out}}

```txt
  32 : Spc    48 : 0      64 : @      80 : P      96 : `     112 : p
  33 : !      49 : 1      65 : A      81 : Q      97 : a     113 : q
  34 : "      50 : 2      66 : B      82 : R      98 : b     114 : r
  35 : #      51 : 3      67 : C      83 : S      99 : c     115 : s
  36 : $      52 : 4      68 : D      84 : T     100 : d     116 : t
  37 : %      53 : 5      69 : E      85 : U     101 : e     117 : u
  38 : &      54 : 6      70 : F      86 : V     102 : f     118 : v
  39 : '      55 : 7      71 : G      87 : W     103 : g     119 : w
  40 : (      56 : 8      72 : H      88 : X     104 : h     120 : x
  41 : )      57 : 9      73 : I      89 : Y     105 : i     121 : y
  42 : *      58 : :      74 : J      90 : Z     106 : j     122 : z
  43 : +      59 : ;      75 : K      91 : [     107 : k     123 : {
  44 : ,      60 : <      76 : L      92 : \     108 : l     124 : |
  45 : -      61 : =      77 : M      93 : ]     109 : m     125 : }
  46 : .      62 : >      78 : N      94 : ^     110 : n     126 : ~
  47 : /      63 : ?      79 : O      95 : _     111 : o     127 : Del
```



## Jsish


```javascript
#!/usr/bin/env jsish

/* Show ASCII table, -showAll true to include control codes */
function showASCIITable(args:array|string=void, conf:object=void) {
    var options = {           // Rosetta Code, Show ASCII table
        rootdir      :'',      // Root directory.
        showAll      : false  // include control code labels if true
    };
    var self = {};
    parseOpts(self, options, conf);

    function main() {
        var e;
        var first = (self.showAll) ? 0 : 2;
        var filler = '='.repeat(19 + ((first) ? 0 : 9));
        puts(filler, "ASCII table", filler + '=');
        var labels = [
            'NUL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK', 'BEL',
            'BS ', 'HT ', 'LF ', 'VT ', 'FF ', 'CR ', 'SO ', 'SI ',
            'DLE', 'DC1', 'DC2', 'DC3', 'DC4', 'NAK', 'SYN', 'ETB',
            'CAN', 'EM ', 'SUB', 'ESC', 'FS ', 'GS ', 'RS ', 'US '];
        var table = new Array(128);
        for (e = 0; e < 32; e++) table[e] = labels[e];
        for (e = 32; e < 127; e++) table[e] = ' ' + Util.fromCharCode(e) + ' ';
        table[32] = 'SPC';
        table[127] = 'DEL';

        for (var row = 0; row < 16; row++) {
            for (var col = first; col < 8; col++) {
                e = row + col * 16;
                printf('%03d %s  ', e, table[e]);
            }
            printf('\n');
        }
    }
    return main();
}

provide(showASCIITable, 1);

if (isMain()) {
    if (Interp.conf('unitTest')) showASCIITable('', {showAll:true});
    else runModule(showASCIITable);
}


/*
=!EXPECTSTART!=

### ========================= ASCII table ==========================

000 NUL  016 DLE  032 SPC  048  0   064  @   080  P   096  `   112  p
001 SOH  017 DC1  033  !   049  1   065  A   081  Q   097  a   113  q
002 STX  018 DC2  034  "   050  2   066  B   082  R   098  b   114  r
003 ETX  019 DC3  035  #   051  3   067  C   083  S   099  c   115  s
004 EOT  020 DC4  036  $   052  4   068  D   084  T   100  d   116  t
005 ENQ  021 NAK  037  %   053  5   069  E   085  U   101  e   117  u
006 ACK  022 SYN  038  &   054  6   070  F   086  V   102  f   118  v
007 BEL  023 ETB  039  '   055  7   071  G   087  W   103  g   119  w
008 BS   024 CAN  040  (   056  8   072  H   088  X   104  h   120  x
009 HT   025 EM   041  )   057  9   073  I   089  Y   105  i   121  y
010 LF   026 SUB  042  *   058  :   074  J   090  Z   106  j   122  z
011 VT   027 ESC  043  +   059  ;   075  K   091  [   107  k   123  {
012 FF   028 FS   044  ,   060  <   076  L   092  \   108  l   124  |
013 CR   029 GS   045  -   061  =   077  M   093  ]   109  m   125  }
014 SO   030 RS   046  .   062  >   078  N   094  ^   110  n   126  ~
015 SI   031 US   047  /   063  ?   079  O   095  _   111  o   127 DEL
=!EXPECTEND!=
*/
```

{{out}}

```txt
prompt$ jsish -u showASCIITable.jsi
[PASS] showASCIITable.jsi

prompt$ ./showASCIITable.jsi

### ================ ASCII table =================

032 SPC  048  0   064  @   080  P   096  `   112  p
033  !   049  1   065  A   081  Q   097  a   113  q
034  "   050  2   066  B   082  R   098  b   114  r
035  #   051  3   067  C   083  S   099  c   115  s
036  $   052  4   068  D   084  T   100  d   116  t
037  %   053  5   069  E   085  U   101  e   117  u
038  &   054  6   070  F   086  V   102  f   118  v
039  '   055  7   071  G   087  W   103  g   119  w
040  (   056  8   072  H   088  X   104  h   120  x
041  )   057  9   073  I   089  Y   105  i   121  y
042  *   058  :   074  J   090  Z   106  j   122  z
043  +   059  ;   075  K   091  [   107  k   123  {
044  ,   060  <   076  L   092  \   108  l   124  |
045  -   061  =   077  M   093  ]   109  m   125  }
046  .   062  >   078  N   094  ^   110  n   126  ~
047  /   063  ?   079  O   095  _   111  o   127 DEL
```



## Julia


### Base Task


```julia
for i in 32:127
    c= i== 0 ? "NUL" : i== 7 ? "BEL" : i== 8 ? "BKS" : i== 9 ? "TAB" :
       i==10 ? "LF " : i==13 ? "CR " : i==27 ? "ESC" : i==155 ? "CSI" : "|$(Char(i))|"
    print("$(lpad(i,3)) $(string(i,base=16,pad=2)) $c")
    (i&7)==7 ? println() : print("  ")
end
```
{{out}}

```txt

 32 20 | |   33 21 |!|   34 22 |"|   35 23 |#|   36 24 |$|   37 25 |%|   38 26 |&|   39 27 |'|
 40 28 |(|   41 29 |)|   42 2a |*|   43 2b |+|   44 2c |,|   45 2d |-|   46 2e |.|   47 2f |/|
 48 30 |0|   49 31 |1|   50 32 |2|   51 33 |3|   52 34 |4|   53 35 |5|   54 36 |6|   55 37 |7|
 56 38 |8|   57 39 |9|   58 3a |:|   59 3b |;|   60 3c |<|   61 3d |=|   62 3e |>|   63 3f |?|
 64 40 |@|   65 41 |A|   66 42 |B|   67 43 |C|   68 44 |D|   69 45 |E|   70 46 |F|   71 47 |G|
 72 48 |H|   73 49 |I|   74 4a |J|   75 4b |K|   76 4c |L|   77 4d |M|   78 4e |N|   79 4f |O|
 80 50 |P|   81 51 |Q|   82 52 |R|   83 53 |S|   84 54 |T|   85 55 |U|   86 56 |V|   87 57 |W|
 88 58 |X|   89 59 |Y|   90 5a |Z|   91 5b |[|   92 5c |\|   93 5d |]|   94 5e |^|   95 5f |_|
 96 60 |`|   97 61 |a|   98 62 |b|   99 63 |c|  100 64 |d|  101 65 |e|  102 66 |f|  103 67 |g|
104 68 |h|  105 69 |i|  106 6a |j|  107 6b |k|  108 6c |l|  109 6d |m|  110 6e |n|  111 6f |o|
112 70 |p|  113 71 |q|  114 72 |r|  115 73 |s|  116 74 |t|  117 75 |u|  118 76 |v|  119 77 |w|
120 78 |x|  121 79 |y|  122 7a |z|  123 7b |{|  124 7c |||  125 7d |}|  126 7e |~|  127 7f ||

```


### Extended Ascii

The appearance of the table with the extended ASCII characters below depends on the font (code page) used in the terminal and in your browser (DejaVu Sans Mono is a reasonable choice). The output shown is copied from the console ConEmu in Windows 10.
{{works with|Julia|1.0}}


```julia
for i in 0:255
    c= i== 0 ? "NUL" : i== 7 ? "BEL" : i== 8 ? "BKS" : i== 9 ? "TAB" :
       i==10 ? "LF " : i==13 ? "CR " : i==27 ? "ESC" : i==155 ? "CSI" : "|$(Char(i))|"
    print("$(lpad(i,3)) $(string(i,base=16,pad=2)) $c")
    (i&7)==7 ? println() : print("  ")
end
```

{{out}}

```txt
  0 00 NUL    1 01 |☺|    2 02 |☻|    3 03 |♥|    4 04 |♦|    5 05 |♣|    6 06 |♠|    7 07 BEL
  8 08 BKS    9 09 TAB   10 0a LF    11 0b |♂|   12 0c |♀|   13 0d CR    14 0e |♫|   15 0f |☼|
 16 10 |►|   17 11 |◄|   18 12 |↕|   19 13 |‼|   20 14 |¶|   21 15 |§|   22 16 |■|   23 17 |↨|
 24 18 |↑|   25 19 |↓|   26 1a |→|   27 1b ESC   28 1c |∟|   29 1d |↔|   30 1e |▲|   31 1f |▼|
 32 20 | |   33 21 |!|   34 22 |"|   35 23 |#|   36 24 |$|   37 25 |%|   38 26 |&|   39 27 |'|
 40 28 |(|   41 29 |)|   42 2a |*|   43 2b |+|   44 2c |,|   45 2d |-|   46 2e |.|   47 2f |/|
 48 30 |0|   49 31 |1|   50 32 |2|   51 33 |3|   52 34 |4|   53 35 |5|   54 36 |6|   55 37 |7|
 56 38 |8|   57 39 |9|   58 3a |:|   59 3b |;|   60 3c |<|   61 3d |=|   62 3e |>|   63 3f |?|
 64 40 |@|   65 41 |A|   66 42 |B|   67 43 |C|   68 44 |D|   69 45 |E|   70 46 |F|   71 47 |G|
 72 48 |H|   73 49 |I|   74 4a |J|   75 4b |K|   76 4c |L|   77 4d |M|   78 4e |N|   79 4f |O|
 80 50 |P|   81 51 |Q|   82 52 |R|   83 53 |S|   84 54 |T|   85 55 |U|   86 56 |V|   87 57 |W|
 88 58 |X|   89 59 |Y|   90 5a |Z|   91 5b |[|   92 5c |\|   93 5d |]|   94 5e |^|   95 5f |_|
 96 60 |`|   97 61 |a|   98 62 |b|   99 63 |c|  100 64 |d|  101 65 |e|  102 66 |f|  103 67 |g|
104 68 |h|  105 69 |i|  106 6a |j|  107 6b |k|  108 6c |l|  109 6d |m|  110 6e |n|  111 6f |o|
112 70 |p|  113 71 |q|  114 72 |r|  115 73 |s|  116 74 |t|  117 75 |u|  118 76 |v|  119 77 |w|
120 78 |x|  121 79 |y|  122 7a |z|  123 7b |{|  124 7c |||  125 7d |}|  126 7e |~|  127 7f ||
128 80 ||  129 81 ||  130 82 ||  131 83 ||  132 84 ||  133 85 ||  134 86 ||  135 87 ||
136 88 ||  137 89 ||  138 8a ||  139 8b ||  140 8c ||  141 8d ||  142 8e ||  143 8f ||
144 90 ||  145 91 ||  146 92 ||  147 93 ||  148 94 ||  149 95 ||  150 96 ||  151 97 ||
152 98 ||  153 99 ||  154 9a ||  155 9b CSI  156 9c ||  157 9d ||  158 9e ||  159 9f ||
160 a0 | |  161 a1 |¡|  162 a2 |¢|  163 a3 |£|  164 a4 |¤|  165 a5 |¥|  166 a6 |¦|  167 a7 |§|
168 a8 |¨|  169 a9 |©|  170 aa |ª|  171 ab |«|  172 ac |¬|  173 ad |­ |  174 ae |®|  175 af |¯|
176 b0 |°|  177 b1 |±|  178 b2 |²|  179 b3 |³|  180 b4 |´|  181 b5 |µ|  182 b6 |¶|  183 b7 |·|
184 b8 |¸|  185 b9 |¹|  186 ba |º|  187 bb |»|  188 bc |¼|  189 bd |½|  190 be |¾|  191 bf |¿|
192 c0 |À|  193 c1 |Á|  194 c2 |Â|  195 c3 |Ã|  196 c4 |Ä|  197 c5 |Å|  198 c6 |Æ|  199 c7 |Ç|
200 c8 |È|  201 c9 |É|  202 ca |Ê|  203 cb |Ë|  204 cc |Ì|  205 cd |Í|  206 ce |Î|  207 cf |Ï|
208 d0 |Ð|  209 d1 |Ñ|  210 d2 |Ò|  211 d3 |Ó|  212 d4 |Ô|  213 d5 |Õ|  214 d6 |Ö|  215 d7 |×|
216 d8 |Ø|  217 d9 |Ù|  218 da |Ú|  219 db |Û|  220 dc |Ü|  221 dd |Ý|  222 de |Þ|  223 df |ß|
224 e0 |à|  225 e1 |á|  226 e2 |â|  227 e3 |ã|  228 e4 |ä|  229 e5 |å|  230 e6 |æ|  231 e7 |ç|
232 e8 |è|  233 e9 |é|  234 ea |ê|  235 eb |ë|  236 ec |ì|  237 ed |í|  238 ee |î|  239 ef |ï|
240 f0 |ð|  241 f1 |ñ|  242 f2 |ò|  243 f3 |ó|  244 f4 |ô|  245 f5 |õ|  246 f6 |ö|  247 f7 |÷|
248 f8 |ø|  249 f9 |ù|  250 fa |ú|  251 fb |û|  252 fc |ü|  253 fd |ý|  254 fe |þ|  255 ff |ÿ|
```



This version draws a more fancy table, positioning the items on the console monitor with ANSI control sequences:


```julia
print("\e[2J")          # clear screen
print("""
                    0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
                  ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗
                  ║nul│soh│stx│etx│eot│enq│ack│bel│ bs│tab│ lf│ vt│ ff│ cr│ so│ si║
                """)    # indent is set by this (least indented) line
for i = 0:14
    a = string(i,base=16)
println(      "$a ║   │   │   │   │   │   │   │   │   │   │   │   │   │   │   │   ║ $a")
println(       "  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢")
println(i==0 ? "  ║dle│dc1│dc2│dc3│dc4│nak│syn│etb│can│ em│eof│esc│ fs│ gs│ rs│ us║"
             : "  ║   │   │   │   │   │   │   │   │   │   │   │   │   │   │   │   ║")
end
println("""
                f ║   │   │   │   │   │   │   │   │   │   │   │   │   │   │   │   ║ f
                  ╚═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╝
                    0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
                """)    # """ string is indented here
for i = 1:255
    r,c = divrem(i,16)
    r,c = 3r+4,4c+5
    i > 31 && print("\e[$(r-1);$(c-1)H$(lpad(i,3))")
    6<i<11 || i==155 || i==173 || print("\e[$r;$(c)H$(Char(i))")
end
print("\e[54;1H")
```

{{out}}

```txt
    0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
  ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗
  ║nul│soh│stx│etx│eot│enq│ack│bel│ bs│tab│ lf│ vt│ ff│ cr│ so│ si║
0 ║   │ ☺ │ ☻ │ ♥ │ ♦ │ ♣ │ ♠ │   │   │   │   │ ♂ │ ♀ │   │ ♫ │ ☼ ║ 0
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║dle│dc1│dc2│dc3│dc4│nak│syn│etb│can│ em│eof│esc│ fs│ gs│ rs│ us║
1 ║ ► │ ◄ │ ↕ │ ‼ │ ¶ │ § │ ■ │ ↨ │ ↑ │ ↓ │ → │   │ ∟ │ ↔ │ ▲ │ ▼ ║ 1
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║ 32│ 33│ 34│ 35│ 36│ 37│ 38│ 39│ 40│ 41│ 42│ 43│ 44│ 45│ 46│ 47║
2 ║   │ ! │ " │ # │ $ │ % │ & │ ' │ ( │ ) │ * │ + │ , │ - │ . │ / ║ 2
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║ 48│ 49│ 50│ 51│ 52│ 53│ 54│ 55│ 56│ 57│ 58│ 59│ 60│ 61│ 62│ 63║
3 ║ 0 │ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │ 9 │ : │ ; │ < │ = │ > │ ? ║ 3
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║ 64│ 65│ 66│ 67│ 68│ 69│ 70│ 71│ 72│ 73│ 74│ 75│ 76│ 77│ 78│ 79║
4 ║ @ │ A │ B │ C │ D │ E │ F │ G │ H │ I │ J │ K │ L │ M │ N │ O ║ 4
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║ 80│ 81│ 82│ 83│ 84│ 85│ 86│ 87│ 88│ 89│ 90│ 91│ 92│ 93│ 94│ 95║
5 ║ P │ Q │ R │ S │ T │ U │ V │ W │ X │ Y │ Z │ [ │ \ │ ] │ ^ │ _ ║ 5
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║ 96│ 97│ 98│ 99│100│101│102│103│104│105│106│107│108│109│110│111║
6 ║ ` │ a │ b │ c │ d │ e │ f │ g │ h │ i │ j │ k │ l │ m │ n │ o ║ 6
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║112│113│114│115│116│117│118│119│120│121│122│123│124│125│126│127║
7 ║ p │ q │ r │ s │ t │ u │ v │ w │ x │ y │ z │ { │ | │ } │ ~ │  ║ 7
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║128│129│130│131│132│133│134│135│136│137│138│139│140│141│142│143║
8 ║  │  │  │  │  │ │  │  │  │  │  │  │  │  │  │  ║ 8
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║144│145│146│147│148│149│150│151│152│153│154│155│156│157│158│159║
9 ║  │  │  │  │  │  │  │  │  │  │  │   │  │  │  │  ║ 9
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║160│161│162│163│164│165│166│167│168│169│170│171│172│173│174│175║
a ║   │ ¡ │ ¢ │ £ │ ¤ │ ¥ │ ¦ │ § │ ¨ │ © │ ª │ « │ ¬ │   │ ® │ ¯ ║ a
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║176│177│178│179│180│181│182│183│184│185│186│187│188│189│190│191║
b ║ ° │ ± │ ² │ ³ │ ´ │ µ │ ¶ │ · │ ¸ │ ¹ │ º │ » │ ¼ │ ½ │ ¾ │ ¿ ║ b
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║192│193│194│195│196│197│198│199│200│201│202│203│204│205│206│207║
c ║ À │ Á │ Â │ Ã │ Ä │ Å │ Æ │ Ç │ È │ É │ Ê │ Ë │ Ì │ Í │ Î │ Ï ║ c
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║208│209│210│211│212│213│214│215│216│217│218│219│220│221│222│223║
d ║ Ð │ Ñ │ Ò │ Ó │ Ô │ Õ │ Ö │ × │ Ø │ Ù │ Ú │ Û │ Ü │ Ý │ Þ │ ß ║ d
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║224│225│226│227│228│229│230│231│232│233│234│235│236│237│238│239║
e ║ à │ á │ â │ ã │ ä │ å │ æ │ ç │ è │ é │ ê │ ë │ ì │ í │ î │ ï ║ e
  ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
  ║240│241│242│243│244│245│246│247│248│249│250│251│252│253│254│255║
f ║ ð │ ñ │ ò │ ó │ ô │ õ │ ö │ ÷ │ ø │ ù │ ú │ û │ ü │ ý │ þ │ ÿ ║ f
  ╚═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╝
    0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F
```


A similar output can be produced without ANSI control sequences, just filling up a huge string and printing it. Below is a general Object (struct) and the corresponding methods to draw a table of arbitrary shape in the console. It is the Julia way of OOP. The Table structure holds the relevant data and the constructor. The Base.iterate function extends the general iterate function, and allows using the field names in a function, w/o prefixing them with "<structName>." The function prt() fills up a string with data, formatting spaces and new-lines, and prints it to the console.

```julia
#= CONSOLE TABLES
### =======================================================

   rn: nrows, rh: height of rows
   cn: ncols, cw: width of columns
   T[rn,cn,rh] table data strings
   cr: rows in colum headers
   CH[cn,cr] column header strings of max cw length
   hl: lengths of row header strings
   RH[rn,rh] row header strings of max length hl

### ========================================================================
#

struct Table
  rn::Int; rh::Int; cn::Int; cw::Int; T::Array{String,3}
  cr::Int; CH::Array{String,2}
  hl::Int; RH::Array{String,2}
  function Table(rn,rh,cn,cw,cr,hl) # constructor
    new(rn,rh,cn,cw,fill("",(rn,cn,rh)), # arrays initialized with empty strings
        cr,fill("",(cr,cn)), hl,fill("",(rn,rh)))
  end
end
Base.iterate(T::Table,i=1) = i<=nfields(T) ? (getfield(T,i),i+1) : nothing

cpad(s::String,n::Integer) = (m=length(s))<n ? lpad(rpad(s,(n+m)>>1),n) : first(s,n)

function prt((rn,rh,cn,cw,T, cr,CH, hl,RH)::Table)
  TL,TX,TR,BH  = '╔','╤','╗','═'
  IL,IX,IR,IV,IH='╟','┼','╢','│','─'
  BL,BX,BR,BV  = '╚','╧','╝','║'

  u,v,w,d,t = BH^cw, IH^cw, " "^hl, cn-2, " "^hl
  b = w*(cn==1 ? IL*v*IR : IL*v*(IX*v)^d*IX*v*IR)*'\n' # internal separator
  for r = 1:cr
    for c = 1:cn t*=cpad(CH[r,c],cw+1) end
    t *= "\n$w"
  end

  t *= (cn==1 ? TL*u*TR : TL*u*(TX*u)^d*TX*u*TR)*'\n' # top border
  for r = 1:rn
    for p = 1:rh
      s = cpad(RH[r,p],hl)*BV
      for c = 1:cn-1
        s *= cpad(T[r,c,p],cw) * IV
      end
      t*= s * cpad(T[r,cn,p],cw) * BV *'\n'
    end
    t*=r<rn ? b : cn<2 ? w*BL*u*BR : w*BL*u*(BX*u)^d*BX*u*BR # bottom border
  end
  println("\n$t\n")
end
```

Using these is simple, only provide the data, and prt it.

```julia
Tbl = Table(16,2,16,3, 2,3)     # construct table
Tbl.CH[1,:] = string.(0:15,base=16) # Column headers
Tbl.RH[:,2] = string.(0:15,base=16) # Row headers
for i = 0:255                   # populate table, exclude special characters
  Tbl.T[i>>4+1,i&15+1,1:2]=["$i",i∈(0,7,8,9,10,13,27,155) ? "" : "$(Char(i))"]
end
prt(Tbl)                        # format and print table on console
```



## Kotlin

{{trans|Go}}

```scala
// Version 1.2.60

fun main(args: Array<String>) {
    for (i in 0..15) {
        for (j in 32 + i..127 step 16) {
            val k = when (j) {
                32   -> "Spc"
                127  -> "Del"
                else -> j.toChar().toString()
            }
            System.out.printf("%3d : %-3s   ", j, k)
        }
        println()
    }
}
```


{{output}}

```txt
 32 : Spc    48 : 0      64 : @      80 : P      96 : `     112 : p
 33 : !      49 : 1      65 : A      81 : Q      97 : a     113 : q
 34 : "      50 : 2      66 : B      82 : R      98 : b     114 : r
 35 : #      51 : 3      67 : C      83 : S      99 : c     115 : s
 36 : $      52 : 4      68 : D      84 : T     100 : d     116 : t
 37 : %      53 : 5      69 : E      85 : U     101 : e     117 : u
 38 : &      54 : 6      70 : F      86 : V     102 : f     118 : v
 39 : '      55 : 7      71 : G      87 : W     103 : g     119 : w
 40 : (      56 : 8      72 : H      88 : X     104 : h     120 : x
 41 : )      57 : 9      73 : I      89 : Y     105 : i     121 : y
 42 : *      58 : :      74 : J      90 : Z     106 : j     122 : z
 43 : +      59 : ;      75 : K      91 : [     107 : k     123 : {
 44 : ,      60 : <      76 : L      92 : \     108 : l     124 : |
 45 : -      61 : =      77 : M      93 : ]     109 : m     125 : }
 46 : .      62 : >      78 : N      94 : ^     110 : n     126 : ~
 47 : /      63 : ?      79 : O      95 : _     111 : o     127 : Del
```



## Langur

{{trans|Go}}

```Langur
# using the fact that string multiplication does not throw on negative numbers (treat like 0)
val .padL = f(.s, .with, .len) .with x (.len - len(.s)) ~ .s
val .padR = f(.s, .with, .len) .s ~ .with x (.len - len(.s))

for .i of 16 {
    for .j = 31 + .i; .j < 128; .j += 16 {
        write .padL(toString(.j), " ", 3), " : "
        write .padR(given(.j; 32: "spc"; 127: "del"; cp2s .j), " ", 4)
    }
    writeln()
}
```


{{out}}

```txt
 32 : spc  48 : 0    64 : @    80 : P    96 : `   112 : p
 33 : !    49 : 1    65 : A    81 : Q    97 : a   113 : q
 34 : "    50 : 2    66 : B    82 : R    98 : b   114 : r
 35 : #    51 : 3    67 : C    83 : S    99 : c   115 : s
 36 : $    52 : 4    68 : D    84 : T   100 : d   116 : t
 37 : %    53 : 5    69 : E    85 : U   101 : e   117 : u
 38 : &    54 : 6    70 : F    86 : V   102 : f   118 : v
 39 : '    55 : 7    71 : G    87 : W   103 : g   119 : w
 40 : (    56 : 8    72 : H    88 : X   104 : h   120 : x
 41 : )    57 : 9    73 : I    89 : Y   105 : i   121 : y
 42 : *    58 : :    74 : J    90 : Z   106 : j   122 : z
 43 : +    59 : ;    75 : K    91 : [   107 : k   123 : {
 44 : ,    60 : <    76 : L    92 : \   108 : l   124 : |
 45 : -    61 : =    77 : M    93 : ]   109 : m   125 : }
 46 : .    62 : >    78 : N    94 : ^   110 : n   126 : ~
 47 : /    63 : ?    79 : O    95 : _   111 : o   127 : del
```



## Lua

{{trans|Go}}

```lua

-- map of character values to desired representation
local chars = setmetatable({[32] = "Spc", [127] = "Del"}, {__index = function(_, k) return string.char(k) end})

-- row iterator
local function iter(s,a)
  a = (a or s) + 16
  if a <= 127 then return a, chars[a] end
end

-- print loop
for i = 0, 15 do
   for j, repr in iter, i+16 do
      io.write(("%3d : %3s    "):format(j, repr))
   end
   io.write"\n"
end
```


{{out}}

```txt
 32 : Spc     48 :   0     64 :   @     80 :   P     96 :   `    112 :   p
 33 :   !     49 :   1     65 :   A     81 :   Q     97 :   a    113 :   q
 34 :   "     50 :   2     66 :   B     82 :   R     98 :   b    114 :   r
 35 :   #     51 :   3     67 :   C     83 :   S     99 :   c    115 :   s
 36 :   $     52 :   4     68 :   D     84 :   T    100 :   d    116 :   t
 37 :   %     53 :   5     69 :   E     85 :   U    101 :   e    117 :   u
 38 :   &     54 :   6     70 :   F     86 :   V    102 :   f    118 :   v
 39 :   '     55 :   7     71 :   G     87 :   W    103 :   g    119 :   w
 40 :   (     56 :   8     72 :   H     88 :   X    104 :   h    120 :   x
 41 :   )     57 :   9     73 :   I     89 :   Y    105 :   i    121 :   y
 42 :   *     58 :   :     74 :   J     90 :   Z    106 :   j    122 :   z
 43 :   +     59 :   ;     75 :   K     91 :   [    107 :   k    123 :   {
 44 :   ,     60 :   <     76 :   L     92 :   \    108 :   l    124 :   |
 45 :   -     61 :   =     77 :   M     93 :   ]    109 :   m    125 :   }
 46 :   .     62 :   >     78 :   N     94 :   ^    110 :   n    126 :   ~
 47 :   /     63 :   ?     79 :   O     95 :   _    111 :   o    127 : Del
```



## MiniScript


```MiniScript
// Prints ASCII table
// Note changing the values of startChar and endChar will print
// a flexible table in that range

startChar = 32
endChar = 127
characters = []

for i in range(startChar, endChar)
    addString = char(i) + "  "
    if i == 32 then addString = "SPC"
    if i == 127 then addString = "DEL"
    characters.push addString
end for

for i in characters.indexes
    iNum = i + startChar
    iChar = "     " + str(iNum)
    characters[i] = iChar[-5:] + " : " + characters[i]
end for

columns = 6
line = ""
col = 0
for out in characters
    col = col + 1
    line = line + out
    if col == columns then
        print line
        line = ""
        col = 0
    end if
end for
if line then print line // final check for odd incomplete line output
```

{{out}}

```txt

   32 : SPC   33 : !     34 : "     35 : #     36 : $     37 : %
   38 : &     39 : '     40 : (     41 : )     42 : *     43 : +
   44 : ,     45 : -     46 : .     47 : /     48 : 0     49 : 1
   50 : 2     51 : 3     52 : 4     53 : 5     54 : 6     55 : 7
   56 : 8     57 : 9     58 : :     59 : ;     60 : <     61 : =
   62 : >     63 : ?     64 : @     65 : A     66 : B     67 : C
   68 : D     69 : E     70 : F     71 : G     72 : H     73 : I
   74 : J     75 : K     76 : L     77 : M     78 : N     79 : O
   80 : P     81 : Q     82 : R     83 : S     84 : T     85 : U
   86 : V     87 : W     88 : X     89 : Y     90 : Z     91 : [
   92 : \     93 : ]     94 : ^     95 : _     96 : `     97 : a
   98 : b     99 : c    100 : d    101 : e    102 : f    103 : g
  104 : h    105 : i    106 : j    107 : k    108 : l    109 : m
  110 : n    111 : o    112 : p    113 : q    114 : r    115 : s
  116 : t    117 : u    118 : v    119 : w    120 : x    121 : y
  122 : z    123 : {    124 : |    125 : }    126 : ~    127 : DEL

```



## Nim


```Nim
import strformat

for i in 0..15:
  for j in countup(32 + i, 127, step = 16):
    var k = ""
    case j
    of 32:
      k = "Spc"
    of 127:
      k = "Del"
    else:
      k = $char(j)
    write(stdout, fmt"{j:3d} : {k:<6s}")
  write(stdout, "\n")
```

{{out}}

```txt

 32 : Spc    48 : 0      64 : @      80 : P      96 : `     112 : p
 33 : !      49 : 1      65 : A      81 : Q      97 : a     113 : q
 34 : "      50 : 2      66 : B      82 : R      98 : b     114 : r
 35 : #      51 : 3      67 : C      83 : S      99 : c     115 : s
 36 : $      52 : 4      68 : D      84 : T     100 : d     116 : t
 37 : %      53 : 5      69 : E      85 : U     101 : e     117 : u
 38 : &      54 : 6      70 : F      86 : V     102 : f     118 : v
 39 : '      55 : 7      71 : G      87 : W     103 : g     119 : w
 40 : (      56 : 8      72 : H      88 : X     104 : h     120 : x
 41 : )      57 : 9      73 : I      89 : Y     105 : i     121 : y
 42 : *      58 : :      74 : J      90 : Z     106 : j     122 : z
 43 : +      59 : ;      75 : K      91 : [     107 : k     123 : {
 44 : ,      60 : <      76 : L      92 : \     108 : l     124 : |
 45 : -      61 : =      77 : M      93 : ]     109 : m     125 : }
 46 : .      62 : >      78 : N      94 : ^     110 : n     126 : ~
 47 : /      63 : ?      79 : O      95 : _     111 : o     127 : Del

```



## Perl

Output in the same style as Perl 6.
{{trans|Perl 6}}

```perl
use charnames ':full';
binmode STDOUT, ':utf8';

sub glyph {
    my($n) = @_;
    if    ($n < 33) { chr 0x2400 + $n } # display symbol names for invisible glyphs
    elsif ($n==124) { '<nowiki>|</nowiki>' }
    elsif ($n==127) { 'DEL' }
    else            { chr $n }
}

print qq[{|class="wikitable" style="text-align:center;background-color:hsl(39, 90%, 95%)"\n];

for (0..127) {
    print qq[|-\n] unless $_ % 16;;
        printf qq[|%d
0x%02X
<big><big title="%s">%s</big></big>\n],
                $_, $_, charnames::viacode($_), glyph($_);
    }
}
print qq[|}\n];
```



## Perl 6


Alternately, and perhaps more usefully, output as a wiki-table rather than ASCII art. Hover mouse over the glyph to get the name.


```perl6
sub glyph ($_) {
    when * < 33 { (0x2400 + $_).chr } # display symbol names for invisible glyphs
    when 127    { '␡' }
    default     { .chr }
}

say '{|class="wikitable" style="text-align:center;background-color:hsl(39, 90%, 95%)"';

for (^128).rotor(16) -> @row {
    say '|-';
    printf(q[|%d
0x%02X
<big><big title="%s">%s</big></big>] ~ "\n",
      $_, $_, .&glyph.uniname.subst('SYMBOL FOR ', ''),
      .&glyph.subst('|', '<nowiki>|</nowiki>')) for @row;
}

say '|}';
```

{{out}}
{|class="wikitable" style="text-align:center;background-color:hsl(39, 90%, 95%)"
|-
|0
0x00
<big><big title="NULL">␀</big></big>
|1
0x01
<big><big title="START OF HEADING">␁</big></big>
|2
0x02
<big><big title="START OF TEXT">␂</big></big>
|3
0x03
<big><big title="END OF TEXT">␃</big></big>
|4
0x04
<big><big title="END OF TRANSMISSION">␄</big></big>
|5
0x05
<big><big title="ENQUIRY">␅</big></big>
|6
0x06
<big><big title="ACKNOWLEDGE">␆</big></big>
|7
0x07
<big><big title="BELL">␇</big></big>
|8
0x08
<big><big title="BACKSPACE">␈</big></big>
|9
0x09
<big><big title="HORIZONTAL TABULATION">␉</big></big>
|10
0x0A
<big><big title="LINE FEED">␊</big></big>
|11
0x0B
<big><big title="VERTICAL TABULATION">␋</big></big>
|12
0x0C
<big><big title="FORM FEED">␌</big></big>
|13
0x0D
<big><big title="CARRIAGE RETURN">␍</big></big>
|14
0x0E
<big><big title="SHIFT OUT">␎</big></big>
|15
0x0F
<big><big title="SHIFT IN">␏</big></big>
|-
|16
0x10
<big><big title="DATA LINK ESCAPE">␐</big></big>
|17
0x11
<big><big title="DEVICE CONTROL ONE">␑</big></big>
|18
0x12
<big><big title="DEVICE CONTROL TWO">␒</big></big>
|19
0x13
<big><big title="DEVICE CONTROL THREE">␓</big></big>
|20
0x14
<big><big title="DEVICE CONTROL FOUR">␔</big></big>
|21
0x15
<big><big title="NEGATIVE ACKNOWLEDGE">␕</big></big>
|22
0x16
<big><big title="SYNCHRONOUS IDLE">␖</big></big>
|23
0x17
<big><big title="END OF TRANSMISSION BLOCK">␗</big></big>
|24
0x18
<big><big title="CANCEL">␘</big></big>
|25
0x19
<big><big title="END OF MEDIUM">␙</big></big>
|26
0x1A
<big><big title="SUBSTITUTE">␚</big></big>
|27
0x1B
<big><big title="ESCAPE">␛</big></big>
|28
0x1C
<big><big title="FILE SEPARATOR">␜</big></big>
|29
0x1D
<big><big title="GROUP SEPARATOR">␝</big></big>
|30
0x1E
<big><big title="RECORD SEPARATOR">␞</big></big>
|31
0x1F
<big><big title="UNIT SEPARATOR">␟</big></big>
|-
|32
0x20
<big><big title="SPACE">␠</big></big>
|33
0x21
<big><big title="EXCLAMATION MARK">!</big></big>
|34
0x22
<big><big title="QUOTATION MARK">"</big></big>
|35
0x23
<big><big title="NUMBER SIGN">#</big></big>
|36
0x24
<big><big title="DOLLAR SIGN">$</big></big>
|37
0x25
<big><big title="PERCENT SIGN">%</big></big>
|38
0x26
<big><big title="AMPERSAND">&</big></big>
|39
0x27
<big><big title="APOSTROPHE">'</big></big>
|40
0x28
<big><big title="LEFT PARENTHESIS">(</big></big>
|41
0x29
<big><big title="RIGHT PARENTHESIS">)</big></big>
|42
0x2A
<big><big title="ASTERISK">*</big></big>
|43
0x2B
<big><big title="PLUS SIGN">+</big></big>
|44
0x2C
<big><big title="COMMA">,</big></big>
|45
0x2D
<big><big title="HYPHEN-MINUS">-</big></big>
|46
0x2E
<big><big title="FULL STOP">.</big></big>
|47
0x2F
<big><big title="SOLIDUS">/</big></big>
|-
|48
0x30
<big><big title="DIGIT ZERO">0</big></big>
|49
0x31
<big><big title="DIGIT ONE">1</big></big>
|50
0x32
<big><big title="DIGIT TWO">2</big></big>
|51
0x33
<big><big title="DIGIT THREE">3</big></big>
|52
0x34
<big><big title="DIGIT FOUR">4</big></big>
|53
0x35
<big><big title="DIGIT FIVE">5</big></big>
|54
0x36
<big><big title="DIGIT SIX">6</big></big>
|55
0x37
<big><big title="DIGIT SEVEN">7</big></big>
|56
0x38
<big><big title="DIGIT EIGHT">8</big></big>
|57
0x39
<big><big title="DIGIT NINE">9</big></big>
|58
0x3A
<big><big title="COLON">:</big></big>
|59
0x3B
<big><big title="SEMICOLON">;</big></big>
|60
0x3C
<big><big title="LESS-THAN SIGN"><</big></big>
|61
0x3D
<big><big title="EQUALS SIGN">=</big></big>
|62
0x3E
<big><big title="GREATER-THAN SIGN">></big></big>
|63
0x3F
<big><big title="QUESTION MARK">?</big></big>
|-
|64
0x40
<big><big title="COMMERCIAL AT">@</big></big>
|65
0x41
<big><big title="LATIN CAPITAL LETTER A">A</big></big>
|66
0x42
<big><big title="LATIN CAPITAL LETTER B">B</big></big>
|67
0x43
<big><big title="LATIN CAPITAL LETTER C">C</big></big>
|68
0x44
<big><big title="LATIN CAPITAL LETTER D">D</big></big>
|69
0x45
<big><big title="LATIN CAPITAL LETTER E">E</big></big>
|70
0x46
<big><big title="LATIN CAPITAL LETTER F">F</big></big>
|71
0x47
<big><big title="LATIN CAPITAL LETTER G">G</big></big>
|72
0x48
<big><big title="LATIN CAPITAL LETTER H">H</big></big>
|73
0x49
<big><big title="LATIN CAPITAL LETTER I">I</big></big>
|74
0x4A
<big><big title="LATIN CAPITAL LETTER J">J</big></big>
|75
0x4B
<big><big title="LATIN CAPITAL LETTER K">K</big></big>
|76
0x4C
<big><big title="LATIN CAPITAL LETTER L">L</big></big>
|77
0x4D
<big><big title="LATIN CAPITAL LETTER M">M</big></big>
|78
0x4E
<big><big title="LATIN CAPITAL LETTER N">N</big></big>
|79
0x4F
<big><big title="LATIN CAPITAL LETTER O">O</big></big>
|-
|80
0x50
<big><big title="LATIN CAPITAL LETTER P">P</big></big>
|81
0x51
<big><big title="LATIN CAPITAL LETTER Q">Q</big></big>
|82
0x52
<big><big title="LATIN CAPITAL LETTER R">R</big></big>
|83
0x53
<big><big title="LATIN CAPITAL LETTER S">S</big></big>
|84
0x54
<big><big title="LATIN CAPITAL LETTER T">T</big></big>
|85
0x55
<big><big title="LATIN CAPITAL LETTER U">U</big></big>
|86
0x56
<big><big title="LATIN CAPITAL LETTER V">V</big></big>
|87
0x57
<big><big title="LATIN CAPITAL LETTER W">W</big></big>
|88
0x58
<big><big title="LATIN CAPITAL LETTER X">X</big></big>
|89
0x59
<big><big title="LATIN CAPITAL LETTER Y">Y</big></big>
|90
0x5A
<big><big title="LATIN CAPITAL LETTER Z">Z</big></big>
|91
0x5B
<big><big title="LEFT SQUARE BRACKET">[</big></big>
|92
0x5C
<big><big title="REVERSE SOLIDUS">\</big></big>
|93
0x5D
<big><big title="RIGHT SQUARE BRACKET">]</big></big>
|94
0x5E
<big><big title="CIRCUMFLEX ACCENT">^</big></big>
|95
0x5F
<big><big title="LOW LINE">_</big></big>
|-
|96
0x60
<big><big title="GRAVE ACCENT">`</big></big>
|97
0x61
<big><big title="LATIN SMALL LETTER A">a</big></big>
|98
0x62
<big><big title="LATIN SMALL LETTER B">b</big></big>
|99
0x63
<big><big title="LATIN SMALL LETTER C">c</big></big>
|100
0x64
<big><big title="LATIN SMALL LETTER D">d</big></big>
|101
0x65
<big><big title="LATIN SMALL LETTER E">e</big></big>
|102
0x66
<big><big title="LATIN SMALL LETTER F">f</big></big>
|103
0x67
<big><big title="LATIN SMALL LETTER G">g</big></big>
|104
0x68
<big><big title="LATIN SMALL LETTER H">h</big></big>
|105
0x69
<big><big title="LATIN SMALL LETTER I">i</big></big>
|106
0x6A
<big><big title="LATIN SMALL LETTER J">j</big></big>
|107
0x6B
<big><big title="LATIN SMALL LETTER K">k</big></big>
|108
0x6C
<big><big title="LATIN SMALL LETTER L">l</big></big>
|109
0x6D
<big><big title="LATIN SMALL LETTER M">m</big></big>
|110
0x6E
<big><big title="LATIN SMALL LETTER N">n</big></big>
|111
0x6F
<big><big title="LATIN SMALL LETTER O">o</big></big>
|-
|112
0x70
<big><big title="LATIN SMALL LETTER P">p</big></big>
|113
0x71
<big><big title="LATIN SMALL LETTER Q">q</big></big>
|114
0x72
<big><big title="LATIN SMALL LETTER R">r</big></big>
|115
0x73
<big><big title="LATIN SMALL LETTER S">s</big></big>
|116
0x74
<big><big title="LATIN SMALL LETTER T">t</big></big>
|117
0x75
<big><big title="LATIN SMALL LETTER U">u</big></big>
|118
0x76
<big><big title="LATIN SMALL LETTER V">v</big></big>
|119
0x77
<big><big title="LATIN SMALL LETTER W">w</big></big>
|120
0x78
<big><big title="LATIN SMALL LETTER X">x</big></big>
|121
0x79
<big><big title="LATIN SMALL LETTER Y">y</big></big>
|122
0x7A
<big><big title="LATIN SMALL LETTER Z">z</big></big>
|123
0x7B
<big><big title="LEFT CURLY BRACKET">{</big></big>
|124
0x7C
<big><big title="VERTICAL LINE"><nowiki>|</nowiki></big></big>
|125
0x7D
<big><big title="RIGHT CURLY BRACKET">}</big></big>
|126
0x7E
<big><big title="TILDE">~</big></big>
|127
0x7F
<big><big title="DELETE">␡</big></big>
|}


## Phix


```Phix
sequence ascii = {}
for ch=32 to 127 do
    ascii = append(ascii,sprintf("%4d (#%02x): %c ",ch))
end for
puts(1,substitute(join_by(ascii,16,6),x"7F","del"))
```

{{out}}

```txt
  32 (#20):        48 (#30): 0      64 (#40): @      80 (#50): P      96 (#60): `     112 (#70): p
  33 (#21): !      49 (#31): 1      65 (#41): A      81 (#51): Q      97 (#61): a     113 (#71): q
  34 (#22): "      50 (#32): 2      66 (#42): B      82 (#52): R      98 (#62): b     114 (#72): r
  35 (#23): #      51 (#33): 3      67 (#43): C      83 (#53): S      99 (#63): c     115 (#73): s
  36 (#24): $      52 (#34): 4      68 (#44): D      84 (#54): T     100 (#64): d     116 (#74): t
  37 (#25): %      53 (#35): 5      69 (#45): E      85 (#55): U     101 (#65): e     117 (#75): u
  38 (#26): &      54 (#36): 6      70 (#46): F      86 (#56): V     102 (#66): f     118 (#76): v
  39 (#27): '      55 (#37): 7      71 (#47): G      87 (#57): W     103 (#67): g     119 (#77): w
  40 (#28): (      56 (#38): 8      72 (#48): H      88 (#58): X     104 (#68): h     120 (#78): x
  41 (#29): )      57 (#39): 9      73 (#49): I      89 (#59): Y     105 (#69): i     121 (#79): y
  42 (#2A): *      58 (#3A): :      74 (#4A): J      90 (#5A): Z     106 (#6A): j     122 (#7A): z
  43 (#2B): +      59 (#3B): ;      75 (#4B): K      91 (#5B): [     107 (#6B): k     123 (#7B): {
  44 (#2C): ,      60 (#3C): <      76 (#4C): L      92 (#5C): \     108 (#6C): l     124 (#7C): |
  45 (#2D): -      61 (#3D): =      77 (#4D): M      93 (#5D): ]     109 (#6D): m     125 (#7D): }
  46 (#2E): .      62 (#3E): >      78 (#4E): N      94 (#5E): ^     110 (#6E): n     126 (#7E): ~
  47 (#2F): /      63 (#3F): ?      79 (#4F): O      95 (#5F): _     111 (#6F): o     127 (#7F): del
```



## PureBasic


```PureBasic
If OpenConsole("Show_Ascii_table: rosettacode.org")
  Define r.i, c.i
  For r=0 To 15
    For c=32+r To 112+r Step 16
      Print(RSet(Str(c),3)+" : ")
      Select c
        Case 32
          Print("Spc")
        Case 127
          Print("Del")
        Default
          Print(LSet(Chr(c),3))
      EndSelect
      Print(Space(3))
    Next
    PrintN("")
  Next
  Input()
EndIf
```

{{out}}

```txt
 32 : Spc    48 : 0      64 : @      80 : P      96 : `     112 : p
 33 : !      49 : 1      65 : A      81 : Q      97 : a     113 : q
 34 : &quot;      50 : 2      66 : B      82 : R      98 : b     114 : r
 35 : #      51 : 3      67 : C      83 : S      99 : c     115 : s
 36 : $      52 : 4      68 : D      84 : T     100 : d     116 : t
 37 : %      53 : 5      69 : E      85 : U     101 : e     117 : u
 38 : &amp;      54 : 6      70 : F      86 : V     102 : f     118 : v
 39 : &apos;      55 : 7      71 : G      87 : W     103 : g     119 : w
 40 : (      56 : 8      72 : H      88 : X     104 : h     120 : x
 41 : )      57 : 9      73 : I      89 : Y     105 : i     121 : y
 42 : *      58 : :      74 : J      90 : Z     106 : j     122 : z
 43 : +      59 : ;      75 : K      91 : [     107 : k     123 : {
 44 : ,      60 : &lt;      76 : L      92 : \     108 : l     124 : |
 45 : -      61 : =      77 : M      93 : ]     109 : m     125 : }
 46 : .      62 : &gt;      78 : N      94 : ^     110 : n     126 : ~
 47 : /      63 : ?      79 : O      95 : _     111 : o     127 : Del
```



## Python


### HTML

After Perl6, but creating an HTML table:

```python
from unicodedata import name
from html import escape

def pp(n):
    if n <= 32:
        return chr(0x2400 + n)
    if n == 127:
        return '␡'
    return chr(n)

print('<table border="3px" style="background-color:LightCyan;text-align:center">\n <tr>')
for n in range(128):
    if n %16 == 0 and 1 < n:
        print(" </tr><tr>")
    print(f'  <td style="center">{n}
0x{n:02x}
<big><b title="{escape(name(pp(n)))}">{escape(pp(n))}</b></big></td>')
print(""" </tr>\n</table>""")
```


{{out}}
<table border="3px" style="background-color:LightCyan;text-align:center">
 <tr>
  <td style="center">0
0x00
<big><b title="SYMBOL FOR NULL">␀</b></big></td>
  <td style="center">1
0x01
<big><b title="SYMBOL FOR START OF HEADING">␁</b></big></td>
  <td style="center">2
0x02
<big><b title="SYMBOL FOR START OF TEXT">␂</b></big></td>
  <td style="center">3
0x03
<big><b title="SYMBOL FOR END OF TEXT">␃</b></big></td>
  <td style="center">4
0x04
<big><b title="SYMBOL FOR END OF TRANSMISSION">␄</b></big></td>
  <td style="center">5
0x05
<big><b title="SYMBOL FOR ENQUIRY">␅</b></big></td>
  <td style="center">6
0x06
<big><b title="SYMBOL FOR ACKNOWLEDGE">␆</b></big></td>
  <td style="center">7
0x07
<big><b title="SYMBOL FOR BELL">␇</b></big></td>
  <td style="center">8
0x08
<big><b title="SYMBOL FOR BACKSPACE">␈</b></big></td>
  <td style="center">9
0x09
<big><b title="SYMBOL FOR HORIZONTAL TABULATION">␉</b></big></td>
  <td style="center">10
0x0a
<big><b title="SYMBOL FOR LINE FEED">␊</b></big></td>
  <td style="center">11
0x0b
<big><b title="SYMBOL FOR VERTICAL TABULATION">␋</b></big></td>
  <td style="center">12
0x0c
<big><b title="SYMBOL FOR FORM FEED">␌</b></big></td>
  <td style="center">13
0x0d
<big><b title="SYMBOL FOR CARRIAGE RETURN">␍</b></big></td>
  <td style="center">14
0x0e
<big><b title="SYMBOL FOR SHIFT OUT">␎</b></big></td>
  <td style="center">15
0x0f
<big><b title="SYMBOL FOR SHIFT IN">␏</b></big></td>
 </tr><tr>
  <td style="center">16
0x10
<big><b title="SYMBOL FOR DATA LINK ESCAPE">␐</b></big></td>
  <td style="center">17
0x11
<big><b title="SYMBOL FOR DEVICE CONTROL ONE">␑</b></big></td>
  <td style="center">18
0x12
<big><b title="SYMBOL FOR DEVICE CONTROL TWO">␒</b></big></td>
  <td style="center">19
0x13
<big><b title="SYMBOL FOR DEVICE CONTROL THREE">␓</b></big></td>
  <td style="center">20
0x14
<big><b title="SYMBOL FOR DEVICE CONTROL FOUR">␔</b></big></td>
  <td style="center">21
0x15
<big><b title="SYMBOL FOR NEGATIVE ACKNOWLEDGE">␕</b></big></td>
  <td style="center">22
0x16
<big><b title="SYMBOL FOR SYNCHRONOUS IDLE">␖</b></big></td>
  <td style="center">23
0x17
<big><b title="SYMBOL FOR END OF TRANSMISSION BLOCK">␗</b></big></td>
  <td style="center">24
0x18
<big><b title="SYMBOL FOR CANCEL">␘</b></big></td>
  <td style="center">25
0x19
<big><b title="SYMBOL FOR END OF MEDIUM">␙</b></big></td>
  <td style="center">26
0x1a
<big><b title="SYMBOL FOR SUBSTITUTE">␚</b></big></td>
  <td style="center">27
0x1b
<big><b title="SYMBOL FOR ESCAPE">␛</b></big></td>
  <td style="center">28
0x1c
<big><b title="SYMBOL FOR FILE SEPARATOR">␜</b></big></td>
  <td style="center">29
0x1d
<big><b title="SYMBOL FOR GROUP SEPARATOR">␝</b></big></td>
  <td style="center">30
0x1e
<big><b title="SYMBOL FOR RECORD SEPARATOR">␞</b></big></td>
  <td style="center">31
0x1f
<big><b title="SYMBOL FOR UNIT SEPARATOR">␟</b></big></td>
 </tr><tr>
  <td style="center">32
0x20
<big><b title="SYMBOL FOR SPACE">␠</b></big></td>
  <td style="center">33
0x21
<big><b title="EXCLAMATION MARK">!</b></big></td>
  <td style="center">34
0x22
<big><b title="QUOTATION MARK">&quot;</b></big></td>
  <td style="center">35
0x23
<big><b title="NUMBER SIGN">#</b></big></td>
  <td style="center">36
0x24
<big><b title="DOLLAR SIGN">$</b></big></td>
  <td style="center">37
0x25
<big><b title="PERCENT SIGN">%</b></big></td>
  <td style="center">38
0x26
<big><b title="AMPERSAND">&amp;</b></big></td>
  <td style="center">39
0x27
<big><b title="APOSTROPHE">&#x27;</b></big></td>
  <td style="center">40
0x28
<big><b title="LEFT PARENTHESIS">(</b></big></td>
  <td style="center">41
0x29
<big><b title="RIGHT PARENTHESIS">)</b></big></td>
  <td style="center">42
0x2a
<big><b title="ASTERISK">*</b></big></td>
  <td style="center">43
0x2b
<big><b title="PLUS SIGN">+</b></big></td>
  <td style="center">44
0x2c
<big><b title="COMMA">,</b></big></td>
  <td style="center">45
0x2d
<big><b title="HYPHEN-MINUS">-</b></big></td>
  <td style="center">46
0x2e
<big><b title="FULL STOP">.</b></big></td>
  <td style="center">47
0x2f
<big><b title="SOLIDUS">/</b></big></td>
 </tr><tr>
  <td style="center">48
0x30
<big><b title="DIGIT ZERO">0</b></big></td>
  <td style="center">49
0x31
<big><b title="DIGIT ONE">1</b></big></td>
  <td style="center">50
0x32
<big><b title="DIGIT TWO">2</b></big></td>
  <td style="center">51
0x33
<big><b title="DIGIT THREE">3</b></big></td>
  <td style="center">52
0x34
<big><b title="DIGIT FOUR">4</b></big></td>
  <td style="center">53
0x35
<big><b title="DIGIT FIVE">5</b></big></td>
  <td style="center">54
0x36
<big><b title="DIGIT SIX">6</b></big></td>
  <td style="center">55
0x37
<big><b title="DIGIT SEVEN">7</b></big></td>
  <td style="center">56
0x38
<big><b title="DIGIT EIGHT">8</b></big></td>
  <td style="center">57
0x39
<big><b title="DIGIT NINE">9</b></big></td>
  <td style="center">58
0x3a
<big><b title="COLON">:</b></big></td>
  <td style="center">59
0x3b
<big><b title="SEMICOLON">;</b></big></td>
  <td style="center">60
0x3c
<big><b title="LESS-THAN SIGN">&lt;</b></big></td>
  <td style="center">61
0x3d
<big><b title="EQUALS SIGN">=</b></big></td>
  <td style="center">62
0x3e
<big><b title="GREATER-THAN SIGN">&gt;</b></big></td>
  <td style="center">63
0x3f
<big><b title="QUESTION MARK">?</b></big></td>
 </tr><tr>
  <td style="center">64
0x40
<big><b title="COMMERCIAL AT">@</b></big></td>
  <td style="center">65
0x41
<big><b title="LATIN CAPITAL LETTER A">A</b></big></td>
  <td style="center">66
0x42
<big><b title="LATIN CAPITAL LETTER B">B</b></big></td>
  <td style="center">67
0x43
<big><b title="LATIN CAPITAL LETTER C">C</b></big></td>
  <td style="center">68
0x44
<big><b title="LATIN CAPITAL LETTER D">D</b></big></td>
  <td style="center">69
0x45
<big><b title="LATIN CAPITAL LETTER E">E</b></big></td>
  <td style="center">70
0x46
<big><b title="LATIN CAPITAL LETTER F">F</b></big></td>
  <td style="center">71
0x47
<big><b title="LATIN CAPITAL LETTER G">G</b></big></td>
  <td style="center">72
0x48
<big><b title="LATIN CAPITAL LETTER H">H</b></big></td>
  <td style="center">73
0x49
<big><b title="LATIN CAPITAL LETTER I">I</b></big></td>
  <td style="center">74
0x4a
<big><b title="LATIN CAPITAL LETTER J">J</b></big></td>
  <td style="center">75
0x4b
<big><b title="LATIN CAPITAL LETTER K">K</b></big></td>
  <td style="center">76
0x4c
<big><b title="LATIN CAPITAL LETTER L">L</b></big></td>
  <td style="center">77
0x4d
<big><b title="LATIN CAPITAL LETTER M">M</b></big></td>
  <td style="center">78
0x4e
<big><b title="LATIN CAPITAL LETTER N">N</b></big></td>
  <td style="center">79
0x4f
<big><b title="LATIN CAPITAL LETTER O">O</b></big></td>
 </tr><tr>
  <td style="center">80
0x50
<big><b title="LATIN CAPITAL LETTER P">P</b></big></td>
  <td style="center">81
0x51
<big><b title="LATIN CAPITAL LETTER Q">Q</b></big></td>
  <td style="center">82
0x52
<big><b title="LATIN CAPITAL LETTER R">R</b></big></td>
  <td style="center">83
0x53
<big><b title="LATIN CAPITAL LETTER S">S</b></big></td>
  <td style="center">84
0x54
<big><b title="LATIN CAPITAL LETTER T">T</b></big></td>
  <td style="center">85
0x55
<big><b title="LATIN CAPITAL LETTER U">U</b></big></td>
  <td style="center">86
0x56
<big><b title="LATIN CAPITAL LETTER V">V</b></big></td>
  <td style="center">87
0x57
<big><b title="LATIN CAPITAL LETTER W">W</b></big></td>
  <td style="center">88
0x58
<big><b title="LATIN CAPITAL LETTER X">X</b></big></td>
  <td style="center">89
0x59
<big><b title="LATIN CAPITAL LETTER Y">Y</b></big></td>
  <td style="center">90
0x5a
<big><b title="LATIN CAPITAL LETTER Z">Z</b></big></td>
  <td style="center">91
0x5b
<big><b title="LEFT SQUARE BRACKET">[</b></big></td>
  <td style="center">92
0x5c
<big><b title="REVERSE SOLIDUS">\</b></big></td>
  <td style="center">93
0x5d
<big><b title="RIGHT SQUARE BRACKET">]</b></big></td>
  <td style="center">94
0x5e
<big><b title="CIRCUMFLEX ACCENT">^</b></big></td>
  <td style="center">95
0x5f
<big><b title="LOW LINE">_</b></big></td>
 </tr><tr>
  <td style="center">96
0x60
<big><b title="GRAVE ACCENT">`</b></big></td>
  <td style="center">97
0x61
<big><b title="LATIN SMALL LETTER A">a</b></big></td>
  <td style="center">98
0x62
<big><b title="LATIN SMALL LETTER B">b</b></big></td>
  <td style="center">99
0x63
<big><b title="LATIN SMALL LETTER C">c</b></big></td>
  <td style="center">100
0x64
<big><b title="LATIN SMALL LETTER D">d</b></big></td>
  <td style="center">101
0x65
<big><b title="LATIN SMALL LETTER E">e</b></big></td>
  <td style="center">102
0x66
<big><b title="LATIN SMALL LETTER F">f</b></big></td>
  <td style="center">103
0x67
<big><b title="LATIN SMALL LETTER G">g</b></big></td>
  <td style="center">104
0x68
<big><b title="LATIN SMALL LETTER H">h</b></big></td>
  <td style="center">105
0x69
<big><b title="LATIN SMALL LETTER I">i</b></big></td>
  <td style="center">106
0x6a
<big><b title="LATIN SMALL LETTER J">j</b></big></td>
  <td style="center">107
0x6b
<big><b title="LATIN SMALL LETTER K">k</b></big></td>
  <td style="center">108
0x6c
<big><b title="LATIN SMALL LETTER L">l</b></big></td>
  <td style="center">109
0x6d
<big><b title="LATIN SMALL LETTER M">m</b></big></td>
  <td style="center">110
0x6e
<big><b title="LATIN SMALL LETTER N">n</b></big></td>
  <td style="center">111
0x6f
<big><b title="LATIN SMALL LETTER O">o</b></big></td>
 </tr><tr>
  <td style="center">112
0x70
<big><b title="LATIN SMALL LETTER P">p</b></big></td>
  <td style="center">113
0x71
<big><b title="LATIN SMALL LETTER Q">q</b></big></td>
  <td style="center">114
0x72
<big><b title="LATIN SMALL LETTER R">r</b></big></td>
  <td style="center">115
0x73
<big><b title="LATIN SMALL LETTER S">s</b></big></td>
  <td style="center">116
0x74
<big><b title="LATIN SMALL LETTER T">t</b></big></td>
  <td style="center">117
0x75
<big><b title="LATIN SMALL LETTER U">u</b></big></td>
  <td style="center">118
0x76
<big><b title="LATIN SMALL LETTER V">v</b></big></td>
  <td style="center">119
0x77
<big><b title="LATIN SMALL LETTER W">w</b></big></td>
  <td style="center">120
0x78
<big><b title="LATIN SMALL LETTER X">x</b></big></td>
  <td style="center">121
0x79
<big><b title="LATIN SMALL LETTER Y">y</b></big></td>
  <td style="center">122
0x7a
<big><b title="LATIN SMALL LETTER Z">z</b></big></td>
  <td style="center">123
0x7b
<big><b title="LEFT CURLY BRACKET">{</b></big></td>
  <td style="center">124
0x7c
<big><b title="VERTICAL LINE">|</b></big></td>
  <td style="center">125
0x7d
<big><b title="RIGHT CURLY BRACKET">}</b></big></td>
  <td style="center">126
0x7e
<big><b title="TILDE">~</b></big></td>
  <td style="center">127
0x7f
<big><b title="SYMBOL FOR DELETE">␡</b></big></td>
 </tr>
</table>



### Plain text


Composed from generic abstractions:

```python
'''Plain text ASCII code table'''

from functools import reduce
from itertools import chain


# asciiTable :: String
def asciiTable():
    '''Table of ASCII codes arranged in 16 rows * 6 columns.'''
    return unlines(
        concat(c.ljust(12, ' ') for c in xs) for xs in (
            transpose(chunksOf(16)(
                [asciiEntry(n) for n in enumFromTo(32)(127)]
            ))
        )
    )


# asciiEntry :: Int -> String
def asciiEntry(n):
    '''Number, and name or character, for given point in ASCII code.'''
    k = asciiName(n)
    return k if '' == k else (
        concat([str(n).rjust(3, ' '), ' : ', k])
    )


# asciiName :: Int -> String
def asciiName(n):
    '''Name or character for given ASCII code.'''
    return '' if 32 > n or 127 < n else (
        'Spc' if 32 == n else (
            'Del' if 127 == n else chr(n)
        )
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test'''
    print(
        asciiTable()
    )


# GENERIC ABSTRACTIONS ------------------------------------

# chunksOf :: Int -> [a] -> [[a]]
def chunksOf(n):
    '''A series of lists of length n,
       subdividing the contents of xs.
       Where the length of xs is not evenly divible
       the final list will be shorter than n.'''
    return lambda xs: reduce(
        lambda a, i: a + [xs[i:n + i]],
        range(0, len(xs), n), []
    ) if 0 < n else []


# concat :: [[a]] -> [a]
# concat :: [String] -> String
def concat(xxs):
    '''The concatenation of all the elements in a list.'''
    xs = list(chain.from_iterable(xxs))
    unit = '' if isinstance(xs, str) else []
    return unit if not xs else (
        ''.join(xs) if isinstance(xs[0], str) else xs
    )


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# splitAt :: Int -> [a] -> ([a], [a])
def splitAt(n):
    '''A tuple pairing the prefix of length n
       with the rest of xs.'''
    return lambda xs: (xs[0:n], xs[n:])


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


# unlines :: [String] -> String
def unlines(xs):
    '''A single newline-delimited string derived
       from a list of strings.'''
    return '\n'.join(xs)


# MAIN ---
if __name__ == '__main__':
    main()
```

{{Out}}

```txt
 32 : Spc    48 : 0      64 : @      80 : P      96 : `     112 : p
 33 : !      49 : 1      65 : A      81 : Q      97 : a     113 : q
 34 : "      50 : 2      66 : B      82 : R      98 : b     114 : r
 35 : #      51 : 3      67 : C      83 : S      99 : c     115 : s
 36 : $      52 : 4      68 : D      84 : T     100 : d     116 : t
 37 : %      53 : 5      69 : E      85 : U     101 : e     117 : u
 38 : &      54 : 6      70 : F      86 : V     102 : f     118 : v
 39 : '      55 : 7      71 : G      87 : W     103 : g     119 : w
 40 : (      56 : 8      72 : H      88 : X     104 : h     120 : x
 41 : )      57 : 9      73 : I      89 : Y     105 : i     121 : y
 42 : *      58 : :      74 : J      90 : Z     106 : j     122 : z
 43 : +      59 : ;      75 : K      91 : [     107 : k     123 : {
 44 : ,      60 : <      76 : L      92 : \     108 : l     124 : |
 45 : -      61 : =      77 : M      93 : ]     109 : m     125 : }
 46 : .      62 : >      78 : N      94 : ^     110 : n     126 : ~
 47 : /      63 : ?      79 : O      95 : _     111 : o     127 : Del
```



## R


```rsplus
chr <- function(n) {
  rawToChar(as.raw(n))
}

idx <- 32
while (idx < 128) {
  for (i in 0:5) {
	num <- idx + i
	if (num<100) cat(" ")
        cat(num,": ")
	if (num == 32) { cat("Spc "); next }
        if (num == 127) { cat("Del "); next }
	cat(chr(num),"  ")
  }
  idx <- idx + 6
  cat("\n")
}
```

{{out}}

```txt
 32 : Spc  33 : !    34 : "    35 : #    36 : $    37 : %
 38 : &    39 : '    40 : (    41 : )    42 : *    43 : +
 44 : ,    45 : -    46 : .    47 : /    48 : 0    49 : 1
 50 : 2    51 : 3    52 : 4    53 : 5    54 : 6    55 : 7
 56 : 8    57 : 9    58 : :    59 : ;    60 : <    61 : =
 62 : >    63 : ?    64 : @    65 : A    66 : B    67 : C
 68 : D    69 : E    70 : F    71 : G    72 : H    73 : I
 74 : J    75 : K    76 : L    77 : M    78 : N    79 : O
 80 : P    81 : Q    82 : R    83 : S    84 : T    85 : U
 86 : V    87 : W    88 : X    89 : Y    90 : Z    91 : [
 92 : \    93 : ]    94 : ^    95 : _    96 : `    97 : a
 98 : b    99 : c   100 : d   101 : e   102 : f   103 : g
104 : h   105 : i   106 : j   107 : k   108 : l   109 : m
110 : n   111 : o   112 : p   113 : q   114 : r   115 : s
116 : t   117 : u   118 : v   119 : w   120 : x   121 : y
122 : z   123 : {   124 : |   125 : }   126 : ~   127 : Del
```



## Racket



```racket
#lang racket

(for ([i (in-range 16)])
  (for ([j (in-range 6)])
    (define n (+ 32 (* j 16) i))
    (printf "~a : ~a"
            (~a n #:align 'right #:min-width 3)
            (~a (match n
                  [32 "SPC"]
                  [127 "DEL"]
                  [_ (integer->char n)]) #:min-width 5)))
  (newline))
```


{{out}}

```txt
 32 : SPC   48 : 0     64 : @     80 : P     96 : `    112 : p
 33 : !     49 : 1     65 : A     81 : Q     97 : a    113 : q
 34 : "     50 : 2     66 : B     82 : R     98 : b    114 : r
 35 : #     51 : 3     67 : C     83 : S     99 : c    115 : s
 36 : $     52 : 4     68 : D     84 : T    100 : d    116 : t
 37 : %     53 : 5     69 : E     85 : U    101 : e    117 : u
 38 : &     54 : 6     70 : F     86 : V    102 : f    118 : v
 39 : '     55 : 7     71 : G     87 : W    103 : g    119 : w
 40 : (     56 : 8     72 : H     88 : X    104 : h    120 : x
 41 : )     57 : 9     73 : I     89 : Y    105 : i    121 : y
 42 : *     58 : :     74 : J     90 : Z    106 : j    122 : z
 43 : +     59 : ;     75 : K     91 : [    107 : k    123 : {
 44 : ,     60 : <     76 : L     92 : \    108 : l    124 : |
 45 : -     61 : =     77 : M     93 : ]    109 : m    125 : }
 46 : .     62 : >     78 : N     94 : ^    110 : n    126 : ~
 47 : /     63 : ?     79 : O     95 : _    111 : o    127 : DEL
```



## REXX

Note that some REXX interpreters can't display
the   <big> '1b'x </big>   ('''esc''')   character glyph properly,   so a special check was

made for those two REXXes to not show that glyph.

A fair amount of code was added to show all possible characters (glyphs),   with
special attention paid to:
:::*   presenting an index   (top and bottom;   left and right).
:::*   using a grid instead of using blanks for visual fidelity.
:::*   using a two─tired type of grid   (with single─   and   double─lined cells).
:::*   preserving indentation and other whitespace.
:::*   showing the function of some characters   (those that have a  ''lower''   value than a blank).
:::*   showing the name of a blank   (as '''bla''').
:::*   the suppression of displaying particular glyphs by REXX that are preempted by the OS.
:::*   adding homage to the adage of:   ''anything worth doing is worth doing well''.

```rexx
/*REXX program displays an  ASCII  table of characters  (within a  16x16  indexed grid).*/
parse upper version !ver .                       /*some REXXes can't display '1b'x glyph*/
!pcRexx= 'REXX/PERSONAL'==!ver | "REXX/PC"==!ver /*is this  PC/REXX  or  REXX/Personal? */
          func= '  nul soh stx etx eot enq ack bel  bs tab  lf  vt  ff  cr  so  si  '  ||,
                "  dle dc1 dc2 dc3 dc4 nak syn etb can  em eof esc  fs  gs  rs  us  "
@.=
@.1= "x'07'    x'08'       x'09'     x'0a'      x'0d'      x'1a'      x'1b'     x'20'"
@.2= "bel      b/s         tab       l/f        c/r        eof        esc       bla"
@.3= "bell     backspace   tabchar   linefeed   carriage   end-of-    escape    blank"
@.4= "                                          return     file"
@.5= copies('≈', 79)
            do a=1  for 8;   say @.a             /*display header info  (abbreviations).*/
            end   /*i*/                          /*also included are three blank lines. */
b= ' ';         hdr= left(b, 7)                  /*prepend blanks to HDR  (indentation).*/
call xhdr                                        /*construct a  top  index for the grid.*/
call grid '╔',  "╤",  '╗',  "═══"                /*construct & display bottom of a cell.*/
iidx= left(b, length(hdr) - 4 )                  /*the length of the indentation of idx.*/
cant= copies('═', 3)                             /*can't show a character with this REXX*/
                                                 /* [↓]  construct a sixteen-row grid.  */
   do j=0  by 16  for 16;  idx= left(d2x(j),1,2) /*prepend an index literal for the grid*/
   _= iidx idx b;           _h= iidx "   "       /*an index and indent; without an index*/
   sep= '║'                                      /*assign a character to cell separator.*/
             do #=j  to j+15;               chr= center( d2c(#), 3)   /*true char glyph.*/
             if #>6 & #<11  |  #==13   then chr= cant         /*can't show these glyphs.*/
   /*esc*/   if #==27 then if !pcRexx  then chr= cant         /*  "     "  this  glyph. */
                                       else chr= center( d2c(#), 3)   /*true char glyph.*/
             if # <32 then _h= _h || sep || right(word(func, #+1), 3) /*show a function.*/
             if #==32 then chr= 'bla'            /*spell out (within 3 chars) a "BLAnk".*/
             if # >31 then _h=                   /*Above a blank?  Then nullify 3rd line*/
             _= _ || sep || chr;     sep= '│'    /*append grid cell; use a new sep char.*/
             end   /*#*/
   if _h\==''  then say _h"║ "                   /*append the  last grid cell character.*/
   say _'║ '   idx                               /*append an   index   to the grid line.*/
   if j\==240  then call grid '╟',"┼",'╢',"───"  /*construct & display most cell bottoms*/
   end   /*j*/

call grid '╚',  "╧",  '╝',  "═══"                /*construct & display last cell bottom.*/
call xhdr                                        /*construct a bottom index for the grid*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
xhdr: say; _= hdr;  sep= b;   do k=0  for 16; _=_||b d2x(k)b;  end;    say _; say;  return
grid: arg $1,$2,$3,$4; _=hdr; do 16;  _=_ || $1 || $4;  $1= $2;  end;  say _ || $3; return
```

{{out|output|text=  showing a   ''horizontal''   formatted grid (table):}}

(Code page '''437''' was used for the example below   using DOS under Microsoft Windows.)

```txt

x'07'    x'08'       x'09'     x'0a'      x'0d'      x'1a'      x'1b'     x'20'
bel      b/s         tab       l/f        c/r        eof        esc       bla
bell     backspace   tabchar   linefeed   carriage   end-of-    escape    blank
                                          return     file
≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈



         0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F

       ╔═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╤═══╗
       ║nul│soh│stx│etx│eot│enq│ack│bel│ bs│tab│ lf│ vt│ ff│ cr│ so│ si║
    0  ║   │ ☺ │ ☻ │ ♥ │ ♦ │ ♣ │ ♠ │═══│═══│═══│═══│ ♂ │ ♀ │═══│ ♫ │ ☼ ║  0
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
       ║dle│dc1│dc2│dc3│dc4│nak│syn│etb│can│ em│eof│esc│ fs│ gs│ rs│ us║
    1  ║ ► │ ◄ │ ↕ │ ‼ │ ¶ │ § │ ▬ │ ↨ │ ↑ │ ↓ │ → │ ← │ ∟ │ ↔ │ ▲ │ ▼ ║  1
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    2  ║bla│ ! │ " │ # │ $ │ % │ & │ ' │ ( │ ) │ * │ + │ , │ - │ . │ / ║  2
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    3  ║ 0 │ 1 │ 2 │ 3 │ 4 │ 5 │ 6 │ 7 │ 8 │ 9 │ : │ ; │ < │ = │ > │ ? ║  3
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    4  ║ @ │ A │ B │ C │ D │ E │ F │ G │ H │ I │ J │ K │ L │ M │ N │ O ║  4
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    5  ║ P │ Q │ R │ S │ T │ U │ V │ W │ X │ Y │ Z │ [ │ \ │ ] │ ^ │ _ ║  5
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    6  ║ ` │ a │ b │ c │ d │ e │ f │ g │ h │ i │ j │ k │ l │ m │ n │ o ║  6
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    7  ║ p │ q │ r │ s │ t │ u │ v │ w │ x │ y │ z │ { │ | │ } │ ~ │ ⌂ ║  7
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    8  ║ Ç │ ü │ é │ â │ ä │ à │ å │ ç │ ê │ ë │ è │ ï │ î │ ì │ Ä │ Å ║  8
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    9  ║ É │ æ │ Æ │ ô │ ö │ ò │ û │ ù │ ÿ │ Ö │ Ü │ ¢ │ £ │ ¥ │ ₧ │ ƒ ║  9
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    A  ║ á │ í │ ó │ ú │ ñ │ Ñ │ ª │ º │ ¿ │ ⌐ │ ¬ │ ½ │ ¼ │ ¡ │ « │ » ║  A
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    B  ║ ░ │ ▒ │ ▓ │ │ │ ┤ │ ╡ │ ╢ │ ╖ │ ╕ │ ╣ │ ║ │ ╗ │ ╝ │ ╜ │ ╛ │ ┐ ║  B
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    C  ║ └ │ ┴ │ ┬ │ ├ │ ─ │ ┼ │ ╞ │ ╟ │ ╚ │ ╔ │ ╩ │ ╦ │ ╠ │ ═ │ ╬ │ ╧ ║  C
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    D  ║ ╨ │ ╤ │ ╥ │ ╙ │ ╘ │ ╒ │ ╓ │ ╫ │ ╪ │ ┘ │ ┌ │ █ │ ▄ │ ▌ │ ▐ │ ▀ ║  D
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    E  ║ α │ ß │ Γ │ π │ Σ │ σ │ µ │ τ │ Φ │ Θ │ Ω │ δ │ ∞ │ φ │ ε │ ∩ ║  E
       ╟───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───┼───╢
    F  ║ ≡ │ ± │ ≥ │ ≤ │ ⌠ │ ⌡ │ ÷ │ ≈ │ ° │ ∙ │ · │ √ │ ⁿ │ ² │ ■ │   ║  F
       ╚═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╧═══╝

         0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F

```



## Ruby


```ruby
chars = (32..127).map do |ord|
  k = case ord
    when 32  then "␠"
    when 127 then "␡"
    else ord.chr
  end
  "#{ord.to_s.ljust(3)}: #{k}"
end

chars.each_slice(chars.size/6).to_a.transpose.each{|s| puts s.join("  ")}
```

{{out}}

```txt
32 : ␠  48 : 0  64 : @  80 : P  96 : `  112: p
33 : !  49 : 1  65 : A  81 : Q  97 : a  113: q
34 : "  50 : 2  66 : B  82 : R  98 : b  114: r
35 : #  51 : 3  67 : C  83 : S  99 : c  115: s
36 : $  52 : 4  68 : D  84 : T  100: d  116: t
37 : %  53 : 5  69 : E  85 : U  101: e  117: u
38 : &  54 : 6  70 : F  86 : V  102: f  118: v
39 : '  55 : 7  71 : G  87 : W  103: g  119: w
40 : (  56 : 8  72 : H  88 : X  104: h  120: x
41 : )  57 : 9  73 : I  89 : Y  105: i  121: y
42 : *  58 : :  74 : J  90 : Z  106: j  122: z
43 : +  59 : ;  75 : K  91 : [  107: k  123: {
44 : ,  60 : <  76 : L  92 : \  108: l  124: |
45 : -  61 : =  77 : M  93 : ]  109: m  125: }
46 : .  62 : >  78 : N  94 : ^  110: n  126: ~
47 : /  63 : ?  79 : O  95 : _  111: o  127: ␡

```



## Scala

{{Out}}Best seen in running your browser either by [https://scalafiddle.io/sf/ouiyD9x/0 ScalaFiddle (your local ES aka JavaScript execution, non JVM)] or [https://scastie.scala-lang.org/OD7rBCSMQgKSyKKcGB24cg Scastie (remote JVM)].
{{works with|Scala|2.13}}

```Scala
object AsciiTable extends App {
  val (strtCharVal, lastCharVal, nColumns) = (' '.toByte, '\u007F'.toByte, 6)
  require(nColumns % 2 == 0, "Number of columns must be even.")

  val nChars = lastCharVal - strtCharVal + 1
  val step = nChars / nColumns
  val threshold = strtCharVal + (nColumns - 1) * step

  def indexGen(start: Byte): LazyList[Byte] =
    start #:: indexGen(
      (if (start >= threshold) strtCharVal + start % threshold + 1 else start + step).toByte
    )

  def k(j: Byte): Char = j match {
    case `strtCharVal` => '\u2420'
    case 0x7F => '\u2421'
    case _ => j.toChar
  }

  indexGen(strtCharVal)
    .take(nChars)
    .sliding(nColumns, nColumns)
    .map(_.map(byte => f"$byte%3d : ${k(byte)}"))
    .foreach(line => println(line.mkString("  ")))
}
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var integer: row is 0;
    var integer: column is 0;
    var integer: number is 0;
  begin
    for row range 0 to 15 do
      for column range 0 to 5 do
        number := 32 + 16 * column + row;
        write(number lpad 3 <& " : ");
        case number of
          when {32}:  write("Spc  ");
          when {127}: write("Del  ");
          otherwise:  write(chr(number) <& "    ");
        end case;
      end for;
      writeln;
    end for;
  end func;
```


{{out}}

```txt
 32 : Spc   48 : 0     64 : @     80 : P     96 : `    112 : p
 33 : !     49 : 1     65 : A     81 : Q     97 : a    113 : q
 34 : "     50 : 2     66 : B     82 : R     98 : b    114 : r
 35 : #     51 : 3     67 : C     83 : S     99 : c    115 : s
 36 : $     52 : 4     68 : D     84 : T    100 : d    116 : t
 37 : %     53 : 5     69 : E     85 : U    101 : e    117 : u
 38 : &     54 : 6     70 : F     86 : V    102 : f    118 : v
 39 : '     55 : 7     71 : G     87 : W    103 : g    119 : w
 40 : (     56 : 8     72 : H     88 : X    104 : h    120 : x
 41 : )     57 : 9     73 : I     89 : Y    105 : i    121 : y
 42 : *     58 : :     74 : J     90 : Z    106 : j    122 : z
 43 : +     59 : ;     75 : K     91 : [    107 : k    123 : {
 44 : ,     60 : <     76 : L     92 : \    108 : l    124 : |
 45 : -     61 : =     77 : M     93 : ]    109 : m    125 : }
 46 : .     62 : >     78 : N     94 : ^    110 : n    126 : ~
 47 : /     63 : ?     79 : O     95 : _    111 : o    127 : Del
```



## Spin

{{works with|BST/BSTC}}
{{works with|FastSpin/FlexSpin}}
{{works with|HomeSpun}}
{{works with|OpenSpin}}

```spin
con
  _clkmode = xtal1+pll16x
  _clkfreq = 80_000_000

obj
  ser : "FullDuplexSerial"

pub main | i, j

  ser.start(31, 30, 0, 115200)

  repeat i from 0 to 15
    repeat j from i + 32 to 127 step 16
      if j < 100
        ser.tx(32)
      ser.dec(j)
      ser.str(string(": "))
      case j
        32:
          ser.str(string("SPC"))
        127:
          ser.str(string("DEL"))
        other:
          ser.tx(j)
          ser.str(string("  "))
      ser.str(string("  "))
    ser.str(string(13, 10))

  waitcnt(_clkfreq + cnt)
  ser.stop
```

{{out}}

```txt
 32: SPC   48: 0     64: @     80: P     96: `    112: p
 33: !     49: 1     65: A     81: Q     97: a    113: q
 34: "     50: 2     66: B     82: R     98: b    114: r
 35: #     51: 3     67: C     83: S     99: c    115: s
 36: $     52: 4     68: D     84: T    100: d    116: t
 37: %     53: 5     69: E     85: U    101: e    117: u
 38: &     54: 6     70: F     86: V    102: f    118: v
 39: '     55: 7     71: G     87: W    103: g    119: w
 40: (     56: 8     72: H     88: X    104: h    120: x
 41: )     57: 9     73: I     89: Y    105: i    121: y
 42: *     58: :     74: J     90: Z    106: j    122: z
 43: +     59: ;     75: K     91: [    107: k    123: {
 44: ,     60: <     76: L     92: \    108: l    124: |
 45: -     61: =     77: M     93: ]    109: m    125: }
 46: .     62: >     78: N     94: ^    110: n    126: ~
 47: /     63: ?     79: O     95: _    111: o    127: DEL
```



## Ring


```ring

# Project : Show Ascii table

load "guilib.ring"
load "stdlib.ring"

decarr = newlist(16,6)
ascarr = newlist(16,6)

new qapp
        {
        win1 = new qwidget() {
                  setwindowtitle("Show Ascii table")
                  setgeometry(100,100,800,600)
                  for n = 1 to 16
                       for m = 1 to 6
                            decarr[n][m] = new qpushbutton(win1) {
                                                  x = 150+m*60
                                                  y = 30 + n*30
                                                  ind = string((m-1)*16+n+31)
                                                  setgeometry(x,y,30,30)
                                                  settext(ind)
                                                  }
                       next
                  next
                  for n = 1 to 16
                       for m = 1 to 6
                            ascarr[n][m] = new qpushbutton(win1) {
                                                  x = 180+m*60
                                                  y = 30 + n*30
                                                  ind = (m-1)*16+n+31
                                                  setgeometry(x,y,30,30)
                                                  if ind = 32
                                                     settext("Spc")
                                                     loop
                                                  ok
                                                  if ind = 127
                                                     settext("Del")
                                                     loop
                                                  ok
                                                  settext(char(ind))
                                                  }
                       next
                  next
                  show()
        }
        exec()
        }
```

Output:

[http://kepkezelo.com/images/anuscoqy8ejyr5bnhvnu.jpg Show Ascii table]

=={{Header|Rust}}==
{{trans|Go}}

```Rust
fn main() {
    for i in 0u8..16 {
        for j in ((32+i)..128).step_by(16) {
            let k = (j as char).to_string();
            print!("{:3} : {:<3}   ", j, match j {
                32 => "Spc",
                127 => "Del",
                _ => &k,
            });
        }
        println!();
    }
}
```


{{out}}

```txt
 32 : Spc    48 : 0      64 : @      80 : P      96 : `     112 : p
 33 : !      49 : 1      65 : A      81 : Q      97 : a     113 : q
 34 : "      50 : 2      66 : B      82 : R      98 : b     114 : r
 35 : #      51 : 3      67 : C      83 : S      99 : c     115 : s
 36 : $      52 : 4      68 : D      84 : T     100 : d     116 : t
 37 : %      53 : 5      69 : E      85 : U     101 : e     117 : u
 38 : &      54 : 6      70 : F      86 : V     102 : f     118 : v
 39 : '      55 : 7      71 : G      87 : W     103 : g     119 : w
 40 : (      56 : 8      72 : H      88 : X     104 : h     120 : x
 41 : )      57 : 9      73 : I      89 : Y     105 : i     121 : y
 42 : *      58 : :      74 : J      90 : Z     106 : j     122 : z
 43 : +      59 : ;      75 : K      91 : [     107 : k     123 : {
 44 : ,      60 : <      76 : L      92 : \     108 : l     124 : |
 45 : -      61 : =      77 : M      93 : ]     109 : m     125 : }
 46 : .      62 : >      78 : N      94 : ^     110 : n     126 : ~
 47 : /      63 : ?      79 : O      95 : _     111 : o     127 : Del
```



## VBA

{{incorrect|VBA|Output isn't consistent with the task's requirements.}}

```VB

Public Sub ascii()
    Dim s As String
    For i = 0 To 16
        For j = 32 + i To 127 Step 16
            Select Case j
                Case 32: s = "Spc"
                Case 127: s = "Del"
                Case Else: s = Chr(j)
            End Select
            Debug.Print Tab(10 * (j - 32 - i) / 16); Spc(3 - Len(CStr(j))); j & ": " & s;
        Next j
        Debug.Print vbCrLf
    Next i
End Sub
```

{{out}}

```txt
 32: Spc  48: 0     64: @     80: P     96: `    112: p
 33: !    49: 1     65: A     81: Q     97: a    113: q
 34: "    50: 2     66: B     82: R     98: b    114: r
 35: #    51: 3     67: C     83: S     99: c    115: s
 36: $    52: 4     68: D     84: T    100: d    116: t
 37: %    53: 5     69: E     85: U    101: e    117: u
 38: &    54: 6     70: F     86: V    102: f    118: v
 39: '    55: 7     71: G     87: W    103: g    119: w
 40: (    56: 8     72: H     88: X    104: h    120: x
 41: )    57: 9     73: I     89: Y    105: i    121: y
 42: *    58: :     74: J     90: Z    106: j    122: z
 43: +    59: ;     75: K     91: [    107: k    123: {
 44: ,    60: <     76: L     92: \    108: l    124: |
 45: -    61: =     77: M     93: ]    109: m    125: }
 46: .    62: >     78: N     94: ^    110: n    126: ~
 47: /    63: ?     79: O     95: _    111: o    127: Del
 48: 0    64: @     80: P     96: `    112: p
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.Console
Imports System.Linq.Enumerable

Module Program
    Sub Main()
        Dim Text = Function(index As Integer) If(index = 32, "Sp", If(index = 127, "Del", ChrW(index) & ""))

        Dim start = 32
        Do
            WriteLine(String.Concat(Range(0, 6).Select(Function(i) $"{start + 16 * i, -3} : {Text(start + 16 * i), -6}")))
            start += 1
        Loop While start + 16 * 5 < 128
    End Sub
End Module
```

{{out}}

```txt
32  : Sp    48  : 0     64  : @     80  : P     96  : `     112 : p
33  : !     49  : 1     65  : A     81  : Q     97  : a     113 : q
34  : "     50  : 2     66  : B     82  : R     98  : b     114 : r
35  : #     51  : 3     67  : C     83  : S     99  : c     115 : s
36  : $     52  : 4     68  : D     84  : T     100 : d     116 : t
37  : %     53  : 5     69  : E     85  : U     101 : e     117 : u
38  : &     54  : 6     70  : F     86  : V     102 : f     118 : v
39  : '     55  : 7     71  : G     87  : W     103 : g     119 : w
40  : (     56  : 8     72  : H     88  : X     104 : h     120 : x
41  : )     57  : 9     73  : I     89  : Y     105 : i     121 : y
42  : *     58  : :     74  : J     90  : Z     106 : j     122 : z
43  : +     59  : ;     75  : K     91  : [     107 : k     123 : {
44  : ,     60  : <     76  : L     92  : \     108 : l     124 : |
45  : -     61  : =     77  : M     93  : ]     109 : m     125 : }
46  : .     62  : >     78  : N     94  : ^     110 : n     126 : ~
47  : /     63  : ?     79  : O     95  : _     111 : o     127 : Del
```



## zkl


```zkl
const width=9;
println("  ",[0..width].pump(String,"%4d".fmt));
[30..127].pump("".println,T(Void.Read,width,False), // don't fail on short lines
    fcn(a,b,c){
       String("%3d: ".fmt(a),
       vm.arglist.pump(String,"toChar", // parameters (ints) to list to text
          T("replace","\x1e",""),T("replace","\x1f",""),  // 30,31
          T("replace"," ","spc"),T("replace","\x7f","del"), "%-4s".fmt)
       )
    })
```

{{out}}

```txt
     0   1   2   3   4   5   6   7   8   9
 30:         spc !   "   #   $   %   &   '
 40: (   )   *   +   ,   -   .   /   0   1
 50: 2   3   4   5   6   7   8   9   :   ;
 60: <   =   >   ?   @   A   B   C   D   E
 70: F   G   H   I   J   K   L   M   N   O
 80: P   Q   R   S   T   U   V   W   X   Y
 90: Z   [   \   ]   ^   _   `   a   b   c
100: d   e   f   g   h   i   j   k   l   m
110: n   o   p   q   r   s   t   u   v   w
120: x   y   z   {   |   }   ~   del
```

