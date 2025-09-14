+++
title = "Sierpinski carpet"
description = ""
date = 2019-07-12T18:19:28Z
aliases = []
[extra]
id = 2737
[taxonomies]
categories = ["task", "Fractals"]
tags = []
languages = [
  "ada",
  "algol_68",
  "applescript",
  "applesoft_basic",
  "asymptote",
  "autohotkey",
  "awk",
  "bbc_basic",
  "befunge",
  "c",
  "c_plus_plus",
  "c_sharp",
  "clojure",
  "common_lisp",
  "crystal",
  "d",
  "dwscript",
  "e",
  "elixir",
  "erlang",
  "erre",
  "euphoria",
  "fan",
  "forth",
  "fortran",
  "gnuplot",
  "go",
  "groovy",
  "haskell",
  "io",
  "j",
  "java",
  "javascript",
  "kotlin",
  "jq",
  "julia",
  "liberty_basic",
  "mathematica",
  "matlab",
  "netrexx",
  "nim",
  "objeck",
  "ocaml",
  "oforth",
  "oz",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "pl_i",
  "postscript",
  "powershell",
  "purebasic",
  "python",
  "r",
  "rexx",
  "racket",
  "ring",
  "ruby",
  "rust",
  "scala",
  "scheme",
  "seed7",
  "sidef",
  "sinclair_zx81_basic",
  "swift",
  "tcl",
  "ubasic_4th",
  "unix_shell",
  "ursala",
  "vba",
  "vbscript",
  "x86_assembly",
  "xpl0",
  "yabasic",
  "zkl",
]
+++

## Task

Produce a graphical or ASCII-art representation of a [[wp:Sierpinski carpet|Sierpinski carpet]] of order   '''N'''.


For example, the Sierpinski carpet of order   '''3'''   should look like this:

```txt

###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
#########         #########
# ## ## #         # ## ## #
#########         #########
###   ###         ###   ###
# #   # #         # #   # #
###   ###         ###   ###
#########         #########
# ## ## #         # ## ## #
#########         #########
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################

```


The use of the   #   character is not rigidly required for ASCII art.

The important requirement is the placement of whitespace and non-whitespace characters.


## Related tasks

*   [[Sierpinski triangle]]





## Ada


```ada
with Ada.Text_Io; use Ada.Text_Io;

procedure Sierpinski_Carpet is
   subtype Index_Type is Integer range 1..81;
   type Pattern_Array is array(Index_Type range <>, Index_Type range <>) of Boolean;
   Pattern : Pattern_Array(1..81,1..81) := (Others =>(others => true));
   procedure Clear_Center(P : in out Pattern_Array; X1 : Index_Type; X2 : Index_Type;
         Y1 : Index_Type; Y2 : Index_Type) is
      Xfirst : Index_Type;
      Xlast  : Index_Type;
      Yfirst : Index_Type;
      Ylast  : Index_Type;
      Diff   : Integer;
   begin
      Xfirst :=(X2 - X1 + 1) / 3 + X1;
      Diff := Xfirst - X1;
      Xlast  := Xfirst + Diff;
      Yfirst := (Y2 - Y1) / 3 + Y1;
      YLast  := YFirst + Diff;

      for I in XFirst..XLast loop
         for J in YFirst..YLast loop
            P(I, J) := False;
         end loop;
      end loop;
   end Clear_Center;

   procedure Print(P : Pattern_Array) is
   begin
      for I in P'range(1) loop
         for J in P'range(2) loop
            if P(I,J) then
               Put('*');
            else
               Put(' ');
            end if;
         end loop;
         New_Line;
      end loop;
   end Print;

   procedure Divide_Square(P : in out Pattern_Array; Order : Positive) is
      Factor : Natural := 0;
      X1, X2 : Index_Type;
      Y1, Y2  : Index_Type;
      Division : Index_Type;
      Num_Sections : Index_Type;
   begin
      while Factor < Order loop
         Num_Sections := 3**Factor;
         Factor := Factor + 1;
         X1  := P'First;
         Division   := P'Last / Num_Sections;
         X2 := Division;
         Y1 := X1;
         Y2 := X2;
         loop
            loop
               Clear_Center(P, X1, X2, Y1, Y2);
               exit when X2 = P'Last;
               X1 := X2;
               X2 := X2 + Division;
            end loop;
            exit when Y2 = P'Last;
            Y1 := Y2;
            Y2 := Y2 + Division;
            X1 := P'First;
            X2 := Division;
         end loop;
      end loop;
   end Divide_Square;

begin
   Divide_Square(Pattern, 3);
   Print(Pattern);
end Sierpinski_Carpet;
```



## ALGOL 68

```algol68
PROC in carpet = (INT in x, in y)BOOL: (
    INT x := in x, y := in y;
    BOOL out;
    DO
        IF x = 0 OR y = 0 THEN
            out := TRUE; GO TO stop iteration
        ELIF x MOD 3 = 1 AND y MOD 3 = 1 THEN
            out := FALSE; GO TO stop iteration
        FI;

        x %:= 3;
        y %:= 3
    OD;
    stop iteration: out
);

PROC carpet = (INT n)VOID:
    FOR i TO 3 ** n DO
        FOR j TO 3 ** n DO
            IF in carpet(i-1, j-1) THEN
                print("* ")
            ELSE
                print("  ")
            FI
        OD;
        print(new line)
    OD;

carpet(3)
```




## AppleScript


(ES5 Functional version)


```AppleScript
-- CARPET MODEL --------------------------------------------------------------

-- sierpinskiCarpet :: Int -> [[Bool]]
on sierpinskiCarpet(n)

    -- rowStates :: Int -> [Bool]
    script rowStates
        on |λ|(x, _, xs)

            -- cellState :: Int -> Bool
            script cellState

                -- inCarpet :: Int -> Int -> Bool
                on inCarpet(x, y)
                    if (x = 0 or y = 0) then
                        true
                    else
                        not ((x mod 3 = 1) and ¬
                            (y mod 3 = 1)) and ¬
                            inCarpet(x div 3, y div 3)
                    end if
                end inCarpet

                on |λ|(y)
                    inCarpet(x, y)
                end |λ|
            end script

            map(cellState, xs)
        end |λ|
    end script

    map(rowStates, enumFromTo(0, (3 ^ n) - 1))
end sierpinskiCarpet


-- TEST ----------------------------------------------------------------------
on run
    -- Carpets of orders 1, 2, 3

    set strCarpets to ¬
        intercalate(linefeed & linefeed, ¬
            map(showCarpet, enumFromTo(1, 3)))

    set the clipboard to strCarpets

    return strCarpets
end run

-- CARPET DISPLAY ------------------------------------------------------------

-- showCarpet :: Int -> String
on showCarpet(n)

    -- showRow :: [Bool] -> String
    script showRow
        -- showBool :: Bool -> String
        script showBool
            on |λ|(bool)
                if bool then
                    character id 9608
                else
                    " "
                end if
            end |λ|
        end script

        on |λ|(xs)
            intercalate("", map(my showBool, xs))
        end |λ|
    end script

    intercalate(linefeed, map(showRow, sierpinskiCarpet(n)))
end showCarpet


-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- intercalate :: Text -> [Text] -> Text
on intercalate(strText, lstText)
    set {dlm, my text item delimiters} to {my text item delimiters, strText}
    set strJoined to lstText as text
    set my text item delimiters to dlm
    return strJoined
end intercalate

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
```

```txt
███
█ █
███

█████████
█ ██ ██ █
█████████
███   ███
█ █   █ █
███   ███
█████████
█ ██ ██ █
█████████

███████████████████████████
█ ██ ██ ██ ██ ██ ██ ██ ██ █
███████████████████████████
███   ██████   ██████   ███
█ █   █ ██ █   █ ██ █   █ █
███   ██████   ██████   ███
███████████████████████████
█ ██ ██ ██ ██ ██ ██ ██ ██ █
███████████████████████████
█████████         █████████
█ ██ ██ █         █ ██ ██ █
█████████         █████████
███   ███         ███   ███
█ █   █ █         █ █   █ █
███   ███         ███   ███
█████████         █████████
█ ██ ██ █         █ ██ ██ █
█████████         █████████
███████████████████████████
█ ██ ██ ██ ██ ██ ██ ██ ██ █
███████████████████████████
███   ██████   ██████   ███
█ █   █ ██ █   █ ██ █   █ █
███   ██████   ██████   ███
███████████████████████████
█ ██ ██ ██ ██ ██ ██ ██ ██ █
███████████████████████████
```



Or, defining the Sierpinski carpet weave more simply in terms of generic abstractions like '''zipWith''' and '''concatMap''':


```applescript
-- weave :: [String] -> [String]
on weave(xs)
    script thread
        property f : zipWith(my append)
        on |λ|(x)
            f's |λ|(f's |λ|(xs, x), xs)
        end |λ|
    end script

    script blank
        on |λ|(x)
            replicate(length of x, space)
        end |λ|
    end script

    concatMap(thread, {xs, map(blank, xs), xs})
end weave



-- TEST ---------------------------------------------------
on run
    -- sierpinksi :: Int -> String
    script sierpinski
        on |λ|(n)
            unlines(item n of take(n, ¬
                iterate(weave, {character id 9608})))
        end |λ|
    end script

    sierpinski's |λ|(3)
end run


-- GENERIC ABSTRACTIONS -----------------------------------

-- Append two lists.
-- append (++) :: [a] -> [a] -> [a]
-- append (++) :: String -> String -> String
on append(xs, ys)
    xs & ys
end append

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    set lng to length of xs
    set acc to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set acc to acc & |λ|(item i of xs, i, xs)
        end repeat
    end tell
    return acc
end concatMap

-- iterate :: (a -> a) -> a -> Gen [a]
on iterate(f, x)
    script
        property v : missing value
        property g : mReturn(f)'s |λ|
        on |λ|()
            if missing value is v then
                set v to x
            else
                set v to g(v)
            end if
            return v
        end |λ|
    end script
end iterate


-- length :: [a] -> Int
on |length|(xs)
    set c to class of xs
    if list is c or string is c then
        length of xs
    else
        (2 ^ 29 - 1) -- (maxInt - simple proxy for non-finite)
    end if
end |length|

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

-- min :: Ord a => a -> a -> a
on min(x, y)
    if y < x then
        y
    else
        x
    end if
end min

-- replicate :: Int -> String -> String
on replicate(n, s)
    set out to ""
    if n < 1 then return out
    set dbl to s

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicate

-- take :: Int -> [a] -> [a]
-- take :: Int -> String -> String
on take(n, xs)
    set c to class of xs
    if list is c then
        if 0 < n then
            items 1 thru min(n, length of xs) of xs
        else
            {}
        end if
    else if string is c then
        if 0 < n then
            text 1 thru min(n, length of xs) of xs
        else
            ""
        end if
    else if script is c then
        set ys to {}
        repeat with i from 1 to n
            set v to xs's |λ|()
            if missing value is v then
                return ys
            else
                set end of ys to v
            end if
        end repeat
        return ys
    else
        missing value
    end if
end take

-- unlines :: [String] -> String
on unlines(xs)
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines

-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
on zipWith(f)
    script
        on |λ|(xs, ys)
            set lng to min(|length|(xs), |length|(ys))
            if 1 > lng then return {}
            set xs_ to take(lng, xs) -- Allow for non-finite
            set ys_ to take(lng, ys) -- generators like cycle etc
            set lst to {}
            tell mReturn(f)
                repeat with i from 1 to lng
                    set end of lst to |λ|(item i of xs_, item i of ys_)
                end repeat
                return lst
            end tell
        end |λ|
    end script
end zipWith
```

```txt
█████████
█ ██ ██ █
█████████
███   ███
█ █   █ █
███   ███
█████████
█ ██ ██ █
█████████
```



## Applesoft BASIC


```ApplesoftBasic
 100 HGR
 110 POKE 49234,0
 120 DEF  FN M(X) = X -  INT (D * 3) *  INT (X /  INT (D * 3))
 130 DE = 4
 140 DI = 3 ^ DE * 3
 150 FOR I = 0 TO DI - 1
 160      FOR J = 0 TO DI - 1
 170          FOR D = DI / 3 TO 0 STEP 0
 180              IF  INT ( FN M(I) / D) = 1 AND  INT ( FN M(J) / D) = 1 THEN 200BREAK
 190              D =  INT (D / 3): NEXT D
 200          HCOLOR= 3 * (D = 0)
 210          HPLOT J,I
 220      NEXT J
 230 NEXT I
```



## Asymptote


```asymptote
path across(path p, real node) {
    return
        point(p, node + 1/3) + point(p, node - 1/3) - point(p, node);
}

path corner_subquad(path p, real node) {
    return
        point(p, node) --
        point(p, node + 1/3) --
        across(p, node) --
        point(p, node - 1/3) --
        cycle;
}

path noncorner_subquad(path p, real node1, real node2) {
    return
        point(p, node1 + 1/3) --
        across(p, node1) --
        across(p, node2) --
        point(p, node2 - 1/3) --
        cycle;
}

void carpet(path p, int order) {
    if (order == 0)
        fill(p);
    else {
        for (real node : sequence(0, 3)) {
            carpet(corner_subquad(p, node), order - 1);
            carpet(noncorner_subquad(p, node, node + 1), order - 1);
        }
    }
}

path q =
   // A square
   unitsquare
   // An oblong rhombus
   // (0, 0) -- (5, 3) -- (0, 6) -- (-5, 3) -- cycle
   // A trapezoid
   // (0, 0) -- (4, 2) -- (6, 2) -- (10, 0) -- cycle
   // A less regular quadrilateral
   // (0, 0) -- (4, 1) -- (9, -4) -- (1, -1) -- cycle
   // A concave shape
   // (0, 0) -- (5, 3) -- (10, 0) -- (5, 1) -- cycle
   ;

size(9 inches, 6 inches);

carpet(q, 5);
```


## AutoHotkey

ahk [http://www.autohotkey.com/forum/topic44657-150.html discussion]

```autohotkey
Loop 4
   MsgBox % Carpet(A_Index)

Carpet(n) {
   Loop % 3**n {
      x := A_Index-1
      Loop % 3**n
         t .= Dot(x,A_Index-1)
      t .= "`n"
   }
   Return t
}

Dot(x,y) {
   While x>0 && y>0
      If (mod(x,3)=1 && mod(y,3)=1)
         Return " "
      Else x //= 3, y //= 3
   Return "."
}
```



## AWK


```AWK
# WSC.AWK - Waclaw Sierpinski's carpet contributed by Dan Nielsen
#
# syntax: GAWK -f WSC.AWK [-v o={a|A}{b|B}] [-v X=anychar] iterations
#
#   -v o=ab default
#      a|A  loose weave | tight weave
#      b|B  don't show | show how the carpet is built
#   -v X=?  Carpet is built with X's. The character assigned to X replaces all X's.
#
#   iterations
#      The number of iterations. The default is 0 which produces one carpet.
#
# what is the difference between a loose weave and a tight weave:
#   loose                tight
#   X X X X X X X X X    XXXXXXXXX
#   X   X X   X X   X    X XX XX X
#   X X X X X X X X X    XXXXXXXXX
#   X X X       X X X    XXX   XXX
#   X   X       X   X    X X   X X
#   X X X       X X X    XXX   XXX
#   X X X X X X X X X    XXXXXXXXX
#   X   X X   X X   X    X XX XX X
#   X X X X X X X X X    XXXXXXXXX
#
# examples:
#   GAWK -f WSC.AWK 2
#   GAWK -f WSC.AWK -v o=Ab -v X=# 2
#   GAWK -f WSC.AWK -v o=Ab -v X=\xDB 2
#
BEGIN {
    optns = (o == "") ? "ab" : o
    n = ARGV[1] + 0 # iterations
    if (n !~ /^[0-9]+$/) { exit(1) }
    seed = (optns ~ /A/) ? "XXX,X X,XXX" : "X X X ,X   X ,X X X " # tight/loose weave
    leng = row = split(seed,A,",") # seed the array
    for (i=1; i<=n; i++) { # build carpet
      for (a=1; a<=3; a++) {
        row = 0
        for (b=1; b<=3; b++) {
          for (c=1; c<=leng; c++) {
            row++
            tmp = (a == 2 && b == 2) ? sprintf("%*s",length(A[c]),"") : A[c]
            B[row] = B[row] tmp
          }
          if (optns ~ /B/) { # show how the carpet is built
            if (max_row < row+0) { max_row = row }
            for (r=1; r<=max_row; r++) {
              printf("i=%d row=%02d a=%d b=%d '%s'\n",i,r,a,b,B[r])
            }
            print("")
          }
        }
      }
      leng = row
      for (j=1; j<=row; j++) { A[j] = B[j] } # re-seed the array
      for (j in B) { delete B[j] } # delete work array
    }
    for (j=1; j<=row; j++) { # print carpet
      if (X != "") { gsub(/X/,substr(X,1,1),A[j]) }
      sub(/ +$/,"",A[j])
      printf("%s\n",A[j])
    }
    exit(0)
}
```

```txt

GAWK -f WSC.AWK 1

X X X X X X X X X
X   X X   X X   X
X X X X X X X X X
X X X       X X X
X   X       X   X
X X X       X X X
X X X X X X X X X
X   X X   X X   X
X X X X X X X X X

GAWK -f WSC.AWK -v o=A 1

XXXXXXXXX
X XX XX X
XXXXXXXXX
XXX   XXX
X X   X X
XXX   XXX
XXXXXXXXX
X XX XX X
XXXXXXXXX

```



## BBC BASIC

```bbcbasic
      Order% = 3
      side% = 3^Order%
      VDU 23,22,8*side%;8*side%;64,64,16,128
      FOR Y% = 0 TO side%-1
        FOR X% = 0 TO side%-1
          IF FNincarpet(X%,Y%) PLOT X%*16,Y%*16+15
        NEXT
      NEXT Y%
      REPEAT WAIT 1 : UNTIL FALSE
      END

      DEF FNincarpet(X%,Y%)
      REPEAT
        IF X% MOD 3 = 1 IF Y% MOD 3 = 1 THEN = FALSE
        X% DIV= 3
        Y% DIV= 3
      UNTIL X%=0 AND Y%=0
      = TRUE
```

[[File:sierpinski_carpet_bbc.gif]]


## Befunge

The order, N, is specified by the first number on the stack. The upper limit is implementation dependent and is determined by the interpreter's cell size.


```befunge>311
*#3\>#-:#1_$:00p00g-#@_010p0>:20p10g30v
>p>40p"#"30g40g*!#v_$48*30g3%1-v^ >$55+,1v>
0 ^p03/3g03/3g04$_v#!*!-1%3g04!<^_^#- g00 <
^3g01p02:0p01_@#-g>#0,#02#:0#+g#11#g+#0:#<^
```



## C

If you write coordinates of any point on the carpet in base 3, the pixel if blank if and only if any matching pair of digits are (1, 1).

```c
#include <stdio.h>

int main()
{
	int i, j, dim, d;
	int depth = 3;

	for (i = 0, dim = 1; i < depth; i++, dim *= 3);

	for (i = 0; i < dim; i++) {
		for (j = 0; j < dim; j++) {
			for (d = dim / 3; d; d /= 3)
				if ((i % (d * 3)) / d == 1 && (j % (d * 3)) / d == 1)
					break;
			printf(d ? "  " : "##");
		}
		printf("\n");
	}

	return 0;
}
```



```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct sCarpet {
    int dim;      // dimension
    char *data;   // character data
    char **rows;  // pointers to data rows
} *Carpet;

/* Clones a tile into larger carpet, or blank if center */
void TileCarpet( Carpet d, int r, int c, Carpet tile )
{
    int y0 = tile->dim*r;
    int x0 = tile->dim*c;
    int k,m;

    if ((r==1) && (c==1)) {
        for(k=0; k < tile->dim; k++) {
           for (m=0; m < tile->dim; m++) {
               d->rows[y0+k][x0+m] = ' ';
           }
        }
    }
    else {
        for(k=0; k < tile->dim; k++) {
           for (m=0; m < tile->dim; m++) {
               d->rows[y0+k][x0+m] = tile->rows[k][m];
           }
        }
    }
}

/* define a 1x1 starting carpet */
static char s1[]= "#";
static char *r1[] = {s1};
static struct sCarpet single = { 1, s1, r1};

Carpet Sierpinski( int n )
{
   Carpet carpet;
   Carpet subCarpet;
   int row,col, rb;
   int spc_rqrd;

   subCarpet = (n > 1) ? Sierpinski(n-1) : &single;

   carpet = malloc(sizeof(struct sCarpet));
   carpet->dim = 3*subCarpet->dim;
   spc_rqrd = (2*subCarpet->dim) * (carpet->dim);
   carpet->data = malloc(spc_rqrd*sizeof(char));
   carpet->rows = malloc( carpet->dim*sizeof(char *));
   for (row=0; row<subCarpet->dim; row++) {
       carpet->rows[row] = carpet->data + row*carpet->dim;
       rb = row+subCarpet->dim;
       carpet->rows[rb] = carpet->data + rb*carpet->dim;
       rb = row+2*subCarpet->dim;
       carpet->rows[rb] = carpet->data + row*carpet->dim;
   }

    for (col=0; col < 3; col++) {
      /* 2 rows of tiles to copy - third group points to same data a first */
      for (row=0; row < 2; row++)
         TileCarpet( carpet, row, col, subCarpet );
    }
    if (subCarpet != &single ) {
       free(subCarpet->rows);
       free(subCarpet->data);
       free(subCarpet);
    }

    return carpet;
}

void CarpetPrint( FILE *fout, Carpet carp)
{
    char obuf[730];
    int row;
    for (row=0; row < carp->dim; row++) {
       strncpy(obuf, carp->rows[row], carp->dim);
       fprintf(fout, "%s\n", obuf);
    }
    fprintf(fout,"\n");
}

int main(int argc, char *argv[])
{
//    FILE *f = fopen("sierp.txt","w");
    CarpetPrint(stdout, Sierpinski(3));
//    fclose(f);
    return 0;
}
```

Recursive version:

```c
#include <stdio.h>
#include <stdlib.h>

typedef struct _PartialGrid{
        char** base;
        int xbegin, xend, ybegin, yend; // yend strictly not used
} PartialGrid;

void sierpinski_hollow(PartialGrid G){
        int len = G.xend - G.xbegin+1;
        int unit = len/3;
        for(int i = G.xbegin+unit; i <G.xbegin+2*unit;i++){
        for(int j = G.ybegin+unit; j <G.ybegin+2*unit;j++){
                G.base[j][i] = ' ';
        }}
}

void sierpinski(PartialGrid G, int iterations){
        if(iterations==0)
                return;
        if((iterations)==1){
                sierpinski_hollow(G);
                sierpinski(G,0);
        }
        sierpinski_hollow(G);
        for(int i=0;i<3;i++){
                for(int j=0;j<3;j++){
                        int length = G.xend-G.xbegin+1;
                        int unit = length/3;
                        PartialGrid q = {G.base, G.xbegin + i*unit, G.xbegin+(i+1)*unit-1,
                                G.ybegin+j*unit, G.ybegin+(j+1)*unit-1};
                        sierpinski(q, iterations-1);
                }
        }
}

int intpow(int base, int expo){
        if(expo==0){
                return 1;
        }
        return base*intpow(base,expo-1);
}

int allocate_grid(char*** g, int n, const char sep){
        int size = intpow(3,n+1);
        *g = (char**)calloc(size, sizeof(char*));
        if(*g==NULL)
                return -1;

        for(int i = 0; i < size; ++i){
                (*g)[i] = (char*)calloc(size, sizeof(char));
                if((*g)[i] == NULL)
                        return -1;
                for(int j = 0; j < size; j++){
                        (*g)[i][j] = sep;
                }
        }

        return size;
}

void print_grid(char** b, int size){
        for(int i = 0; i < size; i++){
                printf("%s\n",b[i]);
        }
}

int main(){
        int n = 3;

        char** basegrid;
        int size = allocate_grid(&basegrid, n, '#');
        if(size == -1)
                return 1; //bad alloc
        PartialGrid b = {basegrid, 0, size-1, 0, size-1};
        sierpinski(b, n);
        print_grid(basegrid, size);
        free(basegrid);

        return 0;
}
```



## C++

[[File:sierpinski_cpp.png|300px]]

```cpp

#include <windows.h>
#include <math.h>

//--------------------------------------------------------------------------------------------------
const int BMP_SIZE = 738;

//--------------------------------------------------------------------------------------------------
class Sierpinski
{
public:
    void draw( HDC wdc, int wid, int hei, int ord )
    {
	_wdc = wdc;
        _ord = wid / static_cast<int>( pow( 3.0, ord ) );
	drawIt( 0, 0, wid, hei );
    }

    void setHWND( HWND hwnd ) { _hwnd = hwnd; }

private:
    void drawIt( int x, int y, int wid, int hei )
    {
	if( wid < _ord || hei < _ord ) return;
	int w = wid / 3, h = hei / 3;
	RECT rc;
	SetRect( &rc, x + w, y + h, x + w + w, y + h + h );
	FillRect( _wdc, &rc, static_cast<HBRUSH>( GetStockObject( BLACK_BRUSH ) ) );

	for( int a = 0; a < 3; a++ )
	    for( int b = 0; b < 3; b++ )
	    {
		if( a == 1 && b == 1 ) continue;
		drawIt( x + b * w, y + a * h, w, h );
	    }
    }

    HWND     _hwnd;
    HDC      _wdc;
    int      _ord;
};
//--------------------------------------------------------------------------------------------------
class wnd
{
public:
    wnd() { _inst = this; }
    int wnd::Run( HINSTANCE hInst )
    {
	_hInst = hInst;
	_hwnd = InitAll();

	_carpet.setHWND( _hwnd );

	ShowWindow( _hwnd, SW_SHOW );
	UpdateWindow( _hwnd );

	MSG msg;
	ZeroMemory( &msg, sizeof( msg ) );
	while( msg.message != WM_QUIT )
	{
	    if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) != 0 )
	    {
		TranslateMessage( &msg );
		DispatchMessage( &msg );
	    }
	}
	return UnregisterClass( "_SIERPINSKI_", _hInst );
    }
private:
    void wnd::doPaint( HDC dc ) { _carpet.draw( dc, BMP_SIZE, BMP_SIZE, 5 ); }

    static int WINAPI wnd::WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
    {
	switch( msg )
	{
	    case WM_DESTROY: PostQuitMessage( 0 ); break;
	    case WM_PAINT:
	    {
		PAINTSTRUCT ps;
		HDC dc = BeginPaint( hWnd, &ps );
		_inst->doPaint( dc );
		EndPaint( hWnd, &ps );
	    }
	    default:
	        return DefWindowProc( hWnd, msg, wParam, lParam );
	}
	return 0;
    }

    HWND InitAll()
    {
	WNDCLASSEX wcex;
	ZeroMemory( &wcex, sizeof( wcex ) );
	wcex.cbSize	       = sizeof( WNDCLASSEX );
	wcex.style	       = CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc   = ( WNDPROC )WndProc;
	wcex.hInstance     = _hInst;
	wcex.hCursor       = LoadCursor( NULL, IDC_ARROW );
	wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
	wcex.lpszClassName = "_SIERPINSKI_";

	RegisterClassEx( &wcex );

	RECT rc = { 0, 0, BMP_SIZE, BMP_SIZE };
	AdjustWindowRect( &rc, WS_SYSMENU | WS_CAPTION, FALSE );
	int w = rc.right - rc.left,
	    h = rc.bottom - rc.top;
	return CreateWindow( "_SIERPINSKI_", ".: Sierpinski carpet -- PJorente :.", WS_SYSMENU, CW_USEDEFAULT, 0, w, h, NULL, NULL, _hInst, NULL );
    }

    static wnd* _inst;
    HINSTANCE  _hInst;
    HWND       _hwnd;
    Sierpinski _carpet;
};
wnd* wnd::_inst = 0;
//--------------------------------------------------------------------------------------------------
int APIENTRY _tWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow )
{
    wnd myWnd;
    return myWnd.Run( hInstance );
}
//--------------------------------------------------------------------------------------------------

```


## C#
```c#
using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static List<string> NextCarpet(List<string> carpet)
    {
        return carpet.Select(x => x + x + x)
                     .Concat(carpet.Select(x => x + x.Replace('#', ' ') + x))
                     .Concat(carpet.Select(x => x + x + x)).ToList();
    }

    static List<string> SierpinskiCarpet(int n)
    {
        return Enumerable.Range(1, n).Aggregate(new List<string> { "#" }, (carpet, _) => NextCarpet(carpet));
    }

    static void Main(string[] args)
    {
        foreach (string s in SierpinskiCarpet(3))
            Console.WriteLine(s);
    }
}
```



## Clojure

```clojure
(ns example
  (:require [clojure.contrib.math :as math]))

(defn in-carpet? [x y]
  (loop [x x, y y]
    (cond
     (or (zero? x) (zero? y))              true
     (and (= 1 (mod x 3)) (= 1 (mod y 3))) false
     :else                                 (recur (quot x 3) (quot y 3)))))

(defn carpet [n]
  (apply str
         (interpose
	  \newline
	  (for [x (range (math/expt 3 n))]
	    (apply str
		   (for [y (range (math/expt 3 n))]
		     (if (in-carpet? x y) "*" " ")))))))

(println (carpet 3))
```



## Common Lisp

This solution works by printing a square of # except where both of the coordinates of a cell contain a 1 in the same digit position in base 3. For example, the central empty square has a 1 in the highest base-3 digit of all its cells, and the smallest empty squares have 1s in the lowest base-3 digit.

```lisp
(defun print-carpet (order)
  (let ((size (expt 3 order)))
    (flet ((trinary (x) (format nil "~3,vR" order x))
           (ones (a b) (and (eql a #\1) (eql b #\1))))
      (loop for i below size do
        (fresh-line)
        (loop for j below size do
          (princ (if (some #'ones (trinary i) (trinary j))
                   " "
                   "#")))))))
```


## Crystal

```ruby
def sierpinski_carpet(n)
  carpet = ["#"]
  n.times do
    carpet = carpet.map { |x| x + x + x } +
             carpet.map { |x| x + x.tr("#"," ") + x } +
             carpet.map { |x| x + x + x }
  end
  carpet
end

5.times{ |i| puts "\nN=#{i}"; sierpinski_carpet(i).each { |row| puts row } }
```


<pre style="height: 64ex; overflow: scroll">
N=0
#

N=1
###
# #
###

N=2
#########
# ## ## #
#########
###   ###
# #   # #
###   ###
#########
# ## ## #
#########

N=3
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
#########         #########
# ## ## #         # ## ## #
#########         #########
###   ###         ###   ###
# #   # #         # #   # #
###   ###         ###   ###
#########         #########
# ## ## #         # ## ## #
#########         #########
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################

N=4
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
# #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # #
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
#########         ##################         ##################         #########
# ## ## #         # ## ## ## ## ## #         # ## ## ## ## ## #         # ## ## #
#########         ##################         ##################         #########
###   ###         ###   ######   ###         ###   ######   ###         ###   ###
# #   # #         # #   # ## #   # #         # #   # ## #   # #         # #   # #
###   ###         ###   ######   ###         ###   ######   ###         ###   ###
#########         ##################         ##################         #########
# ## ## #         # ## ## ## ## ## #         # ## ## ## ## ## #         # ## ## #
#########         ##################         ##################         #########
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
# #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # #
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###########################                           ###########################
# ## ## ## ## ## ## ## ## #                           # ## ## ## ## ## ## ## ## #
###########################                           ###########################
###   ######   ######   ###                           ###   ######   ######   ###
# #   # ## #   # ## #   # #                           # #   # ## #   # ## #   # #
###   ######   ######   ###                           ###   ######   ######   ###
###########################                           ###########################
# ## ## ## ## ## ## ## ## #                           # ## ## ## ## ## ## ## ## #
###########################                           ###########################
#########         #########                           #########         #########
# ## ## #         # ## ## #                           # ## ## #         # ## ## #
#########         #########                           #########         #########
###   ###         ###   ###                           ###   ###         ###   ###
# #   # #         # #   # #                           # #   # #         # #   # #
###   ###         ###   ###                           ###   ###         ###   ###
#########         #########                           #########         #########
# ## ## #         # ## ## #                           # ## ## #         # ## ## #
#########         #########                           #########         #########
###########################                           ###########################
# ## ## ## ## ## ## ## ## #                           # ## ## ## ## ## ## ## ## #
###########################                           ###########################
###   ######   ######   ###                           ###   ######   ######   ###
# #   # ## #   # ## #   # #                           # #   # ## #   # ## #   # #
###   ######   ######   ###                           ###   ######   ######   ###
###########################                           ###########################
# ## ## ## ## ## ## ## ## #                           # ## ## ## ## ## ## ## ## #
###########################                           ###########################
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
# #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # #
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
#########         ##################         ##################         #########
# ## ## #         # ## ## ## ## ## #         # ## ## ## ## ## #         # ## ## #
#########         ##################         ##################         #########
###   ###         ###   ######   ###         ###   ######   ###         ###   ###
# #   # #         # #   # ## #   # #         # #   # ## #   # #         # #   # #
###   ###         ###   ######   ###         ###   ######   ###         ###   ###
#########         ##################         ##################         #########
# ## ## #         # ## ## ## ## ## #         # ## ## ## ## ## #         # ## ## #
#########         ##################         ##################         #########
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
# #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # #
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################

```




## D

```d
import std.stdio, std.string, std.algorithm, std.array;

auto sierpinskiCarpet(in int n) pure nothrow @safe {
    auto r = ["#"];
    foreach (immutable _; 0 .. n) {
        const p = r.map!q{a ~ a ~ a}.array;
        r = p ~ r.map!q{a ~ a.replace("#", " ") ~ a}.array ~ p;
    }
    return r.join('\n');
}

void main() {
    3.sierpinskiCarpet.writeln;
}
```

More functional style:

```d
import std.stdio, std.algorithm, std.range, std.functional;

auto nextCarpet(in string[] c) pure nothrow {
    /*immutable*/ const b = c.map!q{a ~ a ~ a}.array;
    return b ~ c.map!q{a ~ a.replace("#", " ") ~ a}.array ~ b;
}

void main() {
    ["#"]
    .recurrence!((a, n) => a[n - 1].nextCarpet)
    .dropExactly(3)
    .front
    .binaryReverseArgs!writefln("%-(%s\n%)");
}
```


A more direct and efficient version:

```d
import std.stdio, std.array;

char[][] sierpinskiCarpet(in size_t n) pure nothrow @safe {
    auto mat = uninitializedArray!(typeof(return))(3 ^^ n, 3 ^^ n);

    foreach (immutable r, row; mat) {
        row[] = '#';
        foreach (immutable c, ref cell; row) {
            size_t r2 = r, c2 = c;
            while (r2 && c2) {
                if (r2 % 3 == 1 && c2 % 3 == 1) {
                    cell = ' ';
                    break;
                }
                r2 /= 3;
                c2 /= 3;
            }
        }
    }

    return mat;
}

void main() {
    writefln("%-(%s\n%)", 3.sierpinskiCarpet);
    7.sierpinskiCarpet.length.writeln;
}
```



## DWScript

```delphi
function InCarpet(x, y : Integer) : Boolean;
begin
   while (x<>0) and (y<>0) do begin
      if ((x mod 3)=1) and ((y mod 3)=1) then
         Exit(False);
      x := x div 3;
      y := y div 3;
   end;
   Result := True;
end;

procedure Carpet(n : Integer);
var
   i, j, p : Integer;
begin
   p := Round(IntPower(3, n));

   for i:=0 to p-1 do begin
      for j:=0 to p-1 do begin
         if InCarpet(i, j) then
            Print('#')
         else Print(' ');
      end;
      PrintLn('');
   end;
end;

Carpet(3);
```



## E

```e
def inCarpet(var x, var y) {
    while (x > 0 && y > 0) {
        if (x %% 3 <=> 1 && y %% 3 <=> 1) {
            return false
        }
        x //= 3
        y //= 3
    }
    return true
}

def carpet(order) {
    for y in 0..!(3**order) {
        for x in 0..!(3**order) {
            print(inCarpet(x, y).pick("#", " "))
        }
        println()
    }
}
```



## Elixir

```elixir
defmodule RC do
  def sierpinski_carpet(n), do: sierpinski_carpet(n, ["#"])

  def sierpinski_carpet(0, carpet), do: carpet
  def sierpinski_carpet(n, carpet) do
    new_carpet = Enum.map(carpet, fn x -> x <> x <> x end) ++
                 Enum.map(carpet, fn x -> x <> String.replace(x, "#", " ") <> x end) ++
                 Enum.map(carpet, fn x -> x <> x <> x end)
    sierpinski_carpet(n-1, new_carpet)
  end
end

Enum.each(0..3, fn n ->
  IO.puts "\nN=#{n}"
  Enum.each(RC.sierpinski_carpet(n), fn line -> IO.puts line end)
end)
```


<pre style="height: 80ex; overflow: scroll">
N=0
#

N=1
###
# #
###

N=2
#########
# ## ## #
#########
###   ###
# #   # #
###   ###
#########
# ## ## #
#########

N=3
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
#########         #########
# ## ## #         # ## ## #
#########         #########
###   ###         ###   ###
# #   # #         # #   # #
###   ###         ###   ###
#########         #########
# ## ## #         # ## ## #
#########         #########
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################

```



## Erlang


```erlang
% Implemented by Arjun Sunel
-module(carpet).
-export([main/0]).

main() ->
	sierpinski_carpet(3).

sierpinski_carpet(N) ->
	lists: foreach(fun(X) -> lists: foreach(fun(Y) -> carpet(X,Y) end,lists:seq(0,trunc(math:pow(3,N))-1)), io:format("\n") end, lists:seq(0,trunc(math:pow(3,N))-1)).

carpet(X,Y) ->
	if
		X=:=0 ; Y=:=0 ->
			io:format("*");
		(X rem 3)=:=1, (Y rem 3) =:=1 ->
			io:format(" ");
		true ->
			carpet(X div 3, Y div 3)
	end.

```

```txt
***************************
* ** ** ** ** ** ** ** ** *
***************************
***   ******   ******   ***
* *   * ** *   * ** *   * *
***   ******   ******   ***
***************************
* ** ** ** ** ** ** ** ** *
***************************
*********         *********
* ** ** *         * ** ** *
*********         *********
***   ***         ***   ***
* *   * *         * *   * *
***   ***         ***   ***
*********         *********
* ** ** *         * ** ** *
*********         *********
***************************
* ** ** ** ** ** ** ** ** *
***************************
***   ******   ******   ***
* *   * ** *   * ** *   * *
***   ******   ******   ***
***************************
* ** ** ** ** ** ** ** ** *
***************************
ok

```



## ERRE


```ERRE


PROGRAM SIERP_CARPET

! for rosettacode.org

!$INTEGER

BEGIN
 OPEN("O",1,"OUT.PRN")
 PRINT(CHR$(12);) !CLS
 DEPTH=3
 DIMM=1

 FOR I=0 TO DEPTH-1 DO
   DIMM=DIMM*3
 END FOR

 FOR I=0 TO DIMM-1 DO
   FOR J=0 TO DIMM-1 DO
     D=DIMM DIV 3
     REPEAT
        EXIT IF ((I MOD (D*3)) DIV D=1 AND (J MOD (D*3)) DIV D=1)
        D=D DIV 3
     UNTIL NOT(D>0)
     IF D>0 THEN PRINT(#1,"  ";)  ELSE PRINT(#1,"##";) END IF
   END FOR
   PRINT(#1,)
 END FOR
 ! PRINT(#1,CHR$(12);) for printer only!
 CLOSE(1)
END PROGRAM

```

Output is redirected to file OUT.PRN: you can change this to SCRN: to screen or "LPTx:" for a parallel printer.
Output taken from OUT.PRN file:
```txt

######################################################
##  ####  ####  ####  ####  ####  ####  ####  ####  ##
######################################################
######      ############      ############      ######
##  ##      ##  ####  ##      ##  ####  ##      ##  ##
######      ############      ############      ######
######################################################
##  ####  ####  ####  ####  ####  ####  ####  ####  ##
######################################################
##################                  ##################
##  ####  ####  ##                  ##  ####  ####  ##
##################                  ##################
######      ######                  ######      ######
##  ##      ##  ##                  ##  ##      ##  ##
######      ######                  ######      ######
##################                  ##################
##  ####  ####  ##                  ##  ####  ####  ##
##################                  ##################
######################################################
##  ####  ####  ####  ####  ####  ####  ####  ####  ##
######################################################
######      ############      ############      ######
##  ##      ##  ####  ##      ##  ####  ##      ##  ##
######      ############      ############      ######
######################################################
##  ####  ####  ####  ####  ####  ####  ####  ####  ##
######################################################

```



## Euphoria


```euphoria

include std/math.e

integer order = 4

function InCarpet(atom x, atom y)
	while 1 do
		if x = 0 or y = 0 then
			return 1
		elsif floor(mod(x,3)) = 1 and floor(mod(y,3)) = 1 then
			return 0
		end if
		x /= 3
		y /= 3
	end while
end function

for i = 0 to power(3,order)-1 do
	for j = 0 to power(3,order)-1 do
		if InCarpet(i,j) = 1 then
			puts(1,"#")
		else
			puts(1," ")
		end if
	end for
	puts(1,'\n')
end for

```


=={{header|F_Sharp|F#}}==
```fsharp
open System

let blank x = new String(' ', String.length x)

let nextCarpet carpet =
  List.map (fun x -> x + x + x) carpet @
  List.map (fun x -> x + (blank x) + x) carpet @
  List.map (fun x -> x + x + x) carpet

let rec sierpinskiCarpet n =
  let rec aux n carpet =
    if n = 0 then carpet
             else aux (n-1) (nextCarpet carpet)
  aux n ["#"]

List.iter (printfn "%s") (sierpinskiCarpet 3)
```



## Fan


```Fan
**
** Generates a square Sierpinski gasket
**
class SierpinskiCarpet
{
  public static Bool inCarpet(Int x, Int y){
    while(x!=0 && y!=0){
      if (x % 3 == 1 && y % 3 == 1)
        return false;
      x /= 3;
      y /= 3;
    }
    return true;
  }

  static Int pow(Int n, Int exp)
  {
    rslt := 1
    exp.times { rslt *= n }
    return rslt
  }

  public static Void carpet(Int n){
    for(i := 0; i < pow(3, n); i++){
      buf := StrBuf()
      for(j := 0; j < pow(3, n); j++){
        if( inCarpet(i, j))
          buf.add("*");
        else
          buf.add(" ");
      }
      echo(buf);
    }
  }

  Void main()
  {
    carpet(4)
  }
}
```



## Forth

```forth
\ Generates a square Sierpinski gasket
: 1? over 3 mod 1 = ;                  ( n1 n2 -- n1 n2 f)
: 3/ 3 / swap ;                        ( n1 n2 -- n2/3 n1)
                                       \ is this cell in the carpet?
: incarpet                             ( n1 n2 -- f)
  begin over over or while 1? 1? and if 2drop false exit then 3/ 3/ repeat
  2drop true                           \ return true if in the carpet
;
                                       \ draw a carpet of n size
: carpet                               ( n --)
  1 swap 0 ?do 3 * loop dup            \ calculate power of 3
  0 ?do dup 0 ?do i j incarpet if [char] # else bl then emit loop cr loop
  drop                                 \ evaluate every cell in the carpet
;

cr 4 carpet
```



## Fortran

```fortran
program Sierpinski_carpet
  implicit none

  call carpet(4)

contains

function In_carpet(a, b)
  logical :: in_carpet
  integer, intent(in) :: a, b
  integer :: x, y

  x = a ; y = b
  do
    if(x == 0 .or. y == 0) then
      In_carpet = .true.
      return
    else if(mod(x, 3) == 1 .and. mod(y, 3) == 1) then
      In_carpet = .false.
      return
    end if
    x = x / 3
    y = y / 3
  end do
end function

subroutine Carpet(n)
  integer, intent(in) :: n
  integer :: i, j

  do i = 0, 3**n - 1
    do j = 0, 3**n - 1
      if(In_carpet(i, j)) then
        write(*, "(a)", advance="no") "#"
      else
        write(*, "(a)", advance="no") " "
      end if
    end do
    write(*,*)
  end do
end subroutine Carpet
end program Sierpinski_carpet
```



## Gnuplot

### Version #1.

;Note:
* Find '''plotff.gp''' file-function for load command here on RC [[Brownian_tree#gnuplot| Brownian tree page]].
* dat-files are PARI/GP generated output files. See [[Sierpinski_carpet#PARI.2FGP| this page]] below.
[[File:SC5gp1.png|right|thumb|Output SC5gp1.png]]


```gnuplot

## SCff.gp 1/14/17 aev
## Plotting Sierpinski carpet fractal.
## dat-files are PARI/GP generated output files:
## http://rosettacode.org/wiki/Sierpinski_carpet#PARI.2FGP
#cd 'C:\gnupData'

##SC5
clr = '"green"'
filename = "SC5gp1"
ttl = "Sierpinski carpet fractal, v.#1"
load "plotff.gp"

```

```txt

1. All SCff.gp commands.
2. All plotted png-files:
   SC5gp1.png (for now)

```



### Plotting a Sierpinski carpet fractal: versions #2 and #3

'''plotscf.gp''' and '''plotscf1.gp''' file-functions for the load command are the only possible imitation of the fine functions in the '''gnuplot'''.

[[File:SCF21gp.png|right|thumb|Output SCF21gp.png]]
[[File:SCF22gp.png|right|thumb|Output SCF22gp.png]]
[[File:SCF31gp.png|right|thumb|Output SCF31gp.png]]

;plotscf.gp:

```gnuplot

## plotscf.gp 12/7/16 aev
## Plotting a Sierpinski carpet fractal to the png-file.
## Note: assign variables: ord (order), clr (color), filename and ttl (before using load command).
## ord (order)  # a.k.a. level - defines size of fractal (also number of dots).
reset
set terminal png font arial 12 size 640,640
ofn=filename.".png"
set output ofn
unset border; unset xtics; unset ytics; unset key;
set title ttl font "Arial:Bold,12"
set xrange [0:1]; set yrange [0:1];
sc(n, x, y, d) = n >= ord ?  \
  sprintf('set object rect from %f,%f to %f,%f fc rgb @clr fs solid;', x, y, x+d, y+d) : \
  sc(n+1, x, y, d/3) . sc(n+1, x+d/3, y, d/3) . \
  sc(n+1, x+2*d/3, y, d/3) . sc(n+1, x, y+d/3, d/3) . \
  sc(n+1, x+2*d/3, y+d/3, d/3) . sc(n+1, x, y+2*d/3, d/3) . \
  sc(n+1, x+d/3, y+2*d/3, d/3) . sc(n+1, x+2*d/3, y+2*d /3, d/3);
eval(sc(0, 0.0, 0.0, 1.0))
plot -100
set output

```


;plotscf1.gp:

```gnuplot

## plotscf1.gp 12/7/16 aev
## Plotting a Sierpinski carpet fractal to the png-file.
## Note: assign variables: ord (order, just for title), clr (color), filename and ttl (before using load command).
## In this version order is always 5.
reset
set terminal png font arial 12 size 640,640
ofn=filename.".png"
set output ofn
unset border; unset xtics; unset ytics; unset key;
set title ttl font "Arial:Bold,12"
o=3
sqr(x,y) = abs(x + y) + abs(x - y) - o
f(x) = o*abs(x) - o
c0(x,y) = abs(x + y) + abs(x - y) - 1
c1(x,y) = c0(o*x,f(y)) * c0(f(x),o*y) * c0(f(x),f(y))
c2(x,y) = c1(o*x,f(y)) * c1(f(x),o*y) * c1(f(x),f(y))
c3(x,y) = c2(o*x,f(y)) * c2(f(x),o*y) * c2(f(x),f(y))
c4(x,y) = c3(o*x,f(y)) * c3(f(x),o*y) * c3(f(x),f(y))
sc(x,y) = sqr(x,y)>0 || c0(x,y)*c1(x,y)*c2(x,y)*c3(x,y)*c4(x,y)<0 ? 0:1
set xrange [-1.5:1.5]; set yrange [-1.5:1.5];
set pm3d map;
set palette model RGB defined (0 "white", 1 @clr);
set size ratio -1
smp=640; set samples smp; set isosamples smp;
unset colorbox
splot sc(x,y)
set output

```


;Plotting v.#2 and v.#3:

```gnuplot

## pSCF.gp 12/7/16 aev
## Plotting Sierpinski carpet fractals.
## Note: assign variables: ord (order), clr (color), filename and ttl (before using load command).
## ord (order)  # a.k.a. level - defines size of fractal (also number of dots).
#cd 'C:\gnupData'

##SCF21
ord=3; clr = '"red"';
filename = "SCF21gp"; ttl = "Sierpinski carpet fractal #21, ord ".ord;
load "plotscf.gp"

##SCF22
ord=5; clr = '"brown"';
filename = "SCF22gp"; ttl = "Sierpinski carpet fractal #22, ord ".ord;
load "plotscf.gp"

##SCF31
ord=5; clr = '"navy"';
filename = "SCF31gp"; ttl = "Sierpinski carpet fractal #31, ord ".ord;
load "plotscf1.gp"

```

```txt

1. All pSCF.gp file commands.
2. 3 plotted png-files: SCF21gp, SCF22gp and SCF31gp

```



## Go

Variable "grain" shown set to "#" here, but it's fun to experiment with other values. "|", ". ", "[]", "___", "██", "░░"...

```go
package main

import (
    "fmt"
    "strings"
    "unicode/utf8"
)

var order = 3
var grain = "#"

func main() {
    carpet := []string{grain}
    for ; order > 0; order-- {
        // repeat expression allows for multiple character
        // grain and for multi-byte UTF-8 characters.
        hole := strings.Repeat(" ", utf8.RuneCountInString(carpet[0]))
        middle := make([]string, len(carpet))
        for i, s := range carpet {
            middle[i] = s + hole + s
            carpet[i] = strings.Repeat(s, 3)
        }
        carpet = append(append(carpet, middle...), carpet...)
    }
    for _, r := range carpet {
        fmt.Println(r)
    }
}
```



## Groovy

Solution, uses list-indexing of base 3 string representation:

```groovy
def base3 = { BigInteger i -> i.toString(3) }

def sierpinskiCarpet = { int order ->
    StringBuffer sb = new StringBuffer()
    def positions = 0..<(3**order)
    def digits = 0..<([order,1].max())

    positions.each { i ->
        String i3 = base3(i).padLeft(order, '0')

        positions.each { j ->
            String j3 = base3(j).padLeft(order, '0')

            sb << (digits.any{ i3[it] == '1' && j3[it] == '1' } ? '  ' : order.toString().padRight(2) )
        }
        sb << '\n'
    }
    sb.toString()
}
```

Test Program:

```groovy
(0..4).each { println sierpinskiCarpet(it) }
```

<pre style="height:30ex;overflow:scroll;">
0

1 1 1
1   1
1 1 1

2 2 2 2 2 2 2 2 2
2   2 2   2 2   2
2 2 2 2 2 2 2 2 2
2 2 2       2 2 2
2   2       2   2
2 2 2       2 2 2
2 2 2 2 2 2 2 2 2
2   2 2   2 2   2
2 2 2 2 2 2 2 2 2

3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
3   3 3   3 3   3 3   3 3   3 3   3 3   3 3   3 3   3
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
3 3 3       3 3 3 3 3 3       3 3 3 3 3 3       3 3 3
3   3       3   3 3   3       3   3 3   3       3   3
3 3 3       3 3 3 3 3 3       3 3 3 3 3 3       3 3 3
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
3   3 3   3 3   3 3   3 3   3 3   3 3   3 3   3 3   3
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
3 3 3 3 3 3 3 3 3                   3 3 3 3 3 3 3 3 3
3   3 3   3 3   3                   3   3 3   3 3   3
3 3 3 3 3 3 3 3 3                   3 3 3 3 3 3 3 3 3
3 3 3       3 3 3                   3 3 3       3 3 3
3   3       3   3                   3   3       3   3
3 3 3       3 3 3                   3 3 3       3 3 3
3 3 3 3 3 3 3 3 3                   3 3 3 3 3 3 3 3 3
3   3 3   3 3   3                   3   3 3   3 3   3
3 3 3 3 3 3 3 3 3                   3 3 3 3 3 3 3 3 3
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
3   3 3   3 3   3 3   3 3   3 3   3 3   3 3   3 3   3
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
3 3 3       3 3 3 3 3 3       3 3 3 3 3 3       3 3 3
3   3       3   3 3   3       3   3 3   3       3   3
3 3 3       3 3 3 3 3 3       3 3 3 3 3 3       3 3 3
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
3   3 3   3 3   3 3   3 3   3 3   3 3   3 3   3 3   3
3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3

4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4
4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4
4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4
4   4 4   4 4   4                   4   4 4   4 4   4 4   4 4   4 4   4                   4   4 4   4 4   4 4   4 4   4 4   4                   4   4 4   4 4   4
4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4
4 4 4       4 4 4                   4 4 4       4 4 4 4 4 4       4 4 4                   4 4 4       4 4 4 4 4 4       4 4 4                   4 4 4       4 4 4
4   4       4   4                   4   4       4   4 4   4       4   4                   4   4       4   4 4   4       4   4                   4   4       4   4
4 4 4       4 4 4                   4 4 4       4 4 4 4 4 4       4 4 4                   4 4 4       4 4 4 4 4 4       4 4 4                   4 4 4       4 4 4
4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4
4   4 4   4 4   4                   4   4 4   4 4   4 4   4 4   4 4   4                   4   4 4   4 4   4 4   4 4   4 4   4                   4   4 4   4 4   4
4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4
4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4
4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                                                       4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4                                                       4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                                                       4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4                                                       4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4
4   4       4   4 4   4       4   4 4   4       4   4                                                       4   4       4   4 4   4       4   4 4   4       4   4
4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4                                                       4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                                                       4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4                                                       4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                                                       4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4                                                       4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4
4   4 4   4 4   4                   4   4 4   4 4   4                                                       4   4 4   4 4   4                   4   4 4   4 4   4
4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4                                                       4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4
4 4 4       4 4 4                   4 4 4       4 4 4                                                       4 4 4       4 4 4                   4 4 4       4 4 4
4   4       4   4                   4   4       4   4                                                       4   4       4   4                   4   4       4   4
4 4 4       4 4 4                   4 4 4       4 4 4                                                       4 4 4       4 4 4                   4 4 4       4 4 4
4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4                                                       4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4
4   4 4   4 4   4                   4   4 4   4 4   4                                                       4   4 4   4 4   4                   4   4 4   4 4   4
4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4                                                       4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                                                       4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4                                                       4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                                                       4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4                                                       4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4
4   4       4   4 4   4       4   4 4   4       4   4                                                       4   4       4   4 4   4       4   4 4   4       4   4
4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4                                                       4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                                                       4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4                                                       4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                                                       4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4
4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4
4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4
4   4 4   4 4   4                   4   4 4   4 4   4 4   4 4   4 4   4                   4   4 4   4 4   4 4   4 4   4 4   4                   4   4 4   4 4   4
4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4
4 4 4       4 4 4                   4 4 4       4 4 4 4 4 4       4 4 4                   4 4 4       4 4 4 4 4 4       4 4 4                   4 4 4       4 4 4
4   4       4   4                   4   4       4   4 4   4       4   4                   4   4       4   4 4   4       4   4                   4   4       4   4
4 4 4       4 4 4                   4 4 4       4 4 4 4 4 4       4 4 4                   4 4 4       4 4 4 4 4 4       4 4 4                   4 4 4       4 4 4
4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4
4   4 4   4 4   4                   4   4 4   4 4   4 4   4 4   4 4   4                   4   4 4   4 4   4 4   4 4   4 4   4                   4   4 4   4 4   4
4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4                   4 4 4 4 4 4 4 4 4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4
4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4 4   4       4   4
4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4 4 4 4       4 4 4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4 4   4
4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4
```



## Haskell


```haskell
inCarpet :: Int -> Int -> Bool
inCarpet 0 _ = True
inCarpet _ 0 = True
inCarpet x y = not ((xr == 1) && (yr == 1)) && inCarpet xq yq
  where ((xq, xr), (yq, yr)) = (x `divMod` 3, y `divMod` 3)

carpet :: Int -> [String]
carpet n = map
            (zipWith
              (\x y -> if inCarpet x y then '#' else ' ')
              [0..3^n-1]
             . repeat)
            [0..3^n-1]

printCarpet :: Int -> IO ()
printCarpet = mapM_ putStrLn . carpet
```


```haskell
nextCarpet :: [String] -> [String]
nextCarpet carpet = border ++ map f carpet ++ border
  where border = map (concat . replicate 3) carpet
        f x = x ++ map (const ' ') x ++ x

sierpinskiCarpet :: Int -> [String]
sierpinskiCarpet n = iterate nextCarpet ["#"] !! n

main :: IO ()
main = mapM_ putStrLn $ sierpinskiCarpet 3
```


Seems not very different from version above,

```haskell
main :: IO ()
main = putStr . unlines . (!!3) $ iterate next ["#"]

next :: [String] -> [String]
next block =
    block ! block  ! block
              ++
    block ! center ! block
              ++
    block ! block  ! block
    where
      (!)    = zipWith (++)
      center = map (map $ const ' ') block
```


which we could also read as:


```haskell
carpet :: Int -> String
carpet = unlines . (iterate weave ["██"] !!)

weave :: [String] -> [String]
weave xs =
  let f = zipWith (++)
      g = flip f
  in [xs, fmap (const ' ') <$> xs, xs] >>= g xs . f xs

main :: IO ()
main = mapM_ (putStrLn . carpet) [0 .. 2]
```

```txt
██

██████
██  ██
██████

██████████████████
██  ████  ████  ██
██████████████████
██████      ██████
██  ██      ██  ██
██████      ██████
██████████████████
██  ████  ████  ██
██████████████████
```


==={{{header|GUI variant}}}===
Via high-level vector graphics (diagrams -> cairo -> gtk), very slow.
```haskell
{-# LANGUAGE DoRec #-}
import Control.Monad.Trans (lift)
import Data.Colour (Colour)

import Diagrams.Prelude hiding (after)
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Backend.Cairo.Gtk (defaultRender)

import Graphics.Rendering.Diagrams.Points ()
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.GC (gcNew)


main :: IO ()
main = do
  _ <- initGUI
  window <- windowNew
  _ <- window `onDestroy` mainQuit
  window `windowSetResizable` False

  area <- drawingAreaNew
  _ <- area `on` sizeRequest $ return (Requisition 500 500)
  _ <- window `containerAdd` area
  widgetShowAll window

  rec con <- area `on` exposeEvent $ do
                lift $ signalDisconnect con
                lift $ area `defaultRender` carpet 5
                switchToPixBuf area
  mainGUI


-- just workaround for slow redrawing
switchToPixBuf :: DrawingArea -> EventM EExpose Bool
switchToPixBuf area =
    eventArea >>= \ea -> lift $ do
        dw      <- widgetGetDrawWindow area
        (w,h)   <- drawableGetSize dw
        Just pb <- pixbufGetFromDrawable dw ea
        gc      <- gcNew dw
        _ <- area `on` exposeEvent $ lift $
              False <$ drawPixbuf dw gc pb 0 0 0 0 w h RgbDitherNone 0 0
        return False


carpet :: Int -> Diagram Cairo R2
carpet = (iterate next (cell white) !!)

-- of course, one can use hcat / vcat - combinators
next :: Diagram Cairo R2 -> Diagram Cairo R2
next block =
    scale (1/3) . centerXY  $

	(block ||| block ||| block)
    	            ===
    	(block ||| centr ||| block)
    	            ===
    	(block ||| block ||| block)
    where
      centr = cell black

cell :: Colour Float -> Diagram Cairo R2
cell color = square 1 # lineWidth 0 # fillColor color

```


=={{header|Icon}} and {{header|Unicon}}==
The IsFilled procedure is a translation of Java and Python.

```Icon
$define FILLER "*"  # the filler character

procedure main(A)

   width := 3 ^ ( order := (0 < \A[1]) | 3 )
   write("Carpet order= ",order)

   every !(canvas := list(width)) := list(width," ")        # prime the canvas

   every y := 1 to width & x := 1 to width do               # traverse it
      if IsFilled(x-1,y-1) then canvas[x,y] := FILLER       # fill

   every x := 1 to width & y := 1 to width do
      writes((y=1,"\n")|"",canvas[x,y]," ")                 # print
end

procedure IsFilled(x,y)                      #  succeed if x,y should be filled
   while  x ~= 0 & y ~= 0 do {
      if x % 3 = y %3 = 1 then fail
      x /:= 3
      y /:=3
      }
   return
end
```

```txt
Carpet order= 2

* * * * * * * * *
*   * *   * *   *
* * * * * * * * *
* * *       * * *
*   *       *   *
* * *       * * *
* * * * * * * * *
*   * *   * *   *
* * * * * * * * *
```



## Io

Based on Python translation of Ruby.

```Io
sierpinskiCarpet := method(n,
    carpet := list("@")
    n repeat(
        next := list()
        carpet foreach(s, next append(s .. s .. s))
        carpet foreach(s, next append(s .. (s asMutable replaceSeq("@"," ")) .. s))
        carpet foreach(s, next append(s .. s .. s))
        carpet = next
    )
    carpet join("\n")
)

sierpinskiCarpet(3) println
```

```txt
@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ @@ @@ @@ @@ @@ @@ @@ @@ @
@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@   @@@@@@   @@@@@@   @@@
@ @   @ @@ @   @ @@ @   @ @
@@@   @@@@@@   @@@@@@   @@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ @@ @@ @@ @@ @@ @@ @@ @@ @
@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@@@@@@@         @@@@@@@@@
@ @@ @@ @         @ @@ @@ @
@@@@@@@@@         @@@@@@@@@
@@@   @@@         @@@   @@@
@ @   @ @         @ @   @ @
@@@   @@@         @@@   @@@
@@@@@@@@@         @@@@@@@@@
@ @@ @@ @         @ @@ @@ @
@@@@@@@@@         @@@@@@@@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ @@ @@ @@ @@ @@ @@ @@ @@ @
@@@@@@@@@@@@@@@@@@@@@@@@@@@
@@@   @@@@@@   @@@@@@   @@@
@ @   @ @@ @   @ @@ @   @ @
@@@   @@@@@@   @@@@@@   @@@
@@@@@@@@@@@@@@@@@@@@@@@@@@@
@ @@ @@ @@ @@ @@ @@ @@ @@ @
@@@@@@@@@@@@@@@@@@@@@@@@@@@
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Carpet.bas"
110 SET VIDEO MODE 5:SET VIDEO COLOR 0:SET VIDEO X 32:SET VIDEO Y 27
120 OPEN #101:"video:"
130 DISPLAY #101:AT 1 FROM 1 TO 27
140 CALL CARPET(30,0,1000,970,4)
150 DEF CARPET(X1,Y1,X2,Y2,LEV)
160   NUMERIC XT,XY,KX1,KX2,KY1,KY2
170   IF LEV>0 THEN
180     LET XT=(X2-X1)/3:LET YT=(Y2-Y1)/3
190     LET KX1=X1+XT:LET KY1=Y1+YT
200     LET KX2=X2-XT:LET KY2=Y2-YT
210     CALL CARPET(X1,Y1,KX1,KY1,LEV-1)
220     CALL CARPET(KX1,Y1,KX2,KY1,LEV-1)
230     CALL CARPET(KX2,Y1,X2,KY1,LEV-1)
240     CALL CARPET(KX2,KY1,X2,KY2,LEV-1)
250     CALL CARPET(KX2,KY2,X2,Y2,LEV-1)
260     CALL CARPET(KX1,KY2,KX2,Y2,LEV-1)
270     CALL CARPET(X1,KY2,KX1,Y2,LEV-1)
280     CALL CARPET(X1,KY1,KX1,KY2,LEV-1)
290   ELSE
300     PLOT X1,Y1;X2,Y1;X2,Y2;X1,Y2;X1,Y1
310     PLOT X1+4,Y1+4,PAINT
320   END IF
330 END DEF
```



## J

Like the sierpinski triangle, the carpet is straightforward to produce in J. One approach is based on repeatedly putting a function's argument in a box, forming 9 copies of it into a 3 by 3 array, and then replacing the contents of the middle box with blanks:

```j
N=:3
(a:(<1;1)}3 3$<)^:N'   '
```


But N=:3 is big, so let's use N=:2


```j
   N=:2
   (a:(<1;1)}3 3$<)^:N'   '
┌─────────────┬─────────────┬─────────────┐
│┌───┬───┬───┐│┌───┬───┬───┐│┌───┬───┬───┐│
││   │   │   │││   │   │   │││   │   │   ││
│├───┼───┼───┤│├───┼───┼───┤│├───┼───┼───┤│
││   │   │   │││   │   │   │││   │   │   ││
│├───┼───┼───┤│├───┼───┼───┤│├───┼───┼───┤│
││   │   │   │││   │   │   │││   │   │   ││
│└───┴───┴───┘│└───┴───┴───┘│└───┴───┴───┘│
├─────────────┼─────────────┼─────────────┤
│┌───┬───┬───┐│             │┌───┬───┬───┐│
││   │   │   ││             ││   │   │   ││
│├───┼───┼───┤│             │├───┼───┼───┤│
││   │   │   ││             ││   │   │   ││
│├───┼───┼───┤│             │├───┼───┼───┤│
││   │   │   ││             ││   │   │   ││
│└───┴───┴───┘│             │└───┴───┴───┘│
├─────────────┼─────────────┼─────────────┤
│┌───┬───┬───┐│┌───┬───┬───┐│┌───┬───┬───┐│
││   │   │   │││   │   │   │││   │   │   ││
│├───┼───┼───┤│├───┼───┼───┤│├───┼───┼───┤│
││   │   │   │││   │   │   │││   │   │   ││
│├───┼───┼───┤│├───┼───┼───┤│├───┼───┼───┤│
││   │   │   │││   │   │   │││   │   │   ││
│└───┴───┴───┘│└───┴───┴───┘│└───┴───┴───┘│
└─────────────┴─────────────┴─────────────┘
```


or another way of getting the same image starts with the boolean array


```J
   #:7 5 7
1 1 1
1 0 1
1 1 1
```
 and uses that to select either a blank box or a boxed copy if the function's argument:

```j
   N=:2
   ((#:7 5 7){_2{.<)^:N'   '
┌─────────────┬─────────────┬─────────────┐
│┌───┬───┬───┐│┌───┬───┬───┐│┌───┬───┬───┐│
││   │   │   │││   │   │   │││   │   │   ││
│├───┼───┼───┤│├───┼───┼───┤│├───┼───┼───┤│
││   │   │   │││   │   │   │││   │   │   ││
│├───┼───┼───┤│├───┼───┼───┤│├───┼───┼───┤│
││   │   │   │││   │   │   │││   │   │   ││
│└───┴───┴───┘│└───┴───┴───┘│└───┴───┴───┘│
├─────────────┼─────────────┼─────────────┤
│┌───┬───┬───┐│             │┌───┬───┬───┐│
││   │   │   ││             ││   │   │   ││
│├───┼───┼───┤│             │├───┼───┼───┤│
││   │   │   ││             ││   │   │   ││
│├───┼───┼───┤│             │├───┼───┼───┤│
││   │   │   ││             ││   │   │   ││
│└───┴───┴───┘│             │└───┴───┴───┘│
├─────────────┼─────────────┼─────────────┤
│┌───┬───┬───┐│┌───┬───┬───┐│┌───┬───┬───┐│
││   │   │   │││   │   │   │││   │   │   ││
│├───┼───┼───┤│├───┼───┼───┤│├───┼───┼───┤│
││   │   │   │││   │   │   │││   │   │   ││
│├───┼───┼───┤│├───┼───┼───┤│├───┼───┼───┤│
││   │   │   │││   │   │   │││   │   │   ││
│└───┴───┴───┘│└───┴───┴───┘│└───┴───┴───┘│
└─────────────┴─────────────┴─────────────┘
```


That said, using spaces and '#' characters takes a bit more work.  One approach would be:

```j
   N=:2
   ' #'{~(#:7 5 7) ,/@(1 3 ,/"2@|: */)^:N ,.1
#########
# ## ## #
#########
###   ###
# #   # #
###   ###
#########
# ## ## #
#########
   N=:3
   ' #'{~(#:7 5 7) ,/@(1 3 ,/"2@|: */)^:N ,.1
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
#########         #########
# ## ## #         # ## ## #
#########         #########
###   ###         ###   ###
# #   # #         # #   # #
###   ###         ###   ###
#########         #########
# ## ## #         # ## ## #
#########         #########
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
```


Here, what we are doing is forming a tensor product of our #:7 5 7 boolean array with our argument and then collapsing two of the dimensions so they line up right. Our starting argument is the 1 by 1 array with the value 1. Once we have repeated this process enough times, we select spaces for our zeros and pound signs for our 1s.


## Java

```java
public static boolean inCarpet(long x, long y) {
    while (x!=0 && y!=0) {
        if (x % 3 == 1 && y % 3 == 1)
            return false;
        x /= 3;
        y /= 3;
    }
    return true;
}

public static void carpet(final int n) {
    final double power = Math.pow(3,n);
    for(long i = 0; i < power; i++) {
        for(long j = 0; j < power; j++) {
            System.out.print(inCarpet(i, j) ? "*" : " ");
        }
        System.out.println();
    }
}
```



### Animated version

[[File:sierpinski_carpet_java.png|300px|thumb|right]]
```java
import java.awt.*;
import java.awt.event.ActionEvent;
import javax.swing.*;

public class SierpinskiCarpet extends JPanel {
    private final int dim = 513;
    private final int margin = 20;

    private int limit = dim;

    public SierpinskiCarpet() {
        setPreferredSize(new Dimension(dim + 2 * margin, dim + 2 * margin));
        setBackground(Color.white);
        setForeground(Color.orange);

        new Timer(2000, (ActionEvent e) -> {
            limit /= 3;
            if (limit <= 3)
                limit = dim;
            repaint();
        }).start();
    }

    void drawCarpet(Graphics2D g, int x, int y, int size) {
        if (size < limit)
            return;
        size /= 3;
        for (int i = 0; i < 9; i++) {
            if (i == 4) {
                g.fillRect(x + size, y + size, size, size);
            } else {
                drawCarpet(g, x + (i % 3) * size, y + (i / 3) * size, size);
            }
        }
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);
        g.translate(margin, margin);
        drawCarpet(g, 0, 0, dim);
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Sierpinski Carpet");
            f.setResizable(false);
            f.add(new SierpinskiCarpet(), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript



### ES5

In-browser JavaScript (HTML output)
This version also produces a "graphic" via HTML and CSS.

```html4strict
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html;charset=utf-8">
<title>Sierpinski Carpet</title>
<script type='text/javascript'>

var black_char = "#";
var white_char = " ";

var SierpinskiCarpet = function(size) {
    this.carpet = [black_char];
    for (var i = 1; i <= size; i++) {
        this.carpet = [].concat(
            this.carpet.map(this.sier_top),
            this.carpet.map(this.sier_middle),
            this.carpet.map(this.sier_top)
        );
    }
}

SierpinskiCarpet.prototype.sier_top = function(x) {
    var str = new String(x);
    return new String(str+str+str);
}

SierpinskiCarpet.prototype.sier_middle = function (x) {
    var str = new String(x);
    var spacer = str.replace(new RegExp(black_char, 'g'), white_char);
    return new String(str+spacer+str);
}

SierpinskiCarpet.prototype.to_string = function() {
    return this.carpet.join("\n")
}

SierpinskiCarpet.prototype.to_html = function(target) {
    var table = document.createElement('table');
    for (var i = 0; i < this.carpet.length; i++) {
        var row = document.createElement('tr');
        for (var j = 0; j < this.carpet[i].length; j++) {
            var cell = document.createElement('td');
            cell.setAttribute('class', this.carpet[i][j] == black_char ? 'black' : 'white');
            cell.appendChild(document.createTextNode('\u00a0'));
            row.appendChild(cell);
        }
        table.appendChild(row);
    }
    target.appendChild(table);
}

</script>
<style type='text/css'>
    table {border-collapse: collapse;}
    td {width: 1em;}
    .black {background-color: black;}
    .white {background-color: white;}
</style>
</head>
<body>

<pre id='to_string' style='float:left; margin-right:0.25in'>
```

<div id='to_html'></div>

<script type='text/javascript'>
    var sc = new SierpinskiCarpet(3);
    document.getElementById('to_string').appendChild(document.createTextNode(sc.to_string()));
    sc.to_html(document.getElementById('to_html'));
</script>

</body>
</html>
```

[[File:Sierpinski carpet js.png]]


Or, in a functional idiom, generating plain text, and suitable for use in any ES5 JavaScript, whether in a browser or some other environment.

Creates an N by N array of boolean values, which are mapped to lines of characters for output.


```JavaScript
// Orders 1, 2 and 3 of the Sierpinski Carpet
// as lines of text.

// Generic text output for use in any JavaScript environment
// Browser JavaScripts may use console.log() to return textual output
// others use print() or analogous functions.

[1, 2, 3].map(function sierpinskiCarpetOrder(n) {

    // An (n * n) grid of (filled or empty) sub-rectangles
    // n --> [[s]]
    var carpet = function (n) {
            var lstN = range(0, Math.pow(3, n) - 1);

            // State of each cell in an N * N grid
            return lstN.map(function (x) {
                return lstN.map(function (y) {
                    return inCarpet(x, y);
                });
            });
        },

        // State of a given coordinate in the grid:
        // Filled or not ?
        // (See https://en.wikipedia.org/wiki/Sierpinski_carpet#Construction)
        // n --> n --> bool
        inCarpet = function (x, y) {
            return (!x || !y) ? true :
                !(
                    (x % 3 === 1) &&
                    (y % 3 === 1)
                ) && inCarpet(
                    x / 3 | 0,
                    y / 3 | 0
                );
        },

        // Sequence of integers from m to n
        // n --> n --> [n]
        range = function (m, n) {
            return Array.apply(null, Array(n - m + 1)).map(
                function (x, i) {
                    return m + i;
                }
            );
        };

    // Grid of booleans mapped to lines of characters
    // [[bool]] --> s
    return carpet(n).map(function (line) {
        return line.map(function (bool) {
            return bool ? '\u2588' : ' ';
        }).join('');
    }).join('\n');

}).join('\n\n');
```


Output (orders 1, 2 and 3):


```txt
███
█ █
███

█████████
█ ██ ██ █
█████████
███   ███
█ █   █ █
███   ███
█████████
█ ██ ██ █
█████████

███████████████████████████
█ ██ ██ ██ ██ ██ ██ ██ ██ █
███████████████████████████
███   ██████   ██████   ███
█ █   █ ██ █   █ ██ █   █ █
███   ██████   ██████   ███
███████████████████████████
█ ██ ██ ██ ██ ██ ██ ██ ██ █
███████████████████████████
█████████         █████████
█ ██ ██ █         █ ██ ██ █
█████████         █████████
███   ███         ███   ███
█ █   █ █         █ █   █ █
███   ███         ███   ███
█████████         █████████
█ ██ ██ █         █ ██ ██ █
█████████         █████████
███████████████████████████
█ ██ ██ ██ ██ ██ ██ ██ ██ █
███████████████████████████
███   ██████   ██████   ███
█ █   █ ██ █   █ ██ █   █ █
███   ██████   ██████   ███
███████████████████████████
█ ██ ██ ██ ██ ██ ██ ██ ██ █
███████████████████████████
```



### ES6


```JavaScript
(() => {
    'use strict';

    // sierpinskiCarpet :: Int -> String
    let sierpinskiCarpet = n => {

        // carpet :: Int -> [[String]]
        let carpet = n => {
                let xs = range(0, Math.pow(3, n) - 1);
                return xs.map(x => xs.map(y => inCarpet(x, y)));
            },

            // https://en.wikipedia.org/wiki/Sierpinski_carpet#Construction

            // inCarpet :: Int -> Int -> Bool
            inCarpet = (x, y) =>
                (!x || !y) ? true : !(
                    (x % 3 === 1) &&
                    (y % 3 === 1)
                ) && inCarpet(
                    x / 3 | 0,
                    y / 3 | 0
                );

        return carpet(n)
            .map(line => line.map(bool => bool ? '\u2588' : ' ')
                .join(''))
            .join('\n');
    };

    // GENERIC

    // range :: Int -> Int -> [Int]
    let range = (m, n) =>
            Array.from({
                length: Math.floor(n - m) + 1
            }, (_, i) => m + i);

    // TEST

    return [1, 2, 3]
        .map(sierpinskiCarpet);
})();
```


```txt
███
█ █
███

█████████
█ ██ ██ █
█████████
███   ███
█ █   █ █
███   ███
█████████
█ ██ ██ █
█████████

███████████████████████████
█ ██ ██ ██ ██ ██ ██ ██ ██ █
███████████████████████████
███   ██████   ██████   ███
█ █   █ ██ █   █ ██ █   █ █
███   ██████   ██████   ███
███████████████████████████
█ ██ ██ ██ ██ ██ ██ ██ ██ █
███████████████████████████
█████████         █████████
█ ██ ██ █         █ ██ ██ █
█████████         █████████
███   ███         ███   ███
█ █   █ █         █ █   █ █
███   ███         ███   ███
█████████         █████████
█ ██ ██ █         █ ██ ██ █
█████████         █████████
███████████████████████████
█ ██ ██ ██ ██ ██ ██ ██ ██ █
███████████████████████████
███   ██████   ██████   ███
█ █   █ ██ █   █ ██ █   █ █
███   ██████   ██████   ███
███████████████████████████
█ ██ ██ ██ ██ ██ ██ ██ ██ █
███████████████████████████
```



Or, defining the Sierpinksi carpet weave declaratively, in terms of '''zipWith''' and '''concatMap''':


```javascript
(() => {
    'use strict';

    // weave :: [String] -> [String]
    const weave = xs => {
        const f = zipWith(append);
        return concatMap(
            x => f(f(xs)(x))(xs)
        )([
            xs,
            map(x => replicate(length(x))(' '))(
                xs
            ),
            xs
        ]);
    };

    // TEST -----------------------------------------------
    const main = () => {
        const
            sierp = n => unlines(
                take(1 + n, iterate(weave, ['\u2588']))[n]
            ),
            carpet = sierp(2);
        return (
            // console.log(carpet),
            carpet
        );
    };


    // GENERIC ABSTRACTIONS -------------------------------

    // append (++) :: [a] -> [a] -> [a]
    // append (++) :: String -> String -> String
    const append = xs => ys => xs.concat(ys);

    // concatMap :: (a -> [b]) -> [a] -> [b]
    const concatMap = f => xs =>
        xs.reduce((a, x) => a.concat(f(x)), []);

    // iterate :: (a -> a) -> a -> Gen [a]
    function* iterate(f, x) {
        let v = x;
        while (true) {
            yield(v);
            v = f(v);
        }
    }

    // Returns Infinity over objects without finite length
    // this enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs => xs.length || Infinity;

    // map :: (a -> b) -> [a] -> [b]
    const map = f => xs => xs.map(f);

    // replicate :: Int -> String -> String
    const replicate = n => s => s.repeat(n);

    // take :: Int -> [a] -> [a]
    // take :: Int -> String -> String
    const take = (n, xs) =>
        xs.constructor.constructor.name !== 'GeneratorFunction' ? (
            xs.slice(0, n)
        ) : [].concat.apply([], Array.from({
            length: n
        }, () => {
            const x = xs.next();
            return x.done ? [] : [x.value];
        }));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = f => xs => ys => {
        const
            lng = Math.min(length(xs), length(ys)),
            as = take(lng, xs),
            bs = take(lng, ys);
        return Array.from({
            length: lng
        }, (_, i) => f(as[i])(bs[i]));
    };

    // MAIN -----------------------------------------------
    return main();
})();
```

```txt
█████████
█ ██ ██ █
█████████
███   ███
█ █   █ █
███   ███
█████████
█ ██ ██ █
█████████

```



## Kotlin



### ASCII Art Version

```scala
// version 1.1.2

fun inCarpet(x: Int, y: Int): Boolean {
    var xx = x
    var yy = y
    while (xx != 0 && yy != 0) {
        if (xx % 3 == 1 && yy % 3 == 1) return false
        xx /= 3
        yy /= 3
    }
    return true
}

fun carpet(n: Int) {
    val power = Math.pow(3.0, n.toDouble()).toInt()
    for(i in 0 until power) {
        for(j in 0 until power) print(if (inCarpet(i, j)) "*" else " ")
        println()
    }
}

fun main(args: Array<String>) = carpet(3)
```


```txt

***************************
* ** ** ** ** ** ** ** ** *
***************************
***   ******   ******   ***
* *   * ** *   * ** *   * *
***   ******   ******   ***
***************************
* ** ** ** ** ** ** ** ** *
***************************
*********         *********
* ** ** *         * ** ** *
*********         *********
***   ***         ***   ***
* *   * *         * *   * *
***   ***         ***   ***
*********         *********
* ** ** *         * ** ** *
*********         *********
***************************
* ** ** ** ** ** ** ** ** *
***************************
***   ******   ******   ***
* *   * ** *   * ** *   * *
***   ******   ******   ***
***************************
* ** ** ** ** ** ** ** ** *
***************************

```



### Graphical Animated Version

```scala
// version 1.1.2

import java.awt.*
import javax.swing.*

public class SierpinskiCarpet : JPanel() {
    private val dim = 513
    private val margin = 20
    private var limit = dim

    init {
        val size = dim + 2 * margin
        preferredSize = Dimension(size, size)
        background = Color.blue
        foreground = Color.yellow
        Timer(2000) {
            limit /= 3
            if (limit <= 3) limit = dim
            repaint()
        }.start()
    }

    private fun drawCarpet(g: Graphics2D, x: Int, y: Int, s: Int) {
        var size = s
        if (s < limit) return
        size /= 3
        for (i in 0 until 9) {
            if (i == 4) {
                g.fillRect(x + size, y + size, size, size)
            }
            else {
                drawCarpet(g, x + (i % 3) * size, y + (i / 3) * size, size)
            }
        }
    }

    override fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
        g.translate(margin, margin)
        drawCarpet(g, 0, 0, dim)
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        f.defaultCloseOperation = JFrame.EXIT_ON_CLOSE
        f.title = "Sierpinski Carpet"
        f.isResizable = false
        f.add(SierpinskiCarpet(), BorderLayout.CENTER)
        f.pack()
        f.setLocationRelativeTo(null)
        f.isVisible = true
    }
}
```



## jq

jq does Cartesian products, so the heart of the matter is the line:

<tt>inCarpet( range(0; $power) ; range(0; $power), -1 )</tt>

This is like a "for" loop within a "for" loop in C-like languages, because range(0;n) generates a stream of n integers beginning at 0. The -1 is used to signal that a newline character is required.

```jq

def inCarpet(x; y):
  x as $x | y as $y |
  if $x == -1 or $y == -1 then "\n"
  elif $x == 0 or $y == 0 then "*"
  elif ($x % 3) == 1 and ($y % 3) == 1 then " "
  else inCarpet($x/3 | floor; $y/3 | floor)
  end;

def ipow(n):
  . as $in | reduce range(0;n) as $i (1; . * $in);

def carpet(n):
  (3|ipow(n)) as $power
  | [ inCarpet( range(0; $power) ; range(0; $power), -1 )]
  | join("") ;


carpet(3)
```
The following command produces the required pattern, and so the output is not repeated here:

```sh
jq -n -r -c -f sierpinski.jq
```



## Julia

```julia
function sierpinski(n::Integer, token::AbstractString="*")
    x = fill(token, 1, 1)
    for _ in 1:n
        t = fill(" ", size(x))
        x = [x x x; x t x; x x x]
    end
    return x
end

function printsierpinski(m::Matrix)
    for r in 1:size(m, 1)
        println(join(m[r, :]))
    end
end

sierpinski(2, "#") |> printsierpinski
```



## Liberty BASIC


```lb
NoMainWin
WindowWidth  = 508
WindowHeight = 575
Open "Sierpinski Carpets" For Graphics_nsb_nf As #g
#g "Down; TrapClose [halt]"

'labels
#g "Place  90  15;\Order 0"
#g "Place 340  15;\Order 1"
#g "Place  90 286;\Order 2"
#g "Place 340 286;\Order 3"
'carpets
Call carpet   5,  20, 243, 0
Call carpet 253,  20, 243, 1
Call carpet   5, 293, 243, 2
Call carpet 253, 293, 243, 3
#g "Flush"
Wait

[halt]
Close #g
End

Sub carpet x, y, size, order
    #g "Color 0 0 128; BackColor 0 0 128"
    #g "Place ";x;" ";y
    #g "BoxFilled ";x+size-1;" ";y+size-1
    #g "Color white; BackColor white"
    side = Int(size/3)
    newX = x+side
    newY = y+side
    #g "Place ";newX;" ";newY
    #g "BoxFilled ";newX+side-1;" ";newY+side-1
    order = order - 1
    If order > -1 Then
        Call carpet newX-side, newY-side+1, side, order
        Call carpet newX,      newY-side+1, side, order
        Call carpet newX+side, newY-side+1, side, order
        Call carpet newX+side, newY,        side, order
        Call carpet newX+side, newY+side,   side, order
        Call carpet newX,      newY+side,   side, order
        Call carpet newX-side, newY+side,   side, order
        Call carpet newX-side, newY,        side, order
    End If
End Sub
```



## Mathematica

Replace a empty spot with a 3x3 empty matrix, and replace a full spot with an empty spot surrounded by 8 full spots:

```Mathematica
full={{1,1,1},{1,0,1},{1,1,1}}
empty={{0,0,0},{0,0,0},{0,0,0}}
n=3;
Grid[Nest[ArrayFlatten[#/.{0->empty,1->full}]&,{{1}},n]//.{0->" ",1->"#"}]
```



## MATLAB


```MATLAB
n = 3;
c = string('#');
for k = 1 : n
  c = [c + c + c, c + c.replace('#', ' ') + c, c + c + c];
end
disp(c.join(char(10)))
```

```txt
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
#########         #########
# ## ## #         # ## ## #
#########         #########
###   ###         ###   ###
# #   # #         # #   # #
###   ###         ###   ###
#########         #########
# ## ## #         # ## ## #
#########         #########
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

numeric digits 1000
runSample(arg)
return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method runSample(arg) public static
  DARK_SHADE = '\u2593'
  parse arg ordr filr .
  if ordr = '' | ordr = '.' then ordr = 3
  if filr = '' | filr = '.' then filler = DARK_SHADE
  else                           filler = filr
  drawSierpinskiCarpet(ordr, filler)
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method drawSierpinskiCarpet(ordr, filler = Rexx '@') public static binary
  x = long
  y = long
  powr = 3 ** ordr
  loop x = 0 to powr - 1
    loop y = 0 to powr - 1
      if isSierpinskiCarpetCellFilled(x, y) then cell = filler
      else                                       cell = ' '
      say cell'\-'
      end y
      say
    end x
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method isSierpinskiCarpetCellFilled(x = long, y = long) public static binary returns boolean
  isTrue  = boolean (1 == 1)
  isFalse = \isTrue
  isFilled = isTrue
  loop label edge while x \= 0 & y \= 0
    if x // 3 = 1 & y // 3 = 1 then do
      isFilled = isFalse
      leave edge
      end
    x = x % 3
    y = y % 3
    end edge
  return isFilled

```

Sample shown with order "2".

```txt

▓▓▓▓▓▓▓▓▓
▓ ▓▓ ▓▓ ▓
▓▓▓▓▓▓▓▓▓
▓▓▓   ▓▓▓
▓ ▓   ▓ ▓
▓▓▓   ▓▓▓
▓▓▓▓▓▓▓▓▓
▓ ▓▓ ▓▓ ▓
▓▓▓▓▓▓▓▓▓

```



## Nim

```nim
proc `^`*(base: int, exp: int): int =
  var (base, exp) = (base, exp)
  result = 1

  while exp != 0:
    if (exp and 1) != 0:
      result *= base
    exp = exp shr 1
    base *= base

proc inCarpet(x, y): bool =
  var x = x
  var y = y
  while true:
    if x == 0 or y == 0:
      return true
    if x mod 3 == 1 and y mod 3 == 1:
      return false

    x = x div 3
    y = y div 3

proc carpet(n) =
  for i in 0 .. <(3^n):
    for j in 0 .. <(3^n):
      if inCarpet(i, j):
        stdout.write "* "
      else:
        stdout.write "  "
    echo ""

carpet(3)
```

Output:

```txt
* * * * * * * * * * * * * * * * * * * * * * * * * * *
*   * *   * *   * *   * *   * *   * *   * *   * *   *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
* * *       * * * * * *       * * * * * *       * * *
*   *       *   * *   *       *   * *   *       *   *
* * *       * * * * * *       * * * * * *       * * *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
*   * *   * *   * *   * *   * *   * *   * *   * *   *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * *                   * * * * * * * * *
*   * *   * *   *                   *   * *   * *   *
* * * * * * * * *                   * * * * * * * * *
* * *       * * *                   * * *       * * *
*   *       *   *                   *   *       *   *
* * *       * * *                   * * *       * * *
* * * * * * * * *                   * * * * * * * * *
*   * *   * *   *                   *   * *   * *   *
* * * * * * * * *                   * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
*   * *   * *   * *   * *   * *   * *   * *   * *   *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
* * *       * * * * * *       * * * * * *       * * *
*   *       *   * *   *       *   * *   *       *   *
* * *       * * * * * *       * * * * * *       * * *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
*   * *   * *   * *   * *   * *   * *   * *   * *   *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
```



## Objeck

```objeck
class SierpinskiCarpet {
  function : Main(args : String[]) ~ Nil {
    Carpet(3);
  }

  function : InCarpet(x : Int, y : Int) ~ Bool {
    while(x<>0 & y<>0) {
      if(x % 3 = 1 & y % 3 = 1) {
        return false;
      };

      x /= 3;
      y /= 3;
    };

    return true;
  }

  function : Carpet(n : Int) ~ Nil {
    power := 3.0->Power(n->As(Float));
    for(i := 0; i < power; i+=1;) {
      for(j := 0; j < power; j+=1;) {
        c := InCarpet(i, j) ? '*' : ' ';
        c->Print();
      };
      IO.Console->PrintLine();
    };
  }
}
```



## OCaml


```ocaml
let rec in_carpet x y =
  if x = 0 || y = 0 then true
  else if x mod 3 = 1 && y mod 3 = 1 then false
  else in_carpet (x / 3) (y / 3)

(* I define my own operator for integer exponentiation *)
let rec (^:) a b =
  if b = 0 then 1
  else if b mod 2 = 0 then let x = a ^: (b / 2) in x * x
  else a * (a ^: (b - 1))

let carpet n =
  for i = 0 to (3 ^: n) - 1 do
    for j = 0 to (3 ^: n) - 1 do
      print_char (if in_carpet i j then '*' else ' ')
    done;
    print_newline ()
  done
```

```ocaml
let nextCarpet carpet =
  List.map (fun x -> x ^ x ^ x) carpet @
  List.map (fun x -> x ^ String.make (String.length x) ' ' ^ x) carpet @
  List.map (fun x -> x ^ x ^ x) carpet

let rec sierpinskiCarpet n =
  let rec aux n carpet =
    if n = 0 then carpet
             else aux (n-1) (nextCarpet carpet)
  in
  aux n ["#"]

let () =
  List.iter print_endline (sierpinskiCarpet 3)
```



## Oforth



```Oforth
: carpet(n)
| dim i j k |
   3 n pow ->dim

   0 dim 1 - for: i [
      0 dim 1 - for: j [
          dim 3 / ->k
          while(k) [
             i k 3 * mod k / 1 == j k 3 * mod k / 1 == and ifTrue: [ break ]
             k 3 / ->k
             ]
          k ifTrue: [ " " ] else: [ "#" ] print
          ]
      printcr
      ] ;
```



## Order

Since the C Preprocessor cannot print newlines, this Order program produces a string for a simple C program to print:

```c
#include <order/interpreter.h>

#define ORDER_PP_DEF_8in_carpet ORDER_PP_FN( \
8fn(8X, 8Y, \
    8if(8or(8is_0(8X), 8is_0(8Y)), \
        8true, \
        8let((8Q, 8quotient(8X, 3)) \
             (8R, 8remainder(8X, 3)) \
             (8S, 8quotient(8Y, 3)) \
             (8T, 8remainder(8Y, 3)), \
             8and(8not(8and(8equal(8R, 1), 8equal(8T, 1))), \
                  8in_carpet(8Q, 8S))))) )

#define ORDER_PP_DEF_8carpet ORDER_PP_FN( \
8fn(8N, \
    8lets((8R, 8seq_iota(0, 8pow(3, 8N))) \
          (8G, 8seq_map(8fn(8Y, 8seq_map(8fn(8X, 8pair(8X, 8Y)), \
                                         8R)), \
                        8R)), \
           8seq_map(8fn(8S, 8seq_map(8fn(8P, 8apply(8in_carpet, 8P)), \
                                     8S)), \
                    8G))) )

#define ORDER_PP_DEF_8carpet_to_string ORDER_PP_FN( \
8fn(8C, \
    8seq_fold( \
      8fn(8R, 8S, \
          8adjoin(8R, \
                  8seq_fold(8fn(8P, 8B, 8adjoin(8P, 8if(8B, 8("#"), 8(" ")))), \
                            8nil, 8S), \
                  8("\n"))), \
      8nil, 8C)) )

#include <stdio.h>

int main(void) {
	printf(ORDER_PP( 8carpet_to_string(8carpet(3)) ));
	return 0;
}
```


(This example may take a long time to compile: change the <code>8carpet</code> parameter to 2 for a much quicker compile time and a smaller graphic.)

The <code>8carpet</code> function creates a 2-dimensional array of boolean values (i.e. an internal representation of the Sierpinski carpet), and the <code>8carpet_to_string</code> function then converts this to a C string.

Since the # and space characters are rather difficult for the C Preprocessor to handle (as a result Order can only print them, not concatenate them), this program takes advantage of the fact that two string literals side-by-side in C must be interpreted as a single string. An alternative method might be to pass the carpet to a print function, that prints the carpet string and returns nil, and then use a regular C macro to stringify the entire result of the Order program.


## Oz


```oz
declare
  %% A carpet is a list of lines.
  fun {NextCarpet Carpet}
     {Flatten
      [{Map Carpet XXX}
       {Map Carpet X_X}
       {Map Carpet XXX}
      ]}
  end

  fun {XXX X} X#X#X end
  fun {X_X X} X#{Spaces {VirtualString.length X}}#X end
  fun {Spaces N} if N == 0 then nil else & |{Spaces N-1} end end

  fun lazy {Iterate F X}
     X|{Iterate F {F X}}
  end

  SierpinskiCarpets = {Iterate NextCarpet ["#"]}
in
  %% print all lines of the Sierpinski carpet of order 3
  {ForAll {Nth SierpinskiCarpets 4} System.showInfo}
```



## PARI/GP

[[File:SierpC5.png|right|thumb|Output SierpC5.png]]
[[File:SierpC5a.png|right|thumb|Output SierpC5a.png]]


### Plotting helper functions

Note: wrtmat() can be found here on RC [[Brownian_tree#PARI.2FGP| Brownian tree page]].

```parigp

\\ Improved simple plotting using matrix mat (color and scaling added).
\\ Matrix should be filled with 0/1. 7/6/16 aev
iPlotmat(mat,clr)={
  my(xz=#mat[1,],yz=#mat[,1],vx=List(),vy=vx,xmin,xmax,ymin,ymax,c=0.625);
  for(i=1,yz, for(j=1,xz, if(mat[i,j]==0, next, listput(vx,i); listput(vy,j))));
  xmin=listmin(vx); xmax=listmax(vx); ymin=listmin(vy); ymax=listmax(vy);
  plotinit(0); plotcolor(0,clr);
  plotscale(0, xmin,xmax,ymin,ymax);
  plotpoints(0, Vec(vx)*c,Vec(vy));
  plotdraw([0,xmin,ymin]);
  print(" *** matrix: ",xz,"x",yz,", ",#vy," DOTS");
}
\\ iPlotV2(): Improved plotting from a file written by the wrtmat(). (color added)
\\ Saving possibly huge generation time if re-plotting needed.
iPlotV2(fn, clr)={
  my(F,nf,vx=List(),vy=vx,Vr,xmin,xmax,ymin,ymax,c=0.625);
  F=readstr(fn); nf=#F;
  print(" *** Plotting from: ", fn, " - ", nf, " DOTS");
  for(i=1,nf, Vr=stok(F[i]," "); listput(vx,eval(Vr[1])); listput(vy,eval(Vr[2])));
  xmin=listmin(vx); xmax=listmax(vx); ymin=listmin(vy); ymax=listmax(vy);
  plotinit(0); plotcolor(0,clr);
  plotscale(0, xmin,xmax,ymin,ymax);
  plotpoints(0, Vec(vx)*c,Vec(vy));
  plotdraw([0,xmin,ymin]);
}
\\ Are x,y inside Sierpinski carpet? (1-yes, 0-no) 6/10/16 aev
inSC(x,y)={
  while(1, if(!x||!y,return(1));
    if(x%3==1&&y%3==1, return(0));
    x\=3; y\=3;);\\wend
}

```



### Sierpinski carpet fractal.


```parigp

\\ Sierpinski carpet fractal (n - order, clr - color, dfn - data file name)
\\ 6/10/16, upgraded 11/29/16 aev
pSierpinskiC(n, clr=5, dfn="")={
  my(n3=3^n-1,M,pf=n>=5);
  if(pf, M=matrix(n3+1,n3+1));
  for(i=0,n3, for(j=0,n3,
    if(inSC(i,j),
      if(pf, M[i+1,j+1]=1, print1("* ")), if(!pf, print1("  ")));
    ); if(!pf, print(""));
  );\\fend i
  if(!pf, return(0));
  if(dfn=="", c, wrtmat(M, dfn));
}
{\\ Test:
pSierpinskiC(3);
pSierpinskiC(5,14); \\ SierpC5.png, color - navy
}
{pSierpinskiC(5,,"c:\\pariData\\SC5.dat");
iPlotV2("c:\\pariData\\SC5.dat",10);} \\ SierpC5a.png, color - dark-green

```


```txt

> pSierpinskiC(3);
* * * * * * * * * * * * * * * * * * * * * * * * * * *
*   * *   * *   * *   * *   * *   * *   * *   * *   *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
* * *       * * * * * *       * * * * * *       * * *
*   *       *   * *   *       *   * *   *       *   *
* * *       * * * * * *       * * * * * *       * * *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
*   * *   * *   * *   * *   * *   * *   * *   * *   *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
* * * * * * * * *                   * * * * * * * * *
*   * *   * *   *                   *   * *   * *   *
* * * * * * * * *                   * * * * * * * * *
* * *       * * *                   * * *       * * *
*   *       *   *                   *   *       *   *
* * *       * * *                   * * *       * * *
* * * * * * * * *                   * * * * * * * * *
*   * *   * *   *                   *   * *   * *   *
* * * * * * * * *                   * * * * * * * * *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
*   * *   * *   * *   * *   * *   * *   * *   * *   *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
* * *       * * * * * *       * * * * * *       * * *
*   *       *   * *   *       *   * *   *       *   *
* * *       * * * * * *       * * * * * *       * * *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
*   * *   * *   * *   * *   * *   * *   * *   * *   *
* * * * * * * * * * * * * * * * * * * * * * * * * * *
> pSierpinskiC(5,14); \\ SierpC5.png, color navy
   *** matrix: 243x243, 32768 DOTS
> {pSierpinskiC(5,,"c:\\pariData\\SC5.dat");
iPlotV2("c:\\pariData\\SC5.dat",10);} \\ SierpC5a.png, color - dark-green
 *** matrix(243x243) 32768 DOTS put in c:\pariData\SC5.dat
 *** Plotting from: c:\pariData\SC5.dat - 32768 DOTS

```



## Pascal


```pascal
Program SierpinskiCarpet;

uses
  Math;

function In_carpet(a, b: longint): boolean;

  var
    x, y: longint;

  begin
    x := a;
    y := b;
    while true do
    begin
      if (x = 0) or (y = 0) then
      begin
        In_carpet := true;
        break;
      end
      else
        if ( (x mod 3) = 1 ) and ((y mod 3) = 1) then
        begin
          In_carpet := false;
          break;
        end;
      x := x div 3;
      y := y div 3;
    end;
  end;

procedure Carpet(n: integer);

  var
    i, j: longint;

  begin
    for i := 0 to 3**n - 1 do
    begin
      for j := 0 to 3**n - 1 do
        if In_carpet(i, j) then
          write('*')
        else
          write(' ');
      writeln;
    end;
  end;

begin
  Carpet(3);
end.
```

```txt
:> ./SierpinskiCarpet
***************************
* ** ** ** ** ** ** ** ** *
***************************
***   ******   ******   ***
* *   * ** *   * ** *   * *
***   ******   ******   ***
***************************
* ** ** ** ** ** ** ** ** *
***************************
*********         *********
* ** ** *         * ** ** *
*********         *********
***   ***         ***   ***
* *   * *         * *   * *
***   ***         ***   ***
*********         *********
* ** ** *         * ** ** *
*********         *********
***************************
* ** ** ** ** ** ** ** ** *
***************************
***   ******   ******   ***
* *   * ** *   * ** *   * *
***   ******   ******   ***
***************************
* ** ** ** ** ** ** ** ** *
***************************

```



## Perl


```perl
my @c = '##';
@c = (map($_ x 3, @c), map($_.(' ' x length).$_, @c), map($_ x 3, @c))
        for 1 .. 3;
print join("\n", @c), "\n";
```



## Perl 6

```perl6
sub carpet
{
    (['#'], -> @c {
       [
        |@c.map({$_ x 3}),
        |@c.map({ $_ ~ $_.trans('#'=>' ') ~ $_}),
        |@c.map({$_ x 3})
       ]
    } ... *).map: { .join("\n") };
}

say carpet[3];

# Same as above, structured as an array bound to a sequence, with a separate sub for clarity.
sub weave ( @c ) {
   [
    |@c.map({ $_ x 3 }),
    |@c.map({ $_ ~ .trans( '#' => ' ' ) ~ $_ }),
    |@c.map({ $_ x 3 })
   ]
}

my @carpet = ( ['#'], &weave ... * ).map: { .join: "\n" };

say @carpet[3];

# Output of both versions matches task example.
```



## Phix

```Phix
constant order = 4

function InCarpet(atom x, atom y)
    while x!=0 and y!=0 do
        if floor(mod(x,3))=1 and floor(mod(y,3))=1 then
            return ' '
        end if
        x /= 3
        y /= 3
    end while
    return '#'
end function

for i=0 to power(3,order)-1 do
    for j=0 to power(3,order)-1 do
        puts(1,InCarpet(i,j))
    end for
    puts(1,'\n')
end for
```

<pre style="font-size: 2px">
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
# #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # #
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
#########         ##################         ##################         #########
# ## ## #         # ## ## ## ## ## #         # ## ## ## ## ## #         # ## ## #
#########         ##################         ##################         #########
###   ###         ###   ######   ###         ###   ######   ###         ###   ###
# #   # #         # #   # ## #   # #         # #   # ## #   # #         # #   # #
###   ###         ###   ######   ###         ###   ######   ###         ###   ###
#########         ##################         ##################         #########
# ## ## #         # ## ## ## ## ## #         # ## ## ## ## ## #         # ## ## #
#########         ##################         ##################         #########
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
# #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # #
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###########################                           ###########################
# ## ## ## ## ## ## ## ## #                           # ## ## ## ## ## ## ## ## #
###########################                           ###########################
###   ######   ######   ###                           ###   ######   ######   ###
# #   # ## #   # ## #   # #                           # #   # ## #   # ## #   # #
###   ######   ######   ###                           ###   ######   ######   ###
###########################                           ###########################
# ## ## ## ## ## ## ## ## #                           # ## ## ## ## ## ## ## ## #
###########################                           ###########################
#########         #########                           #########         #########
# ## ## #         # ## ## #                           # ## ## #         # ## ## #
#########         #########                           #########         #########
###   ###         ###   ###                           ###   ###         ###   ###
# #   # #         # #   # #                           # #   # #         # #   # #
###   ###         ###   ###                           ###   ###         ###   ###
#########         #########                           #########         #########
# ## ## #         # ## ## #                           # ## ## #         # ## ## #
#########         #########                           #########         #########
###########################                           ###########################
# ## ## ## ## ## ## ## ## #                           # ## ## ## ## ## ## ## ## #
###########################                           ###########################
###   ######   ######   ###                           ###   ######   ######   ###
# #   # ## #   # ## #   # #                           # #   # ## #   # ## #   # #
###   ######   ######   ###                           ###   ######   ######   ###
###########################                           ###########################
# ## ## ## ## ## ## ## ## #                           # ## ## ## ## ## ## ## ## #
###########################                           ###########################
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
# #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # #
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
#########         ##################         ##################         #########
# ## ## #         # ## ## ## ## ## #         # ## ## ## ## ## #         # ## ## #
#########         ##################         ##################         #########
###   ###         ###   ######   ###         ###   ######   ###         ###   ###
# #   # #         # #   # ## #   # #         # #   # ## #   # #         # #   # #
###   ###         ###   ######   ###         ###   ######   ###         ###   ###
#########         ##################         ##################         #########
# ## ## #         # ## ## ## ## ## #         # ## ## ## ## ## #         # ## ## #
#########         ##################         ##################         #########
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
# #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # #
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################

```



## PicoLisp

```PicoLisp
(de carpet (N)
   (let Carpet '("#")
      (do N
         (setq Carpet
            (conc
               (mapcar '((S) (pack S S S)) Carpet)
               (mapcar
                  '((S) (pack S (replace (chop S) "#" " ") S))
                  Carpet )
               (mapcar '((S) (pack S S S)) Carpet) ) ) ) ) )

(mapc prinl (carpet 3))
```



## PL/I


```PL/I

/* Sierpinski carpet */

Sierpinski_carpet: procedure options (main); /* 28 January 2013 */

  call carpet(3);

In_carpet: procedure (a, b) returns (bit(1));
  declare (a, b) fixed binary nonassignable;
  declare (x, y) fixed binary;
  declare (true value ('1'b), false value ('0'b)) bit (1);

   x = a ; y = b;
   do forever;
      if x = 0 | y = 0 then
         return (true);
      else if mod(x, 3) = 1 & mod(y, 3) = 1 then
         return (false);
      x = x / 3;
      y = y / 3;
   end;
end in_carpet;

Carpet: procedure (n);
  declare n fixed binary nonassignable;
  declare (i, j) fixed binary;

  do i = 0 to 3**n - 1;
     do j = 0 to 3**n - 1;
        if In_carpet(i, j) then
           put edit ('#') (a);
        else
           put edit (' ') (a);
     end;
     put skip;
  end;
end Carpet;
end Sierpinski_carpet;

```

The above is a translation of the Fortran version.
Output for n=3:

```txt

###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
#########         #########
# ## ## #         # ## ## #
#########         #########
###   ###         ###   ###
# #   # #         # #   # #
###   ###         ###   ###
#########         #########
# ## ## #         # ## ## #
#########         #########
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################

```



## PostScript


```PostScript
%!PS-Adobe-3.0
%%BoundingBox 0 0 300 300

/r { moveto 0 -1 1 0 0 1 3 { rlineto } repeat closepath fill } def
/serp { gsave
        3 1 roll translate
        1 3 div dup scale
        1 1 r
        dup 1 sub dup 0 eq not {
                0 0 0 1 0 2 1 0 1 2 2 0 2 1 2 2 17 -1 roll 8 { serp } repeat
        } if pop
        grestore
} def

300 300 scale 0 0 r 1 setgray

0 0 5 serp

pop showpage
%%EOF
```



## PowerShell

<h3>Text based solution</h3>
```PowerShell
function Draw-SierpinskiCarpet ( [int]$N )
    {
    $Carpet = @( '#' ) * [math]::Pow( 3, $N )
    ForEach ( $i in 1..$N )
        {
        $S = [math]::Pow( 3, $i - 1 )
        ForEach ( $Row in 0..($S-1) )
            {
            $Carpet[$Row+$S+$S] = $Carpet[$Row] * 3
            $Carpet[$Row+$S]    = $Carpet[$Row] + ( " " * $Carpet[$Row].Length ) + $Carpet[$Row]
            $Carpet[$Row]       = $Carpet[$Row] * 3
            }
        }
    $Carpet
    }

Draw-SierpinskiCarpet 3
```

```txt
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
#########         #########
# ## ## #         # ## ## #
#########         #########
###   ###         ###   ###
# #   # #         # #   # #
###   ###         ###   ###
#########         #########
# ## ## #         # ## ## #
#########         #########
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
```

<h3>Graphics based solution</h3>
```PowerShell
Function Draw-SierpinskiCarpet ( [int]$N )
    {
    #  Define form
    $Form = [System.Windows.Forms.Form]@{ Size = '300, 300' }
    $Form.Controls.Add(( $PictureBox = [System.Windows.Forms.PictureBox]@{ Size = $Form.ClientSize; Anchor = 'Top, Bottom, Left, Right' } ))

    #  Main code to draw Sierpinski carpet
    $Draw = {

        #  Create graphics objects to use
        $PictureBox.Image = ( $Canvas = New-Object System.Drawing.Bitmap ( $PictureBox.Size.Width, $PictureBox.Size.Height ) )
        $Graphics = [System.Drawing.Graphics]::FromImage( $Canvas )

        #  Draw single pixel
        $Graphics.FillRectangle( [System.Drawing.Brushes]::Black, 0, 0, 1, 1 )

        #  If N was not specified, use an N that will fill the form
        If ( -not $N ) { $N = [math]::Ceiling( [math]::Log( [math]::Max( $PictureBox.Size.Height, $PictureBox.Size.Width ) ) / [math]::Log( 3 ) ) }

        #  Define the shape of the fractal
        $P  = @( @( 0, 0 ), @( 0, 1 ), @( 0, 2 ) )
        $P += @( @( 1, 0 ),            @( 1, 2 ) )
        $P += @( @( 2, 0 ), @( 2, 1 ), @( 2, 2 ) )

        #  For each iteration
        ForEach ( $i in 0..$N )
            {
            #  Copy the result of the previous iteration
            $Copy = New-Object System.Drawing.TextureBrush ( $Canvas )

            #  Calulate the size of the copy
            $S = [math]::Pow( 3, $i )

            #  For each position in the next layer of the fractal
            ForEach ( $i in 1..7 )
                {
                #  Adjust the copy for the new location
                $Copy.TranslateTransform( - $P[$i-1][0] * $S + $P[$i][0] * $S, - $P[$i-1][1] * $S + $P[$i][1] * $S )

                #  Paste the copy of the previous iteration into the new location
                $Graphics.FillRectangle( $Copy, $P[$i][0] * $S, $P[$i][1] * $S, $S, $S )
                }
            }
        }

    #  Add the main drawing code to the appropriate events to be drawn when the form is first shown and redrawn when the form size is changed
    $Form.Add_Shown(  $Draw )
    $Form.Add_Resize( $Draw )

    #  Launch the form
    $Null = $Form.ShowDialog()
    }

Draw-SierpinskiCarpet 4
```

## PureBasic

```PureBasic
Procedure in_carpet(x,y)
  While x>0 And y>0
    If x%3=1 And y%3=1
      ProcedureReturn #False
    EndIf
    y/3: x/3
  Wend
  ProcedureReturn #True
EndProcedure

Procedure carpet(n)
  Define i, j, l=Pow(3,n)-1
  For i=0 To l
    For j=0 To l
      If in_carpet(i,j)
        Print("#")
      Else
        Print(" ")
      EndIf
    Next
    PrintN("")
  Next
EndProcedure
```



## Python

This inserts a space after every character; but this makes the spacing look better anyway.

```python
def in_carpet(x, y):
    while True:
        if x == 0 or y == 0:
            return True
        elif x % 3 == 1 and y % 3 == 1:
            return False

        x /= 3
        y /= 3

def carpet(n):
    for i in xrange(3 ** n):
        for j in xrange(3 ** n):
            if in_carpet(i, j):
                print '*',
            else:
                print ' ',
        print
```

This version is elegant:
```python
def sierpinski_carpet(n):
  carpet = ["#"]
  for i in xrange(n):
    carpet = [x + x + x for x in carpet] + \
             [x + x.replace("#"," ") + x for x in carpet] + \
             [x + x + x for x in carpet]
  return "\n".join(carpet)

print sierpinski_carpet(3)
```



We can also define a Sierpinski carpet weave declaratively, in terms of generic abstractions like '''zipWith''' and '''bind''':
```python
'''Iterations of the Sierpinski carpet'''

from itertools import chain, islice
from inspect import signature
from operator import add


# sierpinskiCarpet :: Int -> [String]
def sierpinskiCarpet(n):
    '''A string representing the nth
       iteration of a Sierpinski carpet.
    '''
    f = zipWith(add)
    g = flip(f)

    # weave :: [String] -> [String]
    def weave(xs):
        return bind([
            xs,
            [' ' * len(s) for s in xs],
            xs
        ])(compose(g(xs))(f(xs)))

    return index(
        iterate(weave)(['▓▓'])
    )(n)


# TEST ----------------------------------------------------
def main():
    '''Test iteration of the Sierpinski carpet'''

    levels = enumFromTo(0)(3)
    t = ' ' * (
        len(' -> ') +
        max(map(compose(len)(str), levels))
    )
    print(
        fTable(__doc__ + ':')(lambda x: '\n' + str(x))(
            lambda xs: xs[0] + '\n' + (
                unlines(map(lambda x: t + x, xs[1:])))
        )
        (sierpinskiCarpet)(levels)
    )


# GENERIC -------------------------------------------------

# bind (>>=) :: [a] -> (a -> [b]) -> [b]
def bind(xs):
    '''List monad injection operator.
       Two computations sequentially composed,
       with any value produced by the first
       passed as an argument to the second.'''
    return lambda f: list(chain.from_iterable(
        map(f, xs)
    ))


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# enumFromTo :: (Int, Int) -> [Int]
def enumFromTo(m):
    '''Integer enumeration from m to n.'''
    return lambda n: list(range(m, 1 + n))


# flip :: (a -> b -> c) -> b -> a -> c
def flip(f):
    '''The (curried or uncurried) function f with its
       arguments reversed.'''
    if 1 < len(signature(f).parameters):
        return lambda a, b: f(b, a)
    else:
        return lambda a: lambda b: f(b)(a)


# index (!!) :: [a] -> Int -> a
def index(xs):
    '''Item at given (zero-based) index.'''
    return lambda n: None if 0 > n else (
        xs[n] if (
            hasattr(xs, "__getitem__")
        ) else next(islice(xs, n, None))
    )


# iterate :: (a -> a) -> a -> Gen [a]
def iterate(f):
    '''An infinite list of repeated
       applications of f to x.
    '''
    def go(x):
        v = x
        while True:
            yield v
            v = f(v)
    return lambda x: go(x)


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.'''
    return '\n'.join(xs)


# zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
def zipWith(f):
    '''A list constructed by zipping with a
       custom function, rather than with the
       default tuple constructor.'''
    return lambda xs: lambda ys: (
        map(f, xs, ys)
    )


# OUTPUT FORMATTING ---------------------------------------

# fTable :: String -> (a -> String) ->
#                     (b -> String) -> (a -> b) -> [a] -> String
def fTable(s):
    '''Heading -> x display function -> fx display function ->
                     f -> xs -> tabular string.
    '''
    def go(xShow, fxShow, f, xs):
        ys = [xShow(x) for x in xs]
        w = max(map(len, ys))
        return s + '\n' + '\n'.join(map(
            lambda x, y: y.rjust(w, ' ') + ' -> ' + fxShow(f(x)),
            xs, ys
        ))
    return lambda xShow: lambda fxShow: lambda f: lambda xs: go(
        xShow, fxShow, f, xs
    )


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Iterations of the Sierpinski carpet:

0 -> ▓▓


1 -> ▓▓▓▓▓▓
     ▓▓  ▓▓
     ▓▓▓▓▓▓

2 -> ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓▓▓▓▓      ▓▓▓▓▓▓
     ▓▓  ▓▓      ▓▓  ▓▓
     ▓▓▓▓▓▓      ▓▓▓▓▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓

3 -> ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓▓▓▓▓      ▓▓▓▓▓▓▓▓▓▓▓▓      ▓▓▓▓▓▓▓▓▓▓▓▓      ▓▓▓▓▓▓
     ▓▓  ▓▓      ▓▓  ▓▓▓▓  ▓▓      ▓▓  ▓▓▓▓  ▓▓      ▓▓  ▓▓
     ▓▓▓▓▓▓      ▓▓▓▓▓▓▓▓▓▓▓▓      ▓▓▓▓▓▓▓▓▓▓▓▓      ▓▓▓▓▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓                  ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓                  ▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓                  ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓▓▓▓▓      ▓▓▓▓▓▓                  ▓▓▓▓▓▓      ▓▓▓▓▓▓
     ▓▓  ▓▓      ▓▓  ▓▓                  ▓▓  ▓▓      ▓▓  ▓▓
     ▓▓▓▓▓▓      ▓▓▓▓▓▓                  ▓▓▓▓▓▓      ▓▓▓▓▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓                  ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓                  ▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓                  ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓▓▓▓▓      ▓▓▓▓▓▓▓▓▓▓▓▓      ▓▓▓▓▓▓▓▓▓▓▓▓      ▓▓▓▓▓▓
     ▓▓  ▓▓      ▓▓  ▓▓▓▓  ▓▓      ▓▓  ▓▓▓▓  ▓▓      ▓▓  ▓▓
     ▓▓▓▓▓▓      ▓▓▓▓▓▓▓▓▓▓▓▓      ▓▓▓▓▓▓▓▓▓▓▓▓      ▓▓▓▓▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
     ▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓▓▓  ▓▓
     ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓
```



## R


### Version #1.

Note: Find plotmat() here on RC [[User:AnatolV/Helper_Functions| R Helper Functions page]].
[[File:SierpCRo5.png|200px|right|thumb|Output SierpCRo5.png]]

```r

## Are x,y inside Sierpinski carpet (and where)? (1-yes, 0-no)
inSC <- function(x, y) {
  while(TRUE) {
    if(!x||!y) {return(1)}
    if(x%%3==1&&y%%3==1) {return(0)}
    x=x%/%3; y=y%/%3;
  } return(0);
}
## Plotting Sierpinski carpet fractal. aev 4/1/17
## ord - order, fn - file name, ttl - plot title, clr - color
pSierpinskiC <- function(ord, fn="", ttl="", clr="navy") {
  m=640; abbr="SCR"; dftt="Sierpinski carpet fractal";
  n=3^ord-1; M <- matrix(c(0), ncol=n, nrow=n, byrow=TRUE);
  cat(" *** START", abbr, date(), "\n");
  if(fn=="") {pf=paste0(abbr,"o", ord)} else {pf=paste0(fn, ".png")};
  if(ttl!="") {dftt=ttl}; ttl=paste0(dftt,", order ", ord);
  cat(" *** Plot file:", pf,".png", "title:", ttl, "\n");
  for(i in 0:n) {
    for(j in 0:n) {if(inSC(i,j)) {M[i,j]=1}
  }}
  plotmat(M, pf, clr, ttl);
  cat(" *** END", abbr, date(), "\n");
}
## Executing:
pSierpinskiC(5);

```

```txt

> pSierpinskiC(5);
 *** START SCR Sun Apr 02 12:39:21 2017
 *** Plot file: SCRo5 .png title: Sierpinski carpet fractal, order 5
 *** Matrix( 242 x 242 ) 32283 DOTS
 *** END SCR Sun Apr 02 12:39:28 2017

```


### Version #2.

Note: Find plotmat() here on RC [[User:AnatolV/Helper_Functions| R Helper Functions page]].
[[File:SierpCR2o5.png|200px|right|thumb|Output SierpCR2o5.png]]

```r

## Plotting Sierpinski carpet fractal v.2. aev 4/2/17
## ord - order, fn - file name, ttl - plot title, clr - color
pSierpinskiC2 <- function(ord, fn="", ttl="", clr="brown") {
  m=640; abbr="SCR2"; dftt="Sierpinski carpet fractal v.2";
  cat(" *** START", abbr, date(), "\n");
  if(fn=="") {pf=paste0(abbr,"o", ord)} else {pf=paste0(fn, ".png")};
  if(ttl!="") {dftt=ttl}; ttl=paste0(dftt,", order ", ord);
  cat(" *** Plot file:", pf,".png", "title:", ttl, "\n");
  S = matrix(1,1,1);
  for (i in 1:ord) {
    Q = cbind(S,S,S); R = cbind(S,0*S,S); S = rbind(Q,R,Q);
  }
  plotmat(S, pf, clr, ttl);
  cat(" *** END", abbr, date(), "\n");
}
## Executing:
pSierpinskiC2(5);

```

```txt

> pSierpinskiC2(5);
 *** START SCR2 Sun Apr 02 14:44:17 2017
 *** Plot file: SCR2o5 .png title: Sierpinski carpet fractal v.2, order 5
 *** Matrix( 243 x 243 ) 32768 DOTS
 *** END SCR2 Sun Apr 02 14:44:24 2017

```



## REXX


```rexx
/*REXX program draws any order Sierpinski carpet (order 20 would be ≈ 3.4Gx3.4G carpet).*/
parse arg N char .                               /*get the  order  of the carpet.       */
if N==''  |  N==","  then    N= 3                /*if none specified, then assume  3.   */
if char==''          then char= "*"              /*use the default of an asterisk  (*). */
if length(char)==2   then char= x2c(char)        /*it was specified in hexadecimal char.*/
if length(char)==3   then char= d2c(char)        /* "  "      "      " decimal character*/
width= linesize()                                /*the width of the terminal screen.    */
if N>18  then numeric digits 100                 /*just in case the user went  ka─razy. */
nnn= 3**N                                        /* [↓]  NNN  is the  cube of  N.       */

  do   j=0  for nnn;    z=                       /*Z:  will be the line to be displayed.*/
    do k=0  for nnn;   jj= j;   kk= k;   x= char
      do  while  jj\==0  &  kk\==0               /*one symbol for a  not (¬)  is a   \  */
      if jj//3==1  then if kk//3==1  then do     /*in REXX:    //  ≡  division remainder*/
                                          x= ' ' /*use a blank for this display line.   */
                                          leave  /*LEAVE   terminates this   DO  WHILE. */
                                          end
      jj= jj % 3;          kk= kk % 3            /*in REXX:     %  ≡  integer division. */
      end   /*while*/

    z= z || x                                    /*X      is either   black  or  white. */
    end     /*k*/                                /* [↑]    "    "       "     "  blank. */

  if length(z)<width  then say z                 /*display the line if it fits on screen*/
  call lineout 'Sierpinski.'N, z                 /*also, write the line to a (disk) file*/
  end       /*j*/                                /*stick a fork in it,  we're all done. */
```

This REXX program makes use of   '''linesize'''   REXX program (or BIF) which is used to determine the screen width (or linesize) of the terminal (console).

Some REXXes don't have this BIF, so the   '''linesize.rex'''   REXX program is included here   ──►   [[LINESIZE.REX]].


```txt

***************************
* ** ** ** ** ** ** ** ** *
***************************
***   ******   ******   ***
* *   * ** *   * ** *   * *
***   ******   ******   ***
***************************
* ** ** ** ** ** ** ** ** *
***************************
*********         *********
* ** ** *         * ** ** *
*********         *********
***   ***         ***   ***
* *   * *         * *   * *
***   ***         ***   ***
*********         *********
* ** ** *         * ** ** *
*********         *********
***************************
* ** ** ** ** ** ** ** ** *
***************************
***   ******   ******   ***
* *   * ** *   * ** *   * *
***   ******   ******   ***
***************************
* ** ** ** ** ** ** ** ** *
***************************

```

```txt

█████████
█ ██ ██ █
█████████
███   ███
█ █   █ █
███   ███
█████████
█ ██ ██ █
█████████

```



## Racket


```Racket

#lang racket
(define (carpet n)
  (if (zero? n)
    '("#")
    (let* ([prev   (carpet (sub1 n))]
           [spaces (regexp-replace* #rx"#" (car prev) " ")])
      (append (map (λ(x) (~a x x x)) prev)
              (map (λ(x) (~a x spaces x)) prev)
              (map (λ(x) (~a x x x)) prev)))))
(for-each displayln (carpet 3))

```



## Ring


```ring

load "guilib.ring"

new qapp
        {
        win1 = new qwidget() {
                   etwindowtitle("drawing using qpainter")
                   setgeometry(100,100,500,500)
               label1 = new qlabel(win1) {
                        setgeometry(10,10,400,400)
                        settext("")
               }
               new qpushbutton(win1) {
                   setgeometry(200,450,100,30)
                   settext("draw")
                   setclickevent("draw()")
               }
               show()
         }
         exec()
         }

func draw
     p1 = new qpicture()
          color = new qcolor() {
          setrgb(0,0,255,255)
     }
     pen = new qpen() {
           setcolor(color)
           setwidth(1)
     }
     new qpainter() {
         begin(p1)
         setpen(pen)

         order = 3
         side = pow(3,order)
         for y = 0 to side-1
             for x = 0 to side-1
                 if carpet(self,x,y)
                    drawpoint(x*16,y*16+15)
                    drawpoint(x*16+1,y*16+16)
                    drawpoint(x*16+2,y*16+17) ok
             next
         next

         endpaint()
        }
        label1 { setpicture(p1) show() }

        func carpet myObj,x,y
             myObj{while x!=0 and y!=0
                         if x % 3 = 1 if y % 3 = 1 return false ok ok
                         x = floor(x/3)
                         y = floor(y/3)
                   end
                   return true}

```

Output:
[[File:CalmoSoftCarpet.jpg]]


## Ruby

```ruby
def sierpinski_carpet(n)
  carpet = ["#"]
  n.times do
    carpet = carpet.collect {|x| x + x + x} +
             carpet.collect {|x| x + x.tr("#"," ") + x} +
             carpet.collect {|x| x + x + x}
  end
  carpet
end

4.times{|i| puts "\nN=#{i}", sierpinski_carpet(i)}
```


<pre style="height: 64ex; overflow: scroll">
N=0
#

N=1
###
# #
###

N=2
#########
# ## ## #
#########
###   ###
# #   # #
###   ###
#########
# ## ## #
#########

N=3
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
#########         #########
# ## ## #         # ## ## #
#########         #########
###   ###         ###   ###
# #   # #         # #   # #
###   ###         ###   ###
#########         #########
# ## ## #         # ## ## #
#########         #########
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################

```



## Rust

```rust
fn main() {
    for i in 0..4 {
        println!("\nN={}", i);
        println!("{}", sierpinski_carpet(i));
    }
}

fn sierpinski_carpet(n: u32) -> String {
    let mut carpet = vec!["#".to_string()];
    for _ in 0..n {
        let mut top: Vec<_> = carpet.iter().map(|x| x.repeat(3)).collect();
        let middle: Vec<_> = carpet
            .iter()
            .map(|x| x.to_string() + &x.replace("#", " ") + x)
            .collect();
        let bottom = top.clone();

        top.extend(middle);
        top.extend(bottom);
        carpet = top;
    }
    carpet.join("\n")
}


```



## Scala

```scala
def nextCarpet(carpet: List[String]): List[String] = (
  carpet.map(x => x + x + x) :::
  carpet.map(x => x + x.replace('#', ' ') + x) :::
  carpet.map(x => x + x + x))

def sierpinskiCarpets(n: Int) = (Iterator.iterate(List("#"))(nextCarpet) drop n next) foreach println
```



## Scheme


```scheme
(define (carpet n)
  (define (in-carpet? x y)
    (cond ((or (zero? x) (zero? y))
              #t)
          ((and (= 1 (remainder x 3)) (= 1 (remainder y 3)))
              #f)
          (else
              (in-carpet? (quotient x 3) (quotient y 3)))))

  (do ((i 0 (+ i 1))) ((not (< i (expt 3 n))))
    (do ((j 0 (+ j 1))) ((not (< j (expt 3 n))))
      (display (if (in-carpet? i j)
                   #\*
                   #\space)))
    (newline)))
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func boolean: inCarpet (in var integer: x, in var integer: y) is func
  result
    var boolean: result is TRUE;
  begin
    while result and x <> 0 and y <> 0 do
      if x rem 3 = 1 and y rem 3 = 1 then
        result := FALSE;
      else
        x := x div 3;
        y := y div 3;
      end if;
    end while;
  end func;

const proc: carpet (in integer: n) is func
  local
    var integer: i is 0;
    var integer: j is 0;
  begin
    for i range 0 to pred(3 ** n) do
      for j range 0 to pred(3 ** n) do
        if inCarpet(i, j) then
          write("#");
        else
          write(" ");
        end if;
      end for;
      writeln;
    end for;
  end func;

const proc: main is func
  begin
    carpet(3);
  end func;
```



## Sidef


```ruby
var c = ['##']
3.times {
    c = (c.map{|x| x * 3 }             +
         c.map{|x| x + ' '*x.len + x } +
         c.map{|x| x * 3 })
}
say c.join("\n")
```



## Sinclair ZX81 BASIC

Works with the unexpanded (1k RAM) ZX81. A screenshot of the output is [http://www.edmundgriffiths.com/zx81sierpcarpet.jpg here].

```basic
 10 LET O=3
 20 LET S=3**O
 30 FOR I=0 TO S-1
 40 FOR J=0 TO S-1
 50 LET X=J
 60 LET Y=I
 70 GOSUB 120
 80 IF C THEN PLOT J,I
 90 NEXT J
100 NEXT I
110 GOTO 190
120 LET C=0
130 IF X-INT (X/3)*3=1 AND Y-INT (Y/3)*3=1 THEN RETURN
140 LET X=INT (X/3)
150 LET Y=INT (Y/3)
160 IF X>0 OR Y>0 THEN GOTO 130
170 LET C=1
180 RETURN
```



## Swift

```Swift
import Foundation
func sierpinski_carpet(n:Int) -> String {
    func middle(str:String) -> String {
        let spacer = str.stringByReplacingOccurrencesOfString("#", withString:" ", options:nil, range:nil)
        return str + spacer + str
    }

    var carpet = ["#"]
    for i in 1...n {
        let a = carpet.map{$0 + $0 + $0}
        let b = carpet.map(middle)
        carpet = a + b + a
    }
    return "\n".join(carpet)
}

println(sierpinski_carpet(3))
```



## Tcl


```tcl
package require Tcl 8.5

proc map {lambda list} {
    foreach elem $list {
        lappend result [apply $lambda $elem]
    }
    return $result
}

proc sierpinski_carpet n {
    set carpet [list "#"]
    for {set i 1} {$i <= $n} {incr i} {
        set carpet [concat \
            [map {x {subst {$x$x$x}}} $carpet] \
            [map {x {subst {$x[string map {"#" " "} $x]$x}}} $carpet] \
            [map {x {subst {$x$x$x}}} $carpet] \
        ]
    }
    return [join $carpet \n]
}

puts [sierpinski_carpet 3]
```



## uBasic/4tH

<lang>Input "Carpet order: ";n

l = (3^n) - 1
For i = 0 To l
  For j = 0 To l
    Push i,j
    Gosub 100
    If Pop() Then
      Print "#";
    Else
      Print " ";
    EndIf
  Next
  Print
Next
End

100 y = Pop(): x = Pop() : Push 1

    Do While (x > 0) * (y > 0)
      If (x % 3 = 1) * (y % 3 = 1) Then
         Push (Pop() - 1)
         Break
      EndIf
      y = y / 3
      x = x / 3
    Loop

    Return
```


## UNIX Shell

Doesn't pretend to be efficient.

Note that this code inserts a space between characters; some versions of  [http://en.wikipedia.org/wiki/Paste_(Unix) paste(1)] (notably the one that ships with OS X) won't allow an empty delimiter.  If yours does, you can replace the <tt>-d ' '</tt> in the function body with <tt>-d </tt>' '<tt> </tt>for more compact output.

```bash
#!/bin/bash

sierpinski_carpet() {
    local -i n="${1:-3}"
    local carpet="${2:-#}"
    while (( n-- )); do
       local center="${carpet//#/ }"
       carpet="$(paste -d ' ' <(echo "$carpet"$'\n'"$carpet"$'\n'"$carpet")  <(echo "$carpet"$'\n'"$center"$'\n'"$carpet")  <(echo "$carpet"$'\n'"$carpet"$'\n'"$carpet"))"
    done
    echo "$carpet"
}
```


Sample run:


```txt
$ sierpinski_carpet 3
# # # # # # # # # # # # # # # # # # # # # # # # # # #
#   # #   # #   # #   # #   # #   # #   # #   # #   #
# # # # # # # # # # # # # # # # # # # # # # # # # # #
# # #       # # # # # #       # # # # # #       # # #
#   #       #   # #   #       #   # #   #       #   #
# # #       # # # # # #       # # # # # #       # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # #
#   # #   # #   # #   # #   # #   # #   # #   # #   #
# # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # #                   # # # # # # # # #
#   # #   # #   #                   #   # #   # #   #
# # # # # # # # #                   # # # # # # # # #
# # #       # # #                   # # #       # # #
#   #       #   #                   #   #       #   #
# # #       # # #                   # # #       # # #
# # # # # # # # #                   # # # # # # # # #
#   # #   # #   #                   #   # #   # #   #
# # # # # # # # #                   # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # #
#   # #   # #   # #   # #   # #   # #   # #   # #   #
# # # # # # # # # # # # # # # # # # # # # # # # # # #
# # #       # # # # # #       # # # # # #       # # #
#   #       #   # #   #       #   # #   #       #   #
# # #       # # # # # #       # # # # # #       # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # #
#   # #   # #   # #   # #   # #   # #   # #   # #   #
# # # # # # # # # # # # # # # # # # # # # # # # # # #

```



## Ursala

The carpet function works for any natural number n and is tested on 0,1,2, and 3.
The carpet is stored as a list of lists of booleans but converted to characters for display.

```Ursala
#import std
#import nat

carpet = ~&a^?\<<&>>! (-*<7,5,7>; *=DS ~&K7+ ~&B**DS*=rlDS)^|R/~& predecessor

#show+

test = mat0 ~&?(`#!,` !)*** carpet* <0,1,2,3>
```

<pre style="height:30ex;overflow:scroll;">
#

###
# #
###

#########
# ## ## #
#########
###   ###
# #   # #
###   ###
#########
# ## ## #
#########

###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
#########         #########
# ## ## #         # ## ## #
#########         #########
###   ###         ###   ###
# #   # #         # #   # #
###   ###         ###   ###
#########         #########
# ## ## #         # ## ## #
#########         #########
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################

```


## VBA

```vb
Const Order = 4

Function InCarpet(ByVal x As Integer, ByVal y As Integer)
    Do While x <> 0 And y <> 0
        If x Mod 3 = 1 And y Mod 3 = 1 Then
            InCarpet = " "
            Exit Function
        End If
        x = x \ 3
        y = y \ 3
    Loop
    InCarpet = "#"
End Function

Public Sub sierpinski_carpet()
    Dim i As Integer, j As Integer
    For i = 0 To 3 ^ Order - 1
        For j = 0 To 3 ^ Order - 1
            Debug.Print InCarpet(i, j);
        Next j
        Debug.Print
    Next i
End Sub
```
```txt
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
# #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # #
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
#########         ##################         ##################         #########
# ## ## #         # ## ## ## ## ## #         # ## ## ## ## ## #         # ## ## #
#########         ##################         ##################         #########
###   ###         ###   ######   ###         ###   ######   ###         ###   ###
# #   # #         # #   # ## #   # #         # #   # ## #   # #         # #   # #
###   ###         ###   ######   ###         ###   ######   ###         ###   ###
#########         ##################         ##################         #########
# ## ## #         # ## ## ## ## ## #         # ## ## ## ## ## #         # ## ## #
#########         ##################         ##################         #########
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
# #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # #
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###########################                           ###########################
# ## ## ## ## ## ## ## ## #                           # ## ## ## ## ## ## ## ## #
###########################                           ###########################
###   ######   ######   ###                           ###   ######   ######   ###
# #   # ## #   # ## #   # #                           # #   # ## #   # ## #   # #
###   ######   ######   ###                           ###   ######   ######   ###
###########################                           ###########################
# ## ## ## ## ## ## ## ## #                           # ## ## ## ## ## ## ## ## #
###########################                           ###########################
#########         #########                           #########         #########
# ## ## #         # ## ## #                           # ## ## #         # ## ## #
#########         #########                           #########         #########
###   ###         ###   ###                           ###   ###         ###   ###
# #   # #         # #   # #                           # #   # #         # #   # #
###   ###         ###   ###                           ###   ###         ###   ###
#########         #########                           #########         #########
# ## ## #         # ## ## #                           # ## ## #         # ## ## #
#########         #########                           #########         #########
###########################                           ###########################
# ## ## ## ## ## ## ## ## #                           # ## ## ## ## ## ## ## ## #
###########################                           ###########################
###   ######   ######   ###                           ###   ######   ######   ###
# #   # ## #   # ## #   # #                           # #   # ## #   # ## #   # #
###   ######   ######   ###                           ###   ######   ######   ###
###########################                           ###########################
# ## ## ## ## ## ## ## ## #                           # ## ## ## ## ## ## ## ## #
###########################                           ###########################
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
# #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # #
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
#########         ##################         ##################         #########
# ## ## #         # ## ## ## ## ## #         # ## ## ## ## ## #         # ## ## #
#########         ##################         ##################         #########
###   ###         ###   ######   ###         ###   ######   ###         ###   ###
# #   # #         # #   # ## #   # #         # #   # ## #   # #         # #   # #
###   ###         ###   ######   ###         ###   ######   ###         ###   ###
#########         ##################         ##################         #########
# ## ## #         # ## ## ## ## ## #         # ## ## ## ## ## #         # ## ## #
#########         ##################         ##################         #########
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
# #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # ## #   # #
###   ######   ######   ######   ######   ######   ######   ######   ######   ###
#################################################################################
# ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
#################################################################################
```


## VBScript


```VBScript
Function InCarpet(i,j)
	If i > 0 And j > 0 Then
		Do While i > 0 And j > 0
			If i Mod 3 = 1 And j Mod 3 = 1 Then
				InCarpet = " "
				Exit Do
			Else
				InCarpet = "#"
			End If
				i = Int(i / 3)
				j = Int(j / 3)
		Loop
	Else
		InCarpet = "#"
	End If
End Function

Function Carpet(n)
	k = 3^n - 1
	x2 = 0
	y2 = 0
	For y = 0 To k
		For x = 0 To k
			x2 = x
			y2 = y
			WScript.StdOut.Write InCarpet(x2,y2)
		Next
		WScript.StdOut.WriteBlankLines(1)
	Next
End Function

Carpet(WScript.Arguments(0))
```

```txt
F:\VBScript>cscript /nologo RosettaCode-Sierpinski_Carpet.vbs 3
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
#########         #########
# ## ## #         # ## ## #
#########         #########
###   ###         ###   ###
# #   # #         # #   # #
###   ###         ###   ###
#########         #########
# ## ## #         # ## ## #
#########         #########
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
```



## X86 Assembly


Uses magic number division to avoid repeatedly using the div instruction in a loop.

```asm
;x86-64 assembly code for Microsoft Windows
;Tested in windows 7 Enterprise Service Pack 1 64 bit
;With the AMD FX(tm)-6300 processor
;Assembled with NASM version 2.11.06
;Linked to C library with gcc version 4.9.2 (x86_64-win32-seh-rev1, Built by MinGW-W64 project)

;Assembled and linked with the following commands:
;nasm -f win64 <filename>.asm -o <filename>.obj
;gcc <filename>.obj -o <filename>

;Takes magnitude of Sierpinski Carpet as command line argument.

extern atoi,puts,putchar,exit

section .data
errmsg_noarg: db "Magnitude of Sierpinski Carpet was not specified.",0
errmsg_argnumber: db "There should be no more than one argument.",0

section .bss

section .text
global main

main:

;check for argument
cmp rcx,1
jle err_noarg

;ensure that only one argument was entered
cmp rcx,2
jg err_argnumber

;column in rsi
;row in rdi
;x in r8
;y in r9
;width in r13
;magic number in r14

mov r14,2863311531

;get magnitude in rbx from first arg
mov rcx,[rdx + 8]
call atoi
mov rbx,rax

cmp rbx,0
jz magnitude_zero


;determine dimensions of square
mov rax,1

find_width:

lea rax,[rax * 3]

dec rbx
jg find_width

sub rax,1

mov r13,rax
mov rdi,rax


next_row:

mov rsi,r13

fill_row:

;x in r8, y in r9
mov r8,rsi
mov r9,rdi

is_filled:

;if(x%3==1 && y%3==1)
;x%3 in rbx
mov rax,r8
mov rbx,r8
mul r14
shr rax,33
mov r8,rax
lea rax,[rax * 3]
sub rbx,rax

;y%3 in rcx
mov rax,r9
mov rcx,r9
mul r14
shr rax,33
mov r9,rax
lea rax,[rax * 3]
sub rcx,rax

;x%3==1 && y%3==1
xor rbx,1
xor rcx,1
or rbx,rcx
mov rcx,' '
cmp rbx,0
jz dont_fill

;x>0 || y>0
mov rax,r8
or rax,r9
cmp rax,0
jg is_filled

mov rcx,'#'
dont_fill:

call putchar

dec rsi
jge fill_row

;put newline at the end of each row
mov rcx,0xa
call putchar

dec rdi
jge next_row

xor rcx,rcx
call exit

magnitude_zero:

mov rcx,'#'
call putchar

mov rcx,0xa
call putchar

xor rcx,rcx
call exit

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;error message

err_noarg:

mov rcx,errmsg_noarg
call puts

mov rcx,1
call exit


err_argnumber:

mov rcx,errmsg_argnumber
call puts

mov rcx,1
call exit
```


```txt
F:\>asciisierpinski.exe
Magnitude of Sierpinski Carpet was not specified.

F:\>asciisierpinski.exe 1 1 1
There should be no more than one arguement.

F:\>asciisierpinski.exe 0
#

F:\>asciisierpinski.exe 1
###
# #
###

F:\>asciisierpinski.exe 2
#########
# ## ## #
#########
###   ###
# #   # #
###   ###
#########
# ## ## #
#########

F:\>asciisierpinski.exe 3
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################
#########         #########
# ## ## #         # ## ## #
#########         #########
###   ###         ###   ###
# #   # #         # #   # #
###   ###         ###   ###
#########         #########
# ## ## #         # ## ## #
#########         #########
###########################
# ## ## ## ## ## ## ## ## #
###########################
###   ######   ######   ###
# #   # ## #   # ## #   # #
###   ######   ######   ###
###########################
# ## ## ## ## ## ## ## ## #
###########################

```



## XPL0

[[File:CarpetXPL0.gif|right]]

```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

proc DrawPat(X0, Y0, S);        \Draw 3x3 pattern with hole in middle
int  X0, Y0, S;                 \coordinate of upper-left corner, size
int  X, Y;
[for Y:= 0 to 2 do
    for X:= 0 to 2 do
        if X#1 or Y#1 then      \don't draw middle pattern
           [if S>1 then         \recurse
                DrawPat(X*S+X0, Y*S+Y0, S/3)
           else Point(X+X0, Y+Y0, 4\red\);
           ];
];

[SetVid($13);                   \set 320x200 graphic video mode
DrawPat(0, 0, 3*3*3);           \draw Sierpinski carpet
if ChIn(1) then [];             \wait for keystroke
SetVid($3);                     \restore normal text mode
]
```



## Yabasic


```Yabasic
sub sp$(n)
	local i, s$

	for i = 1 to n
		s$ = s$ + " "
	next i
	return s$
end sub

sub replace$(s$, cf$, cr$)
	local i, p

	do
		i = instr(s$, cf$, p)
		if not i break
		mid$(s$, i, 1) = cr$
		p = i
	loop
	return s$
end sub

sub foreach$(carpet$, p$, m)
	local n, i, t$(1)

	n = token(carpet$, t$(), ",")

	for i = 1 to n
		switch(m)
		case 0:	p$ = p$ + "," + t$(i) + t$(i) + t$(i) : break
		case 1: p$ = p$ + "," + t$(i) + sp$(len(t$(i))) + t$(i) : break
		default: error "Method not found!" : break
		end switch
	next i
	return p$
end sub

sub sierpinskiCarpet$(n)
    local carpet$, next$, i

    carpet$ = "@"
    for i = 1 to n
        next$ = foreach$(carpet$, "")
        next$ = foreach$(carpet$, next$, 1)
        carpet$ = foreach$(carpet$, next$)
    next i
    return carpet$
end sub

print replace$(sierpinskiCarpet$(3), ",", "\n")
```



## zkl

[[File:Sierpinski carpet.zkl.gif|250px|thumb|right]]
Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
fcn drawPat(x0,y0,s,img){  // Draw 3x3 pattern with hole in middle
   foreach y,x in (3,3){
      if(x.isEven or y.isEven){	// don't draw middle pattern
	 if(s>1) self.fcn(x*s + x0, y*s + y0, s/3, img);	// recurse
	 else img[x + x0, y + y0]=0xff0000; // red
      }
   }
}
```


```zkl
img:=PPM(800,800);
drawPat(0,0,(3).pow(5),img);
img.write(File("foo.ppm","wb"));
```

