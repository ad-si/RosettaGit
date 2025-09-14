+++
title = "Heronian triangles"
description = ""
date = 2019-03-18T21:24:09Z
aliases = []
[extra]
id = 18449
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "algol_w",
  "applescript",
  "autohotkey",
  "c",
  "coffeescript",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "elixir",
  "erre",
  "factor",
  "fortran",
  "freebasic",
  "futurebasic",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lua",
  "nim",
  "oorexx",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "powershell",
  "python",
  "r",
  "racket",
  "rexx",
  "ring",
  "ruby",
  "scala",
  "sidef",
  "spl",
  "swift",
  "tcl",
  "vba",
  "zkl",
]
+++

[[wp:Heron's formula|Hero's formula]] for the area of a triangle given the length of its three sides   <big> ''a'',</big>   <big>''b'',</big>   and   <big>''c''</big>   is given by:

:::: <big><math>A = \sqrt{s(s-a)(s-b)(s-c)},</math></big>

where   <big>''s''</big>   is half the perimeter of the triangle; that is,

:::: <big><math>s=\frac{a+b+c}{2}.</math></big>



'''[http://www.had2know.com/academics/heronian-triangles-generator-calculator.html Heronian triangles]'''
are triangles whose sides ''and area'' are all integers.
: An example is the triangle with sides   '''3, 4, 5'''   whose area is   '''6'''   (and whose perimeter is   '''12''').



Note that any triangle whose sides are all an integer multiple of   '''3, 4, 5''';   such as   '''6, 8, 10,'''   will also be a Heronian triangle.

Define a '''Primitive Heronian triangle''' as a Heronian triangle where the greatest common divisor
of all three sides is   '''1'''   (unity).

This will exclude, for example, triangle   '''6, 8, 10.'''


## Task

# Create a named function/method/procedure/... that implements Hero's formula.
# Use the function to generate all the ''primitive'' Heronian triangles with sides <= 200.
# Show the count of how many triangles are found.
# Order the triangles by first increasing area, then by increasing perimeter, then by increasing maximum side lengths
# Show the first ten ordered triangles in a table of sides, perimeter, and area.
# Show a similar ordered table for those triangles with area = 210



Show all output here.

<small>'''Note''': when generating triangles it may help to restrict</small> <math>a <= b <= c</math>



## Ada


```Ada
with Ada.Containers.Indefinite_Ordered_Sets;
with Ada.Finalization;
with Ada.Text_IO; use Ada.Text_IO;
procedure Heronian is
   package Int_IO is new Ada.Text_IO.Integer_IO(Integer);
   use Int_IO;

   -- ----- Some math...
   function GCD (A, B : in Natural) return Natural is (if B = 0 then A else GCD (B, A mod B));

   function Int_Sqrt (N : in Natural) return Natural is
      R1 : Natural := N;
      R2 : Natural;
   begin
      if N <= 1 then
         return N;
      end if;
      loop
         R2 := (R1+N/R1)/2;
         if R2 >= R1 then
            return R1;
         end if;
         R1 := R2;
      end loop;
   end Int_Sqrt;

   -- ----- Defines the triangle with sides as discriminants and a constructor which will
   -- compute its other characteristics
   type t_Triangle (A, B, C : Positive) is new Ada.Finalization.Controlled with record
      Is_Heronian  : Boolean;
      Perimeter    : Positive;
      Area         : Natural;
   end record;

   overriding procedure Initialize (Self : in out t_Triangle) is
      -- Let's stick to integer computations, therefore a modified hero's formula
      -- will be used : S*(S-a)*(S-b)*(S-c) = (a+b+c)*(-a+b+c)*(a-b+c)*(a+b-c)/16
      -- This will require long integers because at max side size, the product
      -- before /16 excesses 2^31
      Long_Product  : Long_Long_Integer;
      Short_Product : Natural;
   begin
      Self.Perimeter   := Self.A + Self.B + Self.C;
      Long_Product     :=   Long_Long_Integer(Self.Perimeter)
                          * Long_Long_Integer(- Self.A + Self.B + Self.C)
                          * Long_Long_Integer(  Self.A - Self.B + Self.C)
                          * Long_Long_Integer(  Self.A + Self.B - Self.C);
      Short_Product    := Natural(Long_Product / 16);
      Self.Area        := Int_Sqrt (Short_Product);
      Self.Is_Heronian := (Long_Product mod 16 = 0) and (Self.Area * Self.Area = Short_Product);
   end Initialize;

   -- ----- Ordering triangles with criteria (Area,Perimeter,A,B,C)
   function "<" (Left, Right : in t_Triangle) return Boolean is
     (Left.Area      < Right.Area      or else (Left.Area      = Right.Area      and then
     (Left.Perimeter < Right.Perimeter or else (Left.Perimeter = Right.Perimeter and then
     (Left.A         < Right.A         or else (Left.A         = Right.A         and then
     (Left.B         < Right.B         or else (Left.B         = Right.B         and then
      Left.C         < Right.C))))))));
   package Triangle_Lists is new Ada.Containers.Indefinite_Ordered_Sets (t_Triangle);
   use Triangle_Lists;

   -- ----- Displaying triangle characteristics
   Header : constant String := "  A   B   C Per  Area" & ASCII.LF & "---+---+---+---+-----";
   procedure Put_Triangle (Position : Cursor) is
      Triangle : constant t_Triangle := Element(Position);
   begin
      Put(Triangle.A, 3);
      Put(Triangle.B, 4);
      Put(Triangle.C, 4);
      Put(Triangle.Perimeter, 4);
      Put(Triangle.Area,      6);
      New_Line;
   end Put_Triangle;

   -- ----- Global variables
   Triangles : Set := Empty_Set;
   -- Instead of constructing two sets, or browsing all the beginning of the set during
   -- the second output, start/end cursors will be updated during the insertions.
   First_201 : Cursor := No_Element;
   Last_201  : Cursor := No_Element;

   procedure Memorize_Triangle (A, B, C : in Positive) is
      Candidate : t_Triangle(A, B, C);
      Position  : Cursor;
      Dummy     : Boolean;
   begin
      if Candidate.Is_Heronian then
         Triangles.Insert (Candidate, Position, Dummy);
         if Candidate.Area = 210 then
            First_201 := (if    First_201 = No_Element then Position
                          elsif Position < First_201   then Position
                          else  First_201);
            Last_201 :=  (if    Last_201  = No_Element then Position
                          elsif Last_201  < Position   then Position
                          else  Last_201);
         end if;
      end if;
   end Memorize_Triangle;

begin
   -- Loops restrict to unique A,B,C (ensured by A <= B <= C) with sides < 200 and for
   -- which a triangle is constructible : C is not greater than B+A (flat triangle)
   for A in 1..200 loop
      for B in A..200 loop
         for C in B..Integer'Min(A+B-1,200) loop
            -- Filter non-primitive triangles
            if GCD(GCD(A,B),C) = 1 then
               Memorize_Triangle (A, B, C);
            end if;
         end loop;
      end loop;
   end loop;

   Put_Line (Triangles.Length'Img & " heronian triangles found :");
   Put_Line (Header);
   Triangles.Iterate (Process => Put_Triangle'Access);
   New_Line;

   Put_Line ("Heronian triangles with area = 201");
   Put_Line (Header);
   declare
      Position : Cursor := First_201;
   begin
      loop
         Put_Triangle (Position);
         exit when Position = Last_201;
         Position := Next(Position);
      end loop;
   end;
end Heronian;
```

```txt
 517 heronian triangles found :
  A   B   C Per  Area
---+---+---+---+-----
  3   4   5  12     6
  5   5   6  16    12
  5   5   8  18    12
  4  13  15  32    24
  5  12  13  30    30
  9  10  17  36    36
  3  25  26  54    36
  7  15  20  42    42
 10  13  13  36    60
  8  15  17  40    60
```

...

```txt

Heronian triangles with area = 201
  A   B   C Per  Area
---+---+---+---+-----
 17  25  28  70   210
 20  21  29  70   210
 12  35  37  84   210
 17  28  39  84   210
  7  65  68 140   210
  3 148 149 300   210
```



## ALGOL 68

```algol68
# mode to hold details of a Heronian triangle #
MODE HERONIAN = STRUCT( INT a, b, c, area, perimeter );
# returns the details of the Heronian Triangle with sides a, b, c or nil if it isn't one #
PROC try ht = ( INT a, b, c )REF HERONIAN:
    BEGIN
        REF HERONIAN    t := NIL;
        REAL            s  = ( a + b + c ) / 2;
        REAL area squared  = s * ( s - a ) * ( s - b ) * ( s - c );
        IF area squared > 0 THEN
            # a, b, c does form a triangle #
            REAL area      = sqrt( area squared );
            IF ENTIER area = area THEN
                # the area is integral so the triangle is Heronian #
                t := HEAP HERONIAN := ( a, b, c, ENTIER area, a + b + c )
            FI
        FI;
        t
    END # try ht # ;
# returns the GCD of a and b #
PROC gcd = ( INT a, b )INT: IF b = 0 THEN a ELSE gcd( b, a MOD b ) FI;
# prints the details of the Heronian triangle t #
PROC ht print = ( REF HERONIAN t )VOID:
     print( ( whole( a OF t, -4 ), whole( b OF t, -5 ), whole( c OF t, -5 ), whole( area OF t, -5 ), whole( perimeter OF t, -10 ), newline ) );
# prints headings for the Heronian Triangle table #
PROC ht title = VOID: print( ( "   a    b    c area perimeter", newline, "---- ---- ---- ---- ---------", newline ) );

BEGIN
    # construct ht as a table of the Heronian Triangles with sides up to 200 #
    [ 1 : 1000 ]REF HERONIAN ht;
    REF HERONIAN             t;
    INT                      ht count := 0;

    FOR c TO 200 DO
        FOR b TO c DO
            FOR a TO b DO
                IF gcd( gcd( a, b ), c ) = 1 THEN
                    t := try ht( a, b, c );
                    IF REF HERONIAN(t) ISNT REF HERONIAN(NIL) THEN
                        ht[ ht count +:= 1 ] := t
                    FI
                FI
            OD
        OD
    OD;

    # sort the table on ascending area, perimeter and max side length #
    # note we constructed the triangles with c as the longest side    #
    BEGIN
        INT          lower := 1, upper := ht count;
        WHILE upper        := upper - 1;
              BOOL swapped := FALSE;
              FOR i FROM lower TO upper DO
                  REF HERONIAN h := ht[ i     ];
                  REF HERONIAN k := ht[ i + 1 ];
                  IF area OF k < area OF h OR (   area OF k =  area OF h
                                              AND (  perimeter OF k <  perimeter OF h
                                                  OR (   perimeter OF k = perimeter OF h
                                                     AND c OF k < c OF h
                                                     )
                                                  )
                                              )
                  THEN
                      ht[ i     ] := k;
                      ht[ i + 1 ] := h;
                      swapped     := TRUE
                  FI
              OD;
              swapped
        DO SKIP OD;

        # display the triangles #
        print( ( "There are ", whole( ht count, 0 ), " Heronian triangles with sides up to 200", newline ) );
        ht title;
        FOR ht pos TO 10 DO ht print( ht( ht pos ) ) OD;
        print( ( " ...", newline ) );
        print( ( "Heronian triangles with area 210:", newline ) );
        ht title;
        FOR ht pos TO ht count DO
            REF HERONIAN t := ht[ ht pos ];
            IF area OF t = 210 THEN ht print( t ) FI
        OD
    END
END
```

```txt

There are 517 Heronian triangles with sides up to 200
   a    b    c area perimeter
---- ---- ---- ---- ---------
   3    4    5    6        12
   5    5    6   12        16
   5    5    8   12        18
   4   13   15   24        32
   5   12   13   30        30
   9   10   17   36        36
   3   25   26   36        54
   7   15   20   42        42
  10   13   13   60        36
   8   15   17   60        40
 ...
Heronian triangles with area 210:
   a    b    c area perimeter
---- ---- ---- ---- ---------
  17   25   28  210        70
  20   21   29  210        70
  12   35   37  210        84
  17   28   39  210        84
   7   65   68  210       140
   3  148  149  210       300

```



## ALGOL W

```algolw
begin
    % record to hold details of a Heronian triangle %
    record Heronian ( integer a, b, c, area, perimeter );
    % returns the details of the Heronian Triangle with sides a, b, c or nil if it isn't one %
    reference(Heronian) procedure tryHt( integer value a, b, c ) ;
    begin
        real                s, areaSquared, area;
        reference(Heronian) t;
        s           := ( a + b + c ) / 2;
        areaSquared := s * ( s - a ) * ( s - b ) * ( s - c );
        t           := null;
        if areaSquared > 0 then begin
            % a, b, c does form a triangle %
            area    := sqrt( areaSquared );
            if entier( area ) = area then begin
                % the area is integral so the triangle is Heronian %
                t := Heronian( a, b, c, entier( area ), a + b + c )
            end
        end;
        t
    end tryHt ;

    % returns the GCD of a and b %
    integer procedure gcd( integer value a, b ) ; if b = 0 then a else gcd( b, a rem b );

    % prints the details of the Heronian triangle t %
    procedure htPrint( reference(Heronian) value t ) ; write( i_w := 4, s_w := 1, a(t), b(t), c(t), area(t), "     ", perimeter(t) );
    % prints headings for the Heronian Triangle table %
    procedure htTitle ; begin write( "   a    b    c area perimeter" ); write( "---- ---- ---- ---- ---------" ) end;

    begin
        % construct ht as a table of the Heronian Triangles with sides up to 200 %
        reference(Heronian) array ht ( 1 :: 1000 );
        reference(Heronian)       t;
        integer                   htCount;

        htCount := 0;
        for c := 1 until 200 do begin
            for b := 1 until c do begin
                for a := 1 until b do begin
                    if gcd( gcd( a, b ), c ) = 1 then begin
                        t := tryHt( a, b, c );
                        if t not = null then begin
                            htCount       := htCount + 1;
                            ht( htCount ) := t
                        end
                    end
                end
            end
        end;

        % sort the table on ascending area, perimeter and max side length %
        % note we constructed the triangles with c as the longest side %
        begin
            integer             lower, upper;
            reference(Heronian) k, h;
            logical             swapped;
            lower := 1;
            upper := htCount;
            while begin
                upper   := upper - 1;
                swapped := false;
                for i := lower until upper do begin
                    h := ht( i     );
                    k := ht( i + 1 );
                    if area(k) < area(h) or (   area(k) =  area(h)
                                            and (  perimeter(k) <  perimeter(h)
                                                or (   perimeter(k) = perimeter(h)
                                                   and c(k)         < c(h)
                                                   )
                                                )
                                            )
                    then begin
                        ht( i     ) := k;
                        ht( i + 1 ) := h;
                        swapped     := true;
                    end
                end;
                swapped
            end
            do  begin end;
        end;

        % display the triangles %
        write( "There are ", htCount, " Heronian triangles with sides up to 200" );
        htTitle;
        for htPos := 1 until 10 do htPrint( ht( htPos ) );
        write( " ..." );
        write( "Heronian triangles with area 210:" );
        htTitle;
        for htPos := 1 until htCount do begin
            reference(Heronian) t;
            t := ht( htPos );
            if area(t) = 210 then htPrint( t )
        end
    end
end.
```

```txt

There are            517   Heronian triangles with sides up to 200
   a    b    c area perimeter
---- ---- ---- ---- ---------
   3    4    5    6        12
   5    5    6   12        16
   5    5    8   12        18
   4   13   15   24        32
   5   12   13   30        30
   9   10   17   36        36
   3   25   26   36        54
   7   15   20   42        42
  10   13   13   60        36
   8   15   17   60        40
 ...
Heronian triangles with area 210:
   a    b    c area perimeter
---- ---- ---- ---- ---------
  17   25   28  210        70
  20   21   29  210        70
  12   35   37  210        84
  17   28   39  210        84
   7   65   68  210       140
   3  148  149  210       300

```




## AppleScript


By composition of functional primitives, and using post-Yosemite AppleScript's ability to import Foundation classes (mainly for sorting records, here).

```AppleScript
use framework "Foundation"

-- HERONIAN TRIANGLES --------------------------------------------------------

-- heroniansOfSideUpTo :: Int -> [(Int, Int, Int)]
on heroniansOfSideUpTo(n)
    script sideA
        on |λ|(a)
            script sideB
                on |λ|(b)
                    script sideC
                        -- primitiveHeronian :: Int -> Int -> Int -> Bool
                        on primitiveHeronian(x, y, z)
                            (x ≤ y and y ≤ z) and (x + y > z) and ¬
                                gcd(gcd(x, y), z) = 1 and ¬
                                isIntegerValue(hArea(x, y, z))
                        end primitiveHeronian

                        on |λ|(c)
                            if primitiveHeronian(a, b, c) then
                                {{a, b, c}}
                            else
                                {}
                            end if
                        end |λ|
                    end script

                    concatMap(sideC, enumFromTo(b, n))
                end |λ|
            end script

            concatMap(sideB, enumFromTo(a, n))
        end |λ|
    end script

    concatMap(sideA, enumFromTo(1, n))
end heroniansOfSideUpTo


-- TEST ----------------------------------------------------------------------
on run
    set n to 200

    set lstHeron to ¬
        sortByComparing({{"area", true}, {"perimeter", true}, {"maxSide", true}}, ¬
            map(triangleDimensions, heroniansOfSideUpTo(n)))

    set lstCols to {"sides", "perimeter", "area"}
    set lstColWidths to {20, 15, 0}
    set area to 210

    script areaFilter
        -- Record -> [Record]
        on |λ|(recTriangle)
            if area of recTriangle = area then
                {recTriangle}
            else
                {}
            end if
        end |λ|
    end script

    intercalate("\n \n", {("Number of triangles found (with sides <= 200): " & ¬
        length of lstHeron as string), ¬
        ¬
            tabulation("First 10, ordered by area, perimeter, longest side", ¬
                items 1 thru 10 of lstHeron, lstCols, lstColWidths), ¬
        ¬
            tabulation("Area = 210", ¬
                concatMap(areaFilter, lstHeron), lstCols, lstColWidths)})
end run

-- triangleDimensions :: (Int, Int, Int) ->
--       {sides: (Int, Int, Int),  area: Int, perimeter: Int, maxSize: Int}
on triangleDimensions(lstSides)
    set {x, y, z} to lstSides
    {sides:[x, y, z], area:hArea(x, y, z) as integer, perimeter:x + y + z, maxSide:z}
end triangleDimensions

-- hArea :: Int -> Int -> Int -> Num
on hArea(x, y, z)
    set s to (x + y + z) / 2
    set a to s * (s - x) * (s - y) * (s - z)

    if a > 0 then
        a ^ 0.5
    else
        0
    end if
end hArea

-- gcd :: Int -> Int -> Int
on gcd(m, n)
    if n = 0 then
        m
    else
        gcd(n, m mod n)
    end if
end gcd


-- TABULATION ----------------------------------------------------------------

-- tabulation :: [Record] -> [String] -> String -> [Integer] -> String
on tabulation(strLegend, lstRecords, lstKeys, lstWidths)
    script heading
        on |λ|(strTitle, iCol)
            set str to toTitle(strTitle)
            str & replicate((item iCol of lstWidths) - (length of str), space)
        end |λ|
    end script

    script lineString
        on |λ|(rec)
            script fieldString
                -- fieldString :: String -> Int -> String
                on |λ|(strKey, i)
                    set v to keyValue(strKey, rec)

                    if class of v is list then
                        set strData to ("(" & intercalate(", ", v) & ")")
                    else
                        set strData to v as string
                    end if

                    strData & replicate(space, (item i of (lstWidths)) - (length of strData))
                end |λ|
            end script

            tab & intercalate(tab, map(fieldString, lstKeys))
        end |λ|
    end script

    strLegend & ":" & linefeed & linefeed & ¬
        tab & intercalate(tab, ¬
        map(heading, lstKeys)) & linefeed & ¬
        intercalate(linefeed, map(lineString, lstRecords))
end tabulation

-- GENERIC FUNCTIONS ---------------------------------------------------------

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

-- concatMap :: (a -> [b]) -> [a] -> [b]
on concatMap(f, xs)
    concat(map(f, xs))
end concatMap

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

-- isIntegerValue :: Num -> Bool
on isIntegerValue(n)
    {real, integer} contains class of n and (n = (n as integer))
end isIntegerValue

-- keyValue :: String -> Record -> Maybe String
on keyValue(strKey, rec)
    set ca to current application
    set v to (ca's NSDictionary's dictionaryWithDictionary:rec)'s objectForKey:strKey
    if v is not missing value then
        item 1 of ((ca's NSArray's arrayWithObject:v) as list)
    else
        missing value
    end if
end keyValue

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

-- List of {strKey, blnAscending} pairs -> list of records -> sorted list of records

-- sortByComparing :: [(String, Bool)] -> [Records] -> [Records]
on sortByComparing(keyDirections, xs)
    set ca to current application

    script recDict
        on |λ|(x)
            ca's NSDictionary's dictionaryWithDictionary:x
        end |λ|
    end script
    set dcts to map(recDict, xs)

    script asDescriptor
        on |λ|(kd)
            set {k, d} to kd
            ca's NSSortDescriptor's sortDescriptorWithKey:k ascending:d selector:dcts
        end |λ|
    end script

    ((ca's NSArray's arrayWithArray:dcts)'s ¬
        sortedArrayUsingDescriptors:map(asDescriptor, keyDirections)) as list
end sortByComparing

-- toTitle :: String -> String
on toTitle(str)
    set ca to current application
    ((ca's NSString's stringWithString:(str))'s ¬
        capitalizedStringWithLocale:(ca's NSLocale's currentLocale())) as text
end toTitle
```

```txt
Number of triangles found (with sides <= 200): 517

First 10, ordered by area, perimeter, longest side:

    Sides                   Perimeter          Area
    (3, 4, 5)               12                 6
    (5, 5, 6)               16                 12
    (5, 5, 8)               18                 12
    (4, 13, 15)             32                 24
    (5, 12, 13)             30                 30
    (9, 10, 17)             36                 36
    (3, 25, 26)             54                 36
    (7, 15, 20)             42                 42
    (10, 13, 13)            36                 60
    (8, 15, 17)             40                 60

Area = 210:

    Sides                   Perimeter          Area
    (17, 25, 28)            70                 210
    (20, 21, 29)            70                 210
    (12, 35, 37)            84                 210
    (17, 28, 39)            84                 210
    (7, 65, 68)             140                210
    (3, 148, 149)           300                210
```



## AutoHotkey


```AutoHotkey
Primitive_Heronian_triangles(MaxSide){
	obj :=[]
	loop, % MaxSide {
		a := A_Index
		loop % MaxSide-a+1 {
			b := A_Index+a-1
			loop % MaxSide-b+1 {
				c := A_Index+b-1, s := (a+b+c)/2, Area := Sqrt(s*(s-a)*(s-b)*(s-c))
				if (Area = Floor(Area)) && (Area>0) && !obj[a/s, b/s, c/s]
					obj[a/s, b/s, c/s]:=1 ,res .= (res?"`n":"") StrReplace(Area, ".000000") "`t" a+b+c "`t" a ", " b ", " c
	}	}	}
	Sort, res, F Sort
	return res
}

Sort(x, y){
	x := StrSplit(x, "`t"), y := StrSplit(y, "`t")
	return x.1 > y.1 ? 1 : x.1 < y.1 ? -1 : x.2 > y.2 ? 1 : x.2 < y.2 ? -1 : 0
}
```

Examples:
```AutoHotkey
res := Primitive_Heronian_triangles(200)
loop, parse, res, `n, `r
{
	if A_Index<=10
		res2.= A_LoopField "`n"
	if StrSplit(A_LoopField, "`t").1 = 210
		res3.= A_LoopField "`n"
	Counter := A_Index
}

MsgBox % Counter " results found"
. "`n`nFirst 10 results:"
. "`n" "Area`tPerimeter`tSides`n" res2
. "`nResults for Area = 210:"
. "`n" "Area`tPerimeter`tSides`n" res3
return
```

Outputs:
```txt
517 results found

First 10 results:
Area	Perimeter	Sides
6	12	3, 4, 5
12	16	5, 5, 6
12	18	5, 5, 8
24	32	4, 13, 15
30	30	5, 12, 13
36	36	9, 10, 17
36	54	3, 25, 26
42	42	7, 15, 20
60	36	10, 13, 13
60	40	8, 15, 17

Results for Area = 210:
Area	Perimeter	Sides
210	70	20, 21, 29
210	70	17, 25, 28
210	84	17, 28, 39
210	84	12, 35, 37
210	140	7, 65, 68
210	300	3, 148, 149
```



## C

Takes max side, number of triangles to print and area limit as inputs. Area should be -1 if it is not a restriction. Triangles are stored in a linked list which is built sorted and hence no post processing is required. Usage is printed out on incorrect invocation.

----
'''IMPORTANT''': This is a C99 compatible implementation. May result in errors on earlier compilers.

```C

#include<stdlib.h>
#include<stdio.h>
#include<math.h>

typedef struct{
	int a,b,c;
	int perimeter;
	double area;
}triangle;

typedef struct elem{
	triangle t;
	struct elem* next;
}cell;

typedef cell* list;

void addAndOrderList(list *a,triangle t){
	list iter,temp;
	int flag = 0;

	if(*a==NULL){
		*a = (list)malloc(sizeof(cell));
		(*a)->t = t;
		(*a)->next = NULL;
	}

	else{
		temp = (list)malloc(sizeof(cell));

			iter = *a;
			while(iter->next!=NULL){
				if(((iter->t.area<t.area)||(iter->t.area==t.area && iter->t.perimeter<t.perimeter)||(iter->t.area==t.area && iter->t.perimeter==t.perimeter && iter->t.a<=t.a))
				&&
				(iter->next==NULL||(t.area<iter->next->t.area || t.perimeter<iter->next->t.perimeter || t.a<iter->next->t.a))){
					temp->t = t;
					temp->next = iter->next;
					iter->next = temp;
					flag = 1;
					break;
				}

				iter = iter->next;
			}

			if(flag!=1){
				temp->t = t;
				temp->next = NULL;
				iter->next = temp;
			}
	}
}

int gcd(int a,int b){
	if(b!=0)
		return gcd(b,a%b);
	return a;
}

void calculateArea(triangle *t){
	(*t).perimeter = (*t).a + (*t).b + (*t).c;
	(*t).area = sqrt(0.5*(*t).perimeter*(0.5*(*t).perimeter - (*t).a)*(0.5*(*t).perimeter - (*t).b)*(0.5*(*t).perimeter - (*t).c));
}

list generateTriangleList(int maxSide,int *count){
	int a,b,c;
	triangle t;
	list herons = NULL;

	*count = 0;

	for(a=1;a<=maxSide;a++){
		for(b=1;b<=a;b++){
			for(c=1;c<=b;c++){
				if(c+b > a && gcd(gcd(a,b),c)==1){
					t = (triangle){a,b,c};
					calculateArea(&t);
					if(t.area/(int)t.area == 1){
						addAndOrderList(&herons,t);
						(*count)++;
					}
				}
			}
		}
	}

	return herons;
}

void printList(list a,int limit,int area){
	list iter = a;
	int count = 1;

	printf("\nDimensions\tPerimeter\tArea");

	while(iter!=NULL && count!=limit+1){
		if(area==-1 ||(area==iter->t.area)){
			printf("\n%d x %d x %d\t%d\t\t%d",iter->t.a,iter->t.b,iter->t.c,iter->t.perimeter,(int)iter->t.area);
			count++;
		}
		iter = iter->next;
	}
}

int main(int argC,char* argV[])
{
	int count;
	list herons = NULL;

	if(argC!=4)
		printf("Usage : %s <Max side, max triangles to print and area, -1 for area to ignore>",argV[0]);
	else{
		herons = generateTriangleList(atoi(argV[1]),&count);
		printf("Triangles found : %d",count);
		(atoi(argV[3])==-1)?printf("\nPrinting first %s triangles.",argV[2]):printf("\nPrinting triangles with area %s square units.",argV[3]);
		printList(herons,atoi(argV[2]),atoi(argV[3]));
		free(herons);
	}
	return 0;
}

```

Invocation and output :

```txt

C:\rosettaCode>heronian.exe 200 10 -1
Triangles found : 517
Printing first 10 triangles.
Dimensions      Perimeter       Area
5 x 4 x 3       12              6
6 x 5 x 5       16              12
8 x 5 x 5       18              12
15 x 13 x 4     32              24
13 x 12 x 5     30              30
17 x 10 x 9     36              36
26 x 25 x 3     54              36
20 x 15 x 7     42              42
13 x 13 x 10    36              60
17 x 15 x 8     40              60
C:\rosettaCode>heronian.exe 200 10 210
Triangles found : 517
Printing triangles with area 210 square units.
Dimensions      Perimeter       Area
28 x 25 x 17    70              210
29 x 21 x 20    70              210
37 x 35 x 12    84              210
39 x 28 x 17    84              210
68 x 65 x 7     140             210
149 x 148 x 3   300             210

```



## C++

```cpp
#include <algorithm>
#include <cmath>
#include <iostream>
#include <tuple>
#include <vector>

int gcd(int a, int b)
{
    int rem = 1, dividend, divisor;
    std::tie(divisor, dividend) = std::minmax(a, b);
    while (rem != 0) {
        rem = dividend % divisor;
        if (rem != 0) {
            dividend = divisor;
            divisor = rem;
        }
    }
    return divisor;
}

struct Triangle
{
    int a;
    int b;
    int c;
};

int perimeter(const Triangle& triangle)
{
    return triangle.a + triangle.b + triangle.c;
}

double area(const Triangle& t)
{
    double p_2 = perimeter(t) / 2.;
    double area_sq = p_2 * ( p_2 - t.a ) * ( p_2 - t.b ) * ( p_2 - t.c );
    return sqrt(area_sq);
}

std::vector<Triangle> generate_triangles(int side_limit = 200)
{
    std::vector<Triangle> result;
    for(int a = 1; a <= side_limit; ++a)
        for(int b = 1; b <= a; ++b)
            for(int c = a+1-b; c <= b; ++c) // skip too-small values of c, which will violate triangle inequality
            {
                Triangle t{a, b, c};
                double t_area = area(t);
                if(t_area == 0) continue;
                if( std::floor(t_area) == std::ceil(t_area) && gcd(a, gcd(b, c)) == 1)
                    result.push_back(t);
            }
    return result;
}

bool compare(const Triangle& lhs, const Triangle& rhs)
{
    return std::make_tuple(area(lhs), perimeter(lhs), std::max(lhs.a, std::max(lhs.b, lhs.c))) <
           std::make_tuple(area(rhs), perimeter(rhs), std::max(rhs.a, std::max(rhs.b, rhs.c)));
}

struct area_compare
{
    bool operator()(const Triangle& t, int i) { return area(t) < i; }
    bool operator()(int i, const Triangle& t) { return i < area(t); }
};

int main()
{
    auto tri = generate_triangles();
    std::cout << "There are " << tri.size() << " primitive Heronian triangles with sides up to 200\n\n";

    std::cout << "First ten when ordered by increasing area, then perimeter, then maximum sides:\n";
    std::sort(tri.begin(), tri.end(), compare);
    std::cout << "area\tperimeter\tsides\n";
    for(int i = 0; i < 10; ++i)
        std::cout << area(tri[i]) << '\t' << perimeter(tri[i]) << "\t\t" <<
                     tri[i].a << 'x' << tri[i].b << 'x' << tri[i].c << '\n';

    std::cout << "\nAll with area 210 subject to the previous ordering:\n";
    auto range = std::equal_range(tri.begin(), tri.end(), 210, area_compare());
    std::cout << "area\tperimeter\tsides\n";
    for(auto it = range.first; it != range.second; ++it)
        std::cout << area(*it) << '\t' << perimeter(*it) << "\t\t" <<
                     it->a << 'x' << it->b << 'x' << it->c << '\n';
}
```

```txt
There are 517 primitive Heronian triangles with sides up to 200

First ten when ordered by increasing area, then perimeter, then maximum sides:
area    perimeter       sides
6       12              5x4x3
12      16              6x5x5
12      18              8x5x5
24      32              15x13x4
30      30              13x12x5
36      36              17x10x9
36      54              26x25x3
42      42              20x15x7
60      36              13x13x10
60      40              17x15x8

All with area 210 subject to the previous ordering:
area    perimeter       sides
210     70              28x25x17
210     70              29x21x20
210     84              37x35x12
210     84              39x28x17
210     140             68x65x7
210     300             149x148x3
```


## C#

```c#
using System;
using System.Collections.Generic;

namespace heron
{
    class Program{
        static void Main(string[] args){
            List<int[]> list = new List<int[]>();
            for (int c = 1; c <= 200; c++)
                for (int b = 1; b <= c; b++)
                    for (int a = 1; a <= b; a++)
                        if (gcd(a, gcd(b, c)) == 1 && isHeron(heronArea(a, b, c)))
                            list.Add(new int[] { a, b, c, a + b + c, (int)heronArea(a, b, c)});
            sort(list);
            Console.WriteLine("Number of primitive Heronian triangles with sides up to 200: " + list.Count + "\n\nFirst ten when ordered by increasing area, then perimeter,then maximum sides:\nSides\t\t\tPerimeter\tArea");
            for(int i = 0; i < 10; i++)
                Console.WriteLine(list[i][0] + "\t" + list[i][1] + "\t" + list[i][2] + "\t" + list[i][3] + "\t\t" + list[i][4]);
            Console.WriteLine("\nPerimeter = 210\nSides\t\t\tPerimeter\tArea");
            foreach (int[] i in list)
                if (i[4] == 210)
                    Console.WriteLine(i[0] + "\t" + i[1] + "\t" + i[2] + "\t" + i[3] + "\t\t" + i[4]);
        }
        static bool isHeron(double heronArea){
            return heronArea % 1 == 0 && heronArea != 0;
        }
        static double heronArea(int a, int b, int c){
            double s = (a + b + c) / 2d;
            return Math.Sqrt(s * (s - a) * (s - b) * (s - c));
        }
        static int gcd(int a, int b){
            int remainder = 1, dividend, divisor;
            dividend = a > b ? a : b;
            divisor = a > b ? b : a;
            while (remainder != 0){
                remainder = dividend % divisor;
                if (remainder != 0){
                    dividend = divisor;
                    divisor = remainder;
                }
            }
            return divisor;
        }
        static void sort(List<int[]> list){
            int[] temp = new int[5];
            bool changed = true;
            while(changed){
                changed = false;
                for (int i = 1; i < list.Count; i++)
                    if (list[i][4] < list[i - 1][4] || list[i][4] == list[i - 1][4] && list[i][3] < list[i - 1][3]){
                        temp = list[i];
                        list[i] = list[i - 1];
                        list[i - 1] = temp;
                        changed = true;
                    }
            }
        }
    }
}
```

```txt
Number of primitive Heronian triangles with sides up to 200: 517

First ten when ordered by increasing area, then perimeter,then maximum sides:
Sides                   Perimeter       Area
3       4       5       12              6
5       5       6       16              12
5       5       8       18              12
4       13      15      32              24
5       12      13      30              30
9       10      17      36              36
3       25      26      54              36
7       15      20      42              42
10      13      13      36              60
8       15      17      40              60

Perimeter = 210
Sides                   Perimeter       Area
17      25      28      70              210
20      21      29      70              210
12      35      37      84              210
17      28      39      84              210
7       65      68      140             210
3       148     149     300             210
```



## CoffeeScript

```coffeescript
heronArea = (a, b, c) ->
    s = (a + b + c) / 2
    Math.sqrt s * (s - a) * (s - b) * (s - c)

isHeron = (h) -> h % 1 == 0 and h > 0

gcd = (a, b) ->
    leftover = 1
    dividend = if a > b then a else b
    divisor = if a > b then b else a
    until leftover == 0
        leftover = dividend % divisor
        if leftover > 0
            dividend = divisor
            divisor = leftover
    divisor

list = []
for c in [1..200]
    for b in [1..c]
        for a in [1..b]
            area = heronArea(a, b, c)
            if gcd(gcd(a, b), c) == 1 and isHeron(area)
                list.push new Array(a, b, c, a + b + c, area)

sort = (list) ->
    swapped = true
    while swapped
        swapped = false
        for i in [1..list.length-1]
            if list[i][4] < list[i - 1][4] or list[i][4] == list[i - 1][4] and list[i][3] < list[i - 1][3]
                temp = list[i]
                list[i] = list[i - 1]
                list[i - 1] = temp
                swapped = true
sort list

# some results:
console.log 'primitive Heronian triangles with sides up to 200: ' + list.length
console.log 'First ten when ordered by increasing area, then perimeter:'
for i in list[0..10-1]
    console.log  i[0..2].join(' x ') + ', p = ' + i[3] + ', a = ' + i[4]

console.log '\nHeronian triangles with area = 210:'
for i in list
    if i[4] == 210
        console.log i[0..2].join(' x ') + ', p = ' + i[3]
```

```txt
primitive Heronian triangles with sides up to 200: 517
First ten when ordered by increasing area, then perimeter:
3 x 4 x 5, p = 12, a = 6
5 x 5 x 6, p = 16, a = 12
5 x 5 x 8, p = 18, a = 12
4 x 13 x 15, p = 32, a = 24
5 x 12 x 13, p = 30, a = 30
9 x 10 x 17, p = 36, a = 36
3 x 25 x 26, p = 54, a = 36
7 x 15 x 20, p = 42, a = 42
10 x 13 x 13, p = 36, a = 60
8 x 15 x 17, p = 40, a = 60

Heronian triangles with area = 210:
17 x 25 x 28, p = 70
20 x 21 x 29, p = 70
12 x 35 x 37, p = 84
17 x 28 x 39, p = 84
7 x 65 x 68, p = 140
3 x 148 x 149, p = 300
```



## D

```d
import std.stdio, std.math, std.range, std.algorithm, std.numeric, std.traits, std.typecons;

double hero(in uint a, in uint b, in uint c) pure nothrow @safe @nogc {
    immutable s = (a + b + c) / 2.0;
    immutable a2 = s * (s - a) * (s - b) * (s - c);
    return (a2 > 0) ? a2.sqrt : 0.0;
}

bool isHeronian(in uint a, in uint b, in uint c) pure nothrow @safe @nogc {
    immutable h = hero(a, b, c);
    return h > 0 && h.floor == h.ceil;
}

T gcd3(T)(in T x, in T y, in T z) pure nothrow @safe @nogc {
    return gcd(gcd(x, y), z);
}

void main() /*@safe*/ {
    enum uint maxSide = 200;

    // Sort by increasing area, perimeter, then sides.
    //auto h = cartesianProduct!3(iota(1, maxSide + 1))
    auto r = iota(1, maxSide + 1);
    const h = cartesianProduct(r, r, r)
              //.filter!({a, b, c} => ...
              .filter!(t => t[0] <= t[1] && t[1] <= t[2] &&
                            t[0] + t[1] > t[2] &&
                            t[].gcd3 == 1 && t[].isHeronian)
              .array
              .schwartzSort!(t => tuple(t[].hero, t[].only.sum, t.reverse))
              .release;

    static void showTriangles(R)(R ts) @safe {
        "Area Perimeter Sides".writeln;
        foreach (immutable t; ts)
            writefln("%3s %8d %3dx%dx%d", t[].hero, t[].only.sum, t[]);
    }

    writefln("Primitive Heronian triangles with sides up to %d: %d", maxSide, h.length);
    "\nFirst ten when ordered by increasing area, then perimeter,then maximum sides:".writeln;
    showTriangles(h.take(10));

    "\nAll with area 210 subject to the previous ordering:".writeln;
    showTriangles(h.filter!(t => t[].hero == 210));
}
```

```txt
Primitive Heronian triangles with sides up to 200: 517

First ten when ordered by increasing area, then perimeter,then maximum sides:
Area Perimeter Sides
  6       12   3x4x5
 12       16   5x5x6
 12       18   5x5x8
 24       32   4x13x15
 30       30   5x12x13
 36       36   9x10x17
 36       54   3x25x26
 42       42   7x15x20
 60       36  10x13x13
 60       40   8x15x17

All with area 210 subject to the previous ordering:
Area Perimeter Sides
210       70  17x25x28
210       70  20x21x29
210       84  12x35x37
210       84  17x28x39
210      140   7x65x68
210      300   3x148x149
```



## EchoLisp


```scheme

;; returns quintuple (A s a b c)
;; or #f if not hero
(define (hero a b c (s 0) (A 0))
	(when
	(= 1 (gcd a b c))
	(set! s (// (+ a b c) 2))
	(set! A (* s (- s a)(- s b)(- s c)))
	(when (square? A)
		(list (sqrt A) (* s 2)  c b a))))

;; all heroes a,b,c < sidemax
;; sorted by A|s|c & a <=b <= c
(define (heroes (sidemax 201))
	(list-sort/fields 3
	(for*/list ((a (in-range 1 sidemax)) (b (in-range a sidemax)) (c (in-range b sidemax)))
	#:continue (<= (+ a b) c) ;; triangle inequality must hold !! cut search
	#:continue (not (hero a b c))
		(hero a b c))))

(define (print-hero h)
		(printf "A: %6d s: %6d    sides: %dx%dx%d"
			(list-ref  h  0) (list-ref  h  1)
			(list-ref h 2)(list-ref h 3) (list-ref h 4)))
(define (print-laurels H)
		(writeln '🌿🌿 (length H) 'heroes '🌿🌿))

```

```txt
(define H (heroes))

(print-laurels H)
🌿🌿     517     heroes     🌿🌿

(for-each print-hero (take H 10))

A:      6 s:     12 sides: 5x4x3
A:     12 s:     16 sides: 6x5x5
A:     12 s:     18 sides: 8x5x5
A:     24 s:     32 sides: 15x13x4
A:     30 s:     30 sides: 13x12x5
A:     36 s:     36 sides: 17x10x9
A:     36 s:     54 sides: 26x25x3
A:     42 s:     42 sides: 20x15x7
A:     60 s:     36 sides: 13x13x10
A:     60 s:     40 sides: 17x15x8

(for-each print-hero (filter (lambda(h) (= 210 (first h))) H))

A:    210 s:     70 sides: 28x25x17
A:    210 s:     70 sides: 29x21x20
A:    210 s:     84 sides: 37x35x12
A:    210 s:     84 sides: 39x28x17
A:    210 s:    140 sides: 68x65x7
A:    210 s:    300 sides: 149x148x3
```



## Elixir


```elixir
defmodule Heronian do
  def triangle?(a,b,c) when a+b <= c, do: false
  def triangle?(a,b,c) do
    area = area(a,b,c)
    area == round(area) and primitive?(a,b,c)
  end

  def area(a,b,c) do
    s = (a + b + c) / 2
    :math.sqrt(s * (s-a) * (s-b) * (s-c))
  end

  defp primitive?(a,b,c), do: gcd(gcd(a,b),c) == 1

  defp gcd(a,0), do: a
  defp gcd(a,b), do: gcd(b, rem(a,b))
end

max = 200
triangles = for a <- 1..max, b <- a..max, c <- b..max, Heronian.triangle?(a,b,c), do: {a,b,c}
IO.puts length(triangles)

IO.puts "\nSides\t\t\tPerim\tArea"
Enum.map(triangles, fn {a,b,c} -> {Heronian.area(a,b,c),a,b,c} end)
|> Enum.sort
|> Enum.take(10)
|> Enum.each(fn {area, a, b, c} ->
     IO.puts "#{a}\t#{b}\t#{c}\t#{a+b+c}\t#{round(area)}"
   end)
IO.puts ""
area_size = 210
Enum.filter(triangles, fn {a,b,c} -> Heronian.area(a,b,c) == area_size end)
|> Enum.sort_by(fn {a,b,c} -> a+b+c end)
|> Enum.each(fn {a, b, c} ->
     IO.puts "#{a}\t#{b}\t#{c}\t#{a+b+c}\t#{area_size}"
   end)
```


```txt
517

Sides                   Perim   Area
3       4       5       12      6
5       5       6       16      12
5       5       8       18      12
4       13      15      32      24
5       12      13      30      30
3       25      26      54      36
9       10      17      36      36
7       15      20      42      42
6       25      29      60      60
8       15      17      40      60

17      25      28      70      210
20      21      29      70      210
12      35      37      84      210
17      28      39      84      210
7       65      68      140     210
3       148     149     300     210
```



## ERRE


```ERRE

PROGRAM HERON

DIM LISTA%[600,4]

PROCEDURE GCD(J%,K%->MCD%)
  WHILE J%<>K% DO
     IF J%>K% THEN
         J%=J%-K%
       ELSE
         K%=K%-J%
     END IF
  END WHILE
  MCD%=J%
END PROCEDURE

BEGIN
    PRINT(CHR$(12);) !CLS
    FOR C%=1 TO 200 DO
       FOR B%=1 TO C% DO
          FOR A%=1 TO B% DO
             S#=(A%+B%+C%)/2#
             AREA#=S#*(S#-A%)*(S#-B%)*(S#-C%)
             IF AREA#>0 THEN
             AREA#=SQR(AREA#)
             IF AREA#=INT(AREA#) THEN
                 GCD(B%,C%->RES%)
                 GCD(A%,RES%->RES%)
                    IF RES%=1 THEN
                       COUNT%=COUNT%+1
                       LISTA%[COUNT%,0]=A%    LISTA%[COUNT%,1]=B%   LISTA%[COUNT%,2]=C%
                       LISTA%[COUNT%,3]=2*S#  LISTA%[COUNT%,4]=AREA#
                    END IF
             END IF
         END IF
     END FOR
  END FOR
END FOR

PRINT("Number of triangles:";COUNT%)

! sorting array
FLIPS%=TRUE
WHILE FLIPS% DO
   FLIPS%=FALSE
   FOR I%=1 TO COUNT%-1 DO
      IF LISTA%[I%,4]>LISTA%[I%+1,4] THEN
        FOR K%=0 TO 4 DO
           SWAP(LISTA%[I%,K%],LISTA%[I%+1,K%])
        END FOR
        FLIPS%=TRUE
      END IF
   END FOR
END WHILE

! first ten
FOR I%=1 TO 10 DO
    PRINT(#1,LISTA%[I%,0],LISTA%[I%,1],LISTA%[I%,2],LISTA%[I%,3],LISTA%[I%,4])
END FOR
PRINT

! triangle with area=210
FOR I%=1 TO COUNT% DO
    IF LISTA%[I%,4]=210 THEN
       PRINT(LISTA%[I%,0],LISTA%[I%,1],LISTA%[I%,2],LISTA%[I%,3],LISTA%[I%,4])
    END IF
END FOR
END PROGRAM

```


```txt
Number of triangles: 517
 3             4             5             12            6
 5             5             6             16            12
 5             5             8             18            12
 4             13            15            32            24
 5             12            13            30            30
 9             10            17            36            36
 3             25            26            54            36
 7             15            20            42            42
 10            13            13            36            60
 8             15            17            40            60

 17            25            28            70            210
 20            21            29            70            210
 12            35            37            84            210
 17            28            39            84            210
 7             65            68            140           210
 3             148           149           300           210
```



## Factor


```factor
USING: accessors assocs backtrack combinators.extras
combinators.short-circuit formatting io kernel locals math
math.functions math.order math.parser math.ranges mirrors qw
sequences sorting.slots ;
IN: rosetta-code.heronian-triangles

TUPLE: triangle a b c area perimeter ;

:: area ( a b c -- x )
    a b + c + 2 / :> s
    s s a - * s b - * s c - * sqrt ;

: <triangle> ( triplet-seq -- triangle )
    [ first3 ] [ first3 area >integer ] [ sum ] tri
    triangle boa ;

: heronian? ( a b c -- ? )
    area dup [ complex? ] [ 0 number= ] bi or
    [ drop f ] [ dup >integer number= ] if ;

: 3gcd ( a b c -- n ) [ gcd nip ] twice ;

: primitive-heronian? ( a b c -- ? )
    { [ 3gcd 1 = ] [ heronian? ] } 3&& ;

:: find-triangles ( -- seq )
    [
        200 [1,b] amb-lazy :> c    ! Use backtrack vocab to test
        c   [1,b] amb-lazy :> b    ! permutations of sides such
        b   [1,b] amb-lazy :> a    ! that c >= b >= a.
        a b c primitive-heronian? must-be-true
        { a b c } <triangle>
    ] bag-of ;                     ! collect every triangle

: sort-triangles ( seq -- seq' )
    { { area>> <=> } { perimeter>> <=> } } sort-by ;

CONSTANT: format "%4s%5s%5s%5s%10s\n"

: print-header ( -- )
    qw{ a b c area perimeter } format vprintf
    "---- ---- ---- ---- ---------" print ;

: print-triangle ( triangle -- )
    <mirror> >alist values [ number>string ] map format vprintf ;

: print-triangles ( seq -- ) [ print-triangle ] each ; inline

: first10 ( sorted-triangles -- )
    dup length "%d triangles found. First 10: \n" printf
    print-header 10 head print-triangles ;

: area210= ( sorted-triangles -- )
    "Triangles with area 210: " print print-header
    [ area>> 210 = ] filter print-triangles ;

: main ( -- )
    "Finding heronian triangles with sides <= 200..." print nl
    find-triangles sort-triangles
    [ first10 nl ] [ area210= ] bi ;

MAIN: main
```

```txt

Finding heronian triangles with sides <= 200...

517 triangles found. First 10:
   a    b    c area perimeter
---- ---- ---- ---- ---------
   3    4    5    6        12
   5    5    6   12        16
   5    5    8   12        18
   4   13   15   24        32
   5   12   13   30        30
   9   10   17   36        36
   3   25   26   36        54
   7   15   20   42        42
  10   13   13   60        36
   8   15   17   60        40

Triangles with area 210:
   a    b    c area perimeter
---- ---- ---- ---- ---------
  17   25   28  210        70
  20   21   29  210        70
  12   35   37  210        84
  17   28   39  210        84
   7   65   68  210       140
   3  148  149  210       300

```



## Fortran

Earlier Fortran doesn't offer special functions such as SUM, PRODUCT and MAXVAL of arrays, nor the ability to create compound data aggregates such as STASH to store a triangle's details. Simple code would have to be used in the absence of such conveniences, and multiple  ordinary arrays rather than an array of a compound data entity. Rather than attempt to create the candidate triangles in the desired order, the simple approach is to sort a list of triangles, and using an XNDX array evades tossing compound items about. Rather than create a procedure to do the sorting, a comb sort is not too much trouble to place in-line once. Further, since the ordering is based on a compound key, having only one comparison to code is a boon. The three-way-if statement is central to the expedient evaluation of a compound sort key, but this facility is deprecated by the modernists, with no alternative offered that avoids re-comparison of parts.

```Fortran

      MODULE GREEK MATHEMATICIANS	!Two millenia back and more.
       CONTAINS
        INTEGER FUNCTION GCD(I,J)	!Greatest common divisor.
         INTEGER I,J	!Of these two integers.
         INTEGER N,M,R	!Workers.
          N = MAX(I,J)	!Since I don't want to damage I or J,
          M = MIN(I,J)	!These copies might as well be the right way around.
    1     R = MOD(N,M)		!Divide N by M to get the remainder R.
c          write (6,*) "M,N,R",M,N,R
          IF (R.GT.0) THEN	!Remainder zero?
            N = M			!No. Descend a level.
            M = R			!M-multiplicity has been removed from N.
            IF (R .GT. 1) GO TO 1	!No point dividing by one.
          END IF			!If R = 0, M divides N.
          GCD = M			!There we are.
        END FUNCTION GCD	!Euclid lives on!
        FUNCTION GCD3(I,J,K)	!Double do.
         INTEGER I,J,K	!Three numbers.
         INTEGER R	!One remainder.
          R = GCD(I,J)		!Greatest common divisor.
          IF (R .GT. 1) R = GCD(R,K)	!The first two might be co-prime.
          GCD3 = R		!The result.
        END FUNCTION GCD3

        REAL*8 FUNCTION HERO(SIDE)	!Hero's calculation for the area of a triangle.
Calculations could proceed with non-integer sides.
         INTEGER SIDE(3)	!The lengths of each of the sides.
         REAL*8 S		!A scratchpad.
          S = SUM(SIDE)		!Definitely integer arithmetic.
          S = S/2		!Full precision without muttering /2D0.
          S = S*PRODUCT(S - SIDE)	!Negative for non-joining triangles.
          HERO = SIGN(SQRT(ABS(S)),S)	!Protect the SQRT against such.
        END FUNCTION HERO		!As when one side is longer than the other two combined.
      END MODULE GREEK MATHEMATICIANS	!Only a selection here.

      PROGRAM TEST		!Find triangles with integral sides and areas.
      USE GREEK MATHEMATICIANS	!For guidance.
      INTEGER LIMIT,LOTS	!And then descend to Furrytran.
      PARAMETER (LIMIT = 200, LOTS = 666)	!This should do.
      INTEGER I,J,K,SIDE(3)	!The lengths of the sides of the triangles.
      EQUIVALENCE (SIDE(1),I),(SIDE(2),J),(SIDE(3),K)	!I want two access styles.
      REAL*8 A			!The area of the triangle.
      TYPE ABLOB		!Define a stash for the desired results.
       INTEGER SIDE(3)		!The three sides,
       INTEGER PERIMETER	!Their summation, somewhat redundant.
       INTEGER AREA		!This is rather more difficult to calculate.
      END TYPE ABLOB		!That will do.
      TYPE(ABLOB) STASH(LOTS)	!I'll have some.
      INTEGER N,XNDX(LOTS)	!A counter and an index..
      INTEGER H,T		!Stuff for the in-line combsort.
      LOGICAL CURSE		!Rather than mess with subroutines and parameters.
      INTEGER TASTE,CHOICE	!Output selection stuff.
      PARAMETER (TASTE = 10, CHOICE = 210)	!As specified.

Collect some triangles.
      N = 0	!So, here we go.
      DO K = 1,LIMIT	!Just slog away,
        DO J = 1,K		!With brute force and ignorance.
          DO I = 1,J			!This way, a 3,4,5 triangle is in that order.
            IF (GCD3(I,J,K).GT.1) CYCLE	!A mere multiple. Seen it before.
            A = HERO(SIDE)		!Assess the area.
            IF (A.LE.0) CYCLE		!Not a valid triangle!
            IF (A .NE. INT(A)) CYCLE	!Not an integral area. Precision is adequate...
            N = N + 1			!Another candidate survives.
            IF (N.GT.LOTS) STOP "Too many!"	!Perhaps not for long!
            XNDX(N) = N			!So, keep a finger.
            STASH(N).SIDE = SIDE		!Stash its details.
            STASH(N).PERIMETER = SUM(SIDE)	!Calculate once, here.
            STASH(N).AREA = A			!And save this as an integer.
c            WRITE (6,10) N,STASH(N)
   10       FORMAT (I4,":",3I4,I7,I8)	!A reasonable layout.
          END DO
        END DO
      END DO
      WRITE (6,11) N,LIMIT	!The first result.
   11 FORMAT (I0," triangles of integral area. Sides up to ",I0)

Comb sort involves coding only one test, and the comparison is to be compound...
      H = N - 1	!Last - First, and not +1.
   20 H = MAX(1,H*10/13)	!The special feature.
      IF (H.EQ.9 .OR. H.EQ.10) H = 11	!A twiddle.
      CURSE = .FALSE.		!So far, so good.
      DO 24 I = N - H,1,-1	!If H = 1, this is a BubbleSort.
        IF (STASH(XNDX(I)).AREA - STASH(XNDX(I + H)).AREA) 24,21,23 	!One compare. But, a compound key.
   21   IF (STASH(XNDX(I)).PERIMETER-STASH(XNDX(I+H)).PERIMETER)24,22,23 	!Equal area, so, perimeter?
   22   IF (MAXVAL(STASH(XNDX(I)).SIDE)			!Equal perimeter, so, longest side?
     1    - MAXVAL(STASH(XNDX(I+H)).SIDE)) 24,24,23	!At last, equality here can be passed over.
   23     T=XNDX(I); XNDX(I)=XNDX(I+H); XNDX(I+H)=T	!One swap.
          CURSE = .TRUE.			!One curse.
   24 CONTINUE				!One loop.
      IF (CURSE .OR. H.GT.1) GO TO 20	!Work remains?

Cast forth the results, as per the specification.
      WRITE (6,30) TASTE
   30 FORMAT ("First ",I0,", ordered by area, perimeter, longest side.",
     1 /,"Index ---Sides--- Perimeter Area")
      DO I = 1,TASTE
        WRITE (6,10) XNDX(I),STASH(XNDX(I))
      END DO

      WRITE (6,31) CHOICE
   31 FORMAT ("Those triangles with area",I7)
      DO I = 1,N	!I could go looking through the ordered list for CHOICE entries,
        IF (STASH(XNDX(I)).AREA.NE.CHOICE) CYCLE!But I can't be bothered.
        WRITE (6,10) XNDX(I),STASH(XNDX(I))	!Here is one such.
      END DO		!Just thump through the lot.
      END

```


```txt
517 triangles of integral area. Sides up to 200
First 10, ordered by area, perimeter, longest side.
Index ---Sides--- Perimeter Area
   1:   3   4   5     12       6
   2:   5   5   6     16      12
   3:   5   5   8     18      12
   6:   4  13  15     32      24
   4:   5  12  13     30      30
   8:   9  10  17     36      36
  19:   3  25  26     54      36
  12:   7  15  20     42      42
   5:  10  13  13     36      60
   9:   8  15  17     40      60
Those triangles with area    210
  21:  17  25  28     70     210
  22:  20  21  29     70     210
  33:  12  35  37     84     210
  36:  17  28  39     84     210
  91:   7  65  68    140     210
 329:   3 148 149    300     210
```



## FreeBASIC


```freebasic
' version 02-05-2016
' compile with: fbc -s console

#Macro header
    Print
    Print "      a      b      c      s   area"
    Print "-----------------------------------"
#EndMacro

Type triangle
    Dim As UInteger a
    Dim As UInteger b
    Dim As UInteger c
    Dim As UInteger s
    Dim As UInteger area
End Type

Function gcd(x As UInteger, y As UInteger) As UInteger

    Dim As UInteger t

    While y
        t = y
        y = x Mod y
        x = t
    Wend

    Return x

End Function

Function Heronian_triangles(a_max As UInteger, b_max As UInteger, _
            c_max As UInteger, result() As triangle) As UInteger

    Dim As UInteger a, b, c
    Dim As UInteger s, sqroot, total, temp

    For a = 1 To a_max
        For b = a To b_max
            ' make sure that a + b + c is even
            For c = b + (a And 1) To c_max Step 2
                ' to form a triangle a + b must be greater then c
                If (a + b) <= c Then Exit For
                ' check if a, b and c have a common divisor
                If (gcd(c, b) <> 1 And gcd(c, a) <> 1) Then
                    Continue For
                End If
                s = (a + b + c) \ 2
                temp = s * (s - a) * (s - b) * (s - c)
                sqroot = Sqr(temp)
                If (sqroot * sqroot) = temp Then
                    total += 1
                    With result(total)
                        .a = a
                        .b = b
                        .c = c
                        .s = s
                        .area = sqroot
                    End With
                End If
            Next
        Next
    Next

    Return total

End Function


Sub sort_tri(result() As triangle, total As UInteger)
    ' shell sort
    ' sort order: area, s, c

    Dim As UInteger x, y, inc, done

    inc = total
    Do
        inc = IIf(inc > 1, inc \ 2, 1)
        Do
            done = 0
            For x = 1 To total - inc
                y = x + inc
                If result(x).area > result(y).area Then
                    Swap result(x), result(y)
                    done = 1
                Else
                    If result(x).area = result(y).area Then
                        If result(x).s > result(y).s Then
                            Swap result(x), result(y)
                            done = 1
                        Else
                            If result(x).s = result(y).s Then
                                If result(x).c > result(y).c Then
                                    Swap result(x), result(y)
                                    done = 1
                                End If
                            End If
                        End If
                    End If
                End If
            Next
        Loop Until done = 0
    Loop Until inc = 1

End Sub


' ------=< MAIN >=------

ReDim result(1 To 1000) As triangle
Dim As UInteger x, y, total

total = Heronian_triangles(200, 200, 200, result() )

' trim the array by removing empty entries
ReDim Preserve result(1 To total ) As triangle

sort_tri(result(), total)

Print "There are ";total;" Heronian triangles with sides <= 200"
Print

Print "First ten sorted entries"
header   ' print header
For x = 1 To IIf(total > 9, 10, total)
    With result(x)
        Print Using "  #####"; .a; .b; .c; .s; .area
    End With
Next
Print
Print

Print "Entries with a area = 210"
header   ' print header
For x = 1 To UBound(result)
    With result(x)
        If .area = 210 Then
            Print Using "  #####"; .a; .b; .c; .s; .area
        End If
    End With
Next

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
There are 517 Heronian triangles with sides <= 200

First ten sorted entries

      a      b      c      s   area
-----------------------------------
      3      4      5      6      6
      5      5      6      8     12
      5      5      8      9     12
      4     13     15     16     24
      5     12     13     15     30
      9     10     17     18     36
      3     25     26     27     36
      7     15     20     21     42
     10     13     13     18     60
      8     15     17     20     60


Entries with a area = 210

      a      b      c      s   area
-----------------------------------
     17     25     28     35    210
     20     21     29     35    210
     12     35     37     42    210
     17     28     39     42    210
      7     65     68     70    210
      3    148    149    150    210
```



## FutureBasic


```futurebasic

include "ConsoleWindow"

// Set width of tabs
def tab 10

local fn gcd( a as long, b as long )
dim as long result

if ( b != 0 )
   result = fn gcd( b, a mod b)
else
   result = abs(a)
end if
end fn = result

begin globals
dim as long triangleInfo( 600, 4 )
end globals

local fn CalculateHeronianTriangles( numberToCheck as long ) as long
dim as long c, b, a, result, count : count = 0
dim as double s, area

for c = 1 to numberToCheck
   for b = 1 to c
      for a = 1 to b
         s = ( a + b + c ) / 2
         area = s * ( s - a ) * ( s - b ) * ( s  - c )
            if area > 0
               area = sqr( area )
               if area = int( area )
                  result = fn gcd( b, c )
                  result = fn gcd( a, result )
                     if result == 1
                        count++
                        triangleInfo( count, 0 ) = a
                        triangleInfo( count, 1 ) = b
                        triangleInfo( count, 2 ) = c
                        triangleInfo( count, 3 ) = 2 * s
                        triangleInfo( count, 4 ) = area
                      end if
                end if
           end if
      next
   next
next
end fn = count

dim as long i, k, count

count = fn CalculateHeronianTriangles( 200 )

print
print "Number of triangles:"; count
print
print "---------------------------------------------"
print "Side A", "Side B", "Side C", "Perimeter", "Area"
print "---------------------------------------------"

// Sort array
dim as Boolean flips : flips = _true
while ( flips = _true )
   flips = _false
   for i = 1 to count - 1
      if triangleInfo( i, 4 ) > triangleInfo( i + 1, 4 )
         for k = 0 to 4
            swap triangleInfo( i, k ), triangleInfo( i + 1, k )
         next
      flips = _true
      end if
   next
wend

// Find first 10 heronian triangles
for i = 1 to 10
   print triangleInfo( i, 0 ), triangleInfo( i, 1 ), triangleInfo( i, 2 ), triangleInfo( i, 3 ), triangleInfo( i, 4 )
next
print
print "Triangles with an area of 210:"
print
// Search for triangle with area of 210
for i = 1 to count
   if triangleInfo( i, 4 ) == 210
      print triangleInfo( i, 0 ), triangleInfo( i, 1 ), triangleInfo( i, 2 ), triangleInfo( i, 3 ), triangleInfo( i, 4 )
   end if
next

```


Output:

```txt

Number of triangles: 517

---------------------------------------------
Side A    Side B    Side C    Perimeter Area
---------------------------------------------
 3         4         5         12        6
 5         5         6         16        12
 5         5         8         18        12
 4         13        15        32        24
 5         12        13        30        30
 9         10        17        36        36
 3         25        26        54        36
 7         15        20        42        42
 10        13        13        36        60
 8         15        17        40        60

Triangles with an area of 210:

 17        25        28        70        210
 20        21        29        70        210
 12        35        37        84        210
 17        28        39        84        210
 7         65        68        140       210
 3         148       149       300       210

```



## Go


```go
package main

import (
    "fmt"
    "math"
    "sort"
)

const (
    n = 200
    header = "\nSides          P   A"
)

func gcd(a, b int) int {
    leftover := 1
    var dividend, divisor int
    if (a > b) { dividend, divisor = a, b } else { dividend, divisor = b, a }

    for (leftover != 0) {
        leftover = dividend % divisor
        if (leftover > 0) {
            dividend, divisor = divisor, leftover
        }
    }
    return divisor
}

func is_heron(h float64) bool {
    return h > 0 && math.Mod(h, 1) == 0.0
}

// by_area_perimeter implements sort.Interface for [][]int based on the area first and perimeter value
type by_area_perimeter [][]int

func (a by_area_perimeter) Len() int           { return len(a) }
func (a by_area_perimeter) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a by_area_perimeter) Less(i, j int) bool {
    return a[i][4] < a[j][4] || a[i][4] == a[j][4] && a[i][3] < a[j][3]
}

func main() {
    var l [][]int
    for c := 1; c <= n; c++ {
        for b := 1; b <= c; b++ {
            for a := 1; a <= b; a++ {
                if (gcd(gcd(a, b), c) == 1) {
                    p := a + b + c
                    s := float64(p) / 2.0
                    area := math.Sqrt(s * (s - float64(a)) * (s - float64(b)) * (s - float64(c)))
                    if (is_heron(area)) {
                        l = append(l, []int{a, b, c, p, int(area)})
                    }
                }
            }
        }
    }

    fmt.Printf("Number of primitive Heronian triangles with sides up to %d: %d", n, len(l))
    sort.Sort(by_area_perimeter(l))
    fmt.Printf("\n\nFirst ten when ordered by increasing area, then perimeter:" + header)
    for i := 0; i < 10; i++ { fmt.Printf("\n%3d", l[i]) }

    a := 210
    fmt.Printf("\n\nArea = %d%s", a, header)
    for _, it := range l  {
        if (it[4] == a) {
            fmt.Printf("\n%3d", it)
        }
    }
}
```

```txt
Number of primitive Heronian triangles with sides up to 200: 517

First ten when ordered by increasing area, then perimeter:
Sides          P   A
[  3   4   5  12   6]
[  5   5   6  16  12]
[  5   5   8  18  12]
[  4  13  15  32  24]
[  5  12  13  30  30]
[  9  10  17  36  36]
[  3  25  26  54  36]
[  7  15  20  42  42]
[ 10  13  13  36  60]
[  8  15  17  40  60]

Area = 210
Sides          P   A
[ 17  25  28  70 210]
[ 20  21  29  70 210]
[ 12  35  37  84 210]
[ 17  28  39  84 210]
[  7  65  68 140 210]
[  3 148 149 300 210]
```



## Haskell


```Haskell
import qualified Data.List as L
import Data.Maybe
import Data.Ord
import Text.Printf

-- Determine if a number n is a perfect square and return its square root if so.
-- This is used instead of sqrt to avoid fixed sized floating point numbers.
perfectSqrt :: Integral a => a -> Maybe a
perfectSqrt n
  | n == 1    = Just 1
  | n < 4     = Nothing
  | otherwise =
  let search low high =
        let guess = (low + high) `div` 2
            square = guess ^ 2
            next
              | square == n  = Just guess
              | low == guess = Nothing
              | square < n   = search guess high
              | otherwise    = search low guess
        in next
  in search 0 n

-- Determine the area of a Heronian triangle if it is one.
heronTri :: Integral a => a -> a -> a -> Maybe a
heronTri a b c =
  let -- Rewrite Heron's formula to factor out the term 16 under the root.
    areaSq16    = (a + b + c) * (b + c - a) * (a + c - b) * (a + b - c)
    (areaSq, r) = areaSq16 `divMod` 16
  in if r == 0
     then perfectSqrt areaSq
     else Nothing

isPrimitive :: Integral a => a -> a -> a -> a
isPrimitive a b c = gcd a (gcd b c)

third (_, _, x, _, _) = x
fourth (_, _, _, x, _) = x
fifth (_, _, _, _, x) = x

orders :: Ord b => [(a -> b)] -> a -> a -> Ordering
orders [f] a b = comparing f a b
orders (f:fx) a b =
  case comparing f a b of
    EQ -> orders fx a b
    n  -> n

main :: IO ()
main = do
  let range = [1 .. 200]
      tris :: [(Integer, Integer, Integer, Integer, Integer)]
      tris = L.sortBy (orders [fifth, fourth, third])
             $ map (\(a, b, c, d, e) -> (a, b, c, d, fromJust e))
             $ filter (isJust . fifth)
             [(a, b, c, a + b + c, heronTri a b c)
             | a <- range, b <- range, c <- range
             , a <= b, b <= c, isPrimitive a b c == 1]
      printTri (a, b, c, d, e) = printf "%3d %3d %3d %9d %4d\n" a b c d e
  printf "Heronian triangles found: %d\n\n" $ length tris
  putStrLn "   Sides    Perimeter Area"
  mapM_ printTri $ take 10 tris
  putStrLn ""
  mapM_ printTri $ filter ((== 210) . fifth) tris
```

```txt
Heronian triangles found: 517

   Sides    Perimeter Area
  3   4   5        12    6
  5   5   6        16   12
  5   5   8        18   12
  4  13  15        32   24
  5  12  13        30   30
  9  10  17        36   36
  3  25  26        54   36
  7  15  20        42   42
 10  13  13        36   60
  8  15  17        40   60

 17  25  28        70  210
 20  21  29        70  210
 12  35  37        84  210
 17  28  39        84  210
  7  65  68       140  210
  3 148 149       300  210
```



## J


'''Hero's formula Implementation'''


```J
a=: 0&{"1
b=: 1&{"1
c=: 2&{"1
s=: (a+b+c) % 2:
area=: 2 %: s*(s-a)*(s-b)*(s-c)                   NB. Hero's formula
perim=: +/"1
isPrimHero=: (0&~: * (= <.@:+))@area * 1 = a +. b +. c
```


We exclude triangles with zero area, triangles with complex area, non-integer area, and triangles whose sides share a common integer multiple.

'''Alternative Implementation'''

The implementation above uses the symbols as given in the formula at the top of the page, making it easier to follow along as well as spot any errors. That formula distinguishes between the individual sides of the triangles but J could easily treat these sides as a single entity or array. The implementation below uses this "typical J" approach:


```J
perim=: +/"1
s=: -:@:perim
area=: [: %: s * [: */"1 s - ]                    NB. Hero's formula
isNonZeroInt=: 0&~: *. (= <.@:+)
isPrimHero=: isNonZeroInt@area *. 1 = +./&.|:
```


'''Required examples'''


```J
   Tri=:(1-i.3)+"1]3 comb 202                     NB. distinct triangles with sides <= 200
   HeroTri=: (#~ isPrimHero) Tri                  NB. all primitive Heronian triangles with sides <= 200

   # HeroTri                                      NB. count triangles found
517

   HeroTri=: (/: area ,. perim ,. ]) HeroTri      NB. sort by area, perimeter & sides

   (,. _ ,. perim ,. area) 10 {. HeroTri          NB. tabulate sides, perimeter & area for top 10 triangles
 3  4  5 _ 12  6
 5  5  6 _ 16 12
 5  5  8 _ 18 12
 4 13 15 _ 32 24
 5 12 13 _ 30 30
 9 10 17 _ 36 36
 3 25 26 _ 54 36
 7 15 20 _ 42 42
10 13 13 _ 36 60
 8 15 17 _ 40 60

   (,. _ ,. perim ,. area) (#~ 210 = area) HeroTri NB. tablulate sides, perimeter & area for triangles with area = 210
17  25  28 _  70 210
20  21  29 _  70 210
12  35  37 _  84 210
17  28  39 _  84 210
 7  65  68 _ 140 210
 3 148 149 _ 300 210
```



## Java


```java
import java.util.ArrayList;

public class Heron {
    public static void main(String[] args) {
        ArrayList<int[]> list = new ArrayList<>();

        for (int c = 1; c <= 200; c++) {
            for (int b = 1; b <= c; b++) {
                for (int a = 1; a <= b; a++) {

                    if (gcd(gcd(a, b), c) == 1 && isHeron(heronArea(a, b, c))){
                        int area = (int) heronArea(a, b, c);
                        list.add(new int[]{a, b, c, a + b + c, area});
                    }
                }
            }
        }
        sort(list);

        System.out.printf("Number of primitive Heronian triangles with sides up "
                + "to 200: %d\n\nFirst ten when ordered by increasing area, then"
                + " perimeter:\nSides       Perimeter   Area", list.size());

        for (int i = 0; i < 10; i++) {
            System.out.printf("\n%d x %d x %d   %d      %d",
                    list.get(i)[0], list.get(i)[1], list.get(i)[2],
                    list.get(i)[3], list.get(i)[4]);
        }

        System.out.printf("\n\nArea = 210\nSides        Perimeter   Area");
        for (int i = 0; i < list.size(); i++) {
            if (list.get(i)[4] == 210)
                System.out.printf("\n%d x %d x %d   %d      %d",
                        list.get(i)[0], list.get(i)[1], list.get(i)[2],
                        list.get(i)[3], list.get(i)[4]);
        }
    }

    public static double heronArea(int a, int b, int c) {
        double s = (a + b + c) / 2f;
        return Math.sqrt(s * (s - a) * (s - b) * (s - c));
    }

    public static boolean isHeron(double h) {
        return h % 1 == 0 && h > 0;
    }

    public static int gcd(int a, int b) {
        int leftover = 1, dividend = a > b ? a : b, divisor = a > b ? b : a;
        while (leftover != 0) {
            leftover = dividend % divisor;
            if (leftover > 0) {
                dividend = divisor;
                divisor = leftover;
            }
        }
        return divisor;
    }

    public static void sort(ArrayList<int[]> list) {
        boolean swapped = true;
        int[] temp;
        while (swapped) {
            swapped = false;
            for (int i = 1; i < list.size(); i++) {
                if (list.get(i)[4] < list.get(i - 1)[4] ||
                        list.get(i)[4] == list.get(i - 1)[4] &&
                        list.get(i)[3] < list.get(i - 1)[3]) {
                    temp = list.get(i);
                    list.set(i, list.get(i - 1));
                    list.set(i - 1, temp);
                    swapped = true;
                }
            }
        }
    }
}
```

```txt
Number of primitive Heronian triangles with sides up to 200: 517

First ten when ordered by increasing area, then perimeter:
Sides		Perimeter	Area
3 x 4 x 5	12		6
5 x 5 x 6	16		12
5 x 5 x 8	18		12
4 x 13 x 15	32		24
5 x 12 x 13	30		30
9 x 10 x 17	36		36
3 x 25 x 26	54		36
7 x 15 x 20	42		42
10 x 13 x 13	36		60
8 x 15 x 17	40		60

Area = 210
Sides		Perimeter	Area
17 x 25 x 28	70		210
20 x 21 x 29	70		210
12 x 35 x 37	84		210
17 x 28 x 39	84		210
7 x 65 x 68	140		210
3 x 148 x 149	300		210
```



## JavaScript



### Imperative



```JavaScript

window.onload = function(){
    var list = [];
    var j = 0;
    for(var c = 1; c <= 200; c++)
        for(var b = 1; b <= c; b++)
            for(var a = 1; a <= b; a++)
	        if(gcd(gcd(a, b), c) == 1 && isHeron(heronArea(a, b, c)))
		    list[j++] = new Array(a, b, c, a + b + c, heronArea(a, b, c));
    sort(list);
    document.write("<h2>Primitive Heronian triangles with sides up to 200: " + list.length + "</h2><h3>First ten when ordered by increasing area, then perimeter:</h3><table><tr><th>Sides</th><th>Perimeter</th><th>Area</th><tr>");
    for(var i = 0; i < 10; i++)
	document.write("<tr><td>" + list[i][0] + " x " + list[i][1] + " x " + list[i][2] + "</td><td>" + list[i][3] + "</td><td>" + list[i][4] + "</td></tr>");
    document.write("</table><h3>Area = 210</h3><table><tr><th>Sides</th><th>Perimeter</th><th>Area</th><tr>");
    for(var i = 0; i < list.length; i++)
	if(list[i][4] == 210)
	    document.write("<tr><td>" + list[i][0] + " x " + list[i][1] + " x " + list[i][2] + "</td><td>" + list[i][3] + "</td><td>" + list[i][4] + "</td></tr>");
    function heronArea(a, b, c){
	var s = (a + b + c)/ 2;
	return Math.sqrt(s *(s -a)*(s - b)*(s - c));
    }
    function isHeron(h){
        return h % 1 == 0 && h > 0;
    }
    function gcd(a, b){
	var leftover = 1, dividend = a > b ? a : b, divisor = a > b ? b : a;
	while(leftover != 0){
	    leftover = dividend % divisor;
	    if(leftover > 0){
		dividend = divisor;
		divisor = leftover;
	    }
	}
	return divisor;
    }
    function sort(list){
	var swapped = true;
	var temp = [];
	while(swapped){
	    swapped = false;
	    for(var i = 1; i < list.length; i++){
		if(list[i][4] < list[i - 1][4] || list[i][4] == list[i - 1][4] && list[i][3] < list[i - 1][3]){
		    temp = list[i];
		    list[i] = list[i - 1];
		    list[i - 1] = temp;
		    swapped = true;
		}
	    }
	}
    }
}

```

```txt
Primitive Heronian triangles with sides up to 200: 517

First ten when ordered by increasing area, then perimeter:
Sides	Perimeter	Area
3 x 4 x 5	12	6
5 x 5 x 6	16	12
5 x 5 x 8	18	12
4 x 13 x 15	32	24
5 x 12 x 13	30	30
9 x 10 x 17	36	36
3 x 25 x 26	54	36
7 x 15 x 20	42	42
10 x 13 x 13	36	60
8 x 15 x 17	40	60

Area = 210
Sides	Perimeter	Area
17 x 25 x 28	70	210
20 x 21 x 29	70	210
12 x 35 x 37	84	210
17 x 28 x 39	84	210
7 x 65 x 68	140	210
3 x 148 x 149	300	210
```


===Functional (ES5)===

Using the list monad pattern to define a filtered cartesian product:
:- Monadic bind/chain for lists is ''concat map''.
:- Return/inject for lists is  ''λx -> [x]''
:- Monadic fail for lists is simply ''λx -> []''.

List comprehension syntax is convenient and concise, but efficient use of it may be helped by a clearer understanding of the formally equivalent – but slightly more flexible – list monad pattern.
See, for example [https://wiki.haskell.org/List_comprehension List comprehension] at wiki.haskell.org. (Haskell list comprehensions are themselves implemented in terms of ''concat map'').
ES6 JavaScript introduces syntactic sugar for list comprehensions, but the list monad pattern can already be used in ES5 – indeed in any language which supports the use of higher-order functions.


```JavaScript
(function (n) {

  var chain = function (xs, f) {                  // Monadic bind/chain
      return [].concat.apply([], xs.map(f));
    },

    hArea = function (x, y, z) {
      var s = (x + y + z) / 2,
        a = s * (s - x) * (s - y) * (s - z);
      return a ? Math.sqrt(a) : 0;
    },

    gcd = function (m, n) { return n ? gcd(n, m % n) : m; },

    rng = function (m, n) {
      return Array.apply(null, Array(n - m + 1)).map(function (x, i) {
        return m + i;
      });
    },

    sum = function (a, x) { return a + x; };

  // DEFINING THE SORTED SUB-SET IN TERMS OF A LIST MONAD

  var lstHeron = chain( rng(1, n), function (x) {
          return chain( rng(x, n), function (y) {
          return chain( rng(y, n), function (z) {

        return (
          (x + y > z) &&
          gcd(gcd(x, y), z) === 1 &&            // Primitive.
          (function () {                        // Heronian.
            var a = hArea(x, y, z);
            return a && (a === parseInt(a, 10))
          })()
        ) ? [[x, y, z]] : [];                   // Monadic inject or fail

  })})}).sort(function (a, b) {
    var dArea = hArea.apply(null, a) - hArea.apply(null, b);
    if (dArea) return dArea;
    else {
      var dPerim = a.reduce(sum, 0) - b.reduce(sum, 0);
      return dPerim ? dPerim : (a[2] - b[2]);
    }
  });

  // OUPUT FORMATTED AS TWO WIKITABLES

  var lstColumns = ['Sides Perimeter Area'.split(' ')],
    fnData = function (lst) {
      return [JSON.stringify(lst), lst.reduce(sum, 0), hArea.apply(null, lst)];
    },
    wikiTable = function (lstRows, blnHeaderRow, strStyle) {
      return '{| class="wikitable" ' + (
        strStyle ? 'style="' + strStyle + '"' : ''
      ) + lstRows.map(function (lstRow, iRow) {
        var strDelim = ((blnHeaderRow && !iRow) ? '!' : '|');

        return '\n|-\n' + strDelim + ' ' + lstRow.map(function (v) {
          return typeof v === 'undefined' ? ' ' : v;
        }).join(' ' + strDelim + strDelim + ' ');
      }).join('') + '\n|}';
    };

  return 'Found: ' + lstHeron.length +
    ' primitive Heronian triangles with sides up to ' + n + '.\n\n' +
    '(Showing first 10, sorted by increasing area, ' +
    'perimeter, and longest side)\n\n' +
    wikiTable(
      lstColumns.concat(lstHeron.slice(0, 10).map(fnData)),
      true
    ) + '\n\n' +
    'All primitive Heronian triangles in this range where area = 210\n' +
    '\n(also in order of increasing perimeter and longest side)\n\n' +
    wikiTable(
      lstColumns.concat(lstHeron.filter(function (x) {
        return 210 === hArea.apply(null, x);
      }).map(fnData)),
      true
    ) + '\n\n';

})(200);
```


Found: 517 primitive Heronian triangles with sides up to 200.

(Showing first 10, sorted by increasing area, perimeter, and longest side)

{| class="wikitable"
|-
! Sides !! Perimeter !! Area
|-
| [3,4,5] || 12 || 6
|-
| [5,5,6] || 16 || 12
|-
| [5,5,8] || 18 || 12
|-
| [4,13,15] || 32 || 24
|-
| [5,12,13] || 30 || 30
|-
| [9,10,17] || 36 || 36
|-
| [3,25,26] || 54 || 36
|-
| [7,15,20] || 42 || 42
|-
| [10,13,13] || 36 || 60
|-
| [8,15,17] || 40 || 60
|}

All primitive Heronian triangles in this range where area = 210

(also in order of increasing perimeter and longest side)

{| class="wikitable"
|-
! Sides !! Perimeter !! Area
|-
| [17,25,28] || 70 || 210
|-
| [20,21,29] || 70 || 210
|-
| [12,35,37] || 84 || 210
|-
| [17,28,39] || 84 || 210
|-
| [7,65,68] || 140 || 210
|-
| [3,148,149] || 300 || 210
|}


## jq

```jq
# input should be an array of the lengths of the sides
def hero:
  (add/2) as $s
  | ($s*($s - .[0])*($s - .[1])*($s - .[2])) as $a2
  | if $a2 > 0 then ($a2 | sqrt) else 0 end;

def is_heronian:
  hero as $h
  | $h > 0 and ($h|floor) == $h;

def gcd3(x; y; z):
  # subfunction expects [a,b] as input
  def rgcd:
    if .[1] == 0 then .[0]
    else [.[1], .[0] % .[1]] | rgcd
    end;
  [ ([x,y] | rgcd), z ] | rgcd;

def task(maxside):
  def rjust(width): tostring |  " " * (width - length) + .;

  [ range(1; maxside+1) as $c
    | range(1; $c+1) as $b
    | range(1; $b+1) as $a
    | if ($a + $b) > $c and gcd3($a; $b; $c) == 1
      then [$a,$b,$c] | if is_heronian then . else empty end
      else empty
      end ]

  # sort by increasing area, perimeter, then sides
  | sort_by( [ hero, add, .[2] ] )
  | "The number of primitive Heronian triangles with sides up to \(maxside): \(length)",
    "The first ten when ordered by increasing area, then perimeter, then maximum sides:",
    "      perimeter area",
    (.[0:10][] | "\(rjust(11)) \(add | rjust(3)) \(hero | rjust(4))" ),
    "All those with area 210, ordered as previously:",
    "      perimeter area",
    ( .[] | select( hero == 210 ) | "\(rjust(11)) \(add|rjust(3)) \(hero|rjust(4))" ) ;

task(200)
```

```sh
$ time jq -n -r -f heronian.jq
The number of primitive Heronian triangles with sides up to 200: 517
The first ten when ordered by increasing area, then perimeter, then maximum sides:
      perimeter area
    [3,4,5]  12    6
    [5,5,6]  16   12
    [5,5,8]  18   12
  [4,13,15]  32   24
  [5,12,13]  30   30
  [9,10,17]  36   36
  [3,25,26]  54   36
  [7,15,20]  42   42
 [10,13,13]  36   60
  [8,15,17]  40   60
All those with area 210, ordered as previously:
      perimeter area
 [17,25,28]  70  210
 [20,21,29]  70  210
 [12,35,37]  84  210
 [17,28,39]  84  210
  [7,65,68] 140  210
[3,148,149] 300  210
```



## Julia

The type <tt>IntegerTriangle</tt> stores a triangle's sides (a, b, c), perimeter (p) and area (&sigma;) as integers.  The function <tt>isprimheronian</tt> checks whether the a triangle of integer sides is a primitive Heronian triangle and is called prior to construction of an <tt>IntegerTriangle</tt>.

'''Types and Functions'''

```Julia

type IntegerTriangle{T<:Integer}
    a::T
    b::T
    c::T
    p::T
    σ::T
end

function IntegerTriangle{T<:Integer}(a::T, b::T, c::T)
    p = a + b + c
    s = div(p, 2)
    σ = isqrt(s*(s-a)*(s-b)*(s-c))
    (x, y, z) = sort([a, b, c])
    IntegerTriangle(x, y, z, p, σ)
end

function isprimheronian{T<:Integer}(a::T, b::T, c::T)
    p = a + b + c
    iseven(p) || return false
    gcd(a, b, c) == 1 || return false
    s = div(p, 2)
    t = s*(s-a)*(s-b)*(s-c)
    0 < t || return false
    σ = isqrt(t)
    σ^2 == t
end

```


'''Main'''

```Julia

slim = 200

ht = IntegerTriangle[]

for a in 1:slim, b in a:slim, c in b:slim
    isprimheronian(a, b, c) || continue
    push!(ht, IntegerTriangle(a, b, c))
end

sort!(ht, by=x->(x.σ, x.p, x.c))

print("The number of primitive Hernonian triangles having sides ≤ ")
println(slim, " is ", length(ht))

tlim = 10
tlim = min(tlim, length(ht))

println()
println("Tabulating the first (by σ, p, c) ", tlim, " of these:")
println("    a   b   c    σ    p")
for t in ht[1:tlim]
    println(@sprintf "%6d %3d %3d %4d %4d" t.a t.b t.c t.σ t.p)
end

tlim = 210
println()
println("Tabulating those having σ = ", tlim, ":")
println("    a   b   c    σ    p")
for t in ht
    t.σ == tlim || continue
    t.σ == tlim || break
    println(@sprintf "%6d %3d %3d %4d %4d" t.a t.b t.c t.σ t.p)
end

```


```txt
The number of primitive Hernonian triangles having sides ≤ 200 is 517

Tabulating the first (by σ, p, c) 10 of these:
    a   b   c    σ    p
     3   4   5    6   12
     5   5   6   12   16
     5   5   8   12   18
     4  13  15   24   32
     5  12  13   30   30
     9  10  17   36   36
     3  25  26   36   54
     7  15  20   42   42
    10  13  13   60   36
     8  15  17   60   40

Tabulating those having σ = 210:
    a   b   c    σ    p
    17  25  28  210   70
    20  21  29  210   70
    12  35  37  210   84
    17  28  39  210   84
     7  65  68  210  140
     3 148 149  210  300
```



## Kotlin

```scala
import java.util.ArrayList

object Heron {
    private val n = 200

    fun run() {
        val l = ArrayList<IntArray>()
        for (c in 1..n)
            for (b in 1..c)
                for (a in 1..b)
                    if (gcd(gcd(a, b), c) == 1) {
                        val p = a + b + c
                        val s = p / 2.0
                        val area = Math.sqrt(s * (s - a) * (s - b) * (s - c))
                        if (isHeron(area))
                            l.add(intArrayOf(a, b, c, p, area.toInt()))
                    }
        print("Number of primitive Heronian triangles with sides up to $n: " + l.size)

        sort(l)
        print("\n\nFirst ten when ordered by increasing area, then perimeter:" + header)
        for (i in 0 until 10) {
            print(format(l[i]))
        }
        val a = 210
        print("\n\nArea = $a" + header)
        l.filter { it[4] == a }.forEach { print(format(it)) }
    }

    private fun gcd(a: Int, b: Int): Int {
        var leftover = 1
        var dividend = if (a > b) a else b
        var divisor = if (a > b) b else a
        while (leftover != 0) {
            leftover = dividend % divisor
            if (leftover > 0) {
                dividend = divisor
                divisor = leftover
            }
        }
        return divisor
    }

    fun sort(l: MutableList<IntArray>) {
        var swapped = true
        while (swapped) {
            swapped = false
            for (i in 1 until l.size)
                if (l[i][4] < l[i - 1][4] || l[i][4] == l[i - 1][4] && l[i][3] < l[i - 1][3]) {
                    val temp = l[i]
                    l[i] = l[i - 1]
                    l[i - 1] = temp
                    swapped = true
                }
        }
    }

    private fun isHeron(h: Double) = h.rem(1) == 0.0 && h > 0

    private val header = "\nSides           Perimeter   Area"
    private fun format(a: IntArray) = "\n%3d x %3d x %3d %5d %10d".format(a[0], a[1], a[2], a[3], a[4])
}

fun main(args: Array<String>) = Heron.run()
```

```txt
Number of primitive Heronian triangles with sides up to 200: 517

First ten when ordered by increasing area, then perimeter:
Sides           Perimeter   Area
  3 x   4 x   5    12          6
  5 x   5 x   6    16         12
  5 x   5 x   8    18         12
  4 x  13 x  15    32         24
  5 x  12 x  13    30         30
  9 x  10 x  17    36         36
  3 x  25 x  26    54         36
  7 x  15 x  20    42         42
 10 x  13 x  13    36         60
  8 x  15 x  17    40         60

Area = 210
Sides           Perimeter   Area
 17 x  25 x  28    70        210
 20 x  21 x  29    70        210
 12 x  35 x  37    84        210
 17 x  28 x  39    84        210
  7 x  65 x  68   140        210
  3 x 148 x 149   300        210
```



## Lua


```lua
-- Returns the details of the Heronian Triangle with sides a, b, c or nil if it isn't one
local function tryHt( a, b, c )
    local result
    local s = ( a + b + c ) / 2;
    local areaSquared = s * ( s - a ) * ( s - b ) * ( s - c );
    if areaSquared > 0 then
        -- a, b, c does form a triangle
        local area = math.sqrt( areaSquared );
        if math.floor( area ) == area then
            -- the area is integral so the triangle is Heronian
            result = { a = a, b = b, c = c, perimeter = a + b + c, area = area }
        end
    end
    return result
end

-- Returns the GCD of a and b
local function gcd( a, b ) return ( b == 0 and a ) or gcd( b, a % b ) end

-- Prints the details of the Heronian triangle t
local function htPrint( t ) print( string.format( "%4d %4d %4d %4d      %4d", t.a, t.b, t.c, t.area, t.perimeter ) ) end
-- Prints headings for the Heronian Triangle table
local function htTitle() print( "   a    b    c area perimeter" ); print( "---- ---- ---- ---- ---------" ) end

-- Construct ht as a table of the Heronian Triangles with sides up to 200
local ht = {};
for c = 1, 200 do
    for b = 1, c do
        for a = 1, b do
            local t = gcd( gcd( a, b ), c ) == 1 and tryHt( a, b, c );
            if t then
                ht[ #ht + 1 ] = t
            end
        end
    end
end

-- sort the table on ascending area, perimiter and max side length
-- note we constructed the triangles with c as the longest side
table.sort( ht, function( a, b )
                return a.area < b.area or (   a.area == b.area
                                          and (  a.perimeter <  b.perimeter
                                              or (   a.perimiter == b.perimiter
                                                 and a.c         <  b.c
                                                 )
                                              )
                                          )
                end
           );

-- Display the triangles
print( "There are " .. #ht .. " Heronian triangles with sides up to 200" );
htTitle();
for htPos = 1, 10 do htPrint( ht[ htPos ] ) end
print( " ..." );
print( "Heronian triangles with area 210:" );
htTitle();
for htPos = 1, #ht do
    local t = ht[ htPos ];
    if t.area == 210 then htPrint( t ) end
end
```

```txt

There are 517 Heronian triangles with sides up to 200
   a    b    c area perimeter
---- ---- ---- ---- ---------
   3    4    5    6        12
   5    5    6   12        16
   5    5    8   12        18
   4   13   15   24        32
   5   12   13   30        30
   9   10   17   36        36
   3   25   26   36        54
   7   15   20   42        42
  10   13   13   60        36
   8   15   17   60        40
 ...
Heronian triangles with area 210:
   a    b    c area perimeter
---- ---- ---- ---- ---------
  17   25   28  210        70
  20   21   29  210        70
  12   35   37  210        84
  17   28   39  210        84
   7   65   68  210       140
   3  148  149  210       300

```



## Nim


```nim
import math, algorithm, strfmt, sequtils

type
  HeronianTriangle = tuple
    a: int
    b: int
    c: int
    s: float
    A: int

proc `$` (t: HeronianTriangle): string =
  fmt("{:3d}, {:3d}, {:3d}\t{:7.3f}\t{:3d}", t.a, t.b, t.c, t.s, t.A)

proc hero(a:int, b:int, c:int): tuple[s, A: float] =
  let s: float = (a + b + c) / 2
  result = (s, sqrt( s * (s - float(a)) * (s - float(b)) * (s - float(c)) ))

proc isHeronianTriangle(x: float): bool = ceil(x) == x and x.toInt > 0

proc gcd(x: int, y: int): int =
  var
    (dividend, divisor) = if x > y: (x, y) else: (y, x)
    remainder = dividend mod divisor

  while remainder != 0:
    dividend = divisor
    divisor = remainder
    remainder = dividend mod divisor
  result = divisor


var list = newSeq[HeronianTriangle]()
const max = 200

for c in 1..max:
  for b in 1..c:
    for a in 1..b:
      let (s, A) = hero(a, b, c)
      if isHeronianTriangle(A) and gcd(a, gcd(b, c)) == 1:
        let t:HeronianTriangle = (a, b, c, s, A.toInt)
        list.add(t)

echo "Numbers of Heronian Triangle : ", list.len

list.sort do (x, y: HeronianTriangle) -> int:
  result = cmp(x.A, y.A)
  if result == 0:
    result = cmp(x.s, y.s)
    if result == 0:
      result = cmp(max(x.a, x.b, x.c), max(y.a, y.b, y.c))

echo "Ten first Heronian triangle ordered : "
echo "Sides          Perimeter Area"
for t in list[0 .. <10]:
  echo t

echo "Heronian triangle ordered with Area 210 : "
echo "Sides          Perimeter Area"
for t in list.filter(proc (x: HeronianTriangle): bool = x.A == 210):
  echo t
```

```txt
Numbers of Heronian Triangle : 517
Ten first Heronian triangle ordered :
Sides          Perimeter Area
  3,   4,   5	  6.000	  6
  5,   5,   6	  8.000	 12
  5,   5,   8	  9.000	 12
  4,  13,  15	 16.000	 24
  5,  12,  13	 15.000	 30
  9,  10,  17	 18.000	 36
  3,  25,  26	 27.000	 36
  7,  15,  20	 21.000	 42
 10,  13,  13	 18.000	 60
  8,  15,  17	 20.000	 60
Heronian triangle ordered with Area 210 :
Sides          Perimeter Area
 17,  25,  28	 35.000	210
 20,  21,  29	 35.000	210
 12,  35,  37	 42.000	210
 17,  28,  39	 42.000	210
  7,  65,  68	 70.000	210
  3, 148, 149	150.000	210
```



## ooRexx

Derived from REXX with some changes

```rexx
/*REXX pgm generates primitive Heronian triangles by side length & area.*/
  Call time 'R'
  Numeric Digits 12
  Parse Arg mxs area list
  If mxs ='' Then mxs =200
  If area='' Then area=210
  If list='' Then list=10
  tx='primitive Heronian triangles'
  Call heronian mxs            /* invoke sub with max SIDES.     */
  Say nt tx 'found with side length up to' mxs "(inclusive)."
  Call show '2'
  Call show '3'
  Say time('E') 'seconds elapsed'
  Exit

heronian:
  abc.=0  /* abc.ar.p.* contains 'a b c' for area ar and perimeter p */
  nt=0                              /* number of triangles found     */
  min.=''
  max.=''
  mem.=0
  ln=length(mxs)
  Do a=3 To mxs
    Do b=a To mxs
      ab=a+b
      Do c=b To mxs
        If hgcd(a,b,c)=1 Then Do    /* GCD=1                         */
          ar=heron_area()
          If pos('.',ar)=0 Then Do  /* is an integer                 */
            nt=nt+1                 /* a primitive Heronian triangle.*/
            Call minmax '0P',p
            Call minmax '0A',a
            per=ab+c
            abc_ar=right(per,4) right(a,4) right(b,4) right(c,4),
                                                            right(ar,5)
            Call mem abc_ar
            End
          End
        End
      End
    End
  /*
  say 'min.p='min.0p
  say 'max.p='max.0p
  say 'min.a='min.0a
  say 'max.a='max.0a
  */
  Return nt

hgcd: Procedure
  Parse Arg x
  Do j=2 For 2
    y=arg(j)
    Do Until _==0
      _=x//y
      x=y
      y=_
      End
    End
  Return x

minmax:
  Parse Arg which,x
  If min.which='' Then Do
    min.which=x
    max.which=x
    End
  Else Do
    min.which=min(min.which,x)
    max.which=max(max.which,x)
    End
  --Say which min.which '-' max.which
  Return

heron_area:
  p=ab+c                           /* perimeter                      */
  s=p/2
  ar2=s*(s-a)*(s-b)*(s-c)          /* area**2                        */
  If pos(right(ar2,1),'014569')=0 Then /* ar2 cannot be              */
    Return '.'                         /* square of an integer*/
  If ar2>0 Then
    ar=sqrt(ar2)                   /* area                           */
  Else
    ar='.'
  Return ar

show: Parse Arg which
  Say ''
  Select
    When which='2' Then Do
      Say 'Listing of the first' list tx":"
      Do i=1 To list
        Call ot i,mem.i
        End
      End
    When which='3' Then Do
      Say 'Listing of the' tx "with area=210"
      j=0
      Do i=1 To mem.0
        Parse Var mem.i per a b c area
        If area=210 Then Do
          j=j+1
          Call ot j,mem.i
          End
        End
      End
    End
  Return

ot: Parse Arg k,mem
    Parse Var mem per a b c area
    Say right(k,9)'     area:'right(area,6)||,
                '      perimeter:'right(per,4)'     sides:',
                       right(a,3) right(b,3) right(c,3)
    Return

mem:
  Parse Arg e
  Do i=1 To mem.0
    If mem.i>>e Then Leave
    End
  Do j=mem.0 to i By -1
    j1=j+1
    mem.j1=mem.j
    End
  mem.i=e
  mem.0=mem.0+1
  Return
/* for "Classic" REXX
sqrt: procedure; parse arg x;if x=0 then return 0;d=digits();numeric digits 11
numeric form;  parse value format(x,2,1,,0) 'E0' with g 'E' _ .;  g=g*.5'E'_%2
p=d+d%4+2; m.=11;  do j=0 while p>9; m.j=p; p=p%2+1; end;  do k=j+5 to 0 by -1
if m.k>11 then numeric digits m.k;g=.5*(g+x/g);end;numeric digits d;return g/1
*/
/* for ooRexx */
::requires rxmath library
::routine sqrt
  Return rxCalcSqrt(arg(1),14)
```

```txt
517 primitive Heronian triangles found with side length up to 200 (inclusive).

Listing of the first 10 primitive Heronian triangles:
        1     area:     6      perimeter:  12     sides:   3   4   5
        2     area:    12      perimeter:  16     sides:   5   5   6
        3     area:    12      perimeter:  18     sides:   5   5   8
        4     area:    30      perimeter:  30     sides:   5  12  13
        5     area:    24      perimeter:  32     sides:   4  13  15
        6     area:    36      perimeter:  36     sides:   9  10  17
        7     area:    60      perimeter:  36     sides:  10  13  13
        8     area:    60      perimeter:  40     sides:   8  15  17
        9     area:    42      perimeter:  42     sides:   7  15  20
       10     area:    84      perimeter:  42     sides:  13  14  15

Listing of the primitive Heronian triangles with area=210
        1     area:   210      perimeter:  70     sides:  17  25  28
        2     area:   210      perimeter:  70     sides:  20  21  29
        3     area:   210      perimeter:  84     sides:  12  35  37
        4     area:   210      perimeter:  84     sides:  17  28  39
        5     area:   210      perimeter: 140     sides:   7  65  68
        6     area:   210      perimeter: 300     sides:   3 148 149
26.054000 seconds elapsed
```



## PARI/GP



```parigp
Heron(v)=my([a,b,c]=v); (a+b+c)*(-a+b+c)*(a-b+c)*(a+b-c) \\ returns 16 times the squared area
is(a,b,c)=(a+b+c)%2==0 && gcd(a,gcd(b,c))==1 && issquare(Heron([a,b,c]))
v=List(); for(a=1,200,for(b=a+1,200,for(c=b+1,200, if(is(a,b,c),listput(v, [a,b,c])))));
v=Vec(v); #v
vecsort(v, (a,b)->Heron(a)-Heron(b))[1..10]
vecsort(v, (a,b)->vecsum(a)-vecsum(b))[1..10]
vecsort(v, 3)[1..10] \\ shortcut: order by third component
u=select(v->Heron(v)==705600, v);
vecsort(u, (a,b)->Heron(a)-Heron(b))
vecsort(u, (a,b)->vecsum(a)-vecsum(b))
vecsort(u, 3) \\ shortcut: order by third component
```

```txt
%1 = [[1, 2, 3], [1, 3, 4], [1, 4, 5], [1, 5, 6], [1, 6, 7], [1, 7, 8], [1, 8, 9], [1, 9, 10], [1, 10, 11], [1, 11, 12]]
%2 = [[1, 2, 3], [1, 3, 4], [1, 4, 5], [2, 3, 5], [1, 5, 6], [3, 4, 5], [1, 6, 7], [2, 5, 7], [3, 4, 7], [1, 7, 8]]
%3 = [[1, 2, 3], [1, 3, 4], [1, 4, 5], [2, 3, 5], [3, 4, 5], [1, 5, 6], [1, 6, 7], [2, 5, 7], [3, 4, 7], [1, 7, 8]]
%4 = [[3, 148, 149], [7, 65, 68], [12, 35, 37], [17, 25, 28], [17, 28, 39], [20, 21, 29]]
%5 = [[17, 25, 28], [20, 21, 29], [12, 35, 37], [17, 28, 39], [7, 65, 68], [3, 148, 149]]
%6 = [[17, 25, 28], [20, 21, 29], [12, 35, 37], [17, 28, 39], [7, 65, 68], [3, 148, 149]]
```



## Pascal

```pascal
program heronianTriangles ( input, output );
type
    (* record to hold details of a Heronian triangle *)
    Heronian    = record a, b, c, area, perimeter : integer end;
    refHeronian = ^Heronian;

var

    ht             : array [ 1 .. 1000 ] of refHeronian;
    htCount, htPos : integer;
    a, b, c, i     : integer;
    lower, upper   : integer;
    k, h, t        : refHeronian;
    swapped        : boolean;

    (* returns the details of the Heronian Triangle with sides a, b, c or nil if it isn't one *)
    function tryHt( a, b, c : integer ) : refHeronian;
    var
        s, areaSquared, area : real;
        t                    : refHeronian;
    begin
        s           := ( a + b + c ) / 2;
        areaSquared := s * ( s - a ) * ( s - b ) * ( s - c );
        t           := nil;
        if areaSquared > 0 then begin
            (* a, b, c does form a triangle *)
            area    := sqrt( areaSquared );
            if trunc( area ) = area then begin
                (* the area is integral so the triangle is Heronian *)
                new(t);
                t^.a := a; t^.b := b; t^.c := c; t^.area := trunc( area ); t^.perimeter := a + b + c
            end
        end;
        tryHt := t
    end (* tryHt *) ;

    (* returns the GCD of a and b *)
    function gcd( a, b : integer ) : integer;
    begin
        if b = 0 then gcd := a else gcd := gcd( b, a mod b )
    end (* gcd *) ;

    (* prints the details of the Heronian triangle t *)
    procedure htPrint( t : refHeronian ) ; begin writeln( t^.a:4, t^.b:5, t^.c:5, t^.area:5, t^.perimeter:10 ) end;
    (* prints headings for the Heronian Triangle table *)
    procedure htTitle ; begin writeln( '   a    b    c area perimeter' ); writeln( '---- ---- ---- ---- ---------' ) end;

begin
    (* construct ht as a table of the Heronian Triangles with sides up to 200 *)
    htCount := 0;
    for c := 1 to 200 do begin
        for b := 1 to c do begin
            for a := 1 to b do begin
                if gcd( gcd( a, b ), c ) = 1 then begin
                    t := tryHt( a, b, c );
                    if t <> nil then begin
                        htCount       := htCount + 1;
                        ht[ htCount ] := t
                    end
                end
            end
        end
    end;

    (* sort the table on ascending area, perimeter and max side length *)
    (* note we constructed the triangles with c as the longest side *)
    lower := 1;
    upper := htCount;
    repeat
        upper   := upper - 1;
        swapped := false;
        for i := lower to upper do begin
            h := ht[ i     ];
            k := ht[ i + 1 ];
            if ( k^.area < h^.area ) or (   ( k^.area =  h^.area )
                                        and (  ( k^.perimeter <  h^.perimeter )
                                            or (   ( k^.perimeter = h^.perimeter )
                                               and ( k^.c <  h^.c )
                                               )
                                            )
                                        )
            then begin
                ht[ i     ] := k;
                ht[ i + 1 ] := h;
                swapped     := true
            end
        end;
    until not swapped;

    (* display the triangles *)
    writeln( 'There are ', htCount:1, ' Heronian triangles with sides up to 200' );
    htTitle;
    for htPos := 1 to 10 do htPrint( ht[ htPos ] );
    writeln( ' ...' );
    writeln( 'Heronian triangles with area 210:' );
    htTitle;
    for htPos := 1 to htCount do begin
        t := ht[ htPos ];
        if t^.area = 210 then htPrint( t )
    end
end.
```

```txt

There are 517 Heronian triangles with sides up to 200
   a    b    c area perimeter
---- ---- ---- ---- ---------
   3    4    5    6        12
   5    5    6   12        16
   5    5    8   12        18
   4   13   15   24        32
   5   12   13   30        30
   9   10   17   36        36
   3   25   26   36        54
   7   15   20   42        42
  10   13   13   60        36
   8   15   17   60        40
 ...
Heronian triangles with area 210:
   a    b    c area perimeter
---- ---- ---- ---- ---------
  17   25   28  210        70
  20   21   29  210        70
  12   35   37  210        84
  17   28   39  210        84
   7   65   68  210       140
   3  148  149  210       300

```



## Perl

```perl
use strict;
use warnings;
use List::Util qw(max);

sub gcd { $_[1] == 0 ? $_[0] : gcd($_[1], $_[0] % $_[1]) }

sub hero {
    my ($a, $b, $c) = @_[0,1,2];
    my $s = ($a + $b + $c) / 2;
    sqrt $s*($s - $a)*($s - $b)*($s - $c);
}

sub heronian_area {
    my $hero = hero my ($a, $b, $c) = @_[0,1,2];
    sprintf("%.0f", $hero) eq $hero ? $hero : 0
}

sub primitive_heronian_area {
    my ($a, $b, $c) = @_[0,1,2];
    heronian_area($a, $b, $c) if 1 == gcd $a, gcd $b, $c;
}

sub show {
    print "   Area Perimeter   Sides\n";
    for (@_) {
        my ($area, $perim, $c, $b, $a) = @$_;
	printf "%7d %9d    %d×%d×%d\n", $area, $perim, $a, $b, $c;
    }
}

sub main {
    my $maxside = shift // 200;
    my $first = shift // 10;
    my $witharea = shift // 210;
    my @h;
    for my $c (1 .. $maxside) {
	for my $b (1 .. $c) {
	    for my $a ($c - $b + 1 .. $b) {
		if (my $area = primitive_heronian_area $a, $b, $c) {
		    push @h, [$area, $a+$b+$c, $c, $b, $a];
		}
	    }
	}
    }
    @h = sort {
	$a->[0] <=> $b->[0]
	    or
	$a->[1] <=> $b->[1]
	    or
	max(@$a[2,3,4]) <=> max(@$b[2,3,4])
    } @h;
    printf "Primitive Heronian triangles with sides up to %d: %d\n",
    $maxside,
    scalar @h;
    print "First:\n";
    show @h[0 .. $first - 1];
    print "Area $witharea:\n";
    show grep { $_->[0] == $witharea } @h;


}

&main();
```

```txt
Primitive Heronian triangles with sides up to 200: 517
First:
   Area Perimeter   Sides
      6        12    3×4×5
     12        16    5×5×6
     12        18    5×5×8
     24        32    4×13×15
     30        30    5×12×13
     36        36    9×10×17
     36        54    3×25×26
     42        42    7×15×20
     60        36    10×13×13
     60        40    8×15×17
Area 210:
   Area Perimeter   Sides
    210        70    17×25×28
    210        70    20×21×29
    210        84    12×35×37
    210        84    17×28×39
    210       140    7×65×68
    210       300    3×148×149
```



## Perl 6

```perl6
sub hero($a, $b, $c) {
    my $s = ($a + $b + $c) / 2;
    ($s * ($s - $a) * ($s - $b) * ($s - $c)).sqrt;
}

sub heronian-area($a, $b, $c) {
    $_ when Int given hero($a, $b, $c).narrow;
}

sub primitive-heronian-area($a, $b, $c) {
    heronian-area $a, $b, $c
        if 1 == [gcd] $a, $b, $c;
}

sub show(@measures) {
    say "   Area Perimeter   Sides";
    for @measures -> [$area, $perim, $c, $b, $a] {
	printf "%6d %6d %12s\n", $area, $perim, "$a×$b×$c";
    }
}

sub MAIN ($maxside = 200, $first = 10, $witharea = 210) {
    my @hh[1000];
    my atomicint $i;
    (1 .. $maxside).race(:12batch).map: -> $c {
        for 1 .. $c -> $b {
            for $c - $b + 1 .. $b -> $a {
                if primitive-heronian-area($a,$b,$c) -> $area {
                    @hh[$i⚛++] = [$area, $a+$b+$c, $c, $b, $a];
                }
            }
        }
    }

    my @h = (@hh.grep: so *).sort;
    say "Primitive Heronian triangles with sides up to $maxside: ", +@h;

    say "\nFirst $first:";
    show @h[^$first];

    say "\nArea $witharea:";
    show @h.grep: *[0] == $witharea;
}
```

```txt
Primitive Heronian triangles with sides up to 200: 517

First 10:
   Area Perimeter   Sides
     6     12        3×4×5
    12     16        5×5×6
    12     18        5×5×8
    24     32      4×13×15
    30     30      5×12×13
    36     36      9×10×17
    36     54      3×25×26
    42     42      7×15×20
    60     36     10×13×13
    60     40      8×15×17

Area 210:
   Area Perimeter   Sides
   210     70     17×25×28
   210     70     20×21×29
   210     84     12×35×37
   210     84     17×28×39
   210    140      7×65×68
   210    300    3×148×149
```



## Phix


```Phix
function heroArea(integer a, b, c)
atom s = (a+b+c)/2
    return sqrt(s*(s-a)*(s-b)*(s-c))
end function

function hero(atom h)
    return remainder(h,1)=0 and h>0
end function

sequence list = {}
integer tries = 0
    for a=1 to 200 do
        for b=1 to a do
            for c=1 to b do
                tries += 1
                if gcd({a,b,c})=1 then
                    atom hArea = heroArea(a,b,c)
                    if hero(hArea) then
                        list = append(list,{hArea,a+b+c,a,b,c})
                    end if
                end if
            end for
        end for
    end for
    list = sort(list)
    printf(1,"Primitive Heronian triangles with sides up to 200: %d (of %,d tested)\n\n",{length(list),tries})
    printf(1,"First 10 ordered by area/perimeter/sides:\n")
    printf(1,"area  perimeter sides")
    for i=1 to 10 do
        printf(1,"\n%4d     %3d    %dx%dx%d",list[i])
    end for
    printf(1,"\n\narea = 210:\n")
    printf(1,"area  perimeter sides")
    for i=1 to length(list) do
        if list[i][1]=210 then
            printf(1,"\n%4d     %3d    %dx%dx%d",list[i])
        end if
    end for
```

```txt

Primitive Heronian triangles with sides up to 200: 517 (of 1,353,400 tested)

First 10 ordered by area/perimeter/sides:
area  perimeter sides
   6      12    5x4x3
  12      16    6x5x5
  12      18    8x5x5
  24      32    15x13x4
  30      30    13x12x5
  36      36    17x10x9
  36      54    26x25x3
  42      42    20x15x7
  60      36    13x13x10
  60      40    17x15x8

area = 210:
area  perimeter sides
 210      70    28x25x17
 210      70    29x21x20
 210      84    37x35x12
 210      84    39x28x17
 210     140    68x65x7
 210     300    149x148x3

```



## PowerShell


```powershell

function Get-Gcd($a, $b){
    if($a -ge $b){
        $dividend = $a
        $divisor = $b
    }
    else{
        $dividend = $b
        $divisor = $a
    }
    $leftover = 1
    while($leftover -ne 0){
        $leftover = $dividend % $divisor
        if($leftover -ne 0){
				$dividend = $divisor
				$divisor = $leftover
			}
    }
    $divisor
}
function Is-Heron($heronArea){
    $heronArea -gt 0 -and $heronArea % 1 -eq 0
}
function Get-HeronArea($a, $b, $c){
    $s = ($a + $b + $c) / 2
    [math]::Sqrt($s * ($s - $a) * ($s - $b) * ($s - $c))
}
$result = @()
foreach ($c in 1..200){
    for($b = 1; $b -le $c; $b++){
        for($a = 1; $a -le $b; $a++){
            if((Get-Gcd $c (Get-Gcd $b $a)) -eq 1 -and (Is-Heron(Get-HeronArea $a $b $c))){
                $result += @(,@($a, $b, $c,($a + $b + $c), (Get-HeronArea $a $b $c)))
            }
        }
    }
}
$result = $result | sort-object @{Expression={$_[4]}}, @{Expression={$_[3]}}, @{Expression={$_[2]}}
"Primitive Heronian triangles with sides up to 200: $($result.length)`nFirst ten when ordered by increasing area, then perimeter,then maximum sides:`nSides`t`t`t`tPerimeter`tArea"
for($i = 0; $i -lt 10; $i++){
"$($result[$i][0])`t$($result[$i][1])`t$($result[$i][2])`t`t`t$($result[$i][3])`t`t`t$($result[$i][4])"
}
"`nArea = 210`nSides`t`t`t`tPerimeter`tArea"
foreach($i in $result){
    if($i[4] -eq 210){
        "$($i[0])`t$($i[1])`t$($i[2])`t`t`t$($i[3])`t`t`t$($i[4])"
    }
}

```

<lang>
Primitive Heronian triangles with sides up to 200: 517

First ten when ordered by increasing area, then perimeter,then maximum sides:
Sides				Perimeter	Area
3	4	5		12		6
5	5	6		16		12
5	5	8		18		12
4	13	15		32		24
5	12	13		30		30
9	10	17		36		36
3	25	26		54		36
7	15	20		42		42
10	13	13		36		60
8	15	17		40		60

Area = 210
Sides				Perimeter	Area
17	25	28		70		210
20	21	29		70		210
12	35	37		84		210
17	28	39		84		210
7	65	68		140		210
3	148	149		300		210

```



## Python



```python
from __future__ import division, print_function
from math import gcd, sqrt


def hero(a, b, c):
    s = (a + b + c) / 2
    a2 = s * (s - a) * (s - b) * (s - c)
    return sqrt(a2) if a2 > 0 else 0


def is_heronian(a, b, c):
    a = hero(a, b, c)
    return a > 0 and a.is_integer()


def gcd3(x, y, z):
    return gcd(gcd(x, y), z)


if __name__ == '__main__':
    MAXSIDE = 200

    N = 1 + MAXSIDE
    h = [(x, y, z)
         for x in range(1, N)
         for y in range(x, N)
         for z in range(y, N) if (x + y > z) and
         1 == gcd3(x, y, z) and
         is_heronian(x, y, z)]

    # By increasing area, perimeter, then sides
    h.sort(key=lambda x: (hero(*x), sum(x), x[::-1]))

    print(
        'Primitive Heronian triangles with sides up to %i:' % MAXSIDE, len(h)
    )
    print('\nFirst ten when ordered by increasing area, then perimeter,',
          'then maximum sides:')
    print('\n'.join('  %14r perim: %3i area: %i'
                    % (sides, sum(sides), hero(*sides)) for sides in h[:10]))
    print('\nAll with area 210 subject to the previous ordering:')
    print('\n'.join('  %14r perim: %3i area: %i'
                    % (sides, sum(sides), hero(*sides)) for sides in h
                    if hero(*sides) == 210))

```

```txt
Primitive Heronian triangles with sides up to 200: 517

First ten when ordered by increasing area, then perimeter,then maximum sides:
       (3, 4, 5) perim:  12 area: 6
       (5, 5, 6) perim:  16 area: 12
       (5, 5, 8) perim:  18 area: 12
     (4, 13, 15) perim:  32 area: 24
     (5, 12, 13) perim:  30 area: 30
     (9, 10, 17) perim:  36 area: 36
     (3, 25, 26) perim:  54 area: 36
     (7, 15, 20) perim:  42 area: 42
    (10, 13, 13) perim:  36 area: 60
     (8, 15, 17) perim:  40 area: 60

All with area 210 subject to the previous ordering:
    (17, 25, 28) perim:  70 area: 210
    (20, 21, 29) perim:  70 area: 210
    (12, 35, 37) perim:  84 area: 210
    (17, 28, 39) perim:  84 area: 210
     (7, 65, 68) perim: 140 area: 210
   (3, 148, 149) perim: 300 area: 210
```



## R


Mostly adopted from Python implementation:


```R

area <- function(a, b, c) {
    s = (a + b + c) / 2
    a2 = s*(s-a)*(s-b)*(s-c)
    if (a2>0) sqrt(a2) else 0
}

is.heronian <- function(a, b, c) {
    h = area(a, b, c)
    h > 0 && 0==h%%1
}

# borrowed from stackoverflow http://stackoverflow.com/questions/21502181/finding-the-gcd-without-looping-r
gcd <- function(x,y) {
  r <- x%%y;
  ifelse(r, gcd(y, r), y)
}

gcd3 <- function(x, y, z) {
    gcd(gcd(x, y), z)
}

maxside = 200
r <- NULL
for(c in 1:maxside){
    for(b in 1:c){
        for(a in 1:b){
            if(1==gcd3(a, b, c) && is.heronian(a, b, c)) {
                r <- rbind(r,c(a=a, b=b, c=c, perimeter=a+b+c, area=area(a,b,c)))
            }
        }
    }
}

cat("There are ",nrow(r)," Heronian triangles up to a maximal side length of ",maxside,".\n", sep="")
cat("Showing the first ten ordered first by perimeter, then by area:\n")
print(head(r[order(x=r[,"perimeter"],y=r[,"area"]),],n=10))

```


<lang>There are 517 Heronian triangles up to a maximal side length of 200.
Showing the first ten ordered first by perimeter, then by area:
       a  b  c perimeter area
 [1,]  3  4  5        12    6
 [2,]  5  5  6        16   12
 [3,]  5  5  8        18   12
 [4,]  5 12 13        30   30
 [5,]  4 13 15        32   24
 [6,]  9 10 17        36   36
 [7,] 10 13 13        36   60
 [8,]  8 15 17        40   60
 [9,]  7 15 20        42   42
[10,] 13 14 15        42   84
```



## Racket


<lang>#lang at-exp racket
(require data/order scribble/html)

;; Returns the area of a triangle iff the sides have gcd 1, and it is an
;; integer; #f otherwise
(define (heronian?-area a b c)
  (and (= 1 (gcd a b c))
       (let ([s (/ (+ a b c) 2)])  ; ** If s=\frac{a+b+c}{2}
         (and (integer? s)         ; (s must be an integer for the area to b an integer)
              (let-values ([[q r] (integer-sqrt/remainder ; (faster than sqrt)
                                   ; ** Then the area is \sqrt{s(s-a)(s-b)(s-c)}
                                   (* s (- s a) (- s b) (- s c)))])
                (and (zero? r) q)))))) ; (return only integer areas)

(define (generate-heronian-triangles max-side)
  (for*/list ([c (in-range 1 (add1 max-side))]
              [b (in-range 1 (add1 c))] ; b<=c
              [a (in-range (add1 (- c b)) (add1 b))] ; ensures a<=b and c<a+b
              [area (in-value (heronian?-area a b c))]
              #:when area)
    ;; datum-order can sort this for the tables (c is the max side length)
    (list area (+ a b c) c (list a b c))))

;; Order the triangles by first increasing area, then by increasing perimeter,
;; then by increasing maximum side lengths
(define (tri-sort triangles)
  (sort triangles (λ(t1 t2) (eq? '< (datum-order t1 t2)))))

(define (triangles->table triangles)
  (table
   (tr (map th '("#" sides perimeter area))) "\n"
   (for/list ([i (in-naturals 1)] [triangle (in-list triangles)])
     (match-define (list area perimeter max-side sides) triangle)
     (tr (td i) (td (add-between sides ",")) (td perimeter) (td area) "\n"))))

(module+ main
  (define ts (generate-heronian-triangles 200))
  (output-xml
   @div{@p{number of primitive triangles found with perimeter @entity{le} 200 = @(length ts)}
        @; Show the first ten ordered triangles in a table of sides, perimeter,
        @; and area.
        @(triangles->table (take (tri-sort ts) 10))
        @; Show a similar ordered table for those triangles with area = 210
        @(triangles->table (tri-sort (filter (λ(t) (eq? 210 (car t))) ts)))
        }))
```


This program generates HTML, so the output is inline with the page, not in a <code>&lt;pre></code> block.

<div><p>number of primitive triangles found with perimeter &le; 200 = 517</p>
<table><tr><th>#</th><th>sides</th><th>perimeter</th><th>area</th></tr>
<tr><td>1</td><td>3,4,5</td><td>12</td><td>6</td>
</tr><tr><td>2</td><td>5,5,6</td><td>16</td><td>12</td>
</tr><tr><td>3</td><td>5,5,8</td><td>18</td><td>12</td>
</tr><tr><td>4</td><td>4,13,15</td><td>32</td><td>24</td>
</tr><tr><td>5</td><td>5,12,13</td><td>30</td><td>30</td>
</tr><tr><td>6</td><td>9,10,17</td><td>36</td><td>36</td>
</tr><tr><td>7</td><td>3,25,26</td><td>54</td><td>36</td>
</tr><tr><td>8</td><td>7,15,20</td><td>42</td><td>42</td>
</tr><tr><td>9</td><td>10,13,13</td><td>36</td><td>60</td>
</tr><tr><td>10</td><td>8,15,17</td><td>40</td><td>60</td>
</tr></table>
<table><tr><th>#</th><th>sides</th><th>perimeter</th><th>area</th></tr>
<tr><td>1</td><td>17,25,28</td><td>70</td><td>210</td>
</tr><tr><td>2</td><td>20,21,29</td><td>70</td><td>210</td>
</tr><tr><td>3</td><td>12,35,37</td><td>84</td><td>210</td>
</tr><tr><td>4</td><td>17,28,39</td><td>84</td><td>210</td>
</tr><tr><td>5</td><td>7,65,68</td><td>140</td><td>210</td>
</tr><tr><td>6</td><td>3,148,149</td><td>300</td><td>210</td>
</tr></table></div>





## REXX


###  using iSQRT

This REXX version makes use of these facts:
:::*   if   '''A'''   is even,   then   '''B'''   and   '''C'''   must be odd.
:::*   if   '''B'''   is even,   then   '''C'''                 must be odd.
:::*   if   '''A'''    and   '''B'''    are odd,   then   '''C'''   must be even.
:::*   with the 1<sup>st</sup> three truisms, then:
::::::*   '''C'''   can be incremented by   <big>'''2'''</big>.
::::::*   the area is always even.


Programming notes:

The   '''hGCD'''   subroutine is a specialized version of a '''GCD''' routine in that:
:::*   it doesn't check for non-positive integers
:::*   it expects exactly three arguments


Also, a fair amount of code was added to optimize the speed   (at the expense of program simplicity).

By thoughtful ordering of the elimination checks, and also the use of an   ''integer version''
of a   '''SQRT'''

subroutine,   the execution time was greatly reduced   (by a factor of eight).


Note that the   '''hIsqrt'''   ('''h'''eronian '''I'''nteger '''sq'''are '''r'''oo'''t''')
subroutine doesn't use floating point.

['''hIsqrt'''   is a modified/simplified version of an   '''Isqrt'''   function.]

This REXX version doesn't need to explicitly sort the triangles as they are listed in the proper order.

```rexx
/*REXX program generates & displays primitive Heronian triangles by side length and area*/
parse arg  N  first  area  .                     /*obtain optional arguments from the CL*/
if     N==''  |     N==","  then     N= 200      /*Not specified?  Then use the default.*/
if first==''  | first==","  then first=  10      /* "      "         "   "   "     "    */
if  area==''  |  area==","  then  area= 210      /* "      "         "   "   "     "    */
numeric digits 99                                /*ensure 'nuff dec. digs to calc. N**5.*/
numeric digits max(9, 1 + length(N**5) )         /*minimize decimal digits for REXX pgm.*/
call Heron;       HT= 'Heronian triangles'       /*invoke the  Heron  subroutine.       */
say  #          ' primitive'  HT  "found with sides up to "   N  ' (inclusive).'
call show     , 'Listing of the first '      first      ' primitive'      HT":"
call show area, 'Listing of the (above) found primitive'   HT   "with an area of "    area
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Heron: @.= 0;  minP= 9e9;  maxP= 0;  maxA= 0;  minA= 9e9;  Ln= length(N)    /*        __*/
               #= 0;  #.= 0;  #.2= 1;  #.3= 1;  #.7= 1;  #.8= 1       /*digits ¬good √  */
  do a=3  to N                                   /*start at a minimum side length of 3. */
           Aeven= a//2==0                        /*if  A  is even,  B and C must be odd.*/
    do b=a+Aeven  to N  by 1+Aeven;   ab= a + b  /*AB: a shortcut for the sum of A & B. */
    if b//2==0  then                bump= 1      /*Is  B  even?       Then  C  is odd.  */
                else if Aeven  then bump= 0      /*Is  A  even?         "   "   "  "    */
                               else bump= 1      /*A and B  are both odd,  biz as usual.*/
      do c=b+bump  to N  by 2;   s= (ab + c) % 2 /*calculate triangle's perimeter:   S. */
      _= s*(s-a)*(s-b)*(s-c); if _<=0   then iterate /*is  _  not positive?      Skip it*/
      parse var _ '' -1 z   ; if #.z    then iterate /*Last digit not square?    Skip it*/
      ar= hIsqrt(_);       if ar*ar\==_ then iterate /*Is area not an integer?   Skip it*/
      if hGCD(a, b, c) \== 1            then iterate /*GCD of sides not equal 1? Skip it*/
      #= # + 1;                p= ab + c             /*primitive Heronian triangle.     */
      minP= min( p, minP);     maxP= max( p, maxP);        Lp= length(maxP)
      minA= min(ar, minA);     maxA= max(ar, maxA);        La= length(maxA)
      _=@.ar.p.0 + 1                                 /*bump Heronian triangle counter.  */
      @.ar.p.0= _;  @.ar.p._= right(a, Ln)   right(b, Ln)   right(c, Ln)       /*unique.*/
      end   /*c*/                                    /* [↑]  keep each unique perimeter#*/
    end     /*b*/
  end       /*a*/;             return #              /*return # of Heronian triangles.  */
/*──────────────────────────────────────────────────────────────────────────────────────*/
hGCD: x=a;   do j=2  for 2;    y= arg(j);       do until y==0; parse value x//y y with y x
                                                end   /*until*/
             end   /*j*/;      return x
/*──────────────────────────────────────────────────────────────────────────────────────*/
hIsqrt: procedure; parse arg x;  q= 1;  r= 0;                  do  while q<=x;    q= q * 4
                                                               end   /*while q<=x*/
          do  while q>1; q=q%4; _= x-r-q; r= r%2; if _>=0  then parse value _ r+q with x r
          end   /*while q>1*/;          return r
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: m=0;  say;  say;   parse arg ae;     say arg(2);         if ae\==''  then first= 9e9
      say;  $=left('',9);   $a= $"area:";  $p= $'perimeter:';  $s= $"sides:"  /*literals*/
            do   i=minA  to maxA;  if ae\=='' & i\==ae  then iterate          /*= area? */
              do j=minP  to maxP  until m>=first      /*only display the  FIRST entries.*/
                do k=1  for @.i.j.0;    m= m + 1      /*display each  perimeter  entry. */
                say right(m, 9)   $a    right(i, La)    $p   right(j, Lp)    $s    @.i.j.k
                end   /*k*/
              end     /*j*/                           /* [↑]  use the known perimeters. */
            end       /*i*/;            return        /* [↑]  show any found triangles. */
```

```txt

517  primitive Heronian triangles found with sides up to  200  (inclusive).


Listing of the first  10  primitive Heronian triangles:

        1          area:     6          perimeter:  12          sides:   3   4   5
        2          area:    12          perimeter:  16          sides:   5   5   6
        3          area:    12          perimeter:  18          sides:   5   5   8
        4          area:    24          perimeter:  32          sides:   4  13  15
        5          area:    30          perimeter:  30          sides:   5  12  13
        6          area:    36          perimeter:  36          sides:   9  10  17
        7          area:    36          perimeter:  54          sides:   3  25  26
        8          area:    42          perimeter:  42          sides:   7  15  20
        9          area:    60          perimeter:  36          sides:  10  13  13
       10          area:    60          perimeter:  40          sides:   8  15  17


Listing of the (above) found primitive Heronian triangles with an area of  210

        1          area:   210          perimeter:  70          sides:  17  25  28
        2          area:   210          perimeter:  70          sides:  20  21  29
        3          area:   210          perimeter:  84          sides:  12  35  37
        4          area:   210          perimeter:  84          sides:  17  28  39
        5          area:   210          perimeter: 140          sides:   7  65  68
        6          area:   210          perimeter: 300          sides:   3 148 149

```



### using SQRT table

This REXX version makes use of a precalculated table of squares of some
integers   (which are used to find square roots very quickly).

It is about eight times faster than the 1<sup>st</sup> REXX version.

```rexx
/*REXX program generates & displays primitive Heronian triangles by side length and area*/
parse arg  N  first  area  .                     /*obtain optional arguments from the CL*/
if     N==''  |     N==","  then     N= 200      /*Not specified?  Then use the default.*/
if first==''  | first==","  then first=  10      /* "      "         "   "   "     "    */
if  area==''  |  area==","  then  area= 210      /* "      "         "   "   "     "    */
numeric digits 99; numeric digits max(9, 1+length(N**5))  /*ensure 'nuff decimal digits.*/
call Heron;       HT= 'Heronian triangles'       /*invoke the  Heron  subroutine.       */
say  #          ' primitive'    HT    "found with sides up to "     N      ' (inclusive).'
call show     , 'Listing of the first '       first        ' primitive'            HT":"
call show area, 'Listing of the (above) found primitive'   HT   "with an area of "    area
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Heron: @.= 0;   #= 0;   !.= .;   minP= 9e9;  maxA= 0;  maxP= 0;  minA= 9e9;  Ln= length(N)
                   do i=1  for N**2%2;    _= i*i;      !._= i               /*     __   */
                   end   /*i*/                   /* [↑]  pre─calculate some fast  √     */
  do a=3  to N                                   /*start at a minimum side length of 3. */
           Aeven= a//2==0                        /*if  A  is even,  B and C must be odd.*/
    do b=a+Aeven  to N  by 1+Aeven;   ab= a + b  /*AB: a shortcut for the sum of A & B. */
    if b//2==0  then                bump= 1      /*Is  B  even?       Then  C  is odd.  */
                else if Aeven  then bump= 0      /*Is  A  even?         "   "   "  "    */
                               else bump= 1      /*A and B  are both odd,  biz as usual.*/
      do c=b+bump  to N  by 2;   s= (ab + c) % 2 /*calculate triangle's perimeter:   S. */
      _= s*(s-a)*(s-b)*(s-c);  if !._==.     then iterate  /*Is  _  not a square?  Skip.*/
      if hGCD(a,b,c) \== 1                   then iterate  /*GCD of sides not 1?   Skip.*/
      #= # + 1;     p= ab + c;   ar= !._                   /*primitive Heronian triangle*/
      minP= min( p, minP);     maxP= max( p, maxP);       Lp= length(maxP)
      minA= min(ar, minA);     maxA= max(ar, maxA);       La= length(maxA);         @.ar=
      _= @.ar.p.0  +  1                                    /*bump the triangle counter. */
      @.ar.p.0= _;    @.ar.p._= right(a, Ln)    right(b, Ln)    right(c, Ln)    /*unique*/
      end   /*c*/                                /* [↑]  keep each unique perimeter #.  */
    end     /*b*/
  end       /*a*/;    return #                   /*return number of Heronian triangles. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
hGCD: x=a;  do j=2  for 2;   y= arg(j);         do until y==0; parse value x//y y with y x
                                                end   /*until*/
            end   /*j*/;                return x
/*──────────────────────────────────────────────────────────────────────────────────────*/
show: m=0;  say;  say;   parse arg ae;     say arg(2);         if ae\==''  then first= 9e9
      say;  $=left('',9);   $a= $"area:";  $p= $'perimeter:';  $s= $"sides:"  /*literals*/
            do   i=minA  to maxA;  if ae\=='' & i\==ae  then iterate          /*= area? */
              do j=minP  to maxP  until m>=first      /*only display the  FIRST entries.*/
                do k=1  for @.i.j.0;    m= m + 1      /*display each  perimeter  entry. */
                say right(m, 9)   $a    right(i, La)    $p   right(j, Lp)    $s    @.i.j.k
                end   /*k*/
              end     /*j*/                           /* [↑]  use the known perimeters. */
            end       /*i*/;            return        /* [↑]  show any found triangles. */
```

## Ring


```ring

# Project : Heronian triangles

see "Heronian triangles with sides up to 200" + nl
see "Sides               Perimeter       Area" + nl
for n = 1 to 200
    for m = n to 200
       for p = m to 200
           s = (n + m + p) / 2
           w = sqrt(s * (s - n) * (s - m) * (s - p))
           bool = (gcd(n, m) = 1 or gcd(n, m) = n) and (gcd(n, p) = 1 or gcd(n, p) = n) and (gcd(m, p) = 1 or gcd(m, p) = m)
           if w = floor(w) and w > 0 and bool
              see "{" + n + ", " + m + ", " + p + "}" + "              " + s*2 + "              " + w + nl
           ok
       next
    next
next
see nl

see "Heronian triangles with area 210:" + nl
see "Sides               Perimeter       Area" + nl
for n = 1 to 150
    for m = n to 150
        for p = m to 150
            s = (n + m + p) / 2
            w = sqrt(s * (s - n) * (s - m) * (s - p))
            bool = (gcd(n, m) = 1 or gcd(n, m) = n) and (gcd(n, p) = 1 or gcd(n, p) = n) and (gcd(m, p) = 1 or gcd(m, p) = m)
            if w = 210 and bool
               see "{" + n + ", " + m + ", " + p + "}" + "              " + s*2 + "              " + w + nl
            ok
        next
    next
next

func gcd(gcd, b)
       while b
               c   = gcd
               gcd = b
               b   = c % b
       end
       return gcd

```

Output:

```txt

Heronian triangles with sides up to 200
Sides               Perimeter       Area
{3, 4, 5}              12            6
{3, 25, 26}          54            36
{4, 13, 15}          32            24
{5, 5, 6}              16            12
{5, 5, 8}              18            12
{5, 12, 13}          30            30
{7, 15, 20 }         42            42
{8, 15, 17}          40            60
{9, 10, 17}          36            36
{10, 13, 13}         36            60
{13, 13, 24}         50            60

Heronian triangles with area 210:
Sides               Perimeter       Area
{3, 148, 149}         300          210
{7, 65, 68}            140           210
{12, 35, 37}            84           210
{17, 25, 28}            70           210
{17, 28, 39}            84           210
{20, 21, 29}            70           210

```



## Ruby


```ruby
class Triangle
  def self.valid?(a,b,c)      # class method
    short, middle, long = [a, b, c].sort
    short + middle > long
  end

  attr_reader :sides, :perimeter, :area

  def initialize(a,b,c)
    @sides = [a, b, c].sort
    @perimeter = a + b + c
    s = @perimeter / 2.0
    @area = Math.sqrt(s * (s - a) * (s - b) * (s - c))
  end

  def heronian?
    area == area.to_i
  end

  def <=>(other)
    [area, perimeter, sides] <=> [other.area, other.perimeter, other.sides]
  end

  def to_s
    "%-11s%6d%8.1f" % [sides.join('x'), perimeter, area]
  end
end

max, area = 200, 210
prim_triangles = []
1.upto(max) do |a|
  a.upto(max) do |b|
    b.upto(max) do |c|
      next if a.gcd(b).gcd(c) > 1
      prim_triangles << Triangle.new(a, b, c) if Triangle.valid?(a, b, c)
    end
  end
end

sorted = prim_triangles.select(&:heronian?).sort

puts "Primitive heronian triangles with sides upto #{max}: #{sorted.size}"
puts "\nsides       perim.   area"
puts sorted.first(10).map(&:to_s)
puts "\nTriangles with an area of: #{area}"
sorted.each{|tr| puts tr if tr.area == area}
```

```txt

Primitive heronian triangles with sides upto 200: 517

sides       perim.   area
3x4x5          12     6.0
5x5x6          16    12.0
5x5x8          18    12.0
4x13x15        32    24.0
5x12x13        30    30.0
9x10x17        36    36.0
3x25x26        54    36.0
7x15x20        42    42.0
10x13x13       36    60.0
8x15x17        40    60.0

Triangles with an area of: 210
17x25x28       70   210.0
20x21x29       70   210.0
12x35x37       84   210.0
17x28x39       84   210.0
7x65x68       140   210.0
3x148x149     300   210.0

```



## Scala


```scala
object Heron extends scala.collection.mutable.MutableList[Seq[Int]] with App {
    private final val n = 200
    for (c <- 1 to n; b <- 1 to c; a <- 1 to b if gcd(gcd(a, b), c) == 1) {
        val p = a + b + c
        val s = p / 2D
        val area = Math.sqrt(s * (s - a) * (s - b) * (s - c))
        if (isHeron(area))
            appendElem(Seq(a, b, c, p, area.toInt))
    }
    print(s"Number of primitive Heronian triangles with sides up to $n: " + length)

    private final val list = sortBy(i => (i(4), i(3)))
    print("\n\nFirst ten when ordered by increasing area, then perimeter:" + header)
    list slice (0, 10) map format foreach print
    print("\n\nArea = 210" + header)
    list filter { _(4) == 210 } map format foreach print

    private def gcd(a: Int, b: Int) = {
        var leftover = 1
        var (dividend, divisor) = if (a > b) (a, b) else (b, a)
        while (leftover != 0) {
            leftover = dividend % divisor
            if (leftover > 0) {
                dividend = divisor
                divisor = leftover
            }
        }
        divisor
    }

    private def isHeron(h: Double) = h % 1 == 0 && h > 0

    private final val header = "\nSides           Perimeter   Area"
    private def format: Seq[Int] => String = "\n%3d x %3d x %3d %5d %10d".format
}
```



## Sidef

```ruby
class Triangle(a, b, c) {

  has (sides, perimeter, area)

  method init {
    sides = [a, b, c].sort
    perimeter = [a, b, c].sum
    var s = (perimeter / 2)
    area = sqrt(s * (s - a) * (s - b) * (s - c))
  }

  method is_valid(a,b,c) {
    var (short, middle, long) = [a, b, c].sort...;
    (short + middle) > long
  }

  method is_heronian {
    area == area.to_i
  }

  method <=>(other) {
    [area, perimeter, sides] <=> [other.area, other.perimeter, other.sides]
  }

  method to_s {
    "%-11s%6d%8.1f" % (sides.join('x'), perimeter, area)
  }
}

var (max, area) = (200, 210)
var prim_triangles = []

for a in (1..max) {
  for b in (a..max) {
    for c in (b..max) {
      next if (Math.gcd(a, b, c) > 1)
      prim_triangles << Triangle(a, b, c) if Triangle.is_valid(a, b, c)
    }
  }
}

var sorted = prim_triangles.grep{.is_heronian}.sort

say "Primitive heronian triangles with sides upto #{max}: #{sorted.size}"
say "\nsides       perim.   area"
say sorted.first(10).join("\n")
say "\nTriangles with an area of: #{area}"
sorted.each{|tr| say tr if (tr.area == area)}
```

```txt

Primitive heronian triangles with sides upto 200: 517

sides       perim.   area
3x4x5          12     6.0
5x5x6          16    12.0
5x5x8          18    12.0
4x13x15        32    24.0
5x12x13        30    30.0
9x10x17        36    36.0
3x25x26        54    36.0
7x15x20        42    42.0
10x13x13       36    60.0
8x15x17        40    60.0

Triangles with an area of: 210
17x25x28       70   210.0
20x21x29       70   210.0
12x35x37       84   210.0
17x28x39       84   210.0
7x65x68       140   210.0
3x148x149     300   210.0

```



## SPL


```spl
h,t = getem(200)
#.sort(h,4,5,1,2,3)
#.output("There are ",t," Heronian triangles")
#.output("   a     b     c   area  perimeter")
#.output("----- ----- ----- ------ ---------")
> i, 1..#.min(10,t)
  print(h,i)
<
#.output(#.str("...",">34<"))
> i, 1..t
  ? h[4,i]=210, print(h,i)
<
print(h,i)=
  #.output(#.str(h[1,i],">4>"),"  ",#.str(h[2,i],">4>"),"  ",#.str(h[3,i],">4>"),"  ",#.str(h[4,i],">5>"),"  ",#.str(h[5,i],">8>"))
.
getem(n)=
  > a, 1..n
    > b, #.upper((a+1)/2)..a
      > c, a-b+1..b
        x = ((a+b+c)*(a+b-c)*(a-b+c)*(b-a+c))^0.5
        >> x%1 | #.gcd(a,b,c)>1
        t += 1
        h[1,t],h[2,t],h[3,t] = #.sort(a,b,c)
        h[4,t],h[5,t] = heron(a,b,c)
      <
    <
  <
  <= h,t
.
heron(a,b,c)=
  s = (a+b+c)/2
  <= (s*(s-a)*(s-b)*(s-c))^0.5, s*2
.
```

```txt

There are 517 Heronian triangles
   a     b     c   area  perimeter
----- ----- ----- ------ ---------
   3     4     5      6        12
   5     5     6     12        16
   5     5     8     12        18
   4    13    15     24        32
   5    12    13     30        30
   9    10    17     36        36
   3    25    26     36        54
   7    15    20     42        42
  10    13    13     60        36
   8    15    17     60        40
               ...
  17    25    28    210        70
  20    21    29    210        70
  12    35    37    210        84
  17    28    39    210        84
   7    65    68    210       140
   3   148   149    210       300

```



## Swift

Works with Swift 1.2

```Swift
import Foundation

typealias PrimitiveHeronianTriangle = (s1:Int, s2:Int, s3:Int, p:Int, a:Int)

func heronianArea(side1 s1:Int, side2 s2:Int, side3 s3:Int) -> Int? {
    let d1 = Double(s1)
    let d2 = Double(s2)
    let d3 = Double(s3)

    let s = (d1 + d2 + d3) / 2.0
    let a = sqrt(s * (s - d1) * (s - d2) * (s - d3))

    if a % 1 != 0 || a <= 0 {
        return nil
    } else {
        return Int(a)
    }
}

func gcd(a:Int, b:Int) -> Int {
    if b != 0 {
        return gcd(b, a % b)
    } else {
        return abs(a)
    }
}

var triangles = [PrimitiveHeronianTriangle]()

for s1 in 1...200 {
    for s2 in 1...s1 {
        for s3 in 1...s2 {
            if gcd(s1, gcd(s2, s3)) == 1, let a = heronianArea(side1: s1, side2: s2, side3: s3) {
                triangles.append((s3, s2, s1, s1 + s2 + s3, a))
            }
        }
    }
}

sort(&triangles) {t1, t2 in
    if t1.a < t2.a || t1.a == t2.a && t1.p < t2.p {
        return true
    } else {
        return false
    }
}

println("Number of primitive Heronian triangles with sides up to 200: \(triangles.count)\n")
println("First ten sorted by area, then perimeter, then maximum side:\n")
println("Sides\t\t\tPerimeter\tArea")

for t in triangles[0...9] {
    println("\(t.s1)\t\(t.s2)\t\(t.s3)\t\t\(t.p)\t\t\(t.a)")
}
```


```txt

Number of primitive Heronian triangles with sides up to 200: 517

First ten sorted by area, then perimeter, then maximum side:

Sides			Perimeter	Area
3	4	5		12		6
5	5	6		16		12
5	5	8		18		12
4	13	15		32		24
5	12	13		30		30
9	10	17		36		36
3	25	26		54		36
7	15	20		42		42
10	13	13		36		60
8	15	17		40		60


```



## Tcl


```tcl

if {[info commands let] eq ""} {

    #make some math look nicer:
    proc let {name args} {
        tailcall ::set $name [uplevel 1 $args]
    }
    interp alias {} = {} expr
    namespace import ::tcl::mathfunc::* ::tcl::mathop::*
    interp alias {} sum {} +

    # a simple adaptation of gcd from http://wiki.tcl.tk/2891
    proc coprime {a args} {
        set gcd $a
        foreach arg $args {
            while {$arg != 0} {
                set t $arg
                let arg = $gcd % $arg
                set gcd $t
                if {$gcd == 1} {return true}
            }
        }
        return false
    }
}

namespace eval Hero {

    # Integer square root:  returns 0 if n is not a square.
    proc isqrt? {n} {
        let r = entier(sqrt($n))
        if {$r**2 == $n} {
            return $r
        } else {
            return 0
        }
    }

    # The square of a triangle's area
    proc squarea {a b c} {
        let s = ($a + $b + $c) / 2.0
        let sqrA = $s * ($s - $a) * ($s - $b) * ($s - $c)
        return $sqrA
    }

    proc is_heronian {a b c} {
        isqrt? [squarea $a $b $c]
    }

    proc primitive_triangles {db max} {
        for {set a 1} {$a <= $max} {incr a} {
            for {set b $a} {$b <= $max} {incr b} {
                let maxc = min($a+$b,$max)
                for {set c $b} {$c <= $maxc} {incr c} {
                    set area [is_heronian $a $b $c]
                    if {$area && [coprime $a $b $c]} {
                        set perimiter [expr {$a + $b + $c}]
                        $db eval {insert into herons (area, perimiter, a, b, c) values ($area, $perimiter, $a, $b, $c)}
                    }
                }
            }
        }
    }
}

proc main {db} {
    $db eval {create table herons (area int, perimiter int, a int, b int, c int)}

    set max 200
    puts "Calculating Primitive Heronian triangles up to size length $max"
    puts \t[time {Hero::primitive_triangles $db $max} 1]

    puts "Total Primitive Heronian triangles with side lengths <= $max:"
    $db eval {select count(1) count from herons} {
        puts "\t$count"
    }

    puts "First ten when ordered by increasing area, perimiter, max side length:"
    $db eval {select * from herons order by area, perimiter, c limit 10} {
        puts "\t($a, $b, $c)  perimiter = $perimiter;  area = $area"
    }

    puts "All of area 210:"
    $db eval {select * from herons where area=210 order by area, perimiter, c} {
        puts "\t($a, $b, $c)  perimiter = $perimiter;  area = $area"
    }
}


package require sqlite3
sqlite3 db :memory:
main db

```

```txt

Calculating Primitive Heronian triangles up to size length 200
        11530549 microseconds per iteration
Total Primitive Heronian triangles with side lengths <= 200:
        517
First ten when ordered by increasing area, perimiter, max side length:
        (3, 4, 5)  perimiter = 12;  area = 6
        (5, 5, 6)  perimiter = 16;  area = 12
        (5, 5, 8)  perimiter = 18;  area = 12
        (4, 13, 15)  perimiter = 32;  area = 24
        (5, 12, 13)  perimiter = 30;  area = 30
        (9, 10, 17)  perimiter = 36;  area = 36
        (3, 25, 26)  perimiter = 54;  area = 36
        (7, 15, 20)  perimiter = 42;  area = 42
        (10, 13, 13)  perimiter = 36;  area = 60
        (8, 15, 17)  perimiter = 40;  area = 60
All of area 210:
        (17, 25, 28)  perimiter = 70;  area = 210
        (20, 21, 29)  perimiter = 70;  area = 210
        (12, 35, 37)  perimiter = 84;  area = 210
        (17, 28, 39)  perimiter = 84;  area = 210
        (7, 65, 68)  perimiter = 140;  area = 210
        (3, 148, 149)  perimiter = 300;  area = 210

```


## VBA

```vb
Function heroArea(a As Integer, b As Integer, c As Integer) As Double
    s = (a + b + c) / 2
    On Error GoTo Err
    heroArea = Sqr(s * (s - a) * (s - b) * (s - c))
    Exit Function
Err:
    heroArea = -1
End Function

Function hero(h As Double) As Boolean
    hero = (h - Int(h) = 0) And h > 0
End Function

Public Sub main()
    Dim list() As Variant, items As Integer
    Dim a As Integer, b As Integer, c As Integer
    Dim hArea As Double
    Dim tries As Long
    For a = 1 To 200
        For b = 1 To a
            For c = 1 To b
                tries = tries + 1
                If gcd(gcd(a, b), c) = 1 Then
                    hArea = heroArea(a, b, c)
                    If hero(hArea) Then
                        ReDim Preserve list(items)
                        list(items) = Array(CStr(hArea), CStr(a + b + c), CStr(a), CStr(b), CStr(c))
                        items = items + 1
                    End If
                End If
            Next c
        Next b
    Next a
    list = sort(list)
    Debug.Print "Primitive Heronian triangles with sides up to 200:"; UBound(list) + 1; "(of"; tries; "tested)"
    Debug.Print
    Debug.Print "First 10 ordered by area/perimeter/sides:"
    Debug.Print "area       perimeter        sides"
    For i = 0 To 9
        Debug.Print Format(list(i)(0), "@@@"), Format(list(i)(1), "@@@"),
        Debug.Print list(i)(2); "x"; list(i)(3); "x"; list(i)(4)
    Next i
    Debug.Print
    Debug.Print "area = 210:"
    Debug.Print "area       perimeter        sides"
    For i = 0 To UBound(list)
        If Val(list(i)(0)) = 210 Then
            Debug.Print Format(list(i)(0), "@@@"), Format(list(i)(1), "@@@"),
            Debug.Print list(i)(2); "x"; list(i)(3); "x"; list(i)(4)
        End If
    Next i
End Sub
```
```txt
Primitive Heronian triangles with sides up to 200: 517 (of 1353400 tested)

First 10 ordered by area/perimeter/sides:
area       perimeter        sides
  6            12           5x4x3
 12            16           6x5x5
 12            18           8x5x5
 24            32           15x13x4
 30            30           13x12x5
 36            36           17x10x9
 36            54           26x25x3
 42            42           20x15x7
 60            36           13x13x10
 60            40           17x15x8

area = 210:
area       perimeter        sides
210            70           28x25x17
210            70           29x21x20
210            84           37x35x12
210            84           39x28x17
210           140           68x65x7
210           300           149x148x3
```


## zkl

```zkl
fcn hero(a,b,c){  //--> area (float)
   s,a2:=(a + b + c).toFloat()/2, s*(s - a)*(s - b)*(s - c);
   (a2 > 0) and a2.sqrt() or 0.0
}
fcn isHeronian(a,b,c){
   A:=hero(a,b,c);
   (A>0) and A.modf()[1].closeTo(0.0,1.0e-6) and A  //--> area or False
}
```


```zkl
const MAX_SIDE=200;
heros:=Sink(List);
foreach a,b,c in ([1..MAX_SIDE],[a..MAX_SIDE],[b..MAX_SIDE]){
   if(a.gcd(b).gcd(c)==1 and (h:=isHeronian(a,b,c))) heros.write(T(h,a+b+c,a,b,c));
}
// sort by increasing area, perimeter, then sides
heros=heros.close().sort(fcn([(h1,p1,_,_,c1)],[(h2,p2,_,_,c2)]){
   if(h1!=h2) return(h1<h2);
   if(p1!=p2) return(p1<p2);
   c1<c2;
});

println("Primitive Heronian triangles with sides up to %d: ".fmt(MAX_SIDE),heros.len());

println("First ten when ordered by increasing area, then perimeter,then maximum sides:");
println("Area Perimeter Sides");
heros[0,10].pump(fcn(phabc){ "%3s %8d %3dx%dx%d".fmt(phabc.xplode()).println() });

println("\nAll with area 210 subject to the previous ordering:");
println("Area Perimeter Sides");
heros.filter(fcn([(h,_)]){ h==210 })
  .pump(fcn(phabc){ "%3s %8d %3dx%dx%d".fmt(phabc.xplode()).println() });
```

```txt

Primitive Heronian triangles with sides up to 200: 517
First ten when ordered by increasing area, then perimeter,then maximum sides:
Area Perimeter Sides
  6       12   3x4x5
 12       16   5x5x6
 12       18   5x5x8
 24       32   4x13x15
 30       30   5x12x13
 36       36   9x10x17
 36       54   3x25x26
 42       42   7x15x20
 60       36  10x13x13
 60       40   8x15x17

All with area 210 subject to the previous ordering:
Area Perimeter Sides
210       70  17x25x28
210       70  20x21x29
210       84  12x35x37
210       84  17x28x39
210      140   7x65x68
210      300   3x148x149

```


