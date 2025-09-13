+++
title = "Visualize a tree"
description = ""
date = 2019-10-18T20:49:31Z
aliases = []
[extra]
id = 12276
[taxonomies]
categories = ["task"]
tags = []
+++

A tree structure   (i.e. a rooted, connected acyclic graph)   is often used in programming.

It's often helpful to visually examine such a structure.

There are many ways to represent trees to a reader, such as:
:::*   indented text   (à la unix <code> tree </code> command)
:::*   nested HTML tables
:::*   hierarchical GUI widgets
:::*   2D   or   3D   images
:::*   etc.

## Task

Write a program to produce a visual representation of some tree.

The content of the tree doesn't matter, nor does the output format, the only requirement being that the output is human friendly.

Make do with the vague term "friendly" the best you can.





## 11l

```11l
T Node
   String value
   Node? left
   Node? right

   F (value, Node? left = N, Node? right = N)
      .value = String(value)
      .left = left
      .right = right

   F tree_indent() -> [String]
      V tr = I .right != N {.right.tree_indent()} E [‘-- (null)’]
      R [‘--’(.value)] [+] (I .left != N {.left.tree_indent()} E [‘-- (null)’]).map(a -> ‘  |’a)
                       [+] [‘  `’tr[0]] + tr[1..].map(a -> ‘   ’a)

V tree = Node(1, Node(2, Node(4, Node(7)), Node(5)), Node(3, Node(6, Node(8), Node(9))))
print(tree.tree_indent().join("\n"))
```



## Ada

Prints a tree of the current directory.

```Ada
with Ada.Text_IO, Ada.Directories;

procedure Directory_Tree is

   procedure Print_Tree(Current: String; Indention: Natural := 0) is

      function Spaces(N: Natural) return String is
	 (if N= 0 then "" else " " & Spaces(N-1));

      use Ada.Directories;
      Search: Search_Type;
      Found: Directory_Entry_Type;

   begin
      Start_Search(Search, Current, "");
      while More_Entries(Search) loop
	 Get_Next_Entry(Search, Found);
	 declare
	    Name: String := Simple_Name(Found);
	    Dir: Boolean := Kind(Found) = Directory;
	 begin
	    if Name(Name'First) /= '.' then
               -- skip all files who's names start with ".", namely "." and ".."
	       Ada.Text_IO.Put_Line(Spaces(2*Indention) & Simple_Name(Found)
		  & (if Dir then " (dir)" else ""));
	       if Dir then
		  Print_Tree(Full_Name(Found), Indention + 1);
	       end if;
	    end if;
	 end;
     end loop;
   end Print_Tree;

begin
   Print_Tree(Ada.Directories.Current_Directory);
end Directory_Tree;
```


```txt
outer (dir)
  inner (dir)
    innermost (dir)
      file
      another
file
some
```



## ALGOL 68


```algol68
# outputs nested html tables to visualise a tree #

# mode representing nodes of the tree #
MODE NODE = STRUCT( STRING value, REF NODE child, REF NODE sibling );
REF  NODE nil node = NIL;

# tags etc. #
STRING table = "<table border=""1"" cellspacing=""4"">"
     , elbat = "</table>"
     , tr    = "<tr>"
     , rt    = "</tr>"
     , td    = "<td style=""text-align: center; vertical-align: top; """
     , dt    = "</td>"
     , nbsp  = " "
     ;
CHAR   nl    = REPR 10;

# returns the number of child elements of tree #
OP CHILDCOUNT = ( REF NODE tree )INT:
    BEGIN
        INT      result := 0;
        REF NODE child  := child OF tree;
        WHILE REF NODE( child ) ISNT nil node
        DO
            result +:= 1;
            child   := sibling OF child
        OD;
        result
    END # CHILDCOUNT # ;

# generates nested HTML tables from the tree #
OP  TOHTML = ( REF NODE tree )STRING:
    IF tree IS nil node
    THEN
        # no node #
        ""
    ELSE
        # hae at least one node #
        STRING result     := "";
        INT    child count = CHILDCOUNT tree;
        result +:= table + nl
                 +   tr  + nl
                 +     td + " colspan="""
                          + whole( IF child count < 1 THEN 1 ELSE child count FI, 0 )
                          + """>" + nbsp + value OF tree + nbsp
                 +     dt + nl
                 +   rt  + nl
                 ;
        IF child count > 0
        THEN
            # the node has branches #
            REF NODE  child  := child OF tree;
            INT child number := 1;
            INT mid child     = ( child count + 1 ) OVER 2;
            child   := child OF tree;
            result +:= tr + nl;
            WHILE child ISNT nil node
            DO
                result +:= td + ">" + nl
                         + IF CHILDCOUNT child < 1 THEN nbsp + value OF child + nbsp ELSE TOHTML child FI
                         + dt + nl;
                child := sibling OF child
            OD;
            result +:= rt + nl
        FI;
        result +:= elbat + nl
    FI # TOHTML # ;

# test the tree visualisation #

# returns a new node with the specified value and no child or siblings #
PROC new node = ( STRING value )REF NODE: HEAP NODE := NODE( value, nil node, nil node );
# appends a sibling node to the node n, returns the sibling #
OP +:= = ( REF NODE n, REF NODE sibling node )REF NODE:
    BEGIN
        REF NODE sibling := n;
        WHILE REF NODE( sibling OF sibling ) ISNT nil node
        DO
            sibling := sibling OF sibling
        OD;
        sibling OF sibling := sibling node
    END # +:= # ;
# appends a new sibling node to the node n, returns the sibling #
OP +:= = ( REF NODE n, STRING   sibling value )REF NODE: n +:= new node( sibling value );
# adds a child node to the node n, returns the child #
OP /:= = ( REF NODE n, REF NODE child node    )REF NODE: child OF n := child node;
# adda a new child node to the node n, returns the child #
OP /:= = ( REF NODE n, STRING   child value   )REF NODE: n /:= new node( child value );

NODE animals  := new node( "animals"  );
NODE fish     := new node( "fish"     );
NODE reptiles := new node( "reptiles" );
NODE mammals  := new node( "mammals"  );
NODE primates := new node( "primates" );
NODE sharks   := new node( "sharks"   );
sharks   /:= "great-white" +:= "hammer-head";
fish     /:= "cod"         +:= sharks           +:= "piranha";
reptiles /:= "iguana"      +:= "brontosaurus";
primates /:= "gorilla"     +:= "lemur";
mammals  /:= "sloth"       +:= "horse"          +:= "bison" +:= primates;
animals  /:= fish          +:= reptiles         +:= mammals;

print( ( TOHTML animals ) )
```

<table border="1" cellspacing="4">
<tr>
<td style="text-align: center; vertical-align: top; " colspan="3"> animals </td>
</tr>
<tr>
<td style="text-align: center; vertical-align: top; ">
<table border="1" cellspacing="4">
<tr>
<td style="text-align: center; vertical-align: top; " colspan="3"> fish </td>
</tr>
<tr>
<td style="text-align: center; vertical-align: top; ">
 cod </td>
<td style="text-align: center; vertical-align: top; ">
<table border="1" cellspacing="4">
<tr>
<td style="text-align: center; vertical-align: top; " colspan="2"> sharks </td>
</tr>
<tr>
<td style="text-align: center; vertical-align: top; ">
 great-white </td>
<td style="text-align: center; vertical-align: top; ">
 hammer-head </td>
</tr>
</table>
</td>
<td style="text-align: center; vertical-align: top; ">
 piranha </td>
</tr>
</table>
</td>
<td style="text-align: center; vertical-align: top; ">
<table border="1" cellspacing="4">
<tr>
<td style="text-align: center; vertical-align: top; " colspan="2"> reptiles </td>
</tr>
<tr>
<td style="text-align: center; vertical-align: top; ">
 iguana </td>
<td style="text-align: center; vertical-align: top; ">
 brontosaurus </td>
</tr>
</table>
</td>
<td style="text-align: center; vertical-align: top; ">
<table border="1" cellspacing="4">
<tr>
<td style="text-align: center; vertical-align: top; " colspan="4"> mammals </td>
</tr>
<tr>
<td style="text-align: center; vertical-align: top; ">
 sloth </td>
<td style="text-align: center; vertical-align: top; ">
 horse </td>
<td style="text-align: center; vertical-align: top; ">
 bison </td>
<td style="text-align: center; vertical-align: top; ">
<table border="1" cellspacing="4">
<tr>
<td style="text-align: center; vertical-align: top; " colspan="2"> primates </td>
</tr>
<tr>
<td style="text-align: center; vertical-align: top; ">
 gorilla </td>
<td style="text-align: center; vertical-align: top; ">
 lemur </td>
</tr>
</table>
</td>
</tr>
</table>
</td>
</tr>
</table>


## AppleScript


Using UTF8 box-drawing characters in a monospaced font, with options for (1.) compacted vs vertically centered display, and (2.) retaining or pruning out nodeless lines of text.

```AppleScript
-- Vertically centered textual tree using UTF8 monospaced
-- box-drawing characters, with options for compacting
-- and pruning.

--               ┌── Gamma
--       ┌─ Beta ┼── Delta
--       │       └ Epsilon
-- Alpha ┼─ Zeta ───── Eta
--       │       ┌─── Iota
--       └ Theta ┼── Kappa
--               └─ Lambda

-- TESTS --------------------------------------------------
on run
    set tree to Node(1, ¬
        {Node(2, ¬
            {Node(4, {Node(7, {})}), ¬
                Node(5, {})}), ¬
            Node(3, ¬
                {Node(6, ¬
                    {Node(8, {}), Node(9, {})})})})

    set tree2 to Node("Alpha", ¬
        {Node("Beta", ¬
            {Node("Gamma", {}), ¬
                Node("Delta", {}), ¬
                Node("Epsilon", {})}), ¬
            Node("Zeta", {Node("Eta", {})}), ¬
            Node("Theta", ¬
                {Node("Iota", {}), Node("Kappa", {}), ¬
                    Node("Lambda", {})})})

    set strTrees to unlines({"(NB – view in mono-spaced font)\n\n", ¬
        "Compacted (not all parents vertically centered):\n", ¬
        drawTree2(true, false, tree), ¬
        "\nFully expanded and vertically centered:\n", ¬
        drawTree2(false, false, tree2), ¬
        "\nVertically centered, with nodeless lines pruned out:\n", ¬
        drawTree2(false, true, tree2)})
    set the clipboard to strTrees
    strTrees
end run


-- drawTree2 :: Bool -> Bool -> Tree String -> String
on drawTree2(blnCompressed, blnPruned, tree)
    -- Tree design and algorithm inspired by the Haskell snippet at:
    -- https://doisinkidney.com/snippets/drawing-trees.html
    script measured
        on |λ|(t)
            script go
                on |λ|(x)
                    set s to " " & x & " "
                    Tuple(length of s, s)
                end |λ|
            end script
            fmapTree(go, t)
        end |λ|
    end script
    set measuredTree to |λ|(tree) of measured

    script levelMax
        on |λ|(a, level)
            a & maximum(map(my fst, level))
        end |λ|
    end script
    set levelWidths to foldl(levelMax, {}, ¬
        init(levels(measuredTree)))

    -- Lefts, Mid, Rights
    script lmrFromStrings
        on |λ|(xs)
            set {ls, rs} to items 2 thru -2 of ¬
                (splitAt((length of xs) div 2, xs) as list)
            Tuple3(ls, item 1 of rs, rest of rs)
        end |λ|
    end script

    script stringsFromLMR
        on |λ|(lmr)
            script add
                on |λ|(a, x)
                    a & x
                end |λ|
            end script
            foldl(add, {}, items 2 thru -2 of (lmr as list))
        end |λ|
    end script

    script fghOverLMR
        on |λ|(f, g, h)
            script
                property mg : mReturn(g)
                on |λ|(lmr)
                    set {ls, m, rs} to items 2 thru -2 of (lmr as list)
                    Tuple3(map(f, ls), |λ|(m) of mg, map(h, rs))
                end |λ|
            end script
        end |λ|
    end script

    script lmrBuild
        on leftPad(n)
            script
                on |λ|(s)
                    replicateString(n, space) & s
                end |λ|
            end script
        end leftPad

        -- lmrBuild main
        on |λ|(w, f)
            script
                property mf : mReturn(f)
                on |λ|(wsTree)
                    set xs to nest of wsTree
                    set lng to length of xs
                    set {nChars, x} to items 2 thru -2 of ¬
                        ((root of wsTree) as list)
                    set _x to replicateString(w - nChars, "─") & x

                    -- LEAF NODE ------------------------------------
                    if 0 = lng then
                        Tuple3({}, _x, {})

                    else if 1 = lng then
                        -- NODE WITH SINGLE CHILD ---------------------
                        set indented to leftPad(1 + w)
                        script lineLinked
                            on |λ|(z)
                                _x & "─" & z
                            end |λ|
                        end script
                        |λ|(|λ|(item 1 of xs) of mf) of ¬
                            (|λ|(indented, lineLinked, indented) of ¬
                                fghOverLMR)
                    else
                        -- NODE WITH CHILDREN -------------------------
                        script treeFix
                            on cFix(x)
                                script
                                    on |λ|(xs)
                                        x & xs
                                    end |λ|
                                end script
                            end cFix

                            on |λ|(l, m, r)
                                compose(stringsFromLMR, ¬
                                    |λ|(cFix(l), cFix(m), cFix(r)) of ¬
                                    fghOverLMR)
                            end |λ|
                        end script

                        script linked
                            on |λ|(s)
                                set c to text 1 of s
                                set t to tail(s)
                                if "┌" = c then
                                    _x & "┬" & t
                                else if "│" = c then
                                    _x & "┤" & t
                                else if "├" = c then
                                    _x & "┼" & t
                                else
                                    _x & "┴" & t
                                end if
                            end |λ|
                        end script

                        set indented to leftPad(w)
                        set lmrs to map(f, xs)
                        if blnCompressed then
                            set sep to {}
                        else
                            set sep to {"│"}
                        end if

                        tell lmrFromStrings
                            set tupleLMR to |λ|(intercalate(sep, ¬
                                {|λ|(item 1 of lmrs) of ¬
                                    (|λ|(" ", "┌", "│") of treeFix)} & ¬
                                map(|λ|("│", "├", "│") of treeFix, ¬
                                    init(tail(lmrs))) & ¬
                                {|λ|(item -1 of lmrs) of ¬
                                    (|λ|("│", "└", " ") of treeFix)}))
                        end tell

                        |λ|(tupleLMR) of ¬
                            (|λ|(indented, linked, indented) of fghOverLMR)
                    end if
                end |λ|
            end script
        end |λ|
    end script

    set treeLines to |λ|(|λ|(measuredTree) of ¬
        foldr(lmrBuild, 0, levelWidths)) of stringsFromLMR
    if blnPruned then
        script notEmpty
            on |λ|(s)
                script isData
                    on |λ|(c)
                        "│ " does not contain c
                    end |λ|
                end script
                any(isData, characters of s)
            end |λ|
        end script
        set xs to filter(notEmpty, treeLines)
    else
        set xs to treeLines
    end if
    unlines(xs)
end drawTree2


-- GENERIC ------------------------------------------------

-- Node :: a -> [Tree a] -> Tree a
on Node(v, xs)
    {type:"Node", root:v, nest:xs}
end Node

-- Tuple (,) :: a -> b -> (a, b)
on Tuple(a, b)
    -- Constructor for a pair of values, possibly of two different types.
    {type:"Tuple", |1|:a, |2|:b, length:2}
end Tuple

-- Tuple3 (,,) :: a -> b -> c -> (a, b, c)
on Tuple3(x, y, z)
    {type:"Tuple3", |1|:x, |2|:y, |3|:z, length:3}
end Tuple3

-- Applied to a predicate and a list,
-- |any| returns true if at least one element of the
-- list satisfies the predicate.
-- any :: (a -> Bool) -> [a] -> Bool
on any(f, xs)
    tell mReturn(f)
        set lng to length of xs
        repeat with i from 1 to lng
            if |λ|(item i of xs) then return true
        end repeat
        false
    end tell
end any

-- compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
on compose(f, g)
    script
        property mf : mReturn(f)
        property mg : mReturn(g)
        on |λ|(x)
            |λ|(|λ|(x) of mg) of mf
        end |λ|
    end script
end compose

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
    set acc to {}
    tell mReturn(f)
        repeat with i from 1 to lng
            set acc to acc & (|λ|(item i of xs, i, xs))
        end repeat
    end tell
    return acc
end concatMap

-- filter :: (a -> Bool) -> [a] -> [a]
on filter(f, xs)
    tell mReturn(f)
        set lst to {}
        set lng to length of xs
        repeat with i from 1 to lng
            set v to item i of xs
            if |λ|(v, i, xs) then set end of lst to v
        end repeat
        return lst
    end tell
end filter

-- fmapTree :: (a -> b) -> Tree a -> Tree b
on fmapTree(f, tree)
    script go
        property g : |λ| of mReturn(f)
        on |λ|(x)
            set xs to nest of x
            if xs ≠ {} then
                set ys to map(go, xs)
            else
                set ys to xs
            end if
            Node(g(root of x), ys)
        end |λ|
    end script
    |λ|(tree) of go
end fmapTree

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

-- foldr :: (a -> b -> b) -> b -> [a] -> b
on foldr(f, startValue, xs)
    tell mReturn(f)
        set v to startValue
        set lng to length of xs
        repeat with i from lng to 1 by -1
            set v to |λ|(item i of xs, v, i, xs)
        end repeat
        return v
    end tell
end foldr

-- fst :: (a, b) -> a
on fst(tpl)
    if class of tpl is record then
        |1| of tpl
    else
        item 1 of tpl
    end if
end fst

-- identity :: a -> a
on identity(x)
    -- The argument unchanged.
    x
end identity

-- init :: [a] -> [a]
-- init :: [String] -> [String]
on init(xs)
    set blnString to class of xs = string
    set lng to length of xs

    if lng > 1 then
        if blnString then
            text 1 thru -2 of xs
        else
            items 1 thru -2 of xs
        end if
    else if lng > 0 then
        if blnString then
            ""
        else
            {}
        end if
    else
        missing value
    end if
end init

-- intercalate :: [a] -> [[a]] -> [a]
-- intercalate :: String -> [String] -> String
on intercalate(sep, xs)
    concat(intersperse(sep, xs))
end intercalate

-- intersperse(0, [1,2,3]) -> [1, 0, 2, 0, 3]
-- intersperse :: a -> [a] -> [a]
-- intersperse :: Char -> String -> String
on intersperse(sep, xs)
    set lng to length of xs
    if lng > 1 then
        set acc to {item 1 of xs}
        repeat with i from 2 to lng
            set acc to acc & {sep, item i of xs}
        end repeat
        if class of xs is string then
            concat(acc)
        else
            acc
        end if
    else
        xs
    end if
end intersperse

-- isNull :: [a] -> Bool
-- isNull :: String -> Bool
on isNull(xs)
    if class of xs is string then
        "" = xs
    else
        {} = xs
    end if
end isNull

-- iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
on iterateUntil(p, f, x)
    script
        property mp : mReturn(p)'s |λ|
        property mf : mReturn(f)'s |λ|
        property lst : {x}
        on |λ|(v)
            repeat until mp(v)
                set v to mf(v)
                set end of lst to v
            end repeat
            return lst
        end |λ|
    end script
    |λ|(x) of result
end iterateUntil

-- levels :: Tree a -> [[a]]
on levels(tree)
    script nextLayer
        on |λ|(xs)
            script
                on |λ|(x)
                    nest of x
                end |λ|
            end script
            concatMap(result, xs)
        end |λ|
    end script

    script roots
        on |λ|(xs)
            script
                on |λ|(x)
                    root of x
                end |λ|
            end script
            map(result, xs)
        end |λ|
    end script

    map(roots, iterateUntil(my isNull, nextLayer, {tree}))
end levels

-- map :: (a -> b) -> [a] -> [b]
on map(f, xs)
    -- The list obtained by applying f
    -- to each element of xs.
    tell mReturn(f)
        set lng to length of xs
        set lst to {}
        repeat with i from 1 to lng
            set end of lst to |λ|(item i of xs, i, xs)
        end repeat
        return lst
    end tell
end map

-- maximum :: Ord a => [a] -> a
on maximum(xs)
    script
        on |λ|(a, b)
            if a is missing value or b > a then
                b
            else
                a
            end if
        end |λ|
    end script
    foldl(result, missing value, xs)
end maximum

-- mReturn :: First-class m => (a -> b) -> m (a -> b)
on mReturn(f)
    -- 2nd class handler function lifted into 1st class script wrapper.
    if script is class of f then
        f
    else
        script
            property |λ| : f
        end script
    end if
end mReturn

-- replicateString :: Int -> String -> String
on replicateString(n, s)
    set out to ""
    if n < 1 then return out
    set dbl to s

    repeat while (n > 1)
        if (n mod 2) > 0 then set out to out & dbl
        set n to (n div 2)
        set dbl to (dbl & dbl)
    end repeat
    return out & dbl
end replicateString

-- snd :: (a, b) -> b
on snd(tpl)
    if class of tpl is record then
        |2| of tpl
    else
        item 2 of tpl
    end if
end snd

-- splitAt :: Int -> [a] -> ([a], [a])
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

-- tail :: [a] -> [a]
on tail(xs)
    set blnText to text is class of xs
    if blnText then
        set unit to ""
    else
        set unit to {}
    end if
    set lng to length of xs
    if 1 > lng then
        missing value
    else if 2 > lng then
        unit
    else
        if blnText then
            text 2 thru -1 of xs
        else
            rest of xs
        end if
    end if
end tail

-- unlines :: [String] -> String
on unlines(xs)
    -- A single string formed by the intercalation
    -- of a list of strings with the newline character.
    set {dlm, my text item delimiters} to ¬
        {my text item delimiters, linefeed}
    set str to xs as text
    set my text item delimiters to dlm
    str
end unlines
```

```txt
(NB – view in mono-spaced font)


Compacted (not all parents vertically centered):

       ┌ 4 ─ 7
   ┌ 2 ┴ 5
 1 ┤       ┌ 8
   └ 3 ─ 6 ┴ 9

Fully expanded and vertically centered:

               ┌── Gamma
               │
       ┌─ Beta ┼── Delta
       │       │
       │       └ Epsilon
       │
 Alpha ┼─ Zeta ───── Eta
       │
       │       ┌─── Iota
       │       │
       └ Theta ┼── Kappa
               │
               └─ Lambda

Vertically centered, with nodeless lines pruned out:

               ┌── Gamma
       ┌─ Beta ┼── Delta
       │       └ Epsilon
 Alpha ┼─ Zeta ───── Eta
       │       ┌─── Iota
       └ Theta ┼── Kappa
               └─ Lambda
```



## Batch File

Displays a tree of the current directory.

```batch file
@tree %cd%
```




## BBC BASIC

This creates a native Windows Tree View control:

```bbcbasic
      INSTALL @lib$+"WINLIB5"
      ON ERROR SYS "MessageBox", @hwnd%, REPORT$, 0, 0 : QUIT

      REM!WC Windows constants:
      TVI_SORT = -65533
      TVIF_TEXT = 1
      TVM_INSERTITEM = 4352
      TVS_HASBUTTONS = 1
      TVS_HASLINES = 2
      TVS_LINESATROOT = 4

      REM. TV_INSERTSTRUCT
      DIM tvi{hParent%,       \
      \       hInsertAfter%,  \
      \       mask%,          \
      \       hItem%,         \
      \       state%,         \
      \       stateMask%,     \
      \       pszText%,       \
      \       cchTextMax%,    \
      \       iImage%,        \
      \       iSelectedImage%,\
      \       cChildren%,     \
      \       lParam%         \
      \      }

      SYS "InitCommonControls"
      hTree% = FN_createwindow("SysTreeView32", "", 0, 0, @vdu.tr%, @vdu.tb%, 0, \
      \                        TVS_HASLINES OR TVS_HASBUTTONS OR TVS_LINESATROOT, 0)
      hroot% = FNinsertnode(0, "Root")
      hchild1% = FNinsertnode(hroot%, "Child 1")
      hchild2% = FNinsertnode(hroot%, "Child 2")
      hchild11% = FNinsertnode(hchild1%, "Grandchild 1")
      hchild12% = FNinsertnode(hchild1%, "Grandchild 2")
      hchild21% = FNinsertnode(hchild2%, "Grandchild 3")
      hchild22% = FNinsertnode(hchild2%, "Grandchild 4")

      REPEAT
        WAIT 1
      UNTIL FALSE
      END

      DEF FNinsertnode(hparent%, text$)
      LOCAL hnode%
      text$ += CHR$0

      tvi.hParent% = hparent%
      tvi.hInsertAfter% = TVI_SORT
      tvi.mask% = TVIF_TEXT
      tvi.pszText% = !^text$

      SYS "SendMessage", hTree%, TVM_INSERTITEM, 0, tvi{} TO hnode%
      IF hnode% = 0 ERROR 100, "TVM_INSERTITEM failed"
      SYS "InvalidateRect", hTree%, 0, 0
      = hnode%
```

[[File:visualize_tree_bbc.gif]]


## C

Print a simple tree to standard output:

```c
#include <stdio.h>
#include <stdlib.h>

typedef struct stem_t *stem;
struct stem_t { const char *str; stem next; };

void tree(int root, stem head)
{
	static const char *sdown = "  |", *slast = "  `", *snone = "   ";
	struct stem_t col = {0, 0}, *tail;

	for (tail = head; tail; tail = tail->next) {
		printf("%s", tail->str);
		if (!tail->next) break;
	}

	printf("--%d\n", root);

	if (root <= 1) return;

	if (tail && tail->str == slast)
		tail->str = snone;

	if (!tail)	tail = head = &col;
	else		tail->next = &col;

	while (root) { // make a tree by doing something random
		int r = 1 + (rand() % root);
		root -= r;
		col.str = root ? sdown : slast;

		tree(r, head);
	}

	tail->next = 0;
}

int main(int c, char**v)
{
	int n;
	if (c < 2 || (n = atoi(v[1])) < 0) n = 8;

	tree(n, 0);
	return 0;
}
```

```txt

--8
  `--8
     |--7
     |  |--3
     |  |  |--2
     |  |  |  `--2
     |  |  |     `--2
     |  |  |        |--1
     |  |  |        `--1
     |  |  `--1
     |  |--2
     |  |  |--1
     |  |  `--1
     |  |--1
     |  `--1
     `--1

```



## Clojure


```clojure
(use 'vijual)

(draw-tree [[:A] [:B] [:C [:D [:E] [:F]] [:G]]])

```

```txt

+---+ +---+ +---+
| A | | B | | C |
+---+ +---+ +-+-+
              |
        +-----+
        |     |
      +-+-+ +-+-+
      | D | | G |
      +-+-+ +---+
        |
     +--+--+
     |     |
   +-+-+ +-+-+
   | E | | F |
   +---+ +---+

```



## Common Lisp


```lisp
(defun visualize (tree)
  (labels
      ((rprint (list)
         (mapc #'princ (reverse list)))
       (vis-h (tree branches)
         (let ((len (length tree)))
           (loop
              for item in tree
              for idx from 1 to len do
                (cond
                  ((listp item)
                   (rprint (cdr branches))
                   (princ "+---+")
                   (let ((next (cons "|   "
                                     (if (= idx len)
                                         (cons "    " (cdr branches))
                                         branches))))
                     (terpri)
                     (rprint (if (null item)
                                 (cdr next)
                                 next))
                     (terpri)
                     (vis-h item next)))
                  (t
                   (rprint (cdr branches))
                   (princ item)
                   (terpri)
                   (rprint (if (= idx len)
                               (cdr branches)
                               branches))
                   (terpri)))))))
    (vis-h tree '("|   "))))
```

```lisp
CL-USER> (visualize '(a b c ((d (e ((() ()))) f)) (g)))
A
|
B
|
C
|
+---+
|   |
|   +---+
|       |
|       D
|       |
|       +---+
|       |   |
|       |   E
|       |   |
|       |   +---+
|       |       |
|       |       +---+
|       |           |
|       |           +---+
|       |           |
|       |           +---+
|       |
|       F
|
+---+
    |
    G

NIL
```



## D

```d
import std.stdio, std.conv, std.algorithm, std.array;

struct Node(T) { T value; Node* left, right; }

string[] treeIndent(T)(in Node!T* t) pure nothrow @safe {
    if (!t) return ["-- (null)"];
    const tr = t.right.treeIndent;
    return "--" ~ t.value.text ~
           t.left.treeIndent.map!q{"  |" ~ a}.array ~
           ("  `" ~ tr[0]) ~ tr[1 .. $].map!q{"   " ~ a}.array;
}

void main () {
    static N(T)(T v, Node!T* l=null, Node!T* r=null) {
        return new Node!T(v, l, r);
    }

    const tree = N(1, N(2, N(4, N(7)), N(5)), N(3, N(6, N(8), N(9))));
    writefln("%-(%s\n%)", tree.treeIndent);
}
```

```txt
--1
  |--2
  |  |--4
  |  |  |--7
  |  |  |  |-- (null)
  |  |  |  `-- (null)
  |  |  `-- (null)
  |  `--5
  |     |-- (null)
  |     `-- (null)
  `--3
     |--6
     |  |--8
     |  |  |-- (null)
     |  |  `-- (null)
     |  `--9
     |     |-- (null)
     |     `-- (null)
     `-- (null)
```


## Elena

ELENA 4.1 :

```elena
/// a program to produce a visual representation of some tree.

import system'routines;
import extensions;

class Node
{
    string theValue;
    Node[] theChildren;

    constructor new(string value, Node[] children)
    {
        theValue := value;

        theChildren := children;
    }

    constructor new(string value)
        <= new(value, new Node[](0));

    constructor new(Node[] children)
        <= new(emptyString, children);

    get() = theValue;

    Children = theChildren;
}

extension treeOp
{
    writeTree(node, prefix)
    {
        var children := node.Children;
        var length := children.Length;

        children.zipForEach(new Range(1, length), (child,index)
        {
            self.printLine(prefix,"|");
            self.printLine(prefix,"+---",child.get());

            var nodeLine := prefix + (index==length).iif("    ","|   ");

            self.writeTree(child,nodeLine);
        });

        ^ self
    }

    writeTree(node)
        = self.writeTree(node,"");
}

public program()
{
    var tree := Node.new(
                 new Node[]::(
                    Node.new("a", new Node[]::
                    (
                        Node.new("b", new Node[]::(Node.new("c"))),
                        Node.new("d")
                    )),
                    Node.new("e")
                 ));

    console.writeTree(tree).readChar()
}
```

```txt

|
+---a
|   |
|   +---b
|   |   |
|   |   +---c
|   |
|   +---d
|
+---b

```



## Erlang

Until real code shows up, I follow the lead of Python and print tuples with a width of 1.
```txt

9> io:fwrite("~1p", [{1, 2, {30, 40}, {{500, 600}, 70}}]).
{1,
 2,
 {30,
  40},
 {{500,
   600},
  70}}

```


=={{header|F_Sharp|F#}}==

```fsharp
type tree =
    | T of string * tree list

let prefMid = seq { yield "├─"; while true do yield "│ " }
let prefEnd = seq { yield "└─"; while true do yield "  " }
let prefNone = seq { while true do yield "" }

let c2 x y = Seq.map2 (fun u v -> String.concat "" [u; v]) x y

let rec visualize (T(label, children)) pre =
    seq {
        yield (Seq.head pre) + label
        if children <> [] then
            let preRest = Seq.skip 1 pre
            let last = Seq.last (List.toSeq children)
            for e in children do
                if e = last then yield! visualize e (c2 preRest prefEnd)
                else yield! visualize e (c2 preRest prefMid)
    }

let example =
    T ("root",
            [T ("a",
                    [T ("a1",
                            [T ("a11", []);
                            T ("a12", []) ]) ]);
            T ("b",
                    [T ("b1", []) ]) ])

visualize example prefNone
|> Seq.iter (printfn "%s")
```

```txt
root
├─a
│ └─a1
│   ├─a11
│   └─a12
└─b
  └─b1
```



## Factor

Factor's prettyprinter does this by default with any nested sequences and/or tuples. There are dynamic variables that can be altered to change the prettyprinter's default behavior. The most interesting are <code>tab-size</code> and <code>margin</code> for customizing the look of a tree. For smaller trees, it's best to change <code>margin</code> from its default of <code>64</code> to something low, perhaps <code>10</code>.

```factor
USE: literals

CONSTANT: mammals { "mammals" { "deer" "gorilla" "dolphin" } }
CONSTANT: reptiles { "reptiles" { "turtle" "lizard" "snake" } }

{ "animals" ${ mammals reptiles } } dup . 10 margin set .
```

```txt

{
    "animals"
    {
        { "mammals" { "deer" "gorilla" "dolphin" } }
        { "reptiles" { "turtle" "lizard" "snake" } }
    }
}
{
    "animals"
    {
        {
            "mammals"
            {
                "deer"
                "gorilla"
                "dolphin"
            }
        }
        {
            "reptiles"
            {
                "turtle"
                "lizard"
                "snake"
            }
        }
    }
}

```

An example showcasing tuples by displaying an AVL tree:

```factor
USE: trees.avl
AVL{ { 1 2 } { 9 19 } { 3 4 } { 5 6 } } .
```

```txt

T{ avl
    { root
        T{ avl-node
            { key 3 }
            { value 4 }
            { left
                T{ avl-node
                    { key 1 }
                    { value 2 }
                    { balance 0 }
                }
            }
            { right
                T{ avl-node
                    { key 9 }
                    { value 19 }
                    { left
                        T{ avl-node
                            { key 5 }
                            { value 6 }
                            { balance 0 }
                        }
                    }
                    { balance -1 }
                }
            }
            { balance 1 }
        }
    }
    { count 4 }
}

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Visualize_a_tree this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go


### JSON

Not the most economical output, but at least json.MarshalIndent is in the Go standard library.  Note that the definition of Node has nothing JSON specific about it; it's an ordinary struct.

```Go
package main

import (
    "encoding/json"
    "fmt"
    "log"
)

type Node struct {
    Name     string
    Children []*Node
}

func main() {
    tree := &Node{"root", []*Node{
        &Node{"a", []*Node{
            &Node{"d", nil},
            &Node{"e", []*Node{
                &Node{"f", nil},
            }}}},
        &Node{"b", nil},
        &Node{"c", nil},
    }}
    b, err := json.MarshalIndent(tree, "", "   ")
    if err != nil {
        log.Fatal(err)
    }
    fmt.Println(string(b))
}
```

```txt

{
   "Name": "root",
   "Children": [
      {
         "Name": "a",
         "Children": [
            {
               "Name": "d",
               "Children": null
            },
            {
               "Name": "e",
               "Children": [
                  {
                     "Name": "f",
                     "Children": null
                  }
               ]
            }
         ]
      },
      {
         "Name": "b",
         "Children": null
      },
      {
         "Name": "c",
         "Children": null
      }
   ]
}

```


### TOML

It works in this case, but TOML wasn't really designed for this and encoders may have trouble with general trees.  Empty trees and nils for example might be problematic depending on your data structures and limitations of your TOML encoder.  YMMV.

```go
package main

import (
    "log"
    "os"

    "github.com/BurntSushi/toml"
)

type Node struct {
    Name     string
    Children []*Node
}

func main() {
    tree := &Node{"root", []*Node{
        &Node{"a", []*Node{
            &Node{"d", nil},
            &Node{"e", []*Node{
                &Node{"f", nil},
            }}}},
        &Node{"b", nil},
        &Node{"c", nil},
    }}
    enc := toml.NewEncoder(os.Stdout)
    enc.Indent = "   "
    err := enc.Encode(tree)
    if err != nil {
        log.Fatal(err)
    }
}
```

```txt

Name = "root"

[[Children]]
   Name = "a"

   [[Children.Children]]
      Name = "d"


   [[Children.Children]]
      Name = "e"

      [[Children.Children.Children]]
         Name = "f"


[[Children]]
   Name = "b"


[[Children]]
   Name = "c"

```


### Unicode

A non-library solution, more like a number of other solutions on this page, and with more compact output.  The tree representation here uses integer indexes rather than pointers, which is efficient for representation and computation.  A serialization format like JSON or TOML wouldn't see it as a hierarchical structure, but the code here knows to interpret the child ints as node indexes.

```go
package main

import "fmt"

type tree []node

type node struct {
    label    string
    children []int // indexes into tree
}

func main() {
    vis(tree{
        0: node{"root", []int{1, 2, 3}},
        1: node{"ei", []int{4, 5}},
        2: node{"bee", nil},
        3: node{"si", nil},
        4: node{"dee", nil},
        5: node{"y", []int{6}},
        6: node{"eff", nil},
    })
}

func vis(t tree) {
    if len(t) == 0 {
        fmt.Println("<empty>")
        return
    }
    var f func(int, string)
    f = func(n int, pre string) {
        ch := t[n].children
        if len(ch) == 0 {
            fmt.Println("╴", t[n].label)
            return
        }
        fmt.Println("┐", t[n].label)
        last := len(ch) - 1
        for _, ch := range ch[:last] {
            fmt.Print(pre, "├─")
            f(ch, pre+"│ ")
        }
        fmt.Print(pre, "└─")
        f(ch[last], pre+"  ")
    }
    f(0, "")
}
```

```txt

┐ root
├─┐ ei
│ ├─╴ dee
│ └─┐ y
│   └─╴ eff
├─╴ bee
└─╴ si

```



## Haskell

Tree borrowed from [[Tree traversal]]:

```haskell
data Tree a = Empty | Node { value :: a, left :: Tree a, right :: Tree a }
	deriving (Show, Eq)

tree = Node 1 (Node 2 (Node 4 (Node 7 Empty Empty) Empty)
	(Node 5 Empty Empty)) (Node 3 (Node 6 (Node 8 Empty Empty)
	(Node 9 Empty Empty)) Empty)

treeIndent Empty = ["-- (nil)"]
treeIndent t = ["--" ++ show (value t)]
	++ map ("  |"++) ls ++ ("  `" ++ r):map ("   "++) rs
	where
	(r:rs) = treeIndent$right t
	ls     = treeIndent$left t

main = mapM_ putStrLn $ treeIndent tree
```

```txt

--1
  |--2
  |  |--4
  |  |  |--7
  |  |  |  |-- (nil)
  |  |  |  `-- (nil)
  |  |  `-- (nil)
  |  `--5
  |     |-- (nil)
  |     `-- (nil)
  `--3
     |--6
     |  |--8
     |  |  |-- (nil)
     |  |  `-- (nil)
     |  `--9
     |     |-- (nil)
     |     `-- (nil)
     `-- (nil)

```



The '''Data.Tree''' module in the standard (GHC) libraries also includes a '''drawTree''' function for multiway (rose) trees of strings.
We can ''fmap show'' over our tree of integers to derive a tree of strings, and apply `drawTree` to that.


```haskell
import Data.Tree (Tree(..), drawTree)

tree :: Tree Int
tree =
  Node
    1
    [ Node 2 [Node 4 [Node 7 []], Node 5 []]
    , Node 3 [Node 6 [Node 8 [], Node 9 []]]
    ]

main :: IO ()
main = (putStrLn . drawTree . fmap show) tree
```

```txt
1
|
+- 2
|  |
|  +- 4
|  |  |
|  |  `- 7
|  |
|  `- 5
|
`- 3
   |
   `- 6
      |
      +- 8
      |
      `- 9
```


=={{header|Icon}} and {{header|Unicon}}==

The following works in both languages.

```unicon
procedure main(A)
    showTree("", " -", [1, [2,[3],[4,[5],[6]],[7,[11]]], [8,[9,[10]]] ])
    write()
    showTree("", " -", [1, [2,[3,[4]]], [5,[6],[7,[8],[9]],[10]] ])
end

procedure showTree(prefix, lastc, A)
    write(prefix, lastc, "--", A[1])
    if *A > 1 then {
        prefix ||:= if prefix[-1] == "|" then "   " else "    "
        every showTree(prefix||"|", "-", !A[2:2 < *A])
        showTree(prefix, "`-", A[*A])
        }
end
```


Output:

```txt

->tree
 ---1
    |---2
    |   |---3
    |   |---4
    |   |   |---5
    |   |   `---6
    |   `---7
    |       `---11
    `---8
        `---9
            `---10

 ---1
    |---2
    |   `---3
    |       `---4
    `---5
        |---6
        |---7
        |   |---8
        |   `---9
        `---10
->

```



## J


See: [[j:Essays/Tree Display]] for tree represented as label pairs.

Or, adapted to the [[Tree_traversal#J:_Alternate_implementation|parent index]] representation of a tree (which allows different nodes to share labels and may also be more convenient for other reasons):


```J
BOXC=: 9!:6 ''    NB. box drawing characters
EW  =: {: BOXC    NB. east-west

showtree=: 4 : 0
 NB. y is parent index for each node (non-indices for root nodes)
 NB. x is label for each node
 t=. (<EW,' ') ,@<@,:@,&":&.> x        NB. tree fragments
 c=. |:(#~ e./@|:);(~.,"0&.>(</. i.@#)) y
 while. +./ b=. ({.c)*.//.-.e.~/c do.
  i=. b#~.{.c                          NB. parents whose children are leaves
  j=. </./(({.c)e.i)#"1 c              NB. leaves grouped by parents
  t=. a: (;j)}t i}~ (i{t) subtree&.> j{&.><t
  c=. (-.({.c)e.i)#"1 c                NB. prune edges to leaves
 end.
 ;([: ,.&.>/ extend&.>)&> t -. a:
)

subtree=: 4 : 0
 p=. EW={."1 s=. >{.t=. graft y
 (<(>{.x) root p),(<(connect p),.s),}.t
)

graft=: 3 : 0
 n=. (-~ >./) #&> y
 f=. i.@(,&0)@#&.>@{.&.> y
 ,&.>/ y ,&> n$&.>f
)

connect=: 3 : 0
 b=. (+./\ *. +./\.) y
 c=. (b+2*y){' ',9 3 3{BOXC  NB. │ NS ├ E
 c=. (0{BOXC) (b i. 1)}c     NB. ┌ NW
 c=. (6{BOXC) (b i: 1)}c     NB. └ SW
 j=. (b i. 1)+<.-:+/b
 EW&(j})^:(1=+/b) c j}~ ((0 3 6 9{BOXC)i.j{c){1 4 7 5{BOXC
)

root=: 4 : 0
 j=. k+<.-:1+(y i: 1)-k=. y i. 1
 (-j)|.(#y){.x,.,:' ',EW
)

extend=: 3 : '(+./\"1 (y=EW) *. *./\."1 y e.'' '',EW)}y,:EW'

```


Example use:


```j
   (i.10) showtree _,}.p:inv i.10
                 ┌─ 6
     ┌─ 1 ─── 3 ─┴─ 7
     │           ┌─ 8
─ 0 ─┤     ┌─ 4 ─┴─ 9
     └─ 2 ─┴─ 5
```



## Java

Minimalist BST that can do nothing except print itself to stdout.

```java
public class VisualizeTree {
    public static void main(String[] args) {
        BinarySearchTree tree = new BinarySearchTree();

        tree.insert(100);
        for (int i = 0; i < 20; i++)
            tree.insert((int) (Math.random() * 200));
        tree.display();
    }
}

class BinarySearchTree {
    private Node root;

    private class Node {
        private int key;
        private Node left, right;

        Node(int k) {
            key = k;
        }
    }

    public boolean insert(int key) {
        if (root == null)
            root = new Node(key);
        else {
            Node n = root;
            Node parent;
            while (true) {
                if (n.key == key)
                    return false;

                parent = n;

                boolean goLeft = key < n.key;
                n = goLeft ? n.left : n.right;

                if (n == null) {
                    if (goLeft) {
                        parent.left = new Node(key);
                    } else {
                        parent.right = new Node(key);
                    }
                    break;
                }
            }
        }
        return true;
    }

    public void display() {
        final int height = 5, width = 64;

        int len = width * height * 2 + 2;
        StringBuilder sb = new StringBuilder(len);
        for (int i = 1; i <= len; i++)
            sb.append(i < len - 2 && i % width == 0 ? "\n" : ' ');

        displayR(sb, width / 2, 1, width / 4, width, root, " ");
        System.out.println(sb);
    }

    private void displayR(StringBuilder sb, int c, int r, int d, int w, Node n,
            String edge) {
        if (n != null) {
            displayR(sb, c - d, r + 2, d / 2, w, n.left, " /");

            String s = String.valueOf(n.key);
            int idx1 = r * w + c - (s.length() + 1) / 2;
            int idx2 = idx1 + s.length();
            int idx3 = idx1 - w;
            if (idx2 < sb.length())
                sb.replace(idx1, idx2, s).replace(idx3, idx3 + 2, edge);

            displayR(sb, c + d, r + 2, d / 2, w, n.right, "\\ ");
        }
    }
}
```



```txt
                             100
                /                              \
               49                              106
        /              \                /              \
       44              94              105             152
    /      \        /                               /      \
   26      47      61                              109     178
  /  \            /  \                               \    /
 12  33          51  88                              119 159
```



## JavaScript


### HTML

Javascript wrapped in HTML5 document. ''Should'' work in modern browsers.

```html
<!doctype html>
<html id="doc">
  <head><meta charset="utf-8"/>
    <title>Stuff</title>
    <script type="application/javascript">
	function gid(id) { return document.getElementById(id); }

	function ce(tag, cls, parent_node) {
		var e = document.createElement(tag);
		e.className = cls;
		if (parent_node) parent_node.appendChild(e);
		return e;
	}

	function dom_tree(id) {
		gid('tree').textContent = "";
		gid('tree').appendChild(mktree(gid(id), null));
	}

	function mktree(e, p) {
		var t = ce("div", "tree", p);
		var tog = ce("span", "toggle", t);
		var h = ce("span", "tag", t);

		if (e.tagName === undefined) {
			h.textContent = "#Text";
			var txt = e.textContent;
			if (txt.length > 0 && txt.match(/\S/)) {
				h = ce("div", "txt", t);
				h.textContent = txt;
			}
			return t;
		}

		tog.textContent = "−";
		tog.onclick = function () { clicked(tog); }
		h.textContent = e.nodeName;

		var l = e.childNodes;
		for (var i = 0; i != l.length; i++)
			mktree(l[i], t);
		return t;
	}

	function clicked(e) {
		var is_on = e.textContent == "−";
		e.textContent = is_on ? "+" : "−";
		e.parentNode.className = is_on ? "tree-hide" : "tree";
	}
    </script>
    <style>
      #tree { white-space: pre; font-family: monospace; border: 1px solid }
      .tree > .tree-hide, .tree > .tree
		{ margin-left: 2em; border-left: 1px dotted rgba(0,0,0,.2)}
      .tree-hide > .tree, .tree-hide > .tree-hide { display: none }
      .tag { color: navy }
      .tree-hide > .tag { color: maroon }
      .txt { color: gray; padding: 0 .5em; margin: 0 .5em 0 2em; border: 1px dotted rgba(0,0,0,.1) }
      .toggle { display: inline-block; width: 2em; text-align: center }
    </style>
  </head>
  <body>
    <article>
      <section>
        <h1>Headline</h1>
        Blah blah
      </section>
      <section>
        <h1>More headline</h1>
        <blockquote>Something something</blockquote>
        <section><h2>Nested section</h2>
	  Somethin somethin list:
	  <ul>
	    <li>Apples</li>
	    <li>Oranges</li>
	    <li>Cetera Fruits</li>
	  </ul>
	</section>
      </section>
    </article>
    <div id="tree"><a href="javascript:dom_tree('doc')">click me</a></div>
  </body>
</html>
```



### Plain text


### =Vertically centered tree=

{{Trans|Python}} (Functional version)

```JavaScript
(() => {
    'use strict';

    // UTF8 character-drawn tree, with options for compacting vs
    // centering parents, and for pruning out nodeless lines.

    const example = `
               ┌ Epsilon
       ┌─ Beta ┼─── Zeta
       │       └──── Eta
 Alpha ┼ Gamma ─── Theta
       │       ┌─── Iota
       └ Delta ┼── Kappa
               └─ Lambda`

    // drawTree2 :: Bool -> Bool -> Tree String -> String
    const drawTree2 = blnCompact => blnPruned => tree => {
        // Tree design and algorithm inspired by the Haskell snippet at:
        // https://doisinkidney.com/snippets/drawing-trees.html
        const
            // Lefts, Middle, Rights
            lmrFromStrings = xs => {
                const [ls, rs] = Array.from(splitAt(
                    Math.floor(xs.length / 2),
                    xs
                ));
                return Tuple3(ls, rs[0], rs.slice(1));
            },
            stringsFromLMR = lmr =>
            Array.from(lmr).reduce((a, x) => a.concat(x), []),
            fghOverLMR = (f, g, h) => lmr => {
                const [ls, m, rs] = Array.from(lmr);
                return Tuple3(ls.map(f), g(m), rs.map(h));
            };

        const lmrBuild = (f, w) => wsTree => {
            const
                leftPad = n => s => ' '.repeat(n) + s,
                xs = wsTree.nest,
                lng = xs.length,
                [nChars, x] = Array.from(wsTree.root);

            // LEAF NODE --------------------------------------
            return 0 === lng ? (
                Tuple3([], '─'.repeat(w - nChars) + x, [])

                // NODE WITH SINGLE CHILD -------------------------
            ) : 1 === lng ? (() => {
                const indented = leftPad(1 + w);
                return fghOverLMR(
                    indented,
                    z => '─'.repeat(w - nChars) + x + '─' + z,
                    indented
                )(f(xs[0]));

                // NODE WITH CHILDREN -----------------------------
            })() : (() => {
                const
                    cFix = x => xs => x + xs,
                    treeFix = (l, m, r) => compose(
                        stringsFromLMR,
                        fghOverLMR(cFix(l), cFix(m), cFix(r))
                    ),
                    _x = '─'.repeat(w - nChars) + x,
                    indented = leftPad(w),
                    lmrs = xs.map(f);
                return fghOverLMR(
                    indented,
                    s => _x + ({
                        '┌': '┬',
                        '├': '┼',
                        '│': '┤',
                        '└': '┴'
                    })[s[0]] + s.slice(1),
                    indented
                )(lmrFromStrings(
                    intercalate(
                        blnCompact ? [] : ['│'],
                        [treeFix(' ', '┌', '│')(lmrs[0])]
                        .concat(init(lmrs.slice(1)).map(
                            treeFix('│', '├', '│')
                        ))
                        .concat([treeFix('│', '└', ' ')(
                            lmrs[lmrs.length - 1]
                        )])
                    )
                ));
            })();
        };
        const
            measuredTree = fmapTree(
                v => {
                    const s = ' ' + v + ' ';
                    return Tuple(s.length, s)
                }, tree
            ),
            levelWidths = init(levels(measuredTree))
            .reduce(
                (a, level) => a.concat(maximum(level.map(fst))),
                []
            ),
            treeLines = stringsFromLMR(
                levelWidths.reduceRight(
                    lmrBuild, x => x
                )(measuredTree)
            );
        return unlines(
            blnPruned ? (
                treeLines.filter(
                    s => s.split('')
                    .some(c => !' │'.includes(c))
                )
            ) : treeLines
        );
    };

    // TESTS ----------------------------------------------
    const main = () => {

        // tree :: Tree String
        const tree = Node(
            'Alpha', [
                Node('Beta', [
                    Node('Epsilon', []),
                    Node('Zeta', []),
                    Node('Eta', [])
                ]),
                Node('Gamma', [Node('Theta', [])]),
                Node('Delta', [
                    Node('Iota', []),
                    Node('Kappa', []),
                    Node('Lambda', [])
                ])
            ]);

        // tree2 :: Tree Int
        const tree2 = Node(
            1,
            [
                Node(2, [
                    Node(4, []),
                    Node(5, [Node(7, [])])
                ]),
                Node(3, [
                    Node(6, [
                        Node(8, []),
                        Node(9, [])
                    ])
                ])
            ]
        );

        // strTrees :: String
        const strTrees = ([
            'Compacted (parents not all vertically centered):',
            drawTree2(true)(false)(tree2),
            'Fully expanded, with vertical centering:',
            drawTree2(false)(false)(tree),
            'Vertically centered, with nodeless lines pruned out:',
            drawTree2(false)(true)(tree),
        ].join('\n\n'));

        return (
            console.log(strTrees),
            strTrees
        );
    };

    // GENERIC FUNCTIONS ----------------------------------

    // Node :: a -> [Tree a] -> Tree a
    const Node = (v, xs) => ({
        type: 'Node',
        root: v, // any type of value (consistent across tree)
        nest: xs || []
    });

    // Tuple (,) :: a -> b -> (a, b)
    const Tuple = (a, b) => ({
        type: 'Tuple',
        '0': a,
        '1': b,
        length: 2
    });

    // Tuple3 (,,) :: a -> b -> c -> (a, b, c)
    const Tuple3 = (a, b, c) => ({
        type: 'Tuple3',
        '0': a,
        '1': b,
        '2': c,
        length: 3
    });

    // compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
    const compose = (f, g) => x => f(g(x));

    // concat :: [[a]] -> [a]
    // concat :: [String] -> String
    const concat = xs =>
        0 < xs.length ? (() => {
            const unit = 'string' !== typeof xs[0] ? (
                []
            ) : '';
            return unit.concat.apply(unit, xs);
        })() : [];

    // fmapTree :: (a -> b) -> Tree a -> Tree b
    const fmapTree = (f, tree) => {
        const go = node => Node(
            f(node.root),
            node.nest.map(go)
        );
        return go(tree);
    };

    // fst :: (a, b) -> a
    const fst = tpl => tpl[0];

    // identity :: a -> a
    const identity = x => x;

    // init :: [a] -> [a]
    const init = xs =>
        0 < xs.length ? (
            xs.slice(0, -1)
        ) : undefined;

    // intercalate :: [a] -> [[a]] -> [a]
    // intercalate :: String -> [String] -> String
    const intercalate = (sep, xs) =>
        0 < xs.length && 'string' === typeof sep &&
        'string' === typeof xs[0] ? (
            xs.join(sep)
        ) : concat(intersperse(sep, xs));

    // intersperse(0, [1,2,3]) -> [1, 0, 2, 0, 3]

    // intersperse :: a -> [a] -> [a]
    // intersperse :: Char -> String -> String
    const intersperse = (sep, xs) => {
        const bln = 'string' === typeof xs;
        return xs.length > 1 ? (
            (bln ? concat : x => x)(
                (bln ? (
                    xs.split('')
                ) : xs)
                .slice(1)
                .reduce((a, x) => a.concat([sep, x]), [xs[0]])
            )) : xs;
    };

    // iterateUntil :: (a -> Bool) -> (a -> a) -> a -> [a]
    const iterateUntil = (p, f, x) => {
        const vs = [x];
        let h = x;
        while (!p(h))(h = f(h), vs.push(h));
        return vs;
    };

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // levels :: Tree a -> [[a]]
    const levels = tree =>
        iterateUntil(
            xs => 1 > xs.length,
            ys => [].concat(...ys.map(nest)),
            [tree]
        ).map(xs => xs.map(root));

    // maximum :: Ord a => [a] -> a
    const maximum = xs =>
        0 < xs.length ? (
            xs.slice(1).reduce((a, x) => x > a ? x : a, xs[0])
        ) : undefined;

    // nest :: Tree a -> [a]
    const nest = tree => tree.nest;

    // root :: Tree a -> a
    const root = tree => tree.root;

    // splitAt :: Int -> [a] -> ([a], [a])
    const splitAt = (n, xs) =>
        Tuple(xs.slice(0, n), xs.slice(n));

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // MAIN ---
    return main();
})();
```

```txt
Compacted (parents not all vertically centered):

       ┌ 4
   ┌ 2 ┴ 5 ─ 7
 1 ┤       ┌ 8
   └ 3 ─ 6 ┴ 9

Fully expanded, with vertical centering:

               ┌ Epsilon
               │
       ┌─ Beta ┼─── Zeta
       │       │
       │       └──── Eta
       │
 Alpha ┼ Gamma ─── Theta
       │
       │       ┌─── Iota
       │       │
       └ Delta ┼── Kappa
               │
               └─ Lambda

Vertically centered, with nodeless lines pruned out:

               ┌ Epsilon
       ┌─ Beta ┼─── Zeta
       │       └──── Eta
 Alpha ┼ Gamma ─── Theta
       │       ┌─── Iota
       └ Delta ┼── Kappa
               └─ Lambda
```



### =Decorated outline=


```JavaScript
(() => {
    'use strict';

    // drawTree :: Bool -> Tree String -> String
    const drawTree = blnCompact => tree => {
        // Simple decorated-outline style of ascii tree drawing,
        // with nodeless lines pruned out if blnCompact is True.
        const xs = draw(tree);
        return unlines(
            blnCompact ? (
                xs.filter(
                    s => s.split('')
                    .some(c => !' │'.includes(c))
                )
            ) : xs
        );
    };

    // draw :: Tree String -> [String]
    const draw = node => {
        // shift :: String -> String -> [String] -> [String]
        const shift = (first, other, xs) =>
            zipWith(
                append,
                cons(first, replicate(xs.length - 1, other)),
                xs
            );
        // drawSubTrees :: [Tree String] -> [String]
        const drawSubTrees = xs => {
            const lng = xs.length;
            return 0 < lng ? (
                1 < lng ? append(
                    cons(
                        '│',
                        shift('├─ ', '│  ', draw(xs[0]))
                    ),
                    drawSubTrees(xs.slice(1))
                ) : cons('│', shift('└─ ', '   ', draw(xs[0])))
            ) : [];
        };
        return append(
            lines(node.root.toString()),
            drawSubTrees(node.nest)
        );
    };

    // TEST -----------------------------------------------
    const main = () => {
        const tree = Node(
            'Alpha', [
                Node('Beta', [
                    Node('Epsilon', []),
                    Node('Zeta', []),
                    Node('Eta', [])
                ]),
                Node('Gamma', [Node('Theta', [])]),
                Node('Delta', [
                    Node('Iota', []),
                    Node('Kappa', []),
                    Node('Lambda', [])
                ])
            ]);

        return [true, false]
        .map(blnCompact => drawTree(blnCompact)(tree))
        .join('\n\n');
    };

    // GENERIC FUNCTIONS ----------------------------------

    // Node :: a -> [Tree a] -> Tree a
    const Node = (v, xs) => ({
        type: 'Node',
        root: v, // any type of value (consistent across tree)
        nest: xs || []
    });

    // append (++) :: [a] -> [a] -> [a]
    // append (++) :: String -> String -> String
    const append = (xs, ys) => xs.concat(ys);

    // chars :: String -> [Char]
    const chars = s => s.split('');

    // cons :: a -> [a] -> [a]
    const cons = (x, xs) => [x].concat(xs);

    // Returns Infinity over objects without finite length.
    // This enables zip and zipWith to choose the shorter
    // argument when one is non-finite, like cycle, repeat etc

    // length :: [a] -> Int
    const length = xs =>
        (Array.isArray(xs) || 'string' === typeof xs) ? (
            xs.length
        ) : Infinity;

    // lines :: String -> [String]
    const lines = s => s.split(/[\r\n]/);

    // replicate :: Int -> a -> [a]
    const replicate = (n, x) =>
        Array.from({
            length: n
        }, () => x);

    // take :: Int -> [a] -> [a]
    const take = (n, xs) =>
        xs.slice(0, n);

    // unlines :: [String] -> String
    const unlines = xs => xs.join('\n');

    // Use of `take` and `length` here allows zipping with non-finite lists
    // i.e. generators like cycle, repeat, iterate.

    // zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
    const zipWith = (f, xs, ys) => {
        const
            lng = Math.min(length(xs), length(ys)),
            as = take(lng, xs),
            bs = take(lng, ys);
        return Array.from({
            length: lng
        }, (_, i) => f(as[i], bs[i], i));
    };

    // MAIN ---
    return main();
})();
```

```txt
Alpha
├─ Beta
│  ├─ Epsilon
│  ├─ Zeta
│  └─ Eta
├─ Gamma
│  └─ Theta
└─ Delta
   ├─ Iota
   ├─ Kappa
   └─ Lambda

Alpha
│
├─ Beta
│  │
│  ├─ Epsilon
│  │
│  ├─ Zeta
│  │
│  └─ Eta
│
├─ Gamma
│  │
│  └─ Theta
│
└─ Delta
   │
   ├─ Iota
   │
   ├─ Kappa
   │
   └─ Lambda
```



## Julia

Run from Julia REPL.

```Julia
using Gadfly, LightGraphs, GraphPlot

gx = kronecker(5, 12, 0.57, 0.19, 0.19)
gplot(gx)

```



## Kotlin

```scala
// version 1.2.0

import java.util.Random

class Stem(var str: String? = null, var next: Stem? = null)

const val SDOWN = "  |"
const val SLAST = "  `"
const val SNONE = "   "

val rand = Random()

fun tree(root: Int, head: Stem?) {
    val col = Stem()
    var head2 = head
    var tail = head
    while (tail != null) {
        print(tail.str)
        if (tail.next == null) break
        tail = tail.next
    }
    println("--$root")
    if (root <= 1) return
    if (tail != null && tail.str == SLAST) tail.str = SNONE
    if (tail == null) {
        head2 = col
        tail = head2
    }
    else {
        tail.next = col
    }
    var root2 = root
    while (root2 != 0) { // make a tree by doing something random
        val r = 1 + rand.nextInt(root2)
        root2 -= r
        col.str = if (root2 != 0) SDOWN else SLAST
        tree(r, head2)
    }
    tail.next = null
}

fun main(args: Array<String>) {
    val n = 8
    tree(n, null)
}
```


Sample output (unlike the C entry, should be different each time it's run):

```txt

--8
  |--7
  |  |--6
  |  |  |--5
  |  |  |  |--3
  |  |  |  |  |--2
  |  |  |  |  |  |--1
  |  |  |  |  |  `--1
  |  |  |  |  `--1
  |  |  |  `--2
  |  |  |     `--2
  |  |  |        |--1
  |  |  |        `--1
  |  |  `--1
  |  `--1
  `--1

```



## Lingo


```lingo
-- parent script "TreeItem"
-- (minimal implementation with direct property access)

property name
property children

on new (me, itemName)
  me.name = itemName
  me.children = []
  return me
end

on addChild (me, child)
  me.children.add(child)
end

-- print a tree
on printTree (me, treeItem, indent)
  if voidP(treeItem) then treeItem = me
  if voidP(indent) then indent = ""
  put indent&treeItem.name
  repeat with c in treeItem.children
    me.printTree(c, indent&"  ")
  end repeat
end
```

Usage:

```lingo
-- create a tree
root = script("TreeItem").new("root")
a = script("TreeItem").new("a")
root.addChild(a)
b = script("TreeItem").new("b")
root.addChild(b)
a1 = script("TreeItem").new("a1")
a.addChild(a1)
a11 = script("TreeItem").new("a11")
a1.addChild(a11)
a12 = script("TreeItem").new("a12")
a1.addChild(a12)
b1 = script("TreeItem").new("b1")
b.addChild(b1)

-- print the tree
root.printTree()
```


```txt

-- "root"
-- "  a"
-- "    a1"
-- "      a11"
-- "      a12"
-- "  b"
-- "    b1"

```



## Maple


```Maple
T := GraphTheory:-Graph([1, 2, 3, 4, 5], {{1, 2}, {2, 3}, {2, 4}, {4, 5}}):
GraphTheory:-DrawGraph(T, style = tree);
```



## Mathematica



###  Tree graph

Make a tree graph.  In Mathematica, '''\[DirectedEdge]''' will appear as an arrow in the code.


```Mathematica
edges = {1 \[DirectedEdge] 2, 1 \[DirectedEdge] 3, 2 \[DirectedEdge] 4, 2 \[DirectedEdge] 5,
              3 \[DirectedEdge] 6, 4 \[DirectedEdge] 7};
t = TreeGraph[edges, GraphStyle -> "VintageDiagram"]
```


 [[File:Tree.jpg]]

Show the syntactical structure of the above code.  '''Defer''' is added to impede '''TreeGraph''' from becoming a graphical object.


```Mathematica
TreeForm[Defer@
  TreeGraph[{1 \[DirectedEdge] 2, 1 \[DirectedEdge] 3, 2 \[DirectedEdge] 4, 2 \[DirectedEdge] 5,
   3 \[DirectedEdge] 6,  4 \[DirectedEdge] 7}, VertexLabels -> "Name"]]
```


[[File:syntax.jpg]]


###  Opener view


Here's another way to display a tree. The triangles open/close when clicked on.


```Mathematica
OpenerView[{1, Column@{OpenerView[{2, Column@{OpenerView[{4, 7}, True], 5}}, True],
     OpenerView[{3,  OpenerView[{TraditionalForm[Cos[x]], Plot[Cos[x], {x, 0, 10}, ImageSize -> 150]},
     True]}, True]}}, True]
```


[[File:opener.jpg]]


## Maxima


```maxima
load(graphs)$

g: random_tree(10)$

is_tree(g);
true

draw_graph(g)$
```



## Nim

```nim
import strutils

type
  Node[T] = ref TNode[T]
  TNode[T] = object
    data: T
    left, right: Node[T]

proc n[T](data: T; left, right: Node[T] = nil): Node[T] =
  Node[T](data: data, left: left, right: right)

proc indent[T](n: Node[T]): seq[string] =
  if n == nil: return @["-- (null)"]

  result = @["--" & $n.data]

  for a in indent n.left: result.add "  |" & a

  let r = indent n.right
  result.add "  `" & r[0]
  for a in r[1..r.high]: result.add "   " & a

let tree = 1.n(2.n(4.n(7.n),5.n),3.n(6.n(8.n,9.n)))

echo tree.indent.join("\n")
```



## Perl



```perl
#!/usr/bin/perl
use warnings;
use strict;
use utf8;
use open OUT => ':utf8', ':std';

sub parse {
    my ($tree) = shift;
    if (my ($root, $children) = $tree =~ /^(.+?)\((.*)\)$/) {

        my $depth = 0;
        for my $pos (0 .. length($children) - 1) {
            my $char = \substr $children, $pos, 1;
            if (0 == $depth and ',' eq $$char) {
                $$char = "\x0";
            } elsif ('(' eq $$char) {
                $depth++;
            } elsif (')' eq $$char) {
                $depth--;
            }
        }
        return($root, [map parse($_), split /\x0/, $children]);

    } else { # Leaf.
        return $tree;
    }
}

sub output {
    my ($parsed, $prefix) = @_;
    my $is_root = not defined $prefix;
    $prefix //= ' ';
    while (my $member = shift @$parsed) {
        my $last = !@$parsed || (1 == @$parsed and ref $parsed->[0]);
        unless ($is_root) {
            substr $prefix, -3, 1, ' ';
            substr($prefix, -4, 1) =~ s/├/│/;
            substr $prefix, -2, 1, ref $member ? ' ' : '└' if $last;
        }

        if (ref $member) {
            output($member, $prefix . '├─');
        } else {
            print $prefix, $member, "\n";
        }
    }
}

my $tree = 'a(b0(c1,c2(d(ef,gh)),c3(i1,i2,i3(jj),i4(kk,m))),b1(C1,C2(D1(E),D2,D3),C3))';
my $parsed = [parse($tree)];
output($parsed);
```

```txt
 a
 ├─b0
 │ ├─c1
 │ ├─c2
 │ │ └─d
 │ │   ├─ef
 │ │   └─gh
 │ └─c3
 │   ├─i1
 │   ├─i2
 │   ├─i3
 │   │ └─jj
 │   └─i4
 │     ├─kk
 │     └─m
 └─b1
   ├─C1
   ├─C2
   │ ├─D1
   │ │ └─E
   │ ├─D2
   │ └─D3
   └─C3
```



## Perl 6



```perl6
sub visualize-tree($tree, &label, &children,
                   :$indent = '',
                   :@mid = ('├─', '│ '),
                   :@end = ('└─', '  '),
) {
    sub visit($node, *@pre) {
        | gather {
            take @pre[0] ~ label($node);
            my @children := children($node);
            my $end = @children.end;
            for @children.kv -> $_, $child {
                when $end { take visit($child, (@pre[1] X~ @end)) }
                default   { take visit($child, (@pre[1] X~ @mid)) }
            }
        }
    }
    visit($tree, $indent xx 2);
}

# example tree built up of pairs
my $tree = root=>[a=>[a1=>[a11=>[]]],b=>[b1=>[b11=>[]],b2=>[],b3=>[]]];

.map({.join("\n")}).join("\n").say for visualize-tree($tree, *.key, *.value.list);
```


```txt
root
├─a
│ └─a1
│   └─a11
└─b
  ├─b1
  │ └─b11
  ├─b2
  └─b3
```



## Phix


```Phix
function rand_tree(integer low, integer high)
    for i=1 to 2 do
        integer v = rand(high-low+1)-1+low
        if v!=low
        and v!=high then
            return {v,rand_tree(low,v),rand_tree(v,high)}
        end if
    end for
    return 0
end function

object tree = rand_tree(0,20)   -- (can be 0, ~1% chance)

constant Horizontal = #C4,
         Horizontals = "\#C4",
         TopLeft = #DA,
         Vertical = #B3,
         BtmLeft = #C0

procedure visualise_tree(object tree, string root=Horizontals)
    if atom(tree) then
        puts(1,"<empty>\n")
    else
        object {v,l,r} = tree
        integer g = root[$]
        if sequence(l) then
            root[$] = iff(g=TopLeft or g=Horizontal?' ':Vertical)
            visualise_tree(l,root&TopLeft)
        end if
        root[$] = g
        puts(1,root)
        ?v
        if sequence(r) then
            root[$] = iff(g=TopLeft?Vertical:' ')
            visualise_tree(r,root&BtmLeft)
        end if
    end if
end procedure

visualise_tree(tree)
```

```txt

   ┌3
   │└4
   │ └5
  ┌7
 ┌9
 │└10
 │ └11
─12
 │ ┌13
 │┌14
 └15
  │ ┌16
  │┌17
  ││└18
  └19

```

A much simpler but less aesthetically pleasing way is just

```Phix
pp(tree,{pp_Nest,10})
```

```txt

{1,
 0,
 {5,
  0,
  {9,
   {8,
    {6,
     0,
     0},
    0},
   0}}}

```



## PicoLisp


'view' is a built-in function in PicoLisp.


```PicoLisp
(view '(1 (2 (3 (4) (5) (6 (7))) (8 (9)) (10)) (11 (12) (13))))
```


Output:

```txt

+-- 1
|
+---+-- 2
|   |
|   +---+-- 3
|   |   |
|   |   +---+-- 4
|   |   |
|   |   +---+-- 5
|   |   |
|   |   +---+-- 6
|   |       |
|   |       +---+-- 7
|   |
|   +---+-- 8
|   |   |
|   |   +---+-- 9
|   |
|   +---+-- 10
|
+---+-- 11
    |
    +---+-- 12
    |
    +---+-- 13

```



## Prolog


### XPCE

XPCE is the SWI-Prolog native GUI library.

```prolog
% direction may be horizontal/vertical/list
display_tree(Direction) :-
	sformat(A, 'Display tree ~w', [Direction]),
	new(D, window(A)),
	send(D, size, size(350,200)),
	new(T, tree(text('Root'))),
	send(T, neighbour_gap, 10),
	new(S1, node(text('Child1'))),
	new(S2, node(text('Child2'))),
	send_list(T, son,[S1,S2]),
	new(S11, node(text('Grandchild1'))),
	new(S12, node(text('Grandchild2'))),
	send_list(S1, son, [S11, S12]),
	new(S21, node(text('Grandchild3'))),
	new(S22, node(text('Grandchild4'))),
	send_list(S2, son, [S21, S22]),
	send(T, direction, Direction),
	send(D, display, T),
	send(D, open).

```

[[File:display_tree.png|900px]]

## Python


### Library module

Python has the [http://www.doughellmann.com/PyMOTW/pprint/ pprint] [http://docs.python.org/py3k/library/pprint.html module] for pretty-printing data.

If you set the presumed width of the output to 1 then pprint will print each level of a nested tuple (which is Pythons obvious method of creating a tree), on a separate line:

```python
Python 3.2.3 (default, May  3 2012, 15:54:42)
[GCC 4.6.3] on linux2
Type "copyright", "credits" or "license()" for more information.
>>> help('pprint.pprint')
Help on function pprint in pprint:

pprint.pprint = pprint(object, stream=None, indent=1, width=80, depth=None)
    Pretty-print a Python object to a stream [default is sys.stdout].

>>> from pprint import pprint
>>> for tree in [ (1, 2, 3, 4, 5, 6, 7, 8),
	          (1, (( 2, 3 ), (4, (5, ((6, 7), 8))))),
	          ((((1, 2), 3), 4), 5, 6, 7, 8) ]:
	print("\nTree %r can be pprint'd as:" % (tree, ))
	pprint(tree, indent=1, width=1)



Tree (1, 2, 3, 4, 5, 6, 7, 8) can be pprint'd as:
(1,
 2,
 3,
 4,
 5,
 6,
 7,
 8)

Tree (1, ((2, 3), (4, (5, ((6, 7), 8))))) can be pprint'd as:
(1,
 ((2,
   3),
  (4,
   (5,
    ((6,
      7),
     8)))))

Tree ((((1, 2), 3), 4), 5, 6, 7, 8) can be pprint'd as:
((((1,
    2),
   3),
  4),
 5,
 6,
 7,
 8)
>>>
```


pprint (and print), prints Pythons standard container types in a format that is valid python so Python could parse its output:

```python>>>
 tree = "a",("b0",("c1","c2",("d",("ef","gh")),"c3",("i1","i2","i3",("jj"),"i4",("kk","m"))),"b1",("C1","C2",("D1",("E"),"D2","D3"),"C3"))
>>> pprint(tree, width=1)
('a',
 ('b0',
  ('c1',
   'c2',
   ('d',
    ('ef',
     'gh')),
   'c3',
   ('i1',
    'i2',
    'i3',
    'jj',
    'i4',
    ('kk',
     'm'))),
  'b1',
  ('C1',
   'C2',
   ('D1',
    'E',
    'D2',
    'D3'),
   'C3')))
>>> copypasteoutput = ('a',
...  ('b0',
...   ('c1',
...    'c2',
...    ('d',
...     ('ef',
...      'gh')),
...    'c3',
...    ('i1',
...     'i2',
...     'i3',
...     'jj',
...     'i4',
...     ('kk',
...      'm'))),
...   'b1',
...   ('C1',
...    'C2',
...    ('D1',
...     'E',
...     'D2',
...     'D3'),
...    'C3')))
>>> tree == copypasteoutput
True
>>>
```


pprints width parameter allows it to fold some structure to better fit the page:

```python>>>
 pprint(tree, width=60)
('a',
 ('b0',
  ('c1',
   'c2',
   ('d', ('ef', 'gh')),
   'c3',
   ('i1', 'i2', 'i3', 'jj', 'i4', ('kk', 'm'))),
  'b1',
  ('C1', 'C2', ('D1', 'E', 'D2', 'D3'), 'C3')))
>>>
```


pprint works with with a mix of nested container types. Here we create a tree from both lists and tuples:

```python>>>
 mixedtree = ['a', ('b0', ('c1', 'c2', ['d', ('ef', 'gh')], 'c3', ('i1', 'i2',
...              'i3', 'jj', 'i4', ['kk', 'm'])), 'b1', ('C1', 'C2', ('D1', 'E',
...              'D2', 'D3'), 'C3'))]
>>> pprint(mixedtree, width=1)
['a',
 ('b0',
  ('c1',
   'c2',
   ['d',
    ('ef',
     'gh')],
   'c3',
   ('i1',
    'i2',
    'i3',
    'jj',
    'i4',
    ['kk',
     'm'])),
  'b1',
  ('C1',
   'C2',
   ('D1',
    'E',
    'D2',
    'D3'),
   'C3'))]
>>> pprint(mixedtree, width=60)
['a',
 ('b0',
  ('c1',
   'c2',
   ['d', ('ef', 'gh')],
   'c3',
   ('i1', 'i2', 'i3', 'jj', 'i4', ['kk', 'm'])),
  'b1',
  ('C1', 'C2', ('D1', 'E', 'D2', 'D3'), 'C3'))]
>>>
```




### Functional composition


### =Vertically centered parents=

Using the same tree structure (including tree node constructor and accessors) as in the [[Tree_traversal|Tree Traversal]] task, and centering parent nodes vertically:

```python
'''Textually visualized tree, with vertically-centered parent nodes'''

from functools import reduce
from itertools import (chain, takewhile)

'''
               ┌ Epsilon
               ├─── Zeta
       ┌─ Beta ┼──── Eta
       │       │         ┌───── Mu
       │       └── Theta ┤
 Alpha ┤                 └───── Nu
       ├ Gamma ────── Xi ─ Omicron
       │       ┌─── Iota
       └ Delta ┼── Kappa
               └─ Lambda
'''
# Tree style and algorithm inspired by the Haskell snippet at:
# https://doisinkidney.com/snippets/drawing-trees.html


# drawTree2 :: Bool -> Bool -> Tree a -> String
def drawTree2(blnCompact):
    '''Monospaced UTF8 left-to-right text tree in a
       compact or expanded format, with any lines
       containing no nodes optionally pruned out.
    '''
    def go(blnPruned, tree):
        # measured :: a -> (Int, String)
        def measured(x):
            '''Value of a tree node
               tupled with string length.
            '''
            s = ' ' + str(x) + ' '
            return len(s), s

        # lmrFromStrings :: [String] -> ([String], String, [String])
        def lmrFromStrings(xs):
            '''Lefts, Mid, Rights.'''
            i = len(xs) // 2
            ls, rs = xs[0:i], xs[i:]
            return ls, rs[0], rs[1:]

        # stringsFromLMR :: ([String], String, [String]) -> [String]
        def stringsFromLMR(lmr):
            ls, m, rs = lmr
            return ls + [m] + rs

        # fghOverLMR
        # :: (String -> String)
        # -> (String -> String)
        # -> (String -> String)
        # -> ([String], String, [String])
        # -> ([String], String, [String])
        def fghOverLMR(f, g, h):
            def go(lmr):
                ls, m, rs = lmr
                return (
                    [f(x) for x in ls],
                    g(m),
                    [h(x) for x in rs]
                )
            return lambda lmr: go(lmr)

        # leftPad :: Int -> String -> String
        def leftPad(n):
            return lambda s: (' ' * n) + s

        # treeFix :: (Char, Char, Char) -> ([String], String, [String])
        #                               ->  [String]
        def treeFix(l, m, r):
            def cfix(x):
                return lambda xs: x + xs
            return compose(stringsFromLMR)(
                fghOverLMR(cfix(l), cfix(m), cfix(r))
            )

        def lmrBuild(w, f):
            def go(wsTree):
                nChars, x = wsTree['root']
                _x = ('─' * (w - nChars)) + x
                xs = wsTree['nest']
                lng = len(xs)

                # linked :: String -> String
                def linked(s):
                    c = s[0]
                    t = s[1:]
                    return _x + '┬' + t if '┌' == c else (
                        _x + '┤' + t if '│' == c else (
                            _x + '┼' + t if '├' == c else (
                                _x + '┴' + t
                            )
                        )
                    )

                # LEAF ------------------------------------
                if 0 == lng:
                    return ([], _x, [])

                # SINGLE CHILD ----------------------------
                elif 1 == lng:
                    def lineLinked(z):
                        return _x + '─' + z
                    rightAligned = leftPad(1 + w)
                    return fghOverLMR(
                        rightAligned,
                        lineLinked,
                        rightAligned
                    )(f(xs[0]))

                # CHILDREN --------------------------------
                else:
                    rightAligned = leftPad(w)
                    lmrs = [f(x) for x in xs]
                    return fghOverLMR(
                        rightAligned,
                        linked,
                        rightAligned
                    )(
                        lmrFromStrings(
                            intercalate([] if blnCompact else ['│'])(
                                [treeFix(' ', '┌', '│')(lmrs[0])] + [
                                    treeFix('│', '├', '│')(x) for x
                                    in lmrs[1:-1]
                                ] + [treeFix('│', '└', ' ')(lmrs[-1])]
                            )
                        )
                    )
            return lambda wsTree: go(wsTree)

        measuredTree = fmapTree(measured)(tree)
        levelWidths = reduce(
            lambda a, xs: a + [max(x[0] for x in xs)],
            levels(measuredTree),
            []
        )
        treeLines = stringsFromLMR(
            foldr(lmrBuild)(None)(levelWidths)(
                measuredTree
            )
        )
        return [
            s for s in treeLines
            if any(c not in '│ ' for c in s)
        ] if (not blnCompact and blnPruned) else treeLines

    return lambda blnPruned: (
        lambda tree: '\n'.join(go(blnPruned, tree))
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Trees drawn in varying formats'''

    # tree1 :: Tree Int
    tree1 = Node(1)([
        Node(2)([
            Node(4)([
                Node(7)([])
            ]),
            Node(5)([])
        ]),
        Node(3)([
            Node(6)([
                Node(8)([]),
                Node(9)([])
            ])
        ])
    ])

    # tree :: Tree String
    tree2 = Node('Alpha')([
        Node('Beta')([
            Node('Epsilon')([]),
            Node('Zeta')([]),
            Node('Eta')([]),
            Node('Theta')([
                Node('Mu')([]),
                Node('Nu')([])
            ])
        ]),
        Node('Gamma')([
            Node('Xi')([Node('Omicron')([])])
        ]),
        Node('Delta')([
            Node('Iota')([]),
            Node('Kappa')([]),
            Node('Lambda')([])
        ])
    ])

    print(
        '\n\n'.join([
            'Fully compacted (parents not all centered):',
            drawTree2(True)(False)(
                tree1
            ),
            'Expanded with vertically centered parents:',
            drawTree2(False)(False)(
                tree2
            ),
            'Centered parents with nodeless lines pruned out:',
            drawTree2(False)(True)(
                tree2
            )
        ])
    )


# GENERIC -------------------------------------------------

# Node :: a -> [Tree a] -> Tree a
def Node(v):
    '''Contructor for a Tree node which connects a
       value of some kind to a list of zero or
       more child trees.
    '''
    return lambda xs: {'type': 'Tree', 'root': v, 'nest': xs}


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''A concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).
    '''
    return lambda xs: list(
        chain.from_iterable(map(f, xs))
    )


# fmapTree :: (a -> b) -> Tree a -> Tree b
def fmapTree(f):
    '''A new tree holding the results of
       applying f to each root in
       the existing tree.
    '''
    def go(x):
        return Node(f(x['root']))(
            [go(v) for v in x['nest']]
        )
    return lambda tree: go(tree)


# foldr :: (a -> b -> b) -> b -> [a] -> b
def foldr(f):
    '''Right to left reduction of a list,
       using the binary operator f, and
       starting with an initial accumulator value.
    '''
    def g(x, a):
        return f(a, x)
    return lambda acc: lambda xs: reduce(
        g, xs[::-1], acc
    )


# intercalate :: [a] -> [[a]] -> [a]
# intercalate :: String -> [String] -> String
def intercalate(x):
    '''The concatenation of xs
       interspersed with copies of x.
    '''
    return lambda xs: x.join(xs) if isinstance(x, str) else list(
        chain.from_iterable(
            reduce(lambda a, v: a + [x, v], xs[1:], [xs[0]])
        )
    ) if xs else []


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


# levels :: Tree a -> [[a]]
def levels(tree):
    '''A list of the nodes at each level of the tree.'''
    return list(
        map_(map_(root))(
            takewhile(
                bool,
                iterate(concatMap(nest))(
                    [tree]
                )
            )
        )
    )


# map :: (a -> b) -> [a] -> [b]
def map_(f):
    '''The list obtained by applying f
       to each element of xs.
    '''
    return lambda xs: list(map(f, xs))


# nest :: Tree a -> [Tree a]
def nest(t):
    '''Accessor function for children of tree node.'''
    return t['nest'] if 'nest' in t else None


# root :: Tree a -> a
def root(t):
    '''Accessor function for data of tree node.'''
    return t['root'] if 'root' in t else None


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
Fully compacted (parents not all centered):

       ┌ 4 ─ 7
   ┌ 2 ┴ 5
 1 ┤       ┌ 8
   └ 3 ─ 6 ┴ 9

Expanded with vertically centered parents:

               ┌ Epsilon
               │
               ├─── Zeta
               │
       ┌─ Beta ┼──── Eta
       │       │
       │       │         ┌───── Mu
       │       └── Theta ┤
 Alpha ┤                 └───── Nu
       │
       ├ Gamma ────── Xi ─ Omicron
       │
       │       ┌─── Iota
       │       │
       └ Delta ┼── Kappa
               │
               └─ Lambda

Centered parents with nodeless lines pruned out:

               ┌ Epsilon
               ├─── Zeta
       ┌─ Beta ┼──── Eta
       │       │         ┌───── Mu
       │       └── Theta ┤
 Alpha ┤                 └───── Nu
       ├ Gamma ────── Xi ─ Omicron
       │       ┌─── Iota
       └ Delta ┼── Kappa
               └─ Lambda
```


====Simple decorated-outline tree====
```python
'''Visualize a tree'''

from itertools import (repeat, starmap)
from operator import (add)


# drawTree :: Tree a -> String
def drawTree(tree):
    '''ASCII diagram of a tree.'''
    return '\n'.join(draw(tree))


# draw :: Tree a -> [String]
def draw(node):
    '''List of the lines of an ASCII
       diagram of a tree.'''
    def shift(first, other, xs):
        return list(starmap(
            add,
            zip(
                [first] + list(
                    repeat(other, len(xs) - 1)
                ),
                xs
            )
        ))

    def drawSubTrees(xs):
        return (
            (
                ['│'] + shift(
                    '├─ ', '│  ', draw(xs[0])
                ) + drawSubTrees(xs[1:])
            ) if 1 < len(xs) else ['│'] + shift(
                '└─ ', '   ', draw(xs[0])
            )
        ) if xs else []

    return (str(root(node))).splitlines() + (
        drawSubTrees(nest(node))
    )


# TEST ----------------------------------------------------
# main :: IO ()
def main():
    '''Test'''

    # tree :: Tree Int
    tree = Node(1)([
        Node(2)([
            Node(4)([
                Node(7)([])
            ]),
            Node(5)([])
        ]),
        Node(3)([
            Node(6)([
                Node(8)([]),
                Node(9)([])
            ])
        ])
    ])

    print(drawTree(tree))


# GENERIC -------------------------------------------------


# Node :: a -> [Tree a] -> Tree a
def Node(v):
    '''Contructor for a Tree node which connects a
       value of some kind to a list of zero or
       more child trees.'''
    return lambda xs: {'type': 'Node', 'root': v, 'nest': xs}


# nest :: Tree a -> [Tree a]
def nest(tree):
    '''Accessor function for children of tree node.'''
    return tree['nest'] if 'nest' in tree else None


# root :: Dict -> a
def root(dct):
    '''Accessor function for data of tree node.'''
    return dct['root'] if 'root' in dct else None


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt
1
│
├─ 2
│  │
│  ├─ 4
│  │  │
│  │  └─ 7
│  │
│  └─ 5
│
└─ 3
   │
   └─ 6
      │
      ├─ 8
      │
      └─ 9
```



## Racket


```Racket

#lang racket/base

(define (visualize t0)
  (let loop ([t t0] [last? #t] [indent '()])
    (define (I mid last) (cond [(eq? t t0) ""] [last? mid] [else last]))
    (for-each display (reverse indent))
    (unless (eq? t t0) (printf "|\n"))
    (for-each display (reverse indent))
    (printf "~a~a\n" (I "\\-" "+-") (car t))
    (for ([s (cdr t)] [n (in-range (- (length t) 2) -1 -1)])
      (loop s (zero? n) (cons (I "  " "| ") indent)))))

(visualize '(1 (2 (3 (4) (5) (6 (7))) (8 (9)) (10)) (11 (12) (13))))

```


Output:

```txt

1
|
+-2
| |
| +-3
| | |
| | +-4
| | |
| | +-5
| | |
| | \-6
| |   |
| |   \-7
| |
| +-8
| | |
| | \-9
| |
| \-10
|
\-11
  |
  +-12
  |
  \-13

```



## REXX


```rexx
/* REXX ***************************************************************
* 10.05.2014 Walter Pachl using the tree and the output format of C
**********************************************************************/
Call mktree
Say node.1.0name
Call tt 1,''
Exit

tt: Procedure Expose node.
/**********************************************************************
* show a subtree (recursively)
**********************************************************************/
 Parse Arg k,st
 Do i=1 To node.k.0
   If i=node.k.0 Then
     s='`--'
   Else
     s='|--'
   c=node.k.i
   If st<>'' Then
     st=left(st,length(st)-2)'  '
   st=changestr('` ',st,'  ')
   Say st||s||node.c.0name
   Call tt c,st||s
   End
 Return
Exit

mktree: Procedure Expose node. root
/**********************************************************************
* build the tree according to the task
**********************************************************************/
  node.=0
  r=mknode('R');
  a=mknode('A'); Call attchild a,r
  b=mknode('B'); Call attchild b,a
  c=mknode('C'); Call attchild c,a
  d=mknode('D'); Call attchild d,b
  e=mknode('E'); Call attchild e,b
  f=mknode('F'); Call attchild f,b
  g=mknode('G'); Call attchild g,b
  h=mknode('H'); Call attchild h,d
  i=mknode('I'); Call attchild i,h
  j=mknode('J'); Call attchild j,i
  k=mknode('K'); Call attchild k,j
  l=mknode('L'); Call attchild l,j
  m=mknode('M'); Call attchild m,e
  n=mknode('N'); Call attchild n,e
  Return

mknode: Procedure Expose node.
/**********************************************************************
* create a new node
**********************************************************************/
  Parse Arg name
  z=node.0+1
  node.z.0name=name
  node.0=z
  Return z                        /* number of the node just created */

attchild: Procedure Expose node.
/**********************************************************************
* make a the next child of father
**********************************************************************/
  Parse Arg a,father
  node.a.0father=father
  z=node.father.0+1
  node.father.z=a
  node.father.0=z
  node.a.0level=node.father.0level+1
  Return

```

```txt
R
`--A
   |--B
   |  |--D
   |  |  `--H
   |  |     `--I
   |  |        `--J
   |  |           |--K
   |  |           `--L
   |  |--E
   |  |  |--M
   |  |  `--N
   |  |--F
   |  `--G
   `--C
```



## Ruby

Modifying [[Tree_traversal#Ruby]] by adding somewhere after the line

```Ruby

root = BinaryTreeNode.from_array [1, [2, [4, 7], [5]], [3, [6, [8], [9]]]]

```

the lines

```Ruby

require 'pp'
pp root

```

will produce:
```txt

  #<BinaryTreeNode:0x804f854
   @left=
    #<BinaryTreeNode:0x804fad8
     @left=#<BinaryTreeNode:0x804fc28 @left=nil, @right=nil, @value=7>,
     @right=nil,
     @value=4>,
   @right=#<BinaryTreeNode:0x804f9c0 @left=nil, @right=nil, @value=5>,
   @value=2>,
 @right=
  #<BinaryTreeNode:0x804f074
   @left=
    #<BinaryTreeNode:0x804f218
     @left=#<BinaryTreeNode:0x804f544 @left=nil, @right=nil, @value=8>,
     @right=#<BinaryTreeNode:0x804f384 @left=nil, @right=nil, @value=9>,
     @value=6>,
   @right=nil,
   @value=3>,
 @value=1>

```


```Ruby

def ptree(tree,indent="  ")
  case tree
  when Array
    head,*tail=tree
    ptree(head,indent)
    s=tail.size-1
    tail.each_with_index { |tree1,i| ptree(tree1,"#{indent}#{((i==s) ? ' ':'|')}  ") }
  else
     puts(indent.gsub(/\s\s$/,"--").gsub(/ --$/,"\\--")+tree.to_s)
  end
end
ptree [1,2,3,[4,5,6,[7,8,9]],3,[22,33]]

```

will produce:
```txt

--1
  |--2
  |--3
  |--4
  |  |--5
  |  |--6
  |  \--7
  |     |--8
  |     \--9
  |--3
  \--22
     \--33

```



## Rust

Console visualization of binary trees translated from parts of [http://rosettacode.org/wiki/AVL_tree/C the C AVL tree solution].

```Rust

extern crate rustc_serialize;
extern crate term_painter;

use rustc_serialize::json;
use std::fmt::{Debug, Display, Formatter, Result};
use term_painter::ToStyle;
use term_painter::Color::*;

type NodePtr = Option<usize>;

#[derive(Debug, PartialEq, Clone, Copy)]
enum Side {
    Left,
    Right,
    Up,
}

#[derive(Debug, PartialEq, Clone, Copy)]
enum DisplayElement {
    TrunkSpace,
    SpaceLeft,
    SpaceRight,
    SpaceSpace,
    Root,
}

impl DisplayElement {
    fn string(&self) -> String {
        match *self {
            DisplayElement::TrunkSpace => "    │   ".to_string(),
            DisplayElement::SpaceRight => "    ┌───".to_string(),
            DisplayElement::SpaceLeft => "    └───".to_string(),
            DisplayElement::SpaceSpace => "        ".to_string(),
            DisplayElement::Root => "├──".to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy, RustcDecodable, RustcEncodable)]
struct Node<K, V> {
    key: K,
    value: V,
    left: NodePtr,
    right: NodePtr,
    up: NodePtr,
}

impl<K: Ord + Copy, V: Copy> Node<K, V> {
    pub fn get_ptr(&self, side: Side) -> NodePtr {
        match side {
            Side::Up => self.up,
            Side::Left => self.left,
            _ => self.right,
        }
    }
}

#[derive(Debug, RustcDecodable, RustcEncodable)]
struct Tree<K, V> {
    root: NodePtr,
    store: Vec<Node<K, V>>,
}

impl<K: Ord + Copy + Debug + Display, V: Debug + Copy + Display> Tree<K, V> {
    pub fn get_node(&self, np: NodePtr) -> Node<K, V> {
        assert!(np.is_some());
        self.store[np.unwrap()]
    }

    pub fn get_pointer(&self, np: NodePtr, side: Side) -> NodePtr {
        assert!(np.is_some());
        self.store[np.unwrap()].get_ptr(side)
    }

    // Prints the tree with root p.  The idea is to do an in-order traversal
    // (reverse in-order in this case, where right is on top), and print nodes as they
    // are visited, one per line. Each invocation of display() gets its own copy
    // of the display element vector e, which is grown with either whitespace or
    // a trunk element, then modified in its last and possibly second-to-last
    // characters in context.
    fn display(&self, p: NodePtr, side: Side, e: &Vec<DisplayElement>, f: &mut Formatter) {
        if p.is_none() {
            return;
        }

        let mut elems = e.clone();
        let node = self.get_node(p);
        let mut tail = DisplayElement::SpaceSpace;
        if node.up != self.root {
            // If the direction is switching, I need the trunk element to appear in the lines
            // printed before that node is visited.
            if side == Side::Left && node.right.is_some() {
                elems.push(DisplayElement::TrunkSpace);
            } else {
                elems.push(DisplayElement::SpaceSpace);
            }
        }
        let hindex = elems.len() - 1;
        self.display(node.right, Side::Right, &elems, f);

        if p == self.root {
            elems[hindex] = DisplayElement::Root;
            tail = DisplayElement::TrunkSpace;
        } else if side == Side::Right {
            // Right subtree finished
            elems[hindex] = DisplayElement::SpaceRight;
            // Prepare trunk element in case there is a left subtree
            tail = DisplayElement::TrunkSpace;
        } else if side == Side::Left {
            elems[hindex] = DisplayElement::SpaceLeft;
            let parent = self.get_node(node.up);
            if parent.up.is_some() && self.get_pointer(parent.up, Side::Right) == node.up {
                // Direction switched, need trunk element starting with this node/line
                elems[hindex - 1] = DisplayElement::TrunkSpace;
            }
        }

        // Visit node => print accumulated elements. Each node gets a line and each line gets a
        // node.
        for e in elems.clone() {
            let _ = write!(f, "{}", e.string());
        }
        let _ = write!(f,
                       "{key:>width$} ",
                       key = Green.bold().paint(node.key),
                       width = 2);
        let _ = write!(f,
                       "{value:>width$}\n",
                       value = Blue.bold().paint(format!("{:.*}", 2, node.value)),
                       width = 4);

        // Overwrite last element before continuing traversal
        elems[hindex] = tail;

        self.display(node.left, Side::Left, &elems, f);
    }
}

impl<K: Ord + Copy + Debug + Display, V: Debug + Copy + Display> Display for Tree<K, V> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        if self.root.is_none() {
            write!(f, "[empty]")
        } else {
            let mut v: Vec<DisplayElement> = Vec::new();
            self.display(self.root, Side::Up, &mut v, f);
            Ok(())
        }
    }
}

/// Decodes and prints a previously generated tree.
fn main() {
    let encoded = r#"{"root":0,"store":[{"key":0,"value":0.45,"left":1,"right":3,
        "up":null},{"key":-8,"value":-0.94,"left":7,"right":2,"up":0}, {"key":-1,
        "value":0.15,"left":8,"right":null,"up":1},{"key":7, "value":-0.29,"left":4,
        "right":9,"up":0},{"key":5,"value":0.80,"left":5,"right":null,"up":3},
        {"key":4,"value":-0.85,"left":6,"right":null,"up":4},{"key":3,"value":-0.46,
        "left":null,"right":null,"up":5},{"key":-10,"value":-0.85,"left":null,
        "right":13,"up":1},{"key":-6,"value":-0.42,"left":null,"right":10,"up":2},
        {"key":9,"value":0.63,"left":12,"right":null,"up":3},{"key":-3,"value":-0.83,
        "left":null,"right":11,"up":8},{"key":-2,"value":0.75,"left":null,"right":null,
        "up":10},{"key":8,"value":-0.48,"left":null,"right":null,"up":9},{"key":-9,
        "value":0.53,"left":null,"right":null,"up":7}]}"#;
    let tree: Tree<i32, f32> = json::decode(&encoded).unwrap();
    println!("{}", tree);
}

```

[[File:Visualize_a_tree-rust-1.png]]


## Sidef

```ruby
func visualize_tree(tree, label, children,
                    indent = '',
                    mids = ['├─', '│ '],
                    ends = ['└─', '  '],
) {
    func visit(node, pre) {
        gather {
            take(pre[0] + label(node))
            var chldn = children(node)
            var end = chldn.end
            chldn.each_kv { |i, child|
                if (i == end) { take(visit(child, [pre[1]] ~X+ ends)) }
                else          { take(visit(child, [pre[1]] ~X+ mids)) }
            }
        }
    }
    visit(tree, [indent] * 2)
}

var tree = 'root':['a':['a1':['a11':[]]],'b':['b1':['b11':[]],'b2':[],'b3':[]]]
say visualize_tree(tree, { .first }, { .second }).flatten.join("\n")
```

```txt

root
├─a
│ └─a1
│   └─a11
└─b
  ├─b1
  │ └─b11
  ├─b2
  └─b3

```



## Tcl

```tcl
package require struct::tree

proc visualize_tree {tree {nameattr name}} {
    set path {}
    $tree walk [$tree rootname] -order both {mode node} {
	if {$mode eq "enter"} {
	    set s ""
	    foreach p $path {
		append s [expr {[$tree next $p] eq "" ? "  " : "\u2502 "}]
	    }
	    lappend path $node
	    append s [expr {
		[$tree next $node] eq "" ? "\u2514\u2500" : "\u251c\u2500"
	    }]
	    if {[$tree keyexists $node $nameattr]} {
		set name [$tree get $node $nameattr]
	    } else {
		# No node name attribute; use the raw name
		set name $node
	    }
	    puts "$s$name"
	} else {
	    set path [lrange $path 0 end-1]
	}
    }
}
```

Demonstrating:

```tcl
# Sample tree to demonstrate with
struct::tree t deserialize {root {} {} a 0 {} d 3 {} e 3 {} f 9 {} b 0 {} c 0 {}}
visualize_tree t
```

```txt

└─root
  ├─a
  │ ├─d
  │ └─e
  │   └─f
  ├─b
  └─c

```



## Yabasic


```Yabasic
clear screen

dim colore$(1)

maxCol = token("white yellow cyan green red", colore$())

showTree(0, "[1[2[3][4[5][6]][7]][8[9]]]")
print "\n\n\n"
showTree(0, "[1[2[3[4]]][5[6][7[8][9]]]]")

sub showTree(n, A$)
    local i, c$
    static co

    c$ = left$(A$, 1)

    if c$ = "" return

    switch c$
    case "[":   co = co + 1 : showTree(n + 1, right$(A$, len(A$) - 1))
                break
    case "]":   co = co - 1 : showTree(n - 1, right$(A$, len(A$) - 1))
                break
    default:    for i = 2 to n
                    print "  ";
                next i
                co = max(min(co, maxCol), 1)
                print color(colore$(co)) "\xc0-", c$
                showTree(n, right$(A$, len(A$) - 1))
                break
    end switch
end sub

```



## zkl

In zkl, the Vault is a global object store object (aka thread safe dictionary). Basically a tiny file system for objects. It has a "dir" method to display the contents
```txt

:Vault.dir()
...
Compiler
   Asm
   Compiler
Dictionary
Exception
Test
   UnitTester
   foo
      bar
...

```

It does this with data that looks like:
L("Network.TCPServerSocket","File","ZKLShell.Granny","Int","startup","Utils.Inspector","Thread.Straw","Ref","Utils.Argh" ...)

```zkl
fcn vaultDir(out=Console){
   const INDENT="   ";
   space:=""; lastPath:=L();
   foreach fullname in (TheVault.BaseClass.contents.sort()){
      path:=fullname.split("."); name:=path.pop();
      if(lastPath==path) out.writeln(space,name);
      else{
	 n:=0; p:=path.copy();
	 try{
	    while(path[0]==lastPath[0])
	        { n+=1; path.pop(0); lastPath.pop(0); }
	 }catch{}
	 space=INDENT*n;
	 foreach dir in (path){ out.writeln(space,dir); space+=INDENT; }
	 out.writeln(space,name);
	 lastPath=p;
      }
   }
   ""	// so startup has something to display
}

```

