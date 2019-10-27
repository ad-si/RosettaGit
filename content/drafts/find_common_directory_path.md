+++
title = "Find common directory path"
description = ""
date = 2019-08-20T13:59:44Z
aliases = []
[extra]
id = 6643
[taxonomies]
categories = []
tags = []
+++

{{task|String manipulation}}
Create a routine that, given a set of strings representing directory paths and a single character directory separator, will return a string representing that part of the directory tree that is common to all the directories.

Test your routine using the forward slash '/' character as the directory separator and the following three strings as input paths:
  '/home/user1/tmp/coverage/test'
  '/home/user1/tmp/covert/operator'
  '/home/user1/tmp/coven/members'

Note: The resultant path should be the valid directory <code>'/home/user1/tmp'</code> and not the longest common string <code>'/home/user1/tmp/cove'</code>.

If your language has a routine that performs this function (even if it does not have a changeable separator character), then mention it as part of the task.


;Related tasks
* [[Longest common prefix]]





## Ada


```ada

with Ada.Text_IO;  use Ada.Text_IO;

procedure Test_Common_Path is
   function "rem" (A, B : String) return String is
      Slash : Integer := A'First; -- At the last slash seen in A
      At_A  : Integer := A'first;
      At_B  : Integer := B'first;
   begin
      loop
         if At_A > A'Last then
            if At_B > B'Last or else B (At_B) = '/' then
               return A;
            else
               return A (A'First..Slash - 1);
            end if;
         elsif At_B > B'Last then
            if A (At_A) = '/' then -- A cannot be shorter than B here
               return B;
            else
               return A (A'First..Slash - 1);
            end if;
         elsif A (At_A) /= B (At_B) then
            return A (A'First..Slash - 1);
         elsif A (At_A) = '/' then
            Slash := At_A;
         end if;
         At_A := At_A + 1;
         At_B := At_B + 1;
      end loop;      
   end "rem";
begin
   Put_Line
   (  "/home/user1/tmp/coverage/test" rem
      "/home/user1/tmp/covert/operator" rem
      "/home/user1/tmp/coven/members"
   );
end Test_Common_Path;

```

Output:

```txt

/home/user1/tmp

```



## Aime


```aime
cdp(...)
{
    integer e;
    record r;
    text s;

    ucall(r_add, 1, r, 0);

    if (~r) {
        s = r.low;
        s = s.cut(0, e = b_trace(s, prefix(s, r.high), '/'));
        s = ~s || e == -1 ? s : "/";
    }

    s;
}

main(void)
{
    o_(cdp("/home/user1/tmp/coverage/test",
           "/home/user1/tmp/covert/operator",
           "/home/user1/tmp/coven/members"), "\n");

    0;
}
```



## ALGOL 68

{{works with|ALGOL 68|Revision 1 - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-1.18.0/algol68g-1.18.0-9h.tiny.el5.centos.fc11.i386.rpm/download 1.18.0-9h.tiny]}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d]}}


```algol68
# Utilities code #

CHAR dir sep = "/"; # Assume POSIX #

PROC dir name = (STRING dir)STRING: (
  STRING out;
  FOR pos FROM UPB dir BY -1 TO LWB dir DO
    IF dir[pos] = dir sep THEN
      out := dir[:pos-1];
      GO TO out exit
    FI
  OD;
  # else: # out:=""; 
  out exit: out
);

PROC shortest = ([]STRING string list)STRING: (
  INT min := max int;
  INT min key := LWB string list - 1;
  FOR key FROM LWB string list TO UPB string list DO
    IF UPB string list[key][@1] < min THEN
      min := UPB string list[key][@1];
      min key := key
    FI
  OD;
  string list[min key]
);

# Actual code #

PROC common prefix = ([]STRING strings)STRING: (
  IF LWB strings EQ UPB strings THEN
    # exit: # strings[LWB strings]
  ELSE
    STRING prefix := shortest(strings);
    FOR pos FROM LWB prefix TO UPB prefix DO
      CHAR first = prefix[pos];
      FOR key FROM LWB strings+1 TO UPB strings DO
        IF strings[key][@1][pos] NE first THEN 
          prefix := prefix[:pos-1];
          GO TO prefix exit
        FI
      OD
    OD;
    prefix exit: prefix 
  FI
);

# Test code #

test:(
  []STRING dir list = (
    "/home/user1/tmp/coverage/test",
    "/home/user1/tmp/covert/operator",
    "/home/user1/tmp/coven/members"
  );
  print((dir name(common prefix(dir list)), new line))
)
```

Output:

```txt

/home/user1/tmp

```



## AutoHotkey


```autohotkey
Dir1 := "/home/user1/tmp/coverage/test"
Dir2 := "/home/user1/tmp/covert/operator"
Dir3 := "/home/user1/tmp/coven/members"

StringSplit, Dir1_, Dir1, /
StringSplit, Dir2_, Dir2, /
StringSplit, Dir3_, Dir3, /

Loop
    If  (Dir1_%A_Index% = Dir2_%A_Index%)
    And (Dir1_%A_Index% = Dir3_%A_Index%)
        Result .= (A_Index=1 ? "" : "/") Dir1_%A_Index%
    Else Break

MsgBox, % Result
```

Message box shows:

```txt
/home/user1/tmp
```



## AWK


```awk
# Finds the longest common directory of paths[1], paths[2], ...,
# paths[count], where sep is a single-character directory separator.
function common_dir(paths, count, sep,    b, c, f, i, j, p) {
	if (count < 1)
		return ""

	p = ""	# Longest common prefix
	f = 0	# Final index before last sep

	# Loop for c = each character of paths[1].
	for (i = 1; i <= length(paths[1]); i++) {
		c = substr(paths[1], i, 1)

		# If c is not the same in paths[2], ..., paths[count]
		# then break both loops.
		b = 0
		for (j = 2; j <= count; j++) {
			if (c != substr(paths[j], i, 1)) {
				b = 1
				break
			}
		}
		if (b)
			break

		# Append c to prefix. Update f.
		p = p c
		if (c == sep)
			f = i - 1
	}

	# Return only f characters of prefix.
	return substr(p, 1, f)
}

BEGIN {
	a[1] = "/home/user1/tmp/coverage/test"
	a[2] = "/home/user1/tmp/covert/operator"
	a[3] = "/home/user1/tmp/coven/members"
	print common_dir(a, 3, "/")
}
```


Prints <tt>/home/user1/tmp</tt>.


## BASIC

{{works with|QuickBASIC|7}}
{{works with|FreeBASIC}}

This version is a ''little'' smarter than the one above... but not much. This version could be turned into an actual useful utility by changing it to compare command-line parameters, instead of built-in data.

Also, under FreeBASIC, the <code>pathSep</code> arg to <code>commonPath$</code> could be made optional, or even system-dependent.


```qbasic
DECLARE FUNCTION commonPath$ (paths() AS STRING, pathSep AS STRING)

DATA "/home/user2", "/home/user1/tmp/covert/operator", "/home/user1/tmp/coven/members"

DIM x(0 TO 2) AS STRING, n AS INTEGER
FOR n = 0 TO 2
    READ x(n)
NEXT

PRINT "Common path is '"; commonPath$(x(), "/"); "'"

FUNCTION commonPath$ (paths() AS STRING, pathSep AS STRING)
    DIM tmpint1 AS INTEGER, tmpint2 AS INTEGER, tmpstr1 AS STRING, tmpstr2 AS STRING
    DIM L0 AS INTEGER, L1 AS INTEGER, lowerbound AS INTEGER, upperbound AS INTEGER
    lowerbound = LBOUND(paths): upperbound = UBOUND(paths)

    IF (lowerbound) = upperbound THEN       'Some quick error checking...
        commonPath$ = paths(lowerbound)
    ELSEIF lowerbound > upperbound THEN     'How in the...?
        commonPath$ = ""
    ELSE
        tmpstr1 = paths(lowerbound)

        FOR L0 = (lowerbound + 1) TO upperbound 'Find common strings.
            tmpstr2 = paths(L0)
            tmpint1 = LEN(tmpstr1)
            tmpint2 = LEN(tmpstr2)
            IF tmpint1 > tmpint2 THEN tmpint1 = tmpint2
            FOR L1 = 1 TO tmpint1
                IF MID$(tmpstr1, L1, 1) <> MID$(tmpstr2, L1, 1) THEN
                    tmpint1 = L1 - 1
                    EXIT FOR
                END IF
            NEXT
            tmpstr1 = LEFT$(tmpstr1, tmpint1)
        NEXT

        IF RIGHT$(tmpstr1, 1) <> pathSep THEN
            FOR L1 = tmpint1 TO 2 STEP -1
                IF (pathSep) = MID$(tmpstr1, L1, 1) THEN
                    tmpstr1 = LEFT$(tmpstr1, L1 - 1)
                    EXIT FOR
                END IF
            NEXT
            IF LEN(tmpstr1) = tmpint1 THEN tmpstr1 = ""
        ELSEIF tmpint1 > 1 THEN
            tmpstr1 = LEFT$(tmpstr1, tmpint1 - 1)
        END IF

        commonPath$ = tmpstr1
    END IF
END FUNCTION
```



## Batch File


```dos

@echo off
setlocal enabledelayedexpansion

call:commonpath /home/user1/tmp/coverage/test /home/user1/tmp/covert/operator /home/user1/tmp/coven/members
pause>nul
exit /b

:commonpath
setlocal enabledelayedexpansion

for %%i in (%*) do (
  set /a args+=1
  set arg!args!=%%i
  set fullarg!args!=%%i
)
for /l %%i in (1,1,%args%) do set fullarg%%i=!fullarg%%i:/= !

for /l %%i in (1,1,%args%) do (
  set tempcount=0
  for %%j in (!fullarg%%i!) do (
    set /a tempcount+=1
    set arg%%it!tempcount!=%%j
    set arg%%itokencount=!tempcount!
  )
)

set mintokencount=%arg1tokencount%
set leasttokens=1
for /l %%i in (1,1,%args%) do (
  set currenttokencount=!arg%%itokencount!
  if !currenttokencount! lss !mintokencount! (
    set mintokencount=!currenttokencount!
    set leasttokens=%%i
  )
)

for /l %%i in (1,1,%mintokencount%) do set commonpath%%i=!arg%leasttokens%t%%i!

for /l %%i in (1,1,%mintokencount%) do (
  for /l %%j in (1,1,%args%) do (
    set currentpath=!arg%%jt%%i!
    if !currentpath!==!commonpath%%i! set pathtokens%%j=%%i
  )
)

set minpathtokens=%pathtokens1%
set leastpathtokens=1
for /l %%i in (1,1,%args%) do (
  set currentpathtokencount=!pathtokens%%i!
  if !currentpathtokencount! lss !minpathtokens! (
    set minpathtokencount=!currentpathtokencount!
    set leastpathtokens=%%i
  )
)

set commonpath=/
for /l %%i in (1,1,!pathtokens%leastpathtokens%!) do set commonpath=!commonpath!!arg%leastpathtokens%t%%i!/
echo %commonpath%

endlocal
exit /b

```

{{out}}

```txt

/home/user1/tmp/

```




## BBC BASIC


```bbcbasic
      DIM path$(3)
      
      path$(1) = "/home/user1/tmp/coverage/test"
      path$(2) = "/home/user1/tmp/covert/operator"
      path$(3) = "/home/user1/tmp/coven/members"
      
      PRINT FNcommonpath(path$(), "/")
      END
      
      DEF FNcommonpath(p$(), s$)
      LOCAL I%, J%, O%
      REPEAT
        O% = I%
        I% = INSTR(p$(1), s$, I%+1)
        FOR J% = 2 TO DIM(p$(), 1)
          IF LEFT$(p$(1), I%) <> LEFT$(p$(J%), I%) EXIT REPEAT
        NEXT J%
      UNTIL I% = 0
      = LEFT$(p$(1), O%-1)
```



## C


```C>#include <stdio.h


int common_len(const char *const *names, int n, char sep)
{
	int i, pos;
	for (pos = 0; ; pos++) {
		for (i = 0; i < n; i++) {
			if (names[i][pos] != '\0' &&
					names[i][pos] == names[0][pos])
				continue;

			/* backtrack */
			while (pos > 0 && names[0][--pos] != sep);
			return pos;
		}
	}

	return 0;
}

int main()
{
	const char *names[] = {
		"/home/user1/tmp/coverage/test",
		"/home/user1/tmp/covert/operator",
		"/home/user1/tmp/coven/members",
	};
	int len = common_len(names, sizeof(names) / sizeof(const char*), '/');

	if (!len) printf("No common path\n");
	else      printf("Common path: %.*s\n", len, names[0]);

	return 0;
}
```
output:<lang>Common path: /home/user1/tmp
```



## C++


```cpp>#include <algorithm

#include <iostream>
#include <string>
#include <vector>

std::string longestPath( const std::vector<std::string> & , char ) ;

int main( ) {
   std::string dirs[ ] = {
      "/home/user1/tmp/coverage/test" ,
      "/home/user1/tmp/covert/operator" ,
      "/home/user1/tmp/coven/members" } ;
   std::vector<std::string> myDirs ( dirs , dirs + 3 ) ;
   std::cout << "The longest common path of the given directories is "
             << longestPath( myDirs , '/' ) << "!\n" ;
   return 0 ;
}

std::string longestPath( const std::vector<std::string> & dirs , char separator ) {
   std::vector<std::string>::const_iterator vsi = dirs.begin( ) ;
   int maxCharactersCommon = vsi->length( ) ;
   std::string compareString = *vsi ;
   for ( vsi = dirs.begin( ) + 1 ; vsi != dirs.end( ) ; vsi++ ) {
      std::pair<std::string::const_iterator , std::string::const_iterator> p = 
	 std::mismatch( compareString.begin( ) , compareString.end( ) , vsi->begin( ) ) ;
      if (( p.first - compareString.begin( ) ) < maxCharactersCommon ) 
	 maxCharactersCommon = p.first - compareString.begin( ) ;
   }
   std::string::size_type found = compareString.rfind( separator , maxCharactersCommon ) ;
   return compareString.substr( 0 , found ) ;
}
```

Output:

```txt

The longest common path of the given directories is /home/user1/tmp!

```



## Clojure


```clojure
(use '[clojure.string :only [join,split]]) 

(defn common-prefix [sep paths]
  (let [parts-per-path (map #(split % (re-pattern sep)) paths)
        parts-per-position (apply map vector parts-per-path)]
    (join sep
      (for [parts parts-per-position :while (apply = parts)]
        (first parts)))))

(println
  (common-prefix "/"
    ["/home/user1/tmp/coverage/test"
     "/home/user1/tmp/covert/operator"
     "/home/user1/tmp/coven/members"]))
```


=={{header|C sharp|C#}}==


```csharp

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace RosettaCodeTasks
{

	class Program
	{
		static void Main ( string[ ] args )
		{
			FindCommonDirectoryPath.Test ( );
		}

	}

	class FindCommonDirectoryPath
	{
		public static void Test ( )
		{
			Console.WriteLine ( "Find Common Directory Path" );
			Console.WriteLine ( );
			List<string> PathSet1 = new List<string> ( );
			PathSet1.Add ( "/home/user1/tmp/coverage/test" );
			PathSet1.Add ( "/home/user1/tmp/covert/operator" );
			PathSet1.Add ( "/home/user1/tmp/coven/members" );
			Console.WriteLine("Path Set 1 (All Absolute Paths):");
			foreach ( string path in PathSet1 )
			{
				Console.WriteLine ( path );
			}
			Console.WriteLine ( "Path Set 1 Common Path: {0}", FindCommonPath ( "/", PathSet1 ) );
		}
		public static string FindCommonPath ( string Separator, List<string> Paths )
		{
			string CommonPath = String.Empty;
			List<string> SeparatedPath = Paths
				.First ( str => str.Length == Paths.Max ( st2 => st2.Length ) )
				.Split ( new string[ ] { Separator }, StringSplitOptions.RemoveEmptyEntries )
				.ToList ( );

			foreach ( string PathSegment in SeparatedPath.AsEnumerable ( ) )
			{
				if ( CommonPath.Length == 0 && Paths.All ( str => str.StartsWith ( PathSegment ) ) )
				{
					CommonPath = PathSegment;
				}
				else if ( Paths.All ( str => str.StartsWith ( CommonPath + Separator + PathSegment ) ) )
				{
					CommonPath += Separator + PathSegment;
				}
				else
				{
					break;
				}
			}
			
			return CommonPath;
		}
	}
}


```



## D

This code uses the std.algorithm.commonPrefix function that finds the common prefix of two ranges.

```d
import std.stdio, std.string, std.algorithm, std.path, std.array;

string commonDirPath(in string[] paths, in string sep = "/") pure {
    if (paths.empty)
        return null;
    return paths.map!(p => p.split(sep)).reduce!commonPrefix.join(sep);
}

void main() {
    immutable paths = ["/home/user1/tmp/coverage/test",
                       "/home/user1/tmp/covert/operator",
                       "/home/user1/tmp/coven/members"];
    writeln(`The common path is: "`, paths.commonDirPath, '"');
}
```

{{out}}

```txt
The common path is: "/home/user1/tmp"
```



## Elixir

{{trans|Ruby}}

```elixir
defmodule RC do
  def common_directory_path(dirs, separator \\ "/") do
    dir1 = Enum.min(dirs) |> String.split(separator)
    dir2 = Enum.max(dirs) |> String.split(separator)
    Enum.zip(dir1,dir2) |> Enum.take_while(fn {a,b} -> a==b end)
                        |> Enum.map_join(separator, fn {a,a} -> a end)
  end
end

dirs = ~w( /home/user1/tmp/coverage/test /home/user1/tmp/covert/operator /home/user1/tmp/coven/members )
IO.inspect RC.common_directory_path(dirs)
```


{{out}}

```txt

"/home/user1/tmp"

```



## Erlang


```Erlang

-module( find_common_directory ).

-export( [path/2, task/0] ).

path( [Path | T], _Separator ) -> filename:join( lists:foldl(fun keep_common/2, filename:split(Path), [filename:split(X) || X <- T]) ).

task() -> path( ["/home/user1/tmp/coverage/test", "/home/user1/tmp/covert/operator", "/home/user1/tmp/coven/members"], "/" ).



keep_common( Components, Acc ) -> [X || X <- Components, Y <- Acc, X =:= Y].

```

{{out}}

```txt

78> find_common_directory:task().
"/home/user1/tmp"

```


=={{header|F_Sharp|F#}}==

```fsharp
open System

let (|SeqNode|SeqEmpty|) s =
    if Seq.isEmpty s then SeqEmpty
    else SeqNode ((Seq.head s), Seq.skip 1 s)

[<EntryPoint>]
let main args =
    let splitBySeparator (str : string) = Seq.ofArray (str.Split('/'))

    let rec common2 acc = function
        | SeqEmpty -> Seq.ofList (List.rev acc)
        | SeqNode((p1, p2), rest) ->
            if p1 = p2 then common2 (p1::acc) rest
            else Seq.ofList (List.rev acc)

    let commonPrefix paths =
        match Array.length(paths) with
        | 0 -> [||]
        | 1 -> Seq.toArray (splitBySeparator paths.[0])
        | _ ->
            let argseq = Seq.ofArray paths
            Seq.fold (
                fun (acc : seq<string>) items ->
                    common2 [] (List.ofSeq (Seq.zip acc (splitBySeparator items)))
            ) (splitBySeparator (Seq.head argseq)) (Seq.skip 1 argseq)
            |> Seq.toArray

    printfn "The common preffix is: %A" (String.Join("/", (commonPrefix args)))
    0
```

Output for the given task input

```txt
The common preffix is: "/home/user1/tmp"
```



## Factor


```factor
: take-shorter ( seq1 seq2 -- shorter )
    [ shorter? ] 2keep ? ;

: common-head ( seq1 seq2 -- head )
    2dup mismatch [ nip head ] [ take-shorter ] if* ;

: common-prefix-1 ( file1 file2 separator -- prefix )
    [ common-head ] dip '[ _ = not ] trim-tail ;

: common-prefix ( seq separator -- prefix )
    [ ] swap '[ _ common-prefix-1 ] map-reduce ;
```


 ( scratchpad ) {
                    "/home/user1/tmp/coverage/test"
                    "/home/user1/tmp/covert/operator"
                    "/home/user1/tmp/coven/members" 
                } CHAR: / common-prefix .
 "/home/user1/tmp/"


## FreeBASIC

{{Trans|Visual Basic}}

```freebasic

' compile: fbc.exe -s console cdp.bas

Function CommonDirectoryPath Cdecl(count As Integer, ...) As String
Dim As String Path(), s
Dim As Integer i, j, k = 1
Dim arg As Any Ptr
Const PATH_SEPARATOR As String = "/"
 
  arg = va_first()
  ReDim Preserve Path(1 To count)
  For i = 1 To count
    Path(i) = *Va_Arg(arg, ZString Ptr)
	Print Path(i)
    arg = va_next(arg, ZString Ptr)
  Next i
  
  Do
    For i = 1 To count
      If i > 1 Then
        If InStr(k, Path(i), PATH_SEPARATOR) <> j Then
          Exit Do
        ElseIf Left(Path(i), j) <> Left(Path(1), j) Then
          Exit Do
        End If
      Else
        j = InStr(k, Path(i), PATH_SEPARATOR)
        If j = 0 Then
          Exit Do
        End If
      End If
    Next i
    s = Left(Path(1), j + CLng(k <> 1))
    k = j + 1
  Loop
  Return s
  
End Function


' testing the above function

Print CommonDirectoryPath( 3, _
  "/home/user1/tmp/coverage/test", _
  "/home/user1/tmp/covert/operator", _
  "/home/user1/tmp/coven/members") & " <- common"
Print  
 
Print CommonDirectoryPath( 4, _
  "/home/user1/tmp/coverage/test", _
  "/home/user1/tmp/covert/operator", _
  "/home/user1/tmp/coven/members", _
  "/home/user1/abc/coven/members") & " <- common"
Print    

Print CommonDirectoryPath( 3, _
  "/home/user1/tmp/coverage/test", _
  "/hope/user1/tmp/covert/operator", _
  "/home/user1/tmp/coven/members") & " <- common"
Print    

' empty keyboard buffer
While Inkey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
/home/user1/tmp/coverage/test
/home/user1/tmp/covert/operator
/home/user1/tmp/coven/members
/home/user1/tmp <- common

/home/user1/tmp/coverage/test
/home/user1/tmp/covert/operator
/home/user1/tmp/coven/members
/home/user1/abc/coven/members
/home/user1 <- common

/home/user1/tmp/coverage/test
/hope/user1/tmp/covert/operator
/home/user1/tmp/coven/members
/ <- common
```



## Gambas


```gambas
Public Sub Main()
Dim sFolder As String[] = ["/home/user1/tmp/coverage/test", "/home/user1/tmp/covert/operator", "/home/user1/tmp/coven/members"]
Dim sSame As String
Dim siCount As Short = 1

Do
  If Mid(sFolder[0], siCount, 1) = Mid(sFolder[1], siCount, 1) And Mid(sFolder[0], siCount, 1) = Mid(sFolder[2], siCount, 1) Then
    sSame &= Mid(sFolder[0], siCount, 1)
  Else
    Break
  End If
  Inc siCount
Loop

Print Mid(sSame, 1, RInStr(sSame, "/") - 1)

End
```

Output:

```txt

/home/user1/tmp

```



## Go


If the directory that is the common path is also in the list, then care must be taken to not truncate it (as some other solutions on this page do).
E.g. (<code>/home/user1, /home/user1/foo, /home/user1/bar</code>) should result in <code>/home/user1</code>, not <code>/home</code>.


```go
package main

import (
	"fmt"
	"os"
	"path"
)

func CommonPrefix(sep byte, paths ...string) string {
	// Handle special cases.
	switch len(paths) {
	case 0:
		return ""
	case 1:
		return path.Clean(paths[0])
	}

	// Note, we treat string as []byte, not []rune as is often
	// done in Go. (And sep as byte, not rune). This is because
	// most/all supported OS' treat paths as string of non-zero
	// bytes. A filename may be displayed as a sequence of Unicode
	// runes (typically encoded as UTF-8) but paths are
	// not required to be valid UTF-8 or in any normalized form
	// (e.g. "é" (U+00C9) and "é" (U+0065,U+0301) are different
	// file names.
	c := []byte(path.Clean(paths[0]))

	// We add a trailing sep to handle the case where the
	// common prefix directory is included in the path list
	// (e.g. /home/user1, /home/user1/foo, /home/user1/bar).
	// path.Clean will have cleaned off trailing / separators with
	// the exception of the root directory, "/" (in which case we
	// make it "//", but this will get fixed up to "/" bellow).
	c = append(c, sep)

	// Ignore the first path since it's already in c
	for _, v := range paths[1:] {
		// Clean up each path before testing it
		v = path.Clean(v) + string(sep)

		// Find the first non-common byte and truncate c
		if len(v) < len(c) {
			c = c[:len(v)]
		}
		for i := 0; i < len(c); i++ {
			if v[i] != c[i] {
				c = c[:i]
				break
			}
		}
	}

	// Remove trailing non-separator characters and the final separator
	for i := len(c) - 1; i >= 0; i-- {
		if c[i] == sep {
			c = c[:i]
			break
		}
	}

	return string(c)
}

func main() {
	c := CommonPrefix(os.PathSeparator,
		//"/home/user1/tmp",
		"/home/user1/tmp/coverage/test",
		"/home/user1/tmp/covert/operator",
		"/home/user1/tmp/coven/members",
		"/home//user1/tmp/coventry",
		"/home/user1/././tmp/covertly/foo",
		"/home/bob/../user1/tmp/coved/bar",
	)
	if c == "" {
		fmt.Println("No common path")
	} else {
		fmt.Println("Common path:", c)
	}
}
```



## Groovy

Solution:

```groovy
def commonPath = { delim, Object[] paths ->
    def pathParts = paths.collect { it.split(delim) }
    pathParts.transpose().inject([match:true, commonParts:[]]) { aggregator, part ->
        aggregator.match = aggregator.match && part.every { it == part [0] }
        if (aggregator.match) { aggregator.commonParts << part[0] }
        aggregator
    }.commonParts.join(delim)
}
```


Test:

```groovy
println commonPath('/',
    '/home/user1/tmp/coverage/test',
    '/home/user1/tmp/covert/operator',
    '/home/user1/tmp/coven/members')

println commonPath('/',
    '/home/user1/tmp/coverage/test',
    '/home/user1/tmp/covert/test',
    '/home/user1/tmp/coven/test',
    '/home/user1/tmp/covers/test')
```


Output:

```txt
/home/user1/tmp
/home/user1/tmp
```


=={{header|GW-BASIC}}==
{{works with|GW-BASIC}}
{{works with|Chipmunk Basic}}

Because most BASICs don't have any sort of parsing functions built in, we have to deal with the entire string (rather than checking one level at a time).

Note that if the root directory is the common path, this reports the same as no match found (i.e. blank result).


```qbasic
10 REM All GOTO statements can be replaced with EXIT FOR in newer BASICs.

110 X$ = "/home/user1/tmp/coverage/test"
120 Y$ = "/home/user1/tmp/covert/operator"
130 Z$ = "/home/user1/tmp/coven/members"

150 A = LEN(X$)
160 IF A > LEN(Y$) THEN A = LEN(Y$)
170 IF A > LEN(Z$) THEN A = LEN(Z$)
180 FOR L0 = 1 TO A
190     IF MID$(X$, L0, 1) <> MID$(Y$, L0, 1) THEN GOTO 210
200 NEXT
210 A = L0 - 1

230 FOR L0 = 1 TO A
240     IF MID$(X$, L0, 1) <> MID$(Z$, L0, 1) THEN GOTO 260
250 NEXT
260 A = L0 - 1

280 IF MID$(X$, L0, 1) <> "/" THEN
290     FOR L0 = A TO 1 STEP -1
300        IF "/" = MID$(X$, L0, 1) THEN GOTO 340
310     NEXT
320 END IF

340 REM Task description says no trailing slash, so...
350 A = L0 - 1
360 P$ = LEFT$(X$, A)
370 PRINT "Common path is '"; P$; "'"
```


Output:
 Common path is '/home/user1/tmp'


## Haskell



```haskell
import Data.List

-- Return the common prefix of two lists.
commonPrefix2 (x:xs) (y:ys) | x == y = x : commonPrefix2 xs ys
commonPrefix2 _ _ = []

-- Return the common prefix of zero or more lists.
commonPrefix (xs:xss) = foldr commonPrefix2 xs xss
commonPrefix _ = []

-- Split a string into path components.
splitPath = groupBy (\_ c -> c /= '/')

-- Return the common prefix of zero or more paths.
-- Note that '/' by itself is not considered a path component,
-- so "/" and "/foo" are treated as having nothing in common.
commonDirPath = concat . commonPrefix . map splitPath

main = putStrLn $ commonDirPath [
        "/home/user1/tmp/coverage/test",
        "/home/user1/tmp/covert/operator",
        "/home/user1/tmp/coven/members"
       ]
```


Or perhaps:

```haskell
import Data.List (transpose, intercalate)
import Data.List.Split (splitOn)

cdp :: [String] -> String
cdp xs
  | null xs = []
  | otherwise =
    (intercalate "/" .
     fmap head . takeWhile same . transpose . fmap (splitOn "/"))
      xs

same
  :: Eq a
  => [a] -> Bool
same [] = True
same (x:xs) = all (x ==) xs

main :: IO ()
main =
  (putStrLn . cdp)
    [ "/home/user1/tmp/coverage/test"
    , "/home/user1/tmp/covert/operator"
    , "/home/user1/tmp/coven/members"
    ]
```

{{Out}}

```txt
/home/user1/tmp
```



## HicEst


```HicEst
CHARACTER a='/home/user1/tmp/coverage/test', b='/home/user1/tmp/covert/operator', c='/home/user1/tmp/coven/members'

minLength = MIN( LEN(a), LEN(b), LEN(c) )
lastSlash = 0

DO i = 1, minLength
  IF( (a(i) == b(i)) * (b(i) == c(i)) ) THEN
    IF(a(i) == "/") lastSlash = i
  ELSEIF( lastSlash ) THEN
    WRITE(Messagebox) "Common Directory = ", a(1:lastSlash-1)
  ELSE
    WRITE(Messagebox, Name) "No common directory for", a, b, c
  ENDIF
ENDDO
```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
procedure main()
write(lcdsubstr(["/home/user1/tmp/coverage/test","/home/user1/tmp/covert/operator","/home/user1/tmp/coven/members"]))
end

procedure lcdsubstr(sL,d)   #: return the longest common sub-string of strings in the list sL delimited by d
local ss

/d := "/"
reverse(sL[1]) ? {
   if tab(find(d)+*d) || allmatch(ss := reverse(tab(0)),sL) then
      return ss
   }
end

procedure allmatch(s,L)   #: retrun s if it matches all strings in L
local x
every x := !L do
   if not match(s,x) then fail
return s
end
```



## J

'''Solution:'''

```j
parseDirs       =: = <;.2 ]
getCommonPrefix =: {. ;@{.~ 0 i.~ *./@(="1 {.)

getCommonDirPath=: [: getCommonPrefix parseDirs&>
```


'''Example:'''

```j
   paths=: '/home/user1/tmp/coverage/test';'/home/user1/tmp/covert/operator';'/home/user1/tmp/coven/members'
   getCommonPrefix >paths
/home/user1/tmp/cove
   '/' getCommonDirPath paths
/home/user1/tmp/
```


'''Note:'''
This alternative formulation of parseDirs provides cross-platform support, without the need to specify the path separator.

```j
parseDirs       =: (PATHSEP_j_&= <;.2 ])@jhostpath
```


## Java

{{works with|Java|1.5+}}
This example is case-sensitive.

```java5
public class CommonPath {
	public static String commonPath(String... paths){
		String commonPath = "";
		String[][] folders = new String[paths.length][];
		for(int i = 0; i < paths.length; i++){
			folders[i] = paths[i].split("/"); //split on file separator
		}
		for(int j = 0; j < folders[0].length; j++){
			String thisFolder = folders[0][j]; //grab the next folder name in the first path
			boolean allMatched = true; //assume all have matched in case there are no more paths
			for(int i = 1; i < folders.length && allMatched; i++){ //look at the other paths
				if(folders[i].length < j){ //if there is no folder here
					allMatched = false; //no match
					break; //stop looking because we've gone as far as we can
				}
				//otherwise
				allMatched &= folders[i][j].equals(thisFolder); //check if it matched
			}
			if(allMatched){ //if they all matched this folder name
				commonPath += thisFolder + "/"; //add it to the answer
			}else{//otherwise
				break;//stop looking
			}
		}
		return commonPath;
	}
	
	public static void main(String[] args){
		String[] paths = { "/home/user1/tmp/coverage/test",
				 "/home/user1/tmp/covert/operator",
				 "/home/user1/tmp/coven/members"};
		System.out.println(commonPath(paths));
		
		String[] paths2 = { "/hame/user1/tmp/coverage/test",
				 "/home/user1/tmp/covert/operator",
				 "/home/user1/tmp/coven/members"};
		System.out.println(commonPath(paths2));
	}
}
```

Output:

```txt
/home/user1/tmp/
/
```

Change <code>folders[i] = paths[i].split("/");</code> to add more separators. Ex: to add "\" and "." as separators, change the line to <code>folders[i] = paths[i].split("[/\\\\.]");</code> (adding square brackets makes the regex choose one character out of the group inside, adding all of the extra backslashes escapes the backslash character). After making this change, all separators will be changed to "/" in the result, but they can be mixed in the path (e.g. "/home.user1/tmp\\coverage/test" (escaped backslash) will act the same as "/home/user1/tmp/coverage/test").

A slightly modified version of the previous program, only the method commonPath() is changed.

```java5

	static String commonPath(String...  paths){
		String commonPath = "";
		String[][] folders = new String[paths.length][];
		
		for(int i=0; i<paths.length; i++){
			folders[i] = paths[i].split("/");
		}
			
		for(int j = 0; j< folders[0].length; j++){
			String s = folders[0][j];
			for(int i=1; i<paths.length; i++){
				if(!s.equals(folders[i][j]))
					return commonPath;
			}
			commonPath += s + "/";
		}
		return commonPath;		
	}

```



## JavaScript


```javascript

/**
 * Given an array of strings, return an array of arrays, containing the
 * strings split at the given separator
 * @param {!Array<!string>} a
 * @param {string} sep
 * @returns {!Array<!Array<string>>}
 */
const splitStrings = (a, sep = '/') => a.map(i => i.split(sep));

/**
 * Given an index number, return a function that takes an array and returns the
 * element at the given index
 * @param {number} i
 * @return {function(!Array<*>): *}
 */
const elAt = i => a => a[i];

/**
 * Transpose an array of arrays:
 * Example:
 * [['a', 'b', 'c'], ['A', 'B', 'C'], [1, 2, 3]] ->
 * [['a', 'A', 1], ['b', 'B', 2], ['c', 'C', 3]]
 * @param {!Array<!Array<*>>} a
 * @return {!Array<!Array<*>>}
 */
const rotate = a => a[0].map((e, i) => a.map(elAt(i)));

/**
 * Checks of all the elements in the array are the same.
 * @param {!Array<*>} arr
 * @return {boolean}
 */
const allElementsEqual = arr => arr.every(e => e === arr[0]);


const commonPath = (input, sep = '/') => rotate(splitStrings(input, sep))
    .filter(allElementsEqual).map(elAt(0)).join(sep);

const cdpInput = [
  '/home/user1/tmp/coverage/test',
  '/home/user1/tmp/covert/operator',
  '/home/user1/tmp/coven/members',
];

console.log(`Common path is: ${commonPath(cdpInput)}`);

```


{{out}}

```txt

Common path is: /home/user1/tmp

```




## jq


```jq
# maximal_initial_subarray takes as input an array of arrays:
def maximal_initial_subarray:
  (map( .[0] ) | unique) as $u
  | if $u == [ null ] then []
    elif ($u|length) == 1
    then  $u  + ( map( .[1:] ) | maximal_initial_subarray)
    else []
    end ;

# Solution: read in the strings, convert to an array of arrays, and proceed:
def common_path(slash):
  [.[] | split(slash)] | maximal_initial_subarray | join(slash) ;

common_path("/")
```


Assuming the above jq program is in a file named common_path.jq and that the file directories.txt contains the three given directory strings quoted with double quotation marks:

```jq
$ jq -s -f common_path.jq directories.txt
"home/user1/tmp"
```



## Julia

{{works with|Julia|0.6}}


```julia
function commonpath(ds::Vector{<:AbstractString}, dlm::Char='/')
    0 < length(ds) || return ""
    1 < length(ds) || return String(ds[1])
    p = split(ds[1], dlm)
    mincnt = length(p)
    for d in ds[2:end]
        q = split(d, dlm)
        mincnt = min(mincnt, length(q))
        hits = findfirst(p[1:mincnt] .!= q[1:mincnt])
        if hits != 0 mincnt = hits - 1 end
        if mincnt == 0 return "" end
    end
    1 < mincnt || p[1] != "" || return convert(T, string(dlm))
    return join(p[1:mincnt], dlm)
end

test = ["/home/user1/tmp/coverage/test", "/home/user1/tmp/covert/operator", "/home/user1/tmp/coven/members"]

println("Comparing:\n - ", join(test, "\n - "))
println("for their common directory path yields:\n", commonpath(test))
```


{{out}}

```txt
Comparing:
 - /home/user1/tmp/coverage/test
 - /home/user1/tmp/covert/operator
 - /home/user1/tmp/coven/members
for their common directory path yields:
/home/user1/tmp
```



## Kotlin


```scala
// version 1.1.51

fun findCommonDirPath(paths: List<String>, separator: Char): String {
    if (paths.isEmpty()) return ""
    if (paths.size == 1) return paths[0]
    val splits = paths[0].split(separator)
    val n = splits.size
    val paths2 = paths.drop(1)
    var k = 0
    var common = ""
    while (true) {
        val prevCommon = common
        common += if (k == 0) splits[0] else separator + splits[k]
        if (!paths2.all { it.startsWith(common + separator) || it == common } ) return prevCommon
        if (++k == n) return common
    }
}

fun main(args: Array<String>) {
    val paths = listOf(
        "/home/user1/tmp/coverage/test",
        "/home/user1/tmp/covert/operator",
        "/home/user1/tmp/coven/members"
    )
    val pathsToPrint = paths.map { "   '$it'" }.joinToString("\n")
    println("The common directory path of:\n\n$pathsToPrint\n")
    println("is '${findCommonDirPath(paths, '/')}'")
}
```


{{out}}

```txt

The common directory path of:

   '/home/user1/tmp/coverage/test'
   '/home/user1/tmp/covert/operator'
   '/home/user1/tmp/coven/members'

is '/home/user1/tmp'

```



## Lasso


```Lasso
#!/usr/bin/lasso9

local(
	path1 = '/home/user1/tmp/coverage/test' -> split('/'),
	path2 = '/home/user1/tmp/covert/operator' -> split('/'),
	path3 = '/home/user1/tmp/coven/members' -> split('/')
)

define commonpath(...) => {
	local(shared = #rest -> get(1))
	loop(#rest -> size - 1) => {
		#shared = #shared -> intersection(#rest -> get(loop_count + 1))
	}
	return #shared -> join('/')
}

stdoutnl(commonpath(#path1, #path2, #path3))
```


Output:

```txt
/home/user1/tmp
```



## Liberty BASIC


```lb
path$(1) = "/home/user1/tmp/coverage/test"
path$(2) = "/home/user1/tmp/covert/operator"
path$(3) = "/home/user1/tmp/coven/members"


print samepath$(3,"/")
end

function samepath$(paths,sep$)
    d = 1 'directory depth
    n = 2 'path$(number)
    while 1
        if word$(path$(1),d,sep$) <> word$(path$(n),d,sep$) or word$(path$(1),d,sep$) = "" then exit while
        n = n + 1
        if n > paths then
            if right$(samepath$,1) <> sep$ and d<>1 then
                samepath$ = samepath$ + sep$ + word$(path$(1),d,sep$)
            else
                samepath$ = samepath$ + word$(path$(1),d,sep$)
            end if
            n = 2
            d = d + 1
        end if
    wend
end function
```



## Lingo


```lingo
on getCommonPath (pathes, sep)
  _player.itemDelimiter = sep

  -- find length of shortest path (in terms of items)
  commonCnt = the maxInteger
  repeat with p in pathes
    if p.item.count<commonCnt then commonCnt=p.item.count
  end repeat

  pathCnt = pathes.count
  repeat with i = 1 to commonCnt
    repeat with j = 2 to pathCnt
      if pathes[j].item[i]<>pathes[j-1].item[i] then
        return pathes[1].item[1..i-1]
      end if
    end repeat
  end repeat
  return pathes[1].item[1..commonCnt]
end
```


```lingo
pathes = []
pathes.add("/home/user1/tmp/coverage/test")
pathes.add("/home/user1/tmp/covert/operator")
pathes.add("/home/user1/tmp/coven/members")

put getCommonPath(pathes, "/")
-- "/home/user1/tmp"
```



## MapBasic


Derived from the [https://www.rosettacode.org/wiki/Find_common_directory_path#BASIC BASIC] example above


```qbasic
Include "MapBasic.def"

Declare Sub Main
DECLARE FUNCTION commonPath(paths() AS STRING, ByVal pathSep AS STRING) as String

FUNCTION commonPath(paths() AS STRING, ByVal pathSep AS STRING) as String
    DIM tmpint1 AS INTEGER, tmpint2 AS INTEGER, tmpstr1 AS STRING, tmpstr2 AS STRING
    DIM L0 AS INTEGER, L1 AS INTEGER, lowerbound AS INTEGER, upperbound AS INTEGER
    
    lowerbound = 1
    upperbound = UBOUND(paths)
 
    IF (lowerbound) = upperbound THEN       'Some quick error checking...
        commonPath = paths(lowerbound)
    ELSEIF lowerbound > upperbound THEN     'How in the...?
        commonPath = ""
    ELSE
        tmpstr1 = paths(lowerbound)
 
        FOR L0 = (lowerbound) TO upperbound 'Find common strings.
            tmpstr2 = paths(L0)
            tmpint1 = LEN(tmpstr1)
            tmpint2 = LEN(tmpstr2)
            IF tmpint1 > tmpint2 
            THEN tmpint1 = tmpint2

            FOR L1 = 1 TO tmpint1
                IF MID$(tmpstr1, L1, 1) <> MID$(tmpstr2, L1, 1) THEN
                    tmpint1 = L1 - 1
                    EXIT FOR
                END IF
            NEXT
                      end If 
           tmpstr1 = LEFT$(tmpstr1, tmpint1)
        NEXT
 
        IF RIGHT$(tmpstr1, 1) <> pathSep THEN
            FOR L1 = tmpint1 TO 2 STEP -1
                IF (pathSep) = MID$(tmpstr1, L1, 1) THEN
                    tmpstr1 = LEFT$(tmpstr1, L1 - 1)
                    EXIT FOR
                END IF
            NEXT  
            IF LEN(tmpstr1) = tmpint1 
            THEN tmpstr1 = ""
       		 ELSEIF tmpint1 > 1 
       		 THEN
            tmpstr1 = LEFT$(tmpstr1, tmpint1 - 1)
        	END IF
 		 end if  
        commonPath = tmpstr1
    END IF
END FUNCTION

Sub Main()
	Dim x(3) as  String
	Define Sep "/"
	
	x(1) = "/home/user1/tmp/"
	x(2) = "/home/user1/tmp/covert/operator"
	x(3) = "/home/user1/tmp/coven/members"
 

PRINT "Common path is " + commonPath(x(), Sep)

End Sub
```



## Maple


```Maple

dirpath:=proc(a,b,c)
	local dirtemp,dirnew,x;
	use StringTools in
		dirtemp:=LongestCommonSubString(c, LongestCommonSubString(a,b));
		x:=FirstFromRight("/",dirtemp);
		dirnew:=dirtemp[1..x];
	return dirnew;
	end use;
end proc;

```



## Mathematica


```Mathematica
FindCommonDirectory[x_] := If[StringTake[#, -1] != "/", StringTake[#, Max[StringPosition[#, "/"]]], #] &
 [Fold[LongestCommonSubsequence, First[x] , Rest[x]]]

FindCommonDirectory[{"/home/user1/tmp/coverage/test", "/home/user1/tmp/covert/operator", "/home/user1/tmp/coven/members"}]
->"/home/user1/tmp/"
```



## Maxima


```maxima
scommon(a, b) := block([n: min(slength(a), slength(b))],
   substring(a, 1, catch(for i thru n do (
   if not cequal(charat(a, i), charat(b, i)) then throw(i)), n + 1)))$

commonpath(u, [l]) := block([s: lreduce(scommon, u), c, n],
   n: sposition(if length(l) = 0 then "/" else l[1], sreverse(s)),
   if integerp(n) then substring(s, 1, slength(s) - n) else ""
)$

commonpath(["c:/files/banister.jpg", "c:/files/bank.xls", "c:/files/banana-recipes.txt"]);
"c:/files"
```



## MUMPS


```MUMPS
FCD
 NEW D,SEP,EQ,LONG,DONE,I,J,K,RETURN
 SET D(1)="/home/user1/tmp/coverage/test"
 SET D(2)="/home/user1/tmp/covert/operator"
 SET D(3)="/home/user1/tmp/coven/members"
 SET SEP="/"
 SET LONG=D(1)
 SET DONE=0
 FOR I=1:1:$LENGTH(LONG,SEP) QUIT:DONE  SET EQ(I)=1 FOR J=2:1:3 SET EQ(I)=($PIECE(D(J),SEP,I)=$PIECE(LONG,SEP,I))&EQ(I) SET DONE='EQ(I) QUIT:'EQ(I)
 SET RETURN=""
 FOR K=1:1:I-1 Q:'EQ(K)  SET:EQ(K) $PIECE(RETURN,SEP,K)=$PIECE(LONG,SEP,K)
 WRITE !,"For the paths:" FOR I=1:1:3 WRITE !,D(I)
 WRITE !,"The longest common directory is: ",RETURN
 KILL D,SEP,EQ,LONG,DONE,I,J,K,RETURN
 QUIT
```

Usage:
```txt

USER>D FCD^ROSETTA
 
For the paths:
/home/user1/tmp/coverage/test
/home/user1/tmp/covert/operator
/home/user1/tmp/coven/members
The longest common directory is: /home/user1/tmp
```



## Nim


```nim
import strutils

proc commonprefix(paths: openarray[string], sep = "/"): string =
  if paths.len == 0: return ""
  block outer:
    for i in 0..paths[0].len:
      result = paths[0][0..i]
      for path in paths:
        if not path.startsWith(result):
          break outer
  result = result[0 .. <result.rfind(sep)]

echo commonprefix(@["/home/user1/tmp/coverage/test", "/home/user1/tmp/covert/operator", "/home/user1/tmp/coven/members"])
```

Output:

```txt
/home/user1/tmp
```



## OCaml



```ocaml
let rec aux acc paths =
  if List.mem [] paths
  then (List.rev acc) else
  let heads = List.map List.hd paths in
  let item = List.hd heads in
  let all_the_same =
    List.for_all ((=) item) (List.tl heads)
  in
  if all_the_same
  then aux (item::acc) (List.map List.tl paths)
  else (List.rev acc)

let common_prefix sep = function
  | [] -> invalid_arg "common_prefix"
  | dirs ->
      let paths = List.map (Str.split (Str.regexp_string sep)) dirs in
      let res = aux [] paths in
      (sep ^ (String.concat sep res))

let () =
  let dirs = [
    "/home/user1/tmp/coverage/test";
    "/home/user1/tmp/covert/operator";
    "/home/user1/tmp/coven/members";
  ] in
  print_endline (common_prefix "/" dirs);
;;
```


(uses the module <code>[http://caml.inria.fr/pub/docs/manual-ocaml/libref/Str.html Str]</code>, str.cma)


## OpenEdge/Progress


```progress
FUNCTION findCommonDir RETURNS CHAR(
   i_cdirs        AS CHAR,
   i_cseparator   AS CHAR
):

   DEF VAR idir      AS INT.
   DEF VAR idepth    AS INT. 
   DEF VAR cdir      AS CHAR EXTENT.
   DEF VAR lsame     AS LOGICAL INITIAL TRUE.
   DEF VAR cresult   AS CHAR.

   EXTENT( cdir ) = NUM-ENTRIES( i_cdirs, '~n' ).

   DO idir = 1 TO NUM-ENTRIES( i_cdirs, '~n' ):
      cdir[ idir ] = ENTRY( idir, i_cdirs, '~n' ).
   END.

   DO idepth = 2 TO NUM-ENTRIES( cdir [ 1 ], i_cseparator ) WHILE lsame:
      DO idir = 1 TO EXTENT( cdir ) - 1 WHILE lsame:
         lsame =  ( 
                     ENTRY( idepth, cdir [ idir ], i_cseparator ) = 
                     ENTRY( idepth, cdir [ idir + 1 ], i_cseparator )
                  ).
      END.
      IF lsame THEN
         cresult = cresult + i_cseparator + ENTRY( idepth, cdir [ 1 ], i_cseparator ).
   END.

   RETURN cresult.

END FUNCTION.
```



```progress
MESSAGE 
   findCommonDir(
      '/home/user1/tmp/coverage/test' + '~n' +
      '/home/user1/tmp/covert/operator' + '~n' +
      '/home/user1/tmp/coven/members',
      '/'
   )
VIEW-AS ALERT-BOX
```


Output


```txt
---------------------------
Message (Press HELP to view stack trace)
---------------------------
/home/user1/tmp
---------------------------
OK   Help   
---------------------------
```



## Oz

With a few helper functions, we can express the solution like this in Oz:

```oz
declare
  fun {CommonPrefix Sep Paths}
     fun {GetParts P} {String.tokens P Sep} end
     Parts = {ZipN {Map Paths GetParts}}
     EqualParts = {List.takeWhile Parts fun {$ X|Xr} {All Xr {Equals X}} end}
  in
     {Join Sep {Map EqualParts Head}}
  end

  fun {ZipN Xs}
     if {Some Xs {Equals nil}} then nil
     else
        {Map Xs Head} | {ZipN {Map Xs Tail}}
     end
  end

  fun {Join Sep Xs}
     {FoldR Xs fun {$ X Z} {Append X Sep|Z} end nil}
  end

  fun {Equals C}
     fun {$ X} X == C end
  end

  fun {Head X|_} X end

  fun {Tail _|Xr} Xr end
in
  {System.showInfo {CommonPrefix &/
                    ["/home/user1/tmp/coverage/test"
                     "/home/user1/tmp/covert/operator"
                     "/home/user1/tmp/coven/members"]}}
```



## PARI/GP


```parigp
cdp(v)={
  my(s="");
  v=apply(t->Vec(t),v);
  for(i=1,vecmin(apply(length,v)),
    for(j=2,#v,
      if(v[j][i]!=v[1][i],return(s)));
      if(i>1&v[1][i]=="/",s=concat(vecextract(v[1],1<<(i-1)-1))
    )
  );
  if(vecmax(apply(length,v))==vecmin(apply(length,v)),concat(v[1]),s)
};
cdp(["/home/user1/tmp/coverage/test","/home/user1/tmp/covert/operator","/home/user1/tmp/coven/members"])
```



## Perl


A solution which lets the regex engine do all the work ''(it operates on the concatenation of the given paths delimited by null-bytes, which should be safe since null-bytes are not allowed inside paths)'':


```Perl
sub common_prefix {
    my $sep = shift;
    my $paths = join "\0", map { $_.$sep } @_;
    $paths =~ /^ ( [^\0]* ) $sep [^\0]* (?: \0 \1 $sep [^\0]* )* $/x;
    return $1;
}
```


A more conventional solution, which tallies up all potential prefixes from the given paths and then looks for the longest one that occurred the same number of times as there are paths:


```Perl
use List::Util qw(first);

sub common_prefix {
    my ($sep, @paths) = @_;
    my %prefixes;
    
    for (@paths) {
        do { ++$prefixes{$_} } while s/$sep [^$sep]* $//x
    }
    
    return first { $prefixes{$_} == @paths } reverse sort keys %prefixes;
}
```


'''Testing:'''


```perl
my @paths = qw(/home/user1/tmp/coverage/test 
               /home/user1/tmp/covert/operator
               /home/user1/tmp/coven/members);
print common_prefix('/', @paths), "\n";
```


{{out}}

```txt
/home/user1/tmp
```



## Perl 6


```Perl 6
my $sep = '/';
my @dirs = </home/user1/tmp/coverage/test
            /home/user1/tmp/covert/operator
            /home/user1/tmp/coven/members>;

my @comps = @dirs.map: { [ .comb(/ $sep [ <!before $sep> . ]* /) ] }; 

my $prefix = '';

while all(@comps[*]»[0]) eq @comps[0][0] {
    $prefix ~= @comps[0][0] // last;
    @comps».shift;
}

say "The longest common path is $prefix";

```

Output:

```txt

The longest common path is /home/user1/tmp

```

If you'd prefer a pure FP solution without side effects, you can use this:

```perl6
my $sep := '/';
my @dirs := </home/user1/tmp/coverage/test
             /home/user1/tmp/covert/operator
             /home/user1/tmp/coven/members>;

my @comps = @dirs.map: { [ .comb(/ $sep [ <!before $sep> . ]* /) ] };

say "The longest common path is ",
    gather for 0..* -> $column {
        last unless all(@comps[*]»[$column]) eq @comps[0][$column];
        take @comps[0][$column] // last;
    }
```

Or here's another factoring, that focuses on building the result with cumulative sequences and getting the solution with `first`:

```perl6
my $sep = '/';
my @dirs = </home/user1/tmp/coverage/test
            /home/user1/tmp/covert/operator
            /home/user1/tmp/coven/members>;

sub is_common_prefix { so $^prefix eq all(@dirs).substr(0, $prefix.chars) }

say ([\~] @dirs.comb(/ $sep [ <!before $sep> . ]* /)).reverse.first: &is_common_prefix
```



## Phix

Note: if the testset contains /home/user1/tmp the result is /home/user1, with /home/user1/tmp/ instead of that, it is /home/user1/tmp

To change that behaviour, simply remove both the [1..-2]

For cross-platform operation, simply use the split_path and join_path builtins instead of split and join.

```Phix
function common_directory_path(sequence paths, integer sep='/')
sequence res = {}
    if length(paths) then
        res = split(paths[1],sep)[1..-2]
        for i=2 to length(paths) do
            sequence pi = split(paths[i],sep)[1..-2]
            for j=1 to length(res) do
                if j>length(pi) or res[j]!=pi[j] then
                    res = res[1..j-1]
                    exit
                end if
            end for
            if length(res)=0 then exit end if
        end for
    end if
    return join(res,sep)
end function

constant test = {"/home/user1/tmp/coverage/test",
                 "/home/user1/tmp/covert/operator",
                 "/home/user1/tmp/coven/members"}
?common_directory_path(test)
```

{{out}}

```txt

"home/user1/tmp"

```



## PHP


```php
<?php

/*
 This works with dirs and files in any number of combinations.
*/

function _commonPath($dirList)
{
	$arr = array();
	foreach($dirList as $i => $path)
	{
		$dirList[$i]	= explode('/', $path);
		unset($dirList[$i][0]);
		
		$arr[$i] = count($dirList[$i]);
	}
	
	$min = min($arr);
	
	for($i = 0; $i < count($dirList); $i++)
	{
		while(count($dirList[$i]) > $min)
		{
			array_pop($dirList[$i]);
		}
		
		$dirList[$i] = '/' . implode('/' , $dirList[$i]);
	}
	
	$dirList = array_unique($dirList);
	while(count($dirList) !== 1)
	{
		$dirList = array_map('dirname', $dirList);
		$dirList = array_unique($dirList);
	}
	reset($dirList);
	
	return current($dirList);
}

 /* TEST */

$dirs = array(
 '/home/user1/tmp/coverage/test',
 '/home/user1/tmp/covert/operator',
 '/home/user1/tmp/coven/members',
);


if('/home/user1/tmp' !== common_path($dirs))
{
  echo 'test fail';
} else {
  echo 'test success';
}

?>
```




```php
<?php

/* A more compact string-only version, which I assume would be much faster */
/* If you want the trailing /, return $common; */

function getCommonPath($paths) {
	$lastOffset = 1;
	$common = '/';
	while (($index = strpos($paths[0], '/', $lastOffset)) !== FALSE) {
		$dirLen = $index - $lastOffset + 1;	// include /
		$dir = substr($paths[0], $lastOffset, $dirLen);
		foreach ($paths as $path) {
			if (substr($path, $lastOffset, $dirLen) != $dir)
				return $common;
		}
		$common .= $dir;
		$lastOffset = $index + 1;
	}
	return substr($common, 0, -1);
}

?>
```



## PicoLisp


```PicoLisp
(de commonPath (Lst Chr)
   (glue Chr
      (make
         (apply find
            (mapcar '((L) (split (chop L) Chr)) Lst)
            '(@ (or (pass <>) (nil (link (next))))) ) ) ) )
```

Output:

```txt
(commonPath
   (quote
      "/home/user1/tmp/coverage/test"
      "/home/user1/tmp/covert/operator"
      "/home/user1/tmp/coven/members" )
   "/" )

-> "/home/user1/tmp"
```


## Pike


```Pike
array paths = ({ "/home/user1/tmp/coverage/test",
                 "/home/user1/tmp/covert/operator",
                 "/home/user1/tmp/coven/members" });

// append a / to each entry, so that a path like "/home/user1/tmp" will be recognized as a prefix
// without it the prefix would end up being "/home/user1/"
paths = paths[*]+"/";

string cp = String.common_prefix(paths);
cp = cp[..sizeof(cp)-search(reverse(cp), "/")-2];
Result: "/home/user1/tmp"
```



## PowerBASIC

{{Trans|Visual Basic}}

```powerbasic
#COMPILE EXE
#DIM ALL
#COMPILER PBCC 6
$PATH_SEPARATOR = "/"

FUNCTION CommonDirectoryPath(Paths() AS STRING) AS STRING
LOCAL s AS STRING
LOCAL i, j, k AS LONG
  k = 1
  DO
    FOR i = 0 TO UBOUND(Paths)
      IF i THEN
        IF INSTR(k, Paths(i), $PATH_SEPARATOR) <> j THEN
          EXIT DO
        ELSEIF LEFT$(Paths(i), j) <> LEFT$(Paths(0), j) THEN
          EXIT DO
        END IF
      ELSE
        j = INSTR(k, Paths(i), $PATH_SEPARATOR)
        IF j = 0 THEN
          EXIT DO
        END IF
      END IF
    NEXT i
    s = LEFT$(Paths(0), j + CLNG(k <> 1))
    k = j + 1
  LOOP
  FUNCTION = s

END FUNCTION


FUNCTION PBMAIN () AS LONG

' testing the above function

LOCAL s() AS STRING
LOCAL i AS LONG

REDIM s(0 TO 2)
ARRAY ASSIGN s() = "/home/user1/tmp/coverage/test", _
                   "/home/user1/tmp/covert/operator", _
                   "/home/user1/tmp/coven/members"
FOR i = 0 TO UBOUND(s()): CON.PRINT s(i): NEXT i
CON.PRINT CommonDirectoryPath(s()) & " <- common"
CON.PRINT

REDIM s(0 TO 3)
ARRAY ASSIGN s() = "/home/user1/tmp/coverage/test", _
                   "/home/user1/tmp/covert/operator", _
                   "/home/user1/tmp/coven/members", _
                   "/home/user1/abc/coven/members"
FOR i = 0 TO UBOUND(s()): CON.PRINT s(i): NEXT i
CON.PRINT CommonDirectoryPath(s()) & " <- common"
CON.PRINT

REDIM s(0 TO 2)
ARRAY ASSIGN s() = "/home/user1/tmp/coverage/test", _
                   "/hope/user1/tmp/covert/operator", _
                   "/home/user1/tmp/coven/members"
FOR i = 0 TO UBOUND(s()): CON.PRINT s(i): NEXT i
CON.PRINT CommonDirectoryPath(s()) & " <- common"
CON.PRINT

CON.PRINT "hit any key to end program"
CON.WAITKEY$

END FUNCTION
```

{{out}}

```txt
/home/user1/tmp/coverage/test
/home/user1/tmp/covert/operator
/home/user1/tmp/coven/members
/home/user1/tmp <- common

/home/user1/tmp/coverage/test
/home/user1/tmp/covert/operator
/home/user1/tmp/coven/members
/home/user1/abc/coven/members
/home/user1 <- common

/home/user1/tmp/coverage/test
/hope/user1/tmp/covert/operator
/home/user1/tmp/coven/members
/ <- common
```



## PowerShell



```PowerShell
# Find Common Directory Path
# Input is an array of strings holding the paths
function Get-CommonDirectoryPath($paths){

   # Convert each path into array of tokens (i.e. convert array into jagged array)
   for($i=0; $i -lt $paths.Count; $i++) {
      $paths[$i] =  ($paths[$i].TrimStart('/').Split('/'))
   }
	
   # Loop through tokens	
   $c = -1
   $found = $false
   do {		# Do Until loop used to handle paths with different number of directories
      $t = $paths[0][++$c]
      for($r = 1; $r -lt $paths.Count; $r++) {
         if ($t -ne $paths[$r][$c]) { $found=$true; break }
      }
   } until ($found)

   # Return the answer
   for($i=0; $i -lt $c; $i++) {$s += "/"+$paths[0][$i]}
   return $s
}

# Main Entry Point
"The common directory path is " + (Get-CommonDirectoryPath ("/home/user1/tmp/coverage/test", "/home/user1/tmp/covert/operator", "/home/user1/tmp/coven/members"))
```


Output:
<lang>The common directory path is /home/user1/tmp
```



Another version that leverage cmdlet in Powershell


```Powershell

Function Get-CommonPath( $Separator, $PathList ){
    $SplitPaths = $PathList | foreach { , $_.Split($Separator) }
    $MinDirectoryDepth = $SplitPaths | Measure-Object -Property Length -Minimum | Select -ExpandProperty Minimum
    $CommonPath = foreach ($Index in 0..($MinDirectoryDepth - 1)) {
        $UniquePath = @($SplitPaths | foreach { $_[$Index] } | Sort -Unique)
        if ($UniquePath.Length -gt 1) {
            break;
        }
 
        $UniquePath
    }
 
    [String]::Join($Separator, $CommonPath)
}

```


with the following sample execution:
<lang>
PS> Get-CommonPath '/' "/home/user1/tmp/coverage/test","/home/user1/tmp/covert/operator","/home/user1/tmp/coven/members"
/home/user1/tmp

```



## PureBasic


PureBasic don't have a path comparator directly but instead have powerful string tools.

Simply by checking the catalog names until they mismatch and add up the correct parts, the task is accomplished.

```PureBasic
Procedure.s CommonPath(Array InPaths.s(1),separator.s="/")
  Protected SOut$=""
  Protected i, j, toggle
  
  If ArraySize(InPaths())=0
    ProcedureReturn InPaths(0)  ; Special case, only one path
  EndIf
  
  Repeat
    i+1
    toggle=#False
    For j=1 To ArraySize(InPaths())
      If (StringField(InPaths(j-1),i,separator)=StringField(InPaths(j),i,separator))
        If Not toggle
          SOut$+StringField(InPaths(j-1),i,separator)+separator
          toggle=#True
        EndIf
      Else
        ProcedureReturn SOut$
      EndIf      
    Next
  ForEver
EndProcedure
```


Example of implementation

```PureBasic
Dim t.s(2)
t(0)="/home/user1/tmp/coverage/test"
t(1)="/home/user1/tmp/covert/operator"
t(2)="/home/user1/tmp/coven/members"

Debug CommonPath(t(),"/"))
```



## Python

The Python os.path.commonprefix function is [http://nedbatchelder.com/blog/201003/whats_the_point_of_ospathcommonprefix.html broken] as it returns common characters that may not form a valid directory path:

```python>>>
 import os
>>> os.path.commonprefix(['/home/user1/tmp/coverage/test', 
                          '/home/user1/tmp/covert/operator', '/home/user1/tmp/coven/members'])
'/home/user1/tmp/cove'
```


This result can be fixed:

```python>>>
 def commonprefix(args, sep='/'):
	return os.path.commonprefix(args).rpartition(sep)[0]

>>> commonprefix(['/home/user1/tmp/coverage/test', 
                  '/home/user1/tmp/covert/operator', '/home/user1/tmp/coven/members'])
'/home/user1/tmp'
```


Even shorter:

```python>>>
 paths = ['/home/user1/tmp/coverage/test', '/home/user1/tmp/covert/operator', '/home/user1/tmp/coven/members']
>>> os.path.dirname(os.path.commonprefix(paths))
'/home/user1/tmp'
```


But it may be better to not rely on the faulty implementation at all:

```python>>>
 from itertools import takewhile
>>> def allnamesequal(name):
	return all(n==name[0] for n in name[1:])

>>> def commonprefix(paths, sep='/'):
	bydirectorylevels = zip(*[p.split(sep) for p in paths])
	return sep.join(x[0] for x in takewhile(allnamesequal, bydirectorylevels))

>>> commonprefix(['/home/user1/tmp/coverage/test', 
                  '/home/user1/tmp/covert/operator', '/home/user1/tmp/coven/members'])
'/home/user1/tmp'
>>> # And also
>>> commonprefix(['/home/user1/tmp', '/home/user1/tmp/coverage/test',
                  '/home/user1/tmp/covert/operator', '/home/user1/tmp/coven/members'])
'/home/user1/tmp'
>>> 
```



## R


```r

get_common_dir <- function(paths, delim = "/")
{
  path_chunks <- strsplit(paths, delim)
  
  i <- 1
  repeat({
    current_chunk <- sapply(path_chunks, function(x) x[i])
    if(any(current_chunk != current_chunk[1])) break
    i <- i + 1
  })
  paste(path_chunks[[1]][seq_len(i - 1)], collapse = delim)

}

# Example Usage:
paths <- c(
  '/home/user1/tmp/coverage/test',
  '/home/user1/tmp/covert/operator',
  '/home/user1/tmp/coven/members')

get_common_dir(paths)           # "/home/user1/tmp"


```



## Racket


```Racket

#lang racket

(define (common-directory path . paths)
  (string-join
   (let loop ([path  (string-split path "/" #:trim? #f)]
              [paths (map (λ(p) (string-split p "/" #:trim? #f)) paths)])
     (if (and (pair? path)
              (andmap (λ(p) (and (pair? p) (equal? (car p) (car path))))
                      paths))
       (cons (car path) (loop (cdr path) (map cdr paths)))
       '()))
   "/"))

(common-directory
 "/home/user1/tmp/coverage/test"
 "/home/user1/tmp/covert/operator"
 "/home/user1/tmp/coven/members")
;; --> "/home/user1/tmp"

```



## REXX


```rexx
/*REXX program  finds  the  common directory path  for a list of files.                 */
          @.  =                                  /*the default for all file lists (null)*/
          @.1 = '/home/user1/tmp/coverage/test'
          @.2 = '/home/user1/tmp/covert/operator'
          @.3 = '/home/user1/tmp/coven/members'
L= length(@.1)                                   /*use the length of the first string.  */
                 do j=2  while  @.j\==''         /*start search with the second string. */
                 _= compare(@.j, @.1)            /*use REXX  compare  BIF for comparison*/
                 if _==0  then iterate           /*Strings are equal? Then con't use min*/
                 L= min(L, _)                    /*get the minimum length equal strings.*/
                 if right(@.j, 1)=='/'  then iterate  /*if a directory,  then it's OK.  */
                 L= lastpos('/', left(@.j, L) )       /*obtain directory name up to here*/
                 end   /*j*/

common= left( @.1, lastpos('/', @.1, L) )        /*determine the shortest  DIR  string. */
if right(common, 1)=='/'  then common= left(common, max(0, length(common) - 1) )
if common==''  then common= "/"                  /*if no common directory, assume home. */
say 'common directory path: '  common            /* [↑]  handle trailing   /   delimiter*/
                                                 /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default inputs:}} 

```txt

common directory path:  /home/user1/tmp

```



## Ring


```ring

# Project : Find common directory path

load "stdlib.ring"
i = null
o = null
path = list(3)
 
path[1] = "/home/user1/tmp/coverage/test"
path[2] = "/home/user1/tmp/covert/operator"
path[3] = "/home/user1/tmp/coven/members"
 
see commonpath(path, "/")
 
func commonpath(p, s)
while i != 0
      o = i
      i = substring(p[1], s, i+1)
      for j = 2 to len(p)
          if left(p[1], i) != left(p[j], i) 
             exit 2
          ok
      next
end
return left(p[1], o-1)

```

Output:

```txt

/home/user1/tmp

```



## Ruby

Uses the standard library <code>[http://www.ruby-doc.org/stdlib/libdoc/abbrev/rdoc/index.html abbrev]</code> module: Given a set of strings, calculate the set of unambiguous abbreviations for those strings, and return a hash where the keys are all the possible abbreviations and the values are the full strings.


```ruby
require 'abbrev'

dirs = %w( /home/user1/tmp/coverage/test /home/user1/tmp/covert/operator /home/user1/tmp/coven/members )

common_prefix = dirs.abbrev.keys.min_by {|key| key.length}.chop  # => "/home/user1/tmp/cove"
common_directory = common_prefix.sub(%r{/[^/]*$}, '')            # => "/home/user1/tmp"
```


Implementing without that module:

```ruby
separator = '/'
path0, *paths = dirs.collect {|dir| dir.split(separator)}
uncommon_idx = path0.zip(*paths).index {|dirnames| dirnames.uniq.length > 1}
uncommon_idx = path0.length  unless uncommon_idx                # if uncommon_idx==nil
common_directory = path0[0...uncommon_idx].join(separator)      # => "/home/user1/tmp"
```


or method version

```ruby
def common_directory_path(dirs, separator='/')
  dir1, dir2 = dirs.minmax.map{|dir| dir.split(separator)}
  dir1.zip(dir2).take_while{|dn1,dn2| dn1==dn2}.map(&:first).join(separator)
end

p common_directory_path(dirs)           #=> "/home/user1/tmp"
```



## Run BASIC


```runbasic
' ------------------------------------------
' Find common directory to all directories
' and directories common with other Paths
' ------------------------------------------
print word$(word$(httpget$("http://tycho.usno.navy.mil/cgi-bin/timer.pl"),1,"UTC"),2,"<BR>") ' Universal time

dim path$(20)
path$(1)	= "/home/user1/tmp/coverage/test"
path$(2)	= "/home/user1/tmp/covert/operator"
path$(3)	= "/home/user1/tmp/coven/members"

path$(4)	= "/home/user1/tmp1/coverage/test"
path$(5)	= "/home/user1/tmp1/covert/operator"
path$(6)	= "/home/user1/tmp1/coven/members"

path$(7)	= "/home/user1/tmp2/coverage/test"
path$(8)	= "/home/user1/tmp2/covert/operator"
path$(9)	= "/home/user1/tmp2/coven/members"

path$(10)	= "/home/user1/tmp3/coverage/test"
path$(11)	= "/home/user1/tmp3/covert/operator"
path$(12)	= "/home/user1/tmp3/coven/members"

sqliteconnect #mem, ":memory:"
#mem execute("CREATE TABLE dirTree (seq,pos,dir)")

for i = 1 to 12
j = 1
[loop]
j 	= instr(path$(i),"/",j + 1)
if j > 0 then
	dir$	= mid$(path$(i),1,j)
	mem$	= "INSERT INTO dirTree VALUES (";i;",";j;",'";dir$;"')"
	#mem execute(mem$)
	goto [loop]
end if
next i

mem$ = "SELECT	dir FROM dirTree GROUP BY dir HAVING count(*) = pos ORDER BY pos desc LIMIT 1"
#mem execute(mem$)
rows = #mem ROWCOUNT()		'Get the number of rows
if rows > 0 then
	#row = #mem #nextrow()
	print "
### === Largest Directory Common to all Paths ======
"
	print #row dir$()
    else
	print "No common Directory"
end if

html "<HR>"

print "
### ====== Common paths =============
"

mem$ = "SELECT t.seq as seq,t.pos as pos,t.dir as dir,t1.seq as t1Seq ,t1.dir as t1Dir
FROM	dirTree as t 
JOIN	dirTree as t1
ON	t1.dir = t.dir
AND	t1.seq > t.seq
GROUP BY t.dir,t1.seq"

html "<table border=1><TR align=center>
<TD>Seq</TD>
<TD>Path</TD>
<TD>Common Dir</TD>
<TD>Seq</TD>
<TD>With Path</TD></TR>"

#mem execute(mem$)
WHILE   #mem hasanswer()
	#row 	= #mem #nextrow()
	seq	= #row seq()
	t1Seq	= #row t1Seq()
	pos	= #row pos()
	dir$	= #row dir$()
	t1Dir$	= #row t1Dir$()
html "<TR>"
html "<TD>";seq;"</TD>"
html "<TD>";path$(seq);"</TD>"
html "<TD>";dir$;"</TD>"
html "<TD>";t1Seq;"</TD>"
html "<TD>";path$(t1Seq);"</TD>"
html "</TR>"
WEND
html "</TABLE>"
wait
end
```


### ====== Common paths =============
<br />
shows only the first few common paths.. 

<table border=1><TR align=center>
<TD>Seq</TD><TD>Path</TD><TD>Common Dir</TD><TD>Seq</TD><TD>With Path</TD>
</TR><TR><TD>1</TD><TD>/home/user1/tmp/coverage/test</TD><TD>/home/</TD><TD>2</TD><TD>/home/user1/tmp/covert/operator</TD>
</TR><TR><TD>2</TD><TD>/home/user1/tmp/covert/operator</TD><TD>/home/</TD><TD>3</TD><TD>/home/user1/tmp/coven/members</TD>
</TR><TR><TD>3</TD><TD>/home/user1/tmp/coven/members</TD><TD>/home/</TD><TD>4</TD><TD>/home/user1/tmp1/coverage/test</TD>
</TR><TR><TD>4</TD><TD>/home/user1/tmp1/coverage/test</TD><TD>/home/</TD><TD>5</TD><TD>/home/user1/tmp1/covert/operator</TD>
</TR><TR><TD>5</TD><TD>/home/user1/tmp1/covert/operator</TD><TD>/home/</TD><TD>6</TD><TD>/home/user1/tmp1/coven/members</TD>
</TR><TR><TD>6</TD><TD>/home/user1/tmp1/coven/members</TD><TD>/home/</TD><TD>7</TD><TD>/home/user1/tmp2/coverage/test</TD>
</TR><TR><TD>7</TD><TD>/home/user1/tmp2/coverage/test</TD><TD>/home/</TD><TD>8</TD><TD>/home/user1/tmp2/covert/operator</TD>
</TR><TR><TD>8</TD><TD>/home/user1/tmp2/covert/operator</TD><TD>/home/</TD><TD>9</TD><TD>/home/user1/tmp2/coven/members</TD>
</TR><TR><TD>9</TD><TD>/home/user1/tmp2/coven/members</TD><TD>/home/</TD><TD>10</TD><TD>/home/user1/tmp3/coverage/test</TD>
</TR><TR><TD>10</TD><TD>/home/user1/tmp3/coverage/test</TD><TD>/home/</TD><TD>11</TD><TD>/home/user1/tmp3/covert/operator</TD>
</TR><TR><TD>11</TD><TD>/home/user1/tmp3/covert/operator</TD><TD>/home/</TD><TD>12</TD><TD>/home/user1/tmp3/coven/members</TD>
</TR><TR><TD>1</TD><TD>/home/user1/tmp/coverage/test</TD><TD>/home/user1/</TD><TD>2</TD><TD>/home/user1/tmp/covert/operator</TD>
</TR><TR><TD>2</TD><TD>/home/user1/tmp/covert/operator</TD><TD>/home/user1/</TD><TD>3</TD><TD>/home/user1/tmp/coven/members</TD>
</TR><TR><TD>3</TD><TD>/home/user1/tmp/coven/members</TD><TD>/home/user1/</TD><TD>4</TD><TD>/home/user1/tmp1/coverage/test</TD>
</TR><TR><TD>4</TD><TD>/home/user1/tmp1/coverage/test</TD><TD>/home/user1/</TD><TD>5</TD><TD>/home/user1/tmp1/covert/operator</TD>
</TR><TR><TD>5</TD><TD>/home/user1/tmp1/covert/operator</TD><TD>/home/user1/</TD><TD>6</TD><TD>/home/user1/tmp1/coven/members</TD>
</TR><TR><TD>6</TD><TD>/home/user1/tmp1/coven/members</TD><TD>/home/user1/</TD><TD>7</TD><TD>/home/user1/tmp2/coverage/test</TD>
</TR><TR><TD>7</TD><TD>/home/user1/tmp2/coverage/test</TD><TD>/home/user1/</TD><TD>8</TD><TD>/home/user1/tmp2/covert/operator</TD>
</TR><TR><TD>8</TD><TD>/home/user1/tmp2/covert/operator</TD><TD>/home/user1/</TD><TD>9</TD><TD>/home/user1/tmp2/coven/members</TD>
</TR><TR><TD>9</TD><TD>/home/user1/tmp2/coven/members</TD><TD>/home/user1/</TD><TD>10</TD><TD>/home/user1/tmp3/coverage/test</TD>
</TR><TR><TD>10</TD><TD>/home/user1/tmp3/coverage/test</TD><TD>/home/user1/</TD><TD>11</TD><TD>/home/user1/tmp3/covert/operator</TD>
</TR><TR><TD>11</TD><TD>/home/user1/tmp3/covert/operator</TD><TD>/home/user1/</TD><TD>12</TD><TD>/home/user1/tmp3/coven/members</TD>
</TR><TR><TD>1</TD><TD>/home/user1/tmp/coverage/test</TD><TD>/home/user1/tmp/</TD><TD>2</TD><TD>/home/user1/tmp/covert/operator</TD>
</TR><TR><TD>2</TD><TD>/home/user1/tmp/covert/operator</TD><TD>/home/user1/tmp/</TD><TD>3</TD><TD>/home/user1/tmp/coven/members</TD>
</TR><TR><TD>4</TD><TD>/home/user1/tmp1/coverage/test</TD><TD>/home/user1/tmp1/</TD><TD>5</TD><TD>/home/user1/tmp1/covert/operator</TD>
</TR><TR><TD>5</TD><TD>/home/user1/tmp1/covert/operator</TD><TD>/home/user1/tmp1/</TD><TD>6</TD><TD>/home/user1/tmp1/coven/members</TD>
</TR><TR><TD>7</TD><TD>/home/user1/tmp2/coverage/test</TD><TD>/home/user1/tmp2/</TD><TD>8</TD><TD>/home/user1/tmp2/covert/operator</TD>
</TR><TR><TD>8</TD><TD>/home/user1/tmp2/covert/operator</TD><TD>/home/user1/tmp2/</TD><TD>9</TD><TD>/home/user1/tmp2/coven/members</TD>
</TR><TR><TD>10</TD><TD>/home/user1/tmp3/coverage/test</TD><TD>/home/user1/tmp3/</TD><TD>11</TD><TD>/home/user1/tmp3/covert/operator</TD>
</TR><TR><TD>11</TD><TD>/home/user1/tmp3/covert/operator</TD><TD>/home/user1/tmp3/</TD><TD>12</TD><TD>/home/user1/tmp3/coven/members</TD>
<TR></TABLE>



## Rust

Rust has specific types for owned and borrowed paths. PathBuf is an 'owned' pointer to a path, Path is a borrow; this is similar to String and str, respectively.


```Rust

use std::path::{Path, PathBuf};

fn main() {
    let paths = [
        Path::new("/home/user1/tmp/coverage/test"),
        Path::new("/home/user1/tmp/covert/operator"),
        Path::new("/home/user1/tmp/coven/members"),
    ];
    match common_path(&paths) {
        Some(p) => println!("The common path is: {:#?}", p),
        None => println!("No common paths found"),
    }
}

fn common_path<I, P>(paths: I) -> Option<PathBuf>
where
    I: IntoIterator<Item = P>,
    P: AsRef<Path>,
{
    let mut iter = paths.into_iter();
    let mut ret = iter.next()?.as_ref().to_path_buf();
    for path in iter {
        if let Some(r) = common(ret, path.as_ref()) {
            ret = r;
        } else {
            return None;
        }
    }
    Some(ret)
}

fn common<A: AsRef<Path>, B: AsRef<Path>>(a: A, b: B) -> Option<PathBuf> {
    let a = a.as_ref().components();
    let b = b.as_ref().components();
    let mut ret = PathBuf::new();
    let mut found = false;
    for (one, two) in a.zip(b) {
        if one == two {
            ret.push(one);
            found = true;
        } else {
            break;
        }
    }
    if found {
        Some(ret)
    } else {
        None
    }
}

```



## Scala


### Naive

This simple solution solves the task as given, but has oddities for edge cases due to the implementation of java.lang.String#split.

```Scala
object FindCommonDirectoryPath extends App {
  def commonPath(paths: List[String]): String = {
    def common(a: List[String], b: List[String]): List[String] = (a, b) match {
      case (a :: as, b :: bs) if a equals b => a :: common(as, bs)
      case _ => Nil
    }
    if (paths.length < 2) paths.headOption.getOrElse("")
    else paths.map(_.split("/").toList).reduceLeft(common).mkString("/")
  }

  val test = List(
    "/home/user1/tmp/coverage/test",
    "/home/user1/tmp/covert/operator",
    "/home/user1/tmp/coven/members"
  )
  println(commonPath(test))
}
```

Output:

```txt
/home/user1/tmp
```


### Advanced

This implementation will handle various edge cases and relative paths. It also includes any common trailing '/' but callers can remove this if desired.

```Scala
object FindCommonDirectoryPathRelative extends App {
  def commonPath(paths: List[String]): String = {
    val SEP = "/"
    val BOUNDARY_REGEX = s"(?=[$SEP])(?<=[^$SEP])|(?=[^$SEP])(?<=[$SEP])"
    def common(a: List[String], b: List[String]): List[String] = (a, b) match {
      case (a :: as, b :: bs) if a equals b => a :: common(as, bs)
      case _ => Nil
    }
    if (paths.length < 2) paths.headOption.getOrElse("")
    else paths.map(_.split(BOUNDARY_REGEX).toList).reduceLeft(common).mkString
  }

  val test = List(
    "/home/user1/tmp/coverage/test",
    "/home/user1/tmp/covert/operator",
    "/home/user1/tmp/coven/members"
  )
  println(commonPath(test).replaceAll("/$", ""))

  // test cases
  assert(commonPath(test.take(1)) == test.head)
  assert(commonPath(Nil) == "")
  assert(commonPath(List("")) == "")
  assert(commonPath(List("/")) == "/")
  assert(commonPath(List("/", "")) == "")
  assert(commonPath(List("/", "/a")) == "/")
  assert(commonPath(List("/a", "/b")) == "/")
  assert(commonPath(List("/a", "/a")) == "/a")
  assert(commonPath(List("/a/a", "/b")) == "/")
  assert(commonPath(List("/a/a", "/b")) == "/")
  assert(commonPath(List("/a/a", "/a")) == "/a")
  assert(commonPath(List("/a/a", "/a/b")) == "/a/")
  assert(commonPath(List("/a/b", "/a/b")) == "/a/b")
  assert(commonPath(List("a", "/a")) == "")
  assert(commonPath(List("a/a", "/a")) == "")
  assert(commonPath(List("a/a", "/b")) == "")
  assert(commonPath(List("a", "a")) == "a")
  assert(commonPath(List("a/a", "b")) == "")
  assert(commonPath(List("a/a", "b")) == "")
  assert(commonPath(List("a/a", "a")) == "a")
  assert(commonPath(List("a/a", "a/b")) == "a/")
  assert(commonPath(List("a/b", "a/b")) == "a/b")
  assert(commonPath(List("/a/", "/b/")) == "/")
  assert(commonPath(List("/a/", "/a/")) == "/a/")
  assert(commonPath(List("/a/a/", "/b/")) == "/")
  assert(commonPath(List("/a/a/", "/b/")) == "/")
  assert(commonPath(List("/a/a/", "/a/")) == "/a/")
  assert(commonPath(List("/a/a/", "/a/b/")) == "/a/")
  assert(commonPath(List("/a/b/", "/a/b/")) == "/a/b/")
}
```



## Seed7

Seed7 has a [http://seed7.sourceforge.net/manual/os.htm#Standard_path_representation standard path representation]:
*The slash ('/') is used as path delimiter.
*Drive letters are not allowed, but there is a solution to replace them.
*Except for the path "/" a standard path is not allowed to end with a slash.
Therefore Seed7 programs do not need to consider varying path delimiters,
but they need to make sure that a path does not end with a slash.


```seed7
$ include "seed7_05.s7i";

const func integer: commonLen (in array string: names, in char: sep) is func
  result
    var integer: result is -1;
  local
    var integer: index is 0;
    var integer: pos is 1;
  begin
    if length(names) <> 0 then
      repeat
        for index range 1 to length(names) do
          if pos > length(names[index]) or names[index][pos] <> names[1][pos] then
            decr(pos);
            while pos >= 1 and names[1][pos] <> sep do
              decr(pos);
            end while;
            if pos > 1 then
              decr(pos);
            end if;
            result := pos;
          end if;
        end for;
        incr(pos);
      until result <> -1;
    end if;
  end func;
 
const proc: main is func
  local
    var integer: length is 0;
    const array string: names is [] ("/home/user1/tmp/coverage/test",
                                     "/home/user1/tmp/covert/operator",
                                     "/home/user1/tmp/coven/members")
  begin
    length := commonLen(names, '/');
    if length = 0 then
      writeln("No common path");
    else
      writeln("Common path: " <& names[1][.. length]);
    end if;
  end func;
```


Output:

```txt

Common path: /home/user1/tmp

```



## Sidef


```ruby
var dirs = %w(
    /home/user1/tmp/coverage/test
    /home/user1/tmp/covert/operator
    /home/user1/tmp/coven/members
);

var unique_pref = dirs.map{.split('/')}.abbrev.min_by{.len};
var common_dir  = [unique_pref, unique_pref.pop][0].join('/');
say common_dir;   # => /home/user1/tmp
```



## Swift


The below solution works only in swift in Linux.


```swift
import Foundation


func getPrefix(_ text:[String]) -> String? {
    var common:String = text[0]
    for i in text {
        common = i.commonPrefix(with: common)
    }
    return common
}

var test = ["/home/user1/tmp/coverage/test", 
 "/home/user1/tmp/covert/operator",
 "/home/user1/tmp/coven/members"]

var output:String = getPrefix(test)!
print(output)
```



## Tcl


```tcl
package require Tcl 8.5
proc pop {varname} {
    upvar 1 $varname var
    set var [lassign $var head]
    return $head
}

proc common_prefix {dirs {separator "/"}} {
    set parts [split [pop dirs] $separator]
    while {[llength $dirs]} {
        set r {}
        foreach cmp $parts elt [split [pop dirs] $separator] {
            if {$cmp ne $elt} break
            lappend r $cmp
        }
        set parts $r
    }
    return [join $parts $separator]
}
```



```txt
% common_prefix {/home/user1/tmp/coverage/test /home/user1/tmp/covert/operator /home/user1/tmp/coven/members}
/home/user1/tmp
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT
common=""
dir1="/home/user1/tmp/coverage/test"
dir2="/home/user1/tmp/covert/operator"
dir3="/home/user1/tmp/coven/members"
dir1=SPLIT (dir1,":/:"),dir2=SPLIT (dir2,":/:"), dir3=SPLIT (dir3,":/:")
LOOP d1=dir1,d2=dir2,d3=dir3
 IF (d1==d2,d3) THEN
  common=APPEND(common,d1,"/")
 ELSE
  PRINT common
  EXIT
 ENDIF
ENDLOOP

```

Output:

```txt

/home/user1/tmp/

```



## UNIX Shell

The following is a pure Bourne Shell solution.  The while loop controls the maximum depth to check paths.

```bash

#!/bin/sh

pathlist='/home/user1/tmp/coverage/test
/home/user1/tmp/covert/operator
/home/user1/tmp/coven/members'

i=2

while [ $i -lt 100 ]
do
  path=`echo "$pathlist" | cut -f1-$i -d/ | uniq -d`
  if [ -z "$path" ]
  then
     echo $prev_path
     break
  else
     prev_path=$path
  fi
  i=`expr $i + 1`
done

```




## Ursala

The algorithm is to lex the paths into component directory names, and then find the greatest common prefix of those.

```Ursala
#import std

comdir"s" "p" = mat"s" reduce(gcp,0) (map sep "s") "p"
```

where <code>"s"</code> is a dummy variable representing the separator, <code>"p"</code> is a dummy variable representing the list of paths, and
*<code>sep</code> is second order function in the standard library that takes a separator character and returns a lexer mapping a string containing the separator to a list of the substrings found between occurrences of it
*<code>map</code> is the conventional mapping combinator, which takes a function operating on items of a list to a function operating pointwise on a whole list
*<code>gcp</code> is a polymorphic greatest-common-prefix library function working on pairs of strings or lists of any type
*<code>reduce</code> is the standard functional programming reduction combinator, which cumulatively applies a binary operator to a list of operands given the operator and the vacuous case result
*<code>mat</code> is a second order function in the standard library that takes a separator character and returns a function that flattens a list of strings into a single string with copies of the separator inserted between them
Here is a version using operators instead of mnemonics for <code>map</code> and <code>reduce</code>.

```Ursala
comdir"s" "p" = mat"s" gcp:-0 sep"s"* "p"
```

Here is one in partly point-free form, using the composition operator (<code>+</code>).

```Ursala
comdir"s" = mat"s"+ gcp:-0+ sep"s"*
```

Here it is in point-free form.

```Ursala
comdir = +^/mat gcp:-0++ *+ sep
```

test program:

```Ursala
#cast %s

test = 

comdir`/ <
   '/home/user1/tmp/coverage/test',
   '/home/user1/tmp/covert/operator',
   '/home/user1/tmp/coven/members'>
```

output:

```txt
'/home/user1/tmp'
```


{{omit from|GUISS}}


## VBScript

{{works with|Windows Script Host|*}}

```VBScript

' Read the list of paths (newline-separated) into an array...
strPaths = Split(WScript.StdIn.ReadAll, vbCrLf)
 
' Split each path by the delimiter (/)...
For i = 0 To UBound(strPaths)
	strPaths(i) = Split(strPaths(i), "/")
Next

With CreateObject("Scripting.FileSystemObject")

	' Test each path segment...
	For j = 0 To UBound(strPaths(0))
		
		' Test each successive path against the first...
		For i = 1 To UBound(strPaths)
			If strPaths(0)(j) <> strPaths(i)(j) Then Exit For
		Next

		' If we didn't make it all the way through, exit the block...
		If i <= UBound(strPaths) Then Exit For
		
		' Make sure this path exists...
		If Not .FolderExists(strPath & strPaths(0)(j) & "/") Then Exit For
		strPath = strPath & strPaths(0)(j) & "/"
		
	Next

End With

' Remove the final "/"...
WScript.Echo Left(strPath, Len(strPath) - 1)

```



## Visual Basic

{{works with|Visual Basic|5}}
{{works with|Visual Basic|6}}
{{works with|VBA|6.5}}
{{works with|VBA|7.1}}

```vb
Public Function CommonDirectoryPath(ParamArray Paths()) As String
Dim v As Variant
Dim Path() As String, s As String
Dim i As Long, j As Long, k As Long
Const PATH_SEPARATOR As String = "/"
  
  For Each v In Paths
    ReDim Preserve Path(0 To i)
    Path(i) = v
    i = i + 1
  Next v
  
  k = 1
  
  Do
    For i = 0 To UBound(Path)
      If i Then
        If InStr(k, Path(i), PATH_SEPARATOR) <> j Then
          Exit Do
        ElseIf Left$(Path(i), j) <> Left$(Path(0), j) Then
          Exit Do
        End If
      Else
        j = InStr(k, Path(i), PATH_SEPARATOR)
        If j = 0 Then
          Exit Do
        End If
      End If
    Next i
    s = Left$(Path(0), j + CLng(k <> 1))
    k = j + 1
  Loop
  CommonDirectoryPath = s
  
End Function

Sub Main()

' testing the above function

Debug.Assert CommonDirectoryPath( _
 "/home/user1/tmp/coverage/test", _
 "/home/user1/tmp/covert/operator", _
 "/home/user1/tmp/coven/members") = _
 "/home/user1/tmp"
 
 Debug.Assert CommonDirectoryPath( _
 "/home/user1/tmp/coverage/test", _
 "/home/user1/tmp/covert/operator", _
 "/home/user1/tmp/coven/members", _
 "/home/user1/abc/coven/members") = _
 "/home/user1"

Debug.Assert CommonDirectoryPath( _
 "/home/user1/tmp/coverage/test", _
 "/hope/user1/tmp/covert/operator", _
 "/home/user1/tmp/coven/members") = _
 "/"

End Sub
```



## zkl


```zkl
dirs:=T("/home/user1/tmp/coverage/test", "/home/user1/tmp/covert/operator",
        "/home/user1/tmp/coven/members");
n:=Utils.zipWith('==,dirs.xplode()).find(False); // character pos which differs
n=dirs[0][0,n].rfind("/");  // find last "/"
dirs[0][0,n];
```

{{out}}

```txt

/home/user1/tmp

```

Will throw an error if no match, "" if common dir is "/"
