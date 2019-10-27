+++
title = "Trabb Pardo–Knuth algorithm"
description = ""
date = 2019-04-03T15:45:40Z
aliases = []
[extra]
id = 11755
[taxonomies]
categories = []
tags = []
+++

{{task}}
The TPK algorithm is an early example of a programming chrestomathy. 
It was used in Donald Knuth and Luis Trabb Pardo's Stanford tech report [http://bitsavers.org/pdf/stanford/cs_techReports/STAN-CS-76-562_EarlyDevelPgmgLang_Aug76.pdf The Early Development of Programming Languages]. 
The report traces the early history of work in developing computer languages in the 1940s and 1950s, giving several translations of the algorithm.

From the [[wp:Trabb Pardo–Knuth algorithm|wikipedia entry]]:

 '''ask''' for 11 numbers to be read into a sequence ''S''
 '''reverse''' sequence ''S''
 '''for each''' ''item'' '''in''' sequence ''S''
     ''result'' ''':=''' '''call''' a function to do an ''operation''
     '''if''' ''result'' overflows
         '''alert''' user
     '''else'''
         '''print''' ''result''

The task is to implement the algorithm:
# Use the function:     <big><math>f(x) = |x|^{0.5} + 5x^3</math></big>
# The overflow condition is an answer of greater than 400.
# The 'user alert' should not stop processing of other items of the sequence.
# Print a prompt before accepting '''eleven''', textual, numeric inputs.
# You may optionally print the item as well as its associated result, but the results must be in reverse order of input.
# The sequence   ''' ''S'' '''   may be 'implied' and so not shown explicitly.
# ''Print and show the program in action from a typical run here''. (If the output is graphical rather than text then either add a screendump or describe textually what is displayed).





## Ada



```Ada
with Ada.Text_IO, Ada.Numerics.Generic_Elementary_Functions;

procedure Trabb_Pardo_Knuth is

   type Real is digits 6 range -400.0 .. 400.0;

   package TIO renames Ada.Text_IO;
   package FIO is new TIO.Float_IO(Real);
   package Math is new  Ada.Numerics.Generic_Elementary_Functions(Real);

   function F(X: Real) return Real is
   begin
      return (Math.Sqrt(abs(X)) + 5.0 * X**3);
   end F;

   Values: array(1 .. 11) of Real;

begin
   TIO.Put("Please enter 11 Numbers:");
   for I in Values'Range loop
      FIO.Get(Values(I));
   end loop;

   for I in reverse Values'Range loop
      TIO.Put("f(");
      FIO.Put(Values(I), Fore => 2, Aft => 3, Exp => 0);
      TIO.Put(")=");
      begin
         FIO.Put(F(Values(I)), Fore=> 4, Aft => 3, Exp => 0);
      exception
         when Constraint_Error => TIO.Put("-->too large<--");
      end;
      TIO.New_Line;
   end loop;

end Trabb_Pardo_Knuth;
```


{{out}}

```txt
> ./trabb_pardo_knuth 
Please enter 11 Numbers:10 -1 1 2 3 4 4.3 4.305 4.303 4.302 4.301
f( 4.301)= 399.886
f( 4.302)=-->too large<--
f( 4.303)=-->too large<--
f( 4.305)=-->too large<--
f( 4.300)= 399.609
f( 4.000)= 322.000
f( 3.000)= 136.732
f( 2.000)=  41.414
f( 1.000)=   6.000
f(-1.000)=  -4.000
f(10.000)=-->too large<--
```



## Agena

Tested with Agena 2.9.5 Win32
{{Trans|ALGOL W}}

```agena
scope   # TPK algorithm in Agena
  local y;
  local a := [];
  local f := proc( t :: number ) is return sqrt(abs(t))+5*t*t*t end;
  for i from 0 to 10 do a[i] := tonumber( io.read() ) od;
  for i from 10 to 0 by - 1 do
      y:=f(a[i]);
      if y > 400
      then print( "TOO LARGE" )
      else printf( "%10.4f\n", y )
      fi
  od
epocs
```

{{out}}

```txt

1
2
3
4
5
6
7
8
9
10
11
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
  322.0000
  136.7321
   41.4142
    6.0000

```



## ALGOL 60

This is as close as possible to Pardo and Knuth's original but works with the [http://www.gnu.org/software/marst/marst.html GNU MARST] ALGOL-to-C compiler. Note Pardo and Knuth did not insist on prompts or textual I/O as their report mostly concerned systems that predated even the idea of keyboard interaction.

<lang>begin 
  integer i; real y; real array a[0:10];
  real procedure f(t); value t; real t;
    f:=sqrt(abs(t))+5*t^3;
  for i:=0 step 1 until 10 do inreal(0, a[i]);
  for i:=10 step -1 until 0 do
    begin
       y:=f(a[i]);
       if y > 400 then outstring(1, "TOO LARGE")
                  else outreal(1,y);
       outchar(1, "\n", 1)
    end
end
```


Compilation and sample run:

```txt

bash-3.2$ marst tpk.a60 -o tpk.c
bash-3.2$ gcc tpk.c -lalgol -lm -o tpk
bash-3.2$ ./tpk
1 2 3 4 5 6 7 8 9 10 11
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
322 
136.732050808 
41.4142135624 
6 
bash-3.2$
```



## ALGOL 68

{{Trans|ALGOL W}} which was itself a Translation of ALGOL 60.

```algol68
[ 0 : 10 ]REAL a;
PROC f = ( REAL t )REAL:
    sqrt(ABS t)+5*t*t*t;
FOR i FROM LWB a TO UPB a DO read( ( a[ i ] ) ) OD;
FOR i FROM UPB a BY -1 TO LWB a DO
       REAL y=f(a[i]);
       IF y > 400 THEN print( ( "TOO LARGE", newline ) )
                  ELSE print( ( fixed( y, -9, 4 ), newline ) )
       FI
OD
```

{{out}}

```txt

1 2 3 4 5 6 7 8 9 10 11
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
 322.0000
 136.7321
  41.4142
   6.0000

```



## ALGOL W

{{Trans|ALGOL 60}}

```algolw
begin 
  real y; real array a( 0 :: 10 );
  real procedure f( real value t ); 
    sqrt(abs(t))+5*t*t*t;
  for i:=0 until 10 do read( a(i) );
  r_format := "A"; r_w := 9; r_d := 4;
  for i:=10 step -1 until 0 do
    begin
       y:=f(a(i));
       if y > 400 then write( "TOO LARGE" )
                  else write( y );
    end
end.
```

{{out}}

```txt

1 2 3 4 5 6 7 8 9 10 11
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
 322.0000
 136.7320
  41.4142
   6.0000

```



## AutoIt


```AutoIt
; Trabb Pardo–Knuth algorithm
; by James1337 (autoit.de)
; AutoIt Version: 3.3.8.1

Local $S, $i, $y

Do
	$S = InputBox("Trabb Pardo–Knuth algorithm", "Please enter 11 numbers:", "1 2 3 4 5 6 7 8 9 10 11")
	If @error Then Exit
	$S = StringSplit($S, " ")
Until ($S[0] = 11)

For $i = 11 To 1 Step -1
	$y = f($S[$i])
	If ($y > 400) Then
		ConsoleWrite("f(" & $S[$i] & ") = Overflow!" & @CRLF)
	Else
		ConsoleWrite("f(" & $S[$i] & ") = " & $y & @CRLF)
	EndIf
Next

Func f($x)
	Return Sqrt(Abs($x)) + 5*$x^3
EndFunc
```

{{out}}

```txt
Input: "1 2 3 4 5 6 7 8 9 10 11"

f(11) = Overflow!
f(10) = Overflow!
f(9) = Overflow!
f(8) = Overflow!
f(7) = Overflow!
f(6) = Overflow!
f(5) = Overflow!
f(4) = 322
f(3) = 136.732050807569
f(2) = 41.4142135623731
f(1) = 6
```



## AWK


```AWK

# syntax: GAWK -f TRABB_PARDO-KNUTH_ALGORITHM.AWK
BEGIN {
    printf("enter 11 numbers: ")
    getline S
    n = split(S,arr," ")
    if (n != 11) {
      printf("%d numbers entered; S/B 11\n",n)
      exit(1)
    }
    for (i=n; i>0; i--) {
      x = f(arr[i])
      printf("f(%s) = %s\n",arr[i],(x>400) ? "too large" : x)
    }
    exit(0)
}
function abs(x) { if (x >= 0) { return x } else { return -x } }
function f(x) { return sqrt(abs(x)) + 5 * x ^ 3 }

```

{{out}}

```txt

enter 11 numbers: 1 2 3 -4 5 6 -7 8 9 10 11
f(11) = too large
f(10) = too large
f(9) = too large
f(8) = too large
f(-7) = -1712.35
f(6) = too large
f(5) = too large
f(-4) = -318
f(3) = 136.732
f(2) = 41.4142
f(1) = 6

```



## BASIC256


```BASIC256
dim s(11)
print 'enter 11 numbers'
for i = 0 to 10
   input i + ">" , s[i]
next i

for i = 10 to 0 step -1
   print "f(" + s[i] + ")=";
   x = f(s[i])
   if x > 400 then
      print "--- too large ---"
   else
      print x
   endif
next i
end

function f(n)
   return sqrt(abs(n))+5*n^3
end function
```

{{out}}

```txt
enter 11 numbers
0>-4
1>-3
2>-4
3>-2
4>-1
5>-
6>1
7>2
8>3
9>4
10>5
f(5)=--- too large ---
f(4)=322
f(3)=136.7320508
f(2)=41.4142136
f(1)=6
f(0)=0
f(-1)=-4
f(-2)=-38.5857864
f(-4)=-318
f(-3)=-133.2679492
f(-4)=-318
```



## C


```c

#include<math.h>
#include<stdio.h>

int
main ()
{
  double inputs[11], check = 400, result;
  int i;

  printf ("\nPlease enter 11 numbers :");

  for (i = 0; i < 11; i++)
    {
      scanf ("%lf", &inputs[i]);
    }

  printf ("\n\n\nEvaluating f(x) = |x|^0.5 + 5x^3 for the given inputs :");

  for (i = 10; i >= 0; i--)
    {
      result = sqrt (fabs (inputs[i])) + 5 * pow (inputs[i], 3);

      printf ("\nf(%lf) = ");

      if (result > check)
        {
          printf ("Overflow!");
        }

      else
        {
          printf ("%lf", result);
        }
    }

  return 0;
}

```

{{out}}

```txt
Please enter 11 numbers :10 -1 1 2 3 4 4.3 4.305 4.303 4.302 4.301



Evaluating f(x) = |x|^0.5 + 5x^3 for the given inputs :
f(3.000000) = 399.886300
f(3.000000) = Overflow!
f(3.000000) = Overflow!
f(3.000000) = Overflow!
f(3.000000) = 399.608644
f(3.000000) = 322.000000
f(3.000000) = 136.732051
f(3.000000) = 41.414214
f(3.000000) = 6.000000
f(6.000000) = -4.000000
f(3.000000) = Overflow!
```



## C++


```cpp

#include <iostream>
#include <cmath>
#include <vector>
#include <algorithm>
#include <iomanip>

int main( ) {
   std::vector<double> input( 11 ) , results( 11 ) ;
   std::cout << "Please enter 11 numbers!\n" ;
   for ( int i = 0 ; i < input.size( ) ; i++ ) 
      std::cin >> input[i];
      
   std::transform( input.begin( ) , input.end( ) , results.begin( ) ,
	 [ ]( double n )-> double { return sqrt( abs( n ) ) + 5 * pow( n , 3 ) ; } ) ;
   for ( int i = 10 ; i > -1 ; i-- ) {
      std::cout << "f( " << std::setw( 3 ) << input[ i ] << " ) : " ; 
      if ( results[ i ] > 400 ) 
	 std::cout << "too large!" ;
      else 
	 std::cout << results[ i ] << " !" ;
      std::cout << std::endl ;
   }
   return 0 ;
}
```

{{out}}

```txt
Please enter 11 numbers!
1
2
3
4
5
6
7
8
9
10
11
f(  11 ) : too large!
f(  10 ) : too large!
f(   9 ) : too large!
f(   8 ) : too large!
f(   7 ) : too large!
f(   6 ) : too large!
f(   5 ) : too large!
f(   4 ) : 322 !
f(   3 ) : 136.732 !
f(   2 ) : 41.4142 !
f(   1 ) : 6 !
```



## Common Lisp


```lisp
(defun read-numbers ()
  (princ "Enter 11 numbers (space-separated): ")
  (let ((numbers '()))
    (dotimes (i 11 numbers)
      (push (read) numbers))))

(defun trabb-pardo-knuth (func overflowp)
  (let ((S (read-numbers)))
    (format T "~{~a~%~}"
            (substitute-if "Overflow!" overflowp (mapcar func S)))))
                     
(trabb-pardo-knuth (lambda (x) (+ (expt (abs x) 0.5) (* 5 (expt x 3))))
                   (lambda (x) (> x 400)))
```


{{Out}}

```txt
Enter 11 numbers (space-separated): 10 -1 1 2 3 4 4.3 4.305 4.303 4.302 4.301
399.88635
Overflow!
Overflow!
Overflow!
399.6087
322.0
136.73206
41.414215
6.0
-4.0
Overflow!
```



## D


```d
import std.stdio, std.math, std.conv, std.algorithm, std.array;

double f(in double x) pure nothrow {
    return x.abs.sqrt + 5 * x ^^ 3;
}

void main() {
    double[] data;

    while (true) {
        "Please enter eleven numbers on a line: ".write;
        data = readln.split.map!(to!double).array;
        if (data.length == 11)
            break;
        writeln("Those aren't eleven numbers.");
    }
    foreach_reverse (immutable x; data) {
        immutable y = x.f;
        writefln("f(%0.3f) = %s", x, y > 400 ? "Too large" : y.text);
    }
}
```

{{out}}

```txt
Please enter eleven numbers on a line: 1 2 3 -4.55 5.1111 6 -7 8 9 10
Those aren't eleven numbers.
Please enter eleven numbers on a line: 1 2 3 -4.55 5.1111 6 -7 8 9 10 11
f(11.000) = Too large
f(10.000) = Too large
f(9.000) = Too large
f(8.000) = Too large
f(-7.000) = -1712.35
f(6.000) = Too large
f(5.111) = Too large
f(-4.550) = -468.849
f(3.000) = 136.732
f(2.000) = 41.4142
f(1.000) = 6
```



## EchoLisp


```scheme

(define (trabb-fun n)
		(+  (* 5 n n n) (sqrt(abs n))))
		
(define (check-trabb n)
	(if (number? n)
	(if (<=  (trabb-fun n) 400)
		(printf "🌱 f(%d) = %d" n (trabb-fun n))
		(printf "❌  f(%d) = %d" n (trabb-fun n)))
	(error "not a number" n)))
		
(define (trabb (numlist null))
	(while (< (length numlist) 11)
	(set! numlist (append numlist 
		(or
		(read default: (shuffle (iota 11)) 
		      prompt: (format "Please enter %d more numbers" (- 11 (length numlist))))
		(error 'incomplete-list numlist))))) ;; users cancel
	(for-each check-trabb (reverse (take numlist 11))))

```

{{out}}

```scheme

(trabb)
;; input :   (0 4 1 8 5 9 10 3 6 7 2)

🌱 f(2) = 41.41421356237309
❌ f(7) = 1717.6457513110645
❌ f(6) = 1082.4494897427833
🌱 f(3) = 136.73205080756887
❌ f(10) = 5003.162277660168
❌ f(9) = 3648
❌ f(5) = 627.2360679774998
❌ f(8) = 2562.828427124746
🌱 f(1) = 6
🌱 f(4) = 322
🌱 f(0) = 0

;; extra credit : let's find the threshold
(lib 'math)
(define (g x) (- (trabb-fun x) 400))
(root g 0 10)
    → 4.301409367213084

```



## Ela


Translation of OCaml version:


```ela
open monad io number string

:::IO

take_numbers 0 xs = do
  return $ iter xs
  where f x = sqrt (toSingle x) + 5.0 * (x ** 3.0)
        p x = x < 400.0
        iter [] = return ()
        iter (x::xs) 
          | p res = do
              putStrLn (format "f({0}) = {1}" x res)
              iter xs
          | else = do
              putStrLn (format "f({0}) :: Overflow" x)
              iter xs
          where res = f x
take_numbers n xs = do
  x <- readAny
  take_numbers (n - 1) (x::xs)

do
  putStrLn "Please enter 11 numbers:"
  take_numbers 11 []
```


{{out}}

```txt
Please enter 11 numbers:
1
2
3
4
5
6
7
8
9
10
11
f(11) :: Overflow
f(10) :: Overflow
f(9) :: Overflow
f(8) :: Overflow
f(7) :: Overflow
f(6) :: Overflow
f(5) :: Overflow
f(4) = 322
f(3) = 136.732050807569
f(2) = 41.4142135623731
f(1) = 6
```


## Elena

{{trans|C}}
ELENA 4.x :

```elena
import system'math;
import extensions;
 
public program()
{
    real[] inputs := new real[](11);
    console.printLine("Please enter 11 numbers :");
    for(int i := 0, i < 11, i += 1)
    {
        inputs[i] := console.readLine().toReal()
    };
 
    console.printLine("Evaluating f(x) = |x|^0.5 + 5x^3 for the given inputs :");
    for(int i := 10, i >= 0, i -= 1)
    {
        var r1 := inputs[i].Absolute.sqrt();
        var r2 := inputs[i].power(3);
        //var r :=inputs[i]/*absolute;*/.sqrt() + 5*r2;
 
        real result := (inputs[i].Absolute.sqrt()) + 5 * inputs[i].power(3);
 
        console.print("f(", inputs[i], ")=");
 
        if (result > 400)
        {
            console.printLine("Overflow!")
        }
        else
        {
            console.printLine(result)
        }
    }
}
```

{{out}}

```txt

Please enter 11 numbers :
1
2
3
4
5
6
7
8
9
10
11	
Evaluating f(x) = |x|^0.5 + 5x^3 for the given inputs :
f(11.0)=Overflow!
f(10.0)=Overflow!
f(9.0)=Overflow!
f(8.0)=Overflow!
f(7.0)=Overflow!
f(6.0)=Overflow!
f(5.0)=Overflow!
f(4.0)=322.0
f(3.0)=136.7320508076
f(2.0)=41.41421356237
f(1.0)=6.0

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Trabb_Pardo_Knuth do
  def task do
    Enum.reverse( get_11_numbers )
    |> Enum.each( fn x -> perform_operation( &function(&1), 400, x ) end )
  end
  
  defp alert( n ), do: IO.puts "Operation on #{n} overflowed"
  
  defp get_11_numbers do
    ns = IO.gets( "Input 11 integers.  Space delimited, please: " )
         |> String.split
         |> Enum.map( &String.to_integer &1 )
    if 11 == length( ns ), do: ns, else: get_11_numbers
  end
  
  defp function( x ), do: :math.sqrt( abs(x) ) + 5 * :math.pow( x, 3 )
  
  defp perform_operation( fun, overflow, n ), do: perform_operation_check_overflow( n, fun.(n), overflow )
  
  defp perform_operation_check_overflow( n, result, overflow ) when result > overflow, do: alert( n )
  defp perform_operation_check_overflow( n, result, _overflow ), do: IO.puts "f(#{n}) => #{result}"
end

Trabb_Pardo_Knuth.task
```


{{out}}

```txt

Input 11 integers.  Space delimited, please: 0 1 2 3 4 5 6 7 8 9 10
Operation on 10 overflowed
Operation on 9 overflowed
Operation on 8 overflowed
Operation on 7 overflowed
Operation on 6 overflowed
Operation on 5 overflowed
f(4) => 322.0
f(3) => 136.73205080756887
f(2) => 41.41421356237309
f(1) => 6.0
f(0) => 0.0

```



## Erlang


```Erlang

-module( trabb_pardo_knuth ).

-export( [task/0] ).

task() ->
	Sequence = get_11_numbers(),
	S = lists:reverse( Sequence ),
	[perform_operation( fun  function/1, 400, X) || X <- S].


alert( N ) -> io:fwrite( "Operation on ~p overflowed~n", [N] ).

get_11_numbers() ->
	{ok, Ns} = io:fread( "Input 11 integers.  Space delimited, please:  ", "~d ~d ~d ~d ~d ~d ~d  ~d ~d ~d ~d" ),
	11 = erlang:length( Ns ),
	Ns.

function( X ) -> math:sqrt( erlang:abs(X) ) + 5 * math:pow( X, 3 ).

perform_operation( Fun, Overflow, N ) -> perform_operation_check_overflow( N, Fun(N), Overflow ).

perform_operation_check_overflow( N, Result, Overflow ) when Result > Overflow -> alert( N );
perform_operation_check_overflow( N, Result, _Overflow ) -> io:fwrite( "f(~p) => ~p~n", [N, Result] ).

```

{{out}}

```txt

5> trabb_pardo_knuth:task().
Input 11 integers.  Space delimited, please:  1 2 3 4 5 6 7 8 9 10 11
Operation on 11 overflowed
Operation on 10 overflowed
Operation on 9 overflowed
Operation on 8 overflowed
Operation on 7 overflowed
Operation on 6 overflowed
Operation on 5 overflowed
f(4) => 322.0
f(3) => 136.73205080756887
f(2) => 41.41421356237309
f(1) => 6.0

```



## ERRE


```ERRE

!Trabb Pardo-Knuth algorithm
PROGRAM TPK
!VAR I%,Y
DIM A[10]

FUNCTION F(T)
    F=SQR(ABS(T))+5*T^3
END FUNCTION

BEGIN
  DATA(10,-1,1,2,3,4,4.3,4.305,4.303,4.302,4.301)
  FOR I%=0 TO 10 DO
      READ(A[I%])
  END FOR
  FOR I%=10 TO 0 STEP -1 DO
       Y=F(A[I%])
       PRINT("F(";A[I%];")=";)
       IF Y>400 THEN PRINT("--->too large<---")
                ELSE PRINT(Y)
       END IF
  END FOR
END PROGRAM

```

Numbers to be elaborated is included in the program with a DATA statement. You can substitute
this with an input keyboard like this

    FOR I%=0 TO 10 DO
     PRINT("Number #";I%;)
     INPUT(A[I%])
    END FOR

=={{header|F Sharp|F#}}==

```fsharp

module ``Trabb Pardo - Knuth``
open System
let f (x: float) = sqrt(abs x) + (5.0 * (x ** 3.0))
    
Console.WriteLine "Enter 11 numbers:"
[for _ in 1..11 -> Convert.ToDouble(Console.ReadLine())] 
|> List.rev |> List.map f |> List.iter (function
| n when n <= 400.0 -> Console.WriteLine(n) 
| _                 -> Console.WriteLine("Overflow")) 

```

{{out}}

```txt
fsharpi Program.fsx
[Loading Program.fsx]
Enter 11 numbers:
1
2
3
4
5
6
7
8
9
10
11
Overflow
Overflow
Overflow
Overflow
Overflow
Overflow
Overflow
322
136.732050807569
41.4142135623731
6

```



## Factor


```factor
USING: formatting io kernel math math.functions math.parser
prettyprint sequences splitting ;
IN: rosetta-code.trabb-pardo-knuth

CONSTANT: threshold 400
CONSTANT: prompt "Please enter 11 numbers: "

: fn ( x -- y ) [ abs 0.5 ^ ] [ 3 ^ 5 * ] bi + ;

: overflow? ( x -- ? ) threshold > ;

: get-input ( -- seq )
    prompt write flush readln " " split dup length 11 =
    [ drop get-input ] unless ;
    
: ?result ( ..a quot: ( ..a -- ..b ) -- ..b )
    [ "f(%u) = " sprintf ] swap bi dup overflow?
    [ drop "overflow" ] [ "%.3f" sprintf ] if append ; inline
    
: main ( -- )
    get-input reverse
    [ string>number [ fn ] ?result print ] each ;

MAIN: main
```

{{out}}

```txt

Please enter 11 numbers: 1 2 3
Please enter 11 numbers: 10 -1 1 2 3 4 4.3 4.305 4.303 4.302 4.301
f(4.301) = 399.886
f(4.302) = overflow
f(4.303) = overflow
f(4.305) = overflow
f(4.3) = 399.609
f(4) = 322.000
f(3) = 136.732
f(2) = 41.414
f(1) = 6.000
f(-1) = -4.000
f(10) = overflow

```



## Forth


```forth
: f(x)  fdup fsqrt fswap 3e f** 5e f* f+ ;

4e2 fconstant f-too-big

11 Constant #Elements

: float-array ( compile: n -- / run: n -- addr )
    create 
        floats allot
    does>
        swap floats + ;

#Elements float-array vec

: get-it  ( -- )
    ." Enter " #Elements . ." numbers:" cr
    #Elements 0 DO
        ." > " pad 25 accept cr
        pad swap >float 0= abort" Invalid Number"
        i vec F!
    LOOP ;

: reverse-it ( -- )
    #Elements 2/  0 DO
        i vec F@  #Elements i - 1- vec F@
        i vec F!  #Elements i - 1- vec F!
    LOOP ;

: do-it ( -- )
    #Elements 0 DO
        i vec F@ fdup f. [char] : emit space
	f(x) fdup f-too-big f> IF
            fdrop ." too large"
        ELSE
            f.
        THEN cr
    LOOP ;

: tpk  ( -- )
    get-it reverse-it do-it ;
```

{{out}}

```txt

Gforth 0.7.2, Copyright (C) 1995-2008 Free Software Foundation, Inc.
Gforth comes with ABSOLUTELY NO WARRANTY; for details type `license'
Type `bye' to exit
tpk Enter 11 numbers:
> 1 
> 2 
> 3 
> 4 
> 5 
> 6 
> 2.71828 
> 3.14159 
> 76 
> 7 
> 8 
8. : too large
7. : too large
76. : too large
3.14159 : 156.80344365595 
2.71828 : 102.07620267347 
6. : too large
5. : too large
4. : 322. 
3. : 136.732050807569 
2. : 41.4142135623731 
1. : 6. 
 ok
```



## Fortran


### Fortran 95

{{works with|Fortran|95 and later}}

```fortran
program tpk
  implicit none
  
  real, parameter :: overflow = 400.0
  real :: a(11), res
  integer :: i
 
  write(*,*) "Input eleven numbers:"
  read(*,*) a
 
  a = a(11:1:-1)
  do i = 1, 11
    res = f(a(i))
    write(*, "(a, f0.3, a)", advance = "no") "f(", a(i), ") = "
    if(res > overflow) then
      write(*, "(a)") "overflow!"
    else
       write(*, "(f0.3)") res
    end if
  end do  
      
contains

real function f(x)
  real, intent(in) :: x
  
  f = sqrt(abs(x)) + 5.0*x**3

end function  
end program
```

{{out}}

```txt
 Input eleven numbers:
10 -1 1 2 3 4 4.3 4.305 4.303 4.302 4.301
f(4.301) = 399.886
f(4.302) = overflow!
f(4.303) = overflow!
f(4.305) = overflow!
f(4.300) = 399.609
f(4.000) = 322.000
f(3.000) = 136.732
f(2.000) = 41.414
f(1.000) = 6.000
f(-1.000) = -4.000
f(10.000) = overflow!
```



### Fortran I

Written in FORTRAN I (1957), the original language quoted in the 1976 Donald Knuth & Luis Trabb Pardo’s study. Let’ note: no type declarations (INTEGER, REAL), no subprogram FUNCTION (only statement function), no logical IF, no END statement, and only Hollerith strings. The input data are on 2 80-column punched cards, only 1 to 72 columns are used so 6 values are read on the first card and 5 on the second card, so even input data could be numbered in the 73-80 area.

```fortran
C     THE TPK ALGORITH - FORTRAN I - 1957                               TPK00010
      FTPKF(X)=SQRTF(ABSF(X))+5.0*X**3                                  TPK00020
      DIMENSION A(11)                                                   TPK00030
      READ 100,A                                                        TPK00040
 100  FORMAT(6F12.4/)                                                   TPK00050
      DO 3 I=1,11                                                       TPK00060
      J=12-I                                                            TPK00070           
      Y=FTPKF(A(J))                                                     TPK00080
      IF (Y-400.0)2,2,1                                                 TPK00090
   1  PRINT 301,I,A(J)                                                  TPK00100
 301  FORMAT(I10,F12.7,18H *** TOO LARGE ***)                           TPK00110
      GO TO 10                                                          TPK00120
   2  PRINT 302,I,A(J),Y                                                TPK00130
 302  FORMAT(I10,2F12.7)                                                TPK00140
   3  CONTINUE                                                          TPK00150
      STOP 0                                                            TPK00160

```


## FreeBASIC


```freebasic
' version 22-07-2017
' compile with: fbc -s console

Function f(n As Double) As Double
    return Sqr(Abs(n)) + 5 * n ^ 3
End Function

' ------=< MAIN >=------

Dim As Double x, s(1 To 11)
Dim As Long i

For i = 1 To 11 
    Print Str(i);
    Input " => ", s(i)
Next

Print
Print String(20,"-")

i -= 1
Do
    Print "f(" + Str(s(i)) + ") = ";
   x = f(s(i))
   If x > 400 Then 
       Print "-=< overflow >=-"
   Else 
       Print x
   End If
   i -= 1
Loop Until i < 1

' empty keyboard buffer 
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

{{out}}

```txt
1 => -5
2 => -3
3 => -2
4 => -1
5 => 0
6 => 1
7 => 2
8 => 3
9 => 4
10 => 5
11 => 6

--------------------
f(6) = -=< overflow >=-
f(5) = -=< overflow >=-
f(4) =  322
f(3) =  136.7320508075689
f(2) =  41.41421356237309
f(1) =  6
f(0) =  0
f(-1) = -4
f(-2) = -38.58578643762691
f(-3) = -133.2679491924311
f(-5) = -622.7639320225002
```



## Go


### Task/Wikipedia

This solution follows the task description by reversing the sequence.  It also rejects non-numeric input until 11 numbers are entered.

```go
package main

import (
    "fmt"
    "log"
    "math"
)

func main() {
    // prompt
    fmt.Print("Enter 11 numbers: ")
    // accept sequence
    var s [11]float64
    for i := 0; i < 11; {
        if n, _ := fmt.Scan(&s[i]); n > 0 {
            i++
        }
    }
    // reverse sequence
    for i, item := range s[:5] {
        s[i], s[10-i] = s[10-i], item
    }
    // iterate
    for _, item := range s {
        if result, overflow := f(item); overflow {
            // send alerts to stderr
            log.Printf("f(%g) overflow", item)
        } else {
            // send normal results to stdout
            fmt.Printf("f(%g) = %g\n", item, result)
        }
    }
}

func f(x float64) (float64, bool) {
    result := math.Sqrt(math.Abs(x)) + 5*x*x*x
    return result, result > 400
}
```

{{out}}
The input is chosen to show some interesting boundary cases.

```txt

Enter 11 numbers: 0 1 4.3 4.4 -1 -5 non-number -1e102 -1e103 -Inf Inf NaN 
f(NaN) = NaN
2016/04/15 18:38:29 f(+Inf) overflow
f(-Inf) = NaN
f(-1e+103) = -Inf
f(-1e+102) = -5e+306
f(-5) = -622.7639320225002
f(-1) = -4
2016/04/15 18:38:29 f(4.4) overflow
f(4.3) = 399.6086441353327
f(1) = 6
f(0) = 0

```


### TPK paper

The original paper had no requirement to reverse the sequence in place, but instead processed the sequence in reverse order.

```go
package main

import (
    "fmt"
    "math"
)

func f(t float64) float64 {
    return math.Sqrt(math.Abs(t)) + 5*math.Pow(t, 3)
}

func main() {
    var a [11]float64
    for i := range a {
        fmt.Scan(&a[i])
    }
    for i := len(a) - 1; i >= 0; i-- {
        if y := f(a[i]); y > 400 {
            fmt.Println(i, "TOO LARGE")
        } else {
            fmt.Println(i, y)
        }
    }
}
```



## Haskell


```Haskell
import Control.Monad (replicateM, mapM_)

f :: Floating a => a -> a
f x = sqrt (abs x) + 5 * x ** 3

main :: IO ()
main = do
  putStrLn "Enter 11 numbers for evaluation"
  x <- replicateM 11 readLn
  mapM_
    ((\x ->
         if x > 400
           then putStrLn "OVERFLOW"
           else print x) .
     f) $
    reverse x
```

{{out}}

```txt
Enter 11 numbers for evaluation
1
2
3
4
5
6
7
8
9
10
11
OVERFLOW
OVERFLOW
OVERFLOW
OVERFLOW
OVERFLOW
OVERFLOW
OVERFLOW
322.0
136.73205080756887
41.41421356237309
6.0

```


=={{header|Icon}} and {{header|Unicon}}==

The following Unicon-specific solution can be implemented in Icon by replaces
<tt>reverse(S)</tt> with <tt>S[*S to 1 by -1]</tt>.


```unicon
procedure main()
    S := []
    writes("Enter 11 numbers: ")
    read() ? every !11 do (tab(many(' \t'))|0,put(S, tab(upto(' \t')|0)))
    every item := !reverse(S) do
        write(item, " -> ", (400 >= f(item)) | "overflows")
end

procedure f(x)
   return abs(x)^0.5 + 5*x^3
end
```


Sample run:


```txt

->tpk
Enter 11 numbers: 1 2 3 4 5 6 7 8 9 10 11
11 -> overflows
10 -> overflows
9 -> overflows
8 -> overflows
7 -> overflows
6 -> overflows
5 -> overflows
4 -> 322.0
3 -> 136.7320508075689
2 -> 41.41421356237309
1 -> 6.0
->

```



## Io



```Io

// Initialize objects to be used
in_num := File standardInput()
nums := List clone
result := Number

// Prompt the user and get numbers from standard input
"Please enter 11 numbers:" println
11 repeat(nums append(in_num readLine() asNumber()))

// Reverse the numbers received
nums reverseInPlace

// Apply the function and tell the user if the result is above
// our limit. Otherwise, tell them the result.
nums foreach(v,
  // v needs parentheses around it for abs to properly convert v to its absolute value
  result = (v) abs ** 0.5 + 5 * v ** 3
  if (result > 400,
    "Overflow!" println
  ,
     result println
  )
)

```


{{out}}

```txt

io tpk.io
Please enter 11 numbers:
1
2
3
4
5
6
7
8
9
10
11
Overflow!
Overflow!
Overflow!
Overflow!
Overflow!
Overflow!
Overflow!
322
136.7320508075688679
41.4142135623730923
6

```



## J

Input and output in J is done using "foreigns", in this case it is reading from the keyboard. 
The calculations are straightforward and applied to the whole set simultaneously. 
Similarly, overflow detection and changing the value to 'user alert' is also done once for all values.

No checks are done if the input is actually numbers and if there are actually eleven of them. This doesn't affect the algorithm. 
Additional checks can be done separately.

```J
tpk=: 3 :0
  smoutput 'Enter 11 numbers: '
  t1=: ((5 * ^&3) + (^&0.5@* *))"0 |. _999&".;._1 ' ' , 1!:1 [ 1
  smoutput 'Values of functions of reversed input: ' , ": t1
  ; <@(,&' ')@": ` ((<'user alert ')&[) @. (>&400)"0 t1
)
```

A possible use scenario:

```J
   tpk ''
Enter 11 numbers: 
1 2 3 4 5 6 7 8.8 _9 10.123 0
Values of functions of reversed input: 0 5189.96 _3642 3410.33 1717.65 1082.45 627.236 322 136.732 41.4142 6
0 user alert _3642 user alert user alert user alert user alert 322 136.732 41.4142 6 

```


Note that the result of tpk is persisted in t1 and is also its explicit result rather than being an explicit output.

Here's an alternative approach:


```J
get11numbers=: 3 :0
  smoutput 'Enter 11 numbers: '
  _&". 1!:1]1
)

f_x=: %:@| + 5 * ^&3

overflow400=: 'user alert'"_`":@.(<:&400)"0

tpk=: overflow400@f_x@|.@get11numbers
```


And, here's this alternative in action:


```J
   tpk''
Enter 11 numbers: 
1 2 3 4 5 6 7 8.8 _9 10.123 0
0         
user alert
_3642     
user alert
user alert
user alert
user alert
322       
136.732   
41.4142   
6
```


(clearly, other alternatives are also possible).

Note that no error is reported if something other than 11 numbers are provided, since it's not clear what should be done for that case -- we just process all of them.


## Java



```java
/**
 * Alexander Alvonellos 
 */
import java.util.*;
import java.io.*; 

public class TPKA {
	public static void main(String... args) {
		double[] input = new double[11];
		double userInput = 0.0;
		Scanner in = new Scanner(System.in);
		for(int i = 0; i < 11; i++) {
			System.out.print("Please enter a number: ");
			String s = in.nextLine();
			try {
				userInput = Double.parseDouble(s);
			} catch (NumberFormatException e) { 
				System.out.println("You entered invalid input, exiting");
				System.exit(1);
			}
			input[i] = userInput;
		}
		for(int j = 10; j >= 0; j--) {
			double x = input[j]; double y = f(x);
			if( y < 400.0) {
				System.out.printf("f( %.2f ) = %.2f\n", x, y);
			} else {
				System.out.printf("f( %.2f ) = %s\n", x, "TOO LARGE");
			}
		}
	}

	private static double f(double x) {
		return Math.pow(Math.abs(x), 0.5) + (5*(Math.pow(x, 3)));
	}
}

```


{{out}}

```txt

Please enter a number: 1
Please enter a number: 2
Please enter a number: 3
Please enter a number: 4
Please enter a number: 5
Please enter a number: 6
Please enter a number: 7
Please enter a number: 8
Please enter a number: 9
Please enter a number: 10
Please enter a number: 11
f( 11.00 ) = TOO LARGE
f( 10.00 ) = TOO LARGE
f( 9.00 ) = TOO LARGE
f( 8.00 ) = TOO LARGE
f( 7.00 ) = TOO LARGE
f( 6.00 ) = TOO LARGE
f( 5.00 ) = TOO LARGE
f( 4.00 ) = 322.00
f( 3.00 ) = 136.73
f( 2.00 ) = 41.41
f( 1.00 ) = 6.00

```



## JavaScript



###  Spidermonkey 


```javascript
#!/usr/bin/env js

function main() {
    var nums = getNumbers(11);
    nums.reverse();
    for (var i in  nums) {
        pardoKnuth(nums[i], fn, 400);
    }
}

function pardoKnuth(n, f, max) {
    var res = f(n);
    putstr('f(' + String(n) + ')');
    if (res > max) {
        print(' is too large');
    } else {
        print(' = ' + String(res));
    } 
}

function fn(x) {
    return Math.pow(Math.abs(x), 0.5) + 5 * Math.pow(x, 3);
}

function getNumbers(n) {
    var nums = [];
    print('Enter', n, 'numbers.');
    for (var i = 1; i <= n; i++) {
        putstr('   ' + i + ': ');
        var num = readline();
        nums.push(Number(num));        
    }
    return nums;
}

main();

```


Results:
 Enter 11 numbers.
    1: 1
    2: 2
    3: 3
    4: 4
    5: 5
    6: 6
    7: 7
    8: 8
    9: 9
    10: 10
    11: 11
 f(11)  is too large
 f(10)  is too large
 f(9)  is too large
 f(8)  is too large
 f(7)  is too large
 f(6)  is too large
 f(5)  is too large
 f(4) = 322
 f(3) = 136.73205080756887
 f(2) = 41.41421356237309
 f(1) = 6


## jq

jq does not currently have an interactive mode allowing a prompt to be issued first,
and so the initial prompt is implemented here using "echo", in keeping with the jq approach of dovetailing with other command-line tools. 

```jq
def f:
  def abs: if . < 0 then -. else . end; 
  def power(x): (x * log) | exp;
  . as $x | abs | power(0.5) + (5 * (.*.*. ));

. as $in | split(" ") | map(tonumber)
| if length == 11 then 
    reverse | map(f | if . > 400 then "TOO LARGE" else . end)
  else error("The number of numbers was not 11.")
  end
| .[]  # print one result per line
```

{{out}}

```sh
$ echo "Enter 11 numbers on one line; when done, enter the end-of-file character:" ;\
jq -M -s -R -f Trabb_Pardo-Knuth_algorithm.jq
> Enter 11 numbers on one line; when done, enter the end-of-file character:
1 2 3 4 5 6 7 8 9 10 11
"TOO LARGE"
"TOO LARGE"
"TOO LARGE"
"TOO LARGE"
"TOO LARGE"
"TOO LARGE"
"TOO LARGE"
322
136.73205080756887
41.41421356237309
6
```



## Kotlin


```scala
// version 1.1.2

fun f(x: Double) = Math.sqrt(Math.abs(x)) + 5.0 * x * x * x
 
fun main(args: Array<String>) {
    val da = DoubleArray(11)
    println("Please enter 11 numbers:")
    var i = 0
    while (i < 11) {
        print("  ${"%2d".format(i + 1)}: ")
        val d = readLine()!!.toDoubleOrNull()
        if (d == null)
            println("Not a valid number, try again")
        else
            da[i++] = d
    }
    println("\nThe sequence you just entered in reverse is:")
    da.reverse()
    println(da.contentToString())
    println("\nProcessing this sequence...")
    for (j in 0..10) {
        val v = f(da[j])
        print("  ${"%2d".format(j + 1)}: ") 
        if (v > 400.0) 
            println("Overflow!")
        else
            println(v)
    }
}
```


{{out}}
Sample session:

```txt

Please enter 11 numbers:
   1: 10
   2: -1
   3: 1
   4: 2
   5: 3
   6: 4
   7: 4.3
   8: 4.305
   9: 4.303
  10: 4.302
  11: 4.301

The sequence you just entered in reverse is:
[4.301, 4.302, 4.303, 4.305, 4.3, 4.0, 3.0, 2.0, 1.0, -1.0, 10.0]

Processing this sequence...
   1: 399.88629974772687
   2: Overflow!
   3: Overflow!
   4: Overflow!
   5: 399.6086441353327
   6: 322.0
   7: 136.73205080756887
   8: 41.41421356237309
   9: 6.0
  10: -4.0
  11: Overflow!

```



## Julia


```julia
f(x) = abs(x)^.5 + 5x^3
for i in map(parseint,reverse(split(readline())))
    v = f(i)
    println("$i: ", v > 400 ? "TOO LARGE" : v)
end
```

{{out}}

```txt
1 2 3 4 5 6 7 8 9 10 11
11: TOO LARGE
10: TOO LARGE
9: TOO LARGE
8: TOO LARGE
7: TOO LARGE
6: TOO LARGE
5: TOO LARGE
4: 322.0
3: 136.73205080756887
2: 41.41421356237309
1: 6.0
```



## Lua


### Implementation of task description


```Lua
function f (x) return math.abs(x)^0.5 + 5*x^3 end

function reverse (t)
    local rev = {}
    for i, v in ipairs(t) do rev[#t - (i-1)] = v end
    return rev
end

local sequence, result = {}
print("Enter 11 numbers...")
for n = 1, 11 do
    io.write(n .. ": ")
    sequence[n] = io.read()
end
for _, x in ipairs(reverse(sequence)) do
    result = f(x)
    if result > 400 then print("Overflow!") else print(result) end
end
```

{{out}}

```txt
Enter 11 numbers...
1: 1
2: 2
3: 3
4: 4
5: 5
6: 6
7: 7
8: 8
9: 9
10: 10
11: 11
Overflow!
Overflow!
Overflow!
Overflow!
Overflow!
Overflow!
Overflow!
322
136.73205080757
41.414213562373
6
```


===Line-for-line from TPK paper===

```Lua
local a, y = {}
function f (t)
    return math.sqrt(math.abs(t)) + 5*t^3
end
for i = 0, 10 do a[i] = io.read() end
for i = 10, 0, -1 do
    y = f(a[i])
    if y > 400 then print(i, "TOO LARGE")
               else print(i, y) end
end
```

{{out}}

```txt
1
2
3
4
5
6
7
8
9
10
11
10      TOO LARGE
9       TOO LARGE
8       TOO LARGE
7       TOO LARGE
6       TOO LARGE
5       TOO LARGE
4       TOO LARGE
3       322
2       136.73205080757
1       41.414213562373
0       6
```




## M2000 Interpreter


```M2000 Interpreter

Module Input11 {
      Flush ' empty stack
      For I=1 to 11 {
            Input "Give me a number ", a
            Data a   ' add to bottom of stack, use: Push a to add to top, to get reverse order here
      }
}
Module Run {
      Print "Trabb Pardo–Knuth algorithm"
      Print "f(x)=Sqrt(Abs(x))+5*x^3"
      if not match("NNNNNNNNN") then Error "Need 11 numbers" 
      Shiftback 1, -11 ' reverse  order 11 elements of stack of values
      Def f(x)=Sqrt(Abs(x))+5*x^3
      For i=1 to 11 {
            Read pop
            y=f(pop)
            if y>400 Then {
                  Print format$("f({0}) = Overflow!", pop)
            }  Else {
                  Print format$("f({0}) = {1}", pop, y)
            }
      }
}
Run 10, -1, 1, 2, 3, 4, 4.3, 4.305, 4.303, 4.302, 4.301
Run 1, 2, 3, -4.55,5.1111, 6, -7, 8, 9, 10, 11
Input11
Run

```


To collect the output in clipboard. Global variables need <= to assign values, and document append values using = or <= (for globals)

Output with "," for decimals (Locale 1032). We can change this using statement Locale 1033


```M2000 Interpreter

Global a$
Document a$  ' make a$ as a document - string with paragraphs
Module Run {
      a$<={Trabb Pardo–Knuth algorithm
            f(x)=Sqrt(Abs(x))+5*x^3
            }
      if not match("NNNNNNNNN") then Error "Need 11 numbers" 
      Shiftback 1, -11 ' reverse  order 11 elements of stack of values
      Def f(x)=Sqrt(Abs(x))+5*x^3
      For i=1 to 11 {
            Read pop
            y=f(pop)
            if y>400 Then {
                 a$<=format$("f({0}) = Overflow!", pop)+{
                  }
            }  Else {
                  a$<=format$("f({0}) = {1}", pop, y)+{
                  }
            }
      }
}
Run 10, -1, 1, 2, 3, 4, 4.3, 4.305, 4.303, 4.302, 4.301
Run 1, 2, 3, -4.55,5.1111, 6, -7, 8, 9, 10, 11
Clipboard a$

```


{{out}}
<pre style="height:30ex;overflow:scroll">
Trabb Pardo–Knuth algorithm
f(x)=Sqrt(Abs(x))+5*x^3
f(4,301) = 399,886299747727
f(4,302) = Overflow!
f(4,303) = Overflow!
f(4,305) = Overflow!
f(4,3) = 399,608644135333
f(4) = 322
f(3) = 136,732050807569
f(2) = 41,4142135623731
f(1) = 6
f(-1) = -4
f(10) = Overflow!
Trabb Pardo–Knuth algorithm
f(x)=Sqrt(Abs(x))+5*x^3
f(11) = Overflow!
f(10) = Overflow!
f(9) = Overflow!
f(8) = Overflow!
f(-7) = -1712,35424868894
f(6) = Overflow!
f(5,1111) = Overflow!
f(-4,55) = -468,84880209923
f(3) = 136,732050807569
f(2) = 41,4142135623731
f(1) = 6
</pre >


## Maple


```Maple
seqn := ListTools:-Reverse([parse(Maplets[Display](Maplets:-Elements:-Maplet(Maplets:-Elements:-InputDialog['ID1']("Enter a sequence of numbers separated by comma", 'onapprove' = Maplets:-Elements:-Shutdown(['ID1']), 'oncancel' = Maplets:-Elements:-Shutdown())))[1])]):
f:= x -> abs(x)^0.5 + 5*x^3:
for item in seqn do
	result := f(item):
	if (result > 400) then
		print("Alert: Overflow."):
	else
		print(result):
	end if:
end do:
```

{{Out|Usage}}
Input:1,2,3,4,5,6,7,8,9,10,11

```txt
                       "Alert: Overflow."
                       "Alert: Overflow."
                       "Alert: Overflow."
                       "Alert: Overflow."
                       "Alert: Overflow."
                       "Alert: Overflow."
                       "Alert: Overflow."
                          322.0000000
                          136.7320508
                          41.41421356
                               6.
```




## Mathematica


```Mathematica
numbers=RandomReal[{-2,6},11]
tpk[numbers_,overflowVal_]:=Module[{revNumbers},
 revNumbers=Reverse[numbers];
 f[x_]:=Abs[x]^0.5+5 x^3;
 Do[
  If[f[i]>overflowVal,
   Print["f[",i,"]= Overflow"]
  ,
   Print["f[",i,"]= ",f[i]]
  ]
  ,
  {i,revNumbers}
 ]
]
tpk[numbers,400]
```

{{out}}

```txt

{0.470145,1.18367,2.36984,4.86759,2.40274,5.48793,3.30256,5.34393,4.21944,2.23501,-0.0200707}
f[-0.0200707]= 0.141631
f[2.23501]= 57.3176
f[4.21944]= 377.663
f[5.34393]= Overflow
f[3.30256]= 181.921
f[5.48793]= Overflow
f[2.40274]= 70.9068
f[4.86759]= Overflow
f[2.36984]= 68.0859
f[1.18367]= 9.38004
f[0.470145]= 1.20527

```



## min

{{works with|min|0.19.3}}

```min
((0 <) (-1 *) when) :abs
(((abs 0.5 pow) (3 pow 5 * +)) cleave) :fn
"Enter 11 numbers:" puts!
(gets float) 11 times
(fn (400 <=) (pop "Overflow") unless puts!) 11 times
```

{{out}}

```txt

Enter 11 numbers:
1
2
3
4
5
6
7
8
9
10
11
Overflow
Overflow
Overflow
Overflow
Overflow
Overflow
Overflow
322.0
136.7320508075689
41.41421356237309
6.0

```



## Nim

{{trans|Python}}

```nim
import math, rdstdin, strutils, algorithm

proc f(x): float = x.abs.pow(0.5) + 5 * x.pow(3)

proc ask: seq[float] =
  readLineFromStdin("\n11 numbers: ").strip.split[0..10].map(parseFloat)

var s = ask()
reverse s
for x in s:
  let result = f x
  stdout.write " ",x,":", if result > 400: "TOO LARGE!" else: $result
echo ""
```

{{out}}

```txt
11 numbers: 1 2 3 4 5 6 7 8 9 10 11
 11.0:TOO LARGE! 10.0:TOO LARGE! 9.0:TOO LARGE! 8.0:TOO LARGE! 7.0:TOO LARGE! 6.0:TOO LARGE! 5.0:TOO LARGE! 4.0:322.0 3.0:136.7320508075689 2.0:41.41421356237309 1.0:6.0
```


=={{header|Objective-C}}==
{{works with|Mac OS X|10.6+}}


```objc
//
//  TPKA.m
//  RosettaCode
//
//  Created by Alexander Alvonellos on 5/26/12.
//  Trabb Pardo-Knuth algorithm
//

#import <Foundation/Foundation.h>
double f(double x);

double f(double x) {
    return pow(abs(x), 0.5) + 5*(pow(x, 3)); 
}

int main (int argc, const char * argv[])
{
    @autoreleasepool {
        NSMutableArray *input = [[NSMutableArray alloc] initWithCapacity:0];

        printf("%s", "Instructions: please enter 11 numbers.\n");
        for(int i = 0; i < 11; i++) {
            double userInput = 0.0;
            printf("%s", "Please enter a number: ");
            scanf("%lf", &userInput);
            [input addObject: @(userInput)];
        }
        
        for(int i = 10; i >= 0; i--) {
            double x = [input[i] doubleValue];
            double y = f(x);
            printf("f(%.2f) \t=\t", x);
            if(y < 400.0) {
                printf("%.2f\n", y);
            } else {
                printf("%s\n", "TOO LARGE");
            }
        }
    }
    return 0;
}

```


{{out}}

```txt

Instructions: please enter 11 numbers.
Please enter a number: 1
Please enter a number: 2
Please enter a number: 3
Please enter a number: 4
Please enter a number: 5
Please enter a number: 6
Please enter a number: 7
Please enter a number: 8
Please enter a number: 9
Please enter a number: 10
Please enter a number: 11
f(11.00) 	=	TOO LARGE
f(10.00) 	=	TOO LARGE
f(9.00) 	=	TOO LARGE
f(8.00) 	=	TOO LARGE
f(7.00) 	=	TOO LARGE
f(6.00) 	=	TOO LARGE
f(5.00) 	=	TOO LARGE
f(4.00) 	=	322.00
f(3.00) 	=	136.73
f(2.00) 	=	41.41
f(1.00) 	=	6.00

```



## OCaml



```ocaml
let f x = sqrt x +. 5.0 *. (x ** 3.0)
let p x = x < 400.0

let () =
  print_endline "Please enter 11 Numbers:";
  let lst = Array.to_list (Array.init 11 (fun _ -> read_float ())) in
  List.iter (fun x ->
    let res = f x in
    if p res
    then Printf.printf "f(%g) = %g\n%!" x res
    else Printf.eprintf "f(%g) :: Overflow\n%!" x
  ) (List.rev lst)
```


{{out}}

```txt
$ ocaml trabb_pardo_knuth.ml
Please enter 11 Numbers:
1
2
3
4
5
6
7
8
9
10
11
f(11) :: Overflow
f(10) :: Overflow
f(9) :: Overflow
f(8) :: Overflow
f(7) :: Overflow
f(6) :: Overflow
f(5) :: Overflow
f(4) = 322
f(3) = 136.732
f(2) = 41.4142
f(1) = 6
```


We output error messages on stderr. 
We flush outputs with <code>"%!"</code> 
so that results and error messages do not appear separated.


## PARI/GP


```parigp
{
  print("11 numbers: ");
  v=vector(11, n, eval(input()));
  v=apply(x->x=sqrt(abs(x))+5*x^3;if(x>400,"overflow",x), v);
  vector(11, i, v[12-i])
}
```

{{out}}

```txt
11 numbers:
1
2
3
4
5
6
7
8
9
10
11
%1 = ["overflow", "overflow", "overflow", "overflow", "overflow", "overflow",
"overflow", 322.0000000000000000000000000, 136.7320508075688772935274463, 41.414
21356237309504880168872, 6.000000000000000000000000000]
```



## Perl


```Perl
print "Enter 11 numbers:\n";
for ( 1..11 ) {
   $number = <STDIN>;
   chomp $number;
   push @sequence, $number;
}

for $n (reverse @sequence) {
   my $result = sqrt( abs($n) ) + 5 * $n**3;
   printf "f( %6.2f ) %s\n", $n, $result > 400 ? "  too large!" : sprintf "= %6.2f", $result
}
```

{{out}}

```txt

Enter 11 numbers:
2
1.2
3
3.4
4
4.5
5 
7.8
2.7
13
11.2
f(  11.20 )   too large!
f(  13.00 )   too large!
f(   2.70 ) = 100.06
f(   7.80 )   too large!
f(   5.00 )   too large!
f(   4.50 )   too large!
f(   4.00 ) = 322.00
f(   3.40 ) = 198.36
f(   3.00 ) = 136.73
f(   1.20 ) =   9.74
f(   2.00 ) =  41.41
```



## Perl 6


```perl6
my @nums = prompt("Please type 11 space-separated numbers: ").words
    until @nums == 11;
for @nums.reverse -> $n {
    my $r = $n.abs.sqrt + 5 * $n ** 3;
    say "$n\t{ $r > 400 ?? 'Urk!' !! $r }";
}
```

{{out}}

```txt
Please type 11 space-separated numbers: 10 -1 1 2 3 4 4.3 4.305 4.303 4.302 4.301
4.301	399.88629974772681
4.302	Urk!
4.303	Urk!
4.305	Urk!
4.3	399.60864413533278
4	322
3	136.73205080756887
2	41.414213562373092
1	6
-1	-4
10	Urk!
```


## Phix


```Phix
function f(atom x)
    return sqrt(abs(x))+5*power(x,3)
end function

string s = substitute(prompt_string("Enter 11 numbers:"),","," ")
sequence S = scanf(s,"%f %f %f %f %f %f %f %f %f %f %f")
if length(S)!=1 then puts(1,"not 11 numbers") abort(0) end if
S = reverse(S[1])
for i=1 to length(S) do
    atom result = f(S[i])
    if result>400 then
        printf(1,"f(%g):overflow\n",{S[i]})
    else
        printf(1,"f(%g):%g\n",{S[i],result})
    end if
end for
```

{{Out}}

```txt

Enter 11 numbers:10 -1 1 2 3 4 4.3 4.305 4.303 4.302 4.301
f(4.301):399.886
f(4.302):overflow
f(4.303):overflow
f(4.305):overflow
f(4.3):399.609
f(4):322
f(3):136.732
f(2):41.4142
f(1):6
f(-1):-4
f(10):overflow

Enter 11 numbers:1,2,3,4,5,6,7,8,9,10,11
f(11):overflow
f(10):overflow
f(9):overflow
f(8):overflow
f(7):overflow
f(6):overflow
f(5):overflow
f(4):322
f(3):136.732
f(2):41.4142
f(1):6

Enter 11 numbers:0.470145,1.18367,2.36984,4.86759,2.40274,5.48793,3.30256,5.34393,4.21944,2.23501,-0.0200707
f(-0.0200707):0.141631
f(2.23501):57.3174
f(4.21944):377.662
f(5.34393):overflow
f(3.30256):181.921
f(5.48793):overflow
f(2.40274):70.9071
f(4.86759):overflow
f(2.36984):68.0862
f(1.18367):9.38002
f(0.470145):1.20527

```



## PicoLisp


```PicoLisp
(de f (X)
   (+ (sqrt (abs X)) (* 5 X X X)) )

(trace 'f)

(in NIL
   (prin "Input 11 numbers: ")
   (for X (reverse (make (do 11 (link (read)))))
      (when (> (f X) 400)
         (prinl "TOO LARGE") ) ) )
```

Test:

```PicoLisp
Input 11 numbers: 1 2 3 4 5 6 7 8 9 10 11
 f : 11
 f = 6658
TOO LARGE
 f : 10
 f = 5003
TOO LARGE
 f : 9
 f = 3648
TOO LARGE
 f : 8
 f = 2562
TOO LARGE
 f : 7
 f = 1717
TOO LARGE
 f : 6
 f = 1082
TOO LARGE
 f : 5
 f = 627
TOO LARGE
 f : 4
 f = 322
 f : 3
 f = 136
 f : 2
 f = 41
 f : 1
 f = 6
```



## PL/I


```PL/I

Trabb: Procedure options (main); /* 11 November 2013 */

   declare (i, n) fixed binary;
   declare s fixed (5,1) controlled;
   declare g fixed (15,5);

   put ('Please type 11 values:');
   do i = 1 to 11;
      allocate s;
      get (s);
      put (s);
   end;
   put skip(2) ('Results:');
   do i = 1 to 11;
      g = f(s); put skip list (s);
      if g > 400 then put ('Too large'); else put (g);
      free s;
   end;

f: procedure (x) returns (fixed(15,5));
   declare x fixed (5,1);
   return (sqrt(abs(x)) + 5*x**3);
end f;

end Trabb;

```

{{out}}

```txt

Please type 11 values: 
     1.0 
     3.0 
     2.0 
    -4.0 
    -5.0 
     6.0 
     7.0 
     9.0 
    11.0 
     1.5 
     2.4 

Results: 
     2.4                          70.66920 
     1.5                          18.09974 
    11.0                Too large 
     9.0                Too large 
     7.0                Too large 
     6.0                Too large 
    -5.0                        -622.76391 
    -4.0                        -318.00000 
     2.0                          41.41421 
     3.0                         136.73205 
     1.0                           6.00000 

```



## PL/M

Assuming the existence of suitable external library routines.

```plm
TPK: DO;
    /* external I/O and real mathematical routines */
    WRITE$STRING: PROCEDURE( S )      EXTERNAL; DECLARE S POINTER; END;
    WRITE$REAL:   PROCEDURE( R )      EXTERNAL; DECLARE R REAL;    END;
    WRITE$NL:     PROCEDURE           EXTERNAL;                    END;
    READ$REAL:    PROCEDURE( R ) REAL EXTERNAL; DECLARE R POINTER; END;
    REAL$ABS:     PROCEDURE( R ) REAL EXTERNAL; DECLARE R REAL;    END;
    REAL$SQRT:    PROCEDURE( R ) REAL EXTERNAL; DECLARE R REAL;    END;
    /* end external routines */

    F: PROCEDURE( T ) REAL;
        DECLARE T REAL;
        RETURN REAL$SQRT(REAL$ABS(T))+5*T*T*T;
    END F;
    MAIN: PROCEDURE;
        DECLARE Y REAL, A( 11 ) REAL, I INTEGER;
        DO I = 0 TO 10;
           CALL READ$REAL( @A( I ) );
        END;
        DO I = 10 TO 0 BY -1;
           Y = F( A( I ) );
           IF Y > 400.0 THEN CALL WRITE$STRING( @( 'TOO LARGE', 0 ) );
                        ELSE CALL WRITE$REAL( Y );
           CALL WRITE$NL();
        END;
    END MAIN;

END TPK;
```

{{out}}

```txt

1 2 3 4 5 6 7 8 9 10 11
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
TOO LARGE
322.0000
136.7321
 41.4142
  6.0000

```



## PowerShell


```PowerShell

function Get-Tpk
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        [double]
        $Number
    )

    Begin
    {
        function Get-TpkFunction ([double]$Number)
        {
            [Math]::Pow([Math]::Abs($Number),(0.5)) + 5 * [Math]::Pow($Number,3)
        }

        [object[]]$output = @()
    }
    Process
    {
        $Number | ForEach-Object {
            $n = Get-TpkFunction $_

            if ($n -le 400)
            {
                $result = $n
            }
            else
            {
                $result = "Overflow"
            }
        }

        $output += [PSCustomObject]@{
            Number = $Number
            Result = $result
        }
    }
    End
    {
        [Array]::Reverse($output)
        $output
    }
}

```


```PowerShell

$tpk = 1..11 | Get-Tpk
$tpk

```

{{Out}}

```txt

Number Result          
------ ------          
    11 Overflow        
    10 Overflow        
     9 Overflow        
     8 Overflow        
     7 Overflow        
     6 Overflow        
     5 Overflow        
     4 322             
     3 136.732050807569
     2 41.4142135623731
     1 6               

```

Sort back to ascending order ignoring '''Overflow''' results:

```PowerShell

$tpk | where result -ne overflow | sort number

```

{{Out}}

```txt

Number           Result
------           ------
     1                6
     2 41.4142135623731
     3 136.732050807569
     4              322

```



## PureBasic


```purebasic
Procedure.d f(x.d)
  ProcedureReturn Pow(Abs(x), 0.5) + 5 * x * x * x
EndProcedure

Procedure split(i.s, delimeter.s, List o.d())
  Protected index = CountString(i, delimeter) + 1 ;add 1 because last entry will not have a delimeter
  
  While index > 0
    AddElement(o())
    o() = ValD(Trim(StringField(i, index, delimeter)))
    index - 1
  Wend

  ProcedureReturn ListSize(o())
EndProcedure

Define i$, entriesAreValid = 0, result.d, output$
NewList numbers.d()

If OpenConsole()
  Repeat 
    PrintN(#crlf$ + "Enter eleven numbers that are each separated by spaces or commas:")
    
    i$ = Input(
    i$ = Trim(i$)
    If split(i$, ",", numbers.d()) < 11
      ClearList(numbers())
      If split(i$, " ", numbers.d()) < 11
        PrintN("Not enough numbers were supplied.")
        ClearList(numbers())
      Else 
        entriesAreValid = 1
      EndIf 
    Else
      entriesAreValid = 1
    EndIf
  Until entriesAreValid = 1
  
  ForEach numbers()
    output$  = "f(" + RTrim(RTrim(StrD(numbers(), 3), "0"), ".") + ") = "
    result.d = f(numbers())
    If result > 400
      output$ + "Too Large"
    Else
      output$ + RTrim(RTrim(StrD(result, 3), "0"), ".")
    EndIf
    PrintN(output$)
  Next
  
  Print(#crlf$ + #crlf$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

{{out}}

```txt

Enter eleven numbers that are each separated by spaces or commas:
10, -1, 1, 2, 3, 4, 4.3, 4.305, 4.303, 4.302, 4.301
f(4.301) = 399.886
f(4.302) = Too Large
f(4.303) = Too Large
f(4.305) = Too Large
f(4.3) = 399.609
f(4) = 322
f(3) = 136.732
f(2) = 41.414
f(1) = 6
f(-1) = -4
f(10) = Too Large
```



## Python


### Functional


```python
Python 3.2.2 (default, Sep  4 2011, 09:51:08) [MSC v.1500 32 bit (Intel)] on win32
Type "copyright", "credits" or "license()" for more information.
>>> def f(x): return abs(x) ** 0.5 + 5 * x**3

>>> print(', '.join('%s:%s' % (x, v if v<=400 else "TOO LARGE!")
	           for x,v in ((y, f(float(y))) for y in input('\nnumbers: ').strip().split()[:11][::-1])))

11 numbers: 1 2 3 4 5 6 7 8 9 10 11
11:TOO LARGE!, 10:TOO LARGE!, 9:TOO LARGE!, 8:TOO LARGE!, 7:TOO LARGE!, 6:TOO LARGE!, 5:TOO LARGE!, 4:322.0, 3:136.73205080756887, 2:41.41421356237309, 1:6.0
>>> 
```



### Procedural


```python
def f(x):
    return abs(x) ** 0.5 + 5 * x**3

def ask():
    return [float(y)
            for y in input('\n11 numbers: ').strip().split()[:11]]

if __name__ == '__main__':
    s = ask()
    s.reverse()
    for x in s:
        result = f(x)
        if result > 400:
            print(' %s:%s' % (x, "TOO LARGE!"), end='')
        else:
            print(' %s:%s' % (x, result), end='')
    print('')
```

{{out|Sample output}}

```txt

11 numbers: 1 2 3 4 5 6 7 8 9 10 11
 11.0:TOO LARGE! 10.0:TOO LARGE! 9.0:TOO LARGE! 8.0:TOO LARGE! 7.0:TOO LARGE! 6.0:TOO LARGE! 5.0:TOO LARGE! 4.0:322.0 3.0:136.73205080756887 2.0:41.41421356237309 1.0:6.0
```



## R


```R
S <- scan(n=11)

f <- function(x) sqrt(abs(x)) + 5*x^3

for (i in rev(S)) {
  res <- f(i)
  if (res > 400)
    print("Too large!")
  else
    print(res)
}
```


{{out|Sample output}}

```txt
&gt; source("~/tpk.R")
1: 1 2 3 4 5
6: 6 7 8 9 10
11: 11
Read 11 items
[1] "Too large!"
[1] "Too large!"
[1] "Too large!"
[1] "Too large!"
[1] "Too large!"
[1] "Too large!"
[1] "Too large!"
[1] 322
[1] 136.7321
[1] 41.41421
[1] 6
```



## Racket


```racket

#lang racket

(define input 
  (for/list ([i 11])
    (printf "Enter a number (~a of 11): " (+ 1 i))
    (read)))

(for ([x (reverse input)])
  (define res (+ (sqrt (abs x)) (* 5 (expt x 3))))
  (if (> res 400)
      (displayln "Overflow!")
      (printf "f(~a) = ~a\n" x res)))

```


{{out}}

```txt

Enter a number (1 of 11): 1
Enter a number (2 of 11): 2
Enter a number (3 of 11): 3
Enter a number (4 of 11): 4
Enter a number (5 of 11): 5
Enter a number (6 of 11): 6
Enter a number (7 of 11): 7
Enter a number (8 of 11): 8
Enter a number (9 of 11): 9
Enter a number (10 of 11): 10
Enter a number (11 of 11): 11
Overflow!
Overflow!
Overflow!
Overflow!
Overflow!
Overflow!
Overflow!
f(4) = 322
f(3) = 136.73205080756887
f(2) = 41.41421356237309
f(1) = 6

```



## REXX

The REXX language doesn't have a   '''sqrt'''   function, so a RYO version is included here.     ['''RYO'''   =   '''R'''oll '''Y'''our '''O'''wn.]


It could be noted that almost half of this program is devoted to prompting, parsing and validating of the (input) numbers,

not to mention some hefty code to support right-justified numbers such that they are aligned when displayed.

```rexx
/*REXX program implements the Trabb─Pardo-Knuth algorithm for N numbers (default is 11).*/
numeric digits 200                               /*the number of digits precision to use*/
parse arg N .;   if N=='' | N==","  then N=11    /*Not specified?  Then use the default.*/
maxValue= 400                                    /*the maximum value   f(x)   can have. */
     wid=  20                                    /*  ··· but only show this many digits.*/
    frac=   5                                    /*  ··· show this # of fractional digs.*/
say '                           _____'           /* ◄─── this  SAY  displays a vinculum.*/
say 'function:        ƒ(x)  ≡  √ │x│   +   (5 * x^3)'
prompt= 'enter '      N      " numbers for the Trabb─Pardo─Knuth algorithm:     (or Quit)"

  do ask=0;    say;  /*░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░*/
  say prompt;  say;      pull $;     say                                             /*░*/
  if abbrev('QUIT',$,1)  then do;    say 'quitting.';    exit 1;   end               /*░*/
  ok=0                                                                               /*░*/
                         select                  /*validate there're N numbers.*/    /*░*/
                         when $=''        then say  "no numbers entered"             /*░*/
                         when words($)<N  then say  "not enough numbers entered"     /*░*/
                         when words($)>N  then say  "too many numbers entered"       /*░*/
                         otherwise        ok=1                                       /*░*/
                         end   /*select*/                                            /*░*/
  if \ok  then iterate                                    /* [↓]  W=max width. */    /*░*/
  w=0;                   do v=1  for N;       _=word($, v);   w=max(w, length(_) )   /*░*/
                         if datatype(_, 'N')  then iterate          /*numeric ?*/    /*░*/
                         say _  "isn't numeric";   iterate ask                       /*░*/
                         end   /*v*/                                                 /*░*/
  leave                                                                              /*░*/
  end   /*ask*/      /*░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░*/

say 'numbers entered: '    $
say
        do i=N  by -1  for N;  #=word($, i) / 1  /*process the numbers in reverse.      */
        g =  fmt(   f( # )        )              /*invoke  function  ƒ  with arg number.*/
        gw=right(  'ƒ('#") ",  w+7)              /*nicely formatted  ƒ(number).         */
        if g>maxValue  then say gw    "is >  "     maxValue      ' ['space(g)"]"
                       else say gw     "   = "     g
        end   /*i*/                              /* [↑]  display the result to terminal.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
f:    procedure;  parse arg x;           return  sqrt( abs(x) )    +    5 * x**3
/*──────────────────────────────────────────────────────────────────────────────────────*/
fmt:  z=right(translate(format(arg(1), wid, frac), 'e', "E"), wid) /*right adjust; use e*/
      if pos(.,z)\==0 then z=left(strip(strip(z,'T',0),"T",.),wid) /*strip trailing 0 &.*/
      return right(z, wid - 4*(pos('e', z)==0) )                   /*adjust: no exponent*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x; if x=0  then return 0; d=digits(); m.=9; numeric form; h=d+6
      numeric digits; parse value format(x,2,1,,0) 'E0'  with  g 'E' _ .;  g=g *.5'e'_ % 2
        do j=0  while h>9;      m.j=h;               h=h % 2  + 1;  end  /*j*/
        do k=j+5  to 0  by -1;  numeric digits m.k;  g=(g+x/g)*.5;  end  /*k*/;   return g
```

{{out|output|text=  when prompted, using the input of:     <tt> 5   3.3   3   2e-1   1   0   -1   -222   -33   4.0004   +5 </tt>}}

```txt

                           _____
function:        ƒ(x)  ≡  √ │x│   +   (5 * x^3)

enter  11  numbers for the Trabb─Pardo─Knuth algorithm:     (or Quit)

5   3.3   3   2e-1   1   0   -1   -222   -33   4.0004   +5   ◄■■■■■■■■■■■ this is what the user entered.

numbers entered:  5   3.3   3   2E-1   1   0   -1   -222   -33   4.0004   +5

        ƒ(5)  is >   400  [627.23607]
   ƒ(4.0004)     =         322.09611
      ƒ(-33)     =     -179679.25544
     ƒ(-222)     =   -54705225.10034
       ƒ(-1)     =          -4
        ƒ(0)     =           0
        ƒ(1)     =           6
      ƒ(0.2)     =           0.48721
        ƒ(3)     =         136.73205
      ƒ(3.3)     =         181.50159
        ƒ(5)  is >   400  [627.23607]

```



## Ring


```ring

# Project : Trabb Pardo–Knuth algorithm

decimals(3)
x = list(11)
for n=1 to 11
    x[n] = n
next

s = [-5, -3, -2, -1, 0, 1, 2, 3, 4, 5, 6] 
for i = 1 to 11 
    see string(i) + " => " + s[i] + nl
next 
see copy("-", 20) + nl 
i = i - 1 

while i > 0
      see "f(" + string(s[i]) + ") = " 
      x = f(s[i])
      if x > 400 
         see "-=< overflow >=-" + nl
      else 
         see x + nl
      ok
      i = i - 1
end

func f(n)
     return sqrt(fabs(n)) + 5 * pow(n, 3)

```

Output:

```txt

1 => -5
2 => -3
3 => -2
4 => -1
5 => 0
6 => 1
7 => 2
8 => 3
9 => 4
10 => 5
11 => 6
--------------------
f(6) = -=< overflow >=-
f(5) = -=< overflow >=-
f(4) = 322
f(3) = 136.732
f(2) = 41.414
f(1) = 6
f(0) = 0
f(-1) = -4
f(-2) = -38.586
f(-3) = -133.268
f(-5) = -622.764


```



## Ruby



```ruby
def f(x) x.abs ** 0.5 + 5 * x ** 3 end

puts "Please enter 11 numbers:"
nums = 11.times.map{ gets.to_f }

nums.reverse_each do |n|
  print "f(#{n}) = "
  res = f(n)
  puts res > 400 ? "Overflow!" : res
end
```


{{out}}

```txt

ruby tpk.rb
Please enter 11 numbers:
1
2
3
4
5
6
7
8
9
-1
-4
f(-4.0) = -318.0
f(-1.0) = -4.0
f(9.0) = Overflow!
f(8.0) = Overflow!
f(7.0) = Overflow!
f(6.0) = Overflow!
f(5.0) = Overflow!
f(4.0) = 322.0
f(3.0) = 136.73205080756887
f(2.0) = 41.41421356237309
f(1.0) = 6.0

```



## Rust


```rust

use std::io::{self, BufRead};

fn op(x: f32) -> Option<f32> {
    let y = x.abs().sqrt() + 5.0 * x * x * x;
    if y < 400.0 {
        Some(y)
    } else {
        None
    }
}

fn main() {
    println!("Please enter 11 numbers (one number per line)");
    let stdin = io::stdin();

    let xs = stdin
        .lock()
        .lines()
        .map(|ox| ox.unwrap().trim().to_string())
        .flat_map(|s| str::parse::<f32>(&s))
        .take(11)
        .collect::<Vec<_>>();

    for x in xs.into_iter().rev() {
        match op(x) {
            Some(y) => println!("{}", y),
            None => println!("overflow"),
        };
    }
}

```


{{out}}

```txt

Enter 11 numbers (one number per line)
1
2
3
4
5
6
7
8
9
10
11
overflow
overflow
overflow
overflow
overflow
overflow
overflow
322
136.73206
41.414215
6

```



## Scala


```scala
object TPKa extends App {
    final val numbers = scala.collection.mutable.MutableList[Double]()
    final val in = new java.util.Scanner(System.in)
    while (numbers.length < CAPACITY) {
        print("enter a number: ")
        try {
            numbers += in.nextDouble()
        }
        catch {
            case _: Exception =>
                in.next()
                println("invalid input, try again")
        }
    }

    numbers reverseMap { x =>
        val fx = Math.pow(Math.abs(x), .5D) + 5D * (Math.pow(x, 3))
        if (fx < THRESHOLD)
            print("%8.3f -> %8.3f\n".format(x, fx))
        else
            print("%8.3f -> %s\n".format(x, Double.PositiveInfinity.toString))
    }

    private final val THRESHOLD = 400D
    private final val CAPACITY = 11
}
```



## Sidef

{{trans|Perl 6}}

```ruby
var nums; do {
    nums = Sys.readln("Please type 11 space-separated numbers: ").nums
} while(nums.len != 11)

nums.reverse.each { |n|
    var r = (n.abs.sqrt + (5 * n**3));
    say "#{n}\t#{ r > 400 ? 'Urk!' : r }";
}
```

{{out}}

```txt

Please type 11 space-separated numbers: 10 -1 1 2 3 4 4.3 4.305 4.303 4.302 4.301
4.301	399.886299747726800445468371077898575778355
4.302	Urk!
4.303	Urk!
4.305	Urk!
4.3	399.608644135332772087455898679984992632401
4	322
3	136.732050807568877293527446341505872366943
2	41.41421356237309504880168872420969807857
1	6
-1	-4
10	Urk!

```



## Sinclair ZX81 BASIC

Works with the unexpanded (1k RAM) ZX81

```basic
 10 DIM A(11)
 20 PRINT "ENTER ELEVEN NUMBERS:"
 30 FOR I=1 TO 11
 40 INPUT A(I)
 50 NEXT I
 60 FOR I=11 TO 1 STEP -1
 70 LET Y=SQR ABS A(I)+5*A(I)**3
 80 IF Y<=400 THEN GOTO 110
 90 PRINT A(I),"TOO LARGE"
100 GOTO 120
110 PRINT A(I),Y
120 NEXT I
```

{{out}}

```txt
ENTER ELEVEN NUMBERS:
2.8             111.43332
3.333           186.95529
1.01            6.1564926
2.55            84.503747
11              TOO LARGE
6               TOO LARGE
5               TOO LARGE
4               322
3               136.73205
2               41.414214
1               6
```



## Swift

{{works with|Swift 2.0}}

```swift
import Foundation

print("Enter 11 numbers for the Trabb─Pardo─Knuth algorithm:")

let f: (Double) -> Double = { sqrt(fabs($0)) + 5 * pow($0, 3) }

(1...11)
    .generate()
    .map { i -> Double in
        print("\(i): ", terminator: "")
        guard let s = readLine(), let n = Double(s) else { return 0 }
        return n
    }
    .reverse()
    .forEach {
        let result = f($0)
        print("f(\($0))", result > 400.0 ? "OVERFLOW" : result, separator: "\t")
    }

```

{{Out}}

```txt

Enter 11 numbers for the Trabb─Pardo─Knuth algorithm:
1: 1
2: 2
3: 3
4: 4
5: 5
6: 6
7: 7
8: 8
9: 9
10: 10
11: 11
f(11.0)	OVERFLOW
f(10.0)	OVERFLOW
f(9.0)	OVERFLOW
f(8.0)	OVERFLOW
f(7.0)	OVERFLOW
f(6.0)	OVERFLOW
f(5.0)	OVERFLOW
f(4.0)	322.0
f(3.0)	136.732050807569
f(2.0)	41.4142135623731
f(1.0)	6.0

```



## Symsyn


```symsyn

|Trabb Pardo–Knuth algorithm

a : 11 0

 i
 if i LE 10
    [] $s
    ~ $s w
    w a.i
   + i
    goif
 endif
 10 i
 if i GE 0
    call f
    if x GT 400
       'too large' $s
    else
       ~ x $s
    endif
    ~ i $r
    + ' ' $r
    + $r $s.1
    $s []
    - i
    goif
 endif
 stop
 
f a.i t
  * t t x
  * x t x
  * 5 x
  abs t
  sqrt t y
  + y x
  return

```



## Tcl


```tcl
# Helper procedures
proc f {x} {expr {abs($x)**0.5 + 5*$x**3}}
proc overflow {y} {expr {$y > 400}}

# Read in 11 numbers, with nice prompting
fconfigure stdout -buffering none
for {set n 1} {$n <= 11} {incr n} {
    puts -nonewline "number ${n}: "
    lappend S [scan [gets stdin] "%f"]
}

# Process and print results in reverse order
foreach x [lreverse $S] {
    set result [f $x]
    if {[overflow $result]} {
	puts "${x}: TOO LARGE!"
    } else {
	puts "${x}: $result"
    }
}
```

{{out|Sample run}}

```txt

number 1: 0
number 2: 1
number 3: 2
number 4: 3
number 5: 4
number 6: 5
number 7: 6
number 8: 7
number 9: 8
number 10: 9
number 11: 10
10.0: TOO LARGE!
9.0: TOO LARGE!
8.0: TOO LARGE!
7.0: TOO LARGE!
6.0: TOO LARGE!
5.0: TOO LARGE!
4.0: 322.0
3.0: 136.73205080756887
2.0: 41.41421356237309
1.0: 6.0
0.0: 0.0

```



## VBScript


```vb

Function tpk(s)
	arr = Split(s," ")
	For i = UBound(arr) To 0 Step -1
		n = fx(CDbl(arr(i)))
		If  n > 400 Then
			WScript.StdOut.WriteLine arr(i) & " = OVERFLOW"
		Else
			WScript.StdOut.WriteLine arr(i) & " = " & n
		End If
	Next
End Function

Function fx(x)
	fx = Sqr(Abs(x))+5*x^3
End Function

'testing the function
WScript.StdOut.Write "Please enter a series of numbers:"
list = WScript.StdIn.ReadLine
tpk(list)

```


{{Out}}
The number series was derived from the example of C.

```txt

C:\>cscript /nologo tpk.vbs
Please enter 10 numbers:10 -1 1 2 3 4 4.3 4.305 4.303 4.302 4.301
4.301 = 399.886299747727
4.302 = OVERFLOW
4.303 = OVERFLOW
4.305 = OVERFLOW
4.3 = 399.608644135333
4 = 322
3 = 136.732050807569
2 = 41.4142135623731
1 = 6
-1 = -4
10 = OVERFLOW

```



## XPL0


```XPL0
include c:\cxpl\codes;

func real F(X);
real X;
return sqrt(abs(X)) + 5.0*X*X*X;

real Result, S(11);  int I;
[Text(0, "Please enter 11 numbers: ");
for I:= 0 to 11-1 do S(I):= RlIn(0);

for I:= 11-1 downto 0 do
        [RlOut(0, S(I));
        Result:= F(S(I));
        if Result > 400.0 then
                Text(0, "  overflows")
        else    RlOut(0, Result);
        CrLf(0)];
]
```


{{out}}

```txt

Please enter 11 numbers: 10 -1 1 2 3 4 4.3 4.305 4.303 4.302 4.301
    4.30100  399.88630
    4.30200  overflows
    4.30300  overflows
    4.30500  overflows
    4.30000  399.60864
    4.00000  322.00000
    3.00000  136.73205
    2.00000   41.41421
    1.00000    6.00000
   -1.00000   -4.00000
   10.00000  overflows

```



## zkl


```zkl
fcn f(x) { x.abs().pow(0.5) + x.pow(3)*5 }
reg ns; do{
   ns=ask("11 numbers seperated by spaces: ");
   try{ ns=ns.split(" ").filter().apply("toFloat") } catch{}
}while(not ns.isType(List) or ns.len()!=11);
ns.reverse().apply(fcn(x){
   fx:=f(x); "f(%7.3f)-->%s".fmt(x, if(fx>400)"Overflow" else fx) })
.pump(Console.println);
```

{{out}}

```txt

11 numbers seperated by spaces: 10 -1 1     2 3 4 4.3 4.305 4.303 4.302 4.301
f(  4.301)-->399.886
f(  4.302)-->Overflow
f(  4.303)-->Overflow
f(  4.305)-->Overflow
f(  4.300)-->399.609
f(  4.000)-->322
f(  3.000)-->136.732
f(  2.000)-->41.4142
f(  1.000)-->6
f( -1.000)-->-4
f( 10.000)-->Overflow

```

