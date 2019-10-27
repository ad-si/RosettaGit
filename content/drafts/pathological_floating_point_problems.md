+++
title = "Pathological floating point problems"
description = ""
date = 2019-08-12T14:51:18Z
aliases = []
[extra]
id = 20044
[taxonomies]
categories = []
tags = []
+++

{{task}}

Most programmers are familiar with the inexactness of floating point calculations in a binary processor. 

The classic example being:

```txt

0.1 + 0.2 =  0.30000000000000004

```


In many situations the amount of error in such calculations is very small and can be overlooked or eliminated with rounding.

There are pathological problems however, where seemingly simple, straight-forward calculations are extremely sensitive to even tiny amounts of imprecision.

This task's purpose is to show how your language deals with such classes of problems.


'''A sequence that seems to converge to a wrong limit.''' 

Consider the sequence:
:::::: <big><big>  v<sub>1</sub> =  2                                                                     </big></big>
:::::: <big><big>  v<sub>2</sub> = -4                                                                     </big></big>
:::::: <big><big>  v<sub>n</sub> = 111   -   1130   /   v<sub>n-1</sub>   +   3000  /   (v<sub>n-1</sub> * v<sub>n-2</sub>)    </big></big>


As   '''n'''   grows larger, the series should converge to   '''6'''   but small amounts of error will cause it to approach   '''100'''.


;Task 1:
Display the values of the sequence where   n =   3, 4, 5, 6, 7, 8, 20, 30, 50 & 100   to at least '''16''' decimal places.

```txt

    n = 3     18.5
    n = 4      9.378378
    n = 5      7.801153
    n = 6      7.154414
    n = 7      6.806785
    n = 8      6.5926328
    n = 20     6.0435521101892689
    n = 30     6.006786093031205758530554
    n = 50     6.0001758466271871889456140207471954695237
    n = 100    6.000000019319477929104086803403585715024350675436952458072592750856521767230266

```



;Task 2:
'''The Chaotic Bank Society'''   is offering a new investment account to their customers. 

You first deposit   $e - 1   where   e   is   2.7182818...   the base of natural logarithms.

After each year, your account balance will be multiplied by the number of years that have passed, and $1 in service charges will be removed. 

So ...
::* after 1 year, your balance will be multiplied by 1 and $1 will be removed for service charges.
::* after 2 years your balance will be doubled and $1 removed.
::* after 3 years your balance will be tripled and $1 removed.
::* <b> ... </b>
::* after 10 years, multiplied by 10 and $1 removed,  and so on. 


What will your balance be after   25   years?
    Starting balance: $<i>e</i>-1
    Balance = (Balance * year) - 1 for 25 years
    Balance after 25 years: $0.0399387296732302


;Task 3, extra credit:
'''Siegfried Rump's example.'''   Consider the following function, designed by Siegfried Rump in 1988.
:::::: <big><big>  f(a,b) = 333.75b<sup>6</sup> + a<sup>2</sup>( 11a<sup>2</sup>b<sup>2</sup> - b<sup>6</sup> - 121b<sup>4</sup> - 2 ) + 5.5b<sup>8</sup> + a/(2b)          </big></big>
:::::: <big> compute   <big> f(a,b) </big>   where   <big> a=77617.0 </big>   and   <big> b=33096.0  </big></big>
:::::: <big><big>  f(77617.0, 33096.0)   =   -0.827396059946821          </big></big>


Demonstrate how to solve at least one of the first two problems, or both, and the third if you're feeling particularly jaunty.


;See also;
*   [https://perso.ens-lyon.fr/jean-michel.muller/chapitre1.pdf Floating-Point Arithmetic]   Section 1.3.2 Difficult problems.





## 360 Assembly

The system/360 hexadecimal single precision floating point format is known to its weakness 
in precision. A lot of more precise formats have been added since.

'''A sequence that seems to converge to a wrong limit'''


```360asm
*        Pathological floating point problems  03/05/2016
PATHOFP  CSECT
         USING  PATHOFP,R13 
SAVEAR   B      STM-SAVEAR(R15)
         DC     17F'0'
STM      STM    R14,R12,12(R13)
         ST     R13,4(R15)
         ST     R15,8(R13)
         LR     R13,R15
         LE     F0,=E'2'
         STE    F0,U             u(1)=2
         LE     F0,=E'-4'
         STE    F0,U+4           u(2)=-4
         LA     R6,3             n=3
         LA     R7,U+4           @u(n-1)
         LA     R8,U             @u(n-2)
         LA     R9,U+8           @u(n)
LOOPN    CH     R6,=H'100'       do n=3 to 100
         BH     ELOOPN
         LE     F4,0(R7)         u(n-1)
         LE     F2,=E'1130'      1130
         DER    F2,F4            1130/u(n-1)
         LE     F0,=E'111'       111
         SER    F0,F2            111-1130/u(n-1)
         LE     F2,0(R7)         u(n-1)
         LE     F4,0(R8)         u(n-2)
         MER    F2,F4            u(n-1)*u(n-2)
         LE     F6,=E'3000'      3000
         DER    F6,F2            3000/(u(n-1)*u(n-2))
         AER    F0,F6            111-1130/u(n-1)+3000/(u(n-1)*u(n-2))
         STE    F0,0(R9)         store into u(n)
         XDECO  R6,PG+0          n
         LE     F0,0(R9)         u(n)
         LA     R0,3             number of decimals
         BAL    R14,FORMATF      format(u(n),'F13.3')
         MVC    PG+12(13),0(R1)  put into buffer
         XPRNT  PG,80            print buffer
         LA     R6,1(R6)         n=n+1
         LA     R7,4(R7)         @u(n-1)
         LA     R8,4(R8)         @u(n-2)
         LA     R9,4(R9)         @u(n)
         B      LOOPN
ELOOPN   L      R13,4(0,R13)
         LM     R14,R12,12(R13)
         XR     R15,R15
         BR     R14
         COPY   FORMATF
         LTORG  
PG       DC     CL80' '          buffer
U        DS     100E
         YREGS
         YFPREGS 
         END    PATHOFP
```

The divergence comes very soon.
{{out}}

```txt

           3       18.500
           4        9.378
           5        7.801
           6        7.154
           7        6.805
           8        6.578
           9        6.235
          10        2.915
          11     -111.573
          12      111.905
          13      100.661
          14      100.040
          15      100.002
          16      100.000
          17      100.000
          18      100.000
         ...      100.000

```



## Ada


### Task 1: Converging Sequence



```Ada
with Ada.Text_IO;

procedure Converging_Sequence is
   
   generic
      type Num is digits <>;
      After: Positive;
   procedure Task_1;
      
   procedure Task_1 is
      package FIO is new Ada.Text_IO.Float_IO(Num);
      package IIO is new Ada.Text_IO.Integer_IO(Integer);
   
      procedure Output (I: Integer; N: Num) is
      begin
	 IIO.Put(Item => I, Width => 4);
	 FIO.Put(Item => N, Fore => 4, Aft =>  After, Exp => 0);
	 Ada.Text_IO.New_Line;
      end Output;
   
      Very_Old: Num :=  2.0;
      Old:      Num := -4.0;
      Now:        Num;
   begin
      Ada.Text_IO.Put_Line("Converging Sequence with" & Integer'Image(After) & 
			     " digits");
      for I in 3 .. 100 loop
	 Now := 111.0  - 1130.0   /   Old   + 3000.0  /   (Old * Very_Old);
	 Very_Old := Old;
	 Old := Now;
	 if (I < 9) or else (I=20 or I=30 or I=50 or I=100) then
	    Output(I, Now);
	 end if;
      end loop;
      Ada.Text_IO.New_Line;
   end Task_1;
   
   type Short is digits(8);
   type Long  is digits(16);
   
   procedure Task_With_Short is new Task_1(Short, 8);
   procedure Task_With_Long  is new Task_1(Long, 16);
begin
   Task_With_Short;
   Task_With_Long;
end Converging_Sequence;
```


{{out}}

```txt
Converging Sequence with 8 digits
   3  18.50000000
   4   9.37837838
   5   7.80115274
   6   7.15441448
   7   6.80678474
   8   6.59263277
  20  98.34950312
  30 100.00000000
  50 100.00000000
 100 100.00000000

Converging Sequence with 16 digits
   3  18.5000000000000000
   4   9.3783783783783784
   5   7.8011527377521614
   6   7.1544144809752494
   7   6.8067847369236337
   8   6.5926327687044483
  20   8.9530549789723472
  30  99.9999999981565451
  50 100.0000000000000000
 100 100.0000000000000000
```



### Task 2: Chaotic Bank Society



```Ada
with Ada.Text_IO, Ada.Numerics;

procedure Chaotic_Bank is
   
   generic
     type Num is digits <>;
     After: Positive;
   procedure Task_2;
   
   procedure Task_2 is
      package IIO is new Ada.Text_IO.Integer_IO(Integer);
      package FIO is new Ada.Text_IO.Float_IO(Num);
      Balance: Num :=  Ada.Numerics.E - 1.0;
   begin
      Ada.Text_IO.Put_Line("Chaotic Bank Society with" & 
			     Integer'Image(After) & " digits");
      Ada.Text_IO.Put_Line("year        balance");
      for year in 1 .. 25 loop
	 Balance := (Balance * Num(year))- 1.0;
	 IIO.Put(Item => Year, Width => 2);
	 FIO.Put(Balance, Fore => 11, Aft => After, Exp => 0);
	 Ada.Text_IO.New_Line;
      end loop;
      Ada.Text_IO.New_Line;
   end Task_2;
   
   type Short is digits(8);
   type Long  is digits(16);
   
   procedure Task_With_Short is new Task_2(Short, 8);
   procedure Task_With_Long  is new Task_2(Long, 16);
   
begin
   Task_With_Short;
   Task_With_Long;
end Chaotic_Bank;
```


{{out}}


```txt
Chaotic Bank Socienty with 8 digits
year        balance
 1          0.71828183
 2          0.43656366
 3          0.30969097
 4          0.23876388
...         ...
16          0.06389363
17          0.08619166
18          0.55144980
19          9.47754622
20        188.55092437
21       3958.56941176
22      87087.52705873
23    2003012.12235075
24   48072289.93641794
25 1201807247.41044855

Chaotic Bank Socienty with 16 digits
year        balance
 1          0.7182818284590452
 2          0.4365636569180905
 3          0.3096909707542714
 4          0.2387638830170856
...         ...
17          0.0586186042274583
18          0.0551348760942503
19          0.0475626457907552
20         -0.0487470841848960
21         -2.0236887678828168
22        -45.5211528934219700
23      -1047.9865165487053100
24     -25152.6763971689275000
25    -628817.9099292231860000
```


===Task 3: Rump's Example===


```Ada
with Ada.Text_IO; use Ada.Text_IO;                                                                        
                                                                                                          
procedure Rumps_example is                                                                                
                                                                                                          
   type Short is digits(8);                                                                               
   type Long  is digits(16);                                                                              
                                                                                                          
   A: constant := 77617.0;                                                                                
   B: constant := 33096.0;                                                                                
   C: constant := 333.75*B**6 + A**2*(11.0*A**2*B**2 - B**6 - 121.0*B**4 - 2.0) + 5.5*B**8 + A/(2.0*B);   
                                                                                                          
   package LIO is new Float_IO(Long);                                                                     
   package SIO is new Float_IO(Short);                                                                    
begin                                                                                                     
   Put("Rump's Example, Short: ");                                                                        
   SIO.Put(C, Fore => 1, Aft => 8, Exp => 0);  New_Line;                                                  
   Put("Rump's Example, Long:  ");                                                                        
   LIO.Put(C, Fore => 1, Aft => 16, Exp => 0); New_Line;                                                  
end Rumps_example;  
```


{{out}}


```txt
Rump's Example, Short: -0.82739606
Rump's Example, Long:  -0.827396059946821
```



## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.3.win32}}
In Algol 68G, we can specify the precision of LONG LONG REAL values

```algol68
BEGIN
    # task 1 #
    BEGIN
        PR precision 32 PR
        print( ( " 32 digit REAL numbers", newline ) );
        [ 1 : 100 ]LONG LONG REAL v;
        v[ 1 ] := 2;
        v[ 2 ] := -4;
        FOR n FROM 3 TO UPB v DO v[ n ] := 111 - ( 1130 / v[ n - 1 ] ) + ( 3000 / ( v[ n - 1 ] * v[ n - 2 ] ) ) OD;
        FOR n FROM  3 TO  8       DO print( ( "n = ", whole( n, 3 ), " ", fixed( v[ n ], -22, 16 ), newline ) ) OD;
        FOR n FROM 20 BY 10 TO 50 DO print( ( "n = ", whole( n, 3 ), " ", fixed( v[ n ], -22, 16 ), newline ) ) OD;
        print( ( "n = 100 ", fixed( v[ 100 ], -22, 16 ), newline ) )
    END;
    BEGIN
        PR precision 120 PR
        print( ( "120 digit REAL numbers", newline ) );
        [ 1 : 100 ]LONG LONG REAL v;
        v[ 1 ] := 2;
        v[ 2 ] := -4;
        FOR n FROM 3 TO UPB v DO v[ n ] := 111 - ( 1130 / v[ n - 1 ] ) + ( 3000 / ( v[ n - 1 ] * v[ n - 2 ] ) ) OD;
        print( ( "n = 100 ", fixed( v[ 100 ], -22, 16 ), newline ) )
    END;
    print( ( newline ) );
    # task 2 #
    BEGIN
        print( ( "single precision REAL numbers...", newline ) );
        REAL chaotic balance := exp( 1 ) - 1;
        print( ( "initial chaotic balance: ", fixed( chaotic balance, -22, 16 ), newline ) );
        FOR i FROM 1 TO 25 DO ( chaotic balance *:= i ) -:= 1 OD;
        print( ( "25 year chaotic balance: ", fixed( chaotic balance, -22, 16 ), newline ) )
    END;
    BEGIN
        print( ( "double precision REAL numbers...", newline ) );
        LONG REAL chaotic balance := long exp( 1 ) - 1;
        print( ( "initial chaotic balance: ", fixed( chaotic balance, -22, 16 ), newline ) );
        FOR i FROM 1 TO 25 DO ( chaotic balance *:= i ) -:= 1 OD;
        print( ( "25 year chaotic balance: ", fixed( chaotic balance, -22, 16 ), newline ) )
    END;
    BEGIN
        PR precision 32 PR
        print( ( "        32 digit REAL numbers...", newline ) );
        LONG LONG REAL chaotic balance := long long exp( 1 ) - 1;
        print( ( "initial chaotic balance: ", fixed( chaotic balance, -22, 16 ), newline ) );
        FOR i FROM 1 TO 25 DO ( chaotic balance *:= i ) -:= 1 OD;
        print( ( "25 year chaotic balance: ", fixed( chaotic balance, -22, 16 ), newline ) )
    END
END
```

{{out}}

```txt

 32 digit REAL numbers
n =  +3    18.5000000000000000
n =  +4     9.3783783783783784
n =  +5     7.8011527377521614
n =  +6     7.1544144809752494
n =  +7     6.8067847369236330
n =  +8     6.5926327687044384
n = +20     6.0435521101892689
n = +30     6.0067860930262429
n = +40    -2.9367486132065552
n = +50   100.0000000006552004
n = 100   100.0000000000000000
120 digit REAL numbers
n = 100   100.0000000000000000

single precision REAL numbers...
initial chaotic balance:     1.7182818284590400
25 year chaotic balance: -2242373258.5701500000
double precision REAL numbers...
initial chaotic balance:     1.7182818284590452
25 year chaotic balance:     0.0406729916134442
        32 digit REAL numbers...
initial chaotic balance:     1.7182818284590452
25 year chaotic balance:     0.0399387296732302

```



## AWK


{{needs-review|AWK|Check if results are different for non-GNU awk}}

GNU awk defaults to double-precision floating point numbers (not sure if this is true for other awk implementations?). GNU awk 4.1+ provides library support for arbitrary-precision floating point calculations, but not all available binaries have this compiled in.

awk code:

```awk

BEGIN {
    do_task1()
    do_task2()
    do_task3()
    exit
}


function do_task1(){
    print "Task 1"
    v[1] = 2
    v[2] = -4
    for (n=3; n<=100; n++) v[n] = 111 - 1130 / v[n-1] + 3000 / (v[n-1] * v[n-2])
    
    for (i=3; i<=8; i++) print_results(i)
    print_results(20)
    print_results(30)
    print_results(50)
    print_results(100)
}

# This works because all awk variables are global, except when declared locally
function print_results(n){
    printf("n = %d\t%20.16f\n", n, v[n])
}

# This function doesn't need any parameters; declaring balance and i in the function parameters makes them local
function do_task2(      balance, i){
    balance[0] = exp(1)-1
    for (i=1; i<=25; i++) balance[i] = balance[i-1]*i-1
    printf("\nTask 2\nBalance after 25 years: $%12.10f", balance[25])
}

function do_task3(      a, b, f_ab){
    a = 77617
    b = 33096
    
    f_ab = 333.75 * b^6 + a^2 * (11*a^2*b^2 - b^6 - 121*b^4 - 2) + 5.5*b^8 + a/(2*b) 
    printf("\nTask 3\nf(%6.12f, %6.12f) = %10.24f", a, b, f_ab)
}


```


This version doesn't include the arbitrary-precision libraries, so the program demonstrates the incorrect results:

```txt

Task 1
n = 3    18.5000000000000000
n = 4     9.3783783783783790
n = 5     7.8011527377521688
n = 6     7.1544144809753334
n = 7     6.8067847369248113
n = 8     6.5926327687217920
n = 20   98.3495031221653591
n = 30   99.9999999999989342
n = 50  100.0000000000000000
n = 100 100.0000000000000000

Task 2
Balance after 25 years: $-2242373258.570158004760742
Task 3
f(77617.000000000000, 33096.000000000000) = -1180591620717411303424.000000000000000000000000

```


On versions with the libraries compiled in, the results depend on the level of precision specified. With 1024 bits, the results are as follows:

```txt

Task 1
n = 3	 18.5000000000000000
n = 4	  9.3783783783783784
n = 5	  7.8011527377521614
n = 6	  7.1544144809752494
n = 7	  6.8067847369236330
n = 8	  6.5926327687044384
n = 20	  6.0435521101892689
n = 30	  6.0067860930312058
n = 50	  6.0001758466271872
n = 100	  6.0000000193194779

Task 2
Balance after 25 years: $0.0399387297
Task 3
f(77617.000000000000, 33096.000000000000) = -0.827396059946821368141165

```

With 256 bits of precision, tasks 2 and 3 provide the same answer as above. Task 1 appears to be converging after 50 iterations, but by 100 iterations the answer has changed to 100.0


## C

Such exercises are very good examples that just because you have a nice library doesn't mean you won't get the wrong results. I was over-ambitious and the result is I have been wrangling with the Chaotic Bank task for a long time now, I will come back to it later but for now here are the first two cases, the "trivial" one of 0.1 + 0.2 and the Pathological series :

### First two tasks

{{libheader|GMP}}

```C

#include<stdio.h>
#include<gmp.h>

void firstCase(){
	mpf_t a,b,c;
	
	mpf_inits(a,b,c,NULL);
	
	mpf_set_str(a,"0.1",10);
	mpf_set_str(b,"0.2",10);
	mpf_add(c,a,b);
	
	gmp_printf("\n0.1 + 0.2 = %.*Ff",20,c);
}

void pathologicalSeries(){
	int n;
	mpf_t v1, v2, vn, a1, a2, a3, t2, t3, prod;
	
	mpf_inits(v1,v2,vn, a1, a2, a3, t2, t3, prod,NULL);
	
	mpf_set_str(v1,"2",10);
	mpf_set_str(v2,"-4",10);
	mpf_set_str(a1,"111",10);
	mpf_set_str(a2,"1130",10);
	mpf_set_str(a3,"3000",10);
	
	for(n=3;n<=100;n++){
		mpf_div(t2,a2,v2);
		mpf_mul(prod,v1,v2);
		mpf_div(t3,a3,prod);
		mpf_add(vn,a1,t3);
		mpf_sub(vn,vn,t2);
		
		if((n>=3&&n<=8) || n==20 || n==30 || n==50 || n==100){
			gmp_printf("\nv_%d : %.*Ff",n,(n==3)?1:(n>=4&&n<=7)?6:(n==8)?7:(n==20)?16:(n==30)?24:(n==50)?40:78,vn);
		}
		
		mpf_set(v1,v2);
		mpf_set(v2,vn);
	}
}

void healthySeries(){
	int n;
	
	mpf_t num,denom,result;
	mpq_t v1, v2, vn, a1, a2, a3, t2, t3, prod;
	
	mpf_inits(num,denom,result,NULL);
	mpq_inits(v1,v2,vn, a1, a2, a3, t2, t3, prod,NULL);
	
	mpq_set_str(v1,"2",10);
	mpq_set_str(v2,"-4",10);
	mpq_set_str(a1,"111",10);
	mpq_set_str(a2,"1130",10);
	mpq_set_str(a3,"3000",10);
	
	for(n=3;n<=100;n++){
		mpq_div(t2,a2,v2);
		mpq_mul(prod,v1,v2);
		mpq_div(t3,a3,prod);
		mpq_add(vn,a1,t3);
		mpq_sub(vn,vn,t2);
		
		if((n>=3&&n<=8) || n==20 || n==30 || n==50 || n==100){
			mpf_set_z(num,mpq_numref(vn));
			mpf_set_z(denom,mpq_denref(vn));
			mpf_div(result,num,denom);

			gmp_printf("\nv_%d : %.*Ff",n,(n==3)?1:(n>=4&&n<=7)?6:(n==8)?7:(n==20)?16:(n==30)?24:(n==50)?40:78,result);
		}
		
		mpq_set(v1,v2);
		mpq_set(v2,vn);
	}
}

int main()
{	
	mpz_t rangeProd;
	
	firstCase();
	
	printf("\n\nPathological Series : ");
	
	pathologicalSeries();
	
	printf("\n\nNow a bit healthier : ");
	
	healthySeries();

	return 0;
}

```

The reason I included the trivial case was the discovery that the value of 0.3 is stored inexactly even by GMP if 0.1 and 0.2 are set via the mpf_set_d function, a point observed also during the solution of the [[Currency]] task. Thus mpf_set_str has been used to set the values, for the 2nd task, a great learning was not to convert the values into floating points unless they have to be printed out. It's a polynomial with out a single floating point coefficient or exponent, a clear sign that the values must be treated as rationals for the highest accuracy. Thus there are two implementations for this task in the above code, one uses floating points, the other rationals. There is still a loss of accuracy even when floating points are used, probably during the conversion of a rational to a float.

```txt

0.1 + 0.2 = 0.30000000000000000000

Pathological Series :
v_3 : 18.5
v_4 : 9.378378
v_5 : 7.801153
v_6 : 7.154414
v_7 : 6.806785
v_8 : 6.5926328
v_20 : 6.0751649921786439
v_30 : 99.999999824974113455900000
v_50 : 100.0000000000000000000000000000000000000000
v_100 : 100.000000000000000000000000000000000000000000000000000000000000000000000000000000

Now a bit healthier :
v_3 : 18.5
v_4 : 9.378378
v_5 : 7.801153
v_6 : 7.154414
v_7 : 6.806785
v_8 : 6.5926328
v_20 : 6.0435521101892689
v_30 : 6.006786093031205758530000
v_50 : 6.0001758466271871889100000000000000000000
v_100 : 6.000000019319477929060000000000000000000000000000000000000000000000000000000000

```



## C sharp

{{trans|Visual Basic .NET}}
'''Compiler:''' Roslyn C# (language version >=7.3)
{{works with|.NET Framework|4.6.2}}
{{works with|.NET Core|2.1}}
{{libheader|BigRationalLibrary|1.0.0}}
See VB.NET entry for details (Single is float in C#; Double is double, and Decimal is decimal).

The following sections source code must be located in a single file.


```csharp
#define USE_BIGRATIONAL
#define BANDED_ROWS
#define INCREASED_LIMITS

using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Numerics;
using Numerics;

using static Common;
using static Task1;
using static Task2;
using static Task3;

#if !USE_BIGRATIONAL
// Mock structure to make test code work.
struct BigRational
{
    public override string ToString() => "NOT USING BIGRATIONAL";
    public static explicit operator decimal(BigRational value) => -1;
}
#endif

static class Common
{
    public const string FMT_STR = "{0,4}   {1,-15:G9}   {2,-24:G17}   {3,-32}   {4,-32}";
    public static string Headings { get; } =
        string.Format(
            CultureInfo.InvariantCulture,
            FMT_STR,
            new[] { "N", "Single", "Double", "Decimal", "BigRational (rounded as Decimal)" });

    [Conditional("BANDED_ROWS")]
    static void SetConsoleFormat(int n)
    {
        if (n % 2 == 0)
        {
            Console.BackgroundColor = ConsoleColor.Black;
            Console.ForegroundColor = ConsoleColor.White;
        }
        else
        {
            Console.BackgroundColor = ConsoleColor.White;
            Console.ForegroundColor = ConsoleColor.Black;
        }
    }

    public static string FormatOutput(int n, (float sn, double db, decimal dm, BigRational br) x)
    {
        SetConsoleFormat(n);
        return string.Format(CultureInfo.CurrentCulture, FMT_STR, n, x.sn, x.db, x.dm, (decimal)x.br);
    }

    static void Main()
    {
        WrongConvergence();

        Console.WriteLine();
        ChaoticBankSociety();

        Console.WriteLine();
        SiegfriedRump();

        SetConsoleFormat(0);
    }
}
```



### Task 1: Converging sequence

'''See''' [[#VB.NET Task 1]]


```csharp
static class Task1
{
    public static IEnumerable<float> SequenceSingle()
    {
        // n, n-1, and n-2
        float vn, vn_1, vn_2;
        vn_2 = 2;
        vn_1 = -4;

        while (true)
        {
            yield return vn_2;
            vn = 111f - (1130f / vn_1) + (3000f / (vn_1 * vn_2));
            vn_2 = vn_1;
            vn_1 = vn;
        }
    }

    public static IEnumerable<double> SequenceDouble()
    {
        // n, n-1, and n-2
        double vn, vn_1, vn_2;
        vn_2 = 2;
        vn_1 = -4;

        while (true)
        {
            yield return vn_2;
            vn = 111 - (1130 / vn_1) + (3000 / (vn_1 * vn_2));
            vn_2 = vn_1;
            vn_1 = vn;
        }
    }

    public static IEnumerable<decimal> SequenceDecimal()
    {
        // n, n-1, and n-2
        decimal vn, vn_1, vn_2;
        vn_2 = 2;
        vn_1 = -4;

        // Use constants to avoid calling the Decimal constructor in the loop.
        const decimal i11 = 111;
        const decimal i130 = 1130;
        const decimal E000 = 3000;

        while (true)
        {
            yield return vn_2;
            vn = i11 - (i130 / vn_1) + (E000 / (vn_1 * vn_2));
            vn_2 = vn_1;
            vn_1 = vn;
        }
    }

#if USE_BIGRATIONAL
    public static IEnumerable<BigRational> SequenceRational()
    {
        // n, n-1, and n-2
        BigRational vn, vn_1, vn_2;
        vn_2 = 2;
        vn_1 = -4;

        // Same reasoning as for decimal.
        BigRational i11 = 111;
        BigRational i130 = 1130;
        BigRational E000 = 3000;

        while (true)
        {
            yield return vn_2;
            vn = i11 - (i130 / vn_1) + (E000 / (vn_1 * vn_2));
            vn_2 = vn_1;
            vn_1 = vn;
        }
    }
#else
    public static IEnumerable<BigRational> SequenceRational()
    {
        while (true) yield return default;
    }
#endif

    static void IncreaseMaxN(ref int[] arr)
    {
        int[] tmp = new int[arr.Length + 1];
        arr.CopyTo(tmp, 0);
        tmp[arr.Length] = 1000;
        arr = tmp;
    }

    public static void WrongConvergence()
    {
        Console.WriteLine("Wrong Convergence Sequence:");

        int[] displayedIndices = { 3, 4, 5, 6, 7, 8, 20, 30, 50, 100 };
        IncreaseMaxN(ref displayedIndices);

        var indicesSet = new HashSet<int>(displayedIndices);

        Console.WriteLine(Headings);

        int n = 1;
        // Enumerate the implementations in parallel as tuples.
        foreach (var x in SequenceSingle()
            .Zip(SequenceDouble(), (sn, db) => (sn, db))
            .Zip(SequenceDecimal(), (a, dm) => (a.sn, a.db, dm))
            .Zip(SequenceRational(), (a, br) => (a.sn, a.db, a.dm, br)))
        {
            if (n > displayedIndices.Max()) break;

            if (indicesSet.Contains(n))
                Console.WriteLine(FormatOutput(n, x));

            n++;
        }
    }
}
```


{{out}}

```txt
Wrong Convergence Sequence:
   N   Single            Double                     Decimal                            BigRational (rounded as Decimal)
   3   18.5              18.5                       18.5                               18.5
   4   9.37837982        9.378378378378379          9.378378378378378378378378378      9.378378378378378378378378378
   5   7.80116463        7.8011527377521688         7.80115273775216138328530259       7.8011527377521613832853025937
   6   7.15456009        7.1544144809753334         7.154414480975249353527890606      7.1544144809752493535278906539
   7   6.80883026        6.8067847369248113         6.806784736923632983941755925      6.8067847369236329839417565963
   8   6.62275314        6.592632768721792          6.592632768704438392741992887      6.5926327687044383927420027764
  20   100               98.349503122165359         6.04355210719488789087813234       6.0435521101892688677774773641
  30   100               99.999999999998934         101.88552052291609961584734802     6.006786093031205758530554048
  50   100               100                        100.00000000000000000000000068     6.0001758466271871889456140207
 100   100               100                        100.0                              6.0000000193194779291040868034
1000   100               100                        100.0                              6.0000000000000000000000000000
```



### Task 2: The Chaotic Bank Society

'''See''' [[#VB.NET Task 2]]


```csharp
static class Task2
{
    public static IEnumerable<float> ChaoticBankSocietySingle()
    {
        float balance = (float)(Math.E - 1);

        for (int year = 1; ; year++)
            yield return balance = (balance * year) - 1;
    }

    public static IEnumerable<double> ChaoticBankSocietyDouble()
    {
        double balance = Math.E - 1;

        for (int year = 1; ; year++)
            yield return balance = (balance * year) - 1;
    }

    public static IEnumerable<decimal> ChaoticBankSocietyDecimal()
    {
        // 27! is the largest factorial decimal can represent.
        decimal balance = CalculateEDecimal(27) - 1;

        for (int year = 1; ; year++)
            yield return balance = (balance * year) - 1;
    }

#if USE_BIGRATIONAL
    public static IEnumerable<BigRational> ChaoticBankSocietyRational()
    {
        // 100 iterations is precise enough for 25 years.
        BigRational brBalance = CalculateERational(100) - 1;

        for (int year = 1; ; year++)
            yield return brBalance = (brBalance * year) - 1;
    }
#else
    public static IEnumerable<BigRational> ChaoticBankSocietyRational()
    {
        while (true) yield return default;
    }
#endif

    public static decimal CalculateEDecimal(int terms)
    {
        decimal e = 1;
        decimal fact = 1;
        for (int i = 1; i <= terms; i++)
        {
            fact *= i;
            e += decimal.One / fact;
        }

        return e;
    }

#if USE_BIGRATIONAL
    public static BigRational CalculateERational(int terms)
    {
        BigRational e = 1;
        BigRational fact = 1;
        for (int i = 1; i < terms; i++)
        {
            fact *= i;
            e += BigRational.Invert(fact);
        }

        return e;
    }
#endif

    [Conditional("INCREASED_LIMITS")]
    static void InceaseMaxYear(ref int year) => year = 40;

    public static void ChaoticBankSociety()
    {
        Console.WriteLine("Chaotic Bank Society:");
        Console.WriteLine(Headings);

        int maxYear = 25;
        InceaseMaxYear(ref maxYear);

        int i = 0;
        foreach (var x in ChaoticBankSocietySingle()
            .Zip(ChaoticBankSocietyDouble(), (sn, db) => (sn, db))
            .Zip(ChaoticBankSocietyDecimal(), (a, dm) => (a.sn, a.db, dm))
            .Zip(ChaoticBankSocietyRational(), (a, br) => (a.sn, a.db, a.dm, br)))
        {
            if (i >= maxYear) break;
            Console.WriteLine(FormatOutput(i + 1, x));
            i++;
        }
    }
}
```


{{out}}

```txt
Chaotic Bank Society:
   N   Single            Double                     Decimal                            BigRational (rounded as Decimal)
   1   0.718281865       0.71828182845904509        0.7182818284590452353602874714     0.7182818284590452353602874713
   2   0.43656373        0.43656365691809018        0.4365636569180904707205749428     0.4365636569180904707205749427
   3   0.309691191       0.30969097075427054        0.3096909707542714121617248284     0.3096909707542714121617248281
   4   0.238764763       0.23876388301708218        0.2387638830170856486468993136     0.2387638830170856486468993124
   5   0.193823814       0.1938194150854109         0.1938194150854282432344965680     0.1938194150854282432344965623
   6   0.162942886       0.16291649051246537        0.1629164905125694594069794080     0.1629164905125694594069793739
   7   0.140600204       0.14041543358725761        0.1404154335879862158488558560     0.1404154335879862158488556174
   8   0.124801636       0.12332346869806088        0.1233234687038897267908468480     0.1233234687038897267908449393
   9   0.123214722       0.10991121828254791        0.1099112183350075411176216320     0.1099112183350075411176044541
  10   0.232147217       0.099112182825479067       0.0991121833500754111762163200     0.0991121833500754111760445416
  11   1.55361938        0.090234011080269738       0.0902340168508295229383795200     0.0902340168508295229364899583
  12   17.6434326        0.082808132963236858       0.0828082022099542752605542400     0.0828082022099542752378795006
  13   228.364624        0.076505728522079153       0.0765066287294055783872051200     0.0765066287294055780924335089
  14   3196.10474        0.071080199309108139       0.0710928022116780974208716800     0.0710928022116780932940691248
  15   47940.5703        0.066202989636622078       0.0663920331751714613130752000     0.0663920331751713994110368720
  16   767048.125        0.059247834185953252       0.0622725308027433810092032000     0.0622725308027423905765899521
  17   13039817          0.0072131811612052843      0.0586330236466374771564544000     0.0586330236466206398020291865
  18   234716704         -0.87016273909830488       0.0553944256394745888161792000     0.0553944256391715164365253585
  19   4.45961728E+09    -17.533092042867793        0.0524940871500171875074048000     0.0524940871442588122939818127
  20   8.91923497E+10    -351.66184085735586        0.0498817430003437501480960000     0.0498817428851762458796362544
  21   1.87303933E+12    -7385.898658004473         0.0475166030072187531100160000     0.0475166005887011634723613427
  22   4.12068642E+13    -162490.77047609841        0.0453652661588125684203520000     0.0453652129514255963919495414
  23   9.47757884E+14    -3737288.7209502636        0.0434011216526890736680960000     0.0433998978827887170148394524
  24   2.27461897E+16    -89694930.302806318        0.0416269196645377680343040000     0.0415975491869292083561468582
  25   5.68654735E+17    -2242373258.570158         0.0406729916134442008576000000     0.0399387296732302089036714552
  26   1.47850229E+19    -58301704723.824112        0.0574977819495492222976000000     0.0384069715039854314954578354
  27   3.99195623E+20    -1574146027544.251         0.5524401126378290020352000000     0.0369882306076066503773615576
  28   1.11774772E+22    -44076088771240.031        14.468323153859212056985600000     0.0356704570129862105661236148
  29   3.24146835E+23    -1278206574365962          418.58137146191714965258240000     0.0344432533766001064175848308
  30   9.72440521E+24    -38346197230978864         12556.441143857514489577472000     0.0332976012980031925275449256
  31   3.01456563E+26    -1.1887321141603448E+18    389248.67545958294917690163200     0.0322256402380989683538926936
  32   9.64661E+27       -3.8039427653131035E+19    12455956.614706654373660852224     0.0312204876191669873245661979
  33   3.18338125E+29    -1.2553011125533242E+21    411046567.28531959433080812339     0.0302760914325105817106845313
  34   1.08234959E+31    -4.268023782681302E+22     13975583286.700866207247476195     0.0293871087053597781632740646
  35   3.78822341E+32    -1.4938083239384556E+24    489145415033.53031725366166682     0.0285488046875922357145922644
  36   1.36376043E+34    -5.3777099661784406E+25    17609234941206.091421131820006     0.0277569687533204857253215188
  37   5.04591372E+35    -1.989752687486023E+27     651541692824624.38258187734022     0.0270078438728579718368961961
  38   1.91744716E+37    -7.5610602124468873E+28    24758584327335725.538111338928     0.0262980671686029298020554545
  39   ∞                 -2.9488134828542859E+30    965584788766093294.9863422182      0.0256246195755142622801627278
  40   ∞                 -1.1795253931417144E+32    38623391550643731798.453688728     0.0249847830205704912065091156
```


===Task 3: Rump's example===
'''See''' [[#VB.NET Task 3]]


```csharp
static class Task3
{
    public static float SiegfriedRumpSingle(float a, float b)
    {
        float
            a2 = a * a,
            b2 = b * b,
            b4 = b2 * b2,
            b6 = b4 * b2
            ;

        // Non-integral literals must be coerced to single using the type suffix.
        return 333.75f * b6 +
            (a2 * (
                11 * a2 * b2 -
                b6 -
                121 * b4 -
                2)) +
            5.5f * b4 * b4 +
            a / (2 * b);
    }

    public static double SiegfriedRumpDouble(double a, double b)
    {
        double
            a2 = a * a,
            b2 = b * b,
            b4 = b2 * b2,
            b6 = b4 * b2
            ;

        // Non-integral literals are doubles by default.
        return
            333.75 * b6
            + a2 * (
                11 * a2 * b * b
                - b6
                - 121 * b4
                - 2)
            + 5.5 * b4 * b4
            + a / (2 * b);
    }

    public static decimal SiegfriedRumpDecimal(decimal a, decimal b)
    {
        decimal
            a2 = a * a,
            b2 = b * b,
            b4 = b2 * b2,
            b6 = b4 * b2
            ;

        // The same applies for decimal.
        return
            333.75m * b6
            + a2 * (
                11 * a2 * b * b
                - b6
                - 121 * b4
                - 2)
            + 5.5m * b4 * b4
            + a / (2 * b);
    }

#if USE_BIGRATIONAL
    public static BigRational SiegfriedRumpRational(BigRational a, BigRational b)
    {
        // Use mixed number constructor to maintain exact precision (333+3/4, 5+1/2).
        var c1 = new BigRational(33375, 100);
        var c2 = new BigRational(55, 10);

        return c1 * BigRational.Pow(b, 6)
            + (a * a * (
                11 * a * a * b * b
                - BigRational.Pow(b, 6)
                - 121 * BigRational.Pow(b, 4)
                - 2))
            + c2 * BigRational.Pow(b, 8)
            + a / (2 * b);
    }
#else
    public static IEnumerable<BigRational> SiegfriedRumpRational()
    {
        while (true) yield return default;
    }
#endif

    public static void SiegfriedRump()
    {
        Console.WriteLine("Siegfried Rump Formula");
        int a = 77617;
        int b = 33096;

        Console.Write("Single: ");
        float sn = SiegfriedRumpSingle(a, b);
        Console.WriteLine("{0:G9}", sn);
        Console.WriteLine();

        Console.Write("Double: ");
        double db = SiegfriedRumpDouble(a, b);
        Console.WriteLine("{0:G17}", db);
        Console.WriteLine();

        Console.WriteLine("Decimal:");
        decimal dm = 0;
        try
        {
            dm = SiegfriedRumpDecimal(a, b);
        }
        catch (OverflowException ex)
        {
            Console.WriteLine("Exception: " + ex.Message);
        }
        Console.WriteLine($"  {dm}");
        Console.WriteLine();

        Console.WriteLine("BigRational:");
        BigRational br = SiegfriedRumpRational(a, b);
        Console.WriteLine($"  Rounded: {(decimal)br}");
        Console.WriteLine($"  Exact: {br}");
    }
}
```


{{out}}
Note that the output for Single is slightly different from VB.

```txt
Siegfried Rump Formula
Single: -6.338253E+29

Double: -2.3611832414348226E+21

Decimal:
Exception: Value was either too large or too small for a Decimal.
  0

BigRational:
  Rounded: -0.8273960599468213681411650955
  Exact: -54767/66192
```



## Clojure

In Clojure, rational numbers are a first class data type!  This allow us to avoid these floating point calculation problems.  As long as the operations all involve integers, rational numbers will automatically be used behind the scenes.  If for some twisted reason you really wanted to introduce the error, you could change a number to a floating point in the equation to force floating point calculations instead.   

### Task 1: Converging Sequence


```clojure
(def converge-to-six ((fn task1 [a b] (lazy-seq (cons a (task1 b (+ (- 111 (/ 1130 b)) (/ 3000 (* b a))))))) 2 -4))
(def values [3 4 5 6 7 8 20 30 50 100])
; print decimal values:
(pprint (sort (zipmap values (map double (map #(nth converge-to-six (dec %)) values)))))
; print rational values:
(pprint (sort (zipmap values (map #(nth converge-to-six (dec %)) values))))
```


{{out}}

```txt

decimal representation:
([3 18.5]
 [4 9.378378378378377]
 [5 7.801152737752161]
 [6 7.154414480975249]
 [7 6.806784736923633]
 [8 6.592632768704438]
 [20 6.043552110189269]
 [30 6.006786093031206]
 [50 6.000175846627187]
 [100 6.000000019319478])

rational representation:
([3 37/2]
 [4 347/37]
 [5 2707/347]
 [6 19367/2707]
 [7 131827/19367]
 [8 869087/131827]
 [20 2646751398406607/437946318679747]
 [30 164874117215934539909207/27447975450168574034347]
 [50
  606122140256603032959120809808954954407/101017396114701505075745671950377583547]
 [100
  489988959736444127362399646251257712574995486122651802567073927074333549467407/81664826359787052820062672571300097001570504462706488724837986255629281356547])

```



### Task 2: Chaotic Bank Society


```clojure
(def e-ratio 106246577894593683/39085931702241241)
(defn bank [n m] (- (* n m) 1))
(double (reduce bank (- e-ratio 1) (range 1 26)))
```


{{out}}

```txt

Balance after 25 (decimal representation):
0.03993873004901714

Balance after 25 (rational representation):
1561042474970134/39085931702241241

```


===Task 3: Rump's Example===

```clojure
(defn rump [a b]
  (+ (* (rationalize 333.75) (expt b 6))
      (* (expt a 2)
          (- (* 11 (expt a 2) (expt b 2)) (expt b 6) (* 121 (expt b 4)) 2))
      (* (rationalize 5.5) (expt b 8))
      (/ a (* 2 b))))
; Using BigInt numeric literal style to avoid integer overflow
(double (rump 77617 33096N))
```


{{out}}

```txt

-0.8273960599468214

```



## Excel

{{works with|Excel 2003|Excel 2015}}
'''A sequence that seems to converge to a wrong limit'''

<lang>  A1: 2
  A2: -4
  A3: =111-1130/A2+3000/(A2*A1)
  A4: =111-1130/A3+3000/(A3*A2)
  ...
```

The result converges to the wrong limit!
{{out}}
<pre style="height:20ex">
       A 
   1      2
   2     -4
   3     18.5
   4      9.378378378
   5      7.801152738
   6      7.154414481
   7      6.806784737
   8      6.592632769
   9      6.449465934
  10      6.348452061
  11      6.274438663
  12      6.218696769
  13      6.175853856
  14      6.142627170
  15      6.120248705
  16      6.166086560
  17      7.235021166
  18     22.06207846
  19     78.57557489
  20     98.34950312
  21     99.89856927
  22     99.99387099
  23     99.99963039
  24     99.99997773
  25     99.99999866
  26     99.99999992
  27    100
 ...
  30    100
 ...
  40    100
 ...
  50    100
 ...
 100    100

```



## Factor

These problems are straightforward due to Factor's rational numbers. One needs only take care not to introduce floating point values to the calculations.

```factor
USING: formatting fry io kernel locals math math.functions
math.ranges sequences ;
IN: rosetta-code.pathological

: next2 ( x y -- y z )
    swap dupd dupd '[ 111 1130 _ / - 3000 _ _ * / + ] call ;

: pathological-sequence ( -- seq )
    2 -4 100 [ next2 dup ] replicate 2nip { 0 2 -4 } prepend ;

: show-sequence ( -- )
    { 3 4 5 6 7 8 20 30 50 100 } dup pathological-sequence nths
    [ "n = %-3d %21.16f\n" printf ] 2each ;

CONSTANT: e 106246577894593683/39085931702241241
: balance ( n -- x ) [1,b] e 1 - [ * 1 - ] reduce ;

:: f ( a b -- x )
    333+3/4 b 6 ^ * 11 a sq b sq * * b 6 ^ - b 4 ^ 121 * - 2 - a
    sq * b 8 ^ 5+1/2 * a 2 b * / + + + ;

: pathological-demo ( -- )
    "Task 1 - Sequence convergence:" print show-sequence nl

    "Task 2 - Chaotic Bank fund after 25 years:" print
    25 balance "%.16f\n" printf nl

    "Task 3 - Siegfried Rump's example:" print
    77617 33096 f "77617 33096 f = %.16f\n" printf ;

MAIN: pathological-demo
```

{{out}}

```txt

Task 1 - Sequence convergence:
n = 3     18.5000000000000000
n = 4      9.3783783783783784
n = 5      7.8011527377521614
n = 6      7.1544144809752494
n = 7      6.8067847369236330
n = 8      6.5926327687044384
n = 20     6.0435521101892689
n = 30     6.0067860930312058
n = 50     6.0001758466271872
n = 100    6.0000000193194779

Task 2 - Chaotic Bank fund after 25 years:
0.0399387300490171

Task 3 - Siegfried Rump's example:
77617 33096 f = -0.8273960599468214

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

' As FB's native types have only 64 bit precision at most we need to use the
' C library, GMP v6.1.0, for arbitrary precision arithmetic

#Include Once "gmp.bi"
mpf_set_default_prec(640) '' 640 bit precision, enough for this exercise

Function v(n As UInteger, prev As __mpf_struct, prev2 As __mpf_struct) As __mpf_struct
  Dim As __mpf_struct a, b, c
  mpf_init(@a) : mpf_init(@b) : mpf_init(@c)
  If n = 0 Then mpf_set_ui(@a, 0UL)
  If n = 1 Then mpf_set_ui(@a, 2UL)
  If n = 2 Then mpf_set_si(@a, -4L)
  If n < 3 Then Return a 
  mpf_ui_div(@a, 1130UL, @prev)
  mpf_mul(@b, @prev, @prev2)
  mpf_ui_div(@c, 3000UL, @b)
  mpf_ui_sub(@b, 111UL, @a) 
  mpf_add(@a, @b, @c)
  mpf_clear(@b)
  mpf_clear(@c)
  Return a
End Function

Function f(a As Double, b As Double) As __mpf_Struct
  Dim As __mpf_struct temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8
  mpf_init(@temp1) : mpf_init(@temp2) : mpf_init(@temp3) : mpf_init(@temp4)
  mpf_init(@temp5) : mpf_init(@temp6) : mpf_init(@temp7) : mpf_init(@temp8)
  mpf_set_d(@temp1, a)               '' a
  mpf_set_d(@temp2, b)               '' b 
  mpf_set_d(@temp3, 333.75)          '' 333.75
  mpf_pow_ui(@temp4, @temp2, 6UL)    '' b ^ 6
  mpf_mul(@temp3, @temp3, @temp4)    '' 333.75 * b^6
  mpf_pow_ui(@temp5, @temp1, 2UL)    '' a^2
  mpf_pow_ui(@temp6, @temp2, 2UL)    '' b^2
  mpf_mul_ui(@temp7, @temp5, 11UL)   '' 11 * a^2
  mpf_mul(@temp7, @temp7, @temp6)    '' 11 * a^2 * b^2
  mpf_sub(@temp7, @temp7, @temp4)    '' 11 * a^2 * b^2 - b^6
  mpf_pow_ui(@temp4, @temp2, 4UL)    '' b^4
  mpf_mul_ui(@temp4, @temp4, 121UL)  '' 121 * b^4
  mpf_sub(@temp7, @temp7, @temp4)    '' 11 * a^2 * b^2 - b^6 - 121 * b^4
  mpf_sub_ui(@temp7, @temp7, 2UL)    '' 11 * a^2 * b^2 - b^6 - 121 * b^4 - 2
  mpf_mul(@temp7, @temp7, @temp5)    '' (11 * a^2 * b^2 - b^6 - 121 * b^4 - 2) * a^2
  mpf_add(@temp3, @temp3, @temp7)    '' 333.75 * b^6 + (11 * a^2 * b^2 - b^6 - 121 * b^4 - 2) * a^2
  mpf_set_d(@temp4, 5.5)             '' 5.5
  mpf_pow_ui(@temp5, @temp2, 8UL)    '' b^8  
  mpf_mul(@temp4, @temp4, @temp5)    '' 5.5 * b^8
  mpf_add(@temp3, @temp3, @temp4)    '' 333.75 * b^6 + (11 * a^2 * b^2 - b^6 - 121 * b^4 - 2) * a^2 + 5.5 * b^8
  mpf_mul_ui(@temp4, @temp2, 2UL)    '' 2 * b
  mpf_div(@temp5, @temp1, @temp4)    '' a / (2 * b)
  mpf_add(@temp3, @temp3, @temp5)    '' 333.75 * b^6 + (11 * a^2 * b^2 - b^6 - 121 * b^4 - 2) * a^2 + 5.5 * b^8 + a / (2 * b)
  mpf_clear(@temp1) : mpf_clear(@temp2) : mpf_clear(@temp4) : mpf_clear(@temp5)
  mpf_clear(@temp6) : mpf_clear(@temp7) : mpf_clear(@temp8)
  Return temp3
End Function

Dim As Zstring * 60 z
Dim As __mpf_struct result, prev, prev2
' We cache the two previous results to avoid recursive calls to v
For i As Integer = 1 To 100
  result = v(i, prev, prev2)
  If (i >= 3 AndAlso i <= 8) OrElse i = 20 OrElse i = 30 OrElse i = 50 OrElse i = 100 Then
    gmp_sprintf(@z,"%53.50Ff",@result) '' express result to 50 decimal places
    Print "n ="; i , z
  End If 
  prev2 = prev
  prev = result    
Next

mpf_clear(@prev) : mpf_clear(@prev2) '' note : prev = result

Dim As __mpf_struct e, balance, ii, temp
mpf_init(@e) : mpf_init(@balance) : mpf_init(@ii) : mpf_init(@temp)
mpf_set_str(@e, "2.71828182845904523536028747135266249775724709369995", 10) '' e to 50 decimal places
mpf_sub_ui(@balance, @e, 1UL)

For i As ULong = 1 To 25
  mpf_set_ui(@ii, i)  
  mpf_mul(@temp, @balance, @ii)
  mpf_sub_ui(@balance, @temp, 1UL) 
Next  

Print
Print "Chaotic B/S balance after 25 years : ";
gmp_sprintf(@z,"%.16Ff",@balance) '' express balance to 16 decimal places
Print z
mpf_clear(@e) : mpf_clear(@balance) : mpf_clear(@ii) : mpf_clear(@temp) 

Print
Dim rump As __mpf_struct
rump = f(77617.0, 33096.0)
gmp_sprintf(@z,"%.16Ff", @rump) '' express rump to 16 decimal places
Print "f(77617.0, 33096.0) = "; z

Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

n = 3         18.50000000000000000000000000000000000000000000000000
n = 4          9.37837837837837837837837837837837837837837837837838
n = 5          7.80115273775216138328530259365994236311239193083573
n = 6          7.15441448097524935352789065386036202438123383819727
n = 7          6.80678473692363298394175659627200908762327670780193
n = 8          6.59263276870443839274200277636599482655298231773461
n = 20         6.04355211018926886777747736409754013318771500000612
n = 30         6.00678609303120575853055404795323970583307231443837
n = 50         6.00017584662718718894561402074719546952373517709933
n = 100        6.00000001931947792910408680340358571502435067543695

Chaotic B/S balance after 25 years : 0.0399387296732302

f(77617.0, 33096.0) = -0.8273960599468214

```


=={{header|Fōrmulæ}}==

In [http://wiki.formulae.org/Pathological_floating_point_problems this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Fortran


### Compute from the hip

Problems arise because the floating-point arithmetic as performed by digital computers has only an oblique relationship to the arithmetic of '''Real''' numbers: many axia are violated, even if only by a little, and in certain situations. Most seriously, only a limited precision is available even if the floating-point variables are declared via such words as "REAL". Actions such as subtraction (of nearly-equal values) can in one step destroy many or all the digits of accuracy of the value being developed.

Fortran's only "built-in" assistance in this is provided via the ability to declare floating-point variables DOUBLE PRECISION, and on some systems, QUADRUPLE PRECISION is available. Earlier systems such as the IBM1620 supported decimal arithmetic of up to ninety-nine decimal digits (via hardware!), and the Fortran II compiler offered limited access to this via a control card at the start of the source file of the form <code>*ffkks</code> but the allowed range of ''ff'' was only 2 to 28, not 99. More modern compilers have abandoned this ability. Although the allowable syntax could admit something like <code>REAL*496</code>, the highest usually available is <code>REAL*8</code> for 64-bit floating-point numbers, and perhaps <code>REAL*10</code>, or <code>REAL*16</code> for QUADRUPLE PRECISION. Special "bignumber" arithmetic routines can be written supporting floating-point (or integer, or rational) arithmetic of hundreds or thousands or more words of storage per number, but this is not a standard arrangement.

Otherwise, a troublesome calculation might be recast into a different form that avoids a catastrophic loss of precision, probably after a lot of careful and difficult analysis and exploration and ingenuity.

Here, no such attempt is made. In the spirit of Formula Translation, this is a direct translation of the specified formulae into Fortran, with single and double precision results on display. There is no REAL*16 option, nor the REAL*10 that some systems allow to correspond to the eighty-bit floating-point format supported by the floating-point processor. The various integer constants cause no difficulty and I'm not bothering with writing them as <integer>.0 - the compiler can deal with this. The constants with fractional parts happen to be exactly represented in binary so there is no fuss over 333.75 and 333.75D0 whereas by contrast 0.1 and 0.1D0 are not equal. Similarly, there is no attempt to rearrange the formulae, for instance to have <code>A**2 * B**2</code> replaced by <code>(A*B)**2</code>, nor worry over <code>B**8</code> where 33096**8 = 1.439E36 and the largest possible single-precision number is 3.4028235E+38, in part because arithmetic within an expression can be conducted with a greater dynamic range. Most of all, no attention has been given to the subtractions...

This would be F77 style Fortran, except for certain conveniences offered by F90, especially the availability of generic functions such as EXP whose type is determined by the type of its parameter, rather than having to use EXP and DEXP for single and double precision respectively, or else... The END statement for subroutines and functions names the routine being ended, a useful matter to have checked. 
```Fortran
      SUBROUTINE MULLER
       REAL*4 VN,VNL1,VNL2	!The exact precision and dynamic range
       REAL*8 WN,WNL1,WNL2	!Depends on the format's precise usage of bits.
       INTEGER I		!A stepper.
        WRITE (6,1)		!A heading.
    1   FORMAT ("Muller's sequence should converge to six...",/
     1   "  N     Single      Double")
        VNL1 = 2; VN = -4	!Initialise for N = 2.
        WNL1 = 2; WN = -4	!No fractional parts yet.
        DO I = 3,36			!No point going any further.
          VNL2 = VNL1; VNL1 = VN		!Shuffle the values along one place.
          WNL2 = WNL1; WNL1 = WN		!Ready for the next term's calculation.
          VN = 111 - 1130/VNL1 + 3000/(VNL1*VNL2)	!Calculate the next term.
          WN = 111 - 1130/WNL1 + 3000/(WNL1*WNL2)	!In double precision.
          WRITE (6,2) I,VN,WN			!Show both.
    2     FORMAT (I3,F12.7,F21.16)		!With too many fractional digits.
        END DO				!On to the next term.
      END SUBROUTINE MULLER	!That was easy. Too bad the results are wrong.

      SUBROUTINE CBS		!The Chaotic Bank Society.
       INTEGER YEAR	!A stepper.
       REAL*4 V		!The balance.
       REAL*8 W		!In double precision as well.
        V = 1; W = 1		!Initial values, without dozy 1D0 stuff.
        V = EXP(V) - 1		!Actual initial value desired is e - 1..,
        W = EXP(W) - 1		!This relies on double-precision W selecting DEXP.
        WRITE (6,1)		!Here we go.
    1   FORMAT (///"The Chaotic Bank Society in action..."/"Year")
        WRITE (6,2) 0,V,W	!Show the initial deposit.
    2   FORMAT (I3,F16.7,F28.16)
        DO YEAR = 1,25		!Step through some years.
          V = V*YEAR - 1	!The specified procedure.
          W = W*YEAR - 1	!The compiler handles type conversions.
          WRITE (6,2) YEAR,V,W	!The current balance.
        END DO			!On to the following year.
      END SUBROUTINE CBS	!Madness!

      REAL*4 FUNCTION SR4(A,B)	!Siegfried Rump's example function of 1988.
       REAL*4 A,B
        SR4 = 333.75*B**6
     1      + A**2*(11*A**2*B**2 - B**6 - 121*B**4 - 2)
     2      + 5.5*B**8 + A/(2*B)
      END FUNCTION SR4
      REAL*8 FUNCTION SR8(A,B)	!Siegfried Rump's example function.
       REAL*8 A,B
        SR8 = 333.75*B**6	!.75 is exactly represented in binary.
     1      + A**2*(11*A**2*B**2 - B**6 - 121*B**4 - 2)
     2      + 5.5*B**8 + A/(2*B)!.5 is exactly represented in binary.
      END FUNCTION SR8

      PROGRAM POKE
      REAL*4 V	!Some example variables.
      REAL*8 W	!Whose type goes to the inquiry function.
      WRITE (6,1) RADIX(V),DIGITS(V),"single",DIGITS(W),"double"
    1   FORMAT ("Floating-point arithmetic is conducted in base ",I0,/
     1   2(I3," digits for ",A," precision",/))
      WRITE (6,*) "Single precision limit",HUGE(V)
      WRITE (6,*) "Double precision limit",HUGE(W)
      WRITE (6,*)

      CALL MULLER

      CALL CBS

      WRITE (6,10)
   10 FORMAT (///"Evaluation of Siegfried Rump's function of 1988",
     1 " where F(77617,33096) = -0.827396059946821")
      WRITE (6,*) "Single precision:",SR4(77617.0,33096.0)
      WRITE (6,*) "Double precision:",SR8(77617.0D0,33096.0D0)	!Must match the types.
      END
```



### =Output=

Floating-point numbers in single and double precision use the "implicit leading one" binary format on this system: there have been ''many'' variations across different computers over the years. One can write strange routines that will test the workings of arithmetic (and other matters) so as to determine the situation on the computer of the moment, but F90 introduced special "inquiry" routines that reveal certain details as standard. This information could be used to make choices amongst calculation paths and ploys  appropriate for different results, at of course a large expenditure in thought to produce a compound scheme that will (should?) work correctly on a variety of computers. No such effort has been made  here!

Fifty-three binary digits corresponds to 15·95 decimal digits: there is no simple conversion so the usual ploy is to show additional decimal digits, knowing that the lower-order digits will be fuzz due to the binary/decimal conversion. The "Muller" sequence has for its fourth term 9.3783783783783790 - note that this is a recurring sequence, and its precision is ''less'' than the displayed sixteen decimal digits (seventeen digits of "precision" are on show) - when trying for maximum accuracy, converting a binary value to decimal adds confusion.


```txt

Floating-point arithmetic is conducted in base 2
 24 digits for single precision
 53 digits for double precision

 Single precision limit  3.4028235E+38
 Double precision limit  1.797693134862316E+308

Muller's sequence should converge to six...
  N     Single      Double
  3  18.5000000  18.5000000000000000
  4   9.3783779   9.3783783783783790
  5   7.8011475   7.8011527377521688
  6   7.1543465   7.1544144809753334
  7   6.8058305   6.8067847369248113
  8   6.5785794   6.5926327687217920
  9   6.2355156   6.4494659340539329
 10   2.9135900   6.3484520607466237
 11-111.7097931   6.2744386627281159
 12 111.8982391   6.2186967685821628
 13 100.6615448   6.1758538558153901
 14 100.0406036   6.1426271704810063
 15 100.0024948   6.1202487045701588
 16 100.0001526   6.1660865595980994
 17 100.0000076   7.2350211655349312
 18 100.0000000  22.0620784635257934
 19 100.0000000  78.5755748878722358
 20 100.0000000  98.3495031221653591
 21 100.0000000  99.8985692661829034
 22 100.0000000  99.9938709889027848
 23 100.0000000  99.9996303872863450
 24 100.0000000  99.9999777306794897
 25 100.0000000  99.9999986592166863
 26 100.0000000  99.9999999193218088
 27 100.0000000  99.9999999951477605
 28 100.0000000  99.9999999997082796
 29 100.0000000  99.9999999999824638
 30 100.0000000  99.9999999999989342
 31 100.0000000  99.9999999999999289
 32 100.0000000  99.9999999999999858
 33 100.0000000 100.0000000000000000
 34 100.0000000 100.0000000000000000
 35 100.0000000 100.0000000000000000
 36 100.0000000 100.0000000000000000



The Chaotic Bank Society in action...
Year
  0       1.7182819          1.7182818284590453
  1       0.7182819          0.7182818284590453
  2       0.4365637          0.4365636569180906
  3       0.3096912          0.3096909707542719
  4       0.2387648          0.2387638830170875
  5       0.1938238          0.1938194150854375
  6       0.1629429          0.1629164905126252
  7       0.1406002          0.1404154335883767
  8       0.1248016          0.1233234687070137
  9       0.1232147          0.1099112183631235
 10       0.2321472          0.0991121836312345
 11       1.5536194          0.0902340199435798
 12      17.6434326          0.0828082393229579
 13     228.3646240          0.0765071111984525
 14    3196.1047363          0.0710995567783357
 15   47940.5703125          0.0664933516750352
 16  767048.1250000          0.0638936268005637
 1713039817.0000000          0.0861916556095821
 18****************          0.5514498009724775
 19****************          9.4775462184770731
 20****************        188.5509243695414625
 21****************       3958.5694117603707127
 22****************      87087.5270587281556800
 23****************    2003012.1223507476970553
 24****************   48072289.9364179447293282
 25**************** 1201807247.4104485511779785



Evaluation of Siegfried Rump's function of 1988 where F(77617,33096) = -0.827396059946821
 Single precision: -1.1805916E+21
 Double precision: -1.1805916E+21

```


None of the results are remotely correct! In the absence of a Fortran compiler supporting still higher precision (such as quadruple, or REAL*16) only two options remain: either devise multi-word high-precision arithmetic routines and try again with even more brute-force, or, analyse the calculation with a view to finding a way to avoid the loss of accuracy with calculations conducted in the available precision.

Alternatively, do not present various intermediate results such as might give rise to doubts, nor yet entertain any doubts, just declare the answer to be what appears, and move on. In a letter from F.S. Acton, "A former student of mine now hands out millions of dollars for computation ... and he dismally estimates that 70% of the "answers" are worthless because of poor analysis and poor programming."


### On putting some thought to the matter


### =The Chaotic Bank Society=

From whom but an emissary of the Dark One could come a deposit of a ''transcendental'' sum of money? Following that lead, retreat from the swamp of finite-precision arithmetic to '''Real''' arithmetic, and consider the deposit's progress in a mathematical manner:
 Year        Deposit        =     Deposit.
    0        e - 1                  e - 1      Initial deposit.
    1       (e - 1).1 - 1           e - 2      At the end of the first year.
    2       (e - 2).2 - 1          2e - 5
    3      (2e - 5).3 - 1          6e - 16
    4      (6e - 16).4 - 1        24e - 65
    5     (24e - 65).5 - 1       120e - 326
    6    (120e - 326).6 - 1      720e - 1957
    7    (720e - 1957).7 - 1    5040e - 13700

Clearly, two numbers that are nearly equal are being subtracted, since the value of e is a little below three. For year n, the first term is e.n! (and here a pause to gloat over the arithmetic statement evaluator written in Turbo Pascal decades back whose precedence table had specially-crafted entries for factorial, so that e*n! was ''not'' evaluated as (e*n)!) The series expression for e is straightforward: e = 1 + 1/1! + 1/2! + 1/3! + 1/4! + 1/5! + ... so, for the deposit at the end of year six for example,

e.6! = 6! + 6!/1! + 6!/2! + 6!/3! + 6!/4! + 6!/5! + 6!/6! + 6!/7! + 6!/8! + 6!/9! ...

e.6! = 720 + 720 + 360 + 120 + 30 + 6 + 1 + 6!/7! + 6!/8! + 6!/9! ...

e.6! = 1957 + 6!(1/7! + 1/8! + 1/9! + ...

And obviously, the 1957 exactly cancels: so this is the difference between e and the series for e that has been truncated. Further, the remnant need not be calculated as (large number) times (small number) because the factorial terms cancel as well, so the result is

Deposit = 1/7 + 1/7.8 + 1/7.8.9 + ...

Unlike a recurrence formula whereby a new result is calculated from previous results (thereby incurring the possibility of rapid amplification of any errors), each year's value is produced ''ab initio'' via a series that is easily calculated and which converges rapidly without instability, ever more rapidly for larger n. Indeed, a one-term approximation would suffice for approximate results and in decimal the values for 9 and 99 and 999, ''etc.'' can be achieved at a glance with mental arithmetic: just over 1/10, or 1/100, or 1/1000, ''etc.'' Adding an approximate adjustment from the second term is not much more effort. No need for a thousand-digit value for e, nor any slogging through multi-precision arithmetic...

A simple function CBSERIES handles the special case deposit. The only question is how many terms of the series are required to produce a value accurate to the full precision in use. Thanks to the enquiry function EPSILON(x) offered by F90, the smallest number such that ''1 + eps'' differs from ''1'' for the precision of ''x'' is available without the need for cunning programming; this is a constant. An alternative form might be that EPSILON(X) returned the smallest number that, added to X, produced a result different from X in floating-point arithmetic of the precision of X - but this would not be a constant. Since the terms of the series are rapidly diminishing (and all are positive) a new term may be too small to affect the sum; this happens when S + T = S, or 1 + T/S = 1 + eps, thus the test in CBSERIES of T/S >= EPSILON(S) checks that the term affected the sum so that the loop stops for the first term that does not.

A misthimk had TINY(S) instead of EPSILON(S), and this demonstrates again the importance of providing output that shows the actual  behaviour of a scheme and comparing it to expectations, since it showed that over a hundred terms were being calculated and the last term was tiny. Routine TINY(S) reports the smallest possible floating-point number in the precision of its parameter, which is not what is wanted! EPSILON(S) is tiny, but not so tiny as TINY(S). 2·220446049250313E-016 instead of 2·225073858507201E-308. 
```Fortran
      SUBROUTINE CBS	!The Chaotic Bank Society.
       INTEGER YEAR	!A stepper.
       REAL*4 V		!The balance.
       REAL*8 W		!In double precision as well.
       INTEGER NTERM	!Share information with CBSERIES.
       REAL*8 T		!So as to show workings.
        V = 1; W = 1		!Initial values, without dozy 1D0 stuff.
        V = EXP(V) - 1		!Actual initial value desired is e - 1..,
        W = EXP(W) - 1		!This relies on double-precision W selecting DEXP.
        WRITE (6,1)		!Here we go.
    1   FORMAT (///"The Chaotic Bank Society in action...",/,
     *   "Year",9X,"Real*4",22X,"Real*8",12X,"Series summation",
     *   9X,"Last term",2X,"Terms.")
        WRITE (6,2) 0,V,W,CBSERIES(0),T,NTERM	!Show the initial deposit.
    2   FORMAT (I3,F16.7,2F28.16,D18.8,I7)	!Not quite 16-digit precision for REAL*8.
        DO YEAR = 1,25		!Step through some years.
          V = V*YEAR - 1	!The specified procedure.
          W = W*YEAR - 1	!The compiler handles type conversions.
          WRITE (6,2) YEAR,V,W,CBSERIES(YEAR),T,NTERM	!The current balance.
        END DO			!On to the following year.
        CONTAINS		!An alternative.
        REAL*8 FUNCTION CBSERIES(N)	!Calculates for the special deposit of e - 1.
         INTEGER N	!Desire the balance after year N for the deposit of e - 1.
         REAL*8 S	!Via a series summation.
          S = 0			!Start the summation.
          T = 1			!First term is 1/(N + 1)
          I = N			!Second is 1/[(N + 1)*(N + 2)], etc.
          NTERM = 0		!No terms so far.
    3       NTERM = NTERM + 1	!Here we go.
            I = I + 1		!Thus advance to the next divisor, and not divide by zero.
            T = T/I		!Thus not compute the products from scratch each time.
            S = S + T		!Add the term.
            IF (T/S .GE. EPSILON(S)) GO TO 3	!If they're still making a difference, another.
          CBSERIES = S		!Convergence is ever-faster as N increases.
        END FUNCTION CBSERIES	!So this is easy.
      END SUBROUTINE CBS	!Madness! 
```


And the output is (slightly decorated to show correct digits in bold):

 The Chaotic Bank Society in action...
 Year         Real*4                      Real*8            Series summation         Last term  Terms.
   0       '''1.718281'''9          '''1.718281828459045'''3          1.7182818284590455    0.15619207D-15     18
   1       '''0.718281'''9          '''0.718281828459045'''3          0.7182818284590450    0.15619207D-15     17
   2       '''0.436563'''7          '''0.436563656918090'''6          0.4365636569180904    0.16441270D-16     17
   3       '''0.30969'''12          '''0.309690970754271'''9          0.3096909707542714    0.49323811D-16     16
   4       '''0.23876'''48          '''0.23876388301708'''75          0.2387638830170856    0.98647623D-17     16
   5       '''0.1938'''238          '''0.1938194150854'''375          0.1938194150854282    0.23487529D-17     16
   6       '''0.1629'''429          '''0.162916490512'''6252          0.1629164905125695    0.14092518D-16     15
   7       '''0.140'''6002          '''0.14041543358'''83767          0.1404154335879862    0.44839829D-17     15
   8       '''0.12'''48016          '''0.12332346870'''70137          0.1233234687038897    0.15596462D-17     15
   9       '''0.1'''232147          '''0.1099112183'''631235          0.1099112183350076    0.14036816D-16     14
  10       '''0.'''2321472          '''0.099112183'''6312345          0.0991121833500754    0.58486733D-17     14
  11       1.5536194          '''0.09023401'''99435798          0.0902340168508295    0.25734163D-17     14
  12      17.6434326          '''0.0828082'''393229579          0.0828082022099543    0.11877306D-17     14
  13     228.3646240          '''0.07650'''71111984525          0.0765066287294056    0.15440498D-16     13
  14    3196.1047363          '''0.07109'''95567783357          0.0710928022116781    0.80061839D-17     13
  15   47940.5703125          '''0.066'''4933516750352          0.0663920331751714    0.42890271D-17     13
  16  767048.1250000          '''0.06'''38936268005637          0.0622725308027424    0.23663598D-17     13
  1713039817.0000000          '''0.0'''861916556095821          0.0586330236466206    0.13409372D-17     13
  18****************          '''0.'''5514498009724775          0.0553944256391715    0.77860870D-18     13
  19****************          9.4775462184770731          0.0524940871442588    0.46229891D-18     13
  20****************        188.5509243695414625          0.0498817428851763    0.92459783D-17     12
  21****************       3958.5694117603707127          0.0475166005887012    0.58838044D-17     12
  22****************      87087.5270587281556800          0.0453652129514256    0.38071675D-17     12
  23****************    2003012.1223507476970553          0.0433998978827887    0.25018530D-17     12
  24****************   48072289.9364179447293282          0.0415975491869292    0.16679020D-17     12
  25**************** 1201807247.4104485511779785          0.0399387296732302    0.11269608D-17     12


## Go


```go
package main

import (
    "fmt"
    "math/big"
)

func main() {
    sequence()
    bank()
    rump()
}

func sequence() {
    // exact computations using big.Rat
    var v, v1 big.Rat
    v1.SetInt64(2)
    v.SetInt64(-4)
    n := 2
    c111 := big.NewRat(111, 1)
    c1130 := big.NewRat(1130, 1)
    c3000 := big.NewRat(3000, 1)
    var t2, t3 big.Rat
    r := func() (vn big.Rat) {
        vn.Add(vn.Sub(c111, t2.Quo(c1130, &v)), t3.Quo(c3000, t3.Mul(&v, &v1)))
        return
    }
    fmt.Println("  n  sequence value")
    for _, x := range []int{3, 4, 5, 6, 7, 8, 20, 30, 50, 100} {
        for ; n < x; n++ {
            v1, v = v, r()
        }
        f, _ := v.Float64()
        fmt.Printf("%3d %19.16f\n", n, f)
    }
}

func bank() {
    // balance as integer multiples of e and whole dollars using big.Int
    var balance struct{ e, d big.Int }
    // initial balance
    balance.e.SetInt64(1)
    balance.d.SetInt64(-1)
    // compute balance over 25 years
    var m, one big.Int
    one.SetInt64(1)
    for y := 1; y <= 25; y++ {
        m.SetInt64(int64(y))
        balance.e.Mul(&m, &balance.e)
        balance.d.Mul(&m, &balance.d)
        balance.d.Sub(&balance.d, &one)
    }
    // sum account components using big.Float
    var e, ef, df, b big.Float
    e.SetPrec(100).SetString("2.71828182845904523536028747135")
    ef.SetInt(&balance.e)
    df.SetInt(&balance.d)
    b.Add(b.Mul(&e, &ef), &df)
    fmt.Printf("Bank balance after 25 years: $%.2f\n", &b)
}

func rump() {
    a, b := 77617., 33096.
    fmt.Printf("Rump f(%g, %g): %g\n", a, b, f(a, b))
}

func f(a, b float64) float64 {
    // computations done with big.Float with enough precision to give
    // a correct answer.
    fp := func(x float64) *big.Float { return big.NewFloat(x).SetPrec(128) }
    a1 := fp(a)
    b1 := fp(b)
    a2 := new(big.Float).Mul(a1, a1)
    b2 := new(big.Float).Mul(b1, b1)
    b4 := new(big.Float).Mul(b2, b2)
    b6 := new(big.Float).Mul(b2, b4)
    b8 := new(big.Float).Mul(b4, b4)
    two := fp(2)
    t1 := fp(333.75)
    t1.Mul(t1, b6)
    t21 := fp(11)
    t21.Mul(t21.Mul(t21, a2), b2)
    t23 := fp(121)
    t23.Mul(t23, b4)
    t2 := new(big.Float).Sub(t21, b6)
    t2.Mul(a2, t2.Sub(t2.Sub(t2, t23), two))
    t3 := fp(5.5)
    t3.Mul(t3, b8)
    t4 := new(big.Float).Mul(two, b1)
    t4.Quo(a1, t4)
    s := new(big.Float).Add(t1, t2)
    f64, _ := s.Add(s.Add(s, t3), t4).Float64()
    return f64
}
```

{{out}}

```txt

  n  sequence value
  3 18.5000000000000000
  4  9.3783783783783790
  5  7.8011527377521617
  6  7.1544144809752490
  7  6.8067847369236327
  8  6.5926327687044388
 20  6.0435521101892693
 30  6.0067860930312058
 50  6.0001758466271875
100  6.0000000193194776
Bank balance after 25 years: $0.04
Rump f(77617, 33096): -0.8273960599468214

```


=={{header|Icon}} and {{header|Unicon}}==
Icon and Unicon support large integers.  Used for scaling the intermediates.
Task 1 includes an extra step, 200 iterations, to demonstrate a closer convergence.


```unicon
#
# Pathological floating point problems
#
procedure main()
    sequence()
    chaotic()
end

#
# First task, sequence convergence
#
link printf
procedure sequence()
     local l := [2, -4]
     local iters := [3, 4, 5, 6, 7, 8, 20, 30, 50, 100, 200]
     local i, j, k  
     local n := 1

     write("Sequence convergence")
     # Demonstrate the convergence problem with various precision values
     every k := (100 | 300) do {
         n := 10^k
         write("\n", k, " digits of intermediate precision")

         # numbers are scaled up using large integer powers of 10
         every i := !iters do {
             l := [2 * n, -4 * n]
             printf("i: %3d", i)

             every j := 3 to i do {
                 # build out a list of intermediate passes
                 # order of scaling operations matters
                 put(l, 111 * n - (1130 * n * n / l[j - 1]) +
                        (3000 * n * n * n / (l[j - 1] * l[j - 2])))
             }
             # down scale the result to a real
             # some precision may be lost in the final display
             printf(" %20.16r\n", l[i] * 1.0 / n)
         }
     }
end

#
# Task 2, chaotic bank of Euler
#
procedure chaotic()
    local euler, e, scale, show, y, d

    write("\nChaotic Banking Society of Euler")
    # format the number for listing, string form, way overboard on digits
    euler :=
"2718281828459045235360287471352662497757247093699959574966967627724076630353_
  547594571382178525166427427466391932003059921817413596629043572900334295260_
  595630738132328627943490763233829880753195251019011573834187930702154089149_
  934884167509244761460668082264800168477411853742345442437107539077744992069_
  551702761838606261331384583000752044933826560297606737113200709328709127443_
  747047230696977209310141692836819025515108657463772111252389784425056953696_
  770785449969967946864454905987931636889230098793127736178215424999229576351_
  482208269895193668033182528869398496465105820939239829488793320362509443117_
  301238197068416140397019837679320683282376464804295311802328782509819455815_
  301756717361332069811250996181881593041690351598888519345807273866738589422_
  879228499892086805825749279610484198444363463244968487560233624827041978623_
  209002160990235304369941849146314093431738143640546253152096183690888707016_
  768396424378140592714563549061303107208510383750510115747704171898610687396_
  9655212671546889570350354"

    # precise math with long integers, string form just for pretty listing
    e := integer(euler)

    # 1000 digits after the decimal for scaling intermediates and service fee
    scale := 10^1000

    # initial deposit - $1
    d := e - scale

    # show balance with 16 digits
    show := 10^16
    write("Starting balance:       $", d * show / scale * 1.0 / show, "...")

    # wait 25 years, with only a trivial $1 service fee
    every y := 1 to 25 do {
        d := d * y - scale
    }

    # show final balance with 4 digits after the decimal (truncation)
    show := 10^4
    write("Balance after ", y, " years: $", d * show / scale * 1.0 / show)
end
```


{{out}}

```txt
prompt$ time unicon -s patho.icn -x
Sequence convergence

100 digits of intermediate precision
i:   3  18.5000000000000000
i:   4   9.3783783783783790
i:   5   7.8011527377521620
i:   6   7.1544144809752490
i:   7   6.8067847369236330
i:   8   6.5926327687044380
i:  20   6.0435521101892680
i:  30   6.0067860930312060
i:  50   6.0001758466271870
i: 100  99.9999999999998400
i: 200 100.0000000000000000

300 digits of intermediate precision
i:   3  18.5000000000000000
i:   4   9.3783783783783790
i:   5   7.8011527377521610
i:   6   7.1544144809752490
i:   7   6.8067847369236320
i:   8   6.5926327687044380
i:  20   6.0435521101892680
i:  30   6.0067860930312060
i:  50   6.0001758466271870
i: 100   6.0000000193194780
i: 200   6.0000000000000000

Chaotic Banking Society of Euler
Starting balance:       $1.718281828459045...
Balance after 25 years: $0.0399

real    0m0.075s
user    0m0.044s
sys     0m0.020s
```



## J


'''A sequence that seems to converge to a wrong limit.'''

Implementation of <code>vn</code>:


```J
   vn=: 111 +(_1130 % _1&{) + (3000 % _1&{ * _2&{)
```


Example using IEEE-754 floating point:


```J
   3 21j16 ":"1] 3 4 5 6 7 8 20 30 50 100 ([,.{) (,vn)^:100(2 _4)
  3   9.3783783783783861
  4   7.8011527377522611
  5   7.1544144809765555
  6   6.8067847369419638
  7   6.5926327689743687
  8   6.4494659378910058
 20  99.9934721906444960
 30 100.0000000000000000
 50 100.0000000000000000
100 100.0000000000000000
```


Example using exact arithmetic:


```J
   3 21j16 ":"1] 3 4 5 6 7 8 20 30 50 100 ([,.{) (,vn)^:100(2 _4x)
  3   9.3783783783783784
  4   7.8011527377521614
  5   7.1544144809752494
  6   6.8067847369236330
  7   6.5926327687044384
  8   6.4494659337902880
 20   6.0360318810818568
 30   6.0056486887714203
 50   6.0001465345613879
100   6.0000000160995649
```


'''The Chaotic Bank Society'''

Let's start this example by using exact arithmetic, to make sure we have the right algorithm. We'll go a bit overboard, in representing ''e'', so we don't have to worry too much about that.


```J
   e=: +/%1,*/\1+i.100x
   81j76":e
   2.7182818284590452353602874713526624977572470936999595749669676277240766303535
   21j16":+`*/,_1,.(1+i.-25),e
   0.0399387296732302
```


(Aside: here, we are used the same mechanism for adding -1 to ''e'' that we are using to add -1 to the product of the year number and the running balance.)

Next, we will use <math>6157974361033 \div 2265392166685</math> for ''e'', to represent the limit of what can be expressed using 64 bit IEEE 754 floating point.


```J
   31j16":+`*/,_1,.(1+i.-25),6157974361033%2265392166685x
_2053975868590.1852178761057505
```


That's clearly way too low, so let's try instead using <math>(1+6157974361033) \div 2265392166685</math> for ''e''

```J
   31j16":+`*/,_1,.(1+i.-25),6157974361034%2265392166685x
 4793054977300.3491517765983265
```


So, our problem seems to be that there's no way we can express enough bits of ''e'', using 64 bit IEEE-754 floating point arithmetic. Just to confirm:


```J
   1x1
2.71828
   +`*/,_1,.(1+i.-25),1x1
_2.24237e9
```


Now let's take a closer look using our rational approximation for ''e'':


```J
   21j16":+`*/,_1,.(1+i.-25),+/%1,*/\1+i.40x
   0.0399387296732302
   21j16":+`*/,_1,.(1+i.-25),+/%1,*/\1+i.30x
   0.0399387277260840
   21j16":+`*/,_1,.(1+i.-25),+/%1,*/\1+i.26x
   0.0384615384615385
   21j16":+`*/,_1,.(1+i.-25),+/%1,*/\1+i.25x
   0.0000000000000000
   21j16":+`*/,_1,.(1+i.-25),+/%1,*/\1+i.24x
  _1.0000000000000000
```


Things go haywire when our approximation for ''e'' uses the same number of terms as our bank's term. So, what does that look like, in terms of precision?


```J
   41j36":+/%1,*/\1+i.26x
   2.718281828459045235360287471257428715
   41j36":+/%1,*/\1+i.25x
   2.718281828459045235360287468777832452
   41j36":+/%1,*/\1+i.24x
   2.718281828459045235360287404308329608
```


In other words, we go astray when our approximation for ''e'' is inaccurate in the 26th position after the decimal point. But IEEE-754 floating point arithmetic can only represent approximately 16 decimal digits of precision.

'''Siegfried Rump's example.'''

Again, we use exact arithmetic to see if we have the algorithm right. That said, we'll also do this in small steps, to make sure we're being exact every step of the way, and to keep from building overly long lines:


```J
rump=:4 :0
  NB. enforce exact arithmetic
  add=. +&x:
  sub=. -&x:
  mul=. *&x:
  div=. %&x:

  a=. x
  a2=. a mul a

  b=. y
  b2=. b mul b
  b4=. b2 mul b2
  b6=. b2 mul b4
  b8=. b4 mul b4

  c333_75=. 1335 div 4 NB. 333.75
  term1=. c333_75 mul b6

  t11a2b2=. 11 mul a2 mul b2
  tnb6=. 0 sub b6
  tn121b4=. 0 sub 121 mul b4
  term2=. a2*(t11a2b2 + tnb6 + tn121b4 sub 2)

  c5_5=. 11 div 2 NB. 5.5
  term3=. c5_5 mul b8

  term4=. a div 2 mul b

  term1 add term2 add term3 add term4
)
```


Example use:


```J
   21j16": 77617 rump 33096
  _0.8273960599468214
```


Note that replacing the definitions of <code>add</code>, <code>sub</code>, <code>div</code>, <code>mul</code> with implementations which promote to floating point gives a very different result:


```J
   77617 rump 33096
_1.39061e21
```


But given that b8 is


```J
   33096^8
1.43947e36
```


we're exceeding the limits of our representation here, if we're using 64 bit IEEE-754 floating point arithmetic.


## Java

Uses BigRational class: [[Arithmetic/Rational/Java]]. For comparison purposes, each task is also implemented with standard 64-bit floating point numbers.


```java
import java.math.BigDecimal;
import java.math.RoundingMode;

public class FPProblems {
    public static void wrongConvergence() {
        int[] INDEXES = new int[] { 3, 4, 5, 6, 7, 8, 20, 30, 50, 100 };
        
        // Standard 64-bit floating point
        double[] fpValues = new double[100];
        fpValues[0] = 2.0;
        fpValues[1] = -4.0;
        for (int i = 2; i < fpValues.length; i++) {
            fpValues[i] = 111.0 - 1130.0 / fpValues[i - 1] + 3000.0 / (fpValues[i - 1] * fpValues[i - 2]);
        }
        
        // Using rational representation
        BigRational[] brValues = new BigRational[100];
        brValues[0] = BigRational.valueOf(2);
        brValues[1] = BigRational.valueOf(-4);
        for (int i = 2; i < brValues.length; i++) {
            // Using intermediate values for better readability
            BigRational clause2 = BigRational.valueOf(1130).divide(brValues[i - 1]);
            BigRational clause3 = BigRational.valueOf(3000).divide(brValues[i - 1].multiply(brValues[i - 2]));
            brValues[i] = BigRational.valueOf(111).subtract(clause2).add(clause3);
        }
        
        System.out.println("Wrong Convergence Sequence");
        for (int n : INDEXES) {
            BigDecimal value = brValues[n - 1].toBigDecimal(16, RoundingMode.HALF_UP);
            System.out.println("  For index " + n + ", FP value is " + fpValues[n - 1] + ", and rounded BigRational value is " + value.toPlainString());
        }
        
        return;
    }
    
    public static void chaoticBankSociety() {
        System.out.println("Chaotic Bank Society");
        double balance = Math.E - 1.0;
        
        // Calculate e using first 1000 terms of the reciprocal of factorials formula
        BigRational e = BigRational.ONE;
        BigRational d = BigRational.ONE;
        for (int i = 1; i < 1000; i++) {
            d = d.multiply(BigRational.valueOf(i));
            e = e.add(d.reciprocal());
        }
        System.out.println("DEBUG: e=" + e.toBigDecimal(100, RoundingMode.HALF_UP).toPlainString());
        
        // Alternatively,
        // BigRational e = BigRational.valueOf(Math.E);
        
        BigRational brBalance = e.subtract(BigRational.ONE);
        for (int year = 1; year <= 25; year++) {
            balance = (balance * year) - 1.0;
            brBalance = brBalance.multiply(BigRational.valueOf(year)).subtract(BigRational.ONE);
            BigDecimal bdValue = brBalance.toBigDecimal(16, RoundingMode.HALF_UP);
            System.out.println("  Year=" + year + ", FP balance=" + balance + ", BigRational balance=" + bdValue.toPlainString());
        }
    }
    
    public static void siegfriedRump() {
        System.out.println("Siegfried Rump formula");
        double fpValue;
        {
            double a = 77617.0;
            double b = 33096.0;
            fpValue = 333.75 * Math.pow(b, 6) + a * a * (11.0 * a * a * b * b - Math.pow(b, 6) - 121.0 * Math.pow(b, 4) - 2.0) + 5.5 * Math.pow(b, 8) + a / (2.0 * b);
        }
        
        BigRational brValue;
        {
            BigRational a = BigRational.valueOf(77617);
            BigRational b = BigRational.valueOf(33096);
            BigRational clause1 = BigRational.valueOf(333.75).multiply(b.pow(6));
            BigRational clause2a = BigRational.valueOf(11).multiply(a).multiply(a).multiply(b).multiply(b);
            BigRational clause2b = b.pow(6).add(BigRational.valueOf(121).multiply(b.pow(4))).add(BigRational.valueOf(2));
            BigRational clause2 = a.multiply(a).multiply(clause2a.subtract(clause2b));
            BigRational clause3 = BigRational.valueOf(5.5).multiply(b.pow(8));
            BigRational clause4 = a.divide(b.multiply(BigRational.valueOf(2)));
            brValue = clause1.add(clause2).add(clause3).add(clause4);
        }
        
        System.out.println("  FP value is " + fpValue);
        System.out.println("  BigRational rounded value is " + brValue.toBigDecimal(64, RoundingMode.HALF_UP).toPlainString());
        System.out.println("  BigRational full value is " + brValue.toString());
    }
    
    public static void main(String... args) {
        wrongConvergence();
        
        System.out.println();
        chaoticBankSociety();

        System.out.println();
        siegfriedRump();
    }
}
```

{{out}}

```txt
Wrong Convergence Sequence
  For index 3, FP value is 18.5, and rounded BigRational value is 18.50000000000000
  For index 4, FP value is 9.378378378378379, and rounded BigRational value is 9.378378378378378
  For index 5, FP value is 7.801152737752169, and rounded BigRational value is 7.801152737752161
  For index 6, FP value is 7.154414480975333, and rounded BigRational value is 7.154414480975249
  For index 7, FP value is 6.806784736924811, and rounded BigRational value is 6.806784736923633
  For index 8, FP value is 6.592632768721792, and rounded BigRational value is 6.592632768704438
  For index 20, FP value is 98.34950312216536, and rounded BigRational value is 6.043552110189269
  For index 30, FP value is 99.99999999999893, and rounded BigRational value is 6.006786093031206
  For index 50, FP value is 100.0, and rounded BigRational value is 6.000175846627187
  For index 100, FP value is 100.0, and rounded BigRational value is 6.000000019319478

Chaotic Bank Society
DEBUG: e=2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427
  Year=1, FP balance=0.7182818284590451, BigRational balance=0.7182818284590452
  Year=2, FP balance=0.4365636569180902, BigRational balance=0.4365636569180905
  Year=3, FP balance=0.30969097075427054, BigRational balance=0.3096909707542714
  Year=4, FP balance=0.23876388301708218, BigRational balance=0.2387638830170857
  Year=5, FP balance=0.1938194150854109, BigRational balance=0.1938194150854282
  Year=6, FP balance=0.16291649051246537, BigRational balance=0.1629164905125695
  Year=7, FP balance=0.1404154335872576, BigRational balance=0.1404154335879862
  Year=8, FP balance=0.12332346869806088, BigRational balance=0.1233234687038897
  Year=9, FP balance=0.1099112182825479, BigRational balance=0.1099112183350075
  Year=10, FP balance=0.09911218282547907, BigRational balance=0.09911218335007541
  Year=11, FP balance=0.09023401108026974, BigRational balance=0.09023401685082952
  Year=12, FP balance=0.08280813296323686, BigRational balance=0.08280820220995428
  Year=13, FP balance=0.07650572852207915, BigRational balance=0.07650662872940558
  Year=14, FP balance=0.07108019930910814, BigRational balance=0.07109280221167809
  Year=15, FP balance=0.06620298963662208, BigRational balance=0.06639203317517140
  Year=16, FP balance=0.05924783418595325, BigRational balance=0.06227253080274239
  Year=17, FP balance=0.007213181161205284, BigRational balance=0.05863302364662064
  Year=18, FP balance=-0.8701627390983049, BigRational balance=0.05539442563917152
  Year=19, FP balance=-17.533092042867793, BigRational balance=0.05249408714425881
  Year=20, FP balance=-351.66184085735586, BigRational balance=0.04988174288517625
  Year=21, FP balance=-7385.898658004473, BigRational balance=0.04751660058870116
  Year=22, FP balance=-162490.7704760984, BigRational balance=0.04536521295142560
  Year=23, FP balance=-3737288.7209502636, BigRational balance=0.04339989788278872
  Year=24, FP balance=-8.969493030280632E7, BigRational balance=0.04159754918692921
  Year=25, FP balance=-2.242373258570158E9, BigRational balance=0.03993872967323021

Siegfried Rump formula
  FP value is -1.1805916207174113E21
  BigRational rounded value is -0.8273960599468213681411650954798162919990331157843848199178148417
  BigRational full value is -54767/66192
```



## jq


jq uses IEEE 754 64-bit numbers so it is easy to illustrate the pathological nature of the series.  This is shown in the following implementation of the first problem.  The second problem is solved using symbolic arithmetic.
 

### v series

The following implementation illustrates how a cache can be used in jq to avoid redundant computations. A JSON object is used as the cache.

```jq
# Input: the value at which to compute v
def v:
  # Input: cache
  # Output: updated cache
  def v_(n):
    (n|tostring) as $s
    | . as $cache
    | if ($cache | has($s)) then .
      else if n == 1 then $cache["1"] = 2
           elif n == 2 then $cache["2"] = -4
           else ($cache | v_(n-1) | v_(n - 2)) as $new
           | $new[(n-1)|tostring] as $x
 	   | $new[(n-2)|tostring] as $y
           | $new + {($s):  ((111 - (1130 / $x) + (3000 / ($x * $y)))) }
	   end
       end;
    . as $m | {} | v_($m) | .[($m|tostring)] ; 
```


Example:
```jq
(3,4,5,6,7,8,20,30,50,100) | v
```

{{out}}

```txt
18.5
9.378378378378379
7.801152737752169
7.154414480975333
6.806784736924811
6.592632768721792
98.34950312216536
99.99999999999893
100
100
```



### Chaotic Bank Society


To avoid the pathological issues, the following uses symbolic arithmetic, with {"e": m, "c": n} representing (e*m + n).


```jq
# Given the balance in the prior year, compute the new balance in year n.
# Input: { e: m, c: n } representing m*e + n
def new_balance(n):
  if n == 0 then {e: 1, c: -1}
  else {e: (.e * n), c: (.c * n - 1) }
  end;

def balance(n):
  def e: 1|exp;
  reduce range(0;n) as $i ({}; new_balance($i) )
  | (.e * e) + .c;
```


Example:
```jq
balance(25)
```

{{out}}
    0

Well, at least that's a reasonable approximation to the value represented by {"e":620448401733239400000000,"c":-1686553615927922300000000}


## Julia

{{works with|Julia|0.6}}

The task could be completed even using ```Rational{BigFloat}``` type.


```julia
# arbitrary precision
setprecision(2000)

# Task 1
function seq(n)
    len = maximum(n)
    r = Vector{BigFloat}(len)
    r[1] = 2
    if len > 1 r[2] = -4 end

    for i in 3:len
        r[i] = 111 - 1130 / r[i-1] + 3000 / (r[i-1] * r[i-2])
    end

    return r[n]
end

n = [1, 2, 3, 5, 10, 100]
v = seq(n)
println("Task 1 - Sequence convergence:\n", join((@sprintf("v%-3i = %23.20f", i, s) for (i, s) in zip(n, v)), '\n'))

# Task 2: solution with big float (precision can be set with setprecision function)
function chaoticbankfund(years::Integer)
    balance = big(e) - 1
    for y in 1:years
        balance = (balance * y) - 1
    end

    return balance
end

println("\nTask 2 - Chaotic Bank fund after 25 years:\n", @sprintf "%.20f" chaoticbankfund(25))

# Task 3: solution with big float
f(a::Union{BigInt,BigFloat}, b::Union{BigInt,BigFloat}) =
    333.75b ^ 6 + a ^ 2 * ( 11a ^ 2 * b ^ 2 - b ^ 6 - 121b ^ 4 - 2 ) + 5.5b ^ 8 + a / 2b

println("\nTask 3 - Siegfried Rump's example:\nf(77617.0, 33096.0) = ", @sprintf "%.20f" f(big(77617.0), big(33096.0)))
```


{{out}}

```txt
Task 1 - Sequence convergence:
v1   =  2.00000000000000000000
v2   = -4.00000000000000000000
v3   = 18.50000000000000000000
v5   =  7.80115273775216138329
v10  =  6.34845205665435714710
v100 =  6.00000001931947792910

Task 2 - Chaotic Bank fund after 25 years:
0.03993872967323020890

Task 3 - Siegfried Rump's example:
f(77617.0, 33096.0) = -0.82739605994682136814
```



## Kotlin


```scala
// version 1.0.6

import java.math.*

const val LIMIT = 100

val con480  = MathContext(480)
val bigTwo =  BigDecimal(2)
val bigE    = BigDecimal("2.71828182845904523536028747135266249775724709369995") // precise enough!

fun main(args: Array<String>) {
    // v(n) sequence task
    val c1 = BigDecimal(111)
    val c2 = BigDecimal(1130)
    val c3 = BigDecimal(3000)
    var v1 = bigTwo
    var v2 = BigDecimal(-4)
    var v3:  BigDecimal
    for (i in 3 .. LIMIT) {
        v3 = c1 - c2.divide(v2, con480) + c3.divide(v2 * v1, con480)
        println("${"%3d".format(i)} : ${"%19.16f".format(v3)}")
        v1 = v2
        v2 = v3
    }

    // Chaotic Building Society task
    var balance = bigE - BigDecimal.ONE
    for (year in 1..25) balance = balance.multiply(BigDecimal(year), con480) - BigDecimal.ONE
    println("\nBalance after 25 years is ${"%18.16f".format(balance)}")

    // Siegfried Rump task
    val a  = BigDecimal(77617)
    val b  = BigDecimal(33096)
    val c4 = BigDecimal("333.75")
    val c5 = BigDecimal(11)
    val c6 = BigDecimal(121)
    val c7 = BigDecimal("5.5")
    var f  = c4 * b.pow(6, con480) + c7 * b.pow(8, con480) + a.divide(bigTwo * b, con480)
    val c8 = c5 * a.pow(2, con480) * b.pow(2, con480) - b.pow(6, con480) - c6 * b.pow(4, con480) - bigTwo
    f += c8 * a.pow(2, con480)
    println("\nf(77617.0, 33096.0) is ${"%18.16f".format(f)}") 
}
```


{{out}}

```txt

  3 : 18.5000000000000000
  4 :  9.3783783783783784
  5 :  7.8011527377521614
  6 :  7.1544144809752494
  7 :  6.8067847369236330
  8 :  6.5926327687044384
  9 :  6.4494659337902880
 10 :  6.3484520566543571
 11 :  6.2744385982163279
 12 :  6.2186957398023978
 13 :  6.1758373049212301
 14 :  6.1423590812383559
 15 :  6.1158830665510808
 16 :  6.0947394393336811
 17 :  6.0777223048472427
 18 :  6.0639403224998088
 19 :  6.0527217610161522
 20 :  6.0435521101892689
 21 :  6.0360318810818568
 22 :  6.0298473250239019
 23 :  6.0247496523668479
 24 :  6.0205399840615161
 25 :  6.0170582573289876
 26 :  6.0141749145508190
 27 :  6.0117845878713337
 28 :  6.0098012392984846
 29 :  6.0081543789122289
 30 :  6.0067860930312058
 31 :  6.0056486887714203
 32 :  6.0047028131881752
 33 :  6.0039159416664605
 34 :  6.0032611563057406
 35 :  6.0027161539543513
 36 :  6.0022624374405593
 37 :  6.0018846538818819
 38 :  6.0015700517342190
 39 :  6.0013080341649643
 40 :  6.0010897908901841
 41 :  6.0009079941545271
 42 :  6.0007565473053508
 43 :  6.0006303766028389
 44 :  6.0005252586505718
 45 :  6.0004376772265183
 46 :  6.0003647044182955
 47 :  6.0003039018761868
 48 :  6.0002532387368678
 49 :  6.0002110233741743
 50 :  6.0001758466271872
 51 :  6.0001465345613879
 52 :  6.0001221091522881
 53 :  6.0001017555560260
 54 :  6.0000847948586303
 55 :  6.0000706613835716
 56 :  6.0000588837928413
 57 :  6.0000490693458029
 58 :  6.0000408907870884
 59 :  6.0000340754236785
 60 :  6.0000283960251310
 61 :  6.0000236632422855
 62 :  6.0000197192908008
 63 :  6.0000164326883272
 64 :  6.0000136938694348
 65 :  6.0000114115318177
 66 :  6.0000095095917616
 67 :  6.0000079246472413
 68 :  6.0000066038639788
 69 :  6.0000055032139253
 70 :  6.0000045860073981
 71 :  6.0000038216699107
 72 :  6.0000031847228971
 73 :  6.0000026539343389
 74 :  6.0000022116109709
 75 :  6.0000018430084630
 76 :  6.0000015358399141
 77 :  6.0000012798662675
 78 :  6.0000010665549954
 79 :  6.0000008887956715
 80 :  6.0000007406629499
 81 :  6.0000006172190487
 82 :  6.0000005143491543
 83 :  6.0000004286242585
 84 :  6.0000003571868566
 85 :  6.0000002976556961
 86 :  6.0000002480464011
 87 :  6.0000002067053257
 88 :  6.0000001722544322
 89 :  6.0000001435453560
 90 :  6.0000001196211272
 91 :  6.0000000996842706
 92 :  6.0000000830702242
 93 :  6.0000000692251858
 94 :  6.0000000576876542
 95 :  6.0000000480730447
 96 :  6.0000000400608703
 97 :  6.0000000333840583
 98 :  6.0000000278200485
 99 :  6.0000000231833736
100 :  6.0000000193194779

Balance after 25 years is 0.0399387296732302

f(77617.0, 33096.0) is -0.8273960599468214

```



## Mathematica


Task 1:

```Mathematica
v[1] = 2;
v[2] = -4;
v[n_] := Once[111 - 1130/v[n - 1] + 3000/(v[n - 1]*v[n - 2])]
N[Map[v, {3, 4, 5, 6, 7, 8, 20, 30, 50, 100}], 80]
```

{{out}}

```txt
{18.500000000000000000000000000000000000000000000000000000000000000000000000000000, 
9.3783783783783783783783783783783783783783783783783783783783783783783783783783784, 
7.8011527377521613832853025936599423631123919308357348703170028818443804034582133, 
7.1544144809752493535278906538603620243812338381972663465090506095308459549316587, 
6.8067847369236329839417565962720090876232767078019311199463004079103629885888367, 
6.5926327687044383927420027763659948265529823177346067194125634354115621230855591,
6.0435521101892688677774773640975401331877150000061201379728002521382151385271029,
6.0067860930312057585305540479532397058330723144383667645482877308904928243847153,
6.0001758466271871889456140207471954695237351770993318409845704023663691405177107,
6.0000000193194779291040868034035857150243506754369524580725927508565217672302663}
```


Task 2:

```Mathematica
year = 1; N[Nest[# year++ - 1 &, E - 1, 25], 30]
```

{{out}}
```txt
0.0399387296732302089036714552104
```


Task 3:

```Mathematica
f[a_, b_] := 333.75`100 b^6 + a^2 (11 a^2 b^2 - b^6 - 121 b^4 - 2) + 5.5`100 b^8 + a/(2 b)
f[77617, 33096]
```

{{out}}
```txt
-0.827396059946821368141165095479816291999033115784384819917814842
```



## PARI/GP


Task 1: Define recursive function V(n):

```parigp
V(n,a=2,v=-4.)=if(n < 3,return(v));V(n--,v,111-1130/v+3000/(v*a))
```

In order to work set precision to at least 200 digits:

```txt
\p 200: realprecision = 211 significant digits (200 digits displayed)

V(50):  6.000175846627187188945614020747195469523735177...
V(100): 6.0000000193194779291040868034035857150243506754369524580725927508565217672302663412282...

```

----
Task 2: Define function balance(deposit,years):

```parigp
balance(d,y)=d--;for(n=1,y,d=d*n-1);d
```


Output ''balance(exp(1), 25)'':

```txt
0.039938729673230208903...
```

----
Task 3: Define function f(a,b):

```parigp
f(a,b)=333.75*b^6+a*a*(11*a*a*b*b-b^6-121*b^4-2)+5.5*b^8+a/(2*b)
```


Output:
```txt
f(77617.0,33096.0): -0.827396059946821368141165...
```



## Perl

All three tasks can be solved by using either the <code>bigrat</code> or <code>bignum</code> core modules.
Both approaches are used, as demonstration.

### = Convergent series =

The constants in the equation must be represented with a decimal point (even just <code>111.</code>) so that
they are treated as rationals, not integers.

```perl
use bigrat;

@s = qw(2, -4);
for my $n (2..99) {
    $s[$n]= 111.0 - 1130.0/$s[-1] + 3000.0/($s[-1]*$s[-2]);
}

for $n (3..8, 20, 30, 35, 50, 100) {
    ($nu,$de) = $s[$n-1] =~ m#^(\d+)/(\d+)#;;
    printf "n = %3d %18.15f\n", $n, $nu/$de;
}
```

{{out}}

```txt
n =   3 18.500000000000000
n =   4  9.378378378378379
n =   5  7.801152737752162
n =   6  7.154414480975249
n =   7  6.806784736923633
n =   8  6.592632768704439
n =  20  6.043552110189269
n =  30  6.006786093031206
n =  35  6.002716153954351
n =  50  6.000175846627188
n = 100  6.000000019319478
```



### = Chaotic bank society =

The value of 𝑒 is imported from a module, but could also be calculated, as is done in
[[Calculating_the_value_of_e#Perl|Calculating the value of e]]

```perl
use bignum qw(bexp);
$e       = bexp(1,43);
$years   = 25;
$balance = $e - 1;

print "Starting balance, \$(e-1): \$$balance\n";
for $i (1..$years) { $balance = $i * $balance - 1 }
printf "After year %d, you will have \$%1.15g in your account.\n", $years, $balance;
```

{{out}}

```txt
Starting balance, $(e-1): $1.718281828459045235360287471352662497757247
After year 25, you will have $0.0399387296732302 in your account.
```


==== Rump's example ====

```perl
use bignum;

$a = 77617;
$b = 33096;
printf "%0.16f\n", 333.75*$b**6 + $a**2 * ( 11*$a**2 * $b**2 - $b**6 - 121*$b**4 - 2) + 5.5*$b**8 + $a/(2*$b);
```

{{out}}

```txt
-0.8273960599468214
```



## Perl 6

{{works with|Rakudo|2016-01}}
The simple solution to doing calculations where floating point numbers exhibit pathological behavior is: don't do floating point calculations. :-) Perl 6 is just as susceptible to floating point error as any other C based language, however, it offers built-in rational Types; where numbers are represented as a ratio of two integers. For normal precision it uses Rats - accurate to 1/2^64, and for arbitrary precision, FatRats, whose denominators can grow as large as available memory. Rats don't require any special setup to use. Any decimal number within its limits of precision is automatically stored as a Rat. FatRats require explicit coercion and are "sticky". Any FatRat operand in a calculation will cause all further results to be stored as FatRats.

```perl6
say '1st: Convergent series';
my @series = 2.FatRat, -4, { 111 - 1130 / $^v + 3000 / ( $^v * $^u ) } ... *;
for flat 3..8, 20, 30, 50, 100 -> $n {say "n = {$n.fmt("%3d")} @series[$n-1]"};

say "\n2nd: Chaotic bank society";
sub postfix:<!> (Int $n) { [*] 2..$n } # factorial operator
my $years = 25;
my $balance = sum map { 1 / FatRat.new($_!) }, 1 .. $years + 15; # Generate e-1  to sufficient precision with a Taylor series
put "Starting balance, \$(e-1): \$$balance";
for 1..$years -> $i { $balance = $i * $balance - 1 }
printf("After year %d, you will have \$%1.16g in your account.\n", $years, $balance);

print "\n3rd: Rump's example: f(77617.0, 33096.0) = ";
sub f (\a, \b) { 333.75*b⁶ + a²*( 11*a²*b² - b⁶ - 121*b⁴ - 2 ) + 5.5*b⁸ + a/(2*b) }
say f(77617.0, 33096.0).fmt("%0.16g");
```

{{Out}}

```txt
1st: Convergent series
n =   3 18.5
n =   4 9.378378
n =   5 7.801153
n =   6 7.154414
n =   7 6.806785
n =   8 6.5926328
n =  20 6.0435521101892689
n =  30 6.006786093031205758530554
n =  50 6.0001758466271871889456140207471954695237
n = 100 6.000000019319477929104086803403585715024350675436952458072592750856521767230266

2nd: Chaotic bank society
Starting balance, $(e-1): $1.7182818284590452353602874713526624977572470936999
After year 25, you will have $0.0399387296732302 in your account.

3rd: Rump's example: f(77617.0, 33096.0) = -0.827396059946821

```



## Phix

Standard maths with the IEEE-754 hardware floats fails predictably (that code left in as comments), so roll out the bigatoms.

Task1: needs at least 120 decimal places to avoid serious divergance, and 196 to be accurate to 78 digits

Task2: needs at least 41 decimal places (and 42 passed as the first argument to ba_euler)

Task3: apparently only needs just 15 decimal places, then again ba_scale() defines the minimum, and it may (as in is permitted to) use more.

```Phix
include builtins\bigatom.e
puts(1,"Task 1\n")
constant {fns,fmts} = columnize({{3,1},{4,6},{5,6},{6,6},{7,6},{8,7},{20,16},{30,24},{50,40},{100,78}})
{} = ba_scale(196)
sequence v = {2,-4}
for n=3 to 100 do
--  v = append(v,111 - 1130/v[n-1] + 3000/(v[n-1]*v[n-2]))
    v = append(v,ba_add(ba_sub(111,ba_divide(1130,v[n-1])),ba_divide(3000,ba_multiply(v[n-1],v[n-2]))))
    if n<9 or find(n,{20,30,50,100}) then
--      printf(1,"n = %-3d %20.16f\n", {n, v[n]})
        string fmt = sprintf("%%.%dB",fmts[find(n,fns)])
        printf(1,"n = %-3d %s\n", {n, ba_sprintf(fmt,v[n])})
    end if
end for

puts(1,"\nTask 2\n")
--atom balance = exp(1)-1
--for i=1 to 25 do balance = balance*i-1 end for
--printf(1,"\nTask 2\nBalance after 25 years: $%12.10f", balance)
{} = ba_scale(41)
bigatom balance = ba_sub(ba_euler(42,true),1)
for i=1 to 25 do balance = ba_sub(ba_multiply(balance,i),1) end for
ba_printf(1,"Balance after 25 years: $%.16B\n\n", balance)

puts(1,"Task 3\n")
{} = ba_scale(15)   -- fine!
integer a = 77617,
        b = 33096
--atom pa2 = power(a,2),
--   pb2a211 = 11*pa2*power(b,2),
--   pb4121 = 121*power(b,4),
--   pb6 = power(b,6),
--   pb855 = 5.5*power(b,8),
--   f_ab = 333.75 * pb6 + pa2 * (pb2a211 - pb6 - pb4121 - 2) + pb855 + a/(2*b) 
--printf(1,"f(%d, %d) = %.15f\n\n", {a, b, f_ab})
bigatom pa2 = ba_power(a,2),
        pb2a211 = ba_multiply(11,ba_multiply(pa2,ba_power(b,2))),
        pb4121 = ba_multiply(121,ba_power(b,4)),
        pb6 = ba_power(b,6),
        pa2mid = ba_multiply(pa2,ba_sub(ba_sub(ba_sub(pb2a211,pb6),pb4121),2)),
        pb633375 = ba_multiply(333.75,pb6),
        pb855 = ba_multiply(5.5,ba_power(b,8)),
        f_ab = ba_add(ba_add(ba_add(pb633375,pa2mid),pb855),ba_divide(a,ba_multiply(2,b)))
printf(1,"f(%d, %d) = %s\n", {a, b, ba_sprintf("%.15B",f_ab)})
```

Aside: obviously you don't ''have'' to use lots of temps like that, but I find that style often makes things much easier to debug.
{{out}}

```txt

Task 1
n = 3   18.5
n = 4   9.378378
n = 5   7.801153
n = 6   7.154414
n = 7   6.806785
n = 8   6.5926328
n = 20  6.0435521101892689
n = 30  6.006786093031205758530554
n = 50  6.0001758466271871889456140207471954695237
n = 100 6.000000019319477929104086803403585715024350675436952458072592750856521767230266

Task 2
Balance after 25 years: $0.0399387296732302

Task 3
f(77617, 33096) = -0.827396059946821

```



###  gmp version 

{{libheader|mpfr}}
Note that bigatom is formally deprecated in favour of gmp. However there are significant benefits to the
former on this task, being easier to read and debug.

Again, I have left native/hopelessly incorrect versions in as comments for comparison.

Task1: needs at least 120 decimal places to avoid serious divergance, and 196 to be accurate to 78 digits.

Task2: needs at least 41 decimal places (and e-1 accurately specified to at least 42 decimal places).

Task3: needs at least 36 decimal places (my suspicions above re bigatom in 15 now seem correct).

```Phix
include builtins\mpfr.e
puts(1,"Task 1\n")
constant {fns,fmts} = columnize({{3,1},{4,6},{5,6},{6,6},{7,6},{8,7},{20,16},{30,24},{50,40},{100,78}})
mpfr_set_default_prec(-196) 
sequence v = {mpfr_init(2),mpfr_init(-4)}
mpfr t1 = mpfr_init()
for n=3 to 100 do
--  v = append(v,111 - 1130/v[n-1] + 3000/(v[n-1]*v[n-2]))
    mpfr_set_si(t1,1130)
    mpfr_div(t1,t1,v[n-1])
    mpfr_si_sub(t1,111,t1)
    mpfr t2 = mpfr_init()
    mpfr_mul(t2,v[n-1],v[n-2])
    mpfr_si_div(t2,3000,t2)
    mpfr_add(t2,t1,t2)
    v = append(v,t2)
    if n<9 or find(n,{20,30,50,100}) then
--      printf(1,"n = %-3d %20.16f\n", {n, v[n]})
        string fmt = sprintf("%%.%dRf",fmts[find(n,fns)])
        printf(1,"n = %-3d %s\n", {n, mpfr_sprintf(fmt,v[n])})
    end if
end for
 

puts(1,"\nTask 2\n")
--atom balance = exp(1)-1
--for i=1 to 25 do balance = balance*i-1 end for
--printf(1,"\nTask 2\nBalance after 25 years: $%12.10f", balance)
mpfr_set_default_prec(-41) 
mpfr balance = mpfr_init("1.71828182845904523536028747135266249775724709369995"&
                           "95749669676277240766303535475945713821785251664274")
for i=1 to 25 do
    mpfr_mul_si(balance,balance,i)
    mpfr_sub_si(balance,balance,1)
end for
mpfr_printf(1,"Balance after 25 years: $%.16Rf\n\n", balance)


puts(1,"Task 3\n")
mpfr_set_default_prec(-36)
integer a = 77617,
        b = 33096
--atom pa2 = power(a,2),
--   pb2a211 = 11*pa2*power(b,2),
--   pb4121 = 121*power(b,4),
--   pb6 = power(b,6),
--   pb855 = 5.5*power(b,8),
--   f_ab = 333.75 * pb6 + pa2 * (pb2a211 - pb6 - pb4121 - 2) + pb855 + a/(2*b) 
--printf(1,"f(%d, %d) = %.15f\n\n", {a, b, f_ab})
-- (translation of FreeBASIC)
mpfr {t2,t3,t4,t5,t6,t7} = mpfr_inits(6)
  mpfr_set_d(t1, a)          -- a
  mpfr_set_d(t2, b)          -- b 
  mpfr_set_d(t3, 333.75)     -- 333.75
  mpfr_pow_si(t4, t2, 6)     -- b ^ 6
  mpfr_mul(t3, t3, t4)       -- 333.75 * b^6
  mpfr_pow_si(t5, t1, 2)     -- a^2
  mpfr_pow_si(t6, t2, 2)     -- b^2
  mpfr_mul_si(t7, t5, 11)    -- 11 * a^2
  mpfr_mul(t7, t7, t6)       -- 11 * a^2 * b^2
  mpfr_sub(t7, t7, t4)       -- 11 * a^2 * b^2 - b^6
  mpfr_pow_si(t4, t2, 4)     -- b^4
  mpfr_mul_si(t4, t4, 121)   -- 121 * b^4
  mpfr_sub(t7, t7, t4)       -- 11 * a^2 * b^2 - b^6 - 121 * b^4
  mpfr_sub_si(t7, t7, 2)     -- 11 * a^2 * b^2 - b^6 - 121 * b^4 - 2
  mpfr_mul(t7, t7, t5)       -- (11 * a^2 * b^2 - b^6 - 121 * b^4 - 2) * a^2
  mpfr_add(t3, t3, t7)       -- 333.75 * b^6 + (11 * a^2 * b^2 - b^6 - 121 * b^4 - 2) * a^2
  mpfr_set_d(t4, 5.5)        -- 5.5
  mpfr_pow_si(t5, t2, 8)     -- b^8  
  mpfr_mul(t4, t4, t5)       -- 5.5 * b^8
  mpfr_add(t3, t3, t4)       -- 333.75 * b^6 + (11 * a^2 * b^2 - b^6 - 121 * b^4 - 2) * a^2 + 5.5 * b^8
  mpfr_mul_si(t4, t2, 2)     -- 2 * b
  mpfr_div(t5, t1, t4)       -- a / (2 * b)
  mpfr_add(t3, t3, t5)       -- 333.75 * b^6 + (11 * a^2 * b^2 - b^6 - 121 * b^4 - 2) * a^2 + 5.5 * b^8 + a / (2 * b)
printf(1,"f(%d, %d) = %s\n", {a, b, mpfr_sprintf("%.15Rf",t3)})
{t1,t2,t3,t4,t5,t6,t7} = mpfr_free({t1,t2,t3,t4,t5,t6,t7})
```

Identical output


## Python


===Task 1: Muller's sequence===

Using rational numbers via standard library <code>fractions</code>


```Python
from fractions import Fraction

def muller_seq(n:int) -> float:
    seq = [Fraction(0), Fraction(2), Fraction(-4)]
    for i in range(3, n+1):
        next_value = (111 - 1130/seq[i-1]
            + 3000/(seq[i-1]*seq[i-2]))
        seq.append(next_value)
    return float(seq[n])

for n in [3, 4, 5, 6, 7, 8, 20, 30, 50, 100]:
    print("{:4d} -> {}".format(n, muller_seq(n)))
```


{{Out}}

```txt
   3 -> 18.5
   4 -> 9.378378378378379
   5 -> 7.801152737752162
   6 -> 7.154414480975249
   7 -> 6.806784736923633
   8 -> 6.592632768704439
  20 -> 6.043552110189269
  30 -> 6.006786093031206
  50 -> 6.0001758466271875
 100 -> 6.000000019319478

```



### Task 2: The Chaotic Bank Society


Using <code>decimal</code> numbers with a high precision


```Python
from decimal import Decimal, getcontext

def bank(years:int) -> float:
    """
    Warning: still will diverge and return incorrect results after 250 years
    the higher the precision, the more years will cover
    """
    getcontext().prec = 500
    # standard math.e has not enough precision
    e = Decimal('2.718281828459045235360287471352662497757247093699959574966967627724076630353547594571382178525166427427466391932003059921817413596629043572900334295260595630738132328627943490763233829880753195251019011573834187930702154089149934884167509244761460668082264800168477411853742345442437107539077744992069551702761838606261331384583000752044933826560297606737113200709328709127443747047230696977209310141692836819025515108657463772111252389784425056953696770785449969967946864454905987931636889230098793127736178215424999229576351')
    decimal_balance = e - 1
    for year in range(1, years+1):
        decimal_balance = decimal_balance * year - 1
    return(float(decimal_balance))

print("Bank balance after 25 years = ", bank(25))
```


{{Out}}

```txt
Bank balance after 25 years =  0.03993872967323021

```


'''but, still incorrectly diverging after some time, aprox. 250 years'''

```Python
for year in range(200, 256, 5):
    print(year, '->', bank(year))

```


{{Out}}

```txt
200 -> 0.004999875631110097
205 -> 0.004877933277184028
210 -> 0.004761797301186607
215 -> 0.0046510626428896236
220 -> 0.004545361061789591
225 -> 0.0044443570465329246
230 -> 0.004347744257820075
235 -> 0.004255242425346535
240 -> 0.004166594632576723
245 -> 0.004081564933953891
250 -> 0.003999846590933889
255 -> -92939.78784907148

```


===Task 3: Siegfried Rump's example===

Using rational numbers via standard library <code>fractions</code>


```Python
from fractions import Fraction

def rump(generic_a, generic_b) -> float:
    a = Fraction('{}'.format(generic_a))
    b = Fraction('{}'.format(generic_b))
    fractional_result = Fraction('333.75') * b**6 \
        + a**2 * ( 11 * a**2 * b**2 - b**6 - 121 * b**4 - 2 ) \
        + Fraction('5.5') * b**8 + a / (2 * b)
    return(float(fractional_result)) 

print("rump(77617, 33096) = ", rump(77617.0, 33096.0))

```


{{Out}}

```txt
rump(77617, 33096) =  -0.8273960599468214

```



## Racket


Racket has the concept of exact (rational) and inexact (floating point) numbers, both real and complex.
See: http://docs.racket-lang.org/guide/numbers.html for more details.

The examples below use real numbers, and the <code>x</code> function is used to transform them to floats, if desired, with the function <code>exact->inexact</code>.


```racket
#lang racket

(define current-do-exact-calculations? (make-parameter exact->inexact))

(define (x n) (if (current-do-exact-calculations?) n (exact->inexact n)))

(define (decimal.18 n)
  (regexp-replace #px"0+$" (real->decimal-string n 18) ""))

(define (task-1 n)
  (let ((c_1 (x 111)) (c_2 (x -1130)) (c_3 (x 3000)))
    (let loop ((v_n-2 (x 2)) (v_n-1 (x -4)) (n (- n 2)))
      (if (= n 0) v_n-1 (loop v_n-1 (+ c_1 (/ c_2 v_n-1) (/ c_3 (* v_n-1 v_n-2))) (- n 1))))))

(define (task-2) ; chaotic bank
   (define e (if (current-do-exact-calculations?)
                 #e2.71828182845904523536028747135266249775724709369995
                 (exp 1)))
  (for/fold ((b (- e 1))) ((y (in-range 1 26))) (- (* b y) 1)))

(define (task-3 a b)
    (+ (* (x #e333.75) (expt b 6))
       (* (expt a 2) (- (* 11 (expt a 2) (expt b 2)) (expt b 6) (* 121 (expt b 4)) 2))
       (* (x #e5.5) (expt b 8))
       (/ a (* b 2))))

(define (all-tests)
  (let ((classic-sum (+ (x #e0.2) (x #e0.1))))
    (printf "Classic example: ~a = ~a~%" classic-sum (decimal.18 classic-sum)))

  (displayln "TASK 1")
  (for ((n (in-list '(3 4 5 6 7 8 20 30 50 100))))
    (printf "n=~a\t~a~%" n (decimal.18 (task-1 n))))
  
  (printf "TASK 2: balance after 25 years = ~a~%" (decimal.18 (task-2)))

  (let ((t3 (task-3 77617 33096)))
    (printf "TASK 3: f(77617, 33096) = ~a = ~a~%" t3 (decimal.18 t3))))

(module+ main
  (displayln "INEXACT (Floating Point) NUMBERS")
  (parameterize ([current-do-exact-calculations? #f])
    (all-tests))
  (newline)

  (displayln "EXACT (Rational) NUMBERS")
  (parameterize ([current-do-exact-calculations? #t])
    (all-tests)))
```


{{out}}


```txt
INEXACT (Floating Point) NUMBERS
Classic example: 0.30000000000000004 = 0.300000000000000044
TASK 1
n=3	18.5
n=4	9.378378378378378954
n=5	7.801152737752168775
n=6	7.15441448097533339
n=7	6.806784736924811341
n=8	6.592632768721792047
n=20	98.349503122165359059
n=30	99.999999999998934186
n=50	100.
n=100	100.
TASK 2: balance after 25 years = -2242373258.570158004760742188
TASK 3: f(77617, 33096) = 1.1805916207174113e+021 = 1180591620717411303424.

EXACT (Rational) NUMBERS
Classic example: 3/10 = 0.3
TASK 1
n=3	18.5
n=4	9.378378378378378378
n=5	7.801152737752161383
n=6	7.154414480975249354
n=7	6.806784736923632984
n=8	6.592632768704438393
n=20	6.043552110189268868
n=30	6.006786093031205759
n=50	6.000175846627187189
n=100	6.000000019319477929
TASK 2: balance after 25 years = 0.039938729673230209
TASK 3: f(77617, 33096) = -54767/66192 = -0.827396059946821368
```



## REXX

The REXX language uses character-based arithmetic.   So effectively, it looks, feels, and tastes like <u>decimal</u> floating point 
(implemented in software).

So, the only (minor) problem is how many decimal digits should be used to solve these pathological floating point problems.

A little extra boilerplate code was added to support the specification of how many decimal digits that should be used for the

calculations,   as well how many decimal digits   (past the decimal point)   should be displayed.

### A sequence that seems to converge to a wrong limit


```rexx
/*REXX pgm (pathological FP problem):  a sequence that might converge to a wrong limit. */
parse arg digs show .                            /*obtain optional arguments from the CL*/
if digs=='' | digs==","  then digs= 150          /*Not specified?  Then use the default.*/
if show=='' | show==","  then show=  20          /* "      "         "   "   "     "    */
numeric digits digs                              /*have REXX use "digs" decimal digits. */
#= 2 4 5 6 7 8 9 20 30 50 100                    /*the indices to display value of  V.n */
fin= word(#, words(#) )                          /*find the last (largest) index number.*/
w= length(fin)                                   /*  "   "  length (in dec digs) of FIN.*/
v.1= 2                                           /*the value of the first   V  element. */
v.2=-4                                           /* "    "    "  "  second  "     "     */
      do n=3  to fin;   nm1= n-1;     nm2= n-2   /*compute some values of the V elements*/
      v.n= 111 - 1130/v.nm1 + 3000/(v.nm1*v.nm2) /*   "      a  value  of  a  " element.*/
                                                 /*display digs past the dec. point───┐ */
      if wordpos(n, #)\==0  then say   'v.'left(n, w)       "="        format(v.n, , show)
      end   /*n*/                                /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default inputs:}}

```txt

v.4   = 9.37837837837837837838
v.5   = 7.80115273775216138329
v.6   = 7.15441448097524935353
v.7   = 6.80678473692363298394
v.8   = 6.59263276870443839274
v.9   = 6.44946593379028796887
v.20  = 6.04355211018926886778
v.30  = 6.00678609303120575853
v.50  = 6.00017584662718718895
v.100 = 6.00000001931947792910

```



### The Chaotic Bank Society

To be truly accurate, the number of decimal digits for   <big> ''' ''e'' ''' </big>   should have as many decimal digits as given on the 

command line (if specified),   but what's currently coded will suffice for the (default) number of years. 

However, it makes a difference computing the balance   (with 341 decimal digits)   after   '''183'''   years   (when at 

that point,   the balance becomes negative and grows increasing negative fast).  

With 150 decimal digits, the balance becomes negative after   '''96'''   years.
 

```rexx
/*REXX pgm (pathological FP problem): the chaotic bank society offering a new investment*/
e=2.7182818284590452353602874713526624977572470936999595749669676277240766303535475945713,
  ||8217852516642742746639193200305992181741359662904357290033429526059563073813232862794,
  ||3490763233829880753195251019011573834187930702154089149934884167509244761460668082264,
  ||8001684774118537423454424371075390777449920695517027618386062613313845830007520449338
d = length(e)  -  1                              /*subtract unity for the decimal point.*/
parse arg digs show y .                          /*obtain optional arguments from the CL*/
if digs==''  |  digs==","  then digs=  d         /*Not specified?  Then use the default.*/
if show==''  |  show==","  then show= 20         /* "      "         "   "   "     "    */
if    y==''  |     y==","  then    y= 25         /* "      "         "   "   "     "    */
numeric digits digs                              /*have REXX use "digs" decimal digits. */
$= e - 1                                         /*subtract $1 from e, that's da deposit*/
                                                 /* [↑]  value of newly opened account. */
                           do n=1  for y         /*compute the value of the account/year*/
                           $= $*n  -  1          /*   "     "    "    "  "  account now.*/
                           end   /*n*/
@@@= 'With '     d      " decimal digits, the balance after "      y      ' years is: '
say @@@    '$'format($, , show) / 1              /*stick a fork in it,  we're all done. */
```

{{out|output|text=  when using the default inputs:}}

```txt

With  341  decimal digits, the balance after  25  years is:  $0.0399387296732302

```


===Siegfried Rump's example===

```rexx
/*REXX pgm (pathological FP problem): the Siegfried Rump's example (problem dated 1988).*/
parse arg digs show .                            /*obtain optional arguments from the CL*/
if digs=='' | digs==","  then digs=150           /*Not specified?  Then use the default.*/
if show=='' | show==","  then show= 20           /* "      "         "   "   "     "    */
numeric digits digs                              /*have REXX use "digs" decimal digits. */
a= 77617.0                                       /*initialize  A  to it's defined value.*/
b= 33096.0                                       /*     "      B   "   "     "      "   */
                                                 /*display SHOW digits past the dec. pt.*/
say 'f(a,b)='    format(   f(a,b), , show)       /*display result from the  F  function.*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
f:  procedure; parse arg a,b;  return  333.75* b**6   +   a**2 * (11* a**2* b**2  -  b**6,
                                     - 121*b**4  - 2)     +     5.5*b**8    +    a / (2*b)
```

{{out|output|text=  when using the default inputs:}}

```txt

f(a,b)= -0.82739605994682136814

```



## Ring


```ring

# Project : Pathological floating point problems

decimals(8)
v = list(100)
v[1] = 2
v[2] = -4
for n = 3 to 100
      v[n] = (111 - 1130 / v[n-1]) + 3000 / (v[n-1] * v[n-2])
      if n < 9 or n = 20 or n = 30 or n = 50 or n = 100
         see "n = " + n + "   " + v[n] + nl
      ok
next

```

Output:

```txt

n = 3   18.50000000
n = 4   9.37837838
n = 5   7.80115274
n = 6   7.15441448
n = 7   6.80678474
n = 8   6.59263277
n = 20  98.34950312
n = 30  100.00000000
n = 50  100
n = 100 100

```



## Ruby

===Task 1: Muller's sequence===
Ruby numbers have a "quo" division method, which returns a rational (a fraction) when possible, avoiding Float inaccuracy.

```Ruby
ar = [0, 2, -4]
100.times{ar << (111 - 1130.quo(ar[-1])+ 3000.quo(ar[-1]*ar[-2])) }
 
[3, 4, 5, 6, 7, 8, 20, 30, 50, 100].each do |n|
  puts "%3d -> %0.16f" % [n, ar[n]]
end

```

{{Out}}

```txt
  3 -> 18.5000000000000000
  4 -> 9.3783783783783784
  5 -> 7.8011527377521614
  6 -> 7.1544144809752494
  7 -> 6.8067847369236330
  8 -> 6.5926327687044384
 20 -> 6.0435521101892689
 30 -> 6.0067860930312058
 50 -> 6.0001758466271872
100 -> 6.0000000193194779
```



### Task 2: The Chaotic Bank Society

Using BigDecimal provides a way to specify the number of digits for E. 50 seems to be sufficient.

```Ruby
require 'bigdecimal/math'
balance = BigMath.E(50) - 1
1.upto(25){|y| balance = balance * y - 1}
puts "Bank balance after 25 years = #{balance.to_f}"
```

{{Out}}

```txt
Bank balance after 25 years = 0.03993872967323021

```


===Task 3: Rump's example===
Rationals again.

```Ruby
def rump(a,b)
  a, b = a.to_r, b.to_r
  333.75r * b**6 + a**2 * ( 11 * a**2 * b**2 - b**6 - 121 * b**4 - 2 )  + 5.5r *   b**8 + a / (2 * b)
end

puts "rump(77617, 33096) = #{rump(77617, 33096).to_f}"
```

{{out}}
```txt
rump(77617, 33096) = -0.8273960599468214

```


## Sidef

'''Muller's sequence'''

```ruby
func series (n) {
    var (u, v) = (2, -4)
    (n-2).times { (u, v) = (v, 111 - 1130/v + 3000/(v * u)) }
    return v
}

[(3..8)..., 20, 30, 50, 100].each {|n|
    printf("n = %3d -> %s\n", n, series(n))
}
```

{{out}}

```txt

n =   3 -> 18.5
n =   4 -> 9.3783783783783783783783783783783783783783783783784
n =   5 -> 7.801152737752161383285302593659942363112391930836
n =   6 -> 7.154414480975249353527890653860362024381233838197
n =   7 -> 6.806784736923632983941756596272009087623276707802
n =   8 -> 6.592632768704438392742002776365994826552982317735
n =  20 -> 6.043552110189268867777477364097540133187715000006
n =  30 -> 6.006786093031205758530554047953239705833072314438
n =  50 -> 6.000175846627187188945614020747195469523735177099
n = 100 -> 6.000000019319477929104086803403585715024350675437

```


'''The Chaotic Bank Society'''

```ruby
var years = 25
var balance = (1 .. years+15 -> sum_by {|n| 1 / n! })
say "Starting balance, $(e-1): $#{balance}"
for i in (1..years) { balance = (i*balance - 1) }
printf("After year %d, you will have $%1.16g in your account.\n", years, balance)
```

{{out}}

```txt

Starting balance, $(e-1): $1.7182818284590452353602874713526624977572470937
After year 25, you will have $0.03993872967323021 in your account.

```


'''Siegfried Rump's example'''

```ruby
func f (a, b) {
    (333.75 * b**6) + (a**2 * ((11 * a**2 * b**2) -
      b**6 - (121 * b**4) - 2)) + (5.5 * b**8) + a/(2*b)
}

say f(77617.0, 33096.0)
```

{{out}}

```txt

-0.8273960599468213681411650954798162919990331157844

```



## Stata

'''Task 1'''

```stata
clear
set obs 100
gen n=_n
gen v=.
replace v=2 in 1
replace v=-4 in 2
replace v=111-1130/v[_n-1]+3000/(v[_n-1]*v[_n-2]) in 3/l
format %20.16f v
list if inlist(n,3,4,5,6,7,8,20,30,50,100), noobs
```


'''Output'''

```txt

  +----------------------------+
  |   n                      v |
  |----------------------------|
  |   3    18.5000000000000000 |
  |   4     9.3783783783783790 |
  |   5     7.8011527377521688 |
  |   6     7.1544144809753334 |
  |   7     6.8067847369248113 |
  |----------------------------|
  |   8     6.5926327687217920 |
  |  20    98.3495031221653590 |
  |  30    99.9999999999989340 |
  |  50   100.0000000000000000 |
  | 100   100.0000000000000000 |
  +----------------------------+

```


'''Task 2'''


```stata
clear
set obs 26
gen year=_n-1
gen balance=exp(1)-1 in 1
replace balance=year*balance[_n-1]-1 in 2/l
list balance if year==25, noobs
```


'''Output'''


```txt

  +------------+
  |    balance |
  |------------|
  | -2.242e+09 |
  +------------+

```


If we replace exp(1)-1 by its value computed in higher precision by other means, the result is different, owing to the extreme sensibility of the computation.


```stata
clear
set obs 26
gen year=_n-1
gen balance=1.71828182845904523 in 1
replace balance=year*balance[_n-1]-1 in 2/l
list balance if year==25, noobs
```


'''Output'''


```txt

  +-----------+
  |   balance |
  |-----------|
  | 1.202e+09 |
  +-----------+

```


We can check the hexadecimal representation of both numbers. Note they differ only in the last bit:


```stata
di %21x exp(1)-1
di %21x 1.71828182845904523
```


'''Output'''


```txt

+1.b7e151628aed2X+000
+1.b7e151628aed3X+000

```


=={{header|TI-83 BASIC}}==
'''A sequence that seems to converge to a wrong limit'''

Use the SEQ mode to enter the arithmetic progression. Note the way to set 
  u(1)=2
  u(2)=-4

```ti83b
   nMin=1
   u(n)=111-1130/u(n-1) + 3000/(u(n-1)*u(n-2))
   u(nMin)={-4;2}
```

The result converges to the wrong limit!
{{out}}

```txt

u(20)  : 100.055202
u(30)  : 100
u(50)  : 100
u(100) : 100

```



## Visual Basic .NET

'''Compiler:''' Roslyn Visual Basic (language version >=15.3, e.g. with Visual Studio 2015)
{{works with|.NET Framework|4.6.2}}
{{works with|.NET Core|2.1}}
{{libheader|BigRationalLibrary|1.0.0}}

.NET has three built-in non-integral types, Single, Double, and Decimal.

Single and Double are 32-bit and 64-bit IEEE floating-point numbers, respectively, and so are susceptible to the rounding errors that this task is designed to demonstrate.

Decimal is a fixed-point number intended for use in financial calculations where a high number of significant digits are required and magnitudes are reasonably small. It has a 128-bit size and is stored as a 1-bit sign, 96-bit integer, and scaling factor (from 1 to 10^28) that determines the position of the decimal point. Decimal math in .NET is notoriously slow.

.NET Framework and Core do have the arbitrary-precision BigInteger structure, but do not have an arbitrary-precision rational type, though Microsoft did create a prototype BigRational that never made it into the library. Its source has been released on GitHub and a NuGet package was published for it in 2011 (at [https://www.nuget.org/packages/BigRationalLibrary/]). The type is fully functional and is sufficient for these tasks, so this program has been designed to optionally use it (and must be compiled with a reference to the library to do so).

The program defines three compiler constants that toggle:
* <code>USE_BIGRATIONAL</code>: whether to use BigRationalLibrary; when False, a mock type and functions are defined that allows the code to be compiled without a reference to the library.
* <code>BANDED_ROWS</code>: whether to change the console colors to format output tables with alternating white-on-black and black-on-white rows.
* <code>INCREASED_LIMITS</code>: whether to additionally display n = 1000 for Wrong Convergence Sequence and up to year 40 for The Chaotic Bank Society.

Because of operator overloading, the implementations are visually very similar. Integral literals are used where possible as they implicitly convert to the other types.

The non-floating-point implementations calculate e using the reciprocals of factorials formula because Math.E is a double and is not sufficiently precise.

Because BigRational.ToString() returns a string containing its exact numerator and denominator, the BigRational results are converted to Decimal for display.

The following sections source code must be located in a single file.

Main() procedure and output formatting:


```vbnet
Imports System.Globalization
Imports System.Numerics
Imports Numerics

#Const USE_BIGRATIONAL = True
#Const BANDED_ROWS = True
#Const INCREASED_LIMITS = True

#If Not USE_BIGRATIONAL Then
' Mock structure to make test code work.
Structure BigRational
    Overrides Function ToString() As String
        Return "NOT USING BIGRATIONAL"
    End Function
    Shared Narrowing Operator CType(value As BigRational) As Decimal
        Return -1
    End Operator
End Structure
#End If

Module Common
    Public Const FMT_STR = "{0,4}   {1,-15:G9}   {2,-24:G17}   {3,-32}   {4,-32}"
    Public ReadOnly Property Headings As String =
        String.Format(CultureInfo.InvariantCulture,
                      FMT_STR,
                      {"N", "Single", "Double", "Decimal", "BigRational (rounded as decimal)"})

    <Conditional("BANDED_ROWS")>
    Sub SetConsoleFormat(n As Integer)
        If n Mod 2 = 0 Then
            Console.BackgroundColor = ConsoleColor.Black
            Console.ForegroundColor = ConsoleColor.White
        Else
            Console.BackgroundColor = ConsoleColor.White
            Console.ForegroundColor = ConsoleColor.Black
        End If
    End Sub

    Function FormatOutput(n As Integer, x As (sn As Single, db As Double, dm As Decimal, br As BigRational)) As String
        SetConsoleFormat(n)
        Return String.Format(CultureInfo.CurrentCulture, FMT_STR, n, x.sn, x.db, x.dm, CDec(x.br))
    End Function

    Sub Main()
        WrongConvergence()
        Console.WriteLine()
        ChaoticBankSociety()

        Console.WriteLine()
        SiegfriedRump()

        SetConsoleFormat(0)
    End Sub
End Module
```


<!--Anchors for the C# section to link to-->
==={{anchor|VB.NET Task 1}}Task 1: Converging sequence===
Somewhat predictably, Single fairs the worst and Double slightly better. Decimal lasts past iteration 20, and BigRational remains exactly precise.


```vbnet
Module Task1
    Iterator Function SequenceSingle() As IEnumerable(Of Single)
        ' n, n-1, and n-2
        Dim vn, vn_1, vn_2 As Single
        vn_2 = 2
        vn_1 = -4

        Do
            Yield vn_2
            vn = 111 - (1130 / vn_1) + (3000 / (vn_1 * vn_2))
            vn_2 = vn_1
            vn_1 = vn
        Loop
    End Function

    Iterator Function SequenceDouble() As IEnumerable(Of Double)
        ' n, n-1, and n-2
        Dim vn, vn_1, vn_2 As Double
        vn_2 = 2
        vn_1 = -4

        Do
            Yield vn_2
            vn = 111 - (1130 / vn_1) + (3000 / (vn_1 * vn_2))
            vn_2 = vn_1
            vn_1 = vn
        Loop
    End Function

    Iterator Function SequenceDecimal() As IEnumerable(Of Decimal)
        ' n, n-1, and n-2
        Dim vn, vn_1, vn_2 As Decimal
        vn_2 = 2
        vn_1 = -4

        ' Use constants to avoid calling the Decimal constructor in the loop.
        Const i11 As Decimal = 111
        Const i130 As Decimal = 1130
        Const E000 As Decimal = 3000

        Do
            Yield vn_2
            vn = i11 - (i130 / vn_1) + (E000 / (vn_1 * vn_2))
            vn_2 = vn_1
            vn_1 = vn
        Loop
    End Function

#If USE_BIGRATIONAL Then
    Iterator Function SequenceRational() As IEnumerable(Of BigRational)
        ' n, n-1, and n-2
        Dim vn, vn_1, vn_2 As BigRational
        vn_2 = 2
        vn_1 = -4

        ' Same reasoning as for Decimal.
        Dim i11 As BigRational = 111
        Dim i130 As BigRational = 1130
        Dim E000 As BigRational = 3000

        Do
            Yield vn_2
            vn = i11 - (i130 / vn_1) + (E000 / (vn_1 * vn_2))
            vn_2 = vn_1
            vn_1 = vn
        Loop
    End Function
#Else
    Iterator Function SequenceRational() As IEnumerable(Of BigRational)
        Do
            Yield Nothing
        Loop
    End Function
#End If

    <Conditional("INCREASED_LIMITS")>
    Sub IncreaseMaxN(ByRef arr As Integer())
        ReDim Preserve arr(arr.Length)
        arr(arr.Length - 1) = 1000
    End Sub

    Sub WrongConvergence()
        Console.WriteLine("Wrong Convergence Sequence:")

        Dim displayedIndices As Integer() = {3, 4, 5, 6, 7, 8, 20, 30, 50, 100}
        IncreaseMaxN(displayedIndices)

        Dim indicesSet As New HashSet(Of Integer)(displayedIndices)

        Console.WriteLine(Headings)

        Dim n As Integer = 1
        ' Enumerate the implementations in parallel as tuples.
        For Each x In SequenceSingle().
                      Zip(SequenceDouble(), Function(sn, db) (sn, db)).
                      Zip(SequenceDecimal(), Function(a, dm) (a.sn, a.db, dm)).
                      Zip(SequenceRational(), Function(a, br) (a.sn, a.db, a.dm, br))
            If n > displayedIndices.Max() Then Exit For

            If indicesSet.Contains(n) Then
                Console.WriteLine(FormatOutput(n, x))
            End If

            n += 1
        Next
    End Sub
End Module
```


{{out}}
Note that "Wrong" is not a heading for the first column--a weird optical effect, that would be.

```txt
Wrong Convergence Sequence:
   N   Single            Double                     Decimal                            BigRational (rounded as decimal)
   3   18.5              18.5                       18.5                               18.5
   4   9.37837791        9.378378378378379          9.378378378378378378378378378      9.378378378378378378378378378
   5   7.80114746        7.8011527377521688         7.80115273775216138328530259       7.8011527377521613832853025937
   6   7.15434647        7.1544144809753334         7.154414480975249353527890606      7.1544144809752493535278906539
   7   6.80583048        6.8067847369248113         6.806784736923632983941755925      6.8067847369236329839417565963
   8   6.57857943        6.592632768721792          6.592632768704438392741992887      6.5926327687044383927420027764
  20   100               98.349503122165359         6.04355210719488789087813234       6.0435521101892688677774773641
  30   100               99.999999999998934         101.88552052291609961584734802     6.006786093031205758530554048
  50   100               100                        100.00000000000000000000000068     6.0001758466271871889456140207
 100   100               100                        100.0                              6.0000000193194779291040868034
1000   100               100                        100.0                              6.0000000000000000000000000000
```


==={{anchor|VB.NET Task 2}}Task 2: The Chaotic Bank Society===
Decimal appears to be doing well by year 25, but begins to degenerate ''the very next year'' and soon overflows.


```vbnet
Module Task2
    Iterator Function ChaoticBankSocietySingle() As IEnumerable(Of Single)
        Dim balance As Single = Math.E - 1
        Dim year As Integer = 1

        Do
            balance = (balance * year) - 1
            Yield balance
            year += 1
        Loop
    End Function
    Iterator Function ChaoticBankSocietyDouble() As IEnumerable(Of Double)
        Dim balance As Double = Math.E - 1
        Dim year As Integer = 1

        Do
            balance = (balance * year) - 1
            Yield balance
            year += 1
        Loop
    End Function

    Iterator Function ChaoticBankSocietyDecimal() As IEnumerable(Of Decimal)
        ' 27! is the largest factorial decimal can represent.
        Dim balance As Decimal = CalculateEDecimal(27) - Decimal.One
        Dim year As Integer = 1

        Do
            balance = (balance * year) - Decimal.One
            Yield balance
            year += 1
        Loop
    End Function

#If USE_BIGRATIONAL Then
    Iterator Function ChaoticBankSocietyRational() As IEnumerable(Of BigRational)
        ' 100 iterations is precise enough for 25 years.
        Dim balance As BigRational = CalculateEBigRational(100) - BigRational.One
        Dim year As Integer = 1

        Do
            balance = (balance * year) - BigRational.One
            Yield balance
            year += 1
        Loop
    End Function
#Else
    Iterator Function ChaoticBankSocietyRational() As IEnumerable(Of BigRational)
        Do
            Yield Nothing
        Loop
    End Function
#End If

    Function CalculateEDecimal(terms As Integer) As Decimal
        Dim e As Decimal = 1
        Dim fact As Decimal = 1

        For i As Integer = 1 To terms
            fact *= i
            e += Decimal.One / fact
        Next

        Return e
    End Function

#If USE_BIGRATIONAL Then
    Function CalculateEBigRational(terms As Integer) As BigRational
        Dim e As BigRational = 1
        Dim fact As BigInteger = 1

        For i As Integer = 1 To terms
            fact *= i
            e += BigRational.Invert(fact)
        Next

        Return e
    End Function
#End If

    <Conditional("INCREASED_LIMITS")>
    Sub IncreaseMaxYear(ByRef year As Integer)
        year = 40
    End Sub

    Sub ChaoticBankSociety()
        Console.WriteLine("Chaotic Bank Society:")
        Console.WriteLine(Headings)

        Dim maxYear As Integer = 25
        IncreaseMaxYear(maxYear)

        Dim i As Integer = 0
        For Each x In ChaoticBankSocietySingle().
                      Zip(ChaoticBankSocietyDouble(), Function(sn, db) (sn, db)).
                      Zip(ChaoticBankSocietyDecimal(), Function(a, dm) (a.sn, a.db, dm)).
                      Zip(ChaoticBankSocietyRational(), Function(a, br) (a.sn, a.db, a.dm, br))
            If i >= maxYear Then Exit For
            Console.WriteLine(FormatOutput(i + 1, x))
            i += 1
        Next
    End Sub
End Module
```


{{out}}

```txt
Chaotic Bank Society:
   N   Single            Double                     Decimal                            BigRational (rounded as decimal)
   1   0.718281865       0.71828182845904509        0.7182818284590452353602874714     0.7182818284590452353602874713
   2   0.43656373        0.43656365691809018        0.4365636569180904707205749428     0.4365636569180904707205749427
   3   0.309691191       0.30969097075427054        0.3096909707542714121617248284     0.3096909707542714121617248281
   4   0.238764763       0.23876388301708218        0.2387638830170856486468993136     0.2387638830170856486468993124
   5   0.193823814       0.1938194150854109         0.1938194150854282432344965680     0.1938194150854282432344965623
   6   0.162942886       0.16291649051246537        0.1629164905125694594069794080     0.1629164905125694594069793739
   7   0.140600204       0.14041543358725761        0.1404154335879862158488558560     0.1404154335879862158488556174
   8   0.124801636       0.12332346869806088        0.1233234687038897267908468480     0.1233234687038897267908449393
   9   0.123214722       0.10991121828254791        0.1099112183350075411176216320     0.1099112183350075411176044541
  10   0.232147217       0.099112182825479067       0.0991121833500754111762163200     0.0991121833500754111760445416
  11   1.55361938        0.090234011080269738       0.0902340168508295229383795200     0.0902340168508295229364899583
  12   17.6434326        0.082808132963236858       0.0828082022099542752605542400     0.0828082022099542752378795006
  13   228.364624        0.076505728522079153       0.0765066287294055783872051200     0.0765066287294055780924335089
  14   3196.10474        0.071080199309108139       0.0710928022116780974208716800     0.0710928022116780932940691248
  15   47940.5703        0.066202989636622078       0.0663920331751714613130752000     0.0663920331751713994110368720
  16   767048.125        0.059247834185953252       0.0622725308027433810092032000     0.0622725308027423905765899521
  17   13039817          0.0072131811612052843      0.0586330236466374771564544000     0.0586330236466206398020291865
  18   234716704         -0.87016273909830488       0.0553944256394745888161792000     0.0553944256391715164365253585
  19   4.45961728E+09    -17.533092042867793        0.0524940871500171875074048000     0.0524940871442588122939818127
  20   8.91923415E+10    -351.66184085735586        0.0498817430003437501480960000     0.0498817428851762458796362544
  21   1.8730392E+12     -7385.898658004473         0.0475166030072187531100160000     0.0475166005887011634723613427
  22   4.12068642E+13    -162490.77047609841        0.0453652661588125684203520000     0.0453652129514255963919495414
  23   9.47757884E+14    -3737288.7209502636        0.0434011216526890736680960000     0.0433998978827887170148394524
  24   2.27461897E+16    -89694930.302806318        0.0416269196645377680343040000     0.0415975491869292083561468582
  25   5.68654735E+17    -2242373258.570158         0.0406729916134442008576000000     0.0399387296732302089036714552
  26   1.47850229E+19    -58301704723.824112        0.0574977819495492222976000000     0.0384069715039854314954578354
  27   3.99195623E+20    -1574146027544.251         0.5524401126378290020352000000     0.0369882306076066503773615576
  28   1.11774772E+22    -44076088771240.031        14.468323153859212056985600000     0.0356704570129862105661236148
  29   3.24146835E+23    -1278206574365962          418.58137146191714965258240000     0.0344432533766001064175848308
  30   9.72440521E+24    -38346197230978864         12556.441143857514489577472000     0.0332976012980031925275449256
  31   3.01456563E+26    -1.1887321141603448E+18    389248.67545958294917690163200     0.0322256402380989683538926936
  32   9.64661E+27       -3.8039427653131035E+19    12455956.614706654373660852224     0.0312204876191669873245661979
  33   3.18338125E+29    -1.2553011125533242E+21    411046567.28531959433080812339     0.0302760914325105817106845313
  34   1.08234959E+31    -4.268023782681302E+22     13975583286.700866207247476195     0.0293871087053597781632740646
  35   3.78822341E+32    -1.4938083239384556E+24    489145415033.53031725366166682     0.0285488046875922357145922644
  36   1.36376043E+34    -5.3777099661784406E+25    17609234941206.091421131820006     0.0277569687533204857253215188
  37   5.04591372E+35    -1.989752687486023E+27     651541692824624.38258187734022     0.0270078438728579718368961961
  38   1.91744716E+37    -7.5610602124468873E+28    24758584327335725.538111338928     0.0262980671686029298020554545
  39   ∞                 -2.9488134828542859E+30    965584788766093294.9863422182      0.0256246195755142622801627278
  40   ∞                 -1.1795253931417144E+32    38623391550643731798.453688728     0.0249847830205704912065091156
```


==={{anchor|VB.NET Task 3}}Task 3: Rump's example===
Because Decimal is fixed point, the polynomial as displayed in the task cannot be evaluated for the specified input because of intermediate values (b<sup>8</sup>, for instance) being too large.

Possibly due to JIT optimizations (''and nasal demons resulting from attempting to ensure that floating-point numbers are (un)equal''), the outputs for Single and Double are identical (both equal to that for Double in debug builds) for optimized (release) builds targeting x86 or AnyCPU-prefer-x86. Since the the machine the author used is x64, the Single was likely stored (with extra precision as the CLR specification allows) in a 64-bit register.

Each of these three tasks are susceptible to this phenomenon; the outputs of this program for floating-point arithmetic should not be expected to be the same on different systems.


```vbnet
Module Task3
    Function SiegfriedRumpSingle(a As Single, b As Single) As Single
        Dim a2 = a * a,
            b2 = b * b,
            b4 = b2 * b2,
            b6 = b4 * b2

        ' Non-integral literals must be coerced to Single using the type suffix.
        Return 333.75F * b6 +
            (a2 * (
                11 * a2 * b2 -
                b6 -
                121 * b4 -
                2)) +
            5.5F * b4 * b4 +
            a / (2 * b)
    End Function

    Function SiegfriedRumpDouble(a As Double, b As Double) As Double
        Dim a2 = a * a,
            b2 = b * b,
            b4 = b2 * b2,
            b6 = b4 * b2

        ' Non-integral literals are Doubles by default.
        Return 333.75 * b6 +
            (a2 * (
                11 * a2 * b2 -
                b6 -
                121 * b4 -
                2)) +
            5.5 * b4 * b4 +
            a / (2 * b)
    End Function

    Function SiegfriedRumpDecimal(a As Decimal, b As Decimal) As Decimal
        Dim a2 = a * a,
            b2 = b * b,
            b4 = b2 * b2,
            b6 = b4 * b2

        ' The same applies for Decimal.
        Return 333.75D * b6 +
            (a2 * (
                11 * a2 * b2 -
                b6 -
                121 * b4 -
                2)) +
            5.5D * b4 * b4 +
            a / (2 * b)
    End Function

#If USE_BIGRATIONAL Then
    Function SiegfriedRumpRational(a As BigRational, b As BigRational) As BigRational
        ' Use mixed number constructor to maintain exact precision (333+3/4, 5+1/2).
        Dim c1 As New BigRational(333, 3, 4)
        Dim c2 As New BigRational(5, 1, 2)

        Dim a2 = a * a,
            b2 = b * b,
            b4 = b2 * b2,
            b6 = b4 * b2

        Return c1 * b6 +
            (a2 * (
                11 * a2 * b2 -
                b6 -
                121 * b4 -
                2)) +
            c2 * b4 * b4 +
            a / (2 * b)
    End Function
#Else
    Function SiegfriedRumpRational(a As Integer, b As Integer) As BigRational
        Return Nothing
    End Function
#End If

    Sub SiegfriedRump()
        Console.WriteLine("Siegfried Rump Formula:")
        Dim a As Integer = 77617
        Dim b As Integer = 33096

        Console.Write("Single: ")
        Dim sn As Single = SiegfriedRumpSingle(a, b)
        Console.WriteLine("{0:G9}", sn)
        Console.WriteLine()

        Console.Write("Double: ")
        Dim db As Double = SiegfriedRumpDouble(a, b)
        Console.WriteLine("{0:G17}", db)
        Console.WriteLine()

        Console.WriteLine("Decimal:")
        Dim dm As Decimal
        Try
            dm = SiegfriedRumpDecimal(a, b)
        Catch ex As OverflowException
            Console.WriteLine("Exception: " + ex.Message)
        End Try
        Console.WriteLine($"  {dm}")
        Console.WriteLine()

        Console.WriteLine("BigRational:")
        Dim br As BigRational = SiegfriedRumpRational(a, b)
        Console.WriteLine($"  Rounded: {CDec(br)}")
        Console.WriteLine($"  Exact: {br}")
    End Sub
End Module
```


{{out|note=debug build}}

```txt
Siegfried Rump Formula
Single: -6.27969943E+29

Double: -2.3611832414348226E+21

Decimal:
Exception: Value was either too large or too small for a Decimal.
  0

BigRational:
  Rounded: -0.8273960599468213681411650955
  Exact: -54767/66192
```


{{out|note=release build}}

```txt
Siegfried Rump Formula:
Single: -2.36118324E+21

Double: -2.3611832414348226E+21

Decimal:
Exception: Value was either too large or too small for a Decimal.
  0

BigRational:
  Rounded: -0.8273960599468213681411650955
  Exact: -54767/66192
```



## zkl

zkl doesn't have a big rational or big float library (as of this writing) but does have big ints (via GNU GMP). It does have 64 bit doubles.

```zkl
Series:=Walker(fcn(vs){  // just keep appending new values to a list
   vs.append(111.0 - 1130.0/vs[-1] + 3000.0/(vs[-1]*vs[-2])) }.fp(List(2,-4)));
series:=Series.drop(100).value;
```

We'll use the convenient formula given in the referenced paper to create a fraction with big ints

```zkl
var BN=Import("zklBigNum"), ten2n=BN(10).pow(64);

fcn u(n){  // use formula to create a fraction of big ints
   const B=-3, Y=4;
   N:=BN(6).pow(n+1)*B + BN(5).pow(n+1)*Y;
   D:=BN(6).pow(n)*B   + BN(5).pow(n)*Y;
   tostr(N*ten2n/D,64,32)
}

fcn tostr(bn,m,r){ // convert big int (*10^m) to float string with len r remainder, flakey
   str,d:=bn.toString(), str.len()-m;
   if(d<0) String(".","0"*-d,str[0,r]);
   else    String(str[0,d],".",str[d,r]);
}

println("1st: Convergent series");
foreach n in (T(3,4,5,6,7,8,20,30,50,100)){ 
   "n =%3d; %3.20F  %s".fmt(n,series[n-1],u(n-1)).println();
}
```

{{out}}
Note that, at n=100, we still have diverged (at the 15th place) from the Perl6 solution and 12th place from the J solution.

```txt

1st: Convergent series
n =  3;  18.50000000000000000000  18.50000000000000000000000000000000
n =  4;   9.37837837837837895449  9.37837837837837837837837837837837
n =  5;   7.80115273775216877539  7.80115273775216138328530259365994
n =  6;   7.15441448097533339023  7.15441448097524935352789065386036
n =  7;   6.80678473692481134094  6.80678473692363298394175659627200
n =  8;   6.59263276872179204702  6.59263276870443839274200277636599
n = 20;  98.34950312216535905918  6.04355211018926886777747736409754
n = 30;  99.99999999999893418590  6.00678609303120575853055404795323
n = 50; 100.00000000000000000000  6.00017584662718718894561402074719
n =100; 100.00000000000000000000  6.00000001931947792910408680340358

```

Chaotic banking society is just nasty so we use a five hundred digit e (the e:= text is one long line).

```zkl
println("\n2nd: Chaotic banking society");
e:="271828182845904523536028747135266249775724709369995957496696762772407663035354759457138217852516642742746639193200305992181741359662904357290033429526059563073813232862794349076323382988075319525101901157383418793070215408914993488416750924476146066808226480016847741185374234544243710753907774499206955170276183860626133138458300075204493382656029760673711320070932870912744374704723069697720931014169283681902551510865746377211125238978442505695369677078544996996794686445490598793163688923009879312";
var en=(e.len()-1), tenEN=BN(10).pow(en);
years,balance:=25, BN(e).sub(tenEN);  // in place math
balance=[1..years].reduce(fcn(balance,i){ balance*i - tenEN },balance);
balance=tostr(balance,en,2);
println("After year %d, you will have $%s in your account.".fmt(years,balance));
```

{{out}}

```txt

2nd: Chaotic banking society
After year 25, you will have $.039 in your account.

```

For Rump's example, multiple the formula by 10ⁿ so we can use integer math.

```zkl
fcn rump(a,b){ b=BN(b);
   b2,b4,b6,b8:=b.pow(2),b.pow(4),b.pow(6),b.pow(8);
   a2:=BN(a).pow(2);
   r:=( b6*33375 + a2*(a2*b2*11 - b6 - b4*121 - 2)*100 + b8*550 )*ten2n;
   r+=BN(a)*ten2n*100/(2*b);
   tostr(r,66,32)
}
println("\n3rd: Rump's example: f(77617.0, 33096.0) = ",rump(77617,33096));
```

{{out}}

```txt

3rd: Rump's example: f(77617.0, 33096.0) = -.82739605994682136814116509547981

```

