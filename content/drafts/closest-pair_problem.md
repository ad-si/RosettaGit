+++
title = "Closest-pair problem"
description = ""
date = 2019-08-10T22:23:13Z
aliases = []
[extra]
id = 4142
[taxonomies]
categories = []
tags = []
+++

{{task|Classic CS problems and programs}}
{{Wikipedia|Closest pair of points problem}}


;Task:
Provide a function to find the closest two points among a set of given points in two dimensions,   i.e. to solve the   [[wp:Closest pair of points problem|Closest pair of points problem]]   in the   ''planar''   case.

The straightforward solution is a   O(n<sup>2</sup>)   algorithm   (which we can call ''brute-force algorithm'');   the pseudo-code (using indexes) could be simply:

 '''bruteForceClosestPair''' of P(1), P(2), ... P(N)
 '''if''' N &lt; 2 '''then'''
   '''return''' ∞
 '''else'''
   minDistance ← |P(1) - P(2)|
   minPoints ← { P(1), P(2) }
   '''foreach''' i ∈ [1, N-1]
     '''foreach''' j ∈ [i+1, N]
       '''if''' |P(i) - P(j)| < minDistance '''then'''
         minDistance ← |P(i) - P(j)|
         minPoints ← { P(i), P(j) }
       '''endif'''
     '''endfor'''
   '''endfor'''
   '''return''' minDistance, minPoints
  '''endif'''

A better algorithm is based on the recursive divide&amp;conquer approach,   as explained also at   [[wp:Closest pair of points problem#Planar_case|Wikipedia's Closest pair of points problem]],   which is   O(''n'' log ''n'');   a pseudo-code could be:

 '''closestPair''' of (xP, yP)
                where xP is P(1) .. P(N) sorted by x coordinate, and
                      yP is P(1) .. P(N) sorted by y coordinate (ascending order)
 '''if''' N ≤ 3 '''then'''
   '''return''' closest points of xP using brute-force algorithm
 '''else'''
   xL ← points of xP from 1 to ⌈N/2⌉
   xR ← points of xP from ⌈N/2⌉+1 to N
   xm ← xP(⌈N/2⌉)<sub>x</sub>
   yL ← { p ∈ yP : p<sub>x</sub> ≤ xm }
   yR ← { p ∈ yP : p<sub>x</sub> &gt; xm }
   (dL, pairL) ← ''closestPair'' of (xL, yL)
   (dR, pairR) ← ''closestPair'' of (xR, yR)
   (dmin, pairMin) ← (dR, pairR)
   '''if''' dL &lt; dR '''then'''
     (dmin, pairMin) ← (dL, pairL)
   '''endif'''
   yS ← { p ∈ yP : |xm - p<sub>x</sub>| &lt; dmin }
   nS ← number of points in yS
   (closest, closestPair) ← (dmin, pairMin)
   '''for''' i '''from''' 1 '''to''' nS - 1
     k ← i + 1
     '''while''' k ≤ nS '''and''' yS(k)<sub>y</sub> - yS(i)<sub>y</sub> &lt; dmin
       '''if''' |yS(k) - yS(i)| &lt; closest '''then'''
         (closest, closestPair) ← (|yS(k) - yS(i)|, {yS(k), yS(i)})
       '''endif'''
       k ← k + 1
     '''endwhile'''
   '''endfor'''
   '''return''' closest, closestPair
 '''endif'''


;References and further readings:
*   [[wp:Closest pair of points problem|Closest pair of points problem]]
*   [http://www.cs.mcgill.ca/~cs251/ClosestPair/ClosestPairDQ.html Closest Pair (McGill)]
*   [http://www.cs.ucsb.edu/~suri/cs235/ClosestPair.pdf Closest Pair (UCSB)]
*   [http://classes.cec.wustl.edu/~cse241/handouts/closestpair.pdf Closest pair (WUStL)]
*   [http://www.cs.iupui.edu/~xkzou/teaching/CS580/Divide-and-conquer-closestPair.ppt Closest pair (IUPUI)]






## 360 Assembly


```360asm
*        Closest Pair Problem      10/03/2017
CLOSEST  CSECT
         USING  CLOSEST,R13        base register
         B      72(R15)            skip savearea
         DC     17F'0'             savearea
         STM    R14,R12,12(R13)    save previous context
         ST     R13,4(R15)         link backward
         ST     R15,8(R13)         link forward
         LR     R13,R15            set addressability
         LA     R6,1               i=1
         LA     R7,2               j=2
         BAL    R14,DDCALC         dd=(px(i)-px(j))^2+(py(i)-py(j))^2
         BAL    R14,DDSTORE        ddmin=dd; ii=i; jj=j
         LA     R6,1               i=1
       DO WHILE=(C,R6,LE,N)        do i=1 to n
         LA     R7,1                 j=1
       DO WHILE=(C,R7,LE,N)          do j=1 to n
         BAL    R14,DDCALC         dd=(px(i)-px(j))^2+(py(i)-py(j))^2
       IF CP,DD,GT,=P'0' THEN          if dd>0 then
       IF CP,DD,LT,DDMIN THEN            if dd<ddmin then
         BAL    R14,DDSTORE                ddmin=dd; ii=i; jj=j
       ENDIF    ,                        endif
       ENDIF    ,                      endif
         LA     R7,1(R7)               j++
       ENDDO    ,                    enddo j
         LA     R6,1(R6)             i++
       ENDDO    ,                  enddo i
         ZAP    WPD,DDMIN          ddmin
         DP     WPD,=PL8'2'        ddmin/2
         ZAP    SQRT2,WPD(8)       sqrt2=ddmin/2
         ZAP    SQRT1,DDMIN        sqrt1=ddmin
       DO WHILE=(CP,SQRT1,NE,SQRT2)  do while sqrt1<>sqrt2
         ZAP    SQRT1,SQRT2          sqrt1=sqrt2
         ZAP    WPD,DDMIN            ddmin
         DP     WPD,SQRT1            /sqrt1
         ZAP    WP1,WPD(8)           ddmin/sqrt1
         AP     WP1,SQRT1            +sqrt1
         ZAP    WPD,WP1              ~
         DP     WPD,=PL8'2'          /2
         ZAP    SQRT2,WPD(8)         sqrt2=(sqrt1+(ddmin/sqrt1))/2
       ENDDO    ,                  enddo while
         MVC    PG,=CL80'the minimum distance '
         ZAP    WP1,SQRT2          sqrt2
         BAL    R14,EDITPK         edit
         MVC    PG+21(L'WC),WC     output
         XPRNT  PG,L'PG            print buffer
         XPRNT  =CL22'is between the points:',22
         MVC    PG,PGP             init buffer
         L      R1,II              ii
         SLA    R1,4               *16
         LA     R4,PXY-16(R1)      @px(ii)
         MVC    WP1,0(R4)          px(ii)
         BAL    R14,EDITPK         edit
         MVC    PG+3(L'WC),WC      output
         MVC    WP1,8(R4)          py(ii)
         BAL    R14,EDITPK         edit
         MVC    PG+21(L'WC),WC     output
         XPRNT  PG,L'PG            print buffer
         MVC    PG,PGP             init buffer
         L      R1,JJ              jj
         SLA    R1,4               *16
         LA     R4,PXY-16(R1)      @px(jj)
         MVC    WP1,0(R4)          px(jj)
         BAL    R14,EDITPK         edit
         MVC    PG+3(L'WC),WC      output
         MVC    WP1,8(R4)          py(jj)
         BAL    R14,EDITPK         edit
         MVC    PG+21(L'WC),WC     output
         XPRNT  PG,L'PG            print buffer
         L      R13,4(0,R13)       restore previous savearea pointer
         LM     R14,R12,12(R13)    restore previous context
         XR     R15,R15            rc=0
         BR     R14                exit
DDCALC   EQU    *             ---- dd=(px(i)-px(j))^2+(py(i)-py(j))^2
         LR     R1,R6              i
         SLA    R1,4               *16
         LA     R4,PXY-16(R1)      @px(i)
         LR     R1,R7              j
         SLA    R1,4               *16
         LA     R5,PXY-16(R1)      @px(j)
         ZAP    WP1,0(8,R4)        px(i)
         ZAP    WP2,0(8,R5)        px(j)
         SP     WP1,WP2            px(i)-px(j)
         ZAP    WPS,WP1            =
         MP     WP1,WPS            (px(i)-px(j))*(px(i)-px(j))
         ZAP    WP2,8(8,R4)        py(i)
         ZAP    WP3,8(8,R5)        py(j)
         SP     WP2,WP3            py(i)-py(j)
         ZAP    WPS,WP2            =
         MP     WP2,WPS            (py(i)-py(j))*(py(i)-py(j))
         AP     WP1,WP2            (px(i)-px(j))^2+(py(i)-py(j))^2
         ZAP    DD,WP1             dd=(px(i)-px(j))^2+(py(i)-py(j))^2
         BR     R14           ---- return
DDSTORE  EQU    *             ---- ddmin=dd; ii=i; jj=j
         ZAP    DDMIN,DD           ddmin=dd
         ST     R6,II              ii=i
         ST     R7,JJ              jj=j
         BR     R14           ---- return
EDITPK   EQU    *             ----
         MVC    WM,MASK            set mask
         EDMK   WM,WP1             edit and mark
         BCTR   R1,0               -1
         MVC    0(1,R1),WM+17      set sign
         MVC    WC,WM              len17<-len18
         BR     R14           ---- return
N        DC     A((PGP-PXY)/16)
PXY      DC     PL8'0.654682',PL8'0.925557',PL8'0.409382',PL8'0.619391'
         DC     PL8'0.891663',PL8'0.888594',PL8'0.716629',PL8'0.996200'
         DC     PL8'0.477721',PL8'0.946355',PL8'0.925092',PL8'0.818220'
         DC     PL8'0.624291',PL8'0.142924',PL8'0.211332',PL8'0.221507'
         DC     PL8'0.293786',PL8'0.691701',PL8'0.839186',PL8'0.728260'
PGP      DC     CL80'  [+xxxxxxxxx.xxxxxx,+xxxxxxxxx.xxxxxx]'
MASK     DC     C' ',7X'20',X'21',X'20',C'.',6X'20',C'-'  CL18 15num
II       DS     F
JJ       DS     F
DD       DS     PL8
DDMIN    DS     PL8
SQRT1    DS     PL8
SQRT2    DS     PL8
WP1      DS     PL8
WP2      DS     PL8
WP3      DS     PL8
WPS      DS     PL8
WPD      DS     PL16
WM       DS     CL18
WC       DS     CL17
PG       DS     CL80
         YREGS
         END    CLOSEST
```

{{out}}

```txt

the minimum distance          0.077910
is between the points:
  [         0.891663,         0.888594]
  [         0.925092,         0.818220]

```




## Ada

Dimension independent, but has to be defined at procedure call time
(could be a parameter).
Output is simple, can be formatted using Float_IO.

closest.adb: (uses brute force algorithm)

```Ada
with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Text_IO;

procedure Closest is
   package Math is new Ada.Numerics.Generic_Elementary_Functions (Float);

   Dimension : constant := 2;
   type Vector is array (1 .. Dimension) of Float;
   type Matrix is array (Positive range <>) of Vector;

   -- calculate the distance of two points
   function Distance (Left, Right : Vector) return Float is
      Result : Float := 0.0;
      Offset : Natural := 0;
   begin
      loop
         Result := Result + (Left(Left'First + Offset) - Right(Right'First + Offset))**2;
         Offset := Offset + 1;
         exit when Offset >= Left'Length;
      end loop;
      return Math.Sqrt (Result);
   end Distance;

   -- determine the two closest points inside a cloud of vectors
   function Get_Closest_Points (Cloud : Matrix) return Matrix is
      Result : Matrix (1..2);
      Min_Distance : Float;
   begin
      if Cloud'Length(1) < 2 then
         raise Constraint_Error;
      end if;
      Result := (Cloud (Cloud'First), Cloud (Cloud'First + 1));
      Min_Distance := Distance (Cloud (Cloud'First), Cloud (Cloud'First + 1));
      for I in Cloud'First (1) .. Cloud'Last(1) - 1 loop
         for J in I + 1 .. Cloud'Last(1) loop
            if Distance (Cloud (I), Cloud (J)) < Min_Distance then
               Min_Distance := Distance (Cloud (I), Cloud (J));
               Result := (Cloud (I), Cloud (J));
            end if;
         end loop;
      end loop;
      return Result;
   end Get_Closest_Points;

   Test_Cloud : constant Matrix (1 .. 10) := ( (5.0, 9.0),  (9.0, 3.0),
                                               (2.0, 0.0),  (8.0, 4.0),
                                               (7.0, 4.0),  (9.0, 10.0),
                                               (1.0, 9.0),  (8.0, 2.0),
                                               (0.0, 10.0), (9.0, 6.0));
   Closest_Points : Matrix := Get_Closest_Points (Test_Cloud);

   Second_Test : constant Matrix (1 .. 10) := ( (0.654682, 0.925557), (0.409382, 0.619391),
                                                (0.891663, 0.888594), (0.716629,   0.9962),
                                                (0.477721, 0.946355), (0.925092,  0.81822),
                                                (0.624291, 0.142924), (0.211332, 0.221507),
                                                (0.293786, 0.691701), (0.839186,  0.72826));
   Second_Points : Matrix := Get_Closest_Points (Second_Test);
begin
   Ada.Text_IO.Put_Line ("Closest Points:");
   Ada.Text_IO.Put_Line ("P1: " & Float'Image (Closest_Points (1) (1)) & " " & Float'Image (Closest_Points (1) (2)));
   Ada.Text_IO.Put_Line ("P2: " & Float'Image (Closest_Points (2) (1)) & " " & Float'Image (Closest_Points (2) (2)));
   Ada.Text_IO.Put_Line ("Distance: " & Float'Image (Distance (Closest_Points (1), Closest_Points (2))));
   Ada.Text_IO.Put_Line ("Closest Points 2:");
   Ada.Text_IO.Put_Line ("P1: " & Float'Image (Second_Points (1) (1)) & " " & Float'Image (Second_Points (1) (2)));
   Ada.Text_IO.Put_Line ("P2: " & Float'Image (Second_Points (2) (1)) & " " & Float'Image (Second_Points (2) (2)));
   Ada.Text_IO.Put_Line ("Distance: " & Float'Image (Distance (Second_Points (1), Second_Points (2))));
end Closest;
```


{{out}}

```txt
Closest Points:
P1:  8.00000E+00  4.00000E+00
P2:  7.00000E+00  4.00000E+00
Distance:  1.00000E+00
Closest Points 2:
P1:  8.91663E-01  8.88594E-01
P2:  9.25092E-01  8.18220E-01
Distance:  7.79101E-02
```



## AWK


```AWK

# syntax: GAWK -f CLOSEST-PAIR_PROBLEM.AWK
BEGIN {
    x[++n] = 0.654682 ; y[n] = 0.925557
    x[++n] = 0.409382 ; y[n] = 0.619391
    x[++n] = 0.891663 ; y[n] = 0.888594
    x[++n] = 0.716629 ; y[n] = 0.996200
    x[++n] = 0.477721 ; y[n] = 0.946355
    x[++n] = 0.925092 ; y[n] = 0.818220
    x[++n] = 0.624291 ; y[n] = 0.142924
    x[++n] = 0.211332 ; y[n] = 0.221507
    x[++n] = 0.293786 ; y[n] = 0.691701
    x[++n] = 0.839186 ; y[n] = 0.728260
    min = 1E20
    for (i=1; i<=n-1; i++) {
      for (j=i+1; j<=n; j++) {
        dsq = (x[i]-x[j])^2 + (y[i]-y[j])^2
        if (dsq < min) {
          min = dsq
          mini = i
          minj = j
        }
      }
    }
    printf("distance between (%.6f,%.6f) and (%.6f,%.6f) is %g\n",x[mini],y[mini],x[minj],y[minj],sqrt(min))
    exit(0)
}

```

{{out}}

```txt

distance between (0.891663,0.888594) and (0.925092,0.818220) is 0.0779102

```



## BASIC256

'''Versión de fuerza bruta:

```BASIC256

Dim x(9)
x = {0.654682, 0.409382, 0.891663, 0.716629, 0.477721, 0.925092, 0.624291, 0.211332, 0.293786, 0.839186}
Dim y(9)
y = {0.925557, 0.619391, 0.888594, 0.996200, 0.946355, 0.818220, 0.142924, 0.221507, 0.691701, 0.728260}

minDist = 1^30
For i = 0 To 8
	For j = i+1 To 9
		dist = (x[i] - x[j])^2 + (y[i] - y[j])^2
		If dist < minDist Then minDist = dist : minDisti = i : minDistj = j
	Next j
Next i
Print "El par más cercano es "; minDisti; " y "; minDistj; " a una distancia de "; Sqr(minDist)
End

```

{{out}}

```txt

El par más cercano es 2 y 5 a una distancia de 0,077910191355

```



## BBC BASIC

To find the closest pair it is sufficient to compare the squared-distances,
it is not necessary to perform the square root for each pair!

```bbcbasic
      DIM x(9), y(9)

      FOR I% = 0 TO 9
        READ x(I%), y(I%)
      NEXT

      min = 1E30
      FOR I% = 0 TO 8
        FOR J% = I%+1 TO 9
          dsq = (x(I%) - x(J%))^2 + (y(I%) - y(J%))^2
          IF dsq < min min = dsq : mini% = I% : minj% = J%
        NEXT
      NEXT I%
      PRINT "Closest pair is ";mini% " and ";minj% " at distance "; SQR(min)
      END

      DATA  0.654682, 0.925557
      DATA  0.409382, 0.619391
      DATA  0.891663, 0.888594
      DATA  0.716629, 0.996200
      DATA  0.477721, 0.946355
      DATA  0.925092, 0.818220
      DATA  0.624291, 0.142924
      DATA  0.211332, 0.221507
      DATA  0.293786, 0.691701
      DATA  0.839186, 0.728260

```

{{out}}

```txt
Closest pair is 2 and 5 at distance 0.0779101913
```



## C

See [[Closest-pair problem/C]]


## C++


```cpp
/*
	Author: Kevin Bacon
	Date: 04/03/2014
	Task: Closest-pair problem
*/

#include <iostream>
#include <vector>
#include <utility>
#include <cmath>
#include <random>
#include <chrono>
#include <algorithm>
#include <iterator>

typedef std::pair<double, double> point_t;
typedef std::pair<point_t, point_t> points_t;

double distance_between(const point_t& a, const point_t& b) {
	return std::sqrt(std::pow(b.first - a.first, 2)
		+ std::pow(b.second - a.second, 2));
}

std::pair<double, points_t> find_closest_brute(const std::vector<point_t>& points) {
	if (points.size() < 2) {
		return { -1, { { 0, 0 }, { 0, 0 } } };
	}
	auto minDistance = std::abs(distance_between(points.at(0), points.at(1)));
	points_t minPoints = { points.at(0), points.at(1) };
	for (auto i = std::begin(points); i != (std::end(points) - 1); ++i) {
		for (auto j = i + 1; j < std::end(points); ++j) {
			auto newDistance = std::abs(distance_between(*i, *j));
			if (newDistance < minDistance) {
				minDistance = newDistance;
				minPoints.first = *i;
				minPoints.second = *j;
			}
		}
	}
	return { minDistance, minPoints };
}

std::pair<double, points_t> find_closest_optimized(const std::vector<point_t>& xP,
	const std::vector<point_t>& yP) {
	if (xP.size() <= 3) {
		return find_closest_brute(xP);
	}
	auto N = xP.size();
	auto xL = std::vector<point_t>();
	auto xR = std::vector<point_t>();
	std::copy(std::begin(xP), std::begin(xP) + (N / 2), std::back_inserter(xL));
	std::copy(std::begin(xP) + (N / 2), std::end(xP), std::back_inserter(xR));
	auto xM = xP.at((N-1) / 2).first;
	auto yL = std::vector<point_t>();
	auto yR = std::vector<point_t>();
	std::copy_if(std::begin(yP), std::end(yP), std::back_inserter(yL), [&xM](const point_t& p) {
		return p.first <= xM;
	});
	std::copy_if(std::begin(yP), std::end(yP), std::back_inserter(yR), [&xM](const point_t& p) {
		return p.first > xM;
	});
	auto p1 = find_closest_optimized(xL, yL);
	auto p2 = find_closest_optimized(xR, yR);
	auto minPair = (p1.first <= p2.first) ? p1 : p2;
	auto yS = std::vector<point_t>();
	std::copy_if(std::begin(yP), std::end(yP), std::back_inserter(yS), [&minPair, &xM](const point_t& p) {
		return std::abs(xM - p.first) < minPair.first;
	});
	auto result = minPair;
	for (auto i = std::begin(yS); i != (std::end(yS) - 1); ++i) {
		for (auto k = i + 1; k != std::end(yS) &&
		 ((k->second - i->second) < minPair.first); ++k) {
			auto newDistance = std::abs(distance_between(*k, *i));
			if (newDistance < result.first) {
				result = { newDistance, { *k, *i } };
			}
		}
	}
	return result;
}

void print_point(const point_t& point) {
	std::cout << "(" << point.first
		<< ", " << point.second
		<< ")";
}

int main(int argc, char * argv[]) {
	std::default_random_engine re(std::chrono::system_clock::to_time_t(
		std::chrono::system_clock::now()));
	std::uniform_real_distribution<double> urd(-500.0, 500.0);
	std::vector<point_t> points(100);
	std::generate(std::begin(points), std::end(points), [&urd, &re]() {
                return point_t { 1000 + urd(re), 1000 + urd(re) };
        });
	auto answer = find_closest_brute(points);
	std::sort(std::begin(points), std::end(points), [](const point_t& a, const point_t& b) {
		return a.first < b.first;
	});
	auto xP = points;
	std::sort(std::begin(points), std::end(points), [](const point_t& a, const point_t& b) {
		return a.second < b.second;
	});
	auto yP = points;
	std::cout << "Min distance (brute): " << answer.first << " ";
	print_point(answer.second.first);
	std::cout << ", ";
	print_point(answer.second.second);
	answer = find_closest_optimized(xP, yP);
	std::cout << "\nMin distance (optimized): " << answer.first << " ";
	print_point(answer.second.first);
	std::cout << ", ";
	print_point(answer.second.second);
	return 0;
}
```


{{out}}

```txt
Min distance (brute): 6.95886 (932.735, 1002.7), (939.216, 1000.17)
Min distance (optimized): 6.95886 (932.735, 1002.7), (939.216, 1000.17)
```



## Clojure



```clojure

(defn distance [[x1 y1] [x2 y2]]
  (let [dx (- x2 x1), dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn brute-force [points]
  (let [n (count points)]
    (when (< 1 n)
      (apply min-key first
             (for [i (range 0 (dec n)), :let [p1 (nth points i)],
                   j (range (inc i) n), :let [p2 (nth points j)]]
               [(distance p1 p2) p1 p2])))))

(defn combine [yS [dmin pmin1 pmin2]]
  (apply min-key first
         (conj (for [[p1 p2] (partition 2 1 yS)
                     :let [[_ py1] p1 [_ py2] p2]
                     :while (< (- py1 py2) dmin)]
                 [(distance p1 p2) p1 p2])
               [dmin pmin1 pmin2])))

(defn closest-pair
  ([points]
     (closest-pair
      (sort-by first points)
      (sort-by second points)))
  ([xP yP]
     (if (< (count xP) 4)
       (brute-force xP)
       (let [[xL xR] (partition-all (Math/ceil (/ (count xP) 2)) xP)
             [xm _] (last xL)
             {yL true yR false} (group-by (fn [[px _]] (<= px xm)) yP)
             dL&pairL (closest-pair xL yL)
             dR&pairR (closest-pair xR yR)
             [dmin pmin1 pmin2] (min-key first dL&pairL dR&pairR)
             {yS true} (group-by (fn [[px _]] (< (Math/abs (- xm px)) dmin)) yP)]
         (combine yS [dmin pmin1 pmin2])))))

```



## Common Lisp


Points are conses whose cars are x coördinates and whose cdrs are y coördinates.  This version includes the optimizations given in the [http://www.cs.mcgill.ca/~cs251/ClosestPair/ClosestPairDQ.html McGill description] of the algorithm.


```lisp
(defun point-distance (p1 p2)
  (destructuring-bind (x1 . y1) p1
    (destructuring-bind (x2 . y2) p2
      (let ((dx (- x2 x1)) (dy (- y2 y1)))
        (sqrt (+ (* dx dx) (* dy dy)))))))

(defun closest-pair-bf (points)
  (let ((pair (list (first points) (second points)))
        (dist (point-distance (first points) (second points))))
    (dolist (p1 points (values pair dist))
      (dolist (p2 points)
        (unless (eq p1 p2)
          (let ((pdist (point-distance p1 p2)))
            (when (< pdist dist)
              (setf (first pair) p1
                    (second pair) p2
                    dist pdist))))))))

(defun closest-pair (points)
  (labels
      ((cp (xp &aux (length (length xp)))
         (if (<= length 3)
           (multiple-value-bind (pair distance) (closest-pair-bf xp)
             (values pair distance (sort xp '< :key 'cdr)))
           (let* ((xr (nthcdr (1- (floor length 2)) xp))
                  (xm (/ (+ (caar xr) (caadr xr)) 2)))
             (psetf xr (rest xr)
                    (rest xr) '())
             (multiple-value-bind (lpair ldist yl) (cp xp)
               (multiple-value-bind (rpair rdist yr) (cp xr)
                 (multiple-value-bind (dist pair)
                     (if (< ldist rdist)
                       (values ldist lpair)
                       (values rdist rpair))
                   (let* ((all-ys (merge 'vector yl yr '< :key 'cdr))
                          (ys (remove-if #'(lambda (p)
                                             (> (abs (- (car p) xm)) dist))
                                         all-ys))
                          (ns (length ys)))
                     (dotimes (i ns)
                       (do ((k (1+ i) (1+ k)))
                           ((or (= k ns)
                                (> (- (cdr (aref ys k))
                                      (cdr (aref ys i)))
                                   dist)))
                         (let ((pd (point-distance (aref ys i)
                                                   (aref ys k))))
                           (when (< pd dist)
                             (setf dist pd
                                   (first pair) (aref ys i)
                                   (second pair) (aref ys k))))))
                     (values pair dist all-ys)))))))))
    (multiple-value-bind (pair distance)
        (cp (sort (copy-list points) '< :key 'car))
      (values pair distance))))
```


## C#
We provide a small helper class for distance comparisons:

```c#
class Segment
{
    public Segment(PointF p1, PointF p2)
    {
        P1 = p1;
        P2 = p2;
    }

    public readonly PointF P1;
    public readonly PointF P2;

    public float Length()
    {
        return (float)Math.Sqrt(LengthSquared());
    }

    public float LengthSquared()
    {
        return (P1.X - P2.X) * (P1.X - P2.X)
            + (P1.Y - P2.Y) * (P1.Y - P2.Y);
    }
}
```


Brute force:

```c#
Segment Closest_BruteForce(List<PointF> points)
{
    int n = points.Count;
    var result = Enumerable.Range( 0, n-1)
        .SelectMany( i => Enumerable.Range( i+1, n-(i+1) )
            .Select( j => new Segment( points[i], points[j] )))
            .OrderBy( seg => seg.LengthSquared())
            .First();

    return result;
}
```



And divide-and-conquer.

```c#

public static Segment MyClosestDivide(List<PointF> points)
{
   return MyClosestRec(points.OrderBy(p => p.X).ToList());
}

private static Segment MyClosestRec(List<PointF> pointsByX)
{
   int count = pointsByX.Count;
   if (count <= 4)
      return Closest_BruteForce(pointsByX);

   // left and right lists sorted by X, as order retained from full list
   var leftByX = pointsByX.Take(count/2).ToList();
   var leftResult = MyClosestRec(leftByX);

   var rightByX = pointsByX.Skip(count/2).ToList();
   var rightResult = MyClosestRec(rightByX);

   var result = rightResult.Length() < leftResult.Length() ? rightResult : leftResult;

   // There may be a shorter distance that crosses the divider
   // Thus, extract all the points within result.Length either side
   var midX = leftByX.Last().X;
   var bandWidth = result.Length();
   var inBandByX = pointsByX.Where(p => Math.Abs(midX - p.X) <= bandWidth);

   // Sort by Y, so we can efficiently check for closer pairs
   var inBandByY = inBandByX.OrderBy(p => p.Y).ToArray();

   int iLast = inBandByY.Length - 1;
   for (int i = 0; i < iLast; i++ )
   {
      var pLower = inBandByY[i];

      for (int j = i + 1; j <= iLast; j++)
      {
         var pUpper = inBandByY[j];

         // Comparing each point to successivly increasing Y values
         // Thus, can terminate as soon as deltaY is greater than best result
         if ((pUpper.Y - pLower.Y) >= result.Length())
            break;

         if (Segment.Length(pLower, pUpper) < result.Length())
            result = new Segment(pLower, pUpper);
      }
   }

   return result;
}

```


However, the difference in speed is still remarkable.

```c#
var randomizer = new Random(10);
var points = Enumerable.Range( 0, 10000).Select( i => new PointF( (float)randomizer.NextDouble(), (float)randomizer.NextDouble())).ToList();
Stopwatch sw = Stopwatch.StartNew();
var r1 = Closest_BruteForce(points);
sw.Stop();
Debugger.Log(1, "", string.Format("Time used (Brute force) (float): {0} ms", sw.Elapsed.TotalMilliseconds));
Stopwatch sw2 = Stopwatch.StartNew();
var result2 = Closest_Recursive(points);
sw2.Stop();
Debugger.Log(1, "", string.Format("Time used (Divide & Conquer): {0} ms",sw2.Elapsed.TotalMilliseconds));
Assert.Equal(r1.Length(), result2.Length());
```


{{out}}

```txt
Time used (Brute force) (float): 145731.8935 ms
Time used (Divide & Conquer): 1139.2111 ms
```


Non Linq Brute Force:

```c#

        Segment Closest_BruteForce(List<PointF> points)
        {
            Trace.Assert(points.Count >= 2);

            int count = points.Count;

            // Seed the result - doesn't matter what points are used
            // This just avoids having to do null checks in the main loop below
            var result = new Segment(points[0], points[1]);
            var bestLength = result.Length();

            for (int i = 0; i < count; i++)
                for (int j = i + 1; j < count; j++)
                    if (Segment.Length(points[i], points[j]) < bestLength)
                    {
                        result = new Segment(points[i], points[j]);
                        bestLength = result.Length();
                    }

            return result;
        }
```


Targeted Search: Much simpler than divide and conquer, and actually runs faster for the random points.  Key optimization is that if the distance along the X axis is greater than the best total length you already have, you can terminate the inner loop early.  However, as only sorts in the X direction, it degenerates into an N^2 algorithm if all the points have the same X.


```c#

        Segment Closest(List<PointF> points)
        {
            Trace.Assert(points.Count >= 2);

            int count = points.Count;
            points.Sort((lhs, rhs) => lhs.X.CompareTo(rhs.X));

            var result = new Segment(points[0], points[1]);
            var bestLength = result.Length();

            for (int i = 0; i < count; i++)
            {
                var from = points[i];

                for (int j = i + 1; j < count; j++)
                {
                    var to = points[j];

                    var dx = to.X - from.X;
                    if (dx >= bestLength)
                    {
                        break;
                    }

                    if (Segment.Length(from, to) < bestLength)
                    {
                        result = new Segment(from, to);
                        bestLength = result.Length();
                    }
                }
            }

            return result;
        }

```



## Crystal



## D


### Compact Versions


```d
import std.stdio, std.typecons, std.math, std.algorithm,
       std.random, std.traits, std.range, std.complex;

auto bruteForceClosestPair(T)(in T[] points) pure nothrow @nogc {
//  return pairwise(points.length.iota, points.length.iota)
//         .reduce!(min!((i, j) => abs(points[i] - points[j])));
  auto minD = Unqual!(typeof(T.re)).infinity;
  T minI, minJ;
  foreach (immutable i, const p1; points.dropBackOne)
    foreach (const p2; points[i + 1 .. $]) {
      immutable dist = abs(p1 - p2);
      if (dist < minD) {
        minD = dist;
        minI = p1;
        minJ = p2;
      }
    }
  return tuple(minD, minI, minJ);
}

auto closestPair(T)(T[] points) pure nothrow {
  static Tuple!(typeof(T.re), T, T) inner(in T[] xP, /*in*/ T[] yP)
  pure nothrow {
    if (xP.length <= 3)
      return xP.bruteForceClosestPair;
    const Pl = xP[0 .. $ / 2];
    const Pr = xP[$ / 2 .. $];
    immutable xDiv = Pl.back.re;
    auto Yr = yP.partition!(p => p.re <= xDiv);
    immutable dl_pairl = inner(Pl, yP[0 .. yP.length - Yr.length]);
    immutable dr_pairr = inner(Pr, Yr);
    immutable dm_pairm = dl_pairl[0]<dr_pairr[0] ? dl_pairl : dr_pairr;
    immutable dm = dm_pairm[0];
    const nextY = yP.filter!(p => abs(p.re - xDiv) < dm).array;

    if (nextY.length > 1) {
      auto minD = typeof(T.re).infinity;
      size_t minI, minJ;
      foreach (immutable i; 0 .. nextY.length - 1)
        foreach (immutable j; i + 1 .. min(i + 8, nextY.length)) {
          immutable double dist = abs(nextY[i] - nextY[j]);
          if (dist < minD) {
            minD = dist;
            minI = i;
            minJ = j;
          }
        }
      return dm <= minD ? dm_pairm :
                        typeof(return)(minD, nextY[minI], nextY[minJ]);
    } else
      return dm_pairm;
  }

  points.sort!q{ a.re < b.re };
  const xP = points.dup;
  points.sort!q{ a.im < b.im };
  return inner(xP, points);
}

void main() {
  alias C = complex;
  auto pts = [C(5,9), C(9,3), C(2), C(8,4), C(7,4), C(9,10), C(1,9),
              C(8,2), C(0,10), C(9,6)];
  pts.writeln;
  writeln("bruteForceClosestPair: ", pts.bruteForceClosestPair);
  writeln("          closestPair: ", pts.closestPair);

  rndGen.seed = 1;
  Complex!double[10_000] points;
  foreach (ref p; points)
    p = C(uniform(0.0, 1000.0) + uniform(0.0, 1000.0));
  writeln("bruteForceClosestPair: ", points.bruteForceClosestPair);
  writeln("          closestPair: ", points.closestPair);
}
```

{{out}}

```txt
[5+9i, 9+3i, 2+0i, 8+4i, 7+4i, 9+10i, 1+9i, 8+2i, 0+10i, 9+6i]
bruteForceClosestPair: Tuple!(double, Complex!double, Complex!double)(1, 8+4i, 7+4i)
          closestPair: Tuple!(double, Complex!double, Complex!double)(1, 7+4i, 8+4i)
bruteForceClosestPair: Tuple!(double, Complex!double, Complex!double)(1.76951e-05, 1040.2+0i, 1040.2+0i)
          closestPair: Tuple!(double, Complex!double, Complex!double)(1.76951e-05, 1040.2+0i, 1040.2+0i)
```

About 1.87 seconds run-time for data generation and brute force version, and about 0.03 seconds for data generation and divide & conquer (10_000 points in both cases) with ldc2 compiler.

===Faster Brute-force Version===

```d
import std.stdio, std.random, std.math, std.typecons, std.complex,
       std.traits;

Nullable!(Tuple!(size_t, size_t))
bfClosestPair2(T)(in Complex!T[] points) pure nothrow @nogc {
    auto minD = Unqual!(typeof(points[0].re)).infinity;
    if (points.length < 2)
        return typeof(return)();

    size_t minI, minJ;
    foreach (immutable i; 0 .. points.length - 1)
        foreach (immutable j; i + 1 .. points.length) {
            auto dist = (points[i].re - points[j].re) ^^ 2;
            if (dist < minD) {
                dist += (points[i].im - points[j].im) ^^ 2;
                if (dist < minD) {
                    minD = dist;
                    minI = i;
                    minJ = j;
                }
            }
        }

    return typeof(return)(tuple(minI, minJ));
}

void main() {
    alias C = Complex!double;
    auto rng = 31415.Xorshift;
    C[10_000] pts;
    foreach (ref p; pts)
        p = C(uniform(0.0, 1000.0, rng), uniform(0.0, 1000.0, rng));

    immutable ij = pts.bfClosestPair2;
    if (ij.isNull)
        return;
    writefln("Closest pair: Distance: %f  p1, p2: %f, %f",
             abs(pts[ij[0]] - pts[ij[1]]), pts[ij[0]], pts[ij[1]]);
}
```

{{out}}

```txt
Closest pair: Distance: 0.019212  p1, p2: 9.74223+119.419i, 9.72306+119.418i
```

About 0.12 seconds run-time for brute-force version 2 (10_000 points) with with LDC2 compiler.


## Elixir


```elixir
defmodule Closest_pair do
  # brute-force algorithm:
  def bruteForce([p0,p1|_] = points), do: bf_loop(points, {distance(p0, p1), {p0, p1}})

  defp bf_loop([_], acc), do: acc
  defp bf_loop([h|t], acc), do: bf_loop(t, bf_loop(h, t, acc))

  defp bf_loop(_, [], acc), do: acc
  defp bf_loop(p0, [p1|t], {minD, minP}) do
    dist = distance(p0, p1)
    if dist < minD, do: bf_loop(p0, t, {dist, {p0, p1}}),
                  else: bf_loop(p0, t, {minD, minP})
  end

  defp distance({p0x,p0y}, {p1x,p1y}) do
    :math.sqrt( (p1x - p0x) * (p1x - p0x) + (p1y - p0y) * (p1y - p0y) )
  end

  # recursive divide&conquer approach:
  def recursive(points) do
    recursive(Enum.sort(points), Enum.sort_by(points, fn {_x,y} -> y end))
  end

  def recursive(xP, _yP) when length(xP) <= 3, do: bruteForce(xP)
  def recursive(xP, yP) do
    {xL, xR} = Enum.split(xP, div(length(xP), 2))
    {xm, _} = hd(xR)
    {yL, yR} = Enum.partition(yP, fn {x,_} -> x < xm end)
    {dL, pairL} = recursive(xL, yL)
    {dR, pairR} = recursive(xR, yR)
    {dmin, pairMin} = if dL<dR, do: {dL, pairL}, else: {dR, pairR}
    yS = Enum.filter(yP, fn {x,_} -> abs(xm - x) < dmin end)
    merge(yS, {dmin, pairMin})
  end

  defp merge([_], acc), do: acc
  defp merge([h|t], acc), do: merge(t, merge_loop(h, t, acc))

  defp merge_loop(_, [], acc), do: acc
  defp merge_loop(p0, [p1|_], {dmin,_}=acc) when dmin <= elem(p1,1) - elem(p0,1), do: acc
  defp merge_loop(p0, [p1|t], {dmin, pair}) do
    dist = distance(p0, p1)
    if dist < dmin, do: merge_loop(p0, t, {dist, {p0, p1}}),
                  else: merge_loop(p0, t, {dmin, pair})
  end
end

data = [{0.654682, 0.925557}, {0.409382, 0.619391}, {0.891663, 0.888594}, {0.716629, 0.996200},
        {0.477721, 0.946355}, {0.925092, 0.818220}, {0.624291, 0.142924}, {0.211332, 0.221507},
        {0.293786, 0.691701}, {0.839186, 0.728260}]

IO.inspect Closest_pair.bruteForce(data)
IO.inspect Closest_pair.recursive(data)

data2 = for _ <- 1..5000, do: {:rand.uniform, :rand.uniform}
IO.puts "\nBrute-force:"
IO.inspect :timer.tc(fn -> Closest_pair.bruteForce(data2) end)
IO.puts "Recursive divide&conquer:"
IO.inspect :timer.tc(fn -> Closest_pair.recursive(data2) end)
```


{{out}}

```txt

{0.07791019135517516, {{0.891663, 0.888594}, {0.925092, 0.81822}}}
{0.07791019135517516, {{0.891663, 0.888594}, {0.925092, 0.81822}}}

Brute-force:
{9579000,
 {2.068674444452469e-4,
  {{0.9397601102440695, 0.020420581980209674},
   {0.9399398976079764, 0.020522908141823986}}}}
Recursive divide&conquer:
{109000,
 {2.068674444452469e-4,
  {{0.9397601102440695, 0.020420581980209674},
   {0.9399398976079764, 0.020522908141823986}}}}

```


=={{header|F Sharp|F#}}==
Brute force:

```fsharp

let closest_pairs (xys: Point []) =
  let n = xys.Length
  seq { for i in 0..n-2 do
          for j in i+1..n-1 do
            yield xys.[i], xys.[j] }
  |> Seq.minBy (fun (p0, p1) -> (p1 - p0).LengthSquared)

```

For example:

```fsharp

closest_pairs
  [|Point(0.0, 0.0); Point(1.0, 0.0); Point (2.0, 2.0)|]

```

gives:

```fsharp

(0,0, 1,0)

```


Divide And Conquer:


```fsharp


open System;
open System.Drawing;
open System.Diagnostics;

let Length (seg : (PointF * PointF) option) =
    match seg with
    | None -> System.Single.MaxValue
    | Some(line) ->
        let f = fst line
        let t = snd line

        let dx = f.X - t.X
        let dy = f.Y - t.Y
        sqrt (dx*dx + dy*dy)


let Shortest a b =
    if Length(a) < Length(b) then
        a
    else
        b


let rec ClosestBoundY from maxY (ptsByY : PointF list) =
    match ptsByY with
    | [] -> None
    | hd :: tl ->
        if hd.Y > maxY then
            None
        else
            let toHd = Some(from, hd)
            let bestToRest = ClosestBoundY from maxY tl
            Shortest toHd bestToRest


let rec ClosestWithinRange ptsByY maxDy =
    match ptsByY with
    | [] -> None
    | hd :: tl ->
        let fromHd = ClosestBoundY hd (hd.Y + maxDy) tl
        let fromRest = ClosestWithinRange tl  maxDy
        Shortest fromHd fromRest


// Cuts pts half way through it's length
// Order is not maintained in result lists however
let Halve pts =
    let rec ShiftToFirst first second n =
        match (n, second) with
        | 0, _ -> (first, second)   // finished the split, so return current state
        | _, [] -> (first, [])      // not enough items, so first takes the whole original list
        | n, hd::tl -> ShiftToFirst (hd :: first) tl (n-1)  // shift 1st item from second to first, then recurse with n-1

    let n = (List.length pts) / 2
    ShiftToFirst [] pts n


let rec ClosestPair (pts : PointF list) =
    if List.length pts < 2 then
        None
    else
        let ptsByX = pts |> List.sortBy(fun(p) -> p.X)

        let (left, right) = Halve ptsByX
        let leftResult = ClosestPair left
        let rightResult = ClosestPair right

        let bestInHalf = Shortest  leftResult rightResult
        let bestLength = Length bestInHalf

        let divideX = List.head(right).X
        let inBand = pts |> List.filter(fun(p) -> Math.Abs(p.X - divideX) < bestLength)

        let byY = inBand |> List.sortBy(fun(p) -> p.Y)
        let bestCross = ClosestWithinRange byY bestLength
        Shortest bestInHalf bestCross


let GeneratePoints n =
    let rand = new Random()
    [1..n] |> List.map(fun(i) -> new PointF(float32(rand.NextDouble()), float32(rand.NextDouble())))

let timer = Stopwatch.StartNew()
let pts = GeneratePoints (50 * 1000)
let closest = ClosestPair pts
let takenMs = timer.ElapsedMilliseconds

printfn "Closest Pair '%A'.  Distance %f" closest (Length closest)
printfn "Took %d [ms]" takenMs

```



## Fantom


(Based on the Ruby example.)


```fantom

class Point
{
  Float x
  Float y

  // create a random point
  new make (Float x := Float.random * 10, Float y := Float.random * 10)
  {
    this.x = x
    this.y = y
  }

  Float distance (Point p)
  {
    ((x-p.x)*(x-p.x) + (y-p.y)*(y-p.y)).sqrt
  }

  override Str toStr () { "($x, $y)" }
}

class Main
{
  // use brute force approach
  static Point[] findClosestPair1 (Point[] points)
  {
    if (points.size < 2) return points  // list too small
    Point[] closestPair := [points[0], points[1]]
    Float closestDistance := points[0].distance(points[1])

    (1..<points.size).each |Int i|
    {
      ((i+1)..<points.size).each |Int j|
      {
        Float trydistance := points[i].distance(points[j])
        if (trydistance < closestDistance)
        {
          closestPair = [points[i], points[j]]
          closestDistance = trydistance
        }
      }
    }

    return closestPair
  }

  // use recursive divide-and-conquer approach
  static Point[] findClosestPair2 (Point[] points)
  {
    if (points.size <= 3) return findClosestPair1(points)
    points.sort |Point a, Point b -> Int| { a.x <=> b.x }
    bestLeft := findClosestPair2 (points[0..(points.size/2)])
    bestRight := findClosestPair2 (points[(points.size/2)..-1])

    Float minDistance
    Point[] closePoints := [,]
    if (bestLeft[0].distance(bestLeft[1]) < bestRight[0].distance(bestRight[1]))
    {
      minDistance = bestLeft[0].distance(bestLeft[1])
      closePoints = bestLeft
    }
    else
    {
      minDistance = bestRight[0].distance(bestRight[1])
      closePoints = bestRight
    }
    yPoints := points.findAll |Point p -> Bool|
    {
      (points.last.x - p.x).abs < minDistance
    }.sort |Point a, Point b -> Int| { a.y <=> b.y }

    closestPair := [,]
    closestDist := Float.posInf

    for (Int i := 0; i < yPoints.size - 1; ++i)
    {
      for (Int j := (i+1); j < yPoints.size; ++j)
      {
        if ((yPoints[j].y - yPoints[i].y) >= minDistance)
        {
          break
        }
        else
        {
          dist := yPoints[i].distance (yPoints[j])
          if (dist < closestDist)
          {
            closestDist = dist
            closestPair = [yPoints[i], yPoints[j]]
          }
        }
      }
    }
    if (closestDist < minDistance)
      return closestPair
    else
      return closePoints
  }

  public static Void main (Str[] args)
  {
    Int numPoints := 10 // default value, in case a number not given on command line
    if ((args.size > 0) && (args[0].toInt(10, false) != null))
    {
      numPoints = args[0].toInt(10, false)
    }

    Point[] points := [,]
    numPoints.times { points.add (Point()) }

    Int t1 := Duration.now.toMillis
    echo (findClosestPair1(points.dup))
    Int t2 := Duration.now.toMillis
    echo ("Time taken: ${(t2-t1)}ms")
    echo (findClosestPair2(points.dup))
    Int t3 := Duration.now.toMillis
    echo ("Time taken: ${(t3-t2)}ms")
  }
}

```


{{out}}

```txt

$ fan closestPoints 1000
[(1.4542885676006445, 8.238581003965352), (1.4528464044751888, 8.234724407229772)]
Time taken: 88ms
[(1.4528464044751888, 8.234724407229772), (1.4542885676006445, 8.238581003965352)]
Time taken: 80ms
$ fan closestPoints 10000
[(3.454790171891945, 5.307252398266497), (3.4540208686702245, 5.308350223433488)]
Time taken: 6248ms
[(3.454790171891945, 5.307252398266497), (3.4540208686702245, 5.308350223433488)]
Time taken: 228ms

```



## Fortran

See [[Closest pair problem/Fortran]]


## FreeBASIC

'''Versión de fuerza bruta:

```freebasic

Dim As Integer i, j
Dim As Double minDist = 1^30
Dim As Double x(9), y(9), dist, mini, minj

Data  0.654682, 0.925557
Data  0.409382, 0.619391
Data  0.891663, 0.888594
Data  0.716629, 0.996200
Data  0.477721, 0.946355
Data  0.925092, 0.818220
Data  0.624291, 0.142924
Data  0.211332, 0.221507
Data  0.293786, 0.691701
Data  0.839186, 0.728260

For i = 0 To 9
    Read x(i), y(i)
Next i

For i = 0 To 8
    For j = i+1 To 9
        dist = (x(i) - x(j))^2 + (y(i) - y(j))^2
        If dist < minDist Then
            minDist = dist
            mini = i
            minj = j
        End If
    Next j
Next i

Print "El par más cercano es "; mini; " y "; minj; " a una distancia de "; Sqr(minDist)
End

```

{{out}}

```txt

El par más cercano es 2 y 5 a una distancia de 0.07791019135517516

```




## Go

'''Brute force'''

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

type xy struct {
    x, y float64
}

const n = 1000
const scale = 100.

func d(p1, p2 xy) float64 {
    return math.Hypot(p2.x-p1.x, p2.y-p1.y)
}

func main() {
    rand.Seed(time.Now().Unix())
    points := make([]xy, n)
    for i := range points {
        points[i] = xy{rand.Float64() * scale, rand.Float64() * scale}
    }
    p1, p2 := closestPair(points)
    fmt.Println(p1, p2)
    fmt.Println("distance:", d(p1, p2))
}

func closestPair(points []xy) (p1, p2 xy) {
    if len(points) < 2 {
        panic("at least two points expected")
    }
    min := 2 * scale
    for i, q1 := range points[:len(points)-1] {
        for _, q2 := range points[i+1:] {
            if dq := d(q1, q2); dq < min {
                p1, p2 = q1, q2
                min = dq
            }
        }
    }
    return
}
```

'''O(n)'''

```go
// implementation following algorithm described in
// http://www.cs.umd.edu/~samir/grant/cp.pdf
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

// number of points to search for closest pair
const n = 1e6

// size of bounding box for points.
// x and y will be random with uniform distribution in the range [0,scale).
const scale = 100.

// point struct
type xy struct {
    x, y float64 // coordinates
    key  int64   // an annotation used in the algorithm
}

func d(p1, p2 xy) float64 {
    return math.Hypot(p2.x-p1.x, p2.y-p1.y)
}

func main() {
    rand.Seed(time.Now().Unix())
    points := make([]xy, n)
    for i := range points {
        points[i] = xy{rand.Float64() * scale, rand.Float64() * scale, 0}
    }
    p1, p2 := closestPair(points)
    fmt.Println(p1, p2)
    fmt.Println("distance:", d(p1, p2))
}

func closestPair(s []xy) (p1, p2 xy) {
    if len(s) < 2 {
        panic("2 points required")
    }
    var dxi float64
    // step 0
    for s1, i := s, 1; ; i++ {
        // step 1: compute min distance to a random point
        // (for the case of random data, it's enough to just try
        // to pick a different point)
        rp := i % len(s1)
        xi := s1[rp]
        dxi = 2 * scale
        for p, xn := range s1 {
            if p != rp {
                if dq := d(xi, xn); dq < dxi {
                    dxi = dq
                }
            }
        }

        // step 2: filter
        invB := 3 / dxi             // b is size of a mesh cell
        mx := int64(scale*invB) + 1 // mx is number of cells along a side
        // construct map as a histogram:
        // key is index into mesh.  value is count of points in cell
        hm := map[int64]int{}
        for ip, p := range s1 {
            key := int64(p.x*invB)*mx + int64(p.y*invB)
            s1[ip].key = key
            hm[key]++
        }
        // construct s2 = s1 less the points without neighbors
        s2 := make([]xy, 0, len(s1))
        nx := []int64{-mx - 1, -mx, -mx + 1, -1, 0, 1, mx - 1, mx, mx + 1}
        for i, p := range s1 {
            nn := 0
            for _, ofs := range nx {
                nn += hm[p.key+ofs]
                if nn > 1 {
                    s2 = append(s2, s1[i])
                    break
                }
            }
        }

        // step 3: done?
        if len(s2) == 0 {
            break
        }
        s1 = s2
    }
    // step 4: compute answer from approximation
    invB := 1 / dxi
    mx := int64(scale*invB) + 1
    hm := map[int64][]int{}
    for i, p := range s {
        key := int64(p.x*invB)*mx + int64(p.y*invB)
        s[i].key = key
        hm[key] = append(hm[key], i)
    }
    nx := []int64{-mx - 1, -mx, -mx + 1, -1, 0, 1, mx - 1, mx, mx + 1}
    var min = scale * 2
    for ip, p := range s {
        for _, ofs := range nx {
            for _, iq := range hm[p.key+ofs] {
                if ip != iq {
                    if d1 := d(p, s[iq]); d1 < min {
                        min = d1
                        p1, p2 = p, s[iq]
                    }
                }
            }
        }
    }
    return p1, p2
}
```



## Groovy

Point class:

```groovy
class Point {
    final Number x, y
    Point(Number x = 0, Number y = 0) { this.x = x; this.y = y }
    Number distance(Point that) { ((this.x - that.x)**2 + (this.y - that.y)**2)**0.5 }
    String toString() { "{x:${x}, y:${y}}" }
}
```


Brute force solution. Incorporates X-only and Y-only pre-checks in two places to cut down on the square root calculations:

```groovy
def bruteClosest(Collection pointCol) {
    assert pointCol
    List l = pointCol
    int n = l.size()
    assert n > 1
    if (n == 2) return [distance:l[0].distance(l[1]), points:[l[0],l[1]]]
    def answer = [distance: Double.POSITIVE_INFINITY]
    (0..<(n-1)).each { i ->
        ((i+1)..<n).findAll { j ->
            (l[i].x - l[j].x).abs() < answer.distance &&
            (l[i].y - l[j].y).abs() < answer.distance
        }.each { j ->
            if ((l[i].x - l[j].x).abs() < answer.distance &&
                (l[i].y - l[j].y).abs() < answer.distance) {
                def dist = l[i].distance(l[j])
                if (dist < answer.distance) {
                    answer = [distance:dist, points:[l[i],l[j]]]
                }
            }
        }
    }
    answer
}
```


Elegant (divide-and-conquer reduction) solution. Incorporates X-only and Y-only pre-checks in two places (four if you count the inclusion of the brute force solution) to cut down on the square root calculations:

```groovy
def elegantClosest(Collection pointCol) {
    assert pointCol
    List xList = (pointCol as List).sort { it.x }
    List yList = xList.clone().sort { it.y }
    reductionClosest(xList, xList)
}

def reductionClosest(List xPoints, List yPoints) {
//    assert xPoints && yPoints
//    assert (xPoints as Set) == (yPoints as Set)
    int n = xPoints.size()
    if (n < 10) return bruteClosest(xPoints)

    int nMid = Math.ceil(n/2)
    List xLeft = xPoints[0..<nMid]
    List xRight = xPoints[nMid..<n]
    Number xMid = xLeft[-1].x
    List yLeft = yPoints.findAll { it.x <= xMid }
    List yRight = yPoints.findAll { it.x > xMid }
    if (xRight[0].x == xMid) {
        yLeft = xLeft.collect{ it }.sort { it.y }
        yRight = xRight.collect{ it }.sort { it.y }
    }

    Map aLeft = reductionClosest(xLeft, yLeft)
    Map aRight = reductionClosest(xRight, yRight)
    Map aMin = aRight.distance < aLeft.distance ? aRight : aLeft
    List yMid = yPoints.findAll { (xMid - it.x).abs() < aMin.distance }
    int nyMid = yMid.size()
    if (nyMid < 2) return aMin

    Map answer = aMin
    (0..<(nyMid-1)).each { i ->
        ((i+1)..<nyMid).findAll { j ->
            (yMid[j].x - yMid[i].x).abs() < aMin.distance &&
            (yMid[j].y - yMid[i].y).abs() < aMin.distance &&
            yMid[j].distance(yMid[i]) < aMin.distance
        }.each { k ->
            if ((yMid[k].x - yMid[i].x).abs() < answer.distance && (yMid[k].y - yMid[i].y).abs() < answer.distance) {
                def ikDist = yMid[i].distance(yMid[k])
                if ( ikDist < answer.distance) {
                    answer = [distance:ikDist, points:[yMid[i],yMid[k]]]
                }
            }
        }
    }
    answer
}
```


Benchmark/Test:

```groovy
def random = new Random()

(1..4).each {
def point10 = (0..<(10**it)).collect { new Point(random.nextInt(1000001) - 500000,random.nextInt(1000001) - 500000) }

def startE = System.currentTimeMillis()
def closestE = elegantClosest(point10)
def elapsedE = System.currentTimeMillis() - startE
println """
${10**it} POINTS
-----------------------------------------
Elegant reduction:
elapsed: ${elapsedE/1000} s
closest: ${closestE}
"""


def startB = System.currentTimeMillis()
def closestB = bruteClosest(point10)
def elapsedB = System.currentTimeMillis() - startB
println """Brute force:
elapsed: ${elapsedB/1000} s
closest: ${closestB}

Speedup ratio (B/E): ${elapsedB/elapsedE}

### ===================================

"""
}
```


Results:

```txt
10 POINTS
-----------------------------------------
Elegant reduction:
elapsed: 0.019 s
closest: [distance:85758.5249173515, points:[{x:310073, y:-27339}, {x:382387, y:18761}]]

Brute force:
elapsed: 0.001 s
closest: [distance:85758.5249173515, points:[{x:310073, y:-27339}, {x:382387, y:18761}]]

Speedup ratio (B/E): 0.0526315789

### ===================================



100 POINTS
-----------------------------------------
Elegant reduction:
elapsed: 0.019 s
closest: [distance:3166.229934796271, points:[{x:-343735, y:-244394}, {x:-341099, y:-246148}]]

Brute force:
elapsed: 0.027 s
closest: [distance:3166.229934796271, points:[{x:-343735, y:-244394}, {x:-341099, y:-246148}]]

Speedup ratio (B/E): 1.4210526316

### ===================================



1000 POINTS
-----------------------------------------
Elegant reduction:
elapsed: 0.241 s
closest: [distance:374.22586762542215, points:[{x:411817, y:-83016}, {x:412038, y:-82714}]]

Brute force:
elapsed: 0.618 s
closest: [distance:374.22586762542215, points:[{x:411817, y:-83016}, {x:412038, y:-82714}]]

Speedup ratio (B/E): 2.5643153527

### ===================================



10000 POINTS
-----------------------------------------
Elegant reduction:
elapsed: 1.957 s
closest: [distance:79.00632886041473, points:[{x:187928, y:-452338}, {x:187929, y:-452259}]]

Brute force:
elapsed: 51.567 s
closest: [distance:79.00632886041473, points:[{x:187928, y:-452338}, {x:187929, y:-452259}]]

Speedup ratio (B/E): 26.3500255493

### ===================================

```



## Haskell

BF solution:

```Haskell
import Data.List (minimumBy, tails, unfoldr, foldl1') --'

import System.Random (newStdGen, randomRs)

import Control.Arrow ((&&&))

import Data.Ord (comparing)

vecLeng [[a, b], [p, q]] = sqrt $ (a - p) ^ 2 + (b - q) ^ 2

findClosestPair =
  foldl1'' ((minimumBy (comparing vecLeng) .) . (. return) . (:)) .
  concatMap (\(x:xs) -> map ((x :) . return) xs) . init . tails

testCP = do
  g <- newStdGen
  let pts :: [[Double]]
      pts = take 1000 . unfoldr (Just . splitAt 2) $ randomRs (-1, 1) g
  print . (id &&& vecLeng) . findClosestPair $ pts

main = testCP

foldl1'' = foldl1'

```

{{out}}

```Haskell
*Main> testCP
([[0.8347201880148426,0.40774840545089647],[0.8348731214261784,0.4087113189531284]],9.749825850154334e-4)
(4.02 secs, 488869056 bytes)
```


=={{header|Icon}} and {{header|Unicon}}==
This is a brute force solution.
It combines reading the points with computing the closest pair seen so far.

```unicon
record point(x,y)

procedure main()
    minDist := 0
    minPair := &null
    every (points := [],p1 := readPoint()) do {
        if *points == 1 then minDist := dSquared(p1,points[1])
        every minDist >=:= dSquared(p1,p2 := !points) do minPair := [p1,p2]
        push(points, p1)
        }

    if \minPair then {
        write("(",minPair[1].x,",",minPair[1].y,") -> ",
              "(",minPair[2].x,",",minPair[2].y,")")
        }
    else write("One or fewer points!")
end

procedure readPoint()  # Skips lines that don't have two numbers on them
    suspend !&input ? point(numeric(tab(upto(', '))), numeric((move(1),tab(0))))
end

procedure dSquared(p1,p2)    # Compute the square of the distance
    return (p2.x-p1.x)^2 + (p2.y-p1.y)^2  # (sufficient for closeness)
end
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 PROGRAM "Closestp.bas"
110 NUMERIC X(1 TO 10),Y(1 TO 10)
120 FOR I=1 TO 10
130   READ X(I),Y(I)
140   PRINT X(I),Y(I)
150 NEXT
160 LET MN=INF
170 FOR I=1 TO 9
180   FOR J=I+1 TO 10
190     LET DSQ=(X(I)-X(J))^2+(Y(I)-Y(J))^2
200     IF DSQ<MN THEN LET MN=DSQ:LET MINI=I:LET MINJ=J
210   NEXT
220 NEXT
230 PRINT "Closest pair is (";X(MINI);",";Y(MINI);") and (";X(MINJ);",";Y(MINJ);")":PRINT "at distance";SQR(MN)
240 DATA 0.654682,0.925557
250 DATA 0.409382,0.619391
260 DATA 0.891663,0.888594
270 DATA 0.716629,0.996200
280 DATA 0.477721,0.946355
290 DATA 0.925092,0.818220
300 DATA 0.624291,0.142924
310 DATA 0.211332,0.221507
320 DATA 0.293786,0.691701
330 DATA 0.839186,0.728260
```



## J

Solution of the simpler (brute-force) problem:

```j
vecl   =:  +/"1&.:*:                  NB. length of each vector
dist   =: <@:vecl@:({: -"1 }:)\               NB. calculate all distances among vectors
minpair=: ({~ > {.@($ #: I.@,)@:= <./@;)dist  NB. find one pair of the closest points
closestpairbf =: (; vecl@:-/)@minpair         NB. the pair and their distance
```

Examples of use:

```j
   ]pts=:10 2 ?@$ 0
0.654682 0.925557
0.409382 0.619391
0.891663 0.888594
0.716629   0.9962
0.477721 0.946355
0.925092  0.81822
0.624291 0.142924
0.211332 0.221507
0.293786 0.691701
0.839186  0.72826

   closestpairbf pts
+-----------------+---------+
|0.891663 0.888594|0.0779104|
|0.925092  0.81822|         |
+-----------------+---------+
```

The program also works for higher dimensional vectors:

```j
   ]pts=:10 4 ?@$ 0
0.559164 0.482993     0.876  0.429769
0.217911 0.729463   0.97227  0.132175
0.479206 0.169165  0.495302  0.362738
0.316673 0.797519  0.745821 0.0598321
0.662585 0.726389  0.658895  0.653457
0.965094 0.664519  0.084712   0.20671
0.840877 0.591713  0.630206   0.99119
0.221416 0.114238 0.0991282  0.174741
0.946262 0.505672  0.776017  0.307362
0.262482 0.540054  0.707342  0.465234

   closestpairbf pts
+------------------------------------+--------+
|0.217911 0.729463  0.97227  0.132175|0.708555|
|0.316673 0.797519 0.745821 0.0598321|        |
+------------------------------------+--------+
```



## Java


Both the brute-force and the divide-and-conquer methods are implemented.

'''Code:'''

```Java
import java.util.*;

public class ClosestPair
{
  public static class Point
  {
    public final double x;
    public final double y;

    public Point(double x, double y)
    {
      this.x = x;
      this.y = y;
    }

    public String toString()
    {  return "(" + x + ", " + y + ")";  }
  }

  public static class Pair
  {
    public Point point1 = null;
    public Point point2 = null;
    public double distance = 0.0;

    public Pair()
    {  }

    public Pair(Point point1, Point point2)
    {
      this.point1 = point1;
      this.point2 = point2;
      calcDistance();
    }

    public void update(Point point1, Point point2, double distance)
    {
      this.point1 = point1;
      this.point2 = point2;
      this.distance = distance;
    }

    public void calcDistance()
    {  this.distance = distance(point1, point2);  }

    public String toString()
    {  return point1 + "-" + point2 + " : " + distance;  }
  }

  public static double distance(Point p1, Point p2)
  {
    double xdist = p2.x - p1.x;
    double ydist = p2.y - p1.y;
    return Math.hypot(xdist, ydist);
  }

  public static Pair bruteForce(List<? extends Point> points)
  {
    int numPoints = points.size();
    if (numPoints < 2)
      return null;
    Pair pair = new Pair(points.get(0), points.get(1));
    if (numPoints > 2)
    {
      for (int i = 0; i < numPoints - 1; i++)
      {
        Point point1 = points.get(i);
        for (int j = i + 1; j < numPoints; j++)
        {
          Point point2 = points.get(j);
          double distance = distance(point1, point2);
          if (distance < pair.distance)
            pair.update(point1, point2, distance);
        }
      }
    }
    return pair;
  }

  public static void sortByX(List<? extends Point> points)
  {
    Collections.sort(points, new Comparator<Point>() {
        public int compare(Point point1, Point point2)
        {
          if (point1.x < point2.x)
            return -1;
          if (point1.x > point2.x)
            return 1;
          return 0;
        }
      }
    );
  }

  public static void sortByY(List<? extends Point> points)
  {
    Collections.sort(points, new Comparator<Point>() {
        public int compare(Point point1, Point point2)
        {
          if (point1.y < point2.y)
            return -1;
          if (point1.y > point2.y)
            return 1;
          return 0;
        }
      }
    );
  }

  public static Pair divideAndConquer(List<? extends Point> points)
  {
    List<Point> pointsSortedByX = new ArrayList<Point>(points);
    sortByX(pointsSortedByX);
    List<Point> pointsSortedByY = new ArrayList<Point>(points);
    sortByY(pointsSortedByY);
    return divideAndConquer(pointsSortedByX, pointsSortedByY);
  }

  private static Pair divideAndConquer(List<? extends Point> pointsSortedByX, List<? extends Point> pointsSortedByY)
  {
    int numPoints = pointsSortedByX.size();
    if (numPoints <= 3)
      return bruteForce(pointsSortedByX);

    int dividingIndex = numPoints >>> 1;
    List<? extends Point> leftOfCenter = pointsSortedByX.subList(0, dividingIndex);
    List<? extends Point> rightOfCenter = pointsSortedByX.subList(dividingIndex, numPoints);

    List<Point> tempList = new ArrayList<Point>(leftOfCenter);
    sortByY(tempList);
    Pair closestPair = divideAndConquer(leftOfCenter, tempList);

    tempList.clear();
    tempList.addAll(rightOfCenter);
    sortByY(tempList);
    Pair closestPairRight = divideAndConquer(rightOfCenter, tempList);

    if (closestPairRight.distance < closestPair.distance)
      closestPair = closestPairRight;

    tempList.clear();
    double shortestDistance =closestPair.distance;
    double centerX = rightOfCenter.get(0).x;
    for (Point point : pointsSortedByY)
      if (Math.abs(centerX - point.x) < shortestDistance)
        tempList.add(point);

    for (int i = 0; i < tempList.size() - 1; i++)
    {
      Point point1 = tempList.get(i);
      for (int j = i + 1; j < tempList.size(); j++)
      {
        Point point2 = tempList.get(j);
        if ((point2.y - point1.y) >= shortestDistance)
          break;
        double distance = distance(point1, point2);
        if (distance < closestPair.distance)
        {
          closestPair.update(point1, point2, distance);
          shortestDistance = distance;
        }
      }
    }
    return closestPair;
  }

  public static void main(String[] args)
  {
    int numPoints = (args.length == 0) ? 1000 : Integer.parseInt(args[0]);
    List<Point> points = new ArrayList<Point>();
    Random r = new Random();
    for (int i = 0; i < numPoints; i++)
      points.add(new Point(r.nextDouble(), r.nextDouble()));
    System.out.println("Generated " + numPoints + " random points");
    long startTime = System.currentTimeMillis();
    Pair bruteForceClosestPair = bruteForce(points);
    long elapsedTime = System.currentTimeMillis() - startTime;
    System.out.println("Brute force (" + elapsedTime + " ms): " + bruteForceClosestPair);
    startTime = System.currentTimeMillis();
    Pair dqClosestPair = divideAndConquer(points);
    elapsedTime = System.currentTimeMillis() - startTime;
    System.out.println("Divide and conquer (" + elapsedTime + " ms): " + dqClosestPair);
    if (bruteForceClosestPair.distance != dqClosestPair.distance)
      System.out.println("MISMATCH");
  }
}
```


{{out}}

```txt
java ClosestPair 10000
Generated 10000 random points
Brute force (1594 ms): (0.9246533850872104, 0.098709007587097)-(0.924591196030625, 0.09862206991823985) : 1.0689077146927108E-4
Divide and conquer (250 ms): (0.924591196030625, 0.09862206991823985)-(0.9246533850872104, 0.098709007587097) : 1.0689077146927108E-4
```



## JavaScript

Using bruteforce algorithm, the ''bruteforceClosestPair'' method below expects an array of objects with x- and y-members set to numbers, and returns an object containing the members ''distance'' and ''points''.


```javascript
function distance(p1, p2) {
  var dx = Math.abs(p1.x - p2.x);
  var dy = Math.abs(p1.y - p2.y);
  return Math.sqrt(dx*dx + dy*dy);
}

function bruteforceClosestPair(arr) {
  if (arr.length < 2) {
    return Infinity;
  } else {
    var minDist = distance(arr[0], arr[1]);
    var minPoints = arr.slice(0, 2);

    for (var i=0; i<arr.length-1; i++) {
      for (var j=i+1; j<arr.length; j++) {
        if (distance(arr[i], arr[j]) < minDist) {
          minDist = distance(arr[i], arr[j]);
          minPoints = [ arr[i], arr[j] ];
        }
      }
    }
    return {
      distance: minDist,
      points: minPoints
    };
  }
}
```


divide-and-conquer method:

```javascript


var Point = function(x, y) {
	this.x = x;
	this.y = y;
};
Point.prototype.getX = function() {
	return this.x;
};
Point.prototype.getY = function() {
	return this.y;
};

var mergeSort = function mergeSort(points, comp) {
	if(points.length < 2) return points;


	var n = points.length,
		i = 0,
		j = 0,
		leftN = Math.floor(n / 2),
		rightN = leftN;


	var leftPart = mergeSort( points.slice(0, leftN), comp),
		rightPart = mergeSort( points.slice(rightN), comp );

	var sortedPart = [];

	while((i < leftPart.length) && (j < rightPart.length)) {
		if(comp(leftPart[i], rightPart[j]) < 0) {
			sortedPart.push(leftPart[i]);
			i += 1;
		}
		else {
			sortedPart.push(rightPart[j]);
			j += 1;
		}
	}
	while(i < leftPart.length) {
		sortedPart.push(leftPart[i]);
		i += 1;
	}
	while(j < rightPart.length) {
		sortedPart.push(rightPart[j]);
		j += 1;
	}
	return sortedPart;
};

var closestPair = function _closestPair(Px, Py) {
	if(Px.length < 2) return { distance: Infinity, pair: [ new Point(0, 0), new Point(0, 0) ] };
	if(Px.length < 3) {
		//find euclid distance
		var d = Math.sqrt( Math.pow(Math.abs(Px[1].x - Px[0].x), 2) + Math.pow(Math.abs(Px[1].y - Px[0].y), 2) );
		return {
			distance: d,
			pair: [ Px[0], Px[1] ]
		};
	}

	var	n = Px.length,
		leftN = Math.floor(n / 2),
		rightN = leftN;

	var Xl = Px.slice(0, leftN),
		Xr = Px.slice(rightN),
		Xm = Xl[leftN - 1],
		Yl = [],
		Yr = [];
	//separate Py
	for(var i = 0; i < Py.length; i += 1) {
		if(Py[i].x <= Xm.x)
			Yl.push(Py[i]);
		else
			Yr.push(Py[i]);
	}

	var dLeft = _closestPair(Xl, Yl),
		dRight = _closestPair(Xr, Yr);

	var minDelta = dLeft.distance,
		closestPair = dLeft.pair;
	if(dLeft.distance > dRight.distance) {
		minDelta = dRight.distance;
		closestPair = dRight.pair;
	}


	//filter points around Xm within delta (minDelta)
	var closeY = [];
	for(i = 0; i < Py.length; i += 1) {
		if(Math.abs(Py[i].x - Xm.x) < minDelta) closeY.push(Py[i]);
	}
	//find min within delta. 8 steps max
	for(i = 0; i < closeY.length; i += 1) {
		for(var j = i + 1; j < Math.min( (i + 8), closeY.length ); j += 1) {
			var d = Math.sqrt( Math.pow(Math.abs(closeY[j].x - closeY[i].x), 2) + Math.pow(Math.abs(closeY[j].y - closeY[i].y), 2) );
			if(d < minDelta) {
				minDelta = d;
				closestPair = [ closeY[i], closeY[j] ]
			}
		}
	}

	return {
		distance: minDelta,
		pair: closestPair
	};
};


var points = [
	new Point(0.748501, 4.09624),
	new Point(3.00302, 5.26164),
	new Point(3.61878,  9.52232),
	new Point(7.46911,  4.71611),
	new Point(5.7819,   2.69367),
	new Point(2.34709,  8.74782),
	new Point(2.87169,  5.97774),
	new Point(6.33101,  0.463131),
	new Point(7.46489,  4.6268),
	new Point(1.45428,  0.087596)
];

var sortX = function (a, b) { return (a.x < b.x) ? -1 : ((a.x > b.x) ? 1 : 0); }
var sortY = function (a, b) { return (a.y < b.y) ? -1 : ((a.y > b.y) ? 1 : 0); }

var Px = mergeSort(points, sortX);
var Py = mergeSort(points, sortY);

console.log(JSON.stringify(closestPair(Px, Py))) // {"distance":0.0894096443343775,"pair":[{"x":7.46489,"y":4.6268},{"x":7.46911,"y":4.71611}]}

var points2 = [new Point(37100, 13118), new Point(37134, 1963), new Point(37181, 2008), new Point(37276, 21611), new Point(37307, 9320)];

Px = mergeSort(points2, sortX);
Py = mergeSort(points2, sortY);

console.log(JSON.stringify(closestPair(Px, Py))); // {"distance":65.06919393998976,"pair":[{"x":37134,"y":1963},{"x":37181,"y":2008}]}


```



## jq

{{works with|jq|1.4}}
The solution presented here is essentially a direct translation into jq of the pseudo-code presented in the task description,
but "closest_pair" is added so that any list of [x,y] points can be presented, and extra lines are added to ensure that xL and yL have the same lengths.

'''Infrastructure''':

```jq
# This definition of "until" is included in recent versions (> 1.4) of jq
# Emit the first input that satisfied the condition
def until(cond; next):
  def _until:
    if cond then . else (next|_until) end;
  _until;

# Euclidean 2d distance
def dist(x;y):
  [x[0] - y[0], x[1] - y[1]] | map(.*.) | add | sqrt;
```


```jq

# P is an array of points, [x,y].
# Emit the solution in the form [dist, [P1, P2]]
def bruteForceClosestPair(P):
  (P|length) as $length
  | if $length < 2 then null
    else
      reduce range(0; $length-1) as $i
        ( null;
          reduce range($i+1; $length) as $j
            (.;
             dist(P[$i]; P[$j]) as $d
             | if . == null or $d < .[0] then [$d, [ P[$i], P[$j] ] ] else . end ) )
    end;

def closest_pair:

  def abs: if . < 0 then -. else . end;
  def ceil: floor as $floor
    | if . == $floor then $floor else $floor + 1 end;

  # xP is an array [P(1), .. P(N)] sorted by x coordinate, and
  # yP is an array [P(1), .. P(N)] sorted by y coordinate (ascending order).
  # if N <= 3 then return closest points of xP using the brute-force algorithm.
  def closestPair(xP; yP):
    if xP|length <= 3 then bruteForceClosestPair(xP)
    else
      ((xP|length)/2|ceil) as $N
      | xP[0:$N]  as $xL
      | xP[$N:]   as $xR
      | xP[$N-1][0] as $xm                        # middle
      | (yP | map(select(.[0] <= $xm ))) as $yL0  # might be too long
      | (yP | map(select(.[0] >  $xm ))) as $yR0  # might be too short
      | (if $yL0|length == $N then $yL0 else $yL0[0:$N] end) as $yL
      | (if $yL0|length == $N then $yR0 else $yL0[$N:] + $yR0 end) as $yR
      | closestPair($xL; $yL) as $pairL           #  [dL, pairL]
      | closestPair($xR; $yR) as $pairR           #  [dR, pairR]
      | (if $pairL[0] < $pairR[0] then $pairL else $pairR end) as $pair # [ dmin, pairMin]
      | (yP | map(select( (($xm - .[0])|abs) < $pair[0]))) as $yS
      | ($yS | length) as $nS
      | $pair[0] as $dmin
      | reduce range(0; $nS - 1) as $i
          ( [0, $pair];                         # state: [k, [d, [P1,P2]]]
            .[0] = $i + 1
            | until( .[0] as $k | $k >= $nS or ($yS[$k][1] - $yS[$i][1]) >= $dmin;
                       .[0] as $k
                       | dist($yS[$k]; $yS[$i]) as $d
                       | if $d < .[1][0]
                         then [$k+1, [ $d, [$yS[$k], $yS[$i]]]]
                         else .[0] += 1
                         end) )
      | .[1]
    end;
  closestPair( sort_by(.[0]); sort_by(.[1])) ;
```

'''Example from the Mathematica section''':

```jq
def data:
 [[0.748501, 4.09624],
  [3.00302,  5.26164],
  [3.61878,  9.52232],
  [7.46911,  4.71611],
  [5.7819,   2.69367],
  [2.34709,  8.74782],
  [2.87169,  5.97774],
  [6.33101,  0.463131],
  [7.46489,  4.6268],
  [1.45428,  0.087596] ];

data | closest_pair
```

{{Out}}
 $jq -M -c -n -f closest_pair.jq
 [0.0894096443343775,[[7.46489,4.6268],[7.46911,4.71611]]]


## Julia

{{works with|Julia|0.6}}
Brute-force algorithm:

```julia
function closestpair(P::Vector{Vector{T}}) where T <: Number
    N = length(P)
    if N < 2 return (Inf, ()) end
    mindst = norm(P[1] - P[2])
    minpts = (P[1], P[2])
    for i in 1:N-1, j in i+1:N
        tmpdst = norm(P[i] - P[j])
        if tmpdst < mindst
            mindst = tmpdst
            minpts = (P[i], P[j])
        end
    end
    return mindst, minpts
end

closestpair([[0, -0.3], [1., 1.], [1.5, 2], [2, 2], [3, 3]])
```



## Kotlin


```scala
// version 1.1.2

typealias Point = Pair<Double, Double>

fun distance(p1: Point, p2: Point) = Math.hypot(p1.first- p2.first, p1.second - p2.second)

fun bruteForceClosestPair(p: List<Point>): Pair<Double, Pair<Point, Point>> {
    val n = p.size
    if (n < 2) throw IllegalArgumentException("Must be at least two points")
    var minPoints = p[0] to p[1]
    var minDistance = distance(p[0], p[1])
    for (i in 0 until n - 1)
        for (j in i + 1 until n) {
            val dist = distance(p[i], p[j])
            if (dist < minDistance) {
                minDistance = dist
                minPoints = p[i] to p[j]
            }
        }
    return minDistance to Pair(minPoints.first, minPoints.second)
}

fun optimizedClosestPair(xP: List<Point>, yP: List<Point>): Pair<Double, Pair<Point, Point>> {
    val n = xP.size
    if (n <= 3) return bruteForceClosestPair(xP)
    val xL = xP.take(n / 2)
    val xR = xP.drop(n / 2)
    val xm = xP[n / 2 - 1].first
    val yL = yP.filter { it.first <= xm }
    val yR = yP.filter { it.first >  xm }
    val (dL, pairL) = optimizedClosestPair(xL, yL)
    val (dR, pairR) = optimizedClosestPair(xR, yR)
    var dmin = dR
    var pairMin = pairR
    if (dL < dR) {
        dmin = dL
        pairMin = pairL
    }
    val yS = yP.filter { Math.abs(xm - it.first) < dmin }
    val nS = yS.size
    var closest = dmin
    var closestPair = pairMin
    for (i in 0 until nS - 1) {
        var k = i + 1
        while (k < nS && (yS[k].second - yS[i].second < dmin)) {
            val dist = distance(yS[k], yS[i])
            if (dist < closest) {
                closest = dist
                closestPair = Pair(yS[k], yS[i])
            }
            k++
        }
    }
    return closest to closestPair
}


fun main(args: Array<String>) {
    val points = listOf(
        listOf(
            5.0 to  9.0, 9.0 to 3.0,  2.0 to 0.0, 8.0 to  4.0, 7.0 to 4.0,
            9.0 to 10.0, 1.0 to 9.0,  8.0 to 2.0, 0.0 to 10.0, 9.0 to 6.0
        ),
        listOf(
            0.654682 to 0.925557, 0.409382 to 0.619391, 0.891663 to 0.888594,
            0.716629 to 0.996200, 0.477721 to 0.946355, 0.925092 to 0.818220,
            0.624291 to 0.142924, 0.211332 to 0.221507, 0.293786 to 0.691701,
            0.839186 to 0.728260
        )
    )
    for (p in points) {
        val (dist, pair) = bruteForceClosestPair(p)
        println("Closest pair (brute force) is ${pair.first} and ${pair.second}, distance $dist")
        val xP = p.sortedBy { it.first }
        val yP = p.sortedBy { it.second }
        val (dist2, pair2) = optimizedClosestPair(xP, yP)
        println("Closest pair (optimized)   is ${pair2.first} and ${pair2.second}, distance $dist2\n")
    }
}
```


{{out}}

```txt

Closest pair (brute force) is (8.0, 4.0) and (7.0, 4.0), distance 1.0
Closest pair (optimized)   is (7.0, 4.0) and (8.0, 4.0), distance 1.0

Closest pair (brute force) is (0.891663, 0.888594) and (0.925092, 0.81822), distance 0.07791019135517516
Closest pair (optimized)   is (0.891663, 0.888594) and (0.925092, 0.81822), distance 0.07791019135517516

```



## Liberty BASIC

NB array terms can not be READ directly.

```lb

N =10

dim x( N), y( N)

firstPt  =0
secondPt =0

for i =1 to N
    read f: x( i) =f
    read f: y( i) =f
next i

minDistance  =1E6

for i =1 to N -1
    for j =i +1 to N
      dxSq =( x( i) -x( j))^2
      dySq =( y( i) -y( j))^2
      D    =abs( ( dxSq +dySq)^0.5)
      if D <minDistance then
        minDistance =D
        firstPt     =i
        secondPt    =j
      end if
    next j
next i

print "Distance ="; minDistance; " between ( "; x( firstPt); ", "; y( firstPt); ") and ( "; x( secondPt); ", "; y( secondPt); ")"

end

data  0.654682, 0.925557
data  0.409382, 0.619391
data  0.891663, 0.888594
data  0.716629, 0.996200
data  0.477721, 0.946355
data  0.925092, 0.818220
data  0.624291, 0.142924
data  0.211332, 0.221507
data  0.293786, 0.691701
data  0.839186,  0.72826


```

 Distance =0.77910191e-1 between ( 0.891663, 0.888594) and ( 0.925092, 0.81822)



## Maple


```Maple
ClosestPair := module()

local
    ModuleApply := proc(L::list,$)
    local Lx, Ly, out;
        Ly := sort(L, 'key'=(i->i[2]), 'output'='permutation');
        Lx := sort(L, 'key'=(i->i[1]), 'output'='permutation');
        out := Recurse(L, Lx, Ly, 1, numelems(L));
        return sqrt(out[1]), out[2];
    end proc; # ModuleApply

local
    BruteForce := proc(L, Lx, r1:=1, r2:=numelems(L), $)
    local d, p, n, i, j;
        d := infinity;
        for i from r1 to r2-1 do
            for j from i+1 to r2 do
                n := dist( L[Lx[i]],  L[Lx[j]] );
                if n < d then
                    d := n;
                    p := [ L[Lx[i]], L[Lx[j]] ];
                end if;
            end do; # j
        end do; # i
        return (d, p);
    end proc; # BruteForce

local dist := (p, q)->(( (p[1]-q[1])^2+(p[2]-q[2])^2 ));

local Recurse := proc(L, Lx, Ly, r1, r2)
    local m, xm, rDist, rPair, lDist, lPair, minDist, minPair, S, i, j, Lyr, Lyl;

    if r2-r1 <= 3 then
        return BruteForce(L, Lx, r1, r2);
    end if;

    m := ceil((r2-r1)/2)+r1;
    xm := (L[Lx[m]][1] + L[Lx[m-1]][1])/2;

    (Lyr, Lyl) := selectremove( i->L[i][1] < xm, Ly);

    (rDist, rPair) := thisproc(L, Lx, Lyr, r1, m-1);
    (lDist, lPair) := thisproc(L, Lx, Lyl, m, r2);

    if rDist < lDist then
        minDist := rDist;
        minPair := rPair;
    else
        minDist := lDist;
        minPair := lPair;
    end if;

    S := [ seq( `if`(abs(xm - L[i][1])^2< minDist, L[i], NULL ), i in Ly ) ];

    for i from 1 to nops(S)-1 do
        for j from i+1 to nops(S) do
            if abs( S[i][2] - S[j][2] )^2 >= minDist then
                break;
            elif dist(S[i], S[j]) < minDist then
                minDist := dist(S[i], S[j]);
                minPair := [S[i], S[j]];
            end if;
        end do;
    end do;

    return (minDist, minPair);

    end proc; #Recurse

end module; #ClosestPair
```


{{out}}
```Maple

> L := RandomTools:-Generate(list(list(float(range=0..1),2),512)):
> ClosestPair(L);
 0.002576770304, [[0.4265584800, 0.7443097852], [0.4240649736, 0.7449595321]]

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
nearestPair[data_] :=
 Block[{pos, dist = N[Outer[EuclideanDistance, data, data, 1]]},
  pos = Position[dist, Min[DeleteCases[Flatten[dist], 0.]]];
  data[[pos[[1]]]]]
```


{{out}}

```txt
nearestPair[{{0.748501, 4.09624}, {3.00302, 5.26164}, {3.61878,
  9.52232}, {7.46911, 4.71611}, {5.7819, 2.69367}, {2.34709,
  8.74782}, {2.87169, 5.97774}, {6.33101, 0.463131}, {7.46489,
  4.6268}, {1.45428, 0.087596}}]

{{7.46911, 4.71611}, {7.46489, 4.6268}}
```



## MATLAB


This solution is an almost direct translation of the above pseudo-code into MATLAB.

```MATLAB
function [closest,closestpair] = closestPair(xP,yP)

    N = numel(xP);

    if(N <= 3)

        %Brute force closestpair
        if(N < 2)
            closest = +Inf;
            closestpair = {};
        else
            closest = norm(xP{1}-xP{2});
            closestpair = {xP{1},xP{2}};

            for i = ( 1:N-1 )
                for j = ( (i+1):N )
                    if ( norm(xP{i} - xP{j}) < closest )
                        closest = norm(xP{i}-xP{j});
                        closestpair = {xP{i},xP{j}};
                    end %if
                end %for
            end %for
        end %if (N < 2)
    else

        halfN = ceil(N/2);

        xL = { xP{1:halfN} };
        xR = { xP{halfN+1:N} };
        xm = xP{halfN}(1);

        %cellfun( @(p)le(p(1),xm),yP ) is the same as { p ∈ yP : px ≤ xm }
        yLIndicies = cellfun( @(p)le(p(1),xm),yP );

        yL = { yP{yLIndicies} };
        yR = { yP{~yLIndicies} };

        [dL,pairL] = closestPair(xL,yL);
        [dR,pairR] = closestPair(xR,yR);

        if dL < dR
            dmin = dL;
            pairMin = pairL;
        else
            dmin = dR;
            pairMin = pairR;
        end

        %cellfun( @(p)lt(norm(xm-p(1)),dmin),yP ) is the same as
        %{ p ∈ yP : |xm - px| < dmin }
        yS = {yP{ cellfun( @(p)lt(norm(xm-p(1)),dmin),yP ) }};
        nS = numel(yS);

        closest = dmin;
        closestpair = pairMin;

        for i = (1:nS-1)
            k = i+1;

            while( (k<=nS) && (yS{k}(2)-yS{i}(2) < dmin) )

                if norm(yS{k}-yS{i}) < closest
                    closest = norm(yS{k}-yS{i});
                    closestpair = {yS{k},yS{i}};
                end

                k = k+1;
            end %while
        end %for
    end %if (N <= 3)
end %closestPair
```


{{out}}

```MATLAB
[distance,pair]=closestPair({[0 -.3],[1 1],[1.5 2],[2 2],[3 3]},{[0 -.3],[1 1],[1.5 2],[2 2],[3 3]})

distance =

   0.500000000000000


pair =

    [1x2 double]    [1x2 double] %The pair is [1.5 2] and [2 2] which is correct
```



## Microsoft Small Basic


```smallbasic
' Closest Pair Problem
s="0.654682,0.925557,0.409382,0.619391,0.891663,0.888594,0.716629,0.996200,0.477721,0.946355,0.925092,0.818220,0.624291,0.142924,0.211332,0.221507,0.293786,0.691701,0.839186,0.728260,"
  i=0
  While s<>""
    i=i+1
    For j=1 To 2
      k=Text.GetIndexOf(s,",")
      ss=Text.GetSubText(s,1,k-1)
      s=Text.GetSubTextToEnd(s,k+1)
      pxy[i][j]=ss
    EndFor
  EndWhile
  n=i
  i=1
  j=2
  dd=Math.Power(pxy[i][1]-pxy[j][1],2)+Math.Power(pxy[i][2]-pxy[j][2],2)
  ddmin=dd
  ii=i
  jj=j
  For i=1 To n
    For j=1 To n
      dd=Math.Power(pxy[i][1]-pxy[j][1],2)+Math.Power(pxy[i][2]-pxy[j][2],2)
      If dd>0 Then
        If dd<ddmin Then
          ddmin=dd
          ii=i
          jj=j
        EndIf
      EndIf
    EndFor
  EndFor
  sqrt1=ddmin
  sqrt2=ddmin/2
  For i=1 To 20
    If sqrt1=sqrt2 Then
      Goto exitfor
    EndIf
    sqrt1=sqrt2
    sqrt2=(sqrt1+(ddmin/sqrt1))/2
  EndFor
exitfor:
  TextWindow.WriteLine("the minimum distance "+sqrt2)
  TextWindow.WriteLine("is between the points:")
  TextWindow.WriteLine("  ["+pxy[ii][1]+","+pxy[ii][2]+"] and")
  TextWindow.WriteLine("  ["+pxy[jj][1]+","+pxy[jj][2]+"]")
```

{{out}}

```txt

the minimum distance 0,0779101913551750943201426138
is between the points:
  [0.891663,0.888594] and
  [0.925092,0.818220]

```


=={{header|Objective-C}}==
See [[Closest-pair problem/Objective-C]]


## OCaml



```ocaml


type point = { x : float; y : float }


let cmpPointX (a : point) (b : point) = compare a.x b.x
let cmpPointY (a : point) (b : point) = compare a.y b.y


let distSqrd (seg : (point * point) option) =
  match seg with
  | None -> max_float
  | Some(line) ->
    let a = fst line in
    let b = snd line in

    let dx = a.x -. b.x in
    let dy = a.y -. b.y in

    dx*.dx +. dy*.dy


let dist seg =
  sqrt (distSqrd seg)


let shortest l1 l2 =
  if distSqrd l1 < distSqrd l2 then
    l1
  else
    l2


let halve l =
  let n = List.length l in
  BatList.split_at (n/2) l


let rec closestBoundY from maxY (ptsByY : point list) =
  match ptsByY with
  | [] -> None
  | hd :: tl ->
    if hd.y > maxY then
      None
    else
      let toHd = Some(from, hd) in
      let bestToRest = closestBoundY from maxY tl in
      shortest toHd bestToRest


let rec closestInRange ptsByY maxDy =
  match ptsByY with
  | [] -> None
  | hd :: tl ->
    let fromHd = closestBoundY hd (hd.y +. maxDy) tl in
    let fromRest = closestInRange tl maxDy in
    shortest fromHd fromRest


let rec closestPairByX (ptsByX : point list) =
   if List.length ptsByX < 2 then
       None
   else
       let (left, right) = halve ptsByX in
       let leftResult = closestPairByX left in
       let rightResult = closestPairByX right in

       let bestInHalf = shortest  leftResult rightResult in
       let bestLength = dist bestInHalf in

       let divideX = (List.hd right).x in
       let inBand = List.filter(fun(p) -> abs_float(p.x -. divideX) < bestLength) ptsByX in

       let byY = List.sort cmpPointY inBand in
       let bestCross = closestInRange byY bestLength in
       shortest bestInHalf bestCross


let closestPair pts =
  let ptsByX = List.sort cmpPointX pts in
  closestPairByX ptsByX


let parsePoint str =
  let sep = Str.regexp_string "," in
  let tokens = Str.split sep str in
  let xStr = List.nth tokens 0 in
  let yStr = List.nth tokens 1 in

  let xVal = (float_of_string xStr) in
  let yVal = (float_of_string yStr) in

  { x = xVal; y = yVal }


let loadPoints filename =
  let ic = open_in filename in
  let result = ref [] in
  try
    while true do
      let s = input_line ic in
      if s <> "" then
        let p = parsePoint s in
        result := p :: !result;
    done;
    !result
  with End_of_file ->
    close_in ic;
    !result
;;

let loaded = (loadPoints "Points.txt") in
let start = Sys.time() in
let c = closestPair loaded in
let taken = Sys.time() -. start in
Printf.printf "Took %f [s]\n" taken;

match c with
| None -> Printf.printf "No closest pair\n"
| Some(seg) ->
  let a = fst seg in
  let b = snd seg in

  Printf.printf "(%f, %f) (%f, %f) Dist %f\n" a.x a.y b.x b.y (dist c)


```



## Oz

Translation of pseudocode:

```oz
declare
  fun {Distance X1#Y1 X2#Y2}
     {Sqrt {Pow X2-X1 2.0} + {Pow Y2-Y1 2.0}}
  end

  %% brute force
  fun {BFClosestPair Points=P1|P2|_}
     Ps = {List.toTuple unit Points} %% for efficient random access
     N = {Width Ps}
     MinDist = {NewCell {Distance P1 P2}}
     MinPoints = {NewCell P1#P2}
  in
     for I in 1..N-1 do
        for J in I+1..N do
           IJDist = {Distance Ps.I Ps.J}
        in
           if IJDist < @MinDist then
              MinDist := IJDist
              MinPoints := Ps.I#Ps.J
           end
        end
     end
     @MinPoints
  end

  %% divide and conquer
  fun {ClosestPair Points}
     case {ClosestPair2
           {Sort Points {LessThanBy X}}
           {Sort Points {LessThanBy Y}}}
     of Distance#Pair then
        Pair
     end
  end

  %% XP: points sorted by X, YP: sorted by Y
  %% returns a pair Distance#Pair
  fun {ClosestPair2 XP YP}
     N = {Length XP} = {Length YP}
  in
     if N =< 3 then
        P = {BFClosestPair XP}
     in
        {Distance P.1 P.2}#P
     else
        XL XR
        {List.takeDrop XP (N div 2) ?XL ?XR}
        XM = {Nth XP (N div 2)}.X
        YL YR
        {List.partition YP fun {$ P} P.X =< XM end ?YL ?YR}
        DL#PairL = {ClosestPair2 XL YL}
        DR#PairR = {ClosestPair2 XR YR}
        DMin#PairMin = if DL < DR then DL#PairL else DR#PairR end
        YSList = {Filter YP fun {$ P} {Abs XM-P.X} < DMin end}
        YS = {List.toTuple unit YSList} %% for efficient random access
        NS = {Width YS}
        Closest = {NewCell DMin}
        ClosestPair = {NewCell PairMin}
     in
        for I in 1..NS-1 do
           for K in I+1..NS while:YS.K.Y - YS.I.Y < DMin do
              DistKI = {Distance YS.K YS.I}
           in
              if DistKI < @Closest then
                 Closest := DistKI
                 ClosestPair := YS.K#YS.I
              end
           end
        end
        @Closest#@ClosestPair
     end
  end

  %% To access components when points are represented as pairs
  X = 1
  Y = 2

  %% returns a less-than predicate that accesses feature F
  fun {LessThanBy F}
     fun {$ A B}
        A.F < B.F
     end
  end

  fun {Random Min Max}
     Min +
     {Int.toFloat {OS.rand}} * (Max-Min)
     / {Int.toFloat {OS.randLimits _}}
  end

  fun {RandomPoint}
     {Random 0.0 100.0}#{Random 0.0 100.0}
  end

  Points = {MakeList 5}
in
  {ForAll Points RandomPoint}
  {Show Points}
  {Show {ClosestPair Points}}
```



## PARI/GP

Naive quadratic solution.

```parigp
closestPair(v)={
  my(r=norml2(v[1]-v[2]),at=[1,2]);
  for(a=1,#v-1,
    for(b=a+1,#v,
      if(norml2(v[a]-v[b])<r,
        at=[a,b];
        r=norml2(v[a]-v[b])
      )
    )
  );
  [v[at[1]],v[at[2]]]
};
```


## Pascal

Brute force only calc square of distance, like AWK etc...
As fast as [[Closest-pair_problem#Faster_Brute-force_Version | D ]] .

```pascal
program closestPoints;
{$IFDEF FPC}
   {$MODE Delphi}
{$ENDIF}
const
  PointCnt = 10000;//31623;
type
  TdblPoint = Record
               ptX,
               ptY : double;
              end;
  tPtLst =  array of TdblPoint;

  tMinDIstIdx  = record
                   md1,
                   md2 : NativeInt;
                 end;

function ClosPointBruteForce(var  ptl :tPtLst):tMinDIstIdx;
Var
  i,j,k : NativeInt;
  mindst2,dst2: double; //square of distance, no need to sqrt
  p0,p1 : ^TdblPoint;   //using pointer, since calc of ptl[?] takes much time
Begin
  i := Low(ptl);
  j := High(ptl);
  result.md1 := i;result.md2 := j;
  mindst2 := sqr(ptl[i].ptX-ptl[j].ptX)+sqr(ptl[i].ptY-ptl[j].ptY);
  repeat
    p0 := @ptl[i];
    p1 := p0; inc(p1);
    For k := i+1 to j do
    Begin
      dst2:= sqr(p0^.ptX-p1^.ptX)+sqr(p0^.ptY-p1^.ptY);
      IF mindst2 > dst2  then
      Begin
        mindst2 :=  dst2;
        result.md1 := i;
        result.md2 := k;
      end;
      inc(p1);
    end;
    inc(i);
  until i = j;
end;

var
  PointLst :tPtLst;
  cloPt : tMinDIstIdx;
  i : NativeInt;
Begin
  randomize;
  setlength(PointLst,PointCnt);
  For i := 0 to PointCnt-1 do
    with PointLst[i] do
    Begin
      ptX := random;
      ptY := random;
    end;
  cloPt:=  ClosPointBruteForce(PointLst) ;
  i := cloPt.md1;
  Writeln('P[',i:4,']= x: ',PointLst[i].ptX:0:8,
                     ' y: ',PointLst[i].ptY:0:8);
  i := cloPt.md2;
  Writeln('P[',i:4,']= x: ',PointLst[i].ptX:0:8,
                     ' y: ',PointLst[i].ptY:0:8);
end.
```
{{Out}}
```txt
PointCnt = 10000
//without randomize always same results
//32-Bit
P[ 324]= x: 0.26211815 y: 0.45851455
P[3391]= x: 0.26217852 y: 0.45849116
real  0m0.114s  //fpc 3.1.1 32 Bit -O4 -MDelphi..cpu i4330 3.5 Ghz
//64-Bit doubles the speed   comp switch -O2 ..-O4 same timings
P[ 324]= x: 0.26211815 y: 0.45851455
P[3391]= x: 0.26217852 y: 0.45849116
real    0m0.059s //fpc 3.1.1 64 Bit -O4 -MDelphi..cpu i4330 3.5 Ghz

//with randomize
P[  47]= x: 0.12408823 y: 0.04501338
P[9429]= x: 0.12399629 y: 0.04496700
//32-Bit
PointCnt = { 10000*sqrt(10) } 31623;-> real 0m1.112s 10x times runtime
```



## Perl

The divide & conquer technique is about 100x faster than the brute-force algorithm.

```perl
#! /usr/bin/perl
use strict;
use POSIX qw(ceil);

sub dist
{
    my ( $a, $b) = @_;
    return sqrt( ($a->[0] - $b->[0])**2 +
                 ($a->[1] - $b->[1])**2 );
}

sub closest_pair_simple
{
    my $ra = shift;
    my @arr = @$ra;
    my $inf = 1e600;
    return $inf if scalar(@arr) < 2;
    my ( $a, $b, $d ) = ($arr[0], $arr[1], dist($arr[0], $arr[1]));
    while( @arr ) {
	my $p = pop @arr;
	foreach my $l (@arr) {
	    my $t = dist($p, $l);
	    ($a, $b, $d) = ($p, $l, $t) if $t < $d;
	}
    }
    return ($a, $b, $d);
}

sub closest_pair
{
    my $r = shift;
    my @ax = sort { $a->[0] <=> $b->[0] } @$r;
    my @ay = sort { $a->[1] <=> $b->[1] } @$r;
    return closest_pair_real(\@ax, \@ay);
}

sub closest_pair_real
{
    my ($rx, $ry) = @_;
    my @xP = @$rx;
    my @yP = @$ry;
    my $N = @xP;
    return closest_pair_simple($rx) if scalar(@xP) <= 3;

    my $inf = 1e600;
    my $midx = ceil($N/2)-1;

    my @PL = @xP[0 .. $midx];
    my @PR = @xP[$midx+1 .. $N-1];

    my $xm = ${$xP[$midx]}[0];

    my @yR = ();
    my @yL = ();
    foreach my $p (@yP) {
	if ( ${$p}[0] <= $xm ) {
	    push @yR, $p;
	} else {
	    push @yL, $p;
	}
    }

    my ($al, $bl, $dL) = closest_pair_real(\@PL, \@yR);
    my ($ar, $br, $dR) = closest_pair_real(\@PR, \@yL);

    my ($m1, $m2, $dmin) = ($al, $bl, $dL);
    ($m1, $m2, $dmin) = ($ar, $br, $dR) if $dR < $dL;

    my @yS = ();
    foreach my $p (@yP) {
	push @yS, $p if abs($xm - ${$p}[0]) < $dmin;
    }

    if ( @yS ) {
	my ( $w1, $w2, $closest ) = ($m1, $m2, $dmin);
	foreach my $i (0 .. ($#yS - 1)) {

	    my $k = $i + 1;
	    while ( ($k <= $#yS) && ( (${$yS[$k]}[1] - ${$yS[$i]}[1]) < $dmin) ) {
		my $d = dist($yS[$k], $yS[$i]);
		($w1, $w2, $closest) = ($yS[$k], $yS[$i], $d) if $d < $closest;
		$k++;
	    }

	}
	return ($w1, $w2, $closest);

    } else {
	return ($m1, $m2, $dmin);
    }
}



my @points = ();
my $N = 5000;

foreach my $i (1..$N) {
    push @points, [rand(20)-10.0, rand(20)-10.0];
}


my ($a, $b, $d) = closest_pair_simple(\@points);
print "$d\n";

my ($a1, $b1, $d1) = closest_pair(\@points);
print "$d1\n";
```



## Perl 6


{{trans|Perl 5}}

We avoid taking square roots in the slow method because the squares are just as comparable.
(This doesn't always work in the fast method because of distance assumptions in the algorithm.)

```perl6
sub MAIN ($N = 5000) {
    my @points = (^$N).map: { [rand * 20 - 10, rand * 20 - 10] }

    my ($af, $bf, $df) = closest_pair(@points);
    say "fast $df at [$af], [$bf]";

    my ($as, $bs, $ds) = closest_pair_simple(@points);
    say "slow $ds at [$as], [$bs]";
}

sub dist-squared($a,$b) {
    ($a[0] - $b[0]) ** 2 +
    ($a[1] - $b[1]) ** 2;
}

sub closest_pair_simple(@arr is copy) {
    return Inf if @arr < 2;
    my ($a, $b, $d) = flat @arr[0,1], dist-squared(|@arr[0,1]);
    while  @arr {
        my $p = pop @arr;
        for @arr -> $l {
            my $t = dist-squared($p, $l);
            ($a, $b, $d) = $p, $l, $t if $t < $d;
        }
    }
    return $a, $b, sqrt $d;
}

sub closest_pair(@r) {
    my @ax = @r.sort: { .[0] }
    my @ay = @r.sort: { .[1] }
    return closest_pair_real(@ax, @ay);
}

sub closest_pair_real(@rx, @ry) {
    return closest_pair_simple(@rx) if @rx <= 3;

    my @xP = @rx;
    my @yP = @ry;
    my $N = @xP;

    my $midx = ceiling($N/2)-1;

    my @PL = @xP[0 .. $midx];
    my @PR = @xP[$midx+1 ..^ $N];

    my $xm = @xP[$midx][0];

    my @yR;
    my @yL;
    push ($_[0] <= $xm ?? @yR !! @yL), $_ for @yP;

    my ($al, $bl, $dL) = closest_pair_real(@PL, @yR);
    my ($ar, $br, $dR) = closest_pair_real(@PR, @yL);

    my ($m1, $m2, $dmin) = $dR < $dL
                               ?? ($ar, $br, $dR)
                               !! ($al, $bl, $dL);

    my @yS = @yP.grep: { abs($xm - .[0]) < $dmin }

    if @yS {
        my ($w1, $w2, $closest) = $m1, $m2, $dmin;
        for 0 ..^ @yS.end -> $i {
            for $i+1 ..^ @yS -> $k {
                last unless @yS[$k][1] - @yS[$i][1] < $dmin;
                my $d = sqrt dist-squared(@yS[$k], @yS[$i]);
                ($w1, $w2, $closest) = @yS[$k], @yS[$i], $d if $d < $closest;
            }

        }
        return $w1, $w2, $closest;

    } else {
        return $m1, $m2, $dmin;
    }
}
```



## Phix

Brute force and divide and conquer (translated from pseudocode) approaches compared

```Phix
function bruteForceClosestPair(sequence s)
atom {x1,y1} = s[1], {x2,y2} = s[2], dx = x1-x2, dy = y1-y2, mind = dx*dx+dy*dy
sequence minp = s[1..2]
    for i=1 to length(s)-1 do
        {x1,y1} = s[i]
        for j=i+1 to length(s) do
            {x2,y2} = s[j]
            dx = x1-x2
            dx = dx*dx
            if dx<mind then
                dy = y1-y2
                dx += dy*dy
                if dx<mind then
                    mind = dx
                    minp = {s[i],s[j]}
                end if
            end if
        end for
    end for
    return {sqrt(mind),minp}
end function

sequence testset = sq_rnd(repeat({1,1},10000))
atom t0 = time()
sequence points
atom d
{d,points} = bruteForceClosestPair(testset)
-- (Sorting the final point pair makes brute/dc more likely to tally. Note however
--  when >1 equidistant pairs exist, brute and dc may well return different pairs;
--  it is only a problem if they decide to return different minimum distances.)
atom {{x1,y1},{x2,y2}} = sort(points)
printf(1,"Closest pair: {%f,%f} {%f,%f}, distance=%f (%3.2fs)\n",{x1,y2,x2,y2,d,time()-t0})

t0 = time()
constant X = 1, Y = 2
sequence xP = sort(testset)

function byY(sequence p1, p2)
    return compare(p1[Y],p2[Y])
end function
sequence yP = custom_sort(routine_id("byY"),testset)

function distsq(sequence p1,p2)
atom {x1,y1} = p1, {x2,y2} = p2
    x1 -= x2
    y1 -= y2
    return x1*x1 + y1*y1
end function

function closestPair(sequence xP, yP)
--             where xP is P(1) .. P(N) sorted by x coordinate, and
--                   yP is P(1) .. P(N) sorted by y coordinate (ascending order)
integer N, midN, k, nS
sequence xL, xR, yL, yR, pairL, pairR, pairMin, yS, cPair
atom xm, dL, dR, dmin, closest

    N = length(xP)
    if length(yP)!=N then ?9/0 end if   -- (sanity check)
    if N<=3 then
        return bruteForceClosestPair(xP)
    end if
    midN = floor(N/2)
    xL = xP[1..midN]
    xR = xP[midN+1..N]
    xm = xP[midN][X]
    yL = {}
    yR = {}
    for i=1 to N do
        if yP[i][X]<=xm then
            yL = append(yL,yP[i])
        else
            yR = append(yR,yP[i])
        end if
    end for
    {dL, pairL} = closestPair(xL, yL)
    {dR, pairR} = closestPair(xR, yR)
    {dmin, pairMin} = {dR, pairR}
    if dL<dR then
        {dmin, pairMin} = {dL, pairL}
    end if
    yS = {}
    for i=1 to length(yP) do
        if abs(xm-yP[i][X])<dmin then
            yS = append(yS,yP[i])
        end if
    end for
    nS = length(yS)
    {closest, cPair} = {dmin*dmin, pairMin}
    for i=1 to nS-1 do
        k = i + 1
        while k<=nS and (yS[k][Y]-yS[i][Y])<dmin do
            d = distsq(yS[k],yS[i])
            if d<closest then
                {closest, cPair} = {d, {yS[k], yS[i]}}
            end if
            k += 1
        end while
    end for
    return {sqrt(closest), cPair}
end function

{d,points} = closestPair(xP,yP)
{{x1,y1},{x2,y2}} = sort(points)    -- (see note above)
printf(1,"Closest pair: {%f,%f} {%f,%f}, distance=%f (%3.2fs)\n",{x1,y2,x2,y2,d,time()-t0})
```

{{out}}

```txt

Closest pair: {0.0328051,0.0966250} {0.0328850,0.0966250}, distance=0.000120143 (2.37s)
Closest pair: {0.0328051,0.0966250} {0.0328850,0.0966250}, distance=0.000120143 (0.14s)

```



## PicoLisp


```PicoLisp
(de closestPairBF (Lst)
   (let Min T
      (use (Pt1 Pt2)
         (for P Lst
            (for Q Lst
               (or
                  (== P Q)
                  (>=
                     (setq N
                        (let (A (- (car P) (car Q))  B (- (cdr P) (cdr Q)))
                           (+ (* A A) (* B B)) ) )
                     Min )
                  (setq Min N  Pt1 P  Pt2 Q) ) ) )
         (list Pt1 Pt2 (sqrt Min)) ) ) )
```

Test:

```txt
: (scl 6)
-> 6

: (closestPairBF
   (quote
      (0.654682 . 0.925557)
      (0.409382 . 0.619391)
      (0.891663 . 0.888594)
      (0.716629 . 0.996200)
      (0.477721 . 0.946355)
      (0.925092 . 0.818220)
      (0.624291 . 0.142924)
      (0.211332 . 0.221507)
      (0.293786 . 0.691701)
      (0.839186 . 0.728260) ) )
-> ((891663 . 888594) (925092 . 818220) 77910)
```



## PL/I

<lang>
/* Closest Pair Problem */
closest: procedure options (main);
   declare n fixed binary;

   get list (n);
   begin;
      declare 1 P(n),
               2 x float,
               2 y float;
      declare (i, ii, j, jj) fixed binary;
      declare (distance, min_distance initial (0) ) float;

      get list (P);
      min_distance = sqrt( (P.x(1) - P.x(2))**2 + (P.y(1) - P.y(2))**2 );
      ii = 1;  jj = 2;
      do i = 1 to n;
         do j = 1 to n;
            distance = sqrt( (P.x(i) - P.x(j))**2 + (P.y(i) - P.y(j))**2 );
            if distance > 0 then
             if distance < min_distance  then
               do;
                  min_distance = distance;
                  ii = i; jj = j;
               end;
         end;
      end;
      put skip edit ('The minimum distance ', min_distance,
                     ' is between the points [', P.x(ii),
                     ',', P.y(ii), '] and [', P.x(jj), ',', P.y(jj), ']' )
                     (a, f(6,2));
   end;
end closest;

```



## Prolog

'''Brute force version, works with SWI-Prolog, tested on version 7.2.3.

```Prolog

% main predicate, find and print closest point
do_find_closest_points(Points) :-
	points_closest(Points, points(point(X1,Y1),point(X2,Y2),Dist)),
	format('Point 1 : (~p, ~p)~n', [X1,Y1]),
	format('Point 1 : (~p, ~p)~n', [X2,Y2]),
	format('Distance: ~p~n', [Dist]).

% Find the distance between two points
distance(point(X1,Y1), point(X2,Y2), points(point(X1,Y1),point(X2,Y2),Dist)) :-
	Dx is X2 - X1,
	Dy is Y2 - Y1,
	Dist is sqrt(Dx * Dx + Dy * Dy).

% find the closest point that relatest to another point
point_closest(Points, Point, Closest) :-
	select(Point, Points, Remaining),
	maplist(distance(Point), Remaining, PointList),
	foldl(closest, PointList, 0, Closest).

% find the closest point/dist pair for all points
points_closest(Points, Closest) :-
	maplist(point_closest(Points), Points, ClosestPerPoint),
	foldl(closest, ClosestPerPoint, 0, Closest).

% used by foldl to get the lowest point/distance combination
closest(points(P1,P2,Dist), 0, points(P1,P2,Dist)).
closest(points(_,_,Dist), points(P1,P2,Dist2), points(P1,P2,Dist2)) :-
	Dist2 < Dist.
closest(points(P1,P2,Dist), points(_,_,Dist2), points(P1,P2,Dist)) :-
	Dist =< Dist2.

```

To test, pass in a list of points.

```Prolog
do_find_closest_points([
    point(0.654682, 0.925557),
    point(0.409382, 0.619391),
    point(0.891663, 0.888594),
    point(0.716629, 0.996200),
    point(0.477721, 0.946355),
    point(0.925092, 0.818220),
    point(0.624291, 0.142924),
    point(0.211332, 0.221507),
    point(0.293786, 0.691701),
    point(0.839186, 0.728260)
]).

```

{{out}}

```txt

Point 1 : (0.925092, 0.81822)
Point 1 : (0.891663, 0.888594)
Distance: 0.07791019135517516
true ;
false.

```



## PureBasic

'''Brute force version

```PureBasic
Procedure.d bruteForceClosestPair(Array P.coordinate(1))
  Protected N=ArraySize(P()), i, j
  Protected mindistance.f=Infinity(), t.d
  Shared a, b
  If N<2
    a=0: b=0
  Else
    For i=0 To N-1
      For j=i+1 To N
        t=Pow(Pow(P(i)\x-P(j)\x,2)+Pow(P(i)\y-P(j)\y,2),0.5)
        If mindistance>t
          mindistance=t
          a=i: b=j
        EndIf
      Next
    Next
  EndIf
  ProcedureReturn mindistance
EndProcedure

```


Implementation can be as

```PureBasic
Structure coordinate
  x.d
  y.d
EndStructure

Dim DataSet.coordinate(9)
Define i, x.d, y.d, a, b

;- Load data from datasection
Restore DataPoints
For i=0 To 9
  Read.d x: Read.d y
  DataSet(i)\x=x
  DataSet(i)\y=y
Next i

If OpenConsole()
  PrintN("Mindistance= "+StrD(bruteForceClosestPair(DataSet()),6))
  PrintN("Point 1= "+StrD(DataSet(a)\x,6)+": "+StrD(DataSet(a)\y,6))
  PrintN("Point 2= "+StrD(DataSet(b)\x,6)+": "+StrD(DataSet(b)\y,6))
  Print(#CRLF$+"Press ENTER to quit"): Input()
EndIf

DataSection
  DataPoints:
  Data.d  0.654682, 0.925557, 0.409382, 0.619391, 0.891663, 0.888594
  Data.d  0.716629, 0.996200, 0.477721, 0.946355, 0.925092, 0.818220
  Data.d  0.624291, 0.142924, 0.211332, 0.221507, 0.293786, 0.691701, 0.839186,  0.72826
EndDataSection
```

{{out}}

```txt
Mindistance= 0.077910
Point 1= 0.891663: 0.888594
Point 2= 0.925092: 0.818220

Press ENTER to quit
```



## Python


```python
"""
  Compute nearest pair of points using two algorithms

  First algorithm is 'brute force' comparison of every possible pair.
  Second, 'divide and conquer', is based on:
    www.cs.iupui.edu/~xkzou/teaching/CS580/Divide-and-conquer-closestPair.ppt
"""

from random import randint, randrange
from operator import itemgetter, attrgetter

infinity = float('inf')

# Note the use of complex numbers to represent 2D points making distance == abs(P1-P2)

def bruteForceClosestPair(point):
    numPoints = len(point)
    if numPoints < 2:
        return infinity, (None, None)
    return min( ((abs(point[i] - point[j]), (point[i], point[j]))
                 for i in range(numPoints-1)
                 for j in range(i+1,numPoints)),
                key=itemgetter(0))

def closestPair(point):
    xP = sorted(point, key= attrgetter('real'))
    yP = sorted(point, key= attrgetter('imag'))
    return _closestPair(xP, yP)

def _closestPair(xP, yP):
    numPoints = len(xP)
    if numPoints <= 3:
        return bruteForceClosestPair(xP)
    Pl = xP[:numPoints/2]
    Pr = xP[numPoints/2:]
    Yl, Yr = [], []
    xDivider = Pl[-1].real
    for p in yP:
        if p.real <= xDivider:
            Yl.append(p)
        else:
            Yr.append(p)
    dl, pairl = _closestPair(Pl, Yl)
    dr, pairr = _closestPair(Pr, Yr)
    dm, pairm = (dl, pairl) if dl < dr else (dr, pairr)
    # Points within dm of xDivider sorted by Y coord
    closeY = [p for p in yP  if abs(p.real - xDivider) < dm]
    numCloseY = len(closeY)
    if numCloseY > 1:
        # There is a proof that you only need compare a max of 7 next points
        closestY = min( ((abs(closeY[i] - closeY[j]), (closeY[i], closeY[j]))
                         for i in range(numCloseY-1)
                         for j in range(i+1,min(i+8, numCloseY))),
                        key=itemgetter(0))
        return (dm, pairm) if dm <= closestY[0] else closestY
    else:
        return dm, pairm

def times():
    ''' Time the different functions
    '''
    import timeit

    functions = [bruteForceClosestPair, closestPair]
    for f in functions:
        print 'Time for', f.__name__, timeit.Timer(
            '%s(pointList)' % f.__name__,
            'from closestpair import %s, pointList' % f.__name__).timeit(number=1)



pointList = [randint(0,1000)+1j*randint(0,1000) for i in range(2000)]

if __name__ == '__main__':
    pointList = [(5+9j), (9+3j), (2+0j), (8+4j), (7+4j), (9+10j), (1+9j), (8+2j), 10j, (9+6j)]
    print pointList
    print '  bruteForceClosestPair:', bruteForceClosestPair(pointList)
    print '            closestPair:', closestPair(pointList)
    for i in range(10):
        pointList = [randrange(11)+1j*randrange(11) for i in range(10)]
        print '\n', pointList
        print ' bruteForceClosestPair:', bruteForceClosestPair(pointList)
        print '           closestPair:', closestPair(pointList)
    print '\n'
    times()
    times()
    times()
```


{{out}} followed by timing comparisons

(Note how the two algorithms agree on the minimum distance, but may return a different pair of points if more than one pair of points share that minimum separation):
<div style="height:30ex;overflow:scroll">
```txt
[(5+9j), (9+3j), (2+0j), (8+4j), (7+4j), (9+10j), (1+9j), (8+2j), 10j, (9+6j)]
  bruteForceClosestPair: (1.0, ((8+4j), (7+4j)))
            closestPair: (1.0, ((8+4j), (7+4j)))

[(10+6j), (7+0j), (9+4j), (4+8j), (7+5j), (6+4j), (1+9j), (6+4j), (1+3j), (5+0j)]
 bruteForceClosestPair: (0.0, ((6+4j), (6+4j)))
           closestPair: (0.0, ((6+4j), (6+4j)))

[(4+10j), (8+5j), (10+3j), (9+7j), (2+5j), (6+7j), (6+2j), (9+6j), (3+8j), (5+1j)]
 bruteForceClosestPair: (1.0, ((9+7j), (9+6j)))
           closestPair: (1.0, ((9+7j), (9+6j)))

[(10+0j), (3+10j), (10+7j), (1+8j), (5+10j), (8+8j), (4+7j), (6+2j), (6+10j), (9+3j)]
 bruteForceClosestPair: (1.0, ((5+10j), (6+10j)))
           closestPair: (1.0, ((5+10j), (6+10j)))

[(3+7j), (5+3j), 0j, (2+9j), (2+5j), (9+6j), (5+9j), (4+3j), (3+8j), (8+7j)]
 bruteForceClosestPair: (1.0, ((3+7j), (3+8j)))
           closestPair: (1.0, ((4+3j), (5+3j)))

[(4+3j), (10+9j), (2+7j), (7+8j), 0j, (3+10j), (10+2j), (7+10j), (7+3j), (1+4j)]
 bruteForceClosestPair: (2.0, ((7+8j), (7+10j)))
           closestPair: (2.0, ((7+8j), (7+10j)))

[(9+2j), (9+8j), (6+4j), (7+0j), (10+2j), (10+0j), (2+7j), (10+7j), (9+2j), (1+5j)]
 bruteForceClosestPair: (0.0, ((9+2j), (9+2j)))
           closestPair: (0.0, ((9+2j), (9+2j)))

[(3+3j), (8+2j), (4+0j), (1+1j), (9+10j), (5+0j), (2+3j), 5j, (5+0j), (7+0j)]
 bruteForceClosestPair: (0.0, ((5+0j), (5+0j)))
           closestPair: (0.0, ((5+0j), (5+0j)))

[(1+5j), (8+3j), (8+10j), (6+8j), (10+9j), (2+0j), (2+7j), (8+7j), (8+4j), (1+2j)]
 bruteForceClosestPair: (1.0, ((8+3j), (8+4j)))
           closestPair: (1.0, ((8+3j), (8+4j)))

[(8+4j), (8+6j), (8+0j), 0j, (10+7j), (10+6j), 6j, (1+3j), (1+8j), (6+9j)]
 bruteForceClosestPair: (1.0, ((10+7j), (10+6j)))
           closestPair: (1.0, ((10+7j), (10+6j)))

[(6+8j), (10+1j), 3j, (7+9j), (4+10j), (4+7j), (5+7j), (6+10j), (4+7j), (2+4j)]
 bruteForceClosestPair: (0.0, ((4+7j), (4+7j)))
           closestPair: (0.0, ((4+7j), (4+7j)))


Time for bruteForceClosestPair 4.57953371169
Time for closestPair 0.122539596513
Time for bruteForceClosestPair 5.13221177552
Time for closestPair 0.124602707886
Time for bruteForceClosestPair 4.83609397284
Time for closestPair 0.119326618327
>>>
```
</div>


## R

{{works with|R|2.8.1+}}
Brute force solution as per wikipedia pseudo-code

```R
closest_pair_brute <-function(x,y,plotxy=F) {
    xy = cbind(x,y)
    cp = bruteforce(xy)
    cat("\n\nShortest path found = \n From:\t\t(",cp[1],',',cp[2],")\n To:\t\t(",cp[3],',',cp[4],")\n Distance:\t",cp[5],"\n\n",sep="")
    if(plotxy) {
        plot(x,y,pch=19,col='black',main="Closest Pair", asp=1)
        points(cp[1],cp[2],pch=19,col='red')
        points(cp[3],cp[4],pch=19,col='red')
    }
    distance <- function(p1,p2) {
        x1 = (p1[1])
        y1 = (p1[2])
        x2 = (p2[1])
        y2 = (p2[2])
        sqrt((x2-x1)^2 + (y2-y1)^2)
    }
    bf_iter <- function(m,p,idx=NA,d=NA,n=1) {
        dd = distance(p,m[n,])
        if((is.na(d) || dd<=d) && p!=m[n,]){d = dd; idx=n;}
        if(n == length(m[,1])) { c(m[idx,],d) }
        else bf_iter(m,p,idx,d,n+1)
    }
    bruteforce <- function(pmatrix,n=1,pd=c(NA,NA,NA,NA,NA)) {
        p = pmatrix[n,]
        ppd = c(p,bf_iter(pmatrix,p))
        if(ppd[5]<pd[5] || is.na(pd[5])) pd = ppd
        if(n==length(pmatrix[,1]))  pd
        else bruteforce(pmatrix,n+1,pd)
    }
}
```


Quicker brute force solution for R that makes use of the apply function native to R for dealing with matrices.  It expects x and y to take the form of separate vectors.

```R
closestPair<-function(x,y)
  {
  distancev <- function(pointsv)
    {
    x1 <- pointsv[1]
    y1 <- pointsv[2]
    x2 <- pointsv[3]
    y2 <- pointsv[4]
    sqrt((x1 - x2)^2 + (y1 - y2)^2)
    }
  pairstocompare <- t(combn(length(x),2))
  pointsv <- cbind(x[pairstocompare[,1]],y[pairstocompare[,1]],x[pairstocompare[,2]],y[pairstocompare[,2]])
  pairstocompare <- cbind(pairstocompare,apply(pointsv,1,distancev))
  minrow <- pairstocompare[pairstocompare[,3] == min(pairstocompare[,3])]
  if (!is.null(nrow(minrow))) {print("More than one point at this distance!"); minrow <- minrow[1,]}
  cat("The closest pair is:\n\tPoint 1: ",x[minrow[1]],", ",y[minrow[1]],
                          "\n\tPoint 2: ",x[minrow[2]],", ",y[minrow[2]],
                          "\n\tDistance: ",minrow[3],"\n",sep="")
  c(distance=minrow[3],x1.x=x[minrow[1]],y1.y=y[minrow[1]],x2.x=x[minrow[2]],y2.y=y[minrow[2]])
  }
```



This is the quickest version, that makes use of the 'dist' function of R. It takes a two-column object of x,y-values as input, or creates such an object from seperate x and y-vectors.


```R
closest.pairs <- function(x, y=NULL, ...){
      # takes two-column object(x,y-values), or creates such an object from x and y values
       if(!is.null(y))  x <- cbind(x, y)

       distances <- dist(x)
        min.dist <- min(distances)
          point.pair <- combn(1:nrow(x), 2)[, which.min(distances)]

     cat("The closest pair is:\n\t",
      sprintf("Point 1: %.3f, %.3f \n\tPoint 2: %.3f, %.3f \n\tDistance: %.3f.\n",
        x[point.pair[1],1], x[point.pair[1],2],
          x[point.pair[2],1], x[point.pair[2],2],
            min.dist),
            sep=""   )
     c( x1=x[point.pair[1],1],y1=x[point.pair[1],2],
        x2=x[point.pair[2],1],y2=x[point.pair[2],2],
        distance=min.dist)
     }
```


Example
```R
x = (sample(-1000.00:1000.00,100))
y = (sample(-1000.00:1000.00,length(x)))
cp = closest.pairs(x,y)
#cp = closestPair(x,y)
plot(x,y,pch=19,col='black',main="Closest Pair", asp=1)
points(cp["x1.x"],cp["y1.y"],pch=19,col='red')
points(cp["x2.x"],cp["y2.y"],pch=19,col='red')
#closest_pair_brute(x,y,T)

Performance
system.time(closest_pair_brute(x,y), gcFirst = TRUE)
Shortest path found =
 From:          (32,-987)
 To:            (25,-993)
 Distance:      9.219544

   user  system elapsed
   0.35    0.02    0.37

system.time(closest.pairs(x,y), gcFirst = TRUE)
The closest pair is:
        Point 1: 32.000, -987.000
        Point 2: 25.000, -993.000
        Distance: 9.220.

   user  system elapsed
   0.08    0.00    0.10

system.time(closestPair(x,y), gcFirst = TRUE)
The closest pair is:
        Point 1: 32, -987
        Point 2: 25, -993
        Distance: 9.219544

   user  system elapsed
   0.17    0.00    0.19


```


Using dist function for brute force, but divide and conquer (as per pseudocode) for speed:

```R
closest.pairs.bruteforce <- function(x, y=NULL)
{
	if (!is.null(y))
	{
		x <- cbind(x,y)
	}
	d <- dist(x)
	cp <- x[combn(1:nrow(x), 2)[, which.min(d)],]
	list(p1=cp[1,], p2=cp[2,], d=min(d))
}

closest.pairs.dandc <- function(x, y=NULL)
{
	if (!is.null(y))
	{
		x <- cbind(x,y)
	}
	if (sd(x[,"x"]) < sd(x[,"y"]))
	{
		x <- cbind(x=x[,"y"],y=x[,"x"])
		swap <- TRUE
	}
	else
	{
		swap <- FALSE
	}
	xp <- x[order(x[,"x"]),]
	.cpdandc.rec <- function(xp,yp)
	{
		n <- dim(xp)[1]
		if (n <= 4)
		{
			closest.pairs.bruteforce(xp)
		}
		else
		{
			xl <- xp[1:floor(n/2),]
			xr <- xp[(floor(n/2)+1):n,]
			cpl <- .cpdandc.rec(xl)
			cpr <- .cpdandc.rec(xr)
			if (cpl$d<cpr$d) cp <- cpl else cp <- cpr
			cp
		}
	}
	cp <- .cpdandc.rec(xp)

	yp <- x[order(x[,"y"]),]
	xm <- xp[floor(dim(xp)[1]/2),"x"]
	ys <- yp[which(abs(xm - yp[,"x"]) <= cp$d),]
	nys <- dim(ys)[1]
	if (!is.null(nys) && nys > 1)
	{
		for (i in 1:(nys-1))
		{
			k <- i + 1
			while (k <= nys && ys[i,"y"] - ys[k,"y"] < cp$d)
			{
				d <- sqrt((ys[k,"x"]-ys[i,"x"])^2 + (ys[k,"y"]-ys[i,"y"])^2)
				if (d < cp$d) cp <- list(p1=ys[i,],p2=ys[k,],d=d)
				k <- k + 1
			}
		}
	}
	if (swap)
	{
		list(p1=cbind(x=cp$p1["y"],y=cp$p1["x"]),p2=cbind(x=cp$p2["y"],y=cp$p2["x"]),d=cp$d)
	}
	else
	{
		cp
	}
}

# Test functions
cat("How many points?\n")
n <- scan(what=integer(),n=1)
x <- rnorm(n)
y <- rnorm(n)
tstart <- proc.time()[3]
cat("Closest pairs divide and conquer:\n")
print(cp <- closest.pairs.dandc(x,y))
cat(sprintf("That took %.2f seconds.\n",proc.time()[3] - tstart))
plot(x,y)
points(c(cp$p1["x"],cp$p2["x"]),c(cp$p1["y"],cp$p2["y"]),col="red")
tstart <- proc.time()[3]
cat("\nClosest pairs brute force:\n")
print(closest.pairs.bruteforce(x,y))
cat(sprintf("That took %.2f seconds.\n",proc.time()[3] - tstart))

```

{{out}}

```txt

How many points?
1: 500
Read 1 item
Closest pairs divide and conquer:
$p1
         x          y
1.68807938 0.05876328

$p2
         x          y
1.68904694 0.05878173

$d
[1] 0.0009677302

That took 0.43 seconds.

Closest pairs brute force:
$p1
         x          y
1.68807938 0.05876328

$p2
         x          y
1.68904694 0.05878173

$d
[1] 0.0009677302

That took 6.38 seconds.

```



## Racket

The brute force solution using complex numbers
to represent pairs.

```racket

#lang racket
(define (dist z0 z1) (magnitude (- z1 z0)))
(define (dist* zs)  (apply dist zs))

(define (closest-pair zs)
  (if (< (length zs) 2)
      -inf.0
      (first
       (sort (for/list ([z0 zs])
               (list z0 (argmin (λ(z) (if (= z z0) +inf.0 (dist z z0))) zs)))
             < #:key dist*))))

(define result (closest-pair '(0+1i 1+2i 3+4i)))
(displayln (~a "Closest points: " result))
(displayln (~a "Distance: " (dist* result)))

```


The divide and conquer algorithm using a struct to represent points

```racket

#lang racket
(struct point (x y) #:transparent)

(define (closest-pair ps)
  (check-type ps)
  (cond [(vector? ps) (if (> (vector-length ps) 1)
                          (closest-pair/sorted (vector-sort ps left?)
                                               (vector-sort ps below?))
                          (error 'closest-pair "2 or more points are needed" ps))]
        [(sequence? ps) (closest-pair (for/vector ([x (in-sequences ps)]) x))]
        [else (error 'closest-pair "closest pair only supports sequence types (excluding hash)")]))

;; accept any sequence type except hash
;; any other exclusions needed?
(define (check-type ps)
  (cond [(hash? ps) (error 'closest-pair "Hash tables are not supported")]
        [(sequence? ps) #t]
        [else (error 'closest-pair "Only sequence types are supported")]))

;; vector -> vector -> list
(define (closest-pair/sorted Px Py)
  (define L (vector-length Px))
  (cond [(= L 2) (vector->list Px)]
        [(= L 3) (apply min-pair (combinations (vector->list Px) 2))]
        [else (let*-values ([(Qx Rx) (vector-split-at Px (floor (/ L 2)))]
                            ; Rx-min is the left most point in Rx
                            [(Rx-min) (vector-ref Rx 0)]
                            ; instead of sorting Qx, Rx by y
                            ; - Qy are members of Py to left of Rx-min
                            ; - Ry are the remaining members of Py
                            [(Qy Ry) (vector-partition Py (curryr left? Rx-min))]
                            [(pair1) (closest-pair/sorted Qx Qy)]
                            [(pair2) (closest-pair/sorted Rx Ry)]
                            [(delta) (min (distance^2 pair1) (distance^2 pair2))]
                            [(pair3) (closest-split-pair Px Py delta)])
                ; pair3 is null when there are no split pairs closer than delta
                (min-pair pair1 pair2 pair3))]))

(define (closest-split-pair Px Py delta)
  (define Lp (vector-length Px))
  (define x-mid (point-x (vector-ref Px (floor (/ Lp 2)))))
  (define Sy (for/vector ([p (in-vector Py)]
                          #:when (< (abs (- (point-x p) x-mid)) delta))
               p))
  (define Ls (vector-length Sy))
  (define-values (_ best-pair)
    (for*/fold ([new-best delta]
                [new-best-pair null])
               ([i (in-range (sub1 Ls))]
                [j (in-range (+ i 1) (min (+ i 7) Ls))]
                [Sij (in-value (list (vector-ref Sy i)
                                     (vector-ref Sy j)))]
                [dij (in-value (distance^2 Sij))]
                #:when (< dij new-best))
      (values dij Sij)))
  best-pair)

;; helper procedures

;; same as partition except for vectors
;; it's critical to maintain the relative order of elements
(define (vector-partition Py pred)
  (define-values (left right)
    (for/fold ([Qy null]
               [Ry null])
              ([p (in-vector Py)])
      (if (pred p)
          (values (cons p Qy) Ry)
          (values Qy (cons p Ry)))))
  (values (list->vector (reverse left))
          (list->vector (reverse right))))

; is p1 (strictly) left of p2
(define (left? p1 p2)  (< (point-x p1) (point-x p2)))

; is p1 (strictly) below of p2
(define (below? p1 p2) (< (point-y p1) (point-y p2)))

;; return the pair with minimum distance
(define (min-pair . pairs)
  (argmin distance^2 pairs))

;; pairs are passed around as a list of 2 points
;; distance is only for comparison so no need to use sqrt
(define (distance^2 pair)
  (cond [(null? pair) +inf.0]
        [else (define a (first pair))
              (define b (second pair))
              (+ (sqr (- (point-x b) (point-x a)))
                 (sqr (- (point-y b) (point-y a))))]))

; points on a quadratic curve, shuffled
(define points
       (shuffle
        (for/list ([ i (in-range 1000)]) (point i (* i i)))))
(match-define (list (point p1x p1y) (point p2x p2y)) (closest-pair points))
(printf "Closest points on a quadratic curve (~a,~a) (~a,~a)\n" p1x p1y p2x p2y)

```


{{out}}

```racket

Closest points: (0+1i 1+2i)
Distance: 1.4142135623730951

Closest points on a quadratic curve (0,0) (1,1)

```



## REXX

Programming note:   this REXX version allows two (or more) points to be identical, and will

manifest itself as a minimum distance of zero   (the variable   <big> <tt> '''dd''' </tt> </big>   on line 17).

```rexx
/*REXX program  solves the   closest pair   of  points  problem  (in two dimensions).   */
parse arg N low high seed .                      /*obtain optional arguments from the CL*/
if    N=='' |    N==","  then    N=   100        /*Not specified?  Then use the default.*/
if  low=='' |  low==","  then  low=     0        /* "      "         "   "   "     "    */
if high=='' | high==","  then high= 20000        /* "      "         "   "   "     "    */
if datatype(seed, 'W')   then call random ,,seed /*seed for RANDOM (BIF)  repeatability.*/
w=length(high);   w=w + (w//2==0)                /*W:   for aligning the output columns.*/
   /*╔══════════════════════╗*/      do j=1  for N            /*generate N random points*/
   /*║ generate  N  points. ║*/      @x.j= random(low, high)  /*    "    a random   X   */
   /*╚══════════════════════╝*/      @y.j= random(low, high)  /*    "    "    "     Y   */
                                     end   /*j*/              /*X  &  Y  make the point.*/
           A=1;   B=2                            /* [↓]  MINDD  is actually the  squared*/
minDD= (@x.A - @x.B)**2   +   (@y.A - @y.B)**2   /*distance between the first two points*/
                                                 /* [↓]  use of XJ & YJ speed things up.*/
    do   j=1   for N-1;   xj= @x.j;   yj= @y.j   /*find minimum distance between a ···  */
      do k=j+1  to N                             /*  ··· point and all the other points.*/
      dd= (xj - @x.k)**2   +   (yj - @y.k)**2    /*compute squared distance from points.*/
      if dd<minDD  then parse  value     dd  j  k      with      minDD  A  B
      end   /*k*/                                /* [↑]  needn't take SQRT of DD  (yet).*/
    end     /*j*/                                /* [↑]  when done, A & B are the points*/
                 $= 'For '   N   " points, the minimum distance between the two points:  "
say $ center("x", w, '═')" "      center('y', w, "═")      '  is: '     sqrt(abs(minDD))/1
say left('', length($) - 1)       "["right(@x.A, w)','           right(@y.A, w)"]"
say left('', length($) - 1)       "["right(@x.B, w)','           right(@y.B, w)"]"
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x; if x=0  then return 0; d=digits(); m.=9; numeric form; h=d+6
      numeric digits;  parse value format(x,2,1,,0) 'E0' with g 'E' _ .;  g= g *.5'e'_ % 2
        do j=0  while h>9;      m.j= h;              h= h % 2  +  1;  end  /*j*/
        do k=j+5  to 0  by -1;  numeric digits m.k;  g= (g+x/g)*.5;   end  /*k*/; return g
```

{{out|output|text=  when using the default input of:   <tt> 100 </tt>}}

```txt

For  100  points, the minimum distance between the two points:   ══x══  ══y══   is:  219.228192
                                                                [ 7277,  1625]
                                                                [ 7483,  1700]

```

{{out|output|text=  when using the input of:   <tt> 200 </tt>}}

```txt

For  200  points, the minimum distance between the two points:   ══x══  ══y══   is:  39.408121
                                                                [17604, 19166]
                                                                [17627, 19198]

```

{{out|output|text=  when using the input of:   <tt> 1000 </tt>
}}

```txt

For  1000  points, the minimum distance between the two points:   ══x══  ══y══   is:  5.09901951
                                                                 [ 6264, 19103]
                                                                 [ 6263, 19108]

```



## Ring


```ring

decimals(10)
x = list(10)
y = list(10)
x[1] = 0.654682
y[1] = 0.925557
x[2] = 0.409382
y[2] = 0.619391
x[3] = 0.891663
y[3] = 0.888594
x[4] = 0.716629
y[4] = 0.996200
x[5] = 0.477721
y[5] = 0.946355
x[6] = 0.925092
y[6] = 0.818220
x[7] = 0.624291
y[7] = 0.142924
x[8] = 0.211332
y[8] = 0.221507
x[9] = 0.293786
y[9] = 0.691701
x[10] = 0.839186
y[10] = 0.728260

min = 10000
for i = 1 to 9
    for j = i+1 to 10
        dsq = pow((x[i] - x[j]),2) + pow((y[i] - y[j]),2)
        if dsq < min min = dsq  mini = i minj = j ok
    next
next
see "closest pair is : " + mini + " and " + minj + " at distance " + sqrt(min)

```

Output:

```txt

closest pair is : 3 and 6 at distance 0.0779101914

```



## Ruby


```ruby
Point = Struct.new(:x, :y)

def distance(p1, p2)
  Math.hypot(p1.x - p2.x, p1.y - p2.y)
end

def closest_bruteforce(points)
  mindist, minpts = Float::MAX, []
  points.combination(2) do |pi,pj|
    dist = distance(pi, pj)
    if dist < mindist
      mindist = dist
      minpts = [pi, pj]
    end
  end
  [mindist, minpts]
end

def closest_recursive(points)
  return closest_bruteforce(points) if points.length <= 3
  xP = points.sort_by(&:x)
  mid = points.length / 2
  xm = xP[mid].x
  dL, pairL = closest_recursive(xP[0,mid])
  dR, pairR = closest_recursive(xP[mid..-1])
  dmin, dpair = dL<dR ? [dL, pairL] : [dR, pairR]
  yP = xP.find_all {|p| (xm - p.x).abs < dmin}.sort_by(&:y)
  closest, closestPair = dmin, dpair
  0.upto(yP.length - 2) do |i|
    (i+1).upto(yP.length - 1) do |k|
      break if (yP[k].y - yP[i].y) >= dmin
      dist = distance(yP[i], yP[k])
      if dist < closest
        closest = dist
        closestPair = [yP[i], yP[k]]
      end
    end
  end
  [closest, closestPair]
end

points = Array.new(100) {Point.new(rand, rand)}
p ans1 = closest_bruteforce(points)
p ans2 = closest_recursive(points)
fail "bogus!" if ans1[0] != ans2[0]

require 'benchmark'

points = Array.new(10000) {Point.new(rand, rand)}
Benchmark.bm(12) do |x|
  x.report("bruteforce") {ans1 = closest_bruteforce(points)}
  x.report("recursive")  {ans2 = closest_recursive(points)}
end
```

'''Sample output'''

```txt

[0.005299616045889868, [#<struct Point x=0.24805908871087445, y=0.8413503128160198>, #<struct Point x=0.24355227214243136, y=0.8385620275629906>]]
[0.005299616045889868, [#<struct Point x=0.24355227214243136, y=0.8385620275629906>, #<struct Point x=0.24805908871087445, y=0.8413503128160198>]]
                   user     system      total        real
bruteforce    43.446000   0.000000  43.446000 ( 43.530062)
recursive      0.187000   0.000000   0.187000 (  0.190000)

```



## Run BASIC

Courtesy http://dkokenge.com/rbp

```runbasic
n =10                              ' 10 data points input
dim x(n)
dim y(n)

pt1 = 0                            ' 1st point
pt2 = 0                            ' 2nd point

for i =1 to n                      ' read in data
    read x(i)
    read y(i)
next i

minDist  = 1000000

for i =1 to n -1
    for j =i +1 to n
      distXsq =(x(i) -x(j))^2
      disYsq  =(y(i) -y(j))^2
      d       =abs((dxSq +disYsq)^0.5)
      if d <minDist then
        minDist =d
        pt1     =i
        pt2     =j
      end if
    next j
next i

print "Distance ="; minDist; " between ("; x(pt1); ", "; y(pt1); ") and ("; x(pt2); ", "; y(pt2); ")"

end

data  0.654682, 0.925557
data  0.409382, 0.619391
data  0.891663, 0.888594
data  0.716629, 0.996200
data  0.477721, 0.946355
data  0.925092, 0.818220
data  0.624291, 0.142924
data  0.211332, 0.221507
data  0.293786, 0.691701
data  0.839186,  0.72826
```



## Scala


```Scala
import scala.collection.mutable.ListBuffer
import scala.util.Random

object ClosestPair {
  case class Point(x: Double, y: Double){
    def distance(p: Point) = math.hypot(x-p.x, y-p.y)

    override def toString = "(" + x + ", " + y + ")"
  }

  case class Pair(point1: Point, point2: Point) {
    val distance: Double = point1 distance point2

    override def toString = {
      point1 + "-" + point2 + " : " + distance
    }
  }

  def sortByX(points: List[Point]) = {
    points.sortBy(point => point.x)
  }

  def sortByY(points: List[Point]) = {
    points.sortBy(point => point.y)
  }

  def divideAndConquer(points: List[Point]): Pair = {
    val pointsSortedByX = sortByX(points)
    val pointsSortedByY = sortByY(points)

    divideAndConquer(pointsSortedByX, pointsSortedByY)
  }

  def bruteForce(points: List[Point]): Pair = {
    val numPoints = points.size
    if (numPoints < 2)
      return null
    var pair = Pair(points(0), points(1))
    if (numPoints > 2) {
      for (i <- 0 until numPoints - 1) {
        val point1 = points(i)
        for (j <- i + 1 until numPoints) {
          val point2 = points(j)
          val distance = point1 distance point2
          if (distance < pair.distance)
            pair = Pair(point1, point2)
        }
      }
    }
    return pair
  }


  private def divideAndConquer(pointsSortedByX: List[Point], pointsSortedByY: List[Point]): Pair = {
    val numPoints = pointsSortedByX.size
    if(numPoints <= 3) {
      return bruteForce(pointsSortedByX)
    }

    val dividingIndex = numPoints >>> 1
    val leftOfCenter = pointsSortedByX.slice(0, dividingIndex)
    val rightOfCenter = pointsSortedByX.slice(dividingIndex, numPoints)

    var tempList = leftOfCenter.map(x => x)
    //println(tempList)
    tempList = sortByY(tempList)
    var closestPair = divideAndConquer(leftOfCenter, tempList)

    tempList = rightOfCenter.map(x => x)
    tempList = sortByY(tempList)

    val closestPairRight = divideAndConquer(rightOfCenter, tempList)

    if (closestPairRight.distance < closestPair.distance)
      closestPair = closestPairRight

    tempList = List[Point]()
    val shortestDistance = closestPair.distance
    val centerX = rightOfCenter(0).x

    for (point <- pointsSortedByY) {
      if (Math.abs(centerX - point.x) < shortestDistance)
        tempList = tempList :+ point
    }

    closestPair = shortestDistanceF(tempList, shortestDistance, closestPair)
    closestPair
  }

  private def shortestDistanceF(tempList: List[Point], shortestDistance: Double, closestPair: Pair ): Pair = {
    var shortest = shortestDistance
    var bestResult = closestPair
    for (i <- 0 until tempList.size) {
      val point1 = tempList(i)
      for (j <- i + 1 until tempList.size) {
        val point2 = tempList(j)
        if ((point2.y - point1.y) >= shortestDistance)
          return closestPair
        val distance = point1 distance point2
        if (distance < closestPair.distance)
        {
          bestResult = Pair(point1, point2)
          shortest = distance
        }
      }
    }

    closestPair
  }

  def main(args: Array[String]) {
    val numPoints = if(args.length == 0) 1000 else args(0).toInt

    val points = ListBuffer[Point]()
    val r = new Random()
    for (i <- 0 until numPoints) {
      points.+=:(new Point(r.nextDouble(), r.nextDouble()))
    }
    println("Generated " + numPoints + " random points")

    var startTime = System.currentTimeMillis()
    val bruteForceClosestPair = bruteForce(points.toList)
    var elapsedTime = System.currentTimeMillis() - startTime
    println("Brute force (" + elapsedTime + " ms): " + bruteForceClosestPair)

    startTime = System.currentTimeMillis()
    val dqClosestPair = divideAndConquer(points.toList)
    elapsedTime = System.currentTimeMillis() - startTime
    println("Divide and conquer (" + elapsedTime + " ms): " + dqClosestPair)
    if (bruteForceClosestPair.distance != dqClosestPair.distance)
      println("MISMATCH")
  }
}

```


{{out}}

```txt
scala ClosestPair 1000
Generated 1000 random points
Brute force (981 ms): (0.41984960343173994, 0.4499078600557793)-(0.4198255166110827, 0.45044969701435) : 5.423720721077961E-4
Divide and conquer (52 ms): (0.4198255166110827, 0.45044969701435)-(0.41984960343173994, 0.4499078600557793) : 5.423720721077961E-4

```



## Seed7

This is the brute force algorithm:


```seed7
const type: point is new struct
    var float: x is 0.0;
    var float: y is 0.0;
  end struct;

const func float: distance (in point: p1, in point: p2) is
  return sqrt((p1.x-p2.x)**2+(p1.y-p2.y)**2);

const func array point: closest_pair (in array point: points) is func
  result
    var array point: result is 0 times point.value;
  local
    var float: dist is 0.0;
    var float: minDistance is Infinity;
    var integer: i is 0;
    var integer: j is 0;
    var integer: savei is 0;
    var integer: savej is 0;
  begin
    for i range 1 to pred(length(points)) do
      for j range succ(i) to length(points) do
        dist := distance(points[i], points[j]);
        if dist < minDistance then
          minDistance := dist;
          savei := i;
          savej := j;
        end if;
      end for;
    end for;
    if minDistance <> Infinity then
      result := [] (points[savei], points[savej]);
    end if;
  end func;
```



## Sidef

{{trans|Perl 6}}

```ruby
func dist_squared(a, b) {
    sqr(a[0] - b[0]) + sqr(a[1] - b[1])
}

func closest_pair_simple(arr) {
    arr.len < 2 && return Inf
    var (a, b, d) = (arr[0, 1], dist_squared(arr[0,1]))
    arr.clone!
    while (arr) {
        var p = arr.pop
        for l in arr {
            var t = dist_squared(p, l)
            if (t < d) {
                (a, b, d) = (p, l, t)
            }
        }
    }
    return(a, b, d.sqrt)
}

func closest_pair_real(rx, ry) {
    rx.len <= 3 && return closest_pair_simple(rx)

    var N = rx.len
    var midx = (ceil(N/2)-1)
    var (PL, PR) = rx.part(midx)

    var xm = rx[midx][0]

    var yR = []
    var yL = []

    for item in ry {
        (item[0] <= xm ? yR : yL) << item
    }

    var (al, bl, dL) = closest_pair_real(PL, yR)
    var (ar, br, dR) = closest_pair_real(PR, yL)

    al == Inf && return (ar, br, dR)
    ar == Inf && return (al, bl, dL)

    var (m1, m2, dmin) = (dR < dL ? [ar, br, dR]...
                                  : [al, bl, dL]...)

    var yS = ry.grep { |a| abs(xm - a[0]) < dmin }

    var (w1, w2, closest) = (m1, m2, dmin)
    for i in (0 ..^ yS.end) {
        for k in (i+1 .. yS.end) {
            yS[k][1] - yS[i][1] < dmin || break
            var d = dist_squared(yS[k], yS[i]).sqrt
            if (d < closest) {
                (w1, w2, closest) = (yS[k], yS[i], d)
            }
        }
    }

    return (w1, w2, closest)
}

func closest_pair(r) {
    var ax = r.sort_by { |a| a[0] }
    var ay = r.sort_by { |a| a[1] }
    return closest_pair_real(ax, ay);
}

var N = 5000
var points = N.of { [1.rand*20 - 10, 1.rand*20 - 10] }
var (af, bf, df) = closest_pair(points)
say "#{df} at (#{af.join(' ')}), (#{bf.join(' ')})"
```



## Smalltalk

See [[Closest-pair problem/Smalltalk]]


## Swift



```swift
import Foundation

struct Point {
  var x: Double
  var y: Double

  func distance(to p: Point) -> Double {
    let x = pow(p.x - self.x, 2)
    let y = pow(p.y - self.y, 2)

    return (x + y).squareRoot()
  }
}

extension Collection where Element == Point {
  func closestPair() -> (Point, Point)? {
    let (xP, xY) = (sorted(by: { $0.x < $1.x }), sorted(by: { $0.y < $1.y }))

    return Self.closestPair(xP, xY)?.1
  }

  static func closestPair(_ xP: [Element], _ yP: [Element]) -> (Double, (Point, Point))? {
    guard xP.count > 3 else { return xP.closestPairBruteForce() }

    let half = xP.count / 2
    let xl = Array(xP[..<half])
    let xr = Array(xP[half...])
    let xm = xl.last!.x
    let (yl, yr) = yP.reduce(into: ([Element](), [Element]()), {cur, el in
      if el.x > xm {
        cur.1.append(el)
      } else {
        cur.0.append(el)
      }
    })

    guard let (distanceL, pairL) = closestPair(xl, yl) else { return nil }
    guard let (distanceR, pairR) = closestPair(xr, yr) else { return nil }

    let (dMin, pairMin) = distanceL > distanceR ? (distanceR, pairR) : (distanceL, pairL)

    let ys = yP.filter({ abs(xm - $0.x) < dMin })

    var (closest, pairClosest) = (dMin, pairMin)

    for i in 0..<ys.count {
      let p1 = ys[i]

      for k in i+1..<ys.count {
        let p2 = ys[k]

        guard abs(p2.y - p1.y) < dMin else { break }

        let distance = abs(p1.distance(to: p2))

        if distance < closest {
          (closest, pairClosest) = (distance, (p1, p2))
        }
      }
    }

    return (closest, pairClosest)
  }

  func closestPairBruteForce() -> (Double, (Point, Point))? {
    guard count >= 2 else { return nil }

    var closestPoints = (self.first!, self[index(after: startIndex)])
    var minDistance = abs(closestPoints.0.distance(to: closestPoints.1))

    guard count != 2 else { return (minDistance, closestPoints) }

    for i in 0..<count {
      for j in i+1..<count {
        let (iIndex, jIndex) = (index(startIndex, offsetBy: i), index(startIndex, offsetBy: j))
        let (p1, p2) = (self[iIndex], self[jIndex])

        let distance = abs(p1.distance(to: p2))

        if distance < minDistance {
          minDistance = distance
          closestPoints = (p1, p2)
        }
      }
    }

    return (minDistance, closestPoints)
  }
}

var points = [Point]()

for _ in 0..<10_000 {
  points.append(Point(
    x: .random(in: -10.0...10.0),
    y: .random(in: -10.0...10.0)
  ))
}

print(points.closestPair()!)
```


{{out}}


```txt
(Point(x: 5.279430517795172, y: 8.85108182685002), Point(x: 5.278427575530877, y: 8.851990433099456))
```



## Tcl

Each point is represented as a list of two floating-point numbers, the first being the ''x'' coordinate, and the second being the ''y''.

```Tcl
package require Tcl 8.5

# retrieve the x-coordinate
proc x p {lindex $p 0}
# retrieve the y-coordinate
proc y p {lindex $p 1}

proc distance {p1 p2} {
    expr {hypot(([x $p1]-[x $p2]), ([y $p1]-[y $p2]))}
}

proc closest_bruteforce {points} {
    set n [llength $points]
    set mindist Inf
    set minpts {}
    for {set i 0} {$i < $n - 1} {incr i} {
        for {set j [expr {$i + 1}]} {$j < $n} {incr j} {
            set p1 [lindex $points $i]
            set p2 [lindex $points $j]
            set dist [distance $p1 $p2]
            if {$dist < $mindist} {
                set mindist $dist
                set minpts [list $p1 $p2]
            }
        }
    }
    return [list $mindist $minpts]
}

proc closest_recursive {points} {
    set n [llength $points]
    if {$n <= 3} {
        return [closest_bruteforce $points]
    }
    set xP [lsort -real -increasing -index 0 $points]
    set mid [expr {int(ceil($n/2.0))}]
    set PL [lrange $xP 0 [expr {$mid-1}]]
    set PR [lrange $xP $mid end]
    set procname [lindex [info level 0] 0]
    lassign [$procname $PL] dL pairL
    lassign [$procname $PR] dR pairR
    if {$dL < $dR} {
        set dmin $dL
        set dpair $pairL
    } else {
        set dmin $dR
        set dpair $pairR
    }

    set xM [x [lindex $PL end]]
    foreach p $xP {
        if {abs($xM - [x $p]) < $dmin} {
            lappend S $p
        }
    }
    set yP [lsort -real -increasing -index 1 $S]
    set closest Inf
    set nP [llength $yP]
    for {set i 0} {$i <= $nP-2} {incr i} {
        set yPi [lindex $yP $i]
        for {set k [expr {$i+1}]; set yPk [lindex $yP $k]} {
            $k < $nP-1 && ([y $yPk]-[y $yPi]) < $dmin
        } {incr k; set yPk [lindex $yP $k]} {
            set dist [distance $yPk $yPi]
            if {$dist < $closest} {
                set closest $dist
                set closestPair [list $yPi $yPk]
            }
        }
    }
    expr {$closest < $dmin ? [list $closest $closestPair] : [list $dmin $dpair]}
}

# testing
set N 10000
for {set i 1} {$i <= $N} {incr i} {
    lappend points [list [expr {rand()*100}] [expr {rand()*100}]]
}

# instrument the number of calls to [distance] to examine the
# efficiency of the recursive solution
trace add execution distance enter comparisons
proc comparisons args {incr ::comparisons}

puts [format "%-10s  %9s  %9s  %s" method compares time closest]
foreach method {bruteforce recursive} {
    set ::comparisons 0
    set time [time {set ::dist($method) [closest_$method $points]} 1]
    puts [format "%-10s  %9d  %9d  %s" $method $::comparisons [lindex $time 0] [lindex $::dist($method) 0]]
}
```

{{out}}

```txt
method      compares      time closest
bruteforce  49995000 512967207 0.0015652738546658382
recursive      14613    488094 0.0015652738546658382
```

Note that the <code>lindex</code> and <code>llength</code> commands are both O(1).


## Ursala

The brute force algorithm is easy.
Reading from left to right, clop is defined as a function
that forms the Cartesian product of its argument,
and then extracts the member whose left side is a minimum
with respect to the floating point comparison relation
after deleting equal pairs and attaching to the left of
each remaining pair the sum of the squares of the differences
between corresponding coordinates.

```Ursala
#import flo

clop = @iiK0 fleq$-&l+ *EZF ^\~& plus+ sqr~~+ minus~~bbI
```

The divide and conquer algorithm following the specification
given above is a little more hairy but not much longer.
The <code>eudist</code> library function
is used to compute the distance between points.

```Ursala
#import std
#import flo

clop =

^(fleq-<&l,fleq-<&r); @blrNCCS ~&lrbhthPX2X+ ~&a^& fleq$-&l+ leql/8?al\^(eudist,~&)*altK33htDSL -+
   ^C/~&rr ^(eudist,~&)*tK33htDSL+ @rlrlPXPlX ~| fleq^\~&lr abs+ minus@llPrhPX,
   ^/~&ar @farlK30K31XPGbrlrjX3J ^/~&arlhh @W lesser fleq@bl+-
```

test program:

```Ursala
test_data =

<
   (1.547290e+00,3.313053e+00),
   (5.250805e-01,-7.300260e+00),
   (7.062114e-02,1.220251e-02),
   (-4.473024e+00,-5.393712e+00),
   (-2.563714e+00,-3.595341e+00),
   (-2.132372e+00,2.358850e+00),
   (2.366238e+00,-9.678425e+00),
   (-1.745694e+00,3.276434e+00),
   (8.066843e+00,-9.101268e+00),
   (-8.256901e+00,-8.717900e+00),
   (7.397744e+00,-5.366434e+00),
   (2.060291e-01,2.840891e+00),
   (-6.935319e+00,-5.192438e+00),
   (9.690418e+00,-9.175753e+00),
   (3.448993e+00,2.119052e+00),
   (-7.769218e+00,4.647406e-01)>

#cast %eeWWA

example = clop test_data
```

{{out}}
The output shows the minimum distance and the two points separated
by that distance. (If the brute force algorithm were used, it would
have displayed the square of the distance.)

```txt

9.957310e-01: (
   (-2.132372e+00,2.358850e+00),
   (-1.745694e+00,3.276434e+00))

```



## VBA


```vb
Option Explicit

Private Type MyPoint
    X As Single
    Y As Single
End Type

Private Type MyPair
    p1 As MyPoint
    p2 As MyPoint
End Type

Sub Main()
Dim points() As MyPoint, i As Long, BF As MyPair, d As Single, Nb As Long
Dim T#
Randomize Timer
    Nb = 10
    Do
        ReDim points(1 To Nb)
        For i = 1 To Nb
            points(i).X = Rnd * Nb
            points(i).Y = Rnd * Nb
        Next
        d = 1000000000000#
T = Timer
        BF = BruteForce(points, d)
        Debug.Print "For " & Nb & " points, runtime : " & Timer - T & " sec."
        Debug.Print "point 1 : X:" & BF.p1.X & " Y:" & BF.p1.Y
        Debug.Print "point 2 : X:" & BF.p2.X & " Y:" & BF.p2.Y
        Debug.Print "dist : " & d
        Debug.Print "--------------------------------------------------"
        Nb = Nb * 10
    Loop While Nb <= 10000
End Sub

Private Function BruteForce(p() As MyPoint, mindist As Single) As MyPair
Dim i As Long, j As Long, d As Single, ClosestPair As MyPair
    For i = 1 To UBound(p) - 1
        For j = i + 1 To UBound(p)
            d = Dist(p(i), p(j))
            If d < mindist Then
                mindist = d
                ClosestPair.p1 = p(i)
                ClosestPair.p2 = p(j)
            End If
        Next
    Next
    BruteForce = ClosestPair
End Function

Private Function Dist(p1 As MyPoint, p2 As MyPoint) As Single
    Dist = Sqr((p1.X - p2.X) ^ 2 + (p1.Y - p2.Y) ^ 2)
End Function

```

{{out}}

```txt
For 10 points, runtime : 0 sec.
point 1 : X:7,199265 Y:7,690955
point 2 : X:7,16863 Y:7,681544
dist : 3,204883E-02
--------------------------------------------------
For 100 points, runtime : 0 sec.
point 1 : X:48,97898 Y:96,54872
point 2 : X:48,78981 Y:96,95755
dist : 0,4504737
--------------------------------------------------
For 1000 points, runtime : 0,44921875 sec.
point 1 : X:576,9511 Y:398,5834
point 2 : X:577,364 Y:398,3212
dist : 0,4891393
--------------------------------------------------
For 10000 points, runtime : 47,46875 sec.
point 1 : X:8982,698 Y:1154,133
point 2 : X:8984,763 Y:1152,822
dist : 2,445694
--------------------------------------------------
```



## Visual FoxPro


```vfp

CLOSE DATABASES ALL
CREATE CURSOR pairs(id I, xcoord B(6), ycoord B(6))
INSERT INTO pairs VALUES (1, 0.654682, 0.925557)
INSERT INTO pairs VALUES (2, 0.409382, 0.619391)
INSERT INTO pairs VALUES (3, 0.891663, 0.888594)
INSERT INTO pairs VALUES (4, 0.716629, 0.996200)
INSERT INTO pairs VALUES (5, 0.477721, 0.946355)
INSERT INTO pairs VALUES (6, 0.925092, 0.818220)
INSERT INTO pairs VALUES (7, 0.624291, 0.142924)
INSERT INTO pairs VALUES (8, 0.211332, 0.221507)
INSERT INTO pairs VALUES (9, 0.293786, 0.691701)
INSERT INTO pairs VALUES (10, 0.839186, 0.728260)

SELECT p1.id As id1, p2.id As id2, ;
(p1.xcoord-p2.xcoord)^2 + (p1.ycoord-p2.ycoord)^2 As dist2 ;
FROM pairs p1 JOIN pairs p2 ON p1.id < p2.id ORDER BY 3 INTO CURSOR tmp

GO TOP
? "Closest pair is " + TRANSFORM(id1) + " and " + TRANSFORM(id2) + "."
? "Distance is " + TRANSFORM(SQRT(dist2))

```

{{out}}

```txt

Visual FoxPro uses 1 based indexing,

Closest pair is 3 and 6.
Distance is 0.077910.

```



## XPL0

The brute force method is simpler than the recursive solution
and is perfectly adequate, even for a thousand points.


```XPL0
include c:\cxpl\codes;          \intrinsic 'code' declarations

proc ClosestPair(P, N);         \Show closest pair of points in array P
real P; int N;
real Dist2, MinDist2;
int I, J, SI, SJ;
[MinDist2:= 1e300;
for I:= 0 to N-2 do
    [for J:= I+1 to N-1 do
        [Dist2:= sq(P(I,0)-P(J,0)) + sq(P(I,1)-P(J,1));
        if Dist2 < MinDist2 then \squared distances are sufficient for compares
            [MinDist2:= Dist2;
            SI:= I;  SJ:= J;
            ];
        ];
    ];
IntOut(0, SI);  Text(0, " -- ");  IntOut(0, SJ);  CrLf(0);
RlOut(0, P(SI,0));  Text(0, ",");  RlOut(0, P(SI,1));
Text(0, " -- ");
RlOut(0, P(SJ,0));  Text(0, ",");  RlOut(0, P(SJ,1));
CrLf(0);
];

real Data;
[Format(1, 6);
Data:= [[0.654682, 0.925557],   \0 test data from BASIC examples
        [0.409382, 0.619391],   \1
        [0.891663, 0.888594],   \2
        [0.716629, 0.996200],   \3
        [0.477721, 0.946355],   \4
        [0.925092, 0.818220],   \5
        [0.624291, 0.142924],   \6
        [0.211332, 0.221507],   \7
        [0.293786, 0.691701],   \8
        [0.839186, 0.728260]];  \9
ClosestPair(Data, 10);
]
```


{{out}}

```txt

2 -- 5
0.891663,0.888594 -- 0.925092,0.818220

```



## zkl

An ugly solution in both time and space.

```zkl
class Point{
   fcn init(_x,_y){ var[const] x=_x, y=_y; }
   fcn distance(p){ (p.x-x).hypot(p.y-y) }
   fcn toString   { String("Point(",x,",",y,")") }
}

   // find closest two points using brute ugly force:
   // find all combinations of two points, measure distance, pick smallest
fcn closestPoints(points){
   pairs:=Utils.Helpers.pickNFrom(2,points);
   triples:=pairs.apply(fcn([(p1,p2)]){ T(p1,p2,p1.distance(p2)) });
   triples.reduce(fcn([(_,_,d1)]p1,[(_,_,d2)]p2){
      if(d1 < d2) p1 else p2
   });
}
```


```zkl
points:=T( 5.0, 9.0,  9.0, 3.0,
	   2.0, 0.0,  8.0, 4.0,
	   7.0, 4.0,  9.0, 10.0,
	   1.0, 9.0,  8.0, 2.0,
	   0.0, 10.0, 9.0, 6.0 ).pump(List,Void.Read,Point);

closestPoints(points).println(); //-->L(Point(8,4),Point(7,4),1)

points:=T( 0.654682, 0.925557, 0.409382, 0.619391,
           0.891663, 0.888594, 0.716629, 0.9962,
	   0.477721, 0.946355, 0.925092, 0.81822,
	   0.624291, 0.142924, 0.211332, 0.221507,
	   0.293786, 0.691701, 0.839186, 0.72826)
	   .pump(List,Void.Read,Point);
closestPoints(points).println();
```

{{out}}

```txt

L(Point(8,4),Point(7,4),1)
L(Point(0.925092,0.81822),Point(0.891663,0.888594),0.0779102)

```



## ZX Spectrum Basic

{{trans|BBC_BASIC}}

```zxbasic
10 DIM x(10): DIM y(10)
20 FOR i=1 TO 10
30 READ x(i),y(i)
40 NEXT i
50 LET min=1e30
60 FOR i=1 TO 9
70 FOR j=i+1 TO 10
80 LET p1=x(i)-x(j): LET p2=y(i)-y(j): LET dsq=p1*p1+p2*p2
90 IF dsq<min THEN LET min=dsq: LET mini=i: LET minj=j
100 NEXT j
110 NEXT i
120 PRINT "Closest pair is ";mini;" and ";minj;" at distance ";SQR min
130 STOP
140 DATA 0.654682,0.925557
150 DATA 0.409382,0.619391
160 DATA 0.891663,0.888594
170 DATA 0.716629,0.996200
180 DATA 0.477721,0.946355
190 DATA 0.925092,0.818220
200 DATA 0.624291,0.142924
210 DATA 0.211332,0.221507
220 DATA 0.293786,0.691701
230 DATA 0.839186,0.728260
```


[[Category:Geometry]]
