+++
title = "Trigonometric functions"
description = ""
date = 2019-10-14T11:33:18Z
aliases = []
[extra]
id = 2449
[taxonomies]
categories = []
tags = []
+++

{{task|Arithmetic operations}}
[[Category:Mathematics]]

;Task:
If your language has a library or built-in functions for trigonometry, show examples of:
::*   sine
::*   cosine
::*   tangent
::*   inverses   (of the above)

using the same angle in radians and degrees.

For the non-inverse functions,   each radian/degree pair should use arguments that evaluate to the same angle   (that is, it's not necessary to use the same angle for all three regular functions as long as the two sine calls use the same angle).

For the inverse functions,   use the same number and convert its answer to radians and degrees.

If your language does not have trigonometric functions available or only has some available,   write functions to calculate the functions based on any   [[wp:List of trigonometric identities|known approximation or identity]].





## ACL2

{{incomplete|ACL2}}
(This doesn't have the inverse functions; the Taylor series for those take too long to converge.)


```Lisp
(defun fac (n)
   (if (zp n)
       1
       (* n (fac (1- n)))))

(defconst *pi-approx*
   (/ 3141592653589793238462643383279
      (expt 10 30)))

(include-book "arithmetic-3/floor-mod/floor-mod" :dir :system)

(defun dgt-to-str (d)
   (case d
         (1 "1") (2 "2") (3 "3") (4 "4") (5 "5")
         (6 "6") (7 "7") (8 "8") (9 "9") (0 "0")))

(defmacro cat (&rest args)
   `(concatenate 'string ,@args))

(defun num-to-str-r (n)
   (if (zp n)
       ""
       (cat (num-to-str-r (floor n 10))
            (dgt-to-str (mod n 10)))))

(defun num-to-str (n)
   (cond ((= n 0) "0")
         ((< n 0) (cat "-" (num-to-str-r (- n))))
         (t (num-to-str-r n))))

(defun pad-with-zeros (places str lngth)
   (declare (xargs :measure (nfix (- places lngth))))
   (if (zp (- places lngth))
       str
       (pad-with-zeros places (cat "0" str) (1+ lngth))))

(defun as-decimal-str (r places)
   (let ((before (floor r 1))
         (after (floor (* (expt 10 places) (mod r 1)) 1)))
        (cat (num-to-str before)
             "."
             (let ((afterstr (num-to-str after)))
                  (pad-with-zeros places afterstr
                             (length afterstr))))))

(defun taylor-sine (theta terms term)
   (declare (xargs :measure (nfix (- terms term))))
   (if (zp (- terms term))
       0
       (+ (/ (*(expt -1 term) (expt theta (1+ (* 2 term))))
             (fac (1+ (* 2 term))))
          (taylor-sine theta terms (1+ term)))))

(defun sine (theta)
   (taylor-sine (mod theta (* 2 *pi-approx*))
                 20 0)) ; About 30 places of accuracy

(defun cosine (theta)
   (sine (+ theta (/ *pi-approx* 2))))

(defun tangent (theta)
   (/ (sine theta) (cosine theta)))

(defun rad->deg (rad)
   (* 180 (/ rad *pi-approx*)))

(defun deg->rad (deg)
   (* *pi-approx* (/ deg 180)))

(defun trig-demo ()
   (progn$ (cw "sine of pi / 4 radians:    ")
           (cw (as-decimal-str (sine (/ *pi-approx* 4)) 20))
           (cw "~%sine of 45 degrees:        ")
           (cw (as-decimal-str (sine (deg->rad 45)) 20))
           (cw "~%cosine of pi / 4 radians:  ")
           (cw (as-decimal-str (cosine (/ *pi-approx* 4)) 20))
           (cw "~%tangent of pi / 4 radians: ")
           (cw (as-decimal-str (tangent (/ *pi-approx* 4)) 20))
           (cw "~%")))
```



```txt
sine of pi / 4 radians:    0.70710678118654752440
sine of 45 degrees:        0.70710678118654752440
cosine of pi / 4 radians:  0.70710678118654752440
tangent of pi / 4 radians: 0.99999999999999999999
```



## ActionScript

Actionscript supports basic trigonometric and inverse trigonometric functions via the Math class, including the atan2 function, but not the hyperbolic functions.

```ActionScript
trace("Radians:");
trace("sin(Pi/4) = ", Math.sin(Math.PI/4));
trace("cos(Pi/4) = ", Math.cos(Math.PI/4));
trace("tan(Pi/4) = ", Math.tan(Math.PI/4));
trace("arcsin(0.5) = ", Math.asin(0.5));
trace("arccos(0.5) = ", Math.acos(0.5));
trace("arctan(0.5) = ", Math.atan(0.5));
trace("arctan2(-1,-2) = ", Math.atan2(-1,-2));
trace("\nDegrees")
trace("sin(45) = ", Math.sin(45 * Math.PI/180));
trace("cos(45) = ", Math.cos(45 * Math.PI/180));
trace("tan(45) = ", Math.tan(45 * Math.PI/180));
trace("arcsin(0.5) = ", Math.asin(0.5)*180/Math.PI);
trace("arccos(0.5) = ", Math.acos(0.5)*180/Math.PI);
trace("arctan(0.5) = ", Math.atan(0.5)*180/Math.PI);
trace("arctan2(-1,-2) = ", Math.atan2(-1,-2)*180/Math.PI);

```



## Ada

Ada provides library trig functions which default to radians along with corresponding library functions for which the cycle can be specified.

The examples below specify the cycle for degrees and for radians.

The output of the inverse trig functions is in units of the specified cycle (degrees or radians).

```ada
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Float_Text_Io; use Ada.Float_Text_Io;
with Ada.Text_IO; use Ada.Text_IO;

procedure Trig is
   Degrees_Cycle : constant Float := 360.0;
   Radians_Cycle : constant Float := 2.0 * Ada.Numerics.Pi;
   Angle_Degrees : constant Float := 45.0;
   Angle_Radians : constant Float := Ada.Numerics.Pi / 4.0;
   procedure Put (V1, V2 : Float) is
   begin
      Put (V1, Aft => 5, Exp => 0);
      Put (" ");
      Put (V2, Aft => 5, Exp => 0);
      New_Line;
   end Put;
begin
   Put (Sin (Angle_Degrees, Degrees_Cycle),
        Sin (Angle_Radians, Radians_Cycle));
   Put (Cos (Angle_Degrees, Degrees_Cycle),
        Cos (Angle_Radians, Radians_Cycle));
   Put (Tan (Angle_Degrees, Degrees_Cycle),
        Tan (Angle_Radians, Radians_Cycle));
   Put (Cot (Angle_Degrees, Degrees_Cycle),
        Cot (Angle_Radians, Radians_Cycle));
   Put (ArcSin (Sin (Angle_Degrees, Degrees_Cycle), Degrees_Cycle),
        ArcSin (Sin (Angle_Radians, Radians_Cycle), Radians_Cycle));
   Put (Arccos (Cos (Angle_Degrees, Degrees_Cycle), Degrees_Cycle),
        Arccos (Cos (Angle_Radians, Radians_Cycle), Radians_Cycle));
   Put (Arctan (Y => Tan (Angle_Degrees, Degrees_Cycle)),
        Arctan (Y => Tan (Angle_Radians, Radians_Cycle)));
   Put (Arccot (X => Cot (Angle_Degrees, Degrees_Cycle)),
        Arccot (X => Cot (Angle_Degrees, Degrees_Cycle)));
end Trig;
```


{{out}}

```txt

 0.70711  0.70711
 0.70711  0.70711
 1.00000  1.00000
 1.00000  1.00000
45.00000  0.78540
45.00000  0.78540
45.00000  0.78540
45.00000  0.78540

```



## ALGOL 68

{{trans|C}}

{{works with|ALGOL 68|Standard - no extensions to language used}}

{{works with|ALGOL 68G|Any - tested with release mk15-0.8b.fc9.i386}}

{{works with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release 1.8.8d.fc9.i386}}

```algol68
main:(
  REAL pi = 4 * arc tan(1);
  # Pi / 4 is 45 degrees. All answers should be the same. #
  REAL radians = pi / 4;
  REAL degrees = 45.0;
  REAL temp;
  # sine #
  print((sin(radians), " ", sin(degrees * pi / 180), new line));
  # cosine #
  print((cos(radians), " ", cos(degrees * pi / 180), new line));
  # tangent #
  print((tan(radians), " ", tan(degrees * pi / 180), new line));
  # arcsine #
  temp := arc sin(sin(radians));
  print((temp, " ", temp * 180 / pi, new line));
  # arccosine #
  temp := arc cos(cos(radians));
  print((temp, " ", temp * 180 / pi, new line));
  # arctangent #
  temp := arc tan(tan(radians));
  print((temp, " ", temp * 180 / pi, new line))
)
```

{{out}}

```txt

+.707106781186548e +0  +.707106781186548e +0
+.707106781186548e +0  +.707106781186548e +0
+.100000000000000e +1  +.100000000000000e +1
+.785398163397448e +0  +.450000000000000e +2
+.785398163397448e +0  +.450000000000000e +2
+.785398163397448e +0  +.450000000000000e +2

```



## ALGOL W


```algolw
begin
    % Algol W only supplies sin, cos and arctan as standard. We can define    %
    % arcsin, arccos and tan functions using these. The standard functions    %
    % use radians so we also provide versions that use degrees                %

    % convert degrees to radians   %
    real procedure toRadians( real value x ) ; pi * ( x / 180 );
    % convert radians to degrees   %
    real procedure toDegrees( real value x ) ; 180 * ( x / pi );
    % tan of an angle in radians   %
    real procedure tan( real value x ) ; sin( x ) / cos( x );
    % arcsin in radians            %
    real procedure arcsin( real value x ) ; arctan( x / sqrt( 1 - ( x * x ) ) );
    % arccos in radians            %
    real procedure arccos( real value x ) ; arctan( sqrt( 1 - ( x * x ) ) / x );
    % sin of an angle in degrees   %
    real procedure sinD( real value x ) ; sin( toRadians( x ) );
    % cos of an angle in degrees   %
    real procedure cosD( real value x ) ; cos( toRadians( x ) );
    % tan of an angle in degrees   %
    real procedure tanD( real value x ) ; tan( toRadians( x ) );
    % arctan in degrees            %
    real procedure arctanD( real value x ) ; toDegrees( arctan( x ) );
    % arcsin in degrees            %
    real procedure arcsinD( real value x ) ; toDegrees( arcsin( x ) );
    % arccos in degrees            %
    real procedure arccosD( real value x ) ; toDegrees( arccos( x ) );


    % test the procedures %
    begin

        real piOver4, piOver3, oneOverRoot2, root3Over2;
        piOver3 := pi / 3; piOver4  := pi / 4;
        oneOverRoot2 := 1.0 / sqrt( 2 ); root3Over2 := sqrt( 3 ) / 2;


        r_w := 12; r_d := 5; r_format := "A"; s_w := 0; % set output format   %

        write( "PI/4: ", piOver4, " 1/root(2): ", oneOverRoot2 );
        write();
        write( "sin 45 degrees: ", sinD( 45 ), " sin pi/4 radians: ", sin( piOver4 ) );
        write( "cos 45 degrees: ", cosD( 45 ), " cos pi/4 radians: ", cos( piOver4 ) );
        write( "tan 45 degrees: ", tanD( 45 ), " tan pi/4 radians: ", tan( piOver4 ) );
        write();
        write( "arcsin( sin( pi/4 radians ) ): ", arcsin( sin( piOver4 ) ) );
        write( "arccos( cos( pi/4 radians ) ): ", arccos( cos( piOver4 ) ) );
        write( "arctan( tan( pi/4 radians ) ): ", arctan( tan( piOver4 ) ) );
        write();
        write( "PI/3: ", piOver4, " root(3)/2: ", root3Over2 );
        write();
        write( "sin 60 degrees: ", sinD( 60 ), " sin pi/3 radians: ", sin( piOver3 ) );
        write( "cos 60 degrees: ", cosD( 60 ), " cos pi/3 radians: ", cos( piOver3 ) );
        write( "tan 60 degrees: ", tanD( 60 ), " tan pi/3 radians: ", tan( piOver3 ) );
        write();
        write( "arcsin( sin( 60 degrees ) ): ", arcsinD( sinD( 60 ) ) );
        write( "arccos( cos( 60 degrees ) ): ", arccosD( cosD( 60 ) ) );
        write( "arctan( tan( 60 degrees ) ): ", arctanD( tanD( 60 ) ) );

    end

end.
```

{{out}}

```txt

PI/4:      0.78539 1/root(2):      0.70710

sin 45 degrees:      0.70710 sin pi/4 radians:      0.70710
cos 45 degrees:      0.70710 cos pi/4 radians:      0.70710
tan 45 degrees:      1.00000 tan pi/4 radians:      1.00000

arcsin( sin( pi/4 radians ) ):      0.78539
arccos( cos( pi/4 radians ) ):      0.78539
arctan( tan( pi/4 radians ) ):      0.78539

PI/3:      0.78539 root(3)/2:      0.86602

sin 60 degrees:      0.86602 sin pi/3 radians:      0.86602
cos 60 degrees:      0.50000 cos pi/3 radians:      0.50000
tan 60 degrees:      1.73205 tan pi/3 radians:      1.73205

arcsin( sin( 60 degrees ) ):     60.00000
arccos( cos( 60 degrees ) ):     60.00000
arctan( tan( 60 degrees ) ):     60.00000

```



## Arturo


{{trans|C}}


```arturo
pi  4*$(atan 1)

radians pi/4
degrees 45.0

"sine"
print $(sin radians) + " " + $(sin degrees*pi/180)

"cosine"
print $(cos radians) + " " + $(cos degrees*pi/180)

"tangent"
print $(tan radians) + " " + $(tan degrees*pi/180)

"arcsine"
print $(asin $(sin radians)) + " " +$(asin $(sin radians))*180/pi

"arccosine"
print $(acos $(cos radians)) + " " +$(acos $(cos radians))*180/pi

"arctangent"
print $(atan $(tan radians)) + " " +$(atan $(tan radians))*180/pi
```


{{out}}


```txt
sine
0.707107 0.707107
cosine
0.707107 0.707107
tangent
1 1
arcsine
0.785398 45
arccosine
0.785398 45
arctangent
0.785398 45
```



## AutoHotkey

{{trans|C}}

```AutoHotkey
pi := 4 * atan(1)
  radians := pi / 4
  degrees := 45.0
  result .= "`n" . sin(radians) . "     " . sin(degrees * pi / 180)
  result .= "`n" . cos(radians) . "     " . cos(degrees * pi / 180)
  result .= "`n" . tan(radians) . "     " . tan(degrees * pi / 180)

  temp := asin(sin(radians))
  result .= "`n" . temp . "     " . temp * 180 / pi

  temp := acos(cos(radians))
  result .= "`n" . temp . "     " . temp * 180 / pi

  temp := atan(tan(radians))
  result .= "`n" . temp . "     " . temp * 180 / pi

msgbox % result
/* output
---------------------------
trig.ahk
---------------------------
0.707107     0.707107
0.707107     0.707107
1.000000     1.000000
0.785398     45.000000
0.785398     45.000000
0.785398     45.000000
*/
```



## Autolisp

Autolisp provides <b>(sin x) (cos x) (tan x)</b> and <b>(atan x)</b>.
Function arguments are expressed in radians.

```Autolisp

(defun rad_to_deg (rad)(* 180.0 (/ rad PI)))
(defun deg_to_rad (deg)(* PI (/ deg 180.0)))

(defun asin (x)
  (cond
    ((and(> x -1.0)(< x 1.0)) (atan (/ x (sqrt (- 1.0 (* x x))))))
    ((= x -1.0) (* -1.0 (/ pi 2)))
    ((= x 1) (/ pi 2))
  )
)

(defun acos (x)
  (cond
    ((and(>= x -1.0)(<= x 1.0)) (-(* pi 0.5) (asin x)))
  )
)

(list
(list "cos PI/6" (cos (/ pi 6)) "cos 30 deg" (cos (deg_to_rad 30)))
(list "sin PI/4" (sin (/ pi 4)) "sin 45 deg" (sin (deg_to_rad 45)))
(list "tan PI/3" (tan (/ pi 3))"tan 60 deg" (tan (deg_to_rad 60)))
(list "asin 1 rad" (asin 1.0) "asin 1 rad (deg)" (rad_to_deg (asin 1.0)))
(list "acos 1/2 rad" (acos (/ 1 2.0)) "acos 1/2 rad (deg)" (rad_to_deg (acos (/ 1 2.0))))
(list "atan pi/12" (atan (/ pi 12)) "atan 15 deg" (rad_to_deg(atan(deg_to_rad 15))))
)

```

{{out}}

```txt

(("cos PI/6 rad" 0.866025 "cos 30 deg" 0.866025)
 ("sin PI/4 rad" 0.707107 "sin 45 deg" 0.707107)
 ("tan PI/3 rad" 1.73205 " tan 60 deg" 1.73205)
 ("asin 1 rad" 1.57080 "asin 1 rad (deg)" 90.0000)
 ("acos 1/2 rad" 1.04720 "acos 1/2 rad" 60.0000)
 ("atan pi/12 rad" 0.256053 "atan 15 deg" 14.6707))

```



## AWK

Awk only provides <tt>sin()</tt>, <tt>cos()</tt> and <tt>atan2()</tt>, the three bare necessities for trigonometry. They all use radians. To calculate the other functions, we use these three trigonometric identities:

{|class="wikitable"
! tangent
! arcsine
! arccosine
|-
| <math>\tan \theta = \frac{\sin \theta}{\cos \theta}</math>
| <math>\tan(\arcsin y) = \frac{y}{\sqrt{1 - y^2}}</math>
| <math>\tan(\arccos x) = \frac{\sqrt{1 - x^2}}{x}</math>
|}

With the magic of <tt>atan2()</tt>, arcsine of ''y'' is just <tt>atan2(y, sqrt(1 - y * y))</tt>, and arccosine of ''x'' is just <tt>atan2(sqrt(1 - x * x), x)</tt>. This magic handles the angles ''arcsin(-1)'', ''arcsin 1'' and ''arccos 0'' that have no tangent. This magic also picks the angle in the correct range, so ''arccos(-1/2)'' is ''2*pi/3'' and not some wrong answer like ''-pi/3'' (though ''tan(2*pi/3) = tan(-pi/3) = -sqrt(3)''.)

<tt>atan2(y, x)</tt> actually computes the angle of the point ''(x, y)'', in the range ''[-pi, pi]''. When x > 0, this angle is the principle arctangent of ''y/x'', in the range ''(-pi/2, pi/2)''. The calculations for arcsine and arccosine use points on the unit circle at ''x<sup>2</sup> + y<sup>2</sup> = 1''. To calculate arcsine in the range ''[-pi/2, pi/2]'', we take the angle of points on the half-circle ''x = sqrt(1 - y<sup>2</sup>)''. To calculate arccosine in the range ''[0, pi]'', we take the angle of points on the half-circle ''y = sqrt(1 - x<sup>2</sup>)''.


```awk
# tan(x) = tangent of x
function tan(x) {
	return sin(x) / cos(x)
}

# asin(y) = arcsine of y, domain [-1, 1], range [-pi/2, pi/2]
function asin(y) {
	return atan2(y, sqrt(1 - y * y))
}

# acos(x) = arccosine of x, domain [-1, 1], range [0, pi]
function acos(x) {
	return atan2(sqrt(1 - x * x), x)
}

# atan(y) = arctangent of y, range (-pi/2, pi/2)
function atan(y) {
	return atan2(y, 1)
}

BEGIN {
	pi = atan2(0, -1)
	degrees = pi / 180

	print "Using radians:"
	print "  sin(-pi / 6) =", sin(-pi / 6)
	print "  cos(3 * pi / 4) =", cos(3 * pi / 4)
	print "  tan(pi / 3) =", tan(pi / 3)
	print "  asin(-1 / 2) =", asin(-1 / 2)
	print "  acos(-sqrt(2) / 2) =", acos(-sqrt(2) / 2)
	print "  atan(sqrt(3)) =", atan(sqrt(3))

	print "Using degrees:"
	print "  sin(-30) =", sin(-30 * degrees)
	print "  cos(135) =", cos(135 * degrees)
	print "  tan(60) =", tan(60 * degrees)
	print "  asin(-1 / 2) =", asin(-1 / 2) / degrees
	print "  acos(-sqrt(2) / 2) =", acos(-sqrt(2) / 2) / degrees
	print "  atan(sqrt(3)) =", atan(sqrt(3)) / degrees
}
```


{{out}}

```txt
Using radians:
  sin(-pi / 6) = -0.5
  cos(3 * pi / 4) = -0.707107
  tan(pi / 3) = 1.73205
  asin(-1 / 2) = -0.523599
  acos(-sqrt(2) / 2) = 2.35619
  atan(sqrt(3)) = 1.0472
Using degrees:
  sin(-30) = -0.5
  cos(135) = -0.707107
  tan(60) = 1.73205
  asin(-1 / 2) = -30
  acos(-sqrt(2) / 2) = 135
  atan(sqrt(3)) = 60
```



## Axe

Axe implements sine, cosine, and inverse tangent natively. One period is [0, 256) and the results are [-127, 127] for maximum precision.

The inverse tangent takes dX and dY parameters, rather than a single argument. This is because it is most often used to calculate angles.


```axe
Disp sin(43)▶Dec,i
Disp cos(43)▶Dec,i
Disp tan⁻¹(10,10)▶Dec,i
```


{{out}}

```txt
  113
   68
   32
```


Below is the worked out math.

On a period of 256, an argument of 43 is equivalent to <math>\frac{\pi}{3} * \frac{128}{\pi}</math>.

Furthermore, <math>127*\frac{\sqrt{3}}{2} \approx 111</math> and <math>127*\frac{1}{2} \approx 64</math>.

So <math>\sin{43} \approx 113</math> and <math>\cos{43} \approx 68</math>. Axe uses approximations to calculate the trigonometric functions.

dX and dY values of 10 mean that the angle between them should be <math>\frac{\pi}{4}</math>. Indeed, the result <math>\tan^{-1}{(10, 10)} = 32 = \frac{128}{4}</math>.


## BaCon


```qbasic
' Trigonometric functions in BaCon use Radians for input values
' The RAD() function converts from degrees to radians

FOR v$ IN "0, 10, 45, 90, 190.5"
    d = VAL(v$) * 1.0
    r = RAD(d) * 1.0

    PRINT "Sine: ", d, " degrees (or ", r, " radians) is ", SIN(r)
    PRINT "Cosine: ", d, " degrees (or ", r, " radians) is ", COS(r)
    PRINT "Tangent: ", d, " degrees (or ", r, " radians) is ", TAN(r)
    PRINT
    PRINT "Arc Sine: ", SIN(r), " is ", DEG(ASIN(SIN(r))), " degrees (or ", ASIN(SIN(r)), " radians)"
    PRINT "Arc CoSine: ", COS(r), " is ", DEG(ACOS(COS(r))), " degrees (or ", ACOS(COS(r)), " radians)"
    PRINT "Arc Tangent: ", TAN(r), " is ", DEG(ATN(TAN(r))), " degrees (or ", ATN(TAN(r)), " radians)"
    PRINT
NEXT
```


{{out}}

```txt
prompt$ bacon -q trigonometric-functions.bac
...
Done, program 'trigonometric-functions' ready.

prompt$ ./trigonometric-functions
Sine: 0 degrees (or 0 radians) is 0
Cosine: 0 degrees (or 0 radians) is 1
Tangent: 0 degrees (or 0 radians) is 0

Arc Sine: 0 is 0 degrees (or 0 radians)
Arc CoSine: 1 is 0 degrees (or 0 radians)
Arc Tangent: 0 is 0 degrees (or 0 radians)

Sine: 10 degrees (or 0.174533 radians) is 0.173648
Cosine: 10 degrees (or 0.174533 radians) is 0.984808
Tangent: 10 degrees (or 0.174533 radians) is 0.176327

Arc Sine: 0.173648 is 10 degrees (or 0.174533 radians)
Arc CoSine: 0.984808 is 10 degrees (or 0.174533 radians)
Arc Tangent: 0.176327 is 10 degrees (or 0.174533 radians)

Sine: 45 degrees (or 0.785398 radians) is 0.707107
Cosine: 45 degrees (or 0.785398 radians) is 0.707107
Tangent: 45 degrees (or 0.785398 radians) is 1

Arc Sine: 0.707107 is 45 degrees (or 0.785398 radians)
Arc CoSine: 0.707107 is 45 degrees (or 0.785398 radians)
Arc Tangent: 1 is 45 degrees (or 0.785398 radians)

Sine: 90 degrees (or 1.5708 radians) is 1
Cosine: 90 degrees (or 1.5708 radians) is 6.12323e-17
Tangent: 90 degrees (or 1.5708 radians) is 16331239353195370

Arc Sine: 1 is 90 degrees (or 1.5708 radians)
Arc CoSine: 6.12323e-17 is 90 degrees (or 1.5708 radians)
Arc Tangent: 16331239353195370 is 90 degrees (or 1.5708 radians)

Sine: 190.5 degrees (or 3.32485 radians) is -0.182236
Cosine: 190.5 degrees (or 3.32485 radians) is -0.983255
Tangent: 190.5 degrees (or 3.32485 radians) is 0.185339

Arc Sine: -0.182236 is -10.5 degrees (or -0.18326 radians)
Arc CoSine: -0.983255 is 169.5 degrees (or 2.95833 radians)
Arc Tangent: 0.185339 is 10.5 degrees (or 0.18326 radians)
```



## BASIC

{{works with|QuickBasic|4.5}}
QuickBasic 4.5 does not have arcsin and arccos built in. They are defined by identities found [[wp:Arctan#Relationships_among_the_inverse_trigonometric_functions|here]].

```qbasic
pi = 3.141592653589793#
radians = pi / 4 'a.k.a. 45 degrees
degrees = 45 * pi / 180 'convert 45 degrees to radians once
PRINT SIN(radians) + " " + SIN(degrees) 'sine
PRINT COS(radians) + " " + COS(degrees) 'cosine
PRINT TAN(radians) + " " + TAN (degrees) 'tangent
'arcsin
thesin = SIN(radians)
arcsin = ATN(thesin / SQR(1 - thesin ^ 2))
PRINT arcsin + " " + arcsin * 180 / pi
'arccos
thecos = COS(radians)
arccos = 2 * ATN(SQR(1 - thecos ^ 2) / (1 + thecos))
PRINT arccos + " " + arccos * 180 / pi
PRINT ATN(TAN(radians)) + " " + ATN(TAN(radians)) * 180 / pi 'arctan
```


=
## BBC BASIC
=

```bbcbasic
      @% = &90F : REM set column width

      angle_radians = PI/5
      angle_degrees = 36

      PRINT SIN(angle_radians), SIN(RAD(angle_degrees))
      PRINT COS(angle_radians), COS(RAD(angle_degrees))
      PRINT TAN(angle_radians), TAN(RAD(angle_degrees))

      number = 0.6

      PRINT ASN(number), DEG(ASN(number))
      PRINT ACS(number), DEG(ACS(number))
      PRINT ATN(number), DEG(ATN(number))
```


==={{header|IS-BASIC}}===
<lang IS-BASIC>100 LET DG=DEG(PI/4)
110 OPTION ANGLE DEGREES
120 PRINT SIN(DG)
130 PRINT COS(DG)
140 PRINT TAN(DG)
150 PRINT ASIN(SIN(DG))
160 PRINT ACOS(COS(DG))
170 PRINT ATN(TAN(DG))
180 LET RD=RAD(45)
190 OPTION ANGLE RADIANS
200 PRINT SIN(RD)
210 PRINT COS(RD)
220 PRINT TAN(RD)
230 PRINT ASIN(SIN(RD))
240 PRINT ACOS(COS(RD))
250 PRINT ATN(TAN(RD))
```



## bc

{{libheader|bc -l}}
{{trans|AWK}}

```bc
/* t(x) = tangent of x */
define t(x) {
	return s(x) / c(x)
}

/* y(y) = arcsine of y, domain [-1, 1], range [-pi/2, pi/2] */
define y(y) {
	/* Handle angles with no tangent. */
	if (y == -1) return -2 * a(1)  /* -pi/2 */
	if (y == 1) return 2 * a(1)    /* pi/2 */

	/* Tangent of angle is y / x, where x^2 + y^2 = 1. */
	return a(y / sqrt(1 - y * y))
}

/* x(x) = arccosine of x, domain [-1, 1], range [0, pi] */
define x(x) {
	auto a

	/* Handle angle with no tangent. */
	if (x == 0) return 2 * a(1)  /* pi/2 */

	/* Tangent of angle is y / x, where x^2 + y^2 = 1. */
	a = a(sqrt(1 - x * x) / x)
	if (a < 0) {
		return a + 4 * a(1)  /* add pi */
	} else {
		return a
	}
}


scale = 50
p = 4 * a(1)  /* pi */
d = p / 180   /* one degree in radians */

"Using radians:
"
"  sin(-pi / 6) = "; s(-p / 6)
"  cos(3 * pi / 4) = "; c(3 * p / 4)
"  tan(pi / 3) = "; t(p / 3)
"  asin(-1 / 2) = "; y(-1 / 2)
"  acos(-sqrt(2) / 2) = "; x(-sqrt(2) / 2)
"  atan(sqrt(3)) = "; a(sqrt(3))

"Using degrees:
"
"  sin(-30) = "; s(-30 * d)
"  cos(135) = "; c(135 * d)
"  tan(60) = "; t(60 * d)
"  asin(-1 / 2) = "; y(-1 / 2) / d
"  acos(-sqrt(2) / 2) = "; x(-sqrt(2) / 2) / d
"  atan(sqrt(3)) = "; a(sqrt(3)) / d

quit
```


{{out}}

```txt
Using radians:
  sin(-pi / 6) = -.49999999999999999999999999999999999999999999999999
  cos(3 * pi / 4) = -.70710678118654752440084436210484903928483593768845
  tan(pi / 3) = 1.73205080756887729352744634150587236694280525381032
  asin(-1 / 2) = -.52359877559829887307710723054658381403286156656251
  acos(-sqrt(2) / 2) = 2.35619449019234492884698253745962716314787704953131
  atan(sqrt(3)) = 1.04719755119659774615421446109316762806572313312503
Using degrees:
  sin(-30) = -.49999999999999999999999999999999999999999999999981
  cos(135) = -.70710678118654752440084436210484903928483593768778
  tan(60) = 1.73205080756887729352744634150587236694280525380865
  asin(-1 / 2) = -30.00000000000000000000000000000000000000000000001203
  acos(-sqrt(2) / 2) = 135.00000000000000000000000000000000000000000000005500
  atan(sqrt(3)) = 60.00000000000000000000000000000000000000000000002463
```



## C



```c
#include <math.h>
#include <stdio.h>

int main() {
  double pi = 4 * atan(1);
  /*Pi / 4 is 45 degrees. All answers should be the same.*/
  double radians = pi / 4;
  double degrees = 45.0;
  double temp;
  /*sine*/
  printf("%f %f\n", sin(radians), sin(degrees * pi / 180));
  /*cosine*/
  printf("%f %f\n", cos(radians), cos(degrees * pi / 180));
  /*tangent*/
  printf("%f %f\n", tan(radians), tan(degrees * pi / 180));
  /*arcsine*/
  temp = asin(sin(radians));
  printf("%f %f\n", temp, temp * 180 / pi);
  /*arccosine*/
  temp = acos(cos(radians));
  printf("%f %f\n", temp, temp * 180 / pi);
  /*arctangent*/
  temp = atan(tan(radians));
  printf("%f %f\n", temp, temp * 180 / pi);

  return 0;
}
```


{{out}}

```txt

0.707107 0.707107
0.707107 0.707107
1.000000 1.000000
0.785398 45.000000
0.785398 45.000000
0.785398 45.000000

```



## C++


```cpp
#include <iostream>
#include <cmath>

#ifdef M_PI // defined by all POSIX systems and some non-POSIX ones
double const pi = M_PI;
#else
double const pi = 4*std::atan(1);
#endif

double const degree = pi/180;

int main()
{
  std::cout << "
###  radians
\n";
  std::cout << "sin(pi/3) = " << std::sin(pi/3) << "\n";
  std::cout << "cos(pi/3) = " << std::cos(pi/3) << "\n";
  std::cout << "tan(pi/3) = " << std::tan(pi/3) << "\n";
  std::cout << "arcsin(1/2) = " << std::asin(0.5) << "\n";
  std::cout << "arccos(1/2) = " << std::acos(0.5) << "\n";
  std::cout << "arctan(1/2) = " << std::atan(0.5) << "\n";

  std::cout << "\n
###  degrees
\n";
  std::cout << "sin(60°) = " << std::sin(60*degree) << "\n";
  std::cout << "cos(60°) = " << std::cos(60*degree) << "\n";
  std::cout << "tan(60°) = " << std::tan(60*degree) << "\n";
  std::cout << "arcsin(1/2) = " << std::asin(0.5)/degree << "°\n";
  std::cout << "arccos(1/2) = " << std::acos(0.5)/degree << "°\n";
  std::cout << "arctan(1/2) = " << std::atan(0.5)/degree << "°\n";

  return 0;
}
```


## C#

```c#
using System;

namespace RosettaCode {
    class Program {
        static void Main(string[] args) {
            Console.WriteLine("
###  radians
");
            Console.WriteLine("sin (pi/3) = {0}", Math.Sin(Math.PI / 3));
            Console.WriteLine("cos (pi/3) = {0}", Math.Cos(Math.PI / 3));
            Console.WriteLine("tan (pi/3) = {0}", Math.Tan(Math.PI / 3));
            Console.WriteLine("arcsin (1/2) = {0}", Math.Asin(0.5));
            Console.WriteLine("arccos (1/2) = {0}", Math.Acos(0.5));
            Console.WriteLine("arctan (1/2) = {0}", Math.Atan(0.5));
            Console.WriteLine("");
            Console.WriteLine("
###  degrees
");
            Console.WriteLine("sin (60) = {0}", Math.Sin(60 * Math.PI / 180));
            Console.WriteLine("cos (60) = {0}", Math.Cos(60 * Math.PI / 180));
            Console.WriteLine("tan (60) = {0}", Math.Tan(60 * Math.PI / 180));
            Console.WriteLine("arcsin (1/2) = {0}", Math.Asin(0.5) * 180/ Math.PI);
            Console.WriteLine("arccos (1/2) = {0}", Math.Acos(0.5) * 180 / Math.PI);
            Console.WriteLine("arctan (1/2) = {0}", Math.Atan(0.5) * 180 / Math.PI);

            Console.ReadLine();
        }
    }
}
```


## Clojure


{{trans|fortran}}


```lisp
(ns user
  (:require [clojure.contrib.generic.math-functions :as generic]))

;(def pi Math/PI)
(def pi (* 4 (atan 1)))
(def dtor (/ pi 180))
(def rtod (/ 180 pi))
(def radians (/ pi 4))
(def degrees 45)

(println (str (sin radians) " " (sin (* degrees dtor))))
(println (str (cos radians) " " (cos (* degrees dtor))))
(println (str (tan radians) " " (tan (* degrees dtor))))
(println (str (asin (sin radians) ) " " (* (asin (sin (* degrees dtor))) rtod)))
(println (str (acos (cos radians) ) " " (* (acos (cos (* degrees dtor))) rtod)))
(println (str (atan (tan radians) ) " " (* (atan (tan (* degrees dtor))) rtod)))
```


{{out}} (matches that of Java)
 0.7071067811865475 0.7071067811865475
 0.7071067811865476 0.7071067811865476
 0.9999999999999999 0.9999999999999999
 0.7853981633974482 44.99999999999999
 0.7853981633974483 45.0
 0.7853981633974483 45.0


## COBOL



```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Trig.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  Pi-Third   USAGE COMP-2.
       01  Degree     USAGE COMP-2.

       01  60-Degrees USAGE COMP-2.

       01  Result     USAGE COMP-2.

       PROCEDURE DIVISION.
           COMPUTE Pi-Third = FUNCTION PI / 3

           DISPLAY "Radians:"
           DISPLAY "  Sin(π / 3)  = " FUNCTION SIN(Pi-Third)
           DISPLAY "  Cos(π / 3)  = " FUNCTION COS(Pi-Third)
           DISPLAY "  Tan(π / 3)  = " FUNCTION TAN(Pi-Third)
           DISPLAY "  Asin(0.5)   = " FUNCTION ASIN(0.5)
           DISPLAY "  Acos(0.5)   = " FUNCTION ACOS(0.5)
           DISPLAY "  Atan(0.5)   = " FUNCTION ATAN(0.5)

           COMPUTE Degree = FUNCTION PI / 180
           COMPUTE 60-Degrees = Degree * 60

           DISPLAY "Degrees:"
           DISPLAY "  Sin(60°)  = " FUNCTION SIN(60-Degrees)
           DISPLAY "  Cos(60°)  = " FUNCTION COS(60-Degrees)
           DISPLAY "  Tan(60°)  = " FUNCTION TAN(60-Degrees)
           COMPUTE Result = FUNCTION ASIN(0.5) / 60
           DISPLAY "  Asin(0.5) = " Result
           COMPUTE Result = FUNCTION ACOS(0.5) / 60
           DISPLAY "  Acos(0.5) = " Result
           COMPUTE Result = FUNCTION ATAN(0.5) / 60
           DISPLAY "  Atan(0.5) = " Result

           GOBACK
           .
```


{{out}}

```txt

Radians:
  Sin(π / 3)  = +0.86602540368613976
  Cos(π / 3)  = +0.50000000017025856
  Tan(π / 3)  = 1.732050806782486241
  Asin(0.5) = +0.52359877559829897
  Acos(0.5) = 1.04719755119659785
  Atan(0.5) = +0.52359877559829897
Degrees:
  Sin(60°)  = +0.86602538768613932
  Cos(60°)  = +0.50000002788307131
  Tan(60°)  = 1.732050678782493636
  Asin(0.5) = 0.008726645999999999
  Acos(0.5) = 0.017453291999999999
  Atan(0.5) = 0.007727460000000000

```



## Common Lisp


```lisp
(defun deg->rad (x) (* x (/ pi 180)))
(defun rad->deg (x) (* x (/ 180 pi)))

(mapc (lambda (x) (format t "~s => ~s~%" x (eval x)))
  '((sin (/ pi 4))
    (sin (deg->rad 45))
    (cos (/ pi 6))
    (cos (deg->rad 30))
    (tan (/ pi 3))
    (tan (deg->rad 60))
    (asin 1)
    (rad->deg (asin 1))
    (acos 1/2)
    (rad->deg (acos 1/2))
    (atan 15)
    (rad->deg (atan 15))))
```



## D

{{trans|C}}

```d
void main() {
    import std.stdio, std.math;

    enum degrees = 45.0L;
    enum t0 = degrees * PI / 180.0L;
    writeln("Reference:  0.7071067811865475244008");
    writefln("Sine:       %.20f  %.20f", PI_4.sin, t0.sin);
    writefln("Cosine:     %.20f  %.20f", PI_4.cos, t0.cos);
    writefln("Tangent:    %.20f  %.20f", PI_4.tan, t0.tan);

    writeln;
    writeln("Reference:  0.7853981633974483096156");
    immutable real t1 = PI_4.sin.asin;
    writefln("Arcsine:    %.20f %.20f", t1, t1 * 180.0L / PI);

    immutable real t2 = PI_4.cos.acos;
    writefln("Arccosine:  %.20f %.20f", t2, t2 * 180.0L / PI);

    immutable real t3 = PI_4.tan.atan;
    writefln("Arctangent: %.20f %.20f", t3, t3 * 180.0L / PI);
}
```

{{out}}

```txt
Reference:  0.7071067811865475244008
Sine:       0.70710678118654752442  0.70710678118654752442
Cosine:     0.70710678118654752438  0.70710678118654752438
Tangent:    1.00000000000000000000  1.00000000000000000000

Reference:  0.7853981633974483096156
Arcsine:    0.78539816339744830970 45.00000000000000000400
Arccosine:  0.78539816339744830961 45.00000000000000000000
Arctangent: 0.78539816339744830961 45.00000000000000000000
```



## E


{{trans|ALGOL 68}}

```e
def pi := (-1.0).acos()

def radians := pi / 4.0
def degrees := 45.0

def d2r := (pi/180).multiply
def r2d := (180/pi).multiply

println(`$\
${radians.sin()} ${d2r(degrees).sin()}
${radians.cos()} ${d2r(degrees).cos()}
${radians.tan()} ${d2r(degrees).tan()}
${def asin := radians.sin().asin()} ${r2d(asin)}
${def acos := radians.cos().acos()} ${r2d(acos)}
${def atan := radians.tan().atan()} ${r2d(atan)}
`)
```


{{out}}
 0.7071067811865475 0.7071067811865475
 0.7071067811865476 0.7071067811865476
 0.9999999999999999 0.9999999999999999
 0.7853981633974482 44.99999999999999
 0.7853981633974483 45.0
 0.7853981633974483 45.0


## Elena

{{trans|C++}}
ELENA 4.x:

```elena
import system'math;
import extensions;

public program()
{
    console.printLine("Radians:");
    console.printLine("sin(π/3) = ",(Pi_value/3).sin());
    console.printLine("cos(π/3) = ",(Pi_value/3).cos());
    console.printLine("tan(π/3) = ",(Pi_value/3).tan());
    console.printLine("arcsin(1/2) = ",0.5r.arcsin());
    console.printLine("arccos(1/2) = ",0.5r.arccos());
    console.printLine("arctan(1/2) = ",0.5r.arctan());
    console.printLine();

    console.printLine("Degrees:");
    console.printLine("sin(60º) = ",60.0r.Radian.sin());
    console.printLine("cos(60º) = ",60.0r.Radian.cos());
    console.printLine("tan(60º) = ",60.0r.Radian.tan());
    console.printLine("arcsin(1/2) = ",0.5r.arcsin().Degree,"º");
    console.printLine("arccos(1/2) = ",0.5r.arccos().Degree,"º");
    console.printLine("arctan(1/2) = ",0.5r.arctan().Degree,"º");

    console.readChar()
}
```



## Elixir

{{trans|Erlang}}

```elixir
iex(61)> deg = 45
45
iex(62)> rad = :math.pi / 4
0.7853981633974483
iex(63)> :math.sin(deg * :math.pi / 180) == :math.sin(rad)
true
iex(64)> :math.cos(deg * :math.pi / 180) == :math.cos(rad)
true
iex(65)> :math.tan(deg * :math.pi / 180) == :math.tan(rad)
true
iex(66)> temp = :math.acos(:math.cos(rad))
0.7853981633974483
iex(67)> temp * 180 / :math.pi == deg
true
iex(68)> temp = :math.atan(:math.tan(rad))
0.7853981633974483
iex(69)> temp * 180 / :math.pi == deg
true
```



## Erlang

{{trans|C}}

```erlang

Deg=45.
Rad=math:pi()/4.

math:sin(Deg * math:pi() / 180)==math:sin(Rad).

```


{{out}}
 true


```erlang

math:cos(Deg * math:pi() / 180)==math:cos(Rad).

```


{{out}}
 true


```erlang

math:tan(Deg * math:pi() / 180)==math:tan(Rad).

```


{{out}}
 true


```erlang

Temp = math:acos(math:cos(Rad)).
Temp * 180 / math:pi()==Deg.

```


{{out}}
 true


```erlang

Temp = math:atan(math:tan(Rad)).
Temp * 180 / math:pi()==Deg.

```


{{out}}
 true


## Factor


```factor
USING: kernel math math.constants math.functions math.trig
prettyprint ;

pi 4 / 45 deg>rad [ sin ] [ cos ] [ tan ]
[ [ . ] compose dup compose ] tri@ 2tri

.5 [ asin ] [ acos ] [ atan ] tri [ dup rad>deg [ . ] bi@ ] tri@
```



## Fantom


Fantom's Float library includes all six trigonometric functions,
which assume the number is in radians.

Methods are provided to convert: toDegrees and toRadians.


```fantom

class Main
{
  public static Void main ()
  {
    Float r := Float.pi / 4
    echo (r.sin)
    echo (r.cos)
    echo (r.tan)
    echo (r.asin)
    echo (r.acos)
    echo (r.atan)
    // and from degrees
    echo (45.0f.toRadians.sin)
    echo (45.0f.toRadians.cos)
    echo (45.0f.toRadians.tan)
    echo (45.0f.toRadians.asin)
    echo (45.0f.toRadians.acos)
    echo (45.0f.toRadians.atan)
  }
}

```



## Forth


```forth
45e  pi f* 180e f/     \ radians

cr fdup  fsin f.       \ also available: fsincos ( r -- sin cos )
cr fdup  fcos f.
cr fdup  ftan f.
cr fdup fasin f.
cr fdup facos f.
cr      fatan f.       \ also available: fatan2 ( r1 r2 -- atan[r1/r2] )
```



## Fortran

Trigonometic functions expect arguments in radians so degrees require conversion

```fortran
PROGRAM Trig

  REAL pi, dtor, rtod, radians, degrees

  pi = 4.0 * ATAN(1.0)
  dtor = pi / 180.0
  rtod = 180.0 / pi
  radians = pi / 4.0
  degrees = 45.0

  WRITE(*,*) SIN(radians), SIN(degrees*dtor)
  WRITE(*,*) COS(radians), COS(degrees*dtor)
  WRITE(*,*) TAN(radians), TAN(degrees*dtor)
  WRITE(*,*) ASIN(SIN(radians)), ASIN(SIN(degrees*dtor))*rtod
  WRITE(*,*) ACOS(COS(radians)), ACOS(COS(degrees*dtor))*rtod
  WRITE(*,*) ATAN(TAN(radians)), ATAN(TAN(degrees*dtor))*rtod

END PROGRAM Trig
```

{{out}}
  0.707107   0.707107
  0.707107   0.707107
   1.00000    1.00000
  0.785398    45.0000
  0.785398    45.0000
  0.785398    45.0000
The following trigonometric functions are also available

```fortran
 ATAN2(y,x) ! Arctangent(y/x), ''-pi < result <= +pi''
 SINH(x)    ! Hyperbolic sine
 COSH(x)    ! Hyperbolic cosine
 TANH(x)    ! Hyperbolic tangent
```


But, for those with access to fatter Fortran function libraries, trigonometrical functions working in degrees are also available.

```Fortran

Calculate various trigonometric functions from the Fortran library.
      INTEGER BIT(32),B,IP	!Stuff for bit fiddling.
      INTEGER ENUFF,I		!Step through the test angles.
      PARAMETER (ENUFF = 17)	!A selection of special values.
      INTEGER ANGLE(ENUFF)	!All in whole degrees.
      DATA ANGLE/0,30,45,60,90,120,135,150,180,	!Here they are.
     1 210,225,240,270,300,315,330,360/		!Thus check angle folding.
      REAL PI,DEG2RAD		!Special numbers.
      REAL D,R,FD,FR,AD,AR	!Degree, Radian, F(D), F(R), inverses.
      PI = 4*ATAN(1.0)		!SINGLE PRECISION 1·0.
      DEG2RAD = PI/180		!Limited precision here too for a transcendental number.
Case the first: sines.
      WRITE (6,10) ("Sin", I = 1,4)	!Supply some names.
   10 FORMAT (" Deg.",A7,"(Deg)",A7,"(Rad)   Rad - Deg",	!Ah, layout.
     1 6X,"Arc",A3,"D",6X,"Arc",A3,"R",9X,"Diff")
      DO I = 1,ENUFF		!Step through the test values.
        D = ANGLE(I)			!The angle in degrees, in floating point.
        R = D*DEG2RAD			!Approximation, in radians.
        FD = SIND(D); AD = ASIND(FD)		!Functions working in degrees.
        FR = SIN(R);  AR = ASIN(FR)/DEG2RAD	!Functions working in radians.
        WRITE (6,11) INT(D),FD,FR,FR - FD,AD,AR,AR - AD	!Results.
   11   FORMAT (I4,":",3F12.8,3F13.7)	!Ah, alignment with FORMAT 10...
      END DO			!On to the next test value.
Case the second: cosines.
      WRITE (6,10) ("Cos", I = 1,4)
      DO I = 1,ENUFF
        D = ANGLE(I)
        R = D*DEG2RAD
        FD = COSD(D); AD = ACOSD(FD)
        FR = COS(R);  AR = ACOS(FR)/DEG2RAD
        WRITE (6,11) INT(D),FD,FR,FR - FD,AD,AR,AR - AD
      END DO
Case the third: tangents.
      WRITE (6,10) ("Tan", I = 1,4)
      DO I = 1,ENUFF
        D = ANGLE(I)
        R = D*DEG2RAD
        FD = TAND(D); AD = ATAND(FD)
        FR = TAN(R);  AR = ATAN(FR)/DEG2RAD
        WRITE (6,11) INT(D),FD,FR,FR - FD,AD,AR,AR - AD
      END DO
      WRITE (6,*) "...Special deal for 90 degrees..."
      D = 90
      R = D*DEG2RAD
      FD = TAND(D); AD = ATAND(FD)
      FR = TAN(R);  AR = ATAN(FR)/DEG2RAD
      WRITE (6,*) "TanD =",FD,"Atan =",AD
      WRITE (6,*) "TanR =",FR,"Atan =",AR
Convert PI to binary...
      PI = PI - 3	!I know it starts with three, and I need the fractional part.
      BIT(1:2) = 1	!So, the binary is 11. something.
      B = 2		!Two bits known.
      DO I = 1,26	!For single precision, more than enough additional bits.
        PI = PI*2		!Hoist a bit to the hot spot.
        IP = PI			!The integral part.
        PI = PI - IP		!Remove it from the work in progress.
        B = B + 1		!Another bit bitten.
        BIT(B) = IP		!Place it.
      END DO		!On to the next.
      WRITE (6,20) BIT(1:B)	!Reveal the bits.
   20 FORMAT (" Pi ~ ",2I1,".",66I1)	!A known format.
      WRITE (6,*) "   = 11.00100100001111110110101010001000100001..."	!But actually...
      END		!So much for that.

```

Output:
 Deg.    Sin(Deg)    Sin(Rad)   Rad - Deg      ArcSinD      ArcSinR         Diff
   0:  0.00000000  0.00000000  0.00000000    0.0000000    0.0000000    0.0000000
  30:  0.50000000  0.50000000  0.00000000   30.0000000   30.0000000    0.0000000
  45:  0.70710677  0.70710677  0.00000000   45.0000000   45.0000000    0.0000000
  60:  0.86602539  0.86602545  0.00000006   60.0000000   60.0000038    0.0000038
  90:  1.00000000  1.00000000  0.00000000   90.0000000   90.0000000    0.0000000
 120:  0.86602539  0.86602539  0.00000000   60.0000000   60.0000000    0.0000000
 135:  0.70710677  0.70710677  0.00000000   45.0000000   45.0000000    0.0000000
 150:  0.50000000  0.50000006  0.00000006   30.0000000   30.0000038    0.0000038
 180:  0.00000000 -0.00000009 -0.00000009    0.0000000   -0.0000050   -0.0000050
 210: -0.50000000 -0.49999997  0.00000003  -30.0000000  -29.9999981    0.0000019
 225: -0.70710677 -0.70710671  0.00000006  -45.0000000  -44.9999962    0.0000038
 240: -0.86602539 -0.86602545 -0.00000006  -60.0000000  -60.0000038   -0.0000038
 270: -1.00000000 -1.00000000  0.00000000  -90.0000000  -90.0000000    0.0000000
 300: -0.86602539 -0.86602545 -0.00000006  -60.0000000  -60.0000038   -0.0000038
 315: -0.70710677 -0.70710689 -0.00000012  -45.0000000  -45.0000076   -0.0000076
 330: -0.50000000 -0.50000018 -0.00000018  -30.0000000  -30.0000114   -0.0000114
 360:  0.00000000  0.00000017  0.00000017    0.0000000    0.0000100    0.0000100
 Deg.    Cos(Deg)    Cos(Rad)   Rad - Deg      ArcCosD      ArcCosR         Diff
   0:  1.00000000  1.00000000  0.00000000    0.0000000    0.0000000    0.0000000
  30:  0.86602539  0.86602539  0.00000000   30.0000019   30.0000019    0.0000000
  45:  0.70710677  0.70710677  0.00000000   45.0000000   45.0000000    0.0000000
  60:  0.50000000  0.49999997 -0.00000003   60.0000000   60.0000038    0.0000038
  90:  0.00000000 -0.00000004 -0.00000004   90.0000000   90.0000000    0.0000000
 120: -0.50000000 -0.50000006 -0.00000006  120.0000000  120.0000076    0.0000076
 135: -0.70710677 -0.70710677  0.00000000  135.0000000  135.0000000    0.0000000
 150: -0.86602539 -0.86602539  0.00000000  150.0000000  150.0000000    0.0000000
 180: -1.00000000 -1.00000000  0.00000000  180.0000000  180.0000000    0.0000000
 210: -0.86602539 -0.86602539  0.00000000  150.0000000  150.0000000    0.0000000
 225: -0.70710677 -0.70710683 -0.00000006  135.0000000  135.0000000    0.0000000
 240: -0.50000000 -0.49999991  0.00000009  120.0000000  119.9999924   -0.0000076
 270:  0.00000000  0.00000001  0.00000001   90.0000000   90.0000000    0.0000000
 300:  0.50000000  0.49999991 -0.00000009   60.0000000   60.0000076    0.0000076
 315:  0.70710677  0.70710665 -0.00000012   45.0000000   45.0000114    0.0000114
 330:  0.86602539  0.86602533 -0.00000006   30.0000019   30.0000095    0.0000076
 360:  1.00000000  1.00000000  0.00000000    0.0000000    0.0000000    0.0000000
 Deg.    Tan(Deg)    Tan(Rad)   Rad - Deg      ArcTanD      ArcTanR         Diff
   0:  0.00000000  0.00000000  0.00000000    0.0000000    0.0000000    0.0000000
  30:  0.57735026  0.57735026  0.00000000   30.0000000   30.0000000    0.0000000
  45:  1.00000000  1.00000000  0.00000000   45.0000000   45.0000000    0.0000000
  60:  1.73205078  1.73205090  0.00000012   60.0000000   60.0000000    0.0000000
  90:************************************   90.0000000  -90.0000000 -180.0000000
 120: -1.73205078 -1.73205054  0.00000024  -60.0000000  -59.9999962    0.0000038
 135: -1.00000000 -1.00000000  0.00000000  -45.0000000  -45.0000000    0.0000000
 150: -0.57735026 -0.57735032 -0.00000006  -30.0000000  -30.0000019   -0.0000019
 180:  0.00000000  0.00000009  0.00000009    0.0000000    0.0000050    0.0000050
 210:  0.57735026  0.57735026  0.00000000   30.0000000   30.0000000    0.0000000
 225:  1.00000000  0.99999988 -0.00000012   45.0000000   44.9999962   -0.0000038
 240:  1.73205078  1.73205125  0.00000048   60.0000000   60.0000076    0.0000076
 270:************************************   90.0000000  -90.0000000 -180.0000000
 300: -1.73205078 -1.73205113 -0.00000036  -60.0000000  -60.0000038   -0.0000038
 315: -1.00000000 -1.00000024 -0.00000024  -45.0000000  -45.0000076   -0.0000076
 330: -0.57735026 -0.57735056 -0.00000030  -30.0000000  -30.0000134   -0.0000134
 360:  0.00000000  0.00000017  0.00000017    0.0000000    0.0000100    0.0000100
 ...Special deal for 90 degrees...
 TanD =  1.6331778E+16 Atan =   90.00000
 TanR = -2.2877332E+07 Atan =  -90.00000
 Pi ~ 11.00100100001111110110110000
    = 11.00100100001111110110101010001000100001...
Notice that the calculations in radians are less accurate. Firstly, pi cannot be represented exactly and secondly, the conversion factor of pi/180 or 180/pi adds further to the error. The degree-based functions obviously can fold their angles using exact arithmetic (though ACosD has surprising trouble with 30°) and so 360° is the same as 0°, unlike the case with radians. TanD(90°) should yield Infinity (but, which sign?) but perhaps this latter-day feature of computer floating-point was not included. In any case, Tan(90° in radians) faces the problem that its parameter will not in fact be pi/2 but some value just over (or under), and likewise with double precision and quadruple precision and any other finite precision.


## FreeBASIC

{{trans|C}}

```freebasic
' FB 1.05.0 Win64

Const pi As Double = 4 * Atn(1)
Dim As Double radians = pi / 4
Dim As Double degrees = 45.0 '' equivalent in degrees
Dim As Double temp

Print "Radians     : "; radians, " ";
Print "Degrees     : "; degrees
Print
Print "Sine        : "; Sin(radians), Sin(degrees * pi / 180)
Print "Cosine      : "; Cos(radians), Cos(degrees * pi / 180)
Print "Tangent     : "; Tan(radians), Tan(degrees * pi / 180)
Print
temp = ASin(Sin(radians))
Print "Arc Sine    : "; temp, temp * 180 / pi
temp = ACos(Cos(radians))
Print "Arc Cosine  : "; temp, temp * 180 / pi
temp = Atn(Tan(radians))
Print "Arc Tangent : "; temp, temp * 180 / pi
Sleep
```


{{out}}

```txt

Radians     :  0.7853981633974483          Degrees     :  45

Sine        :  0.7071067811865475          0.7071067811865475
Cosine      :  0.7071067811865476          0.7071067811865476
Tangent     :  0.9999999999999999          0.9999999999999999

Arc Sine    :  0.7853981633974482          44.99999999999999
Arc Cosine  :  0.7853981633974483          45
Arc Tangent :  0.7853981633974483          45

```


=={{header|F_Sharp|F#}}==

```fsharp
open NUnit.Framework
open FsUnit

// radian

[<Test>]
let ``Verify that sin pi returns 0`` () =
  let x = System.Math.Sin System.Math.PI
  System.Math.Round(x,5) |> should equal 0

[<Test>]
let ``Verify that cos pi returns -1`` () =
  let x = System.Math.Cos System.Math.PI
  System.Math.Round(x,5) |> should equal -1

[<Test>]
let ``Verify that tan pi returns 0`` () =
  let x = System.Math.Tan System.Math.PI
  System.Math.Round(x,5) |> should equal 0

[<Test>]
let ``Verify that sin pi/2 returns 1`` () =
  let x = System.Math.Sin (System.Math.PI / 2.0)
  System.Math.Round(x,5) |> should equal 1

[<Test>]
let ``Verify that cos pi/2 returns -1`` () =
  let x = System.Math.Cos (System.Math.PI / 2.0)
  System.Math.Round(x,5) |> should equal 0

[<Test>]
let ``Verify that sin pi/3 returns sqrt 3/2`` () =
  let actual = System.Math.Sin (System.Math.PI / 3.0)
  let expected = System.Math.Round((System.Math.Sqrt 3.0) / 2.0, 5)
  System.Math.Round(actual,5) |> should equal expected

[<Test>]
let ``Verify that cos pi/3 returns -1`` () =
  let x = System.Math.Cos (System.Math.PI / 3.0)
  System.Math.Round(x,5) |> should equal 0.5

[<Test>]
let ``Verify that cos and sin of pi/4 return same value`` () =
  let c = System.Math.Cos (System.Math.PI / 4.0)
  let s = System.Math.Sin (System.Math.PI / 4.0)
  System.Math.Round(c,5) = System.Math.Round(s,5) |> should be True

[<Test>]
let ``Verify that acos pi/3 returns 1/2`` () =
  let actual = System.Math.Acos 0.5
  let expected = System.Math.Round((System.Math.PI / 3.0),5)
  System.Math.Round(actual,5) |> should equal expected

[<Test>]
let ``Verify that asin 1 returns pi/2`` () =
  let actual = System.Math.Asin 1.0
  let expected = System.Math.Round((System.Math.PI / 2.0),5)
  System.Math.Round(actual,5) |> should equal expected

[<Test>]
let ``Verify that atan 0 returns 0`` () =
  let actual = System.Math.Atan 0.0
  let expected = System.Math.Round(0.0,5)
  System.Math.Round(actual,5) |> should equal expected

// degree

let toRadians d = d * System.Math.PI / 180.0

[<Test>]
let ``Verify that pi is 180 degrees`` () =
  toRadians 180.0 |> should equal System.Math.PI

[<Test>]
let ``Verify that pi/2 is 90 degrees`` () =
  toRadians 90.0 |> should equal (System.Math.PI / 2.0)

[<Test>]
let ``Verify that pi/3 is 60 degrees`` () =
  toRadians 60.0 |> should equal (System.Math.PI / 3.0)

[<Test>]
let ``Verify that sin 180 returns 0`` () =
  let x = System.Math.Sin (toRadians 180.0)
  System.Math.Round(x,5) |> should equal 0

[<Test>]
let ``Verify that cos 180 returns -1`` () =
  let x = System.Math.Cos (toRadians 180.0)
  System.Math.Round(x,5) |> should equal -1

[<Test>]
let ``Verify that tan 180 returns 0`` () =
  let x = System.Math.Tan (toRadians 180.0)
  System.Math.Round(x,5) |> should equal 0

[<Test>]
let ``Verify that sin 90 returns 1`` () =
  let x = System.Math.Sin (toRadians 90.0)
  System.Math.Round(x,5) |> should equal 1

[<Test>]
let ``Verify that cos 90 returns -1`` () =
  let x = System.Math.Cos (toRadians 90.0)
  System.Math.Round(x,5) |> should equal 0

[<Test>]
let ``Verify that sin 60 returns sqrt 3/2`` () =
  let actual = System.Math.Sin (toRadians 60.0)
  let expected = System.Math.Round((System.Math.Sqrt 3.0) / 2.0, 5)
  System.Math.Round(actual,5) |> should equal expected

[<Test>]
let ``Verify that cos 60 returns -1`` () =
  let x = System.Math.Cos (toRadians 60.0)
  System.Math.Round(x,5) |> should equal 0.5

[<Test>]
let ``Verify that cos and sin of 45 return same value`` () =
  let c = System.Math.Cos (toRadians 45.0)
  let s = System.Math.Sin (toRadians 45.0)
  System.Math.Round(c,5) = System.Math.Round(s,5) |> should be True
```



## GAP


```gap
# GAP has an improved floating-point support since version 4.5

Pi := Acos(-1.0);

# Or use the built-in constant:
Pi := FLOAT.PI;

r := Pi / 5.0;
d := 36;

Deg := x -> x * Pi / 180;

Sin(r);         Asin(last);
Sin(Deg(d));    Asin(last);
Cos(r);         Acos(last);
Cos(Deg(d));    Acos(last);
Tan(r);         Atan(last);
Tan(Deg(d));    Atan(last);
```



## Go

The Go math package provides the constant pi and the six trigonometric functions called for by the task.  The functions all use the float64 type and work in radians.  It also provides a [http://golang.org/pkg/math/#Sincos Sincos] function.

```go
package main

import (
    "fmt"
    "math"
)

const d = 30.
const r = d * math.Pi / 180

var s = .5
var c = math.Sqrt(3) / 2
var t = 1 / math.Sqrt(3)

func main() {
    fmt.Printf("sin(%9.6f deg) = %f\n", d, math.Sin(d*math.Pi/180))
    fmt.Printf("sin(%9.6f rad) = %f\n", r, math.Sin(r))
    fmt.Printf("cos(%9.6f deg) = %f\n", d, math.Cos(d*math.Pi/180))
    fmt.Printf("cos(%9.6f rad) = %f\n", r, math.Cos(r))
    fmt.Printf("tan(%9.6f deg) = %f\n", d, math.Tan(d*math.Pi/180))
    fmt.Printf("tan(%9.6f rad) = %f\n", r, math.Tan(r))
    fmt.Printf("asin(%f) = %9.6f deg\n", s, math.Asin(s)*180/math.Pi)
    fmt.Printf("asin(%f) = %9.6f rad\n", s, math.Asin(s))
    fmt.Printf("acos(%f) = %9.6f deg\n", c, math.Acos(c)*180/math.Pi)
    fmt.Printf("acos(%f) = %9.6f rad\n", c, math.Acos(c))
    fmt.Printf("atan(%f) = %9.6f deg\n", t, math.Atan(t)*180/math.Pi)
    fmt.Printf("atan(%f) = %9.6f rad\n", t, math.Atan(t))
}
```

{{out}}

```txt

sin(30.000000 deg) = 0.500000
sin( 0.523599 rad) = 0.500000
cos(30.000000 deg) = 0.866025
cos( 0.523599 rad) = 0.866025
tan(30.000000 deg) = 0.577350
tan( 0.523599 rad) = 0.577350
asin(0.500000) = 30.000000 deg
asin(0.500000) =  0.523599 rad
acos(0.866025) = 30.000000 deg
acos(0.866025) =  0.523599 rad
atan(0.577350) = 30.000000 deg
atan(0.577350) =  0.523599 rad

```



## Groovy

Trig functions use radians, degrees must be converted to/from radians

```groovy
def radians = Math.PI/4
def degrees = 45

def d2r = { it*Math.PI/180 }
def r2d = { it*180/Math.PI }

println "sin(\u03C0/4) = ${Math.sin(radians)}  == sin(45\u00B0) = ${Math.sin(d2r(degrees))}"
println "cos(\u03C0/4) = ${Math.cos(radians)}  == cos(45\u00B0) = ${Math.cos(d2r(degrees))}"
println "tan(\u03C0/4) = ${Math.tan(radians)}  == tan(45\u00B0) = ${Math.tan(d2r(degrees))}"
println "asin(\u221A2/2) = ${Math.asin(2**(-0.5))} == asin(\u221A2/2)\u00B0 = ${r2d(Math.asin(2**(-0.5)))}\u00B0"
println "acos(\u221A2/2) = ${Math.acos(2**(-0.5))} == acos(\u221A2/2)\u00B0 = ${r2d(Math.acos(2**(-0.5)))}\u00B0"
println "atan(1) = ${Math.atan(1)} == atan(1)\u00B0 = ${r2d(Math.atan(1))}\u00B0"
```


{{out}}

```txt
sin(π/4) = 0.7071067811865475  == sin(45°) = 0.7071067811865475
cos(π/4) = 0.7071067811865476  == cos(45°) = 0.7071067811865476
tan(π/4) = 0.9999999999999999  == tan(45°) = 0.9999999999999999
asin(√2/2) = 0.7853981633974482 == asin(√2/2)° = 44.99999999999999°
acos(√2/2) = 0.7853981633974484 == acos(√2/2)° = 45.00000000000001°
atan(1) = 0.7853981633974483 == atan(1)° = 45.0°
```



## Haskell


Trigonometric functions use radians; degrees require conversion.


```haskell>fromDegrees :: Floating a =
 a -> a
fromDegrees deg = deg * pi / 180

toDegrees :: Floating a => a -> a
toDegrees rad = rad * 180 / pi

main :: IO ()
main =
  mapM_
    print
    [ sin (pi / 6)
    , sin (fromDegrees 30)
    , cos (pi / 6)
    , cos (fromDegrees 30)
    , tan (pi / 6)
    , tan (fromDegrees 30)
    , asin 0.5
    , toDegrees (asin 0.5)
    , acos 0.5
    , toDegrees (acos 0.5)
    , atan 0.5
    , toDegrees (atan 0.5)
    ]
```

{{Out}}

```txt
0.49999999999999994
0.49999999999999994
0.8660254037844387
0.8660254037844387
0.5773502691896256
0.5773502691896256
0.5235987755982988
29.999999999999996
1.0471975511965976
59.99999999999999
0.46364760900080615
26.56505117707799
```



## HicEst

Translated from Fortran:

```hicest
pi = 4.0 * ATAN(1.0)
dtor = pi / 180.0
rtod = 180.0 / pi
radians = pi / 4.0
degrees = 45.0

WRITE(ClipBoard) SIN(radians), SIN(degrees*dtor)
WRITE(ClipBoard) COS(radians), COS(degrees*dtor)
WRITE(ClipBoard) TAN(radians), TAN(degrees*dtor)
WRITE(ClipBoard) ASIN(SIN(radians)), ASIN(SIN(degrees*dtor))*rtod
WRITE(ClipBoard) ACOS(COS(radians)), ACOS(COS(degrees*dtor))*rtod
WRITE(ClipBoard) ATAN(TAN(radians)), ATAN(TAN(degrees*dtor))*rtod
```


```hicest
0.7071067812 0.7071067812
0.7071067812 0.7071067812
1 1
0.7853981634 45
0.7853981634 45
0.7853981634 45
```

SINH, COSH, TANH, and inverses are available as well.


## IDL



```idl
deg = 35         ; arbitrary number of degrees
rad = !dtor*deg  ; system variables !dtor and !radeg convert between rad and deg
```


```idl
; the trig functions receive and emit radians:
print, rad, sin(rad), asin(sin(rad))
print, cos(rad), acos(cos(rad))
print, tan(rad), atan(tan(rad))        ; etc

; prints the following:
; 0.610865     0.573576     0.610865
; 0.819152     0.610865
; 0.700208     0.610865
```


```idl
; the hyperbolic versions exist and behave as expected:
print, sinh(rad)                       ; etc

; outputs
;   0.649572
```


```idl
;If the input is an array, the output has the same dimensions etc as the input:
x = !dpi/[[2,3],[4,5],[6,7]] ; !dpi is a read-only sysvar = 3.1415...
print,sin(x)

;outputs:
;      1.0000000      0.86602540
;      0.70710678      0.58778525
;      0.50000000      0.43388374
```


```idl
; the trig functions behave as expected for complex arguments:
x = complex(1,2)
print,sin(x)

; outputs
; (      3.16578,      1.95960)
```


== Icon and Unicon ==
Icon and Unicon trig functions 'sin', 'cos', 'tan', 'asin', 'acos', and 'atan' operate on angles expressed in radians.  Conversion functions 'dtor' and 'rtod' convert between the two systems.  The example below uses string invocation to construct and call the functions:
=
## Icon
=

```Icon
invocable all
procedure main()

d := 30          # degrees
r := dtor(d)     # convert to radians

every write(f := !["sin","cos","tan"],"(",r,")=",y := f(r)," ",fi := "a" || f,"(",y,")=",x := fi(y)," rad = ",rtod(x)," deg")
end
```

{{out}}

```txt
sin(0.5235987755982988)=0.4999999999999999 asin(0.4999999999999999)=0.5235987755982988 rad = 30.0 deg
cos(0.5235987755982988)=0.8660254037844387 acos(0.8660254037844387)=0.5235987755982987 rad = 29.99999999999999 deg
tan(0.5235987755982988)=0.5773502691896257 atan(0.5773502691896257)=0.5235987755982988 rad = 30.0 deg
```


=
## Unicon
=
The Icon solution works in Unicon.


## J

The [http://www.jsoftware.com/help/dictionary/dodot.htm circle functions] in J include trigonometric functions. Native operation is in radians, so values in degrees involve conversion.

Sine, cosine, and tangent of a single angle, indicated as pi-over-four radians and as 45 degrees:

```j
   (1&o. , 2&o. ,: 3&o.) (4 %~ o. 1) , 180 %~ o. 45
0.707107 0.707107
0.707107 0.707107
       1        1
```

Arcsine, arccosine, and arctangent of one-half, in radians and degrees:

```j
   ([ ,. 180p_1&*) (_1&o. , _2&o. ,: _3&o.) 0.5
0.523599      30
  1.0472      60
0.463648 26.5651
```


The <code>trig</code> script adds cover functions for the trigonometric operations as well as verbs for converting degrees from radians (<code>dfr</code>) and radians from degrees (<code>rfd</code>)

```j
   require 'trig'
   (sin , cos ,: tan) (1p1 % 4), rfd 45
0.707107 0.707107
0.707107 0.707107
       1        1

   ([ ,. dfr) (arcsin , arccos ,: arctan) 0.5
0.523599      30
  1.0472      60
0.463648 26.5651
```



## Java


Java's <tt>Math</tt> class contains all six functions and is automatically included as part of the language. The functions all accept radians only, so conversion is necessary when dealing with degrees. The <tt>Math</tt> class also has a <tt>PI</tt> constant for easy conversion.


```java
public class Trig {
        public static void main(String[] args) {
                //Pi / 4 is 45 degrees. All answers should be the same.
                double radians = Math.PI / 4;
                double degrees = 45.0;
                //sine
                System.out.println(Math.sin(radians) + " " + Math.sin(Math.toRadians(degrees)));
                //cosine
                System.out.println(Math.cos(radians) + " " + Math.cos(Math.toRadians(degrees)));
                //tangent
                System.out.println(Math.tan(radians) + " " + Math.tan(Math.toRadians(degrees)));
                //arcsine
                double arcsin = Math.asin(Math.sin(radians));
                System.out.println(arcsin + " " + Math.toDegrees(arcsin));
                //arccosine
                double arccos = Math.acos(Math.cos(radians));
                System.out.println(arccos + " " + Math.toDegrees(arccos));
                //arctangent
                double arctan = Math.atan(Math.tan(radians));
                System.out.println(arctan + " " + Math.toDegrees(arctan));
        }
}
```


{{out}}

```txt

0.7071067811865475 0.7071067811865475
0.7071067811865476 0.7071067811865476
0.9999999999999999 0.9999999999999999
0.7853981633974482 44.99999999999999
0.7853981633974483 45.0
0.7853981633974483 45.0

```



## JavaScript


JavaScript's <tt>Math</tt> class contains all six functions and is automatically included as part of the language. The functions all accept radians only, so conversion is necessary when dealing with degrees. The <tt>Math</tt> class also has a <tt>PI</tt> constant for easy conversion.


```javascript
var
 radians = Math.PI / 4, // Pi / 4 is 45 degrees. All answers should be the same.
 degrees = 45.0,
 sine = Math.sin(radians),
 cosine = Math.cos(radians),
 tangent = Math.tan(radians),
 arcsin = Math.asin(sine),
 arccos = Math.acos(cosine),
 arctan = Math.atan(tangent);

// sine
window.alert(sine + " " + Math.sin(degrees * Math.PI / 180));
// cosine
window.alert(cosine + " " + Math.cos(degrees * Math.PI / 180));
// tangent
window.alert(tangent + " " + Math.tan(degrees * Math.PI / 180));
// arcsine
window.alert(arcsin + " " + (arcsin * 180 / Math.PI));
// arccosine
window.alert(arccos + " " + (arccos * 180 / Math.PI));
// arctangent
window.alert(arctan + " " + (arctan * 180 / Math.PI));
```



## jq


jq includes the standard C-library trigonometric functions (sin, cos, tan, asin, acos, atan), but they are provided as filters as illustrated in the definition of <tt>radians</tt> below.

The trigonometric filters only accept radians, so conversion is necessary when dealing with degrees. The constant <tt>π</tt> can be defined as also shown in the following definition of <tt>radians</tt>:
```jq

# degrees to radians
def radians:
  (-1|acos) as $pi | (. * $pi / 180);

def task:
  (-1|acos) as $pi
  |  ($pi / 180) as $degrees
  | "Using radians:",
    "  sin(-pi / 6)     = \( (-$pi / 6)    | sin )",
    "  cos(3 * pi / 4)  = \( (3 * $pi / 4) | cos)",
    "  tan(pi / 3)      = \( ($pi / 3)     | tan)",
    "  asin(-1 / 2)     = \((-1 / 2)       | asin)",
    "  acos(-sqrt(2)/2) = \((-(2|sqrt)/2)  | acos )",
    "  atan(sqrt(3))    = \(      3 | sqrt | atan )",

    "Using degrees:",
    "  sin(-30)         = \((-30 * $degrees) | sin)",
    "  cos(135)         = \((135 * $degrees) | cos)",
    "  tan(60)          = \(( 60 * $degrees) | tan)",
    "  asin(-1 / 2)     = \(        (-1 / 2) | asin / $degrees)",
    "  acos(-sqrt(2)/2) = \( (-(2|sqrt) / 2) | acos / $degrees)",
    "  atan(sqrt(3))    = \( (3 | sqrt)      | atan / $degrees)"
;

task

```

{{out}}

```sh
Using radians:
  sin(-pi / 6)     = -0.49999999999999994
  cos(3 * pi / 4)  = -0.7071067811865475
  tan(pi / 3)      = 1.7320508075688767
  asin(-1 / 2)     = -0.5235987755982988
  acos(-sqrt(2)/2) = 2.356194490192345
  atan(sqrt(3))    = 1.0471975511965979
Using degrees:
  sin(-30)         = -0.49999999999999994
  cos(135)         = -0.7071067811865475
  tan(60)          = 1.7320508075688767
  asin(-1 / 2)     = -29.999999999999996
  acos(-sqrt(2)/2) = 135
  atan(sqrt(3))    = 60.00000000000001
```



## Jsish

Like many programming languages that handle trig, Jsish also includes the ''atan2'' function, which was originally added to Fortran to allow disambiguous results when converting from cartesian to polar coordinates, due to the mirror image nature of normal arctan.

To find what methods are supported, ''jsish'' supports help for the Math module.


```txt
help Math
Math.method(...)
Commands performing math operations on numbers
Methods: abs acos asin atan atan2 ceil cos exp floor log max min pow random round sin sqrt tan
```


Angles passed to the trigonometric functions expect arguments in ''radians'' (Pi by 4 radians being 45 degrees).  Degree to radian conversion is shown by multiplying radians by Pi over 180.

''Note the inexact nature of floating point approximations.''


```javascript
/* Trig in Jsish */
var x;

;x = Math.PI / 4;
;Math.sin(x);
;Math.cos(x);
;Math.tan(x);
;Math.asin(Math.sin(x)) * 4;
;Math.acos(Math.cos(x)) * 4;
;Math.atan(Math.tan(x));
;Math.atan2(Math.tan(x), 1.0);
;Math.atan2(Math.tan(x), -1.0);

;x = 45.0;
;Math.sin(x * Math.PI / 180);
;Math.cos(x * Math.PI / 180);
;Math.tan(x * Math.PI / 180);

/*
=!EXPECTSTART!=
x = Math.PI / 4 ==> 0.7853981633974483
Math.sin(x) ==> 0.7071067811865475
Math.cos(x) ==> 0.7071067811865476
Math.tan(x) ==> 0.9999999999999999
Math.asin(Math.sin(x)) * 4 ==> 3.141592653589793
Math.acos(Math.cos(x)) * 4 ==> 3.141592653589793
Math.atan(Math.tan(x)) ==> 0.7853981633974483
Math.atan2(Math.tan(x), 1.0) ==> 0.7853981633974483
Math.atan2(Math.tan(x), -1.0) ==> 2.356194490192345
x = 45.0 ==> 45
Math.sin(x * Math.PI / 180) ==> 0.7071067811865475
Math.cos(x * Math.PI / 180) ==> 0.7071067811865476
Math.tan(x * Math.PI / 180) ==> 0.9999999999999999
=!EXPECTEND!=
*/
```


{{out}}

```txt
prompt$ jsish --U trigonometric.jsi
x = Math.PI / 4 ==> 0.7853981633974483
Math.sin(x) ==> 0.7071067811865475
Math.cos(x) ==> 0.7071067811865476
Math.tan(x) ==> 0.9999999999999999
Math.asin(Math.sin(x)) * 4 ==> 3.141592653589793
Math.acos(Math.cos(x)) * 4 ==> 3.141592653589793
Math.atan(Math.tan(x)) ==> 0.7853981633974483
Math.atan2(Math.tan(x), 1.0) ==> 0.7853981633974483
Math.atan2(Math.tan(x), -1.0) ==> 2.356194490192345
x = 45.0 ==> 45
Math.sin(x * Math.PI / 180) ==> 0.7071067811865475
Math.cos(x * Math.PI / 180) ==> 0.7071067811865476
Math.tan(x * Math.PI / 180) ==> 0.9999999999999999

prompt$ jsish -u trigonometric.jsi
[PASS] trigonometric.jsi
```



## Julia


```julia
# v0.6.0

rad = π / 4
deg = 45.0

@show rad deg
@show sin(rad) sin(deg2rad(deg))
@show cos(rad) cos(deg2rad(deg))
@show tan(rad) tan(deg2rad(deg))
@show asin(sin(rad)) asin(sin(rad)) |> rad2deg
@show acos(cos(rad)) acos(cos(rad)) |> rad2deg
@show atan(tan(rad)) atan(tan(rad)) |> rad2deg
```


{{out}}

```txt
rad = 0.7853981633974483
deg = 45.0
sin(rad) = 0.7071067811865475
sin(deg2rad(deg)) = 0.7071067811865475
cos(rad) = 0.7071067811865476
cos(deg2rad(deg)) = 0.7071067811865476
tan(rad) = 0.9999999999999999
tan(deg2rad(deg)) = 0.9999999999999999
asin(sin(rad)) = 0.7853981633974482
asin(sin(rad)) |> rad2deg = 44.99999999999999
acos(cos(rad)) = 0.7853981633974483
acos(cos(rad)) |> rad2deg = 45.0
atan(tan(rad)) = 0.7853981633974483
atan(tan(rad)) |> rad2deg = 45.0
```



## Kotlin


```scala
// version 1.1.2

import java.lang.Math.*

fun main(args: Array<String>) {
    val radians = Math.PI / 4.0
    val degrees = 45.0
    val conv = Math.PI / 180.0
    val f = "%1.15f"
    var inverse: Double

    println("                Radians              Degrees")
    println("Angle      : ${f.format(radians)}\t ${f.format(degrees)}\n")
    println("Sin        : ${f.format(sin(radians))}\t  ${f.format(sin(degrees * conv))}")
    println("Cos        : ${f.format(cos(radians))}\t  ${f.format(cos(degrees * conv))}")
    println("Tan        : ${f.format(tan(radians))}\t  ${f.format(tan(degrees * conv))}\n")
    inverse = asin(sin(radians))
    println("ASin(Sin)  : ${f.format(inverse)}\t ${f.format(inverse / conv)}")
    inverse = acos(cos(radians))
    println("ACos(Cos)  : ${f.format(inverse)}\t ${f.format(inverse / conv)}")
    inverse = atan(tan(radians))
    println("ATan(Tan)  : ${f.format(inverse)}\t ${f.format(inverse / conv)}")
}
```


{{out}}

```txt

                Radians              Degrees
Angle      : 0.785398163397448	 45.000000000000000

Sin        : 0.707106781186548	  0.707106781186548
Cos        : 0.707106781186548	  0.707106781186548
Tan        : 1.000000000000000	  1.000000000000000

ASin(Sin)  : 0.785398163397448	 44.999999999999990
ACos(Cos)  : 0.785398163397448	 45.000000000000000
ATan(Tan)  : 0.785398163397448	 45.000000000000000

```



## Liberty BASIC


```lb
pi = ACS(-1)
radians = pi / 4.0
rtod = 180 / pi
degrees = radians * rtod
dtor = pi / 180

'LB works in radians, so degrees require conversion
print "Sin:   ";SIN(radians);"   "; SIN(degrees*dtor)
print "Cos:  ";COS(radians);"    "; COS(degrees*dtor)
print "Tan:  ";TAN(radians);"                   ";TAN(degrees*dtor)
print "-  Inverse functions:"
print "Asn:  ";ASN(SIN(radians));" Rad,  "; ASN(SIN(degrees*dtor))*rtod;" Deg"
print "Acs:  ";ACS(COS(radians));" Rad,  "; ACS(COS(degrees*dtor))*rtod;" Deg"
print "Atn:  ";ATN(TAN(radians));" Rad,  "; ATN(TAN(degrees*dtor))*rtod;" Deg"
```

{{out}}

```txt
Sin:  0.70710678    0.70710678
Cos:  0.70710678    0.70710678
Tan:  1.0           1.0
-  Inverse functions:
Asn:   0.78539816 Rad,  45.0 Deg
Acs:   0.78539816 Rad,  45.0 Deg
Atn:   0.78539816 Rad,  45.0 Deg
```



## Logo

[[UCB Logo]] has sine, cosine, and arctangent; each having variants for degrees or radians.

```logo
print sin 45
print cos 45
print arctan 1
make "pi (radarctan 0 1) * 2 ; based on quadrant if uses two parameters
print radsin :pi / 4
print radcos :pi / 4
print 4 * radarctan 1
```



[[Lhogho]] has pi defined in its trigonometric functions. Otherwise the same as UCB Logo.

```logo
print sin 45
print cos 45
print arctan 1
print radsin pi / 4
print radcos pi / 4
print 4 * radarctan 1
```



## Logtalk


```logtalk

:- object(trignomeric_functions).

    :- public(show/0).
    show :-
        % standard trignomeric functions work with radians
        write('sin(pi/4.0) = '), SIN is sin(pi/4.0), write(SIN), nl,
        write('cos(pi/4.0) = '), COS is cos(pi/4.0), write(COS), nl,
        write('tan(pi/4.0) = '), TAN is tan(pi/4.0), write(TAN), nl,
        write('asin(sin(pi/4.0)) = '), ASIN is asin(sin(pi/4.0)), write(ASIN), nl,
        write('acos(cos(pi/4.0)) = '), ACOS is acos(cos(pi/4.0)), write(ACOS), nl,
        write('atan(tan(pi/4.0)) = '), ATAN is atan(tan(pi/4.0)), write(ATAN), nl,
        write('atan2(3,4) = '), ATAN2 is atan2(3,4), write(ATAN2), nl.

:- end_object.

```

{{out}}

```text

?- trignomeric_functions::show.
sin(pi/4.0) = 0.7071067811865475
cos(pi/4.0) = 0.7071067811865476
tan(pi/4.0) = 0.9999999999999999
asin(sin(pi/4.0)) = 0.7853981633974482
acos(cos(pi/4.0)) = 0.7853981633974483
atan(tan(pi/4.0)) = 0.7853981633974483
atan2(3,4) = 0.6435011087932844
yes

```



## Lua


```lua
print(math.cos(1), math.sin(1), math.tan(1), math.atan(1), math.atan2(3, 4))
```



## Maple

In radians:

```Maple
sin(Pi/3);
cos(Pi/3);
tan(Pi/3);
```

{{out}}

```txt

> sin(Pi/3);
                                 1/2
                                3
                                ----
                                 2
> cos(Pi/3);
                                1/2

> tan(Pi/3);
                                 1/2
                                3

```


The equivalent in degrees with identical output:

```Maple
with(Units[Standard]):
sin(60*Unit(degree));
cos(60*Unit(degree));
tan(60*Unit(degree));
```


Note, Maple also has secant, cosecant, and cotangent:

```Maple
csc(Pi/3);
sec(Pi/3);
cot(Pi/3);
```


Finally, the inverse trigonometric functions:

```Maple
arcsin(1);
arccos(1);
arctan(1);
```

{{out}}

```txt
> arcsin(1);
                 Pi
                ----
                 2

> arccos(1);
                  0

> arctan(1);
                 Pi
                ----
                 4

```


Lastly, Maple also supports the two-argument arctan plus all the hyperbolic trigonometric functions.


## Mathematica


```Mathematica
Sin[1]
Cos[1]
Tan[1]
ArcSin[1]
ArcCos[1]
ArcTan[1]
Sin[90 Degree]
Cos[90 Degree]
Tan[90 Degree]

```



## MATLAB

A full list of built-in trig functions can be found in the [http://www.mathworks.com/access/helpdesk/help/techdoc/ref/f16-5872.html#f16-6197 MATLAB Documentation].


```MATLAB
function trigExample(angleDegrees)

    angleRadians = angleDegrees * (pi/180);

    disp(sprintf('sin(%f)= %f\nasin(%f)= %f',[angleRadians sin(angleRadians) sin(angleRadians) asin(sin(angleRadians))]));
    disp(sprintf('sind(%f)= %f\narcsind(%f)= %f',[angleDegrees sind(angleDegrees) sind(angleDegrees) asind(sind(angleDegrees))]));
    disp('-----------------------');
    disp(sprintf('cos(%f)= %f\nacos(%f)= %f',[angleRadians cos(angleRadians) cos(angleRadians) acos(cos(angleRadians))]));
    disp(sprintf('cosd(%f)= %f\narccosd(%f)= %f',[angleDegrees cosd(angleDegrees) cosd(angleDegrees) acosd(cosd(angleDegrees))]));
    disp('-----------------------');
    disp(sprintf('tan(%f)= %f\natan(%f)= %f',[angleRadians tan(angleRadians) tan(angleRadians) atan(tan(angleRadians))]));
    disp(sprintf('tand(%f)= %f\narctand(%f)= %f',[angleDegrees tand(angleDegrees) tand(angleDegrees) atand(tand(angleDegrees))]));
end
```


{{out}}

```MATLAB>>
 trigExample(78)
sin(1.361357)= 0.978148
asin(0.978148)= 1.361357
sind(78.000000)= 0.978148
arcsind(0.978148)= 78.000000
-----------------------
cos(1.361357)= 0.207912
acos(0.207912)= 1.361357
cosd(78.000000)= 0.207912
arccosd(0.207912)= 78.000000
-----------------------
tan(1.361357)= 4.704630
atan(4.704630)= 1.361357
tand(78.000000)= 4.704630
arctand(4.704630)= 78.000000
```



## Maxima


```maxima
a: %pi / 3;
[sin(a), cos(a), tan(a), sec(a), csc(a), cot(a)];

b: 1 / 2;
[asin(b), acos(b), atan(b), asec(1 / b), acsc(1 / b), acot(b)];

/* Hyperbolic functions are also available */
a: 1 / 2;
[sinh(a), cosh(a), tanh(a), sech(a), csch(a), coth(a)], numer;
[asinh(a), acosh(1 / a), atanh(a), asech(a), acsch(a), acoth(1 / a)], numer;
```



## MAXScript

Maxscript trigonometric functions accept degrees only. The built-ins degToRad and radToDeg allow easy conversion.

```maxscript
local radians = pi / 4
local degrees = 45.0

--sine
print (sin (radToDeg radians))
print (sin degrees)
--cosine
print (cos (radToDeg radians))
print (cos degrees)
--tangent
print (tan (radToDeg radians))
print (tan degrees)
--arcsine
print (asin (sin (radToDeg radians)))
print (asin (sin degrees))
--arccosine
print (acos (cos (radToDeg radians)))
print (acos (cos degrees))
--arctangent
print (atan (tan (radToDeg radians)))
print (atan (tan degrees))
```



## Metafont


Metafont has <code>sind</code> and <code>cosd</code>, which compute sine and cosine of an angle expressed in degree. We need to define the rest.


```metafont
Pi := 3.14159;
vardef torad expr x = Pi*x/180 enddef;       % conversions
vardef todeg expr x = 180x/Pi enddef;
vardef sin expr x = sind(todeg(x)) enddef;   % radians version of sind
vardef cos expr x = cosd(todeg(x)) enddef;   % and cosd

vardef sign expr x = if x>=0: 1 else: -1 fi enddef; % commodity

vardef tand expr x =                      % tan with arg in degree
  if cosd(x) = 0:
    infinity * sign(sind(x))
  else: sind(x)/cosd(x) fi enddef;
vardef tan expr x = tand(todeg(x)) enddef; % arg in rad

% INVERSE

% the arc having x as tanget is that between x-axis and a line
% from the center to the point (1, x); MF angle says this
vardef atand expr x = angle(1,x) enddef;
vardef atan expr x = torad(atand(x))  enddef;  % rad version

% known formula to express asin and acos in function of
% atan; a+-+b stays for sqrt(a^2 - b^2) (defined in plain MF)
vardef asin expr x =  2atan(x/(1+(1+-+x)))  enddef;
vardef acos expr x =  2atan((1+-+x)/(1+x))  enddef;

vardef asind expr x =  todeg(asin(x))  enddef; % degree versions
vardef acosd expr x =  todeg(acos(x))  enddef;

% commodity
def outcompare(expr a, b) = message decimal a & " = " & decimal b enddef;

% output tests
outcompare(torad(60), Pi/3);
outcompare(todeg(Pi/6), 30);

outcompare(Pi/3, asin(sind(60)));
outcompare(30, acosd(cos(Pi/6)));
outcompare(45, atand(tand(45)));
outcompare(Pi/4, atan(tand(45)));

outcompare(sin(Pi/3), sind(60));
outcompare(cos(Pi/4), cosd(45));
outcompare(tan(Pi/3), tand(60));

end
```



## MiniScript


```MiniScript
pi3 = pi/3
degToRad = pi/180
print "sin PI/3 radians = " + sin(pi3)
print "sin 60 degrees = " + sin(60*degToRad)
print "arcsin 0.5 in radians = " + asin(0.5)
print "arcsin 0.5 in degrees = " + asin(0.5)/degToRad
print "cos PI/3 radians = " + cos(pi3)
print "cos 60 degrees = " + cos(60*degToRad)
print "arccos 0.5 in radians = " + acos(0.5)
print "arccos 0.5 in degrees = " + acos(0.5)/degToRad
print "tan PI/3 radians = " + tan(pi3)
print "tan 60 degrees = " + tan(60*degToRad)
print "arctan 0.5 in radians = " + atan(0.5)
print "arctan 0.5 in degrees = " + atan(0.5)/degToRad
```

{{out}}

```txt

sin PI/3 radians = 0.866025
sin 60 degrees = 0.866025
arcsin 0.5 in radians = 0.523599
arcsin 0.5 in degrees = 30.0
cos PI/3 radians = 0.5
cos 60 degrees = 0.5
arccos 0.5 in radians = 1.047198
arccos 0.5 in degrees = 60.0
tan PI/3 radians = 1.732051
tan 60 degrees = 1.732051
arctan 0.5 in radians = 0.463648
arctan 0.5 in degrees = 26.565051

```


=={{header|МК-61/52}}==

```txt

sin	С/П	Вx	cos	С/П	Вx	tg	С/П	Вx	arcsin
С/П	Вx	arccos	С/П	Вx	arctg	С/П

```


Setting the units of angle (degrees, radians, grads) takes care of the switch ''Р-ГРД-Г''.

=={{header|Modula-2}}==

```modula2
MODULE Trig;
FROM RealMath IMPORT pi,sin,cos,tan,arctan,arccos,arcsin;
FROM RealStr IMPORT RealToStr;
FROM Terminal IMPORT WriteString,WriteLn,ReadChar;

PROCEDURE WriteReal(v : REAL);
VAR buf : ARRAY[0..31] OF CHAR;
BEGIN
    RealToStr(v, buf);
    WriteString(buf)
END WriteReal;

VAR theta : REAL;
BEGIN
    theta := pi / 4.0;

    WriteString("theta: ");
    WriteReal(theta);
    WriteLn;

    WriteString("sin: ");
    WriteReal(sin(theta));
    WriteLn;

    WriteString("cos: ");
    WriteReal(cos(theta));
    WriteLn;

    WriteString("tan: ");
    WriteReal(tan(theta));
    WriteLn;

    WriteString("arcsin: ");
    WriteReal(arcsin(sin(theta)));
    WriteLn;

    WriteString("arccos: ");
    WriteReal(arccos(cos(theta)));
    WriteLn;

    WriteString("arctan: ");
    WriteReal(arctan(tan(theta)));
    WriteLn;

    ReadChar
END Trig.
```



## NetRexx


```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary utf8

numeric digits 30

parse 'Radians Degrees angle' RADIANS DEGREES ANGLE .;
parse 'sine cosine tangent arcsine arccosine arctangent' SINE COSINE TANGENT ARCSINE ARCCOSINE ARCTANGENT .

trigVals = ''
trigVals[RADIANS, ANGLE     ] = (Rexx Math.PI) / 4 -- Pi/4 == 45 degrees
trigVals[DEGREES, ANGLE     ] = 45.0
trigVals[RADIANS, SINE      ] = (Rexx Math.sin(trigVals[RADIANS, ANGLE]))
trigVals[DEGREES, SINE      ] = (Rexx Math.sin(Math.toRadians(trigVals[DEGREES, ANGLE])))
trigVals[RADIANS, COSINE    ] = (Rexx Math.cos(trigVals[RADIANS, ANGLE]))
trigVals[DEGREES, COSINE    ] = (Rexx Math.cos(Math.toRadians(trigVals[DEGREES, ANGLE])))
trigVals[RADIANS, TANGENT   ] = (Rexx Math.tan(trigVals[RADIANS, ANGLE]))
trigVals[DEGREES, TANGENT   ] = (Rexx Math.tan(Math.toRadians(trigVals[DEGREES, ANGLE])))
trigVals[RADIANS, ARCSINE   ] = (Rexx Math.asin(trigVals[RADIANS, SINE]))
trigVals[DEGREES, ARCSINE   ] = (Rexx Math.toDegrees(Math.acos(trigVals[DEGREES, SINE])))
trigVals[RADIANS, ARCCOSINE ] = (Rexx Math.acos(trigVals[RADIANS, COSINE]))
trigVals[DEGREES, ARCCOSINE ] = (Rexx Math.toDegrees(Math.acos(trigVals[DEGREES, COSINE])))
trigVals[RADIANS, ARCTANGENT] = (Rexx Math.atan(trigVals[RADIANS, TANGENT]))
trigVals[DEGREES, ARCTANGENT] = (Rexx Math.toDegrees(Math.atan(trigVals[DEGREES, TANGENT])))

say        ' '.right(12)'|' RADIANS.right(17)                           '|' DEGREES.right(17)                           '|'
say      ANGLE.right(12)'|' trigVals[RADIANS, ANGLE     ].format(4, 12) '|' trigVals[DEGREES, ANGLE     ].format(4, 12) '|'
say       SINE.right(12)'|' trigVals[RADIANS, SINE      ].format(4, 12) '|' trigVals[DEGREES, SINE      ].format(4, 12) '|'
say     COSINE.right(12)'|' trigVals[RADIANS, COSINE    ].format(4, 12) '|' trigVals[DEGREES, COSINE    ].format(4, 12) '|'
say    TANGENT.right(12)'|' trigVals[RADIANS, TANGENT   ].format(4, 12) '|' trigVals[DEGREES, TANGENT   ].format(4, 12) '|'
say    ARCSINE.right(12)'|' trigVals[RADIANS, ARCSINE   ].format(4, 12) '|' trigVals[DEGREES, ARCSINE   ].format(4, 12) '|'
say  ARCCOSINE.right(12)'|' trigVals[RADIANS, ARCCOSINE ].format(4, 12) '|' trigVals[DEGREES, ARCCOSINE ].format(4, 12) '|'
say ARCTANGENT.right(12)'|' trigVals[RADIANS, ARCTANGENT].format(4, 12) '|' trigVals[DEGREES, ARCTANGENT].format(4, 12) '|'
say

return

```


{{out}}

```txt

            |           Radians |           Degrees |
       angle|    0.785398163397 |   45.000000000000 |
        sine|    0.707106781187 |    0.707106781187 |
      cosine|    0.707106781187 |    0.707106781187 |
     tangent|    1.000000000000 |    1.000000000000 |
     arcsine|    0.785398163397 |   45.000000000000 |
   arccosine|    0.785398163397 |   45.000000000000 |
  arctangent|    0.785398163397 |   45.000000000000 |

```



## Nim


```nim
import math

proc radians(x): float = x * Pi / 180
proc degrees(x): float = x * 180 / Pi

let rad = Pi/4
let deg = 45.0

echo "Sine: ", sin(rad), " ", sin(radians(deg))
echo "Cosine : ", cos(rad), " ", cos(radians(deg))
echo "Tangent: ", tan(rad), " ", tan(radians(deg))
echo "Arcsine: ", arcsin(sin(rad)), " ", degrees(arcsin(sin(radians(deg))))
echo "Arccocose: ", arccos(cos(rad)), " ", degrees(arccos(cos(radians(deg))))
echo "Arctangent: ", arctan(tan(rad)), " ", degrees(arctan(tan(radians(deg))))
```



## OCaml


OCaml's preloaded <tt>Pervasives</tt> module contains all six functions. The functions all accept radians only, so conversion is necessary when dealing with degrees.

```ocaml
let pi = 4. *. atan 1.

let radians = pi /. 4.
let degrees = 45.;;

Printf.printf "%f %f\n" (sin radians) (sin (degrees *. pi /. 180.));;
Printf.printf "%f %f\n" (cos radians) (cos (degrees *. pi /. 180.));;
Printf.printf "%f %f\n" (tan radians) (tan (degrees *. pi /. 180.));;
let arcsin = asin (sin radians);;
Printf.printf "%f %f\n" arcsin (arcsin *. 180. /. pi);;
let arccos = acos (cos radians);;
Printf.printf "%f %f\n" arccos (arccos *. 180. /. pi);;
let arctan = atan (tan radians);;
Printf.printf "%f %f\n" arctan (arctan *. 180. /. pi);;
```

{{out}}

```txt

0.707107 0.707107
0.707107 0.707107
1.000000 1.000000
0.785398 45.000000
0.785398 45.000000
0.785398 45.000000

```



## Octave



```octave
function d = degree(rad)
  d = 180*rad/pi;
endfunction

r = pi/3;
rd = degree(r);

funcs = { "sin", "cos", "tan", "sec", "cot", "csc" };
ifuncs = { "asin", "acos", "atan", "asec", "acot", "acsc" };

for i = 1 : numel(funcs)
  v = arrayfun(funcs{i}, r);
  vd = arrayfun(strcat(funcs{i}, "d"), rd);
  iv = arrayfun(ifuncs{i}, v);
  ivd = arrayfun(strcat(ifuncs{i}, "d"), vd);
  printf("%s(%f) = %s(%f) = %f (%f)\n",
                    funcs{i}, r, strcat(funcs{i}, "d"), rd, v, vd);
  printf("%s(%f) = %f\n%s(%f) = %f\n",
                    ifuncs{i}, v, iv,
                    strcat(ifuncs{i}, "d"), vd, ivd);
endfor
```


{{out}}

```txt
sin(1.047198) = sind(60.000000) = 0.866025 (0.866025)
asin(0.866025) = 1.047198
asind(0.866025) = 60.000000
cos(1.047198) = cosd(60.000000) = 0.500000 (0.500000)
acos(0.500000) = 1.047198
acosd(0.500000) = 60.000000
tan(1.047198) = tand(60.000000) = 1.732051 (1.732051)
atan(1.732051) = 1.047198
atand(1.732051) = 60.000000
sec(1.047198) = secd(60.000000) = 2.000000 (2.000000)
asec(2.000000) = 1.047198
asecd(2.000000) = 60.000000
cot(1.047198) = cotd(60.000000) = 0.577350 (0.577350)
acot(0.577350) = 1.047198
acotd(0.577350) = 60.000000
csc(1.047198) = cscd(60.000000) = 1.154701 (1.154701)
acsc(1.154701) = 1.047198
acscd(1.154701) = 60.000000
```


(Lacking in this code but present in GNU Octave: sinh, cosh, tanh, coth and inverses)



## Oforth



```Oforth
import: math

: testTrigo
| rad deg hyp z |
   Pi 4 / ->rad
   45.0   ->deg
    0.5   ->hyp

   System.Out rad sin << " - " << deg asRadian sin << cr
   System.Out rad cos << " - " << deg asRadian cos << cr
   System.Out rad tan << " - " << deg asRadian tan << cr

   printcr

   rad sin asin ->z
   System.Out z << " - " << z asDegree << cr

   rad cos acos ->z
   System.Out z << " - " << z asDegree << cr

   rad tan atan ->z
   System.Out z << " - " << z asDegree << cr

   printcr

   System.Out hyp sinh << " - " << hyp sinh asinh << cr
   System.Out hyp cosh << " - " << hyp cosh acosh << cr
   System.Out hyp tanh << " - " << hyp tanh atanh << cr ;
```


{{out}}

```txt

0.707106781186547 - 0.707106781186547
0.707106781186548 - 0.707106781186548
1 - 1

0.785398163397448 - 45
0.785398163397448 - 45
0.785398163397448 - 45

0.521095305493747 - 0.5
1.12762596520638 - 0.5
0.46211715726001 - 0.5

```



## ooRexx


```txt

rxm.cls                                                    20 March 2014

The distribution of ooRexx contains a function package called rxMath
that provides the computation of trigonometric and some other functions.
Based on the underlying C-library the precision of the returned values
is limited to 16 digits. Close observation show that sometimes the last
one to three digits of the returned values are not correct.
Many years ago I experimented with implementing these functions in Rexx
with its virtually unlimited precision.
The rxm class is intended to provide the same functionality as rxMath
with no limit on the specified or implied precision.

Functions in class rxm and invocation syntax are the same as
in the rxMath library. They are implemented as routines which
perform the checking of argument values and invoke the corresponding
methods. Here is a list of the supported functions and a concise
syntax specification.

The arguments are represented by these letters:

x is the value for which the respective function must be evaluated.
b and c for RxCalcPower are base and exponent, respectively.

p if specified is the desired precision (number of digits) in the result.
  It can be any integer from 1 to 999999.
  See below for the default used.

u if specified, is the unit of x given to the trigonometric functions
  or the unit of the value returned by the Arcus functions.
  It can be 'R', 'D', or 'G' for radians, degrees, or grades, respectively.
  See below for the default used.

Trigonometric functions:

• rxmCos(x[,[p][,u]])
• rxmCotan(x[,[p][,u]])
• rxmSin(x[,[p][,u]])
• rxmTan(x[,[p][,u]])

Arcus functions:

• rxmArcCos(x[,[p][,u]])
• rxmArcSin(x[,[p][,u]])
• rxmArcTan(x[,[p][,u]])

Hyperbolic functions:

• rxmCosH(x[,p])
• rxmSinH(x[,p])
• rxmTanH(x[,p])

• rxmExp(x[,p])      e**x
• rxmLog(x[,p])      Natural logarithm of x
• rxmLog10(x[,p])    Brigg's logarithm of x
• rxmSqrt(x[,p])     Square root of x

• rxmPower(b,c[,p])  b**c

• rxmPi([p])         pi to the specified or default precision

Values used for p and u if these are omitted in the invocation

### ========================================================


The directive ::REQUIRES rxm.cls creates an instance of the class
  .local~my.rxm=.rxm~new(16,"D")
which sets the defaults for p=16 and u='D'.
These are used when p or u are omitted in a function invocation.
They can be changed by changing the respective class attributes as follows:
  .locaL~my.rxm~precision=50
  .locaL~my.rxm~type='R'
The current setting of these attributes can be retrieved as follows:
  .locaL~my.rxm~precision()
  .locaL~my.rxm~type()

While I tried to get full compatibility there remain a few
(actually very few) differences:

  rxCalcTan(90) raises the Syntax condition (will be fixed in the next ooRexx release)
  rxCalcexp(x) limits x to 709. or so and returns '+infinity' for larger exponents

```


```oorexx
/* REXX ---------------------------------------------------------------
* show how the functions can be used
* 03.05.2014 Walter Pachl
*--------------------------------------------------------------------*/
Say 'Default precision:'  .locaL~my.rxm~precision()
Say 'Default type:     '  .locaL~my.rxm~type()
Say 'rxmsin(60)      ='rxmsin(60)     -- use default precision and type
Say 'rxmsin(1,21,"R")='rxmsin(1,21,'R') -- precision and type specified
Say 'rxmlog(-1)      ='rxmlog(-1)
Say 'rxmlog( 0)      ='rxmlog( 0)
Say 'rxmlog( 1)      ='rxmlog( 1)
Say 'rxmlog( 2)      ='rxmlog( 2)
  .locaL~my.rxm~precision=50
  .locaL~my.rxm~type='R'
Say 'Changed precision:'    .locaL~my.rxm~precision()
Say 'Changed type:     '    .locaL~my.rxm~type()
Say 'rxmsin(1)       ='rxmsin(1)        -- use changed precision and type
::requires rxm.cls
```

{{out}}

```txt
Default precision: 16
Default type:      D
rxmsin(60)      =0.8660254037844386
rxmsin(1,21,"R")=0.841470984807896506653
rxmlog(-1)      =nan
rxmlog( 0)      =-infinity
rxmlog( 1)      =0
rxmlog( 2)      =0.6931471805599453
Changed precision: 50
Changed type:      R
rxmsin(1)       =0.84147098480789650665250232163029899962256306079837
```



```oorexx
/********************************************************************
* Package rxm
* implements the functions available in RxMath with high precision
* by computing the values with significantly increased precision
* and rounding the result to the specified precision.
* This started 10 years ago when Vladimir Zabrodsky published his
* Album of Algorithms http://dhost.info/zabrodskyvlada/aat/
* Gerard Schildberger suggests on rosettacode.org to use +10 digits
* Rony Flatscher suggested and helped to turn this into an ooRexx class
* Rick McGuire advised on using Use STRICT Arg for argument checking
* Alexander Seik creates this documentation
* Horst Wegscheider helped with reviewing and some improvements
* 12.04.2014 Walter Pachl
*            Documentation: see rxmath.pdf in the ooRexx distribution
*                           and rxm.doc (here)
* 13.04.2014 WP arcsin and arctan commentary corrected (courtesy Horst)
* 13.04.2014 WP improve arctan performance
* 20.04.2014 WP towards completion
* 24.04.2014 WP arcsin verbessert. courtesy Horst Wegscheider
* 28.04.2014 WP run ooRexxDoc
* 11.08.2014 WP replace log algorithm with Vladimir Zabrodsky's code
**********************************************************************/
.local~my.rxm=.rxm~new(16,"D")

::Class rxm Public

::Method init
  Expose precision type
  Use Arg  precision=(digits()),type='D'

::attribute precision set
  Expose precision
  Use Strict Arg  precision=(digits())

::attribute precision get

::attribute type set
  Expose type
  Use Strict Arg type='R'

::attribute type get

::Method arccos
/***********************************************************************
* Return arccos(x,precision,type) -- with specified precision
* arccos(x) = pi/2 - arcsin(x)
***********************************************************************/
  Expose precision type
  Use Strict Arg x,xprec=(precision),xtype=(type)
  iprec=xprec+10
  Numeric Digits iprec
  If x=1 Then
    r=0
  Else Do
    r=self~arcsin(x,iprec,'R')
    If r='nan' Then
      Return r
    r=self~pi(iprec)/2 - r
    End
  Select
    When xtype='D' Then
      r=r*180/self~pi(iprec)
    When xtype='G' Then
      r=r*200/self~pi(iprec)
    Otherwise
      Nop
    End
  Numeric Digits xprec
  Return (r+0)

::Method arcsin
/***********************************************************************
* Return arcsin(x,precision,type) -- with specified precision
* arcsin(x) = x+(x**3)*1/2*3+(x**5)*1*3/2*4*5+(x**7)*1*3*5/2*4*6*7+...
***********************************************************************/
  Expose precision type
  Use Strict Arg x,xprec=(precision),xtype=(type)
  iprec=xprec+10
  Numeric Digits iprec
  sign=sign(x)
  If x<0 Then
    x=abs(x)
  Select
    When abs(x)>1 Then
      Return 'nan'
    When x=0 Then
      r=0
    When x=1 Then
      r=rxmpi(iprec)/2
    When x<0.8 Then Do
      o=x
      u=1
      r=x
      Do i=3 By 2 Until ra=r
        ra=r
        o=o*x*x*(i-2)
        u=u*(i-1)*i/(i-2)
        r=r+(o/u)
        If r=ra Then
          r=r+(o/u)/2 /* final touch */
        End
      End
    Otherwise Do
      z=x
      r=x
      o=x
      s=x*x
      do j=2 by 2;
        o=o*s*(j-1)/j;
        z=z+o/(j+1);
        if z=r then
          leave
        r=z;
        end
      /***********************
      y=(1-x*x)/4
      n=0.5-self~sqrt(y,iprec)
      z=self~sqrt(n,iprec)
      r=2*self~arcsin(z,xprec)
      ***********************/
      End
    End
  Select
    When xtype='D' Then
      r=r*180/self~pi(iprec)
    When xtype='G' Then
      r=r*200/self~pi(iprec)
    Otherwise
      Nop
    End
  Numeric Digits xprec
  Return sign*(r+0)

::Method arctan
/***********************************************************************
* Return arctan(x,precision,type) -- with specified precision
* x=0 -> arctan(x) = 0
* If x>0 Then
*   x<1 -> arctan(x) = arcsin(x/sqrt(x**2+1))
*   x=1 -> arctan(x) = pi/4
*   x>1 -> arctan(x) = pi/2-arcsin((1/x)/sqrt((1/x)**2+1))
* Else
*   adjust as necessary
***********************************************************************/
  Expose precision type
  Use Strict Arg x,xprec=(precision),xtype=(type)
  iprec=xprec+10
  Numeric Digits iprec
  Select
    When abs(x)<1 Then
      r=self~arcsin(x/self~sqrt(1+x**2,iprec),iprec,'R')
    When abs(x)=1 Then
      r=self~pi(iprec)/4*sign(x)
    Otherwise Do
      xr=1/abs(x)
      r=self~arcsin(xr/self~sqrt(1+xr**2,iprec),iprec,'R')
      If x>0 Then
        r=self~pi(iprec)/2-r
      Else
        r=-self~pi(iprec)/2+r
      End
    End
  Select
    When xtype='D' Then
      r=r*180/self~pi(iprec)
    When xtype='G' Then
      r=r*200/self~pi(iprec)
    Otherwise
      Nop
    End
  Numeric Digits xprec
  Return (r+0)

::Method arsinh
/***********************************************************************
* Return arsinh(x,precision,type) -- with specified precision
* arsinh(x) = ln(x+sqrt(x**2+1))
***********************************************************************/
  Expose precision
  Use Strict Arg x,xprec=(precision)
  iprec=xprec+10
  Numeric Digits iprec
  x2p1=x**2+1
  r=self~log(x+self~sqrt(x2p1,iprec),iprec)
  Numeric Digits xprec
  Return (r+0)

::Method cos
/* REXX *************************************************************
* Return cos(x,precision,type) -- with the specified precision
* cos(x)=sin(x+pi/2)
********************************************************************/
  Expose precision type
  Use Strict Arg x,xprec=(precision),xtype=(type)
  iprec=xprec+10
  Numeric Digits iprec
  Select
    When xtype='R' Then xa=x+self~pi(iprec)/2
    When xtype='D' Then xa=x+90
    When xtype='G' Then xa=x+100
    End
  r=self~sin(xa,iprec,xtype)
  Numeric Digits xprec
  Return (r+0)

::Method cosh
/* REXX ****************************************************************
* Return cosh(x,precision,type) -- with specified precision
* cosh(x) = 1+(x**2/2!)+(x**4/4!)+(x**6/6!)+-...
***********************************************************************/
  Expose precision
  Use Strict Arg x,xprec=(precision)
  iprec=xprec+10
  Numeric Digits iprec
  o=1
  u=1
  r=1
  Do i=2 By 2 Until ra=r
    ra=r
    o=o*x*x
    u=u*i*(i-1)
    r=r+(o/u)
    End
  Numeric Digits xprec
  Return (r+0)

::Method cotan
/* REXX *************************************************************
* Return cotan(x,precision,type) -- with the specified precision
* cot(x)=cos(x)/sin(x)
********************************************************************/
  Expose precision type
  Use Strict Arg x,xprec=(precision),xtype=(type)
  iprec=xprec+10
  Numeric Digits iprec
  s=self~sin(x,iprec,xtype)
  c=self~cos(x,iprec,xtype)
  If s=0 Then
    Return '+infinity'
  r=c/s
  Numeric Digits xprec
  Return (r+0)

::Method exp
/***********************************************************************
* exp(x,precision) returns e**x -- with specified precision
* exp(x,precision,base) returns base**x -- with specified precision
***********************************************************************/
  Expose precision
  Use Strict Arg x,xprec=(precision),xbase=''
  iprec=xprec+10
  Numeric Digits iprec
  Numeric Fuzz   3
  If xbase<>'' Then Do
    Select
      When xbase=0 Then Do
        Select
          When x<0 Then Return '+infinity'
          When x=0 Then Return 'nan'
          Otherwise Return 0
          End
        End
      When xbase=1 Then Return 1
      When xbase<0 Then Do
        Select
          When x=0 Then Return 1
          When datatype(x,'W')=0 Then Return 'nan'
          Otherwise Do
            r=xbase**x
            Numeric Digits xprec
            Return r+0
            End
          End
        End
      Otherwise
        x=x*self~log(xbase,iprec)
      End
    End
  o=1
  u=1
  r=1
  Do i=1 By 1 Until ra=r
    ra=r
    o=o*x
    u=u*i
    r=r+(o/u)
    End
  Numeric Digits xprec
  Return (r+0)

::Method log
/***********************************************************************
* log(x,precision) -- returns ln(x) with specified precision
* log(x,precision,base) -- returns blog(x) with specified precision
* Three different series are used for ln(x): x in range  0 to 0.5
*                                                      0.5 to 1.5
*                                                      1.5 to infinity
***********************************************************************/
  Expose precision
  Use Strict Arg x,xprec=(precision),xbase=''
  iprec=xprec+100
  Numeric Digits iprec
  Select
    When x=0 Then Return '-infinity'
    When x<0 Then Return 'nan'
    When x<1 Then  r= -self~Log(1/X,xprec)
    Otherwise Do
      do M = 0 until (2 ** M) > X; end
      M = M - 1
      Z = X / (2 ** M)
      Zeta = (1 - Z) / (1 + Z)
      N = Zeta; Ln = Zeta; Zetasup2 = Zeta * Zeta
      do J = 1
        N = N * Zetasup2; NewLn = Ln + N / (2 * J + 1)
        if NewLn = Ln then Do
          r= M * self~LN2P(xprec) - 2 * Ln
          Leave
          End
        Ln = NewLn
        end
      End
    End
  If x>0 Then Do
    If xbase>'' Then
      r=r/self~log(xbase,iprec)
  Numeric Digits xprec
    r=r+0
    End
  Return r

::Method ln2p
  Parse Arg p
  Numeric Digits p+10
  If p<=1000 Then
    Return self~ln2()
  n=1/3
  ln=n
  zetasup2=1/9
  Do j=1
    n=n*zetasup2
    newln=ln+n/(2*j+1)
    If newln=ln Then
      Return 2*ln
    ln=newln
    End

::Method LN2
    V = ''
    V = V || 0.69314718055994530941723212145817656807
    V = V || 5500134360255254120680009493393621969694
    V = V || 7156058633269964186875420014810205706857
    V = V || 3368552023575813055703267075163507596193
    V = V || 0727570828371435190307038623891673471123350
v=''
v=v||0.69314718055994530941723212145817656807
v=v||5500134360255254120680009493393621969694
v=v||7156058633269964186875420014810205706857
v=v||3368552023575813055703267075163507596193
v=v||0727570828371435190307038623891673471123
v=v||3501153644979552391204751726815749320651
v=v||5552473413952588295045300709532636664265
v=v||4104239157814952043740430385500801944170
v=v||6416715186447128399681717845469570262716
v=v||3106454615025720740248163777338963855069
v=v||5260668341137273873722928956493547025762
v=v||6520988596932019650585547647033067936544
v=v||3254763274495125040606943814710468994650
v=v||6220167720424524529612687946546193165174
v=v||6813926725041038025462596568691441928716
v=v||0829380317271436778265487756648508567407
v=v||7648451464439940461422603193096735402574
v=v||4460703080960850474866385231381816767514
v=v||3866747664789088143714198549423151997354
v=v||8803751658612753529166100071053558249879
v=v||4147295092931138971559982056543928717000
v=v||7218085761025236889213244971389320378439
v=v||3530887748259701715591070882368362758984
v=v||2589185353024363421436706118923678919237
v=v||231467232172053401649256872747782344535348

    return V

::Method log10
/***********************************************************************
* Return log10(x,prec)  specified precision
***********************************************************************/
  Expose precision
  Use Strict Arg x,xprec=(precision)
  iprec=xprec+10
  r=self~log(x,iprec,10)
  Numeric Digits xprec
  Return (r+0)

::Method pi
/* REXX *************************************************************
* Return pi with the specified precision
********************************************************************/
  Expose precision
  Use Strict Arg xprec=(precision)
  p='3.141592653589793238462643383279502884197169399375'||,
    '10582097494459230781640628620899862803482534211706'||,
    '79821480865132823066470938446095505822317253594081'||,
    '28481117450284102701938521105559644622948954930381'||,
    '96442881097566593344612847564823378678316527120190'||,
    '91456485669234603486104543266482133936072602491412'||,
    '73724587006606315588174881520920962829254091715364'||,
    '36789259036001133053054882046652138414695194151160'||,
    '94330572703657595919530921861173819326117931051185'||,
    '48074462379962749567351885752724891227938183011949'||,
    '12983367336244065664308602139494639522473719070217'||,
    '98609437027705392171762931767523846748184676694051'||,
    '32000568127145263560827785771342757789609173637178'||,
    '72146844090122495343014654958537105079227968925892'||,
    '35420199561121290219608640344181598136297747713099'||,
    '60518707211349999998372978049951059731732816096318'||,
    '59502445945534690830264252230825334468503526193118'||,
    '81710100031378387528865875332083814206171776691473'||,
    '03598253490428755468731159562863882353787593751957'||,
    '781857780532171226806613001927876611195909216420199'
  If xprec>1000 Then Do              /* more than 1000 digits wanted */
    iprec=xprec+10                     /* internal precision         */
    Numeric Digits iprec
    new=1
    a=sqrt(2,iprec)
    b=0
    p=2+a
    Do i=1 By 1 Until p=pi
      pi=p
      y=self~sqrt(a,iprec)
      a1=(y+1/y)/2
      b1=y*(b+1)/(b+a)
      p=pi*b1*(1+a1)/(1+b1)
      a=a1
      b=b1
      End
    End
  Numeric Digits xprec
  Return (p+0)

::Method power
/***********************************************************************
* power(base,exponent,precision) returns base**exponent
*                                            -- with specified precision
***********************************************************************/
  Expose precision
  Use Strict Arg b,c,xprec=(precision)
  Numeric Digits xprec
  rsign=1
  If b<0 Then Do                       /* negative base              */
    If datatype(c,'W') Then Do         /* Exponent is an integer     */
      If c//2=1 Then                   /* .. an odd number           */
        rsign=-1                       /* Resuld will be negative    */
      b=abs(b)                         /* proceed with positive base */
      End
    Else Do                            /* Exponent is not an integer */
--    Say 'for a negative base ('||b')',
                           'exponent ('c') must be an integer'
      Return 'nan'                     /* Return not a number        */
      End
    End
  If c=0 Then Do
    If b>=0 Then
      r=1
    End
  Else
    r=self~exp(c,xprec,b)
  If datatype(r)<>'NUM' Then
    Return r
  Return rsign*r

::Method sqrt
/* REXX *************************************************************
* Return sqrt(x,precision) -- with the specified precision
********************************************************************/
  Expose precision type
  Use Strict Arg x,xprec=(precision)
  If x<0 Then Do
    Return 'nan'
    End
  iprec=xprec+10
  Numeric Digits iprec
  r0= x
  r = 1
  Do i=1 By 1 Until r=r0 | (abs(r*r-x)<10**-iprec)
    r0 = r
    r  = (r + x/r) / 2
    End
  Numeric Digits xprec
  Return (r+0)

::Method sin
/* REXX *************************************************************
* Return sin(x,precision,type) -- with the specified precision
* xtype = 'R' (radians, default) 'D' (degrees) 'G' (grades)
* sin(x) = x-(x**3/3!)+(x**5/5!)-(x**7/7!)+-...
********************************************************************/
  Expose precision type
  Use Strict Arg x,xprec=(precision),xtype=(type)
  iprec=xprec+10                       /* internal precision         */
  Numeric Digits iprec
  /* first use pi constant or compute it if necessary                */
  pi=self~pi(iprec)
  /* normalize x to be between 0 and 2*pi (or equivalent)            */
  /* and convert degrees or grades to radians                        */
  xx=x
  Select
    When xtype='R' Then Do
      Do While xx>=pi*2; xx=xx-pi*2; End
      Do While xx<0;     xx=xx+pi*2; End
      End
    When xtype='D' Then Do
      Do While xx>=360;  xx=xx-360; End
      Do While xx<0;     xx=xx+360; End
      xx=xx*pi/180
      End
    When xtype='G' Then Do
      Do While xx>=400;  xx=xx-400; End
      Do While xx<0;     xx=xx+400; End
      xx=xx*pi/200
      End
    End
  /* normalize xx to be between 0 and pi/2                            */
  sign=1
  Select
    When xx<=pi/2   Then Nop
    When xx<=pi     Then xx=pi-xx
    When xx<=3*pi/2 Then Do; sign=-1; xx=xx-pi; End
    Otherwise            Do; sign=-1; xx=2*pi-xx; End
    End
  /* now compute the Taylor series for the normalized xx             */
  o=xx
  u=1
  r=xx
  If abs(xx)<10**(-iprec) Then
    r=0
  Else Do
    Do i=3 By 2 Until ra=r
      ra=r
      o=-o*xx*xx
      u=u*i*(i-1)
      r=r+(o/u)
      End
    End
  Numeric Digits xprec
  Return sign*(r+0)

::Method sinh
/* REXX ****************************************************************
* Return sinh(x,precision) -- with specified precision
* sinh(x) = x+(x**3/3!)+(x**5/5!)+(x**7/7!)+-...
* 920903 Walter Pachl
***********************************************************************/
  Expose precision
  Use Strict Arg x,xprec=(precision)
  iprec=xprec+10
  Numeric Digits iprec
  o=x
  u=1
  r=x
  Do i=3 By 2 Until ra=r
    ra=r
    o=o*x*x
    u=u*i*(i-1)
    r=r+(o/u)
    End
  Numeric Digits xprec
  Return (r+0)

::Method tan
/* REXX *************************************************************
* Return tan(x,precision,type) -- with the specified precision
* tan(x)=sin(x)/cos(x)
********************************************************************/
  Expose precision type
  Use Strict Arg x,xprec=(precision),xtype=(type)
  iprec=xprec+10
  Numeric Digits iprec
  s=self~sin(x,iprec,xtype)
  c=self~cos(x,iprec,xtype)
  If c=0 Then
    Return '+infinity'
  t=s/c
  Numeric Digits xprec
  Return (t+0)

::Method tanh
/***********************************************************************
* Return tanh(x,precision) -- with specified precision
* tanh(x) = sinh(x)/cosh(x)
***********************************************************************/
  Expose precision
  Use Strict Arg x,xprec=(precision)
  iprec=xprec+10
  Numeric Digits iprec
  r=self~sinh(x,iprec)/self~cosh(x,iprec)
  Numeric Digits xprec
  Return (r+0)

::routine rxmarccos public
  Use Strict Arg x,xprec=(.my.rxm~precision),xtype=(.my.rxm~type)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a positive whole number'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  If x<-1 | 1<x Then
    Return 'nan'

  return .my.rxm~arccos(x,xprec,xtype)

::routine rxmarcsin public
  Use Strict Arg x,xprec=(.my.rxm~precision),xtype=(.my.rxm~type)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a positive whole number'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  If wordpos(xtype,'R D G')=0 Then Do
--  Say 'Argument 3 must be R, D, or G'
    Raise Syntax 88.907 array(3,'R, D, or G',xtype)
    End

  If x<-1 | 1<x Then
    Return 'nan'

  return .my.rxm~arcsin(x,xprec,xtype)

::routine rxmarctan public
  Use Strict Arg x,xprec=(.my.rxm~precision),xtype=(.my.rxm~type)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a positive whole number'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  If wordpos(xtype,'R D G')=0 Then Do
--  Say 'Argument 3 must be R, D, or G'
    Raise Syntax 88.907 array(3,'R, D, or G',xtype)
    End

  return .my.rxm~arctan(x,xprec,xtype)

::routine rxmarsinh public
  Use Strict Arg x,xprec=(.my.rxm~precision)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a positive whole number'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  return .my.rxm~arsinh(x,xprec)

::routine rxmcos public
  Use Strict Arg x,xprec=(.my.rxm~precision),xtype=(.my.rxm~type)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a positive whole number'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  If wordpos(xtype,'R D G')=0 Then Do
--  Say 'Argument 3 must be R, D, or G'
    Raise Syntax 88.907 array(3,'R, D, or G',xtype)
    End

  return .my.rxm~cos(x,xprec,xtype)

::routine rxmcosh public
  Use Strict Arg x,xprec=(.my.rxm~precision)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a positive whole number'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  return .my.rxm~cosh(x,xprec)

::routine rxmcotan public
  Use Strict Arg x,xprec=(.my.rxm~precision),xtype=(.my.rxm~type)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a positive whole number'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  If wordpos(xtype,'R D G')=0 Then Do
--  Say 'Argument 3 must be R, D, or G'
    Raise Syntax 88.907 array(3,'R, D, or G',xtype)
    End

  return .my.rxm~cotan(x,xprec)

::routine rxmexp public
  Use Strict Arg x,xprec=(.my.rxm~precision),xbase=''
  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  If datatype(xbase,'NUM')=0 & xbase<>'' Then Do
--  Say 'Argument 3 must be omitted or a number'
    Raise Syntax 88.902 array(3,xbase)
    End

  Select
    When x<0 Then Do
      iprec=xprec+10
      Numeric Digits iprec
      z=.my.rxm~exp(abs(x),iprec,xbase)
      Select
        When z=0 Then Return '+infinity'
        When datatype(z)<>'NUM' Then Return z
        Otherwise r=1/z
        End
      Numeric Digits xprec
      return r+0
      End
    When x=0 Then Do
      If xbase=0 Then
        Return 'nan'
      Else
        Return 1
      End
    Otherwise
      return .my.rxm~exp(x,xprec,xbase)
    End

::routine rxmlog public
  Use Strict Arg x,xprec=(.my.rxm~precision),xbase=''

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  If xbase<>'' &,
     datatype(xbase,'NUM')=0 Then Do
--  Say 'Argument 3 must be a number'
    Raise Syntax 88.902 array(3,xbase)
    End

  If x=0 Then
    Return '-infinity'

  If x<0 Then
    Return 'nan'

  return .my.rxm~log(x,xprec,xbase)

::routine rxmlog10 public
  Use Strict Arg x,xprec=(.my.rxm~precision)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  If x=0 Then
    Return '-infinity'

  If x<0 Then
    Return 'nan'

  return .my.rxm~log10(x,xprec)

::routine rxmpi public
  Use Strict Arg xprec=(.my.rxm~precision)

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  return .my.rxm~pi(xprec)

::routine rxmpower public
  Use Strict Arg b,e,xprec=(.my.rxm~precision)

  If datatype(b,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,b)
    End

  If datatype(e,'NUM')=0 Then Do
--  Say 'Argument 2 must be a number'
    Raise Syntax 88.902 array(2,e)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 3 must be a whole number between 1 and 999999'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 3 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(3,1,999999,xprec)
    End

  If b<0 & datatype(e,'W')=0 Then
    Return 'nan'

  return .my.rxm~power(b,e,xprec)

::routine rxmsqrt public
  Use Strict Arg x,xprec=(.my.rxm~precision)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End
  Select
    When x<0 Then Return 'nan'
    When x=0 Then Return 0
    Otherwise
      return .my.rxm~sqrt(x,xprec)
    End

::routine rxmsin public
  Use Strict Arg x,xprec=(.my.rxm~precision),xtype=(.my.rxm~type)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a positive whole number'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  If wordpos(xtype,'R D G')=0 Then Do
--  Say 'Argument 3 must be R, D, or G'
    Raise Syntax 88.907 array(3,'R, D, or G',xtype)
    End

  return .my.rxm~sin(x,xprec,xtype)

::routine rxmsinh public
  Use Strict Arg x,xprec=(.my.rxm~precision)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a positive whole number'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  return .my.rxm~sinh(x,xprec)

::routine rxmtan public
  Use Strict Arg x,xprec=(.my.rxm~precision),xtype=(.my.rxm~type)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a positive whole number'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  If wordpos(xtype,'R D G')=0 Then Do
--  Say 'Argument 3 must be R, D, or G'
    Raise Syntax 88.907 array(3,'R, D, or G',xtype)
    End

  return .my.rxm~tan(x,xprec,xtype)

::routine rxmtanh public
  Use Strict Arg x,xprec=(.my.rxm~precision)

  If datatype(x,'NUM')=0 Then Do
--  Say 'Argument 1 must be a number'
    Raise Syntax 88.902 array(1,x)
    End

  If datatype(xprec,'W')=0 Then Do
--  Say 'Argument 2 must be a positive whole number'
    Raise Syntax 88.905 array(2,xprec)
    End

  If xprec<1 | 999999<xprec Then Do
--  Say 'Argument 2 must be a whole number between 1 and 999999'
    Raise Syntax 88.907 array(2,1,999999,xprec)
    End

  return .my.rxm~tanh(x,xprec)

::routine rxmhelp public
  Use Arg xprec=(.my.rxm~precision),xtype=(.my.rxm~type)
                   Say 'precision='xprec
                   Say '     type='xtype
  Parse source  s; Say '   source='s
  Parse version v; Say '  version='v
  Do si=2 To 5
    Say substr(sourceline(si),3)
    End
  Say 'You can change the default precision and type as follows:'
  Say "  .locaL~my.rxm~precision=50"
  Say "  .locaL~my.rxm~type='R'"
  return 0
```



## Oz


```oz
declare
  PI = 3.14159265

  fun {FromDegrees Deg}
     Deg * PI / 180.
  end

  fun {ToDegrees Rad}
     Rad * 180. / PI
  end

  Radians = PI / 4.
  Degrees = 45.
in
  for F in [Sin Cos Tan] do
     {System.showInfo {F Radians}#"  "#{F {FromDegrees Degrees}}}
  end

  for I#F in [Asin#Sin Acos#Cos Atan#Tan] do
     {System.showInfo {I {F Radians}}#"  "#{ToDegrees {I {F Radians}}}}
  end
```



## PARI/GP

Pari accepts only radians; the conversion is simple but not included here.

```parigp
cos(Pi/2)
sin(Pi/2)
tan(Pi/2)
acos(1)
asin(1)
atan(1)
```


{{works with|PARI/GP|2.4.3 and above}}

```parigp
apply(f->f(1), [cos,sin,tan,acos,asin,atan])
```



## Pascal

{{libheader|math}}

```pascal
Program TrigonometricFuntions(output);

uses
  math;

var
  radians, degree: double;

begin
  radians := pi / 4.0;
  degree := 45;
  //  Pascal works in radians.  Necessary degree-radian conversions are shown.
  writeln (sin(radians),'   ', sin(degree/180*pi));
  writeln (cos(radians),'   ', cos(degree/180*pi));
  writeln (tan(radians),'   ', tan(degree/180*pi));
  writeln ();
  writeln (arcsin(sin(radians)),' Rad., or ', arcsin(sin(degree/180*pi))/pi*180,' Deg.');
  writeln (arccos(cos(radians)),' Rad., or ', arccos(cos(degree/180*pi))/pi*180,' Deg.');
  writeln (arctan(tan(radians)),' Rad., or ', arctan(tan(degree/180*pi))/pi*180,' Deg.');
  //  ( radians ) / pi * 180 = deg.
end.
```

{{out}}

```txt
 7.0710678118654750E-0001    7.0710678118654752E-0001
 7.0710678118654755E-0001    7.0710678118654752E-0001
 9.9999999999999994E-0001    1.0000000000000000E+0000

 7.8539816339744828E-0001  Rad., or  4.5000000000000000E+0001  Deg.
 7.8539816339744828E-0001  Rad., or  4.5000000000000000E+0001  Deg.
 7.8539816339744828E-0001  Rad., or  4.5000000000000000E+0001  Deg.
```



## Perl

{{works with|Perl|5.8.8}}


```perl
use Math::Trig;

my $angle_degrees = 45;
my $angle_radians = pi / 4;

print sin($angle_radians), ' ', sin(deg2rad($angle_degrees)), "\n";
print cos($angle_radians), ' ', cos(deg2rad($angle_degrees)), "\n";
print tan($angle_radians), ' ', tan(deg2rad($angle_degrees)), "\n";
print cot($angle_radians), ' ', cot(deg2rad($angle_degrees)), "\n";
my $asin = asin(sin($angle_radians));
print $asin, ' ', rad2deg($asin), "\n";
my $acos = acos(cos($angle_radians));
print $acos, ' ', rad2deg($acos), "\n";
my $atan = atan(tan($angle_radians));
print $atan, ' ', rad2deg($atan), "\n";
my $acot = acot(cot($angle_radians));
print $acot, ' ', rad2deg($acot), "\n";
```


{{out}}

```txt

0.707106781186547 0.707106781186547
0.707106781186548 0.707106781186548
1 1
1 1
0.785398163397448 45
0.785398163397448 45
0.785398163397448 45
0.785398163397448 45

```



## Perl 6

{{works with|Rakudo|2016.01}}


```perl6
say sin(pi/3);
say cos(pi/4);
say tan(pi/6);

say asin(sqrt(3)/2);
say acos(1/sqrt 2);
say atan(1/sqrt 3);
```



## Phix


```Phix
?sin(PI/2)
?sin(90*PI/180)
?cos(0)
?cos(0*PI/180)
?tan(PI/4)
?tan(45*PI/180)
?arcsin(1)*2
?arcsin(1)*180/PI
?arccos(0)*2
?arccos(0)*180/PI
?arctan(1)*4
?arctan(1)*180/PI
```

{{out}}

```txt

1
1
1
1
1.0
1.0
3.141592654
90
3.141592654
90
3.141592654
45

```



## PHP


```php
$radians = M_PI / 4;
$degrees = 45 * M_PI / 180;
echo sin($radians) . " " . sin($degrees);
echo cos($radians) . " " . cos($degrees);
echo tan($radians) . " " . tan($degrees);
echo asin(sin($radians)) . " " . asin(sin($radians)) * 180 / M_PI;
echo acos(cos($radians)) . " " . acos(cos($radians)) * 180 / M_PI;
echo atan(tan($radians)) . " " . atan(tan($radians)) * 180 / M_PI;
```



## PicoLisp


```PicoLisp
(load "@lib/math.l")

(de dtor (Deg)
   (*/ Deg pi 180.0) )

(de rtod (Rad)
   (*/ Rad 180.0 pi) )

(prinl
   (format (sin (/ pi 4)) *Scl) " " (format (sin (dtor 45.0)) *Scl) )
(prinl
   (format (cos (/ pi 4)) *Scl) " " (format (cos (dtor 45.0)) *Scl) )
(prinl
   (format (tan (/ pi 4)) *Scl) " " (format (tan (dtor 45.0)) *Scl) )
(prinl
   (format (asin (sin (/ pi 4))) *Scl) " " (format (rtod (asin (sin (dtor 45.0)))) *Scl) )
(prinl
   (format (acos (cos (/ pi 4))) *Scl) " " (format (rtod (acos (cos (dtor 45.0)))) *Scl) )
(prinl
   (format (atan (tan (/ pi 4))) *Scl) " " (format (rtod (atan (tan (dtor 45.0)))) *Scl) )
```

{{out}}

```txt
0.707107 0.707107
0.707107 0.707107
1.000000 1.000000
0.785398 44.999986
0.785398 44.999986
0.785398 44.999986
```



## PL/I


```PL/I

declare (x, xd, y, v) float;

x = 0.5; xd = 45;

/* angle in radians: */
v = sin(x); y = asin(v); put skip list (y);
v = cos(x); y = acos(v); put skip list (y);
v = tan(x); y = atan(v); put skip list (y);

/* angle in degrees: */
v = sind(xd); put skip list (v);
v = cosd(xd); put skip list (v);
v = tand(xd); y = atand(v); put skip list (y);

/* hyperbolic functions: */
v = sinh(x); put skip list (v);
v = cosh(x); put skip list (v);
v = tanh(x); y = atanh(v); put skip list (y);

```

Results:

```txt

 5.00000E-0001
 5.00000E-0001
 5.00000E-0001
 7.07107E-0001
 7.07107E-0001
 4.50000E+0001
 5.21095E-0001
 1.12763E+0000
 5.00000E-0001

```



## PL/SQL

The transcendental functions COS, COSH, EXP, LN, LOG, SIN, SINH, SQRT, TAN, and TANH
are accurate to 36 decimal digits. The transcendental functions ACOS, ASIN, ATAN, and
ATAN2 are accurate to 30 decimal digits.


```plsql
DECLARE
  pi NUMBER := 4 * atan(1);
  radians NUMBER := pi / 4;
  degrees NUMBER := 45.0;
BEGIN
  DBMS_OUTPUT.put_line(SIN(radians) || ' ' || SIN(degrees * pi/180) );
  DBMS_OUTPUT.put_line(COS(radians) || ' ' || COS(degrees * pi/180) );
  DBMS_OUTPUT.put_line(TAN(radians) || ' ' || TAN(degrees * pi/180) );
  DBMS_OUTPUT.put_line(ASIN(SIN(radians)) || ' ' || ASIN(SIN(degrees * pi/180)) * 180/pi);
  DBMS_OUTPUT.put_line(ACOS(COS(radians)) || ' ' || ACOS(COS(degrees * pi/180)) * 180/pi);
  DBMS_OUTPUT.put_line(ATAN(TAN(radians)) || ' ' || ATAN(TAN(degrees * pi/180)) * 180/pi);
end;
```


{{out}}

```txt
,7071067811865475244008443621048490392889 ,7071067811865475244008443621048490392893
,7071067811865475244008443621048490392783 ,7071067811865475244008443621048490392779
1,00000000000000000000000000000000000001 1,00000000000000000000000000000000000002
,7853981633974483096156608458198656891236 44,99999999999999999999999999999942521259
,7853981633974483096156608458198857529988 45,00000000000000000000000000000057478811
,7853981633974483096156608458198757210578 45,00000000000000000000000000000000000067
```


The following trigonometric functions are also available

```plsql
ATAN2(n1,n2) --Arctangent(y/x), -pi < result <= +pi
SINH(n) --Hyperbolic sine
COSH(n) --Hyperbolic cosine
TANH(n) --Hyperbolic tangent
```



## Pop11


Pop11 trigonometric functions accept both degrees and radians. In default mode argument is in degrees, after setting 'popradians' flag to 'true' arguments are in radians.


```pop11
sin(30) =>
cos(45) =>
tan(45) =>
arcsin(0.7) =>
arccos(0.7) =>
arctan(0.7) =>
;;; switch to radians
true -> popradians;

sin(pi*30/180) =>
cos(pi*45/180) =>
tan(pi*45/180) =>
arcsin(0.7) =>
arccos(0.7) =>
arctan(0.7) =>
```



## PostScript


```postscript

90 sin =

60 cos =

%tan of 45 degrees

45 sin 45 cos div =

%inverse tan ( arc tan of sqrt 3)

3 sqrt 1 atan =

```

{{out}}

```txt

1.0

0.5

1.0

60.0

```



## PowerShell

{{Trans|C}}

```powershell
$rad = [Math]::PI / 4
$deg = 45
'{0,10} {1,10}' -f 'Radians','Degrees'
'{0,10:N6} {1,10:N6}' -f [Math]::Sin($rad), [Math]::Sin($deg * [Math]::PI / 180)
'{0,10:N6} {1,10:N6}' -f [Math]::Cos($rad), [Math]::Cos($deg * [Math]::PI / 180)
'{0,10:N6} {1,10:N6}' -f [Math]::Tan($rad), [Math]::Tan($deg * [Math]::PI / 180)
$temp = [Math]::Asin([Math]::Sin($rad))
'{0,10:N6} {1,10:N6}' -f $temp, ($temp * 180 / [Math]::PI)
$temp = [Math]::Acos([Math]::Cos($rad))
'{0,10:N6} {1,10:N6}' -f $temp, ($temp * 180 / [Math]::PI)
$temp = [Math]::Atan([Math]::Tan($rad))
'{0,10:N6} {1,10:N6}' -f $temp, ($temp * 180 / [Math]::PI)
```

{{out}}

```txt
   Radians    Degrees
  0,707107   0,707107
  0,707107   0,707107
  1,000000   1,000000
  0,785398  45,000000
  0,785398  45,000000
  0,785398  45,000000
```


===A More "PowerShelly" Way===
I would send the output as an array of objects containing the (<code>[double]</code>) properties: '''Radians''' and '''Degrees'''.
Notice the difference between the last decimal place in the first two objects.  If you were calculating coordinates as a civil engineer or land surveyor this difference could affect your measurments.  Additionally, the output is an array of objects containing <code>[double]</code> values rather than an array of strings.

```PowerShell

$radians = [Math]::PI / 4
$degrees = 45

[PSCustomObject]@{Radians=[Math]::Sin($radians); Degrees=[Math]::Sin($degrees * [Math]::PI / 180)}
[PSCustomObject]@{Radians=[Math]::Cos($radians); Degrees=[Math]::Cos($degrees * [Math]::PI / 180)}
[PSCustomObject]@{Radians=[Math]::Tan($radians); Degrees=[Math]::Tan($degrees * [Math]::PI / 180)}

[double]$tempVar = [Math]::Asin([Math]::Sin($radians))
[PSCustomObject]@{Radians=$tempVar; Degrees=$tempVar * 180 / [Math]::PI}

[double]$tempVar = [Math]::Acos([Math]::Cos($radians))
[PSCustomObject]@{Radians=$tempVar; Degrees=$tempVar * 180 / [Math]::PI}

[double]$tempVar = [Math]::Atan([Math]::Tan($radians))
[PSCustomObject]@{Radians=$tempVar; Degrees=$tempVar * 180 / [Math]::PI}

```

{{Out}}

```txt

          Radians           Degrees
          -------           -------
0.707106781186547 0.707106781186547
0.707106781186548 0.707106781186548
                1                 1
0.785398163397448                45
0.785398163397448                45
0.785398163397448                45

```



## PureBasic



```purebasic
OpenConsole()

Macro DegToRad(deg)
  deg*#PI/180
EndMacro
Macro RadToDeg(rad)
  rad*180/#PI
EndMacro

degree    = 45
radians.f = #PI/4

PrintN(StrF(Sin(DegToRad(degree)))+" "+StrF(Sin(radians)))
PrintN(StrF(Cos(DegToRad(degree)))+" "+StrF(Cos(radians)))
PrintN(StrF(Tan(DegToRad(degree)))+" "+StrF(Tan(radians)))

arcsin.f = ASin(Sin(radians))
PrintN(StrF(arcsin)+" "+Str(RadToDeg(arcsin)))
arccos.f = ACos(Cos(radians))
PrintN(StrF(arccos)+" "+Str(RadToDeg(arccos)))
arctan.f = ATan(Tan(radians))
PrintN(StrF(arctan)+" "+Str(RadToDeg(arctan)))

Input()
```


{{out}}

```txt
0.707107 0.707107
0.707107 0.707107
1.000000 1.000000
0.785398 45
0.785398 45
0.785398 45
```



## Python


Python's <tt>math</tt> module contains all six functions.

The functions all accept radians only, so conversion is necessary
when dealing with degrees.

The <tt>math</tt> module also has <tt>degrees()</tt> and <tt>radians()</tt> functions for easy conversion.


```python
Python 3.2.2 (default, Sep  4 2011, 09:51:08) [MSC v.1500 32 bit (Intel)] on win32
Type "copyright", "credits" or "license()" for more information.
>>> from math import degrees, radians, sin, cos, tan, asin, acos, atan, pi
>>> rad, deg = pi/4, 45.0
>>> print("Sine:", sin(rad), sin(radians(deg)))
Sine: 0.7071067811865475 0.7071067811865475
>>> print("Cosine:", cos(rad), cos(radians(deg)))
Cosine: 0.7071067811865476 0.7071067811865476
>>> print("Tangent:", tan(rad), tan(radians(deg)))
Tangent: 0.9999999999999999 0.9999999999999999
>>> arcsine = asin(sin(rad))
>>> print("Arcsine:", arcsine, degrees(arcsine))
Arcsine: 0.7853981633974482 44.99999999999999
>>> arccosine = acos(cos(rad))
>>> print("Arccosine:", arccosine, degrees(arccosine))
Arccosine: 0.7853981633974483 45.0
>>> arctangent = atan(tan(rad))
>>> print("Arctangent:", arctangent, degrees(arctangent))
Arctangent: 0.7853981633974483 45.0
>>>
```



## R


```R
deg <- function(radians) 180*radians/pi
rad <- function(degrees) degrees*pi/180
sind <- function(ang) sin(rad(ang))
cosd <- function(ang) cos(rad(ang))
tand <- function(ang) tan(rad(ang))
asind <- function(v) deg(asin(v))
acosd <- function(v) deg(acos(v))
atand <- function(v) deg(atan(v))

r <- pi/3
rd <- deg(r)

print( c( sin(r), sind(rd)) )
print( c( cos(r), cosd(rd)) )
print( c( tan(r), tand(rd)) )

S <- sin(pi/4)
C <- cos(pi/3)
T <- tan(pi/4)

print( c( asin(S), asind(S) ) )
print( c( acos(C), acosd(C) ) )
print( c( atan(T), atand(T) ) )
```



## Racket


```Racket
#lang racket
(define radians (/ pi 4))
(define degrees 45)

(displayln (format "~a ~a" (sin radians) (sin (* degrees (/ pi 180)))))

(displayln (format "~a ~a" (cos radians) (cos (* degrees (/ pi 180)))))

(displayln (format "~a ~a" (tan radians) (tan (* degrees (/ pi 180)))))

(define arcsin (asin (sin radians)))
(displayln (format "~a ~a" arcsin (* arcsin (/ 180 pi))))

(define arccos (acos (cos radians)))
(displayln (format "~a ~a" arccos (* arccos (/ 180 pi))))

(define arctan (atan (tan radians)))
(display (format "~a ~a" arctan (* arctan (/ 180 pi))))
```



## RapidQ


```RapidQ
$APPTYPE CONSOLE
$TYPECHECK ON

SUB pause(prompt$)
    PRINT prompt$
    DO
        SLEEP .1
    LOOP UNTIL LEN(INKEY$) > 0
END SUB

'MAIN
DEFDBL pi , radians , degrees , deg2rad
pi = 4 * ATAN(1)
deg2rad = pi / 180
radians = pi / 4
degrees = 45 * deg2rad

PRINT format$("%.6n" , SIN(radians)) + "  " + format$("%.6n" , SIN(degrees))
PRINT format$("%.6n" , COS(radians)) + "  " + format$("%.6n" , COS(degrees))
PRINT format$("%.6n" , TAN(radians)) + "  " + format$("%.6n" , TAN(degrees))

DEFDBL temp = SIN(radians)
PRINT format$("%.6n" , ASIN(temp)) + "  " + format$("%.6n" , ASIN(temp) / deg2rad)

temp = COS(radians)
PRINT format$("%.6n" , ACOS(temp)) + "  " + format$("%.6n" , ACOS(temp) / deg2rad)

temp = TAN(radians)
PRINT format$("%.6n" , ATAN(temp)) + "  " + format$("%.6n" , ATAN(temp) / deg2rad)

pause("Press any key to continue.")

END 'MAIN
```



## REBOL


```REBOL
REBOL [
	Title: "Trigonometric Functions"
	URL: http://rosettacode.org/wiki/Trigonometric_Functions
]

radians: pi / 4  degrees: 45.0

; Unlike most languages, REBOL's trig functions work in degrees unless
; you specify differently.

print [sine/radians radians     sine degrees]
print [cosine/radians radians   cosine degrees]
print [tangent/radians radians  tangent degrees]

d2r: func [
	"Convert degrees to radians."
	d [number!] "Degrees"
][d * pi / 180]

arcsin: arcsine sine degrees
print [d2r arcsin  arcsin]

arccos: arccosine cosine degrees
print [d2r arccos  arccos]

arctan: arctangent tangent degrees
print [d2r arctan  arctan]
```


{{out}}

```txt
0.707106781186547 0.707106781186547
0.707106781186548 0.707106781186548
1.0 1.0
0.785398163397448 45.0
0.785398163397448 45.0
0.785398163397448 45.0
```



## REXX

The REXX language doesn't have any trig functions (or for that matter,
a square root [SQRT] function), so if higher math

functions are wanted, you have to roll your own.
Some of the normal/regular trigonometric functions are included here.
     ┌──────────────────────────────────────────────────────────────────────────┐
     │  One common method that ensures enough accuracy in REXX is specifying    │
     │  more precision  (via  NUMERIC DIGITS  nnn)  than is needed,  and then   │
     │  displaying the number of digits that are desired,  or  the number(s)    │
     │  could be re-normalized using the  FORMAT  BIF.                          │
     │                                                                          │
     │  The technique used (below) is to set the   numeric digits   ten higher  │
     │  than the desired digits,  as specified by the   SHOWDIGS  variable.     │
     └──────────────────────────────────────────────────────────────────────────┘
Most math (POW, EXP, LOG, LN, GAMMA, etc.), trigonometric, and hyperbolic functions need only five extra digits, but ten

extra digits is safer in case the argument is close to an asymptotic point or a multiple or fractional part of pi or somesuch.

It should also be noted that both the '''pi''' and '''e''' constants have only around 77 decimal digits as included here, if more

precision is needed, those constants should be extended.   Both '''pi''' and '''e''' could've been shown with more precision,

but having large precision numbers would add to this REXX program's length.   If anybody wishes to see this REXX version of

extended digits for '''pi''' or '''e''', I could extend them to any almost any precision (as a REXX constant).   Normally, a REXX

(external) subroutine is used for such purposes so as to not make the program using the constant unwieldy large.

```rexx
/*REXX program demonstrates some  common trig  functions  (30 decimal digits are shown).*/
showdigs= 25                                           /*show only 25 digits of number. */
numeric digits showdigs + 10                           /*DIGITS default is  9,  but use */
                                                       /*extra digs to prevent rounding.*/
say 'Using'    showdigs    'decimal digits precision.' /*show # decimal digs being used.*/
say
     do j=-180  to +180  by 15                         /*let's just do a  half─Monty.   */
     stuff = right(j, 4)         'degrees, rads='   show(  d2r(j) ) ,
                                        '   sin='   show( sinD(j) ) ,
                                        '   cos='   show( cosD(J) )
                                                       /*don't let  TANGENT  go postal. */
     if abs(j)\==90  then stuff=stuff   '   tan='   show( tanD(j) )
     say stuff
     end   /*j*/
say
     do k=-1  to +1  by 1/2                            /*keep the  Arc─functions happy. */
     say right(k, 4)             'radians, degs='   show(  r2d(k) )        ,
                                        '  Acos='   show( Acos(k) ) ,
                                        '  Asin='   show( Asin(k) ) ,
                                        '  Atan='   show( Atan(k) )
     end   /*k*/
exit                                                   /*stick a fork in it, we're done.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
Asin:   procedure;  parse arg x 1 z 1 o 1 p;      a=abs(x);       aa=a*a
          if a>1  then call AsinErr x                  /*X argument is out of range.    */
          if a >= sqrt(2) * .5  then  return sign(x) * acos( sqrt(1 - aa),  '-ASIN')
          do j=2  by 2  until p=z;  p=z;   o= o * aa * (j-1) / j;   z= z +o / (j+1);   end
          return  z                                    /* [↑]  compute until no noise.  */
/*──────────────────────────────────────────────────────────────────────────────────────*/
Acos:  procedure; parse arg x; if x<-1 | x>1  then call AcosErr;  return pi()*.5 - Asin(x)
AcosD: return r2d( Acos( arg(1)      ) )
AsinD: return r2d( Asin( arg(1)      ) )
cosD:  return cos( d2r(  arg(1)      ) )
sinD:  return sin( d2r(  d2d( arg(1) ) ) )
tan:   procedure; parse arg x;  _= cos(x);    if _=0  then call tanErr;  return sin(x) / _
tanD:  return tan( d2r( arg(1) ) )
d2d:   return arg(1)                  //  360      /*normalize degrees ──► a unit circle*/
d2r:   return r2r( d2d( arg(1) )*pi()  /  180)     /*convert   degrees ──► radians.     */
r2d:   return d2d( ( arg(1) * 180      /  pi() ) ) /*convert   radians ──► degrees.     */
r2r:   return arg(1)                  // (pi() *2) /*normalize radians ──► a unit circle*/
show:  return left( left('', arg(1) >= 0)format( arg(1), , showdigs) / 1, showdigs)
tellErr: say; say '*** error! ***';   say;   say arg(1);   say;           exit 13
tanErr:  call tellErr 'tan(' || x") causes division by zero, X=" || x
AsinErr: call tellErr 'Asin(x),  X  must be in the range of  -1 ──► +1,  X=' || x
AcosErr: call tellErr 'Acos(x),  X  must be in the range of  -1 ──► +1,  X=' || x
/*──────────────────────────────────────────────────────────────────────────────────────*/
Atan: procedure; parse arg x;    if abs(x)=1  then return pi() * .25 * sign(x)
                                                   return Asin(x / sqrt(1 + x*x) )
/*──────────────────────────────────────────────────────────────────────────────────────*/
cos:  procedure; parse arg x;       x= r2r(x);     a= abs(x);                 hpi= pi * .5
          numeric fuzz min(6, digits() - 3);       if a=pi      then return -1
          if a=hpi | a=hpi*3  then return 0;       if a=pi / 3  then return .5
          if a=pi * 2 / 3     then return -.5;                  return .sinCos(1, -1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
sin:    procedure; parse arg x;   x=r2r(x);        numeric fuzz min(5, max(1, digits()-3))
        if x=pi*.5  then return 1;                 if x==pi * 1.5  then return -1
        if abs(x)=pi | x=0   then return 0;                            return .sinCos(x,1)
/*──────────────────────────────────────────────────────────────────────────────────────*/
.sinCos:  parse arg z 1 _,i;        q= x*x
          do k=2  by 2  until p=z;  p= z;     _= - _ * q / (k * (k+i) );   z= z + _;   end
          return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x;   if x=0  then return 0;  d=digits();  i=;    m.=9;   h= d+6
      numeric digits; numeric form;     if x<0  then  do;   x= -x;   i= 'i';   end
      parse value format(x, 2, 1, , 0)  'E0'   with   g  'E'  _ .;       g= g *.5'e'_ % 2
         do j=0  while h>9;        m.j=h;                   h= h % 2  +  1;    end  /*j*/
         do k=j+5  to 0  by -1;    numeric digits m.k;      g= (g+x/g) * .5;   end  /*k*/
      numeric digits d;            return (g/1)i               /*make complex if  X < 0.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
e:   e = 2.7182818284590452353602874713526624977572470936999595749669676277240766303535
     return e              /*Note:  the actual E subroutine returns  E's  accuracy that */
                           /*matches the current NUMERIC DIGITS, up to 1 million digits.*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
exp: procedure; parse arg x;  ix=x%1;    if abs(x-ix)>.5  then ix= ix + sign(x);  x=x - ix
     z=1; _=1; w=z;  do j=1;  _= _*x/j;  z= (z+_) / 1;    if z==w  then leave;  w=z;  end
     if z\==0  then z= e()**ix * z;                       return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
pi:  pi= 3.1415926535897932384626433832795028841971693993751058209749445923078164062862
     return pi             /*Note:  the actual PI subroutine returns PI's accuracy that */
                           /*matches the current NUMERIC DIGITS, up to 1 million digits.*/
                           /*John Machin's formula is used for calculating more digits. */
```

Programming note:
  ╔═════════════════════════════════════════════════════════════════════════════╗
  ║ Functions that are not included here are   (among others):                  ║
  ║                                                                             ║
  ║ some of the usual higher-math functions normally associated with trig       ║
  ║ functions:             POW, GAMMA, LGGAMMA,  ERF, ERFC,  ROOT,  ATAN2,      ║
  ║                        LOG (LN),   LOG2,   LOG10,      and all of the       ║
  ║ hyperbolic trigonometric functions  and  their  inverses  (too many to list ║
  ║                        here),                                               ║
  ║ angle conversions/normalizations:  degrees/radians/grads/mils:              ║
  ║     a circle  ≡  2 pi radians  ≡  360 degrees  ≡  400 grads  ≡  6400 mils.  ║
  ║                                                                             ║
  ║ Some of the other trigonometric functions are (hyphens added intentionally):║
  ║                                                                             ║
  ║     CHORD                                                                   ║
  ║     COT    (co-tangent)                                                     ║
  ║     CSC    (co-secant)                                                      ║
  ║     CVC    (co-versed cosine)                                               ║
  ║     CVS    (co-versed sine)                                                 ║
  ║     CXS    (co-exsecant)                                                    ║
  ║     HAC    (haver-cosine)                                                   ║
  ║     HAV    (haver-sine                                                      ║
  ║     SEC    (secant)                                                         ║
  ║     VCS    (versed cosine or ver-cosine)                                    ║
  ║     VSN    (versed sine   or ver-sine)                                      ║
  ║     XCS    (ex-secant)                                                      ║
  ║     COS/SIN/TAN cardinal  (damped  COS/SIN/TAN  functions)                  ║
  ║     COS/SIN     integral                                                    ║
  ║                                                                             ║
  ║       and all pertinent inverses of the above functions  (AVSN, ACVS, ···). ║
  ╚═════════════════════════════════════════════════════════════════════════════╝
{{out|output}}

(Shown at three-quarter size.)
<pre style="font-size:75%>
Using 25 decimal digits precision.

-180 degrees, rads= -3.1415926535897932384626    sin=  0                           cos= -1                           tan=  0
-165 degrees, rads= -2.8797932657906438019240    sin= -0.2588190451025207623488    cos= -0.9659258262890682867497    tan=  0.2679491924311227064725
-150 degrees, rads= -2.6179938779914943653855    sin= -0.5                         cos= -0.8660254037844386467637    tan=  0.5773502691896257645091
-135 degrees, rads= -2.3561944901923449288469    sin= -0.7071067811865475244008    cos= -0.7071067811865475244008    tan=  1
-120 degrees, rads= -2.0943951023931954923084    sin= -0.8660254037844386467637    cos= -0.5                         tan=  1.7320508075688772935274
-105 degrees, rads= -1.8325957145940460557698    sin= -0.9659258262890682867497    cos= -0.2588190451025207623488    tan=  3.7320508075688772935274
 -90 degrees, rads= -1.5707963267948966192313    sin= -1                           cos=  0
 -75 degrees, rads= -1.3089969389957471826927    sin= -0.9659258262890682867497    cos=  0.2588190451025207623488    tan= -3.7320508075688772935274
 -60 degrees, rads= -1.0471975511965977461542    sin= -0.8660254037844386467637    cos=  0.5                         tan= -1.7320508075688772935274
 -45 degrees, rads= -0.7853981633974483096156    sin= -0.7071067811865475244008    cos=  0.7071067811865475244008    tan= -1
 -30 degrees, rads= -0.5235987755982988730771    sin= -0.5                         cos=  0.8660254037844386467637    tan= -0.5773502691896257645091
 -15 degrees, rads= -0.2617993877991494365385    sin= -0.2588190451025207623488    cos=  0.9659258262890682867497    tan= -0.2679491924311227064725
   0 degrees, rads=  0                           sin=  0                           cos=  1                           tan=  0
  15 degrees, rads=  0.2617993877991494365385    sin=  0.2588190451025207623488    cos=  0.9659258262890682867497    tan=  0.2679491924311227064725
  30 degrees, rads=  0.5235987755982988730771    sin=  0.5                         cos=  0.8660254037844386467637    tan=  0.5773502691896257645091
  45 degrees, rads=  0.7853981633974483096156    sin=  0.7071067811865475244008    cos=  0.7071067811865475244008    tan=  1
  60 degrees, rads=  1.0471975511965977461542    sin=  0.8660254037844386467637    cos=  0.5                         tan=  1.7320508075688772935274
  75 degrees, rads=  1.3089969389957471826927    sin=  0.9659258262890682867497    cos=  0.2588190451025207623488    tan=  3.7320508075688772935274
  90 degrees, rads=  1.5707963267948966192313    sin=  1                           cos=  0
 105 degrees, rads=  1.8325957145940460557698    sin=  0.9659258262890682867497    cos= -0.2588190451025207623488    tan= -3.7320508075688772935274
 120 degrees, rads=  2.0943951023931954923084    sin=  0.8660254037844386467637    cos= -0.5                         tan= -1.7320508075688772935274
 135 degrees, rads=  2.3561944901923449288469    sin=  0.7071067811865475244008    cos= -0.7071067811865475244008    tan= -1
 150 degrees, rads=  2.6179938779914943653855    sin=  0.5                         cos= -0.8660254037844386467637    tan= -0.5773502691896257645091
 165 degrees, rads=  2.8797932657906438019240    sin=  0.2588190451025207623488    cos= -0.9659258262890682867497    tan= -0.2679491924311227064725
 180 degrees, rads=  3.1415926535897932384626    sin=  0                           cos= -1                           tan=  0

  -1 radians, degs= -57.295779513082320876798   Acos=  3.1415926535897932384626   Asin= -1.5707963267948966192313   Atan= -0.7853981633974483096156
-0.5 radians, degs= -28.647889756541160438399   Acos=  2.0943951023931954923084   Asin= -0.5235987755982988730771   Atan= -0.4636476090008061162142
   0 radians, degs=  0                          Acos=  1.5707963267948966192313   Asin=  0                          Atan=  0
 0.5 radians, degs=  28.647889756541160438399   Acos=  1.0471975511965977461542   Asin=  0.5235987755982988730771   Atan=  0.4636476090008061162142
 1.0 radians, degs=  57.295779513082320876798   Acos=  0                          Asin=  1.5707963267948966192313   Atan=  0.7853981633974483096156

```



## Ring


```ring

pi = 3.14
decimals(8)
see "sin(pi/4.0) = " + sin(pi/4.0) + nl
see "cos(pi/4.0) = " + cos(pi/4.0) + nl
see "tan(pi/4.0) = " + tan(pi/4.0)+   nl
see "asin(sin(pi/4.0)) = " + asin(sin(pi/4.0)) + nl
see "acos(cos(pi/4.0)) = " + acos(cos(pi/4.0)) + nl
see "atan(tan(pi/4.0)) = " + atan(tan(pi/4.0)) + nl
see "atan2(3,4) = " + atan2(3,4) + nl

```



## Ruby


Ruby's <tt>Math</tt> module contains all six functions. The functions all accept radians only, so conversion is necessary when dealing with degrees.


```ruby
radians = Math::PI / 4
degrees = 45.0

def deg2rad(d)
  d * Math::PI / 180
end

def rad2deg(r)
  r * 180 / Math::PI
end

#sine
puts "#{Math.sin(radians)} #{Math.sin(deg2rad(degrees))}"
#cosine
puts "#{Math.cos(radians)} #{Math.cos(deg2rad(degrees))}"
#tangent
puts "#{Math.tan(radians)} #{Math.tan(deg2rad(degrees))}"
#arcsine
arcsin = Math.asin(Math.sin(radians))
puts "#{arcsin} #{rad2deg(arcsin)}"
#arccosine
arccos = Math.acos(Math.cos(radians))
puts "#{arccos} #{rad2deg(arccos)}"
#arctangent
arctan = Math.atan(Math.tan(radians))
puts "#{arctan} #{rad2deg(arctan)}"
```


{{out}}

```txt

0.7071067811865475 0.7071067811865475
0.7071067811865476 0.7071067811865476
0.9999999999999999 0.9999999999999999
0.7853981633974482 44.99999999999999
0.7853981633974483 45.0
0.7853981633974483 45.0

```



###  BigDecimal

If you want more digits in the answer, then you can use the <tt>BigDecimal</tt> class. <tt>BigMath</tt> only has big versions of sine, cosine, and arctangent; so we must implement tangent, arcsine and arccosine.

{{trans|bc}}
{{works with|Ruby|1.9}}

```ruby
require 'bigdecimal'       # BigDecimal
require 'bigdecimal/math'  # BigMath

include BigMath  # Allow sin(x, prec) instead of BigMath.sin(x, prec).

# Tangent of _x_.
def tan(x, prec)
  sin(x, prec) / cos(x, prec)
end

# Arcsine of _y_, domain [-1, 1], range [-pi/2, pi/2].
def asin(y, prec)
  # Handle angles with no tangent.
  return -PI / 2 if y == -1
  return PI / 2 if y == 1

  # Tangent of angle is y / x, where x^2 + y^2 = 1.
  atan(y / sqrt(1 - y * y, prec), prec)
end

# Arccosine of _x_, domain [-1, 1], range [0, pi].
def acos(x, prec)
  # Handle angle with no tangent.
  return PI / 2 if x == 0

  # Tangent of angle is y / x, where x^2 + y^2 = 1.
  a = atan(sqrt(1 - x * x, prec) / x, prec)
  if a < 0
    a + PI(prec)
  else
    a
  end
end


prec = 52
pi = PI(prec)
degrees = pi / 180  # one degree in radians

b1 = BigDecimal.new "1"
b2 = BigDecimal.new "2"
b3 = BigDecimal.new "3"

f = proc { |big| big.round(50).to_s('F') }
print("Using radians:",
      "\n  sin(-pi / 6) = ", f[ sin(-pi / 6, prec) ],
      "\n  cos(3 * pi / 4) = ", f[ cos(3 * pi / 4, prec) ],
      "\n  tan(pi / 3) = ", f[ tan(pi / 3, prec) ],
      "\n  asin(-1 / 2) = ", f[ asin(-b1 / 2, prec) ],
      "\n  acos(-sqrt(2) / 2) = ", f[ acos(-sqrt(b2, prec) / 2, prec) ],
      "\n  atan(sqrt(3)) = ", f[ atan(sqrt(b3, prec), prec) ],
      "\n")
print("Using degrees:",
      "\n  sin(-30) = ", f[ sin(-30 * degrees, prec) ],
      "\n  cos(135) = ", f[ cos(135 * degrees, prec) ],
      "\n  tan(60) = ", f[ tan(60 * degrees, prec) ],
      "\n  asin(-1 / 2) = ",
      f[ asin(-b1 / 2, prec) / degrees ],
      "\n  acos(-sqrt(2) / 2) = ",
      f[ acos(-sqrt(b2, prec) / 2, prec) / degrees ],
      "\n  atan(sqrt(3)) = ",
      f[ atan(sqrt(b3, prec), prec) / degrees ],
      "\n")
```


{{out}}

```txt

Using radians:
  sin(-pi / 6) = -0.5
  cos(3 * pi / 4) = -0.70710678118654752440084436210484903928483593768847
  tan(pi / 3) = 1.73205080756887729352744634150587236694280525381038
  asin(-1 / 2) = -0.52359877559829887307710723054658381403286156656252
  acos(-sqrt(2) / 2) = 2.35619449019234492884698253745962716314787704953133
  atan(sqrt(3)) = 1.04719755119659774615421446109316762806572313312504
Using degrees:
  sin(-30) = -0.5
  cos(135) = -0.70710678118654752440084436210484903928483593768847
  tan(60) = 1.73205080756887729352744634150587236694280525381038
  asin(-1 / 2) = -30.0
  acos(-sqrt(2) / 2) = 135.0
  atan(sqrt(3)) = 60.0

```



## Run BASIC


```runbasic
' Find these three ratios:  Sine, Cosine, Tangent.  (These ratios have NO units.)

deg = 45.0
' Run BASIC works in radians; so, first convert deg to rad as shown in next line.
rad = deg * (atn(1)/45)
print "Ratios for a "; deg; " degree angle, (or "; rad; " radian angle.)"
print "Sine:        "; SIN(rad)
print "Cosine:      "; COS(rad)
print "Tangent:     "; TAN(rad)

print "Inverse Functions - - (Using above ratios)"
' Now, use those ratios to work backwards to show their original angle in radians.
' Also, use this:  rad / (atn(1)/45) = deg   (To change radians to degrees.)
print "Arcsine:     "; ASN(SIN(rad)); " radians, (or "; ASN(SIN(rad))/(atn(1)/45); " degrees)"
print "Arccosine:   "; ACS(COS(rad)); " radians, (or "; ACS(COS(rad))/(atn(1)/45); " degrees)"
print "Arctangent:  "; ATN(TAN(rad)); " radians, (or "; ATN(TAN(rad))/(atn(1)/45); " degrees)"

' This code also works in Liberty BASIC.
' The above (atn(1)/45) = approx .01745329252
```

{{out}}

```txt
Ratios for a 45.0 degree angle, (or 0.785398163 radian angle.)
Sine:        0.707106781
Cosine:      0.707106781
Tangent:     1.0
Inverse Functions - - (Using above ratios)
Arcsine:     0.785398163 radians, (or 45.0 degrees)
Arccosine:   0.785398163 radians, (or 45.0 degrees)
Arctangent:  0.785398163 radians, (or 45.0 degrees)
```



## SAS


```sas
data _null_;
pi = 4*atan(1);
deg = 30;
rad = pi/6;
k = pi/180;
x = 0.2;

a = sin(rad);
b = sin(deg*k);
put a b;

a = cos(rad);
b = cos(deg*k);
put a b;

a = tan(rad);
b = tan(deg*k);
put a b;

a=arsin(x);
b=arsin(x)/k;
put a b;

a=arcos(x);
b=arcos(x)/k;
put a b;

a=atan(x);
b=atan(x)/k;
put a b;
run;
```



## Scala

{{libheader|Scala}}
```Scala
import scala.math._

object Gonio extends App {
  //Pi / 4 rad is 45 degrees. All answers should be the same.
  val radians = Pi / 4
  val degrees = 45.0

  println(s"${sin(radians)} ${sin(toRadians(degrees))}")
  //cosine
  println(s"${cos(radians)} ${cos(toRadians(degrees))}")
  //tangent
  println(s"${tan(radians)} ${tan(toRadians(degrees))}")
  //arcsine
  val bgsin = asin(sin(radians))
  println(s"$bgsin ${toDegrees(bgsin)}")
  val bgcos = acos(cos(radians))
  println(s"$bgcos ${toDegrees(bgcos)}")
  //arctangent
  val bgtan = atan(tan(radians))
  println(s"$bgtan ${toDegrees(bgtan)}")
  val bgtan2 = atan2(1, 1)
  println(s"$bgtan ${toDegrees(bgtan)}")
}
```



## Scheme


```scheme
(define pi (* 4 (atan 1)))

(define radians (/ pi 4))
(define degrees 45)

(display (sin radians))
(display " ")
(display (sin (* degrees (/ pi 180))))
(newline)

(display (cos radians))
(display " ")
(display (cos (* degrees (/ pi 180))))
(newline)

(display (tan radians))
(display " ")
(display (tan (* degrees (/ pi 180))))
(newline)

(define arcsin (asin (sin radians)))
(display arcsin)
(display " ")
(display (* arcsin (/ 180 pi)))
(newline)

(define arccos (acos (cos radians)))
(display arccos)
(display " ")
(display (* arccos (/ 180 pi)))
(newline)

(define arctan (atan (tan radians)))
(display arctan)
(display " ")
(display (* arctan (/ 180 pi)))
(newline)
```



## Seed7

The example below uses the libaray [http://seed7.sourceforge.net/libraries/math.htm math.s7i],
which defines, besides many other functions,
[http://seed7.sourceforge.net/libraries/math.htm#sin%28ref_float%29 sin],
[http://seed7.sourceforge.net/libraries/math.htm#cos%28ref_float%29 cos],
[http://seed7.sourceforge.net/libraries/math.htm#tan%28ref_float%29 tan],
[http://seed7.sourceforge.net/libraries/math.htm#asin%28ref_float%29 asin],
[http://seed7.sourceforge.net/libraries/math.htm#acos%28ref_float%29 acos] and
[http://seed7.sourceforge.net/libraries/math.htm#atan%28ref_float%29 atan].


```seed7
$ include "seed7_05.s7i";
  include "float.s7i";
  include "math.s7i";

const proc: main is func
  local
    const float: radians is PI / 4.0;
    const float: degrees is 45.0;
  begin
    writeln("            radians  degrees");
    writeln("sine:       " <& sin(radians) digits 5 <& sin(degrees * PI / 180.0) digits 5 lpad 9);
    writeln("cosine:     " <& cos(radians) digits 5 <& cos(degrees * PI / 180.0) digits 5 lpad 9);
    writeln("tangent:    " <& tan(radians) digits 5 <& tan(degrees * PI / 180.0) digits 5 lpad 9);
    writeln("arcsine:    " <& asin(0.70710677) digits 5 <& asin(0.70710677) * 180.0 / PI digits 5 lpad 9);
    writeln("arccosine:  " <& acos(0.70710677) digits 5 <& acos(0.70710677) * 180.0 / PI digits 5 lpad 9);
    writeln("arctangent: " <& atan(1.0) digits 5 <& atan(1.0) * 180.0 / PI digits 5 lpad 9);
  end func;
```


{{out}}

```txt

            radians  degrees
sine:       0.70711  0.70711
cosine:     0.70711  0.70711
tangent:    1.00000  1.00000
arcsine:    0.78540 45.00000
arccosine:  0.78540 45.00000
arctangent: 0.78540 45.00000

```



## Sidef


```ruby
var angle_deg = 45;
var angle_rad = Num.pi/4;

for arr in [
    [sin(angle_rad), sin(deg2rad(angle_deg))],
    [cos(angle_rad), cos(deg2rad(angle_deg))],
    [tan(angle_rad), tan(deg2rad(angle_deg))],
    [cot(angle_rad), cot(deg2rad(angle_deg))],
] {
    say arr.join(" ");
}

for n in [
    asin(sin(angle_rad)),
    acos(cos(angle_rad)),
    atan(tan(angle_rad)),
    acot(cot(angle_rad)),
] {
    say [n, rad2deg(n)].join(' ');
}
```

{{out}}

```txt

0.707106781186547 0.707106781186547
0.707106781186548 0.707106781186548
1 1
1 1
0.785398163397448 45
0.785398163397448 45
0.785398163397448 45
0.785398163397448 45

```



## SQL PL

{{works with|Db2 LUW}}
With SQL only:

```sql pl

--Conversion
values degrees(3.1415926);
values radians(180);
-- This is equal to Pi.

--PI/4 45
values sin(radians(180)/4);
values sin(radians(45));
values cos(radians(180)/4);
values cos(radians(45));
values tan(radians(180)/4);
values tan(radians(45));
values cot(radians(180)/4);
values cot(radians(45));
values asin(sin(radians(180)/4));
values asin(sin(radians(45)));
values atan(tan(radians(180)/4));
values atan(tan(radians(45)));

--PI/3 60
values sin(radians(180)/3);
values sin(radians(60));
values cos(radians(180)/3);
values cos(radians(60));
values tan(radians(180)/3);
values tan(radians(60));
values cot(radians(180)/3);
values cot(radians(60));
values asin(sin(radians(180)/3));
values asin(sin(radians(60)));
values atan(tan(radians(180)/3));
values atan(tan(radians(60)));

```

Output:

```txt

db2 -tx
values degrees(3.1415926)
  +1.79999996929531E+002

values radians(180)
  +3.14159265358979E+000

values sin(radians(180)/4)
  +7.07106781186547E-001

values sin(radians(45))
  +7.07106781186547E-001

values cos(radians(180)/4)
  +7.07106781186548E-001

values cos(radians(45))
  +7.07106781186548E-001

values tan(radians(180)/4)
  +1.00000000000000E+000

values tan(radians(45))
  +1.00000000000000E+000

values cot(radians(180)/4)
  +1.00000000000000E+000

values cot(radians(45))
  +1.00000000000000E+000

values asin(sin(radians(180)/4))
  +7.85398163397448E-001

values asin(sin(radians(45)))
  +7.85398163397448E-001

values atan(tan(radians(180)/4))
  +7.85398163397448E-001

values atan(tan(radians(45)))
  +7.85398163397448E-001

values sin(radians(180)/3)
  +8.66025403784439E-001

values sin(radians(60))
  +8.66025403784439E-001

values cos(radians(180)/3)
  +5.00000000000000E-001

values cos(radians(60))
  +5.00000000000000E-001

values tan(radians(180)/3)
  +1.73205080756888E+000

values tan(radians(60))
  +1.73205080756888E+000

values cot(radians(180)/3)
  +5.77350269189626E-001

values cot(radians(60))
  +5.77350269189626E-001

values asin(sin(radians(180)/3))
  +1.04719755119660E+000

values asin(sin(radians(60)))
  +1.04719755119660E+000

values atan(tan(radians(180)/3))
  +1.04719755119660E+000

values atan(tan(radians(60)))
  +1.04719755119660E+000

```



## Stata

Stata computes only in radians, but the conversion is easy.


```stata
scalar deg=_pi/180

display cos(30*deg)
display sin(30*deg)
display tan(30*deg)

display cos(_pi/6)
display sin(_pi/6)
display tan(_pi/6)

display acos(0.5)
display asin(0.5)
display atan(0.5)
```



## Tcl

The built-in functions only take radian arguments.

```tcl
package require Tcl 8.5

proc PI {} {expr {4*atan(1)}}
proc deg2rad d {expr {$d/180*[PI]}}
proc rad2deg r {expr {$r*180/[PI]}}

namespace path ::tcl::mathfunc

proc trig degrees {
    set radians [deg2rad $degrees]
    puts [sin $radians]
    puts [cos $radians]
    puts [tan $radians]
    set arcsin [asin [sin $radians]]; puts "$arcsin [rad2deg $arcsin]"
    set arccos [acos [cos $radians]]; puts "$arccos [rad2deg $arccos]"
    set arctan [atan [tan $radians]]; puts "$arctan [rad2deg $arctan]"
}
trig 60.0
```


```txt
0.8660254037844386
0.5000000000000001
1.7320508075688767
1.0471975511965976 59.99999999999999
1.0471975511965976 59.99999999999999
1.0471975511965976 59.99999999999999
```



## VBA


```vb
Public Sub trig()
    Pi = WorksheetFunction.Pi()
    Debug.Print Sin(Pi / 2)
    Debug.Print Sin(90 * Pi / 180)
    Debug.Print Cos(0)
    Debug.Print Cos(0 * Pi / 180)
    Debug.Print Tan(Pi / 4)
    Debug.Print Tan(45 * Pi / 180)
    Debug.Print WorksheetFunction.Asin(1) * 2
    Debug.Print WorksheetFunction.Asin(1) * 180 / Pi
    Debug.Print WorksheetFunction.Acos(0) * 2
    Debug.Print WorksheetFunction.Acos(0) * 180 / Pi
    Debug.Print Atn(1) * 4
    Debug.Print Atn(1) * 180 / Pi
End Sub
```
{{out}}

```txt
 1
 1
 1
 1
 1
 1
 3,14159265358979
 90
 3,14159265358979
 90
 3,14159265358979
 45

```


## Visual Basic .NET

{{trans|C#}}

```vbnet
Module Module1

    Sub Main()
        Console.WriteLine("
###  radians
")
        Console.WriteLine("  sin (pi/3) = {0}", Math.Sin(Math.PI / 3))
        Console.WriteLine("  cos (pi/3) = {0}", Math.Cos(Math.PI / 3))
        Console.WriteLine("  tan (pi/3) = {0}", Math.Tan(Math.PI / 3))
        Console.WriteLine("arcsin (1/2) = {0}", Math.Asin(0.5))
        Console.WriteLine("arccos (1/2) = {0}", Math.Acos(0.5))
        Console.WriteLine("arctan (1/2) = {0}", Math.Atan(0.5))
        Console.WriteLine()
        Console.WriteLine("
###  degrees
")
        Console.WriteLine("    sin (60) = {0}", Math.Sin(60 * Math.PI / 180))
        Console.WriteLine("    cos (60) = {0}", Math.Cos(60 * Math.PI / 180))
        Console.WriteLine("    tan (60) = {0}", Math.Tan(60 * Math.PI / 180))
        Console.WriteLine("arcsin (1/2) = {0}", Math.Asin(0.5) * 180 / Math.PI)
        Console.WriteLine("arccos (1/2) = {0}", Math.Acos(0.5) * 180 / Math.PI)
        Console.WriteLine("arctan (1/2) = {0}", Math.Atan(0.5) * 180 / Math.PI)
    End Sub

End Module
```

{{out}}

```txt

###  radians

  sin (pi/3) = 0.866025403784439
  cos (pi/3) = 0.5
  tan (pi/3) = 1.73205080756888
arcsin (1/2) = 0.523598775598299
arccos (1/2) = 1.0471975511966
arctan (1/2) = 0.463647609000806


###  degrees

    sin (60) = 0.866025403784439
    cos (60) = 0.5
    tan (60) = 1.73205080756888
arcsin (1/2) = 30
arccos (1/2) = 60
arctan (1/2) = 26.565051177078
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
def     Pi = 3.14159265358979323846;

func real ATan(Y);      \Arc tangent
real Y;
return ATan2(Y, 1.0);

func real Deg(X);       \Convert radians to degrees
real X;
return 57.2957795130823 * X;

func real Rad(X);       \Convert degrees to radians
real X;
return X / 57.2957795130823;

real A, B, C;
[A:= Sin(Pi/6.0);
RlOut(0, A);  ChOut(0, 9\tab\);  RlOut(0, Sin(Rad(30.0)));  CrLf(0);
B:= Cos(Pi/6.0);
RlOut(0, B);  ChOut(0, 9\tab\);  RlOut(0, Cos(Rad(30.0)));  CrLf(0);
C:= Tan(Pi/4.0);
RlOut(0, C);  ChOut(0, 9\tab\);  RlOut(0, Tan(Rad(45.0)));  CrLf(0);

RlOut(0, ASin(A));  ChOut(0, 9\tab\);  RlOut(0, Deg(ASin(A)));  CrLf(0);
RlOut(0, ACos(B));  ChOut(0, 9\tab\);  RlOut(0, Deg(ACos(B)));  CrLf(0);
RlOut(0, ATan(C));  ChOut(0, 9\tab\);  RlOut(0, Deg(ATan(C)));  CrLf(0);
]
```


{{out}}

```txt

    0.50000         0.50000
    0.86603         0.86603
    1.00000         1.00000
    0.52360        30.00000
    0.52360        30.00000
    0.78540        45.00000

```



## zkl


```zkl

(30.0).toRad().sin()  //-->0.5
(60.0).toRad().cos()  //-->0.5
(45.0).toRad().tan()  //-->1
(0.523599).sin()      //-->0.5
etc

(0.5).asin()         //-->0.523599
(0.5).acos()         //-->1.0472
(1.0).atan()         //-->0.785398
(1.0).atan().toDeg() //-->45
etc
```


{{omit from|Batch File|No access to advanced math.}}
{{omit from|M4}}

[[Category:Geometry]]
