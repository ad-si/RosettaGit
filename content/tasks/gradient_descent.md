+++
title = "Gradient descent"
description = ""
date = 2019-09-05T11:36:21Z
aliases = []
[extra]
id = 22397
[taxonomies]
categories = ["task"]
tags = []
+++

Gradient descent (also known as steepest descent) is a first-order iterative optimization algorithm for finding the minimum of a function which is described in [https://en.wikipedia.org/wiki/Gradient_descent this Wikipedia article].

## Task

Use this algorithm to search for minimum values of the bi-variate function:

   f(x, y) = (x - 1)(x - 1)e^(-y^2) + y(y+2)e^(-2x^2)

around x = 0.1 and y = -1.

[https://books.google.co.uk/books?id=dFHvBQAAQBAJ&pg=PA543&lpg=PA543&dq=c%23+steepest+descent+method+to+find+minima+of+two+variable+function&source=bl&ots=TCyD-ts9ui&sig=ACfU3U306Og2fOhTjRv2Ms-BW00IhomoBg&hl=en&sa=X&ved=2ahUKEwitzrmL3aXjAhWwVRUIHSEYCU8Q6AEwCXoECAgQAQ#v=onepage&q=c%23%20steepest%20descent%20method%20to%20find%20minima%20of%20two%20variable%20function&f=false This book excerpt] shows sample C# code for solving this task.




## Go

This is a translation of the C# code in the book excerpt linked to above and hence also of the first Typescript example below.

For some unknown reason the results differ from the other solutions after the first 4 decimal places but are near enough for an approximate method such as this.

```go
package main

import (
    "fmt"
    "math"
)

func steepestDescent(x []float64, alpha, tolerance float64) {
    n := len(x)
    h := tolerance
    g0 := g(x) // Initial estimate of result.

    // Calculate initial gradient.
    fi := gradG(x, h)

    // Calculate initial norm.
    delG := 0.0
    for i := 0; i < n; i++ {
        delG += fi[i] * fi[i]
    }
    delG = math.Sqrt(delG)
    b := alpha / delG

    // Iterate until value is <= tolerance.
    for delG > tolerance {
        // Calculate next value.
        for i := 0; i < n; i++ {
            x[i] -= b * fi[i]
        }
        h /= 2

        // Calculate next gradient.
        fi = gradG(x, h)

        // Calculate next norm.
        delG = 0
        for i := 0; i < n; i++ {
            delG += fi[i] * fi[i]
        }
        delG = math.Sqrt(delG)
        b = alpha / delG

        // Calculate next value.
        g1 := g(x)

        // Adjust parameter.
        if g1 > g0 {
            alpha /= 2
        } else {
            g0 = g1
        }
    }
}

// Provides a rough calculation of gradient g(x).
func gradG(x []float64, h float64) []float64 {
    n := len(x)
    z := make([]float64, n)
    y := make([]float64, n)
    copy(y, x)
    g0 := g(x)

    for i := 0; i < n; i++ {
        y[i] += h
        z[i] = (g(y) - g0) / h
    }
    return z
}

// Function for which minimum is to be found.
func g(x []float64) float64 {
    return (x[0]-1)*(x[0]-1)*
        math.Exp(-x[1]*x[1]) + x[1]*(x[1]+2)*
        math.Exp(-2*x[0]*x[0])
}

func main() {
    tolerance := 0.0000006
    alpha := 0.1
    x := []float64{0.1, -1} // Initial guess of location of minimum.

    steepestDescent(x, alpha, tolerance)
    fmt.Println("Testing steepest descent method:")
    fmt.Println("The minimum is at x[0] =", x[0], "\b, x[1] =", x[1])
}

```


```txt

Testing steepest descent method:
The minimum is at x[0] = 0.10764302056464771, x[1] = -1.223351901171944

```



## Julia


```julia
using Optim, Base.MathConstants

f(x) = (x[1] - 1) * (x[1] - 1) * e^(-x[2]^2) + x[2] * (x[2] + 2) * e^(-2 * x[1]^2)

println(optimize(f, [0.1, -1.0], GradientDescent()))

```
```txt

Results of Optimization Algorithm
 * Algorithm: Gradient Descent
 * Starting Point: [0.1,-1.0]
 * Minimizer: [0.107626844383003,-1.2232596628723371]
 * Minimum: -7.500634e-01
 * Iterations: 14
 * Convergence: true
   * |x - x'| ≤ 0.0e+00: false
     |x - x'| = 2.97e-09
   * |f(x) - f(x')| ≤ 0.0e+00 |f(x)|: true
     |f(x) - f(x')| = 0.00e+00 |f(x)|
   * |g(x)| ≤ 1.0e-08: true
     |g(x)| = 2.54e-09
   * Stopped by an increasing objective: false
   * Reached Maximum Number of Iterations: false
 * Objective Calls: 35
 * Gradient Calls: 35

```



## Phix

... and just like Go, the results don't quite match anything else.

```Phix
-- Function for which minimum is to be found.
function g(sequence x)
    atom {x0,x1} = x
    return (x0-1)*(x0-1)*exp(-x1*x1) + 
               x1*(x1+2)*exp(-2*x0*x0)
end function

-- Provides a rough calculation of gradient g(x).
function gradG(sequence x, atom h)
    integer n = length(x)
    sequence z = repeat(0, n)
    atom g0 := g(x)
    for i=1 to n do
        x[i] += h
        z[i] = (g(x) - g0) / h
    end for
    return z
end function

function steepestDescent(sequence x, atom alpha, tolerance)
    integer n = length(x)
    atom h = tolerance,
         g0 = g(x) -- Initial estimate of result.

    -- Calculate initial gradient.
    sequence fi = gradG(x, h)

    -- Calculate initial norm.
    atom delG = sqrt(sum(sq_mul(fi,fi))),
         b = alpha / delG

    -- Iterate until value is <= tolerance.
    while delG>tolerance do
        -- Calculate next value.
        x = sq_sub(x,sq_mul(b,fi))
        h /= 2
 
        -- Calculate next gradient.
        fi = gradG(x, h)
 
        -- Calculate next norm.
        delG = sqrt(sum(sq_mul(fi,fi)))
        b = alpha / delG
 
        -- Calculate next value.
        atom g1 = g(x)
 
        -- Adjust parameter.
        if g1>g0 then
            alpha /= 2
        else
            g0 = g1
        end if
    end while
    return x
end function
 
constant tolerance = 0.0000006, alpha = 0.1
sequence x = steepestDescent({0.1,-1}, alpha, tolerance)
printf(1,"Testing steepest descent method:\n")
printf(1,"The minimum is at x[1] = %.16f,  x[1] = %.16f\n", x)
```

```txt

Testing steepest descent method:
The minimum is at x[1] = 0.1076572080934996,    x[1] = -1.2232976080475890  -- (64 bit)
The minimum is at x[1] = 0.1073980565405569,    x[1] = -1.2233251778997771  -- (32 bit)

```



## Racket


Note the different implementation of <code>grad</code>. I believe that the vector should be reset and only the partial derivative in a particular dimension is to be used. For this reason, I've _yet another_ result!

I could have used ∇ and Δ in the variable names, but it looked too confusing, so I've gone with <var>grad-</var> and <var>del-</var>


```racket
#lang racket

(define (apply-vector f v)
  (apply f (vector->list v)))

;; Provides a rough calculation of gradient g(v).
(define ((grad/del f) v δ #:fv (fv (apply-vector f v)))
  (define dim (vector-length v))
  (define tmp (vector-copy v))
  (define grad (for/vector #:length dim ((i dim)
                            (v_i v))
              (vector-set! tmp i (+ v_i δ))
              (define ∂f/∂v_i (/ (- (apply-vector f tmp) fv) δ))
              (vector-set! tmp i v_i)
              ∂f/∂v_i))
  (values grad (sqrt (for/sum ((∂_i grad)) (sqr ∂_i)))))

(define (steepest-descent g x α tolerance)
  (define grad/del-g (grad/del g))

  (define (loop x δ α gx grad-gx del-gx b)
    (cond
      [(<= del-gx tolerance) x]
      [else
        (define δ´ (/ δ 2))
        (define x´ (vector-map + (vector-map (curry * (- b)) grad-gx) x))
        (define gx´ (apply-vector g x´))
        (define-values (grad-gx´ del-gx´) (grad/del-g x´ δ´ #:fv gx´))
        (define b´ (/ α del-gx´))
        (if (> gx´ gx)
            (loop x´ δ´ (/ α 2) gx  grad-gx´ del-gx´ b´)
            (loop x´ δ´ α       gx´ grad-gx´ del-gx´ b´))]))

  (define gx (apply-vector g x))
  (define δ tolerance)
  (define-values (grad-gx del-gx) (grad/del-g x δ #:fv gx))
  (loop x δ α gx grad-gx del-gx (/ α del-gx)))

(define (Gradient-descent)
  (steepest-descent
    (λ (x y)
       (+ (* (- x 1) (- x 1) (exp (- (sqr y))))
        (* y (+ y 2) (exp (- (* 2 (sqr x)))))))
    #(0.1 -1.) 0.1 0.0000006))

(module+ main
  (Gradient-descent))

```


```txt
'#(0.10760797905122492 -1.2232993981966753)
```



## TypeScript

;Translation of :
*   [Numerical Methods, Algorithms and Tools in C# by Waldemar Dos Passos (18.2 Gradient Descent Method]





```Typescript

// Using the steepest-descent method to search
// for minimum values of a multi-variable function
export const steepestDescent = (x: number[], alpha: number, tolerance: number) => {

    let n: number = x.length; // size of input array
    let h: number = 0.0000006; //Tolerance factor
    let g0: number = g(x); //Initial estimate of result

    //Calculate initial gradient
    let fi: number[] = [n];

    //Calculate initial norm
    fi = GradG(x, h);
    // console.log("fi:"+fi);

    //Calculate initial norm
    let DelG: number = 0.0;

    for (let i: number = 0; i < n; ++i) {
        DelG += fi[i] * fi[i];
    }
    DelG = Math.sqrt(DelG);
    let b: number = alpha / DelG;

    //Iterate until value is <= tolerance limit
    while (DelG > tolerance) {
        //Calculate next value
        for (let i = 0; i < n; ++i) {
            x[i] -= b * fi[i];
        }
        h /= 2;

        //Calculate next gradient
        fi = GradG(x, h);
        //Calculate next norm
        DelG = 0;
        for (let i: number = 0; i < n; ++i) {
            DelG += fi[i] * fi[i];
        }

        DelG = Math.sqrt(DelG);
        b = alpha / DelG;

        //Calculate next value
        let g1: number = g(x);

        //Adjust parameter
        if (g1 > g0) alpha /= 2;
        else g0 = g1;
    }
}

// Provides a rough calculation of gradient g(x).
export const GradG = (x: number[], h: number) => {

    let n: number = x.length;
    let z: number[] = [n];
    let y: number[] = x;
    let g0: number = g(x);

    // console.log("y:" + y);

    for (let i = 0; i < n; ++i) {
        y[i] += h;
        z[i] = (g(y) - g0) / h;
    }
    // console.log("z:"+z);
    return z;
}

// Method to provide function g(x).
export const g = (x: number[]) => {
    return (x[0] - 1) * (x[0] - 1)
        * Math.exp(-x[1] * x[1]) + x[1] * (x[1] + 2)
        * Math.exp(-2 * x[0] * x[0]);
}

export const gradientDescentMain = () => {
    let tolerance: number = 0.0000006;
    let alpha: number = 0.1;
    let x: number[] = [2];

    //Initial guesses
    x[0] = 0.1;
    //of location of minimums 
    x[1] = -1;
    steepestDescent(x, alpha, tolerance);

    console.log("Testing steepest descent method");
    console.log("The minimum is at x[0] = " + x[0]
        + ", x[1] = " + x[1]);
    // console.log("");
}

gradientDescentMain();


```

```txt

Testing steepest descent method
The minimum is at x[0] = 0.10768224291553158, x[1] = -1.2233090211217854

```



### Linear Regression

;Translation of :
*   [Linear Regression using Gradient Descent by Adarsh Menon]




```Typescript

let data: number[][] =
    [[32.5023452694530, 31.70700584656990],
    [53.4268040332750, 68.77759598163890],
    [61.5303580256364, 62.56238229794580],
    [47.4756396347860, 71.54663223356770],
    [59.8132078695123, 87.23092513368730],
    [55.1421884139438, 78.21151827079920],
    [52.2117966922140, 79.64197304980870],
    [39.2995666943170, 59.17148932186950],
    [48.1050416917682, 75.33124229706300],
    [52.5500144427338, 71.30087988685030],
    [45.4197301449737, 55.16567714595910],
    [54.3516348812289, 82.47884675749790],
    [44.1640494967733, 62.00892324572580],
    [58.1684707168577, 75.39287042599490],
    [56.7272080570966, 81.43619215887860],
    [48.9558885660937, 60.72360244067390],
    [44.6871962314809, 82.89250373145370],
    [60.2973268513334, 97.37989686216600],
    [45.6186437729558, 48.84715331735500],
    [38.8168175374456, 56.87721318626850],
    [66.1898166067526, 83.87856466460270],
    [65.4160517451340, 118.59121730252200],
    [47.4812086078678, 57.25181946226890],
    [41.5756426174870, 51.39174407983230],
    [51.8451869056394, 75.38065166531230],
    [59.3708220110895, 74.76556403215130],
    [57.3100034383480, 95.45505292257470],
    [63.6155612514533, 95.22936601755530],
    [46.7376194079769, 79.05240616956550],
    [50.5567601485477, 83.43207142132370],
    [52.2239960855530, 63.35879031749780],
    [35.5678300477466, 41.41288530370050],
    [42.4364769440556, 76.61734128007400],
    [58.1645401101928, 96.76956642610810],
    [57.5044476153417, 74.08413011660250],
    [45.4405307253199, 66.58814441422850],
    [61.8962226802912, 77.76848241779300],
    [33.0938317361639, 50.71958891231200],
    [36.4360095113868, 62.12457081807170],
    [37.6756548608507, 60.81024664990220],
    [44.5556083832753, 52.68298336638770],
    [43.3182826318657, 58.56982471769280],
    [50.0731456322890, 82.90598148507050],
    [43.8706126452183, 61.42470980433910],
    [62.9974807475530, 115.24415280079500],
    [32.6690437634671, 45.57058882337600],
    [40.1668990087037, 54.08405479622360],
    [53.5750775316736, 87.99445275811040],
    [33.8642149717782, 52.72549437590040],
    [64.7071386661212, 93.57611869265820],
    [38.1198240268228, 80.16627544737090],
    [44.5025380646451, 65.10171157056030],
    [40.5995383845523, 65.56230126040030],
    [41.7206763563412, 65.28088692082280],
    [51.0886346783367, 73.43464154632430],
    [55.0780959049232, 71.13972785861890],
    [41.3777265348952, 79.10282968354980],
    [62.4946974272697, 86.52053844034710],
    [49.2038875408260, 84.74269780782620],
    [41.1026851873496, 59.35885024862490],
    [41.1820161051698, 61.68403752483360],
    [50.1863894948806, 69.84760415824910],
    [52.3784462192362, 86.09829120577410],
    [50.1354854862861, 59.10883926769960],
    [33.6447060061917, 69.89968164362760],
    [39.5579012229068, 44.86249071116430],
    [56.1303888168754, 85.49806777884020],
    [57.3620521332382, 95.53668684646720],
    [60.2692143939979, 70.25193441977150],
    [35.6780938894107, 52.72173496477490],
    [31.5881169981328, 50.39267013507980],
    [53.6609322616730, 63.64239877565770],
    [46.6822286494719, 72.24725106866230],
    [43.1078202191024, 57.81251297618140],
    [70.3460756150493, 104.25710158543800],
    [44.4928558808540, 86.64202031882200],
    [57.5045333032684, 91.48677800011010],
    [36.9300766091918, 55.23166088621280],
    [55.8057333579427, 79.55043667850760],
    [38.9547690733770, 44.84712424246760],
    [56.9012147022470, 80.20752313968270],
    [56.8689006613840, 83.14274979204340],
    [34.3331247042160, 55.72348926054390],
    [59.0497412146668, 77.63418251167780],
    [57.7882239932306, 99.05141484174820],
    [54.2823287059674, 79.12064627468000],
    [51.0887198989791, 69.58889785111840],
    [50.2828363482307, 69.51050331149430],
    [44.2117417520901, 73.68756431831720],
    [38.0054880080606, 61.36690453724010],
    [32.9404799426182, 67.17065576899510],
    [53.6916395710700, 85.66820314500150],
    [68.7657342696216, 114.85387123391300],
    [46.2309664983102, 90.12357206996740],
    [68.3193608182553, 97.91982103524280],
    [50.0301743403121, 81.53699078301500],
    [49.2397653427537, 72.11183246961560],
    [50.0395759398759, 85.23200734232560],
    [48.1498588910288, 66.22495788805460],
    [25.1284846477723, 53.45439421485050]];

function lossFunction(arr0: number[], arr1: number[], arr2: number[]) {

    let n: number = arr0.length; // Number of elements in X

    //D_m = (-2/n) * sum(X * (Y - Y_pred))  # Derivative wrt m
    let a: number = (-2 / n) * (arr0.map((a, i) => a * (arr1[i] - arr2[i]))).reduce((sum, current) => sum + current);
    //D_c = (-2/n) * sum(Y - Y_pred)  # Derivative wrt c
    let b: number = (-2 / n) * (arr1.map((a, i) => (a - arr2[i]))).reduce((sum, current) => sum + current);
    return [a, b];
}

export const gradientDescentMain = () => {

    // Building the model
    let m: number = 0;
    let c: number = 0;
    let X_arr: number[];
    let Y_arr: number[];
    let Y_pred_arr: number[];
    let D_m: number = 0;
    let D_c: number = 0;

    let L: number = 0.00000001;  // The learning Rate
    let epochs: number = 10000000;  // The number of iterations to perform gradient descent

    //Initial guesses
    for (let i = 0; i < epochs; i++) {
        X_arr = data.map(function (value, index) { return value[0]; });
        Y_arr = data.map(function (value, index) { return value[1]; });

        // The current predicted value of Y
        Y_pred_arr = X_arr.map((a) => ((m * a) + c));

        let all = lossFunction(X_arr, Y_arr, Y_pred_arr);
        D_m = all[0];
        D_c = all[1];

        m = m - L * D_m;  // Update m
        c = c - L * D_c;  // Update c
    }

    console.log("m: " + m + " c: " + c);
}

gradientDescentMain();

```

