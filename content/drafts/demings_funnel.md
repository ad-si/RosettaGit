+++
title = "Deming's Funnel"
description = ""
date = 2019-07-14T10:49:53Z
aliases = []
[extra]
id = 14060
[taxonomies]
categories = []
tags = []
+++

{{task}}
[[wp:W. Edwards Deming|W Edwards Deming]] was an American statistician and management guru who used physical demonstrations to illuminate his teachings.  In one demonstration Deming repeatedly dropped marbles through a funnel at a target, marking where they landed, and observing the resulting pattern.  He applied a sequence of "rules" to try to improve performance.  In each case the experiment begins with the funnel positioned directly over the target.

* '''Rule 1''': The funnel remains directly above the target.
* '''Rule 2''': Adjust the funnel position by shifting the target to compensate after each drop.  E.g. If the last drop missed 1 cm east, move the funnel 1 cm to the west of its current position.
* '''Rule 3''': As rule 2, but first move the funnel back over the target, before making the adjustment.  E.g. If the funnel is 2 cm north, and the marble lands 3 cm north, move the funnel 3 cm south of the target.
* '''Rule 4''': The funnel is moved directly over the last place a marble landed.

Apply the four rules to the set of 50 pseudorandom displacements provided (e.g in the Racket solution) for the dxs and dys.  '''Output''': calculate the mean and standard-deviations of the resulting x and y values for each rule.  

Note that rules 2, 3, and 4 give successively worse results.  Trying to deterministically compensate for a random process is counter-productive, but -- according to Deming -- quite a popular pastime: see the Further Information, below for examples.

'''Stretch goal 1''': Generate fresh pseudorandom data.  The radial displacement of the drop from the funnel position is given by a Gaussian distribution (standard deviation is 1.0) and the angle of displacement is uniformly distributed.

'''Stretch goal 2''': Show scatter plots of all four results.


;Further information:
* Further [http://blog.newsystemsthinking.com/w-edwards-deming-and-the-funnel-experiment/ explanation and interpretation]
* [https://www.youtube.com/watch?v=2VogtYRc9dA Video demonstration] of the funnel experiment at the Mayo Clinic.


## D

{{trans|Python}}

```d
import std.stdio, std.math, std.algorithm, std.range, std.typecons;

auto mean(T)(in T[] xs) pure nothrow @nogc {
    return xs.sum / xs.length;
}

auto stdDev(T)(in T[] xs) pure nothrow {
    immutable m = xs.mean;
    return sqrt(xs.map!(x => (x - m) ^^ 2).sum / xs.length);
}

alias TF = double function(in double, in double) pure nothrow @nogc;

auto funnel(T)(in T[] dxs, in T[] dys, in TF rule) {
    T x = 0, y = 0;
    immutable(T)[] rxs, rys;

    foreach (const dx, const dy; zip(dxs, dys)) {
        immutable rx = x + dx;
        immutable ry = y + dy;
        x = rule(x, dx);
        y = rule(y, dy);
        rxs ~= rx;
        rys ~= ry;
    }

    return tuple!("x", "y")(rxs, rys);
}

void experiment(T)(in string label,
                   in T[] dxs, in T[] dys, in TF rule) {
    //immutable (rxs, rys) = funnel(dxs, dys, rule);
    immutable rs = funnel(dxs, dys, rule);
    label.writeln;
    writefln("Mean x, y:    %.4f, %.4f", rs.x.mean, rs.y.mean);
    writefln("Std dev x, y: %.4f, %.4f", rs.x.stdDev, rs.y.stdDev);
    writeln;
}

void main() {
    immutable dxs = [
    -0.533,  0.270,  0.859, -0.043, -0.205, -0.127, -0.071,  0.275,
     1.251, -0.231, -0.401,  0.269,  0.491,  0.951,  1.150,  0.001,
    -0.382,  0.161,  0.915,  2.080, -2.337,  0.034, -0.126,  0.014,
     0.709,  0.129, -1.093, -0.483, -1.193,  0.020, -0.051,  0.047,
    -0.095,  0.695,  0.340, -0.182,  0.287,  0.213, -0.423, -0.021,
    -0.134,  1.798,  0.021, -1.099, -0.361,  1.636, -1.134,  1.315,
     0.201,  0.034,  0.097, -0.170,  0.054, -0.553, -0.024, -0.181,
    -0.700, -0.361, -0.789,  0.279, -0.174, -0.009, -0.323, -0.658,
     0.348, -0.528,  0.881,  0.021, -0.853,  0.157,  0.648,  1.774,
    -1.043,  0.051,  0.021,  0.247, -0.310,  0.171,  0.000,  0.106,
     0.024, -0.386,  0.962,  0.765, -0.125, -0.289,  0.521,  0.017,
     0.281, -0.749, -0.149, -2.436, -0.909,  0.394, -0.113, -0.598,
     0.443, -0.521, -0.799,  0.087];

    immutable dys = [
     0.136,  0.717,  0.459, -0.225,  1.392,  0.385,  0.121, -0.395,
     0.490, -0.682, -0.065,  0.242, -0.288,  0.658,  0.459,  0.000,
     0.426,  0.205, -0.765, -2.188, -0.742, -0.010,  0.089,  0.208,
     0.585,  0.633, -0.444, -0.351, -1.087,  0.199,  0.701,  0.096,
    -0.025, -0.868,  1.051,  0.157,  0.216,  0.162,  0.249, -0.007,
     0.009,  0.508, -0.790,  0.723,  0.881, -0.508,  0.393, -0.226,
     0.710,  0.038, -0.217,  0.831,  0.480,  0.407,  0.447, -0.295,
     1.126,  0.380,  0.549, -0.445, -0.046,  0.428, -0.074,  0.217,
    -0.822,  0.491,  1.347, -0.141,  1.230, -0.044,  0.079,  0.219,
     0.698,  0.275,  0.056,  0.031,  0.421,  0.064,  0.721,  0.104,
    -0.729,  0.650, -1.103,  0.154, -1.720,  0.051, -0.385,  0.477,
     1.537, -0.901,  0.939, -0.411,  0.341, -0.411,  0.106,  0.224,
    -0.947, -1.424, -0.542, -1.032];

    static assert(dxs.length == dys.length);

    experiment("Rule 1:", dxs, dys, (z, dz) => 0.0);
    experiment("Rule 2:", dxs, dys, (z, dz) => -dz);
    experiment("Rule 3:", dxs, dys, (z, dz) => -(z + dz));
    experiment("Rule 4:", dxs, dys, (z, dz) => z + dz);
}
```

{{out}}

```txt
Rule 1:
Mean x, y:    0.0004, 0.0702
Std dev x, y: 0.7153, 0.6462

Rule 2:
Mean x, y:    0.0008, -0.0103
Std dev x, y: 1.0371, 0.8999

Rule 3:
Mean x, y:    0.0438, -0.0063
Std dev x, y: 7.9871, 4.7784

Rule 4:
Mean x, y:    3.1341, 5.4210
Std dev x, y: 1.5874, 3.9304
```



## Elixir

{{trans|Ruby}}

```elixir
defmodule Deming do
  def funnel(dxs, rule) do
    {_, rxs} = Enum.reduce(dxs, {0, []}, fn dx,{x,rxs} ->
      {rule.(x, dx), [x + dx | rxs]}
    end)
    rxs
  end
  
  def mean(xs), do: Enum.sum(xs) / length(xs)
  
  def stddev(xs) do
    m = mean(xs)
    Enum.reduce(xs, 0.0, fn x,sum -> sum + (x-m)*(x-m) / length(xs) end)
    |> :math.sqrt
  end
  
  def experiment(label, dxs, dys, rule) do
    {rxs, rys} = {funnel(dxs, rule), funnel(dys, rule)}
    IO.puts label
    :io.format "Mean x, y    : ~7.4f, ~7.4f~n",   [mean(rxs), mean(rys)]
    :io.format "Std dev x, y : ~7.4f, ~7.4f~n~n", [stddev(rxs), stddev(rys)]
  end
end

dxs = [ -0.533,  0.270,  0.859, -0.043, -0.205, -0.127, -0.071,  0.275,
         1.251, -0.231, -0.401,  0.269,  0.491,  0.951,  1.150,  0.001,
        -0.382,  0.161,  0.915,  2.080, -2.337,  0.034, -0.126,  0.014,
         0.709,  0.129, -1.093, -0.483, -1.193,  0.020, -0.051,  0.047,
        -0.095,  0.695,  0.340, -0.182,  0.287,  0.213, -0.423, -0.021,
        -0.134,  1.798,  0.021, -1.099, -0.361,  1.636, -1.134,  1.315,
         0.201,  0.034,  0.097, -0.170,  0.054, -0.553, -0.024, -0.181,
        -0.700, -0.361, -0.789,  0.279, -0.174, -0.009, -0.323, -0.658,
         0.348, -0.528,  0.881,  0.021, -0.853,  0.157,  0.648,  1.774,
        -1.043,  0.051,  0.021,  0.247, -0.310,  0.171,  0.000,  0.106,
         0.024, -0.386,  0.962,  0.765, -0.125, -0.289,  0.521,  0.017,
         0.281, -0.749, -0.149, -2.436, -0.909,  0.394, -0.113, -0.598,
         0.443, -0.521, -0.799,  0.087]

dys = [  0.136,  0.717,  0.459, -0.225,  1.392,  0.385,  0.121, -0.395,
         0.490, -0.682, -0.065,  0.242, -0.288,  0.658,  0.459,  0.000,
         0.426,  0.205, -0.765, -2.188, -0.742, -0.010,  0.089,  0.208,
         0.585,  0.633, -0.444, -0.351, -1.087,  0.199,  0.701,  0.096,
        -0.025, -0.868,  1.051,  0.157,  0.216,  0.162,  0.249, -0.007,
         0.009,  0.508, -0.790,  0.723,  0.881, -0.508,  0.393, -0.226,
         0.710,  0.038, -0.217,  0.831,  0.480,  0.407,  0.447, -0.295,
         1.126,  0.380,  0.549, -0.445, -0.046,  0.428, -0.074,  0.217,
        -0.822,  0.491,  1.347, -0.141,  1.230, -0.044,  0.079,  0.219,
         0.698,  0.275,  0.056,  0.031,  0.421,  0.064,  0.721,  0.104,
        -0.729,  0.650, -1.103,  0.154, -1.720,  0.051, -0.385,  0.477,
         1.537, -0.901,  0.939, -0.411,  0.341, -0.411,  0.106,  0.224,
        -0.947, -1.424, -0.542, -1.032]

Deming.experiment("Rule 1:", dxs, dys, fn _z, _dz -> 0 end)
Deming.experiment("Rule 2:", dxs, dys, fn _z, dz -> -dz end)
Deming.experiment("Rule 3:", dxs, dys, fn z, dz -> -(z+dz) end)
Deming.experiment("Rule 4:", dxs, dys, fn z, dz -> z+dz end)
```


{{out}}

```txt

Rule 1:
Mean x, y    :  0.0004,  0.0702
Std dev x, y :  0.7153,  0.6462

Rule 2:
Mean x, y    :  0.0009, -0.0103
Std dev x, y :  1.0371,  0.8999

Rule 3:
Mean x, y    :  0.0439, -0.0063
Std dev x, y :  7.9871,  4.7784

Rule 4:
Mean x, y    :  3.1341,  5.4210
Std dev x, y :  1.5874,  3.9304

```



## Go

{{trans|Python}}

```go
package main

import (
    "fmt"
    "math"
)

type rule func(float64, float64) float64

var dxs = []float64{
    -0.533,  0.270,  0.859, -0.043, -0.205, -0.127, -0.071,  0.275,
     1.251, -0.231, -0.401,  0.269,  0.491,  0.951,  1.150,  0.001,
    -0.382,  0.161,  0.915,  2.080, -2.337,  0.034, -0.126,  0.014,
     0.709,  0.129, -1.093, -0.483, -1.193,  0.020, -0.051,  0.047,
    -0.095,  0.695,  0.340, -0.182,  0.287,  0.213, -0.423, -0.021,
    -0.134,  1.798,  0.021, -1.099, -0.361,  1.636, -1.134,  1.315,
     0.201,  0.034,  0.097, -0.170,  0.054, -0.553, -0.024, -0.181,
    -0.700, -0.361, -0.789,  0.279, -0.174, -0.009, -0.323, -0.658,
     0.348, -0.528,  0.881,  0.021, -0.853,  0.157,  0.648,  1.774,
    -1.043,  0.051,  0.021,  0.247, -0.310,  0.171,  0.000,  0.106,
     0.024, -0.386,  0.962,  0.765, -0.125, -0.289,  0.521,  0.017,
     0.281, -0.749, -0.149, -2.436, -0.909,  0.394, -0.113, -0.598,
     0.443, -0.521, -0.799,  0.087,
}

var dys = []float64{
     0.136,  0.717,  0.459, -0.225,  1.392,  0.385,  0.121, -0.395,
     0.490, -0.682, -0.065,  0.242, -0.288,  0.658,  0.459,  0.000,
     0.426,  0.205, -0.765, -2.188, -0.742, -0.010,  0.089,  0.208,
     0.585,  0.633, -0.444, -0.351, -1.087,  0.199,  0.701,  0.096,
    -0.025, -0.868,  1.051,  0.157,  0.216,  0.162,  0.249, -0.007,
     0.009,  0.508, -0.790,  0.723,  0.881, -0.508,  0.393, -0.226,
     0.710,  0.038, -0.217,  0.831,  0.480,  0.407,  0.447, -0.295,
     1.126,  0.380,  0.549, -0.445, -0.046,  0.428, -0.074,  0.217,
    -0.822,  0.491,  1.347, -0.141,  1.230, -0.044,  0.079,  0.219,
     0.698,  0.275,  0.056,  0.031,  0.421,  0.064,  0.721,  0.104,
    -0.729,  0.650, -1.103,  0.154, -1.720,  0.051, -0.385,  0.477,
     1.537, -0.901,  0.939, -0.411,  0.341, -0.411,  0.106,  0.224,
    -0.947, -1.424, -0.542, -1.032,
}

func funnel(fa []float64, r rule) []float64 {
    x := 0.0
    result := make([]float64, len(fa))
    for i, f := range fa {
        result[i] = x + f
        x = r(x, f)
    }
    return result
}

func mean(fa []float64) float64 {
    sum := 0.0
    for _, f := range fa {
        sum += f
    }
    return sum / float64(len(fa))
}

func stdDev(fa []float64) float64 {
    m := mean(fa)
    sum := 0.0
    for _, f := range fa {
        sum += (f - m) * (f - m)
    }
    return math.Sqrt(sum / float64(len(fa)))
}

func experiment(label string, r rule) {
    rxs := funnel(dxs, r)
    rys := funnel(dys, r)
    fmt.Println(label, " :      x        y")
    fmt.Printf("Mean    :  %7.4f, %7.4f\n", mean(rxs), mean(rys))
    fmt.Printf("Std Dev :  %7.4f, %7.4f\n", stdDev(rxs), stdDev(rys))
    fmt.Println()
}

func main() {
    experiment("Rule 1", func(_, _ float64) float64 {
        return 0.0
    })
    experiment("Rule 2", func(_, dz float64) float64 {
        return -dz
    })
    experiment("Rule 3", func(z, dz float64) float64 {
        return -(z + dz)
    })
    experiment("Rule 4", func(z, dz float64) float64 {
        return z + dz
    })
}
```


{{out}}

```txt

Rule 1  :      x        y
Mean    :   0.0004,  0.0702
Std Dev :   0.7153,  0.6462

Rule 2  :      x        y
Mean    :   0.0009, -0.0103
Std Dev :   1.0371,  0.8999

Rule 3  :      x        y
Mean    :   0.0439, -0.0063
Std Dev :   7.9871,  4.7784

Rule 4  :      x        y
Mean    :   3.1341,  5.4210
Std Dev :   1.5874,  3.9304

```



## Haskell

{{trans|Python}}

```haskell
import Data.List (mapAccumL, genericLength)
import Text.Printf

funnel :: (Num a) => (a -> a -> a) -> [a] -> [a]
funnel rule = snd . mapAccumL (\x dx -> (rule x dx, x + dx)) 0

mean :: (Fractional a) => [a] -> a 
mean xs = sum xs / genericLength xs

stddev :: (Floating a) => [a] -> a
stddev xs = sqrt $ sum [(x-m)**2 | x <- xs] / genericLength xs where
              m = mean xs

experiment :: String -> [Double] -> [Double] -> (Double -> Double -> Double) -> IO ()
experiment label dxs dys rule = do
  let rxs = funnel rule dxs
      rys = funnel rule dys
  putStrLn label
  printf "Mean x, y    : %7.4f, %7.4f\n" (mean rxs) (mean rys)
  printf "Std dev x, y : %7.4f, %7.4f\n" (stddev rxs) (stddev rys)
  putStrLn ""


dxs = [ -0.533,  0.270,  0.859, -0.043, -0.205, -0.127, -0.071,  0.275,
         1.251, -0.231, -0.401,  0.269,  0.491,  0.951,  1.150,  0.001,
        -0.382,  0.161,  0.915,  2.080, -2.337,  0.034, -0.126,  0.014,
         0.709,  0.129, -1.093, -0.483, -1.193,  0.020, -0.051,  0.047,
        -0.095,  0.695,  0.340, -0.182,  0.287,  0.213, -0.423, -0.021,
        -0.134,  1.798,  0.021, -1.099, -0.361,  1.636, -1.134,  1.315,
         0.201,  0.034,  0.097, -0.170,  0.054, -0.553, -0.024, -0.181,
        -0.700, -0.361, -0.789,  0.279, -0.174, -0.009, -0.323, -0.658,
         0.348, -0.528,  0.881,  0.021, -0.853,  0.157,  0.648,  1.774,
        -1.043,  0.051,  0.021,  0.247, -0.310,  0.171,  0.000,  0.106,
         0.024, -0.386,  0.962,  0.765, -0.125, -0.289,  0.521,  0.017,
         0.281, -0.749, -0.149, -2.436, -0.909,  0.394, -0.113, -0.598,
         0.443, -0.521, -0.799,  0.087]

dys = [  0.136,  0.717,  0.459, -0.225,  1.392,  0.385,  0.121, -0.395,
         0.490, -0.682, -0.065,  0.242, -0.288,  0.658,  0.459,  0.000,
         0.426,  0.205, -0.765, -2.188, -0.742, -0.010,  0.089,  0.208,
         0.585,  0.633, -0.444, -0.351, -1.087,  0.199,  0.701,  0.096,
        -0.025, -0.868,  1.051,  0.157,  0.216,  0.162,  0.249, -0.007,
         0.009,  0.508, -0.790,  0.723,  0.881, -0.508,  0.393, -0.226,
         0.710,  0.038, -0.217,  0.831,  0.480,  0.407,  0.447, -0.295,
         1.126,  0.380,  0.549, -0.445, -0.046,  0.428, -0.074,  0.217,
        -0.822,  0.491,  1.347, -0.141,  1.230, -0.044,  0.079,  0.219,
         0.698,  0.275,  0.056,  0.031,  0.421,  0.064,  0.721,  0.104,
        -0.729,  0.650, -1.103,  0.154, -1.720,  0.051, -0.385,  0.477,
         1.537, -0.901,  0.939, -0.411,  0.341, -0.411,  0.106,  0.224,
        -0.947, -1.424, -0.542, -1.032]

main :: IO ()
main = do
  experiment "Rule 1:" dxs dys (\_ _  -> 0)
  experiment "Rule 2:" dxs dys (\_ dz -> -dz)
  experiment "Rule 3:" dxs dys (\z dz -> -(z+dz))
  experiment "Rule 4:" dxs dys (\z dz -> z+dz)
```


{{out}}

```txt

Rule 1:
Mean x, y    :  0.0004,  0.0702
Std dev x, y :  0.7153,  0.6462

Rule 2:
Mean x, y    :  0.0009, -0.0103
Std dev x, y :  1.0371,  0.8999

Rule 3:
Mean x, y    :  0.0439, -0.0063
Std dev x, y :  7.9871,  4.7784

Rule 4:
Mean x, y    :  3.1341,  5.4210
Std dev x, y :  1.5874,  3.9304

```



## J



```J

dx=:".0 :0-.LF
 _0.533 0.270 0.859 _0.043 _0.205 _0.127 _0.071 0.275
 1.251 _0.231 _0.401 0.269 0.491 0.951 1.150 0.001
 _0.382 0.161 0.915 2.080 _2.337 0.034 _0.126 0.014
 0.709 0.129 _1.093 _0.483 _1.193 0.020 _0.051 0.047
 _0.095 0.695 0.340 _0.182 0.287 0.213 _0.423 _0.021
 _0.134 1.798 0.021 _1.099 _0.361 1.636 _1.134 1.315
 0.201 0.034 0.097 _0.170 0.054 _0.553 _0.024 _0.181
 _0.700 _0.361 _0.789 0.279 _0.174 _0.009 _0.323 _0.658
 0.348 _0.528 0.881 0.021 _0.853 0.157 0.648 1.774
 _1.043 0.051 0.021 0.247 _0.310 0.171 0.000 0.106
 0.024 _0.386 0.962 0.765 _0.125 _0.289 0.521 0.017
 0.281 _0.749 _0.149 _2.436 _0.909 0.394 _0.113 _0.598
 0.443 _0.521 _0.799 0.087
)
 
dy=:".0 :0-.LF
 0.136 0.717 0.459 _0.225 1.392 0.385 0.121 _0.395
 0.490 _0.682 _0.065 0.242 _0.288 0.658 0.459 0.000
 0.426 0.205 _0.765 _2.188 _0.742 _0.010 0.089 0.208
 0.585 0.633 _0.444 _0.351 _1.087 0.199 0.701 0.096
 _0.025 _0.868 1.051 0.157 0.216 0.162 0.249 _0.007
 0.009 0.508 _0.790 0.723 0.881 _0.508 0.393 _0.226
 0.710 0.038 _0.217 0.831 0.480 0.407 0.447 _0.295
 1.126 0.380 0.549 _0.445 _0.046 0.428 _0.074 0.217
 _0.822 0.491 1.347 _0.141 1.230 _0.044 0.079 0.219
 0.698 0.275 0.056 0.031 0.421 0.064 0.721 0.104
 _0.729 0.650 _1.103 0.154 _1.720 0.051 _0.385 0.477
 1.537 _0.901 0.939 _0.411 0.341 _0.411 0.106 0.224
 _0.947 _1.424 _0.542 _1.032
)

Rule1=: ]
Rule2=: -/\.&.|.
Rule3=: ]-0,}:
Rule4=: ]+0,}:

smoutput '  Rule 1 (x,y):'
smoutput '  Mean: ',":dx ,&mean&Rule1 dy
smoutput '  Std dev: ',":dx ,&stddev&Rule1 dy
smoutput '  '
smoutput '  Rule 2 (x,y):'
smoutput '  Mean: ',":dx ,&mean&Rule2 dy
smoutput '  Std dev: ',":dx ,&stddev&Rule2 dy
smoutput '  '
smoutput '  Rule 3 (x,y):'
smoutput '  Mean: ',":dx ,&mean&Rule3 dy
smoutput '  Std dev: ',":dx ,&stddev&Rule3 dy
smoutput '  '
smoutput '  Rule 4 (x,y):'
smoutput '  Mean: ',":dx ,&mean&Rule4 dy
smoutput '  Std dev: ',":dx ,&stddev&Rule4 dy
```


Displayed result:

  Rule 1 (x,y):
  Mean: 0.0004 0.07023
  Std dev: 0.718875 0.649462
  
  Rule 2 (x,y):
  Mean: 0.04386 _0.0063
  Std dev: 8.02735 4.80249
  
  Rule 3 (x,y):
  Mean: 0.00087 _0.01032
  Std dev: 1.04236 0.904482
  
  Rule 4 (x,y):
  Mean: _7e_5 0.15078
  Std dev: 0.990174 0.918942

Author's note: these numbers are different from those of other implementations. I claim that this represents errors in the other implementations and invite proof that I am wrong.


## Java

Translation of [[Deming's_Funnel#Python|Python]] via [[Deming's_Funnel#D|D]]
{{works with|Java|8}}

```java
import static java.lang.Math.*;
import java.util.Arrays;
import java.util.function.BiFunction;

public class DemingsFunnel {

    public static void main(String[] args) {
        double[] dxs = {
            -0.533, 0.270, 0.859, -0.043, -0.205, -0.127, -0.071, 0.275,
            1.251, -0.231, -0.401, 0.269, 0.491, 0.951, 1.150, 0.001,
            -0.382, 0.161, 0.915, 2.080, -2.337, 0.034, -0.126, 0.014,
            0.709, 0.129, -1.093, -0.483, -1.193, 0.020, -0.051, 0.047,
            -0.095, 0.695, 0.340, -0.182, 0.287, 0.213, -0.423, -0.021,
            -0.134, 1.798, 0.021, -1.099, -0.361, 1.636, -1.134, 1.315,
            0.201, 0.034, 0.097, -0.170, 0.054, -0.553, -0.024, -0.181,
            -0.700, -0.361, -0.789, 0.279, -0.174, -0.009, -0.323, -0.658,
            0.348, -0.528, 0.881, 0.021, -0.853, 0.157, 0.648, 1.774,
            -1.043, 0.051, 0.021, 0.247, -0.310, 0.171, 0.000, 0.106,
            0.024, -0.386, 0.962, 0.765, -0.125, -0.289, 0.521, 0.017,
            0.281, -0.749, -0.149, -2.436, -0.909, 0.394, -0.113, -0.598,
            0.443, -0.521, -0.799, 0.087};

        double[] dys = {
            0.136, 0.717, 0.459, -0.225, 1.392, 0.385, 0.121, -0.395,
            0.490, -0.682, -0.065, 0.242, -0.288, 0.658, 0.459, 0.000,
            0.426, 0.205, -0.765, -2.188, -0.742, -0.010, 0.089, 0.208,
            0.585, 0.633, -0.444, -0.351, -1.087, 0.199, 0.701, 0.096,
            -0.025, -0.868, 1.051, 0.157, 0.216, 0.162, 0.249, -0.007,
            0.009, 0.508, -0.790, 0.723, 0.881, -0.508, 0.393, -0.226,
            0.710, 0.038, -0.217, 0.831, 0.480, 0.407, 0.447, -0.295,
            1.126, 0.380, 0.549, -0.445, -0.046, 0.428, -0.074, 0.217,
            -0.822, 0.491, 1.347, -0.141, 1.230, -0.044, 0.079, 0.219,
            0.698, 0.275, 0.056, 0.031, 0.421, 0.064, 0.721, 0.104,
            -0.729, 0.650, -1.103, 0.154, -1.720, 0.051, -0.385, 0.477,
            1.537, -0.901, 0.939, -0.411, 0.341, -0.411, 0.106, 0.224,
            -0.947, -1.424, -0.542, -1.032};

        experiment("Rule 1:", dxs, dys, (z, dz) -> 0.0);
        experiment("Rule 2:", dxs, dys, (z, dz) -> -dz);
        experiment("Rule 3:", dxs, dys, (z, dz) -> -(z + dz));
        experiment("Rule 4:", dxs, dys, (z, dz) -> z + dz);
    }

    static void experiment(String label, double[] dxs, double[] dys,
            BiFunction<Double, Double, Double> rule) {

        double[] resx = funnel(dxs, rule);
        double[] resy = funnel(dys, rule);
        System.out.println(label);
        System.out.printf("Mean x, y:    %.4f, %.4f%n", mean(resx), mean(resy));
        System.out.printf("Std dev x, y: %.4f, %.4f%n", stdDev(resx), stdDev(resy));
        System.out.println();
    }

    static double[] funnel(double[] input, BiFunction<Double, Double, Double> rule) {
        double x = 0;
        double[] result = new double[input.length];

        for (int i = 0; i < input.length; i++) {
            double rx = x + input[i];
            x = rule.apply(x, input[i]);
            result[i] = rx;
        }
        return result;
    }

    static double mean(double[] xs) {
        return Arrays.stream(xs).sum() / xs.length;
    }

    static double stdDev(double[] xs) {
        double m = mean(xs);
        return sqrt(Arrays.stream(xs).map(x -> pow((x - m), 2)).sum() / xs.length);
    }
}
```



```txt
Rule 1:
Mean x, y:    0,0004, 0,0702
Std dev x, y: 0,7153, 0,6462

Rule 2:
Mean x, y:    0,0009, -0,0103
Std dev x, y: 1,0371, 0,8999

Rule 3:
Mean x, y:    0,0439, -0,0063
Std dev x, y: 7,9871, 4,7784

Rule 4:
Mean x, y:    3,1341, 5,4210
Std dev x, y: 1,5874, 3,9304
```




## Julia


```julia
# Run from Julia REPL to see the plots.
using Statistics, Distributions, Plots

const racket_xdata = [-0.533, 0.270, 0.859, -0.043, -0.205, -0.127, -0.071, 0.275, 1.251, -0.231,
        -0.401, 0.269, 0.491, 0.951, 1.150, 0.001, -0.382, 0.161, 0.915, 2.080, -2.337,
         0.034, -0.126, 0.014, 0.709, 0.129, -1.093, -0.483, -1.193, 0.020, -0.051,
         0.047, -0.095, 0.695, 0.340, -0.182, 0.287, 0.213, -0.423, -0.021, -0.134, 1.798,
         0.021, -1.099, -0.361, 1.636, -1.134, 1.315, 0.201, 0.034, 0.097, -0.170, 0.054,
        -0.553, -0.024, -0.181, -0.700, -0.361, -0.789, 0.279, -0.174, -0.009, -0.323,
        -0.658, 0.348, -0.528, 0.881, 0.021, -0.853, 0.157, 0.648, 1.774, -1.043, 0.051,
         0.021, 0.247, -0.310, 0.171, 0.000, 0.106, 0.024, -0.386, 0.962, 0.765, -0.125,
        -0.289, 0.521, 0.017, 0.281, -0.749, -0.149, -2.436, -0.909, 0.394, -0.113, -0.598,
         0.443, -0.521, -0.799, 0.087]

const racket_ydata = [0.136, 0.717, 0.459, -0.225, 1.392, 0.385, 0.121, -0.395, 0.490, -0.682, -0.065,
        0.242, -0.288, 0.658, 0.459, 0.000, 0.426, 0.205, -0.765, -2.188, -0.742, -0.010,
        0.089, 0.208, 0.585, 0.633, -0.444, -0.351, -1.087, 0.199, 0.701, 0.096, -0.025,
        -0.868, 1.051, 0.157, 0.216, 0.162, 0.249, -0.007, 0.009, 0.508, -0.790, 0.723,
        0.881, -0.508, 0.393, -0.226, 0.710, 0.038, -0.217, 0.831, 0.480, 0.407, 0.447,
        -0.295, 1.126, 0.380, 0.549, -0.445, -0.046, 0.428, -0.074, 0.217, -0.822, 0.491,
        1.347, -0.141, 1.230, -0.044, 0.079, 0.219, 0.698, 0.275, 0.056, 0.031, 0.421, 0.064,
        0.721, 0.104, -0.729, 0.650, -1.103, 0.154, -1.720, 0.051, -0.385, 0.477, 1.537,
        -0.901, 0.939, -0.411, 0.341, -0.411, 0.106, 0.224, -0.947, -1.424, -0.542, -1.032]

const rules = [(x, y, dx, dy) -> [0, 0], (x, y, dx, dy) -> [-dx, -dy],
         (x, y, dx, dy) -> [-x - dx, -y - dy], (x, y, dx, dy) -> [x + dx, y + dy]]
const plots, colors = plot(layout=(1,2)), [:red, :green, :blue, :yellow]

function makedata()
    radius_angles = zip(rand(Normal(), 100), rand(Uniform(-π, π), 100))
    zip([z[1] * cos(z[2]) for z in radius_angles], [z[1] * sin(z[2]) for z in radius_angles])
end

function testfunnel(useracket=true)
    for (i, rule) in enumerate(rules)
        origin = [0.0, 0.0]
        xvec, yvec = Float64[], Float64[]
        for point in (useracket ? zip(racket_xdata, racket_ydata) : makedata())
            push!(xvec, origin[1] + point[1])
            push!(yvec, origin[2] + point[2])
            origin .= rule(origin[1], origin[2], point[1], point[2])
        end
        println("Rule $i results:")
        println("mean x: ", round(mean(xvec), digits=4), " std x: ", round(std(xvec, corrected=false), digits=4),
            " mean y: ", round(mean(yvec), digits=4), " std y: ", round(std(yvec, corrected=false), digits=4))
        scatter!(xvec, yvec, color=colors[i], subplot=(useracket ? 1 : 2),
            title= useracket ? "Racket Data" : "Random Data", label="Rule $i")
    end
end

println("\nUsing Racket data.")
testfunnel()
println("\nUsing new data.")
testfunnel(false)
display(plots)

```
{{out}}

```txt

Using Racket data.
Rule 1 results:
mean x: 0.0004 std x: 0.7153 mean y: 0.0702 std y: 0.6462
Rule 2 results:
mean x: 0.0009 std x: 1.0371 mean y: -0.0103 std y: 0.8999
Rule 3 results:
mean x: 0.0439 std x: 7.9871 mean y: -0.0063 std y: 4.7784
Rule 4 results:
mean x: 3.1341 std x: 1.5874 mean y: 5.421 std y: 3.9304

Using new data.
Rule 1 results:
mean x: -0.0814 std x: 0.7761 mean y: -0.0187 std y: 0.799
Rule 2 results:
mean x: 0.0009 std x: 0.9237 mean y: 0.0028 std y: 0.9626
Rule 3 results:
mean x: 0.0123 std x: 4.7695 mean y: 0.0658 std y: 3.7198
Rule 4 results:
mean x: -6.7132 std x: 4.5367 mean y: 1.632 std y: 2.0975

```



## Kotlin

{{trans|Python}}

```scala
// version 1.1.3

typealias Rule = (Double, Double) -> Double

val dxs = doubleArrayOf(
    -0.533,  0.270,  0.859, -0.043, -0.205, -0.127, -0.071,  0.275,
     1.251, -0.231, -0.401,  0.269,  0.491,  0.951,  1.150,  0.001,
    -0.382,  0.161,  0.915,  2.080, -2.337,  0.034, -0.126,  0.014,
     0.709,  0.129, -1.093, -0.483, -1.193,  0.020, -0.051,  0.047,
    -0.095,  0.695,  0.340, -0.182,  0.287,  0.213, -0.423, -0.021,
    -0.134,  1.798,  0.021, -1.099, -0.361,  1.636, -1.134,  1.315,
     0.201,  0.034,  0.097, -0.170,  0.054, -0.553, -0.024, -0.181,
    -0.700, -0.361, -0.789,  0.279, -0.174, -0.009, -0.323, -0.658,
     0.348, -0.528,  0.881,  0.021, -0.853,  0.157,  0.648,  1.774,
    -1.043,  0.051,  0.021,  0.247, -0.310,  0.171,  0.000,  0.106,
     0.024, -0.386,  0.962,  0.765, -0.125, -0.289,  0.521,  0.017,
     0.281, -0.749, -0.149, -2.436, -0.909,  0.394, -0.113, -0.598,
     0.443, -0.521, -0.799,  0.087
)

val dys = doubleArrayOf(
     0.136,  0.717,  0.459, -0.225,  1.392,  0.385,  0.121, -0.395,
     0.490, -0.682, -0.065,  0.242, -0.288,  0.658,  0.459,  0.000,
     0.426,  0.205, -0.765, -2.188, -0.742, -0.010,  0.089,  0.208,
     0.585,  0.633, -0.444, -0.351, -1.087,  0.199,  0.701,  0.096,
    -0.025, -0.868,  1.051,  0.157,  0.216,  0.162,  0.249, -0.007,
     0.009,  0.508, -0.790,  0.723,  0.881, -0.508,  0.393, -0.226,
     0.710,  0.038, -0.217,  0.831,  0.480,  0.407,  0.447, -0.295,
     1.126,  0.380,  0.549, -0.445, -0.046,  0.428, -0.074,  0.217,
    -0.822,  0.491,  1.347, -0.141,  1.230, -0.044,  0.079,  0.219,
     0.698,  0.275,  0.056,  0.031,  0.421,  0.064,  0.721,  0.104,
    -0.729,  0.650, -1.103,  0.154, -1.720,  0.051, -0.385,  0.477,
     1.537, -0.901,  0.939, -0.411,  0.341, -0.411,  0.106,  0.224,
    -0.947, -1.424, -0.542, -1.032
)

fun funnel(da: DoubleArray, rule: Rule): DoubleArray {
    var x = 0.0
    val result = DoubleArray(da.size)
    for ((i, d) in da.withIndex()) {
        result[i] = x + d
        x = rule(x, d)
    }
    return result
}

fun mean(da: DoubleArray) = da.average()

fun stdDev(da: DoubleArray): Double {
    val m = mean(da)
    return Math.sqrt(da.map { (it - m) * (it - m) }.average())
}

fun experiment(label: String, rule: Rule) {
    val rxs = funnel(dxs, rule)
    val rys = funnel(dys, rule)
    println("$label  :      x        y")
    println("Mean    :  ${"%7.4f, %7.4f".format(mean(rxs), mean(rys))}")
    println("Std Dev :  ${"%7.4f, %7.4f".format(stdDev(rxs), stdDev(rys))}")
    println()
}

fun main(args: Array<String>) {
    experiment("Rule 1") { _, _  -> 0.0 }
    experiment("Rule 2") { _, dz -> -dz }
    experiment("Rule 3") { z, dz -> -(z + dz) }
    experiment("Rule 4") { z, dz -> z + dz }
}
```


{{out}}

```txt

Rule 1  :      x        y
Mean    :   0.0004,  0.0702
Std Dev :   0.7153,  0.6462

Rule 2  :      x        y
Mean    :   0.0009, -0.0103
Std Dev :   1.0371,  0.8999

Rule 3  :      x        y
Mean    :   0.0439, -0.0063
Std Dev :   7.9871,  4.7784

Rule 4  :      x        y
Mean    :   3.1341,  5.4210
Std Dev :   1.5874,  3.9304

```



## PARI/GP

:''This is a work-in-progress.''

```parigp
drop(drops, rule, rnd)={
  my(v=vector(drops),target=0);
  v[1]=rule(target, 0);
  for(i=2,drops,
    target=rule(target, v[i-1]);
    v[i]=rnd(n)+target
  );
  v
};
R=[-.533-.136*I,.27-.717*I,.859-.459*I,-.043+.225*I,-.205-1.39*I,-.127-.385*I,-.071-.121*I,.275+.395*I,1.25-.490*I,-.231+.682*I,-.401+.0650*I,.269-.242*I,.491+.288*I,.951-.658*I,1.15-.459*I,.001,-.382-.426*I,.161-.205*I,.915+.765*I,2.08+2.19*I,-2.34+.742*I,.034+.0100*I,-.126-.0890*I,.014-.208*I,.709-.585*I,.129-.633*I,-1.09+.444*I,-.483+.351*I,-1.19+1.09*I,.02-.199*I,-.051-.701*I,.047-.0960*I,-.095+.0250*I,.695+.868*I,.34-1.05*I,-.182-.157*I,.287-.216*I,.213-.162*I,-.423-.249*I,-.021+.00700*I,-0.134-.00900*I,1.8-.508*I,.021+.790*I,-1.1-.723*I,-.361-.881*I,1.64+.508*I,-1.13-.393*I,1.32+.226*I,.201-.710*I,.034-.0380*I,.097+.217*I,-.17-.831*I,.054-.480*I,-.553-.407*I,-.024-.447*I,-.181+.295*I,-.7-1.13*I,-.361-.380*I,-.789-.549*I,.279+.445*I,-.174+.0460*I,-.009-.428*I,-.323+.0740*I,-.658-.217*I,.348+.822*I,-.528-.491*I,.881-1.35*I,.021+.141*I,-.853-1.23*I,.157+.0440*I,.648-.0790*I,1.77-.219*I,-1.04-.698*I,.051-.275*I,.021-.0560*I,.247-.0310*I,-.31-.421*I,.171-.0640*I,-.721*I,.106-.104*I,.024+.729*I,-.386-.650*I,.962+1.10*I,.765-.154*I,-.125+1.72*I,-.289-.0510*I,.521+.385*I,.017-.477*I,.281-1.54*I,-.749+.901*I,-.149-.939*I,-2.44+.411*I,-.909-.341*I,.394+.411*I,-.113-.106*I,-.598-.224*I,.443+.947*I,-.521+1.42*I,-.799+.542*I,.087+1.03*I];
rule1(target, result)=0;
rule2(target, result)=target-result;
rule3(target, result)=-result;
rule4(target, result)=result;
mean(v)=sum(i=1,#v,v[i])/#v;
stdev(v,mu=mean(v))=sqrt(sum(i=1,#v,(v[i]-mu)^2)/#v);
main()={
  my(V);
  V=apply(f->drop(100,f,n->R[n]), [rule1, rule2, rule3, rule4]);
  for(i=1,4,
    print("Method #"i);
    print("Means: ", mean(real(V[i])), "\t", mean(imag(V[i])));
    print("StDev: ", stdev(real(V[i])), "\t", stdev(imag(V[i])));
    print()
  )
}
```



## Perl


```perl
@dx = qw<
    -0.533  0.270  0.859 -0.043 -0.205 -0.127 -0.071  0.275
     1.251 -0.231 -0.401  0.269  0.491  0.951  1.150  0.001
    -0.382  0.161  0.915  2.080 -2.337  0.034 -0.126  0.014
     0.709  0.129 -1.093 -0.483 -1.193  0.020 -0.051  0.047
    -0.095  0.695  0.340 -0.182  0.287  0.213 -0.423 -0.021
    -0.134  1.798  0.021 -1.099 -0.361  1.636 -1.134  1.315
     0.201  0.034  0.097 -0.170  0.054 -0.553 -0.024 -0.181
    -0.700 -0.361 -0.789  0.279 -0.174 -0.009 -0.323 -0.658
     0.348 -0.528  0.881  0.021 -0.853  0.157  0.648  1.774
    -1.043  0.051  0.021  0.247 -0.310  0.171  0.000  0.106
     0.024 -0.386  0.962  0.765 -0.125 -0.289  0.521  0.017
     0.281 -0.749 -0.149 -2.436 -0.909  0.394 -0.113 -0.598
     0.443 -0.521 -0.799  0.087>;

@dy = qw<
     0.136  0.717  0.459 -0.225  1.392  0.385  0.121 -0.395
     0.490 -0.682 -0.065  0.242 -0.288  0.658  0.459  0.000
     0.426  0.205 -0.765 -2.188 -0.742 -0.010  0.089  0.208
     0.585  0.633 -0.444 -0.351 -1.087  0.199  0.701  0.096
    -0.025 -0.868  1.051  0.157  0.216  0.162  0.249 -0.007
     0.009  0.508 -0.790  0.723  0.881 -0.508  0.393 -0.226
     0.710  0.038 -0.217  0.831  0.480  0.407  0.447 -0.295
     1.126  0.380  0.549 -0.445 -0.046  0.428 -0.074  0.217
    -0.822  0.491  1.347 -0.141  1.230 -0.044  0.079  0.219
     0.698  0.275  0.056  0.031  0.421  0.064  0.721  0.104
    -0.729  0.650 -1.103  0.154 -1.720  0.051 -0.385  0.477
     1.537 -0.901  0.939 -0.411  0.341 -0.411  0.106  0.224
    -0.947 -1.424 -0.542 -1.032>;

sub mean   { my $s; $s += $_ for @_; $s / @_ }
sub stddev { sqrt( mean(map { $_**2 } @_) - mean(@_)**2) }

@rules = (
sub { 0 },
sub { -$_[1] },
sub { -$_[0] - $_[1] },
sub {  $_[0] + $_[1] }
);

for (@rules) {
    print "Rule " . ++$cnt . "\n";

    my @ddx; my $tx = 0;
    for my $x (@dx) { push @ddx, $tx + $x; $tx = &$_($tx, $x) }
    my @ddy; my $ty = 0;
    for my $y (@dy) { push @ddy, $ty + $y; $ty = &$_($ty, $y) }

    printf "Mean    x, y   : %7.4f %7.4f\n",   mean(@ddx),   mean(@ddy);
    printf "Std dev x, y   : %7.4f %7.4f\n", stddev(@ddx), stddev(@ddy);
}
```

{{out}}
```txt

Rule 1
Mean    x, y   :  0.0004  0.0702
Std dev x, y   :  0.7153  0.6462
Rule 2
Mean    x, y   :  0.0009 -0.0103
Std dev x, y   :  1.0371  0.8999
Rule 3
Mean    x, y   :  0.0439 -0.0063
Std dev x, y   :  7.9871  4.7784
Rule 4
Mean    x, y   :  3.1341  5.4210
Std dev x, y   :  1.5874  3.9304
```



## Perl 6

{{Works with|Rakudo|2018.10}}

```perl6
sub mean { @_ R/ [+] @_ }
sub stddev {
    # <(x - <x>)²> = <x²> - <x>²
    sqrt( mean(@_ »**» 2) - mean(@_)**2 )
}

constant @dz = <
    -0.533  0.270  0.859 -0.043 -0.205 -0.127 -0.071  0.275
     1.251 -0.231 -0.401  0.269  0.491  0.951  1.150  0.001
    -0.382  0.161  0.915  2.080 -2.337  0.034 -0.126  0.014
     0.709  0.129 -1.093 -0.483 -1.193  0.020 -0.051  0.047
    -0.095  0.695  0.340 -0.182  0.287  0.213 -0.423 -0.021
    -0.134  1.798  0.021 -1.099 -0.361  1.636 -1.134  1.315
     0.201  0.034  0.097 -0.170  0.054 -0.553 -0.024 -0.181
    -0.700 -0.361 -0.789  0.279 -0.174 -0.009 -0.323 -0.658
     0.348 -0.528  0.881  0.021 -0.853  0.157  0.648  1.774
    -1.043  0.051  0.021  0.247 -0.310  0.171  0.000  0.106
     0.024 -0.386  0.962  0.765 -0.125 -0.289  0.521  0.017
     0.281 -0.749 -0.149 -2.436 -0.909  0.394 -0.113 -0.598
     0.443 -0.521 -0.799  0.087
> Z+ (1i X* <
     0.136  0.717  0.459 -0.225  1.392  0.385  0.121 -0.395
     0.490 -0.682 -0.065  0.242 -0.288  0.658  0.459  0.000
     0.426  0.205 -0.765 -2.188 -0.742 -0.010  0.089  0.208
     0.585  0.633 -0.444 -0.351 -1.087  0.199  0.701  0.096
    -0.025 -0.868  1.051  0.157  0.216  0.162  0.249 -0.007
     0.009  0.508 -0.790  0.723  0.881 -0.508  0.393 -0.226
     0.710  0.038 -0.217  0.831  0.480  0.407  0.447 -0.295
     1.126  0.380  0.549 -0.445 -0.046  0.428 -0.074  0.217
    -0.822  0.491  1.347 -0.141  1.230 -0.044  0.079  0.219
     0.698  0.275  0.056  0.031  0.421  0.064  0.721  0.104
    -0.729  0.650 -1.103  0.154 -1.720  0.051 -0.385  0.477
     1.537 -0.901  0.939 -0.411  0.341 -0.411  0.106  0.224
    -0.947 -1.424 -0.542 -1.032
>);

constant @rule = 
-> \z, \dz { 0 },
-> \z, \dz { -dz },
-> \z, \dz { -z - dz },
-> \z, \dz {  z + dz },
;

for @rule {
    say "Rule $(++$):";
    my $target = 0i;
    my @z = gather for @dz -> $dz {
	take $target + $dz;
	$target = .($target, $dz)
    }
    printf "Mean    x, y   : %7.4f %7.4f\n",   mean(@z».re),   mean(@z».im);
    printf "Std dev x, y   : %7.4f %7.4f\n", stddev(@z».re), stddev(@z».im);
}
```

{{out}}

```txt
Rule 1:
Mean    x, y   :  0.0004  0.0702
Std dev x, y   :  0.7153  0.6462
Rule 2:
Mean    x, y   :  0.0009 -0.0103
Std dev x, y   :  1.0371  0.8999
Rule 3:
Mean    x, y   :  0.0439 -0.0063
Std dev x, y   :  7.9871  4.7784
Rule 4:
Mean    x, y   :  3.1341  5.4210
Std dev x, y   :  1.5874  3.9304
```



## Phix


```Phix
function funnel(sequence dxs, integer rule)
    atom x:=0.0
    sequence rxs = {}
    for i=1 to length(dxs) do
        atom dx = dxs[i]
        rxs = append(rxs,x + dx)
        switch rule
            case 2: x = -dx
            case 3: x = -(x+dx)
            case 4: x = x+dx
        end switch
    end for
    return rxs
end function

function mean(sequence xs)
    return sum(xs)/length(xs)
end function

function stddev(sequence xs)
    atom m = mean(xs)
    return sqrt(sum(sq_power(sq_sub(xs,m),2))/length(xs))
end function
 
procedure experiment(integer n, sequence dxs, dys)
    sequence rxs = funnel(dxs,n),
             rys = funnel(dys,n)
   printf(1,"Mean x, y    : %7.4f, %7.4f\n",{mean(rxs), mean(rys)})
   printf(1,"Std dev x, y : %7.4f, %7.4f\n",{stddev(rxs), stddev(rys)})
end procedure

constant dxs = {-0.533,  0.270,  0.859, -0.043, -0.205, -0.127, -0.071,  0.275,
                 1.251, -0.231, -0.401,  0.269,  0.491,  0.951,  1.150,  0.001,
                -0.382,  0.161,  0.915,  2.080, -2.337,  0.034, -0.126,  0.014,
                 0.709,  0.129, -1.093, -0.483, -1.193,  0.020, -0.051,  0.047,
                -0.095,  0.695,  0.340, -0.182,  0.287,  0.213, -0.423, -0.021,
                -0.134,  1.798,  0.021, -1.099, -0.361,  1.636, -1.134,  1.315,
                 0.201,  0.034,  0.097, -0.170,  0.054, -0.553, -0.024, -0.181,
                -0.700, -0.361, -0.789,  0.279, -0.174, -0.009, -0.323, -0.658,
                 0.348, -0.528,  0.881,  0.021, -0.853,  0.157,  0.648,  1.774,
                -1.043,  0.051,  0.021,  0.247, -0.310,  0.171,  0.000,  0.106,
                 0.024, -0.386,  0.962,  0.765, -0.125, -0.289,  0.521,  0.017,
                 0.281, -0.749, -0.149, -2.436, -0.909,  0.394, -0.113, -0.598,
                 0.443, -0.521, -0.799,  0.087}
 
constant dys = { 0.136,  0.717,  0.459, -0.225,  1.392,  0.385,  0.121, -0.395,
                 0.490, -0.682, -0.065,  0.242, -0.288,  0.658,  0.459,  0.000,
                 0.426,  0.205, -0.765, -2.188, -0.742, -0.010,  0.089,  0.208,
                 0.585,  0.633, -0.444, -0.351, -1.087,  0.199,  0.701,  0.096,
                -0.025, -0.868,  1.051,  0.157,  0.216,  0.162,  0.249, -0.007,
                 0.009,  0.508, -0.790,  0.723,  0.881, -0.508,  0.393, -0.226,
                 0.710,  0.038, -0.217,  0.831,  0.480,  0.407,  0.447, -0.295,
                 1.126,  0.380,  0.549, -0.445, -0.046,  0.428, -0.074,  0.217,
                -0.822,  0.491,  1.347, -0.141,  1.230, -0.044,  0.079,  0.219,
                 0.698,  0.275,  0.056,  0.031,  0.421,  0.064,  0.721,  0.104,
                -0.729,  0.650, -1.103,  0.154, -1.720,  0.051, -0.385,  0.477,
                 1.537, -0.901,  0.939, -0.411,  0.341, -0.411,  0.106,  0.224,
                -0.947, -1.424, -0.542, -1.032}
 
for i=1 to 4 do
    experiment(i, dxs, dys)
end for
```

{{out}}

```txt

Mean x, y    :  0.0004,  0.0702
Std dev x, y :  0.7153,  0.6462
Mean x, y    :  0.0009, -0.0103
Std dev x, y :  1.0371,  0.8999
Mean x, y    :  0.0439, -0.0063
Std dev x, y :  7.9871,  4.7784
Mean x, y    :  3.1341,  5.4210
Std dev x, y :  1.5874,  3.9304

```



## Python

{{trans|Racket}}

```python
import math 

dxs = [-0.533, 0.27, 0.859, -0.043, -0.205, -0.127, -0.071, 0.275, 1.251,
       -0.231, -0.401, 0.269, 0.491, 0.951, 1.15, 0.001, -0.382, 0.161, 0.915,
       2.08, -2.337, 0.034, -0.126, 0.014, 0.709, 0.129, -1.093, -0.483, -1.193, 
       0.02, -0.051, 0.047, -0.095, 0.695, 0.34, -0.182, 0.287, 0.213, -0.423,
       -0.021, -0.134, 1.798, 0.021, -1.099, -0.361, 1.636, -1.134, 1.315, 0.201, 
       0.034, 0.097, -0.17, 0.054, -0.553, -0.024, -0.181, -0.7, -0.361, -0.789,
       0.279, -0.174, -0.009, -0.323, -0.658, 0.348, -0.528, 0.881, 0.021, -0.853,
       0.157, 0.648, 1.774, -1.043, 0.051, 0.021, 0.247, -0.31, 0.171, 0.0, 0.106,
       0.024, -0.386, 0.962, 0.765, -0.125, -0.289, 0.521, 0.017, 0.281, -0.749,
       -0.149, -2.436, -0.909, 0.394, -0.113, -0.598, 0.443, -0.521, -0.799, 
       0.087]

dys = [0.136, 0.717, 0.459, -0.225, 1.392, 0.385, 0.121, -0.395, 0.49, -0.682,
       -0.065, 0.242, -0.288, 0.658, 0.459, 0.0, 0.426, 0.205, -0.765, -2.188, 
       -0.742, -0.01, 0.089, 0.208, 0.585, 0.633, -0.444, -0.351, -1.087, 0.199,
       0.701, 0.096, -0.025, -0.868, 1.051, 0.157, 0.216, 0.162, 0.249, -0.007, 
       0.009, 0.508, -0.79, 0.723, 0.881, -0.508, 0.393, -0.226, 0.71, 0.038, 
       -0.217, 0.831, 0.48, 0.407, 0.447, -0.295, 1.126, 0.38, 0.549, -0.445, 
       -0.046, 0.428, -0.074, 0.217, -0.822, 0.491, 1.347, -0.141, 1.23, -0.044, 
       0.079, 0.219, 0.698, 0.275, 0.056, 0.031, 0.421, 0.064, 0.721, 0.104, 
       -0.729, 0.65, -1.103, 0.154, -1.72, 0.051, -0.385, 0.477, 1.537, -0.901, 
       0.939, -0.411, 0.341, -0.411, 0.106, 0.224, -0.947, -1.424, -0.542, -1.032]

def funnel(dxs, rule):
    x, rxs = 0, []
    for dx in dxs:
        rxs.append(x + dx)
        x = rule(x, dx)
    return rxs

def mean(xs): return sum(xs) / len(xs)

def stddev(xs):
    m = mean(xs)
    return math.sqrt(sum((x-m)**2 for x in xs) / len(xs))

def experiment(label, rule):
    rxs, rys = funnel(dxs, rule), funnel(dys, rule)
    print label
    print 'Mean x, y    : %.4f, %.4f' % (mean(rxs), mean(rys))
    print 'Std dev x, y : %.4f, %.4f' % (stddev(rxs), stddev(rys))
    print


experiment('Rule 1:', lambda z, dz: 0)
experiment('Rule 2:', lambda z, dz: -dz)
experiment('Rule 3:', lambda z, dz: -(z+dz))
experiment('Rule 4:', lambda z, dz: z+dz)
```


{{output}}

```txt
Rule 1:
Mean x, y    : 0.0004, 0.0702
Std dev x, y : 0.7153, 0.6462

Rule 2:
Mean x, y    : 0.0009, -0.0103
Std dev x, y : 1.0371, 0.8999

Rule 3:
Mean x, y    : 0.0439, -0.0063
Std dev x, y : 7.9871, 4.7784

Rule 4:
Mean x, y    : 3.1341, 5.4210
Std dev x, y : 1.5874, 3.9304

```


'''Alternative''': [Generates pseudo-random data and gives some interpretation.]  The funnel experiment is performed in one dimension. The other dimension would act similarly.

```python
from random import gauss
from math import sqrt
from pprint import pprint as pp

NMAX=50

def statscreator():
    sum_ = sum2 = n = 0
    def stats(x):
        nonlocal sum_, sum2, n

        sum_ += x
        sum2 += x*x
        n    += 1.0
        return sum_/n, sqrt(sum2/n - sum_*sum_/n/n)
    return stats

def drop(target, sigma=1.0):
    'Drop ball at target'
    return gauss(target, sigma)

def deming(rule, nmax=NMAX):
    ''' Simulate Demings funnel in 1D. '''
    
    stats = statscreator()
    target = 0
    for i in range(nmax):
        value = drop(target)
        mean, sdev = stats(value)
        target = rule(target, value)
        if i == nmax - 1:
            return mean, sdev

def d1(target, value):
    ''' Keep Funnel over target. '''

    return target


def d2(target, value):
    ''' The new target starts at the center, 0,0 then is adjusted to
    be the previous target _minus_ the offset of the new drop from the
    previous target. '''
    
    return -value   # - (target - (target - value)) = - value

def d3(target, value):
    ''' The new target starts at the center, 0,0 then is adjusted to
    be the previous target _minus_ the offset of the new drop from the
    center, 0.0. '''
    
    return target - value

def d4(target, value):
    ''' (Dumb). The new target is where it last dropped. '''
    
    return value


def printit(rule, trials=5):
    print('\nDeming simulation. %i trials using rule %s:\n %s'
          % (trials, rule.__name__.upper(), rule.__doc__))
    for i in range(trials):
        print('    Mean: %7.3f, Sdev: %7.3f' % deming(rule))


if __name__ == '__main__':
    rcomments = [ (d1, 'Should have smallest deviations ~1.0, and be centered on 0.0'),
                  (d2, 'Should be centred on 0.0 with larger deviations than D1'),
                  (d3, 'Should be centred on 0.0 with larger deviations than D1'),
                  (d4, 'Center wanders all over the place, with deviations to match!'),
                ]
    for rule, comment in rcomments:
        printit(rule)
        print('  %s\n' % comment)
```


{{out}}

```txt
Deming simulation. 5 trials using rule D1:
  Keep Funnel over target. 
    Mean:  -0.161, Sdev:   0.942
    Mean:  -0.092, Sdev:   0.924
    Mean:  -0.199, Sdev:   1.079
    Mean:  -0.256, Sdev:   0.820
    Mean:  -0.211, Sdev:   0.971
  Should have smallest deviations ~1.0, and be centered on 0.0


Deming simulation. 5 trials using rule D2:
  The new target starts at the center, 0,0 then is adjusted to
    be the previous target _minus_ the offset of the new drop from the
    previous target. 
    Mean:  -0.067, Sdev:   4.930
    Mean:   0.035, Sdev:   4.859
    Mean:  -0.080, Sdev:   2.575
    Mean:   0.147, Sdev:   4.948
    Mean:   0.050, Sdev:   4.149
  Should be centred on 0.0 with larger deviations than D1


Deming simulation. 5 trials using rule D3:
  The new target starts at the center, 0,0 then is adjusted to
    be the previous target _minus_ the offset of the new drop from the
    center, 0.0. 
    Mean:   0.006, Sdev:   1.425
    Mean:  -0.039, Sdev:   1.436
    Mean:   0.030, Sdev:   1.305
    Mean:   0.009, Sdev:   1.419
    Mean:   0.001, Sdev:   1.479
  Should be centred on 0.0 with larger deviations than D1


Deming simulation. 5 trials using rule D4:
  (Dumb). The new target is where it last dropped. 
    Mean:   5.252, Sdev:   2.839
    Mean:   1.403, Sdev:   3.073
    Mean:  -1.525, Sdev:   3.650
    Mean:   3.844, Sdev:   2.715
    Mean:  -7.697, Sdev:   3.715
  Center wanders all over the place, with deviations to match!
```



## Racket

The stretch solutions can be obtained by uncommenting radii etc. (delete the 4 semi-colons) to generate fresh data, and scatter-plots can be obtained by deleting the #; .

```racket
#lang racket
(require math/distributions math/statistics plot)

(define dxs '(-0.533 0.270 0.859 -0.043 -0.205 -0.127 -0.071 0.275 1.251 -0.231 
              -0.401 0.269 0.491 0.951 1.150 0.001 -0.382 0.161 0.915 2.080 -2.337 
              0.034 -0.126 0.014 0.709 0.129 -1.093 -0.483 -1.193 0.020 -0.051
              0.047 -0.095 0.695 0.340 -0.182 0.287 0.213 -0.423 -0.021 -0.134 1.798
              0.021 -1.099 -0.361 1.636 -1.134 1.315 0.201 0.034 0.097 -0.170 0.054 
              -0.553 -0.024 -0.181 -0.700 -0.361 -0.789 0.279 -0.174 -0.009 -0.323
              -0.658 0.348 -0.528 0.881 0.021 -0.853 0.157 0.648 1.774 -1.043 0.051 
              0.021 0.247 -0.310 0.171 0.000 0.106 0.024 -0.386 0.962 0.765 -0.125 
              -0.289 0.521 0.017 0.281 -0.749 -0.149 -2.436 -0.909 0.394 -0.113 -0.598
              0.443 -0.521 -0.799 0.087))

(define dys '(0.136 0.717 0.459 -0.225 1.392 0.385 0.121 -0.395 0.490 -0.682 -0.065 
              0.242 -0.288 0.658 0.459 0.000 0.426 0.205 -0.765 -2.188 -0.742 -0.010 
              0.089 0.208 0.585 0.633 -0.444 -0.351 -1.087 0.199 0.701 0.096 -0.025 
              -0.868 1.051 0.157 0.216 0.162 0.249 -0.007 0.009 0.508 -0.790 0.723
              0.881 -0.508 0.393 -0.226 0.710 0.038 -0.217 0.831 0.480 0.407 0.447
              -0.295 1.126 0.380 0.549 -0.445 -0.046 0.428 -0.074 0.217 -0.822 0.491 
              1.347 -0.141 1.230 -0.044 0.079 0.219 0.698 0.275 0.056 0.031 0.421 0.064
              0.721 0.104 -0.729 0.650 -1.103 0.154 -1.720 0.051 -0.385 0.477 1.537 
              -0.901 0.939 -0.411 0.341 -0.411 0.106 0.224 -0.947 -1.424 -0.542 -1.032))

;(define radii (map abs (sample (normal-dist 0 1) 100)))
;(define angles (sample (uniform-dist (- pi) pi) 100))
;(define dxs (map (λ (r theta) (* r (cos theta))) radii angles))
;(define dys (map (λ (r theta) (* r (sin theta))) radii angles))

(define (funnel dxs rule)
  (let ([x 0])
    (for/fold ([rxs null])
      ([dx dxs])
      (let ([rx (+ x dx)])
        (set! x (rule x dx))
        (cons rx rxs)))))

(define (experiment label rule)
  (define (p s) (real->decimal-string s 4))
  (let ([rxs (funnel dxs rule)]
        [rys (funnel dys rule)])
    (displayln label)
    (printf "Mean x, y   : ~a, ~a\n" (p (mean rxs)) (p (mean rys)))
    (printf "Std dev x, y: ~a, ~a\n\n" (p (stddev rxs)) (p (stddev rys)))
    #;(plot (points (map vector rxs rys)
          #:x-min -15 #:x-max 15 #:y-min -15 #:y-max 15))))

(experiment "Rule 1:" (λ (z dz) 0))
(experiment "Rule 2:" (λ (z dz) (- dz)))
(experiment "Rule 3:" (λ (z dz) (- (+ z dz))))
(experiment "Rule 4:" (λ (z dz) (+ z dz))) 
```


{{output}}

```txt

Rule 1:
Mean x, y   : 0.0004, 0.0702
Std dev x, y: 0.7153, 0.6462

Rule 2:
Mean x, y   : 0.0009, -0.0103
Std dev x, y: 1.0371, 0.8999

Rule 3:
Mean x, y   : 0.0439, -0.0063
Std dev x, y: 7.9871, 4.7784

Rule 4:
Mean x, y   : 3.1341, 5.4210
Std dev x, y: 1.5874, 3.9304

```



## Ruby

{{trans|Python}}

```ruby
def funnel(dxs, &rule)
  x, rxs = 0, []
  for dx in dxs
    rxs << (x + dx)
    x = rule[x, dx]
  end
  rxs
end

def mean(xs) xs.inject(:+) / xs.size end

def stddev(xs)
  m = mean(xs)
  Math.sqrt(xs.inject(0.0){|sum,x| sum + (x-m)**2} / xs.size)
end

def experiment(label, dxs, dys, &rule)
  rxs, rys = funnel(dxs, &rule), funnel(dys, &rule)
  puts label
  puts 'Mean x, y    : %7.4f, %7.4f' % [mean(rxs), mean(rys)]
  puts 'Std dev x, y : %7.4f, %7.4f' % [stddev(rxs), stddev(rys)]
  puts
end

dxs = [ -0.533,  0.270,  0.859, -0.043, -0.205, -0.127, -0.071,  0.275,
         1.251, -0.231, -0.401,  0.269,  0.491,  0.951,  1.150,  0.001,
        -0.382,  0.161,  0.915,  2.080, -2.337,  0.034, -0.126,  0.014,
         0.709,  0.129, -1.093, -0.483, -1.193,  0.020, -0.051,  0.047,
        -0.095,  0.695,  0.340, -0.182,  0.287,  0.213, -0.423, -0.021,
        -0.134,  1.798,  0.021, -1.099, -0.361,  1.636, -1.134,  1.315,
         0.201,  0.034,  0.097, -0.170,  0.054, -0.553, -0.024, -0.181,
        -0.700, -0.361, -0.789,  0.279, -0.174, -0.009, -0.323, -0.658,
         0.348, -0.528,  0.881,  0.021, -0.853,  0.157,  0.648,  1.774,
        -1.043,  0.051,  0.021,  0.247, -0.310,  0.171,  0.000,  0.106,
         0.024, -0.386,  0.962,  0.765, -0.125, -0.289,  0.521,  0.017,
         0.281, -0.749, -0.149, -2.436, -0.909,  0.394, -0.113, -0.598,
         0.443, -0.521, -0.799,  0.087]

dys = [  0.136,  0.717,  0.459, -0.225,  1.392,  0.385,  0.121, -0.395,
         0.490, -0.682, -0.065,  0.242, -0.288,  0.658,  0.459,  0.000,
         0.426,  0.205, -0.765, -2.188, -0.742, -0.010,  0.089,  0.208,
         0.585,  0.633, -0.444, -0.351, -1.087,  0.199,  0.701,  0.096,
        -0.025, -0.868,  1.051,  0.157,  0.216,  0.162,  0.249, -0.007,
         0.009,  0.508, -0.790,  0.723,  0.881, -0.508,  0.393, -0.226,
         0.710,  0.038, -0.217,  0.831,  0.480,  0.407,  0.447, -0.295,
         1.126,  0.380,  0.549, -0.445, -0.046,  0.428, -0.074,  0.217,
        -0.822,  0.491,  1.347, -0.141,  1.230, -0.044,  0.079,  0.219,
         0.698,  0.275,  0.056,  0.031,  0.421,  0.064,  0.721,  0.104,
        -0.729,  0.650, -1.103,  0.154, -1.720,  0.051, -0.385,  0.477,
         1.537, -0.901,  0.939, -0.411,  0.341, -0.411,  0.106,  0.224,
        -0.947, -1.424, -0.542, -1.032]

experiment('Rule 1:', dxs, dys) {|z, dz| 0}
experiment('Rule 2:', dxs, dys) {|z, dz| -dz}
experiment('Rule 3:', dxs, dys) {|z, dz| -(z+dz)}
experiment('Rule 4:', dxs, dys) {|z, dz| z+dz}
```


{{out}}

```txt

Rule 1:
Mean x, y    :  0.0004,  0.0702
Std dev x, y :  0.7153,  0.6462

Rule 2:
Mean x, y    :  0.0009, -0.0103
Std dev x, y :  1.0371,  0.8999

Rule 3:
Mean x, y    :  0.0439, -0.0063
Std dev x, y :  7.9871,  4.7784

Rule 4:
Mean x, y    :  3.1341,  5.4210
Std dev x, y :  1.5874,  3.9304

```



## Sidef

{{trans|Perl 6}}

```ruby
func x̄(a) {
    a.sum / a.len
}

func σ(a) {
    sqrt(x̄(a.map{.**2}) - x̄(a)**2)
}

const Δ = (%n<
    -0.533  0.270  0.859 -0.043 -0.205 -0.127 -0.071  0.275
     1.251 -0.231 -0.401  0.269  0.491  0.951  1.150  0.001
    -0.382  0.161  0.915  2.080 -2.337  0.034 -0.126  0.014
     0.709  0.129 -1.093 -0.483 -1.193  0.020 -0.051  0.047
    -0.095  0.695  0.340 -0.182  0.287  0.213 -0.423 -0.021
    -0.134  1.798  0.021 -1.099 -0.361  1.636 -1.134  1.315
     0.201  0.034  0.097 -0.170  0.054 -0.553 -0.024 -0.181
    -0.700 -0.361 -0.789  0.279 -0.174 -0.009 -0.323 -0.658
     0.348 -0.528  0.881  0.021 -0.853  0.157  0.648  1.774
    -1.043  0.051  0.021  0.247 -0.310  0.171  0.000  0.106
     0.024 -0.386  0.962  0.765 -0.125 -0.289  0.521  0.017
     0.281 -0.749 -0.149 -2.436 -0.909  0.394 -0.113 -0.598
     0.443 -0.521 -0.799  0.087
> ~Z+ %n<
     0.136  0.717  0.459 -0.225  1.392  0.385  0.121 -0.395
     0.490 -0.682 -0.065  0.242 -0.288  0.658  0.459  0.000
     0.426  0.205 -0.765 -2.188 -0.742 -0.010  0.089  0.208
     0.585  0.633 -0.444 -0.351 -1.087  0.199  0.701  0.096
    -0.025 -0.868  1.051  0.157  0.216  0.162  0.249 -0.007
     0.009  0.508 -0.790  0.723  0.881 -0.508  0.393 -0.226
     0.710  0.038 -0.217  0.831  0.480  0.407  0.447 -0.295
     1.126  0.380  0.549 -0.445 -0.046  0.428 -0.074  0.217
    -0.822  0.491  1.347 -0.141  1.230 -0.044  0.079  0.219
     0.698  0.275  0.056  0.031  0.421  0.064  0.721  0.104
    -0.729  0.650 -1.103  0.154 -1.720  0.051 -0.385  0.477
     1.537 -0.901  0.939 -0.411  0.341 -0.411  0.106  0.224
    -0.947 -1.424 -0.542 -1.032
>.map{ .i })

const rules = [
    { 0 },
    {|_,dz| -dz },
    {|z,dz| -z - dz },
    {|z,dz| z + dz },
]

for i,v in (rules.kv) {
    say "Rule #{i+1}:"
    var target = 0
    var z = gather {
        Δ.each { |d|
            take(target + d)
            target = v.run(target, d)
        }
    }
    printf("Mean    x, y   : %.4f %.4f\n", x̄(z.map{.re}), x̄(z.map{.im}))
    printf("Std dev x, y   : %.4f %.4f\n", σ(z.map{.re}), σ(z.map{.im}))
}
```

{{out}}

```txt

Rule 1:
Mean    x, y   : 0.0004 0.0702
Std dev x, y   : 0.7153 0.6462
Rule 2:
Mean    x, y   : 0.0009 -0.0103
Std dev x, y   : 1.0371 0.8999
Rule 3:
Mean    x, y   : 0.0439 -0.0063
Std dev x, y   : 7.9871 4.7784
Rule 4:
Mean    x, y   : 3.1341 5.4210
Std dev x, y   : 1.5874 3.9304

```



## Tcl

{{works with|Tcl|8.6}}
{{trans|Ruby}}

```tcl
package require Tcl 8.6
namespace path {tcl::mathop tcl::mathfunc}

proc funnel {items rule} {
    set x 0.0
    set result {}
    foreach item $items {
	lappend result [+ $x $item]
	set x [apply $rule $x $item]
    }
    return $result
}

proc mean {items} {
    / [+ {*}$items] [double [llength $items]]
}
proc stddev {items} {
    set m [mean $items]
    sqrt [mean [lmap x $items {** [- $x $m] 2}]]
}

proc experiment {label dxs dys rule} {
    set rxs [funnel $dxs $rule]
    set rys [funnel $dys $rule]
    puts $label
    puts [format "Mean x, y    : %7.4f, %7.4f" [mean $rxs] [mean $rys]]
    puts [format "Std dev x, y : %7.4f, %7.4f" [stddev $rxs] [stddev $rys]]
    puts ""
}

set dxs {
    -0.533 0.270 0.859 -0.043 -0.205 -0.127 -0.071 0.275 1.251 -0.231 -0.401
    0.269 0.491 0.951 1.150 0.001 -0.382 0.161 0.915 2.080 -2.337 0.034
    -0.126 0.014 0.709 0.129 -1.093 -0.483 -1.193 0.020 -0.051 0.047 -0.095
    0.695 0.340 -0.182 0.287 0.213 -0.423 -0.021 -0.134 1.798 0.021 -1.099
    -0.361 1.636 -1.134 1.315 0.201 0.034 0.097 -0.170 0.054 -0.553 -0.024
    -0.181 -0.700 -0.361 -0.789 0.279 -0.174 -0.009 -0.323 -0.658 0.348
    -0.528 0.881 0.021 -0.853 0.157 0.648 1.774 -1.043 0.051 0.021 0.247
    -0.310 0.171 0.000 0.106 0.024 -0.386 0.962 0.765 -0.125 -0.289 0.521
    0.017 0.281 -0.749 -0.149 -2.436 -0.909 0.394 -0.113 -0.598 0.443 -0.521
    -0.799 0.087
}
set dys {
    0.136 0.717 0.459 -0.225 1.392 0.385 0.121 -0.395 0.490 -0.682 -0.065
    0.242 -0.288 0.658 0.459 0.000 0.426 0.205 -0.765 -2.188 -0.742 -0.010
    0.089 0.208 0.585 0.633 -0.444 -0.351 -1.087 0.199 0.701 0.096 -0.025
    -0.868 1.051 0.157 0.216 0.162 0.249 -0.007 0.009 0.508 -0.790 0.723
    0.881 -0.508 0.393 -0.226 0.710 0.038 -0.217 0.831 0.480 0.407 0.447
    -0.295 1.126 0.380 0.549 -0.445 -0.046 0.428 -0.074 0.217 -0.822 0.491
    1.347 -0.141 1.230 -0.044 0.079 0.219 0.698 0.275 0.056 0.031 0.421 0.064
    0.721 0.104 -0.729 0.650 -1.103 0.154 -1.720 0.051 -0.385 0.477 1.537
    -0.901 0.939 -0.411 0.341 -0.411 0.106 0.224 -0.947 -1.424 -0.542 -1.032
}

puts "USING STANDARD DATA"
experiment "Rule 1:" $dxs $dys {{z dz} {expr {0}}}
experiment "Rule 2:" $dxs $dys {{z dz} {expr {-$dz}}}
experiment "Rule 3:" $dxs $dys {{z dz} {expr {-($z+$dz)}}}
experiment "Rule 4:" $dxs $dys {{z dz} {expr {$z+$dz}}}
```

The first stretch goal:
{{tcllib|math::constants}}
{{tcllib|simulation::random}}

```tcl
package require math::constants
package require simulation::random

math::constants::constants degtorad

set rng(radius) [simulation::random::prng_Normal 0.0 1.0]
set rng(angle) [simulation::random::prng_Uniform 0.0 360.0]
set dxs [set dys {}]
for {set i 0} {$i < 500} {incr i} {
    set r [$rng(radius)]
    set theta [expr {[$rng(angle)] * $degtorad}]
    lappend dxs [expr {$r * cos($theta)}]
    lappend dys [expr {$r * sin($theta)}]
}

puts "USING RANDOM DATA"
experiment "Rule 1:" $dxs $dys {{z dz} {expr {0}}}
experiment "Rule 2:" $dxs $dys {{z dz} {expr {-$dz}}}
experiment "Rule 3:" $dxs $dys {{z dz} {expr {-($z+$dz)}}}
experiment "Rule 4:" $dxs $dys {{z dz} {expr {$z+$dz}}}
```

{{out}}

```txt

USING STANDARD DATA
Rule 1:
Mean x, y    :  0.0004,  0.0702
Std dev x, y :  0.7153,  0.6462

Rule 2:
Mean x, y    :  0.0009, -0.0103
Std dev x, y :  1.0371,  0.8999

Rule 3:
Mean x, y    :  0.0439, -0.0063
Std dev x, y :  7.9871,  4.7784

Rule 4:
Mean x, y    :  3.1341,  5.4210
Std dev x, y :  1.5874,  3.9304

USING RANDOM DATA
Rule 1:
Mean x, y    :  0.0053,  0.0112
Std dev x, y :  0.4954,  0.5082

Rule 2:
Mean x, y    : -0.0012, -0.0002
Std dev x, y :  0.6914,  0.7331

Rule 3:
Mean x, y    : -0.0132,  0.0098
Std dev x, y :  9.3480,  5.0290

Rule 4:
Mean x, y    : -6.3314, -4.0168
Std dev x, y :  3.2387,  4.4825


```



## zkl

{{trans|Ruby}}

```zkl
fcn funnel(dxs, rule){
   x:=0.0; rxs:=L();
   foreach dx in (dxs){
      rxs.append(x + dx);
      x = rule(x,dx);
   }
   rxs
}
 
fcn mean(xs){ xs.sum(0.0)/xs.len() }
 
fcn stddev(xs){
   m:=mean(xs);
   (xs.reduce('wrap(sum,x){ sum + (x-m)*(x-m) },0.0)/xs.len()).sqrt();
}
 
fcn experiment(label,dxs,dys,rule){
   rxs:=funnel(dxs,rule); rys:=funnel(dys,rule);
   label.println();
   "Mean x, y    : %7.4f, %7.4f".fmt(mean(rxs),  mean(rys))  .println();
   "Std dev x, y : %7.4f, %7.4f".fmt(stddev(rxs),stddev(rys)).println();
   println();
}
```


```zkl
dxs:=T( -0.533,  0.270,  0.859, -0.043, -0.205, -0.127, -0.071,  0.275,
         1.251, -0.231, -0.401,  0.269,  0.491,  0.951,  1.150,  0.001,
        -0.382,  0.161,  0.915,  2.080, -2.337,  0.034, -0.126,  0.014,
         0.709,  0.129, -1.093, -0.483, -1.193,  0.020, -0.051,  0.047,
        -0.095,  0.695,  0.340, -0.182,  0.287,  0.213, -0.423, -0.021,
        -0.134,  1.798,  0.021, -1.099, -0.361,  1.636, -1.134,  1.315,
         0.201,  0.034,  0.097, -0.170,  0.054, -0.553, -0.024, -0.181,
        -0.700, -0.361, -0.789,  0.279, -0.174, -0.009, -0.323, -0.658,
         0.348, -0.528,  0.881,  0.021, -0.853,  0.157,  0.648,  1.774,
        -1.043,  0.051,  0.021,  0.247, -0.310,  0.171,  0.000,  0.106,
         0.024, -0.386,  0.962,  0.765, -0.125, -0.289,  0.521,  0.017,
         0.281, -0.749, -0.149, -2.436, -0.909,  0.394, -0.113, -0.598,
         0.443, -0.521, -0.799,  0.087);
 
dys:=T(  0.136,  0.717,  0.459, -0.225,  1.392,  0.385,  0.121, -0.395,
         0.490, -0.682, -0.065,  0.242, -0.288,  0.658,  0.459,  0.000,
         0.426,  0.205, -0.765, -2.188, -0.742, -0.010,  0.089,  0.208,
         0.585,  0.633, -0.444, -0.351, -1.087,  0.199,  0.701,  0.096,
        -0.025, -0.868,  1.051,  0.157,  0.216,  0.162,  0.249, -0.007,
         0.009,  0.508, -0.790,  0.723,  0.881, -0.508,  0.393, -0.226,
         0.710,  0.038, -0.217,  0.831,  0.480,  0.407,  0.447, -0.295,
         1.126,  0.380,  0.549, -0.445, -0.046,  0.428, -0.074,  0.217,
        -0.822,  0.491,  1.347, -0.141,  1.230, -0.044,  0.079,  0.219,
         0.698,  0.275,  0.056,  0.031,  0.421,  0.064,  0.721,  0.104,
        -0.729,  0.650, -1.103,  0.154, -1.720,  0.051, -0.385,  0.477,
         1.537, -0.901,  0.939, -0.411,  0.341, -0.411,  0.106,  0.224,
        -0.947, -1.424, -0.542, -1.032);
 
experiment("Rule 1:", dxs, dys, fcn(z,dz){ 0.0     });
experiment("Rule 2:", dxs, dys, fcn(z,dz){ -dz     });
experiment("Rule 3:", dxs, dys, fcn(z,dz){ -(z+dz) });
experiment("Rule 4:", dxs, dys, fcn(z,dz){ z+dz    });
```

{{out}}

```txt

Rule 1:
Mean x, y    :  0.0004,  0.0702
Std dev x, y :  0.7153,  0.6462

Rule 2:
Mean x, y    :  0.0009, -0.0103
Std dev x, y :  1.0371,  0.8999

Rule 3:
Mean x, y    :  0.0439, -0.0063
Std dev x, y :  7.9871,  4.7784

Rule 4:
Mean x, y    :  3.1341,  5.4210
Std dev x, y :  1.5874,  3.9304

```

