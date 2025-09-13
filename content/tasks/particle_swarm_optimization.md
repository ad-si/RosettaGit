+++
title = "Particle Swarm Optimization"
description = ""
date = 2019-10-06T17:04:01Z
aliases = []
[extra]
id = 19448
[taxonomies]
categories = ["task"]
tags = []
+++

<p>
  Particle Swarm Optimization (PSO) is an optimization method in which multiple candidate solutions ('particles') migrate through the solution space under the influence of local and global best known positions. PSO does not require that the objective function be differentiable and can optimize over very large problem spaces, but is not guaranteed to converge. The method should be demonstrated by application of the functions recommended below, and possibly other standard or well-known optimization test cases.
</p>
<p>
The goal of parameter selection is to ensure that the global minimum is discriminated from any local minima, and that the minimum is accurately determined, and that convergence is achieved with acceptable resource usage.  To provide a common basis for comparing implementations, the following test cases are recommended:
<ul>
  <li> McCormick function - bowl-shaped, with a single minimum
    <ul> function parameters and bounds (recommended):
      <li> -1.5 < x1 < 4 </li>
      <li> -3   < x2 < 4 </li>
    </ul>
    <ul> search parameters (suggested):
      <li> omega = 0 </li>
      <li> phi p = 0.6 </li>
      <li> phi g = 0.3 </li>
      <li> number of particles = 100 </li>
      <li> number of iterations = 40 </li>
    </ul>
  <li> Michalewicz function - steep ridges and valleys, with multiple minima
    <ul> function parameters and bounds (recommended):
      <li> 0 < x1 < pi </li>
      <li> 0 < x2 < pi </li>
    </ul>
    <ul> search parameters (suggested):
      <li> omega = 0.3 </li>
      <li> phi p = 0.3 </li>
      <li> phi g = 0.3 </li>
      <li> number of particles = 1000 </li>
      <li> number of iterations = 30 </li>
    </ul>
</ul>
</p>
<p>
  References:
  <ul>
    <li>[Particle Swarm Optimization[https://en.wikipedia.org/wiki/Particle_swarm_optimization]]</li>
    <li>[Virtual Library of Optimization Test Functions[http://www.sfu.ca/~ssurjano/optimization.html]]</li>
  </ul>
</p>





## C++

```cpp
#include <algorithm>
#include <functional>
#include <iostream>
#include <random>
#include <vector>

const auto PI = std::atan2(0, -1);

bool double_equals(double a, double b, double epsilon = 0.001) {
    return std::abs(a - b) < epsilon;
}

template <typename T>
bool vector_equals(const std::vector<T> & lhs, const std::vector<T> & rhs) {
    if (lhs.size() != rhs.size()) {
        return false;
    }

    for (size_t i = 0; i < lhs.size(); i++) {
        if (!vector_equals(lhs[i], rhs[i])) {
            return false;
        }
    }

    return true;
}

template <typename T>
bool vector_equals(const T & lhs, const T & rhs) {
    return lhs == rhs;
}

template <>
bool vector_equals(const std::vector<double> & lhs, const std::vector<double> & rhs) {
    if (lhs.size() != rhs.size()) {
        return false;
    }

    for (size_t i = 0; i < lhs.size(); i++) {
        if (!double_equals(lhs[i], rhs[i])) {
            return false;
        }
    }

    return true;
}

template <typename T>
std::ostream& operator<<(std::ostream & os, const std::vector<T> & v) {
    auto it = v.cbegin();
    auto end = v.cend();

    os << '[';
    if (it != end) {
        os << *it;
        it = std::next(it);
    }
    while (it != end) {
        os << ", " << *it;
        it = std::next(it);
    }
    return os << ']';
}

double uniform01() {
    static std::default_random_engine generator;
    static std::uniform_real_distribution<double> distribution(0.0, 1.0);
    return distribution(generator);
}

struct Parameters {
    double omega, phip, phig;

    bool operator==(const Parameters& rhs) {
        return double_equals(omega, rhs.omega)
            && double_equals(phip, rhs.phip)
            && double_equals(phig, rhs.phig);
    }
};

struct State {
    int iter;
    std::vector<double> gbpos;
    double gbval;
    std::vector<double> min;
    std::vector<double> max;
    Parameters parameters;
    std::vector<std::vector<double>> pos;
    std::vector<std::vector<double>> vel;
    std::vector<std::vector<double>> bpos;
    std::vector<double> bval;
    int nParticles;
    int nDims;

    bool operator==(const State& rhs) {
        return iter == rhs.iter
            && vector_equals(gbpos, rhs.gbpos)
            && double_equals(gbval, rhs.gbval)
            && vector_equals(min, rhs.min)
            && vector_equals(max, rhs.max)
            && parameters == rhs.parameters
            && vector_equals(pos, rhs.pos)
            && vector_equals(vel, rhs.vel)
            && vector_equals(bpos, rhs.bpos)
            && vector_equals(bval, rhs.bval)
            && nParticles == rhs.nParticles
            && nDims == rhs.nDims;
    }

    void report(const std::string& testFunc) {
        std::cout << "Test Function        : " << testFunc << '\n';
        std::cout << "Iterations           : " << iter << '\n';
        std::cout << "Global Best Position : " << gbpos << '\n';
        std::cout << "Global Best Value    : " << gbval << '\n';
    }
};

State psoInit(const std::vector<double> & min, const std::vector<double> & max, const Parameters & parameters, int nParticles) {
    int nDims = min.size();

    std::vector<std::vector<double>> pos(nParticles);
    for (int i = 0; i < nParticles; i++) {
        std::copy(min.cbegin(), min.cend(), std::back_inserter(pos[i]));
    }

    std::vector<std::vector<double>> vel(nParticles);
    for (int i = 0; i < nParticles; i++) {
        vel[i].resize(nDims);
    }

    std::vector<std::vector<double>> bpos(nParticles);
    for (int i = 0; i < nParticles; i++) {
        std::copy(min.cbegin(), min.cend(), std::back_inserter(bpos[i]));
    }

    std::vector<double> bval(nParticles, HUGE_VAL);

    auto iter = 0;

    std::vector<double> gbpos(nDims, HUGE_VAL);

    auto gbval = HUGE_VAL;

    return{ iter, gbpos, gbval, min, max, parameters, pos, vel, bpos, bval, nParticles, nDims };
}

State pso(const std::function<double(const std::vector<double>&)> & fn, const State & y) {
    auto p = y.parameters;

    std::vector<double> v(y.nParticles);

    std::vector<std::vector<double>> bpos(y.nParticles);
    for (int i = 0; i < y.nParticles; i++) {
        std::copy(y.min.cbegin(), y.min.cend(), std::back_inserter(bpos[i]));
    }

    std::vector<double> bval(y.nParticles);

    std::vector<double> gbpos(y.nDims);

    auto gbval = HUGE_VAL;

    for (int j = 0; j < y.nParticles; j++) {
        // evaluate
        v[j] = fn(y.pos[j]);
        // update
        if (v[j] < y.bval[j]) {
            bpos[j] = y.pos[j];
            bval[j] = v[j];
        } else {
            bpos[j] = y.bpos[j];
            bval[j] = y.bval[j];
        }
        if (bval[j] < gbval) {
            gbval = bval[j];
            gbpos = bpos[j];
        }
    }

    auto rg = uniform01();

    std::vector<std::vector<double>> pos(y.nParticles);
    for (size_t i = 0; i < pos.size(); i++) {
        pos[i].resize(y.nDims);
    }

    std::vector<std::vector<double>> vel(y.nParticles);
    for (size_t i = 0; i < vel.size(); i++) {
        vel[i].resize(y.nDims);
    }

    for (size_t j = 0; j < y.nParticles; j++) {
        // migrate
        auto rp = uniform01();
        bool ok = true;
        std::fill(vel[j].begin(), vel[j].end(), 0);
        std::fill(pos[j].begin(), pos[j].end(), 0);
        for (int k = 0; k < y.nDims; ++k) {
            vel[j][k] = p.omega * y.vel[j][k] +
                p.phip * rp * (bpos[j][k] - y.pos[j][k]) +
                p.phig * rg * (gbpos[k] - y.pos[j][k]);
            pos[j][k] = y.pos[j][k] + vel[j][k];
            ok = ok && y.min[k] < pos[j][k] && y.max[k] > pos[j][k];
        }
        if (!ok) {
            for (int k = 0; k < y.nDims; ++k) {
                pos[j][k] = y.min[k] + (y.max[k] - y.min[k]) * uniform01();
            }
        }
    }

    auto iter = 1 + y.iter;

    return { iter, gbpos, gbval, y.min, y.max, y.parameters, pos, vel, bpos, bval, y.nParticles, y.nDims };
}

State iterate(const std::function<double(const std::vector<double>&)> & fn, int n, const State & y) {
    State r(y);
    if (n == INT32_MAX) {
        State old(y);
        while (true) {
            r = pso(fn, r);
            if (r == old) {
                break;
            }
            old = r;
        }
    } else {
        for (int i = 0; i < n; i++) {
            r = pso(fn, r);
        }
    }
    return r;
}

double mccormick(const std::vector<double> & x) {
    auto a = x[0];
    auto b = x[1];
    return sin(a + b) + (a - b) * (a - b) + 1.0 + 2.5 * b - 1.5 * a;
}

double michalewicz(const std::vector<double> & x) {
    auto m = 10;
    auto d = x.size();
    auto sum = 0.0;
    for (int i = 1; i < d; ++i) {
        auto j = x[i - 1];
        auto k = sin(i * j * j / PI);
        sum += sin(j) * pow(k, (2.0 * m));
    }
    return -sum;
}

int main() {
    auto state = psoInit(
        { -1.5, -3.0 },
        { 4.0, 4.0 },
        { 0.0, 0.6, 0.3 },
        100
    );
    state = iterate(mccormick, 40, state);
    state.report("McCormick");
    std::cout << "f(-0.54719, -1.54719) : " << mccormick({ -0.54719, -1.54719 }) << '\n';
    std::cout << '\n';

    state = psoInit(
        { 0.0, 0.0 },
        {PI, PI},
        { 0.3, 0.3, 0.3 },
        1000
    );
    state = iterate(michalewicz, 30, state);
    state.report("Michalewicz (2D)");
    std::cout << "f(2.20, 1.57)        : " << michalewicz({ 2.2, 1.57 }) << '\n';
}
```

```txt
Test Function        : McCormick
Iterations           : 40
Global Best Position : [-0.547284, -1.54737]
Global Best Value    : -1.91322
f(-0.54719, -1.54719) : -1.91322

Test Function        : Michalewicz (2D)
Iterations           : 30
Global Best Position : [2.20291, 1.2939]
Global Best Value    : -0.801303
f(2.20, 1.57)        : -0.801166
```


## C#
```c#
using System;

namespace ParticleSwarmOptimization {
    public struct Parameters {
        public double omega, phip, phig;

        public Parameters(double omega, double phip, double phig) : this() {
            this.omega = omega;
            this.phip = phip;
            this.phig = phig;
        }
    }

    public struct State {
        public int iter;
        public double[] gbpos;
        public double gbval;
        public double[] min;
        public double[] max;
        public Parameters parameters;
        public double[][] pos;
        public double[][] vel;
        public double[][] bpos;
        public double[] bval;
        public int nParticles;
        public int nDims;

        public State(int iter, double[] gbpos, double gbval, double[] min, double[] max, Parameters parameters, double[][] pos, double[][] vel, double[][] bpos, double[] bval, int nParticles, int nDims) : this() {
            this.iter = iter;
            this.gbpos = gbpos;
            this.gbval = gbval;
            this.min = min;
            this.max = max;
            this.parameters = parameters;
            this.pos = pos;
            this.vel = vel;
            this.bpos = bpos;
            this.bval = bval;
            this.nParticles = nParticles;
            this.nDims = nDims;
        }

        public void Report(string testfunc) {
            Console.WriteLine("Test Function        : {0}", testfunc);
            Console.WriteLine("Iterations           : {0}", iter);
            Console.WriteLine("Global Best Position : {0}", string.Join(", ", gbpos));
            Console.WriteLine("Global Best Value    : {0}", gbval);
        }
    }

    class Program {
        public static State PsoInit(double[] min, double[] max, Parameters parameters, int nParticles) {
            var nDims = min.Length;
            double[][] pos = new double[nParticles][];
            for (int i = 0; i < nParticles; i++) {
                pos[i] = new double[min.Length];
                min.CopyTo(pos[i], 0);
            }
            double[][] vel = new double[nParticles][];
            for (int i = 0; i < nParticles; i++) {
                vel[i] = new double[nDims];
            }
            double[][] bpos = new double[nParticles][];
            for (int i = 0; i < nParticles; i++) {
                bpos[i] = new double[min.Length];
                min.CopyTo(bpos[i], 0);
            }
            double[] bval = new double[nParticles];
            for (int i = 0; i < nParticles; i++) {
                bval[i] = double.PositiveInfinity;
            }
            int iter = 0;
            double[] gbpos = new double[nDims];
            for (int i = 0; i < nDims; i++) {
                gbpos[i] = double.PositiveInfinity;
            }
            double gbval = double.PositiveInfinity;

            return new State(iter, gbpos, gbval, min, max, parameters, pos, vel, bpos, bval, nParticles, nDims);
        }

        static Random r = new Random();

        public static State Pso(Func<double[], double> fn, State y) {
            var p = y.parameters;
            double[] v = new double[y.nParticles];
            double[][] bpos = new double[y.nParticles][];
            for (int i = 0; i < y.nParticles; i++) {
                bpos[i] = new double[y.min.Length];
                y.min.CopyTo(bpos[i], 0);
            }
            double[] bval = new double[y.nParticles];
            double[] gbpos = new double[y.nDims];
            double gbval = double.PositiveInfinity;
            for (int j = 0; j < y.nParticles; j++) {
                // evaluate
                v[j] = fn.Invoke(y.pos[j]);
                // update
                if (v[j] < y.bval[j]) {
                    y.pos[j].CopyTo(bpos[j], 0);
                    bval[j] = v[j];
                }
                else {
                    y.bpos[j].CopyTo(bpos[j], 0);
                    bval[j] = y.bval[j];
                }
                if (bval[j] < gbval) {
                    gbval = bval[j];
                    bpos[j].CopyTo(gbpos, 0);
                }
            }
            double rg = r.NextDouble();
            double[][] pos = new double[y.nParticles][];
            double[][] vel = new double[y.nParticles][];
            for (int i = 0; i < y.nParticles; i++) {
                pos[i] = new double[y.nDims];
                vel[i] = new double[y.nDims];
            }
            for (int j = 0; j < y.nParticles; j++) {
                // migrate
                double rp = r.NextDouble();
                bool ok = true;
                for (int k = 0; k < y.nDims; k++) {
                    vel[j][k] = 0.0;
                    pos[j][k] = 0.0;
                }
                for (int k = 0; k < y.nDims; k++) {
                    vel[j][k] = p.omega * y.vel[j][k] +
                                p.phip * rp * (bpos[j][k] - y.pos[j][k]) +
                                p.phig * rg * (gbpos[k] - y.pos[j][k]);
                    pos[j][k] = y.pos[j][k] + vel[j][k];
                    ok = ok && y.min[k] < pos[j][k] && y.max[k] > pos[j][k];
                }
                if (!ok) {
                    for (int k = 0; k < y.nDims; k++) {
                        pos[j][k] = y.min[k] + (y.max[k] - y.min[k]) * r.NextDouble();
                    }
                }
            }
            var iter = 1 + y.iter;
            return new State(iter, gbpos, gbval, y.min, y.max, y.parameters, pos, vel, bpos, bval, y.nParticles, y.nDims);
        }

        public static State Iterate(Func<double[], double> fn, int n, State y) {
            State r = y;
            if (n == int.MaxValue) {
                State old = y;
                while (true) {
                    r = Pso(fn, r);
                    if (r.Equals(old)) break;
                    old = r;
                }
            }
            else {
                for (int i = 0; i < n; i++) {
                    r = Pso(fn, r);
                }
            }
            return r;
        }

        public static double Mccormick(double[] x) {
            var a = x[0];
            var b = x[1];
            return Math.Sin(a + b) + (a - b) * (a - b) + 1.0 + 2.5 * b - 1.5 * a;
        }

        public static double Michalewicz(double[] x) {
            int m = 10;
            int d = x.Length;
            double sum = 0.0;
            for (int i = 1; i < d; i++) {
                var j = x[i - 1];
                var k = Math.Sin(i * j * j / Math.PI);
                sum += Math.Sin(j) * Math.Pow(k, 2.0 * m);
            }
            return -sum;
        }

        static void Main(string[] args) {
            var state = PsoInit(
                new double[] { -1.5, -3.0 },
                new double[] { 4.0, 4.0 },
                new Parameters(0.0, 0.6, 0.3),
                100
                );
            state = Iterate(Mccormick, 40, state);
            state.Report("McCormick");
            Console.WriteLine("f(-.54719, -1.54719) : {0}", Mccormick(new double[] { -.54719, -1.54719 }));
            Console.WriteLine();

            state = PsoInit(
                new double[] { -0.0, -0.0 },
                new double[] { Math.PI, Math.PI },
                new Parameters(0.3, 0.3, 0.3),
                1000
                );
            state = Iterate(Michalewicz, 30, state);
            state.Report("Michalewicz (2D)");
            Console.WriteLine("f(2.20, 1.57)        : {0}", Michalewicz(new double[] { 2.20, 1.57 }));
        }
    }
}
```

```txt
Test Function        : McCormick
Iterations           : 40
Global Best Position : -0.546850526417689, -1.54649614884518
Global Best Value    : -1.91322235333426
f(-.54719, -1.54719) : -1.91322295488227

Test Function        : Michalewicz (2D)
Iterations           : 30
Global Best Position : 2.20290514143486, 2.20798457238775
Global Best Value    : -0.801303410096221
f(2.20, 1.57)        : -0.801166387820286
```



## D

```D
import std.math;
import std.random;
import std.stdio;

alias Func = double function(double[]);

struct Parameters {
    double omega, phip, phig;
}

struct State {
    int iter;
    double[] gbpos;
    double gbval;
    double[] min;
    double[] max;
    Parameters parameters;
    double[][] pos;
    double[][] vel;
    double[][] bpos;
    double[] bval;
    int nParticles;
    int nDims;

    void report(string testfunc) {
        writeln("Test Function        : ", testfunc);
        writeln("Iterations           : ", iter);
        writefln("Global Best Position : [%(%.16f, %)]", gbpos);
        writefln("Global Best Value    : %.16f", gbval);
    }
}

State psoInit(double[] min, double[] max, Parameters parameters, int nParticles) {
    auto nDims = min.length;
    double[][] pos;
    pos.length = nParticles;
    pos[] = min;
    double[][] vel;
    vel.length = nParticles;
    for (int i; i<nParticles; i++) vel[i].length = nDims;
    double[][] bpos;
    bpos.length = nParticles;
    bpos[] = min;
    double[] bval;
    bval.length = nParticles;
    bval[] = double.infinity;
    auto iter = 0;
    double[] gbpos;
    gbpos.length = nDims;
    gbpos[] = double.infinity;
    auto gbval = double.infinity;
    return State(iter, gbpos, gbval, min, max, parameters, pos, vel, bpos, bval, nParticles, nDims);
}

State pso(Func fn, State y) {
    auto p = y.parameters;
    double[] v;
    v.length = y.nParticles;
    double[][] bpos;
    bpos.length = y.nParticles;
    bpos[] = y.min;
    double[] bval;
    bval.length = y.nParticles;
    double[] gbpos;
    gbpos.length = y.nDims;
    auto gbval = double.infinity;
    foreach (j; 0..y.nParticles) {
        // evaluate
        v[j] = fn(y.pos[j]);
        // update
        if (v[j] < y.bval[j]) {
            bpos[j] = y.pos[j];
            bval[j] = v[j];
        } else {
            bpos[j] = y.bpos[j];
            bval[j] = y.bval[j];
        }
        if (bval[j] < gbval) {
            gbval = bval[j];
            gbpos = bpos[j];
        }
    }
    auto rg = uniform01();
    double[][] pos;
    pos.length = y.nParticles;
    for (int i; i<pos.length; i++) pos[i].length = y.nDims;
    double[][] vel;
    vel.length = y.nParticles;
    for (int i; i<vel.length; i++) vel[i].length = y.nDims;
    foreach (j; 0..y.nParticles) {
        // migrate
        auto rp = uniform01();
        bool ok = true;
        vel[j][] = 0;
        pos[j][] = 0;
        foreach (k; 0..y.nDims) {
            vel[j][k] = p.omega * y.vel[j][k] +
                        p.phip * rp * (bpos[j][k] - y.pos[j][k]) +
                        p.phig * rg * (gbpos[k] - y.pos[j][k]);
            pos[j][k] = y.pos[j][k] + vel[j][k];
            ok = ok && y.min[k] < pos[j][k] && y.max[k] > pos[j][k];
        }
        if (!ok) {
            foreach (k; 0..y.nDims) {
                pos[j][k] = y.min[k] + (y.max[k] - y.min[k]) * uniform01();
            }
        }
    }
    auto iter = 1 + y.iter;
    return State(iter, gbpos, gbval, y.min, y.max, y.parameters, pos, vel, bpos, bval, y.nParticles, y.nDims);
}

State iterate(Func fn, int n, State y) {
    auto r = y;
    auto old = y;
    if (n == int.max) {
        while (true) {
            r = pso(fn, r);
            if (r == old) break;
            old = r;
        }
    } else {
        foreach (_; 0..n) r = pso(fn, r);
    }
    return r;
}

double mccormick(double[] x) {
    auto a = x[0];
    auto b = x[1];
    return sin(a + b) + (a - b) * (a - b) + 1.0 + 2.5 * b - 1.5 * a;
}

double michalewicz(double[] x) {
    auto m = 10;
    auto d = x.length;
    auto sum = 0.0;
    foreach (i; 1..d) {
        auto j = x[i - 1];
        auto k = sin(i * j * j / PI);
        sum += sin(j) * k^^(2.0*m);
    }
    return -sum;
}

void main() {
    auto state = psoInit(
        [-1.5, -3.0],
        [4.0, 4.0],
        Parameters(0.0, 0.6, 0.3),
        100
    );
    state = iterate(&mccormick, 40, state);
    state.report("McCormick");
    writefln("f(-.54719, -1.54719) : %.16f", mccormick([-.54719, -1.54719]));
    writeln;
    state = psoInit(
        [0.0, 0.0],
        [PI, PI],
        Parameters(0.3, 0.3, 0.3),
        1000
    );
    state = iterate(&michalewicz, 30, state);
    state.report("Michalewicz (2D)");
    writefln("f(2.20, 1.57)        : %.16f", michalewicz([2.2, 1.57]));
}
```

```txt
Test Function        : McCormick
Iterations           : 40
Global Best Position : [-0.5673174452967942, -1.5373177402652800]
Global Best Value    : -1.9122776571457756
f(-.54719, -1.54719) : -1.9132229548822735

Test Function        : Michalewicz (2D)
Iterations           : 30
Global Best Position : [2.1907380816516597, 1.5608474620076016]
Global Best Value    : -1.7949374368688056
f(2.20, 1.57)        : -1.8011407184738251
```



## Go

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "time"
)

type ff = func([]float64) float64

type parameters struct{ omega, phip, phig float64 }

type state struct {
    iter       int
    gbpos      []float64
    gbval      float64
    min        []float64
    max        []float64
    params     parameters
    pos        [][]float64
    vel        [][]float64
    bpos       [][]float64
    bval       []float64
    nParticles int
    nDims      int
}

func (s state) report(testfunc string) {
    fmt.Println("Test Function        :", testfunc)
    fmt.Println("Iterations           :", s.iter)
    fmt.Println("Global Best Position :", s.gbpos)
    fmt.Println("Global Best Value    :", s.gbval)
}

func psoInit(min, max []float64, params parameters, nParticles int) *state {
    nDims := len(min)
    pos := make([][]float64, nParticles)
    vel := make([][]float64, nParticles)
    bpos := make([][]float64, nParticles)
    bval := make([]float64, nParticles)
    for i := 0; i < nParticles; i++ {
        pos[i] = min
        vel[i] = make([]float64, nDims)
        bpos[i] = min
        bval[i] = math.Inf(1)
    }
    iter := 0
    gbpos := make([]float64, nDims)
    for i := 0; i < nDims; i++ {
        gbpos[i] = math.Inf(1)
    }
    gbval := math.Inf(1)
    return &state{iter, gbpos, gbval, min, max, params,
        pos, vel, bpos, bval, nParticles, nDims}
}

func pso(fn ff, y *state) *state {
    p := y.params
    v := make([]float64, y.nParticles)
    bpos := make([][]float64, y.nParticles)
    bval := make([]float64, y.nParticles)
    gbpos := make([]float64, y.nDims)
    gbval := math.Inf(1)
    for j := 0; j < y.nParticles; j++ {
        // evaluate
        v[j] = fn(y.pos[j])
        // update
        if v[j] < y.bval[j] {
            bpos[j] = y.pos[j]
            bval[j] = v[j]
        } else {
            bpos[j] = y.bpos[j]
            bval[j] = y.bval[j]
        }
        if bval[j] < gbval {
            gbval = bval[j]
            gbpos = bpos[j]
        }
    }
    rg := rand.Float64()
    pos := make([][]float64, y.nParticles)
    vel := make([][]float64, y.nParticles)
    for j := 0; j < y.nParticles; j++ {
        pos[j] = make([]float64, y.nDims)
        vel[j] = make([]float64, y.nDims)
        // migrate
        rp := rand.Float64()
        ok := true
        for z := 0; z < y.nDims; z++ {
            pos[j][z] = 0
            vel[j][z] = 0
        }
        for k := 0; k < y.nDims; k++ {
            vel[j][k] = p.omega*y.vel[j][k] +
                p.phip*rp*(bpos[j][k]-y.pos[j][k]) +
                p.phig*rg*(gbpos[k]-y.pos[j][k])
            pos[j][k] = y.pos[j][k] + vel[j][k]
            ok = ok && y.min[k] < pos[j][k] && y.max[k] > pos[j][k]
        }
        if !ok {
            for k := 0; k < y.nDims; k++ {
                pos[j][k] = y.min[k] + (y.max[k]-y.min[k])*rand.Float64()
            }
        }
    }
    iter := 1 + y.iter
    return &state{iter, gbpos, gbval, y.min, y.max, y.params,
        pos, vel, bpos, bval, y.nParticles, y.nDims}
}

func iterate(fn ff, n int, y *state) *state {
    r := y
    for i := 0; i < n; i++ {
        r = pso(fn, r)
    }
    return r
}

func mccormick(x []float64) float64 {
    a, b := x[0], x[1]
    return math.Sin(a+b) + (a-b)*(a-b) + 1.0 + 2.5*b - 1.5*a
}

func michalewicz(x []float64) float64 {
    m := 10.0
    sum := 0.0
    for i := 1; i <= len(x); i++ {
        j := x[i-1]
        k := math.Sin(float64(i) * j * j / math.Pi)
        sum += math.Sin(j) * math.Pow(k, 2*m)
    }
    return -sum
}

func main() {
    rand.Seed(time.Now().UnixNano())
    st := psoInit(
        []float64{-1.5, -3.0},
        []float64{4.0, 4.0},
        parameters{0.0, 0.6, 0.3},
        100,
    )
    st = iterate(mccormick, 40, st)
    st.report("McCormick")
    fmt.Println("f(-.54719, -1.54719) :", mccormick([]float64{-.54719, -1.54719}))
    fmt.Println()
    st = psoInit(
        []float64{0.0, 0.0},
        []float64{math.Pi, math.Pi},
        parameters{0.3, 0.3, 0.3},
        1000,
    )
    st = iterate(michalewicz, 30, st)
    st.report("Michalewicz (2D)")
    fmt.Println("f(2.20, 1.57)        :", michalewicz([]float64{2.2, 1.57}))

```


Sample output:

```txt

Test Function        : McCormick
Iterations           : 40
Global Best Position : [-0.5473437041724806 -1.5464923165739348]
Global Best Value    : -1.9132220947578635
f(-.54719, -1.54719) : -1.913222954882274

Test Function        : Michalewicz (2D)
Iterations           : 30
Global Best Position : [2.2029051150895165 1.570796212894911]
Global Best Value    : -1.8013034100953598
f(2.20, 1.57)        : -1.8011407184738253

```



## J


```J
load 'format/printf'

pso_init =: verb define
   'Min Max parameters nParticles' =. y
   'Min: %j\nMax: %j\nomega, phip, phig: %j\nnParticles: %j\n' printf Min;Max;parameters;nParticles
   nDims =. #Min
   pos =. Min +"1 (Max - Min) *"1 (nParticles,nDims) ?@$ 0
   bpos =. pos
   bval =. (#pos) $ _
   vel  =. ($pos) $ 0
   0;_;_;Min;Max;parameters;pos;vel;bpos;bval      NB. initial state
)

pso =: adverb define
   NB. previous state
   'iter gbpos gbval Min Max parameters pos vel bpos0 bval' =. y

   NB. evaluate
   val    =. u"1 pos

   NB. update
   better =. val < bval
   bpos   =. (better # pos) (I. better)} bpos0
   bval   =. u"1 bpos
   gbval  =. <./ bval
   gbpos  =. bpos {~ (i. <./) bval

   NB. migrate
   'omega phip phig' =. parameters
   rp  =. (#pos) ?@$ 0
   rg  =. ? 0
   vel =. (omega*vel) + (phip * rp * bpos - pos) + (phig * rg * gbpos -"1 pos)
   pos =. pos + vel

   NB. reset out-of-bounds particles
   bad    =. +./"1 (Min >"1 pos) ,. (pos >"1 Max)
   newpos =. Min +"1 (Max-Min) *"1 ((+/bad),#Min) ?@$ 0
   pos    =. newpos (I. bad)} pos
   iter   =. >: iter

   NB. new state
   iter;gbpos;gbval;Min;Max;parameters;pos;vel;bpos;bval
)

reportState=: 'Iteration: %j\nGlobalBestPosition: %j\nGlobalBestValue: %j\n' printf 3&{.
```

Apply to McCormick Function:
```J
   require 'trig'
   mccormick =: sin@(+/) + *:@(-/) + 1 + _1.5 2.5 +/@:* ]

   state =: pso_init _1.5 _3 ; 4 4 ; 0 0.6 0.3; 100
Min: _1.5 _3
Max: 4 4
omega, phip, phig: 0 0.6 0.3
nParticles: 100

   state =: (mccormick pso)^:40 state
   reportState state
Iteration: 40
GlobalBestPosition: _0.547399 _1.54698
GlobalBestValue: _1.91322
```

Apply to Michalewicz Function:

```J
   michalewicz =: 3 : '- +/ (sin y) * 20 ^~ sin (>: i. #y) * (*:y) % pi'
   michalewicz =: [: -@(+/) sin * 20 ^~ sin@(pi %~ >:@i.@# * *:)  NB. tacit equivalent

   state =: pso_init 0 0 ; (pi,pi) ; 0.3 0.3 0.3; 1000
Min: 0 0
Max: 3.14159 3.14159
omega, phip, phig: 0.3 0.3 0.3
nParticles: 1000

   state =: (michalewicz pso)^:30 state
   reportState state
Iteration: 30
GlobalBestPosition: 2.20296 1.57083
GlobalBestValue: _1.8013
```



## JavaScript


Translation of [[Particle_Swarm_Optimization#J|J]].


```JavaScript
function pso_init(y) {
  var nDims= y.min.length;
  var pos=[], vel=[], bpos=[], bval=[];
  for (var j= 0; j<y.nParticles; j++) {
    pos[j]= bpos[j]= y.min;
    var v= []; for (var k= 0; k<nDims; k++) v[k]= 0;
    vel[j]= v;
    bval[j]= Infinity}
  return {
	iter: 0,
	gbpos: Infinity,
	gbval: Infinity,
	min: y.min,
	max: y.max,
	parameters: y.parameters,
	pos: pos,
	vel: vel,
	bpos: bpos,
	bval: bval,
        nParticles: y.nParticles,
        nDims: nDims}
}

function pso(fn, state) {
  var y= state;
  var p= y.parameters;
  var val=[], bpos=[], bval=[], gbval= Infinity, gbpos=[];
  for (var j= 0; j<y.nParticles; j++) {
    // evaluate
    val[j]= fn.apply(null, y.pos[j]);
    // update
    if (val[j] < y.bval[j]) {
      bpos[j]= y.pos[j];
      bval[j]= val[j];
    } else {
      bpos[j]= y.bpos[j];
      bval[j]= y.bval[j]}
    if (bval[j] < gbval) {
      gbval= bval[j];
      gbpos= bpos[j]}}
  var rg= Math.random(), vel=[], pos=[];
  for (var j= 0; j<y.nParticles; j++) {
    // migrate
    var rp= Math.random(), ok= true;
    vel[j]= [];
    pos[j]= [];
    for (var k= 0; k < y.nDims; k++) {
      vel[j][k]= p.omega*y.vel[j][k] + p.phip*rp*(bpos[j]-y.pos[j]) + p.phig*rg*(gbpos-y.pos[j]);
      pos[j][k]= y.pos[j]+vel[j][k];
      ok= ok && y.min[k]<pos[j][k] && y.max>pos[j][k];}
    if (!ok)
      for (var k= 0; k < y.nDims; k++)
        pos[j][k]= y.min[k] + (y.max[k]-y.min[k])*Math.random()}
  return {
	iter: 1+y.iter,
	gbpos: gbpos,
	gbval: gbval,
	min: y.min,
	max: y.max,
	parameters: y.parameters,
	pos: pos,
	vel: vel,
	bpos: bpos,
	bval: bval,
        nParticles: y.nParticles,
        nDims: y.nDims}
}

function display(text) {
  if (document) {
    var o= document.getElementById('o');
    if (!o) {
      o= document.createElement('pre');
      o.id= 'o';
      document.body.appendChild(o)}
    o.innerHTML+= text+'\n';
    window.scrollTo(0,document.body.scrollHeight);
  }
  if (console.log) console.log(text)
}

function reportState(state) {
  var y= state;
  display('');
  display('Iteration: '+y.iter);
  display('GlobalBestPosition: '+y.gbpos);
  display('GlobalBestValue: '+y.gbval);
}

function repeat(fn, n, y) {
  var r=y, old= y;
  if (Infinity == n)
    while ((r= fn(r)) != old) old= r;
  else
    for (var j= 0; j<n; j++) r= fn(r);
  return r
}

function mccormick(a,b) {
  return Math.sin(a+b) + Math.pow(a-b,2) + (1 + 2.5*b - 1.5*a)
}

state= pso_init({
  min: [-1.5,-3], max:[4,4],
  parameters: {omega: 0, phip: 0.6, phig: 0.3},
  nParticles: 100});

reportState(state);

state= repeat(function(y){return pso(mccormick,y)}, 40, state);

reportState(state);
```


Example displayed result (random numbers are involved so there will be a bit of variance between repeated runs:


```Javascript

Iteration: 0
GlobalBestPosition: Infinity
GlobalBestValue: Infinity

Iteration: 40
GlobalBestPosition: -0.5134004259016365,-1.5512442672625184
GlobalBestValue: -1.9114053788600853
```



## Java

```java
import java.util.Arrays;
import java.util.Objects;
import java.util.Random;
import java.util.function.Function;

public class App {
    static class Parameters {
        double omega;
        double phip;
        double phig;

        Parameters(double omega, double phip, double phig) {
            this.omega = omega;
            this.phip = phip;
            this.phig = phig;
        }
    }

    static class State {
        int iter;
        double[] gbpos;
        double gbval;
        double[] min;
        double[] max;
        Parameters parameters;
        double[][] pos;
        double[][] vel;
        double[][] bpos;
        double[] bval;
        int nParticles;
        int nDims;

        State(int iter, double[] gbpos, double gbval, double[] min, double[] max, Parameters parameters, double[][] pos, double[][] vel, double[][] bpos, double[] bval, int nParticles, int nDims) {
            this.iter = iter;
            this.gbpos = gbpos;
            this.gbval = gbval;
            this.min = min;
            this.max = max;
            this.parameters = parameters;
            this.pos = pos;
            this.vel = vel;
            this.bpos = bpos;
            this.bval = bval;
            this.nParticles = nParticles;
            this.nDims = nDims;
        }

        void report(String testfunc) {
            System.out.printf("Test Function        : %s\n", testfunc);
            System.out.printf("Iterations           : %d\n", iter);
            System.out.printf("Global Best Position : %s\n", Arrays.toString(gbpos));
            System.out.printf("Global Best value    : %.15f\n", gbval);
        }
    }

    private static State psoInit(double[] min, double[] max, Parameters parameters, int nParticles) {
        int nDims = min.length;
        double[][] pos = new double[nParticles][];
        for (int i = 0; i < nParticles; ++i) {
            pos[i] = min.clone();
        }
        double[][] vel = new double[nParticles][nDims];
        double[][] bpos = new double[nParticles][];
        for (int i = 0; i < nParticles; ++i) {
            bpos[i] = min.clone();
        }
        double[] bval = new double[nParticles];
        for (int i = 0; i < bval.length; ++i) {
            bval[i] = Double.POSITIVE_INFINITY;
        }
        int iter = 0;
        double[] gbpos = new double[nDims];
        for (int i = 0; i < gbpos.length; ++i) {
            gbpos[i] = Double.POSITIVE_INFINITY;
        }
        double gbval = Double.POSITIVE_INFINITY;
        return new State(iter, gbpos, gbval, min, max, parameters, pos, vel, bpos, bval, nParticles, nDims);
    }

    private static Random r = new Random();

    private static State pso(Function<double[], Double> fn, State y) {
        Parameters p = y.parameters;
        double[] v = new double[y.nParticles];
        double[][] bpos = new double[y.nParticles][];
        for (int i = 0; i < y.nParticles; ++i) {
            bpos[i] = y.min.clone();
        }
        double[] bval = new double[y.nParticles];
        double[] gbpos = new double[y.nDims];
        double gbval = Double.POSITIVE_INFINITY;
        for (int j = 0; j < y.nParticles; ++j) {
            // evaluate
            v[j] = fn.apply(y.pos[j]);
            // update
            if (v[j] < y.bval[j]) {
                bpos[j] = y.pos[j];
                bval[j] = v[j];
            } else {
                bpos[j] = y.bpos[j];
                bval[j] = y.bval[j];
            }
            if (bval[j] < gbval) {
                gbval = bval[j];
                gbpos = bpos[j];
            }
        }
        double rg = r.nextDouble();
        double[][] pos = new double[y.nParticles][y.nDims];
        double[][] vel = new double[y.nParticles][y.nDims];
        for (int j = 0; j < y.nParticles; ++j) {
            // migrate
            double rp = r.nextDouble();
            boolean ok = true;
            Arrays.fill(vel[j], 0.0);
            Arrays.fill(pos[j], 0.0);
            for (int k = 0; k < y.nDims; ++k) {
                vel[j][k] = p.omega * y.vel[j][k] +
                    p.phip * rp * (bpos[j][k] - y.pos[j][k]) +
                    p.phig * rg * (gbpos[k] - y.pos[j][k]);
                pos[j][k] = y.pos[j][k] + vel[j][k];
                ok = ok && y.min[k] < pos[j][k] && y.max[k] > pos[j][k];
            }
            if (!ok) {
                for (int k = 0; k < y.nDims; ++k) {
                    pos[j][k] = y.min[k] + (y.max[k] - y.min[k]) * r.nextDouble();
                }
            }
        }
        int iter = 1 + y.iter;
        return new State(
            iter, gbpos, gbval, y.min, y.max, y.parameters,
            pos, vel, bpos, bval, y.nParticles, y.nDims
        );
    }

    private static State iterate(Function<double[], Double> fn, int n, State y) {
        State r = y;
        if (n == Integer.MAX_VALUE) {
            State old = y;
            while (true) {
                r = pso(fn, r);
                if (Objects.equals(r, old)) break;
                old = r;
            }
        } else {
            for (int i = 0; i < n; ++i) {
                r = pso(fn, r);
            }
        }
        return r;
    }

    private static double mccormick(double[] x) {
        double a = x[0];
        double b = x[1];
        return Math.sin(a + b) + (a - b) * (a - b) + 1.0 + 2.5 * b - 1.5 * a;
    }

    private static double michalewicz(double[] x) {
        int m = 10;
        int d = x.length;
        double sum = 0.0;
        for (int i = 1; i < d; ++i) {
            double j = x[i - 1];
            double k = Math.sin(i * j * j / Math.PI);
            sum += Math.sin(j) * Math.pow(k, 2.0 * m);
        }
        return -sum;
    }

    public static void main(String[] args) {
        State state = psoInit(
            new double[]{-1.5, -3.0},
            new double[]{4.0, 4.0},
            new Parameters(0.0, 0.6, 0.3),
            100
        );
        state = iterate(App::mccormick, 40, state);
        state.report("McCormick");
        System.out.printf("f(-.54719, -1.54719) : %.15f\n", mccormick(new double[]{-.54719, -1.54719}));
        System.out.println();

        state = psoInit(
            new double[]{0.0, 0.0},
            new double[]{Math.PI, Math.PI},
            new Parameters(0.3, 3.0, 0.3),
            1000
        );
        state = iterate(App::michalewicz, 30, state);
        state.report("Michalewicz (2D)");
        System.out.printf("f(2.20, 1.57)        : %.15f\n", michalewicz(new double[]{2.20, 1.57}));
    }
}
```

```txt
Test Function        : McCormick
Iterations           : 40
Global Best Position : [-0.5468738679864172, -1.547048532862534]
Global Best value    : -1.913222827709136
f(-.54719, -1.54719) : -1.913222954882274

Test Function        : Michalewicz (2D)
Iterations           : 30
Global Best Position : [2.2029055320517994, 1.832848319327826]
Global Best value    : -0.801303410098550
f(2.20, 1.57)        : -0.801166387820286
```



## Julia


```julia
using Optim

const mcclow = [-1.5, -3.0]
const mccupp = [4.0, 4.0]
const miclow = [0.0, 0.0]
const micupp = Float64.([pi, pi])
const npar = [100, 1000]
const x0 = [0.0, 0.0]

michalewicz(x, m=10) = -sum(i -> sin(x[i]) * (i * sin( x[i]^2/pi))^(2*m), 1:length(x))

mccormick(x) = sin(x[1] + x[2]) + (x[1] - x[2])^2 - 1.5 * x[1] + 2.5 * x[2] + 1


println(optimize(mccormick, x0, ParticleSwarm(;lower=mcclow, upper=mccupp, n_particles=npar[1])))
@time optimize(mccormick, x0, ParticleSwarm(;lower=mcclow, upper=mccupp, n_particles=npar[1]))

println(optimize(michalewicz, x0, ParticleSwarm(;lower=miclow, upper=micupp, n_particles=npar[2])))
@time optimize(michalewicz, x0, ParticleSwarm(;lower=miclow, upper=micupp, n_particles=npar[2]))

```
```txt

Results of Optimization Algorithm
 * Algorithm: Particle Swarm
 * Starting Point: [0.0,0.0]
 * Minimizer: [-0.5471975503990738,-1.5471975447742121]
 * Minimum: -1.913223e+00
 * Iterations: 1000
 * Convergence: false
   * |x - x'| ≤ 0.0e+00: false
     |x - x'| = NaN
   * |f(x) - f(x')| ≤ 0.0e+00 |f(x)|: false
     |f(x) - f(x')| = NaN |f(x)|
   * |g(x)| ≤ 1.0e-08: false
     |g(x)| = NaN
   * Stopped by an increasing objective: false
   * Reached Maximum Number of Iterations: true
 * Objective Calls: 101001
 * Gradient Calls: 0
0.087319 seconds (228.91 k allocations: 12.098 MiB, 59.41% gc time)

Results of Optimization Algorithm
 * Algorithm: Particle Swarm
 * Starting Point: [0.0,0.0]
 * Minimizer: [2.202905520771759,1.5707963264041795]
 * Minimum: -1.801303e+00
 * Iterations: 1000
 * Convergence: false
   * |x - x'| ≤ 0.0e+00: false
     |x - x'| = NaN
   * |f(x) - f(x')| ≤ 0.0e+00 |f(x)|: false
     |f(x) - f(x')| = NaN |f(x)|
   * |g(x)| ≤ 1.0e-08: false
     |g(x)| = NaN
   * Stopped by an increasing objective: false
   * Reached Maximum Number of Iterations: true
 * Objective Calls: 1001001
 * Gradient Calls: 0
2.312291 seconds (3.52 M allocations: 153.253 MiB, 0.49% gc time)

```



## Kotlin

```scala
// version 1.1.51

import java.util.Random

typealias Func = (DoubleArray) -> Double

class Parameters(val omega: Double, val phip: Double, val phig: Double)

class State(
    val iter: Int,
    val gbpos: DoubleArray,
    val gbval: Double,
    val min: DoubleArray,
    val max: DoubleArray,
    val parameters: Parameters,
    val pos: Array<DoubleArray>,
    val vel: Array<DoubleArray>,
    val bpos: Array<DoubleArray>,
    val bval: DoubleArray,
    val nParticles: Int,
    val nDims: Int
) {
    fun report(testfunc: String) {
        println("Test Function        : $testfunc")
        println("Iterations           : $iter")
        println("Global Best Position : ${gbpos.asList()}")
        println("Global Best Value    : $gbval")
    }
}

fun psoInit(
    min: DoubleArray,
    max: DoubleArray,
    parameters: Parameters,
    nParticles: Int
): State {
    val nDims = min.size
    val pos   = Array(nParticles) { min }
    val vel   = Array(nParticles) { DoubleArray(nDims) }
    val bpos  = Array(nParticles) { min }
    val bval  = DoubleArray(nParticles) { Double.POSITIVE_INFINITY}
    val iter  = 0
    val gbpos = DoubleArray(nDims) { Double.POSITIVE_INFINITY }
    val gbval = Double.POSITIVE_INFINITY
    return State(iter, gbpos, gbval, min, max, parameters,
                 pos, vel, bpos, bval, nParticles, nDims)
}

val r = Random()

fun pso(fn: Func, y: State): State {
    val p = y.parameters
    val v = DoubleArray(y.nParticles)
    val bpos  = Array(y.nParticles) { y.min }
    val bval  = DoubleArray(y.nParticles)
    var gbpos = DoubleArray(y.nDims)
    var gbval = Double.POSITIVE_INFINITY
    for (j in 0 until y.nParticles) {
        // evaluate
        v[j] = fn(y.pos[j])
        // update
        if (v[j] < y.bval[j]) {
            bpos[j] = y.pos[j]
            bval[j] = v[j]
        }
        else {
            bpos[j] = y.bpos[j]
            bval[j] = y.bval[j]
        }
        if (bval[j] < gbval) {
            gbval = bval[j]
            gbpos = bpos[j]
        }
    }
    val rg = r.nextDouble()
    val pos = Array(y.nParticles) { DoubleArray(y.nDims) }
    val vel = Array(y.nParticles) { DoubleArray(y.nDims) }
    for (j in 0 until y.nParticles) {
        // migrate
        val rp = r.nextDouble()
        var ok = true
        vel[j].fill(0.0)
        pos[j].fill(0.0)
        for (k in 0 until y.nDims) {
            vel[j][k] = p.omega * y.vel[j][k] +
                        p.phip * rp * (bpos[j][k] - y.pos[j][k]) +
                        p.phig * rg * (gbpos[k] - y.pos[j][k])
            pos[j][k] = y.pos[j][k] + vel[j][k]
            ok = ok && y.min[k] < pos[j][k] && y.max[k] > pos[j][k]
        }
        if (!ok) {
            for (k in 0 until y.nDims) {
                pos[j][k]= y.min[k] + (y.max[k] - y.min[k]) * r.nextDouble()
            }
        }
    }
    val iter = 1 + y.iter
    return State(
        iter, gbpos, gbval, y.min, y.max, y.parameters,
        pos, vel, bpos, bval, y.nParticles, y.nDims
    )
}

fun iterate(fn: Func, n: Int, y: State): State {
    var r = y
    var old = y
    if (n == Int.MAX_VALUE) {
        while (true) {
            r = pso(fn, r)
            if (r == old) break
            old = r
        }
    }
    else {
        repeat(n) { r = pso(fn, r) }
    }
    return r
}

fun mccormick(x: DoubleArray): Double {
    val (a, b) = x
    return Math.sin(a + b) + (a - b) * (a - b) + 1.0 + 2.5 * b - 1.5 * a
}

fun michalewicz(x: DoubleArray): Double {
    val m = 10
    val d = x.size
    var sum = 0.0
    for (i in 1..d) {
        val j = x[i - 1]
        val k = Math.sin(i * j * j / Math.PI)
        sum += Math.sin(j) * Math.pow(k, 2.0 * m)
    }
    return -sum
}

fun main(args: Array<String>) {
    var state = psoInit(
        min = doubleArrayOf(-1.5, -3.0),
        max = doubleArrayOf(4.0, 4.0),
        parameters = Parameters(0.0, 0.6, 0.3),
        nParticles = 100
    )
    state = iterate(::mccormick, 40, state)
    state.report("McCormick")
    println("f(-.54719, -1.54719) : ${mccormick(doubleArrayOf(-.54719, -1.54719))}")
    println()
    state = psoInit(
        min = doubleArrayOf(0.0, 0.0),
        max = doubleArrayOf(Math.PI, Math.PI),
        parameters = Parameters(0.3, 0.3, 0.3),
        nParticles = 1000
    )
    state = iterate(::michalewicz, 30, state)
    state.report("Michalewicz (2D)")
    println("f(2.20, 1.57)        : ${michalewicz(doubleArrayOf(2.2, 1.57))}")
}
```


Sample output:

```txt

Test Function        : McCormick
Iterations           : 40
Global Best Position : [-0.5471015946082899, -1.5471991634200966]
Global Best Value    : -1.913222941607108
f(-.54719, -1.54719) : -1.913222954882274

Test Function        : Michalewicz (2D)
Iterations           : 30
Global Best Position : [2.202908690715102, 1.5707970450218895]
Global Best Value    : -1.8013034099142804
f(2.20, 1.57)        : -1.801140718473825

```



## ooRexx


```oorexx
/* REXX ---------------------------------------------------------------
* Test for McCormick function
*--------------------------------------------------------------------*/
Numeric Digits 16
Parse Value '-.5 -1.5 1' With x y d
fmin=1e9
Call refine x,y
Do r=1 To 10
  d=d/5
  Call refine xmin,ymin
  End
Say 'which is better (less) than'
Say '        f(-.54719,-1.54719)='f(-.54719,-1.54719)
Say 'and differs from published  -1.9133'
Exit

refine:
Parse Arg xx,yy
Do x=xx-d To xx+d By d/2
  Do y=yy-d To yy+d By d/2
    f=f(x,y)
    If f<fmin Then Do
      Say x y f
      fmin=f
      xmin=x
      ymin=y
      End
    End
  End
Return

f:
Parse Arg x,y
res=rxcalcsin(x+y,16,'R')+(x-y)**2-1.5*x+2.5*y+1
Return res
::requires rxmath library
```

```txt
-1.5 -2.5 -1.243197504692072
-1.0 -2.0 -1.641120008059867
-0.5 -1.5 -1.909297426825682
-0.54 -1.54 -1.913132979507516
-0.548 -1.548 -1.913221840016527
-0.5480 -1.5472 -1.913222034492829
-0.5472 -1.5472 -1.913222954970650
-0.54720000 -1.54719872 -1.913222954973731
-0.54719872 -1.54719872 -1.913222954978670
-0.54719872 -1.54719744 -1.913222954978914
-0.54719744 -1.54719744 -1.913222954981015
-0.5471975424 -1.5471975424 -1.913222954981036
which is better (less) than
        f(-.54719,-1.54719)=-1.913222954882273
and differs from published  -1.9133
```



## Phix

```Phix
enum OMEGA, PHIP, PHIG
enum ITER,GBPOS,GBVAL,MIN,MAX,PARAMS,POS,VEL,BPOS,BVAL,NPARTICLES,NDIMS

constant inf = 1e308*1e308

constant fmt = """
Test Function        : %s
Iterations           : %d
Global Best Position : %s
Global Best Value    : %f
"""

procedure report(sequence state, string testfunc)
    printf(1,fmt,{testfunc,state[ITER],sprint(state[GBPOS]),state[GBVAL]})
end procedure

function psoInit(sequence mins, maxs, params, integer nParticles)
    integer nDims = length(mins), iter=0
    atom gbval = inf
    sequence gbpos = repeat(inf,nDims),
             pos = repeat(mins,nParticles),
             vel = repeat(repeat(0,nDims),nParticles),
             bpos = repeat(mins,nParticles),
             bval = repeat(inf,nParticles)
    return {iter,gbpos,gbval,mins,maxs,params,pos,vel,bpos,bval,nParticles,nDims}
end function

function pso(integer fn, sequence state)
    integer particles = state[NPARTICLES],
            dims = state[NDIMS]
    sequence p = state[PARAMS],
             v = repeat(0,particles),
             bpos = repeat(state[MIN],particles),
             bval = repeat(0,particles),
             gbpos = repeat(0,dims)
    atom gbval = inf
    for j=1 to particles do
        -- evaluate
        v[j] = call_func(fn,{state[POS][j]})
        -- update
        if v[j] < state[BVAL][j] then
            bpos[j] = state[POS][j]
            bval[j] = v[j]
        else
            bpos[j] = state[BPOS][j]
            bval[j] = state[BVAL][j]
        end if
        if bval[j] < gbval then
            gbval = bval[j]
            gbpos = bpos[j]
        end if
    end for
    atom rg = rnd()
    sequence pos = repeat(repeat(0,dims),particles),
             vel = repeat(repeat(0,dims),particles)
    for j=1 to particles do
        -- migrate
        atom rp = rnd()
        bool ok = true
        vel[j] = repeat(0,dims)
        pos[j] = repeat(0,dims)
        for k=1 to dims do
            vel[j][k] = p[OMEGA] * state[VEL][j][k] +
                        p[PHIP] * rp * (bpos[j][k] - state[POS][j][k]) +
                        p[PHIG] * rg * (gbpos[k] - state[POS][j][k])
            pos[j][k] = state[POS][j][k] + vel[j][k]
            ok = ok and state[MIN][k] < pos[j][k] and state[MAX][k] > pos[j][k]
        end for
        if not ok then
            for k=1 to dims do
                pos[j][k]= state[MIN][k] + (state[MAX][k] - state[MIN][k]) * rnd()
            end for
        end if
    end for
    integer iter = 1 + state[ITER]
    return {iter, gbpos, gbval, state[MIN], state[MAX], state[PARAMS],
            pos, vel, bpos, bval, particles, dims}
end function

function iterate(integer fn, n, sequence state)
    sequence r = state,
             old = state
    if n=-1 then
        while true do
            r = pso(fn, r)
            if (r == old) then exit end if
            old = r
        end while
    else
        for i=1 to n do
            r = pso(fn, r)
        end for
    end if
    return r
end function

function mccormick(sequence x)
    atom {a, b} = x
    return sin(a + b) + (a - b) * (a - b) + 1.0 + 2.5 * b - 1.5 * a
end function
constant r_mccormick = routine_id("mccormick")

function michalewicz(sequence x)
    integer m = 10,
            d = length(x)
    atom total = 0.0
    for i=1 to d do
        atom j = x[i],
             k = sin(i * j * j / PI)
        total += sin(j) * power(k, 2.0 * m)
    end for
    return -total
end function
constant r_michalewicz = routine_id("michalewicz")

procedure main()
    sequence mins = {-1.5, -3.0},
             maxs = {4.0, 4.0},
             params = {0.0, 0.6, 0.3}
    integer nParticles = 100
    sequence state = psoInit(mins,maxs,params,nParticles)
    state = iterate(r_mccormick, 40, state)
    report(state,"McCormick")
    atom {x,y} = state[GBPOS]
    printf(1,"f(%.4f, %.4f)  : %f\n\n",{x,y,mccormick({x,y})})

    mins = {0.0, 0.0}
    maxs = {PI, PI}
    params = {0.3, 0.3, 0.3}
    nParticles = 1000
    state = psoInit(mins,maxs,params,nParticles)
    state = iterate(r_michalewicz, 30, state)
    report(state,"Michalewicz (2D)")
    {x,y} = state[GBPOS]
    printf(1,"f(%.5f, %.5f)  : %f\n\n",{x,y,michalewicz({x,y})})
end procedure
main()
```

```txt

Test Function        : McCormick
Iterations           : 40
Global Best Position : {-0.5471808566,-1.547021879}
Global Best Value    : -1.913223
f(-0.5472, -1.5470)  : -1.913223

Test Function        : Michalewicz (2D)
Iterations           : 30
Global Best Position : {2.202905614,1.570796293}
Global Best Value    : -1.801303
f(2.20291, 1.57080)  : -1.801303

```



## Python

```python
import math
import random

INFINITY = 1 << 127
MAX_INT = 1 << 31

class Parameters:
    def __init__(self, omega, phip, phig):
        self.omega = omega
        self.phip = phip
        self.phig = phig

class State:
    def __init__(self, iter, gbpos, gbval, min, max, parameters, pos, vel, bpos, bval, nParticles, nDims):
        self.iter = iter
        self.gbpos = gbpos
        self.gbval = gbval
        self.min = min
        self.max = max
        self.parameters = parameters
        self.pos = pos
        self.vel = vel
        self.bpos = bpos
        self.bval = bval
        self.nParticles = nParticles
        self.nDims = nDims

    def report(self, testfunc):
        print "Test Function :", testfunc
        print "Iterations    :", self.iter
        print "Global Best Position :", self.gbpos
        print "Global Best Value    : %.16f" % self.gbval

def uniform01():
    v = random.random()
    assert 0.0 <= v and v < 1.0
    return v

def psoInit(min, max, parameters, nParticles):
    nDims = len(min)
    pos = [min[:]] * nParticles
    vel = [[0.0] * nDims] * nParticles
    bpos = [min[:]] * nParticles
    bval = [INFINITY] * nParticles
    iter = 0
    gbpos = [INFINITY] * nDims
    gbval = INFINITY
    return State(iter, gbpos, gbval, min, max, parameters, pos, vel, bpos, bval, nParticles, nDims);

def pso(fn, y):
    p = y.parameters
    v = [0.0] * (y.nParticles)
    bpos = [y.min[:]] * (y.nParticles)
    bval = [0.0] * (y.nParticles)
    gbpos = [0.0] * (y.nDims)
    gbval = INFINITY
    for j in xrange(0, y.nParticles):
        # evaluate
        v[j] = fn(y.pos[j])
        # update
        if v[j] < y.bval[j]:
            bpos[j] = y.pos[j][:]
            bval[j] = v[j]
        else:
            bpos[j] = y.bpos[j][:]
            bval[j] = y.bval[j]
        if bval[j] < gbval:
            gbval = bval[j]
            gbpos = bpos[j][:]
    rg = uniform01()
    pos = [[None] * (y.nDims)] * (y.nParticles)
    vel = [[None] * (y.nDims)] * (y.nParticles)
    for j in xrange(0, y.nParticles):
        # migrate
        rp = uniform01()
        ok = True
        vel[j] = [0.0] * (len(vel[j]))
        pos[j] = [0.0] * (len(pos[j]))
        for k in xrange(0, y.nDims):
            vel[j][k] = p.omega * y.vel[j][k] \
                      + p.phip * rp * (bpos[j][k] - y.pos[j][k]) \
                      + p.phig * rg * (gbpos[k] - y.pos[j][k])
            pos[j][k] = y.pos[j][k] + vel[j][k]
            ok = ok and y.min[k] < pos[j][k] and y.max[k] > pos[j][k]
        if not ok:
            for k in xrange(0, y.nDims):
                pos[j][k] = y.min[k] + (y.max[k] - y.min[k]) * uniform01()
    iter = 1 + y.iter
    return State(iter, gbpos, gbval, y.min, y.max, y.parameters, pos, vel, bpos, bval, y.nParticles, y.nDims);

def iterate(fn, n, y):
    r = y
    old = y
    if n == MAX_INT:
        while True:
            r = pso(fn, r)
            if r == old:
                break
            old = r
    else:
        for _ in xrange(0, n):
            r = pso(fn, r)
    return r

def mccormick(x):
    (a, b) = x
    return math.sin(a + b) + (a - b) * (a - b) + 1.0 + 2.5 * b - 1.5 * a

def michalewicz(x):
    m = 10
    d = len(x)
    sum = 0.0
    for i in xrange(1, d):
        j = x[i - 1]
        k = math.sin(i * j * j / math.pi)
        sum += math.sin(j) * k ** (2.0 * m)
    return -sum

def main():
    state = psoInit([-1.5, -3.0], [4.0, 4.0], Parameters(0.0, 0.6, 0.3), 100)
    state = iterate(mccormick, 40, state)
    state.report("McCormick")
    print "f(-.54719, -1.54719) : %.16f" % (mccormick([-.54719, -1.54719]))

    print

    state = psoInit([0.0, 0.0], [math.pi, math.pi], Parameters(0.3, 0.3, 0.3), 1000)
    state = iterate(michalewicz, 30, state)
    state.report("Michalewicz (2D)")
    print "f(2.20, 1.57)        : %.16f" % (michalewicz([2.2, 1.57]))

main()
```

```txt
Test Function : McCormick
Iterations    : 40
Global Best Position : [-0.5471069930124911, -1.5471582891466962]
Global Best Value    : -1.9132229450518705
f(-.54719, -1.54719) : -1.9132229548822739

Test Function : Michalewicz (2D)
Iterations    : 30
Global Best Position : [2.2029052187108036, 0.9404640520657541]
Global Best Value    : -0.8013034100970750
f(2.20, 1.57)        : -0.8011663878202856
```



## Racket


```racket
#lang racket/base
(require racket/list racket/math)

(define (unbox-into-cycle s)
  (if (box? s) (in-cycle (in-value (unbox s))) s))

;; Tries to "maximise" function > (so if you want a minimum, set #:> to <, IYSWIM)
(define (PSO f particles iterations hi lo #:ω ω #:φ_p φ_p #:φ_g φ_g #:> (>? >))
  (define dimensions (procedure-arity f))
  (unless (exact-nonnegative-integer? dimensions)
    (raise-argument-error 'PSO "function of fixed arity" 1 f))

  (define-values (x v)
    (for/lists (x v)
      ((_ particles))
      (for/lists (xi vi)
        ((d (in-range dimensions))
         (h (unbox-into-cycle hi))
         (l (unbox-into-cycle lo)))
        (define h-l (- h l))
        (values (+ l (* (random) h-l)) (+ (- h-l) (* 2 (random) h-l))))))

  (define (particle-step x_i v_i p_i g)
    (for/lists (x_i+ v_i+)
      ((x_id (in-list x_i))
       (v_id (in-list v_i))
       (p_id (in-list p_i))
       (g_d (in-list g)))
      (define v_id+ (+ (* ω v_id)
                       (* φ_p (random) (- p_id x_id))
                       (* φ_g (random) (- g_d x_id))))
      (values (+ x_id v_id+) v_id+)))

  (define (call-f args) (apply f args))
  (define g0 (argmax call-f x))
  (define-values (_X _V _P _P. G G.)
    (for/fold ; because of g and g., we can't use for/lists
     ((X x) (V v) (P x) (P. (map call-f x)) (g g0) (g. (apply f g0)))
     ((_ iterations))
      (for/fold
       ((x+ null) (v+ null) (p+ null) (p.+ null) (g+ g) (g.+ g.))
       ((x_i (in-list X))
        (v_i (in-list V))
        (p_i (in-list P))
        (p._i (in-list P.)))
        (define-values (x_i+ v_i+) (particle-step x_i v_i p_i g+))
        (let* ((x._i+ (apply f x_i+))
               (new-p_i? (>? x._i+ p._i))
               (new-g? (>? x._i+ g.+)))
          (values (cons x_i+ x+)
                  (cons v_i+ v+)
                  (cons (if new-p_i? x_i+ p_i) p+)
                  (cons (if new-p_i? x._i+ p._i) p.+)
                  (if new-g? x_i+ g+)
                  (if new-g? x._i+ g.+))))))
  (values G G.))

(define (McCormick x1 x2)
  (+ (sin (+ x1 x2)) (sqr (- x1 x2)) (* -1.5 x1) (* 2.5 x2) 1))

(define (Michalewitz d #:m (m 10))
  (define 2m (* 2 m))
  (define /pi (/ pi))
  (define (f . xx)
    (let Σ ((s 0) (i 1) (xx xx))
      (if (null? xx)
          (- s)
          (let ((x (car xx)))
            (Σ (+ s (* (sin x) (expt (sin (* i (sqr x) /pi)) 2m))) (+ i 1) (cdr xx))))))
  (procedure-reduce-arity f d))

(displayln "McCormick [-1.993] @ (-0.54719, -1.54719)")
(PSO McCormick 1000 100 #(-1.5 -3) #(4 4)  #:ω 0 #:φ_p 0.6 #:φ_g 0.3 #:> <)
(displayln "Michalewitz 2d [-1.8013] @ (2.20, 1.57)")
(PSO (Michalewitz 2) 1000 30 (box 0) (box pi) #:ω 0.3 #:φ_p 0.3 #:φ_g 0.3 #:> <)
(displayln "Michalewitz 5d [-4.687658]")
(PSO (Michalewitz 5) 1000 30 (box 0) (box pi) #:ω 0.3 #:φ_p 0.3 #:φ_g 0.3 #:> <)
(displayln "Michalewitz 10d [-9.66015]")
(PSO (Michalewitz 10) 1000 30 (box 0) (box pi) #:ω 0.3 #:φ_p 0.3 #:φ_g 0.3 #:> <)
```

Here is a sample run, the particles roll downhill quite nicely for McCormick,
but there's a lot of space to search with the 10-dimensional Michalewitz; so
YMMV with that one!


```txt
McCormick [-1.993] @ (-0.54719, -1.54719)
'(-0.5471975539492846 -1.547197548223612)
-1.9132229549810367
Michalewitz 2d [-1.8013] @ (2.20, 1.57)
'(2.20290527060906 1.5707963523178217)
-1.8013034100975123
Michalewitz 5d [-4.687658]
'(2.188617053067511
  1.571283730996248
  1.2884975345181757
  1.9194689579781514
  1.7202092563763838)
-4.680722049442259
Michalewitz 10d [-9.66015]
'(1.359756739301337
  2.7216986742916007
  1.2823734619604734
  1.097509491839529
  2.2225042675789752
  0.9162856379217913
  1.8753760783453128
  0.7909979596555162
  0.46574677476493
  1.8558804696523914)
-6.432092623300999
```



## REXX

This REXX version uses a large   ''numeric digits''   (the number of decimal digits in pi),   but only displays '''25''' digits.

Classic REXX doesn't have a   '''sine'''   function,   so a RYO version is included here.

The numeric precision is only limited to the number of decimal digits defined in the   <big> '''pi''' </big>   variable   (in this case,   '''110''').

This REXX version supports the specifying of   '''X''',   '''Y''',   and   '''D''',   as well as the number of particles,   and the number of

decimal digits to be displayed.   A little extra code was added to show a title and align the output columns.

The refinement loop is stopped when the calculation of the function value stabilizes.

Note that REXX uses decimal floating point, not binary.

```rexx
/*REXX program calculates Particle Swarm Optimization as it migrates through a solution.*/
numeric digits length( pi() )   -  1             /*use the number of decimal digs in pi.*/
parse arg  x  y  d  #part  sDigs  .              /*obtain optional arguments from the CL*/
if     x==''  |     x==","  then     x=   -0.5   /*Not specified?  Then use the default.*/
if     y==''  |     y==","  then     y=   -1.5   /* "      "         "   "   "     "    */
if     d==''  |     d==","  then     d=    1     /* "      "         "   "   "     "    */
if #part==''  | #part==","  then #part= 1e12     /* "      "         "   "   "     "    */
if sDigs==''  | sDigs==","  then sDigs=   25     /* "      "         "   "   "     "    */
old=                                             /*#part:   1e12  ≡  is one trillion.   */
minF= #part                                      /*the minimum for the function (#part).*/
show= sDigs + 3                                  /*adjust number decimal digits for show*/
say "══iteration══"   center('X',show,"═")    center('Y',show,"═")    center('D',show,"═")
#= 0                                             /*the number of iterations for  REFINE.*/
call refine x,y                                               /* [↓]  same as ÷ by five.*/
                do  until refine(minX, minY)     /*perform until the mix is  "refined". */
                d=d  * .2                        /*decrease the difference in the mix. .*/
                end   /*until*/                  /* [↑]  stop refining if no difference.*/
say
$= 15   +   show * 2                             /*compute the indentation for alignment*/
say right('The global minimum for  f(-.54719, -1.54719)  ───► ', $)    fmt(f(-.54719, -1.54719))
say right('The published global minimum is:'                   , $)    fmt(  -1.9133           )
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
refine: parse arg xx,yy;                         h= d * .5         /*compute ½ distance.*/
                  do   x=xx-d  to xx+d  by h
                    do y=yy-d  to yy+d  by h;    f= f(x, y);    if f>=minF  then iterate
                    new= fmt(x)   fmt(y)   fmt(f);              if new=old  then return 1
                    #= # + 1                                       /*bump # iterations. */
                    say center(#,13)  new                          /*show "      "      */
                    minF= f;    minX= x;     minY= y;    old= new  /*assign new values. */
                    end   /*y*/
                  end     /*x*/
        return 0
/*──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────*/
f:   procedure:  parse arg a,b;                     return  sin(a+b)  +  (a-b)**2  -  1.5*a  +  2.5*b  +  1
fmt: ?=format(arg(1), , sDigs);    L=length(?);     if pos(., ?)\==0  then ?=strip( strip(?, 'T', 0), "T", .);   return left(?, L)
pi:  pi=3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865; return pi
r2r: return arg(1) // ( pi() * 2)                         /*normalize radians  ───►  a unit circle.*/
sin: procedure; arg x; x= r2r(x);  z= x;  xx=x*x;   do k=2  by 2  until p=z;  p=z;  x= -x*xx/ (k*(k+1));  z= z+x;  end;  return z
```

```txt

══iteration══ ═════════════X══════════════ ═════════════Y══════════════ ═════════════D══════════════
      1       -1.5                         -2.5                         -1.2431975046920717486273609
      2       -1                           -2                           -1.6411200080598672221007448
      3       -0.5                         -1.5                         -1.9092974268256816953960199
      4       -0.54                        -1.54                        -1.9131329795075164948766768
      5       -0.548                       -1.548                       -1.9132218400165267634506035
      6       -0.548                       -1.5472                      -1.9132220344928294065568196
      7       -0.5472                      -1.5472                      -1.9132229549706499208388746
      8       -0.5472                      -1.54719872                  -1.9132229549737311254290577
      9       -0.54719872                  -1.54719872                  -1.9132229549786702369612333
     10       -0.54719872                  -1.54719744                  -1.91322295497891365438682
     11       -0.54719744                  -1.54719744                  -1.9132229549810149766572388
     12       -0.5471975424                -1.5471975424                -1.9132229549810362588916172
     13       -0.54719755264               -1.54719755264               -1.9132229549810363893093655
     14       -0.547197550592              -1.547197550592              -1.9132229549810363922848065
     15       -0.5471975514112             -1.5471975514112             -1.9132229549810363928381695
     16       -0.5471975510016             -1.5471975510016             -1.9132229549810363928520779
     17       -0.54719755116544            -1.54719755116544            -1.9132229549810363929162561
     18       -0.547197551198208           -1.547197551198208           -1.9132229549810363929179331
     19       -0.547197551198208           -1.54719755119755264         -1.9132229549810363929179344
     20       -0.54719755119755264         -1.54719755119755264         -1.9132229549810363929179361
     21       -0.54719755119755264         -1.54719755119689728         -1.9132229549810363929179365
     22       -0.54719755119689728         -1.54719755119689728         -1.9132229549810363929179375
     23       -0.54719755119689728         -1.547197551196766208        -1.9132229549810363929179375
     24       -0.547197551196766208        -1.547197551196766208        -1.9132229549810363929179376
     25       -0.547197551196766208        -1.547197551196635136        -1.9132229549810363929179376
     26       -0.547197551196635136        -1.547197551196635136        -1.9132229549810363929179376
     27       -0.547197551196635136        -1.5471975511966089216       -1.9132229549810363929179376
     28       -0.5471975511966089216       -1.5471975511966089216       -1.9132229549810363929179376
     29       -0.5471975511966089216       -1.54719755119660367872      -1.9132229549810363929179376
     30       -0.54719755119660367872      -1.54719755119660367872      -1.9132229549810363929179376
     31       -0.54719755119660367872      -1.54719755119659843584      -1.9132229549810363929179376
     32       -0.54719755119659843584      -1.54719755119659843584      -1.9132229549810363929179376
     33       -0.547197551196597387264     -1.547197551196597387264     -1.9132229549810363929179376
     34       -0.5471975511965978066944    -1.5471975511965978066944    -1.9132229549810363929179376
     35       -0.5471975511965978066944    -1.54719755119659776475136   -1.9132229549810363929179376
     36       -0.54719755119659776475136   -1.54719755119659776475136   -1.9132229549810363929179376
     37       -0.54719755119659776475136   -1.547197551196597756362752  -1.9132229549810363929179376
     38       -0.547197551196597756362752  -1.547197551196597756362752  -1.9132229549810363929179376
     39       -0.547197551196597756362752  -1.547197551196597747974144  -1.9132229549810363929179376
     40       -0.547197551196597747974144  -1.547197551196597747974144  -1.9132229549810363929179376
     41       -0.547197551196597747974144  -1.5471975511965977462964224 -1.9132229549810363929179376
     42       -0.5471975511965977462964224 -1.5471975511965977462964224 -1.9132229549810363929179376
     43       -0.5471975511965977462964224 -1.5471975511965977462293135 -1.9132229549810363929179376
     44       -0.5471975511965977462293135 -1.5471975511965977462293135 -1.9132229549810363929179376
     45       -0.5471975511965977462293135 -1.5471975511965977461622047 -1.9132229549810363929179376
     46       -0.5471975511965977461622047 -1.5471975511965977461622047 -1.9132229549810363929179376
     47       -0.5471975511965977461487829 -1.5471975511965977461487829 -1.9132229549810363929179376
     48       -0.5471975511965977461541516 -1.5471975511965977461541516 -1.9132229549810363929179376
     49       -0.547197551196597746154259  -1.547197551196597746154259  -1.9132229549810363929179376
     50       -0.547197551196597746154259  -1.5471975511965977461542375 -1.9132229549810363929179376
     51       -0.5471975511965977461542375 -1.5471975511965977461542375 -1.9132229549810363929179376
     52       -0.5471975511965977461542375 -1.547197551196597746154216  -1.9132229549810363929179376
     53       -0.547197551196597746154216  -1.547197551196597746154216  -1.9132229549810363929179376
     54       -0.547197551196597746154216  -1.5471975511965977461542152 -1.9132229549810363929179376
     55       -0.5471975511965977461542152 -1.5471975511965977461542152 -1.9132229549810363929179376
     56       -0.5471975511965977461542152 -1.5471975511965977461542143 -1.9132229549810363929179376
     57       -0.5471975511965977461542143 -1.5471975511965977461542143 -1.9132229549810363929179376
     58       -0.5471975511965977461542145 -1.5471975511965977461542145 -1.9132229549810363929179376

                    The global minimum for  f(-.54719, -1.54719)  ───►  -1.9132229548822735814541188
                                       The published global minimum is: -1.9133

```

Output note:   the published global minimum (referenced above, as well as the function's arguments) can be found at:
:::::   <u>[http://www.sfu.ca/~ssurjano/mccorm.html http://www.sfu.ca/~ssurjano/mccorm.html]</u>



