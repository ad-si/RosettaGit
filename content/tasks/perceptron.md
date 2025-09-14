+++
title = "Perceptron"
description = ""
date = 2019-07-20T19:22:27Z
aliases = []
[extra]
id = 20857
[taxonomies]
categories = ["Machine learning", "Neural networks", "AI", "task"]
tags = []
languages = [
  "forth",
  "go",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lua",
  "pascal",
  "phix",
  "racket",
  "rexx",
  "scala",
  "scheme",
  "smalltalk",
  "xlisp",
  "zkl",
]
+++

## Task

A [[wp:Perceptron|perceptron]] is an algorithm  used in machine-learning. It's the simplest of all neural networks, consisting of only one neuron, and is typically used for pattern recognition.

A perceptron attempts to separate input into a positive and a negative class with the aid of a linear function. The inputs are each multiplied by weights, random weights at first, and then summed. Based on the sign of the sum a decision is made.

In order for the perceptron to make the right decision, it needs to train with input for which the ''correct outcome is known'', so that the weights can slowly be adjusted until they start producing the desired results.


### Requirements
The website [http://natureofcode.com/book/chapter-10-neural-networks/ The Nature of Code] demonstrates a perceptron by making it perform a very simple task : determine if a randomly chosen point (x, y) is above or below a line:
  y = mx + b

Implement this perceptron and display an image (or some other visualization) of the result.


## See also
* [http://natureofcode.com/book/chapter-10-neural-networks/ Neural networks (nature of code)]
* [https://youtu.be/dXuNAkHsos4?t=16m44s Machine Learning - Perceptrons (youtube)]





## Forth

{{works with|GNU Forth}}
Where it says <code>[email protected]</code> it should say <code>f&#64;</code>.

```Forth
require random.fs
here seed !

warnings off

( THE PERCEPTRON )

: randomWeight      2000 random 1000 - s>f 1000e f/ ;
: createPerceptron  create  dup ,  0 ?DO randomWeight f, LOOP ;

variable arity
variable ^weights
variable ^inputs

: perceptron!       dup @ arity !  cell+ ^weights ! ;
: inputs!           ^inputs ! ;

0.0001e fconstant learningConstant
: activate          0e f> IF 1e ELSE -1e THEN ;

: feedForward
    ^weights @  ^inputs @  0e
    arity @  0  ?DO
        dup f@  float + swap
        dup f@  float + swap
        f* f+
    LOOP 2drop activate ;

: train
    feedForward f- learningConstant f*
    ^weights @  ^inputs @
    arity @  0  ?DO
        fdup  dup f@ f*  float + swap
        dup f@ f+  dup f!  float + swap
    LOOP 2drop fdrop ;

( THE TRAINER )

create point   0e f, 0e f, 1e f,   \ x y bias

: x             point ;
: y             point float + ;
: randomX       640 random s>f ;
: randomY       360 random s>f ;

\ y = Ax + B
2e fconstant A
1e fconstant B

: randomizePoint
    randomY fdup y f!
    randomX fdup x f!
    A f* B f+ f<  IF -1e ELSE 1e THEN ;

3 createPerceptron myPerceptron
variable trainings
10000 constant #rounds

: setup         0 ;  \ success counter
: calculate     s>f  #rounds s>f  f/ 100e f* ;
: report        ." After " trainings @ . ." trainings: "
                calculate f. ." % accurate" cr ;
: check         learningConstant f~ IF 1+ THEN ;
: evaluate      randomizePoint feedForward check ;
: evaluate      setup #rounds 0 ?DO evaluate LOOP report ;

: tally         1 trainings +! ;
: timesTrain    0 ?DO randomizePoint train tally LOOP ;

: initialize
    myPerceptron perceptron!
    point inputs!
    0 trainings ! ;
: go
        initialize evaluate
      1 timesTrain evaluate
      1 timesTrain evaluate
      1 timesTrain evaluate
      1 timesTrain evaluate
      1 timesTrain evaluate
      5 timesTrain evaluate
     10 timesTrain evaluate
     30 timesTrain evaluate
     50 timesTrain evaluate
    100 timesTrain evaluate
    300 timesTrain evaluate
    500 timesTrain evaluate ;

go bye
```

Example output:

```txt
After 0 trainings: 10.16 % accurate
After 1 trainings: 7.43 % accurate
After 2 trainings: 7.71 % accurate
After 3 trainings: 4.93 % accurate
After 4 trainings: 3.11 % accurate
After 5 trainings: 0.6 % accurate
After 10 trainings: 48.72 % accurate
After 20 trainings: 85.55 % accurate
After 50 trainings: 86.36 % accurate
After 100 trainings: 98.59 % accurate
After 200 trainings: 98.84 % accurate
After 500 trainings: 95.86 % accurate
After 1000 trainings: 99.8 % accurate
```



## Go

{{libheader|Go Graphics}}


This is based on the Java entry but just outputs the final image (as a .png file) rather than displaying its gradual build up. It also uses a different color scheme - blue and red circles with a black dividing line.

```go
package main

import (
    "github.com/fogleman/gg"
    "math/rand"
    "time"
)

const c = 0.00001

func linear(x float64) float64 {
    return x*0.7 + 40
}

type trainer struct {
    inputs []float64
    answer int
}

func newTrainer(x, y float64, a int) *trainer {
    return &trainer{[]float64{x, y, 1}, a}
}

type perceptron struct {
    weights  []float64
    training []*trainer
}

func newPerceptron(n, w, h int) *perceptron {
    weights := make([]float64, n)
    for i := 0; i < n; i++ {
        weights[i] = rand.Float64()*2 - 1
    }

    training := make([]*trainer, 2000)
    for i := 0; i < 2000; i++ {
        x := rand.Float64() * float64(w)
        y := rand.Float64() * float64(h)
        answer := 1
        if y < linear(x) {
            answer = -1
        }
        training[i] = newTrainer(x, y, answer)
    }
    return &perceptron{weights, training}
}

func (p *perceptron) feedForward(inputs []float64) int {
    if len(inputs) != len(p.weights) {
        panic("weights and input length mismatch, program terminated")
    }
    sum := 0.0
    for i, w := range p.weights {
        sum += inputs[i] * w
    }
    if sum > 0 {
        return 1
    }
    return -1
}

func (p *perceptron) train(inputs []float64, desired int) {
    guess := p.feedForward(inputs)
    err := float64(desired - guess)
    for i := range p.weights {
        p.weights[i] += c * err * inputs[i]
    }
}

func (p *perceptron) draw(dc *gg.Context, iterations int) {
    le := len(p.training)
    for i, count := 0, 0; i < iterations; i, count = i+1, (count+1)%le {
        p.train(p.training[count].inputs, p.training[count].answer)
    }
    x := float64(dc.Width())
    y := linear(x)
    dc.SetLineWidth(2)
    dc.SetRGB255(0, 0, 0) // black line
    dc.DrawLine(0, linear(0), x, y)
    dc.Stroke()
    dc.SetLineWidth(1)
    for i := 0; i < le; i++ {
        guess := p.feedForward(p.training[i].inputs)
        x := p.training[i].inputs[0] - 4
        y := p.training[i].inputs[1] - 4
        if guess > 0 {
            dc.SetRGB(0, 0, 1) // blue circle
        } else {
            dc.SetRGB(1, 0, 0) // red circle
        }
        dc.DrawCircle(x, y, 8)
        dc.Stroke()
    }
}

func main() {
    rand.Seed(time.Now().UnixNano())
    w, h := 640, 360
    perc := newPerceptron(3, w, h)
    dc := gg.NewContext(w, h)
    dc.SetRGB(1, 1, 1) // white background
    dc.Clear()
    perc.draw(dc, 2000)
    dc.SavePNG("perceptron.png")
}
```



## Java

{{works with|Java|8}}

```java
import java.awt.*;
import java.awt.event.ActionEvent;
import java.util.*;
import javax.swing.*;
import javax.swing.Timer;

public class Perceptron extends JPanel {

    class Trainer {
        double[] inputs;
        int answer;

        Trainer(double x, double y, int a) {
            inputs = new double[]{x, y, 1};
            answer = a;
        }
    }

    Trainer[] training = new Trainer[2000];
    double[] weights;
    double c = 0.00001;
    int count;

    public Perceptron(int n) {
        Random r = new Random();
        Dimension dim = new Dimension(640, 360);
        setPreferredSize(dim);
        setBackground(Color.white);

        weights = new double[n];
        for (int i = 0; i < weights.length; i++) {
            weights[i] = r.nextDouble() * 2 - 1;
        }

        for (int i = 0; i < training.length; i++) {
            double x = r.nextDouble() * dim.width;
            double y = r.nextDouble() * dim.height;

            int answer = y < f(x) ? -1 : 1;

            training[i] = new Trainer(x, y, answer);
        }

        new Timer(10, (ActionEvent e) -> {
            repaint();
        }).start();
    }

    private double f(double x) {
        return x * 0.7 + 40;
    }

    int feedForward(double[] inputs) {
        assert inputs.length == weights.length : "weights and input length mismatch";

        double sum = 0;
        for (int i = 0; i < weights.length; i++) {
            sum += inputs[i] * weights[i];
        }
        return activate(sum);
    }

    int activate(double s) {
        return s > 0 ? 1 : -1;
    }

    void train(double[] inputs, int desired) {
        int guess = feedForward(inputs);
        double error = desired - guess;
        for (int i = 0; i < weights.length; i++) {
            weights[i] += c * error * inputs[i];
        }
    }

    @Override
    public void paintComponent(Graphics gg) {
        super.paintComponent(gg);
        Graphics2D g = (Graphics2D) gg;
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                RenderingHints.VALUE_ANTIALIAS_ON);

        // we're drawing upside down
        int x = getWidth();
        int y = (int) f(x);
        g.setStroke(new BasicStroke(2));
        g.setColor(Color.orange);
        g.drawLine(0, (int) f(0), x, y);

        train(training[count].inputs, training[count].answer);
        count = (count + 1) % training.length;

        g.setStroke(new BasicStroke(1));
        g.setColor(Color.black);
        for (int i = 0; i < count; i++) {
            int guess = feedForward(training[i].inputs);

            x = (int) training[i].inputs[0] - 4;
            y = (int) training[i].inputs[1] - 4;

            if (guess > 0)
                g.drawOval(x, y, 8, 8);
            else
                g.fillOval(x, y, 8, 8);
        }
    }

    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            JFrame f = new JFrame();
            f.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            f.setTitle("Perceptron");
            f.setResizable(false);
            f.add(new Perceptron(3), BorderLayout.CENTER);
            f.pack();
            f.setLocationRelativeTo(null);
            f.setVisible(true);
        });
    }
}
```



## JavaScript

Uses P5 lib.

```javascript

const EPOCH = 1500, TRAINING = 1, TRANSITION = 2, SHOW = 3;

var perceptron;
var counter = 0;
var learnRate = 0.02;
var state = TRAINING;

function setup() {
    createCanvas( 800, 600 );
    clearBack();
    perceptron = new Perceptron( 2 );
}

function draw() {
    switch( state ) {
        case TRAINING: training(); break;
        case TRANSITION: transition(); break;
        case SHOW: show(); break;
    }
}

function clearBack() {
    background( 0 );
    stroke( 255 );
    strokeWeight( 4 );

    var x = width;
    line( 0, 0, x, lineDef( x ) );
}

function transition() {
    clearBack();
    state = SHOW;
}

function lineDef( x ) {
    return .75 * x;
}

function training() {
    var a = random( width ),
        b = random( height );

    lDef = lineDef( a ) > b ? -1 : 1;

    perceptron.setInput( [a, b] );
    perceptron.feedForward();
    var pRes = perceptron.getOutput();
    var match = (pRes == lDef);
    var clr;

    if( !match ) {
        var err = ( pRes - lDef ) * learnRate;
        perceptron.adjustWeights( err );

        clr = color( 255, 0, 0 );

    } else {
        clr = color( 0, 255, 0 );
    }

    noStroke();
    fill( clr );
    ellipse( a, b, 4, 4 );

    if( ++counter == EPOCH ) state = TRANSITION;
}

function show() {
    var a = random( width ),
        b = random( height ),
        clr;

    perceptron.setInput( [a, b] );
    perceptron.feedForward();
    var pRes = perceptron.getOutput();

    if( pRes < 0 )
        clr = color( 255, 0, 0 );
    else 
        clr = color( 0, 255, 0 );

    noStroke();
    fill( clr );
    ellipse( a, b, 4, 4 );
}

function Perceptron( inNumber ) {
    this.inputs = [];
    this.weights = [];
    this.output;
    this.bias = 1;
    
    // one more weight for bias
    for( var i = 0; i < inNumber + 1; i++ ) {
        this.weights.push( Math.random() );
    };

    this.activation = function( a ) {
        return( Math.tanh( a ) < .5 ? 1 : -1 );
    }

    this.feedForward = function() {
        var sum = 0;
        for( var i = 0; i < this.inputs.length; i++ ) {
            sum += this.inputs[i] * this.weights[i];
        }

        sum += this.bias * this.weights[this.weights.length - 1];

        this.output = this.activation( sum );
    }

    this.getOutput = function() {
        return this.output;
    }

    this.setInput= function( inputs ) {
        this.inputs = [];
        for( var i = 0; i < inputs.length; i++ ) {
            this.inputs.push( inputs[i] );
        }
    }

    this.adjustWeights = function( err ) {
        for( var i = 0; i < this.weights.length - 1; i++ ) {
            this.weights[i] += err * this.inputs[i];
        }
    }
}

```

[[File:perceptronJS.png]]

Well, it seems I cannot upload an image :(


## Julia


```julia
# file module.jl

module SimplePerceptrons

# default activation function
step(x) = x > 0 ? 1 : -1

mutable struct Perceptron{T, F}
    weights::Vector{T}
    lr::T
    activate::F
end

Perceptron{T}(n::Integer, lr = 0.01, f::Function = step) where T =
    Perceptron{T, typeof(f)}(2 .* rand(n + 1) .- 1, lr, f)
Perceptron(args...) = Perceptron{Float64}(args...)

@views predict(p::Perceptron, x::AbstractVector) = p.activate(p.weights[1] + x' * p.weights[2:end])
@views predict(p::Perceptron, X::AbstractMatrix) = p.activate.(p.weights[1] .+ X * p.weights[2:end])

function train!(p::Perceptron, X::AbstractMatrix, y::AbstractVector; epochs::Integer = 100)
    for _ in Base.OneTo(epochs)
        yhat = predict(p, X)
        err = y .- yhat
        ΔX = p.lr .* err .* X
        for ind in axes(ΔX, 1)
            p.weights[1] += err[ind]
            p.weights[2:end] .+= ΔX[ind, :]
        end
    end
    return p
end

accuracy(p, X::AbstractMatrix, y::AbstractVector) = count(y .== predict(p, X)) / length(y)

end  # module SimplePerceptrons

```



```julia
# file _.jl

const SP = include("module.jl")

p = SP.Perceptron(2, 0.1)

a, b = 0.5, 1
X = rand(1000, 2)
y = map(x -> x[2] > a + b * x[1] ? 1 : -1, eachrow(X))

# Accuracy
@show SP.accuracy(p, X, y)

# Train
SP.train!(p, X, y, epochs = 1000)

ahat, bhat = p.weights[1] / p.weights[2], -p.weights[3] / p.weights[2]

using Plots

scatter(X[:, 1], X[:, 2], markercolor = map(x -> x == 1 ? :red : :blue, y))
Plots.abline!(b, a, label = "real line", linecolor = :red, linewidth = 2)

SP.train!(p, X, y, epochs = 1000)
ahat, bhat = p.weights[1] / p.weights[2], -p.weights[3] / p.weights[2]
Plots.abline!(bhat, ahat, label = "predicted line")

```



## Kotlin

{{trans|Java}}

```scala
// version 1.1.4-3

import java.awt.*
import java.awt.event.ActionEvent
import java.util.Random
import javax.swing.JPanel
import javax.swing.JFrame
import javax.swing.Timer
import javax.swing.SwingUtilities

class Perceptron(n: Int) : JPanel() {

    class Trainer(x: Double, y: Double, val answer: Int) {
        val inputs = doubleArrayOf(x, y, 1.0)
    }

    val weights: DoubleArray
    val training: Array<Trainer>
    val c = 0.00001
    var count = 0

    init {
        val r = Random()
        val dim = Dimension(640, 360)
        preferredSize = dim
        background = Color.white
        weights = DoubleArray(n) { r.nextDouble() * 2.0 - 1.0 }
        training = Array(2000) {
            val x = r.nextDouble() * dim.width
            val y = r.nextDouble() * dim.height
            val answer = if (y < f(x)) -1 else 1
            Trainer(x, y, answer)
        }
        Timer(10) { repaint() }.start()
    }

    private fun f(x: Double) = x * 0.7 + 40.0

    fun feedForward(inputs: DoubleArray): Int {
        if (inputs.size != weights.size)
            throw IllegalArgumentException("Weights and input length mismatch")
        val sum = weights.zip(inputs) { w, i -> w * i }.sum()
        return activate(sum)
    }

    fun activate(s: Double) = if (s > 0.0) 1 else -1

    fun train(inputs: DoubleArray, desired: Int) {
        val guess = feedForward(inputs)
        val error = desired - guess
        for (i in 0 until weights.size) weights[i] += c * error * inputs[i]
    }

    public override fun paintComponent(gg: Graphics) {
        super.paintComponent(gg)
        val g = gg as Graphics2D
        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                           RenderingHints.VALUE_ANTIALIAS_ON)

        // we're drawing upside down
        var x = width
        var y = f(x.toDouble()).toInt()
        g.stroke = BasicStroke(2.0f)
        g.color = Color.orange
        g.drawLine(0, f(0.0).toInt(), x, y)

        train(training[count].inputs, training[count].answer)
        count = (count + 1) % training.size

        g.stroke = BasicStroke(1.0f)
        g.color = Color.black
        for (i in 0 until count) {
            val guess = feedForward(training[i].inputs)
            x = training[i].inputs[0].toInt() - 4
            y = training[i].inputs[1].toInt() - 4 
            if (guess > 0) g.drawOval(x, y, 8, 8)
            else g.fillOval(x, y, 8, 8)
        }
    }
}

fun main(args: Array<String>) {
    SwingUtilities.invokeLater {
        val f = JFrame()
        with(f) {
            defaultCloseOperation = JFrame.EXIT_ON_CLOSE
            title = "Perceptron"
            isResizable = false
            add(Perceptron(3), BorderLayout.CENTER)
            pack()
            setLocationRelativeTo(null)
            isVisible = true
        }
    }
}
```



## Lua

Simple implementation allowing for any number of inputs (in this case, just 1), testing of the Perceptron, and training.

```lua
local Perceptron = {}
Perceptron.__index = Perceptron

function Perceptron.new(numInputs)
    local cell = {}
    setmetatable(cell, Perceptron)

    cell.weights = {}
    cell.bias = math.random()
    cell.output = 0

    for i = 1, numInputs do
        cell.weights[i] = math.random()
    end

    return cell
end

--used in both training and testing, calculates the output from inputs and weights
function Perceptron:update(inputs)
    local sum = self.bias
    for i = 1, #inputs do
        sum = sum + self.weights[i] * inputs[i]
    end
    self.output = sum
end

--returns the output from a given table of inputs
function Perceptron:test(inputs)
    self:update(inputs)
    return self.output
end

--used in training to adjust the weights and bias
function Perceptron:optimize(stepSize)
    local gradient = self.delta * self.output
    for i = 1, #self.weights do
        self.weights[i] = self.weights[i] + (stepSize*gradient)
    end
    self.bias = self.bias + (stepSize*self.delta)
end

--takes a table of training data, the number of iterations (or epochs) to train over, and the step size for training
function Perceptron:train(data, iterations, stepSize)
    for i = 1, iterations do
        for j = 1, #data do
            local datum = data[j]
            self:update(datum[1])
            self.delta = datum[2] - self.output
            self:optimize(stepSize)
        end
    end
end

local node = Perceptron.new(1) --creates a new Perceptron that takes in 1 input
local trainingData = {} --this Perceptron will be trained on the function y=2x+1
print("Untrained results:")
for i = -2, 2, 1 do
    print(i..":", node:test({i}))
    trainingData[i+3] = {{i},2*i+1} --the training data is a table, where each element is another table that has a table of inputs and one output
end
node:train(trainingData, 100, .1) --trains on the set for 100 epochs with a step size of 0.1
print("\nTrained results:")
for i = -2, 2, 1 do
    print(i..":", node:test({i}))
end

```

{{out}}

```txt
Untrained results:
-2: -0.55767321178784
-1: 0.1898736124016
0: 0.93742043659104
1: 1.6849672607805
2: 2.4325140849699

Trained results:
-2: -3
-1: -1
0: 1
1: 3
2: 5

```



## Pascal

This is a text-based implementation, using a 20x20 grid (just like the original Mark 1 Perceptron had). The rate of improvement drops quite markedly as you increase the number of training runs.

```pascal
program Perceptron;

(*
 * implements a version of the algorithm set out at
 * http://natureofcode.com/book/chapter-10-neural-networks/ ,
 * but without graphics
 *)

function targetOutput( a, b : integer ) : integer;
(* the function the perceptron will be learning is f(x) = 2x + 1 *)
begin
    if a * 2 + 1 < b then
        targetOutput := 1
    else
        targetOutput := -1
end;

procedure showTargetOutput;
var x, y : integer;
begin
    for y := 10 downto -9 do
    begin
        for x := -9 to 10 do
            if targetOutput( x, y ) = 1 then
                write( '#' )
            else
                write( 'O' );
        writeln
    end;
    writeln
end;

procedure randomWeights( var ws : array of real );
(* start with random weights -- NB pass by reference *)
var i : integer;
begin
    randomize; (* seed random-number generator *)
    for i := 0 to 2 do
        ws[i] := random * 2 - 1
end;

function feedForward( ins : array of integer; ws : array of real ) : integer;
(* the perceptron outputs 1 if the sum of its inputs multiplied by
its input weights is positive, otherwise -1 *)
var sum : real;
    i : integer;
begin
    sum := 0;
    for i := 0 to 2 do
        sum := sum + ins[i] * ws[i];
    if sum > 0 then
        feedForward := 1
    else
        feedForward := -1
end;

procedure showOutput( ws : array of real );
var inputs : array[0..2] of integer;
    x, y : integer;
begin
    inputs[2] := 1; (* bias *)
    for y := 10 downto -9 do
    begin
        for x := -9 to 10 do
        begin
            inputs[0] := x;
            inputs[1] := y;
            if feedForward( inputs, ws ) = 1 then
                write( '#' )
            else
                write( 'O' )
        end;
        writeln
    end;
    writeln
end;

procedure train( var ws : array of real; runs : integer );
(* pass the array of weights by reference so it can be modified *)
var inputs : array[0..2] of integer;
    error : real;
    x, y, i, j : integer;
begin
    inputs[2] := 1; (* bias *)
    for i := 1 to runs do
    begin
        for y := 10 downto -9 do
        begin
            for x := -9 to 10 do
            begin
                inputs[0] := x;
                inputs[1] := y;
                error := targetOutput( x, y ) - feedForward( inputs, ws );
                for j := 0 to 2 do
                    ws[j] := ws[j] + error * inputs[j] * 0.01;
                    (* 0.01 is the learning constant *)
            end;
        end;
    end;
end;

var weights : array[0..2] of real;

begin
    writeln( 'Target output for the function f(x) = 2x + 1:' );
    showTargetOutput;
    randomWeights( weights );
    writeln( 'Output from untrained perceptron:' );
    showOutput( weights );
    train( weights, 1 );
    writeln( 'Output from perceptron after 1 training run:' );
    showOutput( weights );
    train( weights, 4 );
    writeln( 'Output from perceptron after 5 training runs:' );
    showOutput( weights )
end.
```

{{out}}

```txt
Target output for the function f(x) = 2x + 1:
##############OOOOOO
#############OOOOOOO
#############OOOOOOO
############OOOOOOOO
############OOOOOOOO
###########OOOOOOOOO
###########OOOOOOOOO
##########OOOOOOOOOO
##########OOOOOOOOOO
#########OOOOOOOOOOO
#########OOOOOOOOOOO
########OOOOOOOOOOOO
########OOOOOOOOOOOO
#######OOOOOOOOOOOOO
#######OOOOOOOOOOOOO
######OOOOOOOOOOOOOO
######OOOOOOOOOOOOOO
#####OOOOOOOOOOOOOOO
#####OOOOOOOOOOOOOOO
####OOOOOOOOOOOOOOOO

Output from untrained perceptron:
OOO#################
OOOO################
OOOOO###############
OOOOO###############
OOOOOO##############
OOOOOO##############
OOOOOOO#############
OOOOOOOO############
OOOOOOOO############
OOOOOOOOO###########
OOOOOOOOO###########
OOOOOOOOOO##########
OOOOOOOOOOO#########
OOOOOOOOOOO#########
OOOOOOOOOOOO########
OOOOOOOOOOOOO#######
OOOOOOOOOOOOO#######
OOOOOOOOOOOOOO######
OOOOOOOOOOOOOO######
OOOOOOOOOOOOOOO#####

Output from perceptron after 1 training run:
###############OOOOO
###############OOOOO
##############OOOOOO
#############OOOOOOO
#############OOOOOOO
############OOOOOOOO
############OOOOOOOO
###########OOOOOOOOO
##########OOOOOOOOOO
##########OOOOOOOOOO
#########OOOOOOOOOOO
#########OOOOOOOOOOO
########OOOOOOOOOOOO
#######OOOOOOOOOOOOO
#######OOOOOOOOOOOOO
######OOOOOOOOOOOOOO
######OOOOOOOOOOOOOO
#####OOOOOOOOOOOOOOO
####OOOOOOOOOOOOOOOO
####OOOOOOOOOOOOOOOO

Output from perceptron after 5 training runs:
##############OOOOOO
#############OOOOOOO
#############OOOOOOO
############OOOOOOOO
############OOOOOOOO
###########OOOOOOOOO
###########OOOOOOOOO
##########OOOOOOOOOO
##########OOOOOOOOOO
#########OOOOOOOOOOO
#########OOOOOOOOOOO
########OOOOOOOOOOOO
########OOOOOOOOOOOO
#######OOOOOOOOOOOOO
#######OOOOOOOOOOOOO
######OOOOOOOOOOOOOO
######OOOOOOOOOOOOOO
#####OOOOOOOOOOOOOOO
#####OOOOOOOOOOOOOOO
####OOOOOOOOOOOOOOOO
```



## Phix

{{libheader|pGUI}}
Interactive GUI version. Select one of five lines, set the number of points, learning constant, 
learning rate, and max iterations. Plots accuracy vs. iterations and displays the training data
in blue/black=above/incorrect and green/red=below/incorrect [all blue/green = 100% accurate].

```Phix
-- demo\rosetta\Perceptron.exw
--
--  The learning curve turned out more haphazard than I imagined, and adding a
--  non-linear line to f() (case 5) was perhaps not such a great idea given how 
--  much it sometimes struggles with some of the other straight lines anyway.
--
include pGUI.e
--#withtype Ihandle
--#withtype Ihandles
--#withtype cdCanvas

constant help_txt = """
A perceptron is the simplest possible neural network, consisting of just one neuron
that we train to recognise whether a point is above or below a given straight line.
NB: It would probably be unwise to overly assume that this could easily be adapted 
to anything more complex, or actually useful. It is just a basic introduction, but
you have to start somewhere. What is interesting is that ultimately the neuron is
just three numbers, plus a bucket-load of training gumpf.

The left hand panel allows settings to be changed, in the middle we plot the rate of
learning, and on the right we show the training data colour coded as above/below and
correct/incorrect (blue/black=above/incorrect, green/red=below/incorrect). What you
want to see is all blue/green, with no black/red.

You can change the line algorithm (four straight and one curved that it is not meant
to be able to cope with), the number of points (size of training data), the learning 
constant, learning rate (iterations/second) and the maximum number of iterations.
Note that training automatically stops once 100% accuracy is reached (since the error
is then always zero, no further changes would ever occur). Also note that a restart
is triggered when any setting is changed, not just when the restart button is pressed.

The learning curve was expected to start at 50% (random chance of being right) and 
gradually improve towards 100%, except when the non-linear line was selected. It
turned out far more haphazard than I thought it would. Originally it allowed up to
10,000,000 iterations, but it rarely improved much beyond 1,000,000."""

function help_cb(Ihandln /*help*/)
    IupMessage("Perceptron",help_txt)
    return IUP_DEFAULT
end function

Ihandle dlg, plot, canvas, timer,
        iteration, accuracy, w1, w2, w3
cdCanvas cddbuffer, cdcanvas

integer line_alg = 1
integer points = 2000,
        learning_rate = 10000,
        max_iterations = 1_000_000,
        total_iterations = 0
atom learning_constant = 0.00001

enum WEIGHTS,   -- The actual neuron (just 3 numbers)
     TRAINING   -- training data/results, variable length
enum INPUTS, ANSWER -- contents of [TRAINING]
     -- note that length(inputs[i]) must = length(weights)

sequence perceptron = {},
         last_wh -- (recreate "" on resize)

function activate(atom t)
    return iff(t>0?+1:-1)
end function

function f(atom x)
    switch line_alg
        case 1: return x*0.7+40
        case 2: return 300-0.3*x
        case 3: return x*0.75
        case 4: return 2*x+1
        case 5: return x/2+sin(x/100)*100+100 -- (fail)
    end switch
end function

procedure new_perceptron(integer n)
    sequence weights := repeat(0, n)
    for i=1 to n do
        weights[i] = rnd()*2 - 1
    end for
    sequence training := repeat(0,points)
    integer {w,h} = last_wh
    for i=1 to points do
        integer x := rand(w),
                y := rand(h),
                answer := activate(y-f(x))
        sequence inputs = {x, y, 1}
        -- aside: inputs is {x,y,1}, rather than {x,y} because an
        --        input of {0,0} could only ever yield 0, whereas
        --        {0,0,1} can yield a non-zero guess: weights[3].
        training[i] = {inputs, answer}  -- {INPUTS, ANSWER}
    end for
    perceptron = {weights, training}  -- {WEIGHTS, TRAINING}
end procedure
 
function feed_forward(sequence inputs)
    if length(inputs)!=length(perceptron[WEIGHTS]) then
        throw("weights and input length mismatch, program terminated")
    end if
    atom total := 0.0
    for i=1 to length(inputs) do
        total += inputs[i] * perceptron[WEIGHTS][i]
    end for
    return activate(total)
end function
 
procedure train(sequence inputs, integer desired)
    integer guess := feed_forward(inputs),
            error := desired - guess
    for i=1 to length(perceptron[WEIGHTS]) do
        perceptron[WEIGHTS][i] += learning_constant * error * inputs[i]
    end for
end procedure
 
--DEV add to pGUI/doc
procedure cdCanvasCircle(cdCanvas cddbuffer, atom x, y, r)
    cdCanvasArc(cddbuffer,x,y,r,r,0,360)
end procedure

function draw(bool bDraw=true)
-- (if bDraw is false, we just want the "correct" count)
    integer correct = 0
    atom x, y
    for i=1 to points do
        {sequence inputs, integer answer} = perceptron[TRAINING][i]
        integer guess := feed_forward(inputs)
        correct += (guess=answer)
        if bDraw then
            {x,y} = inputs
            -- blue/black=above/incorrect, green/red=below/incorrect
            integer clr = iff(guess=answer?iff(guess>0?CD_BLUE:CD_GREEN)
                                          :iff(guess>0?CD_BLACK:CD_RED))
            cdCanvasSetForeground(cddbuffer, clr)
            cdCanvasCircle(cddbuffer, x, y, 8)
        end if
    end for
    if bDraw then
        cdCanvasSetForeground(cddbuffer, CD_BLACK)
        x := last_wh[1]
        y := f(x)
        if line_alg=5 then
            -- non-linear so (crudely) draw in little segments
            for i=0 to x by 20 do
                cdCanvasLine(cddbuffer,i,f(i),i+20,f(i+20))
            end for
        else
            cdCanvasLine(cddbuffer,0,f(0),x,y)
        end if
    end if
    return correct
end function
 
bool re_plot = true
atom plot0
sequence plotx = repeat(0,19),
         ploty = repeat(0,19)
integer imod = 1,   -- keep every 1, then 10, then 100, ...
        pidx = 1

function restart_cb(Ihandln /*restart*/)
    last_wh = IupGetIntInt(canvas, "DRAWSIZE")
    new_perceptron(3)
    imod = 1
    pidx = 1
    total_iterations = 0
    plot0 = (draw(false)/points)*100
    re_plot = true
    IupSetInt(timer,"RUN",1)
    return IUP_DEFAULT
end function

function redraw_cb(Ihandle /*ih*/, integer /*posx*/, integer /*posy*/)
    if perceptron={}
    or last_wh!=IupGetIntInt(canvas, "DRAWSIZE") then
        {} = restart_cb(NULL)
    end if
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)
    integer correct = draw()
    cdCanvasFlush(cddbuffer)

    if re_plot then
        re_plot = false
        IupSetAttribute(plot, "CLEAR", NULL)
        IupPlotBegin(plot)
        IupPlotAdd(plot, 0, plot0)
        for i=1 to pidx-1 do
            IupPlotAdd(plot, plotx[i], ploty[i])
        end for
        {} = IupPlotEnd(plot)
        IupSetAttribute(plot, "REDRAW", NULL)
    end if
    
    IupSetStrAttribute(iteration,"TITLE","iteration: %d",{total_iterations})
    IupSetStrAttribute(w1,"TITLE","%+f",{perceptron[WEIGHTS][1]})
    IupSetStrAttribute(w2,"TITLE","%+f",{perceptron[WEIGHTS][2]})
    IupSetStrAttribute(w3,"TITLE","%+f",{perceptron[WEIGHTS][3]})
    IupSetStrAttribute(accuracy,"TITLE","accuracy: %.4g%%",{(correct/points)*100})
    IupRefresh({iteration,w1,w2,w3,accuracy})   -- (force label resize)
    if correct=points then
        IupSetInt(timer,"RUN",0)                -- stop at 100%
    end if
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    cdcanvas = cdCreateCanvas(CD_IUP, ih)
    cddbuffer = cdCreateCanvas(CD_DBUFFER, cdcanvas)
    cdCanvasSetBackground(cddbuffer, CD_PARCHMENT)
    return IUP_DEFAULT
end function

function valuechanged_cb(Ihandle ih)
    string name = IupGetAttribute(ih, "NAME")
    integer v = IupGetInt(ih, "VALUE")
    switch name
        case "line":    line_alg = v
        case "points":  points = power(10,v)
        case "learn":   learning_constant = power(10,-v)
        case "rate":    learning_rate = power(10,v-1)
        case "max":     max_iterations = power(10,v)
    end switch
    {} = restart_cb(NULL)
    return IUP_DEFAULT
end function

function timer_cb(Ihandle /*timer*/)
    for i=1 to min(learning_rate,max_iterations) do
        total_iterations += 1
        integer c = mod(total_iterations,points)+1
        train(perceptron[TRAINING][c][INPUTS], perceptron[TRAINING][c][ANSWER])
        if mod(total_iterations,imod)=0 then
            -- save 1,2..10, then 20,30,..100, then 200,300,..1000, etc
            re_plot = true
            plotx[pidx] = total_iterations
            ploty[pidx] = (draw(false)/points)*100
            if pidx=10 or pidx=19 then
                if pidx=19 then
                    -- drop (eg) 1,2,..9, replace with 10,20,..90,
                    -- next time replace 10,20..90 with 100,200..900, etc
                    plotx[1..10] = plotx[10..19]
                    ploty[1..10] = ploty[10..19]
                end if
                imod *= 10
                pidx = 11
            else
                pidx += 1
            end if
        end if      
    end for
    if total_iterations>=max_iterations then
        IupSetInt(timer,"RUN",0)
    end if
    IupUpdate(canvas)
    return IUP_IGNORE
end function

function esc_close(Ihandle /*ih*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    if c=K_F1 then return help_cb(NULL) end if
    if c=K_F5 then return restart_cb(NULL) end if
    return IUP_CONTINUE
end function

function settings(string lname, name, sequence opts, integer v=1)
    Ihandle lbl = IupLabel(lname,"PADDING=0x4"),
            list = IupList("NAME=%s, DROPDOWN=YES",{name}),
            hbox = IupHbox({lbl,IupFill(),list})
    for i=1 to length(opts) do
        IupSetAttributeId(list,"",i,opts[i])
    end for
    IupSetInt(list,"VISIBLEITEMS",length(opts)+1)
    IupSetInt(list,"VALUE",v)
    IupSetCallback(list, "VALUECHANGED_CB", Icallback("valuechanged_cb"));
    return hbox
end function

function sep()
    return IupLabel("","SEPARATOR=HORIZONTAL")
end function

procedure main()
    IupOpen()
    IupControlsOpen()

    Ihandle settings_lbl = IupHbox({IupFill(),IupLabel("Settings"),IupFill()}),
            line = settings("line","line",{"x*0.7 + 40","300 - 0.3*x","x*0.75","2*x + 1","x/2+sin(x/100)*100+100"}),
            points = settings("number of points","points",{"10","100","1000","10000"},3),
            learn = settings("learning constant","learn",{"0.1","0.01","0.001","0.0001","0.00001"},5),
            rate = settings("learning rate","rate",{"1/s","10/s","100/s","1000/s","10000/s"},5),
            maxiter = settings("max iterations","max",{"10","100","1000","10,000","100,000","1,000,000"},6),
            restart = IupButton("Restart (F5)", "ACTION", Icallback("restart_cb")),
            helpbtn = IupButton("Help (F1)", "ACTION", Icallback("help_cb")),
            buttons = IupHbox({restart,IupFill(),helpbtn})

    iteration = IupLabel("iteration: 1")
    w1 = IupLabel("1")
    w2 = IupLabel("2")
    w3 = IupLabel("3")
    Ihandle weights = IupHbox({IupLabel("weights: ","PADDING=0x4"),IupVbox({w1,w2,w3})})
    accuracy = IupLabel("accuracy: 12.34%")

    Ihandle vbox = IupVbox({settings_lbl, sep(),
                            line, sep(), points, sep(), learn, sep(), 
                            rate, sep(), maxiter, sep(), buttons, sep(),
                            IupHbox({iteration}), weights, IupHbox({accuracy})})
    IupSetAttribute(vbox, "GAP", "4");

    plot = IupPlot("MENUITEMPROPERTIES=Yes")
    IupSetAttribute(plot, "TITLE", "Learning Curve");
    IupSetAttribute(plot, "TITLEFONTSIZE", "10");
    IupSetAttribute(plot, "TITLEFONTSTYLE", "ITALIC");
    IupSetAttribute(plot, "GRIDLINESTYLE", "DOTTED");
    IupSetAttribute(plot, "GRID", "YES");
    IupSetAttribute(plot, "AXS_XLABEL", "iterations");
    IupSetAttribute(plot, "AXS_YLABEL", "% correct");
    IupSetAttribute(plot, "AXS_XFONTSTYLE", "ITALIC");
    IupSetAttribute(plot, "AXS_YFONTSTYLE", "ITALIC");
    IupSetAttribute(plot, "AXS_XTICKNUMBER", "No");
    IupSetAttribute(plot, "AXS_YAUTOMIN", "No");
    IupSetAttribute(plot, "AXS_YAUTOMAX", "No");
    IupSetInt(plot, "AXS_YMIN", 0)
    IupSetInt(plot, "AXS_YMAX", 100)

    canvas = IupCanvas(NULL)
    IupSetAttribute(canvas, "RASTERSIZE", "640x360") -- initial size
    IupSetCallback(canvas, "MAP_CB", Icallback("map_cb"))
    IupSetCallback(canvas, "ACTION", Icallback("redraw_cb"))

    Ihandle hbox = IupHbox({vbox, plot, canvas},"MARGIN=4x4, GAP=10")
    dlg = IupDialog(hbox);
    IupSetCallback(dlg, "K_ANY", Icallback("esc_close"))
    IupSetAttribute(dlg, "TITLE", "Perceptron")
    IupMap(dlg)
    IupSetAttribute(canvas, "RASTERSIZE", NULL) -- release limitation
    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    timer = IupTimer(Icallback("timer_cb"), 100) -- (was 1 sec, now 0.1s)
    IupMainLoop()
    IupClose()
end procedure
main()
```



## Racket

{{trans|Java}}

```racket
#lang racket
(require 2htdp/universe
         2htdp/image)

(define (activate s) (if (positive? s) 1 -1))

;; ---------------------------------------------------------------------------------------------------
;; PERCEPTRON
(define perceptron%
  (class object%
    (super-new)
    (init-field n)
    
    (field [weights (build-vector n (λ (i) (- (* (random) 2) 1)))])
    
    (define c 0.001)

    (define/public (feed-forward inputs)
      (unless (= (vector-length inputs) (vector-length weights))
        (error 'feed-forward "weights and inputs lengths mismatch"))
      (activate (for/sum ((i (in-vector inputs)) (w (in-vector weights))) (* i w))))

    (define/public (train! inputs desired)
      (let ((error (- desired (feed-forward inputs))))
        (set! weights (vector-map (λ (w i) (+ w (* c error i))) weights inputs))))))

;; ---------------------------------------------------------------------------------------------------
;; TRAINING
(struct training-data (inputs answer))

(define (make-training-data x y f)
  (training-data (vector x y 1) (activate (- (f x) y))))

;; ---------------------------------------------------------------------------------------------------
;; DEMO
(define (demo)
  (struct demonstration (p w h f i))

  (define (draw-classification-space p w h scl n)
    (for/fold ((scn (place-image (text (~a (get-field weights p)) 12 "red")
                                 (* scl (/ w 2))
                                 (* scl (/ h 2))
                                 (empty-scene (* w scl) (* h scl)))))
              ((_ (in-range n)))
      (let* ((x (* (random) w))
             (y (* (random) h))
             (guess+? (positive? (send p feed-forward (vector x y 1)))))          
        (place-image (rectangle 4 4 (if guess+? 'solid 'outline) (if guess+? 'red 'black))
                     (- (* scl x) 2) (- (* scl (- h y)) 2)
                     scn))))

  (define the-demo
    (let ((w 640/100) (h 360/100) (f (λ (x) (+ (* x 0.7) 0.8))))
      (demonstration (new perceptron% [n 3]) w h f 0)))

  (define (demo-train p w h f)
    (let ((td (make-training-data (* (random) w) (* (random) h) f)))
      (send p train! (training-data-inputs td) (training-data-answer td))))

  (define tick-handler
    (match-lambda
      [(and d (demonstration p w h f i))
       (for ((_ (in-range 100))) (demo-train p w h f))
       (struct-copy demonstration d [i (+ 100 i)])]))

  (define draw-demo (match-lambda
                      [(demonstration p w h f i)
                       (let ((scl 100))
                         (scene+line (place-image (text (~a i) 24 "magenta")
                                                  (* scl (/ w 2))
                                                  (* scl (/ h 3))
                                                  (draw-classification-space p w h scl 1000))
                                     0 (* scl (- h (f 0))) (* scl w) (* scl (- h (f w))) "red"))]))
  
  (big-bang the-demo (to-draw draw-demo) (on-tick tick-handler)))
                      
(module+ main (demo))
```


Run it and see the image for yourself, I can't get it onto RC!


## REXX

{{trans|Java}}

```rexx
/* REXX */
Call init
Call time 'R'
try=0
Call show 0
Do d=1 To dots
  x=x.d
  y=y.d
  Parse Value x y 1 with inputs.0 inputs.1 inputs.2
  answer.d=sign(y-f(x))
  Select
    When f(x)<y Then r='<'
    When f(x)>y Then r='>'
    Otherwise        r='='
    End
  training.d=x y 1 answer.d
  End
Do try=1 To tries
  Call time 'R'
  zz=0
  Do d=1 To dots
    Parse Var training.d inputs.0 inputs.1 inputs.2 answer.d
    Call train d
    Do ii=1 To d
      Parse Var training.ii inputs.0 inputs.1 inputs.2 answer.d
      guess = feedForward(d)
      End
    End
  Call show try
  End
Exit

show:
  Parse Arg run
  show=wordpos(run,'0 1' tries)>0
  If run>0 Then Say ' '
  If show Then  Say 'Point    x f(x) r    y ff ok   zz'
  zz=0
  Do d=1 To dots
    x=x.d
    y=y.d
    Parse Value x.d y.d 1 with inputs.0 inputs.1 inputs.2
    ff=format(feedForward(),2)
    Select
      When f(x)<y Then r='<'
      When f(x)>y Then r='>'
      Otherwise        r='='
      End
    If r='<' & ff=1 |,
       r='>' & ff=-1 Then Do; tag='ok'; zz=zz+1; End
                     Else tag='--'
    If show Then
     Say format(d,5) format(x,4,0) format(f(x),4,0) r format(y,4,0) right(ff,2),
                                                                    tag format(zz,4)
    End
  If show Then Say copies('-',33)
  weights=format(weights.0,2,5) format(weights.1,2,5) format(weights.2,2,5)
  Select
    When run=0 Then txt='Initial pattern'
    When run=1 Then txt='After one loop '
    Otherwise       txt='After' run 'loops'
    End
  Say left(txt,15) format(zz,4) 'points fire. weights='weights
  Return

train: Procedure Expose inputs. weights.
  desired=sign(inputs.1-f(inputs.0))
  guess  = feedForward()
  error  = desired-guess
  Do i=0 To 2
    weights.i=weights.i+0.00001*error*inputs.i
    End
  Return

f: Return arg(1)*0.7+40

nextDouble: /* random number between -1 and +1 */
  Return random(100000)/100000

feedforward: Procedure Expose inputs. weights.
  sum=0
  Do i=0 To 2
    sum=sum+inputs.i*weights.i
    End
  Return activate(sum)

activate:
  If arg(1)>0 Then Return 1
              Else Return -1

init:
  Call random 10000,10000,333 /* seed the random function */
  dots=30
  width=640
  height=360
  tries=10
  Do i=0 To 2
    weights.i=nextDouble()
    End
  Do i=1 To dots
    x.i=nextDouble()*width
    y.i=nextDouble()*height
    End
  Return
```

{{out}}

```txt
Point    x f(x) r    y ff ok   zz
    1  100  110 <  204  1 ok    1
    2  613  469 >  117  1 --    1
    3  528  409 >  125  1 --    1
    4  141  139 >  119  1 --    1
    5   32   62 <  245  1 ok    2
    6   11   48 <  336  1 ok    3
    7  435  344 >  270  1 --    3
    8  572  440 >  280  1 --    3
    9  442  350 >  141  1 --    3
   10  410  327 >  209  1 --    3
   11  290  243 <  355  1 ok    4
   12  257  220 <  260  1 ok    5
   13  235  205 >   51  1 --    5
   14  600  460 >   66  1 --    5
   15   21   55 <  182  1 ok    6
   16  197  178 >   42  1 --    6
   17  444  351 >  150  1 --    6
   18  393  315 >   87  1 --    6
   19  622  475 >  280  1 --    6
   20  436  345 >  292  1 --    6
   21  553  427 >  261  1 --    6
   22  478  374 >  264  1 --    6
   23  373  301 >  120  1 --    6
   24  527  409 >   94  1 --    6
   25  558  431 >   49  1 --    6
   26  616  471 >  358  1 --    6
   27  241  209 >   68  1 --    6
   28  365  295 >  164  1 --    6
   29  371  299 >  155  1 --    6
   30  102  112 <  220  1 ok    7
---------------------------------
Initial pattern    7 points fire. weights= 0.28732  0.50931  0.45298

Point    x f(x) r    y ff ok   zz
    1  100  110 <  204  1 ok    1
    2  613  469 >  117  1 --    1
    3  528  409 >  125  1 --    1
    4  141  139 >  119  1 --    1
    5   32   62 <  245  1 ok    2
    6   11   48 <  336  1 ok    3
    7  435  344 >  270  1 --    3
    8  572  440 >  280  1 --    3
    9  442  350 >  141  1 --    3
   10  410  327 >  209  1 --    3
   11  290  243 <  355  1 ok    4
   12  257  220 <  260  1 ok    5
   13  235  205 >   51  1 --    5
   14  600  460 >   66  1 --    5
   15   21   55 <  182  1 ok    6
   16  197  178 >   42  1 --    6
   17  444  351 >  150  1 --    6
   18  393  315 >   87  1 --    6
   19  622  475 >  280  1 --    6
   20  436  345 >  292  1 --    6
   21  553  427 >  261  1 --    6
   22  478  374 >  264  1 --    6
   23  373  301 >  120  1 --    6
   24  527  409 >   94  1 --    6
   25  558  431 >   49  1 --    6
   26  616  471 >  358  1 --    6
   27  241  209 >   68  1 --    6
   28  365  295 >  164  1 --    6
   29  371  299 >  155  1 --    6
   30  102  112 <  220  1 ok    7
---------------------------------
After one loop     7 points fire. weights= 0.08433  0.43412  0.45252

After 2 loops     16 points fire. weights=-0.10749  0.35991  0.45208

After 3 loops     26 points fire. weights=-0.18168  0.31845  0.45192

After 4 loops     28 points fire. weights=-0.20192  0.30482  0.45186

After 5 loops     29 points fire. weights=-0.20473  0.30245  0.45184

After 6 loops     29 points fire. weights=-0.20755  0.30007  0.45182

After 7 loops     29 points fire. weights=-0.21037  0.29769  0.45180

After 8 loops     29 points fire. weights=-0.21319  0.29532  0.45178

After 9 loops     29 points fire. weights=-0.21601  0.29294  0.45176

Point    x f(x) r    y ff ok   zz
    1  100  110 <  204  1 ok    1
    2  613  469 >  117 -1 ok    2
    3  528  409 >  125 -1 ok    3
    4  141  139 >  119  1 --    3
    5   32   62 <  245  1 ok    4
    6   11   48 <  336  1 ok    5
    7  435  344 >  270 -1 ok    6
    8  572  440 >  280 -1 ok    7
    9  442  350 >  141 -1 ok    8
   10  410  327 >  209 -1 ok    9
   11  290  243 <  355  1 ok   10
   12  257  220 <  260  1 ok   11
   13  235  205 >   51 -1 ok   12
   14  600  460 >   66 -1 ok   13
   15   21   55 <  182  1 ok   14
   16  197  178 >   42 -1 ok   15
   17  444  351 >  150 -1 ok   16
   18  393  315 >   87 -1 ok   17
   19  622  475 >  280 -1 ok   18
   20  436  345 >  292 -1 ok   19
   21  553  427 >  261 -1 ok   20
   22  478  374 >  264 -1 ok   21
   23  373  301 >  120 -1 ok   22
   24  527  409 >   94 -1 ok   23
   25  558  431 >   49 -1 ok   24
   26  616  471 >  358 -1 ok   25
   27  241  209 >   68 -1 ok   26
   28  365  295 >  164 -1 ok   27
   29  371  299 >  155 -1 ok   28
   30  102  112 <  220  1 ok   29
---------------------------------
After 10 loops    29 points fire. weights=-0.21883  0.29057  0.45174
```



## Scala


### Java Swing Interoperability


```Scala
import java.awt._
import java.awt.event.ActionEvent

import javax.swing._

import scala.util.Random

object Perceptron extends App {
  SwingUtilities.invokeLater(() =>

    new JFrame("Perceptron") {

      class Perceptron(val n: Int) extends JPanel {
        private val (c, dim) = (0.00001, new Dimension(640, 360))
        private val (random, training) = (new Random, Array.ofDim[Trainer](2000))
        private val weights = Array.fill(n)(random.nextDouble * 2 - 1)
        private var count = 0

        override def paintComponent(gg: Graphics): Unit = {
          var x = getWidth
          var y = f(x).toInt

          def train(inputs: Array[Double], desired: Int): Unit = {
            val guess = feedForward(inputs)
            for (i <- weights.indices) weights(i) += c * (desired - guess) * inputs(i)
          }

          def feedForward(inputs: Array[Double]) = {
            assert(inputs.length == weights.length, "weights and input length mismatch")
            var sum = 0.0
            for (i <- weights.indices) {
              sum += inputs(i) * weights(i)
            }
            if (sum > 0) 1 else -1
          }

          super.paintComponent(gg)
          val g = gg.asInstanceOf[Graphics2D]
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
          // we're drawing upside down
          g.setStroke(new BasicStroke(2))
          g.setColor(Color.orange)
          g.drawLine(0, f(0).toInt, x, y)
          train(training(count).inputs, training(count).answer)
          count = (count + 1) % training.length
          g.setStroke(new BasicStroke(1))
          g.setColor(Color.black)
          for (i <- 0 until count) {
            val guess = feedForward(training(i).inputs)
            x = training(i).inputs(0).toInt - 4
            y = training(i).inputs(1).toInt - 4
            if (guess > 0) g.drawOval(x, y, 8, 8)
            else g.fillOval(x, y, 8, 8)
          }
        }

        private def f(x: Double) = x * 0.7 + 40

        class Trainer(val x: Double, val y: Double, var answer: Int) {
          val inputs = Array[Double](x, y, 1)
        }

        for (j <- training.indices;
             x = random.nextDouble * dim.width;
             y = random.nextDouble * dim.height;
             answer = if (y < f(x)) -1 else 1
        ) training(j) = new Trainer(x, y, answer)

        new Timer(10, (e: ActionEvent) => repaint()).start()

        setBackground(Color.white)
        setPreferredSize(dim)
      }

      add(new Perceptron(3), BorderLayout.CENTER)
      pack()
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setLocationRelativeTo(null)
      setResizable(false)
      setVisible(true)
    })

}
```



## Scheme


```scheme
(import (scheme base)
        (scheme case-lambda)
        (scheme write)
        (srfi 27))      ; for random numbers

(random-source-randomize! default-random-source)

;;; Function to create a perceptron

;; num-inputs: size of input data
;; learning-rate: small number, to give rate of learning
;; returns perceptron as a function 
;;         accepting 'train data -> trains on given list of data
;;                   'test data  -> returns percent correct on given list of data
;;                   'show       -> displays the perceptron weights
;; classes assumed to be 1, -1
(define (create-perceptron num-inputs learning-rate)
  (define (make-rnd-vector n) ; rnd vector, values in [-1,1]
    (let ((result (make-vector n)))
      (do ((i 0 (+ 1 i)))
        ((= i n) result)
        (vector-set! result i (- (* 2 (random-real)) 1)))))
  (define (extended input) ; add a 1 to end of vector
    (let* ((n (vector-length input))
           (result (make-vector (+ 1 n))))
      (do ((i 0 (+ 1 i)))
        ((= i n) (vector-set! result i 1)
                 result)
        (vector-set! result i (vector-ref input i)))))
  (define (predict weights extended-input)
    (let ((sum 0))
      (vector-for-each (lambda (w i) (set! sum (+ sum (* w i))))
                       weights extended-input)
      (if (positive? sum) 1 -1)))
  ;
  (let ((weights (make-rnd-vector (+ 1 num-inputs))))
    (case-lambda ; defines a function for the perceptron
      ((key)
       (when (eq? key 'show)
         (display weights) (newline)))
      ((action data) 
       (case action
         ((train) 
          (for-each 
            (lambda (datum)
              (let* ((extended-input (extended (car datum)))
                     (error (- (cdr datum) (predict weights extended-input))))
                (set! weights (vector-map (lambda (w i) (+ w (* learning-rate error i)))
                                          weights
                                          extended-input))))
            data))
         ((test) 
          (let ((count 0))
            (for-each 
              (lambda (datum) (when (= (cdr datum) (predict weights (extended (car datum))))
                                (set! count (+ 1 count))))
              data)
            (inexact (* 100 (/ count (length data)))))))))))

;; create data: list of n ( #(input values) . target ) pairs
;; using formula y = mx + b, target based on if input above / below line 
(define (create-data m b n)
  (define (target x y)
    (let ((fx (+ b (* m x)))) 
      (if (< fx y) 1 -1)))
  (define (create-datum)
    (let ((x (random-real))
          (y (random-real)))
      (cons (vector x y) (target x y))))
  ;
  (do ((data '() (cons (create-datum) data)))
    ((= n (length data)) data)))

;; train on 5000 points, show weights and result on 1000 test points
(let* ((m 0.7)
       (b 0.2)
       (perceptron (create-perceptron 2 0.001)))
  (perceptron 'train (create-data m b 5000))
  (perceptron 'show)
  (display "Percent correct on test set: ")
  (display (perceptron 'test (create-data m b 1000)))
  (newline))

;; show performance along training stages
(let* ((m 0.7) ; gradient of target line
       (b 0.2) ; y-intercept of target line
       (train-step 1000)  ; step in training set size
       (train-stop 20000) ; largest training set size
       (test-set (create-data m b 1000)) ; create a fixed test set
       (perceptron (create-perceptron 2 0.001)))
  (do ((i train-step (+ i train-step)))
    ((> i train-stop) )
    (perceptron 'train (create-data m b train-step))
    (display (string-append "Trained on " (number->string i)
                            ", percent correct is " 
                            (number->string (perceptron 'test test-set))
                            "\n"))))
```

{{out}}

```txt
#(-0.5914540100624854 1.073343782042039 -0.29780862758499393)
Percent correct on test set: 95.4
Trained on 1000, percent correct is 18.1
Trained on 2000, percent correct is 91.1
Trained on 3000, percent correct is 98.0
Trained on 4000, percent correct is 92.5
Trained on 5000, percent correct is 98.6
Trained on 6000, percent correct is 98.6
Trained on 7000, percent correct is 98.8
Trained on 8000, percent correct is 97.8
Trained on 9000, percent correct is 99.1
Trained on 10000, percent correct is 96.0
Trained on 11000, percent correct is 98.6
Trained on 12000, percent correct is 98.2
Trained on 13000, percent correct is 99.2
Trained on 14000, percent correct is 99.4
Trained on 15000, percent correct is 99.0
Trained on 16000, percent correct is 98.8
Trained on 17000, percent correct is 97.5
Trained on 18000, percent correct is 99.8
Trained on 19000, percent correct is 99.2
Trained on 20000, percent correct is 100.0
```



## Smalltalk

{{works with|GNU Smalltalk}}

```Smalltalk
Number extend [

    activate
        [^self > 0 ifTrue: [1] ifFalse: [-1]]
]

Object subclass: Perceptron [

    | weights |

    feedForward: inputArray
        [^(self sumOfWeighted: inputArray) activate]

    train: inputArray desire: expected
        [| actual error |
        actual := self feedForward: inputArray.
        error := 0.0001 * (expected - actual).
        weights := weights
            with: inputArray
            collect: [:weight :input | weight + (error * input)]]

    sumOfWeighted: inputArray
        [^(self weighted: inputArray)
            inject: 0
            into: [:each :sum | each + sum]]

    weighted: inputArray
        [^weights
            with: inputArray
            collect: [:weight :input | weight * input]]

    Perceptron class >> new: arity
        [^self basicNew
            initialize: arity;
            yourself]

    initialize: arity
        [weights := 1
            to: arity
            collect: [:x | self randomWeight]]

    randomWeight
        [^(Random between: -1000 and: 1000) / 1000]
]

Perceptron class extend [

    | perceptron trainings input expected actual |

    evaluationSamples := 100000.

    initializeTest
        [perceptron := self new: 3.
        input := Array new: 3.
        trainings := 0.
        input at: 1 put: 1. "Bias"]

    randomizeSample
        [| x y |
        x := Random between: 0 and: 640-1.
        y := Random between: 0 and: 360-1.
        expected := (y >= (2*x+1)) ifTrue: [1] ifFalse: [-1].
        input at: 2 put: x.
        input at: 3 put: y]

    test
        [self
            initializeTest; evaluate;
            train: 1; evaluate;
            train: 1; evaluate;
            train: 1; evaluate;
            train: 1; evaluate;
            train: 1; evaluate;
            train: 5; evaluate;
            train: 10; evaluate;
            train: 30; evaluate;
            train: 50; evaluate;
            train: 100; evaluate;
            train: 300; evaluate;
            train: 500; evaluate]

    evaluate
        [| hits |
        hits := 0.
        evaluationSamples timesRepeat:
            [self randomizeSample.
            expected = (perceptron feedForward: input)
                ifTrue: [hits := hits + 1]].
        Transcript
            display: 'After ';
            display: trainings;
            display: ' trainings: ';
            display: (hits / evaluationSamples * 100) asFloat;
            display: ' % accuracy';
            nl]

    train: anInteger
        [anInteger timesRepeat:
            [self randomizeSample.
            perceptron
                train: input
                desire: expected.
            trainings := trainings + 1]]
]

Perceptron test.
```

Example output:

```txt
After 0 trainings: 14.158 % accuracy
After 1 trainings: 14.018 % accuracy
After 2 trainings: 14.19 % accuracy
After 3 trainings: 14.049 % accuracy
After 4 trainings: 14.029 % accuracy
After 5 trainings: 14.105 % accuracy
After 10 trainings: 20.39 % accuracy
After 20 trainings: 57.08 % accuracy
After 50 trainings: 92.998 % accuracy
After 100 trainings: 98.988 % accuracy
After 200 trainings: 98.055 % accuracy
After 500 trainings: 99.777 % accuracy
After 1000 trainings: 98.523 % accuracy
```



## XLISP

Like the Pascal example, this is a text-based program using a 20x20 grid. It is slightly more general, however, because it allows the function that is to be learnt and the perceptron's bias and learning constant to be passed as arguments to the <tt>trainer</tt> and <tt>perceptron</tt> objects.

```scheme
(define-class perceptron
    (instance-variables weights bias learning-constant) )
(define-method (perceptron 'initialize b lc)
    (defun random-weights (n)
        (if (> n 0)
            (cons (- (/ (random 20000) 10000) 1) (random-weights (- n 1))) ) )
    (setq weights (random-weights 3))
    (setq bias b)
    (setq learning-constant lc)
    self )
(define-method (perceptron 'value x y)
    (if (> (+ (* x (car weights)) (* y (cadr weights)) (* bias (caddr weights))) 0)
    1
    -1 ) )
(define-method (perceptron 'print-grid)
    (print-row self 10) )
(define-method (perceptron 'learn source runs)
    (defun learn-row (row)
        (defun learn-cell (cell)
            (define inputs `(,cell ,row ,bias))
            (define error (- (source 'value cell row) (self 'value cell row)))
            (defun reweight (ins ws)
                (if (car ins)
                    (cons (+ (car ws) (* error (car ins) learning-constant)) (reweight (cdr ins) (cdr ws))) ) )
            (setq weights (reweight inputs weights))
            (if (< cell 10)
                (learn-cell (+ cell 1)) ) )
        (learn-cell -9)
        (if (> row -9)
            (learn-row (- row 1)) ) )
    (do ((i 1 (+ i 1))) ((> i runs))
        (learn-row 10) ) )

(define-class trainer
    (instance-variables fn) )
(define-method (trainer 'initialize function)
    (setq fn function)
    self )
(define-method (trainer 'print-grid)
    (print-row self 10) )
(define-method (trainer 'value x y)
    (if (apply fn `(,x ,y))
        1
        -1 ) )

(defun print-row (obj row)
    (defun print-cell (cell)
        (if (= (obj 'value cell row) 1)
            (display "#")
            (display "O") )
        (if (< cell 10)
            (print-cell (+ cell 1))
            (newline) ) )
    (print-cell -9)
    (if (> row -9)
        (print-row obj (- row 1))
        (newline) ) )

(define ptron (perceptron 'new 1 0.01))

(define training (trainer 'new
    (lambda (x y) (> y (+ (* x 2) 1))) ) )

(newline)
(display "Target output for y = 2x + 1:")
(newline)
(training 'print-grid)
(display "Output from untrained perceptron:")
(newline)
(ptron 'print-grid)
(display "Output from perceptron after 1 training run:")
(newline)
(ptron 'learn training 1)
(ptron 'print-grid)
(display "Output from perceptron after 5 training runs:")
(newline)
(ptron 'learn training 4)
(ptron 'print-grid)
```

{{out}}

```txt
Target output for y = 2x + 1:
##############OOOOOO
#############OOOOOOO
#############OOOOOOO
############OOOOOOOO
############OOOOOOOO
###########OOOOOOOOO
###########OOOOOOOOO
##########OOOOOOOOOO
##########OOOOOOOOOO
#########OOOOOOOOOOO
#########OOOOOOOOOOO
########OOOOOOOOOOOO
########OOOOOOOOOOOO
#######OOOOOOOOOOOOO
#######OOOOOOOOOOOOO
######OOOOOOOOOOOOOO
######OOOOOOOOOOOOOO
#####OOOOOOOOOOOOOOO
#####OOOOOOOOOOOOOOO
####OOOOOOOOOOOOOOOO

Output from untrained perceptron:
######OOOOOOOOOOOOOO
######OOOOOOOOOOOOOO
#######OOOOOOOOOOOOO
#######OOOOOOOOOOOOO
#######OOOOOOOOOOOOO
########OOOOOOOOOOOO
########OOOOOOOOOOOO
########OOOOOOOOOOOO
#########OOOOOOOOOOO
#########OOOOOOOOOOO
#########OOOOOOOOOOO
##########OOOOOOOOOO
##########OOOOOOOOOO
##########OOOOOOOOOO
###########OOOOOOOOO
###########OOOOOOOOO
###########OOOOOOOOO
############OOOOOOOO
############OOOOOOOO
############OOOOOOOO

Output from perceptron after 1 training run:
###############OOOOO
###############OOOOO
##############OOOOOO
##############OOOOOO
#############OOOOOOO
############OOOOOOOO
############OOOOOOOO
###########OOOOOOOOO
##########OOOOOOOOOO
##########OOOOOOOOOO
#########OOOOOOOOOOO
#########OOOOOOOOOOO
########OOOOOOOOOOOO
#######OOOOOOOOOOOOO
#######OOOOOOOOOOOOO
######OOOOOOOOOOOOOO
#####OOOOOOOOOOOOOOO
#####OOOOOOOOOOOOOOO
####OOOOOOOOOOOOOOOO
####OOOOOOOOOOOOOOOO

Output from perceptron after 5 training runs:
##############OOOOOO
#############OOOOOOO
#############OOOOOOO
############OOOOOOOO
############OOOOOOOO
###########OOOOOOOOO
###########OOOOOOOOO
##########OOOOOOOOOO
##########OOOOOOOOOO
#########OOOOOOOOOOO
#########OOOOOOOOOOO
########OOOOOOOOOOOO
########OOOOOOOOOOOO
#######OOOOOOOOOOOOO
#######OOOOOOOOOOOOO
######OOOOOOOOOOOOOO
######OOOOOOOOOOOOOO
#####OOOOOOOOOOOOOOO
#####OOOOOOOOOOOOOOO
####OOOOOOOOOOOOOOOO
```



## zkl

{{trans|Java}}
Uses the PPM class from http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#zkl

```zkl
class Perceptron{
   const c=0.00001;
   var [const] W=640, H=350;
 
   fcn init(n){
      r:=(0.0).random.fp(1); // r()-->[0..1)
      var weights=n.pump(List(),'wrap(){ r()*2 - 1 }), // Float[n]
          training=(2000).pump(List,'wrap(){         // (x,y,1,answer)[2000]
             x,y,answer:=r()*W, r()*H, (if(y<f(x)) -1 or 1);
	     return(x,y,1,answer)
	  });
   }
   fcn f(x){ 0.7*x + 40 }    // a line
   fcn feedForward(xy1a){
      sum:=0.0;
      foreach i in (weights.len()){ sum+=xy1a[i]*weights[i] }
      (sum<0) and -1 or 1   // activate(sum)
   }
   fcn train(xy1a){ 
      guess,error:=feedForward(xy1a), xy1a[-1] - guess;
      foreach i in (weights.len()){ weights[i]+=c*error*xy1a[i] }
   }
}
```


```zkl
p:=Perceptron(3);
p.training.apply2(p.train);

PPM:=Import("ppm.zkl").PPM;
pixmap:=PPM(p.W+20,p.H+20,0xFF|FF|FF);

foreach xy1a in (p.training){
   guess,x,y:=p.feedForward(xy1a), 8 + xy1a[0], 8 + xy1a[1];
   color:=(if(guess>0) 0 else 0xFF|00|00);  // black or red
   pixmap.circle(x,y,8,color);
}
pixmap.writeJPGFile("perceptron.zkl.jpg");
```

{{out}}
[[File:Perceptron.zkl.jpg]]
