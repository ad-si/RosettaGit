+++
title = "Diversity prediction theorem"
description = ""
date = 2019-10-04T00:27:13Z
aliases = []
[extra]
id = 21206
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "c",
  "clojure",
  "cpp",
  "csharp",
  "go",
  "javascript",
  "jsish",
  "julia",
  "kotlin",
  "perl",
  "perl_6",
  "phix",
  "python",
  "rexx",
  "sidef",
  "typescript",
  "zkl",
]
+++

The wisdom of the crowd is the collective opinion of a group of individuals rather than that of a single expert.

Wisdom-of-the-crowds research routinely attributes the superiority of crowd averages over individual judgments to the elimination of individual noise, an explanation that assumes independence of the individual judgments from each other. Thus the crowd tends to make its best decisions if it is made up of diverse opinions and ideologies.

Scott E. Page introduced the diversity prediction theorem: "The squared error of the collective prediction equals the average squared error minus the predictive diversity". Therefore, when the diversity in a group is large, the error of the crowd is small.

- Average Individual Error: Average of the individual squared errors

- Collective Error: Squared error of the collective prediction

- Prediction  Diversity: Average squared distance from the individual predictions to the collective prediction

So, The Diversity Prediction Theorem: ''Given a crowd of predictive models''

Collective Error = Average Individual Error - Prediction Diversity

[https://en.wikipedia.org/wiki/Wisdom_of_the_crowd wikipedia]
[https://web.archive.org/web/20060830201235/http://www.cscs.umich.edu/~spage/teaching_files/modeling_lectures/MODEL5/M18predictnotes.pdf paper]

<br/><br/>


## 11l

```11l
F average_square_diff(a, predictions)
   R sum(predictions.map(x -> (x - @a) ^ 2)) / predictions.len

F diversity_theorem(truth, predictions)
   V average = sum(predictions) / predictions.len
   print(‘average-error: ’average_square_diff(truth, predictions)"\n"‘’
         ‘crowd-error:   ’((truth - average) ^ 2)"\n"‘’
         ‘diversity:     ’average_square_diff(average, predictions))

diversity_theorem(49.0, [Float(48), 47, 51])
diversity_theorem(49.0, [Float(48), 47, 51, 42])
```

```txt

average-error: 3
crowd-error:   0.111111111
diversity:     2.888888889
average-error: 14.5
crowd-error:   4
diversity:     10.5

```



## C

Accepts inputs from command line, prints out usage on incorrect invocation.

```C


#include<string.h>
#include<stdlib.h>
#include<stdio.h>

float mean(float* arr,int size){
	int i = 0;
	float sum = 0;

	while(i != size)
		sum += arr[i++];

	return sum/size;
}

float variance(float reference,float* arr, int size){
	int i=0;
	float* newArr = (float*)malloc(size*sizeof(float));

	for(;i<size;i++)
		newArr[i] = (reference - arr[i])*(reference - arr[i]);

	return mean(newArr,size);
}

float* extractData(char* str, int *len){
	float* arr;
	int i=0,count = 1;
	char* token;

	while(str[i]!=00){
		if(str[i++]==',')
			count++;
	}

	arr = (float*)malloc(count*sizeof(float));
	*len = count;

	token = strtok(str,",");

	i = 0;

	while(token!=NULL){
		arr[i++] = atof(token);
		token = strtok(NULL,",");
	}

	return arr;
}

int main(int argC,char* argV[])
{
	float* arr,reference,meanVal;
	int len;
	if(argC!=3)
		printf("Usage : %s <reference value> <observations separated by commas>");
	else{
		arr = extractData(argV[2],&len);

		reference = atof(argV[1]);

		meanVal = mean(arr,len);

		printf("Average Error : %.9f\n",variance(reference,arr,len));
		printf("Crowd Error : %.9f\n",(reference - meanVal)*(reference - meanVal));
		printf("Diversity : %.9f",variance(meanVal,arr,len));
	}

	return 0;
}

```

Invocation and Output :

```txt

C:\rosettaCode>diversityTheorem.exe 49 48,47,51
Average Error : 3.000000000
Crowd Error : 0.111110263
Diversity : 2.888888597
C:\rosettaCode>diversityTheorem.exe 49 48,47,51,42
Average Error : 14.500000000
Crowd Error : 4.000000000
Diversity : 10.500000000

```



## C++


```Cpp

#include <iostream>
#include <vector>
#include <numeric>

float sum(const std::vector<float> &array)
{
    return std::accumulate(array.begin(), array.end(), 0.0);
}

float square(float x)
{
    return x * x;
}

float mean(const std::vector<float> &array)
{
    return sum(array) / array.size();
}

float averageSquareDiff(float a, const std::vector<float> &predictions)
{
    std::vector<float> results;
    for (float x : predictions)
        results.push_back(square(x - a));
    return mean(results);
}

void diversityTheorem(float truth, const std::vector<float> &predictions)
{
    float average = mean(predictions);
    std::cout
        << "average-error: " << averageSquareDiff(truth, predictions) << "\n"
        << "crowd-error: " << square(truth - average) << "\n"
        << "diversity: " << averageSquareDiff(average, predictions) << std::endl;
}

int main() {
    diversityTheorem(49, {48,47,51});
    diversityTheorem(49, {48,47,51,42});
    return 0;
}

```

```txt

average-error: 3
crowd-error: 0.11111
diversity: 2.88889
average-error: 14.5
crowd-error: 4
diversity: 10.5

```


## C#

```c#

using System;
using System.Linq;
using System.Collections.Generic;

public class MainClass {
    static double Square(double x) => x * x;

    static double AverageSquareDiff(double a, IEnumerable<double> predictions)
        => predictions.Select(x => Square(x - a)).Average();

    static void DiversityTheorem(double truth, IEnumerable<double> predictions)
    {
        var average = predictions.Average();
        Console.WriteLine($@"average-error: {AverageSquareDiff(truth, predictions)}
crowd-error: {Square(truth - average)}
diversity: {AverageSquareDiff(average, predictions)}");
    }

    public static void Main() {
	DiversityTheorem(49, new []{48d,47,51});
    	DiversityTheorem(49, new []{48d,47,51,42});
    }
}
```

```txt

average-error: 3
crowd-error: 0.11111
diversity: 2.88889
average-error: 14.5
crowd-error: 4
diversity: 10.5

```



## Clojure

John Lawrence Aspden's code posted on [http://www.learningclojure.com/2013/08/diversity-prediction-theorem.html Diversity Prediction Theorem].

```Clojure

(defn diversity-theorem [truth predictions]
  (let [square (fn[x] (* x x))
        mean (/ (reduce + predictions) (count predictions))
        avg-sq-diff (fn[a] (/ (reduce + (for [x predictions] (square (- x a)))) (count predictions)))]
    {:average-error (avg-sq-diff truth)
     :crowd-error (square (- truth mean))
     :diversity (avg-sq-diff mean)}))

(println (diversity-theorem 49 '(48 47 51)))
(println (diversity-theorem 49 '(48 47 51 42)))

```

```txt

{:average-error 3, :crowd-error 1/9, :diversity 26/9}
{:average-error 29/2, :crowd-error 4, :diversity 21/2}

```


=={{header|Fōrmulæ}}==

In [https://wiki.formulae.org/Diversity_prediction_theorem this] page you can see the solution of this task.

Fōrmulæ programs are not textual, visualization/edition of programs is done showing/manipulating structures but not text ([http://wiki.formulae.org/Editing_F%C5%8Drmul%C3%A6_expressions more info]). Moreover, there can be multiple visual representations of the same program. Even though it is possible to have textual representation &mdash;i.e. XML, JSON&mdash; they are intended for transportation effects more than visualization and edition.

The option to show Fōrmulæ programs and their results is showing images. Unfortunately images cannot be uploaded in Rosetta Code.


## Go


```go
package main

import "fmt"

func averageSquareDiff(f float64, preds []float64) (av float64) {
    for _, pred := range preds {
        av += (pred - f) * (pred - f)
    }
    av /= float64(len(preds))
    return
}

func diversityTheorem(truth float64, preds []float64) (float64, float64, float64) {
    av := 0.0
    for _, pred := range preds {
        av += pred
    }
    av /= float64(len(preds))
    avErr := averageSquareDiff(truth, preds)
    crowdErr := (truth - av) * (truth - av)
    div := averageSquareDiff(av, preds)
    return avErr, crowdErr, div
}

func main() {
    predsArray := [2][]float64{{48, 47, 51}, {48, 47, 51, 42}}
    truth := 49.0
    for _, preds := range predsArray {
        avErr, crowdErr, div := diversityTheorem(truth, preds)
        fmt.Printf("Average-error : %6.3f\n", avErr)
        fmt.Printf("Crowd-error   : %6.3f\n", crowdErr)
        fmt.Printf("Diversity     : %6.3f\n\n", div)
    }
}
```


```txt

Average-error :  3.000
Crowd-error   :  0.111
Diversity     :  2.889

Average-error : 14.500
Crowd-error   :  4.000
Diversity     : 10.500

```



## JavaScript


### ES5


```JavaScript
'use strict';

function sum(array) {
    return array.reduce(function (a, b) {
        return a + b;
    });
}

function square(x) {
    return x * x;
}

function mean(array) {
    return sum(array) / array.length;
}

function averageSquareDiff(a, predictions) {
    return mean(predictions.map(function (x) {
        return square(x - a);
    }));
}

function diversityTheorem(truth, predictions) {
    var average = mean(predictions);
    return {
        'average-error': averageSquareDiff(truth, predictions),
        'crowd-error': square(truth - average),
        'diversity': averageSquareDiff(average, predictions)
    };
}

console.log(diversityTheorem(49, [48,47,51]))
console.log(diversityTheorem(49, [48,47,51,42]))

```

```txt

{ 'average-error': 3,
  'crowd-error': 0.11111111111111269,
  diversity: 2.888888888888889 }
{ 'average-error': 14.5, 'crowd-error': 4, diversity: 10.5 }

```



### ES6



```JavaScript
(() => {
    'use strict';

    // mean :: Num a => [a] -> b
    const mean = xs => {
        const lng = xs.length;

        return lng > 0 ? (
            xs.reduce((a, b) => a + b, 0) / lng
        ) : undefined;
    }

    // meanErrorSquared :: Num a => a -> [a] -> b
    const meanErrorSquared = (observed, predictions) =>
        mean(predictions.map(x => Math.pow(x - observed, 2)));


    // diversityValues :: Num a => a -> [a] ->
    //     {mean-Error :: b, crowd-error :: b, diversity :: b}
    const diversityValues = (observed, predictions) => {
        const predictionMean = mean(predictions);

        return {
            'mean-error': meanErrorSquared(observed, predictions),
            'crowd-error': Math.pow(observed - predictionMean, 2),
            'diversity': meanErrorSquared(predictionMean, predictions)
        };
    }


    // TEST

    // show :: a -> String
    const show = x => JSON.stringify(x, null, 2);

    return show([{
        observed: 49,
        predictions: [48, 47, 51]
    }, {
        observed: 49,
        predictions: [48, 47, 51, 42]
    }].map(x => {
        const dctData = diversityValues(x.observed, x.predictions),
            dct = {};

        return (
            Object.keys(dctData)
            .forEach(k => dct[k] = dctData[k].toPrecision(3)),
            dct
        );
    }));
})();
```


```txt
[
  {
    "mean-error": "3.00",
    "crowd-error": "0.111",
    "diversity": "2.89"
  },
  {
    "mean-error": "14.5",
    "crowd-error": "4.00",
    "diversity": "10.5"
  }
]
```



## Jsish

From Typescript entry.

```javascript
/* Diverisity Prediction Theorem, in Jsish */
"use strict";

function sum(arr:array):number {
    return arr.reduce(function(acc, cur, idx, arr) { return acc + cur; });
}

function square(x:number):number {
    return x * x;
}

function mean(arr:array):number {
    return sum(arr) / arr.length;
}

function averageSquareDiff(a:number, predictions:array):number {
    return mean(predictions.map(function(x:number):number { return square(x - a); }));
}

function diversityTheorem(truth:number, predictions:array):object {
    var average = mean(predictions);
    return {
        "average-error": averageSquareDiff(truth, predictions),
        "crowd-error": square(truth - average),
        "diversity": averageSquareDiff(average, predictions)
    };
}

;diversityTheorem(49, [48,47,51]);
;diversityTheorem(49, [48,47,51,42]);

/*
=!EXPECTSTART!=
diversityTheorem(49, [48,47,51]) ==> { "average-error":3, "crowd-error":0.1111111111111127, diversity:2.888888888888889 }
diversityTheorem(49, [48,47,51,42]) ==> { "average-error":14.5, "crowd-error":4, diversity:10.5 }
=!EXPECTEND!=
*/
```


```txt
prompt$ jsish -u diversityPrediction.jsi
[PASS] diversityPrediction.jsi
```



## Julia

```julia
import Statistics: mean

function diversitytheorem(truth::T, pred::Vector{T}) where T<:Number
    μ = mean(pred)
    avgerr = mean((pred .- truth) .^ 2)
    crderr = (μ - truth) ^ 2
    divers = mean((pred .- μ) .^ 2)
    avgerr, crderr, divers
end

for (t, s) in [(49, [48, 47, 51]),
               (49, [48, 47, 51, 42])]
    avgerr, crderr, divers = diversitytheorem(t, s)
    println("""
    average-error : $avgerr
    crowd-error   : $crderr
    diversity     : $divers
    """)
end
```


```txt
average-error : 3.0
crowd-error   : 0.11111111111111269
diversity     : 2.888888888888889

average-error : 14.5
crowd-error   : 4.0
diversity     : 10.5

```



## Kotlin

```scala
// version 1.1.4-3

fun square(d: Double) = d * d

fun averageSquareDiff(d: Double, predictions: DoubleArray) =
    predictions.map { square(it - d) }.average()

fun diversityTheorem(truth: Double, predictions: DoubleArray): String {
    val average = predictions.average()
    val f = "%6.3f"
    return "average-error : ${f.format(averageSquareDiff(truth, predictions))}\n" +
           "crowd-error   : ${f.format(square(truth - average))}\n" +
           "diversity     : ${f.format(averageSquareDiff(average, predictions))}\n"
}

fun main(args: Array<String>) {
    println(diversityTheorem(49.0, doubleArrayOf(48.0, 47.0, 51.0)))
    println(diversityTheorem(49.0, doubleArrayOf(48.0, 47.0, 51.0, 42.0)))
}
```


```txt

average-error :  3.000
crowd-error   :  0.111
diversity     :  2.889

average-error : 14.500
crowd-error   :  4.000
diversity     : 10.500

```



## Perl


```perl
sub diversity {
    my($truth, @pred) = @_;
    my($ae,$ce,$cp,$pd,$stats);

    $cp += $_/@pred for @pred;      # collective prediction
    $ae = avg_error($truth, @pred); # average individual error
    $ce = ($cp - $truth)**2;        # collective error
    $pd = avg_error($cp, @pred);    # prediction diversity

    my $fmt = "%13s: %6.3f\n";
    $stats  = sprintf $fmt, 'average-error', $ae;
    $stats .= sprintf $fmt, 'crowd-error',   $ce;
    $stats .= sprintf $fmt, 'diversity',     $pd;
}

sub avg_error {
    my($m, @v) = @_;
    my($avg_err);
    $avg_err += ($_ - $m)**2 for @v;
    $avg_err/@v;
}

print diversity(49, qw<48 47 51>) . "\n";
print diversity(49, qw<48 47 51 42>);
```

```txt
average-error:  3.000
  crowd-error:  0.111
    diversity:  2.889

average-error: 14.500
  crowd-error:  4.000
    diversity: 10.500
```



## Perl 6


```perl6
sub diversity-calc($truth, @pred) {
    my $ae = avg-error($truth, @pred); # average individual error
    my $cp = ([+] @pred)/+@pred;       # collective prediction
    my $ce = ($cp - $truth)**2;        # collective error
    my $pd = avg-error($cp, @pred);    # prediction diversity
    return $ae, $ce, $pd;
}

sub avg-error ($m, @v) { ([+] (@v X- $m) X**2) / +@v }

sub diversity-format (@stats) {
    gather {
        for <average-error crowd-error diversity> Z @stats -> ($label,$value) {
            take $label.fmt("%13s") ~ ':' ~ $value.fmt("%7.3f");
        }
    }
}

.say for diversity-format diversity-calc(49, <48 47 51>);
.say for diversity-format diversity-calc(49, <48 47 51 42>);
```


```txt
average-error:  3.000
  crowd-error:  0.111
    diversity:  2.889
average-error: 14.500
  crowd-error:  4.000
    diversity: 10.500

```



## Phix


```Phix
function mean(sequence s)
    return sum(s)/length(s)
end function

function variance(sequence s, atom d)
    return mean(sq_power(sq_sub(s,d),2))
end function

function diversity_theorem(atom reference, sequence observations)
    atom average_error = variance(observations,reference),
         average = mean(observations),
         crowd_error = power(reference-average,2),
         diversity = variance(observations,average)
    return {{"average_error",average_error},
            {"crowd_error",crowd_error},
            {"diversity",diversity}}
end function

procedure test(atom reference, sequence observations)
    sequence res = diversity_theorem(reference, observations)
    for i=1 to length(res) do
        printf(1," %14s : %g\n",res[i])
    end for
end procedure
test(49, {48, 47, 51})
test(49, {48, 47, 51, 42})
```

```txt

  average_error : 3
    crowd_error : 0.111111
      diversity : 2.88889
  average_error : 14.5
    crowd_error : 4
      diversity : 10.5

```



## Python

By composition of pure functions:
```python
'''Diversity prediction theorem'''

from itertools import chain
from functools import reduce


# main :: IO ()
def main():
    '''Observed value: 49,
       prediction lists: various.
    '''

    print(unlines(map(
        showDiversityValues(49),
        [
            [48, 47, 51],
            [48, 47, 51, 42],
            [50, '?', 50, {}, 50],  # Non-numeric values.
            []                      # Missing predictions.
        ]
    )))
    print(unlines(map(
        showDiversityValues('49'),  # String in place of number.
        [
            [50, 50, 50],
            [40, 35, 40],
        ]
    )))


# meanErrorSquared :: Num -> [Num] -> Num
def meanErrorSquared(x):
    '''The mean of the squared differences
       between the observed value x and
       a non-empty list of predictions ps.
    '''
    return lambda ps: mean(list(map(
        lambda y: pow(y - x, 2),
        ps
    )))


#  diversityValues :: Num a => a -> [a] ->
#     {mean-Error :: a, crowd-error :: a, diversity :: a}
def diversityValues(x):
    '''The mean error, crowd error and
       diversity, for a given observation x
       and a non-empty list of predictions ps.
    '''
    def go(ps):
        mp = mean(ps)
        return {
            'mean-error': meanErrorSquared(x)(ps),
            'crowd-error': pow(x - mp, 2),
            'diversity': meanErrorSquared(mp)(ps)
        }
    return lambda ps: go(ps)


# FORMATTING ----------------------------------------------

# showDiversityValues :: Num -> [Num] -> Either String String
def showDiversityValues(x):
    '''Formatted string representation
       of diversity values for a given
       observation x and a non-empty
       list of predictions p.
    '''
    def go(x, ps):
        def showDict(dct):
            w = 4 + max(map(len, dct.keys()))

            def showKV(a, kv):
                k, v = kv
                return a + k.rjust(w, ' ') + (
                    ' : ' + showPrecision(3)(v) + '\n'
                )
            return 'Predictions: ' + showList(ps) + ' ->\n' + (
                reduce(showKV, dct.items(), '')
            )

        def showProblem(e):
            return (
                unlines(map(indent(1), e)) if (
                    isinstance(e, list)
                ) else indent(1)(repr(e))
            ) + '\n'

        return 'Observation:  ' + repr(x) + '\n' + (
            either(showProblem)(showDict)(
                bindLR(numLR(x))(
                    lambda n: bindLR(numsLR(ps))(
                        compose(Right)(diversityValues(n))
                    )
                )
            )
        )
    return lambda ps: go(x, ps)


# GENERIC -------------------------------------------------

# Right :: b -> Either a b
def Right(x):
    '''Constructor for a populated Either (option type) value'''
    return {'type': 'Either', 'Left': None, 'Right': x}


# Left :: a -> Either a b
def Left(x):
    '''Constructor for an empty Either (option type) value
       with an associated string.'''
    return {'type': 'Either', 'Right': None, 'Left': x}


# bindLR (>>=) :: Either a -> (a -> Either b) -> Either b
def bindLR(m):
    '''Either monad injection operator.
       Two computations sequentially composed,
       with any value produced by the first
       passed as an argument to the second.'''
    return lambda mf: (
        mf(m.get('Right')) if None is m.get('Left') else m
    )


# compose (<<<) :: (b -> c) -> (a -> b) -> a -> c
def compose(g):
    '''Right to left function composition.'''
    return lambda f: lambda x: g(f(x))


# concatMap :: (a -> [b]) -> [a] -> [b]
def concatMap(f):
    '''Concatenated list over which a function has been mapped.
       The list monad can be derived by using a function f which
       wraps its output in a list,
       (using an empty list to represent computational failure).'''
    return lambda xs: list(
        chain.from_iterable(
            map(f, xs)
        )
    )


# either :: (a -> c) -> (b -> c) -> Either a b -> c
def either(fl):
    '''The application of fl to e if e is a Left value,
       or the application of fr to e if e is a Right value.'''
    return lambda fr: lambda e: fl(e['Left']) if (
        None is e['Right']
    ) else fr(e['Right'])


# indent :: Int -> String -> String
def indent(n):
    '''String indented by n multiples
       of four spaces'''
    return lambda s: (n * 4 * ' ') + s


# mean :: [Num] -> Float
def mean(xs):
    '''Arithmetic mean of a list
       of numeric values.
    '''
    return sum(xs) / float(len(xs))


# numLR :: a -> Either String Num
def numLR(x):
    '''Either Right x if x is a float or int,
       or a Left explanatory message.'''
    return Right(x) if (
        isinstance(x, (float, int))
    ) else Left('Expected number, saw: ' + str(type(x)) + ' ' + repr(x))


# numsLR :: [a] -> Either String [Num]
def numsLR(xs):
    '''Either Right xs if all xs are float or int,
       or a Left explanatory message.'''
    def go(ns):
        ls, rs = partitionEithers(map(numLR, ns))
        return Left(ls) if ls else Right(rs)
    return bindLR(
        Right(xs) if (
            bool(xs) and isinstance(xs, list)
        ) else Left(
            'Expected a non-empty list, saw: ' + (
                str(type(xs)) + ' ' + repr(xs)
            )
        )
    )(go)


# partitionEithers :: [Either a b] -> ([a],[b])
def partitionEithers(lrs):
    '''A list of Either values partitioned into a tuple
       of two lists, with all Left elements extracted
       into the first list, and Right elements
       extracted into the second list.
    '''
    def go(a, x):
        ls, rs = a
        r = x.get('Right')
        return (ls + [x.get('Left')], rs) if None is r else (
            ls, rs + [r]
        )
    return reduce(go, lrs, ([], []))


# showList :: [a] -> String
def showList(xs):
    '''Compact string representation of a list'''
    return '[' + ','.join(str(x) for x in xs) + ']'


# showPrecision Int -> Float -> String
def showPrecision(n):
    '''A string showing a floating point number
       at a given degree of precision.'''
    return lambda x: str(round(x, n))


# unlines :: [String] -> String
def unlines(xs):
    '''A single string derived by the intercalation
       of a list of strings with the newline character.'''
    return '\n'.join(xs)


# MAIN ---
if __name__ == '__main__':
    main()
```

```txt

Observation:  49
Predictions: [48,47,51] ->
     mean-error : 3.0
    crowd-error : 0.111
      diversity : 2.889

Observation:  49
Predictions: [48,47,51,42] ->
     mean-error : 14.5
    crowd-error : 4.0
      diversity : 10.5

Observation:  49
    Expected number, saw: <class 'str'> '?'
    Expected number, saw: <class 'dict'> {}

Observation:  49
    "Expected a non-empty list, saw: <class 'list'> []"

Observation:  '49'
    "Expected number, saw: <class 'str'> '49'"

Observation:  '49'
    "Expected number, saw: <class 'str'> '49'"
```



## REXX


### version 1


```rexx
/* REXX */
Numeric Digits 20
Call diversityTheorem 49,'48 47 51'
Say '--------------------------------------'
Call diversityTheorem 49,'48 47 51 42'
Exit

diversityTheorem:
  Parse Arg truth,list
  average=average(list)
  Say 'average-error='averageSquareDiff(truth,list)
  Say 'crowd-error='||(truth-average)**2
  Say 'diversity='averageSquareDiff(average,list)
  Return

average: Procedure
  Parse Arg list
  res=0
  Do i=1 To words(list)
    res=res+word(list,i)  /* accumulate list elements */
    End
  Return res/words(list)  /* return the average */

averageSquareDiff: Procedure
  Parse Arg a,list
  res=0
  Do i=1 To words(list)
    x=word(list,i)
    res=res+(x-a)**2      /* accumulate square of differences */
    End
  Return res/words(list)  /* return the average */
```

```txt
average-error=3
crowd-error=0.11111111111111111089
diversity=2.8888888888888888889
--------------------------------------
average-error=14.5
crowd-error=4
diversity=10.5
```



### version 2

Uses greater precision, but limits the output to six decimal digits past the decimal point   (see the last comment in the program).

```rexx
/*REXX program calculates:   average error,   crowd error,   and   prediction diversity.*/
numeric digits 50                                /*set precision at fifty decimal digits*/
call diversity 49,     48  47  51                /*true value, & the crowd predictions. */
call diversity 49,     48  47  51  42            /*  "    "    "  "    "        "       */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
avg:   $=0;    do k=1  for #;   $= $ +  word(est, k)             ;    end;      return $/#
avgSD: $=0;    do j=1  for #;   $= $ + (word(est, j) - arg(1))**2;    end;      return $/#
/*──────────────────────────────────────────────────────────────────────────────────────*/
diversity: parse arg true, est;    #= words(est) /*get args;  count number of estimates.*/
           say '   the  true   value: '  true  copies("═", 20)  'crowd estimates: '   ests
           avg$= avg()                           /* [↓]  avgSD=avg of squared difference*/
           say '   the average error: '     format( avgSD(true)    , , 6) / 1
           say '   the  crowd  error: '     format( (true-avg$)**2 , , 6) / 1
           say 'prediction diversity: '     format( avgSD(avg$)    , , 6) / 1;   say;  say
           return                                /*only show up to 6───┘  decimal digits*/
```

```txt

   the  true   value:  49 ════════════════════ crowd estimates:  48 47 51
   the average error:  3
   the  crowd  error:  0.111111
prediction diversity:  2.888889


   the  true   value:  49 ════════════════════ crowd estimates:  48 47 51 42
   the average error:  14.5
   the  crowd  error:  4
prediction diversity:  10.5

```



## Sidef

```ruby
func avg_error(m, v) {
    v.map { (_ - m)**2 }.sum / v.len
}

func diversity_calc(truth, pred) {
    var ae = avg_error(truth, pred)
    var cp = pred.sum/pred.len
    var ce = (cp - truth)**2
    var pd = avg_error(cp, pred)
    return [ae, ce, pd]
}

func diversity_format(stats) {
    gather {
        for t,v in (%w(average-error crowd-error diversity) ~Z stats) {
            take(("%13s" % t) + ':' + ('%7.3f' % v))
        }
    }
}

diversity_format(diversity_calc(49, [48, 47, 51])).each{.say}
diversity_format(diversity_calc(49, [48, 47, 51, 42])).each{.say}
```

```txt

average-error:  3.000
  crowd-error:  0.111
    diversity:  2.889
average-error: 14.500
  crowd-error:  4.000
    diversity: 10.500

```



## TypeScript


```TypeScript

function sum(array: Array<number>): number {
    return array.reduce((a, b) => a + b)
}

function square(x : number) :number {
    return x * x
}

function mean(array: Array<number>): number {
    return sum(array) / array.length
}

function averageSquareDiff(a: number, predictions: Array<number>): number {
    return mean(predictions.map(x => square(x - a)))
}

function diversityTheorem(truth: number, predictions: Array<number>): Object {
    const average: number = mean(predictions)
    return {
        "average-error": averageSquareDiff(truth, predictions),
        "crowd-error": square(truth - average),
        "diversity": averageSquareDiff(average, predictions)
    }
}

console.log(diversityTheorem(49, [48,47,51]))
console.log(diversityTheorem(49, [48,47,51,42]))

```

```txt

{ 'average-error': 3,
  'crowd-error': 0.11111111111111269,
  diversity: 2.888888888888889 }
{ 'average-error': 14.5, 'crowd-error': 4, diversity: 10.5 }

```



## zkl

```zkl
fcn avgError(m,v){ v.apply('wrap(n){ (n - m).pow(2) }).sum(0.0)/v.len() }

fcn diversityCalc(truth,pred){  //(Float,List of Float)
   ae,cp := avgError(truth,pred), pred.sum(0.0)/pred.len();
   ce,pd := (cp - truth).pow(2),  avgError(cp, pred);
   return(ae,ce,pd)
}

fcn diversityFormat(stats){  // ( (averageError,crowdError,diversity) )
   T("average-error","crowd-error","diversity").zip(stats)
   .pump(String,Void.Xplode,"%13s :%7.3f\n".fmt)
}
```


```zkl
diversityCalc(49.0, T(48.0,47.0,51.0)) : diversityFormat(_).println();
diversityCalc(49.0, T(48.0,47.0,51.0,42.0)) : diversityFormat(_).println();
```

```txt

average-error :  3.000
  crowd-error :  0.111
    diversity :  2.889

average-error : 14.500
  crowd-error :  4.000
    diversity : 10.500

```

