+++
title = "Check Machin-like formulas"
description = ""
date = 2019-06-16T08:46:39Z
aliases = []
[extra]
id = 12552
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "1_false",
  "1_true",
  "clojure",
  "d",
  "echolisp",
  "factor",
  "freebasic",
  "gap",
  "go",
  "haskell",
  "j",
  "julia",
  "kotlin",
  "maxima",
  "ocaml",
  "oorexx",
  "pari_gp",
  "perl",
  "perl_6",
  "phix",
  "python",
  "r",
  "racket",
  "rexx",
  "seed7",
  "sidef",
  "tcl",
  "visual_basic_.net",
  "xpl0",
]
+++

[[wp:Machin-like_formula|Machin-like formulas]]   are useful for efficiently computing numerical approximations for <big><big><math>\pi</math></big></big>


## Task

Verify the following Machin-like formulas are correct by calculating the value of '''tan'''   (''right hand side)'' for each equation using exact arithmetic and showing they equal '''1''':

: <math>{\pi\over4} = \arctan{1\over2} + \arctan{1\over3}</math>
: <math>{\pi\over4} = 2 \arctan{1\over3} + \arctan{1\over7}</math>
: <math>{\pi\over4} = 4 \arctan{1\over5} - \arctan{1\over239}</math>
: <math>{\pi\over4} = 5 \arctan{1\over7} + 2 \arctan{3\over79}</math>
: <math>{\pi\over4} = 5 \arctan{29\over278} + 7 \arctan{3\over79}</math>
: <math>{\pi\over4} = \arctan{1\over2} + \arctan{1\over5} + \arctan{1\over8}</math>
: <math>{\pi\over4} = 4 \arctan{1\over5} - \arctan{1\over70} + \arctan{1\over99}</math>
: <math>{\pi\over4} = 5 \arctan{1\over7} + 4 \arctan{1\over53} + 2 \arctan{1\over4443}</math>
: <math>{\pi\over4} = 6 \arctan{1\over8} + 2 \arctan{1\over57} + \arctan{1\over239}</math>
: <math>{\pi\over4} = 8 \arctan{1\over10} - \arctan{1\over239} - 4 \arctan{1\over515}</math>
: <math>{\pi\over4} = 12 \arctan{1\over18} + 8 \arctan{1\over57} - 5 \arctan{1\over239}</math>
: <math>{\pi\over4} = 16 \arctan{1\over21} + 3 \arctan{1\over239} + 4 \arctan{3\over1042}</math>
: <math>{\pi\over4} = 22 \arctan{1\over28} + 2 \arctan{1\over443} - 5 \arctan{1\over1393} - 10 \arctan{1\over11018}</math>
: <math>{\pi\over4} = 22 \arctan{1\over38} + 17 \arctan{7\over601} + 10 \arctan{7\over8149}</math>
: <math>{\pi\over4} = 44 \arctan{1\over57} + 7 \arctan{1\over239} - 12 \arctan{1\over682} + 24 \arctan{1\over12943}</math>
: <math>{\pi\over4} = 88 \arctan{1\over172} + 51 \arctan{1\over239} + 32 \arctan{1\over682} + 44 \arctan{1\over5357} + 68 \arctan{1\over12943}</math>

and confirm that the following formula is incorrect by showing   '''tan'''   (''right hand side)''   is ''not''   '''1''':

: <math>{\pi\over4} = 88 \arctan{1\over172} + 51 \arctan{1\over239} + 32 \arctan{1\over682} + 44 \arctan{1\over5357} + 68 \arctan{1\over12944}</math>

These identities are useful in calculating the values:
: <math>\tan(a + b) = {\tan(a) + \tan(b) \over 1 - \tan(a) \tan(b)}</math>

: <math>\tan\left(\arctan{a \over b}\right) = {a \over b}</math>

: <math>\tan(-a) = -\tan(a)</math>



You can store the equations in any convenient data structure, but for extra credit parse them from human-readable [[Check_Machin-like_formulas/text_equations|text input]].

Note: to formally prove the formula correct, it would have to be shown that ''<math>{-3 pi \over 4}</math> < right hand side < <math>{5 pi \over 4}</math>'' due to ''<math>\tan()</math>'' periodicity.






## Clojure

Clojure automatically handles ratio of numbers as fractions
```lisp
(ns tanevaulator
  (:gen-class))

;; Notation: [a b c] -> a x arctan(a/b)
(def test-cases   [
                          [[1, 1, 2], [1, 1, 3]],
                          [[2, 1, 3], [1, 1, 7]],
                          [[4, 1, 5], [-1, 1, 239]],
                          [[5, 1, 7], [2, 3, 79]],
                          [[1, 1, 2], [1, 1, 5], [1, 1, 8]],
                          [[4, 1, 5], [-1, 1, 70], [1, 1, 99]],
                          [[5, 1, 7], [4, 1, 53], [2, 1, 4443]],
                          [[6, 1, 8], [2, 1, 57], [1, 1, 239]],
                          [[8, 1, 10], [-1, 1, 239], [-4, 1, 515]],
                          [[12, 1, 18], [8, 1, 57], [-5, 1, 239]],
                          [[16, 1, 21], [3, 1, 239], [4, 3, 1042]],
                          [[22, 1, 28], [2, 1, 443], [-5, 1, 1393], [-10, 1, 11018]],
                          [[22, 1, 38], [17, 7, 601], [10, 7, 8149]],
                          [[44, 1, 57], [7, 1, 239], [-12, 1, 682], [24, 1, 12943]],
                          [[88, 1, 172], [51, 1, 239], [32, 1, 682], [44, 1, 5357], [68, 1, 12943]],
                          [[88, 1, 172], [51, 1, 239], [32, 1, 682], [44, 1, 5357], [68, 1, 12944]]
                  ])

(defn tan-sum [a b]
  " tan (a + b) "
  (/ (+ a b) (- 1 (* a b))))

(defn tan-eval [m]
  " Evaluates tan of a triplet (e.g. [1, 1, 2])"
  (let [coef (first m)
        rat (/ (nth m 1) (nth m 2))]
  (cond
    (= 1  coef) rat
    (neg? coef) (tan-eval [(- (nth m 0)) (- (nth m 1)) (nth m 2)])
    :else (let [
                ca (quot coef 2)
                cb (- coef ca)
                a (tan-eval [ca (nth m 1) (nth m 2)])
                b (tan-eval [cb (nth m 1) (nth m 2)])]
            (tan-sum a b)))))

(defn tans [m]
  " Evaluates tan of set of triplets (e.g. [[1, 1, 2], [1, 1, 3]])"
  (if (= 1 (count m))
    (tan-eval (nth m 0))
    (let [a (tan-eval (first m))
          b (tans (rest m))]
      (tan-sum a b))))

(doseq [q test-cases]
  " Display results "
  (println "tan " q " = "(tans q)))

```

```txt


tan  [[1 1 2] [1 1 3]]  =  1N
tan  [[2 1 3] [1 1 7]]  =  1N
tan  [[4 1 5] [-1 1 239]]  =  1N
tan  [[5 1 7] [2 3 79]]  =  1N
tan  [[1 1 2] [1 1 5] [1 1 8]]  =  1N
tan  [[4 1 5] [-1 1 70] [1 1 99]]  =  1N
tan  [[5 1 7] [4 1 53] [2 1 4443]]  =  1N
tan  [[6 1 8] [2 1 57] [1 1 239]]  =  1N
tan  [[8 1 10] [-1 1 239] [-4 1 515]]  =  1N
tan  [[12 1 18] [8 1 57] [-5 1 239]]  =  1N
tan  [[16 1 21] [3 1 239] [4 3 1042]]  =  1N
tan  [[22 1 28] [2 1 443] [-5 1 1393] [-10 1 11018]]  =  1N
tan  [[22 1 38] [17 7 601] [10 7 8149]]  =  1N
tan  [[44 1 57] [7 1 239] [-12 1 682] [24 1 12943]]  =  1N
tan  [[88 1 172] [51 1 239] [32 1 682] [44 1 5357] [68 1 12943]]  =  1N
tan  [[88 1 172] [51 1 239] [32 1 682] [44 1 5357] [68 1 12944]]  =  1009288018000944050967896710431587186456256928584351786643498522649995492271475761189348270710224618853590682465929080006511691833816436374107451368838065354726517908250456341991684635768915704374493675498637876700129004484434187627909285979251682006538817341793224963346197503893270875008524149334251672855130857035205217929335932890740051319216343365800342290782260673215928499123722781078448297609548233999010983373327601187505623621602789012550584784738082074783523787011976757247516095289966708782862528690942242793667539020699840402353522108223 /
                                                                     1009288837315638583415701528780402795721935641614456853534313491853293025565940011104051964874275710024625850092154664245109626053906509780125743180758231049920425664246286578958307532545458843067352531217230461290763258378749459637420702619029075083089762088232401888676895047947363883809724322868121990870409574061477638203859217672620508200713073485398199091153535700094640095900731630771349477187594074169815106104524371099618096164871416282464532355211521113449237814080332335526420331468258917484010722587072087349909684004660371264507984339711
                              (equals  0.9999991882257445)

```



## D

This uses the module of the Arithmetic Rational Task.
```d
import std.stdio, std.regex, std.conv, std.string, std.range,
       arithmetic_rational;

struct Pair { int x; Rational r; }

Pair[][] parseEquations(in string text) /*pure nothrow*/ {
    auto r = regex(r"\s*(?P<sign>[+-])?\s*(?:(?P<mul>\d+)\s*\*)?\s*" ~
                   r"arctan\((?P<num>\d+)/(?P<denom>\d+)\)");
    Pair[][] machins;
    foreach (const line; text.splitLines) {
        Pair[] formula;
        foreach (part; line.split("=")[1].matchAll(r)) {
            immutable mul = part["mul"],
                      num = part["num"],
                      denom = part["denom"];
            formula ~= Pair((part["sign"] == "-" ? -1 : 1) *
                            (mul.empty ? 1 : mul.to!int),
                            Rational(num.to!int,
                                     denom.empty ? 1 : denom.to!int));
        }
        machins ~= formula;
    }
    return machins;
}


Rational tans(in Pair[] xs) pure nothrow {
    static Rational tanEval(in int coef, in Rational f)
    pure nothrow {
        if (coef == 1)
            return f;
        if (coef < 0)
            return -tanEval(-coef, f);
        immutable a = tanEval(coef / 2, f),
                  b = tanEval(coef - coef / 2, f);
        return (a + b) / (1 - a * b);
    }

    if (xs.length == 1)
        return tanEval(xs[0].tupleof);
    immutable a = xs[0 .. $ / 2].tans,
              b = xs[$ / 2 .. $].tans;
    return (a + b) / (1 - a * b);
}

void main() {
    immutable equationText =
"pi/4 = arctan(1/2) + arctan(1/3)
pi/4 = 2*arctan(1/3) + arctan(1/7)
pi/4 = 4*arctan(1/5) - arctan(1/239)
pi/4 = 5*arctan(1/7) + 2*arctan(3/79)
pi/4 = 5*arctan(29/278) + 7*arctan(3/79)
pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)
pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)
pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)
pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)
pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)
pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)
pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)
pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)
pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)
pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)
pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)
pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)";

    const machins = equationText.parseEquations;
    foreach (const machin, const eqn; machins.zip(equationText.splitLines)) {
        immutable ans = machin.tans;
        writefln("%5s: %s", ans == 1 ? "OK" : "ERROR", eqn);
    }
}
```

```txt
   OK: pi/4 = arctan(1/2) + arctan(1/3)
   OK: pi/4 = 2*arctan(1/3) + arctan(1/7)
   OK: pi/4 = 4*arctan(1/5) - arctan(1/239)
   OK: pi/4 = 5*arctan(1/7) + 2*arctan(3/79)
   OK: pi/4 = 5*arctan(29/278) + 7*arctan(3/79)
   OK: pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)
   OK: pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)
   OK: pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)
   OK: pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)
   OK: pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)
   OK: pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)
   OK: pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)
   OK: pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)
   OK: pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)
   OK: pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)
   OK: pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)
ERROR: pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)
```



## EchoLisp


```scheme

(lib 'math)
(lib 'match)
(math-precision 1.e-10)

;; formally derive (tan ..) expressions
;; copied from Racket
;; adapted and improved for performance

(define (reduce e)
;; (set! rcount (1+ rcount)) ;; # of calls
  (match e
    [(? number? a)                         a]
    [('+ (? number? a) (? number? b)) (+ a b)]
    [('- (? number? a) (? number? b)) (- a b)]
    [('- (? number? a))               (- a)]
    [('* (? number? a) (? number? b)) (* a b)]
    [('/ (? number? a) (? number? b)) (/ a b)] ; patch

    [( '+ a b)                         (reduce `(+ ,(reduce a) ,(reduce b)))]
    [( '- a b)                         (reduce `(- ,(reduce a) ,(reduce b)))]
    [( '- a)                           (reduce `(- ,(reduce a)))]
    [( '* a b)                         (reduce `(* ,(reduce a) ,(reduce b)))]
    [( '/ a b)                         (reduce `(/ ,(reduce a) ,(reduce b)))]

    [( 'tan ('arctan a))           (reduce a)]
    [( 'tan ( '- a))               (reduce `(- (tan ,a)))]

    ;; x 100 # calls reduction : derive (tan ,a) only once
    [( 'tan ( '+ a b))
          (let ((alpha (reduce  `(tan ,a))) (beta (reduce  `(tan ,b))))
    	  (reduce `(/ (+ ,alpha ,beta) (- 1 (* ,alpha ,beta)))))]

    [( 'tan ( '+ a b c ...))       (reduce `(tan (+ ,a (+ ,b ,@c))))]

    [( 'tan ( '- a b))
                (let ((alpha (reduce  `(tan ,a))) (beta (reduce  `(tan ,b))))
    		(reduce `(/ (- ,alpha ,beta) (+ 1 (* ,alpha ,beta)))))]

    ;; add formula for (tan 2 (arctan a)) = 2 a / (1 - a^2))
    [( 'tan ( '* 2 ('arctan a)))   (reduce `(/ (* 2 ,a) (- 1 (* ,a ,a))))]
    [( 'tan ( '* 1 ('arctan a)))   (reduce a)] ; added

    [( 'tan ( '* (? number? n) a))
     (cond [(< n 0) (reduce `(- (tan (* ,(- n) ,a))))]
           [(= n 0) 0]
           [(= n 1)    (reduce `(tan ,a))]
           [(even? n)
              (let ((alpha (reduce  `(tan (* ,(/ n 2) ,a))))) ;; # calls reduction
    	      (reduce `(/ (* 2 ,alpha) (- 1 (* ,alpha ,alpha)))))]
           [else      (reduce `(tan (+ ,a  (* ,(- n 1) ,a))))])]
    ))

(define (task)
	(for ((f machins))
	(if (~= 1 (reduce f))
		(writeln '👍 f  '⟾ 1 )
		(writeln '❌ f '➽ (reduce f) ))))


```

```scheme


(define machins
  '((tan (+ (arctan 1/2) (arctan 1/3)))
    (tan (+ (* 2 (arctan 1/3)) (arctan 1/7)))
    (tan (- (* 4 (arctan 1/5)) (arctan 1/239)))
    (tan (+ (* 5 (arctan 1/7)) (* 2 (arctan 3/79))))
    (tan (+ (* 5 (arctan 29/278)) (* 7 (arctan 3/79))))
    (tan (+ (arctan 1/2) (arctan 1/5) (arctan 1/8)))
    (tan (+ (* 4 (arctan 1/5)) (* -1 (arctan 1/70)) (arctan 1/99)))
    (tan (+ (* 5 (arctan 1/7)) (* 4 (arctan 1/53)) (* 2 (arctan 1/4443))))
    (tan (+ (* 6 (arctan 1/8)) (* 2 (arctan 1/57)) (arctan 1/239)))
    (tan (+ (* 8 (arctan 1/10)) (* -1 (arctan 1/239)) (* -4 (arctan 1/515))))
    (tan (+ (* 12 (arctan 1/18)) (* 8 (arctan 1/57)) (* -5 (arctan 1/239))))
    (tan (+ (* 16 (arctan 1/21)) (* 3 (arctan 1/239)) (* 4 (arctan 3/1042))))
    (tan (+ (* 22 (arctan 1/28)) (* 2 (arctan 1/443)) (* -5 (arctan 1/1393)) (* -10 (arctan 1/11018))))
    (tan (+ (* 22 (arctan 1/38)) (* 17 (arctan 7/601)) (* 10 (arctan 7/8149))))
    (tan (+ (* 44 (arctan 1/57)) (* 7 (arctan 1/239)) (* -12 (arctan 1/682)) (* 24 (arctan 1/12943))))
    (tan (+ (* 88 (arctan 1/172)) (* 51 (arctan 1/239)) (* 32 (arctan 1/682))
            (* 44 (arctan 1/5357)) (* 68 (arctan 1/12943))))
    (tan (+ (* 88 (arctan 1/172)) (* 51 (arctan 1/239)) (* 32 (arctan 1/682))
           (* 44 (arctan 1/5357)) (* 68 (arctan 1/12944))))))

(task)

👍     (tan (+ (arctan 1/2) (arctan 1/3)))     ⟾     1
👍     (tan (+ (* 2 (arctan 1/3)) (arctan 1/7)))     ⟾     1
👍     (tan (- (* 4 (arctan 1/5)) (arctan 1/239)))     ⟾     1
👍     (tan (+ (* 5 (arctan 1/7)) (* 2 (arctan 3/79))))     ⟾     1
👍     (tan (+ (* 5 (arctan 29/278)) (* 7 (arctan 3/79))))     ⟾     1
👍     (tan (+ (arctan 1/2) (arctan 1/5) (arctan 1/8)))     ⟾     1
👍     (tan (+ (* 4 (arctan 1/5)) (* -1 (arctan 1/70)) (arctan 1/99)))     ⟾     1
👍     (tan (+ (* 5 (arctan 1/7)) (* 4 (arctan 1/53)) (* 2 (arctan 1/4443))))     ⟾     1
👍     (tan (+ (* 6 (arctan 1/8)) (* 2 (arctan 1/57)) (arctan 1/239)))     ⟾     1
👍     (tan (+ (* 8 (arctan 1/10)) (* -1 (arctan 1/239)) (* -4 (arctan 1/515))))     ⟾     1
👍     (tan (+ (* 12 (arctan 1/18)) (* 8 (arctan 1/57)) (* -5 (arctan 1/239))))     ⟾     1
👍     (tan (+ (* 16 (arctan 1/21)) (* 3 (arctan 1/239)) (* 4 (arctan 3/1042))))     ⟾     1
👍     (tan (+ (* 22 (arctan 1/28)) (* 2 (arctan 1/443)) (* -5 (arctan 1/1393)) (* -10 (arctan 1/11018))))     ⟾     1
👍     (tan (+ (* 22 (arctan 1/38)) (* 17 (arctan 7/601)) (* 10 (arctan 7/8149))))     ⟾     1
👍     (tan (+ (* 44 (arctan 1/57)) (* 7 (arctan 1/239)) (* -12 (arctan 1/682)) (* 24 (arctan 1/12943))))     ⟾     1
👍     (tan (+ (* 88 (arctan 1/172)) (* 51 (arctan 1/239)) (* 32 (arctan 1/682))
       (* 44 (arctan 1/5357)) (* 68 (arctan 1/12943))))     ⟾     1
❌     (tan (+ (* 88 (arctan 1/172)) (* 51 (arctan 1/239)) (* 32 (arctan 1/682))
       (* 44 (arctan 1/5357)) (* 68 (arctan 1/12944))))     ➽     0.9999991882257442


```



## Factor


```factor
USING: combinators formatting kernel locals math sequences ;
IN: rosetta-code.machin

: tan+ ( x y -- z ) [ + ] [ * 1 swap - / ] 2bi ;

:: tan-eval ( coef frac -- x )
    {
        { [ coef zero? ] [ 0 ] }
        { [ coef neg? ] [ coef neg frac tan-eval neg ] }
        { [ coef odd? ] [ frac coef 1 - frac tan-eval tan+ ] }
        [ coef 2/ frac tan-eval dup tan+ ]
    } cond ;

: tans ( seq -- x ) [ first2 tan-eval ] [ tan+ ] map-reduce ;

: machin ( -- )
    {
        { { 1 1/2 } { 1 1/3 } }
        { { 2 1/3 } { 1 1/7 } }
        { { 4 1/5 } { -1 1/239 } }
        { { 5 1/7 } { 2 3/79 } }
        { { 5 29/278 } { 7 3/79 } }
        { { 1 1/2 } { 1 1/5 } { 1 1/8 } }
        { { 5 1/7 } { 4 1/53 } { 2 1/4443 } }
        { { 6 1/8 } { 2 1/57 } { 1 1/239 } }
        { { 8 1/10 } { -1 1/239 } { -4 1/515 } }
        { { 12 1/18 } { 8 1/57 } { -5 1/239 } }
        { { 16 1/21 } { 3 1/239 } { 4 3/1042 } }
        { { 22 1/28 } { 2 1/443 }
          { -5 1/1393 } { -10 1/11018 } }
        { { 22 1/38 } { 17 7/601 } { 10 7/8149 } }
        { { 44 1/57 } { 7 1/239 } { -12 1/682 } { 24 1/12943 } }
        { { 88 1/172 } { 51 1/239 } { 32 1/682 }
          { 44 1/5357 } { 68 1/12943 } }
        { { 88 1/172 } { 51 1/239 } { 32 1/682 }
          { 44 1/5357 } { 68 1/12944 } }
    } [ dup tans "tan %u = %u\n" printf ] each ;

MAIN: machin
```

```txt

tan { { 1 1/2 } { 1 1/3 } } = 1
tan { { 2 1/3 } { 1 1/7 } } = 1
tan { { 4 1/5 } { -1 1/239 } } = 1
tan { { 5 1/7 } { 2 3/79 } } = 1
tan { { 5 29/278 } { 7 3/79 } } = 1
tan { { 1 1/2 } { 1 1/5 } { 1 1/8 } } = 1
tan { { 5 1/7 } { 4 1/53 } { 2 1/4443 } } = 1
tan { { 6 1/8 } { 2 1/57 } { 1 1/239 } } = 1
tan { { 8 1/10 } { -1 1/239 } { -4 1/515 } } = 1
tan { { 12 1/18 } { 8 1/57 } { -5 1/239 } } = 1
tan { { 16 1/21 } { 3 1/239 } { 4 3/1042 } } = 1
tan { { 22 1/28 } { 2 1/443 } { -5 1/1393 } { -10 1/11018 } } = 1
tan { { 22 1/38 } { 17 7/601 } { 10 7/8149 } } = 1
tan { { 44 1/57 } { 7 1/239 } { -12 1/682 } { 24 1/12943 } } = 1
tan {
    { 88 1/172 }
    { 51 1/239 }
    { 32 1/682 }
    { 44 1/5357 }
    { 68 1/12943 }
} = 1
tan {
    { 88 1/172 }
    { 51 1/239 }
    { 32 1/682 }
    { 44 1/5357 }
    { 68 1/12944 }
} = 10092...08223/10092...39711

```



## FreeBASIC

```freebasic
' version 07-04-2018
' compile with: fbc -s console

#Include "gmp.bi"

#Define _a(Q) (@(Q)->_mp_num)  'a
#Define _b(Q) (@(Q)->_mp_den)  'b

Data "[1, 1, 2] [1, 1, 3]"
Data "[2, 1, 3] [1, 1, 7]"
Data "[4, 1, 5] [-1, 1, 239]"
Data "[5, 1, 7] [2, 3, 79]"
Data "[1, 1, 2] [1, 1, 5] [1, 1, 8]"
Data "[4, 1, 5] [-1, 1, 70] [1, 1, 99]"
Data "[5, 1, 7] [4, 1, 53] [2, 1, 4443]"
Data "[6, 1, 8] [2, 1, 57] [1, 1, 239]"
Data "[8, 1, 10] [-1, 1, 239] [-4, 1, 515]"
Data "[12, 1, 18] [8, 1, 57] [-5, 1, 239]"
Data "[16, 1, 21] [3, 1, 239] [4, 3, 1042]"
Data "[22, 1, 28] [2, 1, 443] [-5, 1, 1393] [-10, 1, 11018]"
Data "[22, 1, 38] [17, 7, 601] [10, 7, 8149]"
Data "[44, 1, 57] [7, 1, 239] [-12, 1, 682] [24, 1, 12943]"
Data "[88, 1, 172] [51, 1, 239] [32, 1, 682] [44, 1, 5357] [68, 1, 12943]"
Data "[88, 1, 172] [51, 1, 239] [32, 1, 682] [44, 1, 5357] [68, 1, 12944]"
Data ""

Sub work2do (ByRef a As LongInt, f1 As mpq_ptr)

    Dim As LongInt flag = -1

    Dim As Mpq_ptr x, y, z
    x = Allocate(Len(__mpq_struct)) : Mpq_init(x)
    y = Allocate(Len(__mpq_struct)) : Mpq_init(y)
    z = Allocate(Len(__mpq_struct)) : Mpq_init(z)

    Dim As Mpz_ptr temp1, temp2
    temp1 = Allocate(Len(__Mpz_struct)) : Mpz_init(temp1)
    temp2 = Allocate(Len(__Mpz_struct)) : Mpz_init(temp2)

    mpq_set(y, f1)

    While a > 0
        If (a And 1) = 1 Then
            If flag = -1 Then
                mpq_set(x, y)
                flag = 0
            Else
                Mpz_mul(temp1, _a(x), _b(y))
                Mpz_mul(temp2, _b(x), _a(y))
                Mpz_add(_a(z), temp1, temp2)
                Mpz_mul(temp1, _b(x), _b(y))
                Mpz_mul(temp2, _a(x), _a(y))
                Mpz_sub(_b(z), temp1, temp2)
                mpq_canonicalize(z)
                mpq_set(x, z)
            End If
        End If

        Mpz_mul(temp1, _a(y), _b(y))
        Mpz_mul(temp2, _b(y), _a(y))
        Mpz_add(_a(z), temp1, temp2)
        Mpz_mul(temp1, _b(y), _b(y))
        Mpz_mul(temp2, _a(y), _a(y))
        Mpz_sub(_b(z), temp1, temp2)
        mpq_canonicalize(z)
        mpq_set(y, z)

        a = a Shr 1
    Wend

    mpq_set(f1, x)

End Sub

' ------=< MAIN >=------

Dim As Mpq_ptr f1, f2, f3
f1 = Allocate(Len(__mpq_struct)) : Mpq_init(f1)
f2 = Allocate(Len(__mpq_struct)) : Mpq_init(f2)
f3 = Allocate(Len(__mpq_struct)) : Mpq_init(f3)

Dim As Mpz_ptr temp1, temp2
temp1 = Allocate(Len(__Mpz_struct)) : Mpz_init(temp1)
temp2 = Allocate(Len(__Mpz_struct)) : Mpz_init(temp2)

Dim As mpf_ptr float
float = Allocate(Len(__mpf_struct)) : Mpf_init(float)

Dim As LongInt m1, a1, b1, flag, t1, t2, t3, t4
Dim As String s, s1, s2, s3, sign
Dim As ZString Ptr zstr

Do

    Read s
    If s = "" Then Exit Do
    flag = -1

    While s <> ""
        t1 = InStr(s, "[") +1
        t2 = InStr(t1, s, ",") +1
        t3 = InStr(t2, s, ",") +1
        t4 = InStr(t3, s, "]")
        s1 = Trim(Mid(s, t1, t2 - t1 -1))
        s2 = Trim(Mid(s, t2, t3 - t2 -1))
        s3 = Trim(Mid(s, t3, t4 - t3))
        m1 = Val(s1)
        a1 = Val(s2)
        b1 = Val(s3)

        sign = IIf(m1 < 0, " - ", " + ")
        If m1 < 0 Then a1 = -a1 : m1 = Abs(m1)
        s = Mid(s, t4 +1)
        Print IIf(flag = 0, sign, ""); IIf(m1 = 1, "", Str(m1));
        Print "Atn("; s2; "/" ;s3; ")";

        If flag = -1 Then
            flag = 0
            Mpz_set_si(_a(f1), a1)
            Mpz_set_si(_b(f1), b1)
            If m1 > 1 Then work2do(m1, f1)
            Continue While
        End If

        Mpz_set_si(_a(f2), a1)
        Mpz_set_si(_b(f2), b1)
        If m1 > 1 Then work2do(m1, f2)

        Mpz_mul(temp1, _a(f1), _b(f2))
        Mpz_mul(temp2, _b(f1), _a(f2))
        Mpz_add(_a(f3), temp1, temp2)
        Mpz_mul(temp1, _b(f1), _b(f2))
        Mpz_mul(temp2, _a(f1), _a(f2))
        Mpz_sub(_b(f3), temp1, temp2)
        mpq_canonicalize(f3)
        mpq_set(f1, f3)

    Wend

    If Mpz_cmp_ui(_b(f1), 1) = 0 AndAlso Mpz_cmp(_a(f1), _b(f1)) = 0 Then
        Print " = 1"
    Else
        Mpf_set_q(float, f1)
        gmp_printf(!" = %.*Ff\n", 15, float)
    End If

Loop

' empty keyboard buffer
While InKey <> "" : Wend
Print : Print "hit any key to end program"
Sleep
End
```

```txt
Atn(1/2) + Atn(1/3) = 1
2Atn(1/3) + Atn(1/7) = 1
4Atn(1/5) - Atn(1/239) = 1
5Atn(1/7) + 2Atn(3/79) = 1
Atn(1/2) + Atn(1/5) + Atn(1/8) = 1
4Atn(1/5) - Atn(1/70) + Atn(1/99) = 1
5Atn(1/7) + 4Atn(1/53) + 2Atn(1/4443) = 1
6Atn(1/8) + 2Atn(1/57) + Atn(1/239) = 1
8Atn(1/10) - Atn(1/239) - 4Atn(1/515) = 1
12Atn(1/18) + 8Atn(1/57) - 5Atn(1/239) = 1
16Atn(1/21) + 3Atn(1/239) + 4Atn(3/1042) = 1
22Atn(1/28) + 2Atn(1/443) - 5Atn(1/1393) - 10Atn(1/11018) = 1
22Atn(1/38) + 17Atn(7/601) + 10Atn(7/8149) = 1
44Atn(1/57) + 7Atn(1/239) - 12Atn(1/682) + 24Atn(1/12943) = 1
88Atn(1/172) + 51Atn(1/239) + 32Atn(1/682) + 44Atn(1/5357) + 68Atn(1/12943) = 1
88Atn(1/172) + 51Atn(1/239) + 32Atn(1/682) + 44Atn(1/5357) + 68Atn(1/12944) = 0.999999188225744
```



## GAP

The formula is entered as a list of pairs [k, x], where each pair means k*atan(x), and all the terms in the list are summed. Like most other solutions, the program will only check that the tangent of the resulting sum is 1. For instance, <code>Check([[5, 1/2], [5, 1/3]]);</code> returns also <code>true</code>, though the result is 5pi/4.


```gap
TanPlus := function(a, b)
    return (a + b) / (1 - a * b);
end;

TanTimes := function(n, a)
    local x;
    x := 0;
    while n > 0 do
        if IsOddInt(n) then
            x := TanPlus(x, a);
        fi;
        a := TanPlus(a, a);
        n := QuoInt(n, 2);
    od;
    return x;
end;

Check := function(a)
    local x, p;
    x := 0;
    for p in a do
        x := TanPlus(x, SignInt(p[1]) * TanTimes(AbsInt(p[1]), p[2]));
    od;
    return x = 1;
end;

ForAll([
    [[1, 1/2], [1, 1/3]],
    [[2, 1/3], [1, 1/7]],
    [[4, 1/5], [-1, 1/239]],
    [[5, 1/7], [2, 3/79]],
    [[5, 29/278], [7, 3/79]],
    [[1, 1/2], [1, 1/5], [1, 1/8]],
    [[5, 1/7], [4, 1/53], [2, 1/4443]],
    [[6, 1/8], [2, 1/57], [1, 1/239]],
    [[8, 1/10], [-1, 1/239], [-4, 1/515]],
    [[12, 1/18], [8, 1/57], [-5, 1/239]],
    [[16, 1/21], [3, 1/239], [4, 3/1042]],
    [[22, 1/28], [2, 1/443], [-5, 1/1393], [-10, 1/11018]],
    [[22, 1/38], [17, 7/601], [10, 7/8149]],
    [[44, 1/57], [7, 1/239], [-12, 1/682], [24, 1/12943]],
    [[88, 1/172], [51, 1/239], [32, 1/682], [44, 1/5357], [68, 1/12943]]], Check);

Check([[88, 1/172], [51, 1/239], [32, 1/682], [44, 1/5357], [68, 1/12944]]);
```



## Go

```go
package main

import (
    "fmt"
    "math/big"
)

type mTerm struct {
    a, n, d int64
}

var testCases = [][]mTerm{
    {{1, 1, 2}, {1, 1, 3}},
    {{2, 1, 3}, {1, 1, 7}},
    {{4, 1, 5}, {-1, 1, 239}},
    {{5, 1, 7}, {2, 3, 79}},
    {{1, 1, 2}, {1, 1, 5}, {1, 1, 8}},
    {{4, 1, 5}, {-1, 1, 70}, {1, 1, 99}},
    {{5, 1, 7}, {4, 1, 53}, {2, 1, 4443}},
    {{6, 1, 8}, {2, 1, 57}, {1, 1, 239}},
    {{8, 1, 10}, {-1, 1, 239}, {-4, 1, 515}},
    {{12, 1, 18}, {8, 1, 57}, {-5, 1, 239}},
    {{16, 1, 21}, {3, 1, 239}, {4, 3, 1042}},
    {{22, 1, 28}, {2, 1, 443}, {-5, 1, 1393}, {-10, 1, 11018}},
    {{22, 1, 38}, {17, 7, 601}, {10, 7, 8149}},
    {{44, 1, 57}, {7, 1, 239}, {-12, 1, 682}, {24, 1, 12943}},
    {{88, 1, 172}, {51, 1, 239}, {32, 1, 682}, {44, 1, 5357}, {68, 1, 12943}},
    {{88, 1, 172}, {51, 1, 239}, {32, 1, 682}, {44, 1, 5357}, {68, 1, 12944}},
}

func main() {
    for _, m := range testCases {
        fmt.Printf("tan %v = %v\n", m, tans(m))
    }
}

var one = big.NewRat(1, 1)

func tans(m []mTerm) *big.Rat {
    if len(m) == 1 {
        return tanEval(m[0].a, big.NewRat(m[0].n, m[0].d))
    }
    half := len(m) / 2
    a := tans(m[:half])
    b := tans(m[half:])
    r := new(big.Rat)
    return r.Quo(new(big.Rat).Add(a, b), r.Sub(one, r.Mul(a, b)))
}

func tanEval(coef int64, f *big.Rat) *big.Rat {
    if coef == 1 {
        return f
    }
    if coef < 0 {
        r := tanEval(-coef, f)
        return r.Neg(r)
    }
    ca := coef / 2
    cb := coef - ca
    a := tanEval(ca, f)
    b := tanEval(cb, f)
    r := new(big.Rat)
    return r.Quo(new(big.Rat).Add(a, b), r.Sub(one, r.Mul(a, b)))
}
```

Last line edited to show only most significant digits of fraction which is near, but not exactly equal to 1.

```txt

tan [{1 1 2} {1 1 3}] = 1/1
tan [{2 1 3} {1 1 7}] = 1/1
tan [{4 1 5} {-1 1 239}] = 1/1
tan [{5 1 7} {2 3 79}] = 1/1
tan [{1 1 2} {1 1 5} {1 1 8}] = 1/1
tan [{4 1 5} {-1 1 70} {1 1 99}] = 1/1
tan [{5 1 7} {4 1 53} {2 1 4443}] = 1/1
tan [{6 1 8} {2 1 57} {1 1 239}] = 1/1
tan [{8 1 10} {-1 1 239} {-4 1 515}] = 1/1
tan [{12 1 18} {8 1 57} {-5 1 239}] = 1/1
tan [{16 1 21} {3 1 239} {4 3 1042}] = 1/1
tan [{22 1 28} {2 1 443} {-5 1 1393} {-10 1 11018}] = 1/1
tan [{22 1 38} {17 7 601} {10 7 8149}] = 1/1
tan [{44 1 57} {7 1 239} {-12 1 682} {24 1 12943}] = 1/1
tan [{88 1 172} {51 1 239} {32 1 682} {44 1 5357} {68 1 12943}] = 1/1
tan [{88 1 172} {51 1 239} {32 1 682} {44 1 5357} {68 1 12944}] =
100928801... /
100928883...

```



## Haskell


```haskell
import Data.Ratio
import Data.List (foldl')

tanPlus :: Fractional a => a -> a -> a
tanPlus a b = (a + b) / (1 - a * b)

tanEval :: (Integral a, Fractional b) => (a, b) -> b
tanEval (0,_) = 0
tanEval (coef,f)
	| coef < 0 = -tanEval (-coef, f)
	| odd coef = tanPlus f $ tanEval (coef - 1, f)
	| otherwise = tanPlus a a
		where a = tanEval (coef `div` 2, f)

tans :: (Integral a, Fractional b) => [(a, b)] -> b
tans = foldl' tanPlus 0 . map tanEval

machins = [
	[(1, 1%2), (1, 1%3)],
	[(2, 1%3), (1, 1%7)],
	[(12, 1%18), (8, 1%57), (-5, 1%239)],
	[(88, 1%172), (51, 1%239), (32 , 1%682), (44, 1%5357), (68, 1%12943)]]

not_machin = [(88, 1%172), (51, 1%239), (32 , 1%682), (44, 1%5357), (68, 1%12944)]

main = do
	putStrLn "Machins:"
	mapM_ (\x -> putStrLn $ show (tans x) ++ " <-- " ++ show x) machins

	putStr "\nnot Machin: "; print not_machin
	print (tans not_machin)
```


A crazier way to do the above, exploiting the built-in exponentiation algorithms:

```haskell
import Data.Ratio

-- Private type. Do not use outside of the tans function
newtype Tan a = Tan a deriving (Eq, Show)
instance Fractional a => Num (Tan a) where
  _ + _ = undefined
  Tan a * Tan b = Tan $ (a + b) / (1 - a * b)
  negate _ = undefined
  abs _ = undefined
  signum _ = undefined
  fromInteger 1 = Tan 0 -- identity for the (*) above
  fromInteger _ = undefined
instance Fractional a => Fractional (Tan a) where
  fromRational _ = undefined
  recip (Tan f) = Tan (-f) -- inverse for the (*) above

tans :: (Integral a, Fractional b) => [(a, b)] -> b
tans xs = x where
  Tan x = product [Tan f ^^ coef | (coef,f) <- xs]

machins = [
	[(1, 1%2), (1, 1%3)],
	[(2, 1%3), (1, 1%7)],
	[(12, 1%18), (8, 1%57), (-5, 1%239)],
	[(88, 1%172), (51, 1%239), (32 , 1%682), (44, 1%5357), (68, 1%12943)]]

not_machin = [(88, 1%172), (51, 1%239), (32 , 1%682), (44, 1%5357), (68, 1%12944)]

main = do
	putStrLn "Machins:"
	mapM_ (\x -> putStrLn $ show (tans x) ++ " <-- " ++ show x) machins

	putStr "\nnot Machin: "; print not_machin
	print (tans not_machin)
```




## J

'''Solution''':
```j
   machin =: 1r4p1 = [: +/ ({. * _3 o. %/@:}.)"1@:x:
```

'''Example''' (''test cases from task description''):
```j
   R =:  <@:(0&".);._2 ];._2 noun define
  1  1     2
  1  1     3
------------
  2  1     3
  1  1     7
------------
  4  1     5
 _1  1   239
------------
  5  1     7
  2  3    79
------------
  5 29   278
  7  3    79
------------
  1  1     2
  1  1     5
  1  1     8
------------
  4  1     5
 _1  1    70
  1  1    99
------------
  5  1     7
  4  1    53
  2  1  4443
------------
  6  1     8
  2  1    57
  1  1   239
------------
  8  1    10
 _1  1   239
 _4  1   515
------------
 12  1    18
  8  1    57
 _5  1   239
------------
 16  1    21
  3  1   239
  4  3  1042
------------
 22  1    28
  2  1   443
 _5  1  1393
_10  1 11018
------------
 22  1    38
 17  7   601
 10  7  8149
------------
 44  1    57
  7  1   239
_12  1   682
 24  1 12943
------------
 88  1   172
 51  1   239
 32  1   682
 44  1  5357
 68  1 12943
------------
)

   machin&> R
1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
```

'''Example''' (''counterexample''):
```j
   counterExample=. 12944 (<_1;_1)} >{:R
   counterExample  NB. Same as final test case with 12943 incremented to 12944
88 1   172
51 1   239
32 1   682
44 1  5357
68 1 12944
   machin counterExample
0
```

'''Notes''': The function <tt>machin</tt> compares the results of each formula to π/4 (expressed as <tt>1r4p1</tt> in J's numeric notation). The first example above shows the results of these comparisons for each formula (with 1 for true and 0 for false).  In J, arctan is expressed as <tt>_3 o. ''values''</tt> and the function <tt>x:</tt> coerces values to exact representation; thereafter J will maintain exactness throughout its calculations, as long as it can.


## Julia


```julia

using AbstractAlgebra # implements arbitrary precision rationals

tanplus(x,y) = (x + y) / (1 - x * y)

function taneval(coef, frac)
    if coef == 0
        return 0
    elseif coef < 0
        return -taneval(-coef, frac)
    elseif isodd(coef)
        return tanplus(frac, taneval(coef - 1, frac))
    else
        x = taneval(div(coef, 2), frac)
        return tanplus(x, x)
    end
end

taneval(tup::Tuple) = taneval(tup[1], tup[2])

tans(v::Vector{Tuple{BigInt, Rational{BigInt}}}) = foldl(tanplus, map(taneval, v), init=0)

const testmats = Dict{Vector{Tuple{BigInt, Rational{BigInt}}}, Bool}([
    ([(1, 1//2), (1, 1//3)], true), ([(2, 1//3), (1, 1//7)], true),
    ([(12, 1//18), (8, 1//57), (-5, 1//239)], true),
    ([(88, 1//172), (51, 1//239), (32, 1//682), (44, 1//5357), (68, 1//12943)], true),
    ([(88, 1//172), (51, 1//239), (32, 1//682), (44, 1//5357), (68, 1//12944)], false)])


function runtestmats()
    println("Testing matrices:")
    for (k, m) in testmats
        ans = tans(k)
        println((ans == 1) == m ? "Verified as $m: " : "Not Verified as $m: ", "tan $k = $ans")
    end
end

runtestmats()

```
 {{output}}
```txt

Testing matrices:
Verified as true: tan Tuple{BigInt,Rational{BigInt}}[(1, 1//2), (1, 1//3)] = 1//1
Verified as true: tan Tuple{BigInt,Rational{BigInt}}[(2, 1//3), (1, 1//7)] = 1//1
Verified as true: tan Tuple{BigInt,Rational{BigInt}}[(88, 1//172), (51, 1//239), (32, 1//682), (44, 1//5357), (68, 1//12943)] = 1//1
Verified as true: tan Tuple{BigInt,Rational{BigInt}}[(12, 1//18), (8, 1//57), (-5, 1//239)] = 1//1
Verified as false: tan Tuple{BigInt,Rational{BigInt}}[(88, 1//172), (51, 1//239), (32, 1//682), (44, 1//5357), (68, 1//12944)] = 1009288018000944050967896710431587186456256928584351786643498522649995492271475761189348270710224618853590682465929080006511691833816436374107451368838065354726517908250456341991684635768915704374493675498637876700129004484434187627909285979251682006538817341793224963346197503893270875008524149334251672855130857035205217929335932890740051319216343365800342290782260673215928499123722781078448297609548233999010983373327601187505623621602789012550584784738082074783523787011976757247516095289966708782862528690942242793667539020699840402353522108223//1009288837315638583415701528780402795721935641614456853534313491853293025565940011104051964874275710024625850092154664245109626053906509780125743180758231049920425664246286578958307532545458843067352531217230461290763258378749459637420702619029075083089762088232401888676895047947363883809724322868121990870409574061477638203859217672620508200713073485398199091153535700094640095900731630771349477187594074169815106104524371099618096164871416282464532355211521113449237814080332335526420331468258917484010722587072087349909684004660371264507984339711

```



## Kotlin

As the JVM and Kotlin standard libraries lack a BigRational class, I've written one which just provides sufficient functionality to complete this task:

```scala
// version 1.1.3

import java.math.BigInteger

val bigZero = BigInteger.ZERO
val bigOne = BigInteger.ONE

class BigRational : Comparable<BigRational> {

    val num: BigInteger
    val denom: BigInteger

    constructor(n: BigInteger, d: BigInteger) {
        require(d != bigZero)
        var nn = n
        var dd = d
        if (nn == bigZero) {
            dd = bigOne
        }
        else if (dd < bigZero) {
            nn = -nn
            dd = -dd
        }
        val g = nn.gcd(dd)
        if (g > bigOne) {
            nn /= g
            dd /= g
        }
        num = nn
        denom = dd
    }

    constructor(n: Long, d: Long) : this(BigInteger.valueOf(n), BigInteger.valueOf(d))

    operator fun plus(other: BigRational) =
        BigRational(num * other.denom + denom * other.num, other.denom * denom)

    operator fun unaryMinus() = BigRational(-num, denom)

    operator fun minus(other: BigRational) = this + (-other)

    operator fun times(other: BigRational) = BigRational(this.num * other.num, this.denom * other.denom)

    fun inverse(): BigRational {
        require(num != bigZero)
        return BigRational(denom, num)
    }

    operator fun div(other: BigRational) = this * other.inverse()

    override fun compareTo(other: BigRational): Int {
        val diff = this - other
        return when {
            diff.num < bigZero -> -1
            diff.num > bigZero -> +1
            else               ->  0
        }
    }

    override fun equals(other: Any?): Boolean {
       if (other == null || other !is BigRational) return false
       return this.compareTo(other) == 0
    }

    override fun toString() = if (denom == bigOne) "$num" else "$num/$denom"

    companion object {
        val ZERO = BigRational(bigZero, bigOne)
        val ONE  = BigRational(bigOne, bigOne)
    }
}

/** represents a term of the form: c * atan(n / d) */
class Term(val c: Long, val n: Long, val d: Long) {

    override fun toString() = when {
        c ==  1L   -> " + "
        c == -1L   -> " - "
        c <   0L   -> " - ${-c}*"
        else       -> " + $c*"
    } + "atan($n/$d)"
}

val one = BigRational.ONE

fun tanSum(terms: List<Term>): BigRational {
    if (terms.size == 1) return tanEval(terms[0].c, BigRational(terms[0].n, terms[0].d))
    val half = terms.size / 2
    val a = tanSum(terms.take(half))
    val b = tanSum(terms.drop(half))
    return (a + b) / (one - (a * b))
}

fun tanEval(c: Long, f: BigRational): BigRational {
    if (c == 1L)  return f
    if (c < 0L) return -tanEval(-c, f)
    val ca = c / 2
    val cb = c - ca
    val a = tanEval(ca, f)
    val b = tanEval(cb, f)
    return (a + b) / (one - (a * b))
}

fun main(args: Array<String>) {
    val termsList = listOf(
        listOf(Term(1, 1, 2), Term(1, 1, 3)),
        listOf(Term(2, 1, 3), Term(1, 1, 7)),
        listOf(Term(4, 1, 5), Term(-1, 1, 239)),
        listOf(Term(5, 1, 7), Term(2, 3, 79)),
        listOf(Term(5, 29, 278), Term(7, 3, 79)),
        listOf(Term(1, 1, 2), Term(1, 1, 5), Term(1, 1, 8)),
        listOf(Term(4, 1, 5), Term(-1, 1, 70), Term(1, 1, 99)),
        listOf(Term(5, 1, 7), Term(4, 1, 53), Term(2, 1, 4443)),
        listOf(Term(6, 1, 8), Term(2, 1, 57), Term(1, 1, 239)),
        listOf(Term(8, 1, 10), Term(-1, 1, 239), Term(-4, 1, 515)),
        listOf(Term(12, 1, 18), Term(8, 1, 57), Term(-5, 1, 239)),
        listOf(Term(16, 1, 21), Term(3, 1, 239), Term(4, 3, 1042)),
        listOf(Term(22, 1, 28), Term(2, 1, 443), Term(-5, 1, 1393), Term(-10, 1, 11018)),
        listOf(Term(22, 1, 38), Term(17, 7, 601), Term(10, 7, 8149)),
        listOf(Term(44, 1, 57), Term(7, 1, 239), Term(-12, 1, 682), Term(24, 1, 12943)),
        listOf(Term(88, 1, 172), Term(51, 1, 239), Term(32, 1, 682), Term(44, 1, 5357), Term(68, 1, 12943)),
        listOf(Term(88, 1, 172), Term(51, 1, 239), Term(32, 1, 682), Term(44, 1, 5357), Term(68, 1, 12944))
    )

    for (terms in termsList) {
        val f = String.format("%-5s << 1 == tan(", tanSum(terms) == one)
        print(f)
        print(terms[0].toString().drop(3))
        for (i in 1 until terms.size) print(terms[i])
        println(")")
    }
}
```


```txt

true  << 1 == tan(atan(1/2) + atan(1/3))
true  << 1 == tan(2*atan(1/3) + atan(1/7))
true  << 1 == tan(4*atan(1/5) - atan(1/239))
true  << 1 == tan(5*atan(1/7) + 2*atan(3/79))
true  << 1 == tan(5*atan(29/278) + 7*atan(3/79))
true  << 1 == tan(atan(1/2) + atan(1/5) + atan(1/8))
true  << 1 == tan(4*atan(1/5) - atan(1/70) + atan(1/99))
true  << 1 == tan(5*atan(1/7) + 4*atan(1/53) + 2*atan(1/4443))
true  << 1 == tan(6*atan(1/8) + 2*atan(1/57) + atan(1/239))
true  << 1 == tan(8*atan(1/10) - atan(1/239) - 4*atan(1/515))
true  << 1 == tan(12*atan(1/18) + 8*atan(1/57) - 5*atan(1/239))
true  << 1 == tan(16*atan(1/21) + 3*atan(1/239) + 4*atan(3/1042))
true  << 1 == tan(22*atan(1/28) + 2*atan(1/443) - 5*atan(1/1393) - 10*atan(1/11018))
true  << 1 == tan(22*atan(1/38) + 17*atan(7/601) + 10*atan(7/8149))
true  << 1 == tan(44*atan(1/57) + 7*atan(1/239) - 12*atan(1/682) + 24*atan(1/12943))
true  << 1 == tan(88*atan(1/172) + 51*atan(1/239) + 32*atan(1/682) + 44*atan(1/5357) + 68*atan(1/12943))
false << 1 == tan(88*atan(1/172) + 51*atan(1/239) + 32*atan(1/682) + 44*atan(1/5357) + 68*atan(1/12944))

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
<lang>Tan[ArcTan[1/2] + ArcTan[1/3]] == 1
Tan[2 ArcTan[1/3] + ArcTan[1/7]] == 1
Tan[4 ArcTan[1/5] - ArcTan[1/239]] == 1
Tan[5 ArcTan[1/7] + 2 ArcTan[3/79]] == 1
Tan[5 ArcTan[29/278] + 7 ArcTan[3/79]] == 1
Tan[ArcTan[1/2] + ArcTan[1/5] + ArcTan[1/8]] == 1
Tan[4 ArcTan[1/5] - ArcTan[1/70] + ArcTan[1/99]] == 1
Tan[5 ArcTan[1/7] + 4 ArcTan[1/53] + 2 ArcTan[1/4443]] == 1
Tan[6 ArcTan[1/8] + 2 ArcTan[1/57] + ArcTan[1/239]] == 1
Tan[8 ArcTan[1/10] - ArcTan[1/239] - 4 ArcTan[1/515]] == 1
Tan[12 ArcTan[1/18] + 8 ArcTan[1/57] - 5 ArcTan[1/239]] == 1
Tan[16 ArcTan[1/21] + 3 ArcTan[1/239] + 4 ArcTan[3/1042]] == 1
Tan[22 ArcTan[1/28] + 2 ArcTan[1/443] - 5 ArcTan[1/1393] -
   10 ArcTan[1/11018]] == 1
Tan[22 ArcTan[1/38] + 17 ArcTan[7/601] + 10 ArcTan[7/8149]] == 1
Tan[44 ArcTan[1/57] + 7 ArcTan[1/239] - 12 ArcTan[1/682] +
   24 ArcTan[1/12943]] == 1
Tan[88 ArcTan[1/172] + 51 ArcTan[1/239] + 32 ArcTan[1/682] +
   44 ArcTan[1/5357] + 68 ArcTan[1/12943]] == 1
Tan[88 ArcTan[1/172] + 51 ArcTan[1/239] + 32 ArcTan[1/682] +
   44 ArcTan[1/5357] + 68 ArcTan[1/12944]] == 1
```


```txt
True

True

True

True

True

True

True

True

True

True

True

True

True

True

True

True

False
```



## Maxima


```maxima
trigexpand:true$
is(tan(atan(1/2)+atan(1/3))=1);
is(tan(2*atan(1/3)+atan(1/7))=1);
is(tan(4*atan(1/5)-atan(1/239))=1);
is(tan(5*atan(1/7)+2*atan(3/79))=1);
is(tan(5*atan(29/278)+7*atan(3/79))=1);
is(tan(atan(1/2)+atan(1/5)+atan(1/8))=1);
is(tan(4*atan(1/5)-atan(1/70)+atan(1/99))=1);
is(tan(5*atan(1/7)+4*atan(1/53)+2*atan(1/4443))=1);
is(tan(6*atan(1/8)+2*atan(1/57)+atan(1/239))=1);
is(tan(8*atan(1/10)-atan(1/239)-4*atan(1/515))=1);
is(tan(12*atan(1/18)+8*atan(1/57)-5*atan(1/239))=1);
is(tan(16*atan(1/21)+3*atan(1/239)+4*atan(3/1042))=1);
is(tan(22*atan(1/28)+2*atan(1/443)-5*atan(1/1393)-10*atan(1/11018))=1);
is(tan(22*atan(1/38)+17*atan(7/601)+10*atan(7/8149))=1);
is(tan(44*atan(1/57)+7*atan(1/239)-12*atan(1/682)+24*atan(1/12943))=1);
is(tan(88*atan(1/172)+51*atan(1/239)+32*atan(1/682)+44*atan(1/5357)+68*atan(1/12943))=1);
is(tan(88*atan(1/172)+51*atan(1/239)+32*atan(1/682)+44*atan(1/5357)+68*atan(1/12944))=1);
```

```txt
(%i2)
(%o2)                                true
(%i3)
(%o3)                                true
(%i4)
(%o4)                                true
(%i5)
(%o5)                                true
(%i6)
(%o6)                                true
(%i7)
(%o7)                                true
(%i8)
(%o8)                                true
(%i9)
(%o9)                                true
(%i10)
(%o10)                               true
(%i11)
(%o11)                               true
(%i12)
(%o12)                               true
(%i13)
(%o13)                               true
(%i14)
(%o14)                               true
(%i15)
(%o15)                               true
(%i16)
(%o16)                               true
(%i17)
(%o17)                               true
(%i18)
(%o18)                               false
```




## OCaml


```ocaml
open Num;; (* use exact rationals for results *)

let tadd p q = (p +/ q) // ((Int 1) -/ (p */ q)) in

(* tan(n*arctan(a/b)) *)
let rec tan_expr (n,a,b) =
  if n = 1 then (Int a)//(Int b) else
  if n = -1 then (Int (-a))//(Int b) else
    let m = n/2 in
    let tm = tan_expr (m,a,b) in
    let m2 = tadd tm tm and k = n-m-m in
    if k = 0 then m2 else tadd (tan_expr (k,a,b)) m2 in

let verify (k, tlist) =
  Printf.printf "Testing: pi/%d = " k;
  let t_str = List.map (fun (x,y,z) -> Printf.sprintf "%d*atan(%d/%d)" x y z) tlist in
  print_endline (String.concat " + " t_str);
  let ans_terms = List.map tan_expr tlist in
  let answer = List.fold_left tadd (Int 0) ans_terms in
  Printf.printf "  tan(RHS) is %s\n" (if answer = (Int 1) then "one" else "not one") in

(* example: prog 4 5 29 278 7 3 79 represents pi/4 = 5*atan(29/278) + 7*atan(3/79) *)
let args = Sys.argv in
let nargs = Array.length args in
let v k = int_of_string args.(k) in
let rec triples n =
  if n+2 > nargs-1 then []
  else (v n, v (n+1), v (n+2)) :: triples (n+3) in
if nargs > 4 then
let dat = (v 1, triples 2) in
verify dat
else
List.iter verify [
  (4,[(1,1,2);(1,1,3)]);
  (4,[(2,1,3);(1,1,7)]);
  (4,[(4,1,5);(-1,1,239)]);
  (4,[(5,1,7);(2,3,79)]);
  (4,[(5,29,278);(7,3,79)]);
  (4,[(1,1,2);(1,1,5);(1,1,8)]);
  (4,[(4,1,5);(-1,1,70);(1,1,99)]);
  (4,[(5,1,7);(4,1,53);(2,1,4443)]);
  (4,[(6,1,8);(2,1,57);(1,1,239)]);
  (4,[(8,1,10);(-1,1,239);(-4,1,515)]);
  (4,[(12,1,18);(8,1,57);(-5,1,239)]);
  (4,[(16,1,21);(3,1,239);(4,3,1042)]);
  (4,[(22,1,28);(2,1,443);(-5,1,1393);(-10,1,11018)]);
  (4,[(22,1,38);(17,7,601);(10,7,8149)]);
  (4,[(44,1,57);(7,1,239);(-12,1,682);(24,1,12943)]);
  (4,[(88,1,172);(51,1,239);(32,1,682);(44,1,5357);(68,1,12943)]);
  (4,[(88,1,172);(51,1,239);(32,1,682);(44,1,5357);(68,1,12944)])
]
```


Compile with
 ocamlopt -o verify_machin.opt nums.cmxa verify_machin.ml
or run with
 ocaml nums.cma verify_machin.ml

```txt

Testing: pi/4 = 1*atan(1/2) + 1*atan(1/3)
  tan(RHS) is one
Testing: pi/4 = 2*atan(1/3) + 1*atan(1/7)
  tan(RHS) is one
Testing: pi/4 = 4*atan(1/5) + -1*atan(1/239)
  tan(RHS) is one
Testing: pi/4 = 5*atan(1/7) + 2*atan(3/79)
  tan(RHS) is one
Testing: pi/4 = 5*atan(29/278) + 7*atan(3/79)
  tan(RHS) is one
Testing: pi/4 = 1*atan(1/2) + 1*atan(1/5) + 1*atan(1/8)
  tan(RHS) is one
Testing: pi/4 = 4*atan(1/5) + -1*atan(1/70) + 1*atan(1/99)
  tan(RHS) is one
Testing: pi/4 = 5*atan(1/7) + 4*atan(1/53) + 2*atan(1/4443)
  tan(RHS) is one
Testing: pi/4 = 6*atan(1/8) + 2*atan(1/57) + 1*atan(1/239)
  tan(RHS) is one
Testing: pi/4 = 8*atan(1/10) + -1*atan(1/239) + -4*atan(1/515)
  tan(RHS) is one
Testing: pi/4 = 12*atan(1/18) + 8*atan(1/57) + -5*atan(1/239)
  tan(RHS) is one
Testing: pi/4 = 16*atan(1/21) + 3*atan(1/239) + 4*atan(3/1042)
  tan(RHS) is one
Testing: pi/4 = 22*atan(1/28) + 2*atan(1/443) + -5*atan(1/1393) + -10*atan(1/11018)
  tan(RHS) is one
Testing: pi/4 = 22*atan(1/38) + 17*atan(7/601) + 10*atan(7/8149)
  tan(RHS) is one
Testing: pi/4 = 44*atan(1/57) + 7*atan(1/239) + -12*atan(1/682) + 24*atan(1/12943)
  tan(RHS) is one
Testing: pi/4 = 88*atan(1/172) + 51*atan(1/239) + 32*atan(1/682) + 44*atan(1/5357) + 68*atan(1/12943)
  tan(RHS) is one
Testing: pi/4 = 88*atan(1/172) + 51*atan(1/239) + 32*atan(1/682) + 44*atan(1/5357) + 68*atan(1/12944)
  tan(RHS) is not one

```



## ooRexx


```oorexx
/*REXX ----------------------------------------------------------------
* 09.04.2014 Walter Pachl  the REXX solution adapted for ooRexx
*                          which provides a function package rxMath
*--------------------------------------------------------------------*/
Numeric Digits 16
Numeric Fuzz   3;    pi=rxCalcpi();  a.=''
 a.1 = 'pi/4 =    rxCalcarctan(1/2,16,'R')    +    rxCalcarctan(1/3,16,'R')'
 a.2 = 'pi/4 =  2*rxCalcarctan(1/3,16,'R')    +    rxCalcarctan(1/7,16,'R')'
 a.3 = 'pi/4 =  4*rxCalcarctan(1/5,16,'R')    -    rxCalcarctan(1/239,16,'R')'
 a.4 = 'pi/4 =  5*rxCalcarctan(1/7,16,'R')    +  2*rxCalcarctan(3/79,16,'R')'
 a.5 = 'pi/4 =  5*rxCalcarctan(29/278,16,'R') +  7*rxCalcarctan(3/79,16,'R')'
 a.6 = 'pi/4 =  rxCalcarctan(1/2,16,'R')      +    rxCalcarctan(1/5,16,'R')   +    rxCalcarctan(1/8,16,'R')'
 a.7 = 'pi/4 =  4*rxCalcarctan(1/5,16,'R')    -    rxCalcarctan(1/70,16,'R')  +    rxCalcarctan(1/99,16,'R')'
 a.8 = 'pi/4 =  5*rxCalcarctan(1/7,16,'R')    +  4*rxCalcarctan(1/53,16,'R')  +  2*rxCalcarctan(1/4443,16,'R')'
 a.9 = 'pi/4 =  6*rxCalcarctan(1/8,16,'R')    +  2*rxCalcarctan(1/57,16,'R')  +    rxCalcarctan(1/239,16,'R')'
a.10 = 'pi/4 =  8*rxCalcarctan(1/10,16,'R')   -    rxCalcarctan(1/239,16,'R') -  4*rxCalcarctan(1/515,16,'R')'
a.11 = 'pi/4 = 12*rxCalcarctan(1/18,16,'R')   +  8*rxCalcarctan(1/57,16,'R')  -  5*rxCalcarctan(1/239,16,'R')'
a.12 = 'pi/4 = 16*rxCalcarctan(1/21,16,'R')   +  3*rxCalcarctan(1/239,16,'R') +  4*rxCalcarctan(3/1042,16,'R')'
a.13 = 'pi/4 = 22*rxCalcarctan(1/28,16,'R')   +  2*rxCalcarctan(1/443,16,'R') -  5*rxCalcarctan(1/1393,16,'R') - 10*rxCalcarctan(1/11018,16,'R')'
a.14 = 'pi/4 = 22*rxCalcarctan(1/38,16,'R')   + 17*rxCalcarctan(7/601,16,'R') + 10*rxCalcarctan(7/8149,16,'R')'
a.15 = 'pi/4 = 44*rxCalcarctan(1/57,16,'R')   +  7*rxCalcarctan(1/239,16,'R') - 12*rxCalcarctan(1/682,16,'R')  + 24*rxCalcarctan(1/12943,16,'R')'
a.16 = 'pi/4 = 88*rxCalcarctan(1/172,16,'R')  + 51*rxCalcarctan(1/239,16,'R') + 32*rxCalcarctan(1/682,16,'R')  + 44*rxCalcarctan(1/5357,16,'R')  + 68*rxCalcarctan(1/12943,16,'R')'
a.17 = 'pi/4 = 88*rxCalcarctan(1/172,16,'R')  + 51*rxCalcarctan(1/239,16,'R') + 32*rxCalcarctan(1/682,16,'R')  + 44*rxCalcarctan(1/5357,16,'R')  + 68*rxCalcarctan(1/12944,16,'R')'
        do j=1  while  a.j\==''        /*evaluate each of the formulas. */
        interpret  'answer='   "("   a.j   ")"      /*the heavy lifting.*/
        say  right(word('bad OK',answer+1),3)": "     space(a.j,0)
        end   /*j*/                    /* [?]  show OK | bad, formula.  */
::requires rxmath library

```

```txt
 OK:  pi/4=rxCalcarctan(1/2,16,R)+rxCalcarctan(1/3,16,R)
 OK:  pi/4=2*rxCalcarctan(1/3,16,R)+rxCalcarctan(1/7,16,R)
 OK:  pi/4=4*rxCalcarctan(1/5,16,R)-rxCalcarctan(1/239,16,R)
 OK:  pi/4=5*rxCalcarctan(1/7,16,R)+2*rxCalcarctan(3/79,16,R)
 OK:  pi/4=5*rxCalcarctan(29/278,16,R)+7*rxCalcarctan(3/79,16,R)
 OK:  pi/4=rxCalcarctan(1/2,16,R)+rxCalcarctan(1/5,16,R)+rxCalcarctan(1/8,16,R)
 OK:  pi/4=4*rxCalcarctan(1/5,16,R)-rxCalcarctan(1/70,16,R)+rxCalcarctan(1/99,16,R)
 OK:  pi/4=5*rxCalcarctan(1/7,16,R)+4*rxCalcarctan(1/53,16,R)+2*rxCalcarctan(1/4443,16,R)
 OK:  pi/4=6*rxCalcarctan(1/8,16,R)+2*rxCalcarctan(1/57,16,R)+rxCalcarctan(1/239,16,R)
 OK:  pi/4=8*rxCalcarctan(1/10,16,R)-rxCalcarctan(1/239,16,R)-4*rxCalcarctan(1/515,16,R)
 OK:  pi/4=12*rxCalcarctan(1/18,16,R)+8*rxCalcarctan(1/57,16,R)-5*rxCalcarctan(1/239,16,R)
 OK:  pi/4=16*rxCalcarctan(1/21,16,R)+3*rxCalcarctan(1/239,16,R)+4*rxCalcarctan(3/1042,16,R)
 OK:  pi/4=22*rxCalcarctan(1/28,16,R)+2*rxCalcarctan(1/443,16,R)-5*rxCalcarctan(1/1393,16,R)-10*rxCalcarctan(1/11018,16,R)
 OK:  pi/4=22*rxCalcarctan(1/38,16,R)+17*rxCalcarctan(7/601,16,R)+10*rxCalcarctan(7/8149,16,R)
 OK:  pi/4=44*rxCalcarctan(1/57,16,R)+7*rxCalcarctan(1/239,16,R)-12*rxCalcarctan(1/682,16,R)+24*rxCalcarctan(1/12943,16,R)
 OK:  pi/4=88*rxCalcarctan(1/172,16,R)+51*rxCalcarctan(1/239,16,R)+32*rxCalcarctan(1/682,16,R)+44*rxCalcarctan(1/5357,16,R)+68*rxCalcarctan(1/12943,16,R)
bad:  pi/4=88*rxCalcarctan(1/172,16,R)+51*rxCalcarctan(1/239,16,R)+32*rxCalcarctan(1/682,16,R)+44*rxCalcarctan(1/5357,16,R)+68*rxCalcarctan(1/12944,16,R)
```



## PARI/GP


```parigp
tanEval(coef, f)={
	if (coef <= 1, return(if(coef<1,-tanEval(-coef, f),f)));
	my(a=tanEval(coef\2, f), b=tanEval(coef-coef\2, f));
	(a + b)/(1 - a*b)
};
tans(xs)={
	if (#xs == 1, return(tanEval(xs[1][1], xs[1][2])));
	my(a=tans(xs[1..#xs\2]),b=tans(xs[#xs\2+1..#xs]));
	(a + b)/(1 - a*b)
};
test(v)={
	my(t=tans(v));
	if(t==1,print("OK"),print("Error: "v))
};
test([[1,1/2],[1,1/3]]);
test([[2,1/3],[1,1/7]]);
test([[4,1/5],[-1,1/239]]);
test([[5,1/7],[2,3/79]]);
test([[5,29/278],[7,3/79]]);
test([[1,1/2],[1,1/5],[1,1/8]]);
test([[4,1/5],[-1,1/70],[1,1/99]]);
test([[5,1/7],[4,1/53],[2,1/4443]]);
test([[6,1/8],[2,1/57],[1,1/239]]);
test([[8,1/10],[-1,1/239],[-4,1/515]]);
test([[12,1/18],[8,1/57],[-5,1/239]]);
test([[16,1/21],[3,1/239],[4,3/1042]]);
test([[22,1/28],[2,1/443],[-5,1/1393],[-10,1/11018]]);
test([[22,1/38],[17,7/601],[10,7/8149]]);
test([[44,1/57],[7,1/239],[-12,1/682],[24,1/12943]]);
test([[88,1/172],[51,1/239],[32,1/682],[44,1/5357],[68,1/12943]]);
test([[88,1/172],[51,1/239],[32,1/682],[44,1/5357],[68,1/12944]]);
```

```txt
OK
OK
OK
OK
OK
OK
OK
OK
OK
OK
OK
OK
OK
OK
OK
OK
Error: [[88, 1/172], [51, 1/239], [32, 1/682], [44, 1/5357], [68, 1/12944]]
```



## Perl


```perl
use Math::BigRat try=
"GMP";

sub taneval {
  my($coef,$f) = @_;
  $f = Math::BigRat->new($f) unless ref($f);
  return 0 if $coef == 0;
  return $f if $coef == 1;
  return -taneval(-$coef, $f) if $coef < 0;
  my($a,$b) = ( taneval($coef>>1, $f), taneval($coef-($coef>>1),$f) );
  ($a+$b)/(1-$a*$b);
}

sub tans {
  my @xs=@_;
  return taneval(@{$xs[0]}) if scalar(@xs)==1;
  my($a,$b) = ( tans(@xs[0..($#xs>>1)]), tans(@xs[($#xs>>1)+1..$#xs]) );
  ($a+$b)/(1-$a*$b);
}

sub test {
  printf "%5s (%s)\n", (tans(@_)==1)?"OK":"Error", join(" ",map{"[@$_]"} @_);
}

test([1,'1/2'], [1,'1/3']);
test([2,'1/3'], [1,'1/7']);
test([4,'1/5'], [-1,'1/239']);
test([5,'1/7'],[2,'3/79']);
test([5,'29/278'],[7,'3/79']);
test([1,'1/2'],[1,'1/5'],[1,'1/8']);
test([4,'1/5'],[-1,'1/70'],[1,'1/99']);
test([5,'1/7'],[4,'1/53'],[2,'1/4443']);
test([6,'1/8'],[2,'1/57'],[1,'1/239']);
test([8,'1/10'],[-1,'1/239'],[-4,'1/515']);
test([12,'1/18'],[8,'1/57'],[-5,'1/239']);
test([16,'1/21'],[3,'1/239'],[4,'3/1042']);
test([22,'1/28'],[2,'1/443'],[-5,'1/1393'],[-10,'1/11018']);
test([22,'1/38'],[17,'7/601'],[10,'7/8149']);
test([44,'1/57'],[7,'1/239'],[-12,'1/682'],[24,'1/12943']);
test([88,'1/172'],[51,'1/239'],[32,'1/682'],[44,'1/5357'],[68,'1/12943']);
test([88,'1/172'],[51,'1/239'],[32,'1/682'],[44,'1/5357'],[68,'1/12944']);
```

```txt
   OK ([1 1/2] [1 1/3])
   OK ([2 1/3] [1 1/7])
   OK ([4 1/5] [-1 1/239])
   OK ([5 1/7] [2 3/79])
   OK ([5 29/278] [7 3/79])
   OK ([1 1/2] [1 1/5] [1 1/8])
   OK ([4 1/5] [-1 1/70] [1 1/99])
   OK ([5 1/7] [4 1/53] [2 1/4443])
   OK ([6 1/8] [2 1/57] [1 1/239])
   OK ([8 1/10] [-1 1/239] [-4 1/515])
   OK ([12 1/18] [8 1/57] [-5 1/239])
   OK ([16 1/21] [3 1/239] [4 3/1042])
   OK ([22 1/28] [2 1/443] [-5 1/1393] [-10 1/11018])
   OK ([22 1/38] [17 7/601] [10 7/8149])
   OK ([44 1/57] [7 1/239] [-12 1/682] [24 1/12943])
   OK ([88 1/172] [51 1/239] [32 1/682] [44 1/5357] [68 1/12943])
Error ([88 1/172] [51 1/239] [32 1/682] [44 1/5357] [68 1/12944])
```



## Perl 6

The coercion to FatRat provides for exact computation for all input.

```perl6
sub taneval ($coef, $f) {
  return 0 if $coef == 0;
  return $f if $coef == 1;
  return -taneval(-$coef, $f) if $coef < 0;

  my $a = taneval($coef+>1, $f);
  my $b = taneval($coef - $coef+>1, $f);
  ($a+$b)/(1-$a*$b);
}

sub tans (@xs) {
  return taneval(@xs[0;0], @xs[0;1].FatRat) if @xs == 1;

  my $a = tans(@xs[0 .. (-1+@xs+>1)]);
  my $b = tans(@xs[(-1+@xs+>1)+1 .. -1+@xs]);
  ($a+$b)/(1-$a*$b);
}

sub verify (@eqn) {
  printf "%5s (%s)\n", (tans(@eqn) == 1) ?? "OK" !! "Error",
    (map { "[{.[0]} {.[1].nude.join('/')}]" }, @eqn).join(' ');
}

verify($_) for
   ([[1,1/2], [1,1/3]],
    [[2,1/3], [1,1/7]],
    [[4,1/5], [-1,1/239]],
    [[5,1/7], [2,3/79]],
    [[5,29/278], [7,3/79]],
    [[1,1/2], [1,1/5], [1,1/8]],
    [[4,1/5], [-1,1/70], [1,1/99]],
    [[5,1/7], [4,1/53], [2,1/4443]],
    [[6,1/8], [2,1/57], [1,1/239]],
    [[8,1/10], [-1,1/239], [-4,1/515]],
    [[12,1/18], [8,1/57], [-5,1/239]],
    [[16,1/21], [3,1/239], [4,3/1042]],
    [[22,1/28], [2,1/443], [-5,1/1393], [-10,1/11018]],
    [[22,1/38], [17,7/601], [10,7/8149]],
    [[44,1/57], [7,1/239], [-12,1/682], [24,1/12943]],
    [[88,1/172], [51,1/239], [32,1/682], [44,1/5357], [68,1/12943]],
    [[88,1/172], [51,1/239], [32,1/682], [44,1/5357], [68,1/21944]]
    );
```

```txt
   OK ([1 1/2] [1 1/3])
   OK ([2 1/3] [1 1/7])
   OK ([4 1/5] [-1 1/239])
   OK ([5 1/7] [2 3/79])
   OK ([5 29/278] [7 3/79])
   OK ([1 1/2] [1 1/5] [1 1/8])
   OK ([4 1/5] [-1 1/70] [1 1/99])
   OK ([5 1/7] [4 1/53] [2 1/4443])
   OK ([6 1/8] [2 1/57] [1 1/239])
   OK ([8 1/10] [-1 1/239] [-4 1/515])
   OK ([12 1/18] [8 1/57] [-5 1/239])
   OK ([16 1/21] [3 1/239] [4 3/1042])
   OK ([22 1/28] [2 1/443] [-5 1/1393] [-10 1/11018])
   OK ([22 1/38] [17 7/601] [10 7/8149])
   OK ([44 1/57] [7 1/239] [-12 1/682] [24 1/12943])
   OK ([88 1/172] [51 1/239] [32 1/682] [44 1/5357] [68 1/12943])
Error ([88 1/172] [51 1/239] [32 1/682] [44 1/5357] [68 1/21944])
```



## Phix


### Naieve version

Hint: rather than test tan(a) for 1.0, test whether the sprint of it, which is rounded to 10 significant digits, is "1.0".

The failing test case, I believe, is only accurate to 6 (or perhaps 7, see fractions output) significant digits.

```Phix
procedure test(atom a)
    if -3*PI/4 >= a then ?9/0 end if
    if  5*PI/4 <= a then ?9/0 end if
    string s = sprint(tan(a))
    ?s -- or test for "1.0", but not 1.0
end procedure
test(   arctan(1 /   2) +    arctan(1 /   3))
test( 2*arctan(1 /   3) +    arctan(1 /   7))
test( 4*arctan(1 /   5) -    arctan(1 / 239))
test( 5*arctan(1 /   7) +  2*arctan(3 /  79))
test( 5*arctan(29/ 278) +  7*arctan(3 /  79))
test(   arctan(1 /   2) +    arctan(1 /   5) +   arctan(1 /    8))
test( 4*arctan(1 /   5) -    arctan(1 /  70) +   arctan(1 /   99))
test( 5*arctan(1 /   7) +  4*arctan(1 /  53) + 2*arctan(1 / 4443))
test( 6*arctan(1 /   8) +  2*arctan(1 /  57) +   arctan(1 /  239))
test( 8*arctan(1 /  10) -    arctan(1 / 239) - 4*arctan(1 /  515))
test(12*arctan(1 /  18) +  8*arctan(1 /  57) - 5*arctan(1 /  239))
test(16*arctan(1 /  21) +  3*arctan(1 / 239) + 4*arctan(3 / 1042))
test(22*arctan(1 /  28) +  2*arctan(1 / 443) - 5*arctan(1 / 1393) - 10*arctan(1 / 11018))
test(22*arctan(1 /  38) + 17*arctan(7 / 601) +10*arctan(7 / 8149))
test(44*arctan(1 /  57) +  7*arctan(1 / 239) -12*arctan(1 /  682) + 24*arctan(1 / 12943))
test(88*arctan(1 / 172) + 51*arctan(1 / 239) +32*arctan(1 /  682) + 44*arctan(1 /  5357) + 68*arctan(1 / 12943))
?"==="
test(88*arctan(1 / 172) + 51*arctan(1 / 239) + 32*arctan(1 / 682) + 44*arctan(1 /  5357) + 68*arctan(1 / 12944))
```

```txt

1.0
1.0
1.0
1.0
1.0
1.0
1.0
1.0
1.0
1.0
1.0
1.0
1.0
1.0
1.0
1.0
"==="
0.9999991882

```



### Using proper fractions

Routines adapted from [[Arithmetic/Rational#Phix]]


```Phix
include builtins\pfrac.e -- (provisional/0.8.0+)

function tans(sequence x)
    frac a,b
    integer h
    if length(x)=1 then
        {integer m, frac f} = x[1]
        if m=1 then
            return f
        elsif m<0 then
            return frac_uminus(tans({{-m,f}}))
        end if
        h = floor(m/2)
        a = tans({{h,f}})
        b = tans({{m-h,f}})
    else
        h = floor(length(x)/2)
        a = tans(x[1..h])
        b = tans(x[h+1..$])
    end if
    return frac_div(frac_add(a,b) , frac_sub(frac_new(1),frac_mul(a,b)))
end function

function parse(string formula)
-- obviously the error handling here is a bit brutal...
sequence res = {}, r
integer m,n,d
    formula = substitute(formula," ","") -- strip spaces
    if formula[1..5]!="pi/4=" then ?9/0 end if
    formula = formula[6..$]
    res = {}
    while length(formula) do
        integer sgn = +1
        switch formula[1] do
            case '-': sgn = -1; fallthrough
            case '+': formula = formula[2..$]
        end switch
        if formula[1]='a' then
            m = sgn
        else
            r = scanf(formula,"%d*%s")
            if length(r)!=1 then ?9/0 end if
            {m,formula} = r[1]
            m *= sgn
        end if
        r = scanf(formula,"arctan(%d/%d)%s")
        if length(r)!=1 then ?9/0 end if
        {n,d,formula} = r[1]
        res = append(res,{m,frac_new(n,d)})
    end while
    return res
end function

procedure test(string formula)
    frac f = tans(parse(formula))
    if frac_eq(f,frac_one) then
        printf(1,"OK: %s\n",{formula})
    else
        printf(1,"ERROR: %s\n",{formula})
        printf(1,"  %s\n\\ %s\n",frac_sprint(f,asPair:=true))
    end if
end procedure

constant formulae = {"pi/4 = arctan(1/2) + arctan(1/3)",
                     "pi/4 = 2*arctan(1/3) + arctan(1/7)",
                     "pi/4 = 4*arctan(1/5) - arctan(1/239)",
                     "pi/4 = 5*arctan(1/7) + 2*arctan(3/79)",
                     "pi/4 = 5*arctan(29/278) + 7*arctan(3/79)",
                     "pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)",
                     "pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)",
                     "pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)",
                     "pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)",
                     "pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)",
                     "pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)",
                     "pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)",
                     "pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)",
                     "pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)",
                     "pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)",
                     "pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)",
                     "pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)"}

for i=1 to length(formulae) do
    test(formulae[i])
end for
```

Last line manually edited (both numerator and denominator were 550-digit numbers).

```txt

OK: pi/4 = arctan(1/2) + arctan(1/3)
OK: pi/4 = 2*arctan(1/3) + arctan(1/7)
OK: pi/4 = 4*arctan(1/5) - arctan(1/239)
OK: pi/4 = 5*arctan(1/7) + 2*arctan(3/79)
OK: pi/4 = 5*arctan(29/278) + 7*arctan(3/79)
OK: pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)
OK: pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)
OK: pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)
OK: pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)
OK: pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)
OK: pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)
OK: pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)
OK: pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)
OK: pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)
OK: pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)
OK: pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)
ERROR: pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)
  10092880180009440509678967104315871864562569285843 ... 82862528690942242793667539020699840402353522108223
\ 10092888373156385834157015287804027957219356416144 ... 84010722587072087349909684004660371264507984339711

```



## Python

This example parses the [http://rosettacode.org/mw/index.php?title=Check_Machin-like_formulas&oldid=146749 original] equations to form an intermediate representation then does the checks.

Function tans and tanEval are translations of the Haskel functions of the same names.

```python
import re
from fractions import Fraction
from pprint import pprint as pp


equationtext = '''\
  pi/4 = arctan(1/2) + arctan(1/3)
  pi/4 = 2*arctan(1/3) + arctan(1/7)
  pi/4 = 4*arctan(1/5) - arctan(1/239)
  pi/4 = 5*arctan(1/7) + 2*arctan(3/79)
  pi/4 = 5*arctan(29/278) + 7*arctan(3/79)
  pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)
  pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)
  pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)
  pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)
  pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)
  pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)
  pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)
  pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)
  pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)
  pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)
  pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)
  pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)
'''

def parse_eqn(equationtext=equationtext):
    eqn_re = re.compile(r"""(?mx)
    (?P<lhs> ^ \s* pi/4 \s* = \s*)?             # LHS of equation
    (?:                                         # RHS
        \s* (?P<sign> [+-])? \s*
        (?: (?P<mult> \d+) \s* \*)?
        \s* arctan\( (?P<numer> \d+) / (?P<denom> \d+)
    )""")

    found = eqn_re.findall(equationtext)
    machins, part = [], []
    for lhs, sign, mult, numer, denom in eqn_re.findall(equationtext):
        if lhs and part:
            machins.append(part)
            part = []
        part.append( ( (-1 if sign == '-' else 1) * ( int(mult) if mult else 1),
                       Fraction(int(numer), (int(denom) if denom else 1)) ) )
    machins.append(part)
    return machins


def tans(xs):
    xslen = len(xs)
    if xslen == 1:
        return tanEval(*xs[0])
    aa, bb = xs[:xslen//2], xs[xslen//2:]
    a, b = tans(aa), tans(bb)
    return (a + b) / (1 - a * b)

def tanEval(coef, f):
    if coef == 1:
        return f
    if coef < 0:
        return -tanEval(-coef, f)
    ca = coef // 2
    cb = coef - ca
    a, b = tanEval(ca, f), tanEval(cb, f)
    return (a + b) / (1 - a * b)


if __name__ == '__main__':
    machins = parse_eqn()
    #pp(machins, width=160)
    for machin, eqn in zip(machins, equationtext.split('\n')):
        ans = tans(machin)
        print('%5s: %s' % ( ('OK' if ans == 1 else 'ERROR'), eqn))
```


```txt
   OK:   pi/4 = arctan(1/2) + arctan(1/3)
   OK:   pi/4 = 2*arctan(1/3) + arctan(1/7)
   OK:   pi/4 = 4*arctan(1/5) - arctan(1/239)
   OK:   pi/4 = 5*arctan(1/7) + 2*arctan(3/79)
   OK:   pi/4 = 5*arctan(29/278) + 7*arctan(3/79)
   OK:   pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)
   OK:   pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)
   OK:   pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)
   OK:   pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)
   OK:   pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)
   OK:   pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)
   OK:   pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)
   OK:   pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)
   OK:   pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)
   OK:   pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)
   OK:   pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)
ERROR:   pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)
```


'''Note:''' the [http://kodos.sourceforge.net/ Kodos] tool was used in developing the regular expression.


## R


```R

#lang R
library(Rmpfr)
prec <- 1000 # precision in bits
`%:%` <- function(e1, e2) '/'(mpfr(e1, prec), mpfr(e2, prec)) # operator %:% for high precision division
# function for checking identity of tan of expression and 1, making use of high precision division operator %:%
tanident_1 <- function(x) identical(round(tan(eval(parse(text = gsub("/", "%:%", deparse(substitute(x)))))), (prec/10)), mpfr(1, prec))

```


```R

tanident_1( 1*atan(1/2)    +  1*atan(1/3) )
## [1] TRUE
tanident_1( 2*atan(1/3)    +  1*atan(1/7))
## [1] TRUE
tanident_1( 4*atan(1/5)    + -1*atan(1/239))
## [1] TRUE
tanident_1( 5*atan(1/7)    +  2*atan(3/79))
## [1] TRUE
tanident_1( 5*atan(29/278) +  7*atan(3/79))
## [1] TRUE
tanident_1( 1*atan(1/2)    +  1*atan(1/5)   +   1*atan(1/8) )
## [1] TRUE
tanident_1( 4*atan(1/5)    + -1*atan(1/70)  +   1*atan(1/99) )
## [1] TRUE
tanident_1( 5*atan(1/7)    +  4*atan(1/53)  +   2*atan(1/4443))
## [1] TRUE
tanident_1( 6*atan(1/8)    +  2*atan(1/57)  +   1*atan(1/239))
## [1] TRUE
tanident_1( 8*atan(1/10)   + -1*atan(1/239) +  -4*atan(1/515))
## [1] TRUE
tanident_1(12*atan(1/18)   +  8*atan(1/57)  +  -5*atan(1/239))
## [1] TRUE
tanident_1(16*atan(1/21)   +  3*atan(1/239) +   4*atan(3/1042))
## [1] TRUE
tanident_1(22*atan(1/28)   +  2*atan(1/443) +  -5*atan(1/1393) + -10*atan(1/11018))
## [1] TRUE
tanident_1(22*atan(1/38)   + 17*atan(7/601) +  10*atan(7/8149))
## [1] TRUE
tanident_1(44*atan(1/57)   +  7*atan(1/239) + -12*atan(1/682)  +  24*atan(1/12943))
## [1] TRUE
tanident_1(88*atan(1/172)  + 51*atan(1/239) +  32*atan(1/682)  +  44*atan(1/5357) + 68*atan(1/12943))
## [1] TRUE
tanident_1(88*atan(1/172)  + 51*atan(1/239) +  32*atan(1/682)  +  44*atan(1/5357) + 68*atan(1/12944))
## [1] FALSE

```



## Racket


```racket

#lang racket
(define (reduce e)
  (match e
    [(? number? a)                         a]
    [(list '+ (? number? a) (? number? b)) (+ a b)]
    [(list '- (? number? a) (? number? b)) (- a b)]
    [(list '- (? number? a))               (- a)]
    [(list '* (? number? a) (? number? b)) (* a b)]
    [(list '/ (? number? a) (? number? b)) (/ a b)]
    [(list '+ a b)                         (reduce `(+ ,(reduce a) ,(reduce b)))]
    [(list '- a b)                         (reduce `(- ,(reduce a) ,(reduce b)))]
    [(list '- a)                           (reduce `(- ,(reduce a)))]
    [(list '* a b)                         (reduce `(* ,(reduce a) ,(reduce b)))]
    [(list '/ a b)                         (reduce `(/ ,(reduce a) ,(reduce b)))]
    [(list 'tan (list 'arctan a))          (reduce a)]
    [(list 'tan (list '- a))               (reduce `(- ,(reduce `(tan ,a))))]
    [(list 'tan (list '+ a b))             (reduce `(/ (+ (tan ,a) (tan ,b))
                                                       (- 1 (* (tan ,a) (tan ,b)))))]
    [(list 'tan (list '+ a b c ...))       (reduce `(tan (+ ,a (+ ,b ,@c))))]
    [(list 'tan (list '- a b))             (reduce `(/ (+ (tan ,a) (tan (- ,b)))
                                                       (- 1 (* (tan ,a) (tan (- ,b))))))]
    [(list 'tan (list '* 1 a))             (reduce `(tan ,a))]
    [(list 'tan (list '* (? number? n) a))
     (cond [(< n 0) (reduce `(- (tan (* ,(- n) ,a))))]
           [(= n 0) 0]
           [(even? n) (reduce `(tan (+ (* ,(/ n 2) ,a) (* ,(/ n 2) ,a))))]
           [else      (reduce `(tan (+ ,a  (* ,(- n 1) ,a))))])]))

(define correct-formulas
  '((tan (+ (arctan 1/2) (arctan 1/3)))
    (tan (+ (* 2 (arctan 1/3)) (arctan 1/7)))
    (tan (- (* 4 (arctan 1/5)) (arctan 1/239)))
    (tan (+ (* 5 (arctan 1/7)) (* 2 (arctan 3/79))))
    (tan (+ (* 5 (arctan 29/278)) (* 7 (arctan 3/79))))
    (tan (+ (arctan 1/2) (arctan 1/5) (arctan 1/8)))
    (tan (+ (* 4 (arctan 1/5)) (* -1 (arctan 1/70)) (arctan 1/99)))
    (tan (+ (* 5 (arctan 1/7)) (* 4 (arctan 1/53)) (* 2 (arctan 1/4443))))
    (tan (+ (* 6 (arctan 1/8)) (* 2 (arctan 1/57)) (arctan 1/239)))
    (tan (+ (* 8 (arctan 1/10)) (* -1 (arctan 1/239)) (* -4 (arctan 1/515))))
    (tan (+ (* 12 (arctan 1/18)) (* 8 (arctan 1/57)) (* -5 (arctan 1/239))))
    (tan (+ (* 16 (arctan 1/21)) (* 3 (arctan 1/239)) (* 4 (arctan 3/1042))))
    (tan (+ (* 22 (arctan 1/28)) (* 2 (arctan 1/443)) (* -5 (arctan 1/1393)) (* -10 (arctan 1/11018))))
    (tan (+ (* 22 (arctan 1/38)) (* 17 (arctan 7/601)) (* 10 (arctan 7/8149))))
    (tan (+ (* 44 (arctan 1/57)) (* 7 (arctan 1/239)) (* -12 (arctan 1/682)) (* 24 (arctan 1/12943))))
    (tan (+ (* 88 (arctan 1/172)) (* 51 (arctan 1/239)) (* 32 (arctan 1/682))
            (* 44 (arctan 1/5357)) (* 68 (arctan 1/12943))))))

(define wrong-formula
  '(tan (+ (* 88 (arctan 1/172)) (* 51 (arctan 1/239)) (* 32 (arctan 1/682))
           (* 44 (arctan 1/5357)) (* 68 (arctan 1/12944)))))

(displayln "Do all correct formulas reduce to 1?")
(for/and ([f correct-formulas]) (= 1 (reduce f)))

(displayln "The incorrect formula reduces to:")
(reduce wrong-formula)

```

Output:

```racket

Do all correct formulas reduce to 1?
#t
The incorrect formula reduces to:
1009288018000944050967896710431587186456256928584351786643498522649995492271475761189348270710224618853590682465929080006511691833816436374107451368838065354726517908250456341991684635768915704374493675498637876700129004484434187627909285979251682006538817341793224963346197503893270875008524149334251672855130857035205217929335932890740051319216343365800342290782260673215928499123722781078448297609548233999010983373327601187505623621602789012550584784738082074783523787011976757247516095289966708782862528690942242793667539020699840402353522108223/1009288837315638583415701528780402795721935641614456853534313491853293025565940011104051964874275710024625850092154664245109626053906509780125743180758231049920425664246286578958307532545458843067352531217230461290763258378749459637420702619029075083089762088232401888676895047947363883809724322868121990870409574061477638203859217672620508200713073485398199091153535700094640095900731630771349477187594074169815106104524371099618096164871416282464532355211521113449237814080332335526420331468258917484010722587072087349909684004660371264507984339711

```



## REXX

Note:   REXX doesn't have many math functions,   so a few of them are included here.

Noticed:   the test arguments specified for this Rosetta Code task need only '''nine''' decimal digits for verification,   '''eight''' decimal digits is   ''not''   enough to catch the "bad" equation.   With this in mind, the REXX's   ''decimal digit precision''   was increased to the number of decimal digits specified for the variable   '''pi'''       (which, for these cases, is a bit of overkill, but the difference in execution times were barely noticeable).

An extra formula was added to stress test the near exactness of a value.

```rexx
/*REXX program  evaluates  some  Machin─like  formulas  and  verifies  their veracity.  */
@.=;   pi= pi();       numeric digits( length(pi) ) - length(.);       numeric fuzz 3
@.1 = 'pi/4 =    atan(1/2)    +    atan(1/3)'
@.2 = 'pi/4 =  2*atan(1/3)    +    atan(1/7)'
@.3 = 'pi/4 =  4*atan(1/5)    -    atan(1/239)'
@.4 = 'pi/4 =  5*atan(1/7)    +  2*atan(3/79)'
@.5 = 'pi/4 =  5*atan(29/278) +  7*atan(3/79)'
@.6 = 'pi/4 =    atan(1/2)    +    atan(1/5)   +    atan(1/8)'
@.7 = 'pi/4 =  4*atan(1/5)    -    atan(1/70)  +    atan(1/99)'
@.8 = 'pi/4 =  5*atan(1/7)    +  4*atan(1/53)  +  2*atan(1/4443)'
@.9 = 'pi/4 =  6*atan(1/8)    +  2*atan(1/57)  +    atan(1/239)'
@.10= 'pi/4 =  8*atan(1/10)   -    atan(1/239) -  4*atan(1/515)'
@.11= 'pi/4 = 12*atan(1/18)   +  8*atan(1/57)  -  5*atan(1/239)'
@.12= 'pi/4 = 16*atan(1/21)   +  3*atan(1/239) +  4*atan(3/1042)'
@.13= 'pi/4 = 22*atan(1/28)   +  2*atan(1/443) -  5*atan(1/1393) - 10*atan(1/11018)'
@.14= 'pi/4 = 22*atan(1/38)   + 17*atan(7/601) + 10*atan(7/8149)'
@.15= 'pi/4 = 44*atan(1/57)   +  7*atan(1/239) - 12*atan(1/682)  + 24*atan(1/12943)'
@.16= 'pi/4 = 88*atan(1/172)  + 51*atan(1/239) + 32*atan(1/682)  + 44*atan(1/5357)  + 68*atan(1/12943)'
@.17= 'pi/4 = 88*atan(1/172)  + 51*atan(1/239) + 32*atan(1/682)  + 44*atan(1/5357)  + 68*atan(1/12944)'
@.18= 'pi/4 = 88*atan(1/172)  + 51*atan(1/239) + 32*atan(1/682)  + 44*atan(1/5357)  + 67.9999999994*atan(1/12943)'

        do j=1  while  @.j\==''                  /*evaluate each "Machin─like" formulas.*/
        interpret  'answer='   "("   @.j   ')'   /*where REXX does the heavy lifting.   */
        say  right( word( 'bad OK', answer+1), 3)": "                   @.j
        end   /*j*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
pi:   return 3.141592653589793238462643383279502884197169399375105820974944592307816406286
Acos: procedure; parse arg x;            return pi() * .5 - Asin(x)
Atan: procedure; arg x; if abs(x)=1 then return pi()/4*sign(x); return Asin(x/sqrt(1+x*x))
/*──────────────────────────────────────────────────────────────────────────────────────*/
Asin: procedure; parse arg x 1 z 1 o 1 p;       a=abs(x);       aa=a*a
      if a>=sqrt(2)*.5  then  return sign(x)  *  Acos( sqrt(1 - aa) )
          do j=2 by 2 until p=z;  p=z;  o=o*aa*(j-1)/j;  z=z+o/(j+1); end /*j*/;  return z
/*──────────────────────────────────────────────────────────────────────────────────────*/
sqrt: procedure; parse arg x; if x=0  then return 0; d=digits(); m.=9; h=d+6; numeric form
      numeric digits; parse value format(x,2,1,,0) 'E0'  with  g 'E' _ .;  g=g *.5'e'_ % 2
           do j=0  while h>9;      m.j=h;              h=h%2+1;       end /*j*/
           do k=j+5  to 0  by -1;  numeric digits m.k; g=(g+x/g)*.5;  end /*k*/;  return g
```

```txt

 OK:  pi/4 =    atan(1/2)    +    atan(1/3)
 OK:  pi/4 =  2*atan(1/3)    +    atan(1/7)
 OK:  pi/4 =  4*atan(1/5)    -    atan(1/239)
 OK:  pi/4 =  5*atan(1/7)    +  2*atan(3/79)
 OK:  pi/4 =  5*atan(29/278) +  7*atan(3/79)
 OK:  pi/4 =    atan(1/2)    +    atan(1/5)   +    atan(1/8)
 OK:  pi/4 =  4*atan(1/5)    -    atan(1/70)  +    atan(1/99)
 OK:  pi/4 =  5*atan(1/7)    +  4*atan(1/53)  +  2*atan(1/4443)
 OK:  pi/4 =  6*atan(1/8)    +  2*atan(1/57)  +    atan(1/239)
 OK:  pi/4 =  8*atan(1/10)   -    atan(1/239) -  4*atan(1/515)
 OK:  pi/4 = 12*atan(1/18)   +  8*atan(1/57)  -  5*atan(1/239)
 OK:  pi/4 = 16*atan(1/21)   +  3*atan(1/239) +  4*atan(3/1042)
 OK:  pi/4 = 22*atan(1/28)   +  2*atan(1/443) -  5*atan(1/1393) - 10*atan(1/11018)
 OK:  pi/4 = 22*atan(1/38)   + 17*atan(7/601) + 10*atan(7/8149)
 OK:  pi/4 = 44*atan(1/57)   +  7*atan(1/239) - 12*atan(1/682)  + 24*atan(1/12943)
 OK:  pi/4 = 88*atan(1/172)  + 51*atan(1/239) + 32*atan(1/682)  + 44*atan(1/5357)  + 68*atan(1/12943)
bad:  pi/4 = 88*atan(1/172)  + 51*atan(1/239) + 32*atan(1/682)  + 44*atan(1/5357)  + 68*atan(1/12944)
bad:  pi/4 = 88*atan(1/172)  + 51*atan(1/239) + 32*atan(1/682)  + 44*atan(1/5357)  + 67.9999999994*atan(1/12943)

```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "bigint.s7i";
  include "bigrat.s7i";

const type: mTerms is array array bigInteger;

const array mTerms: testCases is [] (
    [] ([] ( 1_, 1_,   2_), [] ( 1_, 1_,   3_)),
    [] ([] ( 2_, 1_,   3_), [] ( 1_, 1_,   7_)),
    [] ([] ( 4_, 1_,   5_), [] (-1_, 1_, 239_)),
    [] ([] ( 5_, 1_,   7_), [] ( 2_, 3_,  79_)),
    [] ([] ( 1_, 1_,   2_), [] ( 1_, 1_,   5_), [] (  1_, 1_,    8_)),
    [] ([] ( 4_, 1_,   5_), [] (-1_, 1_,  70_), [] (  1_, 1_,   99_)),
    [] ([] ( 5_, 1_,   7_), [] ( 4_, 1_,  53_), [] (  2_, 1_, 4443_)),
    [] ([] ( 6_, 1_,   8_), [] ( 2_, 1_,  57_), [] (  1_, 1_,  239_)),
    [] ([] ( 8_, 1_,  10_), [] (-1_, 1_, 239_), [] ( -4_, 1_,  515_)),
    [] ([] (12_, 1_,  18_), [] ( 8_, 1_,  57_), [] ( -5_, 1_,  239_)),
    [] ([] (16_, 1_,  21_), [] ( 3_, 1_, 239_), [] (  4_, 3_, 1042_)),
    [] ([] (22_, 1_,  28_), [] ( 2_, 1_, 443_), [] ( -5_, 1_, 1393_), [] (-10_, 1_, 11018_)),
    [] ([] (22_, 1_,  38_), [] (17_, 7_, 601_), [] ( 10_, 7_, 8149_)),
    [] ([] (44_, 1_,  57_), [] ( 7_, 1_, 239_), [] (-12_, 1_,  682_), [] ( 24_, 1_, 12943_)),
    [] ([] (88_, 1_, 172_), [] (51_, 1_, 239_), [] ( 32_, 1_,  682_), [] ( 44_, 1_,  5357_), [] (68_, 1_, 12943_)),
    [] ([] (88_, 1_, 172_), [] (51_, 1_, 239_), [] ( 32_, 1_,  682_), [] ( 44_, 1_,  5357_), [] (68_, 1_, 12944_))
  );

const func bigRational: tanEval (in bigInteger: coef, in bigRational: f) is func
  result
    var bigRational: tanEval is bigRational.value;
  local
    var bigRational: a is bigRational.value;
    var bigRational: b is bigRational.value;
  begin
    if coef = 1_ then
      tanEval := f;
    elsif coef < 0_ then
      tanEval := -tanEval(-coef, f);
    else
      a := tanEval(coef div 2_, f);
      b := tanEval(coef - coef div 2_, f);
      tanEval := (a + b) / (1_/1_ - a * b);
    end if;
  end func;

const func bigRational: tans (in mTerms: terms) is func
  result
    var bigRational: tans is bigRational.value;
  local
    var bigRational: a is bigRational.value;
    var bigRational: b is bigRational.value;
  begin
    if length(terms) = 1 then
      tans := tanEval(terms[1][1], terms[1][2] / terms[1][3]);
    else
      a := tans(terms[.. length(terms) div 2]);
      b := tans(terms[succ(length(terms) div 2) ..]);
      tans := (a + b) / (1_/1_ - a * b);
    end if;
  end func;

const proc: main is func
  local
    var integer: index is 0;
    var array bigInteger: term is 0 times 0_;
  begin
    for key index range testCases do
      write(tans(testCases[index]) = 1_/1_ <& ": pi/4 = ");
      for term range testCases[index] do
        write([0] ("+", "-")[ord(term[1] < 0_)] <& abs(term[1]) <& "*arctan(" <& term[2] <& "/" <& term[3] <& ")");
      end for;
      writeln;
    end for;
  end func;
```


```txt

TRUE: pi/4 = +1*arctan(1/2)+1*arctan(1/3)
TRUE: pi/4 = +2*arctan(1/3)+1*arctan(1/7)
TRUE: pi/4 = +4*arctan(1/5)-1*arctan(1/239)
TRUE: pi/4 = +5*arctan(1/7)+2*arctan(3/79)
TRUE: pi/4 = +1*arctan(1/2)+1*arctan(1/5)+1*arctan(1/8)
TRUE: pi/4 = +4*arctan(1/5)-1*arctan(1/70)+1*arctan(1/99)
TRUE: pi/4 = +5*arctan(1/7)+4*arctan(1/53)+2*arctan(1/4443)
TRUE: pi/4 = +6*arctan(1/8)+2*arctan(1/57)+1*arctan(1/239)
TRUE: pi/4 = +8*arctan(1/10)-1*arctan(1/239)-4*arctan(1/515)
TRUE: pi/4 = +12*arctan(1/18)+8*arctan(1/57)-5*arctan(1/239)
TRUE: pi/4 = +16*arctan(1/21)+3*arctan(1/239)+4*arctan(3/1042)
TRUE: pi/4 = +22*arctan(1/28)+2*arctan(1/443)-5*arctan(1/1393)-10*arctan(1/11018)
TRUE: pi/4 = +22*arctan(1/38)+17*arctan(7/601)+10*arctan(7/8149)
TRUE: pi/4 = +44*arctan(1/57)+7*arctan(1/239)-12*arctan(1/682)+24*arctan(1/12943)
TRUE: pi/4 = +88*arctan(1/172)+51*arctan(1/239)+32*arctan(1/682)+44*arctan(1/5357)+68*arctan(1/12943)
FALSE: pi/4 = +88*arctan(1/172)+51*arctan(1/239)+32*arctan(1/682)+44*arctan(1/5357)+68*arctan(1/12944)

```



## Sidef

```ruby
var equationtext = <<'EOT'
  pi/4 = arctan(1/2) + arctan(1/3)
  pi/4 = 2*arctan(1/3) + arctan(1/7)
  pi/4 = 4*arctan(1/5) - arctan(1/239)
  pi/4 = 5*arctan(1/7) + 2*arctan(3/79)
  pi/4 = 5*arctan(29/278) + 7*arctan(3/79)
  pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)
  pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)
  pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)
  pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)
  pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)
  pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)
  pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)
  pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)
  pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)
  pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)
  pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)
  pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)
EOT

func parse_eqn(equation) {
    static eqn_re = %r{
    (^ \s* pi/4 \s* = \s* )?                 # LHS of equation
    (?:                                      # RHS
        \s* ( [-+] )? \s*
        (?: ( \d+ ) \s* \*)?
        \s* arctan\((.*?)\)
    )}x

    gather {
        for lhs,sign,mult,rat in (equation.findall(eqn_re)) {
            take([
                [+1, -1][sign == '-'] * (mult ? Num(mult) : 1),
                Num(rat)
            ])
        }
    }
}

func tanEval(coef, f) {
    return f if (coef == 1)
    return -tanEval(-coef, f) if (coef < 0)
    var ca = coef>>1
    var cb = (coef - ca)
    var (a, b) = (tanEval(ca, f), tanEval(cb, f))
    (a + b) / (1 - a*b)
}

func tans(xs) {
    var xslen = xs.len
    return tanEval(xs[0]...) if (xslen == 1)
    var (aa, bb) = xs.part(xslen>>1)
    var (a, b) = (tans(aa), tans(bb))
    (a + b) / (1 - a*b)
}

var machins = equationtext.lines.map(parse_eqn)

for machin,eqn in (machins ~Z equationtext.lines) {
    var ans = tans(machin)
    printf("%5s: %s\n", (ans == 1 ? 'OK' : 'ERROR'), eqn)
}
```

```txt

   OK:   pi/4 = arctan(1/2) + arctan(1/3)
   OK:   pi/4 = 2*arctan(1/3) + arctan(1/7)
   OK:   pi/4 = 4*arctan(1/5) - arctan(1/239)
   OK:   pi/4 = 5*arctan(1/7) + 2*arctan(3/79)
   OK:   pi/4 = 5*arctan(29/278) + 7*arctan(3/79)
   OK:   pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)
   OK:   pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)
   OK:   pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)
   OK:   pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)
   OK:   pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)
   OK:   pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)
   OK:   pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)
   OK:   pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)
   OK:   pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)
   OK:   pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)
   OK:   pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)
ERROR:   pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)

```



## Tcl


```tcl
package require Tcl 8.5

# Compute tan(atan(p)+atan(q)) using rationals
proc tadd {p q} {
    lassign $p pp pq
    lassign $q qp qq
    set topp [expr {$pp*$qq + $qp*$pq}]
    set topq [expr {$pq*$qq}]
    set prodp [expr {$pp*$qp}]
    set prodq [expr {$pq*$qq}]
    set lowp [expr {$prodq - $prodp}]
    set resultp [set gcd1 [expr {$topp * $prodq}]]
    set resultq [set gcd2 [expr {$topq * $lowp}]]
    # Critical! Normalize using the GCD
    while {$gcd2 != 0} {
	lassign [list $gcd2 [expr {$gcd1 % $gcd2}]] gcd1 gcd2
    }
    list [expr {$resultp / abs($gcd1)}] [expr {$resultq / abs($gcd1)}]
}
proc termTan {n a b} {
    if {$n < 0} {
	set n [expr {-$n}]
	set a [expr {-$a}]
    }
    if {$n == 1} {
	return [list $a $b]
    }
    set k [expr {$n - [set m [expr {$n / 2}]]*2}]
    set t2 [termTan $m $a $b]
    set m2 [tadd $t2 $t2]
    if {$k == 0} {
	return $m2
    }
    return [tadd [termTan $k $a $b] $m2]
}
proc machinTan {terms} {
    set sum {0 1}
    foreach term $terms {
	set sum [tadd $sum [termTan {*}$term]]
    }
    return $sum
}

# Assumes that the formula is in the very specific form below!
proc parseFormula {formula} {
    set RE {(-?\s*\d*\s*\*?)\s*arctan\s*\(\s*(-?\s*\d+)\s*/\s*(-?\s*\d+)\s*\)}
    set nospace {" " "" "*" ""}
    foreach {all n a b} [regexp -inline -all $RE $formula] {
	if {![regexp {\d} $n]} {append n 1}
	lappend result [list [string map $nospace $n] [string map $nospace $a] [string map $nospace $b]]
    }
    return $result
}

foreach formula {
    "pi/4 = arctan(1/2) + arctan(1/3)"
    "pi/4 = 2*arctan(1/3) + arctan(1/7)"
    "pi/4 = 4*arctan(1/5) - arctan(1/239)"
    "pi/4 = 5*arctan(1/7) + 2*arctan(3/79)"
    "pi/4 = 5*arctan(29/278) + 7*arctan(3/79)"
    "pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)"
    "pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)"
    "pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)"
    "pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)"
    "pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)"
    "pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)"
    "pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)"
    "pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)"
    "pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)"
    "pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)"
    "pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)"
    "pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)"
} {
    if {[tcl::mathop::== {*}[machinTan [parseFormula $formula]]]} {
	puts "Yes! '$formula' is true"
    } else {
	puts "No! '$formula' not true"
    }
}
```

```txt

Yes! 'pi/4 = arctan(1/2) + arctan(1/3)' is true
Yes! 'pi/4 = 2*arctan(1/3) + arctan(1/7)' is true
Yes! 'pi/4 = 4*arctan(1/5) - arctan(1/239)' is true
Yes! 'pi/4 = 5*arctan(1/7) + 2*arctan(3/79)' is true
Yes! 'pi/4 = 5*arctan(29/278) + 7*arctan(3/79)' is true
Yes! 'pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)' is true
Yes! 'pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)' is true
Yes! 'pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)' is true
Yes! 'pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)' is true
Yes! 'pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)' is true
Yes! 'pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)' is true
Yes! 'pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)' is true
Yes! 'pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)' is true
Yes! 'pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)' is true
Yes! 'pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)' is true
Yes! 'pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)' is true
No! 'pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)' not true

```



## Visual Basic .NET

'''BigRat''' class based on the Arithmetic/Rational#C here at Rosetta Code.<br/>
The parser here allows for some flexibility in the input text. Case is ignored, and a variable number of spaces are allowed.  Atan(), arctan(), atn() are all recognized as valid.  If one of those three are not found, a warning will appear.  The coefficient need not have a multiplication sign between it and the "arctan()".  The left side of the equation must be pi / 4, otherwise a warning will appear.

```vbnet
Imports System.Numerics

Public Class BigRat ' Big Rational Class constructed with BigIntegers
    Implements IComparable
    Public nu, de As BigInteger
    Public Shared Zero = New BigRat(BigInteger.Zero, BigInteger.One),
                  One = New BigRat(BigInteger.One, BigInteger.One)
    Sub New(bRat As BigRat)
        nu = bRat.nu : de = bRat.de
    End Sub
    Sub New(n As BigInteger, d As BigInteger)
        If d = BigInteger.Zero Then _
            Throw (New Exception(String.Format("tried to set a BigRat with ({0}/{1})", n, d)))
        Dim bi As BigInteger = BigInteger.GreatestCommonDivisor(n, d)
        If bi > BigInteger.One Then n /= bi : d /= bi
        If d < BigInteger.Zero Then n = -n : d = -d
        nu = n : de = d
    End Sub
    Shared Operator -(x As BigRat) As BigRat
        Return New BigRat(-x.nu, x.de)
    End Operator
    Shared Operator +(x As BigRat, y As BigRat)
        Return New BigRat(x.nu * y.de + x.de * y.nu, x.de * y.de)
    End Operator
    Shared Operator -(x As BigRat, y As BigRat) As BigRat
        Return x + (-y)
    End Operator
    Shared Operator *(x As BigRat, y As BigRat) As BigRat
        Return New BigRat(x.nu * y.nu, x.de * y.de)
    End Operator
    Shared Operator /(x As BigRat, y As BigRat) As BigRat
        Return New BigRat(x.nu * y.de, x.de * y.nu)
    End Operator
    Public Function CompareTo(obj As Object) As Integer Implements IComparable.CompareTo
        Dim dif As BigRat = New BigRat(nu, de) - obj
        If dif.nu < BigInteger.Zero Then Return -1
        If dif.nu > BigInteger.Zero Then Return 1
        Return 0
    End Function
    Shared Operator =(x As BigRat, y As BigRat) As Boolean
        Return x.CompareTo(y) = 0
    End Operator
    Shared Operator <>(x As BigRat, y As BigRat) As Boolean
        Return x.CompareTo(y) <> 0
    End Operator
    Overrides Function ToString() As String
        If de = BigInteger.One Then Return nu.ToString
        Return String.Format("({0}/{1})", nu, de)
    End Function
    Shared Function Combine(a As BigRat, b As BigRat) As BigRat
        Return (a + b) / (BigRat.One - (a * b))
    End Function
End Class

Public Structure Term ' coefficent, BigRational construction for each term
    Dim c As Integer, br As BigRat
    Sub New(cc As Integer, bigr As BigRat)
        c = cc : br = bigr
    End Sub
End Structure

Module Module1
    Function Eval(c As Integer, x As BigRat) As BigRat
        If c = 1 Then Return x Else If c < 0 Then Return Eval(-c, -x)
        Dim hc As Integer = c \ 2
        Return BigRat.Combine(Eval(hc, x), Eval(c - hc, x))
    End Function

    Function Sum(terms As List(Of Term)) As BigRat
        If terms.Count = 1 Then Return Eval(terms(0).c, terms(0).br)
        Dim htc As Integer = terms.Count / 2
        Return BigRat.Combine(Sum(terms.Take(htc).ToList), Sum(terms.Skip(htc).ToList))
    End Function

    Function ParseLine(ByVal s As String) As List(Of Term)
        ParseLine = New List(Of Term) : Dim t As String = s.ToLower, p As Integer, x As New Term(1, BigRat.Zero)
        While t.Contains(" ") : t = t.Replace(" ", "") : End While
        p = t.IndexOf("pi/4=") : If p < 0 Then _
            Console.WriteLine("warning: tan(left side of equation) <> 1") : ParseLine.Add(x) : Exit Function
        t = t.Substring(p + 5)
        For Each item As String In t.Split(")")
            If item.Length > 5 Then
                If (Not item.Contains("tan") OrElse item.IndexOf("a") < 0 OrElse
                    item.IndexOf("a") > item.IndexOf("tan")) AndAlso Not item.Contains("atn") Then
                    Console.WriteLine("warning: a term is mising a valid arctangent identifier on the right side of the equation: [{0})]", item)
                    ParseLine = New List(Of Term) : ParseLine.Add(New Term(1, BigRat.Zero)) : Exit Function
                End If
                x.c = 1 : x.br = New BigRat(BigRat.One)
                p = item.IndexOf("/") : If p > 0 Then
                    x.br.de = UInt64.Parse(item.Substring(p + 1))
                    item = item.Substring(0, p)
                    p = item.IndexOf("(") : If p > 0 Then
                        x.br.nu = UInt64.Parse(item.Substring(p + 1))
                        p = item.IndexOf("a") : If p > 0 Then
                            Integer.TryParse(item.Substring(0, p).Replace("*", ""), x.c)
                            If x.c = 0 Then x.c = 1
                            If item.Contains("-") AndAlso x.c > 0 Then x.c = -x.c
                        End If
                        ParseLine.Add(x)
                    End If
                End If
            End If
        Next
    End Function

    Sub Main(ByVal args As String())
        Dim nl As String = vbLf
        For Each item In ("pi/4 = ATan(1 / 2) + ATan(1/3)" & nl &
              "pi/4 = 2Atan(1/3) + ATan(1/7)" & nl &
              "pi/4 = 4ArcTan(1/5) - ATan(1 / 239)" & nl &
              "pi/4 = 5arctan(1/7) + 2 * atan(3/79)" & nl &
              "Pi/4 = 5ATan(29/278) + 7*ATan(3/79)" & nl &
              "pi/4 = atn(1/2) + ATan(1/5) + ATan(1/8)" & nl &
              "PI/4   = 4ATan(1/5) - Atan(1/70) + ATan(1/99)" & nl &
              "pi /4 = 5*ATan(1/7) + 4 ATan(1/53) + 2ATan(1/4443)" & nl &
              "pi / 4 = 6ATan(1/8) + 2arctangent(1/57) + ATan(1/239)" & nl &
              "pi/ 4 = 8ATan(1/10) - ATan(1/239) - 4ATan(1/515)" & nl &
              "pi/4 = 12ATan(1/18) + 8ATan(1/57) - 5ATan(1/239)" & nl &
              "pi/4 = 16 * ATan(1/21) + 3ATan(1/239) + 4ATan(3/1042)" & nl &
              "pi/4 = 22ATan(1/28) + 2ATan(1/443) - 5ATan(1/1393)  -    10  ATan( 1  /   11018 )" & nl &
              "pi/4 = 22ATan(1/38) + 17ATan(7/601) + 10ATan(7 /  8149)" & nl &
              "pi/4 = 44ATan(1/57) + 7ATan(1/239) - 12ATan(1/682) + 24ATan(1/12943)" & nl &
              "pi/4 = 88ATan(1/172) + 51ATan(1/239) + 32ATan(1/682) + 44ATan(1/5357) + 68ATan(1/12943)" & nl &
              "pi/4 = 88ATan(1/172) + 51ATan(1/239) + 32ATan(1/682) + 44ATan(1/5357) + 68ATan(1/12944)").Split(nl)
            Console.WriteLine("{0}: {1}", If(Sum(ParseLine(item)) = BigRat.One, "Pass", "Fail"), item)
        Next
    End Sub
End Module
```

```txt
Pass: pi/4 = ATan(1 / 2) + ATan(1/3)
Pass: pi/4 = 2Atan(1/3) + ATan(1/7)
Pass: pi/4 = 4ArcTan(1/5) - ATan(1 / 239)
Pass: pi/4 = 5arctan(1/7) + 2 * atan(3/79)
Pass: pi/4 = 5ATan(29/278) + 7*ATan(3/79)
Pass: pi/4 = atn(1/2) + ATan(1/5) + ATan(1/8)
Pass: pi/4   = 4ATan(1/5) - Atan(1/70) + ATan(1/99)
Pass: pi /4 = 5*ATan(1/7) + 4 ATan(1/53) + 2ATan(1/4443)
Pass: pi / 4 = 6ATan(1/8) + 2arctangent(1/57) + ATan(1/239)
Pass: pi/ 4 = 8ATan(1/10) - ATan(1/239) - 4ATan(1/515)
Pass: pi/4 = 12ATan(1/18) + 8ATan(1/57) - 5ATan(1/239)
Pass: pi/4 = 16 * ATan(1/21) + 3ATan(1/239) + 4ATan(3/1042)
Pass: pi/4 = 22ATan(1/28) + 2ATan(1/443) - 5ATan(1/1393)  -    10  ATan( 1  /   11018 )
Pass: pi/4 = 22ATan(1/38) + 17ATan(7/601) + 10ATan(7 /  8149)
Pass: pi/4 = 44ATan(1/57) + 7ATan(1/239) - 12ATan(1/682) + 24ATan(1/12943)
Pass: pi/4 = 88ATan(1/172) + 51ATan(1/239) + 32ATan(1/682) + 44ATan(1/5357) + 68ATan(1/12943)
Fail: pi/4 = 88ATan(1/172) + 51ATan(1/239) + 32ATan(1/682) + 44ATan(1/5357) + 68ATan(1/12944)
```



## XPL0


```XPL0
code ChOut=8, Text=12;  \intrinsic routines
int  Number(18);        \numbers from equations
def  LF=$0A;            \ASCII line feed (end-of-line character)

func Parse(S);          \Convert numbers in string S to binary in Number array
char S;
int  I, Neg;

        proc GetNum;    \Get number from string S
        int  N;
        [while S(0)<^0 ! S(0)>^9 do S:= S+1;
        N:= S(0)-^0;  S:= S+1;
        while S(0)>=^0 & S(0)<=^9 do
                [N:= N*10 + S(0) - ^0;  S:= S+1];
        Number(I):= N;  I:= I+1;
        ];

[while S(0)#^= do S:= S+1;      \skip to "="
I:= 0;
loop    [Neg:= false;           \assume positive term
        loop    [S:= S+1;       \next char
                case S(0) of
                  LF:   [Number(I):= 0;  return S+1];   \mark end of array
                  ^-:   Neg:= true;                     \term is negative
                  ^a:   [Number(I):= 1;  I:= I+1; quit] \no coefficient so use 1
                other if S(0)>=^0 & S(0)<=^9 then       \if digit
                        [S:= S-1;  GetNum;  quit];      \backup and get number
                ];
        GetNum;                                         \numerator
        if Neg then Number(I-1):= -Number(I-1);         \tan(-a) = -tan(a)
        GetNum;                                         \denominator
        ];
];


func GCD(U, V);         \Return the greatest common divisor of U and V
int  U, V;
int  T;
[while V do             \Euclid's method
    [T:= U;  U:= V;  V:= rem(T/V)];
return abs(U);
];

proc Verify;            \Verify that tangent of equation = 1 (i.e: E = F)
int  E, F, I, J;

    proc Machin(A, B, C, D);
    int  A, B, C, D;
    int  Div;
    \tan(a+b) = (tan(a) + tan(b)) / (1 - tan(a)*tan(b))
    \tan(arctan(A/B) + arctan(C/D))
    \   = (tan(arctan(A/B)) + tan(arctan(C/D))) / (1 - tan(arctan(A/B))*tan(arctan(C/D)))
    \   = (A/B + C/D) / (1 - A/B*C/D)
    \   = (A*D/B*D + B*C/B*D) / (B*D/B*D - A*C/B*D)
    \   = (A*D + B*C) / (B*D - A*C)
    [E:= A*D + B*C;  F:= B*D - A*C;
    Div:= GCD(E, F);    \keep integers from getting too big
    E:= E/Div;  F:= F/Div;
    ];

[E:= 0;  F:= 1;  I:= 0;
while Number(I) do
    [for J:= 1 to Number(I) do
        Machin(E, F, Number(I+1), Number(I+2));
    I:= I+3;
    ];
Text(0, if E=F then "Yes  " else "No   ");
];


char S, SS;  int I;
[S:= "pi/4 = arctan(1/2) + arctan(1/3)
pi/4 = 2*arctan(1/3) + arctan(1/7)
pi/4 = 4*arctan(1/5) - arctan(1/239)
pi/4 = 5*arctan(1/7) + 2*arctan(3/79)
pi/4 = 5*arctan(29/278) + 7*arctan(3/79)
pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)
pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)
pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)
pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)
pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)
pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)
pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)
pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)
pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)
pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)
pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)
pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)
 ";                             \Python version of equations (thanks!)
for I:= 1 to 17 do
        [SS:= S;                \save start of string line
        S:= Parse(S);           \returns start of next line
        Verify;                 \correct Machin equation? Yes or No
        repeat ChOut(0, SS(0)); SS:= SS+1 until SS(0)=LF;  ChOut(0, LF); \show equation
        ];
]
```


```txt

Yes  pi/4 = arctan(1/2) + arctan(1/3)
Yes  pi/4 = 2*arctan(1/3) + arctan(1/7)
Yes  pi/4 = 4*arctan(1/5) - arctan(1/239)
Yes  pi/4 = 5*arctan(1/7) + 2*arctan(3/79)
Yes  pi/4 = 5*arctan(29/278) + 7*arctan(3/79)
Yes  pi/4 = arctan(1/2) + arctan(1/5) + arctan(1/8)
Yes  pi/4 = 4*arctan(1/5) - arctan(1/70) + arctan(1/99)
Yes  pi/4 = 5*arctan(1/7) + 4*arctan(1/53) + 2*arctan(1/4443)
Yes  pi/4 = 6*arctan(1/8) + 2*arctan(1/57) + arctan(1/239)
Yes  pi/4 = 8*arctan(1/10) - arctan(1/239) - 4*arctan(1/515)
Yes  pi/4 = 12*arctan(1/18) + 8*arctan(1/57) - 5*arctan(1/239)
Yes  pi/4 = 16*arctan(1/21) + 3*arctan(1/239) + 4*arctan(3/1042)
Yes  pi/4 = 22*arctan(1/28) + 2*arctan(1/443) - 5*arctan(1/1393) - 10*arctan(1/11018)
Yes  pi/4 = 22*arctan(1/38) + 17*arctan(7/601) + 10*arctan(7/8149)
Yes  pi/4 = 44*arctan(1/57) + 7*arctan(1/239) - 12*arctan(1/682) + 24*arctan(1/12943)
Yes  pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12943)
No   pi/4 = 88*arctan(1/172) + 51*arctan(1/239) + 32*arctan(1/682) + 44*arctan(1/5357) + 68*arctan(1/12944)

```

