+++
title = "Formal power series/D"
description = ""
date = 2011-03-22T14:02:05Z
aliases = []
[extra]
id = 9382
[taxonomies]
categories = []
tags = []
+++

{{works with|D|2.052}}
Using this [[Rational Arithmetic#D|Rational Module]].

```d
module fps ;
import std.stdio, std.conv, std.traits, std.math, rational ;

template Common(U , T) {
    static if(is(U : Rational) || is(T : Rational))
        alias Rational Common ;
    else
        alias CommonType!(U,T) Common ;
}

struct FPS(U) {
    alias FPS!U F ;
    alias U delegate(size_t) G ;

    private G coef ;
    private U[] cache, inverseCache ;

    static int NumTerm = 11 ;
    static string FmxStr = "%s" ;

    static F opCall(G g) {
        F f ;
        f.coef = g ;
        f.inverseCache ~= 1/f.coef(0) ;
        return f ;
    }
    static F opCall(F f) {
        auto newf = F(f.coef) ;
        newf.cache = f.cache.dup ;
        newf.inverseCache = f.inverseCache.dup ;
        return newf ;
    }
    static F opCall(U num) { return F([num]) ; }
    static F opCall(U[] polynomial) {
        return F( delegate U(size_t idx) {
            static U[] poly ;
            if(poly.length == 0)
                poly = polynomial.dup ;
            U res = 0 ;
            if(idx < poly.length)
                res = poly[idx] ;
            return res ;
        }) ;
    }

    private void grow(size_t n) { // grow cache to length n
        foreach(i ; cache.length..n)
            cache ~= coef(i) ;
    }

    U opIndex(size_t idx) { // idx is non-negative
        if(idx >= cache.length)
            grow(idx + 1) ;
        return cache[idx] ;
    }

    U[] opSlice(size_t begin, size_t end) {
        U[] res ;
        if(begin < end) {
            if(end > cache.length)
                grow(end) ;
            res = cache[begin..end].dup ;
        }
        return res ;
    }

    U inverseCoef(size_t idx) {
        alias inverseCache inv ; // short hand
        if(idx >= inv.length) {
            foreach(i; inv.length.. idx + 1) {
                U newterm = 0 ;
                foreach(j ; 0..i)
                    newterm = newterm + this[i - j] * inv[j] ;
                inv ~= -inv[0] * newterm  ;
            }
        }
        return inverseCache[idx] ;
    }

    F opUnary(string op)() if(op == "+") {  return F(this) ; }
    F opUnary(string op)() if(op == "-") {
        return F( delegate U(size_t idx) {
            return - coef(idx)  ;
        }) ;
    }

    FPS!(Common!(R, U)) opBinary(string op, R)(FPS!R rhs) // F add/sub F
    if(op == "+" || op == "-") {  // term by term op
        alias Common!(U, R) C ;
        return FPS!C ( delegate C(size_t idx) {
            return mixin("coef(idx) " ~op~ " rhs.coef(idx)") ;
        }) ;
    }

    FPS!(Common!(R, U)) opBinary(string op, R)(FPS!R rhs) // F mul/div F
    if(op == "*" || op == "/") {
        alias Common!(U, R) C ;
        static if (op == "*") // mul
            return FPS!C ( delegate C(size_t idx) {
                C res = 0 ;
                foreach(i;0..idx+1)
                    res = res + this[i] * rhs[idx - i] ;
                return res ;
            }) ;
        else //  op = "/" div
            return FPS!C ( delegate C(size_t idx) {
                C res = 0 ;
                foreach(i;0..idx+1)
                    res = res + this[i] * rhs.inverseCoef(idx - i) ;
                return res ;
            }) ;
    }

    auto opBinaryRight(string op, R)(R lhs)// number op F
    if(isNumeric!R && (op == "+" || op == "-" || op == "*" || op == "/")) {
        alias Common!(U,R) C ;
        static if(op == "+" || op == "*" )
            return opBinary!(op,R)(lhs) ;
        else static if (op == "-") // num - F  ;
            return FPS!C ( delegate C(size_t idx) {
                return (idx > 0 ) ? - this[idx] : lhs - this[0] ;
            }) ;
        else { // op == "/" , ie. num div F
            return FPS!C ( delegate C(size_t idx) {
                return lhs * inverseCoef(idx) ;
            }) ;
        }
    }

    auto opBinary(string op, R)(R rhs)    // F op number
    if(isNumeric!R && (op == "+" || op == "-" || op == "*" || op == "/")) {
        alias Common!(U, R) C ;
        static if(op == "+" || op == "-")
            return FPS!C ( delegate C(size_t idx) {
                return (idx == 0) ? mixin("coef(0) "~ op ~" rhs") : coef(idx) ;
            }) ;
        else // op is * or /
            return FPS!C ( delegate C(size_t idx) {
                return mixin("coef(idx) " ~ op ~ " rhs") ;
            }) ;
    }

    F deriv() { // derivative
        return F( delegate U(size_t idx) {
            U res = this[idx + 1] * (idx + 1) ;
            return res  ;
        }) ;
    }

    F integ() { // integral
        return F( delegate U(size_t idx) {
            U res = 0 ;
            if(idx > 0)
                res = this[idx - 1] / idx ;
            return res ;
        }) ;
    }

    string toString() { return toStr() ; }

    string toStr(int n = NumTerm, string fmxStr = FmxStr, string xVar = " x") {
        alias std.string.format fmx ;
        string s ;
        bool withTail = false ;
        U c = this[0] ;
        if(c != 0) s ~= fmx("%s", c) ;
        foreach(i ; 1..n)
            if((c = this[i]) != 0) {
                string t ;
                if(s.length > 0)
                    t = (c > 0) ? " +" : " " ;
                if(c == 1)
                    t ~= xVar ;
                else if(c == -1)
                    t ~= "-" ~ xVar ;
                else
                    t ~= fmx(fmxStr, c) ~  xVar ;
                if(i > 1)
                    t ~= fmx("%s", i) ;
                s ~= t ;
                withTail = true ;
            }
        if(s.length == 0)
            return "0" ;
        if(withTail) s ~= " + ..." ;
        return std.string.strip(s) ;
    }
}


void main() {
    alias Rational U ;
    alias FPS!(U) F ;

    U fact(size_t n) {
        U f = n ;
        foreach(i;1..n)
            f = f * i ;
        return f ;
    }

    F SIN  = F(delegate U(size_t idx) { // series definition of SIN
        U res = 0 ;
        if((idx % 2) == 1) {
            U minusone = - 1 ;
            res = (minusone^^( (idx - 1) / 2)) / fact(idx) ;
        }
        return res ;
    }) ;

    F COS  = SIN.deriv ;
    F TAN  = SIN / COS ;
    F SEC  = 1 / COS ;
    writefln("SIN          : %s", SIN) ;
    writefln("COS          : %s", COS) ;
    writefln("TAN          : %s", TAN) ;
    writefln("SEC          : %s", SEC) ;
    writefln("C*C + S*S    : %s", SIN*SIN + COS*COS) ;       // => 1
    writefln("1 + T*T - T' : %s", 1 + TAN*TAN - TAN.deriv) ; // => 0
    // NOTE : tan' = 1 + tan^2

    // these will reset privous defintion
    COS = F(Rational(0,0)) ;
    SIN = COS.integ ;
    writefln("SIN (reset)  : %s", SIN) ;
    COS = 1 - (+ SIN.integ) ;
    writefln("SIN          : %s", SIN) ;
    writefln("COS          : %s", COS) ;
    writefln("C*C + S*S    : %s", SIN*SIN + COS*COS) ;       // => 1
}
```

Output:

```txt
SIN          : x -1/6 x3 +1/120 x5 -1/5040 x7 +1/362880 x9 + ...
COS          : 1 -1/2 x2 +1/24 x4 -1/720 x6 +1/40320 x8 -1/3628800 x10 + ...
TAN          : x +1/3 x3 +2/15 x5 +17/315 x7 +62/2835 x9 + ...
SEC          : 1 +1/2 x2 +5/24 x4 +61/720 x6 +277/8064 x8 +50521/3628800 x10 + ...
C*C + S*S    : 1
1 + T*T - T' : 0
SIN (reset)  : NaRAT x + ...
SIN          : x -1/6 x3 +1/120 x5 -1/5040 x7 +1/362880 x9 + ...
COS          : 1 -1/2 x2 +1/24 x4 -1/720 x6 +1/40320 x8 -1/3628800 x10 + ...
C*C + S*S    : 1
```

