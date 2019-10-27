+++
title = "Arithmetic/Rational/JavaScript"
description = ""
date = 2012-02-20T15:11:06Z
aliases = []
[extra]
id = 5285
[taxonomies]
categories = []
tags = []
+++

<noinclude>{{collection|Rational Arithmetic}}</noinclude>
;The core of the Rational class

```javascript
// the constructor
function Rational(numerator, denominator) {
    if (denominator === undefined)
        denominator = 1;
    else if (denominator == 0)
        throw "divide by zero";

    this.numer = numerator;
    if (this.numer == 0)
        this.denom = 1;
    else
        this.denom = denominator;

    this.normalize();
}

// getter methods
Rational.prototype.numerator   = function() {return this.numer};
Rational.prototype.denominator = function() {return this.denom};

// clone a rational
Rational.prototype.dup = function() {
    return new Rational(this.numerator(), this.denominator()); 
};

// conversion methods
Rational.prototype.toString = function() {
    if (this.denominator() == 1) {
        return this.numerator().toString();
    } else {
        // implicit conversion of numbers to strings
        return this.numerator() + '/' + this.denominator()
    }
};
Rational.prototype.toFloat  = function() {return eval(this.toString())}
Rational.prototype.toInt    = function() {return Math.floor(this.toFloat())};

// reduce 
Rational.prototype.normalize = function() {
    // greatest common divisor
    var a=Math.abs(this.numerator()), b=Math.abs(this.denominator())
    while (b != 0) {
        var tmp = a;
        a = b;
        b = tmp % b;
    }
    // a is the gcd

    this.numer /= a;
    this.denom /= a;
    if (this.denom < 0) {
        this.numer *= -1;
        this.denom *= -1;
    }
    return this;
}

// absolute value
// returns a new rational
Rational.prototype.abs = function() {
    return new Rational(Math.abs(this.numerator()), this.denominator());
};

// inverse
// returns a new rational
Rational.prototype.inv = function() {
    return new Rational(this.denominator(), this.numerator());
};

//
// arithmetic methods

// variadic, modifies receiver
Rational.prototype.add = function() {
    for (var i = 0; i < arguments.length; i++) {
        this.numer = this.numer * arguments[i].denominator() + this.denom * arguments[i].numerator();
        this.denom = this.denom * arguments[i].denominator();
    }
    return this.normalize();
};

// variadic, modifies receiver
Rational.prototype.subtract = function() {
    for (var i = 0; i < arguments.length; i++) {
        this.numer = this.numer * arguments[i].denominator() - this.denom * arguments[i].numerator();
        this.denom = this.denom * arguments[i].denominator();
    }
    return this.normalize();
};

// unary "-" operator
// returns a new rational
Rational.prototype.neg = function() {
    return (new Rational(0)).subtract(this);
};

// variadic, modifies receiver
Rational.prototype.multiply = function() {
    for (var i = 0; i < arguments.length; i++) {
        this.numer *= arguments[i].numerator();
        this.denom *= arguments[i].denominator();
    }
    return this.normalize();
};

// modifies receiver
Rational.prototype.divide = function(rat) {
    return this.multiply(rat.inv());
}


// increment
// modifies receiver
Rational.prototype.inc = function() {
    this.numer += this.denominator();
    return this.normalize();
}

// decrement
// modifies receiver
Rational.prototype.dec = function() {
    this.numer -= this.denominator();
    return this.normalize();
}

//
// comparison methods

Rational.prototype.isZero = function() {
    return (this.numerator() == 0);
}
Rational.prototype.isPositive = function() {
    return (this.numerator() > 0);
}
Rational.prototype.isNegative = function() {
    return (this.numerator() < 0);
}

Rational.prototype.eq = function(rat) {
    return this.dup().subtract(rat).isZero();
}
Rational.prototype.ne = function(rat) {
    return !(this.eq(rat));
}
Rational.prototype.lt = function(rat) {
    return this.dup().subtract(rat).isNegative();
}
Rational.prototype.gt = function(rat) {
    return this.dup().subtract(rat).isPositive();
}
Rational.prototype.le = function(rat) {
    return !(this.gt(rat));
}
Rational.prototype.ge = function(rat) {
    return !(this.lt(rat));
}
```

;Testing

```javascript
function assert(cond, msg) { if (!cond) throw msg; }

print('testing')
var a, b, c, d, e, f;

//test creation
a = new Rational(0); assert(a.toString() == "0", "Rational(0).toString() == '0'")
a = new Rational(2); assert(a.toString() == "2", "Rational(2).toString() == '2'")
a = new Rational(1,2); assert(a.toString() == "1/2", "Rational(1,2).toString() == '1/2'")
b = new Rational(2,-12); assert(b.toString() == "-1/6", "Rational(1,6).toString() == '1/6'")
f = new Rational(0,9)

a = new Rational(1,3)
b = new Rational(1,2)
c = new Rational(1,3)

assert(!(a.eq(b)), "1/3 == 1/2")
assert(a.eq(c), "1/3 == 1/3")
assert(a.ne(b), "1/3 != 1/2")
assert(!(a.ne(c)), "1/3 != 1/3")
assert(a.lt(b), "1/3 < 1/2")
assert(!(b.lt(a)), "1/2 < 1/3")
assert(!(a.lt(c)), "1/3 < 1/3")
assert(!(a.gt(b)), "1/3 > 1/2")
assert(b.gt(a), "1/2 > 1/3")
assert(!(a.gt(c)), "1/3 > 1/3")

assert(a.le(b), "1/3 <= 1/2")
assert(!(b.le(a)), "1/2 <= 1/3")
assert(a.le(c), "1/3 <= 1/3")
assert(!(a.ge(b)), "1/3 >= 1/2")
assert(b.ge(a), "1/2 >= 1/3")
assert(a.ge(c), "1/3 >= 1/3")

a = new Rational(1,2)
b = new Rational(1,6)
a.add(b); assert(a.eq(new Rational(2,3)), "1/2 + 1/6 == 2/3")
c = a.neg(); assert(a.eq(new Rational(2,3)), "neg(1/2) == -1/2")
             assert(c.eq(new Rational(2,-3)), "neg(1/2) == -1/2")
d = c.abs(); assert(c.eq(new Rational(-2,3)), "abs(neg(1/2)) == 1/2")
             assert(d.eq(new Rational(2,3)), "abs(neg(1/2)) == 1/2")
b.subtract(a); assert(b.eq(new Rational(-1,2)), "1/6 - 1/2 == -1/3")

c = a.neg().abs(); assert(c.eq(a), "abs(neg(1/2)) == 1/2")
c = (new Rational(-1,3)).inv(); assert(c.toString() == '-3', "inv(1/6 - 1/2) == -3")
try {
    e = f.inv();
    throw "should have been an error: " +f + '.inv() = ' + e
} catch (e) {
    assert(e == "divide by zero", "0.inv() === error")
}

b = new Rational(1,6)
b.add(new Rational(2,3), new Rational(4,2)); assert(b.toString() == "17/6", "1/6+2/3+4/2 == 17/6");

a = new Rational(1,3);
b = new Rational(1,6)
c = new Rational(5,6);
d = new Rational(1/5);
e = new Rational(2);
f = new Rational(0,9);


assert(c.dup().multiply(d).eq(b), "5/6 * 1/5 = 1/6")
assert(c.dup().multiply(d,e).eq(a), "5/6 * 1/5 *2 = 1/3")
assert(c.dup().multiply(d,e,f).eq(f), "5/6 * 1/5 *2*0 = 0")

c.divide(new Rational(5));
assert(c.eq(b), "5/6 / 5 = 1/6b")

try {
    e = c.divide(f)
    throw "should have been an error: " + c + "/" + f + '= ' + e
} catch (e) {
    assert(e == "divide by zero", "0.inv() === error")
}


print('all tests passed');
```


;Finding perfect numbers

```javascript
function factors(num) {
    var factors = new Array();
    var sqrt = Math.floor(Math.sqrt(num)); 
    for (var i = 1; i <= sqrt; i++) {
        if (num % i == 0) {
            factors.push(i);
            if (num / i != i) 
                factors.push(num / i);
        }
    }
    factors.sort(function(a,b){return a-b});  // numeric sort
    return factors;
}

function isPerfect(n) {
    var sum = new Rational(0);
    var fctrs = factors(n);
    for (var i = 0; i < fctrs.length; i++) 
        sum.add(new Rational(1, fctrs[i]));

    // note, fctrs includes 1, so sum should be 2
    return sum.toFloat() == 2.0;
}

// find perfect numbers less than 2^19
for (var n = 2; n < Math.pow(2,19); n++)
    if (isPerfect(n))
        print("perfect: " + n);

// test 5th perfect number
var n = Math.pow(2,12) * (Math.pow(2,13) - 1);
if (isPerfect(n))
    print("perfect: " + n);
```

{{out}}

```txt
perfect: 6
perfect: 28
perfect: 496
perfect: 8128
perfect: 33550336
```

