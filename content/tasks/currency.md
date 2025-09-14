+++
title = "Currency"
description = ""
date = 2019-09-20T12:29:40Z
aliases = []
[extra]
id = 17038
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "algol_68",
  "awk",
  "c",
  "clojure",
  "cobol",
  "csharp",
  "factor",
  "fsharp",
  "go",
  "haskell",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "m2000_interpreter",
  "maple",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "rexx",
  "ring",
  "scala",
  "sidef",
  "swift",
  "tcl",
  "vba",
  "zkl",
]
+++

## Task

Show how to represent currency in a simple example, using a data type that represent exact values of dollars and cents.


;Note:
The '''IEEE 754''' binary floating point representations of numbers like   '''2.86'''   and   '''.0765'''   are not exact.

For this example, data will be two items with prices in dollars and cents, a quantity for each, and a tax rate.

Use the values:
::* 4000000000000000 hamburgers at $5.50 each
::* 2 milkshakes at $2.86 each, and
::* a tax rate of 7.65%.



(That number of hamburgers is a 4 with 15 zeros after it.    The number is contrived to exclude naïve task solutions using 64 bit floating point types.)

Compute and output (show results on this page):
::* the total price before tax
::* the tax
::* the total with tax



The tax value must be computed by rounding to the nearest whole cent and this exact value must be added to the total price before tax.

The output must show dollars and cents with a decimal point.

The three results displayed should be:
::* 22000000000000005.72
::* 1683000000000000.44
::* 23683000000000006.16



Dollar signs and thousands separators are optional.





## ALGOL 68

```algol68
BEGIN
    # currency calculations                                              #
    # simple fixed point type, LONG INT is 64-bit in Algol 68G           #
    MODE FIXED    = STRUCT( LONG INT value
                          , INT      decimals
                          , INT      fraction modulus
                          );
    # make CURRENCY a synonym for FIXED                                  #
    MODE CURRENCY = FIXED;
    # dyadic operator so we can write e.g. 5 DOLLARS 50 to construct     #
    # a CURRENCY value with 2 decimal places                             #
    PRIO DOLLARS = 9;
    OP DOLLARS = ( LONG INT v, INT dp )CURRENCY: ( ( v * 100 ) + dp, 2, 100 );
    OP DOLLARS = (      INT v, INT dp )CURRENCY: LENG v DOLLARS dp;
    # issues an error message and stops the program if a has a different #
    # number of decimal places to b                                      #
    PROC check compatible = ( CURRENCY a, b )VOID:
         IF decimals OF a /= decimals OF b THEN print( ( "Incompatible CURRENCY values", newline ) ); stop FI;
    # operators to multiply CURRENCY values by integers                  #
    OP * = ( CURRENCY v, LONG INT m )CURRENCY: ( value OF v * m, decimals OF v, fraction modulus OF v );
    OP * = ( CURRENCY v,      INT m )CURRENCY: v * LENG m;
    # returns the CURRENCY value a + b                                   #
    OP + = ( CURRENCY a, CURRENCY b )CURRENCY:
         BEGIN
            check compatible( a, b );
            ( value OF a + value OF b, decimals OF a, fraction modulus OF a )
         END # + # ;
    # multiplies the CURRENCY value a by the FIXED value b,              #
    # rounding the result to the decimal places of a                     #
    OP * = ( CURRENCY a, FIXED b )CURRENCY:
         BEGIN
            LONG INT result := ( value OF a * value OF b );
            IF decimals OF b > 0 THEN
                INT d                = fraction modulus OF b;
                LONG INT abs result := ABS result;
                INT extra places     = SHORTEN ( abs result MOD d );
                abs result OVERAB d;
                IF extra places >= d OVER 2 THEN abs result +:= 1 FI;
                IF result < 0 THEN result := - abs result ELSE result := abs result FI
            FI;
            ( result, decimals OF a, fraction modulus OF a )
         END # * # ;
    # converts a FIXED value to a STRING with the appropriate number of  #
    # decimal places                                                     #
    OP TOSTRING = ( FIXED v )STRING:
         IF decimals OF v < 1 THEN
            whole( value OF v, 0 )
         ELSE
            INT    d       = fraction modulus OF v;
            STRING result := whole( value OF v OVER d, 0 );
            STRING dp     := whole( ( ABS value OF v ) MOD d, - decimals OF v );
            FOR i FROM LWB dp TO UPB dp DO IF dp[ i ] = " " THEN dp[ i ] := "0" FI OD;
            result + "." + dp
         FI # TOSTRING # ;
    # Task calculations                                                   #
    CURRENCY hb    = 5 DOLLARS 50 * LONG 4000000000000000;
    CURRENCY ms    = 2 DOLLARS 86 * 2;
    FIXED    rate  = ( 765, 4, 10 000 ); # 0.0765                         #
    CURRENCY net   = hb + ms;
    CURRENCY tax   = net * rate;
    CURRENCY total = net + tax;
    print( ( "before tax: ", TOSTRING net,   newline ) );
    print( ( "tax:        ", TOSTRING tax,   newline ) );
    print( ( "total:      ", TOSTRING total, newline ) )
END
```

```txt

before tax: 22000000000000005.72
tax:        1683000000000000.44
total:      23683000000000006.16

```



## AWK


### version 1


```AWK

# syntax: GAWK -M -f CURRENCY.AWK
# using GNU Awk 4.1.1, API: 1.1 (GNU MPFR 3.1.2, GNU MP 5.1.2)
BEGIN {
    PREC = 100
    hamburger_p = 5.50
    hamburger_q = 4000000000000000
    hamburger_v = hamburger_p * hamburger_q
    milkshake_p = 2.86
    milkshake_q = 2
    milkshake_v = milkshake_p * milkshake_q
    subtotal = hamburger_v + milkshake_v
    tax = subtotal * .0765
    printf("%-9s %8s %18s %22s\n","item","price","quantity","value")
    printf("hamburger %8.2f %18d %22.2f\n",hamburger_p,hamburger_q,hamburger_v)
    printf("milkshake %8.2f %18d %22.2f\n\n",milkshake_p,milkshake_q,milkshake_v)
    printf("%37s %22.2f\n","subtotal",subtotal)
    printf("%37s %22.2f\n","tax",tax)
    printf("%37s %22.2f\n","total",subtotal+tax)
    exit(0)
}

```

<p>Output:</p>

```txt

item         price           quantity                  value
hamburger     5.50   4000000000000000   22000000000000000.00
milkshake     2.86                  2                   5.72

                             subtotal   22000000000000005.72
                                  tax    1683000000000000.41
                                total   23683000000000006.13

```


### version 2


```AWK

# syntax: GAWK -M -f CURRENCY2.AWK
# using GNU Awk 4.1.1, API: 1.1 (GNU MPFR 3.1.2, GNU MP 5.1.2)
# INT is used to define values and do math; results then converted to FLOAT
BEGIN {
    PREC = 100
    hamburger_p = 550
    hamburger_q = 4000000000000000
    hamburger_v = hamburger_p * hamburger_q
    milkshake_p = 286
    milkshake_q = 2
    milkshake_v = milkshake_p * milkshake_q
    subtotal = hamburger_v + milkshake_v
    tax = subtotal * 765
    subtotal /= 100
    tax /= 1000000
    printf("%-9s %8s %18s %22s\n","item","price","quantity","value")
    printf("hamburger %8.2f %18d %22.2f\n",hamburger_p/100,hamburger_q,hamburger_v/100)
    printf("milkshake %8.2f %18d %22.2f\n\n",milkshake_p/100,milkshake_q,milkshake_v/100)
    printf("%37s %22.2f\n","subtotal",subtotal)
    printf("%37s %22.2f\n","tax",tax)
    printf("%37s %22.2f\n","total",subtotal+tax)
    exit(0)
}

```

<p>Output:</p>

```txt

item         price           quantity                  value
hamburger     5.50   4000000000000000   22000000000000000.00
milkshake     2.86                  2                   5.72

                             subtotal   22000000000000005.72
                                  tax    1683000000000000.44
                                total   23683000000000006.16

```



## C

This implementation uses the GMP library for arbitrary precision arithmetic. The only data type used here is '''mpf_t''', the following text is from the GMP documentation :

```txt

Floating point number or Float for short, is an arbitrary precision mantissa with a limited precision exponent. The C data type for such objects is mpf_t. For example:

mpf_t fp;

```

One remark about the code, notice that for setting all other variables the '''mpf_set_d''' function is used:

```C

	mpf_set_d(burgerUnitPrice,5.50);
	mpf_set_d(milkshakePrice,2 * 2.86);
	mpf_set_d(burgerNum,4000000000000000);
	mpf_set_d(milkshakeNum,2);

```

But when it comes to the tax rate, it's '''mpf_set_str''':

```C

	mpf_set_str(tax,"0.0765",10);

```

The reason is a weird rounding off error which happens if the mpf_set_d function is used. Documentation and example usages of GMP are very rare on the net possibly because it is used almost exclusively by academia and high tech industries. The implementation below is the result of a lot of fiddling, gotchas and lessons learnt, just how good programming should always be :)
```C


#include<stdio.h>
#include<gmp.h>

int main()
{
	mpf_t burgerUnitPrice, milkshakePrice, burgerTotalPrice, totalPrice, tax, burgerNum, milkshakeNum;

	mpf_inits(burgerUnitPrice, milkshakePrice, burgerTotalPrice, totalPrice, tax,burgerNum, milkshakeNum,NULL);

	mpf_set_d(burgerUnitPrice,5.50);
	mpf_set_d(milkshakePrice,2 * 2.86);
	mpf_set_d(burgerNum,4000000000000000);
	mpf_set_d(milkshakeNum,2);

	mpf_mul(burgerTotalPrice,burgerNum,burgerUnitPrice);
	mpf_add(totalPrice,burgerTotalPrice,milkshakePrice);

	mpf_set_str(tax,"0.0765",10);
	mpf_mul(tax,totalPrice,tax);

	gmp_printf("\nTotal price before tax : $ %.*Ff", 2, totalPrice);
	gmp_printf("\nTotal tax : $ %.*Ff", 2, tax);

	mpf_add(totalPrice,totalPrice,tax);

	gmp_printf("\nTotal price after tax : $ %.*Ff", 2, totalPrice);

	return 0;
}

```

Output:

```txt

Total price before tax : $ 22000000000000005.72
Total tax : $ 1683000000000000.44
Total price after tax : $ 23683000000000006.16

```



## C#

The built in C# type decimal has a max value of 79228162514264337593543950335.

```c#
using System;
using System.Collections.Generic;

namespace Currency
{
    class Program
    {
        static void Main(string[] args)
        {
            MenuItem hamburger = new MenuItem() { Name = "Hamburger", Price = 5.5M };
            MenuItem milkshake = new MenuItem() { Name = "Milkshake", Price = 2.86M };

            IList<CartItem> cart = new List<CartItem>();
            cart.Add(new CartItem() { item = hamburger, quantity = 4000000000000000 });
            cart.Add(new CartItem() { item = milkshake, quantity = 2 });

            decimal total = CalculateTotal(cart);

            Console.WriteLine(string.Format("Total before tax: {0:C}", total));

            // Add Tax
            decimal tax = total * 0.0765M;

            Console.WriteLine(string.Format("Tax: {0:C}", tax));

            total += tax;

            Console.WriteLine(string.Format("Total with tax: {0:C}", total));
        }

        private static decimal CalculateTotal(IList<CartItem> cart)
        {
            decimal total = 0M;

            foreach (CartItem item in cart)
            {
                total += item.quantity * item.item.Price;
            }

            return total;
        }

        private struct MenuItem
        {
            public string Name { get; set; }
            public decimal Price { get; set; }
        }

        private struct CartItem
        {
            public MenuItem item { get; set; }
            public decimal quantity { get; set; }
        }
    }
}
```

```txt
Total before tax: $22,000,000,000,000,005.72
Tax: $1,683,000,000,000,000.44
Total with tax: $23,683,000,000,000,006.16
```



## Clojure

```clojure
(require '[clojurewerkz.money.amounts    :as ma])
(require '[clojurewerkz.money.currencies :as mc])
(require '[clojurewerkz.money.format     :as mf])

(let [burgers (ma/multiply (ma/amount-of mc/USD 5.50) 4000000000000000)
      milkshakes (ma/multiply (ma/amount-of mc/USD 2.86) 2)
      pre-tax (ma/plus burgers milkshakes)
      tax (ma/multiply pre-tax 0.0765 :up)]
  (println "Total before tax: " (mf/format pre-tax))
  (println "             Tax: " (mf/format tax))
  (println "  Total with tax: " (mf/format (ma/plus pre-tax tax))))
```


```txt

Total before tax:  $22,000,000,000,000,005.72
             Tax:  $1,683,000,000,000,000.44
  Total with tax:  $23,683,000,000,000,006.16
```



## COBOL

COBOL supports up to 31 digits of decimal precision, so won't need any fancy currency/BigInteger types!

During calculations the default ROUNDED MODE IS clause, when specified, is NEAREST-AWAY-FROM-ZERO. When ROUNDED is not specified, the default mode is TRUNCATION.  The term "banker's rounding" implies NEAREST-EVEN.

```cobol>       >
SOURCE FREE
IDENTIFICATION DIVISION.
PROGRAM-ID. currency-example.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  Burger-Price                        CONSTANT 5.50.
01  Milkshake-Price                     CONSTANT 2.86.
01  num-burgers                         PIC 9(18) VALUE 4000000000000000.
01  num-milkshakes                      PIC 9(18) VALUE 2.
01  tax                                 PIC 9(18)V99.
01  tax-edited                          PIC $(17)9.99.
01  Tax-Rate                            CONSTANT 7.65.
01  total                               PIC 9(18)V99.
01  total-edited                        PIC $(17)9.99.

PROCEDURE DIVISION.
    COMPUTE total rounded, total-edited rounded =
        num-burgers * Burger-Price + num-milkshakes * Milkshake-Price
    DISPLAY "Total before tax: " total-edited

    COMPUTE tax rounded, tax-edited rounded = total * (Tax-Rate / 100)
    DISPLAY "             Tax: " tax-edited

    ADD tax TO total GIVING total-edited rounded
    DISPLAY "  Total with tax: " total-edited
    .
END PROGRAM currency-example.
```


```txt

Total before tax: $22000000000000005.72
             Tax:  $1683000000000000.44
  Total with tax: $23683000000000006.16

```



## Factor

Factor's <tt>ratio</tt> type can handle arbitrary-precision calculations with rational numbers. The <tt>money</tt> vocabulary implements convenience words for treating rationals as money. The <tt>DECIMAL:</tt> parsing word is used to convert the tax rate <tt>0.0765</tt> to a ratio <tt>153/2000</tt>. The <tt>money.</tt> word is used to print the subtotal <tt>22000000000000005+18/25</tt>, tax <tt>1683000000000000+21879/50000</tt>, and total <tt>23683000000000006+7879/50000</tt> formatted as you would expect.

```factor
USING: combinators.smart io kernel math math.functions money ;

10 15 ^ 4 * 5+50/100 * ! hamburger subtotal
2 2+86/100 *           ! milkshake subtotal
+                      ! subtotal
dup DECIMAL: 0.0765 *  ! tax
[ + ] preserving       ! total

"Total before tax: " write [ money. ] 2dip
"Tax: " write [ money. ] dip
"Total with tax: " write money.
```

```txt

Total price before tax: $22,000,000,000,000,005.72
Tax: $1,683,000,000,000,000.44
Total with tax: $23,683,000,000,000,006.16

```



## F#


```fsharp
open System

let hamburgers = 4000000000000000M
let hamburgerPrice = 5.50M
let milkshakes = 2M
let milkshakePrice = 2.86M
let taxRate = 0.0765M

let total = hamburgers * hamburgerPrice + milkshakes * milkshakePrice
let tax = total * taxRate
let totalWithTax = total + tax

printfn "Total before tax:\t$%M" <| Math.Round (total, 2)
printfn "             Tax:\t$%M" <| Math.Round (tax, 2)
printfn "           Total:\t$%M" <| Math.Round (totalWithTax, 2)
```


```txt

Total before tax:    $22000000000000005.72
             Tax:    $1683000000000000.44
  Total with tax:    $23683000000000006.16

```



## Go


```go
package main

import (
    "fmt"
    "log"
    "math/big"
)

// DC for dollars and cents.  Value is an integer number of cents.
type DC int64

func (dc DC) String() string {
    d := dc / 100
    if dc < 0 {
        dc = -dc
    }
    return fmt.Sprintf("%d.%02d", d, dc%100)
}

// Extend returns extended price of a unit price.
func (dc DC) Extend(n int) DC {
    return dc * DC(n)
}

var one = big.NewInt(1)
var hundred = big.NewRat(100, 1)

// ParseDC parses dollars and cents as a string into a DC.
func ParseDC(s string) (DC, bool) {
    r, ok := new(big.Rat).SetString(s)
    if !ok {
        return 0, false
    }
    r.Mul(r, hundred)
    if r.Denom().Cmp(one) != 0 {
        return 0, false
    }
    return DC(r.Num().Int64()), true
}

// TR for tax rate.  Value is an an exact rational.
type TR struct {
    *big.Rat
}
func NewTR() TR {
    return TR{new(big.Rat)}
}

// SetString overrides Rat.SetString to return the TR type.
func (tr TR) SetString(s string) (TR, bool) {
    if _, ok := tr.Rat.SetString(s); !ok {
        return TR{}, false
    }
    return tr, true
}

var half = big.NewRat(1, 2)

// Tax computes a tax amount, rounding to the nearest cent.
func (tr TR) Tax(dc DC) DC {
    r := big.NewRat(int64(dc), 1)
    r.Add(r.Mul(r, tr.Rat), half)
    return DC(new(big.Int).Div(r.Num(), r.Denom()).Int64())
}

func main() {
    hamburgerPrice, ok := ParseDC("5.50")
    if !ok {
        log.Fatal("Invalid hamburger price")
    }
    milkshakePrice, ok := ParseDC("2.86")
    if !ok {
        log.Fatal("Invalid milkshake price")
    }
    taxRate, ok := NewTR().SetString("0.0765")
    if !ok {
        log.Fatal("Invalid tax rate")
    }

    totalBeforeTax := hamburgerPrice.Extend(4000000000000000) +
        milkshakePrice.Extend(2)
    tax := taxRate.Tax(totalBeforeTax)
    total := totalBeforeTax + tax

    fmt.Printf("Total before tax: %22s\n", totalBeforeTax)
    fmt.Printf("             Tax: %22s\n", tax)
    fmt.Printf("           Total: %22s\n", total)
}
```

```txt

Total before tax:   22000000000000005.72
             Tax:    1683000000000000.44
           Total:   23683000000000006.16

```


## Haskell


```haskell
import Data.Fixed
import Text.Printf

type Percent = Centi
type Dollars = Centi

tax :: Percent -> Dollars -> Dollars
tax rate = MkFixed . round . (rate *)

printAmount :: String -> Dollars -> IO ()
printAmount name = printf "%-10s %20s\n" name . showFixed False

main :: IO ()
main = do
  let subtotal = 4000000000000000 * 5.50 + 2 * 2.86
      tx       = tax 7.65 subtotal
      total    = subtotal + tx
  printAmount "Subtotal" subtotal
  printAmount "Tax"      tx
  printAmount "Total"    total
```


```txt

$ ./currency
Subtotal   22000000000000005.72
Tax         1683000000000000.44
Total      23683000000000006.16

```



## J


We use a naive implementation with arbitrary precision (rational) numbers:


```j
require 'format/printf'
fmtD=: 0j2&":     NB. format rational as decimal

Items=:   ;:          'Hamburger     Milkshake'
Quantities=: x:   4000000000000000      2
Prices=:   x:           5.50          2.86
Tax_rate=: x:  0.0765
Values=: Quantities * Prices
Subtotal=: +/ Values
Tax=: Tax_rate * Subtotal
Total=: Subtotal + Tax

OutputTemplate=: noun define
Item          Price            Quantity          Value
%9s %8s %20d %22s
%9s %8s %20d %22s
                               -------------------------------
                               Subtotal:  %20s
                                    Tax:  %20s
                                  Total:  %20s
)

Vals=: (,Items ,. (fmtD&.> Prices) ,. (<"0 Quantities) ,. (fmtD&.> Values)) , fmtD&.> Subtotal,Tax,Total
OutputTemplate printf Vals
```

```txt
Item          Price            Quantity          Value
Hamburger     5.50     4000000000000000   22000000000000000.00
Milkshake     2.86                    2                   5.72
                               -------------------------------
                               Subtotal:  22000000000000005.72
                                    Tax:   1683000000000000.44
                                  Total:  23683000000000006.16
```


(Note that if you ever get a bill like this in real life, you should question the person who gave it to you. And, possibly consider legal action and/or patronizing a different establishment. This is because (a) you did not order that many hamburgers, and (b) they did not deliver that many hamburgers, and (c) that much hamburger probably does not exist, and ...)


## Java


```java
import java.math.*;
import java.util.*;

public class Currency {
    final static String taxrate = "7.65";

    enum MenuItem {

        Hamburger("5.50"), Milkshake("2.86");

        private MenuItem(String p) {
            price = new BigDecimal(p);
        }

        public final BigDecimal price;
    }

    public static void main(String[] args) {
        Locale.setDefault(Locale.ENGLISH);

        MathContext mc = MathContext.DECIMAL128;

        Map<MenuItem, BigDecimal> order = new HashMap<>();
        order.put(MenuItem.Hamburger, new BigDecimal("4000000000000000"));
        order.put(MenuItem.Milkshake, new BigDecimal("2"));

        BigDecimal subtotal = BigDecimal.ZERO;
        for (MenuItem it : order.keySet())
            subtotal = subtotal.add(it.price.multiply(order.get(it), mc));

        BigDecimal tax = new BigDecimal(taxrate, mc);
        tax = tax.divide(new BigDecimal("100"), mc);
        tax = subtotal.multiply(tax, mc);

        System.out.printf("Subtotal: %20.2f%n", subtotal);
        System.out.printf("     Tax: %20.2f%n", tax);
        System.out.printf("   Total: %20.2f%n", subtotal.add(tax));
    }
}
```



```txt
Subtotal: 22000000000000005.72
     Tax:  1683000000000000.44
   Total: 23683000000000006.16
```



## JavaScript


```javascript
const money = require('money-math')

let hamburgers = 4000000000000000
let hamburgerPrice = 5.50

let shakes = 2
let shakePrice = 2.86

let tax = 7.65

let hamburgerTotal = money.mul(hamburgers.toFixed(0), money.floatToAmount(hamburgerPrice))
let shakeTotal = money.mul(shakes.toFixed(0), money.floatToAmount(shakePrice))

let subTotal = money.add(hamburgerTotal, shakeTotal)

let taxTotal = money.percent(subTotal, tax)

let total = money.add(subTotal, taxTotal)

console.log('Hamburger Total:', hamburgerTotal)
console.log('Shake Total:', shakeTotal)
console.log('Sub Total:', subTotal)
console.log('Tax:', taxTotal)
console.log('Total:', total)

```



## Julia

```julia
using Printf

p  = [big"5.50", big"2.86"]
q  = [4000000000000000, 2]
tr = big"0.0765"

beftax = p' * q
tax    = beftax * tr
afttax = beftax + tax

@printf " - tot. before tax: %20.2f \$\n" beftax
@printf " -             tax: %20.2f \$\n" tax
@printf " - tot. after  tax: %20.2f \$\n" afttax
```


```txt
 - tot. before tax: 22000000000000005.72 $
 -             tax:  1683000000000000.44 $
 - tot. after  tax: 23683000000000006.16 $
```



## Kotlin


```scala
// version 1.1.2

import java.math.BigDecimal
import java.math.MathContext

fun main(args: Array<String>) {
    val mc = MathContext.DECIMAL128
    val nHamburger  = BigDecimal("4000000000000000", mc)
    val pHamburger  = BigDecimal("5.50")
    val nMilkshakes = BigDecimal("2", mc)
    val pMilkshakes = BigDecimal("2.86")
    val taxRate     = BigDecimal("0.0765")
    val price = nHamburger * pHamburger + nMilkshakes * pMilkshakes
    val tax = price * taxRate
    val fmt = "%20.2f"
    println("Total price before tax : ${fmt.format(price)}")
    println("Tax thereon @ 7.65%    : ${fmt.format(tax)}")
    println("Total price after tax  : ${fmt.format(price + tax)}")
}
```


```txt

Total price before tax : 22000000000000005.72
Tax thereon @ 7.65%    :  1683000000000000.44
Total price after tax  : 23683000000000006.16

```


## M2000 Interpreter

This task written in M2000 Environment running on Wine 3.6, in a Linux Ubuntu Studio. M2000 environment is an ActiveX object, written with VB6, which use many types from [https://en.wikipedia.org/wiki/Variant_type COM Variant Type].

```M2000 Interpreter

Module Currency_Task {
      Locale 1033
      Font "Courier New"
      Form 80,32
      \\Decimal type
      hamburgers=4000000000000000@
      \\ Currency type
      hamburger_price=5.5#
      milkshakes=2#
      milkshake_price=2.86#
      tax_rate=0.0765#
      \\ Using Columns with variable width in console
      PrHeadLine("Item","price","quantity", "value")
      PrLine("hamburger",hamburger_price,hamburgers,hamburgers*hamburger_price)
      PrLine("milkshake", milkshake_price,milkshakes,milkshakes*milkshake_price)
      PrResults( "subtotal", hamburgers*hamburger_price+milkshakes*milkshake_price)
      PrResults("tax", (hamburgers*hamburger_price+milkshakes*milkshake_price)*tax_rate)
      \\ 1 is double by default we can use 1# or 1@
      PrResults("total", (hamburgers*hamburger_price+milkshakes*milkshake_price)*(tax_rate+1))

      \\ Using variables for partial calculations. They get type from expression result
      h_p_q=hamburgers*hamburger_price
      m_p_q=milkshakes*milkshake_price

      \\ Using format$ to prepare final strings
      Print format$("{0:15}{1:-8}{2:-25}{3:-25}","Item", "price", "quantity", "value")
      Print format$("{0:15}{1:2:-8}{2:0:-25}{3:2:-25}","hamburger",hamburger_price,hamburgers, h_p_q)
      Print format$("{0:15}{1:2:-8}{2:0:-25}{3:2:-25}","milkshake", milkshake_price,milkshakes,m_p_q)
      Print format$("{0:-48}{1:2:-25}","subtotal", h_p_q+m_p_q)
      Print format$("{0:-48}{1:2:-25}","tax", (h_p_q+m_p_q)*tax_rate)
      Print format$("{0:-48}{1:2:-25}","total", (h_p_q+m_p_q)*(tax_rate+1))
      \\ Another time to feed Document to export to clipboard
      Document Doc$=format$("{0:15}{1:-8}{2:-25}{3:-25}","Item", "price", "quantity", "value")+{
      }+format$("{0:15}{1:2:-8}{2:0:-25}{3:2:-25}","hamburger",hamburger_price,hamburgers, h_p_q)+{
      }+format$("{0:15}{1:2:-8}{2:0:-25}{3:2:-25}","milkshake", milkshake_price,milkshakes,m_p_q)+{
      }+format$("{0:-48}{1:2:-25}","subtotal", h_p_q+m_p_q)+{
      }+format$("{0:-48}{1:2:-25}","tax", (h_p_q+m_p_q)*tax_rate)+{
      }+format$("{0:-48}{1:2:-25}","total", (h_p_q+m_p_q)*(tax_rate+1))+{
      }
      clipboard Doc$
      \\ one line user function definition
      \\ x get type from passed value
      Def ExpressionType$(x)=Type$(X)
      \\ Check Expression final type
      Print ExpressionType$(hamburgers)="Decimal"
      Print ExpressionType$(milkshakes)="Currency"
      Print ExpressionType$(h_p_q)="Decimal"
      Print ExpressionType$(m_p_q)="Currency"
      Print ExpressionType$((h_p_q+m_p_q)*tax_rate)="Decimal"
      Print ExpressionType$((h_p_q+m_p_q)*(tax_rate+1))="Decimal"

      Sub PrHeadLine(a$,b$,c$,d$)
            Print Part  $(1,15),a$,$(3,8),b$, $(3,25),c$, $(3,25),d$
            Print
      End Sub
      Sub PrLine(a$,b,c,d)
            Print Part  $(1,15),a$,$("0.00"),$(3,8),b, $("0"),$(3,25),c,$("0.00"), $(3,25),d
            Print
      End Sub
      Sub PrResults(a$,b)
            Print Part  $(3,48),a$,$("0.00"),$(3,25),b
            Print
      End Sub
}
Currency_Task

```

Optional with $ and thousands separator (a smaller version from above)

```M2000 Interpreter

Module Currency_Task {
      Locale 1033
      Font "Courier New"
      Form 80,32
      hamburgers=4000000000000000@
      hamburger_price=5.5#
      milkshakes=2#
      milkshake_price=2.86#
      tax_rate=0.0765#
      PrHeadLine("Item","price","quantity", "value")
      PrLine("hamburger",hamburger_price,hamburgers,hamburgers*hamburger_price)
      PrLine("milkshake", milkshake_price,milkshakes,milkshakes*milkshake_price)
      PrResults( "subtotal", hamburgers*hamburger_price+milkshakes*milkshake_price)
      PrResults("tax", (hamburgers*hamburger_price+milkshakes*milkshake_price)*tax_rate)
      PrResults("total", (hamburgers*hamburger_price+milkshakes*milkshake_price)*(tax_rate+1))

      h_p_q=hamburgers*hamburger_price
      m_p_q=milkshakes*milkshake_price
      Document Doc$=format$("{0:15}{1:-8}{2:-25}{3:-30}","Item", "price", "quantity", "value")+{
      }+format$("{0:15}{1:-8}{2:-25}{3:-30}","hamburger",str$(hamburger_price,"$#,##0.00"),str$(hamburgers, "#,##0"), Str$(h_p_q,"$#,##0.00"))+{
      }+format$("{0:15}{1:-8}{2:-25}{3:-30}","milkshake", str$(milkshake_price,"$#,##0.00"),Str$(milkshakes, "#,##0"), Str$(m_p_q,"$#,##0.00"))+{
      }+format$("{0:-48}{1:-30}","subtotal", Str$(h_p_q+m_p_q,"$#,##0.00"))+{
      }+format$("{0:-48}{1:-30}","tax", Str$((h_p_q+m_p_q)*tax_rate,"$#,##0.00"))+{
      }+format$("{0:-48}{1:-30}","total", Str$((h_p_q+m_p_q)*(tax_rate+1),"$#,##0.00"))+{
      }
      clipboard Doc$


      Sub PrHeadLine(a$,b$,c$,d$)
            Print Part  $(1,15),a$,$(3,8),b$, $(3,25),c$, $(3,30),d$
            Print
      End Sub
      Sub PrLine(a$,b,c,d)
            Print Part  $(1,15),a$,$("$#,###.00"),$(3,8),b, $("#,##0"),$(3,25),c,$("$#,###.00"), $(3,30),d
            Print
      End Sub
      Sub PrResults(a$,b)
            Print Part  $(3,48),a$,$("$#,###.00"),$(3,30),b
            Print
      End Sub
}
Currency_Task

```

<pre style="height:30ex;overflow:scroll">
Item              price                 quantity                    value
hamburger          5.50         4000000000000000     22000000000000000.00
milkshake          2.86                        2                     5.72
                                        subtotal     22000000000000005.72
                                             tax      1683000000000000.44
                                           total     23683000000000006.16
From Optional
Item              price                 quantity                         value
hamburger         $5.50    4,000,000,000,000,000    $22,000,000,000,000,000.00
milkshake         $2.86                        2                         $5.72
                                        subtotal    $22,000,000,000,000,005.72
                                             tax     $1,683,000,000,000,000.44
                                           total    $23,683,000,000,000,006.16

</pre >


## Maple


```Maple


Digits := 50;
tax := .0765;

burgersquantity := 4000000000000000;
burgersprice := 5.50;
burgerscost := burgersquantity * burgersprice;

milkshakesquantity := 2;
milkshakesprice := 2.86;
milkshakescost := milkshakesquantity * milkshakesprice;

total := burgerscost + milkshakescost;
printf("%.2f\n",total);

totaltax := total * tax;
printf("%.2f\n",totaltax);

totalprice := totaltax + total;
printf("%.2f\n",totalprice);

```

```txt

22000000000000005.72
1683000000000000.44
23683000000000006.16

```




## OCaml


Using the [https://github.com/janestreet/bignum Bignum] library.


```ocaml
#require "bignum" ;;

let p1 = Bignum.((of_string "4000000000000000") * (of_float_decimal 5.50)) ;;
let p2 = Bignum.((of_int 2) * (of_float_decimal 2.86)) ;;

let r1 = Bignum.(p1 + p2) ;;
let r2 = Bignum.(r1 * (of_float_decimal (7.65 /. 100.))) ;;
let r3 = Bignum.(r1 + r2) ;;

let my_to_string v =
  Bignum.(v |> round_decimal ~dir:`Nearest ~digits:2
            |> to_string_hum ~decimals:2) ;;

let () =
  Printf.printf "before tax: %s\n" (my_to_string r1);
  Printf.printf "tax:        %s\n" (my_to_string r2);
  Printf.printf "total:      %s\n" (my_to_string r3);
;;
```

```txt

$ opam install bignum
$ opam install utop
$ eval $(opam env)
$ utop currency.ml
before tax: 22000000000000005.72
tax:        1683000000000000.44
total:      23683000000000006.16

```




## Perl


```perl
use Math::Decimal qw(dec_canonise dec_add dec_mul dec_rndiv_and_rem);

@check = (
    [<Hamburger 5.50 4000000000000000>],
    [<Milkshake 2.86                2>]
);

my $fmt = "%-10s %8s %18s %22s\n";
printf $fmt, <Item Price Quantity Extension>;

my $subtotal = dec_canonise(0);
for $line (@check) {
    ($item,$price,$quant) = @$line;
    $dp = dec_canonise($price); $dq = dec_canonise($quant);
    my $extension = dec_mul($dp,$dq);
    $subtotal = dec_add($subtotal, $extension);
    printf $fmt, $item, $price, $quant, rnd($extension);
}

my $rate  = dec_canonise(0.0765);
my $tax   = dec_mul($subtotal,$rate);
my $total = dec_add($subtotal,$tax);

printf $fmt, '', '', '',          '-----------------';
printf $fmt, '', '', 'Subtotal ', rnd($subtotal);
printf $fmt, '', '', 'Tax ',      rnd($tax);
printf $fmt, '', '', 'Total ',    rnd($total);

sub rnd {
    ($q, $r) = dec_rndiv_and_rem("FLR", @_[0], 1);
    $q . substr((sprintf "%.2f", $r), 1, 3);
}
```

```txt
Item          Price           Quantity              Extension
Hamburger      5.50   4000000000000000   22000000000000000.00
Milkshake      2.86                  2                   5.72
                                            -----------------
                             Subtotal    22000000000000005.72
                                  Tax     1683000000000000.44
                                Total    23683000000000006.16
```



## Perl 6

No need for a special type in Perl 6, since the <tt>Rat</tt> type is used for normal fractions.
(In order to achieve imprecision, you have to explicitly use scientific notation,
or use the <tt>Num</tt> type, or calculate a result that requires a denominator in excess of <tt>2 ** 64</tt>.  (There's no limit on the numerator.))

```perl6
my @check = q:to/END/.lines.map: { [.split(/\s+/)] };
    Hamburger   5.50    4000000000000000
    Milkshake   2.86    2
    END

my $tax-rate = 0.0765;

my $fmt = "%-10s %8s %18s %22s\n";

printf $fmt, <Item Price Quantity Extension>;

my $subtotal = [+] @check.map: -> [$item,$price,$quant] {
    my $extension = $price * $quant;
    printf $fmt, $item, $price, $quant, fix2($extension);
    $extension;
}

printf $fmt, '', '', '', '-----------------';
printf $fmt, '', '', 'Subtotal ', $subtotal;

my $tax = ($subtotal * $tax-rate).round(0.01);
printf $fmt, '', '', 'Tax ', $tax;

my $total = $subtotal + $tax;
printf $fmt, '', '', 'Total ', $total;

# make up for lack of a Rat fixed-point printf format
sub fix2($x) { ($x + 0.001).subst(/ <?after \.\d\d> .* $ /, '') }
```

```txt
Item          Price           Quantity              Extension
Hamburger      5.50   4000000000000000   22000000000000000.00
Milkshake      2.86                  2                   5.72
                                            -----------------
                             Subtotal    22000000000000005.72
                                  Tax     1683000000000000.44
                                Total    23683000000000006.16
```



## Phix

```Phix
include mpfr.e
mpfr_set_default_prec(-20) -- ensure accuracy to at least 20 d.p.

mpfr total_price = mpfr_init("4000000000000000"),
     tmp = mpfr_init("5.5"),
     tax = mpfr_init("0.0765"),
     total = mpfr_init()
mpfr_mul(total_price,total_price,tmp)
mpfr_set_str(tmp,"2.86")
mpfr_mul_si(tmp,tmp,2)
mpfr_add(total_price,total_price,tmp)
mpfr_mul(tax,total_price,tax)
mpfr_add(total,total_price,tax)
mpfr_printf(1,"Total before tax:%21.2Rf\n",total_price)
mpfr_printf(1,"             Tax:%21.2Rf\n",tax)
mpfr_printf(1,"           Total:%21.2Rf\n",total)
```

```txt

Total before tax: 22000000000000005.72
             Tax:  1683000000000000.44
           Total: 23683000000000006.16

```



## PicoLisp

<lang>(scl 2)
(let
   (Before
      (+
         (* 4000000000000000 5.50)
         (* 2 2.86) )
      Tax (*/ Before 7.65 100.00)
      Total (+ Before Tax)
      Fmt (17 27) )
   (tab Fmt "Total before tax:" (format Before *Scl "." ","))
   (tab Fmt "Tax:" (format Tax *Scl "." ","))
   (tab Fmt "Total:" (format Total *Scl "." ",")) )
```

```txt

Total before tax:  22,000,000,000,000,005.72
             Tax:   1,683,000,000,000,000.44
           Total:  23,683,000,000,000,006.16

```



## Python

This uses Pythons decimal module,
(and some copying of names from the Perl 6 example).


```python
from decimal import Decimal as D
from collections import namedtuple

Item = namedtuple('Item', 'price, quant')

items = dict( hamburger=Item(D('5.50'), D('4000000000000000')),
              milkshake=Item(D('2.86'), D('2')) )
tax_rate = D('0.0765')

fmt = "%-10s %8s %18s %22s"
print(fmt % tuple('Item Price Quantity Extension'.upper().split()))

total_before_tax = 0
for item, (price, quant) in sorted(items.items()):
    ext = price * quant
    print(fmt % (item, price, quant, ext))
    total_before_tax += ext
print(fmt % ('', '', '', '--------------------'))
print(fmt % ('', '', 'subtotal', total_before_tax))

tax = (tax_rate * total_before_tax).quantize(D('0.00'))
print(fmt % ('', '', 'Tax', tax))

total = total_before_tax + tax
print(fmt % ('', '', '', '--------------------'))
print(fmt % ('', '', 'Total', total))
```


```txt
ITEM          PRICE           QUANTITY              EXTENSION
hamburger      5.50   4000000000000000   22000000000000000.00
milkshake      2.86                  2                   5.72
                                         --------------------
                              subtotal   22000000000000005.72
                                   Tax    1683000000000000.44
                                         --------------------
                                 Total   23683000000000006.16
```



## Racket

Racket can handle fractions.
To read the decimals numbers as fractions instead of floating point numbers,
they must start with #e.
For example #e.3 -> 3/10 and .3 ->3E-1.
The main problem is rounding correctly and this part is handled in cents-*, that implements a multiplication that rounds to the cents.
The rest of the program is only formatting.

```Racket
#lang racket
(define (cents-* x y)
  (/ (round (* 100 x y)) 100))

(struct item (name count price))

(define (string-pad-right len . strs)
  (define all (apply string-append strs))
  (string-append  all (make-string (- len (string-length all)) #\space)))

(define (string-pad-left len . strs)
  (define all (apply string-append strs))
  (string-append  (make-string (- len (string-length all)) #\space) all))

(define (show-formated name count price total)
  (printf "~a ~a ~a -> ~a\n"
          (string-pad-right 10 name)
          (string-pad-left 18 count)
          (string-pad-left 8 price)
          (string-pad-left 23 total)
          ))

(define (show-item it)
  (show-formated (item-name it)
                 (~r (item-count it))
                 (string-append "$" (~r (item-price it) #:precision '(= 2)))
                 (string-append "$" (~r (cents-* (item-count it) (item-price it)) #:precision '(= 2)))
          ))

(define (show-total all tax-rate)
  (define net (for/sum ([it (in-list all)])
                       (cents-* (item-count it) (item-price it))))
  (define tax (cents-* net tax-rate))
  (show-formated "" "" "net" (string-append "$" (~r net #:precision '(= 2))))
  (show-formated "" "" "tax" (string-append "$" (~r tax #:precision '(= 2))))
  (show-formated "" "" "total" (string-append "$" (~r (+ net tax) #:precision '(= 2))))
  )

(define hamburger (item "hamburger" 4000000000000000 #e5.50))
(define milkshake (item "milkshake" 2 #e2.86))
(define all (list hamburger milkshake))

(for-each show-item all)
(newline)
(show-total all (/ #e7.65 100))
```

```txt
hamburger    4000000000000000    $5.50 ->   $22000000000000000.00
milkshake                   2    $2.86 ->                   $5.72

                                   net ->   $22000000000000005.72
                                   tax ->    $1683000000000000.44
                                 total ->   $23683000000000006.16
```



## REXX

REXX uses characters to represent everything, including all forms of numbers.
So what is expressed as a literal (characters) is what REXX uses.
Essentially, it can be thought of as decimal.

Programming note:   the tax rate can be expressed with or without a percent   ('''%''')  suffix.

### without commas


```rexx
/*REXX program shows a method of computing the total price and tax  for purchased items.*/
numeric digits 200                                        /*support for gihugic numbers.*/
taxRate= 7.65                                             /*number is:   nn   or   nn%  */
if right(taxRate, 1)\=='%'  then taxRate=taxRate / 100    /*handle plain tax rate number*/
taxRate=strip(taxRate, , '%')                             /*strip the  %   (if present).*/
item.  =;                          items=0                /*zero out the register.      */
item.1 = '4000000000000000  $5.50  hamburger'             /*the  first  item purchased. */
item.2 = '               2  $2.86  milkshake'             /* "  second    "      "      */
say center('quantity',22)       center("item",22)       center('price',22)
hdr=center('',        27,"─")   center('',    20,"─")   center('',     27,"─");    say hdr
total=0
         do j=1  while item.j\==''                        /*calculate the total and tax.*/
         parse var item.j   quantity price thing          /*ring up an item on register.*/
         items    = items + quantity                      /*tally the number of items.  */
         price    = translate(price, , '$')               /*maybe scrub out the $ symbol*/
         subtotal = quantity * price                      /*calculate the     sub-total.*/
         total    = total + subtotal                      /*    "      "  running total.*/
         say right(quantity, 27)    left(thing, 20)     show$(subtotal)
         end   /*j*/
say                                              /*display a blank line for separator.  */
say translate(hdr, '═', "─")                     /*display the separator part of the hdr*/
tax=format(total * taxRate, , 2)                 /*round the total tax for all the items*/
say right(items  "(items)", 35)     right('total=', 12)            show$(total)
say right('tax at'  (taxRate * 100 / 1)"%=", 48)                   show$(tax)
say
say right('grand total=', 48)                                   show$(total+tax)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
show$:  return right('$'arg(1), 27)              /*right─justify and format a number.   */
```

```txt

       quantity                 item                  price
────────────────────── ────────────────────── ──────────────────────
      4000000000000000 hamburger               $22000000000000000.00
                     2 milkshake                               $5.72

══════════════════════ ══════════════════════ ══════════════════════
      4000000000000002 (items)         total=  $22000000000000005.72
                                tax at 7.65%=   $1683000000000000.44

                                 grand total=  $23683000000000006.16

```



### with commas


```rexx
/*REXX program shows a method of computing the total price and tax  for purchased items.*/
numeric digits 200                                        /*support for gihugic numbers.*/
taxRate= 7.65                                             /*number is:   nn   or   nn%  */
if right(taxRate, 1)\=='%'  then taxRate=taxRate / 100    /*handle plain tax rate number*/
taxRate=strip(taxRate, , '%')                             /*strip the  %   (if present).*/
item.  =;                          items=0                /*zero out the register.      */
item.1 = '4000000000000000  $5.50  hamburger'             /*the  first  item purchased. */
item.2 = '               2  $2.86  milkshake'             /* "  second    "      "      */
say center('quantity',22)       center("item",22)       center('price',22)
hdr=center('',        27,"─")   center('',    20,"─")   center('',     27,"─");    say hdr
total=0
         do j=1  while item.j\==''                        /*calculate the total and tax.*/
         parse var item.j   quantity price thing          /*ring up an item on register.*/
         items    = items + quantity                      /*tally the number of items.  */
         price    = translate(price, , '$')               /*maybe scrub out the $ symbol*/
         subtotal = quantity * price                      /*calculate the     sub-total.*/
         total    = total + subtotal                      /*    "      "  running total.*/
         say right(quantity, 27)    left(thing, 20)     show$(subtotal)
         end   /*j*/
say                                              /*display a blank line for separator.  */
say translate(hdr, '═', "─")                     /*display the separator part of the hdr*/
tax=format(total*taxRate,,2)                     /*round the total tax for all the items*/
say right(commas(items  "(items)"), 35)      right('total=', 12)       show$(total)
say right('tax at'  (taxRate * 100 / 1)"%=", 48)                       show$(tax)
say
say right('grand total=', 48)  show$(total + tax)
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
commas: procedure;  parse arg _;   n= _'.9';    #=123456789;    b=verify(n, #, "M")
        e=verify(n, #'0', , verify(n, #"0.", 'M') )  - 4        /* [↓]  commatize number*/
           do j=e  to b  by -3;    _=insert(',', _, j);    end  /*j*/;            return _
/*──────────────────────────────────────────────────────────────────────────────────────*/
show$:  return right( commas('$'arg(1) ), 27)    /*right─justify and format a number.   */
```

```txt

         quantity                   item                    price
─────────────────────────── ──────────────────── ───────────────────────────
      4,000,000,000,000,000 hamburger             $22,000,000,000,000,000.00
                          2 milkshake                                  $5.72

═══════════════════════════ ════════════════════ ═══════════════════════════
      4,000,000,000,000,002 (items)       total=  $22,000,000,000,000,005.72
                                   tax at 7.65%=   $1,683,000,000,000,000.44

                                    grand total=  $23,683,000,000,000,006.16

```



## Ring


```ring

# Project  : Currency

nhamburger  = "4000000000"
phamburger  = "5.50"
nmilkshakes = "2"
pmilkshakes = "2.86"
taxrate = "0.0765"
price = nhamburger * phamburger + nmilkshakes * pmilkshakes
tax = price * taxrate
see "total price before tax : " + price + nl
see "tax thereon @ 7.65 : " + tax + nl
see "total price after tax  : " + (price + tax) + nl

```

Output:

```txt

total price before tax : 22000000005.72
tax thereon @ 7.65   : 1683000000.44
total price after tax   : 23683000006.16

```



## Scala


{{libheader|Scala}}Locale is manipulated to demonstrate the behavior with other currencies.

```scala
import java.text.NumberFormat
import java.util.Locale

object SizeMeUp extends App {

  val menu: Map[String, (String, Double)] = Map("burg" ->("Hamburger XL", 5.50), "milk" ->("Milkshake", 2.86))
  val order = List((4000000000000000L, "burg"), (2L, "milk"))

  Locale.setDefault(new Locale("ru", "RU"))

  val (currSymbol, tax) = (NumberFormat.getInstance().getCurrency.getSymbol, 0.0765)

  def placeOrder(order: List[(Long, String)]) = {
    val totals = for ((qty, article) <- order) yield {
      val (desc, itemPrize) = menu(article)
      val (items, post) = (qty, qty * BigDecimal(itemPrize))
      println(f"$qty%16d\t$desc%-16s\t$currSymbol%4s$itemPrize%6.2f\t$post%,25.2f")
      (items, post)
    }
    totals.foldLeft((0L, BigDecimal(0))) { (acc, n) => (acc._1 + n._1, acc._2 + n._2)}
  }

  val (items, beforeTax) = placeOrder(order)

  println(f"$items%16d\t${"ordered items"}%-16s${'\t' + "  Subtotal" + '\t'}$beforeTax%,25.2f")

  val taxation = beforeTax * tax
  println(f"${" " * 16 + '\t' + " " * 16 + '\t' + f"${tax * 100}%5.2f%% tax" + '\t'}$taxation%,25.2f")
  println(f"${" " * 16 + '\t' + " " * 16 + '\t' + "Amount due" + '\t'}${beforeTax + taxation}%,25.2f")
}
```

 4000000000000000	Hamburger XL    	руб.  5,50	22 000 000 000 000 000,00
                2	Milkshake       	руб.  2,86	                     5,72
 4000000000000002	ordered items   	  Subtotal	22 000 000 000 000 005,72
                 	                	 7,65% tax	 1 683 000 000 000 000,44
                 	                	Amount due	23 683 000 000 000 006,16

 Process finished with exit code 0


## Sidef

```ruby
struct Item {
    name, price, quant
}

var check = %q{
    Hamburger   5.50    4000000000000000
    Milkshake   2.86    2
}.lines.grep(/\S/).map { Item(.words...) }

var tax_rate = 0.0765
var fmt = "%-10s %8s %18s %22s\n"

printf(fmt, %w(Item Price Quantity Extension)...)

var subtotal = check.map { |item|
    var extension = Num(item.price)*Num(item.quant)
    printf(fmt, item.name, item.price, item.quant, extension.round(-2))
    extension
}.sum(0)

printf(fmt, '', '', '', '-----------------')
printf(fmt, '', '', 'Subtotal ', subtotal)

var tax = (subtotal * tax_rate -> round(-2))
printf(fmt, '', '', 'Tax ', tax)

var total = subtotal+tax
printf(fmt, '', '', 'Total ', total)
```

```txt

Item          Price           Quantity              Extension
Hamburger      5.50   4000000000000000      22000000000000000
Milkshake      2.86                  2                   5.72
                                            -----------------
                             Subtotal    22000000000000005.72
                                  Tax     1683000000000000.44
                                Total    23683000000000006.16

```




## Swift



```swift
import Foundation

extension Decimal {
  func rounded(_ scale: Int, _ roundingMode: NSDecimalNumber.RoundingMode) -> Decimal {
    var result = Decimal()
    var localCopy = self
    NSDecimalRound(&result, &localCopy, scale, roundingMode)
    return result
  }
}

let costHamburgers = Decimal(4000000000000000) * Decimal(5.50)
let costMilkshakes = Decimal(2) * Decimal(2.86)
let totalBeforeTax = costMilkshakes + costHamburgers
let taxesToBeCollected = (Decimal(string: "0.0765")! * totalBeforeTax).rounded(2, .bankers)

print("Price before tax: $\(totalBeforeTax)")
print("Total tax to be collected: $\(taxesToBeCollected)")
print("Total with taxes: $\(totalBeforeTax + taxesToBeCollected)")
```


```txt
Price before tax: $22000000000000005.72
Total tax to be collected: $1683000000000000.44
Total with taxes: $23683000000000006.16
```



## Tcl

```tcl
package require math::decimal
namespace import math::decimal::*

set hamburgerPrice [fromstr 5.50]
set milkshakePrice [fromstr 2.86]
set taxRate [/ [fromstr 7.65] [fromstr 100]]

set burgers 4000000000000000
set shakes 2
set net [+ [* [fromstr $burgers] $hamburgerPrice] [* [fromstr $shakes] $milkshakePrice]]
set tax [round_up [* $net $taxRate] 2]
set total [+ $net $tax]

puts "net=[tostr $net], tax=[tostr $tax], total=[tostr $total]"
```

 net=22000000000000005.72, tax=1683000000000000.44, total=23683000000000006.16


## VBA

Used in locality Euroland. Formatting as currency shows € in stead of $, and thousand separators are "." in stead of "," and the decimail 'point' is "," in stead of "." When run in the USA it will be with dollar sign and so on.

```vb
Public Sub currency_task()
    '4000000000000000 hamburgers at $5.50 each
    Dim number_of_hamburgers As Variant
    number_of_hamburgers = CDec(4E+15)
    Dim price_of_hamburgers As Currency
    price_of_hamburgers = 5.5
    '2 milkshakes at $2.86 each, and
    Dim number_of_milkshakes As Integer
    number_of_milkshakes = 2
    Dim price_of_milkshakes As Currency
    price_of_milkshakes = 2.86
    'a tax rate of 7.65%.
    Dim tax_rate As Single
    tax_rate = 0.0765
    'the total price before tax
    Dim total_price_before_tax As Variant
    total_price_before_tax = number_of_hamburgers * price_of_hamburgers
    total_price_before_tax = total_price_before_tax + number_of_milkshakes * price_of_milkshakes
    Debug.Print "Total price before tax "; Format(total_price_before_tax, "Currency")
    'the tax
    Dim tax As Variant
    tax = total_price_before_tax * tax_rate
    Debug.Print "Tax "; Format(tax, "Currency")
    'the total with tax
    Debug.Print "Total with tax "; Format(total_price_before_tax + tax, "Currency")
End Sub
```
```txt
Total price before tax € 22.000.000.000.000.005,72
Tax € 1.683.000.000.000.000,44
Total with tax € 23.683.000.000.000.006,16
```


## zkl

zkl Ints are 64 bits, so we have 18 digits to play with.
So, just multiply bucks by 100 and be careful when doing tax calculations.
```zkl
var priceList=Dictionary("hamburger",550, "milkshake",286);
var taxRate=765;  // percent*M
const M=0d10_000;

fcn toBucks(n){ "$%,d.%02d".fmt(n.divr(100).xplode()) }
fcn taxIt(n)  { d,c:=n.divr(M).apply('*(taxRate)); d + (c+5000)/M; }
fcn calcTab(items){ // (hamburger,15), (milkshake,100) ...
   items=vm.arglist;
   fmt:="%-10s %8s %18s %26s";
   fmt.fmt("Item Price Quantity Extension".split().xplode()).println();

   totalBeforeTax:=0;
   foreach item,n in (items.sort(fcn(a,b){ a[0]<b[0] })){
      price:=priceList[item]; t:=price*n;
      fmt.fmt(item,toBucks(price),n,toBucks(t)).println();
      totalBeforeTax+=t;
   }
   fmt.fmt("","","","--------------------").println();
   fmt.fmt("","","subtotal",toBucks(totalBeforeTax)).println();

   tax:=taxIt(totalBeforeTax);
   fmt.fmt("","","Tax",toBucks(tax)).println();

   fmt.fmt("","","","--------------------").println();
   fmt.fmt("","","Total",toBucks(totalBeforeTax + tax)).println();
}
```


```zkl
calcTab(T("milkshake",2),T("hamburger",4000000000000000));
```

```txt

Item          Price           Quantity                  Extension
hamburger     $5.50   4000000000000000 $22,000,000,000,000,000.00
milkshake     $2.86                  2                      $5.72
                                             --------------------
                              subtotal $22,000,000,000,000,005.72
                                   Tax  $1,683,000,000,000,000.44
                                             --------------------
                                 Total $23,683,000,000,000,006.16

```

