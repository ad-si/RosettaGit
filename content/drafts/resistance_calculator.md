+++
title = "Resistance Calculator"
description = ""
date = 2019-07-03T19:11:53Z
aliases = []
[extra]
id = 22219
[taxonomies]
categories = []
tags = []
+++

{{draft task}}

;Introduction
* Calculate the resistance of a network of resistors. 
* The resistors can be connected in series or parallel. 
* Use infix or RPN to state the network.
* Calculate resistance, voltage, current and power for every resistor and operation.

;Background
* Serial Resistors: the sum of the resistors gives the equivalent resistor
* Parallel Resistors: the inverse of the sum of the inverse of the resistors
* The voltage drops over the resistors
* Current = Resistance / Voltage
* Power = Current * Voltage

;Input
[https://photos.app.goo.gl/58heQVm8UJYf8Ra29 Resistance Calculator]

* Infix:  ((((10 + 2) * 6 + 8) * 6 + 4) * 8 + 4) * 8 + 6
* RPN:    10 2 + 6 * 8 + 6 * 4 + 8 * 4 + 8 * 6 +  
* Voltage = 18.0 V
  
;Output
* 10.000 ohms in the upper left corner is the equivalent resistance.
* The first operation is 10 + 2 = 12 which can be found in the three middle rows.
     Ohm     Volt   Ampere     Watt  Network tree
  10.000   18.000    1.800   32.400  +
   4.000    7.200    1.800   12.960  | *
   8.000    7.200    0.900    6.480  | | +
   4.000    3.600    0.900    3.240  | | | *
   8.000    3.600    0.450    1.620  | | | | +
   4.000    1.800    0.450    0.810  | | | | | *
  12.000    1.800    0.150    0.270  | | | | | | +
   4.000    0.600    0.150    0.090  | | | | | | | *
  12.000    0.600    0.050    0.030  | | | | | | | | +
  10.000    0.500    0.050    0.025  | | | | | | | | | r
   2.000    0.100    0.050    0.005  | | | | | | | | | r
   6.000    0.600    0.100    0.060  | | | | | | | | r
   8.000    1.200    0.150    0.180  | | | | | | | r
   6.000    1.800    0.300    0.540  | | | | | | r
   4.000    1.800    0.450    0.810  | | | | | r
   8.000    3.600    0.450    1.620  | | | | r
   4.000    3.600    0.900    3.240  | | | r
   8.000    7.200    0.900    6.480  | | r
   6.000   10.800    1.800   19.440  | r


## CoffeeScript


### RPN


```coffeescript
nd = (num) -> num.toFixed(3).padStart 8

class Resistor
	constructor : (@resistance,@a=null,@b=null,@symbol='r') -> 
	res : -> @resistance
	setVoltage : (@voltage) ->
	current : -> @voltage / @res()
	effect : -> @current() * @voltage
	report : (level) ->
		print "#{nd @res()} #{nd @voltage} #{nd @current()} #{nd @effect()}  #{level}#{@symbol}"
		if @a then @a.report level + "| "
		if @b then @b.report level + "| "

class Serial extends Resistor
	constructor : (a,b) -> super 0,a,b,'+'
	res : -> @a.res() + @b.res()
	setVoltage : (@voltage) ->
		ra = @a.res()
		rb = @b.res()
		@a.setVoltage ra/(ra+rb) * @voltage
		@b.setVoltage rb/(ra+rb) * @voltage

class Parallel extends Resistor
	constructor : (a,b) -> super 0,a,b,'*'
	res : -> 1 / (1 / @a.res() + 1 / @b.res())
	setVoltage : (@voltage) ->
		@a.setVoltage @voltage
		@b.setVoltage @voltage

build = (s) ->
	stack = []
	for word in s.split ' '
		if      word == '+' then stack.push new Serial stack.pop(), stack.pop()
		else if word == '*' then stack.push new Parallel stack.pop(), stack.pop()
		else                     stack.push new Resistor parseFloat word
	stack.pop()

node = build "10 2 + 6 * 8 + 6 * 4 + 8 * 4 + 8 * 6 +"
node.setVoltage 18.0
print "     Ohm     Volt   Ampere     Watt  Network tree"
node.report ""
```



## Go


### Infix

{{trans|Nim}}

```go
package main

import "fmt"

type Resistor struct {
    symbol              rune
    resistance, voltage float64
    a, b                *Resistor
}

func (r *Resistor) res() float64 {
    switch r.symbol {
    case '+':
        return r.a.res() + r.b.res()
    case '*':
        return 1 / (1/r.a.res() + 1/r.b.res())
    default:
        return r.resistance
    }
}

func (r *Resistor) setVoltage(voltage float64) {
    switch r.symbol {
    case '+':
        ra := r.a.res()
        rb := r.b.res()
        r.a.setVoltage(ra / (ra + rb) * voltage)
        r.b.setVoltage(rb / (ra + rb) * voltage)
    case '*':
        r.a.setVoltage(voltage)
        r.b.setVoltage(voltage)
    }
    r.voltage = voltage
}

func (r *Resistor) current() float64 {
    return r.voltage / r.res()
}

func (r *Resistor) effect() float64 {
    return r.current() * r.voltage
}

func (r *Resistor) report(level string) {
    fmt.Printf("%8.3f %8.3f %8.3f %8.3f  %s%c\n", r.res(), r.voltage, r.current(), r.effect(), level, r.symbol)
    if r.a != nil {
        r.a.report(level + "| ")
    }
    if r.b != nil {
        r.b.report(level + "| ")
    }
}

func (r *Resistor) add(other *Resistor) *Resistor {
    return &Resistor{'+', 0, 0, r, other}
}

func (r *Resistor) mul(other *Resistor) *Resistor {
    return &Resistor{'*', 0, 0, r, other}
}

func main() {
    var r [10]*Resistor
    resistances := []float64{6, 8, 4, 8, 4, 6, 8, 10, 6, 2}
    for i := 0; i < 10; i++ {
        r[i] = &Resistor{'r', resistances[i], 0, nil, nil}
    }
    node := r[7].add(r[9]).mul(r[8]).add(r[6]).mul(r[5]).add(r[4]).mul(r[3]).add(r[2]).mul(r[1]).add(r[0])
    node.setVoltage(18)
    fmt.Println("     Ohm     Volt   Ampere     Watt  Network tree")
    node.report("")
}
```



### RPN


```go
package main

import (
    "fmt"
    "strconv"
    "strings"
)

type Stack []*Resistor

func (s *Stack) push(r *Resistor) {
    *s = append(*s, r)
}

func (s *Stack) pop() *Resistor {
    le := len(*s)
    if le == 0 {
        panic("Attempt to pop from an empty stack")
    }
    le--
    r := (*s)[le]
    *s = (*s)[:le]
    return r
}

type Resistor struct {
    symbol              rune
    resistance, voltage float64
    a, b                *Resistor
}

func (r *Resistor) res() float64 {
    switch r.symbol {
    case '+':
        return r.a.res() + r.b.res()
    case '*':
        return 1 / (1/r.a.res() + 1/r.b.res())
    default:
        return r.resistance
    }
}

func (r *Resistor) setVoltage(voltage float64) {
    switch r.symbol {
    case '+':
        ra := r.a.res()
        rb := r.b.res()
        r.a.setVoltage(ra / (ra + rb) * voltage)
        r.b.setVoltage(rb / (ra + rb) * voltage)
    case '*':
        r.a.setVoltage(voltage)
        r.b.setVoltage(voltage)
    }
    r.voltage = voltage
}

func (r *Resistor) current() float64 {
    return r.voltage / r.res()
}

func (r *Resistor) effect() float64 {
    return r.current() * r.voltage
}

func (r *Resistor) report(level string) {
    fmt.Printf("%8.3f %8.3f %8.3f %8.3f  %s%c\n", r.res(), r.voltage, r.current(), r.effect(), level, r.symbol)
    if r.a != nil {
        r.a.report(level + "| ")
    }
    if r.b != nil {
        r.b.report(level + "| ")
    }
}

func build(rpn string) *Resistor {
    st := new(Stack)
    for _, token := range strings.Fields(rpn) {
        switch token {
        case "+":
            b, a := st.pop(), st.pop()
            st.push(&Resistor{'+', 0, 0, a, b})
        case "*":
            b, a := st.pop(), st.pop()
            st.push(&Resistor{'*', 0, 0, a, b})
        default:
            r, _ := strconv.ParseFloat(token, 64)
            st.push(&Resistor{'r', r, 0, nil, nil})
        }
    }
    return st.pop()
}

func main() {
    node := build("10 2 + 6 * 8 + 6 * 4 + 8 * 4 + 8 * 6 +")
    node.setVoltage(18)
    fmt.Println("     Ohm     Volt   Ampere     Watt  Network tree")
    node.report("")
}
```



## Nim


```python
import tables,strutils,sequtils,sugar,strformat

type
	Node = ref object
		kind : char  #  + = serial  * = parallel  r = resistor
		resistance : float
		voltage : float
		a : Node
		b : Node

proc res(node : Node) : float =
	if node.kind == '+' : return node.a.res + node.b.res
	if node.kind == '*' : return 1/(1/node.a.res + 1/node.b.res)
	node.resistance

proc current(node : Node) : float = return node.voltage / node.res
proc effect (node : Node) : float = return node.current * node.voltage

proc report(node : Node, level : string = "") =
	echo fmt"{node.res:8.3f} {node.voltage:8.3f} {node.current:8.3f} {node.effect:8.3f}  {level}{node.kind}"
	if node.kind in "+*":
		node.a.report level & "| "
		node.b.report level & "| "

proc setVoltage(node : Node, voltage : float) =
	node.voltage = voltage
	if node.kind == '+':
		let ra = node.a.res
		let rb = node.b.res
		node.a.setVoltage ra/(ra+rb) * voltage
		node.b.setVoltage rb/(ra+rb) * voltage
	if node.kind == '*': 
		node.a.setVoltage voltage
		node.b.setVoltage voltage

proc build(tokens : seq[string]) : Node =
	var stack : seq[Node]
	for token in tokens:
		if   token == "+": stack.add Node(kind : '+', a : stack.pop, b : stack.pop)
		elif token == "*": stack.add Node(kind : '*', a : stack.pop, b : stack.pop)
		else: 	      	   stack.add Node(kind : 'r', resistance : parseFloat(token))
	stack.pop

proc calculate(voltage:float, tokens:seq[string]): Node = 
	echo ""
	echo "     Ohm     Volt   Ampere     Watt  Network tree"
	let node = build tokens
	node.setVoltage voltage
	node.report
	node
```



### RPN


```python
proc rpn(voltage:float, s:string): Node = calculate(voltage, s.split ' ')
var node = rpn 18.0,"10 2 + 6 * 8 + 6 * 4 + 8 * 4 + 8 * 6 +"
assert 10 == node.res
assert 18 == node.voltage
assert 1.8 == node.current()
assert 32.4 == node.effect()
assert '+' == node.kind
```



### Infix


```python
proc parse(s: string): seq[string] =
	var tmp = ""
	for ch in s:
		if ch == ' ': 
			if tmp!="": result.add tmp
			tmp = ""
			continue
		if ch in "+*()": 
			if tmp!="": result.add tmp
			tmp=""
			result.add fmt"{ch}"
		else: tmp &= ch
	if tmp!="": result.add tmp

proc shuntRPN(s:string): seq[string] =
	let ops = "+*" 
	var tokens = parse s
	var stack: seq[string]
	var op: string
 
	for token in tokens:
		case token
		of "(": stack.add token
		of ")":
			while stack.len > 0:
				op = stack.pop()
				if op == "(": break
				result.add op
		else:
			if token in ops:
				while stack.len > 0:
					op = stack[^1]
					if not (op in ops): break
					if ops.find(token) >= ops.find(op): break
					discard stack.pop()
					result.add op
				stack.add token
			else: result.add token
 
	while stack.len > 0: result.add stack.pop()

proc infix(voltage:float, s:string): Node = calculate(voltage, shuntRPN s)
node = infix 18.0,"((((10+2)*6+8)*6+4)*8+4)*8+6"
assert 10 == node.res
assert 18 == node.voltage
assert 1.8 == node.current()
assert 32.4 == node.effect()
assert '+' == node.kind
```



## Perl


### Infix

{{trans|Perl 6}}

```perl
use strict;
use warnings;
use feature <say state>;

{
package Resistor;

sub new {
    my ($class, $args) = @_;
    my $self = {
        symbol     => $args->{symbol},
        voltage    => $args->{voltage},
        resistance => $args->{resistance},
        a          => $args->{a},
        b          => $args->{b},
    };
    return bless $self, $class;
}

sub res {
    my $self = shift;
    if    ($self->{symbol} eq '+') { return res($self->{a}) + res($self->{b}) }
    elsif ($self->{symbol} eq '*') { return 1 / (1/res($self->{a}) + 1/res($self->{b})) }
    else  { return $self->{resistance} }
}

sub set_voltage {
    my($self,$voltage) = @_;
    if ($self->{symbol} eq '+') {
        my $ra = res($self->{a});
        my $rb = res($self->{b});
        set_voltage($self->{a}, $ra / ($ra+$rb) * $voltage );
        set_voltage($self->{b}, $rb / ($ra+$rb) * $voltage );
    } elsif ($self->{symbol} eq '*') {
        set_voltage($self->{a}, $voltage );
        set_voltage($self->{b}, $voltage );
    }
    $self->{voltage} = $voltage;
}

sub current { my $self = shift; return $self->{voltage} / res($self)     }
sub effect  { my $self = shift; return $self->{voltage} * current($self) }

use overload '+' => \&serial,
             '*' => \&parallel;

sub serial   { my($a,$b) = @_; Resistor->new( {symbol => '+', a => $a, b => $b} ) }
sub parallel { my($a,$b) = @_; Resistor->new( {symbol => '*', a => $a, b => $b} ) }

sub report {
    my($self,$level)  = @_;
    state @results;
    push @results, '      Ohm     Volt   Ampere     Watt   Network tree' and $level = 1 unless $level;
    my $pad = ('| ') x $level;
    my $f = sprintf '%9.3f' x 4, res($self), $self->{voltage}, current($self), effect($self);
    say "$f $pad" . $self->{symbol};
    report($self->{a}, $level+1) if defined $self->{a};
    report($self->{b}, $level+1) if defined $self->{b};
    join "\n", @results;
}

}

package main;

my ($R1, $R2, $R3, $R4, $R5, $R6, $R7, $R8, $R9, $R10) =
    map { Resistor->new( {symbol => 'r', resistance => $_} ) } <6 8 4 8 4 6 8 10 6 2>;

my $node = (((($R8 + $R10) * $R9 + $R7) * $R6 + $R5)
                           * $R4 + $R3) * $R2 + $R1;

Resistor::set_voltage($node,18);
say Resistor::report($node);
```

{{out}}
<pre style="height:20ex">      Ohm     Volt   Ampere     Watt   Network tree
   10.000   18.000    1.800   32.400 | +
    4.000    7.200    1.800   12.960 | | *
    8.000    7.200    0.900    6.480 | | | +
    4.000    3.600    0.900    3.240 | | | | *
    8.000    3.600    0.450    1.620 | | | | | +
    4.000    1.800    0.450    0.810 | | | | | | *
   12.000    1.800    0.150    0.270 | | | | | | | +
    4.000    0.600    0.150    0.090 | | | | | | | | *
   12.000    0.600    0.050    0.030 | | | | | | | | | +
   10.000    0.500    0.050    0.025 | | | | | | | | | | r
    2.000    0.100    0.050    0.005 | | | | | | | | | | r
    6.000    0.600    0.100    0.060 | | | | | | | | | r
    8.000    1.200    0.150    0.180 | | | | | | | | r
    6.000    1.800    0.300    0.540 | | | | | | | r
    4.000    1.800    0.450    0.810 | | | | | | r
    8.000    3.600    0.450    1.620 | | | | | r
    4.000    3.600    0.900    3.240 | | | | r
    8.000    7.200    0.900    6.480 | | | r
    6.000   10.800    1.800   19.440 | | r
```



## Perl 6


### Infix

{{trans|Nim}}

```perl6
class Resistor {
    has Str        $.symbol;
    has Numeric  ( $.voltage, $.resistance );
    has Resistor ( $.a, $.b );

    method res ( ) {
        given $.symbol {
            when '+' { return $.a.res + $.b.res }
            when '*' { return 1 / (1 / $.a.res  +  1 / $.b.res) }
            default  { return $.resistance }
        }
    }

    method set-voltage ( Numeric $voltage ) {
        given $.symbol {
            when '+' {
                my $ra = $.a.res;
                my $rb = $.b.res;
                $.a.set-voltage( $ra / ($ra+$rb) * $voltage );
                $.b.set-voltage( $rb / ($ra+$rb) * $voltage );
            }
            when '*' {
                $.a.set-voltage( $voltage );
                $.b.set-voltage( $voltage );
            }
        }
        $!voltage = $voltage;
    }
    method current ( ) { return $.voltage / self.res     }
    method effect  ( ) { return $.voltage * self.current }

    method report ( Int $level = 1 ) {
        my $pad = '| ' x $level;
        my $f = ( self.res, $.voltage, self.current, self.effect ).fmt('%8.3f');
        say "$f $pad$.symbol";
        $.a.report( $level+1 ) if $.a;
        $.b.report( $level+1 ) if $.b;
    }
}
multi sub infix:<+> (Resistor $a, Resistor $b) { $a.new( symbol => '+', :$a, :$b ) }
multi sub infix:<*> (Resistor $a, Resistor $b) { $a.new( symbol => '*', :$a, :$b ) }

my Resistor ($R1, $R2, $R3, $R4, $R5, $R6, $R7, $R8, $R9, $R10) =
    map { Resistor.new: symbol => 'r', resistance => $_ },
    6, 8, 4, 8, 4, 6, 8, 10, 6, 2;

my $node = (((($R8 + $R10) * $R9 + $R7) * $R6 + $R5)
                           * $R4 + $R3) * $R2 + $R1;
$node.set-voltage(18);

say '     Ohm     Volt   Ampere     Watt  Network tree';
$node.report;
```

{{out}}
<pre style="height:20ex">     Ohm     Volt   Ampere     Watt  Network tree
  10.000   18.000    1.800   32.400 | +
   4.000    7.200    1.800   12.960 | | *
   8.000    7.200    0.900    6.480 | | | +
   4.000    3.600    0.900    3.240 | | | | *
   8.000    3.600    0.450    1.620 | | | | | +
   4.000    1.800    0.450    0.810 | | | | | | *
  12.000    1.800    0.150    0.270 | | | | | | | +
   4.000    0.600    0.150    0.090 | | | | | | | | *
  12.000    0.600    0.050    0.030 | | | | | | | | | +
  10.000    0.500    0.050    0.025 | | | | | | | | | | r
   2.000    0.100    0.050    0.005 | | | | | | | | | | r
   6.000    0.600    0.100    0.060 | | | | | | | | | r
   8.000    1.200    0.150    0.180 | | | | | | | | r
   6.000    1.800    0.300    0.540 | | | | | | | r
   4.000    1.800    0.450    0.810 | | | | | | r
   8.000    3.600    0.450    1.620 | | | | | r
   4.000    3.600    0.900    3.240 | | | | r
   8.000    7.200    0.900    6.480 | | | r
   6.000   10.800    1.800   19.440 | | r
```



## Phix


```Phix
-- node contents:
enum KIND, -- '+', '*', or 'r'
     RESISTANCE, VOLTAGE, 
     A, B   -- nested nodes or NULL
 
function resistance(sequence node)
    switch node[KIND] do
        case '+': return resistance(node[A]) + resistance(node[B])
        case '*': return 1 / (1/resistance(node[A]) + 1/resistance(node[B]))
        case 'r': return node[RESISTANCE]
        default: ?9/0 -- unknown node kind
    end switch
end function
 
function setVoltage(sequence node, atom voltage)
    switch node[KIND] do
        case '+':
            atom ra := resistance(node[A]),
                 rb := resistance(node[B])
            node[A] = setVoltage(node[A], ra / (ra + rb) * voltage)
            node[B] = setVoltage(node[B], rb / (ra + rb) * voltage)
        case '*':
            node[A] = setVoltage(node[A],voltage)
            node[B] = setVoltage(node[B],voltage)
    end switch
    node[VOLTAGE] = voltage
    return node
end function
 
function current(sequence node)
    return node[VOLTAGE] / resistance(node)
end function
 
function effect(sequence node)
    return current(node) * node[VOLTAGE]
end function
 
procedure report(sequence node, string level="")
    printf(1,"%8.3f %8.3f %8.3f %8.3f  %s%c\n", {resistance(node), node[VOLTAGE], current(node), effect(node), level, node[KIND]})
    if node[A]!=NULL then
        report(node[A],level & "| ")
    end if
    if node[B]!=NULL then
        report(node[B],level & "| ")
    end if
end procedure
 
function push(sequence stack, string tok)
    switch tok do
        case "+","*": sequence b = stack[$],
                               a = stack[$-1]
                        stack = stack[1..$-1]
                        stack[$] = {tok[1], 0, 0, a, b}
        default:      integer {{r}} = scanf(tok,"%d")
                        stack = append(stack,{'r', r, 0, NULL, NULL})
    end switch
    return stack
end function
```


### RPN


```Phix
function rpn(string s)
    sequence stack = {},
             tokens = split(s)
    for i=1 to length(tokens) do
        stack = push(stack,tokens[i])
    end for
    return stack[$]
end function
 
sequence node = rpn("10 2 + 6 * 8 + 6 * 4 + 8 * 4 + 8 * 6 +")
node = setVoltage(node,18)
printf(1,"     Ohm     Volt   Ampere     Watt  Network tree\n")
report(node,"")
```

{{out}}

```txt

     Ohm     Volt   Ampere     Watt  Network tree
  10.000   18.000    1.800   32.400  +
   4.000    7.200    1.800   12.960  | *
   8.000    7.200    0.900    6.480  | | +
   4.000    3.600    0.900    3.240  | | | *
   8.000    3.600    0.450    1.620  | | | | +
   4.000    1.800    0.450    0.810  | | | | | *
  12.000    1.800    0.150    0.270  | | | | | | +
   4.000    0.600    0.150    0.090  | | | | | | | *
  12.000    0.600    0.050    0.030  | | | | | | | | +
  10.000    0.500    0.050    0.025  | | | | | | | | | r
   2.000    0.100    0.050    0.005  | | | | | | | | | r
   6.000    0.600    0.100    0.060  | | | | | | | | r
   8.000    1.200    0.150    0.180  | | | | | | | r
   6.000    1.800    0.300    0.540  | | | | | | r
   4.000    1.800    0.450    0.810  | | | | | r
   8.000    3.600    0.450    1.620  | | | | r
   4.000    3.600    0.900    3.240  | | | r
   8.000    7.200    0.900    6.480  | | r
   6.000   10.800    1.800   19.440  | r

```


### infix

slightly trickier

```Phix
constant ops = {"+","*"}
function infix(string s)
    string lastnum = ""
    sequence tokens = {}
    for i=1 to length(s) do
        integer ch = s[i]
        if ch>='0' and ch<='9' then
            lastnum &= ch
        else
            if length(lastnum) then
                tokens = append(tokens,lastnum)
                lastnum = ""
            end if
            tokens = append(tokens,ch&"")
        end if
    end for
    if length(lastnum) then
        tokens = append(tokens,lastnum)
    end if
    sequence stack = {}, result = {}
    for i=1 to length(tokens) do
        string token = tokens[i], op
        switch token do
            case "(":   stack = append(stack,token)
            case ")":   while true do
                            op = stack[$]
                            stack = stack[1..$-1]
                            if op == "(" then break end if
                            result = push(result,op)
                        end while
            else:
                        integer tp = find(token,ops)
                        if tp then
                            while length(stack) do
                                op = stack[$]
                                integer sp = find(op,ops)
                                if not sp or tp>=sp then exit end if
                                stack = stack[1..$-1]
                                result = push(result,op)
                            end while
                            stack = append(stack,token)
                        else
                            result = push(result,token)
                        end if 
        end switch
    end for
    for i=length(stack) to 1 by -1 do
        result = push(result,stack[i])
    end for
    return result[1]
end function

sequence node = infix("((((10+2)*6+8)*6+4)*8+4)*8+6")
```

then as per last 3 lines of RPN, same output.


## Python


### RPN


```python
class Resistor :
	def __init__(self, resistance, a=None, b=None, symbol='r'):
		self.resistance = resistance
		self.a = a
		self.b = b
		self.symbol = symbol
	def res(self) : return self.resistance
	def setVoltage(self, voltage): self.voltage = voltage
	def current(self) : return self.voltage / self.res()
	def effect(self) : return self.current() * self.voltage
	def report(self,level=""):
		print(f"{self.res():8.3f} {self.voltage:8.3f} {self.current():8.3f} {self.effect():8.3f}  {level}{self.symbol}")
		if self.a: self.a.report(level + "| ")
		if self.b: self.b.report(level + "| ")

class Serial(Resistor) :
	def __init__(self, a, b) : super().__init__(0, b, a, '+')
	def res(self) : return self.a.res() + self.b.res()
	def setVoltage(self, voltage) :
		ra = self.a.res()
		rb = self.b.res()
		self.a.setVoltage(ra/(ra+rb) * voltage)
		self.b.setVoltage(rb/(ra+rb) * voltage)
		self.voltage = voltage

class Parallel(Resistor) :
	def __init__(self,a,b) : super().__init__(0, b, a, '*')
	def res(self) : return 1 / (1 / self.a.res() + 1 / self.b.res())
	def setVoltage(self, voltage) :
		self.a.setVoltage(voltage)
		self.b.setVoltage(voltage)
		self.voltage = voltage

def build(s) :
	stack = []
	for word in s.split(' '):
		if   word == "+": stack.append(Serial(stack.pop(), stack.pop()))
		elif word == "*": stack.append(Parallel(stack.pop(), stack.pop()))
		else:             stack.append(Resistor(float(word)))
	return stack.pop()

node = build("10 2 + 6 * 8 + 6 * 4 + 8 * 4 + 8 * 6 +")
print("     Ohm     Volt   Ampere     Watt  Network tree")
node.setVoltage(18.0)
node.report()
```



### Infix


```python
class Resistor :
	def __init__(self, resistance, a=None, b=None, symbol='r') :
		self.resistance = resistance
		self.a = a
		self.b = b
		self.symbol = symbol
	def res(self) : return self.resistance
	def setVoltage(self, voltage) : self.voltage = voltage
	def current(self) : return self.voltage / self.res()
	def effect(self) : return self.current() * self.voltage
	def report(self,level="") :
		print(f"{self.res():8.3f} {self.voltage:8.3f} {self.current():8.3f} {self.effect():8.3f}  {level}{self.symbol}")
		if self.a: self.a.report(level + "| ")
		if self.b: self.b.report(level + "| ")
	def __add__(self,other) : return Serial(self,other)
	def __mul__(self,other) : return Parallel(self,other)

class Serial(Resistor) :
	def __init__(self, a, b) : super().__init__(0, a, b, '+')
	def res(self) : return self.a.res() + self.b.res()
	def setVoltage(self, voltage) :
		ra = self.a.res()
		rb = self.b.res()
		self.a.setVoltage(ra/(ra+rb) * voltage)
		self.b.setVoltage(rb/(ra+rb) * voltage)
		self.voltage = voltage

class Parallel(Resistor) :
	def __init__(self,a,b) : super().__init__(0, a, b, '*')
	def res(self) : return 1 / (1 / self.a.res() + 1 / self.b.res())
	def setVoltage(self, voltage):
		self.a.setVoltage(voltage)
		self.b.setVoltage(voltage)
		self.voltage = voltage

[R1,R2,R3,R4,R5,R6,R7,R8,R9,R10] = [Resistor(res) for res in [6,8,4,8,4,6,8,10,6,2]]
node = ((((R8+R10) * R9 + R7) * R6 + R5) * R4 + R3) * R2 + R1
node.setVoltage(18)
print("     Ohm     Volt   Ampere     Watt  Network tree")
node.report()
```



## zkl


```zkl
class Resistor{
   fcn init(resistance_,symbol_="r", a_=Void, b_=Void){
      var resistance,a,b,symbol, voltage=Void;
      resistance,symbol,a,b = vm.arglist;
      resistance=resistance.toFloat();  // deal with strings/ints
   }
   fcn res{ 
      if     (symbol=="+") a.res() + b.res();
      else if(symbol=="*") 1.0/(1.0/a.res() + 1.0/b.res());
      else                 resistance
   }
   fcn setVoltage(voltage){ 
      if(symbol=="+"){
         ra,rb := a.res(), b.res();
	 a.setVoltage(ra/(ra + rb)*voltage);
	 b.setVoltage(rb/(ra + rb)*voltage);
      }
      else if(symbol=="*") T(a,b).apply2("setVoltage",voltage);
      self.voltage = voltage.toFloat();
   }
   fcn current{ voltage/res()     }
   fcn effect { current()*voltage }
   fcn report(level=""){
      println("%8.3f %8.3f %8.3f %8.3f  %s%s".fmt(res(),voltage,current(),effect(),level,symbol));
      T(a,b).apply2("report",level + "| ");  // noop if Void
   }
   fcn __opAdd(other){ Resistor(0,"+",self,other) }
   fcn __opMul(other){ Resistor(0,"*",self,other) }
}
```


### Infix


```zkl
R1,R2,R3,R4,R5,R6,R7,R8,R9,R10 := T(6,8,4,8,4,6,8,10,6,2].apply(Resistor);
node:=((((R8 + R10)*R9 + R7)*R6 + R5)*R4 + R3)*R2 + R1;
node.setVoltage(18);
println("     Ohm     Volt   Ampere     Watt  Network tree");
node.report();
```

{{out}}
<pre style="height:15ex">
     Ohm     Volt   Ampere     Watt  Network tree
  10.000   18.000    1.800   32.400  +
   4.000    7.200    1.800   12.960  | *
   8.000    7.200    0.900    6.480  | | +
   4.000    3.600    0.900    3.240  | | | *
   8.000    3.600    0.450    1.620  | | | | +
   4.000    1.800    0.450    0.810  | | | | | *
  12.000    1.800    0.150    0.270  | | | | | | +
   4.000    0.600    0.150    0.090  | | | | | | | *
  12.000    0.600    0.050    0.030  | | | | | | | | +
  10.000    0.500    0.050    0.025  | | | | | | | | | r
   2.000    0.100    0.050    0.005  | | | | | | | | | r
   6.000    0.600    0.100    0.060  | | | | | | | | r
   8.000    1.200    0.150    0.180  | | | | | | | r
   6.000    1.800    0.300    0.540  | | | | | | r
   4.000    1.800    0.450    0.810  | | | | | r
   8.000    3.600    0.450    1.620  | | | | r
   4.000    3.600    0.900    3.240  | | | r
   8.000    7.200    0.900    6.480  | | r
   6.000   10.800    1.800   19.440  | r

```


### RPN


```zkl
fcn build(rpnStr){
   stack:=List();
   foreach symbol in (rpnStr.split()){
      if(symbol=="+"){
         a,b:=stack.pop(),stack.pop();
	 stack.append(Resistor(0,"+",b,a))
      }
      else if(symbol=="*"){
         a,b:=stack.pop(),stack.pop();
      	 stack.append(Resistor(0,"*",b,a))
      }
      else stack.append(Resistor(symbol,"r")); 
   }
   stack.pop()	// unevaluated top of circuit
}
 
node:=build("10 2 + 6 * 8 + 6 * 4 + 8 * 4 + 8 * 6 +");
node.setVoltage(18);
println("     Ohm     Volt   Ampere     Watt  Network tree");
node.report();
```

{{out}}
<pre style="height:15ex">
     Ohm     Volt   Ampere     Watt  Network tree
  10.000   18.000    1.800   32.400  +
   4.000    7.200    1.800   12.960  | *
   8.000    7.200    0.900    6.480  | | +
   4.000    3.600    0.900    3.240  | | | *
   8.000    3.600    0.450    1.620  | | | | +
   4.000    1.800    0.450    0.810  | | | | | *
  12.000    1.800    0.150    0.270  | | | | | | +
   4.000    0.600    0.150    0.090  | | | | | | | *
  12.000    0.600    0.050    0.030  | | | | | | | | +
  10.000    0.500    0.050    0.025  | | | | | | | | | r
   2.000    0.100    0.050    0.005  | | | | | | | | | r
   6.000    0.600    0.100    0.060  | | | | | | | | r
   8.000    1.200    0.150    0.180  | | | | | | | r
   6.000    1.800    0.300    0.540  | | | | | | r
   4.000    1.800    0.450    0.810  | | | | | r
   8.000    3.600    0.450    1.620  | | | | r
   4.000    3.600    0.900    3.240  | | | r
   8.000    7.200    0.900    6.480  | | r
   6.000   10.800    1.800   19.440  | r

```

