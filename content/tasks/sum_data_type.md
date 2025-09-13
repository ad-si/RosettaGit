+++
title = "Sum data type"
description = ""
date = 2019-09-08T10:40:03Z
aliases = []
[extra]
id = 22393
[taxonomies]
categories = ["task"]
tags = []
+++

## Task

Create a sum data type:

A sum data type is a data structure used to hold a value that could take on several different, but fixed, types.
Only one of the types can be in use at any one time.

Sum data types are considered an [[wp:Algebraic_data_type|algebraic data type]] and are also known as tagged union, variant, variant record, choice type, discriminated union, disjoint union or coproduct.

## Related tasks

* [[Compound data type]]

## ALGOL 68

Algol 68's UNION MODE allows the definition of items which can have different types.

```algol68
MODE LEAF = INT;
MODE NODE = STRUCT( TREE left, TREE right );
MODE TREE = UNION( VOID, LEAF, REF NODE );

TREE t1   = LOC NODE := ( LEAF( 1 ), LOC NODE := ( LEAF( 2 ), LEAF( 3 ) ) );
```


Note that assignment/initialisation of UNION items is just of a matter of specifying the assigned/initial value, as above;
however to use the value requires a CASE clause, such as in the example below (which would print "node", given the above declarations).


```algol68
CASE t1
  IN (REF NODE n): print( ( "node",     newline ) )
   , (    LEAF l): print( ( "leaf ", l, newline ) )
   , (    VOID  ): print( ( "empty",    newline ) )
ESAC
```



## C

C has the union data type which can store multiple variables at the same memory location. This was a very handy feature when memory was a scarce commodity. Even now this is an essential feature which enables low level access such as hardware I/O access, word or bitfield sharing thus making C especially suited for Systems Programming.

What follows are two example programs. In both an union stores an integer, a floating point and a character at the same location. If all values are initialized at once, data is corrupted, as shown in the first example. Proper use of unions require storing and retrieving data only when required.


### Incorrect usage


```C

#include<stdio.h>

typedef union data{
        int i;
        float f;
        char c;
}united;

int main()
{
        united udat;

        udat.i = 5;
        udat.f = 3.14159;
        udat.c = 'C';

        printf("Integer   i = %d , address of i = %p\n",udat.i,&udat.i);
        printf("Float     f = %f , address of f = %p\n",udat.f,&udat.f);
        printf("Character c = %c , address of c = %p\n",udat.c,&udat.c);

        return 0;
}

```

'''Output :'''

```txt

Integer   i = 1078529859 , address of i = 0x7ffc475e3c64
Float     f = 3.141557 , address of f = 0x7ffc475e3c64
Character c = C , address of c = 0x7ffc475e3c64

```



### Correct usage


```C

#include<stdio.h>

typedef union data{
        int i;
        float f;
        char c;
}united;

int main()
{
        united udat;

        udat.i = 5;

        printf("Integer   i = %d , address of i = %p\n",udat.i,&udat.i);

        udat.f = 3.14159;

        printf("Float     f = %f , address of f = %p\n",udat.f,&udat.f);

        udat.c = 'C';

        printf("Character c = %c , address of c = %p\n",udat.c,&udat.c);

        return 0;
}

```

'''Output:'''

```txt

Integer   i = 5 , address of i = 0x7ffd71122354
Float     f = 3.14159 , address of f = 0x7ffd71122354
Character c = C , address of c = 0x7ffd71122354

```



## Delphi

''See [[#Pascal|Pascal]]''


## Factor

This is accomplished by defining a tuple with only one slot. The slot should have a class declaration that is a union class. This ensures that the slot may only contain an object of a class that is in the union. A convenient way to do this is with an anonymous union, as in the example below. An explicit <code>UNION:</code> definition may also be used.

In the example below, we define a <code>pseudo-number</code> tuple with one slot that can hold either a <code>number</code> (a built-in class) or a <code>numeric-string</code> — a class which we have defined to be any string that can parse as a number using the <code>string>number</code> word.

```factor
USING: accessors kernel math math.parser strings ;

PREDICATE: numeric-string < string string>number >boolean ;
TUPLE: pseudo-number { value union{ number numeric-string } } ;
C: <pseudo-number> pseudo-number   ! constructor

5.245 <pseudo-number>   ! ok
"-17"   >>value         ! ok
"abc42" >>value         ! error
```



## Free Pascal

See [[#Pascal|Pascal]].
The type <tt>variant</tt> is implemented as a variant record.


## Go

Go doesn't natively support sum types, though it's not difficult to create one (albeit verbosely) as the following example shows.

Normally, the IPAddr type (and associated types/methods) would be placed in a separate package so its 'v' field couldn't be accessed directly by code outside that package. However here, for convenience, we place it in the 'main' package.

```go
package main

import (
    "errors"
    "fmt"
)

type (
    IpAddr struct{ v interface{} }
    Ipv4   = [4]uint8
    Ipv6   = string
)

var zero = Ipv4{}

func NewIpAddr(v interface{}) (*IpAddr, error) {
    switch v.(type) {
    case Ipv4, Ipv6:
        return &IpAddr{v}, nil
    default:
        err := errors.New("Type of value must either be Ipv4 or Ipv6.")
        return nil, err
    }
}

func (ip *IpAddr) V4() (Ipv4, error) {
    switch ip.v.(type) {
    case Ipv4:
        return ip.v.(Ipv4), nil
    default:
        err := errors.New("IpAddr instance doesn't currently hold an Ipv4.")
        return zero, err
    }
}

func (ip *IpAddr) SetV4(v Ipv4) {
    ip.v = v
}

func (ip *IpAddr) V6() (Ipv6, error) {
    switch ip.v.(type) {
    case Ipv6:
        return ip.v.(Ipv6), nil
    default:
        err := errors.New("IpAddr instance doesn't currently hold an Ipv6.")
        return "", err
    }
}

func (ip *IpAddr) SetV6(v Ipv6) {
    ip.v = v
}

func check(err error) {
    if err != nil {
        fmt.Println(err)
    }
}

func main() {
    v4 := Ipv4{127, 0, 0, 1}
    ip, _ := NewIpAddr(v4)
    home, _ := ip.V4()
    fmt.Println(home)
    v6 := "::1"
    ip.SetV6(v6)
    loopback, _ := ip.V6()
    fmt.Println(loopback)
    _, err := ip.V4()
    check(err)
    rubbish := 6
    ip, err = NewIpAddr(rubbish)
    check(err)
}
```


```txt

[127 0 0 1]
::1
IpAddr instance doesn't currently hold an Ipv4.
Type of value must either be Ipv4 or Ipv6.

```



## Julia

Julia allows the creation of union types.

```Julia
    
    julia> using Sockets # for IP types

    julia> MyUnion = Union{Int64, String, Float64, IPv4, IPv6}
    Union{Float64, Int64, IPv4, IPv6, String}

    julia> arr = MyUnion[2, 4.8, ip"192.168.0.0", ip"::c01e:fc9a", "Hello"]
    5-element Array{Union{Float64, Int64, IPv4, IPv6, String},1}:
     2
     4.8
      ip"192.168.0.0"
      ip"::c01e:fc9a"
      "Hello"


```



## OCaml


```ocaml
type tree = Empty
          | Leaf of int
          | Node of tree * tree

let t1 = Node (Leaf 1, Node (Leaf 2, Leaf 3))
```



## Pascal


```pascal
type
	someOrdinalType = boolean;
	sumDataType = record
			case tag: someOrdinalType of
				false: (
					number: integer;
				);
				true: (
					character: char;
				);
		end;
```

Naming a tag can be omitted, but then introspection, i. e. retrieving which alternative is “active”, can not be done.
A <tt>record</tt> can have at most one variant part, which has to appear next to the ''end'' of the <tt>record</tt> definition.


## Perl 6

Perl 6 doesn't really have Sum Types as a formal data structure but they can be emulated with enums and switches or multi-dispatch. Note that in this case, feeding the dispatcher an incorrect value results in a hard fault; it doesn't just dispatch to the default. Of course those rules can be relaxed or made more restrictive depending on your particular use case.


```perl6
enum Traffic-Signal < Red Yellow Green Blue >;

sub message (Traffic-Signal $light) {
    with $light {
        when Red    { 'Stop!'                                       }
        when Yellow { 'Speed Up!'                                   }
        when Green  { 'Go! Go! Go!'                                 }
        when Blue   { 'Wait a minute, How did we end up in Japan?!' }
        default     { 'Whut?'                                       }
    }
}

my \Pink = 'A Happy Balloon';


for Red, Green, Blue, Pink -> $signal {
    say message $signal;
}
```
 
```txt
Stop!
Go! Go! Go!
Wait a minute, How did we end up in Japan?!
Type check failed in binding to parameter '$light'; expected Traffic-Signal but got Str ("A Happy Balloon")
```



## Phix

Phix has the object type, which can hold an integer, float, string, (nested) sequence, or anything else you can think of.

User defined types can be used to enforce restrictions on the contents of variables

```Phix
type ipv4(object o)
    if not sequence(o) or length(o)!=4 then
        return false
    end if
    for i=1 to 4 do
        if not integer(o[i]) then
            return false
        end if
    end for
    return true
end type

type ipv6(object o)
    return string(o)
end type

type ipaddr(object o)
    return ipv4(o) or ipv6(o)
end type

ipaddr x
x = {127,0,0,1}  -- fine
x = "::c01e:fc9a"  -- fine
x = -1  -- error
```



## REXX

The '''REXX''' language is untyped,   it is up to the program to decide if it's valid and
how to deal with an invalid structure.

```rexx
/*REXX pgm snipette validates a specific type of data structure, an IP v4 address (list)*/
ip= 127 0 0 1
if val_ipv4(ip)  then say                'valid IPV4 type: '    ip
                 else say '***error***  invalid IPV4 type: '    ip
...

exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
val_ipv4: procedure; parse arg $;          if words($)\==4  |  arg()\==1  then return 0
            do j=1  for 4;   _=word($, j);    #=datatype(_, 'W');    L= length(_)
            if verify(_, 0123456789)\==0  |  \#  | _<0  |  _>255  |  L>3  then return 0
            end   /*j*/
          return 1                               /*returns true (1) if valid, 0 if not. */
```




## Rust



```rust
enum IpAddr {
    V4(u8, u8, u8, u8),
    V6(String),
}

let home = IpAddr::V4(127, 0, 0, 1);

let loopback = IpAddr::V6(String::from("::1"));
```



## Scala

{{Out}}See it yourself by running in your browser either by [https://scalafiddle.io/sf/encsuyJ/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/3U8mEeYqTliyKn6ikYnFAg Scastie (remote JVM)].
```Scala
case class Envelop[T](member: T)

val list = List(
  Envelop("a string"),
  Envelop(732), // an integer
  Envelop('☺'), // a character
  Envelop(true) // a boolean value
)

list.foreach { case Envelop(element) => println(element) }
```



## zkl

zkl is untyped - it is up to the container to decide if it wants to deal with a type or not.

```zkl
ip:=List(127,0,0,1);
addrs:=Dictionary("ip",ip);
```


```zkl
class Addr{
   fcn init(addr){
      var ip = addr;
      if(not List.isType(addr)) throw(Exception.TypeError);
   }
}
ip:=Addr(List(127,0,0,1));
Addr(127,0,0,1);	// TypeError : Invalid type
Addr(List("abc"));	// doesn't fail, would need more error checking
ip.ip=L(192,168,1,1);	// this doesn't type check either
```

