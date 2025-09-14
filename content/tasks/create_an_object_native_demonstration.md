+++
title = "Create an object/Native demonstration"
description = ""
date = 2019-06-25T22:37:58Z
aliases = []
[extra]
id = 8372
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "d",
  "go",
  "j",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "m2000_interpreter",
  "perl",
  "perl_6",
  "phix",
  "python",
  "racket",
  "ring",
  "ruby",
  "scala",
  "tcl",
  "zkl",
]
+++

Create a Hash/Associative Array/Dictionary-like object, initialized with some default key/value pairs using the languages native method of object creation. The object should behave like a native Hash/Associative Array/Dictionary of the language, if any, but with the following differences:

* No new item can be added;
* Item cannot be deleted, (but native delete method may used to reset the item's value to default) ;

If the language supports '''Magic Methods''', then show how these work.


## D

```d
struct DefaultAA(TK, TV) {
    TV[TK] standard, current;

    this(TV[TK] default_) pure /*nothrow*/ @safe {
        this.standard = default_;
        this.current = default_.dup;
    }

    alias current this;

    void remove(in TK key) pure nothrow {
        current[key] = standard[key];
    }

    void clear() pure /*nothrow*/ @safe {
        current = standard.dup;
    }
}

void main() {
    import std.stdio;
    auto d = ["a": 1, "b": 2].DefaultAA!(string, int);

    d.writeln;                // ["a":1, "b":2]
    d["a"] = 55; d["b"] = 66;
    d.writeln;                // ["a":55, "b":66]
    d.clear;
    d.writeln;                // ["a":1, "b":2]
    d["a"] = 55; d["b"] = 66;
    d["a"].writeln;           // 55
    d.remove("a");
    d.writeln;                // ["a":1, "b":66]
}
```

```txt
["a":1, "b":2]
["a":55, "b":66]
["a":1, "b":2]
55
["a":1, "b":66]
```



## Go

Go's built-in map type is mutable and so, to complete this task, we need to create a read-only wrapper for it which doesn't permit further items to be added or existing items to be deleted though does allow them to be reset to their default value.

First create a sub-directory, romap, of the project directory and place the following package in it:

```go
package romap

type Romap struct{ imap map[byte]int }

//  Create new read-only wrapper for the given map.
func New(m map[byte]int) *Romap {
    if m == nil {
        return nil
    }
    return &Romap{m}
}

// Retrieve value for a given key, if it exists.
func (rom *Romap) Get(key byte) (int, bool) {
    i, ok := rom.imap[key]
    return i, ok
}

// Reset value for a given key, if it exists.
func (rom *Romap) Reset(key byte) {
    _, ok := rom.imap[key]
    if ok {
        rom.imap[key] = 0 // default value of int
    }
}
```


This package can now be imported and used within the main package as follows:

```go
package main

import (
    "./romap"
    "fmt"
)

func main() {
    // create a normal map
    m := map[byte]int{'A': 65, 'B': 66, 'C': 67}

    // place it in a read-only wrapper so no new item can be added or item deleted.
    rom := romap.New(m)

    // retrieve value represented by 'C' say
    i, _ := rom.Get('C')
    fmt.Println("'C' maps to", i)

    // reset this to default value (doesn't actually delete the key)
    rom.Reset('C')
    i, _ = rom.Get('C')
    fmt.Println("'C' now maps to", i)
}
```


```txt

'C' maps to 67
'C' now maps to 0

```



## J


Given a list of keys and an associated list of values, the idiomatic way of expressing this concept in J would be:


```j
lookup=: values {~ keys&i.
```


For example:


```j
   lookup=: 10 20 30 40 50 {~ (;:'this is a test')&i.
   lookup ;:'a test'
30 40
```


Notes:

1) While the result can not be modified or deleted, the name used to refer to it can be made to refer to something else, and once all references are lost it will be garbage collected.

2) In the above example, we have 5 values and 4 keys.  The extra value is used when no key is found.  If no extra value was provided, the "key not found" case would be an error case.

3) In J, objects are always referenced, but all data is passed by value.  This means that objects can never be passed to a function -- only a reference to an object (its name) can be passed.  This means that objects exist only in the way things are named, in J.  So for the most part, we do not call things "objects" in J, and this task has nothing to do with what are called "objects" in J.  However, this does demonstrate how things are created in J -- you write their definition, and can use them and/or assign to names or inspect them or whatever else.


## JavaScript

This is a first demonstration of the task, but only implemented the functionality, not any native behavior, eg indexing. JavaScript experts may want to replace this one.

```javascript
var keyError = new Error("Invalid Key Error (FixedKeyDict)") ;

function FixedKeyDict(obj)
{    
    var myDefault = new Object() ;
    var myData    = new Object() ;
    for(k in obj) {
        myDefault[k] = obj[k] ;
        myData[k]    = obj[k] ;
    }

    var gotKey = function(k) {
        for(kk in myDefault) {
            if(kk == k) return true ;
        }
        return false ;        
    } ;

    this.hasKey = gotKey ;

    var checkKey = function(k) {
        if(!gotKey(k))
            throw keyError ;
    } ;
   
    this.getItem = function(k) {
        checkKey(k) ;
        return myData[k];
    } ;
    
    this.setItem = function(k, v) {
        checkKey(k) ;
        myData[k] = v ;
    } ;
    
    this.resetItem = function(k) {
        checkKey(k) ;
        myData[k] = myDefault[k] ;      
    } ;
    
    this.delItem = this.resetItem ;
    
    this.clear   = function() {
        for(k in myDefault)
            myData[k] = myDefault[k] ;
    } ;
    
    this.iterator = function() {
        for(k in myDefault)
            yield (k);            
    } ;
    
    this.clone    = function() {
        return new FixedKeyDict(myDefault) ;
    }
    
    this.toStr = function() {
        var s = "" ;
        for(key in myData)
            s = s + key + " => " + myData[key] + ", " ;
        return "FixedKeyDict{" + s + "}" ;
    } ; 
}
```


Test run:


```javascript

const BR = "<BR>\n"

var pl = function(s) {
    document.write(s + BR) ;
} ;

pl("
```txt
") ;

var o = { foo:101, bar:102 } ;

var h = new FixedKeyDict(o) ;
pl("Fixed Key Dict Created") ;
pl("toString   : " + h.toStr()) ;
pl("get an item: " + h.getItem("foo")) ;
pl("check a key: " + h.hasKey("boo")) ;
pl("ditto      : " + h.hasKey("bar")) ;
h.setItem("bar", 999) ;
pl("set an item: " + h.toStr()) ;
pl("Test iterator (or whatever)") ;
for(k in h.iterator())
    pl("  " + k + " => " + h.getItem(k)) ;
var g = h.clone() ;
pl("Clone a dict") ;
pl("  clone    : " + g.toStr()) ;
pl("  original : " + h.toStr()) ;
h.clear() ;
pl("clear or reset the dict") ;
pl("           : " + h.toStr()) ;
try {
    h.setItem("NoNewKey", 666 ) ;
} catch(e) {
    pl("error test : " + e.message) ;
}

```


output :


```txt

Fixed Key Dict Created
toString   : FixedKeyDict{foo => 101, bar => 102, }
get an item: 101
check a key: false
ditto      : true
set an item: FixedKeyDict{foo => 101, bar => 999, }
Test iterator (or whatever)
  foo => 101
  bar => 999
Clone a dict
  clone    : FixedKeyDict{foo => 101, bar => 102, }
  original : FixedKeyDict{foo => 101, bar => 999, }
clear or reset the dict
           : FixedKeyDict{foo => 101, bar => 102, }
error test : Invalid Key Error (FixedKeyDict)

```



## jq

jq objects are JSON objects and can be created using JSON syntax, e.g. 
```jq
{"language": "jq"}
```

Objects can also be created programmatically, e.g. 
```jq
{"one": 1} + {"two": 2}
```


jq objects, however, are really just values: they are immutable, and cannot be "deleted" any more than the number 1 can be deleted.






## Kotlin


```scala
// version 1.1.2

fun main(args: Array<String>) {
    // This line creates a read-only map which cannot be changed in any way nor cleared
    val map = mapOf('A' to 65, 'B' to 66, 'C' to 67)
    println(map)
}
```


```txt

{A=65, B=66, C=67}

```



## Julia


```Julia

using BackedUpImmutable

function testBackedUpImmutableDict()
    fibr = BackedUpImmutableDict{String,Int64}(["a" => 0, "b" => 1, "c" => 1, "d" => 2,
        "e" => 3, "f" => 5, "g" => 8, "h" => 13, "i" => 21, "j" => 34, "extra" => -1])

    x = fibr["extra"]
    @test x == -1
    fibr["extra"] = 0
    y = fibr["extra"]
    @test y == 0
    restore!(fibr, "extra")
    z = fibr["extra"]
    @test z == -1
    
    @test_throws String begin fibr["k"] = 55 end
 
    fibr["a"] = 9
    fibr["b"] = 7
    
    # test restore all to default
    restoreall!(fibr)
    
    @test fibr["a"] == 0
end

```

All tests pass.


## M2000 Interpreter

```M2000 Interpreter

Module CheckIt {
      Class LockedHash {
      Private:
            inventory Vars  ' no same keys
            unlock
            module nosuchvariable {
                  Error "No such value:"+letter$
            }
            module NoNewItem {
                  Error "No new item, use unlock method before"
            }
            module NoRemoveItem {
                  Error "Can't remove item, use unlock method before"
            }
      Public:
            module Unlock {
                  .unlock<=True
            }
            module Writeln {
                  m=each(.Vars)
                  while m {
                        Print Quote$(Eval$(m, m^));",";Eval(m),
                  }
                  Print
            }
            Value (st$){
                  st$=Ucase$(st$)
                  if exist(.Vars, st$)  then =Eval(.Vars) : Exit
                  .nosuchvariable st$
            }
            Set (st$){
                  st$=Ucase$(st$)
                  Read val
                  if exist(.Vars, st$)  then Return .Vars, st$:=val : Exit
                  If .unlock then { Append .Vars, st$:=val} Else .NoNewItem
            }
            module Remove (st$) {
                  if not .unlock then .NoRemoveItem
                  st$=Ucase$(st$)
                  Try {
                        delete .Vars, st$
                  }
            }
            module Clear {
                  Clear .Vars
            }
      Class:  ' this part exist only at construction
            module LockedHash {
                  While match("SN") {
                        read st$, val
                        st$=ucase$(st$)
                        \\ if we append key which exist we get error
                        Append .Vars, st$:=val
                  }
            }
      }
      d=LockedHash("a", 1, "b", 2)
      d.writeln
      d("a")=55 : d("b")=66
      d.writeln
      d.clear
      d.writeln 
      d.unlock
      d("a")=55 : d("b")=66
      Print d("a")=55, d("a")/d("b")<1
      d.remove "a"
      d.writeln
}
Checkit

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
a[1] = "Do not modify after creation";
a[2] = "Native demonstration";
Protect[a];
```

Example usage:

```txt
a[3] = 2
->Set::write: Tag a in a[1] is Protected. >>

```



## Perl


```perl
package LockedHash;
use parent Tie::Hash;
use Carp;
use strict;

sub TIEHASH {
	my $cls = shift;
	my %h = @_;
	bless \%h, ref $cls || $cls;
}

sub STORE {
	my ($self, $k, $v) = @_;
	croak "Can't add key $k" unless exists $self->{$k};
	$self->{$k} = $v;
}

sub FETCH {
	my ($self, $k) = @_;
	croak "No key $k" unless exists $self->{$k};
	$self->{$k};
}

sub DELETE {
	my ($self, $k) = @_;
	croak "No key $k" unless exists $self->{$k};
	$self->{$k} = 0;
}

sub CLEAR { } # ignored
sub EXISTS { exists shift->{+shift} }

sub FIRSTKEY {
	my $self = shift;
	keys %$self;
	each %$self;
}

sub NEXTKEY {
	my $self = shift;
	each %$self;
}

sub lock_hash(\%) {
	my $ref = shift;
	tie(%$ref, __PACKAGE__, %$ref);
}

1;

my %h = (a => 3, b => 4, c => 5);

# lock down %h
LockedHash::lock_hash(%h);

# show hash content and iteration
for (sort keys %h) { print "$_ => $h{$_}\n"; }

# try delete b
delete $h{b};
print "\nafter deleting b: b => $h{b}\n";

# change value of a
$h{a} = 100;
print "\na => $h{a}\n";

# add a new key x: will die
eval { $h{x} = 1 };
if ($@) { print "Operation error: $@" }
```
output:<lang>a => 3
b => 4
c => 5

after deleting b: b => 0

a => 100
operation error: Can't add key x at test.pl line 14
        LockedHash::STORE('LockedHash=HASH(0x8cebe14)', 'x', 1) called at test.pl line 66
        eval {...} called at test.pl line 66
```


## Perl 6

Here we use delegation to handle all the normal hash methods that we don't need to override to define our new class.

```perl6
class FixedHash {
        has $.hash handles *;
        method new(*@args) { self.bless: hash => Hash.new: @args }
        method AT-KEY(FixedHash:D: $key is copy) is rw {
                $!hash.EXISTS-KEY($key) ?? $!hash.AT-KEY($key) !! Failure.new(q{can't store value for unknown key});
        }
        method DELETE-KEY($key) { $!hash.{$key} = Nil }
}

# Testing
my $fh = FixedHash.new: "a" => 1, "b" => 2;
say $fh<a b>;   # 1 2
$fh<b>:delete;
say $fh<a b>;   # 1 Nil
$fh<b> = 42;
say $fh<a b>;   # 1 42
say $fh<c>;     # Nil
$fh<c> = 43;    # error

```

```txt
(1 2)
(1 (Any))
(1 42)
can't store value for unknown key
  in block <unit> at native-demonstration.p6:17

Actually thrown at:
  in block <unit> at native-demonstration.p6:17
```


By defining [http://design.perl6.org/S12.html#FALLBACK_methods FALLBACK] any class can handle undefined method calls. Since any class inherits plenty of methods from <tt>Any</tt> our magic object will be more of a novice conjurer then a master wizard proper.


```perl6
class Magic {
        has %.hash;
        multi method FALLBACK($name, |c) is rw { # this will eat any extra parameters
                %.hash{$name}
        }

        multi method FALLBACK($name) is rw {
                %.hash{$name}
        }
}

my $magic = Magic.new;
$magic.foo = 10;
say $magic.foo;
$magic.defined = False; # error
```


```txt
10
Cannot modify an immutable Bool
  in block <unit> at native-demonstration.p6:15
```



## Phix

There is no native "read-only" setting on phix dictionaries, so the following wraps a pair of them to
provide the requested functionality.

```Phix
enum STD, CUR
sequence fkds = {}      -- fixed key dictionaries ;-)
integer freelist = 0

procedure fkd_destroy(integer id)
    integer {std,cur} = fkds[id]
    destroy_dict(std)
    destroy_dict(cur)
    fkds[id] = freelist
    freelist = id
end procedure

function fkd_new(sequence key_pairs)
    integer std = new_dict(key_pairs),
            cur = new_dict(std),
            id = freelist
    if id=0 then
        fkds = append(fkds,{std,cur})
        id = length(fkds)
    else
        freelist = fkds[id]
        fkds[id] = {std,cur}
    end if
    return id
end function

procedure fkd_clear(integer id)
    integer {std,cur} = fkds[id]
    destroy_dict(cur)
    fkds[id][CUR] = new_dict(std)
end procedure

function fkd_get(integer id, object key)
    return getd(key,fkds[id][CUR])
end function

procedure fkd_set(integer id, object key, data)
    integer node = getd_index(key,fkds[id][CUR])
    if node=NULL then throw("invalid/new key") end if
    setd(key,data,fkds[id][CUR])
end procedure

procedure fkd_remove(integer id, object key)
    integer {std,cur} = fkds[id],
            node = getd_index(key,std)
    if node=NULL then throw("invalid key") end if
    setd(key,getd_by_index(node,std),cur)
end procedure

function fkd_sprint(integer id)
    integer cur = fkds[id][CUR]
    sequence res = getd_all_keys(cur)
    for i=1 to length(res) do
        object ri = res[i]
        res[i] = {ri,getd(ri,cur)}
    end for
    return res
end function

procedure main()
    integer id = fkd_new({{"a",1},{"b",2}})
    ?fkd_sprint(id)                         -- {{"a",1},{"b",2}}
    fkd_set(id,"a",55)
    fkd_set(id,"b",66)
    ?fkd_sprint(id)                         -- {{"a",55},{"b",66}}
    fkd_clear(id)
    ?fkd_sprint(id)                         -- {{"a",1},{"b",2}}
    fkd_set(id,"a",55)
    fkd_set(id,"b",66)
    ?fkd_get(id,"a")                        -- 55
    fkd_remove(id,"a")
    try
        fkd_set(id,"NoNewKey",77)
    catch e
        ?e[E_USER]                          -- "invalid/new key"
    end try
    ?fkd_sprint(id)                         -- {{"a",1},{"b",66}}
    fkd_destroy(id)
end procedure
main()
```



## Python


```python

from collections import UserDict
import copy

class Dict(UserDict):
    '''
    >>> d = Dict(a=1, b=2)
    >>> d
    Dict({'a': 1, 'b': 2})
    >>> d['a'] = 55; d['b'] = 66
    >>> d
    Dict({'a': 55, 'b': 66})
    >>> d.clear()
    >>> d
    Dict({'a': 1, 'b': 2})
    >>> d['a'] = 55; d['b'] = 66
    >>> d['a']
    55
    >>> del d['a']
    >>> d
    Dict({'a': 1, 'b': 66})
    '''
    def __init__(self, dict=None, **kwargs):
        self.__init = True
        super().__init__(dict, **kwargs)
        self.default = copy.deepcopy(self.data)
        self.__init = False
    
    def __delitem__(self, key):
        if key in self.default:
            self.data[key] = self.default[key]
        else:
            raise NotImplementedError

    def __setitem__(self, key, item):
        if self.__init:
            super().__setitem__(key, item)
        elif key in self.data:
            self.data[key] = item
        else:
            raise KeyError

    def __repr__(self):
        return "%s(%s)" % (type(self).__name__, super().__repr__())
    
    def fromkeys(cls, iterable, value=None):
        if self.__init:
            super().fromkeys(cls, iterable, value)
        else:
            for key in iterable:
                if key in self.data:
                    self.data[key] = value
                else:
                    raise KeyError

    def clear(self):
        self.data.update(copy.deepcopy(self.default))

    def pop(self, key, default=None):
        raise NotImplementedError

    def popitem(self):
        raise NotImplementedError

    def update(self, E, **F):
        if self.__init:
            super().update(E, **F)
        else:
            haskeys = False
            try:
                keys = E.keys()
                haskeys = Ture
            except AttributeError:
                pass
            if haskeys:
                for key in keys:
                    self[key] = E[key]
            else:
                for key, val in E:
                    self[key] = val
            for key in F:
                self[key] = F[key]

    def setdefault(self, key, default=None):
        if key not in self.data:
            raise KeyError
        else:
            return super().setdefault(key, default)
```



## Racket

This task is implemented as a new fenced-hash time with an interface similar to the native hash. Also it can be used a native dict.

Implementation of functions that handle fenced-hash:

```Racket

;(struct fenced-hash (actual original) ...)

(define (fenced-hash-ref dict 
                         key 
                         [default (lambda () (error "key not found" key))]) 
  (hash-ref (fenced-hash-actual dict) key default)) 
(define (fenced-hash-set! dict key val) 
  (unless (hash-has-key? (fenced-hash-actual dict)  key)
    (error "unable to add key" key))
  (hash-set! (fenced-hash-actual dict) key val)) 
(define (fenced-hash-remove! dict key) ;reset the value! 
  (unless (hash-has-key? (fenced-hash-actual dict) key)
    (error "key not found" key))
  (hash-set! (fenced-hash-actual dict) 
             key
            (hash-ref (fenced-hash-original dict) key))) 
(define (fenced-hash-clear! dict) ;reset all values! 
  (hash-for-each (fenced-hash-original dict) 
                 (lambda (key val) (hash-set! (fenced-hash-actual dict) key val))))

(define (fenced-hash-has-key? dict key) 
  (hash-has-key? (fenced-hash-actual dict) key))
(define (fenced-hash-count dict)
  (hash-count (fenced-hash-actual dict)))

(define (fenced-hash-iterate-first dict) 
  (hash-iterate-first (fenced-hash-actual dict)))
(define (fenced-hash-iterate-next dict pos) 
  (hash-iterate-next (fenced-hash-actual dict) pos))
(define (fenced-hash-iterate-key dict pos) 
  (hash-iterate-key (fenced-hash-actual dict) pos))
(define (fenced-hash-iterate-value dict pos) 
  (hash-iterate-value (fenced-hash-actual dict) pos))

(define (*fenced-hash-print dict port mode) 
        ;private custom-write ;mode is ignored
     (write-string "#fenced-hash" port)
     (write (hash->list (fenced-hash-actual dict)) port))
```


Definition of the actual structure and a “public” creator:

```Racket
(struct fenced-hash (actual original)
  #:extra-constructor-name *fenced-hash ;private constructor
  #:omit-define-syntaxes ;not sure this is a good idea
  #:methods gen:custom-write 
  [(define write-proc *fenced-hash-print)]

  #:methods gen:dict 
  [(define dict-ref fenced-hash-ref)
   (define dict-set! fenced-hash-set!) 
   (define dict-remove! fenced-hash-remove!)
   (define dict-has-key? fenced-hash-has-key?) ;unused in 5.6.3
   (define dict-count fenced-hash-count)
   (define dict-iterate-first fenced-hash-iterate-first)
   (define dict-iterate-next fenced-hash-iterate-next)
   (define dict-iterate-key fenced-hash-iterate-key)
   (define dict-iterate-value fenced-hash-iterate-value)])


(define (fenced-hash . args) ; public constructor
  (define original (apply hash args))
  (*fenced-hash (hash-copy original) original))
```


'''Example:''' Use the fenced-hash functions:

```Racket
(define d (fenced-hash "a" 1 "b" 2))

(displayln d)
(fenced-hash-set! d "a" 55)
(fenced-hash-set! d "b" 66)
(displayln d)
(fenced-hash-clear! d)
(displayln d)
(fenced-hash-set! d "a" 55)
(fenced-hash-set! d "b" 66)
(displayln d)
(fenced-hash-remove! d "a")
(displayln d)
```

```txt
#fenced-hash(("b" . 2) ("a" . 1))
#fenced-hash(("b" . 66) ("a" . 55))
#fenced-hash(("b" . 2) ("a" . 1))
#fenced-hash(("b" . 66) ("a" . 55))
#fenced-hash(("b" . 66) ("a" . 1))
```


'''Example (continued):''' Use the same object as a dict. The dict-clear! method is not defined, so we must call fenced-hash-clear! instead.

```Racket
(fenced-hash-clear! d)
(displayln d)
(dict-set! d "a" 55)
(dict-set! d "b" 66)
(displayln d)
(fenced-hash-clear! d) ;dict-clear is not defined
(displayln d)
(dict-set! d "a" 55)
(dict-set! d "b" 66)
(displayln d)
(dict-remove! d "a")
(displayln d)
```

```txt
#fenced-hash(("b" . 2) ("a" . 1))
#fenced-hash(("b" . 66) ("a" . 55))
#fenced-hash(("b" . 2) ("a" . 1))
#fenced-hash(("b" . 66) ("a" . 55))
#fenced-hash(("b" . 66) ("a" . 1))
```



## Ring


```ring

# Project : Create an object/Native demonstration

map = []
map["A"] = 65
map["B"] = 66
map["C"] = 67
see map + nl

```

Output:

```txt

A
65
B
66
C
67

```



## Ruby

```ruby
# A FencedHash acts like a Hash, but with a fence around its keys.
# One may change its values, but not its keys.  Any attempt to insert
# a new key raises KeyError.  One may delete a key, but this only
# restores its original value.
#
# FencedHash reimplements these Hash methods: #[] #[]= #clear #delete
# #delete_if #default #default= #each_key #each_pair #each_value
# #fetch #has_key? #keep_if #keys #length #values #values_at
class FencedHash

  # call-seq:
  #   FencedHash.new(hash, obj=nil)  -> fh
  #
  # Creates a FencedHash that takes its keys and original values from
  # a source _hash_.  The source _hash_ can be any object that
  # responds to each_pair.  Sets the default value for missing keys to
  # _obj_, so FencedHash#[] returns _obj_ when a key is not in fence.
  def initialize(hash, obj=nil)
    @default = obj
    @hash = {}
    hash.each_pair do |key, value|
      # @hash[key][0] = current value
      # @hash[key][1] = original value
      @hash[key] = [value, value]
    end
  end

  def initialize_clone(orig)
    # Object#clone calls here in Ruby 2.0.  If _orig_ was frozen, then
    # each array of _values_ is frozen, so make frozen clones.
    super
    copy = {}
    @hash.each_pair {|key, values| copy[key] = values.clone }
    @hash = copy
  end

  def initialize_dup(orig)
    # Object#dup calls here in Ruby 2.0.  If _orig_ was frozen, then
    # make duplicates that are not frozen.
    super
    copy = {}
    @hash.each_pair {|key, values| copy[key] = values.dup }
    @hash = copy
  end

  # Retrieves current value for _key_, like Hash#[].  If _key_ is not
  # in fence, returns default object.
  def [](key)
    values = @hash[key]
    if values
      values[0]
    else
      @default
    end
  end

  # call-seq:
  #   fh[key] = value       -> value
  #   fh.store(key, value)  -> value
  #
  # Sets _value_ for a _key_.  Returns _value.  If _key_ is not in
  # fence, raises KeyError.
  def []=(key, value)
    values = @hash[key]
    if values
      values[0] = value
    else
      raise KeyError, "fence prevents adding new key: #{key.inspect}"
    end
  end
  alias store []=

  # Resets all keys to their original values.  Returns self.
  def clear
    @hash.each_value {|values| values[0] = values[1]}
    self
  end

  # Resets _key_ to its original value.  Returns old value before
  # reset.  If _key_ is not in fence, returns +nil+.
  def delete(key)
    values = @hash[key]
    if values
      old = values[0]
      values[0] = values[1]
      old  # return old
    end    # else return nil
  end

  # call-seq:
  #   fh.delete_if {|key, value| block }  -> fh
  #   fh.delete_if                        -> enumerator
  #
  # Yields each _key_ with current _value_ to _block_.  Resets _key_
  # to its original value when block evaluates to true.
  def delete_if
    if block_given?
      @hash.each_pair do |key, values|
        yield(key, values[0]) and values[0] = values[1]
      end
      self
    else
      enum_for(:delete_if) { @hash.size }
    end
  end

  # The default value for keys not in fence.
  attr_accessor :default

  # call-seq:
  #   fh.each_key {|key| block}  -> fh
  #   fh.each_key                -> enumerator
  #
  # Yields each key in fence to the block.
  def each_key(&block)
    if block
      @hash.each_key(&block)
      self
    else
      enum_for(:each_key) { @hash.size }
    end
  end

  # call-seq:
  #   fh.each_pair {|key, value| block}  -> fh
  #   fh.each_pair                       -> enumerator
  #
  # Yields each key-value pair to the block, like Hash#each_pair.
  # This yields each [key, value] as an array of 2 elements.
  def each_pair
    if block_given?
      @hash.each_pair {|key, values| yield [key, values[0]] }
      self
    else
      enum_for(:each_pair) { @hash.size }
    end
  end

  # call-seq
  #   fh.each_value {|value| block} -> fh
  #   fh.each_value                 -> enumerator
  #
  # Yields current value of each key-value pair to the block.
  def each_value
    if block_given?
      @hash.each_value {|values| yield values[0] }
    else
      enum_for(:each_value) { @hash.size }
    end
  end

  # call-seq:
  #   fenhsh.fetch(key [,default])
  #   fenhsh.fetch(key) {|key| block }
  #
  # Fetches value for _key_.  Takes same arguments as Hash#fetch.
  def fetch(*argv)
    argc = argv.length
    unless argc.between?(1, 2)
      raise(ArgumentError,
            "wrong number of arguments (#{argc} for 1..2)")
    end
    if argc == 2 and block_given?
      warn("#{caller[0]}: warning: " +
           "block supersedes default value argument")
    end

    key, default = argv
    values = @hash[key]
    if values
      values[0]
    elsif block_given?
      yield key
    elsif argc == 2
      default
    else
      raise KeyError, "key not found: #{key.inspect}"
    end
  end

  # Freezes this FencedHash.
  def freeze
    @hash.each_value {|values| values.freeze }
    super
  end

  # Returns true if _key_ is in fence.
  def has_key?(key)
    @hash.has_key?(key)
  end
  alias include? has_key?
  alias member? has_key?

  # call-seq:
  #   fh.keep_if {|key, value| block }  -> fh
  #   fh.keep_if                        -> enumerator
  #
  # Yields each _key_ with current _value_ to _block_.  Resets _key_
  # to its original value when block evaluates to false.
  def keep_if
    if block_given?
      @hash.each_pair do |key, values|
        yield(key, values[0]) or values[0] = values[1]
      end
      self
    else
      enum_for(:keep_if) { @hash.size }
    end
  end

  # Returns array of keys in fence.
  def keys
    @hash.keys
  end

  # Returns number of key-value pairs.
  def length
    @hash.length
  end
  alias size length

  # Converts self to a regular Hash.
  def to_h
    result = Hash.new(@default)
    @hash.each_pair {|key, values| result[key] = values[0]}
    result
  end

  # Converts self to a String.
  def to_s
    "#<#{self.class}: #{to_h}>"
  end
  alias inspect to_s

  # Returns array of current values.
  def values
    @hash.each_value.map {|values| values[0]}
  end

  # Returns array of current values for keys, like Hash#values_at.
  def values_at(*keys)
    keys.map {|key| self[key]}
  end
end
```


## Scala

{{Out}}Best seen running in your browser either by [https://scalafiddle.io/sf/OuVZ3bT/0 ScalaFiddle (ES aka JavaScript, non JVM)] or [https://scastie.scala-lang.org/qW5qzmdKSZSyAbZEqDROoA Scastie (remote JVM)].

```Scala
object CreateMapObject extends App {
  val map = Map('A' -> 65, 'B' -> 66, 'C' -> 67)

  println(map)
}
```



## Tcl

This solution uses a dict(ionary), so requires Tcl 8.5 or better. Variable traces are used to detect write or unset access to such a protected variable, restore it to the backup value at protection time, and throw an exception


```Tcl
proc protect _var {
    upvar 1 $_var var
    trace add variable var {write unset} [list protect0 $var]
}
proc protect0 {backup name1 name2 op} {
    upvar 1 $name1 var
    trace remove variable var {write unset} [list protect 0 $backup]
    set var $backup
    trace add variable var {write unset} [list protect0 $backup]
    return -code error "$name1 is protected"
}
proc trying cmd { #-- convenience function for demo
    puts "trying: $cmd"
    if [catch {uplevel 1 $cmd} msg] {puts $msg}
}
```

Testing:
 dict set dic 1 one 
 dict set dic 2 two
 puts dic:$dic
 protect dic
 trying "dict set dic 3 three"
 puts dic:$dic
 trying "dict unset dic 1"
 trying "unset dic"
 puts dic:$dic

displays on stdout:
 dic:1 one 2 two
 trying: dict set dic 3 three
 can't set "dic": dic is protected
 dic:1 one 2 two
 trying: dict unset dic 1
 can't set "dic": dic is protected
 trying: unset dic
 dic:1 one 2 two


## zkl

zkl has two dictionary objects: SD, a small dictionary that is created immutable and the "regular" dictionary has has a makeReadOnly method. They both behave the same when locked down.
<lang zkl">d:=SD("one",1,"two",2);
d.keys;   //-->L("one","two")
d["one"]; //-->1
d.add("three",3); // error thrown
d.pop("one")   // error thrown
```





