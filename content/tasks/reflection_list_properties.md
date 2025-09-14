+++
title = "Reflection/List properties"
description = ""
date = 2019-10-18T18:29:54Z
aliases = []
[extra]
id = 21023
[taxonomies]
categories = ["task", "Reflection"]
tags = []
languages = [
  "csharp",
  "elena",
  "factor",
  "fortran",
  "go",
  "groovy",
  "j",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "lingo",
  "oorexx",
  "perl",
  "perl_6",
  "php",
  "pl_i",
  "powershell",
  "python",
  "rexx",
  "ruby",
  "scala",
  "tcl",
  "visual_basic_dotnet",
  "zkl",
]
+++

{{task|Reflection}} [[Category:Programming Tasks]] [[Category:Object oriented]]
## Task

The goal is to get the properties of an object, as names, values or both.

Some languages support dynamic properties, which in general can only be inspected if a class' public API includes a way of listing them.


## C#

```c#
using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

public static class Reflection
{
    public static void Main() {
        var t = new TestClass();
        var flags = BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance;
        foreach (var prop in GetPropertyValues(t, flags)) {
            Console.WriteLine(prop);
        }
        foreach (var field in GetFieldValues(t, flags)) {
            Console.WriteLine(field);
        }
    }

    public static IEnumerable<(string name, object value)> GetPropertyValues<T>(T obj, BindingFlags flags) =>
        from p in typeof(T).GetProperties(flags)
        where p.GetIndexParameters().Length == 0 //To filter out indexers
        select (p.Name, p.GetValue(obj, null));

    public static IEnumerable<(string name, object value)> GetFieldValues<T>(T obj, BindingFlags flags) =>
        typeof(T).GetFields(flags).Select(f => (f.Name, f.GetValue(obj)));

    class TestClass
    {
        private int privateField = 7;
        public int PublicNumber { get; } = 4;
        private int PrivateNumber { get; } = 2;
    }

}
```

```txt

(PublicNumber, 4)
(PrivateNumber, 2)
(privateField, 7)
(<PublicNumber>k__BackingField, 4)
(<PrivateNumber>k__BackingField, 2)

```



## Elena

ELENA 4.1 :

```elena
import system'routines;
import system'dynamic;
import extensions;

class MyClass
{
    prop int X;
    prop string Y;
}

public program()
{
    var o := new MyClass::
    {
        this X := 2;

        this Y := "String";
    };

    MyClass.__getProperties().forEach:(p)
    {
        console.printLine("o.",p,"=",cast MessageName(p).getPropertyValue(o))
    }
}
```

```txt

o.X=2
o.Y=String

```



## Factor

Mirrors present an object's slots and slot values as an associative structure.

```factor
USING: assocs kernel math mirrors prettyprint strings ;

TUPLE: foo
{ bar string }
{ baz string initial: "hi" }
{ baxx integer initial: 50 read-only } ;
C: <foo> foo

"apple" "banana" 200 <foo> <mirror>
[ >alist ] [ object-slots ] bi [ . ] bi@
```

```txt

{ { "bar" "apple" } { "baz" "banana" } { "baxx" 200 } }
{
    T{ slot-spec
        { name "bar" }
        { offset 2 }
        { class string }
        { initial "" }
    }
    T{ slot-spec
        { name "baz" }
        { offset 3 }
        { class string }
        { initial "hi" }
    }
    T{ slot-spec
        { name "baxx" }
        { offset 4 }
        { class integer }
        { initial 50 }
        { read-only t }
    }
}

```



## Fortran

Fortran offers services somewhat as desired, at two levels. Suppose <code>X</code> is the name of some variable. Then, <code>WRITE (6,*) X</code> would send to file unit six (the modern default unit number for "standard output"), the value of <code>X</code>, whatever its type - be it an array, a complex number, an integer, a character variable, or, (with F90 and later) an aggregate or "structure", all with appropriate formats for each part, single or double precision, ''etc''. This is the "free-format" or "list-directed" style, signified by the asterisk in place of a format code or the label of a FORMAT statement. With arrays, repeated values are shown with a repetition count, as in 66*303 meaning sixty-six values of 303. Alas, the @ symbol is not used. Thus, inspection of the style of such output will reveal whether <code>X</code> was an integer, ''etc''.

The second level requires slightly more effort: a declaration such as <code>NAMELIST /STUFF/ X</code> enables the use of a statement such as <code>WRITE (6,STUFF)</code> whereupon each item's value will be prefixed with its name in the style of an assignment statement, as in <code>X = 2.7182818</code> if <code>X</code> were to be the name of a simple floating-point variable.

There may be a list of items, not just the lone <code>X</code> and these proceedings apply to READ statements also.


## Go


```Go
package main

import (
	"fmt"
	"image"
	"reflect"
)

// A type definition
type t struct {
	X    int
	next *t
}

func main() {
	report(t{})
	report(image.Point{})
}

func report(x interface{}) {
	t := reflect.TypeOf(x)
	n := t.NumField()
	fmt.Printf("Type %v has %d fields:\n", t, n)
	fmt.Println("Name     Type     Exported")
	for i := 0; i < n; i++ {
		f := t.Field(i)
		fmt.Printf("%-8s %-8v %-8t\n",
			f.Name,
			f.Type,
			f.PkgPath == "",
		)
	}
	fmt.Println()
}
```

```txt

Type main.t has 2 fields:
Name     Type     Exported
X        int      true
next     *main.t  false

Type image.Point has 2 fields:
Name     Type     Exported
X        int      true
Y        int      true

```



## Groovy

```groovy
import java.lang.reflect.Field

@SuppressWarnings("unused")
class ListProperties {
    public int examplePublicField = 42
    private boolean examplePrivateField = true

    static void main(String[] args) {
        ListProperties obj = new ListProperties()
        Class clazz = obj.class

        println "All public fields (including inherited):"
        (clazz.fields).each { Field f ->
            printf "%s\t%s\n", f, f.get(obj)
        }
        println()

        println "All declared fields (excluding inherited):"
        clazz.getDeclaredFields().each { Field f ->
            f.accessible = true
            printf "%s\t%s\n", f, f.get(obj)
        }
    }
}
```

```txt
All public fields (including inherited):
public int ListProperties.examplePublicField	42
public static transient boolean ListProperties.__$stMC	false

All declared fields (excluding inherited):
public int ListProperties.examplePublicField	42
private boolean ListProperties.examplePrivateField	true
private static org.codehaus.groovy.reflection.ClassInfo ListProperties.$staticClassInfo	org.codehaus.groovy.reflection.ClassInfo@400cff1a
public static transient boolean ListProperties.__$stMC	false
private transient groovy.lang.MetaClass ListProperties.metaClass	groovy.lang.MetaClassImpl@275710fc[class ListProperties]
private static org.codehaus.groovy.reflection.ClassInfo ListProperties.$staticClassInfo$	null
private static java.lang.ref.SoftReference ListProperties.$callSiteArray	java.lang.ref.SoftReference@525f1e4e
```



## J

http://rosettacode.org/wiki/Reflection/List_methods#J
Please observe that names&>i.4 lists nouns (pronouns that store data), adverbs (names of verb modifiers returning any of these four parts of speech), conjunctions (which can take three or four arguments, two of which can any part of these four parts of speech and two nouns), and proverbs (names of verbs, which you might call "functions").


## Java


```java
import java.lang.reflect.Field;

public class ListFields {
    public int examplePublicField = 42;
    private boolean examplePrivateField = true;

    public static void main(String[] args) throws IllegalAccessException {
        ListFields obj = new ListFields();
        Class clazz = obj.getClass();

        System.out.println("All public fields (including inherited):");
        for (Field f : clazz.getFields()) {
            System.out.printf("%s\t%s\n", f, f.get(obj));
        }
        System.out.println();
        System.out.println("All declared fields (excluding inherited):");
        for (Field f : clazz.getDeclaredFields()) {
            System.out.printf("%s\t%s\n", f, f.get(obj));
        }
    }
}
```

```txt

All public fields (including inherited):
public int ListFields.examplePublicField	42

All declared fields (excluding inherited):
public int ListFields.examplePublicField	42
private boolean ListFields.examplePrivateField	true

```



## JavaScript

There are multiple ways of getting property names, each of which include different subsets of an object's properties, such as enumerable or inherited properties. Properties in JavaScript can be enumerable or non-enumerable; enumerable properties are accessable when looping over the object with <code>for</code>. <code>Object.getOwnPropertyNames()</code>.


```javascript
var obj = Object.create({
    name: 'proto',
    proto: true,
    doNothing: function() {}
  }, {
    name: {value: 'obj', writable: true, configurable: true, enumerable: true},
    obj: {value: true, writable: true, configurable: true, enumerable: true},
    'non-enum': {value: 'non-enumerable', writable: true, enumerable: false},
    doStuff: {value: function() {}, enumerable: true}
});

// get enumerable properties on an object and its ancestors
function get_property_names(obj) {
    var properties = [];
    for (var p in obj) {
        properties.push(p);
    }
    return properties;
}

get_property_names(obj);
//["name", "obj", "doStuff", "proto", "doNothing"]

Object.getOwnPropertyNames(obj);
//["name", "obj", "non-enum", "doStuff"]

Object.keys(obj);
//["name", "obj", "doStuff"]

Object.entries(obj);
//[["name", "obj"], ["obj", true], ["doStuff", function()]]

```



## Julia

```julia
for obj in (Int, 1, 1:10, collect(1:10), now())
    println("\nObject: $obj\nDescription:")
    dump(obj)
end
```


```txt

Object: Int64
Description:
Int64 <: Signed

Object: 1
Description:
Int64 1

Object: 1:10
Description:
UnitRange{Int64}
  start: Int64 1
  stop: Int64 10

Object: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
Description:
Array{Int64}((10,)) [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

Object: 2017-10-04T18:03:33.691
Description:
DateTime
  instant: Base.Dates.UTInstant{Base.Dates.Millisecond}
    periods: Base.Dates.Millisecond
      value: Int64 63642823413691
```



## Kotlin


```scala
// version 1.1

import kotlin.reflect.full.memberProperties
import kotlin.reflect.jvm.isAccessible

open class BaseExample(val baseProp: String) {
    protected val protectedProp: String = "inherited protected value"
}

class Example(val prop1: String, val prop2: Int, baseProp: String) : BaseExample(baseProp) {
    private val privateProp: String = "private value"

    val prop3: String
        get() = "property without backing field"

    val prop4 by lazy { "delegated value" }
}

fun main(args: Array<String>) {
    val example = Example(prop1 = "abc", prop2 = 1, baseProp = "inherited public value")
    val props = Example::class.memberProperties
    for (prop in props) {
        prop.isAccessible = true  // makes non-public properties accessible
        println("${prop.name.padEnd(13)} -> ${prop.get(example)}")
    }
}
```


```txt

privateProp   -> private value
prop1         -> abc
prop2         -> 1
prop3         -> property without backing field
prop4         -> delegated value
baseProp      -> inherited public value
protectedProp -> inherited protected value

```



## Lingo


```lingo
obj = script("MyClass").new()
obj.foo = 23
obj.bar = 42

-- ...

-- show obj's property names and values
cnt = obj.count
repeat with i = 1 to cnt
  put obj.getPropAt(i)&" = "&obj[i]
end repeat
```


```txt

-- "bar = 42"
-- "foo = 23"

```


=={{header|Objective-C}}==

```objc>#import <Foundation/Foundation.h

#import <objc/runtime.h>

@interface Foo : NSObject {
  int exampleIvar;
}
@property (nonatomic) double exampleProperty;
@end
@implementation Foo
- (instancetype)init {
  self = [super init];
  if (self) {
    exampleIvar = 42;
    _exampleProperty = 3.14;
  }
  return self;
}
@end

int main() {
  id obj = [[Foo alloc] init];
  Class clazz = [obj class];

  NSLog(@"\Instance variables:");
  unsigned int ivarCount;
  Ivar *ivars = class_copyIvarList(clazz, &ivarCount);
  for (unsigned int i = 0; i < ivarCount; i++) {
    Ivar ivar = ivars[i];
    const char *name = ivar_getName(ivar);
    const char *typeEncoding = ivar_getTypeEncoding(ivar);
    // for simple types we can use Key-Value Coding to access it
    // but in general we will have to use object_getIvar and cast it to the right type of function
    // corresponding to the type of the instance variable
    id value = [obj valueForKey:@(name)];
    NSLog(@"%s\t%s\t%@", name, typeEncoding, value);
  }
  free(ivars);

  NSLog(@"");
  NSLog(@"Properties:");
  unsigned int propCount;
  objc_property_t *properties = class_copyPropertyList([Foo class], &propCount);
  for (unsigned int i = 0; i < propCount; i++) {
    objc_property_t p = properties[i];
    const char *name = property_getName(p);
    const char *attributes = property_getAttributes(p);
    // for simple types we can use Key-Value Coding to access it
    // but in general we will have to use objc_msgSend to call the getter,
    // casting objc_msgSend to the right type of function corresponding to the type of the getter
    id value = [obj valueForKey:@(name)];
    NSLog(@"%s\t%s\t%@", name, attributes, value);
  }
  free(properties);

  return 0;
}
```

```txt

Instance variables:
exampleIvar	i	42
_exampleProperty	d	3.14

Properties:
exampleProperty	Td,N,V_exampleProperty	3.14

```



## ooRexx

Whereas in PL/I variables habe a (declared) type, in REXX and ooRexx a "typeless" variable
can be assigned a string or object, respectively.

The datatype builtin function can be used to determine the data type of a given string.

Many of these options are also supported by other REXX implementations.

```oorexx
/* REXX demonstrate uses of datatype() */
/* test values                         */
d.1=''
d.2='a23'
d.3='101'
d.4='123'
d.5='12345678901234567890'
d.6='abc'
d.7='aBc'
d.8='1'
d.9='0'
d.10='Walter'
d.11='ABC'
d.12='f23'
d.13='123'
/* supported options                   */
t.1='A'       /* Alphanumeric          */
t.2='B'       /* Binary                */
t.3='I'       /* Internal whole number */
t.4='L'       /* Lowercase             */
t.5='M'       /* Mixed case            */
t.6='N'       /* Number                */
t.7='O'       /* lOgical               */
t.8='S'       /* Symbol                */
t.9='U'       /* Uppercase             */
t.10='V'      /* Variable              */
t.11='W'      /* Whole number          */
t.12='X'      /* heXadecimal           */
t.13='9'      /* 9 digits              */

hdr=left('',20)
Do j=1 To 13
  hdr=hdr t.j
  End
hdr=hdr 'datatype(v)'
Say hdr
Do i=1 To 13
  ol=left(d.i,20)
  Do j=1 To 13
    ol=ol datatype(d.i,t.j)
    End
  ol=ol datatype(d.i)
  Say ol
  End
Say hdr
```

```txt
                     A B I L M N O S U V W X 9 datatype(v)
                     0 1 0 0 0 0 0 0 0 0 0 1 0 CHAR
a23                  1 0 0 0 0 0 0 1 0 1 0 1 0 CHAR
101                  1 1 1 0 0 1 0 1 0 0 1 1 1 NUM
123                  1 0 1 0 0 1 0 1 0 0 1 1 1 NUM
12345678901234567890 1 0 0 0 0 1 0 1 0 0 0 1 0 NUM
abc                  1 0 0 1 1 0 0 1 0 1 0 1 0 CHAR
aBc                  1 0 0 0 1 0 0 1 0 1 0 1 0 CHAR
1                    1 1 1 0 0 1 1 1 0 0 1 1 1 NUM
0                    1 1 1 0 0 1 1 1 0 0 1 1 1 NUM
Walter               1 0 0 0 1 0 0 1 0 1 0 0 0 CHAR
ABC                  1 0 0 0 1 0 0 1 1 1 0 1 0 CHAR
f23                  1 0 0 0 0 0 0 1 0 1 0 1 0 CHAR
123                  1 0 1 0 0 1 0 1 0 0 1 1 1 NUM
                     A B I L M N O S U V W X 9 datatype(v)
```



## Perl

In Perl's bare-bones native OO system, an object is sometimes nothing more than a hash blessed into a class. It's properties could be simply listed by iterating over the keys.  However more complex data structures can be present, so the safest option is always to use <code>Data::Dumper</code> to examine an object.

```perl
{
     package Point;
     use Class::Spiffy -base;

     field 'x';
     field 'y';
}

{
     package Circle;
     use base qw(Point);
     field 'r';
}

my $p1 = Point->new(x => 8, y => -5);
my $c1 = Circle->new(r => 4);
my $c2 = Circle->new(x => 1, y => 2, r => 3);

use Data::Dumper;
say Dumper $p1;
say Dumper $c1;
say Dumper $c2;
```

```txt
$VAR1 = bless( {
                 'x' => 8,
                 'y' => -5
               }, 'Point' );

$VAR1 = bless( {
                 'r' => 4
               }, 'Circle' );

$VAR1 = bless( {
                 'r' => 3,
                 'x' => 1,
                 'y' => 2
               }, 'Circle' );

```


## Perl 6


You can get a list of an object's attributes (instance variables) using <tt>.^attributes</tt>, which is part of the [https://docs.perl6.org/type/Metamodel$COLON$COLONClassHOW Meta Object Protocol]..

Each is represented as an <tt>Attribute</tt> object that contains a bunch of info:


```perl6
class Foo {
    has $!a = now;
    has Str $.b;
    has Int $.c is rw;
}

my $object = Foo.new: b => "Hello", c => 42;

for $object.^attributes {
    say join ", ", .name, .readonly, .container.^name, .get_value($object);
}
```


```txt

$!a, True, Any, Instant:1470517602.295992
$!b, True, Str, Hello
$!c, False, Int, 42

```


Public attributes (in this case, <tt>$.b</tt> and <tt>$.c</tt>) are really just attributes for which the compiler also auto-generates a method of the same name. See [[Reflection/List_methods#Perl_6]].


## PHP


```php
<?
class Foo {
}
$obj = new Foo();
$obj->bar = 42;
$obj->baz = true;

var_dump(get_object_vars($obj));
?>
```

```txt

array(2) {
  ["bar"]=>
  int(42)
  ["baz"]=>
  bool(true)
}

```



## PL/I

<code>PUT DATA(X)</code> will send to SYSOUT the value(s) of X prefixed by the name(s), formatted appropriately for single/double bit/integer/real/complex, character, ''etc.'' whether <code>X</code> is a single datum, an array, a data structure, ''etc.'' A list of items may be specified, not just the single <code>X</code> and for input, the word is <code>GET</code>. Similarly, <code>PUT STRING(TEXT) DATA(X)</code> will place such output in a character variable, which may have to be large...


## PowerShell

In PowerShell '''everything''' is an object.  To find any type of member of any object use the <code>Get-Member</code> Cmdlet.

Here we find the properties of a <code>[DateTime]</code> object:

```PowerShell

Get-Date | Get-Member -MemberType Property

```


```txt

   TypeName: System.DateTime

Name        MemberType Definition
----        ---------- ----------
Date        Property   datetime Date {get;}
Day         Property   int Day {get;}
DayOfWeek   Property   System.DayOfWeek DayOfWeek {get;}
DayOfYear   Property   int DayOfYear {get;}
Hour        Property   int Hour {get;}
Kind        Property   System.DateTimeKind Kind {get;}
Millisecond Property   int Millisecond {get;}
Minute      Property   int Minute {get;}
Month       Property   int Month {get;}
Second      Property   int Second {get;}
Ticks       Property   long Ticks {get;}
TimeOfDay   Property   timespan TimeOfDay {get;}
Year        Property   int Year {get;}

```

The "Add" methods of a <code>[DateTime]</code> object:

```PowerShell

Get-Date | Get-Member -MemberType Method -Name Add*

```

```txt

   TypeName: System.DateTime

Name            MemberType Definition
----            ---------- ----------
Add             Method     datetime Add(timespan value)
AddDays         Method     datetime AddDays(double value)
AddHours        Method     datetime AddHours(double value)
AddMilliseconds Method     datetime AddMilliseconds(double value)
AddMinutes      Method     datetime AddMinutes(double value)
AddMonths       Method     datetime AddMonths(int months)
AddSeconds      Method     datetime AddSeconds(double value)
AddTicks        Method     datetime AddTicks(long value)
AddYears        Method     datetime AddYears(int value)

```



## Python

The <code>[https://docs.python.org/3.5/library/functions.html#dir dir()]</code> function and Python's <code>[https://docs.python.org/3.5/library/inspect.html#module-inspect inspect]</code> module both will list properties.


```python
class Parent(object):
    __priv = 'private'

    def __init__(self, name):
        self.name = name

    def __repr__(self):
        return '%s(%s)' % (type(self).__name__, self.name)

    def doNothing(self):
        pass

import re

class Child(Parent):
    # prefix for "private" fields
    __rePrivate = re.compile('^_(Child|Parent)__')
    # used when setting dynamic property values
    __reBleh = re.compile('\Wbleh$')
    @property
    def reBleh(self):
        return self.__reBleh

    def __init__(self, name, *args):
        super(Child, self).__init__(name)
        self.args = args

    def __dir__(self):
        myDir = filter(
            # filter out private fields
            lambda p: not self.__rePrivate.match(p),
            list(set( \
                sum([dir(base) for base in type(self).__bases__], []) \
                + type(self).__dict__.keys() \
                + self.__dict__.keys() \
            )))
        return myDir + map(
            # dynamic properties
            lambda p: p + '_bleh',
            filter(
                # don't add dynamic properties for methods and other special properties
                lambda p: (p[:2] != '__' or p[-2:] != '__') and not callable(getattr(self, p)),
                myDir))

    def __getattr__(self, name):
        if name[-5:] == '_bleh':
            # dynamic '_bleh' properties
            return str(getattr(self, name[:-5])) + ' bleh'
        if hasattr(super(Child, chld), '__getattr__'):
            return super(Child, self).__getattr__(name)
        raise AttributeError("'%s' object has no attribute '%s'" % (type(self).__name__, name))

    def __setattr__(self, name, value):
        if name[-5:] == '_bleh':
            # skip backing properties that are methods
            if not (hasattr(self, name[:-5]) and callable(getattr(self, name[:-5]))):
                setattr(self, name[:-5], self.reBleh.sub('', value))
        elif hasattr(super(Child, self), '__setattr__'):
            super(Child, self).__setattr__(name, value)
        elif hasattr(self, '__dict__'):
            self.__dict__[name] = value

    def __repr__(self):
        return '%s(%s, %s)' % (type(self).__name__, self.name, str(self.args).strip('[]()'))

    def doStuff(self):
        return (1+1.0/1e6) ** 1e6

par = Parent('par')
par.parent = True
dir(par)
#['_Parent__priv', '__class__', ..., 'doNothing', 'name', 'parent']
inspect.getmembers(par)
#[('_Parent__priv', 'private'), ('__class__', <class '__main__.Parent'>), ..., ('doNothing', <bound method Parent.doNothing of <__main__.Parent object at 0x100777650>>), ('name', 'par'), ('parent', True)]

chld = Child('chld', 0, 'I', 'two')
chld.own = "chld's own"
dir(chld)
#['__class__', ..., 'args', 'args_bleh', 'doNothing', 'doStuff', 'name', 'name_bleh', 'own', 'own_bleh', 'reBleh', 'reBleh_bleh']
inspect.getmembers(chld)
#[('__class__', <class '__main__.Child'>), ..., ('args', (0, 'I', 'two')), ('args_bleh', "(0, 'I', 'two') bleh"), ('doNothing', <bound method Child.doNothing of Child(chld, 0, 'I', 'two')>), ('doStuff', <bound method Child.doStuff of Child(chld, 0, 'I', 'two')>), ('name', 'chld'), ('name_bleh', 'chld bleh'), ('own', "chld's own"), ('own_bleh', "chld's own bleh"), ('reBleh', <_sre.SRE_Pattern object at 0x10067bd20>), ('reBleh_bleh', '<_sre.SRE_Pattern object at 0x10067bd20> bleh')]

```



## REXX


### version 1

(This REXX version is modeled after the   '''PL/I'''   entry.)


The   '''say'''   instruction can display a value (its contents) to
the console (terminal) as well as
its length   (the number of characters in its value).

Since   ''everything''   in REXX is a character string, the   ''type''   of the
variable (character) need not be explicitly expressed.


A simplistic example:

```rexx
j=2
abc.j= -4.12


say 'variable abc.2 (length' length(abc.2)')='  abc.2
```





### version 2


```rexx
/* REXX shows the "equivalent" to PL/I's PUT DATA for a simple variable */
/* put_data2('a.') to show all a.tail values isn't that easy :-)        */
j=2
abc.j= -4.12
Say put_data('abc.2')     /* Put Data(abc(2)) */
string=put_data('abc.2')  /* Put string(string) Data(abc(2)) */
Say string
Exit
put_data:
Parse Arg variable
return(variable'='''value(variable)'''')
```

```txt
abc.2='-4.12'
abc.2='-4.12'
```



## Ruby


```ruby
class Foo
  @@xyz = nil
  def initialize(name, age)
    @name, @age = name, age
  end
  def add_sex(sex)
    @sex = sex
  end
end

p foo = Foo.new("Angel", 18)            #=> #<Foo:0x0000000305a688 @name="Angel", @age=18>
p foo.instance_variables                #=> [:@name, :@age]
p foo.instance_variable_defined?(:@age) #=> true
p foo.instance_variable_get(:@age)      #=> 18
p foo.instance_variable_set(:@age, 19)  #=> 19
p foo                                   #=> #<Foo:0x0000000305a688 @name="Angel", @age=19>
foo.add_sex(:woman)
p foo.instance_variables                #=> [:@name, :@age, :@sex]
p foo                                   #=> #<Foo:0x0000000305a688 @name="Angel", @age=19, @sex=:woman>
foo.instance_variable_set(:@bar, nil)
p foo.instance_variables                #=> [:@name, :@age, :@sex, :@bar]

p Foo.class_variables                   #=> [:@@xyz]
p Foo.class_variable_defined?(:@@xyz)   #=> true
p Foo.class_variable_get(:@@xyz)        #=> nil
p Foo.class_variable_set(:@@xyz, :xyz)  #=> :xyz
p Foo.class_variable_get(:@@xyz)        #=> :xyz
p Foo.class_variable_set(:@@abc, 123)   #=> 123
p Foo.class_variables                   #=> [:@@xyz, :@@abc]
```



## Scala


### Java Interoperability

{{Out}}Best seen running in your browser [https://scastie.scala-lang.org/MdkPxH6yTlS4W8TaXYxSgA Scastie (remote JVM)].

```Scala
object ListProperties extends App {
  private val obj = new {
    val examplePublicField: Int = 42
    private val examplePrivateField: Boolean = true
  }
  private val clazz = obj.getClass

  println("All public methods (including inherited):")
  clazz.getFields.foreach(f => println(s"${f}\t${f.get(obj)}"))

  println("\nAll declared fields (excluding inherited):")
  clazz.getDeclaredFields.foreach(f => println(s"${f}}"))
}
```



## Tcl

Tcl objects do not have properties exactly (externally visible variables), though a common idiom pioneered by <b>Tk</b> is <i>options</i> exposed by the <tt>configure</tt> and <tt>cget</tt> commands.

For objects supporting this protocol, you can list all options by invoking the <tt>configure</tt> method without arguments (result split over multiple lines for readability):

```Tcl
% package require Tk
8.6.5
% . configure
{-bd -borderwidth} {-borderwidth borderWidth BorderWidth 0 0} {-class class Class Toplevel Tclsh}
 {-menu menu Menu {} {}} {-relief relief Relief flat flat} {-screen screen Screen {} {}} {-use use Use {} {}}
 {-background background Background #d9d9d9 #d9d9d9} {-bg -background} {-colormap colormap Colormap {} {}}
 {-container container Container 0 0} {-cursor cursor Cursor {} {}} {-height height Height 0 0}
 {-highlightbackground highlightBackground HighlightBackground #d9d9d9 #d9d9d9}
 {-highlightcolor highlightColor HighlightColor #000000 #000000}
 {-highlightthickness highlightThickness HighlightThickness 0 0} {-padx padX Pad 0 0} {-pady padY Pad 0 0}
 {-takefocus takeFocus TakeFocus 0 0} {-visual visual Visual {} {}} {-width width Width 0 0}
```


Two-element sublists (eg <tt>-bd -borderwidth</tt>) represent aliases, and five-element sublists are of the form <tt>{optionName dbName dbClass defaultValue currentValue}</tt>.  <tt>dbName</tt> and <tt>dbClass</tt> are related to how the option is specified in the <i>option database</i>.

Simply listing the option names is like this:


```Tcl
% lmap o [. configure] {if {[llength $o] == 2} continue else {lindex $o 0}}
-borderwidth -class -menu -relief -screen -use -background -colormap -container -cursor -height
 -highlightbackground -highlightcolor -highlightthickness -padx -pady -takefocus -visual -width
```



## Visual Basic .NET

```vbnet
Imports System.Reflection

Module Module1

    Class TestClass
        Private privateField = 7
        Public ReadOnly Property PublicNumber = 4
        Private ReadOnly Property PrivateNumber = 2
    End Class

    Function GetPropertyValues(Of T)(obj As T, flags As BindingFlags) As IEnumerable
        Return From p In obj.GetType().GetProperties(flags)
               Where p.GetIndexParameters().Length = 0
               Select New With {p.Name, Key .Value = p.GetValue(obj, Nothing)}
    End Function

    Function GetFieldValues(Of T)(obj As T, flags As BindingFlags) As IEnumerable
        Return obj.GetType().GetFields(flags).Select(Function(f) New With {f.Name, Key .Value = f.GetValue(obj)})
    End Function

    Sub Main()
        Dim t As New TestClass()
        Dim flags = BindingFlags.Public Or BindingFlags.NonPublic Or BindingFlags.Instance
        For Each prop In GetPropertyValues(t, flags)
            Console.WriteLine(prop)
        Next
        For Each field In GetFieldValues(t, flags)
            Console.WriteLine(field)
        Next
    End Sub

End Module
```

```txt
{ Name = PublicNumber, Value = 4 }
{ Name = PrivateNumber, Value = 2 }
{ Name = privateField, Value = 7 }
{ Name = _PublicNumber, Value = 4 }
{ Name = _PrivateNumber, Value = 2 }
```



## zkl

In zkl, properties are static read only informational data.

Every object has a "properties" method, which returns a list of property names [for that object].

```zkl
properties:=List.properties;
properties.println();
List(1,2,3).property(properties[0]).println();   // get value
List(1,2,3).Property(properties[0])().println(); // method that gets value
List(1,2,3).BaseClass(properties[0]).println();  // another way to get value
```

```txt

L("size","isReadOnly","id","name","fullName","type","otype","oID","itype","typeID","properties","methods","vaultPath","numObjects","isThreadSafe","isContainer","createReturnsSelf")
L(24,32)
L(24,32)
L(24,32)

```

