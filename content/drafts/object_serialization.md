+++
title = "Object serialization"
description = ""
date = 2019-09-12T19:45:51Z
aliases = []
[extra]
id = 1995
[taxonomies]
categories = []
tags = []
+++

{{task|Object oriented}}
Create a set of data types based upon [[inheritance]]. Each data type or class should have a print command that displays the contents of an instance of that class to standard output. Create instances of each class in your inheritance hierarchy and display them to standard output. Write each of the objects to a file named ''objects.dat'' in binary form using serialization or marshalling. Read the file ''objects.dat'' and print the contents of each serialized object.


## Ada

This file contains the package specification containing the public definitions of the inheritance tree rooted at the type ''Message''. Each type in the inheritance tree has its own print procedure.

```ada
with Ada.Calendar; use Ada.Calendar;

package Messages is
   type Message is tagged record
      Timestamp : Time;
   end record;

   procedure Print(Item : Message);
   procedure Display(Item : Message'Class);

   type Sensor_Message is new Message with record
      Sensor_Id : Integer;
      Reading : Float;
   end record;

   procedure Print(Item : Sensor_Message);

   type Control_Message is new Message with record
      Actuator_Id : Integer;
      Command     : Float;
   end record;

   procedure Print(Item : Control_Message);

end Messages;
```


The next portion contains the implementation of the procedures defined in the package specification.

```ada
with Ada.Text_Io; use Ada.Text_Io;
with Ada.Integer_Text_Io; use Ada.Integer_Text_Io;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

package body Messages is

   -----------
   -- Print --
   -----------

   procedure Print (Item : Message) is
      The_Year : Year_Number;
      The_Month : Month_Number;
      The_Day   : Day_Number;
      Seconds   : Day_Duration;
   begin
      Split(Date => Item.Timestamp, Year => The_Year,
         Month => The_Month, Day => The_Day, Seconds => Seconds);

      Put("Time Stamp:");
      Put(Item => The_Year, Width => 4);
      Put("-");
      Put(Item => The_Month, Width => 1);
      Put("-");
      Put(Item => The_Day, Width => 1);
      New_Line;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Item : Sensor_Message) is
   begin
      Print(Message(Item));
      Put("Sensor Id: ");
      Put(Item => Item.Sensor_Id, Width => 1);
      New_Line;
      Put("Reading: ");
      Put(Item => Item.Reading, Fore => 1, Aft => 4, Exp => 0);
      New_Line;
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print (Item : Control_Message) is
   begin
      Print(Message(Item));
      Put("Actuator Id: ");
      Put(Item => Item.Actuator_Id, Width => 1);
      New_Line;
      Put("Command: ");
      Put(Item => Item.Command, Fore => 1, Aft => 4, Exp => 0);
      New_Line;
   end Print;

   -------------
   ---Display --
   -------------

   procedure Display(Item : Message'Class) is
   begin
      Print(Item);
   end Display;

end Messages;
```


The final section of code creates objects of the three message types and performs the printing, writing, and reading. The Ada attributes '' 'Class'Output'' serialize the object and write it to the specified stream. The '' 'Class'Input'' attributes  call a function automatically provided by the compiler which reads from the specified stream file and returns the object read. The ''Display'' procedure takes an object in the inheritance tree rooted at ''Message'' and dispatches the correct print procedure.


```ada
with Messages; use Messages;
with Ada.Streams.Stream_Io; use Ada.Streams.Stream_Io;
with Ada.Calendar; use Ada.Calendar;
with Ada.Text_Io;

procedure Streams_Example is
   S1 : Sensor_Message;
   M1 : Message;
   C1 : Control_Message;
   Now : Time := Clock;
   The_File : Ada.Streams.Stream_Io.File_Type;
   The_Stream : Ada.Streams.Stream_IO.Stream_Access;
begin
   S1 := (Now, 1234, 0.025);
   M1.Timestamp := Now;
   C1 := (Now, 15, 0.334);
   Display(S1);
   Display(M1);
   Display(C1);
   begin
      Open(File => The_File, Mode => Out_File,
         Name => "Messages.dat");
   exception
      when others =>
         Create(File => The_File, Name => "Messages.dat");
   end;
   The_Stream := Stream(The_File);
   Sensor_Message'Class'Output(The_Stream, S1);
   Message'Class'Output(The_Stream, M1);
   Control_Message'Class'Output(The_Stream, C1);
   Close(The_File);
   Open(File => The_File, Mode => In_File,
      Name => "Messages.dat");
   The_Stream := Stream(The_File);
   Ada.Text_Io.New_Line(2);
   while not End_Of_File(The_File) loop
      Display(Message'Class'Input(The_Stream));
   end loop;
   Close(The_File);
end Streams_Example;
```

Output results:
 Time Stamp:2007-3-9
 Sensor Id: 1234
 Reading: 0.0250
 Time Stamp:2007-3-9
 Time Stamp:2007-3-9
 Actuator Id: 15
 Command: 0.3340


 Time Stamp:2007-3-9
 Sensor Id: 1234
 Reading: 0.0250
 Time Stamp:2007-3-9
 Time Stamp:2007-3-9
 Actuator Id: 15
 Command: 0.3340


## ALGOL 68

{{trans|python}}

Serialization in ''ALGOL 68'' is achieved through a technique called
''straightening''.

```algol68
MODE ENTITY = STRUCT([6]CHAR name, INT creation);
FORMAT entity repr = $"Name: "g", Created:"g$;
MODE PERSON = STRUCT(ENTITY entity, STRING email);
FORMAT person repr = $f(entity repr)", Email: "g$;

PERSON instance1 := PERSON(ENTITY("Cletus", 20080808), "test+1@localhost.localdomain");
print((name OF entity OF instance1, new line));

ENTITY instance2 := ENTITY("Entity",20111111);
print((name OF instance2, new line));

FILE target;
INT errno := open(target, "rows.dat", stand back channel); # open file #

#  Serialise #
put(target,(instance1, new line, instance2, new line));
printf(($"Serialised..."l$));

close(target); # flush file stream #
errno := open(target, "rows.dat", stand back channel); # load again #

# Unserialise #
PERSON i1;
ENTITY i2;
get(target,(i1, new line, i2, new line));
printf(($"Unserialised..."l$));

printf((person repr, i1, $l$));
printf((entity repr, i2, $l$))
```

'''flex'''ible length arrays (including '''string'''s), and tagged-'''union'''
types are problematic as the lengths of the arrays, and the ''tag'' of the
union is not stored in the '''file'''.  Sometimes a '''format''' can be manually created to handle
these lengths and tags.  Also note that ''ALGOL 68'' is strongly typed and
the type ('''mode''') of the objects not stored, but compiled into the
code itself.

Output:

```txt

Cletus
Entity
Serialised...
Unserialised...
Name: Cletus, Created:  +20080808, Email: test+1@localhost.localdomain
Name: Entity, Created:  +20111111

```



## C++

compiled with g++ -lboost_serialization serializationtest3.cpp -o serializationtest3


```cpp
#include <string>
#include <fstream>
#include <boost/serialization/string.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/serialization/base_object.hpp>
#include <iostream>

class Employee {
public :
   Employee( ) { }

   Employee ( const std::string &dep , const std::string &namen )
      : department( dep ) , name( namen ) {
	 my_id = count++ ;
      }

   std::string getName( ) const {
      return name ;
   }

   std::string getDepartment( ) const {
      return department ;
   }

   int getId( ) const {
      return my_id ;
   }

   void setDepartment( const std::string &dep ) {
      department.assign( dep ) ;
   }

   virtual void print( ) {
      std::cout << "Name: " << name << '\n' ;
      std::cout << "Id: " << my_id << '\n' ;
      std::cout << "Department: " << department << '\n' ;
   }

   virtual ~Employee( ) { }
   static int count ;
private :
   std::string name ;
   std::string department ;
   int my_id ;
   friend class boost::serialization::access ;

   template <class Archive>
      void serialize( Archive &ar, const unsigned int version ) {
	 ar & my_id ;
	 ar & name ;
	 ar & department ;
      }

} ;

class Worker : public Employee {
public :
   Worker( const std::string & dep, const std::string &namen ,
	 double hourlyPay ) : Employee( dep , namen ) , salary( hourlyPay) { }

   Worker( ) { }

   double getSalary( ) {
      return salary ;
   }

   void setSalary( double pay ) {
      if ( pay > 0 )
	 salary = pay ;
   }

   virtual void print( ) {
      Employee::print( ) ;
      std::cout << "wage per hour: " << salary << '\n' ;
   }
private :
   double salary ;
   friend class boost::serialization::access ;
   template <class Archive>
      void serialize ( Archive & ar, const unsigned int version ) {
	 ar & boost::serialization::base_object<Employee>( *this ) ;
	 ar & salary ;
      }
} ;

int Employee::count = 0 ;

int main( ) {
   std::ofstream storefile( "/home/ulrich/objects.dat"  ) ; //creating objects of base class
   const Employee emp1( "maintenance" , "Fritz Schmalstieg"  ) ;
   const Employee emp2( "maintenance" , "John Berry" ) ;
   const Employee emp3( "repair" , "Pawel Lichatschow" ) ;
   const Employee emp4( "IT" , "Marian Niculescu" ) ;
   const Worker worker1( "maintenance" , "Laurent Le Chef" , 20 ) ;//creating objects of derived class
   const Worker worker2 ( "IT" , "Srinivan Taraman" , 55.35 ) ;
   boost::archive::text_oarchive oar ( storefile ) ;//starting serialization into designated file
   oar << emp1 ;
   oar << emp2 ;
   oar << emp3 ;
   oar << emp4 ;
   oar << worker1 ;
   oar << worker2 ;
   storefile.close( ) ;
   std::cout << "Reading out the data again\n" ;
   Employee e1 , e2 , e3 , e4 ; //creating instances of base class objects for deserialization
   Worker w1, w2 ; // same for objects of derived class
   std::ifstream sourcefile( "/home/ulrich/objects.dat"  ) ;
   boost::archive::text_iarchive iar( sourcefile ) ;//starting deserialization
   iar >> e1 >> e2 >> e3 >> e4 ;
   iar >> w1 >> w2 ;
   sourcefile.close( ) ;
   std::cout << "And here are the data after deserialization!( abridged):\n" ;
   e1.print( ) ;
   e3.print( ) ;
   w2.print( ) ;
   return 0 ;
}
```

creating the following output:

```txt

Reading out the data again
And here are the data after deserialization!( abridged):
Name: Fritz Schmalstieg
Id: 0
Department: maintenance
Name: Pawel Lichatschow
Id: 2
Department: repair
Name: Srinivan Taraman
Id: 5
Department: IT
wage per hour: 55.35

```

## C#

```c#
using System;
using System.IO;
using System.Collections.Generic;
using System.Runtime.Serialization.Formatters.Binary;

namespace Object_serialization
{
  [Serializable] public class Being
  {
    public bool Alive { get; set; }
  }

  [Serializable] public class Animal: Being
  {
    public Animal() { }

    public Animal(long id, string name, bool alive = true)
    {
      Id = id;
      Name = name;
      Alive = alive;
    }

    public long Id { get; set; }
    public string Name { get; set; }

    public void Print() { Console.WriteLine("{0}, id={1} is {2}",
      Name, Id, Alive ? "alive" : "dead"); }
  }


  internal class Program
  {
    private static void Main()
    {
      string path =
        Environment.GetFolderPath(Environment.SpecialFolder.Desktop)+"\\objects.dat";

      var n = new List<Animal>
              {
                new Animal(1, "Fido"),
                new Animal(2, "Lupo"),
                new Animal(7, "Wanda"),
                new Animal(3, "Kiki", alive: false)
              };

      foreach(Animal animal in n)
        animal.Print();

      using(var stream = new FileStream(path, FileMode.Create, FileAccess.Write))
        new BinaryFormatter().Serialize(stream, n);

      n.Clear();
      Console.WriteLine("---------------");
      List<Animal> m;

      using(var stream = new FileStream(path, FileMode.Open, FileAccess.Read))
        m = (List<Animal>) new BinaryFormatter().Deserialize(stream);

      foreach(Animal animal in m)
        animal.Print();
    }
  }
}
```


```txt
Fido, id=1 is alive
Lupo, id=2 is alive
Wanda, id=7 is alive
Kiki, id=3 is dead
---------------
Fido, id=1 is alive
Lupo, id=2 is alive
Wanda, id=7 is alive
Kiki, id=3 is dead
```


=={{header|Caché ObjectScript}}==


```cos
Class Serialize.Employee Extends %SerialObject
{

Method %OnNew(ByRef pId As %Integer = 0, pDepartment As %String, pName As %String) As %Status
{
	Do ..IDSet(pId)
	Set pId=pId+1
	Do ..DepartmentSet(pDepartment)
	Do ..NameSet(pName)
	Quit $$$OK
}

Method Print()
{
	Write "[", ..%ClassName(), "]", !
	Write "- ID: "_..IDGet(), !
	Write "- Name: "_..NameGet(), !
	Write "- Department: "_..DepartmentGet(), !
	Quit
}

Property ID As %Integer [ Private ];
Property Name As %String [ Private ];
Property Department As %String [ Private ];

}
```



```cos
Class Serialize.Worker Extends Employee
{

Method %OnNew(ByRef pId As %Integer = 0, pDepartment As %String, pName As %String, pHourlyPay As %Numeric) As %Status
{
	Do ..HourlyPaySet(pHourlyPay)
	Quit ##super(.pId, pDepartment, pName)
}

Method Print()
{
	Do ##super()
	Write "- Hourly Pay: ", $FNumber(..HourlyPayGet(), ",", 2), !
	Quit
}

Method HourlyPaySet(pHourlyPay As %Numeric) As %Status [ ServerOnly = 1 ]
{
	Set i%HourlyPay=$Select(pHourlyPay<0: 0, 1: pHourlyPay)
	Quit $$$OK
}

Property HourlyPay As %Numeric [ Private ];

}
```



```cos
Class Serialize.Example Extends %SerialObject
{

ClassMethod Main()
{
	Do ..Save("/temp/objects.dat")
	Do ..Load("/temp/objects.dat")
	Quit
}

ClassMethod Save(pFilename As %String)
{
	// creating objects of base class
	Set emp1 = ##class(Employee).%New(.id, "Maintenance", "Fritz Schmalstieg")
	Set emp2 = ##class(Employee).%New(.id, "Maintenance", "John Berry")
	Set emp3 = ##class(Employee).%New(.id, "Repair", "Pawel Lichatschow")
	Set emp4 = ##class(Employee).%New(.id, "IT", "Marian Niculescu")

	// creating objects of derived class
	Set worker1 = ##class(Worker).%New(.id, "Maintenance", "Laurent Le Chef", 20)
	Set worker2 = ##class(Worker).%New(.id, "IT", "Srinivan Taraman", 55.35)

	// put objects into collections
	Set example = ..%New()
	Set sc = example.Employees.Insert(emp1)
	Set sc = example.Employees.Insert(emp2)
	Set sc = example.Employees.Insert(emp3)
	Set sc = example.Employees.Insert(emp4)
	Set sc = example.Workers.Insert(worker1)
	Set sc = example.Workers.Insert(worker2)

	// serialize the data and save to a file
	Set sc=example.%GetSwizzleObject(,.oid)
	Set fs=##class(%Stream.FileBinary).%New()
	Set fs.Filename=pFilename
	Set sc=fs.Write(oid)
	Set sc=fs.%Save()
	Quit
}

ClassMethod Load(pFilename As %String)
{
	// read serialized data from file
	Set fs=##class(%Stream.FileBinary).%New()
	Set fs.Filename=pFilename
	Set oid=fs.Read(.len, .sc)

	// open the example object
	Set example = ..%Open(oid,, .sc)
	Do example.Employees.GetAt(1).Print()
	Do example.Employees.GetAt(3).Print()
	Do example.Workers.GetAt(2).Print()
	Quit
}

Property Employees As list Of Employee;
Property Workers As list Of Worker;

}
```


{{out|Examples}}


```txt

USER>Do ##class(Serialize.Example).Main()
[Employee]
- ID: 0
- Name: Fritz Schmalstieg
- Department: Maintenance
[Employee]
- ID: 2
- Name: Pawel Lichatschow
- Department: Repair
[Worker]
- ID: 5
- Name: Srinivan Taraman
- Department: IT
- Hourly Pay: 55.35

```



## Common Lisp


{{libheader|cl-serializer}}


```lisp
(defmacro with-serialization-to-file ((stream pathname) &body body)
  `(with-open-file (,stream ,pathname
                            :element-type '(unsigned-byte 8)
                            :direction :output
                            :if-exists :supersede)
     ,@body))

(defclass entity ()
  ((name :initarg :name :initform "Some entity")))

(defclass person (entity)
  ((name :initarg :name :initform "The Nameless One")))
```


And now the REPL log:


```lisp
CL-USER> (list (make-instance 'entity)
               (make-instance 'person))

(#<ENTITY {1004B13141}> #<PERSON {1004B142B1}>)
CL-USER> (mapc #'describe *)
#<ENTITY {1004B13141}>
  [standard-object]

Slots with :INSTANCE allocation:
  NAME  = "Some entity"
#<PERSON {1004B142B1}>
  [standard-object]

Slots with :INSTANCE allocation:
  NAME  = "The Nameless One"
(#<ENTITY {1004B13141}> #<PERSON {1004B142B1}>)
CL-USER> (with-serialization-to-file (stream "/tmp/objects.dat")
           (cl-serializer:serialize * :output stream)
           ;; SERIALIZE shows an octet-vector as its return value
           (values))
; No value
CL-USER> (mapc #'describe (with-open-file (stream "/tmp/objects.dat"
                                                  :element-type '(unsigned-byte 8))
                            (cl-serializer:deserialize stream)))
#<ENTITY {1003C12911}>
  [standard-object]

Slots with :INSTANCE allocation:
  NAME  = "Some entity"
#<PERSON {1003C12A81}>
  [standard-object]

Slots with :INSTANCE allocation:
  NAME  = "The Nameless One"
(#<ENTITY {1003C12911}> #<PERSON {1003C12A81}>)
```



## D

{{libheader|ProtocolBuffer}}

If the requirement was not for binary, the [http://www.dsource.org/projects/doost doost library] would be a better fit for this task.
First, create a file named test.proto with the following contents:

```txt

package test;
message base1 {
	  required int32 i32 = 1;
}
message base2 {
	  repeated base1 rep = 1;
}

```

Run "pbcompiler test.proto" to generate the serializable code.  It should generate a D code file named test.d.
In your main file, use the following code:

```d
import test1;
import std.stdio;
import std.file;
class full2:base2 {
        this(byte[]manip,bool isroot=true) {super(manip,isroot);}
        this(){super();}
        void print() {
                foreach(item;rep) {
                        writefln(item.i32);
                }
        }
}

void main() {
        full2 base = new full2();
        base1 tmp = new base1;
        tmp.i32 = 34;
        base.add_rep(tmp);
        tmp = new base1;
        tmp.i32 = 32;
        base.add_rep(tmp);
        tmp = new base1;
        tmp.i32 = 33;
        base.add_rep(tmp);
        tmp = new base1;
        tmp.i32 = 36;
        base.add_rep(tmp);
        writefln("Input data:");
        base.print;
        write("objects.dat",base.Serialize());
        byte[]filedata = cast(byte[])read("objects.dat");
        base = new full2(filedata);
        writefln("Output data:");
        base.print;
}
```



## E


(Inheritance, while supported by various features and patterns, is not a preferred design component in [[E]]; nor are simple record data structures.)


```e
def makeEvent(time :int) {
    return def event {
        to __printOn(out) { out.print(`@@$time`) }
        to __optUncall() { return [makeEvent, "run", [time]] }
        to getTime() { return time }
    }
}

def makeArrival(time :int, what :any, position :int) {
    return def arrival extends makeEvent(time) {
        to __printOn(out) {
            out.print(`$what to $position $super`)
        }
        to __optUncall() {
            return [makeArrival, "run", [time, what, position]]
        }

        to getWhat() { return what }
        to getPosition() { return position }
    }
}
```


After defining our data types, we can prepare to serialize them.


```e>def surgeon := <import:org.erights.e.elib.serial.makeSurgeon
().diverge()
surgeon.addExit(makeEvent, "makeEvent")
surgeon.addExit(makeArrival, "makeArrival")
```


The 'exits' of the surgeon (so called because it cuts and joins object subgraphs) specify the points at which serialization should stop, instead replacing references to the objects with the specified names. On unserialization, the names are looked up and replaced with the corresponding objects. (The surgeon provides default exits for such things as <tt>false</tt>, <tt>true</tt>, <tt>null</tt>, and the list constructor.)


```e
def objs := [makeEvent(timer.now()),
             makeArrival(timer.now(), "Smith", 7)]

stdout.println(objs)
<file:objects.dat>.setBytes(surgeon.serialize(objs))
stdout.println(surgeon.unserialize(<file:objects.dat>.getBytes()))
```



## EchoLisp

Our classes will be 'struct' objects, with custom #:tostring procedures, as required. The instances are saved/restored to/from local storage. Serialization is performed by JSONifying the objects. References between instances of struct's are kept in the process. Auto-references and circular references are correctly maintained since EchoLisp version 2.11.

```lisp

(define (person->string self) (format "%a : person." (person-name self)))
(define (writer->string self) (format "%a:  writer of %a."
			(person-name self)
			(writer-books self)))
(define  (father->string self) (format "%a: father of %a."
			(person-name self)
			(map person-name (father-children self))))

; 'classes' definition, with inheritance.
; a writer is a person, too.
(struct person (name)  #:tostring person->string)
(struct writer person (books) #:tostring writer->string)
(struct father person (children)  #:tostring father->string)

(define simon (writer "Simon" '(my-life my-wife my-bike)))
(define elvis (person "Elvis"))
(define papa (father "papa" (list simon elvis)))

(local-put-value 'simon simon "objects.dat")
📕 local-db: local-put:unknown store : "objects.dat"
;; forgot to create the store. Create it :
(local-make-store "objects.dat") → "objects.dat"

(local-put-value 'simon simon "objects.dat")
(local-put-value 'elvis elvis "objects.dat")
(local-put-value 'papa papa   "objects.dat")

;; inspect
simon → Simon: writer of (my-life my-wife my-bike).
papa → papa: father of (Simon Elvis).
elvis → Elvis : person.

```

{{output}}

```lisp

;; reboot (close the browser window)
; inspect objects.dat :
(local-keys 'objects.dat) → ("elvis" "papa" "simon")

(define simon (local-get-value 'simon "objects.dat"))
(define elvis (local-get-value 'elvis "objects.dat"))
(define papa (local-get-value 'papa "objects.dat"))

; data are restored
simon → Simon: writer of (my-life my-wife my-bike).
papa → papa: father of (Simon Elvis).

;; check if references (pointers) are restored
(set-writer-name! simon "Antoinette") → "Antoinette"
simon→ Antoinette: writer of (my-life my-wife my-bike).

;; inspect
papa → papa: father of (Antoinette Elvis). ; YES 😳 !

;; - Self-referencing  (EchoLisp version 2.11)
;; add 'papa' to the chidren of 'papa' - whatever this means - and print it :
(set-father-children! papa (list simon papa elvis))
papa → papa: father of (Antoinette papa Elvis).

; save/restore
(local-put-value 'papa papa "objects.dat")
(define papa (local-get-value 'papa "objects.dat"))
papa → papa: father of (Antoinette papa Elvis).

```



## Erlang

Erlang is not object oriented. This code is based upon my understanding of the Algol 68 example above.

```Erlang

-module( object_serialization ).

-export( [task/0] ).

-record( entity, {name, date} ).
-record( person, {entity, email} ).

task() ->
	Person = #person{entity=#entity{name="Cletus", date=20080808}, email="test+1@localhost.localdomain"},
	print( Person ),
	Entity = #entity{name="Entity", date=20111111},
	print( Entity ),
	ok = file:write_file( "objects.dat", erlang:term_to_binary([Person, Entity]) ),
	{ok, Binary} =  file:read_file( "objects.dat" ),
	[New_person, New_entity] = erlang:binary_to_term( Binary ),
	io:fwrite( "Deserialized\n" ),
	print( New_person ),
	print( New_entity ).



print( #entity{name=Name, date=Date} ) ->
	io:fwrite( "Entity: " ),
	io:fwrite( "name: ~p, date: ~p~n", [Name, Date] );
print( #person{entity=Entity, email=Email} ) ->
	io:fwrite( "Person: " ),
	print( Entity ),
	io:fwrite( "\temail: ~p~n", [Email] ).

```

{{out}}

```txt

<9> object_serialization:task().
Person: Entity: name: "Cletus", date: 20080808
        email: "test+1@localhost.localdomain"
Entity: name: "Entity", date: 20111111
Deserialized
Person: Entity: name: "Cletus", date: 20080808
        email: "test+1@localhost.localdomain"
Entity: name: "Entity", date: 20111111

```



## Factor

The <code>serialize</code> vocabulary provides words for serializing and deserializing any Factor object other than continuations. This example demonstrates that objects may be serialized individually, one at a time. In practice, it's probably easier to serialize a collection of objects.

```factor
USING: accessors combinators.extras io io.encodings.binary
io.files io.files.info kernel prettyprint serialize ;
IN: rosetta-code.object-serialization

! Define two classes, item and armor. armor is a subclass of
! item.

TUPLE: item name value ;
TUPLE: armor < item physical-resistance fire-resistance ;

! Define boa constructors for both classes using C: shorthand.
! boa means By Order of Arguments, and yes, this is a pun on boa
! constrictors.

C: <item> item
C: <armor> armor

! Create three example items and print them out
! non-destructively.

"Fish scales" 0.05 <item>
"Gold piece" 1 <item>
"Breastplate of Ashannar" 50,000 55 30 <armor>
[ [ . ] keep ] tri@ nl

! Serialize the three objects to a binary file named
! objects.dat.

"Serializing objects to objects.dat . . . " print
"objects.dat" binary [ [ serialize ] tri@ ] with-file-writer

! Check that objects.dat exists.

"objects.dat exists? " write "objects.dat" exists? .
"Size on disk: " write "objects.dat" file-info size>> pprint
" bytes" print nl

! Deserialize three objects from objects.dat.

"Deserializing objects from objects.dat . . . " print nl
"objects.dat" binary [ [ deserialize ] thrice ] with-file-reader

! Print out deserialized objects.

[ . ] tri@
```

{{out}}

```txt

T{ item { name "Fish scales" } { value 0.05 } }
T{ item { name "Gold piece" } { value 1 } }
T{ armor
    { name "Breastplate of Ashannar" }
    { value 50000 }
    { physical-resistance 55 }
    { fire-resistance 30 }
}

Serializing objects to objects.dat . . .
objects.dat exists? t
Size on disk: 206 bytes

Deserializing objects from objects.dat . . .

T{ item { name "Fish scales" } { value 0.05 } }
T{ item { name "Gold piece" } { value 1 } }
T{ armor
    { name "Breastplate of Ashannar" }
    { value 50000 }
    { physical-resistance 55 }
    { fire-resistance 30 }
}

```



## Go

Go has a few choices for serialization.  The method shown here is the native method, which is compact and type-aware.

A note on object oriented stuff, the object hierarchy required by the task description is implemented here with embedded structs.  The polymorphism required by the task description is handled nicely with an interface.  The functional polymorphism is orthogonal to the object hierarchy, not conflated with it.

```go
package main

import (
    "encoding/gob"
    "fmt"
    "os"
)

type printable interface {
    print()
}

func main() {
    // create instances
    animals := []printable{
        &Animal{Alive: true},
        &Cat{},
        &Lab{
            Dog:   Dog{Animal: Animal{Alive: true}},
            Color: "yellow",
        },
        &Collie{Dog: Dog{
            Animal:           Animal{Alive: true},
            ObedienceTrained: true,
        }},
    }

    // display
    fmt.Println("created:")
    for _, a := range animals {
        a.print()
    }

    // serialize
    f, err := os.Create("objects.dat")
    if err != nil {
        fmt.Println(err)
        return
    }
    for _, a := range animals {
        gob.Register(a)
    }
    err = gob.NewEncoder(f).Encode(animals)
    if err != nil {
        fmt.Println(err)
        return
    }
    f.Close()

    // read
    f, err = os.Open("objects.dat")
    if err != nil {
        fmt.Println(err)
        return
    }
    var clones []printable
    gob.NewDecoder(f).Decode(&clones)
    if err != nil {
        fmt.Println(err)
        return
    }

    // display
    fmt.Println("\nloaded from objects.dat:")
    for _, c := range clones {
        c.print()
    }
}

type Animal struct {
    Alive bool
}

func (a *Animal) print() {
    if a.Alive {
        fmt.Println("   live animal, unspecified type")
    } else {
        fmt.Println("   dead animal, unspecified type")
    }
}

type Dog struct {
    Animal
    ObedienceTrained bool
}

func (d *Dog) print() {
    switch {
    case !d.Alive:
        fmt.Println("   dead dog")
    case d.ObedienceTrained:
        fmt.Println("   trained dog")
    default:
        fmt.Println("   dog, not trained")
    }
}

type Cat struct {
    Animal
    LitterBoxTrained bool
}

func (c *Cat) print() {
    switch {
    case !c.Alive:
        fmt.Println("   dead cat")
    case c.LitterBoxTrained:
        fmt.Println("   litter box trained cat")
    default:
        fmt.Println("   cat, not litter box trained")
    }
}

type Lab struct {
    Dog
    Color string
}

func (l *Lab) print() {
    var r string
    if l.Color == "" {
        r = "lab, color unspecified"
    } else {
        r = l.Color + " lab"
    }
    switch {
    case !l.Alive:
        fmt.Println("   dead", r)
    case l.ObedienceTrained:
        fmt.Println("   trained", r)
    default:
        fmt.Printf("   %s, not trained\n", r)
    }
}

type Collie struct {
    Dog
    CatchesFrisbee bool
}

func (c *Collie) print() {
    switch {
    case !c.Alive:
        fmt.Println("   dead collie")
    case c.ObedienceTrained && c.CatchesFrisbee:
        fmt.Println("   trained collie, catches frisbee")
    case c.ObedienceTrained && !c.CatchesFrisbee:
        fmt.Println("   trained collie, but doesn't catch frisbee")
    case !c.ObedienceTrained && c.CatchesFrisbee:
        fmt.Println("   collie, not trained, but catches frisbee")
    case !c.ObedienceTrained && !c.CatchesFrisbee:
        fmt.Println("   collie, not trained, doesn't catch frisbee")
    }
}
```

Output:

```txt
created:
   live animal, unspecified type
   dead cat
   yellow lab, not trained
   trained collie, but doesn't catch frisbee

loaded from objects.dat:
   live animal, unspecified type
   dead cat
   yellow lab, not trained
   trained collie, but doesn't catch frisbee
```



## Groovy

Sample Serializable Classes (borrowed from Java example. Sorta.):

```groovy
class Entity implements Serializable {
    static final serialVersionUID = 3504465751164822571L
    String name = 'Thingamabob'
    public String toString() { return name }
}

class Person extends Entity implements Serializable {
    static final serialVersionUID = -9170445713373959735L
    Person() { name = 'Clement' }
    Person(name) { this.name = name }
}
```


Writing objects:

```groovy
File objectStore = new File('objectStore.ser')
if (objectStore.exists()) { objectStore.delete() }
assert ! objectStore.exists()
def os
try {
    os = objectStore.newObjectOutputStream()
    os << new Person()
    os << 10.5
    os << new Person('Cletus')
    os << new Date()
    os << new Person('Pious')
    os << java.awt.Color.RED
    os << new Person('Linus')
    os << 'just random garbage'
    os << new Person('Lucy')
    os << ['lists', 'are', 'serializable']
    os << new Person('Schroeder')
} catch (e) { throw new Exception(e) } finally { os?.close() }
assert objectStore.exists()
```


Reading objects:

```groovy
def is
try {
    is = objectStore.newObjectInputStream(this.class.classLoader)
    is.eachObject { println it }
} catch (e) { throw new Exception(e) } finally { is?.close() }

objectStore.delete()
assert ! objectStore.exists()
```


Output:

```txt
Clement
10.5
Cletus
Wed Jan 18 02:07:29 CST 2012
Pious
java.awt.Color[r=255,g=0,b=0]
Linus
just random garbage
Lucy
[lists, are, serializable]
Schroeder
```



## Haskell


Example uses [https://hackage.haskell.org/package/binary <tt>binary</tt>] package. Since Haskell doesn't directly support OO-style inheritance, we use a sum type instead:


```haskell
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import qualified Data.ByteString.Lazy as ByteString (readFile, writeFile)
import           Data.Binary (Binary)
import qualified Data.Binary as Binary (decode, encode)
import           GHC.Generics (Generic)

data Employee =
    Manager String String
    | IndividualContributor String String
    deriving (Generic, Show)
instance Binary Employee

main :: IO ()
main = do
    ByteString.writeFile "objects.dat" $ Binary.encode
        [ IndividualContributor "John Doe" "Sales"
        , Manager "Jane Doe" "Engineering"
        ]

    bytes <- ByteString.readFile "objects.dat"
    let employees = Binary.decode bytes
    print (employees :: [Employee])
```



## J


This should be sufficient for this task:


```j
lin_z_=:5!:5
serializeObject=:3 :0
  p=. copath y
  d=. ;LF;"1(,'=:';lin__y)"0 nl__y i.4
  '(',(5!:5<'p'),')(copath[cocurrent@])cocreate ''''',,d,LF
)

deserializeObject=:3 :0
  o=.conl 1
  0!:100 y
  (conl 1)-.o
)

coclass'room'
  create=:3 :'size=:y'
  print=:3 :'''room size '',":size'

coclass'kitchen'
  coinsert'room'
  print=:3 :'''kitchen size '',":size'

coclass'kitchenWithSink'
  coinsert'kitchen'
  print=:3 :'''kitchen with sink size '',":size'

cocurrent'base'

R=:'small' conew 'room'
K=:'medium' conew 'kitchen'
S=:'large' conew 'kitchenWithSink'
print__R''
print__K''
print__S''


(;<@serializeObject"0 R,K,S) 1!:2 <'objects.dat'

'r1 k1 s1'=: <"0 deserializeObject 1!:1<'objects.dat'
print__r1''
print__k1''
print__s1''
```


Here is how the last part looks in action:


```j
   print__R''
room size small
   print__K''
kitchen size medium
   print__S''
kitchen with sink size large


   (;<@serializeObject"0 R,K,S) 1!:2 <'objects.dat'

   'r1 k1 s1'=: <"0 deserializeObject 1!:1<'objects.dat'
   print__r1''
room size small
   print__k1''
kitchen size medium
   print__s1''
kitchen with sink size large
```


Note also that J does not attempt to distinguish, at the language level, between an object reference and something that looks like an object reference but is not.  This must be done at the application level, which in turn can create a variety of opportunities and/or burdens for the program designer.

(And this is more than adequate for the use case J's object system was designed for, which is to provide a mechanism where independent programmers can work on different modules of a program with well defined interfaces. Of course, if you try to make it complicated, or try to design something with incomprehensible interfaces, that will create problems.)


## Java


```java
import java.io.*;

// classes must implement java.io.Serializable in order to be serializable
class Entity implements Serializable {
    // it is recommended to hard-code serialVersionUID so changes to class
    // will not invalidate previously serialized objects
    static final long serialVersionUID = 3504465751164822571L;
    String name = "Entity";
    public String toString() { return name; }
}

class Person extends Entity implements Serializable {
    static final long serialVersionUID = -9170445713373959735L;
    Person() { name = "Cletus"; }
}

public class SerializationTest {
    public static void main(String[] args) {
        Person instance1 = new Person();
        System.out.println(instance1);

        Entity instance2 = new Entity();
        System.out.println(instance2);

        // Serialize
        try {
            ObjectOutput out = new ObjectOutputStream(new FileOutputStream("objects.dat")); // open ObjectOutputStream

            out.writeObject(instance1); // serialize "instance1" and "instance2" to "out"
            out.writeObject(instance2);
            out.close();
            System.out.println("Serialized...");
        } catch (IOException e) {
            System.err.println("Something screwed up while serializing");
            e.printStackTrace();
            System.exit(1);
        }

        // Deserialize
        try {
            ObjectInput in = new ObjectInputStream(new FileInputStream("objects.dat")); // open ObjectInputStream

            Object readObject1 = in.readObject(); // read two objects from "in"
            Object readObject2 = in.readObject(); // you may want to cast them to the appropriate types
            in.close();
            System.out.println("Deserialized...");

            System.out.println(readObject1);
            System.out.println(readObject2);
        } catch (IOException e) {
            System.err.println("Something screwed up while deserializing");
            e.printStackTrace();
            System.exit(1);
        } catch (ClassNotFoundException e) {
            System.err.println("Unknown class for deserialized object");
            e.printStackTrace();
            System.exit(1);
        }
    }
}
```



## Julia


```julia

abstract type Hello end

struct HelloWorld <: Hello
    name::String
    HelloWorld(s) = new(s)
end

struct HelloTime <: Hello
    name::String
    tnew::DateTime
    HelloTime(s) = new(s, now())
end

sayhello(hlo) = println("Hello to this world, $(hlo.name)!")

sayhello(hlo::HelloTime) = println("It is now $(now()). Hello from back in $(hlo.tnew), $(hlo.name)!")

h1 = HelloWorld("world")
h2 = HelloTime("new world")

sayhello(h1)
sayhello(h2)

fh = open("objects.dat", "w")
serialize(fh, h1)
serialize(fh,h2)
close(fh)

sleep(10)

fh = open("objects.dat", "r")
hh1 = deserialize(fh)
hh2 = deserialize(fh)
close(fh)

sayhello(hh1)
sayhello(hh2)

```



## Kotlin

{{trans|Java}}

```scala
// version 1.2.0

import java.io.*

open class Entity(val name: String = "Entity"): Serializable {
    override fun toString() = name

    companion object {
        val serialVersionUID = 3504465751164822571L
    }
}

class Person(name: String = "Brian"): Entity(name), Serializable {
    companion object {
        val serialVersionUID = -9170445713373959735L
    }
}

fun main(args: Array<String>) {
    val instance1 = Person()
    println(instance1)

    val instance2 = Entity()
    println(instance2)

    // serialize
    try {
        val out = ObjectOutputStream(FileOutputStream("objects.dat"))
        out.writeObject(instance1)
        out.writeObject(instance2)
        out.close()
        println("Serialized...")
    }
    catch (e: IOException) {
        println("Error occurred whilst serializing")
        System.exit(1)
    }

    // deserialize
    try {
        val inp = ObjectInputStream(FileInputStream("objects.dat"))
        val readObject1 = inp.readObject()
        val readObject2 = inp.readObject()
        inp.close()
        println("Deserialized...")
        println(readObject1)
        println(readObject2)
    }
    catch (e: IOException) {
        println("Error occurred whilst deserializing")
        System.exit(1)
    }
    catch (e: ClassNotFoundException) {
        println("Unknown class for deserialized object")
        System.exit(1)
    }
}
```


{{out}}

```txt

Brian
Entity
Serialized...
Deserialized...
Brian
Entity

```



## Neko


```actionscript
/* Object serialization, in Neko */

var file_open = $loader.loadprim("std@file_open", 2)
var file_write = $loader.loadprim("std@file_write", 4)
var file_read = $loader.loadprim("std@file_read", 4)
var file_close = $loader.loadprim("std@file_close", 1)

var serialize = $loader.loadprim("std@serialize", 1)
var unserialize = $loader.loadprim("std@unserialize", 2)

/* Inheritance by prototype */
proto = $new(null)
proto.print = function () { $print(this, "\n") }

obj = $new(null)
obj.msg = "Hello"
obj.dest = $array("Town", "Country", "World")

$objsetproto(obj, proto)
$print("Original:\n")
obj.print()

/* Serialize the object */
var thing = serialize(obj)
var len = $ssize(thing)

/* To disk */
var f = file_open("object-serialization.bin", "w")
file_write(f, thing, 0, len)
file_close(f)

/* Load the binary data into a new string space */
f = file_open("object-serialization.bin", "r")
var buff = $smake(len)
file_read(f, buff, 0, len)
file_close(f)

/* Unserialize the object into a new variable */
var other = unserialize(buff, $loader)
$print("deserialized:\n")
other.print()
```


{{out}}

```txt
prompt$ nekoc object-serialization.neko
prompt$ neko object-serialization
Original:
{ dest => [Town,Country,World], msg => Hello }
deserialized:
{ dest => [Town,Country,World], msg => Hello }
prompt$ xxd object-serialization.bin
00000000: 6f62 5e66 c261 0300 0000 7304 0000 0054  ob^f.a....s....T
00000010: 6f77 6e73 0700 0000 436f 756e 7472 7973  owns....Countrys
00000020: 0500 0000 576f 726c 6441 1a53 0073 0500  ....WorldA.S.s..
00000030: 0000 4865 6c6c 6f00 0000 0070 6f2d 588b  ..Hello....po-X.
00000040: c84c 7314 0000 006f 626a 6563 742d 7365  .Ls....object-se
00000050: 7269 616c 697a 6174 696f 6e02 0000 0000  rialization.....
00000060: 0000 0061 0000 0000 0000 0000 7a         ...a........z
```


=={{header|Objective-C}}==

{{works with|GNUstep}}
{{works with|Cocoa}}

About Cocoa, I can't test it, but I've used Apple's documentation to learn how to do it (see
[http://developer.apple.com/DOCUMENTATION/Cocoa/Conceptual/Archiving/Archiving.html#//apple_ref/doc/uid/10000047 here]; serializing or marshalling is rather known in Obj-C world as ''archiving'').

There exists also a way of serializing without the GNUstep/Cocoa framework, using the runtime of Objective-C (so it could be slightly implementation dependent, see [[wp:Serialization#Objective-C|Serialization on Wikipedia]]). (I will work on it and will put here a full working example compatible with the task).


```objc>#import <Foundation/Foundation.h


// a fantasy two level hierarchy
@interface Animal : NSObject <NSCoding>
{
  NSString *animalName;
  int numberOfLegs;
}
- (instancetype) initWithName: (NSString*)name andLegs: (NSInteger)legs;
- (void) dump;
@end

@implementation Animal
- (instancetype) initWithName: (NSString*)name andLegs: (NSInteger)legs
{
  if ((self = [super init])) {
    animalName = name;
    numberOfLegs = legs;
  }
  return self;
}
- (void) dump
{
  NSLog(@"%@ has %d legs", animalName, numberOfLegs);
}
//
### ==

- (void) encodeWithCoder: (NSCoder*)coder
{
  [coder encodeObject: animalName forKey: @"Animal.name"];
  [coder encodeInt: numberOfLegs forKey: @"Animal.legs"];
}
- (instancetype) initWithCoder: (NSCoder*)coder
{
  if ((self = [super init])) {
    animalName = [coder decodeObjectForKey: @"Animal.name"];
    numberOfLegs = [coder decodeIntForKey: @"Animal.legs"];
  }
  return self;
}
@end

@interface Mammal : Animal <NSCoding>
{
  BOOL hasFur;
  NSMutableArray *eatenList;
}
- (instancetype) initWithName: (NSString*)name hasFur: (BOOL)fur;
- (void) addEatenThing: (NSString*)thing;
@end

@implementation Mammal
- (instancetype) init
{
  if ((self = [super init])) {
    hasFur = NO;
    eatenList = [[NSMutableArray alloc] initWithCapacity: 10];
  }
  return self;
}
- (instancetype) initWithName: (NSString*)name hasFur: (BOOL)fur
{
  if ((self = [super initWithName: name andLegs: 4])) {
    hasFur = fur;
    eatenList = [[NSMutableArray alloc] initWithCapacity: 10];
  }
  return self;
}
- (void) addEatenThing: (NSString*)thing
{
  [eatenList addObject: thing];
}
- (void) dump
{
  [super dump];
  NSLog(@"has fur? %@", (hasFur) ? @"yes" : @"no" );
  NSLog(@"it has eaten %d things:", [eatenList count]);
  for ( id element in eatenList )
    NSLog(@"it has eaten a %@", element);
  NSLog(@"end of eaten things list");
}
//
### ===
 de/archiving
- (void) encodeWithCoder: (NSCoder*)coder
{
  [super encodeWithCoder: coder];
  [coder encodeBool: numberOfLegs forKey: @"Mammal.hasFur"];
  [coder encodeObject: eatenList forKey: @"Mammal.eaten"];
}
- (instancetype) initWithCoder: (NSCoder*)coder
{
  if ((self = [super initWithCoder: coder])) {
    hasFur = [coder decodeBoolForKey: @"Mammal.hasFur"];
    eatenList = [coder decodeObjectForKey: @"Mammal.eaten"];
  }
  return self;
}
@end


int main()
{
  @autoreleasepool {

    // let us create a fantasy animal
    Animal *anAnimal = [[Animal alloc]
	         initWithName: @"Eptohippos"
	         andLegs: 7
	        ];
    // for some reason an Eptohippos is not an horse with 7 legs,
    // and it is not a mammal, of course...

    // let us create a fantasy mammal (which is an animal too)
    Mammal *aMammal = [[Mammal alloc]
	        initWithName: @"Mammaluc"
	        hasFur: YES
	       ];
    // let us add some eaten stuff...
    [aMammal addEatenThing: @"lamb"];
    [aMammal addEatenThing: @"table"];
    [aMammal addEatenThing: @"web page"];

    // dump anAnimal
    NSLog(@"----- original Animal -----");
    [anAnimal dump];

    // dump aMammal...
    NSLog(@"----- original Mammal -----");
    [aMammal dump];

    // now let us store the objects...
    NSMutableData *data = [[NSMutableData alloc] init];
    NSKeyedArchiver *arch = [[NSKeyedArchiver alloc]
			      initForWritingWithMutableData: data];
    [arch encodeObject: anAnimal forKey: @"Eptohippos"];
    [arch encodeObject: aMammal forKey: @"Mammaluc"];
    [arch finishEncoding];
    [data writeToFile: @"objects.dat" atomically: YES];

    // now we want to retrieve the saved objects...
    NSData *ldata = [[NSData alloc]
		       initWithContentsOfFile: @"objects.dat"];
    NSKeyedUnarchived *darch = [[NSKeyedUnarchiver alloc]
	                         initForReadingWithData: ldata];
    Animal *archivedAnimal = [darch decodeObjectForKey: @"Eptohippos"];
    Mammal *archivedMammal = [darch decodeObjectForKey: @"Mammaluc"];
    [darch finishDecoding];

    // now let's dump/print the objects...
    NSLog(@"\n");
    NSLog(@"----- the archived Animal -----");
    [archivedAnimal dump];
    NSLog(@"----- the archived Mammal -----");
    [archivedMammal dump];

  }
  return EXIT_SUCCESS;
}
```



## Objeck


```objeck

bundle Default {
  class Thingy {
    @id : Int;

    New(id : Int) {
      @id := id;
    }

    method : public : Print() ~ Nil {
      @id->PrintLine();
    }
  }

  class Person from Thingy {
    @name : String;

    New(id : Int, name : String) {
      Parent(id);
      @name := name;
    }

    method : public : Print() ~ Nil {
      @id->PrintLine();
      @name->PrintLine();
    }
  }

  class Serial {
    function : Main(args : String[]) ~ Nil {
      t := Thingy->New(7);
      p := Person->New(13, "Bush");

      s := IO.Serializer->New();
      s->Write(t->As(Base));
      s->Write(p->As(Base));

      writer := IO.FileWriter->New("objects.dat");
      writer->WriteBuffer(s->Serialize());
      writer->Close();

      buffer := IO.FileReader->ReadBinaryFile("objects.dat");
      d := IO.Deserializer->New(buffer);

      t2 := d->ReadObject()->As(Thingy);
      t2->Print();
      p2 := d->ReadObject()->As(Person);
      p2->Print();
    }
  }
}

```



## OCaml


Objects which contain methods are difficult to serialize because it will want to serialize those methods too, but functions usually cannot be serialized. Instead, here we perform the task on non-object datatypes, with an outside function to print them.


```ocaml
type entity = { name : string }

let create_entity () = { name = "Entity" }
let print_entity x = print_endline x.name
let create_person () = { name = "Cletus" }

let instance1 = create_person ()
let instance2 = create_entity ()

(* Serialize *)
let out_chan = open_out_bin "objects.dat";;
output_value out_chan instance1;;
output_value out_chan instance2;;
close_out out_chan;;

(* Deserialize *)
let in_chan = open_in_bin "objects.dat";;
let result1 : entity = input_value in_chan;;
let result2 : entity = input_value in_chan;;
close_in in_chan;;

print_entity result1;;
print_entity result2;;
```


The module which provides functions to encode arbitrary data structures as sequences of bytes is [http://caml.inria.fr/pub/docs/manual-ocaml/libref/Marshal.html the module Marshal].


## Ol

All objects (values and references) can be serialized using `fasl-save` (by `fasl-encode`) and deserialized using `fasl-load` (by `fasl-decode`).


```scheme

$ ol
Welcome to Otus Lisp 1.2,
type ',help' to help, ',quit' to end session.
> (define Object (tuple
   '(1 2 3 4)  ; list
   #(4 3 2 1)  ; bytevector
   "hello"     ; ansi string
   "こんにちは"  ; unicode string
   (list->ff '(; associative array
      (1 . 123456)
      (2 . second)
      (3 . "-th-")))
   {(4 . 'sym) ; alternatively declared..
    (5 . +)}   ; ..associative array
   #false      ; value
   -123        ; short number
   123456789012345678901234567890123456789  ; long number
)
;; Defined Object
#((1 2 3 4) #(4 3 2 1) hello こんにちは #ff((1 . 123456) (2 . second) (3 . -th-))
#ff((4 . sym) (5 . #<function>)) #false -123
123456789012345678901234567890123456789)

> (fasl-save Object "/tmp/object.bin")
#true

> (define New (fasl-load "/tmp/object.bin" #false))
;; Defined New
#((1 2 3 4) #(4 3 2 1) hello こんにちは #ff((1 . 123456) (2 . second) (3 . -th-))
#ff((4 . sym) (5 . #<function>)) #false -123
123456789012345678901234567890123456789)

> (equal? Object New)
#true

> ,quit
bye-bye :/
$

```



## Oz

Stateless values can easily be serialized with functions from the [https://mozart.github.io/mozart-v1/doc-1.4.0/system/node57.html Pickle] module. Objects are not stateless, though.

Some objects can be converted to a stateless chunk by using [https://mozart.github.io/mozart-v1/doc-1.4.0/system/node96.html#section.objectsupport.reflect ObjectSupport.reflect]. For technical reasons, this will only work for a small subset of classes.

For a general solution, see [[Object Serialization/Oz]].


## Perl

{{libheader|Storable}}

```perl
{
    package Greeting;
    sub new {
        my $v = "Hello world!\n";
        bless \$v, shift;
    };
    sub stringify {
        ${shift()};
    };
};
{
    package Son::of::Greeting;
    use base qw(Greeting); # inherit methods
    sub new { # overwrite method of super class
        my $v = "Hello world from Junior!\n";
        bless \$v, shift;
    };
};
{
    use Storable qw(store retrieve);
    package main;
    my $g1 = Greeting->new;
    my $s1 = Son::of::Greeting->new;
    print $g1->stringify;
    print $s1->stringify;

    store $g1, 'objects.dat';
    my $g2 = retrieve 'objects.dat';

    store $s1, 'objects.dat';
    my $s2 = retrieve 'objects.dat';

    print $g2->stringify;
    print $s2->stringify;
};
```


{{libheader|MooseX}}

The same, using [[MooseX]] to serialize to [[uses format::JSON]].

```perl
use MooseX::Declare;

class Greeting {
    use MooseX::Storage;
    with Storage('format' => 'JSON', io => 'File');
    has string => (is => 'ro', default => "Hello world!\n");
}
class Son::Of::Greeting extends Greeting {
    has string => (is => 'ro', default => "Hello from Junior!\n");
}

my $g1 = Greeting->new;
my $s1 = Son::Of::Greeting->new;

print $g1->string;
print $s1->string;

$g1->store('object1.json');
my $g2 = Greeting->load('object1.json');

$s1->store('object2.json');
my $s2 = Son::Of::Greeting->load('object2.json');

print $g2->string;
print $s2->string;

```

This time the objects were serialized to the [http://www.json.org/ JSON] format. Other supported formats are [http://search.cpan.org/perldoc?Storable Storable] and [http://www.yaml.org/ YAML].


## Perl 6


```perl6
#!/usr/bin/env perl6

# Reference:
# https://docs.perl6.org/language/classtut
# https://github.com/teodozjan/perl-store

use v6;
use PerlStore::FileStore;

class Point {
    has Int $.x;
    has Int $.y;
}

class Rectangle does  FileStore {
    has Point $.lower;
    has Point $.upper;

    method area() returns Int {
        ($!upper.x - $!lower.x) * ( $!upper.y - $!lower.y);
    }
}

my $r1 = Rectangle.new(lower => Point.new(x => 0, y => 0),
                      upper => Point.new(x => 10, y => 10));
say "Create Rectangle1 with area ",$r1.area();
say "Serialize Rectangle1 to object.dat";
$r1.to_file('./objects.dat');
say "";
say "take a peek on object.dat ..";
say slurp "./objects.dat";
say "";
say "Deserialize to Rectangle2";
my $r2 = from_file('objects.dat');
say "Rectangle2 is of type ", $r2.WHAT;
say "Rectangle2 area is ", $r2.area();
```

{{out}}

```txt
Create Rectangle1 with area 100
Serialize Rectangle1 to object.dat

take a peek on object.dat ..
Rectangle.new(lower => Point.new(x => 0, y => 0), upper => Point.new(x => 10, y => 10))


Deserialize to Rectangle2
Rectangle2 is of type (Rectangle)
Rectangle2 area is 100
```



## Phix

The serialize() and deserialize() functions handle any kind of data. Whether they are binary, text, data types, classes,
or whatever you choose to treat as such, is up to you.

```Phix
include builtins\serialize.e

function randobj()
-- test function (generate some random garbage)
    object res
    if rand(10)<=3 then     -- make sequence[1..3]
        res = {}
        for i=1 to rand(3) do
            res = append(res,randobj())
        end for
    elsif rand(10)<=3 then  -- make string
        res = repeat('A'+rand(10),rand(10))
    else
        res = rand(10)/2    -- half int/half float
    end if
    return res
end function

object o1 = randobj(),
       o2 = randobj(),
       o3 = randobj()

pp({o1,o2,o3},{pp_Nest,1})
integer fh = open("objects.dat", "wb")
puts(fh, serialize(o1))
puts(fh, serialize(o2))
puts(fh, serialize(o3))
close(fh)

?"==="

fh = open("objects.dat", "rb")
?deserialize(fh)
?deserialize(fh)
?deserialize(fh)
close(fh)
{} = delete_file("objects.dat")
```

{{out}}

```txt

{1.5,
 {"JJJJJJJJ", "FFFF", {4}},
 {{{0.5}, 3}, 3,1}}
"==="
1.5
{"JJJJJJJJ","FFFF",{4}}
{{{0.5},3},3,1}

```



## PHP

Serialization in PHP is straightforward. The built-in function [http://www.php.net/manual/en/function.serialize.php serialize()] handles it in a single statement.

```php
$myObj = new Object();
$serializedObj = serialize($myObj);
```

In order to un-serialize the object, use the [http://www.php.net/manual/en/function.unserialize.php unserialize()] function. Note that the class of object must be defined in the script where un-serialization takes place, or the class' methods will be lost.


## PicoLisp

The built-in function [http://software-lab.de/doc/refP.html#pr pr] serializes
any kind of data, and [http://software-lab.de/doc/refR.html#rd rd] reads it
back. This functionality is also used internally for database access and
interprocess-communication.

```PicoLisp
(class +Point)
# x y

(dm T (X Y)
   (=: x (or X 0))
   (=: y (or Y 0)) )

(dm print> ()
   (prinl "Point " (: x) "," (: y)) )

(class +Circle +Point)
# r

(dm T (X Y R)
   (super X Y)
   (=: r (or R 0)) )

(dm print> ()
   (prinl "Circle " (: x) "," (: y) "," (: r)) )

(setq
   P (new '(+Point) 3 4)
   C (new '(+Circle) 10 10 5) )

(print> P)
(print> C)

(out "objects.dat"
   (pr (val P) (getl P))
   (pr (val C) (getl C)) )
```


```PicoLisp
(in "objects.dat"
   (putl (setq A (box (rd))) (rd))
   (putl (setq B (box (rd))) (rd)) )

(print> A)
(print> B)
```

Output:

```txt
Point 3,4
Circle 10,10,5
Point 3,4
Circle 10,10,5
```



## Python


```python
# Object Serialization in Python
# serialization in python is accomplished via the Pickle module.
# Alternatively, one can use the cPickle module if speed is the key,
# everything else in this example remains the same.

import pickle

class Entity:
	def __init__(self):
		self.name = "Entity"
	def printName(self):
		print self.name

class Person(Entity): #OldMan inherits from Entity
	def __init__(self): #override constructor
		self.name = "Cletus"

instance1 = Person()
instance1.printName()

instance2 = Entity()
instance2.printName()

target = file("objects.dat", "w") # open file

#  Serialize
pickle.dump((instance1, instance2), target) # serialize `instance1` and `instance2`to `target`
target.close() # flush file stream
print "Serialized..."

# Unserialize
target = file("objects.dat") # load again
i1, i2 = pickle.load(target)
print "Unserialized..."

i1.printName()
i2.printName()
```



## Racket


Serialization is described in the Racket documentation in: [http://docs.racket-lang.org/reference/serialization.html?q=serialize Serialization], and more specifically with respect to object oriented programming classes: [http://docs.racket-lang.org/reference/objectserialize.html?q=serializable-class#%28form._%28%28lib._racket%2Fprivate%2Fclass-internal..rkt%29._define-serializable-class%29%29 Object Serialization].

The serialization needs to be included with

```racket
(require racket/serialize)
```

The rest is covered by the Racket language.

(I have elided the paths in objects.dat -- you wouldn't be able to use them anyway)


```racket
#lang racket
;; Object Serialization: Tim Brown, Oct. 2014
(require racket/serialize)

(define (join-person-name-list persons)
  (string-join (map (λ (c) (send c ->string)) persons) ", "))

(define-serializable-class person% object%
  (init-field name [siblings null])
  (define/public (->string #:show (show null))
    (cond
      [(and (member 'siblings show) (not (null? siblings)))
       (format "~a (~a)" name (join-person-name-list siblings))]
      [else name]))
  (super-new))

(define-serializable-class parent% person%
  (init-field [children null])
  (define/override (->string #:show (show null))
    (cond
      [(and (member 'children show) (not (null? children)))
       (format "~a [~a]" (super ->string #:show show) (join-person-name-list children))]
      [else (super ->string #:show show)]))
  (super-new))

;; horribly out of fashion and probaly no longer PC
(define-serializable-class nuclear-family% object%
  (init-field father mother children)
  (define/public (->string)
    (string-append
     (format "~a + ~a -> " (send father ->string) (send mother ->string))
     (format "~a" (join-person-name-list children))))
  (super-new))

;; =| TESTS |
### ===================================================================================

(define jack (new person% [name "Jack"]))
(define joan (new person% [name "Joan"]))
(set-field! siblings jack (list joan))
(set-field! siblings joan (list jack))
(define the-kids (list jack joan))
(define john (new parent% [name "John"] [children the-kids]))
(define jane (new parent% [name "Jane"] [children the-kids]))

(define the-family
  (new nuclear-family% [father john] [mother jane] [children the-kids]))

(define (duplicate-object-through-file o f-name)
  (with-output-to-file f-name #:exists 'replace (λ () (write (serialize o))))
  (with-input-from-file f-name (λ () (deserialize (read)))))

(define cloned-family (duplicate-object-through-file the-family "objects.dat"))

(printf "The original family:\t~a~%" (send the-family ->string))
(printf "The cloned family:\t~a~%~%" (send cloned-family ->string))
(printf "objects.dat contains ----~%~a~%-------------------~%~%" (file->string "objects.dat"))
(printf "Clones are different?~%")
(define cloned-jack (first (get-field children cloned-family)))
(set-field! name cloned-jack "JACK")
(printf "Jack's  name is:\t~s~%" (get-field name jack))
(printf "Clone's name is:\t~s~%~%" (get-field name cloned-jack))
(printf "Relationships are maintained?~%")
(define cloned-joan (second (get-field children cloned-family)))
(printf "Joan's description with siblings:\t~s~%" (send joan ->string #:show '(siblings)))
(printf "Clone's description with siblings:\t~s~%~%"
        (send cloned-joan ->string #:show '(siblings)))
(printf "After Jack's renaming the cloned family is: ~a~%~%" (send cloned-family ->string))
(printf "Various descriptions of cloned John:~%")
(define cloned-john (get-field father cloned-family))
(printf "Just the name:\t~s~%" (send cloned-john ->string))
(printf "With siblings:\t~s (he hasn't any)~%" (send cloned-john ->string #:show '(siblings)))
(printf "With children:\t~s~%" (send cloned-john ->string #:show '(children)))
(printf "With both:\t~s~%" (send cloned-john ->string #:show '(siblings children)))
```


{{out}}

```txt
The original family:	John + Jane -> Jack, Joan
The cloned family:	John + Jane -> Jack, Joan

objects.dat contains ----
((3) 3 ((#"C:\\[...]\\Serializable-Objects.rkt" . deserialize-info:person%) (#"C:\\[...]\\Serializable-Objects.rkt" . deserialize-info:nuclear-family%) (#"C:\\[...]\\Serializable-Objects.rkt" . deserialize-info:parent%)) 4 ((q . #(())) #&0 (0 (c (? . 0) c "Joan" c (c (? . 1)))) (c (? . 1) c (? . 2))) ((1 0 (c (? . 0) c "Jack" c (c (? . 2))))) (1 (c (? . 0) c (2 (c (v! (c (? . 0) q "John" ())) c (? . 3))) c (2 (c (v! (c (? . 0) q "Jane" ())) c (? . 3))) c (? . 3))))
-------------------

Clones are different?
Jack's  name is:	"Jack"
Clone's name is:	"JACK"

Relationships are maintained?
Joan's description with siblings:	"Joan (Jack)"
Clone's description with siblings:	"Joan (JACK)"

After Jack's renaming the cloned family is: John + Jane -> JACK, Joan

Various descriptions of cloned John:
Just the name:	"John"
With siblings:	"John" (he hasn't any)
With children:	"John [JACK, Joan]"
With both:	"John [JACK, Joan]"
```



## Ruby

The core class <code>[http://www.ruby-doc.org/core/classes/Marshal.html Marshal]</code> handles object serialization.  The <code>dump</code> method serializes an object, and the <code>load</code> method reconstitutes it.

```ruby
class Being
  def initialize(specialty=nil)
    @specialty=specialty
  end
  def to_s
    "(object_id = #{object_id})\n"+"(#{self.class}):".ljust(12)+to_s4Being+(@specialty ? "\n"+" "*12+@specialty : "")
  end
  def to_s4Being
    "I am a collection of cooperative molecules with a talent for self-preservation."
  end
end

class Earthling < Being
  def to_s4Being
    "I originate from a blue planet.\n"+" "*12+to_s4Earthling
  end
end

class Mammal < Earthling
  def initialize(type)
    @type=type
  end
  def to_s4Earthling
    "I am champion in taking care of my offspring and eating everything I can find, except mammals of type #{@type}."
  end
end

class Fish < Earthling
  def initialize(iq)
    @iq=(iq>1 ? :instrustableValue : iq)
  end
  def to_s4Earthling
    "Although I think I can think, I can't resist biting in hooks."
  end
end

class Moonling < Being
  def to_s4Being
    "My name is Janneke Maan, and apparently some Earthlings will pay me a visit."
  end
end

diverseCollection=[]
diverseCollection << (marsian=Being.new("I come from Mars and like playing hide and seek."))
diverseCollection << (me=Mammal.new(:human))
diverseCollection << (nemo=Fish.new(0.99))
diverseCollection << (jannakeMaan=Moonling.new)

puts "BEGIN ORIGINAL DIVERSE COLLECTION"
diverseCollection.each do |being|
  puts "",being.to_s
end
puts "END ORIGINAL DIVERSE COLLECTION"
puts "\n"+"*"*50+"\n\n"

#Marshal the diverse Array of beings
File.open('diverseCollection.bin','w') do |fo|
  fo << Marshal.dump(diverseCollection)
end

#load the Array of diverse beings
sameDiverseCollection=Marshal.load(File.read('diverseCollection.bin'))

puts "BEGIN LOADED DIVERSE COLLECTION"
puts(
     sameDiverseCollection.collect do |being|
       being.to_s
     end.join("\n\n")
     )
puts "END LOADED DIVERSE COLLECTION"
```



## Rust

Rust does not have inheritance, but this can be done with either enums or traits.

###  Enum

Dependencies:

```TOML
serde = { version = "1.0.89", features = ["derive"] }
bincode = "1.1.2"
```



```Rust
use std::fmt;

use bincode::{deserialize, serialize};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
enum Animal {
    Dog { name: String, color: String },
    Bird { name: String, wingspan: u8 },
}

impl fmt::Display for Animal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Animal::Dog { name, color } => write!(f, "{} is a dog with {} fur", name, color),
            Animal::Bird { name, wingspan } => {
                write!(f, "{} is a bird with a wingspan of {}", name, wingspan)
            }
        }
    }
}

fn main() -> bincode::Result<()> {
    let animals = vec![
        Animal::Dog {
            name: "Rover".into(),
            color: "brown".into(),
        },
        Animal::Bird {
            name: "Tweety".into(),
            wingspan: 3,
        },
    ];

    for animal in &animals {
        println!("{}", animal);
    }

    let serialized = serialize(&animals)?;

    println!("Serialized into {} bytes", serialized.len());

    let deserialized: Vec<Animal> = deserialize(&serialized)?;

    println!("{:#?}", deserialized);

    Ok(())
}
```

{{out}}

```txt
Rover is a dog with brown fur
Tweety is a bird with a wingspan of 3
Serialized into 57 bytes
[
    Dog {
        name: "Rover",
        color: "brown"
    },
    Bird {
        name: "Tweety",
        wingspan: 3
    }
]
```


###  Trait

Dependencies:

```TOML
serde = { version = "1.0.89", features = ["derive"] }
bincode = "1.1.2"
typetag = "0.1.1"
```



```Rust
use std::fmt::{self, Debug, Display};

use bincode::{deserialize, serialize};
use serde::{Deserialize, Serialize};

#[typetag::serde(tag = "animal")]
trait Animal: Display + Debug {
    fn name(&self) -> Option<&str>;
    fn feet(&self) -> u32;
}

#[derive(Debug, Deserialize, Serialize)]
struct Dog {
    name: String,
    color: String,
}

impl Display for Dog {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} is a dog with {} fur", self.name, self.color)
    }
}

#[typetag::serde]
impl Animal for Dog {
    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn feet(&self) -> u32 {
        4
    }
}

#[derive(Debug, Deserialize, Serialize)]
struct Bird {
    name: String,
    wingspan: u32,
}

impl Display for Bird {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} is a bird with a wingspan of {}",
            self.name, self.wingspan
        )
    }
}

#[typetag::serde]
impl Animal for Bird {
    fn name(&self) -> Option<&str> {
        Some(&self.name)
    }

    fn feet(&self) -> u32 {
        2
    }
}

fn main() -> bincode::Result<()> {
    let animals: Vec<Box<dyn Animal>> = vec![
        Box::new(Dog {
            name: "Rover".into(),
            color: "brown".into(),
        }),
        Box::new(Bird {
            name: "Tweety".into(),
            wingspan: 3,
        }),
    ];

    for animal in &animals {
        println!("{}", animal);
    }

    let serialized = serialize(&animals)?;
    println!("Serialized into {} bytes", serialized.len());

    let deserialized: Vec<Box<dyn Animal>> = deserialize(&serialized)?;

    println!("{:#?}", deserialized);

    Ok(())
}
```

{{out}}

```txt
Rover is a dog with brown fur
Tweety is a bird with a wingspan of 3
Serialized into 172 bytes
[
    Dog {
        name: "Rover",
        color: "brown"
    },
    Bird {
        name: "Tweety",
        wingspan: 3
    }
]
```



## Tcl

{{works with|Tcl|8.6}}
''This example uses an experimental package, available from [http://wiki.tcl.tk/23444 The Tcler's Wiki].

```tcl
package require Tcl 8.6
package require TclOO::serializer 0.1

# These classes are inspired by the Perl example
oo::class create Greeting {
    superclass oo::serializable
    variable v
    constructor {} {
        set v "Hello world!"
    }
    method get {} {
        return $v
    }
}
oo::class create SubGreeting {
    superclass Greeting oo::serializable
    variable v
    constructor {} {
        set v "Hello world from Junior!"
    }
}
oo::class create GreetingsHolder {
    superclass oo::serializable
    variable o1 o2
    constructor {greeting1 greeting2} {
        set o1 $greeting1
        set o2 $greeting2
    }
    method printGreetings {} {
        puts [$o1 get]
        puts [$o2 get]
    }
    destructor {
        $o1 destroy
        $o2 destroy
    }
}

# Make some objects and store them
GreetingsHolder create holder [Greeting new] [SubGreeting new]
set f [open "objects.dat" w]
puts $f [oo::serialize holder]
close $f

# Delete the objects
holder destroy

# Recreate the objects from the file and show that they work
set f [open "objects.dat" r]
set obj [oo::deserialize [read $f]]
close $f
$obj printGreetings
```



## zkl

zkl can serialize a "root class" (usually a file but any static (ie parentless) class) to bit bucket (such as File). This is done via reflection. The state of the class(es) are not stored so no image write/read. In the "normal" course of events, this isn't used, programs are compiled on the fly and run. However, there is extensive support to pre-compile and package files into applications or just pre-compile for faster load times (or to create an image that can be compiled into C code (which is done to package the compiler with the VM). When the compiler writes a class or app to a file, the preferred extension is ".zsc", which is what the Import method looks for. But no matter, we have ways ...


```zkl
class [static] ARootClass{ // A top level class, no instances
   class A{ self.println(" constructor"); } // a regular class
   class B(A){ // ditto
      var x;
      fcn init(x=123){ self.x=x }
      fcn toString{ "x is "+x }
   }
}

ARootClass.B(456).println(); // create an instance
   // prints:
   Class(A) constructor
   x is 456

f:=File("object.dat","wb");
Compiler.Asm.writeRootClass(ARootClass,f); // serialize to file
f.close();

f:=File("object.dat","rb");
rc:=Compiler.Asm.readRootClass(f); // read and re-create
   // prints (readRootClass calls all constructors by default):
   Class(A) constructor
f.close();
rc.B().println();  // create a new instance of B
   // prints:
   x is 123
```



{{omit from|AWK}}
{{omit from|C}}
{{omit from|Fortran}}
{{omit from|M4}}
{{omit from|Mathematica}}
{{omit from|Maxima}}
{{omit from|Metafont}}
{{omit from|Octave}}
{{omit from|PARI/GP}}
{{omit from|PureBasic|PureBasic does not allow serialization without extra add-ons.}}
{{omit from|Retro}}
{{omit from|REXX}}
{{omit from|TI-83 BASIC}}
{{omit from|TI-89 BASIC|Does not have user-defined data structures or objects.}}
{{omit from|ZX Spectrum Basic|Does not have user-defined data structures or objects.}}
