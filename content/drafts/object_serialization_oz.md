+++
title = "Object serialization/Oz"
description = ""
date = 2010-02-06T13:59:17Z
aliases = []
[extra]
id = 5387
[taxonomies]
categories = []
tags = []
+++

This example uses internal implementation details and may not work in future Mozart versions.

First a module that defines a base class and functions for marshalling/unmarshalling:


```oz
functor
import
   Module
export
   Mixin
   InstantiateMarshaller
define
   class Mixin
      meth initUninitialized skip end
     
      meth toData($)
	 As = {Arity {GetClass self}.{AtomToName 'ooAttr'}}
	 Fs = {Arity {GetClass self}.{AtomToName 'ooFeat'}}
      in
	 {Map As fun {$ A} A#@A end}
	 #{Map Fs fun {$ F} F#self.F end}
      end
      
      meth fromData(Data) As#Fs = Data in
	 {ForAll As proc {$ A#V} A := V end}
	 {ForAll Fs proc {$ F#V} self.F = V end}
      end
   end

   fun {InstantiateMarshaller Classes}
      ClassByName = {List.toRecord unit
		     {List.zip
		      {Map Classes GetClassName}
		      Classes
		      fun {$ N C} N#C end}} 

      fun {Unmarshal ClassName#Data}
	 O = {New ClassByName.ClassName initUninitialized}
      in
	 {O fromData(Data)}
	 O
      end
   in
      module(marshal:Marshal
	     unmarshal:Unmarshal
	    )
   end
   
   %% Result: pair of class name and object data
   fun {Marshal Object}
      {GetClassName {GetClass Object}}#{Object toData($)}
   end

   fun {GetClassName Class}
      Class.{AtomToName 'ooPrintName'}
   end
  
   %% Get access to two internal, implementation-dependent functions
   local
      [Boot_Object Boot_Name] = {Module.link ['x-oz://boot/Object' 'x-oz://boot/Name']}
   in
      GetClass = Boot_Object.getClass
      AtomToName = Boot_Name.newUnique
   end
end
```



An example application, inspired by the C++ example:


```oz
functor
import
   Application
   ObjectMarshalling
   Pickle
   System
define
  local
     Count = {NewCell 1}
  in
     class Employee from ObjectMarshalling.mixin
	feat
	   name
	   department
	   id
	   
	meth init(Department Name)
	   self.name = Name
	   self.department = Department
	   self.id = Count := @Count + 1
	end
	
	meth print($)
	   "Id:"#self.id#", name: "#self.name#", dep.: "#self.department
	end
     end
  end
  
  class Worker from Employee
     feat
	salary
	
     meth init(Department Name Salary)
	Employee, init(Department Name)
	self.salary = Salary
     end
     
     meth print($)
	Employee, print($)#", wage per hour: "#self.salary
     end
  end

  Marshaller = {ObjectMarshalling.instantiateMarshaller [Employee Worker]}
  
  Employees = [{New Employee init("maintenance" "Fritz Schmalstieg")}
	       {New Employee init("repair" "John Berry")}
	       {New Worker init("maintenance" "Laurent Le Chef" 20.0)}
	       {New Worker init("IT" "Srinivan Taraman" 55.35)}
	      ]

  {System.showInfo "Original objects:"}
  for E in Employees do {System.showInfo {E print($)}} end

  {Pickle.save {Map Employees Marshaller.marshal} "objects.dat"}

  EmployeesFromDisk = {Map {Pickle.load "objects.dat"} Marshaller.unmarshal}

  {System.showInfo "Loaded objects:"}
  for E in EmployeesFromDisk do {System.showInfo {E print($)}} end

  {Application.exit 0}
end
```

