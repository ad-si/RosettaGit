+++
title = "M2000 Interpreter"
description = ""
date = 2019-07-29T15:08:44Z
aliases = []
[extra]
id = 21886
[taxonomies]
categories = []
tags = []
+++

M2000 is an interpreter running on its own Environment written in Visual Basic 6, as an open source project and can be found in GitHub[https://github.com/M2000Interpreter/Version9]. Current version is 9.8, revision 29. See [M2000 Interpreter Implementation](https://rosettacode.org/wiki/M2000_Interpreter_Implementation)
M2000 has two set of vocabularies, one with English identifiers and one with Greek identifiers. We can mix languages, but not if some identifiers are part of a statement in non matching language.


M2000 start as an experimental interpreted language, using a Module in Module idea (like a Procedure in Procedure) where each inner Module is closed for modification, but open for expansion, by replacing code at run time. Code executed in one pass. There is a low range pass to determine the major type of an expression, a number or a string. Look this paradigm: We call inner Beta in two stages. At the second stage we change inner Theta with Theta2. This is the decoration of Beta with Theta as Theta2. This is a temporary decoration because Beta after execution erase any new identifier including Theta. So each time we call Beta, statement Module Theta make this module unless a decoration stop it.
## English Vocabulary
<lang >Module Beta {
      Module Theta (x){
            Print "This is Theta, we get x=";x
      }
      For i=1 to 3 : Theta i : Next i
}
Beta
Module Theta2 (x) {
            Print "This is Theta2, we get x=";x
}
Beta ; Theta as Theta2
```

## Greek Vocabulary
<lang >Τμήμα Βήτα {
      Τμήμα Θήτα (χ){
            Τύπωσε "Αυτό είναι το Θήτα, θα πάρουμε το χ=";χ
      }
      Για ι=1 έως 3 : Θήτα ι : Επόμενο ι
}
Βήτα
Τμήμα Θήτα2 (χ) {
            Τύπωσε "Αυτό είναι το Θήτα2, θα πάρουμε το χ=";χ
}
Βήτα ; Θήτα ως Θήτα2
```



As we can see from code, some statements are like BASIC, except the use of curly brackets {}. Check the code below. We have a Module (as a Procedure), where we define some functions and some variables, as entities. These entities defined in every call to CheckIt, and erased when execution return from CheckIt. By default every parameter in M2000 pass by value. We define two functions, Alfa() where we set type for parameter x and ExpType$() where we not set type for x, and we wish to return the name of type when we make a call. Using DEF we can define variables and functions (in one line).

<lang >Module CheckIt {
      \\ We can't call something out of this module
      Function Alfa(X as double) {
            =X**2
      }
      Print Alfa(5) ' we pass a double type number
      Print Alfa(12121221121.1212@) ' we pass a Decimal type
      \\ M get type from first assignment
      M=Alfa(3)
      Print Type$(M) ' Double
      \\ Def can create once Z (second time raise error)
      Def Z as Double
      \\ If a variable get a type then any number convert to that type before assign to it
      Z=121212112.11212@
      Print Type$(Z)
      Def Currency Z1=212.12, Z2=323233.12, Z3=223212323323.12
      Print Z1, Z2, Z3
      Def ExpType$(x)=Type$(x)
      Print ExpType$(Z1+Z2) ' Currency
      Print ExpType$(Z1*Z2) ' Currency
      Print ExpType$(Z2*Z3) ' Double (Currency can't hold this number)
      \\ Integer, Long, Single, Currency, Decimal
      Print ExpType$(10%), ExpType$(10&), ExpType$(10~), ExpType$(10#), ExpType$(10@)
      \\ Without symbol is a Double
      Print ExpType$(10)
}
\\ We call it
CheckIt
\\ Now Function Alfa and all variables erased.
\\ We can call it again
CheckIt
```



For small programs, for learning algorithms there is no need to use types, except the major types, Numeric and String, plus one more: Integer. Integers can be number types like Long and Integer types, or can be any numeric type using without decimal part. So Integer variables are variables with % at the end of their name. String Variable need a $ at the end of name.

For Expressions before execution automatic check by names if the result can be for strings names, or for numeric names. A string name has $ at the end of name. Maybe a string name isn't a string, because with same name we can define groups which get or and return strings values. A statement Input Alfa$ can input characters for a string or for object typed group with that name. Interpreter implicitly use the appropriate method. If name has no $ at the end then this can be an object or a numeric value. A Input Beta may return error if Beta has no numeric value to set.

Types are strong for values with a name (variables, constants), but weak for items in containers. In a container (Array, Inventory and Stack) we can place anything including other containers. We can bypass the "strong" capability but this isn't a practice for good programming. Internal or variables are of type of Variant, so we can make a reference of A to A$ and we can save a string "1212" and read from A the number 1212.

<lang >
Module AKindOfMess {
      a=10@
      Print Type$(a)="Decimal"
      \\ a and a$ reference same variant type
      Link a to a$
      Print a$
      Let a$=str$(500.12, "")
      Rem : a++  ' error can't apply ++ to a string type
      Print type$(a)="String"  ' true
      ' Error a=a+a
      Print len(a$)
      Print a=500.12   ' true
      a$+=a$
      Print len(a$)
      Print a$, a=0
      a$=Left$(a$, len(a$)/2)
      Print a$
      Print a=500.12
      let a=100@
      Print Type$(a)="Double"
      Print a$="100"  ' true
      For This {
            M=23451234512345123451234512345@
            swap M, a
      }
      Print Valid(M)=false ' M not exist now
      Print Type$(a)="Decimal"  ' true
      Print a$="23451234512345123451234512345"  ' true
}
AKindOfMess
Print Valid(a)=false  ' not exist
Print Valid(a$<>"")=false  ' not exist
Print Valid(12/0)=false ' evaluate expression and return true if is ok (errors trapped always)
Print Error$=""


```


So check this for proper use of types.

<lang >Module CheckInt {
      A%=1212212.12@
      Print A% ' 1212212
      A%=1212212.52@
      Print A% ' 1212213
      Print Type$(A%) ' Decimal
      A%++
      Print A% ' 1212214
      Print Type$(A%) ' Decimal
      B%=122121212.12 ' make Integer from Double
      Print B% ' 122121212
      Print Type$(B%) ' Double
      B%++
      Print Type$(B%) ' Double
      Print B% ' 122121213
}
CheckInt
```


So we say about Integer Variables, and no Integer Numeric Type. Like in Basic, M2000 is not case sensitive (except for labels), so A% and a% is the same. We may have A, A$ and A% as three variables, or A(), A%(), A$() as arrays or and functions. We can use name(@ ) to call function and not array if we have each with same name.

<lang >Dim A(10)=1
Def A(x)=x**2
Print A(3), A(@ 4)
```



By default all variables are local. M2000 uses heap to store local variables, not stack. We can use global variables, but a local definition hide a global one. We can make global variables for temporary use (at return from module which we define a global, this global erased). A global definition in an already defined global variable with same name, hide for temporary use the old global variable.

It is easy to make modules, with modules, functions, variables, arrays. We can use subroutines when we want code in a module or a function to used more than once. We can use local variables inside subroutines. In subroutines we can use module's variables, functions, subroutines. Until here we see something like BASIC, except that variables are local to modules and functions, but can be used by subroutines. Also we have see that we can replace a module in a module when we call it. With this replacement we can use a predefined logic with some "terminals", modules that get parameters and do final things. We can make global modules and functions in local modules and functions, but these exist until creator module or function exit.

M2000 can be used for more advanced programming. Modules and functions can be members of Groups. Groups are values. So a function can return a Group.


### Stack of Values

Modules and functions get parameters in a stack. Modules get the parent stack with parameters, and functions get a new stack with parameters. Modules return from call and are responsible to clear as needed the stack. Functions drop the stack after the call from an expression. A module can return entities in stack, if we want to do that. We can call a module and pass parameters, and that module can call other module leaving parameters in stack to be read in last module.
We use Read to pop values from stack to feed variables, or Number to pop number, or Letter$ to pop alphanumeric. We can use Empty to check if we have an empty stack. And there are other statement to move or copy items, to drop items.

In the example below (y) is a Read y for module definition. Early versions of language use Module alfa {Read y ... }, and last can use Module alfa(y) {...} with or without space before open parenthesis.

As we see alfa has no read statement, because before get the first number need to make two modules, and then pop one number and call a module passing current stack, so if choose beta, then the second value feed y  from a read y statement.

<lang >
module alfa  {
      module beta (y) {
            print y**3
      }
      module delta (y) {
            print y**2
      }
     if number<2 then {beta} else delta
}
alfa 1, 3   ' 27
alfa 3, 3  ' 9


\\ number pop a number from stack
function sumall {
      sum=0
      While not empty {
            sum+=number
      }
      =sum
}
print sumall(1,2,3,4,5,6,7)=28

``` >




### OOP

Groups are objects for M2000. We can use them as values, and if we want we can use pointers for groups (but we can make a lot of programs without using pointers to groups). Without using pointer, a Group may be local with a name in a Module or can be in a container like array, inventory (a type of map) and stack objects, without name. Groups may have private and public members, may have properties as read only, may get value and return value, and may have operators. Members of Groups can be anything, including other groups. Groups may have events too. We can make groups by using CLASS statement or by using own function which return a Group or a pointer to Group.

A special object is the lambda object, which may have a number of closures, and have to faces, a variable and a function:


<lang >Module  Delta {
      \\ Make a group like a lambda
      \\ A Class make a Global Function
      \\ (in a group a Class definition make a group member, not a global function)
      Class Alfa {
      Private:
            Z=0
      Public:
            Value (x) {
            \\ see dot before Z
            \\ this is the same as This.Z
                  =X**2+.Z
            }
      Class:
            \\ After Class: definitions are lost when we copy group
            \\ this is the constructor
            \\ here only a Read .Z used
            Module Alfa(.Z) {}
      }
      \\ M, N, K and P get type by first value on assignment
      M=Alfa(5)
      \\ M has no Alfa module, because a copy perform in Alfa()
      Print M(3)=14
      \\ Make a lambda
      N=Lambda Z=5 (x)->X**2+Z
      Print N(3)=14
      K=N ' copy lambda N to K
      Print N(3)=K(3)
      Print Type$(N)="lambda"
      \\ we want a copy of M not value of M
      P=Group(M )
      Print M(3)=P(3)
      Print Type$(P) ="Group"
}
Delta
```



We can place a lambda function as closure in a lambda function, and we can build a program as one "top" function, and many lambda as closures, which may have other lambda functions. Closures can be change values from lambda but not from outside unless they are pointers (so we may have same pointers outside of lambda, or in another lambda. A lambda variable can change function and closures in a new assignment. We can store lambda state (if we don't use pointers in closures) in a new variable which take as first value lambda (and then can take only lambda functions)

Until now we see modules/functions/subs for procedural programming, Groups for OOP, Groups as lambda functions and lambda functions for functional programming. We can use events for groups and for COM objects, including GUI objects.

<lang >Module Beta {
      Group WithEvents Alpha {
            Event "One"
            Module DoIt (x) {
                  \\ if no service function founded, no error happen
                  Call Event "One", x
            }
      }
      Alpha.Doit 50
      \\  nothing because we didn't define yet the service function
      M=10
      X=5
      Function Alpha_One (New X){
            \\ Event functions called "local", with scope on module
            Print M+X
            M++
      }
      \\ Now function Alpha_One() exist
      Alpha.Doit 20 ' 30
      Print X=5 ' delete New, in event service function to get 20
      Print M=11
      \\In M2000 we can change definitions
      Function Alpha_One (New X){
            \\ Event functions called "local", with scope on module
            Print M+X
            M+=100
      }
      Alpha.Doit 20 ' 31
      Print M=111
}
Beta
```




Until now we see that programming with events can be used in M2000. There is an Event object among the light events in Groups using WithEvents, and we can use COM Events to handle objects like MS Word.

Modules may have Threads, part of modules that can be executed in intervals, can be halted, or can be released, and can be erased. Each thread has own stack and may have own static variables (Modules and Functions also may have static variables), but can use modules variables and functions/modules/subs. Threads can run concurrent (thread return to task manager after execution of a statement or a block of statements) or sequential (a thread has to exit from interval to start other thread)

<lang >Module Zeta {
      k=10
      Thread {
           k++
      } As M interval 50
      \\ This is the main task (a thread)
      Main.Task 100 {
            Print k
            If keypress(32) then Exit
      }
}
Zeta
```




M2000 Interpreter can work with Structures and Buffers. Buffer is a memory block and Structure can define types of memory, and a buffer can hold arrays of structures. Each structure can be made from other structures or arrays of structures, and may have unions. We can use buffers for code execution (writing machine code), using standard buffers (not executable), for data holder. Also we can use Buffers calling external dll and passing by address.


<lang >Module Kappa {
      Function Theta(x) {
             Structure Points_single {
                  x as single
                  y as single
            }
            Buffer Clear Points as Points_single*x
            =Points
      }
      P=Theta(100)
      Print Len(P)=800 ' 2x4x100 bytes
      \\ return to buffer multiple values using index!offset
      Return P, 0!x:=1212.12, 0!y:=21.1212
      \\
      Print Eval(P, 0!x)=1212.12~, Eval(P, 0!y)=21.1212~
}
Kappa
```

