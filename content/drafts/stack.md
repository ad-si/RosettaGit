+++
title = "Stack"
description = ""
date = 2019-10-17T04:50:16Z
aliases = []
[extra]
id = 1970
[taxonomies]
categories = []
tags = []
+++

{{task|Data Structures}}
[[Category:Encyclopedia]]
{{data structure}}[[Category:Classic CS problems and programs]]

A '''stack''' is a container of elements with   <big><u>l</u>ast <u>i</u>n, <u>f</u>irst <u>o</u>ut</big>    access policy.   Sometimes it also called '''LIFO'''. 

The stack is accessed through its '''top'''. 

The basic stack operations are:

*   ''push''   stores a new element onto the stack top;
*   ''pop''   returns the last pushed stack element, while removing it from the stack;
*   ''empty''   tests if the stack contains no elements.



Sometimes the last pushed stack element is made accessible for immutable access (for read) or mutable access (for write):

*   ''top''   (sometimes called ''peek'' to keep with the ''p'' theme) returns the topmost element without modifying the stack.



Stacks allow a very simple hardware implementation.
 
They are common in almost all processors.
 
In programming, stacks are also very popular for their way ('''LIFO''') of resource management, usually memory. 

Nested scopes of language objects are naturally implemented by a stack (sometimes by multiple stacks). 

This is a classical way to implement local variables of a re-entrant or recursive subprogram. Stacks are also used to describe a formal computational framework. 

See [[wp:Stack_automaton|stack machine]]. 

Many algorithms in pattern matching, compiler construction (e.g. [[wp:Recursive_descent|recursive descent parsers]]), and machine learning (e.g. based on [[wp:Tree_traversal|tree traversal]]) have a natural representation in terms of stacks.


;Task:
Create a stack supporting the basic operations: push, pop, empty.


{{Template:See also lists}}





## ABAP


This works for ABAP Version 7.40 and above


```ABAP

report z_stack.

interface stack.
  methods:
    push
      importing
        new_element      type any
      returning
        value(new_stack) type ref to stack,

    pop
      exporting
        top_element      type any
      returning
        value(new_stack) type ref to stack,

    empty
      returning
        value(is_empty) type abap_bool,

    peek
      exporting
        top_element type any,

    get_size
      returning
        value(size) type int4,

    stringify
      returning
        value(stringified_stack) type string.
endinterface.


class character_stack definition.
  public section.
    interfaces:
      stack.


    methods:
      constructor
        importing
          characters type string optional.


  private section.
    data:
      characters type string.
endclass.


class character_stack implementation.
  method stack~push.
    characters = |{ new_element }{ characters }|.

    new_stack = me.
  endmethod.


  method stack~pop.
    if not me->stack~empty( ).
      top_element = me->characters(1).

      me->characters = me->characters+1.
    endif.

    new_stack = me.
  endmethod.


  method stack~empty.
    is_empty = xsdbool( strlen( me->characters ) eq 0 ).
  endmethod.


  method stack~peek.
    check not me->stack~empty( ).

    top_element = me->characters(1).
  endmethod.


  method stack~get_size.
    size = strlen( me->characters ).
  endmethod.


  method stack~stringify.
    stringified_stack = cond string(
      when me->stack~empty( )
      then `empty`
      else me->characters ).
  endmethod.


  method constructor.
    check characters is not initial.

    me->characters = characters.
  endmethod.
endclass.


class integer_stack definition.
  public section.
    interfaces:
      stack.


    methods:
      constructor
        importing
          integers type int4_table optional.


  private section.
    data:
      integers type int4_table.
endclass.


class integer_stack implementation.
  method stack~push.
    append new_element to me->integers.

    new_stack = me.
  endmethod.


  method stack~pop.
    if not me->stack~empty( ).
      top_element = me->integers[ me->stack~get_size( ) ].

      delete me->integers index me->stack~get_size( ).
    endif.

    new_stack = me.
  endmethod.


  method stack~empty.
    is_empty = xsdbool( lines( me->integers ) eq 0 ).
  endmethod.


  method stack~peek.
    check not me->stack~empty( ).

    top_element = me->integers[ lines( me->integers ) ].
  endmethod.


  method stack~get_size.
    size = lines( me->integers ).
  endmethod.


  method stack~stringify.
    stringified_stack = cond string(
      when me->stack~empty( )
      then `empty`
      else reduce string(
        init stack = ``
        for integer in me->integers
        next stack = |{ integer }{ stack }| ) ).
  endmethod.


  method constructor.
    check integers is not initial.

    me->integers = integers.
  endmethod.
endclass.


start-of-selection.
  data:
    stack1        type ref to stack,
    stack2        type ref to stack,
    stack3        type ref to stack,

    top_character type char1,
    top_integer   type int4.

  stack1 = new character_stack( ).
  stack2 = new integer_stack( ).
  stack3 = new integer_stack( ).

  write: |Stack1 = { stack1->stringify( ) }|, /.
  stack1->push( 'a' )->push( 'b' )->push( 'c' )->push( 'd' ).
  write: |push a, push b, push c, push d -> Stack1 = { stack1->stringify( ) }|, /.
  stack1->pop( )->pop( importing top_element = top_character ).
  write: |pop, pop and return element -> { top_character }, Stack1 = { stack1->stringify( ) }|, /, /.

  write: |Stack2 = { stack2->stringify( ) }|, /.
  stack2->push( 1 )->push( 2 )->push( 3 )->push( 4 ).
  write: |push 1, push 2, push 3, push 4 -> Stack2 = { stack2->stringify( ) }|, /.
  stack2->pop( )->pop( importing top_element = top_integer ).
  write: |pop, pop and return element -> { top_integer }, Stack2 = { stack2->stringify( ) }|, /, /.

  write: |Stack3 = { stack3->stringify( ) }|, /.
  stack3->pop( ).
  write: |pop -> Stack3 = { stack3->stringify( ) }|, /, /.

```


{{out}}

```txt

Stack1 = empty

push a, push b, push c, push d -> Stack1 = dcba

pop, pop and return element -> c, Stack1 = ba


Stack2 = empty

push 1, push 2, push 3, push 4 -> Stack2 = 4321

pop, pop and return element -> 3, Stack2 = 21


Stack3 = empty

pop -> Stack3 = empty

```



## ActionScript

In ActionScript an Array object provides stack functionality.

```actionscript
var stack:Array = new Array();
stack.push(1);
stack.push(2);
trace(stack.pop()); // outputs "2"
trace(stack.pop()); // outputs "1"
```



## Ada

This is a generic stack implementation.

```ada
generic
   type Element_Type is private; 
package Generic_Stack is
   type Stack is private; 
   procedure Push (Item : Element_Type; Onto : in out Stack); 
   procedure Pop (Item : out Element_Type; From : in out Stack); 
   function Create return Stack;
   Stack_Empty_Error : exception;
private
   type Node; 
   type Stack is access Node; 
   type Node is record 
      Element : Element_Type;  
      Next    : Stack        := null;  
   end record; 
end Generic_Stack;
```


```ada
with Ada.Unchecked_Deallocation;

package body Generic_Stack is
   
   ------------
   -- Create --
   ------------
   
   function Create return Stack is
   begin
      return (null);
   end Create;

   ----------
   -- Push --
   ----------

   procedure Push(Item : Element_Type; Onto : in out Stack) is
      Temp : Stack := new Node;
   begin
      Temp.Element := Item;
      Temp.Next := Onto;
      Onto := Temp; 
   end Push;

   ---------
   -- Pop --
   ---------

   procedure Pop(Item : out Element_Type; From : in out Stack) is
      procedure Free is new Ada.Unchecked_Deallocation(Node, Stack);
      Temp : Stack := From;
   begin
      if Temp = null then
         raise Stack_Empty_Error;
      end if;
      Item := Temp.Element;
      From := Temp.Next;
      Free(Temp);
   end Pop;

end Generic_Stack;
```



## ALGOL 68


### ALGOL 68: Using linked list

ALGOL 68 uses "HEAP" variables for new LINKs in a linked list.  Generally ALGOL 68's 
[[garbage collection|garbage collector]] should recover the LINK memory some time after a value is popped.
{{works with|ALGOL 68|Revision 1 - one extension to language used - PRAGMA READ - a non standard feature similar to C's #include directive.}}
{{works with|ALGOL 68G|Any - tested with release [http://sourceforge.net/projects/algol68/files/algol68g/algol68g-2.7 algol68g-2.7].}}
{{wont work with|ELLA ALGOL 68|Any (with appropriate job cards) - tested with release [http://sourceforge.net/projects/algol68/files/algol68toc/algol68toc-1.8.8d/algol68toc-1.8-8d.fc9.i386.rpm/download 1.8-8d] - due to extensive use of '''format'''[ted] ''transput''.}}
'''File: prelude/next_link.a68'''
```algol68
# -*- coding: utf-8 -*- #
CO REQUIRES:
  MODE OBJVALUE = ~ # Mode/type of actual obj to be stacked #
END CO

MODE OBJNEXTLINK = STRUCT(
  REF OBJNEXTLINK next,
  OBJVALUE value # ... etc. required #
);

PROC obj nextlink new = REF OBJNEXTLINK:
  HEAP OBJNEXTLINK;

PROC obj nextlink free = (REF OBJNEXTLINK free)VOID:
  next OF free := obj stack empty # give the garbage collector a BIG hint #
```
'''File: prelude/stack_base.a68'''
```algol68
# -*- coding: utf-8 -*- #
CO REQUIRES:
  MODE OBJNEXTLINK = STRUCT(
    REF OBJNEXTLINK next,
    OBJVALUE value
  );
  PROC obj nextlink new = REF OBJNEXTLINK: ~,
  PROC obj nextlink free = (REF OBJNEXTLINK free)VOID: ~
END CO

# actually a pointer to the last LINK, there ITEMs are ADDED, pushed & popped #
MODE OBJSTACK = REF OBJNEXTLINK; 

OBJSTACK obj stack empty = NIL;

BOOL obj stack par = FALSE; # make code thread safe #
SEMA obj stack sema = LEVEL ABS obj stack par;
# Warning: 1 SEMA for all stacks of type obj, i.e. not 1 SEMA per stack #

PROC obj stack init = (REF OBJSTACK self)REF OBJSTACK:
  self := obj stack empty;

# see if the program/coder wants the OBJ problem mended... #
PROC (REF OBJSTACK #self#)BOOL obj stack index error mended
  := (REF OBJSTACK self)BOOL: (abend("obj stack index error"); stop);

PROC on obj stack index error = (REF OBJSTACK self, PROC(REF OBJSTACK #self#)BOOL mended)VOID:
  obj stack index error mended := mended;

PROC obj stack push = (REF OBJSTACK self, OBJVALUE obj)REF OBJSTACK:(
  IF obj stack par THEN DOWN obj stack sema FI;
  self := obj nextlink new := (self, obj);
  IF obj stack par THEN UP obj stack sema FI;
  self
);

# aliases: define a useful put (+=:) operator... #
OP +=: = (OBJVALUE obj, REF OBJSTACK self)REF OBJSTACK: obj stack push(self, obj);

PROC obj stack pop = (REF OBJSTACK self)OBJVALUE: (
# DOWN obj stack sema; #
  IF self IS obj stack empty THEN
    IF NOT obj stack index error mended(self) THEN abend("obj stack index error") FI FI;

  OBJNEXTLINK old head := self;
  OBJSTACK new head := next OF self;
  OBJVALUE out := value OF old head;
  obj nextlink free(old head); # freeing nextlink, NOT queue! #
  self := new head;
#;UP obj stack sema; #
  out
);

PROC obj stack is empty = (REF OBJSTACK self)BOOL:
  self IS obj stack empty;

SKIP
```
'''File: test/data_stigler_diet.a68'''
```algol68
# -*- coding: utf-8 -*- #
MODE DIETITEM = STRUCT(
  STRING food, annual quantity, units, REAL cost
);

# Stigler's 1939 Diet ... #
FORMAT diet item fmt = $g": "g" "g" = $"zd.dd$;
[]DIETITEM stigler diet = (
  ("Cabbage",           "111","lb.",  4.11),
  ("Dried Navy Beans",  "285","lb.", 16.80),
  ("Evaporated Milk",    "57","cans", 3.84),
  ("Spinach",            "23","lb.",  1.85),
  ("Wheat Flour",       "370","lb.", 13.33),
  ("Total Annual Cost",    "","",    39.93)
)
```
'''File: test/stack.a68'''
```algol68
#!/usr/bin/a68g --script #
# -*- coding: utf-8 -*- #

MODE OBJVALUE = DIETITEM;
PR read "prelude/next_link.a68" PR;
PR read "prelude/stack_base.a68" PR;

PR read "test/data_stigler_diet.a68" PR;
OBJSTACK example stack; obj stack init(example stack);

FOR i TO UPB stigler diet DO
#  obj stack push(example stack, stigler diet[i]) #
  stigler diet[i] +=: example stack
OD;

printf($"Items popped in reverse:"l$);
WHILE NOT obj stack is empty(example stack) DO
# OR example stack ISNT obj stack empty #
  printf((diet item fmt, obj stack pop(example stack), $l$))
OD
```

{{out}}

```txt

Items popped in reverse:
Total Annual Cost:   = $39.93
Wheat Flour: 370 lb. = $13.33
Spinach: 23 lb. = $ 1.85
Evaporated Milk: 57 cans = $ 3.84
Dried Navy Beans: 285 lb. = $16.80
Cabbage: 111 lb. = $ 4.11

```

'''See also:''' [[Queue#ALGOL_68|Queue]]

### ALGOL 68: Using FLEX array

An alternative to using a linked list is to use a FLEX array.

```algol68

MODE DIETITEM = STRUCT (
  STRING food, annual quantity, units, REAL cost
);
 
MODE OBJVALUE = DIETITEM;

# PUSH element to stack #
OP +:= = (REF FLEX[]OBJVALUE stack, OBJVALUE item) VOID:
   BEGIN
      FLEX[UPB stack + 1]OBJVALUE newstack;
      newstack[2:UPB newstack] := stack;
      newstack[1] := item;
      stack := newstack
   END;

OP POP = (REF FLEX[]OBJVALUE stack) OBJVALUE:
   IF UPB stack > 0 THEN
      OBJVALUE result = stack[1];
      stack := stack[2:UPB stack];
      result
   ELSE
      # raise index error; # SKIP 
   FI;

# Stigler's 1939 Diet ... #
FORMAT diet item fmt = $g": "g" "g" = $"zd.dd$;
[]DIETITEM stigler diet = (
  ("Cabbage",           "111","lb.",  4.11),
  ("Dried Navy Beans",  "285","lb.", 16.80),
  ("Evaporated Milk",    "57","cans", 3.84),
  ("Spinach",            "23","lb.",  1.85),
  ("Wheat Flour",       "370","lb.", 13.33),
  ("Total Annual Cost",    "","",    39.93)
);

FLEX[0]DIETITEM example stack;
 
FOR i TO UPB stigler diet DO
   example stack +:= stigler diet[i]
OD;
 
printf($"Items popped in reverse:"l$);
WHILE UPB example stack > 0 DO
  printf((diet item fmt, POP example stack, $l$))
OD

```

{{out}}

```txt

Items popped in reverse:
Total Annual Cost:   = $39.93
Wheat Flour: 370 lb. = $13.33
Spinach: 23 lb. = $ 1.85
Evaporated Milk: 57 cans = $ 3.84
Dried Navy Beans: 285 lb. = $16.80
Cabbage: 111 lb. = $ 4.11

```



## ALGOL W


```algolw
begin
    % define a Stack type that will hold StringStackElements     %
    % and the StringStackElement type                            %
    % we would need separate types for other element types       %
    record StringStack ( reference(StringStackElement) top );
    record StringStackElement ( string(8)                     element
                              ; reference(StringStackElement) next
                              );
    % adds e to the end of the StringStack s                     %
    procedure pushString ( reference(StringStack) value s
                         ; string(8)              value e
                         ) ;
    begin
        reference(StringStackElement) newElement;
        top(s) := StringStackElement( e, top(s) )
    end pushString ;
    % removes and returns the top element from the StringStack s %
    % asserts the Stack is not empty, which will stop the        %
    % program if it is                                           %
    string(8) procedure popString ( reference(StringStack) value s ) ;
    begin
        string(8) v;
        assert( not isEmptyStringStack( s ) );
        v     := element(top(s));
        top(s):= next(top(s));
        v
    end popStringStack ;
    % returns the top element of the StringStack s               %
    % asserts the Stack is not empty, which will stop the        %
    % program if it is                                           %
    string(8) procedure peekStringStack ( reference(StringStack) value s ) ;
    begin
        assert( not isEmptyStringStack( s ) );
        element(top(s))
    end popStringStack ;
    % returns true if the StringStack s is empty, false otherwise %
    logical procedure isEmptyStringStack ( reference(StringStack) value s ) ; top(s) = null;
 
    begin % test the StringStack operations %
        reference(StringStack) s;
        s := StringStack( null );
        pushString( s, "up"      );
        pushString( s, "down"    );
        pushString( s, "strange" );
        pushString( s, "charm"   );
        while not isEmptyStringStack( s ) do write( popString( s )
                                                  , if isEmptyStringStack( s ) then "(empty)"
                                                                               else peekStringStack( s )
                                                  )
    end
end.
```

{{out}}

```txt

charm   strange
strange down
down    up
up      (empty)

```



## Applesoft BASIC


```basic
100  DIM STACK$(1000)
110  DATA "(2*A)","PI","","TO BE OR","NOT TO BE"
120  FOR I = 1 TO 5
130  READ ELEMENT$
140  GOSUB 500_PUSH
150  NEXT 
200  GOSUB 400 POP  AND  PRINT 
210  GOSUB 300_EMPTY AND  PRINT 
220  FOR I = 1 TO 4
230  GOSUB 400 POP  AND  PRINT 
240  NEXT 
250  GOSUB 300_EMPTY AND  PRINT 
260  END 
300  GOSUB 700_EMPTY
310  PRINT "STACK IS ";
320  IF  NOT EMPTY THEN  PRINT "NOT ";
330  PRINT "EMPTY"
340  RETURN 
400  GOSUB 600 POP 
410  PRINT ELEMENT$
420  RETURN 
500  REM 
510  REM PUSH
520  REM 
530  LET STACK$(SP) = ELEMENT$
540  LET SP = SP + 1
550  RETURN 
600  REM 
610  REM POP
620  REM 
630  IF SP THEN SP = SP - 1
640  LET ELEMENT$ = STACK$(SP)
650  LET STACK$(SP) = ""
660  RETURN 
700  REM 
710  REM EMPTY
720  REM 
730  LET EMPTY = SP = 0
740  RETURN

```


{{out}}

```txt
NOT TO BE
STACK IS NOT EMPTY
TO BE OR

PI
(2*A)
STACK IS EMPTY

```



## AutoHotkey


```AutoHotkey
msgbox % stack("push", 4)
msgbox % stack("push", 5)
msgbox % stack("peek")
msgbox % stack("pop")
msgbox % stack("peek")
msgbox % stack("empty")
msgbox % stack("pop")
msgbox % stack("empty")
return 

stack(command, value = 0)
{
  static 
if !pointer 
pointer = 10000
  if (command = "push")
  {
  _p%pointer% := value
  pointer -= 1 
  return value
  }
  if (command = "pop")
  {
    pointer += 1
    return _p%pointer%
  }
  if (command = "peek")
{
next := pointer + 1    
return _p%next%
}
  if (command = "empty")
  {
   if (pointer == 10000)
    return "empty"
else
return 0
  }
}
```



## Axe



```axe
0→S
Lbl PUSH
r₁→{L₁+S}ʳ
S+2→S
Return

Lbl POP
S-2→S
{L₁+S}ʳ
Return

Lbl EMPTY
S≤≤0
Return
```



## Babel


```babel
main : 
    { (1 2 3) foo set     -- foo = (1 2 3)
    4 foo push            -- foo = (1 2 3 4)
    0 foo unshift         -- foo = (0 1 2 3 4)
    foo pop               -- foo = (0 1 2 3)
    foo shift             -- foo = (1 2 3)
    check_foo
    { foo pop } 4 times   -- Pops too many times, but this is OK and Babel won't complain
    check_foo }

empty? : nil?   -- just aliases 'empty?' to the built-in operator 'nil?'

check_foo! : 
    { "foo is " 
    {foo empty?) {nil} {"not " .} ifte 
    "empty" . 
    cr << }

```

{{out}}

```txt

foo is not empty
foo is empty
```



## Batch File

This implementation uses an environment variable naming convention to implement a stack as a pseudo object containing a pseudo dynamic array and top attribute, as well as an empty "method" that is a sort of macro. The implementation depends on delayed expansion being enabled at the time of each call to a stack function. More complex variations can be written that remove this limitation.

```dos
@echo off
setlocal enableDelayedExpansion

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: LIFO stack usage

:: Define the stack
call :newStack myStack

:: Push some values onto the stack
for %%A in (value1 value2 value3) do call :pushStack myStack %%A

:: Test if stack is empty by examining the top "attribute"
if myStack.top==0 (echo myStack is empty) else (echo myStack is NOT empty)

:: Peek at the top stack value
call:peekStack myStack val && echo a peek at the top of myStack shows !val!

:: Pop the top stack value
call :popStack myStack val && echo popped myStack value=!val!

:: Push some more values onto the stack
for %%A in (value4 value5 value6) do call :pushStack myStack %%A

:: Process the remainder of the stack
:processStack
call :popStack myStack val || goto :stackEmpty
echo popped myStack value=!val!
goto :processStack
:stackEmpty

:: Test if stack is empty using the empty "method"/"macro". Use of the
:: second IF statement serves to demonstrate the negation of the empty
:: "method". A single IF could have been used with an ELSE clause instead.
if %myStack.empty% echo myStack is empty
if not %myStack.empty% echo myStack is NOT empty
exit /b

:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
:: LIFO stack definition

:newStack stackName
set /a %~1.top=0
:: Define an empty "method" for this stack as a sort of macro
set "%~1.empty=^!%~1.top^! == 0"
exit /b

:pushStack stackName value
set /a %~1.top+=1
set %~1.!%~1.top!=%2
exit /b

:popStack stackName returnVar
:: Sets errorlevel to 0 if success
:: Sets errorlevel to 1 if failure because stack was empty
if !%~1.top! equ 0 exit /b 1
for %%N in (!%~1.top!) do (
  set %~2=!%~1.%%N!
  set %~1.%%N=
)
set /a %~1.top-=1
exit /b 0

:peekStack stackName returnVar
:: Sets errorlevel to 0 if success
:: Sets errorlevel to 1 if failure because stack was empty
if !%~1.top! equ 0 exit /b 1
for %%N in (!%~1.top!) do set %~2=!%~1.%%N!
exit /b 0
```



## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      STACKSIZE = 1000
      
      FOR n = 3 TO 5
        PRINT "Push ";n : PROCpush(n)
      NEXT
      PRINT "Pop " ; FNpop
      PRINT "Push 6" : PROCpush(6)
      REPEAT
        PRINT "Pop " ; FNpop
      UNTIL FNisempty
      PRINT "Pop " ; FNpop
      END
      
      DEF PROCpush(n) : LOCAL f%
      DEF FNpop : LOCAL f% : f% = 1
      DEF FNisempty : LOCAL f% : f% = 2
      PRIVATE stack(), sptr%
      DIM stack(STACKSIZE-1)
      CASE f% OF
        WHEN 0:
          IF sptr% = DIM(stack(),1) ERROR 100, "Error: stack overflowed"
          stack(sptr%) = n
          sptr% += 1
        WHEN 1:
          IF sptr% = 0 ERROR 101, "Error: stack empty"
          sptr% -= 1
          = stack(sptr%)
        WHEN 2:
          = (sptr% = 0)
      ENDCASE
      ENDPROC
```

{{out}}

```txt

Push 3
Push 4
Push 5
Pop 5
Push 6
Pop 6
Pop 4
Pop 3
Pop
Error: stack empty

```



## beeswax

Beeswax is a stack-based language. The instruction pointers (bees) carry small local stacks (lstacks) of fixed length 3 that can interact with the global stack (gstack) of unrestricted length. The local stacks do not behave exactly like the stack specified in this task, but the global stack does.

'''Push (1)''': <code>f</code> pushes the topmost value of lstack on gstack.

```txt
     instruction: _f

     gstack:      UInt64[0]•         (at the beginning of a program lstack is initialized to [0 0 0]
```


'''Push (2)''': <code>e</code> pushes all three lstack values on gstack, in reversed order.

```txt
     instruction: _e

     gstack:      UInt64[0 0 0]•         (at the beginning of a program lstack is initialized to [0 0 0]
```


'''Push (3)''': <code>i</code> pushes an integer from STDIN as UInt64 value on gstack.

```txt
     instruction: _i
     input:       i123

     gstack:      UInt64[123]•
```


'''Push (4)''': <code>c</code> pushes the Unicode codepoint value of a character from STDIN as UInt64 value on gstack.

```txt
     instruction: _c
     input:       cH

     gstack:      UInt64[72]•
```


'''Push (5)''': <code>V</code> pushes the Unicode codepoint values of the characters of a string given at STDIN as UInt64 values on gstack, last character, followed by newline on top.

```txt
     instruction: _V
     input:       sHello, α∀

     gstack:      UInt64[72 101 108 108 111 44 32 945 8704 10]•
```


'''Pop''':  <code>g{?</code> reads the top value of gstack and stores it on top of lstack. Then outputs top value of lstack to STDOUT and finally pops gstack.

'''Empty''': <code>Ag?';`gstack is empty`</code> pushes length of gstack on gstack, reads top value of gstack, stores it as top value of lstack and prints <code>gstack is empty</code> if lstack top=0.

'''Top''':  <code>g{</code> reads the top value of gstack, stores it on top of lstack. Then outputs top value of lstack to STDOUT. If gstack is empty, this instruction does not do anything but return the topmost value of lstack.

To make sure that there is any value on gstack, you would need to check for gstack length first, using the method shown in the “'''Empty'''” example above:

```txt
*Ag'{`gstack empty, no value to return`
```

This method returns the top value of gstack only if gstack is not empty, otherwise it outputs <code>gstack empty, no value to return</code> to STDOUT.


## Bracmat

A stack is easiest implemented as a dotted list <code>top.top-1.top-2.<i>[...]</i>.</code>. In the example below we also introduce a 'class' <code>stack</code>, instantiated in the 'object' <code>Stack</code>. The class has a member variable <code>S</code> and methods <code>push</code>,<code>pop</code>, <code>top</code> and <code>empty</code>. As a side note, <code>.</code> is to <code>..</code> as C's <code>.</code> is to <code>-&gt;</code>. In a method's body, <code>its</code> refers to the object itself. (Analogous to <code>(*this)</code> in C++.)

```bracmat
( ( stack
  =   (S=)
      (push=.(!arg.!(its.S)):?(its.S))
      ( pop
      = top.!(its.S):(%?top.?(its.S))&!top
      )
      (top=top.!(its.S):(%?top.?)&!top)
      (empty=.!(its.S):)
  )
& new$stack:?Stack
& (Stack..push)$(2*a)
& (Stack..push)$pi
& (Stack..push)$
& (Stack..push)$"to be or"
& (Stack..push)$"not to be"
& out$((Stack..pop)$|"Cannot pop (a)")
& out$((Stack..top)$|"Cannot pop (b)")
& out$((Stack..pop)$|"Cannot pop (c)")
& out$((Stack..pop)$|"Cannot pop (d)")
& out$((Stack..pop)$|"Cannot pop (e)")
& out$((Stack..pop)$|"Cannot pop (f)")
& out$((Stack..pop)$|"Cannot pop (g)")
& out$((Stack..pop)$|"Cannot pop (h)")
&   out
  $ ( str
    $ ( "Stack is "
        ((Stack..empty)$&|not)
        " empty"
      )
    )
& 
);
```

{{out}}

```txt
not to be
to be or
to be or

pi
2*a
Cannot pop (g)
Cannot pop (h)
Stack is  empty
```



## Brat

Built in arrays have push, pop, and empty? methods:


```Brat
stack = []
stack.push 1
stack.push 2
stack.push 3

until { stack.empty? } { p stack.pop }
```


{{out}}

```txt
3
2
1
```



## C

Macro expanding to type flexible stack routines.

```c>#include <stdio.h

#include <stdlib.h>

/* to read expanded code, run through cpp | indent -st */
#define DECL_STACK_TYPE(type, name)					\
typedef struct stk_##name##_t{type *buf; size_t alloc,len;}*stk_##name;	\
stk_##name stk_##name##_create(size_t init_size) {			\
	stk_##name s; if (!init_size) init_size = 4;			\
	s = malloc(sizeof(struct stk_##name##_t));			\
	if (!s) return 0;						\
	s->buf = malloc(sizeof(type) * init_size);			\
	if (!s->buf) { free(s); return 0; }				\
	s->len = 0, s->alloc = init_size;				\
	return s; }							\
int stk_##name##_push(stk_##name s, type item) {			\
	type *tmp;							\
	if (s->len >= s->alloc) {					\
		tmp = realloc(s->buf, s->alloc*2*sizeof(type));		\
		if (!tmp) return -1; s->buf = tmp;			\
		s->alloc *= 2; }					\
	s->buf[s->len++] = item;					\
	return s->len; }						\
type stk_##name##_pop(stk_##name s) {					\
	type tmp;							\
	if (!s->len) abort();						\
	tmp = s->buf[--s->len];						\
	if (s->len * 2 <= s->alloc && s->alloc >= 8) {			\
		s->alloc /= 2;						\
		s->buf = realloc(s->buf, s->alloc * sizeof(type));}	\
	return tmp; }							\
void stk_##name##_delete(stk_##name s) {				\
	free(s->buf); free(s); }

#define stk_empty(s) (!(s)->len)
#define stk_size(s) ((s)->len)

DECL_STACK_TYPE(int, int)

int main(void)
{
	int i;
	stk_int stk = stk_int_create(0);

	printf("pushing: ");
	for (i = 'a'; i <= 'z'; i++) {
		printf(" %c", i);
		stk_int_push(stk, i);
	}

	printf("\nsize now: %d", stk_size(stk));
	printf("\nstack is%s empty\n", stk_empty(stk) ? "" : " not");

	printf("\npoppoing:");
	while (stk_size(stk))
		printf(" %c", stk_int_pop(stk));
	printf("\nsize now: %d", stk_size(stk));
	printf("\nstack is%s empty\n", stk_empty(stk) ? "" : " not");

	/* stk_int_pop(stk); <-- will abort() */
	stk_int_delete(stk);
	return 0;
}
```


### Or


```c>#include <stdio.h

#include <stdlib.h>
#include <stddef.h>
#include <stdbool.h>

#define check_pointer(p) if (!p) {puts("Out of memory."); exit(EXIT_FAILURE);}

#define MINIMUM_SIZE 1
 /* Minimal stack size (expressed in number of elements) for which
 space is allocated. It should be at least 1. */
#define GROWTH_FACTOR 2
 /* How much more memory is allocated each time a stack grows
 out of its allocated segment. */
typedef int T;
 // The type of the stack elements.

typedef struct
 {T *bottom;
  T *top;
  T *allocated_top;} stack;

stack * new(void)
/* Creates a new stack. */
 {stack *s = malloc(sizeof(stack));
  check_pointer(s);
  s->bottom = malloc(MINIMUM_SIZE * sizeof(T));
  check_pointer(s->bottom);
  s->top = s->bottom - 1;
  s->allocated_top = s->bottom + MINIMUM_SIZE - 1;
  return s;}

void destroy(stack *s)
/* Frees all the memory used for a stack. */
 {free(s->bottom);
  free(s);}

bool empty(stack *s)
/* Returns true iff there are no elements on the stack. This
is different from the stack not having enough memory reserved
for even one element, which case is never allowed to arise. */
 {return s->top < s->bottom ? true : false;}

void push(stack *s, T x)
/* Puts a new element on the stack, enlarging the latter's
memory allowance if necessary. */
 {if (s->top == s->allocated_top)
     {ptrdiff_t qtty = s->top - s->bottom + 1;
      ptrdiff_t new_qtty = GROWTH_FACTOR * qtty;
      s->bottom = realloc(s->bottom, new_qtty * sizeof(T));
      check_pointer(s->bottom);
      s->top = s->bottom + qtty - 1;
      s->allocated_top = s->bottom + new_qtty - 1;}
  *(++s->top) = x;}

T pop(stack *s)
/* Removes and returns the topmost element. The result of popping
an empty stack is undefined. */
 {return *(s->top--);}

void compress(stack *s)
/* Frees any memory the stack isn't actually using. The
allocated portion still isn't allowed to shrink smaller than
MINIMUM_SIZE. If all the stack's memory is in use, nothing
happens. */
 {if (s->top == s->allocated_top) return;
  ptrdiff_t qtty = s->top - s->bottom + 1;
  if (qtty < MINIMUM_SIZE) qtty = MINIMUM_SIZE;
  size_t new_size = qtty * sizeof(T);
  s->bottom = realloc(s->bottom, new_size);
  check_pointer(s->bottom);
  s->allocated_top = s->bottom + qtty - 1;}
```


=={{header|C sharp|C#}}==

```csharp
// Non-Generic Stack
System.Collections.Stack stack = new System.Collections.Stack();
stack.Push( obj );
bool isEmpty = stack.Count == 0;
object top = stack.Peek(); // Peek without Popping.
top = stack.Pop();

// Generic Stack
System.Collections.Generic.Stack<Foo> stack = new System.Collections.Generic.Stack<Foo>();
stack.Push(new Foo());
bool isEmpty = stack.Count == 0;
Foo top = stack.Peek(); // Peek without Popping.
top = stack.Pop();
```



## C++

{{libheader|STL}}
The C++ standard library already provides a ready-made stack class. You get it by writing

```cpp>#include <stack></lang

and then using the <tt>std::stack</tt> class.

An example of an explicit implementation of a stack class (which actually implements the standard stack class, except that the standard one is in namespace std):

```cpp>#include <deque

template <class T, class Sequence = std::deque<T> >
class stack {
  friend bool operator== (const stack&, const stack&);
  friend bool operator<  (const stack&, const stack&);
public:
  typedef typename Sequence::value_type      value_type;
  typedef typename Sequence::size_type       size_type;
  typedef          Sequence                  container_type;
  typedef typename Sequence::reference       reference;
  typedef typename Sequence::const_reference const_reference;
protected:
  Sequence seq;
public:
  stack() : seq() {}
  explicit stack(const Sequence& s0) : seq(s0) {}
  bool empty() const { return seq.empty(); }
  size_type size() const { return seq.size(); }
  reference top() { return seq.back(); }
  const_reference top() const { return seq.back(); }
  void push(const value_type& x) { seq.push_back(x); }
  void pop() { seq.pop_back(); }
};

template <class T, class Sequence>
bool operator==(const stack<T,Sequence>& x, const stack<T,Sequence>& y)
{
  return x.seq == y.seq;
}
template <class T, class Sequence>
bool operator<(const stack<T,Sequence>& x, const stack<T,Sequence>& y)
{
  return x.seq < y.seq;
}

template <class T, class Sequence>
bool operator!=(const stack<T,Sequence>& x, const stack<T,Sequence>& y)
{
  return !(x == y);
}
template <class T, class Sequence>
bool operator>(const stack<T,Sequence>& x, const stack<T,Sequence>& y)
{
  return y < x;
}
template <class T, class Sequence>
bool operator<=(const stack<T,Sequence>& x, const stack<T,Sequence>& y)
{
  return !(y < x);
}
template <class T, class Sequence>
bool operator>=(const stack<T,Sequence>& x, const stack<T,Sequence>& y)
{
  return !(x < y);
}
```


== {{header|Clojure}} ==
As is mentioned in the Common Lisp example below, built in cons-based lists work just fine. In this implementation, the list is wrapped in a datatype, providing a stateful solution.

```lisp
(deftype Stack [elements])

(def stack (Stack (ref ())))

(defn push-stack
  "Pushes an item to the top of the stack."
  [x] (dosync (alter (:elements stack) conj x)))

(defn pop-stack
  "Pops an item from the top of the stack."
  [] (let [fst (first (deref (:elements stack)))] 
       (dosync (alter (:elements stack) rest)) fst))

(defn top-stack
  "Shows what's on the top of the stack."
  [] (first (deref (:elements stack))))

(defn empty-stack?
  "Tests whether or not the stack is empty."
  [] (= () (deref (:elements stack))))
```


We can make this a bit smaller and general by using defprotocol along with deftype. Here is a revised version using defprotocol.


```lisp
(defprotocol StackOps
  (push-stack [this x] "Pushes an item to the top of the stack.")
  (pop-stack [this] "Pops an item from the top of the stack.")
  (top-stack [this] "Shows what's on the top of the stack.")
  (empty-stack? [this] "Tests whether or not the stack is empty."))
(deftype Stack [elements]
  StackOps
   (push-stack [x] (dosync (alter elements conj x)))
   (pop-stack [] (let [fst (first (deref elements))]
		   (dosync (alter elements rest)) fst))
   (top-stack [] (first (deref elements)))
   (empty-stack? [] (= () (deref elements))))

(def stack (Stack (ref ())))
```



## COBOL

{{works with|COBOL|2002}}
{{works with|OpenCOBOL|1.1}}

Based loosely on the C stack implementation in Evangel Quiwa's Data Structures.

This example (ab)uses the COPY procedure to ensure that there is a consistently-defined stack type, node type, node information type, p(redicate) type, and set of stack-utilities.

stack.cbl

```COBOL
       01  stack.
         05  head USAGE IS POINTER VALUE NULL.

```


node.cbl

```COBOL
       01  node BASED.
         COPY node-info REPLACING
           01 BY 05
           node-info BY info.
         05  link USAGE IS POINTER VALUE NULL.

```


node-info.cbl

```COBOL
       01  node-info PICTURE X(10) VALUE SPACES.

```


p.cbl

```COBOL
       01  p PICTURE 9.
         88 nil VALUE ZERO WHEN SET TO FALSE IS 1.
         88 t   VALUE 1 WHEN SET TO FALSE IS ZERO.

```


stack-utilities.cbl

```COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. push.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY p.
       COPY node.
       LINKAGE SECTION.
       COPY stack.
       01  node-info-any PICTURE X ANY LENGTH.
       PROCEDURE DIVISION USING stack node-info-any.
         ALLOCATE node
         CALL "pointerp" USING
           BY REFERENCE ADDRESS OF node
           BY REFERENCE p
         END-CALL
         IF nil
           CALL "stack-overflow-error" END-CALL
         ELSE
           MOVE node-info-any TO info OF node
           SET link OF node TO head OF stack
           SET head OF stack TO ADDRESS OF node
         END-IF
         GOBACK.
       END PROGRAM push.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. pop.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY p.
       COPY node.
       LINKAGE SECTION.
       COPY stack.
       COPY node-info.
       PROCEDURE DIVISION USING stack node-info.
         CALL "empty" USING
           BY REFERENCE stack
           BY REFERENCE p
         END-CALL
         IF t
           CALL "stack-underflow-error" END-CALL
         ELSE
           SET ADDRESS OF node TO head OF stack
           SET head OF stack TO link OF node
           MOVE info OF node TO node-info
         END-IF
         FREE ADDRESS OF node
         GOBACK.
       END PROGRAM pop.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. empty.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       COPY stack.
       COPY p.
       PROCEDURE DIVISION USING stack p.
         CALL "pointerp" USING
           BY CONTENT head OF stack
           BY REFERENCE p
         END-CALL
         IF t
           SET t TO FALSE
         ELSE
           SET t TO TRUE
         END-IF
         GOBACK.
       END PROGRAM empty.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. head.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY p.
       COPY node.
       LINKAGE SECTION.
       COPY stack.
       COPY node-info.
       PROCEDURE DIVISION USING stack node-info.
         CALL "empty" USING
           BY REFERENCE stack
           BY REFERENCE p
         END-CALL
         IF t
           CALL "stack-underflow-error" END-CALL
         ELSE
           SET ADDRESS OF node TO head OF stack
           MOVE info OF node TO node-info
         END-IF
         GOBACK.
       END PROGRAM head.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. peek.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       LINKAGE SECTION.
       COPY stack.
       COPY node-info.
       PROCEDURE DIVISION USING stack node-info.
         CALL "head" USING
           BY CONTENT stack
           BY REFERENCE node-info
         END-CALL
         GOBACK.
       END PROGRAM peek.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. pointerp.
       DATA DIVISION.
       LINKAGE SECTION.
       01  test-pointer USAGE IS POINTER.
       COPY p.
       PROCEDURE DIVISION USING test-pointer p.
         IF test-pointer EQUAL NULL
           SET nil TO TRUE
         ELSE
           SET t TO TRUE
         END-IF
         GOBACK.
       END PROGRAM pointerp.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. stack-overflow-error.
       PROCEDURE DIVISION.
         DISPLAY "stack-overflow-error" END-DISPLAY
         STOP RUN.
       END PROGRAM stack-overflow-error.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. stack-underflow-error.
       PROCEDURE DIVISION.
         DISPLAY "stack-underflow-error" END-DISPLAY
         STOP RUN.
       END PROGRAM stack-underflow-error.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. copy-stack.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY p.
       COPY node-info.
       LINKAGE SECTION.
       COPY stack.
       COPY stack REPLACING stack BY new-stack.
       PROCEDURE DIVISION USING stack new-stack.
         CALL "empty" USING
           BY REFERENCE stack
           BY REFERENCE p
         END-CALL
         IF nil
           CALL "pop" USING
             BY REFERENCE stack
             BY REFERENCE node-info
           END-CALL
           CALL "copy-stack" USING
             BY REFERENCE stack
             BY REFERENCE new-stack
           END-CALL
           CALL "push" USING
             BY REFERENCE stack
             BY REFERENCE node-info
           END-CALL
           CALL "push" USING
             BY REFERENCE new-stack
             BY REFERENCE node-info
           END-CALL
         END-IF
         GOBACK.
       END PROGRAM copy-stack.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. reverse-stack.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY p.
       COPY node-info.
       LINKAGE SECTION.
       COPY stack.
       COPY stack REPLACING stack BY new-stack.
       PROCEDURE DIVISION USING stack new-stack.
         CALL "empty" USING
           BY REFERENCE stack
           BY REFERENCE p
         END-CALL
         IF nil
           CALL "pop" USING
             BY REFERENCE stack
             BY REFERENCE node-info
           END-CALL
           CALL "push" USING
             BY REFERENCE new-stack
             BY REFERENCE node-info
           END-CALL
           CALL "reverse-stack" USING
             BY REFERENCE stack
             BY REFERENCE new-stack
           END-CALL
           CALL "push" USING
             BY REFERENCE stack
             BY REFERENCE node-info
           END-CALL
         END-IF
         GOBACK.
       END PROGRAM reverse-stack.

       IDENTIFICATION DIVISION.
       PROGRAM-ID. traverse-stack.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY p.
       COPY node-info.
       COPY stack REPLACING stack BY new-stack.
       LINKAGE SECTION.
       COPY stack.
       PROCEDURE DIVISION USING stack.
         CALL "copy-stack" USING
           BY REFERENCE stack
           BY REFERENCE new-stack
         END-CALL
         CALL "empty" USING
           BY REFERENCE new-stack
           BY REFERENCE p
         END-CALL
         IF nil
           CALL "head" USING
             BY CONTENT new-stack
             BY REFERENCE node-info
           END-CALL
           DISPLAY node-info END-DISPLAY
           CALL "peek" USING
             BY CONTENT new-stack
             BY REFERENCE node-info
           END-CALL
           DISPLAY node-info END-DISPLAY
           CALL "pop" USING
             BY REFERENCE new-stack
             BY REFERENCE node-info
           END-CALL
           DISPLAY node-info END-DISPLAY
           CALL "traverse-stack" USING
             BY REFERENCE new-stack
           END-CALL
         END-IF
         GOBACK.
       END PROGRAM traverse-stack.

```


stack-test.cbl

```COBOL
       IDENTIFICATION DIVISION.
       PROGRAM-ID. stack-test.
       DATA DIVISION.
       LOCAL-STORAGE SECTION.
       COPY stack.
       COPY stack REPLACING stack BY new-stack.
       PROCEDURE DIVISION.
         CALL "push" USING
           BY REFERENCE stack
           BY CONTENT "daleth"
         END-CALL
         CALL "push" USING
           BY REFERENCE stack
           BY CONTENT "gimel"
         END-CALL
         CALL "push" USING
           BY REFERENCE stack
           BY CONTENT "beth"
         END-CALL
         CALL "push" USING
           BY REFERENCE stack
           BY CONTENT "aleph"
         END-CALL
         CALL "traverse-stack" USING
           BY REFERENCE stack
         END-CALL
         CALL "reverse-stack" USING
           BY REFERENCE stack
           BY REFERENCE new-stack
         END-CALL
         CALL "traverse-stack" USING
           BY REFERENCE new-stack
         END-CALL
         STOP RUN.
       END PROGRAM stack-test.

       COPY stack-utilities.

```


{{out}}

```txt

aleph
aleph
beth
beth
beth
gimel
gimel
gimel
daleth
daleth
daleth
daleth
daleth
daleth
gimel
gimel
gimel
beth
beth
beth
aleph
aleph
aleph

```



## CoffeeScript


```CoffeeScript
stack = []
stack.push 1
stack.push 2
console.log stack
console.log stack.pop()
console.log stack
```


== {{header|Common Lisp}} ==
It's a bit unusual to write a wrapper for a stack in Common Lisp; built-in cons-based lists work just fine.  Nonetheless, here's an implementation where the list is wrapped in a structure, providing a stateful solution.

```lisp
(defstruct stack
  elements)

(defun stack-push (element stack)
  (push element (stack-elements stack)))

(defun stack-pop (stack)(deftype Stack [elements])

(defun stack-empty (stack)
  (endp (stack-elements stack)))

(defun stack-top (stack)
  (first (stack-elements stack)))

(defun stack-peek (stack)
  (stack-top stack))
```


## Component Pascal

Works with BlackBox Component Builder

```oberon2

MODULE Stacks;
IMPORT StdLog;

TYPE
	(* some pointers to records *)
	Object* = POINTER TO ABSTRACT RECORD END;
	
	Integer = POINTER TO RECORD (Object)
		i: INTEGER
	END;
	
	Point = POINTER TO RECORD (Object)
		x,y: REAL
	END;

	Node = POINTER TO LIMITED RECORD
		next- : Node;
		data-: ANYPTR;
	END;

	(* Stack *)
	Stack* = POINTER TO RECORD  
		top- : Node;
	END;
	
	PROCEDURE (dn: Object) Show*, NEW, ABSTRACT;
	
	PROCEDURE (i: Integer) Show*;
	BEGIN
		StdLog.String("Integer(");StdLog.Int(i.i);StdLog.String(");");StdLog.Ln
	END Show;
	
	PROCEDURE (p: Point) Show*;
	BEGIN
		StdLog.String("Point(");StdLog.Real(p.x);StdLog.Char(',');
		StdLog.Real(p.y);StdLog.String(");");StdLog.Ln
	END Show;
	
	PROCEDURE (s: Stack) Init, NEW;
	BEGIN
		s.top := NIL;
	END Init;
	
	PROCEDURE (s: Stack) Push*(data: ANYPTR), NEW;
	VAR
		n: Node;
	BEGIN 
		NEW(n);n.next := NIL;n.data := data;
		IF s.top = NIL THEN
			s.top := n
		ELSE
			n.next := s.top;
			s.top := n
		END
	END Push;
	
	PROCEDURE (s: Stack) Pop*(): ANYPTR, NEW;
	VAR
		x: ANYPTR;
	BEGIN
		IF s.top # NIL THEN
			x := s.top.data;
			s.top := s.top.next
		ELSE
			x := NIL
		END;
		RETURN x
	END Pop;
	
	PROCEDURE (s: Stack) Empty*(): BOOLEAN, NEW;
	BEGIN
		RETURN s.top = NIL
	END Empty;
	
	PROCEDURE NewStack*(): Stack;
	VAR
		s: Stack;
	BEGIN
		NEW(s);s.Init;
		RETURN s
	END NewStack;
	
	PROCEDURE NewInteger*(data: INTEGER): Integer;
	VAR
		i: Integer;
	BEGIN
		NEW(i);i.i := data;
		RETURN i
	END NewInteger;
	
	PROCEDURE NewPoint*(x,y: REAL): Point;
	VAR
		p: Point;
	BEGIN
		NEW(p);p.x := x;p.y := y;
		RETURN p
	END NewPoint;
	
	PROCEDURE TestStack*;
	VAR
		s: Stack;
	BEGIN 
		s := NewStack();
		s.Push(NewInteger(1));
		s.Push(NewPoint(2.0,3.4));
		s.Pop()(Object).Show();
		s.Pop()(Object).Show();
	END TestStack;
	
END Stacks.

```


Execute: ^Q Stacks.TestStack<br/>
{{out}}

```txt

Point( 2.0, 3.4);
Integer( 1);

```



## D

Generic stack class implemented with a dynamic array.

```d
import std.array;

class Stack(T) {
    private T[] items;

    @property bool empty() { return items.empty(); }

    void push(T top) { items ~= top; }

    T pop() {
        if (this.empty)
            throw new Exception("Empty Stack.");
        auto top = items.back;
        items.popBack();
        return top;
    }
}

void main() {
    auto s = new Stack!int();
    s.push(10);
    s.push(20);
    assert(s.pop() == 20);
    assert(s.pop() == 10);
    assert(s.empty());
}
```


=={{header|Déjà Vu}}==

```dejavu
local :stack [] #lists used to be stacks in DV

push-to stack 1
push-to stack 2
push-to stack 3

!. pop-from stack #prints 3
!. pop-from stack #prints 2
!. pop-from stack #prints 1

if stack: #empty lists are falsy
    error #this stack should be empty now!
```



## Delphi


```Delphi
program Stack;

{$APPTYPE CONSOLE}

uses Generics.Collections;

var
  lStack: TStack<Integer>;
begin
  lStack := TStack<Integer>.Create;
  try
    lStack.Push(1);
    lStack.Push(2);
    lStack.Push(3);
    Assert(lStack.Peek = 3); // 3 should be at the top of the stack

    Writeln(lStack.Pop); // 3
    Writeln(lStack.Pop); // 2
    Writeln(lStack.Pop); // 1
    Assert(lStack.Count = 0); // should be empty
  finally
    lStack.Free;
  end;
end.
```



## DWScript

Dynamic arrays have pseudo-methods that allow to treat them as a stack.

```Delphi

var stack: array of Integer;

stack.Push(1);
stack.Push(2);
stack.Push(3);

PrintLn(stack.Pop); // 3
PrintLn(stack.Pop); // 2
PrintLn(stack.Pop); // 1

Assert(stack.Length = 0); // assert empty

```



## Dyalect


{{trans|Swift}}


```dyalect
type Stack

static func Stack.Stack() {
    new([])
}

auto func Stack.isEmpty() {
    return valueof(this).len() == 0
}

func Stack.peek() {
    return valueof(this)[valueof(this).len() - 1]
}

func Stack.pop() {
    var e = this.peek()
    valueof(this).removeAt(valueof(this).len() - 1)
    return e
}

func Stack.push(item) {
    valueof(this).add(item)
}
 
var stack = Stack()
stack.push(1)
stack.push(2)
print(stack.pop())
print(stack.peek())
stack.pop()
print(stack.isEmpty)
```


{{out}}


```txt
2
1
true
```



## E

The standard FlexList data structure provides operations for use as a stack.

```e
? def l := [].diverge()
# value: [].diverge()

? l.push(1)
? l.push(2)
? l
# value: [1, 2].diverge()

? l.pop()
# value: 2

? l.size().aboveZero()
# value: true

? l.last()
# value: 1

? l.pop()
# value: 1

? l.size().aboveZero()
# value: false
```

Here's a stack implemented out of a reference to a linked list:

```e
def makeStack() {
    var store := null
    def stack {
        to push(x) { store := [x, store] }
        to pop() { def [x, next] := store; store := next; return x }
        to last() { return store[0] }
        to empty() { return (store == null) }
    }
    return stack
}

? def s := makeStack()
# value: <stack>

? s.push(1)
? s.push(2)
? s.last()
# value: 2

? s.pop()
# value: 2

? s.empty()
# value: false

? s.pop()
# value: 1

? s.empty()
# value: true
```



## EchoLisp

Named stacks are native objects. The following demonstrates the available operations :

```lisp

; build stack [0 1 ... 9 (top)] from a list
(list->stack (iota 10) 'my-stack)
(stack-top 'my-stack) → 9
(pop 'my-stack)  → 9
(stack-top 'my-stack) → 8
(push 'my-stack '🐸) ; any kind of lisp object in the stack
(stack-empty? 'my-stack) → #f
(stack->list 'my-stack) ; convert stack to list
    → (0 1 2 3 4 5 6 7 8 🐸)
(stack-swap 'my-stack) ; swaps two last items 
    → 8 ; new top
(stack->list 'my-stack)
     → (0 1 2 3 4 5 6 7 🐸 8) ; swapped
(while (not (stack-empty? 'my-stack)) (pop 'my-stack)) ; pop until empty
(stack-empty? 'my-stack)  → #t ; true

(push 'my-stack 7)
my-stack ; a stack is not a variable, nor a symbol - cannot be evaluated
   ⛔ error: #|user| : unbound variable : my-stack
(stack-top 'my-stack)  → 7

```



## Eiffel


```Eiffel

class
	STACK_ON_ARRAY

create
	make

feature -- Implementation

	empty: BOOLEAN
		do
			Result := stack.is_empty
		ensure
			empty: Result = (stack.count = 0)
		end

	push (item: ANY)
		do
			stack.force (item, stack.count)
		ensure
			pushed: stack [stack.upper] = item
			growth: stack.count = old stack.count + 1
		end

	pop: ANY
		require
			not_empty: not empty
		do
			Result := stack.at (stack.upper)
			stack.remove_tail (1)
		ensure
			reduction: stack.count = old stack.count - 1
		end

feature {NONE} -- Initialization

	stack: ARRAY [ANY]

	make
		do
			create stack.make_empty
		end

end

```


## Elena


```elena
public program()
{
    var stack := new system'collections'Stack();
 
    stack.push:2;
 
    var isEmpty := stack.Length == 0;
 
    var item := stack.peek(); // Peek without Popping.
 
    item := stack.pop()
}
```



## Elisa

This is a generic Stack component based on arrays. See how in Elisa [http://jklunder.home.xs4all.nl/elisa/part01/doc120.html generic components] are defined. 

```Elisa
 component GenericStack ( Stack, Element );
 type Stack;
      Stack (MaxSize = integer) -> Stack;
      Empty ( Stack )           -> boolean;
      Full ( Stack )            -> boolean;
      Push ( Stack, Element)    -> nothing;
      Pull ( Stack )            -> Element;
begin
      Stack(MaxSize) =
             Stack:[ MaxSize; index:=0; area=array (Element, MaxSize) ];
      Empty( stack ) = (stack.index <= 0);
      Full ( stack ) = (stack.index >= stack.MaxSize);
      Push ( stack, element ) = 
                   [ exception (Full (stack), "Stack Overflow");
                     stack.index:=stack.index + 1; 
                     stack.area[stack.index]:=element ];
      Pull ( stack ) = 
                   [ exception (Empty (stack), "Stack Underflow");
                     stack.index:=stack.index - 1; 
                     stack.area[stack.index + 1] ];
end component GenericStack;
```

Another example of a generic Stack component is based on an unlimited sequence. A sequence is a uni-directional list. See how Elisa defines [http://jklunder.home.xs4all.nl/elisa/part02/doc010.html sequences]. The component has the same interface as the array based version. 

```Elisa
component GenericStack ( Stack, ElementType );
 type Stack;
      Stack(MaxSize = integer)  -> Stack;
      Empty( Stack )            -> boolean;
      Full ( Stack )            -> boolean;
      Push ( Stack, ElementType)-> nothing;
      Pull ( Stack )            -> ElementType;
begin
      type sequence = term;
      ElementType & sequence => sequence;
      nil = null (sequence);

      head (sequence) -> ElementType;
      head (X & Y) = ElementType:X;

      tail (sequence) -> sequence;
      tail (X & Y) = Y;

      Stack (Size) = Stack:[ list = nil ];
      Empty ( stack ) = (stack.list == nil);
      Full ( stack ) = false;
      Push ( stack, ElementType ) = [ stack.list:= ElementType & stack.list ];
      Pull ( stack ) = [ exception (Empty (stack), "Stack Underflow");
                         Head = head(stack.list); stack.list:=tail(stack.list); Head];
end component GenericStack;
```

Both versions give the same answers to the following tests:

```Elisa
use GenericStack (StackofBooks, Book);
type Book = text;
BookStack = StackofBooks(50);

Push (BookStack, "Peter Pan");
Push (BookStack, "Alice in Wonderland");

Pull (BookStack)?
"Alice in Wonderland"

Pull (BookStack)?
"Peter Pan"

Pull (BookStack)?
***** Exception: Stack Underflow
```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Stack do
  def new, do: []
  
  def empty?([]), do: true
  def empty?(_), do: false
  
  def pop([h|t]), do: {h,t}
  
  def push(h,t), do: [h|t]
  
  def top([h|_]), do: h
end
```


Example:

```txt

iex(2)> stack = Stack.new
[]
iex(3)> Stack.empty?(stack)
true
iex(4)> newstack = List.foldl([1,2,3,4,5], stack, fn x,acc -> Stack.push(x,acc) end)
[5, 4, 3, 2, 1]
iex(5)> Stack.top(newstack)
5
iex(6)> {popped, poppedstack} = Stack.pop(newstack)
{5, [4, 3, 2, 1]}
iex(7)> Stack.empty?(newstack)
false

```



## Erlang

Erlang has no built-in stack, but its lists behave basically the same way. A stack module can be implemented as a simple wrapper around lists:

```erlang
-module(stack).
-export([empty/1, new/0, pop/1, push/2, top/1]).

new() -> [].

empty([]) -> true;
empty(_) -> false.

pop([H|T]) -> {H,T}.

push(H,T) -> [H|T].

top([H|_]) -> H.
```

Note that as Erlang doesn't have mutable data structure (destructive updates), pop returns the popped element and the new stack as a tuple.

The module is tested this way:

```erlang>1
 c(stack).
{ok,stack}
2> Stack = stack:new().
[]
3> NewStack = lists:foldl(fun stack:push/2, Stack, [1,2,3,4,5]).
[5,4,3,2,1]
4> stack:top(NewStack).
5
5> {Popped, PoppedStack} = stack:pop(NewStack).
{5,[4,3,2,1]}
6> stack:empty(NewStack).
false
7> stack:empty(stack:new()).
true
```


=={{header|F_Sharp|F#}}==
.NET provides a mutable stack type in <code>System.Collections.Generic.Stack</code>.

A list-based immutable stack type could be implemented like this:

```fsharp
type Stack<'a> //'//(workaround for syntax highlighting problem)
  (?items) =
  let items = defaultArg items []

  member x.Push(A) = Stack(A::items)

  member x.Pop() =
    match items with
      | x::xr ->  (x, Stack(xr))
      | [] -> failwith "Stack is empty."

  member x.IsEmpty() = items = []

// example usage
let anEmptyStack = Stack<int>()
let stack2 = anEmptyStack.Push(42)
printfn "%A" (stack2.IsEmpty())
let (x, stack3) = stack2.Pop()
printfn "%d" x
printfn "%A" (stack3.IsEmpty())
```



## Factor

Factor is a stack based language, but also provides stack "objects", because
all resizable sequences can be treated as stacks (see [http://docs.factorcode.org/content/article-sequences-stacks.html docs]). Typically, a vector is used:

```factor
 V{ 1 2 3 } {
 [ 6 swap push ]
 [ "hi" swap push ]
 [ "Vector is now: " write . ]
 [ "Let's pop it: " write pop . ]
 [ "Vector is now: " write . ]
 [ "Top is: " write last . ] } cleave

 Vector is now: V{ 1 2 3 6 "hi" }
 Let's pop it: "hi"
 Vector is now: V{ 1 2 3 6 }
 Top is: 6
```



## Forth


```forth
: stack ( size -- )
  create here cell+ ,  cells allot ;

: push ( n st -- ) tuck @ !  cell swap +! ;
: pop ( st -- n ) -cell over +!  @ @ ;
: empty? ( st -- ? ) dup @ - cell+ 0= ;

10 stack st

1 st push
2 st push
3 st push
st empty? .  \ 0 (false)
st pop . st pop . st pop .  \ 3 2 1
st empty? .  \ -1 (true)
```



## Fortran

This solution can easily be adapted to data types other than integer.

```fortran
module stack

  public

  ! Define the data-structure to hold the data
  type stack_var
     integer, allocatable :: data(:)
     integer              :: size = 0
  end type stack_var

  ! Set the size of allocated memory blocks
  integer, parameter, private :: block_size = 10

contains

  ! Push ----------------------------------------------------------------------
  subroutine push(s, e)
    type(stack_var), intent(inout) :: s
    integer, intent(in)            :: e
    integer, allocatable :: wk(:)
    if (.not. allocated(s%data)) then
       ! Allocate space if not yet done
       allocate(s%data(block_size))

    elseif (s%size == size(s%data)) then
       ! Grow the allocated space
       allocate(wk(size(s%data)+block_size))
       wk(1:s%size) = s%data
       call move_alloc(wk,s%data)

    end if

    ! Store the data in the stack
    s%size = s%size + 1
    s%data(s%size) = e
  end subroutine push

  ! Pop -----------------------------------------------------------------------
  function pop(s)
    integer :: pop
    type(stack_var), intent(inout) :: s
    if (s%size == 0 .or. .not. allocated(s%data)) then
       pop = 0
       return
    end if
    pop = s%data(s%size)
    s%size = s%size - 1
  end function pop

  ! Peek ----------------------------------------------------------------------
  integer function peek(s)
    type(stack_var), intent(inout) :: s
    if (s%size == 0 .or. .not. allocated(s%data)) then
       peek = 0
       return
    end if
    peek = s%data(s%size)
  end function peek

  ! Empty ---------------------------------------------------------------------
  logical function empty(s)
    type(stack_var), intent(inout) :: s
    empty = (s%size == 0 .or. .not. allocated(s%data))
  end function empty

end module stack

program tstack
  use stack
  implicit none
  type(stack_var) :: s
  integer         :: v

  call push(s,1)
  call push(s,2)
  call push(s,3)
  call push(s,4)

  do
     if (empty(s)) exit
     v = pop(s)
     write(*,'(a,i0)') 'Popped value off stack = ',v
  end do
end program tstack
```



## FreeBASIC

We first use a macro to define a generic Stack type :

```freebasic
' FB 1.05.0 Win64

' stack_rosetta.bi
' simple generic Stack type

#Define Stack(T) Stack_##T

#Macro Declare_Stack(T)
Type Stack(T)
 Public:
    Declare Constructor()
    Declare Destructor()
    Declare Property capacity As Integer
    Declare Property count As Integer 
    Declare Property empty As Boolean
    Declare Property top As T 
    Declare Function pop() As T   
    Declare Sub push(item As T)
  Private:
    a(any) As T 
    count_ As Integer = 0  
    Declare Function resize(size As Integer) As Integer    
End Type

Constructor Stack(T)()
  Redim a(0 To 0) '' create a default T instance for various purposes 
End Constructor

Destructor Stack(T)()
  Erase a
End Destructor

Property Stack(T).capacity As Integer
  Return UBound(a)
End Property
 
Property Stack(T).count As Integer
  Return count_
End Property

Property Stack(T).empty As Boolean
  Return count_ = 0
End Property

Property Stack(T).top As T
  If count_ > 0 Then
    Return a(count_)
  End If
  Print "Error: Attempted to access 'top' element of an empty stack"
  Return a(0)  '' return default element 
End Property

Function Stack(T).pop() As T
  If count_ > 0 Then
    Dim value As T = a(count_)
    a(count_) = a(0)  '' zero element to be removed
    count_ -= 1
    Return value
  End If
  Print "Error: Attempted to remove 'top' element of an empty stack"
  Return a(0)  '' return default element
End Function

Sub Stack(T).push(item As T)
  Dim size As Integer = UBound(a)
  count_ += 1
  If count_ >  size Then
    size = resize(size)
    Redim Preserve a(0 to size)
  End If
  a(count_) = item   
End Sub

Function Stack(T).resize(size As Integer) As Integer
  If size = 0 Then
    size = 4
  ElseIf size <= 32 Then
    size  = 2 * size
  Else
    size += 32
  End If
  Return size
End Function
  
#EndMacro
```


We now use this type to create a Stack of Dog instances :

```freebasic
' FB 1.05.0 Win64

#Include "stack_rosetta.bi"

Type Dog
  name As String
  age As Integer
  Declare Constructor
  Declare Constructor(name_ As string, age_ As integer)
  Declare Operator Cast() As String
end type

Constructor Dog  '' default constructor
End Constructor

Constructor Dog(name_ As String, age_ As Integer)
  name = name_
  age = age_
End Constructor

Operator Dog.Cast() As String
  Return "[" + name + ", " + Str(age) + "]"
End Operator

Declare_Stack(Dog) '' expand Stack type for Dog instances

Dim dogStack As Stack(Dog)

Var cerberus = Dog("Cerberus", 10)
Var rover    = Dog("Rover", 3)
Var puppy    = Dog("Puppy", 0)
With dogStack  '' push these Dog instances onto the stack
  .push(cerberus)
  .push(rover)
  .push(puppy)
End With
Print "Number of dogs on the stack :" ; dogStack.count
Print "Capacity of dog stack       :" ; dogStack.capacity
Print "Top dog                     : "; dogStack.top
dogStack.pop()
Print "Top dog now                 : "; dogStack.top
Print "Number of dogs on the stack :" ; dogStack.count
dogStack.pop()
Print "Top dog now                 : "; dogStack.top
Print "Number of dogs on the stack :" ; dogStack.count
Print "Is stack empty now          : "; dogStack.empty
Print
Print "Press any key to quit"
Sleep
```


{{out}}

```txt

Number of dogs on the stack : 3
Capacity of dog stack       : 4
Top dog                     : [Puppy, 0]
Top dog now                 : [Rover, 3]
Number of dogs on the stack : 2
Top dog now                 : [Cerberus, 10]
Number of dogs on the stack : 1
Is stack empty now          : false

```



## Free Pascal

Example taken and adapted from the Delphi entry.

```pascal
program Stack;
 {$IFDEF FPC}{$MODE DELPHI}{$IFDEF WINDOWS}{$APPTYPE CONSOLE}{$ENDIF}{$ENDIF} 
 {$ASSERTIONS ON} 
uses Generics.Collections;
 
var
  lStack: TStack<Integer>;
begin
  lStack := TStack<Integer>.Create;
  try
    lStack.Push(1);
    lStack.Push(2);
    lStack.Push(3);
    Assert(lStack.Peek = 3); // 3 should be at the top of the stack
 
    Write(lStack.Pop:2);   // 3
    Write(lStack.Pop:2);   // 2
    Writeln(lStack.Pop:2); // 1
    Assert(lStack.Count = 0, 'Stack is not empty'); // should be empty
  finally
    lStack.Free;
  end;
end.
```


```txt

Output:
 3 2 1

```



## Genie


```genie
[indent=4]
/*
   Stack, in Genie, with GLib double ended Queues
   valac stack.gs
*/
init
    var stack = new Queue of int()

    // push
    stack.push_tail(2)
    stack.push_tail(1)

    // pop (and peek at top)
    print stack.pop_tail().to_string()
    print stack.peek_tail().to_string()

    // empty
    print "stack size before clear: " + stack.get_length().to_string()
    stack.clear()
    print "After clear, stack.is_empty(): " + stack.is_empty().to_string()
```


{{out}}

```txt
prompt$ valac stack.gs
prompt$ ./stack
1
2
stack size before clear: 1
After clear, stack.is_empty(): true
```



## Go

Go slices make excellent stacks without defining any extra types, functions, or methods.  For example, to keep a stack of integers, simply declare one as,

```go
var intStack []int
```

Use the built in append function to push numbers on the stack:

```go
intStack = append(intStack, 7)
```

Use a slice expression with the built in len function to pop from the stack:

```go
popped, intStack = intStack[len(intStack)-1], intStack[:len(intStack)-1]
```

The test for an empty stack:

```go
len(intStack) == 0
```

And to peek at the top of the stack:

```go
intStack[len(intStack)-1]
```

It is idiomatic Go to use primitive language features where they are sufficient, and define helper functions or types and methods only as they make sense for a particular situation.  Below is an example using a type with methods and idiomatic "ok" return values to avoid panics.  It is only an example of something that might make sense in some situation.

```go
package main

import "fmt"

type stack []interface{}

func (k *stack) push(s interface{}) {
    *k = append(*k, s)
}

func (k *stack) pop() (s interface{}, ok bool) {
    if k.empty() {
        return
    }
    last := len(*k) - 1
    s = (*k)[last]
    *k = (*k)[:last]
    return s, true
}

func (k *stack) peek() (s interface{}, ok bool) {
    if k.empty() {
        return
    }
    last := len(*k) - 1
    s = (*k)[last]
    return s, true
}

func (k *stack) empty() bool {
    return len(*k) == 0
}

func main() {
    var s stack
    fmt.Println("new stack:", s)
    fmt.Println("empty?", s.empty())
    s.push(3)
    fmt.Println("push 3. stack:", s)
    fmt.Println("empty?", s.empty())
    s.push("four")
    fmt.Println(`push "four" stack:`, s)
    if top, ok := s.peek(); ok {
        fmt.Println("top value:", top)
    } else {
        fmt.Println("nothing on stack")
    }
    if popped, ok := s.pop(); ok {
        fmt.Println(popped, "popped.  stack:", s)
    } else {
        fmt.Println("nothing to pop")
    }
}
```

{{out}}

```txt

new stack: []
empty? true
push 3. stack: [3]
empty? false
push "four" stack: [3 four]
top value: four
four popped.  stack: [3]

```



## Groovy

In Groovy, all lists have stack semantics, including "push()" and "pop()" methods, an "empty" property, and a "last()" method as a stand-in for "top/peek" semantics. Calling "pop()" on an empty list throws an exception.

Of course, these stack semantics are not ''exclusive''. Elements of the list can still be accessed and manipulated in myriads of other ways.

```groovy
def stack = []
assert stack.empty

stack.push(55)
stack.push(21)
stack.push('kittens')
assert stack.last() == 'kittens'
assert stack.size() == 3
assert ! stack.empty
 
println stack

assert stack.pop() == "kittens"
assert stack.size() == 2

println stack

stack.push(-20)

println stack

stack.push( stack.pop() * stack.pop() )
assert stack.last() == -420
assert stack.size() == 2

println stack

stack.push(stack.pop() / stack.pop())
assert stack.size() == 1

println stack

println stack.pop()
assert stack.size() == 0
assert stack.empty

try { stack.pop() } catch (NoSuchElementException e) { println e.message }
```


{{out}}

```txt
[55, 21, kittens]
[55, 21]
[55, 21, -20]
[55, -420]
[-7.6363636364]
-7.6363636364
Cannot pop() an empty List
```



## Haskell

The Haskell solution is trivial, using a list. Note that <code>pop</code> returns both the element and the changed stack, to remain purely functional.

```haskell
type Stack a = [a]

create :: Stack a
create = []

push :: a -> Stack a -> Stack a
push = (:)

pop :: Stack a -> (a, Stack a)
pop []     = error "Stack empty"
pop (x:xs) = (x,xs)

empty :: Stack a -> Bool
empty = null

peek :: Stack a -> a
peek []    = error "Stack empty"
peek (x:_) = x
```

We can make a stack that can be destructively popped by hiding the list inside a <code>State</code> monad.

```haskell
import Control.Monad.State

type Stack a b = State [a] b

push :: a -> Stack a ()
push = modify . (:)

pop :: Stack a a
pop = do
    nonEmpty
    x <- peek
    modify tail
    return x

empty :: Stack a Bool
empty = gets null

peek :: Stack a a
peek = nonEmpty >> gets head

nonEmpty :: Stack a ()
nonEmpty = empty >>= flip when (fail "Stack empty")
```


=={{header|Icon}} and {{header|Unicon}}==
Stacks (and double ended queues) are built into Icon and Unicon as part of normal list access. In addition to 'push' and 'pop', there are the functions 'put', 'get' (alias for pop), 'pull', list element addressing, and list sectioning (like sub-strings).  
Unicon extended 'insert' and 'delete' to work with lists.  
The programmer is free to use any or all of the list processing functions on any problem.  
The following illustrates typical stack usage:

```Icon
procedure main()
stack := []                                     # new empty stack
push(stack,1)                                   # add item
push(stack,"hello",table(),set(),[],5)          # add more items of mixed types in order left to right
y := top(stack)                                 # peek
x := pop(stack)                                 # remove item
write("The stack is ",if isempty(stack) then "empty" else "not empty")
end

procedure isempty(x)           #: test if a datum is empty, return the datum or fail (task requirement)
if *x = 0 then return x        #  in practice just write *x = 0 or *x ~= 0 for is/isn't empty
end

procedure top(x)               #: return top element w/o changing stack
return x[1]                    #  in practice, just use x[1]
end
```



## Io

aside from using built-in lists, a stack can be created using nodes like so:

```io
Node := Object clone do(
    next := nil
    obj := nil
)

Stack := Object clone do(
    node := nil
    
    pop := method(
        obj := node obj
        node = node next
        obj
    )
    
    push := method(obj,
        nn := Node clone
        nn obj = obj
        nn next = self node
        self node = nn
    )
)
```



## Ioke


```ioke
Stack = Origin mimic do(
  initialize = method(@elements = [])
  pop = method(@elements pop!)
  empty = method(@elements empty?)
  push = method(element, @elements push!(element))
)
```


=={{header|IS-BASIC}}==
<lang IS-BASIC>100 LET N=255 ! Size of stack
110 NUMERIC STACK(1 TO N)
120 LET PTR=1
130 DEF PUSH(X)
140   IF PTR>N THEN
150     PRINT "Stack is full.":STOP 
160   ELSE 
170     LET STACK(PTR)=X:LET PTR=PTR+1
180   END IF 
190 END DEF 
200 DEF POP
210   IF PTR=1 THEN
220     PRINT "Stack is empty.":STOP 
230   ELSE 
240     LET PTR=PTR-1:LET POP=STACK(PTR)
250   END IF 
260 END DEF 
270 DEF EMPTY
280   LET PTR=1
290 END DEF 
300 DEF TOP=STACK(PTR-1)
310 CALL PUSH(3):CALL PUSH(5)
320 PRINT POP+POP
```



## J


```J
stack=: ''
push=: monad def '0$stack=:stack,y'
pop=: monad def 'r[ stack=:}:stack[ r=.{:stack'
empty=: monad def '0=#stack'
```

Example use:

```J
   push 9

   pop ''
9
   empty ''
1
```

pop and empty ignore their arguments.  In this implementation. push returns an empty list.


## Java

The collections framework includes a Stack class. Let's test it:

```Java
import java.util.Stack;

public class StackTest {
    public static void main( final String[] args ) {
        final Stack<String> stack = new Stack<String>();

        System.out.println( "New stack empty? " + stack.empty() );

        stack.push( "There can be only one" );
        System.out.println( "Pushed stack empty? " + stack.empty() );
        System.out.println( "Popped single entry: " + stack.pop() );

        stack.push( "First" );
        stack.push( "Second" );
        System.out.println( "Popped entry should be second: " + stack.pop() );

        // Popping an empty stack will throw...
        stack.pop();
        stack.pop();
    }
}
```

{{out}}

```txt
New stack empty? true
Pushed stack empty? false
Popped single entry: There can be only one
Popped entry should be second: Second
Exception in thread "main" java.util.EmptyStackException
	at java.util.Stack.peek(Stack.java:85)
	at java.util.Stack.pop(Stack.java:67)
	at StackTest.main(StackTest.java:21)
```


Alternatively, you might implement a stack yourself...

```java
public class Stack{
    private Node first = null;
    public boolean isEmpty(){
        return first == null;
    }
    public Object Pop(){
        if(isEmpty()) 
            throw new Exception("Can't Pop from an empty Stack.");
        else{
            Object temp = first.value;
            first = first.next;
            return temp;
        }
    }
    public void Push(Object o){
        first = new Node(o, first);
    }
    class Node{
        public Node next;
        public Object value;
        public Node(Object value){
            this(value, null); 
        }
        public Node(Object value, Node next){
            this.next = next;
            this.value = value;
        }
    }
}
```

{{works with|Java|1.5}}

```java5>public class Stack<T
{
    private Node first = null;
    public boolean isEmpty(){
        return first == null;
    }
    public T Pop(){
        if(isEmpty()) 
            throw new Exception("Can't Pop from an empty Stack.");
        else{
            T temp = first.value;
            first = first.next;
            return temp;
        }
    }
    public void Push(T o){
        first = new Node(o, first);
    }
    class Node{
        public Node next;
        public T value;
        public Node(T value){
            this(value, null); 
        }
        public Node(T value, Node next){
            this.next = next;
            this.value = value;
        }
    }
}
```



## JavaScript

The built-in Array class already has stack primitives.

```javascript
var stack = [];
stack.push(1)
stack.push(2,3);
print(stack.pop());   // 3
print(stack.length);   // 2, stack empty if 0
```

Here's a constructor that wraps the array:

```javascript
function Stack() {
    this.data = new Array();

    this.push  = function(element) {this.data.push(element)}
    this.pop   = function() {return this.data.pop()}
    this.empty = function() {return this.data.length == 0}
    this.peek  = function() {return this.data[this.data.length - 1]}
}
```

Here's an example using the revealing module pattern instead of prototypes.

```javascript

function makeStack() {
  var stack = [];

  var popStack = function () {
    return stack.pop();
  };
  var pushStack = function () {
    return stack.push.apply(stack, arguments);
  };
  var isEmpty = function () {
    return stack.length === 0;
  };
  var peekStack = function () {
    return stack[stack.length-1];
  };
    
  return {
    pop: popStack,
    push: pushStack,
    isEmpty: isEmpty,
    peek: peekStack,
    top: peekStack
  };
}

```



## Jsish

From Javascript entry.  Being ECMAScript, Jsi supports stack primitives as part of the Array methods.

```javascript
/* Stack, is Jsish */
var stack = [];
puts('depth:', stack.length);

stack.push(42);
stack.push('abc');
puts('depth:', stack.length);

puts('popped:', stack.pop());
if (stack.length) printf('not '); printf('empty\n');
puts('top:', stack[stack.length-1]);
puts('popped:', stack.pop());
if (stack.length) printf('not '); printf('empty\n');

puts('depth:', stack.length);
```


{{out}}

```txt
prompt$ jsish stack.jsi
depth: 0
depth: 2
popped: abc
not empty
top: 42
popped: 42
empty
depth: 0
```



## Julia

{{works with|Julia|0.6}}

The built-in <code>Array</code> class already has efficient (linear amortized time) stack primitives.

```julia
stack = Int[]           # []
@show push!(stack, 1)   # [1]
@show push!(stack, 2)   # [1, 2]
@show push!(stack, 3)   # [1, 2, 3]
@show pop!(stack)       # 3
@show length(stack)     # 2
@show empty!(stack)     # []
@show isempty(stack)    # true
```



## K


```K
stack:()
push:{stack::x,stack}
pop:{r:*stack;stack::1_ stack;r}
empty:{0=#stack}

/example:
stack:()
  push 3
  stack
,3
  push 5
  stack
5 3
  pop[]
5
  stack
,3
  empty[]
0
  pop[]
3
  stack
!0
  empty[]
1

```



## Kotlin

Rather than use the java.util.Stack<E> class, we will write our own simple Stack<E> class for this task:

```scala
// version 1.1.2

class Stack<E> {
    private val data = mutableListOf<E>()

    val size get() = data.size

    val empty get() = size == 0

    fun push(element: E) = data.add(element)

    fun pop(): E {
        if (empty) throw RuntimeException("Can't pop elements from an empty stack")
        return data.removeAt(data.lastIndex)
    }

    val top: E
        get() {
            if (empty) throw RuntimeException("Empty stack can't have a top element")
            return data.last()
        }

    fun clear() = data.clear()

    override fun toString() = data.toString()
}

fun main(args: Array<String>) {
    val s = Stack<Int>()
    (1..5).forEach { s.push(it) }
    println(s)
    println("Size of stack = ${s.size}")
    print("Popping: ")
    (1..3).forEach { print("${s.pop()} ") }
    println("\nRemaining on stack: $s")
    println("Top element is now ${s.top}")
    s.clear()
    println("After clearing, stack is ${if(s.empty) "empty" else "not empty"}")
    try {
        s.pop()
    }
    catch (e: Exception) {
        println(e.message)
    }
}
```


{{out}}

```txt

[1, 2, 3, 4, 5]
Size of stack = 5
Popping: 5 4 3
Remaining on stack: [1, 2]
Top element is now 2
After clearing, stack is empty
Can't pop elements from an empty stack

```



## lang5


```lang5
: cr  "\n" . ;
: empty?  dup execute length if 0 else -1 then swap drop ;
: pop  dup execute length 1 - extract swap drop ;
: push  dup execute rot append over ;
: s. stack execute . ;

[] '_ set
: stack '_ ;
stack                     # local variable
    1 swap push set
    2 swap push set s. cr # [    1     2  ]
    pop .           s. cr # 2     [    1  ]
    pop drop
    empty? .              # -1
```



## Lasso

Lasso Arrays natively supports push and pop.


```Lasso
local(a) = array

#a->push('a') 
#a->push('b') 
#a->push('c')

#a->pop // c
#a->pop // b
#a->pop // a
#a->pop // null
```



## Liberty BASIC


```lb

global stack$
stack$=""

randomize .51
for i = 1 to 10
    if rnd(1)>0.5 then
        print  "pop => ";pop$()
    else
        j=j+1
        s$ = chr$(j + 64)
        print "push ";s$
        call push s$
    end if
next

print
print "Clean-up"
do
    print  "pop => ";pop$()
loop while not(empty())
print "Stack is empty"

end

'------------------------------------
sub push s$
    stack$=s$+"|"+stack$    'stack
end sub

function pop$()
    if stack$="" then pop$="*EMPTY*": exit function
    pop$=word$(stack$,1,"|")
    stack$=mid$(stack$,instr(stack$,"|")+1)
end function

function empty()
     empty =(stack$="")
end function

```



## Lingo


```lingo
-- parent script "Stack"

property _tos

on push (me, data)
  me._tos = [#data:data, #next:me._tos]
end

on pop (me)
  if voidP(me._tos) then return VOID
  data = me._tos.data
  me._tos = me._tos.next
  return data
end

on peek (me)
  if voidP(me._tos) then return VOID
  return me._tos.data
end

on empty (me)
  return voidP(me.peek())
end
```



## Logo

[[UCB Logo]] has built-in methods for treating lists as stacks. Since they are destructive, they take the name of the stack rather than the list itself.

```logo
make "stack []
push "stack 1
push "stack 2
push "stack 3
print pop "stack   ; 3
print empty? :stack ; false
```



## Logtalk

A stack can be trivially represented using the built-in representation for lists:

```logtalk

:- object(stack).

    :- public(push/3).
    push(Element, Stack, [Element| Stack]).

    :- public(pop/3).
    pop([Top| Stack], Top, Stack).

    :- public(empty/1)
    empty([]).

:- end_object.

```



## Lua

Tables have stack primitives by default:

```lua
stack = {}
table.insert(stack,3)
print(table.remove(stack)) --> 3
```



## M2000 Interpreter

A Stack object can be used as LIFO or FIFO. Push statement push to top of stack. Read pop a value to a variable from top of stack. StackItem(1) read top item without modified stack. Data statement append items to bottom.

```M2000 Interpreter

Module Checkit {
      a=Stack
      Stack a {
            Push 100, 200, 300
      }
      Print StackItem(a, 1)=300
      Stack a {
            Print StackItem(1)=300
            While not empty {
                  Read N
                  Print N
            }
      }
}
Checkit

```


Every module and function has a "current" stack. Number is a read only variable, which pop a value from current stack (or raise error if not number is in top of stack).

User functions get a new stack, and drop it at return. Modules take parent stack, and return stack to parent. So a Module can return values too. In M2000 a call happen without checkig signatures (except for special events calls). We have to leave stack at a proper state, when return from a module.
Return/Execution stack is hidden and different from stack of values.


```M2000 Interpreter

Module Checkit {
      Read a, b
      Print a, b
}
\\ add parameters in a FIFO, and this FIFO merged to current stack
Push 100
Checkit 10, 20
Print StackItem(1)=100
Module Checkit {
      Read a, b
      Print a=20, b=100
}
Checkit 20

Function alfa {
      k=0
      n=0
      while not empty {
            k+=number
            n++
      }
      if n=0 then Error "No parameters found"
      =k/n
}

Print alfa(1,2,3,4)=2.5


```



## Maple


```Maple
with(stack): # load the package, to allow use of short command names

s := stack:-new(a, b):

push(c, s):

# The following statements terminate with a semicolon and print output.
top(s);
pop(s);
pop(s);
empty(s);
pop(s);
empty(s);
```

{{out}}

```txt
                                      c

                                      c

                                      b

                                    false

                                      a

                                    true
```



## Mathematica


```Mathematica
EmptyQ[a_] := If[Length[a] == 0, True, False]
SetAttributes[Push, HoldAll];[a_, elem_] := AppendTo[a, elem]
SetAttributes[Pop, HoldAllComplete]; 
Pop[a_] := If[EmptyQ[a], False, b = Last[a]; Set[a, Most[a]]; b]
Peek[a_] := If[EmptyQ[a], False, Last[a]]

Example use:
stack = {};Push[stack, 1]; Push[stack, 2]; Push[stack, 3]; Push[stack, 4];
Peek[stack]
->4
Pop[stack] 
->4
Peek[stack]
->3
```


=={{header|MATLAB}} / {{header|Octave}}==
Here is a simple implementation of a stack, that works in Matlab and Octave. It is closely related to the queue/fifo example. 

```matlab
mystack = {};
   
% push 
mystack{end+1} = x; 

%pop
x = mystack{end};  mystack{end} = [];

%peek,top
x = mystack{end};

% empty 
isempty(mystack)
```

Below is another solution, that encapsulates the fifo within the object-orientated "class" elements supported by Matlab. The given implementation is exactly the same as the MATLAB FIFO example, except that the "push()" function is modified to add stuff to the end of the queue instead of the beginning. This is a naive implementation, for rigorous applications this should be modified to initialize the LIFO to a buffered size, so that the "pop()" and "push()" functions don't resize the cell array that stores the LIFO's elements, every time they are called. 

To use this implementation you must save this code in a MATLAB script file named "LIFOQueue.m" which must be saved in a folder named @LIFOQueue in your MATLAB directory.

```MATLAB
%This class impliments a standard LIFO queue.
classdef LIFOQueue
    
    properties  
        queue
    end
    
    methods
         
        %Class constructor
        function theQueue = LIFOQueue(varargin)
            
            if isempty(varargin) %No input arguments
                
                %Initialize the queue state as empty
                theQueue.queue = {};
            elseif (numel(varargin) > 1) %More than 1 input arg
                
                %Make the queue the list of input args
                theQueue.queue = varargin;
            elseif iscell(varargin{:}) %If the only input is a cell array
                
                %Make the contents of the cell array the elements in the queue 
                theQueue.queue = varargin{:};
            else %There is one input argument that is not a cell
                
                %Make that one arg the only element in the queue
                theQueue.queue = varargin;
            end
            
        end        
        
        %push() - pushes a new element to the end of the queue
        function push(theQueue,varargin)
            
            if isempty(varargin)
                theQueue.queue(end+1) = {[]};
            elseif (numel(varargin) > 1) %More than 1 input arg
                
                %Make the queue the list of input args
                theQueue.queue( end+1:end+numel(varargin) ) = varargin;
            elseif iscell(varargin{:}) %If the only input is a cell array
                
                %Make the contents of the cell array the elements in the queue 
                theQueue.queue( end+1:end+numel(varargin{:}) ) = varargin{:};
            else %There is one input argument that is not a cell
                
                %Make that one arg the only element in the queue
                theQueue.queue{end+1} = varargin{:};                
            end
            
            %Makes changes to the queue permanent
            assignin('caller',inputname(1),theQueue);  
            
        end
        
        %pop() - pops the first element off the queue
        function element = pop(theQueue)
           
            if empty(theQueue)
                error 'The queue is empty'
            else
                %Returns the first element in the queue
                element = theQueue.queue{end};
                
                %Removes the first element from the queue
                theQueue.queue(end) = [];
                
                %Makes changes to the queue permanent
                assignin('caller',inputname(1),theQueue);
            end
        end
        
        %empty() - Returns true if the queue is empty
        function trueFalse = empty(theQueue)
           
            trueFalse = isempty(theQueue.queue);
            
        end
        
    end %methods
end
```

Sample Usage:

```MATLAB>>
 myLIFO = LIFOQueue(1,'fish',2,'fish','red fish','blue fish')
 
myLIFO =
 
	LIFOQueue

>> myLIFO.pop()

ans =

blue fish

>> myLIFO.push('Cat Fish')
>> myLIFO.pop()

ans =

Cat Fish

>> myLIFO.pop()

ans =

red fish

>> empty(myLIFO)

ans =

     0
```



## Maxima


```maxima
/* lists can be used as stacks; Maxima provides pop and push */

load(basic)$

a: []$
push(25, a)$
push(7, a)$
pop(a);

emptyp(a);
length(a);
```



## Mercury


Efficient, generic stacks are provided as part of the standard library in Mercury.  For sake of illustration, here is how a simple stack could be implemented.


```mercury
:- module sstack.

:- interface.

% We're going to call the type sstack (simple stack) because we don't want to get it
% accidentally confused with the official stack module in the standard library.
:- type sstack(T).

:- func sstack.new = sstack(T).
:- pred sstack.is_empty(sstack(T)::in) is semidet.
:- func sstack.push(sstack(T), T) = sstack(T).
:- pred sstack.pop(T::out, sstack(T)::in, sstack(T)::out) is semidet.

:- implementation.

:- import_module list.

:- type sstack(T)
   --->  sstack(list(T)).

sstack.new = sstack([]).

sstack.is_empty(sstack([])).

sstack.push(Stack0, Elem) = Stack1 :-
   Stack0 = sstack(Elems),
   Stack1 = sstack([Elem | Elems]).

sstack.pop(Elem, !Stack) :-
   !.Stack = sstack([Elem | Elems]),
   !:Stack = sstack(Elems).

:- end_module sstack.
```


It should be noted that this is purely an illustrative example of a very simple stack.  
A real implementation would have predicate (:- pred) versions of the functions (:- func), for example, for consistency's sake with either the functions implemented in terms of the predicates or vice versa.  [http://www.mercurylang.org/information/doc-release/mercury_library/stack.html#stack The real library implementation] also features more functionality including both semi-deterministic and deterministic versions of some functions/predicates as well as the ability to push a list of values in one operation.

Some of the implementation decisions above need an explanation.  
new/0 and push/2 were implemented as functions both for pedagogical reasons (a desire to show function syntax) and because they are a natural fit for functional thought: 0 or more inputs, one output, deterministic.  
is_empty/1 was implemented as a predicate because it's a single, simple succeed/fail test which is precisely what a predicate is in logic.  
pop/3 was implemented as a predicate because it has two outputs (the element and the new stack) ''and'' because it is semi-deterministic (it will fail if the stack is empty).

Note also that while pop/3 has three parameters, the function implementation looks like it has two.  This is because the !Stack "parameter" is actually a ''pair'' of parameters using Mercury's state variable notation.  !Stack is, in effect, two variables: !.Stack and !:Stack, input and output respectively.  Using state variable notation here is a bit of overkill but again was brought in for pedagogical reasons to show the syntax.


## MiniScript


```MiniScript
// Note in Miniscript, a value of zero is false,
// and any other number is true.
// therefore the .len function works as the inverse of a .empty function
stack = [2, 4, 6]
stack.push 8
print "Stack is " + stack
print "Adding '9' to stack " + stack.push(9)
print "Top of stack is " + stack.pop
print "Stack is " + stack
if stack.len then
    print "Stack is not empty"
else
    print "Stack is empty"
end if
```

{{out}}

```txt

Stack is [2, 4, 6, 8]
Adding '9' to stack [2, 4, 6, 8, 9]
Top of stack is 9
Stack is [2, 4, 6, 8]
Stack is not empty

```



## Nanoquery


```nanoquery
class Stack
        declare $internalList

        // constructor
        def Stack()
                $internalList = list()
        end

        def push($val)
                append $internalList $val
        end

        def pop()
                $val = $internalList.get(int(len($internalList) - 1))
                $internalList.remove($val)

                return $val
        end

        def empty()
                return len($internalList) = 0
        end
end
```



## Nemerle

Mutable stacks are available in <tt>System.Collections</tt>, <tt>System.Collections.Generic</tt> and <tt>Nemerle.Collections</tt> depending on what functionality beyond the basics you want. An immutable stack could be implemented fairly easily, as, for example, this quick and dirty list based implementation.

```Nemerle
public class Stack[T]
{
    private stack : list[T];
    
    public this()
    {
        stack = [];
    }
    
    public this(init : list[T])
    {
        stack = init;
    }
    
    public Push(item : T) : Stack[T]
    {
        Stack(item::stack)
    }
    
    public Pop() : T * Stack[T]
    {
        (stack.Head, Stack(stack.Tail))
    }
    
    public Peek() : T
    {
        stack.Head
    }
    
    public IsEmpty() : bool
    {
        stack.Length == 0
    }
}
```



## NetRexx


```netrexx
/* NetRexx ************************************************************
* 13.08.2013 Walter Pachl  translated from REXX version 2
**********************************************************************/
options replace format comments java crossref savelog symbols nobinary

stk = create_stk

say push(stk,123) 'from push'
say empty(stk) 
say peek(stk)     'from peek'
say pull(stk)     'from pull'
say empty(stk) 
Say pull(stk)     'from pull'

method create_stk static returns Rexx
  stk = ''
  stk[0] = 0
  return stk

method push(stk,v) static
  stk[0]=stk[0]+1
  stk[stk[0]]=v
  Return v

method peek(stk) static
  x=stk[0]
  If x=0 Then
    Return 'stk is empty'
  Else
    Return stk[x]

method pull(stk) static
  x=stk[0]
  If x=0 Then
    Return 'stk is empty'
  Else Do
    stk[0]=stk[0]-1
    Return stk[x]
    End

method empty(stk) static
  Return stk[0]=0
```

{{out}}

```txt

123 from push
0
123 from peek
123 from pull
1
stk is empty from pull

```



## Nim


```nim

import math

type
  EStackEmpty = object of E_Base

  TStack* [A] = object 
    data: seq[A]
    count: int

proc initStack*[A](initialSize = 32): TStack[A] =
  assert isPowerOfTwo(initialSize)
  result.count = 0
  newSeq(result.data,initialSize)

proc cap*[A] (s: TStack[A]): int =
  result = s.data.len

proc len*[A](stack: TStack[A]): int =
  result = stack.count

proc push*[A](s: var TStack[A], item: A) = 
  if s.count == s.data.len: 
    # not enough room, make container bigger
    var d: Seq[A]
    newSeq(d,s.len * 2)
    for i in 0 .. s.data.len - 1:
      shallowCopy(d[i],s.data[i])
    shallowCopy(s.data,d)
  s.data[s.count] = item
  inc(s.count)

proc pop*[A](s: var TStack[A]): A {.raises: [EStackEmpty].}= 
  if s.count == 0:
    raise newException(EStackEmpty,"the stack is empty")
  dec(s.count)
  result = s.data[s.count]

proc top*[A](s: TStack[A]): A = 
  result = s.data[s.count - 1]

proc isEmpty*[A](s: var TStack[A]): bool = 
  return s.count == 0

#Tests
when isMainModule:
  var stk: TStack[char] = initStack[char](4)
  stk.push('a')
  stk.push('b')
  stk.push('c')
  stk.push('d')
  
  assert(stk.count == 4)
  assert(stk.data.len == 4)
  stk.push('e')
  assert(stk.cap == 8)
  assert(stk.top == 'e')

  
  discard stk.pop
  discard stk.pop
  discard stk.pop
  discard stk.pop
  assert(stk.isEmpty == false)
  discard stk.pop
  assert(stk.isEmpty == true)

  try:
    discard stk.pop
  except:
    let 
      e = getCurrentException()
      msg = getCurrentExceptionMsg()
    echo "Exception: [[", repr(e), "]] msg: ", msg

  
  

```


=={{header|Oberon-2}}==
{{Works with|oo2c version 2}}

```oberon2

MODULE Stacks;
IMPORT 
  Object,
  Object:Boxed,
  Out := NPCT:Console;

TYPE
  Pool(E: Object.Object) = POINTER TO ARRAY OF E;
  Stack*(E: Object.Object) = POINTER TO StackDesc(E);
  StackDesc*(E: Object.Object) = RECORD
    pool: Pool(E);
    cap-,top: LONGINT;
  END;

  PROCEDURE (s: Stack(E)) INIT*(cap: LONGINT);
  BEGIN
    NEW(s.pool,cap);s.cap := cap;s.top := -1
  END INIT;

  PROCEDURE (s: Stack(E)) Top*(): E;
  BEGIN
    RETURN s.pool[s.top]
  END Top;

  PROCEDURE (s: Stack(E)) Push*(e: E);
  BEGIN
    INC(s.top);
    ASSERT(s.top < s.cap);
    s.pool[s.top] := e;
  END Push;

  PROCEDURE (s: Stack(E)) Pop*(): E;
  VAR
    resp: E;
  BEGIN
    ASSERT(s.top >= 0);
    resp := s.pool[s.top];DEC(s.top);
    RETURN resp
  END Pop;

  PROCEDURE (s: Stack(E)) IsEmpty(): BOOLEAN;
  BEGIN
    RETURN s.top < 0
  END IsEmpty;

  PROCEDURE (s: Stack(E)) Size*(): LONGINT;
  BEGIN
    RETURN s.top + 1
  END Size;

  PROCEDURE Test;
  VAR
    s: Stack(Boxed.LongInt);
  BEGIN
    s := NEW(Stack(Boxed.LongInt),100);
    s.Push(NEW(Boxed.LongInt,10));
    s.Push(NEW(Boxed.LongInt,100));
    Out.String("size: ");Out.Int(s.Size(),0);Out.Ln;
    Out.String("pop: ");Out.Object(s.Pop());Out.Ln;
    Out.String("top: ");Out.Object(s.Top());Out.Ln;
    Out.String("size: ");Out.Int(s.Size(),0);Out.Ln
  END Test;
 
BEGIN 
  Test
END Stacks.

```

{{out}}

```txt

size: 2
pop: 100
top: 10
size: 1

```

{{works with|AOS}}

```oberon2

MODULE Stacks; (** AUTHOR ""; PURPOSE ""; *)

IMPORT
	Out := KernelLog;

TYPE	
	Object = OBJECT
	END Object;
	
	Stack* = OBJECT
	VAR
		top-,capacity-: LONGINT;
		pool: POINTER TO ARRAY OF Object;
		
		PROCEDURE & InitStack*(capacity: LONGINT);
		BEGIN
			SELF.capacity := capacity;
			SELF.top := -1;
			NEW(SELF.pool,capacity)
		END InitStack;
		
		PROCEDURE Push*(a:Object);
		BEGIN
			INC(SELF.top);
			ASSERT(SELF.top < SELF.capacity,100);
			SELF.pool[SELF.top] := a
		END Push;
		
		PROCEDURE Pop*(): Object;
		VAR
			r: Object;
		BEGIN
			ASSERT(SELF.top >= 0);
			r := SELF.pool[SELF.top];
			DEC(SELF.top);RETURN r
		END Pop;
		
		PROCEDURE Top*(): Object;
		BEGIN
			ASSERT(SELF.top >= 0);
			RETURN SELF.pool[SELF.top]
		END Top;
		
		PROCEDURE IsEmpty*(): BOOLEAN;
		BEGIN
			RETURN SELF.top < 0
		END IsEmpty;
		
	END Stack;
	
	BoxedInt = OBJECT
	(Object)
	VAR
		val-: LONGINT;

	PROCEDURE & InitBoxedInt*(CONST val: LONGINT);
	BEGIN	
		SELF.val := val
	END InitBoxedInt;

	END BoxedInt;

	PROCEDURE Test*;
	VAR
		s: Stack;
		bi: BoxedInt;
		obj: Object;
	BEGIN
		NEW(s,10); (* A new stack of ten objects *)
		NEW(bi,100);s.Push(bi);
		NEW(bi,102);s.Push(bi);		
		NEW(bi,104);s.Push(bi);
		Out.Ln;
		Out.String("Capacity:> ");Out.Int(s.capacity,0);Out.Ln;
		Out.String("Size:> ");Out.Int(s.top + 1,0);Out.Ln;
		obj := s.Pop(); obj := s.Pop();
		WITH obj: BoxedInt DO 
			Out.String("obj:> ");Out.Int(obj.val,0);Out.Ln
		ELSE
			Out.String("Unknown object...");Out.Ln;
		END (* with *)
	END Test;
END Stacks.

```

{{out}}

```txt

Capacity:> 10
Size:> 3
obj:> 102

```


## Objeck

Class library support for Stack/IntStack/FloatStack

```objeck
stack := IntStack->New();
stack->Push(13);
stack->Push(7);
(stack->Pop() + stack->Pop())->PrintLine();
stack->IsEmpty()->PrintLine();
```


=={{header|Objective-C}}==
Using a NSMutableArray:

```objc
NSMutableArray *stack = [NSMutableArray array]; // creating

[stack addObject:value]; // pushing

id value = [stack lastObject];
[stack removeLastObject]; // popping

[stack count] == 0 // is empty?
```



## OCaml

Implemented as a singly-linked list, wrapped in an object:

```ocaml
exception Stack_empty

class ['a] stack =
  object (self)
    val mutable lst : 'a list = []

    method push x =
      lst <- x::lst

    method pop =
      match lst with
        []    -> raise Stack_empty
      | x::xs -> lst <- xs;
                 x

    method is_empty =
      lst = []
  end
```



## Oforth


Stack is already defined at startup.


```Oforth
ListBuffer Class new: Stack
Stack method: push  self add ;
Stack method: pop   self removeLast ;
Stack method: top   self last ;
```


Usage : 

```Oforth
: testStack
| s |
   Stack new ->s
   s push(10)
   s push(11)
   s push(12)
   s top println
   s pop println
   s pop println
   s pop println
   s isEmpty ifTrue: [ "Stack is empty" println ] ;
```


{{out}}

```txt

12
12
11
10
Stack is empty

```



## ooRexx

The ooRexx queue class functions as a stack as well (it is a dequeue really).  

```ooRexx

stack = .queue~of(123, 234)  -- creates a stack with a couple of items
stack~push("Abc")   -- pushing
value = stack~pull  -- popping
value = stack~peek  -- peeking
-- the is empty test
if stack~isEmpty then say "The stack is empty"

```



## OxygenBasic

The real stack is freely available!

```oxygenbasic

function f()
  sys a=1,b=2,c=3,d=4
  push a
  push b
  push c
  push d
  print a "," b "," c "," d 'result 1,2,3,4
  a=10
  b=20
  c=30
  d=40
  print a "," b "," c "," d 'result 10,20,30,40
  pop a
  pop b
  pop c
  pop d
  print a "," b "," c "," d 'result 4,3,2,1
end function

f

```



## Oz

A thread-safe, list-based stack. Implemented as a module:

```oz
functor
export
   New
   Push
   Pop
   Empty
define   
   fun {New}
      {NewCell nil}
   end

   proc {Push Stack Element}
      NewStack
      %% Use atomic swap for thread safety
      OldStack = Stack := NewStack
   in
      NewStack = Element|OldStack
   end

   proc {Pop Stack ?Result}
      NewStack
      %% Use atomic swap for thread safety
      OldStack = Stack := NewStack
   in
      Result|NewStack = OldStack
   end
   
   fun {Empty Stack}
      @Stack == nil
   end
end
```

There is also a stack implementation in the [http://www.mozart-oz.org/home/doc/mozart-stdlib/adt/stack.html standard library].


## PARI/GP


```parigp
push(x)=v=concat(v,[x]);;
pop()={
  if(#v,
    my(x=v[#v]);
    v=vecextract(v,1<<(#v-1)-1);
    x
  ,
    error("Stack underflow")
  )
};
empty()=v==[];
peek()={
  if(#v,
    v[#v]
  ,
    error("Stack underflow")
  )
};
```



## Pascal

This implements stacks of integers in standard Pascal (should work on all existing Pascal dialects).

```pascal
{ tStack is the actual stack type, tStackNode a helper type }
type
  pStackNode = ^tStackNode;
  tStackNode = record
                next: pStackNode;
                data: integer;
               end;
  tStack = record
            top: pStackNode;
           end;

{ Always call InitStack before using a stack }
procedure InitStack(var stack: tStack);
 begin
  stack.top := nil
 end;

{ This function removes all content from a stack; call before disposing, or before a local stack variable goes out of scope }
procedure ClearStack(var stack: tStack);
 var
  node: pStackNode;
 begin
  while stack.top <> nil do
   begin
    node := stack.top;
    stack.top := stack.top^.next;
    dispose(node);
   end
 end;

function StackIsEmpty(stack: tStack):Boolean;
 begin
  StackIsEmpty := stack.top = nil
 end;

procedure PushToStack(var stack: tStack; value: integer);
 var
  node: pStackNode;
 begin
  new(node);
  node^.next := stack.top;
  node^.data := value;
  stack.top := node
 end;

{ may only be called on a non-empty stack! }
function PopFromStack(var stack: tStack): integer;
 var
  node: pStackNode;
 begin
  node := stack.top;
  stack.top := node^.next;
  PopFromStack := node^.data;
  dispose(node);
 end;
```



## Perl

Perl comes prepared to treat its arrays as stacks, giving us the push and pop functions for free. To add empty, we basically give a new name to "not":

```perl
sub empty{ not @_ }
```



## Perl 6

Perl 6 still has the stack functions from Perl 5, but now they also can be accessed by object notation: 

```perl6
my @stack;          # just a array
@stack.push($elem); # add $elem to the end of @stack
$elem = @stack.pop; # get the last element back
@stack.elems == 0   # true, because the stack is empty
not @stack          # also true because @stack is false
```



## Phix

I felt it would be helpful to contrast simple, naieve and proper implementations.

v1: simple, but limited to a single stack

```Phix
sequence stack = {}

procedure push(object what)
    stack = append(stack,what)
end procedure

function pop()
    object what = stack[$]
    stack = stack[1..$-1]
    return what
end function

function empty()
    return length(stack)=0
end function

?empty()                -- 1
push(5)
?empty()                -- 0
push(6)
?pop()                  -- 6
?pop()                  -- 5
?empty()                -- 1
```

v2: naieve, multiple stacks but slightly awkward calling convention

```Phix
function push(sequence stack, object what)
    stack = append(stack,what)
    return stack
end function

function pop(sequence stack)
    object what = stack[$]
    stack = stack[1..$-1]
    return {stack,what}
end function

function empty(sequence stack)
    return length(stack)=0
end function

sequence stack = {}
?empty(stack)               -- 1
stack = push(stack,5)
?empty(stack)               -- 0
stack = push(stack,6)
integer top
{stack,top} = pop(stack)
?top                        -- 6
{stack,top} = pop(stack)
?top                        -- 5
?empty(stack)               -- 1
```

v3: multiple stacks, better calling convention

```Phix
sequence stacks = {}
integer freelist = 0

function new_stack()
integer res = freelist
    if res!=0 then
        freelist = stacks[freelist]
        stacks[res] = {}
    else
        stacks = append(stacks,{})
        res = length(stacks)
    end if
    return res
end function

procedure free_stack(integer sid)
    stacks[sid] = freelist
    freelist = sid
end procedure

procedure push(integer sid, object what)
    stacks[sid] = append(stacks[sid],what)
end procedure

function pop(integer sid)
    object res = stacks[sid][$]
    stacks[sid] = stacks[sid][1..$-1]
    return res
end function

function empty(integer sid)
    return length(stacks[sid])=0
end function

integer sid = new_stack()
?empty(sid)                 -- 1
push(sid,5) 
?empty(sid)                 -- 0
push(sid,6)
?pop(sid)                   -- 6
?pop(sid)                   -- 5
?empty(sid)                 -- 1
free_stack(sid)
```

v1 and v2 are thread-safe as long as only one thread is using a particular stack.
full thread-safety for v3 would require something similar to new_dict, see docs.


## PHP

PHP arrays behave like a stack:

```php
$stack = array();

empty( $stack ); // true

array_push( $stack, 1 ); // or $stack[] = 1;
array_push( $stack, 2 ); // or $stack[] = 2;

empty( $stack ); // false

echo array_pop( $stack ); // outputs "2"
echo array_pop( $stack ); // outputs "1"
```



## PicoLisp

The built-in functions [http://software-lab.de/doc/refP.html#push push] and
[http://software-lab.de/doc/refP.html#pop pop] are used to maintain a stack (of any type).

```PicoLisp
(push 'Stack 3)
(push 'Stack 2)
(push 'Stack 1)
```


```txt
: Stack
-> (1 2 3)

: (pop 'Stack)
-> 1

: Stack
-> (2 3)

: (set 'Stack)  # empty
-> NIL

: Stack
-> NIL
```



## PL/I


```PL/I
/* Any controlled variable may behave as a stack. */

declare s float controlled;

/* to push a value on the stack. */
allocate s;
s = 10;

/* To pop a value from the stack. */
put (s);
free s;

/* to peek at the top of stack> */
put (s);

/* To see whether the stack is empty */
if allocation(s) = 0 then ...

/* Note: popping a value from the stack, or peeking,          */
/* would usually require a check that the stack is not empty. */

/* Note: The above is a simple stack for S. */
/* S can be any kind of data structure, an array, etc. */

/* Example to push ten values onto the stack, and then to */
/* remove them.                                           */

/* Push ten values, obtained from the input, onto the stack: */
declare S float controlled;
do i = 1 to 10;
   allocate s;
   get list (s);
end;
/* To pop those values from the stack: */
do while (allocation(s) > 0);
   put skip list (s);
   free s;
end;
/* The values are printed in the reverse order, of course. */
```



## PostScript

{{libheader|initlib}}

```postscript
% empty? is already defined.
/push {exch cons}.
/pop {uncons exch pop}.
[2 3 4 5 6] 1 push
= [1 2 3 4 5 6]
[1 2 3 4 5 6] pop
=[2 3 4 5 6]
[2 3 4 5 6] empty?
=false
[] empty?
=true
```




## PowerShell

A new stack:

```PowerShell

$stack = New-Object -TypeName System.Collections.Stack
# or
$stack = [System.Collections.Stack] @()

```

Push some stuff on the stack:

```PowerShell

1, 2, 3, 4 | ForEach-Object {$stack.Push($_)}

```

Show stack as a string:

```PowerShell

$stack -join ", "

```

{{Out}}

```txt

4, 3, 2, 1

```

Pop the top level of the stack:

```PowerShell

$stack.Pop()

```

{{Out}}

```txt

4

```

Show stack as a string:

```PowerShell

$stack -join ", "

```

{{Out}}

```txt

3, 2, 1

```

Get a copy of the top level of the stack:

```PowerShell

$stack.Peek()

```

{{Out}}

```txt

3

```

The stack:

```PowerShell

$stack

```

{{Out}}

```txt

3
2
1

```



## Prolog

Prolog is a particularly silly language to implement stack functions in, as the built-in lists can be treated as stacks in an ad hoc manner. Nonetheless, in the name of completeness:

```prolog
% push( ELEMENT, STACK, NEW )
% True if NEW is [ELEMENT|STACK]
push(ELEMENT,STACK,[ELEMENT|STACK]).

% pop( STACK, TOP, NEW )
% True if TOP and NEW are head and tail, respectively, of STACK
pop([TOP|STACK],TOP,STACK).

% empty( STACK )
% True if STACK is empty
empty([]).
```



## PureBasic

For LIFO function PureBasic normally uses linked lists.
Usage as described above could look like;

```PureBasic
Global NewList MyStack()

Procedure Push_LIFO(n)
  FirstElement(MyStack())
  InsertElement(MyStack())
  MyStack() = n
EndProcedure

Procedure Pop_LIFO()
  If FirstElement(MyStack())
    Topmost = MyStack()
    DeleteElement(MyStack())
  EndIf
  ProcedureReturn Topmost
EndProcedure

Procedure Empty_LIFO()
  Protected Result
  If ListSize(MyStack())=0
    Result = #True
  EndIf
  ProcedureReturn Result
EndProcedure

Procedure Peek_LIFO()
  If FirstElement(MyStack())
    Topmost = MyStack()
  EndIf
  ProcedureReturn Topmost
EndProcedure

;----   Example of implementation ----
Push_LIFO(3)
Push_LIFO(1)
Push_LIFO(4)
While Not Empty_LIFO()
  Debug Pop_LIFO()
Wend
```


{{out}}

```txt

 4
 1
 3

```



## Python

{{works with|Python|2.5}}
The faster and Pythonic way is using a deque (available from 2.4). 
A regular list is a little slower.

```python
from collections import deque
stack = deque()
stack.append(value) # pushing
value = stack.pop()
not stack # is empty?
```

If you need to expose your stack to the world, you may want to create a simpler wrapper:

```python
from collections import deque

class Stack:
    def __init__(self):
        self._items = deque()
    def append(self, item):
        self._items.append(item)
    def pop(self):
        return self._items.pop()
    def __nonzero__(self):
        return bool(self._items)
```

Here is a stack implemented as linked list - with the same list interface.

```python
class Stack:
    def __init__(self):
        self._first = None
    def __nonzero__(self):
        return self._first is not None 
    def append(self, value):
        self._first = (value, self._first)
    def pop(self):
        if self._first is None:
            raise IndexError, "pop from empty stack"
        value, self._first = self._first
        return value
```

Notes:

Using list interface - append, __nonzero__ make it easier to use, cleanup the client code, and allow changing the implementation later without affecting the client code. 
For example, instead of:

```python
while not stack.empty():
```

You can write:

```python>while stack:</lang

Quick testing show that deque is about 5 times faster then the wrapper linked list implementations. This may be important if your stack is used in tight loops.


## R

{{libheader|proto}}
See [[FIFO]] for functional and object oriented implementations of a First-In-First-Out object, with similar code.

```R
library(proto)

stack <- proto(expr = {
   l <- list()
   empty <- function(.) length(.$l) == 0
   push <- function(., x) 
   {
      .$l <- c(list(x), .$l)
      print(.$l)
      invisible()
   }
   pop <- function(.) 
   {
      if(.$empty()) stop("can't pop from an empty list")
      .$l[[1]] <- NULL
      print(.$l)
      invisible()
   }
})

stack$empty()
# [1] TRUE
stack$push(3)
# [[1]]
# [1] 3
stack$push("abc")
# [[1]]
# [1] "abc"
# [[2]]
# [1] 3
stack$push(matrix(1:6, nrow=2))
# [[1]]
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6
# [[2]]
# [1] "abc"
# [[3]]
# [1] 3
stack$empty()
# [1] FALSE
stack$pop()
# [[1]]
[1] "abc"
# [[2]]
# [1] 3
stack$pop()
# [[1]]
# [1] 3
stack$pop()
# list()
stack$pop()
# Error in get("pop", env = stack, inherits = TRUE)(stack, ...) : 
#   can't pop from an empty list
```



## Racket


Quick functional version:


```Racket

#lang racket
(define stack '())
(define (push x stack) (cons x stack))
(define (pop stack) (values (car stack) (cdr stack)))
(define (empty? stack) (null? stack))

```


And a destructive object:


```Racket

(struct stack ([items #:auto]) #:mutable #:auto-value '())
(define (push! x stack)
  (set-stack-items! stack (cons x (stack-items stack))))
(define (pop! stack)
  (begin0 (car (stack-items stack))
    (set-stack-items! stack (cdr (stack-items stack)))))
(define (empty? stack)
  (null? (stack-items stack)))

```



## Raven

Use built in ''stack'' type:

```raven
new stack as s
1 s push
s pop
```

Word ''empty'' is also built in:

```raven
s empty if 'stack is empty' print
```



## REBOL


```rebol
REBOL [
	Title: "Stack"
	URL: http://rosettacode.org/wiki/Stack
]

stack: make object! [
	data: copy []

	push: func [x][append data x]
	pop: func [/local x][x: last data  remove back tail data  x]
	empty: does [empty? data]

	peek: does [last data]
]

; Teeny Tiny Test Suite
 
assert: func [code][print [either do code ["  ok"]["FAIL"]  mold code]]

print "Simple integers:"
s: make stack []  s/push 1  s/push 2 ; Initialize.

assert [2 = s/peek]
assert [2 = s/pop]
assert [1 = s/pop]
assert [s/empty]

print [lf "Symbolic data on stack:"]
v: make stack [data: [this is a test]] ; Initialize on instance.

assert ['test = v/peek]
assert ['test = v/pop]
assert ['a = v/pop]
assert [not v/empty]
```

Sample run:

```txt
Simple integers:
  ok [2 = s/peek]
  ok [2 = s/pop]
  ok [1 = s/pop]
  ok [s/empty]

Symbolic data on stack:
  ok ['test = v/peek]
  ok ['test = v/pop]
  ok ['a = v/pop]
  ok [not v/empty]

```



## Retro


```Retro
: stack ( n"-  ) create 0 , allot ;
: push  ( na-  ) dup ++ dup @ + ! ;
: pop   (  a-n ) dup @ over -- + @ ;
: top   (  a-n ) dup @ + @ ;
: empty? ( a-f ) @ 0 = ;

10 stack st

1 st push
2 st push
3 st push
st empty? putn
st top putn
st pop putn st pop putn st pop putn
st empty? putn
```



## REXX


### version 1


```rexx
y=123                        /*define a REXX variable, value is 123  */
push y                       /*pushes   123   onto the stack.        */
pull g                       /*pops last value stacked & removes it. */
q=empty()                    /*invokes the  EMPTY  subroutine (below)*/
exit                         /*stick a fork in it, we're done.       */

empty: return queued()       /*subroutine returns # of stacked items.*/
```



### version 2


```rexx
/* REXX ***************************************************************
* supports push, pull, and peek
* 11.08.2013 Walter Pachl
**********************************************************************/
stk.=0
Call push 123
Say empty()
say peek()
say pull()
Say empty()
say peek()
say push(456)
say peek()
Exit

push: Procedure Expose stk.
  Parse Arg v
  z=stk.0+1
  stk.z=v
  stk.0=z
  Return v

peek: Procedure Expose stk.
  If stk.0=0 Then
    Return 'stack is empty'
  Else Do
    z=stk.0
    Return stk.z
    End

pull: Procedure Expose stk.
  If stk.0=0 Then
    Return 'stack is empty'
  Else Do
    z=stk.0
    res=stk.z
    stk.0=stk.0-1
    Return res
    End

empty: Procedure Expose stk.
  Return stk.0=0
```

{{out}}

```txt

0
123
123
1
stack is empty
456
456

```



## Ring


```ring

# Project : Stack

load "stdlib.ring"
ostack = new stack
for n = 5 to 7
     see "Push: " + n + nl
     ostack.push(n) 
next
see "Pop:" + ostack.pop() + nl
see "Push: " + "8" + nl
ostack.push(8)
while len(ostack) > 0
        see "Pop:" + ostack.pop() + nl
end
if len(ostack) = 0
   see "Pop: stack is empty" + nl
ok

```

Output:

```txt

Push: 5
Push: 6
Push: 7
Pop:7
Push: 8
Pop:8
Pop:6
Pop:5
Pop: stack is empty

```



## Ruby

Using an Array, there are already methods Array#push, Array#pop and Array#empty?.

```ruby
stack = []
stack.push(value) # pushing
value = stack.pop # popping
stack.empty? # is empty?
```

If you need to expose your stack to the world, you may want to create a simpler wrapper. Here is a wrapper class ''Stack'' that wraps ''Array'' but only exposes stack methods.

```ruby
require 'forwardable'

# A stack contains elements in last-in, first-out order.
# Stack#push adds new elements to the top of the stack;
# Stack#pop removes elements from the top.
class Stack
  extend Forwardable
  
  # Creates a Stack containing _objects_.
  def self.[](*objects)
    new.push(*objects)
  end
  
  # Creates an empty Stack.
  def initialize
    @ary = []
  end
  
  # Duplicates a Stack.
  def initialize_copy(obj)
    super
    @ary = @ary.dup
  end
  
  # Adds each object to the top of this Stack. Returns self.
  def push(*objects)
    @ary.push(*objects)
    self
  end
  alias << push
  
  ##
  # :method: pop
  # :call-seq:
  #   pop -> obj or nil
  #   pop(n) -> ary
  #
  # Removes an element from the top of this Stack, and returns it.
  # Returns nil if the Stack is empty.
  #
  # If passing a number _n_, removes the top _n_ elements, and returns
  # an Array of them. If this Stack contains fewer than _n_ elements,
  # returns them all. If this Stack is empty, returns an empty Array.
  def_delegator :@ary, :pop
  
  ##
  # :method: top
  # :call-seq:
  #   top -> obj or nil
  #   top(n) -> ary
  # Returns the topmost element without modifying the stack.
  def_delegator :@ary, :last, :top
  
  ##
  # :method: empty?
  # Returns true if this Stack contains no elements.
  def_delegator :@ary, :empty?
  
  ##
  # :method: size
  # Returns the number of elements in this Stack.
  def_delegator :@ary, :size
  alias length size
  
  # Converts this Stack to a String.
  def to_s
    "#{self.class}#{@ary.inspect}"
  end
  alias inspect to_s
end
```



```ruby>p s = Stack.new                 # =
 Stack[]
p s.empty?                      # => true
p s.size                        # => 0
p s.top                         # => nil
p s.pop                         # => nil
p s.pop(1)                      # => []
p s.push(1)                     # => Stack[1]
p s.push(2, 3)                  # => Stack[1, 2, 3]
p s.top                         # => 3
p s.top(2)                      # => [2, 3]
p s                             # => Stack[1, 2, 3]
p s.size                        # => 3
p s.pop                         # => 3
p s.pop(1)                      # => [2]
p s.empty?                      # => false

p s = Stack[:a, :b, :c]         # => Stack[:a, :b, :c]
p s << :d                       # => Stack[:a, :b, :c, :d]
p s.pop                         # => :d
```


Just meeting the requirements of a push, pop and empty method:

```ruby
require 'forwardable'

class Stack
  extend Forwardable

  def initialize
    @stack = []
  end

  def_delegators :@stack, :push, :pop, :empty?
end

```

(push takes multiple arguments; pop takes an optional argument which specifies how many to pop)


## Run BASIC


```runbasic
dim stack$(10)   ' stack of ten
global stack$
global stackEnd

for i = 1 to 5                                      ' push 5 values to the stack
 a$ = push$(chr$(i + 64))
 print "Pushed ";chr$(i + 64);" stack has ";stackEnd
next i

print "Pop Value:";pop$();" stack has ";stackEnd  ' pop last in
print "Pop Value:";pop$();" stack has ";stackEnd  ' pop last in

e$ = mt$()                                        ' MT the stack
print "Empty stack. stack has ";stackEnd

' ------ PUSH the stack
FUNCTION push$(val$)
stackEnd = stackEnd + 1                            ' if more than 10 then lose the oldest
if stackEnd > 10 then
   for i = 0 to 9
      stack$(i) = stack$(i+1)
   next i
   stackEnd   = 10
end if
stack$(stackEnd) = val$
END FUNCTION

' ------ POP the stack -----
FUNCTION pop$()
if stackEnd = 0 then 
   pop$     = "Stack is MT"
  else
   pop$     = stack$(stackEnd)                        ' pop last in
   stackEnd = max(stackEnd - 1,0)
end if
END FUNCTION

' ------ MT the stack ------
FUNCTION mt$()
  stackEnd = 0
END FUNCTION
```

{{out}}

```txt
Pushed A stack has 1
Pushed B stack has 2
Pushed C stack has 3
Pushed D stack has 4
Pushed E stack has 5
Pop Value:E stack has 4
Pop Value:D stack has 3
Empty stack. stack has 0
```



## Rust


### Using the standard library


One could just use a vector (<code>Vec<T></code>) which is part of the standard library


```rust
fn main() {
    let mut stack = Vec::new();
    stack.push("Element1");
    stack.push("Element2");
    stack.push("Element3");

    assert_eq!(Some(&"Element3"), stack.last());
    assert_eq!(Some("Element3"), stack.pop());
    assert_eq!(Some("Element2"), stack.pop());
    assert_eq!(Some("Element1"), stack.pop());
    assert_eq!(None, stack.pop());
}
```



### Simple implementation

Simply uses a singly-linked list.

```rust>type Link<T> = Option<Box<Frame<T>>
;

pub struct Stack<T> {
    head: Link<T>,
}
struct Frame<T> { 
    elem: T,
    next: Link<T>,
}

/// Iterate by value (consumes list)
pub struct IntoIter<T>(Stack<T>); 
impl<T> Iterator for IntoIter<T> {
    type Item = T;
    fn next(&mut self) -> Option<Self::Item> {
        self.0.pop()
    }
}

/// Iterate by immutable reference
pub struct Iter<'a, T: 'a> { 
    next: Option<&'a Frame<T>>,
}
impl<'a, T> Iterator for Iter<'a, T> { // Iterate by immutable reference
    type Item = &'a T;
    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|frame| {
            self.next = frame.next.as_ref().map(|frame| &**frame);
            &frame.elem
        })
    }
}

/// Iterate by mutable reference
pub struct IterMut<'a, T: 'a> {
    next: Option<&'a mut Frame<T>>,
}
impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = &'a mut T;
    fn next(&mut self) -> Option<Self::Item> {
        self.next.take().map(|frame| {
            self.next = frame.next.as_mut().map(|frame| &mut **frame);
            &mut frame.elem
        })
    }
}


impl<T> Stack<T> {
    /// Return new, empty stack
    pub fn new() -> Self {
        Stack { head: None }
    }

    /// Add element to top of the stack
    pub fn push(&mut self, elem: T) {
        let new_frame = Box::new(Frame {
            elem: elem,
            next: self.head.take(),
        });
        self.head = Some(new_frame);
    }

    /// Remove element from top of stack, returning the value
    pub fn pop(&mut self) -> Option<T> {
        self.head.take().map(|frame| { 
            let frame = *frame;
            self.head = frame.next;
            frame.elem
        })
    }

    /// Get immutable reference to top element of the stack
    pub fn peek(&self) -> Option<&T> {
        self.head.as_ref().map(|frame| &frame.elem)
    }

    /// Get mutable reference to top element on the stack
    pub fn peek_mut(&mut self) -> Option<&mut T> {
        self.head.as_mut().map(|frame| &mut frame.elem)
    }

    /// Iterate over stack elements by value
    pub fn into_iter(self) -> IntoIter<T> {
        IntoIter(self)
    }

    /// Iterate over stack elements by immutable reference
    pub fn iter<'a>(&'a self) -> Iter<'a,T> {
        Iter { next: self.head.as_ref().map(|frame| &**frame) }
    }

    /// Iterate over stack elements by mutable reference
    pub fn iter_mut(&mut self) -> IterMut<T> {
        IterMut { next: self.head.as_mut().map(|frame| &mut **frame) }
    }
}

// The Drop trait tells the compiler how to free an object after it goes out of scope. 
// By default, the compiler would do this recursively which *could* blow the stack for
// extraordinarily long lists. This simply tells it to do it iteratively.
impl<T> Drop for Stack<T> {
    fn drop(&mut self) {
        let mut cur_link = self.head.take();
        while let Some(mut boxed_frame) = cur_link {
            cur_link = boxed_frame.next.take();
        }
    }
}
```



## Sather

This one uses a builtin linked list to keep the values pushed onto the stack.

```sather
class STACK{T} is
  private attr stack :LLIST{T};

  create:SAME is 
    res ::= new;
    res.stack := #LLIST{T};
    return res;
  end;

  push(elt: T) is
    stack.insert_front(elt);    
  end;

  pop: T is
    if ~stack.is_empty then
      stack.rewind;
      r ::= stack.current;
      stack.delete;
      return r;
    else
      raise "stack empty!\n";
    end;
  end;

  top: T is
    stack.rewind;
    return stack.current;
  end;

  is_empty: BOOL is
    return stack.is_empty;
  end;
end;
```



```sather
class MAIN is
  main is
    s ::= #STACK{INT};
    #OUT + "push values...\n";
    s.push(3);
    s.push(2);
    s.push(1);
    s.push(0);
    #OUT + "retrieving them...\n";
    loop
      #OUT + s.pop + "\n";
    until!(s.is_empty); end;
  end;
end;
```

Sather library has the abstract class <code>$STACK{T}</code>, but using this forces us to implement other methods too.


## Scala

The Do it yourself approach:

```Scala
class Stack[T] {
  private var items = List[T]()

  def isEmpty = items.isEmpty

  def peek = items match {
    case List()       => error("Stack empty")
    case head :: rest => head
  }

  def pop = items match {
    case List()       => error("Stack empty")
    case head :: rest => items = rest; head
  }

  def push(value: T) = items = value +: items
}
```

Or use the standard Scala library. 
Slightly modified to meet to requirements of this task.

```scala
import collection.mutable.{ Stack => Stak }

class Stack[T] extends Stak[T] {
  override def pop: T = {
    if (this.length == 0) error("Can't Pop from an empty Stack.")
    else super.pop
  }
  def peek: T = this.head
}
```
A test could be:
```Scala
object StackTest extends App {

  val stack = new Stack[String]

  stack.push("Peter Pan")
  stack.push("Suske & Wiske", "Alice in Wonderland")

  assert(stack.peek == "Alice in Wonderland")
  assert(stack.pop() == "Alice in Wonderland")
  assert(stack.pop() == "Suske & Wiske")
  assert(stack.pop() == "Peter Pan")
  println("Completed without errors")
}
```



## Scheme

This version uses primitive message passing.

```scheme
(define (make-stack)
  (let ((st '()))
    (lambda (message . args)
      (case message
        ((empty?) (null? st))
        ((top) (if (null? st)
                   'empty
                   (car st)))
        ((push) (set! st (cons (car args) st)))
        ((pop) (if (null? st)
                   'empty
                   (let ((result (car st)))
                     (set! st (cdr st))
                     result)))
        (else 'badmsg)))))
```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func type: stack (in type: baseType) is func
  result
    var type: stackType is void;
  begin
    stackType := array baseType;

    const proc: push (inout stackType: aStack, in baseType: top) is func
      begin
         aStack := [] (top) & aStack;
      end func;

    const func baseType: pop (inout stackType: aStack) is func
      result
        var baseType: top is baseType.value;
      begin
        if length(aStack) = 0 then
          raise RANGE_ERROR;
        else
          top := aStack[1];
          aStack := aStack[2 ..];
        end if;
      end func;

    const func boolean: empty (in stackType: aStack) is
      return length(aStack) = 0;
  end func;

const type: intStack is stack(integer);

const proc: main is func
  local
    var intStack: s is intStack.value;
  begin
    push(s, 10);
    push(s, 20);
    writeln(pop(s) = 20);
    writeln(pop(s) = 10);
    writeln(empty(s));
  end func;
```



## Sidef

Using a built-in array:

```ruby
var stack = [];
stack.push(42);         # pushing
say stack.pop;          # popping
say stack.is_empty;     # is_emtpy?
```


Creating a Stack class:

```ruby
class Stack(stack=[]) {
    method pop        { stack.pop };
    method push(item) { stack.push(item) };
    method empty      { stack.is_empty };
}

var stack = Stack();
stack.push(42);
say stack.pop;          # => 42
say stack.empty;        # => true
```



## Slate

From Slate's standard library:

```slate
collections define: #Stack &parents: {ExtensibleArray}.
"An abstraction over ExtensibleArray implementations to follow the stack
protocol. The convention is that the Sequence indices run least-to-greatest
from bottom to top."

s@(Stack traits) push: obj
[s addLast: obj].

s@(Stack traits) pop
[s removeLast].

s@(Stack traits) pop: n
[s removeLast: n].

s@(Stack traits) top
[s last].

s@(Stack traits) top: n
[s last: n].

s@(Stack traits) bottom
[s first].
```



## Smalltalk

Smalltalk has a built-in Stack class, instances of which you can send messages:

```smalltalk

s := Stack new.
s push: 1.
s push: 2.
s push: 3.
s pop.
s top. "2"

```



## Standard ML


The signature for a module supplying a stack interface, with a couple added functions.


```sml
signature STACK =
sig
    type 'a stack
    exception EmptyStack

    val empty : 'a stack
    val isEmpty : 'a stack -> bool

    val push : ('a * 'a stack) -> 'a stack
    val pop  : 'a stack -> 'a stack
    val top  : 'a stack -> 'a
    val popTop : 'a stack -> 'a stack * 'a

    val map : ('a -> 'b) -> 'a stack -> 'b stack
    val app : ('a -> unit) -> 'a stack -> unit
end
```


An implementation of the <code>STACK</code> signature, using immutable lists.


```sml>structure Stack :
 STACK =
struct
    type 'a stack = 'a list
    exception EmptyStack

    val empty = []

    fun isEmpty st = null st

    fun push (x, st) = x::st

    fun pop []      = raise EmptyStack
      | pop (x::st) = st

    fun top []      = raise EmptyStack
      | top (x::st) = x

    fun popTop st = (pop st, top st)

    fun map f st = List.map f st
    fun app f st = List.app f st
end
```



## Stata

See [[Singly-linked list/Element definition#Stata]].

## Swift

Generic stack.

```Swift>struct Stack<T
 {
    var items = [T]()
    var empty:Bool {
        return items.count == 0
    }
    
    func peek() -> T {
        return items[items.count - 1]
    }
    
    mutating func pop() -> T {
        return items.removeLast()
    }
    
    mutating func push(obj:T) {
        items.append(obj)
    }
}

var stack = Stack<Int>()
stack.push(1)
stack.push(2)
println(stack.pop())
println(stack.peek())
stack.pop()
println(stack.empty)
```

{{out}}

```txt

2
1
true

```



## Tailspin


```tailspin

processor Stack
  @: $;

  sink push
    ..|@Stack: $;
  end push

  source peek
    $@Stack(-1) !
  end peek

  source pop
    ^@Stack(-1) !
  end pop

  source empty
    $@Stack::length -> #
    <0> 1 !
    <> 0 !
  end empty
end Stack

def myStack: [1] -> Stack;

2 -> !myStack::push

'$myStack::empty; $myStack::pop;
' -> !OUT::write
'$myStack::empty; $myStack::pop;
' -> !OUT::write
'$myStack::empty;
' -> !OUT::write

3 -> !myStack::push
'$myStack::empty; $myStack::peek;
' -> !OUT::write
'$myStack::empty; $myStack::pop;
' -> !OUT::write
'$myStack::empty;' -> !OUT::write

```

{{out}}

```txt

0 2
0 1
1
0 3
0 3
1

```



## Tcl

Here's a simple implementation using a list:

```tcl
proc push {stackvar value} {
    upvar 1 $stackvar stack
    lappend stack $value
}
proc pop {stackvar} {
    upvar 1 $stackvar stack
    set value [lindex $stack end]
    set stack [lrange $stack 0 end-1]
    return $value
}
proc size {stackvar} {
    upvar 1 $stackvar stack
    llength $stack
}
proc empty {stackvar} {
    upvar 1 $stackvar stack
    expr {[size stack] == 0}
}
proc peek {stackvar} {
    upvar 1 $stackvar stack
    lindex $stack end
}

set S [list]
empty S ;# ==> 1 (true)
push S foo
empty S ;# ==> 0 (false)
push S bar
peek S ;# ==> bar
pop S ;# ==> bar
peek S ;# ==> foo
```

{{tcllib|struct::stack}}

```tcl
package require struct::stack
struct::stack S
S size ;# ==> 0
S push a b c d e
S size ;# ==> 5
S peek ;# ==> e
S pop ;# ==> e
S peek ;# ==> d
S pop 4 ;# ==> d c b a
S size ;# ==> 0
```



## UnixPipes


```bash
init() { if [ -e stack ]; then rm stack; fi } # force pop to blow up if empty
push() { echo $1 >> stack; }
pop() {
	tail -1 stack;
	x=`head -n -1 stack | wc -c`
	if [ $x -eq '0' ]; then rm stack; else
		truncate -s `head -n -1 stack | wc -c` stack
	fi
}
empty() { head -n -1 stack |wc -l; }
stack_top() { tail -1 stack; }
```

test it:

```bash
% push me; push you; push us; push them
% pop;pop;pop;pop
them
us
you
me
```



## VBA

Define a class Stack in a class module with that name.

```vb
'Simple Stack class

'uses a dynamic array of Variants to stack the values
'has read-only property "Size"
'and methods "Push", "Pop", "IsEmpty"

Private myStack()
Private myStackHeight As Integer

'method Push
Public Function Push(aValue)
  'increase stack height
  myStackHeight = myStackHeight + 1
  ReDim Preserve myStack(myStackHeight)
  myStack(myStackHeight) = aValue
End Function

'method Pop
Public Function Pop()
  'check for nonempty stack
  If myStackHeight > 0 Then
    Pop = myStack(myStackHeight)
    myStackHeight = myStackHeight - 1
  Else
    MsgBox "Pop: stack is empty!"
  End If
End Function

'method IsEmpty
Public Function IsEmpty() As Boolean
  IsEmpty = (myStackHeight = 0)
End Function

'property Size
Property Get Size() As Integer
  Size = myStackHeight
End Property
```

Usage example:

```vb
'stack test
Public Sub stacktest()
  Dim aStack As New Stack
  With aStack
    'push and pop some value
    .Push 45
    .Push 123.45
    .Pop
    .Push "a string"
    .Push "another string"
    .Pop
    .Push Cos(0.75)
    Debug.Print "stack size is "; .Size
    While Not .IsEmpty
      Debug.Print "pop: "; .Pop
    Wend
    Debug.Print "stack size is "; .Size
    'try to continue popping
    .Pop
  End With
End Sub
```

{{out}}

```txt

stacktest
stack size is  3 
pop:  0,731688868873821 
pop: a string
pop:  45 
stack size is  0 

```

(after wich a message box will pop up)


## VBScript


### Stack class


```vb
class stack
	dim tos
	dim stack()
	dim stacksize
	
	private sub class_initialize
		stacksize = 100
		redim stack( stacksize )
		tos = 0
	end sub

	public sub push( x )
		stack(tos) = x
		tos = tos + 1
	end sub
	
	public property get stackempty
		stackempty = ( tos = 0 )
	end property
	
	public property get stackfull
		stackfull = ( tos > stacksize )
	end property
	
	public property get stackroom
		stackroom = stacksize - tos
	end property
	
	public function pop()
		pop = stack( tos - 1 )
		tos = tos - 1
	end function

	public sub resizestack( n )
		redim preserve stack( n )
		stacksize = n
		if tos > stacksize then
			tos = stacksize
		end if
	end sub
end class

dim s
set s = new stack
s.resizestack 10
wscript.echo s.stackempty
dim i
for i = 1 to 10
	s.push rnd
	wscript.echo s.stackroom
	if s.stackroom = 0 then exit for
next
for i = 1 to 10
	wscript.echo s.pop
	if s.stackempty then exit for
next
```

{{out}} (changes every time)

```txt
-1
9
8
7
6
5
4
3
2
1
0
0.7090379
0.81449
0.7607236
1.401764E-02
0.7747401
0.301948
0.2895625
0.5795186
0.533424
0.7055475
```


### Using an ArrayList.


```vb
' Stack Definition - VBScript
 
Option Explicit

Dim stack, i, x
Set stack = CreateObject("System.Collections.ArrayList")
If Not empty_(stack) Then Wscript.Echo stack.Count
push stack, "Banana"
push stack, "Apple"
push stack, "Pear"
push stack, "Strawberry"
Wscript.Echo "Count=" & stack.Count 		    ' --> Count=4
Wscript.Echo pop(stack) & " - Count=" & stack.Count ' --> Strawberry - Count=3
Wscript.Echo "Tail=" & stack.Item(0) 		    ' --> Tail=Banana
Wscript.Echo "Head=" & stack.Item(stack.Count-1)    ' --> Head=Pear
Wscript.Echo stack.IndexOf("Apple", 0)   	    ' --> 1
For i=1 To stack.Count
	Wscript.Echo join(stack.ToArray(), ", ")
	x = pop(stack)
Next 'i

Sub push(s, what)
    s.Add what
End Sub 'push
 
Function pop(s)
	Dim what
    If s.Count > 0 Then
        what = s(s.Count-1)
        s.RemoveAt s.Count-1
    Else
        what = ""
    End If
    pop = what
End Function 'pop
 
Function empty_(s)
    empty_ = s.Count = 0
End Function 'empty_ 
```

{{out}}

```txt

Count=4
Strawberry - Count=3
Tail=Banana
Head=Pear
1
Banana, Apple, Pear
Banana, Apple
Banana

```



## Wart


Stacks as user-defined objects backed by a list.


```wart
def (stack)
  (tag 'stack nil)

mac (push! x s) :qcase `(isa stack ,s)
  `(push! ,x (rep ,s))

mac (pop! s) :qcase `(isa stack ,s)
  `(pop! (rep ,s))

def (empty? s) :case (isa stack s)
  (empty? rep.s)
```


Example usage:


```txt
s <- (stack)
=> (object stack nil)
push! 3 s
=> (object stack (3))
push! 4 s
=> (object stack (4 3))
push! 5 s
=> (object stack (5 4 3))
pop! s
=> 5
(empty? s)
=> nil
pop! s
=> 4
pop! s
=> 3
(empty? s)
=> 1  # true
```



## X86 Assembly


```x86asm

; x86_64 linux nasm

struc Stack
  maxSize: resb 8
  currentSize: resb 8
  contents:
endStruc

section .data

soError: db "Stack Overflow Exception", 10
seError: db "Stack Empty Error", 10


section .text

createStack:
; IN: max number of elements (rdi)
; OUT: pointer to new stack (rax)
  push rdi
  xor rdx, rdx
  mov rbx, 8
  mul rbx
  mov rcx, rax
  mov rax, 12
  mov rdi, 0
  syscall
  push rax
  mov rdi, rax
  add rdi, rcx
  mov rax, 12
  syscall
  pop rax
  pop rbx
  mov qword [rax + maxSize], rbx
  mov qword [rax + currentSize], 0
  ret


push:
; IN: stack to operate on (stack argument), element to push (rdi)
; OUT: void
  mov rax, qword [rsp + 8]
  mov rbx, qword [rax + currentSize]
  cmp rbx, qword [rax + maxSize]
  je stackOverflow
  lea rsi, [rax + contents + 8*rbx]
  mov qword [rsi], rdi
  add qword [rax + currentSize], 1
  ret


pop:
; pop
; IN: stack to operate on (stack argument)
; OUT: element from stack top
  mov rax, qword [rsp + 8]
  mov rbx, qword [rax + currentSize]
  cmp rbx, 0
  je stackEmpty
  sub rbx, 1
  lea rsi, [rax + contents + 8*rbx]
  mov qword [rax + currentSize], rbx
  mov rax, qword [rsi]
  ret


; stack operation exceptions
stackOverflow:
  mov rsi, soError
  mov rdx, 25
  jmp errExit
stackEmpty:
  mov rsi, seError
  mov rdx, 18
errExit:
  mov rax, 1
  mov rdi, 1
  syscall
  mov rax, 60
  mov rdi, 1
  syscall

```



## XLISP

This is a fairly straightforward implementation, representing a stack as a linked list inside an object.

```lisp
(define-class stack
    (instance-variables vals))

(define-method (stack 'initialize)
    (setq vals '())
    self)

(define-method (stack 'push x)
    (setq vals (cons x vals)))

(define-method (stack 'pop)
    (define tos (car vals))
    (setq vals (cdr vals))
    tos)

(define-method (stack 'emptyp)
    (null vals))
```

A sample REPL session:

```lisp
; Loading 'stack.lsp'
[1] (define st (stack 'new))

ST
[2] (st 'push 1)

(1)
[3] (st 'push 2)

(2 1)
[4] (st 'emptyp)

()
[5] (st 'pop)

2
[6] (st 'pop)

1
[7] (st 'emptyp)

#T
[8] 
```



## XPL0


```XPL0
include c:\cxpl\codes;  \intrinsic 'code' declarations
int Stack(100), SP;

proc Push(I);           \Push an integer onto the Stack
int  I;
[SP:= SP+1;
Stack(SP):= I;
]; \Push

func Pop;               \Pop an integer from the Stack
int  I;
[I:= Stack(SP);
SP:= SP-1;
return I;
]; \Pop

func Empty;             \Return 'true' if Stack is empty
return SP<0;

func Top;               \Return the integer at top of Stack
return Stack(SP);

int I;
[SP:= -1;               \initialize stack pointer
for I:= 0 to 10 do Push(I*I);
IntOut(0, Top);  CrLf(0);
while not Empty do [IntOut(0, Pop);  ChOut(0, ^ )];
CrLf(0);
]
```


{{out}}

```txt

100
100 81 64 49 36 25 16 9 4 1 0 

```



## Yabasic


```Yabasic
limit = 1000
dim stack(limit)

top = 0

sub push(n)
    if top < limit then
        top = top + 1 : stack(top) = n
    else
        print "stack full - ";
    end if
end sub

sub pop()
    if top then
        top = top - 1 : return stack(top + 1)
    else
        print "stack empty - ";
    end if
end sub

sub empty()
    return not top
end sub

// 
### ===== test =====


for n = 3 to 5
    print "Push ", n : push(n)
next

print "Pop ", pop()

print "Push ", 6 : push(6)

while(not empty())
    print "Pop ", pop()
wend

print "Pop ", pop()

```



## zkl

Lists have stack methods so this class is somewhat reduntant

```zkl
class Stack{
   var [const] stack=L();
   fcn push(x){stack.append(x); self}
   fcn pop    {stack.pop()}
   fcn empty  {(not stack.len())}
   var [proxy] isEmpty = empty;
}
```

{{out}}

```txt

var s=Stack();
s.push(5).push("five");
s.isEmpty //-->False
s.pop()   //-->"five"

```

