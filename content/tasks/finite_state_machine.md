+++
title = "Finite state machine"
description = ""
date = 2019-10-21T23:46:34Z
aliases = []
[extra]
id = 21572
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "basic",
  "c",
  "cpp",
  "go",
  "java",
  "javascript",
  "julia",
  "kotlin",
  "pascal",
  "perl_6",
  "phix",
  "prolog",
  "racket",
  "rexx",
  "sinclair_zx81_basic",
  "vba",
  "zkl",
]
+++

A [[wp:Finite state machine|Finite state machine]] (FSM) is computational abstraction which maps a finite number of states to other states within the same set, via transitions. An FSM can only be in one state at any given moment. Transitions can either be explicit or implicit; explicit transitions are triggered by an input signal and implicit transitions by the internal state of the system (that is, the current state). Implicit transitions thus represent "automatic" or sequenced states that are generally processed between explicit transitions (although they can also be used to provide an optional path when no valid transition exists for a given input signal).

;Example
Consider the model of a simple vending machine. The machine is initially in the "ready" state, which maps to exactly two states in the following way:

:ready -> '''deposit''' -> waiting

:ready -> '''quit''' -> exit

The variables in bold-face represent transitions. Any input signal not corresponding to one of those transitions can either trigger an error or be ignored. Otherwise, the current state is updated and the process is repeated. If, for example, a '''deposit''' input signal is encountered, the FSM will move to the "waiting" state, which defines these transitions:

:waiting -> '''select''' -> dispense

:waiting -> '''refund''' -> refunding

The "dispense" state defines only one transition:

:dispense -> '''remove''' -> ready

Note, however, that in this example the "refunding" state doesn't actually require input in order to move to the "ready" state, so an implicit transition is defined as such:

:refunding -> ready 


## Task

Implement a finite state machine which handles both explicit and implicit transitions. Then demonstrate an example which models some real-world process. 


## See also

*[https://www.youtube.com/watch?v=vhiiia1_hC4 Computers Without Memory (Finite State Automata)], A Computerphile Video.
<br/><br/>


## BASIC

=
## Sinclair ZX81 BASIC
=
Works with 1k of RAM.

There doesn't seem much point, in BASIC, implementing a 'general' FSM that would accept a list of states and transition rules as parameters, because an unstructured BASIC program in essence already <i>is</i> that list.

Within each state, if the transition is implicit we just <code>GOTO</code> the next state. If it is explicit, we loop until the user presses a key corresponding to a valid transition. Invalid inputs are ignored.

The line <code>100 GOTO 110</code> is superfluous, because it would go there anyway; but it is worth including it in case we wanted to modify the program later and transition somewhere else out of the <b>dispense</b> state.

Note that the program uses no variables and makes no use of the return stack: all the state is expressed in the (so to speak) state.


```basic
 10 PRINT "PRESS D(EPOSIT) OR Q(UIT)"
 20 IF INKEY$="D" THEN GOTO 50
 30 IF INKEY$="Q" THEN STOP
 40 GOTO 20
 50 PRINT "PRESS S(ELECT) OR R(EFUND)"
 60 IF INKEY$="S" THEN GOTO 90
 70 IF INKEY$="R" THEN GOTO 140
 80 GOTO 60
 90 PRINT "DISPENSED"
100 GOTO 110
110 PRINT "PRESS R(EMOVE)"
120 IF INKEY$="R" THEN GOTO 10
130 GOTO 120
140 PRINT "REFUNDED"
150 GOTO 10
```

It will be seen that the user has pressed, in order, <tt>D</tt>, <tt>R</tt>, <tt>D</tt>, <tt>S</tt>, <tt>R</tt>, and <tt>Q</tt>.

```txt
PRESS D(EPOSIT) OR Q(UIT)
PRESS S(ELECT) OR R(EFUND)
REFUNDED
PRESS D(EPOSIT) OR Q(UIT)
PRESS S(ELECT) OR R(EFUND)
DISPENSED
PRESS R(EMOVE)
PRESS D(EPOSIT) OR Q(UIT)
```



## C

This is an unapologetic implementation of goto. There have been a lot of curse words and obituaries written about it and the inventors of Java were glad to exclude it from the language, but to be fair, goto is one of the many things C inherited from languages such as Assembly or BASIC that make it truly awesome, especially when it comes to such requirements. After all, can there be a clearer and simpler implementation of a Finite State Machine (not counting BASIC ) ?

```C

#include<stdio.h>

int main()
{
	char str[10];
	
	ready: do{
		printf("\nMachine is READY. (D)eposit or (Q)uit :");
		scanf("%s",str);
	}while(!(str[0]!='D'||str[0]!='d'||str[0]!='q'||str[0]!='Q'));
	
	if(str[0]=='q'||str[0]=='Q')
		goto quit;
	goto waiting;
	
	waiting: do{
		printf("(S)elect product or choose to (R)efund :");
		scanf("%s",str);
	}while(!(str[0]!='s'||str[0]!='S'||str[0]!='r'||str[0]!='R'));
	
	if(str[0]=='s'||str[0]=='S'){
		printf("Dispensing product...");
		goto dispense;
	}
	
	else{
		printf("Please collect refund.");
		goto ready;
	}
	
	dispense: do{
		printf("\nPlease (C)ollect product. :");
		scanf("%s",str);
	}while(!(str[0]!='c'||str[0]!='C'));
	
	goto ready;
	
	quit: printf("Thank you, shutting down now.");
	
	return 0;
}

```

Machine simulation :

```txt

C:\rosettaCode>fsm.exe

Machine is READY. (D)eposit or (Q)uit :D
(S)elect product or choose to (R)efund :S
Dispensing product...
Please (C)ollect product. :C

Machine is READY. (D)eposit or (Q)uit :D
(S)elect product or choose to (R)efund :R
Please collect refund.
Machine is READY. (D)eposit or (Q)uit :Q
Thank you, shutting down now.

```



## C++


```C

#include <map>
 
template <typename State, typename Transition = State>
class finite_state_machine
{
protected:
	State
		current;
	std::map<State, std::map<Transition, State>>
		database;
public:
	finite_state_machine()
	{	
		set(State());
	}
	void
		set(State const& state)
	{
		current = state;
	}
	State
		get() const
	{
		return current;
	}
	void
		clear()
	{
		database.clear();
	}
	void
		add(State const& state, Transition const& transition, State const& next)
	{
		database[state][transition] = next;
	}	
/*
	Add a state which is also it's own transition (and thus a link in a chain of sequences)
*/	
	void
		add(State const& state_and_transition, State const& next)
	{
		add(state_and_transition, state_and_transition, next);
	}
	bool
		process(Transition const& transition)
	{
		auto const&
			transitions = database[current],
			found = transitions.find(transition);
		if(found == transitions.end())
			return false;
		auto const&
			next = found->second;
		set(next);
		return true;
	}
/*
	Process so-called "automatic transitions" (ie: sequencing)
*/
	bool
		process()
	{
		return process(get());
	}
/*
	A set of utility functions that may be helpful for displaying valid transitions to the user, etc...
*/	
	template <typename PushBackContainer>
	bool
		get_valid_transitions(State const& state, PushBackContainer& container)
	{
		container.clear();
		auto const&
			found = database.find(state);
		if(found == database.end())
			return false;
		auto const&
			transitions = found->second;
		if(transitions.size() == 0)
			return false;
		for(auto const& iterator : transitions)
		{
			auto const& 
				transition = iterator.first;
			container.push_back(transition);
		}
		return true;
	}
	template <typename Container>
	bool
		get_valid_transitions(Container& container)
	{
		return get_valid_transitions(get(), container);
	}
};
 
/*
	Example usage: a simple vending machine
*/
 
#include <string>
#include <vector>
#include <iostream>
 
using namespace
	std;
void
	print(string const& message)
{
	cout << message << endl;
}
int 
	main() 
{  
	finite_state_machine<string>
		machine;
	machine.add("ready", "quit", "exit");
	machine.add("ready", "deposit", "waiting");
	machine.add("waiting", "select", "dispense");
	machine.add("waiting", "refund", "refunding");
	machine.add("dispense", "remove", "ready");
	machine.add("refunding", "ready");
	machine.set("ready");
	for(;;)
	{
		string
			state = machine.get();
		if(state == "ready")
			print("Please deposit coins.");
		else if(state == "waiting")
			print("Please select a product.");
		else if(state == "dispense")
			print("Dispensed...please remove product from tray.");
		else if(state == "refunding")
			print("Refunding money...");	
		else if(state == "exit")
			break;
		else
			print("Internal error: unaccounted state '" + state + "'!");
	/*
		Handle "automatic" transitions
	*/
		if(machine.process())
			continue;
		vector<string>
			transitions;
		machine.get_valid_transitions(transitions);
		string
			options;
		for(auto const& transition : transitions)
		{
			if(!options.empty())
				options += ", ";
			options += transition;
		}
		print("[" + state + "] Input the next transition (" + options + "): ");
		string
			transition;
		cout << " > ";
		cin >> transition;
		if(!machine.process(transition))
			print( "Error: invalid transition!");	
	}
}

```


```txt

Please deposit coins.
[ready] Enter the next transition (deposit, quit): 
 > deposit
Please select a product.
[waiting] Enter the next transition (refund, select): 
 > refund
Refunding money...
Please deposit coins.
[ready] Enter the next transition (deposit, quit): 
 > deposit
Please select a product.
[waiting] Enter the next transition (refund, select): 
 > select
Dispensed...please remove product from tray.
[dispense] Enter the next transition (remove): 
 > remove
Please deposit coins.
[ready] Enter the next transition (deposit, quit): 
 > quit

```



## Go

```go
package main

import (
    "bufio"
    "fmt"
    "log"
    "os"
    "strings"
)

type state int

const (
    ready state = iota
    waiting
    exit
    dispense
    refunding
)

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func fsm() {
    fmt.Println("Please enter your option when prompted")
    fmt.Println("(any characters after the first will be ignored)")
    state := ready
    var trans string
    scanner := bufio.NewScanner(os.Stdin)
    for {
        switch state {
        case ready:
            for {
                fmt.Print("\n(D)ispense or (Q)uit : ")
                scanner.Scan()
                trans = scanner.Text()
                check(scanner.Err())
                if len(trans) == 0 {
                    continue
                }
                option := strings.ToLower(trans)[0]
                if option == 'd' {
                    state = waiting
                    break
                } else if option == 'q' {
                    state = exit
                    break
                }
            }
        case waiting:
            fmt.Println("OK, put your money in the slot")
            for {
                fmt.Print("(S)elect product or choose a (R)efund : ")
                scanner.Scan()
                trans = scanner.Text()
                check(scanner.Err())
                if len(trans) == 0 {
                    continue
                }
                option := strings.ToLower(trans)[0]
                if option == 's' {
                    state = dispense
                    break
                } else if option == 'r' {
                    state = refunding
                    break
                }
            }
        case dispense:
            for {
                fmt.Print("(R)emove product : ")
                scanner.Scan()
                trans = scanner.Text()
                check(scanner.Err())
                if len(trans) == 0 {
                    continue
                }
                option := strings.ToLower(trans)[0]
                if option == 'r' {
                    state = ready
                    break
                }
            }
        case refunding:
            // no transitions defined
            fmt.Println("OK, refunding your money")
            state = ready
        case exit:
            fmt.Println("OK, quitting")
            return
        }
    }
}

func main() {
    fsm()
}
```


Sample input/output:

```txt

Please enter your option when prompted
(any characters after the first will be ignored)

(D)ispense or (Q)uit : d
OK, put your money in the slot
(S)elect product or choose a (R)efund : s
(R)emove product : r

(D)ispense or (Q)uit : d
OK, put your money in the slot
(S)elect product or choose a (R)efund : r
OK, refunding your money

(D)ispense or (Q)uit : q
OK, quitting

```



## Java


```java
import java.util.*;

public class FiniteStateMachine {

    private enum State {
        Ready(true, "Deposit", "Quit"),
        Waiting(true, "Select", "Refund"),
        Dispensing(true, "Remove"),
        Refunding(false, "Refunding"),
        Exiting(false, "Quiting");

        State(boolean exp, String... in) {
            inputs = Arrays.asList(in);
            explicit = exp;
        }

        State nextState(String input, State current) {
            if (inputs.contains(input)) {
                return map.getOrDefault(input, current);
            }
            return current;
        }

        final List<String> inputs;
        final static Map<String, State> map = new HashMap<>();
        final boolean explicit;

        static {
            map.put("Deposit", State.Waiting);
            map.put("Quit", State.Exiting);
            map.put("Select", State.Dispensing);
            map.put("Refund", State.Refunding);
            map.put("Remove", State.Ready);
            map.put("Refunding", State.Ready);
        }
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        State state = State.Ready;

        while (state != State.Exiting) {
            System.out.println(state.inputs);
            if (state.explicit){
                System.out.print("> ");
                state = state.nextState(sc.nextLine().trim(), state);
            } else {
                state = state.nextState(state.inputs.get(0), state);
            }
        }
    }
}
```


```txt
[Deposit, Quit]
> Deposit
[Select, Refund]
> Refund
[Refunding]
[Deposit, Quit]
> Deposit
[Select, Refund]
> Quit
[Select, Refund]
> Select
[Remove]
> Remove
[Deposit, Quit]
> Quit
```



## JavaScript


### On browser using blocking window methods


```JavaScript
//States
var states = [{
  'name': 'Ready',
  'initial': true,
  'events': {
    'Deposit': 'Waiting',
    'Quit': 'Exiting',
  }
}, {
  'name': 'Waiting',
  'events': {
    'Select': 'Dispensing',
    'Refund': 'Refunding'
  }
}, {
  'name': 'Dispensing',
  'events': {
    'Remove': 'Ready'
  }
}, {
  'name': 'Refunding',
  'events': {
    getReady: 'Ready'
  }
}, {
  'name': 'Exiting',
  'events': {}
}];

function StateMachine(states) {
  this.states = states;
  this.indexes = {};
  for (var i = 0; i < this.states.length; i++) {
    this.indexes[this.states[i].name] = i;
    if (this.states[i].initial) {
      this.currentState = this.states[i];
    }
  }
};
StateMachine.prototype.consumeEvent = function(e) {
  if (this.currentState.events[e]) {
    this.currentState = this.states[this.indexes[this.currentState.events[e]]];
  }
}
StateMachine.prototype.getStatus = function() {
  return this.currentState.name;
}
var fsm = new StateMachine(states);
var s, currentButtons, answer;
while ((s = fsm.getStatus()) !== "Exiting") {
  switch (s) {
    case "Refunding":
      window.alert('Refunding');
      fsm.consumeEvent("getReady")
      break;
    case "Dispensing":
    case "Waiting":
    case "Ready":
      currentButtons = Object.keys(fsm.states[fsm.indexes[s]].events)
      answer = window.prompt(currentButtons.join(' ') + '?');
      answer = currentButtons.find(function(key) {
        return key.match(new RegExp('^' + answer, 'i'))
      });
      if (answer) {
        fsm.consumeEvent(answer);
      }
  }
}

```



## Julia


```julia
abstract type State end

struct Ready <: State
    transitiontable::Dict
    implicit::Union{State, Nothing}
    prompt::String
end

struct Waiting <: State
    transitiontable::Dict
    implicit::Union{State, Nothing}
    prompt::String
end

struct Dispense <: State
    transitiontable::Dict
    implicit::Union{State, Nothing}
    prompt::String
end

struct Refunding <: State
    transitiontable::Dict
    implicit::Union{State, Nothing}
    prompt::String
end

struct Exit <: State
    transitiontable::Dict
    implicit::Union{State, Nothing}
    prompt::String
end

Ready() = Ready(Dict("deposit" => Waiting, "quit" => Exit), nothing, "Vending machine is ready.")
Waiting() = Waiting(Dict("select" => Dispense, "refund" => Refunding), nothing, "Waiting with funds.")
Dispense() = Dispense(Dict("remove" => Ready), nothing, "Thank you! Product dispensed.")
Refunding() = Refunding(Dict(), Ready(), "Please take refund.")
Exit() = Exit(Dict(), nothing, "Halting.")

makeinstance(Ready) = Ready()
makeinstance(Waiting) = Waiting()
makeinstance(Dispense) = Dispense()
makeinstance(Refunding) = Refunding()
makeinstance(Exit) = Exit()

function queryprompt(query, typ)
    print(query, ": ")
    entry = uppercase(strip(readline(stdin)))
    return (typ <: Integer) ? parse(Int, entry) :
        (typ <: Vector) ? map(x -> parse(Int, x), split(entry, r"\s+")) :
        entry
end

function promptinput(state)
    choices = [(s[1], s[2:end]) for s in keys(state.transitiontable)]
    print(state.prompt, join([" ($(w[1]))$(w[2])" for w in choices], ","), ": ")
    while true
        choice = readline()
        if !isempty(choice) && (x = findfirst(s -> s[1] == choice[1], choices)) != nothing
            return state.transitiontable[join(choices[x], "")]
        end
    end
end

quitting(s::State) = false
quitting(s::Exit) = true

function runsim(state)
    while true
        if state.implicit != nothing
            println(state.prompt)
            state = state.implicit
        elseif quitting(state)
            println(state.prompt)
            break
        else
            state = makeinstance(promptinput(state))
        end
    end
end

runsim(Ready())

```
```txt

Vending machine is ready. (q)uit, (d)eposit: d
Waiting with funds. (s)elect, (r)efund: s
Thank you! Product dispensed. (r)emove: r
Vending machine is ready. (q)uit, (d)eposit: d
Waiting with funds. (s)elect, (r)efund: r
Please take refund.
Vending machine is ready. (q)uit, (d)eposit: q
Halting.

```



## Kotlin


```scala
// version 1.1.51

enum class State { READY, WAITING, EXIT, DISPENSE, REFUNDING }

fun fsm() {
    println("Please enter your option when prompted")
    println("(any characters after the first will be ignored)")
    var state = State.READY
    var trans: String

    while (true) {
        when (state) {
            State.READY -> {
                do {
                    print("\n(D)ispense or (Q)uit : ")
                    trans = readLine()!!.toLowerCase().take(1)
                }
                while (trans != "d" && trans != "q")
                state = if (trans == "d") State.WAITING else State.EXIT
            }

            State.WAITING -> {
                println("OK, put your money in the slot")
                do {
                    print("(S)elect product or choose a (R)efund : ")
                    trans = readLine()!!.toLowerCase().take(1)
                }
                while (trans != "s" && trans != "r")
                state = if (trans == "s") State.DISPENSE else State.REFUNDING
            }

            State.DISPENSE -> {
                do {
                    print("(R)emove product : ")
                    trans = readLine()!!.toLowerCase().take(1)
                }
                while (trans != "r")
                state = State.READY
            }

            State.REFUNDING -> {
                // no transitions defined
                println("OK, refunding your money")
                state = State.READY
            }

            State.EXIT -> {
                println("OK, quitting")
                return
            }
        }
    }
}

fun main(args: Array<String>) {
    fsm()
}
```


Sample input/output:

```txt

Please enter your option when prompted
(any characters after the first will be ignored)

(D)ispense or (Q)uit : d
OK, put your money in the slot
(S)elect product or choose a (R)efund : s
(R)emove product : r

(D)ispense or (Q)uit : d
OK, put your money in the slot
(S)elect product or choose a (R)efund : r
OK, refunding your money

(D)ispense or (Q)uit : q
OK, quitting

```



## Pascal

=== (Free Pascal 3.0.0)===

```txt

This version uses fairly vanilla pascal to implement the task. I have 
added some confections to vend and some simple money handeling. It uses 
the table method to implement a FSM which is an explicit table with a 
dispatch loop. 


```


```Pascal

{
   fsm1.pas
   
   Copyright 2018 Trevor Pearson <trevor @ nb-LadyNada co uk >
   
   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.

   Implementing a simulation of a vending machine using a finite state 
   machine. I have used the classic table based method and added a 
   * little extra stuff to give the routines something to do.
      
   
}



program fsm1.pas;

uses sysutils;
  

type

    state = (Null,Ready,Waiting,Refund,Dispense,Stop);
    event = (Epsilon := 1,Deposit,Select,Cancel,Remove,Quit,Error);
	Item = record
	     Name : string[12];
		 Stock: shortint;
		 price: currency;
	end;


var
    amountPaid, itemPrice , changeDue: currency;
    I,J : integer;
	machineState: state;
	newState: state;
	machineEvent: event;
	entry:string;
	vend : array[0..4,0..4] of Item;
	machine : array[1..7,1..7] of state;

	{ The following routines implement the transitions }

procedure TOready();

var
 i,j : integer;

begin

    { Always set the state of a state machine as the first thing you do
	We also set the event to epsiion we can allways set it to error if there is a problem}

	machineState := Ready;
	machineEvent := Epsilon;
    
	{ Now do whatever we need to to transition into this state and check for errors}

    Writeln('            Trevors vending machine');
    Writeln('');
    WriteLn ('        A          B           C            D' );
    for i:=1 to 2 do begin
        write(i,'    ');
        for j:=1 to 4 do begin
	        write(vend[j,i].Name,' ':(12-length(vend[j,i].Name)));
        end;
	    WriteLn();
		Write('       ');
	    for j:=1 to 4 do begin
	        write('£',vend[j,i].price:4:2,'      ');
        end;
	    Writeln('');
    end;

	{ We should have delt with money }
	if (amountPaid > 0) then  machineEvent := Error;
	if (changeDue > 0) then  machineEvent := Error;

end;

procedure TOwaiting();
begin
	machineState := Waiting;
	if ((machineEvent = Select) and (amountPaid >= itemPrice)) then machineEvent := Epsilon;
	if ((machineEvent = Deposit) and (amountPaid >= itemPrice)) then machineEvent := Epsilon;
	
end;

procedure TOrefund();

begin
	machineState := Refund;
    machineEvent := Epsilon;
    
     if (amountPaid > 0) then changeDue := amountPaid;
     WriteLn('REFUNDING >> £' , changeDue:2:2);
     changeDue := 0;
     amountPaid := 0;
end;

procedure TOdispense();
begin
   machineState := Dispense;
  
   if (amountPaid >= vend[I,J].price) then  begin
        machineEvent := Remove;
       changeDue := amountPaid - vend[I,J].price ;
       amountPaid := 0;
       vend[I,J].Stock := vend[I,J].Stock - 1;
       WriteLn('Vending  >>',vend[I,J].Name);
    end
    else machineState := Waiting;
end;

procedure TOstop();
begin
	machineState := Stop;
	machineEvent := Epsilon;
	{ There should not be any transaction in process }
	if ((amountPaid >0) or (changeDue >0)) then machineEvent := Error;
	
end;



procedure Init;
var k,l: integer;
begin
 

   { Lets pretend we have some stuff in this machine }
   
    vend[0,0].Name := 'Dummy';
    vend[0,0].Stock := 0;
    vend[0,0].price := 9999;

    vend[1,1].Name := 'Snickers';
	vend[1,1].Stock := 12;
	vend[1,1].price := 0.50;

    vend[2,1].Name := 'Aero';
	vend[2,1].Stock := 12;
	vend[2,1].price := 0.50;

	vend[3,1].Name := 'Bounty';
	vend[3,1].Stock := 10;
	vend[3,1].price := 0.75;

	vend[4,1].Name := 'Creme egg';
	vend[4,1].Stock := 15;
	vend[4,1].price := 0.60;

	vend[1,2].Name := 'Coke-Cola';
	vend[1,2].Stock := 6;
	vend[1,2].price := 1.10;

	vend[2,2].Name := 'Pepsi';
	vend[2,2].Stock := 6;
	vend[2,2].price := 1.25;

	vend[3,2].Name := '7 up';
	vend[3,2].Stock := 6;
	vend[3,2].price := 1.15;

	vend[4,2].Name := 'Dr Pepper';
	vend[4,2].Stock := 6;
	vend[4,2].price := 1.99;

   { Set up the state table }

    for k :=1 to 7 do begin
	   for l :=1 to 6 do machine[k,l] := Null;
    end;

	machine[ord(Ready),ord(Deposit)] := Waiting;
	machine[ord(Waiting),ord(Deposit)] := Dispense;
	machine[ord(Waiting),ord(Select)] := Dispense;
	machine[ord(Waiting),ord(Cancel)] := Refund;
	machine[ord(Dispense),ord(Remove)] := Refund;
	machine[ord(Dispense),ord(Error)] := Refund;
	machine[ord(Refund),ord(epsilon)] := Ready;
	machine[ord(Ready),ord(Select)] := Waiting;
	machine[ord(Ready),ord(Quit)] := Stop;

   { There should be no money entered so no change is due 
   * set itemPrice to a huge dummy amount}

   amountPaid := 0;
   changeDue := 0;
   itemPrice := 999;
   I:= 0;
   J:=0;
end;



begin
    Init;
    TOready;
 { Here comes the magic bit ... We check for events and if an event 
 * occurs we look up on the table to see if we need to transition to 
 * another state. If we do we call the TO_xxxxx procedure. BUT we do 
 * this in the other order to check for machine generated events like 
 * Error and Epsilon. }
   repeat 
       newState := machine[ord(machineState),ord(machineEvent)]; 
	   case (newState) of
	      Ready : TOready;
		  Waiting : TOwaiting;
		  Dispense : Todispense;
		  Refund: Torefund;
		  Stop: TOStop;
	   end;


{ We get some user input and assign an event to it
* If the user enters a number we convert it to currency and set a 
* deposit event If we have a letter we are making a selection }
       if (machineState = Ready) or (machineState = Waiting) then begin
           WriteLn;
	       Writeln('Enter Selectian A1..D4');
	       Writeln('or deposit amount e.g, 0.20 -- 20p piece.');
	       Write('Or X to cancel, Q to stop this machine :');
	       ReadLn (entry);
	       if ((entry = 'q') or (entry = 'Q')) then machineEvent := Quit;
	       if ((entry = 'x') or (entry = 'X')) then machineEvent := Cancel;
	       if ((entry[1] in ['a'..'d']) or (entry[1] in ['A'..'D'])) then machineEvent:= Select;
	       if (entry[1] in ['0'..'9']) then  begin
	           machineEvent := Deposit;
	           amountPaid := StrToCurr(entry);
	       end;
	       if (machineEvent = Select) then begin
	           I := ord(entry[1]) - 64;
	           if (I > 5) then I := I - 32;
	           J := ord(entry[2]) - ord('0');
	        end;
	           
       end;
   until machineEvent = Quit;

end.


```




```txt

OUTPUT:
 *** Selection First ****

            Trevors vending machine

        A          B           C            D
1    Snickers    Aero        Bounty      Creme egg   
       £0.50      £0.50      £0.75      £0.60      
2    Coke-Cola   Pepsi       7 up        Dr Pepper   
       £1.10      £1.25      £1.15      £1.99      

Enter Selectian A1..D4
or deposit amount e.g, 0.20 -- 20p piece.
Or X to cancel, Q to stop this machine :d1

Enter Selectian A1..D4
or deposit amount e.g, 0.20 -- 20p piece.
Or X to cancel, Q to stop this machine :0.99
Vending  >>Creme egg
REFUNDING >> £0.39
            Trevors vending machine

        A          B           C            D
1    Snickers    Aero        Bounty      Creme egg   
       £0.50      £0.50      £0.75      £0.60      
2    Coke-Cola   Pepsi       7 up        Dr Pepper   
       £1.10      £1.25      £1.15      £1.99      

Enter Selectian A1..D4
or deposit amount e.g, 0.20 -- 20p piece.
Or X to cancel, Q to stop this machine :q


 *** Deposit First ***
 
             Trevors vending machine

        A          B           C            D
1    Snickers    Aero        Bounty      Creme egg   
       £0.50      £0.50      £0.75      £0.60      
2    Coke-Cola   Pepsi       7 up        Dr Pepper   
       £1.10      £1.25      £1.15      £1.99      

Enter Selectian A1..D4
or deposit amount e.g, 0.20 -- 20p piece.
Or X to cancel, Q to stop this machine :2.00

Enter Selectian A1..D4
or deposit amount e.g, 0.20 -- 20p piece.
Or X to cancel, Q to stop this machine :b2
Vending  >>Pepsi
REFUNDING >> £0.75
            Trevors vending machine

        A          B           C            D
1    Snickers    Aero        Bounty      Creme egg   
       £0.50      £0.50      £0.75      £0.60      
2    Coke-Cola   Pepsi       7 up        Dr Pepper   
       £1.10      £1.25      £1.15      £1.99      

Enter Selectian A1..D4
or deposit amount e.g, 0.20 -- 20p piece.
Or X to cancel, Q to stop this machine :q






```



## Perl 6



```perl6
#
### == The state machine ==
#

class StateMachine {
    class State {...}
    class Transition {...}

    has State %!state;
    has &.choose-transition is rw;

    method add-state(Str $id, &action)
    {
        %!state{$id} = State.new(:$id, :&action);
    }

    multi method add-transition(Str $from, Str $to)
    {
        %!state{$from}.implicit-next = %!state{$to};
    }

    multi method add-transition(Str $from, $id, Str $to)
    {
        %!state{$from}.explicit-next.push: Transition.new(:$id, to => %!state{$to});
    }

    method run(Str $initial-state)
    {
        my $state = %!state{$initial-state};
        
        loop {
            $state.action.();
            if $state.implicit-next -> $_ { $state = $_; }
            elsif $state.explicit-next -> $_ { $state = &.choose-transition.(|$_).to; }
            else { last; }
        }
    }

    class Transition {
        has $.id;
        has State $.to;
    }
    class State {
        has $.id;
        has &.action;
        has State $.implicit-next is rw;
        has Transition @.explicit-next;
    }
}


#===== Usage example: Console-based vending machine =====#

my StateMachine $machine .= new;

$machine.choose-transition = sub (*@transitions) {
    say "[{.key + 1}] {.value.id}" for @transitions.pairs;
    loop {
        my $n = val get;
        return @transitions[$n - 1] if $n ~~ Int && $n ~~ 1..@transitions;
        say "Invalid input; try again.";
    }
}

$machine.add-state("ready",     { say "Please deposit coins.";                     });
$machine.add-state("waiting",   { say "Please select a product.";                  });
$machine.add-state("dispense",  { sleep 2; say "Please remove product from tray."; });
$machine.add-state("refunding", { sleep 1; say "Refunding money...";               });
$machine.add-state("exit",      { say "Shutting down...";                          });

$machine.add-transition("ready",     "quit",    "exit");
$machine.add-transition("ready",     "deposit", "waiting");
$machine.add-transition("waiting",   "select",  "dispense");
$machine.add-transition("waiting",   "refund",  "refunding");
$machine.add-transition("dispense",  "remove",  "ready");
$machine.add-transition("refunding",            "ready");

$machine.run("ready");
```



## Phix


```Phix
enum READY, WAITING, DISPENSE, REFUND, QUIT  -- (or just use strings if you prefer)

integer state = READY, ch = ' '
while true do
    printf(1,"%c\n",ch)
    switch state do
        case READY:     printf(1,"Machine is READY. (D)eposit or (Q)uit :")
                        while true do
                            ch = upper(wait_key())
                            if ch='D' then state = WAITING exit end if
                            if ch='Q' then state = QUIT exit end if
                        end while

        case WAITING:   printf(1,"(S)elect product or choose to (R)efund :")
                        while true do
                            ch = upper(wait_key())
                            if ch='S' then state = DISPENSE exit end if
                            if ch='R' then state = REFUND exit end if
                        end while

        case DISPENSE:  printf(1,"Dispensing product...")
                        printf(1,"Please (C)ollect product. :")
                        while true do
                            ch = upper(wait_key())
                            if ch='C' then state = READY exit end if
                        end while

        case REFUND:    printf(1,"Please collect refund.")
                        state = READY
                        ch = ' '

        case QUIT:      printf(1,"Thank you, shutting down now.\n")
                        exit
    end switch
end while
```

```txt

Machine is READY. (D)eposit or (Q)uit :D
(S)elect product or choose to (R)efund :S
Dispensing product...Please (C)ollect product. :C
Machine is READY. (D)eposit or (Q)uit :D
(S)elect product or choose to (R)efund :R
Please collect refund.
Machine is READY. (D)eposit or (Q)uit :Q
Thank you, shutting down now.

```



## Prolog



```Prolog
state(ready, deposit, waiting).
state(ready, quit, exit).
state(waiting, select, dispense).
state(waiting, refund, refunding).
state(dispense, remove, ready).

message(ready, 'Please deposit coins.~n').
message(waiting, 'Please select an item, or refund coins.~n').
message(dispense, 'Please remove your item.~n').
message(refunding, 'Coins have been refunded~n').

act :- act(ready).

act(exit).
act(refunding) :-
	print_message(refunding),
	act(ready).
act(State) :-
	dif(State, exit),
	print_message(State),
	read(Action),
	state(State, Action, NextState),
	act(NextState).
	
print_message(State) :-	message(State, Message), format(Message).
```
	
```txt

2 ?- act.
Please deposit coins.
|: deposit.
Please select an item, or refund coins.
|: select.
Please remove your item.
|: remove.
Please deposit coins.
|: deposit.
Please select an item, or refund coins.
|: refund.
Coins have been refunded
Please deposit coins.
|: quit.

true .

```



## Racket


```racket
#lang racket

(define states
  '((ready (deposit . waiting)
           (quit . exit))
    (waiting (select . dispense)
             (refund . refunding))
    (dispense (remove . ready))
    (refunding . ready)))

(define (machine states prompt get-action quit)
  (let recur ((state (caar states)))
    (printf "CURRENT STATE: ~a~%" state)
    (if (eq? state 'exit)
        (quit)
        (recur (match (cdr (assoc state states))
                 [(list (and transitions (cons actions _)) ...)
                  (prompt "next action (from: ~a): " actions)
                  (match (assoc (get-action) transitions)
                    [(cons action new-state)
                     (printf "~a -> ~a -> ~a~%" state action new-state)
                     new-state]
                    [#f (printf "invalid action for~%") state])]
                 [auto-state
                  (printf "~a -> ~a~%" state auto-state)
                  auto-state])))))

(module+ main
  (let/ec quit
    (with-input-from-string "deposit select remove deposit refund quit"
      (λ () (machine states void read quit)))))
```

```txt
CURRENT STATE: ready
ready -> deposit -> waiting
CURRENT STATE: waiting
waiting -> select -> dispense
CURRENT STATE: dispense
dispense -> remove -> ready
CURRENT STATE: ready
ready -> deposit -> waiting
CURRENT STATE: waiting
waiting -> refund -> refunding
CURRENT STATE: refunding
refunding -> ready
CURRENT STATE: ready
ready -> quit -> exit
CURRENT STATE: exit
```



## REXX


### version 1

This version only works with:
::*   Personal REXX      --or--
::*   PC/REXX

This is essentially a one-for-one translation of the '''BASIC''' program, with the following minor differences:
::* the input allowed is either the uppercase or lowercase version of the letter(s)
::* a mixture of uppercase and lowercase text is used for the output messages
::* messages have extra blanks for readability   (and options are spelled out)

```rexx
/*REXX pgm simulates a FSM (Finite State Machine), input is recognized by pressing keys.*/
 10:  say "Press  D (deposit)   or   Q (quit)"   /*display a prompt (message) to term.  */
 20:  $=inkey();      upper $                    /*since this a terminal, uppercase KEY.*/
      if $=="D"  then signal  50                 /*Is response a "D" ?  Process deposit.*/
      if $=="Q"  then exit                       /*Is response a "Q" ?  Then exit pgm.  */
                      signal  20                 /*Response not recognized, re-issue msg*/

 50:  say "Press  S (select)    or   R (refund)" /*display a prompt (message) to term.  */
 60:  $=inkey();      upper $                    /*since this a terminal, uppercase KEY.*/
      if $=="S"  then signal  90                 /*Is response a "S" ?  Then dispense it*/
      if $=="R"  then signal 140                 /*Is response a "R" ?  Then refund it. */
                      signal  60                 /*Response not recognized? Re-issue msg*/

 90:  say "Dispensed"                            /*display what action just happened.   */
      signal 110                                 /*go and process another option.       */
                                                 /* [↑]  above statement isn't needed.  */
110:  say "Press  R (remove)"                    /*display a prompt (message) to term.  */
120:  $=inkey();      upper $                    /*since this a terminal, uppercase KEY.*/
      if $=="R"  then signal  10                 /*Is response a "R" ?  Then remove it. */
                      signal 120                 /*Response not recognized, re-issue msg*/

140:  say "Refunded"                             /*display what action just happened.   */
      signal  10                                 /*go & re-start process (ready state). */
```

```txt

press  D (deposit)   or   Q (quit)
d                                      ◄■■■■■■■■■■ user pressed this key.
Press  S (select)    or   R (refund)
r                                      ◄■■■■■■■■■■ user pressed this key.
Refunded
press  D (deposit)   or   Q (quit)
d                                      ◄■■■■■■■■■■ user pressed this key. 
Press  S (select)    or   R (refund)
s                                      ◄■■■■■■■■■■ user pressed this key. 
Dispensed
Press  R (remove)
r                                      ◄■■■■■■■■■■ user pressed this key. 
press  D (deposit)   or   Q (quit)
q                                      ◄■■■■■■■■■■ user pressed this key.

```


### version 2

works withooRexx (and any other REXX).
key and Enter must be pressed-

```rexx
/*REXX pgm simulates a FSM (Finite State Machine), input is recognized by pressing keys.*/
 10:  k=inkey('D (deposit)   or   Q (quit)','DQ')
      if k=="D"  then signal  50                 /*Is response a "D" ?  Process deposit.*/
      if k=="Q"  then exit                       /*Is response a "Q" ?  Then exit pgm.  */

 50:  k=inkey('S (select)    or   R (refund)','SR');
      if k=="S"  then signal  90                 /*Is response a "S" ?  Then dispense it*/
      if k=="R"  then signal 140                 /*Is response a "R" ?  Then refund it. */

 90:  say "Dispensed"                            /*display what action just happened.   */
      signal 110                                 /*go and process another option.       */
                                                 
110:  k=inkey('R (remove)','R');
      if k=="R"  then signal  10                 /*Is response a "R" ?  Then remove it. */

140:  say "Refunded"                             /*display what action just happened.   */
      signal  10                                 /*go & re-start process (ready state). */
inkey:
Parse Arg prompt,valid
Do Forever 
  Say 'Press' prompt 'and Enter'
  Parse Upper Pull key
  k=left(key,1)
  If pos(k,valid)>0 Then Leave
  Else 
    Say 'Invalid key, try again.'
  End
Return k
```

```txt
Press D (deposit)   or   Q (quit) and Enter
c
Invalid key, try again.
Press D (deposit)   or   Q (quit) and Enter
d
Press S (select)    or   R (refund) and Enter
g
Invalid key, try again.
Press S (select)    or   R (refund) and Enter
r
Refunded
Press D (deposit)   or   Q (quit) and Enter
```



## VBA

```vb
Enum states
    READY
    WAITING
    DISPENSE
    REFUND
    QU1T
End Enum '-- (or just use strings if you prefer)
Public Sub finite_state_machine()
    Dim state As Integer: state = READY: ch = " "
    Do While True
        Debug.Print ch
        Select Case state
            Case READY:     Debug.Print "Machine is READY. (D)eposit or (Q)uit :"
                            Do While True
                                If ch = "D" Then
                                    state = WAITING
                                    Exit Do
                                End If
                                If ch = "Q" Then
                                    state = QU1T
                                    Exit Do
                                End If
                                ch = InputBox("Machine is READY. (D)eposit or (Q)uit :")
                            Loop
            Case WAITING:   Debug.Print "(S)elect product or choose to (R)efund :"
                            Do While True
                                If ch = "S" Then
                                    state = DISPENSE
                                    Exit Do
                                End If
                                If ch = "R" Then
                                    state = REFUND
                                    Exit Do
                                End If
                                ch = InputBox("(S)elect product or choose to (R)efund :")
                            Loop
            Case DISPENSE:  Debug.Print "Dispensing product..."
                            Do While True
                                If ch = "C" Then
                                    state = READY
                                    Exit Do
                                End If
                                ch = InputBox("Please (C)ollect product. :")
                            Loop
            Case REFUND:    Debug.Print "Please collect refund."
                            state = READY
                            ch = " "
            Case QU1T:      Debug.Print "Thank you, shutting down now."
                            Exit Sub
        End Select
    Loop
End Sub
```
```txt
Machine is READY. (D)eposit or (Q)uit :
D
(S)elect product or choose to (R)efund :
S
Dispensing product...
C
Machine is READY. (D)eposit or (Q)uit :
D
(S)elect product or choose to (R)efund :
R
Please collect refund.
 
Machine is READY. (D)eposit or (Q)uit :
Q
Thank you, shutting down now.
```


## zkl

A lame FSM, we just convert text to a [hopefully valid] zkl program, compile and run it.

If we need true state to state hops, we could use tail recursion (another name for goto).

```zkl
class FSM{	// our Finite State Machine
   var bank=0, item=Void;
   fcn deposit(coin){ bank=coin }
   fcn select(item){
      if(bank){ bank=0; self.item=item; } 
      else print("Depost coin, then select ") 
   }
   fcn take         { if(item) item=Void; else print("Select first "); }
   fcn refund       { coin:=bank; bank=0; return(coin) }

   // couple of wrappers to state changes
   fcn state{ println("Bank(%4d), Item(%s)".fmt(bank,item)) }
   fcn act(f){ print("%-10s-->".fmt(f.name)); f(); state(); }
}

Vault.add(FSM);  // put class FSM where I can find it
```


```zkl
fcn run(program){  // convert text to FSM instructions and run them
   program=program.replace("(",".fp(");  // deposit(10)-->deposit.fp(10)
   a,b,p := 0,0,Sink("class P(FSM){ state(); ");
   while(Void!=(b=program.find(";",a)))
      { p.write("act(",program[a,b-a],");"); a=b + 1; }
   program=p.write(program[a,*],"}").close();
   // println(program);  // WTH did I just do?
   Compiler.Compiler.compileText(program)();  // compile and run our little FSM
}
```


```zkl
run("select(); take(); deposit(10); select(\"snickers\"); take();");
```

The above is converted to:

```zkl
class P(FSM){ 
   state(); 
   act(select.fp());
   act( take.fp());
   act( deposit.fp(10));
   act( select.fp("snickers"));
   act( take.fp());
}
```

The .fp() is function application (ie deferred execution) so I can extract the 
function name and print it.
```txt

Bank(   0), Item(Void)
select    -->Depost coin, then select Bank(   0), Item(Void)
take      -->Select first Bank(   0), Item(Void)
deposit   -->Bank(  10), Item(Void)
select    -->Bank(   0), Item(snickers)
take      -->Bank(   0), Item(Void)

```

