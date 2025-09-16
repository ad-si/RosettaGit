+++
title = "Magic 8-Ball"
description = ""
date = 2019-10-17T23:26:03Z
aliases = []
[extra]
id = 21805
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "ada",
  "algol_68",
  "awk",
  "c",
  "clojure",
  "cpp",
  "factor",
  "forth",
  "go",
  "j",
  "javascript",
  "julia",
  "kotlin",
  "m2000_interpreter",
  "miniscript",
  "oorexx",
  "perl",
  "perl_6",
  "phix",
  "php",
  "python",
  "racket",
  "rexx",
  "ring",
  "rust",
  "zkl",
]
+++

Create Magic 8-Ball. See details: [https://en.wikipedia.org/wiki/Magic_8-Ball Magic 8-Ball]


## Ada


```Ada


with Ada.Text_IO;  use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Numerics.Discrete_Random;


procedure Main is


   --Creation of type with all the possible answers
   --
   type Possible_Answers_Type is (It_Is_Certain, It_Is_Decidedly_So, Without_A_Doubt,
				  Yes_Definitely, You_May_Rely_On_It, As_I_See_It, Yes_Most_Likely,
				  Outlook_Good, Signs_Point_To_Yes, Yes_Reply_Hazy,
				  Try_Again, Ask_Again_Later, Better_Not_Tell_You_Now,
				  Cannot_Predict_Now, Concentrate_And_Ask_Again,
				  Dont_Bet_On_It, My_Reply_Is_No, My_Sources_Say_No,
				  Outlook_Not_So_Good, Very_Doubtful);
   ---------------------------------------------------------------------


   -- Variable declaration
   Answer           : Possible_Answers_Type := Possible_Answers_Type'First;
   User_Question : String := " ";

   -----------------------------------------------------------------------------


   --Randomizer
   --
   package Random_Answer is new Ada.Numerics.Discrete_Random (Possible_Answers_Type);
   use Random_Answer;
   G : Generator;

begin

   Reset (G); -- Starts the generator in a unique state in each run

   --User get provides question
   Put_Line ("Welcome."); New_Line;

   Put_Line ("WARNING!!!  Please remember that there's no need to shake your device for this program to work, and shaking your device could damage it");
   New_Line;

   Put_Line ("What's your question? ");
   Get (Item => User_Question); New_Line;


   --Output Answer
   Answer := (Random (G)); --Assigns random answer to variable Answer

   Put (Answer'Image); --Prints Answer

end Main;


```

```txt

Welcome.

WARNING!!!  Please remember that there's no need to shake your device for this program to work, and shaking your device could damage it

What's your question?
Should I have an apple

MY_SOURCES_SAY_NO



```



## ALGOL 68


```algol68
BEGIN
  []STRING answers = ("It is certain.", "It is decidedly so.",
                      "Without a doubt.", "Yes - definitely.", "You may rely on it.",
                      "As I see it, yes.", "Most likely.", "Outlook good.",
                      "Yes.", "Signs point to yes.", "Reply hazy, try again.",
                      "Ask again later.", "Better not tell you now.", "Cannot predict now.",
                      "Concentrate and ask again.", "Don't count on it.", "My reply is no.",
                      "My sources say no.", "Outlook not so good.", "Very doubtful.");
  DO
    REF STRING question := LOC STRING;
    print("Your question: ");
    read((question, new line));
    print((answers[ENTIER (random * UPB answers) + 1], new line))
  OD
END
```



## AWK


```AWK

# syntax: GAWK -f MAGIC_8-BALL.AWK
BEGIN {
# Ten answers are affirmative, five are non-committal, and five are negative.
    arr[++i] = "It is certain"
    arr[++i] = "It is decidedly so"
    arr[++i] = "Without a doubt"
    arr[++i] = "Yes, definitely"
    arr[++i] = "You may rely on it"
    arr[++i] = "As I see it, yes"
    arr[++i] = "Most likely"
    arr[++i] = "Outlook good"
    arr[++i] = "Signs point to yes"
    arr[++i] = "Yes"
    arr[++i] = "Reply hazy, try again"
    arr[++i] = "Ask again later"
    arr[++i] = "Better not tell you now"
    arr[++i] = "Cannot predict now"
    arr[++i] = "Concentrate and ask again"
    arr[++i] = "Don't bet on it"
    arr[++i] = "My reply is no"
    arr[++i] = "My sources say no"
    arr[++i] = "Outlook not so good"
    arr[++i] = "Very doubtful"
    srand()
    printf("Please enter your question or a blank line to quit.\n")
    while (1) {
      printf("\n? ")
      getline ans
      if (ans ~ /^ *$/) {
        break
      }
      printf("%s\n",arr[int(rand()*i)+1])
    }
    exit(0)
}

```

```txt

Please enter your question or a blank line to quit.

? will you still love me tomorrow
Yes, definitely

?

```


## C

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    char *question = NULL;
    size_t len = 0;
    ssize_t read;
    const char* answers[20] = {
        "It is certain", "It is decidedly so", "Without a doubt",
        "Yes, definitely", "You may rely on it", "As I see it, yes",
        "Most likely", "Outlook good", "Signs point to yes", "Yes",
        "Reply hazy, try again", "Ask again later",
        "Better not tell you now", "Cannot predict now",
        "Concentrate and ask again", "Don't bet on it",
        "My reply is no", "My sources say no", "Outlook not so good",
        "Very doubtful"
    };
    srand(time(NULL));
    printf("Please enter your question or a blank line to quit.\n");
    while (1) {
        printf("\n? : ");
        read = getline(&question, &len, stdin);
        if (read < 2) break;
        printf("\n%s\n", answers[rand() % 20]);
    }
    if (question) free(question);
    return 0;
}
```


Sample session :

```txt

Please enter your question or a blank line to quit.

? : Will May win the next UK election?

It is certain

? : Do flying saucers really exist?

Signs point to yes

? : Will humans ever colonize Mars?

It is decidedly so

? :

```



## C++


```Cpp
#include <array>

#include <cstdlib>
#include <ctime>
#include <iostream>
#include <string>

int main()
{
    constexpr std::array<const char*, 20> answers = {
        "It is certain.",
        "It is decidedly so.",
        "Without a doubt.",
        "Yes - definitely.",
        "You may rely on it.",
        "As I see it, yes.",
        "Most likely.",
        "Outlook good.",
        "Yes.",
        "Signs point to yes.",
        "Reply hazy, try again.",
        "Ask again later.",
        "Better not tell you now.",
        "Cannot predict now.",
        "Concentrate and ask again.",
        "Don't count on it.",
        "My reply is no.",
        "My sources say no.",
        "Outlook not so good.",
        "Very doubtful."
    };

    std::string input;
    std::srand(std::time(nullptr));
    while (true) {
        std::cout << "\n? : ";
        std::getline(std::cin, input);

        if (input.empty()) {
            break;
        }

        std::cout << answers[std::rand() % answers.size()] << '\n';
    }
}
```



## Clojure


```clojure

(def responses
  ["It is certain" "It is decidedly so" "Without a doubt"
  "Yes, definitely" "You may rely on it" "As I see it, yes"
  "Most likely" "Outlook good" "Signs point to yes" "Yes"
  "Reply hazy, try again" "Ask again later"
  "Better not tell you now" "Cannot predict now"
  "Concentrate and ask again" "Don't bet on it"
  "My reply is no" "My sources say no" "Outlook not so good"
  "Very doubtful"])

(do
  (println "Ask a question.  ")
  (read-line)
  (println (rand-nth responses)))

```



## Factor


```factor
USING: io kernel random sequences ;
IN: rosetta-code.magic-8-ball

CONSTANT: phrases {
    "It is certain" "It is decidedly so" "Without a doubt"
    "Yes, definitely" "You may rely on it" "As I see it, yes"
    "Most likely" "Outlook good" "Signs point to yes" "Yes"
    "Reply hazy, try again" "Ask again later"
    "Better not tell you now" "Cannot predict now"
    "Concentrate and ask again" "Don't bet on it"
    "My reply is no" "My sources say no" "Outlook not so good"
    "Very doubtful"
}

"Please enter your question or a blank line to quit." print

[ "? : " write flush readln empty? [ f ]
[ phrases random print t ] if ] loop
```

```txt

Please enter your question or a blank line to quit.
? : Are there more than 10^57 grains of sand in the universe?
Yes
? : Are cats secretly alien spies?

Signs point to yes
? : Am I a genius?
Outlook not so good
? :

```


## Forth

ANS/ISO FORTH, GNU FORTH <BR>
This example defines CASE: ;CASE that create a vector table.
VECTORS compiles a specific number of execution vectors into memory from the data stack.
Responses are created as executable routines that return their Execution token to the Forth Data Stack.
The responses are retrieved from the data stack and compiled into a vector table, after which it is trivial for RANDOM to select any response.
<LANG>\ magic eight ball Rosetta Code
INCLUDE RANDOM.FS
DECIMAL
: CASE:  ( -- -7)   CREATE   ;
: ;CASE   ( n -- )  DOES>  SWAP CELLS +  @ EXECUTE ;

: VECTORS  0  DO  , LOOP ;

:NONAME   ." It is certain" ;
:NONAME   ." It is decidedly so" ;
:NONAME   ." Without a doubt" ;
:NONAME   ." Yes, definitely" ;
:NONAME   ." You may rely on it" ;
:NONAME   ." As I see it, yes." ;
:NONAME   ." Most likely" ;
:NONAME   ." Outlook good" ;
:NONAME   ." Signs point to yes." ;
:NONAME   ." Yes." ;
:NONAME   ." Reply hazy, try again" ;
:NONAME   ." Ask again later" ;
:NONAME   ." Better not tell you now" ;
:NONAME   ." Cannot predict now" ;
:NONAME   ." Concentrate and ask again" ;
:NONAME   ." Don't bet on it" ;
:NONAME   ." My reply is no"  ;
:NONAME   ." My sources say no" ;
:NONAME   ." Outlook not so good" ;
:NONAME   ." Very doubtful" ;

CASE: MAGIC8BALL  20 VECTORS  ;CASE

: GO
       CR ." Please enter your question or a blank line to quit."
       BEGIN   CR ." ? :" PAD 80 ACCEPT 0>
       WHILE   CR 19 RANDOM MAGIC8BALL  CR
       REPEAT ;
```
Test at the console
```txt
 ok
GO
Please enter your question or a blank line to quit.
? :AM I ALIVE
Very doubtful

? :CAN YOU PREDICT THE FUTURE
Outlook not so good

? :ARE YOU VALUABLE
Yes, definitely

? :CAN YOU TELL ME SOMETHING GOOD
Don't bet on it

? :
```



## Go

```go
package main

import (
	"bufio"
	"bytes"
	"fmt"
	"log"
	"math/rand"
	"os"
	"time"
)

func main() {
	rand.Seed(time.Now().UnixNano())
	answers := [...]string{
		"It is certain", "It is decidedly so", "Without a doubt",
		"Yes, definitely", "You may rely on it", "As I see it, yes",
		"Most likely", "Outlook good", "Signs point to yes", "Yes",
		"Reply hazy, try again", "Ask again later",
		"Better not tell you now", "Cannot predict now",
		"Concentrate and ask again", "Don't bet on it",
		"My reply is no", "My sources say no", "Outlook not so good",
		"Very doubtful",
	}
	const prompt = "\n? : "
	fmt.Print("Please enter your question or a blank line to quit.\n" + prompt)
	sc := bufio.NewScanner(os.Stdin)
	for sc.Scan() {
		question := sc.Bytes()
		question = bytes.TrimSpace(question)
		if len(question) == 0 {
			break
		}
		answer := answers[rand.Intn(len(answers))]
		fmt.Printf("\n%s\n"+prompt, answer)
	}
	if err := sc.Err(); err != nil {
		log.Fatal(err)
	}
}
```


```txt

Please enter your question or a blank line to quit.

? : Will Trump win the next US election?

As I see it, yes

? : Is it true that we live in a multiverse?

Yes

? : Will the singularity occur before 2030?

My reply is no

? :

```



## J

Any array should work as the possible answers for the left (x) argument to eight_ball .

```J

NB. translated from awk

prompt=: [: 1!:1 [: 1:  echo

ANSWERS=: [;._2'It is certain"It is decidedly so"Without a doubt"Yes, definitely"You may rely on it"As I see it, yes"Most likely"Outlook good"Signs point to yes"Yes"Reply hazy, try again"Ask again later"Better not tell you now"Cannot predict now"Concentrate and ask again"Don''t bet on it"My reply is no"My sources say no"Outlook not so good"Very doubtful"'

eight_ball=: ANSWERS&$: :(dyad define)
 while. 0 < # prompt 'Please enter your question or a blank line to quit.' do.
  echo ({~ ?@:#) x
 end.
)

```



```txt

   eight_ball''
Please enter your question or a blank line to quit.
Will I finish on time?
Cannot predict now
Please enter your question or a blank line to quit.
Will he?
My reply is no
Please enter your question or a blank line to quit.

```



## JavaScript


```JavaScript

//console
var answers = [ "It is certain", "It is decidedly so", "Without a doubt",
        "Yes, definitely", "You may rely on it", "As I see it, yes",
        "Most likely", "Outlook good", "Signs point to yes", "Yes",
        "Reply hazy, try again", "Ask again later",
        "Better not tell you now", "Cannot predict now",
        "Concentrate and ask again", "Don't bet on it",
        "My reply is no", "My sources say no", "Outlook not so good",
        "Very doubtful"])

console.log("ASK ANY QUESTION TO THE MAGIC 8-BALL AND YOU SHALL RECEIVE AN ANSWER!")

for(;;){
  var answer = prompt("question:")
  console.log(answer)
console.log(answers[Math.floor(Math.random()*answers.length)]);
}

```


```txt

"ASK ANY QUESTION TO THE MAGIC 8-BALL AND YOU SHALL RECEIVE AN ANSWER!"

"Is this code going to work?"

"Most likely"

```



## Julia


```julia
const responses = ["It is certain", "It is decidedly so", "Without a doubt",
    "Yes, definitely", "You may rely on it", "As I see it, yes", "Most likely",
    "Outlook good", "Signs point to yes", "Yes", "Reply hazy, try again",
    "Ask again later", "Better not tell you now", "Cannot predict now",
    "Concentrate and ask again", "Don't bet on it", "My reply is no",
    "My sources say no", "Outlook not so good", "Very doubtful"]

while true
    println("Ask a question (blank to exit):")
    if !occursin(r"[a-zA-Z]", readline(stdin))
        break
    end
    println(rand(responses))
end

```
```txt

Ask a question (blank to exit):
Will there be an eclipse tomorrow?
Outlook good
Ask a question (blank to exit):
Is that a fact?
Reply hazy, try again

```



## Kotlin


```scala
// Version 1.2.40

import java.util.Random

fun main(args: Array<String>) {
    val answers = listOf(
        "It is certain", "It is decidedly so", "Without a doubt",
        "Yes, definitely", "You may rely on it", "As I see it, yes",
        "Most likely", "Outlook good", "Signs point to yes", "Yes",
        "Reply hazy, try again", "Ask again later",
        "Better not tell you now", "Cannot predict now",
        "Concentrate and ask again", "Don't bet on it",
        "My reply is no", "My sources say no", "Outlook not so good",
        "Very doubtful"
    )
    val rand = Random()
    println("Please enter your question or a blank line to quit.")
    while (true) {
        print("\n? : ")
        val question = readLine()!!
        if (question.trim() == "") return
        val answer = answers[rand.nextInt(20)]
        println("\n$answer")
    }
}
```


Sample session :

```txt

Please enter your question or a blank line to quit.

? : Will Trump win the next US election?

Outlook not so good

? : Is it true that we live in a multiverse?

It is certain

? : Will the singularity occur before 2030?

Reply hazy, try again

? : Will the singularity occur before 2030?

Signs point to yes

? :

```



## M2000 Interpreter


```M2000 Interpreter

Module Magic.8.Ball {
      answers=("It is certain", "It is decidedly so", "Without a doubt", "Yes, definitely", "You may rely on it", "As I see it, yes", "Most likely", "Outlook good", "Signs point to yes", "Yes", "Reply hazy, try again", "Ask again later",  "Better not tell you now", "Cannot predict now", "Concentrate and ask again", "Don't bet on it", "My reply is no", "My sources say no", "Outlook not so good", "Very doubtful")
      Print "Please enter your question or a blank line to quit."
      {
            Line Input A$
            Print
            A$=trim$(A$)
            If A$="" Then Exit
            Print Array$(answers, Random(0, 19))
            Loop
      }
}
Magic.8.Ball

```



## MiniScript


```MiniScript
answers = ["It is certain", "It is decidedly so", "Without a doubt",
        "Yes, definitely", "You may rely on it", "As I see it, yes",
        "Most likely", "Outlook good", "Signs point to yes", "Yes",
        "Reply hazy, try again", "Ask again later",
        "Better not tell you now", "Cannot predict now",
        "Concentrate and ask again", "Don't bet on it",
        "My reply is no", "My sources say no", "Outlook not so good",
        "Very doubtful"]
print "Ask your question and the Magic 8 Ball will give you the answer!"
input "What is your question?"
print answers[rnd * answers.len]
```

```txt

Ask your question and the Magic 8 Ball will give you the answer!
What is your question? Are we there yet?
Cannot predict now

```



## ooRexx


```ooRexx

/* REXX */
a=.array~of("It is certain", "It is decidedly so", "Without a doubt",,
            "Yes, definitely", "You may rely on it", "As I see it, yes",,
            "Most likely", "Outlook good", "Signs point to yes", "Yes",,
            "Reply hazy, try again", "Ask again later",,
            "Better not tell you now", "Cannot predict now",,
            "Concentrate and ask again", "Don't bet on it",,
            "My reply is no", "My sources say no", "Outlook not so good",,
            "Very doubtful")
Do Forever
  Say 'your question:'
  Parse Pull q
  If q='' Then Leave
  Say a[random(1,20)]
  Say ''
  End
```

```txt
your question:
will it rain tonight
Concentrate and ask again

your question:
will it rain tonight
You may rely on it

your question:
```



## Perl


```perl
@a = ('It is certain', 'It is decidedly so', 'Without a doubt', 'Yes, definitely',
 'You may rely on it', 'As I see it, yes', 'Most likely', 'Outlook good',
 'Signs point to yes', 'Yes', 'Reply hazy, try again', 'Ask again later',
 'Better not tell you now', 'Cannot predict now', 'Concentrate and ask again',
 "Don't bet on it", 'My reply is no', 'My sources say no', 'Outlook not so good',
 'Very doubtful');

while () {
    print 'Enter your question:';
    last unless <> =~ /\w/;
    print @a[int rand @a], "\n";
}
```



## Perl 6

```perl6
put 'Please enter your question or a blank line to quit.';

["It is certain", "It is decidedly so", "Without a doubt", "Yes, definitely",
 "You may rely on it", "As I see it, yes", "Most likely", "Outlook good",
 "Signs point to yes", "Yes", "Reply hazy, try again", "Ask again later",
 "Better not tell you now", "Cannot predict now", "Concentrate and ask again",
 "Don't bet on it", "My reply is no", "My sources say no", "Outlook not so good",
 "Very doubtful"].roll.put while prompt('? : ').chars;
```

Output very similar to C, Kotlin and zkl examples.



## Phix


```Phix
constant answers = {"As I see it, yes", "Ask again later", "You may rely on it",
                    "Without a doubt", "Don't bet on it", "Outlook not so good",
                    "Signs point to yes", "It is decidedly so", "It is certain",
                    "Better not tell you now", "My reply is no", "Outlook good",
                    "Concentrate and ask again", "Reply hazy, try again", "Yes",
                    "Most likely", "Cannot predict now", "My sources say maybe",
                    "My sources say no", "Yes, definitely", "Yes, probably not",
                    "Very doubtful", "Your question has already been answered"}

string s = prompt_string("Please enter your question or a blank line to quit:")
if s!="" then printf(1,"%s\n",{answers[rand(length(answers))]}) end if
```



## PHP


### CLI


```php
<?php

$fortunes = array(
	"It is certain",
	"It is decidedly so",
	"Without a doubt",
	"Yes, definitely",
	"You may rely on it",
	"As I see it, yes",
	"Most likely",
	"Outlook good",
	"Signs point to yes",
	"Yes",
	"Reply hazy, try again",
	"Ask again later",
	"Better not tell you now",
	"Cannot predict now",
	"Concentrate and ask again",
	"Don't bet on it",
	"My reply is no",
	"My sources say no",
	"Outlook not so good",
	"Very doubtful"
);

/*
 * Prompt the user at the CLI for the command
 */
function cli_prompt( $prompt='> ', $default=false ) {

	// keep asking until a non-empty response is given
	do {
		// display the prompt
		echo $prompt;

		// read input and remove CRLF
		$cmd = chop( fgets( STDIN ) );

	} while ( empty( $default ) and empty( $cmd ) );

	return $cmd ?: $default;

}

$question = cli_prompt( 'What is your question? ' );

echo 'Q: ', $question, PHP_EOL;

echo 'A: ', $fortunes[ array_rand( $fortunes ) ], PHP_EOL;

```


```txt

$ php 8ball.php
What is your question? Are we together forever and never to part?
Q: Are we together forever and never to part?
A: Without a doubt

$ php 8ball.php
What is your question? Are we together forever we two?
Q: Are we together forever we two?
A: Don't bet on it

$ php 8ball.php
What is your question? Will I move heaven and earth to be together forever with you?
Q: Will I move heaven and earth to be together forever with you?
A: It is certain

```



## Python


```python
import random

s = ('It is certain', 'It is decidedly so', 'Without a doubt', 'Yes, definitely',
 'You may rely on it', 'As I see it, yes', 'Most likely', 'Outlook good',
 'Signs point to yes', 'Yes', 'Reply hazy, try again', 'Ask again later',
 'Better not tell you now', 'Cannot predict now', 'Concentrate and ask again',
 "Don't bet on it", 'My reply is no', 'My sources say no', 'Outlook not so good',
 'Very doubtful')

q_and_a = {}

while True:
    question = input('Ask your question:')
    if len(question) == 0: break

    answer = random.choice(s)

    if question in q_and_a:
        print('Your question has already been answered')
    else:
        q_and_a[question] = answer
        print(answer)
```


```txt

Ask your question:Is python the best language ever?
Better not tell you now
Ask your question:You are really not helping your case here.
Very doubtful
Ask your question:So, no?
Signs point to yes
Ask your question:So, no?
Your question has already been answered
Ask your question:

```



## Racket

<lang>(define eight-ball-responses
    (list "It is certain" "It is decidedly so" "Without a doubt" "Yes definitely" "You may rely on it"
          "As I see it, yes" "Most likely" "Outlook good" "Yes" "Signs point to yes"
          "Reply hazy try again" "Ask again later" "Better not tell you now" "Cannot predict now"
          "Concentrate and ask again"
          "Don't count on it" "My reply is no" "My sources say no" "Outlook not so good"
          "Very doubtful"))

(define ((answer-picker answers)) (sequence-ref answers (random (sequence-length answers))))

(define magic-eightball (answer-picker eight-ball-responses))

(module+ main
 (let loop ()
   (display "What do you want to know\n?")
   (read-line)
   (displayln (magic-eightball))
   (loop)))
```

We'll see if it's right.


## REXX


### version 1


```rexx

/* REXX */
Call mk_a "It is certain", "It is decidedly so", "Without a doubt",,
          "Yes, definitely", "You may rely on it", "As I see it, yes",,
          "Most likely", "Outlook good", "Signs point to yes", "Yes",,
          "Reply hazy, try again", "Ask again later",,
          "Better not tell you now", "Cannot predict now",,
          "Concentrate and ask again", "Don't bet on it",,
          "My reply is no", "My sources say no", "Outlook not so good",,
          "Very doubtful"
Do Forever
  Say 'your question:'
  Parse Pull q
  If q='' Then Leave
  z=random(1,a.0)
  Say a.z
  Say ''
  End
Exit
mk_a:
a.0=arg()
Do i=1 To a.0
  a.i=arg(i)
  End
Return

```

```txt
your question:
will it rain tonight
My reply is no

your question:
will it rain tonight
Signs point to yes

your question:

```



### version 2

This REXX version is modeled after the   '''Ring'''   entry.

The method used is to translate all blanks to pseudo-blanks, then extract and show a
random phrase   (after translating the pseudo-blanks back to blanks).

Also, this REXX version appends a period to the phrase as per the (linked) documentation.

```rexx
/*REXX program simulates the  "shaking"  of a  "Magic 8-ball"  and displaying an answer.*/
$="It is certain ÷It is decidedly so ÷Without a doubt÷Yes, definitely÷Signs point to yes",
  "÷You may rely on it÷ As I see it, yes÷My reply is no÷Outlook good÷Outlook not so good",
  "÷Yes÷Ask again later÷Better not tell you now÷Cannot predict now÷Reply hazy, try again",
  "÷Concentrate and ask again÷Don't bet on it÷Most likely÷My sources say no÷Very doubtful"
say space(translate(word(translate(translate($, '┼', " "), , '÷'), random(1, 20)), ,"┼")).
```

```txt

Reply hazy, try again.

```



## Ring


```ring

# Project : Magic 8-Ball

answers = ["It is certain", "It is decidedly so", "Without a doubt",
                 "Yes, definitely", "You may rely on it", "As I see it, yes",
                 "Most likely", "Outlook good", "Signs point to yes", "Yes",
                 "Reply hazy, try again", "Ask again later",
                 "Better not tell you now", "Cannot predict now",
                 "Concentrate and ask again", "Don't bet on it",
                 "My reply is no", "My sources say no", "Outlook not so good",
                 "Very doubtful"]
index = random(len(answers)-1)+1
see answers[index] + nl

```

Output:

```txt

It is certain

```



## Rust

```rust
extern crate rand;

use rand::prelude::*;
use std::io;

fn main() {
    let answers = [
        "It is certain",
        "It is decidedly so",
        "Without a doubt",
        "Yes, definitely",
        "You may rely on it",
        "As I see it, yes",
        "Most likely",
        "Outlook good",
        "Signs point to yes",
        "Yes",
        "Reply hazy, try again",
        "Ask again later",
        "Better not tell you now",
        "Cannot predict now",
        "Concentrate and ask again",
        "Don't bet on it",
        "My reply is no",
        "My sources say no",
        "Outlook not so good",
        "Very doubtful",
    ];
    let mut rng = rand::thread_rng();
    let mut input_line = String::new();

    println!("Please enter your question or a blank line to quit.\n");
    loop {
        io::stdin()
            .read_line(&mut input_line)
            .expect("The read line failed.");
        if input_line.trim() == "" {
            break;
        }
        println!("{}\n", answers.choose(&mut rng).unwrap());
        input_line.clear();
    }
}
```

```txt

Will Keanu Reeves die in my life time?
Yes, definitely

Should I warn him?
Cannot predict now

Can I extend my life time to extend his?
Most likely

```



## zkl

```zkl
answers:=T(
   "It is certain", "It is decidedly so", "Without a doubt",
   "Yes, definitely", "You may rely on it", "As I see it, yes",
   "Most likely", "Outlook good", "Signs point to yes", "Yes",
   "Reply hazy, try again", "Ask again later",
   "Better not tell you now", "Cannot predict now",
   "Concentrate and ask again", "Don't bet on it",
   "My reply is no", "My sources say no", "Outlook not so good",
   "Very doubtful"
);
println("Please enter your question or a blank line to quit.");
while(ask("? : ")){ println(answers[(0).random(answers.len())]) }
```

```txt

lease enter your question or a blank line to quit.
? : will it rain today
It is certain
? : where is Turkey
Yes
? : what is the price of milk
It is decidedly so
? : who is Elton John
Most likely
? :

```

