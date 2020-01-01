+++
title = "Odd word problem"
description = ""
date = 2018-09-04T22:43:37Z
aliases = []
[extra]
id = 10773
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Write a program that solves the [http://c2.com/cgi/wiki?OddWordProblem odd word problem] with the restrictions given below.


;Description:
You are promised an input stream consisting of English letters and punctuations.

It is guaranteed that:
* the words (sequence of consecutive letters) are delimited by one and only one punctuation,
* the stream will begin with a word,
* the words will be at least one letter long,   and
* a full stop (a period, [<b>.</b>]) appears after, and only after, the last word.


;Example:
A stream with six words:
:: <big><code>what,is,the;meaning,of:life.</code></big>


The task is to reverse the letters in every other word while leaving punctuations intact, producing:
:: <big><code>what,si,the;gninaem,of:efil.</code></big>
while observing the following restrictions:
# Only I/O allowed is reading or writing one character at a time, which means: no reading in a string, no peeking ahead, no pushing characters back into the stream, and no storing characters in a global variable for later use;
# You '''are not''' to explicitly save characters in a collection data structure, such as arrays, strings, hash tables, etc, for later reversal;
# You '''are''' allowed to use recursions, closures, continuations, threads, co-routines, etc., even if their use implies the storage of multiple characters.


;Test cases:
Work on both the   "life"   example given above, and also the text:
:: <big><code>we,are;not,in,kansas;any,more.</code></big>





## Ada


This is a rather straightforward approach, using recursion.


```Ada
with Ada.Text_IO;

procedure Odd_Word_Problem is

   use Ada.Text_IO; -- Get, Put, and Look_Ahead

   function Current return Character is
      -- reads the current input character, without consuming it
      End_Of_Line: Boolean;
      C: Character;
   begin
      Look_Ahead(C, End_Of_Line);
      if End_Of_Line then
         raise Constraint_Error with "end of line before the terminating '.'";
      end if;
      return C;
   end Current;

   procedure Skip is
      -- consumes the current input character
      C: Character;
   begin
      Get(C);
   end Skip;

   function Is_Alpha(Ch: Character) return Boolean is
   begin
      return (Ch in  'a' .. 'z') or (Ch in 'A' .. 'Z');
   end Is_Alpha;

   procedure Odd_Word(C: Character) is
   begin
      if Is_Alpha(C) then
         Skip;
         Odd_Word(Current);
         Put(C);
      end if;
   end Odd_Word;

begin -- Odd_Word_Problem
   Put(Current);
   while Is_Alpha(Current) loop -- read an even word
      Skip;
      Put(Current);
   end loop;
   if Current /= '.' then -- read an odd word
      Skip;
      Odd_Word(Current);
      Put(Current);
      if Current /= '.' then -- read the remaining words
         Skip;
         Odd_Word_Problem;
      end if;
   end if;
end Odd_Word_Problem;
```


Output:


```txt
> ./odd_word_problem
what,is,the;meaning,of:life.
what,si,the;gninaem,of:efil.
> ./odd_word_problem
we,are;not,in,kansas;any,more.
we,era;not,ni,kansas;yna,more.
```


## ALGOL 68

{{works with|ALGOL 68G|Any - tested with release 2.8.win32}}
The words and punctuation should be on a single line. Uses recursion.

```algol68
# recursively reverses the current word in the input and returns the    #
# the character that followed it                                        #
# "ch" should contain the first letter of the word on entry and will be #
# updated to the punctuation following the word on exit                 #
PROC reverse word = ( REF CHAR ch )VOID:
BEGIN

    CHAR    next ch;

    read( ( next ch ) );

    IF ( next ch <= "Z" AND next ch >= "A" )
    OR ( next ch <= "z" AND next ch >= "a" )
    THEN
        reverse word( next ch )
    FI;

    print( ( ch ) );

    ch := next ch

END; # reverse word #



# recursively prints the current word in the input and returns the      #
# character that followed it                                            #
# "ch" should contain the first letter of the word on entry and will be #
# updated to the punctuation following the word on exit                 #
PROC normal word = ( REF CHAR ch )VOID:
BEGIN

    print( ( ch ) );
    read ( ( ch ) );

    IF ( ch <= "Z" AND ch >= "A" )
    OR ( ch <= "z" AND ch >= "a" )
    THEN
        normal word( ch )
    FI

END; # normal word #



# read and print words and punctuation from the input stream, reversing #
# every second word                                                     #
PROC reverse every other word = VOID:
BEGIN

    CHAR ch;

    read( ( ch ) );

    WHILE
        ch /= "."
    DO
        normal word( ch );
        IF ch /= "."
        THEN
            print( ( ch ) );
            read ( ( ch ) );
            reverse word( ch )
        FI
    OD;

    print( ( ch ) )

END; # reverse every other word #



main: (
    reverse every other word
)
```

{{out}}

```txt

what,si,the;gninaem,of:efil.
we,era;not,ni,kansas;yna,more.

```



## AutoHotkey


```AutoHotkey
str := "what,is,the;meaning,of:life."
loop, parse, str
	if (A_LoopField ~= "[[:punct:]]")
		res .= A_LoopField, toggle:=!toggle
	else
		res := toggle ? RegExReplace(res, ".*[[:punct:]]\K", A_LoopField ) : res A_LoopField
MsgBox % res
```

Outputs:
```txt
what,si,the;gninaem,of:efil.
```



## BaCon


```qbasic
OPEN "/dev/stdin" FOR DEVICE AS in

FUNCTION get_odd()

    LOCAL ch, letter

    ch = MEMORY(1)
    GETBYTE ch FROM in

    IF NOT(REGEX(CHR$(PEEK(ch)), "[[:punct:]]")) THEN
        letter = get_odd()
        PRINT CHR$(PEEK(ch));
    ELSE
        letter = PEEK(ch)
    END IF

    FREE ch
    RETURN letter

END FUNCTION

mem = MEMORY(1)
PRINT "Enter string: ";

WHILE TRUE

    GETBYTE mem FROM in
    PRINT CHR$(PEEK(mem));

    IF REGEX(CHR$(PEEK(mem)), "[[:punct:]]") THEN
        IF PEEK(mem) <> 46 THEN
            POKE mem, get_odd()
            PRINT CHR$(PEEK(mem));
        END IF
        IF PEEK(mem) = 46 THEN BREAK
    END IF
WEND

FREE mem
CLOSE DEVICE in

PRINT
```

This program uses recursion.
{{out}}

```txt

user@host $ bacon odd_word
Converting 'odd_word.bac'... done, 46 lines were processed in 0.003 seconds.
Compiling 'odd_word.bac'... cc  -c odd_word.bac.c
cc -o odd_word odd_word.bac.o -lbacon -lm
Done, program 'odd_word' ready.
user@host $ ./odd_word
Enter string: what,is,the;meaning,of:life.
what,si,the;gninaem,of:efil.
user@host $ ./odd_word
Enter string: we,are;not,in,kansas;any,more.
we,era;not,ni,kansas;yna,more.

```



## Bracmat


```bracmat
( ( odd-word
  =   dothis doother forward backward
    .   ( forward
        =   ch
          .   fil$:?ch
            & put$!ch
            & ( low$!ch:~<a:~>z&forward$
              | !ch:~"."
              )
        )
      & ( backward
        =   ch
          .   fil$:?ch
            & (   low$!ch:~<a:~>z
                & backward$() (put$!ch&)    { This reduces to the return value of backwards$()}
              | '(.put$($ch)&$ch:~".")      { Macro, evaluates to a function with actual ch.  }
              )
        )
      & fil$(!arg,r)
      &   ((=forward$).(=(backward$)$))
        : (?dothis.?doother)
      &   whl
        ' ( !(dothis.)
          & (!doother.!dothis):(?dothis.?doother)
          )
      & (fil$(,SET,-1)|)                 { This is how a file is closed: seek the impossible. }
  )
& put$("what,is,the;meaning,of:life.","life.txt",NEW)
& put$("we,are;not,in,kansas;any,more.","kansas.txt",NEW)
& odd-word$"life.txt"
& put$\n
& odd-word$"kansas.txt"    { Real file, as Bracmat cannot read a single character from stdin. }
);
```

Output:

```txt
what,si,the;gninaem,of:efil.
we,era;not,ni,kansas;yna,more.
```



## C

Using GCC nested function as closures.  This can only be passed up the stack, not the other way around.  It's also doable with <code>makecontext</code>, and may be possible with <code>setjmp</code>.


```c
#include <stdio.h>
#include <ctype.h>

int do_char(int odd, void (*f)(void))
{
	int c = getchar();

	void write_out(void) {
		putchar(c);
		if (f) f();
	}

	if (!odd) putchar(c);

	if (isalpha(c))
		return do_char(odd, write_out);

	if (odd) {
		if (f) f();
		putchar(c);
	}

	return c != '.';
}

int main()
{
	int i = 1;
	while (do_char(i = !i, 0));

	return 0;
}
```



## C++


{{trans|Python}}
{{works with|C++11}}
{{works with|gcc|4.5}}

Tested with gcc 4.5, with "-std=c++0x" option.


```cpp
#include <iostream>
#include <cctype>
#include <functional>

using namespace std;

bool odd()
{
  function<void ()> prev = []{};
  while(true) {
    int c = cin.get();
    if (!isalpha(c)) {
      prev();
      cout.put(c);
      return c != '.';
    }
    prev = [=] { cout.put(c); prev();  };
  }
}

bool even()
{
  while(true) {
    int c;
    cout.put(c = cin.get());
    if (!isalpha(c)) return c != '.';
  }
}


int main()
{
  bool e = false;
  while( e ? odd() : even() ) e = !e;
  return 0;
}
```



## Ceylon



```ceylon

String[] meaning = ["what,", "is,", "the;", "meaning,", "of:", "life."];
String[] kansas = ["we,", "are,", "not,", "in,", "kansas;", "any,", "more."];

shared void run() {
    print("".join(reverseWords(meaning)));
    print("".join(reverseWords(kansas)));
}

String[] reverseWords(String[] words)
    => recursiveReverseWords(words, []);

String[] recursiveReverseWords(String[] remOrig, String[] revWords)
    => if (nonempty remOrig)
        then recursiveReverseWords(remOrig.rest,
                                   revWords.withTrailing(reverseWordRecursive(remOrig.first.sequence(),
                                                                              [],
                                                                              revWords.size.even)))
        else revWords;

String reverseWordRecursive(Character[] remOldChars, Character[] revChars, Boolean isEven)
    => if (nonempty remOldChars)
        then let (char = remOldChars.first) reverseWordRecursive(remOldChars.rest,
                                                                 conditionalAddChar(char, revChars, isEven),
                                                                 isEven)
        else String(revChars);

Character[] conditionalAddChar(Character char, Character[] chars, Boolean isEven)
    => if (isEven || isPunctuation(char))
        then chars.withTrailing(char)
        else chars.withLeading(char);

Boolean isPunctuation(Character char)
    => ",.:;".contains(char);


```


{{out}}

```txt

what,si,the;gninaem,of:efil.

```


```txt

we,era,not,ni,kansas;yna,more.

```



## Clojure

{{trans|Common Lisp}}


```Clojure
(defn next-char []
  (char (.read *in*)))

(defn forward []
  (let [ch (next-char)]
    (print ch)
    (if (Character/isLetter ch)
      (forward)
      (not= ch \.))))

(defn backward []
  (let [ch (next-char)]
    (if (Character/isLetter ch)
      (let [result (backward)]
        (print ch)
        result)
      (fn [] (print ch) (not= ch \.)))) )

(defn odd-word [s]
  (with-in-str s
    (loop [forward? true]
      (when (if forward?
              (forward)
              ((backward)))
        (recur (not forward?)))) )
    (println))
```


Examples:


```clojure>user=
 (odd-word "what,is,the;meaning,of:life.")
what,si,the;gninaem,of:efil.
nil
user=> (odd-word "we,are;not,in,kansas;any,more.")
we,era;not,ni,kansas;yna,more.
nil
```



## CoffeeScript



```CoffeeScript
isWordChar = (c) -> /^\w/.test c
isLastChar = (c) -> c is '.'

# Pass a function that returns an input character and one that outputs a
# character. JS platforms' ideas of single-character I/O vary widely, but this
# abstraction is adaptable to most or all.
oddWord = (get, put) ->
	forwardWord = ->
		loop
			# No magic here; buffer then immediately output.
			c = get()
			put(c)
			unless isWordChar(c)
				return not isLastChar(c)

	# NB: (->) is a CoffeeScript idiom for no-op.
	reverseWord = (outputPending = (->)) ->
		c = get()
		if isWordChar(c)
			# Continue word.
			# Tell recursive call to output this character, then any previously
			# pending characters, after the next word character, if any, has
			# been output.
			reverseWord ->
				put(c)
				outputPending()
		else
			# Word is done.
			# Output previously pending characters, then this punctuation.
			outputPending()
			put(c)
			return not isLastChar(c)

	# Alternate between forward and reverse until one or the other reports that
	# the end-of-input mark has been reached (causing a return of false).
	continue while forwardWord() and reverseWord()
```



### Same without comments



```CoffeeScript
isWordChar = (c) -> /^\w/.test c
isLastChar = (c) -> c is '.'

oddWord = (get, put) ->
	forwardWord = ->
		loop
			c = get()
			put(c)
			unless isWordChar(c)
				return not isLastChar(c)

	reverseWord = (outputPending = (->)) ->
		c = get()
		if isWordChar(c)
			reverseWord ->
				put(c)
				outputPending()
		else
			outputPending()
			put(c)
			return not isLastChar(c)

	continue while forwardWord() and reverseWord()
```



### Testing code



```CoffeeScript
# Redefine as necessary for target platform.
println = (z) -> console.log z

testData = [
	[
		"what,is,the;meaning,of:life."
		"what,si,the;gninaem,of:efil."
	]
	[
		"we,are;not,in,kansas;any,more."
		"we,era;not,ni,kansas;yna,more."
	]
]

results = for [testString, expectedResult] in testData
	# This test machinery uses string buffers for input and output. If your JS
	# platform sports single-character I/O, by all means, adapt to taste.
	getCursor = 0
	putBuffer = ""
	get = ->
		testString.charAt getCursor++
	put = (c) ->
		putBuffer += c
	oddWord(get,put)
	[testString, expectedResult, putBuffer, putBuffer is expectedResult]

println result for result in results
```


Output in [[node.js]]:

 <nowiki>[ 'what,is,the;meaning,of:life.',
  'what,si,the;gninaem,of:efil.',
  'what,si,the;gninaem,of:efil.',
  true ]
[ 'we,are;not,in,kansas;any,more.',
  'we,era;not,ni,kansas;yna,more.',
  'we,era;not,ni,kansas;yna,more.',
  true ]</nowiki>


## Common Lisp

Even words are straightforward.  For odd words, the final punctuation is printed by a closure passed back up the caller chain.

```lisp
(defun odd-word (s)
  (let ((stream (make-string-input-stream s)))
    (loop for forwardp = t then (not forwardp)
          while (if forwardp
                    (forward stream)
                    (funcall (backward stream)))) ))

(defun forward (stream)
  (let ((ch (read-char stream)))
    (write-char ch)
    (if (alpha-char-p ch)
	(forward stream)
        (char/= ch #\.))))

(defun backward (stream)
  (let ((ch (read-char stream)))
    (if (alpha-char-p ch)
        (prog1 (backward stream) (write-char ch))
        #'(lambda () (write-char ch) (char/= ch #\.)))) )

```


Examples:


```lisp
? (odd-word "what,is,the;meaning,of:life.")
what,si,the;gninaem,of:efil.
NIL
? (odd-word "we,are;not,in,kansas;any,more.")
we,era;not,ni,kansas;yna,more.
NIL
```



## D

{{trans|C}}

```d
bool doChar(in bool odd, in void delegate() nothrow f=null) nothrow {
    import core.stdc.stdio, std.ascii;

    immutable int c = getchar;
    if (!odd)
        c.putchar;
    if (c.isAlpha)
        return doChar(odd, { c.putchar; if (f) f(); });
    if (odd) {
        if (f) f();
        c.putchar;
    }
    return c != '.';
}

void main() {
    bool i = true;
    while (doChar(i = !i)) {}
}
```

{{out}}

```txt
what,is,the;meaning,of:life.
what,si,the;gninaem,of:efil.
```



## EchoLisp

No character input stream in EchoLisp, which runs in a browser window. We simultate it with a character stream, with the only function '''read-char''', as specified in the task.

```scheme

(lib 'sequences)
(define input-stream null)
(define output-stream "")

;;---------------------------
;; character I/O simulation
;; --------------------------
(define (read-char) (next input-stream)) ;; #f if EOF
(define (write-char c) (when c (set! output-stream (string-append output-stream c))))

(define (init-streams sentence)
	(set! input-stream (procrastinator sentence))
	(set! output-stream ""))

;;---------------------------------
;; task , using read-char/write-char
;;----------------------------------

(define (flop) ; reverses,  and returns first non-alpha after word, or EOF
	(define c (read-char))
	(if (string-alphabetic? c) (begin0 (flop) (write-char c)) c))

(define (flip)
    (define c (read-char))
    (if (string-alphabetic? c) (begin (write-char c)  (flip)) c))

(define (task sentence)
	 (init-streams sentence)
	 (while (and (write-char (flip)) (write-char (flop))))
	 output-stream )



```

{{out}}

```scheme

(task "what,is,the;meaning,of:life.")
    → "what,si,the;gninaem,of:efil."
; check diacritical
(task "Longtemps,je me suis couché,héhé,hôhô,de bonne heure.")
    → "Longtemps,ej me sius couché,éhéh,hôhô,ed bonne erueh."

```



## Elixir

{{trans|Erlang}}

```elixir
defmodule Odd_word do
  def handle(s, false, i, o) when ((s >= "a" and s <= "z") or (s >= "A" and s <= "Z")) do
    o.(s)
    handle(i.(), false, i, o)
  end
  def handle(s, t, i, o) when ((s >= "a" and s <= "z") or (s >= "A" and s <= "Z")) do
    d = handle(i.(), :rec, i, o)
    o.(s)
    if t == true, do: handle(d, t, i, o), else: d
  end
  def handle(s, :rec, _, _), do: s
  def handle(?., _, _, o), do: o.(?.); :done
  def handle(:eof, _, _, _), do: :done
  def handle(s, t, i, o) do
    o.(s)
    handle(i.(), not t, i, o)
  end

  def main do
    i = fn() -> IO.getn("") end
    o = fn(s) -> IO.write(s) end
    handle(i.(), false, i, o)
  end
end

Odd_word.main
```


{{out}}

```txt

C:\Elixir>elixir Odd_word.exs
we,are;not,in,kansas;any,more.
we,era;not,ni,kansas;yna,more.

```



## Erlang


```erlang

handle(S, false, I, O) when (((S >= $a) and (S =< $z)) or ((S >= $A) and (S =< $Z))) ->
    O(S),
    handle(I(), false, I, O);
handle(S, T, I, O) when (((S >= $a) and (S =< $z)) or ((S >= $A) and (S =< $Z))) ->
    D = handle(I(), rec, I, O),
    O(S),
    case T of true -> handle(D, T, I, O); _ -> D end;
handle(S, rec, _, _) -> S;
handle($., _, _, O) -> O($.), done;
handle(eof, _, _, _) -> done;
handle(S, T, I, O) -> O(S), handle(I(), not T, I, O).

main([]) ->
    I = fun() -> hd(io:get_chars([], 1)) end,
    O = fun(S) -> io:put_chars([S]) end,
    handle(I(), false, I, O).

```


=={{header|F_Sharp|F#}}==
<p>A recursive solution.</p>

```fsharp
open System
open System.Text.RegularExpressions

let mutable Inp = Console.In

let Out c = printf "%c" c; (if c = '.' then Environment.Exit 0)

let In() = Inp.Read() |> Convert.ToChar

let (|WordCharacter|OtherCharacter|) c =
    if Regex.IsMatch(c.ToString(),"[a-zA-Z]") then
        WordCharacter
    else
        OtherCharacter

let rec forward () =
    let c = In()
    let rec backward () : char =
        let c = In()
        match c with
        | WordCharacter ->
            let s = backward() in Out c; s
        | OtherCharacter -> c
    Out c
    match c with
    | WordCharacter -> forward()
    | OtherCharacter -> backward()

[<EntryPoint>]
let main argv =
    if argv.Length > 0 then Inp <- new System.IO.StringReader(argv.[0])
    let rec loop () = forward() |> Out;  loop()
    loop()
    0
```

{{out}}

```txt

>echo we,are;not,in,kansas;any,more. | RosettaCode
we,era;not,ni,kansas;yna,more.
>echo what,is,the;meaning,of:life. | RosettaCode
what,si,the;gninaem,of:efil.
```



## Factor

This is a delicate program with arcane control flow. To reverse each odd word, this code uses continuations to ''jump-back'' into earlier iterations of a ''while'' loop. This trick reverses the letters by reversing the loop!

This code is difficult to follow, because it twists its control flow like spaghetti. These continuations form a [[singly-linked list]], where each continuation contains a letter and a previous continuation. The program effectively reverses this linked list.


```factor
USING: continuations kernel io io.streams.string locals unicode.categories ;
IN: rosetta.odd-word

<PRIVATE
! Save current continuation.
: savecc ( -- continuation/f )
    [ ] callcc1 ; inline

! Jump back to continuation, where savecc will return f.
: jump-back ( continuation -- )
    f swap continue-with ; inline
PRIVATE>

:: read-odd-word ( -- )
    f :> first-continuation!
    f :> last-continuation!
    f :> reverse!
    ! Read characters. Loop until end of stream.
    [ read1 dup ] [
        dup Letter? [
            ! This character is a letter.
            reverse [
                ! Odd word: Write letters in reverse order.
                last-continuation savecc dup [
                    last-continuation!
                    2drop       ! Drop letter and previous continuation.
                ] [
                    ! After jump: print letters in reverse.
                    drop                ! Drop f.
                    swap write1         ! Write letter.
                    jump-back           ! Follow chain of continuations.
                ] if
            ] [
                ! Even word: Write letters immediately.
                write1
            ] if
        ] [
            ! This character is punctuation.
            reverse [
                ! End odd word. Fix trampoline, follow chain of continuations
                ! (to print letters in reverse), then bounce off trampoline.
                savecc dup [
                    first-continuation!
                    last-continuation jump-back
                ] [ drop ] if
                write1                  ! Write punctuation.
                f reverse!              ! Begin even word.
            ] [
                write1                  ! Write punctuation.
                t reverse!              ! Begin odd word.
                ! Create trampoline to bounce to (future) first-continuation.
                savecc dup [
                    last-continuation!
                ] [ drop first-continuation jump-back ] if
            ] if
        ] if
    ] while
    ! Drop f from read1. Then print a cosmetic newline.
    drop nl ;

: odd-word ( string -- )
    [ read-odd-word ] with-string-reader ;
```


 '''USE: rosetta.odd-word'''
 ( scratchpad ) '''"what,is,the;meaning,of:life." odd-word'''
 what,si,the;gninaem,of:efil.
 ( scratchpad ) '''"we,are;not,in,kansas;any,more." odd-word'''
 we,era;not,ni,kansas;yna,more.


## FALSE

This solution uses recursion to read the backwards words, to output the characters after having done the rest of that word.

```false
[$$$$'.=\',=|\';=|\':=|~[^s;!\,]?]s:              {recursive reading}
[s;!$'.=~[,^f;!]?]r:                              {reverse words}
[[$$$$'.=\',=|\';=|\':=|~][,^]#$'.=~[,^r;!]?]f:   {forward words}
^f;!,                                             {start}
```



## Forth


```forth
: word?  dup [char] . <> over bl <> and ;
: ?quit  dup [char] . = if emit quit then ;
: eatbl  begin dup bl = while drop key repeat ?quit ;
: even   begin word? while emit key repeat ;
: odd    word? if key recurse swap emit then ;
: main   cr key eatbl begin even eatbl space odd eatbl space again ;
```



## Fortran

By not allowing the use of arrays of characters to facilitate the reversing of texts, the obvious solution involves recursion with storage via the stack so that its last-on, first-off style will achieve the required goal. But alas, Fortran compilers were typically written for computers that did not employ a stack mechanism so recursion was not expected even after the introduction of Algol in the 1960s, and the failure of recursively-invoked routines to return correctly became accepted. The standard murmur was that "Fortran is not a recursive language" even though the language contains recursive definitions such as for arithmetic expressions. By contrast, the B6700 system ''did'' employ a hardware stack, and, without any fuss, recursion just worked.

But with F90, the language definition was augmented by the menacing word RECURSIVE, and so...
```Fortran
      MODULE ELUDOM	!Uses the call stack for auxiliary storage.
       INTEGER MSG,INF	!I/O unit numbers.
       LOGICAL DEFER	!To stumble, or not to stumble.
       CONTAINS
        CHARACTER*1 RECURSIVE FUNCTION GET(IN)	!Returns one character, going forwards.
         INTEGER IN	!The input file.
         CHARACTER*1 C	!The single character to be read therefrom.
          READ (IN,1,ADVANCE="NO",EOR=3,END=4) C	!Thus. Not advancing to the next record.
    1     FORMAT (A1,$)	!For output, no advance to the next line either.
    2     IF (("A"<=C .AND. C<="Z").OR.("a"<=C .AND. C<="z")) THEN	!Unsafe for EBCDIC.
            IF (DEFER) THEN	!Are we to reverse the current text?
              GET = GET(IN)	!Yes. Go for the next letter.
              WRITE (MSG,1) C	!And now, backing out, reveal the letter at this level.
              RETURN		!Retreat another level.
            END IF		!Thus passing back the ending non-letter that was encountered.
           ELSE		!And if we've encountered a non-letter,
            DEFER = .NOT. DEFER	!Then our backwardness flips.
          END IF	!Enough inspection of C.
    3     GET = C	!Pass it back.
          RETURN	!And we're done.
    4     GET = CHAR(0)	!Reserving this for end-of-file.
        END FUNCTION GET!That was strange.
      END MODULE ELUDOM	!But as per the specification.

      PROGRAM CONFUSED	!Just so.
      USE ELUDOM	!Forwards? Backwards?
      CHARACTER*1 C	!A scratchpad for multiple inspections.
      MSG = 6	!Standard output.
      INF = 10	!This will do.
      OPEN (INF,NAME = "Confused.txt",STATUS="OLD",ACTION="READ")	!Go for the file.

Chew through the input. A full stop marks the end.
   10 DEFER = .FALSE.	!Start off going forwards.
   11 C = GET(INF)		!Get some character from file INF.
      IF (ICHAR(C).LE.0) STOP		!Perhaps end-of-file is reported.
      IF (C.NE." ") WRITE (MSG,12) C	!Otherwise, write it. A blank for end-of-record.
   12 FORMAT (A1,$)			!Obviously, not finishing the line each time.
      IF (C.NE.".") GO TO 11	!And if not a full stop, do it again.
      WRITE (MSG,"('')")	!End the line of output.
      GO TO 10		!And have another go.
      END	!That was confusing.
```

With file Confused.txt containing the obvious input, the output is

```txt

what,si,the;gninaem,of:efil.
we,era;not,ni,kansas;yna,more.
hot,star.

```

The basic ploy is that the function returns the next character from the input, but, should DEFER be true, it secretly invokes itself until a non-letter is found then returns (bearing that non-letter as its result) and on the way back out, secretly writes the letter previously read. Each level of recursion has its own version of that letter and by revealing them as the returns proceed, they are written in reverse order of input. Seen from the outside of GET, the value of DEFER is always ''true'' but this variable is static with regard to the invocations of GET, it being defined outside GET. If it were defined within there would be a new instance allocated with each level of recursion (as with variable C), which is not what is wanted.

Testing showed that the F90 feature of <code>ADVANCE="NO"</code> was required for the READ action because the $ format code that works for output does not work for input. Should an end-of-record interfere with the READ, the <code>EOR=''label''</code> is taken, and the character read will be a space. To avoid ugly system messages on running into end-of-file, character zero is reserved, just as a space is reserved for end-of-record encounters and skipped for output. Fortunately, the specification does not include spaces as allowed input. No checks are made as to whether the input conforms to the given specifications.

If the ADVANCE feature is unavailable, then the file could be read as UNFORMATTED, one character at a go with a record length of one. And then would arise the annoyance of dealing with the ASCII world's usage of CR, CRLF, LFCR, or CR as markers for the ends of records.


## Go


```go
package main

import (
    "bytes"
    "fmt"
    "io"
    "os"
    "unicode"
)

func main() {
    owp(os.Stdout, bytes.NewBufferString("what,is,the;meaning,of:life."))
    fmt.Println()
    owp(os.Stdout, bytes.NewBufferString("we,are;not,in,kansas;any,more."))
    fmt.Println()
}

func owp(dst io.Writer, src io.Reader) {
    byte_in := func () byte {
        bs := make([]byte, 1)
        src.Read(bs)
        return bs[0]
    }
    byte_out := func (b byte) { dst.Write([]byte{b}) }
    var odd func() byte
    odd = func() byte {
        s := byte_in()
        if unicode.IsPunct(rune(s)) {
            return s
        }
        b := odd()
        byte_out(s)
        return b
    }
    for {
        for {
            b := byte_in()
            byte_out(b)
            if b == '.' {
                return
            }
            if unicode.IsPunct(rune(b)) {
                break
            }
        }
        b := odd()
        byte_out(b)
        if b == '.' {
            return
        }
    }
}
```

Output:

```txt

what,si,the;gninaem,of:efil.
we,era;not,ni,kansas;yna,more.

```



### Using <code>defer</code>


```go
package main

import (
    "bytes"
    "fmt"
    "io"
    "os"
    "unicode"
)

func main() {
    owp(os.Stdout, bytes.NewBufferString("what,is,the;meaning,of:life."))
    fmt.Println()
    owp(os.Stdout, bytes.NewBufferString("we,are;not,in,kansas;any,more."))
    fmt.Println()
}

func owp(dst io.Writer, src io.Reader) {
    byte_in := func () byte {
        bs := make([]byte, 1)
        src.Read(bs)
        return bs[0]
    }
    byte_out := func (b byte) { dst.Write([]byte{b}) }
    odd := func() byte {
        for {
            b := byte_in()
            if unicode.IsPunct(int(b)) {
                return b
            }
            defer byte_out(b)
        }
        panic("impossible")
    }
    for {
        for {
            b := byte_in()
            byte_out(b)
            if b == '.' {
                return
            }
            if unicode.IsPunct(rune(b)) {
                break
            }
        }
        b := odd()
        byte_out(b)
        if b == '.' {
            return
        }
    }
}
```



### Using channels and goroutines

{{trans|Ruby}}
{{trans|Tcl}}

```go
package main

import (
    "bytes"
    "fmt"
    "io"
    "os"
    "unicode"
)

func main() {
    owp(os.Stdout, bytes.NewBufferString("what,is,the;meaning,of:life."))
    fmt.Println()
    owp(os.Stdout, bytes.NewBufferString("we,are;not,in,kansas;any,more."))
    fmt.Println()
}

type Coroutine struct {
    out <-chan Coroutine
    in chan<- byte
}

func owp(dst io.Writer, src io.Reader) {
    byte_in := func () (byte, error) {
        bs := make([]byte, 1)
        _, err := src.Read(bs)
        return bs[0], err
    }
    byte_out := func (b byte) { dst.Write([]byte{b}) }

    var f, r Coroutine

    f = func () Coroutine {
        out := make(chan Coroutine)
	in := make(chan byte)
        var fwd func (byte) byte
        fwd = func (c byte) (z byte) {
            if unicode.IsLetter(rune(c)) {
                byte_out(c)
                out <- f
                z = fwd(<- in)
            } else {
                z = c
            }
            return
        }
        go func () {
            for {
                x, ok := <- in
                if !ok { break }
                byte_out(fwd(x))
                out <- r
            }
        } ()
        return Coroutine{ out, in }
    } ()
    r = func () Coroutine {
        out := make(chan Coroutine)
	in := make(chan byte)
        var rev func (byte) byte
        rev = func (c byte) (z byte) {
            if unicode.IsLetter(rune(c)) {
                out <- r
                z = rev(<- in)
                byte_out(c)
            } else {
                z = c
            }
            return
        }
        go func () {
            for {
                x, ok := <- in
                if !ok { break }
                byte_out(rev(x))
                out <- f
            }
        } ()
        return Coroutine{ out, in }
    } ()

    for coro := f; ; coro = <- coro.out {
        c, err := byte_in()
        if err != nil { break }
        coro.in <- c
    }
    close(f.in)
    close(r.in)
}
```



## Haskell

While it seems like this solution would break the task's rules, Haskell is non-strict, therefore this yields the same behavior of reading and printing one character at a time, without excess storage into a "string". To prove it, run the program and manually enter the input string (Windows command prompt does not respect buffering settings, but urxvt on on Linux does).

```Haskell
import System.IO

isAlpha :: Char -> Bool
isAlpha = flip elem $ ['a'..'z'] ++ ['A'..'Z']

split :: String -> (String, String)
split = break $ not . isAlpha

parse :: String -> String
parse [] = []
parse l  =
  let (a, w) = split l
      (b, x) = splitAt 1 w
      (c, y) = split x
      (d, z) = splitAt 1 y
  in a ++ b ++ reverse c ++ d ++ parse z

main :: IO ()
main = hSetBuffering stdin NoBuffering >> hSetBuffering stdout NoBuffering >>
       getContents >>= putStr . (takeWhile (/= '.')) . parse >> putStrLn "."
```

If the above is not acceptable, or if Haskell was implicitly strict, then this solution would satisfy the requirements:

```Haskell
isAlpha :: Char -> Bool
isAlpha = flip elem $ ['a'..'z'] ++ ['A'..'Z']

parse :: IO ()
parse = do
  x <- getChar
  putChar x
  case () of
   _ | x == '.'  -> return ()
     | isAlpha x -> parse
     | otherwise -> do
         c <- revParse
         putChar c
         if c == '.'
           then return ()
           else parse

revParse :: IO Char
revParse = do
  x <- getChar
  case () of
   _ | x == '.'  -> return x
     | isAlpha x -> do
         c <- revParse
         putChar x
         return c
     | otherwise -> return x

main :: IO ()
main = hSetBuffering stdin NoBuffering >> hSetBuffering stdout NoBuffering >>
       parse >> putStrLn ""
```

Linux urxvt output:

```txt
$ ./OddWord
wwhhaatt,,is,si,tthhee;;meaning,gninaem,ooff::life.efil.
$ echo "what,is,the;meaning,of:life." | ./OddWord
what,si,the;gninaem,of:efil.
$ echo "we,are;not,in,kansas;any,more." | ./OddWord
we,era;not,ni,kansas;yna,more.
```

Windows command prompt output:

```txt
>OddWord.exe
what,is,the;meaning,of:life.
what,si,the;gninaem,of:efil.
>echo what,is,the;meaning,of:life. | OddWord.exe

what,si,the;gninaem,of;efil.
>echo we,are;not,in,kansas;any,more. | OddWord.exe

we,era;not,ni,kansas;yna,more.
```


=={{header|Icon}} and {{header|Unicon}}==
The following recursive version is based on the non-deferred GO version.  A co-expression is used to turn the parameter to the wrapper into a character at a time stream.


```Icon
procedure main()
every OddWord(!["what,is,the;meaning,of:life.",
                "we,are;not,in,kansas;any,more."])
end

procedure OddWord(stream)                     #: wrapper for demonstration
   write("Input stream: ",stream)
   writes("Output stream: ") & eWord(create !stream,'.,;:') & write()
end

procedure eWord(stream,marks)                 #: handle even words
   repeat {
      repeat
         writes(@stream) ? if ="." then return else if any(marks) then break
      if writes(oWord(stream,marks)) == '.' then return
      }
end

procedure oWord(stream,marks)                 #: handle odd words (reverse)
   if any(marks,s := @stream) then return s
   return 1(oWord(stream,marks), writes(s))
end
```


Output:
```txt
Input stream: what,is,the;meaning,of:life.
Output stream: what,si,the;gninaem,of:efil.
Input stream: we,are;not,in,kansas;any,more.
Output stream: we,era;not,ni,kansas;yna,more.
```


A slightly different solution which uses real I/O from stdin is:

```Unicon
procedure main(A)
    repeat (while writes((any(&letters, c := reads(&input,1)),c))) |
           (writes(c) ~== "." ~== writes(rWord())) | break write()
end

procedure rWord(c)
    c1 := rWord((any(&letters, c1 := reads(&input,1)),c1))
    writes(\c)
    return c1
end
```

And some sample runs:

```txt

->rw
what,is,the;meaning,of:life.
what,si,the;gninaem,of:efil.
->rw
we,are;not,in,kansas;any,more.
we,era;not,ni,kansas;yna,more.
->
```



## J


This task's requirement to perform buffering implicitly rather than explicitly was perplexing from a J point of view (see talk page for some of that discussion).  To avoid this issue, this implementation uses a [[Odd_word_problem/SimpleCoroutineSupportForJ|coroutine-like utility]].

J also lacks character stream support, so this implementation uses a [[Odd_word_problem/TrivialCharacterStreamSupportForJ|stream-like implementation]].


```j
putch=: 4 :0                 NB. coroutine verb
  outch y
  return x
)

isletter=: toupper ~: tolower

do_char=: 3 :0                 NB. coroutine verb
  ch=. getch''
  if. isletter ch do.
    if. odd do.
      putch&ch yield do_char '' return.
    end.
  else.
    odd=: -. odd
  end.
  return ch
)

evenodd=: 3 :0
  clear_outstream begin_instream y
  odd=: 0
  whilst. '.'~:char do.
    outch char=. do_char coroutine ''
  end.
)
```


Note that in the couroutine-like support page we defined <code>u yield v y</code> such that it produces a result which, when returned to the <code>coroutine</code> helper verb, will cause the deferred execute <code>u v y</code> in a context where both u and v are expected to be coroutine verbs (they will produce a result either wrapped with <code>yield</code> or with <code>return</code>).  Likewise <code>return</code> wraps the result with instructions for the <code>coroutine</code> helper, instructing it to use the returned result "as-is".  (And, if <code>return</code> is used with an empty stack in the helper, that instance would be the result of the <code>coroutine</code> helper.)

Note that when we curry <code>putch</code> with <code>ch</code> (in <code>putch&ch</code>) we get a verb which needs only one argument.  So in this case, <code>x</code> in <code>putch</code> (its left argument) will be the right argument of the derived verb.  In other words, in this example, it will be the result of the <code>do_char</code> instance that uses <code>return ch</code> -- this will be the first non-letter character that is seen when dealing with the odd case.

With this implementation:


```j
   evenodd 'what,is,the;meaning,of:life.'
what,si,the;gninaem,of:efil.
   evenodd 'we,are;not,in,kansas;any,more.'
we,era;not,ni,kansas;yna,more.
```


That said, note that this implementation has significant overhead when compared to a more direct implementation of the algorithm.


## Java


```java
public class OddWord {
    interface CharHandler {
	CharHandler handle(char c) throws Exception;
    }
    final CharHandler fwd = new CharHandler() {
	public CharHandler handle(char c) {
	    System.out.print(c);
	    return (Character.isLetter(c) ? fwd : rev);
	}
    };
    class Reverser extends Thread implements CharHandler {
	Reverser() {
	    setDaemon(true);
	    start();
	}
	private Character ch; // For inter-thread comms
	private char recur() throws Exception {
	    notify();
	    while (ch == null) wait();
	    char c = ch, ret = c;
	    ch = null;
	    if (Character.isLetter(c)) {
		ret = recur();
		System.out.print(c);
	    }
	    return ret;
	}
	public synchronized void run() {
	    try {
		while (true) {
		    System.out.print(recur());
		    notify();
		}
	    } catch (Exception e) {}
	}
	public synchronized CharHandler handle(char c) throws Exception {
	    while (ch != null) wait();
	    ch = c;
	    notify();
	    while (ch != null) wait();
	    return (Character.isLetter(c) ? rev : fwd);
	}
    }
    final CharHandler rev = new Reverser();

    public void loop() throws Exception {
	CharHandler handler = fwd;
	int c;
	while ((c = System.in.read()) >= 0) {
	    handler = handler.handle((char) c);
	}
    }

    public static void main(String[] args) throws Exception {
	new OddWord().loop();
    }
}
```

Output is equivalent to that of the Python solution.


## Julia

{{works with|Julia|0.6}}
{{trans|Python}}

```julia
# io = readstring(STDIN)
io = "what,is,the;meaning,of:life."
i  = 0

readbyte!() = io[global i += 1]
writebyte(c) = print(Char(c))

function odd(prev::Function = () -> false)
    a = readbyte!()
    if !isalpha(a)
        prev()
        writebyte(a)
        return a != '.'
    end

    # delay action until later, in the shape of a closure
    clos() = (writebyte(a); prev())

    return odd(clos)
end

function even()
    while true
        c = readbyte!()
        writebyte(c)
        if !isalpha(c) return c != '.' end
    end
end

evn = false
while evn ? odd() : even()
    evn = !evn
end
```


{{out}}

```txt
what,si,the;gninaem,of:efil.
```



## Kotlin

{{trans|C}}

```scala
// version 1.1.3

typealias Func = () -> Unit

fun doChar(odd: Boolean, f: Func?): Boolean {
    val c = System.`in`.read()
    if (c == -1) return false // end of stream reached
    val ch = c.toChar()

    fun writeOut() {
        print(ch)
        if (f != null) f()
    }

    if (!odd) print(ch)
    if (ch.isLetter()) return doChar(odd, ::writeOut)
    if (odd) {
        if (f != null) f()
        print(ch)
    }
    return ch != '.'
}

fun main(args: Array<String>) {
    repeat(2) {
        var b = true
        while (doChar(!b, null)) b = !b
        System.`in`.read() // remove '\n' from buffer
        println("\n")
    }
}
```


{{out}}

```txt

what,is,the;meaning,of:life.
what,si,the;gninaem,of:efil.

we,are;not,in,kansas;any,more.
we,era;not,ni,kansas;yna,more.


```



## Lasso


```Lasso
define odd_word_processor(str::string) => {
	local(
		isodd 		= false,
		pos 		= 1,
		invpos	 	= 1,
		lastpos		= 1
	)
	while(#str->get(#pos) != '.' && #pos <= #str->size) => {
		if(not #str->isAlpha(#pos)) => {
			not #isodd ? #isodd = true | #isodd = false
		}
		if(#isodd) => {
			#lastpos = 1
			#invpos = 1
			while(#str->isAlpha(#pos+#lastpos) && #pos+#lastpos <= #str->size) => {
				#lastpos++
			}
			'odd lastpos: '+#lastpos+'\r'
			local(maxpos = #pos+#lastpos-1)
			while(#invpos < (#lastpos+1)/2) => {
				local(i,o,ipos,opos)
				#ipos = #pos+#invpos
				#opos = #pos+(#lastpos-#invpos)
				#i = #str->get(#ipos)
				#o = #str->get(#opos)

				//'switching '+#i+' and '+#o+'\r'

				//'insert '+#o+' at '+(#ipos)+'\r'
				#str = string_insert(#str,-position=(#ipos),-text=#o)

				//'remove redundant pos '+(#ipos+1)+'\r'
				#str->remove(#ipos+1,1)

				//'insert '+#i+' at '+(#opos)+'\r'
				#str = string_insert(#str,-position=(#opos),-text=#i)

				//'remove redundant pos '+(#opos+1)+'\r'
				#str->remove(#opos+1,1)

				#invpos++
			}
			#pos += #lastpos - 1
		}
		//#str->get(#pos) + #isodd + '\r'
		#pos += 1
	}
	return #str
}

'orig:\rwhat,is,the;meaning,of:life.\r'
'new:\r'
odd_word_processor('what,is,the;meaning,of:life.')
'\rShould have:\rwhat,si,the;gninaem,of:efil.'
```


{{out}}

```txt
orig:
what,is,the;meaning,of:life.
new:
what,si,the;gninaem,of:efil.
Should have:
what,si,the;gninaem,of:efil.
```



## M2000 Interpreter



### Using Recursion

PepareStream$() with null string set a lambda which read from keyboard and end reading when . found.

PepareStream$() with not a null string make a file and set a lambda to read it
So code works with any stream, until get a null string
We can pass by reference the lambda function but here we pass by reference the f factor, which for keyboard check the end, and for file work as file handler and at then end as a flag which check the end.


```M2000 Interpreter

Module Checkit {
       global out$
       document out$
      Function PrepareStream$ (buf$) {
            \\ get a temporary file
            if buf$="" then {
                  class ref {
                         f
                  class:
                        module ref (.f) { }
                  }
                  \\ make f closure by reference
                  m->ref(false)
                  =lambda$  m->{
                        if m=>f then exit
                        r$=Key$
                        m=>f=r$="."
                        =r$
                  }
                  \\ exit function
                  break
            }
            name$=tempname$
            \\ we use Ansi type files
            Open name$ for output as F
                  Print #F, buf$;
            Close #F
            Open name$ for input as #f
            class ref {
                   f
            class:
                  module ref (.f) { }
            }
            \\ make f closure by reference
            m->ref(f)
            =lambda$ m -> {
                  if m=>f=-1000 then exit
                  def r$
                  if not eof(#m=>f) then  r$=Input$(#m=>f,2)
                  =r$
                  if r$="" or r$="." then close #m=>f : m=>f=-1000
            }
      }
      Module Odd(c$) {
            one$=""
            Module MyEven(c$, &last$) {
                 one$=If$(last$=""->c$(), last$)
                 if one$="" then exit
                 if not one$ ~"[a-zA-Z]" Then last$=one$: exit
                 \\ print before
                 Print one$;
                 out$<=one$
                 Call MyEven, c$, &last$
            }
            Module MyOdd(c$, &last$) {
                 one$=If$(last$=""->c$(), last$)
                 if one$="" then exit
                 if not one$ ~"[a-zA-Z]" Then last$=one$: exit
                 Call MyOdd, c$, &last$
                 \\ print after
                 Print one$;
                  out$<=one$
            }
            Do {
                  one$=""
                  Call MyEven, c$, &one$
                  if one$="" then exit
                  Print one$;
                  out$<=one$
                  one$=""
                  Call MyOdd, c$, &one$
                  if one$="" then exit
                  Print one$;
                  out$<=one$
            }   Always
            Print
            out$<={
            }
      }
      \\ Feeding keyboard
      keyboard "what,is,the;meaning,of:life."
      Odd PrepareStream$("")
      \\ Use a file for input
      Odd PrepareStream$("we,are;not,in,kansas;any,more.")
      clipboard out$
}
Checkit

```


### Using Closure


```M2000 Interpreter

Module OddWord {
      k$=lambda$->""
      state=false
      odd=True
      do {
            a$=key$
            if a$ ~"[a-zA-Z]" then {
                  If state Else state~ : odd~
                  if state and odd then k$=lambda$ k$, a$->a$+k$() : Continue
                  Print a$;
            } Else {
                  If state Then state~
                  if odd then Print k$(); : k$=lambda$->""
                  Print a$;
            }
      } until a$="."
      Print
}
keyboard "what,is,the;meaning,of:life."
OddWord
Keyboard "we,are;not,in,kansas;any,more."
OddWord

```


{{out}}

```txt

what,si,the;gninaem,of:efil.
we,era;not,ni,kansas;yna,more.

```



## Nim

{{trans|Python}}

```nim
import os, unicode, future

proc nothing(): bool{.closure.} = false

proc odd(prev = nothing): bool =
  let a = stdin.readChar()
  if not isAlpha(Rune(ord(a))):
    discard prev()
    stdout.write(a)
    return a != '.'

  # delay action until later, in the shape of a closure
  proc clos(): bool =
    stdout.write(a)
    prev()

  return odd(clos)

proc even(): bool =
  while true:
    let c = stdin.readChar()
    stdout.write(c)
    if not isAlpha(Rune(ord(c))):
      return c != '.'

var e = false
while (if e: odd() else: even()):
  e = not e
```

Output:

```txt
$ echo "what,is,the;meaning,of:life." | ./oddword
what,si,the;gninaem,of:efil.
echo "we,are;not,in,kansas;any,more." | ./oddword
we,era;not,ni,kansas;yna,more.
```



## OCaml



```ocaml
let is_alpha c =
  c >= 'a' && c <= 'z' ||
  c >= 'A' && c <= 'Z'

let rec odd () =
  let c = input_char stdin in
  if is_alpha c
  then (let e = odd () in print_char c; e)
  else (c)

let rec even () =
  let c = input_char stdin in
  if is_alpha c
  then (print_char c; even ())
  else print_char c

let rev_odd_words () =
  while true do
    even ();
    print_char (odd ())
  done

let () =
  try rev_odd_words ()
  with End_of_file -> ()
```


Executing:


```txt
$ echo "what,is,the;meaning,of:life." | ocaml odd_word_problem.ml
what,si,the;gninaem,of:efil.
```



## Ol

Use string iterators.

```scheme

(define (odd_word_problem words)
   (letrec ((odd (lambda (s out)
               (let loop ((s s) (l '()))
                  (cond
                     ((null? s)
                        out)
                     ((pair? s)
                        (if (<= #\a (car s) #\z)
                           (loop (cdr s) (cons (car s) l))
                           (even (cdr s) (cons (cons (reverse l) (car s)) out))))
                     (else
                        (loop (s) l))))))
            (even (lambda (s out)
               (let loop ((s s) (l '()))
                  (cond
                     ((null? s)
                        out)
                     ((pair? s)
                        (if (<= #\a (car s) #\z)
                           (loop (cdr s) (cons (car s) l))
                           (odd (cdr s) (cons (cons l (car s)) out))))
                     (else
                        (loop (s) l)))))))
      (for-each (lambda (p)
                  (display (runes->string (car p)))
                  (display (string (cdr p))))
         (reverse
            (odd (str-iter words) '()))))
      (print))
(odd_word_problem "what,is,the;meaning,of:life.")
(odd_word_problem "we,are;not,in,kansas;any,more.")

```


Output:

```txt

what,si,the;gninaem,of:efil.
we,era;not,ni,kansas;yna,more.

```




## Perl

All versions process text until EOF, not dot.

Input:

```txt
what,is,the;meaning,of:life.
```

Output:

```txt
what,si,the;gninaem,of:efil.
```



### Closure version


```Perl
sub r
{
	my ($f, $c) = @_;
	return sub { print $c; $f->(); };
}

$r = sub {};

while (read STDIN, $_, 1) {
	$w = /^[a-zA-Z]$/;
	$n++ if ($w && !$l);
	$l = $w;
	if ($n & 1 || !$w) {
		$r->(); $r = sub{};
		print;
	} else {
		$r = r($r, $_);
	}
}
$r->();
```


### Recursion version


```Perl
sub rev
{
	my $c;
	read STDIN, $c, 1;
	if ($c =~ /^[a-zA-Z]$/) {
		my $r = rev();
		print $c;
		return $r;
	} else {
		return $c;
	}
}

while (read STDIN, $_, 1) {
	$w = /^[a-zA-Z]$/;
	$n++ if ($w && !$l);
	$l = $w;
	if ($n & 1) {
		print;
	} else {
		my $r = rev();
		print $_;
		print $r;
		$n = 0; $l = 0;
	}
}
```

===Threads (processes) version===
Perl still has weak threads support. Far more safe yet portable is to use processes (fork).

Here, fork is used instead of threads and pipe is used instead of conditional variable.

```Perl
$|=1;

while (read STDIN, $_, 1) {
	$w = /^[a-zA-Z]$/;
	$n++ if ($w && !$l);
	$l = $w;
	if ($n & 1 || !$w) {
		close W; while(wait()!=-1){}
		print;
	} else {
		open W0, ">&", \*W;
		close W;
		pipe R,W;
		if (!fork()) {
			close W;
			<R>;
			print $_;
			close W0;
			exit;
		}
		close W0;
		close R;
	}
}
```



## Perl 6

A recursive solution, with the added feature that it treats each line separately.

```perl6
my &in = { $*IN.getc // last }

loop {
    ew(in);
    ow(in).print;
}

multi ew ($_ where /\w/) { .print; ew(in); }
multi ew ($_)            { .print; next when "\n"; }

multi ow ($_ where /\w/) { ow(in) x .print; }
multi ow ($_)            { $_; }
```

{{out}}

```txt
$ ./oddword
we,are;not,in,kansas;any,more.
we,era;not,ni,kansas;yna,more.
what,is,the;meaning,of:life.
what,si,the;gninaem,of:efil.
```

Note how the even/oddness is reset on the line boundary; if not, the second line might have started out in an odd state and reversed "what" instead of "is".  The call to <tt>next</tt> prevents that by sending the loop back to its initial state.

There is one clever trick here with the <tt>x</tt> operator; it evaluates both its arguments in order, but in this case only returns the left argument because the right one is always 1 (True).  You can think of it as a reversed C-style comma operator.


## Phix

Uses plain and simple recursion, no closures, and no other tricks.

To test direct console input, comment out string s .. getchar(), and uncomment getc(0) and the prompt.

Likewise use integer fn = open("somefile","r"), and getc(fn) should you want to test file i/o.

```Phix
string s = "what,is,the;meaning,of:life."
--string s = "we,are;not,in,kansas;any,more."
integer i = 0

function getchar()
    i += 1
    return s[i]
end function

function wrod(integer rev)
    integer ch = getchar(), nch
--  integer ch = getc(0), nch
    if not find(ch," .,:;!?") then
        if rev then
            nch = wrod(rev)
        end if
        puts(1,ch)
        if not rev then
            nch = wrod(rev)
        end if
        ch = nch
    end if
    return ch
end function

--puts(1,"Enter words separated by a single punctuation mark (i.e. !?,.;:) and ending with .\n")
integer rev = 0
while 1 do
    integer ch = wrod(rev)
    puts(1,ch)
    if ch='.' then exit end if
    rev = 1-rev
end while
```

{{out}}

```txt

what,si,the;gninaem,of:efil.
we,era;not,ni,kansas;yna,more.

```



## PHP

{{trans|Python}}

```PHP
$odd = function ($prev) use ( &$odd ) {
	$a = fgetc(STDIN);
	if (!ctype_alpha($a)) {
		$prev();
		fwrite(STDOUT, $a);
		return $a != '.';
	}
	$clos = function () use ($a , $prev) {
		fwrite(STDOUT, $a);
		$prev();
	};
	return $odd($clos);
};
$even = function () {
	while (true) {
		$c = fgetc(STDIN);
		fwrite(STDOUT, $c);
		if (!ctype_alpha($c)) {
			return $c != ".";
		}
	}
};
$prev = function(){};
$e = false;
while ($e ? $odd($prev) : $even()) {
	$e = !$e;
}
```



## PicoLisp


```PicoLisp
(de oddWords ()
   (use C
      (loop
         (until (sub? (prin (setq C (char))) "!,.:;?"))
         (T (= "." C))
         (setq C (char))
         (T
            (= "."
               (prin
                  (recur (C)
                     (if (sub? C "!,.:;?")
                        C
                        (prog1 (recurse (char)) (prin C)) ) ) ) ) ) )
   (prinl) ) )
```

Test:

```PicoLisp
(in "txt1" (oddWords))
(in "txt2" (oddWords))
```

Output:

```txt
what,si,the;gninaem,of:efil.
we,era;not,ni,kansas;yna,more.
```



## PL/I


```PL/I
test: procedure options (main);  /* 2 August 2014 */
   declare (ch, ech) character (1);
   declare odd file;

get_word: procedure recursive;
   declare ch character (1);

   get file (odd) edit (ch) (a(1));
   if index('abcdefghijklmnopqrstuvwxyz', ch) > 0 then call get_word;
   if index('abcdefghijklmnopqrstuvwxyz', ch) > 0 then
      put edit (ch) (a);
   else ech = ch;
end get_word;

   open file (odd) input title ('/ODDWORD.DAT,TYPE(text),recsize(100)');
   do forever;
      do  until (index('abcdefghijklmnopqrstuvwxyz', ch) = 0 );
         get file (odd) edit (ch) (a(1)); put edit (ch) (a);
      end;
      if ch = '.' then leave;
      call get_word;
      put edit (ech) (a);
      if ech = '.' then leave;
   end;
end test;
```

file:

```txt

what,is,the;meaning,of:life.

```

output:

```txt

what,si,the;gninaem,of:efil.

```

file:

```txt

we,are;not,in,kansas;any,more.

```

output:

```txt

we,era;not,ni,kansas;yna,more.

```



## Prolog

Works with SWI-Prolog.

```Prolog
odd_word_problem :-
	read_line_to_codes(user_input, L),
	even_word(L, Out, []),
	string_to_list(Str, Out),
	writeln(Str).

even_word(".") --> ".".

even_word([H | T]) -->
	{char_type(H,alnum)},
	[H],
	even_word(T).

even_word([H | T]) -->
	[H],
	odd_word(T, []).

odd_word(".", R) --> R, ".".

odd_word([H|T], R) -->
	{char_type(H,alnum)},
	odd_word(T, [H | R]).

odd_word([H|T], R) -->
	R,
	[H],
	even_word(T).

```

Output :

```txt
?- odd_word_problem.
|: what,is,the;meaning,of:life.
what,si,the;gninaem,of:efil.
true .

?- odd_word_problem.
|: we,are;not,in,kansas;any,more.
we,era;not,ni,kansas;yna,more.
true .

```


## PureBasic

This example uses recursion.

```PureBasic
#False = 0
#True = 1

Global *inputPtr.Character

Macro nextChar()
  *inputPtr + SizeOf(Character)
EndMacro

Procedure isPunctuation(c.s)
  If FindString("!?()[]{},.;:-'" + #DQUOTE$, c)
    ProcedureReturn #True
  EndIf
  ProcedureReturn #False
EndProcedure

Procedure oddWord()
  Protected c.c
  c = *inputPtr\c
  If isPunctuation(Chr(*inputPtr\c))
    ProcedureReturn
  Else
    nextChar()
    oddWord()
  EndIf
  Print(Chr(c))
EndProcedure

Procedure oddWordProblem(inputStream.s)
  *inputPtr = @inputStream
  Define isOdd = #False
  While *inputPtr\c
    If isOdd
      oddWord()
    Else
      Repeat
        Print(Chr(*inputPtr\c))
        nextChar()
      Until isPunctuation(Chr(*inputPtr\c))
    EndIf
    Print(Chr(*inputPtr\c))
    isOdd ! 1 ;toggle word indicator
    nextChar()
  Wend
EndProcedure

Define inputStream.s
If OpenConsole()
  Repeat
    PrintN(#CRLF$ + #CRLF$ + "Enter a series of words consisting only of English letters (i.e. a-z, A-Z)")
    PrintN("and that are separated by a punctuation mark (i.e. !?()[]{},.;:-' or " + #DQUOTE$ + ").")
    inputStream = Input()
    oddWordProblem(inputStream) ;assume input is correct
  Until inputStream = ""

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
Enter a series of words consisting only of English letters (i.e. a-z, A-Z)
and that are separated by a punctuation mark (i.e. !?()[]{},.;:-' or ").
what,is,the;meaning,of:life.
what,si,the;gninaem,of:efil.

Enter a series of words consisting only of English letters (i.e. a-z, A-Z)
and that are separated by a punctuation mark (i.e. !?()[]{},.;:-' or ").
we,are;not,in,kansas;any,more.
we,era;not,ni,kansas;yna,more.
```



## Python


```python
from sys import stdin, stdout

def char_in(): return stdin.read(1)
def char_out(c): stdout.write(c)

def odd(prev = lambda: None):
	a = char_in()
	if not a.isalpha():
		prev()
		char_out(a)
		return a != '.'

	# delay action until later, in the shape of a closure
	def clos():
		char_out(a)
		prev()

	return odd(clos)

def even():
	while True:
		c = char_in()
		char_out(c)
		if not c.isalpha(): return c != '.'

e = False
while odd() if e else even():
	e = not e
```

Running:<lang>$ echo "what,is,the;meaning,of:life." | python odd.py
what,si,the;gninaem,of:efil.
$ echo "we,are;not,in,kansas;any,more." | python odd.py
we,era;not,ni,kansas;yna,more.
```


{{trans|Scheme}}
In this version, the action of printing the terminating punctuation is put in a closure and returned by <code>odd()</code>.

```python
from sys import stdin, stdout

def char_in(): return stdin.read(1)
def char_out(c): stdout.write(c)

def odd():
	a = char_in()
	if a.isalpha():
		r = odd()
		char_out(a)
		return r

	# delay printing terminator until later, in the shape of a closure
	def clos():
		char_out(a)
		return a != '.'

	return clos

def even():
	while True:
		c = char_in()
		char_out(c)
		if not c.isalpha(): return c != '.'

e = False
while odd()() if e else even():
	e = not e
```



### Using coroutines and recursion

{{trans|Ruby}}
{{trans|Tcl}}
{{works with|Python|3.3+}}

```python
from sys import stdin, stdout

def fwd(c):
    if c.isalpha():
        return [stdout.write(c), (yield from fwd((yield f)))][1]
    else:
        return c

def rev(c):
    if c.isalpha():
        return [(yield from rev((yield r))), stdout.write(c)][0]
    else:
        return c

def fw():
    while True:
        stdout.write((yield from fwd((yield r))))

def re():
    while True:
        stdout.write((yield from rev((yield f))))

f = fw()
r = re()
next(f)
next(r)

coro = f
while True:
    c = stdin.read(1)
    if not c:
        break
    coro = coro.send(c)
```



## Racket

Simple solution, using a continuation thunk for the reverse parts.

```racket

#!/bin/sh
#|
exec racket -tm- "$0" "$@"
|#

#lang racket

(define (even k)
  (define c (read-char))
  (cond [(eq? c eof) (k)]
        [(not (char-alphabetic? c)) (k) (write-char c) (odd)]
        [else (even (λ() (write-char c) (k)))]))

(define (odd)
  (define c (read-char))
  (unless (eq? c eof)
    (write-char c)
    (if (char-alphabetic? c) (odd) (even void))))

(provide main)
(define (main) (odd) (newline))

;; (with-input-from-string "what,is,the;meaning,of:life." main)
;; ;; -> what,si,the;gninaem,of:efil.
;; (with-input-from-string "we,are;not,in,kansas;any,more." main)
;; ;; -> we,era;not,ni,kansas;yna,more.

```



## REXX

The REXX program writes some header information to aid in visual fidelity when displaying the output to the

screen (also a blank line is written to make the screen display righteous;   it's assumed that writing titles and

blank lines doesn't break the spirit of the restrictions (single character I/O)   [the 8<sup>th</sup> line with the three <big><big>'''say'''</big></big>s].

This displaying of informative messages is only to help the observer to know what is being performed.

No recursion or the stack is used.   The program could've been written without subroutines.

```rexx
/*REXX program  solves the  odd word  problem  by  only using  byte  input/output.      */
iFID_ = 'ODDWORD.IN'                             /*Note:  numeric suffix is added later.*/
oFID_ = 'ODDWORD.'                               /*  "       "       "    "   "     "   */

     do case=1  for 2;   #=0                     /*#:  is the number of characters read.*/
     iFID=iFID_ || case                          /*read   ODDWORD.IN1  or  ODDWORD.IN2  */
     oFID=oFID_ || case                          /*write  ODDWORD.1    or  ODDWORD.2    */
     say;   say;    say '════════ reading file: '        iFID        "════════"                /* ◄■■■■■■■■■ optional. */

         do  until x==.                               /* [↓]  perform for  "odd"  words.*/
            do  until \isMix(x);                      /* [↓]  perform until punct found.*/
            call readChar;    call writeChar          /*read and write a letter.        */
            end   /*until ¬isMix(x)*/                 /* [↑]  keep reading    "     "   */
         if x==.  then leave                          /*is this the  end─of─sentence ?  */
         call readLetters;    punct=#                 /*save the location of punctuation*/
            do j=#-1  by -1;  call readChar j         /*read previous word (backwards). */
            if \isMix(x)  then leave;  call writeChar /*Found punctuation?  Then leave. */
            end   /*j*/                               /* [↑]  perform for  "even" words.*/
         call readLetters;   call writeChar;  #=punct /*read/write letters; new location*/
         end      /*until x==.*/
     end          /*case*/                            /* [↑]  process both input files. */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
isMix:       return datatype( arg(1), 'M')       /*return   1   if argument is a letter.*/
readLetters: do  until  \isMix(x);          call readChar;         end;             return
writeChar:   call charout , x;              call charout oFID, x;                   return
/*──────────────────────────────────────────────────────────────────────────────────────*/
readChar:    if arg(1)==''  then do;  x=charin(iFID);  #=#+1;  end  /*read the next char*/
                            else      x=charin(iFID, arg(1))        /*  "  specific   " */
             return
```

'''output'''   when using two (default) input files which contain:
:*   input file   '''ODDWORD.IN1'''   ───►   <tt> what,is,the;meaning,of:life. </tt>
:*   input file   '''ODDWORD.IN2'''   ───►   <tt> we,are;not,in,kansas;any,more. </tt>

The output is written to the terminal screen as well as to a couple of unique files.   Only the screen output is

shown here, the output files mirror the display except for the headers   (''reading file:   xxx'')   and the writing

(SAYing) of blank lines which helps keep the screen righteous after using the REXX   '''charout'''   BIF   which

wrote to the terminal.

```txt

════════ reading file:  ODDWORD.IN1 ════════
what,si,the;gninaem,of:efil.

════════ reading file:  ODDWORD.IN2 ════════
we,era;not,ni,kansas;yna,more.

```



## Ring


```ring

# Project : Odd word problem

test = "what,is,the;meaning,of:life."
n1 = 1
testarr = []
testorigin = test
test = substr(test, ",", " ")
test = substr(test, ";", " ")
test = substr(test, ":", " ")
test = substr(test, ".", " ")

while true
      n2 = substring(test, " ", n1)
      n3 = substring(test, " ", n2 + 1)
      if n2>0 and n3>0
         strcut = substr(test, n2 + 1, n3 - n2)
         strcut = trim(strcut)
         if strcut != ""
            add(testarr, strcut)
            n1 = n3 + 1
         else
            exit
         ok
      ok
end

for n = 1 to len(testarr)
    strrev = revstr(testarr[n])
    testorigin = substr(testorigin, testarr[n], strrev)
next
see testorigin + nl

func Substring str,substr,n
     newstr=right(str,len(str)-n+1)
     nr = substr(newstr, substr)
     return n + nr -1

func revstr(cStr)
     cStr2 = ""
     for x = len(cStr) to 1 step -1
         cStr2 += cStr[x]
     next
     return cStr2

```

Output:

```txt

what,si,the;gninaem,of:efil.

```



## Ruby

These Ruby programs store each character in a single-character string.


### Using fibers and recursion

{{trans|Tcl}}
{{works with|Ruby|1.9}}

```ruby
f, r = nil
fwd = proc {|c|
  c =~ /[[:alpha:]]/ ? [(print c), fwd[Fiber.yield f]][1] : c }
rev = proc {|c|
  c =~ /[[:alpha:]]/ ? [rev[Fiber.yield r], (print c)][0] : c }

(f = Fiber.new { loop { print fwd[Fiber.yield r] }}).resume
(r = Fiber.new { loop { print rev[Fiber.yield f] }}).resume

coro = f
until $stdin.eof?
  coro = coro.resume($stdin.getc)
end
```



### Using continuations

{{trans|Factor}}
{{libheader|continuation}}

```ruby
require 'continuation' unless defined? Continuation
require 'stringio'

# Save current continuation.
def savecc(*data)
  # With MRI 1.8 (but not 1.9), the array literal
  #   [callcc {|cc| cc}, *data]
  # used the wrong return value from callcc. The workaround is to
  # put callcc outside the array literal.
  continuation = callcc {|cc| cc}
  [continuation, *data]
end

# Jump back to continuation, where savecc will return [nil, *data].
def jump_back(continuation)
  continuation[nil]
end

def read_odd_word(input, output)
  first_continuation, last_continuation = nil
  reverse = false
  # Read characters. Loop until end of stream.
  while c = input.getc
    c = c.chr   # For Ruby 1.8, convert Integer to String.
    if c =~ /[[:alpha:]]/
      # This character is a letter.
      if reverse
        # Odd word: Write letters in reverse order.
        saving, last_continuation, c = savecc(last_continuation, c)
        if saving
          last_continuation = saving
        else
          # After jump: print letters in reverse.
          output.print c
          jump_back last_continuation
        end
      else
        # Even word: Write letters immediately.
        output.print c
      end
    else
      # This character is punctuation.
      if reverse
        # End odd word. Fix trampoline, follow chain of continuations
        # (to print letters in reverse), then bounce off trampoline.
        first_continuation, c = savecc(c)
        if first_continuation
          jump_back last_continuation
        end
        output.print c      # Write punctuation.
        reverse = false     # Begin even word.
      else
        output.print c      # Write punctuation.
        reverse = true      # Begin odd word.
        # Create trampoline to bounce to (future) first_continuation.
        last_continuation, = savecc
        unless last_continuation
          jump_back first_continuation
        end
      end
    end
  end
  output.puts   # Print a cosmetic newline.
end

def odd_word(string)
  read_odd_word StringIO.new(string), $stdout
end

odd_word "what,is,the;meaning,of:life."
odd_word "we,are;not,in,kansas;any,more."
```



## Run BASIC


```runbasic
open "oddWord.txt" for input as #f               ' read input stream
while not(eof(#f))
line input #f, a$
oddW$  = ""                                      ' begin the result oddW with blank
px     = 0                                       ' begin word search location with 0
count  = 0                                       ' begin the word count to 0
while x < len(a$)                                ' look at each character
	x    = instr(a$,",",px)                  ' search for comma (,)
	if x = 0 then x = len(a$)                ' no more commas?
	x1   = instr(a$,";",px)                  ' search for (;)
	x2   = instr(a$,":",px)                  ' search for (:)
	if x1 <> 0 then x = min(x,x1)            ' what came first the , ; or :
	if x2 <> 0 then x = min(x,x2)

	w$  = mid$(a$,px,x - px)                 ' get the word seperated by , ; or :

	if count and 1 then                      ' is it the odd word
	        w1$ = ""
		for i = len(w$) to 1 step -1
			w1$ = w1$ + mid$(w$,i,1) ' reverse odd words
		next i
		w$ = w1$
	end if
        oddW$ = oddW$ + w$ + mid$(a$,x,1)        ' add the word to the end of oddW$
	px    = x + 1                            ' bump word search location for next while
	count = count + 1                        ' count the words
wend
print a$;" -> ";oddW$                            ' print the original and result
next ii
wend
close #f
```


```txt
what,is,the;meaning,of:life.   -> what,si,the;gninaem,of:efil.
we,are;not,in,kansas;any,more. -> we,era;not,ni,kansas;yna,more.
```


## Scala


```Scala
import scala.io.Source
import java.io.PrintStream

def process(s: Source, p: PrintStream, w: Int = 0): Unit = if (s.hasNext) s.next match {
  case '.' => p append '.'
  case c if !Character.isAlphabetic(c) => p append c; reverse(s, p, w + 1)
  case c => p append c; process(s, p, w)
}

def reverse(s: Source, p: PrintStream, w: Int = 0, x: Char = '.'): Char = s.next match {
  case c if !Character.isAlphabetic(c) => p append x; c
  case c => val n = reverse(s, p, w, c);
    if (x == '.') {p append n; process(s, p, w + 1)} else p append x; n
}

process(Source.fromString("what,is,the;meaning,of:life."), System.out); println
process(Source.fromString("we,are;not,in,kansas;any,more."), System.out); println
```

{{out}}

```txt
what,si,the;gninaem,of:efil.
we,era;not,ni,kansas;yna,more.
```



## Scheme

Output is identical to python.

```lisp
(define (odd)
  (let ((c (read-char)))
    (if (char-alphabetic? c)
      (let ((r (odd)))
	(write-char c)
	r)
      (lambda () (write-char c) (char=? c #\.)))))

(define (even)
  (let ((c (read-char)))
    (write-char c)
    (if (char-alphabetic? c)
      (even)
      (char=? c #\.))))

(let loop ((i #f))
  (if (if i ((odd)) (even))
    (exit)
    (loop (not i))))
```



## Seed7


```seed7
$ include "seed7_05.s7i";
  include "chartype.s7i";

const func char: doChar (in boolean: doReverse) is func
  result
    var char: delimiter is ' ';
  local
    var char: ch is ' ';
  begin
    ch := getc(IN);
    if ch in letter_char then
      if doReverse then
        delimiter := doChar(doReverse);
        write(ch);
      else
        write(ch);
        delimiter := doChar(doReverse);
      end if;
    else
      delimiter := ch;
    end if;
  end func;

const proc: main is func
  local
    var char: delimiter is ' ';
    var boolean: doReverse is FALSE;
  begin
    repeat
      delimiter := doChar(doReverse);
      write(delimiter);
      doReverse := not doReverse;
    until delimiter = '.';
    writeln;
  end func;
```


{{out}}

```txt

> s7 oddWordProblem
SEED7 INTERPRETER Version 5.0.5203  Copyright (c) 1990-2014 Thomas Mertes
what,is,the;meaning,of:life.
what,si,the;gninaem,of:efil.

```



## Sidef

Recursive solution:
{{trans|Perl}}

```ruby
func rev {
    (var c = STDIN.getc) \\ return()
    if (c ~~ /^[a-z]\z/i) {
        var r = rev()
        print c
        return r
    }
    return c
}

var (n=0, l=false)
while (defined(var c = STDIN.getc)) {
    var w = (c ~~ /^[a-z]\z/i)
    ++n if (w && !l)
    l = w
    if (n & 1) {
        print c
    } else {
        var r = rev()
        print(c, r)
        n = 0
        l = false
    }
}
```

{{out}}

```txt

$ echo 'what,is,the;meaning,of:life.' | sidef script.sf
what,si,the;gninaem,of:efil.

$ echo 'we,are;not,in,kansas;any,more.' | sidef script.sf
we,era;not,ni,kansas;yna,more.

```



## Tcl

Although the input is handled as strings, they're all as single-character strings.
{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

proc fwd c {
    expr {[string is alpha $c] ? "[fwd [yield f][puts -nonewline $c]]" : $c}
}
proc rev c {
    expr {[string is alpha $c] ? "[rev [yield r]][puts -nonewline $c]" : $c}
}
coroutine f while 1 {puts -nonewline [fwd [yield r]]}
coroutine r while 1 {puts -nonewline [rev [yield f]]}
for {set coro f} {![eof stdin]} {} {
    set coro [$coro [read stdin 1]]
}
```

Output is identical to Python and Scheme versions.

The only difference between the two coroutines (apart from the different names used when flipping back and forth) is the timing of the write of the character with respect to the recursive call.

## TUSCRIPT

{{incorrect|Run BASIC|You are supposed to read characters one by one and not store them in arrays.}}


```tuscript

$$ MODE TUSCRIPT
inputstring=*
DATA what,is,the;meaning,of:life.
DATA we,are;not,in,kansas;any,more.

BUILD C_GROUP >[pu]=".,;:-"

LOOP i=inputstring
pu=STRINGS (i,"|>[pu]|")
wo=STRINGS (i,"|<></|")
outputstring=""
 loop n,w=wo,p=pu
 r=MOD(n,2)
 IF (r==0) w=TURN (w)
 outputstring=CONCAT(outputstring,w,p)
ENDLOOP
PRINT outputstring
ENDLOOP

```

Output:

```txt

what,si,the;gninaem,of:efil.Brian:
we,era;not,ni,kansas;yna,more.

```



## VBA

First way :

```vb
Private Function OddWordFirst(W As String) As String
Dim i As Integer, count As Integer, l As Integer, flag As Boolean, temp As String
   count = 1
   Do
      flag = Not flag
      l = FindNextPunct(i, W) - count + 1
      If flag Then
         temp = temp & ExtractWord(W, count, l)
      Else
         temp = temp & ReverseWord(W, count, l)
      End If
   Loop While count < Len(W)
   OddWordFirst = temp
End Function

Private Function FindNextPunct(d As Integer, W As String) As Integer
Const PUNCT As String = ",;:."
   Do
      d = d + 1
   Loop While InStr(PUNCT, Mid(W, d, 1)) = 0
   FindNextPunct = d
End Function

Private Function ExtractWord(W As String, c As Integer, i As Integer) As String
   ExtractWord = Mid(W, c, i)
   c = c + Len(ExtractWord)
End Function

Private Function ReverseWord(W As String, c As Integer, i As Integer) As String
Dim temp As String, sep As String
   temp = Left(Mid(W, c, i), Len(Mid(W, c, i)) - 1)
   sep = Right(Mid(W, c, i), 1)
   ReverseWord = StrReverse(temp) & sep
   c = c + Len(ReverseWord)
End Function
```

Second way :

```vb
Private Function OddWordSecond(Words As String) As String
Dim i&, count&, t$, cpt&, j&, l&, d&, f As Boolean
Const PUNCT As String = ",;:"
   For i = 1 To Len(Words)
      'first word
      If i = 1 Then
         cpt = 1
         Do
            t = t & Mid(Words, cpt, 1)
            cpt = cpt + 1
         Loop While InStr(PUNCT, Mid(Words, cpt, 1)) = 0 And cpt < Len(Words)
         i = cpt
         t = t & Mid(Words, i, 1)
      End If
      If Right(t, 1) = "." Then Exit For
      'Odd words ==> reverse
      While InStr(PUNCT, Mid(Words, cpt + 1, 1)) = 0 And cpt < Len(Words)
         cpt = cpt + 1
      Wend
      l = IIf(f = True, i, i + 1)
      d = IIf(cpt = Len(Words), cpt - 1, cpt)
      For j = d To l Step -1
         t = t & Mid(Words, j, 1)
      Next
      If cpt = Len(Words) Then t = t & ".": Exit For
      f = True
      i = cpt + 1
      t = t & Mid(Words, i, 1)
      'Even words
      cpt = i + 1
      t = t & Mid(Words, cpt, 1)
      Do
         cpt = cpt + 1
         t = t & Mid(Words, cpt, 1)
      Loop While InStr(PUNCT, Mid(Words, cpt, 1)) = 0 And cpt < Len(Words)
      i = cpt
   Next
   OddWordSecond = t
End Function
```

To call Functions :

```vb
Option Explicit

Sub Main()
   Debug.Print "Input : " & "we,are;not,in,kansas;any,more."
   Debug.Print "First way : " & OddWordFirst("we,are;not,in,kansas;any,more.")
   Debug.Print "Second way : " & OddWordSecond("we,are;not,in,kansas;any,more.")
   Debug.Print ""
   Debug.Print "Input : " & "what,is,the;meaning,of:life."
   Debug.Print "First way : " & OddWordFirst("what,is,the;meaning,of:life.")
   Debug.Print "Second way : " & OddWordSecond("what,is,the;meaning,of:life.")
End Sub
```

{{out}}

```txt
Input : we,are;not,in,kansas;any,more.
First way : we,era;not,ni,kansas;yna,more.
Second way : we,era;not,ni,kansas;yna,more.

Input : what,is,the;meaning,of:life.
First way : what,si,the;gninaem,of:efil.
Second way : what,si,the;gninaem,of:efil.
```



## zkl

{{trans|D}}

```zkl
var [const] delim=",:;/?!@#$%^&*()_+", stop=".";
fcn oddly(inStream){
   inStream=inStream.walker(3);  // character iterator: string, file, etc
   doWord:=fcn(inStream,rev,f){  // print next word forewards or reverse
      c:=inStream.next();
      if(not rev) c.print();
      if(not (c==stop or delim.holds(c)))
	 return(self.fcn(inStream,rev,'{ c.print(); f(); }));
      if(rev){ f(); c.print(); }
      return(c!=stop);
   };
   tf:=Walker.cycle(False,True);  // every other word printed backwords
   while(doWord(inStream, tf.next(), Void)) {}
   println();
}
```

Showing two different input streams:

```zkl
oddly("what,is,the;meaning,of:life.");
oddly(Data(0,String,"we,are;not,in,kansas;any,more."));
```

{{out}}

```txt

what,si,the;gninaem,of:efil.
we,era;not,ni,kansas;yna,more.

```


{{omit from|Mathematica}}

[[Category:Handicap]]
