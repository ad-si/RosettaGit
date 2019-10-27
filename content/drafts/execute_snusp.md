+++
title = "Execute SNUSP"
description = ""
date = 2019-10-04T15:47:19Z
aliases = []
[extra]
id = 2843
[taxonomies]
categories = []
tags = []
+++

{{task}}[[Category:Compilers and Interpreters]]
{{implementation|SNUSP}}'''RCSNUSP''' is a set of [[SNUSP]] compilers and interpreters written for Rosetta Code in a variety of languages. Below are links to each of the versions of RCSNUSP.

An implementation need only properly implement the Core SNUSP instructions <nowiki>('$', '\', '/', '+', '-', '<', '>', ',', '.', '!', and '?')</nowiki>. Modular SNUSP ('#', '@') and Bloated SNUSP (':', ';', '%', and '&') are also allowed, but not required. Any extra characters that you implement should be noted in the description of your implementation. Any cell size is allowed, EOF support is optional, as is whether you have bounded or unbounded memory.


## 11l

{{trans|Python}}

```11l
V HW = ‘
/++++!/
### =====
?\>++.>+.+++++++..+++\
\+++\ | /+>+++++++>/ /++++++++++<<.++>./
$+++/ | \+++++++++>\ \+++++.>.+++.-----\
      \==-<<<<+>+++/ /=.>.+>.--------.-/’

F snusp(store, code)
   V ds = [Byte(0)] * store
   V dp = 0
   V cs = code.split("\n")
   V ipr = 0
   V ipc = 0

   L(row) cs
      ipc = row.findi(‘$’)
      I ipc != -1
         ipr = L.index
         L.break

   V id = 0

   F step()
      I @id [&] 1
         @ipr += 1 - (@id [&] 2)
      E
         @ipc += 1 - (@id [&] 2)

   L ipr >= 0 & ipr < cs.len & ipc >= 0 & ipc < cs[ipr].len
      S cs[ipr][ipc]
         ‘>’
            dp++
         ‘<’
            dp--
         ‘+’
            ds[dp]++
         ‘-’
            ds[dp]--
         ‘.’
            :stdout.write(Char(code' ds[dp]))
         ‘,’
            ds[dp] = Byte(:stdin.read(1).code)
         ‘/’
            id = (-)id
         ‘\’
            id (+)= 1
         ‘!’
            step()
         ‘?’
            I !(ds[dp])
               step()
      step()

snusp(5, HW)
```

{{out}}

```txt

Hello World!

```



## Ada

See [[Execute SNUSP/Ada]].


## AutoHotkey

See [[RCSNUSP/AutoHotkey]].


## C

See [[RCSNUSP/C]].


## C++

See [[RCSNUSP/C++]].


## COBOL

See [[RCSNUSP/COBOL]].


## D

See [[RCSNUSP/D]].

=={{header|F_Sharp|F#}}==
See [[RCSNUSP/F Sharp]].


## Factor

See [[RCSNUSP/Factor]].


## Go

See [[RCSNUSP/Go]].


## Haskell

See [[RCSNUSP/Haskell]].


## J

This program places no limits on the program or data size.  Perhaps I'll revisit and write a tacit version of the SNUSP interpreter.

```J

Note 'snusp'

    Without $ character the program counter starts at top left (0 0) moving to the right (0 1)

    >       increment the pointer (to point to the next cell to the right).
    <       decrement the pointer (to point to the next cell to the left).
    +       increment (increase by one) the cell at the pointer.
    -       decrement (decrease by one) the cell at the pointer.
    .       output the value of the cell at the pointer as a character.
    ,       accept one character of input, storing its value in the cell at the pointer.
    \/      mirrors
    ?       skip if memory pointer is 0
    !       skip
    $       optional start program here (also heading to the right)

)

smoutput 'Toroidal programs run forever.  Use ^C to interrupt.'

main =: 3 : 0  NB. use: main 'program.snusp'
 PROGRAM =: [;._2 LF ,~ 1!:1 boxopen y
 SHAPE =: $PROGRAM
 PC =: SHAPE#:(,PROGRAM) i.'$'
 PCSTEP =: 0 1
 CELL =: 0
 CELLS =: ,0
 while. 1 do. NB. for_i. i.400 do.
  INSTRUCTION =: (<PC) { PROGRAM
  STEP =: PCSTEP
  select. INSTRUCTION
  case. '<' do.
   CELL =: <: CELL
   if. CELL < 0 do.
    CELL =: 0
    CELLS =: 0 , CELLS
   end.
  case. '>' do.
   CELL =: >: CELL
   if. CELL >: # CELLS do.
    CELLS =: CELLS , 0
   end.
  case. ;/'-+' do. CELLS =: CELL ((<:'- +'i.INSTRUCTION)+{)`[`]} CELLS
  case. '.' do. 1!:2&4 (CELL{CELLS){a.
  case. ',' do. CELLS =: (1!:1<1) CELL } CELLS
  fcase. '/' do. STEP =: - STEP
  case. '\' do. STEP =: PCSTEP =: |. STEP
  case. '?' do. STEP =: +:^:(0 = CELL{CELLS) STEP
  case. '!' do. STEP =: +: STEP
  end.
  PC =: (| (PC + STEP + ])) SHAPE  NB. toroidal
  NB. smoutput PC;CELL;CELLS  NB. debug
 end.
)

```

Store 
```SNUSP
\      display  JJ and linefeed, then loop forever
\      +++++++++++++++++++++++++++++++++++++\
    ! /+++++++++++++++++++++++++++++++++++++/
    / \..<+++++\
      \ . +++++/

```
 as J.snusp

```txt

   load'snusp.ijs'  NB. the j code above
Toroidal programs run forever.  Use ^C to interrupt.
   
   main'J.snusp'
JJ
^C
|attention interrupt: main
   

```



## Java

See [[RCSNUSP/Java]]


## JavaScript

See [[RCSNUSP/JavaScript]].


## Julia

This Modular SNUSP interpreter uses modular calls to echo the first 2 characters entered. Try typing "Hi" at the prompt.

```julia
const echo2 =  raw"""
       /==!/======ECHO==,==.==#
       |   |
$==>==@/==@/==<==#"""

@enum Direction left up right down

function snusp(datalength, progstring)
    stack = Vector{Tuple{Int, Int, Direction}}()
    data = zeros(datalength)
    dp = ipx = ipy = 1
    direction = right    # default to go to right at beginning

    lines = split(progstring, "\n")
    lmax = maximum(map(length, lines))
    lines = map(x -> rpad(x, lmax), lines)
    for (y, li) in enumerate(lines)
        if (x = findfirst("\$", li)) != nothing
            (ipx, ipy) = (x[1], y)
        end
    end

    instruction = Dict([('>', ()-> dp += 1),
        ('<', ()-> (dp -= 1; if dp < 0 running = false end)), ('+', ()-> data[dp] += 1),
        ('-', ()-> data[dp] -= 1), (',', ()-> (data[dp] = read(stdin, UInt8))),
        ('.', ()->print(Char(data[dp]))),
        ('/', ()-> (d = Int(direction); d += (iseven(d) ? 3 : 5); direction = Direction(d % 4))),
        ('\\', ()-> (d = Int(direction); d += (iseven(d) ? 1 : -1); direction = Direction(d))),
        ('!', () -> ipnext()), ('?', ()-> if data[dp] == 0 ipnext() end),
        ('@', ()-> push!(stack, (ipx, ipy, direction))),
        ('#', ()-> if length(stack) > 0 (ipx, ipy, direction) = pop!(stack) end),
        ('\n', ()-> (running = false))])

    inboundsx(plus) = (plus ? (ipx < lmax) : (ipx > 1)) ? true : exit(data[dp])
    inboundsy(plus) = (plus ? (ipy < length(lines)) : (ipy > 1)) ? true : exit(data[dp])
    function ipnext()
        if direction == right && inboundsx(true)     ipx += 1
        elseif direction == left && inboundsx(false) ipx -= 1
        elseif direction == down && inboundsy(true)  ipy += 1
        elseif direction == up && inboundsy(false)   ipy -= 1
        end
    end

    running = true
    while running
        cmdcode = lines[ipy][ipx]
        if haskey(instruction, cmdcode)
            instruction[cmdcode]()
        end
        ipnext()
    end
    exit(data[dp])
end

snusp(100, echo2)
```
 {{output}} 
```txt

 > Hi
 Hi
 >

```



## Kotlin

{{trans|Go}}

```scala
// version 1.1.2

// requires 5 chars (10 bytes) of data store
const val hw = """
/++++!/
### =====
?\>++.>+.+++++++..+++\
\+++\ | /+>+++++++>/ /++++++++++<<.++>./
$+++/ | \+++++++++>\ \+++++.>.+++.-----\
      \==-<<<<+>+++/ /=.>.+>.--------.-/"""

// input is a multi-line string.
fun snusp(dlen: Int, raw: String) {
    val ds = CharArray(dlen)  // data store
    var dp = 0                // data pointer
    var s = raw

    // remove leading '\n' from string if present
    s = s.trimStart('\n')

    // make 2 dimensional instruction store and declare instruction pointers
    val cs = s.split('\n')
    var ipr = 0
    var ipc = 0

    // look for starting instruction
    findStart@  for ((r, row) in cs.withIndex()) {
        for ((i, c) in row.withIndex()) {
            if (c == '$') {
                ipr = r
                ipc = i
                break@findStart
            }
        }
    }

    var id = 0
    val step = fun() {
        if (id and 1 == 0)
            ipc += 1 - (id and 2)
        else
            ipr += 1 - (id and 2)
    }

    // execute
    while ((ipr in 0 until cs.size) && (ipc in 0 until cs[ipr].length)) {
        when (cs[ipr][ipc]) {
            '>'  -> dp++
            '<'  -> dp--
            '+'  -> ds[dp]++
            '-'  -> ds[dp]--
            '.'  -> print(ds[dp])
            ','  -> ds[dp] = readLine()!![0]
            '/'  -> id = id.inv()
            '\\' -> id = id xor 1
            '!'  -> step()
            '?'  -> if (ds[dp] == '\u0000') step()
        }
        step()
    }
}

fun main(args: Array<String>) {
    snusp(5, hw)
}
```


{{out}}

```txt

Hello World!

```



## Lua

See [[RCSNUSP/Lua]].


## Mathematica

See [[RCSNUSP/Mathematica]].


## OCaml

See [[RCSNUSP/OCaml]].


## Perl

See [[RCSNUSP/Perl]].


## Perl 6

{{works with|Rakudo|2017.02}}
Implementation of modular SNUSP.

```perl6
class SNUSP {

    has @!inst-pointer;
    has @!call-stack;
    has @!direction;
    has @!memory;
    has $!mem-pointer;

    method run ($code) {
        init();
        my @code = pad( |$code.lines );
        for @code.kv -> $r, @l {
           my $index = @l.grep( /'$'/, :k );
           if $index {
               @!inst-pointer = $r, $index;
               last
           }
        }

        loop {
            my $instruction = @code[@!inst-pointer[0]; @!inst-pointer[1]];
            given $instruction {
                when '>'  { $!mem-pointer++ }
                when '<'  { $!mem-pointer-- }
                when '+'  { @!memory[$!mem-pointer]++ }
                when '-'  { @!memory[$!mem-pointer]-- }
                when '.'  { print @!memory[$!mem-pointer].chr }
                when ','  { @!memory[$!mem-pointer] = $*IN.getc.ord }
                when '/'  { @!direction = @!direction.reverse «*» -1 }
                when '\\' { @!direction = @!direction.reverse }
                when '!'  { nexti() }
                when '?'  { nexti() unless @!memory[$!mem-pointer] }
                when '@'  { @!call-stack.push: @!inst-pointer.Array }
                when '#'  {
                    last unless +@!call-stack;
                    @!inst-pointer = |@!call-stack.pop;
                    nexti();
                }
            }
            nexti();
            last if @!inst-pointer[0] > +@code or
                    @!inst-pointer[1] > +@code[0];
        }

        sub init () {
            @!inst-pointer = 0, 0;
            @!direction    = 0, 1;
            $!mem-pointer  = 0;
            @!memory       = ()
        }

        sub nexti () { @!inst-pointer Z+= @!direction }

        sub pad ( *@lines ) {
            my $max = max @lines».chars;
            my @pad = @lines.map: $max - *.chars;
            map -> $i { flat @lines[$i].comb, ' ' xx @pad[$i] }, ^@lines;
        }
    }
}

# TESTING
my $hw = q:to/END/;
    /++++!/
### =====
?\>++.>+.+++++++..+++\
    \+++\ | /+>+++++++>/ /++++++++++<<.++>./
    $+++/ | \+++++++++>\ \+++++.>.+++.-----\
          \==-<<<<+>+++/ /=.>.+>.--------.-/
    END

my $snusp = SNUSP.new;
$snusp.run($hw)
```

{{out}}

```txt
Hello World!
```



## Phix

{{trans|Go}}
```Phix
integer id = 0, ipr = 1, ipc = 1

procedure step()
    if and_bits(id,1) == 0 then
        ipc += 1 - and_bits(id,2)
    else 
        ipr += 1 - and_bits(id,2)
    end if
end procedure

procedure snusp(integer dlen, string s)
sequence ds = repeat(0,dlen)  -- data store
integer dp = 1                -- data pointer
 
    -- remove leading '\n' from string if present
    s = trim_head(s,'\n')
 
    -- make 2 dimensional instruction store and set instruction pointers
    sequence cs = split(s,'\n')
    ipr = 1
    ipc = 1
 
    -- look for starting instruction
    for i=1 to length(cs) do
        ipc = find('$',cs[i])
        if ipc then
            ipr = i
            exit
        end if
    end for

    id = 0
 
    -- execute
    while ipr>=1 and ipr<=length(cs)
      and ipc>=1 and ipc<=length(cs[ipr]) do
        integer op = cs[ipr][ipc]
        switch op do
            case '>' : dp += 1
            case '<' : dp -= 1
            case '+' : ds[dp] += 1
            case '-' : ds[dp] -= 1
            case '.' : puts(1,ds[dp])
            case ',' : ds[dp] = getc(0)
            case '/' : id = not_bits(id)
            case '\\': id = xor_bits(id,1)
            case '!' : step()
            case '?' : if ds[dp]=0 then step() end if
        end switch
        step()
    end while
end procedure
 
constant hw = """
/++++!/
### =====
?\>++.>+.+++++++..+++\
\+++\ | /+>+++++++>/ /++++++++++<<.++>./
$+++/ | \+++++++++>\ \+++++.>.+++.-----\
      \==-<<<<+>+++/ /=.>.+>.--------.-/"""

snusp(5, hw)
```

{{out}}

```txt

Hello World!

```



## PicoLisp

See [[RCSNUSP/PicoLisp]].


## Python

{{trans|Go}}

```python
#!/usr/bin/env python3

HW = r'''
/++++!/
### =====
?\>++.>+.+++++++..+++\
\+++\ | /+>+++++++>/ /++++++++++<<.++>./
$+++/ | \+++++++++>\ \+++++.>.+++.-----\
      \==-<<<<+>+++/ /=.>.+>.--------.-/'''

def snusp(store, code):
    ds = bytearray(store)  # data store
    dp = 0                 # data pointer
    cs = code.splitlines() # 2 dimensional code store
    ipr, ipc = 0, 0        # instruction pointers in row and column
    for r, row in enumerate(cs):
        try:
            ipc = row.index('$')
            ipr = r
            break
        except ValueError:
            pass
    rt, dn, lt, up = range(4)
    id = rt  # instruction direction.  starting direction is always rt
    def step():
        nonlocal ipr, ipc
        if id&1:
            ipr += 1 - (id&2)
        else:
            ipc += 1 - (id&2)
    while ipr >= 0 and ipr < len(cs) and ipc >= 0 and ipc < len(cs[ipr]):
        op = cs[ipr][ipc]
        if op == '>':
            dp += 1
        elif op == '<':
            dp -= 1
        elif op == '+':
            ds[dp] += 1
        elif op == '-':
            ds[dp] -= 1
        elif op == '.':
            print(chr(ds[dp]), end='')
        elif op == ',':
            ds[dp] = input()
        elif op == '/':
            id = ~id
        elif op == '\\':
            id ^= 1
        elif op == '!':
            step()
        elif op == '?':
            if not ds[dp]:
                step()
        step()

if __name__ == '__main__':
    snusp(5, HW)
```

{{out}}

```txt

Hello World!

```



## Racket

See [[RCSNUSP/Racket]].


## Ruby

See [[RCSNUSP/Ruby]].


## Seed7

The interpreter below implements Core SNUSP:


```seed7
$ include "seed7_05.s7i";

const proc: snusp (in string: sourceCode, in integer: memSize, inout file: input, inout file: output) is func
  local
    var array string: instructions is 0 times "";
    var array char: memory is 0 times ' ';
    var integer: dataPointer is 1;
    var integer: instrPtrRow is 0;
    var integer: instrPtrColumn is 0;
    var integer: rowDir is 0;
    var integer: columnDir is 1;
    var integer: helpDir is 0;
    var integer: row is 0;
  begin
    instructions := split(sourceCode, "\n");
    memory := memSize times '\0;';

    for key row range instructions do
      if pos(instructions[row], '$') <> 0 then
        instrPtrRow := row;
        instrPtrColumn := pos(instructions[row], '$');
      end if;
    end for;

    while instrPtrRow >= 1 and instrPtrRow <= length(instructions) and
        instrPtrColumn >= 1 and instrPtrColumn <= length(instructions[instrPtrRow]) do
      case instructions[instrPtrRow][instrPtrColumn] of
        when {'>'}:  incr(dataPointer);
        when {'<'}:  decr(dataPointer);
        when {'+'}:  incr(memory[dataPointer]);
        when {'-'}:  decr(memory[dataPointer]);
        when {'.'}:  write(output, memory[dataPointer]);
        when {','}:  memory[dataPointer] := getc(input);
        when {'/'}:  helpDir := rowDir;
                     rowDir := -columnDir;
                     columnDir := -helpDir;
        when {'\\'}: helpDir := rowDir;
                     rowDir := columnDir;
                     columnDir := helpDir;
        when {'!'}:  instrPtrRow +:= rowDir;
                     instrPtrColumn +:= columnDir;
        when {'?'}:  if memory[dataPointer] = '\0;' then
                       instrPtrRow +:= rowDir;
                       instrPtrColumn +:= columnDir;
                     end if;
      end case;
      instrPtrRow +:= rowDir;
      instrPtrColumn +:= columnDir;
    end while;
  end func;

# SNUSP implementation of Hello World.
const string: helloWorld is "/++++!/
### =====
?\\>++.>+.+++++++..+++\\\n\
                            \\\+++\\ | /+>+++++++>/ /++++++++++<<.++>./\n\
                            \$+++/ | \\+++++++++>\\ \\+++++.>.+++.-----\\\n\
                            \      \\==-<<<<+>+++/ /=.>.+>.--------.-/";

const proc: main is func
  begin
    snusp(helloWorld, 5, IN, OUT);
  end func;
```


{{out}}

```txt

Hello World!

```



## Tcl

See [[RCSNUSP/Tcl]].

{{omit from|GUISS}}
