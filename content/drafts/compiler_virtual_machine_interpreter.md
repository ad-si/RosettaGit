+++
title = "Compiler/virtual machine interpreter"
description = ""
date = 2019-04-16T14:32:26Z
aliases = []
[extra]
id = 21170
[taxonomies]
categories = []
tags = []
+++

{{task}}Virtual Machine Interpreter

A virtual machine implements a computer in software.

{{task heading}}

Write a virtual machine interpreter.  This interpreter should be able to run virtual
assembly language programs created via the [[Compiler/code_generator|task]]. This is a
byte-coded, 32-bit word stack based virtual machine.

The program should read input from a file and/or stdin, and write output to a file and/or
stdout.

Input format:

Given the following program:

 count = 1;
 while (count < 10) {
     print("count is: ", count, "\n");
     count = count + 1;
 }

The output from the [[Compiler/code_generator|Code generator]] is a virtual assembly code program:

{| class="wikitable"
|-
! Output from gen, input to VM
|-

| style="vertical-align:top" |
<b>
```txt
Datasize: 1 Strings: 2
"count is: "
"\n"
    0 push  1
    5 store [0]
   10 fetch [0]
   15 push  10
   20 lt
   21 jz     (43) 65
   26 push  0
   31 prts
   32 fetch [0]
   37 prti
   38 push  1
   43 prts
   44 fetch [0]
   49 push  1
   54 add
   55 store [0]
   60 jmp    (-51) 10
   65 halt
```
</b>
|}

The first line of the input specifies the datasize required and the number of constant
strings, in the order that they are reference via the code.

The data can be stored in a separate array, or the data can be stored at the beginning of
the stack.  Data is addressed starting at 0.  If there are 3 variables, the 3rd one if
referenced at address 2.

If there are one or more constant strings, they come next.  The code refers to these
strings by their index.  The index starts at 0.  So if there are 3 strings, and the code
wants to reference the 3rd string, 2 will be used.

Next comes the actual virtual assembly code.  The first number is the code address of that
instruction.  After that is the instruction mnemonic, followed by optional operands,
depending on the instruction.

Registers:

sp:
    the stack pointer - points to the next top of stack.  The stack is a 32-bit integer
    array.

pc:
    the program counter - points to the current instruction to be performed.  The code is an
    array of bytes.

Data:
    data
    string pool

Instructions:

Each instruction is one byte.  The following instructions also have a 32-bit integer
operand:

 fetch [index]

where index is an index into the data array.

 store [index]

where index is an index into the data array.

 push n

where value is a 32-bit integer that will be pushed onto the stack.

 jmp (n) addr

where (n) is a 32-bit integer specifying the distance between the current location and the
desired location.  addr is an unsigned value of the actual code address.

 jz (n) addr

where (n) is a 32-bit integer specifying the distance between the current location and the
desired location.  addr is an unsigned value of the actual code address.

The following instructions do not have an operand.  They perform their operation directly
against the stack:

For the following instructions, the operation is performed against the top two entries in
the stack:

 add
 sub
 mul
 div
 mod
 lt
 gt
 le
 ge
 eq
 ne
 and
 or

For the following instructions, the operation is performed against the top entry in the
stack:

 neg
 not

Print the word at stack top as a character.

 prtc

Print the word at stack top as an integer.

 prti

Stack top points to an index into the string pool.  Print that entry.

 prts

Unconditional stop.

 halt

; A simple example virtual machine:


```python
def run_vm(data_size)
    int stack[data_size + 1000]
    set stack[0..data_size - 1] to 0
    int pc = 0
    while True:
        op = code[pc]
        pc += 1

        if op == FETCH:
            stack.append(stack[bytes_to_int(code[pc:pc+word_size])[0]]);
            pc += word_size
        elif op == STORE:
            stack[bytes_to_int(code[pc:pc+word_size])[0]] = stack.pop();
            pc += word_size
        elif op == PUSH:
            stack.append(bytes_to_int(code[pc:pc+word_size])[0]);
            pc += word_size
        elif op == ADD:   stack[-2] += stack[-1]; stack.pop()
        elif op == SUB:   stack[-2] -= stack[-1]; stack.pop()
        elif op == MUL:   stack[-2] *= stack[-1]; stack.pop()
        elif op == DIV:   stack[-2] /= stack[-1]; stack.pop()
        elif op == MOD:   stack[-2] %= stack[-1]; stack.pop()
        elif op == LT:    stack[-2] = stack[-2] <  stack[-1]; stack.pop()
        elif op == GT:    stack[-2] = stack[-2] >  stack[-1]; stack.pop()
        elif op == LE:    stack[-2] = stack[-2] <= stack[-1]; stack.pop()
        elif op == GE:    stack[-2] = stack[-2] >= stack[-1]; stack.pop()
        elif op == EQ:    stack[-2] = stack[-2] == stack[-1]; stack.pop()
        elif op == NE:    stack[-2] = stack[-2] != stack[-1]; stack.pop()
        elif op == AND:   stack[-2] = stack[-2] and stack[-1]; stack.pop()
        elif op == OR:    stack[-2] = stack[-2] or  stack[-1]; stack.pop()
        elif op == NEG:   stack[-1] = -stack[-1]
        elif op == NOT:   stack[-1] = not stack[-1]
        elif op == JMP:   pc += bytes_to_int(code[pc:pc+word_size])[0]
        elif op == JZ:    if stack.pop() then pc += word_size else pc += bytes_to_int(code[pc:pc+word_size])[0]
        elif op == PRTC:  print stack[-1] as a character; stack.pop()
        elif op == PRTS:  print the constant string referred to by stack[-1]; stack.pop()
        elif op == PRTI:  print stack[-1] as an integer; stack.pop()
        elif op == HALT:  break
```


; Additional examples

Your solution should pass all the test cases above and the additional tests found '''[[Compiler/Sample_programs|Here]]'''.

{{task heading|Reference}}

The C and Python versions can be considered reference implementations.

;Related Tasks

* [[Compiler/lexical_analyzer|Lexical Analyzer task]]
* [[Compiler/syntax_analyzer|Syntax Analyzer task]]
* [[Compiler/code_generator|Code Generator task]]
* [[Compiler/AST_interpreter|AST Interpreter task]]

<hr>
__TOC__


## Aime

<lang>integer n, pc, sp;
file f;
text s;
index code, Data;
list l, stack, strings;

f.affix(argv(1));

f.list(l, 0);

n = atoi(l[-1]);
while (n) {
    f.lead(s);
    strings.append(erase(s, -1, 0));
    n -= 1;
}

while (f.list(l, 0) ^ -1) {
    code.put(atoi(lf_x_text(l)), l);
}

pc = sp = 0;
while (1) {
    l = code[pc];
    s = l[0];
    if (s == "jz") {
        if (lb_pick(stack)) {
            isk_greater(code, pc, pc);
        } else {
            pc = atoi(l[-1]);
        }
    } elif (s == "jmp") {
        pc = atoi(l[-1]);
    } else {
        if (s == "push") {
            lb_push(stack, atoi(l[1]));
        } elif (s == "fetch") {
            lb_push(stack, Data[atoi(erase(l[1], -1, 0))]);
        } elif (s == "neg") {
            stack[-1] = -stack[-1];
        } elif (s == "not") {
            stack[-1] = !stack[-1];
        } elif (s == "halt") {
            break;
        } else {
            n = lb_pick(stack);
            if (s == "store") {
                Data[atoi(erase(l[1], -1, 0))] = n;
            } elif (s == "add") {
                stack[-1] = stack[-1] + n;
            } elif (s == "sub") {
                stack[-1] = stack[-1] - n;
            } elif (s == "mul") {
                stack[-1] = stack[-1] * n;
            } elif (s == "div") {
                stack[-1] = stack[-1] / n;
            } elif (s == "mod") {
                stack[-1] = stack[-1] % n;
            } elif (s == "lt") {
                stack[-1] = stack[-1] < n;
            } elif (s == "gt") {
                stack[-1] = stack[-1] > n;
            } elif (s == "le") {
                stack[-1] = stack[-1] <= n;
            } elif (s == "ge") {
                stack[-1] = stack[-1] >= n;
            } elif (s == "eq") {
                stack[-1] = stack[-1] == n;
            } elif (s == "ne") {
                stack[-1] = stack[-1] != n;
            } elif (s == "and") {
                stack[-1] = stack[-1] && n;
            } elif (s == "or") {
                stack[-1] = stack[-1] || n;
            } elif (s == "prtc") {
                o_byte(n);
            } elif (s == "prti") {
                o_(n);
            } elif (s == "prts") {
                o_(strings[n]);
            } else {
            }
        }

        isk_greater(code, pc, pc);
    }
}
```



## ALGOL W


```algolw
begin % virtual machine interpreter %
    % string literals %
    string(256) array stringValue  ( 0 :: 256 );
    integer     array stringLength ( 0 :: 256 );
    integer     MAX_STRINGS;
    % op codes %
    integer     oFetch, oStore, oPush
          ,     oAdd,   oSub,   oMul, oDiv, oMod, oLt, oGt,   oLe,   oGe,   oEq,   oNe
          ,     oAnd,   oOr,    oNeg, oNot, oJmp, oJz, oPrtc, oPrts, oPrti, oHalt
          ;
    string(6)   array opName       ( 1 :: 24 );
    integer     OP_MAX;
    % code %
    string(1)   array byteCode     ( 0 :: 4096 );
    integer     nextLocation, MAX_LOCATION;
    % data %
    integer     array data         ( 0 :: 4096 );
    integer     dataSize, MAX_DATA, MAX_STACK;
    % tracing %
    logical     trace;

    % reports an error and stops %
    procedure rtError( string(80) value message ); begin
        integer errorPos;
        write( s_w := 0, "**** Runtime error: " );
        errorPos := 0;
        while errorPos < 80 and message( errorPos // 1 ) not = "." do begin
            writeon( s_w := 0, message( errorPos // 1 ) );
            errorPos := errorPos + 1
        end while_not_at_end_of_message ;
        writeon( s_w := 0, "." );
        assert( false )
    end genError ;

    oFetch :=  1; opName( oFetch ) := "fetch"; oStore :=  2; opName( oStore ) := "store"; oPush :=  3; opName( oPush ) := "push";
    oAdd   :=  4; opName( oAdd   ) := "add";   oSub   :=  5; opName( oSub   ) := "sub";   oMul  :=  6; opName( oMul  ) := "mul";
    oDiv   :=  7; opName( oDiv   ) := "div";   oMod   :=  8; opName( oMod   ) := "mod";   oLt   :=  9; opName( oLt   ) := "lt";
    oGt    := 10; opName( oGt    ) := "gt";    oLe    := 11; opName( oLe    ) := "le";    oGe   := 12; opName( oGe   ) := "ge";
    oEq    := 13; opName( oEq    ) := "eq";    oNe    := 14; opName( oNe    ) := "ne";    oAnd  := 15; opName( oAnd  ) := "and";
    oOr    := 16; opName( oOr    ) := "or";    oNeg   := 17; opName( oNeg   ) := "neg";   oNot  := 18; opName( oNot  ) := "not";
    oJmp   := 19; opName( oJmp   ) := "jmp";   oJz    := 20; opName( oJz    ) := "jz";    oPrtc := 21; opName( oPrtc ) := "prtc";
    oPrts  := 22; opName( oPrts  ) := "prts";  oPrti  := 23; opName( oPrti  ) := "prti";  oHalt := 24; opName( oHalt ) := "halt";
    OP_MAX := oHalt;

    trace        := false;
    MAX_STACK    := 256;
    MAX_LOCATION := 4096;
    for pc := 0 until MAX_LOCATION do byteCode( pc ) := code( 0 );
    MAX_DATA := 4096;
    for dPos := 0 until MAX_DATA do data( dPos ) := 0;
    MAX_STRINGS := 256;
    for sPos := 0 until MAX_STRINGS do begin
        stringValue(  sPos ) := " ";
        stringLength( sPos ) := 0
    end for_sPos ;

    % load thge output from syntaxc analyser %
    begin % readCode %

        % skips spaces on the source line %
        procedure skipSpaces ; begin
            while line( lPos // 1 ) = " " do lPos := lPos + 1
        end skipSpaces ;

        % parses a string from line and stores it in the string literals table %
        procedure readString ( integer value stringNumber ) ; begin
            string(256) str;
            integer     sLen;
            str  := " ";
            sLen := 0;
            lPos := lPos + 1; % skip the opening double-quote %
            while lPos <= 255 and line( lPos // 1 ) not = """" do begin
                str( sLen // 1 ) := line( lPos // 1 );
                sLen := sLen + 1;
                lPos := lPos + 1
            end while_more_string ;
            if lPos > 255 then rtError( "Unterminated String." );
            % store the string %
            stringValue(  stringNumber ) := str;
            stringLength( stringNumber ) := sLen
        end readString ;

        % gets an integer from the line - checks for valid digits %
        integer procedure readInteger ; begin
            integer n;
            skipSpaces;
            n := 0;
            while line( lPos // 1 ) >= "0" and line( lPos // 1 ) <= "9" do begin
                n    := ( n * 10 ) + ( decode( line( lPos // 1 ) ) - decode( "0" ) );
                lPos := lPos + 1
            end while_not_end_of_integer ;
            n
        end readInteger ;

        % reads the next line from standard input %
        procedure readALine ; begin
            lPos := 0;
            readcard( line );
            if trace then write( s_w := 0, ">> ", line( 0 // 32 ) )
        end readALine ;

        % loads an instruction from the current source line %
        procedure loadCodeFromLine ; begin
            integer pc, opCode, operand, oPos;
            string(256) op;
            logical haveOperand;
            % get the code location %
            pc := readInteger;
            if pc > MAX_LOCATION then rtError( "Code too large." );
            % get the opCode %
            skipSpaces;
            oPos := 0;
            op := " ";
            while lPos <= 255 and line( lPos // 1 ) not = " " do begin
                op( oPos // 1 ) := line( lPos // 1 );
                oPos := oPos + 1;
                lPos := lPos + 1
            end while_more_opName ;
            % lookup the op code %
            opCode := 0;
            oPos   := 1;
            while oPos <= OP_MAX and opCode = 0 do begin
                if opName( oPos ) = op then opCode := oPos
                                       else oPos   := oPos + 1
            end while_op_not_found ;
            if opCode = 0 then rtError( "Unknown op code." );
            % get the operand if there is one %
            operand     := 0;
            haveOperand := false;
            if      opCode = oFetch or opCode = oStore then begin
                % fetch or store - operand is enclosed in square brackets %
                skipSpaces;
                if line( lPos // 1 ) not = "[" then rtError( """["" expected after fetch/store." );
                lPos        := lPos + 1;
                operand     := readInteger;
                if operand > dataSize then rtError( "fetch/store address out of range." );
                haveOperand := true
                end
            else if opCode = oPush then begin
                % push integer literal instruction %
                operand     := readInteger;
                haveOperand := true
                end
            else if opCode = oJmp or opCode = oJz then begin
                % jump - the operand is the relative address enclosed in parenthesis %
                % followed by the absolute address - we use the absolute address so  %
                % the opewrand will be >= 0 %
                skipSpaces;
                if line( lPos // 1 ) not = "(" then rtError( """("" expected after jmp/jz." );
                lPos        := lPos + 1;
                if line( lPos // 1 ) = "-" then % negative relative address % lPos := lPos + 1;
                operand     := readInteger;
                if line( lPos // 1 ) not = ")" then rtError( """)"" expected after jmp/jz." );
                lPos        := lPos + 1;
                operand     := readInteger;
                haveOperand := true
            end if_various_opcodes ;
            % store the code %
            byteCode( pc ) := code( opCode );
            if haveOperand then begin
                % have an operand for the op code %
                if ( pc + 4 ) > MAX_LOCATION then rtError( "Code too large." );
                for oPos := 1 until 4 do begin
                    pc := pc + 1;
                    byteCode( pc ) := code( operand rem 256 );
                    operand := operand div 256;
                end for_oPos
            end if_have_operand ;
        end loadCodeFromLine ;

        string(256) line;
        string(16)  name;
        integer     lPos, tPos, stringCount;

        % allow us to detect EOF %
        ENDFILE := EXCEPTION( false, 1, 0, false, "EOF" );

        % first line should be "Datasize: d Strings: s" where d = number variables %
        % and s = number of strings                                                %
        readALine;
        if line = "trace" then begin
            % extension - run in trace mode %
            trace := true;
            readALine
        end if_line_eq_trace ;
        if XCPNOTED(ENDFILE) then rtError( "Empty program file." );
        if line( 0 // 10 ) not = "Datasize: " then rtError( "Header line missing." );
        lPos := 10;
        dataSize := readInteger;
        if dataSize > MAX_DATA then rtError( "Datasize too large." );
        skipSpaces;
        if line( lPos // 9 ) not = "Strings: " then rtError( """Strings: "" missing on header line." );
        lPos := lPos + 9;
        stringCount := readInteger;
        if stringCount > MAX_STRINGS then rtError( "Too many strings." );
        % read the string table %
        for stringNumber := 0 until stringCount - 1 do begin
            string(256) str;
            integer     sLen, sPos;
            readALine;
            if XCPNOTED(ENDFILE) then rtError( "End-of-file in string table." );
            if line( lPos // 1 ) not = """" then rtError( "String literal expected." );
            str  := " ";
            sLen := 0;
            lPos := lPos + 1; % skip the opening double-quote %
            while lPos <= 255 and line( lPos // 1 ) not = """" do begin
                str( sLen // 1 ) := line( lPos // 1 );
                sLen := sLen + 1;
                lPos := lPos + 1
            end while_more_string ;
            if lPos > 255 then rtError( "Unterminated String." );
            % store the string %
            stringValue(  stringNumber ) := str;
            stringLength( stringNumber ) := sLen
        end for_sPos ;
        % read the code %
        readALine;
        while not XCPNOTED(ENDFILE) do begin
            if line not = " " then loadCodeFromLine;
            readALine
        end while_not_eof
    end;
    % run the program %
    begin
        integer pc, opCode, operand, sp;
        integer array st ( 0 :: MAX_STACK );
        logical halted;
        % prints a string from the string pool, escape sequences are interpreted %
        procedure writeOnString( integer value stringNumber ) ;
        begin
            integer     cPos, sLen;
            string(256) text;
            if stringNumber < 0 or stringNumber > MAX_STRINGS then rtError( "Invalid string number." );
            cPos := 0;
            sLen := stringLength( stringNumber );
            text := stringValue(  stringNumber );
            while cPos < stringLength( stringNumber ) do begin
                string(1) ch;
                ch := text( cPos // 1 );
                if ch not = "\" then writeon( s_w := 0, ch )
                else begin
                    % escaped character %
                    cPos := cPos + 1;
                    if cPos > sLen then rtError( "String terminates with ""\""." );
                    ch := text( cPos // 1 );
                    if ch = "n" then % newline % write()
                                else writeon( s_w := 0, ch )
                end;
                cPos := cPos + 1
            end while_not_end_of_string
        end writeOnString ;

        pc     := 0;
        sp     := -1;
        halted := false;
        while not halted do begin;
            % get the next op code and operand %
            opCode  := decode( byteCode( pc ) );
            pc      := pc + 1;
            operand := 0;
            if opCode = oFetch or opCode = oStore or opCode = oPush or opCode = oJmp or opCode = oJz then begin
                % this opCode has an operand %
                pc := pc + 4;
                for bPos := 1 until 4 do begin
                    operand := ( operand * 256 ) + decode( byteCode( pc - bPos ) );
                end for_bPos
            end if_opCode_with_an_operand ;
            if trace then begin
                write( i_w:= 1, s_w := 0, pc, " op(", opCode, "): ", opName( opCode ), " ", operand );
                write()
            end if_trace ;
            % interpret the instruction %
            if      opCode = oFetch then begin sp := sp + 1; st( sp ) := data( operand ) end
            else if opCode = oStore then begin data( operand ) := st( sp ); sp := sp - 1 end
            else if opCode = oPush  then begin sp := sp + 1; st( sp ) := operand         end
            else if opCode = oHalt  then halted := true
            else if opCode = oJmp   then pc     := operand
            else if oPCode = oJz    then begin
                if st( sp ) = 0 then pc := operand;
                sp := sp - 1
                end
            else if opCode = oPrtc  then begin writeon( i_w := 1, s_w := 0, code( st( sp ) ) ); sp := sp - 1 end
            else if opCode = oPrti  then begin writeon( i_w := 1, s_w := 0,       st( sp )   ); sp := sp - 1 end
            else if opCode = oPrts  then begin writeonString(                     st( sp )   ); sp := sp - 1 end
            else if opCode = oNeg   then st( sp ) := - st( sp )
            else if opCode = oNot   then st( sp ) := ( if st( sp ) = 0 then 1 else 0 )
            else begin
                operand := st( sp );
                sp      := sp - 1;
                if      opCode = oAdd   then st( sp ) :=    st( sp )    +  operand
                else if opCode = oSub   then st( sp ) :=    st( sp )    -  operand
                else if opCode = oMul   then st( sp ) :=    st( sp )    *  operand
                else if opCode = oDiv   then st( sp ) :=    st( sp )  div  operand
                else if opCode = oMod   then st( sp ) :=    st( sp )  rem  operand
                else if opCode = oLt    then st( sp ) := if st( sp )    <  operand then 1 else 0
                else if opCode = oGt    then st( sp ) := if st( sp )    >  operand then 1 else 0
                else if opCode = oLe    then st( sp ) := if st( sp )    <= operand then 1 else 0
                else if opCode = oGe    then st( sp ) := if st( sp )    >= operand then 1 else 0
                else if opCode = oEq    then st( sp ) := if st( sp )     = operand then 1 else 0
                else if opCode = oNe    then st( sp ) := if st( sp ) not = operand then 1 else 0
                else if opCode = oAnd   then st( sp ) := if st( sp ) not = 0 and operand not = 0 then 1 else 0
                else if opCode = oOr    then st( sp ) := if st( sp ) not = 0 or  operand not = 0 then 1 else 0
                else                         rtError( "Unknown opCode." )
            end if_various_opCodes
        end while_not_halted
    end
end.
```



## AWK

Tested with gawk 4.1.1 and mawk 1.3.4.

```AWK

function error(msg) {
  printf("%s\n", msg)
  exit(1)
}

function bytes_to_int(bstr,          i, sum) {
  sum = 0
  for (i=word_size-1; i>=0; i--) {
    sum *= 256
    sum += code[bstr+i]
  }
  return sum
}

function emit_byte(x) {
  code[next_free_code_index++] = x
}

function emit_word(x,       i) {
  for (i=0; i<word_size; i++) {
    emit_byte(int(x)%256);
    x = int(x/256)
  }
}

function run_vm(data_size) {
  sp = data_size + 1
  pc = 0
  while (1) {
    op = code[pc++]
    if (op == FETCH) {
      stack[sp++] = stack[bytes_to_int(pc)]
      pc += word_size
    } else if (op == STORE) {
      stack[bytes_to_int(pc)] = stack[--sp]
      pc += word_size
    } else if (op == PUSH) {
      stack[sp++] = bytes_to_int(pc)
      pc += word_size
    } else if (op == ADD ) { stack[sp-2] += stack[sp-1]; sp--
    } else if (op == SUB ) { stack[sp-2] -= stack[sp-1]; sp--
    } else if (op == MUL ) { stack[sp-2] *= stack[sp-1]; sp--
    } else if (op == DIV ) { stack[sp-2]  = int(stack[sp-2] / stack[sp-1]); sp--
    } else if (op == MOD ) { stack[sp-2] %= stack[sp-1]; sp--
    } else if (op == LT  ) { stack[sp-2] = stack[sp-2] <  stack[sp-1]; sp--
    } else if (op == GT  ) { stack[sp-2] = stack[sp-2] >  stack[sp-1]; sp--
    } else if (op == LE  ) { stack[sp-2] = stack[sp-2] <= stack[sp-1]; sp--
    } else if (op == GE  ) { stack[sp-2] = stack[sp-2] >= stack[sp-1]; sp--
    } else if (op == EQ  ) { stack[sp-2] = stack[sp-2] == stack[sp-1]; sp--
    } else if (op == NE  ) { stack[sp-2] = stack[sp-2] != stack[sp-1]; sp--
    } else if (op == AND ) { stack[sp-2] = stack[sp-2] && stack[sp-1]; sp--
    } else if (op == OR  ) { stack[sp-2] = stack[sp-2] || stack[sp-1]; sp--
    } else if (op == NEG ) { stack[sp-1] = - stack[sp-1]
    } else if (op == NOT ) { stack[sp-1] = ! stack[sp-1]
    } else if (op == JMP ) { pc += bytes_to_int(pc)
    } else if (op == JZ  ) { if (stack[--sp]) { pc += word_size } else { pc += bytes_to_int(pc) }
    } else if (op == PRTC) { printf("%c", stack[--sp])
    } else if (op == PRTS) { printf("%s", string_pool[stack[--sp]])
    } else if (op == PRTI) { printf("%d", stack[--sp])
    } else if (op == HALT) { break
    }
  } # while
}

function str_trans(srce,           dest, i) {
  dest = ""
  for (i=1; i <= length(srce); ) {
    if (substr(srce, i, 1) == "\\" && i < length(srce)) {
      if (substr(srce, i+1, 1) == "n") {
        dest = dest "\n"
        i += 2
      } else if (substr(srce, i+1, 1) == "\\") {
        dest = dest "\\"
        i += 2
      }
    } else {
      dest = dest substr(srce, i, 1)
      i += 1
    }
  }
  return dest
}

function load_code(            n, i) {
  getline line
  if (line ==  "")
    error("empty line")
  n=split(line, line_list)
  data_size = line_list[2]
  n_strings = line_list[4]
  for (i=0; i<n_strings; i++) {
    getline line
    gsub(/\n/, "", line)
    gsub(/"/ , "", line)
    string_pool[i] = str_trans(line)
  }
  while (getline) {
    offset = int($1)
    instr  = $2
    opcode = code_map[instr]
    if (opcode == "")
      error("Unknown instruction " instr " at " offset)
    emit_byte(opcode)
    if (opcode == JMP || opcode == JZ) {
      p = int($4)
      emit_word(p - (offset + 1))
    } else if (opcode == PUSH) {
      value = int($3)
      emit_word(value)
    } else if (opcode == FETCH || opcode == STORE) {
      gsub(/\[/, "", $3)
      gsub(/\]/, "", $3)
      value = int($3)
      emit_word(value)
    }
  }
  return data_size
}

BEGIN {
  code_map["fetch"] = FETCH =  1
  code_map["store"] = STORE =  2
  code_map["push" ] = PUSH  =  3
  code_map["add"  ] = ADD   =  4
  code_map["sub"  ] = SUB   =  5
  code_map["mul"  ] = MUL   =  6
  code_map["div"  ] = DIV   =  7
  code_map["mod"  ] = MOD   =  8
  code_map["lt"   ] = LT    =  9
  code_map["gt"   ] = GT    = 10
  code_map["le"   ] = LE    = 11
  code_map["ge"   ] = GE    = 12
  code_map["eq"   ] = EQ    = 13
  code_map["ne"   ] = NE    = 14
  code_map["and"  ] = AND   = 15
  code_map["or"   ] = OR    = 16
  code_map["neg"  ] = NEG   = 17
  code_map["not"  ] = NOT   = 18
  code_map["jmp"  ] = JMP   = 19
  code_map["jz"   ] = JZ    = 20
  code_map["prtc" ] = PRTC  = 21
  code_map["prts" ] = PRTS  = 22
  code_map["prti" ] = PRTI  = 23
  code_map["halt" ] = HALT  = 24

  next_free_node_index = 1
  next_free_code_index = 0
  word_size   = 4
  input_file = "-"
  if (ARGC > 1)
    input_file = ARGV[1]
  data_size = load_code()
  run_vm(data_size)
}

```

{{out|case=count}}
<b>

```txt

count is: 1
count is: 2
count is: 3
count is: 4
count is: 5
count is: 6
count is: 7
count is: 8
count is: 9

```

</b>


## C

Tested with gcc 4.81 and later, compiles warning free with -Wall -Wextra

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>

#define NELEMS(arr) (sizeof(arr) / sizeof(arr[0]))

#define da_dim(name, type)  type *name = NULL;          \
                            int _qy_ ## name ## _p = 0;  \
                            int _qy_ ## name ## _max = 0

#define da_redim(name)      do {if (_qy_ ## name ## _p >= _qy_ ## name ## _max) \
                                name = realloc(name, (_qy_ ## name ## _max += 32) * sizeof(name[0]));} while (0)

#define da_rewind(name)     _qy_ ## name ## _p = 0

#define da_append(name, x)  do {da_redim(name); name[_qy_ ## name ## _p++] = x;} while (0)

typedef unsigned char uchar;
typedef uchar code;

typedef enum { FETCH, STORE, PUSH, ADD, SUB, MUL, DIV, MOD, LT, GT, LE, GE, EQ, NE, AND,
    OR, NEG, NOT, JMP, JZ, PRTC, PRTS, PRTI, HALT
} Code_t;

typedef struct Code_map {
    char    *text;
    Code_t   op;
} Code_map;

Code_map code_map[] = {
    {"fetch",  FETCH},
    {"store",  STORE},
    {"push",   PUSH },
    {"add",    ADD  },
    {"sub",    SUB  },
    {"mul",    MUL  },
    {"div",    DIV  },
    {"mod",    MOD  },
    {"lt",     LT   },
    {"gt",     GT   },
    {"le",     LE   },
    {"ge",     GE   },
    {"eq",     EQ   },
    {"ne",     NE   },
    {"and",    AND  },
    {"or",     OR   },
    {"neg",    NEG  },
    {"not",    NOT  },
    {"jmp",    JMP  },
    {"jz",     JZ   },
    {"prtc",   PRTC },
    {"prts",   PRTS },
    {"prti",   PRTI },
    {"halt",   HALT },
};

FILE *source_fp;
da_dim(object, code);

void error(const char *fmt, ... ) {
    va_list ap;
    char buf[1000];

    va_start(ap, fmt);
    vsprintf(buf, fmt, ap);
    va_end(ap);
    printf("error: %s\n", buf);
    exit(1);
}

/*** Virtual Machine interpreter ***/
void run_vm(const code obj[], int32_t data[], int g_size, char **string_pool) {
    int32_t *sp = &data[g_size + 1];
    const code *pc = obj;

    again:
    switch (*pc++) {
        case FETCH: *sp++ = data[*(int32_t *)pc];  pc += sizeof(int32_t); goto again;
        case STORE: data[*(int32_t *)pc] = *--sp;  pc += sizeof(int32_t); goto again;
        case PUSH:  *sp++ = *(int32_t *)pc;        pc += sizeof(int32_t); goto again;
        case ADD:   sp[-2] += sp[-1]; --sp;                             goto again;
        case SUB:   sp[-2] -= sp[-1]; --sp;                             goto again;
        case MUL:   sp[-2] *= sp[-1]; --sp;                             goto again;
        case DIV:   sp[-2] /= sp[-1]; --sp;                             goto again;
        case MOD:   sp[-2] %= sp[-1]; --sp;                             goto again;
        case LT:    sp[-2] = sp[-2] <  sp[-1]; --sp;                    goto again;
        case GT:    sp[-2] = sp[-2] >  sp[-1]; --sp;                    goto again;
        case LE:    sp[-2] = sp[-2] <= sp[-1]; --sp;                    goto again;
        case GE:    sp[-2] = sp[-2] >= sp[-1]; --sp;                    goto again;
        case EQ:    sp[-2] = sp[-2] == sp[-1]; --sp;                    goto again;
        case NE:    sp[-2] = sp[-2] != sp[-1]; --sp;                    goto again;
        case AND:   sp[-2] = sp[-2] && sp[-1]; --sp;                    goto again;
        case OR:    sp[-2] = sp[-2] || sp[-1]; --sp;                    goto again;
        case NEG:   sp[-1] = -sp[-1];                                   goto again;
        case NOT:   sp[-1] = !sp[-1];                                   goto again;
        case JMP:   pc += *(int32_t *)pc;                               goto again;
        case JZ:    pc += (*--sp == 0) ? *(int32_t *)pc : (int32_t)sizeof(int32_t); goto again;
        case PRTC:  printf("%c", sp[-1]); --sp;                         goto again;
        case PRTS:  printf("%s", string_pool[sp[-1]]); --sp;            goto again;
        case PRTI:  printf("%d", sp[-1]); --sp;                         goto again;
        case HALT:                                                      break;
        default:    error("Unknown opcode %d\n", *(pc - 1));
    }
}

char *read_line(int *len) {
    static char *text = NULL;
    static int textmax = 0;

    for (*len = 0; ; (*len)++) {
        int ch = fgetc(source_fp);
        if (ch == EOF || ch == '\n') {
            if (*len == 0)
                return NULL;
            break;
        }
        if (*len + 1 >= textmax) {
            textmax = (textmax == 0 ? 128 : textmax * 2);
            text = realloc(text, textmax);
        }
        text[*len] = ch;
    }
    text[*len] = '\0';
    return text;
}

char *rtrim(char *text, int *len) {         // remove trailing spaces
    for (; *len > 0 && isspace(text[*len - 1]); --(*len))
        ;

    text[*len] = '\0';
    return text;
}

char *translate(char *st) {
    char *p, *q;
    if (st[0] == '"')                       // skip leading " if there
        ++st;
    p = q = st;

    while ((*p++ = *q++) != '\0') {
        if (q[-1] == '\\') {
            if (q[0] == 'n') {
                p[-1] = '\n';
                ++q;
            } else if (q[0] == '\\') {
                ++q;
            }
        }
        if (q[0] == '"' && q[1] == '\0')    // skip trialing " if there
            ++q;
    }

    return st;
}

/* convert an opcode string into its byte value */
int findit(const char text[], int offset) {
    for (size_t i = 0; i < sizeof(code_map) / sizeof(code_map[0]); i++) {
        if (strcmp(code_map[i].text, text) == 0)
            return code_map[i].op;
    }
    error("Unknown instruction %s at %d\n", text, offset);
    return -1;
}

void emit_byte(int c) {
    da_append(object, (uchar)c);
}

void emit_int(int32_t n) {
    union {
        int32_t n;
        unsigned char c[sizeof(int32_t)];
    } x;

    x.n = n;

    for (size_t i = 0; i < sizeof(x.n); ++i) {
        emit_byte(x.c[i]);
    }
}

/*
Datasize: 5 Strings: 3
" is prime\n"
"Total primes found: "
"\n"
 154 jmp    (-73) 82
 164 jz     (32) 197
 175 push  0
 159 fetch [4]
 149 store [3]
 */

/* Load code into global array object, return the string pool and data size */
char **load_code(int *ds) {
    int line_len, n_strings;
    char **string_pool;
    char *text = read_line(&line_len);
    text = rtrim(text, &line_len);

    strtok(text, " ");                      // skip "Datasize:"
    *ds = atoi(strtok(NULL, " "));          // get actual data_size
    strtok(NULL, " ");                      // skip "Strings:"
    n_strings = atoi(strtok(NULL, " "));    // get number of strings

    string_pool = malloc(n_strings * sizeof(char *));
    for (int i = 0; i < n_strings; ++i) {
        text = read_line(&line_len);
        text = rtrim(text, &line_len);
        text = translate(text);
        string_pool[i] = strdup(text);
    }

    for (;;) {
        int len;

        text = read_line(&line_len);
        if (text == NULL)
            break;
        text = rtrim(text, &line_len);

        int offset = atoi(strtok(text, " "));   // get the offset
        char *instr = strtok(NULL, " ");    // get the instruction
        int opcode = findit(instr, offset);
        emit_byte(opcode);
        char *operand = strtok(NULL, " ");

        switch (opcode) {
            case JMP: case JZ:
                operand++;                  // skip the '('
                len = strlen(operand);
                operand[len - 1] = '\0';    // remove the ')'
                emit_int(atoi(operand));
                break;
            case PUSH:
                emit_int(atoi(operand));
                break;
            case FETCH: case STORE:
                operand++;                  // skip the '['
                len = strlen(operand);
                operand[len - 1] = '\0';    // remove the ']'
                emit_int(atoi(operand));
                break;
        }
    }
    return string_pool;
}

void init_io(FILE **fp, FILE *std, const char mode[], const char fn[]) {
    if (fn[0] == '\0')
        *fp = std;
    else if ((*fp = fopen(fn, mode)) == NULL)
        error(0, 0, "Can't open %s\n", fn);
}

int main(int argc, char *argv[]) {
    init_io(&source_fp, stdin,  "r",  argc > 1 ? argv[1] : "");
    int data_size;
    char **string_pool = load_code(&data_size);
    int data[1000 + data_size];
    run_vm(object, data, data_size, string_pool);
}
```




## COBOL

Code by Steve Williams (with changes to work around code highlighting issues). Tested with GnuCOBOL 2.2.


```cobol>        >
SOURCE FORMAT IS FREE
identification division.
*> this code is dedicated to the public domain
*> (GnuCOBOL) 2.3-dev.0
program-id. vminterpreter.
environment division.
configuration section.
repository.  function all intrinsic.
input-output section.
file-control.
    select input-file assign using input-name
        status is input-status
        organization is line sequential.
data division.

file section.
fd  input-file.
01  input-record pic x(64).

working-storage section.
01  program-name pic x(32).
01  input-name pic x(32).
01  input-status pic xx.

01  error-record pic x(64) value spaces global.

01  v-max pic 99.
01  parameters.
    03  offset pic 999.
    03  opcode pic x(8).
    03  parm0 pic x(16).
    03  parm1 pic x(16).
    03  parm2 pic x(16).

01  opcodes.
    03  opFETCH pic x value x'00'.
    03  opSTORE pic x value x'01'.
    03  opPUSH  pic x value x'02'.
    03  opADD   pic x value x'03'.
    03  opSUB   pic x value x'04'.
    03  opMUL   pic x value x'05'.
    03  opDIV   pic x value x'06'.
    03  opMOD   pic x value x'07'.
    03  opLT    pic x value x'08'.
    03  opGT    pic x value x'09'.
    03  opLE    pic x value x'0A'.
    03  opGE    pic x value x'0B'.
    03  opEQ    pic x value x'0C'.
    03  opNE    pic x value x'0D'.
    03  opAND   pic x value x'0E'.
    03  opOR    pic x value x'0F'.
    03  opNEG   pic x value x'10'.
    03  opNOT   pic x value x'11'.
    03  opJMP   pic x value x'13'.
    03  opJZ    pic x value x'14'.
    03  opPRTC  pic x value x'15'.
    03  opPRTS  pic x value x'16'.
    03  opPRTI  pic x value x'17'.
    03  opHALT  pic x value x'18'.

01  filler.
    03  s pic 99.
    03  s-max pic 99 value 0.
    03  s-lim pic 99 value 16.
    03  filler occurs 16.
        05  string-length pic 99.
        05  string-entry pic x(48).

01  filler.
    03  v pic 99.
    03  v-lim pic 99 value 16.
    03  variables occurs 16 usage binary-int.

01  generated-code global.
    03  c  pic 999 value 1.
    03  pc pic 999.
    03  c-lim pic 999 value 512.
    03  kode pic x(512).

01  filler.
    03  stack1 pic 999 value 2.
    03  stack2 pic 999 value 1.
    03  stack-lim pic 999 value 998.
    03  stack occurs 998 usage binary-int.

01  display-definitions global.
    03  ascii-character.
        05  numeric-value usage binary-char.
    03  display-integer pic -(9)9.
    03  word-x.
        05  word usage binary-int.
    03  word-length pic 9.
    03  string1 pic 99.
    03  length1 pic 99.
    03  count1 pic 99.
    03  display-pending pic x.

procedure division.
start-vminterpreter.
    display 1 upon command-line *> get arg(1)
    accept program-name from argument-value
    move length(word) to word-length
    perform load-code
    perform run-code
    stop run
    .
run-code.
    move 1 to pc
    perform until pc >= c
        evaluate kode(pc:1)
        when opFETCH
            perform push-stack
            move kode(pc + 1:word-length) to word-x
            add 1 to word *> convert offset to subscript
            move variables(word) to stack(stack1)
            add word-length to pc
        when opPUSH
            perform push-stack
            move kode(pc + 1:word-length) to word-x
            move word to stack(stack1)
            add word-length to pc
        when opNEG
            compute stack(stack1) = -stack(stack1)
        when opNOT
            if stack(stack1) = 0
                move 1 to stack(stack1)
            else
                move 0 to stack(stack1)
            end-if
        when opJMP
            move kode(pc + 1:word-length) to word-x
            move word to pc
        when opHALT
            if display-pending = 'Y'
                display space
            end-if
            exit perform
        when opJZ
            if stack(stack1) = 0
                move kode(pc + 1:word-length) to word-x
                move word to pc
            else
                add word-length to pc
            end-if
            perform pop-stack
        when opSTORE
            move kode(pc + 1:word-length) to word-x
            add 1 to word *> convert offset to subscript
            move stack(stack1) to variables(word)
            add word-length to pc
            perform pop-stack
        when opADD
            add stack(stack1) to stack(stack2)
            perform pop-stack
        when opSUB
            subtract stack(stack1) from stack(stack2)
            perform pop-stack
        when opMUL
            multiply stack(stack1) by stack(stack2)
                *>rounded mode nearest-toward-zero *> doesn't match python
            perform pop-stack
        when opDIV
            divide stack(stack1) into stack(stack2)
                *>rounded mode nearest-toward-zero *> doesn't match python
            perform pop-stack
        when opMOD
            move mod(stack(stack2),stack(stack1)) to stack(stack2)
            perform pop-stack
        when opLT
            if stack(stack2) <  stack(stack1)
                move 1 to stack(stack2)
            else
                move 0 to stack(stack2)
            end-if
            perform pop-stack
        when opGT
            if stack(stack2) >  stack(stack1)
                move 1 to stack(stack2)
            else
                move 0 to stack(stack2)
            end-if
            perform pop-stack
        when opLE
            if stack(stack2) <= stack(stack1)
                move 1 to stack(stack2)
            else
                move 0 to stack(stack2)
            end-if
            perform pop-stack
        when opGE
            if stack(stack2) >= stack(stack1)
                move 1 to stack(stack2)
            else
                move 0 to stack(stack2)
            end-if
            perform pop-stack
        when opEQ
            if stack(stack2) = stack(stack1)
                move 1 to stack(stack2)
            else
                move 0 to stack(stack2)
            end-if
            perform pop-stack
        when opNE
            if stack(stack2) <> stack(stack1)
                move 1 to stack(stack2)
            else
                move 0 to stack(stack2)
            end-if
            perform pop-stack
        when opAND
            call "CBL_AND" using stack(stack1) stack(stack2) by value word-length
            perform pop-stack
        when opOR
            call "CBL_OR" using stack(stack1) stack(stack2) by value word-length
            perform pop-stack
        when opPRTC
            move stack(stack1) to numeric-value
            if numeric-value = 10
                display space
                move 'N' to display-pending
            else
                display ascii-character with no advancing
                move 'Y' to display-pending
            end-if
            perform pop-stack
        when opPRTS
            add 1 to word *> convert offset to subscript
            move 1 to string1
            move string-length(word) to length1
            perform until string1 > string-length(word)
                move 0 to count1
                inspect string-entry(word)(string1:length1)
                    tallying count1 for characters before initial '\'   *> ' workaround code highlighter problem
                evaluate true
                when string-entry(word)(string1 + count1 + 1:1) = 'n' *> \n
                    display string-entry(word)(string1:count1)
                    move 'N' to display-pending
                    compute string1 = string1 + 2 + count1
                    compute length1 = length1 - 2 - count1
                when string-entry(word)(string1 + count1 + 1:1) = '\' *> ' \\
                    display string-entry(word)(string1:count1 + 1) with no advancing
                    move 'Y' to display-pending
                    compute string1 = string1 + 2 + count1
                    compute length1 = length1 - 2 - count1
                when other
                    display string-entry(word)(string1:count1) with no advancing
                    move 'Y' to display-pending
                    add count1 to string1
                    subtract count1 from length1
                end-evaluate
            end-perform
            perform pop-stack
        when opPRTI
            move stack(stack1) to display-integer
            display trim(display-integer) with no advancing
            move 'Y' to display-pending
            perform pop-stack
        end-evaluate
        add 1 to pc
    end-perform
    .
push-stack.
    if stack1 >= stack-lim
        string 'in vminterpreter at ' pc ' stack overflow at ' stack-lim into error-record
        perform report-error
    end-if
    add 1 to stack1 stack2
    >>d display ' push at ' pc space stack1 space stack2
    .
pop-stack.
    if stack1 < 2
        string 'in vminterpreter at ' pc ' stack underflow' into error-record
        perform report-error
    end-if
    >>d display ' pop at ' pc space stack1 space stack2
    subtract 1 from stack1 stack2
    .
load-code.
    perform read-input
    if input-status <> '00'
        string 'in vminterpreter no input data' into error-record
        perform report-error
    end-if

    unstring input-record delimited by all spaces into parm1 v-max parm2 s-max
    if v-max > v-lim
        string 'in vminterpreter datasize exceeds ' v-lim into error-record
        perform report-error
    end-if
    if s-max > s-lim
        string 'in vminterpreter number of strings exceeds ' s-lim into error-record
        perform report-error
    end-if

    perform read-input
    perform varying s from 1 by 1 until s > s-max
    or input-status <> '00'
        compute string-length(s) string-length(word) = length(trim(input-record)) - 2
        move input-record(2:string-length(word)) to string-entry(s)
        perform read-input
    end-perform
    if s <= s-max
        string 'in vminterpreter not all strings found' into error-record
        perform report-error
    end-if

    perform until input-status <> '00'
        initialize parameters
        unstring input-record delimited by all spaces into
            parm0 offset opcode parm1 parm2
        evaluate opcode
        when 'fetch'
            call 'emitbyte' using opFETCH
            call 'emitword' using parm1
        when 'store'
            call 'emitbyte' using opSTORE
            call 'emitword' using parm1
        when 'push'
            call 'emitbyte' using opPUSH
            call 'emitword' using parm1
        when 'add' call 'emitbyte' using opADD
        when 'sub' call 'emitbyte' using opSUB
        when 'mul' call 'emitbyte' using opMUL
        when 'div' call 'emitbyte' using opDIV
        when 'mod' call 'emitbyte' using opMOD
        when 'lt'  call 'emitbyte' using opLT
        when 'gt'  call 'emitbyte' using opGT
        when 'le'  call 'emitbyte' using opLE
        when 'ge'  call 'emitbyte' using opGE
        when 'eq'  call 'emitbyte' using opEQ
        when 'ne'  call 'emitbyte' using opNE
        when 'and' call 'emitbyte' using opAND
        when 'or'  call 'emitbyte' using opOR
        when 'not' call 'emitbyte' using opNOT
        when 'neg' call 'emitbyte' using opNEG
        when 'jmp'
             call 'emitbyte' using opJMP
             call 'emitword' using parm2
        when 'jz'
             call 'emitbyte' using opJZ
             call 'emitword' using parm2
        when 'prtc' call 'emitbyte' using opPRTC
        when 'prts' call 'emitbyte' using opPRTS
        when 'prti' call 'emitbyte' using opPRTI
        when 'halt' call 'emitbyte' using opHALT
        when other
            string 'in vminterpreter unknown opcode ' trim(opcode) ' at ' offset into error-record
            perform report-error
        end-evaluate
        perform read-input
    end-perform
    .
read-input.
    if program-name = spaces
        move '00' to input-status
        accept input-record on exception move '10' to input-status end-accept
        exit paragraph
    end-if
    if input-name = spaces
        string program-name delimited by space '.gen' into input-name
        open input input-file
        if input-status <> '00'
            string 'in vminterpreter ' trim(input-name) ' file open status ' input-status
                into error-record
            perform report-error
        end-if
    end-if
    read input-file into input-record
    evaluate input-status
    when '00'
        continue
    when '10'
        close input-file
    when other
        string 'in vminterpreter unexpected input-status: ' input-status into error-record
        perform report-error
    end-evaluate
    .
report-error.
    display error-record upon syserr
    stop run with error status -1
    .
identification division.
program-id. emitbyte.
data division.
linkage section.
01  opcode pic x.
procedure division using opcode.
start-emitbyte.
    if c >= c-lim
        string 'in vminterpreter emitbyte c exceeds ' c-lim into error-record
        call 'reporterror'
    end-if
    move opcode to kode(c:1)
    add 1 to c
    .
end program emitbyte.

identification division.
program-id. emitword.
data division.
working-storage section.
01  word-temp pic x(8).
linkage section.
01  word-value any length.
procedure division using word-value.
start-emitword.
    if c + word-length >= c-lim
        string 'in vminterpreter emitword c exceeds ' c-lim into error-record
        call 'reporterror'
    end-if
    move word-value to word-temp
    inspect word-temp converting '[' to ' '
    inspect word-temp converting ']' to ' '
    move numval(trim(word-temp)) to word
    move word-x to kode(c:word-length)
    add word-length to c
    .
end program emitword.

end program vminterpreter.
```


{{out|case=Count}}

```txt
prompt$ ./lexer <testcases/Count | ./parser | ./generator | ./vminterpreter
count is: 1
count is: 2
count is: 3
count is: 4
count is: 5
count is: 6
count is: 7
count is: 8
count is: 9
```




## Forth

Tested with Gforth 0.7.3

```Forth
CREATE BUF 0 ,              \ single-character look-ahead buffer
: PEEK   BUF @ 0= IF KEY BUF ! THEN BUF @ ;
: GETC   PEEK  0 BUF ! ;
: SPACE?   DUP BL = SWAP 9 14 WITHIN OR ;
: >SPACE   BEGIN PEEK SPACE? WHILE GETC DROP REPEAT ;
: DIGIT?   48 58 WITHIN ;
: >INT ( -- n)   >SPACE  0
   BEGIN  PEEK DIGIT?
   WHILE  GETC [CHAR] 0 -  SWAP 10 * +  REPEAT ;
CREATE A 0 ,
: C@A ( -- c)  A @ C@ ;
: C@A+ ( -- c)  C@A  1 CHARS A +! ;
: C!A+ ( c --)  A @ C!  1 CHARS A +! ;
: WORD ( -- c-addr)  >SPACE  PAD 1+ A !
   BEGIN PEEK SPACE? INVERT WHILE GETC C!A+ REPEAT
   >SPACE  PAD A @ OVER - 1- PAD C! ;
: >STRING ( -- c-addr)  >SPACE GETC DROP  PAD 1+ A !
   BEGIN PEEK [CHAR] " <> WHILE GETC C!A+ REPEAT
   GETC DROP  PAD A @ OVER - 1- PAD C! ;
: \INTERN ( c-addr -- c-addr)  HERE >R  A ! C@A+ DUP C,
   BEGIN DUP WHILE C@A+
     DUP [CHAR] \ = IF DROP -1 R@ +!  C@A+
       [CHAR] n = IF 10 ELSE [CHAR] \ THEN
     THEN C,  1-
   REPEAT  DROP R> ;
: .   0 .R ;

CREATE DATA 0 ,
CREATE STRINGS 0 ,
: >DATA   HERE DATA !
   WORD DROP  >INT 4 * BEGIN DUP WHILE 0 C, 1- REPEAT DROP ;
: >STRINGS   HERE STRINGS !
   WORD DROP  >INT DUP >R CELLS  ALLOT
   0 BEGIN DUP R@ < WHILE
     DUP CELLS >STRING \INTERN STRINGS @ ROT + !  1+
   REPEAT R> DROP DROP ;
: >HEADER   >DATA >STRINGS ;
: i32! ( n addr --)
   OVER           $FF AND OVER C! 1+
   OVER  8 RSHIFT $FF AND OVER C! 1+
   OVER 16 RSHIFT $FF AND OVER C! 1+
   SWAP 24 RSHIFT $FF AND SWAP C! ;
: i32@ ( addr -- n) >R  \ This is kinda slow... hmm
   R@     C@
   R@ 1 + C@  8 LSHIFT OR
   R@ 2 + C@ 16 LSHIFT OR
   R> 3 + C@ 24 LSHIFT OR
   DUP $7FFFFFFF AND SWAP $80000000 AND - ;  \ sign extend
: i32, ( n --)  HERE  4 ALLOT  i32! ;
: i32@+ ( -- n)  A @ i32@  A @ 4 + A ! ;
CREATE BYTECODE 0 ,
: @fetch   i32@+ 4 * DATA @ + i32@ ;
: @store   i32@+ 4 * DATA @ + i32! ;
: @jmp     i32@+ BYTECODE @ + A ! ;
: @jz      IF 4 A +! ELSE @jmp THEN ;
: @prts    CELLS STRINGS @ + @ COUNT TYPE ;
: @div     >R S>D R> SM/REM SWAP DROP ;
CREATE OPS
' @fetch , ' @store , ' i32@+ , ' @jmp ,   ' @jz ,
' EMIT ,   ' . ,      ' @prts , ' NEGATE , ' 0= ,
' + ,      ' - ,      ' * ,     ' @div ,   ' MOD ,
' < ,      ' > ,      ' <= ,    ' >= ,
' = ,      ' <> ,     ' AND ,   ' OR ,     ' BYE ,
CREATE #OPS 0 ,
: OP:   CREATE #OPS @ ,  1 #OPS +!  DOES> @ ;
OP: fetch  OP: store  OP: push  OP: jmp  OP: jz
OP: prtc   OP: prti   OP: prts  OP: neg  OP: not
OP: add    OP: sub    OP: mul   OP: div  OP: mod
OP: lt     OP: gt     OP: le    OP: ge
OP: eq     OP: ne     OP: and   OP: or   OP: halt
: >OP   WORD FIND
   0= IF ." Unrecognized opcode" ABORT THEN EXECUTE ;
: >i32   >INT i32, ;
: >[i32]  GETC DROP >i32 GETC DROP ;
: >OFFSET   WORD DROP ( drop relative offset) >i32 ;
CREATE >PARAM  ' >[i32] DUP , , ' >i32 , ' >OFFSET DUP , ,
: >BYTECODE   HERE >R
   BEGIN >INT DROP  >OP >R  R@ C,
     R@ 5 < IF R@ CELLS >PARAM + @ EXECUTE THEN
   R> halt = UNTIL  R> BYTECODE ! ;
: RUN   BYTECODE @ A !
   BEGIN C@A+ CELLS OPS + @ EXECUTE AGAIN ;
>HEADER >BYTECODE RUN
```




## Go

{{trans|Python}}

```go
package main

import (
    "bufio"
    "encoding/binary"
    "fmt"
    "log"
    "math"
    "os"
    "strconv"
    "strings"
)

type code = byte

const (
    fetch code = iota
    store
    push
    add
    sub
    mul
    div
    mod
    lt
    gt
    le
    ge
    eq
    ne
    and
    or
    neg
    not
    jmp
    jz
    prtc
    prts
    prti
    halt
)

var codeMap = map[string]code{
    "fetch": fetch,
    "store": store,
    "push":  push,
    "add":   add,
    "sub":   sub,
    "mul":   mul,
    "div":   div,
    "mod":   mod,
    "lt":    lt,
    "gt":    gt,
    "le":    le,
    "ge":    ge,
    "eq":    eq,
    "ne":    ne,
    "and":   and,
    "or":    or,
    "neg":   neg,
    "not":   not,
    "jmp":   jmp,
    "jz":    jz,
    "prtc":  prtc,
    "prts":  prts,
    "prti":  prti,
    "halt":  halt,
}

var (
    err        error
    scanner    *bufio.Scanner
    object     []code
    stringPool []string
)

func reportError(msg string) {
    log.Fatalf("error : %s\n", msg)
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func btoi(b bool) int32 {
    if b {
        return 1
    }
    return 0
}

func itob(i int32) bool {
    if i != 0 {
        return true
    }
    return false
}

func emitByte(c code) {
    object = append(object, c)
}

func emitWord(n int) {
    bs := make([]byte, 4)
    binary.LittleEndian.PutUint32(bs, uint32(n))
    for _, b := range bs {
        emitByte(code(b))
    }
}

/*** Virtual Machine interpreter ***/
func runVM(dataSize int) {
    stack := make([]int32, dataSize+1)
    pc := int32(0)
    for {
        op := object[pc]
        pc++
        switch op {
        case fetch:
            x := int32(binary.LittleEndian.Uint32(object[pc : pc+4]))
            stack = append(stack, stack[x])
            pc += 4
        case store:
            x := int32(binary.LittleEndian.Uint32(object[pc : pc+4]))
            ln := len(stack)
            stack[x] = stack[ln-1]
            stack = stack[:ln-1]
            pc += 4
        case push:
            x := int32(binary.LittleEndian.Uint32(object[pc : pc+4]))
            stack = append(stack, x)
            pc += 4
        case add:
            ln := len(stack)
            stack[ln-2] += stack[ln-1]
            stack = stack[:ln-1]
        case sub:
            ln := len(stack)
            stack[ln-2] -= stack[ln-1]
            stack = stack[:ln-1]
        case mul:
            ln := len(stack)
            stack[ln-2] *= stack[ln-1]
            stack = stack[:ln-1]
        case div:
            ln := len(stack)
            stack[ln-2] = int32(float64(stack[ln-2]) / float64(stack[ln-1]))
            stack = stack[:ln-1]
        case mod:
            ln := len(stack)
            stack[ln-2] = int32(math.Mod(float64(stack[ln-2]), float64(stack[ln-1])))
            stack = stack[:ln-1]
        case lt:
            ln := len(stack)
            stack[ln-2] = btoi(stack[ln-2] < stack[ln-1])
            stack = stack[:ln-1]
        case gt:
            ln := len(stack)
            stack[ln-2] = btoi(stack[ln-2] > stack[ln-1])
            stack = stack[:ln-1]
        case le:
            ln := len(stack)
            stack[ln-2] = btoi(stack[ln-2] <= stack[ln-1])
            stack = stack[:ln-1]
        case ge:
            ln := len(stack)
            stack[ln-2] = btoi(stack[ln-2] >= stack[ln-1])
            stack = stack[:ln-1]
        case eq:
            ln := len(stack)
            stack[ln-2] = btoi(stack[ln-2] == stack[ln-1])
            stack = stack[:ln-1]
        case ne:
            ln := len(stack)
            stack[ln-2] = btoi(stack[ln-2] != stack[ln-1])
            stack = stack[:ln-1]
        case and:
            ln := len(stack)
            stack[ln-2] = btoi(itob(stack[ln-2]) && itob(stack[ln-1]))
            stack = stack[:ln-1]
        case or:
            ln := len(stack)
            stack[ln-2] = btoi(itob(stack[ln-2]) || itob(stack[ln-1]))
            stack = stack[:ln-1]
        case neg:
            ln := len(stack)
            stack[ln-1] = -stack[ln-1]
        case not:
            ln := len(stack)
            stack[ln-1] = btoi(!itob(stack[ln-1]))
        case jmp:
            x := int32(binary.LittleEndian.Uint32(object[pc : pc+4]))
            pc += x
        case jz:
            ln := len(stack)
            v := stack[ln-1]
            stack = stack[:ln-1]
            if v != 0 {
                pc += 4
            } else {
                x := int32(binary.LittleEndian.Uint32(object[pc : pc+4]))
                pc += x
            }
        case prtc:
            ln := len(stack)
            fmt.Printf("%c", stack[ln-1])
            stack = stack[:ln-1]
        case prts:
            ln := len(stack)
            fmt.Printf("%s", stringPool[stack[ln-1]])
            stack = stack[:ln-1]
        case prti:
            ln := len(stack)
            fmt.Printf("%d", stack[ln-1])
            stack = stack[:ln-1]
        case halt:
            return
        default:
            reportError(fmt.Sprintf("Unknown opcode %d\n", op))
        }
    }
}

func translate(s string) string {
    var d strings.Builder
    for i := 0; i < len(s); i++ {
        if s[i] == '\\' && (i+1) < len(s) {
            if s[i+1] == 'n' {
                d.WriteByte('\n')
                i++
            } else if s[i+1] == '\\' {
                d.WriteByte('\\')
                i++
            }
        } else {
            d.WriteByte(s[i])
        }
    }
    return d.String()
}

func loadCode() int {
    var dataSize int
    firstLine := true
    for scanner.Scan() {
        line := strings.TrimRight(scanner.Text(), " \t")
        if len(line) == 0 {
            if firstLine {
                reportError("empty line")
            } else {
                break
            }
        }
        lineList := strings.Fields(line)
        if firstLine {
            dataSize, err = strconv.Atoi(lineList[1])
            check(err)
            nStrings, err := strconv.Atoi(lineList[3])
            check(err)
            for i := 0; i < nStrings; i++ {
                scanner.Scan()
                s := strings.Trim(scanner.Text(), "\"\n")
                stringPool = append(stringPool, translate(s))
            }
            firstLine = false
            continue
        }
        offset, err := strconv.Atoi(lineList[0])
        check(err)
        instr := lineList[1]
        opCode, ok := codeMap[instr]
        if !ok {
            reportError(fmt.Sprintf("Unknown instruction %s at %d", instr, opCode))
        }
        emitByte(opCode)
        switch opCode {
        case jmp, jz:
            p, err := strconv.Atoi(lineList[3])
            check(err)
            emitWord(p - offset - 1)
        case push:
            value, err := strconv.Atoi(lineList[2])
            check(err)
            emitWord(value)
        case fetch, store:
            value, err := strconv.Atoi(strings.Trim(lineList[2], "[]"))
            check(err)
            emitWord(value)
        }
    }
    check(scanner.Err())
    return dataSize
}

func main() {
    codeGen, err := os.Open("codegen.txt")
    check(err)
    defer codeGen.Close()
    scanner = bufio.NewScanner(codeGen)
    runVM(loadCode())
}
```


{{out}}
Using the 'while count' example:

```txt

count is: 1
count is: 2
count is: 3
count is: 4
count is: 5
count is: 6
count is: 7
count is: 8
count is: 9

```



## Julia


```julia
mutable struct VM32
    code::Vector{UInt8}
    stack::Vector{Int32}
    data::Vector{Int32}
    strings::Vector{String}
    offsets::Vector{Int32}
    lastargs::Vector{Int32}
    ip::Int32
    VM32() = new(Vector{UInt8}(), Vector{Int32}(), Vector{Int32}(),
                 Vector{String}(), Vector{Int32}(), Vector{Int32}(), 1)
end

halt, add, sub, mul, Div, mod, not, neg, and, or, lt, gt, le, ge, ne, eq, prts,
    prti, prtc, store, Fetch, push, jmp, jz = UInt8.(collect(1:24))

function assemble(io)
    vm = VM32()
    header = readline(io)
    datasize, nstrings = match(r"\w+:\s*(\d+)\s+\w+:\s*(\d+)", header).captures
    vm.data = zeros(Int32, parse(Int, datasize) + 4)
    for i in 1:parse(Int, nstrings)
        line = replace(strip(readline(io), ['"', '\n']), r"\\." => x -> x[end] == 'n' ? "\n" : string(x[end]))
        push!(vm.strings, line)
    end
    while !eof(io)
        line = readline(io)
        offset, op, arg1, arg2 = match(r"(\d+)\s+(\w+)\s*(\S+)?\s*(\S+)?", line).captures
        op = op in ["fetch", "div"] ? uppercasefirst(op) : op
        push!(vm.code, eval(Symbol(op)))
        if arg1 != nothing
            v = parse(Int32, strip(arg1, ['[', ']', '(', ')']))
            foreach(x -> push!(vm.code, x), reinterpret(UInt8, [v]))
        end
        if arg2 != nothing
            push!(vm.lastargs, (x = tryparse(Int32, arg2)) == nothing ? 0 : x)
        end
        push!(vm.offsets, parse(Int32, offset))
    end
    vm
end

function runvm(vm)
    value() = (x = vm.ip; vm.ip += 4; reinterpret(Int32, vm.code[x:x+3])[1])
    tobool(x) = (x != 0)
    ops = Dict(
        halt  => () -> exit(),
        add   => () -> begin vm.stack[end-1] += vm.stack[end]; pop!(vm.stack); vm.stack[end] end,
        sub   => () -> begin vm.stack[end-1] -= vm.stack[end]; pop!(vm.stack); vm.stack[end] end,
        mul   => () -> begin vm.stack[end-1] *= vm.stack[end]; pop!(vm.stack); vm.stack[end] end,
        Div   => () -> begin vm.stack[end-1] /= vm.stack[end]; pop!(vm.stack); vm.stack[end] end,
        mod   => () -> begin vm.stack[end-1] %= vm.stack[1]; pop!(vm.stack); vm.stack[end] end,
        not   => () -> vm.stack[end] = vm.stack[end] ? 0 : 1,
        neg   => () -> vm.stack[end] = -vm.stack[end],
        and   => () -> begin vm.stack[end-1] = tobool(vm.stack[end-1]) && tobool(vm.stack[end]) ? 1 : 0; pop!(vm.stack); vm.stack[end] end,
        or    => () -> begin vm.stack[end-1] = tobool(vm.stack[end-1]) || tobool(vm.stack[end]) ? 1 : 0; pop!(vm.stack); vm.stack[end] end,
        lt    => () -> begin x = (vm.stack[end-1] < vm.stack[end] ? 1 : 0); pop!(vm.stack); vm.stack[end] = x end,
        gt    => () -> begin x = (vm.stack[end-1] > vm.stack[end] ? 1 : 0); pop!(vm.stack); vm.stack[end] = x end,
        le    => () -> begin x = (vm.stack[end-1] <= vm.stack[end] ? 1 : 0); pop!(vm.stack); vm.stack[end] = x end,
        ge    => () -> begin x = (vm.stack[end-1] >= vm.stack[end] ? 1 : 0); pop!(vm.stack); vm.stack[end] = x end,
        ne    => () -> begin x = (vm.stack[end-1] != vm.stack[end] ? 1 : 0); pop!(vm.stack); vm.stack[end] = x end,
        eq    => () -> begin x = (vm.stack[end-1] == vm.stack[end] ? 1 : 0); pop!(vm.stack); vm.stack[end] = x end,
        prts  => () -> print(vm.strings[pop!(vm.stack) + 1]),
        prti  => () -> print(pop!(vm.stack)),
        prtc  => () -> print(Char(pop!(vm.stack))),
        store => () -> vm.data[value() + 1] = pop!(vm.stack),
        Fetch => () -> push!(vm.stack, vm.data[value() + 1]),
        push  => () -> push!(vm.stack, value()),
        jmp   => () -> vm.ip += value(),
        jz    => () -> if pop!(vm.stack) == 0 vm.ip += value() else vm.ip += 4 end)
    vm.ip = 1
    while true
        op = vm.code[vm.ip]
        vm.ip += 1
        ops[op]()
    end
end

const testasm = """
Datasize: 1 Strings: 2
"count is: "
"\\n"
    0 push  1
    5 store [0]
   10 fetch [0]
   15 push  10
   20 lt
   21 jz     (43) 65
   26 push  0
   31 prts
   32 fetch [0]
   37 prti
   38 push  1
   43 prts
   44 fetch [0]
   49 push  1
   54 add
   55 store [0]
   60 jmp    (-51) 10
   65 halt   """

const iob = IOBuffer(testasm)
const vm = assemble(iob)
runvm(vm)

```
{{output}}
```txt

 count is: 1
 count is: 2
 count is: 3
 count is: 4
 count is: 5
 count is: 6
 count is: 7
 count is: 8
 count is: 9

```



## M2000 Interpreter


### Using Select Case


```M2000 Interpreter

Module Virtual_Machine_Interpreter (a$){
	\\ function to extract string, replacing escape codes.
	Function GetString$(a$) {
		s=instr(a$, chr$(34))
		m=rinstr(a$,chr$(34))-s
		if m>1 then
			\\ process escape codes
			=format$(mid$(a$, s+1, m-1))
		else
			=""
		end if
	}
	\\ module to print a string to console using codes, 13, 10, 9
	Module printsrv (a$) {
		for i=1 to len(a$)
			select case chrcode(Mid$(a$,i,1))
			case 13
				cursor 0
			case 10
				cursor 0 : Print
			case 9
				cursor ((pos+tab) div tab)*tab
			else case
			{
				m=pos :if pos>=width then Print : m=pos
				Print Mid$(a$,i,1);
				if m<=width then cursor m+1
			}
			end select
		next i
	}
	const nl$=chr$(13)+chr$(10)
	\\ we can set starting value to any number  n where 0<=n<=232
	enum op {	halt_=232, add_, sub_, mul_, div_, mod_, not_, neg_, and_, or_, lt_,
		    	gt_, le_, ge_, ne_, eq_, prts_, prti_, prtc_, store_, fetch_, push_,
			jmp_, jz_
    	}
	Rem : Form 120, 60 ' change console width X height to run Ascii Mandlebrot examlpe
	Report "Virtual Assembly Code:"+{
	}+a$
	Print "Prepare Byte Code"

	\\ get datasize
	a$=rightpart$(a$, "Datasize:")
	m=0
	data_size=val(a$, "int", m)
	a$=mid$(a$, m)
	\\ make stack
	if data_size>0 then Buffer Clear stack_ as long*data_size
	\\ dim or redim buffer append 1000 long as is.
	Buffer stack_ as long*(1000+data_size)
	\\ get strings
	a$=rightpart$(a$, "Strings:")
	m=0
	strings=val(a$, "int", m)
	a$=rightpart$(a$, nl$)

	if strings>0 then
		Dim strings$(strings)
		for i=0 to strings-1
			strings$(i)=GetString$(leftpart$(a$, nl$))
			a$=rightpart$(a$, nl$)
		Next i
	End if
	buffer clear code_ as byte*1000
	do
		m=0
		offset=val(a$,"int", m)
		if m<0 then exit
		a$=mid$(a$,m)
		line$=trim$(leftpart$(a$,nl$))
		if line$="" then line$=trim$(a$) else a$=trim$(rightpart$(a$, nl$))
		op$=if$(instr(line$," ")>0->leftpart$(line$," "), line$)
		if not valid(eval(op$+"_")) then exit
		opc=eval(op$+"_")
		Return code_, offset:=opc
		if opc>=store_ then
			line$=rightpart$(line$," ")
			select case opc
			case store_, fetch_
				Return code_, offset+1:=val(rightpart$(leftpart$(line$,"]"),"[")) as long : offset+=4
			case push_
				Return code_, offset+1:=uint(val(line$)) as long : offset+=4
			case jz_, jmp_
				Return code_, offset+1:=val(rightpart$(line$,")")) as long : offset+=4
			end select
		end if
	Always
	Print "Press any key" : Push key$ : Drop
	\\ Prepare VM
	let pc=0, sp=len(stack_) div 4
	do {
		func=eval(code_, pc)
		pc++
		select case func
		case halt_
			exit
		case push_
			sp--:return stack_, sp:=eval(code_, pc as long):pc+=4
		case jz_
			sp++: if eval(stack_, sp-1)=0 then pc=eval(code_, pc as long) else pc+=4
		case jmp_
			pc=eval(code_, pc as long)
		case fetch_
			sp--:Return stack_, sp:=eval(stack_, eval(code_, pc as long)):pc+=4
		case store_
			Return stack_, eval(code_, pc as long):=eval(stack_, sp):sp++:pc+=4
		case add_
			Return stack_, sp+1:=uint(sint(eval(stack_, sp+1))+sint(eval(stack_, sp))):sp++
		case sub_
			Return stack_, sp+1:=uint(sint(eval(stack_, sp+1))-sint(eval(stack_, sp))):sp++
		case mul_
			Return stack_, sp+1:=uint(sint(eval(stack_, sp+1))*sint(eval(stack_, sp))):sp++
		case div_
			Return stack_, sp+1:=uint(sint(eval(stack_, sp+1)) div sint(eval(stack_, sp))):sp++
		case mod_
			Return stack_, sp+1:=uint(sint(eval(stack_, sp+1)) mod sint(eval(stack_, sp))) :sp++
		case not_
			Return stack_, sp:=if(eval(stack_, sp)=0->uint(-1),0)
		case neg_  \\ we can use neg(sint(value))+1 or uint(-sint(value))
			Return stack_, sp:=uint(-sint(eval(stack_, sp)))
		case and_
			Return stack_, sp+1:=binary.and(eval(stack_, sp+1),eval(stack_, sp)):sp++
		case or_
			Return stack_, sp+1:=binary.or(eval(stack_, sp+1),eval(stack_, sp)):sp++
		case lt_
			Return stack_, sp+1:=uint(if(sint(eval(stack_, sp+1))<sint(eval(stack_, sp))->-1, 0)):sp++
		case gt_
			Return stack_, sp+1:=uint(if(sint(eval(stack_, sp+1))>sint(eval(stack_, sp))->-1, 0)):sp++
		case le_
			Return stack_, sp+1:=uint(if(sint(eval(stack_, sp+1))<=sint(eval(stack_, sp))->-1, 0)):sp++
		case ge_
			Return stack_, sp+1:=uint(if(sint(eval(stack_, sp+1))>=sint(eval(stack_, sp))->-1, 0)):sp++
		case ne_
			Return stack_, sp+1:=uint(if(eval(stack_, sp+1)<>eval(stack_, sp)->-1, 0)):sp++
		case eq_
			Return stack_, sp+1:=uint(if(eval(stack_, sp+1)=eval(stack_, sp)->-1, 0)):sp++
		case prts_
			printsrv strings$(eval(stack_,sp)):sp++
		case prti_
			printsrv str$(sint(eval(stack_,sp)),0):sp++
		case prtc_
			printsrv chrcode$(eval(stack_,sp)):sp++
		else case
			Error "Unkown op "+str$(func)
		end select
	} always
	Print "done"
}
Virtual_Machine_Interpreter {
Datasize: 1 Strings: 2
"count is: "
"\n"
    0 push  1
    5 store [0]
   10 fetch [0]
   15 push  10
   20 lt
   21 jz     (43) 65
   26 push  0
   31 prts
   32 fetch [0]
   37 prti
   38 push  1
   43 prts
   44 fetch [0]
   49 push  1
   54 add
   55 store [0]
   60 jmp    (-51) 10
   65 halt
}

```



### Using Lambda functions


A call local to function pass the current scope to function, so it's like a call to subroutine, but faster.


```M2000 Interpreter

Module Virtual_Machine_Interpreter (a$){
	\\ function to extract string, replacing escape codes.
	Function GetString$(a$) {
		s=instr(a$, chr$(34))
		m=rinstr(a$,chr$(34))-s
		if m>1 then
			\\ process escape codes
			=format$(mid$(a$, s+1, m-1))
		else
			=""
		end if
	}
	\\ module to print a string to console using codes, 13, 10, 9
	Module printsrv (a$) {
		for i=1 to len(a$)
			select case chrcode(Mid$(a$,i,1))
			case 13
				cursor 0
			case 10
				cursor 0 : Print
			case 9
				cursor ((pos+tab) div tab)*tab
			else case
			{
				m=pos :if pos>=width then Print : m=pos
				Print Mid$(a$,i,1);
				if m<=width then cursor m+1
			}
			end select
		next i
	}
	const nl$=chr$(13)+chr$(10)
	\\ we can set starting value to any number  n where 0<=n<=232
	enum op {	halt_=232, add_, sub_, mul_, div_, mod_, not_, neg_, and_, or_, lt_,
		    	gt_, le_, ge_, ne_, eq_, prts_, prti_, prtc_, store_, fetch_, push_,
			jmp_, jz_
    	}
     	exit_now=false
	Inventory  func=halt_:=lambda->{exit_now=true}
	Append  func, push_:=lambda->{sp--:return stack_, sp:=eval(code_, pc as long):pc+=4}
	Append  func, jz_:=lambda->{
		sp++: if eval(stack_, sp-1)=0 then pc=eval(code_, pc as long) else pc+=4
	}
	Append  func, jmp_:=lambda->{pc=eval(code_, pc as long)}
	Append  func, fetch_:=lambda->{sp--:Return stack_, sp:=eval(stack_, eval(code_, pc as long)):pc+=4}
	Append  func, store_:=lambda->{Return stack_, eval(code_, pc as long):=eval(stack_, sp):sp++:pc+=4}
	Append  func, add_:=lambda->{Return stack_, sp+1:=uint(sint(eval(stack_, sp+1))+sint(eval(stack_, sp))):sp++}
	Append  func, sub_:=lambda->{Return stack_, sp+1:=uint(sint(eval(stack_, sp+1))-sint(eval(stack_, sp))):sp++}
	Append  func, mul_:=lambda->{Return stack_, sp+1:=uint(sint(eval(stack_, sp+1))*sint(eval(stack_, sp))):sp++}
	Append  func, div_:=lambda->{Return stack_, sp+1:=uint(sint(eval(stack_, sp+1)) div sint(eval(stack_, sp))):sp++}
	Append  func, mod_:=lambda->{Return stack_, sp+1:=uint(sint(eval(stack_, sp+1)) mod sint(eval(stack_, sp))) :sp++}
	Append  func, not_:=lambda->{Return stack_, sp:=if(eval(stack_, sp)=0->uint(-1),0)}
	Append  func, neg_:=lambda->{Return stack_, sp:=uint(-sint(eval(stack_, sp)))}
	Append  func, and_:=lambda->{Return stack_, sp+1:=binary.and(eval(stack_, sp+1),eval(stack_, sp)):sp++	}
	Append  func, or_:=lambda->{Return stack_, sp+1:=binary.or(eval(stack_, sp+1),eval(stack_, sp)):sp++	}
	Append  func, lt_:=lambda->{Return stack_, sp+1:=uint(if(sint(eval(stack_, sp+1))<sint(eval(stack_, sp))->-1, 0)):sp++}
	Append  func, gt_:=lambda->{Return stack_, sp+1:=uint(if(sint(eval(stack_, sp+1))>sint(eval(stack_, sp))->-1, 0)):sp++}
	Append  func, le_:=lambda->{Return stack_, sp+1:=uint(if(sint(eval(stack_, sp+1))<=sint(eval(stack_, sp))->-1, 0)):sp++}
	Append  func, ge_:=lambda->{Return stack_, sp+1:=uint(if(sint(eval(stack_, sp+1))>=sint(eval(stack_, sp))->-1, 0)):sp++}
	Append  func, ne_:=lambda->{Return stack_, sp+1:=uint(if(eval(stack_, sp+1)<>eval(stack_, sp)->-1, 0)):sp++}
	Append  func, eq_:=lambda->{Return stack_, sp+1:=uint(if(eval(stack_, sp+1)=eval(stack_, sp)->-1, 0)):sp++}
	Append  func, prts_:=lambda->{printsrv strings$(eval(stack_,sp)):sp++}
	Append  func, prti_:=lambda->{printsrv str$(sint(eval(stack_,sp)),0):sp++}
	Append  func, prtc_:=lambda->{printsrv chrcode$(eval(stack_,sp)):sp++}
	Rem : Form 120, 60 ' change console width X height to run Ascii Mandlebrot examlpe
	Report "Virtual Assembly Code:"+{
	}+a$
	Print "Prepare Byte Code"

	\\ get datasize
	a$=rightpart$(a$, "Datasize:")
	m=0
	data_size=val(a$, "int", m)
	a$=mid$(a$, m)
	\\ make stack
	if data_size>0 then Buffer Clear stack_ as long*data_size
	\\ dim or redim buffer append 1000 long as is.
	Buffer stack_ as long*(1000+data_size)
	\\ get strings
	a$=rightpart$(a$, "Strings:")
	m=0
	strings=val(a$, "int", m)
	a$=rightpart$(a$, nl$)

	if strings>0 then
		Dim strings$(strings)
		for i=0 to strings-1
			strings$(i)=GetString$(leftpart$(a$, nl$))
			a$=rightpart$(a$, nl$)
		Next i
	End if
	buffer clear code_ as byte*1000
	do
		m=0
		offset=val(a$,"int", m)
		if m<0 then exit
		a$=mid$(a$,m)
		line$=trim$(leftpart$(a$,nl$))
		if line$="" then line$=trim$(a$) else a$=trim$(rightpart$(a$, nl$))
		op$=if$(instr(line$," ")>0->leftpart$(line$," "), line$)
		if not valid(eval(op$+"_")) then exit
		opc=eval(op$+"_")
		Return code_, offset:=opc
		if opc>=store_ then
			line$=rightpart$(line$," ")
			select case opc
			case store_, fetch_
				Return code_, offset+1:=val(rightpart$(leftpart$(line$,"]"),"[")) as long : offset+=4
			case push_
				Return code_, offset+1:=uint(val(line$)) as long : offset+=4
			case jz_, jmp_
				Return code_, offset+1:=val(rightpart$(line$,")")) as long : offset+=4
			end select
		end if
	Always
	Print "Press any key" : Push key$ : Drop
	\\ Prepare VM
	let pc=0, sp=len(stack_) div 4
	do
		b=func(eval(code_, pc))
		pc++
		call local b()
	until exit_now
	Print "done"
}
Virtual_Machine_Interpreter {
Datasize: 1 Strings: 2
"count is: "
"\n"
    0 push  1
    5 store [0]
   10 fetch [0]
   15 push  10
   20 lt
   21 jz     (43) 65
   26 push  0
   31 prts
   32 fetch [0]
   37 prti
   38 push  1
   43 prts
   44 fetch [0]
   49 push  1
   54 add
   55 store [0]
   60 jmp    (-51) 10
   65 halt
}

```



## Perl

Tested with perl v5.26.1

```Perl
#!/usr/bin/perl

# http://www.rosettacode.org/wiki/Compiler/virtual_machine_interpreter
use strict; # vm.pl - run rosetta code
use warnings;
use integer;

my ($binary, $pc, @stack, @data) = ('', 0);

<> =~ /Strings: (\d+)/ or die "bad header";
my @strings = map <> =~ tr/\n""//dr =~ s/\\(.)/$1 eq 'n' ? "\n" : $1/ger, 1..$1;

sub value { unpack 'l', substr $binary, ($pc += 4) - 4, 4 }

my @ops = (
  [ halt  => sub { exit } ],
  [ add   => sub { $stack[-2] += pop @stack } ],
  [ sub   => sub { $stack[-2] -= pop @stack } ],
  [ mul   => sub { $stack[-2] *= pop @stack } ],
  [ div   => sub { $stack[-2] /= pop @stack } ],
  [ mod   => sub { $stack[-2] %= pop @stack } ],
  [ not   => sub { $stack[-1] = $stack[-1] ? 0 : 1 } ],
  [ neg   => sub { $stack[-1] = - $stack[-1] } ],
  [ and   => sub { $stack[-2] &&= $stack[-1]; pop @stack } ],
  [ or    => sub { $stack[-2] ||= $stack[-1]; pop @stack } ],
  [ lt    => sub { $stack[-1] = $stack[-2] <  pop @stack ? 1 : 0 } ],
  [ gt    => sub { $stack[-1] = $stack[-2] >  pop @stack ? 1 : 0 } ],
  [ le    => sub { $stack[-1] = $stack[-2] <= pop @stack ? 1 : 0 } ],
  [ ge    => sub { $stack[-1] = $stack[-2] >= pop @stack ? 1 : 0 } ],
  [ ne    => sub { $stack[-1] = $stack[-2] != pop @stack ? 1 : 0 } ],
  [ eq    => sub { $stack[-1] = $stack[-2] == pop @stack ? 1 : 0 } ],
  [ prts  => sub { print $strings[pop @stack] } ],
  [ prti  => sub { print pop @stack } ],
  [ prtc  => sub { print chr pop @stack } ],
  [ store => sub { $data[value()] = pop @stack } ],
  [ fetch => sub { push @stack, $data[value()] // 0 } ],
  [ push  => sub { push @stack, value() } ],
  [ jmp   => sub { $pc += value() - 4 } ],
  [ jz    => sub { $pc += pop @stack ? 4 : value() - 4 } ],
  );
my %op2n = map { $ops[$_][0], $_ } 0..$#ops;            # map name to op number

while(<>)
  {
  /^ *\d+ +(\w+)/ or die "bad line $_";                 # format error
  $binary .= chr( $op2n{$1} // die "$1 not defined" ) . # op code
    (/\((-?\d+)\)|(\d+)]?$/ and pack 'l', $+);          # 4 byte value
  }

$ops[vec($binary, $pc++, 8)][1]->() while 1;            # run it
```

Passes all tests.


## Phix

Reusing cgen.e from the [[Compiler/code_generator#Phix|Code Generator task]]

```Phix
--
-- demo\rosetta\Compiler\vm.exw
--
### ======================

--
--  Since we have generated executable machine code, the virtual machine, such as it is, is just
--  the higher level implementations of printc/i/s, see setbuiltins() in cgen.e
--  Otherwise the only difference between this and cgen.exw is call(code_mem) instead of decode().
--
--  A quick test (calculating fib(44) 10^6 times) suggests ~500 times faster than interp.exw -
--  which is to be expected given that a single add instruction (1 clock) here is implemented as
--  at least three (and quite possibly five!) resursive calls to interp() in the other.


format PE32
--format ELF32
--  Note: cgen generates 32-bit machine code, which cannot be executed directly from a 64-bit interpreter.
--        You can however, via the magic of either the above format directives, use a 64-bit version of
--        Phix to compile this (just add a -c command line option) to a 32-bit executable, which can.
--        It would not be particularly difficult to emit 32 or 64 bit code, but some source code files
--        would, fairly obviously, then be very nearly twice as long, and a fair bit harder to read.

include cgen.e

procedure main(sequence cl)
    open_files(cl)
    toks = lex()
    object t = parse()
    code_gen(t)
    fixup()
    if machine_bits()=32 then
        -- ^ as per note above
        call(code_mem)
    end if
    free({var_mem,code_mem})
    close_files()
end procedure

--main(command_line())
main({0,0,"deep.c"})
```

{{out}}

```txt

-5
10
15

```



## Python

Tested with Python 2.7 and 3.x

```Python
from __future__ import print_function
import sys, struct

FETCH, STORE, PUSH, ADD, SUB, MUL, DIV, MOD, LT, GT, LE, GE, EQ, NE, AND, OR, NEG, NOT, \
JMP, JZ, PRTC, PRTS, PRTI, HALT = range(24)

code_map = {
    "fetch": FETCH,
    "store": STORE,
    "push":  PUSH,
    "add":   ADD,
    "sub":   SUB,
    "mul":   MUL,
    "div":   DIV,
    "mod":   MOD,
    "lt":    LT,
    "gt":    GT,
    "le":    LE,
    "ge":    GE,
    "eq":    EQ,
    "ne":    NE,
    "and":   AND,
    "or":    OR,
    "not":   NOT,
    "neg":   NEG,
    "jmp":   JMP,
    "jz":    JZ,
    "prtc":  PRTC,
    "prts":  PRTS,
    "prti":  PRTI,
    "halt":  HALT
}

input_file  = None
code        = bytearray()
string_pool = []
word_size   = 4

#*** show error and exit
def error(msg):
    print("%s" % (msg))
    exit(1)

def int_to_bytes(val):
    return struct.pack("<i", val)

def bytes_to_int(bstr):
    return struct.unpack("<i", bstr)

#***
def emit_byte(x):
    code.append(x)

#***
def emit_word(x):
    s = int_to_bytes(x)
    for x in s:
        code.append(x)

#***
def run_vm(data_size):
    stack = [0 for i in range(data_size + 1)]
    pc = 0
    while True:
        op = code[pc]
        pc += 1

        if op == FETCH:
            stack.append(stack[bytes_to_int(code[pc:pc+word_size])[0]]);
            pc += word_size
        elif op == STORE:
            stack[bytes_to_int(code[pc:pc+word_size])[0]] = stack.pop();
            pc += word_size
        elif op == PUSH:
            stack.append(bytes_to_int(code[pc:pc+word_size])[0]);
            pc += word_size
        elif op == ADD:   stack[-2] += stack[-1]; stack.pop()
        elif op == SUB:   stack[-2] -= stack[-1]; stack.pop()
        elif op == MUL:   stack[-2] *= stack[-1]; stack.pop()
        # use C like division semantics
        elif op == DIV:   stack[-2] = int(float(stack[-2]) / stack[-1]); stack.pop()
        elif op == MOD:   stack[-2] = int(float(stack[-2]) % stack[-1]); stack.pop()
        elif op == LT:    stack[-2] = stack[-2] <  stack[-1]; stack.pop()
        elif op == GT:    stack[-2] = stack[-2] >  stack[-1]; stack.pop()
        elif op == LE:    stack[-2] = stack[-2] <= stack[-1]; stack.pop()
        elif op == GE:    stack[-2] = stack[-2] >= stack[-1]; stack.pop()
        elif op == EQ:    stack[-2] = stack[-2] == stack[-1]; stack.pop()
        elif op == NE:    stack[-2] = stack[-2] != stack[-1]; stack.pop()
        elif op == AND:   stack[-2] = stack[-2] and stack[-1]; stack.pop()
        elif op == OR:    stack[-2] = stack[-2] or  stack[-1]; stack.pop()
        elif op == NEG:   stack[-1] = -stack[-1]
        elif op == NOT:   stack[-1] = not stack[-1]
        elif op == JMP:   pc += bytes_to_int(code[pc:pc+word_size])[0]
        elif op == JZ:
            if stack.pop():
                pc += word_size
            else:
                pc += bytes_to_int(code[pc:pc+word_size])[0]
        elif op == PRTC:  print("%c" % (stack[-1]), end=''); stack.pop()
        elif op == PRTS:  print("%s" % (string_pool[stack[-1]]), end=''); stack.pop()
        elif op == PRTI:  print("%d" % (stack[-1]), end=''); stack.pop()
        elif op == HALT:  break

def str_trans(srce):
    dest = ""
    i = 0
    while i < len(srce):
        if srce[i] == '\\' and i + 1 < len(srce):
            if srce[i + 1] == 'n':
                dest += '\n'
                i += 2
            elif srce[i + 1] == '\\':
                dest += '\\'
                i += 2
        else:
            dest += srce[i]
            i += 1

    return dest

#***
def load_code():
    global string_pool

    line = input_file.readline()
    if len(line) == 0:
        error("empty line")

    line_list = line.split()
    data_size = int(line_list[1])
    n_strings = int(line_list[3])

    for i in range(n_strings):
        string_pool.append(str_trans(input_file.readline().strip('"\n')))

    while True:
        line = input_file.readline()
        if len(line) == 0:
            break
        line_list = line.split()
        offset = int(line_list[0])
        instr  = line_list[1]
        opcode = code_map.get(instr)
        if opcode == None:
            error("Unknown instruction %s at %d" % (instr, offset))
        emit_byte(opcode)
        if opcode in [JMP, JZ]:
            p = int(line_list[3])
            emit_word(p - (offset + 1))
        elif opcode == PUSH:
            value = int(line_list[2])
            emit_word(value)
        elif opcode in [FETCH, STORE]:
            value = int(line_list[2].strip('[]'))
            emit_word(value)

    return data_size

#*** main driver
input_file = sys.stdin
if len(sys.argv) > 1:
    try:
        input_file = open(sys.argv[1], "r", 4096)
    except IOError as e:
        error(0, 0, "Can't open %s" % sys.argv[1])

data_size = load_code()
run_vm(data_size)
```



## Scheme


The interpreter uses recursion, representing the stack as a list; the stack pointer is the reference to the top of the list.  This is a more natural solution in Scheme than a fixed stack array, and removes the danger of stack overflow.  Operations on or returning booleans have been adapted to use integers, 0 for false and anything else for true.

All of the "Compiler/Sample programs" are correctly interpreted.


```scheme

(import (scheme base)
        (scheme char)
        (scheme file)
        (scheme process-context)
        (scheme write)
        (only (srfi 13) string-contains string-delete string-filter
              string-replace string-tokenize))

(define *word-size* 4)

;; Mappings from operation symbols to internal procedures.
;; We define operations appropriate to virtual machine:
;; e.g. division must return an int, not a rational
;; boolean values are treated as numbers: 0 is false, other is true
(define *unary-ops*
  (list (cons 'neg (lambda (a) (- a)))
        (cons 'not (lambda (a) (if (zero? a) 1 0)))))
(define *binary-ops*
  (let ((number-comp (lambda (op) (lambda (a b) (if (op a b) 1 0)))))
    (list (cons 'add +)
          (cons 'sub -)
          (cons 'mul *)
          (cons 'div (lambda (a b) (truncate (/ a b)))) ; int division
          (cons 'mod modulo)
          (cons 'lt (number-comp <))
          (cons 'gt (number-comp >))
          (cons 'le (number-comp <=))
          (cons 'ge (number-comp >=))
          (cons 'eq (lambda (a b) (if (= a b) 1 0)))
          (cons 'ne (lambda (a b) (if (= a b) 0 1)))
          (cons 'and (lambda (a b) ; make "and" work on numbers
                       (if (and (not (zero? a)) (not (zero? b))) 1 0)))
          (cons 'or (lambda (a b) ; make "or" work on numbers
                      (if (or (not (zero? a)) (not (zero? b))) 1 0))))))

;; read information from file, returning vectors for data and strings
;; and a list of the code instructions
(define (read-code filename)
  (define (setup-definitions str)
    (values ; return vectors for (data strings) of required size
      (make-vector (string->number (list-ref str 1)) #f)
      (make-vector (string->number (list-ref str 3)) #f)))
  (define (read-strings strings) ; read constant strings into data structure
    (define (replace-newlines chars) ; replace newlines, obeying \\n
      (cond ((< (length chars) 2) ; finished list
             chars)
            ((and (>= (length chars) 3) ; preserve \\n
                  (char=? #\\ (car chars))
                  (char=? #\\ (cadr chars))
                  (char=? #\n (cadr (cdr chars))))
             (cons (car chars)
                   (cons (cadr chars)
                         (cons (cadr (cdr chars))
                               (replace-newlines (cdr (cdr (cdr chars))))))))
            ((and (char=? #\\ (car chars)) ; replace \n with newline
                  (char=? #\n (cadr chars)))
             (cons #\newline (replace-newlines (cdr (cdr chars)))))
            (else ; keep char and look further
              (cons (car chars) (replace-newlines (cdr chars))))))
    (define (tidy-string str) ; remove quotes, map newlines to actual newlines
      (list->string
        (replace-newlines
          (string->list
            (string-delete #\" str))))) ; " (needed to satisfy rosettacode's scheme syntax highlighter)
    ;
    (do ((i 0 (+ i 1)))
      ((= i (vector-length strings)) )
      (vector-set! strings i (tidy-string (read-line)))))
  (define (read-code)
    (define (cleanup-code opn) ; tidy instructions, parsing numbers
      (let ((addr (string->number (car opn)))
            (instr (string->symbol (cadr opn))))
        (cond ((= 2 (length opn))
               (list addr instr))
              ((= 3 (length opn))
               (list addr
                     instr
                     (string->number
                       (string-filter char-numeric? (list-ref opn 2)))))
              (else ; assume length 4, jump instructions
                (list addr instr (string->number (list-ref opn 3)))))))
    ;
    (let loop ((result '()))
      (let ((line (read-line)))
        (if (eof-object? line)
          (reverse (map cleanup-code result))
          (loop (cons (string-tokenize line) result))))))
  ;
  (with-input-from-file
    filename
    (lambda ()
      (let-values (((data strings)
                    (setup-definitions (string-tokenize (read-line)))))
                  (read-strings strings)
                  (values data
                          strings
                          (read-code))))))

;; run the virtual machine
(define (run-program data strings code)
  (define (get-instruction n)
    (if (assq n code)
      (cdr (assq n code))
      (error "Could not find instruction")))
  ;
  (let loop ((stack '())
             (pc 0))
    (let ((op (get-instruction pc)))
      (case (car op)
        ((fetch)
         (loop (cons (vector-ref data (cadr op)) stack)
               (+ pc 1 *word-size*)))
        ((store)
         (vector-set! data (cadr op) (car stack))
         (loop (cdr stack)
               (+ pc 1 *word-size*)))
        ((push)
         (loop (cons (cadr op) stack)
               (+ pc 1 *word-size*)))
        ((add sub mul div mod lt gt le eq ne and or)
         (let ((instr (assq (car op) *binary-ops*)))
           (if instr
             (loop (cons ((cdr instr) (cadr stack) ; replace top two with result
                                      (car stack))
                         (cdr (cdr stack)))
                   (+ pc 1))
             (error "Unknown binary operation"))))
        ((neg not)
         (let ((instr (assq (car op) *unary-ops*)))
           (if instr
             (loop (cons ((cdr instr) (car stack)) ; replace top with result
                         (cdr stack))
                   (+ pc 1))
             (error "Unknown unary operation"))))
        ((jmp)
         (loop stack
               (cadr op)))
        ((jz)
         (loop (cdr stack)
               (if (zero? (car stack))
                 (cadr op)
                 (+ pc 1 *word-size*))))
        ((prtc)
         (display (integer->char (car stack)))
         (loop (cdr stack)
               (+ pc 1)))
        ((prti)
         (display (car stack))
         (loop (cdr stack)
               (+ pc 1)))
        ((prts)
         (display (vector-ref strings (car stack)))
         (loop (cdr stack)
               (+ pc 1)))
        ((halt)
         #t)))))

;; create and run virtual machine from filename passed on command line
(if (= 2 (length (command-line)))
  (let-values (((data strings code) (read-code (cadr (command-line)))))
              (run-program data strings code))
  (display "Error: pass a .asm filename\n"))

```



## zkl

{{trans|Python}}
File rvm.zkl:

```zkl
// This is a little endian machine
const WORD_SIZE=4;
const{ var _n=-1; var[proxy]N=fcn{ _n+=1 } }  // enumerator
const FETCH=N, STORE=N, PUSH=N, ADD=N, SUB=N, MUL=N, DIV=N, MOD=N,
   LT=N, GT=N, LE=N, GE=N, EQ=N, NE=N, AND=N, OR=N, NEG=N, NOT=N,
   JMP=N, JZ=N, PRTC=N, PRTS=N, PRTI=N, HALT=N;

var [const]
   bops=Dictionary(ADD,'+, SUB,'-, MUL,'*, DIV,'/, MOD,'%,
		   LT,'<, GT,'>, LE,'<=, GE,'>=, NE,'!=, EQ,'==, NE,'!=),
   strings=List();  // filled in by the loader
;

   // do a binary op
fcn bop(stack,op){ a,b:=stack.pop(),stack.pop(); stack.append(bops[op](b,a)) }

fcn run_vm(code,stackSz){
   stack,pc := List.createLong(stackSz,0), 0;
   while(True){
      op:=code[pc]; pc+=1;
      switch(op){
         case(FETCH){
	    stack.append(stack[code.toLittleEndian(pc,WORD_SIZE,False)]);
            pc+=WORD_SIZE;
	 }
	 case(STORE){
	    stack[code.toLittleEndian(pc,WORD_SIZE)]=stack.pop();
	    pc+=WORD_SIZE;
	 }
         case(PUSH){
	    stack.append(code.toLittleEndian(pc,WORD_SIZE,False));  // signed
	    pc+=WORD_SIZE;
	 }
	 case(ADD,SUB,MUL,DIV,MOD,LT,GT,LE,GE,EQ,NE) { bop(stack,op) }
	 case(AND){ stack[-2] = stack[-2] and stack[-1]; stack.pop() }
	 case(OR) { stack[-2] = stack[-2] or  stack[-1]; stack.pop() }
	 case(NEG){ stack[-1] = -stack[-1]    }
	 case(NOT){ stack[-1] = not stack[-1] }
	 case(JMP){ pc+=code.toLittleEndian(pc,WORD_SIZE,False); }  // signed
	 case(JZ) {
	    if(stack.pop()) pc+=WORD_SIZE;
	    else            pc+=code.toLittleEndian(pc,WORD_SIZE,False);
	 }
	 case(PRTC){ }	// not implemented
	 case(PRTS){ print(strings[stack.pop()]) }
	 case(PRTI){ print(stack.pop()) }
	 case(HALT){ break }
	 else{ throw(Exception.AssertionError(
		"Bad op code (%d) @%d".fmt(op,pc-1))) }
      }
   }
}

code:=File(vm.nthArg(0)).read();	// binary code file
    // the string table is prepended to the code:
    //    66,1 byte len,text, no trailing '\0' needed
while(code[0]==66){	// read the string table
   sz:=code[1];
   strings.append(code[2,sz].text);
   code.del(0,sz+2);
}
run_vm(code,1000);
```

The binary code file code.bin:
{{out}}

```txt

$ zkl hexDump code.bin
   0: 42 0a 63 6f 75 6e 74 20 | 69 73 3a 20 42 01 0a 02   B.count is: B...
  16: 01 00 00 00 01 00 00 00 | 00 00 00 00 00 00 02 0a   ................
  32: 00 00 00 08 13 2b 00 00 | 00 02 00 00 00 00 15 00   .....+..........
  48: 00 00 00 00 16 02 01 00 | 00 00 15 00 00 00 00 00   ................
  64: 02 01 00 00 00 03 01 00 | 00 00 00 12 cd ff ff ff   ................
  80: 17

```

{{out}}

```txt

$ zkl rvm code.bin
count is: 1
count is: 2
count is: 3
count is: 4
count is: 5
count is: 6
count is: 7
count is: 8
count is: 9

```

