+++
title = "M2000 Compiler Task"
description = ""
date = 2019-07-18T21:53:14Z
aliases = []
[extra]
id = 22156
[taxonomies]
categories = []
tags = []
+++

One module compile, including lexical analyzer, syntax analyzer, code generator and virtual machine interpreter. The VM running using lambda functions, which include in an inventory (a kind of map), so each byte code is a key to this inventory, and the vm loop is just three lines. Normally functions (including lambda functions) and modules have own name space, but we can call functions with a L Call Local statement, which place the same name space of the caller (subroutines get always name space from the caller).
Each module get a string (the (a$) is a syntactic sugar, interpreter insert a Read a$ when module added to modules/function list), and place a string to the stack of values. So we have to put one module after the other to do all the process.

The stack of values internal is a collection, which we can add to top (using Push) or to bottom (using Data) and we can pop values using Read, or peak values using ItemStack() and ItemStack$() for strings. A module get the parent stack of value, a function in an expression has own stack of value. We can switch temporary to a new stack using a Stack New { } statement, and at the end of this the previous object attach to "process object". (modules and functions run on separate process objects, subs are implemented in execution code, they are not objects).

Enum variables can return the name of value using a Eval$(enum_name_or_variable), So we can store values as Enum types, and handle them as numbers and as strings (using Eval$()). Functions Eval() and Eval$() do many things, depends of the first argument.

Generator save the byte code to a byte buffer. In a buffer bytes, integer, long are unsigned numbers. We have to use Uint(a) to place a signed value, and Sint() to get the signed from buffer. Also we can read from bytes offsets using casting to integer or to long, which we done here to get the long value after the operand (like in jmp or in fetch).

lambda functions can get closures as copies. In lexical analyzer the Scanner variable is a pointer to a buffer, so the copy is a reference. Lambda functions are not reference type, but a value type. We can pass a lambda as a closure too.

Select Case is a mystery in M2000. We can have one line after Case or a Block but no blank lines. This happen because interpreter execute code every time from source; Only look ahead to find if an expression is a string or number which is very fast because, strings functions and variables have $ at the end of name. Maybe some day interpreter find a lexical/syntax analyzer, but for this time works as one function which consume source code.

[https://3.bp.blogspot.com/-ZU26mh0qU_M/XLKFwYC6YMI/AAAAAAAAH4k/QHvk586KvtojspWx7buhK7Dbyw5Nj_g-ACLcBGAs/s1600/compout.png Image]


## M2000 Interpreter


```M2000 Interpreter

Module CompilerExample {
	Set Fast !
	Module lexical_analyzer (a$){
	        lim=Len(a$)
	        LineNo=1
	        ColumnNo=1
	        Document Output$
	        Buffer Scanner as Integer*lim
	        Return Scanner, 0:=a$
	        offset=0
	        buffer1$=""
	        flag_rem=true
	        Ahead=lambda Scanner (a$, offset)->{
	                =false
	                Try {
	                        \\ second parameter is the offset in buffer units
	                        \\ third parameter is length in bytes
	                        =Eval$(Scanner, offset,2*len(a$))=a$
	                }
	        }
	        Ahead2=lambda Scanner (a$, offset)->{
	                =false
	                Try {
	                        =Eval$(Scanner, offset,2) ~ a$
	                }
	        }
	        const nl$=chr$(13)+chr$(10), quo$="""", er$="@", Ansi=3
	        Try {
	                Do
	                If Ahead("/*", offset) Then {
	                        offset+=2 :     ColumnNo+=2
	                        While not Ahead("*/", offset)
	                                If Ahead(nl$, offset) Then
	                                        lineNo++: ColumnNo=1 : offset+=2
	                                Else
	                                        offset++ : ColumnNo++
	                                End If
	                                if offset>lim then
	                                        Error "End-of-file in comment. Closing comment characters not found"+er$
	                                End if
	                        End While
	                        offset+=2 : ColumnNo+=2
	                } Else.if Ahead(nl$, offset) Then{
	                        LineNo++: ColumnNo=1
	                        offset+=2
	                } Else.if Ahead(quo$, offset) Then {
	                        Output$=format$("{0::-10}{1::-10} ", LineNo, ColumnNo)
	                        offset++ : ColumnNo++
	                        strin=offset
	                        While not Ahead(quo$, offset)
	                                If Ahead("/", offset) Then
	                                        offset+=2 : ColumnNo+=2
	                                else
	                                        offset++ : ColumnNo++
	                                End if
	                                checkerror()
	                        End While
	                        Output$="String "+quote$(Eval$(Scanner, strin, (offset-strin)*2))+nl$
	                        offset++ : ColumnNo++
	                } Else.if Ahead("'", offset) Then {
	                        Output$=format$("{0::-10}{1::-10} ", LineNo, ColumnNo)
	                        offset++ : ColumnNo++
	                        strin=offset
	                        While not Ahead("'", offset)
	                                If Ahead("/", offset) Then
	                                        offset+=2 : ColumnNo+=2
	                                else
	                                        offset++ : ColumnNo++
	                                End if
	                                checkerror()
	                        End While
	                        lit$=format$(Eval$(Scanner, strin, (offset-strin)*2))
	                        select case len(lit$)
	                        case 1
	                                Output$="Integer "+str$(asc(lit$),0)+nl$
	                        case >1
	                                {Error "Multi-character constant."+er$}
	                        case 0
	                                {Error "Empty character constant."+er$}
	                        end select
	                        offset++ : ColumnNo++
	                } Else.if Ahead2("[a-z]", offset) Then {
	                        strin=offset
	                        Output$=format$("{0::-10}{1::-10} ", LineNo, ColumnNo)
	                        offset++ : ColumnNo++
	                        While Ahead2("[a-zA-Z0-9_]", offset)
	                                        offset++ : ColumnNo++
	                        End While
	                        Keywords(Eval$(Scanner, strin, (offset-strin)*2))
	                } Else.if Ahead2("[0-9]", offset) Then {
	                        strin=offset
	                        Output$=format$("{0::-10}{1::-10} Integer ", LineNo, ColumnNo)
	                        offset++ : ColumnNo++
	                        While Ahead2("[0-9]", offset)
	                                        offset++ : ColumnNo++
	                        End While
	                        if Ahead2("[a-zA-Z_]", offset) then  
	                                {Error " Invalid number. Starts like a number, but ends in non-numeric characters."+er$}
	                        else
	                                Output$=Eval$(Scanner, strin, (offset-strin)*2)+nl$
	                        end if
	                } Else {
	                        Symbols(Eval$(Scanner, Offset, 2))              
	                        offset++ : ColumnNo++
	                }
	                Until offset>=lim
	        }
	        er1$=leftpart$(error$,er$)
	        if er1$<>"" then
	                Print
	                Report "Error:"+er1$
	                Output$="(Error)"+nl$+"Error:"+er1$
	        else
	                Output$=format$("{0::-10}{1::-10}", LineNo, ColumnNo)+" End_of_Input"+nl$
	        end if
	        Push Output$
	        Exit
	        Clipboard Output$
	        Save.Doc Output$, "lex.t", Ansi
	        document lex$
	        Load.Doc lex$,"lex.t", Ansi
	        Report lex$
	        Sub Keywords(a$)
	                select case a$
	                case "if"
	                        a$="Keyword_if"
	                case "else"
	                        a$="Keyword_else"
	                case "while"
	                        a$="Keyword_while"
	                case "print"
	                        a$="Keyword_print"
	                case "putc"
	                        a$="Keyword_putc"
	                else case
	                        a$="Identifier "+a$
	                end select
	                Output$=a$+nl$
	        End sub
	        Sub Symbols(a$)
	                select case a$
	                case " ", chr$(9)
	                        a$=""
	                case "("
	                        a$="LeftParen"
	                case ")"
	                        a$="RightParen"
	                case "{"
	                        a$="LeftBrace"
	                case "}"
	                        a$="RightBrace"
	                case ";"
	                        a$="Semicolon"
	                case ","
	                        a$="Comma"
	                case "*"
	                        a$="Op_multiply"
	                case "/"
	                        a$="Op_divide"
	                case "+"
	                        a$="Op_add"
	                case "-"
	                        a$="Op_subtract"
	                case "%"
	                        a$="Op_mod"
	                case "<"
	                {       if Ahead("=", offset+1) Then
	                                offset++
	                                a$="Op_lessequal"
	                                ColumnNo++
	                        else
	                                a$="Op_less"
	                        end if
	                }
	                case ">"
	                {       if Ahead("=", offset+1) Then
	                                offset++
	                                ColumnNo++
	                                a$="Op_greaterequal"
	                        else
	                                a$="Op_greater"
	                        end if
	                }
	                case "="
	                {       if Ahead("=", offset+1) Then
	                                offset++
	                                ColumnNo++
	                                a$="Op_equal"
	                        else
	                                a$="Op_assign"
	                        end if
	                }
	                case "!"
	                {       if Ahead("=", offset+1) Then
	                                offset++
	                                ColumnNo++
	                                a$="Op_notequal"
	                        else
	                                a$="Op_not"
	                        end if
	                }
	                case "&"
	                {       if Ahead("&", offset+1) Then
	                                offset++
	                                ColumnNo++
	                                a$="Op_and"
	                        else
	                                a$=""
	                        end if
	                }
	                case "|"
	                {       if Ahead("|", offset+1) Then
	                                offset++
	                                ColumnNo++
	                                a$="Op_or"
	                        else
	                                a$=""
	                        end if
	                }
	                else case
	                        {Error "Unrecognized character."+er$}
	                end select
	                if a$<>"" then
	                Output$=format$("{0::-10}{1::-10} ", LineNo, ColumnNo)+a$+nl$
	                end if
	        End Sub
	        Sub checkerror()
	                if offset>lim then {
	                        Error "End-of-line while scanning string literal. Closing string character not found before end-of-line."+er$
	                } else.if  Ahead(nl$,offset) then {
	                        Error "End-of-file while scanning string literal. Closing string character not found."+er$
	                }
	        End Sub
	}
	Module syntax_analyzer (b$){
	        enum tokens {
	                Op_add, Op_subtract, Op_not=5, Op_multiply=10, Op_divide, Op_mod, 
	                Op_negate,  Op_less, Op_lessequal, Op_greater, Op_greaterequal,
	                Op_equal, Op_notequal, Op_and, Op_or, Op_assign=100, Keyword_if=110,
	                Keyword_else, Keyword_while, Keyword_print, Keyword_putc, LeftParen, RightParen,
	                LeftBrace, RightBrace, Semicolon, Comma, Identifier, Integer, String, End_of_input
	        }
	        Inventory precedence=Op_multiply:=13, Op_divide:=13, Op_mod:=13, Op_add:=12, Op_subtract:=12
	        Append  precedence, Op_negate:=14, Op_not:=14, Op_less:=10, Op_lessequal:=10, Op_greater:=10 
	        Append  precedence, Op_greaterequal:=10, Op_equal:=9, Op_notequal:=9, Op_assign:=-1, Op_and:=5
	        Append  precedence, Op_or:=4
	        Inventory symbols=Op_multiply:="Multiply", Op_divide:="Divide", Op_mod:="Mod", Op_add:="Add"
	        Append  symbols, Op_negate:="Negate", Op_not:="Not", Op_less:="Less", Op_subtract:="Subtract"
	        Append  symbols, Op_lessequal:="LessEqual", Op_greater:="Greater", Op_greaterequal:="GreaterEqual"
	        Append  symbols, Op_equal:="Equal", Op_notequal:="NotEqual",  Op_and:="And", Op_or:="Or"  
	        def lineNo, ColumnNo, m, line$, a, lim, cur=-1
	        const nl$=chr$(13)+chr$(10), Ansi=3
	        Dim lex$()
	        lex$()=piece$(b$,chr$(13)+chr$(10)) 
	        lim=dimension(lex$(),1)-1
	        op=End_of_input
	        flush
	        k=0
	        Try {
	                push (,)   ' Null
	                getone(&op)
	                repeat
	                stmt(&op)
	                shift 2  ' swap two top items
	                push ("Sequence", array, array)
	                k++
	                until op=End_of_Input
	        }
	        er$=error$
	        if er$<>"" then print er$ : flush: break
	        Print "Ast"
	        Document Output$
	        prt_ast()
	        Push Output$
	        exit
	        clipboard Output$
	        Save.Doc Output$, "parse.t", Ansi
	        document parse$
	        Load.Doc parse$,"parse.t", Ansi
	        Report parse$
	        sub prt_ast(t)
	                if len(t)<1 then
	                        Output$=";"+nl$
	                else.if len(t)=3 then
	                        Output$=t#val$(0) +nl$
	                        prt_ast(t#val(1)) : prt_ast(t#val(2))
	                else
	                        Output$=t#val$(0) +nl$
	                end if
	        end sub
	        sub expr(p)   ' only a number
	                local x=(,), prev=op
	                if  op>=Identifier then
	                        x=(line$,)
	                        getone(&op)
	                else.if op=LeftParen then
	                        paren_exp()
	                        x=array
	                else.if op<10 then
	                        getone(&op)
	                        expr(precedence(int(Op_negate)))
	                        read local y
	                        if prev=Op_add then
	                                x=y
	                        else
	                                if prev=Op_subtract then prev=Op_negate
	                                x=(symbols(prev), y,(,))
	                        End if
	                else
	                         {error "??? "+eval$(op)}
	                end if
	                local prec
	                while exist(precedence, int(op))
	                        prev=op : prec=eval(precedence)
	                        if prec<14 and prec>=p else exit
	                        getone(&op)
	                        expr(prec+1)  ' all operators are left associative (use prec for right a.)
	                        x=(symbols(int(prev)), x, array)
	                End While
	                Push x
	        end sub
	        sub paren_exp()
	                expected(LeftParen)
	                getone(&op)
	                expr(0)
	                expected(RightParen)
	                getone(&op)
	        end sub
	        sub stmt(&op)
	                local t=(,)
	                if op=Identifier then
	                        t=(line$)
	                        getone(&op)
	                        expected(Op_assign)
	                        getone(&op) 
	                        expr(0)
	                        read local rightnode
	                        Push ("Assign",t,rightnode)
	                        expected(Semicolon)
	                        getone(&op)
	                else.if op=Semicolon then
	                        getone(&op)
	                        Push (";",)
	                else.if op=Keyword_print then
	                        getone(&op)
	                        expected(LeftParen)
	                        repeat
	                                getone(&op)
	                                if op=String then
	                                        Push ("Prts",(line$,),(,))
	                                        getone(&op)
	                                else
	                                        expr(0)
	                                        Push ("Prti", array,(,))
	                                end if
	                                t=("Sequence", t, array)
	                        until op<>Comma
	                        expected(RightParen)
	                        getone(&op)
	                        expected(Semicolon)
	                        getone(&op)
	                        push t
	                else.if op=Keyword_while then
	                        getone(&op)
	                        paren_exp()
	                        stmt(&op)
	                        shift 2
	                        Push ("While",array, array)
	                else.if op=Keyword_if then
	                        getone(&op)
	                        paren_exp()
	                        stmt(&op)
	                        local s2=(,)
	                        if op=Keyword_else then
	                                getone(&op)
	                                stmt(&op)
	                                read s2
	                        end if
	                        shift 2
	                        Push ("If",array ,("If",array,s2))
	                else.if op=Keyword_putc then
	                        getone(&op)
	                        paren_exp()
	                        Push ("Prtc",array,t)
	                        expected(Semicolon)
	                        getone(&op)
	                else.if op=LeftBrace then
	                        Brace()
	                else
	                        error "Unkown Op"       
	                end if
	        end sub
	        Sub Brace()
	                        getone(&op)
	                        while op<>RightBrace and op<>End_of_input
	                                stmt(&op)
	                                t=("Sequence", t, array)
	                        end while
	                        expected(RightBrace)
	                        getone(&op)
	                        push t
	        End Sub
	        Sub expected(what)
	                if not op=what then {Error "Expected "+eval$(what)+str$(LineNo)+","+Str$(ColumnNo)}
	        End Sub
	        sub getone(&op)
	                op=End_of_input
	                while cur<lim
	                cur++
	                line$=trim$(lex$(cur))
	                if line$<>"" then exit
	                end while
	                if cur=lim then exit sub
	                LineNo=Val(line$,"int",m)
	                line$=mid$(line$, m)
	                ColumnNo=Val(line$,"int",m)
	                line$=trim$(mid$(line$, m))
	                Rem : Print LineNo, ColumnNo
	                m=instr(line$," ")
	                if m>0 then op=Eval("."+leftpart$(line$, " ")) else op=Eval("."+line$)
	        end sub
	}
	Module CodeGenerator (s$){
	        Function code$(op$) {
	                =format$("{0::-6} {1}", pc, op$)
	                pc++
	        }
	        Function code2$(op$, n$) {
	                =format$("{0::-6} {1} {2}", pc, op$, n$)
	                pc+=5
	        }
	        Function code3$(op$,pc, st, ed) {
	                =format$("{0::-6} {1} ({2}) {3}", pc, op$, ed-st-1, ed)
	        }
	        Enum tok {
	                gneg, gnot, gmul, gdiv, gmod, gadd, gle, gsub, glt
	                gle, ggt, gge, geq, gne, gand, gor, gprtc, gprti, gprts,
	                gif, gwhile, gAssign, gSeq, gstring, gidentifier, gint, gnone
	        }
 	        \\ Inventories are lists with keys, or keys/data (key must be unique)
	        \\ there is one type more the Invetory Queue which get same keys.
	        \\ But here not used.
	        Inventory symb="Multiply":=gmul, "Divide":=gdiv, "Mod":=gmod, "Add":=gadd
	        Append  symb, "Negate":=gneg, "Not":=gnot,"Less":=glt,"Subtract":=gsub
	        Append  symb, "LessEqual":=gle, "Greater":=ggt, "GreaterEqual":=gge, "Sequence":=gSeq
	        Append  symb, "Equal":=geq, "NotEqual":=gne,  "And":=gand, "Or":=gor, "While":=gwhile
	        Append  symb, "Prtc":=gprtc,"Prti":=gprti,"Prts":=gprts, "Assign":=gAssign, "If":=gif
	        Append  symb, "String":=gstring, "Identifier":=gidentifier, "Integer":=gint, ";", gnone 
	        Inventory DataSet
	        \\ We set string as key. key maybe an empty string, a string or a number.
	        \\ so we want eash string to saved one time only.
	        Inventory Strings
 	        Const nl$=chr$(13)+chr$(10), Ansi=3
	        Def z$, lim, line$, newvar_ok, i=0
	        Document message$=nl$
	        Global pc     \\ functions have own scope, so we make it global, for this module, and childs.
 	        Dim lines$()
	        s$=filter$(s$,chr$(9))   \\ exclude tabs
	        Lines$()=piece$(s$,nl$) \\ break to lines
	        lim=len(Lines$())
	        Flush ' empty stack (there is a current stack of values which we use here)
	        Load_Ast()
	        If not stack.size=1 Then Flush : Error "Ast not loaded"
	        AST=array   \\ pop the array from stack
	        Document Assembly$, Header$
	        \\ all lines of assembly goes to stack. Maybe not in right order.
	        \\ Push statement push to top, Data statement push to bottom of stack
	        CodeGenerator(Ast)
	        Data  code$("halt") ' append to end of stack
	        \\ So now we get all data (letters) from stack
	        While not empty
	                Assembly$=letter$+nl$
	        end while
	        \\ So now we have to place them in order
	        Sort Assembly$
	        \\ Let's make the header
	        Header$=format$("Datasize: {0} Strings: {1}", Len(Dataset),Len(strings))
	        \\ we use an iterator object, str^ is the counter, readonly, but Eval$() use it from object.
	        str=each(strings)    
	        While str
	                Header$=nl$+Eval$(str)
	        End while
	        Assembly$=nl$
	        \\ insert to line 1 the Header
	        Insert 1 Assembly$=Header$
	        \\ Also we check for warnings
	        If len(message$)>2 then Assembly$="Warnings: "+nl$+message$
	        \\ So now we get a report
	        \\ (at each 3/4 of window's lines, the printing stop and wait for user response, any key)
	        Push Assembly$
	        Exit
	        Report Assembly$
	        Clipboard Assembly$
	        Save.Doc Assembly$, "code.t", Ansi
	        End
	        \\ subs have 10000 limit for recursion but can be extended to 1000000 or more.
	        Sub CodeGenerator(t)
	                If len(t)=3 then
	                        select case  t#val(0)
	                        Case gSeq
	                                CodeGenerator(t#val(1)) : CodeGenerator(t#val(2))
	                        Case gwhile
	                        {
	                                local spc=pc
	                                CodeGenerator(t#val(1)) 
	                                local pc1=pc
	                                pc+=5 ' room for jz
	                                CodeGenerator(t#val(2))
	                                data code3$("jz",pc1, pc1, pc+5)
	                                data code3$("jmp",pc,  pc, spc)
	                                pc+=5  ' room for jmp
	                        }
	                        Case gif
	                        {
	                                CodeGenerator(t#val(1)) 
	                                local pc1=pc, pc2
	                                pc+=5
	                                CodeGenerator(t#val(2)#val(1)) 
	                                If len(t#val(2)#val(2))>0 then
	                                        pc2=pc
	                                        pc+=5
	                                        data code3$("jz",pc1, pc1, pc)
	                                        CodeGenerator(t#val(2)#val(2))
	                                        data code3$("jmp",pc2, pc2, pc)
	                                else
	                                        data code3$("jz",pc1, pc1, pc)
	                                end If          
	                        }
	                        Case gAssign
	                        {
	                                CodeGenerator(t#val(2))
	                                local newvar_ok=true
	                                CodeGenerator(t#val(1))
	                        }
	                        case gneg to gnot, gprtc to gprts
	                                CodeGenerator(t#val(1)) : data code$(mid$(eval$(t#val(0)),2))
	                        case gmul to gor
	                        {
	                                CodeGenerator(t#val(1))
	                                CodeGenerator(t#val(2))
	                                data code$(mid$(eval$(t#val(0)),2))
	                        }
	                        End select
	                Else.if len(t)=2 then
	                        select case  t#val(0)
	                        Case gString
	                        {
	                                local spos
	                                If exist(strings,t#val$(1)) then
	                                        spos=eval(strings!)
	                                else
	                                        append strings, t#val$(1)               
	                                        spos=len(strings)-1
	                                end If
	                                Push code2$("push",str$(spos,0))
	                        }
	                        Case gInt
	                                Push code2$("push",t#val$(1), pc)
	                        Case gIdentifier
	                        {
	                                local ipos
	                                If exist(dataset,t#val$(1)) then
	                                        ipos=Eval(dataset!)  ' return position
	                                else.if newvar_ok then
	                                        Append dataset, t#val$(1)
	                                        ipos=len(dataset)-1
	                                else
	                                        message$="Variable "+t#val$(1)+" not initialized"+nl$
	                                end If
	                                If newvar_ok then
	                                        Push code2$("store","["+str$(ipos, 0)+"]")
	                                else
	                                        Push code2$("fetch","["+str$(ipos, 0)+"]")
	                                end If
	                        }
	                        end select
	                End If
	        End Sub
	        Sub Load_Ast()
	                        If i>=lim then Push (,) : exit sub
	                        do
	                        line$=Trim$(lines$(i))
	                        I++
	                        tok$=piece$(line$," ")(0)
	                        until line$<>"" or i>=lim
	                        If tok$="Identifier" then
	                                Push (gidentifier,trim$(Mid$(line$,11)))
	                        else.if tok$="Integer" then
	                                long n=Val(Mid$(line$,8))  ' check overflow
	                                Push (gint, Trim$(Mid$(line$,8)))
	                        else.if tok$="String" then
	                                Push (gstring,Trim$(Mid$(line$,7)))
	                        else.if tok$=";" then
	                                Push (,)
	                        Else
	                                local otok=symb(tok$)
	                                Load_Ast() 
	                                Load_Ast()
	                                Shift 2
	                                Push (otok,array, array)
	                        End If
	        End Sub
	}
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
		Append  func, prts_:=lambda->{Print #-2, string$(eval(stack_,sp));: Refresh:sp++}
		Append  func, prti_:=lambda->{Print #-2, str$(sint(eval(stack_,sp)),0);: Refresh:sp++}
		Append  func, prtc_:=lambda->{Print #-2, chrcode$(eval(stack_,sp));: Refresh:sp++}
		Rem : Form 120, 60 ' change console width X height to run Ascii Mandlebrot example
		\\ change Report with Print #-2,   (report stop when scrolling 3/4 of height of console, waiting key or mouse key to continue)
		Print #-2,  "Virtual Assembly Code:"+{
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
	Push {
            	{
            	        /*
            	        This is an integer ascii Mandelbrot generator
            	        */
            	        left_edge= -420;
            	        right_edge=300;
            	        top_edge=300;
            	        bottom_edge = -300;
            	        x_step=7;
            	        y_step=15;           
            	        max_iter=200;
            	        y0 = top_edge;
            	        while (y0 > bottom_edge) {
            	                x0 = left_edge;
            	                while (x0 < right_edge) {
            	                        y = 0;
            	                        x = 0;
            	                        the_char = ' ';
            	                        i = 0;
            	                        while (i < max_iter) {
            	                                x_x = (x * x) / 200;
            	                                y_y = (y * y) / 200;
            	                                if (x_x + y_y > 800 ) {
            	                                        the_char = '0' + i;
            	                                        if (i > 9) {
            	                                                the_char = '@';
            	                                        }
            	                                        i = max_iter;
            	                                }
            	                                y = x * y / 100 + y0;
            	                                x = x_x - y_y + x0;
            	                                i = i + 1;
            	                        }
            	                        putc(the_char);
            	                        x0 = x0 + x_step;
            	                }
            	                putc('\n');
            	                y0 = y0 - y_step;
            	        }
            	}
	} 
 	Form ! 120, 60
	Refresh
	Print "Lexical Analyzer" : Refresh
	lexical_analyzer
	Print "Syntaxl Analyzer" : Refresh
	syntax_analyzer
	Print "Code Generator" : Refresh
	CodeGenerator
	Virtual_Machine_Interpreter
	Set Fast 'restore speed setting
}
CompilerExample

```

