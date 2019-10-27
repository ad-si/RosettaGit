+++
title = "Execute Brain****/Elena"
description = ""
date = 2019-03-15T10:02:02Z
aliases = []
[extra]
id = 16687
[taxonomies]
categories = []
tags = []
+++


```elena
import system'collections;
import system'routines;
import system'dynamic;
 
import extensions;
import extensions'scripting;
import extensions'dynamic'expressions;
 
class TapeAssembler
{    
    Stack                theBrackets;
    List<TapeExpression> theTape;
 
    constructor()
    {
        theBrackets := new Stack();
        theTape := new List<TapeExpression>();
 
        theTape.append(TapeExpression.Declaring("ptr"));
        theTape.append(TapeExpression.Assigning("ptr", TapeExpression.Constant(0)))
    }
 
    constructor(assembly_program)
        <= ()
    {
        assembly_program(self)
    }    
 
    open()
    {
        theBrackets.push(theTape);
        theTape := new List<TapeExpression>()
    }
 
    close()
    {
        var loop := TapeExpression.Loop(
                          TapeExpression.MessageCall(
                             TapeExpression.MessageCall(
                                TapeExpression.Variable("tape"),
                                "at",
                                TapeExpression.Variable("ptr")
                             ),
                             "notequal",
                             TapeExpression.Constant($0)),
                          TapeExpression.Code(theTape.Value));
 
        theTape := theBrackets.pop();
        theTape.append(loop)                
    }
 
    input()
    {
        theTape.append(TapeExpression.MessageCall(
                        TapeExpression.Variable("tape"),
                        "setAt",
                        TapeExpression.Variable("ptr"),
                        TapeExpression.MessageCall(
                           TapeExpression.Constant(console),
                           "readChar"
                        )))
    }
 
    output()
    {
        theTape.append(TapeExpression.MessageCall(
                                 TapeExpression.Constant(console), 
                                 "write",
                                 TapeExpression.MessageCall(
                                   TapeExpression.Variable("tape"),
                                   "at",
                                   TapeExpression.Variable("ptr")
                                 )))
    }
 
    next()
    {
        theTape.append(TapeExpression.Assigning(
            "ptr",
            TapeExpression.MessageCall(
                TapeExpression.Variable("ptr"),
                "add",
                TapeExpression.Constant(1))))
    }
 
    previous()
    {
        theTape.append(TapeExpression.Assigning(
            "ptr",
            TapeExpression.MessageCall(
                TapeExpression.Variable("ptr"),
                "subtract",
                TapeExpression.Constant(1))))
    }
 
    increase()
    {
        theTape.append(TapeExpression.MessageCall(
                                TapeExpression.Variable("tape"),
                                "setAt",
                                TapeExpression.Variable("ptr"), 
                                TapeExpression.MessageCall(
                                    TapeExpression.Constant(CharValue),
                                    "load",
                                    TapeExpression.MessageCall(
                                        TapeExpression.MessageCall(
                                            TapeExpression.Constant(convertor),
                                            "toInt",
                                            TapeExpression.MessageCall(
                                                TapeExpression.Variable("tape"),
                                                "at",
                                                TapeExpression.Variable("ptr"))
                                        ),
                                        "add",
                                        TapeExpression.Constant(1)))))
    }
 
    decrease()
    {
        theTape.append(TapeExpression.MessageCall(
                                TapeExpression.Variable("tape"),
                                "setAt",
                                TapeExpression.Variable("ptr"), 
                                TapeExpression.MessageCall(
                                    TapeExpression.Constant(CharValue),
                                    "load",
                                    TapeExpression.MessageCall(
                                        TapeExpression.MessageCall(
                                            TapeExpression.Constant(convertor),
                                            "toInt",
                                            TapeExpression.MessageCall(
                                                TapeExpression.Variable("tape"),
                                                "at",
                                                TapeExpression.Variable("ptr"))
                                        ),
                                        "subtract",
                                        TapeExpression.Constant(1)))))
 
    }
 
    get()
    {
        var program := TapeExpression.Singleton(
                TapeExpression.Method(
                   "eval",
                   TapeExpression.Code(theTape.Value),
                   TapeExpression.Parameter("tape")));
 
        var o := (program.compiled())();
 
        ^(tape){ o.eval(tape) }
    }
}
 
const bf_program = "++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.";
 
public program()
{
    console.writeLine:bf_program;
 
    var bfAssemblyProgram := new ScriptEngine()
        .loadPath:"asmrules.es"
        .eval(bf_program);
 
    var bfProgram := new TapeAssembler(bfAssemblyProgram).get();
 
    var bfTape := Array.allocate(1024).populate:(int n => $0);
 
    bfProgram(bfTape)
}
```

The grammar:

```elena
[[
   #grammar transform
   #grammar cf

   #define start      ::= <= ( > => commands <= " * system'dynamic'ClosureTape= " # ) =>;

   #define commands   ::= command commands;
   #define commands   ::= comment commands;
   #define commands   ::= $eof;

   #define command    ::= <= += " %""output[0]"" system'dynamic'MessageClosure ^""new[1]"" " => ".";
   #define command    ::= <= += " %""input[0]"" system'dynamic'MessageClosure ^""new[1]"" " => ",";
   #define command    ::= <= += " %""previous[0]"" system'dynamic'MessageClosure ^""new[1]"" " => "<";
   #define command    ::= <= += " %""next[0]"" system'dynamic'MessageClosure ^""new[1]"" " => ">";
   #define command    ::= <= += " %""increase[0]"" system'dynamic'MessageClosure ^""new[1]"" " => "+";
   #define command    ::= <= += " %""decrease[0]"" system'dynamic'MessageClosure ^""new[1]"" " => "-";
   #define command    ::= <= += " %""open[0]"" system'dynamic'MessageClosure ^""new[1]"" " => "[";
   #define command    ::= <= += " %""close[0]"" system'dynamic'MessageClosure ^""new[1]"" " => "]";

   #define comment    ::= " " comments;
   #define comment    ::= "'" comments;
   #define comment    ::= "!" comments;
   #define comment    ::= $eol;

   #define comments   ::= $chr comments;
   #define comments   ::= $eps;

   #mode symbolic;
]]
```

{{out}}

```txt

ELENA VM 4.0.14 (C)2005-2019 by Alex Rakov
Initializing...
Done...
++++++++++[>+++++++>++++++++++>+++>+<<<<-]>++.>+.+++++++..+++.>++.<<+++++++++++++++.>.+++.------.--------.>+.>.
Hello World!

```

