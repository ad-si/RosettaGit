+++
title = "Arithmetic evaluation/Pascal"
description = ""
date = 2010-02-06T12:27:12Z
aliases = []
[extra]
id = 4822
[taxonomies]
categories = []
tags = []
+++

{{works with|GNU Pascal|20060325, based on gcc-3.4.4}}

Note: This code is completely standard pascal, checked with <tt>gpc --classic-pascal</tt>. It uses certain features of standard Pascal which are not implemented in all Pascal compilers (e.g. the code will not compile with Turbo/Borland Pascal or Free Pascal).


```pascal
program calculator(input, output);

type
 NodeType = (binop, number, error);

 pAstNode = ^tAstNode;
 tAstNode = record
             case typ: NodeType of
              binop:
              (
                operation: char;
                first, second: pAstNode;
              );
              number:
               (value: integer);
              error:
               ();
            end;

function newBinOp(op: char; left: pAstNode): pAstNode;
 var
  node: pAstNode;
 begin
  new(node, binop);
  node^.operation := op;
  node^.first := left;
  node^.second := nil;
  newBinOp := node;
 end;

procedure disposeTree(tree: pAstNode);
 begin
  if tree^.typ = binop
   then
    begin
     if (tree^.first <> nil)
      then
       disposeTree(tree^.first);
     if (tree^.second <> nil)
      then
       disposeTree(tree^.second)
    end;
  dispose(tree);
 end;

procedure skipWhitespace(var f: text);
 var
  ch:char;
 function isWhite: boolean;
  begin
   isWhite := false;
   if not eoln(f)
    then
     if f^ = ' '
      then
       isWhite := true
  end;
 begin
  while isWhite do
   read(f, ch)
 end;

function parseAddSub(var f: text): pAstNode; forward;
function parseMulDiv(var f: text): pAstNode; forward;
function parseValue(var f: text): pAstNode; forward;

function parseAddSub;
 var
  node1, node2: pAstNode;
  continue: boolean;
 begin
  node1 := parseMulDiv(f);
  if node1^.typ <> error
   then
    begin
     continue := true;
     while continue and not eoln(f) do
      begin
       skipWhitespace(f);
       if f^ in ['+', '-']
        then
         begin
          node1 := newBinop(f^, node1);
          get(f);
          node2 := parseMulDiv(f);
          if (node2^.typ = error)
           then
            begin
             disposeTree(node1);
             node1 := node2;
             continue := false
            end
           else
            node1^.second := node2
         end
        else
         continue := false
      end;
    end;
  parseAddSub := node1;
 end;

function parseMulDiv;
 var
  node1, node2: pAstNode;
  continue: boolean;
 begin
  node1 := parseValue(f);
  if node1^.typ <> error
   then
    begin
     continue := true;
     while continue and not eoln(f) do
      begin
       skipWhitespace(f);
       if f^ in ['*', '/']
        then
         begin
          node1 := newBinop(f^, node1);
          get(f);
          node2 := parseValue(f);
          if (node2^.typ = error)
           then
            begin
             disposeTree(node1);
             node1 := node2;
             continue := false
            end
           else
            node1^.second := node2
         end
        else
         continue := false
      end;
    end;
  parseMulDiv := node1;
 end;

function parseValue;
 var
  node:  pAstNode;
  value: integer;
  neg:   boolean;
 begin
  node := nil;
  skipWhitespace(f);
  if f^ = '('
   then
    begin
     get(f);
     node := parseAddSub(f);
     if node^.typ <> error
      then
       begin
        skipWhitespace(f);
        if f^ = ')'
         then
          get(f)
         else
          begin
           disposeTree(node);
           new(node, error)
          end
       end
    end
   else if f^ in ['0' .. '9', '+', '-']
    then
     begin
      neg := f^ = '-';
      if f^ in ['+', '-']
       then
        get(f);
      value := 0;
      if f^ in ['0' .. '9']
       then
        begin
         while f^ in ['0' .. '9'] do
          begin
           value := 10 * value + (ord(f^) - ord('0'));
           get(f)
          end;
         new(node, number);
         if (neg)
          then
           node^.value := -value
          else
           node^.value := value
        end
     end;
  if node = nil
   then
    new(node, error);
  parseValue := node
 end;

function eval(ast: pAstNode): integer;
 begin
  with ast^ do
   case typ of
    number: eval := value;
    binop:
     case operation of
      '+': eval := eval(first) + eval(second);
      '-': eval := eval(first) - eval(second);
      '*': eval := eval(first) * eval(second);
      '/': eval := eval(first) div eval(second);
     end;
    error:
     writeln('Oops! Program is buggy!')
   end
 end;

procedure ReadEvalPrintLoop;
 var
  ast: pAstNode;
 begin
  while not eof do
   begin
    ast := parseAddSub(input);
    if (ast^.typ = error) or not eoln
     then
      writeln('Error in expression.')
     else
      writeln('Result: ', eval(ast));
    readln;
    disposeTree(ast)
   end
 end;

begin
 ReadEvalPrintLoop
end.
```

