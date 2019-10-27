+++
title = "Parse EBNF"
description = ""
date = 2019-05-17T03:35:13Z
aliases = []
[extra]
id = 7506
[taxonomies]
categories = []
tags = []
+++

{{clarified-review}}{{draft task}}[[Category:Recursion]]
Write a program that can parse a grammar in Extended Backus–Naur Form and then parse something else according to the grammar. The program is only required to decide whether or not the something else belongs to the language described by the grammar, but for extra credit, it can output a syntax tree. See [[Parse EBNF/Tests|the tests]].


## Go

{{trans|Phix}}


A more or less faithful translation except that indices are 0-based rather than 1-based and so 1 less than in the Phix results.

```go
package main

import (
    "fmt"
    "strings"
)

// type aliases for Phix types
type object = interface{}
type sequence = []object

var (
    src         []rune
    ch          rune
    sdx         int
    token       object
    isSeq       bool
    err         = false
    idents      []string
    ididx       []int
    productions []sequence
    extras      sequence
    results     = [2]string{"pass", "fail"}
)

func btoi(b bool) int {
    if b {
        return 1
    }
    return 0
}

func invalid(msg string) int {
    err = true
    fmt.Println(msg)
    sdx = len(src) // set to eof
    return -1
}

func skipSpaces() {
    for {
        if sdx >= len(src) {
            break
        }
        ch = src[sdx]
        if strings.IndexRune(" \t\r\n", ch) == -1 {
            break
        }
        sdx++
    }
}

func getToken() {
    // Yields a single character token, one of {}()[]|=.;
    // or {"terminal",string} or {"ident", string} or -1.
    skipSpaces()
    if sdx >= len(src) {
        token = -1
        isSeq = false
        return
    }
    tokstart := sdx
    if strings.IndexRune("{}()[]|=.;", ch) >= 0 {
        sdx++
        token = ch
        isSeq = false
    } else if ch == '"' || ch == '\'' {
        closech := ch
        for tokend := sdx + 1; tokend < len(src); tokend++ {
            if src[tokend] == closech {
                sdx = tokend + 1
                token = sequence{"terminal", string(src[tokstart+1 : tokend])}
                isSeq = true
                return
            }
        }
        token = invalid("no closing quote")
        isSeq = false
    } else if ch >= 'a' && ch <= 'z' {
        // To simplify things for the purposes of this task,
        // identifiers are strictly a-z only, not A-Z or 1-9.
        for {
            sdx++
            if sdx >= len(src) {
                break
            }
            ch = src[sdx]
            if ch < 'a' || ch > 'z' {
                break
            }
        }
        token = sequence{"ident", string(src[tokstart:sdx])}
        isSeq = true
    } else {
        token = invalid("invalid ebnf")
        isSeq = false
    }
}

func matchToken(ch rune) {
    if token != ch {
        token = invalid(fmt.Sprintf("invalid ebnf (%c expected)", ch))
        isSeq = false
    } else {
        getToken()
    }
}

func addIdent(ident string) int {
    k := -1
    for i, id := range idents {
        if id == ident {
            k = i
            break
        }
    }
    if k == -1 {
        idents = append(idents, ident)
        k = len(idents) - 1
        ididx = append(ididx, -1)
    }
    return k
}

func factor() object {
    var res object
    if isSeq {
        t := token.([]object)
        if t[0] == "ident" {
            idx := addIdent(t[1].(string))
            t = append(t, idx)
            token = t
        }
        res = token
        getToken()
    } else if token == '[' {
        getToken()
        res = sequence{"optional", expression()}
        matchToken(']')
    } else if token == '(' {
        getToken()
        res = expression()
        matchToken(')')
    } else if token == '{' {
        getToken()
        res = sequence{"repeat", expression()}
        matchToken('}')
    } else {
        panic("invalid token in factor() function")
    }
    if s, ok := res.(sequence); ok && len(s) == 1 {
        return s[0]
    }
    return res
}

func term() object {
    res := sequence{factor()}
    tokens := []object{-1, '|', '.', ';', ')', ']', '}'}
outer:
    for {
        for _, t := range tokens {
            if t == token {
                break outer
            }
        }
        res = append(res, factor())
    }
    if len(res) == 1 {
        return res[0]
    }
    return res
}

func expression() object {
    res := sequence{term()}
    if token == '|' {
        res = sequence{"or", res[0]}
        for token == '|' {
            getToken()
            res = append(res, term())
        }
    }
    if len(res) == 1 {
        return res[0]
    }
    return res
}

func production() object {
    // Returns a token or -1; the real result is left in 'productions' etc,
    getToken()
    if token != '}' {
        if token == -1 {
            return invalid("invalid ebnf (missing closing })")
        }
        if !isSeq {
            return -1
        }
        t := token.(sequence)
        if t[0] != "ident" {
            return -1
        }
        ident := t[1].(string)
        idx := addIdent(ident)
        getToken()
        matchToken('=')
        if token == -1 {
            return -1
        }
        productions = append(productions, sequence{ident, idx, expression()})
        ididx[idx] = len(productions) - 1
    }
    return token
}

func parse(ebnf string) int {
    // Returns +1 if ok, -1 if bad.
    fmt.Printf("parse:\n%s ===>\n", ebnf)
    err = false
    src = []rune(ebnf)
    sdx = 0
    idents = idents[:0]
    ididx = ididx[:0]
    productions = productions[:0]
    extras = extras[:0]
    getToken()
    if isSeq {
        t := token.(sequence)
        t[0] = "title"
        extras = append(extras, token)
        getToken()
    }
    if token != '{' {
        return invalid("invalid ebnf (missing opening {)")
    }
    for {
        token = production()
        if token == '}' || token == -1 {
            break
        }
    }
    getToken()
    if isSeq {
        t := token.(sequence)
        t[0] = "comment"
        extras = append(extras, token)
        getToken()
    }
    if token != -1 {
        return invalid("invalid ebnf (missing eof?)")
    }
    if err {
        return -1
    }
    k := -1
    for i, idx := range ididx {
        if idx == -1 {
            k = i
            break
        }
    }
    if k != -1 {
        return invalid(fmt.Sprintf("invalid ebnf (undefined:%s)", idents[k]))
    }
    pprint(productions, "productions")
    pprint(idents, "idents")
    pprint(ididx, "ididx")
    pprint(extras, "extras")
    return 1
}

// Adjusts Go's normal printing of slices to look more like Phix output.
func pprint(ob object, header string) {
    fmt.Printf("\n%s:\n", header)
    pp := fmt.Sprintf("%q", ob)
    pp = strings.Replace(pp, "[", "{", -1)
    pp = strings.Replace(pp, "]", "}", -1)
    pp = strings.Replace(pp, " ", ", ", -1)
    for i := 0; i < len(idents); i++ {
        xs := fmt.Sprintf(`'\x%02d'`, i)
        is := fmt.Sprintf("%d", i)
        pp = strings.Replace(pp, xs, is, -1)
    }
    fmt.Println(pp)
}

// The rules that applies() has to deal with are:
// {factors} - if rule[0] is not string,
// just apply one after the other recursively.
// {"terminal", "a1"}       -- literal constants
// {"or", <e1>, <e2>, ...}  -- (any) one of n
// {"repeat", <e1>}         -- as per "{}" in ebnf
// {"optional", <e1>}       -- as per "[]" in ebnf
// {"ident", <name>, idx}   -- apply the sub-rule

func applies(rule sequence) bool {
    wasSdx := sdx // in case of failure
    r1 := rule[0]
    if _, ok := r1.(string); !ok {
        for i := 0; i < len(rule); i++ {
            if !applies(rule[i].(sequence)) {
                sdx = wasSdx
                return false
            }
        }
    } else if r1 == "terminal" {
        skipSpaces()
        r2 := []rune(rule[1].(string))
        for i := 0; i < len(r2); i++ {
            if sdx >= len(src) || src[sdx] != r2[i] {
                sdx = wasSdx
                return false
            }
            sdx++
        }
    } else if r1 == "or" {
        for i := 1; i < len(rule); i++ {
            if applies(rule[i].(sequence)) {
                return true
            }
        }
        sdx = wasSdx
        return false
    } else if r1 == "repeat" {
        for applies(rule[1].(sequence)) {
        }
    } else if r1 == "optional" {
        applies(rule[1].(sequence))
    } else if r1 == "ident" {
        i := rule[2].(int)
        ii := ididx[i]
        if !applies(productions[ii][2].(sequence)) {
            sdx = wasSdx
            return false
        }
    } else {
        panic("invalid rule in applies() function")
    }
    return true
}

func checkValid(test string) {
    src = []rune(test)
    sdx = 0
    res := applies(productions[0][2].(sequence))
    skipSpaces()
    if sdx < len(src) {
        res = false
    }
    fmt.Printf("%q, %s\n", test, results[1-btoi(res)])
}

func main() {
    ebnfs := []string{
        `"a" {
    a = "a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ;
} "z" `,
        `{
    expr = term { plus term } .
    term = factor { times factor } .
    factor = number | '(' expr ')' .
 
    plus = "+" | "-" .
    times = "*" | "/" .
 
    number = digit { digit } .
    digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
}`,
        `a = "1"`,
        `{ a = "1" ;`,
        `{ hello world = "1"; }`,
        `{ foo = bar . }`,
    }

    tests := [][]string{
        {
            "a1a3a4a4a5a6",
            "a1 a2a6",
            "a1 a3 a4 a6",
            "a1 a4 a5 a6",
            "a1 a2 a4 a5 a5 a6",
            "a1 a2 a4 a5 a6 a7",
            "your ad here",
        },
        {
            "2",
            "2*3 + 4/23 - 7",
            "(3 + 4) * 6-2+(4*(4))",
            "-2",
            "3 +",
            "(4 + 3",
        },
    }

    for i, ebnf := range ebnfs {
        if parse(ebnf) == +1 {
            fmt.Println("\ntests:")
            for _, test := range tests[i] {
                checkValid(test)
            }
        }
        fmt.Println()
    }
}
```


{{out}}

```txt

parse:
"a" {
    a = "a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ;
} "z"  ===>

productions:
{{"a", 0, {{"terminal", "a1"}, {"or", {"terminal", "a2"}, {"terminal", "a3"}}, {"repeat", {"terminal", "a4"}}, {"optional", {"terminal", "a5"}}, {"terminal", "a6"}}}}

idents:
{"a"}

ididx:
{0}

extras:
{{"title", "a"}, {"comment", "z"}}

tests:
"a1a3a4a4a5a6", pass
"a1 a2a6", pass
"a1 a3 a4 a6", pass
"a1 a4 a5 a6", fail
"a1 a2 a4 a5 a5 a6", fail
"a1 a2 a4 a5 a6 a7", fail
"your ad here", fail

parse:
{
    expr = term { plus term } .
    term = factor { times factor } .
    factor = number | '(' expr ')' .
 
    plus = "+" | "-" .
    times = "*" | "/" .
 
    number = digit { digit } .
    digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
} ===>

productions:
{{"expr", 0, {{"ident", "term", 1}, {"repeat", {{"ident", "plus", 2}, {"ident", "term", 1}}}}}, {"term", 1, {{"ident", "factor", 3}, {"repeat", {{"ident", "times", 4}, {"ident", "factor", 3}}}}}, {"factor", 3, {"or", {"ident", "number", 5}, {{"terminal", "("}, {"ident", "expr", 0}, {"terminal", ")"}}}}, {"plus", 2, {"or", {"terminal", "+"}, {"terminal", "-"}}}, {"times", 4, {"or", {"terminal", "*"}, {"terminal", "/"}}}, {"number", 5, {{"ident", "digit", 6}, {"repeat", {"ident", "digit", 6}}}}, {"digit", 6, {"or", {"terminal", "0"}, {"terminal", "1"}, {"terminal", "2"}, {"terminal", "3"}, {"terminal", "4"}, {"terminal", "5"}, {"terminal", "6"}, {"terminal", "7"}, {"terminal", "8"}, {"terminal", "9"}}}}

idents:
{"expr", "term", "plus", "factor", "times", "number", "digit"}

ididx:
{0, 1, 3, 2, 4, 5, 6}

extras:
{}

tests:
"2", pass
"2*3 + 4/23 - 7", pass
"(3 + 4) * 6-2+(4*(4))", pass
"-2", fail
"3 +", fail
"(4 + 3", fail

parse:
a = "1" ===>
invalid ebnf (missing opening {)

parse:
{ a = "1" ; ===>
invalid ebnf (missing closing })

parse:
{ hello world = "1"; } ===>
invalid ebnf (= expected)

parse:
{ foo = bar . } ===>
invalid ebnf (undefined:bar)

```



## Haskell

We use Parsec to generate Parsec.


```haskell
import Control.Applicative
import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import System.Environment (getArgs)
import Text.Parsec hiding (many, optional, (<|>))
import Text.Parsec.String
import Text.Parsec.Error

-----------------------------------------------------------------
-- Main
-----------------------------------------------------------------

main = do
{- Uses the EBNF grammar contained in the first file to parse
the second file, then prints a parse tree. -}
    [grammar_file, other_file] <- getArgs
    ebnf_text <- readFile grammar_file
    case parseGrammar grammar_file ebnf_text of
        Left  err ->
            putStrLn $ "Failed to parse EBNF grammar: " ++ show err
        Right g   -> do
            putStrLn "Successfully parsed EBNF grammar."
            o <- readFile other_file
            case parseWithGrammar g other_file o of
                Left err ->
                    putStrLn $ "Failed to parse second file: " ++ show err
                Right tree ->
                    print tree

-----------------------------------------------------------------
-- Types and user functions
-----------------------------------------------------------------

parseGrammar :: FilePath -> String -> Either ParseError Grammar
parseGrammar fp s = 
    case runParser ebnf M.empty fp s of
        Left e ->
            Left e
        Right (Grammar g, usedNames) ->
            let undefinedRules = foldl (flip M.delete) usedNames $ map fst g
                (undefName, undefNamePos) = M.findMin undefinedRules
            in if   M.null undefinedRules
               then Right $ Grammar g
               else Left $ newErrorMessage
                        (Message $ "Undefined rule: " ++ undefName)
                        undefNamePos

parseWithGrammar :: Grammar -> FilePath -> String -> Either ParseError ParseTree
parseWithGrammar g@(Grammar ((_, firstR) : _)) fp s =
    runParser (liftA cleanTree $ firstR <* eof) g fp s

type GParser = Parsec String UsedNames
type UsedNames = M.Map String SourcePos

type Rule = Parsec String Grammar ParseTree
 -- We need to keep the Grammar around as a Parsec user state
 -- to look up named rules.
data Grammar = Grammar [(String, Rule)]
 -- Grammar would be a type synonym instead of an ADT, but
 -- infinite types aren't allowed in Haskell.

data ParseTree =
    ParseBranch String [ParseTree] |
    ParseLeaf String

instance Show ParseTree where
      show = showIndented 0
--    show (ParseBranch "" t) = '[' : concatMap ((' ' :) . show) t ++ "]"
--    show (ParseBranch s  t) = '(' : s ++ concatMap ((' ' :) . show) t ++ ")"
--    show (ParseLeaf s)      = show s

showIndented :: Int -> ParseTree -> String
showIndented i (ParseBranch "" []) =
    indent i "[]"
showIndented i (ParseBranch "" t) =
    indent i "[" ++
    concatMap (showIndented (i + 2)) t ++
    "]"
showIndented i (ParseBranch s  t) =
    indent i ("(" ++ s) ++
    concatMap (showIndented (i + 2)) t ++
    ")"
showIndented i (ParseLeaf s)      =
    indent i $ show s

indent :: Int -> String -> String
indent i s = "\n" ++ replicate i ' ' ++ s

cleanTree :: ParseTree -> ParseTree
-- Removes empty anonymous branches.
cleanTree (ParseBranch i ts) =
    ParseBranch i $ map cleanTree $ filter p ts
  where p (ParseBranch "" []) = False
        p _                   = True
cleanTree x                 = x

-----------------------------------------------------------------
-- GParser definitions
-----------------------------------------------------------------

ebnf :: GParser (Grammar, UsedNames)
ebnf = liftA2 (,) (ws *> syntax <* eof) getState

syntax :: GParser Grammar
syntax = liftA Grammar $
    optional title *>
    lcbtw '{' '}' (many production) <*
    optional comment

production :: GParser (String, Rule)
production = do
    i <- identifier
    lc '='
    r <- expression
    oneOf ".;"
    ws
    return (i, liftM (nameBranch i) r)
  where nameBranch i (ParseBranch _ l) = ParseBranch i l

expression, term :: GParser Rule
expression = liftA (foldl1 (<|>)) $ term `sepBy1` (lc '|')
term = liftA (branch . sequence) $ many1 factor

factor :: GParser Rule
factor = liftA try $
    liftA ruleByName rememberName <|>
    liftA (leaf . (<* ws) . string) literal <|>
    liftA perhaps (lcbtw '[' ']' expression) <|>
    lcbtw '(' ')' expression <|>
    liftA (branch . many) (lcbtw '{' '}' expression)
  where rememberName :: GParser String
        rememberName = do
            i <- identifier
            p <- getPosition
            modifyState $ M.insertWith (flip const) i p
              {- Adds i → p to the state only if i doesn't
              already have an entry. This ensures we report the
              *first* usage of each unknown identifier. -}
            return i

        ruleByName :: String -> Rule
        ruleByName name = do
            Grammar g <- getState
            fromJust (lookup name g) <?> name

        perhaps = option $ ParseLeaf ""

identifier :: GParser String
identifier = many1 (noneOf " \t\n=|(){}[].;\"'") <* ws

title = literal

comment = literal

literal =
       (lc '\'' *> manyTill anyChar (lc '\'')) <|>
       (lc '"'  *> manyTill anyChar (lc '"'))
    <* ws

-----------------------------------------------------------------
-- Miscellany
-----------------------------------------------------------------

leaf = liftA ParseLeaf
branch = liftA $ ParseBranch ""

lcbtw c1 c2 = between (lc c1) (lc c2)

lc :: Char -> GParser Char
lc c = char c <* ws

ws = many $ oneOf " \n\t"
```


=={{header|Modula-2}}==

```modula2
MODULE EBNF;

FROM  ASCII         IMPORT  EOL;
FROM  InOut         IMPORT  Done, Read, Write, WriteLn, WriteInt, WriteString;
FROM  EBNFScanner   IMPORT  Symbol, sym, id, Ino, GetSym, MarkError, SkipLine;
FROM  TableHandler  IMPORT  WordLength, Table, overflow, InitTable, Record, Tabulate;

VAR   T0, T1                : Table;

PROCEDURE skip (n : INTEGER);

BEGIN
  MarkError (n);
  WHILE  (sym < lpar) OR (sym > period)  DO  GetSym  END
END skip;

PROCEDURE Expression;

  PROCEDURE Term;

    PROCEDURE Factor;

    BEGIN
      IF  sym = ident  THEN
        Record (T0, id, Ino);
        GetSym
      ELSIF  sym = literal  THEN
        Record (T1, id, Ino);
        GetSym
      ELSIF  sym = lpar  THEN
        GetSym;
        Expression;
        IF  sym = rpar  THEN  GetSym  ELSE  skip (2)  END
      ELSIF  sym = lbk  THEN
        GetSym;
        Expression;
        IF  sym = rbk  THEN  GetSym  ELSE  skip (3)  END
      ELSIF  sym = lbr  THEN
        GetSym;
        Expression;
        IF  sym = rbr  THEN  GetSym  ELSE  skip (4)  END
      ELSE
        skip (5)
      END
    END Factor;

  BEGIN
    Factor;
    WHILE  sym < bar  DO  Factor  END
  END Term;

BEGIN
  Term;
  WHILE  sym = bar  DO
    GetSym;
    Term
  END
END Expression;


PROCEDURE Production;

BEGIN
  Record (T0, id, - INTEGER (Ino));
  GetSym;
  IF  sym = eql  THEN  GetSym  ELSE  skip (7)  END;
  Expression;
  IF  sym # period  THEN
    MarkError (8);
    SkipLine
  END;
  GetSym
END Production;


BEGIN
  InitTable (T0);
  InitTable (T1);
  GetSym;
  WHILE  (sym = ident) AND (overflow = 0)  DO  Production  END;
  IF  overflow > 0  THEN
    WriteLn;
    WriteString ("Table overflow");
    WriteInt (overflow, 6);
  END;
  Write (35C);
  Tabulate (T0);
  Tabulate (T1);
END EBNF.
```

And the source for the EBNF scanner. I hope you like nested procedures.

```modula2
IMPLEMENTATION MODULE EBNFScanner;

FROM  ASCII        IMPORT  LF;
FROM  InOut        IMPORT  Read, Write, WriteLn, WriteInt, WriteBf, EOF;

VAR   ch                   : CHAR;

MODULE LineHandler;

  IMPORT   LF, EOF, ch, Ino, Read, Write, WriteLn, WriteInt, WriteBf;
  EXPORT   GetCh, MarkError, SkipLine;

  CONST    LineWidth = 100;

  VAR      cc        : INTEGER;
           cc1       : INTEGER;
           cc2       : INTEGER;
           line      : ARRAY [0..LineWidth - 1] OF CHAR;

  PROCEDURE GetLine;

  BEGIN
    IF  cc2 > 0  THEN
      WriteLn;
      cc2 := 0
    END;
    Read (ch);
    IF  EOF ()  THEN
      line [0] := 177C;
      cc1 := 1
    ELSE
      INC (Ino);
      WriteInt (Ino, 5);
      Write (' ');
      cc1 := 0;
      LOOP
        Write (ch);
        line [cc1] := ch;
        INC (cc1);
        IF  ch = LF  THEN  EXIT  END;
        Read (ch)
      END
    END
  END GetLine;


    PROCEDURE GetCh;

    BEGIN
      WHILE  cc = cc1  DO
        cc := 0;
        GetLine
      END;
      ch := line [cc];
      INC (cc)
    END GetCh;


    PROCEDURE MarkError (n  : INTEGER);

    BEGIN
      IF  cc2 = 0  THEN
        Write ('*');
        cc2 := 3;
        REPEAT
          Write (' ');
          DEC (cc2)
        UNTIL  cc2 = 0;
      END;
      WHILE  cc2 < cc  DO
        Write (' ');
        INC (cc2)
      END;
      Write ('^');
      WriteInt (n, 1);
      INC (cc2, 2)
    END MarkError;

    PROCEDURE SkipLine;

    BEGIN
      WHILE  ch # LF  DO  GetCh  END;
      GetCh
    END SkipLine;

  BEGIN          (* BEGIN of LineHandler        *)
    cc  := 0;
    cc1 := 0;
    cc2 := 0
  END LineHandler;

PROCEDURE GetSym;

VAR     i          : CARDINAL;

BEGIN
  WHILE  ch <= ' '  DO  GetCh  END;
  IF  ch = '/'  THEN
    SkipLine;
    WHILE  ch <= ' '  DO  GetCh  END
  END;
  IF  (CAP (ch) <= 'Z') AND (CAP (ch) >= 'A')  THEN
    i := 0;
    sym := literal;
    REPEAT
      IF  i < IdLength  THEN
        id [i] := ch;
        INC (i)
      END;
      IF  ch > 'Z' THEN  sym := ident  END;
      GetCh
    UNTIL  (CAP (ch) < 'A') OR (CAP (ch) > 'Z');
    id [i] := ' '
  ELSIF  ch = "'"  THEN
    i := 0;
    GetCh;
    sym := literal;
    WHILE  ch # "'"  DO
      IF  i < IdLength  THEN
        id [i] := ch;
        INC (i)
      END;
      GetCh
    END;
    GetCh;
    id [i] := ' '
    WHILE  ch # "'"  DO
      IF  i < IdLength  THEN
        id [i] := ch;
        INC (i)
      END;
      GetCh
    END;
    GetCh;
    id [i] := ' '
  ELSIF  ch = '"'  THEN
    i := 0;
    GetCh;
    sym := literal;
    WHILE  ch # '"'  DO
      IF  i < IdLength  THEN
        id [i] := ch;
        INC (i)
      END;
      GetCh
    END;
    GetCh;
    id [i] := ' '
  ELSIF  ch = '='  THEN  sym := eql;   GetCh
  ELSIF  ch = '('  THEN  sym := lpar;  GetCh
  ELSIF  ch = ')'  THEN  sym := rpar;  GetCh
  ELSIF  ch = '['  THEN  sym := lbk;   GetCh
  ELSIF  ch = ']'  THEN  sym := rbk;   GetCh
  ELSIF  ch = '{'  THEN  sym := lbr;   GetCh
  ELSIF  ch = '}'  THEN  sym := rbr;   GetCh
  ELSIF  ch = '|'  THEN  sym := bar;   GetCh
  ELSIF  ch = '.'  THEN  sym := period;  GetCh
  ELSIF  ch = 177C THEN  sym := other;  GetCh
  ELSE
    sym := other;
    GetCh
  END
END GetSym;

BEGIN
  Ino := 0;
  ch := ' '
END EBNFScanner.
```



## Perl


```perl
#!/usr/bin/perl

use strict;                        # http://www.rosettacode.org/wiki/Parse_EBNF
use warnings;
$SIG{__WARN__} = sub { print "\nWARN: @_\n"; exit };

my $h = qr/\G\s*/;
my $identifier = qr/$h([a-z]\w*)\b/i;
my $literal = qr/$h(['"])(.+?)\1/s;
my ($title, $comment, %productions, %called, $startsymbol, $show, $errpos);

sub node { bless [ @_[1..$#_] ], $_[0] }
sub err { die "ERROR: ", s/\G\s*\K/<**ERROR @_**>/r, "\n" }
sub want { /$h\Q$_[1]\E/gc ? shift : err "missing '$_[1]' " }
sub addin { node $_[0] => ref $_[1] eq $_[0] ? @{$_[1]} : $_[1], pop }

for my $case ( split /^-{50}.*\n/m, do { local $/; @ARGV ? <> : <DATA> } )
  {
  $show = $case =~ s/^#show.*//m;
  my ($ebnf, $tests) = map s/^#.*\n//gmr, split /^#test.*\n/m, $case, 2;
  parse( $ebnf, ($tests // "") =~ /.*\n/g );
  }

sub parse
  {
  eval
    {
    (%productions, %called, $startsymbol) = ();
    local $_ = shift // 'empty ebnf source string';
    print '-' x 75, "\n", s/\s*\z/\n\n/r;
    syntax(); ################################################## parse the EBNF
    print "       title: $title\n     comment: $comment\n";
    $startsymbol or err "no productions";
    print "start symbol: $startsymbol\n";
    for my $key ( sort keys %productions )
      {
      $show and print "\n$key =\n", $productions{$key}->show =~ s/^/   /gmr;
      }
    delete @called{keys %productions};
    %called and die "\nERROR: undefined production(s) <@{[ keys %called]}>\n";

    for ( @_ ) ###################################################### run tests
      {
      $errpos = undef;
      print "\ntry: $_";
      print eval
        {
        $productions{$startsymbol}->run or pos($_) = $errpos, err; ### run tree
        /$h\n/gc or err 'incomplete parse';
        } ? "valid\n" : $@;
      }
    1;
    } or print "$@\n";
  }

sub syntax ############################################## subs for parsing EBNF
  {
  $title = /$literal/gc ? $2 : 'none';
  /$h\{/gc or err 'missing open brace';
  while( /$identifier\s*=/gc )              # is this the start of a production
    {
    my $name = $1;
    $startsymbol //= $name;
    my $tree = expr(0);
    $productions{$name} =
      $productions{$name} ? addin ALT => $productions{$name}, $tree : $tree;
    /$h[.;]/gc or err 'missing production terminator';
    }
  /$h\}/gc or err 'missing close brace';
  $comment = /$literal/gc ? $2 : 'none';
  /$h\z/gc or err 'extra characters after parse';
  }

sub expr
  {
  my $tree =
    /$identifier/gc ? do { $called{$1}++; node PROD => $1 } :
    /$literal/gc    ? node LIT => $2 :
    /$h\[/gc        ? node OPTION => want expr(0), ']' :
    /$h\{/gc        ? node REPEAT => want expr(0), '}' :
    /$h\(/gc        ?                want expr(0), ')' :
    err 'Invalid expression';
  $tree =
    /\G\s+/gc                           ? $tree :
    $_[0] <= 1 && /\G(?=[[{('"a-z])/gci ? addin SEQ => $tree, expr(2) :
    $_[0] <= 0 && /\G\|/gc              ? addin ALT => $tree, expr(1) :
    return $tree while 1;
  }

################################################### run parse tree
sub LIT::run { /$h\Q$_[0][0]\E/gc }
sub SEQ::run
  {
  my $pos = pos($_) // 0;
  for my $node ( @{ $_[0] } )
    {
    $node->run or $errpos = pos($_), pos($_) = $pos, return 0;
    }
  return 1;
  }
sub OPTION::run
  {
  my $pos = pos($_) // 0;
  $_[0][0]->run or pos($_) = $pos, return 1;
  }
sub PROD::run
  {
  $productions{ $_[0][0] }->run;
  }
sub REPEAT::run
  {
  my $pos = pos($_) // 0;
  $pos = pos($_) while $_[0][0]->run;
  pos($_) = $pos;
  return 1;
  }
sub ALT::run
  {
  my $pos = pos($_) // 0;
  for my $node ( @{ $_[0] } )
    {
    eval{ $node->run } and return 1;
    pos($_) = $pos;
    }
  return 0;
  }

sub LIT::show { "LIT $_[0][0]\n" } ###################### for nested tree print
sub PROD::show { "PROD $_[0][0]\n" }
sub UNIVERSAL::show
  {
  join '', ref $_[0], "\n", map $_->show =~ s/^/   /gmr, @{ $_[0] };
  }

__DATA__
"a" {
    a = "a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ;
} "z"
#show
#tests
    a1a3a4a4a5a6
    a1 a2a6
    a1 a3 a4 a6 
    a1 a4 a5 a6
    a1 a2 a4 a5 a5 a6
    a1 a2 a4 a5 a6 a7
    your ad here
----------------------------------------------------------------------
"Arithmetic expressions" {
    expr = term { plus term } .
    term = factor { times factor } .
    factor = number | '(' expr ')' .

    plus = "+" | "-" .
    times = "*" | "/" .

    number = digit { digit } .
    digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
} 'http://www.rosettacode.org/wiki/Parse_EBNF'
#tests
    2
    2*3 + 4/23 - 7
    (3 + 4) * 6-2+(4*(4)) 
    -2
    3 +
    (4 + 3
----------------------------------------------------------------------
'some invalid EBNF' { a = "1" ;
----------------------------------------------------------------------
a = "1";
----------------------------------------------------------------------
{ hello world = "1"; }
----------------------------------------------------------------------
{ foo = bar . } "undefined production check"
----------------------------------------------------------------------
```

{{out}}

```txt

---------------------------------------------------------------------------
"a" {
    a = "a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ;
} "z"

       title: a
     comment: z
start symbol: a

a =
   SEQ
      LIT a1
      ALT
         LIT a2
         LIT a3
      REPEAT
         LIT a4
      OPTION
         LIT a5
      LIT a6

try:     a1a3a4a4a5a6
valid

try:     a1 a2a6
valid

try:     a1 a3 a4 a6 
valid

try:     a1 a4 a5 a6
ERROR:     a1 <**ERROR **>a4 a5 a6


try:     a1 a2 a4 a5 a5 a6
ERROR:     a1 a2 a4 a5 <**ERROR **>a5 a6


try:     a1 a2 a4 a5 a6 a7
ERROR:     a1 a2 a4 a5 a6 <**ERROR incomplete parse**>a7


try:     your ad here
ERROR:     <**ERROR **>your ad here

---------------------------------------------------------------------------
"Arithmetic expressions" {
    expr = term { plus term } .
    term = factor { times factor } .
    factor = number | '(' expr ')' .

    plus = "+" | "-" .
    times = "*" | "/" .

    number = digit { digit } .
    digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
} 'http://www.rosettacode.org/wiki/Parse_EBNF'

       title: Arithmetic expressions
     comment: http://www.rosettacode.org/wiki/Parse_EBNF
start symbol: expr

try:     2
valid

try:     2*3 + 4/23 - 7
valid

try:     (3 + 4) * 6-2+(4*(4)) 
valid

try:     -2
ERROR:     <**ERROR **>-2


try:     3 +
ERROR:     3 <**ERROR incomplete parse**>+


try:     (4 + 3
ERROR:     <**ERROR **>(4 + 3

---------------------------------------------------------------------------
'some invalid EBNF' { a = "1" ;

ERROR: 'some invalid EBNF' { a = "1" ;
<**ERROR missing close brace**>

---------------------------------------------------------------------------
a = "1";

ERROR: <**ERROR missing open brace**>a = "1";


---------------------------------------------------------------------------
{ hello world = "1"; }

ERROR: { <**ERROR missing close brace**>hello world = "1"; }


---------------------------------------------------------------------------
{ foo = bar . } "undefined production check"

       title: none
     comment: undefined production check
start symbol: foo

ERROR: undefined production(s) <bar>

```


## Perl 6

{{works with|Rakudo|2019.03.1}}

This parses the EBNF rule set using a perl 6 grammar, then if it parses as valid EBNF, constructs a grammar and parses the test strings with that. EBNF rule sets that are naively syntactically correct but missing rules will parse as valid but will give a runtime failure warning about missing methods.
It is implemented and exercised using the flavor of EBNF and test cases specified on the [[Parse EBNF/Tests|test page]].


```perl6
# A perl 6 grammar to parse EBNF
grammar EBNF {
  rule         TOP { ^ <title>? '{' [ <production> ]+ '}' <comment>? $ }
  rule  production { <name> '=' <expression> <[.;]> }
  rule  expression { <term> +% "|" }
  rule        term { <factor>+ }
  rule      factor { <group> | <repeat> | <optional> | <identifier> | <literal> }
  rule       group { '(' <expression> ')' }
  rule      repeat { '{' <expression> '}' }
  rule    optional { '[' <expression> ']' }
  token identifier { <-[\|\(\)\{\}\[\]\.\;\"\'\s]>+ } #"
  token    literal { ["'" <-[']>+ "'" | '"' <-["]>+ '"'] } #"
  token      title { <literal> }
  token    comment { <literal> }
  token       name { <identifier>  <?before \h* '='> }
}

class EBNF::Actions {
    method        TOP($/) { 
                            say "Syntax Tree:\n", $/; # Dump the syntax tree to STDOUT
                            make 'grammar ' ~
                              ($<title> ?? $<title>.subst(/\W/, '', :g) !! 'unnamed') ~
                              " \{\n rule TOP \{^[<" ~ $/<production>[0]<name> ~
                              ">]+\$\}\n " ~ $<production>>>.ast ~ "\}"
                          }
    method production($/) { 
                            make 'token ' ~ $<name> ~ ' {' ~
                              $<expression>.ast ~ "}\n"
                          }
    method expression($/) { make join '|', $<term>>>.ast }
    method       term($/) { make join '\h*', $<factor>>>.ast }
    method     factor($/) { 
                            make $<literal>  ?? $<literal> !!
                              $<group>    ?? '[' ~ $<group>.ast    ~ ']'  !!
                              $<repeat>   ?? '[' ~ $<repeat>.ast   ~ '\\s*]*' !!
                              $<optional> ?? '[' ~ $<optional>.ast ~ ']?' !!
                              '<' ~ $<identifier> ~ '>'
                          }
    method     repeat($/) { make $<expression>.ast }
    method   optional($/) { make $<expression>.ast }
    method      group($/) { make $<expression>.ast }
}

# An array of test cases
my @tests = (
    {
        ebnf => 
            q<"a" {
                a = "a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ;
            } "z">
        ,
        teststrings => [
            'a1a3a4a4a5a6',
            'a1 a2a6',
            'a1 a3 a4 a6',
            'a1 a4 a5 a6',
            'a1 a2 a4 a4 a5 a6',
            'a1 a2 a4 a5 a5 a6',
            'a1 a2 a4 a5 a6 a7',
            'your ad here' 
        ]
    },
    {
        ebnf =>
            q<{
                expr = term { plus term } .
                term = factor { times factor } .
                factor = number | '(' expr ')' .

                plus = "+" | "-" .
                times = "*" | "/" .

                number = digit { digit } .
                digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
            }>
        ,
        teststrings => [
            '2',
            '2*3 + 4/23 - 7',
            '(3 + 4) * 6-2+(4*(4))',
            '-2',
            '3 +',
            '(4 + 3'
        ]
    },
    {
        ebnf => q<a = "1";>,
        teststrings => ['foobar']
    },
    {
        ebnf => q<{ a = "1" ;>,
        teststrings => ['foobar']
    },
    {
        ebnf => q<{ hello world = "1"; }>,
        teststrings => ['foobar']
    },
    {
        ebnf => q<{ foo = bar . }>,
        teststrings => ['foobar']
    }
);

# Test the parser.
my $i = 1;
for @tests -> $test {
    unless EBNF.parse($test<ebnf>) {
         say "Parsing EBNF grammar:\n";
         say "{$test<ebnf>.subst(/^^\h*/,'',:g)}\n";
         say "Invalid syntax. Can not be parsed.\n";
         say '*' x 79;
         next;
    }
    my $p = EBNF.parse($test<ebnf>, :actions(EBNF::Actions));
    my $grammar = $p.ast;
    $grammar ~~ m/^'grammar '(\w+)/;
    my $title = $0;
    my $fn = 'EBNFtest'~$i++;
    my $fh = open($fn, :w) orelse .die;
    $fh.say( "\{\n", $grammar );
    $fh.say( qq|say "Parsing EBNF grammar '$title':\\n";| );
    $fh.say( qq|say q<{$test<ebnf>.subst(/^^\h*/,'',:g)}>;| );
    $fh.say(  q|say "\nValid syntax.\n\nTesting:\n";| );
    $fh.say(  q|CATCH { default { say " - $_" } };| );
    my $len = max $test<teststrings>.flat>>.chars;
    for $test<teststrings>.flat -> $s {
        $fh.say( qq|printf "%{$len}s", '{$s}';| ~
                 qq|printf " - %s\\n", {$title}.parse('{$s}')| ~
                 qq| ?? 'valid.' !! 'NOT valid.';|
               );
    }
    $fh.say( qq| "\\n"} |);
    $fh.close;
    say qqx/perl6 $fn/;
    say '*' x 79, "\n";
    unlink $fn;
}
```


Output:

```txt

Syntax Tree:
｢"a" {
                a = "a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ;
            } "z"｣
 title => ｢"a"｣
  literal => ｢"a"｣
 production => ｢a = "a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ;
            ｣
  name => ｢a｣
   identifier => ｢a｣
  expression => ｢"a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ｣
   term => ｢"a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ｣
    factor => ｢"a1" ｣
     literal => ｢"a1"｣
    factor => ｢( "a2" | "a3" ) ｣
     group => ｢( "a2" | "a3" ) ｣
      expression => ｢"a2" | "a3" ｣
       term => ｢"a2" ｣
        factor => ｢"a2" ｣
         literal => ｢"a2"｣
       term => ｢ "a3" ｣
        factor => ｢"a3" ｣
         literal => ｢"a3"｣
    factor => ｢{ "a4" } ｣
     repeat => ｢{ "a4" } ｣
      expression => ｢"a4" ｣
       term => ｢"a4" ｣
        factor => ｢"a4" ｣
         literal => ｢"a4"｣
    factor => ｢[ "a5" ] ｣
     optional => ｢[ "a5" ] ｣
      expression => ｢"a5" ｣
       term => ｢"a5" ｣
        factor => ｢"a5" ｣
         literal => ｢"a5"｣
    factor => ｢"a6" ｣
     literal => ｢"a6"｣
 comment => ｢"z"｣
  literal => ｢"z"｣

Parsing EBNF grammar 'a':

"a" {
a = "a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ;
} "z"

Valid syntax.

Testing:

     a1a3a4a4a5a6 - valid.
          a1 a2a6 - valid.
      a1 a3 a4 a6 - valid.
      a1 a4 a5 a6 - NOT valid.
a1 a2 a4 a4 a5 a6 - valid.
a1 a2 a4 a5 a5 a6 - NOT valid.
a1 a2 a4 a5 a6 a7 - NOT valid.
     your ad here - NOT valid.

*******************************************************************************

Syntax Tree:
｢{
                expr = term { plus term } .
                term = factor { times factor } .
                factor = number | '(' expr ')' .

                plus = "+" | "-" .
                times = "*" | "/" .

                number = digit { digit } .
                digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
            }｣
 production => ｢expr = term { plus term } .
                ｣
  name => ｢expr｣
   identifier => ｢expr｣
  expression => ｢term { plus term } ｣
   term => ｢term { plus term } ｣
    factor => ｢term ｣
     identifier => ｢term｣
    factor => ｢{ plus term } ｣
     repeat => ｢{ plus term } ｣
      expression => ｢plus term ｣
       term => ｢plus term ｣
        factor => ｢plus ｣
         identifier => ｢plus｣
        factor => ｢term ｣
         identifier => ｢term｣
 production => ｢term = factor { times factor } .
                ｣
  name => ｢term｣
   identifier => ｢term｣
  expression => ｢factor { times factor } ｣
   term => ｢factor { times factor } ｣
    factor => ｢factor ｣
     identifier => ｢factor｣
    factor => ｢{ times factor } ｣
     repeat => ｢{ times factor } ｣
      expression => ｢times factor ｣
       term => ｢times factor ｣
        factor => ｢times ｣
         identifier => ｢times｣
        factor => ｢factor ｣
         identifier => ｢factor｣
 production => ｢factor = number | '(' expr ')' .

                ｣
  name => ｢factor｣
   identifier => ｢factor｣
  expression => ｢number | '(' expr ')' ｣
   term => ｢number ｣
    factor => ｢number ｣
     identifier => ｢number｣
   term => ｢ '(' expr ')' ｣
    factor => ｢'(' ｣
     literal => ｢'('｣
    factor => ｢expr ｣
     identifier => ｢expr｣
    factor => ｢')' ｣
     literal => ｢')'｣
 production => ｢plus = "+" | "-" .
                ｣
  name => ｢plus｣
   identifier => ｢plus｣
  expression => ｢"+" | "-" ｣
   term => ｢"+" ｣
    factor => ｢"+" ｣
     literal => ｢"+"｣
   term => ｢ "-" ｣
    factor => ｢"-" ｣
     literal => ｢"-"｣
 production => ｢times = "*" | "/" .

                ｣
  name => ｢times｣
   identifier => ｢times｣
  expression => ｢"*" | "/" ｣
   term => ｢"*" ｣
    factor => ｢"*" ｣
     literal => ｢"*"｣
   term => ｢ "/" ｣
    factor => ｢"/" ｣
     literal => ｢"/"｣
 production => ｢number = digit { digit } .
                ｣
  name => ｢number｣
   identifier => ｢number｣
  expression => ｢digit { digit } ｣
   term => ｢digit { digit } ｣
    factor => ｢digit ｣
     identifier => ｢digit｣
    factor => ｢{ digit } ｣
     repeat => ｢{ digit } ｣
      expression => ｢digit ｣
       term => ｢digit ｣
        factor => ｢digit ｣
         identifier => ｢digit｣
 production => ｢digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
            ｣
  name => ｢digit｣
   identifier => ｢digit｣
  expression => ｢"0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ｣
   term => ｢"0" ｣
    factor => ｢"0" ｣
     literal => ｢"0"｣
   term => ｢ "1" ｣
    factor => ｢"1" ｣
     literal => ｢"1"｣
   term => ｢ "2" ｣
    factor => ｢"2" ｣
     literal => ｢"2"｣
   term => ｢ "3" ｣
    factor => ｢"3" ｣
     literal => ｢"3"｣
   term => ｢ "4" ｣
    factor => ｢"4" ｣
     literal => ｢"4"｣
   term => ｢ "5" ｣
    factor => ｢"5" ｣
     literal => ｢"5"｣
   term => ｢ "6" ｣
    factor => ｢"6" ｣
     literal => ｢"6"｣
   term => ｢ "7" ｣
    factor => ｢"7" ｣
     literal => ｢"7"｣
   term => ｢ "8" ｣
    factor => ｢"8" ｣
     literal => ｢"8"｣
   term => ｢ "9" ｣
    factor => ｢"9" ｣
     literal => ｢"9"｣

Parsing EBNF grammar 'unnamed':

{
expr = term { plus term } .
term = factor { times factor } .
factor = number | '(' expr ')' .

plus = "+" | "-" .
times = "*" | "/" .

number = digit { digit } .
digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
}

Valid syntax.

Testing:

                    2 - valid.
       2*3 + 4/23 - 7 - valid.
(3 + 4) * 6-2+(4*(4)) - valid.
                   -2 - NOT valid.
                  3 + - NOT valid.
               (4 + 3 - NOT valid.

*******************************************************************************

Parsing EBNF grammar:

a = "1";

Invalid syntax. Can not be parsed.

*******************************************************************************
Parsing EBNF grammar:

{ a = "1" ;

Invalid syntax. Can not be parsed.

*******************************************************************************
Parsing EBNF grammar:

{ hello world = "1"; }

Invalid syntax. Can not be parsed.

*******************************************************************************
Syntax Tree:
｢{ foo = bar . }｣
 production => ｢foo = bar . ｣
  name => ｢foo｣
   identifier => ｢foo｣
  expression => ｢bar ｣
   term => ｢bar ｣
    factor => ｢bar ｣
     identifier => ｢bar｣

Parsing EBNF grammar 'unnamed':

{ foo = bar . }

Valid syntax.

Testing:

foobar - No such method 'bar' for invocant of type 'unnamed'

*******************************************************************************
```



## Phix


```Phix
string src
integer ch, sdx
object token

bool error = false
function invalid(string msg)
    error = true
    ?msg
    sdx = length(src)+1 -- (set to eof)
    return -1
end function

procedure skip_spaces()
    while 1 do
        if sdx>length(src) then exit end if
        ch = src[sdx]
        if not find(ch," \t\r\n") then exit end if
        sdx += 1
    end while
end procedure

procedure get_token()
-- yeilds a single character token, one of {}()[]|=.;
-- or {"terminal",string} or {"ident", string} or -1.
    skip_spaces()
    if sdx>length(src) then
        token = -1
        return
    end if
    integer tokstart = sdx
    if find(ch,"{}()[]|=.;") then
        sdx += 1
        token = ch
    elsif ch='\"'
       or ch='\'' then
        integer closech = ch
        for tokend=sdx+1 to length(src) do
            if src[tokend] = closech then
                sdx = tokend+1
                token = {"terminal",src[tokstart+1..tokend-1]}
                return
            end if
        end for
        token = invalid("no closing quote")
    elsif ch>='a' and ch<='z' then
        -- to simplify things for the purposes of this task, 
        -- identifiers are strictly a-z only, not A-Z or 1-9
        while 1 do
            sdx += 1
            if sdx>length(src) then exit end if
            ch = src[sdx]
            if ch<'a' or ch>'z' then exit end if
        end while
        token = {"ident",src[tokstart..sdx-1]}
    else
        token = invalid("invalid ebnf")
    end if
end procedure

procedure match_token(integer ch)
    if token!=ch then
        token = invalid(sprintf("invalid ebnf (%c expected)",ch))
    else
        get_token()
    end if
end procedure

sequence idents = {},
         ididx = {}

function add_ident(string ident)
    integer k = find(ident,idents)
    if k=0 then
        idents = append(idents,ident)
        k = length(idents)
        ididx = append(ididx,0)
    end if
    return k
end function

forward function expression()

function factor()
object res
    if sequence(token) then
        if token[1]="ident" then
            token &= add_ident(token[2])
        end if
        res = token
        get_token()
    elsif token='[' then
        get_token()
        res = {"optional",expression()}
        match_token(']')
    elsif token='(' then
        get_token()
        res = expression()
        match_token(')')
    elsif token='{' then
        get_token()
        res = {"repeat",expression()}
        match_token('}')
    else
        ?9/0        -- erm??
--      res = -1    --  ""
    end if
    return res
end function

function term()
--  sequence res = {"factors",factor()} -- (opted against this)
    sequence res = {factor()}
    while not find(token,{-1,'|','.',';',')',']','}'}) do
        res = append(res,factor())
    end while
    if length(res)=1 then res = res[1] end if
    return res
end function

function expression()
    object res = term()
    if token='|' then
        res = {"or",res}
        while token='|' do
            get_token()
            res = append(res,term())
        end while
    end if
    return res
end function

sequence productions = {}

function production()
-- returns a token or -1; the real result is left in productions[] etc.
    get_token()
    if token!='}' then
        if token=-1 then
            return invalid("invalid ebnf (missing closing })")
        end if
        if not sequence(token)
        or token[1]!="ident" then
            return -1
        end if
        string ident = token[2]
        integer idx = add_ident(ident)
        get_token()
        match_token('=')
        if token=-1 then
            return -1
        end if
        sequence res = expression()
        productions = append(productions,{ident,idx,res})
        ididx[idx] = length(productions)
    end if
    return token
end function

sequence extras = {}

function parse(string ebnf)
-- returns: +1 if ok, -1 if bad
    puts(1,"parse: "&ebnf&" ===>\n")
    error = false
    src = ebnf
    sdx = 1
    idents = {}
    ididx = {}
    productions = {}
    extras = {}
    get_token()
    if sequence(token) then
        token[1] = "title"
        extras = append(extras,token)
        get_token()
    end if
    if token!='{' then
        return invalid("invalid ebnf (missing opening {)")
    end if
    while 1 do
        token = production()
        if token='}' or token=-1 then exit end if
    end while
    get_token()
    if sequence(token) then
        token[1] = "comment"
        extras = append(extras,token)
        get_token()
    end if
    if token!=-1 then
        return invalid("invalid ebnf (missing eof?)")
    end if      
    if error then return -1 end if
    integer k = find(0,ididx)
    if k!=0 then
        return invalid("invalid ebnf (undefined:"&idents[k]&")")
    end if
    ppOpt({pp_Pause,0})
    pp(productions)
--  pp(idents)
--  pp(ididx)
--  pp(extras)
    return 1
end function

-- The rules that applies() has to deal with are:
-- {factors} - if rule[1] is not string, just apply one after the other,
--             recursively. As per term() above, originally had "factors",
--              but getting rid of it made the syntax tree much clearer)
-- {"terminal", "a1"}       -- literal constants
-- {"or", <e1>, <e2>, ...}  -- (any) one of n
-- {"repeat", <e1>}         -- as per "{}" in ebnf
-- {"optional", <e1>}       -- as per "[]" in ebnf
-- {"ident", <name>, idx}   -- apply the sub-rule

function applies(sequence rule)
integer was_sdx = sdx   -- in case of failure
    object r1 = rule[1]
    if not string(r1) then
        for i=1 to length(rule) do
            if not applies(rule[i]) then
                sdx = was_sdx
                return false
            end if
        end for
    elsif r1="terminal" then
        skip_spaces()
        for i=1 to length(rule[2]) do
            if sdx>length(src)
            or src[sdx]!=rule[2][i] then
                sdx = was_sdx
                return false
            end if
            sdx += 1
        end for
    elsif r1="or" then
        for i=2 to length(rule) do
            if applies(rule[i]) then
                return true
            end if
        end for
        sdx = was_sdx
        return false
    elsif r1="repeat" then
        while applies(rule[2]) do
        end while
    elsif r1="optional" then
        if applies(rule[2]) then
        end if
    elsif r1="ident" then
        integer i = rule[3],
                ii = ididx[i]
        if not applies(productions[ii][3]) then
            sdx = was_sdx
            return false
        end if
    else
        ?9/0
    end if
    return true
end function

procedure check_valid(string test)
    src = test
    sdx = 1
    bool res = applies(productions[1][3])
    skip_spaces()
    if sdx<=length(src) then res = false end if
    ?{test,{"pass","fail"}[2-res]}
end procedure
 
constant ebnf = {""" 
"a" {
    a = "a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ;
} "z" """,
""" 
{
    expr = term { plus term } .
    term = factor { times factor } .
    factor = number | '(' expr ')' .
 
    plus = "+" | "-" .
    times = "*" | "/" .
 
    number = digit { digit } .
    digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
}""",
`a = "1"`,
`{ a = "1" ;`,
`{ hello world = "1"; }`,
`{ foo = bar . }`
}

constant tests = { {"a1a3a4a4a5a6",
                    "a1 a2a6",
                    "a1 a3 a4 a6",
                    "a1 a4 a5 a6",
                    "a1 a2 a4 a5 a5 a6",
                    "a1 a2 a4 a5 a6 a7",
                    "your ad here"},
                   {"2",
                    "2*3 + 4/23 - 7",
                    "(3 + 4) * 6-2+(4*(4))",
                    "-2",
                    "3 +",
                    "(4 + 3"}}

for i=1 to length(ebnf) do
    if parse(ebnf[i])=+1 then
        ?"tests:"
        for j=1 to length(tests[i]) do
            check_valid(tests[i][j])
        end for
    end if
end for
```

In real use, I would be tempted to use numeric literals rather than string tags in such structures, but the latter certainly make things ten times easier to debug, plus I got an instantly legible syntax tree dump (the bit just after "===>" below) practically for free.
{{out}}

```txt

parse:
"a" {
    a = "a1" ( "a2" | "a3" ) { "a4" } [ "a5" ] "a6" ;
} "z"  ===>
{{"a", 1,
  {{"terminal", "a1"}, {"or", {"terminal", "a2"}, {"terminal", "a3"}},
   {"repeat", {"terminal", "a4"}}, {"optional", {"terminal", "a5"}},
   {"terminal", "a6"}}}}
"tests:"
{"a1a3a4a4a5a6","pass"}
{"a1 a2a6","pass"}
{"a1 a3 a4 a6","pass"}
{"a1 a4 a5 a6","fail"}
{"a1 a2 a4 a5 a5 a6","fail"}
{"a1 a2 a4 a5 a6 a7","fail"}
{"your ad here","fail"}
parse:
{
    expr = term { plus term } .
    term = factor { times factor } .
    factor = number | '(' expr ')' .

    plus = "+" | "-" .
    times = "*" | "/" .

    number = digit { digit } .
    digit = "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" .
} ===>
{{"expr", 1,
  {{"ident", "term", 2},
   {"repeat", {{"ident", "plus", 3}, {"ident", "term", 2}}}}},
 {"term", 2,
  {{"ident", "factor", 4},
   {"repeat", {{"ident", "times", 5}, {"ident", "factor", 4}}}}},
 {"factor", 4,
  {"or", {"ident", "number", 6},
   {{"terminal", "("}, {"ident", "expr", 1}, {"terminal", ")"}}}},
 {"plus", 3, {"or", {"terminal", "+"}, {"terminal", "-"}}},
 {"times", 5, {"or", {"terminal", "*"}, {"terminal", "/"}}},
 {"number", 6, {{"ident", "digit", 7}, {"repeat", {"ident", "digit", 7}}}},
 {"digit", 7,
  {"or", {"terminal", "0"}, {"terminal", "1"}, {"terminal", "2"},
   {"terminal", "3"}, {"terminal", "4"}, {"terminal", "5"},
   {"terminal", "6"}, {"terminal", "7"}, {"terminal", "8"},
   {"terminal", "9"}}}}
"tests:"
{"2","pass"}
{"2*3 + 4/23 - 7","pass"}
{"(3 + 4) * 6-2+(4*(4))","pass"}
{"-2","fail"}
{"3 +","fail"}
{"(4 + 3","fail"}
parse: a = "1" ===>
"invalid ebnf (missing opening {)"
parse: { a = "1" ; ===>
"invalid ebnf (missing closing })"
parse: { hello world = "1"; } ===>
"invalid ebnf (= expected)"
parse: { foo = bar . } ===>
"invalid ebnf (undefined:bar)"

```



## PicoLisp


```PicoLisp
(de EBNF
   "expr  : term ( ( PLUS | MINUS )  term )* ;"
   "term  : factor ( ( MULT | DIV ) factor )* ;"
   "factor   : NUMBER ;" )

(for E EBNF
   (use (@S @E)
      (unless (and (match '(@S : @E ;) (str E)) (not (cdr @S)))
         (quit "Invalid EBNF" E) )
      (put (car @S) 'ebnf @E) ) )
```


```PicoLisp
(de matchEbnf (Pat)
   (cond
      ((asoq Pat '((PLUS . +) (MINUS . -) (MULT . *) (DIV . /)))
         (let Op (cdr @)
            (when (= Op (car *Lst))
               (pop '*Lst)
               Op ) ) )
      ((== 'NUMBER Pat)
         (cond
            ((num? (car *Lst))
               (pop '*Lst)
               @ )
            ((and (= "-" (car *Lst)) (num? (cadr *Lst)))
               (setq *Lst (cddr *Lst))
               (- @) ) ) )
      ((get Pat 'ebnf) (parseLst @))
      ((atom Pat))
      (T
         (loop
            (T (matchEbnf (pop 'Pat)) @)
            (NIL Pat)
            (NIL (== '| (pop 'Pat)))
            (NIL Pat) ) ) ) )

(de parseLst (Pat)
   (let (P (pop 'Pat)  X (matchEbnf P))
      (loop
         (NIL Pat)
         (if (n== '* (cadr Pat))
            (if (matchEbnf (pop 'Pat))
               (setq X (list @ X))
               (throw) )
            (loop
               (NIL *Lst)
               (NIL (matchEbnf (car Pat)))
               (setq X (list @ X (or (matchEbnf P) (throw)))) )
            (setq Pat (cddr Pat)) ) )
      X ) )

(de parseEbnf (Str)
   (let *Lst (str Str "")
      (catch NIL
         (parseLst (get 'expr 'ebnf)) ) ) )
```

Output:

```txt
: (parseEbnf "1 + 2 * -3 / 7 - 3 * 4")
-> (- (+ 1 (/ (* 2 -3) 7)) (* 3 4))
```



## Ruby

{{in progress|lang=Ruby|day=12|month=May|year=2011}}
{{incomplete|Ruby|The tokenizer is here, but the parser is very incomplete.}}

```ruby
#--
# The tokenizer splits the input into Tokens like "identifier",
# ":", ")*" and so on. This design uses a StringScanner on each line of
# input, therefore a Token can never span more than one line.
#
# Each Token knows its original line and position, so an error message
# can locate a bad token.
#++

require 'strscan'

# A line of input.
# where::  A location like "file.txt:3"
# str::    String of this line
Line = Struct.new :where, :str

# A token.
# cat::   A category like :colon, :ident or so on
# str::   String of this token
# line::  Line containing this token
# pos::   Position of this token within this line
Token = Struct.new :cat, :str, :line, :pos

# Reads and returns the next Token. At end of file, returns nil.
#--
# Needs @filename and @in.
#++
def next_token
  # Loop until we reach a Token.
  loop do
    # If at end of line, then get next line, or else declare end of
    # file.
    if @scanner.eos?
      if s = @in.gets
        # Each line needs a new Line object. Tokens can hold references
        # to old Line objects.
        @line = Line.new("#{@filename}:#{@in.lineno}", s)
        @scanner.string = s
      else
        return nil  # End of file
      end
    end

    # Skip whitespace.
    break unless @scanner.skip(/[[:space:]]+/)
  end

  # Read token by regular expression.
  if s = @scanner.scan(/:/)
    c = :colon
  elsif s = @scanner.scan(/;/)
    c = :semicolon
  elsif s = @scanner.scan(/\(/)
    c = :paren
  elsif s = @scanner.scan(/\)\?/)
    c = :option
  elsif s = @scanner.scan(/\)\*/)
    c = :repeat
  elsif s = @scanner.scan(/\)/)
    c = :group
  elsif s = @scanner.scan(/\|/)
    c = :bar
  elsif s = @scanner.scan(/[[:alpha:]][[:alnum:]]*/)
    c = :ident
  elsif s = @scanner.scan(/'[^']*'|"[^"]*"/)
    # Fix syntax highlighting for Rosetta Code. => '
    c = :string
  elsif s = @scanner.scan(/'[^']*|"[^"]*/)
    c = :bad_string
  elsif s = @scanner.scan(/.*/)
    c = :unknown
  end

  Token.new(c, s, @line, (@scanner.pos - s.length))
end

# Prints a _message_ to standard error, along with location of _token_.
def error(token, message)
  line = token.line

  # We print a caret ^ pointing at the bad token. We make a very crude
  # attempt to align the caret ^ in the correct column. If the input
  # line has a non-[:print:] character, like a tab, then we print it as
  # a space.
  STDERR.puts <<EOF
#{line.where}: #{message}
#{line.str.gsub(/[^[:print:]]/, " ")}
#{" " * token.pos}^
EOF
end


#--
# The parser converts Tokens to a Grammar object. The parser also
# detects syntax errors.
#++

# A parsed EBNF grammar. It is an Array of Productions.
class Grammar < Array; end

# A production.
# ident::  The identifier
# alts::   An Array of Alternatives
Production = Struct.new :ident, :alts

# An array of Alternatives, as from "(a | b)".
class Group < Array; end

# An optional group, as from "(a | b)?".
class OptionalGroup < Group; end

# A repeated group, as from "(a | b)*".
class RepeatedGroup < Group; end

# An array of identifiers and string literals.
class Alternative < Array; end

#--
# Needs @filename and @in.
#++
def parse
  # TODO: this only dumps the tokens.
  while t = next_token
    error(t, "#{t.cat}")
  end
end

# Set @filename and @in. Parse input.
case ARGV.length
when 0 then @filename = "-"
when 1 then @filename = ARGV[0]
else fail "Too many arguments"
end
open(@filename) do |f|
  @in = f
  @scanner = StringScanner.new("")
  parse
end

```



## Tcl

{{improve|Tcl|This is not an EBNF parser. It never uses EBNF. It is a calculator parser, but there is already a calculator parser at [[Arithmetic evaluation#Tcl]]. One should adjust this solution to parse the EBNF language, not the calculator language.}}

Demonstration lexer and parser. Note that this parser supports parenthesized expressions, making the grammar recursive.

```tcl
package require Tcl 8.6

# Utilities to make the coroutine easier to use
proc provide args {while {![yield $args]} {yield}}
proc next lexer {$lexer 1}
proc pushback lexer {$lexer 0}

# Lexical analyzer coroutine core
proc lexer {str} {
    yield [info coroutine]
    set symbols {+ PLUS - MINUS * MULT / DIV ( LPAR ) RPAR}
    set idx 0
    while 1 {
	switch -regexp -matchvar m -- $str {
	    {^\s+} {
		# No special action for whitespace
	    }
	    {^([-+*/()])} {
		provide [dict get $symbols [lindex $m 1]] [lindex $m 1] $idx
	    }
	    {^(\d+)} {
		provide NUMBER [lindex $m 1] $idx
	    }
	    {^$} {
		provide EOT "EOT" $idx
		return
	    }
	    . {
		provide PARSE_ERROR [lindex $m 0] $idx
	    }
	}
	# Trim the matched string
	set str [string range $str [string length [lindex $m 0]] end]
	incr idx [string length [lindex $m 0]]
    }
}

# Utility functions to help with making an LL(1) parser; ParseLoop handles
# EBNF looping constructs, ParseSeq handles sequence constructs.
proc ParseLoop {lexer def} {
    upvar 1 token token payload payload index index
    foreach {a b} $def {
	if {$b ne "-"} {set b [list set c $b]}
	lappend m $a $b
    }
    lappend m default {pushback $lexer; break}
    while 1 {
	lassign [next $lexer] token payload index
	switch -- $token {*}$m
	if {[set c [catch {uplevel 1 $c} res opt]]} {
	    dict set opt -level [expr {[dict get $opt -level]+1}]
	    return -options $opt $res
	}
    }
}
proc ParseSeq {lexer def} {
    upvar 1 token token payload payload index index
    foreach {t s} $def {
	lassign [next $lexer] token payload index
	switch -- $token $t {
	    if {[set c [catch {uplevel 1 $s} res opt]]} {
		dict set opt -level [expr {[dict get $opt -level]+1}]
		return -options $opt $res
	    }
	} EOT {
	    throw SYNTAX "end of text at position $index"
	} default {
	    throw SYNTAX "\"$payload\" at position $index"
	}
    }
}

# Main parser driver; contains "master" grammar that ensures that the whole
# text is matched and not just a prefix substring. Note also that the parser
# runs the lexer as a coroutine (with a fixed name in this basic demonstration
# code).
proc parse {str} {
    set lexer [coroutine l lexer $str]
    try {
	set parsed [parse.expr $lexer]
	ParseLoop $lexer {
	    EOT {
		return $parsed
	    }
	}
	throw SYNTAX "\"$payload\" at position $index"
    } trap SYNTAX msg {
	return -code error "syntax error: $msg"
    } finally {
	catch {rename $lexer ""}
    }
}

# Now the descriptions of how to match each production in the grammar...
proc parse.expr {lexer} {
    set expr [parse.term $lexer]
    ParseLoop $lexer {
	PLUS - MINUS {
	    set expr [list $token $expr [parse.term $lexer]]
	}
    }
    return $expr
}
proc parse.term {lexer} {
    set term [parse.factor $lexer]
    ParseLoop $lexer {
	MULT - DIV {
	    set term [list $token $term [parse.factor $lexer]]
	}
    }
    return $term
}
proc parse.factor {lexer} {
    ParseLoop $lexer {
	NUMBER {
	    return $payload
	}
	MINUS {
	    ParseSeq $lexer {
		NUMBER {return -$payload}
	    }
	}
	LPAR {
	    set result [parse.expr $lexer]
	    ParseSeq $lexer {
		RPAR {return $result}
	    }
	    break
	}
	EOT {
	    throw SYNTAX "end of text at position $index"
	}
    }
    throw SYNTAX "\"$payload\" at position $index"
}
```



```tcl
# Demonstration code
puts [parse "1 - 2 - -3 * 4 + 5"]
puts [parse "1 - 2 - -3 * (4 + 5)"]
```

Output:

```txt

PLUS {MINUS {MINUS 1 2} {MULT -3 4}} 5
MINUS {MINUS 1 2} {MULT -3 {PLUS 4 5}}

```

