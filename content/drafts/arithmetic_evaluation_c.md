+++
title = "Arithmetic evaluation/C"
description = ""
date = 2010-02-06T12:26:42Z
aliases = []
[extra]
id = 4823
[taxonomies]
categories = []
tags = []
+++

{{works with|gcc|4.3.2 20081105 (Red Hat 4.3.2-7)}}

This is a LL(1) recursive descent parser. Only performs integer division. There is a function for every non-terminal in the grammar, save add_op and mult_op, which were lumped into term_tail and factor_tail respectively.

```cpp
#include <iostream>
#include <stdio.h>
#include <ctype.h>

unsigned int G_STRING_ITERATOR = 0;

/*
 * expr        := term term_tail
 * term_tail   := add_op term term_tail | e
 * term        := factor factor_tail
 * factor_tail := mult_op factor factor_tail | e
 * factor      := ( expr ) | number
 * add_op      := + | -
 * mult_op     := * | /
 */

typedef union {
  int terminal;
  struct expression* expr[2];
} Data;

typedef struct expression {
  char op;
  Data data;
} Expr;

void parse_error(const char* string) {
  unsigned int i;
  fprintf(stderr, "Unexpected symbol '%c' at position %u.\n\n", string[G_STRING_ITERATOR], G_STRING_ITERATOR);
  fprintf(stderr, "String: '%s'\n", string);
  fprintf(stderr, "Problem: ");
  for(i = 0; i < G_STRING_ITERATOR; ++i) {
    fprintf(stderr, " ");
  }
  fprintf(stderr, "^\n");
  exit(1);
}

/* Will "consume" a character from the input,
 * (such as +, -, *, etc.) and return it.
 * By consume, I'm really just moving the pointer
 * forward and disregarding the character for
 * future purposes.
 */
char consume_char(const char* string, char c) {
  if(string[G_STRING_ITERATOR] != c) {
    parse_error(string);
  }
  ++G_STRING_ITERATOR;
  return c;
}

/* Same as consume_char, except for integers.
 */
int consume_int(const char* string) {
  int i;

  if(!isdigit(string[G_STRING_ITERATOR])) {
    parse_error(string);
  }

  /* I don't have to pass in the start of the string
   * into atoi, but only where I want it to start
   * scanning for an integer.
   */
  i = atoi(string + G_STRING_ITERATOR);
  while(isdigit(string[G_STRING_ITERATOR])) {
    ++G_STRING_ITERATOR;
  }
  return i;
}

Expr* expression(const char* string);

Expr* factor(const char* string, Expr* expr) {
  if(string[G_STRING_ITERATOR] == '(') {
    expr->op = consume_char(string, '(');
    expr->data.expr[0] = expression(string);
    consume_char(string, ')');
  } else if(isdigit(string[G_STRING_ITERATOR])) {
    expr->op = 'd';
    expr->data.terminal = consume_int(string);
  }
  return expr;
}

Expr* factor_tail(const char* string, Expr* expr) {
  Expr* new_expr;

  switch(string[G_STRING_ITERATOR]) {
  case '*':
  case '/':
    if(NULL == (new_expr = (Expr*)malloc(sizeof(Expr)))) {
      exit(1);
    }
    if(NULL == (new_expr->data.expr[1] = (Expr*)malloc(sizeof(Expr)))) {
      exit(1);
    }
    new_expr->op = consume_char(string, string[G_STRING_ITERATOR]);
    new_expr->data.expr[0] = expr;

    new_expr->data.expr[1] = factor(string, new_expr->data.expr[1]);
    new_expr = factor_tail(string, new_expr);
    return new_expr;
  case '+':
  case '-':
  case ')':
  case 0:
    return expr;
  default:
    parse_error(string);
  }
}

Expr* term(const char* string, Expr* expr) {
  if(string[G_STRING_ITERATOR] == '(' || isdigit(string[G_STRING_ITERATOR])) {
    expr = factor(string, expr);
    expr = factor_tail(string, expr);
    return expr;
  } else {
    parse_error(string);
  }
}

Expr* term_tail(const char* string, Expr* expr) {
  Expr* new_expr;

  switch(string[G_STRING_ITERATOR]) {
  case '+':
  case '-':
    if(NULL == (new_expr = (Expr*)malloc(sizeof(Expr)))) {
      exit(1);
    }
    if(NULL == (new_expr->data.expr[1] = (Expr*)malloc(sizeof(Expr)))) {
      exit(1);
    }
    new_expr->op = consume_char(string, string[G_STRING_ITERATOR]);
    new_expr->data.expr[0] = expr;

    new_expr->data.expr[1] = term(string, new_expr->data.expr[1]);
    new_expr = term_tail(string, new_expr);
    return new_expr;
  case ')':
  case 0:
    return expr;
  default:
    parse_error(string);
  }
}

Expr* expression(const char* string) {
  Expr* expr;

  if(string[G_STRING_ITERATOR] == '(' || isdigit(string[G_STRING_ITERATOR])) {
    if(NULL == (expr = (Expr*)malloc(sizeof(Expr)))) {
      exit(1);
    }

    expr = term(string, expr);
    expr = term_tail(string, expr);
    return expr;
  } else {
    parse_error(string);
  }
}

/* Runs through the AST, evaluating and freeing
 * the tree as it goes.
 */
int evaluate(Expr* expr) {
  int ret;

  switch(expr->op) {
  case '(':
    ret = evaluate(expr->data.expr[0]);
    free(expr->data.expr[0]);
    break;
  case '*':
    ret =
      evaluate(expr->data.expr[0])
      *
      evaluate(expr->data.expr[1])
      ;
    free(expr->data.expr[0]);
    free(expr->data.expr[1]);
    break;
  case '/':
    ret =
      evaluate(expr->data.expr[0])
      /
      evaluate(expr->data.expr[1])
      ;
    free(expr->data.expr[0]);
    free(expr->data.expr[1]);
    break;
  case '+':
    ret =
      evaluate(expr->data.expr[0])
      +
      evaluate(expr->data.expr[1])
      ;
    free(expr->data.expr[0]);
    free(expr->data.expr[1]);
    break;
  case '-':
    ret =
      evaluate(expr->data.expr[0])
      -
      evaluate(expr->data.expr[1])
      ;
    free(expr->data.expr[0]);
    free(expr->data.expr[1]);
    break;
  case 'd':
    ret = expr->data.terminal;
    break;
  default:
    exit(1);
  }
  return ret;
}

int main(int argc, char** argv) {
  Expr* expr = NULL;

  if(argc > 1) {
    expr = expression(argv[1]);
    printf("%d\n", evaluate(expr));
    free(expr);
  }
  return 0;
}

```

