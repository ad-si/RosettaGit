+++
title = "24 game/C"
description = ""
date = 2010-11-23T01:20:27Z
aliases = []
[extra]
id = 4954
[taxonomies]
categories = []
tags = []
+++

First we need to create a RPN parser (RPN since it's easier!).


### rpn.h


```c
#ifndef RC_RPN_H
#define RC_RPN_H 1
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <math.h>
#include <assert.h>

#define RPN_STACK_LIMIT 1024

double rpn_pop(void);
void rpn_push(double);
void rpn_reset();

bool rpn_is_numeric(char *);
double rpn_get_number(char *);

bool rpn_evaluate(const char *);

#endif
```



### rpn.c

{{uses from|Library|C Runtime|component1=va_list|component2=va_start|component3=fprintf|component4=vfprintf|component5=va_end|component6=NULL|component7=malloc|component8=strtok|component9=strtod|component10=strcpy|component11=strlen|component12=assert|component13=free}}

```c
#include "rpn.h"

static double stack[RPN_STACK_LIMIT];
static int stack_ip = 0;

static char *delim = " \t\n";

static void rpn_log(char *msg, ...)
{
  va_list al;
  va_start(al, msg);
  fprintf(stderr, "\n*** RC_RPN says: ");
  vfprintf(stderr, msg, al);
  fprintf(stderr, "\n");
  va_end(al);
}


// OPERATORS
static void rpn_add()
{
  rpn_push( rpn_pop() + rpn_pop() );
}
static void rpn_sub()
{
  double a, b;
  a = rpn_pop(); b = rpn_pop();
  rpn_push( b - a );
}
static void rpn_mul()
{
  rpn_push( rpn_pop() * rpn_pop() );
}
static void rpn_div()
{
  double a, b;
  a = rpn_pop(); b = rpn_pop();
  rpn_push( b / a );  
}
static void rpn_neg()
{
  rpn_push( -rpn_pop() );
}
static void rpn_swap()
{
  double a, b;
  a = rpn_pop(); b = rpn_pop();
  rpn_push(a); rpn_push(b);
}
// END OPERATORS

static struct op_func {
  char *opname;
  void (*func)();
} oplist[] = {
  { "+", rpn_add }, { "-", rpn_sub }, { "*", rpn_mul }, { "/", rpn_div },
  { "n", rpn_neg }, { "s", rpn_swap }, { NULL, NULL }
};

static void rpn_call_op(char *op)
{
  int i;
  
  for(i=0; oplist[i].opname != NULL; i++)
  {
    if ( strcmp(op, oplist[i].opname) == 0 )
    {
      oplist[i].func();
      return;
    }
  }
  rpn_log("unknown operator ignored");
}


double rpn_pop()
{
  if ( stack_ip > 0 ) return stack[--stack_ip];
  rpn_log("stack underflow; computations won't be right");
  return 0.0;
}

void rpn_push(double v)
{
  if ( stack_ip < RPN_STACK_LIMIT )
  {
    stack[ stack_ip++ ] = v;
  } else {
    rpn_log("stack overflow");
  }
}

void rpn_reset()
{
  stack_ip = 0;
}

bool rpn_is_numeric(char *e)
{ // lazy check; it says true even for "1.23hello"...
  char *end;
  double d = strtod(e, &end);
  if ( end == e ) return false;
  return true;
}

// this is meaningful only if e is already checked to be a possible number
// it's rendundant but...
double rpn_get_number(char *e)
{
  if ( !rpn_is_numeric(e) )
  {
    rpn_log("wrong call; return value will be wrong");
  }
  return strtod(e, NULL);
}

bool rpn_evaluate(const char *expr)
{
  char *tok;
  
  char *buf = malloc( strlen(expr) + 1); assert( buf != NULL );
  strcpy(buf, expr);

  for(tok = strtok(buf, delim); tok != NULL; tok = strtok(NULL, delim) )
  {
    if ( rpn_is_numeric(tok) )
    {
      rpn_push( rpn_get_number(tok) );
    } else {
      rpn_call_op(tok);
    }
  }
  free(buf);
  return true; // should check if it's all ok...
}
```



### 24 game

Now we have something able to evaluate simple expression; so here it is the game.
{{uses from|Library|C Runtime|component1=rand|component2=RAND_MAX|component3=malloc|component4=strlen|component5=strcpy|component6=strtok|component7=printf|component8=free|component9=ranged_rand|component10=fgets|component11=fabs}}

```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "rpn.h"

#define N_NUMBERS 4
#define MAX_EXPR_LEN 1024
#define EVAL_TO 24
#define TOLERANCE 0.00001

int ranged_rand(int min, int max)
{
  return min + (int)((double)(max - min) * (rand() / (RAND_MAX + 1.0)));
}

// to be ok (true) all numbers must appears and just once
bool check_expression(const char *expr, int *numbers)
{
  bool rv = true;
  char *tok;
  char *expr_copy = malloc(strlen(expr)+1);
  char nc[10];
  int i;
  const char *delim = " \t\n";

  strcpy(expr_copy, expr);
  for(i=0; i < 10; i++) nc[i] = 0;
  for(i=0; i < N_NUMBERS; i++)
  {
    nc[ numbers[i] ]++;
  }
  for (tok = strtok(expr_copy, delim); tok != NULL; tok = strtok(NULL, delim) )
  {
    if ( rpn_is_numeric(tok) )
    {
      if ( (strlen(tok) != 1) || (*tok < '1') || (*tok > '9') )
      {
	rv = false;
	printf("numbers must be integer and between 1 and 9\n");
	break;
      } else {
	int in = *tok - '0';
	if ( (nc[in] <= 0) ) { 
	  printf("you can't use more numbers than those given!\n");
	  rv = false; break;
	}
	nc[in]--;
      }
    }
  }
  free(expr_copy);
  if ( rv )
  {
    for(i=0; i < 10; i++) { 
      if ( nc[i] > 0 ) { 
	printf("you must use all numbers!\n");
	rv = false; break; 
      } 
    }
  }
  return rv;
}

int main()
{
  int n[N_NUMBERS], i;
  char expression[MAX_EXPR_LEN];
  double ee;
  bool playagain = true;

  while(playagain)
  {
    printf("The numbers are:\n");
    for(i=0; i < N_NUMBERS; i++)
    {
      n[i] = ranged_rand(1, 9);
      printf("n%d = %d\n", i, n[i]);
    }
    printf("\nNow enter the expression (in RPN) which evaluate to %d\n"
	   "Allowed operators: + - / * n"
           "[syntactic sugar for '0 Num -' which is not allowed because of 0]\n"
	   "You must use all numbers only once\n\n", EVAL_TO);
    for(;;)
    {
      printf("Enter the expression (must give %d): ", EVAL_TO);
      fgets(expression, MAX_EXPR_LEN, stdin);
      if ( !check_expression(expression, n) ) { continue;}
      if ( !rpn_evaluate(expression) ) continue;
      ee = rpn_pop();
      if ( fabs(ee - EVAL_TO) < TOLERANCE )
      {
	printf("\n\nYou got it! Well done!!\n\n");
	break;
      } else {
	printf("Your expression evaluate to %lf\n"
	       "Let's try again? (n say no) ", ee);
	fgets(expression, MAX_EXPR_LEN, stdin);
	if ( expression[0] == 'n' ) break;
      }
    }

    printf("\n\nPlay again? (n to say no) ");
    fgets(expression, MAX_EXPR_LEN, stdin);
    playagain = !(expression[0] == 'n');
  }

  return 0;
}
```

