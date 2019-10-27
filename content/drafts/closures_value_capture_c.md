+++
title = "Closures/Value capture/C"
description = ""
date = 2012-12-20T21:32:26Z
aliases = []
[extra]
id = 10598
[taxonomies]
categories = []
tags = []
+++

Very quickly hacked up dynamically typed environment, with no garbage collection, and <code>abort</code> for error handling.


```c>#include <stdio.h

#include <stdarg.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

struct function;
struct number;
struct symbol;
struct string;
struct cons;

enum type_tag {
  FUN, NUM, SYM, STR, CON
};

typedef struct obj *val;

#define nil ((val) 0)
#define nao ((val) 1) /* "not an object" */

struct number {
  int n;
};

struct string {
  const char *s;
  int dynamic;
};

struct symbol {
  val name;
};

struct cons {
  val car, cdr;
};

struct function {
  val nargs;
  val has_env;
  val env;
  union {
    /* First arg is environment */
    val (*f0)(val);
    val (*f1)(val, val);
    val (*f2)(val, val, val);
    /* No environment */
    val (*n0)(void);
    val (*n1)(val);
    val (*n2)(val, val);
  } u;
};

struct obj {
  enum type_tag tt;
  union {
    struct function fun;
    struct number num;
    struct string str;
    struct symbol sym;
    struct cons cons;
  } u;
};

val t, x;
val sym_list = nil;

val mkobj(enum type_tag tt)
{
  val v = malloc(sizeof *v);
  if (!v)
    abort();
  v->tt = tt;
  return v;
}

val cons(val car, val cdr)
{
  val c = mkobj(CON);
  c->u.cons.car = car;
  c->u.cons.cdr = cdr;
  return c;
}

val car(val cons)
{
  if (cons == nil)
    return nil;
  assert (cons->tt == CON);
  return cons->u.cons.car;
}

val cdr(val cons)
{
  if (cons == nil)
    return nil;
  assert (cons->tt == CON);
  return cons->u.cons.cdr;
}

val num(int n)
{
  val vn = mkobj(NUM);
  vn->u.num.n = n;
  return vn;
}

int cnum(val vn)
{
  assert (vn->tt == NUM);
  return vn->u.num.n;
}

val str(const char *s)
{
  size_t size = strlen(s) + 1;
  val sv = mkobj(STR);
  char *sd = malloc(size);

  if (!sv || !sd)
    abort();

  memcpy(sd, s, size);
  sv->u.str.s = sd;
  sv->u.str.dynamic = 1;
  return sv;
}

val lit(const char *s)
{
  val sv = mkobj(STR);

  if (!sv)
    abort();

  sv->u.str.s = s;
  sv->u.str.dynamic = 0;
  return sv;
}

const char *cstr(val vs)
{
  assert (vs->tt == STR);
  return vs->u.str.s;
}

val equal(val left, val right)
{
  if (left == right) /* same object */
    return t;

  if (left == nil || right == nil) /* nil only equal to itself */
    return nil;

  if (left->tt != right->tt) /* different types are not equal */
    return nil;

  switch (left->tt) {
  case FUN: /* identity equivalence */
  case SYM: /* ditto, of course */
    /* left and right are different objects, so ... */
    break;
  case NUM: /* numeric equivalence */
    if (left->u.num.n == right->u.num.n)
      return t;
    break;
  case STR: /* case sensitive equality */
    if (strcmp(left->u.str.s, right->u.str.s) == 0)
      return t;
    break;
  case CON: /* similar structure and equal atoms! */
    if (equal(car(left), car(right)) && equal(cdr(left), cdr(right)))
      return t;
    break;
  }

  return nil;
}

val assoc(val list, val key)
{
  for (; list != nil; list = cdr(list)) {
    val item = car(list);
    if (equal(key, car(item)))
      return item;
  }
  return nil;
}

val intern(val name)
{
  val exist = assoc(sym_list, name);

  if (exist) {
    return cdr(exist);
  } else {
    val sym = mkobj(SYM);

    sym->u.sym.name = name;
    sym_list = cons(cons(name, sym), sym_list);
    return sym;
  }
}

/*
 * Hoist C functions into environment-carrying closures.
 */
val func_f0(val env, val (*cfun)(val))
{
  val f = mkobj(FUN);
  f->u.fun.nargs = num(0);
  f->u.fun.has_env = t;
  f->u.fun.env = env;
  f->u.fun.u.f0 = cfun;
  return f;
}

val func_f1(val env, val (*cfun)(val, val))
{
  val f = mkobj(FUN);
  f->u.fun.nargs = num(1);
  f->u.fun.has_env = t;
  f->u.fun.env = env;
  f->u.fun.u.f1 = cfun;
  return f;
}

val func_f2(val env, val (*cfun)(val, val, val))
{
  val f = mkobj(FUN);
  f->u.fun.nargs = num(2);
  f->u.fun.has_env = t;
  f->u.fun.env = env;
  f->u.fun.u.f2 = cfun;
  return f;
}

/*
 * Hoist C functions into environment-free functions
 */
val func_n0(val (*cfun)(void))
{
  val f = mkobj(FUN);
  f->u.fun.nargs = num(0);
  f->u.fun.has_env = nil;
  f->u.fun.env = nil;
  f->u.fun.u.n0 = cfun;
  return f;
}

val func_n1(val (*cfun)(val))
{
  val f = mkobj(FUN);
  f->u.fun.nargs = num(1);
  f->u.fun.has_env = nil;
  f->u.fun.env = nil;
  f->u.fun.u.n1 = cfun;
  return f;
}

val func_n2(val (*cfun)(val, val))
{
  val f = mkobj(FUN);
  f->u.fun.nargs = num(2);
  f->u.fun.has_env = nil;
  f->u.fun.env = nil;
  f->u.fun.u.n2 = cfun;
  return f;
}


val funcall(val f, ...)
{
  va_list vl;
  int argc;
  val arg[5];

  assert (f->tt == FUN);

  va_start (vl, f);

  for (argc = 0; argc < 5; argc++) {
    val v = va_arg (vl, val);
    if (v == nao)
      break;
    arg[argc] = v;
  }

  if (argc != cnum(f->u.fun.nargs))
    abort();

  if (f->u.fun.has_env) {
    switch (cnum(f->u.fun.nargs)) {
    case 0:
      return f->u.fun.u.f0(f->u.fun.env);
    case 1:
      return f->u.fun.u.f1(f->u.fun.env, arg[0]);
    case 2:
      return f->u.fun.u.f2(f->u.fun.env, arg[0], arg[1]);
    }
  } else {
    switch (cnum(f->u.fun.nargs)) {
    case 0:
      return f->u.fun.u.n0();
    case 1:
      return f->u.fun.u.n1(arg[0]);
    case 2:
      return f->u.fun.u.n2(arg[0], arg[1]);
    }
  }

  abort();
}

void init(void)
{
  t = intern(lit("t"));
  x = intern(lit("x"));
}

val square(val env)
{
  val xbind = assoc(env, x); /* look up binding of variable x in env */
  val xval = cdr(xbind);     /* value is the cdr of the binding cell */
  return num(cnum(xval) * cnum(xval));
}

int main(void)
{
  int i;
  val funlist = nil, iter;

  init();

  for (i = 0; i < 10; i++) {
    val closure_env = cons(cons(x, num(i)), nil);
    funlist = cons(func_f0(closure_env, square), funlist);
  }

  for (iter = funlist; iter != nil; iter = cdr(iter)) {
    val fun = car(iter);
    val square = funcall(fun, nao);

    printf("%d\n", cnum(square));
  }
  return 0;
}
```

