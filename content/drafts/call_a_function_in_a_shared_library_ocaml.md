+++
title = "Call a function in a shared library/OCaml"
description = ""
date = 2010-05-17T21:14:16Z
aliases = []
[extra]
id = 7332
[taxonomies]
categories = []
tags = []
+++

Here is the file '''"dlffi.ml"''' which provides the interface to OCaml:


```ocaml

type lib_handle
type func_handle

type pointer

type arg_type =
  | Int of int
  | Float of float
  | Double of float
  | String of string
  | Ptr of pointer
  | Void

type return_type =
  | Return_int
  | Return_float
  | Return_double
  | Return_string
  | Return_ptr
  | Return_void

type rtld_flags =
  | RTLD_LAZY
  | RTLD_NOW
  | RTLD_GLOBAL
  | RTLD_LOCAL
  | RTLD_NODELETE
  | RTLD_NOLOAD
  | RTLD_DEEPBIND

external dlopen: libname:string -> flags:rtld_flags list -> lib_handle = "ml_dlopen"

external dlsym: lib:lib_handle -> func_name:string -> func_handle = "ml_dlsym"
external dlclose: lib:lib_handle -> unit = "ml_dlclose"

external fficall: func:func_handle -> args:arg_type array -> return:return_type -> arg_type = "ml_fficall"

```



--------


Here is the file '''"dlffi_stubs.c"''':


```c>#include <dlfcn.h

#include <ffi.h>

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static const int rtld_flags_table[] = {
  RTLD_LAZY,
  RTLD_NOW,
  RTLD_GLOBAL,
  RTLD_LOCAL,
  RTLD_NODELETE,
  RTLD_NOLOAD,
  RTLD_DEEPBIND,
};

static inline int
rtld_flags_val( value mask_list )
{
  int c_mask = 0; 
  while (mask_list != Val_emptylist) {
    value head = Field(mask_list, 0);
    c_mask |= rtld_flags_table[Long_val(head)];
    mask_list = Field(mask_list, 1);
  }
  return c_mask;
}

CAMLprim value
ml_dlopen( value libname, value ml_flags )
{
  void *lib_handle = dlopen(String_val(libname), rtld_flags_val(ml_flags));
  if (lib_handle == NULL) failwith(dlerror());
  return (value) lib_handle;
}

CAMLprim value
ml_dlsym( value lib_handle, value func_name )
{
  char *status;
  void (*func_handle)();
  (void)dlerror();
  *(void **)(&func_handle) = dlsym((void *)lib_handle, String_val(func_name));
  status = dlerror();
  if (status != NULL) failwith(status);
  return (value) func_handle;
}

CAMLprim value
ml_dlclose( value lib_handle )
{
  int status = dlclose((void *)lib_handle);
  if (status != 0) failwith("dlclose");
  return Val_unit;
}

#define Val_tagged(v, tag) \
  this = caml_alloc(1, tag); \
  Store_field(this, 0, v);

//define ALLOC_INT_PARAM 1
//define ALLOC_DOUBLE_PARAM 1
//define ALLOC_STRING_PARAM 1

CAMLprim value
ml_fficall( value func_handle, value ml_args, value ml_return )
{
  CAMLparam3(func_handle, ml_args, ml_return);
  CAMLlocal2(ml_ret, this);

  unsigned int i, nargs;
  ffi_cif    cif;
  ffi_type   **arg_types;
  void       **arg_values;
  ffi_status status;
  ffi_type   *rtype;
  int ffi_prep_cif_failed = 0;

  nargs = Wosize_val(ml_args);

  arg_types  = (ffi_type **) malloc(nargs * sizeof(ffi_type *));
  arg_values = (void **) malloc(nargs * sizeof(void *));

  /* Set up the parameters */
  for (i = 0; i < nargs; i++) {
    value v = Field(ml_args, i);
    switch (Tag_val(v)) {
      case 0:  /* Int */
#if defined(ALLOC_INT_PARAM)
      { int *arg;
        arg = malloc(sizeof(int));
        *arg = Long_val(Field(v,0));
        arg_types[i] = &ffi_type_sint;
        arg_values[i] = arg;
      } break;
#else
      { arg_types[i] = &ffi_type_sint;
        Field(v,0) >>= 1;
        arg_values[i] = &Field(v,0);
      } break;
#endif
      case 1:  /* Float */
      { float *arg;
        arg = malloc(sizeof(float));
        *arg = (float) Double_val(Field(v,0));
        arg_types[i] = &ffi_type_float;
        arg_values[i] = arg;
      } break;
      case 2:  /* Double */
#if defined(ALLOC_DOUBLE_PARAM)
      { double *arg;
        arg = malloc(sizeof(double));
        *arg = Double_val(Field(v,0));
        arg_types[i] = &ffi_type_double;
        arg_values[i] = arg;
      } break;
#else
      { arg_types[i] = &ffi_type_double;
        arg_values[i] = &Double_val(Field(v,0));
      } break;
#endif
      case 3:  /* String */
#if defined(ALLOC_STRING_PARAM)
      { char *arg;
        int len = caml_string_length(Field(v,0)) + 1;
        arg = malloc(sizeof(char) * len);
        memcpy(arg, String_val(Field(v,0)), len);
        arg_types[i] = &ffi_type_float;
        arg_values[i] = &arg;
      } break;
#else
      { arg_types[i] = &ffi_type_pointer;
        arg_values[i] = &Byte(v,0);
      } break;
#endif
      case 4:  /* Ptr */
      { arg_types[i] = &ffi_type_pointer;
        arg_values[i] = &Field(v,0);
      } break;
      case 5:  /* Void */
        caml_invalid_argument("fficall");
        break;
    }
  }

  switch (Int_val(ml_return)) {  /* return_type */
    case 0:  /* Return_int */
      rtype = &ffi_type_sint;
      break;
    case 1:  /* Return_float */
      rtype = &ffi_type_float;
      break;
    case 2:  /* Return_double */
      rtype = &ffi_type_double;
      break;
    case 3:  /* Return_string */
      rtype = &ffi_type_pointer;
      break;
    case 4:  /* Return_ptr */
      rtype = &ffi_type_pointer;
      break;
    case 5:  /* Return_void */
      rtype = &ffi_type_uint;
      break;
  }

  status = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, nargs, rtype, arg_types);
  if (status != FFI_OK) {
    ffi_prep_cif_failed = 1;
    goto freeandfail;
  }

  switch (Int_val(ml_return)) {
    case 0:  /* Return_int */
    { int result;
      ffi_call(&cif, (void (*)(void)) func_handle, &result, arg_values);
      ml_ret = Val_tagged( Val_long(result), 0);
    } break;
    case 1:  /* Return_float */
    { float result;
      ffi_call(&cif, (void (*)(void)) func_handle, &result, arg_values);
      ml_ret = Val_tagged( caml_copy_double(result), 1);
    } break;
    case 2:  /* Return_double */
    { double result;
      ffi_call(&cif, (void (*)(void)) func_handle, &result, arg_values);
      ml_ret = Val_tagged( caml_copy_double(result), 2);
    } break;
    case 3:  /* Return_string */
    { char *result;
      ffi_call(&cif, (void (*)(void)) func_handle, &result, arg_values);
      ml_ret = Val_tagged( caml_copy_string((result == NULL ? "" : result)), 3);
    } break;
    case 4:  /* Return_ptr */
    { void *result;
      ffi_call(&cif, (void (*)(void)) func_handle, &result, arg_values);
      ml_ret = Val_tagged( (value) result, 4);
    } break;
    case 5:  /* Return_void */
    { ffi_arg result;
      ffi_call(&cif, (void (*)(void)) func_handle, &result, arg_values);
      ml_ret = Val_int(0);
    } break;
  }

freeandfail:
  for (i = 0; i < nargs; i++) {
    switch (Tag_val(Field(ml_args, i))) {
      case 0:  /* Int */
#if defined(ALLOC_INT_PARAM)
        free(arg_values[i]);
#endif
        break;
      case 1:  /* Float */
        free(arg_values[i]);
        break;
      case 2:  /* Double */
#if defined(ALLOC_DOUBLE_PARAM)
        free(arg_values[i]);
#endif
        break;
      case 3:  /* String */
#if defined(ALLOC_STRING_PARAM)
      { char **arg;
        arg = arg_values[i];
        free(*arg);
      }
#endif
        break;
      case 4:  /* Ptr */
        break;
    }
  }
  free(arg_values);
  free(arg_types);

  if (ffi_prep_cif_failed)
    failwith("ffi_prep_cif");

  CAMLreturn(ml_ret);
}

```



--------


Here is the '''"Makefile"''':


```make
FFI_LIBS := $(shell pkg-config --libs libffi)
FFI_CFLAGS := $(shell pkg-config --cflags libffi)

all: test_opt

dlffi_stubs.o: dlffi_stubs.c
	ocamlc -g -c -ccopt $(FFI_CFLAGS) $<

dlldlffi_stubs.so: dlffi_stubs.o
	ocamlmklib -o dlffi_stubs $< -ldl $(FFI_LIBS)

dlffi.cmo: dlffi.ml
	ocamlc -c $<

dlffi.cmx: dlffi.ml
	ocamlopt -c $<

dlffi.cma: dlffi.cmo  dlldlffi_stubs.so
	ocamlc -a -o $@ $< -dllib -ldlffi_stubs -cclib -ldl -cclib $(FFI_LIBS)

dlffi.cmxa:  dlffi.cmx  dlldlffi_stubs.so
	ocamlopt -a -o $@ $< -cclib -ldlffi_stubs -cclib -ldl -cclib $(FFI_LIBS)

fakelib.so: fakelib.c
	gcc -g -shared -nostartfiles $< -o $@

.PHONY: vim
vim:
	vim dlffi_stubs.c dlffi.ml Makefile test.ml fakelib.c

test: dlffi.cma fakelib.so
	ocaml dlffi.cma test.ml

test.opt: test.ml dlffi.cmxa fakelib.so
	ocamlopt -g -o $@ -I . dlffi.cmxa $<

test_opt: test.opt
	./$<

.PHONY: all clean test test_opt
clean:
	rm -f *.[oa] *.so *.cm[ixoa] *.cmxa *.opt

```

 sed -i "s/        /\t/g" Makefile



--------


Here is a test file named '''"test.ml"''':


```ocaml
open Dlffi

let handle_return = function
  | Int v    -> Printf.printf "ocaml: got (Int %d)\n%!" v
  | Float v  -> Printf.printf "ocaml: got (Float %g)\n%!" v
  | Double v -> Printf.printf "ocaml: got (Double %g)\n%!" v
  | String v -> Printf.printf "ocaml: got (String \"%s\")\n%!" v
  | Ptr _    -> Printf.printf "ocaml: got (Ptr)\n%!"
  | Void     -> Printf.printf "ocaml: got (Void)\n%!"

let main1() =
  let lib = dlopen "./fakelib.so" [RTLD_LAZY] in
  let func = dlsym ~lib ~func_name:"func_static_handle" in
  handle_return(fficall ~func ~args:[| Float 1.1 |] ~return:Return_int);
  handle_return(fficall ~func ~args:[| Float 2.2 |] ~return:Return_int);
  handle_return(fficall ~func ~args:[| Float 3.3 |] ~return:Return_int);
  dlclose ~lib;
;;

let main2() =
  let lib = dlopen "./fakelib.so" [RTLD_LAZY] in
  let func = dlsym ~lib ~func_name:"func_string_param" in
  handle_return(fficall ~func ~args:[| String "one" |] ~return:Return_string);
  dlclose ~lib;
;;

let main3() =
  let lib = dlopen "./fakelib.so" [RTLD_LAZY] in
  let func = dlsym ~lib ~func_name:"func_int_param" in
  handle_return(fficall ~func ~args:[| Int 244 |] ~return:Return_int);
  dlclose ~lib;
;;

let main4() =
  let lib = dlopen "./fakelib.so" [RTLD_LAZY] in
  let func = dlsym ~lib ~func_name:"func_double_param" in
  handle_return(fficall ~func ~args:[| Double 211.6 |] ~return:Return_double);
  dlclose ~lib;
;;

let main5() =
  let lib = dlopen "./fakelib.so" [RTLD_LAZY] in
  let func = dlsym ~lib ~func_name:"func_float_param" in
  handle_return(fficall ~func ~args:[| Float 211.7 |] ~return:Return_float);
  dlclose ~lib;
;;

let main6() =
  let lib = dlopen "./fakelib.so" [RTLD_LAZY] in
  let func = dlsym ~lib ~func_name:"func_void_param" in
  handle_return(fficall ~func ~args:[| |] ~return:Return_void);
  dlclose ~lib;
;;

let () =
  print_newline();  main1();
  print_newline();  main2();
  print_newline();  main3();
  print_newline();  main4();
  print_newline();  main5();
  print_newline();  main6();
;;
```



--------


Here is a test library '''"fakelib.c"''':


```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>

int func_static_handle(float v)
{
  static int handle = 1;
  fprintf(stderr, "fun called %d times, arg: %g\n", handle, v);
  fflush(stderr);  /* ocaml's stderr is different from C's stderr */
  return handle++;
}

char *msgs[] = {
  "alpha",
  "beta",
  "gamma",
  "delta",
  "epsilon",
  "zeta",
};

char *func_string_param(const char *s)
{
  static int handle = 0;
  char *ret;
  ret = &(msgs[handle % 6][0]);
  handle++;
  fprintf(stderr, "c: got string (%s) index[%d], returning: [%s]\n", s, handle, ret);
  fflush(stderr);
  return ret;
  //return NULL;  /* test the special case with NULL */
}

int func_int_param(const int d)
{
  printf("c: got int (%d)\n", d); fflush(stdout);
  return 107;
}

double func_double_param(const double d)
{
  printf("c: got double (%g)\n", d); fflush(stdout);
  return 233.1;
}

float func_float_param(const float f)
{
  printf("c: got float (%g)\n", f); fflush(stdout);
  return 233.2;
}

void func_void_param(void)
{
  printf("c: void function\n"); fflush(stdout);
}

```


If you find some use for this module, you can reuse this code under '''public domain'''.
