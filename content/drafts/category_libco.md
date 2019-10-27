+++
title = "Category:Libco"
description = ""
date = 2011-02-17T22:35:45Z
aliases = []
[extra]
id = 9269
[taxonomies]
categories = []
tags = []
+++

{{library}}
libco is a tiny library that adds ''cooperative multithreading'', also known as ''coroutines'', to the [[C]] language.


```c
typedef void* cothread_t;

cothread_t co_active();
cothread_t co_create(unsigned int heapsize, void (*coentry)(void));
void       co_delete(cothread_t cothread);
void       co_switch(cothread_t cothread);
```


You can get libco from http://byuu.org/programming/
