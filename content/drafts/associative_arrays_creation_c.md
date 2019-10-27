+++
title = "Associative arrays/Creation/C"
description = ""
date = 2016-02-12T23:58:26Z
aliases = []
[extra]
id = 10351
[taxonomies]
categories = []
tags = []
+++

There are no associative arrays in the C language. Some libraries provide hash tables, red-black trees, or other data structures that can become associative arrays.

* Back to [[Associative arrays/Creation]].
* Back to [[Associative arrays/Iteration]].

==From Scratch==
A hash table can be implemented with the following. Because of this example's simplicity, it comes with some restrictions on use and capabilities: It can't be resized automatically, if you try to insert more values than its capacity it will freeze, the hashing function is very simple, etc. All are fixable with additional logic or using a library:


```c>#include <stdio.h

#include <stdlib.h>

typedef struct {
    int size;
    void **keys;
    void **values;
} hash_t;

hash_t *hash_new (int size) {
    hash_t *h = calloc(1, sizeof (hash_t));
    h->keys = calloc(size, sizeof (void *));
    h->values = calloc(size, sizeof (void *));
    h->size = size;
    return h;
}

int hash_index (hash_t *h, void *key) {
    int i = (int) key % h->size;
    while (h->keys[i] && h->keys[i] != key)
        i = (i + 1) % h->size;
    return i;
}

void hash_insert (hash_t *h, void *key, void *value) {
    int i = hash_index(h, key);
    h->keys[i] = key;
    h->values[i] = value;
}

void *hash_lookup (hash_t *h, void *key) {
    int i = hash_index(h, key);
    return h->values[i];
}

int main () {
    hash_t *h = hash_new(15);
    hash_insert(h, "hello", "world");
    hash_insert(h, "a", "b");
    printf("hello => %s\n", hash_lookup(h, "hello"));
    printf("herp => %s\n", hash_lookup(h, "herp"));
    printf("a => %s\n", hash_lookup(h, "a"));
    return 0;
}
```


==Libraries==

### Judy

Example using [http://judy.sourceforge.net/ Judy].

{{libheader|Judy}}


```c>#include <stdio.h

#include <Judy.h>

int main()
{
  Pvoid_t assarray = (Pvoid_t) NULL;
  PWord_t value;
  int retval;

  /* populating */
  JSLI(value, assarray, "red");
  *value = 0xff0000;
  JSLI(value, assarray, "green");
  *value = 0x00ff00;
  JSLI(value, assarray, "blue");
  *value = 0x0000ff;

  /* retrieving existent */
  JSLG(value, assarray, "blue");
  printf("blue is #%06lx\n", (unsigned long)*value);

  /* unknown key */
  JSLG(value, assarray, "nonexistingkey");
  if ( value == NULL ) { fprintf(stderr, "key 'nonexistingkey' does not exists\n"); }

  /* deleting */
  JSLD(retval, assarray, "red");
  JSLG(value, assarray, "red");
  if ( value == NULL ) { fprintf(stderr, "key red does not exist anymore\n"); }

  JudySLFreeArray(&assarray, PJE0);

  return 0;
}
```


{{libheader|Judy}}

We can easily iterate over pair of keys (indexes) and values.


```c>#include <stdio.h

#include <Judy.h>

#define MAXLINELEN 256

int main()
{
  Pvoid_t assoc_arr = (Pvoid_t) NULL;
  PWord_t val;

  uint8_t idx[MAXLINELEN];

  // insert some values
  JSLI(val, assoc_arr, "hello");
  *val = 4;
  JSLI(val, assoc_arr, "world");
  *val = 8;
  JSLI(val, assoc_arr, "!");
  *val = 16;

  // iterate over indexes-values
  idx[0] = '\0';

  JSLF(val, assoc_arr, idx);
  while(val != NULL) {
    printf("'%s' -> %d\n", idx, *val);
    JSLN(val, assoc_arr, idx);
  }

  JudySLFreeArray(&assoc_arr, PJE0);
  return 0;
}
```


===POSIX hsearch()===
POSIX defines hcreate(), hdestroy() and hsearch() to manage a hash table. If you have a [[Unix]] system or clone, then your libc probably has these functions, so there is no extra library to install.

These functions have some major limitations:

* You can only have one hash table, in the entire program!
* There is no way to delete an entry from the table!
* There is no way to iterate all keys in the table!

The Linux manual page [http://www.kernel.org/doc/man-pages/online/pages/man3/hsearch.3.html hsearch(3)] contains a short example.


### =To create the hash table=

The hash table has a fixed capacity. <code>hcreate(50)</code> creates a table for 50 entries. The library might increase 50 to a convenient value (perhaps 64 being a power of 2, or 67 being a prime number). The hash table might have only 64 or 67 slots. Each slot might hold one entry, or one list of entries. Access to the hash table is near [[O|O(1)]], but slows to [[O|O(n)]] as the slots become full.

The hash table is an associative array of key-value pairs. Each key must be a NUL-terminated string. Each value is a void *.


### =To fetch or store=

To use the hash table as an associative array, this program defines fetch() and store().

{{libheader|POSIX}}

```c>#include <inttypes.h
	/* intptr_t, PRIxPTR */
#include <search.h>	/* hcreate(), hsearch() */
#include <stdio.h>	/* perror(), printf() */
#include <stdlib.h>	/* exit() */

void
fail(char *message)
{
	perror(message);
	exit(1);
}

/*
 * Must hcreate() the hash table before calling fetch() or store().
 *
 * Because p->data is a pointer, fetch() and store() cast between
 * void * and intptr_t.
 */

/* Fetch value from the hash table. */
int
fetch(const char *key, intptr_t *value)
{
	ENTRY e = {key: (char *)key}, *p;
	p = hsearch(e, FIND);
	if (p) {
		*value = (intptr_t)p->data;
		return 1;
	} else
		return 0;
}

/* Store key-value pair into the hash table. */
void
store(const char *key, intptr_t value)
{
	/*
	 * hsearch() may insert a new entry or find an existing entry
	 * with the same key. hsearch() ignores e.data if it finds an
	 * existing entry. We must call hsearch(), then set p->data.
	 */
	ENTRY e = {key: (char *)key}, *p;
	p = hsearch(e, ENTER);
	if (p == NULL)
		fail("hsearch");
	p->data = (void *)value;
}

/*
 * Use the hash table to map color strings to integer values,
 * like "red" => 0xff0000.
 */
int
main()
{
	static const char *const keys[] =
	    {"red", "orange", "yellow", "green", "blue", "white", "black"};
	intptr_t value;
	int i;

	/* First, create an empty table that can hold 50 entries. */
	if (hcreate(50) == 0)
		fail("hcreate");

	/*
	 * Some colors from CSS2,
	 * http://www.w3.org/TR/CSS2/syndata.html#value-def-color
	 */
	store("red",	0xff0000);
	store("orange",	0x123456);  /* Insert wrong value! */
	store("green",	0x008000);
	store("blue",	0x0000ff);
	store("white",	0xffffff);
	store("black",	0x000000);
	store("orange", 0xffa500);  /* Replace with correct value. */

	for (i = 0; i < sizeof(keys) / sizeof(keys[0]); i++) {
		if (fetch(keys[i], &value))
			printf("%s has value %06" PRIxPTR "\n",
			    keys[i], value);
		else
			printf("%s is not in table\n", keys[i]);
	}

	/*
	 * DO NOT CALL hdestroy().
	 *
	 * With BSD libc, hdestroy() would call free() with each key in
	 * table. Our keys are static strings, so free() would crash.
	 */
	return 0;
}
```



```txt
red has value ff0000
orange has value ffa500
yellow is not in table
green has value 008000
blue has value 0000ff
white has value ffffff
black has value 000000
```



### =To delete or iterate=

{{libheader|POSIX}}

```c>#include <inttypes.h

#include <search.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void
fail(char *message)
{
	perror(message);
	exit(1);
}

/* A key-value pair */
struct pair {
	/* prev is q_forw, so insque(d, &head) sets head.prev = d */
	struct pair	*prev;		/* q_forw */
	struct pair	*next;		/* q_back */
	int32_t		 key;
	int32_t		 value;
	int		 deleted;
};

/*
 * A circular queue of all pairs in the hash table.
 * head.next begins a list of all pairs in order of insertion.
 */
struct pair head = {&head, &head};

/* Fetch value from the hash table. */
int
fetch(int32_t key, int32_t *value)
{
	ENTRY e, *p;
	char buf[16];

	snprintf(buf, sizeof buf, "%"PRId32, key);
	e.key = buf;
	p = hsearch(e, FIND);
	if (p) {
		struct pair *d = p->data;
		if (d->deleted)
			return 0;
		else {
			*value = d->value;
			return 1;
		}
	} else
		return 0;
}

/* Store key-value pair into the hash table. */
void
store(int32_t key, int32_t value)
{
	ENTRY e, *p;
	char buf[16];

	snprintf(buf, sizeof buf, "%"PRId32, key);
	e.key = buf;
	p = hsearch(e, ENTER);
	if (p == NULL)
		fail("hsearch");

	if (p->key == buf) {
		/* Allocate and initialize a new pair. */
		struct pair *d = malloc(sizeof *d);
		if (d == NULL)
			fail("malloc");
		d->key = key;
		d->value = value;
		d->deleted = 0;

		/* Allocate space for key, apart from buf. */
		p->key = strdup(buf);
		if (p->key == NULL)
			fail("strdup");

		/*
		 * Insert the new pair into the hash table's entry, and
		 * into the circular queue.
		 */
		p->data = d;
		insque(d, &head);
	} else {
		/* Replace the value. */
		struct pair *d = p->data;
		d->value = value;
		if (d->deleted) {
			/* Restore a deleted key. */
			insque(d, &head);
			d->deleted = 0;
		}
	}
}

/* Delete key from the hash table. */
int
delete(int32_t key)
{
	ENTRY e, *p;
	char buf[16];

	snprintf(buf, sizeof buf, "%"PRId32, key);
	e.key = buf;
	p = hsearch(e, FIND);
	if (p) {
		struct pair *d = p->data;
		if (d->deleted)
			return 0;
		else {
			remque(d);
			return d->deleted = 1;
		}
	} else
		return 0;	
}

int
main()
{
	struct pair *p;
	int32_t value;
	int i;

	if (hcreate(50) == 0)
		fail("hcreate");

	store(1, mrand48());
	store(2, mrand48());
	store(3, mrand48());
	for (i = 0; i < 3; i++)
		store(mrand48(), mrand48());
	store(4, mrand48());
	delete(1) || puts("1 is not deleted");
	delete(2) || puts("2 is not deleted");
	delete(5) || puts("5 is not deleted");
	store(1, mrand48());
	store(3, mrand48());
	fetch(2, &value) ? puts("2 is in table") : puts("2 is missing");
	fetch(4, &value) ? puts("4 is in table") : puts("4 is missing");
	fetch(6, &value) ? puts("6 is in table") : puts("6 is missing");

	puts("Iterating the hash table:");
	for (p = head.next; p != &head; p = p->next) {
		printf("  %"PRId32" => %"PRId32"\n", p->key, p->value);
	}

	return 0;
}
```



```txt
5 is not deleted
2 is missing
4 is in table
6 is missing
Iterating the hash table:
  3 => 252797108
  1368775034 => 1918061247
  66927828 => -487786166
  684483038 => -1786318902
  4 => 1648047133
  1 => -1327126111
```


====hdestroy()====
hdestroy() is almost impossible to use. With BSD libc, hdestroy() will call free() with each key in the table. With other systems, hdestroy() might leak memory, because the program has no way to iterate the keys to free them. Most programs keep the hash table and never call hdestroy().

===BSD dbopen()===
[[BSD]] provides [http://www.openbsd.org/cgi-bin/man.cgi?query=dbopen&apropos=0&sektion=0&manpath=OpenBSD+Current&arch=i386&format=html dbopen()] in <db.h>. This is Berkeley DB 1.85. Because dbopen() often puts a database on disk, one easily forgets that dbopen(NULL, ...) can put a small database in memory. When the type is DB_BTREE or DB_HASH, then the database is an associative array.

* Warning: some GNU/Linux systems have a dbopen(3) manual page without a real dbopen() function. See [http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=337581 Debian bug #337581].

Every key and value is a *void, needs a cast to the correct type. Because BSD also has <err.h>, I remove fail() and use err().

{{libheader|BSD libc}}
{{works with|OpenBSD|4.8}}


```c>#include <sys/types.h


#include <err.h>	/* err() */
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>	/* printf(), puts() */
#include <string.h>	/* memset() */
#include <db.h>		/* dbopen() */

/*
 * color_example:
 *   Maps color strings to integer values, like "red" => 0xff0000.
 */
void
color_example(void)
{
	DB *db;
	DBT key, value;
	int i, r;

	puts("color_example:");

	/* Create an empty table. */
	db = dbopen(NULL, O_RDWR, 0777, DB_HASH, NULL);
	if (db == NULL)
		err(1, "dbopen");

	/* Add keys => values to table. */
	{
		char *keys[] = { "red", "green", "blue" };
		int values[] = { 0xff0000, 0x00ff00, 0x0000ff };

		for (i = 0; i < 3; i++) {
			/* key.size must count the '\0' byte */
			key.data = keys[i];
			key.size = strlen(keys[i]) + 1;
			value.data = &values[i];
			value.size = sizeof values[i];

			/*
			 * Insert key, value into table. DB will
			 * allocate its own storage for key, value.
			 */
			if (db->put(db, &key, &value, 0) < 0)
				err(1, "db->put");
		}
	}

	/* Check if keys exist. */
	{
		char *keys[] = { "blue", "green", "orange", "red" };

		for (i = 0; i < 4; i++) {
			key.data = (void *)keys[i];
			key.size = strlen(keys[i]) + 1;

			r = db->get(db, &key, &value, 0);
			if (r < 0)
				err(1, "db->get");
			else if (r > 0) {
				printf("\t'%s' is not in table\n",
				    (char *)key.data);
			} else {
				printf("\t'%s' has value %06x\n",
				    (char *)key.data, *(int *)value.data);
			}
		}
	}

	/* Destroy table. */
	db->close(db);
}


/*
 * number_example:
 *   Maps numbers to strings or numbers, like 2 => 2.71828 or 4 => "four".
 *
 * First I need a VALUE that can either be a string or a number.
 */
typedef struct value {
	int v_type;
#define T_NUM	0x1	/* use v_num */
#define T_STR	0x2	/* use v_str */

	union {
		double u_num;
		char *u_str;
	} v_union;
#define v_num v_union.u_num
#define v_str v_union.u_str
} VALUE;

void
number_example(void)
{
	DB *db;
	DBT key, value;
	VALUE v, *vp;
	double d;
	int i, r;

	puts("number_example:");

	db = dbopen(NULL, O_RDWR, 0777, DB_HASH, NULL);
	if (db == NULL)
		err(1, "dbopen");

	/* Add numeric values. */
	{
		double keys[] = { 2, 3, 4, 5.6 };
		double values[] = { 2.71828, 3.14159, 4.47214, 7.8 };

		for (i = 0; i < 4; i++) {
			memset(&v, 0, sizeof v);
			v.v_type = T_NUM;
			v.v_num = values[i];

			key.data = &keys[i];
			key.size = sizeof keys[i];
			value.data = &v;
			value.size = sizeof v;

			if (db->put(db, &key, &value, 0) < 0)
				err(1, "db->put");
		}
	}

	/*
	 * Add string values.
	 *
	 * For this example, all of my values will be static string
	 * constants. This removes the need to free(vp->v_str)
	 * when I replace or delete a value.
	 */
	{
		double keys[] = { 4, 8, 10 };
		char *values[] = { "four", "eight", "ten" };

		for (i = 0; i < 3; i++) {
			memset(&v, 0, sizeof v);
			v.v_type = T_STR;
			v.v_str = values[i];

			key.data = &keys[i];
			key.size = sizeof keys[i];
			value.data = &v;
			value.size = sizeof v;

			/*
			 * db->put can replace an entry (so I can change
			 * it from 4 => 4.47214 to 4 => "four").
			 *
			 * I am storing the strings outside the DB.
			 * To put them inside the DB, I would need to
			 * remove the v.v_str pointer and append each
			 * string data to value.data.
			 */
			if (db->put(db, &key, &value, 0) < 0)
				err(1, "db->put");
		}
	}

	/* Delete key 8. */
	d = 8;
	key.data = &d;
	key.size = sizeof d;
	r = db->del(db, &key, 0);
	if (r < 0)
		err(1, "db->del");

	/* Iterate all keys. */
	r = db->seq(db, &key, &value, R_FIRST);
	if (r < 0)
		err(1, "db->seq");
	else if (r > 0)
		puts("\tno keys!");
	else {
		printf("\tall keys: %g", *(double *)key.data);
		while ((r = db->seq(db, &key, &value, R_NEXT)) == 0)
			printf(", %g", *(double *)key.data);
		if (r < 0)
			err(1, "db->seq");
		puts("");
	}

	/* Check if keys exist. */
	{
		double keys[] = { 2, 3, 4, 5.6, 7, 8, 10 };

		for (i = 0; i < 7; i++) {
			key.data = &keys[i];
			key.size = sizeof keys[i];

			r = db->get(db, &key, &value, 0);
			if (r < 0)
				err(1, "db->get");
			else if (r > 0) {
				printf("\t%g is not in the table\n", keys[i]);
				continue;
			}

			vp = (VALUE *)value.data;
			if (vp->v_type & T_NUM) {
				printf("\t%g has value %g\n",
				    keys[i], vp->v_num);
			} else if (vp->v_type & T_STR) {
				printf("\t%g has value '%s'\n",
				    keys[i], vp->v_str);
			} else {
				printf("\t%g has invalid value\n",
				    keys[i]);
			}
		}
	}

	db->close(db);
}

int
main()
{
	color_example();
	number_example();
	return 0;
}
```


Output:


```txt
color_example:
        'blue' has value 0000ff
        'green' has value 00ff00
        'orange' is not in table
        'red' has value ff0000
number_example:
        all keys: 2, 3, 5.6, 4, 10
        2 has value 2.71828
        3 has value 3.14159
        4 has value 'four'
        5.6 has value 7.8
        7 is not in the table
        8 is not in the table
        10 has value 'ten'
```



### BSD sys/tree.h

{{libheader|BSD libc}}
{{works with|OpenBSD|4.8}}


```c>#include <sys/tree.h


#include <err.h>	/* err() */
#include <stdio.h>	/* printf(), puts() */
#include <stdlib.h>	/* calloc(), free() */
#include <string.h>	/* strcmp() */

/*
 * color_example:
 *   Maps color strings to integer values, like "red" => 0xff0000.
 *
 * I will use a red-black tree of type 'struct ctree', which contains
 * nodes of type 'struct cnode'.
 */
struct cnode {
	RB_ENTRY(cnode) entry;
	char *key;
	int value;
};

int
cnodecmp(struct cnode *a, struct cnode *b)
{
	return strcmp(a->key, b->key);
}

RB_HEAD(ctree, cnode);
RB_GENERATE(ctree, cnode, entry, cnodecmp)

void
color_example(void)
{
	struct ctree head = RB_INITIALIZER(&head); /* an empty tree */
	struct cnode n, *np, *op;
	int i;

	puts("color_example:");

	/* Add keys => values to tree. */
	{
		char *keys[] = { "red", "green", "blue" };
		int values[] = { 0xff0000, 0x00ff00, 0x0000ff };

		for (i = 0; i < 3; i++) {
			if ((np = calloc(1, sizeof np[0])) == NULL)
				err(1, "calloc");
			np->key = keys[i];
			np->value = values[i];
			RB_INSERT(ctree, &head, np);
		}
	}

	/* Check if keys exist. */
	{
		char *keys[] = { "blue", "green", "orange", "red" };

		for (i = 0; i < 4; i++) {
			n.key = keys[i];
			np = RB_FIND(ctree, &head, &n);

			if (np) {
				printf("\t'%s' has value %06x\n",
				    np->key, np->value);
			} else {
				printf("\t'%s' is not in tree\n",
				    n.key);
			}
		}
	}

	/* Free tree. */
	for (np = RB_MIN(ctree, &head); np != NULL; np = op) {
		op = RB_NEXT(ctree, &head, np);
		RB_REMOVE(ctree, &head, np);
		free(np);
	}
}


/*
 * number_example:
 *   Maps numbers to strings or numbers, like 2 => 2.71828 or 4 => "four".
 *
 * This node has a value that can either be a string or a number.
 */
struct dnode {
	RB_ENTRY(dnode) entry;
	double key;
	int v_type;
#define T_NUM	0x1	/* use v_num */
#define T_STR	0x2	/* use v_str */

	union {
		double u_num;
		char *u_str;
	} v_union;
#define v_num v_union.u_num
#define v_str v_union.u_str
};

int
dnodecmp(struct dnode *a, struct dnode *b)
{
	double aa = a->key;
	double bb = b->key;
	return (aa < bb) ? -1 : (aa > bb);
}

RB_HEAD(dtree, dnode);
RB_GENERATE(dtree, dnode, entry, dnodecmp)

void
number_example(void)
{
	struct dtree head = RB_INITIALIZER(&head);
	struct dnode n, *np, *op;
	int i;

	puts("number_example:");

	/* Add numeric values. */
	{
		double keys[] = { 2, 3, 4, 5.6 };
		double values[] = { 2.71828, 3.14159, 4.47214, 7.8 };

		for (i = 0; i < 4; i++) {
			if ((np = calloc(1, sizeof np[0])) == NULL)
				err(1, "calloc");
			np->key = keys[i];
			np->v_type = T_NUM;
			np->v_num = values[i];
			RB_INSERT(dtree, &head, np);
		}
	}

	/*
	 * Add string values.
	 *
	 * For this example, all of my values will be static string
	 * constants. This removes the need to free(np->v_str)
	 * when I replace or delete a value.
	 */
	{
		double keys[] = { 4, 8, 10 };
		char *values[] = { "four", "eight", "ten" };

		for (i = 0; i < 3; i++) {
			/* 
			 * This shows how to add or replace a value
			 * in the tree (so I can change an entry
			 * from 4 => 4.47214 to 4 => "four").
			 */
			n.key = keys[i];
			if (np = RB_FIND(dtree, &head, &n)) {
				np->v_type = T_STR;
				np->v_str = values[i];
			} else if (np = calloc(1, sizeof np[0])) {
				np->key = keys[i];
				np->v_type = T_STR;
				np->v_str = values[i];
				RB_INSERT(dtree, &head, np);
			} else
				err(1, "calloc");
		}
	}

	/* Delete key 8. */
	n.key = 8;
	if (np = RB_FIND(dtree, &head, &n))
		RB_REMOVE(dtree, &head, np);

	/* Iterate all keys. */
	i = 1;
	RB_FOREACH(np, dtree, &head) {
		if (i) {
			printf("\tall keys: %g", np->key);
			i = 0;
		} else
			printf(", %g", np->key);
	}
	if (i)
		puts("\tno keys!");
	else
		puts("");

	/* Check if keys exist. */
	{
		double keys[] = { 2, 3, 4, 5.6, 7, 8, 10 };

		for (i = 0; i < 7; i++) {
			n.key = keys[i];
			np = RB_FIND(dtree, &head, &n);

			if (np == NULL) {
				printf("\t%g is not in the tree\n", keys[i]);
			} else if (np->v_type & T_NUM) {
				printf("\t%g has value %g\n",
				    keys[i], np->v_num);
			} else if (np->v_type & T_STR) {
				printf("\t%g has value '%s'\n",
				    keys[i], np->v_str);
			} else {
				printf("\t%g has invalid value\n",
				    keys[i]);
			}
		}
	}

	/* Free tree. */
	for (np = RB_MIN(dtree, &head); np != NULL; np = op) {
		op = RB_NEXT(dtree, &head, np);
		RB_REMOVE(dtree, &head, np);
		free(np);
	}
}

int
main()
{
	color_example();
	number_example();
	return 0;
}
```


Output:


```txt
color_example:
        'blue' has value 0000ff
        'green' has value 00ff00
        'orange' is not in tree
        'red' has value ff0000
number_example:
        all keys: 2, 3, 4, 5.6, 10
        2 has value 2.71828
        3 has value 3.14159
        4 has value 'four'
        5.6 has value 7.8
        7 is not in the tree
        8 is not in the tree
        10 has value 'ten'
```

