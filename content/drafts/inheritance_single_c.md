+++
title = "Inheritance/Single/C"
description = ""
date = 2010-02-04T15:33:58Z
aliases = []
[extra]
id = 5335
[taxonomies]
categories = []
tags = []
+++

==Manual Object System==
Although [[C]] is not considered an OO language, you can do the following, which provides a semblance of an OO infrastructure allowing Inheritance and Polymorphism. The original [[C++]] was a front end which translated C++ code to C. (C++ was much simpler in those days.)"

```c
/* Animal.h */
#ifndef _ANIMAL_H 
#define _ANIMAL_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct object *Animal;

extern char *ObjSpeak(Animal a);
extern Animal ObjClone(Animal a);
extern void ObjDestroy(Animal a);
#endif
```



```c
/* AnimalProt.h */
#include "Animal.h"
typedef struct sclass *Class;
typedef void (*CloneFctn)(Animal s, Animal clo);
typedef char * (*SpeakFctn)(Animal s);
typedef void (*DestroyFctn)(Animal s);
typedef void (*ClsRecInit)(Class c);

typedef struct object {
    Class class;
} SObject, *Object;

typedef struct sclass {
    size_t csize;		/* size of the class instance */
    char  *cname;		/* name of the class */
    Class  parent;		/* parent class */
    ClsRecInit crInit;          /* handle method inheritance for class record */

    CloneFctn clone;		/* clone function */
    SpeakFctn speak;		/* speak function */
    DestroyFctn del;		/* delete the object */
} sClass;

#define O_INHERIT (void *)-1
extern void ClassRecInit(Class c);
extern void DfltClsInit(Class c);
extern sClass boc;
```


```c
/* Animal.c */
#include "AnimalProt.h"

static 
Animal obj_copy( Animal s, Class c )
{
    size_t size = s->class->csize;
    Animal clo;
    if (c->parent) 
        clo = obj_copy( s, c->parent);
    else {
        clo = (Animal )malloc( size );
        memcpy(clo, s, size);
    }

    if (clo)
        c->clone( s, clo );
    return clo;
}

static
void obj_del( Animal s, Class c )
{
    if (c->del)
        c->del(s);
    if (c->parent)
        obj_del( s, c->parent);
    else
        free(s);
}

Animal ObjClone( Animal s )
{ return obj_copy( s, s->class ); }

char * ObjSpeak( Animal s )
{ 
    return s->class->speak(s); 
}

void ObjDestroy( Animal s )
{ if (s) obj_del( s, s->class ); }

void ClassRecInit( Class c)
{
    if (c->crInit) {
        (*c->crInit)(c);
        c->crInit = NULL;
    }
}
static
void baseClsRecInit(Class c )
{
	if ((O_INHERIT == c->speak) && c->parent)
		c->speak = c->parent->speak;
}
void DfltClsInit(Class c)
{
	if (c->parent && c->parent->crInit) {
		ClassRecInit(c->parent);
	}
	printf("Initializing class %s\n", c->cname);
	baseClsRecInit(c);
}
/* * * * * * */
static
void baseClone( Animal s, Animal clone)
{  
    clone->class = s->class;
}

static
char *baseSpeak(Animal s)
{
    return "Hello, I'm an animal";
}

sClass boc = { sizeof(SObject), "Animal", NULL, &baseClsRecInit,
    &baseClone, &baseSpeak, NULL };
Class AnimalClass = &boc;
```



```c
/* DogProt.h */
#include "AnimalProt.h"

typedef struct sDogPart {
    double weight;
    char color[32];
    char name[24];
} DogPart;

typedef struct sDog *Dog;

struct sDog {
    Class   class;			// parent structure
    DogPart dog;
};

extern void InitDog(DogPart *dogp, char *name, char *color, double weight);

extern sClass dogc;
```



```c
/* Dog.c */
/** * * * * * * * * * * * * * * * * * *
 * Dog - a class derived from Animal 
 * * * * * * * * * * * * * * * * * * */
#include "DogProt.h"

static
void dogClone( Animal s, Animal c)
{  
    Dog src = (Dog)s;
    Dog clone = (Dog)c;
    clone->dog = src->dog;	/* no pointers so strncpys not needed */
}

static
char *dogSpeak( Animal s)
{
    Dog d = (Dog)s;
    static char  response[90];
    sprintf(response, "woof! woof! My name is %s. I'm a %s %s", 
            d->dog.name, d->dog.color, d->class->cname);
    return response;
}

sClass dogc = { sizeof(struct sDog), "Dog", &boc, &DfltClsInit,
    &dogClone, &dogSpeak, NULL };
Class DogClass = &dogc;


void InitDog(DogPart *dogp, char *name, char *color, double weight)
{
    dogp->weight = weight;
    strncpy(dogp->name, name, 23);
    strncpy(dogp->color, color, 31);
}

Animal NewDog( char *name, char *color, double weight )
{
    Dog dog = (Dog)malloc(DogClass->csize);
    if (dog) {
        ClassRecInit(DogClass);
        dog->class = DogClass;
        InitDog(&dog->dog, name, color, weight);
    }
    return (Animal)dog;
}
```


```c
/* Dog.h */
#ifndef _DOG_H
#define _DOG_H
#include "Animal.h"
extern Animal NewDog(char *name, char *color, double weight);
#endif
```



```c
/* LabProt.h */
#include "DogProt.h"
/** * * * * * * * * * * * * * * * * * *
 * Lab - a class derived from Dog 
 * * * * * * * * * * * * * * * * * * */
typedef struct sLabPart {
    int  friendliness;
    int  tailIsWagging;	
} LabPart;

typedef struct sLab *Lab;

struct sLab {
    Class   class;			// parent structure
    DogPart dog;
    LabPart lab;			// my part
};

extern sClass labc;
```


```c
/* Lab.c */
#include "LabProt.h"

void InitLab(LabPart *lab)
{
    lab->friendliness = 5;
    lab->tailIsWagging = 1;
}
static
void labClone( Animal s, Animal c)
{  
    Lab src = (Lab)s;
    Lab clone = (Lab)c;
    clone->lab = src->lab;
}

sClass labc = { sizeof(struct sLab), "Lab", &dogc, &DfltClsInit,
    &labClone, O_INHERIT, NULL };
Class LabClass = &labc;

Animal NewLab( char *name, char *color, double weight )
{
    Lab dog = (Lab)malloc(LabClass->csize);
    if (dog) {
        ClassRecInit(LabClass);
        dog->class = LabClass;
        InitDog( &dog->dog, name, color, weight);
        InitLab( &dog->lab);
    }
    return (Animal)dog;
}
```


```c
/* Lab.h */
#include "Dog.h"
extern Animal NewLab(char *name, char *color, double weight);
```



```c
/* CollieProt.h */
/** * * * * * * * * * * * * * * * * * *
 * Collie - a class derived from Dog 
 * * * * * * * * * * * * * * * * * * */
#include "DogProt.h"

typedef struct sColliePart {
    double  height;
} ColliePart;

typedef struct sCollie *Collie;

struct sCollie {
    Class   class;			// parent structure
    DogPart dog;
    ColliePart collie;			// my part
};

extern sClass colliec;
```


```c
/* Collie.c */
#include "CollieProt.h"

static
void collieClone( Animal s, Animal c)
{  
    Collie src = (Collie)s;
    Collie clone = (Collie)c;
    clone->collie = src->collie;
}

void InitCollie(ColliePart *collie, double height)
{
    collie->height = 25.0;
}

sClass colliec = { sizeof(struct sCollie), "Collie", &dogc, &DfltClsInit,
    &collieClone, O_INHERIT, NULL };
Class CollieClass = &colliec;

Animal NewCollie( char *name, char *color, double weight, double height )
{
    Collie dog = (Collie)malloc(CollieClass->csize);
    if (dog) {
        ClassRecInit(CollieClass);
        dog->class = CollieClass;
        InitDog( &dog->dog, name, color, weight);
        InitCollie( &dog->collie, height);
    }
    return (Animal)dog;
}
```


```c
/* Collie.h */
#include "Dog.h"
extern Animal NewCollie(char *name, char *color, double weight, double height);
```



```c
/* CatProt.h */
#include "AnimalProt.h"
/* * * * * * * * * * * * */
/* Cat - a derived class */

typedef struct sCatPart {
    char color[32];
    char *name;
    int  age;
} CatPart;

typedef struct sCat *Cat;

struct sCat {
    Class   class;			// parent structure
    CatPart catpart;
};

extern sClass catcls;
```


```c
/* Cat.c */
#include "CatProt.h"

static
void catClone( Animal s, Animal c)
{  
    Cat src = (Cat)s;
    Cat clone = (Cat)c;
    clone->catpart.name = strdup(src->catpart.name);
}

static
char *catSpeak(Animal s)
{
    Cat f = (Cat)s;
    static char  response[90];
    sprintf(response, "My name is %s. I'm a %d mo. old %s wiley %s", 
            f->catpart.name, f->catpart.age, f->catpart.color,
            f->class->cname);
    return response;
}

void catDel(Animal s)
{
    Cat f = (Cat)s;
    CatPart *catpart = &f->catpart;	
    if (catpart->name) {
        free(catpart->name);
    }
}

sClass catcls = { sizeof(struct sCat), "Cat", &boc, &DfltClsInit, 
    &catClone, &catSpeak, &catDel };
Class CatClass = &catcls;

Animal NewCat( char *name, char *color, int age )
{
    Cat cat = (Cat)malloc(CatClass->csize);
    if (cat) {
        CatPart *catpart = &(cat->catpart);
        ClassRecInit(CatClass);
        cat->class = CatClass;
        catpart->name = strdup(name);
        strncpy(catpart->color, color, 31);
        catpart->age = age;
    }
    return (Animal)cat;
}

/** * Now wasn' that fun. * **/
```


```c
/* Cat.h */
#include "Animal.h"
extern Animal NewCat(char *name, char *color, int age);
```



```c
#include "Cat.h"
#include "Lab.h"
#include "Collie.h"

int main(int argc, char *argv[])
{
    Animal  kara = NewCat( "Kara", "grey", 15 );
    Animal  bruce = NewDog("Bruce", "yellow", 85.0 );
    Animal  bigrex = NewLab("Rex", "chocolate", 65.0 );
    Animal  sandy = NewCollie("Sandy", "yellow", 35.0, 21.0 );
    printf("Ok, created pets\n");

    printf("Kara says %s\n", ObjSpeak(kara));
    printf("Bruce says %s\n", ObjSpeak(bruce));
    printf("Big Rex says %s\n", ObjSpeak(bigrex));
    printf("Sandy says %s\n", ObjSpeak(sandy));
    ObjDestroy(kara);
    ObjDestroy(bruce);
    ObjDestroy(bigrex);
    ObjDestroy(sandy);

    return 0;
}
```

