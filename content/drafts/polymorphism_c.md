+++
title = "Polymorphism/C"
description = ""
date = 2010-06-20T02:39:30Z
aliases = []
[extra]
id = 1979
[taxonomies]
categories = []
tags = []
+++

{{Programming-example-page|Polymorphism}}

```c
/* After reading this you may understand */ 
/* why Bjarne Stroustrup's invented C++  */
#if defined( _WIN32 ) || defined( MSC_VER )
#define FN_PTR(x) (& x)
#else
#define FN_PTR(x) (x)
#endif

typedef struct Point
{
  int x;
  int y;
  void (*dtor)();   /* virtual */
  void (*print)();  /* virtual */
} Point;

Point* Point_new0()
{
  Point* pthis = malloc( sizeof( Point ) );
  memset(pthis, 0, sizeof( Point ) );
  pthis->dtor  = FN_PTR(Point_dtor);
  pthis->print = FN_PTR(Point_print);
}

Point* Point_new1(int x0)
{
  Point* pthis = malloc( sizeof( Point ) );
  pthis->x = x0;
  pthis->y = 0;
  pthis->dtor  = FN_PTR(Point_dtor);
  pthis->print = FN_PTR(Point_print);
}

Point* Point_new2(int x0, int y0)
{
  Point* pthis = malloc( sizeof( Point ) );
  pthis->x = x0;
  pthis->y = y0;
  pthis->dtor  = FN_PTR(Point_dtor);
  pthis->print = FN_PTR(Point_print);
}

void Point_delete(Point** pthis)
{
  if(pthis && *pthis)
  {
    (*pthis)->dtor();
    free(*pthis); *pthis = NULL;
  }
}

Point* Point_copy(Point* p)
{
  Point* pthis = malloc( sizeof( Point ) );
  memcpy(pthis, p, sizeof( Point ) );
  pthis->dtor  = FN_PTR(Point_dtor);
  pthis->print = FN_PTR(Point_print);
  return pthis;
}

int Point_getX(Point* pthis) { return pthis->x; }
int Point_getY(Point* pthis) { return pthis->y; }
int Point_setX(Point* pthis, int x0) { pthis->x = x0; }
int Point_setY(Point* pthis, int y0) { pthis->y = y0; }
void Point_print() { printf("Point\n"); }
void Point_dtor() {}

// Trick: This way Circle.x, Circle.y, Circle.r are available
typedef union Circle
{
  Point point;
  struct _Circle
  {
    Point point;
    int r;
  };
} Circle;


Circle* Circle_new0()
{
  Circle* pthis = malloc( sizeof( Circle ) );
  memset(pthis, 0, sizeof( Circle ) );
  pthis->dtor  = FN_PTR(Circle_dtor);
  pthis->print = FN_PTR(Circle_print);
}

Circle* Circle_new1(int x0)
{
  Circle* pthis = malloc( sizeof( Circle ) );
  pthis->x = x0;
  pthis->y = 0;
  pthis->r = 0;
  pthis->dtor  = FN_PTR(Circle_dtor);
  pthis->print = FN_PTR(Circle_print);
}

Circle* Circle_new2(int x0, int y0)
{
  Circle* pthis = malloc( sizeof( Circle ) );
  pthis->x = x0;
  pthis->y = y0;
  pthis->r = 0;
  pthis->dtor  = FN_PTR(Circle_dtor);
  pthis->print = FN_PTR(Circle_print);
}

Circle* Circle_new3(int x0, int y0, int r0)
{
  Circle* pthis = malloc( sizeof( Circle ) );
  pthis->x = x0;
  pthis->y = y0;
  pthis->r = r0;
  pthis->dtor  = FN_PTR(Circle_dtor);
  pthis->print = FN_PTR(Circle_print);
}

Circle* Circle_newP0(Point* p)
{
  Circle* pthis = malloc( sizeof( Circle ) );
  pthis->x = p->x;
  pthis->y = p->y;
  pthis->r = 0;
  pthis->dtor  = FN_PTR(Circle_dtor);
  pthis->print = FN_PTR(Circle_print);
}

Circle* Circle_newP1(Point* p, int r0)
{
  Circle* pthis = malloc( sizeof( Circle ) );
  pthis->x = p->x;
  pthis->y = p->y;
  pthis->r = r0;
  pthis->dtor  = FN_PTR(Circle_dtor);
  pthis->print = FN_PTR(Circle_print);
}

void Circle_delete(Circle** pthis)
{
  if(pthis && *pthis)
  {
    (*pthis)->dtor();
    free(*pthis); *pthis = NULL;
  }
}

Circle* Circle_copy(Circle* c)
{
  Circle* pthis = malloc( sizeof( Circle ) );
  memcpy(pthis, c, sizeof( Circle ) );
  pthis->dtor  = FN_PTR(Circle_dtor);
  pthis->print = FN_PTR(Circle_print);
  return pthis;
}

int Circle_getX(Circle* pthis) { return pthis->x; }
int Circle_getY(Circle* pthis) { return pthis->y; }
int Circle_getR(Circle* pthis) { return pthis->r; }
int Circle_setX(Circle* pthis, int x0) { pthis->x = x0; }
int Circle_setY(Circle* pthis, int y0) { pthis->y = y0; }
int Circle_setR(Circle* pthis, int r0) { pthis->r = r0; }
void Circle_print() { printf("Circle\n"); }
void Circle_dtor() {}

int main()
{
   Point* p = Point_new0();
   Point* c = (Point*)Circle_new0();
   p->print();
   c->print();     
   return 0;
}
```

