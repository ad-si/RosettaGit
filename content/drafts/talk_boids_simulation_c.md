+++
title = "Talk:Boids simulation/C"
description = ""
date = 2013-01-12T18:49:51Z
aliases = []
[extra]
id = 12765
[taxonomies]
categories = []
tags = []
+++

== osx ==

I got this working on os x.  Here's the makefile


```make
CFLAGS='-framework GLUT -framework OpenGL'

boid:
```


and here is the diff:


```diff
*** boid.orig.c	2013-01-12 13:38:00.000000000 -0500
--- boid.c	2013-01-12 13:42:50.000000000 -0500
***************
*** 1,9 ****
  #include <stdio.h>
  #include <stdlib.h>
  #include <math.h>
! #include <GL/glut.h>
! #include <GL/gl.h>
! #include <GL/glu.h>
  #include <sys/time.h>
  #include <unistd.h>
   
--- 1,9 ----
  #include <stdio.h>
  #include <stdlib.h>
  #include <math.h>
! #include <GLUT/glut.h>
! #include <OpenGL/gl.h>
! #include <OpenGL/glu.h>
  #include <sys/time.h>
  #include <unistd.h>
   
***************
*** 30,79 ****
  // 3D vector stuff
  typedef struct { flt x[3]; } vec;
   
! inline void vscale(vec *a, flt r) {
  	a->x[0] *= r;
  	a->x[1] *= r;
  	a->x[2] *= r;
  }
   
! inline void vmuladd_to(vec *a, const vec *b, flt r) {
  	a->x[0] += r * b->x[0];
  	a->x[1] += r * b->x[1];
  	a->x[2] += r * b->x[2];
  }
   
! inline void vadd_to(vec *a, const vec *b) {
  	a->x[0] += b->x[0];
  	a->x[1] += b->x[1];
  	a->x[2] += b->x[2];
  }
   
! inline vec vadd(vec a, vec b) {
  	return (vec) {{	a.x[0] + b.x[0],
  			a.x[1] + b.x[1],
  			a.x[2] + b.x[2] }};
  }
   
! inline vec vsub(vec a, vec b) {
  	return (vec) {{	a.x[0] - b.x[0],
  			a.x[1] - b.x[1],
  			a.x[2] - b.x[2] }};
  }
   
! inline flt vlen2(vec a) {
  	return a.x[0]*a.x[0] + a.x[1]*a.x[1] + a.x[2]*a.x[2];
  }
   
! inline flt vdist2(vec a, vec b) { return vlen2(vsub(a, b)); }
   
! inline vec vcross(vec a, vec b) {
  	return (vec) {{
  		a.x[1]*b.x[2] - a.x[2]*b.x[1],
  		a.x[2]*b.x[0] - a.x[0]*b.x[2],
  		a.x[0]*b.x[1] - a.x[1]*b.x[0] }};
  }
   
! inline void vnormalize(vec *a) {
  	flt r = sqrt(a->x[0]*a->x[0] + a->x[1]*a->x[1] + a->x[2]*a->x[2]);
  	if (!r) return;
  	a->x[0] /= r;
--- 30,79 ----
  // 3D vector stuff
  typedef struct { flt x[3]; } vec;
   
! void vscale(vec *a, flt r) {
  	a->x[0] *= r;
  	a->x[1] *= r;
  	a->x[2] *= r;
  }
   
! void vmuladd_to(vec *a, const vec *b, flt r) {
  	a->x[0] += r * b->x[0];
  	a->x[1] += r * b->x[1];
  	a->x[2] += r * b->x[2];
  }
   
! void vadd_to(vec *a, const vec *b) {
  	a->x[0] += b->x[0];
  	a->x[1] += b->x[1];
  	a->x[2] += b->x[2];
  }
   
! vec vadd(vec a, vec b) {
  	return (vec) {{	a.x[0] + b.x[0],
  			a.x[1] + b.x[1],
  			a.x[2] + b.x[2] }};
  }
   
! vec vsub(vec a, vec b) {
  	return (vec) {{	a.x[0] - b.x[0],
  			a.x[1] - b.x[1],
  			a.x[2] - b.x[2] }};
  }
   
! flt vlen2(vec a) {
  	return a.x[0]*a.x[0] + a.x[1]*a.x[1] + a.x[2]*a.x[2];
  }
   
! flt vdist2(vec a, vec b) { return vlen2(vsub(a, b)); }
   
! vec vcross(vec a, vec b) {
  	return (vec) {{
  		a.x[1]*b.x[2] - a.x[2]*b.x[1],
  		a.x[2]*b.x[0] - a.x[0]*b.x[2],
  		a.x[0]*b.x[1] - a.x[1]*b.x[0] }};
  }
   
! void vnormalize(vec *a) {
  	flt r = sqrt(a->x[0]*a->x[0] + a->x[1]*a->x[1] + a->x[2]*a->x[2]);
  	if (!r) return;
  	a->x[0] /= r;
***************
*** 112,122 ****
   
  camera_t camera = { -PI/4, 0, 100, {{0, 0, 0}} };
   
! unsigned int hash_xy(int x, int y) {
! 	inline unsigned int ror(unsigned int a, int d) {
  		return (a<<d) | (a>>(32-d));
  	}
   
  	unsigned int h = 0x12345678, tmp;
  	tmp = x;
  	h += ror(h, 15) ^ ror(tmp, 5);
--- 112,122 ----
   
  camera_t camera = { -PI/4, 0, 100, {{0, 0, 0}} };
   
! 	unsigned int ror(unsigned int a, int d) {
  		return (a<<d) | (a>>(32-d));
  	}
   
+ unsigned int hash_xy(int x, int y) {
  	unsigned int h = 0x12345678, tmp;
  	tmp = x;
  	h += ror(h, 15) ^ ror(tmp, 5);
***************
*** 518,523 ****
--- 518,525 ----
  	glutInitWindowSize(600, 400);
   
  	gwin = glutCreateWindow("Boids");
+ 	glutDisplayFunc(render);
+ 
   
  	glutIgnoreKeyRepeat(1);
  	glutKeyboardFunc(keydown);
```

