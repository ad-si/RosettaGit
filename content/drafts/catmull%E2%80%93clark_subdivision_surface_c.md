+++
title = "Catmull–Clark subdivision surface/C"
description = ""
date = 2012-05-01T22:39:43Z
aliases = []
[extra]
id = 10189
[taxonomies]
categories = []
tags = []
+++

{{libheader|GLUT}}
Full C code, OpenGL program.  Looooong.  Keybindings of interest: '<' and '>' for subdivision steps; 'w' toggles wireframe mode; arrow keys and space for rotation; 'm' for switching models; misc keys: p, l, a, z, s, p, q.

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <math.h>

#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glut.h>

/* --------- GL stuff ----------*/
int gwin;
GLfloat rot[] = { 20, 40, 0 };
GLfloat rotx = 0, roty = 1;
GLfloat ambient[] = { .5f, .5f, .5f, 1.f };
GLfloat diffuse[] = { .5f, .5f, .5f, .6f };
GLfloat litepos[] = { 0, 2, 3, 1 };
GLfloat litepos2[] = { 0, -2, 5, 1 };
GLfloat color[] = { .3, 1, .6 };
GLfloat hole[] = { .7, .4, 0 };
GLfloat color2[] = { .5, .5, .5 };
GLfloat hole2[] = { .5, .2, 0 };
GLfloat red[] = {1, 0, 0};
GLfloat zpos = -6;

int max_depth = 7;
int show_parent = 1;
int wireframe_mode = 1;
int face_mode = 1;
int model_idx = 0;
int interp_norm = 0;

#define new_struct(type, var) type var = calloc(sizeof(type##_t), 1)
#define len(l) ((l)->n)
#define elem(l, i) ((l)->buf[i])
#define foreach(i, e, l) for (i = 0, e = len(l) ? elem(l,i) : 0;	\
				i < len(l);				\
				e = (++i == len(l) ? 0 : elem(l, i)))
typedef struct {
	int n, alloc;
	void **buf;
} list_t, *list;

list list_new()
{
	new_struct(list, r);
	return r;
}
void list_del(list l)
{
	if (!l) return;
	if (l->alloc) free(l->buf);
	free(l);
}

int list_push(list lst, void *e)
{
	void **p;
	int na;

	if (len(lst) >= lst->alloc) {
		na = lst->alloc * 2;
		if (!na) na = 4;

		assert(p = realloc(lst->buf, sizeof(void*) * na));

		lst->alloc = na;
		lst->buf = p;
	}
	elem(lst, len(lst)) = e;
	return len(lst)++;
}

typedef struct { GLfloat x, y, z; } coord_t, *coord;

#define vadd(a, b) vadd_p(&(a), &(b))
coord vadd_p(coord a, coord b)
{
	a->x += b->x;
	a->y += b->y;
	a->z += b->z;
	return a;
}

coord vsub(coord a, coord b, coord c)
{
	c->x = a->x - b->x;
	c->y = a->y - b->y;
	c->z = a->z - b->z;
	return c;
}

coord vcross(coord a, coord b, coord c)
{
	c->x = a->y * b->z - a->z * b->y;
	c->y = a->z * b->x - a->x * b->z;
	c->z = a->x * b->y - a->y * b->x;
	return c;
}

#define vdiv(a, b) vdiv_p(&(a), b)
coord vdiv_p(coord a, GLfloat l)
{
	a->x /= l;
	a->y /= l;
	a->z /= l;
	return a;
}

coord vnormalize(coord a)
{
	return vdiv_p(a, sqrt(a->x * a->x + a->y * a->y + a->z * a->z));
}

coord vneg(coord a)
{
	a->x = -a->x; a->y = -a->y; a->z = -a->z;
	return a;
}

#define vmadd(a, b, s, c) vmadd_p(&(a), &(b), s, &(c))
coord vmadd_p(coord a, coord b, double s, coord c)
{
	c->x = a->x + s * b->x;
	c->y = a->y + s * b->y;
	c->z = a->z + s * b->z;
	return c;
}

typedef struct vertex_t {
	coord_t pos, avg_norm;
	list e, f;
	struct vertex_t * v_new;
	int idx;
} *vertex, vertex_t;

typedef struct {
	list f;
	vertex v[2];
	vertex e_pt; /* edge point for catmul */
	coord_t avg;
} *edge, edge_t;

typedef struct {
	list e, v;
	coord_t norm;
	vertex avg; /* average of all vertices, i.e. face point */
} *face, face_t;

typedef struct { list e, v, f; } model_t, *model;

/* ------ global data stuff ------ */
list models = 0;
int model_pos = 0;

model model_new()
{
	new_struct(model, m);
	m->e = list_new();
	m->v = list_new();
	m->f = list_new();
	return m;
}

vertex vertex_new()
{
	new_struct(vertex, v);
	v->e = list_new();
	v->f = list_new();
	v->idx = -1;
	return v;
}

void vertex_del(vertex v) {
	list_del(v->e);
	list_del(v->f);
	free(v);
}

edge edge_new()
{
	new_struct(edge, e);
	e->f = list_new();
	return e;
}
void edge_del(edge e) {
	list_del(e->f);
	free(e);
}

face face_new()
{
	new_struct(face, f);
	f->e = list_new();
	f->v = list_new();
	return f;
}
void face_del(face f) {
	list_del(f->e);
	list_del(f->v);
	free(f);
}

void model_del(model m)
{
	int i;
	void *p;

	foreach(i, p, m->v) vertex_del(p);
	foreach(i, p, m->e) edge_del(p);
	foreach(i, p, m->f) face_del(p);

	list_del(m->e);
	list_del(m->v);
	list_del(m->f);

	free(m);
}

int model_add_vertex(model m, GLfloat x, GLfloat y, GLfloat z)
{
	vertex v = vertex_new();
	v->pos.x = x; v->pos.y = y; v->pos.z = z;
	return v->idx = list_push(m->v, v);
}

int model_add_edge(model m, vertex v1, vertex v2)
{
	edge e = edge_new();

	e->v[0] = v1;
	e->v[1] = v2;
	list_push(v1->e, e);
	list_push(v2->e, e);

	return list_push(m->e, e);
}

int model_add_edge_i(model m, int i1, int i2)
{
	assert(i1 < len(m->v) && i2 < len(m->v));
	return model_add_edge(m, elem(m->v, i1), elem(m->v, i2));
}

edge model_edge_by_v(model m, vertex v1, vertex v2)
{
	int i;
	edge e;
	foreach(i, e, v1->e) {
		if ((e->v[0] == v2 || e->v[1] == v2))
			return e;
	}
	i = model_add_edge(m, v1, v2);
	return elem(m->e, i);
}

#define quad_face(m, a, b, c, d) model_add_face(m, 4, a, b, c, d)
#define tri_face(m, a, b, c) model_add_face(m, 3, a, b, c)
#define face_v(f, i) ((vertex)(f->v->buf[i]))
int model_add_face_v(model m, list vl)
{
	int i, n = len(vl);
	vertex v0, v1;
	edge e;
	face f = face_new();

	v0 = elem(vl, 0);
	for (i = 1; i <= n; i++, v0 = v1) {
		v1 = elem(vl, i % len(vl));
		list_push(v1->f, f);

		e = model_edge_by_v(m, v0, v1);

		list_push(e->f, f);
		list_push(f->e, e);
		list_push(f->v, v1);
	}

	return list_push(m->f, f);
}

int model_add_face(model m, int n, ...)
{
	int i, x;
	list lst = list_new();

	va_list ap;
	va_start(ap, n);

	for (i = 0; i < n; i++) {
		x = va_arg(ap, int);
		list_push(lst, elem(m->v, x));
	}

	va_end(ap);
	x = model_add_face_v(m, lst);
	list_del(lst);
	return x;
}

void model_norms(model m)
{
	int i, j, n;
	face f;
	vertex v, v0, v1;

	coord_t d1, d2, norm;
	foreach(j, f, m->f) {
		n = len(f->v);
		foreach(i, v, f->v) {
			v0 = elem(f->v, i ? i - 1 : n - 1);
			v1 = elem(f->v, (i + 1) % n);
			vsub(&(v->pos), &(v0->pos), &d1);
			vsub(&(v1->pos), &(v->pos), &d2);
			vcross(&d1, &d2, &norm);
			vadd(f->norm, norm);
		}
		if (i > 1) vnormalize(&f->norm);
	}

	foreach(i, v, m->v) {
		foreach(j, f, v->f)
			vadd(v->avg_norm, f->norm);
		if (j > 1) vnormalize(&(v->avg_norm));
	}
	printf("New model: %d faces\n", len(m->f));
}

vertex face_point(face f)
{
	int i;
	vertex v;

	if (!f->avg) {
		f->avg = vertex_new();
		foreach(i, v, f->v)
			if (!i) f->avg->pos = v->pos;
			else    vadd(f->avg->pos, v->pos);

		vdiv(f->avg->pos, len(f->v));
	}
	return f->avg;
}

#define hole_edge(e) (len(e->f)==1)
vertex edge_point(edge e)
{
	int i;
	face f;

	if (!e->e_pt) {
		e->e_pt = vertex_new();
		e->avg = e->v[0]->pos;
		vadd(e->avg, e->v[1]->pos);
		e->e_pt->pos = e->avg;

		if (!hole_edge(e)) {
			foreach (i, f, e->f)
				vadd(e->e_pt->pos, face_point(f)->pos);
			vdiv(e->e_pt->pos, 4);
		} else
			vdiv(e->e_pt->pos, 2);

		vdiv(e->avg, 2);
	}

	return e->e_pt;
}

#define hole_vertex(v) (len((v)->f) != len((v)->e))
vertex updated_point(vertex v)
{
	int i, n = 0;
	edge e;
	face f;
	coord_t sum = {0, 0, 0};

	if (v->v_new) return v->v_new;

	v->v_new = vertex_new();
	if (hole_vertex(v)) {
		v->v_new->pos = v->pos;
		foreach(i, e, v->e) {
			if (!hole_edge(e)) continue;
			vadd(v->v_new->pos, edge_point(e)->pos);
			n++;
		}
		vdiv(v->v_new->pos, n + 1);
	} else {
		n = len(v->f);
		foreach(i, f, v->f)
			vadd(sum, face_point(f)->pos);
		foreach(i, e, v->e)
			vmadd(sum, edge_point(e)->pos, 2, sum);
		vdiv(sum, n);
		vmadd(sum, v->pos, n - 3, sum);
		vdiv(sum, n);
		v->v_new->pos = sum;
	}

	return v->v_new;
}

#define _get_idx(a, b) x = b;				\
		if ((a = x->idx) == -1)		\
			a = x->idx = list_push(nm->v, x)
model catmull(model m)
{
	int i, j, a, b, c, d;
	face f;
	vertex v, x;

	model nm = model_new();
	foreach (i, f, m->f) {
		foreach(j, v, f->v) {
			_get_idx(a, updated_point(v));
			_get_idx(b, edge_point(elem(f->e, (j + 1) % len(f->e))));
			_get_idx(c, face_point(f));
			_get_idx(d, edge_point(elem(f->e, j)));
			model_add_face(nm, 4, a, b, c, d);
		}
	}

	model_norms(nm);
	return nm;
}

model star()
{
	int ang, i;
	double rad;
	coord_t v[15];
	model m = model_new();

	for (i = 0; i < 5; i++) {
		ang = i * 72;
		rad = ang * 3.1415926 / 180;
		v[i].x = 2.2 * cos(rad); v[i].y = 2.2 * sin(rad); v[i].z = 0;

		rad = (ang + 36) * 3.1415926 / 180;
		v[i + 5].x = v[i + 10].x = cos(rad);
		v[i + 5].y = v[i + 10].y = sin(rad);
		v[i + 5].z = .5;
		v[i + 10].z = -.5;
	}

	for (i = 0; i < 15; i++) model_add_vertex(m, v[i].x, v[i].y, v[i].z);
	tri_face(m, 0, 5, 9);
	tri_face(m, 1, 6, 5);
	tri_face(m, 2, 7, 6);
	tri_face(m, 3, 8, 7);
	tri_face(m, 4, 9, 8);

	tri_face(m, 0, 14, 10);
	tri_face(m, 1, 10, 11);
	tri_face(m, 2, 11, 12);
	tri_face(m, 3, 12, 13);
	tri_face(m, 4, 13, 14);

	tri_face(m, 0, 10,  5);
	tri_face(m, 1,  5, 10);
	tri_face(m, 1, 11,  6);
	tri_face(m, 2,  6, 11);
	tri_face(m, 2, 12,  7);
	tri_face(m, 3,  7, 12);
	tri_face(m, 3, 13,  8);
	tri_face(m, 4,  8, 13);
	tri_face(m, 4, 14,  9);
	tri_face(m, 0,  9, 14);

	//model_add_face(m, 5, 14, 13, 12, 11, 10);
	//model_add_face(m, 5, 5, 6, 7, 8, 9);

	model_norms(m);
	return m;
}

model donut()
{
	model m = model_new();
	int i;
	coord_t v[] = {
		{ -2, -.5, -2 }, { -2, -.5,  2 }, {  2, -.5, -2 }, {  2, -.5,  2 },
		{ -1, -.5, -1 }, { -1, -.5,  1 }, {  1, -.5, -1 }, {  1, -.5,  1 },
		{ -2,  .5, -2 }, { -2,  .5,  2 }, {  2,  .5, -2 }, {  2,  .5,  2 },
		{ -1,  .5, -1 }, { -1,  .5,  1 }, {  1,  .5, -1 }, {  1,  .5,  1 },
	};

	for (i = 0; i < 16; i++) model_add_vertex(m, v[i].x, v[i].y, v[i].z);
	quad_face(m, 4, 5, 1, 0);
	quad_face(m, 3, 1, 5, 7);
	quad_face(m, 0, 2, 6, 4);
	quad_face(m, 2, 3, 7, 6);

	quad_face(m,  8,  9, 13, 12);
	quad_face(m, 15, 13,  9, 11);
	quad_face(m, 12, 14, 10,  8);
	quad_face(m, 14, 15, 11, 10);

	quad_face(m, 0, 1,  9,  8);
	quad_face(m, 1, 3, 11,  9);
	quad_face(m, 2, 0,  8, 10);
	quad_face(m, 3, 2, 10, 11);

	quad_face(m, 12, 13, 5, 4);
	quad_face(m, 13, 15, 7, 5);
	quad_face(m, 14, 12, 4, 6);
	quad_face(m, 15, 14, 6, 7);

	model_norms(m);
	return m;
}

model cube()
{
	int x, y, z;

	model m = model_new();
	for (x = -1; x <= 1; x += 2)
	for (y = -1; y <= 1; y += 2)
	for (z = -1; z <= 1; z += 2)
		model_add_vertex(m, x, y, z);

	quad_face(m, 0, 1, 3, 2);
	quad_face(m, 6, 7, 5, 4);
	quad_face(m, 4, 5, 1, 0);
	quad_face(m, 2, 3, 7, 6);
	quad_face(m, 0, 2, 6, 4);
	quad_face(m, 5, 7, 3, 1);

	model_norms(m);
	return m;
}

void draw_model(model m)
{
	int i, j;
	face f;
	vertex v;
	foreach(i, f, m->f) {
		glBegin(GL_POLYGON);
		if (!interp_norm) glNormal3fv(&(f->norm.x));
		foreach(j, v, f->v) {
			if (interp_norm)
				glNormal3fv(&(v->avg_norm.x));
			glVertex3fv(&(v->pos.x));
		}
		glEnd();
	}
}

void draw_wireframe(model m, GLfloat *color, GLfloat *hole_color)
{
	int i;
	edge e;

	glDisable(GL_LIGHTING);
	foreach(i, e, m->e) {
		if (e->f->n != 2) glColor3fv(hole_color);
		else		  glColor3fv(color);

		glBegin(GL_LINES);
		glVertex3fv(&(e->v[0]->pos.x));
		glVertex3fv(&(e->v[1]->pos.x));
		glEnd();
	}
}

void draw_faces(model m)
{
	glPushMatrix();
	glLoadIdentity();
	glEnable(GL_LIGHTING);
	glLightfv(GL_LIGHT0, GL_AMBIENT,  ambient);
	glLightfv(GL_LIGHT0, GL_DIFFUSE,  diffuse);
	glLightfv(GL_LIGHT0, GL_POSITION, litepos);
	glEnable(GL_LIGHT0);

	glLightfv(GL_LIGHT1, GL_DIFFUSE,  diffuse);
	glLightfv(GL_LIGHT1, GL_POSITION, litepos2);
	glEnable(GL_LIGHT1);
	glPopMatrix();

	if (wireframe_mode) {
		glEnable(GL_POLYGON_OFFSET_FILL);
		glPolygonOffset(1.0, 1.0);
	}
	draw_model(m);
	if (wireframe_mode)
		glDisable(GL_POLYGON_OFFSET_FILL);
}

void keyspecial(int k, int x, int y)
{
	switch(k) {
	case GLUT_KEY_UP:
		rotx --; break;
	case GLUT_KEY_DOWN:
		rotx ++; break;
	case GLUT_KEY_LEFT:
		roty --; break;
	case GLUT_KEY_RIGHT:
		roty ++; break;
	}
}

void set_model()
{
	int i;
	void *p;

	model_pos = 1;
	model_idx = (model_idx + 1) % 3;

	foreach(i, p, models) model_del(p);

	len(models) = 0;

	switch(model_idx) {
	case 0:
		list_push(models, cube()); break;
	case 1:
		list_push(models, donut()); break;
	case 2:
		list_push(models, star()); break;
	}
}

void keypress(unsigned char key, int x, int y)
{
	switch(key) {
	case 27: case 'q':
		glFinish();
		glutDestroyWindow(gwin);
		return;
	case 'w':
		wireframe_mode = (wireframe_mode + 1) % 3;
		return;
	case 'l':
		diffuse[0] += .1;
		diffuse[1] += .1;
		diffuse[2] += .1;
		return;
	case 'L':
		diffuse[0] -= .1;
		diffuse[1] -= .1;
		diffuse[2] -= .1;
		return;
	case ' ':
		rotx = roty = 0;
		return;
	case 'z':
		zpos ++;
		return;
	case 'a':
		zpos --;
		return;
	case 's':
		interp_norm = !interp_norm;
		break;
	case 'p':
		show_parent = (show_parent + 1) % 3;
		break;

	case '.': case '>':
		if (++model_pos >= max_depth) model_pos = max_depth;
		return;
	case ',': case '<':
		if (--model_pos < 0) model_pos = 0;
		return;

	case 'm':
		set_model();
		break;
	}
}

void render()
{
	if (!len(models)) return;
	while (model_pos >= len(models))
		list_push(models, catmull(elem(models, len(models) - 1)));

	glClear(GL_DEPTH_BUFFER_BIT | GL_COLOR_BUFFER_BIT);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	glTranslatef(.0f, 0.f, zpos);

	rot[0] += rotx / 8.;
	rot[1] += roty / 8.;

	if (rot[0] > 360) rot[0] -= 360;
	if (rot[0] < 0) rot[0] += 360;
	if (rot[1] > 360) rot[1] -= 360;
	if (rot[1] < 0) rot[1] += 360;

	glRotatef(rot[0], 1, 0, 0);
	glRotatef(rot[1], 0, 1, 0);

	GLfloat *pcolor = color2;
	if (model_pos && show_parent) {
		if (show_parent == 2) pcolor = red;
		draw_wireframe(elem(models, model_pos - 1), pcolor, hole2);
	}

	model m = elem(models, model_pos);
	if (wireframe_mode) draw_faces(m);
	if (wireframe_mode < 2) draw_wireframe(m, color, hole);

	glFlush();
	glFinish();
	glutSwapBuffers();
}

void resize(int w, int h)
{
	printf("size %d %d\n", w, h);
	glViewport(0, 0, w, h);
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluPerspective(45.f, (GLfloat)w / h, .1f, 100.f);
	glMatrixMode(GL_MODELVIEW);
}

void init_gfx(int *c, char **v) {
	glutInit(c, v);
	glutInitDisplayMode(GLUT_RGBA | GLUT_DEPTH | GLUT_DOUBLE);
	glutInitWindowSize(640, 480);

	gwin = glutCreateWindow("Catmull-Clark");
	glutReshapeFunc(resize);

	glClearColor(.0f, .0f, .0f, .0f);
	glClearDepth(1.0f);
	glDepthFunc(GL_LESS);
	glEnable(GL_DEPTH_TEST);
	glShadeModel(GL_SMOOTH);

	glutKeyboardFunc(keypress);
	glutSpecialFunc(keyspecial);
	glutDisplayFunc(render);
	glutIdleFunc(render);
	glutMainLoop();
}

int main(int c, char **v)
{
	int i;
	void *p;

	models = list_new();
	list_push(models, cube());
	model_pos = 1;

	init_gfx(&c, v);

	foreach(i, p, models) model_del(p);
	list_del(models);

	return 0;
}
```

