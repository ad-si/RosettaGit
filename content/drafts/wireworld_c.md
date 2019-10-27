+++
title = "Wireworld/C"
description = ""
date = 2011-07-22T20:14:47Z
aliases = []
[extra]
id = 4909
[taxonomies]
categories = []
tags = []
+++

Run the program to see a short help message about key bindings.
{{libheader|OpenGL}}
{{libheader|GLUT}}

Compile with <code>gcc -lpthread -lglut -lGL -lGLU</code>.  Run with <code>a.out file_name</code>.  There are a couple of larger test files on the talk page.  By default the program is compiled to use grayscale; compile time macro is available to switch to RGB.

```c>#include <stdio.h

#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/mman.h>
#include <err.h>
#include <pthread.h>
#include <GL/glut.h>
#include <GL/gl.h>
#include <GL/glu.h>

enum { s_blank = 0, s_condu, s_etail, s_ehead };
typedef struct { unsigned char n, mark, t;} cell_t;
typedef struct { int x, y; } pos_t;

#define USE_RGB 0 /* enable this for hideous colors */
#if USE_RGB

typedef struct { unsigned char r, g, b; } rgb_t;
rgb_t colors[] = {
	{0, 0, 0},
	{33, 45, 10},
	{30, 100, 80},
	{255, 0, 0},
};

#define TEX_COMP 3
#define TEX_MODE GL_RGB

#else	/* grayscale, less texture transfer */

typedef unsigned char rgb_t;
rgb_t colors[] = { 0, 64, 144, 255 };
#define TEX_COMP 1
#define TEX_MODE GL_LUMINANCE

#endif

rgb_t **tex;

int tex_w = 1;

cell_t **cells;
pos_t *heads, *bakup;
int n_heads, n_bakup;
int rows = 0, cols = 0, n_cond = 0;
int evolve_delay = 128, show_delay = 16;
int paused = 0, single = 0;
int min_y, max_y;

pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;

int gwin;
GLuint texture;

void mark_neighbors(pos_t *p)
{
	int x, y;
	for (y = p->y - 1; y <= p->y + 1 && y < rows; y++) {
		if (y < 0) continue;
		for (x = p->x - 1; x <= p->x + 1 && x < cols; x++) {
			if (	x < 0
				|| (x == p->x && y == p->y)
				|| cells[y][x].t != s_condu ) continue;

			cells[y][x].n++;

			if (!cells[y][x].mark) {
				cells[y][x].mark = 1;
				bakup[n_bakup].x = x;
				bakup[n_bakup].y = y;
				n_bakup ++;
			}
		}
	}
}

void evolve()
{
	int i;
	pos_t *p;
	cell_t *c;
	n_bakup = 0;
	for (i = 0, p = heads; i < n_heads; i++, p++) {
		c = &cells[p->y][p->x];
		switch(c->t) {
		case s_ehead:	mark_neighbors(p);
		case s_etail:	bakup[n_bakup++] = *p;
		}
	}

	for (	i = 0; i < n_bakup; c->n = 0) {
		p = bakup + i;
		c = &cells[p->y][p->x];
		c->mark = 0;
		if (p->y < min_y) min_y = p->y;
		if (p->y > max_y) max_y = p->y;

		switch(c->t) {
		case s_ehead:	c->t = s_etail; i++;
				tex[p->y][p->x] = colors[s_etail];
				continue;
		case s_etail:	c->t = s_condu;
				tex[p->y][p->x] = colors[s_condu];
				break;
		case s_condu:	if (c->n > 2) break;
				c->t = s_ehead;
				tex[p->y][p->x] = colors[s_ehead];
				i++;
				continue;
		}
		*p = bakup[--n_bakup];
	}

	n_heads = n_bakup;
	p = heads; heads = bakup; bakup = p;
	if (single)
		paused = 1;
}

#define die(act) err(1, "Can't %s %s", act, fn);
int read_file(char *fn)
{
	struct stat st;
	size_t ofs;
	int fd = open(fn, O_RDONLY);
	if (fd == -1)		die("open");
	if (fstat(fd, &st))	die("stat");
	char *map = mmap(0, st.st_size, PROT_READ, MAP_PRIVATE, fd, 0);
	if (map == (void*)-1)	die("mmap");

	int i = 0;
	/* pass 1, get various sizes */
	for (ofs = 0; ofs < st.st_size; ofs++) {
		switch(map[ofs]) {
		case '\n':	break;
		case 't':
		case 'H':
		case '.':	n_cond++;
		case ' ':	i++;
		case '\r':	continue;
		default:	printf("Bad char %c at row %d, col %d\n",
					map[ofs], rows, i);
				goto bail;
		}

		if (i > cols) cols = i;
		rows ++;
		i = 0;
	}

	heads = malloc(sizeof(pos_t) * n_cond);
	bakup = malloc(sizeof(pos_t) * n_cond);

	cells = malloc(sizeof(cell_t*) * rows);
	cells[0] = calloc(cols * rows, sizeof(cell_t));

	for (i = 1; i < rows; i++)
		cells[i] = cells[i - 1] + cols;

	while (tex_w < cols) tex_w <<= 1;
	while (tex_w < rows) tex_w <<= 1;

	tex = malloc(sizeof(rgb_t *) * tex_w);
	tex[0] = calloc(tex_w * tex_w, sizeof(rgb_t));

	for (i = 1; i < tex_w; i++)
		tex[i] = tex[i - 1] + tex_w;

	/* pass 2, convert char file to data */
	int j = i = n_heads = 0;
	for (ofs = 0; ofs < st.st_size; ofs++) {
		switch(map[ofs]) {
		default:	continue;
		case '\n':	i++; j = 0; continue;
		case '.':	cells[i][j++].t = s_condu; continue;
		case ' ':	cells[i][j++].t = s_blank; continue;
		case 't':	cells[i][j].t = s_etail; goto add_cell;
		case 'H':	cells[i][j].t = s_ehead; goto add_cell;
		}
	add_cell:		heads[n_heads].x = j++;
				heads[n_heads].y = i;
				n_heads++;
	}
bail:	munmap(map, st.st_size);
	close(fd);

	return 1;
}

void * updater(void * _)
{
	while (1) {
		if (evolve_delay)
			usleep(evolve_delay * 1000);
		if (paused) continue;

		pthread_mutex_lock(&lock);
		evolve();
		pthread_mutex_unlock(&lock);
	}

	return 0;
}

void render()
{
	double x, y;

	glClear(GL_COLOR_BUFFER_BIT);
	glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);

	pthread_mutex_lock(&lock);

	glBindTexture(GL_TEXTURE_2D, texture);
	glTexSubImage2D(GL_TEXTURE_2D, 0, 0, min_y, tex_w, max_y - min_y + 1,
		TEX_MODE, GL_UNSIGNED_BYTE, tex[min_y]);
	max_y = 0; min_y = rows;

	glBegin(GL_QUADS);

	x = (double)cols / tex_w;
	y = (double)rows / tex_w;
	glTexCoord2f(0, 0); glVertex2i(   0,    0);
	glTexCoord2f(x, 0); glVertex2i(cols,    0);
	glTexCoord2f(x, y); glVertex2i(cols, rows);
	glTexCoord2f(0, y); glVertex2i(   0, rows);

	glEnd();

	glFlush();
	glFinish();
	pthread_mutex_unlock(&lock);

	x = (evolve_delay < show_delay) ? evolve_delay : show_delay;
	if (x) usleep(x * 1000);
}

void set_delay(int dec)
{
	if (dec) evolve_delay >>= 1;
	else{
		evolve_delay <<= 1;
		if (!evolve_delay)
			evolve_delay = 1;
	}

	if (evolve_delay > 1024) {
		evolve_delay = 1024;
		paused = 1;
	} else
		paused = 0;
	printf("Delay: %d ms\n", evolve_delay);
	if (paused) printf("Paused\n");
}

void show_help()
{
	printf("Keys:\n\t'<': slow down\n\t'>': speed up\n\t'h': this message\n\t"
		"'s': single step\n\t<space>: pause\n\t'q', Esc: quit\n");
}

void keypress(unsigned char key, int x, int y)
{
	switch(key) {
	case ' ':	if ((paused = !paused)) printf("Paused\n");
			return;
	case 'S':
	case 's':	printf((single = !single) ? "Single step\n" : "Continuous\n");
			return;
	case 'q':
	case 27:	glFinish();
			glutDestroyWindow(gwin);
			return;
	case ',':
	case '<':	set_delay(0); return;
	case '.':
	case '>':	set_delay(1); return;
	case 'h':
	case 'H':	show_help(); return;
	}
}

void resize(int w, int h)
{
	int dx = 0, dy = 0;
	double scale;

	w -= 10; h -= 10;
	glViewport(5, 5, w, h);
	if (w * rows > h * cols) {
		scale = (double)h / rows;
		dx = (w / scale - cols) / 2;
	} else {
		scale = (double)w / cols;
		dy = (h / scale - rows) / 2;
	}

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(-dx, cols + dx, rows + dy, -dy, -1, 1);
	glMatrixMode(GL_MODELVIEW);
}

void set_texture()
{
	int i, j;
	for (i = 0; i < rows; i++)
		for (j = 0; j < cols; j++)
			tex[i][j] = colors[cells[i][j].t];

	glEnable(GL_TEXTURE_2D);

	glGenTextures(1, &texture);
	glBindTexture(GL_TEXTURE_2D, texture);

	glTexImage2D(GL_TEXTURE_2D, 0, TEX_COMP, tex_w, tex_w,
		0, TEX_MODE, GL_UNSIGNED_BYTE, tex[0]);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	min_y = rows;
	max_y = 0;
}

void init_gfx(char *prog)
{
	int one = 1;
	glutInit(&one, &prog);
	glutInitDisplayMode(GLUT_RGB);
	glutInitWindowSize(320, 240);
	glutDisplayFunc(render);
	glutIdleFunc(render);
	glutDisplayFunc(render);

	gwin = glutCreateWindow("Wireworld");

	glutKeyboardFunc(keypress);
	glutReshapeFunc(resize);

	set_texture();
}

int main(int c, char **v)
{
	pthread_t th;

	if (c < 2) {
		printf("Usage: %s input_file\n", v[0]);
		return 0;
	}

	if (!read_file(v[1])) return 0;

	show_help();
	init_gfx(v[0]);

	pthread_create(&th, 0, updater, 0);
	pthread_detach(th);

	glutMainLoop();
	return 0;
}
```

