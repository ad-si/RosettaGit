+++
title = "Knight's tour/C"
description = ""
date = 2014-04-16T18:30:17Z
aliases = []
[extra]
id = 10568
[taxonomies]
categories = []
tags = []
+++


### OpenGL

[[File:knight-C.png|thumb|right|100px]]
{{libheader|GLUT}}
OpenGL program with glut.  Compile with <code>gcc -std=c99 -lglut -lGL -lGLU</code>, run with <code>a.out -s [size]</code>.  Program will print a help message at start.

```c
#include <GL/glut.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include <sys/time.h>
#include <string.h>

struct timeval last_update, last_move;
/* board width */
int bw = 7;
/* power of 2 */
int twid = 1;
int W = 240, H = 240, gwin;

void *board;
typedef struct { int x, y; } cell_t;
cell_t *moves;
cell_t ofs[8] = {{2, 1}, {2, -1}, {1, -2}, {-1, -2}, {-2, -1}, {-2, 1}, {-1, 2}, {1, 2}};
cell_t start;
int n_moves, need_move = 1, stopped = 0, failed = 0, paused = 0, skip_draw = 0;
int delay = 1024 * 128;

GLubyte *tex;
GLuint texture;

int good(int x)
{
	return x >= 0 && x < bw;
}

void init_board()
{
	int (*b)[bw] = board;
	for (int i = 0; i < bw; i++)
		memset(b[i], 0, sizeof(int) * bw);

	n_moves = b[start.y][start.x] = 1;
	moves[0] = start;
	need_move = 1;
	stopped = 0;
}

int n_vacant(int x, int y)
{
	int (*b)[bw] = board;
	int cnt = 0;

	for (int i = 0; i < 8; i++) {
		int nx = x + ofs[i].x;
		if (!good(nx)) continue;

		int ny = y + ofs[i].y;
		if (!good(ny)) continue;

		if (!b[ny][nx]) cnt++;
	}

	return cnt;
}

int restart()
{
	if (++start.x < bw) {
		init_board();
		return 1;
	}
	start.x = 0;
	if (++start.y < bw) {
		init_board();
		return 1;
	}
	start.y = 0;
	printf("Already tried all starting positions\n");
	need_move = 0;
	failed = 1;
	return 0;
}

int next_move()
{
	if (!need_move || stopped || paused)
		return 0;

	int (*b)[bw] = board;
	cell_t cur = moves[n_moves - 1];
	cell_t dest;
	int least = 9;

	for (int i = 0; i < 8; i++) {
		int x = cur.x + ofs[i].x;
		if (!good(x)) continue;

		int y = cur.y + ofs[i].y;
		if (!good(y)) continue;

		if (b[y][x]) continue;

		int n = n_vacant(x, y);
		if (n < least) {
			least = n;
			dest.x = x;
			dest.y = y;
		}
	}

	if (least == 9) {
		stopped = 1;
		need_move = 0;
		if (n_moves == bw * bw)
			printf("Tour complete.\n");
		else
			printf("Stuck at %d moves\n", n_moves);
		printf("Press SPACE to continue\n");
		return 0;
	}

	moves[n_moves++] = dest;
	b[dest.y][dest.x] = 1;

	need_move = 1;
	return 1;
}

void resize(int w, int h)
{
	int dx = 0, dy = 0;
	W = w; H = h;

	if (w > h) dx = w - h;
	else	   dy = h - w;

	glViewport(dx / 2, dy / 2, W - dx, H - dy);
	glOrtho(0, bw, 0, bw, -1, 1);
}

void render()
{
	double tw = (double) bw / twid;

	struct timeval tv;
	gettimeofday(&tv, 0);
	long usec = (tv.tv_sec - last_move.tv_sec) * 1000000
			+ tv.tv_usec - last_move.tv_usec;
	if (usec > delay || skip_draw) {
		next_move();
		last_move = tv;
	}

	if (skip_draw && !stopped) return;

	usec = (tv.tv_sec - last_update.tv_sec) * 1000000
			+ tv.tv_usec - last_update.tv_usec;
	if (usec < 25000) return;
	last_update = tv;

	glClear(GL_COLOR_BUFFER_BIT);
	glLoadIdentity();
	glBindTexture(GL_TEXTURE_2D, texture);

	glColor3f(1, 1, 1);
	glBegin(GL_QUADS);
	glTexCoord2f( 0,  0); glVertex2i(0, 0);
	glTexCoord2f( 0, tw); glVertex2i(0, bw);
	glTexCoord2f(tw, tw); glVertex2i(bw, bw);
	glTexCoord2f(tw,  0); glVertex2i(bw, 0);
	glEnd();

	glBegin(GL_QUADS);
	glColor3f(0, .5, 1);
	int x = moves[0].x, y = moves[0].y;

	glVertex2i(x + 0, y + 0);
	glVertex2i(x + 0, y + 1);
	glVertex2i(x + 1, y + 1);
	glVertex2i(x + 1, y + 0);

	glColor4f(.5, .7, .5, .7);
	for (int i = (n_moves == bw * bw) ? n_moves - 1 : 1; i < n_moves; i++) {
		if (i == n_moves - 1)
			glColor3f(1, 0, 0);
		x = moves[i].x, y = moves[i].y;
		glVertex2f(x + 0, y + 0);
		glVertex2f(x + 0, y + 1);
		glVertex2f(x + 1, y + 1);
		glVertex2f(x + 1, y + 0);
	}
	glEnd();

	glBegin(GL_LINE_STRIP);
	if (n_moves == bw * bw)
		glColor3f(0, .4, .4);
	else
		glColor3f(0, 0, 0);

	for (int i = 0; i < n_moves; i++)
		glVertex2f(moves[i].x + .5, moves[i].y + .5);
	glEnd();

	glutSwapBuffers();
	need_move = 1;
}

void init_texture()
{
	int i, j;
	while (twid < bw) twid <<= 1;

	GLubyte * ptr = tex = malloc(twid * twid * 3);

	for (i = 0; i < twid; i++)
		for (j = 0; j < twid; j++, ptr += 3)
			if ((i & 1) != (j & 1))
				ptr[0] = ptr[1] = ptr[2] = 255;
			else
				ptr[0] = 120, ptr[1] = 255, ptr[2] = 200;

	glEnable(GL_TEXTURE_2D);
	glGenTextures(1, &texture);
	glBindTexture(GL_TEXTURE_2D, texture);
	glTexImage2D(GL_TEXTURE_2D, 0, 3, twid, twid,
		0, GL_RGB, GL_UNSIGNED_BYTE, tex);

	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);

	free(tex);
}

void set_delay(int inc)
{
	if (inc) {
		delay *= 2;
		if (!delay) delay = 1;
		if (delay >= 1 << 20) {
			delay = 1 << 20;
			paused = 1;
			printf("Paused\n");
			return;
		}
	} else
		delay /= 2;
	paused = 0;
	printf("Delay = %d usec\n", delay);
}

void keypress(unsigned char key, int x, int y)
{
	switch(key) {
	case ' ':
		if (stopped && !failed) restart();
		break;
	case 'q':
	case 27:
		glFinish();
		glutDestroyWindow(gwin);
		return;
	case ',':
	case '<':
		set_delay(1);
		return;
	case '.':
	case '>':
		set_delay(0);
		return;
	case 's':
		skip_draw = !skip_draw;
		return;
	}
}

void init_graphics(int c, char **v)
{
	glutInit(&c, v);
	glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE);
	glutInitWindowSize(W, H);
	glutDisplayFunc(render);
	glutIdleFunc(render);
	glutDisplayFunc(render);

	gwin = glutCreateWindow("Board");

	glutKeyboardFunc(keypress);
	glutReshapeFunc(resize);

	glClearColor(.2, .2, .2, 1);

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glOrtho(0, bw, 0, bw, -1, 1);
	glMatrixMode(GL_MODELVIEW);

	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

	init_texture();
}

int main(int c, char **v)
{
	if (c >= 3 && !strcmp(v[1], "-s")) {
		bw = atoi(v[2]);
		if (bw < 3) {
			printf("bad argument -s %s, size set to 3\n", v[2]);
			bw = 3;
		}
	}
	printf("Keys:\n\tQ, Esc: exit\n\t<: slow down\n\t>: speed up\n\t"
		"s: skip to finish\n<space>: try a new tour\n");

	int b[bw][bw];
	cell_t g[bw * bw];
	moves = g;
	board = b;
	start.x = start.y = 0;

	init_board();

	init_graphics(c, v);
	gettimeofday(&last_update, 0);
	last_move = last_update;

	glutMainLoop();
	return 0;
}
```

