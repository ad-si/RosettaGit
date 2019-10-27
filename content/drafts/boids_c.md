+++
title = "Boids/C"
description = ""
date = 2016-04-22T23:26:17Z
aliases = []
[extra]
id = 12761
[taxonomies]
categories = []
tags = []
+++


```c>#include <stdio.h

#include <stdlib.h>
#include <math.h>
#include <GL/glut.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <sys/time.h>
#include <unistd.h>

#define PI 3.14159265
#define MIN_MOUNTAIN_RADIUS 3
#define MAX_MOUNTAIN_RADIUS 5
#define MAX_MOUNTAIN_HEIGHT 25
#define MOUNTAIN_RATIO 500

#define IDEAL_HEIGHT 1 // how high a boid prefers to stay above ground
#define IDEAL_DISTANCE 5 // how far boids prefer to stay away from each other
#define MOVE_SPEED 1e-2
#define N 100

typedef GLfloat flt;
unsigned long long get_msec(void)
{
	struct timeval tv;
	gettimeofday(&tv, 0);
	return tv.tv_sec * 1000ULL + tv.tv_usec / 1000;
}
unsigned long long update_time;

// 3D vector stuff
typedef struct { flt x[3]; } vec;

inline void vscale(vec *a, flt r) {
	a->x[0] *= r;
	a->x[1] *= r;
	a->x[2] *= r;
}

inline void vmuladd_to(vec *a, const vec *b, flt r) {
	a->x[0] += r * b->x[0];
	a->x[1] += r * b->x[1];
	a->x[2] += r * b->x[2];
}

inline void vadd_to(vec *a, const vec *b) {
	a->x[0] += b->x[0];
	a->x[1] += b->x[1];
	a->x[2] += b->x[2];
}

inline vec vadd(vec a, vec b) {
	return (vec) {{	a.x[0] + b.x[0],
			a.x[1] + b.x[1],
			a.x[2] + b.x[2] }};
}

inline vec vsub(vec a, vec b) {
	return (vec) {{	a.x[0] - b.x[0],
			a.x[1] - b.x[1],
			a.x[2] - b.x[2] }};
}

inline flt vlen2(vec a) {
	return a.x[0]*a.x[0] + a.x[1]*a.x[1] + a.x[2]*a.x[2];
}

inline flt vdist2(vec a, vec b) { return vlen2(vsub(a, b)); }

inline vec vcross(vec a, vec b) {
	return (vec) {{
		a.x[1]*b.x[2] - a.x[2]*b.x[1],
		a.x[2]*b.x[0] - a.x[0]*b.x[2],
		a.x[0]*b.x[1] - a.x[1]*b.x[0] }};
}

inline void vnormalize(vec *a) {
	flt r = sqrt(a->x[0]*a->x[0] + a->x[1]*a->x[1] + a->x[2]*a->x[2]);
	if (!r) return;
	a->x[0] /= r;
	a->x[1] /= r;
	a->x[2] /= r;
}

typedef struct {
	vec position, heading, newheading;
	flt speed;
} boid_t;

boid_t boids[N];

typedef struct mountain_s mountain_t;
struct mountain_s {
	int x, y, h;
	double r;
	mountain_t *next;
};

typedef struct {
	int x[2], y[2]; // min/max coords of world
	flt *ground;
	vec *ground_normal;
	mountain_t *hills;
} world_t;

#define WORLD_SIZE 40
world_t world;

typedef struct {
	double pitch, yaw, distance;
	vec target;
} camera_t;

camera_t camera = { -PI/4, 0, 100, {{0, 0, 0}} };

unsigned int hash_xy(int x, int y) {
	inline unsigned int ror(unsigned int a, int d) {
		return (a<<d) | (a>>(32-d));
	}

	unsigned int h = 0x12345678, tmp;
	tmp = x;
	h += ror(h, 15) ^ ror(tmp, 5);
	tmp = y;
	h += ror(h, 15) ^ ror(tmp, 5);

	h ^= ror(h, 7);
	h += ror(h, 23);

	h ^= ror(h, 19);
	h += ror(h, 11);

	return h;
}

double hill_height(mountain_t *m, double x, double y)
{
	x -= m->x, y -= m->y;
	return m->h * exp(-(x * x + y * y) / (m->r * m->r));
}

flt hill_hight(mountain_t *m, flt x, flt y)
{
	flt xx = x - m->x, yy = y - m->y;
	return m->h * exp(-(xx*xx + yy*yy) / (m->r * m->r));
}

flt ground_height(flt x, flt y)
{
	mountain_t *p = world.hills;
	flt h = 0;
	while (p) {
		h += hill_hight(p, x, y);
		p = p->next;
	}
	return h;
}

vec calc_normal(flt x, flt y)
{
	vec v = {{0,0,0}};

	mountain_t *p = world.hills;
	while (p) {
		flt h = hill_height(p, x, y);
		flt t = 2 / (p->r * p->r);
		v.x[0] += (x - p->x) * t * h;
		v.x[1] += (y - p->y) * t * h;
		p = p->next;
	}
	v.x[2] = 1;
	vnormalize(&v);
	return v;
}

void make_terrain(int cx, int cy)
{
	if (cx * 2 == world.x[0] + world.x[1] &&
			cy * 2 == world.y[0] + world.y[1])
		return;

	world.x[0] = cx - WORLD_SIZE;
	world.x[1] = cx + WORLD_SIZE;
	world.y[0] = cy - WORLD_SIZE;
	world.y[1] = cy + WORLD_SIZE;

	int x, y;
	int nx = world.x[1] - world.x[0] + 1;
	int ny = world.y[1] - world.y[0] + 1;

	while (world.hills) {
		mountain_t *p = world.hills->next;
		free(world.hills);
		world.hills = p;
	}

	for (x = world.x[0]; x <= world.x[1]; x++) {
		for (y = world.y[0]; y <= world.y[1]; y++) {
			unsigned int h = hash_xy(x, y) % MOUNTAIN_RATIO;
			if (h) continue;

			mountain_t *m = malloc(sizeof(mountain_t));
			m->x = x, m->y = y;
			m->r = MIN_MOUNTAIN_RADIUS + (hash_xy(y, x) % 100) / 100.0
				* (MAX_MOUNTAIN_RADIUS - MIN_MOUNTAIN_RADIUS);
			m->h = hash_xy((y + x) / 2, (y - x) / 2) % MAX_MOUNTAIN_HEIGHT;
			m->next = world.hills;
			world.hills = m;
		}
	}

	//if (world.ground) free(world.ground);
	//if (world.ground_normal) free(world.ground_normal);

	if (!world.ground)
		world.ground = malloc(sizeof(flt) * nx * ny);
	if (!world.ground_normal)
		world.ground_normal = malloc(sizeof(vec) * nx * ny);
	for (x = 0; x < nx; x++) {
		int xx = x + world.x[0];
		for (y = 0; y < ny; y++) {
			int yy = y + world.y[0];
			world.ground[x * ny + y] = ground_height(xx, yy);
			world.ground_normal[x * ny + y] = calc_normal(xx, yy);
		}
	}
}

void boid_think(boid_t *b)
{
	flt g = ground_height(b->position.x[0], b->position.x[1]);

	vec migration_drive = {{0,.5,0}};

	vec height_drive = {{0,0,0}};
	height_drive.x[2] = (IDEAL_HEIGHT + g - b->position.x[2]) * .3;

	// follow the ground surface normal
	vec terrain_drive = calc_normal(b->position.x[0], b->position.x[1]);
	//vnormalize(&terrain_drive);

	int i;

	vec crowding_drive = {{0,0,0}};
	vec grouping_drive = {{0,0,0}};

	flt total_weight = 0;
	for (i = 0; i < N; i++) {
		boid_t *other = boids + i;
		if (other == b) continue;

		vec diff = vsub(other->position, b->position);
		flt d2 = vlen2(diff);
		flt weight = 1 / pow(d2, 2);

		vnormalize(&diff);
		if (d2 > IDEAL_DISTANCE * IDEAL_DISTANCE)
			vmuladd_to(&crowding_drive, &diff, weight);
		else
			vmuladd_to(&crowding_drive, &diff, -weight);

		vmuladd_to(&grouping_drive, &other->heading, weight);
		total_weight += weight;
	}

	vscale(&grouping_drive, 1/total_weight);

	//vnormalize(&crowding_drive);

	b->newheading = migration_drive;
	vadd_to(&b->newheading, &height_drive);
	vadd_to(&b->newheading, &terrain_drive);
	vadd_to(&b->newheading, &crowding_drive);
	vadd_to(&b->newheading, &grouping_drive);
	vscale(&b->newheading, 1./5);
	vnormalize(&b->newheading);

	GLfloat cx = (world.x[0] + world.x[1]) / 2.0;
	GLfloat cy = (world.y[0] + world.y[1]) / 2.0;
	b->newheading.x[0] += (cx - b->position.x[0]) / 400;
	b->newheading.x[1] += (cy - b->position.x[1]) / 400;
}

void run_boids(int msec)
{
	int i;
	for (i = 0; i < N; i++)
		vmuladd_to(&boids[i].position, &boids[i].heading, msec * boids[i].speed);

	vec average = {{0,0,0}};
	for (i = 0; i < N; i++)
		vadd_to(&average, &boids[i].position);
	vscale(&average, 1./N);
	camera.target = average;
	make_terrain(average.x[0], average.x[1]);

	for (i = 0; i < N; i++)
		boid_think(boids + i);

	for (i = 0; i < N; i++)
		boids[i].heading = boids[i].newheading;
}

// windowing stuff
int gwin, win_width, win_height;
int button_r, button_l;

void resize(int w, int h)
{
	win_width = w;
	win_height = h;

	glViewport(0, 0, w, h);
}

void set_projection(int w, int h)
{
	double hor, ver;
	if (w > h) {
		hor = .05;
		ver = hor * h / w;
	} else {
		ver = .05;
		hor = ver * w / h;
	}

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	glFrustum(-hor, hor, -ver, ver, .1, 1000);
}

void clamp(double *x, double min, double max)
{
	if (*x < min) *x = min;
	else if (*x > max) *x = max;
}

void draw_terrain(void)
{
	int x, y;
	int nx = world.x[1] - world.x[0] + 1;
	int ny = world.y[1] - world.y[0] + 1;

	glColor3f(.1, .25, .35);

	for (x = 0; x < nx - 1; x++) {
		int xx = x + world.x[0];
		glBegin(GL_QUAD_STRIP);

		for (y = 0; y < ny; y++) {
			int yy = y + world.y[0];
			glNormal3fv(world.ground_normal[x * ny + y].x);
			glVertex3f(xx, yy, world.ground[x * ny + y]);

			glNormal3fv(world.ground_normal[(1+x) * ny + y].x);
			glVertex3f(xx + 1, yy, world.ground[(1+x) * ny + y]);
		}

		glEnd();
	}
}

void draw_boid(boid_t *b)
{
	glColor3f(.6, .3, .3);
	glPushMatrix();

	glTranslatef(b->position.x[0], b->position.x[1], b->position.x[2]);

	float *x = b->heading.x;
	flt yaw = atan2(x[1], x[0]) / PI * 180 - 90;
	glRotatef(yaw, 0, 0, 1);

	flt rxy = sqrt(x[0] * x[0] + x[1] * x[1]);
	flt pitch = atan2(x[2], rxy) / PI * 180;
	glRotatef(pitch, 1, 0, 0);

	glBegin(GL_TRIANGLES);
	glNormal3f(-.8, 0, .6);
	glVertex3f(0, .5, 0);
	glVertex3f(-.5, -.5, 0);
	glVertex3f(0, 0, .1);

	glNormal3f(.8, 0, .6);
	glVertex3f(0, .5, 0);
	glVertex3f(.5, -.5, 0);
	glVertex3f(0, 0, .1);

	glNormal3f(-.8, 0, -.6);
	glVertex3f(0, .5, 0);
	glVertex3f(-.5, -.5, 0);
	glVertex3f(0, 0, -.1);

	glNormal3f(.8, 0, -.6);
	glVertex3f(0, .5, 0);
	glVertex3f(.5, -.5, 0);
	glVertex3f(0, 0, -.1);

	glNormal3f(1, -1, 0);
	glVertex3f(-.5, -.5, 0);
	glVertex3f(0, 0, .1);
	glVertex3f(0, 0, -.1);

	glNormal3f(-1, -1, 0);
	glVertex3f(.5, -.5, 0);
	glVertex3f(0, 0, .1);
	glVertex3f(0, 0, -.1);

	glEnd();

	glPopMatrix();
}

void set_lighting(void)
{
	flt light_ambient[] = {.3, .3, .3, 1};
	flt light_diffuse[] = { 1, 1, 1, 1 };
	flt light_position[] = {0, 1, 2, 1 };

	glEnable(GL_LIGHTING);
	glLightfv(GL_LIGHT1, GL_AMBIENT, light_ambient);
	glLightfv(GL_LIGHT1, GL_DIFFUSE, light_diffuse);
	glLightfv(GL_LIGHT1, GL_POSITION, light_position);
	glEnable(GL_LIGHT1);
	glShadeModel(GL_FLAT);
	glEnable(GL_COLOR_MATERIAL);
}

void render(void)
{
	unsigned long long msec = get_msec();
	if (msec < update_time + 16) {
		usleep((update_time + 16 - msec) * 1000);
		return;
	}
	run_boids(msec - update_time);
	update_time = msec;

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	glEnable(GL_DEPTH_TEST);

	set_projection(win_width, win_height);

	clamp(&camera.distance, 1, 1000);
	clamp(&camera.pitch, -PI / 2.1, PI / 2.1);

	double rz = camera.distance * sin(camera.pitch);
	double rxy = camera.distance * cos(camera.pitch);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

	set_lighting();
	printf("%.5f %.5f\r", camera.target.x[0], camera.target.x[1]);
	fflush(stdout);

	gluLookAt(camera.target.x[0] - rxy * cos(camera.yaw),
		camera.target.x[1] - rxy * sin(camera.yaw),
		camera.target.x[2] - rz,
		camera.target.x[0],
		camera.target.x[1],
		camera.target.x[2],
		0, 0, 1);

	draw_terrain();

	int i;
	for (i = 0; i < N; i++)
		draw_boid(boids + i);

	glFlush();
	glutSwapBuffers();
}

void keydown(unsigned char key, int x, int y)
{
	printf("key down: %d (%d %d)\n", key, x, y);
	if (key == 'q')
		exit(0);
}

void keyup(unsigned char key, int x, int y)
{
	printf("key up: %d (%d %d)\n", key, x, y);
}

// camera movement stuff
int cursor_x, cursor_y;

void mousebutton(int button, int state, int x, int y)
{
	if (state == GLUT_UP) return;
	if (button == 3) {
		camera.distance /= 2;
	} else if (button == 4) {
		camera.distance *= 2;
	}

	cursor_x = x, cursor_y = y;
}

void mousemove(int x, int y)
{
	int ext = win_width;
	if (ext < win_height) ext = win_height;

	ext /= 4;
	camera.yaw -= (double)(x - cursor_x) / ext;
	camera.pitch -= (double)(y - cursor_y) / ext;
	cursor_y = y, cursor_x = x;
}

void init_gl(int *c, char **v)
{
	update_time = get_msec();

	glutInit(c, v);
	glutInitDisplayMode(GLUT_RGB | GLUT_DOUBLE);
	glutInitWindowSize(600, 400);

	gwin = glutCreateWindow("Boids");

	glutIgnoreKeyRepeat(1);
	glutKeyboardFunc(keydown);
	glutKeyboardUpFunc(keyup);
	glutReshapeFunc(resize);
	glutIdleFunc(render);
	glutMouseFunc(mousebutton);
	glutMotionFunc(mousemove);
	set_lighting();
}

int main(int argc, char **argv)
{
	make_terrain(0, 1);

	int i;
	for (i = 0; i < N; i++) {
		flt x = ((flt)rand() / RAND_MAX) * 10 - 5;
		flt y = ((flt)rand() / RAND_MAX) * 10 - 5;
		flt z = (((flt)rand() / RAND_MAX) + .5) * IDEAL_HEIGHT + ground_height(x, y);
		boids[i].position = (vec){{x, y, z}};
		boids[i].speed = (0.98 + 0.04 * rand() / RAND_MAX) * MOVE_SPEED;
	}

	init_gl(&argc, argv);
	glutMainLoop();

	return 0;
}
```

