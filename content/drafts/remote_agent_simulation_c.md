+++
title = "Remote agent/Simulation/C"
description = ""
date = 2012-12-27T06:52:22Z
aliases = []
[extra]
id = 10405
[taxonomies]
categories = []
tags = []
+++

<noinclude>{{collection|Remote agent/Simulation}}</noinclude>
Server/client programs share a lot of code, so they are all lumped together here.  To compile, dump all three files in the same dir, and compile server.c and client.c separately.  The server binds to port 11111.  Both server and client display the current map, though only the server has the whole world, the client only knows cells the agent has been to.

;<nowiki>server.c:</nowiki>

```c>#include <stdio.h

#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h> 
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <err.h>
#include "common.c"

#define W 30
#define H 30

int do_server_stuff(int);

int main()
{
	int one = 1, client_fd, port = 11111;
	struct sockaddr_in svr_addr, cli_addr;
	socklen_t sin_len = sizeof(cli_addr);
 
	int sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock < 0)
		err(1, "can't open socket");
 
	setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(int));
 
	svr_addr.sin_family = AF_INET;
	svr_addr.sin_addr.s_addr = INADDR_ANY;
	svr_addr.sin_port = htons(port);
 
	if (bind(sock, (struct sockaddr *) &svr_addr, sizeof(svr_addr)) == -1) {
		close(sock);
		err(1, "Can't bind");
	}
 
	listen(sock, 5);
	while (1) {
		client_fd = accept(sock, (struct sockaddr *) &cli_addr, &sin_len);
 
		if (client_fd == -1) {
			perror("Can't accept");
			continue;
		}
 		puts("\033[H\033[J");
 		do_server_stuff(client_fd);
		close(client_fd);
	}
}

int work_done(byte **f)
{
	int x, y, b;
	FOR(y, H) FOR(x, W) {
		b = cell_ball(f, x, y);
		if (b < 0) continue;
		if (b != cell_color(f, x, y)) return 0;
	}
	return 1;
}

byte **f = 0;
int do_server_stuff(int fd)
{
	int c, x, y, b;
	agent.ball = -1;

	say_l('A');

	if (f) free(f);
	f = make_field(W, H);
	c = hear_l();
	if (c != 'A') {
		fprintf(stderr, "Bad hand shake: %c\n", c);
		return 0;
	}

	while (1) {
		show_field(f, W, H);
		c = hear();
		x = agent.x;
		y = agent.y;

		if (c == -1) break;
		switch(c) {
		case c_fwd:
			if (!agent_forward(f)) {
				say(a_bump);
				say(c_stop);
				continue;
			}
			say(s_red + cell_color(f, agent.x, agent.y));

			c = cell_ball(f, agent.x, agent.y);
			if (c >= 0) say(b_red + c);

			say(c_stop);
			continue;
		case c_cw:
			turn(c_cw), say(c_stop); continue;
		case c_ccw:
			turn(c_ccw), say(c_stop);continue;
		case c_get:
			b = cell_ball(f, agent.x, agent.y);
			if (b == -1) say(s_empty);
			if (agent.ball >= 0) b = -1, say(a_full);
			if (b >= 0) {
				agent.ball = b;
				f[y][x] &= ~ball;
			}
			say(c_stop);
			continue;
		case c_drop:
			if ((b = agent.ball) == -1) say(a_empty);
			if (cell_ball(f, x, y) >= 0) b = -1, say(s_full);
			if (b >= 0) {
				agent.ball = -1;
				f[y][x] &= ~12;
				f[y][x] |= ball | (b * 4);
			}
			if (work_done(f)) say(c_over);
			say(c_stop);
			continue;
		}
	}

	return 1;
}
```


;<nowiki>client.c:</nowiki>

```c>#include <stdio.h

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h> 
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <err.h>
#include "common.c"
 
typedef struct task_node_t {
	int x, y;
	struct task_node_t *prev, *sibling, *chain;
} task_node_t, *task;
 
task pool = 0, used = 0;
task find_place_to_go(byte **f);
int is_interesting(byte **f, task t);
void goto_place(int fd, byte **f, task t);
task new_task_node()
{
	task t;
	if (!pool) pool = calloc(1, sizeof(task_node_t));
	t = pool->chain;
	pool->chain = used;
	used = pool;
	pool = t;
	return used;
}
 
void remove_tasks()
{
	task t;
	while (used) {
		t = used;
		used = used->chain;
		t->chain = pool;
		pool = t;
	}
}
 
int w, h;
int do_client_stuff(int);
 
int main(int c, char *v[])
{
	int sfd, port;
	struct sockaddr_in peer;
	struct hostent *server;
 
	if (c < 3) err(1, "Usage: %s host port\n", v[0]);
 
	port = atoi(v[2]);
	if (port <= 0) err(1, "bad port: %d\n", port);
 
	server = gethostbyname(v[1]);
	if (!server) err(1, "Unknown server %s", v[1]);
 
	if ((sfd = socket(AF_INET, SOCK_STREAM, 0)) == -1)
		err(1, "Can't open socket");
 
	memset(&peer, 0, sizeof(peer));
	peer.sin_family = AF_INET;
	memcpy(&peer.sin_addr.s_addr, server->h_addr, server->h_length);
	peer.sin_port = htons(port);
 
	if (connect(sfd, (struct sockaddr *)&peer, sizeof(peer)) < 0)
		err(1, "Can't connect to %s port %d", v[1], port);
 
	puts("\033[H\033[J");
	do_client_stuff(sfd);
	close(sfd);
 
	return 0;
}
 
byte ** init_map()
{
	int i, j;
	byte **f = byte_array(w = 3, h = 3);
	FOR (i, h) FOR(j, w) f[i][j] = unknown;
	agent.x = agent.y = 1;
	return f;
}
 
byte **f;
int try_forward(int);
void say_and_listen(int, int, int*);
 
int do_client_stuff(int fd)
{
	int c = hear_l(), x, y;
	int response[MAX_CMD];
	task tgt;
 
	if (c != 'A') {
		fprintf(stderr, "Bad handshake: %c\n", c);
		return 0;
	}
	say_l('A');
	f = init_map();
	c = 0;
 
	while (c != -1) {
		x = agent.x, y = agent.y;
		show_field(f, w, h);
 
		if (agent.ball == -1 && cell_ball(f, x, y) >= 0
				&& cell_ball(f, x, y) != cell_color(f, x, y))
		{
			say_and_listen(fd, c_get, response);
			if (response[a_full] || response[s_empty]) continue;
			agent.ball = cell_ball(f, x, y);
			f[y][x] &= ~ball;
			continue;
		}
 
		if (agent.ball == cell_color(f, x, y) && cell_ball(f, x, y) == -1) {
			say_and_listen(fd, c_drop, response);
			if (response[s_full] || response[a_empty]) continue;
			f[y][x] |= ball | (agent.ball << 2);
			agent.ball = -1;
			if (response[c_over]) return 1;
			continue;
		}
 
		goto_place(fd, f, tgt = find_place_to_go(f));
		continue;
	}
	return 0;
}
 
void expand_map()
{
	int w2 = w, h2 = h, i, j, d = agent.facing;
	int dx, dy;
	byte **nf;
 
	switch(d) {
	case 0: case 2: w2++; break;
	default: h2++; break;
	}
 
	nf = byte_array(w2, h2);
	FOR(i, h2) FOR(j, w2) nf[i][j] = unknown;
 
	dx = agent.x == 0;
	dy = agent.y == 0;
 
	FOR(i, h) FOR(j, w) nf[i + dy][j + dx] = f[i][j];
 
	if (!agent.x) agent.x = 1;
	if (!agent.y) agent.y = 1;
 
	w = w2, h = h2;
	printf("expand: %d %d\n", w, h);
	free(f);
	f = nf;	
}
 
int try_forward(int fd)
{
	int c, x, y, response[MAX_CMD] = {0};
	say(c_fwd);
	while (1) {
		c = hear();
		if (c == -1) return 0;
		if (c == c_stop) break;
		response[c] = 1;
	}
	x = agent.x + dirs[agent.facing][0];
	y = agent.y + dirs[agent.facing][1];
	if (response[a_bump]) {
		f[y][x] = wall;
		return 1;
	}
	agent.x = x, agent.y = y;
	for (c = s_red; c <= s_blue; c++) {
		if (!response[c]) continue;
		f[y][x] = (c - s_red);
		break;
	}
	for (c = b_red; c <= b_blue; c++) {
		if (!response[c]) continue;
		f[y][x] |= ball | ((c - b_red) * 4);
		break;
	}
	if (y == h - 1 || y == 0 || x == w - 1 || x == 0)
		expand_map();
	return 1;
}
 
void say_and_listen(int fd, int cmd, int *resp)
{
	int c;
	memset(resp, 0, sizeof(int) * MAX_CMD);
	say(cmd);
	while (1) {
		c = hear();
		if (c == -1) exit(1);
		resp[c] = 1;
		if (c == c_stop) return;
	}
}
 
int is_interesting(byte **f, task t)
{
	int x = t->x, y = t->y, a, b, c;
	if ((f[y][x] & unknown))
		return y != agent.y || x != agent.x;
 
	c = cell_color(f, x, y);
	b = cell_ball(f, x, y);
	a = agent.ball;
 
	if (a >= 0)
		return a == c && b == -1;
	else if (b >= 0 && b != c)
		return 1;
 
	return 0;
}
 
task find_place_to_go(byte **f)
{
	int d, x, y;
	task current = 0, next = 0, t;
	remove_tasks();
	FOR(y, h) FOR(x, w) f[y][x] &= ~marked;
	current = new_task_node();
	current->x = agent.x, current->y = agent.y;
	f[current->y][current->x] |= marked;
 
	if (is_interesting(f, current)) return current;
 
	while (current) {
		while (current) {
			FOR(d, 4) {
				x = current->x + dirs[d][0];
				y = current->y + dirs[d][1];
				if (x < 0 || x >= w || y < 0 || y >= h) continue;
				if (f[y][x] & (wall | marked)) continue;
				f[y][x] |= marked;
				t = new_task_node();
				t->x = x, t->y = y, t->prev = current;
				t->sibling = next;
				next = t;
				if (is_interesting(f, t)) return t;
			}
			current = current->sibling;
		}
		current = next;
		next = 0;
	}
	puts("nowhere to go");
	abort();
}
 
void goto_place(int fd, byte **f, task t)
{
	show_field(f, w, h);
	if (t->prev) goto_place(fd, f, t->prev);
	usleep(50000);
	t->prev = 0;
 
	if (agent.x == t->x && agent.y == t->y) return;
	while (	t->x != agent.x + dirs[agent.facing][0] ||
		t->y != agent.y + dirs[agent.facing][1])
			turn(c_cw), say(c_cw), hear();
	try_forward(fd);
}
```


;<nowiki>common.c:</nowiki>

```c
struct agent_t{ int x, y, facing, ball; } agent = { 0, 0, 0, -1 };
enum {
	c_fwd, c_cw, c_ccw, c_get, c_drop, c_over, c_stop,
	s_red, s_green, s_yellow, s_blue,
	b_red, b_green, b_yellow, b_blue,
	a_bump,
	s_full, a_full,
	s_empty, a_empty,
	MAX_CMD
};

const char *cmds = "^><@!+.RGYBrgyb|SAsa";
inline int cmd_to_char(int c) { return cmds[c]; }
inline int char_to_cmd(int c) {
	int i;
	for (i = 0; cmds[i] && cmds[i] != c; i++);
	return cmds[i] ? i : -1;
}

#define say_l(x) send_cmd(fd, x)
#define hear_l() read_cmd(fd)
#define say(x) send_cmd(fd, cmd_to_char(x))
#define hear() char_to_cmd(read_cmd(fd))

int irand(int n)
{
	int r, rand_max = RAND_MAX - RAND_MAX % n;
	while ((r = rand()) >= rand_max);
	return r / (rand_max / n);
}

int wrand(int *x, int l)
{
	int i, sum = 0;
	for (i = 0; i < l; i++) sum += x[i];
	sum = irand(sum);
	for (i = 0; i < l; sum -= x[i++])
		if (sum < x[i]) break;
	return i;
}

int read_fd(int fd, char *buf, int len)
{
	int l;
	l = read(fd, buf, len);
	if (l < 0) return 0;
	buf[l] = 0;
	return l;
}

int write_fd(int fd, char *buf, int len)
{
	int l = write(fd, buf, len);
	if (l != len) return 0;
	return 1;
}

int read_cmd(int fd)
{
	char buf[2];
	if (read_fd(fd, buf, 1) <= 0) return -1;
	return buf[0];
}

int send_cmd(int fd, char cmd)
{
	char c = cmd;
	return write_fd(fd, &c, 1);
}

typedef unsigned char byte;
enum { red, green, yellow, blue, ball = 16, wall = 32, unknown = 64, marked = 128 };

#define FOR(a, x) for (a = 0; a < x; a++)
byte **byte_array(int w, int h)
{
	int i;
	byte **f = calloc(1, sizeof(byte *) * h + w * h);
	f[0] = (byte*)(f + h);
	FOR(i, h - 1) f[i + 1] = f[i] + w;
	return f;
}

void shuffle(int *x, int len)
{
	int i, j, t;
	for (i = len - 1; i > 0; i--) {
		j = irand(i + 1);
		t = x[i], x[i] = x[j], x[j] = t;
	}
}

int dirs[4][2] = {{-1, 0}, {0, 1}, {1, 0}, {0, -1}};
int randomize(byte **f, int w, int h, int x, int y, int rem)
{
	int d[4] = {0, 1, 2, 3}, i;
	if (x <= 0 || x >= w - 1 || y <= 0 || y >= h - 1) return rem;
	if (f[y][x] != wall) return rem;
	
	f[y][x] = 0;
	--rem;

	shuffle(d, 4);
	for (i = 0; i < 4 && rem; i++) {
		rem = randomize(f, w, h,
			x + dirs[d[i]][0], y + dirs[d[i]][1], rem);
	}
	return rem;
}

byte **make_field(int w, int h)
{
	int i, j, c, x, cells[4] = {0};
	byte **f = byte_array(w, h);
	FOR(i, w) FOR(j, h) f[i][j] = wall;
	randomize(f, w, h, irand(w - 2) + 1, irand(h - 2) + 1,
		(h - 2) * (w - 2) * 1 / 2);

	FOR(i, w) FOR(j, h) {
		if (f[i][j] == wall) continue;
		cells[ f[i][j] = irand(4) ] ++;
	}

	x = 0;
	FOR(i, w) FOR(j, h) {
		if (f[i][j] == wall) continue;
		if (irand(4)) {
		/* 1 in 4 chance to place a ball, else try place agent */
			if (!irand(++x))
				agent.x = j, agent.y = i;
			continue;
		}
		c = wrand(cells, 4);
		cells[c] --;
		f[i][j] |= ball | c * 4;
	}

	agent.facing = irand(3);

	return f;
}

const char *colors[] = { "31", "32", "33", "34" };
const char *ag = "<V>A";

inline int cell_color(byte **f, int x, int y)
{
	return f[y][x] & 3;
}

inline int cell_ball(byte **f, int x, int y)
{
	if (!(f[y][x] & ball) || (f[y][x] == unknown)) return -1;
	return (f[y][x] >> 2) & 3;
}

void show_field(byte **f, int w, int h)
{
	int i, j;

	printf("\033[H");
	FOR(i, h) {
		FOR(j, w) {
			if (f[i][j] == wall) {
				printf("## ");
				continue;
			}
			if (f[i][j] == unknown) {
				printf("-- ");
				continue;
			}

			printf("\033[7;%sm", colors[cell_color(f, j, i)]);
			putchar(j == agent.x && i == agent.y ? ag[agent.facing] : ' ');

			if (cell_ball(f, j, i) >= 0)
				printf("\033[%smO\033[m ", colors[cell_ball(f, j, i)]);
			else
				printf(" \033[m ");
 		}
		putchar('\n');
	}
	printf("agent carry: %d\n", agent.ball);
	fflush(stdout);
}

int agent_forward(byte **f)
{
	int x, y;
	x = agent.x + dirs[agent.facing][0];
	y = agent.y + dirs[agent.facing][1];
	if (f[y][x] == wall) return 0;
	agent.x = x;
	agent.y = y;
	return 1;
}

void turn(int dir)
{
	if (dir == c_ccw)
		if (--agent.facing < 0) agent.facing = 3;
	if (dir == c_cw)
		if (++agent.facing > 3) agent.facing = 0;
}
```

