+++
title = "Sokoban/C"
description = ""
date = 2012-06-16T00:32:19Z
aliases = []
[extra]
id = 11713
[taxonomies]
categories = []
tags = []
+++

C99.

```c>#include <stdio.h

#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <stdbool.h>

#define ensure(x) { if (!(x)) printf("\nabort: line %d\n", __LINE__); }

int w, h, n_boxes;
int offset[4] = {0, 0, 1, -1};
uint8_t *board, *goals, *live, *tmpmap;
int *dist;

typedef uint16_t cidx_t;
typedef unsigned int hash_t;

/* board configuration is represented by an array of cell indices
   of player and boxes */
typedef struct state_t state_t;

struct state_t { // variable length
	hash_t h;
	int depth;
	state_t *prev, *next, *qprev, *qnext;
	cidx_t c[];
};

size_t boxsize;
size_t state_size, block_size = 32;
state_t *block_root, *block_head;

int alloced = 0;
inline
state_t* newstate(state_t *parent) {
	inline state_t* next_of(state_t *s) {
		return (void*)((uint8_t*)s + state_size);
	}

	state_t *ptr;
	if (!block_head) {
		block_size *= 2;
		state_t *p;
		ensure(p = malloc(block_size * state_size));
		alloced += block_size - 1;
		p->next = block_root;
		block_root = p;
		ptr = (void*)((uint8_t*)p + state_size * block_size);
		p = block_head = next_of(p);
		state_t *q;
		for (q = next_of(p); q < ptr; p = q, q = next_of(q))
			p->next = q;
		p->next = NULL;
	}

	ptr = block_head;
	block_head = block_head->next;

	ptr->prev = parent;
	ptr->qprev = ptr->qnext = 0;
	ptr->h = 0;
	return ptr;
}

inline
void unnewstate(state_t *p) {
	p->next = block_head;
	block_head = p;
}

enum { space, wall, player, box };

#define E "\033["
const char * const glyph1[] = { " ", "#", E"31m@"E"m", E"33m$"E"m"};
const char * const glyph2[] = { E"32m."E"m", "#", E"32m@"E"m", E"32m$"E"m"};
#undef E

// mark up positions where a box definitely should not be
void mark_live(const int c)
{
	if (live[c]) return;

	live[c] = 1;
	for (int i = 0; i < 4; i++) {
		int d = offset[i];
		if (board[c + d] != wall && board[c + d * 2] != wall)
			mark_live(c + d);
	}
}

state_t *parse_board(const int y, const int x, const char *s)
{
	w = x, h = y;
	ensure( board = calloc(w * h, sizeof(uint8_t)) );
	ensure( tmpmap= calloc(w * h, sizeof(uint8_t)) );
	ensure( goals = calloc(w * h, sizeof(uint8_t)) );
	ensure( live  = calloc(w * h, sizeof(uint8_t)) );
	ensure( dist  = calloc(w * h, sizeof(int)) );

	n_boxes = 0;
	for (int i = 0; s[i]; i++) {
		switch(s[i]) {
		case '#':	board[i] = wall;
				continue;

		case '.':	// fallthrough
		case '+':	goals[i] = 1; // fallthrough
		case '@':	continue;

		case '*':	goals[i] = 1; // fallthrough
		case '$':	n_boxes++;
				continue;
		default:	continue;
		}
	}

	const int is = sizeof(int);
	state_size = (sizeof(state_t) + (1 + n_boxes) * sizeof(cidx_t) + is - 1)
			/ is * is;

	boxsize = sizeof(cidx_t) * (1 + n_boxes);
	state_t *state = newstate(NULL);
	state->depth = 0;

	offset[0] = w;
	offset[1] = -w;

	for (int i = 0, j = 0; i < w * h; i++) {
		if (goals[i]) mark_live(i);
		if (s[i] == '$' || s[i] == '*')
			state->c[++j] = i;
		else if (s[i] == '@' || s[i] == '+')
			state->c[0] = i;
	}

	memcpy(tmpmap, board, sizeof(uint8_t) * w * h);
	return state;
}

void show_board(const state_t *s)
{
	printf("move %d\n", s->depth);
	unsigned char b[w * h];
	memcpy(b, board, w * h);

	b[ s->c[0] ] = player;
	for (int i = 1; i <= n_boxes; i++)
		b[ s->c[i] ] = box;

	for (int i = 0; i < w * h; i++) {
		printf((goals[i] ? glyph2 : glyph1)[ b[i] ]);
		if (! ((1 + i) % w))
			putchar('\n');
	}
}

// K&R hash function
inline
void hash(state_t *s)
{
	if (!s->h) {
		register hash_t ha = 0;
		cidx_t *p = s->c;
		for (int i = 0; i <= n_boxes; i++)
			ha = p[i] + 31 * ha;
		s->h = ha;
	}
}

state_t **buckets;
hash_t hash_size, fill_limit, filled;

void extend_table()
{
	int old_size = hash_size;

	if (!old_size) {
		hash_size = 1024;
		filled = 0;
		fill_limit = hash_size * 3 / 4; // 0.75 load factor
	} else {
		hash_size *= 4;
		fill_limit *= 4;
	}

	// rehash
	ensure(buckets = realloc(buckets, sizeof(state_t*) * hash_size));
	memset(buckets + old_size, 0, sizeof(state_t*) * (hash_size - old_size));

	const hash_t bits = hash_size - 1;
	for (int i = 0; i < old_size; i++) {
		state_t *head = buckets[i];
		buckets[i] = NULL;
		while (head) {
			state_t *next = head->next;
			const int j = head->h & bits;
			head->next = buckets[j];
			buckets[j] = head;
			head = next;
		}
	}
}

state_t *lookup(state_t *s)
{
	hash(s);
	state_t *f = buckets[s->h & (hash_size - 1)];
	while(f && !((f->h == s->h) && !memcmp(s->c, f->c, boxsize)))
		f = f->next;

	return f;
}

state_t* add_to_table(state_t *s)
{
	state_t *f;
	if ((f = lookup(s))) {
		if (f->depth > s->depth) {
			f->depth = s->depth;
			f->prev = s->prev;
			unnewstate(s);
			return f;
		}
		unnewstate(s);
		return 0;
	}

	if (filled++ >= fill_limit)
		extend_table();

	hash_t i = s->h & (hash_size - 1);

	s->next = buckets[i];
	buckets[i] = s;
	return s;
}

inline void sort_state(state_t *n)
{
	// leet bubble sort: boxes are indistinguishable
	cidx_t *p = n->c;
	for (int i = n_boxes; --i; ) {
		cidx_t t = 0;
		for (int j = 1; j < i; j++) {
			if (p[j] > p[j + 1])
				t = p[j], p[j] = p[j+1], p[j+1] = t;
		}
		if (!t) break;
	}
}

inline bool success(const state_t *s)
{
	for (int i = 1; i <= n_boxes; i++)
		if (!goals[s->c[i]]) return false;
	return true;
}

inline int deadlocked(state_t *s)
{
	for (int i = 1; i <= n_boxes; i++)
		if (!live[s->c[i]]) return 1;

	sort_state(s);

	int ret = 0;
	for (int i = 1; i <= n_boxes; i++)
		tmpmap[s->c[i]] = box;

	/* check if two boxes are next to each other and sitting along a wall
	   or some such */
	for (int i = 1; i < n_boxes; i++) {
		int x = s->c[i];
		int y = s->c[i + 1];
		if (y == x + 1 && !(goals[x] && goals[y])) {
			if ((tmpmap[x - w] >= wall && tmpmap[y - w] >= wall) ||
				(tmpmap[x + w] >= wall && tmpmap[y + w] >= wall))
			{
				ret = 1;
				goto bail;
			}
			if ((tmpmap[x - w] == wall || tmpmap[x + w] == wall) &&
				(tmpmap[y - w] == wall || tmpmap[y + w] == wall))
			{
				ret = 1;
				goto bail;
			}
		}
		for (int j = i + 1; j <= n_boxes; j++) {
			y = s->c[j];

			if (y == x + w) {
				if (goals[x] && goals[y]) continue;
				if ((tmpmap[x - 1] >= wall && tmpmap[y - 1] >= wall) ||
					(tmpmap[x + 1] >= wall && tmpmap[y + 1] >= wall))
				{
					ret = 1;
					goto bail;
				}
				if ((tmpmap[x - 1] == wall || tmpmap[x + 1] == wall) &&
					(tmpmap[y - 1] == wall || tmpmap[y + 1] == wall))
				{
					ret = 1;
					goto bail;
				}
			}
		}
	}
bail:	for(int i = 1; i <= n_boxes; i++)
		tmpmap[s->c[i]] = space;

	return ret;
}

// Dijkstra's, calculate move distance from the state's player to all cells
void calc_dist(state_t *s)
{
	int qlen = 0, t = w * h, qidx[t];
	cidx_t pq[t], pop;
	memset(qidx, 0, sizeof(int) * t);
	for (int i = w * h; i--; ) dist[i] = t;

	inline void pq_push(cidx_t c, short d) {
		if (board[c] >= wall || dist[c] <= d) return;

		int i = qidx[c], j;
		if (!i) i = ++qlen;

		while (i > 1 && dist[pq[j = i / 2]] > d) {
			pq[i] = pq[j];
			qidx[pq[i]] = i;
			i = j;
		}
		qidx[c] = i;
		pq[i] = c;
		dist[pq[i]] = d;
	}

	inline void pq_pop() {
		pop = pq[1];
		short tmp = pq[qlen--];
		pq[0] = pq[1];
		int i = 1, j;
		for (i = 1; (j = i * 2) <= qlen; i = j) {
			if (j < qlen && dist[pq[j]] > dist[pq[j + 1]]) j++;
			if (dist[pq[j]] >= dist[tmp]) break;
			pq[i] = pq[j];
			qidx[pq[i]] = i;
		}
		pq[i] = tmp;
		qidx[tmp] = i;
	}

	pq_push(s->c[0], 0);
	while (qlen) {
		pq_pop();
		short d = dist[pop] + 1;
		pq_push(pop - w, d);
		pq_push(pop + w, d);
		pq_push(pop - 1, d);
		pq_push(pop + 1, d);
	}
}

// make a new state by push a box in current state
state_t* push_me(state_t *s, int dc)
{
	int c1 = s->c[0] + dc;
	if (board[c1] != box) return 0;

	int c2 = c1 + dc;
	if (board[c2] >= wall) return 0;

	state_t *n = newstate(s);
	n->depth = s->depth + 1;

	for (int i = 1; i <= n_boxes; i++)
		n->c[i] = (s->c[i] == c1) ? c2 : s->c[i];
	n->c[0] = c1;
	return n;
}

#define MAX_MOVE 1024
state_t *solution, *queue[MAX_MOVE];
int known_moves = MAX_MOVE;

// if a better move is found for a configuration already queued
void unqueue_move(state_t *s)
{
	state_t *p = s->qnext;
	if (p) p->qprev = s->qprev;

	p = s->qprev;
	if (!p) {
		if (queue[s->depth] == s)
			queue[s->depth] = s->qnext;
	} else
		p->qnext = s->qnext;
}

int queue_move(state_t *s)
{
	if (!s) return 0;

	int d = s->depth;
	if (d >= known_moves || deadlocked(s)) {
		unnewstate(s);
		return 0;
	}

	state_t *f = lookup(s);
	if (f) {
		if (f->depth <= d) {
			unnewstate(s);
			return 0;
		}
		unqueue_move(f);
		f->depth = d;
		f->prev = s->prev;
		unnewstate(s);
		s = f;
	} else
		add_to_table(s);

	s->qprev = 0;
	if ((s->qnext = queue[d]))
		s->qnext->qprev = s;

	queue[d] = s;

	if (success(s)) {
		if (s->depth < known_moves) {
			known_moves = s->depth;
			solution = s;
			printf("\nfound solution at move %d\n", known_moves);
		}
	}

	return 1;
}

// this is called for queued moves; only push moves ever show up in queue
int do_move(state_t *s)
{
	for (int i = 1; i <= n_boxes; i++)
		board[s->c[i]] = box;

	calc_dist(s);

	int t = w * h;
	uint8_t near_box[t];
	memset(near_box, 0, t);

	for (int i = 1; i <= n_boxes; i++) {
		int c = s->c[i];
		near_box[c - w] = near_box[c + w]
			= near_box[c - 1] = near_box[c + 1] = 1;
	}

	for (int i = w + 1; i < w * (h - 1); i++) {
		if (!near_box[i] || dist[i] >= t)
			continue;
		state_t *n = s;
		if (dist[i]) {
			n = newstate(s);
			memcpy(n->c, s->c, boxsize);
			n->c[0] = i;
			n->depth = s->depth + dist[i];
			n = add_to_table(n);
		}

		if (!dist[i] || n) {
			queue_move(push_me(n, -1));
			queue_move(push_me(n,  1));
			queue_move(push_me(n, -w));
			queue_move(push_me(n,  w));
		}
	}

	for (int i = 1; i <= n_boxes; i++)
		board[s->c[i]] = space;

	return 0;
}

void show_moves(state_t *s)
{
	if (s->prev) {
		show_moves(s->prev);
		int d = s->depth - s->prev->depth;
		if (d > 1) {
			for (int i = 1; i <= n_boxes; i++)
				board[s->c[i]] = box;
			calc_dist(s);
			s->c[0] = s->prev->c[0];
			s->depth = s->prev->depth;
			while (d--) {
				s->depth++;
				for (int i = 0; i < 4; i++) {
					int c = s->c[0] + offset[i];
					if (dist[c] == d) {
						s->c[0] = c;
						break;
					}
				}
				if (d) {
					printf("\033[H");
					show_board(s);
					usleep(100000);
				}
			}
			for (int i = 1; i <= n_boxes; i++)
				board[s->c[i]] = space;
		}
	}
	printf("\033[H");
	show_board(s);
	usleep(300000);
}

int main()
{
	state_t *s = parse_board(

#define BIG 0

#if BIG == 0
		8, 7,
		"#######"
		"#     #"
		"#     #"
		"#. #  #"
		"#. $$ #"
		"#.$$  #"
		"#.#  @#"
		"#######"

#elif BIG == 1
		5, 13,
		"#############"
		"#  #        #"
		"# $$$$$$$  @#"
		"#.......    #"
		"#############"
#elif BIG == 2
		5, 13,
		"#############"
		"#... #      #"
		"#.$$$$$$$  @#"
		"#...        #"
		"#############"

#elif BIG == 3
		11, 19,
		"    #####          "
		"    #   #          "
		"    #$  #          "
		"  ###  $##         "
		"  #  $ $ #         "
		"### # ## #   ######"
		"#   # ## #####  ..#"
		"# $  $          ..#"
		"##### ### #@##  ..#"
		"    #     #########"
		"    #######        "
#else
		11, 13,
		"#############"
		"####     ####"
		"#   .### ####"
		"# # #    ####"
		"# # $ $#. ###"
		"# #  *  # ###"
		"# .#$ $ # ###"
		"##    # # ###"
		"## ###.    @#"
		"##     ##   #"
		"#############"
#endif
			);

	show_board(s);
	extend_table();

	do_move(s);

	for (int d = 1; d < known_moves; d++) {
		state_t *s = queue[d];
		printf("depth %d (%d records, mem: %d/%dM)\r", d, filled,
			filled * state_size / (1 << 20),
			alloced * state_size / (1 << 20));
		fflush(stdout);

		for (; s; s = s->qnext) do_move(s);
	}

	if (solution) {
		printf("\npress any key to see solution\n");
		getchar();
		puts("\033[H\033[J");
		show_moves(solution);
	}

#if 0
	free(buckets);
	free(board);
	free(tmpmap);
	free(goals);
	free(live);
	free(dist);

	while (block_root) {
		void *tmp = block_root->next;
		free(block_root);
		block_root = tmp;
	}
#endif

	return 0;
}
```

