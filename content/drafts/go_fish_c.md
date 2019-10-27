+++
title = "Go Fish/C"
description = ""
date = 2013-04-20T01:49:14Z
aliases = []
[extra]
id = 10206
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}
Reasonably smart computer AI.  Programs require utf-8 locale.  AI keeps a record of probabilities of each card player may have and always asks for the card with highest score (which backfires quite often btw).

```c>#include <stdio.h

#include <stdlib.h>
#include <locale.h>
#include <ctype.h>
#include <string.h>

#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include <time.h>

int irand(int n)
{
	int r, rand_max = RAND_MAX - (RAND_MAX % n);
	while ((r = rand()) >= rand_max);
	return r / (rand_max / n);
}

/* ------------ keyboard stuff ------------- */
void set_mode(int want_key)
{
	static struct termios old, new;
	if (!want_key) {
		tcsetattr(STDIN_FILENO, TCSANOW, &old);
		return;
	}

	tcgetattr(STDIN_FILENO, &old);
	new = old;
	new.c_lflag &= ~(ICANON);
	tcsetattr(STDIN_FILENO, TCSANOW, &new);
}

int getkey(const char *prompt)
{
	int c = 0;
	fd_set fs;

	printf("%s: ", prompt);
	fflush(stdout);

	set_mode(1);
	FD_ZERO(&fs);
	FD_SET(STDIN_FILENO, &fs);

	select(STDIN_FILENO + 1, &fs, 0, 0, 0);
	if (FD_ISSET(STDIN_FILENO, &fs)) {
		c = getchar();
		set_mode(0);
	}
	return c;
}

int anykey() { return getkey("Press any key to continue"); }

/* display functions */
void xy(int x, int y) { printf("\033[%d;%dH", y + 1, x + 1); }
void clear() { printf("\033[J"); }

/* ---------- card functions ------------- */
wchar_t ssuit[] = L" ♠♥♦♣", snum[] = L" A23456789TJQK";
typedef unsigned char cnum;
typedef struct { cnum suit, num; wchar_t name[3]; } card_t, *card;
typedef struct { card_t card[52]; int n; } deck_t, *deck;

typedef struct { card_t c; int n; } shuffle_t;
int shuffle_cmp(const void * a, const void *b)
{
	int x = ((const shuffle_t*)a)->n, y = ((const shuffle_t*)b)->n;
	return x < y ? -1 : x > y;
}

int card_cmp(const void *aa, const void *bb)
{
	card a = *(const card*)aa, b = *(const card*)bb;
	if (a == b) return 0;
	if (!a) return 1;
	if (!b) return -1;
	if (a->num < b->num) return -1;
	if (a->num > b->num) return 1;
	if (a->suit < b->suit) return -1;
	return a->suit > b->suit;
}

void card_shuffle(deck d)
{
	int i;
	shuffle_t x[52];
	for (i = 0; i < d->n; i++) {
		x[i].c = d->card[i];
		x[i].n = rand();
	}
	qsort(x, d->n, sizeof(shuffle_t), shuffle_cmp);
	for (i = 0; i < d->n; i++)
		d->card[i] = x[i].c;
}

deck card_init_deck(deck d)
{
	int j;
	card c = d->card;
	for (j = 0; j < 52; j++, c++) {
		c->suit = j / 13 + 1; c->num  = (j % 13) + 1;
		c->name[0] = ssuit[c->suit];
		c->name[1] = snum[c->num];
		c->name[2] = 0;
	}
	d->n = 52;
	card_shuffle(d);
	return d;
}

card card_deal(deck d)
{
	if (!d->n) return 0;
	return d->card + --d->n;
}

/* ---------- player functions ------------ */
typedef struct player_t player_t, *player;
typedef cnum (*ask_func)(player);
typedef struct {
	int init_done;
	double prob[14];
	int n_wild[14], draws[14];
} ai_record_t;

enum { init_deal, opponent_has, opponent_hasnot, opponent_draw, self_draw, book_down };
void tell_ai(player, int action, cnum n);

cnum human_ask(player);
cnum computer_ask(player);

struct player_t {
	player opponent;
	card cards[52];
	cnum books[14];
	int n_books, n_cards;
	const char *name;
	ask_func ask;
	ai_record_t * ai;
};

void player_sort_hand(player p)
{
	qsort(p->cards, p->n_cards, sizeof(card), card_cmp);
}

void player_add_card(player p, card c)
{
	p->cards[p->n_cards++] = c;
	player_sort_hand(p);
}

card player_remove_card(player p, int i)
{
	card c = p->cards[i];
	memmove(p->cards + i, p->cards + i + 1, (p->n_cards - i) * sizeof(card));
	p->n_cards--;
	return c;
}

card player_draw_card(player p, deck d)
{
	card c = card_deal(d);
	if (!c) return 0;
	printf(p->ai	? "* %s drew a card\n"
			: "* %s went fishing and got a %ls\n",
		p->name, c->name);
	player_add_card(p, c);
	tell_ai(p, self_draw, c->num);
	tell_ai(p->opponent, opponent_draw, 0);
	return c;
}

int player_has(player p, cnum n)
{
	int i;
	for (i = 0; i < p->n_cards; i++)
		if (p->cards[i]->num == n) return 1;
	return 0;
}

int player_book_check(player p)
{
	int i, j;
	cnum c;
	for (i = 0; i < p->n_cards - 3; i++)
		if (p->cards[i]->num == p->cards[i + 3]->num)
			break;
	if (i >= p->n_cards - 3) return 0;

	printf("* %s put down book of %lc\n", p->name, snum[ p->cards[i]->num ]);
	c = p->books[p->n_books++] = p->cards[i]->num;
	for (j = 0; j < 4; j++)
		player_remove_card(p, i);
	tell_ai(p, book_down, c);
	tell_ai(p->opponent, book_down, c);
	return 1;
}

/* ---------- game stuff -------------*/
typedef struct {
	player_t player[2];
	deck_t deck;
	int current; /* whose turn is it */
	ai_record_t puter_knows;
} game_t, *game;

int game_move(game g);
void game_human_move(game, player);

void game_new(game g, int first)
{
	int i;
	memset(g, 0, sizeof(game_t));
	card_init_deck(&g->deck);
	g->current = !!first;

	player puter = g->player, human = g->player + 1;

	puter->ai = &g->puter_knows;
	puter->opponent = human;
	puter->name = "Puter";
	puter->ask = computer_ask;

	human->ask = human_ask;
	human->opponent = puter;
	human->name = "You";

	for (i = 0; i < 9; i++) {
		player_draw_card(puter, &g->deck);
		player_draw_card(human, &g->deck);
	}
	puter->ai->init_done = 1;
	tell_ai(puter, init_deal, 0);
	while(game_move(g));
}

void game_display(game g)
{
	int i;
	player p1 = g->player, p2 = p1 + 1;

	xy(0, 0); clear();
	for (i = 1; i < 14; i++)
		printf("[%lc]%.1f  ", snum[i], g->player[0].ai->prob[i]);
	printf("\n");

	//xy(0, 0); clear(); printf("[ %s ]", p1->name);
	xy(0, 1); printf("Cards:");
		for (i = 0; i < p1->n_cards; i++)
			printf("%ls", L" ▒ ");

	xy(0, 2); printf("Books:");
		for (i = 0; i < p1->n_books; i++)
			printf(" %lc", snum[ p1->books[i] ]);

	xy(7, 4); printf("Deck: ");
		printf(g->deck.n ? "%d cards" : "empty", g->deck.n);

	xy(0, 6); printf("Books:");
		for (i = 0; i < p2->n_books; i++)
			printf(" %lc", snum[ p2->books[i] ]);

	xy(0, 7); printf("Cards:");
		for (i = 0; i < p2->n_cards; i++)
			printf(" %ls", p2->cards[i]->name);

	xy(0, 8); printf("[ %s ]", p2->name);
	xy(0, 9); for (i = 0; i < 35; i++) printf("%lc", L'─');
	xy(0, 10); printf("Current move: %s.", g->player[g->current].name);
	fflush(stdout);
}

void game_transfer_cards(game g, player from, player to, cnum n)
{
	int i;
	card c;
	printf("* %s gave %s", from->name, to->name);
	for (i = 0; i < from->n_cards && from->cards[i]->num != n; i++);
	while (i < from->n_cards && from->cards[i]->num == n) {
		c = player_remove_card(from, i);
		player_add_card(to, c);
		printf(" %ls", c->name);
	}
	printf("\n");
	tell_ai(to, opponent_hasnot, n);

	player_book_check(to);
	if (!from->n_cards) player_draw_card(from, &g->deck);
	if (!to->n_cards) player_draw_card(to, &g->deck);
}

int game_move(game g)
{
	cnum req;
	int i;
	player p = g->player + g->current;
	player o = p->opponent;

	game_display(g);
	for (i = 0; i < 2; i++) {
		if (g->player[i].n_books >= 7) {
			xy(0, 10); clear();
			printf("%s won!\n", g->player[i].name);
			anykey();
			return 0;
		}
	}

	if (p->ask) {
		req = p->ask(p);
		tell_ai(o, opponent_has, req);

		xy(0, 10); clear();
			printf("%s: \"Got any %lc?\"\n", p->name, snum[ req ]);
		xy(0, 11); printf("%s: ", o->name);
		if (player_has(o, req)) {
			printf("\"Yes.\"\n");
			game_transfer_cards(g, o, p, req);
			anykey();
			return 1;
		} else {
			tell_ai(p, opponent_hasnot, req);
			printf("\"Go fish.\"\n");
			if (!g->deck.n)
				printf("* But %s can't go fish because deck is empty\n",
					p->name);
			else {
				player_draw_card(p, &g->deck);
				player_book_check(p);
			}
		}
		if (!p->n_cards) player_draw_card(p, &g->deck);
		if (!o->n_cards) player_draw_card(o, &g->deck);
		anykey();
	}

	g->current = !g->current;
	return 1;
}

/* let human request a card from opponent */
cnum human_ask(player p)
{
	int i, c;
	do {
		xy(0, 11); clear(); printf("You may ask for");
		for (i = 1; i < 14; i++)
			if (player_has(p, i)) printf(" [%lc]", snum[i]);
		c = toupper(getkey(". Your choice"));
		for (i = 1; i < 14; i++) {
			if (c != snum[i]) continue;
			if (!player_has(p, i)) break;
			return i;
		}
		xy(0, 13);
		printf(i < 14	? "You can't ask for that card. "
				: "Dude, that's not a card. ");
	} while(anykey());
	return 0;
}

/* ------------- AI stuff ------------ */
cnum computer_ask(player p)
{
	int i, j = 1;
	cnum r = 0;
	double prob = 0;
	for (i = 1; i < 14; i++) {
		if (!player_has(p, i)) continue;
		if (p->ai->prob[i] > prob) {
			r = i; prob = p->ai->prob[i];
		}
		j++;
	}

	return r;
}

void check_opponent_draw(player p)
{
	ai_record_t *ai = p->ai;
	int sum = 0, i, j;
	double ch;

	if (!ai) return;

	for (i = 1; i < 14; i++) {
		if (ai->prob[i] < 0 || ai->prob[i] >= 1) continue;
		sum += ai->n_wild[i]; 
	}
	for (i = 1; i < 14; i++) {
		if (ai->prob[i] < 0 || ai->prob[i] >= 1) continue;
		if (!ai->n_wild[i]) continue;
		ch = 1;
		ai->draws[i] ++;
		for (j = 0; j < ai->draws[i] && j < sum; j++)
			ch *= (sum - j - ai->n_wild[i]) * 1.0 / (sum - j);
		ai->prob[i] = 1 - ch;
	}
}

void tell_ai(player p, int action, cnum n)
{
	int i;
	ai_record_t *ai = p->ai;
	if (!ai) return;
	if (!ai->init_done) return;

	/* count cards */
	for (i = 1; i < 14; i++) ai->n_wild[i] = 4;
	for (i = 0; i < p->n_cards; i++)
		ai->n_wild[ p->cards[i]->num ]--;
	for (i = 0; i < p->n_books; i++)
		ai->n_wild[ p->books[i] ] = 0;
	for (i = 0; i < p->opponent->n_books; i++)
		ai->n_wild[ p->opponent->books[i] ] = 0;

	if (action == init_deal) {
		for (i = 0; i < 9; i++)
			check_opponent_draw(p);
		return;
	}

	if (action == book_down) {
		ai->prob[n] = -1;
		ai->n_wild[n] = 0;
		return;
	}

	if (action == opponent_hasnot) {
		ai->prob[n] = 0;
		ai->draws[n] = 0;
		return;
	}

	if (action == opponent_has) {
		ai->prob[n] = 1;
		return;
	}

	if (action == self_draw) {
		--ai->n_wild[n];
		return;
	}

	if (action == opponent_draw) {
		check_opponent_draw(p);
	}
}

int main()
{
	game_t g;
	setlocale(LC_CTYPE, "");
	srand(time(0));
	game_new(&g, 1);
	game_display(&g);
	return 0;
}
```

