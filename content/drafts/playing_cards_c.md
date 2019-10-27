+++
title = "Playing cards/C"
description = ""
date = 2011-05-06T09:13:27Z
aliases = []
[extra]
id = 5312
[taxonomies]
categories = []
tags = []
+++

{{collection|Playing Cards}}

(The code lacks some checking, and I have not deeply tested it yet)

```c>#include <stdio.h


/* fits into a .h */
#include <stdlib.h>
#include <string.h>

#define N_SUITS 4
#define N_PIPS 13
#define N_CARDS (N_SUITS*N_PIPS)

enum cardsuit {
  CLUBS, HEARTS, SPADES, DIAMONDS
};
enum cardpip {
  TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT,
  NINE, TEN, JACK, QUEEN, KING, ACE
};
enum decktype {
  EMPTY_DECK, FULL_DECK
};

struct deckcard {
  enum cardsuit suit;
  enum cardpip pip;
};
typedef struct deckcard card_t;

struct deck {
  card_t *refcards[N_CARDS];
  card_t *cards[N_CARDS];
  int icards[N_CARDS];
  int n;
};
typedef struct deck deck_t;
/* end of .h */


const char *suits[N_SUITS] = {
  "clubs", "hearts", "spades", "diamonds"
};
const char *pips[N_PIPS] = {
  "2", "3", "4", "5", "6", "7", "8", "9", "10",
  "jack", "queen", "king", "ace"
};

static int rrand(int m)
{
  return (int)((double)m * ( rand() / (RAND_MAX+1.0) ));
}

card_t *new_card(enum cardpip p, enum cardsuit s)
{
  card_t *c;

  c = malloc(sizeof(card_t));
  c->suit = s; c->pip = p;
  return c;
}

void destroy_card(card_t *c)
{
  free(c);
}

void print_card(card_t *c)
{
  printf("(%s,%s)", pips[c->pip], suits[c->suit]);
}

static void generate_fullset(card_t **cset)
{
  int suit, pip;

  for(suit=0; suit < N_SUITS; suit++) {
    for(pip=0; pip < N_PIPS; pip++) {
      cset[suit*N_PIPS + pip] = new_card(pip, suit);
    }
  }
}

deck_t *new_deck(enum decktype t)
{
  deck_t *d;
  int i;

  d = malloc(sizeof(deck_t));
  if ( d != NULL ) {
    generate_fullset(d->refcards);
    switch(t) {
    case EMPTY_DECK:
      for(i=0; i < N_CARDS; i++) d->cards[i] = NULL;
      d->n = 0;
      break;
    case FULL_DECK:
      for(i=0; i < N_CARDS; i++) {
	d->cards[i] = d->refcards[i];
	d->refcards[i] = NULL;
	d->icards[i] = i;
      }
      d->n = N_CARDS;
      break;
    default:
      fprintf(stderr, "hm?\n");
    }
  }
  return d;
}

void destroy_deck(deck_t *d)
{
  int i;
  for(i=0; i < N_CARDS; i++) {
    free(d->refcards[i]);
    free(d->cards[i]);
  }
  free(d);
}

void shuffle_deck(deck_t *d)
{
  int i, t, r;

  for(i=0; i < d->n; i++) {
    r = rrand(d->n);
    t = d->icards[i];
    d->icards[i] = d->icards[r];
    d->icards[r] = t;
  }
}

card_t *deck_deal(deck_t *d)
{
  int c;
  card_t *uc;

  if ( d->n == 0 ) return NULL;
  d->n--;
  c = d->icards[d->n];
  d->refcards[c] = d->cards[c];
  d->cards[c] = NULL;
  /* make a copy for the user */
  uc = new_card(d->refcards[c]->pip, d->refcards[c]->suit);
  return uc;
}

void deck_addcard(deck_t *d, card_t *c)
{
  int r;
  int p, s;

  if ( d->n == N_CARDS ) {
    free(c);
    return;
  }
  if ( c == NULL ) {
    /* add a random card */
    do {
      r=rrand(N_CARDS);
    } while ( d->refcards[r] == NULL );
    d->cards[r] = d->refcards[r];
    d->refcards[r] = NULL;
    d->icards[d->n++] = r;
  } else {
    p = c->pip;
    s = c->suit;
    if ( d->cards[s*N_PIPS + p] == NULL ) {
      d->cards[s*N_PIPS + p] = d->refcards[s*N_PIPS + p];
      d->refcards[s*N_PIPS + p] = NULL;
      d->icards[d->n++] = s*N_PIPS + p;
    }
    free(c);
  }
}

void print_deck(deck_t *d)
{
  int i;

  for(i=0; i < d->n; i++) {
    print_card(d->cards[d->icards[i]]);
    printf("\n");
  }
}

/* testing */
int main()
{
  deck_t *a_deck;
  card_t *a_card;

  a_deck = new_deck(FULL_DECK);
  shuffle_deck(a_deck);
  a_card = deck_deal(a_deck);
  printf("picked a ");
  print_card(a_card);
  printf("\n\n");
  free(a_card);
  print_deck(a_deck);
  destroy_deck(a_deck);
  return 0;
}
```

