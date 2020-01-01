+++
title = "RCRPG/C"
description = ""
date = 2012-12-27T06:42:31Z
aliases = []
[extra]
id = 4057
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
This [[C]] version of [[RCRPG]] implements a single key text interface based on [[wp:ncurses|ncurses]].

==What's in use==

This version is a all-in-one file approach; it would be better to split in includes files at least.

* [[wp:ncurses|ncurses]] library for advanced terminal I/O
* functions (also with variable number of arguments)
* enumerations
* structures, their allocation and pointers
* arrays
* loops
* LIFO lists implemented through index and array

==Commands==

All commands are given by hitting a single key. The command dock
displays

<b>n</b>orth <b>s</b>outh <b>e</b>ast <b>w</b>est <b>u</b>p <b>d</b>own <b>g</b>e<b>t</b> d<b>r</b>op <b>a</b>rm <b>h</b>it e<b>xX</b>change <b>H</b>elp <b>Q</b>uit

which also summarizes all the commands the user can enter. What the user can see, what can take and what s/he's carrying is always displayed on the terminal.

The ''rename room'' command is not implemented.

The ''exchange'' command rotates the list of items the user can see or s/he's carrying. This is needed since the "action" is always performed on/with the last item listed.

The Help explains all you need to know (and contains also some cheat
matter if you compiled it this way!).

To avoid the use of the key '''e''' or '''q''' (for equip), what's called ''to equip'' in the Perl version is here called ''to arm''. When the sledge is armed/equipped, it can be used to break walls and discover new rooms.

There's at least one ladder per floor, but other ladders may appear by chance, altogether with sledges and golds.

Ladders position and prize room are random.

==Code==
{{works with|POSIX}}
{{libheader|ncurses}}

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdarg.h>
#include <time.h>
#include <string.h>

#include <curses.h>

#define MAX(a,b) ( (a)>(b) ? (a) : (b) )

/* position */
struct position {
  unsigned int ns, ew, ud;
};
typedef struct position position_t;

/* objects */
enum objects {
  RANDOM = -1, NOTHING=0, SLEDGE, LADDER, GOLD, EQUIPPED_SLEDGE
};
typedef enum objects object_t;

/* rooms */
#define DESCR_LEN 256
#define NS_LIMIT 4
#define EW_LIMIT 4
#define UD_LIMIT 4
#define MAX_OBJS_PER_ROOM 20

enum dir {
  NORTH=0, SOUTH,
  EAST, WEST,
  UP, DOWN, DIR_NUM
};
enum dir invdir[DIR_NUM] = { SOUTH, NORTH, WEST, EAST, DOWN, UP };

position_t incdir[DIR_NUM] = {
  { 1, 0, 0 },
  { -1, 0, 0 },
  { 0, 1, 0 }, { 0, -1, 0 },
  { 0, 0, 1 }, { 0, 0, -1 }
};

const char *str_dirs = "nsewud";

struct room {
  struct room *dirs[DIR_NUM];
  object_t ol[MAX_OBJS_PER_ROOM];
  int lo; /* last object */
  int description;
  bool visited;
};
typedef struct room room_t;

/* descriptions */
const char *descriptions[] = {
  "cold dark room",
  "very stinky room",
  "small but comfortable room",
  "room with an incredible echo! Yahollaydoo!",
  "room resembling a window-less jail",
  "green room",
  "black room",
  "blue room",
  "brown sad room",
  "room which breathes pain"
};

const char *objs[] = {
  "nothing", "sledge", "ladder", "gold", "equipped sledge"
};

const char *dirnames[] = {
  "north", "south", "east", "west", "up", "down"
};

/* room stores and more (private) */
room_t *rooms[NS_LIMIT*EW_LIMIT*UD_LIMIT];
unsigned int room_i = 0;
int py = 0;

/* what you carry */
#define MAX_CARRYABLE 25
object_t carrylist[MAX_CARRYABLE];
unsigned int carried_number = 0;

/* where are you */
position_t my_pos = { NS_LIMIT>>1, EW_LIMIT>>1, UD_LIMIT>>1 };
position_t goal_pos;
room_t *currentroom = NULL;

/* ladders pos.s (global) */
position_t ladders[UD_LIMIT];



/* random */
int rangerandom(int from , int to)
{
  int t;
  if ( from > to ) {
    t = from; from = to; to = t;
  }
  return from + (int) ((to-from+1)*(rand() / (RAND_MAX+1.0)));
}


/* add_object */
void add_object(object_t *ol, int *n, object_t o)
{
  if ( *n < MAX_OBJS_PER_ROOM ) {
    ol[*n] = o; (*n)++;
  }
}

/* check goal */
bool in_pos(position_t *p)
{
  if ( (my_pos.ns == p->ns) &&
       (my_pos.ew == p->ew) &&
       (my_pos.ud == p->ud) ) return true;
  return false;
}

/* ROOM */
room_t *create_room(enum objects ob1, ...)
{
  va_list ap;
  enum objects o;
  int i, l;

  if ( room_i >= (NS_LIMIT*EW_LIMIT*UD_LIMIT) ) return NULL;
  rooms[room_i] = malloc(sizeof(room_t));
  for(i=0; i < DIR_NUM; i++) rooms[room_i]->dirs[i] = NULL;
  rooms[room_i]->lo = 0;

  if ( ob1 != NOTHING )
    add_object(rooms[room_i]->ol, &rooms[room_i]->lo, ob1);

  va_start(ap, ob1);
  while( (o = va_arg(ap, enum objects)) != NOTHING ) {
    add_object(rooms[room_i]->ol, &rooms[room_i]->lo, o);
  }
  va_end(ap);

  rooms[room_i]->visited = false;
  rooms[room_i]->description = rangerandom(0, sizeof(descriptions)/sizeof(*descriptions) - 1);

  /* put some random object - sledge or gold */
  for(i=0; i < 4; i++) {
    if ( rangerandom(0,10) > 5 ) {
      l = rangerandom(0,5);
      add_object(rooms[room_i]->ol, &rooms[room_i]->lo, (l>=2)? GOLD : SLEDGE);
      /* a ladder per floor exists, but we can be lucky and find an extra one... */
      if ( rangerandom(0,20) == 11 ) {
	add_object(rooms[room_i]->ol, &rooms[room_i]->lo, LADDER);
      }
    }
  }

  room_i++;

  return rooms[room_i-1];
}

void free_rooms()
{
  int i;
  for(i=0; i < room_i; i++) free(rooms[i]);
}


/* show description */

void print_cmd(int y, int x, const char *str)
{
  int i, l;

  l = strlen(str);
  move(y, x);
  for(i=0; i < l; i++) {
    if ( str[i] != '_' ) {
      addch(str[i]);
    } else {
      i++;
      attron(A_BOLD);
      addch(str[i]);
      attroff(A_BOLD);
    }
  }
}

void show_description()
{
  int i, pe;
  char buf[100];

  py = 0;
  clear();
  move(0,0);
  addstr("You are in ");
  if ( currentroom->visited ) {
    addstr("the ");
  } else {
    addstr("a ");
  }
  addstr(descriptions[currentroom->description]);
  sprintf(buf, " (%d,%d,%d)", my_pos.ns, my_pos.ew, my_pos.ud);
  addstr(buf);

  move(3, 0);
  addstr("You can see:");
  py = 4; move(py, 0);
  /* add ladder if it's here */
  if ( (ladders[my_pos.ud].ns == my_pos.ns) &&
       (ladders[my_pos.ud].ew == my_pos.ew) ) {
    add_object(currentroom->ol, &currentroom->lo, LADDER);
    ladders[my_pos.ud].ns = -1;
    ladders[my_pos.ud].ew = -1;
  }
  if ( currentroom->lo == 0 ) {
    addstr("nothing");
  } else {
    for(i=0; i < currentroom->lo; i++) {
      addstr(objs[currentroom->ol[i]]);
      if ( i < (currentroom->lo - 1) ) {
	addstr(", ");
      }
      if ( ((i % 6) == 0) && (i>0) ) {
	py++; move(py, 0);
      }
    }
  }
  py = MAX(7, py+1);
  mvaddstr(py++, 0, "Possible exits:");
  move(py, 0);
  pe = 0;
  for(i=0; i < DIR_NUM; i++) {
    if ( currentroom->dirs[i] != NULL ) {
      addstr(dirnames[i]);
      addstr(" "); pe++;
    }
  }
  if ( pe == 0 ) {
    addstr("nowhere");
  }
  py += 2;
  mvaddstr(py++, 0, "You are carrying:");
  move(py, 0);
  if ( carried_number == 0 ) {
    addstr("nothing");
  } else {
    for(i=0; i < carried_number; i++) {
      addstr(objs[carrylist[i]]);
      if ( i < (carried_number-1) ) {
	addstr(", ");
      }
      if ( (i>0) && ( (i % 6) ==  0 ) ) {
	move(++py, 0);
      }
    }
  }
  py += 2;
  print_cmd(py, 0, "_north _south _east _west _up _down _ge_t d_rop _arm _hit e_x_Xchange _Help _Quit");
  move(++py, 0); addstr("> ");
}

void help()
{
  clear();
  mvaddstr(0,0,
	   "Welcome to RC Minimalist RPG C version 0.1\n\n"
	   "The aim is to find the room of the treasure.\n"
	   "You must use the equipped/armed sledge to break walls\n"
	   "and find your passage to a new room. You need ladders\n"
	   "to go up and down, but you can't bring a ladder to a new\n"
	   "floor, sorry. Press the keys as explained in the bottom\n"
	   "line to go north, south... or to take or drop objects.\n"
	   "You can't carry more than a certain number of them.\n"
	   "You drop the last object taken and you can take\n"
	   "the last object you see; use \"exXchange\" to drop/take another\n"
	   "object.\n"
	   "(Less typing must be compensated by at least one odd feature!)\n\n"
	   "Press any key to go back to the game\n"
    );
  if ( getch() == 'x' ) {
    clear();
    char buf[100];
    sprintf(buf, "%2d,%2d,%2d current", my_pos.ns, my_pos.ew, my_pos.ud);
    mvaddstr(0,0,buf);
    sprintf(buf, "%2d,%2d,%2d goal", goal_pos.ns, goal_pos.ew, goal_pos.ud);
    mvaddstr(1,0, buf);
    sprintf(buf, "%2d,%2d,%2d ladder", ladders[my_pos.ud].ns,
	    ladders[my_pos.ud].ew, ladders[my_pos.ud].ud);
    mvaddstr(2,0, buf); getch();
  }
}

/* cmd_get */
bool cmd_get()
{
  if ( carried_number == MAX_CARRYABLE ) {
    clrtoeol();
    addstr("You are carrying too much!");
    move(py, 2);
    return false;
  }
  if ( currentroom->lo > 0 ) {
      currentroom->lo--;
      carrylist[carried_number++] = currentroom->ol[currentroom->lo];
  }
  return true;
}

/* cmd_equip */
bool cmd_equip()
{
  if ( carried_number == 0 ) {
    clrtoeol();
    addstr("What should I arm?!");
    move(py, 2); return false;
  }
  if ( carrylist[carried_number-1] == EQUIPPED_SLEDGE ) {
    clrtoeol();
    addstr("Sledge already armed and ready to fire!");
    move(py, 2); return false;
  } else if ( carrylist[carried_number-1] != SLEDGE ) {
    clrtoeol();
    addstr("I can't arm ");
    addstr(objs[carrylist[carried_number-1]]);
    move(py, 2); return false;
  }
  carrylist[carried_number-1] = EQUIPPED_SLEDGE;
  return true;
}

bool cmd_drop(void);
bool cmd_hit()
{
  int d, c;
  bool nok = true;

  if ( carried_number == 0 ) {
    clrtoeol();
    addstr("Need something?");
    move(py, 2);
    return false;
  }
  if ( carrylist[carried_number-1] != EQUIPPED_SLEDGE ) {
    clrtoeol();
    addstr("Launched the ");
    addstr(objs[carrylist[carried_number-1]]);
    addstr(", nothing happened (Press a key)");
    getch();
    return cmd_drop();
  }
  clrtoeol();
  addstr("Choose a direction now...");
  move(py, 2);
  while(nok) {
    c = getch();
    switch(c) {
    case 'n':
    case 's':
    case 'e':
    case 'w':
    case 'u':
    case 'd':
      d = strchr(str_dirs, c) - str_dirs;
      if ( ((my_pos.ns + incdir[d].ns) >= NS_LIMIT) ||
	   ((my_pos.ew + incdir[d].ew) >= EW_LIMIT) ||
	   ((my_pos.ud + incdir[d].ud) >= UD_LIMIT) ||
	   ((my_pos.ns + incdir[d].ns) < 0 ) ||
	   ((my_pos.ew + incdir[d].ew) < 0 ) ||
	   ((my_pos.ud + incdir[d].ud) < 0 ) ) {
	clrtoeol();
	addstr("Hmm, this wall is harder and can't be broken! (Press a key)"); getch();
	return cmd_drop();
      }
      if ( currentroom->dirs[d] != NULL ) {
	carried_number--;
	add_object(currentroom->dirs[d]->ol, &currentroom->dirs[d]->lo, SLEDGE);
	clrtoeol();
	addstr("Wow! It slides very well!! (Press a key)"); getch();
	move(py, 2); return true;
      }
      currentroom->dirs[d] = create_room(NOTHING, NOTHING);
      currentroom->dirs[d]->dirs[invdir[d]] = currentroom;
      nok = false;
      break;
    default:
      clrtoeol();
      addstr("Not a direction! A direction, please!");
      move(py, 2);
    }
  }
  return true;
}

/* cmd_drop */
bool cmd_drop()
{
  if ( carried_number == 0 ) {
    clrtoeol();
    addstr("I am carrying nothing...");
    move(py, 2);
    return false;
  }
  carried_number--;
  if ( carrylist[carried_number] == EQUIPPED_SLEDGE )
    carrylist[carried_number] = SLEDGE;
  add_object(currentroom->ol, &currentroom->lo, carrylist[carried_number]);
  return true;
}

/* main loop */
bool do_action()
{
  int c, d;

  while(1)
  {
    c = getch();
    switch(c) {
    case 'Q': /* quit */
      addstr("quit");
      return true;
    case 'g': /* get, take */
    case 't':
      if ( currentroom->lo == 0 ) {
	clrtoeol();
	addstr("there's nothing I can get");
	move(py, 2);
      } else {
	if ( cmd_get() ) return false;
      }
      break;
    case 'r': /* drop, release */
      if ( cmd_drop() ) return false;
      break;
    case 'x':
      if ( carried_number > 1 ) {
	int i;
	enum objects o;
	for(i=0; i < (carried_number-1); i++) {
	  o = carrylist[i];
	  carrylist[i] = carrylist[i+1];
	  carrylist[i+1] = o;
	}
      }
      return false;
    case 'X':
      if ( currentroom->lo > 1 ) {
	int i;
	enum objects o;
	for(i=0; i < (currentroom->lo - 1); i++) {
	  o = currentroom->ol[i];
	  currentroom->ol[i] = currentroom->ol[i+1];
	  currentroom->ol[i+1] = o;
	}
      }
      return false;
    case 'n': /* directions */
    case 's':
    case 'e':
    case 'w':
    case 'u':
    case 'd':
      d = strchr(str_dirs, c) - str_dirs;
      if ( currentroom->dirs[d] == NULL ) {
	clrtoeol();
	addstr("can't go ");
	addstr(dirnames[d]);
	move(py, 2); break;
      } else {
	if ( (carried_number > 0) &&
	     ((c == 'u') || (c == 'd') ) &&
	     ( carrylist[carried_number-1] != LADDER ) ) {
	  clrtoeol();
	  addstr("Hm, I need to use a ladder...");
	  move(py, 2);
	  break;
	}
	if ( (c=='u') || (c=='d') ) cmd_drop(); /* we drop the ladder */
	my_pos.ns += incdir[d].ns;
	my_pos.ew += incdir[d].ew;
	my_pos.ud += incdir[d].ud;
	currentroom->visited = true;
	currentroom = currentroom->dirs[d];
      }
      return false;
    case 'a': /* equip / arm sledge */
      if ( cmd_equip() ) return false;
      break;
    case 'h': /* hit a wall */
      if ( cmd_hit() ) return false;
      break;
    case 'H':
      help();
      return false;
    }
  }
  return false;
}

int main()
{
  int i;
  bool quit = false;

  initscr(); cbreak(); noecho();

  srand(time(NULL));

  /* init ladders in at least one room of each "floor" */
  for(i=0; i < UD_LIMIT; i++) {
    ladders[i].ns = rangerandom(0, NS_LIMIT-1);
    ladders[i].ew = rangerandom(0, EW_LIMIT-1);
    ladders[i].ud = i;
  }

  /* goal pos */
  goal_pos.ns = rangerandom(0, NS_LIMIT-1);
  goal_pos.ew = rangerandom(0, EW_LIMIT-1);
  goal_pos.ud = rangerandom(0, UD_LIMIT-1);

  /* init room */
  currentroom = create_room(SLEDGE, GOLD, NOTHING);

  /* main loop */
  while( (! in_pos(&goal_pos)) && !quit )
  {
    show_description();
    quit = do_action();
    clrtoeol();
    refresh();
  }
  if ( in_pos(&goal_pos) ) { /* winner!! */
    clear();
    mvaddstr(0,0, "At the end you reached the goal room! I don't know\n"
                  "if you enjoyed the trip, nor if you've found what you\n"
                  "was searching for... I only hope you know better how\n"
	          "to curse now!!\n\n");
    int gld = 0; char buf[100];
    for(i=0; i < carried_number; i++) if (carrylist[i] == GOLD) gld++;
    sprintf(buf, "You earned %d pieces of gold\n\n", gld);
    addstr(buf);
    addstr("Press a key to quit the game");
    getch();
  }

  free_rooms();
  nocbreak(); echo(); endwin();
  return EXIT_SUCCESS;
}
}
```

