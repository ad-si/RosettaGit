+++
title = "Sokoban"
description = ""
date = 2019-06-02T08:15:01Z
aliases = []
[extra]
id = 9801
[taxonomies]
categories = ["Games", "Puzzles", "AI", "task"]
tags = []
languages = [
  "c",
  "cpp",
  "csharp",
  "d",
  "elixir",
  "go",
  "haskell",
  "java",
  "julia",
  "kotlin",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "ring",
  "ruby",
  "tcl",
]
+++

## Task

Demonstrate how to find a solution to a given [[wp:Sokoban|Sokoban]] level. For the purpose of this task (formally, a PSPACE-complete problem) any method may be used. However a move-optimal or push-optimal (or any other -optimal) solutions is preferred.

Sokoban levels are usually stored as a character array where
* ''space'' is an empty square
* # is a wall
* @ is the player
* $ is a box
* . is a goal
* + is the player on a goal
* * is a box on a goal

Sokoban solutions are usually stored in the LURD format, where lowercase l, u, r and d represent a move in that ('''l'''eft, '''u'''p, '''r'''ight, '''d'''own) direction and capital LURD represents a push.

Please state if you use some other format for either the input or output, and why.

For more information, see [http://www.sokobano.de/wiki/index.php?title=Main_Page the Sokoban wiki].


## C

Long, long, long C99 code (plus GNU C nested functions).  Doesn't output the movement keys, instead it animates the sequence for you.  Solution is move optimized.  For an even longer solution, see [[Sokoban/C]].

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>

int w, h, n_boxes;
uint8_t *board, *goals, *live;

typedef uint16_t cidx_t;
typedef uint32_t hash_t;

/* board configuration is represented by an array of cell indices
   of player and boxes */
typedef struct state_t state_t;

struct state_t { // variable length
	hash_t h;
	state_t *prev, *next, *qnext;
	cidx_t c[];
};

size_t state_size, block_size = 32;
state_t *block_root, *block_head;

inline
state_t* newstate(state_t *parent) {
	inline state_t* next_of(state_t *s) {
		return (void*)((uint8_t*)s + state_size);
	}

	state_t *ptr;
	if (!block_head) {
		block_size *= 2;
		state_t *p = malloc(block_size * state_size);
		assert(p);
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
	const int y = c / w, x = c % w;
	if (live[c]) return;

	live[c] = 1;
	if (y > 1 && board[c - w] != wall && board[c - w * 2] != wall)
		mark_live(c - w);
	if (y < h - 2 && board[c + w] != wall && board[c + w * 2] != wall)
		mark_live(c + w);
	if (x > 1 && board[c - 1] != wall && board[c - 2] != wall)
		mark_live(c - 1);
	if (x < w - 2 && board[c + 1] != wall && board[c + 2] != wall)
		mark_live(c + 1);
}

state_t *parse_board(const int y, const int x, const char *s)
{
	w = x, h = y;
	board = calloc(w * h, sizeof(uint8_t));
	assert(board);
	goals = calloc(w * h, sizeof(uint8_t));
	assert(goals);
	live  = calloc(w * h, sizeof(uint8_t));
	assert(live);

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

	state_t *state = newstate(NULL);

	for (int i = 0, j = 0; i < w * h; i++) {
		if (goals[i]) mark_live(i);
		if (s[i] == '$' || s[i] == '*')
			state->c[++j] = i;
		else if (s[i] == '@' || s[i] == '+')
			state->c[0] = i;
	}

	return state;
}

void show_board(const state_t *s)
{
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
		hash_size *= 2;
		fill_limit *= 2;
	}

	buckets = realloc(buckets, sizeof(state_t*) * hash_size);
	assert(buckets);

	// rehash
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
	for (; f; f = f->next) {
		if (//(f->h == s->h) &&
			!memcmp(s->c, f->c, sizeof(cidx_t) * (1 + n_boxes)))
			break;
	}

	return f;
}

bool add_to_table(state_t *s)
{
	if (lookup(s)) {
		unnewstate(s);
		return false;
	}

	if (filled++ >= fill_limit)
		extend_table();

	hash_t i = s->h & (hash_size - 1);

	s->next = buckets[i];
	buckets[i] = s;
	return true;
}

bool success(const state_t *s)
{
	for (int i = 1; i <= n_boxes; i++)
		if (!goals[s->c[i]]) return false;
	return true;
}

state_t *move_me(state_t *s, const int dy, const int dx)
{
	const int y = s->c[0] / w;
	const int x = s->c[0] % w;
	const int y1 = y + dy;
	const int x1 = x + dx;
	const int c1 = y1 * w + x1;

	if (y1 < 0 || y1 > h || x1 < 0 || x1 > w
			|| board[c1] == wall)
		return NULL;

	int at_box = 0;
	for (int i = 1; i <= n_boxes; i++) {
		if (s->c[i] == c1) {
			at_box = i;
			break;
		}
	}

	int c2;
	if (at_box) {
		c2 = c1 + dy * w + dx;
		if (board[c2] == wall || !live[c2])
			return NULL;
		for (int i = 1; i <= n_boxes; i++)
			if (s->c[i] == c2) return NULL;
	}

	state_t *n = newstate(s);
	memcpy(n->c + 1, s->c + 1, sizeof(cidx_t) * n_boxes);

	cidx_t *p = n->c;
	p[0] = c1;

	if (at_box) p[at_box] = c2;

	// leet bubble sort
	for (int i = n_boxes; --i; ) {
		cidx_t t = 0;
		for (int j = 1; j < i; j++) {
			if (p[j] > p[j + 1])
				t = p[j], p[j] = p[j+1], p[j+1] = t;
		}
		if (!t) break;
	}

	return n;
}

state_t *next_level, *done;

bool queue_move(state_t *s)
{
	if (!s || !add_to_table(s))
		return false;

	if (success(s)) {
		puts("\nSuccess!");
		done = s;
		return true;
	}

	s->qnext = next_level;
	next_level = s;
	return false;
}

bool do_move(state_t *s)
{
	return     queue_move(move_me(s,  1,  0))
		|| queue_move(move_me(s, -1,  0))
		|| queue_move(move_me(s,  0,  1))
		|| queue_move(move_me(s,  0, -1));
}

void show_moves(const state_t *s)
{
	if (s->prev)
		show_moves(s->prev);
	usleep(200000);
	printf("\033[H");
	show_board(s);
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

#else
		11, 19,
		"    #####          "
		"    #   #          "
		"    #   #          "
		"  ### #$##         "
		"  #      #         "
		"### #$## #   ######"
		"#   # ## #####   .#"
		"# $   $         ..#"
		"##### ### #@##   .#"
		"    #     #########"
		"    #######        "
#endif
			);

	show_board(s);
	extend_table();
	queue_move(s);
	for (int i = 0; !done; i++) {
		printf("depth %d\r", i);
		fflush(stdout);

		state_t *head = next_level;
		for (next_level = NULL; head && !done; head = head->qnext)
			do_move(head);

		if (!next_level) {
			puts("no solution?");
			return 1;
		}
	}

	printf("press any key to see moves\n");
	getchar(), puts("\033[H\033[J");
	show_moves(done);

#if 0
	free(buckets);
	free(board);
	free(goals);
	free(live);

	while (block_root) {
		void *tmp = block_root->next;
		free(block_root);
		block_root = tmp;
	}
#endif

	return 0;
}
```



## C++

===Set-based Version===
{{works with|C++11}}
{{wont work with|GCC 4.4.3|gcc -std=c++0x}}
This heavily abuses the STL, including some of the newer features like regex and tuples.

This performs a breadth-first search by moves, so the results should be a move-optimal solution.

```cpp
#include <iostream>
#include <string>
#include <vector>
#include <queue>
#include <regex>
#include <tuple>
#include <set>
#include <array>
using namespace std;

class Board
{
public:
  vector<vector<char>> sData, dData;
  int px, py;

  Board(string b)
  {
    regex pattern("([^\\n]+)\\n?");
    sregex_iterator end, iter(b.begin(), b.end(), pattern);

    int w = 0;
    vector<string> data;
    for(; iter != end; ++iter){
      data.push_back((*iter)[1]);
      w = max(w, (*iter)[1].length());
    }

    for(int v = 0; v < data.size(); ++v){
      vector<char> sTemp, dTemp;
      for(int u = 0; u < w; ++u){
        if(u > data[v].size()){
          sTemp.push_back(' ');
          dTemp.push_back(' ');
        }else{
          char s = ' ', d = ' ', c = data[v][u];

          if(c == '#')
            s = '#';
          else if(c == '.' || c == '*' || c == '+')
            s = '.';

          if(c == '@' || c == '+'){
            d = '@';
            px = u;
            py = v;
          }else if(c == '$' || c == '*')
            d = '*';

          sTemp.push_back(s);
          dTemp.push_back(d);
        }
      }

      sData.push_back(sTemp);
      dData.push_back(dTemp);
    }
  }

  bool move(int x, int y, int dx, int dy, vector<vector<char>> &data)
  {
    if(sData[y+dy][x+dx] == '#' || data[y+dy][x+dx] != ' ')
      return false;

    data[y][x] = ' ';
    data[y+dy][x+dx] = '@';

    return true;
  }

  bool push(int x, int y, int dx, int dy, vector<vector<char>> &data)
  {
    if(sData[y+2*dy][x+2*dx] == '#' || data[y+2*dy][x+2*dx] != ' ')
      return false;

    data[y][x] = ' ';
    data[y+dy][x+dx] = '@';
    data[y+2*dy][x+2*dx] = '*';

    return true;
  }

  bool isSolved(const vector<vector<char>> &data)
  {
    for(int v = 0; v < data.size(); ++v)
      for(int u = 0; u < data[v].size(); ++u)
        if((sData[v][u] == '.') ^ (data[v][u] == '*'))
          return false;
    return true;
  }

  string solve()
  {
    set<vector<vector<char>>> visited;
    queue<tuple<vector<vector<char>>, string, int, int>> open;

    open.push(make_tuple(dData, "", px, py));
    visited.insert(dData);

    array<tuple<int, int, char, char>, 4> dirs;
    dirs[0] = make_tuple(0, -1, 'u', 'U');
    dirs[1] = make_tuple(1, 0, 'r', 'R');
    dirs[2] = make_tuple(0, 1, 'd', 'D');
    dirs[3] = make_tuple(-1, 0, 'l', 'L');

    while(open.size() > 0){
      vector<vector<char>> temp, cur = get<0>(open.front());
      string cSol = get<1>(open.front());
      int x = get<2>(open.front());
      int y = get<3>(open.front());
      open.pop();

      for(int i = 0; i < 4; ++i){
        temp = cur;
        int dx = get<0>(dirs[i]);
        int dy = get<1>(dirs[i]);

        if(temp[y+dy][x+dx] == '*'){
          if(push(x, y, dx, dy, temp) && (visited.find(temp) == visited.end())){
            if(isSolved(temp))
              return cSol + get<3>(dirs[i]);
            open.push(make_tuple(temp, cSol + get<3>(dirs[i]), x+dx, y+dy));
            visited.insert(temp);
          }
        }else if(move(x, y, dx, dy, temp) && (visited.find(temp) == visited.end())){
          if(isSolved(temp))
            return cSol + get<2>(dirs[i]);
          open.push(make_tuple(temp, cSol + get<2>(dirs[i]), x+dx, y+dy));
          visited.insert(temp);
        }
      }
    }

    return "No solution";
  }
};

int main()
{
  string level =
    "#######\n"
    "#     #\n"
    "#     #\n"
    "#. #  #\n"
    "#. $$ #\n"
    "#.$$  #\n"
    "#.#  @#\n"
    "#######";

  Board b(level);

  cout << level << endl << endl << b.solve() << endl;
  return 0;
}
```


Output:

```txt

#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######

ulULLulDDurrrddlULrruLLrrUruLLLulD
```


===Unordered Set-based Version===
{{works with|C++11}}
{{works with|Boost}}
{{works with|GCC 4.6}}
Alternative version, about twice faster (about 2.1 seconds runtime), same output.

```cpp
#include <iostream>
#include <string>
#include <vector>
#include <queue>
#include <tuple>
#include <array>
#include <map>
#include <boost/algorithm/string.hpp>
#include <boost/unordered_set.hpp>

using namespace std;

typedef vector<char> TableRow;
typedef vector<TableRow> Table;

struct Board {
  Table sData, dData;
  int px, py;

  Board(string b) {
    vector<string> data;
    boost::split(data, b, boost::is_any_of("\n"));

    size_t width = 0;
    for (auto &row: data)
      width = max(width, row.size());

    map<char,char> maps = {{' ',' '}, {'.','.'}, {'@',' '},
                           {'#','#'}, {'$',' '}},
                   mapd = {{' ',' '}, {'.',' '}, {'@','@'},
                           {'#',' '}, {'$','*'}};

    for (size_t r = 0; r < data.size(); r++) {
      TableRow sTemp, dTemp;
      for (size_t c = 0; c < width; c++) {
        char ch = c < data[r].size() ? data[r][c] : ' ';
        sTemp.push_back(maps[ch]);
        dTemp.push_back(mapd[ch]);
        if (ch == '@') {
          px = c;
          py = r;
        }
      }
      sData.push_back(sTemp);
      dData.push_back(dTemp);
    }
  }

  bool move(int x, int y, int dx, int dy, Table &data) {
    if (sData[y+dy][x+dx] == '#' || data[y+dy][x+dx] != ' ')
      return false;

    data[y][x] = ' ';
    data[y+dy][x+dx] = '@';
    return true;
  }

  bool push(int x, int y, int dx, int dy, Table &data) {
    if (sData[y+2*dy][x+2*dx] == '#' || data[y+2*dy][x+2*dx] != ' ')
      return false;

    data[y][x] = ' ';
    data[y+dy][x+dx] = '@';
    data[y+2*dy][x+2*dx] = '*';
    return true;
  }

  bool isSolved(const Table &data) {
    for (size_t r = 0; r < data.size(); r++)
      for (size_t c = 0; c < data[r].size(); c++)
        if ((sData[r][c] == '.') != (data[r][c] == '*'))
          return false;
    return true;
  }

  string solve() {
    boost::unordered_set<Table, boost::hash<Table>> visited;
    visited.insert(dData);

    queue<tuple<Table, string, int, int>> open;
    open.push(make_tuple(dData, "", px, py));

    vector<tuple<int, int, char, char>> dirs = {
        make_tuple( 0, -1, 'u', 'U'),
        make_tuple( 1,  0, 'r', 'R'),
        make_tuple( 0,  1, 'd', 'D'),
        make_tuple(-1,  0, 'l', 'L')
    };

    while (open.size() > 0) {
      Table temp, cur = get<0>(open.front());
      string cSol = get<1>(open.front());
      int x = get<2>(open.front());
      int y = get<3>(open.front());
      open.pop();

      for (int i = 0; i < 4; ++i) {
        temp = cur;
        int dx = get<0>(dirs[i]);
        int dy = get<1>(dirs[i]);

        if (temp[y+dy][x+dx] == '*') {
          if (push(x, y, dx, dy, temp) &&
              visited.find(temp) == visited.end()) {
            if (isSolved(temp))
              return cSol + get<3>(dirs[i]);
            open.push(make_tuple(temp, cSol + get<3>(dirs[i]),
                                 x+dx, y+dy));
            visited.insert(temp);
          }
        } else if (move(x, y, dx, dy, temp) &&
                   visited.find(temp) == visited.end()) {
          if (isSolved(temp))
            return cSol + get<2>(dirs[i]);
          open.push(make_tuple(temp, cSol + get<2>(dirs[i]),
                               x+dx, y+dy));
          visited.insert(temp);
        }
      }
    }

    return "No solution";
  }
};

int main() {
  string level = "#######\n"
                 "#     #\n"
                 "#     #\n"
                 "#. #  #\n"
                 "#. $$ #\n"
                 "#.$$  #\n"
                 "#.#  @#\n"
                 "#######";

  cout << level << endl << endl;
  Board board(level);
  cout << board.solve() << endl;
  return 0;
}
```


## C#

```c#
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace SokobanSolver
{
    public class SokobanSolver
    {
        private class Board
        {
            public string Cur { get; internal set; }
            public string Sol { get; internal set; }
            public int X { get; internal set; }
            public int Y { get; internal set; }

            public Board(string cur, string sol, int x, int y)
            {
                Cur = cur;
                Sol = sol;
                X = x;
                Y = y;
            }
        }

        private string destBoard, currBoard;
        private int playerX, playerY, nCols;

        SokobanSolver(string[] board)
        {
            nCols = board[0].Length;
            StringBuilder destBuf = new StringBuilder();
            StringBuilder currBuf = new StringBuilder();

            for (int r = 0; r < board.Length; r++)
            {
                for (int c = 0; c < nCols; c++)
                {

                    char ch = board[r][c];

                    destBuf.Append(ch != '$' && ch != '@' ? ch : ' ');
                    currBuf.Append(ch != '.' ? ch : ' ');

                    if (ch == '@')
                    {
                        this.playerX = c;
                        this.playerY = r;
                    }
                }
            }
            destBoard = destBuf.ToString();
            currBoard = currBuf.ToString();
        }

        private string Move(int x, int y, int dx, int dy, string trialBoard)
        {

            int newPlayerPos = (y + dy) * nCols + x + dx;

            if (trialBoard[newPlayerPos] != ' ')
                return null;

            char[] trial = trialBoard.ToCharArray();
            trial[y * nCols + x] = ' ';
            trial[newPlayerPos] = '@';

            return new string(trial);
        }

        private string Push(int x, int y, int dx, int dy, string trialBoard)
        {

            int newBoxPos = (y + 2 * dy) * nCols + x + 2 * dx;

            if (trialBoard[newBoxPos] != ' ')
                return null;

            char[] trial = trialBoard.ToCharArray();
            trial[y * nCols + x] = ' ';
            trial[(y + dy) * nCols + x + dx] = '@';
            trial[newBoxPos] = '$';

            return new string(trial);
        }

        private bool IsSolved(string trialBoard)
        {
            for (int i = 0; i < trialBoard.Length; i++)
                if ((destBoard[i] == '.')
                        != (trialBoard[i] == '$'))
                    return false;
            return true;
        }

        private string Solve()
        {
            char[,] dirLabels = { { 'u', 'U' }, { 'r', 'R' }, { 'd', 'D' }, { 'l', 'L' } };
            int[,] dirs = { { 0, -1 }, { 1, 0 }, { 0, 1 }, { -1, 0 } };
            ISet<string> history = new HashSet<string>();
            LinkedList<Board> open = new LinkedList<Board>();

            history.Add(currBoard);
            open.AddLast(new Board(currBoard, string.Empty, playerX, playerY));

            while (!open.Count.Equals(0))
            {
                Board item = open.First();
                open.RemoveFirst();
                string cur = item.Cur;
                string sol = item.Sol;
                int x = item.X;
                int y = item.Y;

                for (int i = 0; i < dirs.GetLength(0); i++)
                {
                    string trial = cur;
                    int dx = dirs[i, 0];
                    int dy = dirs[i, 1];

                    // are we standing next to a box ?
                    if (trial[(y + dy) * nCols + x + dx] == '$')
                    {
                        // can we push it ?
                        if ((trial = Push(x, y, dx, dy, trial)) != null)
                        {
                            // or did we already try this one ?
                            if (!history.Contains(trial))
                            {

                                string newSol = sol + dirLabels[i, 1];

                                if (IsSolved(trial))
                                    return newSol;

                                open.AddLast(new Board(trial, newSol, x + dx, y + dy));
                                history.Add(trial);
                            }
                        }
                        // otherwise try changing position
                    }
                    else if ((trial = Move(x, y, dx, dy, trial)) != null)
                    {
                        if (!history.Contains(trial))
                        {
                            string newSol = sol + dirLabels[i, 0];
                            open.AddLast(new Board(trial, newSol, x + dx, y + dy));
                            history.Add(trial);
                        }
                    }
                }
            }
            return "No solution";
        }

        public static void Main(string[] a)
        {
            string level = "#######," +
                           "#     #," +
                           "#     #," +
                           "#. #  #," +
                           "#. $$ #," +
                           "#.$$  #," +
                           "#.#  @#," +
                           "#######";
            System.Console.WriteLine("Level:\n");
            foreach (string line in level.Split(','))
            {
                System.Console.WriteLine(line);
            }
            System.Console.WriteLine("\nSolution:\n");
            System.Console.WriteLine(new SokobanSolver(level.Split(',')).Solve());
        }
    }
}
```

Output:

```txt

Level:

#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######

Solution:

ulULLulDDurrrddlULrruLLrrUruLLLulD
```



## D


### Shorter Version

{{trans|C++}}
This version uses the queue defined in the Queue/Usage task.

```d
import std.string, std.typecons, std.exception, std.algorithm;
import queue_usage2; // No queue in Phobos 2.064.

const struct Board {
    private enum El { floor = ' ', wall = '#', goal = '.',
                      box = '$', player = '@', boxOnGoal='*' }
    private alias CTable = string;
    private immutable size_t ncols;
    private immutable CTable sData, dData;
    private immutable int playerx, playery;

    this(in string[] board) immutable pure nothrow @safe
    in {
        foreach (const row; board) {
            assert(row.length == board[0].length,
                   "Unequal board rows.");
            foreach (immutable c; row)
                assert(c.inPattern(" #.$@*"), "Not valid input");
        }
    } body {
        /*static*/ immutable sMap =
            [' ':' ', '.':'.', '@':' ', '#':'#', '$':' '];
        /*static*/ immutable dMap =
            [' ':' ', '.':' ', '@':'@', '#':' ', '$':'*'];
        ncols = board[0].length;

        int plx = 0, ply = 0;
        CTable sDataBuild, dDataBuild;

        foreach (immutable r, const row; board)
            foreach (immutable c, const ch; row) {
                sDataBuild ~= sMap[ch];
                dDataBuild ~= dMap[ch];
                if (ch == El.player) {
                    plx = c;
                    ply = r;
                }
            }

        this.sData = sDataBuild;
        this.dData = dDataBuild;
        this.playerx = plx;
        this.playery = ply;
    }

    private bool move(in int x, in int y, in int dx,
                      in int dy, ref CTable data)
    const pure nothrow /*@safe*/ {
        if (sData[(y + dy) * ncols + x + dx] == El.wall ||
            data[(y + dy) * ncols + x + dx] != El.floor)
            return false;

        auto data2 = data.dup;
        data2[y * ncols + x] = El.floor;
        data2[(y + dy) * ncols + x + dx] = El.player;
        data = data2.assumeUnique; // Not enforced.
        return true;
    }

    private bool push(in int x, in int y, in int dx,
                      in int dy, ref CTable data)
    const pure nothrow /*@safe*/ {
        if (sData[(y + 2 * dy) * ncols + x + 2 * dx] == El.wall ||
            data[(y + 2 * dy) * ncols + x + 2 * dx] != El.floor)
            return false;

        auto data2 = data.dup;
        data2[y * ncols + x] = El.floor;
        data2[(y + dy) * ncols + x + dx] = El.player;
        data2[(y + 2 * dy) * ncols + x + 2*dx] = El.boxOnGoal;
        data = data2.assumeUnique; // Not enforced.
        return true;
    }

    private bool isSolved(in CTable data)
    const pure nothrow @safe @nogc {
        foreach (immutable i, immutable d; data)
            if ((sData[i] == El.goal) != (d == El.boxOnGoal))
                return false;
        return true;
    }

    string solve() pure nothrow /*@safe*/ {
        bool[immutable CTable] visitedSet = [dData: true];

        alias Four = Tuple!(CTable, string, int, int);
        GrowableCircularQueue!Four open;
        open.push(Four(dData, "", playerx, playery));

        static immutable dirs = [tuple( 0, -1, 'u', 'U'),
                                 tuple( 1,  0, 'r', 'R'),
                                 tuple( 0,  1, 'd', 'D'),
                                 tuple(-1,  0, 'l', 'L')];

        while (!open.empty) {
            //immutable (cur, cSol, x, y) = open.pop;
            immutable item = open.pop;
            immutable cur = item[0];
            immutable cSol = item[1];
            immutable x = item[2];
            immutable y = item[3];

            foreach (immutable di; dirs) {
                CTable temp = cur;
                //immutable (dx, dy) = di[0 .. 2];
                immutable dx = di[0];
                immutable dy = di[1];

                if (temp[(y + dy) * ncols + x + dx] == El.boxOnGoal) {
                    if (push(x, y, dx, dy, temp) && temp !in visitedSet) {
                        if (isSolved(temp))
                            return cSol ~ di[3];
                        open.push(Four(temp, cSol ~ di[3], x + dx, y + dy));
                        visitedSet[temp] = true;
                    }
                } else if (move(x, y, dx, dy, temp) && temp !in visitedSet) {
                    if (isSolved(temp))
                        return cSol ~ di[2];
                    open.push(Four(temp, cSol ~ di[2], x + dx, y + dy));
                    visitedSet[temp] = true;
                }
            }
        }

        return "No solution";
    }
}

void main() {
    import std.stdio, core.memory;
    GC.disable; // Uses about twice the memory.

    immutable level =
"#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######";

    immutable b = immutable(Board)(level.splitLines);
    writeln(level, "\n\n", b.solve);
}
```

{{out}}

```txt
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######

ulULLulDDurrrddlULrruLLrrUruLLLulD
```

Run-time about 0.55 seconds with DMD compiler, 0.49 with LDC2 compiler.


### Faster Version

{{trans|C}}
This code is not idiomatic D, it retains most of the style of the C version.

```d
import core.stdc.stdio: printf, puts, fflush, stdout, putchar;
import core.stdc.stdlib: malloc, calloc, realloc, free, alloca, exit;

enum Cell : ubyte { space, wall, player, box }
alias CellIndex = ushort;
alias Thash = uint;


/// Board configuration is represented by an array of cell
/// indices of player and boxes.
struct State { // Variable length struct.
    Thash h;
    State* prev, next, qNext;
    CellIndex[0] c_;

    CellIndex get(in size_t i) inout pure nothrow @nogc {
        return c_.ptr[i];
    }

    void set(in size_t i, in CellIndex v) pure nothrow @nogc {
        c_.ptr[i] = v;
    }

    CellIndex[] slice(in size_t i, in size_t j) pure nothrow @nogc return {
        return c_.ptr[i .. j];
    }
}


__gshared Cell[] board;
__gshared bool[] goals, live;
__gshared size_t w, h, nBoxes, stateSize, blockSize = 32;
__gshared State* blockRoot, blockHead, nextLevel, done;
__gshared State*[] buckets;
__gshared Thash hashSize, fillLimit, filled;


State* newState(State* parent) nothrow @nogc {
    static State* nextOf(State *s) nothrow @nogc {
        return cast(State*)(cast(ubyte*)s + stateSize);
    }

    State* ptr;
    if (!blockHead) {
        blockSize *= 2;
        auto p = cast(State*)malloc(blockSize * stateSize);
        if (p == null)
            exit(1);

        p.next = blockRoot;
        blockRoot = p;
        ptr = cast(State*)(cast(ubyte*)p + stateSize * blockSize);
        p = blockHead = nextOf(p);
        for (auto q = nextOf(p); q < ptr; p = q, q = nextOf(q))
            p.next = q;
        p.next = null;
    }

    ptr = blockHead;
    blockHead = blockHead.next;
    ptr.prev = parent;
    ptr.h = 0;
    return ptr;
}


void unNewState(State* p) nothrow @nogc {
    p.next = blockHead;
    blockHead = p;
}


/// Mark up positions where a box definitely should not be.
void markLive(in size_t c) nothrow @nogc {
    immutable y = c / w;
    immutable x = c % w;
    if (live[c])
        return;

    live[c] = true;
    if (y > 1 && board[c - w] != Cell.wall &&
        board[c - w * 2] != Cell.wall)
        markLive(c - w);
    if (y < h - 2 && board[c + w] != Cell.wall &&
        board[c + w * 2] != Cell.wall)
        markLive(c + w);
    if (x > 1 && board[c - 1] != Cell.wall &&
        board[c - 2] != Cell.wall)
        markLive(c - 1);
    if (x < w - 2 && board[c + 1] != Cell.wall &&
        board[c + 2] != Cell.wall)
        markLive(c + 1);
}


State* parseBoard(in size_t y, in size_t x, in char* s) nothrow @nogc {
    static T[] myCalloc(T)(in size_t n) nothrow @nogc {
        auto ptr = cast(T*)calloc(n, T.sizeof);
        if (ptr == null)
            exit(1);
        return ptr[0 .. n];
    }

    w = x, h = y;
    board = myCalloc!Cell(w * h);
    goals = myCalloc!bool(w * h);
    live = myCalloc!bool(w * h);

    nBoxes = 0;
    for (int i = 0; s[i]; i++) {
        switch(s[i]) {
            case '#':
                board[i] = Cell.wall;
                continue;
            case '.', '+':
                goals[i] = true;
                goto case;
            case '@':
                continue;
            case '*':
                goals[i] = true;
                goto case;
            case '$':
                nBoxes++;
                continue;
            default:
                continue;
        }
    }

    enum int intSize = int.sizeof;
    stateSize = (State.sizeof +
                  (1 + nBoxes) * CellIndex.sizeof +
                  intSize - 1)
                 / intSize * intSize;

    auto state = null.newState;

    for (CellIndex i = 0, j = 0; i < w * h; i++) {
        if (goals[i])
            i.markLive;
        if (s[i] == '$' || s[i] == '*')
            state.set(++j, i);
        else if (s[i] == '@' || s[i] == '+')
            state.set(0, i);
    }

    return state;
}


/// K&R hash function.
void hash(State* s, in size_t nBoxes) pure nothrow @nogc {
    if (!s.h) {
        Thash ha = 0;
        foreach (immutable i; 0 .. nBoxes + 1)
            ha = s.get(i) + 31 * ha;
        s.h = ha;
    }
}


void extendTable() nothrow @nogc {
    int oldSize = hashSize;

    if (!oldSize) {
        hashSize = 1024;
        filled = 0;
        fillLimit = hashSize * 3 / 4; // 0.75 load factor.
    } else {
        hashSize *= 2;
        fillLimit *= 2;
    }

    auto ptr = cast(State**)realloc(buckets.ptr,
                                    (State*).sizeof * hashSize);
    if (ptr == null)
        exit(6);
    buckets = ptr[0 .. hashSize];
    buckets[oldSize .. hashSize] = null;

    immutable Thash bits = hashSize - 1;
    foreach (immutable i; 0 .. oldSize) {
        auto head = buckets[i];
        buckets[i] = null;
        while (head) {
            auto next = head.next;
            immutable j = head.h & bits;
            head.next = buckets[j];
            buckets[j] = head;
            head = next;
        }
    }
}


State* lookup(State *s) nothrow @nogc {
    s.hash(nBoxes);
    auto f = buckets[s.h & (hashSize - 1)];
    for (; f; f = f.next) {
        if (s.slice(0, nBoxes + 1) == f.slice(0, nBoxes + 1))
            break;
    }

    return f;
}


bool addToTable(State* s) nothrow @nogc {
    if (s.lookup) {
        s.unNewState;
        return false;
    }

    if (filled++ >= fillLimit)
        extendTable;

    immutable Thash i = s.h & (hashSize - 1);

    s.next = buckets[i];
    buckets[i] = s;
    return true;
}


bool success(in State* s) nothrow @nogc {
    foreach (immutable i; 1 .. nBoxes + 1)
        if (!goals[s.get(i)])
            return false;
    return true;
}


State* moveMe(State* s, in int dy, in int dx) nothrow @nogc {
    immutable int y = s.get(0) / w;
    immutable int x = s.get(0) % w;
    immutable int y1 = y + dy;
    immutable int x1 = x + dx;
    immutable int c1 = y1 * w + x1;

    if (y1 < 0 || y1 > h || x1 < 0 || x1 > w || board[c1] == Cell.wall)
        return null;

    int atBox = 0;
    foreach (immutable i; 1 .. nBoxes + 1)
        if (s.get(i) == c1) {
            atBox = i;
            break;
        }

    int c2;
    if (atBox) {
        c2 = c1 + dy * w + dx;
        if (board[c2] == Cell.wall || !live[c2])
            return null;
        foreach (immutable i; 1 .. nBoxes + 1)
            if (s.get(i) == c2)
                return null;
    }

    auto n = s.newState;
    n.slice(1, nBoxes + 1)[] = s.slice(1, nBoxes + 1);

    n.set(0, cast(CellIndex)c1);

    if (atBox)
        n.set(atBox, cast(CellIndex)c2);

    // Bubble sort.
    for (size_t i = nBoxes; --i; ) {
        CellIndex t = 0;
        foreach (immutable j; 1 .. i) {
            if (n.get(j) > n.get(j + 1)) {
                t = n.get(j);
                n.set(j, n.get(j + 1));
                n.set(j + 1, t);
            }
        }
        if (!t)
            break;
    }

    return n;
}


bool queueMove(State *s) nothrow @nogc {
    if (!s || !s.addToTable)
        return false;

    if (s.success) {
        "\nSuccess!".puts;
        done = s;
        return true;
    }

    s.qNext = nextLevel;
    nextLevel = s;
    return false;
}


bool doMove(State* s) nothrow @nogc {
    return s.moveMe( 1,  0).queueMove ||
           s.moveMe(-1,  0).queueMove ||
           s.moveMe( 0,  1).queueMove ||
           s.moveMe( 0, -1).queueMove;
}


void showBoard(in State* s) nothrow @nogc {
    static immutable glyphs1 = " #@$", glyphs2 = ".#@$";

    auto ptr = cast(ubyte*)alloca(w * h * ubyte.sizeof);
    if (ptr == null)
        exit(5);
    auto b = ptr[0 .. w * h];
    b[] = cast(typeof(b))board[];

    b[s.get(0)] = Cell.player;
    foreach (immutable i; 1 .. nBoxes + 1)
        b[s.get(i)] = Cell.box;

    foreach (immutable i, immutable bi; b) {
        putchar((goals[i] ? glyphs2 : glyphs1)[bi]);
        if (!((1 + i) % w))
            '\n'.putchar;
    }
}


void showMoves(in State* s) nothrow @nogc {
    if (s.prev)
        s.prev.showMoves;
    "\n".printf;
    s.showBoard;
}

int main() nothrow @nogc {
    // Workaround for @nogc.
    alias ctEval(alias expr) = expr;

    enum uint problem = 0;

    static if (problem == 0) {
        auto s = parseBoard(8, 7, ctEval!(
        "#######"~
        "#     #"~
        "#     #"~
        "#. #  #"~
        "#. $$ #"~
        "#.$$  #"~
        "#.#  @#"~
        "#######"));

    } else static if (problem == 1) {
        auto s = parseBoard(5, 13, ctEval!(
        "#############"~
        "#  #        #"~
        "# $$$$$$$  @#"~
        "#.......    #"
        "#############"));

    } else static if (problem == 2) {
        auto s = parseBoard(11, 19, ctEval!(
        "    #####          "~
        "    #   #          "~
        "    #   #          "~
        "  ### #$##         "~
        "  #      #         "~
        "### #$## #   ######"~
        "#   # ## #####   .#"~
        "# $   $         ..#"~
        "##### ### #@##   .#"~
        "    #     #########"~
        "    #######        "));
    } else {
        asset(0, "Not present problem.");
    }

    s.showBoard;
    extendTable;
    s.queueMove;
    for (int i = 0; !done; i++) {
        printf("depth %d\r", i);
        stdout.fflush;

        auto head = nextLevel;
        for (nextLevel = null; head && !done; head = head.qNext)
            head.doMove;

        if (!nextLevel) {
            "No solution?".puts;
            return 1;
        }
    }

    done.showMoves;

    version (none) { // Free all allocated memory.
        buckets.ptr.free;
        board.ptr.free;
        goals.ptr.free;
        live.ptr.free;

        while (blockRoot) {
            auto tmp = blockRoot.next;
            blockRoot.free;
            blockRoot = tmp;
        }
    }

    return 0;
}
```



## Elixir

{{works with|Elixir|1.3}}
{{trans|Ruby}}

```elixir
defmodule Sokoban do
  defp setup(level) do
    {leng, board} = normalize(level)
    {player, goal} = check_position(board)
    board = replace(board, [{".", " "}, {"+", " "}, {"*", "$"}])
    lurd = [{-1, "l", "L"}, {-leng, "u", "U"}, {1, "r", "R"}, {leng, "d", "D"}]
    dirs = [-1, -leng, 1, leng]
    dead_zone = set_dead_zone(board, goal, dirs)
    {board, player, goal, lurd, dead_zone}
  end

  defp normalize(level) do
    board = String.split(level, "\n", trim: true)
            |> Enum.map(&String.trim_trailing &1)
    leng  = Enum.map(board, &String.length &1) |> Enum.max
    board = Enum.map(board, &String.pad_trailing(&1, leng)) |> Enum.join
    {leng, board}
  end

  defp check_position(board) do
    board = String.codepoints(board)
    player = Enum.find_index(board, fn c -> c in ["@", "+"] end)
    goal = Enum.with_index(board)
           |> Enum.filter_map(fn {c,_} -> c in [".", "+", "*"] end, fn {_,i} -> i end)
    {player, goal}
  end

  defp set_dead_zone(board, goal, dirs) do
    wall = String.replace(board, ~r/[^#]/, " ")
           |> String.codepoints
           |> Enum.with_index
           |> Enum.into(Map.new, fn {c,i} -> {i,c} end)
    corner = search_corner(wall, goal, dirs)
    set_dead_zone(wall, dirs, goal, corner, corner)
  end

  defp set_dead_zone(wall, dirs, goal, corner, dead) do
    dead2 = Enum.reduce(corner, dead, fn pos,acc ->
              Enum.reduce(dirs, acc, fn dir,acc2 ->
                if wall[pos+dir] == "#", do: acc2,
                    else: acc2 ++ check_side(wall, dirs, pos+dir, dir, goal, dead, [])
              end)
            end)
    if dead == dead2, do: :lists.usort(dead),
                    else: set_dead_zone(wall, dirs, goal, corner, dead2)
  end

  defp replace(string, replacement) do
    Enum.reduce(replacement, string, fn {a,b},str ->
      String.replace(str, a, b)
    end)
  end

  defp search_corner(wall, goal, dirs) do
    Enum.reduce(wall, [], fn {i,c},corner ->
      if c == "#" or i in goal do
        corner
      else
        case count_wall(wall, i, dirs) do
          2 -> if wall[i-1] != wall[i+1], do: [i | corner], else: corner
          3 -> [i | corner]
          _ -> corner
        end
      end
    end)
  end

  defp check_side(wall, dirs, pos, dir, goal, dead, acc) do
    if wall[pos] == "#" or
      count_wall(wall, pos, dirs) == 0 or
      pos in goal do
      []
    else
      if pos in dead, do: acc, else: check_side(wall, dirs, pos+dir, dir, goal, dead, [pos|acc])
    end
  end

  defp count_wall(wall, pos, dirs) do
    Enum.count(dirs, fn dir -> wall[pos + dir] == "#" end)
  end

  defp push_box(board, pos, dir, route, goal, dead_zone) do
    pos2dir = pos + 2 * dir
    if String.at(board, pos2dir) == " " and not pos2dir in dead_zone do
      board2 = board |> replace_at(pos,     " ")
                     |> replace_at(pos+dir, "@")
                     |> replace_at(pos2dir, "$")
      unless visited?(board2) do
        if solved?(board2, goal) do
          IO.puts route
          exit(:normal)
        else
          queue_in({board2, pos+dir, route})
        end
      end
    end
  end

  defp move_player(board, pos, dir) do
    board |> replace_at(pos, " ") |> replace_at(pos+dir, "@")
  end

  defp replace_at(str, pos, c) do
    {left, right} = String.split_at(str, pos)
    {_, right} = String.split_at(right, 1)
    left <> c <> right
    # String.slice(str, 0, pos) <> c <> String.slice(str, pos+1..-1)
  end

  defp solved?(board, goal) do
    Enum.all?(goal, fn g -> String.at(board, g) == "$" end)
  end

  @pattern :sokoban_pattern_set
  @queue   :sokoban_queue

  defp start_link do
    Agent.start_link(fn -> MapSet.new end, name: @pattern)
    Agent.start_link(fn -> :queue.new end, name: @queue)
  end

  defp visited?(board) do
    Agent.get_and_update(@pattern, fn set ->
      {board in set, MapSet.put(set, board)}
    end)
  end

  defp queue_in(data) do
    Agent.update(@queue, fn queue -> :queue.in(data, queue) end)
  end

  defp queue_out do
    Agent.get_and_update(@queue, fn q ->
      case :queue.out(q) do
        {{:value, data}, queue} -> {data, queue}
        x -> x
      end
    end)
  end

  def solve(level) do
    {board, player, goal, lurd, dead_zone} = setup(level)
    start_link
    visited?(board)
    queue_in({board, player, ""})
    solve(goal, lurd, dead_zone)
  end

  defp solve(goal, lurd, dead_zone) do
    case queue_out do
      {board, pos, route} ->
        Enum.each(lurd, fn {dir,move,push} ->
          case String.at(board, pos+dir) do
            "$" -> push_box(board, pos, dir, route<>push, goal, dead_zone)
            " " -> board2 = move_player(board, pos, dir)
                   unless visited?(board2) do
                     queue_in({board2, pos+dir, route<>move})
                   end
            _ -> :not_move    # wall
          end
        end)
      _ ->
        IO.puts "No solution"
        exit(:normal)
    end
    solve(goal, lurd, dead_zone)
  end
end

level = """
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######
"""
IO.puts level
Sokoban.solve(level)
```


{{out}}

```txt

#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######

luULLulDDurrrddlULrruLLrrUruLLLulD

```



## Go

{{trans|C++}}
Well, it started as a C++ translation, but turned out different.  It's still the breadth-first set-based algorithm, but I dropped the sdata/ddata optimization and just maintained a single string as the board representation.  Also dropped the code to handle non-rectangular boards, and probably some other stuff too.

```go
package main

import (
    "fmt"
    "strings"
)

func main() {
    level := `
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######`
    fmt.Printf("level:%s\n", level)
    fmt.Printf("solution:\n%s\n", solve(level))
}

func solve(board string) string {
    buffer = make([]byte, len(board))
    width := strings.Index(board[1:], "\n") + 1
    dirs := []struct {
        move, push string
        dPos       int
    }{
        {"u", "U", -width},
        {"r", "R", 1},
        {"d", "D", width},
        {"l", "L", -1},
    }
    visited := map[string]bool{board: true}
    open := []state{state{board, "", strings.Index(board, "@")}}
    for len(open) > 0 {
        s1 := &open[0]
        open = open[1:]
        for _, dir := range dirs {
            var newBoard, newSol string
            newPos := s1.pos + dir.dPos
            switch s1.board[newPos] {
            case '$', '*':
                newBoard = s1.push(dir.dPos)
                if newBoard == "" || visited[newBoard] {
                    continue
                }
                newSol = s1.cSol + dir.push
                if strings.IndexAny(newBoard, ".+") < 0 {
                    return newSol
                }
            case ' ', '.':
                newBoard = s1.move(dir.dPos)
                if visited[newBoard] {
                    continue
                }
                newSol = s1.cSol + dir.move
            default:
                continue
            }
            open = append(open, state{newBoard, newSol, newPos})
            visited[newBoard] = true
        }
    }
    return "No solution"
}

type state struct {
    board string
    cSol  string
    pos   int
}

var buffer []byte

func (s *state) move(dPos int) string {
    copy(buffer, s.board)
    if buffer[s.pos] == '@' {
        buffer[s.pos] = ' '
    } else {
        buffer[s.pos] = '.'
    }
    newPos := s.pos + dPos
    if buffer[newPos] == ' ' {
        buffer[newPos] = '@'
    } else {
        buffer[newPos] = '+'
    }
    return string(buffer)
}

func (s *state) push(dPos int) string {
    newPos := s.pos + dPos
    boxPos := newPos + dPos
    switch s.board[boxPos] {
    case ' ', '.':
    default:
        return ""
    }
    copy(buffer, s.board)
    if buffer[s.pos] == '@' {
        buffer[s.pos] = ' '
    } else {
        buffer[s.pos] = '.'
    }
    if buffer[newPos] == '$' {
        buffer[newPos] = '@'
    } else {
        buffer[newPos] = '+'
    }
    if buffer[boxPos] == ' ' {
        buffer[boxPos] = '$'
    } else {
        buffer[boxPos] = '*'
    }
    return string(buffer)
}
```

{{out}}

```txt

level:
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######
solution:
ulULLulDDurrrddlULrruLLrrUruLLLulD

```



## Haskell


```Haskell
import Control.Monad (liftM)
import Data.Array
import Data.List (transpose)
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Prelude hiding (Left, Right)

data Field = Space | Wall | Goal
           deriving (Eq)

data Action = Up | Down | Left | Right | PushUp | PushDown | PushLeft | PushRight

instance Show Action where
  show Up        = "u"
  show Down      = "d"
  show Left      = "l"
  show Right     = "r"
  show PushUp    = "U"
  show PushDown  = "D"
  show PushLeft  = "L"
  show PushRight = "R"

type Index = (Int, Int)
type FieldArray = Array Index Field
type BoxArray = Array Index Bool
type PlayerPos = Index
type GameState = (BoxArray, PlayerPos)
type Game = (FieldArray, GameState)

toField :: Char -> Field
toField '#' = Wall
toField ' ' = Space
toField '@' = Space
toField '$' = Space
toField '.' = Goal
toField '+' = Goal
toField '*' = Goal

toPush :: Action -> Action
toPush Up    = PushUp
toPush Down  = PushDown
toPush Left  = PushLeft
toPush Right = PushRight
toPush n     = n

toMove :: Action -> Index
toMove PushUp    = ( 0, -1)
toMove PushDown  = ( 0,  1)
toMove PushLeft  = (-1,  0)
toMove PushRight = ( 1,  0)
toMove n = toMove $ toPush n

-- Parse the string-based game board into an easier-to-use format.
-- Assume that the board is valid (rectangular, one player, etc).
parseGame :: [String] -> Game
parseGame fieldStrs = (field, (boxes, player))
  where
    width     = length $ head fieldStrs
    height    = length fieldStrs
    bound     = ((0, 0), (width - 1, height - 1))
    flatField = concat $ transpose fieldStrs
    charField = listArray bound flatField
    field     = fmap toField charField
    boxes     = fmap (`elem` "$*") charField
    player    = fst $ head $ filter (flip elem "@+" . snd) $ assocs charField

add :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add (a, b) (x, y) = (a + x, b + y)

-- Attempt to perform an action, returning the updated game and adjusted
-- action if the action was legal.
tryAction :: Game -> Action -> Maybe (Game, Action)
tryAction (field, (boxes, player)) action
  | field ! vec == Wall = Nothing
  | boxes ! vec =
     if boxes ! vecB || field ! vecB == Wall
     then Nothing
     else Just ((field, (boxes // [(vec, False), (vecB, True)], vec)),
               toPush action)
  | otherwise = Just ((field, (boxes, vec)), action)
  where
    actionVec = toMove action
    vec       = player `add` actionVec
    vecB      = vec `add` actionVec

-- Search the game for a solution.
solveGame :: Game -> Maybe [Action]
solveGame (field, initState) =
  liftM reverse $ bfs (Seq.singleton (initState, [])) (Set.singleton initState)
  where
    goals           = map fst $ filter ((== Goal) . snd) $ assocs field
    isSolved st     = all (st !) goals
    possibleActions = [Up, Down, Left, Right]

    -- Breadth First Search of the game tree.
    bfs :: Seq.Seq (GameState, [Action]) -> Set.Set GameState -> Maybe [Action]
    bfs queue visited =
      case Seq.viewl queue of
        Seq.EmptyL -> Nothing
        (game@(boxes, _), actions) Seq.:< queueB ->
          if isSolved boxes
          then Just actions
          else
            let newMoves = filter (flip Set.notMember visited . fst) $
                           map (\((_, g), a) -> (g, a)) $
                           mapMaybe (tryAction (field, game)) possibleActions
                visitedB = foldl (flip Set.insert) visited $
                           map fst newMoves
                queueC   = foldl (Seq.|>) queueB $
                           map (\(g, a) -> (g, a:actions)) newMoves
            in bfs queueC visitedB

exampleA :: [String]
exampleA =
  ["#######"
  ,"#     #"
  ,"#     #"
  ,"#. #  #"
  ,"#. $$ #"
  ,"#.$$  #"
  ,"#.#  @#"
  ,"#######"]

main :: IO ()
main =
  case solveGame $ parseGame exampleA of
    Nothing       -> putStrLn "Unsolvable"
    Just solution -> do
      mapM_ putStrLn exampleA
      putStrLn ""
      putStrLn $ concatMap show solution
```

{{out}}

```txt
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######

ulULLulDDurrrddlULrruLLrrUruLLLulD
```



## Java

Translation of [[Sokoban#C++|C++]] via [[Sokoban#D|D]]
{{works with|Java|7}}

```java
import java.util.*;

public class Sokoban {
    String destBoard, currBoard;
    int playerX, playerY, nCols;

    Sokoban(String[] board) {
        nCols = board[0].length();
        StringBuilder destBuf = new StringBuilder();
        StringBuilder currBuf = new StringBuilder();

        for (int r = 0; r < board.length; r++) {
            for (int c = 0; c < nCols; c++) {

                char ch = board[r].charAt(c);

                destBuf.append(ch != '$' && ch != '@' ? ch : ' ');
                currBuf.append(ch != '.' ? ch : ' ');

                if (ch == '@') {
                    this.playerX = c;
                    this.playerY = r;
                }
            }
        }
        destBoard = destBuf.toString();
        currBoard = currBuf.toString();
    }

    String move(int x, int y, int dx, int dy, String trialBoard) {

        int newPlayerPos = (y + dy) * nCols + x + dx;

        if (trialBoard.charAt(newPlayerPos) != ' ')
            return null;

        char[] trial = trialBoard.toCharArray();
        trial[y * nCols + x] = ' ';
        trial[newPlayerPos] = '@';

        return new String(trial);
    }

    String push(int x, int y, int dx, int dy, String trialBoard) {

        int newBoxPos = (y + 2 * dy) * nCols + x + 2 * dx;

        if (trialBoard.charAt(newBoxPos) != ' ')
            return null;

        char[] trial = trialBoard.toCharArray();
        trial[y * nCols + x] = ' ';
        trial[(y + dy) * nCols + x + dx] = '@';
        trial[newBoxPos] = '$';

        return new String(trial);
    }

    boolean isSolved(String trialBoard) {
        for (int i = 0; i < trialBoard.length(); i++)
            if ((destBoard.charAt(i) == '.')
                    != (trialBoard.charAt(i) == '$'))
                return false;
        return true;
    }

    String solve() {
        class Board {
            String cur, sol;
            int x, y;

            Board(String s1, String s2, int px, int py) {
                cur = s1;
                sol = s2;
                x = px;
                y = py;
            }
        }
        char[][] dirLabels = {{'u', 'U'}, {'r', 'R'}, {'d', 'D'}, {'l', 'L'}};
        int[][] dirs = {{0, -1}, {1, 0}, {0, 1}, {-1, 0}};

        Set<String> history = new HashSet<>();
        LinkedList<Board> open = new LinkedList<>();

        history.add(currBoard);
        open.add(new Board(currBoard, "", playerX, playerY));

        while (!open.isEmpty()) {
            Board item = open.poll();
            String cur = item.cur;
            String sol = item.sol;
            int x = item.x;
            int y = item.y;

            for (int i = 0; i < dirs.length; i++) {
                String trial = cur;
                int dx = dirs[i][0];
                int dy = dirs[i][1];

                // are we standing next to a box ?
                if (trial.charAt((y + dy) * nCols + x + dx) == '$') {

                    // can we push it ?
                    if ((trial = push(x, y, dx, dy, trial)) != null) {

                        // or did we already try this one ?
                        if (!history.contains(trial)) {

                            String newSol = sol + dirLabels[i][1];

                            if (isSolved(trial))
                                return newSol;

                            open.add(new Board(trial, newSol, x + dx, y + dy));
                            history.add(trial);
                        }
                    }

                // otherwise try changing position
                } else if ((trial = move(x, y, dx, dy, trial)) != null) {

                    if (!history.contains(trial)) {
                        String newSol = sol + dirLabels[i][0];
                        open.add(new Board(trial, newSol, x + dx, y + dy));
                        history.add(trial);
                    }
                }
            }
        }
        return "No solution";
    }

    public static void main(String[] a) {
        String level = "#######,#     #,#     #,#. #  #,#. $$ #,"
                + "#.$$  #,#.#  @#,#######";
        System.out.println(new Sokoban(level.split(",")).solve());
    }
}
```



```txt
ulULLulDDurrrddlULrruLLrrUruLLLulD
```



## Julia

{{trans|Go}}

```julia
struct BoardState
    board::String
    csol::String
    position::Int
end

function move(s::BoardState, dpos)
    buffer = Vector{UInt8}(deepcopy(s.board))
    if s.board[s.position] == '@'
        buffer[s.position] = ' '
    else
        buffer[s.position] = '.'
    end
    newpos = s.position + dpos
    if s.board[newpos] == ' '
        buffer[newpos] = '@'
    else
        buffer[newpos] = '+'
    end
    String(buffer)
end

function push(s::BoardState, dpos)
    newpos = s.position + dpos
    boxpos = newpos + dpos
    if s.board[boxpos] != ' ' && s.board[boxpos] != '.'
        return ""
    end
    buffer = Vector{UInt8}(deepcopy(s.board))
    if s.board[s.position] == '@'
        buffer[s.position] = ' '
    else
        buffer[s.position] = '.'
    end
    if s.board[newpos] == '$'
        buffer[newpos] = '@'
    else
        buffer[newpos] = '+'
    end
    if s.board[boxpos] == ' '
        buffer[boxpos] = '$'
    else
        buffer[boxpos] = '*'
    end
    String(buffer)
end

function solve(board)
    width = findfirst("\n", board[2:end])[1] + 1
    dopt = (u = -width, l = -1, d = width, r = 1)
    visited = Dict(board => true)
    open::Vector{BoardState} = [BoardState(board, "", findfirst("@", board)[1])]
    while length(open) > 0
        s1 = open[1]
        open = open[2:end]
        for dir in keys(dopt)
            newpos = s1.position + dopt[dir]
            x = s1.board[newpos]
            if x == '$' || x == '*'
                newboard = push(s1, dopt[dir])
                if newboard == "" || haskey(visited, newboard)
                    continue
                end
                newsol = s1.csol * uppercase(string(dir))
                if findfirst(r"[\.\+]", newboard) ==  nothing
                    return newsol
                end
            elseif x == ' ' || x == '.'
                newboard = move(s1, dopt[dir])
                if haskey(visited, newboard)
                    continue
                end
                newsol = s1.csol * string(dir)
            else
                continue
            end
            open = push!(open, BoardState(newboard, newsol, newpos))
            visited[newboard] = true
        end
    end
    "No solution" # we should only get here if no solution to the sokoban
end

const testlevel = strip(raw"""
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######""")
println("For sokoban level:\n$testlevel\n...solution is :\n$(solve(testlevel))")

```
{{output}}
```txt

 For sokoban level:
 #######
 #     #
 #     #
 #. #  #
 #. $$ #
 #.$$  #
 #.#  @#
 #######
 ...solution is :
 ulULLulDDurrrddlULrruLrUruLLLulD

```



## Kotlin

{{trans|Java}}

```scala
// version 1.2.0

import java.util.LinkedList

class Sokoban(board: List<String>) {
    val destBoard: String
    val currBoard: String
    val nCols = board[0].length
    var playerX = 0
    var playerY = 0

    init {
        val destBuf = StringBuilder()
        val currBuf = StringBuilder()
        for (r in 0 until board.size) {
            for (c in 0 until nCols) {
                val ch = board[r][c]
                destBuf.append(if (ch != '$' && ch != '@') ch else ' ')
                currBuf.append(if (ch != '.') ch else ' ')
                if (ch == '@') {
                    playerX = c
                    playerY = r
                }
            }
        }
        destBoard = destBuf.toString()
        currBoard = currBuf.toString()
    }

    fun move(x: Int, y: Int, dx: Int, dy: Int, trialBoard: String): String {
        val newPlayerPos = (y + dy) * nCols + x + dx
        if (trialBoard[newPlayerPos] != ' ') return ""
        val trial = trialBoard.toCharArray()
        trial[y * nCols + x] = ' '
        trial[newPlayerPos] = '@'
        return String(trial)
    }

    fun push(x: Int, y: Int, dx: Int, dy: Int, trialBoard: String): String {
        val newBoxPos = (y + 2 * dy) * nCols + x + 2 * dx
        if (trialBoard[newBoxPos] != ' ') return ""
        val trial = trialBoard.toCharArray()
        trial[y * nCols + x] = ' '
        trial[(y + dy) * nCols + x + dx] = '@'
        trial[newBoxPos] = '$'
        return String(trial)
    }

    fun isSolved(trialBoard: String): Boolean {
        for (i in 0 until trialBoard.length) {
            if ((destBoard[i] == '.') != (trialBoard[i] == '$')) return false
        }
        return true
    }

    fun solve(): String {
        data class Board(val cur: String, val sol: String, val x: Int, val y: Int)
        val dirLabels = listOf('u' to 'U', 'r' to 'R', 'd' to 'D', 'l' to 'L')
        val dirs = listOf(0 to -1, 1 to 0, 0 to 1, -1 to 0)
        val history = mutableSetOf<String>()
        history.add(currBoard)
        val open = LinkedList<Board>()
        open.add(Board(currBoard, "", playerX, playerY))

        while (!open.isEmpty()) {
            val (cur, sol, x, y) = open.poll()
            for (i in 0 until dirs.size) {
                var trial = cur
                val dx = dirs[i].first
                val dy = dirs[i].second

                // are we standing next to a box ?
                if (trial[(y + dy) * nCols + x + dx] == '$') {

                    // can we push it ?
                    trial = push(x, y, dx, dy, trial)
                    if (!trial.isEmpty()) {

                        // or did we already try this one ?
                        if (trial !in history) {
                            val newSol = sol + dirLabels[i].second
                            if (isSolved(trial)) return newSol
                            open.add(Board(trial, newSol, x + dx, y + dy))
                            history.add(trial)
                        }
                    }
                } // otherwise try changing position
                else {
                    trial = move(x, y, dx, dy, trial)
                    if (!trial.isEmpty() && trial !in history) {
                        val newSol = sol + dirLabels[i].first
                        open.add(Board(trial, newSol, x + dx, y + dy))
                        history.add(trial)
                    }
                }
            }
        }
        return "No solution"
    }
}

fun main(args: Array<String>) {
    val level = listOf(
        "#######",
        "#     #",
        "#     #",
        "#. #  #",
        "#. $$ #",
        "#.$$  #",
        "#.#  @#",
        "#######"
    )
    println(level.joinToString("\n"))
    println()
    println(Sokoban(level).solve())
}
```


{{out}}

```txt

#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######

ulULLulDDurrrddlULrruLLrrUruLLLulD

```



## OCaml

{{trans|Python}}
This uses a breadth-first move search, so will find a move-optimal solution.

```OCaml
type dir = U | D | L | R
type move_t = Move of dir | Push of dir

let letter = function
   | Push(U) -> 'U' | Push(D) -> 'D' | Push(L) -> 'L' | Push(R) -> 'R'
   | Move(U) -> 'u' | Move(D) -> 'd' | Move(L) -> 'l' | Move(R) -> 'r'

let cols = ref 0
let delta = function U -> -(!cols) | D -> !cols | L -> -1 | R -> 1

let store = Hashtbl.create 251
let mark t = Hashtbl.replace store t ()
let marked t = Hashtbl.mem store t

let show ml =
   List.iter (fun c -> print_char (letter c)) (List.rev ml); print_newline()

let gen_moves (x,boxes) bd =
   let empty i = bd.(i) = ' ' && not (List.mem i boxes) in
   let check l dir =
      let dx = delta dir in
      let x1 = x+dx in
      if List.mem x1 boxes then (
         if empty (x1+dx) then Push(dir) :: l else l
      ) else (
         if bd.(x1) = ' ' then Move(dir) :: l else l
      ) in
   (List.fold_left check [] [U; L; R; D])

let do_move (x,boxes) = function
   | Push(d) -> let dx = delta d in
      let x1 = x+dx in let x2 = x1+dx in
      let rec shift = function
         | [] -> failwith "shift"
         | h :: t -> if h = x1 then x2 :: t else h :: shift t in
      x1, List.fast_sort compare (shift boxes)
   | Move(d) -> (x+(delta d)), boxes

let init_pos bd =
   let p = ref 0 in
   let q = ref [] in
   let check i c =
      if c = '$' || c = '*' then q := i::!q
      else if c = '@' then p := i in (
   Array.iteri check bd;
   (!p, List.fast_sort compare !q);
   )

let final_box bd =
   let check (i,l) c = if c = '.' || c = '*' then (i+1,i::l) else (i+1,l) in
   List.fast_sort compare (snd (Array.fold_left check (0,[]) bd))

let array_of_input inp =
   let r = List.length inp and c = String.length (List.hd inp) in
   let a = Array.create (r*c) ' ' in (
   for i = 0 to pred r do
      let s = List.nth inp i in
      for j = 0 to pred c do a.(i*c+j) <- s.[j] done
   done;
   cols := c; a)

let solve b =
   let board = array_of_input b in
   let targets = final_box board in
   let solved pos = targets = snd pos in
   let clear = Array.map (function '#' -> '#' | _ -> ' ') in
   let bdc = clear board in
   let q = Queue.create () in
   let pos1 = init_pos board in
   begin
      mark pos1;
      Queue.add (pos1, []) q;
      while not (Queue.is_empty q) do
         let curr, mhist = Queue.pop q in
         let moves = gen_moves curr bdc in
         let check m =
            let next = do_move curr m in
            if not (marked next) then
            if solved next then (show (m::mhist); exit 0)
            else (mark next; Queue.add (next,m::mhist) q) in
         List.iter check moves
      done;
      print_endline "No solution"
   end;;

let level = ["#######";
             "#     #";
             "#     #";
             "#. #  #";
             "#. $$ #";
             "#.$$  #";
             "#.#  @#";
             "#######"] in
solve level
```

Output:

```txt
luULLulDDurrrddlULrruLLrrUruLLLulD
```



## Perl

This performs simultaneous breadth first searches, starting from the initial state
and various possible final states, and meeting somewhere in the middle.

On my laptop, which has a slow cpu and little memory, it can solve the basic puzzle
in about a second, and a slightly harder one in about 50 seconds.

A slightly more basic version of this code, doing a single breadth first search,
took twenty seconds for the basic puzzle, and was unable to solve the slightly harder
one before I lost patience with it (about half an hour).

The meet-in-the-middle search uses massively less memory, but obviously more lines
of code.  Due to the way I alternate between forward and rearward computation, it's
possible for the solution to be at most one step longer than the optimal one... but it
would still be a valid solution.  I could fix it, but at the cost of speed and memory.


```Perl
#!perl
use strict;
use warnings qw(FATAL all);
my @initial = split /\n/, <<'';
#############
#  #        #
# $$$$$$$  @#
#.......    #
#############

#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######

=for
space is an empty square
# is a wall
@ is the player
$ is a box
. is a goal
+ is the player on a goal
* is a box on a goal
=cut


my $cols = length($initial[0]);
my $initial = join '', @initial;
my $size = length($initial);
die unless $size == $cols * @initial;

sub WALL() { 1 }
sub PLAYER() { 2 }
sub BOX() { 4 }
sub GOAL() { 8 }

my %input = (
	' ' => 0, '#' => WALL, '@' => PLAYER, '$' => BOX,
	'.' => GOAL, '+' => PLAYER|GOAL, '*' => BOX|GOAL,
);
my %output = reverse(%input);

sub packed_initial {
	my $ret = '';
	vec( $ret, $_, 4 ) = $input{substr $initial, $_, 1}
		for( 0 .. $size-1 );
	$ret;
}

sub printable_board {
	my $board = shift;
	my @c = @output{map vec($board, $_, 4), 0 .. $size-1};
	my $ret = '';
	while( my @row = splice @c, 0, $cols ) {
		$ret .= join '', @row, "\n";
	}
	$ret;
}

my $packed = packed_initial();

my @udlr = qw(u d l r);
my @UDLR = qw(U D L R);
my @deltas = (-$cols, +$cols, -1, +1);

my %fseen;
INIT_FORWARD: {
	$initial =~ /(\@|\+)/ or die;
	use vars qw(@ftodo @fnext);
	@ftodo = (["", $packed,  $-[0]]);
	$fseen{$packed} = '';
}

my %rseen;
INIT_REVERSE: {
	my $goal = $packed;
	vec($goal, $ftodo[0][2], 4) -= PLAYER;
	my @u = grep { my $t = vec($goal, $_, 4); $t & GOAL and not $t & BOX } 0 .. $size-1;
	my @b = grep { my $t = vec($goal, $_, 4); $t & BOX and not $t & GOAL } 0 .. $size-1;
	die unless @u == @b;
	vec($goal, $_, 4) += BOX for @u;
	vec($goal, $_, 4) -= BOX for @b;
	use vars qw(@rtodo @rnext);
	FINAL_PLACE: for my $player (0 .. $size-1) {
		next if vec($goal, $player, 4);
		FIND_GOAL: {
			vec($goal, $player + $_, 4) & GOAL and last FIND_GOAL for @deltas;
			next FINAL_PLACE;
		}
		my $a_goal = $goal;
		vec($a_goal, $player, 4) += PLAYER;
		push @rtodo, ["", $a_goal, $player ];
		$rseen{$a_goal} = '';
		#print printable_board($a_goal);
	}
}

my $movelen = -1;
my ($solution);
MAIN: while( @ftodo and @rtodo ) {

	FORWARD: {
		my ($moves, $level, $player) = @{pop @ftodo};
		die unless vec($level, $player, 4) & PLAYER;

		for my $dir_num (0 .. 3) {
			my $delta = $deltas[$dir_num];
			my @loc = map $player + $delta * $_, 0 .. 2;
			my @val = map vec($level, $_, 4), @loc;

			next if $val[1] & WALL or ($val[1] & BOX and $val[2] & (BOX|WALL));

			my $new = $level;
			vec($new, $loc[0], 4) -= PLAYER;
			vec($new, $loc[1], 4) += PLAYER;
			my $nmoves;
			if( $val[1] & BOX ) {
				vec($new, $loc[1], 4) -= BOX;
				vec($new, $loc[2], 4) += BOX;
				$nmoves = $moves . $UDLR[$dir_num];
			} else {
				$nmoves = $moves . $udlr[$dir_num];
			}

			next if exists $fseen{$new};
			$fseen{$new} = $nmoves;

			push @fnext, [ $nmoves, $new, $loc[1] ];

			exists $rseen{$new} or next;
			#print(($val[1] & BOX) ? "Push $UDLR[$dir_num]\n" : "Fwalk $udlr[$dir_num]\n");
			$solution = $new;
			last MAIN;
		}

		last FORWARD if @ftodo;
		use vars qw(*ftodo *fnext);
		(*ftodo, *fnext) = (\@fnext, \@ftodo);
	} # end FORWARD

	BACKWARD: {
		my ($moves, $level, $player) = @{pop @rtodo};
		die "<$level>" unless vec($level, $player, 4) & PLAYER;

		for my $dir_num (0 .. 3) {
			my $delta = $deltas[$dir_num];
			# look behind and in front of the player.
			my @loc = map $player + $delta * $_, -1 .. 1;
			my @val = map vec($level, $_, 4), @loc;

			# unlike the forward solution, we cannot push boxes
			next if $val[0] & (WALL|BOX);
			my $new = $level;
			vec($new, $loc[0], 4) += PLAYER;
			vec($new, $loc[1], 4) -= PLAYER;
			# unlike the forward solution, if we have a box behind us
			# we can *either* pull it or not.  This means there are
			# two "successors" to this board.
			if( $val[2] & BOX ) {
				my $pull = $new;
				vec($pull, $loc[2], 4) -= BOX;
				vec($pull, $loc[1], 4) += BOX;
				goto RWALK if exists $rseen{$pull};
				my $pmoves = $UDLR[$dir_num] . $moves;
				$rseen{$pull} = $pmoves;
				push @rnext, [$pmoves, $pull, $loc[0]];
				goto RWALK unless exists $fseen{$pull};
				print "Doing pull\n";
				$solution = $pull;
				last MAIN;
			}
			RWALK:
			next if exists $rseen{$new}; # next direction.
			my $wmoves = $udlr[$dir_num] . $moves;
			$rseen{$new} = $wmoves;
			push @rnext, [$wmoves, $new, $loc[0]];
			next unless exists $fseen{$new};
			print "Rwalk\n";
			$solution = $new;
			last MAIN;
		}

		last BACKWARD if @rtodo;
		use vars qw(*rtodo *rnext);
		(*rtodo, *rnext) = (\@rnext, \@rtodo);
	} # end BACKWARD
}

if( $solution ) {
	my $fmoves = $fseen{$solution};
	my $rmoves = $rseen{$solution};
	print "Solution found!\n";
	print "Time: ", (time() - $^T), " seconds\n";
	print "Moves: $fmoves $rmoves\n";
	print "Move Length: ", length($fmoves . $rmoves), "\n";
	print "Middle Board: \n", printable_board($solution);
} else {
	print "No solution found!\n";
}
__END__

```

{{out}}

```txt
Solution found!
Time: 51 seconds
Moves: lldlllllllluurDldRRRRRRRRuulD rdLLLLLLrrrrrurrrdLLLLLLLrrrruulDulDulDulDLLulD
Move Length: 76
Middle Board:
#############
#  #        #
#  $$$$$@   #
#.......$ $ #
#############

```

On this particular puzzle, the branch factor for the different search directions
were clearly quite different, as the forward search only did 29 moves, while the
reverse search did 47 moves.

Although my code doesn't print out the actual final board, it would be easy enough
to compute from the move list.

## Perl 6

{{trans|Go}}

```perl6
sub MAIN() {
    my $level = q:to//;
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######

    say 'level:';
    print $level;
    say 'solution:';
    say solve($level);
}

class State {
    has Str $.board;
    has Str $.sol;
    has Int $.pos;

    method move(Int $delta --> Str) {
        my $new = $!board;
        if $new.substr($!pos,1) eq '@' {
            substr-rw($new,$!pos,1) = ' ';
        } else {
            substr-rw($new,$!pos,1) = '.';
        }
        my $pos := $!pos + $delta;
        if $new.substr($pos,1) eq ' ' {
            substr-rw($new,$pos,1) = '@';
        } else {
            substr-rw($new,$pos,1) = '+';
        }
        return $new;
    }

    method push(Int $delta --> Str) {
        my $pos := $!pos + $delta;
        my $box := $pos + $delta;
        return '' unless $!board.substr($box,1) eq ' ' | '.';
        my $new = $!board;
        if $new.substr($!pos,1) eq '@' {
            substr-rw($new,$!pos,1) = ' ';
        } else {
            substr-rw($new,$!pos,1) = '.';
        }
        if $new.substr($pos,1) eq '$' {
            substr-rw($new,$pos,1) = '@';
        } else {
            substr-rw($new,$pos,1) = '+';
        }
        if $new.substr($box,1) eq ' ' {
            substr-rw($new,$box,1) = '$';
        } else {
            substr-rw($new,$box,1) = '*';
        }
        return $new;
    }
}

sub solve(Str $start --> Str) {
    my $board = $start;
    my $width = $board.lines[0].chars + 1;
    my @dirs =
        ["u", "U", -$width],
        ["r", "R", 1],
        ["d", "D", $width],
        ["l", "L", -1];

    my %visited = $board => True;

    my $pos = $board.index('@');
    my @open = State.new(:$board, :sol(''), :$pos);
    while @open {
        my $state = @open.shift;
        for @dirs -> [$move, $push, $delta] {
            my $board;
            my $sol;
            my $pos = $state.pos + $delta;
            given $state.board.substr($pos,1) {
                when '$' | '*' {
                    $board = $state.push($delta);
                    next if $board eq "" || %visited{$board};
                    $sol = $state.sol ~ $push;
                    return $sol unless $board ~~ /<[ . + ]>/;
                }
                when ' ' | '.' {
                    $board = $state.move($delta);
                    next if %visited{$board};
                    $sol = $state.sol ~ $move;
                }
                default { next }
            }
            @open.push: State.new: :$board, :$sol, :$pos;
            %visited{$board} = True;
        }
    }
    return "No solution";
}
```

{{out}}

```txt
Level:
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######
Solution:
ulULLulDDurrrddlULrruLLrrUruLLLulD
```



## Phix

Push-optimised, prunes (breadth-first) search space to reachable pushable-to-live boxes.

Fairly fast, but often produces same-push-tally but longer results than move-optimised.

```Phix
-- demo\rosetta\Sokoban.exw
integer w, h    -- (set from parsing the input grid)
sequence moves  --    "", as +/-w and +/-1 (udlr)
string live     --    "", Y if box can go there

function reachable(sequence pushes, string level)
    integer p = find_any("@+",level)
    string ok = repeat('N',length(level))
    ok[p] = 'Y'
    while true do
        p = find('Y',ok)
        if p=0 then exit end if
        ok[p] = 'y'
        for i=1 to length(moves) do
            integer pn = p+moves[i]
            if ok[pn]='N'
            and find(level[pn]," .") then
                ok[pn] = 'Y'
            end if
        end for
    end while
    for i=length(pushes)-1 to 1 by -2 do
        if ok[pushes[i]-pushes[i+1]]!='y' then
            pushes[i..i+1] = {}
        end if
    end for
    return pushes
end function

function pushable(string level)
    sequence res = {}
    for i=1 to length(level) do
        if find(level[i],"$*") then
            if  find(level[i-w]," .@+")
            and find(level[i+w]," .@+") then
                if live[i-w]='Y' then res &= {i,-w} end if
                if live[i+w]='Y' then res &= {i,+w} end if
            end if
            if  find(level[i-1]," .@+")
            and find(level[i+1]," .@+") then
                if live[i-1]='Y' then res &= {i,-1} end if
                if live[i+1]='Y' then res &= {i,+1} end if
            end if
        end if
    end for
    return reachable(res,level)
end function

function solve(string level)
    atom t2 = time()+2
    integer seen = new_dict()
    sequence solution = "No solution.", partial = {}
    sequence todo = {{level,partial,pushable(level)}}, pushes
    while length(todo) do
        sequence t1 = todo[1]
        todo = todo[2..$]
        {level,partial,pushes} = t1
        integer p = find_any("@+",level)
        while length(pushes) do
            integer {s,m} = pushes[1..2]
            pushes = pushes[3..$]
            level[p] = " ."[find(level[p],"@+")]
            level[s] = "@+"[find(level[s],"$*")]
            level[s+m] = "$*"[find(level[s+m]," .")]
            if getd_index(level,seen)=0 then
                sequence np = partial&{s,m}
                if not find('$',level) then
                    solution = np
                    todo = {}
                    pushes = {}
                    exit
                end if
                setd(level,true,seen)
                if time()>t2 then
                    printf(1,"working... (seen %d)\r",dict_size(seen))
                    t2 = time()+2
                end if
                todo = append(todo,{level,np,pushable(level)})
            end if
            level = t1[1] -- (reset)
        end while
    end while
    destroy_dict(seen)
    return solution
end function

procedure plays(string level, sequence solution)
-- This plays push-only solutions (see play() for lurd)
    string res = level
    integer p = find_any("@+",level)
    for i=1 to length(solution) by 2 do
        integer {s,m} = solution[i..i+1] m+=s
        level[p] = " ."[find(level[p],"@+")]
        level[s] = "@+"[find(level[s],"$*")]
        level[m] = "$*"[find(level[m]," .")]
        res &= level
        p = s
    end for
    -- (replacing +0 with 1/2/3 may help in some cases)
    puts(1,join_by(split(res,'\n'),h,floor(80/(w+2))+0))
end procedure

procedure mark_live(integer p, string level)
-- (idea cribbed from the C version)
    if live[p]='N' then
        live[p] = 'Y'
        integer l = length(level)
        if p-w*2>=1 and level[p-w]!='#' and level[p-w*2]!='#' then mark_live(p-w,level) end if
        if p+w*2<=l and level[p+w]!='#' and level[p+w*2]!='#' then mark_live(p+w,level) end if
        if p-2  >=1 and level[p-1]!='#' and level[p-2]  !='#' then mark_live(p-1,level) end if
        if p+2  <=l and level[p+1]!='#' and level[p+2]  !='#' then mark_live(p+1,level) end if
    end if
end procedure

function make_square(string level)
--
-- Sets {h, w, moves, live}, and returns an evened-out/rectangular level
--
    if level[$]!='\n' then level &= '\n' end if -- (for the display)
    sequence lines = split(level,'\n')
    h = length(lines)-1             -- set height (ignore trailing \n)
    sequence ln = repeat(0,h)
    for i=1 to h do
        ln[i] = {length(lines[i]),i}
        for j=1 to length(lines[i]) do
            -- validate each line, why not
            if not find(lines[i,j]," #.$@*") then
                crash("invalid input")
            end if
        end for
    end for
    ln = sort(ln)
    w = ln[$][1]+1                  -- set width (==longest, inc \n)
    moves = {-w,+w,-1,+1}           -- and make these (udlr) legal ...
    for i=1 to h do
        integer {l,n} = ln[i], pad = w-1-l
        if pad=0 then exit end if
        lines[n] &= repeat(' ',pad) -- ... by evening up the "grid"
    end for
    level = join(lines,'\n')
    live = join(repeat(repeat('N',w-1),h),'\n')
    for p=1 to length(level) do
        if find(level[p],".+*") then
            mark_live(p,level)
        end if
    end for
    return level
end function

constant input = """
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######
"""

atom t0 = time()
string level = make_square(input)
sequence pushset = solve(level)
integer pop = length(pushset)/2
if string(pushset) then
    puts(1,level)
    printf(1,"%s\n",{pushset}) -- ("No Solution.")
else
    printf(1,"solution of %d pushes (%s)\n",{pop,elapsed(time()-t0)})
    plays(level,pushset)
end if
```

{{out}}
Note that a full solution in LURD format would show as 48 moves, as opposed to
the move-optimal solutions of other entries of 34 moves, but both are 14 pushes.

```txt

solution of 14 pushes (0.5s)
#######   #######   #######   #######   #######   #######   #######   #######
#     #   #     #   #     #   #     #   #     #   #     #   #     #   #     #
#     #   #     #   #   $ #   #  $@ #   # $@  #   #$@   #   #@    #   #     #
#. #  #   #. #$ #   #. #@ #   #. #  #   #. #  #   #. #  #   #* #  #   #* #  #
#. $$ #   #. $@ #   #. $  #   #. $  #   #. $  #   #. $  #   #. $  #   #.$@  #
#.$$  #   #.$$  #   #.$$  #   #.$$  #   #.$$  #   #.$$  #   #.$$  #   #.$$  #
#.#  @#   #.#   #   #.#   #   #.#   #   #.#   #   #.#   #   #.#   #   #.#   #
#######   #######   #######   #######   #######   #######   #######   #######

#######   #######   #######   #######   #######   #######   #######
#     #   #     #   #     #   #     #   #     #   #     #   #     #
#     #   #     #   #     #   #     #   #     #   #     #   #     #
#* #  #   #* #  #   #* #  #   #* #  #   #* #  #   #* #  #   #* #  #
#.$$  #   #.$$  #   #.@$  #   #. $  #   #.$@  #   #*@   #   #*    #
#.$@  #   #*@   #   #*$   #   #+$   #   #.$   #   #.$   #   #*@   #
#.#   #   #.#   #   #.#   #   #*#   #   #*#   #   #*#   #   #*#   #
#######   #######   #######   #######   #######   #######   #######

```


Other tests:

```Phix
constant input = """
#############
#  #        #
# $$$$$$$  @#
#.......    #
#############
"""
```

{{out}}

```txt

solution of 30 pushes (14.6s)
#############   #############   #############   #############   #############   #############   #############   #############
#  #        #   #  #        #   #  #        #   #  #        #   #  #        #   #  #        #   #  #        #   #  #        #
# $$$$$$$  @#   # @$$$$$$   #   #  $$$$$$   #   #  $$$$$$   #   #  $$$$$$   #   #  $$$$$$   #   #  $$$$$$   #   #  $$$$$$   #
#.......    #   #.*.....    #   #.+*....    #   #..+*...    #   #...+*..    #   #....+*.    #   #.....+*    #   #......+$   #
#############   #############   #############   #############   #############   #############   #############   #############

#############   #############   #############   #############   #############   #############   #############   #############
#  #        #   #  #        #   #  #        #   #  #        #   #  #        #   #  #        #   #  #        #   #  #        #
#  $$$$$$   #   #  $$$$$$   #   #  $@$$$$   #   # $@ $$$$   #   # @  $$$$   #   #    $@$$   #   #   $@ $$   #   #  $@  $$   #
#.......@$  #   #....... @$ #   #...*...  $ #   #...*...  $ #   #.*.*...  $ #   #.*.*.*.  $ #   #.*.*.*.  $ #   #.*.*.*.  $ #
#############   #############   #############   #############   #############   #############   #############   #############

#############   #############   #############   #############   #############   #############   #############   #############
#  #        #   #  #        #   #  #        #   #  #        #   #  #        #   #  #        #   #  #        #   #  #        #
# $@   $$   #   #$@    $$   #   #@     $$   #   #      $@   #   #     $@    #   #    $@     #   #   $@      #   #   $       #
#.*.*.*.  $ #   #.*.*.*.  $ #   #**.*.*.  $ #   #**.*.*.$ $ #   #**.*.*.$ $ #   #**.*.*.$ $ #   #**.*.*.$ $ #   #***+.*.$ $ #
#############   #############   #############   #############   #############   #############   #############   #############

#############   #############   #############   #############   #############   #############   #############
#  #        #   #  #        #   #  #        #   #  #        #   #  #        #   #  #        #   #  #        #
#   @       #   #           #   #           #   #           #   #           #   #           #   #           #
#****.*.$ $ #   #*****+.$ $ #   #*****.*@ $ #   #******+  $ #   #******. $@ #   #******.$@  #   #*******@   #
#############   #############   #############   #############   #############   #############   #############

```

Test #3

```Phix
constant input = """
     ####
    ##. ##
##### .  #
#   #  # #
# $ #  # #
# $  @   #
######  ##
     ####
"""
```

{{out}}

```txt

solution of 16 pushes (0.0s)
     ####         ####         ####         ####         ####         ####         ####         ####         ####
    ##. ##       ##. ##       ##. ##       ##. ##       ##. ##       ##. ##       ##. ##       ##. ##       ##. ##
##### .  #   ##### .  #   ##### .  #   ##### .  #   ##### .  #   ##### .  #   ##### .  #   ##### .  #   ##### *  #
#   #  # #   #   #  # #   #   #  # #   #   #  # #   #   #  # #   #   #  # #   #   #  # #   #   # $# #   #   # @# #
# $ #  # #   # @$#  # #   #  $#  # #   #  $#  # #   #  $#  # #   #  $#  # #   #  $# $# #   #  $# @# #   #  $#  # #
# $  @   #   # $      #   # @$     #   #  @$    #   #   @$   #   #    @$  #   #     @  #   #        #   #        #
######  ##   ######  ##   ######  ##   ######  ##   ######  ##   ######  ##   ######  ##   ######  ##   ######  ##
     ####         ####         ####         ####         ####         ####         ####         ####         ####

     ####         ####         ####         ####         ####         ####         ####         ####
    ##* ##       ##* ##       ##* ##       ##* ##       ##* ##       ##* ##       ##* ##       ##* ##
##### +  #   ##### .  #   ##### .  #   ##### .  #   ##### .  #   ##### .  #   ##### .  #   ##### *  #
#   #  # #   #   #  # #   #   #  # #   #   #  # #   #   #  # #   #   #  # #   #   # $# #   #   # @# #
#  $#  # #   #  @#  # #   #   #  # #   #   #  # #   #   #  # #   #   # $# #   #   # @# #   #   #  # #
#        #   #  $     #   #  @$    #   #   @$   #   #    @$  #   #     @  #   #        #   #        #
######  ##   ######  ##   ######  ##   ######  ##   ######  ##   ######  ##   ######  ##   ######  ##
     ####         ####         ####         ####         ####         ####         ####         ####

```

Test #4

```Phix
constant input = """
#############
#... #      #
#.$$$$$$$  @#
#...        #
#############
"""
```

{{out}}

```txt

"started"
solution of 40 pushes (58.5s)
#############   #############   #############   #############   #############   #############
#... #      #   #... #      #   #.*. #      #   #.** #      #   #.** #      #   #.** #      #
#.$$$$$$$  @#   #.$$@$$$$   #   #.@$ $$$$   #   #. @ $$$$   #   #.   $$$$   #   #.   $$$$   #
#...        #   #...$       #   #...$       #   #...$       #   #...@$      #   #... @$     #
#############   #############   #############   #############   #############   #############
<snip 30 pushes>
#############   #############   #############   #############   #############
#*** #      #   #*** #      #   #*** #      #   #*** #      #   #*** #      #
#*          #   #*          #   #*          #   #*          #   #*          #
#**.   $@   #   #**.  $@    #   #**. $@     #   #**.$@      #   #***@       #
#############   #############   #############   #############   #############

```

Test #5

```Phix
constant input = """
    #####
    #   #
    #   #
  ### #$##
  #      #
### #$## #   ######
#   # ## #####   .#
# $   $         ..#
##### ### #@##   .#
    #     #########
    #######
"""
```

{{out}}

```txt

solution of 59 pushes (25.5s)
    #####                 #####                 #####                 #####
    #   #                 #   #                 #   #                 #   #
    #   #                 #   #                 #   #                 #   #
  ### #$##              ### #@##              ### # ##              ### # ##
  #      #              #    $ #              #   $@ #              #   $  #
### #$## #   ######   ### #$## #   ######   ### #$## #   ######   ### #@## #   ######
#   # ## #####   .#   #   # ## #####   .#   #   # ## #####   .#   #   #$## #####   .#
# $   $         ..#   # $   $         ..#   # $   $         ..#   # $   $         ..#
##### ### #@##   .#   ##### ### # ##   .#   ##### ### # ##   .#   ##### ### # ##   .#
    #     #########       #     #########       #     #########       #     #########
    #######               #######               #######               #######

<snip 52 pushes>

    #####                 #####                 #####                 #####
    #   #                 #   #                 #   #                 #   #
    #   #                 #   #                 #   #                 #   #
  ### # ##              ### # ##              ### # ##              ### # ##
  #      #              #      #              #      #              #      #
### # ## #   ######   ### # ## #   ######   ### # ## #   ######   ### # ## #   ######
#   # ## #####   *#   #   # ## #####   *#   #   # ## #####   *#   #   # ## #####   *#
#             @$.*#   #              @**#   #               **#   #               **#
##### ### # ## $ .#   ##### ### # ## $ .#   ##### ### # ## @$.#   ##### ### # ##  @*#
    #     #########       #     #########       #     #########       #     #########
    #######               #######               #######               #######

```



## PicoLisp

This searches for a solution, without trying for the push-optimal one. The player moves between the pushes, however, are minimized.

```PicoLisp
(load "@lib/simul.l")

# Display board
(de display ()
   (disp *Board NIL
      '((This)
         (pack
            (if2 (== This *Pos) (memq This *Goals)
               "+"                   # Player on goal
               "@"                   # Player elsewhere
               (if (: val) "*" ".")  # On gloal
               (or (: val) " ") )    # Elsewhere
            " " ) ) ) )

# Initialize
(de main (Lst)
   (mapc
      '((B L)
         (mapc
            '((This C)
               (case C
                  (" ")
                  ("." (push '*Goals This))
                  ("@" (setq *Pos This))
                  ("$" (=: val C) (push '*Boxes This))
                  (T (=: val C)) ) )
               B L ) )
      (setq *Board (grid (length (car Lst)) (length Lst)))
      (apply mapcar (flip (mapcar chop Lst)) list) )
   (display) )

# Generate possible push-moves
(de pushes ()
   (make
      (for Box *Boxes
         (unless (or (; (west Box) val) (; (east Box) val))
            (when (moves (east Box))
               (link (cons (cons Box (west Box)) *Pos "L" @)) )
            (when (moves (west Box))
               (link (cons (cons Box (east Box)) *Pos "R" @)) ) )
         (unless (or (; (south Box) val) (; (north Box) val))
            (when (moves (north Box))
               (link (cons (cons Box (south Box)) *Pos "D" @)) )
            (when (moves (south Box))
               (link (cons (cons Box (north Box)) *Pos "U" @)) ) ) ) ) )

# Moves of player to destination
(de moves (Dst Hist)
   (or
      (== Dst *Pos)
      (mini length
         (extract
            '((Dir)
               (with ((car Dir) Dst)
                  (cond
                     ((== This *Pos) (cons (cdr Dir)))
                     ((: val))
                     ((memq This Hist))
                     ((moves This (cons Dst Hist))
                        (cons (cdr Dir) @) ) ) ) )
            '((west . "r") (east . "l") (south . "u") (north . "d")) ) ) ) )

# Find solution
(de go (Res)
   (unless (idx '*Hist (sort (copy *Boxes)) T)  # No repeated state
      (if (find '((This) (<> "$" (: val))) *Goals)
         (pick
            '((Psh)
               (setq  # Move
                  *Pos (caar Psh)
                  *Boxes (cons (cdar Psh) (delq *Pos *Boxes)) )
               (put *Pos 'val NIL)
               (put (cdar Psh) 'val "$")
               (prog1 (go (append (cddr Psh) Res))
                  (setq  # Undo move
                     *Pos (cadr Psh)
                     *Boxes (cons (caar Psh) (delq (cdar Psh) *Boxes)) )
                  (put (cdar Psh) 'val NIL)
                  (put (caar Psh) 'val "$") ) )
            (pushes) )
         (display)  # Display solution
         (pack (flip Res)) ) ) )
```

Test:

```PicoLisp
(main
   (quote
      "#######"
      "#     #"
      "#     #"
      "#. #  #"
      "#. $$ #"
      "#.$$  #"
      "#.#  @#"
      "#######" ) )
(prinl)
(go)
```

Output:

```txt
 8 # # # # # # #
 7 #           #
 6 #           #
 5 # .   #     #
 4 # .   $ $   #
 3 # . $ $     #
 2 # . #     @ #
 1 # # # # # # #
   a b c d e f g

 8 # # # # # # #
 7 #           #
 6 # @         #
 5 # *   #     #
 4 # *         #
 3 # *         #
 2 # * #       #
 1 # # # # # # #
   a b c d e f g
-> "uuulDLLulDDurrrrddlUruLLLrrddlUruLdLUUdrruulLulD"
```



## Python

{{trans|D}}
{{works with|Psyco}}
{{works with|Python 2.6}}

```python
from array import array
from collections import deque
import psyco

data = []
nrows = 0
px = py = 0
sdata = ""
ddata = ""

def init(board):
    global data, nrows, sdata, ddata, px, py
    data = filter(None, board.splitlines())
    nrows = max(len(r) for r in data)

    maps = {' ':' ', '.': '.', '@':' ', '#':'#', '$':' '}
    mapd = {' ':' ', '.': ' ', '@':'@', '#':' ', '$':'*'}

    for r, row in enumerate(data):
        for c, ch in enumerate(row):
            sdata += maps[ch]
            ddata += mapd[ch]
            if ch == '@':
                px = c
                py = r

def push(x, y, dx, dy, data):
    if sdata[(y+2*dy) * nrows + x+2*dx] == '#' or \
       data[(y+2*dy) * nrows + x+2*dx] != ' ':
        return None

    data2 = array("c", data)
    data2[y * nrows + x] = ' '
    data2[(y+dy) * nrows + x+dx] = '@'
    data2[(y+2*dy) * nrows + x+2*dx] = '*'
    return data2.tostring()

def is_solved(data):
    for i in xrange(len(data)):
        if (sdata[i] == '.') != (data[i] == '*'):
            return False
    return True

def solve():
    open = deque([(ddata, "", px, py)])
    visited = set([ddata])
    dirs = ((0, -1, 'u', 'U'), ( 1, 0, 'r', 'R'),
            (0,  1, 'd', 'D'), (-1, 0, 'l', 'L'))

    lnrows = nrows
    while open:
        cur, csol, x, y = open.popleft()

        for di in dirs:
            temp = cur
            dx, dy = di[0], di[1]

            if temp[(y+dy) * lnrows + x+dx] == '*':
                temp = push(x, y, dx, dy, temp)
                if temp and temp not in visited:
                    if is_solved(temp):
                        return csol + di[3]
                    open.append((temp, csol + di[3], x+dx, y+dy))
                    visited.add(temp)
            else:
                if sdata[(y+dy) * lnrows + x+dx] == '#' or \
                   temp[(y+dy) * lnrows + x+dx] != ' ':
                    continue

                data2 = array("c", temp)
                data2[y * lnrows + x] = ' '
                data2[(y+dy) * lnrows + x+dx] = '@'
                temp = data2.tostring()

                if temp not in visited:
                    if is_solved(temp):
                        return csol + di[2]
                    open.append((temp, csol + di[2], x+dx, y+dy))
                    visited.add(temp)

    return "No solution"


level = """\
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######"""

psyco.full()
init(level)
print level, "\n\n", solve()
```

Output:

```txt
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######

ulULLulDDurrrddlULrruLLrrUruLLLulD
```

Runtime: about 0.90 seconds.


## Racket

This was originally inspired by PicoLisp's solution.  Modified to use a priority queue as mentioned on the Sokoban wiki for the main breadth first search on pushes but just a plain queue for the move bfs.  This uses personal libraries.  Vector2 isn't strictly needed but the math/array library is not currently optimized for untyped Racket.  push! is comparable to lisp's, awhen is anaphoric when, ret uses the bound value as the result of its expression, and tstruct is short for struct with the #:transparent option.


```Racket

#lang racket
(require data/heap
  "../lib/vector2.rkt" "../lib/queue.rkt" (only-in "../lib/util.rkt" push! tstruct ret awhen))

(define level (list "#######"
                    "#     #"
                    "#     #"
                    "#. #  #"
                    "#. $$ #"
                    "#.$$  #"
                    "#.#  @#"
                    "#######"))
(define (strings->vec2 l) (lists->vec2 (map string->list l)))
;turn everything except walls into distance from goals
(define (clear-level l)
  (ret ([l (vec2-copy l)])
    (define dots (vec2-atsq l #\.))
    (define q (list->q (map (λ (p) (cons p 0)) dots)))
    (let bfs () ;this search has implicit history in the mutated vector2
      (unless (nilq? q)
        (match-define (cons p n) (deq! q))
        (define x (vec2@ l p))
        ;stop if position is either a wall or a previously filled number
        (cond [(or (eq? x #\#) (number? x)) (bfs)]
              [else (vec2! l p n)
                    (for-adj l x [p p] #f (enq! (cons p (add1 n)) q))
                    (bfs)])))))

;corresponds to PicoLisp's move table in "moves", while also adding a push direction mapping
(tstruct move (f d))
(define-values (mu md ml mr LURD)
  (let ()
    (define t (map (λ (x) (cons (car x) (apply pos (cdr x))))
                   '([#\u -1 0] [#\d 1 0] [#\l 0 -1] [#\r 0 1])))
    (define (mv d)
      (define x (assoc d t))
      (move (λ (p) (pos+ p (cdr x))) (car x)))
    (values (mv #\u) (mv #\d) (mv #\l) (mv #\r)
            (λ (d) (char-upcase (car (findf (λ (x) (equal? d (cdr x))) t)))))))

;state = player pos * box poses
(tstruct st (p b))
(define (st= s1 s2) (andmap (λ (b) (member b (st-b s2))) (st-b s1)))
(define (box? p s) (member p (st-b s)))
;calculates value of a state for insertion into priority queue
;value is sum of box distances from goals
(define (value s l) (apply + (map (λ (p) (vec2@ l p)) (st-b s))))
;init state for a level
(define (st0 l) (st (vec2-atq l #\@) (vec2-atsq l #\$)))
(define (make-solution-checker l)
  (define dots (vec2-atsq l #\.))
  (λ (s) (andmap (λ (b) (member b dots)) (st-b s))))

;state after push * lurd history
(tstruct push (st h))
(define (pushes s l)
  (ret ([pushes '()])
    (for ([b (in-list (st-b s))])
      (for-adj l a [p b] #f
        (define d (pos- p b)) ;direction of push
        (define op (pos- b d)) ;where player stands to push
        (define o (vec2@ l op))
        ;make sure push pos and push dest are clear
        (when (and (number? a) (number? o)
                   (not (box? p s)) (not (box? op s)))
          (awhen [@ (moves s op l)]
            (define new-st (st b (cons p (remove b (st-b s)))))
            (push! (push new-st (cons (LURD d) @)) pushes)))))))

;state * goal pos * level -> lurd string
(define (moves s g l)
  (define h '())
  (define q (list->q (list (list (st-p s)))))
  (let bfs ()
    (if (nilq? q)
        #f
        (match-let ([(cons p lurd) (deq! q)])
          (cond [(equal? p g) lurd]
                [(or (char=? (vec2@ l p) #\#) (box? p s) (member p h)) (bfs)]
                [else (push! p h)
                      (for-each (λ (m)
                                  (match-define (move f s) m)
                                  (enq! (cons (f p) (cons s lurd)) q))
                                (list mu md ml mr))
                      (bfs)])))))

(define (sokoban l)
  (define-values (clear s0 solved?)
    (let ([l (strings->vec2 l)])
      (values (clear-level l) (st0 l) (make-solution-checker l))))
  (define h '())
  (tstruct q-elem (s lurd v)) ;priority queue stores state, lurd hist, and value
  (define (elem<= s1 s2) (<= (q-elem-v s1) (q-elem-v s2))) ;compare wrapped values
  ;queue stores a single element at the beginning consisting of:
  ;1. starting state, 2. empty lurd history, 3. value of starting state
  (define q (vector->heap elem<= (vector (q-elem s0 '() (value s0 clear)))))
  (let bfs ()
    (match-define (q-elem s lurd _) (heap-min q))
    (heap-remove-min! q)
    (cond [(solved? s) (list->string (reverse lurd))]
          [(memf (λ (s1) (st= s s1)) h) (bfs)]
          [else (push! s h)
                (for-each (λ (p)
                            (define s (push-st p))
                            (heap-add! q (q-elem s (append (push-h p) lurd) (value s clear))))
                          (pushes s clear))
                (bfs)])))

```


{{out}}
Times shown are milliseconds.

```txt

> (time (sokoban level))
cpu time: 88 real time: 83 gc time: 0
"uuulDLLrrrddllUdrruulLrrdLuuulldlDDuuurrrddlLrrddlULrruLdlUUdrruulLulD"

```



## Ring


```ring

#--------------------------------------------------#
# Sokoban Game  				   #
#--------------------------------------------------#

# Game Data

	aPlayer = [ :Row = 3, :Col = 4 ]

	aLevel1 = [
		[1,1,1,2,2,2,2,2,1,1,1,1,1,1],
		[1,2,2,2,1,1,1,2,1,1,1,1,1,1],
		[1,2,4,3,5,1,1,2,1,1,1,1,1,1],
		[1,2,2,2,1,5,4,2,1,1,1,1,1,1],
		[1,2,4,2,2,5,1,2,1,1,1,1,1,1],
		[1,2,1,2,1,4,1,2,2,1,1,1,1,1],
		[1,2,5,1,6,5,5,4,2,1,1,1,1,1],
		[1,2,1,1,1,4,1,1,2,1,1,1,1,1],
		[1,2,2,2,2,2,2,2,2,1,1,1,1,1],
		[1,1,1,1,1,1,1,1,1,1,1,1,1,1]
	]

	aLevel2 = [
		[1,1,1,2,2,2,2,2,2,2,2,2,1,1],
		[1,2,2,2,1,5,1,4,1,1,1,2,1,1],
		[1,2,4,3,5,1,1,1,5,1,1,2,1,1],
		[1,2,2,2,1,1,4,1,1,1,1,2,1,1],
		[1,2,4,2,2,1,5,4,1,5,1,2,1,1],
		[1,2,1,2,1,4,1,5,1,1,2,2,1,1],
		[1,2,5,1,6,5,1,4,1,1,1,2,1,1],
		[1,2,1,1,1,4,1,4,1,5,1,2,1,1],
		[1,2,2,2,2,2,2,2,2,2,2,2,1,1],
		[1,1,1,1,1,1,1,1,1,1,1,1,1,1]
	]

        aLevel = aLevel1
	nActiveLevel = 1

	# For Game Restart
		aLevel1Copy  = aLevel1
		aLevel2Copy  = aLevel2
		aPlayerCopy = aPlayer

	C_LEVEL_ROWSCOUNT = 10
	C_LEVEL_COLSCOUNT = 14

	C_EMPTY 	= 1
	C_WALL  	= 2
	C_PLAYER	= 3
	C_DOOR  	= 4
	C_BOX   	= 5
	C_BOXONDOOR 	= 6
	C_PLAYERONDOOR  = 7

	nKeyClock = clock()

	# Will be used when moving a Box
		aCurrentBox = [ :Row = 0, :Col = 0 ]
		nRowDiff = 0
		nColDiff = 0

	# When the player win
		lPlayerWin = False

load "gameengine.ring"

func main

	oGame = New Game
	{

		title = "Sokoban"

		Map {

			blockwidth  = 60
			blockheight = 60

			aMap = aLevel

			aImages = [
				"images/empty.jpg",
				"images/wall.jpg",
				"images/player.jpg",
				"images/door.jpg",
				"images/box.jpg",
				"images/boxondoor.jpg",
				"images/player.jpg"	# Player on Door
			]

			keypress = func oGame,oSelf,nkey {
				# Avoid getting many keys in short time
					if (clock() - nKeyClock) < clockspersecond()/4 return ok
					nKeyClock = Clock()
				Switch nkey
					on Key_Esc
						oGame.Shutdown()
					on Key_Space
						# Restart the Level
							if nActiveLevel = 1
								aLevel = aLevel1Copy
							else
								aLevel = aLevel2Copy
							ok
							aPlayer = aPlayerCopy
							UpdateGameMap(oGame)
							lPlayerWin = False
					on Key_Right
						if aPlayer[:col] < C_LEVEL_COLSCOUNT
							nRowDiff = 0   nColDiff = 1
							MoveObject(oGame,PlayerType(),aPlayer[:row],aPlayer[:col]+1)
						ok
					on Key_Left
						if aPlayer[:col] > 1
							nRowDiff = 0   nColDiff = -1
							MoveObject(oGame,PlayerType(),aPlayer[:row],aPlayer[:col]-1)
						ok
					on Key_Up
						if aPlayer[:row] > 1
							nRowDiff = -1   nColDiff = 0
							MoveObject(oGame,PlayerType(),aPlayer[:row]-1,aPlayer[:col])
						ok
					on Key_Down
						if aPlayer[:row] < C_LEVEL_ROWSCOUNT
							nRowDiff = 1   nColDiff = 0
							MoveObject(oGame,PlayerType(),aPlayer[:row]+1,aPlayer[:col])
						ok
				off
				if lPlayerWin = False
					if CheckWin()
						lPlayerWin = True
						DisplayYouWin(oGame)
					ok
				ok
			}

		}

		text {
			x = 70	y=550
			animate = false
			size = 20
			file = "fonts/pirulen.ttf"
			text = "Level:"
			color = rgb(0,0,0)
		}
                NewButton(oGame,180,550,150,30,"Level 1",:Click1)
                NewButton(oGame,350,550,150,30,"Level 2",:Click2)
	}

func MoveObject oGame,nObjectType,nNewRow,nNewCol
	lMove = False
	switch nObjectType
		on  C_PLAYER
			switch aLevel[nNewRow][nNewCol]
				on C_EMPTY
					aLevel[aPlayer[:row]][aPlayer[:col]] = C_EMPTY
					aLevel[nNewRow][nNewCol] = C_PLAYER
					UpdateGameMap(oGame)
					aPlayer[:row] = nNewRow
					aPlayer[:col] = nNewCol
					lMove = True
				on C_DOOR
					aLevel[aPlayer[:row]][aPlayer[:col]] = C_EMPTY
					aLevel[nNewRow][nNewCol] = C_PLAYERONDOOR
					UpdateGameMap(oGame)
					aPlayer[:row] = nNewRow
					aPlayer[:col] = nNewCol
					lMove = True
				on C_BOX
					aCurrentBox[:row] = nNewRow
					aCurrentBox[:col] = nNewCol
					if MoveObject(oGame,C_BOX,nNewRow+nRowDiff,nNewCol+nColDiff)
						aLevel[aPlayer[:row]][aPlayer[:col]] = C_EMPTY
						aLevel[nNewRow][nNewCol] = C_PLAYER
						UpdateGameMap(oGame)
						aPlayer[:row] = nNewRow
						aPlayer[:col] = nNewCol
						lMove = True
					ok
				on C_BOXONDOOR
					aCurrentBox[:row] = nNewRow
					aCurrentBox[:col] = nNewCol
					if MoveObject(oGame,C_BOXONDOOR,nNewRow+nRowDiff,nNewCol+nColDiff)
						aLevel[aPlayer[:row]][aPlayer[:col]] = C_EMPTY
						aLevel[nNewRow][nNewCol] = C_PLAYERONDOOR
						UpdateGameMap(oGame)
						aPlayer[:row] = nNewRow
						aPlayer[:col] = nNewCol
						lMove = True
					ok
			off
		on  C_PLAYERONDOOR
			switch aLevel[nNewRow][nNewCol]
				on C_EMPTY
					aLevel[aPlayer[:row]][aPlayer[:col]] = C_DOOR
					aLevel[nNewRow][nNewCol] = C_PLAYER
					UpdateGameMap(oGame)
					aPlayer[:row] = nNewRow
					aPlayer[:col] = nNewCol
					lMove = True
				on C_DOOR
					aLevel[aPlayer[:row]][aPlayer[:col]] = C_DOOR
					aLevel[nNewRow][nNewCol] = C_PLAYERONDOOR
					UpdateGameMap(oGame)
					aPlayer[:row] = nNewRow
					aPlayer[:col] = nNewCol
					lMove = True
				on C_BOX
					aCurrentBox[:row] = nNewRow
					aCurrentBox[:col] = nNewCol
					if MoveObject(oGame,C_BOX,nNewRow+nRowDiff,nNewCol+nColDiff)
						aLevel[aPlayer[:row]][aPlayer[:col]] = C_DOOR
						aLevel[nNewRow][nNewCol] = C_PLAYER
						UpdateGameMap(oGame)
						aPlayer[:row] = nNewRow
						aPlayer[:col] = nNewCol
						lMove = True
					ok
				on C_BOXONDOOR
					aCurrentBox[:row] = nNewRow
					aCurrentBox[:col] = nNewCol
					if MoveObject(oGame,C_BOXONDOOR,nNewRow+nRowDiff,nNewCol+nColDiff)
						aLevel[aPlayer[:row]][aPlayer[:col]] = C_DOOR
						aLevel[nNewRow][nNewCol] = C_PLAYER
						UpdateGameMap(oGame)
						aPlayer[:row] = nNewRow
						aPlayer[:col] = nNewCol
						lMove = True
					ok
			off
		on  C_BOX
			switch aLevel[nNewRow][nNewCol]
				on C_EMPTY
					aLevel[aCurrentBox[:row]][aCurrentBox[:col]] = C_EMPTY
					aLevel[nNewRow][nNewCol] = C_BOX
					UpdateGameMap(oGame)
					lMove = True
				on C_DOOR
					aLevel[aCurrentBox[:row]][aCurrentBox[:col]] = C_EMPTY
					aLevel[nNewRow][nNewCol] = C_BOXONDOOR
					UpdateGameMap(oGame)
					lMove = True
				on C_BOX
					aOldBox = aCurrentBox
					aCurrentBox[:row] = nNewRow
					aCurrentBox[:col] = nNewCol
					if MoveObject(oGame,C_BOX,nNewRow+nRowDiff,nNewCol+nColDiff)
						aCurrentBox = aOldBox
						aLevel[aCurrentBox[:row]][aCurrentBox[:col]] = C_EMPTY
						aLevel[nNewRow][nNewCol] = C_BOX
						UpdateGameMap(oGame)
						lMove = True
					ok
				on C_BOXONDOOR
					aOldBox = aCurrentBox
					aCurrentBox[:row] = nNewRow
					aCurrentBox[:col] = nNewCol
					if MoveObject(oGame,C_BOXONDOOR,nNewRow+nRowDiff,nNewCol+nColDiff)
						aCurrentBox = aOldBox
						aLevel[aCurrentBox[:row]][aCurrentBox[:col]] = C_EMPTY
						aLevel[nNewRow][nNewCol] = C_BOXONDOOR
						UpdateGameMap(oGame)
						lMove = True
					ok
			off
		on  C_BOXONDOOR
			switch aLevel[nNewRow][nNewCol]
				on C_EMPTY
					aLevel[aCurrentBox[:row]][aCurrentBox[:col]] = C_DOOR
					aLevel[nNewRow][nNewCol] = C_BOX
					UpdateGameMap(oGame)
					lMove = True
				on C_DOOR
					aLevel[aCurrentBox[:row]][aCurrentBox[:col]] = C_DOOR
					aLevel[nNewRow][nNewCol] = C_BOXONDOOR
					UpdateGameMap(oGame)
					lMove = True
				on C_BOX
					aOldBox = aCurrentBox
					aCurrentBox[:row] = nNewRow
					aCurrentBox[:col] = nNewCol
					if MoveObject(oGame,C_BOX,nNewRow+nRowDiff,nNewCol+nColDiff)
						aCurrentBox = aOldBox
						aLevel[aCurrentBox[:row]][aCurrentBox[:col]] = C_DOOR
						aLevel[nNewRow][nNewCol] = C_BOX
						UpdateGameMap(oGame)
						lMove = True
					ok
				on C_BOXONDOOR
					aOldBox = aCurrentBox
					aCurrentBox[:row] = nNewRow
					aCurrentBox[:col] = nNewCol
					if MoveObject(oGame,C_BOXONDOOR,nNewRow+nRowDiff,nNewCol+nColDiff)
						aCurrentBox = aOldBox
						aLevel[aCurrentBox[:row]][aCurrentBox[:col]] = C_DOOR
						aLevel[nNewRow][nNewCol] = C_BOXONDOOR
						UpdateGameMap(oGame)
						lMove = True
					ok

			off
	off
	return lMove

func UpdateGameMap oGame
	# The Map is our first object in Game Objects
		oGame.aObjects[1].aMap = aLevel

func PlayerType
	# It could be (Player) or (Player on door)
		return aLevel[aPlayer[:row]][aPlayer[:col]]

func CheckWin
	for aRow in aLevel
		if find(aRow,C_DOOR) or find(aRow,C_PLAYERONDOOR)
			return False
		ok
	next
	return True

func DisplayYouWin oGame
	oGame {
          text {
                point = 400
                size = 30
                nStep = 9
                file = "fonts/pirulen.ttf"
                text = "You Win !!!"
                x = 500  y=10
		state = func ogame,oself {
			if oself.y >= 400
				ogame.remove(oSelf.nIndex)
			ok
		}
          }
        }

func NewButton oGame,nX,nY,nWidth,nHeight,cText,cFunc
    oGame {
        Object {
                        x = nX y=nY width = nWidth height=nHeight
            AddAttribute(self,:Text)
            AddAttribute(self,:EventCode)
            Text = cText
            EventCode = cFunc
                        draw = func oGame,oSelf {
                                oSelf {
                    gl_draw_filled_rectangle(x,y,x+width,y+height,gl_map_rgb(0,100,255))
                    gl_draw_rectangle(x,y,x+width,y+height,gl_map_rgb(0,0,0),2)
                    oFont = oResources.LoadFont("fonts/pirulen.ttf",20)
                    gl_draw_text(oFont,gl_map_rgb(0,0,0),x+width/2,y+5,1,Text)
                }
            }
            mouse = func oGame,oSelf,nType,aMouseList {
                if nType = GE_MOUSE_UP
                    MouseX = aMouseList[GE_MOUSE_X]
                    MouseY = aMouseList[GE_MOUSE_Y]
                    oSelf {
                        if MouseX >= x and MouseX <= X+270 and
                           MouseY >= y and MouseY <= Y+40
                               call EventCode(oGame,oSelf)
                        ok
                    }
                ok
            }
        }
    }
    return len(oGame.aObjects)

func Click1 oGame,oSelf
	aLevel = aLevel1
	nActiveLevel = 1
	aPlayer = aPlayerCopy
	UpdateGameMap(oGame)
	lPlayerWin = False

func Click2 oGame,oSelf
	aLevel = aLevel2
	nActiveLevel = 2
	aPlayer = aPlayerCopy
	UpdateGameMap(oGame)
	lPlayerWin = False

```


Output image:

[https://www.mediafire.com/view/6dk3ai36vapua2a/SokobanGame.jpg/file Sokoban game (Level 1)]

[https://www.mediafire.com/view/go66tyi6ij6jtup/SokobanGame2.jpg/file Sokoban game (Level 2)]


## Ruby


### Simple Version

{{trans|Python}}

```ruby
require 'set'

class Sokoban
  def initialize(level)
    board = level.each_line.map(&:rstrip)
    @nrows = board.map(&:size).max
    board.map!{|line| line.ljust(@nrows)}
    board.each_with_index do |row, r|
      row.each_char.with_index do |ch, c|
        @px, @py = c, r  if ch == '@' or ch == '+'
      end
    end
    @goal = board.join.tr(' .@#$+*', ' .   ..')
                 .each_char.with_index.select{|ch, c| ch == '.'}
                 .map(&:last)
    @board = board.join.tr(' .@#$+*', '  @#$ $')
  end

  def pos(x, y)
    y * @nrows + x
  end

  def push(x, y, dx, dy, board)         # modify board
    return  if board[pos(x+2*dx, y+2*dy)] != ' '
    board[pos(x     , y     )] = ' '
    board[pos(x + dx, y + dy)] = '@'
    board[pos(x+2*dx, y+2*dy)] = '$'
  end

  def solved?(board)
    @goal.all?{|i| board[i] == '$'}
  end

  DIRS = [[0, -1, 'u', 'U'], [ 1, 0, 'r', 'R'], [0,  1, 'd', 'D'], [-1, 0, 'l', 'L']]
  def solve
    queue = [[@board, "", @px, @py]]
    visited = Set[@board]

    until queue.empty?
      current, csol, x, y = queue.shift

      for dx, dy, cmove, cpush in DIRS
        work = current.dup
        case work[pos(x+dx, y+dy)]      # next character
        when '$'
          next  unless push(x, y, dx, dy, work)
          next  unless visited.add?(work)
          return csol+cpush  if solved?(work)
          queue << [work, csol+cpush, x+dx, y+dy]
        when ' '
          work[pos(x, y)]       = ' '
          work[pos(x+dx, y+dy)] = '@'
          queue << [work, csol+cmove, x+dx, y+dy]  if visited.add?(work)
        end
      end
    end
    "No solution"
  end
end
```

'''Test:'''

```ruby
level = <<EOS
#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######
EOS
puts level, "", Sokoban.new(level).solve
```


{{out}}

```txt

#######
#     #
#     #
#. #  #
#. $$ #
#.$$  #
#.#  @#
#######

ulULLulDDurrrddlULrruLLrrUruLLLulD

```

Runtime: about 3.2 seconds.


### Faster Version

It examines beforehand the place where the box can not move to the goal.
When a box is pushed there, it doesn't process after that.


```ruby
class Sokoban
  def initialize(level)
    board = level.lines.map(&:rstrip)
    leng  = board.map(&:length).max
    board = board.map{|line| line.ljust(leng)}.join
    @goal = []
    board.each_char.with_index do |c, i|
      @player = i  if c == '@' or c == '+'
      @goal << i   if c == '.' or c == '+' or c == '*'
    end
    @board = board.tr(' .@#$+*', '  @#$ $')
    @lurd = [[-1, 'l', 'L'], [-leng, 'u', 'U'], [1, 'r', 'R'], [leng, 'd', 'D']]
    @dirs = @lurd.map(&:first)
    set_dead_zone(board.tr('^#', ' '))
  end

  def set_dead_zone(wall)
    corner = search_corner(wall)
    @dead = corner.dup
    begin
      size = @dead.size
      corner.each do |pos|
        @dirs.each do |dir|
          next  if wall[pos + dir] == '#'
          @dead.concat(check_side(wall, pos+dir, dir))
        end
      end
    end until size == @dead.size
  end

  def search_corner(wall)
    wall.size.times.with_object([]) do |i, corner|
      next  if wall[i] == '#' or @goal.include?(i)
      case count_wall(wall, i)
      when 2
        corner << i  if wall[i-1] != wall[i+1]
      when 3
        corner << i
      end
    end
  end

  def check_side(wall, pos, dir)
    wk = []
    until wall[pos] == '#' or count_wall(wall, pos) == 0 or @goal.include?(pos)
      return wk if @dead.include?(pos)
      wk << pos
      pos += dir
    end
    []
  end

  def count_wall(wall, pos)
    @dirs.count{|dir| wall[pos + dir] == '#'}
  end

  def push_box(pos, dir, board)
    return board  if board[pos + 2*dir] != ' '
    board[pos        ] = ' '
    board[pos +   dir] = '@'
    board[pos + 2*dir] = '$'
    board
  end

  def solved?(board)
    @goal.all?{|i| board[i] == '$'}
  end

  def solve
    queue = [[@board, "", @player]]
    # When the key doesn't exist in Hash, it subscribes a key but it returns false.
    visited = Hash.new{|h,k| h[k]=true; false}
    visited[@board]                     # first subscription

    until queue.empty?
      board, route, pos = queue.shift
      @lurd.each do |dir, move, push|
        work = board.dup
        case work[pos+dir]
        when '$'    # push
          work = push_box(pos, dir, work)
          next  if visited[work]
          return route+push  if solved?(work)
          queue << [work, route+push, pos+dir]  unless @dead.include?(pos+2*dir)
        when ' '    # move
          work[pos    ] = ' '
          work[pos+dir] = '@'
          next  if visited[work]
          queue << [work, route+move, pos+dir]
        end
      end
    end
    "No solution"
  end
end
```

Runtime: about 0.20 seconds.


## Tcl

This code does a breadth-first search so it finds a solution with a minimum number of moves.
{{trans|OCaml}}<!-- the big difference is that this is all in one procedure for speed as it reduces the amount of packing/unpacking of tuples/lists, and the queue isn't shortened as that's significantly slower for these sorts of board sizes -->

```tcl
package require Tcl 8.5

proc solveSokoban b {
    set cols [string length [lindex $b 0]]
    set dxes [list [expr {-$cols}] $cols -1 1]
    set i 0
    foreach c [split [join $b ""] ""] {
	switch $c {
	    " " {lappend bdc " "}
	    "#" {lappend bdc "#"}
	    "@" {lappend bdc " ";set startplayer $i }
	    "$" {lappend bdc " ";lappend startbox $i}
	    "." {lappend bdc " ";                    lappend targets $i}
	    "+" {lappend bdc " ";set startplayer $i; lappend targets $i}
	    "*" {lappend bdc " ";lappend startbox $i;lappend targets $i}
	}
	incr i
    }
    set q [list [list $startplayer $startbox] {}]
    set store([lindex $q 0]) {}
    for {set idx 0} {$idx < [llength $q]} {incr idx 2} {
	lassign [lindex $q $idx] x boxes
	foreach dir {U D L R} dx $dxes {
	    if {[set x1 [expr {$x + $dx}]] in $boxes} {
		if {[lindex $bdc [incr x1 $dx]] ne " " || $x1 in $boxes} {
		    continue
		}
		set tmpboxes $boxes
		set x1 [expr {$x + $dx}]
		for {set i 0} {$i < [llength $boxes]} {incr i} {
		    if {[lindex $boxes $i] == $x1} {
			lset tmpboxes $i [expr {$x1 + $dx}]
			break
		    }
		}
		if {$dx == 1 || $dx == -1} {
		    set next [list $x1 $tmpboxes]
		} else {
		    set next [list $x1 [lsort -integer $tmpboxes]]
		}
		if {![info exists store($next)]} {
		    if {$targets eq [lindex $next 1]} {
			foreach c [lindex $q [expr {$idx + 1}]] {
			    lassign $c ispush olddir
			    if {$ispush} {
				append solution $olddir
			    } else {
				append solution [string tolower $olddir]
			    }
			}
			return [append solution $dir]
		    }
		    set store($next) {}
		    set nm [lindex $q [expr {$idx + 1}]]
		    lappend q $next
		    lappend q [lappend nm [list 1 $dir]]
		}
	    } elseif {[lindex $bdc $x1] eq " "} {
		set next [list [expr {$x + $dx}] $boxes]
		if {![info exists store($next)]} {
		    set store($next) {}
		    set nm [lindex $q [expr {$idx + 1}]]
		    lappend q $next
		    lappend q [lappend nm [list 0 $dir]]
		}
	    }
	}
    }
    error "no solution"
}
```

Demonstration code:

```tcl
set level {
    "#######"
    "#     #"
    "#     #"
    "#. #  #"
    "#. $$ #"
    "#.$$  #"
    "#.#  @#"
    "#######"
}
puts [solveSokoban $level]
```

Output:
```txt
ulULLulDDurrrddlULrruLLrrUruLLLulD
```

Runtime with stock Tcl 8.5 installation: ≅2.2 seconds<!-- on what is now a fairly old machine -->
