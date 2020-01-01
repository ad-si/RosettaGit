+++
title = "Mastermind"
description = ""
date = 2019-10-19T03:57:53Z
aliases = []
[extra]
id = 21185
[taxonomies]
categories = []
tags = []
+++

[[Category:Puzzles]]

{{task|Games}}

Create a simple version of the board game: [https://en.wikipedia.org/wiki/Mastermind_(board_game) Mastermind].

It must be possible to:
* choose the number of colors will be used in the game(2 - 20)
* choose the color code length(4 - 10)
* choose the maximum number of guesses the player has (7 - 20)
* choose whether or not will be repeated colors in the code



The game should display all the player guesses and the results of that guess.

Display(just an idea.):
{| class="wikitable" border="1"
|-
! Feature !! Graphic Version !! Text Version
|-
| Player guess
| Colored circles
| Alphabet letters
|-
|Correct color & position
|Black circle
|X
|-
|Correct color
|White circle
|O
|-
|None
|Gray circle
| -
|-
|}

A text version example:
1: ADEF  -  XXO-

Translates to:

first guess;

the four colors(ADEF);

result: two correct colors and spot, one correct color/wrong spot one color is not in the code.

Happy coding!


;Related tasks:
*   [[Bulls and cows]]
*   [[Bulls and cows/Player]]
*   [[Guess the number]]
*   [[Guess the number/With Feedback]]






## Ada

{{works with|Ada|Ada|2012}}


```Ada
with Ada.Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;
with Ada.Containers.Ordered_Sets;

use Ada.Strings.Fixed;

procedure MasterMind
is
   subtype Color_Number is Positive range 2 .. 20;
   subtype Code_Size is Positive range 4 .. 10;
   subtype Guesses_Number is Positive range 7 .. 20;
   subtype Color is Character range 'A' .. 'T';

   function Hint(correct, guess : in String) return String
   is
      Xs : Natural := 0;
      Os : Natural := 0;
      to_display : String(1 .. correct'Length) := (others => '-');
   begin
      for I in guess'Range loop
         if guess(I) = correct(I) then
            Xs := Xs + 1;
            to_display(I) := 'X';
         end if;
      end loop;
      for I in guess'Range loop
         if to_display(I) = '-' then
            for J in correct'Range loop
               if J /= I and to_display(J) /= 'X' and correct(J) = guess(I) then
                  Os := Os + 1;
                  exit;
               end if;
            end loop;
         end if;
      end loop;
      return Xs * 'X' & Os * 'O' & (guess'Length - Xs - Os) * '-';
   end Hint;

   generic
      type Data is (<>);
   function Input(message : in String) return Data;
   -- Input will loop until a correct value is given by the user.
   -- For each wrong input, the program will prompt the range of expected values.

   function Input(message : in String) return Data is
   begin
      loop
         Ada.Text_IO.Put(message);
         declare
            S : constant String := Ada.Text_IO.Get_Line;
         begin
            return Data'Value(S);
         exception
            when Constraint_Error =>
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put_Line("Invalid input!");
               Ada.Text_IO.Put_Line
                 ("Expected values in range:"
                  & Data'First'Img & " .." & Data'Last'Img);
               Ada.Text_IO.New_Line;
         end;
      end loop;
   end;

   function Input_Color_Number is new Input(Color_Number);
   function Input_Code_Size is new Input(Code_Size);
   function Input_Guesses_Number is new Input(Guesses_Number);
   function Input_Boolean is new Input(Boolean);

   CN : constant Color_Number := Input_Color_Number("How many colors? ");
   GN : constant Guesses_Number := Input_Guesses_Number("How many guesses? ");
   CS : constant Code_Size := Input_Code_Size("Size of the code? ");
   repeats : Boolean := Input_Boolean("With repeats? ");
   -- Not constant: if Color < Code_Size, we will have repetitions anyway.

   subtype Actual_Colors is Color range Color'First .. Color'Val(Color'Pos(Color'First) + CN - 1);
   package Actual_Colors_Sets is new Ada.Containers.Ordered_Sets(Element_Type => Actual_Colors);

   package Color_Random is new Ada.Numerics.Discrete_Random(Result_Subtype => Actual_Colors);
   generator : Color_Random.Generator;

   function Random return String
   is
      C : String(1 .. CS);
      seen : Actual_Colors_Sets.Set;
   begin
      for I in C'Range loop
         C(I) := Color_Random.Random(generator);
         while (not repeats) and seen.Contains(C(I)) loop
            C(I) := Color_Random.Random(generator);
         end loop;
         seen.Include(C(I));
      end loop;
      return C;
   end Random;

   function Get_Code return String is
   begin
      loop
         Ada.Text_IO.Put("> ");
         declare
            input : constant String := Ada.Text_IO.Get_Line;
         begin
            if input'Length /= CS then
               raise Constraint_Error;
            end if;
            for C of input loop
               if C not in Actual_Colors then
                  raise Constraint_Error;
               end if;
            end loop;
            return input;
         exception
            when Constraint_Error =>
               Ada.Text_IO.New_Line;
               Ada.Text_IO.Put_Line("Invalid input!");
               Ada.Text_IO.New_Line;
         end;
      end loop;
   end Get_Code;

   found : Boolean := False;
begin
   if (not repeats) and (CN < CS) then
      Ada.Text_IO.Put_Line("Not enough colors! Using repeats anyway.");
      repeats := True;
   end if;

   Color_Random.Reset(generator);

   declare
      answer : constant String := Random;
      previous : array(1 .. GN) of String(1 .. CS*2);
   begin
      for I in 1 .. GN loop
         declare
            guess : constant String := Get_Code;
         begin
            if guess = answer then
               Ada.Text_IO.Put_Line("You won, congratulations!");
               found := True;
            else
               previous(I) := guess & Hint(answer, guess);
               Ada.Text_IO.Put_Line(44 * '-');
               for J in 1 .. I loop
                  Ada.Text_IO.Put_Line
                    (previous(J)(1 .. CS)
                     & " => " & previous(J)(CS+1 .. previous(J)'Last));
               end loop;
               Ada.Text_IO.Put_Line(44 * '-');
               Ada.Text_IO.New_Line;
            end if;
         end;
         exit when found;
      end loop;
      if not found then
         Ada.Text_IO.Put_Line("You lost, sorry! The answer was: " & answer);
      end if;
   end;
end MasterMind;

```

{{out}}

```txt
How many colors? 9
How many guesses? 4

Invalid input!
Expected values in range: 7 .. 20

How many guesses? 20
Size of the code? 4
With repeats? False
> ABCD
--------------------------------------------
ABCD => XOO-
--------------------------------------------

> EFGH
--------------------------------------------
ABCD => XOO-
EFGH => O---
--------------------------------------------

> ABCE
--------------------------------------------
ABCD => XOO-
EFGH => O---
ABCE => XOO-
--------------------------------------------

> ABCF
--------------------------------------------
ABCD => XOO-
EFGH => O---
ABCE => XOO-
ABCF => XOO-
--------------------------------------------

> ABCG
--------------------------------------------
ABCD => XOO-
EFGH => O---
ABCE => XOO-
ABCF => XOO-
ABCG => XXOO
--------------------------------------------

> ACBG
You won, congratulations!

```



## C++


```cpp
#include <iostream>
#include <algorithm>
#include <ctime>
#include <string>
#include <vector>

typedef std::vector<char> vecChar;

class master {
public:
    master( size_t code_len, size_t clr_count, size_t guess_count, bool rpt ) {
        std::string color = "ABCDEFGHIJKLMNOPQRST";

        if( code_len < 4 ) code_len = 4; else if( code_len > 10 ) code_len = 10;
        if( !rpt && clr_count < code_len ) clr_count = code_len;
        if( clr_count < 2 ) clr_count = 2; else if( clr_count > 20 ) clr_count = 20;
        if( guess_count < 7 ) guess_count = 7; else if( guess_count > 20 ) guess_count = 20;

        codeLen = code_len; colorsCnt = clr_count; guessCnt = guess_count; repeatClr = rpt;

        for( size_t s = 0; s < colorsCnt; s++ ) {
            colors.append( 1, color.at( s ) );
        }
    }
    void play() {
        bool win = false;
        combo = getCombo();

        while( guessCnt ) {
            showBoard();
            if( checkInput( getInput() ) ) {
                win = true;
                break;
            }
            guessCnt--;
        }
        if( win ) {
            std::cout << "\n\n--------------------------------\n" <<
                "Very well done!\nYou found the code: " << combo <<
                "\n--------------------------------\n\n";
        } else {
            std::cout << "\n\n--------------------------------\n" <<
                "I am sorry, you couldn't make it!\nThe code was: " << combo <<
                "\n--------------------------------\n\n";
        }
    }
private:
    void showBoard() {
        vecChar::iterator y;
        for( int x = 0; x < guesses.size(); x++ ) {
            std::cout << "\n--------------------------------\n";
            std::cout << x + 1 << ": ";
            for( y = guesses[x].begin(); y != guesses[x].end(); y++ ) {
                std::cout << *y << " ";
            }

            std::cout << " :  ";
            for( y = results[x].begin(); y != results[x].end(); y++ ) {
                std::cout << *y << " ";
            }

            int z = codeLen - results[x].size();
            if( z > 0 ) {
                for( int x = 0; x < z; x++ ) std::cout << "- ";
            }
        }
        std::cout << "\n\n";
    }
    std::string getInput() {
        std::string a;
        while( true ) {
            std::cout << "Enter your guess (" << colors << "): ";
            a = ""; std::cin >> a;
            std::transform( a.begin(), a.end(), a.begin(), ::toupper );
            if( a.length() > codeLen ) a.erase( codeLen );
            bool r = true;
            for( std::string::iterator x = a.begin(); x != a.end(); x++ ) {
                if( colors.find( *x ) == std::string::npos ) {
                    r = false;
                    break;
                }
            }
            if( r ) break;
        }
        return a;
    }
    bool checkInput( std::string a ) {
        vecChar g;
        for( std::string::iterator x = a.begin(); x != a.end(); x++ ) {
            g.push_back( *x );
        }
        guesses.push_back( g );

        int black = 0, white = 0;
        std::vector<bool> gmatch( codeLen, false );
        std::vector<bool> cmatch( codeLen, false );

        for( int i = 0; i < codeLen; i++ ) {
            if( a.at( i ) == combo.at( i ) ) {
                gmatch[i] = true;
                cmatch[i] = true;
                black++;
            }
        }

        for( int i = 0; i < codeLen; i++ ) {
            if (gmatch[i]) continue;
            for( int j = 0; j < codeLen; j++ ) {
                if (i == j || cmatch[j]) continue;
                if( a.at( i ) == combo.at( j ) ) {
                    cmatch[j] = true;
                    white++;
                    break;
                }
            }
        }

        vecChar r;
        for( int b = 0; b < black; b++ ) r.push_back( 'X' );
        for( int w = 0; w < white; w++ ) r.push_back( 'O' );
        results.push_back( r );

        return ( black == codeLen );
    }
    std::string getCombo() {
        std::string c, clr = colors;
        int l, z;

        for( size_t s = 0; s < codeLen; s++ ) {
            z = rand() % ( int )clr.length();
            c.append( 1, clr[z] );
            if( !repeatClr ) clr.erase( z, 1 );
        }
        return c;
    }

    size_t codeLen, colorsCnt, guessCnt;
    bool repeatClr;
    std::vector<vecChar> guesses, results;
    std::string colors, combo;
};

int main( int argc, char* argv[] ) {
    srand( unsigned( time( 0 ) ) );
    master m( 4, 8, 12, false );
    m.play();
    return 0;
}

```

{{out}}

```txt

Enter your guess (ABCDEFGH): gbda

--------------------------------
1: A B C D  :  X O O -
--------------------------------
2: A A A E  :  O - - -
--------------------------------
3: E E E E  :  - - - -
--------------------------------
4: B B B C  :  X - - -
--------------------------------
5: D B A F  :  X O O -
--------------------------------
6: G B D A  :  X X X -

Enter your guess (ABCDEFGH): hbda


--------------------------------
Very well done!
You found the code: HBDA
--------------------------------

```



## EasyLang


[https://easylang.online/apps/mastermind.html Run it]

<lang>col[] = [ 902 990 171 229 960 808 ]
len code[] 4
len guess[] 4
#
subr init_vars
  row = 7
.
func draw_rate r black white . .
  for j range 2
    for c range 2
      move c * 3.5 + 76.5 r * 11.5 + 10.4 + j * 3.5
      if black > 0
        color 000
        circle 1.4
        black -= 1
      elif white > 0
        color 999
        circle 1.4
        white -= 1
      else
        color 310
        circle 0.7
      .
    .
  .
.
func show_code . .
  color 531
  move 27 0
  rect 46 8
  for i range 4
    move i * 8 + 33 3
    color col[code[i]]
    circle 2
  .
.
func draw_guess . .
  for c range 4
    move c * 12 + 25 row * 11.5 + 12
    color col[guess[c]]
    circle 3.8
  .
.
func next_row . .
  color 420
  linewidth 11
  move 22 row * 11.5 + 12
  line 65 row * 11.5 + 12
  call draw_guess
  move 78.5 row * 11.5 + 12
  color 310
  circle 5.0
  move 75.7 row * 11.5 + 9.8
  color 753
  text "OK"
.
func rate . .
  move 78.5 row * 11.5 + 12
  color 531
  circle 5.2
  c[] = code[]
  g[] = guess[]
  for i range 4
    if c[i] = g[i]
      black += 1
      c[i] = -1
      g[i] = -2
    .
  .
  for i range 4
    for j range 4
      if c[i] = g[j]
        white += 1
        c[i] = -1
        g[j] = -2
      .
    .
  .
  call draw_rate row black white
  color 531
  linewidth 12
  move 22 row * 11.5 + 12
  line 65 row * 11.5 + 12
  call draw_guess
  row -= 1
  if black = 4
    row = -1
  .
  if row = -1
    call show_code
    timer 1
  else
    call next_row
  .
.
on timer
  row = -2
.
func new . .
  call init_vars
  for i range 4
    code[i] = random 6
  .
  color 531
  move 15 10
  rect 70 80
  linewidth 10
  move 10 5
  line 10 95
  line 90 95
  line 90 5
  line 10 5
  color 310
  linewidth 7
  move 33 3.5
  line 63 3.5
  move 35 1
  color 864
  textsize 4.5
  text "Mastermind"
  color 310
  linewidth 0.5
  move 15 10
  line 15 96
  move 72 10
  line 72 96
  move 85 10
  line 85 96
  for r range 8
    for c range 4
      move c * 12 + 25 r * 11.5 + 12
      circle 2
    .
    call draw_rate r 0 0
  .
  for i range 4
    guess[i] = i
  .
  call next_row
.
func do_move . .
  c = floor ((mouse_x - 20) / 12)
  guess[c] = (guess[c] + 1) mod 6
  call draw_guess
.
on mouse_down
  if row = -2
    call new
  elif mouse_y > row * 11.5 + 7 and mouse_y < row * 11.5 + 17
    if mouse_x > 20 and mouse_x < 66
      call do_move
    elif mouse_x > 72 and mouse_x < 85
      call rate
    .
  .
.
call new
```



## Go



```Go
package main

import (
	"errors"
	"flag"
	"fmt"
	"log"
	"math/rand"
	"strings"
	"time"
)

func main() {
	log.SetPrefix("mastermind: ")
	log.SetFlags(0)
	colours := flag.Int("colours", 6, "number of colours to use (2-20)")
	flag.IntVar(colours, "colors", 6, "alias for colours")
	holes := flag.Int("holes", 4, "number of holes (the code length, 4-10)")
	guesses := flag.Int("guesses", 12, "number of guesses allowed (7-20)")
	unique := flag.Bool("unique", false, "disallow duplicate colours in the code")
	flag.Parse()

	rand.Seed(time.Now().UnixNano())
	m, err := NewMastermind(*colours, *holes, *guesses, *unique)
	if err != nil {
		log.Fatal(err)
	}
	err = m.Play()
	if err != nil {
		log.Fatal(err)
	}
}

type mastermind struct {
	colours int
	holes   int
	guesses int
	unique  bool

	code   string
	past   []string // history of guesses
	scores []string // history of scores
}

func NewMastermind(colours, holes, guesses int, unique bool) (*mastermind, error) {
	if colours < 2 || colours > 20 {
		return nil, errors.New("colours must be between 2 and 20 inclusive")
	}
	if holes < 4 || holes > 10 {
		return nil, errors.New("holes must be between 4 and 10 inclusive")
	}
	if guesses < 7 || guesses > 20 {
		return nil, errors.New("guesses must be between 7 and 20 inclusive")
	}
	if unique && holes > colours {
		return nil, errors.New("holes must be > colours when using unique")
	}

	return &mastermind{
		colours: colours,
		holes:   holes,
		guesses: guesses,
		unique:  unique,
		past:    make([]string, 0, guesses),
		scores:  make([]string, 0, guesses),
	}, nil
}

func (m *mastermind) Play() error {
	m.generateCode()
	fmt.Printf("A set of %s has been selected as the code.\n", m.describeCode(m.unique))
	fmt.Printf("You have %d guesses.\n", m.guesses)
	for len(m.past) < m.guesses {
		guess, err := m.inputGuess()
		if err != nil {
			return err
		}
		fmt.Println()
		m.past = append(m.past, guess)
		str, won := m.scoreString(m.score(guess))
		if won {
			plural := "es"
			if len(m.past) == 1 {
				plural = ""
			}
			fmt.Printf("You found the code in %d guess%s.\n", len(m.past), plural)
			return nil
		}
		m.scores = append(m.scores, str)
		m.printHistory()
		fmt.Println()
	}
	fmt.Printf("You are out of guesses. The code was %s.\n", m.code)
	return nil
}

const charset = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
const blacks = "XXXXXXXXXX"
const whites = "OOOOOOOOOO"
const nones = "----------"

func (m *mastermind) describeCode(unique bool) string {
	ustr := ""
	if unique {
		ustr = " unique"
	}
	return fmt.Sprintf("%d%s letters (from 'A' to %q)",
		m.holes, ustr, charset[m.colours-1],
	)
}

func (m *mastermind) printHistory() {
	for i, g := range m.past {
		fmt.Printf("-----%s---%[1]s--\n", nones[:m.holes])
		fmt.Printf("%2d:  %s : %s\n", i+1, g, m.scores[i])
	}
}

func (m *mastermind) generateCode() {
	code := make([]byte, m.holes)
	if m.unique {
		p := rand.Perm(m.colours)
		for i := range code {
			code[i] = charset[p[i]]
		}
	} else {
		for i := range code {
			code[i] = charset[rand.Intn(m.colours)]
		}
	}
	m.code = string(code)
	//log.Printf("code is %q", m.code)
}

func (m *mastermind) inputGuess() (string, error) {
	var input string
	for {
		fmt.Printf("Enter guess #%d: ", len(m.past)+1)
		if _, err := fmt.Scanln(&input); err != nil {
			return "", err
		}
		input = strings.ToUpper(strings.TrimSpace(input))
		if m.validGuess(input) {
			return input, nil
		}
		fmt.Printf("A guess must consist of %s.\n", m.describeCode(false))
	}
}

func (m *mastermind) validGuess(input string) bool {
	if len(input) != m.holes {
		return false
	}
	for i := 0; i < len(input); i++ {
		c := input[i]
		if c < 'A' || c > charset[m.colours-1] {
			return false
		}
	}
	return true
}

func (m *mastermind) score(guess string) (black, white int) {
	scored := make([]bool, m.holes)
	for i := 0; i < len(guess); i++ {
		if guess[i] == m.code[i] {
			black++
			scored[i] = true
		}
	}
	for i := 0; i < len(guess); i++ {
		if guess[i] == m.code[i] {
			continue
		}
		for j := 0; j < len(m.code); j++ {
			if i != j && !scored[j] && guess[i] == m.code[j] {
				white++
				scored[j] = true
			}
		}
	}
	return
}

func (m *mastermind) scoreString(black, white int) (string, bool) {
	none := m.holes - black - white
	return blacks[:black] + whites[:white] + nones[:none], black == m.holes
}
```

{{out|note=using Knuth's five-guess algorithm}}

```txt

A set of 4 letters (from 'A' to 'F') has been selected as the code.
You have 12 guesses.
[... output of first guesses omitted ...]
Enter guess #4: FEDE

------------------
 1:  AABB : ----
------------------
 2:  FFED : XOOO
------------------
 3:  DFDF : XO--
------------------
 4:  FEDE : XOOO

Enter guess #5: EFFE

You found the code in 5 guesses.

```



## Javascript

You can try it [http://paulo-jorente.de/webgames/repos/mastermind/ here].


```javascript

class Mastermind {
  constructor() {
    this.colorsCnt;
    this.rptColors;
    this.codeLen;
    this.guessCnt;
    this.guesses;
    this.code;
    this.selected;
    this.game_over;
    this.clear = (el) => {
      while (el.hasChildNodes()) {
        el.removeChild(el.firstChild);
      }
    };
    this.colors = ["🤡", "👹", "👺", "👻", "👽", "👾", "🤖", "🐵", "🐭",
      "🐸", "🎃", "🤠", "☠️", "🦄", "🦇", "🛸", "🎅", "👿", "🐲", "🦋"
    ];
  }

  newGame() {
    this.selected = null;
    this.guessCnt = parseInt(document.getElementById("gssCnt").value);
    this.colorsCnt = parseInt(document.getElementById("clrCnt").value);
    this.codeLen = parseInt(document.getElementById("codeLen").value);
    if (this.codeLen > this.colorsCnt) {
      document.getElementById("rptClr").selectedIndex = 1;
    }
    this.rptColors = document.getElementById("rptClr").value === "yes";
    this.guesses = 0;
    this.game_over = false;
    const go = document.getElementById("gameover");
    go.innerText = "";
    go.style.visibility = "hidden";
    this.clear(document.getElementById("code"));
    this.buildPalette();
    this.buildPlayField();
  }

  buildPalette() {
    const pal = document.getElementById("palette"),
      z = this.colorsCnt / 5,
      h = Math.floor(z) != z ? Math.floor(z) + 1 : z;
    this.clear(pal);
    pal.style.height = `${44 * h + 3 * h}px`;
    const clrs = [];
    for (let c = 0; c < this.colorsCnt; c++) {
      clrs.push(c);
      const b = document.createElement("div");
      b.className = "bucket";
      b.clr = c;
      b.innerText = this.colors[c];
      b.addEventListener("click", () => {
        this.palClick(b);
      });
      pal.appendChild(b);
    }
    this.code = [];
    while (this.code.length < this.codeLen) {
      const r = Math.floor(Math.random() * clrs.length);
      this.code.push(clrs[r]);
      if (!this.rptColors) {
        clrs.splice(r, 1);
      }
    }
  }

  buildPlayField() {
    const brd = document.getElementById("board");
    this.clear(brd);
    const w = 49 * this.codeLen + 7 * this.codeLen + 5;
    brd.active = 0;
    brd.style.width = `${w}px`;
    document.querySelector(".column").style.width = `${w + 20}px`;
    this.addGuessLine(brd);
  }

  addGuessLine(brd) {
    const z = document.createElement("div");
    z.style.clear = "both";
    brd.appendChild(z);
    brd.active += 10;
    for (let c = 0; c < this.codeLen; c++) {
      const d = document.createElement("div");
      d.className = "bucket";
      d.id = `brd${brd.active+ c}`;
      d.clr = -1;
      d.addEventListener("click", () => {
        this.playClick(d);
      })
      brd.appendChild(d);
    }
  }

  palClick(bucket) {
    if (this.game_over) return;
    if (null === this.selected) {
      bucket.classList.add("selected");
      this.selected = bucket;
      return;
    }
    if (this.selected !== bucket) {
      this.selected.classList.remove("selected");
      bucket.classList.add("selected");
      this.selected = bucket;
      return;
    }
    this.selected.classList.remove("selected");
    this.selected = null;
  }

  vibrate() {
    const brd = document.getElementById("board");
    let timerCnt = 0;
    const exp = setInterval(() => {
      if ((timerCnt++) > 60) {
        clearInterval(exp);
        brd.style.top = "0px";
        brd.style.left = "0px";
      }
      let x = Math.random() * 4,
        y = Math.random() * 4;
      if (Math.random() < .5) x = -x;
      if (Math.random() < .5) y = -y;
      brd.style.top = y + "px";
      brd.style.left = x + "px";
    }, 10);
  }

  playClick(bucket) {
    if (this.game_over) return;
    if (this.selected) {
      bucket.innerText = this.selected.innerText;
      bucket.clr = this.selected.clr;
    } else {
      this.vibrate();
    }
  }

  check() {
    if (this.game_over) return;
    let code = [];
    const brd = document.getElementById("board");
    for (let b = 0; b < this.codeLen; b++) {
      const h = document.getElementById(`brd${brd.active + b}`).clr;
      if (h < 0) {
        this.vibrate();
        return;
      }
      code.push(h);
    }
    this.guesses++;
    if (this.compareCode(code)) {
      this.gameOver(true);
      return;
    }
    if (this.guesses >= this.guessCnt) {
      this.gameOver(false);
      return;
    }
    this.addGuessLine(brd);
  }

  compareCode(code) {
    let black = 0,
      white = 0,
      b_match = new Array(this.codeLen).fill(false),
      w_match = new Array(this.codeLen).fill(false);
    for (let i = 0; i < this.codeLen; i++) {
      if (code[i] === this.code[i]) {
        b_match[i] = true;
        w_match[i] = true;
        black++;
      }
    }
    for (let i = 0; i < this.codeLen; i++) {
      if (b_match[i]) continue;
      for (let j = 0; j < this.codeLen; j++) {
        if (i == j || w_match[j]) continue;
        if (code[i] === this.code[j]) {
          w_match[j] = true;
          white++;
          break;
        }
      }
    }
    const brd = document.getElementById("board");
    let d;
    for (let i = 0; i < black; i++) {
      d = document.createElement("div");
      d.className = "pin";
      d.style.backgroundColor = "#a00";
      brd.appendChild(d);
    }
    for (let i = 0; i < white; i++) {
      d = document.createElement("div");
      d.className = "pin";
      d.style.backgroundColor = "#eee";
      brd.appendChild(d);
    }
    return (black == this.codeLen);
  }

  gameOver(win) {
    if (this.game_over) return;
    this.game_over = true;
    const cd = document.getElementById("code");
    for (let c = 0; c < this.codeLen; c++) {
      const d = document.createElement("div");
      d.className = "bucket";
      d.innerText = this.colors[this.code[c]];
      cd.appendChild(d);
    }
    const go = document.getElementById("gameover");
    go.style.visibility = "visible";
    go.innerText = win ? "GREAT!" : "YOU FAILED!";
    const i = setInterval(() => {
      go.style.visibility = "hidden";
      clearInterval(i);
    }, 3000);
  }
}
const mm = new Mastermind();
document.getElementById("newGame").addEventListener("click", () => {
  mm.newGame()
});
document.getElementById("giveUp").addEventListener("click", () => {
  mm.gameOver();
});
document.getElementById("check").addEventListener("click", () => {
  mm.check()
});

```


To test you'll need a HTML file

```txt

<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <meta http-equiv="X-UA-Compatible" content="ie=edge">
  <link rel="stylesheet" href="./css/style.css" type="text/css">
  <title>Mastermind</title>
</head>
<body>
  <div id="gameover"></div>
  <div class="column">
    <div id="board" style="position: relative"></div>
    <div style="clear: both"> </div>
    <button id="check" style="margin-top:20px">Check!</button>
    <button id="giveUp">Give up!</button>
    <div id="code" style="margin-top: 40px"></div>
  </div>
  <div class="columnS">
    <div id="ctrls">
      <label for="clrCnt">Colors count:</label>
      <input type="number" id="clrCnt" max="20" min="2" value="6"></input>
      <br />
      <label for="codeLen">Code length:</label>
      <input type="number" id="codeLen" max="10" min="4" value="4"></input>
      <br />
      <label for="gssCnt">Guesses count:</label>
      <input type="number" id="gssCnt" max="20" min="7" value="12"></input>
      <br />
      <label for="rptClr">Repeat colors:</label>
      <select id="rptClr">
        <option value="no">No</option>
        <option value="yes">Yes</option>
      </select>
      <button id="newGame" style="width: 100%">New game</button>
    </div>
    <div id="palette"></div>
  </div>
  <script src="./src/index.js" type="module"></script>
</body>
</html>

```


And a CSS file

```txt

* {
  margin: 0;
  padding: 0;
  box-sizing: border-box;
  user-select: none;
}
body {
  background-color: #222;
  text-align: left
}
button {
  width: 101px;
  height: 26px;
  background-color: #444;
  color: #ddd;
  border: 1px solid #333;
  border-radius: 4px;
}
input,
select {
  float: right;
  width: 60px;
  height: 24px;
  margin-top: 6px;
  font-size: 20px;
}
#palette {
  margin-top: 40px;
  width: 233px;
  border-radius: 5px;
  border: 1px solid #fff;
}
#ctrls {
  border-radius: 5px;
  border: 1px solid #fff;
  padding: 10px;
  font-size: 20px;
  width: 233px;
  line-height: 36px;
  color: #fff;
}
#gameover {
  position: absolute;
  top: 90px;
  left: 0;
  font-size: 100px;
  text-align: center;
  color: rgb(248, 143, 4);
  background: rgba(34, 34, 34, .8);
  padding: 10px;
  z-index: 999;
}
.column,
.columnS {
  float: left;
  width: 249px;
  margin: 30px;
}
.columnS {
  width: 300px;
  margin-left: 0;
}
.bucket {
  float: left;
  width: 42px;
  height: 42px;
  line-height: 40px;
  font-size: 26px;
  border: 2px solid #fff;
  border-radius: 3px;
  margin: 2px;
  cursor: pointer;
}
.selected {
  border: 2px solid #d00;
}
.pin {
  float: left;
  width: 14px;
  height: 14px;
  border-radius: 50%;
  margin: 2px;
}

```



## Julia

GUI version, uses the Gtk toolkit.

```julia
using Gtk, Colors, Cairo, Graphics

struct Guess
    code::Vector{Color}
    guess::Vector{Color}
    hint::Vector{Color}
end

function Guess(code, guess)
    len = length(code)
    hints = fill(colorant"gray", len) # gray default
    for (i, g) in enumerate(guess), (j, c) in enumerate(code)
        if g == c
            if i == j
                hints[i] = colorant"black"
            elseif hints[i] != colorant"black"
                hints[i] = colorant"white"
            end
        end
    end
    g = Guess([c for c in code], [c for c in guess], [c for c in hints])
end

tovec(guess) = [x for x in [guess.code; guess.guess; guess.hint]]

function mastermindapp()
    allu(s) = length(unique(s)) == length(s)
    ccode(c, n, rok) = while true a = rand(c, n); if rok || allu(a) return a end end

    numcolors, codelength, maxguesses, allowrep, gameover = 10, 4, 10, false, false
    colors = distinguishable_colors(numcolors)
    code = ccode(colors, numcolors, allowrep)
    guesshistory = Vector{Guess}()

    win = GtkWindow("Mastermind Game", 900, 750) |> (GtkFrame() |> (box = GtkBox(:v)))
    settingsbox = GtkBox(:h)
    playtoolbar = GtkToolbar()

    setcolors = GtkScale(false, 4:20)
    set_gtk_property!(setcolors, :hexpand, true)
    adj = GtkAdjustment(setcolors)
    set_gtk_property!(adj, :value, 10)
    clabel = GtkLabel("Number of Colors")

    setcodelength = GtkScale(false, 4:10)
    set_gtk_property!(setcodelength, :hexpand, true)
    adj = GtkAdjustment(setcodelength)
    set_gtk_property!(adj, :value, 4)
    slabel = GtkLabel("Code Length")

    setnumguesses = GtkScale(false, 4:40)
    set_gtk_property!(setnumguesses, :hexpand, true)
    adj = GtkAdjustment(setnumguesses)
    set_gtk_property!(adj, :value, 10)
    nlabel = GtkLabel("Max Guesses")

    allowrepeatcolor = GtkScale(false, 0:1)
    set_gtk_property!(allowrepeatcolor, :hexpand, true)
    rlabel = GtkLabel("Allow Repeated Colors (0 = No)")

    newgame = GtkToolButton("New Game")
    set_gtk_property!(newgame, :label, "New Game")
    set_gtk_property!(newgame, :is_important, true)

    tryguess = GtkToolButton("Submit Current Guess")
    set_gtk_property!(tryguess, :label, "Submit Current Guess")
    set_gtk_property!(tryguess, :is_important, true)

    eraselast = GtkToolButton("Erase Last (Unsubmitted) Pick")
    set_gtk_property!(eraselast, :label, "Erase Last (Unsubmitted) Pick")
    set_gtk_property!(eraselast, :is_important, true)

    map(w->push!(settingsbox, w),[clabel, setcolors, slabel, setcodelength,
        nlabel, setnumguesses, rlabel, allowrepeatcolor])
    map(w->push!(playtoolbar, w),[newgame, tryguess, eraselast])

    scrwin = GtkScrolledWindow()
    can = GtkCanvas()
    set_gtk_property!(can, :expand, true)
    map(w -> push!(box, w),[settingsbox, playtoolbar, scrwin])
    push!(scrwin, can)

    currentguess = RGB[]
    guessesused = 0
    colorpositions = Point[]

    function newgame!(w)
        empty!(guesshistory)

        numcolors = Int(GAccessor.value(setcolors))
        codelength = Int(GAccessor.value(setcodelength))
        maxguesses = Int(GAccessor.value(setnumguesses))
        allowrep = Int(GAccessor.value(allowrepeatcolor))

        colors = distinguishable_colors(numcolors)
        code = ccode(colors, codelength, allowrep == 1)
        empty!(currentguess)
        currentneeded = codelength
        guessesused = 0
        gameover = false
        draw(can)
    end
    signal_connect(newgame!, newgame, :clicked)

    function saywon!()
        warn_dialog("You have WON the game!", win)
        gameover = true
    end

    function outofguesses!()
        warn_dialog("You have Run out of moves! Game over.", win)
        gameover = true
    end

    can.mouse.button1press = @guarded (widget, event) -> begin
        if !gameover && (i = findfirst(p ->
            sqrt((p.x - event.x)^2 + (p.y - event.y)^2) < 20,
                colorpositions)) != nothing
            if length(currentguess) < codelength
                if allowrep == 0 && !allu(currentguess)
                    warn_dialog("Please erase the duplicate color.", win)
                else
                    push!(currentguess, colors[i])
                    draw(can)
                end
            else
                warn_dialog("You need to submit this guess if ready.", win)
            end
        end
    end

    @guarded draw(can) do widget
        ctx = Gtk.getgc(can)
        select_font_face(ctx, "Courier", Cairo.FONT_SLANT_NORMAL, Cairo.FONT_WEIGHT_BOLD)
        fontpointsize = 12
        set_font_size(ctx, fontpointsize)
        workcolor = colorant"black"
        set_source(ctx, workcolor)
        move_to(ctx, 0, fontpointsize)
        show_text(ctx, "Color options: " * "-"^70)
        stroke(ctx)
        empty!(colorpositions)
        for i in 1:numcolors
            set_source(ctx, colors[i])
            circle(ctx, i * 40, 40, 20)
            push!(colorpositions, Point(i * 40, 40))
            fill(ctx)
        end
        set_gtk_property!(can, :expand, false) # kludge for good text overwriting
        move_to(ctx, 0, 80)
        set_source(ctx, workcolor)
        show_text(ctx, string(maxguesses - guessesused, pad = 2) * " moves remaining.")
        stroke(ctx)
        set_gtk_property!(can, :expand, true)
        for i in 1:codelength
            set_source(ctx, i > length(currentguess) ? colorant"lightgray" : currentguess[i])
            circle(ctx, i * 40, 110, 20)
            fill(ctx)
        end
        if length(guesshistory) > 0
            move_to(ctx, 0, 155)
            set_source(ctx, workcolor)
            show_text(ctx, "Past Guesses: " * "-"^70)
            for (i, g) in enumerate(guesshistory), (j, c) in enumerate(tovec(g)[codelength+1:end])
                x = j * 40 + (j > codelength ? 20 : 0)
                y = 150 + 40 * i
                set_source(ctx, c)
                circle(ctx, x, y, 20)
                fill(ctx)
            end
        end
        Gtk.showall(win)
    end

    function submitguess!(w)
        if length(currentguess) == length(code)
            g = Guess(code, currentguess)
            push!(guesshistory, g)
            empty!(currentguess)
            guessesused += 1
            draw(can)
            if all(i -> g.code[i] == g.guess[i], 1:length(code))
                saywon!()
            elseif guessesused > maxguesses
                outofguesses!()
            end
        end
    end
    signal_connect(submitguess!, tryguess, :clicked)

    function undolast!(w)
        if length(currentguess) > 0
            pop!(currentguess)
            draw(can)
        end
    end
    signal_connect(undolast!, eraselast, :clicked)

    newgame!(win)
    Gtk.showall(win)

    condition = Condition()
    endit(w) = notify(condition)
    signal_connect(endit, win, :destroy)
    showall(win)
    wait(condition)
end

mastermindapp()

```



## Kotlin

{{trans|C++}}

```scala
// version 1.2.51

import java.util.Random

val rand = Random()

class Mastermind {
    private val codeLen: Int
    private val colorsCnt: Int
    private var guessCnt = 0
    private val repeatClr: Boolean

    private val colors: String
    private var combo = ""

    private val guesses = mutableListOf<CharArray>()
    private val results = mutableListOf<CharArray>()

    constructor(codeLen: Int, colorsCnt: Int, guessCnt: Int, repeatClr: Boolean) {
        val color = "ABCDEFGHIJKLMNOPQRST"
        this.codeLen = codeLen.coerceIn(4, 10)
        var cl = colorsCnt
        if (!repeatClr && cl < this.codeLen) cl = this.codeLen
        this.colorsCnt = cl.coerceIn(2, 20)
        this.guessCnt = guessCnt.coerceIn(7, 20)
        this.repeatClr = repeatClr
        this.colors = color.take(this.colorsCnt)
    }

    fun play() {
        var win = false
        combo = getCombo()
        while (guessCnt != 0) {
            showBoard()
            if (checkInput(getInput())) {
                win = true
                break
            }
            guessCnt--
        }
        println("\n\n--------------------------------")
        if (win) {
            println("Very well done!\nYou found the code: $combo")
        }
        else {
            println("I am sorry, you couldn't make it!\nThe code was: $combo")
        }
        println("--------------------------------\n")
    }

    private fun showBoard() {
        for (x in 0 until guesses.size) {
            println("\n--------------------------------")
            print("${x + 1}: ")
            for (y in guesses[x]) print("$y ")
            print(" :  ")
            for (y in results[x]) print("$y ")
            val z = codeLen - results[x].size
            if (z > 0) print("- ".repeat(z))
        }
        println("\n")
    }

    private fun getInput(): String {
        while (true) {
            print("Enter your guess ($colors): ")
            val a = readLine()!!.toUpperCase().take(codeLen)
            if (a.all { it in colors } ) return a
        }
    }

    private fun checkInput(a: String): Boolean {
        guesses.add(a.toCharArray())
        var black = 0
        var white = 0
        val gmatch = BooleanArray(codeLen)
        val cmatch = BooleanArray(codeLen)
        for (i in 0 until codeLen) {
            if (a[i] == combo[i]) {
                gmatch[i] = true
                cmatch[i] = true
                black++
            }
        }
        for (i in 0 until codeLen) {
            if (gmatch[i]) continue
            for (j in 0 until codeLen) {
                if (i == j || cmatch[j]) continue
                if (a[i] == combo[j]) {
                    cmatch[j] = true
                    white++
                    break
                }
            }
        }
        val r = mutableListOf<Char>()
        r.addAll("X".repeat(black).toList())
        r.addAll("O".repeat(white).toList())
        results.add(r.toCharArray())
        return black == codeLen
    }

    private fun getCombo(): String {
        val c =  StringBuilder()
        val clr = StringBuilder(colors)
        for (s in 0 until codeLen) {
            val z = rand.nextInt(clr.length)
            c.append(clr[z])
            if (!repeatClr) clr.deleteCharAt(z)
        }
        return c.toString()
    }
}

fun main(args: Array<String>) {
    val m = Mastermind(4, 8, 12, false)
    m.play()
}
```


Sample input/output (showing last 2 guesses only):

```txt

Enter your guess (ABCDEFGH): hcgb

--------------------------------
1: A B C D  :  O - - -
--------------------------------
2: E F G H  :  X O O -
--------------------------------
3: E G F A  :  O O - -
--------------------------------
4: G F E A  :  O O - -
--------------------------------
5: F H G B  :  X X O -
--------------------------------
6: F B G C  :  X O - -
--------------------------------
7: F G A B  :  X O - -
--------------------------------
8: A H G C  :  X O - -
--------------------------------
9: G H D B  :  X O O -
--------------------------------
10: H A G B  :  X X X -
--------------------------------
11: H C G B  :  X X X -

Enter your guess (ABCDEFGH): hegb


--------------------------------
Very well done!
You found the code: HEGB
--------------------------------

```



## Lua

Based on C++

```lua

math.randomseed( os.time() )
local black, white, none, code = "X", "O", "-"
local colors, codeLen, maxGuess, rept, alpha, opt = 6, 4, 10, false, "ABCDEFGHIJKLMNOPQRST", ""
local guesses, results
function createCode()
    code = ""
    local dic, a = ""
    for i = 1, colors do
        dic = dic .. alpha:sub( i, i )
    end
    for i = 1, codeLen do
        a = math.floor( math.random( 1, #dic ) )
        code = code .. dic:sub( a, a )
        if not rept then
            dic = dic:sub(1, a - 1 ) .. dic:sub( a + 1, #dic )
        end
    end
end
function checkInput( inp )
    table.insert( guesses, inp )
    local b, w, fnd, str = 0, 0, {}, ""
    for bl = 1, codeLen do
        if inp:sub( bl, bl ) == code:sub( bl, bl ) then
            b = b + 1; fnd[bl] = true
        else
            for wh = 1, codeLen do
                if nil == fnd[bl] and wh ~= bl and inp:sub( wh, wh ) == code:sub( bl, bl ) then
                    w = w + 1; fnd[bl] = true
                end
            end
        end
    end
    for i = 1, b do str = str .. string.format( "%s ", black ) end
    for i = 1, w do str = str .. string.format( "%s ", white ) end
    for i = 1, 2 * codeLen - #str, 2 do str = str .. string.format( "%s ", none ) end
    table.insert( results, str )
    return b == codeLen
end
function play()
    local err, win, r = true, false;
    for j = 1, colors do opt = opt .. alpha:sub( j, j ) end
    while( true ) do
        createCode(); guesses, results = {}, {}
        for i = 1, maxGuess do
            err = true;
            while( err ) do
                io.write( string.format( "\n-------------------------------\nYour guess (%s)?", opt ) )
                inp = io.read():upper();
                if #inp == codeLen then
                    err = false;
                    for k = 1, #inp do
                        if( nil == opt:find( inp:sub( k, k ) ) ) then
                            err = true;
                            break;
                        end
                    end
                end
            end
            if( checkInput( inp ) ) then win = true; break
            else
                for l = 1, #guesses do
                    print( string.format( "%.2d: %s : %s", l, guesses[l], results[l] ) )
                end
            end
        end
        if win then print( "\nWell done!" )
        else print( string.format( "\nSorry, you did not crack the code --> %s!", code ) )
        end
        io.write( "Play again( Y/N )? " ); r = io.read()
        if r ~= "Y" and r ~= "y" then break end
    end
end
--[[ entry point ]]---
if arg[1] ~= nil and tonumber( arg[1] ) > 1 and tonumber( arg[1] ) < 21 then colors = tonumber( arg[1] ) end
if arg[2] ~= nil and tonumber( arg[2] ) > 3 and tonumber( arg[2] ) < 11 then codeLen = tonumber( arg[2] ) end
if arg[3] ~= nil and tonumber( arg[3] ) > 6 and tonumber( arg[3] ) < 21 then maxGuess = tonumber( arg[3] ) end
if arg[4] ~= nil and arg[4] == "true" or arg[4] == "false" then rept = ( arg[4] == "true" ) end
play()

```

{{out}}

```txt

-------------------------------
Your guess (ABCDEF)?bcde
01: BCDE : X X - -

-------------------------------
Your guess (ABCDEF)?bcaf
01: BCDE : X X - -
02: BCAF : X O O -

-------------------------------
Your guess (ABCDEF)?badf
01: BCDE : X X - -
02: BCAF : X O O -
03: BADF : X X O -

-------------------------------
Your guess (ABCDEF)?bafe

Well done!
Play again( Y/N )?

```



## Perl

{{trans|Perl 6}}

```perl
use List::Util qw(any);

print 'Enter pool size, puzzle size, attempts allowed: ';
($pool,$length,$tries) = split /\s+/, <>;
$length =  4 if $length eq '' or $length < 3 or $length > 11;
$pool   =  6 if $pool   eq '' or $pool   < 2 or $pool   > 21;
$tries  = 10 if $tries  eq '' or $tries  < 7 or $tries  > 21;

@valid  = sort { -1 + 2*int(rand 2) } ('A' .. 'T')[0..$pool-1];
@puzzle = @valid[0..$length-1];

$black = '●';
$white = '○';

while () {
    header();
    print "$_\n" for @guesses;
    lose() if  @guesses == $tries;
    @guess = get_guess();
    next unless is_valid(@guess);
    $score = score(\@puzzle, \@guess);
    win() if $score eq join ' ', ($black) x $length;
    push @guesses, join(' ', @guess) . ' :: ' . $score;
}

sub score {
    local *puzzle = shift;
    local *guess  = shift;
    my @score;
    for $i (0..$length-1) {
        if    (     $puzzle[$i] eq $guess[$i]) { push @score, $black }
        elsif (any {$puzzle[$i] eq $_} @guess) { push @score, $white }
        else                                   { push @score, '-'    }
    }
    join ' ', reverse sort @score;
}

sub header {
    $num = $tries - @guesses;
    print  "Valid letter, but wrong position: ○ - Correct letter and position: ●\n";
    print  "Guess the $length element sequence containing the letters " . join(', ', sort @valid) . "\n";
    printf "Repeats are not allowed. You have $num guess%s remaining\n", $num > 1 ? 'es' : '';
}

sub get_guess { print 'Your guess?: '; $g = <>; return split /\s*/, uc $g }

sub is_valid { $length == @_ }

sub win  { print 'You win! The correct answer is: ' . join(' ',@puzzle) . "\n"; exit }

sub lose { print 'Too bad, you ran out of guesses. The solution was: ' . join(' ',@puzzle) . "\n"; exit }

```

Sample output, omitting redundant instructions.
{{out}}
<pre style="height:35ex">Valid letter, but wrong position: ○ - Correct letter and position: ●
Guess the 4 element sequence containing the letters A, B, C, D, E, F
Repeats are not allowed. You have 10 guesses remaining
Your guess?: a b c e
A B C E :: ○ ○ ○ -
Your guess?: b a d c
Repeats are not allowed. You have 8 guesses remaining
A B C E :: ○ ○ ○ -
B A D C :: ● ● ○ ○
Your guess?: a b c d
Repeats are not allowed. You have 7 guesses remaining
A B C E :: ○ ○ ○ -
B A D C :: ● ● ○ ○
A B C D :: ○ ○ ○ ○
Your guess?: b d a c
You win! The correct answer is: B D A C
```



## Perl 6

{{works with|Rakudo|2017.01}}
By default, plays classic Mastermind using letters in place of colors. ( 4 chosen from 6, no repeats, 10 guess limit. ) Pass in parameters to modify the game. Enter a string of --length (default 4) letters with or without spaces. Guesses accept lower or upper case.

```perl6
sub MAIN (
    Int :$colors  where 1 < * < 21 = 6,  Int :$length  where 3 < * < 11 = 4,
    Int :$guesses where 7 < * < 21 = 10, Bool :$repeat = False
  ) {
    my @valid = ('A' .. 'T')[^$colors];
    my $puzzle = $repeat ?? @valid.roll($length) !! @valid.pick($length);
    my @guesses;

    my $black = '●';
    my $white = '○';

    loop {
        clearscr();
        say header();
        printf " %{$length * 2}s :: %s\n", @guesses[$_][0], @guesses[$_][1] for ^@guesses;
        say '';
        lose() if @guesses == $guesses;
        my $guess = get-guess();
        next unless $guess.&is-valid;
        my $score = score($puzzle, $guess);
        win() if $score eq ($black xx $length).join: ' ';
        @guesses.push: [$guess, $score];
    }

    sub header {
        my $num = $guesses - @guesses;
        qq:to/END/;
        Valid letter, but wrong position: ○ - Correct letter and position: ●
        Guess the {$length} element sequence containing the letters {@valid}
        Repeats are {$repeat ?? '' !! 'not '}allowed. You have $num guess{ $num == 1 ?? '' !! 'es'} remaining.
        END
    }

    sub score ($puzzle, $guess) {
        my @score;
        for ^$length {
            if $puzzle[$_] eq $guess[$_] {
                @score.push: $black;
            }
            elsif $puzzle[$_] eq any(@$guess) {
                @score.push: $white;
            }
            else {
                @score.push('-');
            }
        }
        @score.sort.reverse.join: ' ';
    }

    sub clearscr { $*KERNEL ~~ /'win32'/ ?? run('cls') !! run('clear') }

    sub get-guess { (uc prompt 'Your guess?: ').comb(/@valid/) }

    sub is-valid (@guess) { so $length == @guess }

    sub win  { say 'You Win! The correct answer is: ', $puzzle; exit }

    sub lose { say 'Too bad, you ran out of guesses. The solution was: ', $puzzle; exit }
}
```

{{out|Sample output}}

```txt
Valid letter, but wrong position: ○ - Correct letter and position: ●
Guess the 4 element sequence containing the letters A B C D E F
Repeats are not allowed. You have 5 guesses remaining.

  A B C D :: ○ ○ ○ -
  C A B E :: ● ○ ○ -
  D A E F :: ● ○ - -
  B A E C :: ● ○ ○ -
  D E B C :: ○ ○ ○ ○

Your guess?: cdeb
You Win! The correct answer is: (C D E B)
```



## Phix

OTT GUI solution, fully configurable with solver.

```Phix
-- demo/rosetta/mastermind.exw
constant SET_LIMIT = 1_000_000  -- above this, it uses random sampling.

constant help_text = """
The game of mastermind, with a Knuth solver.

Specify the number of colours (1..20), the code length (1..10), the
number of guesses allowed (1-20), and whether colours can be repeated
(auto-ticked & greyed-out inactive when length>colours).

Note that at the highest settings there are 10,240,000,000,000 possible
answers: the (order n squared) analysis of that is simply not practical,
as indeed is simply building the initial list of all possible answers,
and therefore a fixed limit of 1,000,000 has been applied, which also
just about manages to keep the program responsive. Obviously, should the
actual answer not be among those samples, it cannot possibly find it,
and it will tell you in plain english when that occurs. You can always
trim the search space back to something more reasonable at any time, and
still play the game when that limit is breached, with weaker hints.

Conversely the lowest settings do not make for an interesting game, but
proved quite useful when ironing out some bugs, so were left in.

The Use button (disabled until something useful found) allows you to
take the best found (so far), displayed at the top of the colours frame.
Obviously "1/1 (100%)" means that it has deduced the correct answer.
Below that the colours frame shows all available colours, which can be
individually clicked in any order.

Press Delete or click on the last peg (in the left-hand game frame)
to remove it, before the last one is placed, however once full your
turn is immediately scored and cannot be undone.

New Game, Help, and Exit buttons are assumed to be self-explanatory.
Changing the option settings implicitly triggers a new game, except
for the number of permitted guesses, pre-game-over. Reducing the
number of guesses can also be used as a means of conceding.

When a correct guess is entered or all guesses have been used the hint
and colours are replaced with "GAME OVER - YOU WIN/LOSE" under the
actual answer.
"""
include pGUI.e

Ihandle dlg, colours, codelen, maxgoes, repeats, progress,
        usehint, game_canvas, colour_canvas
integer ncolours, ncodelen, nmaxgoes
bool brepeats

sequence secret = {},
         hint = {},
         guesses = {{}},
         scores = {}
--
-- note: while the game is ongoing, length(guesses) should always be
--       length(scores)+1; equal lengths is equivalent to game over.
--

function get_score(sequence guess, goal)
integer blacks = 0, -- (right colour & place)
        whites = 0  -- ("" but wrong place)
    for i=1 to length(guess) do
        if guess[i]=goal[i] then
            blacks += 1
            guess[i] = ' '
            goal[i] = ' '
        end if
    end for
    for i=1 to length(guess) do
        if guess[i]!=' ' then
            integer k = find(guess[i],goal)
            if k then
                whites += 1
                goal[k] = ' '
            end if
        end if
    end for
    return {blacks, whites}
end function

function random_set()
-- create the secret code, and/or (when rqd) a SET_LIMIT random sample
    sequence cset = tagset(ncolours),
             res = repeat(0,ncodelen)
    for i=1 to ncodelen do
        integer c = rand(length(cset))
        res[i] = cset[c]
        if not brepeats then
            cset[c..c] = {}
        end if
    end for
    return res
end function

sequence initial_set
atom is_len,    -- logically length(initial_set), except when > SET_LIMIT.
     excluded   -- initialset[1..excluded-1], are not, [excluded..$] are.

procedure create_initial_set()
    is_len = iff(brepeats?power(ncolours,ncodelen):k_perm(ncolours,ncodelen))
    if is_len<=SET_LIMIT then
        --
        -- generate the full set
        --
        initial_set = repeat(0,is_len)
        excluded = is_len+1 -- (ie none)
        sequence next = iff(brepeats?repeat(1,ncodelen):tagset(ncodelen))
        for i=1 to is_len do
            initial_set[i] = next
            for ndx=length(next) to 1 by -1 do
                integer n = next[ndx]
                while n<=ncolours do
                    n += 1
                    if brepeats
--                  or not find(n,next[1..ndx-1]) then
                    or not find(n,next) then --(see below)
                        exit
                    end if
                end while
                next[ndx] = n
                if n<=ncolours then
                    if not brepeats then
                        --
                        -- Fill in the rest lowest-first, eg
                        -- in the 4 colours and 4 holes case:
                        --   (start)       (above)       (this)
                        --  {1,2,3,4} --> {1,2,4,_} --> {1,2,4,3}
                        --  {1,2,4,3} --> {1,3,_,_} --> {1,3,2,4}
                        --  ...   (20 other cases omitted)
                        --  {4,3,1,2} --> {4,3,2,_} --> {4,3,2,1}
                        --
                        -- (probably sub-optimal, but insignificant
                        --  vs. the o(n^2) analysis which follows.)
                        --
                        for j=ndx+1 to length(next) do
                            for k=1 to ncolours do
--                              if not find(k,next[1..j-1]) then
                                if not find(k,next) then --(see below)
                                    next[j] = k
                                    exit
                                end if
                            end for
                        end for
                    end if
                    exit
                end if
                --
                -- technical note: if not brepeats, we are going to
                -- replace all next[ndx..$] later/above anyway, but
                -- replacing with 0 means we can avoid those slices.
                -- The next three all work: 1 is perfect for the
                -- brepeats=true case, but brepeats=false needs the
                -- above slices, while the 2nd & 3rd are equivalent
                -- the latter is obviously somewhat faster, at the
                -- cost of a wtf?!, without a comment such as this.
                --
--              next[ndx] = 1
--              next[ndx] = iff(brepeats?1:0)
                next[ndx] = brepeats -- (equivalent)
            end for
        end for
    else
        --
        -- generate SET_LIMIT random codes
        -- note that if (as is quite likely) the actual answer is
        -- not present in initial_set, then obviously it cannot
        -- possibly find it!
        --
        initial_set = repeat(0,SET_LIMIT)
        excluded = SET_LIMIT+1  -- (ie none)
        for i=1 to SET_LIMIT do
            initial_set[i] = random_set()
        end for
    end if
end procedure

atom done, is_done, best

function idle_action()
    atom to_do = excluded-1,
         t1 = time()+1
    string samp = iff(is_len=length(initial_set)?"":sprintf(" samples of %,d",{is_len}))
    for i=1 to 100000 do    -- reasonable slice of work
        done += 1
        is_done += (done<excluded)
        sequence guest = initial_set[done],
                 scores = {}, counts = {}
        if not find(guest,guesses) then
            for j=1 to excluded-1 do
                sequence s = get_score(guest,initial_set[j])
                integer k = find(s,scores)
                if k=0 then
                    scores = append(scores,s)
                    counts = append(counts,1)
                else
                    counts[k] += 1
                end if
            end for
            if length(counts)=0 then
                IupSetStrAttribute(progress,"TITLE","[answer not in %,d%s]",{SET_LIMIT,samp})
                return IUP_IGNORE   -- (stop idle)
            end if
            integer k = largest(counts,return_index:=true),
                    ck = counts[k]
            if ck<best then
                best = ck
                hint = guest
                IupSetInt(usehint,"ACTIVE",true)
                IupUpdate(colour_canvas)
            end if
        end if

        if done=length(initial_set) then
            IupSetStrAttribute(progress,"TITLE","%,d/%,d%s (100%%)",{is_done,to_do,samp})
            return IUP_IGNORE   -- (stop idle)
        end if
        if time()>t1 then exit end if
    end for
    IupSetStrAttribute(progress,"TITLE","%,d/%,d%s (%d%%)",{is_done,to_do,samp,100*(is_done/to_do)})
    return IUP_DEFAULT
end function
constant idle_action_cb = Icallback("idle_action")

procedure start_idle()
    done = 0
    is_done = 0
    best = length(initial_set)+1
    IupSetGlobalFunction("IDLE_ACTION",idle_action_cb)
end procedure

procedure new_game()
    ncolours = IupGetInt(colours,"VALUE")
    ncodelen = IupGetInt(codelen,"VALUE")
    nmaxgoes = IupGetInt(maxgoes,"VALUE")
    brepeats = IupGetInt(repeats,"VALUE")
    secret = random_set()
    guesses = {{}}
    scores = {}
    hint = {}
    create_initial_set()
    start_idle()
end procedure

constant colour_table = {#e6194b,   -- Red
                         #3cb44b,   -- Green
                         #ffe119,   -- Yellow
                         #4363d8,   -- Blue
                         #f58231,   -- Orange
                         #911eb4,   -- Purple
                         #42d4f4,   -- Cyan
                         #f032e6,   -- Magenta
                         #bfef45,   -- Lime
                         #fabebe,   -- Pink
                         #469990,   -- Teal
                         #e6beff,   -- Lavender
                         #9A6324,   -- Brown
                         #fffac8,   -- Beige
                         #800000,   -- Maroon
                         #aaffc3,   -- Mint
                         #808000,   -- Olive
                         #ffd8b1,   -- Apricot
                         #000075,   -- Navy
                         #a9a9a9}   -- Grey

-- saved in redraw_cb(), for click testing in button_cb():
sequence last_guess = {},
         colour_centres = {}
integer guess_r2 = 0,
        colour_r2 = 0

function redraw_cb(Ihandle ih, integer /*posx*/, integer /*posy*/)
    Ihandle frame = IupGetParent(ih)
    string title = IupGetAttribute(ih,"TITLE")
    if not find(title,{"Game","Colours"}) then ?9/0 end if
    integer {cw,ch} = IupGetIntInt(ih, "DRAWSIZE")

    cdCanvas cddbuffer = IupGetAttributePtr(ih,"DBUFFER")
    IupGLMakeCurrent(ih)
    cdCanvasActivate(cddbuffer)
    cdCanvasClear(cddbuffer)

    if title="Game" then
        integer mx = min(floor(cw/(ncodelen*1.5+0.5)),floor(ch/(nmaxgoes+1))),
                diameter = floor(mx/2),
                px = floor((cw-(ncodelen*1.5+0.5)*mx)/2), -- (set margin)
                cy = ch, cx, c, r
        last_guess = {}
        for g=1 to length(guesses) do
            cy -= mx
            cx = px+floor(mx/2)
            for i=1 to 2*ncodelen+1 do
                if i!=ncodelen+1 then
                    if i<=ncodelen then
                        if i<=length(guesses[g]) then
                            c = colour_table[guesses[g][i]]
                            if g=length(guesses) then
                                last_guess = {{cx,ch-cy}}
                            end if
                        else
                            c = CD_GREY
                        end if
                        r = diameter
                    else
                        c = CD_GREY
                        if g<=length(scores) then
                            integer k = i-ncodelen-1,
                                    {b,w} = scores[g]
                            c = iff(k<=b ? CD_BLACK : iff(k<=b+w ? CD_WHITE : CD_GREY))
                        end if
                        r = floor(diameter*0.5)
                    end if
                    cdCanvasSetForeground(cddbuffer,c)
                    cdCanvasSector(cddbuffer, cx, cy, r, r, 0, 360)
                    cdCanvasSetForeground(cddbuffer,CD_DARK_GREY)
                    cdCanvasCircle(cddbuffer, cx, cy, r)
                end if
                cx += iff(i<ncodelen?mx:floor(mx/2))
            end for
        end for
        guess_r2 = floor(diameter*diameter/4)
    elsif title="Colours" then
        integer mx = min(floor(cw/ncodelen),floor(ch/2)),
                r = floor(mx/2),
                px = floor((cw-ncodelen*mx)/2), -- (set margin)
                cy = ch-r, cx, c
        cx = px+floor(mx/2)
        bool active = length(hint)>0
        if length(scores)=nmaxgoes then
            hint = secret
            active = true
        end if
        for i=1 to ncodelen do
            c = iff(active?colour_table[hint[i]]:CD_GREY)
            cdCanvasSetForeground(cddbuffer,c)
            cdCanvasSector(cddbuffer, cx, cy, r, r, 0, 360)
            cdCanvasSetForeground(cddbuffer,CD_DARK_GREY)
            cdCanvasCircle(cddbuffer, cx, cy, r)
            cx += mx
        end for
        if length(scores)=nmaxgoes
        or guesses[$]=secret then
            ch -= floor(mx/2)
            {} = cdCanvasTextAlignment(cddbuffer, CD_CENTER)
            string wl = iff(guesses[$]=secret?"WIN":"LOSE"),
                   msg = sprintf("GAME OVER - YOU %s",{wl})
            cdCanvasText(cddbuffer, cw/2, ch/2, msg)
        else
            integer ch0 = ch
            ch -= mx
            --
            -- calculate the best nw*nh way to fit all the colours in:
            -- (if nw ends up = ncodelen there is no clear separation
            --  between the hint and the colour table; the start with
            --  ncodelen+1 solves that and looks pretty good to me.)
            --
            integer nw = ncodelen+1,    -- (as above)
                    nh = 1
            while nw*nh<ncolours do
                if (cw/(nw+1))>(ch/(nh+1)) then
                    nw += 1
                else
                    nh += 1
                end if
            end while
            --
            -- now draw all the colours
            --
            mx = min(floor(cw/nw),floor(ch/nh))
            r = floor(mx/2)
            px = floor((cw-nw*mx)/2)
            cx = px+floor(mx/2)
            cy = ch-r
            integer this_row = 0
            colour_centres = repeat(0,ncolours)
            colour_r2 = floor(r*r/4)
            for i=1 to ncolours do
                colour_centres[i] = {cx,ch0-cy}
                c = colour_table[i]
                cdCanvasSetForeground(cddbuffer,c)
                cdCanvasSector(cddbuffer, cx, cy, r, r, 0, 360)
                cdCanvasSetForeground(cddbuffer,CD_DARK_GREY)
                cdCanvasCircle(cddbuffer, cx, cy, r)
                cx += mx
                this_row += 1
                if this_row>=nw then
                    this_row = 0
                    cx = px + floor(mx/2)
                    cy -= mx
                end if
            end for
        end if
    end if
    cdCanvasFlush(cddbuffer)
    return IUP_DEFAULT
end function

function map_cb(Ihandle ih)
    IupGLMakeCurrent(ih)
    atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
    cdCanvas cddbuffer = cdCreateCanvas(CD_GL, "10x10 %g", {res})
    IupSetAttributePtr(ih,"DBUFFER",cddbuffer)
    cdCanvasSetBackground(cddbuffer, CD_PARCHMENT)
    return IUP_DEFAULT
end function

function canvas_resize_cb(Ihandle canvas)
    cdCanvas cddbuffer = IupGetAttributePtr(canvas,"DBUFFER")
    integer {canvas_width, canvas_height} = IupGetIntInt(canvas, "DRAWSIZE")
    atom res = IupGetDouble(NULL, "SCREENDPI")/25.4
    cdCanvasSetAttribute(cddbuffer, "SIZE", "%dx%d %g", {canvas_width, canvas_height, res})
    return IUP_DEFAULT
end function

procedure redraw_all()
    IupUpdate({game_canvas,colour_canvas})
end procedure

procedure undo_move()
-- Called from button_cb and from K_DEL, but latter may be invalid.
    if length(guesses[$])!=0 then
        guesses[$] = guesses[$][1..$-1]
        redraw_all()
    end if
end procedure

procedure add_move(integer i)
    if i!=0 then
        guesses[$] &= i
    end if
    if length(guesses[$])=ncodelen then
        sequence guest = guesses[$],
                 score = get_score(guest,secret)
        scores = append(scores,score)
        if score!={ncodelen,0}  -- (not all black==game over)
        and length(guesses)<nmaxgoes then
            for i=excluded-1 to 1 by -1 do
                sequence isi = initial_set[i]
                if get_score(guest,isi)!=score then
                    excluded -= 1
                    if excluded!=i then
                        initial_set[i] = initial_set[excluded]
                        initial_set[excluded] = isi -- (swap)
                    end if
                end if
            end for
            guesses = append(guesses,{})
            hint = {}
            IupSetAttribute(progress,"TITLE","-")
            IupSetInt(usehint,"ACTIVE",false)
            start_idle()
        end if
    end if
    redraw_all()
end procedure

function usehint_cb(Ihandle /*usehint*/)
    guesses[$] = hint
    add_move(0)
    return IUP_DEFAULT
end function

function button_cb(Ihandle canvas, integer button, pressed, x, y, atom /*pStatus*/)
    Ihandle frame = IupGetParent(canvas)
    string title = IupGetAttribute(frame,"TITLE")
    if not find(title,{"Game","Colours"}) then ?9/0 end if
    if button=IUP_BUTTON1 and not pressed then      -- (left button released)
        {sequence centres, integer r2} = iff(title="Game"?{last_guess,guess_r2}
                                                         :{colour_centres,colour_r2})
        for i=1 to length(centres) do
            integer {cx,cy} = sq_sub(centres[i],{x,y})
            if (cx*cx+cy*cy)<=r2 then
                if title="Game" then
                    undo_move()
                else
                    add_move(i)
                end if
                exit
            end if
        end for
    end if
    return IUP_CONTINUE
end function

function new_game_cb(Ihandle /*ih*/)
    new_game()
    redraw_all()
    return IUP_DEFAULT
end function

function exit_cb(Ihandle /*ih*/)
    return IUP_CLOSE
end function
constant cb_exit = Icallback("exit_cb")

function help_cb(Ihandln /*ih*/)
    IupMessage("Mastermind",help_text)
    return IUP_DEFAULT
end function

function key_cb(Ihandle /*dlg*/, atom c)
    if c=K_ESC then return IUP_CLOSE end if
    if c=K_F1 then return help_cb(NULL) end if
    if find(c,{K_DEL,K_BS}) then undo_move() end if
    return IUP_CONTINUE
end function

function valuechanged_cb(Ihandle ih)
    ncolours = IupGetInt(colours,"VALUE")
    ncodelen = IupGetInt(codelen,"VALUE")
    nmaxgoes = IupGetInt(maxgoes,"VALUE")
    IupSetInt(repeats,"ACTIVE",ncodelen<=ncolours)
    if ncodelen>ncolours then
        IupSetInt(repeats,"VALUE",true)
    end if
    brepeats = IupGetInt(repeats,"VALUE")
    if ih!=maxgoes
    or length(scores)=length(guesses) then  -- (game over)
        new_game()
    elsif nmaxgoes<=length(scores) then
        -- (signal/force game over state)
        guesses = guesses[1..length(scores)]
    end if
    redraw_all()
    return IUP_DEFAULT
end function
constant cb_valuechanged = Icallback("valuechanged_cb")

procedure main()
    IupOpen()

    colours = IupText("SPIN=Yes, SPINMIN=1, SPINMAX=20, VALUE=6, RASTERSIZE=34x")
    codelen = IupText("SPIN=Yes, SPINMIN=1, SPINMAX=10, VALUE=4, RASTERSIZE=34x")
    maxgoes = IupText("SPIN=Yes, SPINMIN=1, SPINMAX=20, VALUE=7, RASTERSIZE=34x")
    repeats = IupToggle("Repeatable?","VALUE=YES, RIGHTBUTTON=YES, PADDING=5x4")
    progress = IupLabel("-","EXPAND=HORIZONTAL, PADDING=5x4")
    usehint = IupButton("Use",Icallback("usehint_cb"),"PADDING=5x4, ACTIVE=NO")
    game_canvas = IupGLCanvas("RASTERSIZE=200x")
    colour_canvas = IupGLCanvas("RASTERSIZE=x200")
    Ihandle newgame = IupButton("New Game",Icallback("new_game_cb"),"PADDING=5x4"),
            help = IupButton("Help (F1)",Icallback("help_cb"),"PADDING=5x4"),
            quit = IupButton("E&xit",Icallback("exit_cb"),"PADDING=5x4"),
            vbox = IupVbox({IupHbox({IupLabel("Colours (1-20)","PADDING=5x4"),colours}),
                            IupHbox({IupLabel("Code Length (1-10)","PADDING=5x4"),codelen}),
                            IupHbox({IupLabel("Guesses (1-20)","PADDING=5x4"),maxgoes}),
                            IupHbox({repeats},"MARGIN=10x5"),
                            IupHbox({progress}),
                            IupHbox({usehint,newgame,help,quit})},"MARGIN=5x5"),
            game_frame = IupFrame(IupHbox({game_canvas},"MARGIN=3x3"),"TITLE=Game"),
            option_frame = IupFrame(vbox,"TITLE=Options"),
            colour_frame = IupFrame(colour_canvas,"TITLE=Colours"),
            full = IupHbox({game_frame,IupVbox({option_frame,colour_frame})})
    IupSetCallbacks({colours,codelen,maxgoes,repeats}, {"VALUECHANGED_CB", cb_valuechanged})
    IupSetCallbacks({game_canvas,colour_canvas}, {"ACTION", Icallback("redraw_cb"),
                                                  "MAP_CB", Icallback("map_cb"),
                                                  "RESIZE_CB", Icallback("canvas_resize_cb"),
                                                  "BUTTON_CB", Icallback("button_cb")})
    dlg = IupDialog(IupHbox({full},"MARGIN=3x3"),"TITLE=Mastermind")
    IupSetCallback(dlg, "K_ANY", Icallback("key_cb"))
    IupSetAttributeHandle(dlg,"DEFAULTENTER", usehint)

    new_game()

    IupShowXY(dlg,IUP_CENTER,IUP_CENTER)
    IupSetAttribute(dlg, "RASTERSIZE", NULL)
    IupSetStrAttribute(dlg, "MINSIZE", IupGetAttribute(dlg,"RASTERSIZE"))
    IupMainLoop()
    IupClose()
end procedure
main()
```



## Prolog



```Prolog
mastermind :- mastermind(7, 4, 8, no_duplicates).

mastermind(Colours, Length, Guesses, Duplicates) :-
	between(2, 20, Colours),
	between(4, 10, Length),
	between(7, 20, Guesses),
	member(Duplicates, [allow_duplicates, no_duplicates]),

	create_board(Colours, Length, Duplicates, Board),
	intro(Colours, Length, Duplicates),
	play(board(Board, Length, Colours, Guesses), [], 0), !.

intro(Colours, Length, Duplicates) :-
	format('Guess the code!~n'),
	format('There are ~p character types, and ~p letters to guess~n',
	    [Colours, Length]),
	Duplicates = allow_duplicates
	-> format('Duplicates are allowed~n~n')
	; format('Duplicates are not allowed~n~n').

/* Create the combination to be guessed */
create_board(Colours, Length, Duplicates, Board) :-
	length(Board, Length),
	valid_char_list(Colours, CharSet),
	repeat,
	maplist(random_alpha(CharSet), Board),
	check_for_duplicates(Board, Duplicates).

check_for_duplicates(_, allow_dupicates).
check_for_duplicates(Board, no_duplicates) :- is_set(Board).

/* Main loop - get the player guess and print out status */
play(board(Board,_,_,MaxGuesses), _, MaxGuesses) :-
	write('Sorry, You failed to guess in time...!\nThe code was : '),
	maplist(write, Board),
	nl.
play(BoardData, PrevGuesses, GuessNum) :-
	BoardData = board(_, Length, Colours, MaxGuesses),
	GuessNum < MaxGuesses,
	ReportedGuess is GuessNum + 1,
	format('Guess #~p of #~p: ', [ReportedGuess, MaxGuesses]),
	get_player_guess(Length, Colours, Guess),
	evaluate_and_print_result(BoardData, PrevGuesses, ReportedGuess, Guess).

evaluate_and_print_result(board(Board,_,_,_), _, _,Board) :-
	format('Well done! You Guessed Correctly.~n').
evaluate_and_print_result(BoardData, PrevGuesses, NextGuessNum, Guess) :-
	BoardData = board(Board, _, _, _),
	dif(Board, Guess),
	match_guess_to_board(Board, Guess, Diffs),
	append(PrevGuesses, [guess(NextGuessNum, Guess, Diffs)], Guesses),
	maplist(print_guess, Guesses),
	play(BoardData, Guesses, NextGuessNum).

/* Get the player guess and validate that it matches the rules */
get_player_guess(Length, Colours, Guess) :-
	repeat,
	read_line_to_string(user_input, Line),
	string_chars(Line, Guess),

	% validate the correct number of items have been entered
	length(Guess, Length),

	% validate that all the characters are valid for the number of colours
	valid_char_list(Colours, ValidCharSet),
	subset(Guess, ValidCharSet).

/* Predicates to figure out how many places are correct */
match_guess_to_board(Board, Guess, Matches) :-
	maplist(guess_differences(Board), Board, Guess, Differences),
	sort(0, @>=, Differences, Matches).

% Same position, same type
guess_differences(_Board, B, B, 'X').
% Same type, different position
guess_differences(Board, B, G, 'O') :- dif(B, G), member(G, Board).
% Type not on board
guess_differences(Board, B, G, '-') :- dif(B, G), \+ member(G, Board).

/* Print out the current progress */
print_guess(guess(NextGuessNumber, Guess, Differences))	:-
	format('~w: ', NextGuessNumber),
	maplist(format('~w '), Guess),
	format(' : '),
	maplist(format('~w '), Differences),
	nl.

/* Utils */
alpha_chars([a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t]).

valid_char_list(Colours, CharSet) :-
	alpha_chars(AllChars),
	truncate_list(AllChars, Colours, CharSet).

random_alpha(AllChars, RandomMember) :- random_member(RandomMember, AllChars).

truncate_list(_, 0, []).
truncate_list([A|T], N, [A|R]) :-
	N > 0,
	N1 is N - 1,
	truncate_list(T, N1, R).
```

{{out}}

```txt

1 ?- mastermind.
Guess the code!
There are 7 character types, and 4 letters to guess
Duplicates are not allowed

Guess #1 of #8: abcd
1: a b c d  : X O O -
Guess #2 of #8: acbe
1: a b c d  : X O O -
2: a c b e  : X O - -
Guess #3 of #8: fgab
1: a b c d  : X O O -
2: a c b e  : X O - -
3: f g a b  : O O - -
Guess #4 of #8: cdfg
1: a b c d  : X O O -
2: a c b e  : X O - -
3: f g a b  : O O - -
4: c d f g  : X X O -
Guess #5 of #8: cdfb
1: a b c d  : X O O -
2: a c b e  : X O - -
3: f g a b  : O O - -
4: c d f g  : X X O -
5: c d f b  : X X O -
Guess #6 of #8: cdaf
1: a b c d  : X O O -
2: a c b e  : X O - -
3: f g a b  : O O - -
4: c d f g  : X X O -
5: c d f b  : X X O -
6: c d a f  : X O O O
Guess #7 of #8: acdf
1: a b c d  : X O O -
2: a c b e  : X O - -
3: f g a b  : O O - -
4: c d f g  : X X O -
5: c d f b  : X X O -
6: c d a f  : X O O O
7: a c d f  : X O O O
Guess #8 of #8: adfc
Well done! You Guess Correctly.
true.

```



## Python

{{works with|cpython|3.7.3}}
Tested in Python 3.7.3. Includes input verification.

```python

import random


def encode(correct, guess):
    output_arr = [''] * len(correct)

    for i, (correct_char, guess_char) in enumerate(zip(correct, guess)):
        output_arr[i] = 'X' if guess_char == correct_char else 'O' if guess_char in correct else '-'

    return ''.join(output_arr)


def safe_int_input(prompt, min_val, max_val):
    while True:
        user_input = input(prompt)

        try:
            user_input = int(user_input)
        except ValueError:
            continue

        if min_val <= user_input <= max_val:
            return user_input


def play_game():
    print("Welcome to Mastermind.")
    print("You will need to guess a random code.")
    print("For each guess, you will receive a hint.")
    print("In this hint, X denotes a correct letter, and O a letter in the original string but in a different position.")
    print()

    number_of_letters = safe_int_input("Select a number of possible letters for the code (2-20): ", 2, 20)
    code_length = safe_int_input("Select a length for the code (4-10): ", 4, 10)

    letters = 'ABCDEFGHIJKLMNOPQRST'[:number_of_letters]
    code = ''.join(random.choices(letters, k=code_length))
    guesses = []

    while True:
        print()
        guess = input(f"Enter a guess of length {code_length} ({letters}): ").upper().strip()

        if len(guess) != code_length or any([char not in letters for char in guess]):
            continue
        elif guess == code:
            print(f"\nYour guess {guess} was correct!")
            break
        else:
            guesses.append(f"{len(guesses)+1}: {' '.join(guess)} => {' '.join(encode(code, guess))}")

        for i_guess in guesses:
            print("------------------------------------")
            print(i_guess)
        print("------------------------------------")


if __name__ == '__main__':
    play_game()


```

{{out}}

```txt

Welcome to Mastermind.
You will need to guess a random code.
For each guess, you will receive a hint.
In this hint, X denotes a correct letter, and O a letter in the original string but a different position.

Select a number of possible letters for the code (2-20): 4
Select a length for the code (4-10): 4

*omitted first guesses*

Enter a guess of length 4 (ABCD): cdaa
------------------------------------
1: A A B B => O O - -
------------------------------------
2: C D A A => O X X O
------------------------------------

Enter a guess of length 4 (ABCD): ddac

Your guess DDAC was correct!

```



## REXX

More checks could have been added   (for illegal inputs and illegal options).

```rexx
/*REXX pgm scores  mastermind  game with a human  or  CBLFs (Carbon Based Life Forms).  */
parse arg let wid mxG oRep seed _                /*obtain optional arguments from the CL*/
      arg  .   .   .   rep .                     /*get uppercase 4th argument  "   "  " */
if let=='' | let==","  then let= 20              /*Not specified?  Then use the default.*/
if wid=='' | wid==","  then wid=  4              /* "      "         "   "   "     "    */
if mxG=='' | mxG==","  then mxG= 20              /* "      "         "   "   "     "    */
if rep=='' | rep==","  then rep=  0              /* "      "         "   "   "     "    */
if datatype(seed,'W')  then call random ,,seed   /*use a seed for random repeatability. */
if abbrev(  'REPEATSALLOWED',rep,3)  then rep=1  /*allow an abbreviated option for REP. */
if abbrev('NOREPEATSALLOWED',rep,3)  then rep=0  /*  "    "      "         "    "   "   */
call vet   arg(),     'args'                     /*Vet the number of arguments entered. */  /*◄■■■■■■ optional vetting.*/
call vet     let,  'letters', 2, 20              /* "   "     "    " letters in the code*/  /*◄■■■■■■ optional vetting.*/
call vet     wid,    'width', 4, 10              /* "   "     "    " the width of code. */  /*◄■■■■■■ optional vetting.*/
call vet     mxG, 'maxGuess', 7, 20              /* "   "     "    " maximum guesses.   */  /*◄■■■■■■ optional vetting.*/
call vet     rep,      'REP', 0, 1e8             /* "   "   value if repeats are allowed*/  /*◄■■■■■■ optional vetting.*/
call gen;                                        yourG= 'Your guess must be exactly '
                                                 youve= "You've already tried that guess "
        do prompt=0  by 0  until xx==wid;   say  /*play until guessed or QUIT is entered*/
        say id 'Please enter a guess with '   wid  ' letters                   [or Quit]:'
        pull g;   g=space(g,0);  L=length(g);     if abbrev('QUIT',g,1)  then exit 0
        if L\==wid  then do;  say id '***error***'  yourG wid  " letters.";  iterate;  end
        call dups                                /*look through the history log for dups*/
        q=?;      XX=0;      OO=0;     try=try+1 /*initialize some REXX vars;  bump TRY.*/

             do j=1  for L;  if substr(g,j,1) \== substr(q,j,1)  then iterate    /*hit? */
             xx=xx+1;    q=overlay('▒', q, j)    /*bump the  XX  correct   count.       */
             end   /*j*/                         /* [↑]  XX  correct count; scrub guess.*/

             do k=1  for L;   _=substr(g, k, 1)  /*process the count for  "spots".      */
             if pos(_, q)==0  then iterate       /*is this  (spot)  letter in the code? */
             oo=oo+1;       q=translate(q, , _)  /*bump the  OO  spot count.            */
             end   /*k*/                         /* [↑]  OO  spot count;  & scrub guess.*/
        say
        @.try=id  right('guess'  try, 11)     '  ('mxG       "is the max):"    g   '──►' ,
                                      copies('X', xx)copies("O", oo)copies('-', wid-xx-oo)
        call hist
        if try==mxG  then do;  say;      say id   "you've used the maximum guesses:"   mxG
                               say;      say id   "The code was: "   ?;    say;     exit 1
                          end
        end   /*prompt*/
say;                           say "          ┌─────────────────────────────────────────┐"
                               say "          │                                         │"
                               say "          │  Congratulations, you've guessed it !!  │"
                               say "          │                                         │"
                               say "          └─────────────────────────────────────────┘"
exit 0                                           /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
dups:    do h=1  for try;  if g\=word(@.h, 8)  then iterate   /*any duplicated guesses? */
         say;  say id youve  " (guess number" h').'; iterate prompt; end  /*h*/;    return
/*──────────────────────────────────────────────────────────────────────────────────────*/
gen:  if rep==0  then reps= 'no'                 /*create a literal for the prompt msg. */
                 else reps=
      @abc= 'QWERTYUIOPASDFGHJKLZXCVBNM'         /*capital letters used for random code.*/
      id='────────';  try=0;  L@abc=length(@abc) /*identifier in front of msg from here.*/
      ?=
          do  until  length(?)==wid              /*gen random codes 'til there's enough.*/
          r=substr(@abc, random(1, L@abc), 1)    /*generate a random letter, 1 at a time*/
          if \rep & pos(r, ?)\==0  then iterate  /*maybe  don't  allow a repeated digit.*/
          ?=? || r; if ?=='QUIT'&let==4  then ?= /*append random letter; ··· except this*/
          end   /*until*/                        /* [↑]  builds a unique  N-letter code.*/
      say
      say id 'A random code of '   wid   "letters  (out of a possible "  let  ' letters) '
      say id 'has been generated   (with'    reps    "repeats)."
      return
/*──────────────────────────────────────────────────────────────────────────────────────*/
hist:    do hist=1  for try;  say @.hist;  end;   return         /*show "guess" history.*/
s:    if arg(1)==1  then return '';      return "s"              /*a simpler pluraizer. */
ser:  say;  say;    say '***error***'   arg(1);     say;   say;         exit 13
/*──────────────────────────────────────────────────────────────────────────────────────*/  /*◄■■■■■■ optional vetting.*/
vet:  parse arg val,?,mn,mx                      /*vet (validate) a specified argument. */  /*◄■■■■■■ optional vetting.*/
      if ?=="args" & (val>1 | _\='')  then call ser "Too many arguments specified. "  _     /*◄■■■■■■ optional vetting.*/
      if ?=="args"       then return                                                        /*◄■■■■■■ optional vetting.*/
      if \datatype(val, 'N')          then call ser ? "isn't numeric: "               val   /*◄■■■■■■ optional vetting.*/
      if \datatype(val, 'W')          then call ser ? "isn't an integer: "            val   /*◄■■■■■■ optional vetting.*/
      if val < mn                     then call ser ? "has a value less than "        mn    /*◄■■■■■■ optional vetting.*/
      if val > mx                     then call ser ? "has a value greater than "     mx    /*◄■■■■■■ optional vetting.*/
      if ?=='REP' & \datatype(val,W)  then call ser "Value for REPEATS isn't valid: " oRep  /*◄■■■■■■ optional vetting.*/
      return 1
```

'''output'''

```txt

──────── A random code of  4 letters  (out of a possible  20  letters)
──────── has been generated   (with no repeats).

──────── Please enter a guess with  4  letters                   [or Quit]:
abcd                  ◄■■■■■■ user input

────────     guess 1   (20 is the max): ABCD ──► ----

──────── Please enter a guess with  4  letters                   [or Quit]:
efgh                  ◄■■■■■■ user input

────────     guess 1   (20 is the max): ABCD ──► ----
────────     guess 2   (20 is the max): EFGH ──► ----

──────── Please enter a guess with  4  letters                   [or Quit]:
ijkl                  ◄■■■■■■ user input

────────     guess 1   (20 is the max): ABCD ──► ----
────────     guess 2   (20 is the max): EFGH ──► ----
────────     guess 3   (20 is the max): IJKL ──► O---

──────── Please enter a guess with  4  letters                   [or Quit]:
mnop                  ◄■■■■■■ user input

     ···············································
     ·    (Some of the output has been elided.)    ·
     ···············································

──────── Please enter a guess with  4  letters                   [or Quit]:
yinp                  ◄■■■■■■ user input

────────     guess 1   (20 is the max): ABCD ──► ----
────────     guess 2   (20 is the max): EFGH ──► ----
────────     guess 3   (20 is the max): IJKL ──► O---
────────     guess 4   (20 is the max): MNOP ──► XO--
────────     guess 5   (20 is the max): WXYZ ──► O---
────────     guess 6   (20 is the max): LKHI ──► O---
────────     guess 7   (20 is the max): PONM ──► XO--
────────     guess 8   (20 is the max): ZYXW ──► O---
────────     guess 9   (20 is the max): YZWX ──► X---
────────    guess 10   (20 is the max): MOPN ──► OO--
────────    guess 11   (20 is the max): OMPN ──► OO--
────────    guess 12   (20 is the max): LKJI ──► O---
────────    guess 13   (20 is the max): JILK ──► X---
────────    guess 14   (20 is the max): YINP ──► XXXX

          ┌─────────────────────────────────────────┐
          │                                         │
          │  Congratulations, you've guessed it !!  │
          │                                         │
          └─────────────────────────────────────────┘

```



## Ring


```ring

# Project : Mastermind

colors = ["A", "B", "C", "D"]
places = list(2)
mind = list(len(colors))
rands = list(len(colors))
master = list(len(colors))
test = list(len(colors))
guesses  = 7
repeat = false
nr = 0

if repeat
   for n = 1 to len(colors)
        while true
                  rnd = random(len(colors)-1) + 1
                  if rands[rnd] != 1
                     mind[n] = rnd
                     rands[rnd] = 1
                    exit
                  ok
        end
   next
else
   for n = 1 to len(colors)
        rnd = random(len(colors)-1) + 1
        mind[n] = rnd
    next
ok

for n = 1 to len(colors)
      master[n] = char(64+mind[n])
next
while true
         for p = 1 to len(places)
               places[p] = 0
         next
         nr = nr + 1
         see "Your guess (ABCD)? "
         give testbegin
         for d = 1 to len(test)
               test[d] = testbegin[d]
         next
         flag = 1
         for n = 1 to len(test)
               if upper(test[n]) != master[n]
                  flag = 0
               ok
         next
         if flag = 1
            exit
         else
            for x = 1 to len(master)
                  if upper(test[x]) = master[x]
                      places[1] = places[1] + 1
                  ok
            next
            mastertemp = master
            for p = 1 to len(test)
                  pos = find(mastertemp, upper(test[p]))
                  if pos > 0
                     del(mastertemp, pos)
                     places[2] = places[2] + 1
                  ok
            next
         ok
         place1 = places[1]
         place2 = places[2] - place1
         place3 = len(master) - (place1 + place2)
         showresult(test, place1, place2, place3)
         if nr = guesses
            exit
         ok
end
see "Well done!" + nl
see "End of game" + nl

func showresult(test, place1, place2, place3)
        see "" + nr + " : "
        for r = 1 to len(test)
             see test[r]
        next
        see " : "
        for n1 = 1 to place1
              see "X" + " "
        next
        for n2 = 1 to place2
              see "O" + " "
        next
        for n3 = 1 to place3
              see "-" + " "
        next
        see nl

```

Output:

```txt

Your guess (ABCD)? BCDA
1 : BCDA : X X O -
Your guess (ABCD)? BCDB
2 : BCDB : X X X -
Your guess (ABCD)? BCBB
3 : BCBB : X X X -
Your guess (ABCD)? BCAB
Well done!
End of game

```



## Rust

{{libheader|rand}}

```rust
extern crate rand;

use rand::prelude::*;
use std::io;

fn main() {
    let mut input_line = String::new();
    let colors_n;
    let code_len;
    let guesses_max;
    let colors_dup;

    loop {
        println!("Please enter the number of colors to be used in the game (2 - 20): ");
        input_line.clear();
        io::stdin()
            .read_line(&mut input_line)
            .expect("The read line failed.");
        match (input_line.trim()).parse::<i32>() {
            Ok(n) => {
                if n >= 2 && n <= 20 {
                    colors_n = n;
                    break;
                } else {
                    println!("Outside of range (2 - 20).");
                }
            }
            Err(_) => println!("Invalid input."),
        }
    }
    let colors = &"ABCDEFGHIJKLMNOPQRST"[..colors_n as usize];

    println!("Playing with colors {}.\n", colors);

    loop {
        println!("Are duplicated colors allowed in the code? (Y/N): ");
        input_line.clear();
        io::stdin()
            .read_line(&mut input_line)
            .expect("The read line failed.");
        if ["Y", "N"].contains(&&input_line.trim().to_uppercase()[..]) {
            colors_dup = input_line.trim().to_uppercase() == "Y";
            break;
        } else {
            println!("Invalid input.");
        }
    }
    println!(
        "Duplicated colors {}allowed.\n",
        if colors_dup { "" } else { "not " }
    );
    loop {
        let min_len = if colors_dup { 4 } else { 4.min(colors_n) };
        let max_len = if colors_dup { 10 } else { 10.min(colors_n) };
        println!(
            "Please enter the length of the code ({} - {}): ",
            min_len, max_len
        );
        input_line.clear();
        io::stdin()
            .read_line(&mut input_line)
            .expect("The read line failed.");
        match (input_line.trim()).parse::<i32>() {
            Ok(n) => {
                if n >= min_len && n <= max_len {
                    code_len = n;
                    break;
                } else {
                    println!("Outside of range ({} - {}).", min_len, max_len);
                }
            }
            Err(_) => println!("Invalid input."),
        }
    }
    println!("Code of length {}.\n", code_len);
    loop {
        println!("Please enter the number of guesses allowed (7 - 20): ");
        input_line.clear();
        io::stdin()
            .read_line(&mut input_line)
            .expect("The read line failed.");
        match (input_line.trim()).parse::<i32>() {
            Ok(n) => {
                if n >= 7 && n <= 20 {
                    guesses_max = n;
                    break;
                } else {
                    println!("Outside of range (7 - 20).");
                }
            }
            Err(_) => println!("Invalid input."),
        }
    }
    println!("{} guesses allowed.\n", guesses_max);

    let mut rng = rand::thread_rng();
    let mut code;
    if colors_dup {
        code = (0..code_len)
            .map(|_| ((65 + rng.gen_range(0, colors_n) as u8) as char))
            .collect::<Vec<_>>();
    } else {
        code = colors.chars().collect::<Vec<_>>();
        code.shuffle(&mut rng);
        code = code[..code_len as usize].to_vec();
    }
    //code = vec!['J', 'A', 'R', 'D', 'A', 'N', 'I'];
    //println!("Secret code: {:?}", code);
    let mut guesses: Vec<(String, String)> = vec![];
    let mut i = 1;
    loop {
        println!("Your guess ({}/{})?: ", i, guesses_max);
        input_line.clear();
        io::stdin()
            .read_line(&mut input_line)
            .expect("The read line failed.");
        let mut guess = input_line.trim().to_uppercase();
        if guess.len() as i32 > code_len {
            guess = guess[..code_len as usize].to_string();
        }
        let guess_v = guess.chars().collect::<Vec<char>>();
        let res = evaluate(&code, &guess_v);
        guesses.push((guess, res.clone()));
        let width = 8 + guesses_max.to_string().len() + code_len as usize * 2;
        println!("{}", "-".repeat(width));
        for (i, guess) in guesses.iter().enumerate() {
            let line = format!(
                " {:w1$} : {:w2$} : {:w2$} ",
                i + 1,
                guess.0,
                guess.1,
                w1 = guesses_max.to_string().len(),
                w2 = code_len as usize
            );
            println!("{}", line);
        }
        println!("{}", "-".repeat(width));
        if res == "X".repeat(code_len as usize) {
            println!("You won! Code: {}", code.into_iter().collect::<String>());
            break;
        }
        i += 1;
        if i > guesses_max {
            println!("You lost. Code: {}", code.into_iter().collect::<String>());
            break;
        }
    }
}

fn evaluate(code: &[char], guess: &[char]) -> String {
    let mut res: Vec<char> = vec![];
    for i in 0..guess.len() {
        if guess[i] == code[i] {
            res.push('X');
        } else if code.contains(&guess[i]) {
            res.push('O');
        } else {
            res.push('-');
        }
    }
    res.sort_by(|a, b| b.cmp(a));
    res.into_iter().collect()
}
```

{{out}}

```txt

Please enter the number of colors to be used in the game (2 - 20):
20
Playing with colors ABCDEFGHIJKLMNOPQRST.

Are duplicated colors allowed in the code? (Y/N):
n
Duplicated colors not allowed.

Please enter the length of the code (4 - 10):
10
Code of length 10.

Please enter the number of guesses allowed (7 - 20):
20
20 guesses allowed.

Your guess (1/20)?:
abcdefghi
------------------------------
  1 : ABCDEFGHI  : OOOOOO---
------------------------------
<...>
Your guess (20/20)?:
ipgecnmhdk
------------------------------
  1 : ABCDEFGHI  : OOOOOO---
  2 : ABCDE      : OOO--
  3 : ABC        : O--
  4 : DEBF       : OO--
  5 : DEAG       : OOO-
  6 : DEAH       : OOO-
  7 : DEAGH      : OOOO-
  8 : DEGHI      : XOOOO
  9 : JKLMN      : OOO--
 10 : JKL        : O--
 11 : K          : O
 12 : DEGHIK     : XOOOOO
 13 : MN         : OO
 14 : OPQ        : X--
 15 : QRS        : ---
 16 : PT         : O-
 17 : PEGHIKCMND : XOOOOOOOOO
 18 : HKMNICDPEG : OOOOOOOOOO
 19 : EGNHKMPDCI : OOOOOOOOOO
 20 : IPGECNMHDK : XXXXXXXXOO
------------------------------
You lost. Code: IPGECHMNDK

```



## zkl

{{trans|C++}}

```zkl
class MasterMind{
   fcn init(code_len,guess_count){
      var codeLen =code_len.max(4).min(10);
      var guessCnt=guess_count.max(7).min(20);
      var colors  ="ABCDEFGHIJKLMNOPQRST"[0,codeLen];
   }
   fcn play{
      guesses,win,blackWhite:=List(),False,Void;
      code:=codeLen.pump(String,'wrap(_){ colors[(0).random(codeLen)] });
      do(guessCnt){
	 str:=getInput();
	 win,blackWhite = checkInput(str,code);
	 guesses.append(T(str,blackWhite));
	 showBoard(guesses);
	 if(win) break;
      }
      if(win) println("--------------------------------\n",
		"Very well done!\nYou found the code: ",code);
       else println("--------------------------------\n",
		"I am sorry, you didn't discover the code!\nThe code was: ",code);
    }
    fcn [private] showBoard(guesses){
       foreach n,gbw in ([1..].zip(guesses)){
          guess,blackWhite := gbw;
          println("%2d: %s :% s %s".fmt(n,
	     guess.split("").concat(" "), blackWhite.split("").concat(" "),
	     "- "*(codeLen - blackWhite.len())));
       }
    }
    fcn [private] getInput{
       while(True){
	  a:=ask("Enter your guess (" + colors + "): ").toUpper()[0,codeLen];
	  if(not (a-colors) and a.len()>=codeLen) return(a);
       }
    }
    fcn [private] checkInput(guess,code){
	// black: guess is correct in both color and position
        // white: correct color, wrong position
	matched,black := guess.split("").zipWith('==,code), matched.sum(0);
	// remove black from code, prepend null to make counting easy
	code  = L("-").extend(matched.zipWith('wrap(m,peg){ m and "-" or peg },code));
	white:=0; foreach m,p in (matched.zip(guess)){
	   if(not m and (z:=code.find(p))){ white+=1; code[z]="-"; }
	}
	return(black==codeLen,"X"*black + "O"*white)
    }
}(4,12).play();
```

{{out}}

```txt

Enter your guess (ABCD): abcd
 1: A B C D : X O O -
Enter your guess (ABCD): abcc
 1: A B C D : X O O -
 2: A B C C : O O - -
Enter your guess (ABCD): aaad
 1: A B C D : X O O -
 2: A B C C : O O - -
 3: A A A D : X - - -
Enter your guess (ABCD): bccd
 1: A B C D : X O O -
 2: A B C C : O O - -
 3: A A A D : X - - -
 4: B C C D : X X O -
Enter your guess (ABCD): dcbd
 1: A B C D : X O O -
 2: A B C C : O O - -
 3: A A A D : X - - -
 4: B C C D : X X O -
 5: D C B D : X X X X
--------------------------------
Very well done!
You found the code: DCBD

```

