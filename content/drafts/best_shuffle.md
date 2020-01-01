+++
title = "Best shuffle"
description = ""
date = 2019-10-02T10:08:08Z
aliases = []
[extra]
id = 8971
[taxonomies]
categories = []
tags = []
+++

{{task}}

;Task:
Shuffle the characters of a string in such a way that as many of the character values are in a different position as possible.

A shuffle that produces a randomized result among the best choices is to be preferred. A deterministic approach that produces the same sequence every time is acceptable as an alternative.

Display the result as follows:

 original string, shuffled string, (score)

The score gives the number of positions whose character value did ''not'' change.


;Example:
 tree, eetr, (0)


;Test cases:
 abracadabra
 seesaw
 elk
 grrrrrr
 up
 a


;Related tasks
*   [[Anagrams/Deranged anagrams]]
*   [[Permutations/Derangements]]





## Ada

 {{trans|AWK}}

```Ada
with Ada.Text_IO;
with Ada.Strings.Unbounded;

procedure Best_Shuffle is

   function Best_Shuffle (S : String) return String;

   function Best_Shuffle (S : String) return String is
      T : String (S'Range) := S;
      Tmp : Character;
   begin
      for I in S'Range loop
         for J in S'Range loop
            if I /= J and S (I) /= T (J) and S (J) /= T (I) then
               Tmp  := T (I);
               T (I) := T (J);
               T (J) := Tmp;
            end if;
         end loop;
      end loop;
      return T;
   end Best_Shuffle;

   Test_Cases : constant array (1 .. 6)
     of Ada.Strings.Unbounded.Unbounded_String :=
                  (Ada.Strings.Unbounded.To_Unbounded_String ("abracadabra"),
                   Ada.Strings.Unbounded.To_Unbounded_String ("seesaw"),
                   Ada.Strings.Unbounded.To_Unbounded_String ("elk"),
                   Ada.Strings.Unbounded.To_Unbounded_String ("grrrrrr"),
                   Ada.Strings.Unbounded.To_Unbounded_String ("up"),
                   Ada.Strings.Unbounded.To_Unbounded_String ("a"));

begin -- main procedure
   for Test_Case in Test_Cases'Range loop
      declare
         Original : constant String := Ada.Strings.Unbounded.To_String
           (Test_Cases (Test_Case));
         Shuffle  : constant String := Best_Shuffle (Original);
         Score : Natural := 0;
      begin
         for I in Original'Range loop
            if Original (I) = Shuffle (I) then
               Score := Score + 1;
            end if;
         end loop;
         Ada.Text_IO.Put_Line (Original & ", " & Shuffle & ", (" &
                                Natural'Image (Score) & " )");
      end;
   end loop;
end Best_Shuffle;
```


Output:

```txt
abracadabra, caadrbabaar, ( 0 )
seesaw, ewaess, ( 0 )
elk, kel, ( 0 )
grrrrrr, rgrrrrr, ( 5 )
up, pu, ( 0 )
a, a, ( 1 )
```



## AutoHotkey


```AutoHotkey
words := "abracadabra,seesaw,elk,grrrrrr,up,a"
Loop Parse, Words,`,
   out .= Score(A_LoopField, Shuffle(A_LoopField))
MsgBox % clipboard := out


Shuffle(String)
{
 Cord := String
 Length := StrLen(String)
 CharType := A_IsUnicode ? "UShort" : "UChar"

 Loop, Parse, String  ; For each old character in String...
 {
  Char1 := SubStr(Cord, A_Index, 1)
  If (Char1 <> A_LoopField)  ; If new character already differs,
   Continue                  ;  do nothing.

  Index1 := A_Index
  OldChar1 := A_LoopField
  Random, Index2, 1, Length  ; Starting at some random index,
  Loop, %Length%             ;  for each index...
  {
   If (Index1 <> Index2)     ; Swap requires two different indexes.
   {
    Char2 := SubStr(Cord, Index2, 1)
    OldChar2 := SubStr(String, Index2, 1)

    ; If after the swap, the two new characters would differ from
    ; the two old characters, then do the swap.
    If (Char1 <> OldChar2) and (Char2 <> OldChar1)
    {
     ; Swap Char1 and Char2 inside Cord.
     NumPut(Asc(Char1), Cord, (Index2 - 1) << !!A_IsUnicode, CharType)
     NumPut(Asc(Char2), Cord, (Index1 - 1) << !!A_IsUnicode, CharType)
     Break
   }
   }
   Index2 += 1           ; Get next index.
   If (Index2 > Length)  ; If after last index,
    Index2 := 1          ;  use first index.
  }
 }
 Return Cord
}
Score(a, b){
	r := 0
	Loop Parse, a
		If (A_LoopField = SubStr(b, A_Index, 1))
			r++
	return a ", " b ", (" r ")`n"
}
```

Output:

```txt
abracadabra, caadarrbaab, (0)
seesaw, easews, (0)
elk, kel, (0)
grrrrrr, rrrrrrg, (5)
up, pu, (0)
a, a, (1)

```



## AWK

{{trans|Icon}}
The Icon and Unicon program uses a simple algorithm of swapping. This is relatively easy to translate to Awk.


```awk
{
	scram = best_shuffle($0)
	print $0 " -> " scram " (" unchanged($0, scram) ")"
}

function best_shuffle(s,    c, i, j, len, r, t) {
	len = split(s, t, "")

	# Swap elements of t[] to get a best shuffle.
	for (i = 1; i <= len; i++) {
		for (j = 1; j <= len; j++) {
			# Swap t[i] and t[j] if they will not match
			# the original characters from s.
			if (i != j &&
			    t[i] != substr(s, j, 1) &&
			    substr(s, i, 1) != t[j]) {
				c = t[i]
				t[i] = t[j]
				t[j] = c
				break
			}
		}
	}

	# Join t[] into one string.
	r = ""
	for (i = 1; i <= len; i++)
		r = r t[i]
	return r
}

function unchanged(s1, s2,    count, len) {
	count = 0
	len = length(s1)
	for (i = 1; i <= len; i++) {
		if (substr(s1, i, 1) == substr(s2, i, 1))
			count++
	}
	return count
}
```


This program has the same output as the Icon and Unicon program.

{{trans|Perl 6}}
The Perl 6 program (and the equivalent Ruby program) use several built-in array functions. Awk provides no array functions, except for split(). This Awk program, a translation from Perl 6, uses its own code

* to sort an array,
* to insert an element into the middle of an array,
* to remove an element from the middle of an array (and close the gap),
* to pop an element from the end of an array, and
* to join the elements of an array into a string.

If those built-in array functions seem strange to you, and if you can understand these for loops, then you might prefer this Awk program. This algorithm counts the letters in the string, sorts the positions, and fills the positions in order.


```awk
# out["string"] = best shuffle of string _s_
# out["score"] = number of matching characters
function best_shuffle(out, s,    c, i, j, k, klen, p, pos, set, rlen, slen) {
	slen = length(s)
	for (i = 1; i <= slen; i++) {
		c = substr(s, i, 1)

		# _set_ of all characters in _s_, with count
		set[c] += 1

		# _pos_ classifies positions by letter,
		# such that pos[c, 1], pos[c, 2], ..., pos[c, set[c]]
		# are the positions of _c_ in _s_.
		pos[c, set[c]] = i
	}

	# k[1], k[2], ..., k[klen] sorts letters from low to high count
	klen = 0
	for (c in set) {
		# insert _c_ into _k_
		i = 1
		while (i <= klen && set[k[i]] <= set[c])
			i++              # find _i_ to sort by insertion
		for (j = klen; j >= i; j--)
			k[j + 1] = k[j]  # make room for k[i]
		k[i] = c
		klen++
	}

	# Fill pos[slen], ..., pos[3], pos[2], pos[1] with positions
	# in the order that we want to fill them.
	i = 1
	while (i <= slen) {
		for (j = 1; j <= klen; j++) {
			c = k[j]
			if (set[c] > 0) {
				pos[i] = pos[c, set[c]]
				i++
				delete pos[c, set[c]]
				set[c]--
			}
		}
	}

	# Now fill in _new_ with _letters_ according to each position
	# in pos[slen], ..., pos[1], but skip ahead in _letters_
	# if we can avoid matching characters that way.
	rlen = split(s, letters, "")
	for (i = slen; i >= 1; i--) {
		j = 1
		p = pos[i]
		while (letters[j] == substr(s, p, 1) && j < rlen)
			j++
		for (new[p] = letters[j]; j < rlen; j++)
			letters[j] = letters[j + 1]
		delete letters[rlen]
		rlen--
	}

	out["string"] = ""
	for (i = 1; i <= slen; i++) {
		out["string"] = out["string"] new[i]
	}

	out["score"] = 0
	for (i = 1; i <= slen; i++) {
		if (new[i] == substr(s, i, 1))
			out["score"]++
	}
}

BEGIN {
	count = split("abracadabra seesaw elk grrrrrr up a", words)
	for (i = 1; i <= count; i++) {
		best_shuffle(result, words[i])
		printf "%s, %s, (%d)\n",
		    words[i], result["string"], result["score"]
	}
}
```


Output:


```bash
$ awk -f best-shuffle.awk
abracadabra, baarrcadaab, (0)
seesaw, essewa, (0)
elk, kel, (0)
grrrrrr, rgrrrrr, (5)
up, pu, (0)
a, a, (1)
```


The output might change if the <tt>for (c in set)</tt> loop iterates the array in a different order.


## BBC BASIC

{{works with|BBC BASIC for Windows}}

```bbcbasic
      a$ = "abracadabra" : b$ = FNshuffle(a$) : PRINT a$ " -> " b$ FNsame(a$,b$)
      a$ = "seesaw"      : b$ = FNshuffle(a$) : PRINT a$ " -> " b$ FNsame(a$,b$)
      a$ = "elk"         : b$ = FNshuffle(a$) : PRINT a$ " -> " b$ FNsame(a$,b$)
      a$ = "grrrrrr"     : b$ = FNshuffle(a$) : PRINT a$ " -> " b$ FNsame(a$,b$)
      a$ = "up"          : b$ = FNshuffle(a$) : PRINT a$ " -> " b$ FNsame(a$,b$)
      a$ = "a"           : b$ = FNshuffle(a$) : PRINT a$ " -> " b$ FNsame(a$,b$)
      END

      DEF FNshuffle(s$)
      LOCAL i%, j%, l%, s%, t%, t$
      t$ = s$ : s% = !^s$ : t% = !^t$ : l% = LEN(t$)
      FOR i% = 0 TO l%-1 : SWAP t%?i%,t%?(RND(l%)-1) : NEXT
      FOR i% = 0 TO l%-1
        FOR j% = 0 TO l%-1
          IF i%<>j% THEN
            IF t%?i%<>s%?j% IF s%?i%<>t%?j% THEN
              SWAP t%?i%,t%?j%
              EXIT FOR
            ENDIF
          ENDIF
        NEXT
      NEXT i%
      = t$

      DEF FNsame(s$, t$)
      LOCAL i%, n%
      FOR i% = 1 TO LEN(s$)
        IF MID$(s$,i%,1)=MID$(t$,i%,1) n% += 1
      NEXT
      = " (" + STR$(n%) + ")"
```

Output (varies between runs):

```txt

abracadabra -> daaracababr (0)
seesaw -> essewa (0)
elk -> lke (0)
grrrrrr -> rgrrrrr (5)
up -> pu (0)
a -> a (1)

```



## Bracmat

Not optimized:

```bracmat

    ( shuffle
    =   m car cdr todo a z count string
      .     !arg:(@(?:%?car ?cdr).?todo)
          & !Count:?count
          & (   @( !todo
                 :   ?a
                     (%@:~!car:?m)
                     ( ?z
                     &   shuffle$(!cdr.str$(!a !z))
                       : (<!count:?count.?string)
                     & ~
                     )
                 )
              | !count:<!Count
              |   @(!todo:%?m ?z)
                & shuffle$(!cdr.!z):(?count.?string)
                & !count+1
            . !m !string
            )
        | (0.)
    )
  & abracadabra seesaw elk grrrrrr up a:?words
  &   whl
    ' ( !words:%?word ?words
      & @(!word:? [?Count)
      & out$(!word shuffle$(!word.!word))
      )
  & Done

```


Optimized (~100 x faster):

```bracmat

    ( shuffle
    =   m car cdr todo a z count M string tried
      .     !arg:(@(?:%?car ?cdr).?todo)
          & !Count:?count
          & :?tried
          & (   @( !todo
                 :   ?a
                     ( %@?M
                     & ~(!tried:? !M ?)
                     & !M !tried:?tried
                     & !M:~!car
                     )
                     ( ?z
                     &   shuffle$(!cdr.str$(!a !z))
                       : (<!count:?count.?string)
                     & !M:?m
                     & ~
                     )
                 )
              | !count:<!Count
              |   @(!todo:%?m ?z)
                & shuffle$(!cdr.!z):(?count.?string)
                & !count+1
            . !m !string
            )
        | (0.)
    )
  & abracadabra seesaw elk grrrrrr up a:?words
  &   whl
    ' ( !words:%?word ?words
      & @(!word:? [?Count)
      & out$(!word shuffle$(!word.!word))
      )
  & Done

```

Output:

```txt

abracadabra (0.b a a r a c a d r a b)
seesaw (0.e s s e w a)
elk (0.l k e)
grrrrrr (5.r g r r r r r)
up (0.p u)
a (1.a)
{!} Done

```



## C


This approach is totally deterministic, and is based on the final J implementation from the talk page.

In essence: we form cyclic groups of character indices where each cyclic group is guaranteed to represent each character only once (two instances of the letter 'a' must have their indices in separate groups), and then we rotate each of the cyclic groups.  We then use the before/after version of these cycles to shuffle the original text.  The only way a character can be repeated, here, is when a cyclic group contains only one character index, and this can only happen when more than half of the text uses that character. This is C99 code.


```cpp
#include <iostream>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

#define DEBUG

void best_shuffle(const char* txt, char* result) {
    const size_t len = strlen(txt);
    if (len == 0)
        return;

#ifdef DEBUG
    // txt and result must have the same length
    assert(len == strlen(result));
#endif

    // how many of each character?
    size_t counts[UCHAR_MAX];
    memset(counts, '\0', UCHAR_MAX * sizeof(int));
    size_t fmax = 0;
    for (size_t i = 0; i < len; i++) {
        counts[(unsigned char)txt[i]]++;
        const size_t fnew = counts[(unsigned char)txt[i]];
        if (fmax < fnew)
             fmax = fnew;
    }
    assert(fmax > 0 && fmax <= len);

    // all character positions, grouped by character
    size_t *ndx1 = malloc(len * sizeof(size_t));
    if (ndx1 == NULL)
        exit(EXIT_FAILURE);
    for (size_t ch = 0, i = 0; ch < UCHAR_MAX; ch++)
       if (counts[ch])
            for (size_t j = 0; j < len; j++)
                if (ch == (unsigned char)txt[j]) {
                    ndx1[i] = j;
                    i++;
                }

    // regroup them for cycles
    size_t *ndx2 = malloc(len * sizeof(size_t));
    if (ndx2 == NULL)
        exit(EXIT_FAILURE);
    for (size_t i = 0, n = 0, m = 0; i < len; i++) {
        ndx2[i] = ndx1[n];
        n += fmax;
        if (n >= len) {
            m++;
            n = m;
        }
    }

    // how long can our cyclic groups be?
    const size_t grp = 1 + (len - 1) / fmax;
    assert(grp > 0 && grp <= len);

    // how many of them are full length?
    const size_t lng = 1 + (len - 1) % fmax;
    assert(lng > 0 && lng <= len);

    // rotate each group
    for (size_t i = 0, j = 0; i < fmax; i++) {
        const size_t first = ndx2[j];
        const size_t glen = grp - (i < lng ? 0 : 1);
        for (size_t k = 1; k < glen; k++)
            ndx1[j + k - 1] = ndx2[j + k];
        ndx1[j + glen - 1] = first;
        j += glen;
    }

    // result is original permuted according to our cyclic groups
    result[len] = '\0';
    for (size_t i = 0; i < len; i++)
        result[ndx2[i]] = txt[ndx1[i]];

    free(ndx1);
    free(ndx2);
}

void display(const char* txt1, const char* txt2) {
    const size_t len = strlen(txt1);
    assert(len == strlen(txt2));
    int score = 0;
    for (size_t i = 0; i < len; i++)
        if (txt1[i] == txt2[i])
            score++;
    (void)printf("%s, %s, (%u)\n", txt1, txt2, score);
}

int main() {
    const char* data[] = {"abracadabra", "seesaw", "elk", "grrrrrr",
                          "up", "a", "aabbbbaa", "", "xxxxx"};
    const size_t data_len = sizeof(data) / sizeof(data[0]);
    for (size_t i = 0; i < data_len; i++) {
        const size_t shuf_len = strlen(data[i]) + 1;
        char shuf[shuf_len];

#ifdef DEBUG
        memset(shuf, 0xFF, sizeof shuf);
        shuf[shuf_len - 1] = '\0';
#endif

        best_shuffle(data[i], shuf);
        display(data[i], shuf);
    }

    return EXIT_SUCCESS;
}
```

Output:

```txt
abracadabra, brabacadaar, (0)
seesaw, wssaee, (0)
elk, kel, (0)
grrrrrr, rrrrrrg, (5)
up, pu, (0)
a, a, (1)
aabbbbaa, bbaaaabb, (0)
, , (0)
xxxxx, xxxxx, (5)
```



### Version with random result


```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct letter_group_t {
	char c;
	int count;
} *letter_p;

struct letter_group_t all_letters[26];
letter_p letters[26];

/* counts how many of each letter is in a string, used later
 * to generate permutations
 */
int count_letters(const char *s)
{
	int i, c;
	for (i = 0; i < 26; i++) {
		all_letters[i].count = 0;
		all_letters[i].c = i + 'a';
	}
	while (*s != '\0') {
		i = *(s++);

		/* don't want to deal with bad inputs */
		if (i < 'a' || i > 'z') {
			fprintf(stderr, "Abort: Bad string %s\n", s);
			exit(1);
		}

		all_letters[i - 'a'].count++;
	}
	for (i = 0, c = 0; i < 26; i++)
		if (all_letters[i].count)
			letters[c++] = all_letters + i;

	return c;
}

int least_overlap, seq_no;
char out[100], orig[100], best[100];

void permutate(int n_letters, int pos, int overlap)
{
	int i, ol;
	if (pos < 0) {
                /* if enabled will show all shuffles no worse than current best */
	//	printf("%s: %d\n", out, overlap);

                /* if better than current best, replace it and reset counter */
		if (overlap < least_overlap) {
			least_overlap = overlap;
			seq_no = 0;
		}

                /* the Nth best tie has 1/N chance of being kept, so all ties
                 * have equal chance of being selected even though we don't
                 * how many there are before hand
                 */
		if ( (double)rand() / (RAND_MAX + 1.0) * ++seq_no <= 1)
			strcpy(best, out);

		return;
	}

        /* standard "try take the letter; try take not" recursive method */
	for (i = 0; i < n_letters; i++) {
		if (!letters[i]->count) continue;

		out[pos] = letters[i]->c;
		letters[i]->count --;
		ol = (letters[i]->c == orig[pos]) ? overlap + 1 : overlap;

                /* but don't try options that's already worse than current best */
		if (ol <= least_overlap)
			permutate(n_letters, pos - 1, ol);

		letters[i]->count ++;
	}
	return;
}

void do_string(const char *str)
{
	least_overlap = strlen(str);
	strcpy(orig, str);

	seq_no = 0;
	out[least_overlap] = '\0';
	least_overlap ++;

	permutate(count_letters(str), least_overlap - 2, 0);
	printf("%s -> %s, overlap %d\n", str, best, least_overlap);
}

int main()
{
	srand(time(0));
	do_string("abracadebra");
	do_string("grrrrrr");
	do_string("elk");
	do_string("seesaw");
	do_string("");
	return 0;
}
```
Output<lang>abracadebra -> edbcarabaar, overlap 0
grrrrrr -> rrgrrrr, overlap 5
elk -> kel, overlap 0
seesaw -> ewsesa, overlap 0
 -> , overlap 0
```



### Deterministic method


```c
#include <stdio.h>
#include <string.h>

#define FOR(x, y) for(x = 0; x < y; x++)
char *best_shuffle(const char *s, int *diff)
{
	int i, j = 0, max = 0, l = strlen(s), cnt[128] = {0};
	char buf[256] = {0}, *r;

	FOR(i, l) if (++cnt[(int)s[i]] > max) max = cnt[(int)s[i]];
	FOR(i, 128) while (cnt[i]--) buf[j++] = i;

	r = strdup(s);
	FOR(i, l) FOR(j, l)
		if (r[i] == buf[j]) {
			r[i] = buf[(j + max) % l] & ~128;
			buf[j] |= 128;
			break;
		}

	*diff = 0;
	FOR(i, l) *diff += r[i] == s[i];

	return r;
}

int main()
{
	int i, d;
	const char *r, *t[] = {"abracadabra", "seesaw", "elk", "grrrrrr", "up", "a", 0};
	for (i = 0; t[i]; i++) {
		r = best_shuffle(t[i], &d);
		printf("%s %s (%d)\n", t[i], r, d);
	}
	return 0;
}
```



## C++

{{works with|C++|11}}
{{trans|Java}}

```cpp
#include <iostream>
#include <sstream>
#include <algorithm>

using namespace std;

template <class S>
class BestShuffle {
public:
    BestShuffle() : rd(), g(rd()) {}

    S operator()(const S& s1) {
        S s2 = s1;
        shuffle(s2.begin(), s2.end(), g);
        for (unsigned i = 0; i < s2.length(); i++)
            if (s2[i] == s1[i])
                for (unsigned j = 0; j < s2.length(); j++)
                    if (s2[i] != s2[j] && s2[i] != s1[j] && s2[j] != s1[i]) {
                        swap(s2[i], s2[j]);
                        break;
                    }
        ostringstream os;
        os << s1 << endl << s2 << " [" << count(s2, s1) << ']';
        return os.str();
    }

private:
    static int count(const S& s1, const S& s2) {
        auto count = 0;
        for (unsigned i = 0; i < s1.length(); i++)
            if (s1[i] == s2[i])
                count++;
        return count;
    }

    random_device rd;
    mt19937 g;
};

int main(int argc, char* arguments[]) {
    BestShuffle<basic_string<char>> bs;
    for (auto i = 1; i < argc; i++)
        cout << bs(basic_string<char>(arguments[i])) << endl;
    return 0;
}
```

{{out}}

```txt
abracadabra
raabadabcar (0)
seesaw
wssaee (0)
grrrrrr
rgrrrrr (5)
pop
opp (1)
up
pu (0)
a
a (1)
```


## C#
For both solutions, a class is used to encapsulate the original string and to scrambling. A private function of the class does the actual sorting. An implicit conversion from string is also provided to allow for simple initialization, e.g.:

```c#
ShuffledString[] array = {"cat", "dog", "mouse"};
```

Which will immediately shuffle each word.

A sequential solution, which always produces the same output for the same input.

```c#

using System;
using System.Text;
using System.Collections.Generic;

namespace BestShuffle_RC
{
    public class ShuffledString
    {
        private string original;
        private StringBuilder shuffled;
        private int ignoredChars;

        public string Original
        {
            get { return original; }
        }

        public string Shuffled
        {
            get { return shuffled.ToString(); }
        }

        public int Ignored
        {
            get { return ignoredChars; }
        }

        private void Swap(int pos1, int pos2)
        {
            char temp = shuffled[pos1];
            shuffled[pos1] = shuffled[pos2];
            shuffled[pos2] = temp;
        }

        //Determine if a swap between these two would put a letter in a "bad" place
        //If true, a swap is OK.
        private bool TrySwap(int pos1, int pos2)
        {
            if (original[pos1] == shuffled[pos2] || original[pos2] == shuffled[pos1])
                return false;
            else
                return true;
        }

        //Constructor carries out calls Shuffle function.
        public ShuffledString(string word)
        {
            original = word;
            shuffled = new StringBuilder(word);
            Shuffle();
            DetectIgnores();
        }

        //Does the hard work of shuffling the string.
        private void Shuffle()
        {
            int length = original.Length;
            int swaps;
            Random rand = new Random();
            List<int> used = new List<int>();

            for (int i = 0; i < length; i++)
            {
                swaps = 0;
                while(used.Count <= length - i)//Until all possibilities have been tried
                {
                    int j = rand.Next(i, length - 1);
                    //If swapping would make a difference, and wouldn't put a letter in a "bad" place,
                    //and hasn't already been tried, then swap
                    if (original[i] != original[j] && TrySwap(i, j) && !used.Contains(j))
                    {
                        Swap(i, j);
                        swaps++;
                        break;
                    }
                    else
                        used.Add(j);//If swapping doesn't work, "blacklist" the index
                }
                if (swaps == 0)
                {
                    //If a letter was ignored (no swap was found), look backward for another change to make
                    for (int k = i; k >= 0; k--)
                    {
                        if (TrySwap(i, k))
                            Swap(i, k);
                    }
                }
                //Clear the used indeces
                used.Clear();
            }
        }

        //Count how many letters are still in their original places.
        private void DetectIgnores()
        {
            int ignores = 0;
            for (int i = 0; i < original.Length; i++)
            {
                if (original[i] == shuffled[i])
                    ignores++;
            }

            ignoredChars = ignores;
        }

        //To allow easy conversion of strings.
        public static implicit operator ShuffledString(string convert)
        {
            return new ShuffledString(convert);
        }
    }

    public class Program
    {
        public static void Main(string[] args)
        {
            ShuffledString[] words = { "abracadabra", "seesaw", "elk", "grrrrrr", "up", "a" };

            foreach(ShuffledString word in words)
                Console.WriteLine("{0}, {1}, ({2})", word.Original, word.Shuffled, word.Ignored);

            Console.ReadKey();
        }
    }
}

```


And a randomized solution, which will produce a more or less different result on every run:

```c#

using System;
using System.Text;
using System.Collections.Generic;

namespace BestShuffle_RC
{
    public class ShuffledString
    {
        private string original;
        private StringBuilder shuffled;
        private int ignoredChars;

        public string Original
        {
            get { return original; }
        }

        public string Shuffled
        {
            get { return shuffled.ToString(); }
        }

        public int Ignored
        {
            get { return ignoredChars; }
        }

        private void Swap(int pos1, int pos2)
        {
            char temp = shuffled[pos1];
            shuffled[pos1] = shuffled[pos2];
            shuffled[pos2] = temp;
        }

        //Determine if a swap between these two would put a letter in a "bad" place
        //If true, a swap is OK.
        private bool TrySwap(int pos1, int pos2)
        {
            if (original[pos1] == shuffled[pos2] || original[pos2] == shuffled[pos1])
                return false;
            else
                return true;
        }

        //Constructor carries out calls Shuffle function.
        public ShuffledString(string word)
        {
            original = word;
            shuffled = new StringBuilder(word);
            Shuffle();
            DetectIgnores();
        }

        //Does the hard work of shuffling the string.
        private void Shuffle()
        {
            int length = original.Length;
            int swaps;
            Random rand = new Random();
            List<int> used = new List<int>();

            for (int i = 0; i < length; i++)
            {
                swaps = 0;
                while(used.Count <= length - i)//Until all possibilities have been tried
                {
                    int j = rand.Next(i, length - 1);
                    //If swapping would make a difference, and wouldn't put a letter in a "bad" place,
                    //and hasn't already been tried, then swap
                    if (original[i] != original[j] && TrySwap(i, j) && !used.Contains(j))
                    {
                        Swap(i, j);
                        swaps++;
                        break;
                    }
                    else
                        used.Add(j);//If swapping doesn't work, "blacklist" the index
                }
                if (swaps == 0)
                {
                    //If a letter was ignored (no swap was found), look backward for another change to make
                    for (int k = i; k >= 0; k--)
                    {
                        if (TrySwap(i, k))
                            Swap(i, k);
                    }
                }
                //Clear the used indeces
                used.Clear();
            }
        }

        //Count how many letters are still in their original places.
        private void DetectIgnores()
        {
            int ignores = 0;
            for (int i = 0; i < original.Length; i++)
            {
                if (original[i] == shuffled[i])
                    ignores++;
            }

            ignoredChars = ignores;
        }

        //To allow easy conversion of strings.
        public static implicit operator ShuffledString(string convert)
        {
            return new ShuffledString(convert);
        }
    }

    public class Program
    {
        public static void Main(string[] args)
        {
            ShuffledString[] words = { "abracadabra", "seesaw", "elk", "grrrrrr", "up", "a" };

            foreach(ShuffledString word in words)
                Console.WriteLine("{0}, {1}, ({2})", word.Original, word.Shuffled, word.Ignored);

            Console.ReadKey();
        }
    }
}

```


A sample output for the sequential shuffle:

```txt

abracadabra, rdabarabaac, (0)
seesaw, easwse, (0)
elk, lke, (0)
grrrrrr, rgrrrrr, (5)
up, pu, (0)
a, a, (1)
hounddog, unddohgo, (0)

```


A sample of the randomized shuffle:

```txt

abracadabra, raacarbdaab, (0)
seesaw, essewa, (0)
elk, lke, (0)
grrrrrr, rrrgrrr, (5)
up, pu, (0)
a, a, (1)

```



## Clojure

Uses same method as J


```Clojure
(defn score [before after]
   (->> (map = before after)
	(filter true? ,)
	count))

(defn merge-vecs [init vecs]
  (reduce (fn [counts [index x]]
		 (assoc counts x (conj (get counts x []) index)))
	  init vecs))

(defn frequency
  "Returns a collection of indecies of distinct items"
  [coll]
  (->> (map-indexed vector coll)
       (merge-vecs {} ,)))

(defn group-indecies [s]
  (->> (frequency s)
       vals
       (sort-by count ,)
       reverse))

(defn cycles [coll]
  (let [n (count (first coll))
	cycle (cycle (range n))
	coll (apply concat coll)]
    (->> (map vector coll cycle)
	 (merge-vecs [] ,))))

(defn rotate [n coll]
  (let [c (count coll)
	n (rem (+ c n) c)]
    (concat (drop n coll) (take n coll))))

(defn best-shuffle [s]
  (let [ref (cycles (group-indecies s))
	prm (apply concat (map (partial rotate 1) ref))
	ref (apply concat ref)]
    (->> (map vector ref prm)
	 (sort-by first ,)
	 (map second ,)
	 (map (partial get s) ,)
	 (apply str ,)
	 (#(vector s % (score s %))))))

user> (->> ["abracadabra" "seesaw" "elk" "grrrrrr" "up" "a"]
	   (map best-shuffle ,)
	   vec)
[["abracadabra" "bdabararaac" 0]
 ["seesaw" "eawess" 0]
 ["elk" "lke" 0]
 ["grrrrrr" "rgrrrrr" 5]
 ["up" "pu" 0]
 ["a" "a" 1]]
```



## Common Lisp


```lisp
(defun count-equal-chars (string1 string2)
  (loop for c1 across string1 and c2 across string2
        count (char= c1 c2)))

(defun shuffle (string)
  (let ((length (length string))
        (result (copy-seq string)))
    (dotimes (i length result)
      (dotimes (j length)
        (when (and (/= i j)
                   (char/= (aref string i) (aref result j))
                   (char/= (aref string j) (aref result i)))
          (rotatef (aref result i) (aref result j)))))))

(defun best-shuffle (list)
  (dolist (string list)
    (let ((shuffled (shuffle string)))
      (format t "~%~a ~a (~a)"
              string
              shuffled
              (count-equal-chars string shuffled)))))

(best-shuffle '("abracadabra" "seesaw" "elk" "grrrrrr" "up" "a"))
```

Output:
 abracadabra caadrbabaar (0)
 seesaw ewaess (0)
 elk kel (0)
 grrrrrr rgrrrrr (5)
 up pu (0)
 a a (1)


### Version 2


```lisp
(defun all-best-shuffles (str)
  (let (tbl out (shortest (length str)) (s str))

    (labels ((perm (ar l tmpl res overlap)
               (when (> overlap shortest)
                 (return-from perm))
               (when (zerop l) ; max depth of perm
                 (when (< overlap shortest)
                   (setf shortest overlap out '()))
                 (when (= overlap shortest)
                   (setf res (reverse (format nil "~{~c~^~}" res)))
                   (push (list res overlap) out)
                   (return-from perm)))
               (decf l)
               (dolist (x ar)
                 (when (plusp (cdr x))
                   (when (char= (car x) (char tmpl l))
                     (incf overlap))
                   (decf (cdr x))
                   (push (car x) res)
                   (perm ar l tmpl res overlap)
                   (pop res)
                   (incf (cdr x))
                   (when (char= (car x) (char tmpl l))
                     (decf overlap))))))

      (loop while (plusp (length s)) do
            (let* ((c (char s 0))
                   (l (count c s)))
              (push (cons c l) tbl)
              (setf s (remove c s))))

      (perm tbl (length str) (reverse str) '() 0))
    out))

(defun best-shuffle (str)
  "Algorithm: list all best shuffles, then pick one"
  (let ((c (all-best-shuffles str)))
    (elt c (random (length c)))))

(format t "All best shuffles:")
(print (all-best-shuffles "seesaw"))

(format t "~%~%Random best shuffles:~%")
(dolist (s (list "abracadabra" "seesaw" "elk" "grrrrrr" "up" "a"))
  (format t "~A: ~A~%" s (best-shuffle s)))

```


The output is:

```lisp
abracadabra: (caardrabaab 0)
seesaw: (ewsase 0)
elk: (kel 0)
grrrrrr: (rrrgrrr 5)
up: (pu 0)
a: (a 1)

```


== {{header|Crystal}} ==
{{trans|Ruby}}


```ruby
def best_shuffle(s)
  # Fill _pos_ with positions in the order
  # that we want to fill them.
  pos = [] of Int32
  # g["a"] = [2, 4] implies that s[2] == s[4] == "a"
  g = s.size.times.group_by { |i| s[i] }

  # k sorts letters from low to high count
  # k = g.sort_by { |k, v| v.length }.map { |k, v| k }        # in Ruby
  # k = g.to_a.sort_by { |(k, v)| v.size }.map { |(k, v)| k } # Crystal direct
  k = g.to_a.sort_by { |h| h[1].size }.map { |h| h[0] }       # Crystal shorter

  until g.empty?
    k.each do |letter|
      g.has_key?(letter) || next          # next unless g.has_key? letter
      pos << g[letter].pop
      g[letter].empty? && g.delete letter # g.delete(letter) if g[letter].empty?
    end
  end

  # Now fill in _new_ with _letters_ according to each position
  # in _pos_, but skip ahead in _letters_ if we can avoid
  # matching characters that way.
  letters = s.dup
  new = "?" * s.size

  until letters.empty?
    i, p = 0, pos.pop
    while letters[i] == s[p] && i < (letters.size - 1); i += 1 end
    # new[p] = letters.slice! i                            # in Ruby
    new = new.sub(p, letters[i]); letters = letters.sub(i, "")
  end
  score = new.chars.zip(s.chars).count { |c, d| c == d }
  {new, score}
end

%w(abracadabra seesaw elk grrrrrr up a).each do |word|
  # puts "%s, %s, (%d)" % [word, *best_shuffle(word)]      # in Ruby
  new, score = best_shuffle(word)
  puts "%s, %s, (%d)" % [word, new, score]
end
```


{{out}}

```txt

abracadabra, baarrcadaab, (0)
seesaw, essewa, (0)
elk, lke, (0)
grrrrrr, rgrrrrr, (5)
up, pu, (0)
a, a, (1)

```



## D


### Version with random result

Translation of [[Best_shuffle#Icon_and_Unicon|Icon]] via [[Best_shuffle#AWK|AWK]]

```d
import std.stdio, std.random, std.algorithm, std.conv, std.range,
       std.traits, std.typecons;

auto bestShuffle(S)(in S orig) @safe if (isSomeString!S) {
    static if (isNarrowString!S)
        immutable o = orig.dtext;
    else
        alias o = orig;

    auto s = o.dup;
    s.randomShuffle;

    foreach (immutable i, ref ci; s) {
        if (ci != o[i])
            continue;
        foreach (immutable j, ref cj; s)
            if (ci != cj && ci != o[j] && cj != o[i]) {
                swap(ci, cj);
                break;
            }
    }

    return tuple(s, s.zip(o).count!q{ a[0] == a[1] });
} unittest {
    assert("abracadabra".bestShuffle[1] == 0);
    assert("immediately".bestShuffle[1] == 0);
    assert("grrrrrr".bestShuffle[1] == 5);
    assert("seesaw".bestShuffle[1] == 0);
    assert("pop".bestShuffle[1] == 1);
    assert("up".bestShuffle[1] == 0);
    assert("a".bestShuffle[1] == 1);
    assert("".bestShuffle[1] == 0);
}

void main(in string[] args) @safe {
    if (args.length > 1) {
        immutable entry = args.dropOne.join(' ');
        const res = entry.bestShuffle;
        writefln("%s : %s (%d)", entry, res[]);
    }
}
```



### Deterministic approach


```d
import std.stdio, std.algorithm, std.range;

extern(C) pure nothrow void* alloca(in size_t size);

void bestShuffle(in char[] txt, ref char[] result) pure nothrow {
    // Assume alloca to be pure.
    //extern(C) pure nothrow void* alloca(in size_t size);
    enum size_t NCHAR = size_t(char.max + 1);
    enum size_t MAX_VLA_SIZE = 1024;
    immutable size_t len = txt.length;
    if (len == 0)
        return;

    // txt and result must have the same length
    // allocate only when necessary
    if (result.length != len)
        result.length = len;

    // how many of each character?
    size_t[NCHAR] counts;
    size_t fmax = 0;
    foreach (immutable char c; txt) {
        counts[c]++;
        if (fmax < counts[c])
            fmax = counts[c];
    }
    assert(fmax > 0 && fmax <= len);

    // all character positions, grouped by character
    size_t[] ndx1;
    {
        size_t* ptr1;
        if ((len * size_t.sizeof) < MAX_VLA_SIZE)
            ptr1 = cast(size_t*)alloca(len * size_t.sizeof);
        // If alloca() has failed, or the memory needed is too much
        // large, then allocate from the heap.
        ndx1 = (ptr1 == null) ? new size_t[len] : ptr1[0 .. len];
    }
    {
        int pos = 0;
        foreach (immutable size_t ch; 0 .. NCHAR)
           if (counts[ch])
                foreach (j, char c; txt)
                    if (c == ch) {
                        ndx1[pos] = j;
                        pos++;
                    }
    }

    // regroup them for cycles
    size_t[] ndx2;
    {
        size_t* ptr2;
        if ((len * size_t.sizeof) < MAX_VLA_SIZE)
            ptr2 = cast(size_t*)alloca(len * size_t.sizeof);
        ndx2 = (ptr2 == null) ? new size_t[len] : ptr2[0 .. len];
    }
    {
        size_t n, m;
        foreach (immutable size_t i; 0 .. len) {
            ndx2[i] = ndx1[n];
            n += fmax;
            if (n >= len) {
                m++;
                n = m;
            }
        }
    }

    // How long can our cyclic groups be?
    immutable size_t grp = 1 + (len - 1) / fmax;

    // How many of them are full length?
    immutable size_t lng = 1 + (len - 1) % fmax;

    // Rotate each group.
    {
        size_t j;
        foreach (immutable size_t i; 0 .. fmax) {
            immutable size_t first = ndx2[j];
            immutable size_t glen = grp - (i < lng ? 0 : 1);
            foreach (immutable size_t k; 1 .. glen)
                ndx1[j + k - 1] = ndx2[j + k];
            ndx1[j + glen - 1] = first;
            j += glen;
        }
    }

    // Result is original permuted according to our cyclic groups.
    foreach (immutable size_t i; 0 .. len)
        result[ndx2[i]] = txt[ndx1[i]];
}

void main() {
    auto data = ["abracadabra", "seesaw", "elk", "grrrrrr",
                 "up", "a", "aabbbbaa", "", "xxxxx"];
    foreach (txt; data) {
        auto result = txt.dup;
        bestShuffle(txt, result);
        immutable nEqual = zip(txt, result).count!q{ a[0] == a[1] };
        writefln("%s, %s, (%d)", txt, result, nEqual);
    }
}
```

{{out}}

```txt
abracadabra, brabacadaar, (0)
seesaw, wssaee, (0)
elk, kel, (0)
grrrrrr, rrrrrrg, (5)
up, pu, (0)
a, a, (1)
aabbbbaa, bbaaaabb, (0)
, , (0)
xxxxx, xxxxx, (5)
```



## Elena

ELENA 4.1 :

```Elena
import system'routines;
import extensions;
import extensions'text;

extension op
{
    get Shuffled()
    {
        var original := self.toArray();
        var shuffled := self.toArray();

        for (int i := 0, i < original.Length, i += 1) {
            for (int j := 0, j < original.Length, j += 1) {
                if (i != j && original[i] != shuffled[j] && original[j] != shuffled[i])
                {
                    shuffled.exchange(i,j)
                }
            }
        };

        ^ shuffled.summarize(new StringWriter()).toString()
    }

    score(originalText)
    {
        var shuffled := self.toArray();
        var original := originalText.toArray();
        int score := 0;

        for (int i := 0, i < original.Length, i += 1) {
            if (original[i] == shuffled[i]) { score += 1 }
        };

        ^ score
    }
}

public program()
{
    new::("abracadabra", "seesaw", "grrrrrr", "pop", "up", "a").forEach:(s)
    {
        var shuffled_s := s.Shuffled;

        console.printLine("The best shuffle of ",s," is ",shuffled_s,"(",shuffled_s.score(s),")")
    };

    console.readChar()
}
```

{{out}}

```txt

The best shuffle of abracadabra is caadrbabaar(0)
The best shuffle of seesaw is ewaess(0)
The best shuffle of grrrrrr is rgrrrrr(5)
The best shuffle of pop is opp(1)
The best shuffle of up is pu(0)
The best shuffle of a is a(1)

```



## Erlang

Deterministic version.

```Erlang

-module( best_shuffle ).

-export( [sameness/2, string/1, task/0] ).

sameness( String1, String2 ) -> lists:sum( [1 || {X, X} <- lists:zip(String1, String2)] ).

string( String ) ->
	{"", String, Acc} = lists:foldl( fun different/2, {lists:reverse(String), String, []}, String ),
	lists:reverse( Acc ).

task() ->
	Strings = ["abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"],
	Shuffleds = [string(X) || X <- Strings],
	[io:fwrite("~p ~p ~p~n", [X, Y, sameness(X,Y)]) || {X, Y} <- lists:zip(Strings, Shuffleds)].



different( Character, {[Character], Original, Acc} ) ->
	try_to_save_last( Character, Original, Acc );
different( Character, {[Character | T]=Not_useds, Original, Acc} ) ->
	Different_or_same = different_or_same( [X || X <- T, X =/= Character], Character ),
	{lists:delete(Different_or_same, Not_useds), Original, [Different_or_same | Acc]};
different( _Character1, {[Character2 | T], Original, Acc} ) ->
	{T, Original, [Character2 | Acc]}.

different_or_same( [Different | _T], _Character ) -> Different;
different_or_same( [], Character ) -> Character.

try_to_save_last( Character, Original_string, Acc ) ->
	Fun = fun ({X, Y}) -> (X =:= Y) orelse (X =:= Character) end,
	New_acc = try_to_save_last( lists:splitwith(Fun, lists:zip(lists:reverse(Original_string), [Character | Acc])), [Character | Acc] ),
	{"", Original_string, New_acc}.

try_to_save_last( {_Not_split, []}, Acc ) -> Acc;
try_to_save_last( {Last_reversed_zip, First_reversed_zip}, _Acc ) ->
	{_Last_reversed_original, [Last_character_acc | Last_part_acc]} = lists:unzip( Last_reversed_zip ),
	{_First_reversed_original, [Character_acc | First_part_acc]} = lists:unzip( First_reversed_zip ),
	[Character_acc | Last_part_acc] ++ [Last_character_acc | First_part_acc].

```

{{out}}

```txt

32> best_shuffle:task().
"abracadabra" "rabdacaraab" 0
"seesaw" "wasees" 0
"elk" "kel" 0
"grrrrrr" "rgrrrrr" 5
"up" "pu" 0
"a" "a" 1

```



## Go

{{trans|Icon and Unicon}}

```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

var ts = []string{"abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"}

func main() {
    rand.Seed(time.Now().UnixNano())
    for _, s := range ts {
        // create shuffled byte array of original string
        t := make([]byte, len(s))
        for i, r := range rand.Perm(len(s)) {
            t[i] = s[r]
        }
        // algorithm of Icon solution
        for i := range t {
            for j := range t {
                if i != j && t[i] != s[j] && t[j] != s[i] {
                    t[i], t[j] = t[j], t[i]
                    break
                }
            }
        }
        // count unchanged and output
        var count int
        for i, ic := range t {
            if ic == s[i] {
                count++
            }
        }
        fmt.Printf("%s -> %s (%d)\n", s, string(t), count)
    }
}
```

{{out|Output of two runs}}

```txt

abracadabra -> raaracbbaad (0)
seesaw -> asswee (0)
elk -> lke (0)
grrrrrr -> rgrrrrr (5)
up -> pu (0)
a -> a (1)

```


```txt

abracadabra -> raadabaracb (0)
seesaw -> wsseea (0)
elk -> kel (0)
grrrrrr -> rrrrrgr (5)
up -> pu (0)
a -> a (1)

```



## Groovy


```groovy
def shuffle(text) {
    def shuffled = (text as List)
    for (sourceIndex in 0..<text.size()) {
        for (destinationIndex in 0..<text.size()) {
                if (shuffled[sourceIndex] != shuffled[destinationIndex] && shuffled[sourceIndex] != text[destinationIndex] && shuffled[destinationIndex] != text[sourceIndex]) {
                    char tmp = shuffled[sourceIndex];
                    shuffled[sourceIndex] = shuffled[destinationIndex];
                    shuffled[destinationIndex] = tmp;
                    break;
                }
        }
    }
    [original: text, shuffled: shuffled.join(""), score: score(text, shuffled)]
}

def score(original, shuffled) {
    int score = 0
    original.eachWithIndex { character, index ->
        if (character == shuffled[index]) {
            score++
        }
    }
    score
}

["abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"].each { text ->
    def result = shuffle(text)
    println "${result.original}, ${result.shuffled}, (${result.score})"
}
```

Output:

```txt

abracadabra, baaracadabr, (0)
seesaw, esswea, (0)
elk, lke, (0)
grrrrrr, rgrrrrr, (5)
up, pu, (0)
a, a, (1)

```



## Haskell


We demonstrate several approaches here. In order to test the program we define a testing suite:


```Haskell
shufflingQuality l1 l2 = length $ filter id $ zipWith (==) l1 l2

printTest prog = mapM_ test texts
  where
    test s = do
      x <- prog s
      putStrLn $ unwords $ [ show s
                           , show x
                           , show $ shufflingQuality s x]
    texts = [ "abba", "abracadabra", "seesaw", "elk" , "grrrrrr"
            , "up", "a", "aaaaa.....bbbbb"
            , "Rosetta Code is a programming chrestomathy site." ]
```


=== Deterministic List-based solution ===

The core of the algorithm is swapping procedure similar to those implemented in AWK and Icon examples. It could be done by a pure program with use of immutable vectors (though it is possible to use mutable vectors living in <tt>ST</tt> or <tt>IO</tt>, but it won't make the program more clear).


```Haskell
import Data.Vector ((//), (!))
import qualified Data.Vector as V
import Data.List (delete, find)

swapShuffle :: Eq a => [a] -> [a] -> [a]
swapShuffle lref lst = V.toList $ foldr adjust (V.fromList lst) [0..n-1]
  where
    vref = V.fromList lref
    n = V.length vref
    adjust i v = case find alternative [0.. n-1] of
      Nothing -> v
      Just j -> v // [(j, v!i), (i, v!j)]
      where
        alternative j = and [ v!i == vref!i
                            , i /= j
                            , v!i /= vref!j
                            , v!j /= vref!i ]

shuffle :: Eq a => [a] -> [a]
shuffle lst = swapShuffle lst lst
```


{{Out}}

```txt
λ> printTest (pure . shuffle)
"abba" "baab" 0
"abracadabra" "daabacarrab" 0
"seesaw" "esaews" 0
"elk" "lke" 0
"grrrrrr" "rrrrrrg" 5
"up" "pu" 0
"a" "a" 1
"aaaaa.....bbbbb" ".....bbbbbaaaaa" 0
"Rosetta Code is a programming chrestomathy site." "stetma Code is a programoing chrestomathy site.R" 0
```


The program works but shuffling is not good in case of a real text, which was just shifted. We can make it better using [[Perfect shuffle]] (faro shuffle) before the swapping procedure.


```Haskell
perfectShuffle :: [a] -> [a]
perfectShuffle [] = []
perfectShuffle lst | odd n = b : shuffle (zip bs a)
                   | even n = shuffle (zip (b:bs) a)
  where
    n = length lst
    (a,b:bs) = splitAt (n `div` 2) lst
    shuffle = foldMap (\(x,y) -> [x,y])

shuffleP :: Eq a => [a] -> [a]
shuffleP lst = swapShuffle lst $ perfectShuffle lst
```


{{Out}}

```txt
λ> qualityTest (pure . shuffleP)
"abba" "baab" 0
"abracadabra" "baadabrraac" 0
"seesaw" "assewe" 0
"elk" "lke" 0
"grrrrrr" "rrgrrrr" 5
"up" "pu" 0
"a" "a" 1
"aaaaa.....bbbbb" "bbb.baaaaba...." 0
"Rosetta Code is a programming chrestomathy site." " Rmoisnegt tcahmrCeosdteo miast hay  psriotger.a" 0
```


That's much better.

=== Nondeterministic List-based solution ===

Adding randomness is easy: just perform random shuffle before swapping procedure.

Additional import:


```Haskell
import Control.Monad.Random (getRandomR)
```



```Haskell
randomShuffle :: [a] -> IO [a]
randomShuffle [] = return []
randomShuffle lst = do
  i <- getRandomR (0,length lst-1)
  let (a, x:b) = splitAt i lst
  xs <- randomShuffle $ a ++ b
  return (x:xs)

shuffleR :: Eq a => [a] -> IO [a]
shuffleR lst = swapShuffle lst <$> randomShuffle lst
```


{{Out}}

```txt
λ> qualityTest shuffleR
"abba" "baab" 0
"abracadabra" "raacadababr" 0
"seesaw" "wsaese" 0
"elk" "kel" 0
"grrrrrr" "rrrgrrr" 5
"up" "pu" 0
"a" "a" 1
"aaaaa.....bbbbb" "b.b.baababa.a.." 0
"Rosetta Code is a programming chrestomathy site." "esodmnithsrasrmeogReat taoCp gtrty i .mi as ohce" 0
```


Now everything is Ok except for the efficiency. Both randomization and swapping procedure are O[n^2], moreover the whole text must be kept in memory, so for large data sequences it will take a while to shuffle.

=== Nondeterministic Conduit-based solution ===

Using streaming technique it is possible to shuffle the sequence on the fly, using relatively small moving window (say of length k) for shuffling procedure. In that case the program will consume constant memory amount O[k] and require O[n*k] operations.


```Haskell
{-# LANGUAGE TupleSections, LambdaCase #-}
import Conduit
import Control.Monad.Random (getRandomR)
import Data.List (delete, find)

shuffleC :: Eq a => Int -> Conduit a IO a
shuffleC 0 = awaitForever yield
shuffleC k = takeC k .| sinkList >>= \v -> delay v .| randomReplace v

delay :: Monad m => [a] -> Conduit t m (a, [a])
delay [] = mapC $ \x -> (x,[x])
delay (b:bs) = await >>= \case
  Nothing -> yieldMany (b:bs) .| mapC (,[])
  Just x -> yield (b, [x]) >> delay (bs ++ [x])

randomReplace :: Eq a => [a] -> Conduit (a, [a]) IO a
randomReplace vars = awaitForever $ \(x,b) -> do
  y <- case filter (/= x) vars of
    [] -> pure x
    vs -> lift $ (vs !!) <$> getRandomR (0, length vs - 1)
  yield y
  randomReplace $ b ++ delete y vars

shuffleW :: Eq a => Int -> [a] -> IO [a]
shuffleW k lst = yieldMany lst =$= shuffleC k $$ sinkList
```


Here we define a new conduit <code>shuffleC</code> which uses a moving window of length <tt>k</tt> and returns shuffled elements of upstream data.

{{Out}}

```txt
λ> qualityTest (shuffleW 8)
"abba" "baab" 0
"abracadabra" "daabrcabaar" 0
"seesaw" "eswesa" 0
"elk" "kel" 0
"grrrrrr" "rgrrrrr" 5
"up" "pu" 0
"a" "a" 1
"aaaaa.....bbbbb" "....baabaaa.bbb" 3
"Rosetta Code is a programming chrestomathy site." "sCaoeRei d os pttaogrr  nrgshmeaotaichiy .ttmsme" 0
```


This program is good for real texts with high entropy. In case of homogeneous strings like <tt>"aaaaa.....bbbbb"</tt> it gives poor results for windows smaller then homogeneous regions.

The main goal of streaming solution is to be able to process data from any resources, so let's use it to shuffle texts being transferred from <tt>stdin</tt> to <tt>stdout</tt>.

Additional imports


```Haskell
import Data.ByteString.Builder (charUtf8)
import Data.ByteString.Char8 (ByteString, unpack, pack)
import Data.Conduit.ByteString.Builder (builderToByteString)
import System.IO (stdin, stdout)
```



```Haskell

shuffleBS :: Int -> ByteString -> IO ByteString
shuffleBS n s =
  yieldMany (unpack s)
  =$ shuffleC n
  =$ mapC charUtf8
  =$ builderToByteString
  $$ foldC

main :: IO ()
main =
  sourceHandle stdin
  =$ mapMC (shuffleBS 10)
  $$ sinkHandle stdout
```


{{Out}}

```txt
$ ghc --make -O3 ./shuffle
[1 of 1] Compiling Main             ( shuffle.hs, shuffle.o )
Linking shuffle ...

$ cat input.txt
Rosetta Code is a programming chrestomathy site. The idea is to present solutions to the same task in as many different languages as possible, to demonstrate how languages are similar and different, and to aid a person with a grounding in one approach to a problem in learning another. Rosetta Code currently has 823 tasks, 193 draft tasks, and is aware of 642 languages, though we do not (and cannot) have solutions to every task in every language.

$ cat input.txt | ./shuffle
aeotdR s  aoiCtrpmmgi crn theemaysg srioT the tseo.dih psae re isltn ountstoeo  tosmaetia es nssimhn ad kaeeinrlataffauytse g oanbs ,e ol e sio ttngdasmw esphut ro ganeemas g alsi arlaeefn,ranifddoii a drnp det r toi ahowgnutan n rgneanppi raohi d oaop  blrcst imeioaer ngohrla.eRotn  Cst n dce aenletya th8r3 n2ssout1  3dasktaft,rrk9as,a ss iewarf6  d2l ogu  asga te g un oa hn4d enaodho(ctt)n, eha laovnsotusw oeinyetsakvn eo ienlrav  ygtnu aer. g
```


=={{header|Icon}} and {{header|Unicon}}==
The approach taken requires 2n memory and will run in O(n^2) time swapping once per final changed character.  The algorithm is concise and conceptually simple avoiding the lists of indices, sorting, cycles, groups, and special cases requiring rotation needed by many of the other solutions.  It proceeds through the entire string swapping characters ensuring that neither of the two characters are swapped with another instance of themselves in the ''original'' string.

Additionally, this can be trivially modified to randomize the shuffle by uncommenting the line

```icon
# every !t :=: ?t    # Uncomment to get a random best shuffling
```
 in <tt>bestShuffle</tt>.

```icon
procedure main(args)
    while scram := bestShuffle(line := read()) do
        write(line," -> ",scram," (",unchanged(line,scram),")")
end

procedure bestShuffle(s)
    t := s
    # every !t :=: ?t    # Uncomment to get a random best shuffling
    every i := 1 to *t do
        every j := (1 to i-1) | (i+1 to *t) do
           if (t[i] ~== s[j]) & (s[i] ~== t[j]) then break t[i] :=: t[j]
    return t
end

procedure unchanged(s1,s2)      # Number of unchanged elements
    every (count := 0) +:= (s1[i := 1 to *s1] == s2[i], 1)
    return count
end
```


The code works in both Icon and Unicon.

Sample output:

```txt

->scramble <scramble.data
abracadabra -> raaracababd (0)
seesaw -> wasese (0)
elk -> lke (0)
grrrrrr -> rgrrrrr (5)
up -> pu (0)
a -> a (1)
aardvarks are ant eaters -> sdaaaraaasv rer nt keter (0)
->

```



## J


Based on [http://rosettacode.org/mw/index.php?title=Best_shuffle&oldid=97419#J Dan Bron's approach]:


```j
bestShuf =: verb define
  yy=. <@({~ ?~@#)@I.@= y
  y C.~ (;yy) </.~ (i.#y) |~ >./#@> yy
)

fmtBest=:3 :0
  b=. bestShuf y
  y,', ',b,' (',')',~":+/b=y
)

```


yy is (a list of) boxes of (lists of) indices where all characters selected by indices in a box are the same, and where the first box is the biggest box (contains the most indices).  The phrase <code>({~ ?~@#)</code> shuffles the indices going into each box which makes the (deterministic) rotate which follows produce differing results sometimes (but only when that is possible).

Example:


```j
   fmtBest&>;:'abracadabra seesaw elk grrrrrr up a'
abracadabra, bdacararaab (0)
seesaw, eawess (0)
elk, lke (0)
grrrrrr, rrrrrrg (5)
up, pu (0)
a, a (1)
```



## Java

Translation of [[Best_shuffle#Icon_and_Unicon|Icon]] via [[Best_shuffle#AWK|AWK]]

```java
import java.util.Random;

public class BestShuffle {
    private final static Random rand = new Random();

    public static void main(String[] args) {
        String[] words = {"abracadabra", "seesaw", "grrrrrr", "pop", "up", "a"};
        for (String w : words)
            System.out.println(bestShuffle(w));
    }

    public static String bestShuffle(final String s1) {
        char[] s2 = s1.toCharArray();
        shuffle(s2);
        for (int i = 0; i < s2.length; i++) {
            if (s2[i] != s1.charAt(i))
                continue;
            for (int j = 0; j < s2.length; j++) {
                if (s2[i] != s2[j] && s2[i] != s1.charAt(j) && s2[j] != s1.charAt(i)) {
                    char tmp = s2[i];
                    s2[i] = s2[j];
                    s2[j] = tmp;
                    break;
                }
            }
        }
        return s1 + " " + new String(s2) + " (" + count(s1, s2) + ")";
    }

    public static void shuffle(char[] text) {
        for (int i = text.length - 1; i > 0; i--) {
            int r = rand.nextInt(i + 1);
            char tmp = text[i];
            text[i] = text[r];
            text[r] = tmp;
        }
    }

    private static int count(final String s1, final char[] s2) {
        int count = 0;
        for (int i = 0; i < s2.length; i++)
            if (s1.charAt(i) == s2[i])
                count++;
        return count;
    }
}
```


Output:

```txt
abracadabra raaracabdab (0)
seesaw eswaes (0)
grrrrrr rgrrrrr (5)
pop ppo (1)
up pu (0)
a a (1)
```



## JavaScript


Based on the J implementation (and this would be a lot more concise if we used something like jQuery):


```javascript
function raze(a) { // like .join('') except producing an array instead of a string
    var r= [];
    for (var j= 0; j<a.length; j++)
        for (var k= 0; k<a[j].length; k++)  r.push(a[j][k]);
    return r;
}
function shuffle(y) {
    var len= y.length;
    for (var j= 0; j < len; j++) {
        var i= Math.floor(Math.random()*len);
        var t= y[i];
        y[i]= y[j];
        y[j]= t;
    }
    return y;
}
function bestShuf(txt) {
    var chs= txt.split('');
    var gr= {};
    var mx= 0;
    for (var j= 0; j<chs.length; j++) {
        var ch= chs[j];
        if (null == gr[ch])  gr[ch]= [];
        gr[ch].push(j);
        if (mx < gr[ch].length)  mx++;
    }
    var inds= [];
    for (var ch in gr)  inds.push(shuffle(gr[ch]));
    var ndx= raze(inds);
    var cycles= [];
    for (var k= 0; k < mx; k++)  cycles[k]= [];
    for (var j= 0; j<chs.length; j++)  cycles[j%mx].push(ndx[j]);
    var ref= raze(cycles);
    for (var k= 0; k < mx; k++)  cycles[k].push(cycles[k].shift());
    var prm= raze(cycles);
    var shf= [];
    for (var j= 0; j<chs.length; j++)  shf[ref[j]]= chs[prm[j]];
    return shf.join('');
}

function disp(ex) {
    var r= bestShuf(ex);
    var n= 0;
    for (var j= 0; j<ex.length; j++)
        n+= ex.substr(j, 1) == r.substr(j,1) ?1 :0;
    return ex+', '+r+', ('+n+')';
}
```


Example:


```html><html><head><title></title></head><body
<pre id="out">
```
</body></html>
<script type="text/javascript">
/* ABOVE CODE GOES HERE */
var sample= ['abracadabra', 'seesaw', 'elk', 'grrrrrr', 'up', 'a']
for (var i= 0; i<sample.length; i++)
	document.getElementById('out').innerHTML+= disp(sample[i])+'\r\n';
</script>
```


Produced:

```txt
abracadabra, raababacdar, (0)
seesaw, ewaess, (0)
elk, lke, (0)
grrrrrr, rrrrrgr, (5)
up, pu, (0)
a, a, (1)
```



## jq

{{works with|jq|1.5}}
The implementation in this section uses the deterministic "swap" algorithm found in other entries on this page.


```jq
def count(s): reduce s as $i (0;.+1);

def swap($i;$j):
  .[$i] as $x | .[$i] = .[$j] | .[$j] = $x;

# Input: an array
# Output: a best shuffle
def bestShuffleArray:
  . as $s
  | reduce range(0; length) as $i (.;
      . as $t
      | (first(range(0; length)
               | select( $i != . and
                         $t[$i] != $s[.] and
                         $s[$i] != $t[.] and
                         $t[$i] != $t[.])) as $j
         | swap($i;$j))
	 // $t  # fallback
    );

# Award 1 for every spot which changed:
def score($base):
  . as $in
  | count( range(0;length)
           | select($base[.] != $in[.]) );

# Input: a string
# Output: INPUT, BESTSHUFFLE, (NUMBER)
def bestShuffle:
  . as $in
  | explode
  | . as $s
  | bestShuffleArray
  | "\($in), \(implode), (\( length - score($s) ))" ;
```


'''Examples:'''

```jq
"abracadabra", "seesaw", "elk", "grrrrrr", "up", "a", "antidisestablishmentarianism"
| bestShuffle
```


'''Invocation and Output'''

```txt
jq -nr -f best-shuffle.jq
abracadabra, baaracadabr, (0)
seesaw, esswea, (0)
elk, lke, (0)
grrrrrr, rgrrrrr, (5)
up, pu, (0)
a, a, (1)
antidisestablishmentarianism, maaaadisesitblishmenttrninis, (0)
```



## Julia

{{trans|Python}}

```julia
# v0.6

function bestshuffle(str::String)::Tuple{String,Int}
    s = Vector{Char}(str)

    # Count the supply of characters.
    cnt = Dict{Char,Int}(c => 0 for c in s)
    for c in s; cnt[c] += 1 end

    # Allocate the result
    r = similar(s)
    for (i, x) in enumerate(s)
        # Find the best character to replace x.
        best = x
        rankb = -2
        for (c, rankc) in cnt
            # Prefer characters with more supply.
            # (Save characters with less supply.)
            # Avoid identical characters.
            if c == x; rankc = -1 end
            if rankc > rankb
                best = c
                rankb = rankc
            end
        end

        # Add character to list. Remove it from supply.
        r[i] = best
        cnt[best] -= 1
        if cnt[best] == 0; delete!(cnt, best) end
    end

    # If the final letter became stuck (as "ababcd" became "bacabd",
    # and the final "d" became stuck), then fix it.
    i = length(s)
    if r[i] == s[i]
        for j in 1:i
            if r[i] != s[j] && r[j] != s[i]
                r[i], r[j] = r[j], r[i]
                break
            end
        end
    end

    score = sum(x == y for (x, y) in zip(r, s))
    return r, score
end

for word in ("abracadabra", "seesaw", "elk", "grrrrrr", "up", "a")
    shuffled, score = bestshuffle(word)
    println("$word: $shuffled ($score)")
end
```


{{out}}

```txt
abracadabra: baarabadacr (0)
seesaw: esawse (0)
elk: kel (0)
grrrrrr: rgrrrrr (5)
up: pu (0)
a: a (1)
```



## Kotlin

{{trans|Java}}

```scala
import java.util.Random

object BestShuffle {
    operator fun invoke(s1: String) : String {
        val s2 = s1.toCharArray()
        s2.shuffle()
        for (i in s2.indices)
            if (s2[i] == s1[i])
                for (j in s2.indices)
                    if (s2[i] != s2[j] && s2[i] != s1[j] && s2[j] != s1[i]) {
                        val tmp = s2[i]
                        s2[i] = s2[j]
                        s2[j] = tmp
                        break
                    }
        return s1 + ' ' + String(s2) + " (" + s2.count(s1) + ')'
    }

    private fun CharArray.shuffle() {
        val rand = Random()
        for (i in size - 1 downTo 1) {
            val r = rand.nextInt(i + 1)
            val tmp = this[i]
            this[i] = this[r]
            this[r] = tmp
        }
    }

    private fun CharArray.count(s1: String) : Int {
        var count = 0
        for (i in indices)
            if (s1[i] == this[i]) count++
        return count
    }
}

fun main(words: Array<String>) = words.forEach { println(BestShuffle(it)) }
```


{{out}}

```txt
abracadabra raaracabdab (0)
seesaw eswaes (0)
grrrrrr rgrrrrr (5)
pop ppo (1)
up pu (0)
a a (1)
```



## Liberty BASIC


```lb
'see Run BASIC solution
list$ = "abracadabra seesaw pop grrrrrr up a"

while word$(list$,ii + 1," ") <> ""
 ii    = ii + 1
 w$    = word$(list$,ii," ")
 bs$   = bestShuffle$(w$)
 count = 0
 for i = 1 to len(w$)
  if mid$(w$,i,1) = mid$(bs$,i,1) then count = count + 1
 next i
 print  w$;" ";bs$;" ";count
wend

function bestShuffle$(s1$)
   s2$   = s1$
   for i = 1 to len(s2$)
        for j =  1 to len(s2$)
            if (i <> j) and (mid$(s2$,i,1) <> mid$(s1$,j,1)) and (mid$(s2$,j,1) <> mid$(s1$,i,1)) then
            if j < i then i1 = j:j1 = i else i1 = i:j1 = j
            s2$ = left$(s2$,i1-1) + mid$(s2$,j1,1) + mid$(s2$,i1+1,(j1-i1)-1) + mid$(s2$,i1,1) + mid$(s2$,j1+1)
            end if
        next j
   next i
bestShuffle$ = s2$
end function
```

output

```txt

abracadabra caadrbabaar 0
seesaw ewaess 0
pop opp 1
grrrrrr rgrrrrr 5
up pu 0
a a 1
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
BestShuffle[data_] :=
 Flatten[{data,First[SortBy[
     List[#, StringLength[data]-HammingDistance[#,data]] & /@ StringJoin /@ Permutations[StringSplit[data, ""]], Last]]}]

Print[#[[1]], "," #[[2]], ",(", #[[3]], ")"] & /@  BestShuffle /@ {"abracadabra","seesaw","elk","grrrrrr","up","a"}

```


Output :

```txt
abracadabra, baabacadrar,(0)
seesaw, assewe,(0)
elk, kel,(0)
grrrrrr, rgrrrrr,(5)
up, pu,(0)
a, a,(1)
```



## Nim

{{trans|Java}}

```Nim
import times
import sequtils
import strutils
import random

proc count(s1, s2: string): int =
    for i, c in s1:
        if c == s2[i]:
            result.inc

proc shuffle(str: string): string =
    var r = initRand(getTime().toUnix())
    var chrs = toSeq(str.items)
    for i in 0 ..< chrs.len:
        let chosen = r.rand(chrs.len-1)
        swap(chrs[i], chrs[chosen])
    return chrs.join("")

proc bestShuffle(str: string): string =
    var chrs = toSeq(shuffle(str).items)
    for i in chrs.low .. chrs.high:
        if chrs[i] != str[i]:
            continue
        for j in chrs.low .. chrs.high:
            if chrs[i] != chrs[j] and chrs[i] != str[j] and chrs[j] != str[i]:
                swap(chrs[i], chrs[j])
                break
    return chrs.join("")

when isMainModule:
    let words = @["abracadabra", "seesaw", "grrrrrr", "pop", "up", "a", "antidisestablishmentarianism"];
    for w in words:
        let shuffled = bestShuffle(w)
        echo "$1 $2 $3" % [w, shuffled, $count(w, shuffled)]

```


Run:


```txt
abracadabra baabadaracr 0
seesaw wsseea 0
grrrrrr rrrrrgr 5
pop ppo 1
up pu 0
a a 1
antidisestablishmentarianism mietnshieistrlaatbsdsnaiinma 0
```



## OCaml


Deterministic


```ocaml
let best_shuffle s =
  let len = String.length s in
  let r = String.copy s in
  for i = 0 to pred len do
    for j = 0 to pred len do
      if i <> j && s.[i] <> r.[j] && s.[j] <> r.[i] then
        begin
          let tmp = r.[i] in
          r.[i] <- r.[j];
          r.[j] <- tmp;
        end
    done;
  done;
  (r)

let count_same s1 s2 =
  let len1 = String.length s1
  and len2 = String.length s2 in
  let n = ref 0 in
  for i = 0 to pred (min len1 len2) do
    if s1.[i] = s2.[i] then incr n
  done;
  !n

let () =
  let test s =
    let s2 = best_shuffle s in
    Printf.printf " '%s', '%s' -> %d\n" s s2 (count_same s s2);
  in
  test "tree";
  test "abracadabra";
  test "seesaw";
  test "elk";
  test "grrrrrr";
  test "up";
  test "a";
;;
```


Run:


```txt
$ ocaml best_shuffle_string.ml
 'tree', 'eert' -> 0
 'abracadabra', 'caadrbabaar' -> 0
 'seesaw', 'ewaess' -> 0
 'elk', 'kel' -> 0
 'grrrrrr', 'rgrrrrr' -> 5
 'up', 'pu' -> 0
 'a', 'a' -> 1
```



## Pascal

{{works with|Free_Pascal}}

```pascal
program BestShuffleDemo(output);

function BestShuffle(s: string): string;

  var
    tmp: char;
    i, j: integer;
    t: string;
  begin
    t := s;
    for i := 1 to length(t) do
      for j := 1 to length(t) do
        if (i <> j) and (s[i] <> t[j]) and (s[j] <> t[i]) then
        begin
          tmp  := t[i];
          t[i] := t[j];
          t[j] := tmp;
        end;
    BestShuffle := t;
  end;

const
  original: array[1..6] of string =
    ('abracadabra', 'seesaw', 'elk', 'grrrrrr', 'up', 'a');

var
  shuffle: string;
  i, j, score: integer;

begin
 for i := low(original) to high(original) do
 begin
   shuffle := BestShuffle(original[i]);
   score := 0;
   for j := 1 to length(shuffle) do
     if original[i][j] = shuffle[j] then
       inc(score);
    writeln(original[i], ', ', shuffle, ', (', score, ')');
  end;
end.
```

Output:

```txt
% ./BestShuffle
abracadabra, caadrbabaar, (0)
seesaw, ewaess, (0)
elk, kel, (0)
grrrrrr, rgrrrrr, (5)
up, pu, (0)
a, a, (1)
```



## Perl

The Algorithm::Permute module does not ship with perl, but is freely available from CPAN.


```perl
use strict;
use warnings;
use List::Util qw(shuffle);
use Algorithm::Permute;

best_shuffle($_) for qw(abracadabra seesaw elk grrrrrr up a);

sub best_shuffle {
	my ($original_word) = @_;
	my $best_word = $original_word;
	my $best_score = length $best_word;

	my @shuffled = shuffle split //, $original_word;
	my $iterator = Algorithm::Permute->new(\@shuffled);

	while( my @array = $iterator->next ) {
		my $word = join '', @array;
		# For each letter which is the same in the two words,
		# there will be a \x00 in the "^" of the two words.
		# The tr operator is then used to count the "\x00"s.
		my $score = ($original_word ^ $word) =~ tr/\x00//;
		next if $score >= $best_score;
		($best_word, $best_score) = ($word, $score);
		last if $score == 0;
	}

	print "$original_word, $best_word, $best_score\n";
}


```

{{out|Output of two runs}}

```txt
abracadabra, dabrabacaar, 0
seesaw, easews, 0
elk, kel, 0
grrrrrr, rrrrgrr, 5
up, pu, 0
a, a, 1
```


```txt
abracadabra, caabararadb, 0
seesaw, esawes, 0
elk, lke, 0
grrrrrr, rrgrrrr, 5
up, pu, 0
a, a, 1
```


After creating a shuffled array of letters, we iterate through
all permutations of that array.  We keep the first word we encounter
with a score better than all previous words.  As an optimization,
if we discover a word with score zero, we stop iterating early.

If the best score is nonzero, then we will iterate through every
possible permutation.  So "aaaaaaaaaaah" will take a long time.

A faster solution is to shuffle once, and then make any additional
swaps which will improve the score.

{{trans|go}}

```perl
use strict;
use warnings;
use List::Util qw(shuffle);

best_shuffle($_) for qw(abracadabra seesaw elk grrrrrr up a);

sub best_shuffle {
	my ($original_word) = @_;

	my @s = split //, $original_word;
	my @t = shuffle @s;

	for my $i ( 0 .. $#s ) {
		for my $j ( 0 .. $#s ) {
			next if $j == $i or
				$t[$i] eq $s[$j] or
				$t[$j] eq $s[$i];
			@t[$i,$j] = @t[$j,$i];
			last;
		}
	}

	my $word = join '', @t;

	my $score = ($original_word ^ $word) =~ tr/\x00//;
	print "$original_word, $word, $score\n";
}

```


The output has the same format as the first perl implementation,
but only takes quadratic time per word.


## Perl 6

{{trans|Sidef}}
{{works with|Rakudo Star|2015.12}}


```perl6
sub best-shuffle(Str $orig) {

    my @s = $orig.comb;
    my @t = @s.pick(*);

    for ^@s -> $i {
        for ^@s -> $j {
            if $i != $j and @t[$i] ne @s[$j] and @t[$j] ne @s[$i] {
                @t[$i, $j] = @t[$j, $i];
                last;
            }
        }
    }

    my $count = 0;
    for @t.kv -> $k,$v {
        ++$count if $v eq @s[$k]
    }

    return (@t.join, $count);
}

printf "%s, %s, (%d)\n", $_, best-shuffle $_
    for <abracadabra seesaw elk grrrrrr up a>;
```

{{out}}

```txt

abracadabra, raacarabadb, (0)
seesaw, wssaee, (0)
elk, lke, (0)
grrrrrr, rrrgrrr, (5)
up, pu, (0)
a, a, (1)

```



## Phix


```Phix
constant tests = {"abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"}
string s,t
    for test=1 to length(tests) do
        s = tests[test]
        t = shuffle(s)
        for i=1 to length(t) do
            for j=1 to length(t) do
                if i!=j and t[i]!=s[j] and t[j]!=s[i] then
                    {t[i], t[j]} = {t[j], t[i]}
                    exit
                end if
            end for
        end for
        printf(1,"%s -> %s (%d)\n",{s,t,sum(sq_eq(t,s))})
    end for
```

{{out}}

```txt

abracadabra -> baacabrdaar (0)
seesaw -> aswees (0)
elk -> lke (0)
grrrrrr -> rrrgrrr (5)
up -> pu (0)
a -> a (1)

```

By replacing <code>t=shuffle(s)</code> with <code>t=s</code>, the following deterministic result is output every time:

```txt

abracadabra -> raaracababd (0)
seesaw -> wasese (0)
elk -> lke (0)
grrrrrr -> rgrrrrr (5)
up -> pu (0)
a -> a (1)

```



## PHP

Translation of [[Best_shuffle#Icon_and_Unicon|Icon]] via [[Best_shuffle#AWK|AWK]]

```php
foreach (split(' ', 'abracadabra seesaw pop grrrrrr up a') as $w)
    echo bestShuffle($w) . '
';

function bestShuffle($s1) {
    $s2 = str_shuffle($s1);
    for ($i = 0; $i < strlen($s2); $i++) {
        if ($s2[$i] != $s1[$i]) continue;
        for ($j = 0; $j < strlen($s2); $j++)
            if ($i != $j && $s2[$i] != $s1[$j] && $s2[$j] != $s1[$i]) {
                $t = $s2[$i];
                $s2[$i] = $s2[$j];
                $s2[$j] = $t;
                break;
            }
    }
    return "$s1 $s2 " . countSame($s1, $s2);
}

function countSame($s1, $s2) {
    $cnt = 0;
    for ($i = 0; $i < strlen($s2); $i++)
        if ($s1[$i] == $s2[$i])
            $cnt++;
    return "($cnt)";
}
```


Output:

```txt
abracadabra drabacabaar (0)
seesaw esswea (0)
pop ppo (1)
grrrrrr rrgrrrr (5)
up pu (0)
a a (1)
```



## PicoLisp


```PicoLisp
(de bestShuffle (Str)
   (let Lst NIL
      (for C (setq Str (chop Str))
         (if (assoc C Lst)
            (con @ (cons C (cdr @)))
            (push 'Lst (cons C)) ) )
      (setq Lst (apply conc (flip (by length sort Lst))))
      (let Res
         (mapcar
            '((C)
               (prog1 (or (find <> Lst (circ C)) C)
                  (setq Lst (delete @ Lst)) ) )
            Str )
         (prinl Str " " Res " (" (cnt = Str Res) ")") ) ) )
```

Output:

```txt
: (bestShuffle "abracadabra")
abracadabra raarababadc (0)

: (bestShuffle "seesaw")
seesaw essewa (0)

: (bestShuffle "elk")
elk lke (0)

: (bestShuffle "grrrrrr")
grrrrrr rgrrrrr (5)

: (bestShuffle "up")
up pu (0)

: (bestShuffle "a")
a a (1)
```



## PL/I


```pli
shuffle: procedure options (main);                  /* 14/1/2011 */
   declare (s, saves) character (20) varying, c character (1);
   declare t(length(s)) bit (1);
   declare (i, k, moves initial (0)) fixed binary;

   get edit (s) (L);
   put skip list (s);
   saves = s;
   t = '0'b;
   do i = 1 to length (s);
      if t(i) then iterate; /* This character has already been moved. */
      c = substr(s, i, 1);
      k = search (s, c, i+1);
      if k > 0 then
         do;
            substr(s, i, 1) = substr(s, k, 1);
            substr(s, k, 1) = c;
            t(k), t(i) = '1'b;
         end;
   end;

   do k = length(s) to 2 by -1;
      if ^t(k) then /* this character wasn't moved. */
all:     do;
            c = substr(s, k, 1);
            do i = k-1 to 1 by -1;
               if c ^= substr(s, i, 1) then
                  if substr(saves, i, 1) ^= c then
                     do;
                        substr(s, k, 1) = substr(s, i, 1);
                        substr(s, i, 1) = c;
                        t(k) = '1'b;
                        leave all;
                     end;
            end;
         end;
   end;
   moves = length(s) - sum(t);
   put skip edit (s, trim(moves))(a, x(1));

search: procedure (s, c, k) returns (fixed binary);
   declare s character (*) varying;
   declare c character (1);
   declare k fixed binary;
   declare i fixed binary;

   do i = k to length(s);
      if ^t(i) then if c ^= substr(s, i, 1) then return (i);
   end;
   return (0); /* No eligible character. */
end search;

end shuffle;
```


OUTPUT:

```txt

abracadabra
baaracadrab 0

prrrrrr
rprrrrr 5

tree
eert 0

A
A 1

```



## PowerShell

{{works with|PowerShell|3}}

```PowerShell
#  Calculate best possible shuffle score for a given string
#  (Split out into separate function so we can use it separately in our output)
function Get-BestScore ( [string]$String )
    {
    #  Convert to array of characters, group identical characters,
    #  sort by frequecy, get size of first group
    $MostRepeats = $String.ToCharArray() |
                    Group |
                    Sort Count -Descending |
                    Select -First 1 -ExpandProperty Count

    #  Return count of most repeated character minus all other characters (math simplified)
    return [math]::Max( 0, 2 * $MostRepeats - $String.Length )
    }

function Get-BestShuffle ( [string]$String )
    {
    #  Convert to arrays of characters, one for comparison, one for manipulation
    $S1 = $String.ToCharArray()
    $S2 = $String.ToCharArray()

    #  Calculate best possible score as our goal
    $BestScore = Get-BestScore $String

    #  Unshuffled string has score equal to number of characters
    $Length = $String.Length
    $Score = $Length

    #  While still striving for perfection...
    While ( $Score -gt $BestScore )
        {
        #  For each character
        ForEach ( $i in 0..($Length-1) )
            {
            #  If the shuffled character still matches the original character...
            If ( $S1[$i] -eq $S2[$i] )
                {
                #  Swap it with a random character
                #  (Random character $j may be the same as or may even be
                #   character $i. The minor impact on speed was traded for
                #   a simple solution to guarantee randomness.)
                $j = Get-Random -Maximum $Length
                $S2[$i], $S2[$j] = $S2[$j], $S2[$i]
                }
            }
        #  Count the number of indexes where the two arrays match
        $Score = ( 0..($Length-1) ).Where({ $S1[$_] -eq $S2[$_] }).Count
        }
    #  Put it back into a string
    $Shuffle = ( [string[]]$S2 -join '' )
    return $Shuffle
    }
```


```PowerShell
ForEach ( $String in ( 'abracadabra', 'seesaw', 'elk', 'grrrrrr', 'up', 'a' ) )
    {
    $Shuffle = Get-BestShuffle $String
    $Score   = Get-BestScore   $String
    "$String, $Shuffle, ($Score)"
    }
```

{{out}}

```txt
abracadabra, craradabaab, (0)
seesaw, ewsase, (0)
elk, kel, (0)
grrrrrr, rrrrrrg, (5)
up, pu, (0)
a, a, (1)
```



## Prolog

Works with SWI-Prolog

```Prolog
:- dynamic score/2.

best_shuffle :-
	maplist(best_shuffle, ["abracadabra", "eesaw", "elk", "grrrrrr",
			       	"up", "a"]).

best_shuffle(Str) :-
	retractall(score(_,_)),
	length(Str, Len),
	assert(score(Str, Len)),
	calcule_min(Str, Len, Min),
	repeat,
	   shuffle(Str, Shuffled),
	   maplist(comp, Str, Shuffled, Result),
	   sumlist(Result, V),
	   retract(score(Cur, VCur)),
	   (  V < VCur -> assert(score(Shuffled, V)); assert(score(Cur, VCur))),
	   V = Min,
	retract(score(Cur, VCur)),
	writef('%s : %s (%d)\n', [Str, Cur, VCur]).

comp(C, C1, S):-
	(   C = C1 -> S = 1; S = 0).

% this code was written by P.Caboche and can be found here :
% http://pcaboche.developpez.com/article/prolog/listes/?page=page_3#Lshuffle
shuffle(List, Shuffled) :-
  length(List, Len),
  shuffle(Len, List, Shuffled).

shuffle(0, [], []) :- !.

shuffle(Len, List, [Elem|Tail]) :-
  RandInd is random(Len),
  nth0(RandInd, List, Elem),
  select(Elem, List, Rest),
  NewLen is Len - 1,
  shuffle(NewLen, Rest, Tail).


% letters are sorted out then packed
% If a letter is more numerous than the rest
% the min is the difference between the quantity of this letter and
% the sum of the quantity of the other letters
calcule_min(Str, Len, Min) :-
	msort(Str, SS),
	packList(SS, Lst),
	sort(Lst, Lst1),
	last(Lst1, [N, _]),
	(   N * 2 > Len -> Min is 2 * N - Len; Min = 0).



% almost the same code as in "run_length" page
packList([],[]).

packList([X],[[1,X]]) :- !.


packList([X|Rest],[XRun|Packed]):-
    run(X,Rest, XRun,RRest),
    packList(RRest,Packed).


run(Var,[],[1,Var],[]).

run(Var,[Var|LRest],[N1, Var],RRest):-
    run(Var,LRest,[N, Var],RRest),
    N > 0,
    N1 is N + 1.


run(Var,[Other|RRest], [1,Var],[Other|RRest]):-
     dif(Var,Other).

```


output :
```txt
 ?- test.
abracadabra : brabaracaad (0)
eesaw : sweea (0)
elk : kel (0)
grrrrrr : rrrgrrr (5)
up : pu (0)
a : a (1)
true .

```


## PureBasic

This solution creates cycles of letters of letters that are then rotated to produce the final maximal shuffle.  It includes an extra sort step that ensures the original string to be returned if it is repeatedly shuffled.

```PureBasic
Structure charInfo
  Char.s
  List Position.i()
  count.i          ;number of occurrences of Char
EndStructure

Structure cycleInfo
  Char.s
  Position.i
EndStructure

Structure cycle
  List cycle.cycleInfo()
EndStructure

Procedure.s shuffleWordLetters(word.s)
  Protected i
  Dim originalLetters.s(len(word) - 1)
  For i = 1 To Len(word)
    originalLetters(i - 1) = Mid(word, i, 1)
  Next

  Dim shuffledLetters.s(0)
  CopyArray(originalLetters(), shuffledLetters())

  ;record original letters and their positions
  Protected curChar.s
  NewList letters.charInfo()
  NewMap *wordInfo.charInfo()
  For i = 0 To ArraySize(originalLetters())
    curChar = originalLetters(i)
    If FindMapElement(*wordInfo(), curChar)
      AddElement(*wordInfo()\position())
      *wordInfo()\position() = i
    Else
      *wordInfo(curChar) = AddElement(letters())
      If *wordInfo()
        *wordInfo()\Char = curChar
        AddElement(*wordInfo()\position())
        *wordInfo()\position() = i
      EndIf
    EndIf
  Next

  ForEach letters()
    letters()\count = ListSize(letters()\Position())
  Next

  SortStructuredList(letters(), #PB_Sort_Ascending, OffsetOf(charInfo\Char), #PB_Sort_String) ;extra sort step, not strictly necessary
  SortStructuredList(letters(), #PB_Sort_Descending, OffsetOf(charInfo\count), #PB_Sort_integer)

  ;construct letter cycles
  FirstElement(letters())
  Protected maxLetterCount = letters()\count
  Dim letterCycles.cycle(maxLetterCount - 1)

  Protected curCycleIndex
  ForEach letters()
    ForEach letters()\Position()
      With letterCycles(curCycleIndex)
        AddElement(\cycle())
        \cycle()\Char = letters()\Char
        \cycle()\Position = letters()\position()
      EndWith
      curCycleIndex = (curCycleIndex + 1) % maxLetterCount
    Next
  Next

  ;rotate letters in each cycle
  Protected isFirst, prevChar.s, pos_1
  For i = 0 To maxLetterCount - 1
    With letterCycles(i)
      isFirst = #True
      ForEach \cycle()
        If Not isFirst
          shuffledLetters(\cycle()\Position) = prevChar
        Else
          pos_1 = \cycle()\Position
          isFirst = #False
        EndIf
        prevChar = \cycle()\Char
      Next
      shuffledLetters(pos_1) = prevChar
    EndWith
  Next

  ;score and display shuffle
  Protected shuffledWord.s, ignored
  For i = 0 To ArraySize(shuffledLetters())
    shuffledWord + shuffledLetters(i)
    If shuffledLetters(i) = originalLetters(i)
      ignored + 1
    EndIf
  Next

  PrintN(word + ", " + shuffledWord + ", (" + Str(ignored) + ")")
  ProcedureReturn shuffledWord
EndProcedure

If OpenConsole()
  shuffleWordLetters("abracadabra")
  shuffleWordLetters("seesaw")
  shuffleWordLetters("elk")
  shuffleWordLetters("grrrrrr")
  shuffleWordLetters("up")
  shuffleWordLetters("a")

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```

Sample output:

```txt
abracadabra, daabarbraac, (0)
seesaw, eawess, (0)
elk, lke, (0)
grrrrrr, rgrrrrr, (5)
up, pu, (0)
a, a, (1)
```



## Python


### Swap if it is locally better algorithm

With added randomization of swaps!

```python
import random

def count(w1,wnew):
    return sum(c1==c2 for c1,c2 in zip(w1, wnew))

def best_shuffle(w):
    wnew = list(w)
    n = len(w)
    rangelists = (list(range(n)), list(range(n)))
    for r in rangelists:
        random.shuffle(r)
    rangei, rangej = rangelists
    for i in rangei:
        for j in rangej:
            if i != j and wnew[j] != wnew[i] and w[i] != wnew[j] and w[j] != wnew[i]:
                wnew[j], wnew[i] = wnew[i], wnew[j]
                break
    wnew = ''.join(wnew)
    return wnew, count(w, wnew)


if __name__ == '__main__':
    test_words = ('tree abracadabra seesaw elk grrrrrr up a '
                  + 'antidisestablishmentarianism hounddogs').split()
    test_words += ['aardvarks are ant eaters', 'immediately', 'abba']
    for w in test_words:
        wnew, c = best_shuffle(w)
        print("%29s, %-29s ,(%i)" % (w, wnew, c))
```


;Sample output
Two runs showing variability in shuffled results

```txt
>>>
### ============================= RESTART =============================

>>>
                         tree, eetr                          ,(0)
                  abracadabra, daaracbraab                   ,(0)
                       seesaw, asswee                        ,(0)
                          elk, kel                           ,(0)
                      grrrrrr, rrgrrrr                       ,(5)
                           up, pu                            ,(0)
                            a, a                             ,(1)
 antidisestablishmentarianism, sintmdnirhimasibtnasetaisael  ,(0)
                    hounddogs, ohodgnsud                     ,(0)
     aardvarks are ant eaters, sesanretatva kra errada       ,(0)
                  immediately, tedlyaeiimm                   ,(0)
                         abba, baab                          ,(0)
>>>
### ============================= RESTART =============================

>>>
                         tree, eert                          ,(0)
                  abracadabra, bdacararaab                   ,(0)
                       seesaw, ewsase                        ,(0)
                          elk, kel                           ,(0)
                      grrrrrr, rrrrrrg                       ,(5)
                           up, pu                            ,(0)
                            a, a                             ,(1)
 antidisestablishmentarianism, rtitiainnnshtmdesibalassemai  ,(0)
                    hounddogs, ddousngoh                     ,(0)
     aardvarks are ant eaters, sretrnat a edseavra akar      ,(0)
                  immediately, litiaemmyed                   ,(0)
                         abba, baab                          ,(0)
>>>
```



### Alternative algorithm #1



```python
#!/usr/bin/env python

def best_shuffle(s):
    # Count the supply of characters.
    from collections import defaultdict
    count = defaultdict(int)
    for c in s:
        count[c] += 1

    # Shuffle the characters.
    r = []
    for x in s:
        # Find the best character to replace x.
        best = None
        rankb = -2
        for c, rankc in count.items():
            # Prefer characters with more supply.
            # (Save characters with less supply.)
            # Avoid identical characters.
            if c == x: rankc = -1
            if rankc > rankb:
                best = c
                rankb = rankc

        # Add character to list. Remove it from supply.
        r.append(best)
        count[best] -= 1
        if count[best] >= 0: del count[best]

    # If the final letter became stuck (as "ababcd" became "bacabd",
    # and the final "d" became stuck), then fix it.
    i = len(s) - 1
    if r[i] == s[i]:
        for j in range(i):
            if r[i] != s[j] and r[j] != s[i]:
                r[i], r[j] = r[j], r[i]
                break

    # Convert list to string. PEP 8, "Style Guide for Python Code",
    # suggests that ''.join() is faster than + when concatenating
    # many strings. See http://www.python.org/dev/peps/pep-0008/
    r = ''.join(r)

    score = sum(x == y for x, y in zip(r, s))

    return (r, score)

for s in "abracadabra", "seesaw", "elk", "grrrrrr", "up", "a":
    shuffled, score = best_shuffle(s)
    print("%s, %s, (%d)" % (s, shuffled, score))
```


Output:
```txt
abracadabra, raabarabacd, (0)
seesaw, wsaese, (0)
elk, kel, (0)
grrrrrr, rgrrrrr, (5)
up, pu, (0)
a, a, (1)
```



## Racket


```Racket

#lang racket

(define (best-shuffle s)
  (define len (string-length s))
  (define @ string-ref)
  (define r (list->string (shuffle (string->list s))))
  (for* ([i (in-range len)] [j (in-range len)])
    (when (not (or (= i j) (eq? (@ s i) (@ r j)) (eq? (@ s j) (@ r i))))
      (define t (@ r i))
      (string-set! r i (@ r j))
      (string-set! r j t)))
  r)

(define (count-same s1 s2)
  (for/sum ([c1 (in-string s1)] [c2 (in-string s2)])
    (if (eq? c1 c2) 1 0)))

(for ([s (in-list '("abracadabra" "seesaw" "elk" "grrrrrr" "up" "a"))])
  (define sh (best-shuffle s))
  (printf " ~a, ~a, (~a)\n" s sh (count-same s sh)))

```

{{out}}

```txt

 abracadabra, baabadcraar, (0)
 seesaw, wsaees, (0)
 elk, kel, (0)
 grrrrrr, rgrrrrr, (5)
 up, pu, (0)
 a, a, (1)

```



## Rascal

{{incomplete|Rascal|No output given.}}

```Rascal
import Prelude;

public tuple[str, str, int] bestShuffle(str s){
     characters = chars(s);

     ranking = {<p, countSame(p, characters)> | p <- permutations(characters)};
     best = {<s, stringChars(p), n> | <p, n> <- ranking, n == min(range(ranking))};
     return takeOneFrom(best)[0];
}

public int countSame(list[int] permutations, list[int] characters){
     return (0 | it + 1 | n <- index(characters), permutations[n] == characters[n]);
}
```



## REXX


```rexx
/*REXX program determines and displays the best shuffle for any list of words or tokens.*/
parse arg $                                      /*get some words from the command line.*/
if $=''  then $= 'tree abracadabra seesaw elk grrrrrr up a'          /*use the defaults?*/
w=0;                #=words($)                   /* [↑]  finds the widest word in $ list*/
        do i=1  for #;  @.i=word($,i);  w=max(w, length(@.i) );   end  /*i*/
w= w+9                                           /*add 9 blanks for output indentation. */
        do n=1  for #;  new= bestShuffle(@.n)    /*process the examples in the @ array. */
        same=0;                    do m=1  for length(@.n)
                                   same=same  +  (substr(@.n, m, 1) == substr(new, m, 1) )
                                   end   /*m*/
        say '       original:'   left(@.n, w)    'new:'    left(new,w)    'score:'    same
        end   /*n*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
bestShuffle: procedure; parse arg x 1 ox;    L=length(x);   if L<3  then return reverse(x)
                                                             /*[↑] fast track short strs*/
               do j=1  for L-1;  parse var x =(j) a +1 b +1  /*get A,B at Jth & J+1 pos.*/
               if a\==b  then iterate                        /*ignore any replicates.   */
               c= verify(x,a);    if c==0  then iterate      /*   "    "      "         */
               x= overlay( substr(x,c,1), overlay(a,x,c), j) /*swap the  x,c  characters*/
               rx= reverse(x)                                /*obtain the reverse of X. */
               y= substr(rx, verify(rx, a), 1)               /*get 2nd replicated char. */
               x= overlay(y, overlay(a,x, lastpos(y,x)),j+1) /*fast swap of 2 characters*/
               end   /*j*/
                          do k=1  for L;  a=substr(x, k, 1)      /*handle a possible rep*/
                          if a\==substr(ox, k, 1)  then iterate  /*skip non-replications*/
                          if k==L  then x= left(x, k-2)a || substr(x, k-1,1) /*last case*/
                                   else x= left(x, k-1)substr(x, k+1, 1)a || substr(x,k+2)
                          end   /*k*/
             return x
```

{{out|output|text=  (with a freebie thrown in):}}

```txt

       original: tree                 new: eert                 score: 0
       original: abracadabra          new: baaracadrab          score: 0
       original: seesaw               new: eswase               score: 0
       original: elk                  new: lke                  score: 0
       original: grrrrrr              new: rrrrrrg              score: 5
       original: up                   new: pu                   score: 0
       original: a                    new: a                    score: 1

```



## Ring


```ring

# Project : Best shuffle

test = ["abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"]

for n = 1 to len(test)
     bs   = bestshuffle(test[n])
     count = 0
     for p = 1 to len(test[n])
          if substr(test[n],p,1) = substr(bs,p,1)
             count = count + 1
          ok
      next
      see test[n] + " -> " + bs + " " + count + nl
next

func bestshuffle(s1)
       s2 = s1
       for i = 1 to len(s2)
            for j =  1 to len(s2)
                 if (i != j) and (s2[i] != s1[j]) and (s2[j] != s1[i])
                    if j < i
                       i1 = j
                       j1 = i
                    else
                       i1 = i
                       j1 = j
                    ok
                    s2 = left(s2,i1-1) + substr(s2,j1,1) + substr(s2,i1+1,(j1-i1)-1) + substr(s2,i1,1) + substr(s2,j1+1)
                 ok
            next
       next
       bestshuffle = s2
       return bestshuffle

```

Output:

```txt

abracadabra -> caadrbabaar 0
seesaw -> ewaess 0
elk -> kel 0
grrrrrr -> rgrrrrr 5
up -> pu 0
a -> a 1

```


== {{header|Ruby}} ==
{{works with|Ruby|1.9}}
{{trans|Perl 6}}


```ruby
def best_shuffle(s)
  # Fill _pos_ with positions in the order
  # that we want to fill them.
  pos = []
  # g["a"] = [2, 4] implies that s[2] == s[4] == "a"
  g = s.length.times.group_by { |i| s[i] }

  # k sorts letters from low to high count
  k = g.sort_by { |k, v| v.length }.map { |k, v| k }

  until g.empty?
    k.each do |letter|
      g[letter] or next
      pos.push(g[letter].pop)
      g[letter].empty? and g.delete letter
    end
  end

  # Now fill in _new_ with _letters_ according to each position
  # in _pos_, but skip ahead in _letters_ if we can avoid
  # matching characters that way.
  letters = s.dup
  new = "?" * s.length
  until letters.empty?
    i, p = 0, pos.pop
    i += 1 while letters[i] == s[p] and i < (letters.length - 1)
    new[p] = letters.slice! i
  end

  score = new.chars.zip(s.chars).count { |c, d| c == d }
  [new, score]
end

%w(abracadabra seesaw elk grrrrrr up a).each do |word|
  puts "%s, %s, (%d)" % [word, *best_shuffle(word)]
end
```


{{out}}

```txt

abracadabra, baarrcadaab, (0)
seesaw, essewa, (0)
elk, lke, (0)
grrrrrr, rgrrrrr, (5)
up, pu, (0)
a, a, (1)

```


== {{header|Run BASIC}} ==

```runbasic
list$ = "abracadabra seesaw pop grrrrrr up a"

while word$(list$,ii + 1," ") <> ""
 ii    = ii + 1
 w$    = word$(list$,ii," ")
 bs$   = bestShuffle$(w$)
 count = 0
 for i = 1 to len(w$)
  if mid$(w$,i,1) = mid$(bs$,i,1) then count = count + 1
 next i
 print  w$;" ";bs$;" ";count
wend

function bestShuffle$(s1$)
   s2$   = s1$
   for i = 1 to len(s2$)
        for j =  1 to len(s2$)
            if (i <> j) and (mid$(s2$,i,1) <> mid$(s1$,j,1)) and (mid$(s2$,j,1) <> mid$(s1$,i,1)) then
            if j < i then i1 = j:j1 = i else i1 = i:j1 = j
            s2$ = left$(s2$,i1-1) + mid$(s2$,j1,1) + mid$(s2$,i1+1,(j1-i1)-1) + mid$(s2$,i1,1) + mid$(s2$,j1+1)
            end if
        next j
   next i
bestShuffle$ = s2$
end function
```


Output:


```txt
abracadabra raabadacabr 0
seesaw eswaes 0
pop opp 1
grrrrrr rgrrrrr 5
up pu 0
a a 1
```


== {{header|Rust}} ==
{{libheader|rand}}

```rust
extern crate permutohedron;
extern crate rand;

use std::cmp::{min, Ordering};
use std::env;
use rand::{thread_rng, Rng};
use std::str;

const WORDS: &'static [&'static str] = &["abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"];

#[derive(Eq)]
struct Solution {
    original: String,
    shuffled: String,
    score: usize,
}

// Ordering trait implementations are only needed for the permutations method
impl PartialOrd for Solution {
    fn partial_cmp(&self, other: &Solution) -> Option<Ordering> {
        match (self.score, other.score) {
            (s, o) if s < o => Some(Ordering::Less),
            (s, o) if s > o => Some(Ordering::Greater),
            (s, o) if s == o => Some(Ordering::Equal),
            _ => None,
        }
    }
}


impl PartialEq for Solution {
    fn eq(&self, other: &Solution) -> bool {
        match (self.score, other.score) {
            (s, o) if s == o => true,
            _ => false,
        }
    }
}

impl Ord for Solution {
    fn cmp(&self, other: &Solution) -> Ordering {
        match (self.score, other.score) {
            (s, o) if s < o => Ordering::Less,
            (s, o) if s > o => Ordering::Greater,
            _ => Ordering::Equal,
        }
    }
}

fn _help() {
    println!("Usage: best_shuffle <word1> <word2> ...");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut words: Vec<String> = vec![];

    match args.len() {
        1 => {
            for w in WORDS.iter() {
                words.push(String::from(*w));
            }
        }
        _ => {
            for w in args.split_at(1).1 {
                words.push(w.clone());
            }
        }
    }

    let solutions = words.iter().map(|w| best_shuffle(w)).collect::<Vec<_>>();

    for s in solutions {
        println!("{}, {}, ({})", s.original, s.shuffled, s.score);
    }
}

// Implementation iterating over all permutations
fn _best_shuffle_perm(w: &String) -> Solution {
    let mut soln = Solution {
        original: w.clone(),
        shuffled: w.clone(),
        score: w.len(),
    };
    let w_bytes: Vec<u8> = w.clone().into_bytes();
    let mut permutocopy = w_bytes.clone();
    let mut permutations = permutohedron::Heap::new(&mut permutocopy);
    while let Some(p) = permutations.next_permutation() {
        let hamm = hamming(&w_bytes, p);
        soln = min(soln,
                   Solution {
                       original: w.clone(),
                       shuffled: String::from(str::from_utf8(p).unwrap()),
                       score: hamm,
                   });
        // Accept the solution if score 0 found
        if hamm == 0 {
            break;
        }
    }
    soln
}

// Quadratic implementation
fn best_shuffle(w: &String) -> Solution {
    let w_bytes: Vec<u8> = w.clone().into_bytes();
    let mut shuffled_bytes: Vec<u8> = w.clone().into_bytes();

    // Shuffle once
    let sh: &mut [u8] = shuffled_bytes.as_mut_slice();
    thread_rng().shuffle(sh);

    // Swap wherever it doesn't decrease the score
    for i in 0..sh.len() {
        for j in 0..sh.len() {
            if (i == j) | (sh[i] == w_bytes[j]) | (sh[j] == w_bytes[i]) | (sh[i] == sh[j]) {
                continue;
            }
            sh.swap(i, j);
            break;
        }
    }

    let res = String::from(str::from_utf8(sh).unwrap());
    let res_bytes: Vec<u8> = res.clone().into_bytes();
    Solution {
        original: w.clone(),
        shuffled: res,
        score: hamming(&w_bytes, &res_bytes),
    }
}

fn hamming(w0: &Vec<u8>, w1: &Vec<u8>) -> usize {
    w0.iter().zip(w1.iter()).filter(|z| z.0 == z.1).count()
}

```

{{out}}

```txt

abracadabra, caadabarabr, (0)
seesaw, esswea, (0)
elk, lke, (0)
grrrrrr, rrrrgrr, (5)
up, pu, (0)
a, a, (1)

```



## Scala

There are two implementations. One is simple but exponential and very inefficient. The second one is quadratic. Both are pure functional. Given quadratic solution has a bigger constant than the one used in the Python implementation, but doesn't use mutable datastructures.

```scala

  def coincidients(s1: Seq[Char], s2: Seq[Char]): Int = (s1, s2).zipped.count(p => (p._1 == p._2))
  def freqMap(s1: List[Char]) = s1.groupBy(_.toChar).mapValues(_.size)
  def estimate(s1: List[Char]): Int = if (s1 == Nil) 0 else List(0, freqMap(s1).maxBy(_._2)._2 - (s1.size / 2)).max

  def bestShuffle(s: String): Pair[String, Int] = {
    if (s == "") return ("", 0) else {}
    val charList = s.toList
    val estim = estimate(charList)

    // purely functional polynomial solution
    def doStep(accu: List[Pair[Int, Int]], sourceFreqMap: Map[Int, Int], targetFreqMap: Map[Int, Int], stepsLeft: Int): List[Pair[Int, Int]] = {
      if (stepsLeft == 0) accu else {
        val srcChoices = sourceFreqMap.groupBy(_._2).minBy(_._1)._2
        val src = srcChoices.toList.apply(Random.nextInt(srcChoices.size))._1

        val tgtChoices = targetFreqMap.map(p => if (charList(p._1) != charList(src)) (p._1, p._2) else (p._1, Int.MaxValue / 2)).groupBy(_._2).minBy(_._1)._2
        val tgt = tgtChoices.toList.apply(Random.nextInt(tgtChoices.size))._1
        doStep((src, tgt) :: accu,
          sourceFreqMap.filterKeys(_ != src).map(p => if (charList(p._1) != charList(tgt)) (p._1, p._2 - 1) else (p._1, p._2)),
          targetFreqMap.filterKeys(_ != tgt).map(p => if (charList(p._1) != charList(src)) (p._1, p._2 - 1) else (p._1, p._2)),
          stepsLeft - 1)
      }
    }

    val leftFreqMap: Map[Int, Int] = charList.zipWithIndex.map(p => (p._2, p._1)).toMap.mapValues(x => freqMap(charList).mapValues(charList.size - _)(x))

    val substs = doStep(List(), leftFreqMap, leftFreqMap, charList.size)
    val res = substs.sortBy(_._1).map(p => charList(p._2))
    (res.mkString, coincidients(charList, res))

    // exponential solution (inefficient)
    //Random.shuffle(charList).permutations.find(coincidients(charList, _) <= estim)

  }

```

The test code:

```scala

  def main(args: Array[String]): Unit = {
    println(bestShuffle("abracadabra"));
    println(bestShuffle("seesaw"));
    println(bestShuffle("elk"));
    println(bestShuffle("grrrrrr"));
    println(bestShuffle("up"));
    println(bestShuffle("a"));

    BestShuffleSpecification.check
  }

```

{{out}}

```txt

(bcabadaraar,0)
(easews,0)
(kel,0)
(rgrrrrr,5)
(pu,0)
(a,1)

```

The ScalaCheck code

```scala

object BestShuffleSpecification extends Properties("BestShuffle") {

  property("size") = forAll { (src: String) =>
    val s = Main.bestShuffle(src)
    s._1.size == src.size
  }

  property("freq") = forAll { (src: String) =>
    val s = Main.bestShuffle(src)
    Main.freqMap(s._1.toList) == Main.freqMap(src.toList)
  }

  property("estimate") = forAll { (src: String) =>
    val s = Main.bestShuffle(src)
    Main.estimate(src.toList) == s._2
  }

}

```




## Scheme


```scheme

(define count
  (lambda (str1 str2)
    (let ((len (string-length str1)))
      (let loop ((index 0)
                 (result 0))
        (if (= index len)
            result
            (loop (+ index 1)
                  (if (eq? (string-ref str1 index)
                           (string-ref str2 index))
                      (+ result 1)
                      result)))))))

(define swap
  (lambda (str index1 index2)
    (let ((mutable (string-copy str))
          (char1 (string-ref str index1))
          (char2 (string-ref str index2)))
      (string-set! mutable index1 char2)
      (string-set! mutable index2 char1)
      mutable)))

(define shift
  (lambda (str)
    (string-append (substring str 1 (string-length str))
                   (substring str 0 1))))

(define shuffle
  (lambda (str)
    (let* ((mutable (shift str))
           (len (string-length mutable))
           (max-index (- len 1)))
      (let outer ((index1 0)
                  (best mutable)
                  (best-count (count str mutable)))
        (if (or (< max-index index1)
                (= best-count 0))
            best
            (let inner ((index2 (+ index1 1))
                        (best best)
                        (best-count best-count))
              (if (= len index2)
                  (outer (+ index1 1)
                         best
                         best-count)
                  (let* ((next-mutable (swap best index1 index2))
                         (next-count (count str next-mutable)))
                    (if (= 0 next-count)
                        next-mutable
                        (if (< next-count best-count)
                            (inner (+ index2 1)
                                   next-mutable
                                   next-count)
                            (inner (+ index2 1)
                                   best
                                   best-count)))))))))))


(for-each
 (lambda (str)
   (let ((shuffled (shuffle str)))
     (display
      (string-append str " " shuffled " ("
                     (number->string (count str shuffled)) ")\n"))))
 '("abracadabra" "seesaw" "elk" "grrrrrr" "up" "a"))

```


Output:

```txt

abracadabra baacadabrar (0)
seesaw easews (0)
elk lke (0)
grrrrrr rrrrrrg (5)
up pu (0)
a a (1)

```



## Seed7


```seed7
$ include "seed7_05.s7i";

const func string: bestShuffle (in string: stri) is func
  result
    var string: shuffled is "";
  local
    var char: tmp is ' ';
    var integer: i is 0;
    var integer: j is 0;
  begin
    shuffled := stri;
    for key i range shuffled do
      for key j range shuffled do
        if i <> j and stri[i] <> shuffled[j] and stri[j] <> shuffled[i] then
          tmp  := shuffled[i];
          shuffled @:= [i] shuffled[j];
          shuffled @:= [j] tmp;
        end if;
      end for;
    end for;
  end func;

const proc: main is func
  local
    const array string: testData is [] ("abracadabra", "seesaw", "elk", "grrrrrr", "up", "a");
    var string: original is "";
    var string: shuffled is "";
    var integer: j is 0;
    var integer: score is 0;
  begin
    for original range testData do
      shuffled := bestShuffle(original);
      score := 0;
      for key j range shuffled do
        if original[j] = shuffled[j] then
          incr(score);
        end if;
      end for;
      writeln(original <& ", " <& shuffled <& ", (" <& score <& ")");
    end for;
  end func;
```


Output:

```txt

abracadabra, caadrbabaar, (0)
seesaw, ewaess, (0)
elk, kel, (0)
grrrrrr, rgrrrrr, (5)
up, pu, (0)
a, a, (1)

```



## Sidef

{{trans|Go}}

```ruby
func best_shuffle(String orig) -> (String, Number) {

    var s = orig.chars
    var t = s.shuffle

    for i (^s) {
        for j (^s) {
            if (i!=j && t[i]!=s[j] && t[j]!=s[i]) {
                t[i, j] = t[j, i]
                break
            }
        }
    }

    (t.join, s ~Z== t -> count(true))
}

for word (<abracadabra seesaw elk grrrrrr up a>) {
    var (sword, score) = best_shuffle(word)
    "%-12s %12s: %d\n".printf(word, sword, score)
}
```

{{out}}

```txt
abracadabra   daabacarrab: 0
seesaw             esaews: 0
elk                   lke: 0
grrrrrr           rgrrrrr: 5
up                     pu: 0
a                       a: 1
```



## Tcl

{{tcllib|struct::list}}

```tcl
package require Tcl 8.5
package require struct::list

# Simple metric function; assumes non-empty lists
proc count {l1 l2} {
    foreach a $l1 b $l2 {incr total [string equal $a $b]}
    return $total
}
# Find the best shuffling of the string
proc bestshuffle {str} {
    set origin [split $str ""]
    set best $origin
    set score [llength $origin]
    struct::list foreachperm p $origin {
	if {$score > [set score [tcl::mathfunc::min $score [count $origin $p]]]} {
	    set best $p
	}
    }
    set best [join $best ""]
    return "$str,$best,($score)"
}
```

Demonstration:

```tcl
foreach sample {abracadabra seesaw elk grrrrrr up a} {
    puts [bestshuffle $sample]
}
```

Output:

```txt

abracadabra,baabacadrar,(0)
seesaw,assewe,(0)
elk,kel,(0)
grrrrrr,rgrrrrr,(5)
up,pu,(0)
a,a,(1)

```



## Ursala

An implementation based on the J solution looks like this.

```Ursala
#import std
#import nat

words = <'abracadabra','seesaw','elk','grrrrrr','up','a'>

shuffle = num; ^H/(*@K24) ^H\~&lS @rK2lSS *+ ^arPfarhPlzPClyPCrtPXPRalPqzyCipSLK24\~&L leql$^NS

#show+

main = ~&LS <.~&l,@r :/` ,' ('--+ --')'+ ~&h+ %nP+ length@plrEF>^(~&,shuffle)* words
```

A solution based on exponential search would use this definition of <code>shuffle</code> (cf. Haskell and Tcl).

```Ursala
shuffle = ~&r+ length@plrEZF$^^D/~& permutations
```

output:

```txt
abracadabra caarrbabaad (0)
seesaw wssaee (0)
elk lke (0)
grrrrrr rgrrrrr (5)
up pu (0)
a a (1)
```



## VBA



```vb

Option Explicit

Sub Main_Best_shuffle()
Dim S() As Long, W, b As Byte, Anagram$, Count&, myB As Boolean, Limit As Byte, i As Integer

    W = Array("a", "abracadabra", "seesaw", "elk", "grrrrrr", "up", "qwerty", "tttt")
    For b = 0 To UBound(W)
        Count = 0
        Select Case Len(W(b))
            Case 1: Limit = 1
            Case Else
                i = NbLettersDiff(W(b))
                If i >= Len(W(b)) \ 2 Then
                    Limit = 0
                ElseIf i = 1 Then
                    Limit = Len(W(b))
                Else
                    Limit = Len(W(b)) - i
                End If
        End Select
RePlay:
        Do
            S() = ShuffleIntegers(Len(W(b)))
            myB = GoodShuffle(S, Limit)
        Loop While Not myB
        Anagram = ShuffleWord(CStr(W(b)), S)
        Count = Nb(W(b), Anagram)
        If Count > Limit Then GoTo RePlay
        Debug.Print W(b) & " ==> " & Anagram & " (Score : " & Count & ")"
    Next
End Sub

Function ShuffleIntegers(l As Long) As Long()
Dim i As Integer, ou As Integer, temp() As Long
Dim C As New Collection

    ReDim temp(l - 1)
    If l = 1 Then
        temp(0) = 0
    ElseIf l = 2 Then
        temp(0) = 1: temp(1) = 0
    Else
        Randomize
        Do
            ou = Int(Rnd * l)
            On Error Resume Next
            C.Add CStr(ou), CStr(ou)
            If Err <> 0 Then
                On Error GoTo 0
            Else
                temp(ou) = i
                i = i + 1
            End If
        Loop While C.Count <> l
    End If
    ShuffleIntegers = temp
End Function

Function GoodShuffle(t() As Long, Lim As Byte) As Boolean
Dim i&, C&

    For i = LBound(t) To UBound(t)
        If t(i) = i Then C = C + 1
    Next i
    GoodShuffle = (C <= Lim)
End Function

Function ShuffleWord(W$, S() As Long) As String
Dim i&, temp, strR$

    temp = Split(StrConv(W, vbUnicode), Chr(0))
    For i = 0 To UBound(S)
        strR = strR & temp(S(i))
    Next i
    ShuffleWord = strR
End Function

Function Nb(W, A) As Integer
Dim i As Integer, l As Integer

    For i = 1 To Len(W)
        If Mid(W, i, 1) = Mid(A, i, 1) Then l = l + 1
    Next i
    Nb = l
End Function

Function NbLettersDiff(W) As Integer
Dim i&, C As New Collection
    For i = 1 To Len(W)
        On Error Resume Next
        C.Add Mid(W, i, 1), Mid(W, i, 1)
    Next i
    NbLettersDiff = C.Count
End Function

```

{{out}}

```txt
a ==> a (Score : 1)
abracadabra ==> baacdbaraar (Score : 0)
seesaw ==> awsees (Score : 0)
elk ==> kel (Score : 0)
grrrrrr ==> rgrrrrr (Score : 5)
up ==> pu (Score : 0)
qwerty ==> eytwrq (Score : 0)
tttt ==> tttt (Score : 4)
```



## VBScript

{{trans|Java}}

```vb
'Best Shuffle Task
'VBScript Implementation

Function bestshuffle(s)
    Dim arr:Redim arr(Len(s)-1)

    'The Following Does the toCharArray() Functionality
    For i = 0 To Len(s)-1
        arr(i) = Mid(s, i + 1, 1)
    Next

    arr = shuffler(arr)     'Make this line a comment for deterministic solution
    For i = 0 To UBound(arr):Do
        If arr(i) <> Mid(s, i + 1, 1) Then Exit Do
        For j = 0 To UBound(arr)
            If arr(i) <> arr(j) And arr(i) <> Mid(s, j + 1, 1) And arr(j) <> Mid(s, i + 1, 1) Then
                tmp = arr(i)
                arr(i) = arr(j)
                arr(j) = tmp
            End If
        Next
    Loop While False:Next

    shuffled_word = Join(arr,"")

    'This section is the scorer
    score = 0
    For k = 1 To Len(s)
        If Mid(s,k,1) = Mid(shuffled_word,k,1) Then
            score = score + 1
        End If
    Next

    bestshuffle = shuffled_word & ",(" & score & ")"
End Function

Function shuffler(array)
    Set rand = CreateObject("System.Random")
    For i = UBound(array) to 0 Step -1
       r = rand.next_2(0, i + 1)
       tmp = array(i)
       array(i) = array(r)
       array(r) = tmp
    Next
    shuffler = array
End Function

'Testing the function
word_list = Array("abracadabra","seesaw","elk","grrrrrr","up","a")
For Each word In word_list
    WScript.StdOut.WriteLine word & "," & bestshuffle(word)
Next
```


{{Out}}

```txt
abracadabra,caadbrabaar,(0)
seesaw,essawe,(0)
elk,kel,(0)
grrrrrr,rrrrgrr,(5)
up,pu,(0)
a,a,(1)
```



## XPL0


```XPL0
include c:\cxpl\codes;  \'code' declarations
string 0;               \use zero-terminated string convention

func StrLen(A);         \Return number of characters in an ASCIIZ string
char A;
int  I;
for I:= 0 to -1>>1-1 do
        if A(I) = 0 then return I;

proc Shuffle(W0);       \Display best shuffle of characters in a word
char W0;
char W(20), SW(20);
int  L, I, S, SS, C, T;
[L:= StrLen(W0);                        \word length
for I:= 0 to L do W(I):= W0(I);         \get working copy of word (including 0)
SS:= 20;                                \initialize best (saved) score
for C:= 1 to 1_000_000 do               \overkill? XPL0 is fast
        [I:= Ran(L);                    \shuffle: swap random char with end char
        T:= W(I);  W(I):= W(L-1);  W(L-1):= T;
        S:= 0;                          \compute score
        for I:= 0 to L-1 do
                if W(I) = W0(I) then S:= S+1;
        if S < SS then
                [SS:= S;                \save best score and best shuffle
                for I:= 0 to L do SW(I):= W(I);
                ];
        ];
Text(0, W0);  Text(0, ", ");            \show original and shuffled words, score
Text(0, SW);  Text(0, ", (");  IntOut(0, SS);  ChOut(0, ^));  CrLf(0);
];

int S, I;
[S:= ["abracadabra", "seesaw", "elk", "grrrrrr", "up", "a"];
for I:= 0 to 5 do Shuffle(S(I));
]
```


Output:

```txt

abracadabra, drababaraac, (0)
seesaw, easwes, (0)
elk, lke, (0)
grrrrrr, rrrrrrg, (5)
up, pu, (0)
a, a, (1)

```



## zkl

{{trans|D}}
{{trans|Common Lisp}}

```zkl
fcn bestShuffle(str){
   s:=str.split("").shuffle();    // -->List
   if(not s) return(str,str.len()); // can't shuffle "" or "a"

   n:=str.len();
   foreach i in (n){
      foreach j in (n){
         if (i!=j and s[i]!=str[j] and s[j]!=str[i]){
	    s.swap(i,j);
	    break;
	 }
      }
   }
   return(s.concat(), s.zipWith('==,str).sum(0));
}
```


```zkl
ss:=T("abracadabra","immediately","grrrrrr","seesaw","pop","up","a","");
foreach s in (ss){
   ns,cnt:=bestShuffle(s);
   println("%s --> %s (%d)".fmt(s,ns,cnt));
}
```

{{out}}

```txt

abracadabra --> raabaracadb (0)
immediately --> mietlmedyia (0)
grrrrrr --> rgrrrrr (5)
seesaw --> asswee (0)
pop --> opp (1)
up --> pu (0)
a --> a (1)
 -->  (0)

```



## ZX Spectrum Basic

{{trans|AWK}}

```zxbasic
10 FOR n=1 TO 6
20 READ w$
30 GO SUB 1000
40 LET count=0
50 FOR i=1 TO LEN w$
60 IF w$(i)=b$(i) THEN LET count=count+1
70 NEXT i
80 PRINT w$;" ";b$;" ";count
90 NEXT n
100 STOP
1000 REM Best shuffle
1010 LET b$=w$
1020 FOR i=1 TO LEN b$
1030 FOR j=1 TO LEN b$
1040 IF (i<>j) AND (b$(i)<>w$(j)) AND (b$(j)<>w$(i)) THEN LET t$=b$(i): LET b$(i)=b$(j): LET b$(j)=t$
1110 NEXT j
1120 NEXT i
1130 RETURN
2000 DATA "abracadabra","seesaw","elk","grrrrrr","up","a"

```

{{out}}

```txt
abracadabra caadrbabaar 0
seesaw ewaess 0
elk kel 0
grrrrrr rgrrrrr 5
up pu 0
a a 1
```


{{omit from|bc|No string operations.}}
{{omit from|dc|No string operations.}}
