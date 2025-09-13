+++
title = "Natural sorting"
description = ""
date = 2018-12-05T06:56:21Z
aliases = []
[extra]
id = 9454
[taxonomies]
categories = ["task", "Sorting Algorithms"]
tags = []
+++

## Task

Natural sorting is the sorting of text that does more than rely on the
order of individual characters codes to make the finding of
individual strings easier for a ''human'' reader.

There is no "one true way" to do this, but for the purpose of this task 'natural' orderings might include:
:1.  Ignore leading, trailing and multiple adjacent spaces
:2.  Make all whitespace characters equivalent.
:3.  Sorting without regard to case.
:4.  Sorting numeric portions of strings in numeric order. That is split the string into fields on numeric boundaries, then sort on each field, with the rightmost fields being the most significant, and numeric fields of integers treated as numbers.
:: foo9.txt before foo10.txt
:: As well as ... x9y99 before x9y100, before x10y0
:: ... (for any number of groups of integers in a string).
:5.  Title sorts: without regard to a leading, very common, word such
:: as 'The' in "The thirty-nine steps".
:6.  Sort letters without regard to accents.
:7.  Sort ligatures as separate letters.
:8.  Replacements:
:: Sort german scharfes S (ß) as ss
:: Sort ſ,  LATIN SMALL LETTER LONG S as s
:: Sort ʒ,  LATIN SMALL LETTER EZH as s
:: ...

;Task Description
* '''Implement the first four''' of the eight given features in a natural sorting routine/function/method...
* Test each feature implemented separately with an ordered list of test strings from the 'Sample inputs' section below, and make sure your naturally sorted output is in the same order as other language outputs such as Python.
* Print and display your output.

* '''For extra credit''' implement more than the first four.

Note: It is not necessary to have individual control of which features are active in the natural sorting routine at any time.

;Sample input:


```txt

# Ignoring leading spaces
Text strings:
['ignore leading spaces: 2-2', ' ignore leading spaces: 2-1', '  ignore leading spaces: 2+0', '   ignore leading spaces: 2+1']

# Ignoring multiple adjacent spaces (m.a.s)
Text strings:
['ignore m.a.s spaces: 2-2', 'ignore m.a.s  spaces: 2-1', 'ignore m.a.s   spaces: 2+0', 'ignore m.a.s    spaces: 2+1']


# Equivalent whitespace characters
Text strings:
['Equiv. spaces: 3-3', 'Equiv.\rspaces: 3-2', 'Equiv.\x0cspaces: 3-1', 'Equiv.\x0bspaces: 3+0', 'Equiv.\nspaces: 3+1', 'Equiv.\tspaces: 3+2']

# Case Indepenent sort
Text strings:
['cASE INDEPENENT: 3-2', 'caSE INDEPENENT: 3-1', 'casE INDEPENENT: 3+0', 'case INDEPENENT: 3+1']

# Numeric fields as numerics
Text strings:
['foo100bar99baz0.txt', 'foo100bar10baz0.txt', 'foo1000bar99baz10.txt', 'foo1000bar99baz9.txt']

# Title sorts
Text strings:
['The Wind in the Willows', 'The 40th step more', 'The 39 steps', 'Wanda']

# Equivalent accented characters (and case)
Text strings:
[u'Equiv. \xfd accents: 2-2', u'Equiv. \xdd accents: 2-1', u'Equiv. y accents: 2+0', u'Equiv. Y accents: 2+1']


# Separated ligatures
Text strings:
[u'\u0132 ligatured ij', 'no ligature']

# Character replacements
Text strings:
[u'Start with an \u0292: 2-2', u'Start with an \u017f: 2-1', u'Start with an \xdf: 2+0', u'Start with an s: 2+1']
```




## C

Some differences from task requirement:
# req 1 and 2 are not separated.  I can't imagine a situation where I'd want one but not the other.
# req 5 is implemented differently: not only leading "the", but some common words like "it", "to" etc are discarded everywhere in the string
# req 6, 7, 8 are pretty much the same thing, so the implementation doesn't make too much of a distinction of it ("ß" is a ligature, you know.)

Besides the numeric part, everything else was done in a uniform way by transforming input strings into some normalized format and comparing those instead.  All sort options flags can be freely mixed together.  C source is written in UTF-8 for easier reading here: don't do this for serious code.

```c
#include <stdio.h>
#include <stdlib.h>
#include <wchar.h>
#include <wctype.h>
#include <string.h>
#include <locale.h>

typedef struct wstr {
	wchar_t *s;
	int n, alloc;
} wstr;

#define w_del(w) { free(w->s); free(w); }
#define forchars(i, c, w) for(i = 0, c = w->s[0]; i < w->n && c; c = w->s[++i])
wstr *w_new()
{
	wstr *w = malloc(sizeof(wstr));
	w->alloc = 1;
	w->n = 0;
	w->s = malloc(sizeof(wchar_t));
	w->s[0] = 0;
	return w;
}

void w_append(wstr *w, wchar_t c)
{
	int n = w->n + 1;
	if (n >= w->alloc) {
		w->alloc *= 2;
		w->s = realloc(w->s, w->alloc * sizeof(wchar_t));
	}
	w->s[w->n++] = c;
	w->s[w->n] = 0;
}

wstr *w_make(wchar_t *s)
{
	int i, len = wcslen(s);
	wstr *w = w_new();
	for (i = 0; i < len; i++) w_append(w, s[i]);
	return w;
}

typedef void (*wtrans_func)(wstr *, wstr *);
void w_transform(wstr *in, wtrans_func f)
{
	wstr t, *out = w_new();
	f(in, out);
	t = *in; *in = *out; *out = t;
	w_del(out);
}
#define transfunc(x) void w_##x(wstr *in, wstr *out)

transfunc(nocase) {
	int i;
	wchar_t c;
	forchars(i, c, in) w_append(out, towlower(c));
}

transfunc(despace) {
	int i, gotspace = 0;
	wchar_t c;
	forchars(i, c, in) {
		if (!iswspace(c)) {
			if (gotspace && out->n)
				w_append(out, L' ');
			w_append(out, c);
			gotspace = 0;
		} else	gotspace = 1;
	}
}

static const wchar_t *const tbl_accent[] = { /* copied from Perl6 code */
	L"Þ", L"TH", L"þ", L"th", L"Ð", L"TH", L"ð", L"th", L"À", L"A",
	L"Á", L"A", L"Â", L"A", L"Ã", L"A", L"Ä", L"A", L"Å", L"A", L"à",
	L"a", L"á", L"a", L"â", L"a", L"ã", L"a", L"ä", L"a", L"å", L"a",
	L"Ç", L"C", L"ç", L"c", L"È", L"E", L"É", L"E", L"Ê", L"E", L"Ë",
	L"E", L"è", L"e", L"é", L"e", L"ê", L"e", L"ë", L"e", L"Ì",
	L"I", L"Í", L"I", L"Î", L"I", L"Ï", L"I", L"ì", L"i", L"í",
	L"i", L"î", L"i", L"ï", L"i", L"Ò", L"O", L"Ó", L"O", L"Ô",
	L"O", L"Õ", L"O", L"Ö", L"O", L"Ø", L"O", L"ò", L"o", L"ó", L"o",
	L"ô", L"o", L"õ", L"o", L"ö", L"o", L"ø", L"o", L"Ñ", L"N", L"ñ", L"n",
	L"Ù", L"U", L"Ú", L"U", L"Û", L"U", L"Ü", L"U", L"ù", L"u", L"ú", L"u",
	L"û", L"u", L"ü", L"u", L"Ý", L"Y", L"ÿ", L"y", L"ý", L"y" };

static const wchar_t *const tbl_ligature[] = {
	L"Æ", L"AE", L"æ", L"ae", L"ß", L"ss",
	L"ﬄ", L"ffl", L"ﬃ", L"ffi", L"ﬁ", L"fi", L"ﬀ", L"ff", L"ﬂ", L"fl",
	L"ſ", L"s", L"ʒ", L"z", L"ﬆ", L"st", /* ... come on ... */
};

void w_char_repl(wstr *in, wstr *out, const wchar_t *const *tbl, int len)
{
	int i, j, k;
	wchar_t c;
	forchars(i, c, in) {
		for (j = k = 0; j < len; j += 2) {
			if (c != tbl[j][0]) continue;
			for (k = 0; tbl[j + 1][k]; k++)
				w_append(out, tbl[j + 1][k]);
			break;
		}
		if (!k) w_append(out, c);
	}
}

transfunc(noaccent) {
	w_char_repl(in, out, tbl_accent, sizeof(tbl_accent)/sizeof(wchar_t*));
}

transfunc(noligature) {
	w_char_repl(in, out, tbl_ligature, sizeof(tbl_ligature)/sizeof(wchar_t*));
}

static const wchar_t *const tbl_article[] = {
	L"the", L"a", L"of", L"to", L"is", L"it" };
#define N_ARTICLES sizeof(tbl_article)/sizeof(tbl_article[0])
transfunc(noarticle) {
	int i, j, n;
	wchar_t c, c0 = 0;
	forchars(i, c, in) {
		if (!c0 || (iswalnum(c) && !iswalnum(c0))) { /* word boundary */
			for (j = N_ARTICLES - 1; j >= 0; j--) {
				n = wcslen(tbl_article[j]);
				if (wcsncasecmp(in->s + i, tbl_article[j], n))
					continue;
				if (iswalnum(in->s[i + n])) continue;
				i += n;
				break;
			}
			if (j < 0) w_append(out, c);
		} else
			w_append(out, c);
		c0 = c;
	}
}

enum { wi_space = 0, wi_case, wi_accent, wi_lig, wi_article, wi_numeric };
#define WS_NOSPACE	(1 << wi_space)
#define WS_NOCASE	(1 << wi_case)
#define WS_ACCENT	(1 << wi_accent)
#define WS_LIGATURE	(1 << wi_lig)
#define WS_NOARTICLE	(1 << wi_article)
#define WS_NUMERIC	(1 << wi_numeric)
const wtrans_func trans_funcs[] = {
	w_despace, w_nocase, w_noaccent, w_noligature, w_noarticle, 0
};
const char *const flagnames[] = {
	"collapse spaces",
	"case insensitive",
	"disregard accent",
	"decompose ligatures",
	"discard common words",
	"numeric",
};

typedef struct { wchar_t* s; wstr *w; } kw_t;
int w_numcmp(const void *a, const void *b)
{
	wchar_t *pa = ((const kw_t*)a)->w->s, *pb = ((const kw_t*)b)->w->s;
	int sa, sb, ea, eb;
	while (*pa && *pb) {
		if (iswdigit(*pa) && iswdigit(*pb)) {
			/* skip leading zeros */
			sa = sb = 0;
			while (pa[sa] == L'0') sa++;
			while (pb[sb] == L'0') sb++;
			/* find end of numbers */
			ea = sa; eb = sb;
			while (iswdigit(pa[ea])) ea++;
			while (iswdigit(pb[eb])) eb++;
			if (eb - sb > ea - sa) return -1;
			if (eb - sb < ea - sa) return 1;
			while (sb < eb) {
				if (pa[sa] > pb[sb]) return 1;
				if (pa[sa] < pb[sb]) return -1;
				sa++; sb++;
			}

			pa += ea; pb += eb;
		}
		else if (iswdigit(*pa)) return 1;
		else if (iswdigit(*pb)) return -1;
		else {
			if (*pa > *pb) return 1;
			if (*pa < *pb) return -1;
			pa++; pb++;
		}
	}
	return (!*pa && !*pb) ? 0 : *pa ?  1 : -1;
}

int w_cmp(const void *a, const void *b)
{
	return wcscmp(((const kw_t*)a)->w->s, ((const kw_t*)b)->w->s);
}

void natural_sort(wchar_t **strings, int len, int flags)
{
	int i, j;
	kw_t *kws = malloc(sizeof(kw_t) * len);

	for (i = 0; i < len; i++) {
		kws[i].s = strings[i];
		kws[i].w = w_make(strings[i]);
		for (j = 0; j < wi_numeric; j++)
			if (flags & (1 << j) && trans_funcs[j])
				w_transform(kws[i].w, trans_funcs[j]);
	}

	qsort(kws, len, sizeof(kw_t), (flags & WS_NUMERIC) ? w_numcmp : w_cmp);
	for (i = 0; i < len; i++) {
		w_del(kws[i].w);
		strings[i] = kws[i].s;
	}
	free(kws);
}

const wchar_t *const test[] = {
	L" 0000098 nina", L"100 niño", L"99 Ninja", L"100 NINA",
	L" The work is so diﬃcult to do it took ſome 100 aeons.  ",
	L"The work is so difficult it took some 100 aeons.",
	L"  The work is so diﬃcult   it took ſome 99 æons.  ",
};
#define N_STRINGS sizeof(test)/sizeof(*test)

void test_sort(int flags)
{
	int i, j;
	const wchar_t *str[N_STRINGS];
	memcpy(str, test, sizeof(test));

	printf("Sort flags: (");
	for (i = 0, j = flags; j; i++, j >>= 1)
		if ((j & 1))
			printf("%s%s", flagnames[i], j > 1 ? ", ":")\n");

	natural_sort((wchar_t **)str, N_STRINGS, flags);

	for (i = 0; i < N_STRINGS; i++)
		printf("%ls\n", str[i]);
	printf("\n");
}

int main()
{
	setlocale(LC_CTYPE, "");

	test_sort(WS_NOSPACE);
	test_sort(WS_NOCASE);
	test_sort(WS_NUMERIC);
	test_sort(WS_NOARTICLE|WS_NOSPACE);
	test_sort(WS_NOCASE|WS_NOSPACE|WS_ACCENT);
	test_sort(WS_LIGATURE|WS_NOCASE|WS_NOSPACE|WS_NUMERIC|WS_ACCENT|WS_NOARTICLE);

	return 0;
}
```
output<lang>Sort flags: (collapse spaces)
 0000098 nina
100 NINA
100 niño
99 Ninja
The work is so difficult it took some 100 aeons.
  The work is so diﬃcult   it took ſome 99 æons.
 The work is so diﬃcult to do it took ſome 100 aeons.

Sort flags: (case insensitive)
  The work is so diﬃcult   it took ſome 99 æons.
 0000098 nina
 The work is so diﬃcult to do it took ſome 100 aeons.
100 NINA
100 niño
99 Ninja
The work is so difficult it took some 100 aeons.

Sort flags: (numeric)
  The work is so diﬃcult   it took ſome 99 æons.
 The work is so diﬃcult to do it took ſome 100 aeons.
 0000098 nina
The work is so difficult it took some 100 aeons.
99 Ninja
100 NINA
100 niño

Sort flags: (collapse spaces, discard common words)
 0000098 nina
100 NINA
100 niño
99 Ninja
The work is so difficult it took some 100 aeons.
 The work is so diﬃcult to do it took ſome 100 aeons.
  The work is so diﬃcult   it took ſome 99 æons.

Sort flags: (collapse spaces, case insensitive, disregard accent)
 0000098 nina
100 NINA
100 niño
99 Ninja
The work is so difficult it took some 100 aeons.
  The work is so diﬃcult   it took ſome 99 æons.
 The work is so diﬃcult to do it took ſome 100 aeons.

Sort flags: (collapse spaces, case insensitive, disregard accent, decompose ligatures, discard common words, numeric)
 The work is so diﬃcult to do it took ſome 100 aeons.
  The work is so diﬃcult   it took ſome 99 æons.
The work is so difficult it took some 100 aeons.
 0000098 nina
99 Ninja
100 NINA
100 niño
```



## D

Implements requests 1-5.

```d
import std.stdio, std.string, std.algorithm, std.array, std.conv,
    std.ascii, std.range;

string[] naturalSort(string[] arr) /*pure @safe*/ {
    static struct Part {
        string s;

        int opCmp(in ref Part other) const pure {
            return (s[0].isDigit && other.s[0].isDigit) ?
                cmp([s.to!ulong], [other.s.to!ulong]) :
                cmp(s, other.s);
        }
    }

    static mapper(in string txt) /*pure nothrow @safe*/ {
        auto r = txt
                 .strip
                 .tr(whitespace, " ", "s")
                 .toLower
                 .chunkBy!isDigit
                 .map!(p => Part(p.text))
                 .array;
        return (r.length > 1 && r[0].s == "the") ? r.dropOne : r;
    }

    return arr.schwartzSort!mapper.release;
}

void main() /*@safe*/ {
    const tests = [
        // Ignoring leading spaces.
        ["ignore leading spaces: 2-2", " ignore leading spaces: 2-1", "
     ignore leading spaces: 2+1", "  ignore leading spaces: 2+0"],

        // Ignoring multiple adjacent spaces (m.a.s).
        ["ignore m.a.s spaces: 2-2", "ignore m.a.s  spaces: 2-1",
         "ignore m.a.s   spaces: 2+0", "ignore m.a.s    spaces: 2+1"],

        // Equivalent whitespace characters.
        ["Equiv. spaces: 3-3", "Equiv.\rspaces: 3-2",
         "Equiv.\x0cspaces: 3-1", "Equiv.\x0bspaces: 3+0",
         "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2"],

        // Case Indepenent [sic] sort.
        ["cASE INDEPENENT: 3-2" /* [sic] */, "caSE INDEPENENT: 3-1" /* [sic] */,
         "casE INDEPENENT: 3+0" /* [sic] */, "case INDEPENENT: 3+1" /* [sic] */],

        // Numeric fields as numerics.
        ["foo100bar99baz0.txt", "foo100bar10baz0.txt",
         "foo1000bar99baz10.txt", "foo1000bar99baz9.txt"],

        // Title sorts.
        ["The Wind in the Willows", "The 40th step more",
         "The 39 steps", "Wanda"]];

    void printTexts(Range)(string tag, Range range) {
        const sic = range.front.canFind("INDEPENENT") ? " [sic]" : "";
        writefln("\n%s%s:\n%-(  |%s|%|\n%)", tag, sic, range);
    }

    foreach (test; tests) {
        printTexts("Test strings", test);
        printTexts("Normally sorted", test.dup.sort());
        printTexts("Naturally sorted", test.dup.naturalSort());
    }
}

```

```txt
Test strings:
  |ignore leading spaces: 2-2|
  | ignore leading spaces: 2-1|
  |
     ignore leading spaces: 2+1|
  |  ignore leading spaces: 2+0|

Normally sorted:
  |
     ignore leading spaces: 2+1|
  |  ignore leading spaces: 2+0|
  | ignore leading spaces: 2-1|
  |ignore leading spaces: 2-2|

Naturally sorted:
  |  ignore leading spaces: 2+0|
  |
     ignore leading spaces: 2+1|
  | ignore leading spaces: 2-1|
  |ignore leading spaces: 2-2|

Test strings:
  |ignore m.a.s spaces: 2-2|
  |ignore m.a.s  spaces: 2-1|
  |ignore m.a.s   spaces: 2+0|
  |ignore m.a.s    spaces: 2+1|

Normally sorted:
  |ignore m.a.s    spaces: 2+1|
  |ignore m.a.s   spaces: 2+0|
  |ignore m.a.s  spaces: 2-1|
  |ignore m.a.s spaces: 2-2|

Naturally sorted:
  |ignore m.a.s   spaces: 2+0|
  |ignore m.a.s    spaces: 2+1|
  |ignore m.a.s  spaces: 2-1|
  |ignore m.a.s spaces: 2-2|

Test strings:
  |Equiv. spaces: 3-3|
spaces: 3-2|
  |Equiv.�spaces: 3-1|
  |Equiv.�spaces: 3+0|
  |Equiv.
spaces: 3+1|
  |Equiv.	spaces: 3+2|

Normally sorted:
  |Equiv.	spaces: 3+2|
  |Equiv.
spaces: 3+1|
  |Equiv.�spaces: 3+0|
  |Equiv.�spaces: 3-1|
spaces: 3-2|
  |Equiv. spaces: 3-3|

Naturally sorted:
  |Equiv.�spaces: 3+0|
  |Equiv.
spaces: 3+1|
  |Equiv.	spaces: 3+2|
  |Equiv.�spaces: 3-1|
spaces: 3-2|
  |Equiv. spaces: 3-3|

Test strings [sic]:
  |cASE INDEPENENT: 3-2|
  |caSE INDEPENENT: 3-1|
  |casE INDEPENENT: 3+0|
  |case INDEPENENT: 3+1|

Normally sorted [sic]:
  |cASE INDEPENENT: 3-2|
  |caSE INDEPENENT: 3-1|
  |casE INDEPENENT: 3+0|
  |case INDEPENENT: 3+1|

Naturally sorted [sic]:
  |casE INDEPENENT: 3+0|
  |case INDEPENENT: 3+1|
  |caSE INDEPENENT: 3-1|
  |cASE INDEPENENT: 3-2|

Test strings:
  |foo100bar99baz0.txt|
  |foo100bar10baz0.txt|
  |foo1000bar99baz10.txt|
  |foo1000bar99baz9.txt|

Normally sorted:
  |foo1000bar99baz10.txt|
  |foo1000bar99baz9.txt|
  |foo100bar10baz0.txt|
  |foo100bar99baz0.txt|

Naturally sorted:
  |foo1000bar99baz10.txt|
  |foo1000bar99baz9.txt|
  |foo100bar10baz0.txt|
  |foo100bar99baz0.txt|

Test strings:
  |The Wind in the Willows|
  |The 40th step more|
  |The 39 steps|
  |Wanda|

Normally sorted:
  |The 39 steps|
  |The 40th step more|
  |The Wind in the Willows|
  |Wanda|

Naturally sorted:
  |The 39 steps|
  |The 40th step more|
  |The Wind in the Willows|
  |Wanda|

```



## Elixir

Implements requests 1-5.

```elixir
defmodule Natural do
  def sorting(texts) do
    Enum.sort_by(texts, fn text -> compare_value(text) end)
  end

  defp compare_value(text) do
    text
    |> String.downcase
    |> String.replace(~r/\A(a |an |the )/, "")
    |> String.split
    |> Enum.map(fn word ->
         Regex.scan(~r/\d+|\D+/, word)
         |> Enum.map(fn [part] ->
              case Integer.parse(part) do
                {num, ""} -> num
                _         -> part
              end
            end)
       end)
  end

  def task(title, input) do
    IO.puts "\n#{title}:"
    IO.puts "< input >"
    Enum.each(input, &IO.inspect &1)
    IO.puts "< normal sort >"
    Enum.sort(input) |> Enum.each(&IO.inspect &1)
    IO.puts "< natural sort >"
    Enum.sort_by(input, &compare_value &1) |> Enum.each(&IO.inspect &1)
  end
end

[{"Ignoring leading spaces",
  ["ignore leading spaces: 2-2",   " ignore leading spaces: 2-1",
   "  ignore leading spaces: 2+0", "   ignore leading spaces: 2+1"]},

 {"Ignoring multiple adjacent spaces (m.a.s)",
  ["ignore m.a.s spaces: 2-2",   "ignore m.a.s  spaces: 2-1",
   "ignore m.a.s   spaces: 2+0", "ignore m.a.s    spaces: 2+1"]},

 {"Equivalent whitespace characters",
  ["Equiv. spaces: 3-3",    "Equiv.\rspaces: 3-2", "Equiv.\x0cspaces: 3-1",
   "Equiv.\x0bspaces: 3+0", "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2"]},

 {"Case Indepenent sort",
  ["cASE INDEPENENT: 3-2", "caSE INDEPENENT: 3-1",
   "casE INDEPENENT: 3+0", "case INDEPENENT: 3+1"]},

 {"Numeric fields as numerics",
  ["foo100bar99baz0.txt",   "foo100bar10baz0.txt",
   "foo1000bar99baz10.txt", "foo1000bar99baz9.txt"]},

 {"Title sorts",
  ["The Wind in the Willows", "The 40th step more", "The 39 steps", "Wanda"]}
]
|> Enum.each(fn {title, input} -> Natural.task(title, input) end)
```


<pre style="height: 80ex; overflow: scroll">
Ignoring leading spaces:
< input >
"ignore leading spaces: 2-2"
" ignore leading spaces: 2-1"
"  ignore leading spaces: 2+0"
"   ignore leading spaces: 2+1"
< normal sort >
"   ignore leading spaces: 2+1"
"  ignore leading spaces: 2+0"
" ignore leading spaces: 2-1"
"ignore leading spaces: 2-2"
< natural sort >
"  ignore leading spaces: 2+0"
"   ignore leading spaces: 2+1"
" ignore leading spaces: 2-1"
"ignore leading spaces: 2-2"

Ignoring multiple adjacent spaces (m.a.s):
< input >
"ignore m.a.s spaces: 2-2"
"ignore m.a.s  spaces: 2-1"
"ignore m.a.s   spaces: 2+0"
"ignore m.a.s    spaces: 2+1"
< normal sort >
"ignore m.a.s    spaces: 2+1"
"ignore m.a.s   spaces: 2+0"
"ignore m.a.s  spaces: 2-1"
"ignore m.a.s spaces: 2-2"
< natural sort >
"ignore m.a.s   spaces: 2+0"
"ignore m.a.s    spaces: 2+1"
"ignore m.a.s  spaces: 2-1"
"ignore m.a.s spaces: 2-2"

Equivalent whitespace characters:
< input >
"Equiv. spaces: 3-3"
"Equiv.\rspaces: 3-2"
"Equiv.\fspaces: 3-1"
"Equiv.\vspaces: 3+0"
"Equiv.\nspaces: 3+1"
"Equiv.\tspaces: 3+2"
< normal sort >
"Equiv.\tspaces: 3+2"
"Equiv.\nspaces: 3+1"
"Equiv.\vspaces: 3+0"
"Equiv.\fspaces: 3-1"
"Equiv.\rspaces: 3-2"
"Equiv. spaces: 3-3"
< natural sort >
"Equiv.\vspaces: 3+0"
"Equiv.\nspaces: 3+1"
"Equiv.\tspaces: 3+2"
"Equiv.\fspaces: 3-1"
"Equiv.\rspaces: 3-2"
"Equiv. spaces: 3-3"

Case Indepenent sort:
< input >
"cASE INDEPENENT: 3-2"
"caSE INDEPENENT: 3-1"
"casE INDEPENENT: 3+0"
"case INDEPENENT: 3+1"
< normal sort >
"cASE INDEPENENT: 3-2"
"caSE INDEPENENT: 3-1"
"casE INDEPENENT: 3+0"
"case INDEPENENT: 3+1"
< natural sort >
"casE INDEPENENT: 3+0"
"case INDEPENENT: 3+1"
"caSE INDEPENENT: 3-1"
"cASE INDEPENENT: 3-2"

Numeric fields as numerics:
< input >
"foo100bar99baz0.txt"
"foo100bar10baz0.txt"
"foo1000bar99baz10.txt"
"foo1000bar99baz9.txt"
< normal sort >
"foo1000bar99baz10.txt"
"foo1000bar99baz9.txt"
"foo100bar10baz0.txt"
"foo100bar99baz0.txt"
< natural sort >
"foo100bar10baz0.txt"
"foo100bar99baz0.txt"
"foo1000bar99baz9.txt"
"foo1000bar99baz10.txt"

Title sorts:
< input >
"The Wind in the Willows"
"The 40th step more"
"The 39 steps"
"Wanda"
< normal sort >
"The 39 steps"
"The 40th step more"
"The Wind in the Willows"
"Wanda"
< natural sort >
"The 39 steps"
"The 40th step more"
"Wanda"
"The Wind in the Willows"

```



## Fortran


### Via a complex auxiliary sort key

The standard way of supporting ordering rules that become complex is to prepare an auxiliary sort key that can be sorted according to a simple rule. Thus, telephone directories often convert McLean into MacLean, etc. so that no-one need agonise over Mac/Mc. Happily, no normal English word starts "mc". For this problem, numbers provide a further difficulty. One could for example arrange that all digit sequences be expanded into say six-digit sequences with leading zeroes. Thus "10" would become "000010" - except, why only six digits? File names often incorporate dates, as in 20150601, and so on. By absorbing integers into a 32-bit integer, large numbers can be handled and if necessary, 64-bit integers could be used.

The plan is to convert the texts into a series of <text><integer> pairs, carefully not incorporating signs into the numbers and also not periods, thanks to experiences with version numbers such as 3.14.15. Alas, a hyphen character follows a plus sign - swapping them in the auxiliary key would solve that. As it proceeds, the conversion process can ignore leading spaces and multiple included spaces, and convert all letters to uppercase. As a result, for this test many of the entries have identical text parts, and these are not stashed multiple times. Subroutine LIBRARIAN recognises leading articles such as "A", "An", and "The" and moves them to the end of the text in the approved manner. This really should be a part of a catalogue's subject classification system, as really, it is removing non-significant words from the start of a title so that titles might be ordered by subject, somewhat. For "The 39 steps", the ", The" is not appended to the end of the entry, because the entry's first part pair is ended by the digits, however for the example texts, it works...

Objectives one to five are attained, presuming that "whitespace" character codes all precede a space, including the NUL character. The test caused trouble, because on output the special characters ''are'' acted upon, thereby wrecking the layout. Accordingly, a variant output routine converts such characters into the irritating "backslash-mnemonic" scheme, thereby only damaging the layout. In further vexation, the source highlighter here has difficulty in maintaining its context, so the source is presented with CALL PUT("!") instead of the actual backslash in the actual source file! The contents of a text literal should be ... literal.

Objectives six to eight are attainable, except that the character encodements available are not portable. Code page 850 doesn't offer the same accented character codes as for other systems, such as code page 437. But the auxiliary sort key approach easily accommodates substitute characters (and could also swap + and -, for example!), and could recognise ligatures as well. One might be prodded into escalating to 16-bit or even 32-bit character codes to maintain the same ease of manipulation.

```Fortran

      MODULE STASHTEXTS		!Using COMMON is rather more tedious.
       INTEGER MSG,KBD			!I/O unit numbers.
       DATA MSG,KBD/6,5/		!Output, input.

       INTEGER LSTASH,NSTASH,MSTASH	!Prepare a common text stash.
       PARAMETER (LSTASH = 2468, MSTASH = 234)	!LSTASH characters for MSTASH texts.
       INTEGER ISTASH(MSTASH + 1)	!Index to start positions.
       CHARACTER*(LSTASH) STASH		!One pool.
       DATA NSTASH,ISTASH(1)/0,1/	!Which is empty.
       CONTAINS
        SUBROUTINE CROAK(GASP)	!A dying remark.
         CHARACTER*(*) GASP	!The last words.
         WRITE (MSG,*) "Oh dear."	!Shock.
         WRITE (MSG,*) GASP		!Aargh!
         STOP "How sad."		!Farewell, cruel world.
        END SUBROUTINE CROAK	!Farewell...

        SUBROUTINE UPCASE(TEXT)	!In the absence of an intrinsic...
Converts any lower case letters in TEXT to upper case...
Concocted yet again by R.N.McLean (whom God preserve) December MM.
Converting from a DO loop evades having both an iteration counter to decrement and an index variable to adjust.
         CHARACTER*(*) TEXT	!The stuff to be modified.
c         CHARACTER*26 LOWER,UPPER	!Tables. a-z may not be contiguous codes.
c         PARAMETER (LOWER = "abcdefghijklmnopqrstuvwxyz")
c         PARAMETER (UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
CAREFUL!! The below relies on a-z and A-Z being contiguous, as is NOT the case with EBCDIC.
         INTEGER I,L,IT	!Fingers.
          L = LEN(TEXT)		!Get a local value, in case LEN engages in oddities.
          I = L			!Start at the end and work back..
    1     IF (I.LE.0) RETURN 	!Are we there yet? Comparison against zero should not require a subtraction.
c         IT = INDEX(LOWER,TEXT(I:I))	!Well?
c         IF (IT .GT. 0) TEXT(I:I) = UPPER(IT:IT)	!One to convert?
          IT = ICHAR(TEXT(I:I)) - ICHAR("a")		!More symbols precede "a" than "A".
          IF (IT.GE.0 .AND. IT.LE.25) TEXT(I:I) = CHAR(IT + ICHAR("A"))	!In a-z? Convert!
          I = I - 1		!Back one.
          GO TO 1		!Inspect..
        END SUBROUTINE UPCASE	!Easy.

        SUBROUTINE SHOWSTASH(BLAH,I)	!One might be wondering.
         CHARACTER*(*) BLAH		!An annotation.
         INTEGER I			!The desired stashed text.
          IF (I.LE.0 .OR. I.GT.NSTASH) THEN	!Paranoia rules.
            WRITE (MSG,1) BLAH,I		!And is not always paranoid.
    1       FORMAT (A,': Text(',I0,') is not in the stash!')	!Hopefully, helpful.
           ELSE		!But surely I will only be asked for what I have.
            WRITE (MSG,2) BLAH,I,STASH(ISTASH(I):ISTASH(I + 1) - 1)	!Whee!
    2       FORMAT (A,': Text(',I0,')=>',A,'<')	!Hopefully, informative.
          END IF			!So, it is shown.
        END SUBROUTINE SHOWSTASH	!Ah, debugging.

       INTEGER FUNCTION STASHIN(L2)	!Assimilate the text ending at L2.
Careful: furrytran regards "blah" and "blah   " as equal, so, compare lengths first.
        INTEGER L2	!The text to add is at ISTASH(NSTASH + 1):L2.
        INTEGER I,L1	!Assistants.
         L1 = ISTASH(NSTASH + 1)!Where the scratchpad starts.
         L = L2 - L1 + 1	!The length of the text.
Check to see if I already have stashed this exact text.
         DO I = 1,NSTASH	!Search my existing texts.
           IF (L.EQ.ISTASH(I + 1) - ISTASH(I)) THEN	!Matching lengths?
             IF (STASH(L1:L2)				!Yes. Does the scratchpad
     1       .EQ.STASH(ISTASH(I):ISTASH(I + 1) - 1)) THEN	!Match the stashed text?
               STASHIN = I		!Yes! I already have this exact text.
               RETURN			!And there is no need to duplicate it.
             END IF		!So much for matching text, furrytran style.
           END IF		!This time, trailing space differences will count.
         END DO			!On to the next stashed text.
Can't find it. Assimilate the scratchpad. No text is moved, just extend the fingers.
         IF (NSTASH.GE.MSTASH) CALL CROAK("The text pool is crowded!")	!Alas.
         IF (L2.GT.LSTASH) CALL CROAK("Overtexted!")	!Alack.
         NSTASH = NSTASH + 1		!Count in another entry.
         ISTASH(NSTASH + 1) = L2 + 1	!The new "first available" position.
         STASHIN = NSTASH		!Fingered for the caller.
       END FUNCTION STASHIN	!Rather than assimilating a supplied text.
      END MODULE STASHTEXTS	!Others can extract text as they wish.

      MODULE BADCHARACTER	!Some characters are not for glyphs but for action.
       CHARACTER*1 BS,HT,LF,VT,FF,CR	!Nicknames for a bunch of troublemakers.
       CHARACTER*6 BADC,GOODC		!I want a system.
       INTEGER*1 IBADC(6)		!Initialisation syntax is restricive.
       PARAMETER (GOODC="btnvfr")	!Mnemonics.
       EQUIVALENCE (BADC(1:1),BS),(BADC(2:2),HT),(BADC(3:3),LF),!Match the names
     1  (BADC(4:4),VT),(BADC(5:5),FF),(BADC(6:6),CR),	!To their character.
     2  (IBADC,BADC)				!Alas, a PARAMETER style is rejected.
       DATA IBADC/8,9,10,11,12,13/	!ASCII encodements.
       PRIVATE IBADC			!Keep this quiet.
      END MODULE BADCHARACTER	!They can disrupt layout.

      MODULE COMPOUND	!Stores entries, each of multiple parts, each part a text and a number.
       USE STASHTEXTS		!Gain access to the text repository.
       INTEGER LENTRY,NENTRY,MENTRY	!Entry counting.
       PARAMETER (MENTRY = 28)		!Should be enough for the test runs.
       INTEGER TENTRY(MENTRY)		!Each entry has a source text somewhere in STASH.
       INTEGER IENTRY(MENTRY + 1)	!This fingers its first part in PARTT and PARTI.
       INTEGER MPART,NPART		!Now for the pool of parts.
       PARAMETER (MPART = 120)		!Should suffice.
       INTEGER PARTT(MPART)		!A part's text number in STASH.
       INTEGER PARTI(MPART)		!A part's number, itself.
       DATA NENTRY,NPART,IENTRY(1)/0,0,1/	!There are no entries, with no parts either.
       CONTAINS		!The fun begins.
       INTEGER FUNCTION ADDENTRY(X)	!Create an entry holding X.
Chops X into many parts, alternating <text><integer>,<text><integer>,...
Converts the pieces' texts to upper case, as they will be used as a sort key later.
        CHARACTER*(*) X		!The text.
        INTEGER BORED,GRIST,NUMERIC	!Might as well supply some mnemonics.
        PARAMETER (BORED = 0, GRIST = 1, NUMERIC = 2)	!For nearly arbitrary integers.
        INTEGER I,STATE,D	!For traipsing through the text.
        INTEGER L1,L2		!Bounds of the scratchpad in STASH.
        CHARACTER*1 C		!Save on some typing.
Create a new entry. First, save its source text exactly as supplied.
         IF (NENTRY.GE.MENTRY) CALL CROAK("Too many entries!")	!Perhaps I can't.
         NENTRY = NENTRY + 1		!Another entry.
         L2 = ISTASH(NSTASH + 1) - 1	!Find my scratchpad.
         STASH(L2 + 1:L2 + LEN(X)) = X	!Place the text as it stands.
         TENTRY(NENTRY) = STASHIN(L2 + LEN(X))	!Find a finger to it in my text stash.
         CALL SHOWSTASH("Entering",TENTRY(NENTRY))	!Ah, debugging.
         ADDENTRY = NENTRY		!I shall return this.
Contemplate the text of the entry. Leading spaces, multiple spaces, numeric portions...
         STATE = BORED			!As if in leading space stuff.
         L2 = ISTASH(NSTASH + 1) - 1	!Syncopation for text piece placement.
         N = 0				!A number may be encountered.
         DO I = 1,LEN(X)	!Step through the text.
           C = X(I:I)		!Grab a character.
           IF (C.LE." ") THEN	!A space, or somesuch.
             SELECT CASE(STATE)	!What were we doing?
              CASE(BORED)	!Ignoring spaces.
              				!Do nothing with this one too.
              CASE(GRIST)	!We were in stuff.
               CALL ONESPACE		!So accept one space only.
              CASE(NUMERIC)	!We were in a number.
               CALL ADDPART		!So, the number has been ended.
               STATE = BORED		!But the space wot did it is ignored.
              CASE DEFAULT	!This should never happen.
               CALL CROAK("Confused state!")	!So this shouldn't.
             END SELECT		!So much for encountering spaceish stuff.
           ELSE IF ("0".LE.C .AND. C.LE."9") THEN	!A digit?
             D = ICHAR(C) - ICHAR("0")	!Yes. Convert to a numerical digit.
             N = N*10 + D		!Assimilate into a proper number.
             STATE = NUMERIC		!Perhaps more digits follow.
           ELSE		!All other characters are accepted as they stand.
             IF (STATE.EQ.NUMERIC) CALL ADDPART	!A number has just ended.
             L2 = L2 + 1		!Starting a new pair's text.
             STASH(L2:L2) = C	!With this.
             STATE = GRIST	!And anticipating more to come.
           END IF		!Types are: spaceish, grist, digits.
         END DO			!On to the next character.
         CALL ADDPART		!Ended by the end-of-text.
         IENTRY(NENTRY + 1) = NPART + 1	!Thus be able to find an entry's last part.
        CONTAINS	!Odd assistants.
         SUBROUTINE ONESPACE	!Places a space, then declares BORED.
           L2 = L2 + 1		!Advance one.
           STASH(L2:L2) = " "	!An actual blank.
           STATE = BORED	!Any subsequent spaces are to be ignored.
         END SUBROUTINE ONESPACE!Skipping them.
         SUBROUTINE ADDPART	!Augment the paired PARTT and PARTI.
           IF (NPART.GE.MPART) CALL CROAK("Too many parts!")	!If space remains.
           NPART = NPART + 1		!So, another part.
           IF (STASH(L2:L2).EQ." ") L2 = L2 - 1	!A trailing space trimmed. BORED means at most only one.
           L1 = ISTASH(NSTASH + 1)	!My scratchpad starts after the last stashed text.
           CALL UPCASE(STASH(L1:L2))	!Simplify the text to be a sort key part.
           IF (IENTRY(NENTRY).EQ.NPART) CALL LIBRARIAN	!The first part of an entry?
           PARTT(NPART) = STASHIN(L2)	!Finger the text part.
           PARTI(NPART) = N		!Save the numerical value.
           L2 = ISTASH(NSTASH + 1) - 1	!The text may not have been a newcomer.
           N = 0			!Ready for another number.
         END SUBROUTINE ADDPART	!Always paired, even if no number was found.
         SUBROUTINE LIBRARIAN	!Adjusts names starting "The ..." or "An ..." or "A ...", library style.
          CHARACTER*4 ARTICLE(3)	!By chance, three, by happy chance, lengths 1, 2, 3!
          PARAMETER (ARTICLE = (/"A","AN","THE"/))	!These each have trailing space.
          INTEGER I		!A stepper.
           DO I = 1,3		!So step through the known articles.
             IF (L1 + I.GT.L2) RETURN	!Insufficient text? Give up.
             IF (STASH(L1:L1 + I).EQ.ARTICLE(I)(1:I + 1)) THEN	!Starts with this one?
               STASH(L1:L2 - I - 1) = STASH(L1 + I + 1:L2)	!Yes! Shift the rest back over it.
               STASH(L2 - I:L2 + 1) = ", "//ARTICLE(I)(1:I)	!Place the article at the end.
               L2 = L2 + 1				!One more, for the comma.
               RETURN				!Done!
             END IF			!But if that article didn't match,
           END DO			!Try the next.
         END SUBROUTINE LIBRARIAN	!Ah, catalogue order. Blah, The.
       END FUNCTION ADDENTRY	!That was fun!

       SUBROUTINE SHOWENTRY(BLAH,E)	!Ah, debugging.
        CHARACTER*(*) BLAH	!With distinguishing mark.
        INTEGER E,P		!Entry and part fingering.
        INTEGER L1,L2		!Fingers.
         L1 = ISTASH(TENTRY(E))		!The source text is stashed as text #TENTRY(E).
         L2 = ISTASH(TENTRY(E) + 1) - 1	!ISTASH(i) is where in STASH text #i starts.
         WRITE (MSG,1) BLAH,E,IENTRY(E),IENTRY(E + 1) - 1,STASH(L1:L2)
    1    FORMAT (/,A," Entry(",I0,")=Pt ",I0," to ",I0,", text >",A,"<")
         DO P = IENTRY(E),IENTRY(E + 1) - 1	!Step through the part list.
           L1 = ISTASH(PARTT(P))		!Find the text of the part.
           L2 = ISTASH(PARTT(P) + 1) - 1	!Saved in STASH.
           WRITE (MSG,2) P,PARTT(P),PARTI(P),STASH(L1:L2)	!The text is of variable length,
    2      FORMAT ("Part(",I0,") = text#",I0,", N = ",I0," >",A,"<")	!So present it *after* the number.
         END DO					!On to the next part.
       END SUBROUTINE SHOWENTRY		!Shows entry = <text><number>, <text><number>, ...

       INTEGER FUNCTION ENTRYORDER(E1,E2)	!Report on the order of entries E1 and E2.
Chug through the parts list of the two entries, for each part comparing the text, then the number.
        INTEGER E1,E2		!Finger entries via TENTRY(i) and IENTRY(i)...
        INTEGER T1,T2		!Fingers texts in STASH.
        INTEGER I1,N1,I2,N2	!Fingers and counts.
        INTEGER I,D		!A stepper and a difference.
c         CALL SHOWENTRY("E1",E1)
c         CALL SHOWENTRY("E2",E2)
         P1 = IENTRY(E1)		!Finger the first parts
         P2 = IENTRY(E2)		!Of the two entries.
Compare the text part of the two parts.
   10    T1 = PARTT(P1)		!So, what is the number of the text,
         T2 = PARTT(P2)		!Safely stored in STASH.
         IF (T1.NE.T2) THEN	!Inspect text only if the text parts differ.
           I1 = ISTASH(T1)		!Where its text is stashed.
           N1 = ISTASH(T1 + 1) - I1	!Thus the length of that text.
           I2 = ISTASH(T2)		!First character of the other text.
           N2 = ISTASH(T2 + 1) - I2	!Thus its length.
           DO I = 1,MIN(N1,N2)	!Step along both texts while they have characters to match.
             D = ICHAR(STASH(I2:I2)) - ICHAR(STASH(I1:I1))	!The difference.
             IF (D.NE.0) GO TO 666	!Is there a difference?
             I1 = I1 + 1		!No.
             I2 = I2 + 1		!Advance to the next character for both.
           END DO		!And try again.
Can't compare character pairs beyond the shorter of the two texts.
           D = N2 - N1			!Very well, which text is the shorter?
           IF (D.NE.0) GO TO 666	!No difference in length?
         END IF			!So much for the text comparison.
Compare the numeric part.
         D = PARTI(P2) - PARTI(P1)	!Righto, compare the numeric side.
         IF (D.NE.0) GO TO 666		!A difference here?
Can't find any difference between those two parts.
         P1 = P1 + 1			!Move on to the next part.
         P2 = P2 + 1			!For both entries.
         N1 = IENTRY(E1 + 1) - P1	!Knowing where the next entry's parts start
         N2 = IENTRY(E2 + 1) - P2	!Means knowing where an entry's parts end.
         IF (N1.GT.0 .AND. N2.GT.0) GO TO 10	!At least one for both, so compare the next pair.
         D = N2 - N1			!Thus, the shorter precedes the longer.
Conclusion.
  666    ENTRYORDER = D			!Zero sez "equal".
       END FUNCTION ENTRYORDER	!That was a struggle.

       SUBROUTINE ORDERENTRY(LIST,N)
Crank up a Comb sort of the entries fingered by LIST. Working backwards, just for fun.
Caution: the H*10/13 means that H ought not be INTEGER*2. Otherwise, use H/1.3.
        INTEGER LIST(*)	!This is an index to the items being compared.
        INTEGER T	!In the absence of a SWAP(a,b). Same type as LIST.
        INTEGER N	!The number of entries.
        INTEGER I,H	!Tools. H ought not be a small integer.
        LOGICAL CURSE	!Annoyance.
         H = N - 1		!Last - First, and not +1.
         IF (H.LE.0) RETURN	!Ha ha.
    1    H = MAX(1,H*10/13)	!The special feature.
         IF (H.EQ.9 .OR. H.EQ.10) H = 11	!A twiddle.
         CURSE = .FALSE.	!So far, so good.
         DO I = N - H,1,-1	!If H = 1, this is a BubbleSort.
           IF (ENTRYORDER(LIST(I),LIST(I + H)).LT.0) THEN	!One compare.
             T=LIST(I); LIST(I)=LIST(I+H); LIST(I+H)=T	!One swap.
             CURSE = .TRUE.			!One curse.
           END IF			!One test.
         END DO			!One loop.
         IF (CURSE .OR. H.GT.1) GO TO 1	!Work remains?
       END SUBROUTINE ORDERENTRY

       CHARACTER*44 FUNCTION ENTRYTEXT(E)	!Ad-hoc extraction of an entry's source text.
        INTEGER E	!The desired entry's number.
        INTEGER P	!A stage in the dereferencing.
         P = TENTRY(E)	!Entry E's source text is #P.
         ENTRYTEXT = STASH(ISTASH(P):ISTASH(P + 1) - 1)	!Stashed here.
       END FUNCTION ENTRYTEXT	!Fixed size only, with trailing spaces.

       CHARACTER*44 FUNCTION ENTRYTEXTCHAR(E)	!The same, but with nasty characters defanged.
        USE BADCHARACTER	!Just so.
        INTEGER E		!The desired entry's number.
        INTEGER P		!A stage in the dereferencing.
        CHARACTER*44 TEXT	!A scratchpad, to avoid confusing the compiler.
        INTEGER I,L,H		!Fingers.
        CHARACTER*1 C		!A waystation.
         L = 0			!No text has been extracted.
         P = TENTRY(E)		!Entry E's source text is #P.
         DO I = ISTASH(P),ISTASH(P + 1) - 1	!Step along the stash..
           C = STASH(I:I)	!Grab a character.
           H = INDEX(BADC,C)	!Scan the shit list.
           IF (H.LE.0) THEN	!One of the troublemakers?
             CALL PUT(C)		!No. Just copy it.
            ELSE		!Otherwise,
             CALL PUT("!")		!Place a context changer.
             CALL PUT(GOODC(H:H))	!Place the corresponding mnemonic.
           END IF		!So much for that character.
         END DO			!On to the next.
         ENTRYTEXTCHAR = TEXT(1:MIN(L,44))	!Protect against overflow.
        CONTAINS		!A trivial assistant.
         SUBROUTINE PUT(C)	!But too messy to have in-line.
          CHARACTER*1 C		!The character of the moment.
           L = L + 1			!Advance to place it.
           IF (L.LE.44) TEXT(L:L) = C	!If within range.
         END SUBROUTINE PUT	!Simple enough.
       END FUNCTION ENTRYTEXTCHAR	!On output, the troublemakers make trouble.

       SUBROUTINE ORDERENTRYTEXT(LIST,N)
Crank up a Comb sort of the entries fingered by LIST. Working backwards, just for fun.
Caution: the H*10/13 means that H ought not be INTEGER*2. Otherwise, use H/1.3.
        INTEGER LIST(*)	!This is an index to the items being compared.
        INTEGER T	!In the absence of a SWAP(a,b). Same type as LIST.
        INTEGER N	!The number of entries.
        INTEGER I,H	!Tools. H ought not be a small integer.
        LOGICAL CURSE	!Annoyance.
         H = N - 1		!Last - First, and not +1.
         IF (H.LE.0) RETURN	!Ha ha.
    1    H = MAX(1,H*10/13)	!The special feature.
         IF (H.EQ.9 .OR. H.EQ.10) H = 11	!A twiddle.
         CURSE = .FALSE.	!So far, so good.
         DO I = N - H,1,-1	!If H = 1, this is a BubbleSort.
           IF (ENTRYTEXT(LIST(I)).GT.ENTRYTEXT(LIST(I+H))) THEN	!One compare.
             T=LIST(I); LIST(I)=LIST(I+H); LIST(I+H)=T	!One swap.
             CURSE = .TRUE.			!One curse.
           END IF			!One test.
         END DO			!One loop.
         IF (CURSE .OR. H.GT.1) GO TO 1	!Work remains?
       END SUBROUTINE ORDERENTRYTEXT
      END MODULE COMPOUND	!Accepts, stores, lists and sorts the content.

      PROGRAM MR NATURAL	!Presents a list in sorted order.
      USE COMPOUND		!Stores text in a complicated way.
      USE BADCHARACTER		!Some characters wreck the layout.
      INTEGER I,ITEM(30),PLAIN(30)	!Two sets of indices.
      I = 0	!An array must have equal-length items, so trailing spaces would result.
      I=I+1;ITEM(I) = ADDENTRY("ignore leading spaces: 2-2")
      I=I+1;ITEM(I) = ADDENTRY(" ignore leading spaces: 2-1")
      I=I+1;ITEM(I) = ADDENTRY("  ignore leading spaces: 2+0")
      I=I+1;ITEM(I) = ADDENTRY("   ignore leading spaces: 2+1")
      I=I+1;ITEM(I) = ADDENTRY("ignore m.a.s spaces: 2-2")
      I=I+1;ITEM(I) = ADDENTRY("ignore m.a.s  spaces: 2-1")
      I=I+1;ITEM(I) = ADDENTRY("ignore m.a.s   spaces: 2+0")
      I=I+1;ITEM(I) = ADDENTRY("ignore m.a.s    spaces: 2+1")
      I=I+1;ITEM(I) = ADDENTRY("Equiv."//" "//"spaces: 3-3")
      I=I+1;ITEM(I) = ADDENTRY("Equiv."//CR//"spaces: 3-2")	!CR can't appear as itself.
      I=I+1;ITEM(I) = ADDENTRY("Equiv."//FF//"spaces: 3-1")	!As it is used to mark line endings.
      I=I+1;ITEM(I) = ADDENTRY("Equiv."//VT//"spaces: 3+0")	!And if typed in an editor,
      I=I+1;ITEM(I) = ADDENTRY("Equiv."//LF//"spaces: 3+1")	!It is acted upon there and then.
      I=I+1;ITEM(I) = ADDENTRY("Equiv."//HT//"spaces: 3+2")	!So, name instead of value.
      I=I+1;ITEM(I) = ADDENTRY("cASE INDEPENDENT: 3-2")
      I=I+1;ITEM(I) = ADDENTRY("caSE INDEPENDENT: 3-1")
      I=I+1;ITEM(I) = ADDENTRY("casE INDEPENDENT: 3+0")
      I=I+1;ITEM(I) = ADDENTRY("case INDEPENDENT: 3+1")
      I=I+1;ITEM(I) = ADDENTRY("foo100bar99baz0.txt")
      I=I+1;ITEM(I) = ADDENTRY("foo100bar10baz0.txt")
      I=I+1;ITEM(I) = ADDENTRY("foo1000bar99baz10.txt")
      I=I+1;ITEM(I) = ADDENTRY("foo1000bar99baz9.txt")
      I=I+1;ITEM(I) = ADDENTRY("The Wind in the Willows")
      I=I+1;ITEM(I) = ADDENTRY("The 40th step more")
      I=I+1;ITEM(I) = ADDENTRY("The 39 steps")
      I=I+1;ITEM(I) = ADDENTRY("Wanda")
c      I=I+1;ITEM(I) = ADDENTRY("A Dinosaur Grunts: Fortran Emerges")
c      I=I+1;ITEM(I) = ADDENTRY("The Joy of Text Twiddling with Fortran")
c      I=I+1;ITEM(I) = ADDENTRY("An Aversion to Unused Trailing Spaces")
      WRITE (MSG,*) "nEntry=",NENTRY		!Reach into the compound storage area.
      PLAIN = ITEM				!Copy the list of entries.
      CALL ORDERENTRY(ITEM,NENTRY)		!"Natural" order.
      CALL ORDERENTRYTEXT(PLAIN,NENTRY)		!Plain text order.
      WRITE (MSG,1) "Character","'Natural'"	!Provide a heading.
    1 FORMAT (2("Entry|Text ",A9," Order",24X))	!Usual trickery.
      DO I = 1,NENTRY				!Step through the lot.
        WRITE (MSG,2) PLAIN(I),ENTRYTEXTCHAR(PLAIN(I)),	!Plain order,
     1                ITEM(I), ENTRYTEXTCHAR(ITEM(I))	!Followed by natural order.
    2   FORMAT (2(I5,"|",A44))			!This follows function ENTRYTEXT.
      END DO					!On to the next.
      END	!A handy hint from Mr. Natural: "At home or at work, get the right tool for the job!"

```

Example output, in two columns:
 Entry|Text Character Order                        Entry|Text 'Natural' Order
     4|   ignore leading spaces: 2+1                  17|casE INDEPENDENT: 3+0
     3|  ignore leading spaces: 2+0                   18|case INDEPENDENT: 3+1
     2| ignore leading spaces: 2-1                    16|caSE INDEPENDENT: 3-1
    14|Equiv.\tspaces: 3+2                            15|cASE INDEPENDENT: 3-2
    13|Equiv.\nspaces: 3+1                            12|Equiv.\vspaces: 3+0
    12|Equiv.\vspaces: 3+0                            13|Equiv.\nspaces: 3+1
    11|Equiv.\fspaces: 3-1                            14|Equiv.\tspaces: 3+2
    10|Equiv.\rspaces: 3-2                            11|Equiv.\fspaces: 3-1
     9|Equiv. spaces: 3-3                             10|Equiv.\rspaces: 3-2
    25|The 39 steps                                    9|Equiv. spaces: 3-3
    24|The 40th step more                             20|foo100bar10baz0.txt
    23|The Wind in the Willows                        19|foo100bar99baz0.txt
    26|Wanda                                          22|foo1000bar99baz9.txt
    15|cASE INDEPENDENT: 3-2                          21|foo1000bar99baz10.txt
    16|caSE INDEPENDENT: 3-1                           3|  ignore leading spaces: 2+0
    17|casE INDEPENDENT: 3+0                           4|   ignore leading spaces: 2+1
    18|case INDEPENDENT: 3+1                           2| ignore leading spaces: 2-1
    21|foo1000bar99baz10.txt                           1|ignore leading spaces: 2-2
    22|foo1000bar99baz9.txt                            7|ignore m.a.s   spaces: 2+0
    20|foo100bar10baz0.txt                             8|ignore m.a.s    spaces: 2+1
    19|foo100bar99baz0.txt                             6|ignore m.a.s  spaces: 2-1
     1|ignore leading spaces: 2-2                      5|ignore m.a.s spaces: 2-2
     8|ignore m.a.s    spaces: 2+1                    25|The 39 steps
     7|ignore m.a.s   spaces: 2+0                     24|The 40th step more
     6|ignore m.a.s  spaces: 2-1                      26|Wanda
     5|ignore m.a.s spaces: 2-2                       23|The Wind in the Willows


### Via a complex comparison procedure.

Here, all previous worry over storage consumption is abandoned. An array of CHARACTER*66 is deemed long enough, and too bad over trailing spaces. An auxiliary array holds the length of each entry. The source texts are left alone, at the cost of having the "natural" compare process engage in a lot of cogitation. The plan for the comparison is to have subroutine ANOTHER yield two characters for comparison, one from each string being compared. Subroutine CRUSH deals with spaceish and character case simplification, and each scan can scoot ahead through whatever multiple spaces it encounters. Subroutine LIBRARIAN advances past any initial spaceish stuff then detects articles at the start of a text and advances the initial scan past them while preparing the appropriate addendum for the end of the text, signified in TAIL, should a scan reach that far.

Each scan maintains a STATE, and if both achieve NUMERIC then both scans are looking at a digit in their respective texts. If so, there is a forwards probe to determine how many digits follow in each text, the results of which drive the interpolation of leading zero characters. That is, suppose the second text has fewer digits (looking at "007" versus "7", say) - then a "0" is created for the second text, its finger is backspaced one and the first text's digit count is decremented. The next time around, the digit counts are known so will not be re-scanned and again a "0" will be generated. The third time, the digit counts will be equal, so both texts will yield up their characters for comparison.

When (if) a scan reaches the end of its text, the TAIL will be considered for the extraction of further characters.


```Fortran

      MODULE ASSISTANCE
       INTEGER MSG,KBD			!I/O unit numbers.
       DATA MSG,KBD/6,5/		!Output, input.
       CONTAINS
        SUBROUTINE CROAK(GASP)	!A dying remark.
         CHARACTER*(*) GASP	!The last words.
         WRITE (MSG,*) "Oh dear."	!Shock.
         WRITE (MSG,*) GASP		!Aargh!
         STOP "How sad."		!Farewell, cruel world.
        END SUBROUTINE CROAK	!Farewell...

        SUBROUTINE UPCASE(TEXT)	!In the absence of an intrinsic...
Converts any lower case letters in TEXT to upper case...
Concocted yet again by R.N.McLean (whom God preserve) December MM.
Converting from a DO loop evades having both an iteration counter to decrement and an index variable to adjust.
         CHARACTER*(*) TEXT	!The stuff to be modified.
c         CHARACTER*26 LOWER,UPPER	!Tables. a-z may not be contiguous codes.
c         PARAMETER (LOWER = "abcdefghijklmnopqrstuvwxyz")
c         PARAMETER (UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
CAREFUL!! The below relies on a-z and A-Z being contiguous, as is NOT the case with EBCDIC.
         INTEGER I,L,IT	!Fingers.
          L = LEN(TEXT)		!Get a local value, in case LEN engages in oddities.
          I = L			!Start at the end and work back..
    1     IF (I.LE.0) RETURN 	!Are we there yet? Comparison against zero should not require a subtraction.
c         IT = INDEX(LOWER,TEXT(I:I))	!Well?
c         IF (IT .GT. 0) TEXT(I:I) = UPPER(IT:IT)	!One to convert?
          IT = ICHAR(TEXT(I:I)) - ICHAR("a")		!More symbols precede "a" than "A".
          IF (IT.GE.0 .AND. IT.LE.25) TEXT(I:I) = CHAR(IT + ICHAR("A"))	!In a-z? Convert!
          I = I - 1		!Back one.
          GO TO 1		!Inspect..
        END SUBROUTINE UPCASE	!Easy.

       INTEGER FUNCTION LSTNB(TEXT)  !Sigh. Last Not Blank.
Concocted yet again by R.N.McLean (whom God preserve) December MM.
Code checking reveals that the Compaq compiler generates a copy of the string and then finds the length of that when using the latter-day intrinsic LEN_TRIM. Madness!
Can't   DO WHILE (L.GT.0 .AND. TEXT(L:L).LE.' ')	!Control chars. regarded as spaces.
Curse the morons who think it good that the compiler MIGHT evaluate logical expressions fully.
Crude GO TO rather than a DO-loop, because compilers use a loop counter as well as updating the index variable.
Comparison runs of GNASH showed a saving of ~3% in its mass-data reading through the avoidance of DO in LSTNB alone.
Crappy code for character comparison of varying lengths is avoided by using ICHAR which is for single characters only.
Checking the indexing of CHARACTER variables for bounds evoked astounding stupidities, such as calculating the length of TEXT(L:L) by subtracting L from L!
Comparison runs of GNASH showed a saving of ~25-30% in its mass data scanning for this, involving all its two-dozen or so single-character comparisons, not just in LSTNB.
        CHARACTER*(*),INTENT(IN):: TEXT	!The bumf. If there must be copy-in, at least there need not be copy back.
        INTEGER L		!The length of the bumf.
         L = LEN(TEXT)		!So, what is it?
    1    IF (L.LE.0) GO TO 2	!Are we there yet?
         IF (ICHAR(TEXT(L:L)).GT.ICHAR(" ")) GO TO 2	!Control chars are regarded as spaces also.
         L = L - 1		!Step back one.
         GO TO 1		!And try again.
    2    LSTNB = L		!The last non-blank, possibly zero.
        RETURN			!Unsafe to use LSTNB as a variable.
       END FUNCTION LSTNB	!Compilers can bungle it.
      END MODULE ASSISTANCE

      MODULE BADCHARACTER	!Some characters are not for glyphs but for action.
       CHARACTER*1 BS,HT,LF,VT,FF,CR	!Nicknames for a bunch of troublemakers.
       CHARACTER*6 BADC,GOODC		!I want a system.
       INTEGER*1 IBADC(6)		!Initialisation syntax is restricive.
       PARAMETER (GOODC="btnvfr")	!Mnemonics.
       EQUIVALENCE (BADC(1:1),BS),(BADC(2:2),HT),(BADC(3:3),LF),!Match the names
     1  (BADC(4:4),VT),(BADC(5:5),FF),(BADC(6:6),CR),	!To their character.
     2  (IBADC,BADC)				!Alas, a PARAMETER style is rejected.
       DATA IBADC/8,9,10,11,12,13/	!ASCII encodements.
       PRIVATE IBADC			!Keep this quiet.
       CONTAINS
        CHARACTER*44 FUNCTION DEFANG(THIS)	!Ad-hoc text conversion with nasty characters defanged.
         CHARACTER*(*) THIS	!The text.
         CHARACTER*44 TEXT	!A scratchpad, to avoid confusing the compiler.
         INTEGER I,L,H		!Fingers.
         CHARACTER*1 C		!A waystation.
          L = 0			!No text has been extracted.
          DO I = 1,LEN(THIS)	!Step along the stash..
            C = THIS(I:I)	!Grab a character.
            H = INDEX(BADC,C)	!Scan the shit list.
            IF (H.LE.0) THEN	!One of the troublemakers?
              CALL PUT(C)		!No. Just copy it.
             ELSE		!Otherwise,
              CALL PUT("!")		!Place a context changer.
              CALL PUT(GOODC(H:H))	!Place the corresponding mnemonic.
            END IF		!So much for that character.
          END DO			!On to the next.
          DEFANG = TEXT(1:MIN(L,44))	!Protect against overflow.
         CONTAINS		!A trivial assistant.
          SUBROUTINE PUT(C)	!But too messy to have in-line.
           CHARACTER*1 C		!The character of the moment.
            L = L + 1			!Advance to place it.
            IF (L.LE.44) TEXT(L:L) = C	!If within range.
          END SUBROUTINE PUT	!Simple enough.
        END FUNCTION DEFANG	!On output, the troublemakers make trouble.
      END MODULE BADCHARACTER	!They can disrupt layout.

      MODULE COMPOUND	!Stuff to store the text entries, and to sort lists.
       USE ASSISTANCE
       INTEGER LENTRY,NENTRY,MENTRY		!Size information.
       PARAMETER (LENTRY = 66, MENTRY = 666)	!Should suffice.
       INTEGER ENTRYLENGTH(MENTRY)		!Lengths for the entries.
       CHARACTER*(LENTRY) ENTRYTEXT(MENTRY)	!Their texts.
       CHARACTER*(LENTRY) ENTRYKEY(MENTRY)	!Comparison keys.
       CONTAINS		!The details.
       INTEGER FUNCTION ADDENTRY(X)	!Create an entry holding X.
        CHARACTER*(*) X		!The text to be stashed.
        INTEGER L		!It may have trailing space stuff.
         L = LSTNB(X)		!Thus, LEN(X) won't do.
         IF (L.GT.LENTRY) CALL CROAK("Over-long text!")	!Even though any trailing spaces have been lost.
         IF (NENTRY.GE.MENTRY) CALL CROAK("Too many entries!")	!Perhaps I can't.
         NENTRY = NENTRY + 1		!Righto, another one.
         ENTRYTEXT(NENTRY)(1:L) = X(1:L)!Place. Trailing spaces will not be supplied.
         ENTRYLENGTH(NENTRY) = L	!But I won't be looking where they won't be.
         ADDENTRY = NENTRY		!The caller needn't keep count.
       END FUNCTION ADDENTRY	!That was simple.

       INTEGER FUNCTION TEXTORDER(E1,E2)	!Compare the texts as they stand.
        INTEGER E1,E2		!Finger the entries holding the texts.
         IF (ENTRYTEXT(E1)(1:ENTRYLENGTH(E1))	!If the text of entry E1
     1   .LT.ENTRYTEXT(E2)(1:ENTRYLENGTH(E2))) THEN	!Precedes that of E2,
          TEXTORDER = +1				!Then the order is good.
         ELSE IF (ENTRYTEXT(E1)(1:ENTRYLENGTH(E1))	!ENTRYLENGTH means no trailing spaces.
     1   .GT.ENTRYTEXT(E2)(1:ENTRYLENGTH(E2))) THEN	!Accordingly, no "x" = "x " accommodation.
          TEXTORDER = -1				!So, reversed order.
         ELSE					!Otherwise,
          TEXTORDER = 0					!They're equal.
         END IF					!So, decided.
       END FUNCTION TEXTORDER		!Thus use the character collation sequence.

       INTEGER FUNCTION NATURALORDER(E1,E2)	!Compares the texts in "natural" order.
        INTEGER E1,E2		!Pity this couldn't be an array of two values.
        CHARACTER*4 ARTICLE(3)	!By chance, three, by happy chance, lengths 1, 2, 3!
        PARAMETER (ARTICLE = (/"A","AN","THE"/))	!These each have trailing space.
        INTEGER DONE,BORED,GRIST,NUMERIC		!Might as well supply some mnemonics.
        PARAMETER (DONE=-1,BORED=0,GRIST=1,NUMERIC=2)	!For nearly arbitrary integers.
        INTEGER WOT(2)		!Collect the two entry numbers.
        INTEGER L(2),LST(2)	!Scan text with finger L, ending with LST.
        INTEGER N		!Counter for comparisons.
        INTEGER DCOUNT(2)	!Counts the number of digits for L(is) onwards.
        INTEGER STATE(2)	!The scans vary in mood.
        INTEGER TAIL(2)		!The LIBRARIAN may discover an ARTICLE and put it in the TAIL.
        INTEGER D		!A difference.
        CHARACTER*1 C(2)	!Character pairs ascertained one-by-one by ANOTHER.
         WOT(1) = E1		!Alright,
         WOT(2) = E2		!Into an array to play.
         L = 0			!Syncopation to start the scan.
         LST = ENTRYLENGTH(WOT)	!End markers.
         STATE = BORED		!So far, and no matter what the librarian discovers.
         DCOUNT = 0		!Nor have any digits been counted.
         CALL LIBRARIAN		!Assess the start of the texts.
         N = 0			!No comparisons so far.
Chug along the texts, character by character.
   10    CALL ANOTHER		!Grab one from each text.
         N = N + 1		!Count another compare.
         ENTRYKEY(WOT)(N:N) = C	!Place the characters being compared.
         D = ICHAR(C(2)) - ICHAR(C(1))		!Their difference.
         IF (D.NE.0) GO TO 666			!A decision yet?
         L = L + 1				!No. Advance both fingers.
         IF (ANY(STATE.NE.DONE)) GO TO 10	!And try again.
  666    NATURALORDER = D	!The decision.
         RETURN		!Despite the lack of an END, this is the end of the function.
        CONTAINS	!Which however contains some assistants, defined after use.
         SUBROUTINE CRUSH(C)	!Reduces annoying variation.
          CHARACTER*1 C		!The victim.
           IF (C.LE." ") THEN	!Spaceish?
             C = " "			!Yes. Standardise.
            ELSE		!For all others,
             CALL UPCASE(C)		!Simplify.
           END IF		!Righto, ready to compare.
         END SUBROUTINE CRUSH	!This should do the deed in place.

         SUBROUTINE ANOTHER	!The entry's text may be followed by an article in the tail.
Claws along the text strings, looking for the next character pair to report for matching.
          INTEGER IS	!Steps through the two texts.
          INTEGER L2	!A second finger, for probing ahead and the TAIL.
          CHARACTER*1 D	!Potentially a digit character.
        EE:DO IS = 1,2		!Dealing with both texts in the same way.
   10        L2 = L(IS) - LST(IS)	!Compare the finger to the end-of-text.
             IF (L2.GT.0) THEN		!Perhaps we have reached the tail.
               IF (TAIL(IS).GT.0 .AND. L2.LE.TAIL(IS)) THEN	!Yes. What about the possible tail?
                 C(IS) = ARTICLE(TAIL(IS))(L2:L2)	!Still wagging.
                ELSE			!But if no tail (or the tail is exhausted)
                 C(IS) = CHAR(0)	!Empty space.
                 STATE(IS) = DONE	!Declare this.
               END IF			!So much for the librarian's tail.
               CYCLE EE			!On to the next text.
             END IF		!But if we have text yet to scan,
             C(IS) = ENTRYTEXT(WOT(IS))(L(IS):L(IS))	!Grab the character.
             CALL CRUSH(C(IS))				!Simplify.
             IF (C(IS).EQ." ") THEN	!So, what have we received?
               IF (STATE(IS).EQ.BORED) THEN	!A space. Are we ignoring them?
                 L(IS) = L(IS) + 1		!Yes. Advance in hope.
                 GO TO 10			!And try again.
               END IF			!So much for another space.
               STATE(IS) = BORED	!If we weren't in spaces, we are now.
             ELSE IF (C(IS).GE."0" .AND. C(IS).LE."9") THEN	!A digit?
               STATE(IS) = NUMERIC		!Double trouble might ensue.
             ELSE			!For all other characters,
               STATE(IS) = GRIST		!We have grist.
             END IF			!So much for the character.
           END DO EE		!On to the next text.
Comparing digit sequences is to be done as numbers. "007" vs "70" is to become vs. "070" by length matching.
           IF (ALL(STATE.EQ.NUMERIC)) THEN	!If we're comparing a digit to a digit,
             IF (ALL(DCOUNT.EQ.0)) THEN		!I want to align the comparison from the right.
            DD:DO IS = 1,2	!So I need to determine how many digits follow in both.
   20            DCOUNT(IS) = DCOUNT(IS) + 1	!Count one more.
                 L2 = L(IS) + DCOUNT(IS)	!Finger the next position.
                 IF (L2.GT.LST(IS)) CYCLE DD	!If we're off the end, we're done.
                 D = ENTRYTEXT(WOT(IS))(L2:L2)	!Otherwise, grab the character.
                 IF (D.LT."0" .OR. D.GT."9") CYCLE DD	!Not a digit: done counting.
                 GO TO 20			!Otherwise, keep on looking.
               END DO DD		!On to the other text.
             END IF		!Righto, I now know how many digits are in each sequence.
Choose the shorter, and notionally insert a leading zero for it to be matched against the longer's digit..
             IF (DCOUNT(1).LT.DCOUNT(2)) THEN	!Righto, if the first has fewer digits,
               DCOUNT(2) = DCOUNT(2) - 1	!Then only the second's digit will be used up.
               L(1) = L(1) - 1			!Step back to re-encounter this next time.
               C(1) = "0"			!And create a leading zero from nothing.
             ELSE IF (DCOUNT(2).LT.DCOUNT(1)) THEN	!Likewise if the other way around.
               DCOUNT(1) = DCOUNT(1) - 1	!The scan will consume this side's digit.
               L(2) = L(2) - 1			!The next time here (if there is one)
               C(2) = "0"			!Will find a reduced difference in length.
             ELSE		!But if both have the same number of digits remaining,
               DCOUNT = DCOUNT - 1	!They are used in parallel.
             END IF		!Perhaps even equal digit remnants.
           END IF		!Thus, arbitrary-size numbers are allowed, as they're never numbers.
         END SUBROUTINE ANOTHER	!Characters are announced in array C, moods in array STATE.

         SUBROUTINE LIBRARIAN	!Looks for texts starting "The ..." or "An ..." or "A ...", library style.
Checks the starts of the two texts, skipping leading spaceish stuff.
          INTEGER IS,A,I	!Steppers.
          CHARACTER*1 C		!A character to mess with.
        EE:DO IS = 1,2		!Two texts to inspect.
             TAIL(IS) = 0		!Nothing special found.
   10        L(IS) = L(IS) + 1		!Advance one.
             IF (L(IS).GT.LST(IS)) CYCLE EE	!Run out of text?
             IF (ENTRYTEXT(WOT(IS))(L(IS):L(IS)).LE." ") GO TO 10	!Scoot through leading space stuff.
          AA:DO A = 1,3			!Now step through the known articles.
               DO I = 0,A			!Character by character thereof, with one trailing space.
                 IF (L(IS) + I.GT.LST(IS)) CYCLE EE	!Have I a character to probe?
                 C = ENTRYTEXT(WOT(IS))(L(IS) + I:L(IS) + I)	!Yes. Grab it.
                 CALL CRUSH(C)					!Simplify.
                 IF (C.NE.ARTICLE(A)(1 + I:1 + I)) CYCLE AA	!Mismatch? Try another.
               END DO				!On to the next character of ARTICLE(A).
               TAIL(IS) = A		!A match!
               L(IS) = L(IS) + I	!Finger the first character after the space.
               CYCLE EE			!Finished with this text. Also, BORED.
             END DO AA			!Try the next article..
           END DO EE			!Try the next text.
         END SUBROUTINE LIBRARIAN	!Ah, catalogue order. Blah, The.
       END FUNCTION NATURALORDER	!Not natural to a computer.

       SUBROUTINE ORDERENTRY(LIST,N,WOTORDER)	!Sorts the list according to the ordering function.
Crank up a Comb sort of the entries fingered by LIST. Working backwards, just for fun.
Caution: the H*10/13 means that H ought not be INTEGER*2. Otherwise, use H/1.3.
        INTEGER LIST(*)		!This is an index to the items being compared.
        INTEGER T		!In the absence of a SWAP(a,b). Same type as LIST.
        INTEGER N		!The number of entries.
        EXTERNAL WOTORDER	!A function to compare two entries.
        INTEGER WOTORDER	!Returns an integer result, on principle.
        INTEGER I,H		!Tools. H ought not be a small integer.
        LOGICAL CURSE		!Annoyance.
         H = N - 1		!Last - First, and not +1.
         IF (H.LE.0) RETURN	!Ha ha.
    1    H = MAX(1,H*10/13)	!The special feature.
         IF (H.EQ.9 .OR. H.EQ.10) H = 11	!A twiddle.
         CURSE = .FALSE.	!So far, so good.
         DO I = N - H,1,-1	!If H = 1, this is a BubbleSort.
           IF (WOTORDER(LIST(I),LIST(I + H)) .LT. 0) THEN	!One compare.
             T=LIST(I); LIST(I)=LIST(I+H); LIST(I+H)=T	!One swap.
             CURSE = .TRUE.			!One curse.
           END IF			!One test.
         END DO			!One loop.
         IF (CURSE .OR. H.GT.1) GO TO 1	!Work remains?
       END SUBROUTINE ORDERENTRY!Fast enough, and simple.
      END MODULE COMPOUND	!Enough.

      PROGRAM MR NATURAL	!Presents a list in sorted order.
      USE ASSISTANCE		!Often needed.
      USE COMPOUND		!Deals with text in a complicated way.
      USE BADCHARACTER		!Some characters wreck the layout.
      INTEGER ITEM(30),FANCY(30)!Two sets of indices.
      INTEGER I,IT,TI		!Assistants.
      I = 0	!An array must have equal-length items, so trailing spaces would result.
      I=I+1;ITEM(I) = ADDENTRY("ignore leading spaces: 2-2")
      I=I+1;ITEM(I) = ADDENTRY(" ignore leading spaces: 2-1")
      I=I+1;ITEM(I) = ADDENTRY("  ignore leading spaces: 2+0")
      I=I+1;ITEM(I) = ADDENTRY("   ignore leading spaces: 2+1")
      I=I+1;ITEM(I) = ADDENTRY("ignore m.a.s spaces: 2-2")
      I=I+1;ITEM(I) = ADDENTRY("ignore m.a.s  spaces: 2-1")
      I=I+1;ITEM(I) = ADDENTRY("ignore m.a.s   spaces: 2+0")
      I=I+1;ITEM(I) = ADDENTRY("ignore m.a.s    spaces: 2+1")
      I=I+1;ITEM(I) = ADDENTRY("Equiv."//" "//"spaces: 3-3")
      I=I+1;ITEM(I) = ADDENTRY("Equiv."//CR//"spaces: 3-2")	!CR can't appear as itself.
      I=I+1;ITEM(I) = ADDENTRY("Equiv."//FF//"spaces: 3-1")	!As it is used to mark line endings.
      I=I+1;ITEM(I) = ADDENTRY("Equiv."//VT//"spaces: 3+0")	!And if typed in an editor,
      I=I+1;ITEM(I) = ADDENTRY("Equiv."//LF//"spaces: 3+1")	!It is acted upon there and then.
      I=I+1;ITEM(I) = ADDENTRY("Equiv."//HT//"spaces: 3+2")	!So, name instead of value.
      I=I+1;ITEM(I) = ADDENTRY("cASE INDEPENDENT: 3-2")
      I=I+1;ITEM(I) = ADDENTRY("caSE INDEPENDENT: 3-1")
      I=I+1;ITEM(I) = ADDENTRY("casE INDEPENDENT: 3+0")
      I=I+1;ITEM(I) = ADDENTRY("case INDEPENDENT: 3+1")
      I=I+1;ITEM(I) = ADDENTRY("foo100bar99baz0.txt")
      I=I+1;ITEM(I) = ADDENTRY("foo100bar10baz0.txt")
      I=I+1;ITEM(I) = ADDENTRY("foo1000bar99baz10.txt")
      I=I+1;ITEM(I) = ADDENTRY("foo1000bar99baz9.txt")
      I=I+1;ITEM(I) = ADDENTRY("The Wind in the Willows")
      I=I+1;ITEM(I) = ADDENTRY("The 40th step more")
      I=I+1;ITEM(I) = ADDENTRY("The 39 steps")
      I=I+1;ITEM(I) = ADDENTRY("Wanda")
c      I=I+1;ITEM(I) = ADDENTRY("A Dinosaur Grunts: Fortran Emerges")
c      I=I+1;ITEM(I) = ADDENTRY("The Joy of Text Twiddling with Fortran")
c      I=I+1;ITEM(I) = ADDENTRY("An Abundance of Storage Enables Waste")
c      I=I+1;ITEM(I) = ADDENTRY("Theory Versus Practice: The Chasm")
      WRITE (MSG,*) "nEntry=",NENTRY	!Reach into the compound storage area.
      FANCY = ITEM			!Copy the list of entries.
      ENTRYKEY = ""			!To be written to by NATURALORDER.
      CALL ORDERENTRY(FANCY,NENTRY,NATURALORDER)	!"Natural" order.
      CALL ORDERENTRY(ITEM,NENTRY,TEXTORDER)		!Plain text order.
      WRITE (MSG,1) "Character","'Natural'","N.Key"	!Provide a heading.
    1 FORMAT (3("Entry|Text ",A9," Order",16X))	!Usual trickery.
      DO I = 1,NENTRY	!Step through the lot.
        IT = ITEM(I)		!Saving on some typing.
        TI = FANCY(I)		!Presenting two lists, line by line.
        WRITE (MSG,2) IT,DEFANG(ENTRYTEXT(IT)(1:ENTRYLENGTH(IT)))	!Plain order,
     1               ,TI,DEFANG(ENTRYTEXT(TI)(1:ENTRYLENGTH(TI)))	!Followed by natural order.
     2               ,TI,ENTRYKEY(TI)	!Already defanged.
    2   FORMAT (3(I5,"|",A36))			!This follows function ENTRYTEXT.
      END DO		!On to the next.
      END	!A handy hint from Mr. Natural: "At home or at work, get the right tool for the job!"

```

This time, because the texts are no longer being parsed into pieces, the book titles are not ordered together though they are in the required sequence disregarding the other entries. "The 39" and "The 40" have the "The " part converted into a TAIL, and so their first comparison characters are their digits, and in ASCII, digits precede letters. This is revealed in the third column, where the comparison characters are revealed, in an ad-hoc manner: what appears are the characters placed by every comparison so there may be contention.
Output:
 Entry|Text Character Order                Entry|Text 'Natural' Order                Entry|Text     N.Key Order
     4|   ignore leading spaces: 2+1          25|The 39 steps                           25|3
     3|  ignore leading spaces: 2+0           24|The 40th step more                     24|4
     2| ignore leading spaces: 2-1            17|casE INDEPENDENT: 3+0                  17|CASE INDEPENDENT: 3+0
    14|Equiv.!tspaces: 3+2                    18|case INDEPENDENT: 3+1                  18|CASE INDEPENDENT: 3+1
    13|Equiv.!nspaces: 3+1                    16|caSE INDEPENDENT: 3-1                  16|CASE INDEPENDENT: 3-1
    12|Equiv.!vspaces: 3+0                    15|cASE INDEPENDENT: 3-2                  15|CASE INDEPENDENT: 3-2
    11|Equiv.!fspaces: 3-1                    12|Equiv.!vspaces: 3+0                    12|EQUIV. SPACES: 3+0
    10|Equiv.!rspaces: 3-2                    13|Equiv.!nspaces: 3+1                    13|EQUIV. SPACES: 3+1
     9|Equiv. spaces: 3-3                     14|Equiv.!tspaces: 3+2                    14|EQUIV. SPACES: 3+2
    25|The 39 steps                           11|Equiv.!fspaces: 3-1                    11|EQUIV. SPACES: 3-1
    24|The 40th step more                     10|Equiv.!rspaces: 3-2                    10|EQUIV. SPACES: 3-2
    23|The Wind in the Willows                 9|Equiv. spaces: 3-3                      9|EQUIV. SPACES: 3-3
    26|Wanda                                  20|foo100bar10baz0.txt                    20|FOO100BAR1
    15|cASE INDEPENDENT: 3-2                  19|foo100bar99baz0.txt                    19|FOO100BAR9
    16|caSE INDEPENDENT: 3-1                  22|foo1000bar99baz9.txt                   22|FOO1000BAR99BAZ0
    17|casE INDEPENDENT: 3+0                  21|foo1000bar99baz10.txt                  21|FOO1000BAR99BAZ1
    18|case INDEPENDENT: 3+1                   3|  ignore leading spaces: 2+0            3|IGNORE LEADING SPACES: 2+0
    21|foo1000bar99baz10.txt                   4|   ignore leading spaces: 2+1           4|IGNORE LEADING SPACES: 2+1
    22|foo1000bar99baz9.txt                    2| ignore leading spaces: 2-1             2|IGNORE LEADING SPACES: 2-1
    20|foo100bar10baz0.txt                     1|ignore leading spaces: 2-2              1|IGNORE LEADING SPACES: 2-2
    19|foo100bar99baz0.txt                     7|ignore m.a.s   spaces: 2+0              7|IGNORE M.A.S SPACES: 2+0
     1|ignore leading spaces: 2-2              8|ignore m.a.s    spaces: 2+1             8|IGNORE M.A.S SPACES: 2+1
     8|ignore m.a.s    spaces: 2+1             6|ignore m.a.s  spaces: 2-1               6|IGNORE M.A.S SPACES: 2-1
     7|ignore m.a.s   spaces: 2+0              5|ignore m.a.s spaces: 2-2                5|IGNORE M.A.S SPACES: 2-2
     6|ignore m.a.s  spaces: 2-1              26|Wanda                                  26|WA
     5|ignore m.a.s spaces: 2-2               23|The Wind in the Willows                23|WI

Finally noticed: "Indepenent" rather than "Independent"! But this is in the reference Python example.


## Go

This solution varies from the task in interpretation of rule 4, describing numeric fields.  This solution follows other solutions on the page by treating the <em>left-most</em> fields as most significant.  See talk page.

First four rules, no extra credit:

```go
package main

import (
    "fmt"
    "regexp"
    "sort"
    "strconv"
    "strings"
)

var tests = []struct {
    descr string
    list  []string
}{
    {"Ignoring leading spaces", []string{
        "ignore leading spaces: 2-2",
        " ignore leading spaces: 2-1",
        "  ignore leading spaces: 2+0",
        "   ignore leading spaces: 2+1",
    }},
    {"Ignoring multiple adjacent spaces", []string{
        "ignore m.a.s spaces: 2-2",
        "ignore m.a.s  spaces: 2-1",
        "ignore m.a.s   spaces: 2+0",
        "ignore m.a.s    spaces: 2+1",
    }},
    {"Equivalent whitespace characters", []string{
        "Equiv. spaces: 3-3",
        "Equiv.\rspaces: 3-2",
        "Equiv.\fspaces: 3-1",
        "Equiv.\bspaces: 3+0",
        "Equiv.\nspaces: 3+1",
        "Equiv.\tspaces: 3+2",
    }},
    {"Case Indepenent sort", []string{
        "cASE INDEPENENT: 3-2",
        "caSE INDEPENENT: 3-1",
        "casE INDEPENENT: 3+0",
        "case INDEPENENT: 3+1",
    }},
    {"Numeric fields as numerics", []string{
        "foo100bar99baz0.txt",
        "foo100bar10baz0.txt",
        "foo1000bar99baz10.txt",
        "foo1000bar99baz9.txt",
    }},
}

func main() {
    for _, test := range tests {
        fmt.Println(test.descr)
        fmt.Println("Input order:")
        for _, s := range test.list {
            fmt.Printf("   %q\n", s)
        }
        fmt.Println("Natural order:")
        l := make(list, len(test.list))
        for i, s := range test.list {
            l[i] = newNatStr(s)
        }
        sort.Sort(l)
        for _, s := range l {
            fmt.Printf("   %q\n", s.s)
        }
        fmt.Println()
    }
}

// natStr associates a string with a preprocessed form
type natStr struct {
    s string // original
    t []tok  // preprocessed "sub-fields"
}

func newNatStr(s string) (t natStr) {
    t.s = s
    s = strings.ToLower(strings.Join(strings.Fields(s), " "))
    x := dx.FindAllString(s, -1)
    t.t = make([]tok, len(x))
    for i, s := range x {
        if n, err := strconv.Atoi(s); err == nil {
            t.t[i].n = n
        } else {
            t.t[i].s = s
        }
    }
    return t
}

var dx = regexp.MustCompile(`\d+|\D+`)

// rule is to use s unless it is empty, then use n
type tok struct {
    s string
    n int
}

// rule 2 of "numeric sub-fields" from talk page
func (f1 tok) Cmp(f2 tok) int {
    switch {
    case f1.s == "":
        switch {
        case f2.s > "" || f1.n < f2.n:
            return -1
        case f1.n > f2.n:
            return 1
        }
    case f2.s == "" || f1.s > f2.s:
        return 1
    case f1.s < f2.s:
        return -1
    }
    return 0
}

type list []natStr

func (l list) Len() int      { return len(l) }
func (l list) Swap(i, j int) { l[i], l[j] = l[j], l[i] }
func (l list) Less(i, j int) bool {
    ti := l[i].t
    for k, t := range l[j].t {
        if k == len(ti) {
            return true
        }
        switch ti[k].Cmp(t) {
        case -1:
            return true
        case 1:
            return false
        }
    }
    return false
}
```

```txt

Ignoring leading spaces
Input order:
   "ignore leading spaces: 2-2"
   " ignore leading spaces: 2-1"
   "  ignore leading spaces: 2+0"
   "   ignore leading spaces: 2+1"
Natural order:
   "  ignore leading spaces: 2+0"
   "   ignore leading spaces: 2+1"
   " ignore leading spaces: 2-1"
   "ignore leading spaces: 2-2"

Ignoring multiple adjacent spaces
Input order:
   "ignore m.a.s spaces: 2-2"
   "ignore m.a.s  spaces: 2-1"
   "ignore m.a.s   spaces: 2+0"
   "ignore m.a.s    spaces: 2+1"
Natural order:
   "ignore m.a.s   spaces: 2+0"
   "ignore m.a.s    spaces: 2+1"
   "ignore m.a.s  spaces: 2-1"
   "ignore m.a.s spaces: 2-2"

Equivalent whitespace characters
Input order:
   "Equiv. spaces: 3-3"
   "Equiv.\rspaces: 3-2"
   "Equiv.\fspaces: 3-1"
   "Equiv.\bspaces: 3+0"
   "Equiv.\nspaces: 3+1"
   "Equiv.\tspaces: 3+2"
Natural order:
   "Equiv.\bspaces: 3+0"
   "Equiv.\nspaces: 3+1"
   "Equiv.\tspaces: 3+2"
   "Equiv.\fspaces: 3-1"
   "Equiv.\rspaces: 3-2"
   "Equiv. spaces: 3-3"

Case Indepenent sort
Input order:
   "cASE INDEPENENT: 3-2"
   "caSE INDEPENENT: 3-1"
   "casE INDEPENENT: 3+0"
   "case INDEPENENT: 3+1"
Natural order:
   "casE INDEPENENT: 3+0"
   "case INDEPENENT: 3+1"
   "caSE INDEPENENT: 3-1"
   "cASE INDEPENENT: 3-2"

Numeric fields as numerics
Input order:
   "foo100bar99baz0.txt"
   "foo100bar10baz0.txt"
   "foo1000bar99baz10.txt"
   "foo1000bar99baz9.txt"
Natural order:
   "foo100bar10baz0.txt"
   "foo100bar99baz0.txt"
   "foo1000bar99baz9.txt"
   "foo1000bar99baz10.txt"

```



## Haskell


Implements requests 1-5.

```haskell

import Data.List
import Data.Char
import Data.String.Utils
import Data.List.Utils
import Data.Function (on)


printOutput = do
                putStrLn "# Ignoring leading spaces \n"
                printBlockOfMessages sample1Rule ignoringStartEndSpaces
                putStrLn "\n # Ignoring multiple adjacent spaces (m.a.s) \n"
                printBlockOfMessages sample2Rule ignoringMultipleAdjacentSpaces
                putStrLn "\n # Equivalent whitespace characters \n"
                printBlockOfMessages sample3Rule ignoringMultipleAdjacentSpaces
                putStrLn "\n # Case Indepenent sorts \n"
                printBlockOfMessages sample4Rule caseIndependent
                putStrLn "\n # Numeric fields as numerics \n"
                printBlockOfMessages sample5Rule numericFieldsAsNumbers
                putStrLn "\n # Title sorts \n"
                printBlockOfMessages sample6Rule removeLeadCommonWords

printMessage message content = do
                 putStrLn message
                 mapM_ print content

printBlockOfMessages list function = do
      printMessage "Text strings:" list
      printMessage "Normally sorted:" (sort list)
      printMessage "Naturally sorted:" (sortListWith list function)


-- samples
sample1Rule = ["ignore leading spaces: 2-2", " ignore leading spaces: 2-1", "  ignore leading spaces: 2+0",  "   ignore leading spaces: 2+1"]
sample2Rule = ["ignore m.a.s spaces: 2-2", "ignore m.a.s  spaces: 2-1", "ignore m.a.s   spaces: 2+0", "ignore m.a.s    spaces: 2+1"]
sample3Rule = ["Equiv. spaces: 3-3", "Equiv.\rspaces: 3-2", "Equiv.\x0cspaces: 3-1", "Equiv.\x0bspaces: 3+0", "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2"]
sample4Rule = ["cASE INDEPENENT: 3-2", "caSE INDEPENENT: 3-1", "casE INDEPENENT: 3+0", "case INDEPENENT: 3+1"]
sample5Rule = ["foo100bar99baz0.txt", "foo100bar10baz0.txt", "foo1000bar99baz10.txt", "foo1000bar99baz9.txt"]
sample6Rule = ["The Wind in the Willows", "The 40th step more", "The 39 steps", "Wanda"]


-- function to execute all sorts
sortListWith l f = sort $ f l

-- 1. Ignore leading, trailing and multiple adjacent spaces

 -- Ignoring leading spaces

-- receive a String and remove all spaces from the start and end of that String, a String is considered an List os Char
-- ex: "  a string " = "a string"
ignoringStartEndSpaces :: [String] -> [String]
ignoringStartEndSpaces = map strip

-- Ignoring multiple adjacent spaces and Equivalent whitespace characters

ignoringMultipleAdjacentSpaces :: [String] -> [String]
ignoringMultipleAdjacentSpaces = map (unwords . words)

-- 2. Equivalent whitespace characters
-- 3. Case independent sort
-- lower case of an entire String
-- ex "SomeCAse" = "somecase"
caseIndependent :: [String] -> [String]
caseIndependent = map (map toLower)

-- 4. Numeric fields as numerics (deals with up to 20 digits)
numericFieldsAsNumbers :: [String] -> [[Int]]
numericFieldsAsNumbers = map findOnlyNumerics

findOnlyNumerics :: String -> [Int]
findOnlyNumerics s = convertDigitAsStringToInt $ makeListOfDigitsAsString $ extractDigitsAsString s
extractDigitsAsString :: String -> [String]
extractDigitsAsString s = map (filter isNumber) $ groupBy ((==) `on` isNumber ) s
makeListOfDigitsAsString :: [String] -> [String]
makeListOfDigitsAsString l = tail $ nub l
convertDigitAsStringToInt :: [String] -> [Int]
convertDigitAsStringToInt = map (joiner . map  digitToInt)

-- join a list of numbers into a single number
-- ex [4,2] = 42
joiner :: [Int] -> Int
joiner = read . concatMap show

-- 5. Title sort
removeLeadCommonWords l = map removeLeadCommonWord $ splitList l

splitList = map words
removeLeadCommonWord a = unwords $ if f a commonWords then tail a else a
                        where f l1 = elem (map toLower (head l1))
                              commonWords = ["the","a","an","of"]


```


<pre style="height: 80ex; overflow: scroll">
# Ignoring leading spaces

Text strings:
"ignore leading spaces: 2-2"
" ignore leading spaces: 2-1"
"  ignore leading spaces: 2+0"
"   ignore leading spaces: 2+1"
Normally sorted:
"   ignore leading spaces: 2+1"
"  ignore leading spaces: 2+0"
" ignore leading spaces: 2-1"
"ignore leading spaces: 2-2"
Naturally sorted:
"ignore leading spaces: 2+0"
"ignore leading spaces: 2+1"
"ignore leading spaces: 2-1"
"ignore leading spaces: 2-2"

 # Ignoring multiple adjacent spaces (m.a.s)

Text strings:
"ignore m.a.s spaces: 2-2"
"ignore m.a.s  spaces: 2-1"
"ignore m.a.s   spaces: 2+0"
"ignore m.a.s    spaces: 2+1"
Normally sorted:
"ignore m.a.s    spaces: 2+1"
"ignore m.a.s   spaces: 2+0"
"ignore m.a.s  spaces: 2-1"
"ignore m.a.s spaces: 2-2"
Naturally sorted:
"ignore m.a.s spaces: 2+0"
"ignore m.a.s spaces: 2+1"
"ignore m.a.s spaces: 2-1"
"ignore m.a.s spaces: 2-2"

 # Equivalent whitespace characters

Text strings:
"Equiv. spaces: 3-3"
"Equiv.\rspaces: 3-2"
"Equiv.\fspaces: 3-1"
"Equiv.\vspaces: 3+0"
"Equiv.\nspaces: 3+1"
"Equiv.\tspaces: 3+2"
Normally sorted:
"Equiv.\tspaces: 3+2"
"Equiv.\nspaces: 3+1"
"Equiv.\vspaces: 3+0"
"Equiv.\fspaces: 3-1"
"Equiv.\rspaces: 3-2"
"Equiv. spaces: 3-3"
Naturally sorted:
"Equiv. spaces: 3+0"
"Equiv. spaces: 3+1"
"Equiv. spaces: 3+2"
"Equiv. spaces: 3-1"
"Equiv. spaces: 3-2"
"Equiv. spaces: 3-3"

 # Case Indepenent sorts

Text strings:
"cASE INDEPENENT: 3-2"
"caSE INDEPENENT: 3-1"
"casE INDEPENENT: 3+0"
"case INDEPENENT: 3+1"
Normally sorted:
"cASE INDEPENENT: 3-2"
"caSE INDEPENENT: 3-1"
"casE INDEPENENT: 3+0"
"case INDEPENENT: 3+1"
Naturally sorted:
"case indepenent: 3+0"
"case indepenent: 3+1"
"case indepenent: 3-1"
"case indepenent: 3-2"

 # Numeric fields as numerics

Text strings:
"foo100bar99baz0.txt"
"foo100bar10baz0.txt"
"foo1000bar99baz10.txt"
"foo1000bar99baz9.txt"
Normally sorted:
"foo1000bar99baz10.txt"
"foo1000bar99baz9.txt"
"foo100bar10baz0.txt"
"foo100bar99baz0.txt"
Naturally sorted:
[100,10,0]
[100,99,0]
[1000,99,9]
[1000,99,10]

 # Title sorts

Text strings:
"The Wind in the Willows"
"The 40th step more"
"The 39 steps"
"Wanda"
Normally sorted:
"The 39 steps"
"The 40th step more"
"The Wind in the Willows"
"Wanda"
Naturally sorted:
"39 steps"
"40th step more"
"Wanda"
"Wind in the Willows"

```



## J


The natural way of approaching this task in J is to normalize the text based on the rules desired for sorting.  Here, we limit ourselves to ascii, for portability, and decide that our domain shall be terminated strings (where each string ends with the same character - typically a newline):


```j
require'strings regex'

lines=: <;.2
titleFix=: ('^\s*(the|a|an)\b';'')&rxrplc
isNum=: e.&'0123456789'
num=: ".^:(isNum@{.)
split=: <@num/.~ [:+/\1,2 ~:/\ isNum
norm=: [: split (32 9 12 13 14 15{a.) -.~ [: titleFix tolower

natSor=:  lines ;@/: norm&.>@lines
```


Example data:


```j
IgnoringLeadingSpaces=:0 :0
ignore leading spaces: 2-2
 ignore leading spaces: 2-1
  ignore leading spaces: 2+0
   ignore leading spaces: 2+1
)

IgnoringMultipleAdjacentSpaces=: 0 :0
 ignore m.a.s spaces: 2-2
 ignore m.a.s  spaces: 2-1
 ignore m.a.s   spaces: 2+0
 ignore m.a.s    spaces: 2+1
)

bsSubst=: rplc&((LF;'\'),('\r';13{a.),('\x0c';12{a.),('\x0b';11{a.),('\n';LF),:'\t';TAB)

EquivalentWhitespaceCharacters=: bsSubst 0 :0
 Equiv. spaces: 3-3
 Equiv.\rspaces: 3-2
 Equiv.\x0cspaces: 3-1
 Equiv.\x0bspaces: 3+0
 Equiv.\nspaces: 3+1
 Equiv.\tspaces: 3+2
)

CaseIndepenent=: 0 :0
 cASE INDEPENENT: 3-2
 caSE INDEPENENT: 3-1
 casE INDEPENENT: 3+0
 case INDEPENENT: 3+1
)

NumericFieldsAsNumerics=: 0 :0
 foo100bar99baz0.txt
 foo100bar10baz0.txt
 foo1000bar99baz10.txt
 foo1000bar99baz9.txt
)

Titles=: 0 :0
 The Wind in the Willows
 The 40th step more
 The 39 steps
 Wanda
)
```


Note that the required example which contains equivalent whitespace characters includes a '\n' in the data.  So, for that example, we use a backslash as our terminator.

Example use:


```j
   natSor IgnoringLeadingSpaces
  ignore leading spaces: 2+0
   ignore leading spaces: 2+1
 ignore leading spaces: 2-1
ignore leading spaces: 2-2

   natSor IgnoringMultipleAdjacentSpaces
 ignore m.a.s   spaces: 2+0
 ignore m.a.s    spaces: 2+1
 ignore m.a.s  spaces: 2-1
 ignore m.a.s spaces: 2-2

   natSor EquivalentWhitespaceCharacters
 Equiv.
spaces: 3+1\ Equiv.�spaces: 3+0\ Equiv.	spaces: 3+2\ Equiv.�spaces: 3-1\ Equiv.
spaces: 3-2\ Equiv. spaces: 3-3\
   natSor CaseIndepenent
 casE INDEPENENT: 3+0
 case INDEPENENT: 3+1
 caSE INDEPENENT: 3-1
 cASE INDEPENENT: 3-2

   natSor NumericFieldsAsNumerics
 foo100bar10baz0.txt
 foo100bar99baz0.txt
 foo1000bar99baz9.txt
 foo1000bar99baz10.txt

   natSor Titles
 The 39 steps
 The 40th step more
 Wanda
 The Wind in the Willows


```



## JavaScript


Implements the first four rules. Rule 4 works for digits up to 20 characters.


```JavaScript

var nsort = function(input) {
  var e = function(s) {
    return (' ' + s + ' ').replace(/[\s]+/g, ' ').toLowerCase().replace(/[\d]+/, function(d) {
      d = '' + 1e20 + d;
      return d.substring(d.length - 20);
    });
  };
  return input.sort(function(a, b) {
    return e(a).localeCompare(e(b));
  });
};

console.log(nsort([
  "file10.txt",
  "\nfile9.txt",
  "File11.TXT",
  "file12.txt"
]));
// -> ['\nfile9.txt', 'file10.txt', 'File11.TXT', 'file12.txt']

```



## jq

The task description seems to call for separability, which precludes
various optimizations, so the following implementation generally eschews
optimization in favor of clarity and adaptability.

The implementation is intended to satisfy all the task requirements,
but please note with respect to case-independence, the focus is on Western alphabets,
and that the repertoire of replacements is mainly illustrative.

The fourth requirement is satisfied by leveraging the ability of jq's
builtin "sort" to sort arrays lexicographically. The essence of the
matter therefore comes down to the filter named "splitup", which for
clarity, we define here as a top-level function, as follows:

```jq
def splitup:
  def tidy: if .[0] == "" then .[1:] else . end | if .[length-1] == "" then .[0:length-1] else . end ;

  # a and b are assumed to be arrays:
  def alternate(a;b):
    reduce range(0; [a,b] | map(length) | max) as $i ([]; . + [a[$i], b[$i]]);

  ([splits("[0-9]+")] | tidy) as $a
  | ([splits("[^0-9]+")] | tidy | map(tonumber)) as $n
  | (test("^[0-9]")) as $nfirst
  | if $nfirst then alternate($n; $a) else alternate($a; $n) end ;

# The following implementation of tr is more general than needed here, but the generality
# makes for adaptability.
# x and y should both be strings defining a character-by-character translation, like Unix/Linux "tr".
# if y is shorter than x, then y will in effect be padded with y's last character.
# The input may be a string or an exploded string (i.e. an array);
# the output will have the same type as the input.
def tr(x;y):
  type as $type
  | (x | explode) as $xe
  | (y | explode) as $ye
  | $ye[-1] as $last
  | if $type == "string" then explode else . end
  | map( . as $n | ($xe|index($n)) as $i | if $i then $ye[$i]//$last else . end)
  | if $type == "string" then implode else . end;

# NOTE: the order in which the filters are applied is consequential!
def natural_sort:
  def naturally:
    gsub("\\p{M}"; "")         # combining characters (accents, umlauts, enclosures, etc)
    | tr("ÀÁÂÃÄÅàáâãäåÇçÈÉÊËèéêëÌÍÎÏìíîïÒÓÔÕÖØòóôõöøÑñÙÚÛÜùúûüÝÿý";
         "AAAAAAaaaaaaCcEEEEeeeeIIIIiiiiOOOOOOooooooNnUUUUuuuuYyy")
    # Ligatures:
    | gsub("Æ"; "AE")
    | gsub("æ"; "ae")
    | gsub("\u0132"; "IJ")     #  Ĳ
    | gsub("\u0133"; "ij")     #  ĳ
    | gsub("\u0152"; "Oe")     #  Œ
    | gsub("\u0153"; "oe")     #  œ
    | gsub("ﬄ"; "ffl")
    | gsub("ﬃ"; "ffi")
    | gsub("ﬁ" ; "fi")
    | gsub("ﬀ" ; "ff")
    | gsub("ﬂ" ; "fl")
    # Illustrative replacements:
    | gsub("ß" ; "ss")         # German scharfes S
    | gsub("ſ|ʒ"; "s")         # LATIN SMALL LETTER LONG S and LATIN SMALL LETTER EZH

    | ascii_downcase
    | gsub("\\p{Cc}+";" ")     # control characters
    | gsub("^(the|a|an) "; "") # leading the/a/an (as words)
    | gsub("\\s+"; " ")        # whitespace
    | sub("^ *";"")            # leading whitespace
    | sub(" *$";"")            # trailing whitespace
    | splitup                  # embedded integers
    ;
  sort_by(naturally);
```

'''Testing'''
For brevity, we use the input as given above, but modified slightly so that it can be read
in as valid JSON.  For example, the comments have been quoted.  With these adjustments, the test driver can be written as a one-liner:

```jq
if type == "string" then "", . else natural_sort end
```

{{out}} (scrollable)
<div style="overflow:scroll; height:400px;">

```sh
jq -r -f Natural_sorting.jq Natural_sorting.json

# Ignoring leading spaces
[
  "  ignore leading spaces: 2+0",
  "   ignore leading spaces: 2+1",
  " ignore leading spaces: 2-1",
  "ignore leading spaces: 2-2"
]

# Ignoring multiple adjacent spaces (m.a.s)
[
  "ignore m.a.s   spaces: 2+0",
  "ignore m.a.s    spaces: 2+1",
  "ignore m.a.s  spaces: 2-1",
  "ignore m.a.s spaces: 2-2"
]

# Equivalent whitespace characters
[
  "Equiv.\u000bspaces: 3+0",
  "Equiv.\nspaces: 3+1",
  "Equiv.\tspaces: 3+2",
  "Equiv.\fspaces: 3-1",
  "Equiv.\rspaces: 3-2",
  "Equiv. spaces: 3-3"
]

# Case Indepenent sort
[
  "casE INDEPENENT: 3+0",
  "case INDEPENENT: 3+1",
  "caSE INDEPENENT: 3-1",
  "cASE INDEPENENT: 3-2"
]

# Numeric fields as numerics
[
  "foo100bar10baz0.txt",
  "foo100bar99baz0.txt",
  "foo1000bar99baz9.txt",
  "foo1000bar99baz10.txt"
]

# Title sorts
[
  "The 39 steps",
  "The 40th step more",
  "Wanda",
  "The Wind in the Willows"
]
```

</div>



## Julia

The functional programming principle used was to customize the "lt" comparison option of Julia's basic sort() to the "natural" sort features required.

```julia
#1
natural1(x, y) = strip(x) < strip(y)

#2
natural2(x, y) = replace(x, r"\s+" => " ") < replace(y, r"\s+" => " ")

#3
natural3(x, y) = lowercase(x) < lowercase(y)

#4
splitbynum(x) = split(x, r"(?<=\D)(?=\d)|(?<=\d)(?=\D)")
numstringtonum(arr) = [(n = tryparse(Float32, e)) != nothing ? n : e for e in arr]
function natural4(x, y)
    xarr = numstringtonum(splitbynum(x))
    yarr = numstringtonum(splitbynum(y))
    for i in 1:min(length(xarr), length(yarr))
        if typeof(xarr[i]) != typeof(yarr[i])
            a = string(xarr[i]); b = string(yarr[i])
        else
             a = xarr[i]; b = yarr[i]
        end
        if a == b
            continue
        else
            return a < b
        end
    end
    return length(xarr) < length(yarr)
end

#5
deart(x) = replace(x, r"^The\s+|^An\s+|^A\s+"i => "")
natural5(x, y) = deart(x) < deart(y)

#6
const accentdict = Dict(
'À'=> 'A', 'Á'=> 'A', 'Â'=> 'A', 'Ã'=> 'A', 'Ä'=> 'A',
'Å'=> 'A', 'Ç'=> 'C', 'È'=> 'E', 'É'=> 'E',
'Ê'=> 'E', 'Ë'=> 'E', 'Ì'=> 'I', 'Í'=> 'I', 'Î'=> 'I',
'Ï'=> 'I', 'Ñ'=> 'N', 'Ò'=> 'O', 'Ó'=> 'O',
'Ô'=> 'O', 'Õ'=> 'O', 'Ö'=> 'O', 'Ù'=> 'U', 'Ú'=> 'U',
'Û'=> 'U', 'Ü'=> 'U', 'Ý'=> 'Y', 'à'=> 'a', 'á'=> 'a',
'â'=> 'a', 'ã'=> 'a', 'ä'=> 'a', 'å'=> 'a', 'è'=> 'e',
'é'=> 'e', 'ê'=> 'e', 'ë'=> 'e', 'ì'=> 'i', 'í'=> 'i',
'î'=> 'i', 'ï'=> 'i', 'ð'=> 'd', 'ñ'=> 'n', 'ò'=> 'o',
'ó'=> 'o', 'ô'=> 'o', 'õ'=> 'o', 'ö'=> 'o', 'ù'=> 'u',
'ú'=> 'u', 'û'=> 'u', 'ü'=> 'u', 'ý'=> 'y', 'ÿ'=> 'y')
function tr(str, dict=accentdict)
    for (i, ch) in enumerate(str)
        if haskey(dict, ch)
            arr = split(str, "")
            arr[i] = string(dict[ch])
            str = join(arr)
        end
    end
    str
end

natural6(x, y) = tr(x) < tr(y)

#7
const ligaturedict = Dict('œ' => "oe", 'Œ' => "OE", 'æ' => "ae", 'Æ' => "AE", 'Ĳ' => "IJ")
natural7(x, y) = tr(x, ligaturedict) < tr(y, ligaturedict)

#8
const altsdict = Dict('ß' => "ss", 'ſ' => 's', 'ʒ' => 's')
natural8(x, y) = tr(x, altsdict) < tr(y, altsdict)

preprocessors = [natural1, natural2, natural2, natural3, natural4, natural5, natural6, natural7, natural8]

const testarrays = Vector{Vector{String}}([
["ignore leading spaces: 2-2", " ignore leading spaces: 2-1", "  ignore leading spaces: 2+0", "   ignore leading spaces: 2+1"],
["ignore m.a.s spaces: 2-2", "ignore m.a.s  spaces: 2-1", "ignore m.a.s   spaces: 2+0", "ignore m.a.s    spaces: 2+1"],
["Equiv. spaces: 3-3", "Equiv.\rspaces: 3-2", "Equiv.\x0cspaces: 3-1", "Equiv.\x0bspaces: 3+0", "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2"],
["cASE INDEPENENT: 3-2", "caSE INDEPENENT: 3-1", "casE INDEPENENT: 3+0", "case INDEPENENT: 3+1"],
["foo100bar99baz0.txt", "foo100bar10baz0.txt", "foo1000bar99baz10.txt", "foo1000bar99baz9.txt"],
["The Wind in the Willows", "The 40th step more", "The 39 steps", "Wanda"],
["Equiv. ý accents: 2-2", "Equiv. Ý accents: 2-1", "Equiv. y accents: 2+0", "Equiv. Y accents: 2+1"],
["Ĳ ligatured ij", "no ligature"],
["Start with an ʒ: 2-2", "Start with an ſ: 2-1", "Start with an ß: 2+0", "Start with an s: 2+1"]])

for (i, ltfunction) in enumerate(preprocessors)
    println("Testing sorting mod number $i. Sorted is: $(sort(testarrays[i], lt=ltfunction)).")
end

```
```txt

 Testing sorting mod number 1. Sorted is: ["  ignore leading spaces: 2+0", "   ignore leading spaces: 2+1", " ignore leading spaces: 2-1", "ignore leading spaces: 2-2"].
 Testing sorting mod number 2. Sorted is: ["ignore m.a.s   spaces: 2+0", "ignore m.a.s    spaces: 2+1", "ignore m.a.s  spaces: 2-1", "ignore m.a.s spaces: 2-2"].
 Testing sorting mod number 3. Sorted is: ["Equiv.\vspaces: 3+0", "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2", "Equiv.\fspaces: 3-1", "Equiv.\rspaces: 3-2", "Equiv. spaces: 3-3"].
 Testing sorting mod number 4. Sorted is: ["casE INDEPENENT: 3+0", "case INDEPENENT: 3+1", "caSE INDEPENENT: 3-1", "cASE INDEPENENT: 3-2"].
 Testing sorting mod number 5. Sorted is: ["foo100bar10baz0.txt", "foo100bar99baz0.txt", "foo1000bar99baz9.txt", "foo1000bar99baz10.txt"].
 Testing sorting mod number 6. Sorted is: ["The 39 steps", "The 40th step more", "Wanda", "The Wind in the Willows"].
 Testing sorting mod number 7. Sorted is: ["Equiv. Y accents: 2+1", "Equiv. Ý accents: 2-1", "Equiv. y accents: 2+0", "Equiv. ý accents: 2- 2"].
 Testing sorting mod number 8. Sorted is: ["Ĳ ligatured ij", "no ligature"].
 Testing sorting mod number 9. Sorted is: ["Start with an s: 2+1", "Start with an ſ: 2-1", "Start with an ʒ: 2-2", "Start with an ß: 2+0"].

```



## Kotlin


```scala
// version 1.1.4-3

val r2 = Regex("""[ ]{2,}""")
val r3 = Regex("""\s""")  // \s represents any whitespace character
val r5 = Regex("""\d+""")

/** Only covers ISO-8859-1 accented characters plus (for consistency) Ÿ */
val ucAccented = arrayOf("ÀÁÂÃÄÅ", "Ç", "ÈÉÊË", "ÌÍÎÏ", "Ñ", "ÒÓÔÕÖØ", "ÙÚÛÜ", "ÝŸ")
val lcAccented = arrayOf("àáâãäå", "ç", "èéêë", "ìíîï", "ñ", "òóôõöø", "ùúûü", "ýÿ")
val ucNormal = "ACEINOUY"
val lcNormal = "aceinouy"

/** Only the commoner ligatures */
val ucLigatures = "ÆĲŒ"
val lcLigatures = "æĳœ"
val ucSeparated = arrayOf("AE", "IJ", "OE")
val lcSeparated = arrayOf("ae", "ij", "oe")

/** Miscellaneous replacements */
val miscLetters = "ßſʒ"
val miscReplacements = arrayOf("ss", "s", "s")

/** Displays strings including whitespace as if the latter were literal characters */
fun String.toDisplayString(): String {
    val whitespace  = arrayOf("\t", "\n", "\u000b", "\u000c", "\r")
    val whitespace2 = arrayOf("\\t", "\\n", "\\u000b", "\\u000c", "\\r")
    var s = this
    for (i in 0..4) s = s.replace(whitespace[i], whitespace2[i])
    return s
}

/** Ignoring leading space(s) */
fun selector1(s: String) = s.trimStart(' ')

/** Ignoring multiple adjacent spaces i.e. condensing to a single space */
fun selector2(s: String) = s.replace(r2, " ")

/** Equivalent whitespace characters (equivalent to a space say) */
fun selector3(s: String) = s.replace(r3, " ")

/** Case independent sort */
fun selector4(s: String) = s.toLowerCase()

/** Numeric fields as numerics (deals with up to 20 digits) */
fun selector5(s: String) = r5.replace(s) { it.value.padStart(20, '0') }

/** Title sort */
fun selector6(s: String): String {
    if (s.startsWith("the ", true)) return s.drop(4)
    if (s.startsWith("an ", true)) return s.drop(3)
    if (s.startsWith("a ", true)) return s.drop(2)
    return s
}

/** Equivalent accented characters (and case) */
fun selector7(s: String): String {
    val sb = StringBuilder()
    outer@ for (c in s) {
        for ((i, ucs) in ucAccented.withIndex()) {
            if (c in ucs) {
                sb.append(ucNormal[i])
                continue@outer
            }
        }
        for ((i, lcs) in lcAccented.withIndex()) {
            if (c in lcs) {
                sb.append(lcNormal[i])
                continue@outer
            }
        }
        sb.append(c)
    }
    return sb.toString().toLowerCase()
}

/** Separated ligatures */
fun selector8(s: String): String {
    var ss = s
    for ((i, c) in ucLigatures.withIndex()) ss = ss.replace(c.toString(), ucSeparated[i])
    for ((i, c) in lcLigatures.withIndex()) ss = ss.replace(c.toString(), lcSeparated[i])
    return ss
}

/** Character replacements */
fun selector9(s: String): String {
    var ss = s
    for ((i, c) in miscLetters.withIndex()) ss = ss.replace(c.toString(), miscReplacements[i])
    return ss
}

fun main(args: Array<String>) {
    println("The 9 string lists, sorted 'naturally':\n")
    val s1 = arrayOf(
        "ignore leading spaces: 2-2",
        " ignore leading spaces: 2-1",
        "  ignore leading spaces: 2+0",
        "   ignore leading spaces: 2+1"
    )
    s1.sortBy(::selector1)
    println(s1.map { "'$it'" }.joinToString("\n"))

    val s2 = arrayOf(
        "ignore m.a.s spaces: 2-2",
        "ignore m.a.s  spaces: 2-1",
        "ignore m.a.s   spaces: 2+0",
        "ignore m.a.s    spaces: 2+1"
    )
    println()
    s2.sortBy(::selector2)
    println(s2.map { "'$it'" }.joinToString("\n"))

    val s3 = arrayOf(
        "Equiv. spaces: 3-3",
        "Equiv.\rspaces: 3-2",
        "Equiv.\u000cspaces: 3-1",
        "Equiv.\u000bspaces: 3+0",
        "Equiv.\nspaces: 3+1",
        "Equiv.\tspaces: 3+2"
    )
    println()
    s3.sortBy(::selector3)
    println(s3.map { "'$it'".toDisplayString() }.joinToString("\n"))

    val s4 = arrayOf(
        "cASE INDEPENENT: 3-2",
        "caSE INDEPENENT: 3-1",
        "casE INDEPENENT: 3+0",
        "case INDEPENENT: 3+1"
    )
    println()
    s4.sortBy(::selector4)
    println(s4.map { "'$it'" }.joinToString("\n"))

    val s5 = arrayOf(
        "foo100bar99baz0.txt",
        "foo100bar10baz0.txt",
        "foo1000bar99baz10.txt",
        "foo1000bar99baz9.txt"
    )
    println()
    s5.sortBy(::selector5)
    println(s5.map { "'$it'" }.joinToString("\n"))

    val s6 = arrayOf(
        "The Wind in the Willows",
        "The 40th step more",
        "The 39 steps",
        "Wanda"
    )
    println()
    s6.sortBy(::selector6)
    println(s6.map { "'$it'" }.joinToString("\n"))

    val s7 = arrayOf(
        "Equiv. ý accents: 2-2",
        "Equiv. Ý accents: 2-1",
        "Equiv. y accents: 2+0",
        "Equiv. Y accents: 2+1"
    )
    println()
    s7.sortBy(::selector7)
    println(s7.map { "'$it'" }.joinToString("\n"))

    val s8 = arrayOf(
        "Ĳ ligatured ij",
        "no ligature"
    )
    println()
    s8.sortBy(::selector8)
    println(s8.map { "'$it'" }.joinToString("\n"))

    val s9 = arrayOf(
        "Start with an ʒ: 2-2",
        "Start with an ſ: 2-1",
        "Start with an ß: 2+0",
        "Start with an s: 2+1"
    )
    println()
    s9.sortBy(::selector9)
    println(s9.map { "'$it'" }.joinToString("\n"))
}
```


```txt

The 9 string lists, sorted 'naturally':

'  ignore leading spaces: 2+0'
'   ignore leading spaces: 2+1'
' ignore leading spaces: 2-1'
'ignore leading spaces: 2-2'

'ignore m.a.s   spaces: 2+0'
'ignore m.a.s    spaces: 2+1'
'ignore m.a.s  spaces: 2-1'
'ignore m.a.s spaces: 2-2'

'Equiv.\u000bspaces: 3+0'
'Equiv.\nspaces: 3+1'
'Equiv.\tspaces: 3+2'
'Equiv.\u000cspaces: 3-1'
'Equiv.\rspaces: 3-2'
'Equiv. spaces: 3-3'

'casE INDEPENENT: 3+0'
'case INDEPENENT: 3+1'
'caSE INDEPENENT: 3-1'
'cASE INDEPENENT: 3-2'

'foo100bar10baz0.txt'
'foo100bar99baz0.txt'
'foo1000bar99baz9.txt'
'foo1000bar99baz10.txt'

'The 39 steps'
'The 40th step more'
'Wanda'
'The Wind in the Willows'

'Equiv. y accents: 2+0'
'Equiv. Y accents: 2+1'
'Equiv. Ý accents: 2-1'
'Equiv. ý accents: 2-2'

'Ĳ ligatured ij'
'no ligature'

'Start with an s: 2+1'
'Start with an ſ: 2-1'
'Start with an ʒ: 2-2'
'Start with an ß: 2+0'

```



## Pascal

This employs the string-type variables which in Turbo Pascal are limited to 255 characters. Delphi for example allows much longer string capacities, but 255 will do for the example. There are no built-in facilities for parsing input into some sort of flexible data structure that could then be sorted stylishly, so the plan is to engage in a complex comparison routine of text strings. With sorting, it is usually better to convert a complex ordering into a simple auxiliary key, because there will be many more comparisons than conversions. However, that would be difficult here because of the requirement for matching digit sequences aligned to the right: when an entry is compared against others, their digit sequences may be of different lengths or indeed not be aligned against digits in that entry. One could try a scan of all texts to determine the maximum length of all the digit sequences and then in each auxiliary key supply sufficient leading zero characters to make the digit sequences all the same size... Instead, the plan is for each comparison pair, to supply leading zero characters as the need arises, which is only when a digit in one text is to be compared with a digit in the other.

Only integers are considered (so no decimal fractions, to avoid confusion over the likes of "Version 3.1.4") and likewise, signs are not taken as a part of a number, lest there be confusion over "+6" and " 6".

Recognised articles are "A", "An" and "The" - this could be extended (as in "Thoughts on Linguistics, Some") but context and meaning soon become important in determining what to keep and what to skip, similarly with possible internal words such as "of" and "and", etc. Really, what is wanted is an extraction from the title of key words. But, what are the key words and what aren't?

The "structured" features of Pascal do not facilitate escape from loops, so, ... some <code>goto</code> atavisms appear in what follows...

```Pascal

Program Natural; Uses DOS, crt;	{Simple selection.}
{Demonstrates a "natural" order of sorting text with nameish parts.}

 Const null=#0; BS=#8; HT=#9; LF=#10{0A}; VT=#11{0B}; FF=#12{0C}; CR=#13{0D};

 Procedure Croak(gasp: string);
  Begin
   WriteLn(Gasp);
   HALT;
  End;

 Function Space(n: integer): string;	{Can't use n*" " either.}
  var text: string;	{A scratchpad.}
  var i: integer;	{A stepper.}
  Begin
   if n > 255 then n:=255	{A value parameter,}
    else if n < 0 then n:=0;	{So this just messes with my copy.}
   for i:=1 to n do text[i]:=' ';	{Place some spaces.}
   text[0]:=char(n);			{Place the length thereof.}
   Space:=text;		{Take that.}
  End; {of Space.}

 Function DeFang(x: string): string;	{Certain character codes cause action.}
  var text: string;	{A scratchpad, as using DeFang directly might imply recursion.}
  var i: integer;	{A stepper.}
  var c: char;		{Reduce repetition.}
  Begin			{I hope that appending is recognised by the compiler...}
   text:='';			{Scrub the scratchpad.}
   for i:=1 to Length(x) do	{Step through the source text.}
    begin			{Inspecting each character.}
     c:=char(x[i]);		{Grab it.}
     if c > CR then text:=text + c	{Deemed not troublesome.}
      else if c < BS then text:=text + c	{Lacks an agreed alternative, and may not cause trouble.}
       else text:=text + '!' + copy('btnvfr',ord(c) - ord(BS) + 1,1);	{The alternative codes.}
    end;			{On to the next.}
   DeFang:=text;	{Alas, the "escape" convention lengthens the text.}
  End; {of DeFang.}	{But that only mars the layout, rather than ruining it.}

 Const mEntry = 66;	{Sufficient for demonstrations.}
 Type EntryList = array[0..mEntry] of integer;	{Identifies texts by their index.}
 var  EntryText: array[1..mEntry] of string;	{Inbto this array.}
 var nEntry: integer;				{The current number.}
 Function AddEntry(x: string): integer;	{Add another text to the collection.}
  Begin	 {Could extend to checking for duplicates via a sorted list...}
   if nEntry >= mEntry then Croak('Too many entries!');	{Perhaps not!}
   inc(nEntry);			{So, another.}
   EntryText[nEntry]:=x;	{Placed.}
   AddEntry:=nEntry;		{The caller will want to know where.}
  End; {of AddEntry.}

 Function TextOrder(i,j: integer): boolean;	{This is easy.}
  Begin						{But despite being only one statement, and simple at that,}
   TextOrder:=EntryText[i] <= EntryText[j];	{Begin...End is insisted upon.}
  End;	{of TextOrder.}

 Function NaturalOrder(e1,e2: integer): boolean;{Not so easy.}
  const Article: array[1..3] of string[4] = ('A ','AN ','THE ');	{Each with its trailing space.}
  Function Crush(var c: char): char;	{Suppresses divergence.}
   Begin				{To simplify comparisons.}
    if c <= ' ' then Crush:=' '		{Crush the fancy control characters.}
     else Crush:=UpCase(c);		{Also crush a < A or a > A or a = A questions.}
   End; {of Crush.}
  var Wot: array[1..2] of integer;	{Which text is being fingered.}
  var Tail: array[1..2] of integer;	{Which article has been found at the start.}
  var l,lst: array[1..2] of integer;	{Finger to the current point, and last character.}
  Procedure Librarian;		{Initial inspection of the texts.}
   var Blocked: boolean;	{Further progress may be obstructed.}
   var a,is,i: integer;		{Odds and ends.}
   label Hic;			{For escaping the search when a match is complete.}
   Begin		{There are two texts to inspect.}
    for is:=1 to 2 do	{Treat them alike.}
     begin			{This is the first encounter.}
      l[is]:=1;			{So start the scan with the first character.}
      Tail[is]:=0;		{No articles found.}
      while (l[is] <= lst[is]) and (EntryText[wot[is]][l[is]] <= ' ') do inc(l[is]);	{Leading spaceish.}
      for a:=1 to 3 do		{Try to match an article at the start of the text.}
       begin				{Each article's text has a trailing space to be matched also.}
        i:=0;				{Start a for-loop, but with early escape in mind.}
        Repeat 				{Compare successive characters, for i:=0 to a...}
         if l[is] + i > lst[is] then Blocked:=true	{Probed past the end of text?}
          else Blocked:=Crush(EntryText[wot[is]][l[is] + i]) <> Article[a][i + 1];	{No. Compare capitals.}
         inc(i);			{Stepping on to the next character.}
        Until Blocked or (i > a);	{Conveniently, Length(Article[a]) = a.}
        if not Blocked then	{Was a mismatch found?}
         begin			{No!}
          Tail[is]:=a;		{So, identify the discovery.}
          l[is]:=l[is] + i;	{And advance the scan to whatever follows.}
          goto Hic;		{Escape so as to consider the other text.}
         end;			{Since two texts are being considered separately.}
      end;		{Sigh. no "Next a" or similar syntax.}
 Hic:dec(l[is]);	{Backstep one, ready to advance later.}
     end;	{Likewise, no "for is:=1 to 2 do ... Next is" syntax.}
   End; {of Librarian.}
  var c: array[1..2] of string[1];	{Selected by Advance for comparison.}
  var d: integer;		{Their difference.}
  type moody = (Done,Bored,Grist,Numeric);	{Might as well have some mnemonics.}
  var Mood: array[1..2] of moody;		{As the scan proceeds, moods vary.}
  var depth: array[1..2] of integer;	{Digit depth.}
  Procedure Another;	{Choose a pair of characters to compare.}
  {Digit sequences are special! But periods are ignored, also signs, avoiding confusion over "+6" and " 6".}
   var is: integer;	{Selects from one text or the other.}
   var ll: integer;	{Looks past the text into any Article.}
   var d: char;		{Possibly a digit.}
   Begin
    for is:=1 to 2 do	{Same treatment for both texts.}
     begin			{Find the next character, and taste it.}
      repeat			{If already bored, slog through any following spaces.}
       inc(l[is]);			{So, advance one character onwards.}
       ll:=l[is] - lst[is];		{Compare to the end of the normal text.}
       if ll <= 0 then c[is]:=Crush(EntryText[wot[is]][l[is]])	{Still in the normal text.}
        else if Tail[is] <= 0 then c[is]:=''		{Perhaps there is no tail.}
         else if ll <= 2 then c[is]:=copy(', ',ll,1)	{If there is, this is the junction.}
          else if ll <= 2 + Tail[is] then c[is]:=copy(Article[Tail[is]],ll - 2,1)	{And this the tail.}
           else c[is]:='';				{Actually, the copy would do this.}
      until not ((c[is] = ' ') and (Mood[is] = Bored));	{Thus pass multiple enclosed spaces, but not the first.}
      if length(c[is]) <= 0 then Mood[is]:=Done 	{Perhaps we ran off the end, even of the tail.}
       else if c[is] = ' ' then Mood[is]:=Bored		{The first taste of a space induces boredom.}
        else if ('0' <= c[is]) and (c[is] <= '9') then Mood[is]:=Numeric	{Paired, evokes special attention.}
         else Mood[is]:=Grist;		{All else is grist for my comparisons.}
     end;		{Switch to the next text.}
{Comparing digit sequences is to be done as if numbers. "007" vs "70" is to become vs. "070" by length matching.}
    if (Mood[1] = Numeric) and (Mood[2] = Numeric) then	{Are both texts yielding a digit?}
     begin					{Yes. Special treatment impends.}
      if (Depth[1] = 0) and (Depth[2] = 0) then	{Do I already know how many digits impend?}
       for is:=1 to 2 do				{No. So for each text,}
         repeat						{Keep looking until I stop seeing digits.}
          inc(Depth[is]);				{I am seeing a digit, so there will be one to count.}
          ll:=l[is] + Depth[is];			{Finger the next position.}
          if ll > lst[is] then d:=null			{And if not off the end,}
           else d:=EntryText[wot[is]][ll];		{Grab a potential digit.}
         until (d < '0') or (d > '9');			{If it is one, probe again.}
      if Depth[1] < Depth[2] then	{Righto, if the first sequence has fewer digits,}
       begin					{Supply a free zero.}
        dec(Depth[2]);				{The second's digit will be consumed.}
        dec(l[1]);				{The first's will be re-encountered.}
        c[1]:='0';				{Here is the zero}
       end					{For the comparison.}
       else if Depth[2] < Depth[1] then	{But if the second has fewer digits to come,}
        begin					{Don't dig into them yet.}
         dec(Depth[1]);				{The first's digit will be used.}
         dec(l[2]);				{But the second's seen again.}
         c[2]:='0';				{After this has been used}
        end					{In the comparison.}
        else				{But if both have the same number of digits remaining,}
         begin					{Then the comparison is aligned.}
          dec(Depth[1]);			{So this digit will be used.}
          dec(Depth[2]);			{As will this.}
         end;					{In the comparison.}
     end;			{Thus, arbitrary-size numbers are allowed, as they're never numbers.}
   End; {of Another.}	{Possibly, the two characters will be the same, and another pair will be requested.}
  Begin {of NaturalOrder.}
   Wot[1]:=e1; Wot[2]:=e2;		{Make the two texts accessible via indexing.}
   lst[1]:=Length(EntryText[e1]);	{The last character of the first text.}
   lst[2]:=Length(EntryText[e2]);	{And of the second. Saves on repetition.}
   Mood[1]:=Bored; Mood[2]:=Bored;	{Behave as if we have already seen a space.}
   depth[1]:=0;     depth[2]:=0;	{And, no digits in concert have been seen.}
   Librarian;		{Start the inspection.}
   repeat		{Chug along, until a difference is found.}
    Another;			{To do so, choose another pair of characters to compare.}
    d:=Length(c[2]) - Length(c[1]);	{If one text has run out, favour the shorter.}
    if (d = 0) and (Length(c[1]) > 0) then d:=ord(c[2][1]) - ord(c[1][1]);	{Otherwise, their difference.}
   until (d <> 0) or ((Mood[1] = Done) and (Mood[2] = Done));	{Well? Are we there yet?}
   NaturalOrder:=d >= 0;	{And so, does e1's text precede e2's?}
  End; {of NatualOrder.}

 var TextSort: boolean;		{Because I can't pass a function as a parameter,}
 Function InOrder(i,j: integer): boolean;	{I can only use one function.}
  Begin						{Which messes with a selector.}
   if TextSort then InOrder:=TextOrder(i,j)	{So then,}
    else InOrder:=NaturalOrder(i,j);		{Which is it to be?}
  End; {of InOrder.}
 Procedure OrderEntry(var List: EntryList);	{Passing a ordinary array is not Pascalish, damnit.}
{Crank up a Comb sort of the entries fingered by List. Working backwards, just for fun.}
{Caution: the H*10/13 means that H ought not be INTEGER*2. Otherwise, use H/1.3.}
  var t: integer;	{Same type as the elements of List.}
  var N,i,h: integer;	{Odds and ends.}
  var happy: boolean;	{To be attained.}
  Begin
   N:=List[0];		{Extract the count.}
   h:=N - 1;		{"Last" - "First", and not +1.}
   if h <= 0 then exit;	{Ha ha.}
   Repeat		{Start the pounding.}
    h:=LongInt(h)*10 div 13;	{Beware overflow, or, use /1.3.}
    if h <= 0 then h:=1;	{No "max" function, damnit.}
    if (h = 9) or (h = 10) then h:=11;	{A fiddle.}
    happy:=true;		{No disorder seen.}
    for i:=N - h downto 1 do	{So, go looking. If h = 1, this is a Bubblesort.}
     if not InOrder(List[i],List[i + h]) then	{How about this pair?}
      begin						{Alas.}
       t:=List[i]; List[i]:=List[i + h]; List[i + h]:=t;{No Swap(a,b), damnit.}
       happy:=false;				{Disorder has been discovered.}
      end;				{On to the next comparison.}
   Until happy and (h = 1);	{No suspicion remains?}
  End; {of OrderEntry.}

 var Item,Fancy: EntryList;	{Two lists of entry indices.}
 var i: integer;	{A stepper.}
 var t1: string;	{A scratchpad.}
 BEGIN
  nEntry:=0;	{No entries are stored.}
  i:=0;		{Start a stepper.}
  inc(i);Item[i]:=AddEntry('ignore leading spaces: 2-2');
  inc(i);Item[i]:=AddEntry(' ignore leading spaces: 2-1');
  inc(i);Item[i]:=AddEntry('  ignore leading spaces: 2+0');
  inc(i);Item[i]:=AddEntry('   ignore leading spaces: 2+1');
  inc(i);Item[i]:=AddEntry('ignore m.a.s spaces: 2-2');
  inc(i);Item[i]:=AddEntry('ignore m.a.s  spaces: 2-1');
  inc(i);Item[i]:=AddEntry('ignore m.a.s   spaces: 2+0');
  inc(i);Item[i]:=AddEntry('ignore m.a.s    spaces: 2+1');
  inc(i);Item[i]:=AddEntry('Equiv.'+' '+'spaces: 3-3');
  inc(i);Item[i]:=AddEntry('Equiv.'+CR+'spaces: 3-2');	{CR can't appear as itself.}
  inc(i);Item[i]:=AddEntry('Equiv.'+FF+'spaces: 3-1');	{As it is used to mark line endings.}
  inc(i);Item[i]:=AddEntry('Equiv.'+VT+'spaces: 3+0');	{And if typed in an editor,}
  inc(i);Item[i]:=AddEntry('Equiv.'+LF+'spaces: 3+1');	{It is acted upon there and then.}
  inc(i);Item[i]:=AddEntry('Equiv.'+HT+'spaces: 3+2');	{So, name instead of value.}
  inc(i);Item[i]:=AddEntry('cASE INDEPENDENT: 3-2');
  inc(i);Item[i]:=AddEntry('caSE INDEPENDENT: 3-1');
  inc(i);Item[i]:=AddEntry('casE INDEPENDENT: 3+0');
  inc(i);Item[i]:=AddEntry('case INDEPENDENT: 3+1');
  inc(i);Item[i]:=AddEntry('foo100bar99baz0.txt');
  inc(i);Item[i]:=AddEntry('foo100bar10baz0.txt');
  inc(i);Item[i]:=AddEntry('foo1000bar99baz10.txt');
  inc(i);Item[i]:=AddEntry('foo1000bar99baz9.txt');
  inc(i);Item[i]:=AddEntry('The Wind in the Willows');
  inc(i);Item[i]:=AddEntry('The 40th step more');
  inc(i);Item[i]:=AddEntry('The 39 steps');
  inc(i);Item[i]:=AddEntry('Wanda');
  {inc(i);Item[i]:=AddEntry('The Worth of Wirth''s Way');}
  Item[0]:=nEntry;	{Complete the EntryList protocol.}
  for i:=0 to nEntry do Fancy[i]:=Item[i];	{Sigh. Fancy:=Item.}

  TextSort:=true; OrderEntry(Item);	{Plain text ordering.}

  TextSort:=false; OrderEntry(Fancy);	{Natural order.}

  WriteLn('    Text order                         Natural order');
  for i:=1 to nEntry do
   begin
    t1:=DeFang(EntryText[Item[i]]);
    WriteLn(Item[i]:3,'|',t1,Space(30 - length(t1)),' ',
           Fancy[i]:3,'|',DeFang(EntryText[Fancy[i]]));
   end;

 END.

```

Output, with "!" instead of a backslash to prevent context confusions here:
    Text order                         Natural order
  4|   ignore leading spaces: 2+1   25|The 39 steps
  3|  ignore leading spaces: 2+0    24|The 40th step more
  2| ignore leading spaces: 2-1     17|casE INDEPENDENT: 3+0
 14|Equiv.!tspaces: 3+2             18|case INDEPENDENT: 3+1
 13|Equiv.!nspaces: 3+1             16|caSE INDEPENDENT: 3-1
 12|Equiv.!vspaces: 3+0             15|cASE INDEPENDENT: 3-2
 11|Equiv.!fspaces: 3-1             12|Equiv.!vspaces: 3+0
 10|Equiv.!rspaces: 3-2             13|Equiv.!nspaces: 3+1
  9|Equiv. spaces: 3-3              14|Equiv.!tspaces: 3+2
 25|The 39 steps                    11|Equiv.!fspaces: 3-1
 24|The 40th step more              10|Equiv.!rspaces: 3-2
 23|The Wind in the Willows          9|Equiv. spaces: 3-3
 26|Wanda                           20|foo100bar10baz0.txt
 15|cASE INDEPENDENT: 3-2           19|foo100bar99baz0.txt
 16|caSE INDEPENDENT: 3-1           22|foo1000bar99baz9.txt
 17|casE INDEPENDENT: 3+0           21|foo1000bar99baz10.txt
 18|case INDEPENDENT: 3+1            3|  ignore leading spaces: 2+0
 21|foo1000bar99baz10.txt            4|   ignore leading spaces: 2+1
 22|foo1000bar99baz9.txt             2| ignore leading spaces: 2-1
 20|foo100bar10baz0.txt              1|ignore leading spaces: 2-2
 19|foo100bar99baz0.txt              7|ignore m.a.s   spaces: 2+0
  1|ignore leading spaces: 2-2       8|ignore m.a.s    spaces: 2+1
  8|ignore m.a.s    spaces: 2+1      6|ignore m.a.s  spaces: 2-1
  7|ignore m.a.s   spaces: 2+0       5|ignore m.a.s spaces: 2-2
  6|ignore m.a.s  spaces: 2-1       26|Wanda
  5|ignore m.a.s spaces: 2-2        23|The Wind in the Willows


## Perl


This implements all 8 requirements<sup>*</sup>:


```perl

use feature 'fc';
use Unicode::Normalize;

sub natural_sort {
    my @items = map {
        my $str = fc(NFKD($_));
        $str =~ s/\s+/ /;
        $str =~ s/|^(?:the|a|an) \b|\p{Nonspacing_Mark}| $//g;
        my @fields = $str =~ /(?!\z) ([^0-9]*+) ([0-9]*+)/gx;
        [$_, \@fields]
    } @_;
    return map { $_->[0] } sort {
        my @x = @{$a->[1]};
        my @y = @{$b->[1]};
        my $numeric;
        while (@x && @y) {
            my ($x, $y) = (shift @x, shift @y);
            return (($numeric = !$numeric) ? $x cmp $y : $x <=> $y or next);
        }
        return @x <=> @y;
    } @items;
}

```


: <sup>*)</sup> Note that decomposing the strings to the NFKD normalization form and subsequently stripping off all code points of the <code>Nonspacing_Mark</code> category, removes differences caused by accents / ligatures / alternate character forms / etc. in a standards-compliant way. This coincides with all the examples given in the task description, with the exception that it does ''not'' replace "ʒ" with "s" &mdash; one could add <pre style="display:inline;padding:0.3em">$str =~ tr/ʒ/s/;
```
 for that but it seems a bit [[wp:International_Phonetic_Alphabet_chart_for_English_dialects#Chart|whimsical]].)

'''Testing:'''


```perl

use utf8;        # interpret this script's source code as UTF8
use Test::More;  # for plan(), is_deeply()
use Data::Dump;  # for dd()

my @testcases = (
    ['Leading spaces',   '%sleading spaces: %i',  map {' ' x $_} 2, 3, 1, 0             ],
    ['Adjacent spaces',  'adjacent%s spaces: %i', map {' ' x $_} 2, 3, 1, 0             ],
    ['Different spaces', 'equiv.%sspaces: %i',    split //, "\x0b\n\t\x0c\r "           ],
    ['Case differences', '%s INDEPENENT: %i',     'casE', 'case', 'caSE', 'cASE'        ],
    ['Numeric fields',   'foo%ibar%ibaz%i.txt',   [100, 10, 0], [100, 99, 0],
                                                  [1000,99,9], [1000,99,10]             ],
    ['Title case',       '%s',                    'The 39 steps', 'The 40th step more',
                                                  'Wanda', 'The Wind in the Willows'    ],
    ['Accents',          'Equiv. %s accents: %i', 'y', 'Y', "\x{dd}", "\x{fd}"          ],
    ['Ligatures',        '%s',                    "Ĳ ligatured ij", 'no ligature'       ],
    ['Alternate forms',  'Start with an %s: %i',  's', 'ſ', 'ß'                         ],
);

plan tests => scalar @testcases;

foreach (@testcases) {
    my ($name, $pattern, @args) = @$_;
    my $i = 0;
    my @strings = map { sprintf $pattern, ref $_ ? @$_ : $_, $i++ } @args;

    is_deeply( [natural_sort(reverse sort @strings)], \@strings, $name );

    dd @strings;
    print "\n";
}

```


```txt

1..9
ok 1 - Leading spaces
(
  "  leading spaces: 0",
  "   leading spaces: 1",
  " leading spaces: 2",
  "leading spaces: 3",
)

ok 2 - Adjacent spaces
(
  "adjacent   spaces: 0",
  "adjacent    spaces: 1",
  "adjacent  spaces: 2",
  "adjacent spaces: 3",
)

ok 3 - Different spaces
(
  "equiv.\13spaces: 0",
  "equiv.\nspaces: 1",
  "equiv.\tspaces: 2",
  "equiv.\fspaces: 3",
  "equiv.\rspaces: 4",
  "equiv. spaces: 5",
)

ok 4 - Case differences
(
  "casE INDEPENENT: 0",
  "case INDEPENENT: 1",
  "caSE INDEPENENT: 2",
  "cASE INDEPENENT: 3",
)

ok 5 - Numeric fields
(
  "foo100bar10baz0.txt",
  "foo100bar99baz0.txt",
  "foo1000bar99baz9.txt",
  "foo1000bar99baz10.txt",
)

ok 6 - Title case
(
  "The 39 steps",
  "The 40th step more",
  "Wanda",
  "The Wind in the Willows",
)

ok 7 - Accents
(
  "Equiv. y accents: 0",
  "Equiv. Y accents: 1",
  "Equiv. \xDD accents: 2",
  "Equiv. \xFD accents: 3",
)

ok 8 - Ligatures
("\x{132} ligatured ij", "no ligature")

ok 9 - Alternate forms
(
  "Start with an s: 0",
  "Start with an \x{17F}: 1",
  "Start with an \xDF: 2",
)

```



## Perl 6

In Perl6 it is very easy to modify the default sorting order by passing in a
transform routine to the sort function. If the transform routines are arity one,
the sort function will apply a Schwartzian Transform so it only needs to calculate
the transform once. Note that the transforms are non-destructive; The sort function
returns the original strings.

The following are a series of subroutines to perform the various natural
sorting transforms. They may be applied individually or mixed and matched
to get the particular result desired. When more than one is strung
together, they apply left to right. Some combinations may yield
different results depending on the order they are applied.


```perl6
# Sort groups of digits in number order. Sort by order of magnitude then lexically.
sub naturally ($a) { $a.lc.subst(/(\d+)/, ->$/ {0~$0.chars.chr~$0},:g) ~"\x0"~$a }

# Collapse multiple ws characters to a single.
sub collapse ($a) { $a.subst( / ( \s ) $0+ /, -> $/ { $0 }, :g ) }

# Convert all ws characters to a space.
sub normalize ($a) { $a.subst( / ( \s ) /, ' ', :g ) }

# Ignore common leading articles for title sorts
sub title ($a) { $a.subst( / :i ^ ( a | an | the ) >> \s* /, '' ) }

# Decompose ISO-Latin1 glyphs to their base character.
sub latin1_decompose ($a) {
    $a.trans: <
       Æ AE æ ae Þ TH þ th Ð TH ð th ß ss À A Á A Â A Ã A Ä A Å A à a á a
        â a ã a ä a å a Ç C ç c È E É E Ê E Ë E è e é e ê e ë e Ì I Í I Î
        I Ï I ì i í i î i ï i Ò O Ó O Ô O Õ O Ö O Ø O ò o ó o ô o õ o ö o
        ø o Ñ N ñ n Ù U Ú U Û U Ü U ù u ú u û u ü u Ý Y ÿ y ý y
    >.hash;
}

# Used as:

my @tests = (
    [
        "Task 1a\nSort while ignoring leading spaces.",
        [
          'ignore leading spaces: 1', '   ignore leading spaces: 4',
          '  ignore leading spaces: 3', ' ignore leading spaces: 2'
        ],
        {.trim} # builtin method.
    ],
    [
        "Task 1b\nSort while ignoring multiple adjacent spaces.",
        [
          'ignore m.a.s   spaces: 3', 'ignore m.a.s spaces: 1',
          'ignore m.a.s    spaces: 4', 'ignore m.a.s  spaces: 2'
        ],
        {.&collapse}
    ],
    [
        "Task 2\nSort with all white space normalized to regular spaces.",
        [
          "Normalized\tspaces: 4", "Normalized\xa0spaces: 1",
          "Normalized\x20spaces: 2", "Normalized\nspaces: 3"
        ],
        {.&normalize}
    ],
    [
        "Task 3\nSort case independently.",
        [
          'caSE INDEPENDENT: 3', 'casE INDEPENDENT: 2',
          'cASE INDEPENDENT: 4', 'case INDEPENDENT: 1'
        ],
        {.lc} # builtin method
    ],
    [
        "Task 4\nSort groups of digits in natural number order.",
        [
          <Foo100bar99baz0.txt foo100bar10baz0.txt foo1000bar99baz10.txt
           foo1000bar99baz9.txt 201st 32nd 3rd 144th 17th 2 95>
        ],
        {.&naturally}
    ],
    [
        "Task 5 ( mixed with 1, 2, 3 & 4 )\n"
        ~ "Sort titles, normalize white space, collapse multiple spaces to\n"
        ~ "single, trim leading white space, ignore common leading articles\n"
        ~ 'and sort digit groups in natural order.',
        [
          'The Wind	in the Willows  8', '  The 39 Steps               3',
          'The    7th Seal              1', 'Wanda                        6',
          'A Fish Called Wanda          5', ' The Wind and the Lion       7',
          'Any Which Way But Loose      4', '12 Monkeys                   2'
        ],
        {.&normalize.&collapse.trim.&title.&naturally}
    ],
    [
        "Task 6, 7, 8\nMap letters in Latin1 that have accents or decompose to two\n"
        ~ 'characters to their base characters for sorting.',
        [
          <apple Ball bald car Card above Æon æon aether
            niño nina e-mail Évian evoke außen autumn>
        ],
        {.&latin1_decompose.&naturally}
    ]
);


for @tests -> $case {
    my $code_ref = $case.pop;
    my @array = $case.pop.list;
    say $case.pop, "\n";

    say "Standard Sort:\n";
    .say for @array.sort;

    say "\nNatural Sort:\n";
    .say for @array.sort: {.$code_ref};

    say "\n" ~ '*' x 40 ~ "\n";
}
```


Sample output:

```txt

Task 1a
Sort while ignoring leading spaces.

Standard Sort:

   ignore leading spaces: 4
  ignore leading spaces: 3
 ignore leading spaces: 2
ignore leading spaces: 1

Natural Sort:

ignore leading spaces: 1
 ignore leading spaces: 2
  ignore leading spaces: 3
   ignore leading spaces: 4

****************************************

Task 1b
Sort while ignoring multiple adjacent spaces.

Standard Sort:

ignore m.a.s    spaces: 4
ignore m.a.s   spaces: 3
ignore m.a.s  spaces: 2
ignore m.a.s spaces: 1

Natural Sort:

ignore m.a.s spaces: 1
ignore m.a.s  spaces: 2
ignore m.a.s   spaces: 3
ignore m.a.s    spaces: 4

****************************************

Task 2
Sort with all white space normalized to regular spaces.

Standard Sort:

Normalized	spaces: 4
Normalized
spaces: 3
Normalized spaces: 2
Normalized spaces: 1

Natural Sort:

Normalized spaces: 1
Normalized spaces: 2
Normalized
spaces: 3
Normalized	spaces: 4

****************************************

Task 3
Sort case independently.

Standard Sort:

cASE INDEPENDENT: 4
caSE INDEPENDENT: 3
casE INDEPENDENT: 2
case INDEPENDENT: 1

Natural Sort:

case INDEPENDENT: 1
casE INDEPENDENT: 2
caSE INDEPENDENT: 3
cASE INDEPENDENT: 4

****************************************

Task 4
Sort groups of digits in natural number order.

Standard Sort:

144th
17th
2
201st
32nd
3rd
95
Foo100bar99baz0.txt
foo1000bar99baz10.txt
foo1000bar99baz9.txt
foo100bar10baz0.txt

Natural Sort:

2
3rd
17th
32nd
95
144th
201st
foo100bar10baz0.txt
Foo100bar99baz0.txt
foo1000bar99baz9.txt
foo1000bar99baz10.txt

****************************************

Task 5 ( mixed with 1, 2, 3 & 4 )
Sort titles, normalize white space, collapse multiple spaces to
single, trim leading white space, ignore common leading articles
and sort digit groups in natural order.

Standard Sort:

  The 39 Steps               3
 The Wind and the Lion       7
12 Monkeys                   2
A Fish Called Wanda          5
Any Which Way But Loose      4
The    7th Seal              1
The Wind	in the Willows  8
Wanda                        6

Natural Sort:

The    7th Seal              1
12 Monkeys                   2
  The 39 Steps               3
Any Which Way But Loose      4
A Fish Called Wanda          5
Wanda                        6
 The Wind and the Lion       7
The Wind	in the Willows  8

****************************************

Task 6, 7, 8
Map letters in Latin1 that have accents or decompose to two
characters to their base characters for sorting.

Standard Sort:

Ball
Card
above
aether
apple
autumn
außen
bald
car
e-mail
evoke
nina
niño
Æon
Évian
æon

Natural Sort:

above
Æon
æon
aether
apple
außen
autumn
bald
Ball
car
Card
e-mail
Évian
evoke
nina
niño

****************************************

```



## Phix

As per C, common words anywhere in the string are omitted. All eight features.

Needs chcp 65001 (or 28591) to get this to work on Windows, be sure to save as utf8.

```Phix
--
-- demo/rosetta/Natural_sorting2.exw
--
function utf32ch(sequence s)
    for i=1 to length(s) do
        s[i] = utf8_to_utf32(s[i])[1]
    end for
    return s
end function

constant common = {"the","it","to","a","of","is"},
         {al,ac_replacements} = columnize({
            {"Æ","AE"},{"æ","ae"},{"Þ","TH"},{"þ","th"},
            {"Ð","TH"},{"ð","th"},{"ß","ss"},{"�","fi"},
            {"�","fl"},{"",'s'},{"’",'z'},
            {"À",'A'},{"Á",'A'},{"Â",'A'},{"Ã",'A'},
            {"Ä",'A'},{"Å",'A'},{"à",'a'},{"á",'a'},
            {"â",'a'},{"ã",'a'},{"ä",'a'},{"å",'a'},
            {"Ç",'C'},{"ç",'c'},{"È",'E'},{"É",'E'},
            {"Ê",'E'},{"Ë",'E'},{"è",'e'},{"é",'e'},
            {"ê",'e'},{"ë",'e'},{"Ì",'I'},{"Í",'I'},
            {"Î",'I'},{"Ï",'I'},{"ì",'i'},{"í",'i'},
            {"î",'i'},{"ï",'i'},{"Ò",'O'},{"Ó",'O'},
            {"Ô",'O'},{"Õ",'O'},{"Ö",'O'},{"Ø",'O'},
            {"ò",'o'},{"ó",'o'},{"ô",'o'},{"õ",'o'},
            {"ö",'o'},{"ø",'o'},{"Ñ",'N'},{"ñ",'n'},
            {"Ù",'U'},{"Ú",'U'},{"Û",'U'},{"Ü",'U'},
            {"ù",'u'},{"ú",'u'},{"û",'u'},{"ü",'u'},
            {"Ý",'Y'},{"ÿ",'y'},{"ý",'y'}}),
        accents_and_ligatures = utf32ch(al)

function normalise(string s)
    sequence utf32 = utf8_to_utf32(s)
    sequence res = {}
    integer i = 1, ch, prev
    for i=1 to length(utf32) do
        ch = utf32[i]
        if find(ch," \t\r\n\x0b\x0c") then
            if length(res)>0 and prev!=' ' then
                res &= -1
            end if
            prev = ' '
        elsif find(ch,"0123456789") then
            if length(res)=0 or prev!='0' then
                res &= ch-'0'
            else
                res[$] = res[$]*10+ch-'0'
            end if
            prev = '0'
        else
            object rep = find(ch,accents_and_ligatures)
            if rep then
                rep = lower(ac_replacements[rep])
            else
                rep = lower(ch)
            end if
            if length(res) and sequence(res[$]) then
                res[$] &= rep
            else
                res = append(res,""&rep)
            end if
            prev = ch
        end if
    end for
    for i=1 to length(common) do
        while 1 do
            integer k = find(common[i],res)
            if k=0 then exit end if
            res[k..k] = {}
            if length(res) and res[1]=-1 then
                res = res[2..$]
            end if
        end while
    end for
    if length(res) and prev=' ' then
        res = res[1..$-1]
    end if
    return res
end function

sequence tests = {
                  {"  leading spaces: 4",
                   "    leading spaces: 3",
                   "leading spaces: 2",
                   " leading spaces: 1"},
                  {"adjacent spaces: 3",
                   "adjacent  spaces: 4",
                   "adjacent   spaces: 1",
                   "adjacent    spaces: 2"},
                  {"white    space: 3-2",
                   "white\r  space: 3-3",
                   "white\x0cspace: 3-1",
                   "white\x0bspace: 3+0",
                   "white\n  space: 3+1",
                   "white\t  space: 3+2"},
                  {"caSE independent: 3-1",
                   "cASE independent: 3-2",
                   "casE independent: 3+0",
                   "case independent: 3+1"},
                  {"foo1000bar99baz9.txt",
                   "foo100bar99baz0.txt",
                   "foo100bar10baz0.txt",
                   "foo1000bar99baz10.txt"},
                  {"foo1bar",
                   "foo100bar",
                   "foo bar",
                   "foo1000bar"},
                  {"The Wind in the Willows",
                   "The 40th step more",
                   "The 39 steps",
                   "Wanda"},
                  {"ignore ý accents: 2-2",
                   "ignore Ý accents: 2-1",
                   "ignore y accents: 2+0",
                   "ignore Y accents: 2+1"},
                  {"Ball","Card","above","aether",
                   "apple","autumn","außen","bald",
                   "car","e-mail","evoke","nina",
                   "niño","Æon","Évian","æon"},
                 }

sequence s, n, t, tags

function natural(integer i, integer j)
    return compare(t[i],t[j])
end function

for i=1 to length(tests) do
    s = tests[i]
    n = sort(s)
    t = repeat(0,length(s))
    for j=1 to length(s) do
        t[j] = normalise(s[j])
    end for
    tags = custom_sort(routine_id("natural"),tagset(length(s)))
    if i=3 then -- clean up the whitespace mess
        for j=1 to length(s) do
            s[j] = substitute_all(s[j],{"\r","\x0c","\x0b","\n","\t"},{"\\r","\\x0c","\\x0b","\\n","\\t"})
            n[j] = substitute_all(n[j],{"\r","\x0c","\x0b","\n","\t"},{"\\r","\\x0c","\\x0b","\\n","\\t"})
        end for
    end if
    printf(1,"%-30s %-30s %-30s\n",{"original","normal","natural"})
    printf(1,"%-30s %-30s %-30s\n",{"
### ==
","======","
### =
"})
    for k=1 to length(tags) do
        printf(1,"%-30s|%-30s|%-30s\n",{s[k],n[k],s[tags[k]]})
    end for
    puts(1,"\n")
end for
```

```txt


C:\Program Files (x86)\Phix>p demo\rosetta\Natural_sorting.exw
original                       normal                         natural

### =====                       ======                         ====

  leading spaces: 4           |    leading spaces: 3         | leading spaces: 1
    leading spaces: 3         |  leading spaces: 4           |leading spaces: 2
leading spaces: 2             | leading spaces: 1            |    leading spaces: 3
 leading spaces: 1            |leading spaces: 2             |  leading spaces: 4

original                       normal                         natural

### =====                       ======                         ====

adjacent spaces: 3            |adjacent    spaces: 2         |adjacent   spaces: 1
adjacent  spaces: 4           |adjacent   spaces: 1          |adjacent    spaces: 2
adjacent   spaces: 1          |adjacent  spaces: 4           |adjacent spaces: 3
adjacent    spaces: 2         |adjacent spaces: 3            |adjacent  spaces: 4

original                       normal                         natural

### =====                       ======                         ====

white    space: 3-2           |white\t  space: 3+2           |white\x0bspace: 3+0
white\r  space: 3-3           |white\n  space: 3+1           |white\n  space: 3+1
white\x0cspace: 3-1           |white\x0bspace: 3+0           |white\t  space: 3+2
white\x0bspace: 3+0           |white\x0cspace: 3-1           |white\x0cspace: 3-1
white\n  space: 3+1           |white\r  space: 3-3           |white    space: 3-2
white\t  space: 3+2           |white    space: 3-2           |white\r  space: 3-3

original                       normal                         natural

### =====                       ======                         ====

caSE independent: 3-1         |cASE independent: 3-2         |casE independent: 3+0
cASE independent: 3-2         |caSE independent: 3-1         |case independent: 3+1
casE independent: 3+0         |casE independent: 3+0         |caSE independent: 3-1
case independent: 3+1         |case independent: 3+1         |cASE independent: 3-2

original                       normal                         natural

### =====                       ======                         ====

foo1000bar99baz9.txt          |foo1000bar99baz10.txt         |foo100bar10baz0.txt
foo100bar99baz0.txt           |foo1000bar99baz9.txt          |foo100bar99baz0.txt
foo100bar10baz0.txt           |foo100bar10baz0.txt           |foo1000bar99baz9.txt
foo1000bar99baz10.txt         |foo100bar99baz0.txt           |foo1000bar99baz10.txt

original                       normal                         natural

### =====                       ======                         ====

foo1bar                       |foo bar                       |foo bar
foo100bar                     |foo1000bar                    |foo1bar
foo bar                       |foo100bar                     |foo100bar
foo1000bar                    |foo1bar                       |foo1000bar

original                       normal                         natural

### =====                       ======                         ====

The Wind in the Willows       |The 39 steps                  |The 39 steps
The 40th step more            |The 40th step more            |The 40th step more
The 39 steps                  |The Wind in the Willows       |Wanda
Wanda                         |Wanda                         |The Wind in the Willows

original                       normal                         natural

### =====                       ======                         ====

ignore ý accents: 2-2         |ignore Ý accents: 2-1         |ignore y accents: 2+0
ignore Ý accents: 2-1         |ignore ý accents: 2-2         |ignore Y accents: 2+1
ignore y accents: 2+0         |ignore Y accents: 2+1         |ignore Ý accents: 2-1
ignore Y accents: 2+1         |ignore y accents: 2+0         |ignore ý accents: 2-2

original                       normal                         natural

### =====                       ======                         ====

Ball                          |Æon                           |above
Card                          |Évian                         |Æon
above                         |æon                           |æon
aether                        |Ball                          |aether
apple                         |Card                          |apple
autumn                        |above                         |außen
außen                         |aether                        |autumn
bald                          |apple                         |bald
car                           |außen                         |Ball
e-mail                        |autumn                        |car
evoke                         |bald                          |Card
nina                          |car                           |e-mail
niño                          |e-mail                        |Évian
Æon                           |evoke                         |evoke
Évian                         |niño                          |nina
æon                           |nina                          |niño

```



## PicoLisp

This parser takes care of features 1,2,3,4,5 and 8:

```PicoLisp
(de parseNatural (Str)
   (clip
      (make
         (for (L (chop Str)  L)
            (cond
               ((sp? (car L))
                  (link " ")
                  (while (and L (sp? (car L)))
                     (pop 'L) ) )
               ((>= "9" (car L) "0")
                  (link
                     (format
                        (make
                           (loop
                              (link (pop 'L))
                              (NIL (>= "9" (car L) "0")) ) ) ) ) )
               (T
                  (let Word
                     (pack
                        (replace
                           (make
                              (loop
                                 (link (lowc (pop 'L)))
                                 (NIL L)
                                 (T (sp? (car L)))
                                 (T (>= "9" (car L) "0")) ) )
                            "ß" "ss" "ſ" "s" "ʒ" "s" ) )
                     (unless (member Word '(the it to))
                        (link Word) ) ) ) ) ) ) ) )
```

Test:

```PicoLisp
: (parseNatural " ^MThe abc123Defß ^I Ghi ")
-> ("abc" 123 "defss" " " "ghi")
```

Sorting is trivial then:

```PicoLisp
(de naturalSort (Lst)
   (by parseNatural sort Lst) )
```

Test:

```PicoLisp
(de *TestData
   "# Ignoring leading spaces"
   ("ignore leading spaces: 2-2" " ignore leading spaces: 2-1" "  ignore leading spaces: 2+0" "   ignore leading spaces: 2+1")

   "# Ignoring multiple adjacent spaces (m.a.s)"
   ("ignore m.a.s spaces: 2-2" "ignore m.a.s  spaces: 2-1" "ignore m.a.s   spaces: 2+0" "ignore m.a.s    spaces: 2+1")

   "# Equivalent whitespace characters"
   ("Equiv. spaces: 3-3" "Equiv.^Mspaces: 3-2" "Equiv.^Acspaces: 3-1" "Equiv.^Kbspaces: 3+0" "Equiv.^Jspaces: 3+1" "Equiv.^Ispaces: 3+2")

   "# Case Indepenent sort"
   ("cASE INDEPENENT: 3-2" "caSE INDEPENENT: 3-1" "casE INDEPENENT: 3+0" "case INDEPENENT: 3+1")

   "# Numeric fields as numerics"
   ("foo100bar99baz0.txt" "foo100bar10baz0.txt" "foo1000bar99baz10.txt" "foo1000bar99baz9.txt")

   "# Title sorts"
   ("The Wind in the Willows" "The 40th step more" "The 39 steps" "Wanda")

   "# Equivalent accented characters (and case)"
   ("Equiv. ý accents: 2-2" "Equiv. Ý accents: 2-1" "Equiv. y accents: 2+0" "Equiv. Y accents: 2+1")

   # "Separated ligatures"
   ### ("Ĳ ligatured ij" "no ligature")

   "# Character replacements"
   ("Start with an ʒ: 2-2" "Start with an ſ: 2-1" "Start with an ß: 2+0" "Start with an s: 2+1") )

(de pythonOut (Ttl Lst)
   (prinl Ttl)
   (prin "['" (car Lst))
   (for S (cdr Lst)
      (prin "',^J '" S) )
   (prinl "']") )

(for X *TestData
   (if (atom X)
      (prinl X)
      (pythonOut "Text strings:" X)
      (pythonOut "Normally sorted :" (sort (copy X)))
      (pythonOut "Naturally sorted:" (naturalSort X))
      (prinl) ) )
```

Output:

```txt
# Ignoring leading spaces
Text strings:
['ignore leading spaces: 2-2',
 ' ignore leading spaces: 2-1',
 '  ignore leading spaces: 2+0',
 '   ignore leading spaces: 2+1']
Normally sorted :
['   ignore leading spaces: 2+1',
 '  ignore leading spaces: 2+0',
 ' ignore leading spaces: 2-1',
 'ignore leading spaces: 2-2']
Naturally sorted:
['  ignore leading spaces: 2+0',
 '   ignore leading spaces: 2+1',
 ' ignore leading spaces: 2-1',
 'ignore leading spaces: 2-2']

# Ignoring multiple adjacent spaces (m.a.s)
Text strings:
['ignore m.a.s spaces: 2-2',
 'ignore m.a.s  spaces: 2-1',
 'ignore m.a.s   spaces: 2+0',
 'ignore m.a.s    spaces: 2+1']
Normally sorted :
['ignore m.a.s    spaces: 2+1',
 'ignore m.a.s   spaces: 2+0',
 'ignore m.a.s  spaces: 2-1',
 'ignore m.a.s spaces: 2-2']
Naturally sorted:
['ignore m.a.s   spaces: 2+0',
 'ignore m.a.s    spaces: 2+1',
 'ignore m.a.s  spaces: 2-1',
 'ignore m.a.s spaces: 2-2']

# Equivalent whitespace characters
Text strings:
['Equiv. spaces: 3-3',
 'Equiv.
spaces: 3-2',
 'Equiv.�cspaces: 3-1',
 'Equiv.�bspaces: 3+0',
 'Equiv.
spaces: 3+1',
 'Equiv.	spaces: 3+2']
Normally sorted :
['Equiv.�cspaces: 3-1',
 'Equiv.	spaces: 3+2',
 'Equiv.
spaces: 3+1',
 'Equiv.�bspaces: 3+0',
 'Equiv.
spaces: 3-2',
 'Equiv. spaces: 3-3']
Naturally sorted:
['Equiv.�bspaces: 3+0',
 'Equiv.�cspaces: 3-1',
 'Equiv.
spaces: 3+1',
 'Equiv.	spaces: 3+2',
 'Equiv.
spaces: 3-2',
 'Equiv. spaces: 3-3']

# Case Indepenent sort
Text strings:
['cASE INDEPENENT: 3-2',
 'caSE INDEPENENT: 3-1',
 'casE INDEPENENT: 3+0',
 'case INDEPENENT: 3+1']
Normally sorted :
['cASE INDEPENENT: 3-2',
 'caSE INDEPENENT: 3-1',
 'casE INDEPENENT: 3+0',
 'case INDEPENENT: 3+1']
Naturally sorted:
['casE INDEPENENT: 3+0',
 'case INDEPENENT: 3+1',
 'caSE INDEPENENT: 3-1',
 'cASE INDEPENENT: 3-2']

# Numeric fields as numerics
Text strings:
['foo100bar99baz0.txt',
 'foo100bar10baz0.txt',
 'foo1000bar99baz10.txt',
 'foo1000bar99baz9.txt']
Normally sorted :
['foo1000bar99baz10.txt',
 'foo1000bar99baz9.txt',
 'foo100bar10baz0.txt',
 'foo100bar99baz0.txt']
Naturally sorted:
['foo100bar10baz0.txt',
 'foo100bar99baz0.txt',
 'foo1000bar99baz9.txt',
 'foo1000bar99baz10.txt']

# Title sorts
Text strings:
['The Wind in the Willows',
 'The 40th step more',
 'The 39 steps',
 'Wanda']
Normally sorted :
['The 39 steps',
 'The 40th step more',
 'The Wind in the Willows',
 'Wanda']
Naturally sorted:
['The 39 steps',
 'The 40th step more',
 'Wanda',
 'The Wind in the Willows']

# Equivalent accented characters (and case)
Text strings:
['Equiv. ý accents: 2-2',
 'Equiv. Ý accents: 2-1',
 'Equiv. y accents: 2+0',
 'Equiv. Y accents: 2+1']
Normally sorted :
['Equiv. Y accents: 2+1',
 'Equiv. y accents: 2+0',
 'Equiv. Ý accents: 2-1',
 'Equiv. ý accents: 2-2']
Naturally sorted:
['Equiv. y accents: 2+0',
 'Equiv. Y accents: 2+1',
 'Equiv. Ý accents: 2-1',
 'Equiv. ý accents: 2-2']

# Character replacements
Text strings:
['Start with an ʒ: 2-2',
 'Start with an ſ: 2-1',
 'Start with an ß: 2+0',
 'Start with an s: 2+1']
Normally sorted :
['Start with an s: 2+1',
 'Start with an ß: 2+0',
 'Start with an ſ: 2-1',
 'Start with an ʒ: 2-2']
Naturally sorted:
['Start with an s: 2+1',
 'Start with an ſ: 2-1',
 'Start with an ʒ: 2-2',
 'Start with an ß: 2+0']
```



## PowerShell


```powershell

# six sorting
$Discard = '^a ', '^an ', '^the '
$List =
   'ignore leading spaces: 2-2<==',
   ' ignore leading spaces: 2-1 <==',
   '  ignore leading spaces: 2+0  <==',
   '   ignore leading spaces: 2+1   <==',
   'ignore m.a.s spaces: 2-2<==',
   'ignore m.a.s  spaces: 2-1<==',
   'ignore m.a.s   spaces: 2+0<==',
   'ignore m.a.s    spaces: 2+1<==',
   'Equiv. spaces: 3-3<==',
   "Equiv.`rspaces: 3-2<==",
   "Equiv.`fspaces: 3-1<==",
   "Equiv.`vspaces: 3+0<==",
   "Equiv.`nspaces: 3+1<==",
   "Equiv.`tspaces: 3+2<==",
   'cASE INDEPENDENT: 3-2<==',
   'caSE INDEPENDENT: 3-1<==',
   'casE INDEPENDENT: 3+0<==',
   'case INDEPENDENT: 3+1<==',
   'Lâmpada accented characters<==',
   'Lúdico accented characters<==',
   '     Lula    Free   !!! accented characters<==',
   'Amanda accented characters<==',
   'Ágata accented characters<==',
   'Ångström accented characters<==',
   'Ângela accented characters<==',
   'À toa accented characters<==',
   'ânsia accented characters<==',
   'álibi accented characters<==',
   'foo100bar99baz0.txt<==',
   'foo100bar10baz0.txt<==',
   'foo1000bar99baz10.txt<==',
   'foo1000bar99baz9.txt<==',
   'The Wind in the Willows<==',
   'The 40th step more<==',
   'The 39 steps<==',
   'Wanda<=='

'List index sorting'
$List
' '
'Lexicographically sorting'
$List | Sort-Object
' '
'Natural sorting'
$List | Sort-Object -Property {
   [Regex]::Replace(
      (
         (
            & {
               If ($_.Trim() -match ($Discard -join '|')) {
                  $_ -replace '^\s*[^\s]+\s*'
               } Else {
                  $_.Trim()
               }
            }
         ) -replace '\s+'
      ), '\d+', { $args[0].Value.PadLeft(20) }
   )
}

```

```txt

List index sorting
ignore leading spaces: 2-2<==
 ignore leading spaces: 2-1 <==
  ignore leading spaces: 2+0  <==
   ignore leading spaces: 2+1   <==
ignore m.a.s spaces: 2-2<==
ignore m.a.s  spaces: 2-1<==
ignore m.a.s   spaces: 2+0<==
ignore m.a.s    spaces: 2+1<==
Equiv. spaces: 3-3<==
spaces: 3-2<==
Equiv.`fspaces: 3-1<==
Equiv.`vspaces: 3+0<==
Equiv.
spaces: 3+1<==
Equiv.  spaces: 3+2<==
cASE INDEPENDENT: 3-2<==
caSE INDEPENDENT: 3-1<==
casE INDEPENDENT: 3+0<==
case INDEPENDENT: 3+1<==
Lâmpada accented characters<==
Lúdico accented characters<==
     Lula    Free   !!! accented characters<==
Amanda accented characters<==
Ágata accented characters<==
Ångström accented characters<==
Ângela accented characters<==
À toa accented characters<==
ânsia accented characters<==
álibi accented characters<==
foo100bar99baz0.txt<==
foo100bar10baz0.txt<==
foo1000bar99baz10.txt<==
foo1000bar99baz9.txt<==
The Wind in the Willows<==
The 40th step more<==
The 39 steps<==
Wanda<==

Lexicographically sorting
     Lula    Free   !!! accented characters<==
   ignore leading spaces: 2+1   <==
  ignore leading spaces: 2+0  <==
 ignore leading spaces: 2-1 <==
À toa accented characters<==
Ágata accented characters<==
álibi accented characters<==
Amanda accented characters<==
Ângela accented characters<==
Ångström accented characters<==
ânsia accented characters<==
casE INDEPENDENT: 3+0<==
case INDEPENDENT: 3+1<==
caSE INDEPENDENT: 3-1<==
cASE INDEPENDENT: 3-2<==
Equiv. spaces: 3-3<==
Equiv.  spaces: 3+2<==
Equiv.
spaces: 3+1<==
Equiv.`vspaces: 3+0<==
Equiv.`fspaces: 3-1<==
spaces: 3-2<==
foo1000bar99baz10.txt<==
foo1000bar99baz9.txt<==
foo100bar10baz0.txt<==
foo100bar99baz0.txt<==
ignore leading spaces: 2-2<==
ignore m.a.s    spaces: 2+1<==
ignore m.a.s   spaces: 2+0<==
ignore m.a.s  spaces: 2-1<==
ignore m.a.s spaces: 2-2<==
Lâmpada accented characters<==
Lúdico accented characters<==
The 39 steps<==
The 40th step more<==
The Wind in the Willows<==
Wanda<==

Natural sorting
The 39 steps<==
The 40th step more<==
Ágata accented characters<==
álibi accented characters<==
Amanda accented characters<==
Ângela accented characters<==
Ångström accented characters<==
ânsia accented characters<==
À toa accented characters<==
caSE INDEPENDENT: 3-1<==
cASE INDEPENDENT: 3-2<==
casE INDEPENDENT: 3+0<==
case INDEPENDENT: 3+1<==
Equiv.`fspaces: 3-1<==
spaces: 3-2<==
Equiv. spaces: 3-3<==
Equiv.`vspaces: 3+0<==
Equiv.
spaces: 3+1<==
Equiv.  spaces: 3+2<==
foo100bar10baz0.txt<==
foo100bar99baz0.txt<==
foo1000bar99baz9.txt<==
foo1000bar99baz10.txt<==
 ignore leading spaces: 2-1 <==
ignore leading spaces: 2-2<==
  ignore leading spaces: 2+0  <==
   ignore leading spaces: 2+1   <==
ignore m.a.s  spaces: 2-1<==
ignore m.a.s spaces: 2-2<==
ignore m.a.s   spaces: 2+0<==
ignore m.a.s    spaces: 2+1<==
Lâmpada accented characters<==
Lúdico accented characters<==
     Lula    Free   !!! accented characters<==
Wanda<==
The Wind in the Willows<==

```



## Python

All eight features:

```python
# -*- coding: utf-8 -*-
# Not Python 3.x (Can't compare str and int)


from itertools import groupby
from unicodedata import decomposition, name
from pprint import pprint as pp

commonleaders = ['the'] # lowercase leading words to ignore
replacements = {u'ß': 'ss',  # Map single char to replacement string
                u'ſ': 's',
                u'ʒ': 's',
                }

hexdigits = set('0123456789abcdef')
decdigits = set('0123456789')   # Don't use str.isnumeric

def splitchar(c):
    ' De-ligature. De-accent a char'
    de = decomposition(c)
    if de:
        # Just the words that are also hex numbers
        de = [d for d in de.split()
                  if all(c.lower()
                         in hexdigits for c in d)]
        n = name(c, c).upper()
        # (Gosh it's onerous)
        if len(de)> 1 and 'PRECEDE' in n:
            # E.g. ŉ  LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
            de[1], de[0] = de[0], de[1]
        tmp = [ unichr(int(k, 16)) for k in de]
        base, others = tmp[0], tmp[1:]
        if 'LIGATURE' in n:
            # Assume two character ligature
            base += others.pop(0)
    else:
        base = c
    return base


def sortkeygen(s):
    '''Generate 'natural' sort key for s

    Doctests:
        >>> sortkeygen('  some extra    spaces  ')
        [u'some extra spaces']
        >>> sortkeygen('CasE InseNsItIve')
        [u'case insensitive']
        >>> sortkeygen('The Wind in the Willows')
        [u'wind in the willows']
        >>> sortkeygen(u'\462 ligature')
        [u'ij ligature']
        >>> sortkeygen(u'\335\375 upper/lower case Y with acute accent')
        [u'yy upper/lower case y with acute accent']
        >>> sortkeygen('foo9.txt')
        [u'foo', 9, u'.txt']
        >>> sortkeygen('x9y99')
        [u'x', 9, u'y', 99]
    '''
    # Ignore leading and trailing spaces
    s = unicode(s).strip()
    # All space types are equivalent
    s = ' '.join(s.split())
    # case insentsitive
    s = s.lower()
    # Title
    words = s.split()
    if len(words) > 1 and words[0] in commonleaders:
        s = ' '.join( words[1:])
    # accent and ligatures
    s = ''.join(splitchar(c) for c in s)
    # Replacements (single char replaced by one or more)
    s = ''.join( replacements.get(ch, ch) for ch in s )
    # Numeric sections as numerics
    s = [ int("".join(g)) if isinteger else "".join(g)
          for isinteger,g in groupby(s, lambda x: x in decdigits)]

    return s

def naturalsort(items):
    ''' Naturally sort a series of strings

    Doctests:
        >>> naturalsort(['The Wind in the Willows','The 40th step more',
                         'The 39 steps', 'Wanda'])
        ['The 39 steps', 'The 40th step more', 'Wanda', 'The Wind in the Willows']

    '''
    return sorted(items, key=sortkeygen)

if __name__ == '__main__':
    import string

    ns = naturalsort

    print '\n# Ignoring leading spaces'
    txt = ['%signore leading spaces: 2%+i' % (' '*i, i-2) for i in range(4)]
    print 'Text strings:'; pp(txt)
    print 'Normally sorted :'; pp(sorted(txt))
    print 'Naturally sorted:'; pp(ns(txt))

    print '\n# Ignoring multiple adjacent spaces (m.a.s)'
    txt = ['ignore m.a.s%s spaces: 2%+i' % (' '*i, i-2) for i in range(4)]
    print 'Text strings:'; pp(txt)
    print 'Normally sorted :'; pp(sorted(txt))
    print 'Naturally sorted:'; pp(ns(txt))

    print '\n# Equivalent whitespace characters'
    txt = ['Equiv.%sspaces: 3%+i' % (ch, i-3)
           for i,ch in enumerate(reversed(string.whitespace))]
    print 'Text strings:'; pp(txt)
    print 'Normally sorted :'; pp(sorted(txt))
    print 'Naturally sorted:'; pp(ns(txt))

    print '\n# Case Indepenent sort'
    s = 'CASE INDEPENENT'
    txt = [s[:i].lower() + s[i:] + ': 3%+i' % (i-3) for i in range(1,5)]
    print 'Text strings:'; pp(txt)
    print 'Normally sorted :'; pp(sorted(txt))
    print 'Naturally sorted:'; pp(ns(txt))

    print '\n# Numeric fields as numerics'
    txt = ['foo100bar99baz0.txt', 'foo100bar10baz0.txt',
           'foo1000bar99baz10.txt', 'foo1000bar99baz9.txt']
    print 'Text strings:'; pp(txt)
    print 'Normally sorted :'; pp(sorted(txt))
    print 'Naturally sorted:'; pp(ns(txt))

    print '\n# Title sorts'
    txt = ['The Wind in the Willows','The 40th step more',
                         'The 39 steps', 'Wanda']
    print 'Text strings:'; pp(txt)
    print 'Normally sorted :'; pp(sorted(txt))
    print 'Naturally sorted:'; pp(ns(txt))

    print '\n# Equivalent accented characters (and case)'
    txt = ['Equiv. %s accents: 2%+i' % (ch, i-2)
           for i,ch in enumerate(u'\xfd\xddyY')]
    print 'Text strings:'; pp(txt)
    print 'Normally sorted :'; pp(sorted(txt))
    print 'Naturally sorted:'; pp(ns(txt))

    print '\n# Separated ligatures'
    txt = [u'\462 ligatured ij', 'no ligature',]
    print 'Text strings:'; pp(txt)
    print 'Normally sorted :'; pp(sorted(txt))
    print 'Naturally sorted:'; pp(ns(txt))

    print '\n# Character replacements'
    s = u'ʒſßs' # u'\u0292\u017f\xdfs'
    txt = ['Start with an %s: 2%+i' % (ch, i-2)
           for i,ch in enumerate(s)]
    print 'Text strings:'; pp(txt)
    print 'Normally sorted :'; print '\n'.join(sorted(txt))
    print 'Naturally sorted:'; print '\n'.join(ns(txt))
```



### Sample Python output


```txt

# Ignoring leading spaces
Text strings:
['ignore leading spaces: 2-2',
 ' ignore leading spaces: 2-1',
 '  ignore leading spaces: 2+0',
 '   ignore leading spaces: 2+1']
Normally sorted :
['   ignore leading spaces: 2+1',
 '  ignore leading spaces: 2+0',
 ' ignore leading spaces: 2-1',
 'ignore leading spaces: 2-2']
Naturally sorted:
['  ignore leading spaces: 2+0',
 '   ignore leading spaces: 2+1',
 ' ignore leading spaces: 2-1',
 'ignore leading spaces: 2-2']

# Ignoring multiple adjacent spaces (m.a.s)
Text strings:
['ignore m.a.s spaces: 2-2',
 'ignore m.a.s  spaces: 2-1',
 'ignore m.a.s   spaces: 2+0',
 'ignore m.a.s    spaces: 2+1']
Normally sorted :
['ignore m.a.s    spaces: 2+1',
 'ignore m.a.s   spaces: 2+0',
 'ignore m.a.s  spaces: 2-1',
 'ignore m.a.s spaces: 2-2']
Naturally sorted:
['ignore m.a.s   spaces: 2+0',
 'ignore m.a.s    spaces: 2+1',
 'ignore m.a.s  spaces: 2-1',
 'ignore m.a.s spaces: 2-2']

# Equivalent whitespace characters
Text strings:
['Equiv. spaces: 3-3',
 'Equiv.\rspaces: 3-2',
 'Equiv.\x0cspaces: 3-1',
 'Equiv.\x0bspaces: 3+0',
 'Equiv.\nspaces: 3+1',
 'Equiv.\tspaces: 3+2']
Normally sorted :
['Equiv.\tspaces: 3+2',
 'Equiv.\nspaces: 3+1',
 'Equiv.\x0bspaces: 3+0',
 'Equiv.\x0cspaces: 3-1',
 'Equiv.\rspaces: 3-2',
 'Equiv. spaces: 3-3']
Naturally sorted:
['Equiv.\x0bspaces: 3+0',
 'Equiv.\nspaces: 3+1',
 'Equiv.\tspaces: 3+2',
 'Equiv.\x0cspaces: 3-1',
 'Equiv.\rspaces: 3-2',
 'Equiv. spaces: 3-3']

# Case Indepenent sort
Text strings:
['cASE INDEPENENT: 3-2',
 'caSE INDEPENENT: 3-1',
 'casE INDEPENENT: 3+0',
 'case INDEPENENT: 3+1']
Normally sorted :
['cASE INDEPENENT: 3-2',
 'caSE INDEPENENT: 3-1',
 'casE INDEPENENT: 3+0',
 'case INDEPENENT: 3+1']
Naturally sorted:
['casE INDEPENENT: 3+0',
 'case INDEPENENT: 3+1',
 'caSE INDEPENENT: 3-1',
 'cASE INDEPENENT: 3-2']

# Numeric fields as numerics
Text strings:
['foo100bar99baz0.txt',
 'foo100bar10baz0.txt',
 'foo1000bar99baz10.txt',
 'foo1000bar99baz9.txt']
Normally sorted :
['foo1000bar99baz10.txt',
 'foo1000bar99baz9.txt',
 'foo100bar10baz0.txt',
 'foo100bar99baz0.txt']
Naturally sorted:
['foo100bar10baz0.txt',
 'foo100bar99baz0.txt',
 'foo1000bar99baz9.txt',
 'foo1000bar99baz10.txt']

# Title sorts
Text strings:
['The Wind in the Willows', 'The 40th step more', 'The 39 steps', 'Wanda']
Normally sorted :
['The 39 steps', 'The 40th step more', 'The Wind in the Willows', 'Wanda']
Naturally sorted:
['The 39 steps', 'The 40th step more', 'Wanda', 'The Wind in the Willows']

# Equivalent accented characters (and case)
Text strings:
[u'Equiv. \xfd accents: 2-2',
 u'Equiv. \xdd accents: 2-1',
 u'Equiv. y accents: 2+0',
 u'Equiv. Y accents: 2+1']
Normally sorted :
[u'Equiv. Y accents: 2+1',
 u'Equiv. y accents: 2+0',
 u'Equiv. \xdd accents: 2-1',
 u'Equiv. \xfd accents: 2-2']
Naturally sorted:
[u'Equiv. y accents: 2+0',
 u'Equiv. Y accents: 2+1',
 u'Equiv. \xdd accents: 2-1',
 u'Equiv. \xfd accents: 2-2']

# Separated ligatures
Text strings:
[u'\u0132 ligatured ij', 'no ligature']
Normally sorted :
['no ligature', u'\u0132 ligatured ij']
Naturally sorted:
[u'\u0132 ligatured ij', 'no ligature']

# Character replacements
Text strings:
[u'Start with an \u0292: 2-2',
 u'Start with an \u017f: 2-1',
 u'Start with an \xdf: 2+0',
 u'Start with an s: 2+1']
Normally sorted :
Start with an s: 2+1
Start with an ß: 2+0
Start with an ſ: 2-1
Start with an ʒ: 2-2
Naturally sorted:
Start with an s: 2+1
Start with an ſ: 2-1
Start with an ʒ: 2-2
Start with an ß: 2+0
```



## Racket

Implements 1-4 (but only normalize spaces -- don't ignore spaces at the
beginning/end, easy to implement, but sounds wrong).


```racket

#lang racket
(define (natural-sort l)
  (define (list<? l1 l2)
    (cond [(null? l2) #f]
          [(null? l1) #t]
          [(number? (car l1)) (cond [(< (car l1) (car l2)) #t]
                                    [(< (car l2) (car l1)) #f]
                                    [else (list<? (cdr l1) (cdr l2))])]
          [(string? (car l1)) (cond [(string<? (car l1) (car l2)) #t]
                                    [(string<? (car l2) (car l1)) #f]
                                    [else (list<? (cdr l1) (cdr l2))])]))
  (define (->keys s)
    (define s* (string-normalize-spaces (string-foldcase s)))
    (for/list ([x (regexp-match* #px"\\d+" s* #:gap-select? #t)]
               [i (in-naturals)])
      (if (odd? i) (string->number x) x)))
  (sort l list<? #:key ->keys #:cache-keys? #t))

(natural-sort
 (shuffle '("foo9.txt" "foo10.txt" "x9y99" "x9y100" "x10y0" "x  z" "x y")))
;; => '("foo9.txt" "foo10.txt" "x9y99" "x9y100" "x10y0" "x y" "x  z")

```



## Ruby

Requirements 1,2,3 and 5 are met in one line of code:

```ruby
ar.sort_by{|str| str.downcase.gsub(/\Athe |\Aa |\Aan /, "").lstrip.gsub(/\s+/, " ")}
```

Almost all of the code below is handling requirement 4. The problem is that Ruby will happily sort ["a",1] against ["a",2] or even ["b"], but it does not know how to handle [1, "a"] against ["a", 2] and raises an ArgumentError. The code below does not define a new sort method, it defines a new class which is sortable by the existing method (falling back on string comparison).

```ruby
class NatSortString
  include Comparable
  attr_reader :scrubbed, :ints_and_strings, :i_s_pattern

  def initialize(str)
    @str = str
    @scrubbed = str.downcase.gsub(/\Athe |\Aa |\Aan /, "").lstrip.gsub(/\s+/, " ")
    @ints_and_strings = @scrubbed.scan(/\d+|\D+/).map{|s| s =~ /\d/ ? s.to_i : s}
    @i_s_pattern = @ints_and_strings.map{|el| el.is_a?(Integer) ? :i : :s}.join
  end

  def <=> (other)
    if i_s_pattern.start_with?(other.i_s_pattern) or other.i_s_pattern.start_with?(i_s_pattern) then
      ints_and_strings <=> other.ints_and_strings
    else
      scrubbed <=> other.scrubbed
    end
  end

  def to_s
    @str.dup
  end

end

```

Demo:

```ruby
tests =
  {"Ignoring leading spaces" =>
  [ "ignore leading spaces: 2-2 ",  " ignore leading spaces: 2-1 ",  "  ignore leading spaces: 2+0 ",  "   ignore leading spaces: 2+1 "],
  "Ignoring multiple adjacent spaces" =>
  [ "ignore m.a.s spaces: 2-2 ",  "ignore m.a.s  spaces: 2-1 ",  "ignore m.a.s   spaces: 2+0 ",  "ignore m.a.s    spaces: 2+1 "],
  "Equivalent whitespace characters" =>
  ["Equiv. spaces: 3-3", "Equiv.\rspaces: 3-2", "Equiv.\x0cspaces: 3-1", "Equiv.\x0bspaces: 3+0", "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2"],
  "Case Indepenent sort" =>
  [ "cASE INDEPENENT: 3-2 ",  "caSE INDEPENENT: 3-1 ",  "casE INDEPENENT: 3+0 ",  "case INDEPENENT: 3+1 "],
  "Numeric fields as numerics" =>
  [ "foo100bar99baz0.txt ",  "foo100bar10baz0.txt ",  "foo1000bar99baz10.txt ",  "foo1000bar99baz9.txt "],
  "Title sorts" =>
  [ "The Wind in the Willows ",  "The 40th step more ",  "The 39 steps ",  "Wanda "]}

tests.each do |title, ar|
  nat_sorts = ar.map{|s| NatSortString.new(s)}
  puts [title,"--input--", ar, "--normal sort--", ar.sort, "--natural sort--", nat_sorts.sort, "\n"]
end

```

```txt

Ignoring leading spaces
--input--
ignore leading spaces: 2-2
 ignore leading spaces: 2-1
  ignore leading spaces: 2+0
   ignore leading spaces: 2+1
--normal sort--
   ignore leading spaces: 2+1
  ignore leading spaces: 2+0
 ignore leading spaces: 2-1
ignore leading spaces: 2-2
--natural sort--
  ignore leading spaces: 2+0
   ignore leading spaces: 2+1
 ignore leading spaces: 2-1
ignore leading spaces: 2-2

Ignoring multiple adjacent spaces
--input--
ignore m.a.s spaces: 2-2
ignore m.a.s  spaces: 2-1
ignore m.a.s   spaces: 2+0
ignore m.a.s    spaces: 2+1
--normal sort--
ignore m.a.s    spaces: 2+1
ignore m.a.s   spaces: 2+0
ignore m.a.s  spaces: 2-1
ignore m.a.s spaces: 2-2
--natural sort--
ignore m.a.s   spaces: 2+0
ignore m.a.s    spaces: 2+1
ignore m.a.s  spaces: 2-1
ignore m.a.s spaces: 2-2

Equivalent whitespace characters
--input--
Equiv. spaces: 3-3
spaces: 3-2
Equiv.
      spaces: 3-1
Equiv.
      spaces: 3+0
Equiv.
spaces: 3+1
Equiv.	spaces: 3+2
--normal sort--
Equiv.	spaces: 3+2
Equiv.
spaces: 3+1
Equiv.
      spaces: 3+0
Equiv.
      spaces: 3-1
spaces: 3-2
Equiv. spaces: 3-3
--natural sort--
Equiv.
      spaces: 3+0
Equiv.
spaces: 3+1
Equiv.	spaces: 3+2
Equiv.
      spaces: 3-1
spaces: 3-2
Equiv. spaces: 3-3

Case Indepenent sort
--input--
cASE INDEPENENT: 3-2
caSE INDEPENENT: 3-1
casE INDEPENENT: 3+0
case INDEPENENT: 3+1
--normal sort--
cASE INDEPENENT: 3-2
caSE INDEPENENT: 3-1
casE INDEPENENT: 3+0
case INDEPENENT: 3+1
--natural sort--
casE INDEPENENT: 3+0
case INDEPENENT: 3+1
caSE INDEPENENT: 3-1
cASE INDEPENENT: 3-2

Numeric fields as numerics
--input--
foo100bar99baz0.txt
foo100bar10baz0.txt
foo1000bar99baz10.txt
foo1000bar99baz9.txt
--normal sort--
foo1000bar99baz10.txt
foo1000bar99baz9.txt
foo100bar10baz0.txt
foo100bar99baz0.txt
--natural sort--
foo100bar10baz0.txt
foo100bar99baz0.txt
foo1000bar99baz9.txt
foo1000bar99baz10.txt

Title sorts
--input--
The Wind in the Willows
The 40th step more
The 39 steps
Wanda
--normal sort--
The 39 steps
The 40th step more
The Wind in the Willows
Wanda
--natural sort--
The 39 steps
The 40th step more
Wanda
The Wind in the Willows

```



## Scala

All 8:

```Scala
object NaturalSorting {
  implicit object ArrayOrdering extends Ordering[Array[String]] { // 4
    val INT = "([0-9]+)".r
    def compare(a: Array[String], b: Array[String]) = {
      val l = Math.min(a.length, b.length)
      (0 until l).prefixLength(i => a(i) equals b(i)) match {
        case i if i == l => Math.signum(b.length - a.length).toInt
        case i => (a(i), b(i)) match {
          case (INT(c), INT(d)) => Math.signum(c.toInt - d.toInt).toInt
          case (c, d) => c compareTo d
        }
      }
    }
  }

  def natural(s: String) = {
    val replacements = Map('\u00df' -> "ss", '\u017f' -> "s", '\u0292' -> "s").withDefault(s => s.toString) // 8
    import java.text.Normalizer
    Normalizer.normalize(Normalizer.normalize(
      s.trim.toLowerCase, // 1.1, 1.2, 3
      Normalizer.Form.NFKC), // 7
      Normalizer.Form.NFD).replaceAll("[\\p{InCombiningDiacriticalMarks}]", "") // 6
     .replaceAll("^(the|a|an) ", "") // 5
     .flatMap(replacements.apply) // 8
     .split(s"\\s+|(?=[0-9])(?<=[^0-9])|(?=[^0-9])(?<=[0-9])") // 1.3, 2 and 4
  }
}

object NaturalSortingTest extends App {
  import NaturalSorting._

  val tests = List(
    ("1 Ignoring leading spaces", List("ignore leading spaces: 2-2", " ignore leading spaces: 2-1", "  ignore leading spaces: 2+0", "   ignore leading spaces: 2+1"), List("  ignore leading spaces: 2+0", "   ignore leading spaces: 2+1", " ignore leading spaces: 2-1", "ignore leading spaces: 2-2")),
    ("1 Ignoring multiple adjacent spaces (m.a.s)", List("ignore m.a.s spaces: 2-2", "ignore m.a.s  spaces: 2-1", "ignore m.a.s   spaces: 2+0", "ignore m.a.s  spaces: 2+1"), List("ignore m.a.s   spaces: 2+0", "ignore m.a.s  spaces: 2+1", "ignore m.a.s  spaces: 2-1", "ignore m.a.s spaces: 2-2")),
    ("2 Equivalent whitespace characters", List("Equiv. spaces: 3-3", "Equiv.\rspaces: 3-2", "Equiv.\u000cspaces: 3-1", "Equiv.\u000bspaces: 3+0", "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2"), List("Equiv.\u000bspaces: 3+0", "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2", "Equiv.\u000cspaces: 3-1", "Equiv.\rspaces: 3-2", "Equiv. spaces: 3-3")),
    ("3 Case Independent sort", List("cASE INDEPENENT: 3-2", "caSE INDEPENENT: 3-1", "casE INDEPENENT: 3+0", "case INDEPENENT: 3+1"), List("casE INDEPENENT: 3+0", "case INDEPENENT: 3+1", "caSE INDEPENENT: 3-1", "cASE INDEPENENT: 3-2")),
    ("4 Numeric fields as numerics", List("foo100bar99baz0.txt", "foo100bar10baz0.txt", "foo1000bar99baz10.txt", "foo1000bar99baz9.txt"), List("foo100bar10baz0.txt", "foo100bar99baz0.txt", "foo1000bar99baz9.txt", "foo1000bar99baz10.txt")),
    ("5 Title sorts", List("The Wind in the Willows", "The 40th step more", "The 39 steps", "Wanda"), List("The 39 steps", "The 40th step more", "Wanda", "The Wind in the Willows")),
    ("6 Equivalent accented characters (and case)", List("Equiv. \u00fd accents: 2-2", "Equiv. \u00dd accents: 2-1", "Equiv. y accents: 2+0", "Equiv. Y accents: 2+1"), List("Equiv. y accents: 2+0", "Equiv. Y accents: 2+1", "Equiv. \u00dd accents: 2-1", "Equiv. \u00fd accents: 2-2")),
    ("7 Separated ligatures", List("\u0132 ligatured ij", "no ligature"), List("\u0132 ligatured ij", "no ligature")),
    ("8 Character replacements", List("Start with an \u0292: 2-2", "Start with an \u017f: 2-1", "Start with an \u00df: 2+0", "Start with an s: 2+1"), List("Start with an s: 2+1", "Start with an \u017f: 2-1", "Start with an \u0292: 2-2", "Start with an \u00df: 2+0"))
  )

  val width = tests.flatMap(_._2).map(_.length).max
  assert(tests.forall{case (title, input, expected) =>
    val result = input.sortBy(natural)
    val okay = result == expected
    val label = if (okay) "pass" else "fail"
    println(s"$label: $title".toUpperCase)
    input.zip(result).foreach{case (a, b) => println(s"  ${a.padTo(width, ' ')}  |  ${b.padTo(width, ' ')}")}
    okay
  })
}
```

Output:

```txt
PASS: 1 IGNORING LEADING SPACES
  ignore leading spaces: 2-2     |    ignore leading spaces: 2+0
   ignore leading spaces: 2-1    |     ignore leading spaces: 2+1
    ignore leading spaces: 2+0   |   ignore leading spaces: 2-1
     ignore leading spaces: 2+1  |  ignore leading spaces: 2-2
PASS: 1 IGNORING MULTIPLE ADJACENT SPACES (M.A.S)
  ignore m.a.s spaces: 2-2       |  ignore m.a.s   spaces: 2+0
  ignore m.a.s  spaces: 2-1      |  ignore m.a.s  spaces: 2+1
  ignore m.a.s   spaces: 2+0     |  ignore m.a.s  spaces: 2-1
  ignore m.a.s  spaces: 2+1      |  ignore m.a.s spaces: 2-2
PASS: 2 EQUIVALENT WHITESPACE CHARACTERS
  Equiv. spaces: 3-3             |  Equiv.spaces: 3+0
spaces: 3-2             |  Equiv.
spaces: 3+1
  Equiv.spaces: 3-1             |  Equiv.       spaces: 3+2
  Equiv.spaces: 3+0             |  Equiv.spaces: 3-1
  Equiv.
spaces: 3-2             |  Equiv.
  Equiv.        spaces: 3+2             |  Equiv. spaces: 3-3
PASS: 3 CASE INDEPENDENT SORT
  cASE INDEPENENT: 3-2           |  casE INDEPENENT: 3+0
  caSE INDEPENENT: 3-1           |  case INDEPENENT: 3+1
  casE INDEPENENT: 3+0           |  caSE INDEPENENT: 3-1
  case INDEPENENT: 3+1           |  cASE INDEPENENT: 3-2
PASS: 4 NUMERIC FIELDS AS NUMERICS
  foo100bar99baz0.txt            |  foo100bar10baz0.txt
  foo100bar10baz0.txt            |  foo100bar99baz0.txt
  foo1000bar99baz10.txt          |  foo1000bar99baz9.txt
  foo1000bar99baz9.txt           |  foo1000bar99baz10.txt
PASS: 5 TITLE SORTS
  The Wind in the Willows        |  The 39 steps
  The 40th step more             |  The 40th step more
  The 39 steps                   |  Wanda
  Wanda                          |  The Wind in the Willows
PASS: 6 EQUIVALENT ACCENTED CHARACTERS (AND CASE)
  Equiv. ý accents: 2-2          |  Equiv. y accents: 2+0
  Equiv. Ý accents: 2-1          |  Equiv. Y accents: 2+1
  Equiv. y accents: 2+0          |  Equiv. Ý accents: 2-1
  Equiv. Y accents: 2+1          |  Equiv. ý accents: 2-2
PASS: 7 SEPARATED LIGATURES
  Ĳ ligatured ij                 |  Ĳ ligatured ij
  no ligature                    |  no ligature
PASS: 8 CHARACTER REPLACEMENTS
  Start with an ʒ: 2-2           |  Start with an s: 2+1
  Start with an ſ: 2-1           |  Start with an ſ: 2-1
  Start with an ß: 2+0           |  Start with an ʒ: 2-2
  Start with an s: 2+1           |  Start with an ß: 2+0
```



## Scheme


Tasks 1-5 are completed.


```scheme

(import (scheme base)
        (scheme char)
        (scheme write)
        (only (srfi 1) drop take-while)
        (only (srfi 13) string-drop string-join string-prefix-ci? string-tokenize)
        (srfi 132))


;; Natural sort function
(define (natural-sort lst)
  ; <1><2> ignores leading, trailing and multiple adjacent spaces
  ;        by tokenizing on whitespace (all whitespace characters),
  ;        and joining with a single space
  (define (ignore-spaces str)
    (string-join (string-tokenize str) " "))
  ; <5> Remove articles from string
  (define (drop-articles str)
    (define (do-drop articles str)
      (cond ((null? articles)
             str)
            ((string-prefix-ci? (car articles) str)
             (string-drop str (string-length (car articles))))
            (else
              (do-drop (cdr articles) str))))
    (do-drop '("a " "an " "the ") str))
  ; <4> split string into number/non-number groups
  (define (group-digits str)
    (let loop ((chars (string->list str))
               (doing-num? (char-numeric? (string-ref str 0)))
               (groups '()))
      (if (null? chars)
        (map (lambda (s) ; convert numbers to actual numbers
               (if (char-numeric? (string-ref s 0))
                 (string->number s)
                 s))
             (map list->string groups)) ; leave groups in reverse, as right-most significant
        (let ((next-group (take-while (if doing-num?
                                        char-numeric?
                                        (lambda (c) (not (char-numeric? c))))
                                      chars)))
          (loop (drop chars (length next-group))
                (not doing-num?)
                (cons next-group groups))))))
  ;
  (list-sort
    (lambda (a b) ; implements the numeric fields comparison <4>
      (let loop ((lft (group-digits (drop-articles (ignore-spaces a))))
                 (rgt (group-digits (drop-articles (ignore-spaces b)))))
        (cond ((null? lft) ; a is shorter
               #t)
              ((null? rgt) ; b is shorter
               #f)
              ((equal? (car lft) (car rgt)) ; if equal, look at next pair
               (loop (cdr lft) (cdr rgt)))
              ((and (number? (car lft)) ; compare as numbers
                    (number? (car rgt)))
               (< (car lft) (car rgt)))
              ((and (string? (car lft)) ; compare as strings
                    (string? (car rgt)))
               (string-ci<? (car lft) (car rgt))) ; <3> ignoring case
              ((and (number? (car lft)) ; strings before numbers
                    (string? (car rgt)))
               #f)
              ((and (string? (car lft)) ; strings before numbers
                    (number? (car rgt)))
               #t))))
    lst))

;; run string examples
(define (display-list title lst)
  (display title) (newline)
  (display "[\n") (for-each (lambda (i) (display i)(newline)) lst) (display "]\n"))

(for-each
  (lambda (title example)
    (display title) (newline)
    (display-list "Text strings:" example)
    (display-list "Normally sorted:" (list-sort string<? example))
    (display-list "Naturally sorted:" (natural-sort example))
    (newline))
  '("# Ignoring leading spaces" "# Ignoring multiple adjacent spaces (m.a.s.)"
    "# Equivalent whitespace characters" "# Case Independent sort"
    "# Numeric fields as numerics" "# Numeric fields as numerics - shows sorting from right"
    "# Title sorts")
  '(("ignore leading spaces: 2-2" " ignore leading spaces: 2-1" "  ignore leading spaces: 2+0" "   ignore leading spaces: 2+1")
    ("ignore m.a.s spaces: 2-2" "ignore m.a.s  spaces: 2-1" "ignore m.a.s   spaces: 2+0" "ignore m.a.s    spaces: 2+1")
    ("Equiv. spaces: 3-3" "Equiv.\rspaces: 3-2" "Equiv.\x0c;spaces: 3-1" "Equiv.\x0b;spaces: 3+0" "Equiv.\nspaces: 3+1" "Equiv.\tspaces: 3+2")
    ("cASE INDEPENDENT: 3-2" "caSE INDEPENDENT: 3-1" "casE INDEPENDENT: 3+0" "case INDEPENDENT: 3+1")
    ("foo100bar99baz0.txt" "foo100bar10baz0.txt" "foo1000bar99baz10.txt" "foo1000bar99baz9.txt")
    ("foo1bar99baz4.txt" "foo2bar99baz3.txt" "foo4bar99baz1.txt" "foo3bar99baz2.txt")
    ("The Wind in the Willows" "The 40th step more" "The 39 steps" "Wanda")))

```


Note that "Equivalent whitespace characters" shows the effect of the input character.
Also for "Equivalent whitespace characters", sorting includes numeric fields, which sorts from the right.


```txt

# Ignoring leading spaces
Text strings:
[
ignore leading spaces: 2-2
 ignore leading spaces: 2-1
  ignore leading spaces: 2+0
   ignore leading spaces: 2+1
]
Normally sorted:
[
   ignore leading spaces: 2+1
  ignore leading spaces: 2+0
 ignore leading spaces: 2-1
ignore leading spaces: 2-2
]
Naturally sorted:
[
  ignore leading spaces: 2+0
   ignore leading spaces: 2+1
 ignore leading spaces: 2-1
ignore leading spaces: 2-2
]

# Ignoring multiple adjacent spaces (m.a.s.)
Text strings:
[
ignore m.a.s spaces: 2-2
ignore m.a.s  spaces: 2-1
ignore m.a.s   spaces: 2+0
ignore m.a.s    spaces: 2+1
]
Normally sorted:
[
ignore m.a.s    spaces: 2+1
ignore m.a.s   spaces: 2+0
ignore m.a.s  spaces: 2-1
ignore m.a.s spaces: 2-2
]
Naturally sorted:
[
ignore m.a.s   spaces: 2+0
ignore m.a.s    spaces: 2+1
ignore m.a.s  spaces: 2-1
ignore m.a.s spaces: 2-2
]

# Equivalent whitespace characters
Text strings:
[
Equiv. spaces: 3-3
spaces: 3-2
Equiv.
      spaces: 3-1
Equiv.
      spaces: 3+0
Equiv.
spaces: 3+1
Equiv.	spaces: 3+2
]
Normally sorted:
[
Equiv.	spaces: 3+2
Equiv.
spaces: 3+1
Equiv.
      spaces: 3+0
Equiv.
      spaces: 3-1
spaces: 3-2
Equiv. spaces: 3-3
]
Naturally sorted:
[
Equiv.
      spaces: 3+0
Equiv.
spaces: 3+1
Equiv.
      spaces: 3-1
Equiv.	spaces: 3+2
spaces: 3-2
Equiv. spaces: 3-3
]

# Case Independent sort
Text strings:
[
cASE INDEPENDENT: 3-2
caSE INDEPENDENT: 3-1
casE INDEPENDENT: 3+0
case INDEPENDENT: 3+1
]
Normally sorted:
[
cASE INDEPENDENT: 3-2
caSE INDEPENDENT: 3-1
casE INDEPENDENT: 3+0
case INDEPENDENT: 3+1
]
Naturally sorted:
[
casE INDEPENDENT: 3+0
case INDEPENDENT: 3+1
caSE INDEPENDENT: 3-1
cASE INDEPENDENT: 3-2
]

# Numeric fields as numerics
Text strings:
[
foo100bar99baz0.txt
foo100bar10baz0.txt
foo1000bar99baz10.txt
foo1000bar99baz9.txt
]
Normally sorted:
[
foo1000bar99baz10.txt
foo1000bar99baz9.txt
foo100bar10baz0.txt
foo100bar99baz0.txt
]
Naturally sorted:
[
foo100bar10baz0.txt
foo100bar99baz0.txt
foo1000bar99baz9.txt
foo1000bar99baz10.txt
]

# Numeric fields as numerics - shows sorting from right
Text strings:
[
foo1bar99baz4.txt
foo2bar99baz3.txt
foo4bar99baz1.txt
foo3bar99baz2.txt
]
Normally sorted:
[
foo1bar99baz4.txt
foo2bar99baz3.txt
foo3bar99baz2.txt
foo4bar99baz1.txt
]
Naturally sorted:
[
foo4bar99baz1.txt
foo3bar99baz2.txt
foo2bar99baz3.txt
foo1bar99baz4.txt
]

# Title sorts
Text strings:
[
The Wind in the Willows
The 40th step more
The 39 steps
Wanda
]
Normally sorted:
[
The 39 steps
The 40th step more
The Wind in the Willows
Wanda
]
Naturally sorted:
[
The 39 steps
The 40th step more
Wanda
The Wind in the Willows
]

```



## Sidef

```ruby
class String {
    # Sort groups of digits in number order. Sort by order of magnitude then lexically.
    -> naturally { self.lc.gsub(/(\d+)/, {|s1| "0" + s1.len.chr + s1 }) + "\x0" + self };

    # Collapse multiple ws characters to a single.
    -> collapse { self.gsub(/(\s)\1+/, {|s1| s1 }) };

    # Convert all ws characters to a space.
    -> normalize { self.gsub(/(\s)/, ' ') };

    # Ignore common leading articles for title sorts
    -> title { self.sub(/^(?:a|an|the)\b\s*/i, '') };

    # Decompose ISO-Latin1 glyphs to their base character.
    -> latin1_decompose {
        static tr = Hash.new(%w(
           Æ AE æ ae Þ TH þ th Ð TH ð th ß ss À A Á A Â A Ã A Ä A Å A à a á a
            â a ã a ä a å a Ç C ç c È E É E Ê E Ë E è e é e ê e ë e Ì I Í I Î
            I Ï I ì i í i î i ï i Ò O Ó O Ô O Õ O Ö O Ø O ò o ó o ô o õ o ö o
            ø o Ñ N ñ n Ù U Ú U Û U Ü U ù u ú u û u ü u Ý Y ÿ y ý y
        )...);

        var re = Regex.new('(' + tr.keys.join('|') + ')');
        self.gsub(re, {|s1| tr{s1} });
    }
}
```


Tests:

```ruby
var tests = [
    [
        "Task 1a\nSort while ignoring leading spaces.",
        [
          'ignore leading spaces: 1', '   ignore leading spaces: 4',
          '  ignore leading spaces: 3', ' ignore leading spaces: 2'
        ],
        { .trim } # builtin method.
    ],
    [
        "Task 1b\nSort while ignoring multiple adjacent spaces.",
        [
          'ignore m.a.s   spaces: 3', 'ignore m.a.s spaces: 1',
          'ignore m.a.s    spaces: 4', 'ignore m.a.s  spaces: 2'
        ],
        { .collapse }
    ],
    [
        "Task 2\nSort with all white space normalized to regular spaces.",
        [
          "Normalized\tspaces: 4", "Normalized\xa0spaces: 1",
          "Normalized\x20spaces: 2", "Normalized\nspaces: 3"
        ],
        { .normalize }
    ],
    [
        "Task 3\nSort case independently.",
        [
          'caSE INDEPENDENT: 3', 'casE INDEPENDENT: 2',
          'cASE INDEPENDENT: 4', 'case INDEPENDENT: 1'
        ],
        { .lc } # builtin method
    ],
    [
        "Task 4\nSort groups of digits in natural number order.",
        %w(Foo100bar99baz0.txt foo100bar10baz0.txt foo1000bar99baz10.txt
           foo1000bar99baz9.txt 201st 32nd 3rd 144th 17th 2 95),
        { .naturally }
    ],
    [
        "Task 5 ( mixed with 1, 2, 3 & 4 )\n"
        + "Sort titles, normalize white space, collapse multiple spaces to\n"
        + "single, trim leading white space, ignore common leading articles\n"
        + 'and sort digit groups in natural order.',
        [
          'The Wind     in the Willows  8', '  The 39 Steps               3',
          'The    7th Seal              1', 'Wanda                        6',
          'A Fish Called Wanda          5', ' The Wind and the Lion       7',
          'Any Which Way But Loose      4', '12 Monkeys                   2'
        ],
        { .normalize.collapse.trim.title.naturally }
    ],
    [
        "Task 6, 7, 8\nMap letters in Latin1 that have accents or decompose to two\n"
        + 'characters to their base characters for sorting.',
        %w(apple Ball bald car Card above Æon æon aether
            niño nina e-mail Évian evoke außen autumn),
        { .latin1_decompose.naturally }
    ]
];

tests.each { |case|
    var code = case.pop;
    var array = case.pop;
    say case.pop+"\n";

    say "Standard Sort:\n";
    array.sort.each { .say };

    say "\nNatural Sort:\n";
    array.sort_by(code).each { .say };

    say "\n#{'*' * 40}\n";
}
```


```txt

Task 1a
Sort while ignoring leading spaces.

Standard Sort:

   ignore leading spaces: 4
  ignore leading spaces: 3
 ignore leading spaces: 2
ignore leading spaces: 1

Natural Sort:

ignore leading spaces: 1
 ignore leading spaces: 2
  ignore leading spaces: 3
   ignore leading spaces: 4

****************************************

Task 1b
Sort while ignoring multiple adjacent spaces.

Standard Sort:

ignore m.a.s    spaces: 4
ignore m.a.s   spaces: 3
ignore m.a.s  spaces: 2
ignore m.a.s spaces: 1

Natural Sort:

ignore m.a.s spaces: 1
ignore m.a.s  spaces: 2
ignore m.a.s   spaces: 3
ignore m.a.s    spaces: 4

****************************************

Task 2
Sort with all white space normalized to regular spaces.

Standard Sort:

Normalized	spaces: 4
Normalized
spaces: 3
Normalized spaces: 2
Normalized spaces: 1

Natural Sort:

Normalized spaces: 1
Normalized spaces: 2
Normalized
spaces: 3
Normalized	spaces: 4

****************************************

Task 3
Sort case independently.

Standard Sort:

cASE INDEPENDENT: 4
caSE INDEPENDENT: 3
casE INDEPENDENT: 2
case INDEPENDENT: 1

Natural Sort:

case INDEPENDENT: 1
casE INDEPENDENT: 2
caSE INDEPENDENT: 3
cASE INDEPENDENT: 4

****************************************

Task 4
Sort groups of digits in natural number order.

Standard Sort:

144th
17th
2
201st
32nd
3rd
95
Foo100bar99baz0.txt
foo1000bar99baz10.txt
foo1000bar99baz9.txt
foo100bar10baz0.txt

Natural Sort:

2
3rd
17th
32nd
95
144th
201st
foo100bar10baz0.txt
Foo100bar99baz0.txt
foo1000bar99baz9.txt
foo1000bar99baz10.txt

****************************************

Task 5 ( mixed with 1, 2, 3 & 4 )
Sort titles, normalize white space, collapse multiple spaces to
single, trim leading white space, ignore common leading articles
and sort digit groups in natural order.

Standard Sort:

  The 39 Steps               3
 The Wind and the Lion       7
12 Monkeys                   2
A Fish Called Wanda          5
Any Which Way But Loose      4
The    7th Seal              1
The Wind     in the Willows  8
Wanda                        6

Natural Sort:

The    7th Seal              1
12 Monkeys                   2
  The 39 Steps               3
Any Which Way But Loose      4
A Fish Called Wanda          5
Wanda                        6
 The Wind and the Lion       7
The Wind     in the Willows  8

****************************************

Task 6, 7, 8
Map letters in Latin1 that have accents or decompose to two
characters to their base characters for sorting.

Standard Sort:

Ball
Card
above
aether
apple
autumn
außen
bald
car
e-mail
evoke
nina
niño
Æon
Évian
æon

Natural Sort:

above
Æon
æon
aether
apple
außen
autumn
bald
Ball
car
Card
e-mail
Évian
evoke
nina
niño

****************************************

```



## Tcl

Tcl supports two methods of doing sorting by non-natural keys in the <code>lsort</code> command: the <tt>-command</tt> option allows the specification of code that makes the ordering decision for a pair of values, but instead the code below demonstrates sorting through the use of ''collation keys'', strings that when sorted in their normal order result in the natural order being used. (These are handled through the use of the <tt>-indices</tt> option which makes it easy to generate a sorted original list without any need to build compound intermediate tuples.)

Note also that Tcl supports case-insensitive sorting and “treat digit sequences as numbers” as native sorting options. (The latter is particularly useful for handling filenames.)

```tcl
package require Tcl 8.5

proc sortWithCollationKey {keyBuilder list} {
    if {![llength $list]} return
    foreach value $list {
	lappend toSort [{*}$keyBuilder $value]
    }
    foreach idx [lsort -indices $toSort] {
	lappend result [lindex $list $idx]
    }
    return $result
}
proc normalizeSpaces {str} {
    regsub -all {[ ]+} [string trim $str " "] " "
}
proc equivalentWhitespace {str} {
    regsub -all {\s} $str " "
}

proc show {description sorter strings} {
    puts "Input:\n\t[join $strings \n\t]"
    set sorted [lsort $strings]
    puts "Normally sorted:\n\t[join $sorted \n\t]"
    set sorted [{*}$sorter $strings]
    puts "Naturally sorted with ${description}:\n\t[join $sorted \n\t]"
}

# Two demonstrations of the space normalizer
show "normalized spaces" {sortWithCollationKey normalizeSpaces} {
    {ignore leading spaces: 2-2}
    { ignore leading spaces: 2-1}
    {  ignore leading spaces: 2+0}
    {   ignore leading spaces: 2+1}}
show "normalized spaces" {sortWithCollationKey normalizeSpaces} {
    {ignore m.a.s spaces: 2-2}
    {ignore m.a.s  spaces: 2-1}
    {ignore m.a.s   spaces: 2+0}
    {ignore m.a.s    spaces: 2+1}}

# Use a collation key that maps all whitespace to spaces
show "all whitespace equivalent" {sortWithCollationKey equivalentWhitespace} {
    "Equiv. spaces: 3-3"
    "Equiv.\rspaces: 3-2"
    "Equiv.\u000cspaces: 3-1"
    "Equiv.\u000bspaces: 3+0"
    "Equiv.\nspaces: 3+1"
    "Equiv.\tspaces: 3+2"}

# These are built-in modes
show "(built-in) case insensitivity" {lsort -nocase} {
    {cASE INDEPENENT: 3-2}
    {caSE INDEPENENT: 3-1}
    {casE INDEPENENT: 3+0}
    {case INDEPENENT: 3+1}}
show "digit sequences as numbers" {lsort -dictionary} {
    foo100bar99baz0.txt
    foo100bar10baz0.txt
    foo1000bar99baz10.txt
    foo1000bar99baz9.txt}
```

Output:

```txt

Input:
	ignore leading spaces: 2-2
	 ignore leading spaces: 2-1
	  ignore leading spaces: 2+0
	   ignore leading spaces: 2+1
Normally sorted:
	   ignore leading spaces: 2+1
	  ignore leading spaces: 2+0
	 ignore leading spaces: 2-1
	ignore leading spaces: 2-2
Naturally sorted with normalized spaces:
	  ignore leading spaces: 2+0
	   ignore leading spaces: 2+1
	 ignore leading spaces: 2-1
	ignore leading spaces: 2-2
Input:
	ignore m.a.s spaces: 2-2
	ignore m.a.s  spaces: 2-1
	ignore m.a.s   spaces: 2+0
	ignore m.a.s    spaces: 2+1
Normally sorted:
	ignore m.a.s    spaces: 2+1
	ignore m.a.s   spaces: 2+0
	ignore m.a.s  spaces: 2-1
	ignore m.a.s spaces: 2-2
Naturally sorted with normalized spaces:
	ignore m.a.s   spaces: 2+0
	ignore m.a.s    spaces: 2+1
	ignore m.a.s  spaces: 2-1
	ignore m.a.s spaces: 2-2
Input:
	Equiv. spaces: 3-3
spaces: 3-2iv.
	Equiv.
              spaces: 3-1
	Equiv.
              spaces: 3+0
	Equiv.
spaces: 3+1
	Equiv.	spaces: 3+2
Normally sorted:
	Equiv.	spaces: 3+2
	Equiv.
spaces: 3+1
	Equiv.
              spaces: 3+0
	Equiv.
              spaces: 3-1
spaces: 3-2iv.
	Equiv. spaces: 3-3
Naturally sorted with all whitespace equivalent:
	Equiv.
              spaces: 3+0
	Equiv.
spaces: 3+1
	Equiv.	spaces: 3+2
	Equiv.
              spaces: 3-1
spaces: 3-2iv.
	Equiv. spaces: 3-3
Input:
	cASE INDEPENENT: 3-2
	caSE INDEPENENT: 3-1
	casE INDEPENENT: 3+0
	case INDEPENENT: 3+1
Normally sorted:
	cASE INDEPENENT: 3-2
	caSE INDEPENENT: 3-1
	casE INDEPENENT: 3+0
	case INDEPENENT: 3+1
Naturally sorted with (built-in) case insensitivity:
	casE INDEPENENT: 3+0
	case INDEPENENT: 3+1
	caSE INDEPENENT: 3-1
	cASE INDEPENENT: 3-2
Input:
	foo100bar99baz0.txt
	foo100bar10baz0.txt
	foo1000bar99baz10.txt
	foo1000bar99baz9.txt
Normally sorted:
	foo1000bar99baz10.txt
	foo1000bar99baz9.txt
	foo100bar10baz0.txt
	foo100bar99baz0.txt
Naturally sorted with digit sequences as numbers:
	foo100bar10baz0.txt
	foo100bar99baz0.txt
	foo1000bar99baz9.txt
	foo1000bar99baz10.txt

```



## zkl

These are all immutable list sorts. If using mutable lists, mangle a copy and sort that.

First, a tag-mangled-fields sort, use tags to pull from the original list in sorted order function:

```zkl
fcn dsuSort(x,orig){ // decorate-sort-undecorate sort
   x.enumerate().sort(fcn([(_,a)],[(_,b)]){a<b})
   .apply('wrap([(n,_)]){orig[n]});
}
```

Now, mangle a copy of the original list to "normalize" it to the task requirements and sort.

```zkl
# Ignoring leading spaces
ts1:=T("ignore leading spaces: 2-2", " ignore leading spaces: 2-1",
  "  ignore leading spaces: 2+0", "   ignore leading spaces: 2+1");
dsuSort(ts1.apply("strip"),ts1).println();
```

```txt

"  ignore leading spaces: 2+0"
"   ignore leading spaces: 2+1"
" ignore leading spaces: 2-1"
"ignore leading spaces: 2-2"

```



```zkl
# Ignoring multiple adjacent spaces (m.a.s)
ts2:=T("ignore m.a.s spaces: 2-2", "ignore m.a.s  spaces: 2-1",
  "ignore m.a.s   spaces: 2+0", "ignore m.a.s    spaces: 2+1");
dsuSort(ts2.apply('-(" ")),ts2).println();
```

```txt

"ignore m.a.s   spaces: 2+0"
"ignore m.a.s    spaces: 2+1"
"ignore m.a.s  spaces: 2-1"
"ignore m.a.s spaces: 2-2"

```



```zkl
# Equivalent whitespace characters
ts3:=T("Equiv. spaces: 3-3", "Equiv.\rspaces: 3-2", "Equiv.\x0cspaces: 3-1",
       "Equiv.\x0bspaces: 3+0", "Equiv.\nspaces: 3+1", "Equiv.\tspaces: 3+2");
dsuSort(ts3.apply('-.fp1("\n\r\t\f\b\x0b ")),ts3).println();
```

```txt

"Equiv.?spaces: 3+0"
"Equiv.?spaces: 3+1"
"Equiv.	spaces: 3+2"
"Equiv.?spaces: 3-1"
"Equiv.?spaces: 3-2"
"Equiv. spaces: 3-3"

```



```zkl
# Case Indepenent sort
ts4:=T("cASE INDEPENENT: 3-2", "caSE INDEPENENT: 3-1",
       "casE INDEPENENT: 3+0", "case INDEPENENT: 3+1");
dsuSort(ts4.apply("toLower"),ts4).println();
```

```txt

"casE INDEPENENT: 3+0"
"case INDEPENENT: 3+1"
"caSE INDEPENENT: 3-1"
"cASE INDEPENENT: 3-2"

```


And the nasty one: multiple numeric fields. Each key is blown apart into a list of ints, where each int is consecutive numeric characters. To compare a pair of fields (ie two lists of ints), walk the two in parallel, stopping when a difference is found. Ugly but it works. Another tactic would be to normalize each field to the same length, concat the fields and use that as the mangled key. But that might have issues if the number of fields differ, the former doesn't care.

```zkl
# Numeric fields as numerics
fcn fieldize(s){
   s.apply(fcn(c){"1234567890".holds(c) and c or "."}).split(".")
   .filter().apply("toInt");
}
fcn fcmp(a,b){ // T(key, T(numeric fields)), eg L(0, L(100,99,0))
   a[1].zip(b[1]).reduce(fcn(_,[(a,b)]){
      if(a==b)return(True); // continue to next field
      return(Void.Stop,a<b);
   },True);
}
fcn fsort(list){
   list.apply(fieldize).enumerate().sort(fcmp)
   .apply('wrap([(n,_)]){list[n]});
}
```


```zkl
ts5:=T("foo100bar99baz0.txt", "foo100bar10baz0.txt", "foo1000bar99baz10.txt",
       "foo1000bar99baz9.txt");
fsort(ts5).println();

x:=T("x9y99","foo10.txt","x10y0","foo9.txt","x9y100");
fsort(x).println();
```

```txt

"foo100bar10baz0.txt"
"foo100bar99baz0.txt"
"foo1000bar99baz9.txt"
"foo1000bar99baz10.txt"

L("foo9.txt","x9y99","x9y100","x10y0","foo10.txt")

```

