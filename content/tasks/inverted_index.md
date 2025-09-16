+++
title = "Inverted index"
description = ""
date = 2018-12-09T07:11:35Z
aliases = []
[extra]
id = 7088
[taxonomies]
categories = ["task", "Classic CS problems and programs"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "bbc_basic",
  "c",
  "clojure",
  "coffeescript",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "echolisp",
  "erlang",
  "factor",
  "go",
  "haskell",
  "j",
  "java",
  "jq",
  "julia",
  "kotlin",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "powershell",
  "python",
  "racket",
  "rexx",
  "ruby",
  "scala",
  "tcl",
  "tuscript",
  "unix_shell",
]
+++

{{task|Classic CS problems and programs}}[[Category:Search]]

An [[wp:Inverted_index|Inverted Index]] is a data structure used to create full text search.


## Task

Given a set of text files, implement a program to create an inverted index.

Also create a user interface to do a search using that inverted index which returns a list of files that contain the query term / terms.

The search index  can be in memory.





## Ada



### Main program


Here is the main program (file inverted_index.adb):


```Ada
with Ada.Text_IO, Generic_Inverted_Index, Ada.Strings.Hash, Parse_Lines;
use Ada.Text_IO;

procedure Inverted_Index is

   type Process_Word is access procedure (Word: String);

   package Inv_Idx is new Generic_Inverted_Index
     (Source_Type => String,
      Item_Type   => String,
      Hash        => Ada.Strings.Hash);

   use Inv_Idx;

   procedure Output(Sources: Source_Vecs.Vector) is
      Any_Output: Boolean := False;

      procedure Print_Source(S: String) is
      begin
         if not Any_Output then -- this is the first source found
            Put("Found in the following files: ");
            Any_Output := True;
         else -- there has been at least one source before
            Put(", ");
         end if;
         Put(S);
      end Print_Source;

      procedure Print is new Inv_Idx.Iterate(Print_Source);

   begin
      Print(Sources);
      if not Any_Output then
         Put("I did not find this in any of the given files!");
      end if;
      New_Line(2);
   end Output;

   procedure Read_From_File(Table: in out Storage_Type;
                            Filename: String) is
      F: File_Type;

      procedure Enter_Word(S: String) is
      begin
         Table.Store(Source => Filename,  Item => S);
      end Enter_Word;

      procedure Store_Words is new
        Parse_Lines.Iterate_Words(Parse_Lines.Word_Pattern, Enter_Word);

   begin
      Open(File => F, Mode => In_File, Name => Filename);
       while not End_Of_File(F) loop
          Store_Words(Get_Line(F));
      end loop;
      Close(F);
   exception
      when others =>
         Put_Line("Something wrong with File '" & Filename & "'");
         Put_Line("I'll ignore this!");
   end Read_From_File;

   procedure Read_Files(Tab: out Storage_Type; Line: in String) is

      procedure Read_File(S: String) is
      begin
         Read_From_File(Tab, S);
      end Read_File;

      procedure Read_All is new
        Parse_Lines.Iterate_Words(Parse_Lines.Filename_Pattern, Read_File);

   begin
      Read_All(Line);
   end Read_Files;

   S: Storage_Type;
   Done: Boolean := False;

begin
   Put_Line("Enter Filenames:");
   Read_Files(S, Get_Line);
   New_Line;

   while not Done loop
      Put_Line("Enter one or more words to search for; <return> to finish:");
      declare
         Words: String := Get_Line;
         First: Boolean := True;
         Vec: Source_Vecs.Vector := Source_Vecs.Empty_Vector;

         procedure Compute_Vector(Item: String) is
         begin
            if First then
               Vec := S.Find(Item);
               First := False;
            else
               Vec := Vec and S.Find(Item);
            end if;
         end Compute_Vector;

         procedure Compute is new
           Parse_Lines.Iterate_Words(Parse_Lines.Word_Pattern, Compute_Vector);

      begin
         if Words = "" then
            Done := True;
         else
            Compute(Words);
            Output(Vec);
         end if;
      end;
   end loop;
end Inverted_Index;
```


A sample output:

```txt
Enter Filenames:
0.txt 1.txt 2.txt

Enter one or more words to search for; <return> to finish:
it
Found in the following files: 0.txt, 1.txt, 2.txt

Enter one or more words to search for; <return> to finish:
that
I did not find this in any of the given files!

Enter one or more words to search for; <return> to finish:
what is it
Found in the following files: 0.txt, 1.txt

Enter one or more words to search for; <return> to finish:

```


===Package Generic_Inverted_Index===

The real work is actually done in the package Generic_Inverted_Index. Here is the specification (file generic_inverted_index.ads):


```Ada
with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Indefinite_Hashed_Maps;

generic
   type Source_Type (<>) is private;
   type Item_Type (<>) is private;
   with function Hash(Item: Item_Type) return Ada.Containers.Hash_Type is <>;
package Generic_Inverted_Index is

   type Storage_Type is tagged private;

   package Source_Vecs is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Source_Type);

   procedure Store(Storage: in out Storage_Type;
                   Source: Source_Type;
                   Item: Item_Type);
   -- stores Source in a table, indexed by Item
   -- if there is already an Item/Source entry, the Table isn_t changed

   function Find(Storage: Storage_Type; Item: Item_Type)
                return Source_Vecs.Vector;
   -- Generates a vector of all Sources for the given Item

   function "and"(Left, Right: Source_Vecs.Vector) return Source_Vecs.Vector;
   -- returns a vector of all sources, which are both in Left and in Right

   function "or"(Left, Right: Source_Vecs.Vector) return Source_Vecs.Vector;
   -- returns a vector of all sources, which are in Left, Right, or both

   function Empty(Vec: Source_Vecs.Vector) return Boolean;
   -- returns true if Vec is empty

   function First_Source(The_Sources: Source_Vecs.Vector) return Source_Type;
   -- returns the first enty in The_Sources; pre: The_Sourses is not empty

   procedure Delete_First_Source(The_Sources: in out Source_Vecs.Vector;
                                 Count: Ada.Containers.Count_Type := 1);
   -- Removes the first Count entries; pre: The_Sourses has that many entries

   type Process_Source is not null access procedure (Source: Source_Type);

   generic
      with procedure Do_Something(Source: Source_Type);
   procedure Iterate(The_Sources: Source_Vecs.Vector);
   -- calls Do_Something(Source) for all sources in The_Sources;

private

   function Same_Vector(U,V: Source_Vecs.Vector) return Boolean;

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     -- for each item (=key) we store a vector with sources
     (Key_Type         => Item_Type,
      Element_Type     => Source_Vecs.Vector,
      Hash             => Hash,
      Equivalent_Keys  => "=",
      "="              => Same_Vector);

   type Storage_Type is new Maps.Map with null record;

end Generic_Inverted_Index;
```


Here is the implementation (generic_inverted_index.adb):


```Ada
package body Generic_Inverted_Index is

   use Source_Vecs;
   use type Maps.Cursor;


   procedure Store(Storage: in out Storage_Type;
                   Source: Source_Type;
                   Item: Item_Type) is
   begin
      if (Storage.Find(Item) = Maps.No_Element) then
         Storage.Insert(Key => Item,
                        New_Item => Empty_Vector & Source);
      else
         declare
            The_Vector: Vector := Storage.Element(Item);
         begin
            if The_Vector.Last_Element /= Source then
               Storage.Replace
                 (Key      => Item,
                  New_Item => Storage.Element(Item) & Source);
            end if;
         end;
      end if;
   end Store;

   function Find(Storage: Storage_Type; Item: Item_Type)
                return Vector is
   begin
      return Storage.Element(Item);
   exception
      when Constraint_Error => return Empty_Vector; -- found nothing
   end Find;

   function Is_In(S: Source_Type; V: Vector) return Boolean is
      VV: Vector := V;
   begin
      if Empty(V) then
         return False;
      elsif First_Source(V) = S then
         return True;
      else
         Delete_First_Source(VV);
         return Is_In(S, VV);
      end if;
   end Is_In;

   function "and"(Left, Right: Vector) return Vector is
       V: Vector := Empty_Vector;
   begin
      for I in First_Index(Left) .. Last_Index(Left) loop
         if Is_In(Element(Left, I), Right) then
            V := V & Element(Left, I);
         end if;
      end loop;
      return V;
   end "and";

   function "or"(Left, Right: Vector) return Vector is
       V: Vector := Left; -- all sources in Left
   begin -- ... add all sources in Right, which are not already in Left
      for I in First_Index(Right) .. Last_Index(Right) loop
         if not Is_In(Element(Right, I), Left) then
            V := V & Element(Right, I);
         end if;
      end loop;
      return V;
   end "or";

   function Empty(Vec: Vector) return Boolean
     renames Is_Empty;

   function First_Source(The_Sources: Vector)
                        return Source_Type renames First_Element;

   procedure Delete_First_Source(The_Sources: in out Vector;
                                 Count: Ada.Containers.Count_Type := 1)
     renames Delete_First;

   procedure Iterate(The_Sources: Vector) is
      V: Vector := The_Sources;
   begin
      while not Empty(V) loop
         Do_Something(First_Source(V));
         Delete_First_Source(V);
      end loop;
   end Iterate;

   function Same_Vector(U,V: Vector) return Boolean is
   begin
      raise Program_Error with "there is no need to call this function";
      return False; -- this avoices a compiler warning
   end Same_Vector;

end Generic_Inverted_Index;
```


===Package Parse_Lines===

The main program also uses an auxiliary package Parse_Lines. Note the usage of Gnat.Regpat, which itself is pattern matching package, specific for gnat/gcc. This package is derived from the [[Regular expressions#Ada|Ada implementation of the regular expressions task]]. Here is the spec (parse_lines.ads):


```Ada
with Gnat.Regpat;

package Parse_Lines is

   Word_Pattern: constant String := "([a-zA-Z]+)";
   Filename_Pattern: constant String := "([a-zA-Z0-9_.,;:]+)";

   procedure Search_For_Pattern(Pattern: Gnat.Regpat.Pattern_Matcher;
                                Search_In: String;
                                First, Last: out Positive;
                                Found: out Boolean);

   function Compile(Raw: String) return Gnat.Regpat.Pattern_Matcher;

   generic
      Pattern: String;
      with procedure Do_Something(Word: String);
   procedure Iterate_Words(S: String);

end Parse_Lines;
```


And here is the implementation (parse_lines.adb):


```Ada
with Gnat.Regpat;

package body Parse_Lines is

   procedure Search_For_Pattern(Pattern: Gnat.Regpat.Pattern_Matcher;
                                Search_In: String;
                                First, Last: out Positive;
                                Found: out Boolean) is
      use Gnat.Regpat;
      Result: Match_Array (0 .. 1);
   begin
      Match(Pattern, Search_In, Result);
      Found := Result(1) /= No_Match;
      if Found then
         First := Result(1).First;
         Last := Result(1).Last;
      end if;
   end Search_For_Pattern;

   function Compile(Raw: String) return Gnat.Regpat.Pattern_Matcher is
   begin
      return Gnat.Regpat.Compile(Raw);
   end Compile;

      procedure Iterate_Words(S: String) is
      Current_First: Positive := S'First;
      First, Last:   Positive;
      Found:         Boolean;
      use Parse_Lines;
      Compiled_P: Gnat.Regpat.Pattern_Matcher := Compile(Pattern);
   begin
      loop
         Search_For_Pattern(Compiled_P,
                            S(Current_First .. S'Last),
                            First, Last, Found);
         exit when not Found;
         Do_Something(S(First .. Last));
         Current_First := Last+1;
      end loop;
   end Iterate_Words;

end Parse_Lines;
```


===Alternative Implementation of Generic_Inverted_Index (Ada 2012)===

The new standard Ada 2012 simplifies the usage of containers significantly. The following runs under gnat (GNAT GPL 2011 (20110419)), when using the experimental -gnat2012 switch. The main program is the same. Here is the spec for Generic_Inverted_Index:


```Ada
with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Indefinite_Hashed_Maps;

generic
   type Source_Type (<>) is private;
   type Item_Type (<>) is private;
   with function Hash(Item: Item_Type) return Ada.Containers.Hash_Type is <>;
package Generic_Inverted_Index is

   type Storage_Type is tagged private;

   package Source_Vecs is new Ada.Containers.Indefinite_Vectors
     (Index_Type   => Positive,
      Element_Type => Source_Type);

   procedure Store(Storage: in out Storage_Type;
                   Source: Source_Type;
                   Item: Item_Type);
   -- stores Source in a table, indexed by Item
   -- if there is already an Item/Source entry, the Table isn_t changed

   function Find(Storage: Storage_Type; Item: Item_Type)
                return Source_Vecs.Vector;
   -- Generates a vector of all Sources for the given Item

   function "and"(Left, Right: Source_Vecs.Vector) return Source_Vecs.Vector;
   -- returns a vector of all sources, which are both in Left and in Right

   function "or"(Left, Right: Source_Vecs.Vector) return Source_Vecs.Vector;
   -- returns a vector of all sources, which are in Left, Right, or both

   function Empty(Vec: Source_Vecs.Vector) return Boolean;
   -- returns true if Vec is empty

   type Process_Source is not null access procedure (Source: Source_Type);

   generic
      with procedure Do_Something(Source: Source_Type);
   procedure Iterate(The_Sources: Source_Vecs.Vector);
   -- calls Do_Something(Source) for all sources in The_Sources;

private

   function Same_Vector(U,V: Source_Vecs.Vector) return Boolean;

   package Maps is new Ada.Containers.Indefinite_Hashed_Maps
     -- for each item (=key) we store a vector with sources
     (Key_Type         => Item_Type,
      Element_Type     => Source_Vecs.Vector,
      Hash             => Hash,
      Equivalent_Keys  => "=",
      "="              => Same_Vector);

   type Storage_Type is new Maps.Map with null record;

end Generic_Inverted_Index;
```


The implementation:


```Ada
package body Generic_Inverted_Index is
             -- uses some of the new Ada 2012 syntax
   use Source_Vecs;

   procedure Store(Storage: in out Storage_Type;
                   Source: Source_Type;
                   Item: Item_Type) is
      use type Maps.Cursor;
   begin
      if (Storage.Find(Item) = Maps.No_Element) then
         Storage.Insert(Key => Item,
                        New_Item => Empty_Vector & Source);
      else
         declare
            The_Vector: Vector := Storage.Element(Item);
         begin
            if The_Vector.Last_Element /= Source then
               Storage.Replace
                 (Key      => Item,
                  New_Item => Storage.Element(Item) & Source);
            end if;
         end;
      end if;
   end Store;

   function Find(Storage: Storage_Type; Item: Item_Type)
                return Vector is
   begin
      return Storage.Element(Item);
   exception
      when Constraint_Error => return Empty_Vector; -- found nothing
   end Find;

   function Is_In(S: Source_Type; V: Vector) return Boolean is
   begin
      for Some_Element of V loop
         if Some_Element = S then
            return True;
         end if;
      end loop;
      return False;
   end Is_In;

   function "and"(Left, Right: Vector) return Vector is
       V: Vector := Empty_Vector;
   begin
      for Some_Element of Left loop
         if Is_In(Some_Element, Right) then
            V := V & Some_Element;
         end if;
      end loop;
      return V;
   end "and";

   function "or"(Left, Right: Vector) return Vector is
       V: Vector := Left; -- all sources in Left
   begin
      for Some_Element of Right loop
         if not Is_In(Some_Element, Left) then
            V := V & Some_Element;
         end if;
      end loop;
      return V;
   end "or";

   function Empty(Vec: Vector) return Boolean
     renames Is_Empty;

   procedure Iterate(The_Sources: Vector) is
   begin
      for Some_Element in The_Sources loop
         Do_Something(Element(Some_Element));
      end loop;
   end Iterate;

   function Same_Vector(U,V: Vector) return Boolean is
   begin
      raise Program_Error with "there is no need to call this function";
      return False; -- this avoices a compiler warning
   end Same_Vector;

end Generic_Inverted_Index;
```



## AutoHotkey

```AutoHotkey
; http://www.autohotkey.com/forum/viewtopic.php?t=41479
inputbox, files, files, file pattern such as c:\files\*.txt

word2docs := object() ; autohotkey_L is needed.

stime := A_tickcount
Loop, %files%, 0,1
{
   tooltip,%A_index%  / 500

   wordList := WordsIn(A_LoopFileFullPath)
   InvertedIndex(wordList, A_loopFileFullpath)
}

tooltip
msgbox, % "total time " (A_tickcount-stime)/1000

gosub, search
return

search:
Loop
{
   InputBox, keyword , input single keyword only
   msgbox, % foundDocs := findword(keyword)
}
return

WordsIn(docpath)
{
   FileRead, content, %docpath%
  spos = 1
   Loop
   {
     if !(spos := Regexmatch(content, "[a-zA-Z]{2,}",match, spos))
       break
     spos += strlen(match)
     this_wordList .= match "`n"
   }

  Sort, this_wordList, U
  return this_wordList
}

InvertedIndex(byref words, docpath)
{
   global word2docs

  loop, parse, words, `n,`r
  {
    if A_loopField =
      continue
    word2docs[A_loopField] := word2docs[A_loopField] docpath "`n"
  }
}

findWord(word2find)
{
  global word2docs

  if (word2docs[word2find] = "")
     return ""
  else
    return word2docs[word2find]
}
```



## BBC BASIC

This uses a hashed index and linked lists to hold the file numbers.

```bbcbasic
      DIM FileList$(4)
      FileList$() = "BBCKEY0.TXT", "BBCKEY1.TXT", "BBCKEY2.TXT", \
      \             "BBCKEY3.TXT", "BBCKEY4.TXT"

      DictSize% = 30000
      DIM Index{(DictSize%-1) word$, link%}

      REM Build inverted index:
      FOR file% = DIM(FileList$(),1) TO 0 STEP -1
        filename$ = FileList$(file%)
        F% = OPENIN(filename$)
        IF F% = 0 ERROR 100, "Failed to open file"

        WHILE NOT EOF#F%
          REPEAT C%=BGET#F% : UNTIL C%>64 OR EOF#F% : word$ = CHR$(C%)
          REPEAT C%=BGET#F% : word$ += CHR$(C%) : UNTIL C%<65
          word$ = FNlower(LEFT$(word$))

          hash% = FNhash(word$)
          WHILE Index{(hash%)}.word$<>"" AND Index{(hash%)}.word$<>word$
            hash% = (hash% + 1) MOD DictSize% : REM Collision
          ENDWHILE
          Index{(hash%)}.word$ = word$
          link% = Index{(hash%)}.link%
          IF link%=0 OR link%!4<>file% THEN
            DIM heap% 7 : heap%!4 = file%
            !heap% = link%
            Index{(hash%)}.link% = heap% : REM Linked list
          ENDIF
        ENDWHILE

        CLOSE #F%
      NEXT file%

      REM Now query the index:
      PRINT FNquery("random")
      PRINT FNquery("envelope")
      PRINT FNquery("zebra")
      PRINT FNquery("the")
      END

      DEF FNquery(A$)
      LOCAL hash%, link%, temp%
      A$ = FNlower(A$)
      hash% = FNhash(A$)
      temp% = hash%
      WHILE Index{(hash%)}.word$ <> A$
        hash% = (hash% + 1) MOD DictSize%
        IF hash% = temp% THEN = """" + A$ + """ not found"
      ENDWHILE
      link% = Index{(hash%)}.link%
      A$ = """" + A$ + """ found in "
      WHILE link%
        A$ += FileList$(link%!4) + ", "
        link% = !link%
      ENDWHILE
      = LEFT$(LEFT$(A$))

      DEF FNhash(A$)
      LOCAL hash%
      IF LEN(A$) < 4 A$ += STRING$(4-LEN(A$),CHR$0)
      hash% = !!^A$
      IF LEN(A$) > 4 hash% EOR= !(!^A$ + LEN(A$) - 4)
      = hash% MOD DictSize%

      DEF FNlower(A$)
      LOCAL A%,C%
      FOR A% = 1 TO LEN(A$)
        C% = ASCMID$(A$,A%)
        IF C% >= 65 IF C% <= 90 MID$(A$,A%,1) = CHR$(C%+32)
      NEXT
      = A$
```

'''Output:'''

```txt

"random" found in BBCKEY2.TXT, BBCKEY3.TXT, BBCKEY4.TXT
"envelope" found in BBCKEY1.TXT, BBCKEY4.TXT
"zebra" not found
"the" found in BBCKEY0.TXT, BBCKEY1.TXT, BBCKEY2.TXT, BBCKEY3.TXT, BBCKEY4.TXT

```



## C

The code is stupidly long, having to implement a Trie to store strings and all -- the program doesn't do anything shiny, but Tries may be interesting to look at.

```c
#include <stdio.h>
#include <stdlib.h>

char chr_legal[] = "abcdefghijklmnopqrstuvwxyz0123456789_-./";
int  chr_idx[256] = {0};
char idx_chr[256] = {0};

#define FNAME 0
typedef struct trie_t *trie, trie_t;
struct trie_t {
	trie next[sizeof(chr_legal)]; /* next letter; slot 0 is for file name */
	int eow;
};

trie trie_new() { return calloc(sizeof(trie_t), 1); }

#define find_word(r, w) trie_trav(r, w, 1)
/* tree traversal: returns node if end of word and matches string, optionally
 * create node if doesn't exist
 */
trie trie_trav(trie root, const char * str, int no_create)
{
	int c;
	while (root) {
		if ((c = str[0]) == '\0') {
			if (!root->eow && no_create) return 0;
			break;
		}
		if (! (c = chr_idx[c]) ) {
			str++;
			continue;
		}

		if (!root->next[c]) {
			if (no_create) return 0;
			root->next[c] = trie_new();
		}
		root = root->next[c];
		str++;
	}
	return root;
}

/*  complete traversal of whole tree, calling callback at each end of word node.
 *  similar method can be used to free nodes, had we wanted to do that.
 */
int trie_all(trie root, char path[], int depth, int (*callback)(char *))
{
	int i;
	if (root->eow && !callback(path)) return 0;

	for (i = 1; i < sizeof(chr_legal); i++) {
		if (!root->next[i]) continue;

		path[depth] = idx_chr[i];
		path[depth + 1] = '\0';
		if (!trie_all(root->next[i], path, depth + 1, callback))
			return 0;
	}
	return 1;
}

void add_index(trie root, const char *word, const char *fname)
{
	trie x = trie_trav(root, word, 0);
	x->eow = 1;

	if (!x->next[FNAME])
		x->next[FNAME] = trie_new();
	x = trie_trav(x->next[FNAME], fname, 0);
	x->eow = 1;
}

int print_path(char *path)
{
	printf(" %s", path);
	return 1;
}

/*  pretend we parsed text files and got lower cased words: dealing     *
 *  with text file is a whole other animal and would make code too long */
const char *files[] = { "f1.txt", "source/f2.txt", "other_file" };
const char *text[][5] ={{ "it", "is", "what", "it", "is" },
		        { "what", "is", "it", 0 },
		        { "it", "is", "a", "banana", 0 }};

trie init_tables()
{
	int i, j;
	trie root = trie_new();
	for (i = 0; i < sizeof(chr_legal); i++) {
		chr_idx[(int)chr_legal[i]] = i + 1;
		idx_chr[i + 1] = chr_legal[i];
	}

/* Enable USE_ADVANCED_FILE_HANDLING to use advanced file handling.
 * You need to have files named like above files[], with words in them
 * like in text[][].  Case doesn't matter (told you it's advanced).
 */
#define USE_ADVANCED_FILE_HANDLING 0
#if USE_ADVANCED_FILE_HANDLING
	void read_file(const char * fname) {
		char cmd[1024];
		char word[1024];
		sprintf(cmd, "perl -p -e 'while(/(\\w+)/g) {print lc($1),\"\\n\"}' %s", fname);
		FILE *in = popen(cmd, "r");
		while (!feof(in)) {
			fscanf(in, "%1000s", word);
			add_index(root, word, fname);
		}
		pclose(in);
	};

	read_file("f1.txt");
	read_file("source/f2.txt");
	read_file("other_file");
#else
	for (i = 0; i < 3; i++) {
		for (j = 0; j < 5; j++) {
			if (!text[i][j]) break;
			add_index(root, text[i][j], files[i]);
		}
	}
#endif /*USE_ADVANCED_FILE_HANDLING*/

	return root;
}

void search_index(trie root, const char *word)
{
	char path[1024];
	printf("Search for \"%s\": ", word);
	trie found = find_word(root, word);

	if (!found) printf("not found\n");
	else {
		trie_all(found->next[FNAME], path, 0, print_path);
		printf("\n");
	}
}

int main()
{
	trie root = init_tables();

	search_index(root, "what");
	search_index(root, "is");
	search_index(root, "banana");
	search_index(root, "boo");
	return 0;
}
```
Output:<lang>Search for "what":  f1.txt source/f2.txt
Search for "is":  f1.txt other_file source/f2.txt
Search for "banana":  other_file
Search for "boo": not found
```


## C++

Same idea as the C implementation - trie to store the words

```cpp

#include <algorithm>
#include <fstream>
#include <iostream>
#include <vector>
#include <string>

const std::string _CHARS = "abcdefghijklmnopqrstuvwxyz0123456789.:-_/";
const size_t MAX_NODES = 41;

class node
{
public:
    node() { clear(); }
    node( char z ) { clear(); }
    ~node() { for( int x = 0; x < MAX_NODES; x++ ) if( next[x] ) delete next[x]; }
    void clear() { for( int x = 0; x < MAX_NODES; x++ ) next[x] = 0; isWord = false; }
    bool isWord;
    std::vector<std::string> files;
    node* next[MAX_NODES];
};

class index {
public:
    void add( std::string s, std::string fileName ) {
        std::transform( s.begin(), s.end(), s.begin(), tolower );
        std::string h;
        for( std::string::iterator i = s.begin(); i != s.end(); i++ ) {
            if( *i == 32 ) {
                pushFileName( addWord( h ), fileName );
                h.clear();
                continue;
            }
            h.append( 1, *i );
        }
        if( h.length() )
            pushFileName( addWord( h ), fileName );
    }
    void findWord( std::string s ) {
        std::vector<std::string> v = find( s );
        if( !v.size() ) {
            std::cout << s + " was not found!\n";
            return;
        }
        std::cout << s << " found in:\n";
        for( std::vector<std::string>::iterator i = v.begin(); i != v.end(); i++ ) {
            std::cout << *i << "\n";
        }
        std::cout << "\n";
    }
private:
    void pushFileName( node* n, std::string fn ) {
        std::vector<std::string>::iterator i = std::find( n->files.begin(), n->files.end(), fn );
        if( i == n->files.end() ) n->files.push_back( fn );
    }
    const std::vector<std::string>& find( std::string s ) {
        size_t idx;
        std::transform( s.begin(), s.end(), s.begin(), tolower );
        node* rt = &root;
        for( std::string::iterator i = s.begin(); i != s.end(); i++ ) {
            idx = _CHARS.find( *i );
            if( idx < MAX_NODES ) {
                if( !rt->next[idx] ) return std::vector<std::string>();
                rt = rt->next[idx];
            }
        }
        if( rt->isWord ) return rt->files;
        return std::vector<std::string>();
    }
    node* addWord( std::string s ) {
        size_t idx;
        node* rt = &root, *n;
        for( std::string::iterator i = s.begin(); i != s.end(); i++ ) {
            idx = _CHARS.find( *i );
            if( idx < MAX_NODES ) {
                n = rt->next[idx];
                if( n ){
                    rt = n;
                    continue;
                }
                n = new node( *i );
                rt->next[idx] = n;
                rt = n;
            }
        }
        rt->isWord = true;
        return rt;
    }
    node root;
};
int main( int argc, char* argv[] ) {
    index t;
    std::string s;
    std::string files[] = { "file1.txt", "f_text.txt", "text_1b.txt" };

    for( int x = 0; x < 3; x++ ) {
        std::ifstream f;
        f.open( files[x].c_str(), std::ios::in );
        if( f.good() ) {
            while( !f.eof() ) {
                f >> s;
                t.add( s, files[x] );
                s.clear();
            }
            f.close();
        }
    }

    while( true ) {
        std::cout << "Enter one word to search for, return to exit: ";
        std::getline( std::cin, s );
        if( !s.length() ) break;
        t.findWord( s );

    }
    return 0;
}

```

```txt

Enter one word to search for, return to exit: goodness
goodness found in:
file1.txt
f_text.txt

Enter one word to search for, return to exit: because
because found in:
f_text.txt

Enter one word to search for, return to exit: her
her found in:
text_1b.txt

Enter one word to search for, return to exit: fat
fat was not found!

```


## C#

```c#
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

class InvertedIndex
{
    static Dictionary<TItem, IEnumerable<TKey>> Invert<TKey, TItem>(Dictionary<TKey, IEnumerable<TItem>> dictionary)
    {
        return dictionary
            .SelectMany(keyValuePair => keyValuePair.Value.Select(item => new KeyValuePair<TItem, TKey>(item, keyValuePair.Key)))
            .GroupBy(keyValuePair => keyValuePair.Key)
            .ToDictionary(group => group.Key, group => group.Select(keyValuePair => keyValuePair.Value));
    }

    static void Main()
    {
        Console.Write("files: ");
        var files = Console.ReadLine();
        Console.Write("find: ");
        var find = Console.ReadLine();
        var dictionary = files.Split().ToDictionary(file => file, file => File.ReadAllText(file).Split().AsEnumerable());
        Console.WriteLine("{0} found in: {1}", find, string.Join(" ", Invert(dictionary)[find]));
    }
}
```

Sample output:
<lang>files: file1 file2 file3
find: what
what found in: file1 file2
```



## Clojure


```clojure
(ns inverted-index.core
  (:require [clojure.set :as sets]
            [clojure.java.io :as io]))

(def pattern #"\w+")     ; Java regex for a raw term: here a substring of alphanums
(defn normalize [match] (.toLowerCase match))  ; normalization of a raw term

(defn term-seq [text] (map normalize (re-seq pattern text)))

(defn set-assoc
  "Produces map with v added to the set associated with key k in map m"
  [m k v] (assoc m k (conj (get m k #{}) v)))

(defn index-file [index file]
  (with-open [reader (io/reader file)]
    (reduce
      (fn [idx term] (set-assoc idx term file))
      index
      (mapcat term-seq (line-seq reader)))))

(defn make-index [files]
  (reduce index-file {} files))

(defn search [index query]
  (apply sets/intersection (map index (term-seq query))))

```



## CoffeeScript


```coffeescript

fs = require 'fs'

make_index = (fns) ->
  # words are indexed by filename and 1-based line numbers
  index = {}
  for fn in fns
    for line, line_num in fs.readFileSync(fn).toString().split '\n'
      words = get_words line
      for word in words
        word = mangle(word)
        index[word] ||= []
        index[word].push [fn, line_num+1]
  index

grep = (index, word) ->
  console.log "locations for '#{word}':"
  locations = index[mangle(word)] || []
  for location in locations
    [fn, line_num] = location
    console.log "#{fn}:#{line_num}"
  console.log "\n"

get_words = (line) ->
  words = line.replace(/\W/g, ' ').split ' '
  (word for word in words when word != '')

mangle = (word) ->
  # avoid conflicts with words like "constructor"
  '_' + word

do ->
  fns = (fn for fn in fs.readdirSync('.') when fn.match /\.coffee/)
  index = make_index(fns)
  grep index, 'make_index'
  grep index, 'sort'

```

output
<lang>
> coffee inverted_index.coffee
locations for 'make_index':
inverted_index.coffee:3
inverted_index.coffee:33
inverted_index.coffee:34


locations for 'sort':
anagrams.coffee:8
derangements.coffee:14
heap.coffee:34
heap.coffee:43
huffman.coffee:81
inverted_index.coffee:35
knuth_sample.coffee:12

```





## Common Lisp


```Lisp
(defpackage rosettacode.inverted-index
  (:use cl))
(in-package rosettacode.inverted-index)

;; Return a list of tokens in the string LINE.  This is rather
;; complicated as CL has no good standard function to do it.
(defun tokenize (line)
  (let ((start 0) (len (length line)))
    (loop for s = (position-if #'alphanumericp line :start start)
          while s
          for e = (position-if-not #'alphanumericp line :start (1+ s))
          collect (subseq line s e)
          while (and e (< e len))
          do (setq start e))))

(defun index-file (index filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          do (dolist (token (tokenize line))
               (pushnew filename (gethash token index '()))))))

(defun build-index (filenames)
  (let ((index (make-hash-table :test #'equal)))
    (dolist (f filenames)
      (index-file index f))
    index))

;; Find the files for QUERY.  We use the same tokenizer for the query
;; as for files.
(defun lookup (index query)
  (remove-duplicates (loop for token in (tokenize query)
                           append (gethash token index))
                     :test #'equal))


```

Example:

```lisp
(defparameter *index* (build-index '("file1.txt" "file2.txt" "file3.txt")))
(defparameter *query* "foo bar")
(defparameter *result* (lookup *index* *query*))
(format t "Result for query ~s: ~{~a~^, ~}~%" *query* *result*)
```



## D


```d
import std.stdio, std.algorithm, std.string, std.file, std.regex;

void main() {
    string[][string] index;

    void parseFile(in string fn) {
        if (!exists(fn) || !isFile(fn))
            throw new Exception("File not found");

        foreach (word; readText(fn).splitter(regex(r"\W"))) {
            word = word.toLower();
            if (!index.get(word, null).canFind(fn))
                index[word] ~= fn;
        }
    }

    immutable fileNames = ["inv1.txt", "inv2.txt", "inv3.txt"];
    foreach (fName; fileNames)
        parseFile(fName);

    while (true) {
        writef("\nEnter a word to search for: (q to quit): ");
        immutable w = readln().strip().toLower();
        if (w == "q") {
            writeln("quitting.");
            break;
        }
        if (w in index)
            writefln("'%s' found in%( %).", w, index[w]);
        else
            writefln("'%s' not found.", w);
    }
}
```

Both the demo text files and the queries are from the Wikipedia page, they contain:
 It is what it is.

 What is it?

 It is a banana!
```txt
Enter a word to search for: (q to quit): cat
'cat' not found.

Enter a word to search for: (q to quit): is
'is' found in "inv1.txt" "inv2.txt" "inv3.txt".

Enter a word to search for: (q to quit): banana
'banana' found in "inv3.txt".

Enter a word to search for: (q to quit): it
'it' found in "inv1.txt" "inv2.txt" "inv3.txt".

Enter a word to search for: (q to quit): what
'what' found in "inv1.txt" "inv2.txt".

Enter a word to search for: (q to quit): q
quitting.
```



## EchoLisp



### Indexing

Index values are sets associated with each word (key). We use the local-put-value function to permanently store the index, in the browser local storage.

```lisp

;; set of input files
(define FILES {T0.txt T1.txt T2.txt})
;; store name for permanent inverted index
(define INVERT "INVERTED-INDEX")

;; get text for each file, and call (action filename text)
(define (map-files action files)
	(for ((file files))
	(file->string action file)))

;; create store
(local-make-store INVERT)

; invert-word : word -> set of files
(define (invert-word word file store)
	    (local-put-value word
	    	(make-set  (cons file (local-get-value word store))) store))

; parse file text and invert each word
(define (invert-text file text)
		(writeln 'Inverting file text)
		(let ((text (text-parse text)))
		(for ((word text))  (invert-word (string-downcase word) file INVERT))))

```



###  Query

Intersect sets values of each word.

```lisp

;; usage : (inverted-search w1 w2 ..)
(define-syntax-rule (inverted-search w ...)
			(and-get-invert (quote w )))

;; intersects all sets referenced by words
;; returns the intersection
(define (and-get-invert words)
		(foldl
			(lambda(word res)
				 (set-intersect res (local-get-value word  INVERT)))
			FILES words))

```

Output :

```lisp

(map-files invert-text FILES)
(inverted-search is it)
[0]→ { T0.txt T1.txt T2.txt }
(inverted-search is banana)
[1]→ { T2.txt }
(inverted-search is what)
[2]→ { T0.txt T1.txt }
(inverted-search boule)
[3]→ null

```




## Erlang

This might be used with a lot of large files so we use binaries to save space. That adds <<>> to the search terms.
If somebody wants to avoid "end." and "end" being two different terms, just add <<".">> to binary:compile_pattern/1
Ditto for any other character.


```Erlang

-module( inverted_index ).

-export( [from_files/1, search/2, task/0] ).

from_files( Files ) ->
        lists:foldl( fun import_from_file/2, dict:new(), Files ).

search(	Binaries, Inverted_index ) ->
        [Files | T] = [dict:fetch(X, Inverted_index) || X <- Binaries],
        lists:foldl( fun search_common/2, Files, T ).

task() ->
       Files_contents = [{"file_1", <<"it is what it is">>}, {"file_2", <<"what is it">>}, {"file_3", <<"it is a banana">>}],
       [file:write_file(X, Y) || {X, Y} <- Files_contents],
       Inverted_index = from_files( [X || {X, _Y} <- Files_contents] ),
       Result = search( [<<"what">>, <<"is">>, <<"it">>], Inverted_index ),
       io:fwrite( "~p~n", [Result] ),
       [file:delete(X) || {X, _Y} <- Files_contents].



import_from_file( File, Dict_acc ) ->
        New_dict = dict:from_list( import_from_file_contents(File, file:read_file(File)) ),
	dict:merge( fun import_from_file_merge/3, Dict_acc, New_dict ).

import_from_file_contents( File, {ok, Binary} ) ->
        [{X, [File]} || X <- binary:split( Binary, binary:compile_pattern([<<" ">>, <<"\n">>]), [global] )];
import_from_file_contents( File, {error, Error} ) ->
	io:fwrite( "Error: could not open file ~p: ~p~nContinuing with the rest of them~n", [File,	Error] ),
	[].

import_from_file_merge(	_Key, Files, [New_file] ) -> [New_file | Files].

search_common( Files, Acc ) -> [X || X <- Acc, lists:member(X, Files)].

```



## Factor


```factor
USING: assocs fry io.encodings.utf8 io.files kernel sequences
sets splitting vectors ;
IN: rosettacode.inverted-index

: file-words ( file -- assoc )
    utf8 file-contents " ,;:!?.()[]{}\n\r" split harvest ;
: add-to-file-list ( files file -- files )
    over [ swap [ adjoin ] keep ] [ nip 1vector ] if ;
: add-to-index ( words index file -- )
    '[ _ [ _ add-to-file-list ] change-at ] each ;
: (index-files) ( files index -- )
   [ [ [ file-words ] keep ] dip swap add-to-index ] curry each ;
: index-files ( files -- index )
    H{ } clone [ (index-files) ] keep ;
: query ( terms index -- files )
    [ at ] curry map [ ] [ intersect ] map-reduce ;

```


Example use :
<lang>( scratchpad ) { "f1" "f2" "f3" } index-files

--- Data stack:
H{ { "a" ~vector~ } { "is" ~vector~ } { "what" ~vector~ } { ...
( scratchpad ) { "what" "is" "it" } swap query .
V{ "f1" "f2" }

```


=={{header|F Sharp|F#}}==

```fsharp
open System
open System.IO

// Map search terms to associated set of files
type searchIndexMap = Map<string, Set<string>>

let inputSearchCriteria() =
    let readLine prompt =
        printf "%s: " prompt
        Console.ReadLine().Split()

    readLine "Files", (readLine "Find") |> Array.map (fun s -> s.ToLower())

let updateIndex indexMap keyValuePair =
    let k, v = keyValuePair

    match Map.tryFind k indexMap with
        | None     -> Map.add k (Set.singleton v) indexMap
        | Some set -> Map.add k (Set.add v set) indexMap

let buildIndex files =
    let fileData file =
        File.ReadAllText(file).Split() |> Seq.map (fun word -> word.ToLower(), file)

    files |> Seq.collect fileData
          |> Seq.fold updateIndex Map.empty

let searchFiles() =
    let files, terms = inputSearchCriteria()
    let indexes = buildIndex files

    let searchResults = terms |> Seq.map (fun term -> Map.find term indexes)
                              |> Set.intersectMany

    printf "Found in: " ; searchResults |> Set.iter (printf "%s ") ; printfn ""
```

Sample usage:

```txt

searchFiles()

Files: file1.txt file2.txt file3.txt
Find: what is
Found in: file1.txt file2.txt
```



## Go


```go
package main

import (
    "bufio"
    "bytes"
    "errors"
    "fmt"
    "io"
    "os"
)

// inverted index representation
var index map[string][]int // ints index into indexed
var indexed []doc

type doc struct {
    file  string
    title string
}

func main() {
    // initialize representation
    index = make(map[string][]int)

    // build index
    if err := indexDir("docs"); err != nil {
        fmt.Println(err)
        return
    }

    // run user interface
    ui()
}

func indexDir(dir string) error {
    df, err := os.Open(dir)
    if err != nil {
        return err
    }
    fis, err := df.Readdir(-1)
    if err != nil {
        return err
    }
    if len(fis) == 0 {
        return errors.New(fmt.Sprintf("no files in %s", dir))
    }
    indexed := 0
    for _, fi := range fis {
        if !fi.IsDir() {
            if indexFile(dir + "/" + fi.Name()) {
                indexed++
            }
        }
    }
    return nil
}

func indexFile(fn string) bool {
    f, err := os.Open(fn)
    if err != nil {
        fmt.Println(err)
        return false // only false return
    }

    // register new file
    x := len(indexed)
    indexed = append(indexed, doc{fn, fn})
    pdoc := &indexed[x]

    // scan lines
    r := bufio.NewReader(f)
    lines := 0
    for {
        b, isPrefix, err := r.ReadLine()
        switch {
        case err == io.EOF:
            return true
        case err != nil:
            fmt.Println(err)
            return true
        case isPrefix:
            fmt.Printf("%s: unexpected long line\n", fn)
            return true
        case lines < 20 && bytes.HasPrefix(b, []byte("Title:")):
            // in a real program you would write code
            // to skip the Gutenberg document header
            // and not index it.
            pdoc.title = string(b[7:])
        }
        // index line of text in b
        // again, in a real program you would write a much
        // nicer word splitter.
    wordLoop:
        for _, bword := range bytes.Fields(b) {
            bword := bytes.Trim(bword, ".,-~?!\"'`;:()<>[]{}\\|/=_+*&^%$#@")
            if len(bword) > 0 {
                word := string(bword)
                dl := index[word]
                for _, d := range dl {
                    if d == x {
                        continue wordLoop
                    }
                }
                index[word] = append(dl, x)
            }
        }
    }
    return true
}

func ui() {
    fmt.Println(len(index), "words indexed in", len(indexed), "files")
    fmt.Println("enter single words to search for")
    fmt.Println("enter a blank line when done")
    var word string
    for {
        fmt.Print("search word: ")
        wc, _ := fmt.Scanln(&word)
        if wc == 0 {
            return
        }
        switch dl := index[word]; len(dl) {
        case 0:
            fmt.Println("no match")
        case 1:
            fmt.Println("one match:")
            fmt.Println("   ", indexed[dl[0]].file, indexed[dl[0]].title)
        default:
            fmt.Println(len(dl), "matches:")
            for _, d := range dl {
                fmt.Println("   ", indexed[d].file, indexed[d].title)
            }
        }
    }
}
```

Session:

```txt

8448 words indexed in 11 files
enter single words to search for
enter a blank line when done
search word: dog
no match
search word: cat
one match:
    docs/pg28554.txt Beyond Lies the Wub
search word: robot
6 matches:
    docs/pg32032.txt Second Variety
    docs/pg32522.txt Mr. Spaceship
    docs/pg32832.txt Piper in the Woods
    docs/pg28698.txt The Crystal Crypt
    docs/pg28767.txt The Defenders
    docs/pg32154.txt The Variable Man

```



## Haskell



```haskell
import Control.Monad
import Data.Char (isAlpha, toLower)
import qualified Data.Map as M
import qualified Data.IntSet as S
import System.Environment (getArgs)

main =  do
    (files, _ : q) <- liftM (break (== "--")) getArgs
    buildII files >>= mapM_ putStrLn . queryII q

data IIndex = IIndex
    [FilePath]              -- Files in the index
    (M.Map String S.IntSet) -- Maps word to indices of the list
  deriving Show

buildII :: [FilePath] -> IO IIndex
buildII files =
    liftM (IIndex files . foldl f M.empty . zip [0..]) $
    mapM readFile files
  where f m (i, s) =
            foldl g m $ map (lowercase . filter isAlpha) $ words s
          where g m word = M.insertWith S.union word (S.singleton i) m

queryII :: [String] -> IIndex -> [FilePath]
queryII q (IIndex files m) =
    map (files !!) $ S.toList $ intersections $
    map (\word -> M.findWithDefault S.empty (lowercase word) m) q

intersections [] = S.empty
intersections xs = foldl1 S.intersection xs

lowercase = map toLower
```


An example of use, assuming the program is named <code>iindex</code> and there exist files <code>t0</code>, <code>t1</code>, and <code>t2</code> with contents "It is what it is.", "What is it?", and "It is a banana.":


```txt
$ iindex t0 t1 t2 -- what is it
t0
t1
```


=={{header|Icon}} and {{header|Unicon}}==

The following implements a simple case insensitive inverse index using lists simulating texts.

```Icon
procedure main()

  texts := table()     # substitute for read and parse files
  texts["T0.txt"] := ["it", "is", "what", "it", "is"]
  texts["T1.txt"] := ["what", "is", "it"]
  texts["T2.txt"] := ["it", "is", "a", "banana"]

  every textname := key(texts) do  # build index for each 'text'
     SII := InvertedIndex(SII,textname,texts[textname])

  TermSearchUI(SII)  # search UI

end

procedure InvertedIndex(ii,k,words)  #: accumulate a simple inverted index

/ii := table(set())    # create lookup table and null set
every w := !words do {
   if *ii[w] = 0 then ii[w] := set()  # new word, new set
   insert(ii[w],k)
   }

return ii
end

procedure TermSearchUI(ii)    #: search UI, all words must match

repeat {
   writes("Enter search terms (^z to quit) : ")
   terms := map(trim(read() | break))

   x := []
   terms ? while not pos(0) do {
      tab(many(' \t'))
      put(x,tab(upto('\ \t')|0))
      }

   show("Searching for : ",x)
   show("Found in : ",s := TermSearch(ii,x)) | show("Not found : ",x)
   }
write("End of search")
return
end

procedure TermSearch(ii,x)  #: return set of matches or fail
every s := !x do
   ( /u := ii[s] ) | (u **:= ii[s])
if *u > 0 then return u
end

procedure show(s,x) # display helper
every writes(s|!x) do writes(" ")
write()
return
end
```


Output:
```txt
Enter search terms (^z to quit) : is it
Searching for :  is it
Found in :  T0.txt T2.txt T1.txt
Enter search terms (^z to quit) : banana
Searching for :  banana
Found in :  T2.txt
Enter search terms (^z to quit) : fox
Searching for :  fox
Not found :  fox
Enter search terms (^z to quit) : what
Searching for :  what
Found in :  T0.txt T1.txt
```


The following code will build a full index.  Modification of search routines is left as an exercise:

```Unicon
record InvertedIndexRec(simple,full)

procedure FullInvertedIndex(ii,k,words)  #: accumulate a full inverted index

/ii := InvertedIndexRec( table(set()), table() ) # create lookup table and null set

wc := 0
every (w := !words, wc +:= 1) do {
   if *ii.simple[w] = 0 then {
       ii.simple[w] := set()  # new word, new set
       ii.full[w] := table()  # also new table
       }
   insert(ii.simple[w],k)
   /ii.full[w,k] := set()
   insert(ii.full[w,k],wc)
   }

return ii
end
```



## J


This just implements the required spec, with a simplistic definition for what a word is, and with no support for stop words, nor for phrase searching.


```J
require'files regex strings'

rxutf8 0  NB. support latin1 searches for this example, instead of utf8
files=:words=:buckets=:''
wordre=: rxcomp '[\w'']+'
parse=: ,@:rxfrom~ wordre&rxmatches

invert=: verb define
  files=: files,todo=. ~.y-.files
  >invert1 each todo
)

invert1=: verb define
  file=. files i.<y
  words=: ~.words,contents=. ~.parse tolower fread jpath y
  ind=. words i. contents
  buckets=: buckets,(1+words -&# buckets)#a:
  #buckets=: (file,~each ind{buckets) ind}buckets
)

search=: verb define
  hits=. buckets{~words i.~.parse tolower y
  files {~ >([-.-.)each/hits
)
```


Example use:


```J
   invert '~help/primer/cut.htm';'~help/primer/end.htm';'~help/primer/gui.htm'
   >search 'finally learning'
~help/primer/end.htm
~help/primer/gui.htm
   >search 'argument'
~help/primer/cut.htm
~help/primer/gui.htm
   >search 'around'
~help/primer/gui.htm
```


## Java


```Java

package org.rosettacode;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class InvertedIndex {

	List<String> stopwords = Arrays.asList("a", "able", "about",
			"across", "after", "all", "almost", "also", "am", "among", "an",
			"and", "any", "are", "as", "at", "be", "because", "been", "but",
			"by", "can", "cannot", "could", "dear", "did", "do", "does",
			"either", "else", "ever", "every", "for", "from", "get", "got",
			"had", "has", "have", "he", "her", "hers", "him", "his", "how",
			"however", "i", "if", "in", "into", "is", "it", "its", "just",
			"least", "let", "like", "likely", "may", "me", "might", "most",
			"must", "my", "neither", "no", "nor", "not", "of", "off", "often",
			"on", "only", "or", "other", "our", "own", "rather", "said", "say",
			"says", "she", "should", "since", "so", "some", "than", "that",
			"the", "their", "them", "then", "there", "these", "they", "this",
			"tis", "to", "too", "twas", "us", "wants", "was", "we", "were",
			"what", "when", "where", "which", "while", "who", "whom", "why",
			"will", "with", "would", "yet", "you", "your");

	Map<String, List<Tuple>> index = new HashMap<String, List<Tuple>>();
	List<String> files = new ArrayList<String>();

	public void indexFile(File file) throws IOException {
		int fileno = files.indexOf(file.getPath());
		if (fileno == -1) {
			files.add(file.getPath());
			fileno = files.size() - 1;
		}

		int pos = 0;
		BufferedReader reader = new BufferedReader(new FileReader(file));
		for (String line = reader.readLine(); line != null; line = reader
				.readLine()) {
			for (String _word : line.split("\\W+")) {
				String word = _word.toLowerCase();
				pos++;
				if (stopwords.contains(word))
					continue;
				List<Tuple> idx = index.get(word);
				if (idx == null) {
					idx = new LinkedList<Tuple>();
					index.put(word, idx);
				}
				idx.add(new Tuple(fileno, pos));
			}
		}
		System.out.println("indexed " + file.getPath() + " " + pos + " words");
	}

	public void search(List<String> words) {
		for (String _word : words) {
			Set<String> answer = new HashSet<String>();
			String word = _word.toLowerCase();
			List<Tuple> idx = index.get(word);
			if (idx != null) {
				for (Tuple t : idx) {
					answer.add(files.get(t.fileno));
				}
			}
			System.out.print(word);
			for (String f : answer) {
				System.out.print(" " + f);
			}
			System.out.println("");
		}
	}

	public static void main(String[] args) {
		try {
			InvertedIndex idx = new InvertedIndex();
			for (int i = 1; i < args.length; i++) {
				idx.indexFile(new File(args[i]));
			}
			idx.search(Arrays.asList(args[0].split(",")));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private class Tuple {
		private int fileno;
		private int position;

		public Tuple(int fileno, int position) {
			this.fileno = fileno;
			this.position = position;
		}
	}
}


```


Example output:

```Java

java -cp bin org.rosettacode.InvertedIndex "huntsman,merit,dog,the,gutenberg,lovecraft,olympian" pg30637.txt pg7025.txt pg82.txt pg9090.txt
indexed pg30637.txt 106473 words
indexed pg7025.txt 205714 words
indexed pg82.txt 205060 words
indexed pg9090.txt 68962 words
huntsman pg82.txt pg7025.txt
merit pg9090.txt pg30637.txt pg82.txt pg7025.txt
dog pg30637.txt pg82.txt pg7025.txt
the
gutenberg pg9090.txt pg30637.txt pg82.txt pg7025.txt
lovecraft pg30637.txt
olympian pg30637.txt


```



## jq


In the first part of this section, the core functions for computing an inverted index and for searching it are presented.
These functions will work with jq 1.4 as well as later (and possibly earlier) versions.

The second section shows how to accomplish the interactive task using a version of jq with support for 'input' and 'input_filename' (e.g. jq 1.5).

'''Part 1: inverted_index and search'''

```jq
# Given an array of [ doc, array_of_distinct_words ]
# construct a lookup table: { word: array_of_docs }
def inverted_index:
  reduce .[] as $pair
    ({};
     $pair[0] as $doc
     | reduce $pair[1][] as $word
       (.; .[$word] += [$doc]));

def search(words):
  def overlap(that): . as $this
  | reduce that[] as $item ([]; if $this|index($item) then . + [$item] else . end);

  . as $dict
  | if (words|length) == 0 then []
    else reduce words[1:][] as $word
      ( $dict[words[0]]; overlap( $dict[$word] ) )
    end ;
```


'''Part 2: Interactive Search'''

In this section, a solution to the task is presented using two
invocations of jq: one parses the input files, and the other does
everything else. If your shell does not support <(...) then you
could create a temporary file to hold the parsed output.


```jq
def prompt_search:
  "Enter a string or an array of strings to search for, quoting each string, or 0 to exit:",
  ( (input | if type == "array" then . elif type == "string" then [.]
             else empty
             end) as $in
    | search($in), prompt_search ) ;

$in | inverted_index | prompt_search
```


'''Example''':

```sh
$ jq -r -c -n --argfile in <(jq -R 'split(" ") | select(length>0) | [input_filename, unique]' T?.txt) -f Inverted_index.jq
Enter a string or an array of strings to search for, quoting each string, or 0 to exit:
"is"
["T0.txt","T1.txt","T2.txt"]
Enter a string or an array of strings to search for, quoting each string, or 0 to exit:
["is", "banana"]
["T2.txt"]
Enter a string or an array of strings to search for, quoting each string, or 0 to exit:
0
$
```




## Julia


```julia
function makedoubleindex(files)
    idx = Dict{String, Dict}()
    for file in files
        str = lowercase(read(file, String))
        words = split(str, r"\s+")
        for word in words
            if !haskey(idx, word)
                idx[word] = Dict{String, Int}()
            elseif !haskey(idx[word], file)
                (idx[word])[file] = 1
            else
                (idx[word])[file] += 1
            end
        end
    end
    idx
end

function wordsearch(dict, words::Vector{String})
    for word in words
        if haskey(dict, word)
            for (f, n) in dict[word]
                println("File $f contains $n instances of <$word>.")
            end
        else
            println("No instances of \"$word\" were found.")
        end
    end
end
wordsearch(dict, word::String) = wordsearch(dict, [word])


const filenames = ["file1.txt", "file2.txt", "file3.txt"]
const didx = makedoubleindex(filenames)
const searchterms = ["forehead", "of", "hand", "a", "foot"]
wordsearch(didx, searchterms)

```



## Kotlin


```scala
// version 1.1.51

import java.io.File

val invIndex  = mutableMapOf<String, MutableList<Location>>()
val fileNames = mutableListOf<String>()
val splitter  = Regex("""\W+""")

class Location(val fileName: String, val wordNum: Int) {
    override fun toString() = "{$fileName, word number $wordNum}"
}

fun indexFile(fileName: String) {
    if (fileName in fileNames) {
        println("'$fileName' already indexed")
        return
    }
    fileNames.add(fileName)
    File(fileName).forEachLine { line ->
        for ((i, w) in line.toLowerCase().split(splitter).withIndex()) {
            var locations = invIndex[w]
            if (locations == null) {
                locations = mutableListOf<Location>()
                invIndex.put(w, locations)
            }
            locations.add(Location(fileName, i + 1))
        }
    }
    println("'$fileName' has been indexed")
}

fun findWord(word: String) {
    val w = word.toLowerCase()
    val locations = invIndex[w]
    if (locations != null) {
       println("\n'$word' found in the following locations:")
       println(locations.map { "    $it" }.joinToString("\n"))
    }
    else println("\n'$word' not found")
    println()
}

fun main(args: Array<String>) {
    // files to be indexed entered as command line arguments
    if (args.size == 0) {
        println("No file names have been supplied")
        return
    }
    for (arg in args) indexFile(arg)
    println()
    println("Enter word(s) to be searched for in these files or 'q' to quit")
    while (true) {
        print("  ? : ")
        val word = readLine()!!
        if (word.toLowerCase() == "q") return
        findWord(word)
    }
}
```


Contents of files:

```txt

inv1.txt  -> It is what it is.
inv2.txt  -> What is it?
inv3.txt  -> It is a banana!

```


Sample output:

```txt
$ java -jar inverted_index.jar inv1.txt inv2.txt inv3.txt inv1.txt
'inv1.txt' has been indexed
'inv2.txt' has been indexed
'inv3.txt' has been indexed
'inv1.txt' already indexed

Enter word(s) to be searched for in these files or 'q' to quit
  ? : cat

'cat' not found

  ? : is

'is' found in the following locations:
    {inv1.txt, word number 2}
    {inv1.txt, word number 5}
    {inv2.txt, word number 2}
    {inv3.txt, word number 2}

  ? : banana

'banana' found in the following locations:
    {inv3.txt, word number 4}

  ? : it

'it' found in the following locations:
    {inv1.txt, word number 1}
    {inv1.txt, word number 4}
    {inv2.txt, word number 3}
    {inv3.txt, word number 1}

  ? : what

'what' found in the following locations:
    {inv1.txt, word number 3}
    {inv2.txt, word number 1}

  ? : a

'a' found in the following locations:
    {inv3.txt, word number 3}

  ? : q

```



## OCaml


We store the inverted index data in the file "data.inv" using the [http://www.ocaml.info/home/ocaml_sources.html#sexplib310 sexplib] library, so we compile with:

 ocamlc -c \
   -pp "camlp4o -I `ocamlc -where`/type-conv \
                -I `ocamlc -where`/sexplib \
                pa_type_conv.cmo pa_sexp_conv.cmo" \
   unix.cma bigarray.cma nums.cma -I +sexplib sexplib.cma str.cma \
   inv.ml

 ocamlc -o inv.byte unix.cma bigarray.cma nums.cma -I +sexplib sexplib.cma str.cma inv.cmo


```ocaml
TYPE_CONV_PATH "Inverted_index"

type files = string array with sexp
type inverted_index = (string * int list) list with sexp

type t = files * inverted_index with sexp

open Sexplib

let data_file = "data.inv"
let data_path = Filename.concat Filename.temp_dir_name data_file

let get_inv_index() =
  if Sys.file_exists data_path
  then t_of_sexp(Sexp.load_sexp data_path)
  else ([| |], [])

let load_file f =
  let ic = open_in f in
  let n = in_channel_length ic in
  let s = String.create n in
  really_input ic s 0 n;
  close_in ic;
  (s)

let array_push ar v =
  let len = Array.length ar in
  Array.init (succ len) (fun i ->
    if i < len then Array.unsafe_get ar i else v), len

let uniq lst =
  let h = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace h x ()) lst;
  Hashtbl.fold (fun x () xs -> x :: xs) h []

let combine words i inv_index =
  let h = Hashtbl.create (List.length inv_index) in
  List.iter (fun (w, from) -> Hashtbl.replace h w from) inv_index;
  List.iter (fun w ->
    let from =
      try Hashtbl.find h w
      except Not_found -> []
    in
    Hashtbl.replace h w (i::from)
  ) words;
  Hashtbl.fold (fun w from acc -> (w, from) :: acc) h []

let words_of_file in_file =
  let str = load_file in_file in
  let words = Str.split (Str.regexp "[ \r\n\t,;.?!:'/\034()]") str in
  let words = uniq words in
  (words)

let index_file in_file =
  let words = words_of_file in_file in
  let files, inv_index = get_inv_index() in
  let files, i = array_push files in_file in
  let inv_index = combine words i inv_index in
  let se = sexp_of_t (files, inv_index) in
  Sexp.save data_path se

let search_word word =
  let files, inv_index = get_inv_index() in
  try
    let is_in = List.assoc word inv_index in
    List.iter (fun i -> print_endline files.(i)) is_in
  with Not_found ->
    print_endline "# Not Found"

let usage() =
  Printf.printf "Usage: %s \
    --index-file <file.txt> / \
    --search-word <some-word>\n%!" Sys.argv.(0);
  exit 1

let () =
  let cmd, arg = try (Sys.argv.(1), Sys.argv.(2)) with _ -> usage() in
  match cmd, arg with
  | "--index-file", in_file -> index_file in_file
  | "--search-word", word -> search_word word
  | _ -> usage()
```



## Perl


```perl
use Set::Object 'set';

# given an array of files, returns the index
sub createindex
{
    my @files = @_;

    my %iindex;

    foreach my $file (@files)
    {
	open(F, "<", $file) or die "Can't read file $file: $!";
	while(<F>) {
            s/\A\W+//;
	    foreach my $w (map {lc} grep {length() >= 3} split /\W+/)
	    {
		if ( exists($iindex{$w}) )
		{
		    $iindex{$w}->insert($file);
		} else {
		    $iindex{$w} = set($file);
		}
	    }
	}
	close(F);
    }
    return %iindex;
}

# given an index, search for words
sub search_words_with_index
{
    my %idx = %{shift()};
    my @words = @_;
    my $res = set();

    foreach my $w (map {lc} @words)
    {
	$w =~ s/\W+//g;            # strip non-words chars
        length $w < 3 and next;
        exists $idx{$w} or return set();
        $res = $res->is_null
          ? set(@{$idx{$w}})
          : $res * $idx{$w};       # set intersection
    }
    return @$res;
}

# TESTING
# USAGE: invidx.pl the,list,of,words file1 file2 .. fileN
my @searchwords = split /,/, shift;
# first arg is a comma-separated list of words to search for
print "$_\n"
    foreach search_words_with_index({createindex(@ARGV)}, @searchwords);
```


## Perl 6

```perl6
sub MAIN (*@files) {
    my %norm;
    do for @files -> $file {
        %norm.push: $file X=> slurp($file).lc.words;
    }
    (my %inv).push: %norm.invert.unique;

    while prompt("Search terms: ").words -> @words {
        for @words -> $word {
            say "$word => {%inv.{$word.lc}//'(not found)'}";
        }
    }
}
```



## Phix

The following is included in the distro as demo\rosetta\Inverted_index.exw.

Loads all text files in demo\rosetta\ and builds a list of filenames and
a dictionary of {word,file_indexes}, before a handful of quick tests.

Might be better (and almost as easy) for the dictionary values to be say
{total_count, {file nos}, {file counts}}.

```Phix
integer word_count = 0
sequence filenames = {}

function is_ascii(string txt)
    for i=1 to length(txt) do
        integer ch = txt[i]
        if ch='\0' or ch>=#7F then return 0 end if
    end for
    return 1
end function

procedure add_words(string name, sequence words)
    filenames = append(filenames,name)
    integer fn = length(filenames)
    for i=1 to length(words) do
        string word = words[i]
        if word[1]>='a'         -- skip numbers
        and word[1]<='z' then
            integer node = getd_index(word)
            if node=0 then  -- not present
                setd(word,{fn})
                word_count += 1
            else
                sequence val = getd_by_index(node)
                if find(fn,val)=0 then
                    setd(word,append(val,fn))
                end if
            end if
        end if
    end for
end procedure

procedure load_directory()
sequence d = dir(".")
    for i=1 to length(d) do
        if not find('d',d[i][D_ATTRIBUTES])     -- skip directories
        and d[i][D_SIZE]<1024*1024*1024 then    -- and files > 1GB
            string name = d[i][D_NAME]
            integer fn = open(name,"rb")
            string txt = lower(get_text(fn))
            close(fn)
            if is_ascii(txt) then               -- skip any bitmaps etc
                sequence words = split_any(txt,"\0\r\n\t !\"#$%&\'()*+,-./:;<=>?@[\\]^`{|}~",0,1)
                add_words(name,words)
            end if
        end if
    end for
end procedure

function lookup(sequence words)
sequence files = {}     -- indexes to filenames
    for i=1 to length(words) do
        string word = words[i]
        integer node = getd_index(word)
        if node=0 then return {} end if
        sequence val = getd_by_index(node)
        if i=1 then
            files = val
        else
            for j=length(files) to 1 by -1 do
                if not find(files[j],val) then
                    files[j..j] = {}
                end if
            end for
            if length(files)=0 then return {} end if
        end if
    end for
    for i=1 to length(files) do
        files[i] = filenames[files[i]]
    end for
    return files
end function

load_directory()
?word_count
?lookup({"load_directory"})     -- should only be this file
?lookup({"dir"})                -- currently two use this
?lookup({"lower"})              -- currently four use this
?lookup({"lower","dir"})        -- currently two use both
?lookup({"dir","lower"})        -- should be the same two
?lookup({"ban"&"anafish"})      -- should be none ({})
```

```txt

3365
{"Inverted_index.exw"}
{"Inverted_index.exw","viewppm.exw"}
{"AlmostPrime.exw","Inverted_index.exw","RockPaperScissors.exw","viewppm.exw"}
{"Inverted_index.exw","viewppm.exw"}
{"Inverted_index.exw","viewppm.exw"}
{}

```



## PHP



```php
<?php

function buildInvertedIndex($filenames)
{
    $invertedIndex = [];

    foreach($filenames as $filename)
    {
        $data = file_get_contents($filename);

        if($data === false) die('Unable to read file: ' . $filename);

        preg_match_all('/(\w+)/', $data, $matches, PREG_SET_ORDER);

        foreach($matches as $match)
        {
            $word = strtolower($match[0]);

            if(!array_key_exists($word, $invertedIndex)) $invertedIndex[$word] = [];
            if(!in_array($filename, $invertedIndex[$word], true)) $invertedIndex[$word][] = $filename;
        }
    }

    return $invertedIndex;
}

function lookupWord($invertedIndex, $word)
{
    return array_key_exists($word, $invertedIndex) ? $invertedIndex[$word] : false;
}

$invertedIndex = buildInvertedIndex2(['file1.txt', 'file2.txt', 'file3.txt']);

foreach(['cat', 'is', 'banana', 'it'] as $word)
{
    $matches = lookupWord($invertedIndex, $word);

    if($matches !== false)
    {
        echo "Found the word \"$word\" in the following files: " . implode(', ', $matches) . "\n";
    }
    else
    {
        echo "Unable to find the word \"$word\" in the index\n";
    }
}
```



## PicoLisp

Assuming three files "file1", "file2" and "file3":

```txt
$ cat file1
it is what it is

$ cat file2
what is it

$ cat file3
it is a banana
```

we can read them into a binary tree in the global variable '*MyIndex'

```PicoLisp
(off *MyIndex)

(use Word
   (for File '("file1" "file2" "file3")
      (in File
         (while (skip)
            (if (idx '*MyIndex (setq Word (till " ^I^J^M" T)) T)
               (push1 (car @) File)
               (set Word (cons File)) ) ) ) ) )

(de searchFor @
   (apply sect
      (extract
         '((Word) (val (car (idx '*MyIndex Word))))
         (rest) ) ) )
```

Output:

```txt
: (searchFor "what" "is" "it")
-> ("file2" "file1")

: (searchFor "a" "banana")
-> ("file3")

: (searchFor "it" "is")
-> ("file3" "file2" "file1")
```



## PowerShell

```PowerShell
function Index-File ( [string[]]$FileList )
    {
    #  Create index hashtable, as needed
    If ( -not $Script:WordIndex ) { $Script:WordIndex = @{} }

    #  For each file to be indexed...
    ForEach ( $File in $FileList )
        {
        #  Find any previously existing entries for this file
        $ExistingEntries = $Script:WordIndex.Keys | Where { $Script:WordIndex[$_] -contains $File }

        #  For any previously existing entries
        #    Delete them (prior to reindexing the file)
        ForEach ( $Key in $ExistingEntries )
            {
            $Script:WordIndex[$Key] = @( $Script:WordIndex[$Key] | Where { $_ -ne $File } )
            }

        #  Get the contents of the file, split on non-alphanumeric characters, and remove duplicates
        $Words = ( Get-Content $File ) -split '[^a-zA-Z\d]' | Sort -Unique

        #  For each word in the file...
        ForEach ( $Word in $Words )
            {
            #  If the entry for the word already exists...
            If ( $Script:WordIndex[$Word] )
                {
                #  Add the file name to the entry
                $Script:WordIndex[$Word] += $File
                }
            Else
                {
                #  Create a new entry
                $Script:WordIndex[$Word] = @( $File )
                }
            }
        }
    }

function Find-Word ( [string]$Word )
    {
    return $WordIndex[$Word]
    }
```


```PowerShell
#  Populate test files
@'
Files full of
various words.
'@ | Out-File -FilePath C:\Test\File1.txt

@'
Create an index
of words.
'@ | Out-File -FilePath C:\Test\File2.txt

@'
Use the index
to find the files.
'@ | Out-File -FilePath C:\Test\File3.txt
```


```PowerShell
#  Index files
Index-File C:\Test\File1.txt, C:\Test\File2.txt, C:\Test\File3.txt
```

Because PowerShell is a shell language, it is "a user interface to do a search". After running the script defining the functions and running a command to index the files, the user can simply run the search function at the PowerShell command prompt.
Alternatively, one could create a more complex custom UI or GUI if desired.

```PowerShell
#  Query index
Find-Word files
```

```txt
C:\Test\File1.txt
C:\Test\File3.txt
```



## Python



###  Simple inverted index


First the simple inverted index from [[wp:Inverted index|here]] together with an implementation of a search for (multiple) terms from that index.

```python
'''
This implements: http://en.wikipedia.org/wiki/Inverted_index of 28/07/10
'''

from pprint import pprint as pp
from glob import glob
try: reduce
except: from functools import reduce
try:    raw_input
except: raw_input = input


def parsetexts(fileglob='InvertedIndex/T*.txt'):
    texts, words = {}, set()
    for txtfile in glob(fileglob):
        with open(txtfile, 'r') as f:
            txt = f.read().split()
            words |= set(txt)
            texts[txtfile.split('\\')[-1]] = txt
    return texts, words

def termsearch(terms): # Searches simple inverted index
    return reduce(set.intersection,
                  (invindex[term] for term in terms),
                  set(texts.keys()))

texts, words = parsetexts()
print('\nTexts')
pp(texts)
print('\nWords')
pp(sorted(words))

invindex = {word:set(txt
                        for txt, wrds in texts.items() if word in wrds)
            for word in words}
print('\nInverted Index')
pp({k:sorted(v) for k,v in invindex.items()})

terms = ["what", "is", "it"]
print('\nTerm Search for: ' + repr(terms))
pp(sorted(termsearch(terms)))
```


'''Sample Output'''

```txt

Texts
{'T0.txt': ['it', 'is', 'what', 'it', 'is'],
 'T1.txt': ['what', 'is', 'it'],
 'T2.txt': ['it', 'is', 'a', 'banana']}

Words
['a', 'banana', 'is', 'it', 'what']

Inverted Index
{'a': ['T2.txt'],
 'banana': ['T2.txt'],
 'is': ['T0.txt', 'T1.txt', 'T2.txt'],
 'it': ['T0.txt', 'T1.txt', 'T2.txt'],
 'what': ['T0.txt', 'T1.txt']}

Term Search for: ['what', 'is', 'it']
['T0.txt', 'T1.txt']
```



###  Full inverted index

There is a re-write of the <code>termsearch</code> function to work off this type of index, as well as a new <code>phrasesearch</code> function

The <code>phrasesearch</code> function will return multiple matches in a text, and goes on to show how this can be used to pick the text with most matches.

<small>It is assumed that the following code is added to the end of the code for the simple case above and so shares its file opening and parsing results</small>

```python
from collections import Counter


def termsearch(terms): # Searches full inverted index
    if not set(terms).issubset(words):
        return set()
    return reduce(set.intersection,
                  (set(x[0] for x in txtindx)
                   for term, txtindx in finvindex.items()
                   if term in terms),
                  set(texts.keys()) )

def phrasesearch(phrase):
    wordsinphrase = phrase.strip().strip('"').split()
    if not set(wordsinphrase).issubset(words):
        return set()
    #firstword, *otherwords = wordsinphrase # Only Python 3
    firstword, otherwords = wordsinphrase[0], wordsinphrase[1:]
    found = []
    for txt in termsearch(wordsinphrase):
        # Possible text files
        for firstindx in (indx for t,indx in finvindex[firstword]
                          if t == txt):
            # Over all positions of the first word of the phrase in this txt
            if all( (txt, firstindx+1 + otherindx) in finvindex[otherword]
                    for otherindx, otherword in enumerate(otherwords) ):
                found.append(txt)
    return found


finvindex = {word:set((txt, wrdindx)
                      for txt, wrds in texts.items()
                      for wrdindx in (i for i,w in enumerate(wrds) if word==w)
                      if word in wrds)
             for word in words}
print('\nFull Inverted Index')
pp({k:sorted(v) for k,v in finvindex.items()})

print('\nTerm Search on full inverted index for: ' + repr(terms))
pp(sorted(termsearch(terms)))

phrase = '"what is it"'
print('\nPhrase Search for: ' + phrase)
print(phrasesearch(phrase))

# Show multiple match capability
phrase = '"it is"'
print('\nPhrase Search for: ' + phrase)
ans = phrasesearch(phrase)
print(ans)
ans = Counter(ans)
print('  The phrase is found most commonly in text: ' + repr(ans.most_common(1)[0][0]))
```


'''Sample Output'''

```txt
Full Inverted Index
{'a': [('T2.txt', 2)],
 'banana': [('T2.txt', 3)],
 'is': [('T0.txt', 1), ('T0.txt', 4), ('T1.txt', 1), ('T2.txt', 1)],
 'it': [('T0.txt', 0), ('T0.txt', 3), ('T1.txt', 2), ('T2.txt', 0)],
 'what': [('T0.txt', 2), ('T1.txt', 0)]}

Term Search on full inverted index for: ['what', 'is', 'it']
['T0.txt', 'T1.txt']

Phrase Search for: "what is it"
['T1.txt']

Phrase Search for: "it is"
['T0.txt', 'T0.txt', 'T2.txt']
  The phrase is found most commonly in text: 'T0.txt'
```



## Racket


```racket

#!/usr/bin/env racket
#lang racket
(command-line
 #:args (term . files)
 (define rindex (make-hasheq))
 (for ([file files])
   (call-with-input-file file
     (λ(in) (let loop ()
              (define w (regexp-match #px"\\w+" in))
              (when w
                (let* ([w (bytes->string/utf-8 (car w))]
                       [w (string->symbol (string-foldcase w))]
                       [r (hash-ref rindex w '())])
                  (unless (member file r) (hash-set! rindex w (cons file r)))
                  (loop)))))))
 (define res
   (for/list ([w (regexp-match* #px"\\w+" term)])
     (list->set (hash-ref rindex (string->symbol (string-foldcase w)) '()))))
 (define all (set->list (apply set-intersect res)))
 (if (null? all)
   (printf "No matching files.\n")
   (printf "Terms found at: ~a.\n" (string-join all ", "))))

```

```txt

$ echo "It is what it is." > F1
$ echo "What is it?" > F2
$ echo "It is a banana." > F3
$ ./search.rkt "what" F?
Terms found at: F1, F2.
$ ./search.rkt "a" F?
Terms found at: F3.
$ ./search.rkt "what a" F?
No matching files.
$ ./search.rkt "what is it" F?
Terms found at: F1, F2.

```



## REXX

Note: In this algorithm, word indices start at 1.

Note:   the Burma Shave signs were created from 1930 ──► 1951   and were common among the rural byways of America.

To see more about Burma Shave signs, see the Wikipedia entry:   [http://en.wikipedia.org/wiki/Burma-Shave Burma Shave signs.]

```rexx
/*REXX program illustrates building a simple inverted index  and  a method of word find.*/
@.=                                              /*a dictionary of words   (so far).    */
!=                                               /*a list of found words   (so far).    */
call invertI 0, 'BURMA0.TXT'                     /*read the file:  BURMA0.TXT  ···      */
call invertI 1, 'BURMA1.TXT'                     /*  "   "    "    BURMA1.TXT  ···      */
call invertI 2, 'BURMA2.TXT'                     /*  "   "    "    BURMA2.TXT  ···      */
call invertI 3, 'BURMA3.TXT'                     /*  "   "    "    BURMA3.TXT  ···      */
call invertI 4, 'BURMA4.TXT'                     /*  "   "    "    BURMA4.TXT  ···      */
call invertI 5, 'BURMA5.TXT'                     /*  "   "    "    BURMA5.TXT  ···      */
call invertI 6, 'BURMA6.TXT'                     /*  "   "    "    BURMA6.TXT  ···      */
call invertI 7, 'BURMA7.TXT'                     /*  "   "    "    BURMA7.TXT  ···      */
call invertI 8, 'BURMA8.TXT'                     /*  "   "    "    BURMA8.TXT  ···      */
call invertI 9, 'BURMA9.TXT'                     /*  "   "    "    BURMA9.TXT  ···      */
call findAword  "huz"                            /*find a word.                         */
call findAword  "60"                             /*find another word.                   */
call findAword  "don't"                          /*and find another word.               */
call findAword  "burma-shave"                    /*and find yet another word.           */
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
findAword: procedure expose @.;  arg x           /*get an uppercase version of the X arg*/
           parse arg ox                          /*get original (as-is)  value of X arg.*/
           _=@.x;    oxo='───'ox"───"
           if _==''  then do
                          say 'word'   oxo   "not found."
                          return 0
                          end
           _@=_                                  /*save _ text, pass it back to invoker.*/
           say 'word'  oxo  "found in:"
                          do  until _=='';    parse var   _   f  w  _
                          say '       file='f   "  word="w
                          end   /*until ··· */
           return _@
/*──────────────────────────────────────────────────────────────────────────────────────*/
invertI:   procedure expose @. !; parse arg #,fn /*the file number and the filename.    */
           call lineout fn                       /*close the file, ··· just in case.    */
           w=0                                   /*the number of words found  (so far). */
               do  while lines(fn)\==0           /* [↓]   process the entire file.      */
               _=space( linein(fn) )             /*read a line, elide superfluous blanks*/
               if _==''  then iterate            /*if a blank record,  then ignore it.  */
               say 'file' #", record:" _         /*display the record ──► terminal.     */

                  do  until _==''                /*pick off words from record until done*/
                  parse upper var   _   ?  _     /*pick off a word  (it's in uppercase).*/
                  ?=stripper(?)                  /*strip any trailing punctuation.      */
                  if ?=''  then iterate          /*is the word now all blank (or null)? */
                  w=w+1                          /*bump the word counter (index).       */
                  @.?=@.?  #  w                  /*append the new word to a list.       */
                  if wordpos(?,!)==0  then !=! ? /*add it to the list of words found.   */
                  end   /*until ··· */
               end      /*while ··· */
           say;     call lineout fn              /*close the file, just to be neat&safe.*/
           return w                              /*return the index of word in record.  */
/*──────────────────────────────────────────────────────────────────────────────────────*/
stripper:  procedure;  parse arg q               /*remove punctuation at the end of word*/
           @punctuation= '.,:;?¿!¡∙·';        do j=1  for length(@punctuation)
                                              q=strip(q, 'T', substr(@punctuation, j, 1) )
                                              end   /*j*/
           return q
```

'''output'''

```txt
file 0, record: Rip a fender
file 0, record: Off your Car
file 0, record: Send it in
file 0, record: For a half-pound jar
file 0, record: Burma-Shave

file 1, record: A peach
file 1, record: Looks good
file 1, record: With lots of fuzz
file 1, record: Man's no peach
file 1, record: And never was
file 1, record: Burma-Shave

file 2, record: Does your husband
file 2, record: Misbehave
file 2, record: Grunt and grumble
file 2, record: Rant and rave ?
file 2, record: Shoot the brute some
file 2, record: Burma-Shave

file 3, record: Don't take a curve
file 3, record: At 60 per
file 3, record: We hate to lose
file 3, record: A customer
file 3, record: Burma-Shave

file 4, record: Every shaver
file 4, record: Now can snore
file 4, record: Six more minutes
file 4, record: Than before
file 4, record: By using
file 4, record: Burma-Shave

file 5, record: He played
file 5, record: a sax
file 5, record: Had no B.O.
file 5, record: But his whiskers scratched
file 5, record: So she let him go
file 5, record: Burma-Shave

file 6, record: Henry the Eighth
file 6, record: Prince of Friskers
file 6, record: Lost five wives
file 6, record: But kept his whiskers
file 6, record: Burma-Shave

file 7, record: Listen birds
file 7, record: These signs cost
file 7, record: Money
file 7, record: So roost a while
file 7, record: But don't get funny
file 7, record: Burma-Shave

file 8, record: My man
file 8, record: Won't shave
file 8, record: Sez Hazel Huz
file 8, record: But I should worry
file 8, record: Dora's does
file 8, record: Burma-Shave

file 9, record: Past
file 9, record: Schoolhouses
file 9, record: Take it slow
file 9, record: Let the little
file 9, record: Shavers grow
file 9, record: Burma-Shave

word ───huz─── found in:
       file=8   word=7
word ───60─── found in:
       file=3   word=6
word ───don't─── found in:
       file=3   word=1
       file=7   word=12
word ───burma-shave─── found in:
       file=0   word=14
       file=1   word=15
       file=2   word=15
       file=3   word=14
       file=4   word=13
       file=5   word=17
       file=6   word=14
       file=7   word=15
       file=8   word=14
       file=9   word=11

```



## Ruby


I broke this into two parts, storing the index as a file on disk to better represent how this might actually be used in practice.  The indexmerge part will create or update the index data file with any files given on the command line, and then indexsearch will use the data file to search for any terms listed on the command line.  The example is based on http://en.wikipedia.org/wiki/Inverted_index of 2010/09/10.

'''indexmerge.rb'''

```ruby
if File.exist? "index.dat"
  @data = Marshal.load open("index.dat")
else
  @data = {}
end

# Let's give the string class the ability to tokenize itsself into lowercase
# words with no punctuation.
class String
  def index_sanitize
    self.split.collect do |token|
      token.downcase.gsub(/\W/, '')
    end
  end
end

# Just implementing a simple inverted index here.
ARGV.each do |filename|
  open filename do |file|
    file.read.index_sanitize.each do |word|
      @data[word] ||= []
      @data[word] << filename unless @data[word].include? filename
    end
  end
end

open("index.dat", "w") do |index|
  index.write Marshal.dump(@data)
end
```


'''indexsearch.rb'''

```ruby
if File.exist? "index.dat"
  @data = Marshal.load open("index.dat")
else
  raise "The index data file could not be located."
end

class String
  def index_sanitize
    self.split.collect do |token|
      token.downcase.gsub(/\W/, '')
    end
  end
end

# Take anything passed in on the command line in any form and break it
# down the same way we did when making the index.
ARGV.join(' ').index_sanitize.each do |word|
  @result ||= @data[word]
  @result &= @data[word]
end

p @result
```


'''Output'''

```txt
> ./indexmerge.rb file1
> ./indexmerge.rb file2 file3
> ./indexsearch.rb what is it
["file1", "file2"]
> ./indexsearch.rb "a banana"
["file3"]
> ./indexsearch.rb It iS\!
["file1", "file2", "file3"]
```


## Scala


```Scala
object InvertedIndex extends App {
  import java.io.File

  // indexer
  val WORD = raw"(\w+)".r
  def parse(s: String) = WORD.findAllIn(s).map(_.toString.toLowerCase)
  def invertedIndex(files: Seq[File]): Map[String,Set[File]] = {
    var i = Map[String,Set[File]]() withDefaultValue Set.empty
    files.foreach{f => scala.io.Source.fromFile(f).getLines flatMap parse foreach
      (w => i = i + (w -> (i(w) + f)))}
    i
  }

  // user interface
  args match {
    case _ if args.length < 2 => println("Usage: InvertedIndex ALLSEARCHWORDS FILENAME...")
    case Array(searchwords, filenames @ _*) =>
      val queries = parse(searchwords).toList
      val files = filenames.map(new File(_)).filter{f => if (!f.exists) println(s"Ignoring $f"); f.exists}
      (queries, files) match {
        case (q, _) if q.isEmpty => println("Missing search words")
        case (_, f) if f.isEmpty => println("Missing extant files")
        case _ => val index = invertedIndex(files)
          println(s"""Searching for ${queries map ("\""+_+"\"") mkString " and "} in ${files.size} files:""")
          queries.map(index).foldLeft(files.toSet)(_ intersect _) match {
            case m if m.isEmpty => println("No matching files")
            case m => println(m mkString "\n")
          }
      }
  }
}
```

```txt
> InvertedIndex "the" file1.txt file2.txt file3.txt
Searching for "the" in 3 files:
data/file1.txt
data/file2.txt
data/file3.txt

> InvertedIndex "the cat sat" file1.txt file2.txt file3.txt
Searching for "the" and "cat" and "sat" in 3 files:
file1.txt
file2.txt

> InvertedIndex fox file1.txt file2.txt file3.txt
Searching for "fox" in 3 files:
file3.txt

> InvertedIndex abc file1.txt file2.txt file3.txt
Searching for "abc" in 3 files:
No matching files
```



## Tcl


```tcl
package require Tcl 8.5
proc wordsInString str {
    # We define "words" to be "maximal sequences of 'word' characters".
    # The other possible definition is to use 'non-space' characters.
    regexp -all -inline {\w+} $str
}

# Adds a document to the index. The index is a map from words to a map
# from filenames to lists of word locations.
proc addDocumentToIndex {filename} {
    global index
    set f [open $filename]
    set data [read $f]
    close $f

    set i 0
    array set localidx {}
    foreach word [wordsInString $data] {
	lappend localidx($word) $i
	incr i
    }

    # Transcribe into global index
    foreach {word places} [array get localidx] {
	dict set index($word) $filename $places
    }
}

# How to use the index to find files containing a word
proc findFilesForWord {word} {
    global index
    if {[info exists index($word)]} {
	return [dict keys $index($word)]
    }
}
# How to use the index to find files containing all words from a list.
# Note that this does not use the locations within the file.
proc findFilesWithAllWords {words} {
    set files [findFilesForWord [lindex $words 0]]
    foreach w [lrange $words 1 end] {
	set wf [findFilesForWord $w]
	set newfiles {}
	foreach f $files {
	    if {$f in $wf} {lappend newfiles $f}
	}
	set files $newfiles
    }
    return $files
}

# How to use the index to find a sequence of words in a file.
proc findFilesWithWordSequence {words} {
    global index
    set files {}
    foreach w $words {
	if {![info exist index($w)]} {
	    return
	}
    }
    dict for {file places} $index([lindex $words 0]) {
	if {$file in $files} continue
	foreach start $places {
	    set gotStart 1
	    foreach w [lrange $words 1 end] {
		incr start
		set gotNext 0
		foreach {f ps} $index($w) {
		    if {$f ne $file} continue
		    foreach p $ps {
			if {$p == $start} {
			    set gotNext 1
			    break
			}
		    }
		    if {$gotNext} break
		}
		if {!$gotNext} {
		    set gotStart 0
		    break
		}
	    }
	    if {$gotStart} {
		lappend files $file
		break
	    }
	}
    }
    return $files
}
```

For the GUI:

```tcl
package require Tk
pack [labelframe .files -text Files] -side left -fill y
pack [listbox .files.list -listvariable files]
pack [button .files.add -command AddFile -text "Add File to Index"]
pack [labelframe .found -text Found] -side right -fill y
pack [listbox .found.list -listvariable found] -fill x
pack [entry .found.entry -textvariable terms] -fill x
pack [button .found.findAll -command FindAll \
	-text "Find File with All"] -side left
pack [button .found.findSeq -command FindSeq \
	-text "Find File with Sequence"] -side right

# The actions invoked by various GUI buttons
proc AddFile {} {
    global files
    set f [tk_getOpenFile]
    if {$f ne ""} {
	addDocumentToIndex $f
	lappend files $f
    }
}
proc FindAll {} {
    global found terms
    set words [wordsInString $terms]
    set fs [findFilesWithAllWords $words]
    lappend found "Searching for files with all $terms" {*}$fs \
	"---------------------"
}
proc FindSeq {} {
    global found terms
    set words [wordsInString $terms]
    set fs [findFilesWithWordSequence $words]
    lappend found "Searching for files with \"$terms\"" {*}$fs \
	"---------------------"
}
```



## TUSCRIPT


```tuscript

$$ MODE TUSCRIPT

files="file1'file2'file3"
LOOP file=files
ERROR/STOP CREATE (file,seq-o,-std-)
ENDLOOP

content1="it is what it is"
content2="what is it"
content3="it is a banana"

FILE/ERASE "file1" = content1
FILE/ERASE "file2" = content2
FILE/ERASE "file3" = content3

ASK "search for": search=""
IF (search=="") STOP

BUILD R_TABLE/USER/AND search = *
DATA  {search}

LOOP/CLEAR file=files
 ACCESS q: READ/RECORDS $file s.z/u,content,count
  LOOP
  COUNT/NEXT/EXIT q (-; search;-;-)
  IF (count!=0) files=APPEND (files," ",file)
  ENDLOOP
 ENDACCESs q
ENDLOOP
PRINT "-> ",files

```

Output:

```txt

search for >what is it
-> file1 file2

search for >banana
-> file3

search for >it is
-> file1 file2 file3

```



## UNIX Shell


### Associative array

```bash
#!/bin/ksh

typeset -A INDEX

function index {
  typeset num=0
  for file in "$@"; do
    tr -s '[:punct:]' ' ' < "$file" | while read line; do
      for token in $line; do
        INDEX[$token][$num]=$file
      done
    done
  ((++num))
  done
}

function search {
  for token in "$@"; do
    for file in "${INDEX[$token][@]}"; do
      echo "$file"
    done
  done | sort | uniq -c | while read count file; do
    (( count == $# )) && echo $file
  done
}
```


Example use:

```korn
index *.txt
search hello world

```



### Directory on filesystem

The following is an attempt (not yet complete) to port the above script to [[pdksh]], and perhaps other Bourne-compatible shells.

* TODO Fill in "search.sh".
* Add note about slowness.


```bash
#!/bin/sh
# index.sh - create an inverted index

unset IFS
: ${INDEX:=index}

# Prohibit '\n' in filenames (because '\n' is
# the record separator for $INDEX/all.tab).
for file in "$@"; do
	# Use printf(1), not echo, because "$file" might start with
	# a hyphen and become an option to echo.
	test 0 -eq $(printf %s "$file" | wc -l) || {
		printf '%s\n' "$file: newline in filename" >&2
		exit 1
	}
done

# Make a new directory for the index, or else
# exit with the error message from mkdir(1).
mkdir "$INDEX" || exit $?

fi=1
for file in "$@"; do
	printf %s "Indexing $file." >&2

	# all.tab maps $fi => $file
	echo "$fi $file" >> "$INDEX/all.tab"

	# Use punctuation ([:punct:]) and whitespace (IFS)
	# to split tokens.
	ti=1
	tr -s '[:punct:]' ' ' < "$file" | while read line; do
		for token in $line; do
			# Index token by position ($fi, $ti). Ignore
			# error from mkdir(1) if directory exists.
			mkdir "$INDEX/$token" 2>/dev/null
			echo $ti >> "$INDEX/$token/$fi"
			: $((ti += 1))

			# Show progress. Print a dot per 1000 tokens.
			case "$ti" in
			*000)	printf .
			esac
		done
	done

	echo >&2
	: $((fi += 1))
done
```



```bash
#!/bin/sh
# search.sh - search an inverted index

unset IFS
: ${INDEX:=index}

want=sequence
while getopts aos name; do
	case "$name" in
	a)	want=all;;
	o)	want=one;;
	s)	want=sequence;;
	*)	exit 2;;
	esac
done
shift $((OPTIND - 1))

all() {
	echo "TODO"
	exit 2
}

one() {
	echo "TODO"
	exit 2
}

sequence() {
	echo "TODO"
	exit 2
}

$want "$@"
```

