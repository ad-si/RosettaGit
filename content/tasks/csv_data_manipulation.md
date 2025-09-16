+++
title = "CSV data manipulation"
description = ""
date = 2019-10-08T02:48:51Z
aliases = []
[extra]
id = 14402
[taxonomies]
categories = ["task"]
tags = []
languages = [
  "11l",
  "ada",
  "aime",
  "algol_68",
  "autohotkey",
  "awk",
  "c",
  "clojure",
  "common_lisp",
  "cpp",
  "csharp",
  "d",
  "display_the_object_in_tabular_form",
  "echolisp",
  "ecl",
  "elixir",
  "erlang",
  "export_the_array_of_modified_objects_to_the_csv_file",
  "factor",
  "freebasic",
  "funl",
  "gambas",
  "go",
  "groovy",
  "haskell",
  "import_each_line_of_the_csv_file_into_an_array_of_powershell_objects",
  "j",
  "java",
  "javascript",
  "jq",
  "julia",
  "kotlin",
  "lingo",
  "lua",
  "m2000_interpreter",
  "maple",
  "nanoquery",
  "netrexx",
  "nim",
  "objeck",
  "pari_gp",
  "pascal",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pl_i",
  "powershell",
  "prolog",
  "purebasic",
  "python",
  "q",
  "r",
  "racket",
  "red",
  "rexx",
  "ring",
  "ruby",
  "run_basic",
  "rust",
  "sas",
  "scala",
  "seed7",
  "sidef",
  "stata",
  "sum_the_values_of_the_properties_of_each_object",
  "tcl",
  "tuscript",
  "txr",
  "unix_shell",
  "ursa",
  "vba",
  "vbscript",
  "vedit_macro_language",
  "visual_foxpro",
  "zkl",
]
+++

[[wp:Comma-separated values|CSV spreadsheet files]] are suitable for storing tabular data in a relatively portable way.

The CSV format is flexible but somewhat ill-defined.

For present purposes, authors may assume that the data fields contain no commas, backslashes, or quotation marks.


## Task

Read a CSV file, change some values and save the changes back to a file.

For this task we will use the following CSV file:

 C1,C2,C3,C4,C5
 1,5,9,13,17
 2,6,10,14,18
 3,7,11,15,19
 4,8,12,16,20

<i>Suggestions</i>
<ul>
<li/> Show how to add a column, headed 'SUM', of the sums of the rows.
<li/> If possible, illustrate the use of built-in or standard functions, methods, or libraries, that handle generic CSV files.
</ul>





## 11l


```11l
L(=line) File(‘data.csv’).read_lines()
   I L.index == 0
      line ‘’= ‘,SUM’
   E
      line ‘’= ‘,’sum(line.split(‘,’).map(i -> Int(i)))
   print(line)
```


```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## Ada


Ada has no build-in or predefined functions to read or write CSV tables. We thus define a (very simplistic) package CSV, which allows to read a row (function Line), to step from column to column (function Next), and to get the items in the column (function Item):


```Ada
package CSV is

   type Row(<>) is tagged private;

   function Line(S: String; Separator: Character := ',') return Row;
   function Next(R: in out Row) return Boolean;
     -- if there is still an item in R, Next advances to it and returns True
   function Item(R: Row) return String;
     -- after calling R.Next i times, this returns the i'th item (if any)

private
   type Row(Length: Natural) is tagged record
      Str: String(1 .. Length);
      Fst: Positive;
      Lst: Natural;
      Nxt: Positive;
      Sep: Character;
   end record;
end CSV;
```


The implementation of the package is


```Ada
package body CSV is

   function Line(S: String; Separator: Character := ',')
                return Row is
      (Length => S'Length, Str => S,
       Fst => S'First, Lst => S'Last, Nxt => S'First, Sep => Separator);

   function Item(R: Row) return String is
      (R.Str(R.Fst .. R.Lst));

   function Next(R: in out Row) return Boolean is
      Last: Natural := R.Nxt;
   begin
      R.Fst := R.Nxt;
      while Last <= R.Str'Last and then R.Str(Last) /= R.Sep loop
         -- find Separator
         Last := Last + 1;
      end loop;
      R.Lst := Last - 1;
      R.Nxt := Last + 1;
      return (R.Fst <= R.Str'Last);
   end Next;

end CSV;
```


Finally, the main program which uses the package CSV:


```Ada
with CSV, Ada.Text_IO; use Ada.Text_IO;

procedure CSV_Data_Manipulation is
   Header: String := Get_Line;
begin
   Put_Line(Header & ", SUM");
   while not End_Of_File loop
      declare
         R: CSV.Row := CSV.Line(Get_Line);
         Sum: Integer := 0;
      begin
         while R.Next loop
            Sum := Sum + Integer'Value(R.Item);
            Put(R.Item & ",");
         end loop;
         Put_Line(Integer'Image(Sum));
      end;
   end loop;
end CSV_Data_Manipulation;
```


```txt
>./csv_data_manipulation < csv_sample.csv
C1,C2,C3,C4,C5, SUM
1,5,9,13,17, 45
2,6,10,14,18, 50
3,7,11,15,19, 55
4,8,12,16,20, 60
```



## Aime


```aime
void
read_csv(list t, text path)
{
    file f;
    list l;

    f_affix(f, path);
    while (f_news(f, l, 0, 0, ",") ^ -1) {
        l_append(t, l);
    }
}

list
sum_columns(list t)
{
    list c, l;
    integer i;

    l_append(c, "SUM");
    for (i, l in t) {
        if (i) {
            integer j, sum;
            text s;

            sum = 0;
            for (j, s in l) {
                sum += atoi(s);
            }

            l_append(c, sum);
        }
    }

    return c;
}

void
add_column(list t, list c)
{
    integer i;
    list l;

    for (i, l in t) {
        l_append(l, c[i]);
    }
}

void
write_csv(list t, text path)
{
    integer i;
    file f;
    list l;

    f_create(f, path, 00644);
    for (i, l in t) {
        f_(f, l[0]);
        l_ocall(l, f_, 2, 1, -1, f, ",");
        f_newline(f);
    }
}

integer
main(void)
{
    list t;

    read_csv(t, "tmp/CSV_data_manipulation.csv");
    add_column(t, sum_columns(t));
    write_csv(t, "tmp/CSV_data_manipulated.csv");

    return 0;
}
```

```txt
C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
```



## ALGOL 68


```algol68
# count occurrances of a char in string #
PROC char count = (CHAR c, STRING str) INT:
   BEGIN
      INT count := 0;
      FOR i TO UPB str DO
	 IF c = str[i] THEN count +:= 1
	 FI
      OD;
      count
   END;

# split string on separator #
PROC char split = (STRING str, CHAR sep) FLEX[]STRING :
   BEGIN
      INT strlen := UPB str, cnt := 0;
      INT len, p;
      INT start := 1;
      [char count (sep, str) + 1] STRING list;
      WHILE start <= strlen ANDF char in string (sep, p, str[start:]) DO
	 p +:= start - 1;
	 list[cnt +:= 1] := str[start:p-1];
	 start := p + 1
      OD;
      IF cnt = 0 THEN list[cnt +:= 1] := str
      ELIF start <= UPB str + 1 THEN list[cnt +:= 1] := str[start:]
      FI;
      list
   END;

PROC join = ([]STRING words, STRING sep) STRING:
   IF UPB words > 0 THEN
      STRING str := words [1];
      FOR i FROM 2 TO UPB words DO
	 str +:= sep + words[i]
      OD;
      str
   ELSE
      ""
   FI;

# read a line from file #
PROC readline = (REF FILE f) STRING:
   BEGIN
      STRING line;
      get (f, line); new line (f);
      line
   END;

# Add one item to tuple #
OP +:= = (REF FLEX[]STRING tuple, STRING item) VOID:
   BEGIN
      [UPB tuple+1]STRING new;
      new[:UPB tuple] := tuple;
      new[UPB new] := item;
      tuple := new
   END;

# convert signed number TO INT #
OP TOINT = (STRING str) INT:
   BEGIN
      INT n := 0, sign := 1;
      FOR i TO UPB str WHILE sign /= 0 DO
	 IF is digit (str[i]) THEN n := n * 10 + ABS str[i] - ABS "0"
	 ELIF i = 1 AND str[i] = "-" THEN sign := -1
	 ELIF i /= 1 OR str[i] /= "+" THEN sign := 0
	 FI
      OD;
      n * sign
   END;

OP STR = (INT i) STRING: whole (i,0);

# The main program #
FILE foo;
open (foo, "CSV_data_manipulation.data", stand in channel);
FLEX[0]STRING header := char split (readline (foo), ",");
header +:= "SUM";
print ((join (header, ","), new line));
WHILE NOT end of file (foo) DO
   FLEX[0]STRING fields := char split (readline (foo), ",");
   INT sum := 0;
   FOR i TO UPB fields DO
      sum +:= TOINT fields[i]
   OD;
   fields +:= STR sum;
   print ((join (fields, ","), new line))
OD;
close (foo)
```

```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## AutoHotkey


```AutoHotkey
Loop, Read, Data.csv
{
    i := A_Index
    Loop, Parse, A_LoopReadLine, CSV
        Output .= (i=A_Index && i!=1 ? A_LoopField**2 : A_LoopField) (A_Index=5 ? "`n" : ",")
}
FileAppend, %Output%, NewData.csv
```

'''Output:'''

```txt
C1,C2,C3,C4,C5
1,25,9,13,17
2,6,100,14,18
3,7,11,225,19
4,8,12,16,400
```



## AWK

adds a column sum to a csv table

```AWK
#!/usr/bin/awk -f
BEGIN { FS = OFS = "," }
NR==1 {
    print $0, "SUM"
    next
}
{
    sum = 0
    for (i=1; i<=NF; i++) {
        sum += $i
    }
    print $0, sum
}
```


```txt
awk -f csv_data_manipulation.awk data.csv
C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
```



## C


```c


#define TITLE "CSV data manipulation"
#define URL "http://rosettacode.org/wiki/CSV_data_manipulation"

#define _GNU_SOURCE
#define bool int
#include <stdio.h>
#include <stdlib.h> /* malloc...*/
#include <string.h> /* strtok...*/
#include <ctype.h>
#include <errno.h>


/**
 * How to read a CSV file ?
 */


typedef struct {
	char * delim;
	unsigned int rows;
	unsigned int cols;
	char ** table;
} CSV;


/**
 * Utility function to trim whitespaces from left & right of a string
 */
int trim(char ** str) {
	int trimmed;
	int n;
	int len;

	len = strlen(*str);
	n = len - 1;
	/* from right */
	while((n>=0) && isspace((*str)[n])) {
		(*str)[n] = '\0';
		trimmed += 1;
		n--;
	}

	/* from left */
	n = 0;
	while((n < len) && (isspace((*str)[0]))) {
		(*str)[0] = '\0';
		*str = (*str)+1;
		trimmed += 1;
		n++;
	}
	return trimmed;
}


/**
 * De-allocate csv structure
 */
int csv_destroy(CSV * csv) {
	if (csv == NULL) { return 0; }
	if (csv->table != NULL) { free(csv->table); }
	if (csv->delim != NULL) { free(csv->delim); }
	free(csv);
	return 0;
}


/**
 * Allocate memory for a CSV structure
 */
CSV * csv_create(unsigned int cols, unsigned int rows) {
	CSV * csv;

	csv = malloc(sizeof(CSV));
	csv->rows = rows;
	csv->cols = cols;
	csv->delim = strdup(",");

	csv->table = malloc(sizeof(char *) * cols * rows);
	if (csv->table == NULL) { goto error; }

	memset(csv->table, 0, sizeof(char *) * cols * rows);

	return csv;

error:
	csv_destroy(csv);
	return NULL;
}


/**
 * Get value in CSV table at COL, ROW
 */
char * csv_get(CSV * csv, unsigned int col, unsigned int row) {
	unsigned int idx;
	idx = col + (row * csv->cols);
	return csv->table[idx];
}


/**
 * Set value in CSV table at COL, ROW
 */
int csv_set(CSV * csv, unsigned int col, unsigned int row, char * value) {
	unsigned int idx;
	idx = col + (row * csv->cols);
	csv->table[idx] = value;
	return 0;
}

void csv_display(CSV * csv) {
	int row, col;
	char * content;
	if ((csv->rows == 0) || (csv->cols==0)) {
		printf("[Empty table]\n");
		return ;
	}

	printf("\n[Table cols=%d rows=%d]\n", csv->cols, csv->rows);
	for (row=0; row<csv->rows; row++) {
		printf("[|");
		for (col=0; col<csv->cols; col++) {
			content = csv_get(csv, col, row);
            printf("%s\t|", content);
		}
        printf("]\n");
	}
	printf("\n");
}

/**
 * Resize CSV table
 */
int csv_resize(CSV * old_csv, unsigned int new_cols, unsigned int new_rows) {
	unsigned int cur_col,
				 cur_row,
				 max_cols,
				 max_rows;
	CSV * new_csv;
	char * content;
	bool in_old, in_new;

	/* Build a new (fake) csv */
	new_csv = csv_create(new_cols, new_rows);
	if (new_csv == NULL) { goto error; }

	new_csv->rows = new_rows;
	new_csv->cols = new_cols;


	max_cols = (new_cols > old_csv->cols)? new_cols : old_csv->cols;
	max_rows = (new_rows > old_csv->rows)? new_rows : old_csv->rows;

	for (cur_col=0; cur_col<max_cols; cur_col++) {
		for (cur_row=0; cur_row<max_rows; cur_row++) {
			in_old = (cur_col < old_csv->cols) && (cur_row < old_csv->rows);
			in_new = (cur_col < new_csv->cols) && (cur_row < new_csv->rows);

			if (in_old && in_new) {
				/* re-link data */
				content = csv_get(old_csv, cur_col, cur_row);
				csv_set(new_csv, cur_col, cur_row, content);
			} else if (in_old) {
				/* destroy data */
				content = csv_get(old_csv, cur_col, cur_row);
				free(content);
			} else { /* skip */ }
		}
	}
	/* on rows */
	free(old_csv->table);
	old_csv->rows = new_rows;
	old_csv->cols = new_cols;
	old_csv->table = new_csv->table;
	new_csv->table = NULL;
	csv_destroy(new_csv);

	return 0;

error:
	printf("Unable to resize CSV table: error %d - %s\n", errno, strerror(errno));
	return -1;
}


/**
 * Open CSV file and load its content into provided CSV structure
 **/
int csv_open(CSV * csv, char * filename) {
	FILE * fp;
	unsigned int m_rows;
	unsigned int m_cols, cols;
	char line[2048];
	char * lineptr;
	char * token;


	fp = fopen(filename, "r");
	if (fp == NULL) { goto error; }

	m_rows = 0;
	m_cols = 0;
	while(fgets(line, sizeof(line), fp) != NULL) {
 		m_rows += 1;
 		cols = 0;
 		lineptr = line;
 		while ((token = strtok(lineptr, csv->delim)) != NULL) {
 			lineptr = NULL;
 			trim(&token);
            cols += 1;
        	if (cols > m_cols) { m_cols = cols; }
            csv_resize(csv, m_cols, m_rows);
            csv_set(csv, cols-1, m_rows-1, strdup(token));
        }
	}

	fclose(fp);
	csv->rows = m_rows;
	csv->cols = m_cols;
	return 0;

error:
	fclose(fp);
	printf("Unable to open %s for reading.", filename);
	return -1;
}


/**
 * Open CSV file and save CSV structure content into it
 **/
int csv_save(CSV * csv, char * filename) {
	FILE * fp;
	int row, col;
	char * content;

	fp = fopen(filename, "w");
	for (row=0; row<csv->rows; row++) {
		for (col=0; col<csv->cols; col++) {
			content = csv_get(csv, col, row);
            fprintf(fp, "%s%s", content,
            		((col == csv->cols-1) ? "" : csv->delim) );
		}
        fprintf(fp, "\n");
	}

	fclose(fp);
	return 0;
}


/**
 * Test
 */
int main(int argc, char ** argv) {
	CSV * csv;

	printf("%s\n%s\n\n",TITLE, URL);

	csv = csv_create(0, 0);
	csv_open(csv, "fixtures/csv-data-manipulation.csv");
	csv_display(csv);

	csv_set(csv, 0, 0, "Column0");
	csv_set(csv, 1, 1, "100");
	csv_set(csv, 2, 2, "200");
	csv_set(csv, 3, 3, "300");
	csv_set(csv, 4, 4, "400");
	csv_display(csv);

	csv_save(csv, "tmp/csv-data-manipulation.result.csv");
	csv_destroy(csv);

	return 0;
}

```

```txt

Column0,C2,C3,C4,C5
1,100,9,13,17
2,6,200,14,18
3,7,11,300,19
4,8,12,16,400

```



## C++


```cpp
#include <map>
#include <vector>
#include <iostream>
#include <fstream>
#include <utility>
#include <functional>
#include <string>
#include <sstream>
#include <algorithm>
#include <cctype>

class CSV
{
public:
    CSV(void) : m_nCols( 0 ), m_nRows( 0 )
    {}

    bool open( const char* filename, char delim = ',' )
    {
        std::ifstream file( filename );

        clear();
        if ( file.is_open() )
        {
            open( file, delim );
            return true;
        }

        return false;
    }

    void open( std::istream& istream, char delim = ',' )
    {
        std::string         line;

        clear();
        while ( std::getline( istream, line ) )
        {
            unsigned int nCol = 0;
            std::istringstream    lineStream(line);
            std::string           cell;

            while( std::getline( lineStream, cell, delim ) )
            {
                m_oData[std::make_pair( nCol, m_nRows )] = trim( cell );
                nCol++;
            }
            m_nCols = std::max( m_nCols, nCol );
            m_nRows++;
        }
    }

    bool save( const char* pFile, char delim = ',' )
    {
        std::ofstream ofile( pFile );
        if ( ofile.is_open() )
        {
            save( ofile );
            return true;
        }
        return false;
    }

    void save( std::ostream& ostream, char delim = ',' )
    {
        for ( unsigned int nRow = 0; nRow < m_nRows; nRow++ )
        {
            for ( unsigned int nCol = 0; nCol < m_nCols; nCol++ )
            {
                ostream << trim( m_oData[std::make_pair( nCol, nRow )] );
                if ( (nCol+1) < m_nCols )
                {
                    ostream << delim;
                }
                else
                {
                    ostream << std::endl;
                }
            }
        }
    }

    void clear()
    {
        m_oData.clear();
        m_nRows = m_nCols = 0;
    }

    std::string& operator()( unsigned int nCol, unsigned int nRow )
    {
        m_nCols = std::max( m_nCols, nCol+1 );
        m_nRows = std::max( m_nRows, nRow+1 );
        return m_oData[std::make_pair(nCol, nRow)];
    }

    inline unsigned int GetRows() { return m_nRows; }
    inline unsigned int GetCols() { return m_nCols; }

private:
    // trim string for empty spaces in begining and at the end
    inline std::string &trim(std::string &s)
    {

        s.erase(s.begin(), std::find_if(s.begin(), s.end(), std::not1(std::ptr_fun<int, int>(std::isspace))));
        s.erase(std::find_if(s.rbegin(), s.rend(), std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
        return s;
    }

private:
    std::map<std::pair<unsigned int, unsigned int>, std::string> m_oData;

    unsigned int    m_nCols;
    unsigned int    m_nRows;
};


int main()
{
    CSV oCSV;

    oCSV.open( "test_in.csv" );
    oCSV( 0, 0 ) = "Column0";
    oCSV( 1, 1 ) = "100";
    oCSV( 2, 2 ) = "200";
    oCSV( 3, 3 ) = "300";
    oCSV( 4, 4 ) = "400";
    oCSV.save( "test_out.csv" );
    return 0;
}
```

```txt

Column0,C2,C3,C4,C5
1,100,9,13,17
2,6,200,14,18
3,7,11,300,19
4,8,12,16,400

```



## C#


```c#
using System.IO;
using System.Linq;

namespace CSV_data_manipulation
{
    class Program
    {
        static void Main()
        {
            var input = File.ReadAllLines("test_in.csv");
            var output = input.Select((line, i) =>
            {
                if (i == 0)
                    return line + ",SUM";
                var sum = line.Split(',').Select(int.Parse).Sum();
                return line + "," + sum;
            }).ToArray();
            File.WriteAllLines("test_out.csv", output);
        }
    }
}

```

```txt

Column0,C2,C3,C4,C5,SUM
1,100,9,13,17,140
2,6,200,14,18,240
3,7,11,300,19,340
4,8,12,16,400,440

```



## Clojure


```clojure

(require '[clojure.data.csv :as csv]
         '[clojure.java.io :as io])

(defn add-sum-column [coll]
  (let [titles (first coll)
        values (rest coll)]
    (cons (conj titles "SUM")
      (map #(conj % (reduce + (map read-string %))) values))))

(with-open [in-file (io/reader "test_in.csv")]
  (doall
    (let [out-data (add-sum-column (csv/read-csv in-file))]
      (with-open [out-file (io/writer "test_out.csv")]
        (csv/write-csv out-file out-data)))))

```


```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## Common Lisp

Used only built-in functions which are in the standard. There are widespread libraries for working with csv (which can be easily loaded via quicklisp). As another example, I didn't use a split-string function, even though it is available in some implementations and in many compatibility layers and libraries. Instead, I formatted the csv file into s-expressions for the reader to understand it. Also, it deserves a mention that Common Lisp has built-in arrays, but for so little data it is easier to use nested lists.


```lisp

(defun csvfile-to-nested-list (filename delim-char)
    "Reads the csv to a nested list, where each sublist represents a line."
  (with-open-file (input filename)
    (loop :for line := (read-line input nil) :while line
          :collect (read-from-string
                    (substitute #\SPACE delim-char
                                (format nil "(~a)~%" line))))))

(defun sublist-sum-list (nested-list)
  "Return a list with the sum of each list of numbers in a nested list."
  (mapcar (lambda (l) (if (every #'numberp l)
                          (reduce #'+ l) nil))
          nested-list))

(defun append-each-sublist (nested-list1 nested-list2)
  "Horizontally append the sublists in two nested lists. Used to add columns."
  (mapcar #'append nested-list1 nested-list2))

(defun nested-list-to-csv (nested-list delim-string)
  "Converts the nested list back into a csv-formatted string."
  (format nil (concatenate 'string "~{~{~2,'0d" delim-string "~}~%~}")
          nested-list))

(defun main ()
  (let* ((csvfile-path #p"projekte/common-lisp/example_comma_csv.txt")
         (result-path #p"results.txt")
         (data-list (csvfile-to-nested-list csvfile-path #\,))
         (list-of-sums (sublist-sum-list data-list))
         (result-header "C1,C2,C3,C4,C5,SUM"))

    (setf data-list    ; add list of sums as additional column
          (rest    ; remove old header
           (append-each-sublist data-list
                                (mapcar #'list list-of-sums))))
    ;; write to output-file
    (with-open-file (output result-path :direction :output :if-exists :supersede)
      (format output "~a~%~a"
              result-header (nested-list-to-csv data-list ",")))))
(main)

```


```txt

C1,C2,C3,C4,C5,SUM
01,05,09,13,17,45,
02,06,10,14,18,50,
03,07,11,15,19,55,
04,08,12,16,20,60,

```



## D


```d
void main() {
    import std.stdio, std.csv, std.file, std.typecons, std.array,
           std.algorithm, std.conv, std.range;

    auto rows = "csv_data_in.csv".File.byLine;
    auto fout = "csv_data_out.csv".File("w");
    fout.writeln(rows.front);
    fout.writef("%(%(%d,%)\n%)", rows.dropOne
                .map!(r => r.csvReader!int.front.map!(x => x + 1)));
}
```

```txt
C1,C2,C3,C4,C5
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20
5,9,13,17,21
```



## EchoLisp


```scheme

;; CSV -> LISTS
(define (csv->row line)  (map (lambda(x) (or (string->number x) x)) (string-split line ",")))
(define (csv->table csv) (map  csv->row (string-split csv "\n")))

;; LISTS -> CSV
(define (row->csv row) (string-join row ","))
(define (table->csv header rows)
    (string-join  (cons (row->csv header) (for/list ((row rows)) (row->csv row))) "\n"))


(define (task file)
 (let*
 	((table (csv->table file))
 	(header (first table))
 	(rows (rest table)))

 	(table->csv
 		(append header "SUM") ;; add last column
 		(for/list ((row rows)) (append row (apply + row))))))



```

```scheme

(define file.csv #<<
C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20
>>#)

(task file.csv)

 → "C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60"

```



## ECL

<lang>// Assumes a CSV file exists and has been sprayed to a Thor cluster
MyFileLayout := RECORD
STRING Field1;
STRING Field2;
STRING Field3;
STRING Field4;
STRING Field5;
END;

MyDataset := DATASET ('~Rosetta::myCSVFile', MyFileLayout,CSV(SEPARATOR(',')));

MyFileLayout Appended(MyFileLayout pInput):= TRANSFORM
  SELF.Field1 := pInput.Field1 +'x';
  SELF.Field2 := pInput.Field2 +'y';
  SELF.Field3 := pInput.Field3 +'z';
  SELF.Field4 := pInput.Field4 +'a';
  SELF.Field5 := pInput.Field5 +'b';
END ;

MyNewDataset := PROJECT(MyDataset,Appended(LEFT));
OUTPUT(myNewDataset,,'~Rosetta::myNewCSVFile',CSV,OVERWRITE);
```

{{Out}} (contents of Rosetta::myNewCSVFile):

```txt
C1x,C2y,C3z,C4a,C5b
1x,5y,9z,13a,17b
2x,6y,10z,14a,18b
3x,7y,11z,15a,19b
4x,8y,12z,16a,20b
```



## Elixir


```Elixir

defmodule Csv do
  defstruct header: "", data: "", separator: ","

  def from_file(path) do
    [header | data] = path
    |> File.stream!
    |> Enum.to_list
    |> Enum.map(&String.trim/1)

    %Csv{ header: header, data: data }
  end

  def sums_of_rows(csv) do
    Enum.map(csv.data, fn (row) -> sum_of_row(row, csv.separator) end)
  end

  def sum_of_row(row, separator) do
    row
    |> String.split(separator)
    |> Enum.map(&String.to_integer/1)
    |> Enum.sum
    |> to_string
  end

  def append_column(csv, column_header, column_data) do
    header = append_to_row(csv.header, column_header, csv.separator)

    data = [csv.data, column_data]
    |> List.zip
    |> Enum.map(fn ({ row, value }) ->
      append_to_row(row, value, csv.separator)
    end)

    %Csv{ header: header, data: data }
  end

  def append_to_row(row, value, separator) do
    row <> separator <> value
  end

  def to_file(csv, path) do
    body = Enum.join([csv.header | csv.data], "\n")

    File.write(path, body)
  end
end

csv = Csv.from_file("in.csv")
csv
|> Csv.append_column("SUM", Csv.sums_of_rows(csv))
|> Csv.to_file("out.csv")

```


```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## Erlang


```Erlang

-module( csv_data ).

-export( [change/2, from_binary/1, from_file/1, into_file/2, task/0] ).

change( CSV, Changes ) -> lists:foldl( fun change_foldl/2, CSV, Changes ).

from_binary( Binary ) ->
        Lines = binary:split( Binary, <<"\n">>, [global] ),
        [binary:split(X, <<",">>, [global]) || X <- Lines].

from_file( Name ) ->
        {ok, Binary} = file:read_file( Name ),
        from_binary( Binary ).

into_file( Name, CSV ) ->
        Binaries = join_binaries( [join_binaries(X, <<",">>) || X <- CSV], <<"\n">> ),
        file:write_file( Name, Binaries ).

task() ->
       CSV = from_file( "CSV_file.in" ),
       New_CSV = change( CSV, [{2,3,<<"23">>}, {4,4,<<"44">>}] ),
       into_file( "CSV_file.out", New_CSV ).



change_foldl( {Row_number, Column_number, New}, Acc ) ->
        {Row_befores, [Row_columns | Row_afters]} = split( Row_number, Acc ),
        {Column_befores, [_Old | Column_afters]} = split( Column_number, Row_columns ),
        Row_befores ++ [Column_befores ++ [New | Column_afters]] ++ Row_afters.

join_binaries( Binaries, Binary ) ->
        [_Last | Rest] = lists:reverse( lists:flatten([[X, Binary] || X <- Binaries]) ),
        lists:reverse( Rest ).

split( 1, List ) -> {[], List};
split( N, List ) -> lists:split( N - 1, List ).

```


Contents of "CSV_file.out"

```txt

C1,C2,C3,C4,C5
1,5,23,13,17
2,6,10,14,18
3,7,11,44,19
4,8,12,16,20

```


=={{header|Euphoria|}}==

```euphoria
--- Read CSV file and add columns headed with 'SUM'
--- with trace
-- trace(0)

include get.e
include std/text.e

function split(sequence s, integer c)
    sequence removables = " \t\n\r\x05\u0234\" "
    sequence out
    integer first, delim
    out = {}
    first = 1
    while first <= length(s) do
        delim = find_from(c,s,first)
        if delim = 0 then
            delim = length(s)+1
        end if
        out = append(out,trim(s[first..delim-1],removables))
        first = delim + 1
    end while
    return out
end function

procedure main()
    integer fn    -- the file number
    integer fn2   -- the output file number
    integer e     -- the number of lines read
    object line   -- the next line from the file
    sequence data = {} -- parsed csv data row
    sequence headerNames = {} -- array saving column names
    atom sum = 0.0     -- sum for each row
    sequence var  -- holds numerical data read

    -- First we try to open the file called "data.csv".
    fn = open("data.csv", "r")
    if fn = -1 then
        puts(1, "Can't open data.csv\n")
	-- abort();
    end if

    -- Then we create an output file for processed data.
    fn2 = open("newdata.csv", "w")
    if fn2 = -1 then
        puts(1, "Can't create newdata.csv\n")
    end if

    -- By successfully opening the file we have established that
    -- the file exists, and open() gives us a file number (or "handle")
    -- that we can use to perform operations on the file.

    e = 1
    while 1 do
        line = gets(fn)
        if atom(line) then
            exit
        end if
        data = split(line, ',')

        if (e=1) then
            -- Save the header labels and
	    -- write them to output file.
            headerNames = data
	    for i=1 to length(headerNames) do
	        printf(fn2, "%s,", {headerNames[i]})
	    end for
	    printf(fn2, "SUM\n")
        end if

        -- Run a sum for the numerical data.
        if (e >= 2) then
	    for i=1 to length(data) do
	        printf(fn2, "%s,", {data[i]})
		var = value(data[i])
		if var[1] = 0 then
		    -- data read is numerical
		    -- add to sum
		    sum = sum + var[2]
		end if
	    end for
            printf(fn2, "%g\n", {sum})
	    sum = 0.0
        end if
        e = e + 1
    end while

    close(fn)
    close(fn2)
end procedure

main()

```


Contents of "newdata.csv"

```txt

C1	C2	C3	C4	C5	SUM
1	5	9	13	17	45
2	6	10	14	18	50
3	7	11	15	19	55
4	8	12	16	20	60

```


=={{header|F_Sharp|F#}}==

```fsharp
open System.IO

[<EntryPoint>]
let main _ =
    let input = File.ReadAllLines "test_in.csv"
    let output =
        input
        |> Array.mapi (fun i line ->
            if i = 0 then line + ",SUM"
            else
                let sum = Array.sumBy int (line.Split(','))
                sprintf "%s,%i" line sum)
    File.WriteAllLines ("test_out.csv", output)
    0

```

```txt

Column0,C2,C3,C4,C5,SUM
1,100,9,13,17,140
2,6,200,14,18,240
3,7,11,300,19,340
4,8,12,16,400,440

```



## Factor

The <code>csv</code> vocabulary provides words for working with csv files, strings, and streams.

```factor
USING: csv io.encodings.utf8 kernel math.parser sequences ;
IN: rosetta-code.csv-manipulation

: append-sum ( seq -- seq' )
    dup [ string>number ] map-sum number>string suffix ;

: csv-sums ( seq -- seq' )
    [ 0 = [ "SUM" suffix ] [ append-sum ] if ] map-index ;

"example.csv" utf8 [ file>csv csv-sums ] [ csv>file ] 2bi
```

Contents of <code>example.csv</code>

```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```


=={{Header|Forth}}==

```forth
\ csvsum.fs     Add a new column named SUM that contain sums from rows of CommaSeparatedValues
\ USAGE:
\       gforth-fast csvsum.fs -e "stdout stdin csvsum bye" <input.csv >output.csv

        CHAR ,  CONSTANT SEPARATOR
        3       CONSTANT DECIMALS
        1E1 DECIMALS S>D D>F F** FCONSTANT FSCALE

: colsum        ( ca u -- F: -- sum ;return SUM from CSV-string )
        0E0 OVER SWAP BOUNDS
        ?DO     ( a )
                I C@ SEPARATOR =
                IF      ( a )
                        I TUCK OVER - >FLOAT  IF F+ THEN
                        1+
                THEN
        LOOP    DROP
;
: f>string      ( -- ca u F: x -- )
        FSCALE F*
        F>D TUCK DABS <# DECIMALS 0 DO # LOOP [CHAR] . HOLD #S ROT SIGN #>
;
: rowC!+        ( offs char -- u+1  ;store CHAR at here+OFFS,increment offset )
        OVER HERE + C! 1+
;
: row$!+        ( offs ca u -- offs+u ;store STRING at here+OFFS,update offset )
        ROT 2DUP + >R HERE + SWAP MOVE R>
;
\ If run program with '-m 4G'option, we have practically 4G to store a row
: csvsum        ( fo fi --  ;write into FILEID-OUTPUT processed input from FILEID-INPUT )
        2DUP
        HERE UNUSED ROT READ-LINE THROW
        IF      ( fo fi fo u )
                HERE SWAP               ( fo fi fo ca u )
                SEPARATOR rowC!+
                s\" SUM" row$!+         ( fo fi fo ca u' )
                ROT WRITE-LINE THROW
                BEGIN   ( fo fi )
                        2DUP HERE UNUSED ROT READ-LINE THROW
                WHILE   ( fo fi fo u )
                        HERE SWAP                       ( fo fi fo ca u )
                        SEPARATOR rowC!+
                        HERE OVER colsum f>string       ( fo fi fo ca u ca' u' )
                        row$!+                          ( fo fi fo ca u'+u )
                        ROT WRITE-LINE THROW
                REPEAT
        THEN
        2DROP 2DROP
;
```

```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45.000
2,6,10,14,18,50.000
3,7,11,15,19,55.000
4,8,12,16,20,60.000
```


=={{Header|Fortran}}==

###  Fortran 2003

It's fairly easy to read arbitrary lines using allocatable character strings, available since Fortran 2003.


```fortran
program rowsum
    implicit none
    character(:), allocatable :: line, name, a(:)
    character(20) :: fmt
    double precision, allocatable :: v(:)
    integer :: n, nrow, ncol, i

    call get_command_argument(1, length=n)
    allocate(character(n) :: name)
    call get_command_argument(1, name)
    open(unit=10, file=name, action="read", form="formatted", access="stream")
    deallocate(name)

    call get_command_argument(2, length=n)
    allocate(character(n) :: name)
    call get_command_argument(2, name)
    open(unit=11, file=name, action="write", form="formatted", access="stream")
    deallocate(name)

    nrow = 0
    ncol = 0
    do while (readline(10, line))
        nrow = nrow + 1

        call split(line, a)

        if (nrow == 1) then
            ncol = size(a)
            write(11, "(A)", advance="no") line
            write(11, "(A)") ",Sum"
            allocate(v(ncol + 1))
            write(fmt, "('(',G0,'(G0,:,''',A,'''))')") ncol + 1, ","
        else
            if (size(a) /= ncol) then
                print "(A,' ',G0)", "Invalid number of values on row", nrow
                stop
            end if

            do i = 1, ncol
                read(a(i), *) v(i)
            end do
            v(ncol + 1) = sum(v(1:ncol))
            write(11, fmt) v
        end if
    end do
    close(10)
    close(11)
contains
    function readline(unit, line)
        use iso_fortran_env
        logical :: readline
        integer :: unit, ios, n
        character(:), allocatable :: line
        character(10) :: buffer

        line = ""
        readline = .false.
        do
            read(unit, "(A)", advance="no", size=n, iostat=ios) buffer
            if (ios == iostat_end) return
            readline = .true.
            line = line // buffer(1:n)
            if (ios == iostat_eor) return
        end do
    end function

    subroutine split(line, array, separator)
        character(*) line
        character(:), allocatable :: array(:)
        character, optional :: separator
        character :: sep
        integer :: n, m, p, i, k

        if (present(separator)) then
            sep = separator
        else
            sep = ","
        end if

        n = len(line)
        m = 0
        p = 1
        k = 1
        do i = 1, n
            if (line(i:i) == sep) then
                p = p + 1
                m = max(m, i - k)
                k = i + 1
            end if
        end do
        m = max(m, n - k + 1)

        if (allocated(array)) deallocate(array)
        allocate(character(m) :: array(p))

        p = 1
        k = 1
        do i = 1, n
            if (line(i:i) == sep) then
                array(p) = line(k:i-1)
                p = p + 1
                k = i + 1
            end if
        end do
        array(p) = line(k:n)
    end subroutine
end program
```



###  Old Fortran

This task in general soon becomes messy, especially when texts instead of numbers are amongst the input possibilities. Primarily, what is the upper bound on the number of values to a line? Will all lines have the same number of values; if not, is this an error? Do the data have special types (such as calendar dates?) or are all simple numbers? Very soon it becomes better to read a record as text and scan the text: problems can then be dealt with and good complaints made. So, how long is the longest record? Only the B6700 filesystem would make available a file's maximum record length: others offer no help. I have confronted a singularly witless format for supplying electricity data that would write up to an entire year's worth of half-hourly values to one line though it might be used to write just a few day's worth of data also. The header line specified the date and time slot for each column as <code>Country,Island,Node,MEAN Energy,01AUG2010 Daily ENERGY,01AUG2010 01,01AUG2010 02,01AUG2010 03, ''etc.''</code> so all-in-all it was less trouble to specify CHARACTER*246810 for the input record scratchpad so as not to have to struggle with piecemeal input. More generally, I have a routine for reading this sort of data that, provoked by multiple methods of specifying dates and times, is 1,500 lines long...

Rather than present a monster, what follows is a simple programme that employs the Fortran free-format (or "list-directed") input protocol, applied to the example data. Each READ statement reads an arbitrary number of input records until its input list is satisfied. In this case, notably, every line has five values, and five values are requested in each READ. Instead of numbers the first line presents texts that are column headings and, happily, the free-format input reader will parse the example headings as desired, provided that the read statement's input list is of CHARACTER type, not a number type. Commas are used as as delimiters, and the READ statement accepts that, ''but also spaces''; fortunately no spaces appear within the heading texts. This would be the first reason for writing a data-scanning system. It also accepts quoted texts (and also, allows hexadecimal/octal/binary values thereby) but this example does not require them. On the other hand, the appearance of a / in an input line marks the end of reading for that line and if the input list demands more data, an additional input record will be sought. Thus, if the input data includes dates expressed as say 6/12/2015 there will be difficulty, and another cause for devising a data-scanning system. The ISO standard of 2015-12-6 would also cause trouble as that would manifest as three numbers, if accepted at all, there being no delimiters (spaces or commas) in the sequence. Thus the / character marks the start of in in-line comment (as ! does in later-style Fortran source code) and is treated as end-of-line. The free-format input scan also recognises boolean values ("T" or "F", without the quotes) and complex numbers presented as <code>(''real'',''imag'')</code> pairs, and also repetition counts as in <code>24*6</code> meaning twenty-four occurrences of the value six, alas not using the @-symbol. Opportunities escalate still further with NAMELIST style I/O (allowing <code>''name'' = ''value(s)''</code>), so considerable flexibility is available. However, the input statement's list of variables must have the correct type for the input data (or ''vice-versa'') and the counting of the number of values requested and the number supplied must be correct too, lest everything get out of step. These are further reasons for writing a data-scanning system, but for the simple example, the standard features suffice. The idea is that these features suffice for getting data into a programme without much trouble, so that programming time can be devoted to what is to be done with the data, not how they are to be read.

So much for the header. The data lines can be read as numbers without difficulty, as they are numbers, so all that is necessary is that the input list be of type number. Chosen to be integer here, to match the example.

Output presents some worries, the usual one being how long is a piece of string? Unlike a number, which can be read as input with arbitrary size (0007 or 7, or 7.000000, etc.) character sequences are not translated into a number. Until there is provision for reading a string of arbitrary length, which will be stored by the READ statement as a string of an arbitrary length as encountered, an upper bound must be specified, and for this problem, six will do. For output, the special function TRIM removes trailing spaces, but is an intrinsic function only for F90 and later. Alas, it does not work for an array span, only a specific element at a time, so an implicit DO-loop is needed in the WRITE statement. Text literals in a FORMAT statement are rolled forth until such time as there is an edit code for a list item for which the output list is already exhausted. This would normally mean that the last datum written would be followed by a comma (from the text literal ",") however the mysterious colon in the FORMAT statement (instead of the more usual comma) means that text literals following it are ''not'' to be rolled forth unless a value from the output list is still waiting for its opportunity. One could instead use 666(",",I0) to omit the trailing comma but now there would be a leading comma. So, ... I0,666(",",I0) would avoid the leading comma, except if there was only one value to send forth, there would still be a trailing comma...

Another F90 feature is the SUM function that adds the elements of an array span. Even though for the example the first column looks rather like a record number, all five columns will be added, but otherwise the statement would be SUM(X(2:N)). Other modifications can be made without much difficulty, if desired. The output format is I0 rather than say I2, as it provides only the needed number of characters to present the integer's value. There is no corresponding F format code, and free-format output would roll out many spaces as padding in case of large numbers, that are not present here. It would be needed for a more general solution, but for this example, I0 will do.


```Fortran

Copies a file with 5 comma-separated values to a line, appending a column holding their sum.
      INTEGER N			!Instead of littering the source with "5"
      PARAMETER (N = 5)		!Provide some provenance.
      CHARACTER*6 HEAD(N)	!A perfect size?
      INTEGER X(N)		!Integers suffice.
      INTEGER LINPR,IN		!I/O unit numbers.
      LINPR = 6		!Standard output via this unit number.
      IN = 10		!Some unit number for the input file.
      OPEN (IN,FILE="CSVtest.csv",STATUS="OLD",ACTION="READ")	!For formatted input.

      READ (IN,*) HEAD	!The first line has texts as column headings.
      WRITE (LINPR,1) (TRIM(HEAD(I)), I = 1,N),"Sum"	!Append a "Sum" column.
    1 FORMAT (666(A:","))	!The : sez "stop if no list element awaits".
    2 READ (IN,*,END = 10) X	!Read a line's worth of numbers, separated by commas or spaces.
      WRITE (LINPR,3) X,SUM(X)	!Write, with a total appended.
    3 FORMAT (666(I0:","))	!I0 editing uses only as many columns as are needed.
      GO TO 2			!Do it again.

   10 CLOSE (IN)	!All done.
      END	!That's all.

```

Output could of course be written to a disc file instead of a screen, but here it is:

```txt

C1,C2,C3,C4,C5,Sum
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Open "manip.csv" For Input As #1 ' existing CSV file
Open "manip2.csv" For Output As #2 ' new CSV file for writing changed data

Dim header As String
Line Input #1, header
header += ",SUM"
Print #2, header

Dim As Integer c1, c2, c3, c4, c5, sum

While Not Eof(1)
  Input #1, c1, c2, c3, c4, c5
  sum = c1 + c2 + c3 + c4 + c5
  Write #2, c1, c2, c3, c4, c5, sum
Wend

Close #1
Close #2
```


```txt

' contents of manip2.csv

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## FunL


```funl
import io.{lines, PrintWriter}

data Table( header, rows )

def read( file ) =
  l = lines( file )

  def next = vector( l.next().split(',') )

  if l.isEmpty() then
    return Table( vector(), [] )

  header = next()
  rows = seq()

  while l.hasNext()
    rows += next()

  Table( header, rows.toList() )

def write( table, out ) =
  w = if out is String then PrintWriter( out ) else out

  w.println( table.header.mkString(',') )

  for r <- table.rows
    w.println( r.mkString(',') )

  if out is String
    w.close()

def updateRow( header, row, updates ) =
  r = dict( (header(i), row(i)) | i <- 0:header.length() )
  updates( r )
  vector( r(f) | f <- header )

def update( table, updates ) =
  Table( table.header, (updateRow(table.header, r, updates) | r <- table.rows).toList() )

def addColumn( table, column, updates ) =
  Table( table.header + [column], (updateRow(table.header + [column], r + [null], updates) | r <- table.rows).toList() )

t = addColumn( read('test.csv'), 'SUM', r -> r('SUM') = sum(int(v) | (_, v) <- r if v != null) )
write( t, 'test_out.csv' )
write( t, System.out )
```


```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## Gambas


```gambas
Public Sub Form_Open()
Dim sData As String = File.Load("data.csv")
Dim sLine, sTemp As String
Dim sOutput As New String[]
Dim siCount As Short
Dim bLine1 As Boolean

For Each sLine In Split(sData, gb.NewLine)
  If Not bLine1 Then
    sLine &= ",SUM"
    sOutput.Add(sLine)
    bLine1 = True
    Continue
  End If
  For Each sTemp In Split(sLine)
    siCount += Val(sTemp)
  Next
  sOutput.Add(sLine & "," & Str(siCount))
  siCount = 0
Next

sData = ""

For Each sTemp In sOutput
  sData &= sTemp & gb.NewLine
  Print sTemp;
  Print
Next

File.Save(User.home &/ "CSVData.csv", sData)

End
```

Output:

```txt

C1, C2, C3, C4, C5,SUM
1, 5, 9, 13, 17,45
2, 6, 10, 14, 18,50
3, 7, 11, 15, 19,55
4, 8, 12, 16, 20,60

```



## Go


```go
package main

import (
	"encoding/csv"
	"log"
	"os"
	"strconv"
)

func main() {
	rows := readSample()
	appendSum(rows)
	writeChanges(rows)
}

func readSample() [][]string {
	f, err := os.Open("sample.csv")
	if err != nil {
		log.Fatal(err)
	}
	rows, err := csv.NewReader(f).ReadAll()
	f.Close()
	if err != nil {
		log.Fatal(err)
	}
	return rows
}

func appendSum(rows [][]string) {
	rows[0] = append(rows[0], "SUM")
	for i := 1; i < len(rows); i++ {
		rows[i] = append(rows[i], sum(rows[i]))
	}
}

func sum(row []string) string {
	sum := 0
	for _, s := range row {
		x, err := strconv.Atoi(s)
		if err != nil {
			return "NA"
		}
		sum += x
	}
	return strconv.Itoa(sum)
}

func writeChanges(rows [][]string) {
	f, err := os.Create("output.csv")
	if err != nil {
		log.Fatal(err)
	}
	err = csv.NewWriter(f).WriteAll(rows)
	f.Close()
	if err != nil {
		log.Fatal(err)
	}
}
```

```txt

C1,C2,C3,C4,C5
1,5,9,13,17
2,six,10,14,18
3,7,11,15,19
4,8,12,16,20

```

```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,six,10,14,18,NA
3,7,11,15,19,55
4,8,12,16,20,60

```



## Groovy


```groovy
def csv = []
def loadCsv = { source -> source.splitEachLine(/,/) { csv << it.collect { it } } }
def saveCsv = { target -> target.withWriter { writer -> csv.each { writer.println it.join(',') } } }

loadCsv new File('csv.txt')
csv[0][0] = 'Column0'
(1..4).each { i -> csv[i][i] = i * 100 }
saveCsv new File('csv_out.txt')
```


csv_out.txt:

```txt
Column0,C2,C3,C4,C5
1,100,9,13,17
2,6,200,14,18
3,7,11,300,19
4,8,12,16,400
```



## Haskell

'''Solution 1'''
Array-based solution:


```haskell
import Data.Array (Array(..), (//), bounds, elems, listArray)
import Data.List (intercalate)
import Control.Monad (when)
import Data.Maybe (isJust)

delimiters :: String
delimiters = ",;:"

fields :: String -> [String]
fields [] = []
fields xs =
  let (item, rest) = break (`elem` delimiters) xs
      (_, next) = break (`notElem` delimiters) rest
  in item : fields next

unfields :: Maybe (Array (Int, Int) String) -> [String]
unfields Nothing = []
unfields (Just a) = every fieldNumber $ elems a
  where
    ((_, _), (_, fieldNumber)) = bounds a
    every _ [] = []
    every n xs =
      let (y, z) = splitAt n xs
      in intercalate "," y : every n z

fieldArray :: [[String]] -> Maybe (Array (Int, Int) String)
fieldArray [] = Nothing
fieldArray xs =
  Just $ listArray ((1, 1), (length xs, length $ head xs)) $ concat xs

fieldsFromFile :: FilePath -> IO (Maybe (Array (Int, Int) String))
fieldsFromFile = fmap (fieldArray . map fields . lines) . readFile

fieldsToFile :: FilePath -> Maybe (Array (Int, Int) String) -> IO ()
fieldsToFile f = writeFile f . unlines . unfields

someChanges :: Maybe (Array (Int, Int) String)
            -> Maybe (Array (Int, Int) String)
someChanges =
  fmap (// [((1, 1), "changed"), ((3, 4), "altered"), ((5, 2), "modified")])

main :: IO ()
main = do
  a <- fieldsFromFile "example.txt"
  when (isJust a) $ fieldsToFile "output.txt" $ someChanges a
```


'''Solution 2'''
List-based solution, heavily using functors and lenses

```haskell
{-# LANGUAGE FlexibleContexts,
             TypeFamilies,
             NoMonomorphismRestriction #-}
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Lens.Micro

(<$$>) :: (Functor f1, Functor f2) =>
          (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) = fmap . fmap

------------------------------------------------------------
-- reading and writing

newtype CSV = CSV { values :: [[String]] }

readCSV :: String -> CSV
readCSV = CSV . (splitOn "," <$$> lines)

instance Show CSV where
  show = unlines . map (intercalate ",") . values

------------------------------------------------------------
-- construction and combination

mkColumn, mkRow :: [String] -> CSV
(<||>), (<==>) :: CSV -> CSV -> CSV

mkColumn lst = CSV $ sequence [lst]
mkRow lst    = CSV [lst]

CSV t1 <||> CSV t2 = CSV $ zipWith (++) t1 t2
CSV t1 <==> CSV t2 = CSV $ t1 ++ t2

------------------------------------------------------------
-- access and modification via lenses

table = lens values (\csv t -> csv {values = t})
row i = table . ix i . traverse
col i = table . traverse . ix i
item i j = table . ix i . ix j

------------------------------------------------------------

sample = readCSV "C1, C2, C3, C4, C5\n\
                 \1,  5,  9,  13, 17\n\
                 \2,  6,  10, 14, 18\n\
                 \3,  7,  11, 15, 19\n\
                 \4,  8,  12, 16, 20"
```


Examples:

1. Reading from a file


```txt

λ> readCSV <$> readFile "example.csv"
C1, C2, C3, C4, C5
1,  5,  9,  13, 17
2,  6,  10, 14, 18
3,  7,  11, 15, 19
4,  8,  12, 16, 20

```


2. Access and modification

```txt
λ> sample ^. item 2 3
"14"

λ> sample ^.. row 2
["2","6","10","14","18"]

λ> sample ^.. col 2
["C3","9","10","11","12"]

λ> (item 3 2 .~ "Ok") sample
C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,Ok,15,19
4,8,12,16,20

λ> (item 3 2 %~ (show.(^2).read)) sample
C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,121,15,19
4,8,12,16,20

λ> (row 4 %~ (show.(^2).read)) sample
C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
16,64,144,256,400

λ> (col 4 %~ (++"!")) sample
C1,C2,C3,C4,C5!
1,5,9,13,17!
2,6,10,14,18!
3,7,11,15,19!
4,8,12,16,20!
```


3. Construction and combination

```haskell
sampleSum = sample <||> (mkRow ["SUM"] <==> mkColumn sums)
  where sums = map (show . sum) (read <$$> drop 1 (values sample))
```



```txt
λ> sampleSum
C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
```


=={{header|Icon}} and {{header|Unicon}}==

This version only works in Unicon, but can be easily adapted to work in Icon.


```unicon
import Utils   # To get CSV procedures

procedure main(A)
    f := open(A[1]) | &input
    i := 1
    write(!f)   # header line(?)
    every csv := parseCSV(!f) do {
        csv[i+:=1] *:= 100
        write(encodeCSV(csv))
        }
end
```


Sample run:


```txt

->csv csv.dat
C1,C2,C3,C4,C5
1,500,9,13,17
2,6,1000,14,18
3,7,11,1500,19
4,8,12,16,2000
->

```



## J

Like other languages it is not necessary to use the csv utilities to accomplish this task.


```j
   data=: (','&splitstring);.2 freads 'rc_csv.csv'       NB. read and parse data
   data=: (<'"spam"') (<2 3)} data                       NB. amend cell in 3rd row, 4th column (0-indexing)
   'rc_outcsv.csv' fwrites~ ;<@(','&joinstring"1) data   NB. format and write out amended data
```


Using the [[j:Addons/tables/dsv|delimiter-separated-values utilities]]  (of which <code>tables/csv</code> is a special case) will handle more complex csv constructs:


```j
   require 'tables/csv'
   data=: makenum readcsv 'rc_csv.csv'  NB. read data and convert cells to numeric where possible
   data=:  (<'spam') (2 3;3 0)} data    NB. amend 2 cells
   data writecsv 'rc_outcsv.csv'        NB. write out amended data. Strings are double-quoted
```


Adding a column with the sum of the rows:

```j
   require 'tables/csv'
   'hdr data'=: split readcsv 'rc_csv.csv'   NB. read data, split the header & data
   hdr=: hdr , <'SUM'                        NB. add title for extra column to header
   data=: <"0 (,. +/"1) makenum data         NB. convert to numeric, sum rows & append column
   (hdr,data) writecsv 'rc_out.csv'
```


Tacit version of above:

```j
   sumCSVrows=: writecsv~ (((<'SUM') ,~ {.) , [: (<"0)@(,. +/"1) makenum@}.)@readcsv
   'rc_out.csv' sumCSVrows 'rc.csv'
```



## Java


### Roll Your Own


```java
import java.io.*;
import java.awt.Point;
import java.util.HashMap;
import java.util.Scanner;

public class CSV {

    private HashMap<Point, String> _map = new HashMap<Point, String>();
    private int _cols;
    private int _rows;

    public void open(File file) throws FileNotFoundException, IOException {
        open(file, ',');
    }

    public void open(File file, char delimiter)
            throws FileNotFoundException, IOException {
        Scanner scanner = new Scanner(file);
        scanner.useDelimiter(Character.toString(delimiter));

        clear();

        while(scanner.hasNextLine()) {
            String[] values = scanner.nextLine().split(Character.toString(delimiter));

            int col = 0;
            for ( String value: values ) {
                _map.put(new Point(col, _rows), value);
                _cols = Math.max(_cols, ++col);
            }
            _rows++;
        }
        scanner.close();
    }

    public void save(File file) throws IOException {
        save(file, ',');
    }

    public void save(File file, char delimiter) throws IOException {
        FileWriter fw = new FileWriter(file);
        BufferedWriter bw = new BufferedWriter(fw);

        for (int row = 0; row < _rows; row++) {
            for (int col = 0; col < _cols; col++) {
                Point key = new Point(col, row);
                if (_map.containsKey(key)) {
                    bw.write(_map.get(key));
                }

                if ((col + 1) < _cols) {
                    bw.write(delimiter);
                }
            }
            bw.newLine();
        }
        bw.flush();
        bw.close();
    }

    public String get(int col, int row) {
        String val = "";
        Point key = new Point(col, row);
        if (_map.containsKey(key)) {
            val = _map.get(key);
        }
        return val;
    }

    public void put(int col, int row, String value) {
        _map.put(new Point(col, row), value);
        _cols = Math.max(_cols, col+1);
        _rows = Math.max(_rows, row+1);
    }

    public void clear() {
        _map.clear();
        _cols = 0;
        _rows = 0;
    }

    public int rows() {
        return _rows;
    }

    public int cols() {
        return _cols;
    }

    public static void main(String[] args) {
        try {
            CSV csv = new CSV();

            csv.open(new File("test_in.csv"));
            csv.put(0, 0, "Column0");
            csv.put(1, 1, "100");
            csv.put(2, 2, "200");
            csv.put(3, 3, "300");
            csv.put(4, 4, "400");
            csv.save(new File("test_out.csv"));
        } catch (Exception e) {
        }
    }
}
```

```txt

Column0,C2,C3,C4,C5
1,100,9,13,17
2,6,200,14,18
3,7,11,300,19
4,8,12,16,400

```

===Apache commons-csv===
Using the [http://commons.apache.org/proper/commons-csv/ Apache commons-csv] library.

```java
import java.io.*;
import java.util.*;

import org.apache.commons.csv.*;

public class RCsv {
  private static final String NL = System.getProperty("line.separator");
  private static final String FILENAME_IR = "data/csvtest_in.csv";
  private static final String FILENAME_OR = "data/csvtest_sum.csv";
  private static final String COL_NAME_SUM = "SUM, \"integers\""; // demonstrate white space, comma & quote handling

  public static void main(String[] args) {
    Reader iCvs = null;
    Writer oCvs = null;
    System.out.println(textFileContentsToString(FILENAME_IR));
    try {
      iCvs = new BufferedReader(new FileReader(FILENAME_IR));
      oCvs = new BufferedWriter(new FileWriter(FILENAME_OR));
      processCsv(iCvs, oCvs);
    }
    catch (IOException ex) {
      ex.printStackTrace();
    }
    finally {
      try {
        if (iCvs != null) { iCvs.close(); }
        if (oCvs != null) { oCvs.close(); }
      }
      catch (IOException ex) {
        ex.printStackTrace();
      }
    }
    System.out.println(textFileContentsToString(FILENAME_OR));
    return;
  }

  public static void processCsv(Reader iCvs, Writer oCvs) throws IOException {
    CSVPrinter printer = null;
    try {
      printer = new CSVPrinter(oCvs, CSVFormat.DEFAULT.withRecordSeparator(NL));
      List<String> oCvsHeaders;
      List<String> oCvsRecord;
      CSVParser records = CSVFormat.DEFAULT.withHeader().parse(iCvs);
      Map<String, Integer> irHeader = records.getHeaderMap();
      oCvsHeaders = new ArrayList<String>(Arrays.asList((irHeader.keySet()).toArray(new String[0])));
      oCvsHeaders.add(COL_NAME_SUM);
      printer.printRecord(oCvsHeaders);
      for (CSVRecord record : records) {
        oCvsRecord = record2list(record, oCvsHeaders);
        printer.printRecord(oCvsRecord);
      }
    }
    finally {
      if (printer != null) {
        printer.close();
      }
    }
    return;
  }

  private static List<String> record2list(CSVRecord record, List<String> oCvsHeaders) {
    List<String> cvsRecord;
    Map<String, String> rMap = record.toMap();
    long recNo = record.getRecordNumber();
    rMap = alterRecord(rMap, recNo);
    int sum = 0;
    sum = summation(rMap);
    rMap.put(COL_NAME_SUM, String.valueOf(sum));
    cvsRecord = new ArrayList<String>();
    for (String key : oCvsHeaders) {
      cvsRecord.add(rMap.get(key));
    }
    return cvsRecord;
  }

  private static Map<String, String> alterRecord(Map<String, String> rMap, long recNo) {
    int rv;
    Random rg = new Random(recNo);
    rv = rg.nextInt(50);
    String[] ks = rMap.keySet().toArray(new String[0]);
    int ix = rg.nextInt(ks.length);
    long yv = 0;
    String ky = ks[ix];
    String xv = rMap.get(ky);
    if (xv != null && xv.length() > 0) {
      yv = Long.valueOf(xv) + rv;
      rMap.put(ks[ix], String.valueOf(yv));
    }
    return rMap;
  }

  private static int summation(Map<String, String> rMap) {
    int sum = 0;
    for (String col : rMap.keySet()) {
      String nv = rMap.get(col);
      sum += nv != null && nv.length() > 0 ? Integer.valueOf(nv) : 0;
    }
    return sum;
  }

  private static String textFileContentsToString(String filename) {
    StringBuilder lineOut = new StringBuilder();
    Scanner fs = null;
    try {
      fs = new Scanner(new File(filename));
      lineOut.append(filename);
      lineOut.append(NL);
      while (fs.hasNextLine()) {
        String line = fs.nextLine();
        lineOut.append(line);
        lineOut.append(NL);
      }
    }
    catch (FileNotFoundException ex) {
      // TODO Auto-generated catch block
      ex.printStackTrace();
    }
    finally {
      if (fs != null) {
        fs.close();
      }
    }
    return lineOut.toString();
  }
}

```

{{in}} data/csvtest_in.csv

```txt

C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20

```

{{out}} data/csvtest_sum.csv

```txt

C1,C2,C3,C4,C5,"SUM, ""integers"""
1,5,9,21,17,53
36,6,10,14,18,84
3,7,11,27,19,67
4,8,12,53,20,97

```

===uniVocity-parsers===
Using the [http://www.univocity.com/pages/parsers-tutorial uniVocity-parsers] library.

```java

public static void main(String[] args) throws IOException {

        // 1st, config the CSV reader with line separator
        CsvParserSettings settings = new CsvParserSettings();
        settings.getFormat().setLineSeparator("\n");

        // 2nd, config the CSV reader with row processor attaching the bean definition
        BeanListProcessor<Employee> rowProcessor = new BeanListProcessor<Employee>(Employee.class);
        settings.setRowProcessor(rowProcessor);

        // 3rd, creates a CSV parser with the configs
        CsvParser parser = new CsvParser(settings);

        // 4th, parse all rows from the CSF file into the list of beans you defined
        parser.parse(new FileReader("/examples/employees.csv"));
        List<Employee> resolvedBeans = rowProcessor.getBeans();

        // 5th, Store, Delete duplicates, Re-arrange the words in specific order
        // ......

        // 6th, Write the listed of processed employee beans out to a CSV file.
        CsvWriterSettings writerSettings = new CsvWriterSettings();

        // 6.1 Creates a BeanWriterProcessor that handles annotated fields in the Employee class.
        writerSettings.setRowWriterProcessor(new BeanWriterProcessor<Employee>(Employee.class));

        // 6.2 persistent the employee beans to a CSV file.
        CsvWriter writer = new CsvWriter(new FileWriter("/examples/processed_employees.csv"), writerSettings);
        writer.processRecords(resolvedBeans);
        writer.writeRows(new ArrayList<List<Object>>());
    }

```




## JavaScript



### ES5


As an embedded scripting language which evolved in browsers carefully isolated from local file systems, JavaScript has no standard file IO libraries. The readFile() and writeFile() functions used in this example are written for JS embedded in macOS as 'JavaScript for Automation'. Other embeddings will require other definitions of these functions, and in some JS contexts it will not be possible to write them at all.


```JavaScript
(function () {
    'use strict';

    // splitRegex :: Regex -> String -> [String]
    function splitRegex(rgx, s) {
        return s.split(rgx);
    }

    // lines :: String -> [String]
    function lines(s) {
        return s.split(/[\r\n]/);
    }

    // unlines :: [String] -> String
    function unlines(xs) {
        return xs.join('\n');
    }

    // macOS JavaScript for Automation version of readFile.
    // Other JS contexts will need a different definition of this function,
    // and some may have no access to the local file system at all.

    // readFile :: FilePath -> maybe String
    function readFile(strPath) {
        var error = $(),
            str = ObjC.unwrap(
                $.NSString.stringWithContentsOfFileEncodingError(
                    $(strPath)
                    .stringByStandardizingPath,
                    $.NSUTF8StringEncoding,
                    error
                )
            );
        return error.code ? error.localizedDescription : str;
    }

    // macOS JavaScript for Automation version of writeFile.
    // Other JS contexts will need a different definition of this function,
    // and some may have no access to the local file system at all.

    // writeFile :: FilePath -> String -> IO ()
    function writeFile(strPath, strText) {
        $.NSString.alloc.initWithUTF8String(strText)
            .writeToFileAtomicallyEncodingError(
                $(strPath)
                .stringByStandardizingPath, false,
                $.NSUTF8StringEncoding, null
            );
    }

    // EXAMPLE - appending a SUM column

    var delimCSV = /,\s*/g;

    var strSummed = unlines(
        lines(readFile('~/csvSample.txt'))
        .map(function (x, i) {
            var xs = x ? splitRegex(delimCSV, x) : [];

            return (xs.length ? xs.concat(
                // 'SUM' appended to first line, others summed.
                i > 0 ? xs.reduce(
                    function (a, b) {
                        return a + parseInt(b, 10);
                    }, 0
                ).toString() : 'SUM'
            ) : []).join(',');
        })
    );

    return (
        writeFile('~/csvSampleSummed.txt', strSummed),
        strSummed
    );

})();
```


```txt
C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
```




### Node.js

We can easily manipulate files with Node.js.
Below is a toy example to add new columns to an CSV file. Other manipulations, i.e. adding new rows, modifying existing values and so forth, can be accomplished very easily.


```javascript
const fs = require('fs');

// formats for the data parameter in the function below: {col1: array | function, col2: array | function}

function addCols(path, data) {
  let csv = fs.readFileSync(path, 'utf8');
  csv = csv.split('\n').map(line => line.trim());
  let colNames = Object.keys(data);
  for (let i = 0; i < colNames.length; i++) {
    let c = colNames[i];
    if (typeof data[c] === 'function') {
      csv = csv.map((line, idx) => idx === 0
        ? line + ',' + c
        : line + ',' + data[c](line, idx)
      );
    } else if (Array.isArray(data[c])) {
      csv = csv.map((line, idx) => idx === 0
        ? line + ',' + c
        : line + ',' + data[c][idx - 1]
      );
    }
  }
  fs.createWriteStream(path, {
    flag: 'w',
    defaultEncoding: 'utf8'
  }).end(csv.join('\n'));
}

addCols('test.csv', {
    sum: function (line, idx) {
      let s = 0;
      line = line.split(',').map(d => +(d.trim()));
      for (let i = 0; i < line.length; i++) {
        s += line[i];
      }
      return s;
    },
    id: function(line, idx) {
      return idx;
    }
  });
```


```txt

C1,C2,C3,C4,C5,sum,id
1,5,9,13,17,45,1
2,6,10,14,18,50,2
3,7,11,15,19,55,3
4,8,12,16,20,60,4

```



## jq

The following adds a column with header "SUM" as suggested in the task description.

The writing out of each line is facilitated by jq's @csv builtin. We must "slurp in" the file (using the -s option) so the header line can be handled specially.

```jq
# Omit empty lines
def read_csv:
  split("\n")
  | map(if length>0 then split(",") else empty end) ;

# add_column(label) adds a summation column (with the given label) to
# the matrix representation of the CSV table, and assumes that all the
# entries in the body of the CSV file are, or can be converted to,
# numbers:
def add_column(label):
  [.[0] + [label],
   (reduce .[1:][] as $line
     ([]; ($line|map(tonumber)) as $line | . + [$line + [$line|add]]))[] ] ;

read_csv | add_column("SUM") | map(@csv)[]
```

 $ jq -s -R -r -f CSV_data_manipulation.jq input.csv
 "C1","C2","C3","C4","C5","SUM"
 1,5,9,13,17,45
 2,6,10,14,18,50
 3,7,11,15,19,55
 4,8,12,16,20,60


## Julia


```Julia
using DataFrames, CSV

ifn = "csv_data_manipulation_in.dat"
ofn = "csv_data_manipulation_out.dat"

df = CSV.read(ifn)
df.SUM = sum.(eachrow(df))
CSV.write(ofn, df)

```
```txt

$ cat csv_data_manipulation_out.dat
C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## Kotlin


```scala
// version 1.1.3

import java.io.File

fun main(args: Array<String>) {
    val lines = File("example.csv").readLines().toMutableList()
    lines[0] += ",SUM"
    for (i in 1 until lines.size) {
        lines[i] += "," + lines[i].split(',').sumBy { it.toInt() }
    }
    val text = lines.joinToString("\n")
    File("example2.csv").writeText(text)  // write to new file
    println(text)  // print to console
}
```


```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## Lingo


```lingo
----------------------------------------
-- Simplified CSV parser (without escape character support etc.).
-- First line is interrepted as header with column names.
-- @param {string} csvStr
-- @param {string} [sep=","] - single char as string
-- @param {string} [eol=RETURN]
-- @return {propList}
----------------------------------------
on parseSimpleCSVString (csvStr, sep, eol)
  if voidP(sep) then sep=","
  if voidP(eol) then eol = RETURN
  lines = explode(eol, csvStr)
  if lines.getLast()="" then lines.deleteAt(lines.count)
  res = [:]
  res[#header] = explode(sep, lines[1])
  res[#data] = []
  cnt = lines.count
  repeat with i = 2 to cnt
    res[#data].append(explodeBySingleChar(sep, lines[i]))
  end repeat
  return res
end

----------------------------------------
-- Simplified CSV creater (without escape character support etc.).
-- @param {propList} csvData
-- @param {string} [sep=","]
-- @param {string} [eol=RETURN]
-- @return {string}
----------------------------------------
on createSimpleCSVString (csvData, sep, eol)
  if voidP(sep) then sep=","
  if voidP(eol) then eol = RETURN
  res = ""
  put implode(sep, csvData[#header])&eol after res
  cnt = csvData[#data].count
  repeat with i = 1 to cnt
    put implode(sep, csvData[#data][i])&eol after res
  end repeat
  return res
end

----------------------------------------
-- Explodes string into list
-- @param {string} delim
-- @param {string} str
-- @return {list}
----------------------------------------
on explode (delim, str)
  if delim.length=1 then return explodeBySingleChar(delim, str)
  l = []
  if voidP(str) then return l
  dl = delim.length
  repeat while true
    pos = offset(delim, str)
    if pos=0 then exit repeat
    l.add(str.char[1..pos-1])
    delete char 1 to pos+dl-1 of str
  end repeat
  if pos=0 then pos = 1-dl
  l.add(str.char[pos+dl..str.length])
  return l
end

----------------------------------------
-- Explode string into list based on single char delimiter
-- (uses Lingo's build-in 'item' support, therefor faster)
-- @param {string} delim
-- @param {string} str
-- @return {list}
----------------------------------------
on explodeBySingleChar (delim, str)
  l = []
  if voidP(str) then return l
  od = _player.itemDelimiter
  _player.itemDelimiter = delim
  cnt = str.item.count
  repeat with i = 1 to cnt
    l.add(str.item[i])
  end repeat
  _player.itemDelimiter = od
  return l
end

----------------------------------------
-- Implodes list into string
-- @param {string} delim
-- @param {list} l
-- @return {string}
----------------------------------------
on implode (delim, l)
  str = ""
  cnt = l.count
  repeat with i = 1 to cnt
    put l[i]&delim after str
  end repeat
  delete char (str.length-delim.length+1) to str.length of str
  return str
end
```



```lingo
sep = ","
eol = numtochar(10)

-- load CSV string from file
fn = _movie.path & "file.csv"
fp = xtra("fileIO").new()
fp.openFile(fn, 1)
csvStr = fp.readFile()
fp.closeFile()

-- parse CSV string into propList
csvData = parseSimpleCSVString(csvStr, sep, eol)

-- add SUM column
csvData[#header].append("SUM")
repeat with row in csvData[#data]
  sum = 0
  repeat with cell in row
    sum = sum+integer(cell)
  end repeat
  row.append(sum)
end repeat

-- create CSV string from updated propList
csvString = createSimpleCSVString(csvData, sep, eol)

-- save CSV string to file
fn = _movie.path & "file.csv"
fp.openFile(fn, 2)
if not fp.status() then fp.delete()
fp.createFile(fn)
fp.openFile(fn, 2)
fp.writeString(csvString)
fp.closeFile()

-- show the CSV string
put csvString

-- "C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
"
```



## Lua

Adds a SUM column.

```lua
local csv={}
for line in io.lines('file.csv') do
    table.insert(csv, {})
    local i=1
    for j=1,#line do
        if line:sub(j,j) == ',' then
            table.insert(csv[#csv], line:sub(i,j-1))
            i=j+1
        end
    end
    table.insert(csv[#csv], line:sub(i,j))
end

table.insert(csv[1], 'SUM')
for i=2,#csv do
    local sum=0
    for j=1,#csv[i] do
        sum=sum + tonumber(csv[i][j])
    end
    if sum>0 then
        table.insert(csv[i], sum)
    end
end

local newFileData = ''
for i=1,#csv do
    newFileData=newFileData .. table.concat(csv[i], ',') .. '\n'
end

local file=io.open('file.csv', 'w')
file:write(newFileData)

```


```txt
C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```




## M2000 Interpreter


```M2000 Interpreter

Module Checkit {
      Function Sum {
            Long c=0
            while not empty {
                  c+=number
            }
            =c
      }
      Document CSV$ = {C1,C2,C3,C4,C5
            1,5,9,13,17
            2,6,10,14,18
            3,7,11,15,19
            4,8,12,16,20
      }
      \\ export encoded to UTF-16LE
      Save.Doc CSV$, "data1.csv", 0
      \\ Open Wide read UTF-16LE
      \\ use standard colum sep. as ","
      \\ use standard decimal point char
      \\ use standard (non json style string)
      \\ True = use bare strings (without "")
      Input With "",,,true
      Write With"",,,true
      \\ for excel csv use Input With chr$(9),,true, true
      Open "data1.csv" for Wide Input as #M
      Open "data2.csv" for Wide Output as #M1
      Input #M, h1$, h2$, h3$, h4$, h5$
      Write #M1, h1$, h2$, h3$, h4$, h5$
      Print h1$, h2$, h3$, h4$, h5$

      While not Eof(#M) {
            Input #M, A1, A2, A3, A4, A5
            Write #M1, A1, A2, A3, A4, A5, Sum(A1, A2, A3, A4, A5)
            Print A1, A2, A3, A4, A5
      }
      close #M1
      Close #M
      Open "data2.csv" for Wide Input as #M
      Input #M, h1$, h2$, h3$, h4$, h5$
      Print h1$, h2$, h3$, h4$, h5$
      While not Eof(#M) {
            Input #M, A1, A2, A3, A4, A5, Sum
            Print A1, A2, A3, A4, A5, Sum
      }
      Close #M
}
Checkit

```


=={{header|Mathematica}} / {{header|Wolfram Language}}==
Mathematica's Import and Export functions support CSV files.

```mathematica
iCSV=Import["test.csv"]
->{{"C1","C2","C3","C4","C5"},{1,5,9,13,17},{2,6,10,14,18},{3,7,11,15,19},{4,8,12,16,20}}
iCSV[[1, 1]] = Column0;
iCSV[[2, 2]] = 100;
iCSV[[3, 3]] = 200;
iCSV[[4, 4]] = 300;
iCSV[[5, 5]] = 400;
iCSV[[2, 3]] = 60;
Export["test.csv",iCSV];
```

```txt
Column0,C2,C3,C4,C5
1,100,60,13,17
2,6,200,14,18
3,7,11,300,19
4,8,12,16,400
```



## Maple

Entire script:

```Maple
M := ImportMatrix("data.csv",source=csv);
M(..,6) := < "Total", seq( add(M[i,j], j=1..5), i=2..5 ) >;
ExportMatrix("data_out.csv",M,target=csv);

```


Running this script showing interactive results:

```maple

 M := ImportMatrix("data.csv",source=csv);
                                 ["C1"    "C2"    "C3"    "C4"    "C5"]
                                 [                                    ]
                                 [ 1       5       9       13      17 ]
                                 [                                    ]
                            M := [ 2       6       10      14      18 ]
                                 [                                    ]
                                 [ 3       7       11      15      19 ]
                                 [                                    ]
                                 [ 4       8       12      16      20 ]

> M(..,6) := < "Total", seq( add(M[i,j], j=1..5), i=2..5 ) >;
                           ["C1"    "C2"    "C3"    "C4"    "C5"    "Total"]
                           [                                               ]
                           [ 1       5       9       13      17       45   ]
                           [                                               ]
                      M := [ 2       6       10      14      18       50   ]
                           [                                               ]
                           [ 3       7       11      15      19       55   ]
                           [                                               ]
                           [ 4       8       12      16      20       60   ]

> ExportMatrix("data_out.csv",M,target=csv);
                                               96

```


=={{header|MATLAB}} / {{header|Octave}}==

###  Using file manipulation


```Matlab
filename='data.csv';
fid = fopen(filename);
header = fgetl(fid);
fclose(fid);
X = dlmread(filename,',',1,0);

fid = fopen('data.out.csv','w+');
fprintf(fid,'%s,sum\n',header);
for k=1:size(X,1),
	fprintf(fid,"%i,",X(k,:));
	fprintf(fid,"%i\n",sum(X(k,:)));
end;
fclose(fid);
```



###  Using <code>table</code>


```Matlab
filename='data.csv';
data = readtable(filename);
data.SUM = sum([data{:,:}],2);
writetable(data,filename);
```



## Nanoquery


```Nanoquery
def sum($record)
        $sum = 0

        for ($i = 1) ($i <= len($record) - 1) ($i = $i+1)
                $sum = $sum+int($record ~ $i)
        end for

        return $sum
end def

open "file.csv"
add "SUM"

for ($i = $dbsize) ($i > 0) ($i = $i-1)
        ($i ~ @"SUM") = sum(#$i)
end for

write
```




## NetRexx

Using the [http://commons.apache.org/proper/commons-csv/ Apache commons-csv] library.

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols

import org.apache.commons.csv.

--
### =======================================================================

class RCsv public final

properties private constant
  NL           = String System.getProperty("line.separator")
  COL_NAME_SUM = String 'SUM, "integers"'
  CSV_IFILE    = 'data/csvtest_in.csv'
  CSV_OFILE    = 'data/csvtest_sumRexx.csv'

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method main(args = String[]) public static
  Arg  = Rexx(args)
  iCvs = Reader null
  oCvs = Writer null
  parse arg ifile ofile .
  if ifile = '', ifile = '.' then ifile = CSV_IFILE
  if ofile = '', ofile = '.' then ofile = CSV_OFILE
  say textFileContentsToString(ifile)
  do
    iCvs = BufferedReader(FileReader(ifile))
    oCvs = BufferedWriter(FileWriter(ofile))
    processCsv(iCvs, oCvs);
  catch ex = IOException
    ex.printStackTrace();
  finally
    do
      if iCvs \= null then iCvs.close()
      if oCvs \= null then oCvs.close()
    catch ex = IOException
      ex.printStackTrace()
    end
  end
  say textFileContentsToString(ofile)
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method processCsv(iCvs = Reader, oCvs = Writer) public static binary signals IOException
  printer = CSVPrinter null
  do
    printer = CSVPrinter(oCvs, CSVFormat.DEFAULT.withRecordSeparator(NL))
    oCvsHeaders = java.util.List
    oCvsRecord = java.util.List
    records = CSVFormat.DEFAULT.withHeader(String[0]).parse(iCvs)
    irHeader = records.getHeaderMap()
    oCvsHeaders = ArrayList(Arrays.asList((irHeader.keySet()).toArray(String[0])))
    oCvsHeaders.add(COL_NAME_SUM)
    printer.printRecord(oCvsHeaders)
    recordIterator = records.iterator()
    record = CSVRecord
    loop while recordIterator.hasNext()
      record = CSVRecord recordIterator.next()
      oCvsRecord = record2list(record, oCvsHeaders)
      printer.printRecord(oCvsRecord)
      end
  finally
    if printer \= null then printer.close()
  end
  return

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method record2list(record = CSVRecord, oCvsHeaders = java.util.List) private static binary returns java.util.List
  cvsRecord = java.util.List
  rMap = record.toMap()
  recNo = record.getRecordNumber()
  rMap = alterRecord(rMap, recNo)
  sum = summation(record.iterator())
  rMap.put(COL_NAME_SUM, sum)
  cvsRecord = ArrayList()
  loop ci = 0 to oCvsHeaders.size() - 1
    key = oCvsHeaders.get(ci)
    cvsRecord.add(rMap.get(key))
    end ci
  return cvsRecord

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method alterRecord(rMap = Map, recNo = long) private static binary returns Map
    rv = int
    rg = Random(recNo)
    rv = rg.nextInt(50)
    ks = rMap.keySet().toArray(String[0])
    ix = rg.nextInt(ks.length)
    yv = long 0
    ky = ks[ix];
    xv = String rMap.get(ky)
    if xv \= null & xv.length() > 0 then do
      yv = Long.valueOf(xv).longValue() + rv
      rMap.put(ks[ix], String.valueOf(yv))
      end
    return rMap

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method summation(iColumn = Iterator) private static
  sum = 0
  loop while iColumn.hasNext()
    nv = Rexx(String iColumn.next())
    if nv = null, nv.length() = 0, \nv.datatype('n') then nv = 0
    sum = sum + nv
    end
  return sum

-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
method textFileContentsToString(filename) private static
    lineOut = ''
    fs = Scanner null
    do
      fs = Scanner(File(filename))
      lineOut = lineout || filename || NL
      loop while fs.hasNextLine()
        line = fs.nextLine()
        lineOut = lineout || line || NL
        end
    catch ex = FileNotFoundException
      ex.printStackTrace()
    finally
      if fs \= null then fs.close()
    end
    return lineOut

```

{{in}} data/csvtest_in.csv

```txt

C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20

```

{{out}} data/csvtest_sumRexx.csv

```txt

C1,C2,C3,C4,C5,"SUM, ""integers"""
1,5,9,21,17,45
36,6,10,14,18,50
3,7,11,27,19,55
4,8,12,53,20,60

```



## Nim


Nim's standard library contains a robust CSV parser, but for this simple document that's not necessary.


```nim
import strutils, streams

let
  csv = newFileStream("data.csv", fmRead)
  outf = newFileStream("data-out.csv", fmWrite)

var lineNumber = 1

while true:
  if atEnd(csv):
    break
  var line = readLine(csv)

  if lineNumber == 1:
    line.add(",SUM")
  else:
    var tmp = 0
    for n in split(line, ","):
      tmp += parseInt(n)
    line.add(",")
    line.add($tmp)

  outf.writeLn($line)

  inc lineNumber
```


```txt
C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60


```



## Objeck

Objeck has a CSV parser with built-in functions.


```objeck
use System.IO.File;
use Data.CSV;

class CsvData {
  function : Main(args : String[]) ~ Nil {
    file_out : FileWriter;
    leaving {
      if(file_out <> Nil) {
        file_out->Close();
      };
    };

    if(args->Size() > 0) {
      file_name := args[0];
      csv := CsvTable->New(FileReader->ReadFile(file_name));
      if(csv->IsParsed()) {
        csv->AppendColumn("SUM");
        for(i := 1; i < csv->Size(); i += 1;) {
          row := csv->Get(i);
          sum := row->Sum(row->Size() - 1);
          row->Set("SUM", sum->ToString());
        };
      };

      output := csv->ToString();
      output->PrintLine();

      file_out := FileWriter->New("new-csv.csv");
      file_out->WriteString(output);
    };
  }
}

```


```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## PARI/GP

PARI/GP has no built-in support for strings or/and generic CSV files.

Note: stok(str,d) - Tokenize a string - can be found here on RC.
```parigp

\\ CSV data manipulation
\\ 10/24/16 aev
\\ processCsv(fn): Where fn is an input path and file name (but no actual extension).
processCsv(fn)=
{my(F, ifn=Str(fn,".csv"), ofn=Str(fn,"r.csv"), cn=",SUM",nf,nc,Vr,svr);
if(fn=="", return(-1));
F=readstr(ifn); nf=#F;
F[1] = Str(F[1],cn);
for(i=2, nf,
  Vr=stok(F[i],",");  if(i==2,nc=#Vr);
  svr=sum(k=1,nc,eval(Vr[k]));
  F[i] = Str(F[i],",",svr);
);\\fend i
for(j=1, nf, write(ofn,F[j]))
}

\\ Testing:
processCsv("c:\\pariData\\test");

```


{{in}} data/test.csv file

```txt

C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20
```


{{out}} data/testr.csv file

```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
```



## Pascal

In Pascal you can use TStringList CommaText property to work with CSV.

```pascal

program CSV_Data_Manipulation;
uses Classes, SysUtils;

var s: string;
    ts: tStringList;
    inFile,
    outFile: Text;
    Sum: integer;
    Number: string;

begin

  Assign(inFile,'input.csv');
  Reset(inFile);

  Assign(outFile,'result.csv');
  Rewrite(outFile);

  ts:=tStringList.Create;
  ts.StrictDelimiter:=True;

  // Handle the header
  ReadLn(inFile,s);                     // Read a line from input file
  ts.CommaText:=s;                      // Split it to lines
  ts.Add('SUM');                        // Add a line
  WriteLn(outFile,ts.CommaText);        // Reassemble it with comma as delimiter

  // Handle the data
  while not eof(inFile) do
  begin
    ReadLn(inFile,s);
    ts.CommaText:=s;

    Sum:=0;
    for Number in ts do
      Sum+=StrToInt(Number);

    ts.Add('%D',[Sum]);

    writeln(outFile, ts.CommaText);
  end;
  Close(outFile);
  Close(inFile);
  ts.Free;
end.

```


{{in}} input.csv file

```txt

C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20
```


{{out}} result.csv file

```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
```



## Perl

For simple files, you can use [http://p3rl.org/split split]:

```perl
#!/usr/bin/perl
use warnings;
use strict;

use List::Util 'sum';

my @header = split /,/, <>;
# Remove the newline.
chomp $header[-1];

my %column_number;
for my $i (0 .. $#header) {
    $column_number{$header[$i]} = $i;
}
my @rows   = map [ split /,/ ], <>;
chomp $_->[-1] for @rows;

# Add 1 to the numbers in the 2nd column:
$_->[1]++ for @rows;

# Add C1 into C4:
$_->[ $column_number{C4} ] += $_->[ $column_number{C1} ] for @rows;

# Add sums to both rows and columns.
push @header, 'Sum';
$column_number{Sum} = $#header;

push @$_, sum(@$_) for @rows;
push @rows, [
                map  {
                    my $col = $_;
                    sum(map $_->[ $column_number{$col} ], @rows);
                } @header
            ];

# Print the output.
print join(',' => @header), "\n";
print join(',' => @$_), "\n" for @rows;

```


However, if the CSV can contain quoted texts (the type MS Excel produces), you should rather use the [http://p3rl.org/Text::CSV Text::CSV]. Only reading the data and printing the result is different:

```perl
#!/usr/bin/perl
use warnings;
use strict;

use Text::CSV;
use List::Util 'sum';

my $csv = 'Text::CSV'->new({eol => "\n"})
          or die 'Cannot use CSV: ' . 'Text::CSV'->error_diag;

my $file = shift;
my @rows;
open my $FH, '<', $file or die "Cannot open $file: $!";
my @header = @{ $csv->getline($FH) };
while (my $row = $csv->getline($FH)) {
    push @rows, $row;
}
$csv->eof or $csv->error_diag;

#
# The processing is the same.
#

# Print the output.
$csv->print(*STDOUT, $_) for \@header, @rows;
```



## Perl 6

On the face of it this task is pretty simple. Especially given the sample CSV file and the total lack of specification of ''what'' changes to make to the file. Something like this would suffice.

```perl6
my $csvfile = './whatever.csv';
my $fh = open($csvfile, :r);
my @header = $fh.get.split(',');
my @csv = map {[.split(',')]>>.Num}, $fh.lines;
close $fh;

my $out = open($csvfile, :w);
$out.say((@header,'SUM').join(','));
$out.say((@$_, [+] @$_).join(',')) for @csv;
close $out;
```

But if your CSV file is at all complex you are better off using a CSV parsing module. (Complex meaning fields that contain commas, quotes, newlines, etc.)

```perl6
use Text::CSV;
my $csvfile = './whatever.csv';
my @csv = Text::CSV.parse-file($csvfile);
# modify(@csv); # do whatever;
csv-write-file( @csv, :file($csvfile) );
```



## Phix

Note that error checking is omitted, in particular for scanf.

```Phix
integer fn = open("test.csv","r")
sequence lines = {}
    while 1 do
        object line = gets(fn)
        if atom(line) then exit end if
        lines = append(lines,split(trim(line),','))
    end while
    close(fn)
    lines[1] = join(lines[1],',')&",SUM"
    for i=2 to length(lines) do
        sequence s = lines[i]
        for j=1 to length(s) do
            {{s[j]}} = scanf(s[j],"%d")
        end for
--      s[rand(length(s))] = rand(100) -- (if you like)
        lines[i] = sprintf("%d,%d,%d,%d,%d,%d",s&sum(s))
    end for
    lines = join(lines,'\n')
    fn = open("out.csv","w")
    puts(fn,lines)
    close(fn)
    puts(1,lines)
```

```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## PHP



```PHP

<?php

// fputcsv() requires at least PHP 5.1.0
// file "data_in.csv" holds input data
// the result is saved in "data_out.csv"
// this version has no error-checking

$handle = fopen('data_in.csv','r');
$handle_output = fopen('data_out.csv','w');
$row = 0;
$arr = array();

while ($line = fgetcsv($handle))
{
    $arr[] = $line;
}

//change some data to zeroes
$arr[1][0] = 0; // 1,5,9,13,17 => 0,5,9,13,17
$arr[2][1] = 0; // 2,6,10,14,18 => 2,0,10,14,18

//add sum and write file
foreach ($arr as $line)
{
    if ($row==0)
    {
        array_push($line,"SUM");
    }
    else
    {
        array_push($line,array_sum($line));
    }
    fputcsv($handle_output, $line);
    $row++;
}
?>
```



## PicoLisp


```PicoLisp
(in "data.csv"
   (prinl (line) "," "SUM")
   (while (split (line) ",")
      (prinl (glue "," @) "," (sum format @)) ) )
```

```txt
C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
```



## PL/I


```pli
*process source xref attributes or(!);
 csv: Proc Options(Main);
 /*********************************************************************
 * 19.10.2013 Walter Pachl
 * 'erase d:\csv.out'
 * 'set dd:in=d:\csv.in,recsize(300)'
 * 'set dd:out=d:\csv.out,recsize(300)'
 * Say 'Input:'
 * 'type csv.in'
 * 'csv'
 * Say ' '
 * Say 'Output:'
 * 'type csv.out'
 *********************************************************************/
 Dcl in  Record Input;
 Dcl out Record Output;
 On Endfile(in) Goto part2;
 Dcl (INDEX,LEFT,SUBSTR,TRIM) Builtin;

 Dcl (i,j,p,m,n) Bin Fixed(31) Init(0);
 Dcl s Char(100) Var;
 Dcl iline(10) Char(100) Var;
 Dcl a(20,20) Char(10) Var;
 Dcl sum Dec Fixed(3);
 Dcl oline Char(100) Var;

 Do i=1 By 1;
   Read File(in) Into(s);
   iline(i)=s;
   m=i;
   Call sep((s));
   End;

 part2:
 Do i=1 To m;
   If i=1 Then
     oline=iline(1)!!','!!'SUM';
   Else Do;
     sum=0;
     Do j=1 To n;
       sum=sum+a(i,j);
       End;
     oline=iline(i)!!','!!trim(sum);
     End;
   Write File(out) From(oline);
   End;

 sep: Procedure(line);
 Dcl line Char(*) Var;
 loop:
 Do j=1 By 1;
   p=index(line,',');
   If p>0 Then Do;
     a(i,j)=left(line,p-1);
     line=substr(line,p+1);
     End;
   Else Do;
     a(i,j)=line;
     Leave loop;
     End;
   End;
 n=j;
 End;

 End;
```

```txt

C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20

```

```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## Prolog

Add a "SUM" column.  Output is as for Lua and is not repeated here.

The following uses SWI-Prolog's csv_read_file_row/3 in order to
demonstrate that it is not necessary to read more than a line at a time.

```Prolog
test :- augment('test.csv', 'test.out.csv').

% augment( +InFileName, +OutFileName)
augment(InFile, OutFile)  :-
	open(OutFile, write, OutStream),
	( ( csv_read_file_row(InFile, Row, [line(Line)]),
	    % Row is of the form row( Item1, Item2, ....).
	    addrow(Row, Out),
	    csv_write_stream(OutStream, [Out], []),
	    fail
	  )
	; close(OutStream)
	).

% If the first item in a row is an integer, then append the sum;
% otherwise append 'SUM':
addrow( Term, NewTerm ) :-
	Term =.. [F | List],
	List = [X|_],
	(integer(X) -> sum_list(List, Sum) ; Sum = 'SUM'),
	append(List, [Sum], NewList),
	NewTerm =.. [F | NewList].

```



## PowerShell


```PowerShell
## Create a CSV file
@"
C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20
"@ -split "`r`n" | Out-File -FilePath .\Temp.csv -Force

## Import each line of the CSV file into an array of PowerShell objects
$records = Import-Csv -Path .\Temp.csv

## Sum the values of the properties of each object
$sums = $records | ForEach-Object {
    [int]$sum = 0
    foreach ($field in $_.PSObject.Properties.Name)
    {
        $sum += $_.$field
    }
    $sum
}

## Add a column (Sum) and its value to each object in the array
$records = for ($i = 0; $i -lt $sums.Count; $i++)
{
    $records[$i] | Select-Object *,@{Name='Sum';Expression={$sums[$i]}}
}

## Export the array of modified objects to the CSV file
$records | Export-Csv -Path .\Temp.csv -Force

## Display the object in tabular form
$records | Format-Table -AutoSize

```

```txt

C1 C2 C3 C4 C5 Sum
-- -- -- -- -- ---
1  5  9  13 17  45
2  6  10 14 18  50
3  7  11 15 19  55
4  8  12 16 20  60

```



## PureBasic


```PureBasic

EnableExplicit

#Separator$ = ","

Define fInput$ = "input.csv"; insert path to input file
Define fOutput$ = "output.csv"; insert path to output file
Define header$, row$, field$
Define nbColumns, sum, i

If OpenConsole()
  If Not ReadFile(0, fInput$)
    PrintN("Error opening input file")
    Goto Finish
  EndIf

  If Not CreateFile(1, fOutput$)
    PrintN("Error creating output file")
    CloseFile(0)
    Goto Finish
  EndIf

  ; Read header row
  header$ = ReadString(0)
  ; Determine number of columns
  nbColumns = CountString(header$, ",") + 1
  ; Change header row
  header$ + #Separator$ + "SUM"
  ; Write to output file
  WriteStringN(1, header$)

  ; Read remaining rows, process and write to output file
  While Not Eof(0)
    row$ = ReadString(0)
    sum = 0
    For i = 1 To nbColumns
      field$ = StringField(row$, i, #Separator$)
      sum + Val(field$)
    Next
    row$ + #Separator$ + sum
    WriteStringN(1, row$)
  Wend

  CloseFile(0)
  CloseFile(1)

  Finish:
  PrintN("")
  PrintN("Press any key to close the console")
  Repeat: Delay(10) : Until Inkey() <> ""
  CloseConsole()
EndIf

```


```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## Python



###  Using <code>fileinput</code>

Note that the [http://docs.python.org/3.3/library/csv.html csv module] is not required for such a simple and regular CSV file. Here overwriting is done in place.

```python
import fileinput

changerow, changecolumn, changevalue = 2, 4, '"Spam"'

with fileinput.input('csv_data_manipulation.csv', inplace=True) as f:
    for line in f:
        if fileinput.filelineno() == changerow:
            fields = line.rstrip().split(',')
            fields[changecolumn-1] = changevalue
            line = ','.join(fields) + '\n'
        print(line, end='')
```

After this the data file <code>csv_data_manipulation.csv</code> gets changed from that of the task to:

```txt
C1,C2,C3,C4,C5
1,5,9,"Spam",17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20
```


=== Using <code>csv</code>, <code>pathlib</code> and <code>tempfile</code> ===
In this example overwriting is performed ''not'' in place but by using [https://docs.python.org/library/tempfile.html <code>tempfile</code> library] for creating a temporary file and [https://docs.python.org/library/pathlib.html <code>pathlib</code> library] for overwriting the initial file. [http://docs.python.org/library/csv.html <code>csv</code> module] is used to allow easier manipulation with delimiters.

```python
import csv
from pathlib import Path
from tempfile import NamedTemporaryFile

filepath = Path('data.csv')
temp_file = NamedTemporaryFile('w',
                               newline='',
                               delete=False)

with filepath.open() as csv_file, temp_file:
    reader = csv.reader(csv_file)
    writer = csv.writer(temp_file)

    header = next(reader)
    writer.writerow(header + ['SUM'])

    for row in reader:
        row_sum = sum(map(int, row))
        writer.writerow(row + [row_sum])

temp_file_path = Path(temp_file.name)
temp_file_path.replace(filepath)

```

```txt
C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
```



###  Using <code>pandas</code>


```python
import pandas as pd

filepath = 'data.csv'

df = pd.read_csv(filepath)
rows_sums = df.sum(axis=1)
df['SUM'] = rows_sums
df.to_csv(filepath, index=False)
```



## Q


```q
t:("IIIII";enlist ",")0: `:input.csv    / Read CSV file input.csv into table t
t:update SUM:sum value flip t from t    / Add SUM column to t
`:output.csv 0: csv 0: t                / Write updated table as CSV to output.csv
```



## R


```R

df <- read.csv(textConnection(
"C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20"))

df <- transform(df,SUM = rowSums(df))

write.csv(df,row.names = FALSE)

```


```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```


This output can also be saved to a file:


```R
 write.csv(df,row.names = FALSE,file = "foo.csv")
```



## Racket


```racket
#lang racket
(require (planet neil/csv:1:=7) net/url)

(define make-reader
  (make-csv-reader-maker
   '((separator-chars              #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define (all-rows port)
  (define read-row (make-reader port))
  (define head (append (read-row) '("SUM")))
  (define rows (for/list ([row (in-producer read-row '())])
                 (define xs (map string->number row))
                 (append row (list (~a (apply + xs))))))
  (define (->string row) (string-join row "," #:after-last "\n"))
  (string-append* (map ->string (cons head rows))))
```

Example:

```racket
(define csv-file
  "C1, C2, C3, C4, C5
    1,  5,  9, 13, 17
    2,  6, 10, 14, 18
    3,  7, 11, 15, 19
    4,  8, 12, 16, 20")

(display (all-rows (open-input-string csv-file)))
```

```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## Red


```Red
>>
filein: read/lines %file.csv
>>data: copy []
>>foreach item filein [append/only data split item ","]
; [["C1" "C2" "C3" "C4" "C5"] ["1" "5" "9" "13" "17"] ["2" "6" "10" "14" "18"] ["3" "7" "11" "15" "19"]["4" "8" "12" "16" "20"]]
```



```Red
>>
forall data [either (index? data) = 1[
	append data/1 "SUM"
][
	append data/1 to string!
	(to integer! data/1/1) + (to integer! data/1/2) + (to integer! data/1/3) + (to integer! data/1/4) + (to integer! data/1/5)
]]
```


```Red
>>
foreach item data [append item/6 "^/" repeat c 5 [append item/:c ","]]
>> print data
C1, C2, C3, C4, C5, SUM
1, 5, 9, 13, 17, 45
2, 6, 10, 14, 18, 50
3, 7, 11, 15, 19, 55
4, 8, 12, 16, 20, 60
>>write fileout.csv form data
```



## REXX


### version 1


```rexx
/* REXX ***************************************************************
* extend in.csv to add a column containing the sum of the lines' elems
* 21.06.2013 Walter Pachl
**********************************************************************/
csv='in.csv'
Do i=1 By 1 While lines(csv)>0
  l=linein(csv)
  If i=1 Then
    l.i=l',SUM'
  Else Do
    ol=l
    sum=0
    Do While l<>''
      Parse Var l e ',' l
      sum=sum+e
      End
    l.i=ol','sum
    End
  End
Call lineout csv
'erase' csv
Do i=1 To i-1
  Call lineout csv,l.i
  End
```

```txt
C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
```



### version 2

This REXX version has no need to use an operating system command to ERASE the (input) file,   it uses instead

an idiomatic REXX method to write to the input file   (starting at record one).

Also supported is the ability to specify the fileID of the data file to be specified.

```rexx
/*REXX program reads a CSV file & appends a SUM column (which is the sum of all columns)*/
parse arg iFID .                                  /*obtain optional argument from the CL*/
if iFID=='' | iFID==","  then iFID= 'CSV_SUM.DAT' /*Not specified?  Then use the default*/
call linein iFID,1,0                              /*position the input file to line one.*/
                                                  /* [↑]  only needed if pgm is nested. */
    do rec=1  while lines(iFID)\==0               /*read the input file  (all records). */
    x= linein(iFid);     y= translate(x, , ',')   /*read a rec; change commas to blanks.*/
    $= 0                                          /*initial the sum to zero.            */
                         do j=1  for words(y);     _= word(y, j)    /*get a CSV value.  */
                         if datatype(_, 'N')  then $= $ + _         /*Numeric? Add to $.*/
                                              else $= 'SUM'         /*Not? Append "SUM".*/
                         end   /*j*/
    @.rec = x','$                                 /*append the   sum   to the record.   */
    end   /*rec*/                                 /*Note: at EOF,  REC ≡ # of records+1.*/
say rec-1    ' records read from: '      iFID     /* [↓]  this elides the need for ERASE*/
call lineout iFID,@.1,1                           /*set file ptr to 1st rec., write hdr.*/
                         do k=2  for rec-2        /*process all the records just read.  */
                         call lineout iFID,@.k    /*write the new CSV record (has SUM). */
                         end   /*k*/              /*stick a fork in it,  we're all done.*/
```

```txt

5  records read from:  CSV_SUM.DAT

```

## Ring


```ring

# Project : CSV data manipulation

load "stdlib.ring"
fnin = "input.csv"
fnout = "output.csv"
fpin = fopen(fnin,"r")
fpout = fopen(fnout,"r")
csv = read(fnin)
nr = 0
csvstr = ""

while not feof(fpin)
        sum = 0
        nr = nr + 1
        line = readline(fpin)
        if nr = 1
           line = substr(line,nl,"")
           line = line + ",SUM"
           csvstr = csvstr + line + windowsnl()
        else
           csvarr = split(line,",")
           for n = 1 to len(csvarr)
                sum = sum + csvarr[n]
           next
           line = substr(line,nl,"")
           line = line + "," + string(sum)
           csvstr = csvstr + line + windowsnl()
        ok
end
write(fnout,csvstr)
csvend = read(fnout)
fclose(fpin)
fclose(fpout)
see csvend + nl

```

Output:

```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## Ruby


```ruby
require 'csv'
# read:
ar = CSV.table("test.csv").to_a #table method assumes headers and converts numbers if possible.

# manipulate:
ar.first << "SUM"
ar[1..-1].each{|row| row << row.inject(:+)}

# write:
CSV.open("out.csv", 'w') do |csv|
  ar.each{|line| csv << line}
end
```

```txt
c1,c2,c3,c4,c5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
```



## Run BASIC


```runbasic
csv$ = "C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20
"

print csv$
dim csvData$(5,5)

for r = 1 to 5
  a$ = word$(csv$,r,chr$(13))
  for c = 1 to 5
    csvData$(r,c) = word$(a$,c,",")
  next c
next r

[loop]
input "Row to change:";r
input "Col to change;";c
if r > 5 or c > 5 then
  print "Row ";r;" or Col ";c;" is greater than 5"
  goto [loop]
end if
input "Change Row ";r;" Col ";c;" from ";csvData$(r,c);" to ";d$
csvData$(r,c) = d$
for r = 1 to 5
  for c = 1 to 5
    print cma$;csvData$(r,c);
    cma$ = ","
   next c
   cma$ = ""
   print
next r
```


```txt
C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20

Row to change:?4
Col to change;?4
Change Row 4 Col 4 from 15 to ?99
C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,99,19
4,8,12,16,20
```



## Rust

```rust
use std::error::Error;
use std::num::ParseIntError;
use csv::{Reader, Writer};

fn main() -> Result<(), Box<dyn Error>> {
    let mut reader = Reader::from_path("data.csv")?;
    let mut writer = Writer::from_path("output.csv")?;

    // headers() returns an immutable reference, so clone() before appending
    let mut headers = reader.headers()?.clone();
    headers.push_field("SUM");
    writer.write_record(headers.iter())?;

    for row in reader.records() {
        let mut row = row?;

        // `sum` needs the type annotation so that `parse::<i64>` knows what error type to return
        let sum: Result<_, ParseIntError> = row.iter().try_fold(0, |accum, s| {
            Ok(accum + s.parse::<i64>()?)
        });

        row.push_field(&sum?.to_string());
        writer.write_record(row.iter())?;
    }

    writer.flush()?;
    Ok(())
}
```

```txt
C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
```



## SAS


```sas
data _null_;
infile datalines dlm="," firstobs=2;
file "output.csv" dlm=",";
input c1-c5;
if _n_=1 then put "C1,C2,C3,C4,C5,Sum";
s=sum(of c1-c5);
put c1-c5 s;
datalines;
C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20
;
run;
```



## Scala

```Scala
import scala.io.Source

object parseCSV extends App {

  val rawData = """|C1,C2,C3,C4,C5
		  		   |1,5,9,13,17
		  		   |2,6,10,14,18
		  		   |3,7,11,15,19
		  		   |20,21,22,23,24""".stripMargin

  val data = Seq((Source.fromString(rawData).getLines()).map(_.split(",")).toSeq: _*)

  val output = ((data.take(1).flatMap(x => x) :+ "SUM").mkString(",") +: // Header line
    data.drop(1).map(_.map(_.toInt)). // Convert per line each array of String to array of integer
    map(cells => (cells, cells.sum)). //Add sum column to assemble a tuple. Part 1 are original numbers, 2 is the sum
    map(part => s"${part._1.mkString(",")},${part._2}")).mkString("\n")

  println(output)
  /* Outputs:

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
20,21,22,23,24,110

*/
}
```



## Seed7

The program below assumes that the input file has the name csvDataManipulation.in and
is in the same directory as the program.


```seed7
$ include "seed7_05.s7i";

const proc: main is func
  local
    var file: input is STD_NULL;
    var array array string: csvData is 0 times 0 times "";
    var integer: line is 0;
  begin
    input := open(dir(PROGRAM) & "/csvDataManipulation.in", "r");
    while hasNext(input) do
      csvData &:= split(getln(input), ",");
    end while;
    csvData[3][3] := "X";
    for key line range csvData do
      writeln(join(csvData[line], ","));
    end for;
  end func;
```


```txt

C1,C2,C3,C4,C5
1,5,9,13,17
2,6,X,14,18
3,7,11,15,19
4,8,12,16,20

```



## Sidef

For simple files we can use the ''split'' method.

```ruby
# Read
var csvfile = %f'data.csv';
var fh = csvfile.open_r;
var header = fh.line.trim_end.split(',');
var csv = fh.lines.map { .trim_end.split(',').map{.to_num} };
fh.close;

# Write
var out = csvfile.open_w;
out.say([header..., 'SUM'].join(','));
csv.each { |row| out.say([row..., row.sum].join(',')) };
out.close;
```


For complex files, the ''Text::CSV'' library is recommended.

```ruby
var csv = require('Text::CSV').new(
    Hash(eol => "\n")
);

# Open
var csvfile = %f'data.csv';
var fh = csvfile.open_r;

# Read
var rows = [];
var header = csv.getline(fh);
while (var row = csv.getline(fh)) {
    rows.append(row.map{.to_num});
}

# Process
header.append('SUM');
rows.each { |row| row.append(row.sum) };

# Write
var out = csvfile.open_w;
[header, rows...].each { |row|
    csv.print(out, row);
};
```



## Stata


```stata
import delim input.csv, clear
replace c5=c3+c4
egen sum=rowtotal(c*)
drop if mod(c3,3)==0
export delim output.csv, replace
```



## Tcl

```tcl
package require struct::matrix
package require csv

proc addSumColumn {filename {title "SUM"}} {
    set m [struct::matrix]

    # Load the CSV in
    set f [open $filename]
    csv::read2matrix $f $m "," auto
    close $f

    # Add the column with the sums
    set sumcol [$m columns]
    $m add column $title
    for {set i 1} {$i < [$m rows]} {incr i} {
	# Fill out a dummy value
	$m set cell $sumcol $i 0
	$m set cell $sumcol $i [tcl::mathop::+ {*}[$m get row $i]]
    }

    # Write the CSV out
    set f [open $filename w]
    csv::writematrix $m $f
    close $f

    $m destroy
}

addSumColumn "example.csv"
```

```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```


Although, for this specific small task,

```tcl
set f [open example.csv r]
puts "[gets $f],SUM"
while { [gets $f row] > 0 } {
	puts "$row,[expr [string map {, +} $row]]"
}
close $f
```

suffices.


## TUSCRIPT


```tuscript

$$ MODE DATA
$$ csv=*
C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20
$$ MODE TUSCRIPT
LOOP/CLEAR n,line=csv
 IF (n==1) THEN
  line=CONCAT (line,",SUM")
 ELSE
  lineadd=EXCHANGE(line,":,:':")
  sum=SUM(lineadd)
  line=JOIN(line,",",sum)
 ENDIF
 csv=APPEND(csv,line)
ENDLOOP

```

```txt

C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```



## TXR


```txr
@(coll)@{name /[^,]+/}@(end)
@(collect :vars (value sum))
@  (bind sum 0)
@  (coll)@{value /[^,]+/}@(set sum @(+ sum (int-str value)))@(end)
@(end)
@(output)
@  (rep)@name,@(last)SUM@(end)
@  (repeat)
@    (rep)@value,@(last)@sum@(end)
@  (end)
@(end)

```



## UNIX Shell

```bash
exec 0<"$1"                 # open the input file on stdin
exec 1>"$1.new"             # open an output file on stdout
{
    read -r header
    echo "$header,SUM"
    IFS=,
    while read -r -a numbers; do
        sum=0
        for num in "${numbers[@]}"; do
            (( sum += num ))
        done

        # can write the above loop as
        #   sum=$(( $(IFS=+; echo "${numbers[*]}") ))

        echo "${numbers[*]},$sum"
    done
} &&
mv "$1" "$1.bak" &&
mv "$1.new" "$1"
```


To make this work with ksh, change
 read -a
to
 read -A


## Ursa


```ursa
#
# csv data manipulation
#

# declare a string stream to hold lines
decl string<> lines

# open the file specified on the command line, halting
# execution if they didn't enter one. it will be created if
# it doesn't exist yet
decl file f
if (< (size args) 2)
        out "error: please specify a csv file" endl console
        stop
end if
f.create args<1>
f.open args<1>

# read in all lines from the file
set lines (f.readlines)

# append sum column to header
set lines<0> (+ lines<0> ",SUM")

# determine sums and append them
decl int i sum
for (set i 1) (< i (size lines)) (inc i)
        set sum 0
        for (decl int j) (< j (size (split lines<i> ","))) (inc j)
                set sum (int (+ sum (int (split lines<i> ",")<j>)))
        end for
        set lines<i> (+ lines<i> (+ "," sum))
end for

# delete the file, then create it again
f.delete args<1>
f.create args<1>

# output all lines to the file
for (set i 0) (< i (size lines)) (inc i)
        out lines<i> endl f
end for
```



## VBA

Using Excel VBA to load a CSV file in a new workbook.


```vb
Sub ReadCSV()
    Workbooks.Open Filename:="L:\a\input.csv"
    Range("F1").Value = "Sum"
    Range("F2:F5").Formula = "=SUM(A2:E2)"
    ActiveWorkbook.SaveAs Filename:="L:\a\output.csv", FileFormat:=xlCSV
    ActiveWindow.Close
End Sub
```



## VBScript


```vb
'Instatiate FSO.
Set objFSO = CreateObject("Scripting.FileSystemObject")
'Open the CSV file for reading. The file is in the same folder as the script and named csv_sample.csv.
Set objInCSV = objFSO.OpenTextFile(objFSO.GetParentFolderName(WScript.ScriptFullName) & "\csv_sample.csv",1,False)
'Set header status to account for the first line as the column headers.
IsHeader = True
'Initialize the var for the output string.
OutTxt = ""
'Read each line of the file.
Do Until objInCSV.AtEndOfStream
	line = objInCSV.ReadLine
	If IsHeader Then
		OutTxt = OutTxt & line & ",SUM" & vbCrLf
		IsHeader = False
	Else
		OutTxt = OutTxt & line & "," & AddElements(line) & vbCrLf
	End If
Loop
'Close the file.
objInCSV.Close
'Open the same file for writing.
Set objOutCSV = objFSO.OpenTextFile(objFSO.GetParentFolderName(WScript.ScriptFullName) & "\csv_sample.csv",2,True)
'Write the var OutTxt to the file overwriting existing contents.
objOutCSV.Write OutTxt
'Close the file.
objOutCSV.Close
Set objFSO = Nothing

'Routine to add each element in a row.
Function AddElements(s)
	arr = Split(s,",")
	For i = 0 To UBound(arr)
		AddElements = AddElements + CInt(arr(i))
	Next
End Function
```


```txt

C1,C2,C3,C4,C5
1,5,9,13,17
2,6,10,14,18
3,7,11,15,19
4,8,12,16,20

```


```txt
C1,C2,C3,C4,C5,SUM
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60
```



## Vedit macro language

This example adds 100 to the values in each cell at row n+1, column n.

```vedit
File_Open("input.csv")
for (#1 = 0; #1 < 4; #1++) {
    Goto_Line(#1+2)                             // line (starting from line 2)
    if (#1) {
        Search(",", ADVANCE+COUNT, #1)          // column
    }
    #2 = Num_Eval()                             // #2 = old value
    Del_Char(Chars_Matched)                     // delete old value
    Num_Ins(#2+100, LEFT+NOCR)                  // write new value
}
File_Save_As("output.csv", OK+NOMSG)
```

output.csv:

```txt

C1,C2,C3,C4,C5
101,5,9,13,17
2,106,10,14,18
3,7,111,15,19
4,8,12,116,20

```


## Visual FoxPro


```vfp

CLOSE DATABASES ALL
SET SAFETY OFF
MODIFY FILE file1.csv NOEDIT
*!* Create a cursor with integer columns
CREATE CURSOR tmp1 (C1 I, C2 I, C3 I, C4 I, C5 I)
APPEND FROM file1.csv TYPE CSV
SELECT C1, C2, C3, C4, C5, C1+C2+C3+C4+C5 As sum ;
FROM tmp1 INTO CURSOR tmp2
COPY TO file2.csv TYPE CSV
MODIFY FILE file2.csv NOEDIT IN SCREEN
SET SAFETY ON

```



## zkl


```zkl
csvFile:=File("test.csv");
header:=csvFile.readln().strip(); // remove trailing "\n" and leading white space
listOfLines:=csvFile.pump(List,fcn(line){ line.strip().split(",").apply("toInt") });

newFile:=File("test2.csv","w");
newFile.writeln(header + ",sum");
listOfLines.pump(newFile.writeln,fcn(ns){ String(ns.concat(","),",",ns.sum()) });
newFile.close();
```

```txt

$ cat test2.csv
C1,C2,C3,C4,C5,sum
1,5,9,13,17,45
2,6,10,14,18,50
3,7,11,15,19,55
4,8,12,16,20,60

```

