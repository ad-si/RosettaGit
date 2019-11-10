+++
title = "Simple database"
description = ""
date = 2018-12-08T10:24:42Z
aliases = []
[extra]
id = 10750
[taxonomies]
categories = []
tags = []
+++

{{task|Data Structures}}

;Task:
Write a simple tool to track a small set of data.

The tool should have a command-line interface to enter at least two different values.

The entered data should be stored in a structured format and saved to disk.

It does not matter what kind of data is being tracked.   It could be a collection (CDs, coins, baseball cards, books), a diary, an electronic organizer (birthdays/anniversaries/phone numbers/addresses), etc.


You should track the following details:
* A description of the item. (e.g., title, name)
* A category or tag (genre, topic, relationship such as “friend” or “family”)
* A date (either the date when the entry was made or some other date that is meaningful, like the birthday); the date may be generated or entered manually
* Other optional fields



The command should support the following [[Command-line arguments]] to run:
* Add a new entry
* Print the latest entry
* Print the latest entry for each category
* Print all entries sorted by a date



The category may be realized as a tag or as structure (by making all entries in that category subitems)

The file format on disk should be human readable, but it need not be standardized.   A natively available format that doesn't need an external library is preferred.   Avoid developing your own format if you can use an already existing one.   If there is no existing format available, pick one of:
:::*   [[JSON]]
:::*   [[S-Expressions]]
:::*   [[YAML]]
:::*   [[wp:Comparison_of_data_serialization_formats|others]]


;Related task:
*   [[Take notes on the command line]]





## Bracmat

This is a rather minimal solution. The program is run from the command line of the operating system, in this example the Windows command prompt. The program is stored in a file called 'sdb':

```bracmat
  whl
' ( arg$:?command
  & ( get'db
    | (db=1)&lst$(db,db,NEW)
    )
  &   !command
    : (   add
        & :?name:?tag:?date
        &   whl
          ' ( arg$:?argmnt
            & arg$:?value
            &   (!argmnt.!value)
              : ( (title|name.?name)
                | (category|tag.?tag)
                | (date.?date)
                )
            )
        & (   !name:~
            & !tag:~
            & !date:~
            & (   !db:?*!tag^(?+(!date.!name)+?)*?
                & out$"This record already exists"
              |   !tag^(!date.!name)*!db:?db
                & lst$(db,db,NEW)
              )
          | out$"invalid data"
          )
      |   latest
        & :?date
        & nothing found:?latest
        & (   !db
            :   ?
              *   ?tag
                ^ ( ?
                  + ( (>!date:?date.?name)
                    & (!name,!tag,!date):?latest
                    & ~
                    )
                  + ?
                  )
              * ?
          | out$!latest
          )
      |   latest/category
        & :?date:?latests:?latest
        & (   !db
            :   ?
              *   ( ?tag
                  & !latests !latest:?latests
                  & :?latest:?date
                  )
                ^ ( ?
                  + ( (>!date:?date.?name)
                    & (!name,!tag,!date):?latest
                    & ~
                    )
                  + ?
                  )
              * ?
          | !latests !latest:?latests&out$!latests
          )
      |   sorted
        & 0:?sorted
        & (   !db
            :   ?
              *   ?tag
                ^ ( ?
                  + ( (?date.?name)
                    & (!date.!name,!tag,!date)+!sorted:?sorted
                    & ~
                    )
                  + ?
                  )
              * ?
          |   whl
            ' (!sorted:(?.?row)+?sorted&out$!row)
          )
      )
  );

```

First we add some records, a some ships that arrived at the harbour in Rotterdam today.

```txt
bracmat "get$sdb" add name "CORONA BULKER" tag "BULK CARRIER" date "2014.10.21.04:00"
bracmat "get$sdb" add name "FPMC 21" tag "CHEMICAL TANKER" date "2014.10.15.12:00"
bracmat "get$sdb" add name "CHINA PROGRESS" tag "BULK CARRIER" date "2014.10.13.22:00"
bracmat "get$sdb" add name "FAIRCHEM YUKA" tag "CHEMICAL TANKER" date "2014.10.13.12:00"
bracmat "get$sdb" add name "NAVE COSMOS" tag "CHEMICAL TANKER" date "2014.10.13.10:00"
bracmat "get$sdb" add name "GOLDEN ICE" tag "BULK CARRIER" date "2014.10.10.12:00"
bracmat "get$sdb" add name "GHAZAL" tag "CRUDE OIL" date "2014.10.10.12:00"
bracmat "get$sdb" add name "HS MEDEA" tag "CRUDE OIL" date "2014.10.10.02:00"
```

Instead of 'name' you can use 'title' and instead of 'tag' you can use 'category'. The date has to be year first and day last, followed by time information if you like. No attempt is made to validate the date/time. Now the queries:

```txt
prompt> bracmat "get$sdb" latest
CORONA BULKER,BULK CARRIER,2014.10.21.04:00

prompt> bracmat "get$sdb" latest/category
  (CORONA BULKER,BULK CARRIER,2014.10.21.04:00)
  (FPMC 21,CHEMICAL TANKER,2014.10.15.12:00)
  (GHAZAL,CRUDE OIL,2014.10.10.12:00)

prompt> bracmat "get$sdb" sorted
HS MEDEA,CRUDE OIL,2014.10.10.02:00
GHAZAL,CRUDE OIL,2014.10.10.12:00
GOLDEN ICE,BULK CARRIER,2014.10.10.12:00
NAVE COSMOS,CHEMICAL TANKER,2014.10.13.10:00
FAIRCHEM YUKA,CHEMICAL TANKER,2014.10.13.12:00
CHINA PROGRESS,BULK CARRIER,2014.10.13.22:00
FPMC 21,CHEMICAL TANKER,2014.10.15.12:00
CORONA BULKER,BULK CARRIER,2014.10.21.04:00
```


The database file 'db' looks like this:

```txt
db= "BULK CARRIER"
  ^ ( ("2014.10.10.12:00"."GOLDEN ICE")
    + ("2014.10.13.22:00"."CHINA PROGRESS")
    + ("2014.10.21.04:00"."CORONA BULKER")
    )
*   "CHEMICAL TANKER"
  ^ ( ("2014.10.13.10:00"."NAVE COSMOS")
    + ("2014.10.13.12:00"."FAIRCHEM YUKA")
    + ("2014.10.15.12:00"."FPMC 21")
    )
* "CRUDE OIL"^(("2014.10.10.02:00"."HS MEDEA")+("2014.10.10.12:00".GHAZAL));
```


Use is made of Bracmat's automatic normalization of algebraic formula to turn the data into a hierarchical structure, with the tag as the top level and the date/time immediately below that level.


## C

A simple database in C with some error checking, even. A quick test with Valgrind revealed no obvious memory leaks. The following data was used for testing. -> database.csv

 "Soon Rising","Dee","Lesace","10-12-2000","New Hat Press"
 "Brave Chicken","Tang","Owe","04-01-2008","Nowhere Press"
 "Aardvark Point","Dee","Lesace","5-24-2001","New Hat Press"
 "Bat Whisperer, The","Tang","Owe","01-03-2004","Nowhere Press"
 "Treasure Beach","Argus","Jemky","09-22-1999","Lancast"


```C>#include <stdio.h

#include <stdlib.h> /* malloc */
#include <string.h> /* strlen */
#define _XOPEN_SOURCE /* requred for time functions */
#define __USE_XOPEN
#include <time.h>
#define DB "database.csv" /* database name */
#define TRY(a)  if (!(a)) {perror(#a);exit(1);}
#define TRY2(a) if((a)<0) {perror(#a);exit(1);}
#define FREE(a) if(a) {free(a);a=NULL;}
#define sort_by(foo) \
static int by_##foo (const void*p1, const void*p2) { \
    return strcmp ((*(const pdb_t*)p1)->foo, (*(const pdb_t*)p2)->foo); }
typedef struct db {
    char title[26];
    char first_name[26];
    char last_name[26];
    time_t date;
    char publ[100];
    struct db *next;
}
db_t,*pdb_t;
typedef int (sort)(const void*, const void*);
enum {CREATE,PRINT,TITLE,DATE,AUTH,READLINE,READ,SORT,DESTROY};
static pdb_t dao (int cmd, FILE *f, pdb_t db, sort sortby);
static char *time2str (time_t *time);
static time_t str2time (char *date);
/* qsort callbacks */
sort_by(last_name);
sort_by(title);
static int by_date(pdb_t *p1, pdb_t *p2);
/* main */
int main (int argc, char **argv) {
    char buf[100];
    const char *commands[]={"-c", "-p", "-t", "-d", "-a", NULL};
    db_t db;
    db.next=NULL;
    pdb_t dblist;
    int i;
    FILE *f;
    TRY (f=fopen(DB,"a+"));
    if (argc<2) {
usage:  printf ("Usage: %s [commands]\n"
        "-c  Create new entry.\n"
        "-p  Print the latest entry.\n"
        "-t  Print all entries sorted by title.\n"
        "-d  Print all entries sorted by date.\n"
        "-a  Print all entries sorted by author.\n",argv[0]);
        fclose (f);
        return 0;
    }
    for (i=0;commands[i]&&strcmp(argv[1],commands[i]);i++);
    switch (i) {
        case CREATE:
        printf("-c  Create a new entry.\n");
        printf("Title           :");if((scanf(" %25[^\n]",db.title     ))<0)break;
        printf("Author Firstname:");if((scanf(" %25[^\n]",db.first_name))<0)break;
        printf("Author Lastname :");if((scanf(" %25[^\n]",db.last_name ))<0)break;
        printf("Date 10-12-2000 :");if((scanf(" %10[^\n]",buf          ))<0)break;
        printf("Publication     :");if((scanf(" %99[^\n]",db.publ      ))<0)break;
        db.date=str2time (buf);
        dao (CREATE,f,&db,NULL);
        break;
        case PRINT:
        printf ("-p  Print the latest entry.\n");
        while (!feof(f)) dao (READLINE,f,&db,NULL);
        dao (PRINT,f,&db,NULL);
        break;
        case TITLE:
        printf ("-t  Print all entries sorted by title.\n");
        dblist = dao (READ,f,&db,NULL);
        dblist = dao (SORT,f,dblist,by_title);
        dao (PRINT,f,dblist,NULL);
        dao (DESTROY,f,dblist,NULL);
        break;
        case DATE:
        printf ("-d  Print all entries sorted by date.\n");
        dblist = dao (READ,f,&db,NULL);
        dblist = dao (SORT,f,dblist,(int (*)(const void *,const  void *)) by_date);
        dao (PRINT,f,dblist,NULL);
        dao (DESTROY,f,dblist,NULL);
        break;
        case AUTH:
        printf ("-a  Print all entries sorted by author.\n");
        dblist = dao (READ,f,&db,NULL);
        dblist = dao (SORT,f,dblist,by_last_name);
        dao (PRINT,f,dblist,NULL);
        dao (DESTROY,f,dblist,NULL);
        break;
        default: {
            printf ("Unknown command: %s.\n",strlen(argv[1])<10?argv[1]:"");
            goto usage;
    }   }
    fclose (f);
    return 0;
}
/* Data Access Object (DAO) */
static pdb_t dao (int cmd, FILE *f, pdb_t in_db, sort sortby) {
    pdb_t *pdb=NULL,rec=NULL,hd=NULL;
    int i=0,ret;
    char buf[100];
    switch (cmd) {
        case CREATE:
        fprintf (f,"\"%s\",",in_db->title);
        fprintf (f,"\"%s\",",in_db->first_name);
        fprintf (f,"\"%s\",",in_db->last_name);
        fprintf (f,"\"%s\",",time2str(&in_db->date));
        fprintf (f,"\"%s\" \n",in_db->publ);
        break;
        case PRINT:
        for (;in_db;i++) {
            printf ("Title       : %s\n",     in_db->title);
            printf ("Author      : %s %s\n",  in_db->first_name, in_db->last_name);
            printf ("Date        : %s\n",     time2str(&in_db->date));
            printf ("Publication : %s\n\n",   in_db->publ);
            if (!((i+1)%3)) {
                printf ("Press Enter to continue.\n");
                ret = scanf ("%*[^\n]");
                if (ret<0) return rec; /* handle EOF */
                else getchar();
            }
            in_db=in_db->next;
        }
        break;
        case READLINE:
        if((fscanf(f," \"%[^\"]\",",in_db->title     ))<0)break;
        if((fscanf(f," \"%[^\"]\",",in_db->first_name))<0)break;
        if((fscanf(f," \"%[^\"]\",",in_db->last_name ))<0)break;
        if((fscanf(f," \"%[^\"]\",",buf              ))<0)break;
        if((fscanf(f," \"%[^\"]\" ",in_db->publ      ))<0)break;
        in_db->date=str2time (buf);
        break;
        case READ:
        while (!feof(f)) {
            dao (READLINE,f,in_db,NULL);
            TRY (rec=malloc(sizeof(db_t)));
            *rec=*in_db; /* copy contents */
            rec->next=hd;/* to linked list */
            hd=rec;i++;
        }
        if (i<2) {
            puts ("Empty database. Please create some entries.");
            fclose (f);
            exit (0);
        }
        break;
        case SORT:
        rec=in_db;
        for (;in_db;i++) in_db=in_db->next;
        TRY (pdb=malloc(i*sizeof(pdb_t)));
        in_db=rec;
        for (i=0;in_db;i++) {
            pdb[i]=in_db;
            in_db=in_db->next;
        }
        qsort (pdb,i,sizeof in_db,sortby);
        pdb[i-1]->next=NULL;
        for (i=i-1;i;i--) {
            pdb[i-1]->next=pdb[i];
        }
        rec=pdb[0];
        FREE (pdb);
        pdb=NULL;
        break;
        case DESTROY: {
            while ((rec=in_db)) {
                in_db=in_db->next;
                FREE (rec);
    }   }   }
    return rec;
}
/* convert numeric time to date string */
static char *time2str (time_t *time) {
    static char buf[255];
    struct tm *ptm;
    ptm=localtime (time);
    strftime(buf, 255, "%m-%d-%Y", ptm);
    return buf;
}
/* convert date string to numeric time */
static time_t str2time (char *date) {
    struct tm tm;
    memset (&tm, 0, sizeof(struct tm));
    strptime(date, "%m-%d-%Y", &tm);
    return mktime(&tm);
}
/* sort by date callback for qsort */
static int by_date (pdb_t *p1, pdb_t *p2) {
    if ((*p1)->date < (*p2)->date) {
        return -1;
    }
    else return ((*p1)->date > (*p2)->date);
}
```



## C#


```C sharp

using System;
using System.IO;

namespace Simple_database
{
	class Program
	{
		public static void Main(string[] args)
		{
			//
			// For appropriate use of this program
			// use standard Windows Command Processor or cmd.exe
			//
			// Create cmd.bat file at the same folder with executive version
			// of program Simple_database.exe, so when started, the correct
			// file path will be automatically set to cmd console.
			//
			// Start notepad, write only cmd.exe and save file as cmd.bat
			//
			// To start cmd just double click at cmd.bat file.
			//
			//
			//
			// Console application command line start command
			//
			// application name.exe [argument] [argument parameters]
			//
			//
			// Command line argument followed by parameters
			//
			// [a] - Add new entry
			//
			// ["data1"]["data2"]["data3"]...["data n"]
			//
			// ["data1"] - Data category !
			// ["data2"] - Data
			// ["data3"] - Data
			//
			//
			// NOTICE !
			//
			// First parameter is taken for data category.
			//
			//
			//
			// Command line argument with no parameters
			//
			// [p1] - Print the latest entry
			//
			// [p2] - Print the latest entry for each category
			//
			// [p3] - Print all entries sorted by a date
			//
			//
			//

			//
			// Command line example
			//
			// Small_database.exe [a] ["home"] ["+398125465458"] ["My tel number"]
			//
			// Small_database.exe [a] ["office"] ["+398222789000"] ["Boss"]
			//
			// Small_database.exe [a] [cd] ["Action movie"] ["Movie title"]
			// Small_database.exe [a] [cd] ["SF movie"] ["Movie title"]
			// Small_database.exe [a] [dvd] ["Action movie"] ["Movie title"]
			//
			//
			// NOTICE !
			//
			// Brackets and space between arguments and parameters are necessary.
			// Quotes must be used when parameters have more than one word.
			//
			// If not used as shown in examples, program will show error message.
			//
			//


			//
			// Check command line for arguments
			//
			//
			if(args.Length==0)
			{
				Console.WriteLine();
				Console.WriteLine(" Missing Argument Error. ");
				Console.WriteLine();
			}
			else
			{
				switch (args[0])
				{
						case "[a]" : Add_New_Entry(args);
						break;

						case "[p1]": Print_Document("Print the latest entry.txt");
						break;

						case "[p2]": Print_Document("Print the latest entry for each category.txt");
						break;

						case "[p3]": Print_Document("Print all entries sorted by a date.txt");
						break;

						default :
						{
							Console.WriteLine();
							Console.WriteLine(" Incorrect Argument Error. ");
							Console.WriteLine();
						}
						break;
				}
			}
		}

		static void Add_New_Entry(string [] args)
		{

			//
			// Check parameters
			//
			//
			// Minimum one parameter, category
			//
			if(args.Length==1)
			{
				Console.WriteLine();
				Console.WriteLine(" Missing Parameters Error..... ");
				Console.WriteLine();
			}
			else
			{
				bool parameters_ok = true;
				foreach (string a in args)
				{
					if(!a.StartsWith("[") || !a.EndsWith("]"))
					{
						parameters_ok = !parameters_ok;
						break;
					}
				}
				//
				// Add new entry to Data base document
				//
				if(parameters_ok)
				{
					//
					//
					//
					Console.WriteLine();
					Console.WriteLine(" Parameters are ok..... ");
					Console.WriteLine();
					Console.WriteLine(" Writing new entry to database..... ");
					//
					// Create new Data base entry
					//
					args[0] = string.Empty;
					string line = string.Empty;
					foreach (string a in args)
					{
						line+=a;
					}
					line+="[" + DateTime.Now.ToString() + "]";
					args[0] = "[" + DateTime.Now.ToString() + "]";
					//
					// Write entry to Data base
					//
					StreamWriter w = new StreamWriter("Data base.txt",true);
					w.WriteLine(line);
					//
					// Close and dispose stream writer
					//
					w.Close();
					w.Dispose();
					//
					//
					//
					Console.WriteLine();
					Console.WriteLine(" New entry is written to database. ");

					Create_Print_Documents(args);

					//
					//
					//
					Console.WriteLine();
					Console.WriteLine(" Add new entry command executed. ");
					//
					//
					//
				}
				else
				{
					Console.WriteLine();
					Console.WriteLine(" ! Parameters are not ok ! ");
					Console.WriteLine();
					Console.WriteLine(" Add new entry command is not executed. ");
					Console.WriteLine();
				}
			}
		}

		static void Create_Print_Documents(string [] args)
		{
			//
			//
			//
			Console.WriteLine();
			Console.WriteLine(" Creating new print documents. ");
			//
			// Create "Print all entries sorted by a date.txt"
			//
			File.Copy("Data base.txt","Print all entries sorted by a date.txt",true);
			//
			//
			//
			Console.WriteLine();
			Console.WriteLine(" Print all entries sorted by a date.txt created. ");
			//
			// Create "Print the latest entry.txt"
			//
			//
			// Create new entry
			//
			string line = string.Empty;
			foreach (string a in args)
			{
				line+=a;
			}
			//
			StreamWriter w = new StreamWriter("Print the latest entry.txt");
			//
			w.WriteLine(line);
			//
			w.Close();
			w.Dispose();
			//
			//
			//
			Console.WriteLine();
			Console.WriteLine(" Print the latest entry.txt created. ");
			//
			// Create "Print the latest entry for each category.txt"
			//
			string latest_entry = string.Empty;
			foreach (string a in args)
			{
				latest_entry+=a;
			}

			if(!File.Exists("Print the latest entry for each category.txt"))
			{
				File.WriteAllText("Print the latest entry for each category.txt",latest_entry);
			}
			else
			{
				StreamReader r = new StreamReader("Print the latest entry for each category.txt");
				//
				w = new StreamWriter("Print the latest entry for each category 1.txt",true);
				//
				line = string.Empty;
				//
				while(!r.EndOfStream)
				{
					line = r.ReadLine();
					if(line.Contains(args[1].ToString()))
					{
						w.WriteLine(latest_entry);
						latest_entry = "ok";
					}
					else
					{
						w.WriteLine(line);
					}
				}
				// add new category
				if(latest_entry != "ok")
					w.WriteLine(latest_entry);
				//
				w.Close();
				w.Dispose();
				//
				r.Close();
				r.Dispose();
				//
				File.Copy("Print the latest entry for each category 1.txt",
				          "Print the latest entry for each category.txt",true);
				//
				File.Delete("Print the latest entry for each category 1.txt");
				//
				//
				//
				Console.WriteLine();
				Console.WriteLine(" Print the latest entry for each category.txt created. ");
				Console.WriteLine();
			}
		}

		static void Print_Document(string file_name)
		{
			//
			// Print document
			//
			Console.WriteLine();
			Console.WriteLine(file_name.Replace(".txt","")+ " : ");
			Console.WriteLine();
			//
			StreamReader r = new StreamReader(file_name);
			//
			string line = string.Empty;
			//
			line = r.ReadToEnd();
			//
			Console.WriteLine(line);
			//
			r.Close();
			r.Dispose();
			//
			//
			//
		}
	}
}

```

{{out| Program Input and Output}}

```txt

Data base can be filled by creating cmd.bat file with following contents:

```


```txt

Simple_database.exe [a] [cd] ["Action"] ["Movie name"]
Simple_database.exe [a] [cd] ["Fiction"] ["Movie name"]
Simple_database.exe [a] [cd] ["Drama"] ["Movie name"]
Simple_database.exe [a] [cd] ["SF"] ["Movie name"]
Simple_database.exe [a] [cd] ["Comedy"] ["Movie name"]
Simple_database.exe [a] [cd] ["Horor"] ["Movie name"]

Simple_database.exe [a] [dvd] ["Action"] ["Movie name"] ["Fiction"] ["Movie name"]
Simple_database.exe [a] [dvd] ["SF"] ["Movie name"] ["SF"] ["Movie name"]
Simple_database.exe [a] [dvd] ["Drama"] ["Movie name"] ["Action"] ["Movie name"]
Simple_database.exe [a] [dvd] ["Comedy"] ["Movie name"] ["SF"] ["Movie name"]
Simple_database.exe [a] [dvd] ["Horor"] ["Movie name"] ["Fiction"] ["Movie name"]

pause

```


```txt

When data base is formed use start command with parameters for data base query:

C:\PROJECTS\Roseta\Simple database\Simple database\bin\Debug>Simple_database.exe [p2]

And the result is

```


```txt

Print the latest entry for each category :

[28.3.2018 15:06:18][cd][Horor][Movie name]
[28.3.2018 15:06:18][dvd][Horor][Movie name][Fiction][Movie name]

```



## COBOL

This is a souped-up version of the task from [[Take notes on the command line]]. It stores the current date, a tag, a title and  a note as an entry in a file. The database produced is not particularly human-readable or easy to modify, but it is in a well-structured format.
{{works with|OpenCOBOL}}

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. simple-database.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL database-file ASSIGN Database-Path
               ORGANIZATION INDEXED
               ACCESS SEQUENTIAL
               RECORD KEY data-title
               ALTERNATE RECORD KEY data-tag
                   WITH DUPLICATES
               ALTERNATE RECORD KEY date-added
                   WITH DUPLICATES
               FILE STATUS file-status
               .
       DATA DIVISION.
       FILE SECTION.
       FD  database-file.
       01  database-record.
           *> Date is in YYYYMMDD format.
           03  date-added          PIC 9(8).
           03  data-tag            PIC X(20).
           03  data-title          PIC X(50).
           03  data-contents       PIC X(200).
           *> Adding extra space is considered good style so the record
           *> can be expanded in the future.
           03  FILLER              PIC X(50).

       WORKING-STORAGE SECTION.
       78  Database-Path           VALUE "database.dat".

       01  file-status             PIC XX.
           88  file-ok             VALUE "00".
           88  duplicate-key       VALUE "22".
           88  key-not-found       VALUE "23".

       01  num-args                PIC 99.

       01  action                  PIC XX.
           88  create-entry        VALUE "-c".
           88  remove-entry        VALUE "-r".
           88  find-entry          VALUE "-f".
           88  print-latest        VALUE "-l".
           88  print-database      VALUES "-a", "-d", "-t".
           *> Printed by title.
           88  print-by-title      VALUE "-a".
           88  print-by-date       VALUE "-d".
           88  print-by-tag        VALUE "-t".
           88  print-help          VALUES "-h", SPACES.

       01  read-direction-flag     PIC X VALUE SPACE.
           88  read-backwards      VALUE "B".

       01  edited-date             PIC 9(4)/99/99.
       PROCEDURE DIVISION.
       DECLARATIVES.
       database-file-error SECTION.
           USE AFTER ERROR ON database-file

           DISPLAY "An error has occurred while using " Database-Path
               ". Error no. " file-status
           DISPLAY "The program will terminate."

           CLOSE database-file

           GOBACK
           .
       END DECLARATIVES.

       main-line.
           DISPLAY 1 UPON ARGUMENT-NUMBER
           ACCEPT action FROM ARGUMENT-VALUE

           ACCEPT num-args FROM ARGUMENT-NUMBER

           EVALUATE TRUE
               WHEN create-entry
                   IF num-args >= 4
                       PERFORM write-entry
                   ELSE
                       DISPLAY "-a requires arguments to enter in the "
                           "database. See help (-h) for details."
                   END-IF

               WHEN remove-entry
                   IF num-args >= 2
                       PERFORM delete-entry
                   ELSE
                       DISPLAY "-r requires the title of the entry to "
                           "delete."
                   END-IF

               WHEN find-entry
                   IF num-args >= 2
                       PERFORM display-specified-entry
                   ELSE
                       DISPLAY "-f requires the title of the entry to "
                           "find."
                   END-IF

               WHEN print-latest
                   PERFORM show-latest

               WHEN print-database
                   PERFORM show-database

               WHEN print-help
                   PERFORM show-general-help

               WHEN OTHER
                   DISPLAY action " is not a valid option."
           END-EVALUATE

           GOBACK
           .
       write-entry.
           OPEN EXTEND database-file

           DISPLAY 2 UPON ARGUMENT-NUMBER
           ACCEPT data-tag FROM ARGUMENT-VALUE
           DISPLAY 3 UPON ARGUMENT-NUMBER
           ACCEPT data-title FROM ARGUMENT-VALUE
           IF data-title = SPACES
               DISPLAY "The title cannot be blank."
               PERFORM close-and-terminate
           END-IF

           DISPLAY 4 UPON ARGUMENT-NUMBER
           ACCEPT data-contents FROM ARGUMENT-VALUE

           ACCEPT date-added FROM DATE YYYYMMDD

           WRITE database-record
               INVALID KEY
                   IF duplicate-key
                       DISPLAY "An entry in the database already has "
                           "that title. Please choose a different "
                           "title or remove the entry."
                   ELSE
                       PERFORM database-file-error
                   END-IF
           END-WRITE

           PERFORM close-database
           .
       delete-entry.
           PERFORM get-title-arg
           OPEN I-O database-file
           PERFORM read-title

           DELETE database-file

           PERFORM close-database
           .
       display-specified-entry.
           PERFORM get-title-arg
           OPEN INPUT database-file
           PERFORM read-title

           PERFORM show-record

           PERFORM close-database
           .
       get-title-arg.
           DISPLAY 2 UPON ARGUMENT-NUMBER
           ACCEPT data-title FROM ARGUMENT-VALUE
           .
       read-title.
           START database-file KEY IS = data-title
               INVALID KEY
                   IF key-not-found
                       DISPLAY "An entry with that title was not found."
                       PERFORM close-and-terminate
                   ELSE
                       PERFORM database-file-error
                   END-IF
           END-START

           READ database-file
           .
       close-and-terminate.
            PERFORM close-database
            GOBACK
            .
       show-latest.
           OPEN INPUT database-file

           PERFORM start-at-last-date
           READ database-file
           PERFORM show-record

           PERFORM close-database
           .
       show-database.
           OPEN INPUT database-file

           EVALUATE TRUE
               WHEN print-by-title
                   *> Primary key is the title.
                   CONTINUE
               WHEN print-by-tag
                   MOVE LOW-VALUES TO data-tag
                   START database-file KEY IS > data-tag
               WHEN print-by-date
                   PERFORM start-at-last-date
                   SET read-backwards TO TRUE
           END-EVALUATE

           PERFORM FOREVER
               *> The problem with statements instead of functions...
               IF NOT read-backwards
                   READ database-file NEXT
                       AT END
                           EXIT PERFORM
                   END-READ
               ELSE
                   READ database-file PREVIOUS
                       AT END
                           EXIT PERFORM
                   END-READ
               END-IF

               PERFORM show-record
               DISPLAY SPACE
           END-PERFORM

           PERFORM close-database
           .
       start-at-last-date.
           MOVE HIGH-VALUES TO date-added
           START database-file KEY IS < date-added
           .
       close-database.
           CLOSE database-file
           .
       show-record.
           MOVE date-added TO edited-date
           DISPLAY "Date added: " edited-date " Tag: " data-tag
           DISPLAY "Title: " data-title
           DISPLAY "Contents:"
           DISPLAY "  " FUNCTION TRIM(data-contents)
           .
       show-general-help.
           DISPLAY "Help: Possible options are:"
           DISPLAY "  -a - Show all the entries (sorted by title)."
           DISPLAY "  -c - Create a new entry in the database. -c needs"
               " further arguments in this format:"
           DISPLAY '    "tag" "title" "content"'
           DISPLAY "    Max argument sizes (in characters): tag - 20, "
               "title - 50, content - 200"
           DISPLAY "    The title must be unique and not be blank."
           DISPLAY "  -d - Show all the entries sorted by date added."
           DISPLAY "  -f - Finds and displays entry with the title "
               "provided. The title should be specified as shown for "
               "-c."
           DISPLAY "  -h - Show this help menu."
           DISPLAY "  -l - Show the latest entry."
           DISPLAY "  -r - Remove the entry with the title provided. "
               "The title should be specified as shown for -c."
           DISPLAY "  -t - Show all the entries sorted by tag."
           .
```


Sample session:

```txt

$ ./database -c "Reminder" "Bob's Birthday" "Buy birthday present for Bob."
$ ./database -c "Wishlist" "Beethoven" "Beethoven's Ode to Joy"
$ ./database -c "Reminder" "Add to Simple Database" "Add Brainfuck example for Simple Database on Rosetta Code."
...
$ ./database -f "Mozart"
An entry with that title was not found.
$ ./database -t
Date added: 2013/08/13 Tag: Reminder
Title: Bob's Birthday
Contents:
  Buy birthday present for Bob.

Date added: 2013/08/13 Tag: Reminder
Title: Add to Simple Database
Contents:
  Add Brainfuck example for Simple Database on Rosetta Code.

Date added: 2013/08/13 Tag: Wishlist
Title: Beethoven
Contents:
  Beethoven's Ode to Joy

$ ./database -r "Beethoven"
$ ./database -l
Date added: 2013/08/13 Tag: Reminder
Title: Add to Simple Database
Contents:
  Add Brainfuck example for Simple Database on Rosetta Code.
$

```



## Common Lisp


Tested with [[Common Lisp]]. ''(Save the code below as db.lisp)''


```lisp
(defvar *db* nil)

(defvar *db-cat* (make-hash-table :test 'equal))

(defvar *db-file* "db.txt")

(defstruct item
  "this is the unit of data stored/displayed in *db*"
  (title " ")
  (category "default")
  (date (progn (get-universal-time))))

(defun set-category(new-item)
  (setf (gethash (item-category new-item) *db-cat*) 't))

(defun find-item-in-db (&optional category)
  (if (null category)
      (car *db*)
    (find category *db* :key #'item-category :test #'string=)))

(defun scan-category ()
  "scan categories from an existing database -- after reading it from disk"
  (dolist (itm *db*) (set-category itm)))

(defun pr-univ-time (utime)
  (multiple-value-bind
   (second minute hour date month year day-of-week dst-p tz)
   (decode-universal-time utime)
   (declare (ignore day-of-week dst-p tz))
   (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month date hour minute second)))

(defun pr (&optional (item (find-item-in-db)) (stream t))
  "print an item"
  (when item
    (format stream "~a: (~a) (~a)~%"
	    (item-title item)
	    (item-category item)
	    (pr-univ-time (item-date item)))))

(defun pr-per-category ()
  "print the latest item from each category"
  (loop for k being the hash-keys in *db-cat*
	do (pr (find-item-in-db k))))

(defun pr-all ()
  "print all the items, *db* is sorted by time."
  (dolist (itm *db*)  (pr itm)))

(defun pr-all-categories (&optional (stream t))
  (loop for k being the hash-keys in *db-cat*
       do (format stream "(~a) " k)))

(defun insert-item (item)
  "insert item into database in a time sorted list. okay for a small list, as per spec."
  (let ((first-item (car *db*)) (new-itm item))
    (set-category new-itm)
    (push new-itm *db*)
    (when (and first-item (>= (item-date new-itm) (item-date first-item)))
      (setf *db* (sort *db* #'> :key #'item-date)))
    *db*))

(defun read-db-from-file (&optional (file *db-file*))
  (with-open-file (in file :if-does-not-exist nil)
		  (when in
		    (with-standard-io-syntax (setf *db* (read in)))
		    (scan-category))))

(defun save-db-to-file (&optional (file *db-file*))
  (with-open-file (out file :direction :output :if-exists :supersede)
		  (with-standard-io-syntax
		   (print *db* out))))

(defun del-db ()
  (setf *db* nil)
  (save-db-to-file))

(defun del-item (itm)
  (read-db-from-file)
  (setf *db* (remove itm *db* :key #'item-title :test #'string=))
  (save-db-to-file))

(defun add-item-to-db (args)
  (read-db-from-file)
  (insert-item (make-item :title (first args) :category (second args)))
  (save-db-to-file))

(defun help-menu ()
  (format t "clisp db.lisp ~{~15T~a~^~% ~}"
	  '("delete <item-name> -------------------  delete an item"
	    "delete-all ---------------------------  delete the database"
            "insert <item-name> <item-category> ---  insert an item with its category"
	    "show ---------------------------------  shows the latest inserted item"
	    "show-categories ----------------------  show all categories"
	    "show-all -----------------------------  show all items"
	    "show-per-category --------------------  show the latest item per category")))

(defun db-cmd-run (args)
  (cond ((and (> (length args) 1) (equal (first args) "delete"))
	 (del-item (second args)))
	((equal (first args) "delete-all") (del-db))
	((and (> (length args) 2) (equal (first args) "insert"))
	 (add-item-to-db (rest args)))
	((equal (first args) "show") (read-db-from-file) (pr))
	((equal (first args) "show-categories") (read-db-from-file) (pr-all-categories))
        ((equal (first args) "show-all") (read-db-from-file) (pr-all))
        ((equal (first args) "show-per-category") (read-db-from-file) (pr-per-category))
        (t (help-menu))))

;; modified https://rosettacode.org/wiki/Command-line_arguments#Common_Lisp
(defun db-argv ()
  (or
   #+clisp ext:*args*
   #+sbcl (cdr sb-ext:*posix-argv*)
   #+allegro (cdr (sys:command-line-arguments))
   #+lispworks (cdr sys:*line-arguments-list*)
   nil))

(db-cmd-run  (db-argv))
```



Help menu:
 $ clisp db.lisp
 clisp db.lisp delete <item-name> -------------------  delete an item
               delete-all ---------------------------  delete the database
               insert <item-name> <item-category> ---  insert an item with its category
               show ---------------------------------  shows the latest inserted item
               show-categories ----------------------  show all categories
               show-all -----------------------------  show all items
               show-per-category --------------------  show the latest item per category

Here are a few steps to add a few titles, and their categories:
 $ clisp db.lisp insert "title-vinyl-1" "vinyl"
 $ clisp db.lisp insert "title-cd-1" "cd"
 $ clisp db.lisp insert "title-dvd-1" "dvd"
 $ clisp db.lisp insert "title-tape-1" "tape"
 $ clisp db.lisp insert "title-tape-2" "tape"

Here is the very latest entry in the db:
 $ clisp db.lisp show
 title-tape-2: (tape) (2017-04-04 20:19:06)

Here is a (sorted time wise) list of all the entries:
 $ clisp db.lisp show-all
 title-tape-2: (tape) (2017-04-04 20:19:06)
 title-tape-1: (tape) (2017-04-04 20:19:00)
 title-dvd-1: (dvd) (2017-04-04 20:18:55)
 title-cd-1: (cd) (2017-04-04 20:18:48)
 title-vinyl-1: (vinyl) (2017-04-04 20:18:41)

Here is the latest entry for each category:
 $ clisp db.lisp show-per-category
 title-vinyl-1: (vinyl) (2017-04-04 20:18:41)
 title-cd-1: (cd) (2017-04-04 20:18:48)
 title-dvd-1: (dvd) (2017-04-04 20:18:55)
 title-tape-2: (tape) (2017-04-04 20:19:06)

Here is the list of all categories:
 $ clisp db.lisp show-categories
 (vinyl) (cd) (dvd) (tape)

To delete an entry:
 $ clisp db.lisp delete "title-tape-2"

To delete all entries:
 $ clisp db.lisp delete-all


## D


```d
import std.stdio, std.algorithm, std.string, std.conv, std.array,
       std.file, std.csv, std.datetime;

private {
    immutable filename = "simdb.csv";

    struct Item {
        string name, date, category;
    }

    void addItem(in string[] item) {
        if (item.length < 3)
            return printUsage();
        auto db = load();
        const date = (cast(DateTime)Clock.currTime).toISOExtString;
        const cat = (item.length == 4) ? item[3] : "none";
        db ~= Item(item[2], date, cat);
        store(db);
    }

    void printLatest(in string[] a) {
       auto db = load();
       if (db.empty)
           return writeln("No entries in database.");
       db.sort!q{ a.date > b.date };
       if (a.length == 3) {
           foreach (item; db)
               if (item.category == a[2])
                   writefln("%s, %s, %s", item.tupleof);
       } else {
           writefln("%s, %s, %s", db[0].tupleof);
       }
    }

    void printAll() {
        auto db = load();
        if (db.empty)
            return writeln("No entries in database.");
        db.sort!q{ a.date < b.date };
        foreach (item; db)
            writefln("%s, %s, %s", item.tupleof);
    }

    Item[] load() {
        Item[] db;
        if (filename.exists && filename.isFile) {
            try {
                const text = filename.readText;
                if (!text.empty)
                    db = csvReader!Item(text).array;
            } catch (CSVException e) {
                writeln(e.msg);
            }
        }
        return db;
    }

    void store(in Item[] db) {
        auto f = File(filename, "w+");
        foreach (item; db)
            f.writefln("%s,%s,%s", item.tupleof);
    }

    void printUsage() {
        writeln(
`Usage:
  simdb cmd [categoryName]

  add     add item, followed by optional category
  latest  print last added item(s), followed by optional category
  all     print all

  For instance: add "some item name" "some category name"`);
    }
}

void main(in string[] args) {
    if (args.length < 2 || args.length > 4)
        return printUsage();

    switch (args[1].toLower) {
        case "add":    addItem(args);     break;
        case "latest": printLatest(args); break;
        case "all":    printAll();        break;
        default:       printUsage();      break;
    }
}
```

{{out}}

```txt
C:\>simdb add item1 cat1

C:\>simdb add item2 cat2

C:\>simdb add item3 cat3

C:\>simdb add item4

C:\>simdb add item5

C:\>simdb add item6 cat4

C:\>simdb add item7 cat4

C:\>simdb latest
item7, 2014-06-04T16:02:01, cat4

C:\>simdb latest cat4
item7, 2014-06-04T16:02:01, cat4
item6, 2014-06-04T16:01:55, cat4

C:\>simdb all
item1, 2014-06-04T16:01:26, cat1
item2, 2014-06-04T16:01:34, cat2
item3, 2014-06-04T16:01:41, cat3
item4, 2014-06-04T16:01:46, none
item5, 2014-06-04T16:01:49, none
item6, 2014-06-04T16:01:55, cat4
item7, 2014-06-04T16:02:01, cat4

C:\>simdb add
Usage:
 simdb cmd [categoryname]

 add     add item, followed by optional category
 latest  print last added item(s), followed by optional category
 all     print all

 For instance: add "some item name" "some category name"
```

File:

```txt
item1,2014-06-04T16:01:26,cat1
item2,2014-06-04T16:01:34,cat2
item3,2014-06-04T16:01:41,cat3
item4,2014-06-04T16:01:46,none
item5,2014-06-04T16:01:49,none
item6,2014-06-04T16:01:55,cat4
item7,2014-06-04T16:02:01,cat4
```



## Erlang


```Erlang

#! /usr/bin/env escript

-compile({no_auto_import,[date/0]}).

main( ["add", Tag | Descriptions] ) -> add( date(), Tag, Descriptions );
main( ["add_date", Date, Tag | Descriptions] ) -> add( date_internal(string:tokens(Date, "-")), Tag, Descriptions );
main( ["print_latest"] ) -> print_latest( contents() );
main( ["print_latest_for_each"] ) -> print_latest_for_each( contents() );
main( ["print_all_date", Date] ) -> print_all_date( date_internal(string:tokens(Date, "-")), contents() );
main( _Error ) -> usage().



add( Date, Tag, Descriptions ) ->
	Contents = contents(),
	file:write_file( file(), io_lib:format("simple_database_v1.~n~p.~n", [[{Date, Tag, Descriptions} | Contents]]) ).

date() ->
	{{Date, _Time}} = calendar:local_time(),
	Date.

date_external( {Year, Month, Day} ) -> string:join( [erlang:integer_to_list(Year), erlang:integer_to_list(Month), erlang:integer_to_list(Day)], "-" );
date_external( _Error ) -> usage().

date_internal( [Year, Month, Day] ) -> {erlang:list_to_integer(Year), erlang:list_to_integer(Month), erlang:list_to_integer(Day)};
date_internal( _Error ) -> usage().

file() -> "simple_database_contents".

contents() -> contents( file:consult(file()) ).

contents( {ok, [simple_database_v1, Contents]} ) -> Contents;
contents( {error, Error} ) when is_atom(Error) -> [];
contents( {error, _Error} ) ->
	io:fwrite( "Error: ~p corrupt. Starting from scratch~n", [file()] ),
	[].

print_all_date( _Date, [] ) -> ok;
print_all_date( Date, Contents ) -> [print_latest([{D, Tag, Descriptions}]) || {D, Tag, Descriptions} <- Contents, D =:= Date].

print_latest( [] ) -> ok;
print_latest( [{Date, Tag, Descriptions} | _T] ) -> io:fwrite( "~s~n", [string:join( [date_external(Date), Tag | Descriptions], " ")] ).

print_latest_for_each( [] ) -> ok;
print_latest_for_each( Contents ) ->
	Tags = lists:usort( [Tag || {_Date, Tag, _Descriptions} <- Contents] ),
	[print_latest([lists:keyfind(X, 2, Contents)]) ||  X <- Tags].

usage() ->
	io:fwrite( "Usage: ~p [add | add_date <date>]  tag description ...~n", [escript:script_name()] ),
	io:fwrite( "Or: ~p [print_latest | print_latest_for_each | print_all_date <date>]~n", [escript:script_name()] ),
	io:fwrite( "Date format is YYYY-MM-DD~n" ),
	io:fwrite( "Data stored in ~p~n", [file()] ),
	init:stop().

```

Command line session started with these file contents as database:

```txt

simple_database_v1.
[{{2013,9,16},"comic",["hobbe"]},
 {{2013,9,16},"comic",["gustaf"]}].

```

Command line session:

```txt

./simple_database add num 1 2 3
./simple_database add_date 1991-01-01 num 999 888
./simple_database print_latest
1991-1-1 num 999 888
./simple_database print_latest_for_each
2013-9-16 comic hobbe
1991-1-1 num 999 888
./simple_database print_all_date 2013-09-16
2013-9-16 num 1 2 3
2013-9-16 comic hobbe
2013-9-16 comic gustaf

```




## Forth

Simple in-memory database. Load/dump database from/to a file.

{{works with|GNU Forth|Gforth|0.7}}
Example file 'test.sdb':

```txt

+: betty 1974.03.03;coworker;reading;

+: geea 1980.01.01;friend;sketch writer;

+: tom 1991.03.07;family member;reading;

+: alice 1987.09.01;coworker;classical music;

+: gammaQ3.14 3045.09.09;friend;watch movies, star walking;

```


Example usage:

```txt

$ gforth sdb.fs

help

[ -erased help message- ]

load test.sdb  ok

.keys

gammaQ3.14 alice tom geea betty  ok

betty .record betty 1974.03.03;coworker;reading; ok

newline geea .record

geea 1980.01.01;friend;sketch writer; ok

+: theea 1979.04.05;coworker;astronomy;  ok

newline .bydate

gammaQ3.14 3045.09.09;friend;watch movies, star walking;

tom 1991.03.07;family member;reading;

alice 1987.09.01;coworker;classical music;

geea 1980.01.01;friend;sketch writer;

theea 1979.04.05;coworker;astronomy;

betty 1974.03.03;coworker;reading;

dump t2.sdb  ok

bye

$

$ cat t2.sdb

+: betty 1974.03.03;coworker;reading;

+: geea 1980.01.01;friend;sketch writer;

+: tom 1991.03.07;family member;reading;

+: alice 1987.09.01;coworker;classical music;

+: gammaQ3.14 3045.09.09;friend;watch movies, star walking;

+: theea 1979.04.05;coworker;astronomy;

$

```


Code:

```forth
\ sdb.fs	Simple database. Gforth 0.7.0 specific
' noop is bootmessage

wordlist constant USER:  \ USER functions
wordlist constant SDB    \ DB keys
wordlist constant FIELDS \ DB fields

: -SDB?EXIT	( -- ; Continue if `SDB' non-empty )
	SDB cell+ @ 0= IF rdrop exit THEN ;

\ MACROS
: |comp|	( MACRO: | code to be compiled | )
	char parse evaluate ; immediate
: LIST	( node -- )	]] BEGIN @ dup WHILE >R [[ ; immediate
: LOOP-LIST	( -- )	]] R> REPEAT drop [[ ; immediate
: UNLIST	( -- )	POSTPONE rdrop ; immediate

\ Helper words
: .;	[char] ; emit ;
: !+	dup cell+ -rot ! ;
: ($+)	( cas us cad ud -- cad us+ud ; append source string to destination )
	swap >R	2dup + R> swap >R tuck + swap >R swap move R> R> ;

\ --- Working Record Fields
current @ FIELDS current !
2variable person
2variable birthday
2variable relationship
2variable hobby
current ! FIELDS >order

\ --- Ring list
: new-node	( -- node ; root of circular list )
	here dup , ;
: do-link	( node new-node  -- ; do link after current node )
	over @ over ! swap ! ;
: empty-ring?	( node -- f )	dup @ = ;

: RING	( node -- )	]] dup BEGIN @ 2dup <> WHILE 2>R [[ ; immediate
: LOOP-RING	( -- )	]] 2R> REPEAT 2drop [[ ; immediate
: UNRING	( -- )	postpone 2RDROP ; immediate

:noname
	s" " person 2!  s" 9999.01.01" birthday 2!  s" " relationship 2!  s" " hobby 2! ;
new-node swap , CONSTANT OLD

\ --- Insertion into ring
: node>xt	( node -- xt )		cell+ @ ;
: bday@		( node -- ca u )	node>xt execute birthday 2@ ;
: datecmp	( node node -- -1|0|1 )	bday@ rot bday@ compare ;

: search-by-date	( new-node node -- node' ; Linear search by birth date )
	dup
	RING	( new-node node )
		over I datecmp 0<
		IF   nip  UNRING exit
		ELSE drop I THEN
	LOOP-RING    nip ;

: insert-ordered	( new-node -- )
	OLD dup empty-ring? 0=
	IF over swap search-by-date THEN swap do-link ;

\ --- Field compiling
: fld:   [char] ; parse postpone sliteral ; immediate

: _sdb-walk	( MACRO: | xt[--] -- | )
	]] -SDB?EXIT here dup SDB cell+
	LIST I name>int swap !+ LOOP-LIST [[  1 cells ]] literal -
	DO   I @ |comp| [[ -1 cells ]] literal +LOOP [[ ; immediate

2variable category
: msg_nocategory	( -- )
	." No such category!" cr ;

\ --- USER definitions
user: >order current @ USER: current !
: bye bye ;
: .s .s ;
: newline cr ;
: godb SDB >order ;
: .keys  SDB wordlist-words ;
: .person	person  2@ type ;
: .birthday	birthday  2@ type ;
: .relationship	relationship 2@ type ;
: .hobby	hobby 2@ type ;
: .record	.person 1 spaces .birthday .; .relationship .; .hobby .; ;

: .last  SDB cell+ @ dup
	IF name>int execute .record ELSE drop THEN ;

: .lastbycategory	( "field-name" -- )
	here 0 category 2! parse-name FIELDS search-wordlist
	IF	sdb cell+
		LIST	( xt )
			I name>int execute dup execute 2@ category 2@ 2swap search nip nip 0=
			IF .record cr dup execute 2@ category 2@ ($+) category 2! THEN
		LOOP-LIST drop
	ELSE    ['] msg_nocategory stderr OUTFILE-EXECUTE THEN ;

: .bydate	( -- )
	OLD RING I node>xt execute .record cr LOOP-RING ;

: .sdb		_sdb-walk | execute .record cr | ;
: sdb-dump	_sdb-walk | execute ." +: " .record cr | ;

: dump	( "db-file" -- )
	parse-name w/o
	CREATE-FILE throw dup
		['] sdb-dump swap OUTFILE-EXECUTE
	CLOSE-FILE throw ;

: load	( "db-file" -- )	include ;

: +:	( "person birth-day;relationship;hobby; -- )
	new-node dup cell+ >R 0 ,			\ post    `xt'
	current @ SDB current !				\ compile into `sdb' wordlist
	: latestxt dup R> !				\ fix-up  `xt'
	  ]] literal >name name>string person 2! fld: birthday 2!  fld: relationship 2!  fld: hobby 2! ; [[
	current !
	insert-ordered ;

: HELP	cr
	cr ." KEYWORDS: " USER: wordlist-words cr ." ---"
	cr ." Prefix `.' means print"
	cr ." To add a record, type: +: name yyyy.mm.dd;relationship;hobby;"
	cr cr ;
current !
SEAL godb
```



## Go


```go
package main

import (
    "encoding/json"
    "fmt"
    "io"
    "os"
    "sort"
    "strings"
    "time"
    "unicode"
)

// Database record format.  Time stamp and name are required.
// Tags and notes are optional.
type Item struct {
    Stamp time.Time
    Name  string
    Tags  []string `json:",omitempty"`
    Notes string   `json:",omitempty"`
}

// Item implements stringer interface
func (i *Item) String() string {
    s := i.Stamp.Format(time.ANSIC) + "\n  Name:  " + i.Name
    if len(i.Tags) > 0 {
        s = fmt.Sprintf("%s\n  Tags:  %v", s, i.Tags)
    }
    if i.Notes > "" {
        s += "\n  Notes: " + i.Notes
    }
    return s
}

// collection of Items
type db []*Item

// db implements sort.Interface
func (d db) Len() int           { return len(d) }
func (d db) Swap(i, j int)      { d[i], d[j] = d[j], d[i] }
func (d db) Less(i, j int) bool { return d[i].Stamp.Before(d[j].Stamp) }

// hard coded database file name
const fn = "sdb.json"

func main() {
    if len(os.Args) == 1 {
        latest()
        return
    }
    switch os.Args[1] {
    case "add":
        add()
    case "latest":
        latest()
    case "tags":
        tags()
    case "all":
        all()
    case "help":
        help()
    default:
        usage("unrecognized command")
    }
}

func usage(err string) {
    if err > "" {
        fmt.Println(err)
    }
    fmt.Println(`usage:  sdb [command] [data]
    where command is one of add, latest, tags, all, or help.`)
}

func help() {
    usage("")
    fmt.Println(`
Commands must be in lower case.
If no command is specified, the default command is latest.

Latest prints the latest item.
All prints all items in chronological order.
Tags prints the lastest item for each tag.
Help prints this message.

Add adds data as a new record.  The format is,

  name [tags] [notes]

Name is the name of the item and is required for the add command.

Tags are optional.  A tag is a single word.
A single tag can be specified without enclosing brackets.
Multiple tags can be specified by enclosing them in square brackets.

Text remaining after tags is taken as notes.  Notes do not have to be
enclosed in quotes or brackets.  The brackets above are only showing
that notes are optional.

Quotes may be useful however--as recognized by your operating system shell
or command line--to allow entry of arbitrary text.  In particular, quotes
or escape characters may be needed to prevent the shell from trying to
interpret brackets or other special characters.

Examples:
sdb add Bookends                        // no tags, no notes
sdb add Bookends rock my favorite       // tag: rock, notes: my favorite
sdb add Bookends [rock folk]            // two tags
sdb add Bookends [] "Simon & Garfunkel" // notes, no tags
sdb add "Simon&Garfunkel [artist]"      // name: Simon&Garfunkel, tag: artist

As shown in the last example, if you use features of your shell to pass
all data as a single string, the item name and tags will still be identified
by separating whitespace.

The database is stored in JSON format in the file "sdb.json"
`)
}

// load data for read only purposes.
func load() (db, bool) {
    d, f, ok := open()
    if ok {
        f.Close()
        if len(d) == 0 {
            fmt.Println("no items")
            ok = false
        }
    }
    return d, ok
}

// open database, leave open
func open() (d db, f *os.File, ok bool) {
    var err error
    f, err = os.OpenFile(fn, os.O_RDWR|os.O_CREATE, 0666)
    if err != nil {
        fmt.Println("cant open??")
        fmt.Println(err)
        return
    }
    jd := json.NewDecoder(f)
    err = jd.Decode(&d)
    // EOF just means file was empty.  That's okay with us.
    if err != nil && err != io.EOF {
        fmt.Println(err)
        f.Close()
        return
    }
    ok = true
    return
}

// handle latest command
func latest() {
    d, ok := load()
    if !ok {
        return
    }
    sort.Sort(d)
    fmt.Println(d[len(d)-1])
}

// handle all command
func all() {
    d, ok := load()
    if !ok {
        return
    }
    sort.Sort(d)
    for _, i := range d {
        fmt.Println("-----------------------------------")
        fmt.Println(i)
    }
    fmt.Println("-----------------------------------")
}

// handle tags command
func tags() {
    d, ok := load()
    if !ok {
        return
    }
    // we have to traverse the entire list to collect tags so there
    // is no point in sorting at this point.
    // collect set of unique tags associated with latest item for each
    latest := make(map[string]*Item)
    for _, item := range d {
        for _, tag := range item.Tags {
            li, ok := latest[tag]
            if !ok || item.Stamp.After(li.Stamp) {
                latest[tag] = item
            }
        }
    }
    // invert to set of unique items, associated with subset of tags
    // for which the item is the latest.
    type itemTags struct {
        item *Item
        tags []string
    }
    inv := make(map[*Item][]string)
    for tag, item := range latest {
        inv[item] = append(inv[item], tag)
    }
    // now we sort just the items we will output
    li := make(db, len(inv))
    i := 0
    for item := range inv {
        li[i] = item
        i++
    }
    sort.Sort(li)
    // finally ready to print
    for _, item := range li {
        tags := inv[item]
        fmt.Println("-----------------------------------")
        fmt.Println("Latest item with tags", tags)
        fmt.Println(item)
    }
    fmt.Println("-----------------------------------")
}

// handle add command
func add() {
    if len(os.Args) < 3 {
        usage("add command requires data")
        return
    } else if len(os.Args) == 3 {
        add1()
    } else {
        add4()
    }
}

// add command with one data string.  look for ws as separators.
func add1() {
    data := strings.TrimLeftFunc(os.Args[2], unicode.IsSpace)
    if data == "" {
        // data must have at least some non-whitespace
        usage("invalid name")
        return
    }
    sep := strings.IndexFunc(data, unicode.IsSpace)
    if sep < 0 {
        // data consists only of a name
        addItem(data, nil, "")
        return
    }
    name := data[:sep]
    data = strings.TrimLeftFunc(data[sep:], unicode.IsSpace)
    if data == "" {
        // nevermind trailing ws, it's still only a name
        addItem(name, nil, "")
        return
    }
    if data[0] == '[' {
        sep = strings.Index(data, "]")
        if sep < 0 {
            // close bracketed list for the user.  no notes.
            addItem(name, strings.Fields(data[1:]), "")
        } else {
            // brackets make things easy
            addItem(name, strings.Fields(data[1:sep]),
                strings.TrimLeftFunc(data[sep+1:], unicode.IsSpace))
        }
        return
    }
    sep = strings.IndexFunc(data, unicode.IsSpace)
    if sep < 0 {
        // remaining word is a tag
        addItem(name, []string{data}, "")
    } else {
        // there's a tag and some data
        addItem(name, []string{data[:sep]},
            strings.TrimLeftFunc(data[sep+1:], unicode.IsSpace))
    }
}

// add command with multiple strings remaining on command line
func add4() {
    name := os.Args[2]
    tag1 := os.Args[3]
    if tag1[0] != '[' {
        // no brackets makes things easy
        addItem(name, []string{tag1}, strings.Join(os.Args[4:], " "))
        return
    }
    if tag1[len(tag1)-1] == ']' {
        // tags all in one os.Arg is easy too
        addItem(name, strings.Fields(tag1[1:len(tag1)-1]),
            strings.Join(os.Args[4:], " "))
        return
    }
    // start a list for tags
    var tags []string
    if tag1 > "[" {
        tags = []string{tag1[1:]}
    }
    for x, tag := range os.Args[4:] {
        if tag[len(tag)-1] != ']' {
            tags = append(tags, tag)
        } else {
            // found end of tag list
            if tag > "]" {
                tags = append(tags, tag[:len(tag)-1])
            }
            addItem(name, tags, strings.Join(os.Args[5+x:], " "))
            return
        }
    }
    // close bracketed list for the user.  no notes.
    addItem(name, tags, "")
}

// complete the add command
func addItem(name string, tags []string, notes string) {
    db, f, ok := open()
    if !ok {
        return
    }
    defer f.Close()
    // add the item and format JSON
    db = append(db, &Item{time.Now(), name, tags, notes})
    sort.Sort(db)
    js, err := json.MarshalIndent(db, "", "  ")
    if err != nil {
        fmt.Println(err)
        return
    }
    // time to overwrite the file
    if _, err = f.Seek(0, 0); err != nil {
        fmt.Println(err)
        return
    }
    f.Truncate(0)
    if _, err = f.Write(js); err != nil {
        fmt.Println(err)
    }
}
```


## Haskell


The database is a list of type Item stored in a StateT monad transformer that allows for IO actions to be perfomed with it (StateT [Item] IO a). All IO actions occurring inside the StateT monad (handling command line arguments and printing items) must be lifted.
The database is written to disk using "show" and read back using "read". Therefore, Item is declared as an instance of both Show and Read classes. An item in the database file looks like the following:
Item {description = "La traviata", category = ["Classical"], date = Date 2012 10 19, optional = ["Giuseppe Verdi","1853"]}


```Haskell
import Control.Monad.State
import Data.List (sortBy, nub)
import System.Environment (getArgs, getProgName)
import System.Directory (doesFileExist)
import System.IO (openFile, hGetContents, hClose, IOMode(..),
    Handle, hPutStrLn)

-- for storing dates
data Date = Date Integer Int Int deriving (Show, Read, Eq, Ord)

-- for storing database items
data Item = Item    {description :: String
                    ,category    :: [String]
                    ,date        :: Date
                    ,optional    :: [String]}
                    deriving (Show, Read)

-- a state monad transformer which wraps IO actions.
-- the database (state) is passed implicitly between functions.
type ItemList a = StateT [Item] IO a

-- add an item to the database
addItem :: Item -> ItemList ()
addItem i = modify (++ [i])

-- get the newest of a list of items
latest :: [Item] -> [Item]
latest [] = []
latest [x]= [x]
latest xs = take 1 $ sortBy newer xs

-- compare two items to see which one is newer
newer :: Item -> Item -> Ordering
newer a b = compare (date b) (date a)

-- list all different categories (no duplicates)
categories :: ItemList [String]
categories = liftM (nub . concatMap category) get

-- list only the items with the given category tag
filterByCategory :: String -> ItemList [Item]
filterByCategory c = liftM (filter (\i -> c `elem` category i)) get

-- get the newest of all items
lastOfAll :: ItemList [Item]
lastOfAll = liftM latest get

-- get the newest item in each category
latestByCategory :: ItemList [Item]
latestByCategory = do
    cats <- categories
    filt <- mapM filterByCategory cats
    return $ concatMap latest filt

-- sort all items chronologically, newest first
sortByDate :: ItemList [Item]
sortByDate = liftM (sortBy newer) get

toScreen :: Item -> IO ()
toScreen (Item desc cats (Date y m d) opt) = putStrLn $
    "Description:\t" ++ desc ++ "\nCategories:\t" ++ show cats ++
    "\nDate:\t\t" ++ show y ++ "-" ++ show m ++ "-" ++ show d ++
    "\nOther info:\t" ++ show opt

-- command line argument handling
-- if the user called the program with the option "add", the
-- new item is returned to main so that it can be saved to disk.
-- the argument "opt" is a list.
arguments :: ItemList [Item]
arguments = do
    args <- liftIO getArgs
    case args of
        ("add":desc:cat:year:month:day:opt) -> do
            let newItem = parseItem args
            addItem newItem
            return [newItem]
        ("latest":[]) -> do
            item <- lastOfAll
            lift $ mapM_ toScreen item
            return []
        ("category":[]) -> do
            items <- latestByCategory
            lift $ mapM_ toScreen items
            return []
        ("all":[]) -> do
            sorted <- sortByDate
            lift $ mapM_ toScreen sorted
            return []
        _ -> do
            lift usage
            return []

parseItem :: [String] -> Item
parseItem (_:desc:cat:year:month:day:opt) =
    Item {description = desc, category = words cat,
        date = Date (read year) (read month) (read day),
        optional = opt}

usage :: IO ()
usage = do
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " add|all|category|latest \
        \OPTIONS\n\nadd \"description\" \"category1 category2\"... \
        \year month day [\"note1\" \"note2\"...]\n\tAdds a new record \
        \to the database.\n\nall\n\tPrints all items in chronological \
        \order.\n\ncategory\n\tPrints the latest item for each category.\
        \\n\nlatest\n\tPrints the latest item."

-- the program creates, reads and writes to a file in the current directory
main :: IO ()
main = do
    progName <- getProgName
    let fileName = progName ++ ".db"
    e <- doesFileExist fileName
    if e
        then do
            hr <- openFile fileName ReadMode
            f <- hGetContents hr
            v <- evalStateT arguments (map read $ lines f)
            hClose hr -- must be called after working with contents!
            hw <- openFile fileName AppendMode
            mapM_ (hPutStrLn hw . show) v
            hClose hw
        else do
            v <- evalStateT arguments []
            hw <- openFile fileName WriteMode
            mapM_ (hPutStrLn hw . show) v
            hClose hw

```



## J


J comes with a sql database, jdb.  Jdb's columns are memory mapped files with header information.  These won't meet the human readable data file requirement.  Hence, this program:


```j
HELP=: 0 :0
Commands:

DBNAME add DATA
DBNAME display the latest entry
DBNAME display the latest entry where CATEGORY contains WORD
DBNAME display all entries
DBNAME display all entries order by CATEGORY

1) The first add with new DBNAME assign category names.
2) lower case arguments verbatim.
3) UPPER CASE: substitute your values.

Examples, having saved this program as a file named s :
$ jconsole s simple.db display all entries
$ jconsole s simple.db add "first field" "2nd field"
)

Q=: ''''                                NB. quote character
time=: 6!:0@:('YYYY-MM-DD:hh:mm:ss.sss'"_)

embed=: >@:{.@:[ , ] , >@:{:@:[
assert '(x+y)' -: '()' embed 'x+y'

Duplicate=: 1 :'#~ m&= + 1 #~ #'
assert 0 1 2 3 3 4 -: 3 Duplicate i.5

prepare=: LF ,~ [: }:@:; (Q;Q,';')&embed@:(Q Duplicate)&.>@:(;~ time)
assert (-: }.@:".@:}:@:prepare) 'boxed';'';'li''st';'of';'''strings'''

categorize=: dyad define
i=. x i. y
if. (1 (~: #) i) +. i (= #) x do.
 smoutput 'choose 1 of'
 smoutput x
 exit 1
end.
{. i                                  NB. "index of" frame has rank 1.
)
assert 0 -: 'abc' categorize 'a'

loadsdb=: (<'time') (<0 0)} ".;._2@:(1!:1)

Dispatch=: conjunction define
help
:
commands=. y
command=. {. commands
x (u`help@.(command(i.~ ;:)n)) }.commands
)

NB. the long fork in show groups (": #~ (1 1 embed (1j1 }:@:# (1 #~ #))))
show=: smoutput@:(": #~ 1 1 embed 1j1 }:@:# 1 #~ #)

in=: +./@:E.
assert 'the'    in'quick brown fox jumps over the lazy dog'
assert 'the'-.@:in'QUICK BROWN FOX JUMPS OVER THE LAZY DOG'

where=: dyad define
'category contains word'=. 3 {. y
if. 'contains' -.@:-: contains do.
 help''
 exit 1
end.
i=. x ({.@:[ categorize <@:]) category
j=. {: I. ; word&in&.> i {"1 x
if. 0 (= #) j do.
 smoutput 'no matches'
else.
 x (show@:{~ 0&,) j
end.
)

entry=: 4 : 0
if. a: = y do.
 show@:({. ,: {:) x
else.
 x ''`where Dispatch'where' y
end.
)

latest=: ''`entry Dispatch'entry'
the=: ''`latest Dispatch'latest'

by=: 4 : 0
i=. x (categorize~ {.)~ y
show ({. , (/: i&{"1)@:}.) x
)

order=: ''`by Dispatch'by'

entries=: 4 : 0
if. a: = y do.
 show x
else.
 x ''`order Dispatch'order' y
end.
)

all=: ''`entries Dispatch'entries'

help=: smoutput@:(HELP"_)
add=: 1!:3~ prepare                     NB. minimal---no error tests
display=: (the`all Dispatch'the all'~ loadsdb)~  NB. load the simple db for some sort of display

({. add`display Dispatch'add display' }.)@:(2&}.)ARGV

exit 0

```

Assume the j code is stored in file s .  These bash commands, stored in file input , create a database using add .

```sh
D='jconsole s dataflow'

$D add name expression algebraic  rank     valence example explanation
$D add insert 'f/ y' 'insert f within y' infinite dyad 'sum=: +/' 'continued_fraction=:+`%/'
$D add fork '(f g h)y' 'g(f(y),h(y))' infinite monad 'average=: +/ % #' 'sum divided by tally'
$D add hook '(f g)y' 'f(y,g(y))' infinite monad '(/: 2&{"1)table' 'sort by third column'
$D add hook 'x(f g)y' 'f(x,g(y))' infinite dyad 'display verb in s' 'a reflexive dyadic hook'
$D add fork 'x(f g h)y' 'g(f(x,y),h(x,y))' infinite dyad '2j1(+ * -)9 12' 'product of sum and difference'
$D add reflexive 'f~ y' 'f(y,y)' infinite monad '^~y' 'y raised to the power of y'
$D add passive 'x f~ y' 'f(y,x)' 'ranks of f' dyad '(%~ i.@:>:) 8x' '8 intervals from 0 to 1'
$D add atop 'f@g y' 'f(g(y))' 'rank of g' monad '*:@(+/)' 'square the sum'
$D add atop 'x f@g y' 'f(g(x,y))' 'rank of g' dyad '>@{.' '(lisp) open the car'
$D add 'many more!'

```

Now we look up data from the bash command line.

```sh

$ . input  # source the input
$ echo $D
jconsole s dataflow
$ $D display the latest entry
┌───────────────────────┬──────────┬──────────┬─────────┬────┬───────┬───────┬───────────┐
│time                   │name      │expression│algebraic│rank│valence│example│explanation│
│2012-02-07:20:36:54.749│many more!│          │         │    │       │       │           │
└───────────────────────┴──────────┴──────────┴─────────┴────┴───────┴───────┴───────────┘
$ $D display the latest entry where 'part of speech' contains verb
choose 1 of
┌────┬────┬──────────┬─────────┬────┬───────┬───────┬───────────┐
│time│name│expression│algebraic│rank│valence│example│explanation│
└────┴────┴──────────┴─────────┴────┴───────┴───────┴───────────┘
$ $D display the latest entry where example contains average
┌───────────────────────┬────┬──────────┬────────────┬────────┬───────┬────────────────┬────────────────────┐
│time                   │name│expression│algebraic   │rank    │valence│example         │explanation         │
│2012-02-07:20:36:54.564│fork│(f g h)y  │g(f(y),h(y))│infinite│monad  │average=: +/ % #│sum divided by tally│
└───────────────────────┴────┴──────────┴────────────┴────────┴───────┴────────────────┴────────────────────┘
$ $D display all entries ordre by valence # oops!  transposition typo.
Commands:

DBNAME add DATA
DBNAME display the latest entry
DBNAME display the latest entry where CATEGORY contains WORD
DBNAME display all entries
DBNAME display all entries order by CATEGORY

1) The first add with new DBNAME assign category names.
2) lower case arguments verbatim.
3) UPPER CASE: substitute your values.

Examples, having saved this program as a file named s :
$ jconsole s simple.db display all entries
$ jconsole s simple.db add "first field" "2nd field"

$ $D display all entries order by valence
┌───────────────────────┬──────────┬──────────┬─────────────────┬──────────┬───────┬─────────────────┬─────────────────────────────┐
│time                   │name      │expression│algebraic        │rank      │valence│example          │explanation                  │
│2012-02-08:23:45:06.539│many more!│          │                 │          │       │                 │                             │
│2012-02-08:23:45:06.329│insert    │f/ y      │insert f within y│infinite  │dyad   │sum=: +/         │continued_fraction=:+`%/     │
│2012-02-08:23:45:06.400│hook      │x(f g)y   │f(x,g(y))        │infinite  │dyad   │display verb in s│a reflexive dyadic hook      │
│2012-02-08:23:45:06.426│fork      │x(f g h)y │g(f(x,y),h(x,y)) │infinite  │dyad   │2j1(+ * -)9 12   │product of sum and difference│
│2012-02-08:23:45:06.471│passive   │x f~ y    │f(y,x)           │ranks of f│dyad   │(%~ i.@:>:) 8x   │8 intervals from 0 to 1      │
│2012-02-08:23:45:06.515│atop      │x f@g y   │f(g(x,y))        │rank of g │dyad   │>@{.             │(lisp) open the car          │
│2012-02-08:23:45:06.353│fork      │(f g h)y  │g(f(y),h(y))     │infinite  │monad  │average=: +/ % # │sum divided by tally         │
│2012-02-08:23:45:06.376│hook      │(f g)y    │f(y,g(y))        │infinite  │monad  │(/: 2&{"1)table  │sort by third column         │
│2012-02-08:23:45:06.448│reflexive │f~ y      │f(y,y)           │infinite  │monad  │^~y              │y raised to the power of y   │
│2012-02-08:23:45:06.493│atop      │f@g y     │f(g(y))          │rank of g │monad  │*:@(+/)          │square the sum               │
└───────────────────────┴──────────┴──────────┴─────────────────┴──────────┴───────┴─────────────────┴─────────────────────────────┘
$ $D display all entries
┌───────────────────────┬──────────┬──────────┬─────────────────┬──────────┬───────┬─────────────────┬─────────────────────────────┐
│time                   │name      │expression│algebraic        │rank      │valence│example          │explanation                  │
│2012-02-08:23:45:06.329│insert    │f/ y      │insert f within y│infinite  │dyad   │sum=: +/         │continued_fraction=:+`%/     │
│2012-02-08:23:45:06.353│fork      │(f g h)y  │g(f(y),h(y))     │infinite  │monad  │average=: +/ % # │sum divided by tally         │
│2012-02-08:23:45:06.376│hook      │(f g)y    │f(y,g(y))        │infinite  │monad  │(/: 2&{"1)table  │sort by third column         │
│2012-02-08:23:45:06.400│hook      │x(f g)y   │f(x,g(y))        │infinite  │dyad   │display verb in s│a reflexive dyadic hook      │
│2012-02-08:23:45:06.426│fork      │x(f g h)y │g(f(x,y),h(x,y)) │infinite  │dyad   │2j1(+ * -)9 12   │product of sum and difference│
│2012-02-08:23:45:06.448│reflexive │f~ y      │f(y,y)           │infinite  │monad  │^~y              │y raised to the power of y   │
│2012-02-08:23:45:06.471│passive   │x f~ y    │f(y,x)           │ranks of f│dyad   │(%~ i.@:>:) 8x   │8 intervals from 0 to 1      │
│2012-02-08:23:45:06.493│atop      │f@g y     │f(g(y))          │rank of g │monad  │*:@(+/)          │square the sum               │
│2012-02-08:23:45:06.515│atop      │x f@g y   │f(g(x,y))        │rank of g │dyad   │>@{.             │(lisp) open the car          │
│2012-02-08:23:45:06.539│many more!│          │                 │          │       │                 │                             │
└───────────────────────┴──────────┴──────────┴─────────────────┴──────────┴───────┴─────────────────┴─────────────────────────────┘
$ cat dataflow
'2012-02-08:23:45:06.304';'name';'expression';'algebraic';'rank';'valence';'example';'explanation'
'2012-02-08:23:45:06.329';'insert';'f/ y';'insert f within y';'infinite';'dyad';'sum=: +/';'continued_fraction=:+`%/'
'2012-02-08:23:45:06.353';'fork';'(f g h)y';'g(f(y),h(y))';'infinite';'monad';'average=: +/ % #';'sum divided by tally'
'2012-02-08:23:45:06.376';'hook';'(f g)y';'f(y,g(y))';'infinite';'monad';'(/: 2&{"1)table';'sort by third column'
'2012-02-08:23:45:06.400';'hook';'x(f g)y';'f(x,g(y))';'infinite';'dyad';'display verb in s';'a reflexive dyadic hook'
'2012-02-08:23:45:06.426';'fork';'x(f g h)y';'g(f(x,y),h(x,y))';'infinite';'dyad';'2j1(+ * -)9 12';'product of sum and difference'
'2012-02-08:23:45:06.448';'reflexive';'f~ y';'f(y,y)';'infinite';'monad';'^~y';'y raised to the power of y'
'2012-02-08:23:45:06.471';'passive';'x f~ y';'f(y,x)';'ranks of f';'dyad';'(%~ i.@:>:) 8x';'8 intervals from 0 to 1'
'2012-02-08:23:45:06.493';'atop';'f@g y';'f(g(y))';'rank of g';'monad';'*:@(+/)';'square the sum'
'2012-02-08:23:45:06.515';'atop';'x f@g y';'f(g(x,y))';'rank of g';'dyad';'>@{.';'(lisp) open the car'
'2012-02-08:23:45:06.539';'many more!'
$
```



## Java

{{trans|D}}
{{works with|Java|7}}

```java
import java.io.*;
import java.text.*;
import java.util.*;

public class SimpleDatabase {

    final static String filename = "simdb.csv";

    public static void main(String[] args) {
        if (args.length < 1 || args.length > 3) {
            printUsage();
            return;
        }

        switch (args[0].toLowerCase()) {
            case "add":
                addItem(args);
                break;
            case "latest":
                printLatest(args);
                break;
            case "all":
                printAll();
                break;
            default:
                printUsage();
                break;
        }
    }

    private static class Item implements Comparable<Item>{
        final String name;
        final String date;
        final String category;

        Item(String n, String d, String c) {
            name = n;
            date = d;
            category = c;
        }

        @Override
        public int compareTo(Item item){
            return date.compareTo(item.date);
        }

        @Override
        public String toString() {
            return String.format("%s,%s,%s%n", name, date, category);
        }
    }

    private static void addItem(String[] input) {
        if (input.length < 2) {
            printUsage();
            return;
        }
        List<Item> db = load();
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        String date = sdf.format(new Date());
        String cat = (input.length == 3) ? input[2] : "none";
        db.add(new Item(input[1], date, cat));
        store(db);
    }

    private static void printLatest(String[] a) {
        List<Item> db = load();
        if (db.isEmpty()) {
            System.out.println("No entries in database.");
            return;
        }
        Collections.sort(db);
        if (a.length == 2) {
            for (Item item : db)
                if (item.category.equals(a[1]))
                    System.out.println(item);
        } else {
            System.out.println(db.get(0));
        }
    }

    private static void printAll() {
        List<Item> db = load();
        if (db.isEmpty()) {
            System.out.println("No entries in database.");
            return;
        }
        Collections.sort(db);
        for (Item item : db)
            System.out.println(item);
    }

    private static List<Item> load() {
        List<Item> db = new ArrayList<>();
        try (Scanner sc = new Scanner(new File(filename))) {
            while (sc.hasNext()) {
                String[] item = sc.nextLine().split(",");
                db.add(new Item(item[0], item[1], item[2]));
            }
        } catch (IOException e) {
            System.out.println(e);
        }
        return db;
    }

    private static void store(List<Item> db) {
        try (FileWriter fw = new FileWriter(filename)) {
            for (Item item : db)
                fw.write(item.toString());
        } catch (IOException e) {
            System.out.println(e);
        }
    }

    private static void printUsage() {
         System.out.println("Usage:");
         System.out.println("  simdb cmd [categoryName]");
         System.out.println("  add     add item, followed by optional category");
         System.out.println("  latest  print last added item(s), followed by "
                 + "optional category");
         System.out.println("  all     print all");
         System.out.println("  For instance: add \"some item name\" "
                 + "\"some category name\"");
    }
}
```


Output:


```txt

C:\temp>java -jar SimpleDatabase.jar add item1

C:\temp>java -jar SimpleDatabase.jar add item2

C:\temp>java -jar SimpleDatabase.jar add item3 cat3

C:\temp>java -jar SimpleDatabase.jar add item4 cat3

C:\temp>java -jar SimpleDatabase.jar add item5 cat3

C:\temp>java -jar SimpleDatabase.jar latest
item1,2014-06-03 19:30:05,none

C:\temp>java -jar SimpleDatabase.jar latest cat3
item3,2014-06-03 19:30:14,cat3

item4,2014-06-03 19:30:20,cat3

item5,2014-06-03 19:30:23,cat3

C:\temp>java -jar SimpleDatabase.jar all
item1,2014-06-03 19:30:05,none

item2,2014-06-03 19:30:08,none

item3,2014-06-03 19:30:14,cat3

item4,2014-06-03 19:30:20,cat3

item5,2014-06-03 19:30:23,cat3
```



## Julia

Command line CSV based simple database. The file used contained:
```txt

Name,Birthdate,State,Relation,Email
Sally Whittaker,1988-12-05,Illinois,friend,sally@mail.com
Belinda Jameson,1994-02-17,California,family,beljames@example.com
Jeff Bragg,2018-10-10,Texas,family,jb@texas.edu
Sandy Allen,2002-03-09,Colorado,friend,sandya@mail.com
Fred Kobo,1967-10-10,Colorado,friend,fkobo@example.net
```


```julia
using CSV, DataFrames, ArgParse, Dates

setting = ArgParseSettings()
@add_arg_table setting begin
    "--add"
        help = "add an entry, within double quotes, comma separated as \"name,birthdate,state,relation,email\" with birthdate as yyyy-mm-dd"
    "--latest"
        action = :store_true
        nargs = 0
        help = "print latest (last) entry"
    "--latestfriend"
        action = :store_true
        nargs = 0
        help = "print last friend listed"
    "--latestfamily"
        action = :store_true
        nargs = 0
        help = "print last family member listed"
    "--listbyage"
        action = :store_true
        nargs = 0
        help = "print all ages and entries in birth order"
end

const filename = "example.csv"
const df = CSV.File(filename, dateformat="yyyy-mm-dd") |> DataFrame
const commands = parse_args(setting)
if length(ARGS) == 0
    ArgParse.show_help(setting)
end
const changeflag = [false]

for (k, v) in commands
    if k == "add" && v != nothing
        newrow = Vector{Any}(split(v, r","))
        if length(newrow) == 5 && tryparse(DateTime, newrow[2]) != nothing
            newrow[2] = DateTime(newrow[2])
            push!(df, newrow)
            changeflag[1] = true
            println("Added entry $newrow.")
        end
    elseif k == "latest" && v
        println("The latest entry is $(df[end, :])")
    elseif k == "latestfriend" && v
        println(df[df.Relation .== "friend", :][end, :])
    elseif k == "latestfamily" && v
        println(df[df.Relation .== "family", :][end, :])
    elseif k == "listbyage" && v
        dobcol = df[:Birthdate]
        age = map(x -> round((now() - DateTime(x)).value /(1000*3600*24*365.25), digits=1), dobcol)
        df2 = deepcopy(df)
        df2 = insert!(df, 1, age, :Age)
        println(sort(df2, (:Age)))
    end
end

if changeflag[1]
    CSV.write(filename, df)
    println("Changes written to file $filename.")
end

```



## Kotlin

{{trans|Java}}
... though not quite the same.

```scala
// version 1.2.31

import java.text.SimpleDateFormat
import java.util.Date
import java.io.File
import java.io.IOException

val file = File("simdb.csv")

class Item(
    val name: String,
    val date: String,
    val category: String
) : Comparable<Item> {

    override fun compareTo(other: Item) = date.compareTo(other.date)

    override fun toString() = "$name, $date, $category"
}

fun addItem(input: Array<String>) {
    if (input.size < 2) {
        printUsage()
        return
    }
    val sdf = SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    val date = sdf.format(Date())
    val cat = if (input.size == 3) input[2] else "none"
    store(Item(input[1], date, cat))
}

fun printLatest(a: Array<String>) {
    val db = load()
    if (db.isEmpty()) {
        println("No entries in database.")
        return
    }
    // no need to sort db as items are added chronologically
    if (a.size == 2) {
        var found = false
        for (item in db.reversed()) {
            if (item.category == a[1]) {
                println(item)
                found = true
                break
            }
        }
        if (!found) println("There are no items for category '${a[1]}'")
    }
    else println(db[db.lastIndex])
}

fun printAll() {
    val db = load()
    if (db.isEmpty()) {
        println("No entries in database.")
        return
    }
    // no need to sort db as items are added chronologically
    for (item in db) println(item)
}

fun load(): MutableList<Item> {
    val db = mutableListOf<Item>()
    try {
        file.forEachLine { line ->
            val item = line.split(", ")
            db.add(Item(item[0], item[1], item[2]))
        }
    }
    catch (e: IOException) {
        println(e)
        System.exit(1)
    }
    return db
}

fun store(item: Item) {
    try {
        file.appendText("$item\n")
    }
    catch (e: IOException) {
        println(e)
    }
}

fun printUsage() {
    println("""
        |Usage:
        |  simdb cmd [categoryName]
        |  add     add item, followed by optional category
        |  latest  print last added item(s), followed by optional category
        |  all     print all
        |  For instance: add "some item name" "some category name"
    """.trimMargin())
}

fun main(args: Array<String>) {
    if (args.size !in 1..3) {
        printUsage()
        return
    }
    file.createNewFile()  // create file if it doesn't already exist
    when (args[0].toLowerCase()) {
        "add"    -> addItem(args)
        "latest" -> printLatest(args)
        "all"    -> printAll()
        else     -> printUsage()
    }
}
```


{{out}}
Sample session.

```txt

$ java -jar SimpleDatabase.jar add item1
$ java -jar SimpleDatabase.jar add item2
$ java -jar SimpleDatabase.jar add item3 cat3
$ java -jar SimpleDatabase.jar add item4 cat3
$ java -jar SimpleDatabase.jar add item5 cat3
$ java -jar SimpleDatabase.jar latest
item5, 2018-03-23 16:49:46, cat3
$ java -jar SimpleDatabase.jar latest none
item2, 2018-03-23 16:49:28, none
$ java -jar SimpleDatabase.jar latest cat4
There are no items for category 'cat4'
$ java -jar SimpleDatabase.jar all
item1, 2018-03-23 16:49:25, none
item2, 2018-03-23 16:49:28, none
item3, 2018-03-23 16:49:34, cat3
item4, 2018-03-23 16:49:40, cat3
item5, 2018-03-23 16:49:46, cat3

```



## Perl


```Perl
#!/usr/bin/perl
use warnings;
use strict;
use feature qw{ say };

use JSON::PP;
use Time::Piece;

use constant {
    NAME     => 0,
    CATEGORY => 1,
    DATE     => 2,
    DB       => 'simple-db',
};

my $operation = shift // "";

my %dispatch = (
    n => \&add_new,
    l => \&print_latest,
    L => \&print_latest_for_categories,
    a => \&print_all,
);

if ($dispatch{$operation}) {
    $dispatch{$operation}->(@ARGV);
} else {
    die "Invalid option. Use one of n, l, L, a.\n"
}

sub add_new {
    my ($name, $category, $date) = @_;
    my $db = eval { load() } || {};
    if (defined $date) {
        eval { 'Time::Piece'->strptime($date, '%Y-%m-%d'); 1 }
            or die "Invalid date format: YYYY-MM-DD.\n";

    } else {
        $date //= localtime->ymd;
    }

    my @ids = keys %{ $db->{by_id} };
    my $max_id = max(num => @ids) || 0;
    $db->{by_id}{ ++$max_id } = [ $name, $category, $date ];
    save($db);
}

sub print_latest {
    build_indexes( my $db = load(), 0, 1 );
    _print_latest($db);
}

sub _print_latest {
    my ($db, $category) = @_;
    my @dates = keys %{ $db->{by_date} };
    @dates = grep {
        grep $db->{by_id}{$_}[CATEGORY] eq $category,
            @{ $db->{by_date}{$_} };
    } @dates if defined $category;

    my $last_date = max(str => @dates);
    say for map $db->{by_id}{$_}[NAME],
            grep ! defined $category
                 || $db->{by_id}{$_}[CATEGORY] eq $category,
            @{ $db->{by_date}{$last_date} };
}

sub max {
    my $type = shift;
    my $max = $_[0];
    { num => sub { $_ >  $max },
      str => sub { $_ gt $max},
    }->{$type}->() and $max = $_
        for @_[ 1 .. $#_ ];
    return $max
}

sub print_latest_for_categories {
    build_indexes( my $db = load(), 1, 1 );

    for my $category (sort keys %{ $db->{by_category} }){
        say "* $category";
        _print_latest($db, $category);
    }
}

sub print_all {
    build_indexes( my $db = load(), 0, 1 );

    for my $date (sort keys %{ $db->{by_date} }) {
        for my $id (@{ $db->{by_date}{$date} }) {
            say $db->{by_id}{$id}[NAME];
        }
    }
}

sub load {
    open my $in, '<', DB or die "Can't open database: $!\n";
    local $/;
    return { by_id => decode_json(<$in>) };
}

sub save {
    my ($db) = @_;
    open my $out, '>', DB or die "Can't save database: $!\n";
    print {$out} encode_json($db->{by_id});
    close $out;
}

sub build_indexes {
    my ($db, $by_category, $by_date) = @_;
    for my $id (keys %{ $db->{by_id} }) {
        push @{ $db->{by_category}{ $db->{by_id}{$id}[CATEGORY] } }, $id
            if $by_category;
        push @{ $db->{by_date}{ $db->{by_id}{$id}[DATE] } }, $id
            if $by_date;
    }
}
```

Sample session
```txt
 ~ $ db.pl n 'Donald Trump' Republican 2017-01-20
 ~ $ db.pl n 'Barack Obama' Democratic 2009-01-20
 ~ $ db.pl n 'Bill Clinton' Democratic 1993-01-20
 ~ $ db.pl n 'George W. Bush' Republican 2001-01-20
 ~ $ db.pl a
Bill Clinton
George W. Bush
Barack Obama
Donald Trump
 ~ $ db.pl l
Donald Trump
 ~ $ db.pl L
* Democratic
Barack Obama
* Republican
Donald Trump
```


## Perl 6

A generic client/server JSON database.
server.p6:

```perl6
#!/usr/bin/env perl6
use JSON::Fast ;
sub MAIN( :$server='0.0.0.0', :$port=3333, :$dbfile='db' ) {
    my %db;
    my %index;
    my $dbdata = slurp "$dbfile.json" ;
    my $indexdata = slurp "{$dbfile}_index.json" ;
    %db = from-json($dbdata) if $dbdata ;
    %index = from-json($indexdata) if $indexdata ;
  react {
    whenever IO::Socket::Async.listen( $server , $port ) -> $conn {
        whenever $conn.Supply.lines -> $line {
            my %response = 'status' => '' ;
            my $msg = from-json $line ;
            say $msg.perl ;
            given $msg<function> {
                when 'set' {
                    %db{ $msg<topic> } = $msg<message> ;
                    %response<status> = 'ok' ;
                    %index<last_> = $msg<topic> ;
                    for %index<keys_>.keys -> $key {
                        if $msg<message>{$key} {
                            %index<lastkey_>{ $key }{ $msg<message>{$key} } = $msg<topic> ;
                            %index<idx_>{ $key }{ $msg<message>{$key} }{ $msg<topic> } = 1 ;
                        }
                    }
                    spurt "$dbfile.json", to-json(%db);
                    spurt "{$dbfile}_index.json", to-json(%index);
                }
                when 'get' {
                    %response<topic> = $msg<topic> ;
                    %response<message> = %db{ $msg<topic> } ;
                    %response<status> = 'ok' ;
                }
                when 'dump' {
                    %response{'data'} = %db ;
                    %response<status> = 'ok' ;
                }
                when 'dumpindex' {
                    %response{'data'} = %index ;
                    %response<status> = 'ok' ;
                }
                when 'delete' {
                    %db{ $msg<topic> }:delete;
                    %response<status> = 'ok' ;
                    spurt "$dbfile.json", to-json(%db);
                    reindex();
                }
                when 'addindex' {
                    %response<status> = 'ok' ;
                    %index<keys_>{ $msg<key>} =1 ;
                    reindex();
                }
                when 'reportlast' {
                    %response{'data'} = %db{%index<last_>} ;
                    %response<status> = 'ok' ;
                }
                when 'reportlastindex' {
                    %response<key> = $msg<key> ;
                    for %index<lastkey_>{$msg<key>}.keys -> $value {
                        #%response{'data'}.push: %db{ %index<lastkey_>{ $msg<key>  }{ $value } } ;
                        %response{'data'}{$value} = %db{ %index<lastkey_>{ $msg<key>  }{ $value } } ;
                    }
                    %response<status> = 'ok' ;
                }
                when 'reportindex' {
                    %response<status> = 'ok' ;
                    for %index<idx_>{$msg<key>}.keys.sort -> $value  {
                        for %index<idx_>{ $msg<key> }{ $value }.keys.sort -> $topic {
                            %response<data>.push:  %db{ $topic }  ;
                            #%response<data>{$value} = %db{ $topic }  ;
                        }
                    }
                }
                when 'commit' {
                    spurt "$dbfile.json", to-json(%db);
                    spurt "{$dbfile}_index.json", to-json(%index);
                    %response<status> = 'ok' ;
                }
                default {
                    %response<status> = 'error';
                    %response<error> = 'no function or not supported';
                }
            }
            $conn.print( to-json(%response, :!pretty) ~ "\n" ) ;
            LAST { $conn.close ; }
            QUIT { default { $conn.close ; say "oh no, $_";}}
            CATCH { default { say .^name, ': ', .Str ,  " handled in $?LINE";}}
        }
    }
  }
    sub reindex {
        %index<idx_>:delete;
        for %db.keys -> $topic {
            my $msg = %db{$topic} ;
            for %index<keys_>.keys -> $key {
                if $msg{$key} {
                    %index<idx_>{ $key }{ $msg{$key} }{ $topic } = 1 ;
                }
            }
        }
        spurt "{$dbfile}_index.json", to-json(%index) ;
    }
}
```

client.p6

```perl6
#!/usr/bin/env perl6
use JSON::Fast ;
multi MAIN('set', $topic,  $message='', :$server='localhost', :$port='3333', :$json='') {
    my %msg = function => 'set' , topic=> $topic , message=> $message ;
    %msg{"message"} = from-json( $json ) if $json ;
    sendmsg( %msg , $server, $port) ;
}
multi MAIN('add', $topic,  $json, :$server='localhost', :$port='3333' ) {
    my %msg = function => 'set' , topic=> $topic;
    %msg{"message"} = from-json( $json ) if $json ;
    sendmsg( %msg , $server, $port) ;
}
multi MAIN('get', $topic, :$server='localhost', :$port='3333') {
    my %msg = function => 'get' , topic=> $topic ;
    sendmsg( %msg , $server, $port) ;
}
multi MAIN('delete', $topic, :$server='localhost', :$port='3333') {
    my %msg = function => 'delete' , topic=> $topic ;
    sendmsg( %msg , $server, $port) ;
}
multi MAIN('dump', :$server='localhost', :$port='3333') {
    my %msg = function => 'dump'  ;
    sendmsg( %msg , $server, $port) ;
}
multi MAIN('addindex', $key, :$server='localhost', :$port='3333') {
    my %msg = function => 'addindex', key => $key  ;
    sendmsg( %msg , $server, $port) ;
}
multi MAIN('reportindex', $key, :$server='localhost', :$port='3333') {
    my %msg = function => 'reportindex', key => $key  ;
    sendmsg( %msg , $server, $port) ;
}
multi MAIN('reportlastindex', $key, :$server='localhost', :$port='3333') {
    my %msg = function => 'reportlastindex', key => $key  ;
    sendmsg( %msg , $server, $port) ;
}
multi MAIN('reportlast', :$server='localhost', :$port='3333') {
    my %msg = function => 'reportlast' ;
    sendmsg( %msg , $server, $port) ;
}
sub sendmsg( %msg , $server, $port){
    my $conn = await IO::Socket::Async.connect( $server , $port );
    $conn.print: to-json( %msg,:!pretty)~"\n";
    react {
        whenever $conn.Supply -> $data {
            print $data;
            $conn.close;
        }
    }
}
```

Example:

```txt
./client.p6 addindex constructor
./client.p6 addindex date

./client.p6 add 2007 '{"date":"2007-11-04","constructor":"Ducati","name":"Casey Stoner"}'
./client.p6 add 2008 '{"date":"2008-10-26","constructor":"Yamaha","name":"Valentino Rossi"}'
./client.p6 add 2009 '{"date":"2009-11-08","constructor":"Yamaha","name":"Valentino Rossi"}'
./client.p6 add 2010 '{"date":"2010-11-17","constructor":"Yamaha","name":"Jorge Lorenzo"}'
./client.p6 add 2011 '{"date":"2011-11-06","constructor":"Honda","name":"Casey Stoner"}'
./client.p6 add 2012 '{"date":"2012-11-11","constructor":"Yamaha","name":"Jorge Lorenzo"}'
./client.p6 add 2013 '{"date":"2013-11-10","constructor":"Honda","name":"Marc Márquez"}'
./client.p6 add 2014 '{"date":"2014-11-09","constructor":"Honda","name":"Marc Márquez"}'
./client.p6 add 2015 '{"date":"2015-11-08","constructor":"Yamaha","name":"Jorge Lorenzo"}'
./client.p6 add 2016 '{"date":"2016-11-13","constructor":"Honda","name":"Marc Márquez"}'
./client.p6 add 2017 '{"date":"2017-11-12","constructor":"Honda","name":"Marc Márquez"}'

./client.p6 reportlast
./client.p6 reportlastindex constructor
./client.p6 reportindex date
```



## Phix


```Phix
--
-- demo\rosetta\Simple_db.exw
--
### ====================

--
include timedate.e

constant filename = getenv(iff(platform()=WINDOWS?"APPDATA":"HOME"))&"/simple_db.csv"

procedure add(sequence cmd)
    if length(cmd)=0
    or length(cmd)>2 then
        printf(1,"usage: add name [cat]\n")
    else
        string name = cmd[1]
        string cat = iff(length(cmd)=2?cmd[2]:"none")
        string datestr = format_timedate(date(),"YYYY/MM/DD h:mmpm")
        integer fn = open(filename,"a")
        printf(fn,"%s,%s,%s\n",{name,cat,datestr})
        close(fn)
    end if
end procedure

procedure last(sequence cmd)
    integer fn = open(filename,"r")
    if fn=-1 then
        puts(1,"file not found\n")
        return
    end if
    integer lc = length(cmd)
    string last = iff(lc?"<no entries for that category>\n":"<empty>\n")
    while 1 do
        object line = gets(fn)
        if atom(line) then exit end if
        if lc=0 or split(line,',')[2]=cmd[1] then
            last = line
        end if
    end while
    puts(1,last)
    close(fn)
end procedure

sequence dates

function by_date(integer d1, integer d2)
    return compare(dates[d1],dates[d2])
end function
constant r_by_date = routine_id("by_date")

procedure sort_by_date()
-- (simple_db.csv should be edited manually to prove the date sort works)
    integer fn = open(filename,"r")
    if fn=-1 then
        puts(1,"file not found\n")
        return
    end if
    sequence lines = {}
    dates = {}
    while 1 do
        object line = gets(fn)
        if atom(line) then exit end if
        lines = append(lines,line)
        dates = append(dates,split(line,',')[3])
    end while
    close(fn)
    sequence tags = custom_sort(r_by_date,tagset(length(lines)))
    for i=1 to length(tags) do
        puts(1,lines[tags[i]])
    end for
end procedure

procedure process(sequence cmd)
    switch cmd[1] do
        case "add": add(cmd[2..$])
        case "last": last(cmd[2..$])
        case "sort": sort_by_date()
        default: printf(1,"unknown command: %s\n",{cmd[1]})
    end switch
end procedure

constant helptext = """
p demo\rosetta\Simple_db                    -- interactive mode, commands as below
p demo\rosetta\Simple_db add name [cat]     -- add entry
p demo\rosetta\Simple_db last [cat]         -- show last entry [in specified category]
p demo\rosetta\Simple_db sort               -- show full list sorted by date
"""
sequence cl = command_line()
    if length(cl)<3 then
        -- interactive mode
        puts(1,helptext)
        while 1 do
            puts(1,">")
            object line = trim(gets(0))
            if atom(line) or length(line)=0 then exit end if
            puts(1,"\n")
            process(split(line))
        end while
    else
        process(cl[3..$])
    end if
```

Sample session

```txt

C:\Program Files (x86)\Phix>p demo\rosetta\simple_db
p demo\rosetta\Simple_db                    -- interactive mode, commands as below
p demo\rosetta\Simple_db add name [cat]     -- add entry
p demo\rosetta\Simple_db last [cat]         -- show last entry [in specified category]
p demo\rosetta\Simple_db sort               -- show full list sorted by date
>sort
fred,none,2016/07/25 12:30pm
cliff,none,2016/07/25 12:31pm
barney,none,2016/07/25 12:32pm
one,two,2016/07/25 12:33pm
>add three four
>sort
fred,none,2016/07/25 12:30pm
cliff,none,2016/07/25 12:31pm
barney,none,2016/07/25 12:32pm
one,two,2016/07/25 12:33pm
three,four,2016/07/25 12:39pm
>last
three,four,2016/07/25 12:39pm
>last one
<no entries for that category>
>last two
one,two,2016/07/25 12:33pm
>

```



## PicoLisp

The '[http://software-lab.de/doc/refR.html#rc rc]' resource file handling function is used typically for such tasks. It also takes care of proper locking and protection.

```PicoLisp
#!/usr/bin/pil

(de usage ()
   (prinl
      "Usage:^J\
      sdb <file> add <title> <cat> <date> ...  Add a new entry^J\
      sdb <file> get <title>                   Retrieve an entry^J\
      sdb <file> latest                        Print the latest entry^J\
      sdb <file> categories                    Print the latest for each cat^J\
      sdb <file>                               Print all, sorted by date" ) )

(de printEntry (E)
   (apply println (cdddr E) (car E) (cadr E) (datStr (caddr E))) )

(ifn (setq *File (opt))
   (usage)
   (case (opt)
      (add
         (let (Ttl (opt)  Cat (opt))
            (if (strDat (opt))
               (rc *File Ttl (cons Cat @ (argv)))
               (prinl "Bad date") ) ) )
      (get
         (let Ttl (opt)
            (when (rc *File Ttl)
               (printEntry (cons Ttl @)) ) ) )
      (latest
         (printEntry (maxi caddr (in *File (read)))) )
      (categories
         (for Cat (by cadr group (in *File (read)))
            (printEntry (maxi caddr Cat)) ) )
      (NIL
         (mapc printEntry (by caddr sort (in *File (read)))) )
      (T (usage)) ) )

(bye)
```

Test:

```txt
$ sdb CDs add "Title 1" "Category 1" 2011-11-13
$ sdb CDs add "Title 2" "Category 2" 2011-11-12
$ sdb CDs add "Title 3" "Category 1" 2011-11-14 foo bar
$ sdb CDs add "Title 4" "Category 2" 2011-11-15 mumble

$ sdb CDs get "Title 3"
"Title 3" "Category 1" "2011-11-14" "foo" "bar"

$ sdb CDs latest
"Title 4" "Category 2" "2011-11-15" "mumble"

$ sdb CDs categories
"Title 4" "Category 2" "2011-11-15" "mumble"
"Title 3" "Category 1" "2011-11-14" "foo" "bar"

$ sdb CDs
"Title 2" "Category 2" "2011-11-12"
"Title 1" "Category 1" "2011-11-13"
"Title 3" "Category 1" "2011-11-14" "foo" "bar"
"Title 4" "Category 2" "2011-11-15" "mumble"
```



## Pike

{{trans|Common Lisp}} (simplified)

```Pike
mapping db = ([]);

mapping make_episode(string series, string title, string episode, array date)
{
    return ([ "series":series, "episode":episode, "title":title, "date":date ]);
}

void print_episode(mapping episode)
{
    write("  %-30s %10s %-30s (%{%d.%})\n",
          episode->series, episode->episode, episode->title, episode->date);
}

void print_series(mapping series)
{
    write("%-30s %-10s\n", series->series, series->status);
    map(series->episodes, print_episode);
}

void dump_db(mapping database)
{
    foreach(database; string name; mapping series)
    {
        print_series(series);
    }
}

array get_latest(mapping database)
{
    array latest = ({});
    foreach(database; string name; mapping series)
    {
       latest += ({ series->episodes[0] });
    }
    return latest;
}

int(0..1) compare_date(array a, array b)
{
    if (!arrayp(a) && !arrayp(b)) return false;
    if (!arrayp(a) || !sizeof(a)) return arrayp(b) && sizeof(b);
    if (!arrayp(b) || !sizeof(b)) return arrayp(a) && sizeof(a);
    if (a[0] == b[0]) return compare_date(a[1..], b[1..]);
    return a[0] < b[0];
}

int(0..1) compare_by_date(mapping a, mapping b)
{
    return compare_date(reverse(a->date), reverse(b->date));
}

void watch_list(mapping database)
{
    map(Array.sort_array(get_latest(database), compare_by_date),
        print_episode);
}

string prompt_read(string prompt)
{
    write("%s: ", prompt);
    return Stdio.stdin.gets();
}

array parse_date(string date)
{
    return (array(int))(date/".");
}

mapping prompt_for_episode()
{
    return make_episode(prompt_read("Series"),
                        prompt_read("Title"),
                        prompt_read("Episode"),
                        parse_date(prompt_read("Date watched")));
}

// pike offers encode_value() and decode_value() as standard ways
// to save and read data, but that is not a human readable format.
// therefore we are instead printing the structure as debug-output
// which is a readable form as long as it only contains integers,
// strings, mappings, arrays and multisets this format can be read by pike.
// to read it we are creating a class that contains the data as a value,
// which is then compiled and instantiated to allow us to pull the data out.
void save_db(string filename, mapping database)
{
    Stdio.write_file(filename, sprintf("%O", database));
}

void watch_save()
{
    save_db("pwatch", db);
}

mapping load_db(string filename)
{
    if (file_stat(filename))
        return compile_string("mixed data = " +
                              Stdio.read_file(filename) + ";")()->data;
    else return ([]);
}

mapping get_series(string name, mapping database)
{
    return database[name];
}

array get_episode_list(string series, mapping database)
{
    return database[series]->episodes;
}

void watch_new_series(string name, string status, mapping database)
{
    database[name] = (["series":name, "status":status, "episodes":({}) ]);
}

mapping get_or_add_series(string name, mapping database)
{
    if (!database[name])
    {
        string answer = prompt_read("Add new series? [y/n]: ");
        if (answer == "y")
            watch_new_series(name, "active", database);
    }
    return database[name];
}

void watch_add(mapping database)
{
    mapping episode = prompt_for_episode();
    string series_name = episode->series;
    mapping series = get_or_add_series(series_name, database);
    if (!series)
        watch_add(database);
    else
        series->episodes = Array.unshift(series->episodes, episode);
}

void watch_load()
{
    db = load_db("pwatch");
}

int main(int argc, array argv)
{
    watch_load();
    if (argc>1 && argv[1] == "add")
    {
        watch_add(db);
        watch_save();
    }
    else
        watch_list(db);
}
```



## PowerShell


```PowerShell

function db
{
    [CmdletBinding(DefaultParameterSetName="None")]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$false,
                   Position=0,
                   ParameterSetName="Add a new entry")]
        [string]
        $Path = ".\SimpleDatabase.csv",

        [Parameter(Mandatory=$true,
                   ParameterSetName="Add a new entry")]
        [string]
        $Name,

        [Parameter(Mandatory=$true,
                   ParameterSetName="Add a new entry")]
        [string]
        $Category,

        [Parameter(Mandatory=$true,
                   ParameterSetName="Add a new entry")]
        [datetime]
        $Birthday,

        [Parameter(ParameterSetName="Print the latest entry")]
        [switch]
        $Latest,

        [Parameter(ParameterSetName="Print the latest entry for each category")]
        [switch]
        $LatestByCategory,

        [Parameter(ParameterSetName="Print all entries sorted by a date")]
        [switch]
        $SortedByDate
    )

    if (-not (Test-Path -Path $Path))
    {
        '"Name","Category","Birthday"' | Out-File -FilePath $Path
    }

    $db = Import-Csv -Path $Path | Foreach-Object {
        $_.Birthday = $_.Birthday -as [datetime]
        $_
    }

    switch ($PSCmdlet.ParameterSetName)
    {
        "Add a new entry"
        {
            [PSCustomObject]@{Name=$Name; Category=$Category; Birthday=$Birthday} | Export-Csv -Path $Path -Append
        }
        "Print the latest entry"
        {
            $db[-1]
        }
        "Print the latest entry for each category"
        {
            ($db | Group-Object -Property Category).Name | ForEach-Object {($db | Where-Object -Property Category -Contains $_)[-1]}
        }
        "Print all entries sorted by a date"
        {
            $db | Sort-Object -Property Birthday
        }
        Default
        {
            $db
        }
    }
}

db -Name Bev   -Category friend   -Birthday 3/3/1983
db -Name Bob   -Category family   -Birthday 7/19/1987
db -Name Gill  -Category friend   -Birthday 12/9/1986
db -Name Gail  -Category family   -Birthday 2/11/1986
db -Name Vince -Category family   -Birthday 3/10/1960
db -Name Wayne -Category coworker -Birthday 5/29/1962

```

Here is the data from the CSV file as a PowerShell Object:

```PowerShell

db

```

{{Out}}

```txt

Name  Category Birthday
----  -------- --------
Bev   friend   3/3/1983 12:00:00 AM
Bob   family   7/19/1987 12:00:00 AM
Gill  friend   12/9/1986 12:00:00 AM
Gail  family   2/11/1986 12:00:00 AM
Vince family   3/10/1960 12:00:00 AM
Wayne coworker 5/29/1962 12:00:00 AM

```

The latest entry:

```PowerShell

db -Latest

```

{{Out}}

```txt

Name  Category Birthday
----  -------- --------
Wayne coworker 5/29/1962 12:00:00 AM

```

The latest entries by category:

```PowerShell

db -LatestByCategory

```

{{Out}}

```txt

Name  Category Birthday
----  -------- --------
Gill  friend   12/9/1986 12:00:00 AM
Vince family   3/10/1960 12:00:00 AM
Wayne coworker 5/29/1962 12:00:00 AM

```

The database sorted on the Birthday property:

```PowerShell

db -SortedByDate

```

{{Out}}

```txt

Name  Category Birthday
----  -------- --------
Vince family   3/10/1960 12:00:00 AM
Wayne coworker 5/29/1962 12:00:00 AM
Bev   friend   3/3/1983 12:00:00 AM
Gail  family   2/11/1986 12:00:00 AM
Gill  friend   12/9/1986 12:00:00 AM
Bob   family   7/19/1987 12:00:00 AM

```



## Python


```python
#!/usr/bin/python3

'''\
Simple database for: http://rosettacode.org/wiki/Simple_database

'''

import argparse
from argparse import Namespace
import datetime
import shlex


def parse_args():
    'Set up, parse, and return arguments'

    parser = argparse.ArgumentParser(epilog=globals()['__doc__'])

    parser.add_argument('command', choices='add pl plc pa'.split(),
                        help='''\
add: Add a new entry
pl:  Print the latest entry
plc: Print the latest entry for each category/tag
pa:  Print all entries sorted by a date''')
    parser.add_argument('-d', '--description',
                        help='A description of the item. (e.g., title, name)')
    parser.add_argument('-t', '--tag',
                        help=('''A category or tag (genre, topic, relationship '''
                              '''such as “friend” or “family”)'''))
    parser.add_argument('-f', '--field', nargs=2, action='append',
                        help='Other optional fields with value (can be repeated)')

    return parser

def do_add(args, dbname):
    'Add a new entry'
    if args.description is None:
        args.description = ''
    if args.tag is None:
        args.tag = ''
    del args.command
    print('Writing record to %s' % dbname)
    with open(dbname, 'a') as db:
        db.write('%r\n' % args)

def do_pl(args, dbname):
    'Print the latest entry'
    print('Getting last record from %s' % dbname)
    with open(dbname, 'r') as db:
        for line in db: pass
    record = eval(line)
    del record._date
    print(str(record))

def do_plc(args, dbname):
    'Print the latest entry for each category/tag'
    print('Getting latest record for each tag from %s' % dbname)
    with open(dbname, 'r') as db:
        records = [eval(line) for line in db]
    tags = set(record.tag for record in records)
    records.reverse()
    for record in records:
        if record.tag in tags:
            del record._date
            print(str(record))
            tags.discard(record.tag)
            if not tags: break

def do_pa(args, dbname):
    'Print all entries sorted by a date'
    print('Getting all records by date from %s' % dbname)
    with open(dbname, 'r') as db:
        records = [eval(line) for line in db]
    for record in records:
        del record._date
        print(str(record))

def test():
    import time
    parser = parse_args()
    for cmdline in [
                    """-d Book -f title 'Windy places' -f type hardback --tag DISCOUNT add""",
                    """-d Book -f title 'RC spammers'  -f type paperback -t   DISCOUNT add""",
                    """-d Book -f title 'Splat it' -f type hardback -f special 'first edition' -t PREMIUM add""",
                    """pl""",
                    """plc""",
                    ]:
        args = parser.parse_args(shlex.split(cmdline))
        now = datetime.datetime.utcnow()
        args._date = now.isoformat()
        do_command[args.command](args, dbname)
        time.sleep(0.5)



do_command = dict(add=do_add, pl=do_pl, plc=do_plc, pa=do_pa)
dbname = '_simple_db_db.py'


if __name__ == '__main__':
    if 0:
        test()
    else:
        parser = parse_args()
        args = parser.parse_args()
        now = datetime.datetime.utcnow()
        args._date = now.isoformat()
        do_command[args.command](args, dbname)
```


;Sample session (Unix):

```txt
paddy3118:~$ ./simple_db.py -h
usage: simple_db.py [-h] [-d DESCRIPTION] [-t TAG] [-f FIELD FIELD]
                    {add,pl,plc,pa}

positional arguments:
  {add,pl,plc,pa}       add: Add a new entry pl: Print the latest entry plc:
                        Print the latest entry for each category/tag pa: Print
                        all entries sorted by a date

optional arguments:
  -h, --help            show this help message and exit
  -d DESCRIPTION, --description DESCRIPTION
                        A description of the item. (e.g., title, name)
  -t TAG, --tag TAG     A category or tag (genre, topic, relationship such as
                        “friend” or “family”)
  -f FIELD FIELD, --field FIELD FIELD
                        Other optional fields with value (can be repeated)

Simple database for: http://rosettacode.org/wiki/Simple_database
paddy3118:~$ ./simple_db.py -d Book -f title 'Windy places' -f type hardback --tag DISCOUNT add
Writing record to _simple_db_db.py
paddy3118:~$ ./simple_db.py -d Book -f title 'RC spammers'  -f type paperback -t   DISCOUNT add
Writing record to _simple_db_db.py
paddy3118:~$ ./simple_db.py -d Book -f title 'Splat it' -f type hardback -f special 'first edition' -t PREMIUM add
Writing record to _simple_db_db.py
paddy3118:~$ ./simple_db.py pl
Getting last record from _simple_db_db.py
Namespace(description='Book', field=[['title', 'Splat it'], ['type', 'hardback'], ['special', 'first edition']], tag='PREMIUM')
paddy3118:~$ ./simple_db.py plc
Getting latest record for each tag from _simple_db_db.py
Namespace(description='Book', field=[['title', 'Splat it'], ['type', 'hardback'], ['special', 'first edition']], tag='PREMIUM')
Namespace(description='Book', field=[['title', 'RC spammers'], ['type', 'paperback']], tag='DISCOUNT')
paddy3118:~$ ./simple_db.py pa
Getting all records by date from _simple_db_db.py
Namespace(description='Book', field=[['title', 'Windy places'], ['type', 'hardback']], tag='DISCOUNT')
Namespace(description='Book', field=[['title', 'RC spammers'], ['type', 'paperback']], tag='DISCOUNT')
Namespace(description='Book', field=[['title', 'Splat it'], ['type', 'hardback'], ['special', 'first edition']], tag='PREMIUM')
paddy3118:~$
paddy3118:~$ cat _simple_db_db.py
Namespace(_date='2012-08-18T06:02:44.947091', description='Book', field=[['title', 'Windy places'], ['type', 'hardback']], tag='DISCOUNT')
Namespace(_date='2012-08-18T06:03:11.477429', description='Book', field=[['title', 'RC spammers'], ['type', 'paperback']], tag='DISCOUNT')
Namespace(_date='2012-08-18T06:03:34.319799', description='Book', field=[['title', 'Splat it'], ['type', 'hardback'], ['special', 'first edition']], tag='PREMIUM')
paddy3118:~$
```



## Racket


```racket

#!/usr/bin/env racket
#lang racket

(define (*write file data) ; write data in human readable format (sexpr/line)
  (with-output-to-file file #:exists 'replace
    (lambda () (for ([x data]) (printf "~s\n" x)))))
(define *read file->list) ; read our "human readable format"

(command-line
 #:once-any
 [("-a") file title category date "Add an entry"
  (*write file `(,@(*read file) (,title ,category ,date)))]
 [("-d") file title "Delete an entry (all matching)"
  (*write file (filter-not (lambda (x) (equal? (car x) title)) (*read file)))]
 [("-p") file mode "Print entries, mode = latest, latest/cat, all, by-date"
  (define data (*read file))
  (define (show item)
    (match item [(list title cat date) (printf "[~a] ~a; ~a\n" cat title date)]))
  (case (string->symbol mode)
    [(all) (for-each show data)]
    [(by-date) (for-each show (sort data string<? #:key cadr))]
    [(latest) (show (last data))]
    [(latest/cat)
     (define (last/cat c) (for/last ([x data] #:when (equal? c (cadr x))) x))
     (for-each (compose1 show last/cat) (remove-duplicates (map cadr data)))]
    [else (error 'sdb "bad printout mode")])])

```


Sample run:

```txt

$ ./sdb -h
sdb [ <option> ... ]
 where <option> is one of
/ -a <file> <title> <category> <date> : Add an entry
| -d <file> <title> : Delete an entry (all matching)
\ -p <file> <mode> : Print entries, mode = latest, latest/cat, all
  --help, -h : Show this help
  -- : Do not treat any remaining argument as a switch (at this level)
 /|\ Brackets indicate mutually exclusive options.
 Multiple single-letter switches can be combined after one `-'; for
  example: `-h-' is the same as `-h --'
$ ./sdb -a Stuffs A-Book books 2013-01-01
$ ./sdb -a Stuffs Another-Book books 2013-01-02
$ ./sdb -a Stuffs Some-CD cds 2013-01-03
$ ./sdb -a Stuffs A-Bad-CD cds 2013-01-04
$ ./sdb -p Stuffs all
[books] A-Book; 2013-01-01
[books] Another-Book; 2013-01-02
[cds] Some-CD; 2013-01-03
[cds] A-Bad-CD; 2013-01-04
$ ./sdb -p Stuffs latest
[cds] A-Bad-CD; 2013-01-04
$ ./sdb -p Stuffs latest/cat
[books] Another-Book; 2013-01-02
[cds] A-Bad-CD; 2013-01-04
$ ./sdb -d Stuffs A-Bad-CD
$ ./sdb -p Stuffs by-date
[books] A-Book; 2013-01-01
[books] Another-Book; 2013-01-02
[cds] Some-CD; 2013-01-03

```



## REBOL


```rebol
rebol [author: "Nick Antonaccio"]
write/append %rdb ""  db: load %rdb
switch system/options/args/1 [
    "new" [write/append %rdb rejoin [now " " mold/only next system/options/args newline]]
    "latest" [print copy/part tail sort/skip db 4 -4]
    "latestcat" [
        foreach cat unique extract at db 3 4 [
            t: copy []
            foreach [a b c d] db [if c = cat [append t reduce [a b c d]]]
            print copy/part tail sort/skip t 4 -4
        ]
    ]
    "sort" [probe sort/skip db 4]
]
halt
```


## REXX


```rexx
/* REXX ---------------------------------------------------------------
* 05.10.2014
*--------------------------------------------------------------------*/
x05='05'x
mydb='sidb.txt'
Say 'Enter your commands, ?, or end'
Do Forever
  Parse Pull l
  Parse Var l command text
  Select
    When command='?' Then
      Call help
    When command='add' Then Do
      Parse Var text item ',' category ',' date
      If date='' Then
        date=date('S') /*yyyymmdd*/
      Say 'adding item' item'/'category 'dated' date
      Call lineout mydb,date item x05 category
      End
    When command='latest' Then Do
      Call lineout mydb
      Parse Var text category
      hidt='00000000'
      ol=''
      Do While lines(mydb)>0
        l=linein(mydb)
        Parse Var l dt a (x05) b
        If category=''|,
           category='-' & b='' |,
           b=category Then Do
          If dt>>hidt Then Do
            ol=l
            hidt=dt
            End
          End
        End
        If ol>'' Then
          Call o ol
        Else
          Say 'no matching item found'
      End
    When command='all' Then Do
      Call lineout mydb
      Parse Var text category
      Do While lines(mydb)>0
        l=linein(mydb)
        Parse Var l a (x05) b
        If category=''|,
           category='-' & b=''|,
           b=category Then
          Call o l
        End
      End
    When command='end' Then
      Leave
    Otherwise Do
      Say 'invalid command ('command')'
      Call help
      End
    End
  End
Say 'Bye'
Exit

o: Parse Value arg(1) With dt text
   Say left(dt,8) text
   Return

help:
  Say 'add item[,[category][,date]] to add an item'
  Say 'latest category to list the latest item of a category'
  Say 'latest to list the latest item'
  Say 'all category to list all items of a category'
  Say 'all to list all items'
  Say 'end to end this program'
  Say 'Use category - to list items without category'
  Return
```

{{out}}

```txt
   Enter your commands, ?, or end
?
   add item[,[category][,date]] to add an item
   latest category to list the latest item of a category
   latest to list the latest item
   all category to list all items of a category
   all to list all items
   end to end this program
   Use category - to list items without category
add item1
   adding item item1/ dated 20141006
add item2
   adding item item2/ dated 20141006
add item3,cat3
   adding item item3/cat3 dated 20141006
add item4,cat3,20201910
   adding item item4/cat3 dated 20201910
add item5,cat3,190201910
   adding item item5/cat3 dated 190201910
all
   20141006 item1 �
   20141006 item2 �
   20141006 item3 � cat3
   20201910 item4 � cat3
   190201910 item5 � cat3
latest
   20201910 item4 � cat3
latest cat1
   no matching item found
latest cat3
   20201910 item4 � cat3
end
   Bye
```



## Ruby


```ruby
require 'date'
require 'json'
require 'securerandom'

class SimpleDatabase
  def initialize(dbname, *fields)
    @dbname = dbname
    @filename = @dbname + ".dat"
    @fields = fields
    @maxl = @fields.collect {|f| f.length}.max
    @data = {
      'fields' => fields,
      'items' => {},
      'history' => [],
      'tags' => {},
    }
  end
  attr_reader :dbname, :fields

  def self.open(dbname)
    db = new(dbname)
    db.read
    db
  end

  def read()
    if not File.exists?(@filename)
      raise ArgumentError, "Database #@dbname has not been created"
    end
    @data = JSON.parse(File.read(@filename))
    @fields = @data['fields']
    @maxl = @fields.collect {|f| f.length}.max
  end

  def write()
    File.open(@filename, 'w') {|f| f.write(JSON.generate(@data))}
  end

  def add(*values)
    id = SecureRandom.uuid
    @data['items'][id] = Hash[ @fields.zip(values) ]
    @data['history'] << [Time.now.to_f, id]
    id
  end

  def tag(id, *tags)
    tags.each do |tag|
      if @data['tags'][tag].nil?
        @data['tags'][tag] = [id]
      else
        @data['tags'][tag] << id
      end
    end
    id
  end

  def latest
    @data['history'].sort_by {|val| val[0]}.last.last
  end

  def get_item(id)
    @data['items'][id]
  end

  def tags()
    @data['tags'].keys.sort
  end

  def ids_for_tag(tag)
    @data['tags'][tag]
  end

  def tags_for_id(id)
    @data['tags'].keys.inject([]) do |tags, tag|
      tags << tag if @data['tags'][tag].include?(id)
      tags
    end
  end

  def display(id)
    item = get_item(id)
    fmt = "%#{@maxl}s - %s\n"
    puts fmt % ['id', id]
    @fields.each {|f| print fmt % [f, item[f]]}
    puts fmt % ['tags', tags_for_id(id).join(',')]
    added = @data['history'].find {|x| x[1] == id}.first
    puts fmt % ['date added', Time.at(added).ctime]
    puts ""
  end

  def each()
    @data['history'].each {|time, id| yield id}
  end

  def each_item_with_tag(tag)
    @data['tags'][tag].each {|id| yield id}
  end
end
def usage()
  puts <<END
usage: #{$0} command args ...

commands:
  help
  create dbname field ...
  fields dbname
  add dbname value ...
  tag dbname id tag ...
  tags dbname
  list dbname [tag ...]
  latest dbname
  latest_by_tag dbname
END
end

def open_database(args)
  dbname = args.shift
  begin
    SimpleDatabase.open(dbname)
  rescue ArgumentError => e
    STDERR.puts e.message
    exit 1
  end
end

def process_command_line(command, *args)
  case command
  when 'help'
    usage

  when 'create'
    db = SimpleDatabase.new(*args)
    db.write
    puts "Database #{args[0]} created"

  when 'fields'
    db = open_database(args)
    puts "Database #{db.dbname} fields:"
    puts db.fields.join(',')

  when 'add'
    db = open_database(args)
    id = db.add(*args)
    db.write
    puts "Database #{db.dbname} added id #{id}"

  when 'tag'
    db = open_database(args)
    id = args.shift
    db.tag(id, *args)
    db.write
    db.display(id)

  when 'tags'
    db = open_database(args)
    puts "Database #{db.dbname} tags:"
    puts db.tags.join(',')

  when 'list'
    db = open_database(args)
    if args.empty?
      db.each {|id| db.display(id)}
    else
      args.each do |tag|
        puts "Items tagged #{tag}"
        db.each_item_with_tag(tag) {|id| db.display(id)}
      end
    end

  when 'latest'
    db = open_database(args)
    db.display(db.latest)

  when 'latest_by_tag'
    db = open_database(args)
    db.tags.each do |tag|
      puts tag
      db.display(db.ids_for_tag(tag).last)
    end

  else
    puts "Error: unknown command '#{command}'"
    usage
  end
end

process_command_line *ARGV
```


Sample session
<pre "style='height: 40ex; overflow: scroll">$ ruby simple_database.rb create appointments event_title start_time stop_time location event_description
Database appointments created
$ ruby simple_database.rb add appointments "Wife's Birthday" "2011-11-01" "2011-11-01" "" "happy 39th"
Database appointments added id 6dd02195-1efe-40d1-b43e-c2efd852cd1d
$ ruby simple_database.rb tag appointments 6dd02195-1efe-40d1-b43e-c2efd852cd1d birthday family
               id - 6dd02195-1efe-40d1-b43e-c2efd852cd1d
      event_title - Wife's Birthday
       start_time - 2011-11-01
        stop_time - 2011-11-01
         location -
event_description - happy 39th
             tags - birthday,family
       date added - Thu Nov  3 15:52:31 2011

$ ruby simple_database.rb add appointments "Parent-Teacher Conference" "2011-11-03 19:30" "2011-11-03 20:00" "school" "desc"
Database appointments added id 0190b835-401d-42da-9ed3-1d335d27b83c
$ ruby simple_database.rb tag appointments 0190b835-401d-42da-9ed3-1d335d27b83c school family
               id - 0190b835-401d-42da-9ed3-1d335d27b83c
      event_title - Parent-Teacher Conference
       start_time - 2011-11-03 19:30
        stop_time - 2011-11-03 20:00
         location - school
event_description - desc
             tags - family,school
       date added - Thu Nov  3 15:54:05 2011

$ ruby simple_database.rb add appointments "Buy gift for wife" "2011-10-31 16:00" "2011-10-31 16:30" "the mall" "hmm, maybe jewelery?"
Database appointments added id 4023e6f1-bcc1-49e5-a59f-138157b413f4
$ ruby simple_database.rb tag appointments 4023e6f1-bcc1-49e5-a59f-138157b413f4 family last-minute
               id - 4023e6f1-bcc1-49e5-a59f-138157b413f4
      event_title - Buy gift for wife
       start_time - 2011-10-31 16:00
        stop_time - 2011-10-31 16:30
         location - the mall
event_description - hmm, maybe jewelery?
             tags - family,last-minute
       date added - Thu Nov  3 15:55:02 2011

$ ruby simple_database.rb fields appointments
Database appointments fields:
event_title,start_time,stop_time,location,event_description
$ ruby simple_database.rb tags appointments
Database appointments tags:
birthday,family,last-minute,school
$ ruby simple_database.rb list appointments
               id - 6dd02195-1efe-40d1-b43e-c2efd852cd1d
      event_title - Wife's Birthday
       start_time - 2011-11-01
        stop_time - 2011-11-01
         location -
event_description - happy 39th
             tags - birthday,family
       date added - Thu Nov  3 15:52:31 2011

               id - 0190b835-401d-42da-9ed3-1d335d27b83c
      event_title - Parent-Teacher Conference
       start_time - 2011-11-03 19:30
        stop_time - 2011-11-03 20:00
         location - school
event_description - desc
             tags - family,school
       date added - Thu Nov  3 15:54:05 2011

               id - 4023e6f1-bcc1-49e5-a59f-138157b413f4
      event_title - Buy gift for wife
       start_time - 2011-10-31 16:00
        stop_time - 2011-10-31 16:30
         location - the mall
event_description - hmm, maybe jewelery?
             tags - family,last-minute
       date added - Thu Nov  3 15:55:02 2011
$ cat appointments.dat
{"fields":["event_title","start_time","stop_time","location","event_description"],"items":{"6dd02195-1efe-40d1-b43e-c2efd852cd1d":{"event_title":"Wife's Birthday","start_time":"2011-11-01","stop_time":"2011-11-01","location":"","event_description":"happy 39th"},"0190b835-401d-42da-9ed3-1d335d27b83c":{"event_title":"Parent-Teacher Conference","start_time":"2011-11-03 19:30","stop_time":"2011-11-03 20:00","location":"school","event_description":"desc"},"4023e6f1-bcc1-49e5-a59f-138157b413f4":{"event_title":"Buy gift for wife","start_time":"2011-10-31 16:00","stop_time":"2011-10-31 16:30","location":"the mall","event_description":"hmm, maybe jewelery?"}},"history":[[1320349951.000625,"6dd02195-1efe-40d1-b43e-c2efd852cd1d"],[1320350045.4736252,"0190b835-401d-42da-9ed3-1d335d27b83c"],[1320350102.9486248,"4023e6f1-bcc1-49e5-a59f-138157b413f4"]],"tags":{"birthday":["6dd02195-1efe-40d1-b43e-c2efd852cd1d"],"family":["6dd02195-1efe-40d1-b43e-c2efd852cd1d","0190b835-401d-42da-9ed3-1d335d27b83c","4023e6f1-bcc1-49e5-a59f-138157b413f4"],"school":["0190b835-401d-42da-9ed3-1d335d27b83c"],"last-minute":["4023e6f1-bcc1-49e5-a59f-138157b413f4"]}}

```



## Run BASIC


```runbasic
sqliteconnect    #sql,  "f:\client.db"  ' Connect to the DB

' -------------------------------
' show user options
' -------------------------------
[sho]
cls ' clear screen
button #acd, "Add a new entry",                          [add]
button #acd, "Print the latest entry", 			 [last]
button #acd, "Print the latest entry for each category", [lastCat]
button #acd, "Print all entries sorted by a date",       [date]
button #ex, "Exit",                                      [exit]
wait

' ------------------------------------
' add a new entry (user input screen)
' ------------------------------------
[add]
cls ' clear the screen
html "<TABLE BORDER=1 CELLPADDING=0 CELLSPACING=0 bgcolor=wheat>"
html "<TR align=center BGCOLOR=tan><TD colspan=2>Client Maintenance</TD></TR><TR>"
html "<TD bgcolor=tan align=right>Client Num</TD><TD>"
       textbox #clientNum,clientNum$,5

html "</TD></TR><TR><TD bgcolor=tan align=right>Name</TD><TD>"
     textbox #name,name$,30

html "</TD></TR><TR><TD bgcolor=tan align=right>Client Date</TD><TD>"
     textbox #clientDate,clientDate$,19

html "</TD></TR><TR><TD bgcolor=tan align=right>Category</TD><TD>"
     textbox #category,category$,10

html "</TD></TR><TR><TR bgcolor=tan><TD colspan=2 ALIGN=CENTER>"
     button #acd, "Add", [addIt]
     button #ex, "Exit", [sho]
html "</TD></TR></TABLE>"
wait

' ---------------------------------------------
' Get data from the screen
' ---------------------------------------------
[addIt]
clientNum    =       #clientNum contents$()
name$        = trim$(#name contents$())
clientDate$  = trim$(#clientDate contents$())
category$    = trim$(#category contents$())
dbVals$      = clientNum;",'";name$;"','";clientDate$;"','";category$;"'"
sql$         = "INSERT into client VALUES ("; dbVals$ ; ")"
#sql execute(sql$)
goto [sho]

' ------------------------------------
' Select last entry
' ------------------------------------
[last]
sql$ = "SELECT *,client.rowid as rowid FROM client ORDER BY rowid desc LIMIT 1"
what$ = "---- Last Entry ----"
goto [shoQuery]

' ------------------------------------
' Select by category (Last date only)
' ------------------------------------
[lastCat]
sql$ = "SELECT * FROM client
WHERE client.clientDate = (SELECT max(c.clientDate)
FROM client as c WHERE c.category = client.category)
ORDER BY category"
what$ = "---- Last Category Sequence ----"
goto [shoQuery]

' ------------------------------------
' Select by date
' ------------------------------------
[date]
sql$ = "SELECT * FROM client ORDER BY clientDate"
what$ = "---- By Date ----"

[shoQuery]
cls
print what$
html "<TABLE BORDER=1 CELLPADDING=0 CELLSPACING=0>"
html "<TR align=center bgcolor=wheat><TD>Client
Num</TD><TD>Name</TD><TD>Client
Date</TD><TD>Category</TD></TR>" ' heading
#sql	execute(sql$)
WHILE  #sql hasanswer()
  #row = #sql #nextrow()
  clientNum	= #row clientNum()
  name$		= #row name$()
  clientDate$	= #row clientDate$()
  category$	= #row category$()

  html "<TR><TD align=right>";clientNum;"</TD><TD>";name$;"</TD><TD>";clientDate$;"</TD><TD>";category$;"</TD></TR>"
WEND
html "</TABLE>"
button #c, "Continue", [sho]
wait

' ------ the end -------
[exit]
end
```

Output:

---- User Input ----<br />
<TABLE BORDER=1 CELLPADDING=0 CELLSPACING=0 bgcolor=wheat>
<TR align=center BGCOLOR=tan><TD colspan=2>Client Maintenance</TD></TR>
<TR><TD bgcolor=tan align=right>Client Num</TD><TD>5</TD></TR>
<TR><TD bgcolor=tan align=right>Name</TD><TD>Dawnridge Winery</TD></TR>
<TR><TD bgcolor=tan align=right>Client Date</TD><TD>2008-06-18 22:16</TD></TR>
<TR><TD bgcolor=tan align=right>Category</TD><TD>wine</TD></TR>
<TR><TR bgcolor=tan><TD colspan=2 ALIGN=CENTER>[Add]  [Exit]</TD></TR></TABLE>

---- Last Entry ----<br />
<TABLE BORDER=1 CELLPADDING=0 CELLSPACING=0>
<TR align=center bgcolor=wheat><TD>Client
Num</TD><TD>Name</TD><TD>Client
Date</TD><TD>Category</TD></TR>
<TR><TD align=right>5</TD><TD>Dawnridge Winery</TD><TD>2008-06-18 22:16</TD><TD>wine</TD></TR></TABLE>

---- Last category Sequence ----<br />
<TABLE BORDER=1 CELLPADDING=0 CELLSPACING=0>
<TR align=center bgcolor=wheat><TD>Client
Num</TD><TD>Name</TD><TD>Client
Date</TD><TD>Category</TD></TR>
<TR><TD align=right>1</TD><TD>Home Sales</TD><TD>2012-01-01 10;20</TD><TD>broker</TD></TR>
<TR><TD align=right>4</TD><TD>Back 40 Equipment</TD><TD>2009-09-18 20:18</TD><TD>farm</TD></TR>
<TR><TD align=right>3</TD><TD>Floral Designs</TD><TD>2010-10-14 09:16</TD><TD>flowers</TD></TR>
<TR><TD align=right>2</TD><TD>Best Foods</TD><TD>2011-02-02 12:33</TD><TD>food</TD></TR>
<TR><TD align=right>5</TD><TD>Dawnridge Winery</TD><TD>2008-06-18 22:16</TD><TD>wine</TD></TR></TABLE>

---- Date Sequence ----<br />
<TABLE BORDER=1 CELLPADDING=0 CELLSPACING=0>
<TR align=center bgcolor=wheat><TD>Client
Num</TD><TD>Name</TD><TD>Client
Date</TD><TD>Category</TD></TR>
<TR><TD align=right>5</TD><TD>Dawnridge Winery</TD><TD>2008-06-18 22;16</TD><TD>wine</TD></TR>
<TR><TD align=right>4</TD><TD>Back 40 Equipment</TD><TD>2009-09-18 20:18</TD><TD>farm</TD></TR>
<TR><TD align=right>3</TD><TD>Floral Designs</TD><TD>2010-10-14 09:16</TD><TD>flowers</TD></TR>
<TR><TD align=right>2</TD><TD>Best Foods</TD><TD>2011-02-02 12:33</TD><TD>food</TD></TR>
<TR><TD align=right>1</TD><TD>Home Sales</TD><TD>2012-01-01 10:20</TD><TD>broker</TD></TR></TABLE>

## Scala


```Scala
object SimpleDatabase extends App {
  type Entry = Array[String]
  def asTSV(e: Entry) = e mkString "\t"
  def fromTSV(s: String) = s split "\t"
  val header = asTSV(Array("TIMESTAMP", "DESCRIPTION", "CATEGORY", "OTHER"))

  def read(filename: String) = try {
    scala.io.Source.fromFile(filename).getLines.drop(1).map(fromTSV)
  } catch {
    case e: java.io.FileNotFoundException => Nil
  }

  def write(filename: String, all: Seq[Entry]) = {
    import java.nio.file.{Files,Paths}
    import scala.collection.JavaConversions.asJavaIterable
    Files.write(Paths.get(filename), asJavaIterable(header +: all.map(asTSV)))
    all.size
  }

  def add(filename: String, description: String, category: String = "none", optional: Seq[String] = Nil) {
    val format = new java.text.SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
    val e = Array(format.format(new java.util.Date), description, category) ++ optional
    println(write(filename, read(filename).toBuffer :+ e) + " entries")
  }

  def print(filename: String, filter: Seq[Entry] => TraversableOnce[Entry]) =
    filter(read(filename).toList.sortBy(_.headOption)) map(_ mkString ",") foreach println

  args match {
    case Array(f, "latest") => print(f, _ takeRight 1)
    case Array(f, "latest", cat) => print(f, _ filter(_.lift(2) == Some(cat)) takeRight 1)
    case Array(f, "all") => print(f, _.toSeq)
    case Array(f, "all", "latest") => print(f, _ groupBy (_ lift 2 getOrElse "") map{case (_, cat) => cat.last})
    case Array(f, "add", desc) => add(f, desc, category = "")
    case Array(f, "add", desc, cat, opt @ _*) => add(f, desc, cat, opt)
    case _ => println("Usage: SimpleDatabase filename.tsv [all [latest]| latest [CATEGORY] | add [DESCRIPTION [CATEGORY [OPTIONAL]...]]]")
  }
}
```

{{out}}

```txt
> SimpleDatabase
Usage: SimpleDatabase filename.tsv [all [latest]| latest [CATEGORY] | add [DESCRIPTION [CATEGORY [OPTIONAL]...]]]

> SimpleDatabase database.tsv all

> SimpleDatabase database.tsv add one
1 entries

> SimpleDatabase database.tsv add two test
2 entries

> SimpleDatabase database.tsv add three test optional
3 entries

> SimpleDatabase database.tsv add four final
4 entries

> SimpleDatabase database.tsv all
2014-10-03 12:00:01,one
2014-10-03 12:00:02,two,test
2014-10-03 12:00:03,three,test,optional
2014-10-03 12:00:04,four,final

> SimpleDatabase database.tsv all latest
2014-10-03 12:00:04,four,final
2014-10-03 12:00:03,three,test,optional
2014-10-03 12:00:01,one

> SimpleDatabase database.tsv latest
2014-10-03 12:00:04,four,final

> SimpleDatabase database.tsv latest test
2014-10-03 12:00:03,three,test,optional
```



## Tcl

The format used is that of a Tcl dictionary, where each entry uses the title as a key and the remaining information (category, date and miscellaneous metadata) is the value associated with it. The only variation from the standard internal format is that entries are separated by newlines instead of spaces; this is still a legal value, but is a non-canonical.

```tcl
#!/usr/bin/env tclsh8.6
package require Tcl 8.6
namespace eval udb {
    variable db {}

    proc Load {filename} {
	variable db
	if {[catch {set f [open $filename]}]} {
	    set db {}
	    return
	}
	set db [read $f]
	close $f
    }
    proc Store {filename} {
	variable db
	if {[catch {set f [open $filename w]}]} return
	dict for {nm inf} $db {
	    puts $f [list $nm $inf]
	}
	close $f
    }

    proc add {title category {date "now"} args} {
	variable db
	if {$date eq "now"} {
	    set date [clock seconds]
	} else {
	    set date [clock scan $date]
	}
	dict set db $title [list $category $date $args]
	return
    }
    proc Rec {nm cat date xtra} {
	dict create description $nm category $cat date [clock format $date] \
	    {*}$xtra _names [dict keys $xtra]
    }
    proc latest {{category ""}} {
	variable db
	if {$category eq ""} {
	    set d [lsort -stride 2 -index {1 1} -integer -decreasing $db]
	    dict for {nm inf} $d break
	    return [list [Rec $nm {*}$inf]]
	}
	set latestbycat {}
	dict for {nm inf} [lsort -stride 2 -index {1 1} -integer $db] {
	    dict set latestbycat [lindex $inf 0] [list $nm {*}$inf]
	}
	return [list [Rec {*}[dict get $latestbycat $category]]]
    }
    proc latestpercategory {} {
	variable db
	set latestbycat {}
	dict for {nm inf} [lsort -stride 2 -index {1 1} -integer $db] {
	    dict set latestbycat [lindex $inf 0] [list $nm {*}$inf]
	}
	set result {}
	dict for {- inf} $latestbycat {
	    lappend result [Rec {*}$inf]
	}
	return $result
    }
    proc bydate {} {
	variable db
	set result {}
	dict for {nm inf} [lsort -stride 2 -index {1 1} -integer $db] {
	    lappend result [Rec $nm {*}$inf]
	}
	return $result
    }

    namespace export add latest latestpercategory bydate
    namespace ensemble create
}

if {$argc < 2} {
    puts stderr "wrong # args: should be \"$argv0 dbfile subcommand ?args...?\""
    exit 1
}
udb::Load [lindex $argv 0]
set separator ""
if {[catch {udb {*}[lrange $argv 1 end]} msg]} {
    puts stderr [regsub "\"udb " $msg "\"$argv0 dbfile "]
    exit 1
}
foreach row $msg {
    puts -nonewline $separator
    apply {row {
	dict with row {
	    puts "Title: $description"
	    puts "Category: $category"
	    puts "Date: $date"
	    foreach v $_names {
		puts "${v}: [dict get $row $v]"
	    }
	}
    }} $row
    set separator [string repeat - 70]\n
}

udb::Store [lindex $argv 0]
```

Sample session:

```bash
bash$ udb.tcl db
wrong # args: should be "udb.tcl dbfile subcommand ?args...?"
bash$ udb.tcl db ?
unknown or ambiguous subcommand "?": must be add, bydate, latest, or latestpercategory
bash$ udb.tcl db add
wrong # args: should be "udb.tcl dbfile add title category ?date? ?arg ...?"
bash$ udb.tcl db add "Title 1" foo
bash$ udb.tcl db add "Title 2" foo
bash$ udb.tcl db add "Title 3" bar
bash$ udb.tcl db bydate
Title: Title 1
Category: foo
Date: Tue Nov 15 18:11:58 GMT 2011
----------------------------------------------------------------------
Title: Title 2
Category: foo
Date: Tue Nov 15 18:12:01 GMT 2011
----------------------------------------------------------------------
Title: Title 3
Category: bar
Date: Tue Nov 15 18:12:07 GMT 2011
bash$ udb.tcl db latest
Title: Title 3
Category: bar
Date: Tue Nov 15 18:12:07 GMT 2011
bash$ udb.tcl db latest foo
Title: Title 2
Category: foo
Date: Tue Nov 15 18:12:01 GMT 2011
bash$ udb.tcl db latest bar
Title: Title 3
Category: bar
Date: Tue Nov 15 18:12:07 GMT 2011
bash$ udb.tcl db latestpercategory
Title: Title 2
Category: foo
Date: Tue Nov 15 18:12:01 GMT 2011
----------------------------------------------------------------------
Title: Title 3
Category: bar
Date: Tue Nov 15 18:12:07 GMT 2011
bash$ udb.tcl db add "Title 4" bar "12:00 Monday last week"
bash$ udb.tcl db bydate
Title: Title 4
Category: bar
Date: Mon Nov 14 12:00:00 GMT 2011
----------------------------------------------------------------------
Title: Title 1
Category: foo
Date: Tue Nov 15 18:11:58 GMT 2011
----------------------------------------------------------------------
Title: Title 2
Category: foo
Date: Tue Nov 15 18:12:01 GMT 2011
----------------------------------------------------------------------
Title: Title 3
Category: bar
Date: Tue Nov 15 18:12:07 GMT 2011
bash$ cat db
{Title 1} {foo 1321380718 {}}
{Title 2} {foo 1321380721 {}}
{Title 3} {bar 1321380727 {}}
{Title 4} {bar 1321272000 {}}
bash$ udb.tcl db add "Title 5" foo "12:00 Monday last week" Comment 'Wholly excellent!'
bash$ cat db
{Title 1} {foo 1321380718 {}}
{Title 2} {foo 1321380721 {}}
{Title 3} {bar 1321380727 {}}
{Title 4} {bar 1321272000 {}}
{Title 5} {foo 1321272000 {Comment {Wholly excellent!}}}
bash$ udb.tcl db bydate
Title: Title 4
Category: bar
Date: Mon Nov 14 12:00:00 GMT 2011
----------------------------------------------------------------------
Title: Title 5
Category: foo
Date: Mon Nov 14 12:00:00 GMT 2011
Comment: Wholly excellent!
----------------------------------------------------------------------
Title: Title 1
Category: foo
Date: Tue Nov 15 18:11:58 GMT 2011
----------------------------------------------------------------------
Title: Title 2
Category: foo
Date: Tue Nov 15 18:12:01 GMT 2011
----------------------------------------------------------------------
Title: Title 3
Category: bar
Date: Tue Nov 15 18:12:07 GMT 2011
```



## ToffeeScript


```coffeescript
#!/usr/local/bin/toffee

prog = require 'commander'
fs = require 'fs-extra'

if not fs.exists! 'data.json'
  fs.outputJson! 'data.json', {}

prog
  .command('add <name> <category> [date]')
  .description('Add a new entry')
  .option('-n <text>', 'notes')
  .option('-t <tags>', 'tags')
  .action addentry

prog
  .command('latest')
  .description('Print the latest entry')
  .action latest

prog
  .command('catlatest')
  .description('Print the latest entry for each category')
  .action catlatestout

prog
  .command('list')
  .description('Print all entries sorted by date')
  .action bydate


addentry = (name, category, dt, options) ->
  if dt? then dat = new Date(dt) else dat = new Date()
  update =
    name: name
    category: category
    tags: options?.T
    notes: options?.N
    date: dat.getTime()
  e, data = fs.readJson! 'data.json'
  if not data[category]?
    data[category] = []
  data[category].push update
  fs.outputJson 'data.json', data

byDateNew = (a, b) ->
  if a.date<b.date then return 1
  if b.date>a.date then return -1
  return 0

catlatest = (cb) ->
  e, data = fs.readJson! 'data.json'
  ret = []
  for cat, list of data
    list.sort byDateNew
    ret.push list[0]
  cb ret

printFormatted = (entry) ->
  tmp = entry.date
  entry.date = new Date(entry.date).toDateString()
  console.log entry
  entry.date = tmp

latest = ->
  newpercat = catlatest!
  newpercat.sort byDateNew
  printFormatted newpercat[0]

catlatestout = ->
  items = catlatest!
  for item in items
    printFormatted item

bydate = ->
  e, data = fs.readJson! 'data.json'
  entries = []
  for cat, list of data
    for item in list
      entries.push item
  entries.sort byDateNew
  for entry in entries
    printFormatted entry

prog.parse process.argv
```



## UNIX Shell

This format is guaranteed to be human readable: if you can type it, you can read it.

```bash
#!/bin/sh

db_create() {
	mkdir ./"$1" && mkdir "./$1/.tag" && echo "Create DB \`$1'"
}

db_delete() {
	rm -r ./"$1" && echo "Delete DB \`$1'"
}

db_show() {
	if [ -z "$2" ]; then show_help; fi
	for x in "./$1/$2/"*; do
		echo "$x:" | sed "s/.*\///"
		cat "$x" | sed "s/^/    /"
		echo
	done

	printf "Tags: "
	ls "./$1/$2/.tag"
}

db_tag() {
	local db="$1" item="$2"
	shift
	shift
	for tag in $@; do
		mkdir "./$db/.tag/$tag"
		ln -s "$PWD/$db/$item" "./$db/.tag/$tag/"
		touch "./$db/$item/.tag/$tag"
	done
}

show_help() {
	echo "Usage: $0 command [args]"
	echo "Commands:"
	cat $0 | grep ") ##" | grep -v grep | sed 's/) ## /:\t/'
	exit
}

if [ -z "$1" ]; then show_help; fi

action=$1 it=database
shift
case $action in
	create) ## db -- create $it
		db_create "$@" ;;

	drop) ## db -- delete $it
		db_delete "$@" ;;

	add) ## db item -- add new item to $it
		mkdir -p "./$1/$2/.tag" && touch "./$1/$2/Description" ;;

	rem) ## db item -- delete item from $it
		rm -r "./$1/$2"
		rm "./$1/.tag/"*"/$2"
		;;

	show) ## db item -- show item
		db_show "$@" ;;

	newtag) ## db new-tag-name -- create new tag name
		mkdir "./$1/.tag/$2" ;;

	prop) ## db item property-name property-content -- add property to item
		echo "$4" > "./$1/$2/$3" ;;

	tag) ## db item tag [more-tags...] -- mark item with tags
		db_tag "$@" ;;

	last) ## db -- show latest item
		ls "$1" --sort=time | tail -n 1
		;;

	list) ## db -- list all items
		ls "$1" -1 --sort=time
		;;

	last-all) ## db -- list items in each category
		for x in "$1/.tag/"*; do
			echo "$x" | sed 's/.*\//Tag: /'
			printf "    "
			ls "$x" --sort=time | tail -n 1
			echo
		done
		;;

	help) ## this message
		show_help
		;;

	*)	echo Bad DB command: $1
		show_help
		;;
esac
```

Sample usage (assuming script is named "sdb"):<lang>$ sdb create CDs
Create DB `CDs'
$ sdb add CDs Bookends
$ sdb prop CDs Bookends artists "Simon & Garfunkel"
$ sdb add CDs "Ode to joy"
$ sdb prop CDs "Ode to joy" artist "Beethoven"
$ sdb tag CDs Bookends rock folk  # I'm not sure about this
$ sdb tag CDs "Ode to joy" classical
$ sdb show CDs Bookends
Description:

artists:
    Simon & Garfunkel

Tags: folk  rock
$ sdb prop CDs "Ode to joy" Description "Sym. No. 9"
$ sdb show CDs "Ode to joy"
Description:
    Sym. No. 9

artist:
    Beethoven

Tags: classical
$ sdb last-all CDs
Tag: classical
    Ode to joy

Tag: folk
    Bookends

Tag: rock
    Bookends

$ sdb drop CDs
Delete DB `CDs'
$
```

