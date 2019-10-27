+++
title = "Table creation"
description = ""
date = 2019-01-22T15:28:47Z
aliases = []
[extra]
id = 1553
[taxonomies]
categories = []
tags = []
+++

{{draft task|Database operations}}

;Task:
Create a database table to exemplify most commonly used data types and options.


;Related task:
* [[Table Creation - Address]]





## AWK



### AWK + SQLite


AWK is just a glue language. Simply pipe the creation command into SQLite and capture the output.

```awk
#!/bin/sh -f
awk '
BEGIN {
    "echo \"create table pow (name, rank, serno);\" |sqlite3 pow.db" | getline
    print "Result: " $0
    exit;
}
'
```



## C

Most database tables store data as text, number (mostly integer) and date or date time columns. Specialized requirements would need BLOB and other datatypes. The following implementation requires SQLite.

{{libheader|SQLite}}

```C

#include <sqlite3.h>
#include <stdlib.h>
#include <stdio.h>

int main()
{
  sqlite3 *db = NULL;
  char *errmsg;
  
	const char *code = 
	"CREATE TABLE employee (\n"
	"    empID		INTEGER PRIMARY KEY AUTOINCREMENT,\n"
	"	firstName	TEXT NOT NULL,\n"
	"	lastName	TEXT NOT NULL,\n"
	"	AGE			INTEGER NOT NULL,\n"
	"	DOB			DATE NOT NULL)\n" ; 
	
  if ( sqlite3_open("employee.db", &db) == SQLITE_OK ) {
    sqlite3_exec(db, code, NULL, NULL,  &errmsg);
    sqlite3_close(db);
  } else {
    fprintf(stderr, "cannot open db...\n");
    sqlite3_close(db);
    exit(EXIT_FAILURE);
  }
  return 0;
}

```



## FunL


```funl
import db.*
import util.*

Class.forName( 'org.h2.Driver' )
conn = DriverManager.getConnection( "jdbc:h2:~/test", "sa", "" )
statement = conn.createStatement()
statement.execute( '''
  CREATE TABLE Persons
  (
    PersonID int,
    FirstName varchar(255),
    LastName varchar(255),
    Address varchar(255),
    City varchar(255),
    Province char(2)
  )''' )
statement.execute( '''
  INSERT INTO Persons VALUES
    (1, 'Sylvia', 'Henry', '5896 Cotton Prairie Wharf', 'Parrsboro', 'SK'),
    (2, 'Kelly', 'Saunders', '3608 Indian Island Promenade', 'Goober Hill', 'SK'),
    (3, 'Vernon', 'Douglas', '394 Dusty Impasse', 'Muleshoe', 'NS'),
    (4, 'Jim', 'Fleming', '2523 Quaking Fawn Trace', 'Halbrite', 'ON'),
    (5, 'Roderick', 'Owens', '7596 Umber View', 'Frognot', 'SK')
    ''' )
statement.execute( "SELECT * FROM Persons ORDER BY LastName" )
print( TextTable.apply(statement.getResultSet()) )
conn.close()
```

  
{{out}}


```txt

+----------+-----------+----------+------------------------------+-------------+----------+
| PERSONID | FIRSTNAME | LASTNAME |           ADDRESS            |    CITY     | PROVINCE |
+----------+-----------+----------+------------------------------+-------------+----------+
|        3 | Vernon    | Douglas  | 394 Dusty Impasse            | Muleshoe    | NS       |
|        4 | Jim       | Fleming  | 2523 Quaking Fawn Trace      | Halbrite    | ON       |
|        1 | Sylvia    | Henry    | 5896 Cotton Prairie Wharf    | Parrsboro   | SK       |
|        5 | Roderick  | Owens    | 7596 Umber View              | Frognot     | SK       |
|        2 | Kelly     | Saunders | 3608 Indian Island Promenade | Goober Hill | SK       |
+----------+-----------+----------+------------------------------+-------------+----------+

```



## Go

{{libheader|Bolt}}


This uses a key/value store rather than a relational database to create the table.

```go
package main

import (
    "encoding/binary"
    "encoding/json"
    "fmt"
    "github.com/boltdb/bolt"
    "log"
)

type StockTrans struct {
    Id       int // this will be auto-incremented by db
    Date     string
    Trans    string
    Symbol   string
    Quantity int
    Price    float32
    Settled  bool
}

// save stock transaction to bucket in db
func (st *StockTrans) save(db *bolt.DB, bucket string) error {
    err := db.Update(func(tx *bolt.Tx) error {
        b := tx.Bucket([]byte(bucket))
        id, _ := b.NextSequence()
        st.Id = int(id)
        encoded, err := json.Marshal(st)
        if err != nil {
            return err
        }
        return b.Put(itob(st.Id), encoded)
    })
    return err
}

// itob returns an 8-byte big endian representation of i.
func itob(i int) []byte {
    b := make([]byte, 8)
    binary.BigEndian.PutUint64(b, uint64(i))
    return b
}

func check(err error) {
    if err != nil {
        log.Fatal(err)
    }
}

func main() {
    // create database
    db, err := bolt.Open("store.db", 0600, nil)
    check(err)
    defer db.Close()

    // create bucket
    err = db.Update(func(tx *bolt.Tx) error {
        _, err := tx.CreateBucketIfNotExists([]byte("stocks"))
        return err
    })
    check(err)

    transactions := []*StockTrans{
        {0, "2006-01-05", "BUY", "RHAT", 100, 35.14, true},
        {0, "2006-03-28", "BUY", "IBM", 1000, 45, true},
        {0, "2006-04-06", "SELL", "IBM", 500, 53, true},
        {0, "2006-04-05", "BUY", "MSOFT", 1000, 72, false},
    }

    // save transactions to bucket
    for _, trans := range transactions {
        err := trans.save(db, "stocks")
        check(err)
    }

    // print out contents of bucket
    fmt.Println("Id     Date    Trans  Sym    Qty  Price  Settled")
    fmt.Println("------------------------------------------------")
    db.View(func(tx *bolt.Tx) error {
        b := tx.Bucket([]byte("stocks"))
        b.ForEach(func(k, v []byte) error {
            st := new(StockTrans)
            err := json.Unmarshal(v, st)
            check(err)
            fmt.Printf("%d  %s  %-4s  %-5s  %4d  %2.2f  %t\n",
                st.Id, st.Date, st.Trans, st.Symbol, st.Quantity, st.Price, st.Settled)
            return nil
        })
        return nil
    })
}
```


{{out}}

```txt

Id     Date    Trans  Sym    Qty  Price  Settled
------------------------------------------------
1  2006-01-05  BUY   RHAT    100  35.14  true
2  2006-03-28  BUY   IBM    1000  45.00  true
3  2006-04-06  SELL  IBM     500  53.00  true
4  2006-04-05  BUY   MSOFT  1000  72.00  false

```



## J


If we define a <code>table</code> as a named collection of columns, and we define a <code>type</code> as a mechanism for the representation of some kind of data, then:


```j
stocks=: |: ,: ;:'date trans symbol qty price'
insertStock=: 3 :'0#stocks=: stocks,.y'
insertStock@".;._2]0 :0
   '2006-01-05'; 'BUY';  'RHAT';   100; 35.14
   '2006-03-28'; 'BUY';  'IBM';   1000; 45.00
   '2006-04-05'; 'BUY';  'MSOFT'; 1000; 72.00
   '2006-04-06'; 'SELL'; 'IBM';    500; 53.00
)
```


declares a table and some data within that table.

And, here's an example of sorting:


```j
cols=: [:; {."1@[ <@i.`(<@i.@#@[)@.(=&(<,'*')@]"1 0) cutopen@]
sortBy=: [ /:"1 2 (<__)|:@,. [ }.@{~ cols
from=: cols~ {"0 _ ]
select=: |:

   select '*' from stocks sortBy 'price'
┌──────────┬─────┬──────┬────┬─────┐
│date      │trans│symbol│qty │price│
├──────────┼─────┼──────┼────┼─────┤
│2006-01-05│BUY  │RHAT  │100 │35.14│
├──────────┼─────┼──────┼────┼─────┤
│2006-03-28│BUY  │IBM   │1000│45   │
├──────────┼─────┼──────┼────┼─────┤
│2006-04-06│SELL │IBM   │500 │53   │
├──────────┼─────┼──────┼────┼─────┤
│2006-04-05│BUY  │MSOFT │1000│72   │
└──────────┴─────┴──────┴────┴─────┘
```


Note that this particular example is both overly general in some senses (for example, named column handling has features not demonstrated here) and overly specific in others (for example, I did not implement sort in descending order).  

Also, a properly tuned system would likely use different code (for example, you could get better performance if you put an entire column into a box instead of introducing a new box for each element in a column).


## Julia

{{works with|Julia|0.6}}


```julia
using SQLite

conn = SQLite.DB() # in-memory
SQLite.execute!(conn, """
    create table stocks
    (date text, trans text, symbol text,
    qty real, price real)
    """)

# Insert a row of data
SQLite.execute!(conn, """
    insert into stocks
        values ('2006-01-05','BUY','RHAT',100,35.14)
    """)

for v in [["2006-03-28", "BUY",  "IBM",   1000, 45.00],
          ["2006-04-05", "BUY",  "MSOFT", 1000, 72.00],
          ["2006-04-06", "SELL", "IBM",   500,  53.00]]
    SQLite.query(conn, "insert into stocks values (?,?,?,?,?)", values = v)
end

df = SQLite.query(conn, "select * from stocks order by price")
println(df)
```


{{out}}

```txt
4×5 DataFrames.DataFrame
│ Row │ date         │ trans  │ symbol  │ qty    │ price │
├─────┼──────────────┼────────┼─────────┼────────┼───────┤
│ 1   │ "2006-01-05" │ "BUY"  │ "RHAT"  │ 100.0  │ 35.14 │
│ 2   │ "2006-03-28" │ "BUY"  │ "IBM"   │ 1000.0 │ 45.0  │
│ 3   │ "2006-04-06" │ "SELL" │ "IBM"   │ 500.0  │ 53.0  │
│ 4   │ "2006-04-05" │ "BUY"  │ "MSOFT" │ 1000.0 │ 72.0  │
```



## Lua


```lua
Columns = {};
Columns.ID = {};
Columns.FName = {};
Columns.LName = {};
Columns.Email = {};
Columns.Names = {"ID","FName","LName","Email"};

function Insert(id,fname,lname,email)
	table.insert(Columns.ID, id);
	table.insert(Columns.FName, fname);
	table.insert(Columns.LName, lname);
	table.insert(Columns.Email, email);
end

for i,v in pairs(Columns.ID) do
	print(v,Columns.FName[i],Columns.LName[i]);
end

function getMax(Table)
	local cmax = #Table
	for i,v in pairs(Columns[Table]) do
		if #tostring(v) > cmax then
			cmax = #tostring(v)
		end
	end
	return cmax;
end

function listTables()
	local Total = (#Columns.Names*2)+1;
	for i,v in pairs(Columns.Names) do
		Total = Total + getMax(v);
	end
	print()
	local CS = "|";
	for i,v in pairs(Columns.Names) do
		CS = CS.." "..v..string.rep(" ",(getMax(v)-#v)).."|";
	end
	print(string.rep("-",Total).."\n"..CS.."\n"..string.rep("-",Total))
	for it = 1,#Columns.ID do
		CS = "|";
		for i,v in pairs(Columns.Names) do
			CS = CS.." "..Columns[v][it]..string.rep(" ",(getMax(v)-(#tostring((Columns[v][it]))))).."|";
		end
		print(CS);
	end
	print(string.rep("-",Total));
end

--[[Inserting items]]--
Insert(#Columns.ID,"John","Doel","John.Doe000@ExampleEmail.com");
Insert(#Columns.ID,"Jane","Miller","Jane.Miller000@ExampleEmail.com");
Insert(#Columns.ID,"Eerie","Crate","Eeriecrate@ExampleEmail.com");
--[[               ]]--

listTables();
```


{{out}}


```txt

-----------------------------------------------------
| ID| FName| LName | Email                          |
-----------------------------------------------------
| 0 | John | Doel  | John.Doe000@ExampleEmail.com   |
| 1 | Jane | Miller| Jane.Miller000@ExampleEmail.com|
| 2 | Eerie| Crate | Eeriecrate@ExampleEmail.com    |
-----------------------------------------------------

```



## M2000 Interpreter

M2000 Environment use ADO to connect to databases. Default type is Mdb (Acess 2007).

We can use text from UTF16LE set. Here we have Greek letters in Memo.


```M2000 Interpreter

MODULE SIMPLEBASE {
      BASE "ALFA"  ' ERASED IF FOUND THE NAME OF "ALFA.MDB"
      EXECUTE "ALFA", { CREATE TABLE Employees(ID autoincrement primary key,  LastName   VARCHAR(40) ,  FirstName  VARCHAR(40)  NOT NULL, MyMemo TEXT )}
      APPEND "ALFA","Employees",,"George","Getbreak","Γεια χαρά από Ελλάδα"
      RETRIEVE "ALFA", "Employees", 1,"",""
      READ MANY, ID, LASTNAME$, FIRSTNAMES$, MEMO$
      PRINT $(4, 6),  "BASE:","ALFA"
      Print "ID:",ID
      PRINT  "NAME:",LASTNAME$ + " " + FIRSTNAMES$
      PRINT "MEMO:",
      REPORT  MEMO$
      CLOSE BASE "ALFA"
      PRINT
}
SIMPLEBASE

```


## Mathematica


```Mathematica
Needs["DatabaseLink`"];conn = OpenSQLConnection[JDBC["mysql", 
"databases:1234/conn_test"], "Username" -> "test"]
SQLCreateTable[conn, SQLTable["TEST"],If[Length[#] == 0,SQLColumn[StringJoin[#,"COL"],"DataTypeName" -> #],SQLColumn[StringJoin[#[[1]], "COL"], "DataTypeName" -> #[[1]],"DataLength" -> #[[2]]]] & /@ {"TINYINT", "SMALLINT", "INTEGER","BIGINT", "NUMERIC", "DECIMAL", "FLOAT", "REAL", "DOUBLE", "BIT","LONGVARBINARY", "VARBINARY", "BINARY","LONGVARCHAR",{"VARCHAR", 5},{"CHAR", 3},"DATE","TIME","TIMESTAMP","OBJECT"}]
```



## Oracle

Great SCOTT! from utlsampl.sql

```sql

CREATE TABLE EMP
       (EMPNO NUMBER(4) CONSTRAINT PK_EMP PRIMARY KEY,
        ENAME VARCHAR2(10),
        JOB VARCHAR2(9),
        MGR NUMBER(4),
        HIREDATE DATE,
        SAL NUMBER(7,2),
        COMM NUMBER(7,2),
        DEPTNO NUMBER(2) CONSTRAINT FK_DEPTNO REFERENCES DEPT);

```



## Oz

{{trans|Python}}

{{libheader|SQLite}}
{{libheader|Ozsqlite}}


```oz
declare
  [Sqlite] = {Module.link ['x-ozlib:/sqlite/Sqlite.ozf']}

  DB = {Sqlite.open 'test.db'}
in
  try
     %% show strings as text, not as number lists
     {Inspector.configure widgetShowStrings true}

     %% create table
     {Sqlite.exec DB
      "create table stocks(date text, trans text, symbol test,"
      #"qty real, price real)" _}
     
     %% insert using a SQL string
     {Sqlite.exec DB "insert into stocks values "
      #"('2006-01-05','BUY','RHAT',100,35.14)" _}
     
     %% insert with insert procedure
     for T in
	[r(date:"2006-03-28" trans:"BUY" symbol:"IBM" qty:1000 price:45.00)
	 r(date:"2006-04-05" trans:"BUY" symbol:"MSOFT" qty:1000 price:72.00)
	 r(date:"2006-04-06" trans:"SELL" symbol:"IBM" qty:500 price:53.00)]
     do
	{Sqlite.insert DB stocks T}
     end
     
     %% read table and show rows in Inspector
     for R in {Sqlite.exec DB "select * from stocks order by price"} do
	{Inspect R}
     end

  catch E then
     {Inspect E}
  finally
     {Sqlite.close DB}
  end

```



## PARI/GP

The most natural way to store tabular data in GP is in a matrix:

```parigp
m=matrix(10,3);
m[1,] = ["Barack", "Obama", 20500];
\\ ...
```



## Perl 6

In Perl 6, there is no 'database' type built in, so it is somewhat ambiguous when specifying 'create a database table'. Perl 6 offers bindings to most common databases through its DBIish module but mostly abstracts away the differences between the underlying databases, which hides many of the finer distinctions of what may be stored where. The actual data types and options available are properties of the database used. 

If on the other hand, we are meant to show built in collective types that may be used to hold tabular data, this may be of some use.

In general, a container type can hold objects of any data type, even instances of their own type; allowing 'multi-dimensional' (tabular) containers. 

Perl 6 offers two broad categories of collective container types; those that do the Positional role and those that do Associative. Positional objects are collective objects that access the individual storage slots using an integer index. Associative objects use some sort of other pointer (typically string) to access their storage slots.

The various Associative types mostly differ in their value handling. Hash, Map and QuantHash may have any type of object as their value. All the others have some specific, usually numeric, type as their value.


```txt

Positional - Object that supports looking up values by integer index
    Array     Sequence of itemized objects
    List      Immutable sequence of objects

Associative - Object that supports looking up values by key (typically string)
    Bag        Immutable collection of distinct objects with integer weights
    BagHash    Mutable collection of distinct objects with integer weights
    Hash       Mapping from strings to itemized values
    Map        Immutable mapping from strings to values
    Mix        Immutable collection of distinct objects with Real weights
    MixHash    Mutable collection of distinct objects with Real weights
    QuantHash  Collection of objects represented as hash keys
    Set        Immutable collection of distinct objects, no value except 'present'
    SetHash    Mutable collection of distinct objects, no value except 'present'

```


If you want a persistent instance of any of these types, you need to declare the name with some scope constraint, but the are no prerequisites to creating instances. Simply assigning values to them will call them into existence.


## Phix

{{libheader|SQLite}}

```Phix
include pSQLite.e
constant sqlcode = """
CREATE TABLE IF NOT EXISTS employee (
 empID      INTEGER PRIMARY KEY AUTOINCREMENT,
 firstName  TEXT NOT NULL,
 lastName   TEXT NOT NULL,
 age        INTEGER NOT NULL,
 dob        DATE NOT NULL)"""
 
sqlite3 db = sqlite3_open("employees.sqlite")
integer res = sqlite3_exec(db,sqlcode)
if res=SQLITE_OK then
    sqlite3_close(db)
else
    printf(1,"sqlite3_exec error: %d [%s]\n",{res,sqlite_last_exec_err})
end if
```



## PicoLisp


```PicoLisp
(scl 2)

(class +Account +Entity)
(rel id        (+Key +Number))
(rel created   (+Date))
(rel active    (+Bool))
(rel username  (+Key +String))
(rel balance   (+Number) 2)
(rel age       (+Number))
(rel notes     (+Blob))

(pool "account.db")  # Create database

(new! '(+Account)
   'id 12345
   'username "John Doe"
   'balance 77.22
   'created (date 2009 5 13) )

(new! '(+Account)
   'id 12346
   'username "Jane Miller"
   'active T
   'created (date 2009 5 14)
   'balance 123.75 )

(let Fmt (-13 -10 -9 -11 10)
   (tab Fmt "account_id" "created" "active" "username" "balance")
   (for This (collect 'id '+Account)
      (tab Fmt
         (: id)
         (dat$ (: created))
         (if (: active) "Yes" "No")
         (: username)
         (money (: balance)) ) ) )
```

Output:

```txt
account_id   created   active   username      balance
12345        20090513  No       John Doe        77.22
12346        20090514  Yes      Jane Miller    123.75
```



## PL/I


```PL/I
declare 1 table (100),
          2 name character (20) varying,
          2 address,
            3 number fixed decimal,
            3 street character (30) varying,
            3 suburb character (30) varying,
            3 zip picture '9999',
          2 transaction_date date,
          2 sex character (1),
          2 suppress_junk_mail bit (1);
```



## PostgreSQL


Postgres developers, please feel free to add additional data-types you commonly use to this example.


```sql
-- This is a comment

CREATE SEQUENCE account_seq start 100;
CREATE TABLE account (
  account_id  int4        PRIMARY KEY DEFAULT nextval('account_seq'),
  created     date        not null default now(),
  active      bool        not null default 't',
  username    varchar(16) unique not null,
  balance     float       default 0,
  age         int2,
  notes       text
);

CREATE TABLE account_note (
  account_id  int4      not null REFERENCES account,
  created     timestamp not null default now(),
  note        text      not null,
  unique(account_id, note)
); 
-- bool:       't', 'f' or NULL
-- int2:       -32768 to +32767
-- int4:       -2147483648 to +2147483647
-- float:      decimal
-- date:       obvious
-- timestamp:  date time
-- char(#):    space padded text field with length of #
-- varchar(#): variable length text field up to #
-- text:       not limited
```




## Python

{{libheader|SQLite}}
The sqlite3 database is a part of the Python standard library. It does not associate type with table columns, any cell can be of any type.

```python>>>
 import sqlite3
>>> conn = sqlite3.connect(':memory:')
>>> c = conn.cursor()
>>> c.execute('''create table stocks
(date text, trans text, symbol text,
 qty real, price real)''')
<sqlite3.Cursor object at 0x013263B0>
>>> # Insert a row of data
c.execute("""insert into stocks
          values ('2006-01-05','BUY','RHAT',100,35.14)""")

<sqlite3.Cursor object at 0x013263B0>
>>> for t in [('2006-03-28', 'BUY', 'IBM', 1000, 45.00),
          ('2006-04-05', 'BUY', 'MSOFT', 1000, 72.00),
          ('2006-04-06', 'SELL', 'IBM', 500, 53.00),
         ]:
	c.execute('insert into stocks values (?,?,?,?,?)', t)

	
<sqlite3.Cursor object at 0x013263B0>
<sqlite3.Cursor object at 0x013263B0>
<sqlite3.Cursor object at 0x013263B0>
>>> # Data retrieval
>>> c = conn.cursor()
>>> c.execute('select * from stocks order by price')
<sqlite3.Cursor object at 0x01326530>
>>> for row in c:
	print row

	
(u'2006-01-05', u'BUY', u'RHAT', 100.0, 35.140000000000001)
(u'2006-03-28', u'BUY', u'IBM', 1000.0, 45.0)
(u'2006-04-06', u'SELL', u'IBM', 500.0, 53.0)
(u'2006-04-05', u'BUY', u'MSOFT', 1000.0, 72.0)
>>> 
```



## Racket

This is the relevant part of [[Table creation/Postal addresses#Racket]] that creates the DB table:

```racket

#lang racket
(require db)
(define postal (sqlite3-connect #:database "/tmp/postal.db" #:mode 'create))

```



## REXX

REXX doesn't have tables (structures), as there is only one data type in REXX:   '''character'''.

However, tables (or structures) can be constructed by using stemmed arrays;   the index would (should) be 

a unique identifier,   something akin to a SSN  (Social Security Number)  or something similar.

```rexx
      id             = 000112222   /*could be a SSN or some other unique ID (or number).*/

table.id.!firstname  = 'Robert'
table.id.!middlename = 'Jon'
table.id.!lastname   = 'Smith'
table.id.!dob        = '06/09/1946'
table.id.!gender     = 'm'
table.id.!phone      = '(111)-222-3333'
table.id.!addr       = '123 Elm Drive\Apartment 6A'
table.id.!town       = 'Gotham City'
table.id.!state      = 'NY'
table.id.!zip        = '12345-6789'
```





## Ring


```ring

# Project : Table creation

load "stdlib.ring"
oSQLite = sqlite_init()

sqlite_open(oSQLite,"mytest.db")

sql = "CREATE TABLE COMPANY("  +
        "ID INT PRIMARY KEY     NOT NULL," +
        "NAME           TEXT    NOT NULL," +
        "AGE            INT     NOT NULL," +
        "ADDRESS        CHAR(50)," +
        "SALARY         REAL );"

sqlite_execute(oSQLite,sql)

sql = "INSERT INTO COMPANY (ID,NAME,AGE,ADDRESS,SALARY) "  +
        "VALUES (1, 'Mahmoud', 29, 'Jeddah', 20000.00 ); " +
        "INSERT INTO COMPANY (ID,NAME,AGE,ADDRESS,SALARY) "  +
        "VALUES (2, 'Ahmed', 27, 'Jeddah', 15000.00 ); "     +
        "INSERT INTO COMPANY (ID,NAME,AGE,ADDRESS,SALARY)" +
        "VALUES (3, 'Mohammed', 31, 'Egypt', 20000.00 );" +
        "INSERT INTO COMPANY (ID,NAME,AGE,ADDRESS,SALARY)" +
        "VALUES (4, 'Ibrahim', 24, 'Egypt ', 65000.00 );"

sqlite_execute(oSQLite,sql)

aResult =  sqlite_execute(oSQLite,"select * from COMPANY")
for x in aResult
    for t in x
        see t[2] + nl
    next
next
see copy("*",50)  + nl
for x in aResult
    see x["name"] + nl
next
sqlite_close(oSQLite)

```

Output:

```txt

1
Mahmoud
29
Jeddah
20000.0
2
Ahmed
27
Jeddah
15000.0
3
Mohammed
31
Egypt
20000.0
4
Ibrahim
24
Egypt 
65000.0
**************************************************
Mahmoud
Ahmed
Mohammed
Ibrahim

```



## Ruby

This code is enough to create a PStore (or open an existing PStore).


```ruby
require 'pstore'
db = PStore.new "filename.pstore"
```


The example at [[Table creation/Postal addresses#Ruby]] puts Ruby objects into the PStore.



## Run BASIC

Run Basic supports all features of SQLite. 
This is a sample of a item master

```runbasic

#sql execute("
CREATE TABLE 	item (
itemNum		SMALLINT(4),
descr		VARCHAR(30),
short		VARCHAR(10),
cartSw		CHAR(1),
itemCat		CHAR(2),
itemType	VARCHAR(4),
itemDate        DATE,
uomId		VARCHAR(4),
decml		TINYINT(2),
onHand		FLOAT,
onOrder		DECIMAL(9,2),
eoq		INT(11),
weight		DECIMAL(8,1) ,
length		DECIMAL(8,1) ,
width		DECIMAL(8,1) ,
height		DECIMAL(8,1) ,
compNum		INT(10),
status		CHAR(1),
itemUrl		VARCHAR(200),
photoId		VARCHAR(40),
photoHigh	SMALLINT(4),
photoWide	SMALLINT(4),
specs		TEXT,
notes		TEXT,
treatUomId	VARCHAR(4) ,
errMinQty	FLOAT ,
errMaxQty	FLOAT ,
wrnMinQty	FLOAT ,
wrnMaxQty	FLOAT ,
labUomId	VARCHAR(4) ,
labErrMin	FLOAT ,
labErrMax	FLOAT ,
labWrnMin	FLOAT )
;
CREATE UNIQUE INDEX item_descr ON item( descr, itemNum);
CREATE UNIQUE INDEX item_itemNum ON item(itemNum);"

```



## Scala


### using SLICK FRM


```Scala
// Use H2Profile to connect to an H2 database
import slick.jdbc.H2Profile.api._

import scala.concurrent.ExecutionContext.Implicits.global

object Main extends App {
// Definition of the SUPPLIERS table
class Suppliers(tag: Tag) extends Table[(Int, String, String, String, String, String)](tag, "SUPPLIERS") {
  def id = column[Int]("SUP_ID", O.PrimaryKey) // This is the primary key column
  def name = column[String]("SUP_NAME")
  def street = column[String]("STREET")
  def city = column[String]("CITY")
  def state = column[String]("STATE")
  def zip = column[String]("ZIP")
  // Every table needs a * projection with the same type as the table's type parameter
  def * = (id, name, street, city, state, zip)
}
val suppliers = TableQuery[Suppliers]

// Definition of the COFFEES table
class Coffees(tag: Tag) extends Table[(String, Int, Double, Int, Int)](tag, "COFFEES") {
  def name = column[String]("COF_NAME", O.PrimaryKey)
  def supID = column[Int]("SUP_ID")
  def price = column[Double]("PRICE")
  def sales = column[Int]("SALES")
  def total = column[Int]("TOTAL")
  def * = (name, supID, price, sales, total)
  // A reified foreign key relation that can be navigated to create a join
  def supplier = foreignKey("SUP_FK", supID, suppliers)(_.id)
}
val coffees = TableQuery[Coffees]

val setup = DBIO.seq(
  // Create the tables, including primary and foreign keys
  (suppliers.schema ++ coffees.schema).create
)}
```


## SQL PL

{{works with|Db2 LUW}}

```sql pl

CREATE TABLE dept (
    deptno          NUMERIC(2)
        NOT NULL CONSTRAINT dept_pk PRIMARY KEY,
    dname           VARCHAR(14)
        NOT NULL CONSTRAINT dept_dname_uq UNIQUE,
    loc             VARCHAR(13)
);
CREATE TABLE emp (
    empno           NUMERIC(4)
        NOT NULL CONSTRAINT emp_pk PRIMARY KEY,
    ename           VARCHAR(10),
    job             VARCHAR(9),
    mgr             NUMERIC(4),
    hiredate        DATE,
    sal             DECIMAL(7,2)
        CONSTRAINT emp_sal_ck CHECK (sal > 0),
    comm            DECIMAL(7,2),
    deptno          NUMERIC(2)
        CONSTRAINT emp_ref_dept_fk
            REFERENCES dept(deptno)
);

INSERT INTO dept VALUES (10, 'ACCOUNTING', 'NEW YORK');
INSERT INTO dept VALUES (20, 'RESEARCH', 'DALLAS');

INSERT INTO emp VALUES (7369, 'SMITH', 'CLERK', 7902, '1980-12-17', 800, NULL, 20);
INSERT INTO emp VALUES (7566, 'JONES', 'MANAGER', 7839, '1981-04-02', 2975, NULL, 20);
INSERT INTO emp VALUES (7782, 'CLARK', 'MANAGER', 7839, '1981-06-09', 2450, NULL, 10);
INSERT INTO emp VALUES (7788, 'SCOTT', 'ANALYST', 7566, '1987-04-19', 3000, NULL, 20);
INSERT INTO emp VALUES (7839, 'KING', 'PRESIDENT', NULL, '1981-11-17', 5000, NULL, 10);
INSERT INTO emp VALUES (7876, 'ADAMS', 'CLERK', 7788, '1987-05-23', 1100, NULL, 20);
INSERT INTO emp VALUES (7902, 'FORD', 'ANALYST', 7566, '1981-12-03', 3000, NULL, 20);
INSERT INTO emp VALUES (7934, 'MILLER', 'CLERK', 7782, '1982-01-23', 1300, NULL, 10);

```

Output:

```txt

db2 => CREATE TABLE dept (
db2 (cont.) =>    deptno          NUMERIC(2)
db2 (cont.) =>        NOT NULL CONSTRAINT dept_pk PRIMARY KEY,
db2 (cont.) =>    dname           VARCHAR(14)
db2 (cont.) =>        NOT NULL CONSTRAINT dept_dname_uq UNIQUE,
db2 (cont.) =>    loc             VARCHAR(13)
db2 (cont.) =>);
DB20000I  The SQL command completed successfully.
db2 => CREATE TABLE emp (
db2 (cont.) =>    empno           NUMERIC(4)
db2 (cont.) =>        NOT NULL CONSTRAINT emp_pk PRIMARY KEY,
db2 (cont.) =>    ename           VARCHAR(10),
db2 (cont.) =>    job             VARCHAR(9),
db2 (cont.) =>    mgr             NUMERIC(4),
db2 (cont.) =>    hiredate        DATE,
db2 (cont.) =>    sal             DECIMAL(7,2)
db2 (cont.) =>        CONSTRAINT emp_sal_ck CHECK (sal > 0),
db2 (cont.) =>    comm            DECIMAL(7,2),
db2 (cont.) =>    deptno          NUMERIC(2)
db2 (cont.) =>        CONSTRAINT emp_ref_dept_fk
db2 (cont.) =>            REFERENCES dept(deptno)
db2 (cont.) =>);
DB20000I  The SQL command completed successfully.
db2 => INSERT INTO dept VALUES (10, 'ACCOUNTING', 'NEW YORK');
DB20000I  The SQL command completed successfully.
db2 => INSERT INTO dept VALUES (20, 'RESEARCH', 'DALLAS');
DB20000I  The SQL command completed successfully.
db2 => INSERT INTO emp VALUES (7369, 'SMITH', 'CLERK', 7902, '1980-12-17', 800, NULL, 20);
DB20000I  The SQL command completed successfully.
db2 => INSERT INTO emp VALUES (7566, 'JONES', 'MANAGER', 7839, '1981-04-02', 2975, NULL, 20);
DB20000I  The SQL command completed successfully.
db2 => INSERT INTO emp VALUES (7782, 'CLARK', 'MANAGER', 7839, '1981-06-09', 2450, NULL, 10);
DB20000I  The SQL command completed successfully.
db2 => INSERT INTO emp VALUES (7788, 'SCOTT', 'ANALYST', 7566, '1987-04-19', 3000, NULL, 20);
DB20000I  The SQL command completed successfully.
db2 => INSERT INTO emp VALUES (7839, 'KING', 'PRESIDENT', NULL, '1981-11-17', 5000, NULL, 10);
DB20000I  The SQL command completed successfully.
db2 => INSERT INTO emp VALUES (7876, 'ADAMS', 'CLERK', 7788, '1987-05-23', 1100, NULL, 20);
DB20000I  The SQL command completed successfully.
db2 => INSERT INTO emp VALUES (7902, 'FORD', 'ANALYST', 7566, '1981-12-03', 3000, NULL, 20);
DB20000I  The SQL command completed successfully.
db2 => INSERT INTO emp VALUES (7934, 'MILLER', 'CLERK', 7782, '1982-01-23', 1300, NULL, 10);
DB20000I  The SQL command completed successfully.
db2 => describe table dept;

                                Data type                     Column
Column name                     schema    Data type name      Length     Scale Nulls
------------------------------- --------- ------------------- ---------- ----- ------
DEPTNO                          SYSIBM    DECIMAL                      2     0 No    
DNAME                           SYSIBM    VARCHAR                     14     0 No    
LOC                             SYSIBM    VARCHAR                     13     0 Yes   

  3 record(s) selected.

db2 => describe table emp;

                                Data type                     Column
Column name                     schema    Data type name      Length     Scale Nulls
------------------------------- --------- ------------------- ---------- ----- ------
EMPNO                           SYSIBM    DECIMAL                      4     0 No    
ENAME                           SYSIBM    VARCHAR                     10     0 Yes   
JOB                             SYSIBM    VARCHAR                      9     0 Yes   
MGR                             SYSIBM    DECIMAL                      4     0 Yes   
HIREDATE                        SYSIBM    DATE                         4     0 Yes   
SAL                             SYSIBM    DECIMAL                      7     2 Yes   
COMM                            SYSIBM    DECIMAL                      7     2 Yes   
DEPTNO                          SYSIBM    DECIMAL                      2     0 Yes   

  8 record(s) selected.

db2 => select * from dept;

DEPTNO DNAME          LOC          
------ -------------- -------------
   10. ACCOUNTING     NEW YORK     
   20. RESEARCH       DALLAS       

  2 record(s) selected.

db2 => select * from emp;

EMPNO  ENAME      JOB       MGR    HIREDATE   SAL       COMM      DEPTNO
------ ---------- --------- ------ ---------- --------- --------- ------
 7369. SMITH      CLERK      7902. 12/17/1980    800.00         -    20.
 7566. JONES      MANAGER    7839. 04/02/1981   2975.00         -    20.
 7782. CLARK      MANAGER    7839. 06/09/1981   2450.00         -    10.
 7788. SCOTT      ANALYST    7566. 04/19/1987   3000.00         -    20.
 7839. KING       PRESIDENT      - 11/17/1981   5000.00         -    10.
 7876. ADAMS      CLERK      7788. 05/23/1987   1100.00         -    20.
 7902. FORD       ANALYST    7566. 12/03/1981   3000.00         -    20.
 7934. MILLER     CLERK      7782. 01/23/1982   1300.00         -    10.

  8 record(s) selected.

```



## Tcl

Tables, as used in relational databases, seem far away conceptually from Tcl. However, the following code demonstrates how a table (implemented as a list of lists, the first being the header line) can be type-checked and rendered:

```Tcl
proc table_update {_tbl row args} {
    upvar $_tbl tbl
    set heads [lindex $tbl 0]
    if {$row eq "end+1"} {
        lappend tbl [lrepeat [llength $heads] {}]
        set row [expr [llength $tbl]-1]
    }
    foreach {key val} $args {
        set col [lsearch $heads $key*]
        foreach {name type} [split [lindex $heads $col] |] break
        if {$type eq "float"} {set type double}
        if {$type eq "date"} {
            if [catch {clock scan $val}] {
                error "bad date value $val"
            }
        } elseif {$type ne ""} {
            if ![string is $type -strict $val] {
                error "bad $type value $val"
            }
        }
        lset tbl $row $col $val
    }
}
proc table_format table {
    set maxs {}
    foreach item [lindex $table 0] {
        set item [lindex [split $item |] 0]
        lappend maxs [string length $item]
    }
    foreach row [lrange $table 1 end] {
        set i 0
        foreach item $row max $maxs {
            if {[string length $item]>$max} {lset maxs $i [string length $item]}
            incr i
        }
    }
    set head +
    foreach max $maxs {append head -[string repeat - $max]-+}
    set res $head\n
    foreach row $table {
        if {$row eq [lindex $table 0]} {
            regsub -all {\|[^ ]+} $row "" row
        }
        append res |
        foreach item $row max $maxs {
             append res [format " %-${max}s |" $item]
        }
        append res \n
        if {$row eq [lindex $table 0]} {
            append res $head \n
        }
    }
    append res $head
}
#------------------------------------- Test and demo:
set mytbl [list [list \
                     account_id|int \
                     created|date  \
                     active|bool \
                     username \
                     balance|float \
                    ]]

table_update mytbl end+1 \
    account_id 12345 \
    username   "John Doe" \
    balance    0.0 \
    created    2009-05-13

table_update mytbl end+1 \
    account_id 12346 \
    username   "Jane Miller" \
    balance    0.0 \
    created    2009-05-14
puts [table_format $mytbl]
```

Output:

```txt

+------------+------------+--------+-------------+---------+
| account_id | created    | active | username    | balance |
+------------+------------+--------+-------------+---------+
| 12345      | 2009-05-13 |        | John Doe    | 0.0     |
| 12346      | 2009-05-14 |        | Jane Miller | 0.0     |
+------------+------------+--------+-------------+---------+

```

