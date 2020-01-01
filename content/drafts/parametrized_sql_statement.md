+++
title = "Parametrized SQL statement"
description = ""
date = 2019-10-16T09:46:09Z
aliases = []
[extra]
id = 4878
[taxonomies]
categories = []
tags = []
+++

{{task|Database operations}}Parameterized SQL statements are an easy way to avoid [[wp:SQL injection|SQL injection]] attacks. SQL drivers and libraries will automatically "sanitize" input to parameterized SQL statements to avoid these catastrophic database attacks. Second, parameterized SQL performs better. A lot better.

Using a SQL update statement like this one (spacing is optional):

```sql
UPDATE players
   SET name = 'Smith, Steve', score = 42, active = true
   WHERE jerseyNum = 99
```
show how to make a parameterized SQL statement, set the parameters to the values given above, and execute the statement.

<blockquote cite="http://blog.codinghorror.com/give-me-parameterized-sql-or-give-me-death/">Non-parameterized SQL is the GoTo statement of database programming. Don't do it, and make sure your coworkers don't either.</blockquote>


## 8th


```forth
\ assuming the var 'db' contains an opened database with a schema matching the problem:
db @
"UPDATE players SET name=?1,score=?2,active=?3 WHERE jerseyNum=?4"
db:prepare var, stmt

\ bind values to the statement:
stmt @ 1 "Smith, Steve" db:bind
       2 42 db:bind
       3 true db:bind
       4 99 db:bind

\ execute the query
db @  swap db:exec
```


## Ada


```Ada
-- Version for sqlite
with GNATCOLL.SQL_Impl;   use GNATCOLL.SQL_Impl;
with GNATCOLL.SQL.Exec;   use GNATCOLL.SQL.Exec;
with GNATCOLL.SQL.Sqlite; use GNATCOLL.SQL;

procedure Prepared_Query is

   DB_Descr : Database_Description;
   Conn     : Database_Connection;
   Query    : Prepared_Statement;
   --sqlite does not support boolean fields
   True_Str : aliased String          := "TRUE";
   Param    : SQL_Parameters (1 .. 4) :=
     (1 => (Parameter_Text, null),
      2 => (Parameter_Integer, 0),
      3 => (Parameter_Text, null),
      4 => (Parameter_Integer, 0));
begin
   -- Allocate and initialize the description of the connection
   Setup_Database (DB_Descr, "rosetta.db", "", "", "", DBMS_Sqlite);
   -- Allocate the connection
   Conn := Sqlite.Build_Sqlite_Connection (DB_Descr);
   -- Initialize the connection
   Reset_Connection (DB_Descr, Conn);
   Query :=
      Prepare
        ("UPDATE players SET name = ?, score = ?, active = ? " &
         " WHERE jerseyNum = ?");
   declare
      Name : aliased String := "Smith, Steve";
   begin
      Param := ("+" (Name'Access), "+" (42), "+" (True_Str'Access), "+" (99));
      Execute (Conn, Query, Param);
   end;
   Commit_Or_Rollback (Conn);
   Free (Conn);
   Free (DB_Descr);
end Prepared_Query;
```



## C

{{libheader|SQLite}}
{{trans|Pascal}}
Compile with:

  gcc example.c -lsqlite3

Tested with gcc version 4.9.2 (Raspbian 4.9.2-10) and SQLite 3.8.7.1

```c
#include <stdio.h>
#include <stdlib.h>
#include <sqlite3.h>

static const char* db_file = ":memory:"; // Create an in-memory database.

void check_error(int result_code, sqlite3 *db);
int select_callback(void* data, int column_count, char** columns, char** column_names);

int main(void) {
    sqlite3 *db;
    int result_code;
    char *sql;
    char *insert_statements[4];
    sqlite3_stmt *compiled_statement;
    int i;

    // Open the database.
    result_code = sqlite3_open(db_file, &db);
    check_error(result_code, db);

    // Create the players table in the database.
    sql = "create table players("
          "id integer primary key asc, "
          "name text, "
          "score real, "
          "active integer, " // Store the bool value as integer (see https://sqlite.org/datatype3.html chapter 2.1).
          "jerseyNum integer);";
    result_code = sqlite3_exec(db, sql, NULL, NULL, NULL);
    check_error(result_code, db);

    // Insert some values into the players table.
    insert_statements[0] = "insert into players (name, score, active, jerseyNum) "
                                        "values ('Roethlisberger, Ben', 94.1, 1, 7);";
    insert_statements[1] = "insert into players (name, score, active, jerseyNum) "
                                        "values ('Smith, Alex', 85.3, 1, 11);";
    insert_statements[2] = "insert into players (name, score, active, jerseyNum) "
                                        "values ('Manning, Payton', 96.5, 0, 18);";
    insert_statements[3] = "insert into players (name, score, active, jerseyNum) "
                                        "values ('Doe, John', 15, 0, 99);";

    for (i=0; i<4; i++) {
        result_code = sqlite3_exec(db, insert_statements[i], NULL, NULL, NULL);
        check_error(result_code, db);
    }

    // Display the contents of the players table.
    printf("Before update:\n");
    sql = "select * from players;";
    result_code = sqlite3_exec(db, sql, select_callback, NULL, NULL);
    check_error(result_code, db);

    // Prepare the parametrized SQL statement to update player #99.
    sql = "update players set name=?, score=?, active=? where jerseyNum=?;";
    result_code = sqlite3_prepare_v2(db, sql, -1, &compiled_statement, NULL);
    check_error(result_code, db);

    // Bind the values to the parameters (see https://sqlite.org/c3ref/bind_blob.html).
    result_code = sqlite3_bind_text(compiled_statement, 1, "Smith, Steve", -1, NULL);
    check_error(result_code, db);
    result_code = sqlite3_bind_double(compiled_statement, 2, 42);
    check_error(result_code, db);
    result_code = sqlite3_bind_int(compiled_statement, 3, 1);
    check_error(result_code, db);
    result_code = sqlite3_bind_int(compiled_statement, 4, 99);
    check_error(result_code, db);

    // Evaluate the prepared SQL statement.
    result_code = sqlite3_step(compiled_statement);
    if (result_code != SQLITE_DONE) {
        printf("Error #%d: %s\n", result_code, sqlite3_errmsg(db));
        sqlite3_close(db);
        return result_code;
    }

    // Destroy the prepared statement object.
    result_code = sqlite3_finalize(compiled_statement);
    check_error(result_code, db);

    // Display the contents of the players table.
    printf("After update:\n");
    sql = "select * from players;";
    result_code = sqlite3_exec(db, sql, select_callback, NULL, NULL);
    check_error(result_code, db);

    // Close the database connection.
    sqlite3_close(db);

    return EXIT_SUCCESS;
}

/*
  Checks the result code from an SQLite operation.
  If it contains an error code then this function prints the error message,
  closes the database and exits.
*/
void check_error(int result_code, sqlite3 *db) {
    if (result_code != SQLITE_OK) {
        printf("Error #%d: %s\n", result_code, sqlite3_errmsg(db));
        sqlite3_close(db);
        exit(result_code);
    }
}

/* This callback function prints the results of the select statement. */
int select_callback(void* data, int column_count, char** columns, char** column_names) {
    int i;

    for (i=0; i<column_count; i++) {
        printf(columns[i]);
        if (i < column_count-1) printf(" | ");
    }
    printf("\n");
}
```


{{out}}

```txt

Before update:
1 | Roethlisberger, Ben | 94.1 | 1 | 7
2 | Smith, Alex | 85.3 | 1 | 11
3 | Manning, Payton | 96.5 | 0 | 18
4 | Doe, John | 15.0 | 0 | 99
After update:
1 | Roethlisberger, Ben | 94.1 | 1 | 7
2 | Smith, Alex | 85.3 | 1 | 11
3 | Manning, Payton | 96.5 | 0 | 18
4 | Smith, Steve | 42.0 | 1 | 99

```


=={{header|C sharp|C#}}==

```csharp
using System.Data.Sql;
using System.Data.SqlClient;

namespace ConsoleApplication1
{
    class Program
    {
        static void Main(string[] args)
        {
            SqlConnection tConn = new SqlConnection("ConnectionString");

            SqlCommand tCommand = new SqlCommand();
            tCommand.Connection = tConn;
            tCommand.CommandText = "UPDATE players SET name = @name, score = @score, active = @active WHERE jerseyNum = @jerseyNum";

            tCommand.Parameters.Add(new SqlParameter("@name", System.Data.SqlDbType.VarChar).Value = "Smith, Steve");
            tCommand.Parameters.Add(new SqlParameter("@score", System.Data.SqlDbType.Int).Value = "42");
            tCommand.Parameters.Add(new SqlParameter("@active", System.Data.SqlDbType.Bit).Value = true);
            tCommand.Parameters.Add(new SqlParameter("@jerseyNum", System.Data.SqlDbType.Int).Value = "99");

            tCommand.ExecuteNonQuery();
        }
    }
}
```



## Clojure


```clojure
(require '[clojure.java.jdbc :as sql])
; Using h2database for this simple example.
(def db {:classname "org.h2.Driver"
         :subprotocol "h2:file"
         :subname "db/my-dbname"})

(sql/update! db :players {:name "Smith, Steve" :score 42 :active true} ["jerseyNum = ?" 99])

; As an alternative to update!, use execute!
(sql/execute! db ["UPDATE players SET name = ?, score = ?, active = ? WHERE jerseyNum = ?" "Smith, Steve" 42 true 99])
```


=={{header|F_Sharp|F#}}==
{{trans|C#}}

```fsharp
open System.Data.SqlClient

[<EntryPoint>]
let main argv =
    use tConn = new SqlConnection("ConnectionString")

    use tCommand = new SqlCommand()
    tCommand.Connection <- tConn
    tCommand.CommandText <- "UPDATE players SET name = @name, score = @score, active = @active WHERE jerseyNum = @jerseyNum"

    tCommand.Parameters.Add(SqlParameter("@name", System.Data.SqlDbType.VarChar).Value = box "Smith, Steve") |> ignore
    tCommand.Parameters.Add(SqlParameter("@score", System.Data.SqlDbType.Int).Value = box 42) |> ignore
    tCommand.Parameters.Add(SqlParameter("@active", System.Data.SqlDbType.Bit).Value = box true) |> ignore
    tCommand.Parameters.Add(SqlParameter("@jerseyNum", System.Data.SqlDbType.Int).Value = box 99) |> ignore

    tCommand.ExecuteNonQuery() |> ignore
    0
```



## Go


```go
package main

import (
    "database/sql"
    "fmt"

    _ "github.com/mattn/go-sqlite3"
)

func main() {
    db, _ := sql.Open("sqlite3", "rc.db")
    defer db.Close()
    db.Exec(`create table players (name, score, active, jerseyNum)`)
    db.Exec(`insert into players values ("",0,0,"99")`)
    db.Exec(`insert into players values ("",0,0,"100")`)

    // Parameterized
    db.Exec(`update players set name=?, score=?, active=? where jerseyNum=?`,
        "Smith, Steve", 42, true, "99")

    rows, _ := db.Query("select * from players")
    var (
        name      string
        score     int
        active    bool
        jerseyNum string
    )
    for rows.Next() {
        rows.Scan(&name, &score, &active, &jerseyNum)
        fmt.Printf("%3s %12s %3d %t\n", jerseyNum, name, score, active)
    }
    rows.Close()
}
```

{{out}}

```txt

 99 Smith, Steve  42 true
100                0 false

```



## Haskell


Example uses the [http://hackage.haskell.org/package/HDBC <tt>HDBC</tt>] package:


```haskell
module Main (main) where

import           Database.HDBC (IConnection, commit, run, toSql)

updatePlayers :: IConnection a => a -> String -> Int -> Bool -> Int -> IO Bool
updatePlayers conn name score active jerseyNum = do
    rowCount <- run conn
        "UPDATE players\
        \ SET name = ?, score = ?, active = ?\
        \ WHERE jerseyNum = ?"
        [ toSql name
        , toSql score
        , toSql active
        , toSql jerseyNum
        ]
    commit conn
    return $ rowCount == 1

main :: IO ()
main = undefined
```


You'll need an instance of a type with an instance for the <tt>IConnection</tt> type class in order to use this function, such as [http://hackage.haskell.org/package/HDBC-postgresql-2.3.2.5/docs/Database-HDBC-PostgreSQL.html#t:Connection <tt>Connection</tt>] from [http://hackage.haskell.org/package/HDBC-postgresql <tt>HDBC-postgresql</tt>].


## Huginn


```huginn
import Database as db;
import Algorithms as algo;
import FileSystem as fs;

main() {
  dbPath = "/tmp/parametrized-sql.sqlite";
  fs.remove( dbPath );
  fs.open( dbPath, fs.OPEN_MODE.WRITE );
  conn = db.connect( "sqlite3:///" + dbPath );

  // Setup...
  conn.query(
    "CREATE TABLE Players (\n"
    "\tname VARCHAR(64),\n"
    "\tscore FLOAT,\n"
    "\tactive INTEGER,\n"
    "\tno VARCHAR(8)\n"
    ");"
  ).execute();
  conn.query(
    "INSERT INTO Players VALUES ( 'name', 0, 'false', 99 );"
  ).execute();
  conn.query(
    "INSERT INTO Players VALUES ( 'name', 0, 'false', 100 );"
  ).execute();

  // Demonstrate parameterized SQL...
  parametrizedQuery = conn.query(
     "UPDATE Players SET name=?, score=?, active=? WHERE no=?"
  );
  for ( i, v : algo.enumerate( ( "Smith, Steve", 42, true, 99 ) ) ) {
    parametrizedQuery.bind( i + 1, string( v ) );
  }
  parametrizedQuery.execute();

  // and show the results...
  for ( record : conn.query( "SELECT * FROM Players;" ).execute() ) {
    print( "{}\n".format( record ) );
  }
  return ( 0 );
}
```



## Java


```java

import java.sql.DriverManager;
import java.sql.Connection;
import java.sql.PreparedStatement;

public class DBDemo{
   private String protocol; //set this to some connection protocol like "jdbc:sqlserver://"
   private String dbName;   //set this to the name of your database
   private String username;
   private String password;

   PreparedStatement query;

   public int setUpAndExecPS(){
      try {
         Connection conn = DriverManager.getConnection(protocol + dbName, username, password);

         query = conn.prepareStatement(
            "UPDATE players SET name = ?, score = ?, active = ? WHERE jerseyNum = ?");

         query.setString(1, "Smith, Steve");//automatically sanitizes and adds quotes
         query.setInt(2, 42);
         query.setBoolean(3, true);
         query.setInt(4, 99);
         //there are similar methods for other SQL types in PerparedStatement
         return query.executeUpdate();//returns the number of rows changed
         //PreparedStatement.executeQuery() will return a java.sql.ResultSet,
         //execute() will simply return a boolean saying whether it succeeded or not

      } catch (Exception e) {
         e.printStackTrace();
      }

      return 0;
   }
}

```



## Julia

{{works with|Julia|0.6}}

Uses the SQLite package.

```julia
using SQLite

name = "Smith, Steve"
jerseys =  Dict("Smith, Steve" => 99)
sqlbool(tf::Bool) = if(tf) "TRUE" else "FALSE" end

db = SQLite.DB() # no filename given, so create an in-memory temporary
SQLite.execute!(db, "create table players (id integer primary key,
                                           name text,
                                           score number,
                                           active bool,
                                           jerseynum integer)")

SQLite.query(db, "INSERT INTO players (name, score, active, jerseynum) values ('Jones, James', 9, 'FALSE', 99)")
SQLite.query(db, "UPDATE players SET name = ?, score = ?, active = ? WHERE jerseynum = ?";
    values = ["Smith, Steve", 42, sqlbool(true), jerseys[name]])

tbl = SQLite.query(db, "SELECT * from players")
println(tbl)
```



{{output}}
```txt

1×5 DataFrames.DataFrame
│ Row │ id │ name           │ score │ active │ jerseynum │
├─────┼────┼────────────────┼───────┼────────┼───────────┤
│ 1   │ 1  │ "Smith, Steve" │ 42    │ "TRUE" │ 99        │

```



## Kotlin


```scala
// Version 1.2.41

import java.sql.DriverManager
import java.sql.Connection

fun main(args: Array<String>) {
    val url = "jdbc:mysql://localhost:3306/test"
    val username = "example"
    val password = "password123"
    val conn = DriverManager.getConnection(url, username, password)
    val query = conn.prepareStatement(
        "UPDATE players SET name = ?, score = ?, active = ? WHERE jerseyNum = ?"
    )
    with (query) {
        setString(1, "Smith, Steve")
        setInt(2, 42)
        setBoolean(3, true)
        setInt(4, 99)
        val rowCount = executeUpdate()
        if (rowCount == 0) println("Update failed")
        close()
    }
    conn.close()
}
```



## M2000 Interpreter


```M2000 Interpreter

Module Parametrized_Sql {
	Base "rosetta"  ' warning erase database if found it in current directory
	Execute "rosetta", {create table players (name VarChar(64), score Float, active Integer, jerseyNum Integer);}
	Append "rosetta", "players","name",0,FALSE,99
	sql$={
	UPDATE players
	   SET name = '{0}', score = {1}, active = {2}
	   WHERE jerseyNum = {3}
	}
	Execute "rosetta", format$(sql$,"Smith, Steve", 42,TRUE, 99)
	Retrieve "rosetta","players",1,"jerseyNum",99
	Read how_many
	Read Name$,score, active,jerseynum
	Print Name$="Smith, Steve", score=42, active=True, jerseynum=99  ' true true true true
}
Parametrized_Sql

```




## Mathematica

{{incorrect|Mathematica|Executing a NON-parameterized update DML. This solution is exactly the opposite of the task. This example is what is explicitly warned in the task.}}

```Mathematica
Needs["DatabaseLink`"];
conn=OpenSQLConnection[JDBC["ODBC(DSN)", "testdb"], "Username" -> "John", "Password" -> "JohnsPassword"];
SQLUpdate[conn,"players",{"name","score","active"},{"Smith, Steve", 42,"TRUE"},SQLColumn["jerseyNum"] = 99];
CloseSQLConnection[conn];
```



## NetRexx

Using an [http://db.apache.org/derby/ Apache Derby] embedded database:

```NetRexx
/* NetRexx */
options replace format comments java crossref symbols nobinary

import java.sql.

--
### =======================================================================

class RParameterizedSQLSimple public

  properties indirect
    connexion = Connection

  properties inheritable constant
    DRIVER = "org.apache.derby.jdbc.EmbeddedDriver"
    DBURL = "jdbc:derby:"
    DBNAME = "/workspace/DB.DerbySamples/DB/TEAMS01"
    DBMODE_CREATE   = ";create=true"
    DBMODE_NOCREATE = ";create=false"
    DBMODE_SHUTDOWN = ";shutdown=true"

--
### =======================================================================

method RParameterizedSQLSimple()
  setConnexion(null)
  return

--
### =======================================================================

method createConnexion() inheritable returns Connection signals ClassNotFoundException, InstantiationException, IllegalAccessException
  if getConnexion() = null then do
    props = Properties()
    props.put("user", "user1")
    props.put("password", "user1")

    xURL = String DBURL || DBNAME || DBMODE_CREATE
    loadDriver(DRIVER)
    setConnexion(DriverManager.getConnection(xURL, props))
    end

  return getConnexion()

--
### =======================================================================

method shutdownConnexion() inheritable returns boolean signals SQLException

  dbState = boolean
  xURL = String DBURL || DBNAME || DBMODE_SHUTDOWN

  do
    DriverManager.getConnection(xURL)
    dbState = isTrue

  catch se = SQLException
    if (se.getErrorCode() = 50000) & ("XJ015".equals(se.getSQLState())) then do
      say "Derby shut down normally"
      dbState = isTrue
      end
    else if (se.getErrorCode() = 45000) & ("08006".equals(se.getSQLState())) then do
      say "Derby database shut down normally"
      dbState = isTrue
      end
    else do
      say "Derby did not shut down normally"
      dbState = isFalse
      signal se
      end
  end

  return dbState

--
### =======================================================================

method loadDriver(xdriver = String) inheritable static signals ClassNotFoundException, InstantiationException, IllegalAccessException

  do
    Class.forName(xdriver).newInstance()
    say "Loaded the appropriate driver"

  catch cnfe = ClassNotFoundException
    say "Unable to load the JDBC driver" xdriver
    say "Please check your CLASSPATH."
    signal cnfe

  catch ie = InstantiationException
    say "Unable to instantiate the JDBC driver" xdriver
    signal ie

  catch iae = IllegalAccessException
    say "Not allowed to access the JDBC driver" xdriver
    signal iae

  end

  return

--
### =======================================================================

method updatePlayer(jerseyNum = int, name = String, score = int, active = boolean) binary inheritable returns int signals SQLException

  updateSQL = "" -
    || "UPDATE TEAM.PLAYERS" -
    || "  SET NAME = ?, SCORE = ?, ACTIVE = ?" -
    || "  WHERE JERSEYNUM = ?"

  rowCt = int
  ix = int 0

  ps = getConnexion().prepareStatement(updateSQL)
  ix = ix + 1; ps.setString(ix, name)
  ix = ix + 1; ps.setInt(ix, score)
  ix = ix + 1; ps.setBoolean(ix, active)
  ix = ix + 1; ps.setInt(ix, jerseyNum)
  rowCt = ps.executeUpdate()

  return rowCt

--
### =======================================================================

method main(args = String[]) public static

  do
    tda = RParameterizedSQLSimple()
    tda.createConnexion()

    if tda.getConnexion() \= null then do
        updated = tda.updatePlayer(99, "Smith, Steve", 42, isTrue)
        if updated > 0 then say "Update successful"
        else say "Update failed"

      finally
        tda.shutdownConnexion()
      end

  catch ex = Exception
    ex.printStackTrace
  end

  return

--
### =======================================================================

method isTrue() public static returns boolean
  return 1 == 1

--
### =======================================================================

method isFalse() public static returns boolean
  return \isTrue

```



## Objeck


```objeck
use IO;
use ODBC;

bundle Default {
   class Sql {
      function : Main(args : String[]) ~ Nil {
      conn := Connection->New("ds", "user", "password");
      if(conn <> Nil) {
         sql := "UPDATE players SET name = ?, score = ?, active = ? WHERE jerseyNum = ?";
         pstmt := conn->CreateParameterStatement(sql);
         pstmt->SetVarchar(1, "Smith, Steve");
         pstmt->SetInt(2, 42);
         pstmt->SetBit(3, true);
         pstmt->SetInt(4, 99);
         pstmt->Update()->PrintLine();
         conn->Close();
      };
   }
}
```



## Pascal

{{works with|Free_Pascal}}
{{libheader|SQLite}}
Tested with Free Pascal 2.6.4 (arm) and SQLite 3.8.7.1

```pascal
program Parametrized_SQL_Statement;
uses
  sqlite3, sysutils;

const
  DB_FILE : PChar = ':memory:'; // Create an in-memory database.

var
  DB                :Psqlite3;
  ResultCode        :Integer;
  SQL               :PChar;
  InsertStatements  :array [1..4] of PChar;
  CompiledStatement :Psqlite3_stmt;
  i                 :integer;

{ CheckError

  Checks the result code from an SQLite operation.
  If it contains an error code then this procedure prints the error message,
  closes the database and halts the program. }

procedure CheckError(ResultCode: integer; DB: Psqlite3);
begin
  if ResultCode <> SQLITE_OK then
  begin
    writeln(format('Error #%d: %s', [ResultCode, sqlite3_errmsg(db)]));
    sqlite3_close(DB);
    halt(ResultCode);
  end;
end;

{ SelectCallback

  This callback function prints the results of the select statement.}

function SelectCallback(Data: pointer; ColumnCount: longint; Columns: PPChar; ColumnNames: PPChar):longint; cdecl;
var
  i   :longint;
  col :PPChar;
begin
  col := Columns;
  for i:=0 to ColumnCount-1 do
  begin
    write(col^); // Print the current column value.
    inc(col);    // Advance the pointer.
    if i<>ColumnCount-1 then write(' | ');
  end;
  writeln;
end;

begin
  // Open the database.
  ResultCode := sqlite3_open(DB_FILE, @DB);
  CheckError(ResultCode, DB);

  // Create the players table in the database.
  SQL := 'create table players(' +
         'id integer primary key asc, ' +
         'name text, ' +
         'score real, ' +
         'active integer, ' + // Store the bool value as integer (see https://sqlite.org/datatype3.html chapter 2.1).
         'jerseyNum integer);';
  ResultCode := sqlite3_exec(DB, SQL, nil, nil, nil);
  CheckError(ResultCode, DB);

  // Insert some values into the players table.
  InsertStatements[1] := 'insert into players (name, score, active, jerseyNum) ' +
                                      'values (''Roethlisberger, Ben'', 94.1, 1, 7);';
  InsertStatements[2] := 'insert into players (name, score, active, jerseyNum) ' +
                                      'values (''Smith, Alex'', 85.3, 1, 11);';
  InsertStatements[3] := 'insert into players (name, score, active, jerseyNum) ' +
                                      'values (''Manning, Payton'', 96.5, 0, 18);';
  InsertStatements[4] := 'insert into players (name, score, active, jerseyNum) ' +
                                      'values (''Doe, John'', 15, 0, 99);';

  for i:=1 to 4 do
  begin
    ResultCode := sqlite3_exec(DB, InsertStatements[i], nil, nil, nil);
    CheckError(ResultCode, DB);
  end;

  // Display the contents of the players table.
  writeln('Before update:');
  SQL := 'select * from players;';
  ResultCode := sqlite3_exec(DB, SQL, @SelectCallback, nil, nil);
  CheckError(ResultCode, DB);

  // Prepare the parametrized SQL statement to update player #99.
  SQL := 'update players set name=?, score=?, active=? where jerseyNum=?;';
  ResultCode := sqlite3_prepare_v2(DB, SQL, -1, @CompiledStatement, nil);
  CheckError(ResultCode, DB);

  // Bind the values to the parameters (see https://sqlite.org/c3ref/bind_blob.html).
  ResultCode := sqlite3_bind_text(CompiledStatement, 1, PChar('Smith, Steve'), -1, nil);
  CheckError(ResultCode, DB);
  ResultCode := sqlite3_bind_double(CompiledStatement, 2, 42);
  CheckError(ResultCode, DB);
  ResultCode := sqlite3_bind_int(CompiledStatement, 3, 1);
  CheckError(ResultCode, DB);
  ResultCode := sqlite3_bind_int(CompiledStatement, 4, 99);
  CheckError(ResultCode, DB);

  // Evaluate the prepared SQL statement.
  ResultCode := sqlite3_step(CompiledStatement);
  if ResultCode <> SQLITE_DONE then
  begin
    writeln(format('Error #%d: %s', [ResultCode, sqlite3_errmsg(db)]));
    sqlite3_close(DB);
    halt(ResultCode);
  end;

  // Destroy the prepared statement object.
  ResultCode := sqlite3_finalize(CompiledStatement);
  CheckError(ResultCode, DB);

  // Display the contents of the players table.
  writeln('After update:');
  SQL := 'select * from players;';
  ResultCode := sqlite3_exec(DB, SQL, @SelectCallback, nil, nil);
  CheckError(ResultCode, DB);

  // Close the database connection.
  sqlite3_close(db);
end.
```

{{out}}

```txt

Before update:
1 | Roethlisberger, Ben | 94.1 | 1 | 7
2 | Smith, Alex | 85.3 | 1 | 11
3 | Manning, Payton | 96.5 | 0 | 18
4 | Doe, John | 15.0 | 0 | 99
After update:
1 | Roethlisberger, Ben | 94.1 | 1 | 7
2 | Smith, Alex | 85.3 | 1 | 11
3 | Manning, Payton | 96.5 | 0 | 18
4 | Smith, Steve | 42.0 | 1 | 99

```



## Perl


```perl
use DBI;

my $db = DBI->connect('DBI:mysql:mydatabase:host','login','password');

$statment = $db->prepare("UPDATE players SET name = ?, score = ?, active = ? WHERE jerseyNum = ?");

$rows_affected = $statment->execute("Smith, Steve",42,'true',99);
```


## Perl 6


```perl6
use DBIish;

my $db = DBIish.connect('DBI:mysql:mydatabase:host','login','password');

my $update = $db.prepare("UPDATE players SET name = ?, score = ?, active = ? WHERE jerseyNum = ?");

my $rows-affected = $update.execute("Smith, Steve",42,'true',99);
```



## Phix

{{libheader|SQLite}}

```Phix
--
-- demo\rosetta\Parametrized_SQL_statement.exw
--
include pSQLite.e

sqlite3 db = sqlite3_open(":memory:")

integer res = sqlite3_exec(db,`create table players (name, score, active, jerseyNum)`)
res = sqlite3_exec(db,`insert into players values ('Roethlisberger, Ben', 94.1, 1, 7 )`)
res = sqlite3_exec(db,`insert into players values ('Smith, Alex',         85.3, 1, 11)`)
res = sqlite3_exec(db,`insert into players values ('Doe, John',             15, 0, 99)`)
res = sqlite3_exec(db,`insert into players values ('Manning, Payton',     96.5, 0, 123)`)

pp({"Before",sqlite3_get_table(db, "select * from players")},{pp_Nest,2})

sqlite3_stmt pStmt = sqlite3_prepare(db, `update players set name=?, score=?, active=? where jerseyNum=?`)
sqlite3_bind_text(pStmt,1,"Smith, Steve")
sqlite3_bind_double(pStmt,2,42)
sqlite3_bind_int(pStmt,3,true)
sqlite3_bind_int(pStmt,4,99)
res = sqlite3_step(pStmt);
if res!=SQLITE_DONE then ?9/0 end if
if sqlite3_finalize(pStmt)!=SQLITE_OK then ?9/0 end if

pp({"After",sqlite3_get_table(db, "select * from players")},{pp_Nest,2})

sqlite3_close(db)
```

<small>(The distributed version of this code displays nicer formatted output, but is nearly twice as long.)</small>
{{out}}

```txt

{"Before",
 {{"name", "score", "active", "jerseyNum"},
  {"Roethlisberger, Ben", "94.1", "1", "7"},
  {"Smith, Alex", "85.3", "1", "11"},
  {"Doe, John", "15", "0", "99"},
  {"Manning, Payton", "96.5", "0", "123"}}}
{"After",
 {{"name", "score", "active", "jerseyNum"},
  {"Roethlisberger, Ben", "94.1", "1", "7"},
  {"Smith, Alex", "85.3", "1", "11"},
  {"Smith, Steve", "42.0", "1", "99"},
  {"Manning, Payton", "96.5", "0", "123"}}}

```



## PHP


```php
$updatePlayers = "UPDATE `players` SET `name` = ?, `score` = ?, `active` = ?\n".
		"WHERE `jerseyNum` = ?";
$dbh = new PDO( "mysql:dbname=db;host=localhost", "username", "password" );

$updateStatement = $dbh->prepare( $updatePlayers );

$updateStatement->bindValue( 1, "Smith, Steve", PDO::PARAM_STR );
$updateStatement->bindValue( 2, 42, PDO::PARAM_INT );
$updateStatement->bindValue( 3, 1, PDO::PARAM_INT );
$updateStatement->bindValue( 4, 99, PDO::PARAM_INT );

$updateStatement->execute();

// alternatively pass parameters as an array to the execute method
$updateStatement = $dbh->prepare( $updatePlayers );
$updateStatement->execute( array( "Smith, Steve", 42, 1, 99 ) );
```



## PicoLisp

As PicoLisp uses normal function calls for DB manipulations, parameters are always treated as plain data and are not executed.

```PicoLisp
(for P (collect 'jerseyNum '+Players 99)
   (put!> P 'name "Smith, Steve")
   (put!> P 'score 42)
   (put!> P 'active T) )
```


## PureBasic


```PureBasic
UseSQLiteDatabase()

Procedure CheckDatabaseUpdate(database, query$)
  result = DatabaseUpdate(database, query$)
  If result = 0
    PrintN(DatabaseError())
  EndIf

  ProcedureReturn result
EndProcedure


If OpenConsole()
  If OpenDatabase(0, ":memory:", "", "")
    ;create players table with sample data
    CheckDatabaseUpdate(0, "CREATE table players (name, score, active, jerseyNum)")
    CheckDatabaseUpdate(0, "INSERT INTO players VALUES ('Jones, Bob',0,'N',99)")
    CheckDatabaseUpdate(0, "INSERT INTO players VALUES ('Jesten, Jim',0,'N',100)")
    CheckDatabaseUpdate(0, "INSERT INTO players VALUES ('Jello, Frank',0,'N',101)")

    Define name$, score, active$, jerseynum
    name$ = "Smith, Steve"
    score = 42
    active$ ="TRUE"
    jerseynum = 99
    SetDatabaseString(0, 0, name$)
    SetDatabaseLong(0, 1, score)
    SetDatabaseString(0, 2, active$)
    SetDatabaseLong(0, 3, jerseynum)
    CheckDatabaseUpdate(0, "UPDATE players SET name = ?, score = ?, active = ? WHERE jerseyNum = ?")

    ;display database contents
    If DatabaseQuery(0, "Select * from players")
      While NextDatabaseRow(0)
        name$ = GetDatabaseString(0, 0)
        score = GetDatabaseLong(0, 1)
        active$ = GetDatabaseString(0, 2)
        jerseynum = GetDatabaseLong(0, 3)
        row$ = "['" + name$ + "', " + score + ", '" + active$ + "', " + jerseynum + "]"
        PrintN(row$)
      Wend

      FinishDatabaseQuery(0)
    EndIf

    CloseDatabase(0)
  Else
    PrintN("Can't open database !")
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
```


Sample output:

```txt
['Smith, Steve', 42, 'TRUE', 99]
['Jesten, Jim', 0, 'N', 100]
['Jello, Frank', 0, 'N', 101]
```



## Python

{{trans|Ruby}}

```python
import sqlite3

db = sqlite3.connect(':memory:')

# setup
db.execute('create temp table players (name, score, active, jerseyNum)')
db.execute('insert into players values ("name",0,"false",99)')
db.execute('insert into players values ("name",0,"false",100)')

# demonstrate parameterized SQL

# example 1 -- simple placeholders
db.execute('update players set name=?, score=?, active=? where jerseyNum=?', ('Smith, Steve', 42, True, 99))

# example 2 -- named placeholders
db.execute('update players set name=:name, score=:score, active=:active where jerseyNum=:num',
    {'num': 100,
     'name': 'John Doe',
     'active': False,
     'score': -1}
)

# and show the results
for row in db.execute('select * from players'):
   print(row)
```

outputs

```txt
(u'Smith, Steve', 42, 1, 99)
(u'John Doe', -1, 0, 100)
```



## Racket

{{works with|PostgreSQL}}
{{libheader|sql db-lib}}

```racket

#lang racket/base
(require sql db)

(define pgc
  ; Don't actually inline sensitive data ;)
  (postgresql-connect #:user     "resu"
                      #:database "esabatad"
                      #:server   "example.com"
                      #:port     5432
                      #:password "s3>r37P455"))

(define update-player
  (parameterize ((current-sql-dialect 'postgresql))
                (update players
                        #:set [name ?] [score ?] [active ?]
                        #:where [jerseyNum ?])))

(apply query
       pgc
       update-player
       '("Smith, Steve" 42 #t 99))


```



## Ruby

Using the {{libheader|sqlite3-ruby}} gem
[[Category:SQLite]]

```ruby
require 'sqlite3'

db = SQLite3::Database.new(":memory:")

# setup
db.execute('create temp table players (name, score, active, jerseyNum)')
db.execute('insert into players values ("name",0,"false",99)')
db.execute('insert into players values ("name",0,"false",100)')
db.execute('insert into players values ("name",0,"false",101)')

# demonstrate parameterized SQL

# example 1 -- simple placeholders
db.execute('update players set name=?, score=?, active=? where jerseyNum=?', 'Smith, Steve', 42, true, 99)

# example 2 -- named placeholders
db.execute('update players set name=:name, score=:score, active=:active where jerseyNum=:num',
    :num => 100,
    :name => 'John Doe',
    :active => false,
    :score => -1
)

# example 3 -- numbered placeholders
stmt = db.prepare('update players set name=?4, score=?3, active=?2 where jerseyNum=?1')
stmt.bind_param(1, 101)
stmt.bind_param(2, true)
stmt.bind_param(3, 3)
stmt.bind_param(4, "Robert'; DROP TABLE players--")
stmt.execute

# and show the results
db.execute2('select * from players') {|row| p row}
```

outputs

```txt
["name", "score", "active", "jerseyNum"]
["Smith, Steve", "42", "true", "99"]
["John Doe", "-1", "false", "100"]
["Robert'; DROP TABLE players--", "3", "true", "101"]
```



## Run BASIC

{{incorrect|Run BASIC|Executing a NON-parameterized update DML. This solution is exactly the opposite of the task. This example is what is explicitly warned in the task.}}

```runbasic
sqliteconnect #mem, ":memory:"
#mem execute("CREATE table players (name, score, active, jerseyNum)")
#mem execute("INSERT INTO players VALUES ('Jones, Bob',0,'N',99)")
#mem execute("INSERT INTO players VALUES ('Jesten, Jim',0,'N',100)")
#mem execute("INSERT INTO players VALUES ('Jello, Frank',0,'N',101)")
sql$ = "
UPDATE players
   SET name = 'Smith, Steve',
   score   = 42,
   active  = 'TRUE'
   WHERE jerseyNum = 99"
#mem execute(sql$)
#mem execute("SELECT * FROM players ORDER BY jerseyNum")
WHILE  #mem hasanswer()
	#row      = #mem #nextrow()
	name$     = #row name$()
	score     = #row score()
	active$   = #row active$()
	jerseyNum = #row jerseyNum()

print name$;chr$(9);score;chr$(9);active$;chr$(9);jerseyNum
WEND
end
```


```txt
Output
Smith, Steve	42	TRUE	99
Jesten, Jim	0	N	100
Jello, Frank	0	N	101
```



## Scala

===Using [http://slick.lightbend.com/doc/3.2.3/introduction.html Slick] FRM===
{{Out}}Best seen running in your browser [https://scastie.scala-lang.org/fJKRDaydSsGGlZQXJUhvxw Scastie (remote JVM)].

```Scala
import slick.jdbc.H2Profile.api._
import slick.sql.SqlProfile.ColumnOption.SqlType

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object PlayersApp extends App {
  lazy val playerRecords = TableQuery[PlayerRecords]
  val db = Database.forURL("jdbc:h2:mem:test1;DB_CLOSE_DELAY=-1", driver = "org.h2.Driver")
  //  Pre-compiled parameterized statement
  val compiledUpdate = Compiled { jerseyN: Rep[Int] =>
    for {c <- playerRecords if c.jerseyNum === jerseyN} yield (c.name, c.score, c.active)
  }

  def setup = DBIO.seq(
    playerRecords.schema.create,
    playerRecords ++= Seq( // JDBC batch update
                          (7, "Roethlisberger, Ben", 94.1f, true),
                          (11, "Smith, Alex", 85.3f, true),
                          (18, "Manning, Payton", 96.5f, false),
                          (99, "Doe, John", 15f, false))
  )

  def queryPlayers(prelude: String) = {
    println("\n  " +prelude)
    println(
      "│ Name                          │Scor│ Active │Jerseynum│\n" +
      "├───────────────────────────────┼────┼────────┼─────────┤"
    )
    DBIO.seq(playerRecords.result.map(_.map {
      case (jerseyN, name, score, active) =>
        f"$name%32s $score ${(if (active) "" else "in") + "active"}%8s $jerseyN%8d"
    }.foreach(println)))
  }

  // Definition of the PLAYERS table
  class PlayerRecords(tag: Tag) extends Table[(Int, String, Float, Boolean)](tag, "PLAYER_RECORDS") {
    def active = column[Boolean]("ACTIVE")
    def jerseyNum = column[Int]("JERSEY_NUM", O.PrimaryKey)
    def name = column[String]("NAME", SqlType("VARCHAR2(32)"))
    def score = column[Float]("SCORE")

    def * = (jerseyNum, name, score, active)
  }

  println(s"The pre-compiled parameterized update DML:\n${compiledUpdate(0).updateStatement}")

  Await.result(db.run(
    for { // Using the for comprehension
    _ <- setup
    _ <- queryPlayers("Before update:")
    n <- compiledUpdate(99).update("Smith, Steve", 42f, true)
    _ <- queryPlayers("After update:")
  } yield n), Duration.Inf)

}
```


## Seed7

The library [http://seed7.sourceforge.net/libraries/sql_base.htm sql_base.s7i] provides access to databases.
The type [http://seed7.sourceforge.net/libraries/sql_base.htm#database database] describes a database connection
and the type [http://seed7.sourceforge.net/libraries/sql_base.htm#sqlStatement sqlStatement] can store a prepared statement.
In the example below the table ''players'' is created and filled with hard coded SQL statements, that are ''execute''d without parametrization.
The SQL statement to update the table uses parametrization.
The SQL statement is [http://seed7.sourceforge.net/libraries/sql_base.htm#prepare%28in_database,in_string%29 prepared],
parameters are [http://seed7.sourceforge.net/libraries/sql_base.htm#bind%28inout_sqlStatement,in_integer,in_integer%29 bound] and
the statement is [http://seed7.sourceforge.net/libraries/sql_base.htm#execute%28inout_sqlStatement%29 executed].
Finally a SQL select statement is prepared, executed and the result rows are [http://seed7.sourceforge.net/libraries/sql_base.htm#fetch%28in_sqlStatement%29 fetched].
A column from a result row is retrieved with the function [http://seed7.sourceforge.net/libraries/sql_base.htm#column%28in_sqlStatement,in_integer,attr_integer%29 column].


```seed7
$ include "seed7_05.s7i";
  include "sql_base.s7i";

const proc: main is func
  local
    var database: testDb is database.value;
    var sqlStatement: statement is sqlStatement.value;
    var string: name is "Smith, Steve";
  begin
    testDb := openDatabase(DB_SQLITE, "test", "test", "test");
    execute(testDb, "create table players (name CHAR(32), score INTEGER, active CHAR, jerseyNum INTEGER)");
    execute(testDb, "insert into players values ('Jones, Bob',0,0,99)");
    execute(testDb, "insert into players values ('Jesten, Jim',0,0,100)");
    execute(testDb, "insert into players values ('Jello, Frank',0,0,101)");
    statement := prepare(testDb, "update players set name = ?, score = ?, active = ? \
                                 \where jerseyNum = ?");
    bind(statement, 1, name);
    bind(statement, 2, 42);
    bind(statement, 3, TRUE);
    bind(statement, 4, 99);
    execute(statement);
    statement := prepare(testDb, "select * from players");
    execute(statement);
    while fetch(statement) do
      writeln(column(statement, 1, string) <& " " <&
              column(statement, 2, integer) <& " " <&
              column(statement, 3, boolean) <& " " <&
              column(statement, 4, integer));
    end while;
    execute(testDb, "drop table players");
    close(testDb);
  end func;
```


{{out}}

```txt
Smith, Steve 42 TRUE 99
Jesten, Jim 0 FALSE 100
Jello, Frank 0 FALSE 101
```



## SQL

{{works with|Oracle}}

```sql
-- This works in Oracle's SQL*Plus command line utility

VARIABLE P_NAME VARCHAR2(20);
VARIABLE P_SCORE NUMBER;
VARIABLE P_ACTIVE VARCHAR2(5);
VARIABLE P_JERSEYNUM NUMBER;

begin

:P_NAME := 'Smith, Steve';
:P_SCORE := 42;
:P_ACTIVE := 'TRUE';
:P_JERSEYNUM := 99;

end;
/

drop table players;

create table players
(
NAME VARCHAR2(20),
SCORE NUMBER,
ACTIVE VARCHAR2(5),
JERSEYNUM NUMBER
);

insert into players values ('No name',0,'FALSE',99);

commit;

select * from players;

UPDATE players
   SET name = :P_NAME, score = :P_SCORE, active = :P_ACTIVE
   WHERE jerseyNum = :P_JERSEYNUM;

commit;

select * from players;
```

{{Out}}

```txt
SQL> SQL>
NAME                      SCORE ACTIV  JERSEYNUM
-------------------- ---------- ----- ----------
No name                       0 FALSE         99

SQL> SQL>   2    3
1 row updated.

SQL> SQL>
Commit complete.

SQL> SQL>
NAME                      SCORE ACTIV  JERSEYNUM
-------------------- ---------- ----- ----------
Smith, Steve                 42 TRUE          99
```



## SQL PL

{{works with|Db2 LUW}}
The following example is indeed parameterized SQL with named placeholders and it prevents SQL injections, and the SQL performs very well, because the execution plan is also precompiled.

```sql pl

--#SET TERMINATOR @

CREATE TABLE PLAYERS (
  NAME VARCHAR(32),
  SCORE INT,
  ACTIVE SMALLINT,
  JERSEYNUM INT
) @

CREATE PROCEDURE UPDATE_PLAYER (
  IN PLAYER_NAME VARCHAR(32),
  IN PLAYER_SCORE INT,
  IN PLAYER_ACTIVE SMALLINT,
  IN JERSEY_NUMBER INT
  )
 BEGIN
  UPDATE PLAYERS
  SET NAME = PLAYER_NAME, SCORE = PLAYER_SCORE, ACTIVE = PLAYER_ACTIVE
  WHERE JERSEYNUM = JERSEY_NUMBER;
 END @

INSERT INTO PLAYERS VALUES ('Pele', '1280', 0, 10) @

CALL UPDATE_PLAYER ('Maradona', '600', 1, 10) @

SELECT * FROM PLAYERS @

```

Output:

```txt

db2 -td@
db2 => CREATE TABLE PLAYERS (
  NAME VARCHAR(32),
  SCORE INT,
  ACTIVE SMALLINT,
  JERSEYNUM INT
) @
DB20000I  The SQL command completed successfully.
db2 => CREATE PROCEDURE UPDATE_PLAYER (
  IN PLAYER_NAME VARCHAR(32),
  IN PLAYER_SCORE INT,
  IN PLAYER_ACTIVE SMALLINT,
  IN JERSEY_NUMBER INT
  )
 BEGIN
  UPDATE PLAYERS
  SET NAME = PLAYER_NAME, SCORE = PLAYER_SCORE, ACTIVE = PLAYER_ACTIVE
  WHERE JERSEYNUM = JERSEY_NUMBER;
 END @
DB20000I  The SQL command completed successfully.
db2 => INSERT INTO PLAYERS VALUES ('Pele', '1280', 0, 10) @
DB20000I  The SQL command completed successfully.

db2 => CALL UPDATE_PLAYER ('Maradona', '600', 1, 10) @
  Return Status = 0

db2 => SELECT * FROM PLAYERS @

NAME                             SCORE       ACTIVE JERSEYNUM
-------------------------------- ----------- ------ -----------
Maradona                                 600      1          10

  1 record(s) selected.
```



## Tcl

{{works with|Tcl|8.6}}

```tcl
package require Tcl 8.6

# These next two lines are the only ones specific to SQLite
package require tdbc::sqlite3
set db [tdbc::sqlite3::connection new /path/to/database.sql]

# Use a helper procedure to make a scope
proc setPlayer {db jersey -> playerName playerScore playerActive} {
    # Note that the '->' above is just syntactic noise for readability
    $db allrows {
	UPDATE players
	SET name = :playerName, score = :playerScore, active = :playerActive
	WHERE jerseyNum = :jersey
    }
    # The named parameters are bound to local variables by default
}

# How to use...
setPlayer $db 99 -> "Smith, Steve" 42 true
# With apologies to http://xkcd.com/327/
setPlayer $db 76 -> "Robert'; DROP TABLE players--" 0 false
$db close
```


{{omit from|360 Assembly}}
{{omit from|Maxima}}
{{omit from|PARI/GP}}
{{omit from|Pentium Assembly}}
{{omit from|X86 Assembly}}
{{omit from|6502 Assembly}}
{{omit from|ARM Assembly}}
{{omit from|MIPS Assembly}}
{{omit from|Assembly}}
{{omit from|Z80 Assembly}}
{{omit from|Computer/zero Assembly}}
{{omit from|OASYS Assembler}}
{{omit from|8086 Assembly}}
{{omit from|6800 Assembly}}
{{omit from|80386 Assembly}}
{{omit from|Jacquard Loom}}
{{omit from|DIV Games Studio}}
{{omit from|LC3 Assembly}}
{{omit from|8051 Assembly}}
{{omit from|VAX Assembly}}
{{omit from|68000 Assembly}}
{{omit from|PDP-11 Assembly}}
{{omit from|REXX}}
{{omit from|TI-83 BASIC|No network access or built-in SQL}}
{{omit from|TI-89 BASIC|No network access or built-in SQL}}
{{omit from|Unlambda|No network access or built-in SQL}}
{{omit from|XSLT 1.0}}
