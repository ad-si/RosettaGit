+++
title = "SQL-based authentication"
description = ""
date = 2018-11-29T09:17:48Z
aliases = []
[extra]
id = 1912
[taxonomies]
categories = ["task", "Database operations"]
tags = []
languages = [
  "c",
  "csharp",
  "go",
  "java",
  "julia",
  "kotlin",
  "mathematica",
  "objeck",
  "perl",
  "perl_6",
  "php",
  "python",
  "racket",
  "raven",
  "ruby",
  "sidef",
  "tcl",
]
+++

## Task

This task has three parts:
* Connect to a [[MySQL]] database (<tt>connect_db</tt>)
* Create user/password records in the following table (<tt>create_user</tt>)
* Authenticate login requests against the table (<tt>authenticate_user</tt>)

This is the table definition:

```sql
create table users (
    userid int primary key auto_increment,
    username varchar(32) unique key not null,
    pass_salt tinyblob not null,
            -- a string of 16 random bytes
    pass_md5 tinyblob not null
            -- binary MD5 hash of pass_salt concatenated with the password
);
```

(<tt>pass_salt</tt> and <tt>pass_md5</tt> would be <tt>binary(16)</tt> values, but MySQL versions before 5.0.15 strip trailing spaces when selecting them.)


## C

{{libheader|mysqlclient}} (MySQL client library)
{{libheader|OpenSSL}} (for MD5)

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <time.h>

#include <mysql.h>
#include <openssl/md5.h>

void end_with_db(void);

MYSQL *mysql = NULL; // global...

bool connect_db(const char *host, const char *user, const char *pwd,
		const char *db, unsigned int port)
{
  if ( mysql == NULL )
  {
    if (mysql_library_init(0, NULL, NULL)) return false;
    mysql = mysql_init(NULL); if ( mysql == NULL ) return false;
    MYSQL *myp = mysql_real_connect(mysql, host, user, pwd, db, port, NULL, 0);
    if (myp == NULL) {
      fprintf(stderr, "connection error: %s\n", mysql_error(mysql));
      end_with_db();
      return false;
    }
  }
  return true; // already connected... ?
}

#define USERNAMELIMIT 32
// no part of the spec, but it is reasonable!!
#define PASSWORDLIMIT 32
#define SALTBYTE 16
bool create_user(const char *username, const char *password)
{
  int i;
  char binarysalt[SALTBYTE];
  char salt[SALTBYTE*2+1];
  char md5hash[MD5_DIGEST_LENGTH];
  char saltpass[SALTBYTE+PASSWORDLIMIT+1];
  char pass_md5[MD5_DIGEST_LENGTH*2 + 1];
  char user[USERNAMELIMIT*2 + 1];
  char *q = NULL;
  static const char query[] =
    "INSERT INTO users "
    "(username,pass_salt,pass_md5) "
    "VALUES ('%s', X'%s', X'%s')";
  static const size_t qlen = sizeof query;


  for(i=0; username[i] != '\0' && i < USERNAMELIMIT; i++) ;
  if ( username[i] != '\0' ) return false;
  for(i=0; password[i] != '\0' && i < PASSWORDLIMIT; i++) ;
  if ( password[i] != '\0' ) return false;

  srand(time(NULL));

  for(i=0; i < SALTBYTE; i++)
  {
    // this skews the distribution but it is lazyness-compliant;)
    binarysalt[i] = rand()%256;
  }

  (void)mysql_hex_string(salt, binarysalt, SALTBYTE);

  for(i=0; i < SALTBYTE; i++) saltpass[i] = binarysalt[i];
  strcpy(saltpass+SALTBYTE, password);
  (void)MD5(saltpass, SALTBYTE + strlen(password), md5hash);
  (void)mysql_hex_string(pass_md5, md5hash, MD5_DIGEST_LENGTH);

  (void)mysql_real_escape_string(mysql, user, username, strlen(username));

  // salt, pass_md5, user are db-query-ready
  q = malloc(qlen + USERNAMELIMIT*2 + MD5_DIGEST_LENGTH*2 + SALTBYTE*2 + 1);
  if ( q == NULL ) return false;
  sprintf(q, query, user, salt, pass_md5);
#if defined(DEBUG)
  fprintf(stderr, "QUERY:\n%s\n\n", q);
#endif
  int res = mysql_query(mysql, q);
  free(q);
  if ( res != 0 )
  {
    fprintf(stderr, "create_user query error: %s\n", mysql_error(mysql));
    return false;
  }
  return true;
}


bool authenticate_user(const char *username, const char *password)
{
  char user[USERNAMELIMIT*2 + 1];
  char md5hash[MD5_DIGEST_LENGTH];
  char saltpass[SALTBYTE+PASSWORDLIMIT+1];
  bool authok = false;
  char *q = NULL;
  int i;
  static const char query[] =
    "SELECT * FROM users WHERE username='%s'";
  static const size_t qlen = sizeof query;

  // can't be authenticated with invalid username or password
  for(i=0; username[i] != '\0' && i < USERNAMELIMIT; i++) ;
  if ( username[i] != '\0' ) return false;
  for(i=0; password[i] != '\0' && i < PASSWORDLIMIT; i++) ;
  if ( password[i] != '\0' ) return false;

  (void)mysql_real_escape_string(mysql, user, username, strlen(username));

  q = malloc(qlen + strlen(user) + 1);
  if (q == NULL) return false;
  sprintf(q, query, username);

  int res = mysql_query(mysql, q);
  free(q);
  if ( res != 0 )
  {
    fprintf(stderr, "authenticate_user query error: %s\n", mysql_error(mysql));
    return false;
  }

  MYSQL_RES *qr = mysql_store_result(mysql);
  if ( qr == NULL ) return false;

  // should be only a result, or none
  if ( mysql_num_rows(qr) != 1 ) {
    mysql_free_result(qr);
    return false;
  }

  MYSQL_ROW row = mysql_fetch_row(qr);          // 1 row must exist
  unsigned long *len = mysql_fetch_lengths(qr); // and should have 4 cols...

  memcpy(saltpass, row[2], len[2]); // len[2] should be SALTBYTE
  memcpy(saltpass + len[2], password, strlen(password));
  (void)MD5(saltpass, SALTBYTE + strlen(password), md5hash);

  authok = memcmp(md5hash, row[3], len[3]) == 0;
  mysql_free_result(qr);

  return authok;
}

void end_with_db(void)
{
  mysql_close(mysql); mysql = NULL;
  mysql_library_end();
}

int main(int argc, char **argv)
{

  if ( argc < 4 ) return EXIT_FAILURE;

  if ( connect_db("localhost", "devel", "", "test", 0 ) )
  {
    if ( strcmp(argv[1], "add") == 0 )
    {
      if (create_user(argv[2], argv[3]))
	printf("created\n");
    } else if ( strcmp(argv[1], "auth") == 0 ) {
      if (authenticate_user(argv[2], argv[3]))
	printf("authorized\n");
      else
	printf("access denied\n");
    } else {
      printf("unknown command\n");
    }
    end_with_db();
  }
  return EXIT_SUCCESS;
}
```


From the command line, <tt>program add user password</tt> to add users, and <tt>program auth user password</tt> to see if the user with that password is authorized or not.


## C#

Class for hashing and random salt generation.

```c#
using System.Security.Cryptography;
using System.Text;

namespace rosettaMySQL
{
    class Hasher
    {
        private static string _BytesToHex(byte[] input)
        {
            var strBuilder = new StringBuilder();
            foreach (byte _byte in input)
            {
                strBuilder.Append(_byte.ToString("x2"));
            }
            return strBuilder.ToString();
        }

        public static string Hash(string salt, string input)
        {
            using (MD5 md5 = new MD5CryptoServiceProvider())
            {
                var bytes = Encoding.Default.GetBytes(salt + input);
                var data = md5.ComputeHash(bytes);
                return _BytesToHex(data);
            }
        }

        public static string GenSalt()
        {
            using (RandomNumberGenerator rng = new RNGCryptoServiceProvider())
            {
                var salt = new byte[16];
                rng.GetBytes(salt);
                return _BytesToHex(salt);
            }
        }
    }
}
```

Class for creating and authenticating users.

```c#
using MySql.Data.MySqlClient;

namespace rosettaMySQL
{
    class UserOperator
    {
        private MySqlConnection _connection;

        public UserOperator(MySqlConnection connection)
        {
            _connection = connection;
        }

        public bool CreateUser(string username, string password)
        {
            try
            {
                var salt = Hasher.GenSalt();
                var hash = Hasher.Hash(salt, password);
                var sql = $"INSERT INTO users " +
                          $"(username, pass_salt, pass_md5) " +
                          $"VALUES ('{username}','{salt}','{hash}')";
                using (var command = new MySqlCommand(sql, _connection))
                {
                    command.ExecuteNonQuery();
                    return true;
                }
            }
            catch (MySqlException e)
            {
                if (e.Number == 1062) //username is a duplicate
                {
                    return false;
                }
                else
                {
                    throw e;
                }
            }
        }

        public bool AuthenticateUser(string username, string password)
        {
            var sql = $"SELECT userid, username, pass_salt, pass_md5 " +
                      $"FROM users " +
                      $"WHERE username='{username}';";

            using (var command = new MySqlCommand(sql, _connection))
            using (var reader = command.ExecuteReader())
            {
                if (reader.HasRows)
                {
                    reader.Read();
                    var salt = reader.GetString("pass_salt");
                    var hash = reader.GetString("pass_md5");
                    return (Hasher.Hash(salt, password) == hash);
                }
                else
                {
                    return false;
                }
            }
        }
    }
}
```

Class with main method and database connection method.

```c#
using System;
using MySql.Data.MySqlClient;

namespace rosettaMySQL
{
    class Program
    {
        public static MySqlConnection ConnectDB(string server, int port, string db,
                                                string username, string password)
        {
            var connectStr = $"server={server};" +
                             $"user={username};" +
                             $"database={db};" +
                             $"port={port};" +
                             $"password={password}";
            return new MySqlConnection(connectStr);
        }

        static void Main(string[] args)
        {
            try
            {
                var connection = ConnectDB("localhost", 3306, "test", "root", "password");
                connection.Open();
                var userOperator = new UserOperator(connection);
                Console.WriteLine("Bob has been created: " + userOperator.CreateUser("Bob", "123456"));
                Console.WriteLine("Bob has been duplicated: " + userOperator.CreateUser("Bob", "123456"));
                Console.WriteLine("Wrong password works: " + userOperator.AuthenticateUser("BOB", "notpassword"));
                Console.WriteLine("Right password works: " + userOperator.AuthenticateUser("BOB", "123456"));
            }
            catch(MySqlException e)
            {
                switch(e.Number)
                {
                    case 0:
                        Console.WriteLine("Cannot connect to server");
                        break;
                    case 1045:
                        Console.WriteLine("Invalid database username/password");
                        break;
                    default:
                        Console.WriteLine(e.ToString());
                        Console.WriteLine(e.Number);
                        break;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.ToString());
            }
        }
    }
}
```

Output

```txt
Bob has been created: True
Bob has been duplicated: False
Wrong password works: False
Right password works: True
```



## Go


```go
package main

import (
    "bytes"
    "crypto/md5"
    "crypto/rand"
    "database/sql"
    "fmt"

    _ "github.com/go-sql-driver/mysql"
)

func connectDB() (*sql.DB, error) {
    return sql.Open("mysql", "rosetta:code@/rc")
}

func createUser(db *sql.DB, user, pwd string) error {
    salt := make([]byte, 16)
    rand.Reader.Read(salt)
    _, err := db.Exec(`insert into users (username, pass_salt, pass_md5)
        values (?, ?, ?)`, user, salt, saltHash(salt, pwd))
    if err != nil {
        return fmt.Errorf("User %s already exits", user)
    }
    return nil
}

func authenticateUser(db *sql.DB, user, pwd string) error {
    var salt, hash []byte
    row := db.QueryRow(`select pass_salt, pass_md5 from users
        where username=?`, user)
    if err := row.Scan(&salt, &hash); err != nil {
        return fmt.Errorf("User %s unknown", user)
    }
    if !bytes.Equal(saltHash(salt, pwd), hash) {
        return fmt.Errorf("User %s invalid password", user)
    }
    return nil
}

func saltHash(salt []byte, pwd string) []byte {
    h := md5.New()
    h.Write(salt)
    h.Write([]byte(pwd))
    return h.Sum(nil)
}

func main() {
    // demonstrate
    db, err := connectDB()
    defer db.Close()
    createUser(db, "sam", "123")
    err = authenticateUser(db, "sam", "123")
    if err == nil {
        fmt.Println("User sam authenticated")
    }

    // extra
    fmt.Println()
    // show contents of database
    rows, _ := db.Query(`select username, pass_salt, pass_md5 from users`)
    var user string
    var salt, hash []byte
    for rows.Next() {
        rows.Scan(&user, &salt, &hash)
        fmt.Printf("%s %x %x\n", user, salt, hash)
    }
    // try creating same user again
    err = createUser(db, "sam", "123")
    fmt.Println(err)
    // try authenticating unknown user
    err = authenticateUser(db, "pam", "123")
    fmt.Println(err)
    // try wrong password
    err = authenticateUser(db, "sam", "1234")
    fmt.Println(err)
    // clear table to run program again
    db.Exec(`truncate table users`)
}
```

```txt

User sam authenticated

sam d5d1ef775a89f2b504dc808e66087f3d a939f77466e91527e748377cc14dfd66
User sam already exits
User pam unknown
User sam invalid password

```



## Java

```java
import java.io.UnsupportedEncodingException;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.math.BigInteger;


class UserManager {
    private Connection dbConnection;

    public UserManager() {
    }

    private String md5(String aString) throws NoSuchAlgorithmException, UnsupportedEncodingException {
        MessageDigest md;
        String hex;
        StringBuffer hexString;
        byte[] bytesOfMessage;
        byte[] theDigest;

        hexString = new StringBuffer();
        bytesOfMessage = aString.getBytes("UTF-8");
        md = MessageDigest.getInstance("MD5");
        theDigest = md.digest(bytesOfMessage);

        for (int i = 0; i < theDigest.length; i++) {
            hex = Integer.toHexString(0xff & theDigest[i]);
            if (hex.length() == 1) {
                hexString.append('0');
            }
            hexString.append(hex);
        }

        return hexString.toString();
    }

    public void connectDB(String host, int port, String db, String user, String password)
      throws ClassNotFoundException, SQLException {

        Class.forName("com.mysql.jdbc.Driver");

        this.dbConnection =  DriverManager.getConnection("jdbc:mysql://"
                                + host
                                + ":"
                                + port
                                + "/"
                                + db, user, password);
    }

    public boolean createUser(String user, String password) {
        SecureRandom random;
        String insert;
        String salt;

        random = new SecureRandom();
        salt =  new BigInteger(130, random).toString(16);

        insert = "INSERT INTO users "
            + "(username, pass_salt, pass_md5) "
            + "VALUES (?, ?, ?)";

        try (PreparedStatement pstmt = this.dbConnection.prepareStatement(insert)) {
            pstmt.setString(1, user);
            pstmt.setString(2, salt);
            pstmt.setString(3, this.md5(salt + password));
            pstmt.executeUpdate();

            return true;
        } catch(NoSuchAlgorithmException | SQLException | UnsupportedEncodingException ex) {
            return false;
        }
    }

    public boolean authenticateUser(String user, String password) {
        String pass_md5;
        String pass_salt;
        String select;
        ResultSet res;

        select = "SELECT pass_salt, pass_md5 FROM users WHERE username = ?";
        res = null;

        try(PreparedStatement pstmt = this.dbConnection.prepareStatement(select)) {
            pstmt.setString(1, user);
            res = pstmt.executeQuery();

            res.next(); // We assume that username is unique

            pass_salt = res.getString(1);
            pass_md5 = res.getString(2);

            if (pass_md5.equals(this.md5(pass_salt + password))) {
                return true;
            } else {
                return false;
            }

        } catch(NoSuchAlgorithmException | SQLException | UnsupportedEncodingException ex) {
            return false;
        } finally {
            try {
                if (res instanceof ResultSet && !res.isClosed()) {
                    res.close();
                }
            } catch(SQLException ex) {
            }
        }
    }

    public void closeConnection() {
        try {
            this.dbConnection.close();
        } catch(NullPointerException | SQLException ex) {
        }
    }

    public static void main(String[] args) {
        UserManager um;

        um = new UserManager();
        try {
            um.connectDB("localhost", 3306, "test", "root", "admin");

            if (um.createUser("johndoe", "test")) {
                System.out.println("User created");
            }

            if (um.authenticateUser("johndoe", "test")) {
                System.out.println("User authenticated");
            }
        } catch(ClassNotFoundException | SQLException ex) {
            ex.printStackTrace();
        } finally {
            um.closeConnection();
        }
    }
}
```



## Julia


```julia

using MySQL
using Nettle  # for md5

function connect_db(uri, user, pw, dbname)
    mydb = mysql_connect(uri, user, pw, dbname)

    const command = """CREATE TABLE IF NOT EXISTS users (
                          userid INT PRIMARY KEY AUTO_INCREMENT,
                          username VARCHAR(32) UNIQUE KEY NOT NULL,
                          pass_salt tinyblob NOT NULL,
                              -- a string of 16 random bytes
                          pass_md5 tinyblob NOT NULL
                              -- binary MD5 hash of pass_salt concatenated with the password
                  );"""
    mysql_execute(mydb, command)
    mydb
end

function create_user(dbh, user, pw)
    mysql_stmt_prepare(dbh, "INSERT IGNORE INTO users (username, pass_salt, pass_md5) values (?, ?, ?);")
    salt = join([Char(c) for c in rand(UInt8, 16)], "")
    passmd5 = digest("md5", salt * user)
    mysql_execute(dbh, [MYSQL_TYPE_VARCHAR, MYSQL_TYPE_VARCHAR, MYSQL_TYPE_VARCHAR], [user, salt, passmd5])
end

function addusers(dbh, userdict)
    for user in keys(userdict)
        create_user(dbh, user, userdict[user])
    end
end

"""
    authenticate_user
Note this returns true if password provided authenticates as correct, false otherwise
"""
function authenticate_user(dbh, username, pw)
    mysql_stmt_prepare(dbh, "SELECT pass_salt, pass_md5 FROM users WHERE username = ?;")
    pass_salt, pass_md5 = mysql_execute(dbh, [MYSQL_TYPE_VARCHAR], [username], opformat=MYSQL_TUPLES)[1]
    pass_md5 == digest("md5", pass_salt * username)
end

const users = Dict("Joan" => "joanspw", "John" => "johnspw", "Mary" => "marpw", "Mark" => "markpw")
const mydb = connect_db("192.168.1.1", "julia", "julia", "mydb")

addusers(mydb, users)
println("""John authenticates correctly: $(authenticate_user(mydb, "John", "johnspw")==false)""")
println("""Mary does not authenticate with password of 123: $(authenticate_user(mydb, "Mary", "123")==false)""")
mysql_disconnect(mydb)

```



## Kotlin

```scala
// Version 1.2.41

import java.sql.Connection
import java.sql.DriverManager
import java.sql.ResultSet
import java.security.MessageDigest
import java.security.SecureRandom
import java.math.BigInteger

class UserManager {
    private lateinit var dbConnection: Connection

    private fun md5(message: String): String {
        val hexString = StringBuilder()
        val bytes = message.toByteArray()
        val md = MessageDigest.getInstance("MD5")
        val dig = md.digest(bytes)
        for (i in 0 until dig.size) {
            val hex = (0xff and dig[i].toInt()).toString(16)
            if (hex.length == 1) hexString.append('0')
            hexString.append(hex)
        }
        return hexString.toString()
    }

    fun connectDB(host: String, port: Int, db: String, user: String, pwd: String) {
        Class.forName("com.mysql.jdbc.Driver")
        dbConnection = DriverManager.getConnection(
            "jdbc:mysql://$host:$port/$db", user, pwd
        )
    }

    fun createUser(user: String, pwd: String): Boolean {
        val random = SecureRandom()
        val salt = BigInteger(130, random).toString(16)
        val insert = "INSERT INTO users " +
            "(username, pass_salt, pass_md5) " +
            "VALUES (?, ?, ?)"
        try {
            val pstmt = dbConnection.prepareStatement(insert)
            with (pstmt) {
                setString(1, user)
                setString(2, salt)
                setString(3, md5(salt + pwd))
                val rowCount = executeUpdate()
                close()
                if (rowCount == 0) return false
            }
            return true
        }
        catch (ex: Exception) {
            return false
        }
    }

    fun authenticateUser(user: String, pwd: String): Boolean {
        val select = "SELECT pass_salt, pass_md5 FROM users WHERE username = ?"
        lateinit var res: ResultSet
        try {
            val pstmt = dbConnection.prepareStatement(select)
            with (pstmt) {
                setString(1, user)
                res = executeQuery()
                res.next()  // assuming that username is unique
                val passSalt = res.getString(1)
                val passMD5  = res.getString(2)
                close()
                return passMD5 == md5(passSalt + pwd)
            }
        }
        catch (ex: Exception) {
            return false
        }
        finally {
            if (!res.isClosed) res.close()
        }
    }

    fun closeConnection() {
        if (!dbConnection.isClosed) dbConnection.close()
    }
}

fun main(args: Array<String>) {
    val um = UserManager()
    with (um) {
        try {
            connectDB("localhost", 3306, "test", "root", "admin")
            if (createUser("johndoe", "test")) println("User created")
            if (authenticateUser("johndoe", "test")) {
                println("User authenticated")
            }
        }
        catch(ex: Exception) {
            ex.printStackTrace()
        }
        finally {
            closeConnection()
        }
    }
}
```



## Mathematica

```Mathematica
Needs["DatabaseLink`"];
connectDb[dbUser_, dbPass_, dbUrl_] :=
  OpenSQLConnection[JDBC["mysql", dbUrl], "Username" -> dbUser,
   "Password" -> dbPass];
createUser::nameTaken = "The username '`1`' is already taken.";
createUser[dbUser_, dbPass_, dbUrl_, user_, pass_] :=
  Module[{db = connectDb[dbUser, dbPass, dbUrl],
    salt = RandomChoice[Range[32, 127], 16]},
   If[MemberQ[SQLSelect[db, "users", {"username"}], {user}],
    Message[createUser::nameTaken, user]; Return[]];
   SQLInsert[db,
    "users", {"username", "pass_salt", "pass_md5"}, {user,
     SQLBinary[salt],
     SQLBinary[
      IntegerDigits[Hash[FromCharacterCode[salt] <> pass, "MD5"], 256,
        16]]}]; CloseSQLConnection[db];];
authenticateUser[dbUser_, dbPass_, dbUrl_, user_, pass_] :=
  Module[{db = connectDb[dbUser, dbPass, dbUrl], rtn},
   rtn = MemberQ[SQLSelect[db, "users", {"username"}], {user}] &&
     Module[{data =
        SQLSelect[db, "users", {"username", "pass_salt", "pass_md5"},
          SQLColumn["username"] == user][[1]]},
      Hash[FromCharacterCode[data[[2, 1]]] <> pass, "MD5"] ==
       FromDigits[data[[3, 1]], 256]]; CloseSQLConnection[db]; rtn];
```



## Objeck


```objeck
use ODBC;
use Encryption;

class SqlTest {
  @conn : Connection;

  function : Main(args : String[]) ~ Nil {
    SqlTest->New()->Run();
  }

  New() {
    @conn := Connection->New("test", "root", "helloworld");
  }

  method : Run() ~ Nil {
    CreateUser("objeck", "beer");
    AuthenticateUser("objeck", "beer");
    leaving {
      @conn->Close();
    };
  }

  method : AuthenticateUser(username : String, password : String) ~ Nil {
    status := false;
    ps : ParameterStatement;
    result : ResultSet;
    if(@conn->IsOpen()) {
      sql := "SELECT pass_salt, pass_md5 FROM users WHERE username = ?";
      ps := @conn->CreateParameterStatement(sql);
      ps->SetVarchar(1, username);

      result := ps->Select();
      if(result <> Nil & result->Next()) {
        salt_buffer := Byte->New[16];
        result->GetBlob(1, salt_buffer);
        salt := "";
        for(i := 0; i < 16; i+=1;) {
          salt->Append(salt_buffer[i]);
        };

        db_password_buffer := Byte->New[16];
        result->GetBlob(2, db_password_buffer);

        password->Append(salt);
        user_password_buffer := Hash->MD5(password->ToByteArray());

        IO.Console->Print("user: authenticated=")->PrintLine(IsEqual(db_password_buffer, user_password_buffer));
      };

    };

    leaving {
      if(ps <> Nil) {
        ps->Close();
      };

      if(ps <> Nil) {
        ps->Close();
      };
    };
  }

  method : CreateUser(username : String, password : String) ~ Nil {
    salt := "";
    for(i := 0; i < 16; i+=1;) { salt->Append((Float->Random() * 100)->As(Int)); };
    salt := salt->SubString(16);

    password->Append(salt);
    md5_password := Hash->MD5(password->ToByteArray());

    ps : ParameterStatement;
    if(@conn->IsOpen()) {
      sql := "INSERT INTO users(username, pass_salt, pass_md5) VALUES (?, ?, ?)";
      ps := @conn->CreateParameterStatement(sql);
      ps->SetVarchar(1, username);
      ps->SetBytes(2, salt->ToByteArray());
      ps->SetBytes(3, md5_password);

      IO.Console->Print("adding user: username=")->Print(username)
        ->Print(", salt=")->Print(salt)
        ->Print(", status=")->PrintLine(ps->Update());
    };

    leaving {
      if(ps <> Nil) {
        ps->Close();
      };
    };
  }

  method : IsEqual(left : Byte[], right : Byte[]) ~ Bool {
    if(left->Size() <> right->Size()) {
      return false;
    };

    each(i : left) {
      if(left[i] <> right[i]) {
        return false;
      };
    };

    return true;
  }
}
```



## Perl

```perl
use DBI;

 # returns a database handle configured to throw an exception on query errors
sub connect_db {
    my ($dbname, $host, $user, $pass) = @_;
    my $db = DBI->connect("dbi:mysql:$dbname:$host", $user, $pass)
        or die $DBI::errstr;
    $db->{RaiseError} = 1;
    $db
}

 # if the user was successfully created, returns its user id.
 # if the name was already in use, returns undef.
sub create_user {
    my ($db, $user, $pass) = @_;
    my $salt = pack "C*", map {int rand 256} 1..16;
    $db->do("INSERT IGNORE INTO users (username, pass_salt, pass_md5)
        VALUES (?, ?, unhex(md5(concat(pass_salt, ?))))",
        undef, $user, $salt, $pass)
      and $db->{mysql_insertid} or undef
}

 # if the user is authentic, returns its user id.  otherwise returns undef.
sub authenticate_user {
    my ($db, $user, $pass) = @_;
    my $userid = $db->selectrow_array("SELECT userid FROM users WHERE
        username=? AND pass_md5=unhex(md5(concat(pass_salt, ?)))",
        undef, $user, $pass);
    $userid
}
```



## Perl 6

```perl6

use v6;
use DBIish;

multi connect_db(:$dbname, :$host, :$user, :$pass) {
   my $db = DBIish.connect("mysql",host => $host, database =>$dbname, user =>$user, password =>$pass, :RaiseError)
      or die "ERROR: {DBIish.errstr}.";
   $db;
}

multi create_user(:$db, :$user, :$pass) {
   #https://stackoverflow.com/questions/53365101/converting-pack-to-perl6
   my $salt = Buf.new((^256).roll(16));
   my $sth = $db.prepare(q:to/STATEMENT/);
      INSERT IGNORE INTO users (username, pass_salt, pass_md5)
      VALUES (?, ?, unhex(md5(concat(pass_salt, ?))))
   STATEMENT
   $sth.execute($user,$salt,$pass);
   $sth.insert-id or Any;
}

multi authenticate_user (:$db, :$user, :$pass) {
   my $sth = $db.prepare(q:to/STATEMENT/);
       SELECT userid FROM users WHERE
       username=? AND pass_md5=unhex(md5(concat(pass_salt, ?)))
   STATEMENT
   $sth.execute($user,$pass);
   my $userid =  $sth.fetch;
   $userid[0] or Any;
}
```



## PHP

To use MySQL in PHP you need the php_mysql module installed

```php

function connect_db($database, $db_user, $db_password, $host = 'localhost', $port = NULL, $die = false) {
	// Returns a MySQL link identifier (handle) on success
	// Returns false or dies() on error depending on the setting of parameter $die
	// Parameter $die configures error handling, setting it any non-false value will die() on error
	// Parameters $host, $port and $die have sensible defaults and are not usually required

	if(!$db_handle = @mysql_connect($host.($port ? ':'.$port : ''), $db_user, $db_password)) {
		if($die)
			die("Can't connect to MySQL server:\r\n".mysql_error());
		else
			return false;
	}
	if(!@mysql_select_db($database, $db_handle)) {
		if($die)
			die("Can't select database '$database':\r\n".mysql_error());
		else
			return false;
	}
	return $db_handle;
}

function create_user($username, $password, $db_handle) {
	// Returns the record ID on success or false on failure
	// Username limit is 32 characters (part of spec)
	if(strlen($username) > 32)
		return false;

	// Salt limited to ASCII 32 thru 254 (not part of spec)
	$salt = '';
	do {
		$salt .= chr(mt_rand(32, 254));
	} while(strlen($salt) < 16);

	// Create pass_md5
	$pass_md5 = md5($salt.$password);

	// Make it all binary safe
	$username = mysql_real_escape_string($username);
	$salt = mysql_real_escape_string($salt);

	// Try to insert it into the table - Return false on failure
	if(!@mysql_query("INSERT INTO users (username,pass_salt,pass_md5) VALUES('$username','$salt','$pass_md5')", $db_handle))
		return false;

	// Return the record ID
	return mysql_insert_id($db_handle);
}

function authenticate_user($username, $password, $db_handle) {
	// Checks a username/password combination against the database
	// Returns false on failure or the record ID on success

	// Make the username parmeter binary-safe
	$safe_username = mysql_real_escape_string($username);

	// Grab the record (if it exists) - Return false on failure
	if(!$result = @mysql_query("SELECT * FROM users WHERE username='$safe_username'", $db_handle))
		return false;

	// Grab the row
	$row = @mysql_fetch_assoc($result);

	// Check the password and return false if incorrect
	if(md5($row['pass_salt'].$password) != $row['pass_md5'])
		return false;

	// Return the record ID
	return $row['userid'];
}

```



## Python

Uses the [http://dev.mysql.com/downloads/connector/python/ official Python MySQL connector]

```python
import mysql.connector
import hashlib

import sys
import random

DB_HOST = "localhost"
DB_USER = "devel"
DB_PASS = "devel"
DB_NAME = "test"

def connect_db():
    ''' Try to connect DB and return DB instance, if not, return False '''
    try:
        return mysql.connector.connect(host=DB_HOST, user=DB_USER, passwd=DB_PASS, db=DB_NAME)
    except:
        return False

def create_user(username, passwd):
    ''' if user was successfully created, returns its ID; returns None on error '''
    db = connect_db()
    if not db:
        print "Can't connect MySQL!"
        return None

    cursor = db.cursor()

    salt = randomValue(16)
    passwd_md5 = hashlib.md5(salt+passwd).hexdigest()

    # If username already taken, inform it
    try:
        cursor.execute("INSERT INTO users (`username`, `pass_salt`, `pass_md5`) VALUES (%s, %s, %s)", (username, salt, passwd_md5))
        cursor.execute("SELECT userid FROM users WHERE username=%s", (username,) )
        id = cursor.fetchone()
        db.commit()
        cursor.close()
        db.close()
        return id[0]
    except:
        print 'Username was already taken. Please select another'
        return None

def authenticate_user(username, passwd):
    db = connect_db()
    if not db:
        print "Can't connect MySQL!"
        return False

    cursor = db.cursor()

    cursor.execute("SELECT pass_salt, pass_md5 FROM users WHERE username=%s", (username,))

    row = cursor.fetchone()
    cursor.close()
    db.close()
    if row is None:     # username not found
        return False
    salt = row[0]
    correct_md5 = row[1]
    tried_md5 = hashlib.md5(salt+passwd).hexdigest()
    return correct_md5 == tried_md5

def randomValue(length):
    ''' Creates random value with given length'''
    salt_chars = 'abcdefghijklmnopqrstuvwxyz0123456789'

    return ''.join(random.choice(salt_chars) for x in range(length))

if __name__ == '__main__':
    user = randomValue(10)
    passwd = randomValue(16)

    new_user_id = create_user(user, passwd)
    if new_user_id is None:
        print 'Failed to create user %s' % user
        sys.exit(1)
    auth = authenticate_user(user, passwd)
    if auth:
        print 'User %s authenticated successfully' % user
    else:
        print 'User %s failed' % user

```



## Racket



```racket
#lang racket
(require db file/md5)
(define-logger authentication)
(current-logger authentication-logger)

(define DB-HOST "localhost")
(define DB-USER "devel")
(define DB-PASS "devel")
(define DB-NAME "test")

(define (connect-db)
  (mysql-connect
   #:user DB-USER
   #:database DB-NAME
   #:password DB-PASS))

(define (salt+password->hash salt password #:hex-encode? (hex-encode? #f))
  (md5 (bytes-append salt password) hex-encode?))

(define (report-sql-error e)
  (log-authentication-error "Failed to create user:~s" (exn-message e))
  #f)

(define (create-user db username passwd)
  ; if user was successfully created, returns its ID else #f
  (define salt (list->bytes (for/list ((i (in-range 16))) (random 256))))
  (define hash (salt+password->hash salt passwd))
  (with-handlers ((exn:fail:sql? report-sql-error))
    (query db "INSERT INTO users (username, pass_salt, pass_md5) VALUES (?, ?, ?)"
           username salt hash)))

(define (authenticate-user db username password)
  (or
   (match (query-maybe-row db "SELECT pass_salt, pass_md5 FROM users WHERE username = ?" username)
     [#f #f]
     [(vector salt hash) (bytes=? hash (salt+password->hash salt password))])
   ; don't let the deviants know whether it's the username or password that's dodgy
   (error "the username, password combination does not exist in system")))

(module+ test
  (require rackunit)
  (define test-DB (connect-db))
  ; typically, you only do this the once (or risk upsetting your users bigtime!)
  ; call this just the once!
  (define (create-users-table db)
    (query-exec db "DROP TABLE IF EXISTS users")
    (query-exec db #<<EOS
CREATE TABLE users (
    userid INT PRIMARY KEY AUTO_INCREMENT,
    username VARCHAR(32) UNIQUE KEY NOT NULL,
    pass_salt tinyblob NOT NULL,
            -- a string of 16 random bytes
    pass_md5 tinyblob NOT NULL
            -- binary MD5 hash of pass_salt concatenated with the password
);
EOS
                ))
  (create-users-table test-DB)
  (create-user test-DB #"tim" #"shh! it's a secret!")
  ; ensure the user exists (for testing purposes)
  (check-match (query-list test-DB "SELECT userid FROM users WHERE username = 'tim'") (list _))
  ; (ah... but tim exists!!!)
  (check-false (create-user test-DB #"tim" #"tim's password"))
  (check-exn exn:fail? (λ () (authenticate-user test-DB #"tim" #"password")))
  (check-true (authenticate-user test-DB #"tim" #"shh! it's a secret!")))
```



## Raven

MySQL connectivity is available out of the box.  Below, we do not ever send the plain text password over the wire.


```raven
 'mysql://root@localhost/test' open as mysql
'abcdefghijklmnopqrstuvwxyz0123456789' as $salt_chars

# return userid for success and FALSE for failure.
define create_user use $user, $pass
    group 16 each as i
        $salt_chars choose chr
    join as $pass_salt
     "%($pass_salt)s%($pass)s" md5 as $pass_md5
    $user copy mysql escape as $user_name
    group 'INSERT IGNORE into users (username, pass_md5, pass_salt)'
        " VALUES ('%($user_name)s', unhex('%($pass_md5)s'), '%($pass_salt)s')"
    join mysql query inserted

# return userid for success and FALSE for failure.
define authenticate_user use $user, $pass
    FALSE as $userid
    $user copy mysql escape as $user_name
    group 'SELECT userid, pass_salt, hex(pass_md5)'
        " FROM users WHERE username = '%($user_name)s'"
    join mysql query as rs
    rs selected
    if  rs fetch values into $possible_userid, $pass_salt, $pass_md5
        "%($pass_salt)s%($pass)s" md5 $pass_md5 lower =
        if  $possible_userid as $userid
    $userid

'foo' 'bar' create_user       !if "could not create user\n"       print bye
'foo' 'bar' authenticate_user !if "could not authenticate user\n" print bye

"user successfully created and authenticated!\n" print
```



## Ruby

Uses the [https://github.com/brianmario/mysql2 mysql2 gem]

```ruby
require 'mysql2'
require 'securerandom'
require 'digest'

def connect_db(host, port = nil, username, password, db)
  Mysql2::Client.new(
    host: host,
    port: port,
    username: username,
    password: password,
    database: db
  )
end

def create_user(client, username, password)
  salt = SecureRandom.random_bytes(16)
  password_md5 = Digest::MD5.hexdigest(salt + password)

  statement = client.prepare('INSERT INTO users (username, pass_salt, pass_md5) VALUES (?, ?, ?)')
  statement.execute(username, salt, password_md5)
  statement.last_id
end

def authenticate_user(client, username, password)
  user_record = client.prepare("SELECT SELECT pass_salt, pass_md5 FROM users WHERE username = '#{client.escape(username)}'").first
  return false unless user_record

  password_md5 = Digest::MD5.hexdigest(user_record['pass_salt'] + password)
  password_md5 == user_record['pass_md5']
end
```



## Sidef

```ruby
require('DBI')

 # returns a database handle configured to throw an exception on query errors
func connect_db(dbname, host, user, pass) {
    var db = %s<DBI>.connect("dbi:mysql:#{dbname}:#{host}", user, pass)
    db || die (global DBI::errstr)
    db{:RaiseError} = 1
    db
}

 # if the user was successfully created, returns its user id.
 # if the name was already in use, returns nil.
func create_user(db, user, pass) {
    var salt = "C*".pack(16.of { 256.irand }...)
    db.do(
        "INSERT IGNORE INTO users (username, pass_salt, pass_md5)
         VALUES (?, ?, unhex(md5(concat(pass_salt, ?))))", nil, user, salt, pass
    ) ? db{:mysql_insertid} : nil
}

 # if the user is authentic, returns its user id.  otherwise returns nil.
func authenticate_user(db, user, pass) {
    db.selectrow_array("SELECT userid FROM users WHERE
        username=? AND pass_md5=unhex(md5(concat(pass_salt, ?)))",
        nil, user, pass
    )
}
```



## Tcl

Also requires the TDBC driver for MySQL.

```Tcl
package require tdbc

proc connect_db {handleName dbname host user pass} {
    package require tdbc::mysql
    tdbc::mysql::connection create $handleName -user $user -passwd $pass \
        -host $host -database $dbname
    return $handleName
}

# A simple helper to keep code shorter
proc r64k {} {
    expr int(65536*rand())
}

proc create_user {handle user pass} {
    set salt [binary format ssssssss \
        [r64k] [r64k] [r64k] [r64k] [r64k] [r64k] [r64k] [r64k]]
    # Note that we are using named parameters below, :user :salt :pass
    # They are bound automatically to local variables with the same name
    $handle allrows {
        INSERT IGNORE INTO users (username, pass_salt, pass_md5)
            VALUES (:user, :salt, unhex(md5(concat(:salt, :pass))))
    }
    return   ;# Ignore the result of the allrows method
}

proc authenticate_user {handle user pass} {
    $handle foreach row {
        SELECT userid FROM users WHERE
            username=:user AND pass_md5=unhex(md5(concat(pass_salt, :pass)))
    } {
        return [dict get $row userid]
    }
    # Only get here if no rows selected
    error "authentication failed for user \"$user\""
}
```


{{omit from|SQL PL|Db2 is a database}} <!-- However, Db2 can connect to other databases via federation or with an external stored procedure written in Java or C. -->
