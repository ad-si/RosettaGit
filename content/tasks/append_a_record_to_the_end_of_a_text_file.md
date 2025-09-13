+++
title = "Append a record to the end of a text file"
description = ""
date = 2019-09-27T23:46:43Z
aliases = []
[extra]
id = 10507
[taxonomies]
categories = ["File handling", "task"]
tags = []
+++

## Task
Many systems offer the ability to open a file for writing, such that any data written will be appended to the end of the file. Further, the file operations will always adjust the position pointer to guarantee the end of the file, even in a multitasking environment.

This feature is most useful in the case of log files, where many jobs may be appending to the log file at the same time, or where care ''must'' be taken to avoid concurrently overwriting the same record from another job.


;Task:
Given a two record sample for a mythical "passwd" file:
* Write these records out in the typical system format.
** Ideally these records will have named fields of various types.
* Close the file, then reopen the file for append.
** '''Append''' a new record to the file and close the file again.
** Take appropriate care to avoid concurrently overwrites from another job.
* Open the file and demonstrate the new record has indeed written to the end.

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Source record field types and contents.
|-
!account||password||UID||GID||fullname,office,extension,homephone,email||directory||shell
|-
!string||string||int||int||struct(string,string,string,string,string)||string||string
|-
|jsmith||x||1001||1000||Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org||/home/jsmith||/bin/bash
|-
|jdoe||x||1002||1000||Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org||/home/jdoe||/bin/bash
|}

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Record to be appended.
|-
!account||password||UID||GID||fullname,office,extension,homephone,email||directory||shell
|-
!string||string||int||int||struct(string,string,string,string,string)||string||string
|-
|xyz||x||1003||1000||X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org||/home/xyz||/bin/bash
|}

'''Resulting file format:''' should mimic Linux's /etc/passwd file format with particular attention to the "," separator used in the [[wp:Gecos field|GECOS field]].  But if the specific language has a particular or unique format of storing records in text file, then this format should be named and demonstrated with an additional example.

'''Expected output:'''

```txt

Appended record: xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```


'''Finally:''' Provide a summary of the language's "append record" capabilities in a table. eg.
{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| C struct || CSV text file || glibc/stdio || ☑ || ☑ || ☑ (Not all, eg NFS)
|}

Alternatively: If the language's appends can not guarantee its writes will '''always''' append, then note this restriction in the table. If possible, provide an actual code example (possibly using file/record locking) to guarantee correct concurrent appends.





## AWK


```AWK

# syntax: GAWK -f APPEND_A_RECORD_TO_THE_END_OF_A_TEXT_FILE.AWK
BEGIN {
    fn = "\\etc\\passwd"
# create and populate file
    print("account:password:UID:GID:fullname,office,extension,homephone,email:directory:shell") >fn
    print("jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash") >fn
    print("jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash") >fn
    close(fn)
    show_file("initial file")
# append record
    print("xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash") >>fn
    close(fn)
    show_file("file after append")
    exit(0)
}
function show_file(desc,  nr,rec) {
    printf("%s:\n",desc)
    while (getline rec <fn > 0) {
      nr++
      printf("%s\n",rec)
    }
    close(fn)
    printf("%d records\n\n",nr)
}

```

<p>Output:</p>

```txt

initial file:
account:password:UID:GID:fullname,office,extension,homephone,email:directory:shell
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
3 records

file after append:
account:password:UID:GID:fullname,office,extension,homephone,email:directory:shell
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
4 records

```


## Batch File


```dos

@echo off

(
  echo jsmith:x:1001:1000:Joe Smith,Room 1007,^(234^)555-8917,^(234^)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
  echo jdoe:x:1002:1000:Jane Doe,Room 1004,^(234^)555-8914,^(234^)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
) > append.txt

echo Current contents of append.txt:
type append.txt
echo.

echo xyz:x:1003:1000:X Yz,Room 1003,^(234^)555-8913,^(234^)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash >> append.txt

echo New contents of append.txt:
type append.txt
pause>nul

```


{{out}}

```txt

Current contents of append.txt:
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash

New contents of append.txt:
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```



## C

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| C struct || CSV text file || glibc/stdio || ☑ || ☑ || ☑ (Not all, eg NFS)
|}
'''Note:''' Not all [[wp:File system|File Systems]] support [[wp:Atomicity_(database_systems)#Implementation|atomic appends]].  In particular [[wp:Network File System (protocol)|NFS]] does not.  It is not known if there is a standard [[wp:Operating system|OS]] independent way of detecting if atomic appends are available.  However most Unix & Linux File Systems do support atomic appends, especially for [[wp:Log file|log files]].

'''From a C "struct" to CSV File'''

```c
#include <stdio.h>
#include <string.h>
/* note that UID & GID are of type "int" */
typedef const char *STRING;
typedef struct{STRING fullname, office, extension, homephone, email; } gecos_t;
typedef struct{STRING account, password; int uid, gid; gecos_t gecos; STRING directory, shell; } passwd_t;

#define GECOS_FMT "%s,%s,%s,%s,%s"
#define PASSWD_FMT "%s:%s:%d:%d:"GECOS_FMT":%s:%s"

passwd_t passwd_list[]={
  {"jsmith", "x", 1001, 1000, /* UID and GID are type int */
    {"Joe Smith", "Room 1007", "(234)555-8917", "(234)555-0077", "jsmith@rosettacode.org"},
    "/home/jsmith", "/bin/bash"},
  {"jdoe", "x", 1002, 1000,
    {"Jane Doe", "Room 1004", "(234)555-8914", "(234)555-0044", "jdoe@rosettacode.org"},
    "/home/jdoe", "/bin/bash"}
};

main(){
/****************************
* Create a passwd text file *
****************************/
  FILE *passwd_text=fopen("passwd.txt", "w");
  int rec_num;
  for(rec_num=0; rec_num < sizeof passwd_list/sizeof(passwd_t); rec_num++)
    fprintf(passwd_text, PASSWD_FMT"\n", passwd_list[rec_num]);
  fclose(passwd_text);

/********************************
* Load text ready for appending *
********************************/
  passwd_text=fopen("passwd.txt", "a+");
  char passwd_buf[BUFSIZ]; /* warning: fixed length */
  passwd_t new_rec =
      {"xyz", "x", 1003, 1000, /* UID and GID are type int */
          {"X Yz", "Room 1003", "(234)555-8913", "(234)555-0033", "xyz@rosettacode.org"},
          "/home/xyz", "/bin/bash"};
  sprintf(passwd_buf, PASSWD_FMT"\n", new_rec);
/* An atomic append without a file lock,
   Note: wont work on some file systems, eg NFS */
  write(fileno(passwd_text), passwd_buf, strlen(passwd_buf));
  close(passwd_text);

/***********************************************
* Finally reopen and check record was appended *
***********************************************/
  passwd_text=fopen("passwd.txt", "r");
  while(!feof(passwd_text))
    fscanf(passwd_text, "%[^\n]\n", passwd_buf, "\n");
  if(strstr(passwd_buf, "xyz"))
    printf("Appended record: %s\n", passwd_buf);
}
```

{{out}}

```txt

Appended record: xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```



## C++

{{trans|C#}}

```cpp
#include <iostream>
#include <fstream>
#include <string>
#include <vector>

std::ostream& operator<<(std::ostream& out, const std::string s) {
    return out << s.c_str();
}

struct gecos_t {
    std::string fullname, office, extension, homephone, email;

    friend std::ostream& operator<<(std::ostream&, const gecos_t&);
};

std::ostream& operator<<(std::ostream& out, const gecos_t& g) {
    return out << g.fullname << ',' << g.office << ',' << g.extension << ',' << g.homephone << ',' << g.email;
}

struct passwd_t {
    std::string account, password;
    int uid, gid;
    gecos_t gecos;
    std::string directory, shell;

    passwd_t(const std::string& a, const std::string& p, int u, int g, const gecos_t& ge, const std::string& d, const std::string& s)
        : account(a), password(p), uid(u), gid(g), gecos(ge), directory(d), shell(s)
    {
        //empty
    }

    friend std::ostream& operator<<(std::ostream&, const passwd_t&);
};

std::ostream& operator<<(std::ostream& out, const passwd_t& p) {
    return out << p.account << ':' << p.password << ':' << p.uid << ':' << p.gid << ':' << p.gecos << ':' << p.directory << ':' << p.shell;
}

std::vector<passwd_t> passwd_list{
    {
        "jsmith", "x", 1001, 1000,
        {"Joe Smith", "Room 1007", "(234)555-8917", "(234)555-0077", "jsmith@rosettacode.org"},
        "/home/jsmith", "/bin/bash"
    },
    {
        "jdoe", "x", 1002, 1000,
        {"Jane Doe", "Room 1004", "(234)555-8914", "(234)555-0044", "jdoe@rosettacode.org"},
        "/home/jdoe", "/bin/bash"
    }
};

int main() {
    // Write the first two records
    std::ofstream out_fd("passwd.txt");
    for (size_t i = 0; i < passwd_list.size(); ++i) {
        out_fd << passwd_list[i] << '\n';
    }
    out_fd.close();

    // Append the third record
    out_fd.open("passwd.txt", std::ios::app);
    out_fd << passwd_t("xyz", "x", 1003, 1000, { "X Yz", "Room 1003", "(234)555-8913", "(234)555-0033", "xyz@rosettacode.org" }, "/home/xyz", "/bin/bash") << '\n';
    out_fd.close();

    // Verify the record was appended
    std::ifstream in_fd("passwd.txt");
    std::string line, temp;
    while (std::getline(in_fd, temp)) {
        // the last line of the file is empty, make sure line contains the last record
        if (!temp.empty()) {
            line = temp;
        }
    }
    if (line.substr(0, 4) == "xyz:") {
        std::cout << "Appended record: " << line << '\n';
    } else {
        std::cout << "Failed to find the expected record appended.\n";
    }

    return 0;
}
```

{{out}}

```txt
Appended record: xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
```


## C#

```c#
using System;
using System.IO;

namespace AppendPwdRosetta
{
    class PasswordRecord
    {
        public string account, password, fullname, office, extension, homephone, email, directory, shell;
        public int UID, GID;
        public PasswordRecord(string account, string password, int UID, int GID, string fullname, string office, string extension, string homephone,
            string email, string directory, string shell)
        {
            this.account = account; this.password = password; this.UID = UID; this.GID = GID; this.fullname = fullname; this.office = office;
            this.extension = extension; this.homephone = homephone; this.email = email; this.directory = directory; this.shell = shell;
        }
        public override string ToString()
        {
            var gecos = string.Join(",", new string[] { fullname, office, extension, homephone, email });
            return string.Join(":", new string[] { account, password, UID.ToString(), GID.ToString(), gecos, directory, shell });
        }
    }
    class Program
    {
        static void Main(string[] args)
        {
            var jsmith = new PasswordRecord("jsmith", "x", 1001, 1000, "Joe Smith", "Room 1007", "(234)555-8917", "(234)555-0077", "jsmith@rosettacode.org",
                "/home/jsmith", "/bin/bash");
            var jdoe = new PasswordRecord("jdoe", "x", 1002, 1000, "Jane Doe", "Room 1004", "(234)555-8914", "(234)555-0044", "jdoe@rosettacode.org", "/home/jdoe",
                "/bin/bash");
            var xyz = new PasswordRecord("xyz", "x", 1003, 1000, "X Yz", "Room 1003", "(234)555-8913", "(234)555-0033", "xyz@rosettacode.org", "/home/xyz", "/bin/bash");

            // Write these records out in the typical system format.
            File.WriteAllLines("passwd.txt", new string[] { jsmith.ToString(), jdoe.ToString() });

            // Append a new record to the file and close the file again.
            File.AppendAllText("passwd.txt", xyz.ToString());

            // Open the file and demonstrate the new record has indeed written to the end.
            string[] lines = File.ReadAllLines("passwd.txt");
            Console.WriteLine("Appended record: " + lines[2]);
        }
    }
}

```


{{out}}

```txt
>AppendPwdRosetta.exe
Appended record: xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
```



## COBOL

{{works with|GnuCOBOL}}
COBOL is a record oriented language.  Commonly referred to as ISAM, Indexed
Sequential Access Mode IO, is the main type of file operations with COBOL, but
that is never quite technically correct as there can be various file handling
subsystems included with a COBOL installation.

GnuCOBOL supports ISAM for SEQUENTIAL, RANDOM and INDEXED file organizations,
that can be accessed with SEQUENTIAL, RANDOM and DYNAMIC modes.

LINE SEQUENTIAL access (an extension to standard COBOL) is also supported for
"normal" newline delimited text files. The GnuCOBOL compiler source kit ships
with a choice of four different file subsystem configurations, as well as
allowing external site defined handlers during compiler builds. So, even though
it is a common expression, ISAM is rarely a complete or technically correct
statement.  IBM COBOL dialects support a wide variety of mainframe file access
subsystems, for instance.

With GnuCOBOL, normal text file operations rely on the POSIX layer, and have
features very similar (or identical) to the technical interface enjoyed by C.

'''OPEN EXTEND''' provides automatic file appends. OPEN I-O allows for program
controlled appends with START LAST, READ NEXT and READ PREVIOUS positioning
control, paired with WRITE and REWRITE statements.

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| Record hierarchy || text file || POSIX || ☑ || ☑ || ☑ (Not all, eg TAPE devices)
|-
| Record hierarchy || SEQUENTIAL || POSIX/ISAM || ☑ || ☑ || ☑ (Not all)
|-
| Record hierarchy || RANDOM || POSIX/ISAM || ☑ || ☑ || ☑ (Not all)
|-
| Record hierarchy || INDEXED || ISAM || ☑ || ☑ || ☑ (Not all)
|}
'''Note:''' Not all [[wp:File system|File Systems]] support [[wp:Atomicity_(database_systems)#Implementation|atomic appends]].  In particular [[wp:Network File System (protocol)|NFS]], and many TAPE devices do not.  Guaranteed atomic append operations will be highly dependent on file handling subsystem and device capabilities.

'''Record group to passwd format, LINE SEQUENTIAL'''

```COBOL

      *> Tectonics:
      *>   cobc -xj append.cob
      *>   cobc -xjd -DDEBUG append.cob
      *> ***************************************************************
       identification division.
       program-id. append.

       environment division.
       configuration section.
       repository.
           function all intrinsic.

       input-output section.
       file-control.
           select pass-file
           assign to pass-filename
           organization is line sequential
           status is pass-status.

       REPLACE ==:LRECL:== BY ==2048==.

       data division.
       file section.
       fd pass-file record varying depending on pass-length.
       01 fd-pass-record.
          05 filler pic x occurs 0 to :LRECL: times
                          depending on pass-length.

       working-storage section.
       01 pass-filename.
          05 filler            value "passfile".
       01 pass-status          pic xx.
          88 ok-status         values '00' thru '09'.
          88 eof-pass          value '10'.

       01 pass-length          usage index.
       01 total-length         usage index.

       77 file-action          pic x(11).

       01 pass-record.
          05 account           pic x(64).
             88 key-account    value "xyz".
          05 password          pic x(64).
          05 uid               pic z(4)9.
          05 gid               pic z(4)9.
          05 details.
             10 fullname       pic x(128).
             10 office         pic x(128).
             10 extension      pic x(32).
             10 homephone      pic x(32).
             10 email          pic x(256).
          05 homedir           pic x(256).
          05 shell             pic x(256).

       77 colon                pic x value ":".
       77 comma-mark           pic x value ",".
       77 newline              pic x value x"0a".

      *> ***************************************************************
       procedure division.
       main-routine.
       perform initial-fill

       >>IF DEBUG IS DEFINED
       display "Initial data:"
       perform show-records
       >>END-IF

       perform append-record

       >>IF DEBUG IS DEFINED
       display newline "After append:"
       perform show-records
       >>END-IF

       perform verify-append
       goback
       .

      *> ***************************************************************
       initial-fill.
       perform open-output-pass-file

       move "jsmith" to account
       move "x" to password
       move 1001 to uid
       move 1000 to gid
       move "Joe Smith" to fullname
       move "Room 1007" to office
       move "(234)555-8917" to extension
       move "(234)555-0077" to homephone
       move "jsmith@rosettacode.org" to email
       move "/home/jsmith" to homedir
       move "/bin/bash" to shell
       perform write-pass-record

       move "jdoe" to account
       move "x" to password
       move 1002 to uid
       move 1000 to gid
       move "Jane Doe" to fullname
       move "Room 1004" to office
       move "(234)555-8914" to extension
       move "(234)555-0044" to homephone
       move "jdoe@rosettacode.org" to email
       move "/home/jdoe" to homedir
       move "/bin/bash" to shell
       perform write-pass-record

       perform close-pass-file
       .

      *> **********************
       check-pass-file.
       if not ok-status then
           perform file-error
       end-if
       .

      *> **********************
       check-pass-with-eof.
       if not ok-status and not eof-pass then
           perform file-error
       end-if
       .

      *> **********************
       file-error.
       display "error " file-action space pass-filename
               space pass-status upon syserr
       move 1 to return-code
       goback
       .

      *> **********************
       append-record.
       move "xyz" to account
       move "x" to password
       move 1003 to uid
       move 1000 to gid
       move "X Yz" to fullname
       move "Room 1003" to office
       move "(234)555-8913" to extension
       move "(234)555-0033" to homephone
       move "xyz@rosettacode.org" to email
       move "/home/xyz" to homedir
       move "/bin/bash" to shell

       perform open-extend-pass-file
       perform write-pass-record
       perform close-pass-file
       .

      *> **********************
       open-output-pass-file.
       open output pass-file with lock
       move "open output" to file-action
       perform check-pass-file
       .

      *> **********************
       open-extend-pass-file.
       open extend pass-file with lock
       move "open extend" to file-action
       perform check-pass-file
       .

      *> **********************
       open-input-pass-file.
       open input pass-file
       move "open input" to file-action
       perform check-pass-file
       .

      *> **********************
       close-pass-file.
       close pass-file
       move "closing" to file-action
       perform check-pass-file
       .

      *> **********************
       write-pass-record.
       set total-length to 1
       set pass-length to :LRECL:
       string
           account delimited by space
           colon
           password delimited by space
           colon
           trim(uid leading) delimited by size
           colon
           trim(gid leading) delimited by size
           colon
           trim(fullname trailing) delimited by size
           comma-mark
           trim(office trailing) delimited by size
           comma-mark
           trim(extension trailing) delimited by size
           comma-mark
           trim(homephone trailing) delimited by size
           comma-mark
           email delimited by space
           colon
           trim(homedir trailing) delimited by size
           colon
           trim(shell trailing) delimited by size
           into fd-pass-record with pointer total-length
           on overflow
               display "error: fd-pass-record truncated at "
                       total-length upon syserr
       end-string
       set pass-length to total-length
       set pass-length down by 1

       write fd-pass-record
       move "writing" to file-action
       perform check-pass-file
       .

      *> **********************
       read-pass-file.
       read pass-file
       move "reading" to file-action
       perform check-pass-with-eof
       .

      *> **********************
       show-records.
       perform open-input-pass-file

       perform read-pass-file
       perform until eof-pass
           perform show-pass-record
           perform read-pass-file
       end-perform

       perform close-pass-file
       .

      *> **********************
       show-pass-record.
       display fd-pass-record
       .

      *> **********************
       verify-append.
       perform open-input-pass-file

       move 0 to tally
       perform read-pass-file
       perform until eof-pass
           add 1 to tally
           unstring fd-pass-record delimited by colon
               into account
           if key-account then exit perform end-if
           perform read-pass-file
       end-perform
       if (key-account and tally not > 2) or (not key-account) then
           display
               "error: appended record not found in correct position"
              upon syserr
       else
           display "Appended record: " with no advancing
           perform show-pass-record
       end-if

       perform close-pass-file
       .

       end program append.
```

{{out}}

```txt
prompt$ cobc -xj append.cob
Appended record: xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```



## Common Lisp

Tested on CLISP 2.49

```lisp
(defvar *initial_data*
  (list
    (list "jsmith" "x" 1001 1000
      (list "Joe Smith" "Room 1007" "(234)555-8917" "(234)555-0077" "jsmith@rosettacode.org")
      "/home/jsmith" "/bin/bash")
    (list "jdoe" "x" 1002 1000
      (list "Jane Doe" "Room 1004" "(234)555-8914" "(234)555-0044" "jdoe@rosettacode.org")
      "/home/jdoe" "/bin/bash")))

(defvar *insert*
  (list "xyz" "x" 1003 1000
    (list "X Yz" "Room 1003" "(234)555-8913" "(234)555-0033" "xyz@rosettacode.org")
    "/home/xyz" "/bin/bash"))


(defun serialize (record delim)
  (string-right-trim delim ;; Remove trailing delimiter
    (reduce (lambda (a b)
      (typecase b
        (list (concatenate 'string a (serialize b ",") delim))
        (t (concatenate 'string a
          (typecase b
            (integer (write-to-string b))
            (t b))
          delim))))
  record :initial-value "")))


(defun main ()
  ;; Write initial values to file
  (with-open-file (stream "./passwd"
                  :direction :output
                  :if-exists :supersede
                  :if-does-not-exist :create)
    (loop for x in *initial_data* do
      (format stream (concatenate 'string (serialize x ":") "~%"))))

  ;; Reopen file, append insert value
  (with-open-file (stream "./passwd"
                  :direction :output
                  :if-exists :append)
    (format stream (concatenate 'string (serialize *insert* ":") "~%")))

  ;; Reopen file, search for new record
  (with-open-file (stream "./passwd")
    (when stream
      (loop for line = (read-line stream nil)
        while line do
          (if (search "xyz" line)
            (format t "Appended record: ~a~%" line))))))

(main)

```

{{out}}

```txt
$ clisp append_file.cl
Appended record: xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
```


{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| Lists || Text file || Common Lisp Standard Library || ☑ || ☑ || ☑ (Hopefully!)
|}


## D

{{trans|Java}}

```D
class Record {
    private const string account;
    private const string password;
    private const int uid;
    private const int gid;
    private const string[] gecos;
    private const string directory;
    private const string shell;

    public this(string account, string password, int uid, int gid, string[] gecos, string directory, string shell) {
        import std.exception;
        this.account = enforce(account);
        this.password = enforce(password);
        this.uid = uid;
        this.gid = gid;
        this.gecos = enforce(gecos);
        this.directory = enforce(directory);
        this.shell = enforce(shell);
    }

    public void toString(scope void delegate(const(char)[]) sink) const {
        import std.conv   : toTextRange;
        import std.format : formattedWrite;
        import std.range  : put;

        sink(account);
        put(sink, ':');
        sink(password);
        put(sink, ':');
        toTextRange(uid, sink);
        put(sink, ':');
        toTextRange(gid, sink);
        put(sink, ':');
        formattedWrite(sink, "%-(%s,%)", gecos);
        put(sink, ':');
        sink(directory);
        put(sink, ':');
        sink(shell);
    }
}

public Record parse(string text) {
    import std.array  : split;
    import std.conv   : to;
    import std.string : chomp;

    string[] tokens = text.chomp.split(':');
    return new Record(
            tokens[0],
            tokens[1],
            to!int(tokens[2]),
            to!int(tokens[3]),
            tokens[4].split(','),
            tokens[5],
            tokens[6]);
}

void main() {
    import std.algorithm : map;
    import std.file      : exists, mkdir;
    import std.stdio;

    auto rawData = [
        "jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash",
        "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash",
        "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash"
    ];

    auto records = rawData.map!parse;

    if (!exists("_rosetta")) {
        mkdir("_rosetta");
    }

    auto passwd = File("_rosetta/.passwd", "w");
    passwd.lock();
    passwd.writeln(records[0]);
    passwd.writeln(records[1]);
    passwd.unlock();
    passwd.close();

    passwd.open("_rosetta/.passwd", "a");
    passwd.lock();
    passwd.writeln(records[2]);
    passwd.unlock();
    passwd.close();

    passwd.open("_rosetta/.passwd");
    foreach(string line; passwd.lines()) {
        parse(line).writeln();
    }
    passwd.close();
}
```


{{out}}

```txt
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
```



## Elixir

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| objects (subclass of Struct builtin) || text file || builtin || ☑ || ☑ || ☒
|}

```elixir

defmodule Gecos do
  defstruct [:fullname, :office, :extension, :homephone, :email]

  defimpl String.Chars do
    def to_string(gecos) do
      [:fullname, :office, :extension, :homephone, :email]
      |> Enum.map_join(",", &Map.get(gecos, &1))
    end
  end
end

defmodule Passwd do
  defstruct [:account, :password, :uid, :gid, :gecos, :directory, :shell]

  defimpl String.Chars do
    def to_string(passwd) do
      [:account, :password, :uid, :gid, :gecos, :directory, :shell]
      |> Enum.map_join(":", &Map.get(passwd, &1))
    end
  end
end

defmodule Appender do
  def write(filename) do
    jsmith = %Passwd{
      account: "jsmith",
      password: "x",
      uid: 1001,
      gid: 1000,
      gecos: %Gecos{
        fullname: "Joe Smith",
        office: "Room 1007",
        extension: "(234)555-8917",
        homephone: "(234)555-0077",
        email: "jsmith@rosettacode.org"
      },
      directory: "/home/jsmith",
      shell: "/bin/bash"
    }

    jdoe = %Passwd{
      account: "jdoe",
      password: "x",
      uid: 1002,
      gid: 1000,
      gecos: %Gecos{
        fullname: "Jane Doe",
        office: "Room 1004",
        extension: "(234)555-8914",
        homephone: "(234)555-0044",
        email: "jdoe@rosettacode.org"
      },
      directory: "/home/jdoe",
      shell: "/bin/bash"
    }

    xyz = %Passwd{
      account: "xyz",
      password: "x",
      uid: 1003,
      gid: 1000,
      gecos: %Gecos{
        fullname: "X Yz",
        office: "Room 1003",
        extension: "(234)555-8913",
        homephone: "(234)555-0033",
        email: "xyz@rosettacode.org"
      },
      directory: "/home/xyz",
      shell: "/bin/bash"
    }

    File.open!(filename, [:write], fn file ->
      IO.puts(file, jsmith)
      IO.puts(file, jdoe)
    end)

    File.open!(filename, [:append], fn file ->
      IO.puts(file, xyz)
    end)

    IO.puts File.read!(filename)
  end
end

Appender.write("passwd.txt")

```

{{out}}

```txt

jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```



## Fortran

Prior to F90, data aggregates can't be defined so one would use an assemblage of variables (perhaps with similar names) and be diligent over every READ and WRITE statement naming them all and in the right order. With F90 the assemblage can be declared via a single name such as NOTE which has subcomponents, if defined via repeated use of TYPE. In COBOL and pl/i, the definition of an aggregate can be done more smoothly. A further difficulty is that character variables have a fixed size, and there is no length field to allow a "string" style usage. Fortran 2003 formalised a scheme whereby character variables can be re-allocated with a suitable size on assignment so that for example <code>NOTE.ACCOUNT = "Fred"</code> would regenerate the variable as a four-character text until it was next assigned to, and there would be no trailing spaces as there are when the variable's length is fixed at 28. However, when written, there is no indication of the actual length of such a variable.

In the absence of a length indication, when outputting text variables with a view to having them read in again, delimiting them with quotes (and doubling internal quotes) is the route to peace of mind, because otherwise a text might contain a comma as in "43 Gurney Road, Belmont" and in the absence of quoting, such a sequence on input might well be taken as two fields rather than one and a mess is certain. This facility is offered by the free-format (or, "list directed") style initiated by the use of * in place of a format label, as in <code>WRITE (MSG,*) ''etc.''</code>, provided however that the output file has been opened with the optional usage attribute <code>DELIM = "QUOTE"</code>, an unfortunate choice of name, because the field separator character could also be termed a "delimiter" and the default value is both a space or comma, when often what is preferable is only a comma, or even a tab. But there is no standard method to specify this desire. Only if the texts never themselves contain commas or spaces will this unquoted free-format scheme evade extra effort, and the specified example precludes this simplicity.

The resulting output is strung along an output line, with a default line length of 132 (a standard lineprinter width); the specification of RECL = 666 ensures that all the output fields are rolled to one line - that isn't padded out to 666 characters: this is not a fixed-length record despite the specification of RECL as a constant, though each record turns out to be 248 characters long. Unfortunately, the trailing spaces in each character variable are rolled forth and there is no option along the lines of <code>WRITE (MSG,*) TRIM(NOTE)</code> One could instead use a suitable FORMAT statement with "A" format codes, but every element of NOTE would have to be named in the output list and the TRIM function applied only to each character field. Not only would this be tedious and error-prone, there is no format code for the enquoting and double-quoting of the texts and thus, no peace of mind... One would of course devise a subroutine to write out such a record (which would probably be more complex, with the FULLNAME subdivided, etc.), but the task's main objective is to demonstrate appending output to a file.
```Fortran
      PROGRAM DEMO	!As per the described task, more or less.
      TYPE DETAILS		!Define a component.
       CHARACTER*28 FULLNAME
       CHARACTER*12 OFFICE
       CHARACTER*16 EXTENSION
       CHARACTER*16 HOMEPHONE
       CHARACTER*88 EMAIL
      END TYPE DETAILS
      TYPE USERSTUFF		!Define the desired data aggregate.
       CHARACTER*8 ACCOUNT
       CHARACTER*8 PASSWORD	!Plain text!! Eeek!!!
       INTEGER*2 UID
       INTEGER*2 GID
       TYPE(DETAILS) PERSON
       CHARACTER*18 DIRECTORY
       CHARACTER*12 SHELL
      END TYPE USERSTUFF
      TYPE(USERSTUFF) NOTE	!After all that, I'll have one.
      NAMELIST /STUFF/ NOTE	!Enables free-format I/O, with names.
      INTEGER F,MSG,N
      MSG = 6	!Standard output.
      F = 10	!Suitable for some arbitrary file.
      OPEN(MSG, DELIM = "QUOTE")	!Text variables are to be enquoted.

Create the file and supply its initial content.
      OPEN (F, FILE="Staff.txt",STATUS="REPLACE",ACTION="WRITE",
     1 DELIM="QUOTE",RECL=666)	!Special parameters for the free-format WRITE working.

      WRITE (F,*) USERSTUFF("jsmith","x",1001,1000,
     1 DETAILS("Joe Smith","Room 1007","(234)555-8917",
     2  "(234)555-0077","jsmith@rosettacode.org"),
     2 "/home/jsmith","/bin/bash")

      WRITE (F,*) USERSTUFF("jdoe","x",1002,1000,
     1 DETAILS("Jane Doe","Room 1004","(234)555-8914",
     2  "(234)555-0044","jdoe@rosettacode.org"),
     3 "home/jdoe","/bin/bash")
      CLOSE (F)		!The file is now existing.

Choose the existing file, and append a further record to it.
      OPEN (F, FILE="Staff.txt",STATUS="OLD",ACTION="WRITE",
     1 DELIM="QUOTE",RECL=666,ACCESS="APPEND")

      NOTE = USERSTUFF("xyz","x",1003,1000,		!Create a new record's worth of stuff.
     1 DETAILS("X Yz","Room 1003","(234)555-8193",
     2  "(234)555-033","xyz@rosettacode.org"),
     3 "/home/xyz","/bin/bash")
      WRITE (F,*) NOTE					!Append its content to the file.
      CLOSE (F)

Chase through the file, revealing what had been written..
      OPEN (F, FILE="Staff.txt",STATUS="OLD",ACTION="READ",
     1 DELIM="QUOTE",RECL=666)
      N = 0
   10 READ (F,*,END = 20) NOTE	!As it went out, so it comes in.
      N = N + 1			!Another record read.
      WRITE (MSG,11) N		!Announce.
   11 FORMAT (/,"Record ",I0)	!Thus without quotes around the text part.
      WRITE (MSG,STUFF)		!Reveal.
      GO TO 10			!Try again.

Closedown.
   20 CLOSE (F)
      END
```


The output can be read back in with the same free-format style. The fields are separated by spaces (outside a quoted string) though commas are also allowed. The file content is as follows:

```txt

 "jsmith  " "x       "   1001   1000 "Joe Smith                   " "Room 1007   " "(234)555-8917   " "(234)555-0077   " "jsmith@rosettacode.org                                                                  " "/home/jsmith      " "/bin/bash   "
 "jdoe    " "x       "   1002   1000 "Jane Doe                    " "Room 1004   " "(234)555-8914   " "(234)555-0044   " "jdoe@rosettacode.org                                                                    " "home/jdoe         " "/bin/bash   "
 "xyz     " "x       "   1003   1000 "X Yz                        " "Room 1003   " "(234)555-8193   " "(234)555-033    " "xyz@rosettacode.org                                                                     " "/home/xyz         " "/bin/bash   "

```

Numerical values are written with sufficient space to allow for their maximum value, so the sixteen-bit integers are allowed six spaces (in case a minus sign might be needed) and so on for other types. Thus, complex numbers are written in the (''real'',''imaginary'') style. Given this fixed layout, an explicit FORMAT statement could be used and delimiters be abandoned along with worry over quoting, but any changes to the data structure will require corresponding changes to the associated FORMAT statement, and mistakes are easily made. For input via free-format, the trailing spaces in the quoted strings could be omitted; any needed will be supplied by the READ process.

To demonstrate that they have been read successfully, the data aggregate is printed out using the NAMELIST protocol, whereby each item in the output list is presented in the form <''name''> = <''value''>, as follows:

```txt


Record 1
 &STUFF
 NOTE%ACCOUNT = "jsmith  ",
 NOTE%PASSWORD        = "x       ",
 NOTE%UID     =   1001,
 NOTE%GID     =   1000,
 NOTE%PERSON%FULLNAME        = "Joe Smith                   ",
 NOTE%PERSON%OFFICE  = "Room 1007   ",
 NOTE%PERSON%EXTENSION       = "(234)555-8917   ",
 NOTE%PERSON%HOMEPHONE       = "(234)555-0077   ",
 NOTE%PERSON%EMAIL   = "jsmith@rosettacode.org                                                                  ",
 NOTE%DIRECTORY       = "/home/jsmith      ",
 NOTE%SHELL   = "/bin/bash   "
 /

Record 2
 &STUFF
 NOTE%ACCOUNT = "jdoe    ",
 NOTE%PASSWORD        = "x       ",
 NOTE%UID     =   1002,
 NOTE%GID     =   1000,
 NOTE%PERSON%FULLNAME        = "Jane Doe                    ",
 NOTE%PERSON%OFFICE  = "Room 1004   ",
 NOTE%PERSON%EXTENSION       = "(234)555-8914   ",
 NOTE%PERSON%HOMEPHONE       = "(234)555-0044   ",
 NOTE%PERSON%EMAIL   = "jdoe@rosettacode.org                                                                    ",
 NOTE%DIRECTORY       = "home/jdoe         ",
 NOTE%SHELL   = "/bin/bash   "
 /

Record 3
 &STUFF
 NOTE%ACCOUNT = "xyz     ",
 NOTE%PASSWORD        = "x       ",
 NOTE%UID     =   1003,
 NOTE%GID     =   1000,
 NOTE%PERSON%FULLNAME        = "X Yz                        ",
 NOTE%PERSON%OFFICE  = "Room 1003   ",
 NOTE%PERSON%EXTENSION       = "(234)555-8193   ",
 NOTE%PERSON%HOMEPHONE       = "(234)555-033    ",
 NOTE%PERSON%EMAIL   = "xyz@rosettacode.org                                                                     ",
 NOTE%DIRECTORY       = "/home/xyz         ",
 NOTE%SHELL   = "/bin/bash   "
 /
```

Because the output file (MSG) was opened with DELIM="QUOTE", the text variables are presented enquoted. This makes it clear whether fields have leading spaces or not, which with the usual proportionally-spaced typefaces and .html rendering is not clear in the example and could cause serious difficulty in practice as between "Joe Bloggs" and " Joe Bloggs". Because the DELIM feature applies to both free-format and namelist output, <code>WRITE (MSG,*) "Record",N</code> would produce output with the text "Record" in quotes, so instead a suitable FORMAT statement is used. Alas, for compound names the % symbol is used instead of a period, a pity. NAMELIST style I/O starts with "&STUFF" (the name of the NAMELIST) and ends with "/" and like free-format output, starts each line with a space that would be consumed as a carriage-control character for lineprinter output.

The "append" to a file is available only if the ACCESS="APPEND" facility is available, and alas, this is not standard Fortran though a common extension. Files opened for output are exclusive use. If a file exists but is in use, the OPEN statement will fail, so one should include the <code>ERR=''label'',IOSTAT=WHAT</code> to attempt to recover from lockouts. Except that IOSTAT error codes for a given problem may well vary from one system to another.


## FreeBASIC


```freebasic
' FB 1.05.0 Win64

Type Person
  As String  fullname
  As String  office
  As String  extension
  As String  homephone
  As String  email
  Declare Constructor()
  Declare Constructor(As String, As String, As String, As String, As String)
  Declare Operator Cast() As String
End Type

Constructor Person()
End Constructor

Constructor Person(fullname As String, office As String, extension As String, _
homephone As String, email As String)

  With This
    .fullname  = fullname
    .office    = office
    .extension = extension
    .homephone = homephone
    .email     = email
  End With

End Constructor

Operator Person.Cast() As String
  Return fullname + "," + office + "," + extension + "," + homephone + "," + email
End Operator

Type Record
  As String  account
  As String  password
  As Integer uid
  As Integer gid
  As Person  user
  As String  directory
  As String  shell
  Declare Constructor()
  Declare Constructor(As String, As String, As Integer, As Integer, As Person, As String, As String)
  Declare Operator Cast() As String
End Type

Constructor Record()
End Constructor

Constructor Record(account As String, password As String, uid As Integer, gid As Integer, user As Person, _
directory As String, shell As String)

  With This
   .account   = account
   .password  = password
   .uid       = uid
   .gid       = gid
   .user      = user
   .directory = directory
   .shell     = shell
  End With

End Constructor

Operator Record.Cast() As String
  Return account + ":" + password + ":" + Str(uid) + ":" + Str(gid) + ":" + user + ":" + directory + ":" + shell
End Operator

Dim persons(1 To 3) As Person
persons(1) = Person("Joe Smith", "Room 1007", "(234)555-8917", "(234)555-0077", "jsmith@rosettacode.org")
persons(2) = Person("Jane Doe",  "Room 1004", "(234)555-8914", "(234)555-0044", "jdoe@rosettacode.org"  )
persons(3) = Person("X Yz",      "Room 1003", "(234)555-8913", "(234)555-0033", "xyz@rosettacode.org"   )

Dim records(1 To 3) As Record
records(1) = Record("jsmith", "x", 1001, 1000, persons(1), "/home/jsmith", "/bin/bash")
records(2) = Record("jdoe",   "x", 1002, 1000, persons(2), "/home/jdoe"  , "/bin/bash")
records(3) = Record("xyz",    "x", 1003, 1000, persons(3), "/home/xyz"   , "/bin/bash")

Open "passwd.txt" For Output As #1
Print #1, records(1)
Print #1, records(2)
Close #1

Open "passwd.txt" For Append Lock Write As #1
Print #1, records(3)
Close #1
```


{{out}}

```txt

' contents of passwd.txt after appending the third record

jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```


{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| FB Type || CSV text file || FB Standard Library || ☑ || ☑ || ☑ (Hopefully!)
|}


## Go


```go
package main

import (
    "bytes"
    "fmt"
    "io"
    "io/ioutil"
    "os"
)

type pw struct {
    account, password string
    uid, gid          uint
    gecos
    directory, shell string
}

type gecos struct {
    fullname, office, extension, homephone, email string
}

func (p *pw) encode(w io.Writer) (int, error) {
    return fmt.Fprintf(w, "%s:%s:%d:%d:%s,%s,%s,%s,%s:%s:%s\n",
        p.account, p.password, p.uid, p.gid,
        p.fullname, p.office, p.extension, p.homephone, p.email,
        p.directory, p.shell)
}

// data cut and pasted from task description
var p2 = []pw{
    {"jsmith", "x", 1001, 1000, gecos{"Joe Smith", "Room 1007",
        "(234)555-8917", "(234)555-0077", "jsmith@rosettacode.org"},
        "/home/jsmith", "/bin/bash"},
    {"jdoe", "x", 1002, 1000, gecos{"Jane Doe", "Room 1004",
        "(234)555-8914", "(234)555-0044", "jdoe@rosettacode.org"},
        "/home/jsmith", "/bin/bash"},
}

var pa = pw{"xyz", "x", 1003, 1000, gecos{"X Yz", "Room 1003",
    "(234)555-8913", "(234)555-0033", "xyz@rosettacode.org"},
    "/home/xyz", "/bin/bash"}

var expected = "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913," +
    "(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash"

const pfn = "mythical"

func main() {
    writeTwo()
    appendOneMore()
    checkResult()
}

func writeTwo() {
    // overwrites any existing file
    f, err := os.Create(pfn)
    if err != nil {
        fmt.Println(err)
        return
    }
    defer func() {
        if cErr := f.Close(); cErr != nil && err == nil {
            fmt.Println(cErr)
        }
    }()
    for _, p := range p2 {
        if _, err = p.encode(f); err != nil {
            fmt.Println(err)
            return
        }
    }
}

func appendOneMore() {
    // file must already exist
    f, err := os.OpenFile(pfn, os.O_RDWR|os.O_APPEND, 0)
    if err != nil {
        fmt.Println(err)
        return
    }
    if _, err := pa.encode(f); err != nil {
        fmt.Println(err)
    }
    if cErr := f.Close(); cErr != nil && err == nil {
        fmt.Println(cErr)
    }
}

func checkResult() {
    // reads entire file then closes it
    b, err := ioutil.ReadFile(pfn)
    if err != nil {
        fmt.Println(err)
        return
    }
    if string(bytes.Split(b, []byte{'\n'})[2]) == expected {
        fmt.Println("append okay")
    } else {
        fmt.Println("it didn't work")
    }
}
```

{{out}}

```txt

append okay

```


{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| Byte slice. See note 1. || Abstracted. See note 2. || Go standard library. See note 3. || Yes || No. See note 4. || Should be, but see note 5.
|}
'''Notes:'''
# Data arguments for basic IO functions are type []byte.  All data being written ultimately gets encoded to []byte.  Some higher level functions accept strings and do the conversion as a convenience.  The fmt.Fprintf function used in this task encodes basic types directly to an io.Writer.  Some packages such as json and xml have marshallers to encode more complex data structures.  These features are common to all writes and are not specific to append.
# The device data representation is abstracted by the type os.File and its associated functions and methods.  These methods include seeking by linear file position so a typical data representation is mappable to a linear sequence of bytes.  That is about all that can be concluded from the capabilites of the os.File type.  This linear nature is not specific to append.
# Go's os package provides high-level services which work reasonably the same across all supported operating systems.  Currently the list is darwin, freebsd, linux, netbsd, openbsd, windows.  It does this with a common OS-independent interface.  Operations which cannot be offered accross all operating systems are not in this package.  Additional low-level and operating system specific functions are provided in the syscall package.  This organization is not specific to append.
# The file position after opening a file is 0 unless os.O_APPEND is specified in the os.OpenFile function.
# The Go documentation says nothing about file operations by multiple concurrent processes or threads.  As the operations are expected to work the same across operating systems however, they presumably do what you would expect.  It seems os.FileOpen with os.O_RDWR for example, gives exclusive file access (on local file systems anyway) whether os.O_APPEND is specified or not.


## Groovy

Solution:

```groovy
class PasswdRecord {
    String account, password, directory, shell
    int uid, gid
    SourceRecord source

    private static final fs = ':'
    private static final fieldNames = ['account', 'password', 'uid', 'gid', 'source', 'directory', 'shell']
    private static final stringFields = ['account', 'password', 'directory', 'shell']
    private static final intFields = ['uid', 'gid']

    PasswdRecord(String line = null) {
        if (!line) return
        def fields = line.split(fs)
        if (fields.size() != fieldNames.size()) {
            throw new IllegalArgumentException(
            "Passwd record must have exactly ${fieldNames.size()} '${fs}'-delimited fields")
        }
        (0..<fields.size()).each { i ->
            switch (fieldNames[i]) {
                case stringFields:    this[fieldNames[i]] = fields[i];            break
                case intFields:       this[fieldNames[i]] = fields[i] as Integer; break
                default /* source */: this.source = new SourceRecord(fields[i]);  break
            }
        }
    }

    @Override String toString() { fieldNames.collect { "${this[it]}${fs}" }.sum()[0..-2] }
}

class SourceRecord {
    String fullname, office, extension, homephone, email

    private static final fs = ','
    private static final fieldNames =
    ['fullname', 'office', 'extension', 'homephone', 'email']

    SourceRecord(String line = null) {
        if (!line) return
        def fields = line.split(fs)
        if (fields.size() != fieldNames.size()) {
            throw new IllegalArgumentException(
            "Source record must have exactly ${fieldNames.size()} '${fs}'-delimited fields")
        }
        (0..<fields.size()).each { i ->
            this[fieldNames[i]] = fields[i]
        }
    }

    @Override String toString() { fieldNames.collect { "${this[it]}${fs}" }.sum()[0..-2] }
}

def appendNewPasswdRecord = {
    PasswdRecord pwr = new PasswdRecord().with { p ->
        (account, password, uid, gid) = ['xyz', 'x', 1003, 1000]
        source = new SourceRecord().with { s ->
            (fullname, office, extension, homephone, email) =
                    ['X Yz', 'Room 1003', '(234)555-8913', '(234)555-0033', 'xyz@rosettacode.org']
            s
        }
        (directory, shell) = ['/home/xyz', '/bin/bash']
        p
    };

    new File('passwd.txt').withWriterAppend { w ->
        w.append(pwr as String)
        w.append('\r\n')
    }
}
```


Test:

```groovy
def checkPasswdFile = { it ->
    File passwd = new File('passwd.txt')
    assert passwd.exists()

    passwd.eachLine { line ->
        def pw = new PasswdRecord(line)
        assert pw && pw instanceof PasswdRecord
        assert pw.source && pw.source instanceof SourceRecord
        println pw
    }

    println()
}

println "File contents before new record added"
checkPasswdFile()

appendNewPasswdRecord()

println "File contents after new record added"
checkPasswdFile()
```


Output:

```txt
File contents before new record added
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash

File contents after new record added
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
```



## Haskell

Solution:

```haskell

{-# LANGUAGE RecordWildCards #-}

import System.IO
import Data.List (intercalate)

data Gecos = Gecos { fullname  :: String
                   , office    :: String
                   , extension :: String
                   , homephone :: String
                   , email     :: String
                   }

data Record = Record { account   :: String
                     , password  :: String
                     , uid       :: Int
                     , gid       :: Int
                     , directory :: String
                     , shell     :: String
                     , gecos     :: Gecos
                     }

instance Show Gecos where
    show (Gecos {..}) = intercalate "," [fullname, office, extension, homephone, email]

instance Show Record where
    show (Record {..}) = intercalate ":" [account, password, show uid, show gid, show gecos, directory, shell]

addRecord :: String -> Record -> IO ()
addRecord path r = appendFile path (show r)

```


Test:

```haskell

t1 = Record "jsmith" "x" 1001 1000 "/home/jsmith" "/bin/bash"
            (Gecos "Joe Smith" "Room 1007" "(234)555-8917" "(234)555-0077" "jsmith@rosettacode.org")

t2 = Record "jdoe" "x" 1002 1000 "/home/jdoe" "/bin/bash"
            (Gecos "Jane Doe" "Room 1004" "(234)555-8914" "(234)555-0044" "jdoe@rosettacode.org")

t3 = Record "xyz" "x" 1003 1000 "/home/xyz" "/bin/bash"
            (Gecos "X Yz" "Room 1003" "(234)555-8913" "(234)555-0033" "xyz@rosettacode.org")

main = do
    let path = "test.txt"
    forM [t1,t2,t3] (addRecord path)
    lastLine <- fmap (last . lines) (readFile path)
    putStrLn lastLine

```


=={{header|Icon}} and {{header|Unicon}}==
Works in both languages:

```unicon
procedure main()
    orig := [
       "jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash",
       "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash"
       ]
    new := [
       "xyz:x:1003:1000:X:Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash"
       ]
    fName := !open("mktemp","rp")
    every (f := open(fName,"w")) | write(f,!orig) | close(f)
    every (f := open(fName,"a")) | write(f,!new)  | close(f)
    every (f := open(fName,"r")) | write(!f)      | close(f)
end
```


Run:

```txt
->arteotf
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash
xyz:x:1003:1000:X:Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
->

```


{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| String || String || built-in || ☑ || ☑ || ☑ (Not all, eg NFS)
|}


## J


```j
require'strings ~system/packages/misc/xenos.ijs'
record=: [:|: <@deb;._1@(':',]);._2@do bind '0 :0'

passfields=: <;._1':username:password:gid:uid:gecos:home:shell'

passrec=: LF,~ [: }.@;@ , ':';"0 (passfields i. {. ) { a:,~ {:

R1=: passrec record''
   username: jsmith
   password: x
   gid: 1001
   uid: 1000
   gecos: Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org
   home: /home/jsmith
   shell: /bin/bash
)

R2=: passrec record''
   username: jdoe
   password: x
   gid: 1002
   uid: 1000
   gecos: Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org
   home: /home/jdoe
   shell: /bin/bash
)

R3=: passrec record''
   username: xyz
   password: x
   gid: 1003
   uid: 1000
   gecos: X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org
   home: /home/xyz
   shell: /bin/bash
)

passwd=: <'/tmp/passwd.txt'  NB. file needs to be writable on implementation machine

(R1,R2) fwrite passwd
R3 fappend passwd

assert 1 e. R3 E. fread passwd
```


{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
|-
! In core || On disk
|-
| array || literal
|}

Note that no file locking is needed if this is implemented under windows (since all file writes are atomic across processes -- only one process can have a file open at one time, by default).  Note that file locking would be needed under Linux (or unix), but it's [http://0pointer.de/blog/projects/locking2 not clear] which file locking mechanism should be used.


## Java



```java
import static java.util.Objects.requireNonNull;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RecordAppender {
    static class Record {
        private final String account;
        private final String password;
        private final int uid;
        private final int gid;
        private final List<String> gecos;
        private final String directory;
        private final String shell;

        public Record(String account, String password, int uid, int gid, List<String> gecos, String directory, String shell) {
            this.account = requireNonNull(account);
            this.password = requireNonNull(password);
            this.uid = uid;
            this.gid = gid;
            this.gecos = requireNonNull(gecos);
            this.directory = requireNonNull(directory);
            this.shell = requireNonNull(shell);
        }

        @Override
        public String toString() {
            return account + ':' + password + ':' + uid + ':' + gid + ':' + String.join(",", gecos) + ':' + directory + ':' + shell;
        }

        public static Record parse(String text) {
            String[] tokens = text.split(":");
            return new Record(
                    tokens[0],
                    tokens[1],
                    Integer.parseInt(tokens[2]),
                    Integer.parseInt(tokens[3]),
                    Arrays.asList(tokens[4].split(",")),
                    tokens[5],
                    tokens[6]);
        }
    }

    public static void main(String[] args) throws IOException {
        List<String> rawData = Arrays.asList(
                "jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,[email protected]:/home/jsmith:/bin/bash",
                "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,[email protected]:/home/jdoe:/bin/bash",
                "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,[email protected]:/home/xyz:/bin/bash"
        );

        List<Record> records = rawData.stream().map(Record::parse).collect(Collectors.toList());

        Path tmp = Paths.get("_rosetta", ".passwd");
        Files.createDirectories(tmp.getParent());
        Files.write(tmp, (Iterable<String>) records.stream().limit(2).map(Record::toString)::iterator);

        Files.write(tmp, Collections.singletonList(records.get(2).toString()), StandardOpenOption.APPEND);

        try (Stream<String> lines = Files.lines(tmp)) {
            lines.map(Record::parse).forEach(System.out::println);
        }
    }
}
```

{{out}}

```txt
Appended Record: xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
```



## Julia


```julia

using SHA  # security instincts say do not write bare passwords to a shared file even in toy code :)

mutable struct Personnel
    fullname::String
    office::String
    extension::String
    homephone::String
    email::String
    Personnel(ful,off,ext,hom,ema) = new(ful,off,ext,hom,ema)
end

mutable struct Passwd
     account::String
     password::String
     uid::Int32
     gid::Int32
     personal::Personnel
     directory::String
     shell::String
     Passwd(acc,pas,uid,gid,per,dir,she) =  new(acc,pas,uid,gid,per,dir,she)
end

function writepasswd(filename, passrecords)
    if(passrecords isa Array) == false
        passrecords = [passrecords]
    end
    fh = open(filename, "a") # should throw an exception if cannot open in a locked or exclusive mode for append
    for pas in passrecords
        record = join([pas.account, bytes2hex(sha256(pas.password)), pas.uid, pas.gid,
                 join([pas.personal.fullname, pas.personal.office, pas.personal.extension,
                 pas.personal.homephone, pas.personal.email], ','),
                 pas.directory, pas.shell], ':')
        write(fh, record, "\n")
    end
    close(fh)
end

const jsmith = Passwd("jsmith","x",1001, 1000, Personnel("Joe Smith", "Room 1007", "(234)555-8917", "(234)555-0077", "jsmith@rosettacode.org"), "/home/jsmith", "/bin/bash")
const jdoe = Passwd("jdoe","x",1002, 1000, Personnel("Jane Doe", "Room 1004", "(234)555-8914", "(234)555-0044", "jdoe@rosettacode.org"), "/home/jdoe", "/bin/bash")
const xyz = Passwd("xyz","x",1003, 1000, Personnel("X Yz", "Room 1003", "(234)555-8913", "(234)555-0033", "xyz@rosettacode.org"), "/home/xyz", "/bin/bash")

const pfile = "pfile.csv"
writepasswd(pfile, [jsmith, jdoe])
println("Before last record added, file is:\n$(readstring(pfile))")
writepasswd(pfile, xyz)
println("After last record added, file is:\n$(readstring(pfile))")

```

{{output}}
```txt

Before last record added, file is:
jsmith:2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash

After last record added, file is:
jsmith:2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
xyz:2d711642b726b04401627ca9fbac32f5c8530fb1903cc4db02258717921a4881:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```



## Kotlin

{{works with|Ubuntu 16.04}}

```scala
// Version 1.2.41

import java.io.File

class Record(
    val account: String,
    val password: String,
    val uid: Int,
    val gid: Int,
    val gecos: List<String>,
    val directory: String,
    val shell: String
){
    override fun toString() =
        "$account:$password:$uid:$gid:${gecos.joinToString(",")}:$directory:$shell"
}

fun parseRecord(line: String): Record {
    val fields = line.split(':')
    return Record(
        fields[0],
        fields[1],
        fields[2].toInt(),
        fields[3].toInt(),
        fields[4].split(','),
        fields[5],
        fields[6]
    )
}

fun main(args: Array<String>) {
    val startData = listOf(
        "jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,[email protected]:/home/jsmith:/bin/bash",
        "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,[email protected]:/home/jdoe:/bin/bash"
    )
    val records = startData.map { parseRecord(it) }
    val f = File("passwd.csv")
    f.printWriter().use {
        for (record in records) it.println(record)
    }
    println("Initial records:\n")
    f.forEachLine {
        println(parseRecord(it))
    }

    val newData = "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,[email protected]:/home/xyz:/bin/bash"
    val record = parseRecord(newData)
    if (!f.setWritable(true, true)) {
        println("\nFailed to make file writable only by owner\n.")
    }
    f.appendText("$record\n")
    println("\nRecords after another one is appended:\n")
    f.forEachLine {
        println(parseRecord(it))
    }
}
```


{{output}}

```txt

Initial records:

jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,[email protected]:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,[email protected]:/home/jdoe:/bin/bash

Records after another one is appended:

jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,[email protected]:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,[email protected]:/home/jdoe:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,[email protected]:/home/xyz:/bin/bash

```


{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| Class || Text file || JDK || ☑ || ☑ || ☑ (System dependent)
|}


## Lua


```lua
function append(tbl,filename)
    local file,err = io.open(filename, "a")
    if err then return err end

    file:write(tbl.account..":")
    file:write(tbl.password..":")
    file:write(tbl.uid..":")
    file:write(tbl.gid..":")

    for i,v in ipairs(tbl.gecos) do
        if i>1 then
            file:write(",")
        end
        file:write(v)
    end
    file:write(":")

    file:write(tbl.directory..":")
    file:write(tbl.shell.."\n")

    file:close()
end

local smith = {}
smith.account = "jsmith"
smith.password = "x"
smith.uid = 1001
smith.gid = 1000
smith.gecos = {"Joe Smith", "Room 1007", "(234)555-8917", "(234)555-0077", "jsmith@rosettacode.org"}
smith.directory = "/home/jsmith"
smith.shell = "/bin/bash"
append(smith, ".passwd")

local doe = {}
doe.account = "jdoe"
doe.password = "x"
doe.uid = 1002
doe.gid = 1000
doe.gecos = {"Jane Doe", "Room 1004", "(234)555-8914", "(234)555-0044", "jdoe@rosettacode.org"}
doe.directory = "/home/jdoe"
doe.shell = "/bin/bash"
append(doe, ".passwd")

local xyz = {}
xyz.account = "xyz"
xyz.password = "x"
xyz.uid = 1003
xyz.gid = 1000
xyz.gecos = {"X Yz", "Room 1003", "(234)555-8913", "(234)555-0033", "xyz@rosettacode.org"}
xyz.directory = "/home/xyz"
xyz.shell = "/bin/bash"
append(xyz, ".passwd")
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
data = <|"account" -> "xyz", "password" -> "x", "UID" -> 1003,
   "GID" -> 1000, "fullname" -> "X Yz", "office" -> "Room 1003",
   "extension" -> "(234)555-8913", "homephone" -> "(234)555-0033",
   "email" -> "xyz@rosettacode.org", "directory" -> "/home/xyz",
   "shell" -> "/bin/bash"|>;
asString[data_] :=
  StringRiffle[
   ToString /@
    Insert[data /@ {"account", "password", "UID", "GID", "directory",
       "shell"},
     StringRiffle[
      data /@ {"fullname", "office", "extension", "homephone",
        "email"}, ","], 5], ":"];
fname = FileNameJoin[{$TemporaryDirectory, "testfile"}];
str = OpenWrite[fname]; (* Use OpenAppend if file exists *)
Close[str];
Print["Appended record: " <> asString[data]];
```

{{out}}

```txt
Appended record: xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
```

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| Association || Nearly anything || Standard Library || ☑ || ☑ || ☐
|}

=={{header|MATLAB}} / {{header|Octave}}==

```Matlab
  DS{1}.account='jsmith';
  DS{1}.password='x';
  DS{1}.UID=1001;
  DS{1}.GID=1000;
  DS{1}.fullname='Joe Smith';
  DS{1}.office='Room 1007';
  DS{1}.extension='(234)555-8917';
  DS{1}.homephone='(234)555-0077';
  DS{1}.email='jsmith@rosettacode.org';
  DS{1}.directory='/home/jsmith';
  DS{1}.shell='/bin/bash';

  DS{2}.account='jdoe';
  DS{2}.password='x';
  DS{2}.UID=1002;
  DS{2}.GID=1000;
  DS{2}.fullname='Jane Doe';
  DS{2}.office='Room 1004';
  DS{2}.extension='(234)555-8914';
  DS{2}.homephone='(234)555-0044';
  DS{2}.email='jdoe@rosettacode.org';
  DS{2}.directory='/home/jdoe';
  DS{2}.shell='/bin/bash';

  function WriteRecord(fid, rec)
     fprintf(fid,"%s:%s:%i:%i:%s,%s,%s,%s,%s:%s%s\n", rec.account, rec.password, rec.UID, rec.GID, rec.fullname, rec.office, rec.extension, rec.homephone, rec.email, rec.directory, rec.shell);
     return;
  end

  %% write
  fid = fopen('passwd.txt','w');
  WriteRecord(fid,DS{1});
  WriteRecord(fid,DS{2});
  fclose(fid);

  new.account='xyz';
  new.password='x';
  new.UID=1003;
  new.GID=1000;
  new.fullname='X Yz';
  new.office='Room 1003';
  new.extension='(234)555-8913';
  new.homephone='(234)555-0033';
  new.email='xyz@rosettacode.org';
  new.directory='/home/xyz';
  new.shell='/bin/bash';

  %% append
  fid = fopen('passwd.txt','a+');
  WriteRecord(fid,new);
  fclose(fid);

  % read password file
  fid = fopen('passwd.txt','r');
  while ~feof(fid)
	printf('%s\n',fgetl(fid));
  end;
  fclose(fid);
```

{{out}}

```txt
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz/bin/bash

```



## Perl

The program uses flock(2) (or emulation, if flock is not available) to lock the file.

```Perl
use strict;
use warnings;

use Fcntl qw( :flock SEEK_END );

use constant {
    RECORD_FIELDS => [qw( account password UID GID GECOS directory shell )],
    GECOS_FIELDS  => [qw( fullname office extension homephone email )],
    RECORD_SEP    => ':',
    GECOS_SEP     => ',',
    PASSWD_FILE   => 'passwd.txt',
};

# here's our three records
my $records_to_write = [
    {
        account  => 'jsmith',
        password => 'x',
        UID      => 1001,
        GID      => 1000,
        GECOS    => {
            fullname  => 'John Smith',
            office    => 'Room 1007',
            extension => '(234)555-8917',
            homephone => '(234)555-0077',
            email     => 'jsmith@rosettacode.org',
        },
        directory => '/home/jsmith',
        shell     => '/bin/bash',
    },
    {
        account  => 'jdoe',
        password => 'x',
        UID      => 1002,
        GID      => 1000,
        GECOS    => {
            fullname  => 'Jane Doe',
            office    => 'Room 1004',
            extension => '(234)555-8914',
            homephone => '(234)555-0044',
            email     => 'jdoe@rosettacode.org',
        },
        directory => '/home/jdoe',
        shell     => '/bin/bash',
    },
];
my $record_to_append = {
    account  => 'xyz',
    password => 'x',
    UID      => 1003,
    GID      => 1000,
    GECOS    => {
        fullname  => 'X Yz',
        office    => 'Room 1003',
        extension => '(234)555-8913',
        homephone => '(234)555-0033',
        email     => 'xyz@rosettacode.org',
    },
    directory => '/home/xyz',
    shell     => '/bin/bash',
};

sub record_to_string {
    my $rec    = shift;
    my $sep    = shift // RECORD_SEP;
    my $fields = shift // RECORD_FIELDS;
    my @ary;
    for my $field (@$fields) {
        my $r = $rec->{$field};
        die "Field '$field' not found" unless defined $r;    # simple sanity check
        push @ary, ( $field eq 'GECOS' ? record_to_string( $r, GECOS_SEP, GECOS_FIELDS ) : $r );
    }
    return join $sep, @ary;
}

sub write_records_to_file {
    my $records  = shift;
    my $filename = shift // PASSWD_FILE;
    open my $fh, '>>', $filename or die "Can't open $filename: $!";
    flock( $fh, LOCK_EX ) or die "Can't lock $filename: $!";
    # if someone appended while we were waiting...
    seek( $fh, 0, SEEK_END ) or die "Can't seek $filename: $!" ;
    print $fh record_to_string($_), "\n" for @$records;
    flock( $fh, LOCK_UN ) or die "Can't unlock $filename: $!";
    # note: the file is closed automatically when function returns (and refcount of $fh becomes 0)
}

# write two records to file
write_records_to_file( $records_to_write );

# append one more record to file
write_records_to_file( [$record_to_append] );

# test
{
    use Test::Simple tests => 1;

    open my $fh, '<', PASSWD_FILE or die "Can't open ", PASSWD_FILE, ": $!";
    my @lines = <$fh>;
    chomp @lines;
    ok(
        $lines[-1] eq
'xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash',
        "Appended record: $lines[-1]"
    );
}

```


Output:

```txt

1..1
ok 1 - Appended record: xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```

File contents:

```txt

jsmith:x:1001:1000:John Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```


{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| Hash table || Text file || PerlIO || ☑ || ☑ || Advisory lock
|}
Note that flock uses advisory lock; some other program (if it doesn't use flock) can still unexpectedly write to the file.


## Perl 6

{{works with|Rakudo|2017.09}}

This is kind of silly as it takes a string, converts it to a record, and then instantly converts it back to a string to write out to a file. Most of the "record handling" code is just demonstrating a possible way to store records in memory. It really has nothing to do with appending a string to a file.


```perl6
class record {
    has $.name;
    has $.password;
    has $.UID;
    has $.GID;
    has $.fullname;
    has $.office;
    has $.extension;
    has $.homephone;
    has $.email;
    has $.directory;
    has $.shell;

    method gecos { join ',', $.fullname, $.office, $.extension, $.homephone, $.email }

    method gist {
        join ':',
        $.name,
        $.password,
        $.UID,
        $.GID,
        self.gecos,
        $.directory,
        $.shell;
    }
};

my $fname = 'foo.fil';

given $fname.IO.open(:w) { .close }; # clear file

sub append ($file, $line){
    my $fh = $file.IO.open(:a) or fail "Unable to open $file";
    given $fh {
        # Get a lock on the file, waits until lock is active
        .lock;
        # seek to the end in case some other process wrote to
        # the file while we were waiting for the lock
        .seek(0, SeekType::SeekFromEnd);
        # write the record
        .say: $line;
        .close;
    }
}

sub str-to-record ($str) {
    my %rec = <name password UID GID fullname office extension
      homephone email directory shell> Z=> $str.split(/<[:,]>/);
    my $user = record.new(|%rec);
}

for
  'jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash',
  'jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash'
-> $line {
        my $thisuser = str-to-record $line;
       $fname.&append: $thisuser.gist;
}

put "Last line of $fname before append:";
put $fname.IO.lines.tail;

$fname.&append: str-to-record('xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash').gist;

put "Last line of $fname after append:";
put $fname.IO.lines.tail;

```

{{out}}

```txt
Last line of foo.fil before append:
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
Last line of foo.fil after append:
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
```



{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| Class || Text file || Whichever is provided by<BR>the underlying VM || ☑ || ☑ || ☑ Advisory lock<BR>Depends on OS and VM
|}
Note that advisory locks do not prevent some other program (if it doesn't use flock) from unexpectedly writing to the file.


## Phix

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| sequence || text file || builtin || ☑ || ☑ || ☑ (Advisory lock)
|}
Locking is used to ensure multitasking safety. Note that on Phix, "multitasking" is the kid brother of multithreading, and the
calls to task_yield() in the code that follows are there to respect the phix-specific definition of multiple tasks being in the
same process, in which case locking does not make any real difference - because there is no task_yield() in a locked state.
You can also test the locking when running multiple processes/threads by uncommenting the wait_key() lines.

```Phix
constant filename = "passwd.txt"
integer fn

constant
 rec1 = {"jsmith","x",1001,1000,{"Joe Smith","Room 1007","(234)555-8917","(234)555-0077","jsmith@rosettacode.org"},"/home/jsmith","/bin/bash"},
 rec2 = {"jdoe","x",1002,1000,{"Jane Doe","Room 1004","(234)555-8914","(234)555-0044","jdoe@rosettacode.org"},"/home/jdoe","/bin/bash"},
 rec3 = {"xyz","x",1003,1000,{"X Yz","Room 1003","(234)555-8913","(234)555-0033","xyz@rosettacode.org"},"/home/xyz","/bin/bash"}

function tostring(sequence record)
    record[3] = sprintf("%d",{record[3]})
    record[4] = sprintf("%d",{record[4]})
    record[5] = join(record[5],",")
    record = join(record,":")
    return record
end function

procedure wait(string what)
    ?sprintf("wait (%s)",{what})
    sleep(1)
    task_yield()
end procedure

    if not file_exists(filename) then
        fn = open(filename,"w")
        if fn!=-1 then -- (someone else just beat us to it?)
            printf(fn,"account:password:UID:GID:fullname,office,extension,homephone,email:directory:shell\n")
            printf(fn,"%s\n",{tostring(rec1)})
            printf(fn,"%s\n",{tostring(rec2)})
            close(fn)
        end if
    end if
    while 1 do
        fn = open(filename,"a")
        if fn!=-1 then exit end if
        wait("append")
    end while
--  ?"file open in append mode"; {} = wait_key()
    while 1 do
        if lock_file(fn,LOCK_EXCLUSIVE,{}) then exit end if
        wait("lock")
    end while
--  ?"file locked"; {} = wait_key()
    printf(fn,"%s\n",{tostring(rec3)})
    unlock_file(fn,{})
    close(fn)
    while 1 do
        fn = open(filename,"r")
        if fn!=-1 then exit end if
        wait("read")
    end while

    ?gets(fn)
    while 1 do
        object line = gets(fn)
        if atom(line) then exit end if
        ?line
        {line} = scanf(line,"%s:%s:%d:%d:%s:%s:%s\n")
        line[5] = split(line[5],',')
        ?line
    end while
    close(fn)
```

{{out}}

```txt

"account:password:UID:GID:fullname,office,extension,homephone,email:directory:shell\n"
"jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash\n"
{"jsmith","x",1001,1000,{"Joe Smith","Room 1007","(234)555-8917","(234)555-0077","jsmith@rosettacode.org"},"/home/jsmith","/bin/bash"}
"jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash\n"
{"jdoe","x",1002,1000,{"Jane Doe","Room 1004","(234)555-8914","(234)555-0044","jdoe@rosettacode.org"},"/home/jdoe","/bin/bash"}
"xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash\n"
{"xyz","x",1003,1000,{"X Yz","Room 1003","(234)555-8913","(234)555-0033","xyz@rosettacode.org"},"/home/xyz","/bin/bash"}

```



## PicoLisp


```PicoLisp
(setq L '((jsmith x 1001 1000
   "Joe Smith,Room 1007,(234)555-8917,\
   (234)555-0077,jsmith@rosettacode.org"
   /home/jsmith /bin/bash )
   (jdoe x 1002 1000
   "Jane Doe,Room 1004,(234)555-8914,\
   (234)555-0044,jdoe@rosettacode.org"
   /home/jsmith /bin/bash ) ) )

(setq A '(xyz x 1003 1000
    "X Yz,Room 1003,(234)555-8913,\
    (234)555-0033,xyz@rosettacode.org"
    /home/xyz /bin/bash ) )

(out "mythical"
   (for I L
      (prinl (glue ': I)) ) )
(out "+mythical"
   (prinl (glue ': A)) )
(in "mythical"
   (do 2 (line))
   (println
      (and
         (= "xyz" (pack (till ':)))
         (= 3 (lines "mythical") ) ) ) )

(bye)
```



## PowerShell

I treated the file as a CSV file without header information.  File testing is minimal.

Since PowerShell loves to deal in objects I wrote extra code to better manipilate and display data.

```PowerShell

function Test-FileLock
{
    Param
    (
        [parameter(Mandatory=$true)]
        [string]
        $Path
    )

    $outFile = New-Object System.IO.FileInfo $Path

    if (-not(Test-Path -Path $Path))
    {
        return $false
    }

    try
    {
        $outStream = $outFile.Open([System.IO.FileMode]::Open, [System.IO.FileAccess]::ReadWrite, [System.IO.FileShare]::None)

        if ($outStream)
        {
            $outStream.Close()
        }

        return $false
    }
    catch
    {
        # File is locked by a process.
        return $true
    }
}

function New-Record
{
    Param
    (
        [string]$Account,
        [string]$Password,
        [int]$UID,
        [int]$GID,
        [string]$FullName,
        [string]$Office,
        [string]$Extension,
        [string]$HomePhone,
        [string]$Email,
        [string]$Directory,
        [string]$Shell
    )

    $GECOS = [PSCustomObject]@{
        FullName  = $FullName
        Office    = $Office
        Extension = $Extension
        HomePhone = $HomePhone
        Email     = $Email
    }

    [PSCustomObject]@{
        Account   = $Account
        Password  = $Password
        UID       = $UID
        GID       = $GID
        GECOS     = $GECOS
        Directory = $Directory
        Shell     = $Shell
    }
}


function Import-File
{
    Param
    (
        [Parameter(Mandatory=$false,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true)]
        [string]
        $Path = ".\passwd.txt"
    )

    if (-not(Test-Path $Path))
    {
        throw [System.IO.FileNotFoundException]
    }

    $header = "Account","Password","UID","GID","GECOS","Directory","Shell"

    $csv = Import-Csv -Path $Path -Delimiter ":" -Header $header -Encoding ASCII
    $csv | ForEach-Object {
        New-Record -Account   $_.Account `
                   -Password  $_.Password `
                   -UID       $_.UID `
                   -GID       $_.GID `
                   -FullName  $_.GECOS.Split(",")[0] `
                   -Office    $_.GECOS.Split(",")[1] `
                   -Extension $_.GECOS.Split(",")[2] `
                   -HomePhone $_.GECOS.Split(",")[3] `
                   -Email     $_.GECOS.Split(",")[4] `
                   -Directory $_.Directory `
                   -Shell     $_.Shell
    }
}


function Export-File
{
    [CmdletBinding()]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true)]
        $InputObject,

        [Parameter(Mandatory=$false,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true)]
        [string]
        $Path = ".\passwd.txt"
    )

    Begin
    {
        if (-not(Test-Path $Path))
        {
            New-Item -Path . -Name $Path -ItemType File | Out-Null
        }

        [string]$recordString = "{0}:{1}:{2}:{3}:{4}:{5}:{6}"
        [string]$gecosString  = "{0},{1},{2},{3},{4}"
        [string[]]$lines = @()
        [string[]]$file  = Get-Content $Path
    }
    Process
    {
        foreach ($object in $InputObject)
        {
            $lines += $recordString -f $object.Account,
                                       $object.Password,
                                       $object.UID,
                                       $object.GID,
                                       $($gecosString -f $object.GECOS.FullName,
                                                         $object.GECOS.Office,
                                                         $object.GECOS.Extension,
                                                         $object.GECOS.HomePhone,
                                                         $object.GECOS.Email),
                                       $object.Directory,
                                       $object.Shell
        }
    }
    End
    {
        foreach ($line in $lines)
        {
            if (-not ($line -in $file))
            {
                $line | Out-File -FilePath $Path -Encoding ASCII -Append
            }
        }
    }
}

```

Create record objects.

```PowerShell

$records = @()

$records+= New-Record -Account   'jsmith' `
                      -Password  'x' `
                      -UID       1001 `
                      -GID       1000 `
                      -FullName  'Joe Smith' `
                      -Office    'Room 1007' `
                      -Extension '(234)555-8917' `
                      -HomePhone '(234)555-0077' `
                      -Email     'jsmith@rosettacode.org' `
                      -Directory '/home/jsmith' `
                      -Shell     '/bin/bash'

$records+= New-Record -Account   'jdoe' `
                      -Password  'x' `
                      -UID       1002 `
                      -GID       1000 `
                      -FullName  'Jane Doe' `
                      -Office    'Room 1004' `
                      -Extension '(234)555-8914' `
                      -HomePhone '(234)555-0044' `
                      -Email     'jdoe@rosettacode.org' `
                      -Directory '/home/jdoe' `
                      -Shell     '/bin/bash'

```

Display record objects.

```PowerShell

$records | Format-Table -AutoSize

```

{{Out}}

```txt

Account Password  UID  GID GECOS                                                                                                                   Directory    Shell
------- --------  ---  --- -----                                                                                                                   ---------    -----
jsmith  x        1001 1000 @{FullName=Joe Smith; Office=Room 1007; Extension=(234)555-8917; HomePhone=(234)555-0077; Email=jsmith@rosettacode.org} /home/jsmith /bin/bash
jdoe    x        1002 1000 @{FullName=Jane Doe; Office=Room 1004; Extension=(234)555-8914; HomePhone=(234)555-0044; Email=jdoe@rosettacode.org}    /home/jdoe   /bin/bash

```

Export records to file.

```PowerShell

if (-not(Test-FileLock -Path ".\passwd.txt"))
{
    $records | Export-File -Path ".\passwd.txt"
}

```

This is the file.

```PowerShell

Get-Content -Path ".\passwd.txt"

```

{{Out}}

```txt

jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash

```

Add a record to the record set.

```PowerShell

$records+= New-Record -Account   'xyz' `
                      -Password  'x' `
                      -UID       1003 `
                      -GID       1000 `
                      -FullName  'X Yz' `
                      -Office    'Room 1003' `
                      -Extension '(234)555-8913' `
                      -HomePhone '(234)555-0033' `
                      -Email     'xyz@rosettacode.org' `
                      -Directory '/home/xyz' `
                      -Shell     '/bin/bash'

```

Display record objects, sorted on last name; and display them.

```PowerShell

$records | Sort-Object { $_.GECOS.FullName.Split(" ")[1] } | Format-Table -AutoSize

```

{{Out}}

```txt

Account Password  UID  GID GECOS                                                                                                                   Directory    Shell
------- --------  ---  --- -----                                                                                                                   ---------    -----
jdoe    x        1002 1000 @{FullName=Jane Doe; Office=Room 1004; Extension=(234)555-8914; HomePhone=(234)555-0044; Email=jdoe@rosettacode.org}    /home/jdoe   /bin/bash
jsmith  x        1001 1000 @{FullName=Joe Smith; Office=Room 1007; Extension=(234)555-8917; HomePhone=(234)555-0077; Email=jsmith@rosettacode.org} /home/jsmith /bin/bash
xyz     x        1003 1000 @{FullName=X Yz; Office=Room 1003; Extension=(234)555-8913; HomePhone=(234)555-0033; Email=xyz@rosettacode.org}         /home/xyz    /bin/bash

```

Export records to file.

```PowerShell

if (-not(Test-FileLock -Path ".\passwd.txt"))
{
    $records | Export-File -Path ".\passwd.txt"
}

```

This is the file.

```PowerShell

Get-Content -Path ".\passwd.txt"

```

{{Out}}

```txt

jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```

<table class="wikitable" style="text-align: center; margin: 1em auto 1em auto;">
<caption> Append Capabilities.
</caption>
<tr>
<th colspan="2"> Data Representation
</th>
<th rowspan="2"> IO<br/>Library
</th>
<th rowspan="2"> Append<br/>Possible
</th>
<th rowspan="2"> Automatic<br/>Append
</th>
<th rowspan="2"> Multi-tasking<br/>Safe
</th></tr>
<tr>
<th> In core </th>
<th> On disk
</th></tr>
<tr>
<td> PSCustomObject </td>
<td> (CSV?) text file </td>
<td> Built-in (.NET) </td>
<td> ☑ </td>
<td> ☑ </td>
<td> ☑ (High probability)
</td></tr></table>


## Python

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| dict || CSV text file || builtin || ☑ || ☑ || ☒
|-
| instance || CSV text file || builtin ||colspan=3| To do.
|}
'''From a "dict" to a CSV File'''

```python
#############################
# Create a passwd text file
#############################
# note that UID & gid are of type "text"
passwd_list=[
  dict(account='jsmith', password='x', UID=1001, GID=1000, # UID and GID are type int
       GECOS=dict(fullname='Joe Smith', office='Room 1007', extension='(234)555-8917',
                  homephone='(234)555-0077', email='jsmith@rosettacode.org'),
                  directory='/home/jsmith', shell='/bin/bash'),
  dict(account='jdoe', password='x', UID=1002, GID=1000,
       GECOS=dict(fullname='Jane Doe', office='Room 1004', extension='(234)555-8914',
                  homephone='(234)555-0044', email='jdoe@rosettacode.org'),
       directory='/home/jdoe', shell='/bin/bash')
]

passwd_fields="account password UID GID GECOS directory shell".split()
GECOS_fields="fullname office extension homephone email".split()

def passwd_text_repr(passwd_rec):
# convert individual fields to string type
  passwd_rec["GECOS"]=",".join([ passwd_rec["GECOS"][field] for field in GECOS_fields])
  for field in passwd_rec: # convert "int" fields
    if not isinstance(passwd_rec[field], str):
      passwd_rec[field]=`passwd_rec[field]`
  return ":".join([ passwd_rec[field] for field in passwd_fields ])

passwd_text=open("passwd.txt","w")
for passwd_rec in passwd_list:
  print >> passwd_text,passwd_text_repr(passwd_rec)
passwd_text.close()

#################################
# Load text ready for appending
#################################
passwd_text=open("passwd.txt","a+")
new_rec=dict(account='xyz', password='x', UID=1003, GID=1000,
             GECOS=dict(fullname='X Yz', office='Room 1003', extension='(234)555-8913',
                        homephone='(234)555-0033', email='xyz@rosettacode.org'),
             directory='/home/xyz', shell='/bin/bash')
print >> passwd_text,  passwd_text_repr(new_rec)
passwd_text.close()

##############################################
# Finally reopen and check record was appended
##############################################
passwd_list=list(open("passwd.txt","r"))
if "xyz" in passwd_list[-1]:
  print "Appended record:",passwd_list[-1][:-1]
```

{{out}}

```txt
Appended record: xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
```



## Racket


The simplest format for such data in plain text is S-expression.

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| S-exprs || text file || builtin || ☑ || ☑ || ☑
|}


```racket

#lang racket

(define sample1
  '("jsmith" "x" 1001 1000
    ("Joe Smith" "Room 1007" "(234)555-8917" "(234)555-0077" "jsmith@rosettacode.org")
    "/home/jsmith" "/bin/bash"))

(define sample2
  '("jdoe" "x" 1002 1000
    ("Jane Doe" "Room 1004" "(234)555-8914" "(234)555-0044" "jdoe@rosettacode.org")
    "/home/jdoe" "/bin/bash"))

(define sample3
  '("xyz" "x" 1003 1000
    ("X Yz" "Room 1003" "(234)555-8913" "(234)555-0033" "xyz@rosettacode.org")
    "/home/xyz" "/bin/bash"))

(define passwd-file "sexpr-passwd")

(define (write-passwds mode . ps)
  (with-output-to-file passwd-file #:exists mode
    (λ() (for ([p (in-list ps)]) (printf "~s\n" p)))))

(define (lookup username)
  (with-input-from-file passwd-file
    (λ() (for/first ([p (in-producer read eof)]
                     #:when (equal? username (car p)))
           p))))

(printf "Creating file with two sample records.\n")
(write-passwds 'replace sample1 sample2)

(printf "Appending third sample.\n")
(write-passwds 'append sample3)

(printf "Looking up xyz in current file:\n=> ~s\n" (lookup "xyz"))

```


{{out}}

```txt

Creating file with two sample records.
Appending third sample.
Looking up xyz in current file:
=> ("xyz" "x" 1003 1000 ("X Yz" "Room 1003" "(234)555-8913" "(234)555-0033" "xyz@rosettacode.org") "/home/xyz" "/bin/bash")

```



## RapidQ

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| QObject || CSV text file || Standard lib || ☑ || ☑ || ☑
|}
'''The easy short solution'''

```vb

'Short solution: Append record and read last record
$Include "Rapidq.inc"

dim file as qfilestream
dim filename as string
dim LogRec as string

'First create our logfile
filename = "C:\Logfile2.txt"
file.open(filename, fmCreate)
file.writeline "jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash"
file.writeline "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash"
file.close

'Append record
file.open(filename, fmOpenWrite)
file.position = File.size
file.writeline "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash"
file.close

'Read last record
file.open (filename, fmOpenRead)
while not file.EOF
    LogRec = File.Readline
wend
file.close

showmessage "Appended record: " + LogRec

```

{{out}}

```txt
Appended record: xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
```


'''Full solution: create an object to handle all functions'''

```vb

'Full solution: Create an object with all required fields and
'build-in functions to append a record and to read the last record
$include "Rapidq.inc"

Type TLogFile extends QObject
  Private:
    file as qfilestream

  Public:
    account as string
    password as string
    UID as integer
    GID as integer
    GECOS.fullname as string
    GECOS.office as string
    GECOS.extension as string
    GECOS.homephone as string
    GECOS.email as string
    directory as string
    shell as string

    RSep as string
    GSep as string

    function AppendRecord(LogFile as string) as integer
        with This
            if fileexists(LogFile) then
                Result = .file.open(LogFile, fmOpenWrite)
            else
                Result = .file.open(LogFile, fmCreate)
            end if

            .file.position = .file.size
            .file.writeline .account + .RSep + .password + .RSep + str$(.UID) + .RSep + str$(.GID) + .RSep +_
                            .GECOS.fullname + .GSep + .GECOS.office + .GSep + .GECOS.extension +_
                            .GSep + .GECOS.homephone + .GSep + .GECOS.email + .RSep +_
                            .directory + .RSep + .shell
            .file.close
        end with
    end function

    Function ReadLastRecord(LogFile as string) as string
        dim x as integer
        dim LogRec as string
        dim GECOSRec as string

        With This
            if fileexists(LogFile) then
                .file.open(LogFile, fmOpenRead)
                While not .file.eof
                    LogRec = .file.readline
                Wend
                .file.close

                .account = field$(LogRec, .RSep, 1)
                .password = field$(LogRec, .RSep, 2)
                .UID = val(field$(LogRec, .RSep, 3))
                .GID = val(field$(LogRec, .RSep, 4))
                GECOSRec = field$(LogRec, .RSep, 5)
                .directory = field$(LogRec, .RSep, 6)
                .shell = field$(LogRec, .RSep, 7)
                .GECOS.fullname = field$(GECOSRec, .GSep, 1)
                .GECOS.office = field$(GECOSRec, .GSep, 2)
                .GECOS.extension = field$(GECOSRec, .GSep, 3)
                .GECOS.homephone = field$(GECOSRec, .GSep, 4)
                .GECOS.email = field$(GECOSRec, .GSep, 5)

            else
                showmessage "Can't read file " + Logfile

            end if
        end with
    End function

    Constructor
        RSep = ":"
        GSep = ","
    End Constructor
end type

'--- Now we can use our LogFile object:
dim LogFile as TLogFile

'--- Let's save a record: Set field values
Logfile.account = "jsmith"
Logfile.password = "X"
Logfile.UID = 1001
Logfile.GID = 1000
Logfile.directory = "/home/jsmith"
Logfile.shell = "/bin/bash"
Logfile.GECOS.fullname = "Joe Smith"
Logfile.GECOS.office = "Room 1007"
Logfile.GECOS.extension = "(234)555-8917"
Logfile.GECOS.homephone = "(234)555-0077"
Logfile.GECOS.email = "jsmith@rosettacode.org"
'--- And save it to our logfile
Logfile.appendrecord("c:\A test.txt")

'--- Let's save the second one: Set field values
Logfile.account = "jdoe"
Logfile.password = "X"
Logfile.UID = 1002
Logfile.GID = 1000
Logfile.directory = "/home/jsmith"
Logfile.shell = "/bin/bash"
Logfile.GECOS.fullname = "Jane Doe"
Logfile.GECOS.office = "Room 1004"
Logfile.GECOS.extension = "(234)555-8914"
Logfile.GECOS.homephone = "(234)555-0044"
Logfile.GECOS.email = "jdoe@rosettacode.org"
'--- And save it to our logfile
Logfile.appendrecord("c:\A test.txt")

'--- And append the last one: Set field values
Logfile.account = "xyz"
Logfile.password = "X"
Logfile.UID = 1003
Logfile.GID = 1000
Logfile.directory = "/home/xyz"
Logfile.shell = "/bin/bash"
Logfile.GECOS.fullname = "X Yz"
Logfile.GECOS.office = "Room 1003"
Logfile.GECOS.extension = "(234)555-8913"
Logfile.GECOS.homephone = "(234)555-0033"
Logfile.GECOS.email = "xyz@rosettacode.org"
'--- And save it to our logfile
Logfile.appendrecord("c:\A test.txt")


'--- Read last record: load all CSV fields in our LogFile object props
Logfile.ReadLastRecord("c:\A test.txt")
'--- And simply Read values from the objects properties
Print Logfile.account
Print Logfile.password
Print Logfile.UID
Print Logfile.GID
Print Logfile.directory
Print Logfile.shell
Print Logfile.GECOS.fullname
Print Logfile.GECOS.office
Print Logfile.GECOS.extension
Print Logfile.GECOS.homephone
Print Logfile.GECOS.email
Print ""
input "Press enter to exit:";a$

```



## REXX

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities
|-
!colspan=2| data representation
!rowspan=2| I/O<BR>library
!rowspan=2| append<BR>
!rowspan=2| automatic<BR>append
!rowspan=2| multi-tasking<BR>safe
|-
! in memory || on disk
|-
| strings || text file || builtin || yes || yes || yes
|}
The data fields for the three records were coded on two statements instead of

continuing them on separate statements for brevity.

```rexx
/*REXX program  writes (appends) two records,  closes the file,  appends another record.*/
tFID= 'PASSWD.TXT'                               /*define the name of the  output  file.*/
call lineout tFID                                /*close the output file,  just in case,*/
                                                 /*   it could be open from calling pgm.*/
call writeRec tFID,,                             /*append the  1st record  to the file. */
   'jsmith',"x", 1001, 1000, 'Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org', "/home/jsmith", '/bin/bash'

call writeRec tFID,,                             /*append the  2nd record  to the file. */
   'jdoe',  "x", 1002, 1000, 'Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org',    "/home/jsmith", '/bin/bash'

call lineout fid                                 /*close the outfile  (just to be tidy).*/

call writeRec tFID,,                             /*append the  3rd record  to the file. */
   'xyz',   "x", 1003, 1000, 'X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org',         "/home/xyz",    '/bin/bash'
/*─account─pw────uid───gid──────────────fullname,office,extension,homephone,Email────────────────────────directory───────shell──*/

call lineout fid                                 /*"be safe" programming: close the file*/
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
s:   if arg(1)==1  then return arg(3);       return word(arg(2) 's', 1)     /*pluralizer*/
/*──────────────────────────────────────────────────────────────────────────────────────*/
writeRec: parse arg fid,_                        /*get the fileID, and also the 1st arg.*/
          sep=':'                                /*field delimiter used in file, it ··· */
                                                 /*      ··· can be unique and any size.*/
                       do i=3  to arg()          /*get each argument and append it to   */
                       _=_ || sep || arg(i)      /*  the previous arg, with a   :   sep.*/
                       end   /*i*/

                       do tries=0  for 11        /*keep trying for  66  seconds.        */
                       r=lineout(fid, _)         /*write (append)  the new record.      */
                       if r==0  then return      /*Zero?   Then record was written.     */
                       call sleep tries          /*Error?  So try again after a delay.  */
                       end   /*tries*/           /*Note:  not all REXXes have  SLEEP.   */

          say '***error***';  say r  'record's(r)   "not written to file"   fid;   exit 13
          /*some error causes: no write access, disk is full, file lockout, no authority*/
```

'''PASSWD.TXT''' file before the REXX program ran:

```txt

:::::::::::::::::::::::::::::::::::::::: Hopefully, this will be considered a comment.

```

'''PASSWD.TXT''' file after the REXX program ran:

```txt

:::::::::::::::::::::::::::::::::::::::: Hopefully, this will be considered a comment.
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```



## Ruby

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| objects (subclass of Struct builtin) || text file || builtin || ☑ || ☑ || ☒
|}

```ruby
Gecos = Struct.new :fullname, :office, :extension, :homephone, :email
class Gecos
  def to_s
    "%s,%s,%s,%s,%s" % to_a
  end
end

# Another way define 'to_s' method
Passwd = Struct.new(:account, :password, :uid, :gid, :gecos, :directory, :shell) do
  def to_s
    to_a.join(':')
  end
end

jsmith = Passwd.new('jsmith','x',1001, 1000, Gecos.new('Joe Smith', 'Room 1007', '(234)555-8917', '(234)555-0077', 'jsmith@rosettacode.org'), '/home/jsmith', '/bin/bash')
jdoe = Passwd.new('jdoe','x',1002, 1000, Gecos.new('Jane Doe', 'Room 1004', '(234)555-8914', '(234)555-0044', 'jdoe@rosettacode.org'), '/home/jdoe', '/bin/bash')
xyz = Passwd.new('xyz','x',1003, 1000, Gecos.new('X Yz', 'Room 1003', '(234)555-8913', '(234)555-0033', 'xyz@rosettacode.org'), '/home/xyz', '/bin/bash')

filename = 'append.records.test'

# create the passwd file with two records
File.open(filename, 'w') do |io|
  io.puts jsmith
  io.puts jdoe
end

puts "before appending:"
puts File.readlines(filename)

# append the third record
File.open(filename, 'a') do |io|
  io.puts xyz
end

puts "after appending:"
puts File.readlines(filename)
```

{{out}}

```txt

before appending:
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
after appending:
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```


## Rust


```rust

use std::fs::File;
use std::fs::OpenOptions;
use std::io::BufRead;
use std::io::BufReader;
use std::io::BufWriter;
use std::io::Result;
use std::io::Write;
use std::path::Path;

/// Password record with all fields
#[derive(Eq, PartialEq, Debug)]
pub struct PasswordRecord {
    pub account: String,
    pub password: String,
    pub uid: u64,
    pub gid: u64,
    pub gecos: Vec<String>,
    pub directory: String,
    pub shell: String,
}


impl PasswordRecord {
    /// new instance, cloning all fields
    pub fn new(
        account: &str,
        password: &str,
        uid: u64,
        gid: u64,
        gecos: Vec<&str>,
        directory: &str,
        shell: &str,
    ) -> PasswordRecord {
        PasswordRecord {
            account: account.to_string(),
            password: password.to_string(),
            uid,
            gid,
            gecos: gecos.iter().map(|s| s.to_string()).collect(),
            directory: directory.to_string(),
            shell: shell.to_string(),
        }
    }

    /// convert to one line string
    pub fn to_line(&self) -> String {
        let gecos = self.gecos.join(",");
        format!(
            "{}:{}:{}:{}:{}:{}:{}",
            self.account, self.password, self.uid, self.gid, gecos, self.directory, self.shell
        )
    }

    /// read record from line
    pub fn from_line(line: &str) -> PasswordRecord {
        let sp: Vec<&str> = line.split(":").collect();
        if sp.len() < 7 {
            panic!("Less than 7 fields found");
        } else {
            let uid = sp[2].parse().expect("Cannot parse uid");
            let gid = sp[3].parse().expect("Cannot parse gid");
            let gecos = sp[4].split(",").collect();
            PasswordRecord::new(sp[0], sp[1], uid, gid, gecos, sp[5], sp[6])
        }
    }
}

/// read all records from file
pub fn read_password_file(file_name: &str) -> Result<Vec<PasswordRecord>> {
    let p = Path::new(file_name);
    if !p.exists() {
        Ok(vec![])
    } else {
        let f = OpenOptions::new().read(true).open(p)?;
        Ok(BufReader::new(&f)
            .lines()
            .map(|l| PasswordRecord::from_line(&l.unwrap()))
            .collect())
    }
}

/// overwrite file with records
pub fn overwrite_password_file(file_name: &str, recs: &Vec<PasswordRecord>) -> Result<()> {
    let f = OpenOptions::new()
        .create(true)
        .write(true)
        .open(file_name)?;
    write_records(f, recs)
}

/// append records to file
pub fn append_password_file(file_name: &str, recs: &Vec<PasswordRecord>) -> Result<()> {
    let f = OpenOptions::new()
        .create(true)
        .append(true)
        .open(file_name)?;
    write_records(f, recs)
}

/// internal, write records line by line
fn write_records(f: File, recs: &Vec<PasswordRecord>) -> Result<()> {
    let mut writer = BufWriter::new(f);
    for rec in recs {
        write!(writer, "{}\n", rec.to_line())?;
    }
    Ok(())
}

fn main(){
    let recs1 = vec![
            PasswordRecord::new(
                "jsmith",
                "x",
                1001,
                1000,
                vec![
                    "Joe Smith",
                    "Room 1007",
                    "(234)555-8917",
                    "(234)555-0077",
                    "jsmith@rosettacode.org",
                ],
                "/home/jsmith",
                "/bin/bash",
            ),
            PasswordRecord::new(
                "jdoe",
                "x",
                1002,
                1000,
                vec![
                    "Jane Doe",
                    "Room 1004",
                    "(234)555-8914",
                    "(234)555-0044",
                    "jdoe@rosettacode.org",
                ],
                "/home/jdoe",
                "/bin/bash",
            ),
        ];

    overwrite_password_file("passwd", &recs1).expect("cannot write file");
    let recs2 = read_password_file("passwd").expect("cannot read file");
    println!("Original file:");
    for r in recs2 {
        println!("{}",r.to_line());
    }
    let append0 = vec![PasswordRecord::new(
            "xyz",
            "x",
            1003,
            1000,
            vec![
                "X Yz",
                "Room 1003",
                "(234)555-8913",
                "(234)555-0033",
                "xyz@rosettacode.org",
            ],
            "/home/xyz",
            "/bin/bash",
        )];
    append_password_file("passwd", &append0).expect("cannot append to file");
    let recs2 = read_password_file("passwd").expect("cannot read file");
    println!("");
    println!("Appended file:");
    for r in recs2 {
        println!("{}",r.to_line());
    }
}

```

{{out}}

```txt

Original file:
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash

Appended file:
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```



## Scala

{{libheader|Scala}}
```scala
import java.io.{File, FileWriter, IOException}
import scala.io.Source

object RecordAppender extends App {
  val rawDataIt = Source.fromString(rawData).getLines()

  def writeStringToFile(file: File, data: String, appending: Boolean = false) =
    using(new FileWriter(file, appending))(_.write(data))

  def using[A <: {def close() : Unit}, B](resource: A)(f: A => B): B =
    try f(resource) finally resource.close()

  def rawData =
    """jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
      |jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
      |xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash""".stripMargin

  case class Record(account: String,
                    password: String,
                    uid: Int,
                    gid: Int,
                    gecos: Array[String],
                    directory: String,
                    shell: String) {
    def asLine: String = s"$account:$password:$uid:$gid:${gecos.mkString(",")}:$directory:$shell\n"
  }

  object Record {
    def apply(line: String): Record = {
      val token = line.trim.split(":")
      require((token != null) || (token.length == 7))
      this(token(0).trim,
        token(1).trim,
        Integer.parseInt(token(2).trim),
        Integer.parseInt(token(3).trim),
        token(4).split(","),
        token(5).trim,
        token(6).trim)
    }
  }

  try {
    val file = File.createTempFile("_rosetta", ".passwd")
    using(new FileWriter(file))(writer => rawDataIt.take(2).foreach(line => writer.write(Record(line).asLine)))

    writeStringToFile(file, Record(rawDataIt.next()).asLine, appending = true) // Append a record

    Source.fromFile(file).getLines().foreach(line => {
      if (line startsWith """xyz""") print(s"Selected record: ${Record(line).asLine}")
    })
    scala.compat.Platform.collectGarbage() // JVM Windows related bug workaround JDK-4715154
    file.deleteOnExit()
  } catch {
    case e: IOException => println(s"Running Example failed: ${e.getMessage}")
  }
} // 57 lines
```



## Sidef

{{trans|Perl}}
{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| Hash table || Text file || built-in || ☑ || ☑ || Advisory lock
|}


```ruby
define (
    RECORD_FIELDS = %w(account password UID GID GECOS directory shell),
    GECOS_FIELDS  = %w(fullname office extension homephone email),
    RECORD_SEP    = ':',
    GECOS_SEP     = ',',
    PASSWD_FILE   = 'passwd.txt',
)

# here's our three records
var records_to_write = [
    Hash(
        account  => 'jsmith',
        password => 'x',
        UID      => 1001,
        GID      => 1000,
        GECOS    => Hash(
            fullname  => 'John Smith',
            office    => 'Room 1007',
            extension => '(234)555-8917',
            homephone => '(234)555-0077',
            email     => 'jsmith@rosettacode.org',
        ),
        directory => '/home/jsmith',
        shell     => '/bin/bash',
    ),
    Hash(
        account  => 'jdoe',
        password => 'x',
        UID      => 1002,
        GID      => 1000,
        GECOS    => Hash(
            fullname  => 'Jane Doe',
            office    => 'Room 1004',
            extension => '(234)555-8914',
            homephone => '(234)555-0044',
            email     => 'jdoe@rosettacode.org',
        ),
        directory => '/home/jdoe',
        shell     => '/bin/bash',
    ),
];

var record_to_append = Hash(
    account  => 'xyz',
    password => 'x',
    UID      => 1003,
    GID      => 1000,
    GECOS    => Hash(
        fullname  => 'X Yz',
        office    => 'Room 1003',
        extension => '(234)555-8913',
        homephone => '(234)555-0033',
        email     => 'xyz@rosettacode.org',
    ),
    directory => '/home/xyz',
    shell     => '/bin/bash',
);

func record_to_string(rec, sep = RECORD_SEP, fields = RECORD_FIELDS) {
    gather {
        fields.each { |field|
            var r = rec{field} \\ die "Field #{field} not found"
            take(field == 'GECOS' ? record_to_string(r, GECOS_SEP, GECOS_FIELDS)
                                  : r)
        }
    }.join(sep)
}

func write_records_to_file(records, filename = PASSWD_FILE, append = false) {
    File(filename).(append ? :open_a : :open_w)(\var fh, \var err)
    err && die "Can't open #{filename}: #{err}";
    fh.flock(File.LOCK_EX) || die "Can't lock #{filename}: $!"
    fh.seek(0, File.SEEK_END) || die "Can't seek #{filename}: $!"
    records.each { |record| fh.say(record_to_string(record)) }
    fh.flock(File.LOCK_UN) || die "Can't unlock #{filename}: $!"
    fh.close
}

# write two records to file
write_records_to_file(records: records_to_write);

# append one more record to file
write_records_to_file(records: [record_to_append], append: true);

# test

File(PASSWD_FILE).open_r(\var fh, \var err)
err && die "Can't open file #{PASSWD_FILE}: #{err}"
var lines = fh.lines

# There should be more than one line
assert(lines.len > 1)

# Check the last line
assert_eq(lines[-1], 'xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,' +
                     '(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash')

say "** Test passed!"
```


Note that flock uses advisory lock; some other program (if it doesn't use flock) can still unexpectedly write to the file.


## Tcl

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| nested lists || Colon/Comma-separated text file || builtin || ☑ || ☑ || ☑
|}
Note that appending is only safe on POSIX OSes where the data is written in “small enough” amounts to a local disk. This is a limitation of the OS APIs.

```tcl
# Model the data as nested lists, as that is a natural fit for Tcl
set basicRecords {
    {
	jsmith
	x
	1001
	1000
	{
	    {Joe Smith}
	    {Room 1007}
	    (234)555-8917
	    (234)555-0077
	    jsmith@rosettacode.org
	}
	/home/jsmith
	/bin/bash
    }
    {
	jdoe
	x
	1002
	1000
	{
	    {Jane Doe}
	    {Room 1004}
	    (234)555-8914
	    (234)555-0044
	    jdoe@rosettacode.org
	}
	/home/jsmith
	/bin/bash
    }
}
set addedRecords {
    {
	xyz
	x
	1003
	1000
	{
	    {X Yz}
	    {Room 1003}
	    (234)555-8913
	    (234)555-0033
	    xyz@rosettacode.org
	}
	/home/xyz
	/bin/bash
    }
}

proc printRecords {records fd} {
    fconfigure $fd -buffering none
    foreach record $records {
	lset record 4 [join [lindex $record 4] ","]
	puts -nonewline $fd [join $record ":"]\n
    }
}
proc readRecords fd {
    set result {}
    foreach line [split [read $fd] "\n"] {
	if {$line eq ""} continue
	set record [split $line ":"]
	# Special handling for GECOS
	lset record 4 [split [lindex $record 4] ","]
	lappend result $record
    }
    return $result
}

# Write basic set
set f [open ./passwd w]
printRecords $basicRecords $f
close $f

# Append the extra ones
# Use {WRONLY APPEND} on Tcl 8.4
set f [open ./passwd a]
printRecords $addedRecords $f
close $f

set f [open ./passwd]
set recs [readRecords $f]
close $f
puts "last record is for [lindex $recs end 0], named [lindex $recs end 4 0]"
```
{{out}}
 last record is for xyz, named X Yz
The file will have this content:
 jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
 jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jsmith:/bin/bash
 xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash


## UNIX Shell

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| one-dimensional arrays (indexed or associative) || text file || builtin (shell redirections) || ☑ || ☑ || OS defined
|}

```bash
rec1=(
    jsmith
    x
    1001
    1000
    "Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org"
    /home/jsmith
    /bin/bash
)

rec2=(
    jdoe
    x
    1002
    1000
    "Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org"
    /home/jdoe
    /bin/bash
)

rec3=(
    xyz
    x
    1003
    1000
    "X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org"
    /home/xyz
    /bin/bash
)

filename=./passwd-ish

# use parentheses to run the commands in a subshell, so the
# current shell's IFS variable is not changed
(
    IFS=:
    echo "${rec1[*]}"
    echo "${rec2[*]}"
) > "$filename"

echo before appending:
cat "$filename"

# appending, use the ">>" redirection symbol
IFS=:
echo "${rec3[*]}" >> "$filename"

echo after appending:
cat "$filename"
```


{{output}}

```txt
before appending:
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
after appending:
jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
```



## Ursa

{{trans|Awk}}

```ursa
# ursa appends to files by default when the out function is used

# create new passwd in working directory
decl file f
f.create "passwd"
f.open "passwd"
out "account:password:UID:GID:fullname,office,extension,homephone,email:directory:shell" endl f
out "jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash" endl f
out "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash" endl f
f.close

# display the created file
f.open "passwd"
out "initial file:" endl console
while (f.hasline)
	out (in string f) endl console
end while

# append the new record
out "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash" endl f
f.close

# output the new file contents
f.open "passwd"
out endl endl "file after append:" endl console
while (f.hasline)
	out (in string f) endl console
end while
```



## Visual Basic .NET

{{trans|C#}}

```vbnet
Imports System.IO

Module Module1

    Class PasswordRecord
        Public account As String
        Public password As String
        Public fullname As String
        Public office As String
        Public extension As String
        Public homephone As String
        Public email As String
        Public directory As String
        Public shell As String
        Public UID As Integer
        Public GID As Integer

        Public Sub New(account As String, password As String, UID As Integer, GID As Integer, fullname As String, office As String, extension As String, homephone As String, email As String, directory As String, shell As String)
            Me.account = account
            Me.password = password
            Me.UID = UID
            Me.GID = GID
            Me.fullname = fullname
            Me.office = office
            Me.extension = extension
            Me.homephone = homephone
            Me.email = email
            Me.directory = directory
            Me.shell = shell
        End Sub

        Public Overrides Function ToString() As String
            Dim gecos = String.Join(",", New String() {fullname, office, extension, homephone, email})
            Return String.Join(":", New String() {account, password, UID.ToString(), GID.ToString(), gecos, directory, shell})
        End Function
    End Class

    Sub Main()
        Dim jsmith As New PasswordRecord("jsmith", "x", 1001, 1000, "Joe Smith", "Room 1007", "(234)555-8917", "(234)555-0077", "jsmith@rosettacode.org", "/home/jsmith", "/bin/bash")
        Dim jdoe As New PasswordRecord("jdoe", "x", 1002, 1000, "Jane Doe", "Room 1004", "(234)555-8914", "(234)555-0044", "jdoe@rosettacode.org", "/home/jdoe", "/bin/bash")
        Dim xyz As New PasswordRecord("xyz", "x", 1003, 1000, "X Yz", "Room 1003", "(234)555-8913", "(234)555-0033", "xyz@rosettacode.org", "/home/xyz", "/bin/bash")

        ' Write these records out in the typical system format.
        File.WriteAllLines("passwd.txt", New String() {jsmith.ToString(), jdoe.ToString()})

        ' Append a new record to the file and close the file again.
        File.AppendAllText("passwd.txt", xyz.ToString())

        ' Open the file and demonstrate the new record has indeed been written to the end.
        Dim lines = File.ReadAllLines("passwd.txt")
        Console.WriteLine("Appended record: {0}", lines(2))
    End Sub

End Module
```

{{out}}

```txt
Appended record: xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash
```



## Yabasic


```Yabasic
a = open("passwd", "a") // Open the file for appending, i.e. what you write to the file will be appended after its initial contents.
                        // If the file does not exist, it will be created.

print #a "account:password:UID:GID:fullname,office,extension,homephone,email:directory:shell"
print #a "jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash"
print #a "jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash"

close #a

a = open("passwd", "a")

if not a error "Could not open 'passwd' for appending"

print #a "xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash"

close #a

if (not open(a,"passwd")) error "Could not open 'passwd' for reading"

dim nameField$(1), contentField$(1)

line input #a a$

n = token(a$, nameField$(), ":,")
for i = 1 to n
    if nameField$(i) = "account" field1 = i
    if nameField$(i) = "homephone" field2 = i
next

print upper$(nameField$(field1)), "\t", upper$(nameField$(field2))
print

while(not eof(#a))
  line input #a a$
  n = token(a$, contentField$(), ":,")
  print contentField$(field1), "\t", contentField$(field2)
wend

close #a
```



## zkl

{|class="wikitable" style="text-align: center; margin: 1em auto 1em auto;"
|+ Append Capabilities.
|-
!colspan=2| Data Representation
!rowspan=2| IO<BR>Library
!rowspan=2| Append<BR>Possible
!rowspan=2| Automatic<BR>Append
!rowspan=2| Multi-tasking<BR>Safe
|-
! In core || On disk
|-
| classes || text file || libc || ☑ || ☑ || OS defined
|}
{{trans|Ruby}}

```zkl
var [const]
   gnames=T("fullname","office","extension","homephone","email"),
   pnames=T("account","password","uid","gid","gecos","directory","shell");

class Gecos{
   var fullname, office, extension, homephone, email;
   fcn init(str){ gnames.zipWith(setVar,vm.arglist) }
   fcn toString { gnames.apply(setVar).concat(",")  }
}
class Passwd{
   var account,password,uid,gid,gecos,directory,shell;
   fcn init(str){ pnames.zipWith(setVar,vm.arglist) }
   fcn toString { pnames.apply(setVar).concat(":")  }
}
```

The class setVar method takes one or two parameters. With two, it sets the named class variable to a value; with one, it gets the var.
If there aren't enough parameters, the missing ones with be set to Void (yeah, poor error checking).

```zkl
fcn strToPasswd(str){  // blow apart file line to class
   p:=str.strip().split(":");
   g:=Gecos(p[4].split(",").xplode());
   Passwd(p[0,4].xplode(),g,p[5,*].xplode());
}
```

The List xplode method pushes the list contents to the parameter list.

```zkl
jsmith:=Passwd("jsmith","x",1001, 1000,
	Gecos("Joe Smith", "Room 1007", "(234)555-8917", "(234)555-0077", "jsmith@rosettacode.org"),
	"/home/jsmith", "/bin/bash");
jdoe:=strToPasswd("jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,"
                  "jdoe@rosettacode.org:/home/jdoe:/bin/bash");
xyz:=strToPasswd("xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,"
                 "xyz@rosettacode.org:/home/xyz:/bin/bash");

filename:="append.records.test";
f:=File(filename,"w");	// create file with 2 records
f.writeln(jsmith,"\n",jdoe); f.close();

f:=File(filename,"a+");	// append a third record
f.writeln(xyz); f.close();

File(filename).read().text.print();  // print file
```

{{out}}

```txt

jsmith:x:1001:1000:Joe Smith,Room 1007,(234)555-8917,(234)555-0077,jsmith@rosettacode.org:/home/jsmith:/bin/bash
jdoe:x:1002:1000:Jane Doe,Room 1004,(234)555-8914,(234)555-0044,jdoe@rosettacode.org:/home/jdoe:/bin/bash
xyz:x:1003:1000:X Yz,Room 1003,(234)555-8913,(234)555-0033,xyz@rosettacode.org:/home/xyz:/bin/bash

```

