+++
title = "FTP"
description = ""
date = 2019-07-28T14:33:43Z
aliases = []
[extra]
id = 17673
[taxonomies]
categories = []
tags = []
+++

{{task|Programming environment operations}}[[Category:Networking and Web Interaction]]

;Task
Connect to a server, change directory, list its contents and download a file as binary using the FTP protocol. Use passive mode if available.
<br/><br/>


## Batch File

This uses the native FTP.EXE in Windows. I am not sure, but I think FTP.EXE client does not support passive mode.

```dos
::Playing with FTP
::Batch File Implementation

@echo off

set site="ftp.hq.nasa.gov"
set user="anonymous"
set pass="ftptest@example.com"
set dir="pub/issoutreach/Living in Space Stories (MP3 Files)"
set download="Gravity in the Brain.mp3"

(
	echo.open %site%
	echo.user %user% %pass%
	echo.dir
	echo.!echo.
	echo.!echo.This is a just a text to seperate two directory listings. 
	echo.!echo.
	echo.cd %dir%
	echo.dir
	echo.binary
	echo.get %download%
	echo.disconnect
)|ftp -n
```

{{Out}}

```txt
\Desktop>RCFTP
-rw-r--r--   1 ftpadmin ftp-adm      3997 May 26  1998 README
drwxrwx-wx   6 lgipson  armd       696320 Jan 23  2015 armd
drwxrwx-wx   2 chmgt    ftp-adm      4096 Aug 18 16:17 chmgt
-r-xr-xr-x   1 root     root        18120 Nov 28  2001 ftp-exec
drwxrws-wx   2 ftpadmin ftp-adm     57344 Aug 18 13:08 incoming
-rw-rw-r--   1 ftpadmin ftp-adm       133 Jan 29  1996 index.html
drwx------   2 root     root         4096 Apr 11  2003 lost+found
drwxr-sr-x   2 ftpadmin ftp-adm      4096 Apr 14  1998 office
drwxrwsr-x  17 ftpadmin ftp-adm      4096 Nov  4  2013 pub
-rw-r--r--   1 root     ftp-adm        26 Jan 27  2011 robots.txt

This is a just a text to seperate two directory listings.

-rw-rw-r--   1 109      space-station  2327118 May  9  2005 09sept_spacepropulsion.mp3
-rw-rw-r--   1 109      space-station  1260304 May  9  2005 Can People go to Mars.mp3
-rw-rw-r--   1 109      space-station  1350270 May  9  2005 Distill some water.mp3
-rw-rw-r--   1 109      space-station  1290888 May  9  2005 Good Vibrations.mp3
-rw-rw-r--   1 109      space-station  1431834 May  9  2005 Gravity Hurts_So good.mp3
-rw-rw-r--   1 109      space-station  1072644 May  9  2005 Gravity in the Brain.mp3
-rw-rw-r--   1 109      space-station  1230594 May  9  2005 Power to the ISS.mp3
-rw-rw-r--   1 109      space-station  1309062 May  9  2005 Space Bones.mp3
-rw-rw-r--   1 109      space-station  2292715 May  9  2005 Space Power.mp3
-rw-rw-r--   1 109      space-station   772075 May  9  2005 We have a solution.mp3
-rw-rw-r--   1 109      space-station  1134654 May  9  2005 When Space Makes you Dizzy.mp3

\Desktop>
```



## C

Using [http://nbpfaus.net/~pfau/ftplib/ ftplib]

```c

#include <ftplib.h>

int main(void)
{
    netbuf *nbuf;

    FtpInit();
    FtpConnect("kernel.org", &nbuf);
    FtpLogin("anonymous", "", nbuf);
    FtpOptions(FTPLIB_CONNMODE, FTPLIB_PASSIVE, nbuf);
    FtpChdir("pub/linux/kernel", nbuf);
    FtpDir((void*)0, ".", nbuf);
    FtpGet("ftp.README", "README", FTPLIB_ASCII, nbuf);
    FtpQuit(nbuf);

    return 0;
}

```



## Common Lisp


Using package [http://code.kepibu.org/cl-ftp/ cl-ftp].


```lisp
(use-package :ftp)

(with-ftp-connection (conn :hostname "ftp.hq.nasa.gov" 
                           :passive-ftp-p t)
  (send-cwd-command conn "/pub/issoutreach/Living in Space Stories (MP3 Files)")
  (send-list-command conn t)
  (let ((filename "Gravity in the Brain.mp3"))
    (retrieve-file conn filename filename :type :binary)))

```

{{Out}}

```txt

-rw-rw-r--   1 109      space-station  2327118 May  9  2005 09sept_spacepropulsion.mp3
-rw-rw-r--   1 109      space-station  1260304 May  9  2005 Can People go to Mars.mp3
-rw-rw-r--   1 109      space-station  1350270 May  9  2005 Distill some water.mp3
-rw-rw-r--   1 109      space-station  1290888 May  9  2005 Good Vibrations.mp3
-rw-rw-r--   1 109      space-station  1431834 May  9  2005 Gravity Hurts_So good.mp3
-rw-rw-r--   1 109      space-station  1072644 May  9  2005 Gravity in the Brain.mp3
-rw-rw-r--   1 109      space-station  1230594 May  9  2005 Power to the ISS.mp3
-rw-rw-r--   1 109      space-station  1309062 May  9  2005 Space Bones.mp3
-rw-rw-r--   1 109      space-station  2292715 May  9  2005 Space Power.mp3
-rw-rw-r--   1 109      space-station   772075 May  9  2005 We have a solution.mp3
-rw-rw-r--   1 109      space-station  1134654 May  9  2005 When Space Makes you Dizzy.mp3

```



## Go

Using the FTP package from [https://godoc.org/github.com/stacktic/ftp github.com/stacktic/ftp].

```go
package main

import (
	"fmt"
	"io"
	"log"
	"os"

	"github.com/stacktic/ftp"
)

func main() {
	// Hard-coded demonstration values
	const (
		hostport = "localhost:21"
		username = "anonymous"
		password = "anonymous"
		dir      = "pub"
		file     = "somefile.bin"
	)

	conn, err := ftp.Connect(hostport)
	if err != nil {
		log.Fatal(err)
	}
	defer conn.Quit()
	fmt.Println(conn)

	if err = conn.Login(username, password); err != nil {
		log.Fatal(err)
	}
	if err = conn.ChangeDir(dir); err != nil {
		log.Fatal(err)
	}
	fmt.Println(conn.CurrentDir())
	files, err := conn.List(".")
	if err != nil {
		log.Fatal(err)
	}
	for _, f := range files {
		fmt.Printf("%v %12d %v %v\n", f.Time, f.Size, f.Type, f.Name)
	}

	r, err := conn.Retr(file)
	if err != nil {
		log.Fatal(err)
	}
	defer r.Close()

	f, err := os.Create(file)
	if err != nil {
		log.Fatal(err)
	}
	defer f.Close()

	n, err := io.Copy(f, r)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("Wrote", n, "bytes to", file)
}
```



## Groovy

This is the code from [https://gist.github.com/ran488/1135043 Ran488 GitHub], modified to be executable.
It relies on external Apache FTP client.
Dependencies are automatically loaded with the @Grab annotation.
let's say the code is saved in the file ftpTest.groovy:

```Groovy
   
@Grab(group='commons-net', module='commons-net', version='2.0')
import org.apache.commons.net.ftp.FTPClient

println("About to connect....");
new FTPClient().with {
    connect "ftp.easynet.fr"
    enterLocalPassiveMode()
    login "anonymous", "ftptest@example.com"
    changeWorkingDirectory "/debian/"
    def incomingFile = new File("README.html")
    incomingFile.withOutputStream { ostream -> retrieveFile "README.html", ostream }
    disconnect()
}
println("                      ...Done.");

```

By typing groovy ftpTest.groovy, you should see a README.html file on your directory.

```txt

Debian Archive

See http://www.debian.org/ for information about Debian GNU/Linux.

Current Releases

Four Debian releases are available on the main site:

Debian 6.0.10, or squeeze
Debian 6.0.10 was released Saturday, 19th July 2014. Please note that the 6.0 distribution is no longer receiving security support. If you are using the amd64 or i386 architecture and not able to upgrade to the current stable release, you may wish to investigate the "squeeze-lts" distribution. Installation and upgrading instructions, More information
....
...

```



## Haskell


Example uses [https://hackage.haskell.org/package/ftphs <tt>ftphs</tt>] package:


```haskell
module Main (main) where

import           Control.Exception (bracket)
import           Control.Monad (void)
import           Data.Foldable (for_)
import           Network.FTP.Client
                    ( cwd
                    , easyConnectFTP
                    , getbinary
                    , loginAnon
                    , nlst
                    , quit
                    , setPassive
                    )

main :: IO ()
main = bracket ((flip setPassive) True <$> easyConnectFTP "ftp.kernel.org") quit $ \h -> do
    -- Log in anonymously
    void $ loginAnon h

    -- Change directory
    void $ cwd h "/pub/linux/kernel/Historic"

    -- List current directory
    fileNames <- nlst h Nothing
    for_ fileNames $ \fileName ->
        putStrLn fileName

    -- Download in binary mode
    (fileData, _) <- getbinary h "linux-0.01.tar.gz.sign"
    print fileData
```



## J



```J
   require 'web/gethttp'
   gethttp 'ftp://anonymous:example@ftp.hq.nasa.gov/pub/issoutreach/Living%20in%20Space%20Stories%20(MP3%20Files)/'
-rw-rw-r--   1 109      space-station  2327118 May  9  2005 09sept_spacepropulsion.mp3
-rw-rw-r--   1 109      space-station  1260304 May  9  2005 Can People go to Mars.mp3
-rw-rw-r--   1 109      space-station  1350270 May  9  2005 Distill some water.mp3
-rw-rw-r--   1 109      space-station  1290888 May  9  2005 Good Vibrations.mp3
-rw-rw-r--   1 109      space-station  1431834 May  9  2005 Gravity Hurts_So good.mp3
-rw-rw-r--   1 109      space-station  1072644 May  9  2005 Gravity in the Brain.mp3
-rw-rw-r--   1 109      space-station  1230594 May  9  2005 Power to the ISS.mp3
-rw-rw-r--   1 109      space-station  1309062 May  9  2005 Space Bones.mp3
-rw-rw-r--   1 109      space-station  2292715 May  9  2005 Space Power.mp3
-rw-rw-r--   1 109      space-station   772075 May  9  2005 We have a solution.mp3
-rw-rw-r--   1 109      space-station  1134654 May  9  2005 When Space Makes you Dizzy.mp3
   #file=: gethttp rplc&(' ';'%20') 'ftp://anonymous:example@ftp.hq.nasa.gov/pub/issoutreach/Living in Space Stories (MP3 Files)/We have a solution.mp3'
772075
```



## Java

requires apache.commons.net

```java
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import org.apache.commons.net.ftp.FTP;
import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPFile;
import org.apache.commons.net.ftp.FTPReply;

public class FTPconn {

    public static void main(String[] args) throws IOException {
        String server = "ftp.hq.nasa.gov";
        int port = 21;
        String user = "anonymous";
        String pass = "ftptest@example.com";

        OutputStream output = null;

        FTPClient ftpClient = new FTPClient();
        try {
            ftpClient.connect(server, port);

            serverReply(ftpClient);

            int replyCode = ftpClient.getReplyCode();
            if (!FTPReply.isPositiveCompletion(replyCode)) {
                System.out.println("Failure. Server reply code: " + replyCode);
                return;
            }

            serverReply(ftpClient);

            if (!ftpClient.login(user, pass)) {
                System.out.println("Could not login to the server.");
                return;
            }

            String dir = "pub/issoutreach/Living in Space Stories (MP3 Files)/";
            if (!ftpClient.changeWorkingDirectory(dir)) {
                System.out.println("Change directory failed.");
                return;
            }

            ftpClient.enterLocalPassiveMode();

            for (FTPFile file : ftpClient.listFiles())
                System.out.println(file);

            String filename = "Can People go to Mars.mp3";
            output = new FileOutputStream(filename);

            ftpClient.setFileType(FTP.BINARY_FILE_TYPE);
            if (!ftpClient.retrieveFile(filename, output)) {
                System.out.println("Retrieving file failed");
                return;
            }

            serverReply(ftpClient);

            ftpClient.logout();

        } finally {
            if (output != null)
                output.close();
        }
    }

    private static void serverReply(FTPClient ftpClient) {
        for (String reply : ftpClient.getReplyStrings()) {
            System.out.println(reply);
        }
    }
}
```


Output:


```txt
220-Warning: This system is owned and operated by the US Federal Government.
          Unauthorized access to this system is a violation of US Federal
          law and could lead to prosecution.
 
 This is NASA HQ ANONYMOUS FTP SERVER.
 
 Please read the README file located in the initial server root directory.
 
 IF you place files into the /incoming directory, it is IMPERATIVE that you
 notify ftp-admin@hq.nasa.gov that you have done so and of your intended
 disposition of those files.  Absent such notification, all files placed
 in /incoming that cannot be identified will be immediately deleted.
 
220 FTP Server Ready
220-Warning: This system is owned and operated by the US Federal Government.
          Unauthorized access to this system is a violation of US Federal
          law and could lead to prosecution.
 
 This is NASA HQ ANONYMOUS FTP SERVER.
 
 Please read the README file located in the initial server root directory.
 
 IF you place files into the /incoming directory, it is IMPERATIVE that you
 notify ftp-admin@hq.nasa.gov that you have done so and of your intended
 disposition of those files.  Absent such notification, all files placed
 in /incoming that cannot be identified will be immediately deleted.
 
220 FTP Server Ready
-rw-rw-r--   1 109      space-station  2327118 May  9  2005 09sept_spacepropulsion.mp3
-rw-rw-r--   1 109      space-station  1260304 May  9  2005 Can People go to Mars.mp3
-rw-rw-r--   1 109      space-station  1350270 May  9  2005 Distill some water.mp3
-rw-rw-r--   1 109      space-station  1290888 May  9  2005 Good Vibrations.mp3
-rw-rw-r--   1 109      space-station  1431834 May  9  2005 Gravity Hurts_So good.mp3
-rw-rw-r--   1 109      space-station  1072644 May  9  2005 Gravity in the Brain.mp3
-rw-rw-r--   1 109      space-station  1230594 May  9  2005 Power to the ISS.mp3
-rw-rw-r--   1 109      space-station  1309062 May  9  2005 Space Bones.mp3
-rw-rw-r--   1 109      space-station  2292715 May  9  2005 Space Power.mp3
-rw-rw-r--   1 109      space-station   772075 May  9  2005 We have a solution.mp3
-rw-rw-r--   1 109      space-station  1134654 May  9  2005 When Space Makes you Dizzy.mp3
226 Transfer complete
```



## Julia

{{works with|Julia|0.6}}


```julia
using FTPClient

ftp = FTP(hostname = "ftp.ed.ac.uk", username = "anonymous")
cd(ftp, "pub/courses")
println(readdir(ftp))
bytes = read(download(ftp, "make.notes.tar"))

close(ftp)
```



## Kotlin

{{trans|C}}
{{libheader|ftplib}}
{{works with|Ubuntu 14.04}}
Assuming that ftplib is already installed on your system in the default location(s), you first need to build ftplib.klib using the following .def file and the cinterop tool. As the NetBuf struct is not defined in ftplib.h (but is in ftplib.c) it has been included in the .def file so that the tool can deal with it (and its alias 'netbuf') properly from a Kotlin perspective. 


```txt

headers = /usr/include/ftplib.h
linkerOpts.linux = -L/usr/lib -lftp

---

#include <sys/time.h>

struct NetBuf {
    char *cput,*cget;
    int handle;
    int cavail,cleft;
    char *buf;
    int dir;
    netbuf *ctrl;
    netbuf *data;    
    int cmode;
    struct timeval idletime;
    FtpCallback idlecb;
    void *idlearg;
    int xfered;
    int cbbytes;
    int xfered1;
    char response[256];
};

```

Next, you need to compile the following Kotlin program, linking against ftplib.klib.

```scala
// Kotlin Native v0.6

import kotlinx.cinterop.*
import ftplib.*

fun main(args: Array<String>) {
    val nbuf = nativeHeap.allocPointerTo<netbuf>()
    FtpInit()
    FtpConnect("ftp.easynet.fr", nbuf.ptr)
    val vnbuf = nbuf.value
    FtpLogin("anonymous", "ftptest@example.com", vnbuf)
    FtpOptions(FTPLIB_CONNMODE, FTPLIB_PASSIVE.toLong(), vnbuf)
    FtpChdir("/debian/", vnbuf)
    FtpDir(null, ".", vnbuf)
    FtpGet("ftp.README", "README.html", FTPLIB_ASCII.toByte(), vnbuf)
    FtpQuit(vnbuf)
    nativeHeap.free(nbuf)
}
```

Finally, the resulting .kexe file should be executed producing something similar to the following output:

```txt

drwxr-xr-x  23 1002     1002         4096 Dec  9 09:44 dists
drwxr-xr-x   4 1002     1002         4096 Mar  3 19:52 doc
-rw-r--r--   1 1002     1002       361654 Mar  3 20:49 extrafiles
drwxr-xr-x   3 1002     1002         4096 Mar  3 20:42 indices
-rw-r--r--   1 1002     1002     14948661 Mar  3 20:42 ls-lR.gz
drwxr-xr-x   5 1002     1002         4096 Dec 19  2000 pool
drwxr-xr-x   4 1002     1002         4096 Nov 17  2008 project
-rw-r--r--   1 1002     1002         1186 Dec  9 09:42 README
-rw-r--r--   1 1002     1002         1290 Jun 26  2010 README.CD-manufacture
-rw-r--r--   1 1002     1002         2903 Dec  9 09:42 README.html
-rw-r--r--   1 1002     1002          291 Mar  4  2017 README.mirrors.html
-rw-r--r--   1 1002     1002           86 Mar  4  2017 README.mirrors.txt
drwxr-xr-x   3 1002     1002         4096 Oct 10  2012 tools
drwxr-xr-x  23 1002     1002         4096 Jun 17  2017 zzz-dists

```



## Lingo

{{libheader|Curl Xtra}}

```lingo
CURLOPT_URL = 10002
ch = xtra("Curl").new()
url = "ftp://domain.com"

-- change to remote dir "/foo/bar/"
put "/foo/bar/" after url

ch.setOption(CURLOPT_URL, url)
res = ch.exec(1)

-- print raw FTP listing as string
put res.readRawString(res.length)

-- download file "download.mp3" (passive mode is the internal default behavior)
filename = "download.mp3"
ch.setOption(CURLOPT_URL, url & filename)
ch.setDestinationFile(_movie.path & filename)
res = ch.exec()
```



## LiveCode


```LiveCode
libURLSetFTPMode "passive"  --default is passive anyway
put url "ftp://ftp.hq.nasa.gov/" into listing
repeat for each line ftpln in listing
    set itemdel to space
    if the first char of (the first item of ftpln) is "d" then
        -- is a directory
        put the last item of ftpln after dirlist
    else
        put the last item of ftpln after filelist
    end if
end repeat

put listing //(subset)
//  -rw-r--r--   1 ftpadmin ftp-adm      3997 May 26  1998 README
//  drwxrwsr-x  17 ftpadmin ftp-adm      4096 Sep 10 16:08 pub

put dirlist
//  armd 
//  chmgt
//  incoming
//  lost+found
//  office
//  pub

put filelist
//  README
//  ftp-exec
//  index.html
//  robots.txt

-- downloading a file (upload is same, but use put)
-- you don't have to cd manually
-- file up/down transfer is binary in livecode (always enforced by livecode)
put URL  "ftp://ftp.hq.nasa.gov/pub/robots.txt" into URL "file:myFile.txt"

You can execute any ftp command using the libURLftpCommand command
e.g. to know the working directory, issue "pwd", we could issue "list" for above too, 
but using an url with slash on the end with the ftp protocol causes a dir listing by default.
put libURLftpCommand("PWD",ftp.example.org)
```



## Perl


```perl
use Net::FTP;

# set server and credentials
my $host     = 'speedtest.tele2.net';
my $user     = 'anonymous';
my $password = '';

# connect in passive mode
my $f = Net::FTP->new($host) or die "Can't open $host\n";
$f->login($user, $password)  or die "Can't login as $user\n";
$f->passive();

# change remote directory, list contents
$f->cwd('upload');
@files = $f->ls();
printf "Currently %d files in the 'upload' directory.\n", @files;

# download file in binary mode
$f->cwd('/');
$f->type('binary');
$local = $f->get('512KB.zip');
print "Your file was stored as $local in the current directory\n";
```

{{out}}

```txt
Currently 20 files in the 'upload' directory
Your file was stored as 512KB.zip in the current directory!
```



## Perl 6

{{works with|rakudo|2018.04}}


```perl6
use Net::FTP;

my $host = 'speedtest.tele2.net';
my $user = 'anonymous';
my $password = '';

my $ftp = Net::FTP.new( host => $host, :passive );

$ftp.login( user => $user, pass => $password );

$ftp.cwd( 'upload' );

$ftp.cwd( '/' );

say $_<name> for $ftp.ls;

$ftp.get( '1KB.zip', :binary );
```


{{out}}

```txt
1000GB.zip
100GB.zip
100KB.zip
100MB.zip
10GB.zip
10MB.zip
1GB.zip
1KB.zip
1MB.zip
200MB.zip
20MB.zip
2MB.zip
3MB.zip
500MB.zip
50MB.zip
512KB.zip
5MB.zip
upload
```



## Phix


```Phix
include libcurl.e
constant url = "ftp://speedtest.tele2.net/"

curl_global_init()
atom curl = curl_easy_init(),
     pErrorBuffer = allocate(CURL_ERROR_SIZE)
curl_easy_setopt(curl, CURLOPT_ERRORBUFFER, pErrorBuffer)
curl_easy_setopt(curl, CURLOPT_URL, url)
object res = curl_easy_perform_ex(curl)
if integer(res) then
    ?{res,peek_string(pErrorBuffer)}
else
    puts(1,res)
end if

string filename = "1KB.zip"
{} = delete_file(filename)
res = curl_easy_get_file(url&filename, "", filename) 
if res=CURLE_OK then
    printf(1,"successfully downloaded %s (size %s)\n",{filename,get_file_size(filename,true)})
else
    ?{"error",res}
end if
```

{{out}}

```txt

-rw-r--r--    1 0        0        1073741824000 Feb 19  2016 1000GB.zip
-rw-r--r--    1 0        0        107374182400 Feb 19  2016 100GB.zip
-rw-r--r--    1 0        0          102400 Feb 19  2016 100KB.zip
-rw-r--r--    1 0        0        104857600 Feb 19  2016 100MB.zip
-rw-r--r--    1 0        0        10737418240 Feb 19  2016 10GB.zip
-rw-r--r--    1 0        0        10485760 Feb 19  2016 10MB.zip
-rw-r--r--    1 0        0        1073741824 Feb 19  2016 1GB.zip
-rw-r--r--    1 0        0            1024 Feb 19  2016 1KB.zip
-rw-r--r--    1 0        0         1048576 Feb 19  2016 1MB.zip
-rw-r--r--    1 0        0        209715200 Feb 19  2016 200MB.zip
-rw-r--r--    1 0        0        20971520 Feb 19  2016 20MB.zip
-rw-r--r--    1 0        0         2097152 Feb 19  2016 2MB.zip
-rw-r--r--    1 0        0         3145728 Feb 19  2016 3MB.zip
-rw-r--r--    1 0        0        524288000 Feb 19  2016 500MB.zip
-rw-r--r--    1 0        0        52428800 Feb 19  2016 50MB.zip
-rw-r--r--    1 0        0          524288 Feb 19  2016 512KB.zip
-rw-r--r--    1 0        0         5242880 Feb 19  2016 5MB.zip
drwxr-xr-x    2 105      108        561152 Jul 18 13:11 upload
successfully downloaded 1KB.zip (size 1KB)

```



## PicoLisp

Passive is the default behavior of 'curl'

```PicoLisp
(in '(curl "-sl" "ftp://kernel.org/pub/site/")
   (while (line)
      (prinl @) ) )
(call "curl" "-s" "-o" "sha256sums.asc" "ftp://kernel.org/pub/site/sha256sums.asc")
```

Output:

```txt
README
sample_mirror_script.pl
sha256sums.asc
```



## Python

{{works with|Python|2.7.10}}

```Python

from ftplib import FTP
ftp = FTP('kernel.org')
ftp.login()
ftp.cwd('/pub/linux/kernel')
ftp.set_pasv(True) # Default since Python 2.1
print ftp.retrlines('LIST')
print ftp.retrbinary('RETR README', open('README', 'wb').write)
ftp.quit()

```



## Racket

Note: <tt>net/ftp</tt> in Racket uses passive mode exclusively.

```racket

#lang racket
(require net/ftp)
(let* ([server "kernel.org"]
       [remote-dir "/pub/linux/kernel/"]
       [conn (ftp-establish-connection
               server
               21
               "anonymous"
               "")])
  (ftp-cd conn remote-dir)
  (map
   (lambda (elem) (displayln (string-join elem "\t")))
   (ftp-directory-list conn "."))
  (ftp-download-file conn "." "README")
  (ftp-close-connection conn))

```



## REBOL


```REBOL

system/schemes/ftp/passive: on
print read ftp://kernel.org/pub/linux/kernel/
write/binary %README read/binary ftp://kernel.org/pub/linux/kernel/README

```


## Ruby


```ruby
require 'net/ftp'

Net::FTP.open('ftp.ed.ac.uk', "anonymous","aaa@gmail.com" ) do |ftp|
  ftp.passive = true  # default since Ruby 2.3
  ftp.chdir('pub/courses')
  puts ftp.list
  ftp.getbinaryfile("make.notes.tar")
end
```

The connection is closed automatically at the end of the block.


## Rust

Using crate <code>ftp</code> version 3.0.1

```Rust
use std::{error::Error, fs::File, io::copy};
use ftp::FtpStream;

fn main() -> Result<(), Box<dyn Error>> {
    let mut ftp = FtpStream::connect("ftp.easynet.fr:21")?;
    ftp.login("anonymous", "")?;
    ftp.cwd("debian")?;
    for file in ftp.list(None)? {
        println!("{}", file);
    }
    let mut stream = ftp.get("README")?;
    let mut file = File::create("README")?;
    copy(&mut stream, &mut file)?;
    Ok(())
}
```



## Scala

{{libheader|commons-net}}

```Scala
import java.io.{File, FileOutputStream, InputStream}

import org.apache.commons.net.ftp.{FTPClient, FTPFile, FTPReply}

import scala.util.{Failure, Try}

object FTPconn extends App {
  val (server, pass) = ("ftp.ed.ac.uk", "-ftptest@example.com")
  val (dir, filename, ftpClient) = ("/pub/cartonet/", "readme.txt", new FTPClient())

  def canConnect(host: String): Boolean = {
    ftpClient.connect(host)
    val connectionWasEstablished = ftpClient.isConnected
    ftpClient.disconnect()
    connectionWasEstablished
  }

  def downloadFileStream(remote: String): InputStream = {
    val stream: InputStream = ftpClient.retrieveFileStream(remote)
    ftpClient.completePendingCommand()
    stream
  }

  def uploadFile(remote: String, input: InputStream): Boolean = ftpClient.storeFile(remote, input)

  if (Try {
    def cwd(path: String): Boolean = ftpClient.changeWorkingDirectory(path)

    def filesInCurrentDirectory: Seq[String] = listFiles().map(_.getName)

    def listFiles(): List[FTPFile] = ftpClient.listFiles.toList

    def downloadFile(remote: String): Boolean = {
      val os = new FileOutputStream(new File(remote))
      ftpClient.retrieveFile(remote, os)
    }

    def connectWithAuth(host: String,
                        password: String,
                        username: String = "anonymous",
                        port: Int = 21): Try[Boolean] = {
      def connect(): Try[Unit] = Try {
        try {
          ftpClient.connect(host, port)
        } catch {
          case ex: Throwable =>
            println(ex.getMessage)
            Failure
        }
        ftpClient.enterLocalPassiveMode()
        serverReply(ftpClient)

        val replyCode = ftpClient.getReplyCode
        if (!FTPReply.isPositiveCompletion(replyCode))
          println("Failure. Server reply code: " + replyCode)
      }

      for {
        connection <- connect()
        login <- Try {
          ftpClient.login(username, password)
        }
      } yield login
    }

    def serverReply(ftpClient: FTPClient): Unit =
      for (reply <- ftpClient.getReplyStrings) println(reply)

    connectWithAuth(server, pass)

    cwd(dir)
    listFiles().foreach(println)

    downloadFile(filename)
    serverReply(ftpClient)
    ftpClient.logout
  }.isFailure) println(s"Failure.")
}
```

{{Out}}See it in running in your browser by [https://scastie.scala-lang.org/3Lq8ehzIQTCuAOPXWofNLw Scastie (JVM)].

## Seed7

The library [http://seed7.sourceforge.net/libraries/ftp.htm ftp.s7i] contains functions to
[http://seed7.sourceforge.net/libraries/ftp.htm#openFtp(in_string) open] and handle an
[http://seed7.sourceforge.net/libraries/ftp.htm#ftpFileSys ftpFileSys].

```seed7
$ include "seed7_05.s7i";
  include "ftp.s7i";

const proc: main is func
  local
    var ftpFileSys: ftp is fileSys.value;
    var string: line is "";
  begin
    ftp := openFtp("kernel.org");
    setActiveMode(ftp, FALSE);  # Passive is the default.
    chdir(ftp, "/pub/linux/kernel");
    for line range listDir(ftp, ".") do
      writeln(line);
    end for;
    setAsciiTransfer(ftp, FALSE);
    writeln(getFile(ftp, "README"));
    close(ftp);
  end func;
```



## Sidef

{{trans|Ruby}}

```ruby
require('Net::FTP');

var ftp = %s'Net::FTP'.new('ftp.ed.ac.uk', Passive => 1);
ftp.login('anonymous','aaa@gmail.com');
ftp.cwd('pub/courses');
[ftp.dir].each {|line| say line };
ftp.binary;   # set binary mode
ftp.get("make.notes.tar");
ftp.quit;
```



## Tcl


### Using package ftp


```Tcl

package require ftp

set conn [::ftp::Open kernel.org anonymous "" -mode passive]
::ftp::Cd $conn /pub/linux/kernel
foreach line [ftp::NList $conn] {
    puts $line
}
::ftp::Type $conn binary
::ftp::Get $conn README README

```



### Using a virtual file system

An alternative approach that uses the package [http://sourceforge.net/projects/tclvfs/ TclVFS] to access ftp:// paths as a virtual file system.


```tcl

package require vfs::urltype
vfs::urltype::Mount ftp

# Patch to enable FTP passive mode.
source vfsftpfix.tcl

set dir [pwd]
cd ftp://kernel.org/pub/linux/kernel
foreach line [glob -dir ftp://kernel.org/pub/linux/kernel *] {
    puts $line
}
file copy README [file join $dir README]

```


The file <tt>vfsftpfix.tcl</tt> with the passive mode patch (see http://wiki.tcl.tk/12837):

```tcl

# Replace vfs::ftp::Mount to enable vfs::ftp to work in passive
# mode and make that the default.
package require vfs::ftp
proc vfs::ftp::Mount {dirurl local {mode passive}} {
    set dirurl [string trim $dirurl]
    ::vfs::log "ftp-vfs: attempt to mount $dirurl at $local"
    if {[string index $dirurl end] != "/"} {
        ::vfs::log "ftp-vfs: adding missing directory delimiter to mount point"
        append dirurl "/"
    }

    set urlRE {(?:ftp://)?(?:([^@:]*)(?::([^@]*))?@)?([^/:]+)(?::([0-9]*))?/(.*/)?$}
    if {![regexp $urlRE $dirurl - user pass host port path]} {
        return -code error "Sorry I didn't understand\
          the url address \"$dirurl\""
    }

    if {![string length $user]} {
        set user anonymous
    }

    if {![string length $port]} {
        set port 21
    }

    set fd [::ftp::Open $host $user $pass -port $port -output ::vfs::ftp::log -mode $mode]
    if {$fd == -1} {
        error "Mount failed"
    }

    if {$path != ""} {
        if {[catch {
            ::ftp::Cd $fd $path
        } err]} {
            ftp::Close $fd
            error "Opened ftp connection, but then received error: $err"
        }
    }

    if {![catch {vfs::filesystem info $dirurl}]} {
        # unmount old mount
        ::vfs::log "ftp-vfs: unmounted old mount point at $dirurl"
        vfs::unmount $dirurl
    }
    ::vfs::log "ftp $host, $path mounted at $fd"
    vfs::filesystem mount $local [list vfs::ftp::handler $fd $path]
    # Register command to unmount
    vfs::RegisterMount $local [list ::vfs::ftp::Unmount $fd]
    return $fd
}

```



## zkl

Using the cURL library, doing this from the REPL. Moving around in the tree isn't supported.

```zkl
zkl: var cURL=Import("zklCurl")
zkl: var d=cURL().get("ftp.hq.nasa.gov/pub/issoutreach/Living in Space Stories (MP3 Files)/")
L(Data(2,567),1630,23) // downloaded listing, 1630 bytes of header, 23 bytes of trailer
zkl: d[0][1630,-23].text
-rw-rw-r--   1 109      space-station  2327118 May  9  2005 09sept_spacepropulsion.mp3
...
-rw-rw-r--   1 109      space-station  1134654 May  9  2005 When Space Makes you Dizzy.mp3

zkl: d=cURL().get("ftp.hq.nasa.gov/pub/issoutreach/Living in Space Stories (MP3 Files)/When Space Makes you Dizzy.mp3")
L(Data(1,136,358),1681,23)
zkl: File("foo.mp3","w").write(d[0][1681,-23])
1134654  // note that this matches size in listing
```

The resulting file foo.mp3 has a nice six minute description of what can happen when returning from space.

{{omit from|PARI/GP}}
{{omit from|Commodore BASIC}}
