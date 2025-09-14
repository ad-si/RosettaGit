+++
title = "Send email"
description = ""
date = 2019-09-17T09:04:55Z
aliases = []
[extra]
id = 4467
[taxonomies]
categories = ["task", "Networking and Web Interaction"]
tags = []
languages = [
  "ada",
  "autohotkey",
  "bbc_basic",
  "c",
  "clojure",
  "cpp",
  "csharp",
  "d",
  "delphi",
  "emacs_lisp",
  "factor",
  "fantom",
  "fortran",
  "go",
  "groovy",
  "haskell",
  "java",
  "julia",
  "kotlin",
  "lasso",
  "liberty_basic",
  "lingo",
  "livecode",
  "lotusscript",
  "lua",
  "mathematica",
  "newlisp",
  "nim",
  "ocaml",
  "perl",
  "perl_6",
  "phix",
  "php",
  "picolisp",
  "pike",
  "powershell",
  "purebasic",
  "python",
  "r",
  "racket",
  "rebol",
  "rexx",
  "ring",
  "ruby",
  "sas",
  "scala",
  "sql_pl",
  "tcl",
  "tuscript",
  "txr",
  "vba",
  "vbscript",
]
+++

## Task

Write a function to send an email.

The function should have parameters for setting From, To and Cc addresses; the Subject, and the message text, and optionally fields for the server name and login details.
* If appropriate, explain what notifications of problems/success are given.
* Solutions using libraries or functions from the language are preferred, but failing that, external programs can be used with an explanation.
* Note how portable the solution given is between operating systems when multi-OS languages are used.



(Remember to obfuscate any sensitive data used in examples)





## Ada

```Ada
with AWS.SMTP, AWS.SMTP.Client, AWS.SMTP.Authentication.Plain;
with Ada.Text_IO;
use  Ada, AWS;

procedure Sendmail is
   Status : SMTP.Status;
   Auth : aliased constant SMTP.Authentication.Plain.Credential :=
      SMTP.Authentication.Plain.Initialize ("id", "password");
   Isp : SMTP.Receiver;
begin
   Isp :=
      SMTP.Client.Initialize
        ("smtp.mail.com",
         Port       => 5025,
         Credential => Auth'Unchecked_Access);
   SMTP.Client.Send
     (Isp,
      From    => SMTP.E_Mail ("Me", "me@some.org"),
      To      => SMTP.E_Mail ("You", "you@any.org"),
      Subject => "subject",
      Message => "Here is the text",
      Status  => Status);
   if not SMTP.Is_Ok (Status) then
      Text_IO.Put_Line
        ("Can't send message :" & SMTP.Status_Message (Status));
   end if;
end Sendmail;

```



## AutoHotkey

ahk [http://www.autohotkey.com%2Fforum%2Ftopic39797.html discussion]
```autohotkey
sSubject:= "greeting"
sText   := "hello"
sFrom   := "ahk@rosettacode"
sTo   := "whomitmayconcern"

sServer   := "smtp.gmail.com" ; specify your SMTP server
nPort     := 465 ; 25
bTLS      := True ; False
inputbox, sUsername, Username
inputbox, sPassword, password

COM_Init()
pmsg :=   COM_CreateObject("CDO.Message")
pcfg :=   COM_Invoke(pmsg, "Configuration")
pfld :=   COM_Invoke(pcfg, "Fields")

COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/sendusing", 2)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/smtpconnectiontimeout", 60)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/smtpserver", sServer)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/smtpserverport", nPort)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/smtpusessl", bTLS)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/smtpauthenticate", 1)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/sendusername", sUsername)
COM_Invoke(pfld, "Item", "http://schemas.microsoft.com/cdo/configuration/sendpassword", sPassword)
COM_Invoke(pfld, "Update")

COM_Invoke(pmsg, "Subject", sSubject)
COM_Invoke(pmsg, "From", sFrom)
COM_Invoke(pmsg, "To", sTo)
COM_Invoke(pmsg, "TextBody", sText)
COM_Invoke(pmsg, "Send")

COM_Release(pfld)
COM_Release(pcfg)
COM_Release(pmsg)
COM_Term()
#Include COM.ahk
```



## BBC BASIC

```bbcbasic
      INSTALL @lib$+"SOCKLIB"

      Server$ = "smtp.gmail.com"
      From$   = "sender@somewhere"
      To$     = "recipient@elsewhere"
      CC$     = "another@nowhere"
      Subject$ = "Rosetta Code"
      Message$ = "This is a test of sending email."

      PROCsendmail(Server$, From$, To$, CC$, "", Subject$, "", Message$)
      END

      DEF PROCsendmail(smtp$,from$,to$,cc$,bcc$,subject$,replyto$,body$)
      LOCAL D%, S%, skt%, reply$
      DIM D% LOCAL 31, S% LOCAL 15

      SYS "GetLocalTime", S%
      SYS "GetDateFormat", 0, 0, S%, "ddd, dd MMM yyyy ", D%, 18
      SYS "GetTimeFormat", 0, 0, S%, "HH:mm:ss +0000", D%+17, 15
      D%?31 = 13

      PROC_initsockets
      skt% = FN_tcpconnect(smtp$,"mail")
      IF skt% <= 0 skt% = FN_tcpconnect(smtp$,"25")
      IF skt% <= 0 ERROR 100, "Failed to connect to SMTP server"
      IF FN_readlinesocket(skt%, 1000, reply$)
      WHILE FN_readlinesocket(skt%, 10, reply$) > 0 : ENDWHILE

      PROCsend(skt%,"HELO "+FN_gethostname)
      PROCmail(skt%,"MAIL FROM: ",from$)
      IF to$<>"" PROClist(skt%,to$)
      IF cc$<>"" PROClist(skt%,cc$)
      IF bcc$<>"" PROClist(skt%,bcc$)
      PROCsend(skt%, "DATA")

      IF FN_writelinesocket(skt%, "Date: "+$D%)
      IF FN_writelinesocket(skt%, "From: "+from$)
      IF FN_writelinesocket(skt%, "To: "+to$)
      IF cc$<>"" IF FN_writelinesocket(skt%, "Cc: "+cc$)
      IF subject$<>"" IF FN_writelinesocket(skt%, "Subject: "+subject$)
      IF replyto$<>"" IF FN_writelinesocket(skt%, "Reply-To: "+replyto$)
      IF FN_writelinesocket(skt%, "MIME-Version: 1.0")
      IF FN_writelinesocket(skt%, "Content-type: text/plain; charset=US-ASCII")

      IF FN_writelinesocket(skt%, "")
      IF FN_writelinesocket(skt%, body$)
      IF FN_writelinesocket(skt%, ".")

      PROCsend(skt%,"QUIT")

      PROC_exitsockets
      ENDPROC

      DEF PROClist(skt%,list$)
      LOCAL comma%
      REPEAT
        WHILE ASClist$=32 list$=MID$(list$,2):ENDWHILE
        comma% = INSTR(list$,",")
        IF comma% THEN
          PROCmail(skt%,"RCPT TO: ",LEFT$(list$,comma%-1))
          list$ = MID$(list$,comma%+1)
        ELSE
          PROCmail(skt%,"RCPT TO: ",list$)
        ENDIF
      UNTIL comma% = 0
      ENDPROC

      DEF PROCmail(skt%,cmd$,mail$)
      LOCAL I%,J%
      I% = INSTR(mail$,"<")
      J% = INSTR(mail$,">",I%)
      IF I% IF J% THEN
        PROCsend(skt%, cmd$+MID$(mail$,I%,J%-I%+1))
      ELSE
        PROCsend(skt%, cmd$+"<"+mail$+">")
      ENDIF
      ENDPROC

      DEF PROCsend(skt%,cmd$)
      LOCAL reply$
      IF FN_writelinesocket(skt%,cmd$) < 0 THEN ERROR 100, "Send failed"
      IF FN_readlinesocket(skt%, 200, reply$)
      WHILE FN_readlinesocket(skt%, 10, reply$) > 0 : ENDWHILE
      ENDPROC

```



## C

Sends mail via the GMail SMTP server, requires [https://curl.haxx.se/libcurl/ libcurl]
```C


#include <curl/curl.h>
#include <string.h>
#include <stdio.h>

#define from    "<sender@duniya.com>"
#define to      "<addressee@gmail.com>"
#define cc      "<info@example.org>"

static const char *payload_text[] = {
  "Date: Mon, 13 Jun 2018 11:30:00 +0100\r\n",
  "To: " to "\r\n",
  "From: " from " (Example User)\r\n",
  "Cc: " cc " (Another example User)\r\n",
  "Message-ID: <ecd7db36-10ab-437a-9g3a-e652b9458efd@"
  "rfcpedant.example.org>\r\n",
  "Subject: Sanding mail via C\r\n",
  "\r\n",
  "This mail is being sent by a C program.\r\n",
  "\r\n",
  "It connects to the GMail SMTP server, by far, the most popular mail program of all.\r\n",
  "Which is also probably written in C.\r\n",
  "To C or not to C..............\r\n",
  "That is the question.\r\n",
  NULL
};

struct upload_status {
  int lines_read;
};

static size_t payload_source(void *ptr, size_t size, size_t nmemb, void *userp)
{
  struct upload_status *upload_ctx = (struct upload_status *)userp;
  const char *data;

  if((size == 0) || (nmemb == 0) || ((size*nmemb) < 1)) {
    return 0;
  }

  data = payload_text[upload_ctx->lines_read];

  if(data) {
    size_t len = strlen(data);
    memcpy(ptr, data, len);
    upload_ctx->lines_read++;

    return len;
  }

  return 0;
}

int main(void)
{
  CURL *curl;
  CURLcode res = CURLE_OK;
  struct curl_slist *recipients = NULL;
  struct upload_status upload_ctx;

  upload_ctx.lines_read = 0;

  curl = curl_easy_init();
  if(curl) {

    curl_easy_setopt(curl, CURLOPT_USERNAME, "user");
    curl_easy_setopt(curl, CURLOPT_PASSWORD, "secret");

    curl_easy_setopt(curl, CURLOPT_URL, "smtp://smtp.gmail.com:465");

    curl_easy_setopt(curl, CURLOPT_USE_SSL, (long)CURLUSESSL_ALL);

    curl_easy_setopt(curl, CURLOPT_CAINFO, "/path/to/certificate.pem");

    curl_easy_setopt(curl, CURLOPT_MAIL_FROM, from);

    recipients = curl_slist_append(recipients, to);
    recipients = curl_slist_append(recipients, cc);
    curl_easy_setopt(curl, CURLOPT_MAIL_RCPT, recipients);

    curl_easy_setopt(curl, CURLOPT_READFUNCTION, payload_source);
    curl_easy_setopt(curl, CURLOPT_READDATA, &upload_ctx);
    curl_easy_setopt(curl, CURLOPT_UPLOAD, 1L);

    curl_easy_setopt(curl, CURLOPT_VERBOSE, 1L);

    res = curl_easy_perform(curl);

    if(res != CURLE_OK)
      fprintf(stderr, "curl_easy_perform() failed: %s\n",curl_easy_strerror(res));

    curl_slist_free_all(recipients);

    curl_easy_cleanup(curl);
  }

  return (int)res;
}

```



## C++


```cpp
// on Ubuntu: sudo apt-get install libpoco-dev
// or see http://pocoproject.org/
// compile with: g++ -Wall -O3 send-mail-cxx.C -lPocoNet -lPocoFoundation

#include <cstdlib>
#include <iostream>
#include <Poco/Net/SMTPClientSession.h>
#include <Poco/Net/MailMessage.h>

using namespace Poco::Net;

int main (int argc, char **argv)
{
  try
    {
      MailMessage msg;

      msg.addRecipient (MailRecipient (MailRecipient::PRIMARY_RECIPIENT,
                                       "alice@example.com",
                                       "Alice Moralis"));
      msg.addRecipient (MailRecipient (MailRecipient::CC_RECIPIENT,
                                       "pat@example.com",
                                       "Patrick Kilpatrick"));
      msg.addRecipient (MailRecipient (MailRecipient::BCC_RECIPIENT,
                                       "mike@example.com",
                                       "Michael Carmichael"));

      msg.setSender ("Roy Kilroy <roy@example.com>");

      msg.setSubject ("Rosetta Code");
      msg.setContent ("Sending mail from C++ using POCO C++ Libraries");

      SMTPClientSession smtp ("mail.example.com"); // SMTP server name
      smtp.login ();
      smtp.sendMessage (msg);
      smtp.close ();
      std::cerr << "Sent mail successfully!" << std::endl;
    }
  catch (std::exception &e)
    {
      std::cerr << "failed to send mail: " << e.what() << std::endl;
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
```


When run literally as above, should print:


```txt
failed to send mail: Host not found
```


since mail.example.com does not exist.  To get it to work, you'll need to fill in the name of an SMTP server (such as the one provided by your ISP), and you should adjust the addresses of the sender and the recipient(s).

This version does not do authentication.  However, the login() method can accept a username and password for authentication.  Also, newer versions of POCO provide SecureSMTPClientSession, for doing STARTTLS.

## C#
```c#

static void Main(string[] args)
{
    //First of all construct the SMTP client

    SmtpClient SMTP = new SmtpClient("smtp.gmail.com", 587); //I have provided the URI and port for GMail, replace with your providers SMTP details
    SMTP.EnableSsl = true; //Required for gmail, may not for your provider, if your provider does not require it then use false.
    SMTP.DeliveryMethod = SmtpDeliveryMethod.Network;
    SMTP.Credentials = new NetworkCredential("YourUserName", "YourPassword");
    MailMessage Mail = new MailMessage("yourEmail@address.com", "theirEmail@address.com");


    //Then we construct the message

    Mail.Subject = "Important Message";
    Mail.Body = "Hello over there"; //The body contains the string for your email
    //using "Mail.IsBodyHtml = true;" you can put an HTML page in your message body

    //Then we use the SMTP client to send the message

    SMTP.Send(Mail);

    Console.WriteLine("Message Sent");
}

```



## Clojure

[https://github.com/drewr/postal Postal] wraps JavaMail to make sending emails simple and platform independent.

```clojure
(require '[postal.core :refer [send-message]])

(send-message {:host "smtp.gmail.com"
               :ssl true
               :user your_username
               :pass your_password}
              {:from "you@yourdomain.com"
               :to ["your_friend@example.com"]
               :cc ["bob@builder.com" "dora@explorer.com"]
               :subject "Yo"
               :body "Testing."})
```


```txt
{:error :SUCCESS, :code 0, :message "messages sent"}
```



## D

Requires the libcurl library to be installed on the system.

```d
void main() {
    import std.net.curl;

    auto s = SMTP("smtps://smtp.gmail.com");
    s.setAuthentication("someuser@gmail.com", "somepassword");
    s.mailTo = ["<friend@example.com>"];
    s.mailFrom = "<someuser@gmail.com>";
    s.message = "Subject:test\n\nExample Message";
    s.perform;
}
```



## Delphi


```Delphi

procedure SendEmail;
var
  msg: TIdMessage;
  smtp: TIdSMTP;
begin
  smtp := TIdSMTP.Create;
  try
    smtp.Host := 'smtp.server.com';
    smtp.Port := 587;
    smtp.Username := 'login';
    smtp.Password := 'password';
    smtp.AuthType := satNone;
    smtp.Connect;
    msg := TIdMessage.Create(nil);
    try
      with msg.Recipients.Add do begin
        Address := 'doug@gmail.com';
        Name := 'Doug';
      end;
      with msg.Sender do begin
        Address := 'fred@server.com';
        Name := 'Fred';
      end;
      msg.Subject := 'subj';
      msg.Body.Text := 'here goes email message';
      smtp.Send(msg);
    finally
      msg.Free;
    end;
  finally
    smtp.Free;
  end;
end;

```



## Emacs Lisp


Variable <code>send-mail-function</code> holds a function for sending a message from the current buffer.  The user or sysadmin is expected to set that variable to a preferred method (<code>sendmail</code>, SMTP, etc).  The default queries the user for initial setup.


```Lisp
(defun my-send-email (from to cc subject text)
  (with-temp-buffer
    (insert "From: " from "\n"
            "To: " to "\n"
            "Cc: " cc "\n"
            "Subject: " subject "\n"
            mail-header-separator "\n"
            text)
    (funcall send-mail-function)))

(my-send-email "from@example.com" "to@example.com" ""
               "very important"
               "body\ntext\n")
```


The buffer filling here pays no attention to charset or possible special characters in the fields or text.

(For user-level interactive mailing, <code>compose-mail</code> creates and pre-fills a message buffer ready for the user to edit and send, or not send.)


## Factor

This one uses the build-in SMTP vocabulary. Note that 'to' and 'cc' need to be arrays of strings containing an email address.


```Factor

USING: accessors io.sockets locals namespaces smtp ;
IN: scratchpad
:: send-mail ( f t c s b -- )
    default-smtp-config "smtp.gmail.com" 587 <inet> >>server
    t >>tls?
    "my.gmail.address@gmail.com" "qwertyuiasdfghjk" <plain-auth>
    >>auth \ smtp-config set-global <email> f >>from t >>to
    c >>cc s >>subject b >>body send-email ;
```



## Fantom


There's a built-in Email library, which will work on the JVM, CLR and Javascript runtimes.  Errors are thrown if there is a problem with the protocol or the network.


```fantom

using email

class Mail
{
  // create a client for sending email - add your own host/username/password
  static SmtpClient makeClient ()
  {
    client := SmtpClient
    {
      host     = "yourhost"
      username = "yourusername"
      password = "yourpassword"
    }
    return client
  }

  public static Void main()
  {
    // create email
    email := Email
    {
      to = ["to@addr"]
      from = "from@addr"
      cc = ["cc@addr"]
      subject = test"
      body = TextPart { text = "test email" }
    }

    // create client and send email
    makeClient.send (email)
  }
}

```



## Fortran


### Intel Fortran on Windows

Using Outlook COM server. Before compiling the program, it's necessary to use the '''[https://software.intel.com/en-us/node/535422 Intel Fortran Module Wizard]''' from the Visual Studio editor, to generate a Fortran module for the Microsoft Outlook Object Library. The following program has to be linked with this module (msoutl).


```fortran
program sendmail
    use ifcom
    use msoutl
    implicit none
    integer(4) :: app, status, msg

    call cominitialize(status)
    call comcreateobject("Outlook.Application", app, status)
    msg = $Application_CreateItem(app, olMailItem, status)
    call $MailItem_SetTo(msg, "somebody@somewhere", status)
    call $MailItem_SetSubject(msg, "Title", status)
    call $MailItem_SetBody(msg, "Hello", status)
    call $MailItem_Send(msg, status)
    call $Application_Quit(app, status)
    call comuninitialize()
end program
```



## Go


A full little command-line program that can be used to send simple e-mails. Uses the built-in smtp package.
Supports TLS connections.


```go
package main

import (
	"bufio"
	"bytes"
	"errors"
	"flag"
	"fmt"
	"io/ioutil"
	"net/smtp"
	"os"
	"strings"
)

type Message struct {
	From    string
	To      []string
	Cc      []string
	Subject string
	Content string
}

func (m Message) Bytes() (r []byte) {
	to := strings.Join(m.To, ",")
	cc := strings.Join(m.Cc, ",")

	r = append(r, []byte("From: "+m.From+"\n")...)
	r = append(r, []byte("To: "+to+"\n")...)
	r = append(r, []byte("Cc: "+cc+"\n")...)
	r = append(r, []byte("Subject: "+m.Subject+"\n\n")...)
	r = append(r, []byte(m.Content)...)

	return
}

func (m Message) Send(host string, port int, user, pass string) (err error) {
	err = check(host, user, pass)
	if err != nil {
		return
	}

	err = smtp.SendMail(fmt.Sprintf("%v:%v", host, port),
		smtp.PlainAuth("", user, pass, host),
		m.From,
		m.To,
		m.Bytes(),
	)

	return
}

func check(host, user, pass string) error {
	if host == "" {
		return errors.New("Bad host")
	}
	if user == "" {
		return errors.New("Bad username")
	}
	if pass == "" {
		return errors.New("Bad password")
	}

	return nil
}

func main() {
	var flags struct {
		host string
		port int
		user string
		pass string
	}
	flag.StringVar(&flags.host, "host", "", "SMTP server to connect to")
	flag.IntVar(&flags.port, "port", 587, "Port to connect to SMTP server on")
	flag.StringVar(&flags.user, "user", "", "Username to authenticate with")
	flag.StringVar(&flags.pass, "pass", "", "Password to authenticate with")
	flag.Parse()

	err := check(flags.host, flags.user, flags.pass)
	if err != nil {
		flag.Usage()
		os.Exit(1)
	}

	bufin := bufio.NewReader(os.Stdin)

	fmt.Printf("From: ")
	from, err := bufin.ReadString('\n')
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		os.Exit(1)
	}
	from = strings.Trim(from, " \t\n\r")

	var to []string
	for {
		fmt.Printf("To (Blank to finish): ")
		tmp, err := bufin.ReadString('\n')
		if err != nil {
			fmt.Printf("Error: %v\n", err)
			os.Exit(1)
		}
		tmp = strings.Trim(tmp, " \t\n\r")

		if tmp == "" {
			break
		}

		to = append(to, tmp)
	}

	var cc []string
	for {
		fmt.Printf("Cc (Blank to finish): ")
		tmp, err := bufin.ReadString('\n')
		if err != nil {
			fmt.Printf("Error: %v\n", err)
			os.Exit(1)
		}
		tmp = strings.Trim(tmp, " \t\n\r")

		if tmp == "" {
			break
		}

		cc = append(cc, tmp)
	}

	fmt.Printf("Subject: ")
	subject, err := bufin.ReadString('\n')
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		os.Exit(1)
	}
	subject = strings.Trim(subject, " \t\n\r")

	fmt.Printf("Content (Until EOF):\n")
	content, err := ioutil.ReadAll(os.Stdin)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		os.Exit(1)
	}
	content = bytes.Trim(content, " \t\n\r")

	m := Message{
		From:    from,
		To:      to,
		Cc:      cc,
		Subject: subject,
		Content: string(content),
	}

	fmt.Printf("\nSending message...\n")
	err = m.Send(flags.host, flags.port, flags.user, flags.pass)
	if err != nil {
		fmt.Printf("Error: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("Message sent.\n")
}
```



## Groovy

From [http://www.jedox.com/en/send-email-using-javamail-groovy-script/]  we can get email solution for Groovy

```Groovy

import javax.mail.*
import javax.mail.internet.*

public static void simpleMail(String from, String password, String to,
    String subject, String body) throws Exception {

    String host = "smtp.gmail.com";
    Properties props = System.getProperties();
    props.put("mail.smtp.starttls.enable",true);
    /* mail.smtp.ssl.trust is needed in script to avoid error "Could not convert socket to TLS"  */
    props.setProperty("mail.smtp.ssl.trust", host);
    props.put("mail.smtp.auth", true);
    props.put("mail.smtp.host", host);
    props.put("mail.smtp.user", from);
    props.put("mail.smtp.password", password);
    props.put("mail.smtp.port", "587");

    Session session = Session.getDefaultInstance(props, null);
    MimeMessage message = new MimeMessage(session);
    message.setFrom(new InternetAddress(from));

    InternetAddress toAddress = new InternetAddress(to);

    message.addRecipient(Message.RecipientType.TO, toAddress);

    message.setSubject(subject);
    message.setText(body);

    Transport transport = session.getTransport("smtp");

    transport.connect(host, from, password);

    transport.sendMessage(message, message.getAllRecipients());
    transport.close();
}

/* Set email address sender */
String s1 = "example@gmail.com";

/* Set password sender */
String s2 = "";

/* Set email address sender */
String s3 = "example@gmail.com"

/*Call function */
simpleMail(s1, s2 , s3, "TITLE", "TEXT");

```



## Haskell


Example using [https://hackage.haskell.org/package/smtp-mail <tt>smtp-mail</tt>] package:


```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Network.Mail.SMTP
                    ( Address(..)
                    , htmlPart
                    , plainTextPart
                    , sendMailWithLogin'
                    , simpleMail
                    )

main :: IO ()
main =
    sendMailWithLogin' "smtp.example.com" 25 "user" "password" $
        simpleMail
            (Address (Just "From Example") "from@example.com")
            [Address (Just "To Example") "to@example.com"]
            [] -- CC
            [] -- BCC
            "Subject"
            [ plainTextPart "This is plain text."
            , htmlPart "<h1>Title</h1><p>This is HTML.</p>"
            ]
```


==Icon and {{header|Unicon}}==

A Unicon-specific solution is:

```unicon
procedure main(args)
    mail := open("mailto:"||args[1], "m", "Subject : "||args[2],
                 "X-Note: automatically send by Unicon") |
            stop("Cannot send mail to ",args[1])
    every write(mail , !&input)
    close (mail)
end
```



## Java



```java5
import java.util.Properties;

import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.Message.RecipientType;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

/**
 * Mail
 */
public class Mail
{
 /**
  * Session
  */
 protected Session session;

 /**
  * Mail constructor.
  *
  * @param host Host
  */
 public Mail(String host)
 {
  Properties properties = new Properties();
  properties.put("mail.smtp.host", host);
  session = Session.getDefaultInstance(properties);
 }

 /**
  * Send email message.
  *
  * @param from From
  * @param tos Recipients
  * @param ccs CC Recipients
  * @param subject Subject
  * @param text Text
  * @throws MessagingException
  */
 public void send(String from, String tos[], String ccs[], String subject,
        String text)
        throws MessagingException
 {
  MimeMessage message = new MimeMessage(session);
  message.setFrom(new InternetAddress(from));
  for (String to : tos)
   message.addRecipient(RecipientType.TO, new InternetAddress(to));
  for (String cc : ccs)
   message.addRecipient(RecipientType.TO, new InternetAddress(cc));
  message.setSubject(subject);
  message.setText(text);
  Transport.send(message);
 }
}
```



## Julia


```julia

using SMTPClient

addbrackets(s) = replace(s, r"^\s*([^\<\>]+)\s*$", s"<\1>")

function wrapRFC5322(from, to, subject, msg)
    timestr = Libc.strftime("%a, %d %b %Y %H:%M:%S %z", time())
    IOBuffer("Date: $timestr\nTo: $to\nFrom: $from\nSubject: $subject\n\n$msg")
end

function sendemail(from, to, subject, messagebody, serverandport;
                   cc=[], user="", password="", isSSL=true, blocking=true)
    opt = SendOptions(blocking=blocking, isSSL=isSSL, username=user, passwd=password)
    send(serverandport, map(s -> addbrackets(s), vcat(to, cc)), addbrackets(from),
         wrapRFC5322(addbrackets(from), addbrackets(to), subject, messagebody), opt)
end

sendemail("to@example.com", "from@example.com", "TEST", "hello there test message text here", "smtps://smtp.gmail.com",
          user="from@example.com", password="example.com")

```



## Kotlin

To compile and run this program 'javax.mail.jar' will need to be present on your system and added to your classpath. Also if you're using the Google SMTP Server then, as well as requiring a gmail account, you'll probably need to temporarily turn on 'access for less secure apps' to prevent it from being blocked.

```scala
// version 1.1.4-3

import java.util.Properties
import javax.mail.Authenticator
import javax.mail.PasswordAuthentication
import javax.mail.Session
import javax.mail.internet.MimeMessage
import javax.mail.internet.InternetAddress
import javax.mail.Message.RecipientType
import javax.mail.Transport

fun sendEmail(user: String, tos: Array<String>, ccs: Array<String>, title: String,
              body: String, password: String) {
    val props = Properties()
    val host = "smtp.gmail.com"
    with (props) {
        put("mail.smtp.host", host)
        put("mail.smtp.port", "587") // for TLS
        put("mail.smtp.auth", "true")
        put("mail.smtp.starttls.enable", "true")
    }
    val auth = object: Authenticator() {
        protected override fun getPasswordAuthentication() =
            PasswordAuthentication(user, password)
    }
    val session = Session.getInstance(props, auth)
    val message = MimeMessage(session)
    with (message) {
        setFrom(InternetAddress(user))
        for (to in tos) addRecipient(RecipientType.TO, InternetAddress(to))
        for (cc in ccs) addRecipient(RecipientType.TO, InternetAddress(cc))
        setSubject(title)
        setText(body)
    }
    val transport = session.getTransport("smtp")
    with (transport) {
        connect(host, user, password)
        sendMessage(message, message.allRecipients)
        close()
    }
}

fun main(args: Array<String>) {
    val user = "some.user@gmail.com"
    val tos = arrayOf("other.user@otherserver.com")
    val ccs = arrayOf<String>()
    val title = "Rosetta Code Example"
    val body = "This is just a test email"
    val password = "secret"
    sendEmail(user, tos, ccs, title, body, password)
}
```



## Lasso

This example leverages Lasso's built in Email_Send method.


```Lasso
// with a lot of unneeded params.
// sends plain text and html in same email
// simple usage is below
email_send(
	-host = 'mail.example.com',
	-port = 25,
	-timeout = 100,
	-username = 'user.name',
	-password = 'secure_password',
	-priority = 'immediate',
	-to = 'joe@average.com',
	-cc = 'jane@average.com',
	-bcc = 'me@too.com',
	-from = 'lasso@example.com',
	-replyto = 'lassorocks@example.com',
	-sender = 'lasso@example.com',
	-subject = 'Lasso is awesome',
	-body = 'Lasso is awesome, you should try it!',
	-html = '<p>Lasso is <b>awesome</b>, you should try it!</p>',
	-attachments = '/path/to/myFile.txt'
)

// simple usage
// sends plan text email
email_send(
	-host = 'mail.example.com',
	-username = 'user.name',
	-password = 'secure_password',
	-to = 'joe@average.com',
	-from = 'lasso@example.com',
	-subject = 'Lasso is awesome',
	-body = 'Lasso is awesome, you should try it!'
)

```



## Liberty BASIC

This program requires sendemail.exe and sendemail.pl in the same directory, available free from Caspian's SendEmail Site.

```lb

text$       = "This is a simple text message."

from$       = "user@diga.me.es"
username$   = "me@diga.me.es"
'password$   = "***********"
recipient$  = "somebody@gmail.com"
server$     = "auth.smtp.1and1.co.uk:25"
subject$    = chr$( 34) +text$          +chr$( 34)  '   Use quotes to allow spaces in text.
message$    = chr$( 34) +"Hello world." +chr$( 34)
attach$     = "a.txt"
logfile$    = "sendemail.log"

cmd$ = " -f ";  from$;_             'from
       " -t ";  recipient$;_        'to
       " -u ";  subject$;_          'subject
       " -s ";  server$;_           'server
       " -m ";  message$;_          'message
       " -a ";  attach$;_           'file to attach
       " -l ";  logfile$;_          'file to log result in
       " -xu "; username$         'smtp user name
       '" -xp "; password$           'smtp password not given so will ask in a CMD window

run "sendEmail.exe "; cmd$, HIDE

end

```



## Lingo

Lingo has no built-in support for sending email. But this can be achieved e.g. by using Shell Xtra and one of the available command-line SMTP clients.
```lingo
----------------------------------------
-- Sends email via SMTP using senditquiet.exe (15 KB)
-- @param {string} fromAddr
-- @param {string} toAddr - multiple addresses separated with ;
-- @param {string} subject
-- @param {string} message - use "\n" for line breaks
-- @param {string} [cc=VOID] - optional; multiple addresses separated with ;
-- @param {string} [bcc=VOID] - optional; multiple addresses separated with ;
-- @param {propList} [serverProps=VOID] - optional; allows to overwrite default settings
-- @return {bool} success
----------------------------------------
on sendEmail (fromAddr, toAddr, subject, message, cc, bcc, serverProps)

  sx = xtra("Shell").new()

  -- senditquiet.exe in folder "bin" relative to current movie
  sx.shell_setcurrentdir(_movie.path&"bin")

  -- defaults
  host = "smtp.gmail.com"
  protocol = "ssl"
  port = 587
  user = "johndoe"
  pass = "foobar"

  -- if propList 'serverProps' was passed, overwrite defaults
  if ilk(serverProps)=#propList then
    repeat with i = 1 to serverProps.count
      do(serverProps.getPropAt(i)&"="&QUOTE&serverProps[i]&QUOTE)
    end repeat
  end if

  cmd = "senditquiet"
  put " -s "&host after cmd
  put " -protocol "&protocol after cmd
  put " -port "&port after cmd
  put " -u "&user after cmd
  put " -p "&pass after cmd

  put " -f "&QUOTE&fromAddr&QUOTE after cmd
  put " -t "&QUOTE&toAddr&QUOTE after cmd
  put " -subject "&QUOTE&subject&QUOTE after cmd
  put " -body "&QUOTE&message&QUOTE after cmd

  -- optional args
  if not voidP(cc) then put " -cc "&QUOTE&cc&QUOTE after cmd
  if not voidP(bcc) then put " -bcc "&QUOTE&bcc&QUOTE after cmd

  put " 1>nul 2>nul & if errorlevel 1 echo ERROR" after cmd

  res = sx.shell_cmd(cmd)
  return not(res contains "ERROR")
end
```



## LiveCode

LiveCode provides a built-in method that will create an email in the registered mailto: handler on supported OS.

```LiveCode
revMail "help@example.com",,"Help!",field "Message"
```

To create and ''send'' an email in LiveCode requires coding your own smtp client, or using one of a couple of 3rd party stacks.


## LotusScript



```Lotusscript
Dim session As New NotesSession
Dim db As NotesDatabase
Dim doc As NotesDocument
Set db = session.CurrentDatabase
Set doc = New NotesDocument( db )
doc.Form = "Memo"
doc.SendTo = "John Doe"
doc.Subject = "Subject of this mail"
Call doc.Send( False )
```



## Lua


Using [http://w3.impa.br/~diego/software/luasocket/smtp.html LuaSocket's SMTP module] (from the documentation on that page):


```Lua
-- load the smtp support
local smtp = require("socket.smtp")

-- Connects to server "localhost" and sends a message to users
-- "fulano@example.com",  "beltrano@example.com",
-- and "sicrano@example.com".
-- Note that "fulano" is the primary recipient, "beltrano" receives a
-- carbon copy and neither of them knows that "sicrano" received a blind
-- carbon copy of the message.
from = "<luasocket@example.com>"

rcpt = {
  "<fulano@example.com>",
  "<beltrano@example.com>",
  "<sicrano@example.com>"
}

mesgt = {
  headers = {
    to = "Fulano da Silva <fulano@example.com>",
    cc = '"Beltrano F. Nunes" <beltrano@example.com>',
    subject = "My first message"
  },
  body = "I hope this works. If it does, I can send you another 1000 copies."
}

r, e = smtp.send{
  from = from,
  rcpt = rcpt,
  source = smtp.message(mesgt)
}

```



## Mathematica

Mathematica has the built-in function SendMail, example:

```Mathematica
SendMail["From" -> "from@email.com", "To" -> "to@email.com",
 "Subject" -> "Sending Email from Mathematica", "Body" -> "Hello world!",
 "Server" -> "smtp.email.com"]
```

The following options can be specified:

```Mathematica
"To"
"Cc"
"Bcc"
"Subject"
"Body"
"Attachments"
"From"
"Server"
"EncryptionProtocol"
"Fullname"
"Password"
"PortNumber"
"ReplyTo"
"ServerAuthentication"
"UserName"
```

Possible options for EncryptionProtocol are: "SSL","StartTLS" and "TLS". This function should work fine on all the OS's Mathematica runs, which includes the largest 3: Windows, Linux, Mac OSX.



## NewLISP

* using library smtp.lsp

```NewLISP
(module "smtp.lsp")
(SMTP:send-mail "user@asite.com" "somebody@isp.com" "Greetings" "How are you today? - john doe -" "smtp.asite.com" "user" "password")
```



## Nim

Compile with <code>nim c -d:ssl mail</code>

```nim
import smtp, net

proc sendMail(fromAddr: string; toAddrs, ccAddrs: seq[string];
              subject, message, login, password: string;
              server = "smtp.gmail.com"; port = Port 465; ssl = true) =
  var msg = createMessage(subject, message, toAddrs, ccAddrs)
  var s = connect(server, port, ssl, debug = true)
  s.auth(login, password)
  s.sendmail(fromAddr, toAddrs, $msg)

sendMail(fromAddr = "nim@gmail.com",
         toAddrs  = @["someone@example.com"],
         ccAddrs  = @[],
         subject  = "Hi from Nim",
         message  = "Nim says hi!\nAnd bye again!",
         login    = "nim@gmail.com",
         password = "XXXXXX")
```



## OCaml

* using the library [http://www.linux-nantes.org/~fmonnier/OCaml/smtp-mail/ smtp-mail-0.1.3]

```ocaml
let h = Smtp.connect "smtp.gmail.fr";;
Smtp.helo h "hostname";;
Smtp.mail h "<john.smith@example.com>";;
Smtp.rcpt h "<john-doe@example.com>";;
let email_header = "\
From: John Smith <john.smith@example.com>
To: John Doe <john-doe@example.com>
Subject: surprise";;
let email_msg = "Happy Birthday";;
Smtp.data h (email_header ^ "\r\n\r\n" ^ email_msg);;
Smtp.quit h;;
```



## Perl

This subroutine throws an appropriate error if it fails to connect to the server or authenticate. It should work on any platform Perl does.


```perl
use Net::SMTP;
use Authen::SASL;
  # Net::SMTP's 'auth' method needs Authen::SASL to work, but
  # this is undocumented, and if you don't have the latter, the
  # method will just silently fail. Hence we explicitly use
  # Authen::SASL here.

sub send_email
 {my %o =
     (from => '', to => [], cc => [],
      subject => '', body => '',
      host => '', user => '', password => '',
      @_);
  ref $o{$_} or $o{$_} = [$o{$_}] foreach 'to', 'cc';

  my $smtp = new Net::SMTP($o{host} ? $o{host} : ())
      or die "Couldn't connect to SMTP server";

  $o{password} and
     $smtp->auth($o{user}, $o{password}) ||
     die 'SMTP authentication failed';

  $smtp->mail($o{user});
  $smtp->recipient($_) foreach @{$o{to}}, @{$o{cc}};
  $smtp->data;
  $o{from} and $smtp->datasend("From: $o{from}\n");
  $smtp->datasend('To: ' . join(', ', @{$o{to}}) . "\n");
  @{$o{cc}} and $smtp->datasend('Cc: ' . join(', ', @{$o{cc}}) . "\n");
  $o{subject} and $smtp->datasend("Subject: $o{subject}\n");
  $smtp->datasend("\n$o{body}");
  $smtp->dataend;

  return 1;}
```


An example call:


```perl
send_email
   from => 'A. T. Tappman',
   to => ['suchandsuch@example.com', 'soandso@example.org'],
   cc => 'somebodyelse@example.net',
   subject => 'Important message',
   body => 'I yearn for you tragically.',
   host => 'smtp.example.com:587',
   user => 'tappman@example.com',
   password => 'yossarian';
```


If the <code>host</code> parameter is omitted, <code>send_email</code> falls back on the <code>SMTP_Hosts</code> defined in <code>Net::Config</code>. Hence, only two arguments are strictly necessary:


```perl
send_email
   to => 'suchandsuch@example.com',
   user => 'tappman@example.com';
```


LWP can send email by a POST to a <code>mailto:</code> URL.  The message is given as a HTTP request.  This is mainly of interest for treating different types of URLs in a common way.  LWP sends merely by running the <code>sendmail</code> program, or on MacOS classic by SMTP (to <code>SMTPHOSTS</code> environment variable).  For reference, the <code>$ua-&gt;post()</code> method does not suit since it constructs a message as MIME "form data".


```perl
use strict;
use LWP::UserAgent;
use HTTP::Request;

sub send_email {
  my ($from, $to, $cc, $subject, $text) = @_;

  my $ua = LWP::UserAgent->new;
  my $req = HTTP::Request->new (POST => "mailto:$to",
                                [ From => $from,
                                  Cc => $cc,
                                  Subject => $subject ],
                                $text);
  my $resp = $ua->request($req);
  if (! $resp->is_success) {
    print $resp->status_line,"\n";
  }
}

send_email('from-me@example.com', 'to-foo@example.com', '',
           "very important subject",
           "Body text\n");
```



## Perl 6


```perl6
use Email::Simple;

my $to      = 'mail@example.com';
my $from    = 'me@example.com';
my $subject = 'test';
my $body    = 'This is a test.';

my $email = Email::Simple.create(
    :header[['To', $to], ['From', $from], ['Subject', $subject]],
    :body($body)
);

say ~$email;

# Note that the following will fail without an actual smtp server that
# will accept anonymous emails on port 25 (Not very common anymore).
# Most public email servers now require authentication and encryption.

my $smtp-server = 'smtp.example.com';
my $smtp-port   = 25;

await IO::Socket::Async.connect($smtp-server, $smtp-port).then(
    -> $smtp {
        if $smtp.status {
            given $smtp.result {
                react {
                    whenever .Supply() -> $response {
                        if $response ~~ /^220/ {
                            .print( join "\r\n",
                                "EHLO $smtp-server",
                                "MAIL FROM:<{$email.from}>",
                                "RCPT TO:<{$email.to}>",
                                "DATA", $email.body,
                                '.', ''
                            )
                        }
                        elsif $response ~~ /^250/ {
                            .print("QUIT\r\n");
                            done
                        }
                        else {
                           say "Send email failed with: $response";
                           done
                        }
                    }
                    .close
                }
            }
        }
    }
)
```



## Phix

Obviously, USER/PWD/URL/etc. would all need altering for your details.
For gmail, make sure you enable https://myaccount.google.com/lesssecureapps

```Phix
include builtins\libcurl.e

constant USER = "you@gmail.com",
         PWD = "secret",
         URL = "smtps://smtp.gmail.com:465",
         FROM = "sender@gmail.com",
         TO = "addressee@email.com",
         CC = "info@example.org",
         FMT = "Date: Mon, 13 Jun 2018 11:30:00 +0100\r\n"&
               "To: %s\r\n"&
               "From: %s (Example User)\r\n"&
               "Cc: %s (Another example User)\r\n"&
               "Subject: Sanding mail via Phix\r\n"&
               "\r\n"&
               "This mail is being sent by a Phix program.\r\n"&
               "\r\n"&
               "It connects to the GMail SMTP server, by far the most popular mail program of all.\r\n"&
               "Which is, however, probably not written in Phix.\r\n"

function read_callback(atom pbuffer, integer size, nmemb, atom pUserData)
-- copy a maximum of size*nmemb bytes into pbuffer
    if size==0 or nmemb==0 or size*nmemb<1 then return 0 end if
    {integer sent, integer len, atom pPayload} = peekns({pUserData,3})
    integer bytes_written = min(size*nmemb,len-sent)
    mem_copy(pbuffer,pPayload+sent,bytes_written)
    sent += bytes_written
    pokeN(pUserData,sent,machine_word())
--  printf(2, "*** We read %d bytes from file\n", bytes_written)
    return bytes_written
end function
constant read_cb = call_back({'+',routine_id("read_callback")})

constant string payload_text = sprintf(FMT,{TO,FROM,CC})

curl_global_init()
CURLcode res = CURLE_OK
atom slist_recipients = NULL
atom curl = curl_easy_init()
curl_easy_setopt(curl, CURLOPT_USERNAME, USER)
curl_easy_setopt(curl, CURLOPT_PASSWORD, PWD)
curl_easy_setopt(curl, CURLOPT_URL, URL)
curl_easy_setopt(curl, CURLOPT_USE_SSL, CURLUSESSL_ALL)
curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 0)
curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 0)
curl_easy_setopt(curl, CURLOPT_MAIL_FROM, FROM)
slist_recipients = curl_slist_append(slist_recipients, TO)
slist_recipients = curl_slist_append(slist_recipients, CC)
curl_easy_setopt(curl, CURLOPT_MAIL_RCPT, slist_recipients)
curl_easy_setopt(curl, CURLOPT_READFUNCTION, read_cb);
atom pUserData = allocate(machine_word()*3),
     pPayload = allocate_string(payload_text)
pokeN(pUserData,{0,length(payload_text),pPayload},machine_word())
curl_easy_setopt(curl, CURLOPT_READDATA, pUserData)
curl_easy_setopt(curl, CURLOPT_UPLOAD, true)
--curl_easy_setopt(curl, CURLOPT_VERBOSE, true)
res = curl_easy_perform(curl)
if res!=CURLE_OK then
    printf(2, "curl_easy_perform() failed: %d (%s)\n",{res,curl_easy_strerror(res)})
end if
curl_slist_free_all(slist_recipients)
curl_easy_cleanup(curl)
curl_global_cleanup()
```



## PHP


```php
mail('hello@world.net', 'My Subject', "A Message!", "From: my@address.com");
```



## PicoLisp

PicoLisp has a built-in '[http://software-lab.de/doc/refM.html#mail mail]'
function. A minimal call would be

```PicoLisp
(mail "localhost" 25 "me@from.org" "you@to.org" "Subject" NIL "Hello")
```

Instead of "Hello" an arbitrary number of arguments may follow (possibly
containing executable expressions) for the message body.

The 6th argument (here 'NIL') may specify a list of attachments.


## Pike

Untested:


```pike
int main(){
   string to         = "some@email.add";
   string subject    = "Hello There.";
   string from       = "me@myaddr.ess";
   string msg        = "Hello there! :)";

   Protocols.SMTP.Client()->simple_mail(to,subject,from,msg);
}
```



## PowerShell

PowerShell has a cmdlet named 'Send-MailMessage', and this is the easiest way to use it.

The parameters are splatted with a hashtable:

```PowerShell

[hashtable]$mailMessage = @{
    From = "weirdBoy@gmail.com"
    To = "anudderBoy@YourDomain.com"
    Cc = "daWaghBoss@YourDomain.com"
    Attachment = "C:\temp\Waggghhhh!_plan.txt"
    Subject = "Waggghhhh!"
    Body = "Wagggghhhhhh!"
    SMTPServer = "smtp.gmail.com"
    SMTPPort = "587"
    UseSsl = $true
    ErrorAction = "SilentlyContinue"
}

Send-MailMessage @mailMessage

```



## PureBasic


```Purebasic
InitNetwork()

CreateMail(0, "from@mydomain.com", "This is the Subject")

SetMailBody(0, "Hello   " + Chr(10) + "This is a mail !")

AddMailRecipient(0, "test@yourdomain.com", #PB_Mail_To)

AddMailRecipient(0, "test2@yourdomain.com", #PB_Mail_Cc)

If SendMail(0, "smtp.mail.com")
    MessageRequester("Information", "Mail correctly sent !")
Else
    MessageRequester("Error", "Can't sent the mail !")
EndIf
```



## Python


### Python: POSIX

The function returns a dict of any addresses it could not forward to;
other connection problems raise [http://docs.python.org/library/smtplib.html?highlight=smtplib#smtplib.SMTP.sendmail errors].

Tested on Windows, it should work on all [[wp:POSIX|POSIX]] platforms.


```python
import smtplib

def sendemail(from_addr, to_addr_list, cc_addr_list,
              subject, message,
              login, password,
              smtpserver='smtp.gmail.com:587'):
    header  = 'From: %s\n' % from_addr
    header += 'To: %s\n' % ','.join(to_addr_list)
    header += 'Cc: %s\n' % ','.join(cc_addr_list)
    header += 'Subject: %s\n\n' % subject
    message = header + message

    server = smtplib.SMTP(smtpserver)
    server.starttls()
    server.login(login,password)
    problems = server.sendmail(from_addr, to_addr_list, message)
    server.quit()
    return problems
```


Example use:

```python
sendemail(from_addr    = 'python@RC.net',
          to_addr_list = ['RC@gmail.com'],
          cc_addr_list = ['RC@xx.co.uk'],
          subject      = 'Howdy',
          message      = 'Howdy from a python function',
          login        = 'pythonuser',
          password     = 'XXXXX')
```


```txt
Message-ID: <4a4a1e78.0717d00a.1ba8.ffcfdbdd@xx.google.com>
Date: Tue, 30 Jun 2009 22:04:56 -0700 (PDT)
From: python@RC.net
To: RC@gmail.com
Cc: RC@xx.co.uk
Subject: Howdy

Howdy from a python function

```



### Python: Windows

Using Outlook COM server with the Pywin32 library.


```python
import win32com.client

def sendmail(to, title, body):
    olMailItem = 0
    ol = win32com.client.Dispatch("Outlook.Application")
    msg = ol.CreateItem(olMailItem)
    msg.To = to
    msg.Subject = title
    msg.Body = body
    msg.Send()
    ol.Quit()

sendmail("somebody@somewhere", "Title", "Hello")
```



## R

R does not have a built-in facility for sending emails though there is a package for this on CRAN: '''[https://cran.r-project.org/web/packages/mail/ mail]'''.


### Windows

Using Outlook COM server with the '''[http://www.omegahat.net/RDCOMClient/ RDCOMClient]''' package.


```r
library(RDCOMClient)

send.mail <- function(to, title, body) {
  olMailItem <- 0
  ol <- COMCreate("Outlook.Application")
  msg <- ol$CreateItem(olMailItem)
  msg[["To"]] <- to
  msg[["Subject"]] <- title
  msg[["Body"]] <- body
  msg$Send()
  ol$Quit()
}

send.mail("somebody@somewhere", "Title", "Hello")
```



## Racket


Racket has a built-in library for sending e-mails:

```racket

#lang racket

;; using sendmail:
(require net/sendmail)
(send-mail-message
 "sender@somewhere.com" "Some Subject"
 '("recipient@elsewhere.com" "recipient2@elsewhere.com")
 '("cc@elsewhere.com")
 '("bcc@elsewhere.com")
 (list "Some lines of text" "go here."))

;; and using smtp (and adding more headers here):
(require net/head net/smtp)
(smtp-send-message
 "192.168.0.1"
 "Sender <sender@somewhere.com>"
 '("Recipient <recipient@elsewhere.com>")
 (standard-message-header
  "Sender <sender@somewhere.com>"
  '("Recipient <recipient@elsewhere.com>")
  '() ; CC
  '() ; BCC
  "Subject")
 '("Hello World!"))

```



## REBOL


```rebol
send user@host.dom "My message"
```



## REXX

There is a REXX program to send email via REXX,   I'm trying to locate the author to get permission to include it here on Rosetta Code.




## Ring


```ring

load "stdlib.ring"
See "Send email..." + nl
sendemail("smtp://smtp.gmail.com",
          "calmosoft@gmail.com",
          "password",
          "calmosoft@gmail.com",
          "calmosoft@gmail.com",
          "calmosoft@gmail.com",
          "Sending email from Ring",
          "Hello
           How are you?
           Are you fine?
           Thank you!
           Greetings,
           CalmoSoft")
see "Done.." + nl

```

Output:

```txt

Hello
How are you?
Are you fine?
Thank you!
Greetings,
CalmoSoft

```



## Ruby

Uses the {{libheader|RubyGems}} gems [http://tmail.rubyforge.org TMail] which allows us to manipulate email objects conveniently, and [http://mime-types.rubyforge.org/ mime-types] which guesses a file's mime type based on its filename.


```ruby
require 'base64'
require 'net/smtp'
require 'tmail'
require 'mime/types'

class Email
  def initialize(from, to, subject, body, options={})
    @opts = {:attachments => [], :server => 'localhost'}.update(options)
    @msg = TMail::Mail.new
    @msg.from    = from
    @msg.to      = to
    @msg.subject = subject
    @msg.cc      = @opts[:cc]  if @opts[:cc]
    @msg.bcc     = @opts[:bcc] if @opts[:bcc]

    if @opts[:attachments].empty?
      # just specify the body
      @msg.body = body
    else
      # attach attachments, including the body
      @msg.body = "This is a multi-part message in MIME format.\n"

      msg_body = TMail::Mail.new
      msg_body.body = body
      msg_body.set_content_type("text","plain", {:charset => "ISO-8859-1"})
      @msg.parts << msg_body

      octet_stream = MIME::Types['application/octet-stream'].first

      @opts[:attachments].select {|file| File.readable?(file)}.each do |file|
        mime_type = MIME::Types.type_for(file).first || octet_stream
        @msg.parts << create_attachment(file, mime_type)
      end
    end
  end
  attr_reader :msg

  def create_attachment(file, mime_type)
    attach = TMail::Mail.new
    if mime_type.binary?
      attach.body = Base64.encode64(File.read(file))
      attach.transfer_encoding = 'base64'
    else
      attach.body = File.read(file)
    end
    attach.set_disposition("attachment", {:filename => file})
    attach.set_content_type(mime_type.media_type, mime_type.sub_type, {:name=>file})
    attach
  end

  # instance method to send an Email object
  def send
    args = @opts.values_at(:server, :port, :helo, :username, :password, :authtype)
    Net::SMTP.start(*args) do |smtp|
      smtp.send_message(@msg.to_s, @msg.from[0], @msg.to)
    end
  end

  # class method to construct an Email object and send it
  def self.send(*args)
    self.new(*args).send
  end
end

Email.send(
  'sender@sender.invalid',
  %w{ recip1@recipient.invalid recip2@example.com },
  'the subject',
  "the body\nhas lines",
  {
    :attachments => %w{ file1 file2 file3 },
    :server => 'mail.example.com',
    :helo => 'sender.invalid',
    :username => 'user',
    :password => 'secret'
  }
)
```



## SAS


```sas
filename msg email
   to="afriend@someserver.com"
   cc="anotherfriend@somecompany.com"
   subject="Important message"
;

data _null_;
   file msg;
   put "Hello, Connected World!";
run;
```



## Scala

```Scala
import java.util.Properties

import javax.mail.internet.{ InternetAddress, MimeMessage }
import javax.mail.Message.RecipientType
import javax.mail.{ Session, Transport }

/** Mail constructor.
 *  @constructor Mail
 *  @param host Host
 */
class Mail(host: String) {
  val session = Session.getDefaultInstance(new Properties() { put("mail.smtp.host", host) })

  /** Send email message.
   *
   *  @param from From
   *  @param tos Recipients
   *  @param ccs CC Recipients
   *  @param subject Subject
   *  @param text Text
   *  @throws MessagingException
   */
  def send(from: String, tos: List[String], ccs: List[String], subject: String, text: String) {
    val message = new MimeMessage(session)
    message.setFrom(new InternetAddress(from))
    for (to <- tos)
      message.addRecipient(RecipientType.TO, new InternetAddress(to))
    for (cc <- ccs)
      message.addRecipient(RecipientType.TO, new InternetAddress(cc))
    message.setSubject(subject)
    message.setText(text)
    Transport.send(message)
  }
}
```



## SQL PL

{{works with|Db2 LUW}} version 9.7 or higher.
With SQL PL:
You must first set the SMTP server in the database from which you want to send the message.

```sql pl

UPDATE DB CFG FOR myDb USING SMTP_SERVER 'smtp.ibm.com';

CALL UTL_MAIL.SEND ('senderAccount@myDomain.com','recipientAccount@yourDomain.com', 'copy@anotherDomain.com', NULL, 'The subject of the message', 'The content of the message');

```

Output:

```txt

db2 => UPDATE DB CFG FOR myDb USING SMTP_SERVER 'smtp.ibm.com';
DB20000I  The UPDATE DATABASE CONFIGURATION command completed successfully.
db2 => CALL UTL_MAIL.SEND ('senderAccount@myDomain.com','recipientAccount@yourDomain.com', NULL, NULL, 'The subject of the message', 'The content of the message');

  Return Status = 0

```

If you receive a "SQL1336N  The remote host "smtp.ibm.com" was not found.  SQLSTATE=08001" message, it is because the SMTP_SERVER is not valid.
More information in the [https://www.ibm.com/support/knowledgecenter/en/SSEPGG_11.1.0/com.ibm.db2.luw.apdv.sqlpl.doc/doc/r0055177.html IBM Knowledge center]


## Tcl

Also may use the [http://tls.sourceforge.net/ tls] package (needed for sending via gmail).

```tcl
package require smtp
package require mime
package require tls

set gmailUser *******
set gmailPass hunter2; # Hello, bash.org!

proc send_simple_message {recipient subject body} {
    global gmailUser gmailPass

    # Build the message
    set token [mime::initialize -canonical text/plain -string $body]
    mime::setheader $token Subject $subject

    # Send it!
    smtp::sendmessage $token -userame $gamilUser -password $gmailPass \
            -recipients $recipient -servers smtp.gmail.com -ports 587

    # Clean up
    mime::finalize $token
}

send_simple_message recipient@example.com "Testing" "This is a test message."
```



## TUSCRIPT

works only with Windows, on Linux OS it is possible to send an email by using the Execute function

```tuscript

$$ MODE TUSCRIPT

system=SYSTEM ()

IF (system=="WIN") THEN
SET to="name@domain.org"
SET cc="name@domain.net"
subject="test"
text=*
DATA how are you?

status = SEND_MAIL (to,cc,subject,text,-)

ENDIF

```



## TXR


```txr
#!/usr/bin/txr
@(next :args)
@(cases)
@TO
@SUBJ
@  (maybe)
@CC
@  (or)
@  (bind CC "")
@  (end)
@(or)
@  (throw error "must specify at least To and Subject")
@(end)
@(next *stdin*)
@(collect)
@BODY
@(end)
@(output (open-command `mail -s "@SUBJ" -a CC: "@CC" "@TO"` "w"))
@(repeat)
@BODY
@(end)
.
@(end)
```


```txt
$ ./sendmail.txr linux-kernel@vger.kernel.org "Patch to rewrite scheduler #378"
Here we go
again ...
[Ctrl-D]
$
```


{{omit from|TI-83 BASIC}} {{omit from|TI-89 BASIC}} {{omit from|JavaScript}} <!-- Does not have network access. -->
## VBA


```vb
Option Explicit
Const olMailItem = 0

Sub SendMail(MsgTo As String, MsgTitle As String, MsgBody As String)
    Dim OutlookApp As Object, Msg As Object
    Set OutlookApp = CreateObject("Outlook.Application")
    Set Msg = OutlookApp.CreateItem(olMailItem)
    With Msg
        .To = MsgTo
        .Subject = MsgTitle
        .Body = MsgBody
        .Send
    End With
    Set OutlookApp = Nothing
End Sub

Sub Test()
    SendMail "somebody@somewhere", "Title", "Hello"
End Sub
```



## VBScript


```vb

Function send_mail(from,recipient,cc,subject,message)
	With CreateObject("CDO.Message")
		.From = from
		.To = recipient
		.CC = cc
		.Subject = subject
		.Textbody = message
		.Configuration.Fields.Item _
			("http://schemas.microsoft.com/cdo/configuration/sendusing") = 2
		.Configuration.Fields.Item _
			("http://schemas.microsoft.com/cdo/configuration/smtpserver") = _
		        "mystmpserver"
		.Configuration.Fields.Item _
		    ("http://schemas.microsoft.com/cdo/configuration/smtpserverport") = 25
		.Configuration.Fields.Update
		.Send
	End With
End Function

Call send_mail("Alerts@alerts.org","jkspeed@jkspeed.org","","Test Email","this is a test message")

```

