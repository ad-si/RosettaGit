+++
title = "HTTPS/Client-authenticated"
description = ""
date = 2018-11-28T08:48:38Z
aliases = []
[extra]
id = 4291
[taxonomies]
categories = ["task", "Programming environment operations"]
tags = []
languages = [
  "csharp",
  "go",
  "julia",
  "kotlin",
  "lasso",
  "perl",
  "perl_6",
  "phix",
  "picolisp",
  "python",
  "racket",
  "ruby",
  "scala",
  "tcl",
  "zkl",
]
+++

## Task

{{task|Programming environment operations}}[[Category:Networking and Web Interaction]]

Demonstrate how to connect to a web server over HTTPS where that server requires that the client present a certificate to prove who (s)he is. Unlike with the [[HTTPS request with authentication]] task, it is ''not'' acceptable to perform the authentication by a username/password or a set cookie.

This task is in general useful for use with [[Creating a SOAP Client|webservice client]]s as it offers a high level of assurance that the client is an acceptable counterparty for the server. For example, [http://aws.amazon.com/ Amazon Web Services] uses this style of authentication.

## C#
```c#

using System;
using System.Net;

class Program
{
    class MyWebClient : WebClient
    {
        protected override WebRequest GetWebRequest(Uri address)
        {
            HttpWebRequest request = (HttpWebRequest)base.GetWebRequest(address);
            request.ClientCertificates.Add(new X509Certificate());
            return request;
        }
    }
    static void Main(string[] args)
    {
        var client = new MyWebClient();

        var data = client.DownloadString("https://example.com");

        Console.WriteLine(data);
    }
}

```




## Go


```Go
package main

import (
	"crypto/tls"
	"io/ioutil"
	"log"
	"net/http"
)

func main() {

	// load key pair
	cert, err := tls.LoadX509KeyPair(
		"./client.local.tld/client.local.tld.crt",
		"./client.local.tld/client.local.tld.key",
	)

	if err != nil {
		log.Fatal("Error while loading x509 key pair", err)
	}

	// Create TLS Config in order to had client certificate
	tlsConfig := &tls.Config{Certificates: []tls.Certificate{cert}}

	tlsConfig.BuildNameToCertificate()
	transport := &http.Transport{TLSClientConfig: tlsConfig}

	// create http client with our custom transport with TLS config
	client := &http.Client{Transport: transport}

	res, err := client.Get("https://www.example.com/")
	if err != nil {
		log.Fatal(err)
	}
	contents, err := ioutil.ReadAll(res.Body)
	log.Print(string(contents))

}

```



## Julia


```julia
using HTTP, MbedTLS

conf = MbedTLS.SSLConfig(true, log_secrets="/utl/secret_key_log.log")
resp = HTTP.get("https://httpbin.org/ip", sslconfig=conf)

println(resp)

```
```txt

HTTP.Messages.Response:
"""
HTTP/1.1 200 OK
Connection: keep-alive
Server: gunicorn/19.9.0
Date: Wed, 28 Nov 2018 08:42:25 GMT
Content-Type: application/json
Content-Length: 30
Access-Control-Allow-Origin: *
Access-Control-Allow-Credentials: true
Via: 1.1 vegur

{
  "origin": "104.28.10.103"
}
"""

```



## Kotlin


```scala
// version 1.2.0

import java.security.KeyStore
import javax.net.ssl.KeyManagerFactory
import javax.net.ssl.SSLContext
import javax.net.ssl.HttpsURLConnection
import java.net.URL
import java.io.FileInputStream
import java.io.InputStreamReader
import java.io.BufferedReader

fun getSSLContext(p12Path: String, password: String): SSLContext {
    val ks = KeyStore.getInstance("pkcs12")
    val fis = FileInputStream(p12Path)
    val pwd = password.toCharArray()
    ks.load(fis, pwd)
    val kmf = KeyManagerFactory.getInstance("PKIX")
    kmf.init(ks, pwd)
    val sc = SSLContext.getInstance("TLS")
    sc.init(kmf.keyManagers, null, null)
    return sc
}

fun main(args: Array<String>) {
    // The .p12 file contains the client certificate and private key
    val sc = getSSLContext("whatever.p12", "password")
    val url = URL("https://somehost.com")
    val con = url.openConnection() as HttpsURLConnection
    con.sslSocketFactory = sc.socketFactory
    val isr = InputStreamReader(con.inputStream)
    val br = BufferedReader(isr)
    while (true) {
        val line = br.readLine()
        if (line == null) break
        println(line)
    }
}
```



## Lasso


```Lasso
local(sslcert = file('myCert.pem'))
local(x = curl('https://sourceforge.net'))
#x->set(CURLOPT_SSLCERT, #sslcert->readstring)
#sslcert->close
#x->result->asString
```


=={{header|Mathematica}} / {{header|Wolfram Language}}==

```Mathematica
a = RunThrough["curl -E myCert.pem https://www.example.com", 1]
For[ i=0, i < Length[a] , i++, SomeFunction[a]]
```


## Perl


```python
#!/usr/bin/env perl -T
use 5.018_002;
use warnings;
use LWP;

our $VERSION = 1.000_000;

my $ua = LWP::UserAgent->new(
    ssl_opts => {
        SSL_cert_file   => 'certificate.pem',
        SSL_key_file    => 'key.pem',
        verify_hostname => 1,
    }
);
my $req = HTTP::Request->new( GET => 'https://www.example.com' );
my $res = $ua->request($req);
if ( $res->is_success ) {
    say $res->content;
}
else {
    say $res->status_line;
}
```



## Perl 6


```perl6

# cert creation commands

# openssl req -newkey rsa:4096 -keyout my_key.pem -out my_csr.pem -nodes -subj "/CN=ME"

# openssl x509 -req -in my_csr.pem -signkey my_key.pem -out my_cert.pem

use v6;
use OpenSSL;

my $host = "github.com";

my $ssl = OpenSSL.new(:client);

$ssl.use-certificate-file("./my_cert.pem");
$ssl.use-privatekey-file("./my_key.pem");
$ssl.check-private-key;


my $s = IO::Socket::INET.new(:$host, :port(443));

$ssl.set-socket($s);
$ssl.set-connect-state;
$ssl.connect;
$ssl.write("GET / HTTP/1.1\r\n\r\n");
say $ssl.read(1024);
$ssl.close;
$s.close;


```



## Phix

Exactly the same as the HTTP#Phix task, except for the CURLOPT_SSLCERT part.

```Phix
include builtins\libcurl.e
curl_global_init()
atom curl = curl_easy_init()
curl_easy_setopt(curl, CURLOPT_URL, "https://sourceforge.net")
integer fn = open("myCert.pem","r")
curl_easy_setopt(curl, CURLOPT_SSLCERT, get_text(fn))
close(fn)
object res = curl_easy_perform_ex(curl)
curl_easy_cleanup(curl)
curl_global_cleanup()

puts(1,res)
```



## PicoLisp


```PicoLisp
(in '(curl "-E" "myCert.pem" "https://www.example.com")
   (while (line)
      (doSomeProcessingWithLine @) ) )
```



## Python


```python
import httplib

connection = httplib.HTTPSConnection('www.example.com',cert_file='myCert.PEM')
connection.request('GET','/index.html')
response = connection.getresponse()
data = response.read()

```



## Racket


Skeleton code to connect to a server:

```racket

#lang racket
(require openssl/mzssl)
(define ctx (ssl-make-client-context))
(ssl-set-verify! ctx #t) ; verify the connection
(ssl-load-verify-root-certificates! ctx "my-cert.pem")
(define-values [I O] (ssl-connect "www.example.com" 443 ctx))

```



## Ruby


```Ruby
require 'uri'
require 'net/http'

uri = URI.parse('https://www.example.com')
pem = File.read("/path/to/my.pem")
cert = OpenSSL::X509::Certificate.new(pem)
key = OpenSSL::PKey::RSA.new(pem)
response = Net::HTTP.start(uri.host, uri.port, use_ssl: true,
                           cert: cert, key: key) do |http|
  request = Net::HTTP::Get.new uri
  http.request request
end
```



## Scala


```Scala
import java.io.FileInputStream
import java.net.URL
import java.security.KeyStore

import javax.net.ssl.{HttpsURLConnection, KeyManagerFactory, SSLContext}

import scala.io.BufferedSource

object ClientAuthenticated extends App {

  val con: HttpsURLConnection =
    new URL("https://somehost.com").openConnection().asInstanceOf[HttpsURLConnection]

  def getSSLContext(p12Path: String, password: String): SSLContext = {
    val ks = KeyStore.getInstance("pkcs12")
    val pwd = password.toCharArray
    ks.load(new FileInputStream(p12Path), pwd)
    val kmf = KeyManagerFactory.getInstance("PKIX")
    kmf.init(ks, pwd)
    val sc = SSLContext.getInstance("TLS")
    sc.init(kmf.getKeyManagers, null, null)
    sc
  }

  // The .p12 file contains the client certificate and private key
  HttpsURLConnection.setDefaultSSLSocketFactory(getSSLContext("whatever.p12", "password").getSocketFactory)
  new BufferedSource(con.getInputStream).getLines.foreach(println(_))

}
```


## Tcl

Uses the [http://tls.sourceforge.net Tls] package.

```tcl
package require http
package require tls

set cert myCert.p12
http::register https 443 [list \
    ::tls::socket -certfile $cert -password getPass]
proc getPass {} {
    return "myPassword";  # Just a noddy example...
}

# Make a secure authenticated connection
set token [http::geturl https://verysecure.example.com/]

# Now as for conventional use of the “http” package
set data [http::data $token]
http::cleanup $token
```



## zkl

Uses libCurl.

```zkl
var CURL=Import("zklCurl"), c=CURL();
c.setOpt("SSLCERT","certFile.pem"); c.setOpt("SSLCERTTYPE","pem");
c.get("http://zenkinetic.com");  // lame example to show how to read
```


