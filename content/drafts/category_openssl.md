+++
title = "Category:OpenSSL"
description = ""
date = 2012-03-26T20:24:45Z
aliases = []
[extra]
id = 4471
[taxonomies]
categories = []
tags = []
+++

{{library}}
From the [http://www.openssl.org/ OpenSSL site]: <cite>The OpenSSL Project is a collaborative effort to develop a robust, commercial-grade, full-featured, and Open Source toolkit implementing the Secure Sockets Layer (SSL v2/v3) and Transport Layer Security (TLS v1) protocols as well as a full-strength general purpose cryptography library.</cite>

OpenSSL provides libraries for [[C|C language]], and the [http://www.openssl.org/docs/apps/openssl.html <code>openssl(1)</code>] command for [[UNIX Shell|shell scripts]]. Several other languages provide access to OpenSSL. [[Ruby]] has <code>'openssl'</code> in its standard library.

OpenSSL has two libraries:

* [http://www.openssl.org/docs/crypto/crypto.html <code>crypto(3)</code>] implements cryptographic algorithms, such as ciphers, message digests, and X.509 certificates. <code>crypto(3)</code> also exposes features, including [[arbitrary-precision integers (included)|arbitrary-precision integers]] called BIGNUMs, and a secure random number generator.
* [http://www.openssl.org/docs/ssl/ssl.html <code>ssl(3)</code>] implements SSL and TLS.

To link OpenSSL to a C program, a typical [[Unix]] command is

 cc -o program program.c -lcrypto -lssl
