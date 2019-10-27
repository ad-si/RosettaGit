+++
title = "Talk:URL encoding"
description = ""
date = 2011-08-13T17:06:38Z
aliases = []
[extra]
id = 9938
[taxonomies]
categories = []
tags = []
+++

I believe that some symbols are usually not encoded in URLs. The exact list varies, but they usually include the period (.) and hyphen (-), and sometimes underscore (_). Should we not include those? This is complicated by the fact that there are several standards on URI syntax (RFC 1738, RFC 3986), additional restrictions for specific protocols, like HTTP (e.g. the plus character (+) is encoded in form data), as well as lots of slightly different implementations across languages (and sometimes even in the same language). So whatever solutions that people present that use library functions will invariably encode a slightly smaller set of characters than in the task specification. It would be hard to keep all the solutions consistent. --[[Special:Contributions/98.210.210.193|98.210.210.193]] 07:06, 17 June 2011 (UTC)
: Nailing this down would help since there are two tasks dependent on this (URL encoding and decoding).  Sorting out and making sense of the current set of RFCs is probably a prerequisite. 
:: RFC 3986 is about URIs and updates 1738  - these two appear to be the most relavent RFCs
::: RFC 1738 is about URLs
::: Superseding RFCs may only supersede some of the functionality (such as for a protocol like gopher)
::: Superseded RFCs should be ignored
::: As this task seems to be about HTTP URLs we should ignore some of the RFCs for other protocols like mail, tn3270, etc.   There are also RFCs that extend functionality such as for extensions of protocols such as WebDav which would seem not to be part of the core task.  Also, some of these RFC's have been marked as 'historic' a polite way of sayng obsolete.
:::: This task is not restricted to HTTP urls, and can be applied to any string that can be encoded into this format.  
:: I believe the example of an encoded url is in error (or not described properly).  Specifically, 
::: The string "<nowiki>http://foo bar/</nowiki>" would be encoded as "<nowiki>http%3A%2F%2Ffoo%20bar%2F</nowiki>".  
::: Would only be encoded if this URL were being passed as data within another URL.  See the RFC sections on Reserved Characters and When to Encode or Decode.
:::: The task is to demonstrate the encoding mechanism, rather than when to use the application of this, so we can assume that this will be used in applications where the URL string requires encoding. --[[User:Markhobley|Markhobley]] 13:01, 17 June 2011 (UTC)
::::: Fair enough. --[[User:Dgamey|Dgamey]] 01:43, 19 June 2011 (UTC)
:: There probably should be soome required input(s) and output(s).  I noticed the perl example is very cryptic using a library and provides no output.  The output it would produce doesn't match the 'example' string as it only encodes data in the path portion of the URL and not the entire URL.
: --[[User:Dgamey|Dgamey]] 09:54, 17 June 2011 (UTC)
The point of encoding strings is to avoid confusion.  Some characters, such as '+' and '?', tend to be metacharaters used by CGI interface (? for begining of query string, + for separating parameters), while '\r' '\n' must be encoded because they signify end of input; also encoding can carry whatever text not in low 127 bits and printable with "normal text", so dumber server or client software won't get totally confused.  I don't know how much we need to conform to various RFCs here, maybe common sense would suffice.  In principle you can escape the "http" too, and still conform to most standards, but that would be utterly pointless, wouldn't it? --[[User:Ledrug|Ledrug]] 02:23, 19 June 2011 (UTC)

:: I suppose as a bonus, we could provide an exception string, which contains a list of characters that do not become encoded. --[[User:Markhobley|Markhobley]] 17:57, 20 June 2011 (UTC)

== Encoding by RFC 3986 or HTML 5 ==

The current task lists six groups of characters to encode. The puzzle became, which groups of characters to preserve?

* The current task preserves only "0-9A-Za-z".
* My interpretation of RFC 3986 is to preserve "-._~0-9A-Za-z".
* My interpretation of HTML 5, [http://www.whatwg.org/specs/web-apps/current-work/multipage/association-of-controls-and-forms.html#url-encoded-form-data URL-encoded form data], is to preserve "-._*0-9A-Za-z" and to encode " " to "+".

I added this information to the task. If I understand well, RFC 3986 preserves '~' and encodes '*', while HTML 5 preserves '*' and encodes '~'. RFC 3986 also permits lowercase, so "http%3a%2f%2ffoo%20bar%2f" is valid. HTML 5 has specific rule to always encode to uppercase. --[[User:Kernigh|Kernigh]] 00:29, 31 July 2011 (UTC)

:I can think of several ways to approach this.  One would be to move the information to another page and link to it from here.  Another would be to change the task itself.  That said, personally, I do not see much use in "preserving characters".  There is a minor bulk advantage, but all encoded characters will pass through safely.  So the safest interpretation of multiple standards would be to encode any character suggested by any of them (and there are a variety of standards...).  --[[User:Rdm|Rdm]] 01:04, 1 August 2011 (UTC)
::I think we have mostly covered this now. The provision for an exception string allows for variations. For this task I don't really want a space to be encoded as a plus symbol, as this is not common and would also require a decoder with the reverse capability. A separate task is required for such a variation, if desired.

I think this task is ready for promotion. [[User:Markhobley|Markhobley]] 17:06, 13 August 2011 (UTC)
