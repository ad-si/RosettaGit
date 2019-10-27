+++
title = "URL parser/URI parser ALGOL68"
description = ""
date = 2016-03-25T18:06:38Z
aliases = []
[extra]
id = 20670
[taxonomies]
categories = []
tags = []
+++

==ALGOL 68 URI Parser==
This is a URI parser implemented in Algol 68. The text can be cut-and-paste into an Algol 68 program or saved in a file and included in another program by using the read pragma available in Algol 68, e.g.:

```algol68
PR read "uriParser.a68" PR
URI u := parse uri( "fred://harry@wombat.com" );
...
```



## ALGOL 68


```algol68
# URI parser #

# MODE returned by the URI parser #
MODE URI = STRUCT( STRING  scheme
                 , STRING  userinfo
                 , STRING  host
                 , STRING  port
                 , STRING  path         
                 , STRING  query 
                 , STRING  fragment id
                 , BOOL    ok     # TRUE if the URI parse was OK #
                 , STRING  error  # error message if the parse failed #
                 );

# returns the URI parsed from text                                    #
# ok OF the result will be TRUE  if the parse was successful          #
# ok OF the result will be FALSE if the parse failed                  #
#       and error OF the result will be a suitable error message      #
# the authority is split into the userinfo, host and port fields      #
# and not returned as a separate combined field                       #
PROC parse uri = ( STRING text )URI:
     BEGIN
         INT    pos         := 0;    # current character position     #
         INT    end pos     := 0;    # last character position        #

         STRING alphas       = "abcdefghijklmnopqrstuvwxyz"
                             + "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                             ;
         STRING digits       = "0123456789";
         STRING sub delims   = "!$&'()*+,;=";
         STRING unreserved   = alphas + digits + "-._~";
         STRING hex digits   = digits + "abcdefABCDEF";


         # sets the error message of the result and indicates the parse failed #
         PROC error = ( STRING message )VOID:
              BEGIN
                  ok    OF result := FALSE;
                  error OF result := message + " (near position " + whole( pos, 0 ) + ")"
              END # error # ;

         # returns TRUE if we have passed the end of text, FALSE otherwise #
         PROC at end = BOOL: pos > end pos;

         # returns the current character from the string #
         #      or REPR 0 if we have passed the end of the string #
         PROC curr char = CHAR: IF at end THEN REPR 0 ELSE text[ pos ] FI;

         # returns the character n positions after the current one or REPR 0 if there isn't one #
         PROC peek = ( INT n )CHAR: IF pos + n > end pos THEN REPR 0 ELSE text[ pos + n ] FI;

         # returns TRUE if the current character is ch, FALSE otherwise #
         PROC have = ( CHAR ch )BOOL: curr char = ch;

         # returns TRUE if the current character is one of the specified characters, FALSE otherwise #
         PROC have one of = ( STRING characters )BOOL: char in string( curr char, NIL, characters );

         # returns TRUE if ch is a letter (a-z, A-Z only), FALSE otherwise #
         PROC is letter = ( CHAR ch )BOOL: char in string( ch, NIL, alphas );

         # returns TRUE if ch is a hex digit, FALSE otherwise #
         PROC is hex    = ( CHAR ch )BOOL: char in string( ch, NIL, hex digits );

         # positions to the next character, if there is one #
         PROC next char = VOID: IF at end THEN pos := end pos + 1 ELSE pos +:= 1 FI;

         # returns and skips over the sequence of chatracters matching the specified characters #
         #         or hex encoded characters ( "%" followed by 2 hex digits )                   #
         PROC possibly encoded seq = ( STRING characters )STRING:
              BEGIN
                  STRING result := "";
                  BOOL   ok     := TRUE;
                  WHILE CHAR ch := curr char;
                        ok AND ( have one of( characters ) OR ch = "%" )
                  DO
                      IF ch = "%"
                      THEN
                          # should be "%" followed by a hex digit and a hex digit           #
                          IF NOT is hex( peek( 1 ) ) OR NOT is hex( peek( 2 ) )
                          THEN
                              # invalid encoded character                                   #
                              error( "Invalid encoded character" );
                              ok := FALSE
                          ELSE
                              # encoding looks OK #
                              result +:= curr char;
                              next char;
                              result +:= curr char;
                              next char;
                              result +:= curr char;
                              next char
                          FI
                      ELSE
                          # single character element                                        #
                          result +:= curr char;
                          next char
                      FI
                  OD;
                  result
              END # possibly encoded seq # ;

         # returns and skips over the sequence of the specified characters starting         #
         #         at the current position, if there is one                                 #
         PROC seq = ( STRING characters )STRING:
              BEGIN
                  STRING result := "";
                  WHILE have one of( characters )
                  DO
                      result +:= curr char;
                      next char
                  OD;
                  result
              END # seq # ;

         # returns and skips over the sequence of the specified characters starting         #
         #         at the current position                                                  #
         # if the sequence is empty, the specified error message is issued                  #
         PROC seq 1 = ( STRING characters, error message )STRING:
              BEGIN
                  STRING result := seq( characters );
                  IF result = ""
                  THEN
                      # empty sequence                                                      #
                      error( "Expected at least one of: """ + characters + """ for " + error message )
                  FI;
                  result
              END # seq 1 # ;

         # checks the current character is ch and advances over it if it is                 #
         # if the current character is not ch, an error is indicated                        #
         PROC must be = ( CHAR ch, STRING message )VOID: IF have( ch ) THEN next char ELSE error( message ) FI;

         # checks we have reached the end of the text and sets an error if we haven't       #
         PROC must be at end = ( STRING message )VOID: IF NOT at end THEN error( message ) FI;

         # returns and skips over an IPV6 address - the address format is not validated     #
         PROC ipv6 address = STRING: seq( hex digits + ":" ) + seq( digits + "." );

         # ------------                                                                     #
         # main parsing                                                                     #
         # 
### ======
                                                                     #

         URI result := ( "", "", "", "", "", "", "", TRUE, "" );

         # initialise parsing #
         pos     := LWB text;
         end pos := UPB text;

         # get the scheme                                                                   #
         IF ok OF result
         THEN
             scheme OF result := seq 1( alphas + digits + "+-.", "URI scheme" );
             IF ok OF result
             THEN
                 # the scheme must start with a letter                                      #
                 IF NOT is letter( ( scheme OF result )[ 1 ] )
                 THEN
                     # scheme didn't start with a-z, A-Z                                    #
                     error( "URI scheme must start with a letter (a-z, A-Z only)" )
                 ELSE
                     # ok so far, there should be a ":" next                                #
                     must be( ":", "after the URI scheme" )
                 FI
             FI
         FI;

         # get the path #
         IF ok OF result
         THEN
             # got the scheme OK, get the path #
             IF curr char = "/" AND peek( 1 ) = "/"
             THEN
                 # URI has an authority                                                     #
                 # there will optionally be userinfo followed by @                          #
                 # if there is no "@", the element will be the host                         #
                 next char;
                 next char;
                 # remember the start positioin of the element, incase we need to backtrack #
                 INT start pos := pos;
                 userinfo OF result := possibly encoded seq( unreserved + sub delims + ":" );
                 IF ok OF result
                 THEN
                     # got an element OK #
                     IF have( "@" )
                     THEN
                         # there was an "@", so the element we just parsed was the user info #
                         next char
                     ELSE
                         # didn't get any user info, backtrack to parse the text as the host #
                         userinfo OF result := "";
                         pos := start pos
                     FI
                 FI;
                 # we should now have the host optionally followed by ":" and the port       #
                 IF have( "[" )
                 THEN
                     # host is an IP literal #
                     next char;
                     host OF result := ipv6 address;
                     must be( "]", "following IPV6 address in URI host" )
                 ELSE
                     # host is a reg-name or IPV4 address #
                     # note an IPV4 address matches the reg-name pattern and we do not       #
                     # distinguish between them                                              #
                     host OF result := possibly encoded seq( unreserved + sub delims )
                 FI;
                 # can now have a port - ":" followed by digits                              #
                 IF have( ":" )
                 THEN
                     # have a port #
                     next char;
                     port OF result := seq( digits );
                     # the port can only be followed by "/", "?" or a hash character         #
                     # as the authority must be followed by a path-abempty                   #
                     # and that is followed by optional query and optional fragment id       #
                     IF NOT have one of( "/?#" ) AND NOT at end
                     THEN
                         # the port is invalid or followed by extraneous characters          #
                         error( "Invalid URI port" )
                     FI
                 FI
             FI;
             # get the path                                                                  #
             # we expect a possibly empty sequence of segments separated by "/"              #
             # a segment is a possibly empty sequence of                                     #
             #     unreserved, sub-delims, %xx characters, ":" or "@"                        #
             # the RFC categorises paths as:                                                 #
             #     path-abempty    - begins with "/" or is empty                             #
             #     path-absolute   - begins with "/" but not "//"                            #
             #     path-noscheme   - no leading  "/" and no ":" in the first segment         #
             #     path-rootless   - no leading  "/" can have ":" in the first segment       #
             #     path-empty      - empty path                                              #
             # we do not attempt to distinguish between them                                 #
             WHILE path OF result +:= possibly encoded seq( unreserved + sub delims + ":@" );
                   have( "/" ) AND ok OF result
             DO
                 path OF result +:= "/";
                 next char
             OD
         FI;

         # get the query                                                                     #
         IF have( "?" ) AND ok OF result
         THEN
             # have a query                                                                  #
             next char;
             query OF result := possibly encoded seq( unreserved + sub delims + ":@/?" )
         FI;

         # get the fragment id, if there is one                                              #
         IF have( "#" ) AND ok OF result
         THEN
             # have a fragment id                                                            #
             next char;
             fragment id OF result := possibly encoded seq( unreserved + sub delims + "/?" )
         FI;

         # should have reached the end of the text                                           #
         IF ok OF result
         THEN
             # haven't reached the end of the text                                           #
             must be at end( "unexpected text at the end of the URI: " + text[ pos : ] )
         FI;

         result
     END # parse uri # ;
```

