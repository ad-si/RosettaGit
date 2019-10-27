+++
title = "Parse an IP Address"
description = ""
date = 2019-07-22T19:55:21Z
aliases = []
[extra]
id = 10573
[taxonomies]
categories = []
tags = []
+++

{{task}}The purpose of this task is to demonstrate parsing of text-format IP addresses, using IPv4 and IPv6.

Taking the following as inputs:
::: {| border="5" cellspacing="0" cellpadding=2
|-
|127.0.0.1
|The "localhost" IPv4 address
|-
|127.0.0.1:80
|The "localhost" IPv4 address, with a specified port (80)
|-
|::1
|The "localhost" IPv6 address
|-
|<nowiki>[::1]:80</nowiki>
|The "localhost" IPv6 address, with a specified port (80)
|-
|2605:2700:0:3::4713:93e3
|Rosetta Code's primary server's public IPv6 address
|-
|<nowiki>[2605:2700:0:3::4713:93e3]:80</nowiki>
|Rosetta Code's primary server's public IPv6 address, with a specified port (80)
|}


Emit each described IP address as a hexadecimal integer representing the address, the address space, and the port number specified, if any.  In languages where variant result types are clumsy, the result should be ipv4 or ipv6 address number, something which says which address space was represented, port number and something that says if the port was specified.

For example 127.0.0.1 has the address number 7F000001 (2130706433 decimal) in the ipv4 address space.  ::ffff:127.0.0.1 represents the same address in the ipv6 address space where it has the address number FFFF7F000001 (281472812449793 decimal).  Meanwhile ::1 has address number 1 and serves the same purpose in the ipv6 address space that 127.0.0.1 serves in the ipv4 address space.


## C

{{libheader|C standard library}}

```C

#include <string.h>
#include <memory.h>


static unsigned int _parseDecimal ( const char** pchCursor )
{
	unsigned int nVal = 0;
	char chNow;
	while ( chNow = **pchCursor, chNow >= '0' && chNow <= '9' )
	{
		//shift digit in
		nVal *= 10;
		nVal += chNow - '0';

		++*pchCursor;
	}
	return nVal;
}



static unsigned int _parseHex ( const char** pchCursor )
{
	unsigned int nVal = 0;
	char chNow;
	while ( chNow = **pchCursor & 0x5f,	//(collapses case, but mutilates digits)
			(chNow >= ('0'&0x5f) && chNow <= ('9'&0x5f)) || 
			(chNow >= 'A' && chNow <= 'F') 
			)
	{
		unsigned char nybbleValue;
		chNow -= 0x10;	//scootch digital values down; hex now offset by x31
		nybbleValue = ( chNow > 9 ? chNow - (0x31-0x0a) : chNow );
		//shift nybble in
		nVal <<= 4;
		nVal += nybbleValue;

		++*pchCursor;
	}
	return nVal;
}



//Parse a textual IPv4 or IPv6 address, optionally with port, into a binary
//array (for the address, in network order), and an optionally provided port.
//Also, indicate which of those forms (4 or 6) was parsed.  Return true on
//success.  ppszText must be a nul-terminated ASCII string.  It will be
//updated to point to the character which terminated parsing (so you can carry
//on with other things.  abyAddr must be 16 bytes.  You can provide NULL for
//abyAddr, nPort, bIsIPv6, if you are not interested in any of those
//informations.  If we request port, but there is no port part, then nPort will
//be set to 0.  There may be no whitespace leading or internal (though this may
//be used to terminate a successful parse.
//Note:  the binary address and integer port are in network order.
int ParseIPv4OrIPv6 ( const char** ppszText, 
		unsigned char* abyAddr, int* pnPort, int* pbIsIPv6 )
{
	unsigned char* abyAddrLocal;
	unsigned char abyDummyAddr[16];

	//find first colon, dot, and open bracket
	const char* pchColon = strchr ( *ppszText, ':' );
	const char* pchDot = strchr ( *ppszText, '.' );
	const char* pchOpenBracket = strchr ( *ppszText, '[' );
	const char* pchCloseBracket = NULL;


	//we'll consider this to (probably) be IPv6 if we find an open
	//bracket, or an absence of dots, or if there is a colon, and it
	//precedes any dots that may or may not be there
	int bIsIPv6local = NULL != pchOpenBracket || NULL == pchDot ||
			( NULL != pchColon && ( NULL == pchDot || pchColon < pchDot ) );
	//OK, now do a little further sanity check our initial guess...
	if ( bIsIPv6local )
	{
		//if open bracket, then must have close bracket that follows somewhere
		pchCloseBracket = strchr ( *ppszText, ']' );
		if ( NULL != pchOpenBracket && ( NULL == pchCloseBracket ||
				pchCloseBracket < pchOpenBracket ) )
			return 0;
	}
	else	//probably ipv4
	{
		//dots must exist, and precede any colons
		if ( NULL == pchDot || ( NULL != pchColon && pchColon < pchDot ) )
			return 0;
	}

	//we figured out this much so far....
	if ( NULL != pbIsIPv6 )
		*pbIsIPv6 = bIsIPv6local;
	
	//especially for IPv6 (where we will be decompressing and validating)
	//we really need to have a working buffer even if the caller didn't
	//care about the results.
	abyAddrLocal = abyAddr;	//prefer to use the caller's
	if ( NULL == abyAddrLocal )	//but use a dummy if we must
		abyAddrLocal = abyDummyAddr;

	//OK, there should be no correctly formed strings which are miscategorized,
	//and now any format errors will be found out as we continue parsing
	//according to plan.
	if ( ! bIsIPv6local )	//try to parse as IPv4
	{
		//4 dotted quad decimal; optional port if there is a colon
		//since there are just 4, and because the last one can be terminated
		//differently, I'm just going to unroll any potential loop.
		unsigned char* pbyAddrCursor = abyAddrLocal;
		unsigned int nVal;
		const char* pszTextBefore = *ppszText;
		nVal =_parseDecimal ( ppszText );			//get first val
		if ( '.' != **ppszText || nVal > 255 || pszTextBefore == *ppszText )	//must be in range and followed by dot and nonempty
			return 0;
		*(pbyAddrCursor++) = (unsigned char) nVal;	//stick it in addr
		++(*ppszText);	//past the dot

		pszTextBefore = *ppszText;
		nVal =_parseDecimal ( ppszText );			//get second val
		if ( '.' != **ppszText || nVal > 255 || pszTextBefore == *ppszText )
			return 0;
		*(pbyAddrCursor++) = (unsigned char) nVal;
		++(*ppszText);	//past the dot

		pszTextBefore = *ppszText;
		nVal =_parseDecimal ( ppszText );			//get third val
		if ( '.' != **ppszText || nVal > 255 || pszTextBefore == *ppszText )
			return 0;
		*(pbyAddrCursor++) = (unsigned char) nVal;
		++(*ppszText);	//past the dot

		pszTextBefore = *ppszText;
		nVal =_parseDecimal ( ppszText );			//get fourth val
		if ( nVal > 255 || pszTextBefore == *ppszText )	//(we can terminate this one in several ways)
			return 0;
		*(pbyAddrCursor++) = (unsigned char) nVal;

		if ( ':' == **ppszText && NULL != pnPort )	//have port part, and we want it
		{
			unsigned short usPortNetwork;	//save value in network order
			++(*ppszText);	//past the colon
			pszTextBefore = *ppszText;
			nVal =_parseDecimal ( ppszText );
			if ( nVal > 65535 || pszTextBefore == *ppszText )
				return 0;
			((unsigned char*)&usPortNetwork)[0] = ( nVal & 0xff00 ) >> 8;
			((unsigned char*)&usPortNetwork)[1] = ( nVal & 0xff );
			*pnPort = usPortNetwork;
			return 1;
		}
		else	//finished just with ip address
		{
			if ( NULL != pnPort )
				*pnPort = 0;	//indicate we have no port part
			return 1;
		}
	}
	else	//try to parse as IPv6
	{
		unsigned char* pbyAddrCursor;
		unsigned char* pbyZerosLoc;
		int bIPv4Detected;
		int nIdx;
		//up to 8 16-bit hex quantities, separated by colons, with at most one
		//empty quantity, acting as a stretchy run of zeroes.  optional port
		//if there are brackets followed by colon and decimal port number.
		//A further form allows an ipv4 dotted quad instead of the last two
		//16-bit quantities, but only if in the ipv4 space ::ffff:x:x .
		if ( NULL != pchOpenBracket )	//start past the open bracket, if it exists
			*ppszText = pchOpenBracket + 1;
		pbyAddrCursor = abyAddrLocal;
		pbyZerosLoc = NULL;	//if we find a 'zero compression' location
		bIPv4Detected = 0;
		for ( nIdx = 0; nIdx < 8; ++nIdx )	//we've got up to 8 of these, so we will use a loop
		{
			const char* pszTextBefore = *ppszText;
			unsigned nVal =_parseHex ( ppszText );		//get value; these are hex
			if ( pszTextBefore == *ppszText )	//if empty, we are zero compressing; note the loc
			{
				if ( NULL != pbyZerosLoc )	//there can be only one!
				{
					//unless it's a terminal empty field, then this is OK, it just means we're done with the host part
					if ( pbyZerosLoc == pbyAddrCursor )
					{
						--nIdx;
						break;
					}
					return 0;	//otherwise, it's a format error
				}
				if ( ':' != **ppszText )	//empty field can only be via :
					return 0;
				if ( 0 == nIdx )	//leading zero compression requires an extra peek, and adjustment
				{
					++(*ppszText);
					if ( ':' != **ppszText )
						return 0;
				}

				pbyZerosLoc = pbyAddrCursor;
				++(*ppszText);
			}
			else
			{
				if ( '.' == **ppszText )	//special case of ipv4 convenience notation
				{
					//who knows how to parse ipv4?  we do!
					const char* pszTextlocal = pszTextBefore;	//back it up
					unsigned char abyAddrlocal[16];
					int bIsIPv6local;
					int bParseResultlocal = ParseIPv4OrIPv6 ( &pszTextlocal, abyAddrlocal, NULL, &bIsIPv6local );
					*ppszText = pszTextlocal;	//success or fail, remember the terminating char
					if ( ! bParseResultlocal || bIsIPv6local )	//must parse and must be ipv4
						return 0;
					//transfer addrlocal into the present location
					*(pbyAddrCursor++) = abyAddrlocal[0];
					*(pbyAddrCursor++) = abyAddrlocal[1];
					*(pbyAddrCursor++) = abyAddrlocal[2];
					*(pbyAddrCursor++) = abyAddrlocal[3];
					++nIdx;	//pretend like we took another short, since the ipv4 effectively is two shorts
					bIPv4Detected = 1;	//remember how we got here for further validation later
					break;	//totally done with address
				}

				if ( nVal > 65535 )	//must be 16 bit quantity
					return 0;
				*(pbyAddrCursor++) = nVal >> 8;		//transfer in network order
				*(pbyAddrCursor++) = nVal & 0xff;
				if ( ':' == **ppszText )	//typical case inside; carry on
				{
					++(*ppszText);
				}
				else	//some other terminating character; done with this parsing parts
				{
					break;
				}
			}
		}
		
		//handle any zero compression we found
		if ( NULL != pbyZerosLoc )
		{
			int nHead = (int)( pbyZerosLoc - abyAddrLocal );	//how much before zero compression
			int nTail = nIdx * 2 - (int)( pbyZerosLoc - abyAddrLocal );	//how much after zero compression
			int nZeros = 16 - nTail - nHead;		//how much zeros
			memmove ( &abyAddrLocal[16-nTail], pbyZerosLoc, nTail );	//scootch stuff down
			memset ( pbyZerosLoc, 0, nZeros );		//clear the compressed zeros
		}
		
		//validation of ipv4 subspace ::ffff:x.x
		if ( bIPv4Detected )
		{
			static const unsigned char abyPfx[] = { 0,0, 0,0, 0,0, 0,0, 0,0, 0xff,0xff };
			if ( 0 != memcmp ( abyAddrLocal, abyPfx, sizeof(abyPfx) ) )
				return 0;
		}

		//close bracket
		if ( NULL != pchOpenBracket )
		{
			if ( ']' != **ppszText )
				return 0;
			++(*ppszText);
		}

		if ( ':' == **ppszText && NULL != pnPort )	//have port part, and we want it
		{
			const char* pszTextBefore;
			unsigned int nVal;
			unsigned short usPortNetwork;	//save value in network order
			++(*ppszText);	//past the colon
			pszTextBefore = *ppszText;
			pszTextBefore = *ppszText;
			nVal =_parseDecimal ( ppszText );
			if ( nVal > 65535 || pszTextBefore == *ppszText )
				return 0;
			((unsigned char*)&usPortNetwork)[0] = ( nVal & 0xff00 ) >> 8;
			((unsigned char*)&usPortNetwork)[1] = ( nVal & 0xff );
			*pnPort = usPortNetwork;
			return 1;
		}
		else	//finished just with ip address
		{
			if ( NULL != pnPort )
				*pnPort = 0;	//indicate we have no port part
			return 1;
		}
	}

}


//simple version if we want don't care about knowing how much we ate
int ParseIPv4OrIPv6_2 ( const char* pszText, 
		unsigned char* abyAddr, int* pnPort, int* pbIsIPv6 )
{
	const char* pszTextLocal = pszText;
	return ParseIPv4OrIPv6 ( &pszTextLocal, abyAddr, pnPort, pbIsIPv6);
}

```

	
Test:

```C

#include <stdio.h>

... (above code for ParseIPv4OrIPv6 goes here) ...

unsigned short htons ( unsigned short us )
{
	return ( ((unsigned char*)&us)[0] << 8 ) + ((unsigned char*)&us)[1];
}

void dumpbin ( unsigned char* pbyBin, int nLen )
{
	int i;
	for ( i = 0; i < nLen; ++i )
	{
		printf ( "%02x", pbyBin[i] );
	}
}


void testcase ( const char* pszTest )
{
	unsigned char abyAddr[16];
	int bIsIPv6;
	int nPort;
	int bSuccess;

	printf ( "Test case '%s'\n", pszTest );
	const char* pszTextCursor = pszTest;
	bSuccess = ParseIPv4OrIPv6 ( &pszTextCursor, abyAddr, &nPort, &bIsIPv6 );
	if ( ! bSuccess )
	{
		printf ( "parse failed, at about index %d; rest: '%s'\n", pszTextCursor - pszTest, pszTextCursor );
		return;
	}
	
	printf ( "addr:  " );
	dumpbin ( abyAddr, bIsIPv6 ? 16 : 4 );
	printf ( "\n" );
	if ( 0 == nPort )
		printf ( "port absent" );
	else
		printf ( "port:  %d", htons ( nPort ) );
	printf ( "\n\n" );
	
}



int main ( int argc, char* argv[] )
{
	
	//The "localhost" IPv4 address
	testcase ( "127.0.0.1" );
	
	//The "localhost" IPv4 address, with a specified port (80)
	testcase ( "127.0.0.1:80" );
	//The "localhost" IPv6 address
	testcase ( "::1" );
	//The "localhost" IPv6 address, with a specified port (80)
	testcase ( "[::1]:80" );
	//Rosetta Code's primary server's public IPv6 address
	testcase ( "2605:2700:0:3::4713:93e3" );
	//Rosetta Code's primary server's public IPv6 address, with a specified port (80)
	testcase ( "[2605:2700:0:3::4713:93e3]:80" );
	
	//ipv4 space
	testcase ( "::ffff:192.168.173.22" );
	//ipv4 space with port
	testcase ( "[::ffff:192.168.173.22]:80" );
	//trailing compression
	testcase ( "1::" );
	//trailing compression with port
	testcase ( "[1::]:80" );
	//'any' address compression
	testcase ( "::" );
	//'any' address compression with port
	testcase ( "[::]:80" );
	
	return 0;
}

```


Output:

```txt

Test case '127.0.0.1'
addr:  7f000001
port absent

Test case '127.0.0.1:80'
addr:  7f000001
port:  80

Test case '::1'
addr:  00000000000000000000000000000001
port absent

Test case '[::1]:80'
addr:  00000000000000000000000000000001
port:  80

Test case '2605:2700:0:3::4713:93e3'
addr:  260527000000000300000000471393e3
port absent

Test case '[2605:2700:0:3::4713:93e3]:80'
addr:  260527000000000300000000471393e3
port:  80

Test case '::ffff:192.168.173.22'
addr:  00000000000000000000ffffc0a8ad16
port absent

Test case '[::ffff:192.168.173.22]:80'
addr:  00000000000000000000ffffc0a8ad16
port:  80

Test case '1::'
addr:  00010000000000000000000000000000
port absent

Test case '[1::]:80'
addr:  00010000000000000000000000000000
port:  80

Test case '::'
addr:  00000000000000000000000000000000
port absent

Test case '[::]:80'
addr:  00000000000000000000000000000000
port:  80

```



## Go


```go
package main

import (
	"errors"
	"fmt"
	"net"
	"strconv"
	"strings"
)

func ParseIPPort(s string) (ip net.IP, port, space string, err error) {
	ip = net.ParseIP(s)
	if ip == nil {
		var host string
		host, port, err = net.SplitHostPort(s)
		if err != nil {
			return
		}
		if port != "" {
			// This check only makes sense if service names are not allowed
			if _, err = strconv.ParseUint(port, 10, 16); err != nil {
				return
			}
		}
		ip = net.ParseIP(host)
	}
	if ip == nil {
		err = errors.New("invalid address format")
	} else {
		space = "IPv6"
		if ip4 := ip.To4(); ip4 != nil {
			space = "IPv4"
			ip = ip4
		}
	}
	return
}

func main() {
	var testCases = []string{
		"127.0.0.1",
		"127.0.0.1:80",
		"::1",
		"[::1]:80",
		"2605:2700:0:3::4713:93e3",
		"[2605:2700:0:3::4713:93e3]:80",
	}
	max := len("Input")
	for _, addr := range testCases {
		if len(addr) > max {
			max = len(addr)
		}
	}
	fmt.Printf("%-*s  %*s  %-6s %s\n", max, "Input",
		2*net.IPv6len, "Address", "Space", "Port")
	for _, addr := range testCases {
		fmt.Printf("%-*s  ", max, addr)
		ip, port, space, err := ParseIPPort(addr)
		if err != nil {
			fmt.Println(err)
			continue
		}
		fmt.Print(strings.Repeat("  ", net.IPv6len-len(ip)))
		for _, b := range ip {
			fmt.Printf("%02x", b)
		}
		fmt.Printf("  %-6s %s\n", space, port)
	}
}
```

{{out}}

```txt

Input                                                   Address  Space  Port
127.0.0.1                                              7f000001  IPv4   
127.0.0.1:80                                           7f000001  IPv4   80
::1                            00000000000000000000000000000001  IPv6   
[::1]:80                       00000000000000000000000000000001  IPv6   80
2605:2700:0:3::4713:93e3       260527000000000300000000471393e3  IPv6   
[2605:2700:0:3::4713:93e3]:80  260527000000000300000000471393e3  IPv6   80

```



## Haskell


```Haskell
import Data.List (isInfixOf)
import Numeric (showHex)
import Data.Char (isDigit)

data IPChunk = IPv6Chunk String | IPv4Chunk (String, String) |
    IPv6WithPort [IPChunk] String | IPv6NoPort [IPChunk] |
    IPv4WithPort IPChunk String | IPv4NoPort IPChunk |
    IPInvalid | IPZeroSection | IPUndefinedWithPort String |
    IPUndefinedNoPort

instance Show IPChunk where
    show (IPv6Chunk a) = a
    show (IPv4Chunk (a,b)) = a ++ b
    show (IPv6WithPort a p) = "IPv6 " ++ concatMap show a ++ " port " ++ p
    show (IPv6NoPort a) = "IPv6 " ++ concatMap show a ++ " no port"
    show (IPv4WithPort a p) = "IPv4 " ++ show a ++ " port " ++ p
    show (IPv4NoPort a) = "IPv4 " ++ show a
    show IPInvalid = "Invalid IP address"
    
isIPInvalid IPInvalid = True
isIPInvalid _ = False

isIPZeroSection IPZeroSection = True
isIPZeroSection _ = False

splitOn _ [] = []
splitOn x xs = let (a, b) = break (== x) xs in a : splitOn x (drop 1 b)

count x = length . filter (== x)

between a b x = x >= a && x <= b

none f = all (not . f)

parse1 [] = IPInvalid
parse1 "::" = IPUndefinedNoPort
parse1 ('[':':':':':']':':':ps) = if portIsValid ps then IPUndefinedWithPort ps else IPInvalid
parse1 ('[':xs) = if "]:" `isInfixOf` xs
    then let (a, b) = break (== ']') xs in
            if tail b == ":" then IPInvalid else IPv6WithPort (map chunk (splitOn ':' a)) (drop 2 b)
    else IPInvalid
parse1 xs
    | count ':' xs <= 1 && count '.' xs == 3 =
        let (a, b) = break (== ':') xs in case b of
                "" -> IPv4NoPort (chunk a)
                (':':ps) -> IPv4WithPort (chunk a) ps
                _ -> IPInvalid
    | count ':' xs > 1 && count '.' xs <= 3 =
        IPv6NoPort (map chunk (splitOn ':' xs))
            
chunk [] = IPZeroSection
chunk xs
    | '.' `elem` xs = case splitOn '.' xs of
        [a,b,c,d] -> let [e,f,g,h] = map read [a,b,c,d]
                     in if all (between 0 255) [e,f,g,h]
                            then let [i,j,k,l] = map (\n -> fill 2 $ showHex n "") [e,f,g,h]
                                 in IPv4Chunk (i ++ j, k ++ l)
                            else IPInvalid
    | ':' `notElem` xs && between 1 4 (length xs) && all (`elem` "0123456789abcdef") xs = IPv6Chunk (fill 4 xs)
    | otherwise = IPInvalid 

fill n xs = replicate (n - length xs) '0' ++ xs

parse2 IPInvalid = IPInvalid
parse2 (IPUndefinedWithPort p) = IPv6WithPort (replicate 8 zeroChunk) p
parse2 IPUndefinedNoPort = IPv6NoPort (replicate 8 zeroChunk)
parse2 a = case a of
    IPv6WithPort xs p -> if none isIPInvalid xs && portIsValid p
        then let ys = complete xs
             in  if countChunks ys == 8
                     then IPv6WithPort ys p
                     else IPInvalid
        else IPInvalid
    IPv6NoPort xs -> if none isIPInvalid xs
        then let ys = complete xs
             in  if countChunks ys == 8
                     then IPv6NoPort ys
                     else IPInvalid
        else IPInvalid
    IPv4WithPort (IPv4Chunk a) p -> if portIsValid p
        then IPv4WithPort (IPv4Chunk a) p
        else IPInvalid
    IPv4NoPort (IPv4Chunk a) -> IPv4NoPort (IPv4Chunk a)
    _ -> IPInvalid

zeroChunk = IPv6Chunk "0000"

portIsValid a = all isDigit a && between 0 65535 (read a)

complete xs = case break isIPZeroSection xs of
    (_, [IPZeroSection]) -> []
    (ys, []) -> ys
    ([], (IPZeroSection:IPZeroSection:ys)) -> if any isIPZeroSection ys || countChunks ys > 7
        then []
        else replicate (8 - countChunks ys) zeroChunk ++ ys
    (ys, (IPZeroSection:zs)) -> if any isIPZeroSection zs || countChunks ys + countChunks zs > 7
        then []
        else ys ++ replicate (8 - countChunks ys - countChunks zs) zeroChunk ++ zs
    _ -> []

countChunks xs = foldl f 0 xs
    where f n (IPv4Chunk _) = n + 2
          f n (IPv6Chunk _) = n + 1
    
ip = parse2 . parse1

main = mapM_ (putStrLn . show . ip)
    ["127.0.0.1",                                  -- loop back
     "127.0.0.1:80",                               -- loop back +port
     "::1",                                        -- loop back
     "[::1]:80",                                   -- loop back +port
     "2605:2700:0:3::4713:93e3",                   -- Rosetta Code
     "[2605:2700:0:3::4713:93e3]:80"]              -- Rosetta Code

```

Output:
```txt

IPv4 7f000001
IPv4 7f000001 port 80
IPv6 00000000000000000000000000000001 no port
IPv6 00000000000000000000000000000001 port 80
IPv6 260527000000000300000000471393e3 no port
IPv6 260527000000000300000000471393e3 port 80

```


=={{header|Icon}} and {{header|Unicon}}==

```Icon
link printf, hexcvt

procedure main()
   L := ["192.168.0.1",                                # private
         "127.0.0.1",                                  # loop back
         "127.0.0.1:80",                               # loop back +port
         "2001:db8:85a3:0:0:8a2e:370:7334",            # doc, IPv6 for 555-1234
         "2001:db8:85a3::8a2e:370:7334",               # doc
         "::1",                                        # loop back
         "[::1]:80",                                   # loop back +port
         "::",                                         # unspecified
         "::ffff:192.168.0.1",                         # transition
         "2605:2700:0:3::4713:93e3",                   # RC 	
         "[2605:2700:0:3::4713:93e3]:80",              # RC 
         "::ffff:71.19.147.227",                       # RC transition
         "[::ffff:71.19.147.227]:80",                  # RC transition  +port        
         "[2001:db8:85a3:8d3:1319:8a2e:370:7348]:443", # doc +port
         "256.0.0.0",                                  # invalid
         "g::1"]                                       # invalid
                 
   every x := !L do {
      if x ?  (ip := ipmatch(), port := portmatch(), pos(0)) then {
         if i := IPv4decode(ip) then 
            printf("%s is the IPv4 address = x'%s'",x,i)
         else if i := IPv6decode(ip) then 
               printf("%s is the IPv6 address = x'%s'",x,i)
         else {
            printf("%s is not a valid IP address\n",x)
            next
            }
         if \port then printf(" port=%s\n",port) else printf("\n")
         }
      else printf("%s is not an IP address\n",x)
      }
end


procedure ipmatch()                                #: match an ip v4/v6 address
static c4,c6
initial {
   c4 := &digits ++ '.'
   c6 := &digits ++ 'abcdef:'
   }
   suspend (="[" || ( (="::ffff:" || tab(many(c4))) | tab(many(c6)) ) || ="]") |
           ( ="::ffff:" || tab(many(c4))) |  tab(many(c6|c4))
end

procedure portmatch()                              #: match a port number
   return (=":",0 < (65536 > tab(many(&digits)))) | &null
end

procedure IPv4decode(s)                            #: match IPv4 to hex string 
   s ? ( ip  := (0 <= (256 > tab(many(&digits)))), ip *:= 256, =".", 
         ip +:= (0 <= (256 > tab(many(&digits)))), ip *:= 256, =".",
         ip +:= (0 <= (256 > tab(many(&digits)))), ip *:= 256, =".",
         ip +:= (0 <= (256 > tab(many(&digits)))),
         return right(hexstring(ip,,&lcase),8) )            
end

procedure IPv6decode(s)                            #: IPv6 to hex string
   s ?:=  2(="[", tab(-1), ="]")                         # remove any [] 
   if find(".",s) then                                   # transitional
      s ? ( tab(many(':0')), ="ffff:", 
            return right("ffff" || IPv4decode(tab(0)),32,"0") )
   else { 
      h := t := ""
      s ? {
         while x := tab(find(":")) do {                  # head
            if *x <= 4 then h ||:= right(x,4,"0")
            if ="::" then break
            else move(1)
            }
         while x := tab(find(":")|0) do {                # tail 
            if *x <= 4 then t ||:= right(x,4,"0")          
            move(1) | break
            }
         if x := h || repl("0",32-(*h+*t)) || t then     # and insides
            return x
         }
      }
end
```


{{libheader|Icon Programming Library}}  
[http://www.cs.arizona.edu/icon/library/src/procs/printf.icn printf.icn provides the printf family] 
[http://www.cs.arizona.edu/icon/library/src/procs/hexcvt.icn hexcvt.icn provides hex and hexstring] 

Output:
```txt
192.168.0.1 is the IPv4 address = x'c0a80001'
127.0.0.1 is the IPv4 address = x'7f000001'
127.0.0.1:80 is the IPv4 address = x'7f000001' port=80
2001:db8:85a3:0:0:8a2e:370:7334 is the IPv6 address = x'20010db885a3000000008a2e03707334'
2001:db8:85a3::8a2e:370:7334 is the IPv6 address = x'20010db885a3000000008a2e03707334'
::1 is the IPv6 address = x'00000000000000000000000000000001'
[::1]:80 is the IPv6 address = x'00000000000000000000000000000001' port=80
:: is the IPv6 address = x'00000000000000000000000000000000'
::ffff:192.168.0.1 is the IPv6 address = x'00000000000000000000ffffc0a80001'
2605:2700:0:3::4713:93e3 is the IPv6 address = x'260527000000000300000000471393e3'
[2605:2700:0:3::4713:93e3]:80 is the IPv6 address = x'260527000000000300000000471393e3' port=80
::ffff:71.19.147.227 is the IPv6 address = x'00000000000000000000ffff471393e3'
[::ffff:71.19.147.227]:80 is the IPv6 address = x'00000000000000000000ffff471393e3' port=80
[2001:db8:85a3:8d3:1319:8a2e:370:7348]:443 is the IPv6 address = x'20010db885a308d313198a2e03707348' port=443
256.0.0.0 is not a valid IP address
g::1 is not an IP address
```



## J


Implementation:


```J
parseaddr=:3 :0
  if. '.' e. y do.
    if. +./'::' E. y do.
      parsehybrid y
    else.
      parseipv4 y
    end.
  else.
    parseipv6 y
  end.
)

parseipv4=:3 :0
  'addr port'=. 2{.<;._2 y,'::'
  4,((4#256)#._&".;._1'.',addr),_".port
)

parseipv6=:3 :0
  'addr port'=. 2{.<;._2 (y-.'['),']]'
  split=. I. '::' E. addr
  a1=. 8{. dfh;._2 (split {. addr),8#':'
  a2=._8{. dfh;._1 (8#':'),split }. addr
  6,(65536x#.a1+a2),_".port-.':'
)

parsehybrid=:3 :0
  'kludge port'=. 2{.<;._2 (tolower y-.'['),']]'
  addr=. _1 {:: <;._2 kludge,':'
  assert. (kludge-:'::ffff:',addr) +. kludge-: '::',addr
  6,(16bffff00000000+1{parseipv4 addr),_".port-.':'
)

fmt=:3 :0
  port=. ''
  ((#y){.'v';'addr';'port')=. y
  'ipv',(":v),' ',(hfd addr),(#port)#' ',":port
)
```


Task examples:


```J
   fmt parseaddr '127.0.0.1'
ipv4 7f000001
   fmt parseaddr '127.0.0.1:80'
ipv4 7f000001 80
   fmt parseaddr '::1'
ipv6 1
   fmt parseaddr '[::1]:80'
ipv6 1 80
   fmt parseaddr '2605:2700:0:3::4713:93e3'
ipv6 260527000000000300000000471393e3
   fmt parseaddr '[2605:2700:0:3::4713:93e3]:80'
ipv6 260527000000000300000000471393e3 80
```




## Java

{{incorrect|Java|Address of 0001F019 output for '127.0.0.1:80'}}
{{works with|Java|7}}
Implementation:


```Java
import java.net.Inet6Address;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.regex.Pattern;

import javax.xml.bind.DatatypeConverter;

/**
 * Parses ipv4 and ipv6 addresses. Emits each described IP address as a
 * hexadecimal integer representing the address, the address space, and the port
 * number specified, if any.
 */
public class IPParser {
	/*
	 * Using regex to ensure that the address is a valid one. This allows for
	 * separating by format and ensures that the operations done on a format
	 * will be valid.
	 */
	// 0.0.0.0-255.255.255.255
	private final String ipv4segment =
			"(25[0-5]|(2[0-4]|1{0,1}[0-9]){0,1}[0-9])";

	// 0-65535
	private final String portsegment =
			":(?:6553[0-5]|655[0-2][0-9]|65[0-4][0-9]{2}|"
					+ "6[0-4][0-9]{3}|[1-5][0-9]{4}|[1-9][0-9]{1,3}|[0-9])";
	private final String ipv4address = "(" + ipv4segment + "\\.){3,3}"
			+ ipv4segment;
	private final String ipv4addressWithPort = ipv4address + portsegment + "?";
	private final String ipv6segment = "[a-fA-F0-9]{1,4}";
	private final String ipv6address = "(" +
	// 1:2:3:4:5:6:7:8
			"(" + ipv6segment + ":){7,7}" + ipv6segment + "|" +
	// 1::, 1:2:3:4:5:6:7::
			"(" + ipv6segment + ":){1,7}:|" +
			// 1::8, 1:2:3:4:5:6::8, 1:2:3:4:5:6::8
			"(" + ipv6segment + ":){1,6}:" + ipv6segment + "|" +
			// 1::7:8, 1:2:3:4:5::7:8, 1:2:3:4:5::8
			"(" + ipv6segment + ":){1,5}(:" + ipv6segment + "){1,2}|" +
			// 1::6:7:8, 1:2:3:4::6:7:8, 1:2:3:4::8
			"(" + ipv6segment + ":){1,4}(:" + ipv6segment + "){1,3}|" +
			// 1::5:6:7:8, 1:2:3::5:6:7:8, 1:2:3::8
			"(" + ipv6segment + ":){1,3}(:" + ipv6segment + "){1,4}|" +
			// # 1::4:5:6:7:8, 1:2::4:5:6:7:8, 1:2::8
			"(" + ipv6segment + ":){1,2}(:" + ipv6segment + "){1,5}|" +
			// # 1::3:4:5:6:7:8, 1::3:4:5:6:7:8, 1::8
			ipv6segment + ":((:" + ipv6segment + "){1,6})|" +
			// ::2:3:4:5:6:7:8, ::2:3:4:5:6:7:8, ::8, ::
			":((:" + ipv6segment + "){1,7}|:)|" +
			// fe80::7:8%eth0, fe80::7:8%1 (link-local IPv6 addresses with
			// zone index)
			"fe80:(:" + ipv6segment + "){0,4}%[0-9a-zA-Z]{1,}|" +
			// ::255.255.255.255, ::ffff:255.255.255.255,
			// ::ffff:0:255.255.255.255 (IPv4-mapped IPv6 addresses and
			// IPv4-translated addresses)
			"::(ffff(:0{1,4}){0,1}:){0,1}" + ipv4address + "|" +
			// 2001:db8:3:4::192.0.2.33, 64:ff9b::192.0.2.33 (IPv4-Embedded
			// IPv6 Address)
			"(" + ipv6segment + ":){1,4}:" + ipv4address + ")";

	private final String ipv6addressWithPort = "\\[" + ipv6address + "\\]"
			+ portsegment + "?";

	/**
	 * Parses ipv4 and ipv6 addresses. Emits each described IP address as a
	 * hexadecimal integer representing the address, the address space, and the
	 * port number specified, if any.
	 * 
	 * @param address the address to analyze
	 */
	public void parse(String address) {

		// Used for storing values to be printed
		String space = "";// ipv4, ipv6, or unknown
		String hex = "";// hex value of the address
		String port = "";// the port or unknown

		// Try to match the pattern with one of the 2 types, with or without a
		// port
		if (Pattern.matches("^" + ipv4address + "$", address)) {
			InetAddress a;
			space = "IPv4";
			try {
				a = InetAddress.getByName(address);
				hex = DatatypeConverter.printHexBinary(a.getAddress());
			}
			catch (UnknownHostException e) {
				e.printStackTrace();
				hex = "Invalid";
			}
			port = "Absent";
		}
		else if (Pattern.matches("^" + ipv4addressWithPort + "$", address)) {
			String[] parts = address.split("\\.");
			port = parts[3].split(":")[1];
			parts[3] = parts[3].split(":")[0];
			InetAddress a;
			space = "IPv4";
			try {
				address = parts[0] + parts[1] + parts[2] + parts[3];
				a = InetAddress.getByName(address);
				hex = DatatypeConverter.printHexBinary(a.getAddress());
			}
			catch (UnknownHostException e) {
				e.printStackTrace();
				hex = "Invalid";
			}
		}
		else if (Pattern.matches("^" + ipv6address + "$", address)) {
			InetAddress a;
			space = "IPv6";
			try {
				a = Inet6Address.getByName(address);
				hex = DatatypeConverter.printHexBinary(a.getAddress());
			}
			catch (UnknownHostException e) {
				e.printStackTrace();
				hex = "Invalid";
			}
			port = "Absent";
		}
		else if (Pattern.matches("^" + ipv6addressWithPort + "$", address)) {
			String[] parts = address.split(":");
			InetAddress a;
			space = "IPv6";
			address =
					address.replace("[", "").replace("]", "")
							.replaceAll(portsegment + "$", "");
			try {
				a = Inet6Address.getByName(address);
				hex = DatatypeConverter.printHexBinary(a.getAddress());
			}
			catch (UnknownHostException e) {
				e.printStackTrace();
				hex = "Invalid";
			}
			port = parts[parts.length - 1];
		}
		else {
			// Not a valid address
			hex = "Invalid";
			space = "Invalid";
			port = "Invalid";
		}

		// Output the findings to the console
		System.out.println("Test case: '" + address + "'");
		System.out.println("Space:      " + space);
		System.out.println("Address:    " + hex);
		System.out.println("Port:       " + port);
		System.out.println();

	}

	/**
	 * Tests the parser using various addresses.
	 * 
	 * @param args arguments for the program
	 */
	public static void main(String[] args) {
		IPParser parser = new IPParser();

		// The "localhost" IPv4 address
		parser.parse("127.0.0.1");
		// The "localhost" IPv4 address, with a specified port (80)
		parser.parse("127.0.0.1:80");
		// The "localhost" IPv6 address
		parser.parse("::1");
		// The "localhost" IPv6 address, with a specified port (80)
		parser.parse("[::1]:80");
		// Rosetta Code's primary server's public IPv6 address
		parser.parse("2605:2700:0:3::4713:93e3");
		// Rosetta Code's primary server's public IPv6 address, with a specified
		// port (80)
		parser.parse("[2605:2700:0:3::4713:93e3]:80");

		// ipv6 space
		parser.parse("::ffff:192.168.173.22");
		// ipv6 space with port
		parser.parse("[::ffff:192.168.173.22]:80");
		// trailing compression
		parser.parse("1::");
		// trailing compression with port
		parser.parse("[1::]:80");
		// 'any' address compression
		parser.parse("::");
		// 'any' address compression with port
		parser.parse("[::]:80");
	}
}

```

output<lang>
Test case: '127.0.0.1'
Space:      IPv4
Address:    7F000001
Port:       Absent

Test case: '127.0.0.1:80'
Space:      IPv4
Address:    0001F019
Port:       80

Test case: '::1'
Space:      IPv6
Address:    00000000000000000000000000000001
Port:       Absent

Test case: '[::1]:80'
Space:      IPv6
Address:    00000000000000000000000000000001
Port:       80

Test case: '2605:2700:0:3::4713:93e3'
Space:      IPv6
Address:    260527000000000300000000471393E3
Port:       Absent

Test case: '[2605:2700:0:3::4713:93e3]:80'
Space:      IPv6
Address:    260527000000000300000000471393E3
Port:       80

Test case: '::ffff:192.168.173.22'
Space:      IPv6
Address:    C0A8AD16
Port:       Absent

Test case: '[::ffff:192.168.173.22]:80'
Space:      IPv6
Address:    C0A8AD16
Port:       80

Test case: '1::'
Space:      IPv6
Address:    00010000000000000000000000000000
Port:       Absent

Test case: '[1::]:80'
Space:      IPv6
Address:    00010000000000000000000000000000
Port:       80

Test case: '::'
Space:      IPv6
Address:    00000000000000000000000000000000
Port:       Absent

Test case: '[::]:80'
Space:      IPv6
Address:    00000000000000000000000000000000
Port:       80

```



## Julia


```julia

const testdata = ["127.0.0.1", "127.0.0.1:80", "::1", "[::1]:80",
                  "2605:2700:0:3::4713:93e3", "[2605:2700:0:3::4713:93e3]:80",
                  "::ffff:192.168.173.22", "[::ffff:192.168.173.22]:80",
                  "1::", "[1::]:80", "::", "[::]:80"]

maybev4(ip) = search(ip, '.') > 0 && length(matchall(r":", ip)) < 2
maybev6(ip) = length(matchall(r":", ip)) > 1

function parseip(ip)
    if (mat = match(r"^\[([:.\da-fA-F]+)\]:(\d+)$", ip))!= nothing ||
       (mat = match(r"^([\d.]+)[:/](\d+)$", ip)) != nothing
        port = mat.captures[2]
        ip = mat.captures[1]
    else
        port = "none"
    end
    if maybev4(ip)
        println("Processing ip v4 $ip")
        iphex = hex(Int(Base.IPv4(ip)))
        addresspace = "IPv4"
    elseif maybev6(ip)
        println("Processing ip v6 $ip")
        iphex = hex(UInt128(Base.IPv6(ip)))
        addresspace = "IPv6"
    else
        throw("Bad IP address argument $ip")
    end
    iphex, addresspace, port
end

for ip in testdata
    hx, add, por = parseip(ip)
    println("For input $ip, IP in hex is $hx, address space $add, port $por.")
end

```

{{output}}

```txt

Processing ip v4 127.0.0.1
For input 127.0.0.1, IP in hex is 7f000001, address space IPv4, port none.
Processing ip v4 127.0.0.1
For input 127.0.0.1:80, IP in hex is 7f000001, address space IPv4, port 80.
Processing ip v6 ::1
For input ::1, IP in hex is 1, address space IPv6, port none.
Processing ip v6 ::1
For input [::1]:80, IP in hex is 1, address space IPv6, port 80.
Processing ip v6 2605:2700:0:3::4713:93e3
For input 2605:2700:0:3::4713:93e3, IP in hex is 260527000000000300000000471393e3, address space IPv6, port none.
Processing ip v6 2605:2700:0:3::4713:93e3
For input [2605:2700:0:3::4713:93e3]:80, IP in hex is 260527000000000300000000471393e3, address space IPv6, port 80.
Processing ip v6 ::ffff:192.168.173.22
For input ::ffff:192.168.173.22, IP in hex is ffffc0a8ad16, address space IPv6, port none.
Processing ip v6 ::ffff:192.168.173.22
For input [::ffff:192.168.173.22]:80, IP in hex is ffffc0a8ad16, address space IPv6, port 80.
Processing ip v6 1::
For input 1::, IP in hex is 10000000000000000000000000000, address space IPv6, port none.
Processing ip v6 1::
For input [1::]:80, IP in hex is 10000000000000000000000000000, address space IPv6, port 80.
Processing ip v6 ::
For input ::, IP in hex is 0, address space IPv6, port none.
Processing ip v6 ::
For input [::]:80, IP in hex is 0, address space IPv6, port 80.

```



## Kotlin


```scala
// version 1.1.3

import java.math.BigInteger

enum class AddressSpace { IPv4, IPv6, Invalid }

data class IPAddressComponents(
    val address: BigInteger,
    val addressSpace: AddressSpace,
    val port: Int  // -1 denotes 'not specified'
)

val INVALID = IPAddressComponents(BigInteger.ZERO, AddressSpace.Invalid, 0)
  
fun ipAddressParse(ipAddress: String): IPAddressComponents {
    var addressSpace = AddressSpace.IPv4
    var ipa = ipAddress.toLowerCase()
    var port = -1
    var trans = false
    
    if (ipa.startsWith("::ffff:") && '.' in ipa) {
        addressSpace = AddressSpace.IPv6
        trans = true
        ipa = ipa.drop(7)
    }
    else if (ipa.startsWith("[::ffff:") && '.' in ipa) {
        addressSpace = AddressSpace.IPv6
        trans = true
        ipa = ipa.drop(8).replace("]", "")
    } 
    val octets = ipa.split('.').reversed().toTypedArray()
    var address = BigInteger.ZERO
    if (octets.size == 4) {
        val split = octets[0].split(':')
        if (split.size == 2) {
            val temp = split[1].toIntOrNull()
            if (temp == null || temp !in 0..65535) return INVALID                
            port = temp
            octets[0] = split[0]
        }
       
        for (i in 0..3) {
            val num = octets[i].toLongOrNull()
            if (num == null || num !in 0..255) return INVALID
            val bigNum = BigInteger.valueOf(num)
            address = address.or(bigNum.shiftLeft(i * 8))
        }

        if (trans) address += BigInteger("ffff00000000", 16)
    }
    else if (octets.size == 1) {
        addressSpace = AddressSpace.IPv6
        if (ipa[0] == '[') {
            ipa = ipa.drop(1)
            val split = ipa.split("]:")
            if (split.size != 2) return INVALID
            val temp = split[1].toIntOrNull()
            if (temp == null || temp !in 0..65535) return INVALID
            port = temp
            ipa = ipa.dropLast(2 + split[1].length)
        }
        val hextets = ipa.split(':').reversed().toMutableList()
        val len = hextets.size

        if (ipa.startsWith("::")) 
            hextets[len - 1] = "0"
        else if (ipa.endsWith("::")) 
            hextets[0] = "0"

        if (ipa == "::") hextets[1] = "0"        
        if (len > 8 || (len == 8 && hextets.any { it == "" }) || hextets.count { it == "" } > 1)
            return INVALID
        if (len < 8) {
            var insertions = 8 - len            
            for (i in 0..7) {
                if (hextets[i] == "") {
                    hextets[i] = "0"
                    while (insertions-- > 0) hextets.add(i, "0") 
                    break 
                }
            } 
        }
        for (j in 0..7) {
            val num = hextets[j].toLongOrNull(16)
            if (num == null || num !in 0x0..0xFFFF) return INVALID
            val bigNum = BigInteger.valueOf(num)
            address = address.or(bigNum.shiftLeft(j * 16))
        }   
    }
    else return INVALID

    return IPAddressComponents(address, addressSpace, port)
}

fun main(args: Array<String>) {
    val ipas = listOf(
        "127.0.0.1",
		"127.0.0.1:80",
		"::1",
		"[::1]:80",
		"2605:2700:0:3::4713:93e3",
		"[2605:2700:0:3::4713:93e3]:80",
        "::ffff:192.168.173.22",
        "[::ffff:192.168.173.22]:80",
        "1::",
        "::",
        "256.0.0.0",
        "::ffff:127.0.0.0.1"
    )
    for (ipa in ipas) {
        val (address, addressSpace, port) = ipAddressParse(ipa)
        println("IP address    : $ipa")
        println("Address       : ${"%X".format(address)}") 
        println("Address Space : $addressSpace")
        println("Port          : ${if (port == -1) "not specified" else port.toString()}")
        println()
    } 
}
```


{{out}}

```txt

IP address    : 127.0.0.1
Address       : 7F000001
Address Space : IPv4
Port          : not specified

IP address    : 127.0.0.1:80
Address       : 7F000001
Address Space : IPv4
Port          : 80

IP address    : ::1
Address       : 1
Address Space : IPv6
Port          : not specified

IP address    : [::1]:80
Address       : 1
Address Space : IPv6
Port          : 80

IP address    : 2605:2700:0:3::4713:93e3
Address       : 260527000000000300000000471393E3
Address Space : IPv6
Port          : not specified

IP address    : [2605:2700:0:3::4713:93e3]:80
Address       : 260527000000000300000000471393E3
Address Space : IPv6
Port          : 80

IP address    : ::ffff:192.168.173.22
Address       : FFFFC0A8AD16
Address Space : IPv6
Port          : not specified

IP address    : [::ffff:192.168.173.22]:80
Address       : FFFFC0A8AD16
Address Space : IPv6
Port          : 80

IP address    : 1::
Address       : 10000000000000000000000000000
Address Space : IPv6
Port          : not specified

IP address    : ::
Address       : 0
Address Space : IPv6
Port          : not specified

IP address    : 256.0.0.0
Address       : 0
Address Space : Invalid
Port          : 0

IP address    : ::ffff:127.0.0.0.1
Address       : 0
Address Space : Invalid
Port          : 0

```



## Perl


```perl
sub parse_v4 {
	my ($ip, $port) = @_;
	my @quad = split(/\./, $ip);

	return unless @quad == 4;
	for (@quad) { return if ($_ > 255) }

	if (!length $port) { $port = -1 }
	elsif ($port =~ /^(\d+)$/) { $port = $1 }
	else { return }

	my $h = join '' => map(sprintf("%02x", $_), @quad);
	return $h, $port
}

sub parse_v6 {
	my $ip = shift;
	my $omits;

	return unless $ip =~ /^[\da-f:.]+$/i; # invalid char

	$ip =~ s/^:/0:/;
	$omits = 1 if $ip =~ s/::/:z:/g;
	return if $ip =~ /z.*z/;	# multiple omits illegal

	my $v4 = '';
	my $len = 8;

	if ($ip =~ s/:((?:\d+\.){3}\d+)$//) {
		# hybrid 4/6 ip
		($v4) = parse_v4($1)	or return;
		$len -= 2;

	}
	# what's left should be v6 only
	return unless $ip =~ /^[:a-fz\d]+$/i;

	my @h = split(/:/, $ip);
	return if @h + $omits > $len;	# too many segments

	@h = map( $_ eq 'z' ? (0) x ($len - @h + 1) : ($_), @h);
	return join('' => map(sprintf("%04x", hex($_)), @h)).$v4;
}

sub parse_ip {
	my $str = shift;
	$str =~ s/^\s*//;
	$str =~ s/\s*$//;

	if ($str =~ s/^((?:\d+\.)+\d+)(?::(\d+))?$//) {
		return 'v4', parse_v4($1, $2);
	}

	my ($ip, $port);
	if ($str =~ /^\[(.*?)\]:(\d+)$/) {
		$port = $2;
		$ip = parse_v6($1);
	} else {
		$port = -1;
		$ip = parse_v6($str);
	}

	return unless $ip;
	return 'v6', $ip, $port;
}

for (qw/127.0.0.1 127.0.0.1:80
	::1
	[::1]:80
	2605:2700:0:3::4713:93e3
	[2605:2700:0:3::4713:93e3]:80
	::ffff:192.168.0.1
	[::ffff:192.168.0.1]:22
	::ffff:127.0.0.0.1
	a::b::1/)
{
	print "$_\n\t";
	my ($ver, $ip, $port) = parse_ip($_)
		or print "parse error\n" and next;

	print "$ver $ip\tport $port\n\n";
}
```
output<lang>127.0.0.1
        v4 7f000001     port -1

127.0.0.1:80
        v4 7f000001     port 80

::1
        v6 00000000000000000000000000000001     port -1

[::1]:80
        v6 00000000000000000000000000000001     port 80

2605:2700:0:3::4713:93e3
        v6 260527000000000300000000471393e3     port -1

[2605:2700:0:3::4713:93e3]:80
        v6 260527000000000300000000471393e3     port 80

::ffff:192.168.0.1
        v6 00000000000000000000ffffc0a80001     port -1

[::ffff:192.168.0.1]:22
        v6 00000000000000000000ffffc0a80001     port 22

::ffff:127.0.0.0.1
        parse error
a::b::1
        parse error
```



## Perl 6


```perl6
grammar IP_Addr {
    token TOP { ^ [ <IPv4> | <IPv6> ] $ }

    token IPv4 {
        [ <d8> +% '.' ] <?{ $<d8> == 4 }> <port>?
                { @*by8 = @$<d8> }
    }

    token IPv6 {
        |     <ipv6>
        | '[' <ipv6> ']' <port>
    }

    token ipv6 {
        | <h16> +% ':' <?{ $<h16> == 8 }>
                { @*by16 = @$<h16> }

        | [ (<h16>) +% ':']? '::' [ (<h16>) +% ':' ]? <?{ @$0 + @$1 ≤ 8 }>
                { @*by16 = |@$0, |('0' xx 8 - (@$0 + @$1)), |@$1 }

        | '::ffff:' <IPv4>
                { @*by16 = |('0' xx 5), 'ffff', |(by8to16 @*by8) }
    }

    token d8  { (\d+) <?{ $0 < 256   }> }
    token d16 { (\d+) <?{ $0 < 65536 }> }
    token h16 { (<:hexdigit>+) <?{ @$0 ≤ 4 }> }

    token port {
        ':' <d16> { $*port = +$<d16> }
    }
}

sub by8to16 (@m) { gather for @m -> $a,$b { take ($a * 256 + $b).fmt("%04x") } }

my @cases = <
    127.0.0.1
    127.0.0.1:80
    ::1
    [::1]:80
    2605:2700:0:3::4713:93e3
    [2605:2700:0:3::4713:93e3]:80
    2001:db8:85a3:0:0:8a2e:370:7334
    2001:db8:85a3::8a2e:370:7334
    [2001:db8:85a3:8d3:1319:8a2e:370:7348]:443
    192.168.0.1
    ::ffff:192.168.0.1
    ::ffff:71.19.147.227
    [::ffff:71.19.147.227]:80
    ::
    256.0.0.0
    g::1
    0000
    0000:0000
    0000:0000:0000:0000:0000:0000:0000:0000
    0000:0000:0000::0000:0000
    0000::0000::0000:0000
    ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
    ffff:ffff:ffff:fffg:ffff:ffff:ffff:ffff
    fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
    fff:ffff:0:ffff:ffff:ffff:ffff:ffff
>;

for @cases -> $addr {
    my @*by8;
    my @*by16;
    my $*port;

    IP_Addr.parse($addr);

    say $addr;
    if @*by16 {
        say "  IPv6: ", @*by16.map({:16(~$_)})».fmt("%04x").join;
        say "  Port: ", $*port if $*port;
    }
    elsif @*by8 {
        say "  IPv4: ", @*by8».fmt("%02x").join;
        say "  Port: ", $*port if $*port;
    }
    else {
        say "  BOGUS!";
    }
    say '';
}
```

{{out}}

```txt
127.0.0.1
  IPv4: 7f000001

127.0.0.1:80
  IPv4: 7f000001
  Port: 80

::1
  IPv6: 00000000000000000000000000000001

[::1]:80
  IPv6: 00000000000000000000000000000001
  Port: 80

2605:2700:0:3::4713:93e3
  IPv6: 260527000000000300000000471393e3

[2605:2700:0:3::4713:93e3]:80
  IPv6: 260527000000000300000000471393e3
  Port: 80

2001:db8:85a3:0:0:8a2e:370:7334
  IPv6: 20010db885a3000000008a2e03707334

2001:db8:85a3::8a2e:370:7334
  IPv6: 20010db885a3000000008a2e03707334

[2001:db8:85a3:8d3:1319:8a2e:370:7348]:443
  IPv6: 20010db885a308d313198a2e03707348
  Port: 443

192.168.0.1
  IPv4: c0a80001

::ffff:192.168.0.1
  IPv6: 00000000000000000000ffffc0a80001

::ffff:71.19.147.227
  IPv6: 00000000000000000000ffff471393e3

[::ffff:71.19.147.227]:80
  IPv6: 00000000000000000000ffff471393e3
  Port: 80

::
  IPv6: 00000000000000000000000000000000

256.0.0.0
  BOGUS!

g::1
  BOGUS!

0000
  BOGUS!

0000:0000
  BOGUS!

0000:0000:0000:0000:0000:0000:0000:0000
  IPv6: 00000000000000000000000000000000

0000:0000:0000::0000:0000
  IPv6: 00000000000000000000000000000000

0000::0000::0000:0000
  IPv6: 00000000000000000000000000000000

ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
  IPv6: ffffffffffffffffffffffffffffffff

ffff:ffff:ffff:fffg:ffff:ffff:ffff:ffff
  BOGUS!

fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff
  IPv6: 0fffffffffffffffffffffffffffffff

fff:ffff:0:ffff:ffff:ffff:ffff:ffff
  IPv6: 0fffffff0000ffffffffffffffffffff
```



## Phix


```Phix
function parse_ip(string txt)
    sequence r
    integer dot = find('.',txt),
            colon = find(':',txt),
            openbr = find('[',txt),
            closebr = find(']',txt)
    if colon=5 and txt[1..4] = "http" then 
        txt = trim(txt[6..$],"/")
        return parse_ip(txt)
    end if
    bool ipv6 = openbr or dot=0 or (colon and colon<dot)
    if ipv6 then
        sequence res = repeat(0,8)
        if openbr then
            if openbr!=1 or closebr<openbr then return 0 end if
            r = scanf(txt[closebr+1..$],":%d")
            if length(r)!=1 then return 0 end if
            atom port = r[1][1]
            if port<0 or port>65535 then return 0 end if
            res &= port
            txt = txt[2..closebr-1]
        end if
        if dot then
            colon = rfind(':',txt)
            if colon>dot then return 0 end if
            object r4 = parse_ip(txt[colon+1..$])
            if not sequence(r4) or length(r4)!=4 then return 0 end if
            res[7] = r4[1]*#100+r4[2]
            res[8] = r4[3]*#100+r4[4]
            txt = txt[1..colon-1+(txt[colon-1]=':')]
            dot = 2
        end if
        sequence r8 = {}
        integer ip = 0
        while length(txt) do
            colon = find(':',txt)
            if colon=1 then
                if ip then return 0 end if
                ip = length(r8)+1
                txt = txt[2+(length(r8)=0)..$]
            else
                r = scanf(txt[1..colon-1],"%x")
                if length(r)!=1 then return 0 end if
                atom r11 = r[1][1]
                if r11<0 or r11>#FFFF then return 0 end if
                r8 &= r11
                if colon=0 then exit end if
                txt = txt[colon+1..$]
            end if
        end while
        if ip then
            if length(r8)>=(8-dot) then return 0 end if
            r8[ip..ip-1] = repeat(0,(8-dot)-length(r8))
        else
            if length(r8)!=8-dot then return 0 end if
        end if
        res[1..8-dot] = r8
        return res
    end if
    -- ipv4:
    if dot=0 or (colon and colon<dot) then return 0 end if
    r = scanf(txt[1..colon-1],"%d.%d.%d.%d")
    if length(r)!=1 then return 0 end if
    r = r[1]
    for i=1 to length(r) do
        if r[i]<0 or r[i]>255 then return 0 end if
    end for
    if colon then
        sequence r2 = scanf(txt[colon+1..$],"%d")
        if length(r2)!=1 then return 0 end if
        atom port = r2[1][1]
        if port<0 or port>65535 then return 0 end if
        r &= port
    end if
    return r
end function

constant tests = {{"127.0.0.1",{127,0,0,1}},
                  {"127.0.0.1:80",{127,0,0,1,80}},
                  {"::1",{0,0,0,0,0,0,0,1}},
                  {"[::1]:80",{0,0,0,0,0,0,0,1,80}},
                  {"2605:2700:0:3::4713:93e3",{#2605,#2700,0,3,0,0,#4713,#93e3}},
                  {"[2605:2700:0:3::4713:93e3]:80",{#2605,#2700,0,3,0,0,#4713,#93e3,80}},
                  {"::ffff:192.168.173.22",{0,0,0,0,0,#ffff,#c0a8,#ad16}},
                  {"[::ffff:192.168.173.22]:80",{0,0,0,0,0,#ffff,#c0a8,#ad16,80}},
                  {"1::",{1,0,0,0,0,0,0,0}},
                  {"[1::]:80",{1,0,0,0,0,0,0,0,80}},
                  {"::",{0,0,0,0,0,0,0,0}},
                  {"[::]:80",{0,0,0,0,0,0,0,0,80}},
                  {"192.168.0.1",{192,168,0,1}},
                  {"2001:db8:85a3:0:0:8a2e:370:7334",{#2001,#db8,#85a3,0,0,#8a2e,#370,#7334}},
                  {"2001:db8:85a3::8a2e:370:7334",{#2001,#db8,#85a3,0,0,#8a2e,#370,#7334}},
                  {"[2001:db8:85a3:8d3:1319:8a2e:370:7334]:443",{#2001,#db8,#85a3,#8d3,#1319,#8a2e,#370,#7334,443}},
                  {"256.0.0.0",0},
                  {"g::1",0},
                  {"::ffff:127.0.0.0.1",0},
                  {"a::b::1",0},
                  {"0000",0},
                  {"0000:0000",0},
                  {"0000:0000:0000:0000:0000:0000:0000:0000",{0,0,0,0,0,0,0,0}},
                  {"0000:0000:0000::0000:0000",{0,0,0,0,0,0,0,0}},
                  {"0000::0000::0000:0000",0},
                  {"ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff",{#ffff,#ffff,#ffff,#ffff,#ffff,#ffff,#ffff,#ffff}},
                  {"ffff:ffff:ffff:fffg:ffff:ffff:ffff:ffff",0},
                  {"fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff",{#fff,#ffff,#ffff,#ffff,#ffff,#ffff,#ffff,#ffff}},
                  {"fff:ffff:0:ffff:ffff:ffff:ffff:ffff",{#fff,#ffff,0,#ffff,#ffff,#ffff,#ffff,#ffff}},
                  {"2001:0db8:0:0:0:0:1428:57ab",{#2001,#0db8,0,0,0,0,#1428,#57ab}},
                  {"2001:0db8::1428:57ab",{#2001,#0db8,0,0,0,0,#1428,#57ab}},
                  {"2001:0db8:0:0:8d3:0:0:0",{#2001,#0db8,0,0,#8d3,0,0,0}},
                  {"2001:0db8:0:0:8d3::",{#2001,#0db8,0,0,#8d3,0,0,0}},
                  {"2001:0db8::8d3:0:0:0",{#2001,#0db8,0,0,#8d3,0,0,0}},
                  {"http://127.0.0.1/",{127,0,0,1}},
                  {"http:",0},
                  {"http:/2001:db8:3:4::127.0.2.0",{#2001,#db8,3,4,0,0,#7F00,#200}},
                  {"::192.168.0.1",{0,0,0,0,0,0,#C0A8,1}},
                  {"::ffff:0:255.255.255.255",{0,0,0,0,#ffff,0,#ffff,#ffff}},
                  {"",0},
                  {"ffffffffffffffffffffffffffffffff::",0},
                  {"[1::]:9999999999999999999999999999",0},
                  {"I think that's enough tests for now",0}}

for i=1 to length(tests) do
    {string ip, object expected} = tests[i]
    object actual = parse_ip(ip)
    if actual!=expected then
        ?{ip,actual,expected}
        ?9/0
    end if
    string addr
    if actual=0 then
        addr = "**not a valid ip address**"
    else
        if remainder(length(actual),2) then
            actual[$] = sprintf(", port %d",actual[$])
        else
            actual = append(actual,"")
        end if
        if length(actual)=5 then
            addr = sprintf("ivp4 address: %02x%02x%02x%02x%s",actual)
        elsif length(actual)=9 then
            addr = sprintf("ivp6 address: %04x%04x%04x%04x%04x%04x%04x%04x%s",actual)
        else
            ?9/0
        end if
    end if
    printf(1,"%45s %s\n",{ip,addr})
end for
```

{{out}}

```txt

                                    127.0.0.1 ivp4 address: 7F000001
                                 127.0.0.1:80 ivp4 address: 7F000001, port 80
                                          ::1 ivp6 address: 00000000000000000000000000000001
                                     [::1]:80 ivp6 address: 00000000000000000000000000000001, port 80
                     2605:2700:0:3::4713:93e3 ivp6 address: 260527000000000300000000471393E3
                [2605:2700:0:3::4713:93e3]:80 ivp6 address: 260527000000000300000000471393E3, port 80
                        ::ffff:192.168.173.22 ivp6 address: 00000000000000000000FFFFC0A8AD16
                   [::ffff:192.168.173.22]:80 ivp6 address: 00000000000000000000FFFFC0A8AD16, port 80
                                          1:: ivp6 address: 00010000000000000000000000000000
                                     [1::]:80 ivp6 address: 00010000000000000000000000000000, port 80
                                           :: ivp6 address: 00000000000000000000000000000000
                                      [::]:80 ivp6 address: 00000000000000000000000000000000, port 80
                                  192.168.0.1 ivp4 address: C0A80001
              2001:db8:85a3:0:0:8a2e:370:7334 ivp6 address: 20010DB885A3000000008A2E03707334
                 2001:db8:85a3::8a2e:370:7334 ivp6 address: 20010DB885A3000000008A2E03707334
   [2001:db8:85a3:8d3:1319:8a2e:370:7334]:443 ivp6 address: 20010DB885A308D313198A2E03707334, port 443
                                    256.0.0.0 **not a valid ip address**
                                         g::1 **not a valid ip address**
                           ::ffff:127.0.0.0.1 **not a valid ip address**
                                      a::b::1 **not a valid ip address**
                                         0000 **not a valid ip address**
                                    0000:0000 **not a valid ip address**
      0000:0000:0000:0000:0000:0000:0000:0000 ivp6 address: 00000000000000000000000000000000
                    0000:0000:0000::0000:0000 ivp6 address: 00000000000000000000000000000000
                        0000::0000::0000:0000 **not a valid ip address**
      ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff ivp6 address: FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      ffff:ffff:ffff:fffg:ffff:ffff:ffff:ffff **not a valid ip address**
       fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff ivp6 address: 0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          fff:ffff:0:ffff:ffff:ffff:ffff:ffff ivp6 address: 0FFFFFFF0000FFFFFFFFFFFFFFFFFFFF
                  2001:0db8:0:0:0:0:1428:57ab ivp6 address: 20010DB80000000000000000142857AB
                         2001:0db8::1428:57ab ivp6 address: 20010DB80000000000000000142857AB
                      2001:0db8:0:0:8d3:0:0:0 ivp6 address: 20010DB80000000008D3000000000000
                          2001:0db8:0:0:8d3:: ivp6 address: 20010DB80000000008D3000000000000
                         2001:0db8::8d3:0:0:0 ivp6 address: 20010DB80000000008D3000000000000
                            http://127.0.0.1/ ivp4 address: 7F000001
                                        http: **not a valid ip address**
                http:/2001:db8:3:4::127.0.2.0 ivp6 address: 20010DB800030004000000007F000200
                                ::192.168.0.1 ivp6 address: 000000000000000000000000C0A80001
                     ::ffff:0:255.255.255.255 ivp6 address: 0000000000000000FFFF0000FFFFFFFF
                                              **not a valid ip address**
           ffffffffffffffffffffffffffffffff:: **not a valid ip address**
           [1::]:9999999999999999999999999999 **not a valid ip address**
          I think that's enough tests for now **not a valid ip address**

```



## PicoLisp


```PicoLisp
# Return a cons pair of address and port: (address . port)
(de ipAddress (Adr)
   (use (@A @B @C @D @Port)
      (cond
         ((match '("[" @A "]" ":" @Port) Adr)
            (adrIPv6 (split @A ":") @Port) )
         ((match '("[" @A "]") Adr)
            (adrIPv6 (split @A ":")) )
         ((match '(@A ":" @B ":" @C) Adr)
            (adrIPv6 (cons @A @B (split @C ":"))) )
         ((match '(@A "." @B "." @C "." @D ":" @Port) Adr)
            (adrIPv4 (list @A @B @C @D) @Port) )
         ((match '(@A "." @B "." @C "." @D) Adr)
            (adrIPv4 (list @A @B @C @D)) )
         (T (quit "Bad IP address" (pack Adr))) ) ) )

(de adrIPv4 (Lst Port)
   (cons
      (sum >> (-24 -16 -8 0) (mapcar format Lst))
      (format Port) ) )

(de adrIPv6 (Lst Port)
   (cons
      (sum >>
         (-112 -96 -80 -64 -48 -32 -16 0)
         (mapcan
            '((X)
               (if X
                  (cons (hex X))
                  (need (- 9 (length Lst)) 0) ) )  # Handle '::'
            (cons (or (car Lst) "0") (cdr Lst)) ) )
      (format Port) ) )
```

Test:

```PicoLisp
(for A
   (quote
      "127.0.0.1"
      "127.0.0.1:80"
      "::1"
      "[::1]:80"
      "2605:2700:0:3::4713:93e3"
      "[2605:2700:0:3::4713:93e3]:80" )
   (let I (ipAddress (chop A))
      (tab (-29 34 40 7)
         A
         (hex (car I))
         (format (car I))
         (cdr I) ) ) )
```

Output:
<pre style="height:8em;overflow:scroll">127.0.0.1                                              7F000001                              2130706433
127.0.0.1:80                                           7F000001                              2130706433     80
::1                                                           1                                       1
[::1]:80                                                      1                                       1     80
2605:2700:0:3::4713:93e3       260527000000000300000000471393E3  50537416338094019778974086937420469219
[2605:2700:0:3::4713:93e3]:80  260527000000000300000000471393E3  50537416338094019778974086937420469219     80
```


==PL/I==

```pli
*process or(!) source xref attributes macro options;
 /*********************************************************************
 * Program to parse an IP address into -->   IPv4 or IPv6 format
 * 28.05.2013 Walter Pachl translated from REXX version 3
 *                         x2d was the hard part :-)
 *********************************************************************/
 ip: proc options(main);
 Dcl ipa   char(50) Var;
 Dcl ipi   char(50) Var;
 Dcl ipax  char(50) Var Init('');
 Dcl ipad  char(50) Var Init('');
 Dcl space char(4);
 Dcl port  char(5) Var;
 dcl head Char(132) Var;
 head='       input IP address                 hex IP address   '!!
      '                 decimal IP address            space  port';
 Put Edit(head)(Skip,a);
 Put Edit(copies('_',30),
          copies('_',32),
          copies('_',39),
          copies('_', 5),
          copies('_', 5))
         (Skip,6(a,x(1)));

 call expand('127.0.0.1');
 call expand('127.0.0.1:80');
 call expand('2605:2700:0:3::4713:93e3');
 call expand('[2605:2700:0:3::4713:93e3]:80');
 call expand('::1');
 call expand('[::1]:80');

 expand: procedure(s);
 Dcl s Char(50) Var;
 ipi=s;
 ipa=s;
 If index(ipa,'.')>0 Then
   Call expand_ipv4;
 Else
   Call expand_ipv6;
 ipad=x2d(ipax);
 Put Edit(left(ipi,30),right(ipax,32),right(ipad,39),
          right(space,5),right(port,5))
         (Skip,6(a,x(1)));
 End;

 expand_ipv4: Proc;
 Dcl a(4) Char(3) Var;
 Dcl (pp,j) Bin Fixed(31);
 space='IPv4';
 pp=index(ipa,':');
 If pp>0 Then Do;
   port=substr(ipa,pp+1);
   ipa=left(ipa,pp-1);
   End;
 Else
   Port='';
 Call parse((ipa),'.',a);
 ipax='';
 do j=1 To 4;
   ipax=ipax!!a(j);
   end;
 End;

 expand_ipv6: Proc;
 Dcl a(8) Char(4) Var;
 Dcl (s,o1,o2) Char(50) Var Init('');
 Dcl (i,ii,pp,j,n) Bin Fixed(31) Init(0);
 space='IPv6';
 pp=index(ipa,']:');
 If pp>0 Then Do;
   port=substr(ipa,pp+2);
   ipa=substr(ipa,2,pp-2);
   End;
 Else
   Port='';
 s=ipa;
 j=0;
 Do i=1 To 8 While(s>'');
   pp=index(s,':');
   dcl temp Char(6) Var;
   If pp>1 Then
     temp=left(s,pp-1);
   Else
     temp=s;
   temp=right(temp,4,'0');
   Select(pp);
     When(0) Do;
       a(i)=temp;
       s='';
       End;
     When(1) Do;
       a(i)='----';
       ii=i;
       s=substr(s,pp+1);
       If left(s,1)=':' Then
         s=substr(s,2);
       End;
     Otherwise Do;
       a(i)=temp;
       s=substr(s,pp+1);
       End;
     End;
   End;
 n=i-1;
 o1='';
 o2='';
 Do i=1 To n;
   If i=ii Then Do;
     o1=o1!!'----';
     Do j=1 To 9-n;
       o2=o2!!'0000';
       End;
     End;
   Else Do;
     o1=o1!!right(a(i),4,'0');
     o2=o2!!right(a(i),4,'0');
     End;
   End;
  ipax=o2;
 End;

 parse: Proc(s,c,a);
 Dcl s Char(50) Var;
 Dcl c Char( 1);
 Dcl a(*) Char(*) Var;
 Dcl (i,p) Bin Fixed(31);
 a='';
 Do i=1 By 1 While(length(s)>0);
   p=index(s,c);
   If p>0 Then Do;
     a(i)=left(s,p-1);
     s=substr(s,p+1);
     End;
   Else Do;
     a(i)=s;
     s='';
     End;
   End;
  End;

 /*
 underscore: Proc(s) Returns(char(132) Var);
 Dcl s Char(*);
 Dcl r Char(length(s)) Var Init('');
 Dcl i Bin Fixed(31);
 Dcl us Bit(1) Init('0'b);
 Do i=1 To length(s)-1;
   If substr(s,i,1)>' ' Then Do;
     r=r!!'_';
     us='1'b;
     End;
   Else Do;
     If substr(s,i+1,1)>' ' & us Then
       r=r!!'_';
     Else Do;
       r=r!!' ';
       us='0'b;
       End;
     End;
   End;
 If substr(s,length(s),1)>' ' Then
   r=r!!'_';
 Return(r);
 End;

 center: Proc(s,l) Returns(char(50) Var);
 Dcl s char(50) Var;
 Dcl (l,b) Bin Fixed(31);
 b=(l-length(s))/2;
 Return(left(copies(' ',b)!!s,l));
 End;
 */
 copies: Proc(c,n) Returns(char(50) Var);
 Dcl c char(50) Var;
 Dcl n Bin Fixed(31);
 Return(repeat(c,n-1));
 End;


 c2d: Procedure(s) Returns(Char(50) Var);
 Dcl s Char(*) Var;
 Dcl d Pic'99';
 Dcl (v,part,result,old) Char(100) Var;
 Dcl i Bin Fixed(31);
 result='0';
 v='1';

 Do i=length(s) To 1 By -1;
   d=c2d(substr(s,i,1));
   part=longmult((v),(d));
   result=longadd((result),(part));
   v=longmult((v),'16');
   End;
 Do While(left(result,1)='0');
   result=substr(result,2);
   End;
 Return(result);
 /*
 dbg: Proc(txt);
 Dcl txt Char(*);
 Put Skip list(txt);
 End;
 */
 x2d: Procedure(c) Returns(Char(2));
 Dcl c Char(1);
 Dcl res Char(2);
 Select(c);
   When('a','A') res='10';
   When('b','B') res='11';
   When('c','C') res='12';
   When('d','D') res='13';
   When('e','E') res='14';
   When('f','F') res='15';
   Otherwise res='0'!!c;
   End;
 Return(res);
 End;

 longmult: Procedure(as,bs) Returns(Char(1000) Var);
 /* REXX **************************************************************
 * Multiply(as,bs) -> as*bs
 *********************************************************************/
 Dcl (as,bs) Char(*);
 Dcl (a(1000),b(1000),r(1000)) Pic'9';
 Dcl (p,s) Pic'99';
 Dcl (al,bl) Bin Fixed(31);
 Dcl (i,ai,bi,ri,rim) Bin Fixed(31);
 Dcl res Char(1000) Var Init((1000)'0');
 al=length(as); Do ai=al To 1 By -1; a(ai)=substr(as,al-ai+1,1); End;
 bl=length(bs); Do bi=bl To 1 By -1; b(bi)=substr(bs,bl-bi+1,1); End;
 r=0;
 rim=0;
 Do bi=1 To bl;
   Do ai=1 To al;
     ri=ai+bi-1;
     p=a(ai)*b(bi);
     Do i=ri by 1 Until(p=0);
       s=r(i)+p;
       r(i)=mod(s,10);
       p=s/10;
       End;
     rim=max(rim,i);
     End;
   End;
 res='';
 Do i=1 To rim;
   res=r(i)!!res;
   End;
 Return(res);
 End;

 longadd: proc(as,bs) Returns(Char(100) Var);
 Dcl (as,bs) Char(*) Var;
 Dcl cs Char(100) Var Init('');
 Dcl (al,bl,cl,i) Bin Fixed(31);
 Dcl a(100) Pic'9' Init((100)0);
 Dcl b(100) Pic'9' Init((100)0);
 Dcl c(100) Pic'9' Init((100)0);
 Dcl temp Pic'99';
 al=length(as);
 bl=length(bs);
 Do i=1 To al; a(i)=substr(as,al-i+1,1); End;
 Do i=1 To bl; b(i)=substr(bs,bl-i+1,1); End;
 cl=max(al,bl)+1;
 Do i=1 To cl;
   temp=a(i)+b(i)+c(i);
   c(i)=mod(temp,10);
   c(i+1)=c(i+1)+temp/10;
   End;
 Do i=1 To cl;
   cs=c(i)!!cs;
   End;
 Return(cs);
 End;
 End;

 end;
```

Output:
<pre style="overflow:scroll">

       input IP address                 hex IP address                    decimal IP address            space  port
______________________________ ________________________________ _______________________________________ _____ _____
127.0.0.1                                                127001                                 1208321  IPv4
127.0.0.1:80                                             127001                                 1208321  IPv4    80
2605:2700:0:3::4713:93e3       260527000000000300000000471393e3  50537416338094019778974086937420469219  IPv6
[2605:2700:0:3::4713:93e3]:80  260527000000000300000000471393e3  50537416338094019778974086937420469219  IPv6    80
::1                            00000000000000000000000000000001                                       1  IPv6
[::1]:80                       00000000000000000000000000000001                                       1  IPv6    80
```



## PowerShell


```PowerShell

function Get-IpAddress
{
    [CmdletBinding()]
    [OutputType([PSCustomObject])]
    Param
    (
        [Parameter(Mandatory=$true,
                   ValueFromPipeline=$true,
                   ValueFromPipelineByPropertyName=$true,
                   Position=0)]
        $InputObject
    )

    Begin
    {
        function Get-Address ([string]$Address)
        {
            if ($Address.IndexOf(".") -ne -1)
            {
                $Address, $port = $Address.Split(":")

                [PSCustomObject]@{
                    IPAddress = [System.Net.IPAddress]$Address
                    Port      = $port
                }
            }
            else
            {
                if ($Address.IndexOf("[") -ne -1)
                {
                    [PSCustomObject]@{
                        IPAddress = [System.Net.IPAddress]$Address
                        Port      = ($Address.Split("]")[-1]).TrimStart(":")
                    }
                }
                else
                {
                    [PSCustomObject]@{
                        IPAddress = [System.Net.IPAddress]$Address
                        Port      = $null
                    }
                }
            }
        }
    }
    Process
    {
        $InputObject | ForEach-Object {
            $address = Get-Address $_
            $bytes = ([System.Net.IPAddress]$address.IPAddress).GetAddressBytes()
            [Array]::Reverse($bytes)
            $i = 0
            $bytes | ForEach-Object -Begin   {[bigint]$decimalIP = 0} `
                                    -Process {$decimalIP += [bigint]$_ * [bigint]::Pow(256, $i); $i++} `
                                    -End     {[PSCustomObject]@{
                                                  Address = $address.IPAddress
                                                  Port    = $address.Port
                                                  Hex     = "0x$($decimalIP.ToString('x'))"}
                                             }
        }
    }
}

```

Parse an array of IP addresses in a text format:

```PowerShell

$ipAddresses = "127.0.0.1","127.0.0.1:80","::1","[::1]:80","2605:2700:0:3::4713:93e3","[2605:2700:0:3::4713:93e3]:80" | Get-IpAddress
$ipAddresses

```

{{Out}}

```txt

Address                  Port Hex                               
-------                  ---- ---                               
127.0.0.1                     0x7f000001                        
127.0.0.1                80   0x7f000001                        
::1                           0x1                               
::1                      80   0x1                               
2605:2700:0:3::4713:93e3      0x260527000000000300000000471393e3
2605:2700:0:3::4713:93e3 80   0x260527000000000300000000471393e3

```

The '''Address''' "property" is an object containing more information...

```PowerShell

$ipAddresses[5].Address

```

{{Out}}

```txt

Address            : 
AddressFamily      : InterNetworkV6
ScopeId            : 0
IsIPv6Multicast    : False
IsIPv6LinkLocal    : False
IsIPv6SiteLocal    : False
IsIPv6Teredo       : False
IsIPv4MappedToIPv6 : False
IPAddressToString  : 2605:2700:0:3::4713:93e3

```

... allowing for specific filtering:

```PowerShell

$ipAddresses | where {$_.Address.AddressFamily -eq "InterNetworkV6" -and $_.Port -ne $null}

```

{{Out}}

```txt

Address                  Port Hex                               
-------                  ---- ---                               
::1                      80   0x1                               
2605:2700:0:3::4713:93e3 80   0x260527000000000300000000471393e3

```



## Python


### Python: Using ipaddress

{{works with|Python|3.5}}

```python
from ipaddress import ip_address
from urllib.parse import urlparse

tests = [
    "127.0.0.1",
    "127.0.0.1:80",
    "::1",
    "[::1]:80",
    "::192.168.0.1",
    "2605:2700:0:3::4713:93e3",
    "[2605:2700:0:3::4713:93e3]:80" ]

def parse_ip_port(netloc):
    try:
        ip = ip_address(netloc)
        port = None
    except ValueError:
        parsed = urlparse('//{}'.format(netloc))
        ip = ip_address(parsed.hostname)
        port = parsed.port
    return ip, port

for address in tests:
    ip, port = parse_ip_port(address)
    hex_ip = {4:'{:08X}', 6:'{:032X}'}[ip.version].format(int(ip))
    print("{:39s}  {:>32s}  IPv{}  port={}".format(
        str(ip), hex_ip, ip.version, port ))
```


{{out}}

```txt

127.0.0.1                                                        7F000001  IPv4  port=None
127.0.0.1                                                        7F000001  IPv4  port=80
::1                                      00000000000000000000000000000001  IPv6  port=None
::1                                      00000000000000000000000000000001  IPv6  port=80
::c0a8:1                                 000000000000000000000000C0A80001  IPv6  port=None
2605:2700:0:3::4713:93e3                 260527000000000300000000471393E3  IPv6  port=None
2605:2700:0:3::4713:93e3                 260527000000000300000000471393E3  IPv6  port=80

```


### Python: Using pyparsing

{{libheader|pyparse}}
The following uses [http://pyparsing.wikispaces.com/ pyparse] to parse the IP address.  It's an attempt at using pyparse to describe an IP address in an ''extended'' [[wp:Backus–Naur_Form|BNF syntax]].  Using a parser does seems a bit like using a sledgehammer to crack a nut.  However it does make for an interesting alternative to using a [[Regular expression|regular expressions]] to parse IP addresses.  Note - for example - that the parser specifically reports - as an exception - the location where the IP address is syntactically wrong.

```python
import string
from pyparsing import * # import antigravity

tests="""#
127.0.0.1                       # The "localhost" IPv4 address
127.0.0.1:80                    # The "localhost" IPv4 address, with a specified port (80)
::1                             # The "localhost" IPv6 address
[::1]:80                        # The "localhost" IPv6 address, with a specified port (80)
2605:2700:0:3::4713:93e3        # Rosetta Code's primary server's public IPv6 address
[2605:2700:0:3::4713:93e3]:80   # Rosetta Code's primary server's public IPv6 address, +port (80)
2001:db8:85a3:0:0:8a2e:370:7334 # doc, IPv6 for 555-1234
2001:db8:85a3::8a2e:370:7334    # doc
[2001:db8:85a3:8d3:1319:8a2e:370:7348]:443 # doc +port
192.168.0.1                     # private
::ffff:192.168.0.1              # private transitional
::ffff:71.19.147.227            # Rosetta Code's transitional
[::ffff:71.19.147.227]:80       # Rosetta Code's transitional +port
::                              # unspecified
256.0.0.0                       # invalid, octet > 255 (currently not detected)
g::1                            # invalid
0000                                    Bad address
0000:0000                               Bad address
0000:0000:0000:0000:0000:0000:0000:0000 Good address
0000:0000:0000::0000:0000               Good Address
0000::0000::0000:0000                   Bad address
ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff Good address
ffff:ffff:ffff:fffg:ffff:ffff:ffff:ffff Bad address
fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff  Good address
fff:ffff:0:ffff:ffff:ffff:ffff:ffff     Good address
"""

def print_args(args):
  print "print_args:", args

def join(args):
  args[0]="".join(args)
  del args[1:]

def replace(val):
  def lambda_replace(args):
    args[0]=val
    del args[1:]
  return lambda_replace

def atoi(args): args[0]=string.atoi(args[0])
def itohex2(args): args[0]="%02x"%args[0]

def hextoi(args): args[0]=string.atoi(args[0], 16)
def itohex4(args): args[0]="%04x"%args[0]

def assert_in_range(lwb, upb):
  def range_check(args):
    return # turn range checking off
    if args[0] < lwb:
      raise ValueError,"value %d < %d"%(args[0], lwb)
    if args[0] > upb:
      raise ValueError,"value %d > %d"%(args[0], upb)
  return range_check

dot = Literal(".").suppress()("dot"); colon = Literal(":").suppress()("colon")
octet = Word(nums).setParseAction(atoi,assert_in_range(0,255),itohex2)("octet");

port = Word(nums).setParseAction(atoi,assert_in_range(0,256*256-1))("port")
ipv4 = (octet + (dot+octet)*3)("addr")
ipv4.setParseAction(join) #,hextoi)

ipv4_port = ipv4+colon.suppress()+port

a2f = "abcdef"
hex = oneOf(" ".join(nums+a2f));

hexet = (hex*(0,4))("hexet")
hexet.setParseAction(join, hextoi, itohex4)

max=8; stop=max+1

xXXXX_etc = [None, hexet]; xXXXX_etc.extend([hexet + (colon+hexet)*n for n in range(1,max)])
x0000_etc = [ Literal("::").setParseAction(replace("0000"*num_x0000s)) for num_x0000s in range(stop) ]

ipv6=xXXXX_etc[-1]+x0000_etc[0] | xXXXX_etc[-1]

# Build a table of rules for IPv6, in particular the double colon
for num_prefix in range(max-1, -1, -1):
  for num_x0000s in range(0,stop-num_prefix):
    x0000 = x0000_etc[num_x0000s]
    num_suffix=max-num_prefix-num_x0000s
    if num_prefix:
      if num_suffix: pat = xXXXX_etc[num_prefix]+x0000+xXXXX_etc[num_suffix]
      else:          pat = xXXXX_etc[num_prefix]+x0000
    elif num_suffix: pat =                       x0000+xXXXX_etc[num_suffix]
    else: pat=x0000
    ipv6 = ipv6 | pat

ipv6.setParseAction(join) # ,hextoi)
ipv6_port = Literal("[").suppress() + ipv6 + Literal("]").suppress()+colon+port

ipv6_transitional = (Literal("::ffff:").setParseAction(replace("0"*20+"ffff"))+ipv4).setParseAction(join)
ipv6_transitional_port = Literal("[").suppress() + ipv6_transitional + Literal("]").suppress()+colon+port

ip_fmt = (
           (ipv4_port|ipv4)("ipv4") |
           (ipv6_transitional_port|ipv6_transitional|ipv6_port|ipv6)("ipv6")
         ) + LineEnd()

class IPAddr(object):
  def __init__(self, string):
    self.service = dict(zip(("address","port"), ip_fmt.parseString(string)[:]))
  def __getitem__(self, key): return self.service[key]
  def __contains__(self, key): return key in self.service
  def __repr__(self): return `self.service` # "".join(self.service)
  address=property(lambda self: self.service["address"])
  port=property(lambda self: self.service["port"])
  is_service=property(lambda self: "port" in self.service)
  version=property(lambda self: {False:4, True:6}[len(self.address)>8])

for test in tests.splitlines():
  if not test.startswith("#"):
    ip_str, desc = test.split(None,1)
    print ip_str,"=>",
    try:
      ip=IPAddr(ip_str)
      print ip, "IP Version:",ip.version,"- Address is OK!",
    except (ParseException,ValueError), details: print "Bad! IP address syntax error detected:",details,
    print "- Actually:",desc
```

Output:

```txt

127.0.0.1 => {'address': '7f000001'} IP Version: 4 - Address is OK! - Actually: # The "localhost" IPv4 address
127.0.0.1:80 => {'port': 80, 'address': '7f000001'} IP Version: 4 - Address is OK! - Actually: # The "localhost" IPv4 address, with a specifie
d port (80)
::1 => {'address': '00000000000000000000000000000001'} IP Version: 6 - Address is OK! - Actually: # The "localhost" IPv6 address
[::1]:80 => {'port': 80, 'address': '00000000000000000000000000000001'} IP Version: 6 - Address is OK! - Actually: # The "localhost" IPv6 addr
ess, with a specified port (80)
2605:2700:0:3::4713:93e3 => {'address': '260527000000000300000000471393e3'} IP Version: 6 - Address is OK! - Actually: # Rosetta Code's primar
y server's public IPv6 address
[2605:2700:0:3::4713:93e3]:80 => {'port': 80, 'address': '260527000000000300000000471393e3'} IP Version: 6 - Address is OK! - Actually: # Rose
tta Code's primary server's public IPv6 address, +port (80)
2001:db8:85a3:0:0:8a2e:370:7334 => {'address': '20010db885a3000000008a2e03707334'} IP Version: 6 - Address is OK! - Actually: # doc, IPv6 for 
555-1234
2001:db8:85a3::8a2e:370:7334 => {'address': '20010db885a3000000008a2e03707334'} IP Version: 6 - Address is OK! - Actually: # doc
[2001:db8:85a3:8d3:1319:8a2e:370:7348]:443 => {'port': 443, 'address': '20010db885a308d313198a2e03707348'} IP Version: 6 - Address is OK! - Ac
tually: # doc +port
192.168.0.1 => {'address': 'c0a80001'} IP Version: 4 - Address is OK! - Actually: # private
::ffff:192.168.0.1 => {'address': '00000000000000000000ffffc0a80001'} IP Version: 6 - Address is OK! - Actually: # private transitional
::ffff:71.19.147.227 => {'address': '00000000000000000000ffff471393e3'} IP Version: 6 - Address is OK! - Actually: # Rosetta Code's transition
al
[::ffff:71.19.147.227]:80 => {'port': 80, 'address': '00000000000000000000ffff471393e3'} IP Version: 6 - Address is OK! - Actually: # Rosetta 
Code's transitional +port
:: => {'address': '00000000000000000000000000000000'} IP Version: 6 - Address is OK! - Actually: # unspecified
256.0.0.0 => {'address': '100000000'} IP Version: 6 - Address is OK! - Actually: # invalid, octet > 255 (currently not detected)
g::1 => Bad! IP address syntax error detected:  (at char 4), (line:1, col:5) - Actually: # invalid
0000 => Bad! IP address syntax error detected: Expected "." (at char 4), (line:1, col:5) - Actually: Bad address
0000:0000 => Bad! IP address syntax error detected: Expected ":" (at char 9), (line:1, col:10) - Actually: Bad address
0000:0000:0000:0000:0000:0000:0000:0000 => {'address': '00000000000000000000000000000000'} IP Version: 6 - Address is OK! - Actually: Good add
ress
0000:0000:0000::0000:0000 => {'address': '00000000000000000000000000000000'} IP Version: 6 - Address is OK! - Actually: Good Address
0000::0000::0000:0000 => Bad! IP address syntax error detected: Expected end of line (at char 10), (line:1, col:11) - Actually: Bad address
ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff => {'address': 'ffffffffffffffffffffffffffffffff'} IP Version: 6 - Address is OK! - Actually: Good add
ress
ffff:ffff:ffff:fffg:ffff:ffff:ffff:ffff => Bad! IP address syntax error detected: Expected ":" (at char 18), (line:1, col:19) - Actually: Bad 
address
fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff => {'address': '0fffffffffffffffffffffffffffffff'} IP Version: 6 - Address is OK! - Actually: Good addr
ess
fff:ffff:0:ffff:ffff:ffff:ffff:ffff => {'address': '0fffffff0000ffffffffffffffffffff'} IP Version: 6 - Address is OK! - Actually: Good address

```



## Racket


```racket

#lang racket

(require net/private/ip)

(define (bytes->hex bs)
  (string-append* (map (λ(n) (~r n #:base 16 #:min-width 2 #:pad-string "0"))
                       (bytes->list bs))))

(define (parse-ip str)
  (define-values [ipstr portstr]
    (match str
      [(regexp #rx"^([0-9.]+):([0-9]+)$" (list _ i p)) (values i p)]
      [(regexp #rx"^\\[([0-9a-fA-F:]+)\\]:([0-9]+)$" (list _ i p)) (values i p)]
      [_ (values str "")]))
  (define ip (make-ip-address ipstr))
  (define 4? (ipv4? ip))
  (define hex (bytes->hex ((if 4? ipv4-bytes ipv6-bytes) ip)))
  (displayln (~a (~a str #:min-width 30)
                 " "
                 (~a hex #:min-width 32 #:align 'right)
                 " ipv" (if 4? "4" "6") " " portstr)))

(for-each parse-ip
          '("127.0.0.1"
            "127.0.0.1:80"
            "::1"
            "[::1]:80"
            "2605:2700:0:3::4713:93e3"
            "[2605:2700:0:3::4713:93e3]:80"))

```

{{out}}

```txt

127.0.0.1                                              7f000001 ipv4 
127.0.0.1:80                                           7f000001 ipv4 80
::1                            00000000000000000000000000000001 ipv6 
[::1]:80                       00000000000000000000000000000001 ipv6 80
2605:2700:0:3::4713:93e3       260527000000000300000000471393e3 ipv6 
[2605:2700:0:3::4713:93e3]:80  260527000000000300000000471393e3 ipv6 80

```



## REXX

One of REXX's strongest features is its ability for parsing, it has PARSE instruction for this purpose.

### version 1


```rexx
/*REXX program parses an  IP address  into  ──►  IPv4  or  IPv6 format,  optional pport.*/
_="_";     say center('input IP address'   , 30),
               center('hex IP address'     , 32),
               center('decimal IP address' , 39)         'space  port'
           say copies(_, 30)   copies(_, 32)   copies(_, 39)   copies(_, 5)   copies(_, 5)
call IP_parse  127.0.0.1                         /*this simple  IP  doesn't need quotes.*/
call IP_parse '127.0.0.1:80'
call IP_parse '::1'
call IP_parse '[::1]:80'
call IP_parse '2605:2700:0:3::4713:93e3'
call IP_parse '[2605:2700:0:3::4713:93e3]:80'
exit                                             /*stick a fork in it,  we're all done. */
/*──────────────────────────────────────────────────────────────────────────────────────*/
IP_parse:  procedure;        parse arg a .;        hx=;       @.=;       numeric digits 50
           dot=pos(.,a)\==0                      /*see if there is a dot present in IP. */

           if dot then do;   parse var   a    @.1  '.'  @.2  "."  @.3  '.'  @.4  ":"  port
                                            do j=1  for 4;    hx=hx || d2x(@.j, 2)
                                            end   /*j*/
                       end
                  else do;   parse var  a  pureA  ']:'  port
                       _=space( translate( pureA, , '[]'), 0)         /*remove brackets.*/
                       parse var _ x '::' y
                                            do L=1  until x==''       /*get  left side. */
                                            parse var  x  @.L  ':'  x
                                            end   /*L*/
                       y=reverse(y)
                                            do r=8  by -1             /*get right side. */
                                            parse var  y  z  ':'  y;   if z=='' then leave
                                            @.r=reverse(z)
                                            end   /*r*/

                            do k=1  for 8;  hx=hx || right( word(@.k 0, 1), 4, 0)
                            end   /*k*/
                       end

           say left(a,30) right(hx,32) right(x2d(hx),39) ' IPv' || (6-2*dot) right(port,5)
           return
```

{{out|output|text=  when using the internal default input:}}

```txt

       input IP address                 hex IP address                    decimal IP address            space  port
────────────────────────────── ──────────────────────────────── ─────────────────────────────────────── ───── ─────
127.0.0.1                                              7F000001                              2130706433  IPv4
127.0.0.1:80                                           7F000001                              2130706433  IPv4    80
::1                            00000000000000000000000000000001                                       1  IPv6
[::1]:80                       00000000000000000000000000000001                                       1  IPv6    80
2605:2700:0:3::4713:93e3       260527000000000300000000471393e3  50537416338094019778974086937420469219  IPv6
[2605:2700:0:3::4713:93e3]:80  260527000000000300000000471393e3  50537416338094019778974086937420469219  IPv6    80

```



### version 2


```rexx
/* REXX ***************************************************************
* 27.05.2013 Walter Pachl
**********************************************************************/
Numeric Digits 100
say center('input IP address'   ,30),
    center('hex IP address'     ,32),
    center('decimal IP address' ,39)         'space  port'

say copies(_,30) copies(_,32) copies(_,39) copies(_,5) copies(_,5) /*hdr*/
call expand '127.0.0.1'
call expand '127.0.0.1:80'
call expand '::1'
call expand '[::1]:80'
call expand '2605:2700:0:3::4713:93e3'
call expand '[2605:2700:0:3::4713:93e3]:80'
Say ' '
Say 'The following 2 are the same'
Call expand '2001:0db8:0:0:0:0:1428:57ab'
Call expand '2001:db8::1428:57ab'
Say ' '
Say 'The following 3 are all the same'
Call expand '2001:0db8:0:0:8d3:0:0:0'
Call expand '2001:db8:0:0:8d3::'
Call expand '2001:db8::8d3:0:0:0'
Exit

expand: Procedure
Parse Arg s
If pos('.',s)>0 Then
  Parse Value expand_ip4(s) With exp space port
Else
  Parse Value expand_ip6(s) With exp space port
Say left(s,30) right(exp,32) right(x2d(exp),39) right(space,5) right(port,5)
Return

expand_ip4: Procedure
Parse Arg s
If pos(':',s)>0 Then
  Parse Var s s ':' port
Else
  port=''
Do i=1 To 4
  Parse Var s a.i '.' s
  End
res=''
Do i=1 To 4
  res=res||d2x(a.i,2)
  End
Return res 'IPv4' port

expand_ip6: Procedure
/**********************************************************************
* Note: Doublecolon ('::') requires the inclusion of as many 0000
* tokens as necessary to result in 8 tokens
**********************************************************************/
Parse Arg s
If pos(']:',s)>0 Then
  Parse Var s '[' s ']:' port
Else
  port=''
sc=s
ii=0
Do i=1 To 8 While s<>''
  Parse Var s x.i ':' s
  If left(s,1)=':' Then Do
    ii=i
    s=substr(s,2)
    End
  End
n=i-1
ol=''
o2=''
Do i=1 To n
  ol=ol||right(x.i,4,'0')
  o2=o2||right(x.i,4,'0')
  If i=ii Then Do
    ol=ol||'----'
    Do j=1 To 8-n
      o2=o2||'0000'
      End
    End
  End
Return o2 'IPv6' port
```

Output:
<pre style="overflow:scroll">

       input IP address                 hex IP address                    decimal IP address            space  port
______________________________ ________________________________ _______________________________________ _____ _____
127.0.0.1                                              7F000001                              2130706433  IPv4
127.0.0.1:80                                           7F000001                              2130706433  IPv4    80
::1                            00000000000000000000000000000001                                       1  IPv6
[::1]:80                       00000000000000000000000000000001                                       1  IPv6    80
2605:2700:0:3::4713:93e3       260527000000000300000000471393e3  50537416338094019778974086937420469219  IPv6
[2605:2700:0:3::4713:93e3]:80  260527000000000300000000471393e3  50537416338094019778974086937420469219  IPv6    80

The following 2 are the same
2001:0db8:0:0:0:0:1428:57ab    20010db80000000000000000142857ab  42540766411282592856903984951992014763  IPv6
2001:db8::1428:57ab            20010db80000000000000000142857ab  42540766411282592856903984951992014763  IPv6

The following 3 are all the same
2001:0db8:0:0:8d3:0:0:0        20010db80000000008d3000000000000  42540766411282592857539836924043198464  IPv6
2001:db8:0:0:8d3::             20010db80000000008d3000000000000  42540766411282592857539836924043198464  IPv6
2001:db8::8d3:0:0:0            20010db80000000008d3000000000000  42540766411282592857539836924043198464  IPv6
```



## Ruby



```ruby
require 'ipaddr'

 
TESTCASES = ["127.0.0.1",                "127.0.0.1:80",
                "::1",                      "[::1]:80",
                "2605:2700:0:3::4713:93e3", "[2605:2700:0:3::4713:93e3]:80"]                            

output = [%w(String Address Port Family Hex),
          %w(------ ------- ---- ------ ---)]

def output_table(rows)
  widths = []
  rows.each {|row| row.each_with_index {|col, i| widths[i] = [widths[i].to_i, col.to_s.length].max }}
  format = widths.map {|size| "%#{size}s"}.join("\t")
  rows.each {|row| puts format % row}
end

TESTCASES.each do |str|
  case str  # handle port; IPAddr does not.
  when /\A\[(?<address> .* )\]:(?<port> \d+ )\z/x      # string like "[::1]:80"
    address, port = $~[:address], $~[:port]
  when /\A(?<address> [^:]+ ):(?<port> \d+ )\z/x       # string like "127.0.0.1:80"
    address, port = $~[:address], $~[:port]
  else                                                 # string with no port number
    address, port = str, nil
  end
  
  ip_addr = IPAddr.new(address) 
  family = "IPv4" if ip_addr.ipv4?
  family = "IPv6" if ip_addr.ipv6?

  output << [str, ip_addr.to_s, port.to_s, family, ip_addr.to_i.to_s(16)]
end

output_table(output)
```

{{Output}}

```txt
                       String	                 Address	Port	Family	                             Hex
                       ------	                 -------	----	------	                             ---
                    127.0.0.1	               127.0.0.1	    	  IPv4	                        7f000001
                 127.0.0.1:80	               127.0.0.1	  80	  IPv4	                        7f000001
                          ::1	                     ::1	    	  IPv6	                               1
                     [::1]:80	                     ::1	  80	  IPv6	                               1
     2605:2700:0:3::4713:93e3	2605:2700:0:3::4713:93e3	    	  IPv6	260527000000000300000000471393e3
[2605:2700:0:3::4713:93e3]:80	2605:2700:0:3::4713:93e3	  80	  IPv6	260527000000000300000000471393e3

```



## Scala


### Full blown ultimate solution

{{Out}}Best seen running in your browser [https://scastie.scala-lang.org/kWO8C5pLSu6xEQ0kHAlswg Scastie (remote JVM)].

```Scala
object IPparser extends App {

  /*
  Parse an IP (v4/v6) Address

  This software can parse all ipv4/ipv6 address text representations
  of IP Address in common usage against the IEF RFC 5952 specification.

  The results of the parse are:
  - The parts of the text are valid representations. This is indicated in the list by a ✔ or ✘.
  - The intended version; 4 or 6.
  - Compliance with RFC 5952 in respect with double colons Compressed zeroes expansion ('::') and lower case letters.
  - Hexadecimal representation of the intended IP address.
  - If part in the text the port number which is optional.
  - The used text string search pattern.

  As much of the information is produced if there are invalid parts in the remark field.
  */

  def myCases = Map(
    "http:"                                      -> IPInvalidAddressComponents(remark = "No match at all: 'http:'."),
    "http://"                                    -> IPInvalidAddressComponents(remark = "No match at all: 'http://'."),
    "http:// "                                   -> IPInvalidAddressComponents(remark = "No match at all: 'http:// '."),
    "http://127.0.0.1/"                          -> ResultContainer(4, BigInt("7F000001", 16)),
    "http://127.0.0.1:80/"                       -> ResultContainer(4, BigInt("7F000001", 16), Some(80)),
    "http://127.0.0.1:65536" ->
      IPInvalidAddressComponents(4, BigInt("7F000001", 16), Some(65536), remark = "Port number out of range."),
    "http://192.168.0.1"                         -> ResultContainer(4, BigInt("C0A80001", 16)),
    "http:/1::"                                  -> ResultContainer(6, BigInt("10000000000000000000000000000", 16)),
    "http:/2001:0db8:0:0:0:0:1428:57ab/"         -> ResultContainer(6, BigInt("20010db80000000000000000142857ab", 16)),
    "2001:0db8:0:0:8d3:0:0:0"                    -> ResultContainer(6, BigInt("20010db80000000008d3000000000000", 16)),
    "2001:db8:0:0:8d3::"                         -> ResultContainer(6, BigInt("20010db80000000008d3000000000000", 16)),
    "http:/2001:db8:3:4::192.0.2.33"                   -> ResultContainer(6, BigInt("20010db80003000400000000c0000221", 16)),
    "2001:db8:85a3:0:0:8a2e:370:7334"            -> ResultContainer(6, BigInt("20010db885a3000000008a2e03707334", 16)),
    "2001:db8::1428:57ab"                        -> ResultContainer(6, BigInt("20010db80000000000000000142857ab", 16)),
    "2001:db8::8d3:0:0:0"                        -> ResultContainer(6, BigInt("20010db80000000008d3000000000000", 16)),
    "256.0.0.0"                                  -> IPInvalidAddressComponents(4, remark = "Invalid octets."),
    "2605:2700:0:3::4713:93e3"                   -> ResultContainer(6, BigInt("260527000000000300000000471393e3", 16)),
    "::"                                         -> ResultContainer(6, BigInt("00000000000000000000000000000000", 16)),
    "1::8"                                       -> ResultContainer(6, BigInt("00010000000000000000000000000008", 16)),
    "::1"                                        -> ResultContainer(6, BigInt("00000000000000000000000000000001", 16)),
    "::192.168.0.1"                              -> ResultContainer(6, BigInt("000000000000000000000000c0a80001", 16)),
    "::255.255.255.255"                          -> ResultContainer(6, BigInt("000000000000000000000000ffffffff", 16)),
    "http:/[::255.255.255.255]:65536" ->
      IPInvalidAddressComponents(6, BigInt("000000000000000000000000ffffffff", 16), Some(65536), remark = "Port number out of range."),
    "::2:3:4:5:6:7:8"                            -> ResultContainer(6, BigInt("00000002000300040005000600070008", 16), strictRFC5952 = false),
    "::8"                                        -> ResultContainer(6, BigInt("00000000000000000000000000000008", 16)),
    "::c0a8:1"                                   -> ResultContainer(6, BigInt("000000000000000000000000c0a80001", 16)),
    "::ffff:0:255.255.255.255"                   -> ResultContainer(6, BigInt("0000000000000000ffff0000ffffffff", 16)),
    "::ffff:127.0.0.0.1"                         -> IPInvalidAddressComponents(4, remark = "Address puntation error: ':127.0.0.0.1'."),
    "::ffff:127.0.0.1"                           -> ResultContainer(6, BigInt("00000000000000000000ffff7f000001", 16)),
    "::ffff:192.168.0.1"                         -> ResultContainer(6, BigInt("00000000000000000000ffffc0a80001", 16)),
    "::ffff:192.168.173.22"                      -> ResultContainer(6, BigInt("00000000000000000000ffffc0a8ad16", 16)),
    "::ffff:255.255.255.255"                     -> ResultContainer(6, BigInt("00000000000000000000ffffffffffff", 16)),
    "::ffff:71.19.147.227"                       -> ResultContainer(6, BigInt("00000000000000000000ffff471393e3", 16)),
    "1:2:3:4:5:6:7::"                            -> ResultContainer(6, BigInt("00010002000300040005000600070000", 16), strictRFC5952 = false),
    "8000:2:3:4:5:6:7::"                         -> ResultContainer(6, BigInt("80000002000300040005000600070000", 16), strictRFC5952 = false),
    "1:2:3:4:5:6::8"                             -> ResultContainer(6, BigInt("00010002000300040005000600000008", 16), strictRFC5952 = false),
    "1:2:3:4:5::8"                               -> ResultContainer(6, BigInt("00010002000300040005000000000008", 16)),
    "1::7:8"                                     -> ResultContainer(6, BigInt("00010000000000000000000000070008", 16)),
    "a::b::1"                                    -> IPInvalidAddressComponents(remark = "Noise found: 'a::b::1'."),
    "fff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"     -> ResultContainer(6, BigInt("0fffffffffffffffffffffffffffffff", 16)),
    "FFFF:ffff:ffff:ffff:ffff:ffff:ffff:ffff"    -> ResultContainer(6, BigInt("ffffffffffffffffffffffffffffffff", 16), strictRFC5952 = false),
    "ffff:ffff:ffff:fffg:ffff:ffff:ffff:ffff"    -> IPInvalidAddressComponents(remark = "No match at all: 'ffff:ffff:ffff:fffg…'."),
    "g::1"                                       -> IPInvalidAddressComponents(6, remark ="Invalid input 'g::1'."),
    "[g::1]:192.0.2.33"                          -> IPInvalidAddressComponents(4, remark = "Address puntation error: ':192.0.2.33'."),
    "1:2:3:4:5:6:7:8"                            -> ResultContainer(6, BigInt("00010002000300040005000600070008", 16)),
    "1:2:3:4:5::7:8"                             -> ResultContainer(6, BigInt("00010002000300040005000000070008", 16), strictRFC5952 = false),
    "1:2:3:4::6:7:8"                             -> ResultContainer(6, BigInt("00010002000300040000000600070008", 16), strictRFC5952 = false),
    "1:2:3:4::8"                                 -> ResultContainer(6, BigInt("00010002000300040000000000000008", 16)),
    "1:2:3::5:6:7:8"                             -> ResultContainer(6, BigInt("00010002000300000005000600070008", 16), strictRFC5952 = false),
    "1:2:3::8"                                   -> ResultContainer(6, BigInt("00010002000300000000000000000008", 16)),
    "1:2::4:5:6:7:8"                             -> ResultContainer(6, BigInt("00010002000000040005000600070008", 16), strictRFC5952 = false),
    "1:2::8"                                     -> ResultContainer(6, BigInt("00010002000000000000000000000008", 16)),
    "1::3:4:5:6:7:8"                             -> ResultContainer(6, BigInt("00010000000300040005000600070008", 16), strictRFC5952 = false),
    "1::4:5:6:7:8"                               -> ResultContainer(6, BigInt("00010000000000040005000600070008", 16)),
    "1::5:6:7:8"                                 -> ResultContainer(6, BigInt("00010000000000000005000600070008", 16)),
    "[1::6:7:8]"                                 -> ResultContainer(6, BigInt("0010000000000000000000600070008", 16)),
    "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff"    -> ResultContainer(6, BigInt("ffffffffffffffffffffffffffffffff", 16)),
    "64:ff9b::192.0.2.33"                        -> ResultContainer(6, BigInt("0064ff9b0000000000000000c0000221", 16)),
    "64:ff9b::256.0.2.33" ->
      IPInvalidAddressComponents(6, BigInt("0064ff9b000000000000000000000000", 16), remark = "Invalid octets."),
    "[2001:db8:85a3:8d3:1319:8a2e:370:7348]:443" -> ResultContainer(6, BigInt("20010db885a308d313198a2e03707348", 16), Some(443)),
    "[2001:db8:85a3:8d3:1319:8a2e:370:7348]:100000" ->
      IPInvalidAddressComponents(6, BigInt("20010db885a308d313198a2e03707348", 16), Some(100000), remark = "Port number out of range."),
    "[2605:2700:0:3::4713:93e3]:80"              -> ResultContainer(6, BigInt("260527000000000300000000471393e3", 16), Some(80)),
    "[::ffff:192.168.0.1]:22"                    -> ResultContainer(6, BigInt("00000000000000000000ffffc0a80001", 16), Some(22)),
    "[::ffff:192.168.173.22]:80"                 -> ResultContainer(6, BigInt("00000000000000000000ffffc0a8ad16", 16), Some(80)),
    "[::ffff:71.19.147.227]:80"                  -> ResultContainer(6, BigInt("00000000000000000000FFFF471393E3", 16), Some(80)),
    "2001:0DB8:0:0:0:0:1428:57AB"                -> ResultContainer(6, BigInt("20010DB80000000000000000142857AB", 16), strictRFC5952 = false),
    "2001:0DB8:0:0:8D3:0:0:0"                    -> ResultContainer(6, BigInt("20010DB80000000008D3000000000000", 16), strictRFC5952 = false),
    "2001:DB8:0:0:8D3::"                         -> ResultContainer(6, BigInt("20010DB80000000008D3000000000000", 16), strictRFC5952 = false),
    "2001:DB8:3:4::192.0.2.33"                   -> ResultContainer(6, BigInt("20010DB80003000400000000C0000221", 16), strictRFC5952 = false),
    "2001:DB8:85A3:0:0:8A2E:370:7334"            -> ResultContainer(6, BigInt("20010DB885A3000000008A2E03707334", 16), strictRFC5952 = false),
    "2001:DB8::1428:57AB"                        -> ResultContainer(6, BigInt("20010DB80000000000000000142857AB", 16), strictRFC5952 = false),
    "2001:DB8::8D3:0:0:0"                        -> ResultContainer(6, BigInt("20010DB80000000008D3000000000000", 16), strictRFC5952 = false),
    "2605:2700:0:3::4713:93E3"                   -> ResultContainer(6, BigInt("260527000000000300000000471393E3", 16), strictRFC5952 = false),
    "::192.168.0.1"                              -> ResultContainer(6, BigInt("000000000000000000000000C0A80001", 16)),
    "::255.255.255.255"                          -> ResultContainer(6, BigInt("000000000000000000000000FFFFFFFF", 16)),
    "::C0A8:1"                                   -> ResultContainer(6, BigInt("000000000000000000000000c0a80001", 16), strictRFC5952 = false),
    "::FFFF:0:255.255.255.255"                   -> ResultContainer(6, BigInt("0000000000000000FFFF0000FFFFFFFF", 16), strictRFC5952 = false),
    "::FFFF:127.0.0.0.1"                         -> IPInvalidAddressComponents(4, remark = "Address puntation error: ':127.0.0.0.1'."),
    "::FFFF:127.0.0.1"                           -> ResultContainer(6, BigInt("00000000000000000000FFFF7F000001", 16), strictRFC5952 = false),
    "::FFFF:192.168.0.1"                         -> ResultContainer(6, BigInt("00000000000000000000FFFFC0A80001", 16), strictRFC5952 = false),
    "::FFFF:192.168.173.22"                      -> ResultContainer(6, BigInt("00000000000000000000FFFFC0A8AD16", 16), strictRFC5952 = false),
    "::FFFF:255.255.255.255"                     -> ResultContainer(6, BigInt("00000000000000000000FFFFFFFFFFFF", 16), strictRFC5952 = false),
    "::FFFF:71.19.147.227"                       -> ResultContainer(6, BigInt("00000000000000000000FFFF471393E3", 16), strictRFC5952 = false),
    "[1::]:80"                                   -> ResultContainer(6, BigInt("00010000000000000000000000000000", 16), Some(80)),
    "[2001:DB8:85A3:8D3:1319:8A2E:370:7348]:443" -> ResultContainer(6, BigInt("20010db885a308d313198a2e03707348", 16), Some(443), strictRFC5952 = false),
    "[2605:2700:0:3::4713:93E3]:80"              -> ResultContainer(6, BigInt("260527000000000300000000471393e3", 16), Some(80), strictRFC5952 = false),
    "[::1]:80"                                   -> ResultContainer(6, BigInt("00000000000000000000000000000001", 16), Some(80)),
    "[::1]:65536" ->
      IPInvalidAddressComponents(6, BigInt("00000000000000000000000000000001", 16), Some(65536), remark = "Port number out of range."),
    "[::]:80"                                    -> ResultContainer(6, BigInt("00000000000000000000000000000000", 16), Some(80)),
    "[::FFFF:192.168.0.1]:22"                    -> ResultContainer(6, BigInt("00000000000000000000ffffc0a80001", 16), Some(22), strictRFC5952 = false),
    "[::FFFF:192.168.173.22]:80"                 -> ResultContainer(6, BigInt("00000000000000000000ffffc0a8ad16", 16), Some(80), strictRFC5952 = false),
    "[::FFFF:71.19.147.227]:80"                  -> ResultContainer(6, BigInt("00000000000000000000ffff471393e3", 16), Some(80), strictRFC5952 = false),
    "A::B::1"                                    -> IPInvalidAddressComponents(remark = "Noise found: 'A::B::1'."),
    "FFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF"     -> ResultContainer(6, BigInt("0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16), strictRFC5952 = false),
    "FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF"    -> ResultContainer(6, BigInt("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF", 16), strictRFC5952 = false),
    "FFFF:FFFF:FFFF:FFFG:FFFF:FFFF:FFFF:FFFF"    -> IPInvalidAddressComponents(remark = "No match at all: 'FFFF:FFFF:FFFF:FFFG…'."),
    "G::1"                                       -> IPInvalidAddressComponents(6, remark = "Invalid input 'G::1'."),
    "64:FF9B::192.0.2.33"                        -> ResultContainer(6, BigInt("0064FF9B0000000000000000C0000221", 16), strictRFC5952 = false),
    "64:FF9B::256.0.2.33" -> IPInvalidAddressComponents(6, BigInt("0064FF9B000000000000000000000000", 16), remark = "Invalid octets.")
    )

  def IPInvalidAddressComponents(version: Int = 0,
                                 address: BigInt = BigInt(0),
                                 port: Option[Int] = None,
                                 valid: Boolean = false,
                                 remark: String = "",
                                 strict: Boolean = false) = ResultContainer(version, address, port, valid, remark, strict)

  case class ResultContainer(version: Int,
                             address: BigInt,
                             port: Option[Int] = None,
                             valid: Boolean = true,
                             remark: String = "",
                             strictRFC5952: Boolean = true)

  class IpAddress(val originalString: String) {

    import IpAddress._

    val (usedPattern, result: ResultContainer) = originalString match {
      case trapPattern() => (trapPattern, IPInvalidAddressComponents(remark = s"Noise found: '${shortener(originalString)}'."))
      case allIpV6PortedPatternsCompiled(adr, port) => parseIpV6(adr, Option(port).map(_.toInt))
      case allIpV6UnspecPortPatternsCompiled(adr) => parseIpV6(adr)
      case ipV4PortSpecCompiled(adr, port) => (ipV4PortSpecCompiled, parseIpV4(adr, Option(port).map(_.toInt)))
      case _ => ("Exhausted of all matches.", IPInvalidAddressComponents(remark = s"No match at all: '${shortener(originalString, 19)}'."))
    }

    override def toString: String = {
      def hexAddr = if (result.version == 6) f"${result.address}%#034x" else f"${result.address}%#010x"

      def validInd = if (result.valid) '\u2714' else '\u2718'

      def rfc5952 = if (result.strictRFC5952) "comply" else "broken"

      def version = result.version match {
        case 0 => "   ?"
        case 4 => "IPv4"
        case 6 => "IPv6"
      }

      def surround(s: String) = if (result.valid) s" $s " else s"($s)"

      def port = if (result.port.isDefined) surround(result.port.get.toString) else if (result.valid) " " else "? "

      def hexAddrField = f"${if (result.valid || result.address != 0) surround(hexAddr) else "? "}%36s "

      f"${shortener(originalString, 45)}%46s $version $validInd $rfc5952 $hexAddrField $port%8s ${result.remark}%-40s $usedPattern"
    }

    def shortener(s: String, maxlength: Int = 12): String = {
      val size = s.length()
      s.substring(0, math.min(size, maxlength)) + (if (size > maxlength) "…" else "")
    }

    private def parseIpV6(ipAddress: String, port: Option[Int] = None): (String, ResultContainer) = {

      def colonedStringToBigInt(colonedString: String): (BigInt, Int) = {
        // Compressed zeroes expansion
        val ar = if (colonedString contains "::") colonedString.split("::", 2) else Array("", colonedString)
        val (left, right) = (ar.head.split(':').filterNot(_.isEmpty), ar(1).split(':').filterNot(_.isEmpty))
        val sixteenBitExpansions = 8 - (right.length + left.length)

        ((left ++ Seq.fill(sixteenBitExpansions)("0") ++ right)
          .map(BigInt(_, 16).toLong).map(BigInt(_)).reduceLeft((acc, i) => (acc << 16) | i),
          sixteenBitExpansions)
      }

      def parseEmbeddedV4(seg: String, ip4Seg: String, usedRegEx: String): (String, ResultContainer) = {
        val (ip4, ip6Parser, test) =
          (parseIpV4(ip4Seg), colonedStringToBigInt(seg.replaceFirst(ipV4Regex("3"), "0:0")), portNumberTest(port))

        (usedRegEx, ResultContainer(originalString, 6, ip4.address + ip6Parser._1, port,
          ip4.valid && test.isEmpty, ip4.remark + test, ip4.valid && test.isEmpty))
      }

      if (!ipAddress.forall((('A' to 'F') ++ ('a' to 'f') ++ ('0' to '9') ++ Vector(':', '.')).contains(_)))
        ("[^:.[0-9][A-F][a-f]]", IPInvalidAddressComponents(6, remark = s"Invalid input '${shortener(ipAddress)}'."))
      else
        ipAddress match {
          case pattern10Compiled(seg, ip4Seg) => parseEmbeddedV4(seg, ip4Seg, pattern10Compiled.toString())
          case pattern11Compiled(seg, ip4Seg) => parseEmbeddedV4(seg, ip4Seg, pattern11Compiled.toString())
          case ip6PatternsRawCompiled(seg, _*) =>
            val (ip6Parser, test) = (colonedStringToBigInt(seg), portNumberTest(port))

            (ip6PatternsRawCompiled.toString(),
              ResultContainer(ipAddress, 6, ip6Parser._1, port,
                valid = test.isEmpty, test, strictRFC5952 = ip6Parser._2 != 1 && test.isEmpty))
          case _ => ("V6 match exhausted.", IPInvalidAddressComponents(6, remark = "V6 address puntation error."))
        }
    } // parseIpV6


    private def parseIpV4(sIP: String, port: Option[Int] = None): ResultContainer = {

      def wordsToNum(words: Array[Long]): Long = words.reduceLeft((acc, i) => (acc << 8) | i)

      if (sIP.head.isDigit && sIP.matches(ipV4Regex("3"))) {
        val octets = sIP.split('.').map(_.toLong)
        if (octets.forall(_ < 256)) {
          val portNumberOK = portNumberTest(port)
          ResultContainer(4, BigInt(wordsToNum(octets)), port, portNumberOK.isEmpty, portNumberOK, portNumberOK.isEmpty)
        } else IPInvalidAddressComponents(4, remark = "Invalid octets.")
      }
      else IPInvalidAddressComponents(4, remark = s"Address puntation error: '${shortener(sIP)}'.")
    }

    private def portNumberTest(port: Option[Int]) = if (port.isEmpty || port.get < math.pow(2, 16)) "" else "Port number out of range."
  } // IpAddress

  object IpAddress {
    val (ip6PatternsRawCompiled, pattern11Compiled) = (ipV6Patterns.mkString("(", "|", ")").r, embeddedV4patterns()(1).r)
    val (trapPattern, pattern10Compiled) = (""".*?(?:(?:\w*:{2,}?){2,}?\w)|(?:\[?)""".r, embeddedV4patterns().head.r)
    val allIpV6PortedPatternsCompiled = ("""[^\\.]*?\[(""" + allIpV6 +""")\](?::(\d{1,6}))?[^\.:]*?""").r
    val allIpV6UnspecPortPatternsCompiled = (""".*?(""" + allIpV6 +""")[^\.:]*?""").r
    val ipV4PortSpecCompiled = s".*?([:.\\]]?${ipV4Regex()})(?::(\\d{1,6}))?.*?".r

    // Make a regex pattern with non-capturing groups by the disabling the capturing group syntax (?:).
    def allIpV6 = (embeddedV4patterns("(?:") ++ ipV6Patterns).map(s => "(?:" + s.drop(1)).mkString("|")

    def ipV6Patterns = {
      def ipV6SegRegWC = """\w{1,4}"""

      Seq(
        s"((?::(?:(?::$ipV6SegRegex){1,7}|:)))",
        s"((?:$ipV6SegRegWC:(?::$ipV6SegRegex){1,6}))",
        s"((?:$ipV6SegRegex:){1,2}(?::$ipV6SegRegex){1,5})",
        s"((?:$ipV6SegRegex:){1,3}(?::$ipV6SegRegex){1,4})",
        s"((?:$ipV6SegRegex:){1,4}(?::$ipV6SegRegex){1,3})",
        s"((?:$ipV6SegRegex:){1,5}(?::$ipV6SegRegex){1,2})",
        s"((?:$ipV6SegRegex:){1,6}:$ipV6SegRegex)",
        s"((?:$ipV6SegRegex:){1,7}:)",
        s"((?:$ipV6SegRegex:){7}$ipV6SegRegex)"
      )
    }

    private def embeddedV4patterns(nonCapturePrefix: String = "(") =
      Seq(s"(::(?:(?:FFFF|ffff)(?::0{1,4}){0,1}:){0,1}$nonCapturePrefix${ipV4Regex("3")}))",
        s"((?:$ipV6SegRegex:){1,4}:$nonCapturePrefix${ipV4Regex("3")}))")

    private def ipV6SegRegex = """[\dA-Fa-f]{1,4}"""

    private def ipV4Regex(octets: String = "3,") = s"(?:\\d{1,3}\\.){$octets}\\d{1,3}"
  }

  object ResultContainer {
    def apply(orginalString: String, version: Int,
              address: BigInt, port: Option[Int],
              valid: Boolean, remark: String,
              strictRFC5952: Boolean): ResultContainer =
    // To comply with strictRFC5952 all alpha character must be lowercase too.
      this (version, address, port, valid, remark, strictRFC5952 && !orginalString.exists(_.isUpper))
  }

  {
    val headline = Seq(f"${"IP addresses to be parsed. "}%46s", "Ver.", f"${"S"}%1s", "RFC5952",
      f"${"Hexadecimal IP address"}%34s", f"${"Port "}%10s", f"${" Remark"}%-40s", f"${" Effective RegEx"}%-40s")

    println(headline.mkString("|") + "\n" + headline.map(s => "-" * s.length).mkString("+"))

    val cases: Set[IpAddress] = myCases.keySet.map(new IpAddress(_))

    println(cases.toList.sortBy(s => (s.originalString.length, s.originalString)).mkString("\n"))
    logInfo(s"Concluding: ${myCases.size} cases processed, ${cases.count(_.result.valid)} valid ✔ and ${cases.count(!_.result.valid)} invalid ✘.")
    logInfo("Successfully completed without errors.")

    def logInfo(info: String) {
      println(f"[Info][${System.currentTimeMillis() - executionStart}%5d ms]" + info)
    }
  }

} // IPparser cloc.exe : 235 loc
```



## Tcl

{{tcllib|ip}}

```tcl
package require Tcl 8.5
package require ip

proc parseIP {address} {
    set result {}
    set family [ip::version $address]
    set port -1
    if {$family == -1} {
	if {[regexp {^\[(.*)\]:(\d+)$} $address -> address port]} {
	    dict set result port $port
	    set family [ip::version $address]
	    if {$family != 6} {
		return -code error "bad address"
	    }
	} elseif {[regexp {^(.*):(\d+)$} $address -> address port]} {
	    dict set result port $port
	    set family [ip::version $address]
	    if {$family != 4} {
		return -code error "bad address"
	    }
	} else {
	    return -code error "bad address"
	}
    }
    # Only possible error in ports is to be too large an integer
    if {$port > 65535} {
	return -code error "bad port"
    }
    dict set result family $family
    if {$family == 4} {
	# IPv4 normalized form is dotted quad, but toInteger helps
	dict set result addr [format %x [ip::toInteger $address]]
    } else {
	# IPv6 normalized form is colin-separated hex
	dict set result addr [string map {: ""} [ip::normalize $address]]
    }
    # Return the descriptor dictionary
    return $result
}
```

Demonstration code:

```tcl
foreach address {
    127.0.0.1
    127.0.0.1:80
    ::1
    [::1]:80
    2605:2700:0:3::4713:93e3
    [2605:2700:0:3::4713:93e3]:80
    ::ffff:192.168.0.1
    [::ffff:192.168.0.1]:22
    ::ffff:127.0.0.0.1
    a::b::1
    127.0.0.1:100000
} {
    if {[catch {
	set parsed [parseIP $address]
    } msg]} {
	puts "error ${msg}: \"$address\""
	continue
    }
    dict with parsed {
	puts -nonewline "family: IPv$family addr: $addr"
	if {[dict exists $parsed port]} {
	    puts -nonewline " port: $port"
	}
	puts ""
    }
}
```

Output:

```txt

family: IPv4 addr: 7f000001
family: IPv4 addr: 7f000001 port: 80
family: IPv6 addr: 00000000000000000000000000000001
family: IPv6 addr: 00000000000000000000000000000001 port: 80
family: IPv6 addr: 260527000000000300000000471393e3
family: IPv6 addr: 260527000000000300000000471393e3 port: 80
family: IPv6 addr: 00000000000000000000ffffc0a80001
family: IPv6 addr: 00000000000000000000ffffc0a80001 port: 22
error bad address: "::ffff:127.0.0.0.1"
error bad address: "a::b::1"
error bad port: "127.0.0.1:100000"

```



## VBScript


```vb
Function parse_ip(addr)
	'ipv4 pattern
	Set ipv4_pattern = New RegExp
	ipv4_pattern.Global = True
	ipv4_pattern.Pattern = "(\d{1,3}\.){3}\d{1,3}"
	'ipv6 pattern
	Set ipv6_pattern = New RegExp
	ipv6_pattern.Global = True
	ipv6_pattern.Pattern = "([0-9a-fA-F]{0,4}:){2}[0-9a-fA-F]{0,4}"
	'test if address is ipv4
	If ipv4_pattern.Test(addr) Then
		port = Split(addr,":")
		octet = Split(port(0),".")
		ipv4_hex = ""
		For i = 0 To UBound(octet)
			If octet(i) <= 255 And octet(i) >= 0 Then
				ipv4_hex = ipv4_hex & Right("0" & Hex(octet(i)),2)
			Else
				ipv4_hex = "Erroneous Address"
				Exit For
			End If 
		Next
		parse_ip = "Test Case: " & addr & vbCrLf &_
		           "Address: " & ipv4_hex & vbCrLf
		If UBound(port) = 1 Then
			If port(1) <= 65535 And port(1) >= 0 Then
				parse_ip = parse_ip & "Port: " & port(1) & vbCrLf
			Else
				parse_ip = parse_ip & "Port: Invalid" & vbCrLf
			End If
		End If
	End If
	'test if address is ipv6
	If ipv6_pattern.Test(addr) Then
		parse_ip = "Test Case: " & addr & vbCrLf
		port_v6 = "Port: "
		ipv6_hex = ""
		'check and extract port information if any
		If InStr(1,addr,"[") Then
			'extract the port
			port_v6 = port_v6 & Mid(addr,InStrRev(addr,"]")+2,Len(addr)-Len(Mid(addr,1,InStrRev(addr,"]")+1)))
			'extract the address
			addr = Mid(addr,InStrRev(addr,"[")+1,InStrRev(addr,"]")-(InStrRev(addr,"[")+1))
		End If
		word = Split(addr,":")
		word_count = 0
		For i = 0 To UBound(word)
			If word(i) = "" Then
				If i < UBound(word) Then
					If Int((7-(i+1))/2) = 1 Then
						k = 1
					ElseIf UBound(word) < 6 Then
						k = Int((7-(i+1))/2)
					ElseIf UBound(word) >= 6 Then
						k = Int((7-(i+1))/2)-1
					End If
					For j = 0 To k
						ipv6_hex = ipv6_hex & "0000"
						word_count = word_count + 1
					Next
				Else
					For j = 0 To (7-word_count)
						ipv6_hex = ipv6_hex & "0000"
					Next
				End If
			Else
				ipv6_hex = ipv6_hex & Right("0000" & word(i),4)
				word_count = word_count + 1
			End If
		Next
		parse_ip = parse_ip & "Address: " & ipv6_hex &_
				vbCrLf & port_v6 & vbCrLf
	End If
	'test if the address in invalid
	If ipv4_pattern.Test(addr) = False And ipv6_pattern.Test(addr) = False Then
		parse_ip = "Test Case: " & addr & vbCrLf &_
		           "Address: Invalid Address" & vbCrLf
	End If
End Function

'Testing the function
ip_arr = Array("127.0.0.1","127.0.0.1:80","::1",_
	"[::1]:80","2605:2700:0:3::4713:93e3","[2605:2700:0:3::4713:93e3]:80","RosettaCode")

For n = 0 To UBound(ip_arr)
	WScript.StdOut.Write parse_ip(ip_arr(n)) & vbCrLf
Next
```


{{Out}}

```txt

Test Case: 127.0.0.1
Address: 7F000001

Test Case: 127.0.0.1:80
Address: 7F000001
Port: 80

Test Case: ::1
Address: 00000000000000000000000000000001
Port: 

Test Case: [::1]:80
Address: 00000000000000000000000000000001
Port: 80

Test Case: 2605:2700:0:3::4713:93e3
Address: 260527000000000300000000471393e3
Port: 

Test Case: [2605:2700:0:3::4713:93e3]:80
Address: 260527000000000300000000471393e3
Port: 80

Test Case: RosettaCode
Address: Invalid Address

```

