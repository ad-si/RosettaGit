+++
title = "Talk:LZW compression"
description = ""
date = 2015-10-28T17:28:40Z
aliases = []
[extra]
id = 3328
[taxonomies]
categories = []
tags = []
+++

==Task and example==
The task talks about compression, but I've seen a lot of examples implement both compression and decompression; should we create a new task (LZW decompression) where to move the decompression related code, or change this one and add decompression to those examples that still does not implement it? --[[User:ShinTakezou|ShinTakezou]] 16:51, 31 January 2009 (UTC)
: I'd be in favor of splitting them apart, for clarity. (Never overestimate the understanding of an individual referred here by Google.  Well, you should never underestimate them, either, but I think there's a greater risk of overestimating in this case.) --[[User:Short Circuit|Short Circuit]] 21:32, 31 January 2009 (UTC)
:: I see it hard (1), since most OO-lang implementation must be split by the people who wrote the code or know the language; (1) by hard I mean, I can't do it by myself, I could just add a new decompression task and write an impl. for Obj-C that would be the "complement" of the one here, then mark all other examples as ... hm... they do the task, but do also more... so they are task-compliant in some way... (what?) --[[User:ShinTakezou|ShinTakezou]] 16:43, 13 February 2009 (UTC)
::: Not a problem, currently.  I can understand the currently-listed languages enough to do the separation myself.  I'll get to it as soon as I can, which will likely be some time after midnight EST. --[[User:Short Circuit|Short Circuit]] 21:27, 13 February 2009 (UTC)

==JavaScript and Unicode==
The implementation of algorithm in JavaScript can treat ASCII characters only. Any attempt to work with Unicode causes data lost because of the dictionary doesn't contain characters with code greater than 255. But the line

```javascript

result.push(dictionary[w]);

```

expects the string stored in variable ''w'' is in the dictionary, which is wrong.--[[User:ArtyomShegeda|ArtyomShegeda]] ([[User talk:ArtyomShegeda|talk]]) 17:28, 28 October 2015 (UTC)

==C# Decompression function==
I'm not sure if it would be acceptable to just post the decompression, so I'm posting it here. Since I'm lazy, and only need decomp (which seems to be easier anyway), I only wrote that.

```c#

    class LZWDecompress
    {
        static void Main(string[] args)
        {
            int[] Compressed = { 84, 79, 66, 69, 79, 82, 78, 79, 84, 256, 258, 260, 265, 259, 261, 263 };

            String Decompressed = DeCompress(Compressed);
            //Decompressed should now contain "TOBEORNOTTOBEORTOBEORNOT"
        }
        static String DeCompress(int[] Compressed)
        {
            ArrayList Dictionary = new ArrayList();

            String Output = "";
            String tmpOut = "";
            String tmpIn = "";
            bool JustOnce = false;

            //Build dictionary
            for (int i = 0; i < 256; i++)
                Dictionary.Add(Convert.ToChar(i).ToString());

            foreach (int Character in Compressed)
            {
                if (JustOnce == false)
                {
                    JustOnce = true;
                    Output = tmpIn = (String)Dictionary[Character];
                    continue;
                }

                if (Character < Dictionary.Count)
                    tmpOut = (String)Dictionary[Character];
                else if (Character == Dictionary.Count)
                    tmpOut = tmpIn + tmpIn.Substring(0, 1);
                else
                    break;

                Output = Output + tmpOut;
                Dictionary.Add((tmpIn + tmpOut.Substring(0, 1)));
                tmpIn = tmpOut;
            }
            return Output;
        }
    }

```

Cyberman (not registered here) --[[Special:Contributions/85.127.79.163|85.127.79.163]] 19:10, 2 April 2011 (UTC)
You can go ahead an add it. Just add <nowiki>{{incomplete|C sharp|It does not have a compress function.}}</nowiki> underneath the header. Hopefully someone can help you out with that eventually. --[[User:Mwn3d|Mwn3d]] 19:24, 2 April 2011 (UTC)

== C example problem ==

Again, terrible code.  Depends on 4 other badly written files, so it's incovenient to test at least; doesn't actually compile after pulling all files together; after some quick fix (probably incorrectly, but I don't have the patience to wade through all the mess), program outputs garbage.  By the look of it, the dictionary data structure is quite suboptimal, to put it lightly. --[[User:Ledrug|Ledrug]] 23:13, 5 August 2011 (UTC)
:I've added the following message to the C section. I was able to adapt this code to produce a GIF encoder seeing that it was nearly there. This is a specific variant of LZW that probably should have its own page on this wiki with a redirect at the top of this page. Unfortunately the code could use a complete rewrite, which I intend to do for myself, but I've no need for a decoder, and have already used C++ in my changes, so I must leave this as an exercise (for others to do for themself. EDITED: I may post the rewrite on this Talk page.) Furthermore I've noticed that one or two other languages on this page have adapted this C code (they would require repair as well.) As for a GIF-LZW encoder, it only implements 8bit color. GIF can also do 1~7 bit color. Just something to keep in mind. By "efficient" the comments probably mean how GIF uses a variable length code size that increases as the codes require more bits to be represented. I do not believe it's a commentary on its algorithm; although it looks fine to me (aside from being poorly worded.) ''I THINK it is an interesting example, because it's unlikely anyone would want an LZW encoder for anything other than GIF files, since other formats that use LZW are unlikely to be in use today. And no one would use LZW otherwise except to work with old files/systems!''

:'''WARNING: This code appears to have come from a GIF codec that has been modified to meet the requirements of this page, provided that the decoder works with the encoder to produce correct output. For writing GIF files the write_bits subroutine is wrong for Little Endian systems (it may be wrong for Big Endian as well.) The encoder also increases the number of bits in the variable length GIF-LZW after the N-2 code, whereas this must be done after N-1 to produce a working GIF file (just looking at the encoder, it's easy to see how this mistake could be made.)''' --[[User:Mick P.|Mick P.]] ([[User talk:Mick P.|talk]]) 03:17, 21 October 2015 (UTC)
*I was told by a person monitoring the GIF page on Wikipedia that the C code here looks like it may have came from a PDF reader, which they say is similar to GIF but different in subtle ways. In any case, I rewrote the C example for a GIF flavored codec, which does LZW packed to the GIF specification, combined, like the C code. Technically the packing is not LZW, so it may not even be appropriate. The following code could be used to make the C example more readable, although it switches to C++. Unfortunately I do not have a matching decoder to offer. It's also written to be ultra-compact, which may not be helpful, in addition to being slightly unorthodox.

```cpp

//GIF flavored LZW based on:
//rosettacode.org/wiki/LZW_compression#C
//encode_t dictionary[4096];
struct encode_t{ short next[256]; };
static std::vector<encode_t> dictionary;
static std::vector<char> encoded;
template<class T> size_t encode(T in)
{
	//2-color must use 2
	const int colorbits = 8;
	enum{cc=2<<(colorbits-1),eoi,c0};
	auto &out = encoded; out.clear();

	int next_code = c0;
	int next_shift = 2<<colorbits;
	dictionary.resize(next_shift);

	int len = in.size();
	int o = 0, o_bits = 0;
	int c_bits = colorbits+1;
	unsigned c,nc, code = cc; do
	{
		c = *in; ++in; //c holds a byte
		if(!(nc=dictionary[code].next[c])) cc:
		{
			o|=code<<o_bits;
			for(o_bits+=c_bits;o_bits>=8;o>>=8)
			{
				o_bits-=8; out.push_back(o&0xFF);
			}
			if(next_code>=next_shift)
			{
				if(++c_bits>12) //12 is GIF's limit
				{
					c_bits = 12; //13? spec is unclear

					next_code = c0;
					next_shift = 2<<colorbits;
					dictionary.clear();
					dictionary.resize(next_shift);
					code = cc; goto cc;
				}
				else dictionary.resize(next_shift*=2);
			}
			if(code!=cc)
			nc = dictionary[code].next[c] = next_code++;
			else if(c_bits>=12) c_bits = colorbits+1;
			code = c;
		}
		else code = nc;

	}while(--len>0); switch(len)
	{
	case  0: goto cc; //truncate
	case -1: code = eoi; goto cc;
	case -2: c_bits = 8; if(o_bits) goto cc;
	}
	dictionary.clear();
	return out.size();
}

```

--[[User:Mick P.|Mick P.]] ([[User talk:Mick P.|talk]]) 11:08, 27 October 2015 (UTC)
